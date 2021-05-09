unit LuaRemoteExecutor;
{
Remote Executor is a thread that runs inside the target process. It waits for a "HasFunctionAndParametersReadyForExecution" event, and executes it according to ExecuteMethod parameters

Each executor has it's own parameter store in shared memory accessible by both CE and the target, and only executes one function at a time

A process can have more than one executor thread at the same time

}

{$mode delphi}

interface

uses
  {$ifdef windows}
  windows,
  {$endif}

  {$ifdef darwin}
  macport,
  {$endif}

  Classes, SysUtils, math;    //todo: port to mac

type
  TStubdata=record
    address: ptruint;
    parameters: array of integer;
  end;



type
  TRemoteExecutorSharedMemory=record
    HasDataEventHandle: QWORD;       //0
    HasProcessedDataEventHandle: QWORD; //8
    Command: QWORD; //10
    Address: QWORD; //18  (inithandle)
    ReturnValue: QWORD;
    ParamStart: byte;
    //....   (pointers to strings will point inside this block, based on the target process's executor map address)
  end;

  PRemoteExecutorSharedMemory=^TRemoteExecutorSharedMemory;

  TExecBuffer=record
    parameternr: integer;
    data: ptruint;
    length: integer;
  end;

  Tvalueinfo=record
    value: qword;
    bytesize: integer; //only used for bytetables
  end;

  TExecBufferArray=array of TExecBuffer;


  TRemoteExecutor=class
  private
    targetedProcessID: dword;
    HasDataEventHandle: THandle;
    HasProcessedDataEventHandle: THandle;

    sharedMemorySize: integer;
    sharedMemory: PRemoteExecutorSharedMemory;
    sharedMemoryClientLocation: QWORD;


    memmap: THandle;
    remoteMemMapHandle: THandle;

    ExecutorThreadHandle: THandle;
    ExecutorThreadExecMemory: ptruint;

    lastBuffers: TExecBufferArray;
    procedure waitForProcessedDataEvent(timeout: dword);
    procedure growSharedMemorySize(newsize: integer; timeout: DWORD);
    function LocalToRemoteAddress(LocalAddress: ptruint): ptruint;
  public
    procedure executeStub(stubdata: TStubdata; values: array of Tvalueinfo; timeout: dword);
    function waitTillDoneAndGetResult(timeout: integer; var Buffers: TExecBufferArray): QWORD;
    constructor create;
    destructor destroy; override;
  end;

procedure InitializeLuaRemoteExecutor;

implementation

uses ProcessHandlerUnit, autoassembler, commonTypeDefs, lua, luahandler, luaclass,
  LuaObject, LuaByteTable, Clipbrd;

const
  CMD_RELOADMEM = 0;
  CMD_EXECUTE = 1;
  CMD_QUIT = 2;


procedure TRemoteExecutor.waitForProcessedDataEvent(timeout: dword);
//the HasProcessedDataEventHandle is a manualreset event, so this can be called whenever
var
  starttime: qword;
  r: dword;
begin
  {$ifdef windows}
  starttime:=gettickcount64;
  repeat
    r:=WaitForSingleObject(HasProcessedDataEventHandle, min(timeout,100));
    if (r<>WAIT_OBJECT_0) and (timeout>0) then
      CheckSynchronize;
  until (r<>WAIT_TIMEOUT) or (timeout=0) or ((timeout<>$ffffffff) and (gettickcount64>starttime+timeout));

  if r<>WAIT_OBJECT_0 then
    raise exception.Create('Timeout');
  {$else}
  raise exception.Create('Not yet implemented');
  {$endif}
end;

function TRemoteExecutor.waitTillDoneAndGetResult(timeout: integer; var Buffers: TExecBufferArray): QWORD;
begin
  waitForProcessedDataEvent(timeout);

  buffers:=lastBuffers;
  result:=sharedMemory^.ReturnValue;
end;

function TRemoteExecutor.LocalToRemoteAddress(LocalAddress: ptruint): ptruint;
begin
  result:=sharedMemoryClientLocation+(LocalAddress-ptruint(sharedMemory));
end;

procedure TRemoteExecutor.executeStub(stubdata: TStubdata; values: array of Tvalueinfo; timeout: dword);
var
  i: integer;
  r: dword;
  len: integer;

  paramsizeneeded: integer; //after this space the buffers can be placed

  sizeneeded: integer;

  currentParam: ptruint;

  dataPosition: ptruint;

  ExecBuffer: TExecBuffer;
begin
  {$ifdef windows}
  if length(stubdata.parameters)<>length(values) then
    raise exception.create('Incorrect parameter count');

  sizeneeded:=0;
  paramsizeneeded:=0;

  //size check
  for i:=0 to length(stubdata.parameters)-1 do
  begin
    //0: integer/pointer
    //1: float
    //2: double
    //3: asciistring (turns into 0:pointer after writing the string)
    //4: widestring

    //<0: pointer to input/output buffer:
    //  bit31: Pointer to buffer
    //  bit30: Output Only. Needs no init
    //  bit29: Input Only. Don't return when getting results



    case stubdata.parameters[i] of
      0,1: //integer/pointer/float
      begin
        inc(sizeneeded, processhandler.pointersize);
        inc(paramsizeneeded, processhandler.pointersize);
      end;

      2: //double
      begin
        inc(sizeneeded, 8);
        inc(paramsizeneeded,8); //32-bit uses 8 bytes for double params
      end;
      3:
      begin
        inc(sizeneeded, processhandler.pointersize+strlen(pchar(values[i].value))+32);
        inc(paramsizeneeded, processhandler.pointersize);
      end;

      4:
      begin
        inc(sizeneeded, processhandler.pointersize+StrLen(pwidechar(values[i].value))+32);
        inc(paramsizeneeded, processhandler.pointersize);
      end;

      else
      begin
        if stubdata.parameters[i]<0 then
          inc(sizeneeded, processhandler.pointersize+32+values[i].bytesize)
        else
          inc(sizeNeeded, processhandler.pointersize);

        inc(paramsizeneeded, processhandler.pointersize);
      end;
    end;
  end;

  if sizeneeded>sharedMemorySize then
    growSharedMemorySize(sizeneeded*2+1024, timeout);

  //enter the stub data into shared memory
  //still here, so everything is ok

  waitForProcessedDataEvent(timeout);
  //fill in the call data

  sharedMemory^.Command:=CMD_EXECUTE;
  sharedMemory^.Address:=stubdata.address;
  sharedMemory^.ReturnValue:=0;
  currentParam:=ptruint(@sharedMemory^.ParamStart);
  dataPosition:=align(ptruint(@sharedMemory^.ParamStart)+paramsizeneeded,16);

  setlength(lastbuffers,0);

  for i:=0 to length(stubdata.parameters)-1 do
  begin
    //0: integer/pointer
    //1: float
    //2: double
    //3: asciistring (turns into 0:pointer after writing the string)
    //4: widestring

    case stubdata.parameters[i] of
      0,1: //integer/pointer/float
      begin
        if processhandler.is64Bit then
          pqword(currentparam)^:=values[i].value
        else
          pdword(currentparam)^:=values[i].value;

        inc(currentparam, processhandler.pointersize);
      end;

      2: //double
      begin
        pqword(currentparam)^:=values[i].value;
        inc(currentparam, 8);
      end;

      3: //pchar
      begin
        if processhandler.is64Bit then
          pqword(currentparam)^:=LocalToRemoteAddress(DataPosition)
        else
          pdword(currentparam)^:=LocalToRemoteAddress(DataPosition);


        len:=strlen(pchar(values[i].value))+1;
        strcopy(pchar(dataPosition),pchar(values[i].value));
        dataposition:=align(DataPosition+len,16);

        inc(currentparam, processhandler.pointersize);
      end;

      4: //widechar
      begin
        if processhandler.is64Bit then
          pqword(currentparam)^:=LocalToRemoteAddress(DataPosition)
        else
          pdword(currentparam)^:=LocalToRemoteAddress(DataPosition);


        len:=strlen(pwidechar(values[i].value))+2;
        strcopy(pwidechar(dataPosition),pwidechar(values[i].value));
        dataposition:=align(DataPosition+len,16);

        inc(currentparam, processhandler.pointersize);
      end;

      else
      begin
        if stubdata.parameters[i]<0 then
        begin
          len:=values[i].bytesize;
          if processhandler.is64Bit then
            pqword(currentparam)^:=LocalToRemoteAddress(DataPosition)
          else
            pdword(currentparam)^:=LocalToRemoteAddress(DataPosition);


          if (stubdata.parameters[i] and (1 shl 29))=0 then //it's not an input only param, record it for output
          begin
            Execbuffer.parameternr:=i;
            Execbuffer.length:=len;
            Execbuffer.data:=DataPosition;

            setlength(lastbuffers, length(lastBuffers)+1);
            lastbuffers[length(lastBuffers)-1]:=ExecBuffer;
          end;


          if (stubdata.parameters[i] and (1 shl 30))=0 then //not an output only param, copy the data
            copymemory(pointer(dataPosition),pointer(values[i].value),len);

          dataposition:=align(DataPosition+len,16);

          inc(currentparam, processhandler.pointersize);
        end
        else
        begin
          //unknown type
          if processhandler.is64Bit then
            pqword(currentparam)^:=values[i].value
          else
            pdword(currentparam)^:=values[i].value;

          inc(currentparam, processhandler.pointersize);
        end;
      end;
    end;

  end;


  ResetEvent(HasProcessedDataEventHandle);
  SetEvent(HasDataEventHandle);  //do the call command

  {$else}
  raise exception.create('Not yet implemented');
  {$endif}
end;

procedure TRemoteExecutor.growSharedMemorySize(newsize: integer; timeout: DWORD);
var
  newmemmap: THandle;
  newSharedMemory: pointer;

  oldSharedMemory: PRemoteExecutorSharedMemory;
  oldmemmap: THandle;
begin
  {$ifdef windows}
  if newsize>sharedMemorySize then
  begin
    oldSharedMemory:=sharedmemory;
    oldmemmap:=memmap;


    waitForProcessedDataEvent(timeout); //in case there was a previous no-wait call going on


    newmemmap:=CreateFileMapping(INVALID_HANDLE_VALUE,nil,PAGE_READWRITE,0,newsize,nil);
    if (newmemmap=0) or (newmemmap=INVALID_HANDLE_VALUE) then raise exception.create('Remap: Failure creating remote executor filemapping');

    newsharedMemory:=MapViewOfFile(newmemmap,FILE_MAP_READ or FILE_MAP_WRITE,0,0,newsize);
    if newsharedMemory=nil then
      raise exception.create('Remap: Failure mapping memorymap into memory');

    if DuplicateHandle(GetCurrentProcess, newmemmap, processhandle, @remoteMemMapHandle, 0, false, DUPLICATE_SAME_ACCESS )=false then
      raise exception.create('Remap: Failure duplicating memmap handle');

    zeromemory(newSharedMemory, newsize);
    copymemory(newSharedMemory, sharedmemory, sharedmemorysize);


    sharedMemory^.Address:=remoteMemMapHandle;
    sharedMemory^.Command:=CMD_RELOADMEM;

    ResetEvent(HasProcessedDataEventHandle);
    setevent(HasDataEventHandle);

    waitForProcessedDataEvent(timeout);

    //after this, the map has swapped to the new one, so the return is in the new one
    memmap:=newmemmap;
    sharedMemory:=newSharedMemory;
    sharedMemoryClientLocation:=sharedmemory^.ReturnValue;
    sharedmemorysize:=newsize;

    UnmapViewOfFile(oldsharedMemory);
    CloseHandle(oldmemmap);


  end;
  {$endif}
end;

constructor TRemoteExecutor.create;
var
  h: THandle;

  disableinfo: TDisableInfo;
  executorThreadID: dword;
  script: TStringlist=nil;
begin
  {$ifdef windows}
  sharedmemorysize:=5*8; //sizeof(TRemoteExecutorSharedMemory); //can grow if needed
  memmap:=CreateFileMapping(INVALID_HANDLE_VALUE,nil,PAGE_READWRITE,0,sharedmemorysize,nil);
  if (memmap=0) or (memmap=INVALID_HANDLE_VALUE) then raise exception.create('Failure creating remote executor filemapping');

  sharedMemory:=MapViewOfFile(memmap,FILE_MAP_READ or FILE_MAP_WRITE,0,0,sharedmemorysize);
  if sharedmemory=nil then
    raise exception.create('Failure mapping memorymap into memory');

  zeromemory(sharedMemory, sharedmemorysize);


  HasDataEventHandle:=CreateEvent(nil,false,false,nil);
  HasProcessedDataEventHandle:=CreateEvent(nil,true,false,nil);

  if DuplicateHandle(GetCurrentProcess, HasDataEventHandle, processhandle, @(sharedmemory^.HasDataEventHandle), 0, false, DUPLICATE_SAME_ACCESS )=false then
    raise exception.create('Failure duplicating HasDataEventHandle');

  if  DuplicateHandle(GetCurrentProcess, HasProcessedDataEventHandle, processhandle, @(sharedmemory^.HasProcessedDataEventHandle), 0, false, DUPLICATE_SAME_ACCESS )=false then
    raise exception.create('Failure duplicating HasProcessedDataEventHandle');

  if  DuplicateHandle(GetCurrentProcess, MemMap, processhandle, @remoteMemMapHandle, 0, false, DUPLICATE_SAME_ACCESS )=false then
    raise exception.create('Failure duplicating HasProcessedDataEventHandle');


  //inject the executor code
  disableinfo:=TDisableInfo.create;
  script:=tstringlist.create;
  try
    script.add('alloc(Executor,2048)');
    script.add('Executor:'); //At entrypoint the first param ([esp+4 or RCX) contains a filemapping handle (created by ce and duplicated to the target, like veh)
    if processhandler.is64Bit then
    begin
      script.add('sub rsp,18');  //allocate local vars
      script.add('mov rbp,rsp'); //[rbp: +0-7: storage of the mapped memory address, 8-f: filemapping handle; 10-17: old filehandle on reinit; 18-1f: return address; 20-28: scratchspace for param1
      script.add('mov [rbp],0');  //mapped base address
      script.add('mov [rbp+8],rcx'); //filemapping handle
      //rbp+10 will contain the old filehandle on reinit
      script.add('sub rsp,30'); //allocate scratchspace memory for function calls (mapviewoffile takes 5)
    end
    else
    begin
      script.add('mov ecx,[esp+4]'); //ecx now contains the filemapping handle
      script.add('sub esp,c'); //allocate local vars
      script.add('mov ebp,esp'); //[ebp: +0-3: stortage of the mapped memory address, +4-7: filemapping handle +8-b:old filehandle on reinit c-f: return address 10-13: filemaphandle

      script.add('mov [ebp],0');
      script.add('mov [ebp+4],ecx'); //it's a bit double, but just to confirm
    end;

    script.add('reinit:');
    script.add('cmp [rbp],0');
    script.add('je short getSharedMemory');

    //free the old memoryblock first
    if processhandler.is64Bit then
      script.add('mov rcx,[rbp]')  //[rbp] contains the old address
    else
      script.add('push [ebp]');    //[ebp] contains the old address

    script.add('call unmapviewoffile');

    if processhandler.is64Bit then
      script.add('mov rcx,[rbp+10]') //[rbp+10] contains the old filehandle
    else
      script.add('mov rcx,[rbp+8]'); //[ebp+8] contains the old filehandle

    script.add('call CloseHandle');


    script.add('getSharedMemory:');
    if processhandler.is64Bit then
    begin
      script.add('mov rcx,[rbp+8]'); //p1: Handle. [rbp+8] = filemapping handle
      script.add('mov rdx,6');       //p2: access rights FILE_MAP_READ or FILE_MAP_WRITE
      script.add('mov r8,0');        //p3:
      script.add('mov r9,0');        //p4:
      script.add('mov [rsp+20],0');  //p5:
    end
    else
    begin
      script.add('push 0');           //p5
      script.add('push 0');           //p4
      script.add('push 0');           //p3
      script.add('push 6');           //p2
      script.add('push [ebp+4]');     //p1 Handle
    end;
    script.add('call mapviewoffile');

    script.add('cmp rax,0');
    script.add('je end'); //the fuck?

    //handle result
    script.add('mov [rbp],rax');
    script.add('mov [rax+20],rax'); //store the address also in the shared memory result field so CE knows which address to use for direct memory access

    if processhandler.is64Bit then
      script.add('mov rcx,[rax+8]') //HasProcessedDataEventHandle (is not set at the start, this will set it, so CE will know it can go)
    else
      script.add('push [rax+8]');

    script.add('call SetEvent');  //let CE know it can send some new commands
    script.add('cmp rax,0'); //did SetEvent fail?
    script.add('je end'); //if so, quit

    script.add('workloop:');
    {
      //sharedblock layout:
      0: HasDataEventHandle  : Set by CE when it has configured a datablock
      8: HasProcessedDataEventHandle :Set by the client when it has read out the datablock
      10: CMD   (0=reinit: resizes the shared memory block so there is enough data available for calls, 1=execute , 2+=terminate
      18: If reinit, contains the new handle, else address to execute
      20: Return value of the function/CMD
      28: parameters....
      }

      //wait for data
    if processhandler.is64Bit then
    begin
      script.add('mov rcx,[rbp+0]');
      script.add('mov rcx,[rcx]');
      script.add('mov rdx,FFFFFFFF');
    end
    else
    begin
      script.add('push ffffffff');
      script.add('mov eax,[ebp+0]');
      script.add('push [eax]');
    end;
    script.add('call WaitForSingleObject');

    script.add('cmp rax,0');
    script.add('jne end');   //invalid result. give up

    //if realloc sharedblock then jmp reinit with new rcx
    script.add('mov rax,[rbp]'); //get shared mem
    script.add('cmp [rax+10],0');  //check CMD.  Is it 0?
    script.add('jne notreinit');

    //reinit call
    if processhandler.is64bit then
    begin
      script.add('mov rcx,[rbp+8]');  //get the current handle
      script.add('mov [rbp+10],rcx'); //and write it to the old handle
    end
    else
    begin
      script.add('mov ecx,[ebp+4]');
      script.add('mov [ebp+8],ecx');
    end;

    script.add('mov rcx,[rax+18]'); //get the new handle
    if processhandler.is64Bit then
      script.add('mov [rbp+8],rcx') //put it in place of the current handle
    else
      script.add('mov [ebp+4],ecx');

    script.add('jmp reinit');

    script.add('notreinit:');
    //make sure it's 1, else it's either the terminate command, or garbage. either way, terminate

    script.add('cmp [rax+10],1');
    script.add('jne end');

    //execute the function
    script.add('lea rcx,[rax+28]'); //load rcx with the pointer to the parameters
    if processhandler.is64Bit=false then
      script.add('push ecx');

    script.add('call [rax+18]'); //call the stub

    //save the result.
    script.add('mov rbx,[rbp]');
    script.add('mov [rbx+20],rax');

    if processhandler.is64Bit then
      script.add('mov rcx,[rbx+8]')  //HasProcessedEventHandle
    else
      script.add('push [ebx+8]');

    script.add('call SetEvent'); //let CE know it can send some new commands

    script.add('jmp workloop');
    script.add('end:');
    if processhandler.is64Bit then
    begin
      script.add('add rsp,48');
      script.add('ret');
    end
    else
      script.add('ret 4');

    //debug:
   // clipboard.AsText:=script.text;

    if autoAssemble(script,false,true,false,false,disableinfo) then
    begin
      //create thread
      ExecutorThreadExecMemory:=disableinfo.allocs[0].address;
      ExecutorThreadHandle:=CreateRemoteThread(processhandle,nil,0,pointer(ExecutorThreadExecMemory), pointer(remoteMemMapHandle),0,executorThreadID);

      if (ExecutorThreadHandle=0) or (ExecutorThreadHandle=INVALID_HANDLE_VALUE) then
        raise exception.create('Failure creating executor thread');
    end
    else
      raise exception.create('Failure assembling remote executor');


  finally
    script.free;
    disableinfo.free;
  end;
  {$endif}
end;

destructor TRemoteExecutor.destroy;
begin
  {$ifdef windows}
  //terminate the thread if it exists and free the memory
  if (ExecutorThreadHandle<>0) and (ExecutorThreadHandle=INVALID_HANDLE_VALUE) then
    TerminateThread(ExecutorThreadHandle,$101);

  if (targetedProcessID=processid) then
  begin
    VirtualFreeEx(processhandle, pointer(ExecutorThreadExecMemory),0,MEM_RELEASE);

    if sharedMemory<>nil then
    begin
      //close the handles
      if (sharedMemory^.HasProcessedDataEventHandle<>0) and (sharedMemory^.HasProcessedDataEventHandle<>INVALID_HANDLE_VALUE) then
      begin
        DuplicateHandle(processhandle, sharedMemory^.HasProcessedDataEventHandle,0,nil, 0, false, DUPLICATE_CLOSE_SOURCE);
        sharedMemory^.HasProcessedDataEventHandle:=0;
      end;

      if (sharedMemory^.HasDataEventHandle<>0) and (sharedMemory^.HasDataEventHandle<>INVALID_HANDLE_VALUE) then
      begin
        DuplicateHandle(processhandle, sharedMemory^.HasDataEventHandle,0,nil, 0, false, DUPLICATE_CLOSE_SOURCE);
        sharedMemory^.HasProcessedDataEventHandle:=0;
      end;
    end;

    if (remoteMemMapHandle<>0) and (remoteMemMapHandle<>INVALID_HANDLE_VALUE) then
      DuplicateHandle(processhandle, remoteMemMapHandle,0,nil, 0, false, DUPLICATE_CLOSE_SOURCE);

  end;

  if (HasProcessedDataEventHandle<>0) and (HasProcessedDataEventHandle<>INVALID_HANDLE_VALUE) then
    closeHandle(HasProcessedDataEventHandle);

  if (HasDataEventHandle<>0) and (HasDataEventHandle<>INVALID_HANDLE_VALUE) then
    closeHandle(HasDataEventHandle);

  if sharedMemory<>nil then
    UnmapViewOfFile(sharedMemory);

  closehandle(memmap);
  {$endif}
end;



function remoteexecutor_waitTillDoneAndGetResult(L: PLua_state): integer; cdecl;
var
  re: TRemoteExecutor;
  timeout: DWORD;
  buffers: array of TExecBuffer;

  r: qword;
  i: integer;
begin
  re:=luaclass_getClassObject(L);
  if lua_gettop(L)>0 then
    timeout:=lua_tointeger(L,1)
  else
    timeout:=INFINITE;

  setlength(buffers,0);
  try
    r:=re.waitTillDoneAndGetResult(timeout, buffers);

    lua_pushinteger(L,r);
    //return the buffers as tables
    for i:=0 to length(buffers)-1 do
    begin
      lua_createtable(L,2,0);
      lua_pushstring(L,'Data');
      CreateByteTableFromPointer(L,pointer(buffers[i].data),buffers[i].length);
      lua_settable(L,-3);

      lua_pushstring(L,'Parameter');
      lua_pushinteger(L,buffers[i].parameternr);
      lua_settable(L,-3);
    end;

    result:=1+length(buffers);
  except
    on e:Exception do
    begin
      lua_pushnil(L);
      lua_pushstring(L,pchar(e.message));
      result:=2;
    end;
  end;

end;

function remoteexecutor_executeStub(L: PLua_state): integer; cdecl;
  function invalidStubData:integer;
  begin
    lua_pushnil(L);
    lua_pushstring(L,'Invalid stubdata');
    exit(2);
  end;

var
  re: TRemoteExecutor;
  stubdata: TStubdata;
  values: array of Tvalueinfo;
  timeout: dword;
  waittilldone: boolean;
  i,len: integer;

  intvalue: qword;
  floatvalue: single absolute intvalue;
  doublevalue: double absolute intvalue;

  widecharstrings: array of string;
  objectlist: array of pointer;
  obj: pointer;
begin
  re:=luaclass_getClassObject(L);
  result:=0;
  try
    if (lua_gettop(L)>=1) and (lua_istable(L,1)) then
    begin
      //get the stubdata
      lua_pushstring(L,'StubAddress');
      lua_gettable(L,1);

      if lua_isnil(L,-1) then exit(invalidStubData);
      stubdata.address:=lua_tointeger(L,-1);

      lua_pop(L,1);

      lua_pushstring(L,'Parameters');
      lua_gettable(L,1);
      if not lua_istable(L,-1) then exit(invalidStubData);


      len:=lua_objlen(L,-1);
      setlength(stubdata.parameters, len);

      for i:=0 to len-1 do
      begin
        lua_pushinteger(L,i+1);
        lua_gettable(L,-2);
        stubdata.parameters[i]:=lua_tointeger(L,-1);
        lua_pop(L,1);
      end;
      lua_pop(L,1);

      if (lua_gettop(L)>=2) and (not lua_isnil(L,2)) then
      begin
        if lua_istable(L,2) then
        begin
          len:=lua_objlen(L,2);

          if len<>length(stubdata.parameters) then
          begin
            lua_pushnil(L);
            lua_pushstring(L,'Incorrect parameter count');
            exit(2);
          end;

          setlength(values, len);

          for i:=0 to len-1 do
          begin
            lua_pushinteger(L,i+1);
            lua_gettable(L,2);

            //interpret the value based on the given stubdata parametertype
            //0: integer/pointer
            //1: float
            //2: double
            //3: asciistring (turns into 0:pointer after writing the string)
            //4: widestring
            //<0: bytetable

            case stubdata.parameters[i] of
              0: values[i].value:=lua_tointeger(L,-1);
              1:
              begin
                floatvalue:=lua_tonumber(L,-1);
                values[i].value:=intvalue;
              end;

              2:
              begin
                doublevalue:=lua_tonumber(L,-1);
                values[i].value:=intvalue;
              end;

              3: values[i].value:=qword(lua.lua_tostring(L,-1));
              4:
              begin
                setlength(widecharstrings, length(widecharstrings)+1);
                widecharstrings[length(widecharstrings)-1]:=Lua_ToString(L,-1);
                values[i].value:=qword(@widecharstrings[length(widecharstrings)-1][1]);
              end;

              else
              begin
                if stubdata.parameters[i]<0 then
                begin
                  if lua_istable(L,-1) then
                  begin
                    //create a memoryblock that encompasses this table
                    len:=stubdata.parameters[i] and $1fffffff;

                    if len=0 then //no length given. Rely on the provided table info
                    begin
                      //get the length from the table instead
                      len:=lua_objlen(L,-1);
                    end; //else use the predefined size

                    getmem(obj, len);

                    readBytesFromTable(L,lua_gettop(L),obj,len);

                    values[i].value:=qword(obj);
                    values[i].bytesize:=len;

                    setlength(objectlist,length(objectlist)+1);
                    objectlist[length(objectlist)-1]:=obj; //so it can be freed aftrerwards
                  end
                  else
                  begin
                    lua_pushnil(L);
                    lua_pushstring(L,pchar('Parameter '+inttostr(i+1)+' is supposed to be a table'));
                    exit(2);
                  end;
                end;
              end;

            end;

            lua_pop(L,1);
          end;
        end
        else
        begin
          lua_pushnil(L);
          lua_pushstring(L,'Invalid value field. Has to be a table');
          exit(2);
        end;
      end;

      if (lua_gettop(L)>=3) and (not lua_isnil(L,3)) then
        timeout:=lua_tointeger(L,3)
      else
        timeout:=INFINITE;

      if (lua_gettop(L)>=4) and (not lua_isnil(L,4)) then
        waittilldone:=not lua_toboolean(L,4)
      else
        waittilldone:=true;

      try
        re.executeStub(stubdata, values, timeout);
        if waittilldone then
        begin
          lua_pop(L,lua_gettop(L));
          lua_pushinteger(L, timeout);
          exit(remoteexecutor_waitTillDoneAndGetResult(L));
        end
        else
          exit(0);

      except
        on e:exception do
        begin
          lua_pushnil(L);
          lua_pushstring(L, pchar(e.Message));
          result:=2;
        end;
      end;
    end;

  finally
    for i:=0 to length(objectlist)-1 do
      freemem(objectlist[i]);
  end;
end;

function lua_createStubExecutor(L: PLua_state): integer; cdecl;
begin
  try
    luaclass_newClass(L, TRemoteExecutor.create);
    result:=1;
  except
    on e:exception do
    begin
      lua_pushnil(L);
      lua_pushstring(L, pchar(e.message));
      result:=2;
    end;
  end;

end;

procedure remoteexecutor_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'executeStub', remoteexecutor_executeStub);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'waitTillDoneAndGetResult', remoteexecutor_waitTillDoneAndGetResult);

end;

procedure InitializeLuaRemoteExecutor;
begin
  {$ifdef windows}
  lua_register(LuaVM, 'createStubExecutor', lua_createStubExecutor);
  lua_register(LuaVM, 'createRemoteExecutor', lua_createStubExecutor); //just for those that dislike stubs
  {$endif}
end;

initialization
  luaclass_register(TRemoteExecutor, remoteexecutor_addMetaData);

end.

