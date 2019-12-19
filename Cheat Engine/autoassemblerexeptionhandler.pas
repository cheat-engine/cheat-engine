unit autoassemblerexeptionhandler;

{$mode delphi}

interface

uses
  {$ifdef darwin}
  macport,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}
  Classes, SysUtils, NewKernelHandler, Clipbrd;

type
  TAAExceptionInfo=record
    trylabel: string;
    exceptlabel: string;
  end;

  TAAExceptionInfoList=array of TAAExceptionInfo;


procedure InitializeAutoAssemblerExceptionHandler;
procedure AutoAssemblerExceptionHandlerRemoveExceptionRange(startaddress: ptruint);
procedure AutoAssemblerExceptionHandlerAddExceptionRange(tryaddress: ptruint; exceptionAddress: ptruint);
procedure AutoAssemblerExceptionHandlerApplyChanges;

function AutoAssemblerExceptionHandlerHasEntries: boolean;

implementation

uses autoassembler, symbolhandler, ProcessHandlerUnit, commonTypeDefs;

type
  TAAExceptionListEntry=record
    tryaddress: ptruint;
    exceptionaddress: ptruint;
  end;
  TAAExceptionList=array of TAAExceptionListEntry;

  TAAExceptionListEntry32=record
    tryaddress: dword;
    exceptionaddress: dword;
  end;

  TAAExceptionListEntry64=record
    tryaddress: qword;
    exceptionaddress: qword;
  end;

  TAAExceptionList32=array of TAAExceptionListEntry32;
  TAAExceptionList64=array of TAAExceptionListEntry64;

var
  pid: dword;
  signatureaddress: ptruint;
  setlistaddress: ptruint;
  listaddress: ptruint;

  exceptionlist: TAAExceptionList;

function AutoAssemblerExceptionHandlerHasEntries: boolean;
begin
  result:=(pid=processid) and (length(Exceptionlist)>0);
end;

procedure AutoAssemblerExceptionHandlerAddExceptionRange(tryaddress: ptruint; ExceptionAddress: ptruint);
var i: integer;
begin
  i:=length(exceptionlist);
  setlength(exceptionlist, i+1);
  exceptionlist[i].tryaddress:=tryaddress;
  exceptionlist[i].exceptionaddress:=ExceptionAddress;
end;

procedure AutoAssemblerExceptionHandlerRemoveExceptionRange(startaddress: ptruint);
var i,j: integer;
begin
  i:=0;
  while i<length(exceptionlist) do
  begin
    if exceptionlist[i].tryaddress=startaddress then
    begin
      for j:=i to length(exceptionlist)-2 do
        exceptionlist[j]:=exceptionlist[j+1];

      setlength(exceptionlist, length(Exceptionlist)-1);
    end
    else
      inc(i);
  end;
end;

procedure AutoAssemblerExceptionHandlerApplyChanges;
var
  params32: record
    list: dword;
    listsize: integer;
  end;

  params64: record
    list: qword;
    listsize: integer;
  end;

  params: pointer;
  th: thandle;

  templist32: TAAExceptionList32;
  templist64: TAAExceptionList64;
  i: integer;

  oldlist: ptruint;
  x: ptruint;
  y: dword;
begin
  //setList
  InitializeAutoAssemblerExceptionHandler;

  {$ifdef windows}

  oldlist:=0;
  readprocessmemory(processhandle, pointer(listaddress), @oldlist, 8,x);

  if processhandler.is64Bit then
  begin
    setlength(templist64, length(exceptionlist));
    for i:=0 to length(exceptionlist)-1 do
    begin
      templist64[i].tryaddress:=exceptionlist[i].tryaddress;
      templist64[i].exceptionaddress:=exceptionlist[i].exceptionaddress;
    end;
    params64.listsize:=length(exceptionlist);
    if params64.listsize=0 then
      params64.list:=0
    else
    begin
      params64.list:=ptruint(VirtualAllocEx(processhandle, nil,length(templist64)*sizeof(TAAExceptionList64),mem_commit or mem_Reserve, PAGE_READWRITE));
      writeprocessmemory(processhandle, pointer(ptruint(params64.list)), @templist64[0], length(templist64)*sizeof(TAAExceptionListEntry64), x);
    end;

    params:=VirtualAllocEx(processhandle, nil,sizeof(params64),mem_commit or MEM_RESERVE, PAGE_READWRITE);
    writeprocessmemory(processhandle, pointer(params), @params64, sizeof(params64), x);
  end
  else
  begin
    setlength(templist32, length(exceptionlist));
    for i:=0 to length(exceptionlist)-1 do
    begin
      templist32[i].tryaddress:=exceptionlist[i].tryaddress;
      templist32[i].exceptionaddress:=exceptionlist[i].exceptionaddress;
    end;
    params32.listsize:=length(exceptionlist);
    if params32.listsize=0 then
      params32.list:=0
    else
    begin
      params32.list:=ptruint(VirtualAllocEx(processhandle, nil,length(templist32)*sizeof(TAAExceptionList32),mem_commit or mem_Reserve, PAGE_READWRITE));
      writeprocessmemory(processhandle, pointer(ptruint(params32.list)), @templist32[0], length(templist32)*sizeof(TAAExceptionListEntry32), x);
    end;

    params:=VirtualAllocEx(processhandle, nil,sizeof(params32),mem_commit or MEM_RESERVE, PAGE_READWRITE);
    writeprocessmemory(processhandle, pointer(params), @params32, sizeof(params32), x);
  end;

  th:=CreateRemoteThread(processhandle, nil, 0, pointer(setlistaddress),params,0,y);

  if waitforsingleobject(th,5000)<>WAIT_OBJECT_0 then
    raise exception.create('Failure to set the exception list. Thread error');

  //free the old list if there was one
  if oldlist<>0 then
    VirtualFreeEx(processhandle, pointer(oldlist), 0, MEM_RELEASE);

  //and the parameters
  VirtualFreeEx(processhandle, params, 0, MEM_RELEASE); //(not the list, on purpose)
  {$endif}
end;

procedure InitializeAutoAssemblerExceptionHandler;
var
  init: tstringlist;

  eh: ptruint;
  signature: pchar;
  x: ptruint;

  ehallocated: boolean;
  allocs: TCEAllocArray;
  exceptions: TCEExceptionListArray; //will hopefully be 0 long
  i: integer;

begin
  {$ifdef windows}
  //check if CEAAExceptionHandler is already defined, and if not, define it here

  ehallocated:=false;


  if (pid=processid) and (signatureaddress<>0) then
  begin
    getmem(signature,5);
    if readprocessmemory(processhandle, pointer(signatureaddress), signature,4,x) then
    begin
      signature[4]:=#0;
      if signature='AAEH' then ehallocated:=true;
    end;

    FreeMemAndNil(signature);
  end;

  if ehallocated=false then
  begin
    setlength(exceptionlist,0);

    init:=tstringlist.create;
    init.add('alloc(Signature,4)');
    init.add('registersymbol(debugnr)');
    init.add('registersymbol(exceptioncount)');
    init.add('registersymbol(exceptionhandler)');
    init.add('registersymbol(vehid)');
    init.add('alloc(MREW,8)');    //todo: in XP use Critical section instead
    init.add('alloc(exceptioncount,4)');
    init.add('alloc(debugnr,4)');
    init.add('alloc(vehid,8)');
    init.add('alloc(List,8)');
    init.add('alloc(ListSize,8)');
    init.add('alloc(ExceptionHandler,1024)');
    init.add('alloc(SetList,1024)');
    init.add('alloc(registereh,128)');

    init.add('');
    init.add('Signature:');
    init.add('db ''AAEH'''); //signature
    init.add('');
    init.add('MREW:');
    init.add('dq 0');

    init.add('List:');
    init.add('dq 0');


    init.add('ListSize:'); //number of entries in the list
    init.add('dd 0');

    init.add('SetList:');
    if processhandler.is64bit then
    begin

      init.add('sub rsp,28');

      //rsp=to call scratch space 1
      //rsp+8=to call scratch space 2
      //rsp+10=to call scratch space 3
      //rsp+18=to call scratch space 4
      //rsp+20=alignment

      //rsp+28=return address
      //rsp+30=local scratchspace 1 (params)
      init.add('mov [rsp+30],rcx');
      init.add('mov rcx,MREW');
      init.add('call AcquireSRWLockExclusive');

      init.add('mov rax,[rsp+30]');
      init.add('mov r8,[rax]'); //list
      init.add('mov r9,[rax+8]'); //listsize

      init.add('mov [List],r8');
      init.add('mov [ListSize],r9');

      init.add('mov rcx,MREW');
      init.add('call ReleaseSRWLockExclusive');

      init.add('add rsp,28');
      init.add('ret');
    end
    else
    begin
      init.add('push ebp');
      init.add('mov ebp,esp');
      init.add('push MREW');
      init.add('call AcquireSRWLockExclusive');

      init.add('mov eax,[ebp+8]');
      init.add('mov ebx,[eax]');   //list
      init.add('mov ecx,[eax+4]'); //listsize

      init.add('mov [List],ebx');
      init.add('mov [ListSize],ecx');
      init.add('push MREW');
      init.add('call ReleaseSRWLockExclusive');
      init.add('pop ebp');
      init.add('ret 4');
    end;


    init.add('ExceptionHandler:');
    if processhandler.is64bit then
    begin
      init.add('sub rsp,28');
      //free to edit: RCX, RDX, R8, R9, R10, R11

      //rsp=to call scratch space 1
      //rsp+8=to call scratch space 2
      //rsp+10=to call scratch space 3
      //rsp+18=to call scratch space 4
      //rsp+20=alignment

      //rsp+28=return address
      //rsp+30=local scratchspace 1 (ExceptionInfo)
      init.add('mov [rsp+30],rcx');

      init.add('mov rcx,MREW');
      init.add('call AcquireSRWLockShared');

      init.add('cmp [List],0');
      init.add('je ExceptionHandler_exit');

      init.add('mov rax,[rsp+30]');  //rax=ExceptionInfo pointer
      init.add('mov rax,[rax+8]');  //rax=ContextRecord
      init.add('lea rax,[rax+f8]');  //rax points to RIP

      init.add('mov r8,[List]');
      init.add('mov rcx,[ListSize]');

      init.add('next:');
      init.add('mov r9,[r8]');  //try address
      init.add('mov r10,[r8+8]');  //except address
      init.add('cmp [rax],r9');
      init.add('jb nomatch');

      init.add('cmp [rax],r10');
      init.add('jb match');

      init.add('nomatch:');
      init.add('add r8,10');
      init.add('loop next');

      init.add('xor rax,rax');  //end of the list and not in it
      init.add('jmp ExceptionHandler_exit');

      init.add('match:');
      init.add('mov [rax],r10');
      init.add('mov eax,ffffffff');

      init.add('ExceptionHandler_exit:');
      init.add('mov [rsp+30],rax');  //save the return value
      init.add('mov rcx,MREW');
      init.add('call ReleaseSRWLockShared');
      init.add('mov rax,[rsp+30]');  //restore the return value
      init.add('add rsp,28');
      init.add('ret');
    end
    else
    begin
      init.add('push ebp');
      init.add('mov ebp,esp'); //ebp+4 = return, ebp+8=ExceptionInfo
      init.add('push ebx');
      init.add('push ecx');
      init.add('push edx');
      init.add('push esi');
      init.add('push MREW');
      init.add('call AcquireSRWLockShared'); //read lock

      init.add('cmp [List],0');   //check if it's an empty list
      init.add('je ExceptionHandler_exit');

      //check that the EIP/RIP falls within a known region
      //if so, change rip to go to that region
      //if not, continue searching

      init.add('mov eax,[ebp+8]');

      //[eax+0]=ExceptionRecord
      //[eax+4]=ContextRecord
      init.add('mov eax,[eax+4]');


      //[eax+b8]=eip
      init.add('lea eax,[eax+b8]');
      //now check if this eip(stoed in eax) is in the list
      init.add('mov esi,[List]');
      init.add('mov ecx,[ListSize]');


      init.add('next:');
      init.add('mov ebx,[esi]'); //try address
      init.add('mov edx,[esi+4]'); //except address
      init.add('cmp [eax],ebx');
      init.add('jb nomatch');

      init.add('cmp [eax],edx');
      init.add('jb match');

      init.add('nomatch:');
      init.add('add esi,8');
      init.add('loop next');
      init.add('mov eax,0'); //not in the list, continue unhandled
      init.add('jmp ExceptionHandler_exit');

      init.add('match:');
      init.add('mov [eax],edx'); //change eip to the exception handler
      init.add('mov eax,ffffffff'); //continue execution

      init.add('ExceptionHandler_exit:');
      init.add('push eax');  //store the return value
      init.add('push MREW');
      init.add('call ReleaseSRWLockShared'); //read lock release
      init.add('pop eax'); //restore the return value

      init.add('pop esi');
      init.add('pop edx');
      init.add('pop ecx');
      init.add('pop ebx');

      init.add('pop ebp');
      init.add('ret 4');
    end;

    //addvectoredexceptionhandler
    init.add('registereh:');
    if processhandler.is64bit then
    begin
      init.add('sub rsp,28');
      init.add('mov rcx,1');
      init.add('mov rdx,ExceptionHandler');
      init.add('call AddVectoredExceptionHandler');
      init.add('mov [vehid],rax');
      init.add('add rsp,28');
      init.add('ret');
    end
    else
    begin
      init.add('push ExceptionHandler');
      init.add('push 1');
      init.add('call AddVectoredExceptionHandler');
      init.add('mov [vehid],eax');
      init.add('ret 4');
    end;

    init.add('createthreadandwait(registereh)');

    //Clipboard.AsText:=init.text;

    if autoassemble(init, false, true,false,false,allocs, exceptions)=false then
      raise exception.create('Failure to assemble exception handler');

    for i:=0 to length(allocs)-1 do
      if lowercase(allocs[i].varname)='signature' then
        signatureaddress:=allocs[i].address
      else
      if lowercase(allocs[i].varname)='list' then
        listaddress:=allocs[i].address
      else
      if lowercase(allocs[i].varname)='setlist' then
        setlistaddress:=allocs[i].address;


    init.free;

    pid:=processid;
  end;
  {$else}
  raise exception.create('{$try}/{$except} is not yet implemented for this system');
  {$endif}
end;

end.

