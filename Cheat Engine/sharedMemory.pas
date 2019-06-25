unit sharedMemory;

{$mode delphi}

interface

uses
  windows, Classes, SysUtils, symbolhandler, autoassembler, frmautoinjectunit,
  cefuncproc, NewKernelHandler, Clipbrd, commonTypeDefs;

const FILE_MAP_EXECUTE = $20;

function allocateSharedMemory(name: string; size: integer=4096): pointer;
function allocateSharedMemoryIntoTargetProcess(name: string; size: integer=4096): pointer;
function createSharedMemory(name: string; size: integer): THandle;

implementation

uses ProcessHandlerUnit;


function createSharedMemory(name: string; size: integer): THandle;
begin
  result:=CreateFileMapping(INVALID_HANDLE_VALUE,nil,PAGE_EXECUTE_READWRITE,0,size,pchar(name));
end;

function allocateSharedMemory(name: string; size: integer=4096): pointer;
var
  access: dword;
  h: thandle;
begin
  access:=FILE_MAP_EXECUTE or FILE_MAP_READ or FILE_MAP_WRITE;


  //if name does not exist allocate it first
  h:=OpenFileMapping(access, false, pchar(name));

  if h=0 then
    h:=createSharedMemory(name, size);

  result:=MapViewOfFile(h,access,0,0,0);
end;

function allocateSharedMemoryIntoTargetProcess(name: string; size: integer=4096): pointer;
var s: tstringlist;
  CEAllocArray: TCEAllocArray;
  ExceptionList: TCEExceptionListArray;
  i: integer;
  starttime: qword;
  x: ptruint;
  address: ptruint;

  access: dword;

  h: THandle;
begin
  access:=FILE_MAP_EXECUTE or FILE_MAP_READ or FILE_MAP_WRITE;


  //if name does not exist allocate it first
  h:=OpenFileMapping(access, false, pchar(name));

  if h=0 then
    h:=createSharedMemory(name, size);


  result:=nil;
  s:=tstringlist.create;


  s.add('alloc(allocator,1024)');
  s.add('alloc(sname,128)');
  s.add('alloc(address, 4)');
  s.add('label(error)');
  s.add('sname:');
  s.add('db '''+name+''',0');
  s.add('');
  s.add('address:');
  if processhandler.is64Bit then
    s.add('dq 0')
  else
    s.add('dd 0');

  s.add('');
  s.add('allocator:');

  if processhandler.is64bit then
  begin
    s.add('sub rsp,#64');

    s.add('mov rcx,'+inttohex(access,16));
    s.add('mov rdx,0');
    s.add('mov r8,sname');
    s.add('call OpenFileMappingA');
    s.add('cmp rax,0');
  end
  else
  begin
    s.add('push sname');
    s.add('push 0'); //inherit handle
    s.add('push '+inttohex(access,8));
    s.add('call OpenFileMappingA');
    s.add('cmp eax,0');
  end;
  s.add('je short error');
  s.add('');
  if processhandler.is64bit then
  begin
    s.add('mov rcx,eax');
    s.add('mov rdx,'+inttohex(access,16));
    s.add('mov r8,0');
    s.add('mov r9,0');
    s.add('xor rax,rax');

    s.add('mov [rsp],rcx');    //just to be sure...
    s.add('mov [rsp+8],rdx');
    s.add('mov [rsp+10],r8');
    s.add('mov [rsp+18],r9');
    s.add('mov [rsp+20],rax');
    s.add('call MapViewOfFile');
    s.add('mov [address],rax');

    s.add('add rsp,#64');
  end
  else
  begin
    s.add('push 0');
    s.add('push 0');
    s.add('push 0');
    s.add('push '+inttohex(access,8));
    s.add('push eax');
    s.add('call MapViewOfFile');
    s.add('mov [address],eax');

  end;

  s.add('ret');
  s.add('');
  s.add('error:');
  s.add('call GetLastError');
  s.add('mov [address],eax');
  if processhandler.is64bit then
    s.add('add rsp,#64');
  s.add('ret');

  s.add('createThread(allocator)');

 // Clipboard.AsText:=s.text;

  try
    setlength(CEAllocArray,0);
    if autoassemble(s,false, true, false, false, CEAllocArray, exceptionlist) then
    begin
      starttime:=GetTickCount64;
      for i:=0 to length(ceallocarray)-1 do
      begin

        if ceallocarray[i].varname='address' then
        begin
          while gettickcount64-starttime<10*1000 do
          begin
            //poll if address is still 0

            if ReadProcessMemory(processhandle, pointer(ceallocarray[i].address), @address, processhandler.pointersize, x) then
            begin
              if address<>0 then
              begin
                if address>=$10000 then
                  result:=pointer(address);

                break;
              end;


            end else exit; //unreadable...
          end;
          break;
        end;
      end;

      VirtualFreeEx(processhandle, pointer(ceallocarray[0].address), 0, MEM_DECOMMIT);
    end;
  except
  end;
end;

end.

