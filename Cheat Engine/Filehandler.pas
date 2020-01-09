unit Filehandler;

{$MODE Delphi}

{
implement replaced handlers for ReadProcssMemory and WriteProcessMemory so it
reads/writes to the file instead
}

interface

uses
  {$ifdef darwin}
  macport,
  {$endif}
  {$ifdef windows}
  jwawindows, windows,
  {$endif}
  LCLIntf, syncobjs, sysutils, Classes;

function ReadProcessMemoryFile(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer;  nSize: DWORD; var lpNumberOfBytesRead: ptruint): BOOL; stdcall;
function WriteProcessMemoryFile(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesWritten: ptruint): BOOL; stdcall;
function VirtualQueryExFile(hProcess: THandle; lpAddress: Pointer; var lpBuffer: TMemoryBasicInformation; dwLength: DWORD): DWORD; stdcall;
procedure CommitChanges(fn: string='');

var filename: string;
    filedata: TMemorystream;
    filebaseaddress: ptruint;
    //filehandle: thandle;
    bigendianfileaccess: boolean=false;
    blockfilehandlerpopup: boolean=false;

implementation

uses dialogs, controls;

procedure CommitChanges(fn: string='');
begin
  if filedata<>nil then
  begin
    if fn='' then
      fn:=filename;

    filedata.SaveToFile(fn);
  end;
end;

var filecs: tcriticalsection; //only 1 filehandle, so make sure rpm does not change the filepointer while another is still reading it
function ReadProcessMemoryFile(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer;  nSize: DWORD; var lpNumberOfBytesRead: ptruint): BOOL; stdcall;
var filesize,ignore:dword;

    i: integer;

    b: pdword;


    t: dword;

    ba: ptruint;

    s: integer;

begin
//ignore hprocess
  if filedata=nil then
  begin
    exit(false);
    lpNumberOfBytesRead:=0;
  end;

  {$ifdef windows}

  if hprocess=GetCurrentProcess then
    exit(windows.ReadProcessMemory(hProcess, lpBaseAddress, lpBuffer, nSize, lpNumberOfBytesRead));
  {$endif}

  result:=false;
  ba:=ptruint(lpBaseAddress);
  inc(ba,ptruint(filedata.Memory));
  dec(ba,filebaseaddress);

  filesize:=filedata.Size;

  if ptruint(lpbaseaddress)<filebaseaddress then exit;
  if ptrUint(lpbaseaddress)>=filebaseaddress+filesize then exit;

  s:=nsize;

  if ptrUint(lpbaseaddress)+s>=filebaseaddress+filesize then
  begin
    ZeroMemory(lpBuffer, nsize);
    dec(s, ((ptrUint(lpbaseaddress)+s)-(filebaseaddress+filesize)));
  end;

  if s<=0 then exit;

  nsize:=s;

  CopyMemory(lpbuffer,pointer(ba),nsize);
  lpNumberOfBytesRead:=nsize;

  result:=true;

  if bigendianfileaccess then
  begin
    i:=0;
    while i<nSize do
    begin
      if (nsize-i)>=4 then
      begin
        b:=@PByteArray(lpBuffer)[i];
        t:=b^;

        {$ifdef cpu64}
        asm
          push rax
          xor rax,rax
          mov eax,t
          bswap eax
          mov t,eax
          pop rax
        end;
        {$else}
        asm
          push eax
          xor eax,eax
          mov eax,t
          bswap eax
          mov t,eax
          pop eax
        end;

        {$endif}

        b^:=t;
      end;

      inc(i, 4);
    end;
  end;

end;

function WriteProcessMemoryFile(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesWritten: ptruint): BOOL; stdcall;
var filesize,ignore:dword;

    i,o: integer;


    b: pdword;


    t: dword;

    ba: ptruint;
    s: integer;

begin
//ignore hprocess
  {$ifdef windows}
  if hprocess=GetCurrentProcess then
    exit(windows.WriteProcessMemory(hProcess, lpBaseAddress, lpBuffer, nSize, lpNumberOfBytesWritten));
  {$endif}

  if filedata=nil then
  begin
    exit(false);
    lpNumberOfBytesWritten:=0;
  end;


  result:=false;
  ba:=ptruint(lpBaseAddress);
  inc(ba,ptruint(filedata.Memory));
  dec(ba,filebaseaddress);

  filesize:=filedata.Size;


  s:=nsize;

  if ptrUint(lpbaseaddress)<filebaseaddress then exit(False);

  if ptrUint(lpbaseaddress)+s>(filebaseaddress+filesize) then
  begin
    if (MainThreadID=GetCurrentThreadId) and (not blockfilehandlerpopup) then
    begin
      i:=ptrUint(lpbaseaddress-filebaseaddress)+s;
      if MessageDlg('Change the file size to '+inttostr(i)+' bytes?',mtConfirmation,[mbyes,mbno],0)=mryes then
      begin
        filedata.SetSize(i);
        ZeroMemory(pointer(ptruint(filedata.Memory)+filesize), i-filesize);
        filesize:=filedata.size;
      end
      else
        dec(s, ((ptrUint(lpbaseaddress)+s)-filesize));
    end
    else
      dec(s, ((ptrUint(lpbaseaddress)+s)-filesize));
  end;

  if s<=0 then exit;

  nsize:=s;

  CopyMemory(pointer(ba),lpbuffer,nsize);
  lpNumberOfBytesWritten:=nsize;

  result:=true;

  if bigendianfileaccess then
  begin
    i:=0;
    while i<nSize do
    begin
      if (nsize-i)>=4 then
      begin
        b:=@PByteArray(lpBuffer)[i];
        t:=b^;

        {$ifdef cpu64}
        asm
          push rax
          xor rax,rax
          mov eax,t
          bswap eax
          mov t,eax
          pop rax
        end;
        {$else}
        asm
          push eax
          xor eax,eax
          mov eax,t
          bswap eax
          mov t,eax
          pop eax
        end;

        {$endif}

        b^:=t;
      end;

      inc(i, 4);
    end;
  end;

end;

function VirtualQueryExFile(hProcess: THandle; lpAddress: Pointer; var lpBuffer: TMemoryBasicInformation; dwLength: DWORD): DWORD; stdcall;
var ignore: dword;
    filesize: ptrUint;
begin
  lpBuffer.BaseAddress:=pointer((ptrUint(lpAddress) div $1000)*$1000);

  if ptruint(lpAddress)<filebaseaddress then
  begin
    lpbuffer.AllocationBase:=pointer(0);
    lpbuffer.AllocationProtect:=0;
    lpbuffer.State:=MEM_FREE;
    lpbuffer.protect:=PAGE_NOACCESS;
    lpbuffer._Type:=0;
    lpbuffer.RegionSize:=filebaseaddress-ptruint(lpBuffer.BaseAddress);
    result:=dwlength;
  end
  else
  begin
    filesize:=filedata.Size; // getfilesize(hprocess,@ignore);
    lpbuffer.AllocationBase:=pointer(filebaseaddress);
    lpbuffer.AllocationProtect:=PAGE_EXECUTE_READWRITE;
    lpbuffer.RegionSize:=filesize-ptruint(lpBuffer.BaseAddress-lpbuffer.AllocationBase);
    if (lpbuffer.RegionSize mod 4096)>0 then
      lpbuffer.RegionSize:=lpbuffer.RegionSize+($1000-lpbuffer.RegionSize mod $1000);

    lpbuffer.State:=mem_commit;
    lpbuffer.Protect:=PAGE_EXECUTE_READWRITE;
    lpbuffer._Type:=MEM_PRIVATE;

    if (ptrUint(lpAddress)>filesize+filebaseaddress) //bigger than the file
    then
    begin
      zeromemory(@lpbuffer,dwlength);
      result:=0
    end
    else
      result:=dwlength;
  end;

end;

initialization
  filecs:=tcriticalsection.create;

finalization
  filecs.free;


end.






