unit Filehandler;

{$MODE Delphi}

{
implement replaced handlers for ReadProcssMemory and WriteProcessMemory so it
reads/writes to the file instead
}

interface

uses windows, LCLIntf, syncobjs;

function ReadProcessMemoryFile(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer;  nSize: DWORD; var lpNumberOfBytesRead: DWORD): BOOL; stdcall;
function WriteProcessMemoryFile(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesWritten: DWORD): BOOL; stdcall;
function VirtualQueryExFile(hProcess: THandle; lpAddress: Pointer; var lpBuffer: TMemoryBasicInformation; dwLength: DWORD): DWORD; stdcall;

var filehandle: thandle;

implementation

var filecs: tcriticalsection; //only 1 filehandle, so make sure rpm does not change the filepointer while another is still reading it
function ReadProcessMemoryFile(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer;  nSize: DWORD; var lpNumberOfBytesRead: DWORD): BOOL; stdcall;
var filesize,ignore:dword;
begin
//ignore hprocess
  result:=false;
  filesize:=getfilesize(hprocess,@ignore);
  if ptrUint(lpbaseaddress)>filesize then exit;

  if ptrUint(lpbaseaddress)+nSize>filesize then
  begin
    ZeroMemory(lpBuffer, nsize);
    nsize:=filesize-ptrUint(lpbaseaddress);
  end;

  filecs.enter;
  SetfilePointer(hprocess,ptrUint(lpBaseAddress),nil,FILE_BEGIN);
  result:=Readfile(hprocess,lpbuffer^,nsize,lpNumberOfBytesRead,nil);
  filecs.leave;
end;

function WriteProcessMemoryFile(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesWritten: DWORD): BOOL; stdcall;
begin
  filecs.enter;
  SetfilePointer(hprocess,ptrUint(lpBaseAddress),nil,FILE_BEGIN);
  result:=Writefile(hprocess,lpbuffer^,nsize,lpNumberOfBytesWritten,nil);
  filecs.leave;
end;

function VirtualQueryExFile(hProcess: THandle; lpAddress: Pointer; var lpBuffer: TMemoryBasicInformation; dwLength: DWORD): DWORD; stdcall;
var ignore: dword;
    filesize: ptrUint;
begin
  filesize:=getfilesize(hprocess,@ignore);
  lpBuffer.BaseAddress:=pointer((ptrUint(lpAddress) div $1000)*$1000);
  lpbuffer.AllocationBase:=lpbuffer.BaseAddress;
  lpbuffer.AllocationProtect:=PAGE_EXECUTE_READWRITE;
  lpbuffer.RegionSize:=filesize-ptrUint(lpBuffer.BaseAddress);
  lpbuffer.RegionSize:=lpbuffer.RegionSize+($1000-lpbuffer.RegionSize mod $1000);


  lpbuffer.State:=mem_commit;
  lpbuffer.Protect:=PAGE_EXECUTE_READWRITE;
  lpbuffer._Type:=MEM_PRIVATE;

  if (ptrUint(lpAddress)>filesize) //bigger than the file
  then
  begin
    zeromemory(@lpbuffer,dwlength);
    result:=0
  end
  else
    result:=dwlength;

end;

initialization
  filecs:=tcriticalsection.create;

finalization
  filecs.free;


end.






