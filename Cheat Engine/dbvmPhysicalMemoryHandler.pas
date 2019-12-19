unit dbvmPhysicalMemoryHandler;

{$MODE Delphi}

{
implement replaced handlers for ReadProcssMemory and WriteProcessMemory so it
reads/writes to the physical memory instead
}

interface

{$IFDEF windows}
uses windows, LCLIntf, vmxfunctions;

function ReadProcessMemoryPhys(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer;  nSize: DWORD; var lpNumberOfBytesRead: DWORD): BOOL; stdcall;
function WriteProcessMemoryPhys(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesWritten: DWORD): BOOL; stdcall;
function VirtualQueryExPhys(hProcess: THandle; lpAddress: Pointer; var lpBuffer: TMemoryBasicInformation; dwLength: DWORD): DWORD; stdcall;
{$ENDIF}


implementation

{$IFDEF windows}
uses NewKernelHandler, DBK32functions;

function ReadProcessMemoryPhys(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer;  nSize: DWORD; var lpNumberOfBytesRead: DWORD): BOOL; stdcall;
begin
  lpNumberOfBytesRead:=dbvm_read_physical_memory(uint64(ptrUint(lpBaseAddress)),lpBuffer,nSize);
  result:=lpNumberOfBytesRead>0;
end;

function WriteProcessMemoryPhys(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesWritten: DWORD): BOOL; stdcall;
begin
  lpNumberOfBytesWritten:=dbvm_write_physical_memory(uint64(ptrUint(lpBaseAddress)),lpBuffer,nSize);
  result:=lpNumberOfBytesWritten>0;
end;

function VirtualQueryExPhys(hProcess: THandle; lpAddress: Pointer; var lpBuffer: TMemoryBasicInformation; dwLength: DWORD): DWORD; stdcall;
var filesize: uint64;
begin
  if DBK32functions.hdevice<>INVALID_HANDLE_VALUE then
  begin
    //prefer the driver over this guess
    result:=DBK32functions.VirtualQueryExPhysical(hProcess, lpAddress, lpBuffer, dwLength);
  end
  else
  begin
    filesize:=qword($200000000);
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

end;
{$ENDIF}



end.
 
