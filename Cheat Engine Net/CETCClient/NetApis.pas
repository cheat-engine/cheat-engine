unit NetApis;

interface

uses windows,CEClient;

function ReadProcessMemory(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer;
  nSize: DWORD; var lpNumberOfBytesRead: DWORD): BOOL;

function WriteProcessMemory(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer;
  nSize: DWORD; var lpNumberOfBytesWritten: DWORD): BOOL;

function VirtualProtectEx(hProcess: THandle; lpAddress: Pointer;
  dwSize, flNewProtect: DWORD; var OldProtect: DWORD): BOOL;

function VirtualQueryEx(hProcess: THandle; lpAddress: Pointer;
  var lpBuffer: TMemoryBasicInformation; dwLength: DWORD): DWORD;

function FlushInstructionCache(hProcess: THandle; const lpBaseAddress: Pointer; dwSize: DWORD): BOOL;

function SuspendThread(hThread: THandle): DWORD; stdcall;
function ResumeThread(hThread: THandle): DWORD; stdcall;


function MakeWritable(address: dword;size: dword; x: boolean):boolean;

var darkbytekernel: thandle;



implementation

function MakeWritable(address: dword;size: dword; x: boolean):boolean;
begin
  result:=true;
end;

function FlushInstructionCache(hProcess: THandle; const lpBaseAddress: Pointer; dwSize: DWORD): BOOL;
begin
  result:=true;
end;

function VirtualQueryEx(hProcess: THandle; lpAddress: Pointer;
  var lpBuffer: TMemoryBasicInformation; dwLength: DWORD): DWORD;
begin
  result:=0;
end;

function VirtualProtectEx(hProcess: THandle; lpAddress: Pointer;
  dwSize, flNewProtect: DWORD; var OldProtect: DWORD): BOOL;
begin
  result:=VirtualProtectExNet(hProcess,lpAddress,dwSize,flNewProtect,OldProtect);
end;

function ReadProcessMemory(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer;
  nSize: DWORD; var lpNumberOfBytesRead: DWORD): BOOL;
begin
  result:=ReadProcessMemoryNet(hProcess,lpBaseAddress,lpBuffer,nSize,lpNumberOfBytesRead);
end;

function WriteProcessMemory(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer;
  nSize: DWORD; var lpNumberOfBytesWritten: DWORD): BOOL;
begin
  result:=WriteProcessMemoryNet(hProcess,lpBaseAddress,lpBuffer,nSize,lpNumberOfBytesWritten);
end;

function SuspendThread(hThread: THandle): DWORD; stdcall;
begin
  //suspend thread
end;

function ResumeThread(hThread: THandle): DWORD; stdcall;
begin
  //resume thread
end;

end.
