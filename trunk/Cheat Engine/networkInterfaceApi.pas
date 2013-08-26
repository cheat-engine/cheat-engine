unit networkInterfaceApi;

{$mode objfpc}{$H+}

interface

uses
  {jwawindows,} windows, Classes, SysUtils, networkinterface, newkernelhandler;


procedure InitializeNetworkInterface;
function getConnection: TCEConnection;
procedure disconnect;

function NetworkVersion(var name: string): integer;

function NetworkReadProcessMemory(hProcess: THandle; lpBaseAddress, lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesRead: DWORD): BOOL; stdcall;
function NetworkWriteProcessMemory(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesWritten: DWORD): BOOL; stdcall;

function NetworkVirtualQueryEx(hProcess: THandle; lpAddress: Pointer; var lpBuffer: TMemoryBasicInformation; dwLength: DWORD): DWORD; stdcall;
function NetworkOpenProcess(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; dwProcessId:DWORD):HANDLE; stdcall;
function NetworkCreateToolhelp32Snapshot(dwFlags, th32ProcessID: DWORD): HANDLE; stdcall;
function NetworkProcess32First(hSnapshot: HANDLE; var lppe: PROCESSENTRY32): BOOL; stdcall;
function NetworkProcess32Next(hSnapshot: HANDLE; var lppe: PROCESSENTRY32): BOOL; stdcall;

function NetworkModule32First(hSnapshot: HANDLE; var lpme: MODULEENTRY32): BOOL; stdcall;
function NetworkModule32Next(hSnapshot: HANDLE; var lpme: MODULEENTRY32): BOOL; stdcall;

function NetworkCloseHandle(handle: THandle):WINBOOL; stdcall;
function NetworkSetBreakpoint(handle: THandle; threadid: integer; debugregister: integer; address: PtrUInt; bptype: integer; bpsize: integer): boolean;
function NetworkRemoveBreakpoint(handle: THandle; threadid: integer; debugregister: integer; wasWatchpoint: boolean): boolean;


implementation

uses networkConfig;


threadvar connection: TCEConnection;


var threadManagerIsHooked: boolean=false;
    oldendthread: TEndThreadHandler;

function getConnection: TCEConnection;
begin
  result:=nil;
  if networkconfig.host.s_addr<>0 then
  begin

    if (connection=nil) or (not connection.connected) then
    begin
      disconnect;

      connection:=TCEConnection.create;
      if connection.connected then
        result:=connection;

    end
    else
      result:=connection;

  end;
end;

procedure disconnect;
begin
  if connection<>nil then
    freeandnil(connection);
end;

function NetworkCloseHandle(handle: THandle):WINBOOL; stdcall;
begin
  if getConnection<>nil then
    result:=connection.CloseHandle(handle)
  else
    result:=false;
end;

function NetworkProcess32Next(hSnapshot: HANDLE; var lppe: PROCESSENTRY32): BOOL; stdcall;
begin
  if getConnection<>nil then
    result:=connection.Process32Next(hSnapshot, lppe)
  else
    result:=FALSE;
end;

function NetworkProcess32First(hSnapshot: HANDLE; var lppe: PROCESSENTRY32): BOOL; stdcall;
begin
  if getConnection<>nil then
    result:=connection.Process32First(hSnapshot, lppe)
  else
    result:=FALSE;
end;

function NetworkModule32Next(hSnapshot: HANDLE; var lpme: MODULEENTRY32): BOOL; stdcall;
begin
  if getConnection<>nil then
    result:=connection.Module32Next(hSnapshot, lpme)
  else
    result:=FALSE;
end;

function NetworkModule32First(hSnapshot: HANDLE; var lpme: MODULEENTRY32): BOOL; stdcall;
begin
  if getConnection<>nil then
    result:=connection.Module32First(hSnapshot, lpme)
  else
    result:=FALSE;
end;

function NetworkCreateToolhelp32Snapshot(dwFlags, th32ProcessID: DWORD): HANDLE; stdcall;
begin
  if getConnection<>nil then
    result:=connection.CreateToolhelp32Snapshot(dwflags, th32ProcessId)
  else
    result:=INVALID_HANDLE_VALUE;
end;

function NetworkSetBreakpoint(handle: THandle; threadid: integer; debugregister: integer; address: PtrUInt; bptype: integer; bpsize: integer): boolean;
begin
  if getConnection<>nil then
    result:=connection.SetBreakpoint(handle, threadid, debugregister, address, bptype, bpsize)
  else
    result:=FALSE;
end;

function NetworkRemoveBreakpoint(handle: THandle; threadid: integer; debugregister: integer; wasWatchpoint: boolean): boolean;
begin
  if getConnection<>nil then
    result:=connection.RemoveBreakpoint(handle, threadid, debugregister, wasWatchpoint)
  else
    result:=FALSE;
end;

function NetworkReadProcessMemory(hProcess: THandle; lpBaseAddress, lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesRead: DWORD): BOOL; stdcall;
var a,b,c,d: dword;
begin
  if getConnection<>nil then
  begin
    result:=connection.readProcessMemory(hProcess, lpBaseAddress, lpBuffer, nSize, lpNumberOfBytesRead);
    if (result=false) and (connection.connected=false) and (getConnection<>nil) then //try again one more time
    begin
      //try a smaller chunk
      a:=nsize div 2;
      b:=nsize-a;
      c:=0;
      d:=0;
      result:=connection.readProcessMemory(hProcess, lpBaseAddress, lpBuffer, a, c);
      if result and (b>0) then
        result:=connection.readProcessMemory(hProcess, pointer(ptruint(lpBaseAddress)+a), pointer(ptruint(lpBuffer)+a), b, d);

      lpNumberOfBytesRead:=c+d;
    end;


  end
  else
    result:=false;
end;

function NetworkWriteProcessMemory(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesWritten: DWORD): BOOL; stdcall;
begin
  if getConnection<>nil then
    result:=connection.writeProcessMemory(hProcess, lpBaseAddress, lpBuffer, nSize, lpNumberOfBytesWritten)
  else
    result:=false;
end;


function NetworkVirtualQueryEx(hProcess: THandle; lpAddress: Pointer; var lpBuffer: TMemoryBasicInformation; dwLength: DWORD): DWORD; stdcall;
begin
  if getConnection<>nil then
    result:=connection.VirtualQueryEx(hProcess, lpAddress, lpBuffer, dwLength)
  else
    result:=0;
end;

function NetworkVirtualAllocEx(hProcess: THandle; lpAddress: Pointer; dwSize, flAllocationType: DWORD; flProtect: DWORD): Pointer; stdcall;
begin
  if getconnection<>nil then
    result:=connection.VirtualAllocEx(hProcess, lpAddress, dwSize, flAllocationType, flProtect)
  else
    result:=nil;
end;

function NetworkOpenProcess(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; dwProcessId:DWORD):HANDLE; stdcall;
begin
  if getConnection<>nil then
    result:=connection.OpenProcess(dwDesiredAccess, bInheritHandle, dwProcessId)
  else
    result:=0;
end;

function NetworkVersion(var name: string): integer;
begin
  if getConnection<>nil then
    result:=connection.getVersion(name)
  else
  begin
    result:=-1;
    name:='No connection';
  end;
end;

Procedure EndThread(ExitCode : DWord); //called when a thread is terminated
begin
  if connection<>nil then
    freeandnil(connection);

  oldendthread(ExitCode);
end;

procedure InitializeNetworkInterface;
var tm: TThreadManager;
begin
  //hook the threadmanager if it hasn't been hooked yet
  if not threadManagerIsHooked then
  begin
    GetThreadManager(tm);

    oldendthread:=tm.EndThread;
    tm.EndThread:=@EndThread;

    SetThreadManager(tm);

    threadManagerIsHooked:=true;
  end;

  newkernelhandler.OpenProcess:=@NetworkOpenProcess;
  newkernelhandler.ReadProcessMemory:=@NetworkReadProcessMemory;
  newkernelhandler.WriteProcessMemory:=@NetworkWriteProcessMemory;
  newkernelhandler.VirtualQueryEx:=@NetworkVirtualQueryEx;
  newkernelhandler.CreateToolhelp32Snapshot:=@NetworkCreateToolhelp32Snapshot;
  newkernelhandler.Process32First:=@NetworkProcess32First;
  newkernelhandler.Process32Next:=@NetworkProcess32Next;
  newkernelhandler.Module32First:=@NetworkModule32First;
  newkernelhandler.Module32Next:=@NetworkModule32Next;
  newkernelhandler.closehandle:=@networkclosehandle;

  newkernelhandler.VirtualAllocEx:=@networkVirtualAllocEx;


end;

end.

