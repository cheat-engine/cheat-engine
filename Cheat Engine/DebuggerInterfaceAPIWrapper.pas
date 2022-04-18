unit DebuggerInterfaceAPIWrapper;
{
This unit hold the DebuggerInterface currently used, and overrides the default windows debug api's so they make use of the DebuggerInterface's version
}

{$mode delphi}

interface

uses
  Classes, SysUtils, {$ifdef windows}windows,{$endif} debuggerinterface, newkernelhandler{$ifdef darwin}, macport{$endif};

function WaitForDebugEvent(var lpDebugEvent: TDebugEvent; dwMilliseconds: DWORD): BOOL;
function ContinueDebugEvent(dwProcessId: DWORD; dwThreadId: DWORD; dwContinueStatus: DWORD): BOOL;
function SetThreadContext(hThread: THandle; const lpContext: TContext; isFrozenThread: Boolean=false): BOOL; overload;
function SetThreadContext(hThread: THandle; const lpContext: TARMCONTEXT; isFrozenThread: Boolean=false): BOOL; overload;
function SetThreadContext(hThread: THandle; const lpContext: TARM64CONTEXT; isFrozenThread: Boolean=false): BOOL; overload;
function GetThreadContext(hThread: THandle; var lpContext: TContext; isFrozenThread: Boolean=false): BOOL; overload;
function GetThreadContext(hThread: THandle; var lpContext: TARMCONTEXT; isFrozenThread: Boolean=false): BOOL; overload;
function GetThreadContext(hThread: THandle; var lpContext: TARM64CONTEXT; isFrozenThread: Boolean=false): BOOL; overload;
function GetThreadContextArm(hThread: THandle; var lpContext: TARMCONTEXT; isFrozenThread: Boolean=false): BOOL;
function GetThreadContextArm64(hThread: THandle; var lpContext: TARM64CONTEXT; isFrozenThread: Boolean=false): BOOL;

function DebugActiveProcess(dwProcessId: DWORD): WINBOOL;
function DebugActiveProcessStop(dwProcessID: DWORD): WINBOOL;

var CurrentDebuggerInterface: TDebuggerInterface;

implementation

uses CEDebugger;

function WaitForDebugEvent(var lpDebugEvent: TDebugEvent; dwMilliseconds: DWORD): BOOL;
begin
  if CurrentDebuggerInterface<>nil then
    result:=CurrentDebuggerInterface.WaitForDebugEvent(lpDebugEvent, dwMilliseconds)
  else
    result:=false;
end;

function ContinueDebugEvent(dwProcessId: DWORD; dwThreadId: DWORD; dwContinueStatus: DWORD): BOOL;
begin
  if CurrentDebuggerInterface<>nil then
    result:=CurrentDebuggerInterface.ContinueDebugEvent(dwProcessID, dwThreadID, dwContinueStatus)
  else
    result:=false;
end;


function SetThreadContextArm(hThread: THandle; const lpContext: TArmContext; isFrozenThread: Boolean=false): BOOL;
begin
  if CurrentDebuggerInterface<>nil then
    result:=CurrentDebuggerInterface.SetThreadContextArm(hThread, lpContext, isFrozenThread)
  else
    result:=false;
end;

function SetThreadContextArm64(hThread: THandle; const lpContext: TArm64Context; isFrozenThread: Boolean=false): BOOL;
begin
  if CurrentDebuggerInterface<>nil then
    result:=CurrentDebuggerInterface.SetThreadContextArm64(hThread, lpContext, isFrozenThread)
  else
    result:=false;
end;

function SetThreadContext(hThread: THandle; const lpContext: TContext; isFrozenThread: Boolean=false): BOOL;
begin
  if CurrentDebuggerInterface<>nil then
    result:=CurrentDebuggerInterface.SetThreadContext(hThread, lpContext, isFrozenThread)
  else
    result:=NewKernelHandler.SetThreadContext(hThread, lpcontext);
end;

function SetThreadContext(hThread: THandle; const lpContext: TARMCONTEXT; isFrozenThread: Boolean=false): BOOL;
begin
  result:=SetThreadContextArm(hThread, lpContext, isFrozenThread);
end;

function SetThreadContext(hThread: THandle; const lpContext: TARM64CONTEXT; isFrozenThread: Boolean=false): BOOL;
begin
  result:=SetThreadContextArm64(hThread, lpContext, isFrozenThread);
end;






function GetThreadContextArm(hThread: THandle; var lpContext: TARMCONTEXT; isFrozenThread: Boolean=false): BOOL;
begin
  if CurrentDebuggerInterface<>nil then
    result:=CurrentDebuggerInterface.GetThreadContextArm(hThread, lpContext, isFrozenThread)
  else
    result:=false;
end;

function GetThreadContextArm64(hThread: THandle; var lpContext: TARM64CONTEXT; isFrozenThread: Boolean=false): BOOL;
begin
  if CurrentDebuggerInterface<>nil then
    result:=CurrentDebuggerInterface.GetThreadContextArm64(hThread, lpContext, isFrozenThread)
  else
    result:=false;
end;

function GetThreadContext(hThread: THandle; var lpContext: TContext; isFrozenThread: Boolean=false): BOOL;
begin
  if CurrentDebuggerInterface<>nil then
    result:=CurrentDebuggerInterface.GetThreadContext(hThread, lpContext, isFrozenThread)
  else
    result:=NewKernelHandler.GetThreadContext(hThread, lpContext);
end;

function GetThreadContext(hThread: THandle; var lpContext: TARMCONTEXT; isFrozenThread: Boolean=false): BOOL; overload;
begin
  result:=GetThreadContextArm(hThread, lpContext, isFrozenThread);
end;

function GetThreadContext(hThread: THandle; var lpContext: TARM64CONTEXT; isFrozenThread: Boolean=false): BOOL; overload;
begin
  result:=GetThreadContextArm64(hThread, lpContext, isFrozenThread);
end;

function DebugActiveProcess(dwProcessId: DWORD): WINBOOL;
begin
  if CurrentDebuggerInterface<>nil then
    result:=CurrentDebuggerInterface.DebugActiveProcess(dwProcessID)
  else
    result:=false;
end;

function DebugActiveProcessStop(dwProcessID: DWORD): WINBOOL;
begin
  if CurrentDebuggerInterface<>nil then
    result:=CurrentDebuggerInterface.DebugActiveProcessStop(dwProcessID)
  else
  {$ifdef windows}
    result:=cedebugger.DebugActiveProcessStop(dwProcessID);
  {$else}
    result:=false;
  {$endif}
end;


end.

