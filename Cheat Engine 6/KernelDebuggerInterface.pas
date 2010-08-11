unit KernelDebuggerInterface;

{$mode delphi}

interface

uses
  windows, Classes, SysUtils,cefuncproc, newkernelhandler,DebuggerInterface;

type
  TKernelDebugInterface=class(TDebuggerInterface)
  private
    pid: DWORD;
    currentdebuggerstate: TDebuggerstate;
  public
    function WaitForDebugEvent(var lpDebugEvent: TDebugEvent; dwMilliseconds: DWORD): BOOL; override;
   // function ContinueDebugEvent(dwProcessId: DWORD; dwThreadId: DWORD; dwContinueStatus: DWORD): BOOL; override;
   // function SetThreadContext(hThread: THandle; const lpContext: TContext; isFrozenThread: Boolean=false): BOOL; override;
   // function GetThreadContext(hThread: THandle; var lpContext: TContext; isFrozenThread: Boolean=false):  BOOL; override;

    function DebugActiveProcess(dwProcessId: DWORD): WINBOOL; override;
  end;

implementation

function TKernelDebugInterface.DebugActiveProcess(dwProcessId: DWORD): WINBOOL;
{Start the kerneldebugger for the current process}
begin
  loaddbk32;
  if not loaddbvmifneeded then
    raise exception.Create('You can''t currently use the kernel debugger');

  result:=DBKDebug_StartDebugging(dwProcessId);
  pid:=dwProcessID;
end;

function TKernelDebugInterface.WaitForDebugEvent(var lpDebugEvent: TDebugEvent; dwMilliseconds: DWORD): BOOL;
begin
  result:=DBKDebug_WaitForDebugEvent(dwMilliseconds);
  if result then
  begin
    //get the state and convert to lpDebugEvent
    DBKDebug_GetDebuggerState(@currentdebuggerstate);


    lpDebugEvent.dwProcessId:=pid;
    lpDebugEvent.dwThreadId:=currentdebuggerstate.threadid;
    lpDebugEvent.Exception.dwFirstChance:=1;
  end;
end;

end.

