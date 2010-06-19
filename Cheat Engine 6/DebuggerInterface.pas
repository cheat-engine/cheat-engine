unit DebuggerInterface;
{
This unit contains the base class description for the debugger interface.
The other debugger interfaces are inherited from this
}

{$mode delphi}

interface

uses
  Classes, SysUtils,windows,newkernelhandler;

type TDebuggerInterface=class
  private
  public
    function WaitForDebugEvent(var lpDebugEvent: TDebugEvent; dwMilliseconds: DWORD): BOOL; virtual; abstract;
    function ContinueDebugEvent(dwProcessId: DWORD; dwThreadId: DWORD; dwContinueStatus: DWORD): BOOL; virtual; abstract;
    function SetThreadContext(hThread: THandle; const lpContext: TContext; isFrozenThread: Boolean=false): BOOL; virtual; abstract;
    function GetThreadContext(hThread: THandle; var lpContext: TContext; isFrozenThread: Boolean=false): BOOL; virtual; abstract;
    function DebugActiveProcess(dwProcessId: DWORD): WINBOOL; virtual; abstract;
end;

implementation

end.

