unit DebuggerInterface;
{
This unit contains the base class description for the debugger interface.
The other debugger interfaces are inherited from this
}

{$mode delphi}

interface

uses
  Classes, SysUtils{$ifdef windows},windows{$endif},NewKernelHandler{$ifdef darwin}, macport{$endif};

type
  TDebuggerCapabilities=(dbcHardwareBreakpoint, dbcSoftwareBreakpoint, dbcBreakOnEntry);
  TDebuggerCapabilitiesSet=set of TDebuggerCapabilities;
  TDebuggerInterface=class
  protected
    fDebuggerCapabilities: TDebuggerCapabilitiesSet;
    fErrorString: string;
  public
    name: string;
    function WaitForDebugEvent(var lpDebugEvent: TDebugEvent; dwMilliseconds: DWORD): BOOL; virtual; abstract;
    function ContinueDebugEvent(dwProcessId: DWORD; dwThreadId: DWORD; dwContinueStatus: DWORD): BOOL; virtual; abstract;
    function SetThreadContext(hThread: THandle; const lpContext: TContext; isFrozenThread: Boolean=false): BOOL; virtual; abstract;
    function GetThreadContext(hThread: THandle; var lpContext: TContext; isFrozenThread: Boolean=false): BOOL; virtual; abstract;
    function DebugActiveProcess(dwProcessId: DWORD): BOOL; virtual; abstract;
    function DebugActiveProcessStop(dwProcessID: DWORD): BOOL; virtual;

    property DebuggerCapabilities: TDebuggerCapabilitiesSet read fDebuggerCapabilities;
    property errorstring: string read ferrorstring;

end;

implementation

function TDebuggerInterface.DebugActiveProcessStop(dwProcessID: DWORD): BOOL;
begin
  //don't complain if not implemented
  result:=true;
end;

end.

