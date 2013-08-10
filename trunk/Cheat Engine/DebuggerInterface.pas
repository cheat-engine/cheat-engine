unit DebuggerInterface;
{
This unit contains the base class description for the debugger interface.
The other debugger interfaces are inherited from this
}

{$mode delphi}

interface

uses
  Classes, SysUtils{$ifdef windows},windows{$endif},NewKernelHandler, debuggertypedefinitions{$ifdef darwin}, macport{$endif};

type
  TDebuggerCapabilities=(dbcHardwareBreakpoint, dbcSoftwareBreakpoint, dbcExceptionBreakpoint, dbcBreakOnEntry);
  TDebuggerCapabilitiesSet=set of TDebuggerCapabilities;
  TDebuggerInterface=class
  protected
    fDebuggerCapabilities: TDebuggerCapabilitiesSet;
    fErrorString: string;
  public
    name: string;
    function WaitForDebugEvent(var lpDebugEvent: TDebugEvent; dwMilliseconds: DWORD): BOOL; virtual; abstract;
    function ContinueDebugEvent(dwProcessId: DWORD; dwThreadId: DWORD; dwContinueStatus: DWORD): BOOL; virtual; abstract;
    function SetThreadContext(hThread: THandle; const lpContext: TContext; isFrozenThread: Boolean=false): BOOL; virtual;
    function SetThreadContextArm(hThread: THandle; const lpContext: TARMCONTEXT; isFrozenThread: Boolean=false): BOOL; virtual;
    function GetThreadContext(hThread: THandle; var lpContext: TContext; isFrozenThread: Boolean=false): BOOL; virtual;
    function GetThreadContextArm(hThread: THandle; var lpContext: TARMCONTEXT; isFrozenThread: Boolean=false): BOOL; virtual;
    function DebugActiveProcess(dwProcessId: DWORD): BOOL; virtual; abstract;
    function DebugActiveProcessStop(dwProcessID: DWORD): BOOL; virtual;
    function GetLastBranchRecords(lbr: pointer): integer; virtual;

    property DebuggerCapabilities: TDebuggerCapabilitiesSet read fDebuggerCapabilities;
    property errorstring: string read ferrorstring;
end;

implementation

function TDebuggerInterface. SetThreadContext(hThread: THandle; const lpContext: TContext; isFrozenThread: Boolean=false): BOOL;
begin
  result:=false;
end;

function TDebuggerInterface.SetThreadContextArm(hThread: THandle; const lpContext: TARMCONTEXT; isFrozenThread: Boolean=false): BOOL;
begin
  result:=false;
end;

function TDebuggerInterface.GetThreadContextArm(hThread: THandle; var lpContext: TARMCONTEXT; isFrozenThread: Boolean=false): BOOL;
begin
  result:=false;
end;

function TDebuggerInterface.GetThreadContext(hThread: THandle; var lpContext: TContext; isFrozenThread: Boolean=false): BOOL;
begin
  result:=false;
end;



function TDebuggerInterface.DebugActiveProcessStop(dwProcessID: DWORD): BOOL;
begin
  //don't complain if not implemented
  result:=true;
end;

function TDebuggerInterface.GetLastBranchRecords(lbr: pointer): integer;
begin
  //if implemented fill in the lbr pointer with the lbr records (array of qwords) and return the count (max 16)
  result:=-1;
end;

end.

