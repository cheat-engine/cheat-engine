unit NetworkDebuggerInterface;

{$mode delphi}

interface

uses
  jwawindows, windows, Classes, SysUtils,cefuncproc, newkernelhandler,
  DebuggerInterface, networkInterface, networkInterfaceApi;

type
  TNetworkDebuggerInterface=class(TDebuggerInterface)
  private


  public
    function WaitForDebugEvent(var lpDebugEvent: TDebugEvent; dwMilliseconds: DWORD): BOOL; override;
    function ContinueDebugEvent(dwProcessId: DWORD; dwThreadId: DWORD; dwContinueStatus: DWORD): BOOL; override;
    function SetThreadContext(hThread: THandle; const lpContext: TContext; isFrozenThread: Boolean=false): BOOL; override;
    function GetThreadContext(hThread: THandle; var lpContext: TContext; isFrozenThread: Boolean=false):  BOOL; override;

    function GetLastBranchRecords(lbr: pointer): integer; override;

    function DebugActiveProcess(dwProcessId: DWORD): WINBOOL; override;

    destructor destroy; override;
    constructor create;
  end;


implementation

function TNetworkDebuggerInterface.WaitForDebugEvent(var lpDebugEvent: TDebugEvent; dwMilliseconds: DWORD): BOOL;
begin

end;

function TNetworkDebuggerInterface.ContinueDebugEvent(dwProcessId: DWORD; dwThreadId: DWORD; dwContinueStatus: DWORD): BOOL;
begin

end;

function TNetworkDebuggerInterface.SetThreadContext(hThread: THandle; const lpContext: TContext; isFrozenThread: Boolean=false): BOOL;
begin

end;

function TNetworkDebuggerInterface.GetThreadContext(hThread: THandle; var lpContext: TContext; isFrozenThread: Boolean=false):  BOOL;
begin

end;


function TNetworkDebuggerInterface.GetLastBranchRecords(lbr: pointer): integer;
begin

end;

function TNetworkDebuggerInterface.DebugActiveProcess(dwProcessId: DWORD): WINBOOL;
begin

end;


destructor TNetworkDebuggerInterface.destroy;
begin
  inherited destroy;
end;

constructor TNetworkDebuggerInterface.create;
begin

end;


end.

