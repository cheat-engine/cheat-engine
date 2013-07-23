unit NetworkDebuggerInterface;

{$mode delphi}

interface

uses
  jwawindows, windows, Classes, SysUtils,cefuncproc, newkernelhandler,
  DebuggerInterface, networkInterface, networkInterfaceApi, contnrs;

type
  TNetworkDebuggerInterface=class(TDebuggerInterface)
  private
    handle: THandle;
    lastevent: TNetworkDebugEvent;

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

uses debuggertypedefinitions;

function TNetworkDebuggerInterface.WaitForDebugEvent(var lpDebugEvent: TDebugEvent; dwMilliseconds: DWORD): BOOL;
var
  c: TCEConnection;
begin

  result:=false;
  c:=getConnection;
  if c<>nil then
  begin
    lastevent.signal:=5;

    result:=c.WaitForDebugEvent(handle, dwMilliseconds*5, lastevent);

    if result then
    begin
      //convert it to 'something' useful
      lpDebugEvent.dwThreadId:=lastevent.threadid;
      lpDebugEvent.dwProcessId:=processid;

      case lastevent.signal of
        -1 : //create thread
        begin
          lpDebugEvent.dwDebugEventCode:=CREATE_THREAD_DEBUG_EVENT;
          lpDebugEvent.CreateThread.hThread:=lastevent.threadid;
        end;

        -2 : //create process
        begin
          lpDebugEvent.dwDebugEventCode:=CREATE_PROCESS_DEBUG_EVENT;
          lpDebugEvent.CreateProcessInfo.hProcess:=handle;
          lpDebugEvent.CreateProcessInfo.hThread:=lastevent.threadid;

        end;

        5: //SIGTRAP
        begin
          lpDebugEvent.dwDebugEventCode:=EXCEPTION_DEBUG_EVENT;
          lpDebugEvent.Exception.dwFirstChance:=1;
          lpDebugEvent.Exception.ExceptionRecord.NumberParameters:=0;

          lpDebugEvent.Exception.ExceptionRecord.ExceptionCode:=EXCEPTION_SINGLE_STEP;

        end;

        19: //sigstop
        begin
          //just ignore. continue and return that no stop happened (timeout)
          ContinueDebugEvent(handle, lastevent.threadid, DBG_CONTINUE);
          result:=false;
        end;



        else //e.g: SIGSTOP
        begin

          ContinueDebugEvent(handle, lastevent.threadid, DBG_EXCEPTION_NOT_HANDLED);
          result:=false;
        end;
      end;
    end;
  end;

end;

function TNetworkDebuggerInterface.ContinueDebugEvent(dwProcessId: DWORD; dwThreadId: DWORD; dwContinueStatus: DWORD): BOOL;
var
  c: TCEConnection;
begin

  result:=false;
  c:=getConnection;
  if c<>nil then
  begin
    if dwContinueStatus=DBG_CONTINUE then
      result:=c.ContinueDebugEvent(handle, dwThreadID, 0)
    else
      result:=c.ContinueDebugEvent(handle, dwThreadID, 1);
  end;

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
var c: TCEConnection;
begin
  result:=false;
  processhandler.processid:=dwProcessID;
  Open_Process;

  handle:=ProcessHandle;

  if (handle<>0) then
  begin
    c:=getConnection;
    if c<>nil then
      result:=c.StartDebug(handle);

  end;

end;


destructor TNetworkDebuggerInterface.destroy;
begin
  {
  if (handle)
    networkStopDebug();
    }
  inherited destroy;
end;

constructor TNetworkDebuggerInterface.create;
begin
  //no software breakpoint for now
  fDebuggerCapabilities:=[dbcHardwareBreakpoint];
end;


end.

