unit WindowsDebugger;
{
Debugger interface for the default windows api.
It's basically just a forward for everything
}

{$mode delphi}

interface

{$ifdef windows}

uses
  Classes, SysUtils, DebuggerInterface, windows, cefuncproc,newkernelhandler,
  symbolhandler, dialogs;

type
  TWindowsDebuggerInterface=class(TDebuggerInterface)
  public

    function WaitForDebugEvent(var lpDebugEvent: TDebugEvent; dwMilliseconds: DWORD): BOOL; override;
    function ContinueDebugEvent(dwProcessId: DWORD; dwThreadId: DWORD; dwContinueStatus: DWORD): BOOL; override;
    function SetThreadContext(hThread: THandle; const lpContext: TContext; isFrozenThread: Boolean=false): BOOL; override;
    function GetThreadContext(hThread: THandle; var lpContext: TContext; isFrozenThread: Boolean=false): BOOL; override;
    function DebugActiveProcess(dwProcessId: DWORD): WINBOOL; override;
    function DebugActiveProcessStop(dwProcessID: DWORD): BOOL; override;
    function canUseIPT: boolean; override;
    constructor create;
end;

{$endif}

implementation

{$ifdef windows}
uses autoassembler, pluginexports, CEDebugger, DebugHelper, processhandlerunit;

resourcestring
  rsErrorAttachingTheWindowsDebugger = 'Error attaching the windows debugger: '
    +'%s';

constructor TWindowsDebuggerInterface.create;
begin
  inherited create;
  fDebuggerCapabilities:=fDebuggerCapabilities+[dbcSoftwareBreakpoint, dbcHardwareBreakpoint, dbcExceptionBreakpoint, dbcBreakOnEntry];
  name:='Windows Debugger';

  fmaxSharedBreakpointCount:=4;
end;

function TWindowsDebuggerInterface.WaitForDebugEvent(var lpDebugEvent: TDebugEvent; dwMilliseconds: DWORD): BOOL;
begin
  result:=newkernelhandler.WaitForDebugEvent(lpDebugEvent, dwMilliseconds);
end;

function TWindowsDebuggerInterface.ContinueDebugEvent(dwProcessId: DWORD; dwThreadId: DWORD; dwContinueStatus: DWORD): BOOL;
begin
  result:=newkernelhandler.ContinueDebugEvent(dwProcessId, dwThreadId, dwContinueStatus);
end;

function TWindowsDebuggerInterface.SetThreadContext(hThread: THandle; const lpContext: TContext; isFrozenThread: Boolean=false): BOOL;
begin
  result:=newkernelhandler.SetThreadContext(hThread, lpContext);
end;

function TWindowsDebuggerInterface.GetThreadContext(hThread: THandle; var lpContext: TContext; isFrozenThread: Boolean=false):BOOL;
begin
  result:=newkernelhandler.GetThreadContext(hThread, lpContext);
end;

function TWindowsDebuggerInterface.canUseIPT: boolean;
begin
  result:=true;
end;

function TWindowsDebuggerInterface.DebugActiveProcessStop(dwProcessID: DWORD): BOOL;
begin
  if assigned(CEDebugger.DebugActiveProcessStop) then
    result:=CEDebugger.DebugActiveProcessStop(dwProcessID)
  else
    result:=false;
end;

function TWindowsDebuggerInterface.DebugActiveProcess(dwProcessId: DWORD): WINBOOL;
var d: tstringlist;
begin
 // OutputDebugString('Windows Debug Active Process');
  if processhandler.processid<>dwProcessId then
  begin
    processhandler.processid:=dwProcessID;
    Open_Process;

    symhandler.reinitialize;
    symhandler.waitforsymbolsloaded(true);
  end;

  if PreventDebuggerDetection then
  begin
    d:=tstringlist.create;
    try
      d.Add('IsDebuggerPresent:');
      d.add('xor eax,eax');
      d.add('ret');
      try
        autoassemble(d,false);
      except
      end;

    finally
      d.free;
    end;
  end;

  result:=newkernelhandler.DebugActiveProcess(dwProcessId);

  if result=false then
    ferrorstring:=Format(rsErrorAttachingTheWindowsDebugger, [inttostr(
      getlasterror)])
  else
    symhandler.reinitialize;
  //processhandler.processid:=dwProcessID;
  //Open_Process;

end;

{$endif}

end.

