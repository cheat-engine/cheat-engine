unit debugger;  //stub

interface

uses windows,ceclient;

type TDebugger=class
  private

  public
    threadlist: array of array [0..4] of dword;
    procedure suspend;
    procedure Resume;
end;

function StartDebuggerIfNeeded:boolean;
procedure DetachIfPossible;

var debuggerthread: TDebugger;

implementation

procedure DetachIfPossible;
begin
  //tell the server to try to detach from the process
end;

function StartDebuggerIfNeeded:boolean;
var output,state: byte;
begin
{
  if debuggerthread=nil then
  begin
    if formsettings.rbDebugregs.enabled then

  end;
  }

  result:=false;
  //tell the server to start the debugger if needed
  output:=CS_ENABLEDEBUGGER;
  connectform.IdTCPClient1.writebuffer(output,1);

  Debuggerstatusevent.WaitFor(infinite);
  Debuggerstatusevent.ResetEvent;

  state:=0;
  connectform.IdTCPClient1.ReadBuffer(state,1);
  result:=state=1;

  DebuggerstatuseventDone.SetEvent;
end;

procedure TDebugger.suspend;
begin
  //tell the server to suspend the debuggerthread

end;

procedure TDebugger.Resume;
begin
  //tell the server to resume the debuggerthread

end;


end.
