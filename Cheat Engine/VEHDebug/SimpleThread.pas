unit SimpleThread;
//future fpc 2.6+ has a horrible bug with TThread in a dll (sync problem I think)
//That's why this basic thread library is used instead

{$mode delphi}

interface

uses
  windows, Classes, SysUtils;


type TSimpleThread=class
  private
    threadid: dword;
    handle: thandle;
    fTerminated: boolean;

    procedure suspend;
    procedure resume;
  protected
    procedure execute; virtual; abstract;
  public
    constructor create(suspended: boolean);
    function waitfor(timeout: dword=INFINITE): boolean;
    procedure terminate;
    property terminated: boolean read fTerminated;
end;


implementation


function threadstart(self: TSimpleThread): dword; stdcall;
begin
  //OutputDebugString('threadstart called');
  try
    self.execute;
    result:=0;
  except
    OutputDebugString('A thread has crashed');
    result:=1;
  end;
end;

procedure TSimpleThread.suspend;
begin
  SuspendThread(handle);
end;

procedure TSimpleThread.resume;
begin
  ResumeThread(handle);
end;


function TSimpleThread.waitfor(timeout: DWORD=INFINITE): boolean;
begin
  if WaitForSingleObject(handle, timeout)=WAIT_TIMEOUT then
    result:=false
  else
    result:=true;
end;

procedure TSimpleThread.terminate;
begin
  fterminated:=true;
end;

constructor TSimpleThread.create(suspended: boolean);
begin
  handle:=CreateThread(nil, 0, @threadstart, self, CREATE_SUSPENDED, threadid);

  if handle<>0 then
  begin
    if not suspended then
      resume;
  end
  else
    OutputDebugString('CreateThread failure');
end;


end.

