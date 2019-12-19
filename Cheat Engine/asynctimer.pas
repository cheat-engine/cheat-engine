unit AsyncTimer;
{
Timer like thread that will call a specific notifyevent routiner every specified interval
}

{$mode delphi}

interface

uses
  Classes, SysUtils, syncobjs;

type
  TAsyncTimer=class(tthread)
  private
    fInterval: integer;
    fOnTimer: TNotifyEvent;
    fOnTimerCS: TCriticalSection;
    fEnabled: boolean;
    eenabled: TEvent;
    timerevent: TEvent;

    triggeronce: boolean;
    procedure setOnTimer(f: TNotifyEvent);
    procedure setEnabled(state: boolean);
    procedure setInterval(interval: integer);
  public
    procedure Terminate;
    procedure execute; override;
    procedure TriggerNow; //triggers the event
    constructor create(suspended: boolean);
    destructor destroy; override;

    property OnTimer: TnotifyEvent read fOnTimer write setOnTimer;
    property Interval: integer read fInterval write setInterval;
    property Enabled: boolean read fEnabled write setEnabled;
  end;

implementation

procedure TAsyncTimer.setEnabled(state: boolean);
begin
  if state then
    eenabled.SetEvent
  else
    eenabled.ResetEvent;

  fEnabled:=state;
end;

procedure TAsyncTimer.setInterval(interval: integer);
begin
  finterval:=interval;
  timerevent.SetEvent; //if the timer was sleeping
end;

procedure TAsyncTimer.setOnTimer(f: TNotifyEvent);
begin
  fOnTimerCS.Enter;
  fOnTimer:=f;
  fOnTimerCS.Leave;
end;

procedure TAsyncTimer.Terminate;
begin
  setOnTimer(nil);
  inherited Terminate;

  timerevent.SetEvent;
  Enabled:=true;
end;

procedure TAsyncTimer.TriggerNow; //triggers the event as soon as possible
begin
  triggeronce:=true;
  timerevent.SetEvent;
end;

procedure TAsyncTimer.execute;
var f: TNotifyEvent;
begin
  while not terminated do
  begin
    while (not terminated) and (eenabled.WaitFor(INFINITE)=wrTimeout) do ;

    while (not terminated) and (timerevent.WaitFor(fInterval)=wrSignaled) and (not triggeronce) do //each time the interval changes this will get signaled. When that happens, wait again
      timerevent.ResetEvent;

    triggeronce:=false;

    if not terminated then
    begin
      try
        fOnTimerCS.enter;
        try
          f:=fOnTimer;
        finally
          fOnTimerCS.leave;
        end;


        if enabled and assigned(f) then
          f(self);
      except
        //on unexpected exceptions don't destroy the timer
      end;
    end;


  end;
end;

constructor TAsyncTimer.create(suspended: boolean);
begin
  timerevent:=TEvent.create(nil, true, false,'');
  eenabled:=TEvent.Create(nil, true, false,'');
  fOnTimercs:=TCriticalSection.create;

  fEnabled:=false;
  finterval:=1000;

  inherited create(suspended);
end;

destructor TAsyncTimer.destroy;
begin
  terminate;
  self.WaitFor;

  if fOnTimerCS<>nil then
    fOnTimerCS.free;

  inherited destroy;
end;

end.

