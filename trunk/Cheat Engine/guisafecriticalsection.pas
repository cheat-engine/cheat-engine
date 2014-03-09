unit guisafecriticalsection;

{
This critical section implementation allows synchronize events to be fired from
inside a thread while it has a lock

Of course, do keep on the lookout that watching that the synchronize routines
don't acquire a lock themselves.
}

{$mode DELPHI}

interface

uses
  Classes, SysUtils, syncobjs;

type
  TGuiSafeCriticalSection = class
  private
    e: Tevent;
    haslock: boolean;
    lockedthreadid: dword;
    lockcount: integer;
  public
    procedure enter(maxtimeout: DWORD=INFINITE; currentThreadId: dword=0);
    procedure leave(currentThreadId: dword=0);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

resourcestring
  rsCriticalsectionLeaveWithoutEnter = 'Criticalsection leave without enter';

procedure TGuiSafeCriticalSection.enter(maxtimeout: DWORD=INFINITE; currentThreadID: dword=0);
var deadlockprevention: integer;
begin
  if currentThreadID=0 then
    currentThreadID:=GetCurrentThreadId;

  if haslock and (currentThreadID = lockedthreadid) then
  begin
    Inc(lockcount);
    exit; //same thread called it
  end;

  deadlockprevention:=0;


  if getcurrentthreadid = MainThreadID then
  begin
   // if maxtimeout=INFINITE then maxtimeout:=10000; //10 seconds max for the main gui
    maxtimeout:=maxtimeout div 10;

    while (e.WaitFor(10) = wrTimeout) and ((maxtimeout=INFINITE) or (deadlockprevention<maxtimeout)) do
    begin
      CheckSynchronize;
      inc(deadlockprevention);
    end;
  end
  else
    e.WaitFor(maxtimeout);


  haslock   := True;
  lockedthreadid := getcurrentthreadid;
  lockcount := 1;
end;

procedure TGuiSafeCriticalSection.leave(currentthreadid: dword=0);
begin
  if currentThreadID=0 then
    currentThreadID:=GetCurrentThreadId;

  if haslock and (currentThreadID <> lockedthreadid) then
    raise Exception.Create(rsCriticalsectionLeaveWithoutEnter);

  Dec(lockcount);
  if lockcount = 0 then
  begin
    haslock := False;
    lockedthreadid := 0;
    e.setEvent;
  end;
end;

constructor TGuiSafeCriticalSection.Create;
begin
  e := Tevent.Create(nil, False, True, '');
end;

destructor TGuiSafeCriticalSection.Destroy;
begin
  e.setevent;
  FreeAndNil(e);
  inherited Destroy;
end;


end.

