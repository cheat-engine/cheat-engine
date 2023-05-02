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
    lockedthreadid: {$ifdef windows}dword{$else}TThreadID{$endif};
    lockcount: integer;
  public
    procedure enter(maxtimeout: DWORD=INFINITE; currentThreadID: {$ifdef windows}dword=0{$else}tthreadid=nil{$endif});
    procedure leave(currentthreadid: {$ifdef windows}dword=0{$else}TThreadID=nil{$endif});
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses SyncObjs2;

resourcestring
  rsCriticalsectionLeaveWithoutEnter = 'Criticalsection leave without enter';

procedure TGuiSafeCriticalSection.enter(maxtimeout: DWORD=INFINITE; currentThreadID: {$ifdef windows}dword=0{$else}tthreadid=nil{$endif});
var deadlockprevention: integer;
begin
  if currentThreadID={$ifdef windows}0{$else}nil{$endif} then
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

    while (e.WaitFor(10) = wrTimeout) do
    begin
      CheckSynchronize;
      inc(deadlockprevention);

      if (maxtimeout<>INFINITE) and (deadlockprevention>maxtimeout) then
      begin
        raise exception.create('Pipe lock timeout. Still in use by thread '+inttostr(lockedthreadid){$ifdef THREADNAMESUPPORT} +' ('+GetThreadName(lockedthreadid)+')'{$endif});

      end;
    end;
  end
  else
    e.WaitFor(maxtimeout);


  haslock   := True;
  lockedthreadid := getcurrentthreadid;
  lockcount := 1;
end;

procedure TGuiSafeCriticalSection.leave(currentthreadid: {$ifdef windows}dword=0{$else}TThreadID=nil{$endif});
begin
  if currentThreadID={$ifdef windows}0{$else}nil{$endif} then
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

