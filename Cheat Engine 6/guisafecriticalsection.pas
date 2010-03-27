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
    procedure enter;
    procedure leave;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

procedure TGuiSafeCriticalSection.enter;
begin
  if haslock and (getcurrentthreadid = lockedthreadid) then
  begin
    Inc(lockcount);
    exit; //same thread called it
  end;

  if getcurrentthreadid = MainThreadID then
  begin
    while e.WaitFor(10) = wrTimeout do
      CheckSynchronize;
  end
  else
    e.WaitFor(infinite);

  haslock   := True;
  lockedthreadid := getcurrentthreadid;
  lockcount := 1;
end;

procedure TGuiSafeCriticalSection.leave;
begin
  if haslock and (getcurrentthreadid <> lockedthreadid) then
    raise Exception.Create('Criticalsection leave without enter');

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

