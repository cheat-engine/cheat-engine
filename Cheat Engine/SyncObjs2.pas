unit SyncObjs2;

{$MODE Delphi}

interface

uses {$ifdef darwin}
  macport, SyncObjs,
  {$else}
  windows,
  {$endif}classes, sysutils, LCLIntf;

type TSemaphore=class
  private
    {$ifdef windows}
    h: THandle;
    {$endif}

    max: integer;
    {$ifdef darwin}
    h: pointer;
    left: integer;

    cs: TCriticalSection;
    {$endif}

  public
    function TryAcquire(time: integer=0): boolean;
    procedure Acquire;
    function Release(count:integer=1):integer;
    constructor create(maxcount: integer; init0:boolean=false);
    destructor destroy;  override;
end;

implementation


constructor TSemaphore.create(maxcount:integer; init0: boolean=false);
var
  init: integer;
  i: integer;
begin
  max:=maxcount;
  if init0 then
    init:=0
  else
    init:=maxcount;

  {$ifdef windows}
  h:=CreateSemaphore(nil,init,maxcount,nil);
  {$else}
  cs:=tcriticalsection.create;
  h:=SemaphoreInit;

  left:=init;

  for i:=1 to left do
    SemaphorePost(h);
  {$endif}
end;

destructor TSemaphore.destroy;
begin
  {$ifdef windows}
  closehandle(h);
  {$else}
  SemaphoreDestroy(h);
  cs.destroy;
  {$endif}

  inherited destroy;
end;

procedure TSemaphore.Acquire;
begin
  {$ifdef windows}
  waitforsingleobject(h,infinite);
  {$else}
  cs.enter;
  SemaphoreWait(h);    //wait inside a critical section
  dec(left);
  cs.leave;
  {$endif}
end;

function TSemaphore.TryAcquire(time: integer=0):boolean;
{$ifndef windows}
var
  t: TThread;
  starttime: qword;
{$endif}
begin
  {$ifdef windows}
  result:=waitforsingleobject(h,time)=WAIT_OBJECT_0;
  {$else}
  starttime:=gettickcount64;
  result:=false;


  repeat
    if cs.TryEnter then
    begin
      if left>0 then
      begin
        result:=true;
        SemaphoreWait(h); //wait inside a critical section
        dec(left);
      end;

      cs.Leave;
    end
    else
      result:=false;

    if (not result) and (time>0) then
      sleep(10);  //active'ish' wait


  until result or (gettickcount64>(starttime+time));
  {$endif}
end;

function TSemaphore.Release(count: integer=1): integer;
var
  previouscount: LONG;
  e: integer;
begin
  {$ifdef windows}
  if releasesemaphore(h,count,@previouscount) then
    result:=previouscount
  else
    result:=-1;
  {$else}
  result:=left;
  for e:=1 to count do
  begin
    if left<max then
    begin
      SemaphorePost(h);  //lets the cs in acquire get released

      cs.enter;
      inc(left);
      cs.leave;
    end;
  end;
  {$endif}
end;

end.








