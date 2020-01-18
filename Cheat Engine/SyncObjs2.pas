unit SyncObjs2;

{$MODE Delphi}

interface

uses {$ifdef darwin}
  macport, SyncObjs, cthreads, unix, unixtype, pthreads, baseunix,
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
    h: psem_t;
    semaphorecount: cardinal;

    semname: string;
    {$endif}



  public
    function TryAcquire(time: integer=0): boolean;
    procedure Acquire;
    function Release(count:integer=1):integer;
    constructor create(maxcount: integer; init0:boolean=false);
    destructor destroy;  override;
end;

implementation


{$ifdef darwin}
function sem_open(name: pchar; oflags: integer; mode: integer; value: integer):Psem_t;cdecl; external;

var
  count: integer;
{$endif}

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
  {$endif}

  {$ifdef unix}
  {$ifndef darwin}
  i:=sem_init(@h,0,init);
  {$else}
  inc(count);
  semname:='Semaphore'+inttohex(GetCurrentProcessID,8)+'-'+inttostr(count);
  h:=sem_open(pchar(semname), O_CREAT, &644{&777},init);

  if IntPtr(h)=-1 then
  begin
    i:=errno;
    raise exception.create('sem_open error '+inttostr(i));

  end;
  {$endif}
  {$endif}
end;

destructor TSemaphore.destroy;
begin
  {$ifdef windows}
  closehandle(h);
  {$endif}

  {$ifdef unix}
  {$ifndef darwin}
  sem_destroy(h);
  {$else}
  sem_unlink(pchar(semname));
  sem_close(@h);
  {$endif}
  {$endif}

  inherited destroy;
end;

procedure TSemaphore.Acquire;
begin
  {$ifdef windows}
  waitforsingleobject(h,infinite);
  {$else}
  if sem_wait(h)=0 then //wait inside a critical section
    InterlockedDecrement(semaphorecount);
  {$endif}
end;

function TSemaphore.TryAcquire(time: integer=0):boolean;
{$ifndef windows}
var
  t: TThread;
  starttime: qword;

  abstime: timespec;
  tspec: TTimeSpec;
{$endif}
begin
  {$ifdef windows}
  result:=waitforsingleobject(h,time)=WAIT_OBJECT_0;
  {$else}
  starttime:=gettickcount64;
  result:=false;

  if sem_trywait(h)=0 then
  begin
    InterlockedDecrement(semaphorecount);
    exit(true);
  end;




  if time>0 then
  begin
    {$ifndef darwin}
    if clock_gettime(CLOCK_REALTIME, tspec)=0 then
    begin
      //1000000000=1 second
      //100000000=100 milliseconds
      //1000000=1 millisecond
      inc(tspec.tv_nsec, time*1000000);
      while (tv_nsec>=1000000000) do
      begin
        inc(tspec.tv_sec);
        dec(tspec.tv_nsec,1000000000);
      end
      result:=sem_timedwait(h,abstime)=0;
    end
    else sleep(50);
    {$else}
    //mac
    while (gettickcount64<starttime+time) do
    begin
      if sem_trywait(h)=0 then
      begin
        InterlockedDecrement(semaphorecount);
        exit(true);
      end;
      sleep(50);
    end;

    {$endif}

  end;


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

  result:=semaphorecount;

  for e:=1 to count do
  begin
    if semaphorecount<max then
    begin
      InterlockedIncrement(semaphorecount);
      sem_post(h);
    end;
  end;
  {$endif}

end;

end.








