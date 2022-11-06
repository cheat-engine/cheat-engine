unit SyncObjs2;

{$MODE Delphi}

interface

uses {$ifdef darwin}
  macport, cthreads, unix, unixtype, pthreads, baseunix,
  {$else}
  windows,
  {$endif}SyncObjs, classes, sysutils, LCLIntf;

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


type
  TThreadHelper=class helper for TThread
    function WaitTillDone(timeout: DWORD; granularity: integer=25): boolean;
  end;


{$ifdef THREADNAMESUPPORT}
function GetThreadName(tid: TThreadID={$ifdef windows}0{$else}nil{$endif}): string;
{$endif}


implementation

uses networkInterfaceApi, maps;

var
  tm: TThreadManager;
  oldSetThreadDebugNameA: procedure(threadHandle: TThreadID; const ThreadName: AnsiString);

  threadnames: TMap;
  threadnamesCS: TCriticalSection;

{$ifdef THREADNAMESUPPORT}

function GetThreadName(tid: TThreadID={$ifdef windows}0{$else}nil{$endif}): string;
var s: pstring;
begin
  result:='';
  if {$ifdef windows}tid=0{$else}tid=nil{$endif} then tid:=GetCurrentThreadId;

  threadnamesCS.enter;
  s:=nil;
  if threadnames.GetData(tid, s) then
    result:=s^;

  threadnamesCS.Leave;
end;

procedure SetThreadDebugNameA(tid: TThreadID; const ThreadName: AnsiString);
var s: pstring;
  str: string;
begin
  if assigned(oldSetThreadDebugNameA) then
    oldSetThreadDebugNameA(tid, threadname);

  if tid=TThreadID(-1) then
    tid:=GetCurrentThreadId;

  threadnamesCS.enter;
  if threadnames.GetData(tid, s) then
  begin
    DisposeStr(s);
    threadnames.Delete(tid);
  end;
  threadnames.Add(tid, NewStr(threadname));
  threadnamesCS.Leave;

  if (tid=GetCurrentThreadId) and (getConnection<>nil) then
    Getconnection.setconnectionname(threadname);


end;

procedure EndThread(exitcode: dword);
var s: pstring;
begin
  threadnamesCS.enter;
  if threadnames.GetData(GetCurrentThreadId, s) then
  begin
    DisposeStr(s);
    threadnames.Delete(GetCurrentThreadId);
  end;

  threadnamesCS.Leave;
end;
{$endif}

function TThreadHelper.WaitTillDone(timeout: dword; granularity: integer=25): boolean;
var
  needsynchronize: boolean;
  endtime: qword;
begin
  needsynchronize:=MainThreadID=GetCurrentThreadId;

  if Finished then exit(true);
  if timeout=0 then exit(Finished);
  if timeout=$ffffffff then
  begin
    WaitFor;
    exit(true);
  end;
  endtime:=gettickcount64+timeout;


  repeat
    if needsynchronize then
      CheckSynchronize(granularity)
    else
    begin
{$ifdef windows}
      exit(waitforsingleobject(self.handle, timeout)=WAIT_OBJECT_0);
{$else}
      sleep(granularity);
{$endif}
    end;
  until (gettickcount64>endtime) or Finished;

  result:=finished;
end;

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

{$ifdef THREADNAMESUPPORT}
procedure finalizeThreadNames;
var i: TMapIterator;
  s: pstring;
begin
  threadnamesCS.enter;
  try
    i:=TMapIterator.Create(threadnames);
    i.First;
    while not i.EOM do
    begin
      s:=nil;
      i.GetData(s);

      if s<>nil then
        DisposeStr(s);

      i.Next;
    end;
  finally
    threadnamesCS.leave;
  end;

  freeandnil(threadnamesCS);
  freeandnil(threadnames);
end;
{$endif}

initialization
  {$ifdef THREADNAMESUPPORT}
  threadnames:=TMap.Create(itu4,sizeof(pointer));
  threadnamesCS:=TCriticalSection.Create;


  GetThreadManager(tm);
  oldSetThreadDebugNameA:=tm.SetThreadDebugNameA;
  tm.SetThreadDebugNameA:=@SetThreadDebugNameA;
  SetThreadManager(tm);
  {$endif}

finalization
  {$ifdef THREADNAMESUPPORT}
  finalizeThreadNames();
  {$endif}



end.








