unit speedhackmain;

{$MODE Delphi}


interface
uses windows, classes, sysutils{$ifdef USECS},syncobjs{$endif};

procedure InitializeSpeedhack(speed: single); stdcall;

procedure InitDLL;

type TGetTickCount=function: DWORD; stdcall;
type TGetTickCount64=function: QWORD; stdcall;
type TQueryPerformanceCounter=function(var x: int64): BOOL; stdcall;

function speedhackversion_GetTickCount: DWORD; stdcall;
function speedhackversion_GetTickCount64: QWORD; stdcall;
function speedhackversion_QueryPerformanceCounter(var x: int64): BOOL; stdcall;

type TSimpleLock=record
{$ifdef USECS}
  cs: TCriticalSection;
{$else}
  count: integer;
  owner: dword;
{$endif}
end;

procedure lock(var l: TSimpleLock);
procedure unlock(var l: TSimpleLock);


//function GetTime:dword; stdcall;
//function NewQueryPerformanceCounter(var output: int64):BOOl; stdcall;
var CETick: dword;
    CETick64: int64;

    PerformanceFrequency: int64;
    PerformanceFrequencyMS: int64;
    acceleration: single;
    sleeptime: dword;
    slow: boolean;
    tickerstopped: boolean;
    speedhackenabled: boolean;


   { timeGetTimeInfo:TAPiInfo;
    getTickcountInfo: TAPIInfo;
    QueryPerformanceCounterInfo: TAPIInfo;  }
    winmmlib,kernel32lib: thandle;

    //5.5:
    confighaschanged: integer;
    speedmultiplier: single;
    realgettime: pointer;
    realGetTickCount: pointer;
    realGetTickCount64: pointer;
    realQueryPerformanceCounter: pointer;
    initialoffset: dword;
    initialtime: dword;
    initialoffset64: int64;
    initialtime64: int64;

    initialoffset_tc64: QWord;
    initialtime_tc64: QWord;



    GTCLock: TSimpleLock;
    QPCLock: TSimpleLock;


implementation

procedure lock(var l: TSimpleLock);
var tid: dword;
begin
  {$ifdef USECS}
  l.cs.enter;
  {$else}
  tid:=GetCurrentThreadId;
  if l.owner<>tid then //check if it's already locked
  begin
    while InterLockedExchange(l.count, 1)<>0 do sleep(0);
    l.owner:=tid;
  end
  else
    InterLockedIncrement(l.count);
  {$endif}
end;

procedure unlock(var l: TSimpleLock);
var c: integer;
begin

  {$ifdef USECS}
  l.cs.leave;
  {$else}
  if l.count=1 then      //disable optimization when compiling if you wish to use this code
    l.owner:=0;

  InterLockedDecrement(l.count);
  if l.count<0 then OutputDebugString('error -1');
  {$endif}
end;

function speedhackversion_GetTickCount: DWORD; stdcall;
var currentTime: dword;
begin
  //also used for timeGetTime

  lock(GTCLock);

  currentTime:=TGetTickCount(realgettickcount);
  //time past since activation, mulitplied by speed multiplier
  result:=trunc((currentTime-initialtime)*speedmultiplier)+initialoffset;

  unlock(GTCLock);
end;

function speedhackversion_GetTickCount64: QWORD; stdcall;
var currentTime: qword;
begin
  //also used for timeGetTime

  lock(GTCLock);

  currentTime:=TGetTickCount64(realgettickcount64);
  //time past since activation, mulitplied by speed multiplier
  result:=trunc((currentTime-initialtime_tc64)*speedmultiplier)+initialoffset_tc64;

  unlock(GTCLock);
end;

function speedhackversion_QueryPerformanceCounter(var x: int64): BOOL; stdcall;
var currentTime64: int64;
    newx: int64;
begin
  lock(QPCLock);

  newx:=0;
  currentTime64:=0;

//also used for timeGetTime
  result:=TQueryPerformanceCounter(realQueryPerformanceCounter)(currentTime64);

  //time past since activation, multiplied by speed multiplier
  newx:=trunc((currentTime64-initialtime64)*speedmultiplier)+initialoffset64;

  unlock(QPCLock);

  x:=newx; //access violation possible here
end;

procedure InitializeSpeedhack(speed: single); stdcall;
{
Called by createremotethread
}
begin
  lock(QPCLock);
  lock(GTCLock);

 // outputdebugstring(pchar(format('locked: tid=%d, lockQPC=%d (%d), lockGTC=%d (%d)',[GetThreadID, QPCLock.owner, QPCLock.count, GTCLock.owner, GTCLock.count])));

  //sleep(5000);

  initialoffset:=gettickcount; //get the time the process currently sees (could be fake)
  initialtime:=TGetTickCount(realgettickcount);   //get the real time

 // OutputDebugString('c1');


  QueryPerformanceCounter(initialoffset64);
  TQueryPerformanceCounter(realQueryPerformanceCounter)(initialtime64);


  if realGetTickCount64<>nil then   //xp doesn't have this
  begin
    initialoffset_tc64:=GetTickCount64;
    initialtime_tc64:=TGetTickCount64(realGetTickCount64);
  end;


 // OutputDebugString('c2');
  speedmultiplier:=speed;

 // OutputDebugString('d');
  // sleep(100);

   //OutputDebugString('Going to unlock');
  // outputdebugstring(pchar(format('before unlock: tid=%d, lockQPC=%d (%d), lockGTC=%d (%d)',[GetThreadID, QPCLock.owner, QPCLock.count, GTCLock.owner, GTCLock.count])));

  unlock(GTCLock);
  unlock(QPCLock);

//  outputdebugstring(pchar(format('unlocked: tid=%d, lockQPC=%d (%d), lockGTC=%d (%d)',[GetThreadID, QPCLock.owner, QPCLock.count, GTCLock.owner, GTCLock.count])));

 // OutputDebugString('f');
end;


procedure InitDLL;
begin
  speedmultiplier:=1;
  confighaschanged:=0; //not changed, speed is 1
  initialoffset:=gettickcount;
  initialtime:=initialoffset;
  QueryPerformanceCounter(initialoffset64);
  initialtime64:=initialoffset64;
  loadlibrary('winmm.dll');

  {$ifdef USECS}
  QPCLock.cs:=TCriticalsection.create;
  GTCLock.cs:=TCriticalSection.create;
  {$endif}
end;

end.
