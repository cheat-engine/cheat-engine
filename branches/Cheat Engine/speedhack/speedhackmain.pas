unit speedhackmain;
{obsolete since 5.5 has a seperate dll initialized by ce's auto assembler}

interface
uses windows,classes;

procedure InitializeSpeedhack(speed: single); stdcall;

type TGetTickCount=function: DWORD; stdcall;
type TQueryPerformanceCounter=function(var x: int64): BOOL; stdcall;

function speedhackversion_GetTickCount: DWORD; stdcall;
function speedhackversion_QueryPerformanceCounter(var x: int64): BOOL; stdcall;

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
    realQueryPerformanceCounter: pointer;
    initialoffset: dword;
    initialtime: dword;
    initialoffset64: int64;
    initialtime64: int64;

implementation

function speedhackversion_GetTickCount: DWORD; stdcall;
var x: dword;
begin
//also used for timeGetTime
  x:=TGetTickCount(realgettickcount);
  //time past since activation, mulitplied by speed multiplier
  result:=trunc((x-initialtime)*speedmultiplier)+initialoffset; 

end;

function speedhackversion_QueryPerformanceCounter(var x: int64): BOOL; stdcall;
var y: int64;
begin
//also used for timeGetTime
  result:=TQueryPerformanceCounter(realQueryPerformanceCounter)(y);

  //time past since activation, mulitplied by speed multiplier
  x:=trunc((y-initialtime64)*speedmultiplier)+initialoffset64;

end;

procedure InitializeSpeedhack(speed: single); stdcall;
{
Called by createremotethread
}
var x: int64;
begin


 // messagebox(0,'called','called',mb_ok);
  initialoffset:=gettickcount;
  initialtime:=TGetTickCount(realgettickcount);

  QueryPerformanceCounter(x);
  initialoffset64:=x;
  TQueryPerformanceCounter(realQueryPerformanceCounter)(x);
  initialtime64:=x;

  speedmultiplier:=speed;
end;

end.
