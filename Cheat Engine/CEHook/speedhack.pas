unit speedhack;

interface
uses windows,classes,globals;

type TTick=class(TThread)
  private
  public
    procedure Execute; override;
  end;

procedure InitializeSpeedhack;
procedure StopSpeedhack;

procedure GetTime; stdcall;
//function GetTime:dword; stdcall;
function NewQueryPerformanceCounter(var output: int64):BOOl; stdcall;
var CETick: dword;
    CETick64: int64;
    Ticker: TTick;

    PerformanceFrequency: int64;
    PerformanceFrequencyMS: int64;
    acceleration: single;
    sleeptime: dword;
    slow: boolean;
    tickerstopped: boolean;
    speedhackenabled: boolean;


    timeGetTimeInfo:TAPiInfo;
    getTickcountInfo: TAPIInfo;
    QueryPerformanceCounterInfo: TAPIInfo;
    winmmlib,kernel32lib: thandle;

implementation

procedure InitializeSpeedhack;
var op:dword;
begin
  cetick:=gettickcount;
  //change the gettickcount and timegettime functions so that they look at cetick
  if ticker<>nil then
  begin
    ticker.Terminate;
    stopspeedhack;
  end;
  ticker:=nil;


  winmmlib:=LoadLibrary('winmm.dll');
  if winmmlib<>0 then
  begin
    timeGetTimeInfo.location:=GetProcAddress(winmmlib,'timeGetTime');
    if VirtualProtect(timeGetTimeInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      timeGetTimeInfo.jump[0]:=$e9;
      pdword(@timeGetTimeInfo.jump[1])^:=dword(@GetTime)-dword(timeGetTimeInfo.location)-5;

      try
        asm
          //store original
          push edi
          push esi
          lea edi,timeGetTimeInfo.original[0]
          mov esi,timeGetTimeInfo.location
          movsd
          movsb

          //replace with jump
          lea esi,timeGetTimeInfo.jump[0]
          mov edi,timeGetTimeInfo.location
          movsd
          movsb

          pop esi
          pop edi
        end;
      except

      end;
    end;
  end;


  kernel32lib:=LoadLibrary('kernel32.dll');
  if kernel32lib<>0 then
  begin
    //gettickcount
    GetTickCountInfo.location:=GetProcAddress(kernel32lib,'GetTickCount');
    if VirtualProtect(GetTickCountInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      GetTickCountInfo.jump[0]:=$e9;
      pdword(@GetTickCountInfo.jump[1])^:=dword(@GetTime)-dword(GetTickCountInfo.location)-5;

      try
        asm
          //store original
          push edi
          push esi
          lea edi,GetTickCountInfo.original[0]
          mov esi,GetTickCountInfo.location
          movsd
          movsb

          //replace with jump
          lea esi,GetTickCountInfo.jump[0]
          mov edi,GetTickCountInfo.location
          movsd
          movsb

          pop esi
          pop edi
        end;
      except

      end;
    end;


    //QueryPerformanceCounter
    if QueryPerformanceFrequency(PerformanceFrequency) then
    begin
      QueryPerformanceCounter(CETick64);
      PerformanceFrequencyMS:=PerformanceFrequency div 1000;

      //there is a high performance counter
      QueryPerformanceCounterInfo.location:=GetProcAddress(kernel32lib,'QueryPerformanceCounter');
      if VirtualProtect(QueryPerformanceCounterInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
      begin
        QueryPerformanceCounterInfo.jump[0]:=$e9;
        pdword(@QueryPerformanceCounterInfo.jump[1])^:=dword(@NewQueryPerformanceCounter)-dword(QueryPerformanceCounterInfo.location)-5;

        try
          asm
            //store original
            push edi
            push esi
            lea edi,QueryPerformanceCounterInfo.original[0]
            mov esi,QueryPerformanceCounterInfo.location
            movsd
            movsb

            //replace with jump
            lea esi,QueryPerformanceCounterInfo.jump[0]
            mov edi,QueryPerformanceCounterInfo.location
            movsd
            movsb

            pop esi
            pop edi
          end;
        except

        end;
      end;
    end;
  end;

  speedhackenabled:=true;

  if ticker=nil then ticker:=TTick.Create(false);
end;

procedure StopSpeedhack;
begin
  if not speedhackenabled then exit;

  speedhackenableD:=false;

  try
    asm
      lea esi,timeGetTimeInfo.original[0]
      mov edi,timeGetTimeInfo.location
      movsd
      movsb
    end;
  except

  end;

  try
    asm
      lea esi,GetTickCountInfo.original[0]
      mov edi,GetTickCountInfo.location
      movsd
      movsb
    end;
  except

  end;

  try
    asm
      lea esi,QueryPerformanceCounterInfo.original[0]
      mov edi,QueryPerformanceCounterInfo.location
      movsd
      movsb
    end;
  except

  end;



  FreeLibrary(winmmlib);
  FreeLibrary(kernel32lib);
  winmmlib:=0;
  kernel32lib:=0;
  if ticker<>nil then ticker.terminate;
  ticker:=nil;
end;


procedure GetTime; stdcall;
asm
  mov eax,[CETick]
  ret
end;

{function GetTime:dword; stdcall;
begin
  result:=CETick;
end;}

function NewQueryPerformanceCounter(var output: int64):BOOl; stdcall;
begin
  output:=cetick64;
  result:=true;
end;

procedure TTick.Execute;
begin
  tickerstopped:=false;
  freeonterminate:=true;
  priority:=tpTimeCritical; //if not a thread with higher priority will prevent the timer from running
  while not terminated do
  begin
    inc(cetick64,trunc(acceleration*(PerformanceFrequency / (1000 / sleeptime))) );
    inc(cetick,trunc(sleeptime*acceleration));
    sleep(sleeptime);
  end;
  tickerstopped:=true;
end;

end.
