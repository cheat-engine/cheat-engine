program systemcallsignal;

uses windows,messages;

type tsharedmem=record
  Infunction:boolean;
  RetrieverWindowHandle: thandle;
end;

var
  sharedmem: ^tsharedmem;
  sharedmemmapping: thandle;


function x(handle:thandle; lparam:dworD):bool; stdcall;
begin
  result:=true;
end;

procedure function1;
begin
 // Messagebox(0,'1','1',MB_ICONERROR);

  sharedmem^.Infunction:=true;
  asm
    int 3
  end;
  //loadlibrary('emptydll.dll');
  EnumChildWindows(sharedmem.RetrieverWindowHandle,@x,0);
  sharedmem^.Infunction:=false;
end;

procedure function2;
var threadid,procid: dword;
begin
 // Messagebox(0,'2','2',MB_ICONERROR);

  sharedmem^.Infunction:=true;
  asm
    int 3
  end;
  threadid:=GetWindowThreadProcessId(sharedmem^.RetrieverWindowHandle,procid);
  sharedmem^.Infunction:=false;

  if threadid=0 then Messagebox(0,'Error 2','Signal Caller Error',MB_OK);
end;

procedure function3;
var found: thandle;
begin
  //Messagebox(0,'3','3',MB_ICONERROR);

  sharedmem^.Infunction:=true;
  asm
    int 3
  end;

  found:=FindWindowEx(0,0,'TForm1','Callnumber retriever');
  sharedmem^.infunction:=false;

  if found=0 then Messagebox(0,'Error 3','Signal Caller Error',MB_OK);
end;

procedure function4;
var h: thandle;
begin
 // Messagebox(0,'4','4',MB_ICONERROR);

  sharedmem^.Infunction:=true;
  asm
    int 3
  end;
  h:=getforegroundwindow;
  sharedmem^.infunction:=false;

  if h=0 then beep(1000,10);

end;

procedure function5;
var h: thandle;
begin
 // Messagebox(0,'4','4',MB_ICONERROR);

  sharedmem^.Infunction:=true;
  asm
    int 3
  end;
  h:=getdc(0);
  sharedmem^.infunction:=false;

  if h=0 then beep(1000,10);

end;

begin
  SharedMemMapping:=OpenFileMapping(FILE_MAP_ALL_ACCESS,false,'SystemCallInfo');
  if SharedMemMapping=0 then
  begin
    Messagebox(0,'There was a error but I refuse to tell you what it is.','Signal Caller Error',MB_ICONERROR);
    exit;
  end;

  SharedMem:=MapViewOfFile(SharedMemMapping,FILE_MAP_ALL_ACCESS,0,0,0);

  if paramcount=0 then
  begin
    try
    function1;
    sleep(100);

    function2;
    sleep(100);

    function3;
    sleep(100);

    function4;
    sleep(100);

    function5;
    sleep(100);
    
    except
      Messagebox(0,'There was a error but I refuse to tell you what it is.','Signal Caller Error',MB_ICONERROR);
      exit;
    end;
  end;

  if paramcount=1 then
  begin
  try
    if ParamStr(1)='1' then function1;
    if paramstr(1)='2' then function2;
    if paramstr(1)='3' then function3;
    if paramstr(1)='4' then function4;
    if paramstr(1)='5' then function5;
  except
    sharedmem.Infunction:=false;
    Messagebox(0,'There was a error but I refuse to tell you what it is.','Signal Caller Error',MB_ICONERROR);
  end;
  end;
end.
