unit injectedpointerscanunit;

interface

uses classes, sysutils,windows,messages,cefuncproc;

type TInjectPointerscan=class(tthread)
  public
    processid: dword;
    address: dword;
    found: boolean;
    procedure execute; override;
end;

procedure InjectPointerscan(address:dword);

implementation

function enum(hwnd:THANDLE; lp: lParam ):BOOL; stdcall;
var self: TInjectPointerscan;
    pid: dword;
    tempbuf: pchar;
begin
  self:=TInjectPointerscan(lp);
  GetWindowThreadProcessId(hwnd,pid);
  if pid=self.processid then
  begin
    //check the name
    getmem(tempbuf,100);
    getwindowtext(hwnd,tempbuf,100);
    if tempbuf='CE Injected Pointerscan' then
    begin
      postmessage(hwnd,wm_user+3,self.address,0);
      self.found:=true;
      result:=false;
      exit;
    end;

    freemem(tempbuf);

  end;


  result:=true;
end;

procedure TInjectPointerscan.execute;
var i: integer;
    x: dword;
begin
  freeonterminate:=true;
  injectdll(CheatEngineDir+'pscan.dll','');
  sleep(250);
  i:=0;
  while i<10 do //it has 10 seconds
  begin
    //find the processscan window
    found:=false;
    EnumWindows(@enum,dword(self));
    if found then exit;

    sleep(1000);
  end;
end;

procedure InjectPointerscan(address:dword);
var temp: TInjectPointerscan;
begin
  temp:=TInjectPointerscan.create(true);
  temp.processid:=processid;
  temp.address:=address;
  temp.resume;
end;

end.
