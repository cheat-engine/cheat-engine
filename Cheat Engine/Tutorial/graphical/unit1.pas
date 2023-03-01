unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  jwawindows, windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  GamePanel, renderobject,glext, GL,glu, player,scoreboard, target, bullet, guitextobject,
  staticguiobject, gamebase, levelselect, gametutorial1, gametutorial2, gametutorial3;

type



  { TForm1 }
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    p: TGamePanel;
    currentGame: TGameBase;

    lasttick: qword;
    codebase: ptruint;
    codesize: ptruint;


    integrityCheckThread: TThread;
    procedure integritycheckcheck;

    procedure startgame2(sender: TObject);
    procedure startgame3(sender: TObject);
    procedure finishedTutorial(sender: TObject);
    procedure GameSelect(sender: TObject);


    procedure renderGame(sender: TObject);
    procedure gametick(sender: TObject);
    function KeyHandler(GamePanel: TObject; keventtype: integer; Key: Word; Shift: TShiftState):boolean;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

uses registry, md5, frmHelpUnit;

type
  TIntegrityCheckThread=class(tthread)
  private
    codebase: ptruint;
    codesize: integer;
    integrityCheckValue: dword;
    function check1: dword;
    function check2: dword;
    function check3: dword;
    procedure valid;
    procedure invalid;
  public

    heartbeat: qword;
    lastcheckwasvalid: boolean;
    procedure execute; override;
    procedure docheck;
  end;


function peinfo_getcodesize(header: pointer; headersize: integer=0): dword;
var
    ImageNTHeader: PImageNtHeaders;
begin
  result:=0;

  if (headersize=0) or (PImageDosHeader(header)^._lfanew<=headersize-sizeof(TImageNtHeaders)) then
  begin
    ImageNTHeader:=PImageNtHeaders(ptrUint(header)+PImageDosHeader(header)^._lfanew);
    result:=ImageNTHeader^.OptionalHeader.SizeOfCode;
  end;
end;

function peinfo_getdatabase(header: pointer; headersize: integer=0): ptrUint;
var
    ImageNTHeader: PImageNtHeaders;
begin
  result:=0;
  if (headersize=0) or (PImageDosHeader(header)^._lfanew<=headersize-sizeof(TImageNtHeaders)) then
  begin
    ImageNTHeader:=PImageNtHeaders(ptrUint(header)+PImageDosHeader(header)^._lfanew);
    {$ifndef cpu64}
    result:=ImageNTHeader^.OptionalHeader.BaseOfData;
    {$endif}
  end;
end;

function peinfo_getcodebase(header: pointer; headersize: integer=0): ptrUint;
var
    ImageNTHeader: PImageNtHeaders;
begin
  result:=0;
  if (headersize=0) or (PImageDosHeader(header)^._lfanew<=headersize-sizeof(TImageNtHeaders)) then
  begin
    ImageNTHeader:=PImageNtHeaders(ptrUint(header)+PImageDosHeader(header)^._lfanew);
    result:=ImageNTHeader^.OptionalHeader.BaseOfCode;
  end;
end;

function peinfo_getEntryPoint(header: pointer; headersize: integer=0): ptrUint;
var
    ImageNTHeader: PImageNtHeaders;
begin
  result:=0;
  if (headersize=0) or (PImageDosHeader(header)^._lfanew<=headersize-sizeof(TImageNtHeaders)) then
  begin
    ImageNTHeader:=PImageNtHeaders(ptrUint(header)+PImageDosHeader(header)^._lfanew);
    result:=ImageNTHeader^.OptionalHeader.AddressOfEntryPoint;
  end;

end;

function peinfo_getheadersize(header: pointer): dword;
var
    ImageNTHeader: PImageNtHeaders;
begin
  result:=0;
  if PImageDosHeader(header)^.e_magic<>IMAGE_DOS_SIGNATURE then exit;

  ImageNTHeader:=PImageNtHeaders(ptrUint(header)+PImageDosHeader(header)^._lfanew);
  if ptrUint(ImageNTHeader)-ptrUint(header)>$1000 then exit;
  if ImageNTHeader^.Signature<>IMAGE_NT_SIGNATURE then exit;

  result:=ImageNTHeader^.OptionalHeader.SizeOfHeaders;
end;

function peinfo_getimagesize(header: pointer): dword;
var
    ImageNTHeader: PImageNtHeaders;
begin
  ImageNTHeader:=PImageNtHeaders(ptrUint(header)+PImageDosHeader(header)^._lfanew);
  result:=ImageNTHeader^.OptionalHeader.SizeOfImage;
end;


function TIntegrityCheckThread.check1:dword;
var
  mem: pword;
  r: dword;
  i: integer;
begin
  mem:=pword(codebase);
  r:=$deadbeef;
  for i:=0 to (codesize div 2)-1 do
  begin
    r:=r xor mem[i];
    inc(r,12);
  end;

  result:=r;
end;

function TIntegrityCheckThread.check2:dword;
var
  mem: pword;
  r: dword;
  i: integer;
begin
  mem:=pword(codebase);
  r:=$feeddead;
  for i:=0 to (codesize div 2)-1 do
  begin
    r:=r + mem[i];
  end;

  result:=r;
end;

function TIntegrityCheckThread.check3:dword;
var
  mem: pword;
  r: dword;
  i: integer;
begin
  mem:=pword(codebase);
  r:=$cececece;
  for i:=0 to (codesize div 2)-1 do
  begin
    r:=r xor mem[i];
    dec(r,77);
  end;

  result:=r;
end;

procedure TIntegrityCheckThread.valid;
var
  i: integer;
  s: string;
begin
  s:=form1.caption;
  i:=Pos(' (Integrity check error)',s);
  if i>0 then //valid now
  begin
    form1.caption:=copy(form1.caption,1,i);
  end;
end;

procedure TIntegrityCheckThread.invalid;
var s: string;
begin
  s:=form1.caption;
  if Pos(' (Integrity check error)',s)=0 then
    form1.caption:=form1.caption+' (Integrity check error)';
end;

procedure TIntegrityCheckThread.docheck;
var
  c1,c2,c3: dword;
  cf: dword;
begin
  c1:=check1;
  c2:=check2;
  c3:=check3;

  cf:=(c1 xor c2)+c3;
  if cf=0 then inc(cf);

  if integrityCheckValue=0 then
    integrityCheckValue:=cf
  else
  begin
    lastcheckwasvalid:=cf=integrityCheckValue;
    if lastcheckwasvalid then
      synchronize(@Valid)
    else
      synchronize(@Invalid);
  end;
end;

procedure TIntegrityCheckThread.execute;
var

  m: pointer;
begin
  m:=pointer(GetModuleHandle(nil));

  codebase:=ptruint(m)+peinfo_getcodebase(m);
  codesize:=peinfo_getcodesize(m);

  while not terminated do
  begin
    heartbeat:=GetTickCount64;
    docheck;


    sleep(100);
  end;
end;

procedure TForm1.gametick(sender:TObject);
var
  currenttime: qword;
  diff: qword;
begin
  currenttime:=GetTickCount64;
  diff:=currenttime-lasttick;
  lasttick:=currenttime;

  if currentgame<>nil then
    currentgame.gametick(currenttime, diff);
end;

procedure TForm1.renderGame(sender: TObject);
begin
  if currentgame<>nil then
    currentgame.render;
end;

function TForm1.KeyHandler(GamePanel: TObject; keventtype: integer; Key: Word; Shift: TShiftState):boolean;
begin
  if currentgame<>nil then
    exit(currentgame.keyhandler(GamePanel, keventtype, key, shift))
  else
    result:=false;
end;

procedure TForm1.IntegrityCheckCheck;
var
  itt: TIntegrityCheckThread;
  hb: qword;
  startwait: qword;
begin
  itt:=TIntegrityCheckThread(integrityCheckThread);
  itt.docheck;

  hb:=itt.heartbeat;
  if ((gettickcount64-hb)>1000) then
  begin
    startwait:=gettickcount64;
    while (gettickcount64-startwait)<5000 do //wait 5 seconds max
    begin
      if itt.heartbeat>hb then exit;
      CheckSynchronize(100);
    end;

    itt.Terminate;
    itt.WaitFor;
    itt.lastcheckwasvalid:=false;
    form1.caption:=form1.caption+' (total integrity check failure)';
  end;

end;


procedure TForm1.finishedTutorial(sender: TObject);
var
  s,s2: string;
  mem: pword;
  r: dword;
  i: integer;
begin
  if usedcheats then
  begin
    s:='winned';
    s2:='. yay';
  end
  else
  begin
    s:='beaten';

    IntegrityCheckCheck;

    if TIntegrityCheckThread(integrityCheckThread).lastcheckwasvalid then
      s2:=#13#10', and you have beaten the integrity check!'#13#10#13#10'Really well done!'
    else
      s2:='. Well done';
  end;


  showmessage(format('You have %s all 3 ''games'' %s!',[s,s2]));
  ExitProcess(0);
end;

procedure TForm1.startgame3(sender: TObject);
begin
  IntegrityCheckCheck;

  if currentgame<>nil then
    freeandnil(currentGame);

  caption:='Step 3';
  currentgame:=TGame3.create(p);
  currentgame.OnWin:=@finishedTutorial;    //todo someday: rpg kinda game, followed by an online 'game' (chatgame more likely)

  frmHelp.Attach(self,'G3');
end;


procedure TForm1.startgame2(sender: TObject);

begin
  IntegrityCheckCheck;

  if currentgame<>nil then
    freeandnil(currentGame);

  caption:='Step 2';
  currentgame:=TGame2.create(p);
  currentgame.OnWin:=@startgame3;
  frmHelp.Attach(self,'G2');
end;

procedure TForm1.GameSelect(sender: TObject);
var gs: TLevelSelect;
begin
  IntegrityCheckCheck;

  gs:=TLevelSelect(currentgame);
  case gs.level of
    1:
    begin
      if currentgame<>nil then
        freeandnil(currentGame);

      Caption:='Step 1';
      currentGame:=TGame1.create(p);
      currentGame.OnWin:=@startGame2;
    end;
    2: startgame2(sender);
    3: startgame3(sender);
  end;
end;



procedure TForm1.FormCreate(Sender: TObject);
begin
  integrityCheckThread:=TIntegrityCheckThread.create(false);
end;

procedure TForm1.FormShow(Sender: TObject);
var reg: Tregistry;
begin
  p:=TGamePanel.Create(Self);
  p.OnGameRender:=@renderGame;
  p.OnGameTick:=@gametick;
  p.Align:=alClient;
  p.parent:=self;

  // startgame3(Self);
  // startgame2(self);

  reg:=tregistry.create;
  if reg.OpenKey('\Software\Cheat Engine\GTutorial', false) then
  begin
    //if reg.ValueExists('This does not count as a solution for tutorial 1') then
    begin
      //level select screen
      Caption:='Select Level';
      currentGame:=TLevelSelect.create(p);
      currentGame.OnWin:=@GameSelect;
    end;
  end;

  //still here, so game1:
  if currentgame=nil then
  begin
    IntegrityCheckCheck;

    currentGame:=TGame1.create(p);
    currentGame.OnWin:=@startGame2;
    frmHelp.Attach(self,'G1');
  end;

  p.AddKeyEventHandler(@keyhandler);
  lasttick:=GetTickCount64;
end;

end.

