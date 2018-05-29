unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  GamePanel, renderobject,glext, GL,glu, player,scoreboard, target, bullet, guitextobject,
  staticguiobject, gamebase, gametutorial1, gametutorial2, gametutorial3;

type



  { TForm1 }
  TForm1 = class(TForm)
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    p: TGamePanel;
    currentGame: TGameBase;

    lasttick: qword;

    procedure startgame2(sender: TObject);
    procedure startgame3(sender: TObject);
    procedure finishedTutorial(sender: TObject);


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

procedure TForm1.finishedTutorial(sender: TObject);
var s,s2: string;
begin
  if usedcheats then
  begin
    s:='winned';
    s2:='yay';
  end
  else
  begin
    s:='beaten';
    s2:='Well done';
  end;

  showmessage(format('You have %s all 3 ''games''. %s!',[s,s2]));
  ExitProcess(0);
end;

procedure TForm1.startgame3(sender: TObject);
begin
  if currentgame<>nil then
    freeandnil(currentGame);

  caption:='Step 3';
  currentgame:=TGame3.create(p);
  currentgame.OnWin:=@finishedTutorial;    //todo someday: rpg kinda game, followed by an online 'game' (chatgame more likely)
end;

procedure TForm1.startgame2(sender: TObject);
begin
  if currentgame<>nil then
    freeandnil(currentGame);

  caption:='Step 2';
  currentgame:=TGame2.create(p);
  currentgame.OnWin:=@startgame3;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  p:=TGamePanel.Create(Self);
  p.OnGameRender:=@renderGame;
  p.OnGameTick:=@gametick;
  p.Align:=alClient;
  p.parent:=self;

  // startgame3(Self);
  // startgame2(self);

  currentGame:=TGame1.create(p);
  currentGame.OnWin:=@startGame2;

 //currentGame:=TGame2.create(p);

  p.AddKeyEventHandler(@keyhandler);
  lasttick:=GetTickCount64;
end;

end.

