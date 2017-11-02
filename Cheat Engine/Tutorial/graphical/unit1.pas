unit Unit1;

{$mode objfpc}{$H+}

interface

//tutorial step1: player and target
//the player has an ammo capacity of  5 bullets, and the target heals itself each time the player reloads
//each bullet does 10 damage, and the target has 100 health...

//the task: destroy the target.  (e.g add more bullets, make the bullets do more damage, change to code to instant kill, jump to the success code, ...)

uses
  windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  GamePanel, renderobject,glext, GL,glu, player,scoreboard, target, bullet, guitextobject,
  staticguiobject, gamebase, gametutorial1;

type



  { TForm1 }
  TForm1 = class(TForm)
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    p: TGamePanel;
    currentGame: TGameBase;



   // scoreboard: Tscoreboard;

    lasttick: qword;

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

procedure TForm1.FormShow(Sender: TObject);
begin
  p:=TGamePanel.Create(Self);
  p.OnGameRender:=@renderGame;
  p.OnGameTick:=@gametick;
  p.Align:=alClient;
  p.parent:=self;

  currentGame:=TGame1.create(p);


  p.AddKeyEventHandler(@keyhandler);
  lasttick:=GetTickCount64;
end;

end.

