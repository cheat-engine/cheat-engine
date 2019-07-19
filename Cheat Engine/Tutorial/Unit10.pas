unit Unit10;
{Shared code tutorial}

{$MODE Delphi}

interface

uses
  windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Unit8, StdCtrls, Buttons, LResources, ExtCtrls, math;

type
  TPlayer=class
  private
  public
    health: Single;
    boguscrap: dword;
    unrelatedrandomlychangingthing: integer;
    team: integer;
    name: string[63];
    teammate: TPlayer;
    healthlabel: TLabel;
    wasteofspace: array [0..99123] of byte;
    procedure Hit(damage: integer);
  end;

type

  { TForm10 }

  TForm10 = class(TForm8)
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
    p1, p2, p3,p4: TPlayer;

    procedure  LaunchGraphicalTut;

  public
    { Public declarations }
  end;

var
  Form10: TForm10;

implementation

uses Unit4, frmHelpUnit;

resourcestring
  rsThisPlayerIsAlreadyDeadRestartTheGame = 'This player is already dead. Restart the game';
  rsDEAD = 'DEAD';
  rsHealth = 'Health: %s';
  rsStop = 'Stop';
  rsRestartGameAndAutoplay = 'Restart game and autoplay';
  rsFailureYourTeamDied = 'Failure. Your team died';
  rsTryAgain10 = 'Can''t figure out how to do this? Don''t worry. Try asking in the forum at cheatengine.org or perhaps someone already explained it better '
    +'there. Are you sure you want to quit?';
  rsStep9SharedCodePW = 'Step 9: Shared code: (PW=%s)';

  rsTutorialStep9=
    'This step will explain how to deal with code that is used for other object of the same type'+#13#10+
    ''+#13#10+
    'Often when you''ve found health of a unit or your own player, you will find that if you remove the code, it affects '+
    'enemies as well.'+#13#10+
    'In these cases you must find out how to distinguish between your and the enemies objects.'+#13#10+
    'Sometimes this is as easy as checking the first 4 bytes (Function pointer table) which often point to a unique location '+
    'for the player, and sometimes it''s a team number, or a pointer to a pointer to a pointer to a pointer to a pointer to a '+
    'playername. It all depends on the complexity of the game, and your luck'+#13#10+
    ''+#13#10+
    'The easiest method is finding what addresses the code you found writes to and then use the dissect data feature to '+
    'compare against two structures. (Your unit(s)/player and the enemies) And then see if you can find out a way to '+
    'distinguish between them.'+#13#10+
    'When you have found out how to distinguish between you and the computer you can inject an assembler script that '+
    'checks for the condition and then either do not execute the code or do something else. (One hit kills for example)'+#13#10+
    'Alternatively, you can also use this to build a so called "Array of byte" string which you can use to search which will '+
    'result in a list of all your or the enemies players'+
    ''+#13#10+
    'In this tutorial I have implemented the most amazing game you will ever play.'+#13#10+
    'It has 4 players. 2 Players belong to your team, and 2 Players belong to the computer. '+#13#10+
    'Your task is to find the code that writes the health and make it so you win the game WITHOUT freezing your health'+#13#10+
    'To continue, press "Restart game and autoplay" to test that your code is correct'+#13#10+
    ''+#13#10+
    ''+#13#10+
    'Tip: Health is a float'+#13#10+
    'Tip2: There are multiple solutions';
  rsU10ThisWasTheLastTutorial = 'This was the last tutorial and you skipped it. You lose';

procedure TPlayer.Hit(damage: integer);
var x: single;
begin
  if health=0 then
  begin
    showmessage(rsThisPlayerIsAlreadyDeadRestartTheGame);
    exit;
  end;

  x:=max(0, health-damage);
  health:=x;

  if health=0 then
    healthlabel.Caption:=rsDEAD
  else
    healthlabel.caption:=Format(rsHealth, [FloatToStr(health)]);

  unrelatedrandomlychangingthing:=random(5000000);
end;


procedure TForm10.LaunchGraphicalTut;
var nexttut: string;
    filename: string;
begin
  filename:='gtutorial-'+{$ifdef cpu32}'i386'{$else}'x86_64'{$endif}+'.exe';
  nexttut:=ExtractFilePath(application.ExeName)+filename;

  if fileexists(nexttut) then
  begin
    //launch the graphical tutorial
    ShellExecute(0, PChar('open'), PChar(nexttut),PChar(''), PChar(extractfilepath(nexttut)), SW_SHOW);
    ExitProcess(0);
  end;


  nexttut:=ExtractFileDir(application.ExeName);

  if ExtractFileName(nexttut)='bin' then
  begin
    nexttut:=ExtractFilePath(nexttut)+'tutorial\graphical\'+filename;

    if fileexists(nexttut) then
    begin
      //launch the graphical tutorial
      ShellExecute(0, PChar('open'), PChar(nexttut),PChar(''), PChar(extractfilepath(nexttut)), SW_SHOW);
      ExitProcess(0);
    end;
  end;
end;

procedure TForm10.Button2Click(Sender: TObject);
begin
  LaunchGraphicalTut;
  hide;
  form4:=tform4.create(self);
  form4.show;

  frmHelp.free;
  frmHelp:=nil;
end;

procedure TForm10.Button1Click(Sender: TObject);
begin



end;

procedure TForm10.Button3Click(Sender: TObject);
var z: pchar;
begin
  //memoryleak on purpose
  p1:=TPlayer.Create;
  p1.name:='Dave';
  p1.health:=100;
  p1.boguscrap:=random(10000);
  p1.team:=1;
  p1.healthlabel:=Label4;
  p1.healthlabel.caption:=Format(rsHealth, [FloatToStr(p1.health)]);

  getmem(z, 1+random(90000));

  p2:=TPlayer.Create;
  p2.name:='Eric';
  p2.health:=100;
  p2.boguscrap:=random(10000);
  p2.team:=1;
  p2.healthlabel:=Label6;
  p2.healthlabel.caption:=Format(rsHealth, [FloatToStr(p2.health)]);

  p1.teammate:=p2;
  p2.teammate:=p1;

  getmem(z, 1+random(90000));

  p3:=TPlayer.Create;
  p3.name:='HAL';
  p3.health:=500;
  p3.boguscrap:=random(10000);
  p3.team:=2;
  p3.healthlabel:=label8;
  p3.healthlabel.caption:=Format(rsHealth, [FloatToStr(p3.health)]);

  getmem(z, 1+random(90000));

  p4:=TPlayer.Create;
  p4.name:='KITT';
  p4.health:=500;
  p4.boguscrap:=random(10000);
  p4.team:=2;
  p4.healthlabel:=label10;
  p4.healthlabel.caption:=Format(rsHealth, [FloatToStr(p4.health)]);

  p3.teammate:=p4;
  p4.teammate:=p3;
end;

procedure TForm10.Button4Click(Sender: TObject);
begin
  p3.Hit(1+random(1));
end;

procedure TForm10.Button5Click(Sender: TObject);
begin
  p4.Hit(1+random(1));
end;

procedure TForm10.Button6Click(Sender: TObject);
begin
  if button6.Caption=rsStop then
  begin
    button6.Caption:=rsRestartGameAndAutoplay;
    exit;
  end;

  button3.Click;
  button6.Caption:=rsStop;

  while button6.Caption=rsStop do
  begin
    if p1.health>0 then p1.Hit(2+random(5));
    if p2.health>0 then p2.Hit(2+random(5));
    if p3.health>0 then p3.Hit(1+random(1));
    if p4.health>0 then p4.Hit(1+random(1));

    application.ProcessMessages;

    if (p1.health=0) and (p2.health=0) then
    begin
      ShowMessage(rsFailureYourTeamDied);
      button6.Caption:=rsRestartGameAndAutoplay;
      break;
    end;

    if (p3.health=0) and (p4.health=0) then
    begin
      button2.enabled:=true;
      button6.Caption:=rsRestartGameAndAutoplay;
      break;
    end;
  end;
end;

procedure TForm10.Button7Click(Sender: TObject);
begin
  p2.Hit(2+random(5));
end;

procedure TForm10.Button8Click(Sender: TObject);
begin
  p1.Hit(2+random(5));
end;

procedure TForm10.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  canclose:=MessageDlg(rsTryAgain10, mtconfirmation, [mbyes, mbno], 0)=mryes;
end;

procedure TForm10.FormCreate(Sender: TObject);
begin

      //31337157
  memo1.lines.text:=rsTutorialStep9;
  memo1.Lines.Insert(0, Format(rsStep9SharedCodePW, [inttostr(313)+inttostr(37157)]));

  button3.Click;
  font.size:=12;
  frmHelp.attach(self,'9');
end;

procedure TForm10.SpeedButton1Click(Sender: TObject);
begin
  LaunchGraphicalTut;
  showmessage(rsU10ThisWasTheLastTutorial);
  Application.Terminate;
end;

initialization
  {$i Unit10.lrs}

end.
