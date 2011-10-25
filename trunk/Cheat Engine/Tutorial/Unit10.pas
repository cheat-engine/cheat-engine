unit Unit10;
{Shared code tutorial}

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Unit8, StdCtrls, Buttons, LResources, math;

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
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
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
  public
    { Public declarations }
  end;

var
  Form10: TForm10;

implementation

uses Unit4;

procedure TPlayer.Hit(damage: integer);
var x: single;
begin
  if health=0 then
  begin
    showmessage('this player is already dead. Restart the game');
    exit;
  end;

  x:=max(0, health-damage);
  health:=x;

  if health=0 then
    healthlabel.Caption:='DEAD'
  else
    healthlabel.caption:='Health: '+FloatToStr(health);

  unrelatedrandomlychangingthing:=random(5000000);
end;

procedure TForm10.Button2Click(Sender: TObject);
begin
  hide;
  form4:=tform4.create(self);
  form4.show;
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
  p1.healthlabel.caption:='Health: '+FloatToStr(p1.health);

  getmem(z, 1+random(90000));

  p2:=TPlayer.Create;
  p2.name:='Eric';
  p2.health:=100;
  p2.boguscrap:=random(10000);
  p2.team:=1;
  p2.healthlabel:=Label6;
  p2.healthlabel.caption:='Health: '+FloatToStr(p2.health);

  p1.teammate:=p2;
  p2.teammate:=p1;

  getmem(z, 1+random(90000));

  p3:=TPlayer.Create;
  p3.name:='HAL';
  p3.health:=500;
  p3.boguscrap:=random(10000);
  p3.team:=2;
  p3.healthlabel:=label8;
  p3.healthlabel.caption:='Health: '+FloatToStr(p3.health);

  getmem(z, 1+random(90000));

  p4:=TPlayer.Create;
  p4.name:='KITT';
  p4.health:=500;
  p4.boguscrap:=random(10000);
  p4.team:=2;
  p4.healthlabel:=label10;
  p4.healthlabel.caption:='Health: '+FloatToStr(p4.health);

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
  if button6.Caption='Stop' then
  begin
    button6.Caption:='Restart game and autoplay';
    exit;
  end;

  button3.Click;
  button6.Caption:='Stop';

  while button6.Caption='Stop' do
  begin
    if p1.health>0 then p1.Hit(2+random(5));
    if p2.health>0 then p2.Hit(2+random(5));
    if p3.health>0 then p3.Hit(1+random(1));
    if p4.health>0 then p4.Hit(1+random(1));

    application.ProcessMessages;

    if (p1.health=0) and (p2.health=0) then
    begin
      ShowMessage('Failure. Your team died');
      button6.Caption:='Restart game and autoplay';
      break;
    end;

    if (p3.health=0) and (p4.health=0) then
    begin
      button2.enabled:=true;
      button6.Caption:='Restart game and autoplay';
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
  canclose:=MessageDlg('Can''t figure out how to do this? Don''t worry. Try asking in the forum at cheatengine.org or perhaps someone already explained it better there. Are you sure you want to quit?',mtconfirmation,[mbyes,mbno],0)=mryes;
end;

procedure TForm10.FormCreate(Sender: TObject);
begin
      //31337157
  memo1.Lines.Insert(0,'Step 9: Shared code: (PW='+inttostr(313)+inttostr(37157)+')');

  button3.Click;

end;

procedure TForm10.SpeedButton1Click(Sender: TObject);
begin
  showmessage('This was the last tutorial and you skipped it. You lose');
  Application.Terminate;
end;

initialization
  {$i Unit10.lrs}

end.
