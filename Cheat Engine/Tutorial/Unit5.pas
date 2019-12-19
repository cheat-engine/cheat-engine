unit Unit5;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls,math, Buttons, LResources;

type
  TForm5 = class(TForm)
    Memo1: TMemo;
    Button2: TButton;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Timer1: TTimer;
    Label3: TLabel;
    Label4: TLabel;
    Button3: TButton;
    Label5: TLabel;
    Label6: TLabel;
    SpeedButton1: TSpeedButton;
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
    health: single;
    ammo: double;
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

uses Unit4, Unit6, frmHelpUnit;

resourcestring
  rsStep4FloatingPointsPW = 'Step 4: Floating points (PW=';
  rsOutOfAmmo = 'Out of ammo!%sPress ok to stock up on some ammo';
  rsDead = 'I think you''re dead!%sPress ok to become a brain eating zombie';
  rsConfirmClose5 = 'Come on. This step is simple. For health do a float scan, and for ammo a double type. (don''t forget to disable fastscan for double in '
    +'this case) Just ignore the fact that it looks different because it has a "." in the value. You sure you want to quit?';
  rsLOSER = 'BOO';

  rsTutorialStep4=
    'In the previous tutorial we used bytes to scan, but some games store information in so called ''floating point'' notations. '+#13#10+
    '(probably to prevent simple memory scanners from finding it the easy way)'+#13#10+
    'a floating point is a value with some digits behind the point. (like 5.12 or 11321.1)'+#13#10+
    ''+#13#10+
    'Below you see your health and ammo. Both are stored as Floating point notations, but health is stored as a float and '+
    'ammo is stored as a double.'+#13#10+
    'Click on hit me to lose some health, and on shoot to decrease your ammo with 0.5'+#13#10+
    ' '+#13#10+
    'You have to set BOTH values to 5000 or higher to proceed.'+#13#10+
    ''+#13#10+
    'Exact value scan will work fine here, but you may want to experiment with other types too.'+#13#10+
    ''+#13#10+
    ''+#13#10+
    ''+#13#10+
    ''+#13#10+
    ''+#13#10+
    ''+#13#10+
    ''+#13#10+
    ''+#13#10+
    ''+#13#10+
    ''+#13#10+
    ''+#13#10+
    ''+#13#10+
    ''+#13#10+
    ''+#13#10+
    'Hint: It is recommended to disable "Fast Scan" for type double';


procedure TForm5.Button2Click(Sender: TObject);
begin
  hide;
  form6:=tform6.create(self);
  form6.width:=width;
  form6.height:=height;
  form6.left:=left;
  form6.top:=top;
  form6.show;
end;

procedure TForm5.FormCreate(Sender: TObject);
begin
  health:=100.0;
  ammo:=100.0;
  memo1.lines.text:=rsTutorialStep4;
  memo1.Lines.Insert(0, rsStep4FloatingPointsPW+inttostr(890124)+')');
  memo1.SelStart:=0;
  font.size:=12;
  frmHelp.attach(self,'4');
end;

procedure TForm5.Button1Click(Sender: TObject);
var temp: double;
    i: integer;
begin
  i:=1+random(10);
  if i=0 then i:=1;
  temp:=0.5;
  ammo:=ammo-temp;
  label1.caption:=FloatToStrF(ammo,ffGeneral,4,4);
  application.ProcessMessages;

  if ammo<=0 then
  begin
    showmessage(Format(rsOutOfAmmo, [#13#10]));
    ammo:=100;
    label1.caption:=FloatToStrF(ammo,ffGeneral,4,4);
  end;
end;

procedure TForm5.Timer1Timer(Sender: TObject);
begin
  if (health>=5000) and (ammo>=5000) then
  begin
    button2.Enabled:=true;
    timer1.enabled:=false;
  end;
end;

procedure TForm5.Button3Click(Sender: TObject);
var temp: single;
    i,j: integer;
begin
  i:=1+random(10);
  if i=0 then i:=1;
  temp:=random(4)+(1+random(10)/i);
  health:=health-temp;


  label4.caption:=FloatToStrF(health,ffGeneral,4,4);
  if health<=0 then
  begin
    showmessage(Format(rsDead, [#13#10]));
    health:=4000;
    label4.caption:=FloatToStrF(health,ffGeneral,4,4);
  end;
end;

procedure TForm5.FormClose(Sender: TObject; var Action: TCloseAction);
begin
application.Terminate;
end;

procedure TForm5.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  canclose:=MessageDlg(rsConfirmClose5, mtconfirmation, [mbyes, mbno], 0)=mryes;
end;

procedure TForm5.SpeedButton1Click(Sender: TObject);
begin
  showmessage(rsLOSER);
  button2.Click;
end;

initialization
  {$i Unit5.lrs}
  {$i Unit5.lrs}

end.
