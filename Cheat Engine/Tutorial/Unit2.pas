unit Unit2;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, LResources;

type

  { TForm2 }

  TForm2 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Button2: TButton;
    Timer1: TTimer;
    SpeedButton1: TSpeedButton;
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
    health: integer;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses Unit3, frmHelpUnit;

resourcestring
  rsAwYouReDeathLetMeReviveYou = 'Aw, you''re dead! Let me revive you';
  rsStep2ExactValueScanningPW = 'Step 2: Exact Value scanning (PW=';
  rsQuittingOnStep2ThisIsTheEasiestStepThereIsFindHeal = 'Quitting on step2? This is the easiest step there is. Find health, change health, done.... Sure you '
    +'want to quit?';
  rsLOSER = 'BOO';

  rsTutorialStep2=
    'Now that you have opened the tutorial with Cheat Engine let''s get on with the next step.'+#13#10+
    ''+#13#10+
    'You can see at the bottom of this window is the text Health: xxx'+#13#10+
    'Each time you click ''Hit me''  your health gets decreased.'+#13#10+
    ''+#13#10+
    'To get to the next step you have to find this value and change it to 1000'+#13#10+
    ''+#13#10+
    'To find the value there are different ways, but I''ll tell you about the easiest, ''Exact Value'':'+#13#10+
    'First make sure value type is set to at least 2-bytes or 4-bytes. 1-byte will also work, but you''ll run into an easy to fix'+
    ' problem when you''ve found the address and want to change it. The 8-byte may perhaps works if the'+
    ' bytes after the address are 0, but I wouldn''t take the bet.'+#13#10+
    'Single, double, and the other scans just don''t work, because they store the value in a different way.'+#13#10+
    ''+#13#10+
    'When the value type is set correctly, make sure the scantype is set to ''Exact Value'''+#13#10+
    'Then fill in the number your health is in the value box. And click ''First Scan'''+#13#10+
    'After a while (if you have a extremely slow pc) the scan is done and the results are shown in the list on the'+
    ' left'+#13#10+
    ''+#13#10+
    'If you find more than 1 address and you don''t know for sure which address it is, click ''Hit me'', fill in the new'+
    ' health value into the value box, and click ''Next Scan'''+#13#10+
    'repeat this until you''re sure you''ve found it. (that includes that there''s only 1 address in the list.....)'+#13#10+
    ''+#13#10+
    'Now double click the address in the list on the left. This makes the address pop-up in the list at the bottom,'+
    ' showing you the current value.'+#13#10+
    'Double click the value, (or select it and press enter), and change the value to 1000.'+#13#10+
    ''+#13#10+
    'If everything went ok the next button should become enabled, and you''re ready for the next step.'+#13#10+
    ''+#13#10+
    ''+#13#10+
    'Note:'+#13#10+
    'If you did anything wrong while scanning, click "New Scan" and repeat the scanning again.'+#13#10+
    'Also, try playing around with the value and click ''hit me''';



type TTester=class(TThread)
    procedure execute; override;
  end;


procedure TTester.execute;
begin
  while form2.health<>666 do
  asm
    nop
    nop
    nop
    nop
    nop
  end;
end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  application.Terminate;
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  TTester.create(false);
  TTester.create(false);
end;

procedure TForm2.Button2Click(Sender: TObject);

begin
  health:=health-(1+random(5));
  label1.Caption:=inttostr(health);
  if health<0 then
  begin
    showmessage(rsAwYouReDeathLetMeReviveYou);
    health:=100;
    label1.Caption:=inttostr(health);
  end;

end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  memo1.lines.text:=rsTutorialStep2;
  memo1.Lines.Insert(0, rsStep2ExactValueScanningPW+inttostr(0)+inttostr(90453)+')');
  memo1.SelStart:=0;
  health:=100;

  font.size:=12;
  frmHelp.attach(self,'2');
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  if health=1000 then
  begin
    button1.Enabled:=true;
    timer1.enabled:=false;
  end;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  hide;
  form3:=TForm3.create(self);
  form3.width:=width;
  form3.height:=height;
  form3.left:=left;
  form3.top:=top;
  form3.show;
end;

procedure TForm2.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  canclose:=MessageDlg(rsQuittingOnStep2ThisIsTheEasiestStepThereIsFindHeal, mtconfirmation, [mbyes, mbno], 0)=mryes;
end;

procedure TForm2.SpeedButton1Click(Sender: TObject);
begin
  showmessage(rsLOSER);
  button1.Click;
end;

initialization
  {$i Unit2.lrs}
  {$i Unit2.lrs}

end.
