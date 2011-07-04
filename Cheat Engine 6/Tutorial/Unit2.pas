unit Unit2;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, LResources;

type

  { TForm2 }

  TForm2 = class(TForm)
    Button3: TButton;
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

uses Unit3;

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
    showmessage('Aw, you''re death! Let me revive you');
    health:=100;
    label1.Caption:=inttostr(health);
  end;

end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  memo1.Lines.Insert(0,'Step 2: Exact Value scanning (PW='+inttostr(0)+inttostr(90453)+')');
  memo1.SelStart:=0;
  health:=100;
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
  form3.show;
end;

procedure TForm2.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  canclose:=MessageDlg('Quitting on step2? This is the easiest step there is. Find health, change health, done.... Sure you want to quit?',mtconfirmation,[mbyes,mbno],0)=mryes;
end;

procedure TForm2.SpeedButton1Click(Sender: TObject);
begin
  showmessage('LOSER');
  button1.Click;
end;

initialization
  {$i Unit2.lrs}
  {$i Unit2.lrs}

end.
