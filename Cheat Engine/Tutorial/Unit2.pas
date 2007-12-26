unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Button2: TButton;
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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

{$R *.dfm}

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  application.Terminate;
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

end.
