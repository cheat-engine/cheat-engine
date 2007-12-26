unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls;

type
  TForm3 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Timer1: TTimer;
    Timer2: TTimer;
    ProgressBar1: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    health: integer;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses Unit4, Unit5;

{$R *.dfm}

procedure TForm3.FormCreate(Sender: TObject);
begin
  health:=random(500);
  progressbar1.Min:=-2000;
  progressbar1.Max:=-2000+health;
  progressbar1.Position:=-2000+health;

  memo1.Lines.Insert(0,'Step 3: Unknown initial value (PW='+inttostr(419482)+')');
  memo1.SelStart:=0;
end;

procedure TForm3.Button2Click(Sender: TObject);
var loss: integer;
begin
  loss:=1+random(10);
  dec(health,loss);
  progressbar1.Position:=-2000+health;

  if health<0 then
  begin
    showmessage('Seems you''ve done it again! Let me get a replacement! (And restart your scan!)');
    health:=random(500);
    progressbar1.Min:=-2000;
    progressbar1.Max:=-2000+health;
    progressbar1.Position:=-2000+health;
  end else
  begin
    timer2.Enabled:=false;
    timer2.Interval:=1000;
    timer2.enabled:=true;
    label1.Visible:=true;
    label1.Caption:='-'+IntToStr(loss);
  end;
end;

procedure TForm3.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  application.Terminate;
end;

procedure TForm3.Timer1Timer(Sender: TObject);
begin
  if health=5000 then
  begin
    timer1.enabled:=false;
    button1.Enabled:=true;
  end;
end;

procedure TForm3.Timer2Timer(Sender: TObject);
begin
  timer2.enabled:=false;
  label1.visible:=false;
end;

procedure TForm3.Button1Click(Sender: TObject);
begin
  hide;
  form5:=tform5.create(self);
  form5.show;
end;

procedure TForm3.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  canclose:=MessageDlg('Step 3 isn''t really that hard. Just do a new scan, unkown initial value and then decreased value till you find it. Almost everyone gets past this one. Sure you want to quit?',mtconfirmation,[mbyes,mbno],0)=mryes;
end;

end.
