unit Unit5;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls,math;

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
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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

uses Unit4, Unit6;

{$R *.dfm}

procedure TForm5.Button2Click(Sender: TObject);
begin
  hide;
  form6:=tform6.create(self);
  form6.show;
end;

procedure TForm5.FormCreate(Sender: TObject);
begin
  health:=100.0;
  ammo:=100.0;
  memo1.Lines.Insert(0,'Step 4: Floating points (PW='+inttostr(890124)+')');
  memo1.SelStart:=0;
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
    showmessage('Out of ammo!'#13#10'Press ok to stock up on some ammo');
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
    showmessage('I think you''re death!'#13#10'Press ok to become a brain eating zombie');
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
  canclose:=MessageDlg('Come on. This step is simple. For health do a float scan, and for ammo a double type. (don''t forget to disable fastscan for double in this case) Just ignore the fact that it looks different because it has a "." in the value. You sure you want to quit?',mtconfirmation,[mbyes,mbno],0)=mryes;
end;

end.
