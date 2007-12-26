unit Unit8;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm8 = class(TForm)
    Memo1: TMemo;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    health: integer;
  public
    { Public declarations }
  end;

var
  Form8: TForm8;

implementation

uses Unit4, Unit9;

{$R *.dfm}

procedure TForm8.Button1Click(Sender: TObject);
var oldhealth: dword;
begin
  oldhealth:=health;
  dec(health);
  label1.Caption:=inttostr(health);

  if health=oldhealth+2 then button2.Enabled:=true;

  if health<0 then
  begin
    showmessage('Aw, you''re death! Let me revive you');
    health:=100;
    label1.Caption:=inttostr(health);
  end;
end;

procedure TForm8.FormCreate(Sender: TObject);
begin
  health:=100;


  memo1.Lines.Insert(0,'Step 7: Code Injection: (PW='+inttostr(0)+inttostr(13370)+')');
  memo1.SelStart:=0;
end;

procedure TForm8.Button2Click(Sender: TObject);
begin
  hide;
  form9:=tform9.create(self);
  form9.show;
end;

procedure TForm8.FormClose(Sender: TObject; var Action: TCloseAction);
begin
application.Terminate;
end;

procedure TForm8.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  canclose:=MessageDlg('Code injections too tough? No problem, memory scanning and basic pointers should be enough to get you experienced enough and you can always try the tutorial later. Are you sure you want to quit?',mtconfirmation,[mbyes,mbno],0)=mryes;
end;

end.
