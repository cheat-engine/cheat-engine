unit tester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    x: integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  label1.caption:=inttohex(dword(@x),8);
  loadlibrary('..\pscan.dll');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  x:=random(100000);
  label2.caption:=inttostr(x);
end;

end.
