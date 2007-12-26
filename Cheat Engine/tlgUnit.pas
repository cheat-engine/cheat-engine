unit tlgUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  Ttlg = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Timer1: TTimer;
    Label2: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    score: integer;
  end;

var
  tlg: Ttlg;

implementation

{$R *.dfm}

procedure Ttlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  action:=cafree;
end;

procedure Ttlg.Timer1Timer(Sender: TObject);
begin
  image1.Left:=random(width-image1.Width);
  image1.Top:=random(height-image1.Height);
  if not image1.Visible then image1.Visible:=true;
end;

procedure Ttlg.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inc(score);
  label2.Caption:=IntToStr(score);

  image1.Left:=random(width-image1.Width);
  image1.Top:=random(height-image1.Height);
  timer1.Enabled:=false;
  timer1.Interval:=timer1.Interval-100;

  if timer1.Interval<=0 then showmessage('OMG, You must have cheated or don''t have a life. (Although some people claim that thats the same)');
  timer1.Enabled:=true;
end;

end.
