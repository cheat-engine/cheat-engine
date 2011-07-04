unit Exampletrainerstyle3Unit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, ExtCtrls;

type
  TExampleTrainerStyle3 = class(TForm)
    CheckListBox1: TCheckListBox;
    Label1: TLabel;
    Image2: TImage;
    Image1: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    procedure FormShow(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure Image3MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Image4MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Image5MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Image6MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }

  end;

var
  ExampleTrainerStyle3: TExampleTrainerStyle3;

implementation

{$R *.dfm}


procedure TExampleTrainerStyle3.FormShow(Sender: TObject);
begin
  image2.Picture.Bitmap.TransparentColor:=$FFFFFF;
end;

procedure TExampleTrainerStyle3.Image2Click(Sender: TObject);
begin
  Exampletrainerstyle3.Close;
end;

procedure TExampleTrainerStyle3.Image3MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) then
  begin
      ReleaseCapture;
      SendMessage(Handle,WM_NCLBUTTONDOWN,HTCAPTION,0);
  end;
end;

procedure TExampleTrainerStyle3.Image4MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) then
  begin
      ReleaseCapture;
      SendMessage(Handle,WM_NCLBUTTONDOWN,HTCAPTION,0);
  end;
end;

procedure TExampleTrainerStyle3.Image5MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) then
  begin
      ReleaseCapture;
      SendMessage(Handle,WM_NCLBUTTONDOWN,HTCAPTION,0);
  end;
end;

procedure TExampleTrainerStyle3.Image6MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) then
  begin
      ReleaseCapture;
      SendMessage(Handle,WM_NCLBUTTONDOWN,HTCAPTION,0);
  end;
end;

end.
