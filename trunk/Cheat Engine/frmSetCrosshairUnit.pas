unit frmSetCrosshairUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls, ExtDlgs, ComCtrls;

type

  { TfrmSetCrosshair }

  TfrmSetCrosshair = class(TForm)
    btnApply: TButton;
    Image1: TImage;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    TrackBar1: TTrackBar;
    procedure btnApplyClick(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { private declarations }
    overlayid: integer;
  public
    { public declarations }
  end; 

var
  frmSetCrosshair: TfrmSetCrosshair;

implementation

uses d3dhookUnit;

{ TfrmSetCrosshair }

procedure TfrmSetCrosshair.MenuItem2Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
    image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);



end;

procedure TfrmSetCrosshair.Panel1Resize(Sender: TObject);
begin
  btnApply.left:=(panel3.ClientWidth div 2)- (btnapply.Width div 2);
  trackbar1.Left:=label1.Left+label1.Width+3;
  trackbar1.Width:=panel2.clientwidth-trackbar1.Left-4;
end;

procedure TfrmSetCrosshair.TrackBar1Change(Sender: TObject);
begin
  safed3dhook;

  if (d3dhook<>nil) and (overlayid<>0) then
    d3dhook.SetOverlayAlphaBlend(overlayid, trackbar1.position );
end;

procedure TfrmSetCrosshair.btnApplyClick(Sender: TObject);
begin
  safed3dhook;
  if d3dhook<>nil then
  begin
    if overlayid=0 then
      overlayid:=D3DHook.createOverlayFromPicture(image1.Picture, -1,-1)
    else
      D3DHook.updateOverlayImage(overlayid);
  end;

  TrackBar1Change(trackbar1);
end;

{$R *.lfm}

end.

