unit frmSetCrosshairUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls, ExtDlgs, ComCtrls, d3dhookUnit;

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
    crosshairTexture: TD3DHook_Texture;
    crosshairSprite: TD3dhook_Sprite;
  public
    { public declarations }
  end; 

var
  frmSetCrosshair: TfrmSetCrosshair;

implementation



{ TfrmSetCrosshair }

procedure TfrmSetCrosshair.MenuItem2Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    btnApply.enabled:=true;
  end;
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
  {$ifndef D3DDebug}
  {$error You forgot to reimplement this}
  {$endif}

  if (d3dhook<>nil) and (crosshairSprite<>nil) then
    crosshairSprite.alphaBlend:=trackbar1.position / 100;
end;

procedure TfrmSetCrosshair.btnApplyClick(Sender: TObject);
var old: td3dhook;
begin
  old:=d3dhook;

  safed3dhook;
  if d3dhook<>nil then
  begin
    if old<>d3dhook then
    begin
      crosshairtexture:=nil; //don't free. Perhaps the game got terminated due to a freeze in the renderer (lock acquired)
      crosshairSprite:=nil;
    end;


    if crosshairtexture=nil then
    begin
      crosshairtexture:=D3DHook.createTexture(image1.Picture);
      crosshairSprite:=d3dhook.createSprite(crosshairtexture);
      crosshairSprite.beginUpdate;
      crosshairsprite.x:=-1;
      crosshairsprite.y:=-1;
      crosshairsprite.endupdate;

    end
    else
      crosshairtexture.LoadTextureByPicture(image1.Picture);
  end;

  TrackBar1Change(trackbar1);
end;

{$R *.lfm}

end.

