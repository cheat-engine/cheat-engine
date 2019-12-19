unit frmD3DTrainerGeneratorOptionsUnit;

{$mode delphi}

interface

uses
  {$ifdef windows}
  windows,
  {$endif}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, ExtDlgs, cefuncproc, commonTypeDefs;

type

  { TfrmD3DTrainerGeneratorOptions }

  TfrmD3DTrainerGeneratorOptions = class(TForm)
    btnClear: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    cbAllowDrag: TCheckBox;
    cbHasCheckbox: TCheckBox;
    cbShowHotkeys: TCheckBox;
    cbStretch: TCheckBox;
    cbUseD3DKeys: TCheckBox;
    ColorDialog1: TColorDialog;
    edtd3dkeys: TEdit;
    edtDistanceBetweenLines: TEdit;
    edtDistanceFromBorder: TEdit;
    edtDistanceFromTop: TEdit;
    FontDialog1: TFontDialog;
    imgChecked: TImage;
    imgPreview: TImage;
    imgUnchecked: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblTextColor: TLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlPosition: TPanel;
    rbBottomLeft: TRadioButton;
    rbBottomRight: TRadioButton;
    rbCenter: TRadioButton;
    rbTopLeft: TRadioButton;
    rbTopRight: TRadioButton;
    TextOverlayImage: TImage;
    TrackBar1: TTrackBar;
    procedure btnClearClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure cbStretchChange(Sender: TObject);
    procedure cbUseD3DKeysChange(Sender: TObject);
    procedure edtd3dkeysKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    procedure updatefontexample;
  public
    { public declarations }
    d3dkeys: TKeycombo;

  end;

var
  frmD3DTrainerGeneratorOptions: TfrmD3DTrainerGeneratorOptions;

implementation

{$R *.lfm}

{ TfrmD3DTrainerGeneratorOptions }

procedure TfrmD3DTrainerGeneratorOptions.FormCreate(Sender: TObject);
begin
  imgPreview.Picture.Bitmap.Canvas.Brush.color:=clWhite;
  imgPreview.Picture.Bitmap.width:=imgPreview.width;
  imgPreview.Picture.Bitmap.Height:=imgPreview.height;
  imgPreview.Picture.Bitmap.Canvas.Pixels[0,0]:=imgPreview.Picture.Bitmap.Canvas.Pixels[0,0];

  updatefontexample;
end;

procedure TfrmD3DTrainerGeneratorOptions.FormShow(Sender: TObject);
begin
  lblTextColor.Font.Height:=GetFontData(font.handle).height;

  {$ifdef darwin}
  close
  {$endif}
end;

procedure TfrmD3DTrainerGeneratorOptions.Button4Click(Sender: TObject);
begin
  close;
end;

procedure TfrmD3DTrainerGeneratorOptions.cbStretchChange(Sender: TObject);
begin
  imgPreview.stretch:=cbStretch.checked;
end;

procedure TfrmD3DTrainerGeneratorOptions.cbUseD3DKeysChange(Sender: TObject);
begin
  edtd3dkeys.enabled:=cbUseD3DKeys.checked;
  btnClear.enabled:=cbUseD3DKeys.checked;

end;

procedure TfrmD3DTrainerGeneratorOptions.edtd3dkeysKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var i: integer;
begin
  if d3dkeys[4]=0 then
  begin
    for i:=0 to 4 do
      if d3dkeys[i]=0 then
      begin
        d3dkeys[i]:=key;
        break;
      end else
      if d3dkeys[i]=key then break;
  end;

  edtd3dkeys.Text:=ConvertKeyComboToString(d3dkeys);

  key:=0;
end;

procedure TfrmD3DTrainerGeneratorOptions.btnClearClick(Sender: TObject);
begin

  {$IFDEF windows}
  zeromemory(@d3dkeys,sizeof(TKeyCombo));
  edtd3dkeys.Text:=ConvertKeyComboToString(d3dkeys);
  edtd3dkeys.SetFocus;
  {$ENDIF}
end;

procedure TfrmD3DTrainerGeneratorOptions.Button2Click(Sender: TObject);
begin
  if OpenPictureDialog1.execute then
    imgPreview.Picture.LoadFromFile(OpenPictureDialog1.FileName);
end;

procedure TfrmD3DTrainerGeneratorOptions.updatefontexample;
begin
  TextOverlayImage.Picture.Bitmap.Width:=0;
  TextOverlayImage.Picture.Bitmap.Height:=0;

  TextOverlayImage.Picture.Bitmap.TransparentColor:=$010101;
  TextOverlayImage.Picture.Bitmap.Transparent:=true;
  TextOverlayImage.Picture.Bitmap.Canvas.Brush.Color:=TextOverlayImage.Picture.Bitmap.TransparentColor;
  TextOverlayImage.Picture.Bitmap.Width:=TextOverlayImage.Width;
  TextOverlayImage.Picture.Bitmap.Height:=TextOverlayImage.Height;
  TextOverlayImage.Picture.Bitmap.Canvas.FillRect(0,0,TextOverlayImage.Picture.Bitmap.Width, TextOverlayImage.Picture.Bitmap.Height);
  TextOverlayImage.Picture.Bitmap.Canvas.font:=lblTextColor.font;
  TextOverlayImage.Picture.Bitmap.Canvas.font.Quality:=fqNonAntialiased;

  TextOverlayImage.picture.Bitmap.Canvas.TextOut(0,0,'Example text');
end;

procedure TfrmD3DTrainerGeneratorOptions.Button3Click(Sender: TObject);
begin
  fontdialog1.font.assign(lblTextColor.Font);
  if fontdialog1.execute then
  begin
    lblTextColor.font.assign(fontdialog1.font);
    lblTextColor.font.Quality:=fqNonAntialiased;
  end;


  updatefontexample;

end;

end.

