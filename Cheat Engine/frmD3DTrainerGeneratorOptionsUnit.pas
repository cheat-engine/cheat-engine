unit frmD3DTrainerGeneratorOptionsUnit;

{$mode delphi}

interface

uses
  windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, ExtDlgs, cefuncproc;

type

  { TfrmD3DTrainerGeneratorOptions }

  TfrmD3DTrainerGeneratorOptions = class(TForm)
    btnClear: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    cbUseD3DKeys: TCheckBox;
    cbShowHotkeys: TCheckBox;
    cbStretch: TCheckBox;
    cbHasCheckbox: TCheckBox;
    cbAllowDrag: TCheckBox;
    ColorDialog1: TColorDialog;
    edtd3dkeys: TEdit;
    ImageList1: TImageList;
    imgPreview: TImage;
    Label1: TLabel;
    Label2: TLabel;
    lblTextColor: TLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    rbTopLeft: TRadioButton;
    rbBottomLeft: TRadioButton;
    rbTopRight: TRadioButton;
    rbBottomRight: TRadioButton;
    rbCenter: TRadioButton;
    TrackBar1: TTrackBar;
    procedure btnClearClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure cbUseD3DKeysChange(Sender: TObject);
    procedure edtd3dkeysKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
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
end;

procedure TfrmD3DTrainerGeneratorOptions.Button4Click(Sender: TObject);
begin
  close;
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
  zeromemory(@d3dkeys,sizeof(TKeyCombo));
  edtd3dkeys.Text:=ConvertKeyComboToString(d3dkeys);
  edtd3dkeys.SetFocus;
end;

procedure TfrmD3DTrainerGeneratorOptions.Button2Click(Sender: TObject);
begin
  if OpenPictureDialog1.execute then
    imgPreview.Picture.LoadFromFile(OpenPictureDialog1.FileName);
end;

procedure TfrmD3DTrainerGeneratorOptions.Button3Click(Sender: TObject);
begin
  if colordialog1.execute then
    lblTextColor.font.color:=ColorDialog1.Color;

end;

end.

