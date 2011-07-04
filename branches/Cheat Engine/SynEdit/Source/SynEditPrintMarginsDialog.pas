{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditPrintMarginsDialog.pas, released 2000-06-01.

The Initial Author of the Original Code is Morten J. Skovrup.
Portions written by Morten J. Skovrup are copyright 2000 Morten J. Skovrup.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynEditPrintMarginsDialog.pas,v 1.5 2003/04/30 12:59:48 etrusco Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------
CONTENTS:
  Property editor for TSynEditPrintMargins - nothing fancy, it only displays
  a picture that can help with understanding the different values.
-------------------------------------------------------------------------------}

{$IFNDEF QSYNEDITPRINTMARGINSDIALOG}
unit SynEditPrintMarginsDialog;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  Qt,
  QGraphics,
  QForms,
  QControls,
  QStdCtrls,
  QButtons,
  QExtCtrls,
  QDialogs,
  QSynEditPrint,
  QSynEditPrintTypes,
  QSynEditPrintMargins,
{$ELSE}
  Windows,
  Graphics,
  Forms,
  Controls,
  StdCtrls,
  Buttons,
  ExtCtrls,
  Dialogs,
  SynEditPrint,
  SynEditPrintTypes,
  SynEditPrintMargins,
{$ENDIF}
  SysUtils,
  Classes;

type
  TSynEditPrintMarginsDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    CBMirrorMargins: TCheckBox;
    Label10: TLabel;
    Label11: TLabel;
    EditLeft: TEdit;
    EditRight: TEdit;
    EditTop: TEdit;
    EditBottom: TEdit;
    EditGutter: TEdit;
    EditHeader: TEdit;
    EditFooter: TEdit;
    EditHFInternalMargin: TEdit;
    EditLeftHFTextIndent: TEdit;
    EditRightHFTextIndent: TEdit;
    CBUnits: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CBUnitsChange(Sender: TObject);
  private
    { Private declarations }
    FMargins: TSynEditPrintMargins;
    FInternalCall: Boolean;
  public
    { Public declarations }
    procedure SetMargins(SynEditMargins: TSynEditPrintMargins);
    procedure GetMargins(SynEditMargins: TSynEditPrintMargins);
  end;

implementation

{$R *.dfm}

{ TSynEditPrintMarginsDlg }

procedure TSynEditPrintMarginsDlg.FormCreate(Sender: TObject);
begin
  FMargins := TSynEditPrintMargins.Create;
  FInternalCall := False;
end;

procedure TSynEditPrintMarginsDlg.FormDestroy(Sender: TObject);
begin
  FMargins.Free;
end;

procedure TSynEditPrintMarginsDlg.GetMargins(
  SynEditMargins: TSynEditPrintMargins);
var
  CurEdit: TEdit;
  function StringToFloat(Edit: TEdit): Double;
  begin
    CurEdit := Edit;
    Result := StrToFloat(Edit.Text);
  end;
begin
  with SynEditMargins do begin
    if not FInternalCall then
      UnitSystem := TUnitSystem(CBUnits.ItemIndex);
    try
      Left := StringToFloat(EditLeft);
      Right := StringToFloat(EditRight);
      Top := StringToFloat(EditTop);
      Bottom := StringToFloat(EditBottom);
      Gutter := StringToFloat(EditGutter);
      Header := StringToFloat(EditHeader);
      Footer := StringToFloat(EditFooter);
      LeftHFTextIndent := StringToFloat(EditLeftHFTextIndent);
      RightHFTextIndent := StringToFloat(EditRightHFTextIndent);
      HFInternalMargin := StringToFloat(EditHFInternalMargin);
    except
      MessageDlg('Invalid number!', mtError, [mbOk], 0);
      CurEdit.SetFocus;
    end;
    MirrorMargins := CBMirrorMargins.Checked;
  end;
end;

procedure TSynEditPrintMarginsDlg.SetMargins(
  SynEditMargins: TSynEditPrintMargins);
begin
  with SynEditMargins do begin
    CBUnits.ItemIndex := Ord(UnitSystem);
    EditLeft.Text := FloatToStr(Left);
    EditRight.Text := FloatToStr(Right);
    EditTop.Text := FloatToStr(Top);
    EditBottom.Text := FloatToStr(Bottom);
    EditGutter.Text := FloatToStr(Gutter);
    EditHeader.Text := FloatToStr(Header);
    EditFooter.Text := FloatToStr(Footer);
    EditLeftHFTextIndent.Text := FloatToStr(LeftHFTextIndent);
    EditRightHFTextIndent.Text := FloatToStr(RightHFTextIndent);
    EditHFInternalMargin.Text := FloatToStr(HFInternalMargin);
    CBMirrorMargins.Checked := MirrorMargins;
  end;
end;

procedure TSynEditPrintMarginsDlg.CBUnitsChange(Sender: TObject);
begin
  FInternalCall := True;
  GetMargins(FMargins);
  FInternalCall := False;
  FMargins.UnitSystem := TUnitSystem(CBUnits.ItemIndex);
  SetMargins(FMargins);
end;

end.

