{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynAutoCorrectEditor.pas, released 2001-10-05.
Author of this file is Aaron Chan. All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynAutoCorrectEditor.pas,v 1.9 2003/04/30 14:20:08 etrusco Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{$IFNDEF QSYNAUTOCORRECTEDITOR}
unit SynAutoCorrectEditor;
{$ENDIF}

interface

{$I SynEdit.inc}

uses
{$IFDEF SYN_CLX}  //js 06-04-2002
  QGraphics, QControls, QForms, QDialogs, QExtCtrls, QStdCtrls, QButtons, Types,
  QSynAutoCorrect,
{$ELSE}
  Windows,  Messages, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls,
  Buttons, Registry,
  SynAutoCorrect,
{$ENDIF}
  SysUtils,
  Classes;

type
  TfrmAutoCorrectEditor = class(TForm)
    lblLabel1: TLabel;
    lblLabel2: TLabel;
    lbxItems: TListBox;
    btnAdd: TSpeedButton;
    btnDelete: TSpeedButton;
    btnClear: TSpeedButton;
    btnEdit: TSpeedButton;
    btnDone: TSpeedButton;
    bvlSeparator: TBevel;
    procedure FormShow(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnDoneClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure lbxItemsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    procedure lbxItemsDrawItemCLX(Sender: TObject; Index: Integer;
      Rect: TRect; State: TOwnerDrawState; var Handled: Boolean);
{$IFNDEF SYN_CLX}
    procedure lbxItemsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
{$ENDIF}
  public
    SynAutoCorrect: TSynAutoCorrect;
  end;

const
  SConfirmation = 'Confirmation';
  SError = 'Error';
  SOriginal = 'Original:';
  SCorrection = 'Correction:';
  SAdd = 'Add...';
  SEdit = 'Edit...';
  SPleaseSelectItem = 'Please select an item before executing this command!';
  SClearListConfirmation = 'Are you sure you want to clear the entire list?';

implementation

{$R *.dfm}

procedure TfrmAutoCorrectEditor.FormShow(Sender: TObject);
begin
  lbxItems.Items.Assign(SynAutoCorrect.Items);
  Invalidate;
end;

procedure TfrmAutoCorrectEditor.lbxItemsDrawItemCLX(Sender: TObject;
  Index: Integer; Rect: TRect; State: TOwnerDrawState;
  var Handled: Boolean);
var
  s: string;
begin
  with lbxItems do
  begin
    s := Items[Index];
    with Canvas do
    begin
      FillRect(Rect);

      TextOut(Rect.Left + 2, Rect.Top, SynAutoCorrect.HalfString(s, True));
      TextOut(Rect.Left + (lbxItems.ClientWidth div 2) + 2, Rect.Top,
        SynAutoCorrect.HalfString(s, False));
      FormPaint(nil);
    end;
  end;
end;

{$IFNDEF SYN_CLX}
procedure TfrmAutoCorrectEditor.lbxItemsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Dummy: boolean;
begin
  Dummy := True;
  lbxItemsDrawItemCLX( Control, Index, Rect, State, Dummy );
end;
{$ENDIF}

procedure TfrmAutoCorrectEditor.btnAddClick(Sender: TObject);
var
  Original, Correction: string;

begin
  if InputQuery(SAdd, SOriginal, Original) then
    InputQuery(SAdd, SCorrection, Correction)
  else
    Exit;

  with SynAutoCorrect do
  begin
    if (Original <> '') and (Correction <> '') then
    begin
      Add(Original, Correction);
      lbxItems.Items.Assign(SynAutoCorrect.Items);
    end;
  end;

  btnDelete.Enabled := lbxItems.ItemIndex > -1;
  btnEdit.Enabled := lbxItems.ItemIndex > -1;
end;

procedure TfrmAutoCorrectEditor.btnDeleteClick(Sender: TObject);
begin
  if lbxItems.ItemIndex < 0 then
  begin
  {$IFDEF SYN_CLX}
    ShowMessage(SPleaseSelectItem);
  {$ELSE} //js 06-04-2002 no messagebox in clx
    MessageBox(0, SPleaseSelectItem, SError, MB_ICONERROR or MB_OK);
  {$ENDIF}

    Exit;
  end;

  SynAutoCorrect.Delete(lbxItems.ItemIndex);
  lbxItems.Items.Assign(SynAutoCorrect.Items);

  btnDelete.Enabled := lbxItems.ItemIndex > -1;
  btnEdit.Enabled := lbxItems.ItemIndex > -1;
end;

procedure TfrmAutoCorrectEditor.btnEditClick(Sender: TObject);
var
  Original, Correction, CurrText: String;

begin
  if lbxItems.ItemIndex < 0 then
  begin
  {$IFDEF SYN_CLX}
    ShowMessage(SPleaseSelectItem);
  {$ELSE} //js 06-04-2002 no messagebox in clx
    MessageBox(0, SPleaseSelectItem, SError, MB_ICONERROR or MB_OK);
  {$ENDIF}
    Exit;
  end;

  with SynAutoCorrect do
  begin
    CurrText := SynAutoCorrect.Items[lbxItems.ItemIndex];
    Original := SynAutoCorrect.HalfString(CurrText, True);
    Correction := SynAutoCorrect.HalfString(CurrText, False);

    if InputQuery(SEdit, SOriginal, Original) then
      InputQuery(SEdit, SCorrection, Correction)
    else
      Exit;

    Edit(lbxItems.ItemIndex, Original, Correction);
    lbxItems.Items.Assign(SynAutoCorrect.Items);
  end;

  btnDelete.Enabled := lbxItems.ItemIndex > -1;
  btnEdit.Enabled := lbxItems.ItemIndex > -1;
end;

procedure TfrmAutoCorrectEditor.btnDoneClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmAutoCorrectEditor.btnClearClick(Sender: TObject);
begin
{$IFNDEF SYN_CLX} //js 06-04-2002
  if MessageBox(0, SClearListConfirmation, SConfirmation,
    MB_YESNO or MB_ICONQUESTION) <> IDYES then Exit;
{$ENDIF}
  SynAutoCorrect.Items.Clear;
  lbxItems.Items.Clear;

  btnDelete.Enabled := lbxItems.ItemIndex > -1;
  btnEdit.Enabled := lbxItems.ItemIndex > -1;
end;

procedure TfrmAutoCorrectEditor.lbxItemsClick(Sender: TObject);
begin
  btnDelete.Enabled := lbxItems.ItemIndex > -1;
  btnEdit.Enabled := lbxItems.ItemIndex > -1;
end;

procedure TfrmAutoCorrectEditor.FormCreate(Sender: TObject);
begin
  ClientWidth := 521;
  ClientHeight := 377;
{$IFDEF SYN_CLX}
  lbxItems.OnDrawItem := lbxItemsDrawItemCLX;
  BorderStyle := fbsSingle;
{$ELSE}
  lbxItems.OnDrawItem := lbxItemsDrawItem;
  BorderStyle := bsSingle;
{$ENDIF}
end;

procedure TfrmAutoCorrectEditor.FormPaint(Sender: TObject);
begin
  { Paints the line in the middle of the listbox. }
  with lbxItems.Canvas do
  begin
    Pen.Color := clBlack;
    PenPos := Point(lbxItems.Width div 2 - 8, 0);
    LineTo(lbxItems.Width div 2 - 8, lbxItems.Height);
  end;
end;

end.
