{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditKeyCmdsEditor.pas, released 2000-04-07.
The Original Code is based on the mwKeyCmdsEditor.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Brad Stowers.
All Rights Reserved.

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

$Id: SynEditKeyCmdsEditor.pas,v 1.11 2004/11/25 14:06:27 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{$IFNDEF QSYNEDITKEYCMDSEDITOR}
unit SynEditKeyCmdsEditor;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  Qt,
  QGraphics,
  QControls,
  QForms,
  QDialogs,
  QComCtrls,
  QMenus,
  QStdCtrls,
  QExtCtrls,
  QButtons,
  QSynEditKeyCmds,
{$ELSE}
  Windows,
  Messages,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ComCtrls,
  Menus,
  StdCtrls,
  Buttons,
  ExtCtrls,
  SynEditKeyCmds,
{$ENDIF}
  SysUtils,
  Classes;

type
  TSynEditKeystrokesEditorForm = class(TForm)
    pnlBottom: TPanel;
    lnlInfo: TLabel;
    lnlInfo2: TLabel;
    btnAdd: TButton;
    btnEdit: TButton;
    btnDelete: TButton;
    btnClear: TButton;
    btnReset: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    pnlCommands: TPanel;
    KeyCmdList: TListView;
    procedure FormResize(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure KeyCmdListClick(Sender: TObject);
  private
    FKeystrokes: TSynEditKeystrokes;
    FExtended: Boolean;
    procedure SetKeystrokes(const Value: TSynEditKeyStrokes);
    procedure UpdateKeystrokesList;
    {**************}
    {$IFNDEF SYN_CLX}
    procedure WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo);
      message WM_GETMINMAXINFO;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Keystrokes: TSynEditKeyStrokes read FKeystrokes write SetKeystrokes;
    property ExtendedString: Boolean read FExtended write FExtended;
  end;

implementation

{$R *.dfm}

uses
{$IFDEF SYN_CLX}
  QSynEditKeyCmdEditor,
  QSynEditStrConst;
{$ELSE}
  SynEditKeyCmdEditor,
  SynEditStrConst;
{$ENDIF}

{ TSynEditKeystrokesEditorForm }

constructor TSynEditKeystrokesEditorForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKeystrokes := NIL;
end;

destructor TSynEditKeystrokesEditorForm.Destroy;
begin
  if Assigned(FKeyStrokes) then FKeystrokes.Free;
  inherited Destroy;
end;

procedure TSynEditKeystrokesEditorForm.SetKeystrokes(const Value:
  TSynEditKeyStrokes);
begin
  if FKeystrokes = NIL then
    FKeystrokes := TSynEditKeyStrokes.Create(Self);
  FKeystrokes.Assign(Value);
  UpdateKeystrokesList;
end;

procedure TSynEditKeystrokesEditorForm.UpdateKeystrokesList;
var
  x: integer;
begin
  KeyCmdList.Items.BeginUpdate;
  try
    KeyCmdList.Items.Clear;
    for x := 0 to FKeystrokes.Count-1 do
    begin
      with KeyCmdList.Items.Add do
      begin
        if FExtended then
          Caption := ConvertCodeStringToExtended(EditorCommandToCodeString(FKeystrokes[x].Command))
        else Caption := EditorCommandToCodeString(FKeystrokes[x].Command);
        if FKeystrokes[x].ShortCut = 0 then
          SubItems.Add(SYNS_ShortCutNone)
        else
          if FKeystrokes[x].ShortCut2 = 0 then
          {$IFDEF SYN_CLX}
            SubItems.Add(QMenus.ShortCutToText(FKeystrokes[x].ShortCut))
          {$ELSE}
            SubItems.Add(Menus.ShortCutToText(FKeystrokes[x].ShortCut))
          {$ENDIF}
          else
          {$IFDEF SYN_CLX}
            SubItems.Add(QMenus.ShortCutToText(FKeystrokes[x].ShortCut)+ ' '+
              QMenus.ShortCutToText(FKeystrokes[x].ShortCut2));
          {$ELSE}
            SubItems.Add(Menus.ShortCutToText(FKeystrokes[x].ShortCut)+ ' '+
              Menus.ShortCutToText(FKeystrokes[x].ShortCut2));
          {$ENDIF}
      end;
    end;
  finally
    KeyCmdList.Items.EndUpdate;
  end;
end;

procedure TSynEditKeystrokesEditorForm.FormResize(Sender: TObject);
begin
  pnlBottom.Width := pnlBottom.Left + ClientWidth - 25;
  pnlBottom.Height := ClientHeight - 11;
  pnlCommands.Width := ClientWidth - 136;
  pnlCommands.Height := ClientHeight - 75;

  btnAdd.Left := pnlCommands.Left + pnlCommands.Width + 14;
  btnEdit.Left := pnlCommands.Left + pnlCommands.Width + 14;
  btnDelete.Left := pnlCommands.Left + pnlCommands.Width + 14;
  btnClear.Left := pnlCommands.Left + pnlCommands.Width + 14;
  btnReset.Left := pnlCommands.Left + pnlCommands.Width + 14;

  btnOK.Left := pnlCommands.Left + pnlCommands.Width + 14;
  btnOK.Top := pnlCommands.Top + pnlCommands.Height - 19;
  btnCancel.Left := pnlCommands.Left + pnlCommands.Width + 14;
  btnCancel.Top := pnlCommands.Top + pnlCommands.Height + 13;

  lnlInfo.Top := pnlCommands.Top + pnlCommands.Height + 11;
  lnlInfo2.Top := pnlCommands.Top + pnlCommands.Height + 27;
end;

{***************}
{$IFNDEF SYN_CLX}
procedure TSynEditKeystrokesEditorForm.WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo);
begin
  inherited;
  Msg.MinMaxInfo.ptMinTrackSize := Point(300, 225);
end;
{$ENDIF}

procedure TSynEditKeystrokesEditorForm.btnAddClick(Sender: TObject);            //DDH 10/16/01 Begin (reworked proc)
var
  NewStroke: TSynEditKeyStroke;
  AForm : TSynEditKeystrokeEditorForm;

  Function AddKeyStroke: Boolean;
  VAR KeyLoc : Integer;
      TmpCommand: String;
  begin
    Result := False;
    KeyLoc := 0;
    if AForm.ShowModal = mrOK then
    begin
      Result := True;
      NewStroke := FKeystrokes.Add;
      NewStroke.Command := AForm.Command;
      try
        KeyLoc := TSynEditKeyStrokes(NewStroke.Collection).FindShortcut2(AForm.Keystroke, AForm.Keystroke2);
        NewStroke.ShortCut := AForm.Keystroke;
        NewStroke.ShortCut2 := AForm.Keystroke2;
      except
        on ESynKeyError do
          begin
            // Shortcut already exists in the collection!
            if FExtended then
              TmpCommand := ConvertCodeStringToExtended(EditorCommandToCodeString(TSynEditKeyStrokes(NewStroke.Collection).Items[KeyLoc].Command))
            else TmpCommand := EditorCommandToCodeString(TSynEditKeyStrokes(NewStroke.Collection).Items[KeyLoc].Command);

          {$IFDEF SYN_CLX}
            Result := MessageDlg(Format(SYNS_DuplicateShortcutMsg,
              [QMenus.ShortCutToText(AForm.Keystroke), TmpCommand]),
              mtError, [mbOK, mbCancel], 0) = mrOK;
          {$ELSE}
            Result := MessageDlg(Format(SYNS_DuplicateShortcutMsg,
              [Menus.ShortCutToText(AForm.Keystroke), TmpCommand]),
              mtError, [mbOK, mbCancel], 0) = mrOK;
          {$ENDIF}
            NewStroke.Free;

            if Result then
              Result := AddKeyStroke;
          end;
        // Some other kind of exception, we don't deal with it...
      end;
    end;
  end;

begin
  AForm := TSynEditKeystrokeEditorForm.Create(Self);
  with AForm do
    try
      Caption := 'Add Keystroke';
      ExtendedString := self.ExtendedString;
      Command := ecNone;
      Keystroke := 0;
      Keystroke2 := 0;

      if AddKeyStroke then
      begin

        with KeyCmdList.Items.Add do
        begin
          if FExtended then
            Caption := ConvertCodeStringToExtended(EditorCommandToCodeString(NewStroke.Command))
          else Caption := EditorCommandToCodeString(NewStroke.Command);
          if NewStroke.ShortCut = 0 then
            SubItems.Add(SYNS_ShortcutNone)
          else
          if NewStroke.ShortCut2 = 0 then
          {$IFDEF SYN_CLX}
            SubItems.Add(QMenus.ShortCutToText(NewStroke.ShortCut))
          {$ELSE}
            SubItems.Add(Menus.ShortCutToText(NewStroke.ShortCut))
          {$ENDIF}
          else
          {$IFDEF SYN_CLX}
            SubItems.Add(QMenus.ShortCutToText(NewStroke.ShortCut)+ ' '+
              QMenus.ShortCutToText(NewStroke.ShortCut2));
          {$ELSE}
            SubItems.Add(Menus.ShortCutToText(NewStroke.ShortCut)+ ' '+
              Menus.ShortCutToText(NewStroke.ShortCut2));
          {$ENDIF}
        end;
      end;
    finally
      AForm.Free;
    end;
end;

procedure TSynEditKeystrokesEditorForm.btnEditClick(Sender: TObject);
var
  SelItem: TListItem;
  OldShortcut: TShortcut;
  OldShortcut2: TShortcut;
  AForm : TSynEditKeystrokeEditorForm;

  function EditKeyStroke: Boolean;
  VAR KeyLoc : Integer;
      TmpCommand: String;
  begin
    Result := False;
    KeyLoc := 0;
    if AForm.ShowModal = mrOK then
    begin
      Result := True;
      OldShortCut := FKeystrokes[SelItem.Index].ShortCut;
      OldShortCut2 := FKeystrokes[SelItem.Index].ShortCut2;

      try
        KeyLoc := TSynEditKeyStrokes(FKeystrokes[SelItem.Index].Collection).FindShortcut2(AForm.Keystroke, AForm.Keystroke2);
        FKeystrokes[SelItem.Index].Command := AForm.Command;
        FKeystrokes[SelItem.Index].ShortCut := AForm.Keystroke;
        FKeystrokes[SelItem.Index].ShortCut2 := AForm.Keystroke2;
      except
        on ESynKeyError do
          begin
            // Shortcut already exists in the collection!
            if FExtended then
              TmpCommand := ConvertCodeStringToExtended(EditorCommandToCodeString(TSynEditKeyStrokes(FKeystrokes[SelItem.Index].Collection).Items[KeyLoc].Command))
            else TmpCommand := EditorCommandToCodeString(TSynEditKeyStrokes(FKeystrokes[SelItem.Index].Collection).Items[KeyLoc].Command);

          {$IFDEF SYN_CLX}
            Result := MessageDlg(Format(SYNS_DuplicateShortcutMsg,
              [QMenus.ShortCutToText(AForm.Keystroke), TmpCommand]),
              mtError, [mbOK, mbCancel], 0) = mrOK;
          {$ELSE}
            Result := MessageDlg(Format(SYNS_DuplicateShortcutMsg,
              [Menus.ShortCutToText(AForm.Keystroke), TmpCommand]),
              mtError, [mbOK, mbCancel], 0) = mrOK;
          {$ENDIF}

            FKeystrokes[SelItem.Index].ShortCut := OldShortCut;
            FKeystrokes[SelItem.Index].ShortCut2 := OldShortCut2;

            if Result then
              Result := EditKeyStroke;
          end;
        // Some other kind of exception, we don't deal with it...
      end;
    end;
(*
      if ShowModal = mrOK then
      begin

        try
        except
          on ESynKeyError do
            begin
              // Shortcut already exists in the collection!
              {$IFDEF SYN_KYLIX}
              MessageDlg(Format(SYNS_DuplicateShortcutMsg2,
                [QMenus.ShortCutToText(Keystroke)]), mtError, [mbOK], 0);
              {$ELSE}
              MessageDlg(Format(SYNS_DuplicateShortcutMsg2,
                [Menus.ShortCutToText(Keystroke)]), mtError, [mbOK], 0);
              {$ENDIF}
            end;
          // Some other kind of exception, we don't deal with it...
        end;
*)
  end;
begin
  SelItem := KeyCmdList.Selected;
  if SelItem = NIL then
  begin
    {$IFDEF SYN_CLX}
    QControls.Beep;
    {$ELSE}
    MessageBeep(1);
    {$ENDIF}
    exit;
  end;
  AForm := TSynEditKeystrokeEditorForm.Create(Self);
  with AForm do
    try
      ExtendedString := self.ExtendedString;
      Command := FKeystrokes[SelItem.Index].Command;
      Keystroke := FKeystrokes[SelItem.Index].Shortcut;
      Keystroke2 := FKeystrokes[SelItem.Index].Shortcut2;
      if EditKeyStroke then
      begin
        KeyCmdList.Items.BeginUpdate;
        try
          with SelItem do
          begin

            if FExtended then
              Caption := ConvertCodeStringToExtended(EditorCommandToCodeString(FKeystrokes[Index].Command))
            else Caption := EditorCommandToCodeString(FKeystrokes[Index].Command);

            if FKeystrokes[Index].ShortCut = 0 then
              SubItems[0] := SYNS_ShortcutNone
            else
              if FKeystrokes[Index].ShortCut2 = 0 then
              {$IFDEF SYN_CLX}
                SubItems[0] := QMenus.ShortCutToText(FKeystrokes[Index].ShortCut)
              {$ELSE}
                SubItems[0] := Menus.ShortCutToText(FKeystrokes[Index].ShortCut)
              {$ENDIF}
              else
              {$IFDEF SYN_CLX}
                SubItems[0] := QMenus.ShortCutToText(FKeystrokes[Index].ShortCut)
                  + ' ' + QMenus.ShortCutToText(FKeystrokes[Index].ShortCut2);
              {$ELSE}
                SubItems[0] := Menus.ShortCutToText(FKeystrokes[Index].ShortCut)
                  + ' ' + Menus.ShortCutToText(FKeystrokes[Index].ShortCut2);
              {$ENDIF}
          end;
        finally
          KeyCmdList.Items.EndUpdate;
        end;
      end;
    finally
      AForm.Free;
    end;
end;                                                                            //DDH 10/16/01 End (reworked procs)

procedure TSynEditKeystrokesEditorForm.btnDeleteClick(Sender: TObject);
var
  SelItem: TListItem;
begin
  SelItem := KeyCmdList.Selected;
  if SelItem = NIL then
  begin
    {$IFDEF SYN_CLX}
    QControls.Beep;
    {$ELSE}
    MessageBeep(1);
    {$ENDIF}
    exit;
  end;
  FKeystrokes[SelItem.Index].Free;
  KeyCmdList.Items.Delete(SelItem.Index);
end;

procedure TSynEditKeystrokesEditorForm.btnClearClick(Sender: TObject);
begin
  FKeystrokes.Clear;
  KeyCmdList.Items.Clear;
end;

procedure TSynEditKeystrokesEditorForm.btnResetClick(Sender: TObject);
begin
  FKeystrokes.ResetDefaults;
  UpdateKeystrokesList;
end;

procedure TSynEditKeystrokesEditorForm.FormCreate(Sender: TObject);
begin
  {$IFDEF SYN_COMPILER_3_UP}
  KeyCmdList.RowSelect := True;
  {$ENDIF}
end;

procedure TSynEditKeystrokesEditorForm.btnOKClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TSynEditKeystrokesEditorForm.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSynEditKeystrokesEditorForm.KeyCmdListClick(Sender: TObject);
begin
  btnEdit.Enabled := Assigned(KeyCmdList.Selected);
  btnDelete.Enabled := btnEdit.Enabled;
end;

end.

