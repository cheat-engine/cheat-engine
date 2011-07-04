{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditKeyCmdEditor.pas, released 2000-04-07.
The Original Code is based on the mwKeyCmdEditor.pas file from the
mwEdit component suite by Martin Waldenburg and other developers.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
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

$Id: SynEditKeyCmdEditor.pas,v 1.10 2004/06/25 14:14:20 markonjezic Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{$IFNDEF QSYNEDITKEYCMDEDITOR}
unit SynEditKeyCmdEditor;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  Qt,
  QGraphics,
  QMenus,
  QControls,
  QForms,
  QDialogs,
  QStdCtrls,
  QExtCtrls,
  QComCtrls,
  QSynEditKeyCmds,
  QSynEditMiscClasses,
{$ELSE}
  Windows,
  Messages,
  Graphics,
  Menus,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ComCtrls,
  ExtCtrls,
  SynEditKeyCmds,
  SynEditMiscClasses,
{$ENDIF}
  SysUtils,
  Classes;


type
  TSynEditKeystrokeEditorForm = class(TForm)
    pnlAlign: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    bntClearKey: TButton;
    btnOK: TButton;
    cmbCommand: TComboBox;
    btnCancel: TButton;

    procedure FormShow(Sender: TObject);
    procedure bntClearKeyClick(Sender: TObject);
    procedure cmbCommandKeyPress(Sender: TObject; var Key: Char);
    procedure cmbCommandExit(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FExtended: Boolean;
    procedure SetCommand(const Value: TSynEditorCommand);
    procedure SetKeystroke(const Value: TShortcut);
    procedure AddEditorCommand(const S: string);
    function GetCommand: TSynEditorCommand;
    function GetKeystroke: TShortcut;
    function GetKeystroke2: TShortcut;
    procedure SetKeystroke2(const Value: TShortcut);
  public
    hkKeystroke2: TSynHotKey;
    hkKeystroke: TSynHotKey;
    property Command: TSynEditorCommand read GetCommand write SetCommand;
    property Keystroke: TShortcut read GetKeystroke write SetKeystroke;
    property Keystroke2: TShortcut read GetKeystroke2 write SetKeystroke2;
    property ExtendedString: Boolean read FExtended write FExtended default True;
  end;

var
  SynEditKeystrokeEditorForm: TSynEditKeystrokeEditorForm;

implementation

{$R *.dfm}

{ TSynEditKeystrokeEditorForm }

procedure TSynEditKeystrokeEditorForm.SetCommand(const Value: TSynEditorCommand);
begin
  if FExtended then
    cmbCommand.Text := ConvertCodeStringToExtended(EditorCommandToCodeString(Value))
  else cmbCommand.Text := EditorCommandToCodeString(Value);
end;

procedure TSynEditKeystrokeEditorForm.SetKeystroke(const Value: TShortcut);
begin
  {*****************}
  hkKeystroke.Hotkey := Value;
end;

procedure TSynEditKeystrokeEditorForm.FormShow(Sender: TObject);
Var i : Integer;
begin
  if FExtended then
    GetEditorCommandExtended(AddEditorCommand)
  else GetEditorCommandValues(AddEditorCommand);

  //Now add the values for quick access
  for i := 0 to cmbCommand.Items.Count - 1 do
    cmbCommand.Items.Objects[i] := TObject(IndexToEditorCommand(i));
  if FExtended then
    cmbCommand.Sorted := True;
end;

procedure TSynEditKeystrokeEditorForm.AddEditorCommand(const S: string);
begin
  cmbCommand.Items.Add(S);
end;

function TSynEditKeystrokeEditorForm.GetCommand: TSynEditorCommand;
var
  NewCmd: longint;
begin
  cmbCommand.ItemIndex := cmbCommand.Items.IndexOf(cmbCommand.Text);
  if cmbCommand.ItemIndex <> -1 then
  begin
    NewCmd := TSynEditorCommand(Integer(cmbCommand.Items.Objects[cmbCommand.ItemIndex]));
  end else if not IdentToEditorCommand(cmbCommand.Text, NewCmd) then
  begin
     try
       NewCmd := StrToInt(cmbCommand.Text);
     except
       NewCmd := ecNone;
     end;
  end;
  Result := NewCmd;
end;

function TSynEditKeystrokeEditorForm.GetKeystroke: TShortcut;
begin
 {*****************}
  Result := hkKeystroke.HotKey;
end;

procedure TSynEditKeystrokeEditorForm.bntClearKeyClick(Sender: TObject);
begin
  hkKeystroke.HotKey := 0;
  hkKeystroke2.HotKey := 0;
end;

function TSynEditKeystrokeEditorForm.GetKeystroke2: TShortcut;
begin
  Result := hkKeystroke2.HotKey;
end;

procedure TSynEditKeystrokeEditorForm.SetKeystroke2(const Value: TShortcut);
begin
  hkKeystroke2.Hotkey := Value;
end;

procedure TSynEditKeystrokeEditorForm.cmbCommandKeyPress(Sender: TObject;
  var Key: Char);
var WorkStr : String;
    i       : Integer;
begin
//This would be better if componentized, but oh well...
  WorkStr := AnsiUppercase(Copy(cmbCommand.Text, 1, cmbCommand.SelStart) + Key);
  i := 0;
  While i < cmbCommand.Items.Count do
  begin
    if pos(WorkStr, AnsiUppercase(cmbCommand.Items[i])) = 1 then
    begin
      cmbCommand.Text := cmbCommand.Items[i];
      cmbCommand.SelStart := length(WorkStr);
      cmbCommand.SelLength := Length(cmbCommand.Text) - cmbCommand.SelStart;
      Key := #0;
      break;
    end else inc(i);
  end;
end;

procedure TSynEditKeystrokeEditorForm.cmbCommandExit(Sender: TObject);
VAR TmpIndex : Integer;
begin
  TmpIndex := cmbCommand.Items.IndexOf(cmbCommand.Text);
  if TmpIndex = -1 then
  begin
     cmbCommand.ItemIndex := cmbCommand.Items.IndexOf(ConvertCodeStringToExtended('ecNone'));
  end else cmbCommand.ItemIndex := TmpIndex;  //need to force it incase they just typed something in
end;

procedure TSynEditKeystrokeEditorForm.btnOKClick(Sender: TObject);
begin
  if Command = ecNone then
  begin
    MessageDlg('You must first select a command.', mtError, [mbOK], 0);
    cmbCommand.SetFocus;
    cmbCommand.SelectAll;
  end else if Keystroke = 0 then
  begin
    MessageDlg('The command "'+cmbCommand.Text+'" needs to have at least one keystroke assigned to it.', mtError, [mbOK], 0);
    hkKeystroke.SetFocus;
  end else ModalResult := mrOK;
end;

procedure TSynEditKeystrokeEditorForm.FormCreate(Sender: TObject);
begin    
  hkKeystroke := TSynHotKey.Create(self);
  with hkKeystroke do
  begin
    Parent := pnlAlign;
    Left := 65;
    Top := 38;
    Width := 186;
    Height := 19;
    HotKey := 0;
    InvalidKeys := [];
    Modifiers := [];
    TabOrder := 1;
  end;

  hkKeystroke2 := TSynHotKey.Create(self);
  with hkKeystroke2 do
  begin
    Parent := pnlAlign;
    Left := 65;
    Top := 62;
    Width := 186;
    Height := 19;
    HotKey := 0;
    InvalidKeys := [];
    Modifiers := [];
    TabOrder := 2;
  end;
end;

procedure TSynEditKeystrokeEditorForm.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  // if this event is not present CLX will complain
end;

end.



