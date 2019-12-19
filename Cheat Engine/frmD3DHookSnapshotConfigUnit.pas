unit frmD3DHookSnapshotConfigUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, ExtCtrls, CEFuncProc, registry, commonTypeDefs;

type

  { TfrmD3DHookSnapshotConfig }

  TfrmD3DHookSnapshotConfig = class(TForm)
    btnClearFullSnapshot: TButton;
    btnClearSmallSnapshot: TButton;
    cbAlsoOutputPng: TCheckBox;
    cbProgressive: TCheckBox;
    cbClearDepth: TCheckBox;
    dirSnapshot: TDirectoryEdit;
    edtFullSnapshot: TEdit;
    edtSmallSnapshot: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    mbCancel: TButton;
    mbOk: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    rgPictureFormat: TRadioGroup;
    procedure btnClearFullSnapshotClick(Sender: TObject);
    procedure btnClearSmallSnapshotClick(Sender: TObject);
    procedure cbClearDepthChange(Sender: TObject);
    procedure cbProgressiveChange(Sender: TObject);
    procedure edtFullSnapshotKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtSmallSnapshotKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure mbOkClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    fullsnapshotkey: integer;
    smallsnapshotkey: integer;

  end;

implementation

{$R *.lfm}

{ TfrmD3DHookSnapshotConfig }

procedure TfrmD3DHookSnapshotConfig.btnClearFullSnapshotClick(Sender: TObject);
begin
  fullsnapshotkey:=0;
  edtFullSnapshot.text:='';
end;

procedure TfrmD3DHookSnapshotConfig.btnClearSmallSnapshotClick(Sender: TObject);
begin
  smallsnapshotkey:=0;
  edtSmallSnapshot.text:='';
end;

procedure TfrmD3DHookSnapshotConfig.cbClearDepthChange(Sender: TObject);
begin
  if cbClearDepth.checked then
    cbProgressive.checked:=false;

  cbProgressive.enabled:=not cbClearDepth.checked;
end;

procedure TfrmD3DHookSnapshotConfig.cbProgressiveChange(Sender: TObject);
begin
  edtSmallSnapshot.enabled:=not cbProgressive.checked;
  btnClearSmallSnapshot.enabled:=not cbProgressive.checked;

end;


procedure TfrmD3DHookSnapshotConfig.edtFullSnapshotKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var k: Tkeycombo;
begin
  fullsnapshotkey:=key;
  k[0]:=fullsnapshotkey;
  k[1]:=0;
  edtFullSnapshot.text:=ConvertKeyComboToString(k);
end;

procedure TfrmD3DHookSnapshotConfig.edtSmallSnapshotKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var k: Tkeycombo;
begin
  smallsnapshotkey:=key;
  k[0]:=smallsnapshotkey;
  k[1]:=0;
  edtSmallSnapshot.text:=ConvertKeyComboToString(k);
end;

procedure TfrmD3DHookSnapshotConfig.FormCreate(Sender: TObject);
var reg: TRegistry;
    k: Tkeycombo;
begin
  reg:=tregistry.create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('\Software\Cheat Engine\D3DHook',false) then
    begin
      if reg.ValueExists('Snapshot Folder') then
        dirSnapshot.text:=reg.ReadString('Snapshot Folder');

      if reg.ValueExists('Snapshot Progressive') then
        cbProgressive.checked:=reg.ReadBool('Snapshot Progressive');

      if reg.ValueExists('Snapshot ClearDepth') then
        cbClearDepth.Checked:=reg.readBool('Snapshot ClearDepth');

      if reg.ValueExists('Full Snapshot Key') then
        fullsnapshotkey:=reg.ReadInteger('Full Snapshot Key');

      if reg.ValueExists('Small Snapshot Key') then
        smallsnapshotkey:=reg.ReadInteger('Small Snapshot Key');

      if reg.ValueExists('Also save PNG') then
        cbAlsoOutputPng.Checked:=reg.readBool('Also save PNG');

      if reg.ValueExists('Snapshot picture format') then
        rgPictureFormat.ItemIndex:=reg.ReadInteger('Snapshot picture format');

      k[1]:=0;
      k[0]:=fullsnapshotkey;
      edtFullSnapshot.text:=ConvertKeyComboToString(k);

      k[0]:=smallsnapshotkey;
      edtSmallSnapshot.text:=ConvertKeyComboToString(k);


    end;

  finally
    reg.free;
  end;

  if dirSnapshot.text='' then
    dirSnapshot.Text:=GetTempDir;

end;

procedure TfrmD3DHookSnapshotConfig.mbOkClick(Sender: TObject);
var reg: TRegistry;
begin
  reg:=tregistry.create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('\Software\Cheat Engine\D3DHook',true) then
    begin
      reg.WriteString('Snapshot Folder', dirSnapshot.Text);
      reg.WriteBool('Snapshot Progressive', cbProgressive.checked);
      reg.WriteBool('Snapshot ClearDepth', cbClearDepth.checked);
      reg.WriteInteger('Full Snapshot Key', fullsnapshotkey);
      reg.WriteInteger('Small Snapshot Key', smallsnapshotkey);
      reg.writeBool('Also save PNG', cbAlsoOutputPng.Checked);

      reg.WriteInteger('Snapshot picture format', rgPictureFormat.ItemIndex);
    end;

  finally
    reg.Free;
  end;

  modalresult:=mrok;
end;

end.

