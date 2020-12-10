unit frmStructuresNewStructureUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Registry, LCLType, ExtCtrls, betterControls;

type

  { TfrmStructuresNewStructure }

  TfrmStructuresNewStructure = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    cbUseAutoTypes: TCheckBox;
    cbGuessFieldTypes: TCheckBox;
    edtGuessSize: TEdit;
    edtStructName: TEdit;
    labelStructureSize: TLabel;
    labelStructureName: TLabel;
    Panel1: TPanel;
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);

    function getStructName: string;
    procedure setStructName(v: string);
    function getUseAutoTypes: boolean;
    function getGuessFieldTypes: boolean;
    function getGuessSize: integer;
    procedure setGuessSize(v: integer);
  private
    { private declarations }
  public
    { public declarations }
    property structName: string read getStructName write setStructName;
    property useAutoTypes: boolean read getUseAutoTypes;
    property guessFieldTypes: boolean read getGuessFieldTypes;
    property guessSize: integer read getGuessSize write setGuessSize;
  end;



implementation

resourcestring
  rsWindowTitle = 'New Structure';
  rsUseAutoTypesIfAvailable = 'Use Auto Types If Available';
  rsGuessFieldTypes = 'Guess Field Types';
  rsStructureName = 'Structure Name:';
  rsSize = 'Size:';

{$R *.lfm}

{ TfrmStructuresNewStructure }

procedure TfrmStructuresNewStructure.setStructName(v: string);
begin
  self.edtStructName.Text := v;
end;

function TfrmStructuresNewStructure.getStructName: String;
begin
  result := self.edtStructName.Text;
end;

function TfrmStructuresNewStructure.getUseAutoTypes: Boolean;
begin
  result := self.cbUseAutoTypes.Checked;
end;

function TfrmStructuresNewStructure.getGuessFieldTypes: Boolean;
begin
  result := self.cbGuessFieldTypes.Checked;
end;

function TfrmStructuresNewStructure.getGuessSize : Integer;
begin
  try
    result := StrToInt(self.edtGuessSize.Text);
  except
    On E : EConvertError do
      result := -1;
  end;
end;

procedure TfrmStructuresNewStructure.setGuessSize(v : Integer);
begin
  self.edtGuessSize.Text := Format('%d', [v]);
end;


procedure TfrmStructuresNewStructure.btnOkClick(Sender: TObject);
var
  reg: TRegistry;
begin
  reg := tregistry.create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('\Software\Cheat Engine\DissectData',true) then
    begin
      reg.WriteBool('Use Auto Types', useAutoTypes);
      reg.WriteBool('Guess Field Types', guessFieldTypes);
      reg.WriteInteger('Guess Size', guessSize);
    end;
  finally
    reg.free;
  end;

  modalresult := mrok;
end;

procedure TfrmStructuresNewStructure.FormCreate(Sender: TObject);
var
  reg: TRegistry;
begin
  reg := TRegistry.create;
  try
    self.Caption := rsWindowTitle;
    self.labelStructureName.Caption := rsStructureName;
    self.labelStructureSize.Caption := rsSize;
    self.cbUseAutoTypes.Caption := rsUseAutoTypesIfAvailable;
    self.cbGuessFieldTypes.Caption := rsGuessFieldTypes;

    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('\Software\Cheat Engine\DissectData',false) then
    begin
      if reg.ValueExists('Use Auto Types') then self.cbUseAutoTypes.Checked := reg.ReadBool('Use Auto Types') else self.cbUseAutoTypes.Checked := true;
      if reg.ValueExists('Guess Field Types') then self.cbGuessFieldTypes.Checked := reg.ReadBool('Guess Field Types') else self.cbGuessFieldTypes.Checked := true;
      if reg.ValueExists('Guess Size') then self.edtGuessSize.Text := Format('%d', [reg.ReadInteger('Guess Size')]) else guessSize := 4096;
      if (guessSize <= 0) then guessSize := 4096;
    end;
  finally
    reg.free;
  end;
end;


procedure TfrmStructuresNewStructure.FormShow(Sender: TObject);
begin
  self.edtStructName.SelectAll;
  self.ActiveControl := self.edtStructName;
end;


end.

