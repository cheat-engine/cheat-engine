unit FrmMemoryRecordDropdownSettingsUnit;

{$mode delphi}

interface

uses
  LCLType, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, MemoryRecordUnit;

resourcestring
rsDDDropdownOtionsFor = 'Dropdown options for ';

type

  { TFrmMemoryRecordDropdownSettings }

  TFrmMemoryRecordDropdownSettings = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    cbDisallowUserInput: TCheckBox;
    cbOnlyShowDescription: TCheckBox;
    cbDisplayAsDropdownItem: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    memoDropdownItems: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure btnOkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure memoDropdownItemsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { private declarations }
    memrec: TMemoryrecord;
  public
    { public declarations }
    constructor create(memrec: TMemoryrecord);
  end;

implementation

{$R *.lfm}

{ TFrmMemoryRecordDropdownSettings }

procedure TFrmMemoryRecordDropdownSettings.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:=cafree;
end;

procedure TFrmMemoryRecordDropdownSettings.memoDropdownItemsKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key=vk_escape then
    modalresult:=mrCancel;
end;

procedure TFrmMemoryRecordDropdownSettings.btnOkClick(Sender: TObject);
begin
  memrec.DropDownList.Assign(memoDropdownItems.Lines);
  memrec.DropDownReadOnly:=cbDisallowUserInput.checked;
  memrec.DropDownDescriptionOnly:=cbOnlyShowDescription.checked;
  memrec.DisplayAsDropDownListItem:=cbDisplayAsDropdownItem.checked;

  modalresult:=mrok;
end;

constructor TFrmMemoryRecordDropdownSettings.create(memrec: TMemoryrecord);
begin
  inherited create(Application);

  self.memrec:=memrec;
  if memrec.DropDownList<>nil then
    memoDropdownItems.Lines.AddStrings(memrec.DropDownList);

  cbDisallowUserInput.checked:=memrec.DropDownReadOnly;
  cbOnlyShowDescription.checked:=memrec.DropDownDescriptionOnly;
  cbDisplayAsDropdownItem.checked:=memrec.DisplayAsDropDownListItem;

  caption:=rsDDDropdownOtionsFor+memrec.description;
end;

end.

