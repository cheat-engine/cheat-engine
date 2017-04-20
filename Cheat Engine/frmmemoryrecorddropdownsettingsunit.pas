unit FrmMemoryRecordDropdownSettingsUnit;

{$mode delphi}

interface

uses
  LCLType, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, MemoryRecordUnit, CEFuncProc;

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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure memoDropdownItemsChange(Sender: TObject);
    procedure memoDropdownItemsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { private declarations }
    memrec: TMemoryrecord;
    linkedToMemrec: boolean;
    linkedMemrec: string;

  public
    { public declarations }
    constructor create(memrec: TMemoryrecord);
  end;

implementation

{$R *.lfm}

{ TFrmMemoryRecordDropdownSettings }

uses MainUnit;

procedure TFrmMemoryRecordDropdownSettings.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:=cafree;
end;

procedure TFrmMemoryRecordDropdownSettings.FormCreate(Sender: TObject);
begin
  if LoadFormPosition(self) then
    autosize:=false;
end;

procedure TFrmMemoryRecordDropdownSettings.FormDestroy(Sender: TObject);
begin
  SaveFormPosition(self);
end;

procedure TFrmMemoryRecordDropdownSettings.FormShow(Sender: TObject);
var wanted: integer;
begin
  if autosize then
  begin
    autosize:=false;

    wanted:=canvas.TextHeight('AjCgyi')*8;
    if memoDropdownItems.Height<wanted then
      height:=height+wanted-memoDropdownItems.Height;
  end;
end;

procedure TFrmMemoryRecordDropdownSettings.memoDropdownItemsChange(
  Sender: TObject);
var
  s: string;
  options: boolean;
  mr: TMemoryRecord;
begin
  if (memoDropdownItems.lines.Count=1) then
  begin
    s:=trim(memoDropdownItems.lines[0]);
    if length(s)>2 then
    begin
      if (s[1]='(') and (s[length(s)]=')') then
      begin
        s:=copy(s,2,length(s)-2);
        mr:=MainForm.addresslist.getRecordWithDescription(s);
        if mr<>nil then
        begin
          cbDisallowUserInput.enabled:=false;
          cbOnlyShowDescription.enabled:=false;
          cbDisplayAsDropdownItem.enabled:=false;

          cbDisallowUserInput.checked:=mr.DropDownReadOnly;
          cbOnlyShowDescription.checked:=mr.DropDownDescriptionOnly;
          cbDisplayAsDropdownItem.checked:=mr.DisplayAsDropDownListItem;

          linkedToMemrec:=true;
          linkedMemrec:=s;
          exit;
        end;
      end;
    end;
  end;

  if cbDisallowUserInput.enabled=false then cbDisallowUserInput.enabled:=true;
  if cbOnlyShowDescription.enabled=false then cbOnlyShowDescription.enabled:=true;
  if cbDisplayAsDropdownItem.enabled=false then cbDisplayAsDropdownItem.enabled:=true;
end;

procedure TFrmMemoryRecordDropdownSettings.memoDropdownItemsKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key=vk_escape then
    modalresult:=mrCancel;
end;

procedure TFrmMemoryRecordDropdownSettings.btnOkClick(Sender: TObject);
begin


  if linkedtomemrec then
  begin
    memrec.DropDownLinked:=linkedToMemrec;
    memrec.DropDownLinkedMemrec:=linkedMemrec;
  end
  else
  begin
    memrec.DropDownList.Assign(memoDropdownItems.Lines);
    memrec.DropDownReadOnly:=cbDisallowUserInput.checked;
    memrec.DropDownDescriptionOnly:=cbOnlyShowDescription.checked;
    memrec.DisplayAsDropDownListItem:=cbDisplayAsDropdownItem.checked;
  end;


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

