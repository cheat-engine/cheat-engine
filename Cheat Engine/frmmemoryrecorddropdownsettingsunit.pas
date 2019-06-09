unit FrmMemoryRecordDropdownSettingsUnit;

{$mode delphi}

interface

uses
  LCLType, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, MemoryRecordUnit, CEFuncProc, SynEdit, Menus;

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
    doImageList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Label3: TLabel;
    lblFormat: TLabel;
    Paste1: TMenuItem;
    Undo1: TMenuItem;
    Panel0: TPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    procedure btnOkClick(Sender: TObject);
    procedure cbDisallowUserInputChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure synEditDropdownItemsChange(Sender: TObject);
    procedure synEditDropdownItemsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Undo1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
  private
    { private declarations }
    memrec: TMemoryrecord;
    synEditDropdownItems: TSynEdit;
    linkedToMemrec: boolean;
    linkedMemrec: string;

  public
    { public declarations }
    constructor create(memrec: TMemoryrecord);  overload;
  end;

implementation

{$R *.lfm}

{ TFrmMemoryRecordDropdownSettings }

uses MainUnit,SynPluginMultiCaret;

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
    if synEditDropdownItems.Height<wanted then
      height:=height+wanted-synEditDropdownItems.Height;
  end;
  synEditDropdownItems.SetFocus;
end;

procedure TFrmMemoryRecordDropdownSettings.synEditDropdownItemsChange(
  Sender: TObject);
var
  s: string;
  options: boolean;
  mr: TMemoryRecord;
begin
  if (synEditDropdownItems.lines.Count=1) then
  begin
    s:=trim(synEditDropdownItems.lines[0]);
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

  linkedToMemrec:=false;
  if cbDisallowUserInput.enabled=false then cbDisallowUserInput.enabled:=true;
  if cbOnlyShowDescription.enabled=false then cbOnlyShowDescription.enabled:=true;
  if cbDisplayAsDropdownItem.enabled=false then cbDisplayAsDropdownItem.enabled:=true;
end;

procedure TFrmMemoryRecordDropdownSettings.synEditDropdownItemsKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key=vk_escape then
    modalresult:=mrCancel;
end;

procedure TFrmMemoryRecordDropdownSettings.btnOkClick(Sender: TObject);
var i: integer;
begin
  if linkedtomemrec then
  begin
    memrec.DropDownLinked:=true;
    memrec.DropDownLinkedMemrec:=linkedMemrec;
  end
  else
  begin
    memrec.DropDownLinked:=false;

    memrec.DropDownList.Clear;
    for i:=0 to synEditDropdownItems.lines.Count-1 do
      if pos(':', synEditDropdownItems.lines[i])>0 then
        memrec.DropDownList.add(synEditDropdownItems.lines[i]);

    memrec.DropDownReadOnly:=cbDisallowUserInput.checked;
    memrec.DropDownDescriptionOnly:=cbOnlyShowDescription.checked;
    memrec.DisplayAsDropDownListItem:=cbDisplayAsDropdownItem.checked;
  end;


  modalresult:=mrok;
end;

procedure TFrmMemoryRecordDropdownSettings.cbDisallowUserInputChange(
  Sender: TObject);
begin
  label3.visible:=cbDisallowUserInput.checked and cbOnlyShowDescription.checked and cbDisplayAsDropdownItem.checked;

end;

constructor TFrmMemoryRecordDropdownSettings.create(memrec: TMemoryrecord);
var
  multicaret: TSynPluginMultiCaret;
  fs: integer;
begin
  inherited create(Application);


  fs:=font.size;
  self.memrec:=memrec;

  synEditDropdownItems:=TSynEdit.Create(Self);
  with synEditDropdownItems do begin
    Name:='synEditDropdownItems';
    Text:='';
    Parent:=Panel0;
    Align:=alClient;
    WantTabs:=false;
    Options:=[eoKeepCaretX,eoTrimTrailingSpaces];
    OnKeyDown:=synEditDropdownItemsKeyDown;
    OnChange:=synEditDropdownItemsChange;
    PopupMenu:=PopupMenu1;
    Gutter.LineNumberPart.Visible:=true;
    Gutter.ChangesPart.Visible:=true;
    Gutter.CodeFoldPart.Visible:=false;
    Gutter.MarksPart.Visible:=false;
    Gutter.SeparatorPart.Visible:=false;

    font.size:=13;
  end;

  multicaret:=TSynPluginMultiCaret.Create(synEditDropdownItems);
  multicaret.EnableWithColumnSelection:=true;
  multicaret.DefaultMode:=mcmMoveAllCarets;
  multicaret.DefaultColumnSelectMode:=mcmCancelOnCaretMove;

  if memrec.DropDownList<>nil then
    synEditDropdownItems.Lines.AddStrings(memrec.DropDownList);

  cbDisallowUserInput.checked:=memrec.DropDownReadOnly;
  cbOnlyShowDescription.checked:=memrec.DropDownDescriptionOnly;
  cbDisplayAsDropdownItem.checked:=memrec.DisplayAsDropDownListItem;

  caption:=rsDDDropdownOtionsFor+memrec.description;

  if memrec.DropDownLinked then
  begin
    synEditDropdownItems.Text:='('+memrec.DropDownLinkedMemrec+')';
    synEditDropdownItemsChange(synEditDropdownItems);
  end;

end;

procedure TFrmMemoryRecordDropdownSettings.Undo1Click(Sender: TObject);
begin
  synEditDropdownItems.Undo;
end;

procedure TFrmMemoryRecordDropdownSettings.Cut1Click(Sender: TObject);
begin
  synEditDropdownItems.CutToClipboard;
end;

procedure TFrmMemoryRecordDropdownSettings.Copy1Click(Sender: TObject);
begin
  synEditDropdownItems.CopyToClipboard;
end;

procedure TFrmMemoryRecordDropdownSettings.Paste1Click(Sender: TObject);
begin
  synEditDropdownItems.PasteFromClipboard;
end;

end.

