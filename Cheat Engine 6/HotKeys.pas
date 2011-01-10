unit HotKeys;

{$MODE Delphi}

interface

uses
  windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, registry, CEFuncProc, ExtCtrls, LResources,
  comCtrls, menus, hotkeyhandler, MemoryRecordUnit;

type

  { THotKeyForm }

  THotKeyForm = class(TForm)
    BitBtn1: TBitBtn;
    Button2: TButton;
    btnApply: TButton;
    btnCreateHotkey: TButton;
    btnEditHotkey: TButton;
    btnCancel: TButton;
    cbFreezedirection: TComboBox;
    edtFreezeValue: TEdit;
    edtHotkey: TEdit;
    Label1: TLabel;
    ListView1: TListView;
    miDelete: TMenuItem;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    pmHotkeylist: TPopupMenu;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure BitBtn1Click(Sender: TObject);
    procedure btnCreateHotkeyClick(Sender: TObject);
    procedure btnEditHotkeyClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure cbFreezedirectionSelect(Sender: TObject);
    procedure edtHotkeyKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure miDeleteClick(Sender: TObject);
    procedure Panel2Resize(Sender: TObject);
    procedure pmHotkeylistPopup(Sender: TObject);
  private
    { Private declarations }
    keys: tkeycombo;
    editHotkey: boolean;
    fmemrec: TMemoryRecord;
    procedure SetMemrec(x: TMemoryRecord);
    function HotkeyActionToText(a: TMemrecHotkeyAction): string;
    function getHotkeyAction: TMemrecHotkeyAction;
  public
    { Public declarations }

    property memrec: TMemoryRecord read fmemrec write SetMemrec;
  end;


implementation

uses MainUnit;

function THotkeyform.getHotkeyAction: TMemrecHotkeyAction;
begin
  case cbFreezedirection.ItemIndex of
    0: result:=mrhToggleActivation;
    1: result:=mrhToggleActivationAllowIncrease;
    2: result:=mrhToggleActivationAllowDecrease;
    3: result:=mrhSetValue;
    4: result:=mrhDecreaseValue;
    5: result:=mrhIncreaseValue;
  end;

end;

function THotkeyform.HotkeyActionToText(a: TMemrecHotkeyAction): string;
begin
  {
  Toggle freeze/Toggle activated
  Toggle freeze and allow increase
  Toggle freeze and allow decrease
  Set value to:
  Decrease value with:
  Increase value with:

  }
  case a of
    mrhToggleActivation:                   result:=cbFreezedirection.Items[0];
    mrhToggleActivationAllowIncrease:      result:=cbFreezedirection.Items[1];
    mrhToggleActivationAllowDecrease:      result:=cbFreezedirection.Items[2];
    mrhSetValue:                           result:=cbFreezedirection.Items[3];
    mrhDecreaseValue:                      result:=cbFreezedirection.Items[4];
    mrhIncreaseValue:                      result:=cbFreezedirection.Items[5];
  end;
end;

procedure THotkeyform.SetMemrec(x: TMemoryRecord);
var i: integer;
li: TListItem;
begin
  if x.VarType=vtAutoAssembler then
  begin
    cbFreezedirection.Clear;
    cbFreezedirection.Items.add('Toggle script');
    cbFreezedirection.Enabled:=false;
    edtFreezeValue.visible:=false;
  end;

  listview1.clear;
  fmemrec:=x;
  for i:=0 to length(memrec.Hotkeys)-1 do
  begin
    if memrec.hotkeys[i].active then
    begin
      li:=listview1.Items.Add;

      li.caption:=ConvertKeyComboToString(memrec.hotkeys[i].keys);
      li.SubItems.Add(HotkeyActionToText(memrec.hotkeys[i].action));
      li.SubItems.Add(memrec.hotkeys[i].value);
    end;
  end;


end;

procedure THotKeyForm.btnCreateHotkeyClick(Sender: TObject);
var li: TListitem;
i: integer;
begin
  for i:=0 to length(keys)-1 do
    keys[i]:=0;

  pagecontrol1.ActivePage:=tabsheet2;
  li:=listview1.items.add;
  li.SubItems.add('');
  li.SubItems.add('');
  li.Data:=pointer(-1);
  li.selected:=true;

  edtHotkey.text:='';
  cbFreezedirection.ItemIndex:=0;
  edtFreezeValue.text:='';

  editHotkey:=true;
  listview1.Enabled:=false;
end;

procedure THotKeyForm.BitBtn1Click(Sender: TObject);
begin
  close;
end;

procedure THotKeyForm.btnEditHotkeyClick(Sender: TObject);
begin
  if listview1.selected=nil then exit;

  pagecontrol1.ActivePage:=tabsheet2;
  listview1.Enabled:=false;

  keys:=memrec.Hotkeys[ptruInt(listview1.selected.data)].keys;
  edtHotkey.text:=ConvertKeyComboToString(keys);

  cbFreezedirection.ItemIndex:=cbFreezedirection.Items.IndexOf(listview1.selected.SubItems[0]);
  edtFreezeValue.text:=listview1.selected.subitems[1];


  editHotkey:=false;
end;

procedure THotKeyForm.btnApplyClick(Sender: TObject);
var hotkeytag: integer;
begin
  if editHotkey then
  begin
    //delete the old hotkey
    hotkeytag:=ptrInt(listview1.Selected.data);
    UnregisterAddressHotkey(memrec, hotkeytag);
  end;

  hotkeytag:=memrec.Addhotkey(mainform.handle, keys, getHotkeyAction, edtFreezeValue.text );

  listview1.selected.Caption:=edtHotkey.Text;
  listview1.Selected.SubItems[0]:=cbFreezedirection.Text;
  listview1.selected.subitems[1]:=edtFreezeValue.text;
  listview1.Selected.data:=pointer(ptrInt(hotkeytag));


  pagecontrol1.ActivePage:=tabsheet1;
  listview1.Enabled:=true;
end;

procedure THotKeyForm.btnCancelClick(Sender: TObject);
begin
  if (listview1.Selected<>nil) and (ptrint(ptruint(listview1.selected.data))=-1) then //created hotkey
    listview1.selected.delete;

  pagecontrol1.ActivePage:=tabsheet1;
  listview1.Enabled:=true;
end;

procedure THotKeyForm.Button2Click(Sender: TObject);
begin
  zeromemory(@keys,sizeof(TKeyCombo));
  edtHotkey.Text:=ConvertKeyComboToString(keys);
  edtHotkey.SetFocus;
end;

procedure THotKeyForm.cbFreezedirectionSelect(Sender: TObject);
begin
  edtFreezeValue.enabled:=cbFreezeDirection.itemindex >=3; //set value, increase by and decrease by
end;

procedure THotKeyForm.edtHotkeyKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if keys[4]=0 then
  begin
    for i:=0 to 4 do
      if keys[i]=0 then
      begin
        keys[i]:=key;
        break;
      end else
      if keys[i]=key then break;
  end;

  edtHotkey.Text:=ConvertKeyComboToString(keys);

  key:=0;
end;

procedure THotKeyForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  fmemrec.endEdit;
  CloseAction:=caFree;
end;



procedure THotKeyForm.FormCreate(Sender: TObject);
begin
  pagecontrol1.ActivePage:=tabsheet1;
end;

procedure THotKeyForm.ListView1SelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  btnEditHotkey.enabled:=selected;
end;

procedure THotKeyForm.miDeleteClick(Sender: TObject);
var hkt: integer;
begin
  if listview1.enabled and (listview1.Selected<>nil) then
  begin
    hkt:=ptrint(listview1.selected.data);
    if hkt>=0 then
      memrec.removeHotkey(hkt);

    listview1.selected.delete;
  end;
end;

procedure THotKeyForm.Panel2Resize(Sender: TObject);
begin
  bitbtn1.left:=(panel2.clientwidth div 2) - (bitbtn1.width div 2);
end;

procedure THotKeyForm.pmHotkeylistPopup(Sender: TObject);
begin
  midelete.visible:=listview1.enabled and (listview1.Selected<>nil);
end;

initialization
  {$i HotKeys.lrs}

end.
