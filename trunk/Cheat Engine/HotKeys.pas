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
    edtDescription: TEdit;
    edtFreezeValue: TEdit;
    edtHotkey: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lblID: TLabel;
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
    procedure FormShow(Sender: TObject);
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

resourcestring
  rsHotkeyID = 'Hotkey ID=%s';
  rsToggleScript = 'Toggle script';
  rsEnableScript = 'Enable script';
  rsDisableScript = 'Disable script';
  rsToggleFreeze = 'Toggle freeze';
  rsToggleFreezeAndAllowIncrease = 'Toggle freeze and allow increase';
  rsToggleFreezeAndAllowDecrease = 'Toggle freeze and allow decrease';
  rsFreeze = 'Freeze';
  rsUnfreeze = 'Unfreeze';
  rsSetValueTo = 'Set value to:';
  rsDecreaseValueWith = 'Decrease value with:';
  rsIncreaseValueWith = 'Increase value with:';



function THotkeyform.getHotkeyAction: TMemrecHotkeyAction;
begin
  if fmemrec.vartype=vtAutoAssembler then
  begin
    case cbFreezedirection.ItemIndex of
      0: result:=mrhToggleActivation;
      1: result:=mrhActivate;
      2: result:=mrhDeactivate;
    end;
  end
  else
  begin
    case cbFreezedirection.ItemIndex of
      0: result:=mrhToggleActivation;
      1: result:=mrhToggleActivationAllowIncrease;
      2: result:=mrhToggleActivationAllowDecrease;
      3: result:=mrhActivate;
      4: result:=mrhDeactivate;
      5: result:=mrhSetValue;
      6: result:=mrhDecreaseValue;
      7: result:=mrhIncreaseValue;
    end;
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
  if memrec.VarType=vtAutoAssembler then
  begin
    case a of
      mrhToggleActivation:                   result:=cbFreezedirection.Items[0];
      mrhActivate:                           result:=cbFreezedirection.items[1];
      mrhDeactivate:                         result:=cbFreezedirection.items[2];
    end;
  end
  else
  begin
    case a of
      mrhToggleActivation:                   result:=cbFreezedirection.Items[0];
      mrhToggleActivationAllowIncrease:      result:=cbFreezedirection.Items[1];
      mrhToggleActivationAllowDecrease:      result:=cbFreezedirection.Items[2];
      mrhActivate:                           result:=cbFreezedirection.items[3];
      mrhDeactivate:                         result:=cbFreezedirection.items[4];
      mrhSetValue:                           result:=cbFreezedirection.Items[5];
      mrhDecreaseValue:                      result:=cbFreezedirection.Items[6];
      mrhIncreaseValue:                      result:=cbFreezedirection.Items[7];
    end;

  end;
end;

procedure THotkeyform.SetMemrec(x: TMemoryRecord);
var i: integer;
li: TListItem;
hk: TMemoryRecordHotkey;
begin
  if x.VarType=vtAutoAssembler then
  begin
    cbFreezedirection.Clear;
    cbFreezedirection.Items.add(rsToggleScript);
    cbFreezedirection.items.add(rsEnableScript);
    cbFreezedirection.items.add(rsDisableScript);
    cbFreezedirection.ItemIndex:=0;
    edtFreezeValue.visible:=false;
  end;

  listview1.clear;
  fmemrec:=x;



  for i:=0 to memrec.HotkeyCount-1 do
  begin
    hk:=memrec.Hotkey[i];

    begin
      li:=listview1.Items.Add;


      li.caption:=ConvertKeyComboToString(hk.keys);
      li.SubItems.Add(HotkeyActionToText(hk.action));
      li.SubItems.Add(hk.value);
      li.SubItems.Add(hk.description);

      lblid.caption:=inttostr(hk.id);

      li.Data:=hk;
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
  li.SubItems.add(''); //on hotkey
  li.SubItems.add(''); //value
  li.SubItems.add(''); //description
  li.Data:=nil;
  li.selected:=true;

  edtHotkey.text:='';
  cbFreezedirection.ItemIndex:=0;
  edtFreezeValue.text:='';

  editHotkey:=true;
  listview1.Enabled:=false;

  if visible then
    edtHotkey.SetFocus;
end;

procedure THotKeyForm.BitBtn1Click(Sender: TObject);
begin
  if edithotkey then
    btnApply.click;

  close;
end;

procedure THotKeyForm.btnEditHotkeyClick(Sender: TObject);
var s: TListitem;
i: integer;
begin
  s:=listview1.selected;

  if s=nil then
  begin
    i:=listview1.itemindex;
    if i=-1 then exit;

    s:=listview1.items[i];
  end;

  if s=nil then exit;

  pagecontrol1.ActivePage:=tabsheet2;
  listview1.Enabled:=false;

  keys:=TMemoryRecordHotkey(listview1.selected.data).keys;
  edtHotkey.text:=ConvertKeyComboToString(keys);

  cbFreezedirection.ItemIndex:=cbFreezedirection.Items.IndexOf(listview1.selected.SubItems[0]);
  edtFreezeValue.text:=listview1.selected.subitems[1];
  edtDescription.text:=listview1.selected.subitems[2];

  if visible then
    edtHotkey.SetFocus;

  editHotkey:=true;
end;

procedure THotKeyForm.btnApplyClick(Sender: TObject);
var hk: TMemoryRecordHotkey;
begin
  if editHotkey and (listview1.Selected.data<>nil) then
  begin
    hk:=TMemoryRecordHotkey(listview1.Selected.data);
    hk.keys:=keys;
    hk.action:=getHotkeyAction;
    hk.value:=edtFreezeValue.text;
    hk.description:=edtDescription.text;
  end
  else
    hk:=memrec.Addhotkey(keys, getHotkeyAction, edtFreezeValue.text, edtDescription.text );

  listview1.selected.Caption:=edtHotkey.Text;
  listview1.Selected.SubItems[0]:=cbFreezedirection.Text;
  listview1.selected.subitems[1]:=edtFreezeValue.text;
  listview1.selected.subitems[2]:=edtDescription.text;
  listview1.Selected.data:=hk;


  pagecontrol1.ActivePage:=tabsheet1;
  listview1.Enabled:=true;

  edithotkey:=false;
end;

procedure THotKeyForm.btnCancelClick(Sender: TObject);
begin
  editHotkey:=false;
  if (listview1.Selected<>nil) and (listview1.selected.data=nil) then //created hotkey
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


  with cbFreezedirection.Items do
  begin
    clear;
    add(rsToggleFreeze);
    add(rsToggleFreezeAndAllowIncrease);
    add(rsToggleFreezeAndAllowDecrease);
    add(rsFreeze);
    add(rsUnfreeze);
    add(rsSetValueTo);
    add(rsDecreaseValueWith);
    add(rsIncreaseValueWith);
  end;

  cbFreezedirection.itemindex:=0;


end;

procedure THotKeyForm.FormShow(Sender: TObject);
begin
  if editHotkey then
    edtHotkey.SetFocus;
end;

procedure THotKeyForm.ListView1SelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  btnEditHotkey.enabled:=selected;


  if (listview1.selected<>nil) and (listview1.selected.data<>nil) then
    lblid.caption:=Format(rsHotkeyID, [inttostr(TMemoryRecordHotkey(listview1.selected.data).id)])
  else
    lblid.caption:='';
end;

procedure THotKeyForm.miDeleteClick(Sender: TObject);
var hk: TMemoryRecordHotkey;
begin
  if listview1.enabled and (listview1.Selected<>nil) then
  begin
    hk:=TMemoryRecordHotkey(listview1.selected.data);
    hk.free;

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
