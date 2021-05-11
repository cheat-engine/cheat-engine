unit symbolconfigunit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, LMessages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,symbolhandler, symbolhandlerstructs, ComCtrls, ExtCtrls,
  Menus, LResources, betterControls;

type

  { TfrmSymbolhandler }

  TfrmSymbolhandler = class(TForm)
    miUnregister: TMenuItem;
    edtSymbolname: TEdit;
    Label4: TLabel;
    miUnregisterAll: TMenuItem;
    PopupMenu2: TPopupMenu;
    scImageList: TImageList;
    Label3: TLabel;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    Label2: TLabel;
    edtAddress: TEdit;
    Button1: TButton;
    Panel2: TPanel;
    Label1: TLabel;
    ListView1: TListView;
    Panel3: TPanel;
    PopupMenu1: TPopupMenu;
    Delete1: TMenuItem;
    Splitter1: TSplitter;
    tvSymbolGroups: TTreeView;
    procedure edtSymbolnameChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure miUnregisterAllClick(Sender: TObject);
    procedure miUnregisterClick(Sender: TObject);
    procedure tvSymbolGroupsDblClick(Sender: TObject);
    procedure tvSymbolGroupsExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
  private
    { Private declarations }
    updatepart: TUserdefinedSymbolCallbackPart;
    procedure SymUpdate;

  public
    { Public declarations }

    procedure refreshlist;
    procedure refreshSymbolHandlerList;
  end;

var
  frmSymbolhandler: TfrmSymbolhandler;

implementation

uses MemoryBrowserFormUnit, cefuncproc, SymbolListHandler;

resourcestring
  rsAreYouSureYouWantToRemoveThisSymbolFromTheList = 'Are you sure you want to remove this symbol from the list?';
  rsAreYouSureYouWantToRemoveAllSymbolsFromTheList = 'Are you sure you want to remove all symbols from the list?';

procedure SymbolUpdate(item: TUserdefinedSymbolCallbackPart=suUserdefinedSymbol);
begin
  if frmsymbolhandler<>nil then
  begin
    frmsymbolhandler.updatepart:=item;
    if MainThreadID=GetCurrentThreadId then
      frmsymbolhandler.SymUpdate
    else
      tthread.Queue(tthread.CurrentThread, frmsymbolhandler.SymUpdate);
  end;
end;

procedure TfrmSymbolhandler.SymUpdate;
begin
  if updatepart=suUserdefinedSymbol then refreshlist;
  if updatepart=suSymbolList then refreshSymbolHandlerList;
end;


procedure TfrmSymbolhandler.refreshlist;
var sl: tstringlist;
    i: integer;
    li: tlistitem;
    extradata: ^TUDSEnum;

begin
  listview1.Items.Clear;
  sl:=tstringlist.create;
  try
    symhandler.EnumerateUserdefinedSymbols(sl);

    for i:=0 to sl.Count-1 do
    begin
      li:=listview1.Items.Add;
      li.Caption:=sl[i];
      extradata:=pointer(sl.objects[i]);
      li.SubItems.Add(extradata^.addressstring);

      if extradata^.doNotSave=false then
      begin
        if extradata^.allocsize>0 then
          li.SubItems.Add(inttohex(dword(extradata^.allocsize),8));
      end;

      freememandnil(extradata);
    end;
  finally
    sl.free;
  end;
end;

procedure TfrmSymbolhandler.refreshSymbolHandlerList;
var
  sll: TList;
  i: integer;
  sl: TSymbolListHandler;
  name: string;
  tn: TTreenode;
begin
  sll:=TList.create;
  symhandler.GetSymbolLists(sll);

  tvSymbolGroups.BeginUpdate;
  tvSymbolGroups.Items.Clear;

  for i:=0 to sll.Count-1 do
  begin
    sl:=sll[i];
    name:=sl.name;
    if name='' then
      name:='Unnamed '+inttohex(ptruint(sl),8);

    tn:=tvSymbolGroups.Items.Add(nil,name);
    tn.HasChildren:=sl.count>0;
    tn.Data:=sl;
  end;
  tvSymbolGroups.EndUpdate;

  sll.free;
end;

procedure TfrmSymbolhandler.FormShow(Sender: TObject);
begin
  refreshlist;
  refreshSymbolHandlerList;
  panel2.Width:=listview1.Column[0].Width+listview1.Column[1].Width+listview1.Column[2].Width;
end;

procedure TfrmSymbolhandler.edtSymbolnameChange(Sender: TObject);
begin

end;

procedure TfrmSymbolhandler.FormDestroy(Sender: TObject);
begin
  SaveFormPosition(self);
  frmsymbolhandler:=nil;
end;

procedure TfrmSymbolhandler.Button1Click(Sender: TObject);
var symbolname:string;
    address: dword;
    li: tlistitem;
begin
  symbolname:=edtsymbolname.Text;

  symhandler.DeleteUserdefinedSymbol(symbolname);
  symhandler.AddUserdefinedSymbol(edtaddress.Text,symbolname);

  li:=listview1.Items.Add;
  li.Caption:=symbolname;
  li.SubItems.Add(edtaddress.Text);

  edtSymbolname.SetFocus;
  edtSymbolname.SelectAll;
end;

procedure TfrmSymbolhandler.ListView1Click(Sender: TObject);
var li: tlistitem;
begin
  if listview1.ItemIndex<>-1 then
  begin
    li:=listview1.Items[listview1.itemindex];

    edtSymbolname.Text:=li.Caption;
    if li.SubItems.Count>0 then
      edtAddress.text:=li.SubItems[0]
    else
      edtAddress.text:='';

  end;
end;

procedure TfrmSymbolhandler.FormCreate(Sender: TObject);
begin
  symhandler.RegisterUserdefinedSymbolCallback(@symbolupdate);
  LoadFormPosition(self);
end;

procedure TfrmSymbolhandler.Delete1Click(Sender: TObject);
begin
  if listview1.ItemIndex<>-1 then
  begin
    if messagedlg(rsAreYouSureYouWantToRemoveThisSymbolFromTheList, mtconfirmation, [mbyes, mbno], 0)=mryes then
      symhandler.DeleteUserdefinedSymbol(listview1.Items[listview1.ItemIndex].Caption);
  end;
end;

procedure TfrmSymbolhandler.ListView1DblClick(Sender: TObject);
begin
  //go to the selected address
  if listview1.ItemIndex<>-1 then
    MemoryBrowser.disassemblerview.TopAddress:=symhandler.GetUserdefinedSymbolByName(listview1.Items[listview1.ItemIndex].Caption);
end;

procedure TfrmSymbolhandler.MenuItem1Click(Sender: TObject);
var
  i: integer;
  list: tstringlist;
begin
  if messagedlg(rsAreYouSureYouWantToRemoveAllSymbolsFromTheList, mtconfirmation, [mbyes, mbno], 0)=mryes then
  begin
    list:=tstringlist.Create;
    for i:=0 to ListView1.items.count-1 do
      list.add(listview1.items[i].Caption);

    for i:=0 to list.count-1 do
      symhandler.DeleteUserdefinedSymbol(list[i]);

    list.free;


    refreshlist;
  end;
end;

procedure TfrmSymbolhandler.miUnregisterAllClick(Sender: TObject);
var
  i: integer;
  list: tlist;
  n: TTreenode;
begin
  list:=tlist.create;


  n:=tvSymbolGroups.items.GetFirstNode;
  while n<>nil do
  begin
    if n.data<>nil then
      list.add(n.data);

    n:=n.GetNextSibling;
  end;

  tvSymbolGroups.BeginUpdate;

  for i:=0 to list.count-1 do
  begin
    TSymbolListHandler(list[i]).unregisterList;
    if TSymbolListHandler(list[i]).refcount=0 then
      TSymbolListHandler(list[i]).free;

  end;

  tvSymbolGroups.EndUpdate;

  list.free;
end;

procedure TfrmSymbolhandler.miUnregisterClick(Sender: TObject);
var n: TTreenode;
  l: TSymbolListHandler;
begin
  if tvSymbolGroups.Selected<>nil then
  begin
    n:=tvSymbolGroups.Selected;
    while n.level>0 do n:=n.Parent;
    if n.data<>nil then
    begin
      l:=TSymbolListHandler(n.data);
      l.unregisterList;
      if l.refcount=0 then
        l.free;
    end;
  end;
end;

procedure TfrmSymbolhandler.tvSymbolGroupsDblClick(Sender: TObject);
begin
  if (tvSymbolGroups.Selected<>nil) and (tvSymbolGroups.Selected.level=1) then
    MemoryBrowser.disassemblerview.TopAddress:=ptruint(tvSymbolGroups.Selected.Data);
end;

procedure TfrmSymbolhandler.tvSymbolGroupsExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
var
  l: tstringlist;
  sl: TSymbolListHandler;
  i: integer;

  n: TTreenode;
  address: ptruint;
begin
  if node.data<>nil then
  begin
    sl:=TSymbolListHandler(node.data);
    AllowExpansion:=true;

    if node.Count=0 then
    begin
      //fill it
      l:=tstringlist.create;
      sl.GetSymbolList(l);

      for i:=0 to l.count-1 do
      begin
        address:=ptruint(l.objects[i]);
        n:=tvSymbolGroups.Items.AddChild(node,inttohex(address,8)+'-'+l[i]);
        n.data:=pointer(address);
      end;

      l.free;
    end;

  end;
end;

initialization
  {$i symbolconfigunit.lrs}

end.
