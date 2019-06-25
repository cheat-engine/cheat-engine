unit symbolconfigunit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,symbolhandler, symbolhandlerstructs, ComCtrls, ExtCtrls,
  Menus, LResources;

type

  { TfrmSymbolhandler }

  TfrmSymbolhandler = class(TForm)
    edtSymbolname: TEdit;
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
    procedure edtSymbolnameChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
  private
    { Private declarations }
    procedure SymUpdate(var message:TMessage); message wm_user+1;

  public
    { Public declarations }
    procedure refreshlist;
  end;

var
  frmSymbolhandler: TfrmSymbolhandler;

implementation

uses MemoryBrowserFormUnit;

resourcestring
  rsAreYouSureYouWantToRemoveThisSymbolFromTheList = 'Are you sure you want to remove this symbol from the list?';
  rsAreYouSureYouWantToRemoveAllSymbolsFromTheList = 'Are you sure you want to remove all symbols from the list?';

procedure SymbolUpdate;
begin
  if frmsymbolhandler<>nil then
    postmessage(frmsymbolhandler.handle,wm_user+1,0,0);  //in case of multithreading
end;

procedure TfrmSymbolhandler.SymUpdate(var message: tmessage);
begin
  refreshlist;
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

      if extradata^.doNotSave=false then
      begin
        li.SubItems.Add(extradata^.addressstring);
        if extradata^.allocsize>0 then
          li.SubItems.Add(inttohex(dword(extradata^.allocsize),8));
      end;

      freememandnil(extradata);
    end;
  finally
    sl.free;
  end;
end;

procedure TfrmSymbolhandler.FormShow(Sender: TObject);
begin
  refreshlist;
  panel2.Width:=listview1.Column[0].Width+listview1.Column[1].Width+listview1.Column[2].Width;
end;

procedure TfrmSymbolhandler.edtSymbolnameChange(Sender: TObject);
begin

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
end;

procedure TfrmSymbolhandler.Delete1Click(Sender: TObject);
begin
  if listview1.ItemIndex<>-1 then
  begin
    if messagedlg(rsAreYouSureYouWantToRemoveThisSymbolFromTheList, mtconfirmation, [mbyes, mbno], 0)=mryes then
    begin
      symhandler.DeleteUserdefinedSymbol(listview1.Items[listview1.ItemIndex].Caption);
      listview1.Items[listview1.ItemIndex].Delete;
    end;
  end;
end;

procedure TfrmSymbolhandler.ListView1DblClick(Sender: TObject);
begin
  //go to the selected address
  if listview1.ItemIndex<>-1 then
    MemoryBrowser.disassemblerview.TopAddress:=symhandler.GetUserdefinedSymbolByName(listview1.Items[listview1.ItemIndex].Caption);
end;

procedure TfrmSymbolhandler.MenuItem1Click(Sender: TObject);
var i: integer;
begin
  if messagedlg(rsAreYouSureYouWantToRemoveAllSymbolsFromTheList, mtconfirmation, [mbyes, mbno], 0)=mryes then
  begin
    for i:=listview1.Items.Count-1 downto 0 do
    begin
      symhandler.DeleteUserdefinedSymbol(listview1.Items[i].Caption);
      listview1.Items[i].Delete;
    end;


    refreshlist;
  end;
end;

initialization
  {$i symbolconfigunit.lrs}

end.
