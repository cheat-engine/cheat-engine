unit frmWatchlistUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ValEdit,
  ExtCtrls, ComCtrls, Menus, Clipbrd, NewKernelHandler, commonTypeDefs,strutils,
  ProcessHandlerUnit, byteinterpreter;

type

  { TfrmWatchlist }

  TfrmWatchlist = class(TForm)
    wlImageList: TImageList;
    lvWatchlist: TListView;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem4: TMenuItem;
    miRefresh: TMenuItem;
    MenuItem3: TMenuItem;
    miPasteFromClipboard: TMenuItem;
    miCopyToClipboard: TMenuItem;
    miCopyToClipboardEx: TMenuItem;
    miDeleteItems: TMenuItem;
    miEditItem: TMenuItem;
    miAddItem: TMenuItem;
    pmWatchlist: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvWatchlistDblClick(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure miDeleteItemsClick(Sender: TObject);
    procedure miPasteFromClipboardClick(Sender: TObject);
    procedure miCopyToClipboardClick(Sender: TObject);
    procedure miCopyToClipboardExClick(Sender: TObject);
    procedure miAddItemClick(Sender: TObject);
    procedure miEditItemClick(Sender: TObject);
    procedure pmWatchlistPopup(Sender: TObject);
  private
    { private declarations }
    context: PContext;
  public
    { public declarations }
    procedure UpdateContext(c: PContext);
    procedure RefreshValues;
  end;

var
  frmWatchlist: TfrmWatchlist;

implementation

{$R *.lfm}

uses frmWatchListAddEntryUnit, CEFuncProc, symbolhandler, MemoryBrowserFormUnit;

{ TfrmWatchlist }

resourcestring
  rsUnparsable = '<Unparsable>';
  rsUnreadable = '<Unreadable>';

procedure TfrmWatchlist.UpdateContext(c: PContext);
begin
  context:=c;
  RefreshValues;
end;

procedure TfrmWatchlist.RefreshValues;
var
  i: integer;
  e: boolean;
  a: ptruint;
  t: integer;

  buf: array [0..511] of byte;
  br: SIZE_T;

  vt: TvariableType;
begin
  for i:=0 to lvWatchlist.Items.Count-1 do
  begin
    //evaluate the expression
    a:=symhandler.getAddressFromName(lvWatchlist.items[i].caption, false, e, context);

    if e then
      lvWatchlist.Items[i].subitems[0]:=rsUnparsable
    else
    begin
      br:=0;
      ReadProcessMemory(processhandle, pointer(a), @buf[0], 512, br);
      if br>0 then
      begin


        t:=integer(ptruint(lvWatchlist.items[i].Data));
        case t of
          1: vt:=vtByte;
          2: vt:=vtWord;
          3: vt:=vtDword;
          4: vt:=vtQword;
          5: vt:=vtSingle;
          6: vt:=vtDouble;
          7: vt:=vtString;
          8: vt:=vtUnicodeString;
          else
            vt:=FindTypeOfData(a, @buf[0], 512);
        end;

        lvWatchlist.Items[i].subitems[0]:='<'+inttohex(a,8)+'>'+DataToString(@buf[0], 512, vt, true);
      end
      else
        lvWatchlist.Items[i].subitems[0]:='<'+inttohex(a, 8)+'>'+rsUnreadable;
    end;
  end;


end;

procedure TfrmWatchlist.miEditItemClick(Sender: TObject);
var li: TListItem;
begin
  if lvWatchlist.ItemIndex<>-1 then
  begin
    li:=lvWatchlist.Items[lvWatchlist.ItemIndex];

    if frmwatchlistAddEntry=nil then
      frmwatchlistAddEntry:=tfrmwatchlistAddEntry.Create(self);

    frmwatchlistAddEntry.edtExpression.Text:=li.Caption;
    frmWatchListAddEntry.rgType.ItemIndex:=integer(ptruint(li.data));

    if frmwatchlistAddEntry.Showmodal=mrok then
    begin
      li.caption:=frmwatchlistAddEntry.edtExpression.Text;
      li.Data:=pointer(ptruint(frmWatchListAddEntry.rgType.ItemIndex));
    end;

    RefreshValues;
  end;
end;

procedure TfrmWatchlist.miAddItemClick(Sender: TObject);
var li: TListItem;
begin
  if frmwatchlistAddEntry=nil then
    frmwatchlistAddEntry:=tfrmwatchlistAddEntry.Create(self);

  if frmwatchlistAddEntry.Showmodal=mrok then
  begin
    li:=lvwatchlist.items.add;
    li.caption:=frmwatchlistAddEntry.edtExpression.Text;
    li.Data:=pointer(ptruint(frmWatchListAddEntry.rgType.ItemIndex));
    li.SubItems.add('');
  end;

  RefreshValues;
end;

procedure TfrmWatchlist.lvWatchlistDblClick(Sender: TObject);
begin
  if lvWatchlist.ItemIndex=-1 then
    miAddItem.click
  else
    miEditItem.click;
end;

procedure TfrmWatchlist.MenuItem3Click(Sender: TObject);
begin
  RefreshValues;
end;

procedure TfrmWatchlist.MenuItem4Click(Sender: TObject);
var
  a: ptruint;
  e: boolean;

begin
  if lvWatchlist.ItemIndex<>-1 then
  begin
    a:=symhandler.getAddressFromName(lvWatchlist.items[lvWatchlist.ItemIndex].caption, false, e, context);
    if not e then
    begin
      memorybrowser.hexview.Address:=a;
      memorybrowser.show;
    end;
  end;
end;

procedure TfrmWatchlist.miDeleteItemsClick(Sender: TObject);
var i: integer;
begin
  for i:=lvWatchlist.items.Count-1 downto 0 do
    if lvWatchlist.items[i].Selected then
      lvWatchlist.Items[i].Delete;
end;

procedure TfrmWatchlist.miPasteFromClipboardClick(Sender: TObject);
var
  i: integer;
  s: TStringList;
  li: TListItem;
  t: string;
  tp: integer;
begin
  s:=TStringList.create;
  s.text:=Clipboard.AsText;

  for i:=0 to s.Count-1 do
  begin
    t:=copy(s[i], rpos(':', s[i]), length(s[i]));
    try
      tp:=StrToInt(t);

      li:=lvWatchlist.Items.Add;
      li.caption:=copy(s[i], 1, rpos(':', s[i])-1);
      li.data:=pointer(ptruint(tp));
      li.SubItems.add('');
    except
    end;
  end;

  s.free;

  RefreshValues;
end;

procedure TfrmWatchlist.miCopyToClipboardClick(Sender: TObject);
var
  i: integer;
  s: TStringList;
begin
  s:=TStringList.create;
  for i:=0 to lvWatchlist.Items.Count-1 do
    if lvWatchlist.Items[i].Selected then
      s.add(lvWatchlist.items[i].Caption+':'+inttostr(ptruint(lvWatchlist.items[i].data)));

  clipboard.astext:=s.text;
  s.free;

end;

procedure TfrmWatchlist.miCopyToClipboardExClick(Sender: TObject);
var
  i: integer;
  s: TStringList;
begin
  s:=TStringList.create;
  for i:=0 to lvWatchlist.Items.Count-1 do
    if lvWatchlist.Items[i].Selected then
      s.add(lvWatchlist.items[i].Caption+' - '+lvWatchlist.items[i].SubItems[0]);

  clipboard.astext:=s.text;
  s.free;
end;

procedure TfrmWatchlist.FormCreate(Sender: TObject);
begin
  LoadFormPosition(self);
end;

procedure TfrmWatchlist.FormDestroy(Sender: TObject);
begin
  SaveFormPosition(self);
end;

procedure TfrmWatchlist.FormShow(Sender: TObject);
begin
  lvWatchlist.Column[0].AutoSize:=true;
  lvWatchlist.Column[0].AutoSize:=false;
end;



procedure TfrmWatchlist.pmWatchlistPopup(Sender: TObject);
begin
  miEditItem.enabled:=lvWatchlist.ItemIndex<>-1;
  miDeleteItems.Enabled:=lvWatchlist.SelCount>0;
end;

end.

