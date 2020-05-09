unit frmReferencedFunctionsUnit;

{$mode delphi}

interface

uses
  LCLProc, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, Menus, symbolhandler, Clipbrd;

type

  { TfrmReferencedFunctions }

  TfrmReferencedFunctions = class(TForm)
    FindDialog1: TFindDialog;
    miFind: TMenuItem;
    N1: TMenuItem;
    miSort: TMenuItem;
    miFilter: TMenuItem;
    pmFunctionList: TPopupMenu;
    rfImageList: TImageList;
    lbReflist: TListBox;
    lvCallList: TListView;
    MenuItem1: TMenuItem;
    PopupMenu1: TPopupMenu;
    Splitter1: TSplitter;
    procedure FindDialog1Find(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbReflistDblClick(Sender: TObject);
    procedure lvCallListColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvCallListData(Sender: TObject; Item: TListItem);
    procedure lvCallListDblClick(Sender: TObject);
    procedure lvCallListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure MenuItem1Click(Sender: TObject);
    procedure miFindClick(Sender: TObject);
    procedure miFilterClick(Sender: TObject);
    procedure miSortClick(Sender: TObject);
  private
    { private declarations }
    origCallList: Tlist;

    currentfilter: string;
    filteredList: TList;
    procedure LoadFunctionlist;
    function getList: TList;
    property callList: Tlist read getList;
  public
    { public declarations }
  end;

var
  frmReferencedFunctions: TfrmReferencedFunctions;

implementation

{$R *.lfm}

uses DissectCodeThread, MemoryBrowserFormUnit, CEFuncProc;


function TfrmReferencedFunctions.getList: TList;
begin
  if filteredList<>nil then
    result:=filteredList
  else
    result:=origCallList;
end;

procedure TfrmReferencedFunctions.FormShow(Sender: TObject);
begin
  //cleanup the stringlist
  lbRefList.items.clear;
  lvCallList.Items.Clear;
  lvCallList.Items.Count:=0;

  if dissectcode<>nil then
    LoadFunctionlist;
end;

procedure TfrmReferencedFunctions.FormCreate(Sender: TObject);
var x: array of integer;
begin
  setlength(x, 0);
  if loadformposition(self,x) then
  begin
    lvCallList.Width:=x[0];
    lvCallList.Column[0].Width:=x[1];
    lvCallList.Column[1].Width:=x[2];
  end;

  {$ifdef darwin}
  miFind.Shortcut:=TextToShortCut('Meta+F');
  {$endif}
end;


procedure TfrmReferencedFunctions.FormDestroy(Sender: TObject);
var x: array of integer;
begin
  setlength(x,3);
  x[0]:=lvCallList.Width;
  x[1]:=lvCallList.Column[0].Width;
  x[2]:=lvCallList.Column[1].Width;
  saveformposition(self,x);
end;

procedure TfrmReferencedFunctions.lbReflistDblClick(Sender: TObject);
var x: TDissectReference;
begin
  if (lbreflist.ItemIndex<>-1) and (lvCallList.Selected<>nil) and (lvCallList.Selected.Index<callList.count) then
  begin
    x:=TDissectReference(callList[lvCallList.Selected.index]);

    if lbreflist.ItemIndex<length(x.references) then
      memorybrowser.disassemblerview.SelectedAddress:=x.references[lbreflist.ItemIndex].address;
  end;
end;




function SortByAddress(Item1, Item2: pointer): Integer;
begin
  result:=TDissectReference(item1).address-TDissectReference(item2).address;
end;

function SortByReference(Item1, Item2: pointer): Integer;
begin
  result:=length(TDissectReference(item1).references)-length(TDissectReference(item2).references);

  if result=0 then result:=SortByAddress(Item1,Item2);
end;

procedure TfrmReferencedFunctions.lvCallListColumnClick(Sender: TObject;
  Column: TListColumn);
var
  i,j: integer;
begin
  //sort the callList
  if Column.Index=0 then
    callList.Sort(SortByAddress)
  else
    callList.Sort(SortByReference);

  lvCallList.Refresh;
end;


procedure TfrmReferencedFunctions.lvCallListData(Sender: TObject;
  Item: TListItem);
var
  x: TDissectReference;
begin
  if item.index<callList.Count then
  begin
    x:=callList[item.index];

    if x.addressname='' then
      x.addressname:=symhandler.getNameFromAddress(x.address);

    item.caption:=x.addressname;
    item.SubItems.add(inttostr(length(x.references)));
  end;
end;

procedure TfrmReferencedFunctions.lvCallListDblClick(Sender: TObject);
var
  x: TDissectReference;
begin
  if lvCallList.Selected<>nil then
  begin
    if lvCallList.Selected.Index<callList.count then
    begin
      x:=TDissectReference(callList.Items[lvCallList.Selected.Index]);
      memorybrowser.disassemblerview.SelectedAddress:=x.address;
    end;
  end;
end;

procedure TfrmReferencedFunctions.lvCallListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  x: TDissectReference;
  i: integer;
begin
  if selected then
  begin
    lbReflist.Clear;

    if item.index<callList.count then
    begin
      x:=TDissectReference(callList.Items[item.index]);
      for i:=0 to length(x.references)-1 do
      begin
        if x.references[i].addressname='' then
          x.references[i].addressname:=symhandler.getNameFromAddress(x.references[i].address);

        lbReflist.items.add(x.references[i].addressname);
      end;
    end;
  end;
end;

procedure TfrmReferencedFunctions.MenuItem1Click(Sender: TObject);
begin
  clipboard.AsText:=lbreflist.Items.Text;
end;

procedure TfrmReferencedFunctions.miFindClick(Sender: TObject);
begin
  finddialog1.execute;
end;

procedure TfrmReferencedFunctions.miFilterClick(Sender: TObject);
var
  i: integer;
  item: TDissectReference;
begin
  if InputQuery('Referenced functions', 'Filter',   currentfilter) then
  begin
    if filteredList<>nil then
      freeandnil(filteredList);

    if currentfilter='' then
    begin
      lvCallList.items.count:=callList.count;
      lvCallList.refresh;
      exit;
    end;


    filteredlist:=tlist.create;
    lvCallList.items.count:=0;
    for i:=0 to origCallList.count-1 do
    begin
      item:=TDissectReference(origCallList.Items[i]);
      if item.addressname='' then
        item.addressname:=symhandler.getNameFromAddress(item.address);

      if pos(currentfilter, item.addressname)>0 then
        filteredlist.Add(item);
    end;

    lvCallList.items.count:=filteredlist.count;

    lvCallList.refresh;
  end;


end;

procedure TfrmReferencedFunctions.FindDialog1Find(Sender: TObject);
var
  i: integer;
  start: integer;
  item: TDissectReference;
begin
  start:=lvCallList.itemindex;
  if start=-1 then
    start:=0;


  for i:=start to callList.Count-1 do
  begin
    item:=TDissectReference(callList[i]);
    if item.addressname='' then
      item.addressname:=symhandler.getNameFromAddress(item.address);

    if pos(finddialog1.FindText,item.addressname)>0 then
    begin
      lvCallList.itemindex:=i;
      lvCallList.items[i].MakeVisible(false);
      exit;
    end;

  end;

end;


function callistSort(Item1, Item2: Pointer): Integer;
var i1,i2: TDissectReference;
begin
  i1:=TDissectReference(Item1);
  i2:=TDissectReference(Item2);

  if i1.addressname='' then
    i1.addressname:=symhandler.getNameFromAddress(i1.address);

  if i2.addressname='' then
    i2.addressname:=symhandler.getNameFromAddress(i2.address);


  result:=CompareStr(i1.addressname, i2.addressname);
end;

procedure TfrmReferencedFunctions.miSortClick(Sender: TObject);
begin
  callList.Sort(callistSort);

  lvCallList.Refresh;
end;

procedure TfrmReferencedFunctions.LoadFunctionlist;
var i: integer;
begin
  if filteredList<>nil then
    freeandnil(filteredList);

  if origCallList<>nil then
  begin
    //cleanup
    for i:=0 to origCallList.count-1 do
      TDissectReference(origCallList[i]).free;

    origCallList.Clear;
  end
  else
    origCallList:=tlist.create;

  dissectcode.getCallList(origCallList);
  lvCallList.Items.Count:=origCallList.count;
end;

end.

