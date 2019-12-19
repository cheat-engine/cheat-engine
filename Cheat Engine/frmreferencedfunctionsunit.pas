unit frmReferencedFunctionsUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, Menus, symbolhandler, Clipbrd;

type

  { TfrmReferencedFunctions }

  TfrmReferencedFunctions = class(TForm)
    rfImageList: TImageList;
    lbReflist: TListBox;
    lvCallList: TListView;
    MenuItem1: TMenuItem;
    PopupMenu1: TPopupMenu;
    Splitter1: TSplitter;
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
  private
    { private declarations }
    callList: Tlist;
    procedure LoadFunctionlist;
  public
    { public declarations }
  end;

var
  frmReferencedFunctions: TfrmReferencedFunctions;

implementation

{$R *.lfm}

uses DissectCodeThread, MemoryBrowserFormUnit, CEFuncProc;


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
    x:=TDissectReference(calllist[lvCallList.Selected.index]);

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
  //sort the calllist
  if Column.Index=0 then
    calllist.Sort(SortByAddress)
  else
    calllist.Sort(SortByReference);

  lvCallList.Refresh;
end;


procedure TfrmReferencedFunctions.lvCallListData(Sender: TObject;
  Item: TListItem);
var x: TDissectReference;
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

procedure TfrmReferencedFunctions.LoadFunctionlist;
var i: integer;
begin
  if callList<>nil then
  begin
    //cleanup
    for i:=0 to callList.count-1 do
      TDissectReference(callList[i]).free;

    callList.Clear;
  end
  else
    calllist:=tlist.create;

  dissectcode.getCallList(callList);
  lvCallList.Items.Count:=calllist.count;
end;

end.

