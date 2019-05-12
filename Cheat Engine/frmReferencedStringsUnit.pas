unit frmReferencedStringsUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, syncobjs, CEFuncProc, NewKernelHandler,
  math, Menus, LResources;

type
  TfrmReferencedStrings=class;
  TDelayedStringfiller = class(TThread)
  private
  public
    s: tstringlist;
    csList: TCriticalSection;
    procedure execute; override;
    constructor create(suspended: boolean);
    destructor destroy; override;
  end;

  { TfrmReferencedStrings }

  TfrmReferencedStrings = class(TForm)
    rsImageList: TImageList;

    lvStringlist: TListView;
    lbReflist: TListBox;
    Splitter1: TSplitter;
    MainMenu1: TMainMenu;
    Search1: TMenuItem;
    Find1: TMenuItem;
    FindNext1: TMenuItem;
    FindDialog1: TFindDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvStringlistColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvStringlistData(Sender: TObject; Item: TListItem);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lvStringlistDblClick(Sender: TObject);
    procedure lvStringlistSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lbReflistDblClick(Sender: TObject);
    procedure Find1Click(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure FindNext1Click(Sender: TObject);
  private
    { Private declarations }
    stringlist: TStringlist; //fitting name...
    stringfiller: TDelayedStringfiller;
    procedure LoadStringlist;
  public
    { Public declarations }
  end;

var
  frmReferencedStrings: TfrmReferencedStrings;

implementation

uses DissectCodeunit, DissectCodeThread, MemoryBrowserFormUnit, ProcessHandlerUnit, Parsers;


function getStringFromAddress(address: ptrUint; var isUnicode: boolean): string;
{Reads the string at the given address}
var
  c: pchar;
  wc: pwidechar absolute c;

  s: string;
  ws: widestring;
  x: ptrUint;
  i: integer;

  notascii: boolean;

begin
  result:='-';
  isUnicode:=false;
  getmem(c,512);
  try
    if ReadProcessMemory(processhandle, pointer(address), c, 512, x) then
    begin
      //evaluate the string (is it widestring or ascii) , and place a 0 terminator when invalid char

      //test for ascii
      notascii:=false;
      for i:=0 to 511 do
        if not (c[i] in [#32..#127]) then
        begin
          if i>=4 then //long enough ascii string
          begin
            c[i]:=#0;
            s:=c;
            result:=s;
            exit;
          end;

          notascii:=true;
          break; //still here, so not an ascii string
        end;

      if not notascii then //it IS a ascii string (big one)
      begin
        c[511]:=#0;
        s:=c;
        result:=c;
        exit;
      end;

      for i:=0 to 255 do
        if not (word(wc[i]) in [32..127]) then
        begin
          wc[i]:=#0;
          ws:=wc;
          result:=ws;
          isUnicode:=true;
          exit;
        end;


    end;

  finally
    freememandnil(c);
  end;
end;

constructor TDelayedStringfiller.create(suspended: boolean);
begin
  csList:=TCriticalSection.Create;

  inherited create(suspended);
end;

destructor TDelayedStringfiller.destroy;
begin
  if csList <> nil then
    cslist.free;

  inherited destroy;
end;

procedure TDelayedStringfiller.execute;
var
  i: integer;
  x: TStringReference;
  start: integer;
begin
  start:=0;

  while (not terminated) and (start<s.count) do
  begin
    cslist.Enter;
    try
      for i:=start to min(s.Count,start+512)-1 do
      begin
        x:=TStringReference(s.Objects[i]);
        if x.s='' then
          x.s:=getStringFromAddress(x.address,x.isUnicode);
      end;
    finally
      cslist.Leave;
    end;

    sleep(10);
    inc(start,512);
  end;

end;


procedure TfrmReferencedStrings.LoadStringlist;
//checks the dissectcodethread object for the string addresses
var
  i: integer;
  x: TStringReference;
begin
  if stringfiller<>nil then
  begin
    stringfiller.Terminate;
    stringfiller.WaitFor;
    stringfiller.Free;
    stringfiller:=nil;
  end;

  if stringlist<>nil then
  begin
    for i:=0 to stringlist.Count-1 do
    begin
      x:=TStringReference(stringlist.Objects[i]);
      x.free;
      stringlist.Objects[i]:=nil;
    end;
    stringlist.Free;
  end;

  stringlist:=tstringlist.Create;

  dissectcode.getstringlist(stringlist);



  stringfiller:=tdelayedstringfiller.create(true);
  stringfiller.s:=stringlist;
  stringfiller.start;

  lvStringlist.Items.Count:=stringlist.Count;
end;

procedure TfrmReferencedStrings.FormShow(Sender: TObject);
begin
  //cleanup the stringlist
  lbRefList.items.clear;
  lvStringlist.Items.Clear;
  lvStringlist.Items.Count:=0;

  if dissectcode<>nil then
    LoadStringlist;
end;

procedure TfrmReferencedStrings.FormCreate(Sender: TObject);
var x: array of integer;
begin
  setlength(x, 0);
  if loadformposition(self,x) then
  begin
    lbReflist.Width:=x[0];
    lvStringlist.Column[0].Width:=x[1];
    lvStringlist.Column[1].Width:=x[2];
    lvStringlist.Column[2].Width:=x[3];
    lvStringlist.Column[3].Width:=x[4];
  end;
end;

procedure TfrmReferencedStrings.FormDestroy(Sender: TObject);
var x: array of integer;
begin
  setlength(x,5);
  x[0]:=lbReflist.Width;
  x[1]:=lvStringlist.Column[0].Width;
  x[2]:=lvStringlist.Column[1].Width;
  x[3]:=lvStringlist.Column[2].Width;
  x[4]:=lvStringlist.Column[3].Width;

  saveformposition(self,x);
end;

function AddressSort(List: TStringList; Index1, Index2: Integer): Integer;
begin
  result:=TStringReference(list.Objects[index1]).address-TStringReference(list.Objects[index2]).address;
end;

function StringSort(List: TStringList; Index1, Index2: Integer): Integer;
begin
  result:=CompareStr(TStringReference(list.Objects[index1]).s, TStringReference(list.Objects[index2]).s);
end;

function isUnicodeSort(List: TStringList; Index1, Index2: Integer): Integer;
var a,b: integer;
begin
  a:=ifthen(TStringReference(list.Objects[index1]).isUnicode,1,0);
  b:=ifthen(TStringReference(list.Objects[index2]).isUnicode,1,0);
  result:=a-b;
end;

function RefSort(List: TStringList; Index1, Index2: Integer): Integer;
begin
  result:=length(TStringReference(list.Objects[index1]).references)-length(TStringReference(list.Objects[index2]).references);
end;

procedure TfrmReferencedStrings.lvStringlistColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  case column.index of
    0: stringlist.CustomSort(AddressSort);
    1: stringlist.CustomSort(RefSort);
    2: stringlist.CustomSort(isUnicodeSort);
    3: stringlist.CustomSort(StringSort);
  end;

  lvStringlist.Refresh;
end;

procedure TfrmReferencedStrings.lvStringlistData(Sender: TObject;
  Item: TListItem);
var x: TStringReference;
begin
  if item.index<stringlist.Count then
  begin
    item.Caption:=stringlist[item.Index];
    x:=TStringReference(stringlist.objects[item.index]);

    item.SubItems.Add(inttostr(length(x.references)));

    stringfiller.csList.Enter;
    try
      if x.s='' then
        x.s:=getstringfromaddress(x.address,x.isUnicode);
    finally
      stringfiller.csList.Leave;
    end;

    if x.isUnicode then
      item.SubItems.Add('✔')
    else
      item.SubItems.Add('❌');

    item.SubItems.Add(x.s);

  end;
end;

procedure TfrmReferencedStrings.FormClose(Sender: TObject;
  var Action: TCloseAction);
var i: integer;
    x: TStringReference;
begin
  lbRefList.items.clear;
  lvStringlist.Items.Clear;
  lvStringlist.Items.Count:=0;

  if stringfiller<>nil then
  begin
    stringfiller.Terminate;
    stringfiller.WaitFor;
    stringfiller.free;
    stringfiller:=nil;
  end;

  if stringlist<>nil then
  begin
    for i:=0 to stringlist.Count-1 do
    begin
      x:=TStringReference(stringlist.Objects[i]);
      x.free;
      stringlist.Objects[i]:=nil;
    end;
    freeandnil(stringlist);
  end;


end;

procedure TfrmReferencedStrings.lvStringlistDblClick(Sender: TObject);
var
  x: TStringReference;
begin
  if lvstringlist.Selected<>nil then
  begin
    x:=TStringReference(stringlist.Objects[lvstringlist.Selected.Index]);
    memorybrowser.memoryaddress:=x.address;
  end;
end;

procedure TfrmReferencedStrings.lvStringlistSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var i: integer;
    x: TStringReference;
begin
  if selected then
  begin
    lbreflist.Clear;
    x:=TStringReference(stringlist.Objects[item.Index]);
    for i:=0 to length(x.references)-1 do
      lbreflist.Items.Add(inttohex(x.references[i],8));
  end;
end;

procedure TfrmReferencedStrings.lbReflistDblClick(Sender: TObject);
begin
  if lbreflist.ItemIndex<>-1 then
    memorybrowser.disassemblerview.SelectedAddress:=StrToQWordEx('$'+lbreflist.Items[lbreflist.ItemIndex]);
end;

procedure TfrmReferencedStrings.Find1Click(Sender: TObject);
begin
  finddialog1.Execute;
end;

procedure TfrmReferencedStrings.FindNext1Click(Sender: TObject);
begin
  if finddialog1.FindText='' then
    finddialog1.Execute
  else
    FindDialog1Find(finddialog1); //next scan
end;

procedure TfrmReferencedStrings.FindDialog1Find(Sender: TObject);
var i,startindex: integer;
begin
  startindex:=lvstringlist.ItemIndex;
  if startindex=-1 then startindex:=0;

  if frFindNext in finddialog1.Options then //start from next index
    inc(startindex);

  for i:=startindex to lvStringlist.Items.Count-1 do
  begin
    if pos(lowercase(finddialog1.FindText), lowercase(lvstringlist.Items[i].SubItems[2]))>0 then
    begin
      lvstringlist.Items[i].Selected:=true;
      lvstringlist.ItemIndex:=i;
      lvstringlist.Items[i].MakeVisible(false);
      exit;
    end;
  end;

  beep; //not found
end;

initialization
  {$i frmReferencedStringsUnit.lrs}

end.
