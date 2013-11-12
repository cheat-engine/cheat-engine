unit frmStackViewUnit;

{$mode delphi}

interface

uses
  windows, cefuncproc, newkernelhandler, Classes, SysUtils, FileUtil, LResources,
  Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, stacktrace2, Clipbrd, ComCtrls,
  strutils, frmSelectionlistunit;

type

  { TfrmStackView }

  TfrmStackView = class(TForm)
    FindDialog1: TFindDialog;
    lvStack: TListView;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    miFindNext: TMenuItem;
    miFind: TMenuItem;
    miLockAndTrace: TMenuItem;
    miCopyValue: TMenuItem;
    miCopySecondary: TMenuItem;
    miCopyAddress: TMenuItem;
    miAddESP: TMenuItem;
    miAddEBP: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure FindDialog1Find(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvStackDblClick(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure miFindClick(Sender: TObject);
    procedure miLockAndTraceClick(Sender: TObject);
    procedure miAddESPClick(Sender: TObject);
    procedure miCopyAddressClick(Sender: TObject);
  private
    { private declarations }
    c: PContext;
    stack: pbyte;
    size: integer;

    allocs: tlist;
  public
    { public declarations }
    procedure SetContextPointer(c: PContext; stack: pbyte; size: integer);
  end; 

var
  frmStackView: TfrmStackView;

implementation

uses MemoryBrowserFormUnit, StructuresFrm2, frmstacktraceunit;

procedure TfrmStackView.miAddESPClick(Sender: TObject);
begin
  SetContextPointer(c, stack, size);
end;

procedure TfrmStackView.miCopyAddressClick(Sender: TObject);
var i: integer;
    s: tstringlist;
    part: integer;
begin
  part:=TMenuItem(sender).Tag;
  s:=tstringlist.create;
  try
    for i:=0 to lvstack.items.count-1 do
    begin
      if lvstack.items[i].Selected then
      begin
        if part=0 then
          s.add(lvstack.items[i].caption)
        else
          s.add(lvstack.items[i].SubItems[part-1]);
      end;
    end;

    Clipboard.AsText:=s.text;
  finally
    s.free;
  end;
end;

procedure TfrmStackView.lvStackDblClick(Sender: TObject);
var p: tpoint;
    a: ptruint;
begin
  if lvstack.Selected<>nil then
  begin
    //get tht column that is clicked
    p:=lvStack.ScreenToClient(mouse.CursorPos);

    a:=ptruint(lvstack.selected.data);

    if p.x>lvStack.Column[0].Width then
      a:=StrToQWord('$'+lvstack.Selected.SubItems[0]);

    if not MemoryBrowser.visible then
      MemoryBrowser.visible:=true;

    if p.x>lvStack.column[1].width then
      MemoryBrowser.disassemblerview.SelectedAddress:=a
    else
      MemoryBrowser.hexview.address:=a;
  end;
end;

procedure TfrmStackView.MenuItem3Click(Sender: TObject);
var
  i: integer;
  s: tstringlist;
  f: TfrmSelectionList;

  structurefrm: TfrmStructures2;
  new: boolean;

begin
  //find out which data dissect windows are open
  s:=tstringlist.create;

  if frmStructures2=nil then
    raise exception.create('The structures list is broken');

  for i:=0 to frmStructures2.Count-1 do
    s.add(TfrmStructures2(frmStructures2[i]).Caption);

  s.add('<New window>');

  f:=TfrmSelectionList.Create(self, s);

  f.caption:='Lock and add to structure dissect';
  f.label1.Caption:='Select the structure dissect window you wish to add this region to';

  if f.showmodal=mrok then
  begin
    if f.itemindex=-1 then f.itemindex:=0;

    if f.itemindex>=frmStructures2.Count then       //new window
    begin
      structurefrm:=tfrmstructures2.create(application);
      structurefrm.show;
    end
    else
      structurefrm:=TfrmStructures2(frmStructures2[f.itemindex]);

    //add this esp (c.rsp/esp) as locked address

    structurefrm.addLockedAddress({$ifdef cpu64}c.rsp{$else}c.esp{$endif}, stack,size);

    structurefrm.show;

    if structurefrm.mainStruct=nil then //if no structure is selected define it then
      structurefrm.DefineNewStructure(size);

  end;

end;

procedure TfrmStackView.miFindClick(Sender: TObject);
begin
  finddialog1.execute;
end;

procedure TfrmStackView.miLockAndTraceClick(Sender: TObject);
var
  x: dword;
  alloc: pointer;

  f: TfrmStacktrace;
begin

  alloc:=VirtualAllocEx(processhandle, nil, size+1, MEM_COMMIT or MEM_RESERVE, PAGE_READWRITE);
  if alloc<>nil then
  begin
    if allocs=nil then
      allocs:=tlist.create;

    //copy the original bytes to the copy
    WriteProcessMemory(processhandle, pointer(alloc), stack, size, x);

    allocs.Add(alloc);

    f:=TfrmStacktrace.create(application);
    f.shadowstacktrace(c^, alloc, size);
    f.show;
  end;

end;

procedure TfrmStackView.FormDestroy(Sender: TObject);
begin

end;

procedure TfrmStackView.FormShow(Sender: TObject);
var x: array of integer;
begin
  setlength(x,3);
  if LoadFormPosition(self, x) then
  begin
    if length(x)>=3 then
    begin
      lvstack.Column[0].width:=x[0];
      lvstack.Column[1].width:=x[1];
      lvstack.Column[2].width:=x[2];
    end;
  end;
end;

procedure TfrmStackView.FormCreate(Sender: TObject);
begin

end;

procedure TfrmStackView.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveFormPosition(self, [lvstack.Column[0].Width, lvstack.Column[1].Width, lvstack.Column[2].Width ]);
end;

procedure TfrmStackView.FindDialog1Find(Sender: TObject);
var
  i: integer;
  s: string;
begin
  miFindNext.enabled:=true;

  s:=lowercase(finddialog1.FindText);
  for i:=lvStack.ItemIndex+1 to lvStack.Items.Count-1 do
  begin
    if (pos(s, lowercase(lvStack.Items[i].Caption))>0) or
       (pos(s, lowercase(lvStack.Items[i].SubItems[0]))>0) or
       (pos(s, lowercase(lvStack.Items[i].SubItems[1]))>0) then
    begin
      lvStack.ItemIndex:=i;
      lvStack.Items[lvStack.ItemIndex].MakeVisible(false);
      exit;
    end;
  end;
end;

procedure TfrmStackView.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var r: TModalResult;
  i: integer;
begin
  canclose:=true;

  if allocs<>nil then
  begin
    r:=messagedlg('This stackview window has allocated stack snapshots in the target process. Do you wish to free them?', mtconfirmation, [mbyes, mbno, mbCancel], 0);

    if not (r in [mryes, mrno]) then
    begin
      canclose:=false; //something else besides yes or no was clicked
      exit;
    end;


    if r=mryes then
    begin
      //free the allocated memory in the target process
      for i:=0 to allocs.count-1 do
        VirtualFreeEx(processhandle, allocs[i], 0, MEM_RELEASE);
    end;


    allocs.free;
    allocs:=nil;

  end;
end;

procedure TfrmStackView.SetContextPointer(c: PContext; stack: pbyte; size: integer);
var tempstringlist: tstringlist;
  p1, p2: integer;
  i: integer;
  s: string;

  address: string;
  value: string;
  secondary: string;

  li: tlistitem;
begin
  self.c:=c;
  self.stack:=stack;
  self.size:=size;

  tempstringlist:=tstringlist.create;
  lvStack.BeginUpdate;
  lvStack.Items.BeginUpdate;
  try
    ce_stacktrace(c.{$ifdef cpu64}rsp{$else}esp{$endif}, c.{$ifdef cpu64}rbp{$else}ebp{$endif}, c.{$ifdef cpu64}rip{$else}eip{$endif}, pbytearray(stack), size, tempstringlist, true,false,false,0,miAddEBP.checked);
    //now fill the listview with this information

    lvStack.Items.Clear;

    for i:=0 to tempstringlist.count-1 do
    begin
      s:=tempstringlist[i];

      p1:=posex(' - ',s);
      p2:=posex(' - ',s,p1+1);

      address:=copy(s, 1, p1);
      value:=copy(s, p1+3, p2-p1-3);
      secondary:=copy(s, p2+3, length(s));

      li:=lvStack.Items.Add;
      li.Caption:=address;
      li.SubItems.Add(value);
      li.subitems.add(secondary);
      li.Data:=pointer(tempstringlist.objects[i]);

    end;


  finally
    tempstringlist.Free;
    lvStack.items.EndUpdate;
    lvStack.EndUpdate;
  end;
end;

initialization
  {$I frmStackViewUnit.lrs}

end.

