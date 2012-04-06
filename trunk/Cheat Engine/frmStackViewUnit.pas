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
    lvStack: TListView;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    miCopyValue: TMenuItem;
    miCopySecondary: TMenuItem;
    miCopyAddress: TMenuItem;
    miAddESP: TMenuItem;
    miAddEBP: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvStackDblClick(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure miAddESPClick(Sender: TObject);
    procedure miCopyAddressClick(Sender: TObject);
  private
    { private declarations }
    c: PContext;
    stack: pbyte;
    size: integer;
  public
    { public declarations }
    procedure SetContextPointer(c: PContext; stack: pbyte; size: integer);
  end; 

var
  frmStackView: TfrmStackView;

implementation

uses MemoryBrowserFormUnit, StructuresFrm2;

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

  for i:=0 to frmStructures2.Count-1 do
    s.add(TfrmStructures2(frmStructures2).Caption);

  s.add('<New window>');

  f:=TfrmSelectionList.Create(self, s);

  f.caption:='Lock and add to structure dissect';
  f.label1.Caption:='Select the structure dissect window you wish to add this region to';

  if f.showmodal=mrok then
  begin
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
      structurefrm.Definenewstructure1.click;

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
  end;
end;

initialization
  {$I frmStackViewUnit.lrs}

end.

