unit frmStackViewUnit;

{$mode delphi}

interface

uses
  windows, cefuncproc, newkernelhandler, Classes, SysUtils, FileUtil, LResources,
  Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, stacktrace2, Clipbrd, ComCtrls,
  strutils;

type

  { TfrmStackView }

  TfrmStackView = class(TForm)
    lvStack: TListView;
    MenuItem1: TMenuItem;
    miCopyValue: TMenuItem;
    miCopySecondary: TMenuItem;
    miCopyAddress: TMenuItem;
    miAddESP: TMenuItem;
    miAddEBP: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure lvStackDblClick(Sender: TObject);
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

uses MemoryBrowserFormUnit;

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
begin
  if lvstack.Selected<>nil then
    MemoryBrowser.hexview.address:=ptruint(lvstack.selected.data);
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
      value:=copy(s, p1+3, p2-3);
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

