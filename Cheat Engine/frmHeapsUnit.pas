unit frmHeapsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls,tlhelp32,cefuncproc;

type TFillHeapEntryList=class(tthread)
  private
    c: integer;
    list: array [0..14] of string;
    expanded: boolean;
    procedure updatelist;
  public
    node: ttreenode;
    procedure execute; override;
end;

type
  TfrmHeaps = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    TreeView1: TTreeView;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure TreeView1DblClick(Sender: TObject);
    procedure TreeView1Expanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
  private
    { Private declarations }
    SNAPHandle: THandle;
  public
    { Public declarations }
    procedure Getheap;
  end;

var
  frmHeaps: TfrmHeaps;

implementation

{$R *.dfm}

uses memorybrowserformunit;

procedure TFillHeapEntryList.updatelist;
var i:integer;
begin
  if frmheaps<>nil then
  begin
    with frmheaps do
    begin
      treeview1.Items.BeginUpdate;
      for i:=0 to c-1 do
        treeview1.items.addchild(node,list[i]);

      if expanded and (not node.Expanded) then
      begin
        terminate;
        node.DeleteChildren;
        node.HasChildren:=true;
        node.data:=nil;
      end;

      if not expanded then
      begin
        expanded:=true;
        node.Expand(false);
      end;

      treeview1.Items.EndUpdate;
    end;
  end else terminate;

  c:=0;
end;

procedure TFillHeapEntryList.execute;
var check: boolean;
    HeapEntry: HEAPENTRY32;
    id: dword;
    i: integer;
begin
  freeonterminate:=true;

  id:=strtoint('$'+node.text);

  zeromemory(@heapentry,sizeof(heapentry));
  heapentry.dwSize:=sizeof(heapentry);

  c:=0;
  check:=Heap32First(HeapEntry,processid,id);
  while check and (not terminated) do
  begin
    list[c]:=IntTohex(heapentry.dwAddress,8)+' - '+IntToHex(heapentry.dwAddress+heapentry.dwBlockSize,8);
    inc(c);
    if c=15 then
      synchronize(updatelist);

    check:=Heap32Next(heapentry);
  end;

  if c>0 then
    synchronize(updatelist);
end;

procedure TfrmHeaps.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  action:=caFree;
  frmheaps:=nil;
  closehandle(snaphandle);
end;

procedure TFrmHeaps.getheap;
var
    Heaplist: HEAPLIST32;
    check: boolean;
    x: TTreenode;
begin
  treeview1.Items.Clear;

  if snaphandle>0 then
  begin
    Heaplist.dwSize:=sizeof(Heaplist);
    Heaplist.th32ProcessID:=Processid;
    check:=Heap32ListFirst(SNAPHandle,heaplist);

    while check do
    begin
      treeview1.Items.add(nil,IntToHex(heaplist.th32HeapID,8)).haschildren:=true;
      check:=Heap32ListNext(SNAPHandle,heaplist);
    end;
  end;
end;

procedure TfrmHeaps.FormCreate(Sender: TObject);
begin
  SNAPHandle:=CreateToolhelp32Snapshot(TH32CS_SNAPHEAPLIST,ProcessID);
  getheap;
end;

procedure TfrmHeaps.Button1Click(Sender: TObject);
begin
  close;
end;

procedure TfrmHeaps.TreeView1DblClick(Sender: TObject);
var
  tnode: ttreenode;
  e: integer;
begin
  tnode:=treeview1.selected;

  if (tnode<>nil) and (tnode.count=0) and (integer(tnode.Data)<>-1) then
  begin
    if tnode.level=1 then
    begin
      val('$'+tnode.Text,memorybrowser.memoryaddress,e);
      memorybrowser.RefreshMB;
    end
    else
    begin
      //not (being) filled
      tnode.Data:=pointer(-1);
      with TFillHeapEntryList.create(true) do
      begin
        node:=tnode;
        resume;
      end;
    end;

  end;
end;

procedure TfrmHeaps.TreeView1Expanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  allowexpansion:=true;
  treeview1.Selected:=node;
  treeview1DblClick(sender);
end;

end.
