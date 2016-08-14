unit CEListviewItemEditor;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, propedits, math;

type

  { TfrmCEListviewItemEditor }

  TfrmCEListviewItemEditor = class(TForm)
    btnDelete: TButton;
    btnCancel: TButton;
    btnOk: TButton;
    btnAddItem: TButton;
    btnAddSubItem: TButton;
    edtText: TEdit;
    lblText: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    TreeView1: TTreeView;
    procedure btnAddItemClick(Sender: TObject);
    procedure btnAddSubItemClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure edtTextChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure TreeView1SelectionChanged(Sender: TObject);
  private
    { private declarations }
    c: TWinControl; //the TCEListview object
  public
    procedure setControl(wc: TWincontrol);
    { public declarations }
  end;

  TCEListViewItemsPropertyEditor = Class(TClassPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

implementation

{$R *.lfm}

uses ceguicomponents;

procedure TfrmCEListviewItemEditor.btnOkClick(Sender: TObject);
var
  lv: TCEListView;
  lvi: Tlistitem;
  i: integer;

  tn: TTreenode;
begin
  //apply the changes
  lv:=TCEListView(c);

  lv.Items.Clear;

  tn:=treeview1.Items.GetFirstNode;
  while tn<>nil do
  begin
    lvi:=lv.items.add;
    lvi.caption:=tn.Text;

    i:=0;
    for i:=0 to tn.Count-1 do
      lvi.SubItems.Add(tn[i].Text);


    tn:=tn.GetNextSibling;
  end;


  modalresult:=mrok;
end;

procedure TfrmCEListviewItemEditor.edtTextChange(Sender: TObject);
begin
  if treeview1.selected<>nil then
    treeview1.Selected.text:=edtText.text;
end;

procedure TfrmCEListviewItemEditor.FormShow(Sender: TObject);
var i: integer;
begin
  i:=max(btnOk.width, btnCancel.Width);
  btnok.width:=i;
  btnCancel.width:=i;
end;

procedure TfrmCEListviewItemEditor.TreeView1Change(Sender: TObject;
  Node: TTreeNode);
begin

end;

procedure TfrmCEListviewItemEditor.TreeView1SelectionChanged(Sender: TObject);
begin
  btnAddSubItem.Enabled:=treeview1.selected<>nil;
  btnDelete.enabled:=treeview1.selected<>nil;
end;

procedure TfrmCEListviewItemEditor.btnAddItemClick(Sender: TObject);
var tn: TTreeNode;
begin
  tn:=treeview1.items.Add(nil, edtText.text);
  tn.selected:=true;
  treeview1.MakeSelectionVisible;
end;

procedure TfrmCEListviewItemEditor.btnAddSubItemClick(Sender: TObject);
var tn: TTreeNode;
begin
  tn:=treeview1.selected;


  if tn<>nil then
  begin
    while tn.Parent<>nil do
      tn:=tn.parent;

    tn:=treeview1.items.AddChild(tn, edtText.text);
    tn.selected:=true;
    treeview1.MakeSelectionVisible;
  end;
end;

procedure TfrmCEListviewItemEditor.btnDeleteClick(Sender: TObject);
begin
  if treeview1.selected<>nil then
    treeview1.Selected.Delete;
end;

procedure TfrmCEListviewItemEditor.setControl(wc: twincontrol);
var
  lv: TCEListView;
  i,j: integer;

  tn: TTreeNode;
begin
  c:=wc;

  lv:=TCEListview(wc);

  treeview1.Items.clear;

  for i:=0 to lv.items.Count-1 do
  begin
    tn:=treeview1.items.Add(nil, lv.Items[i].Caption);
    for j:=0 to lv.items[i].SubItems.Count-1 do
      treeview1.Items.AddChild(tn, lv.items[i].SubItems[j]);
  end;
end;


procedure TCEListViewItemsPropertyEditor.Edit;
var lv: TCEListview;
begin
  lv:=TCEListView(GetComponent(0));

  with TfrmCEListviewItemEditor.create(application) do
  begin
    setControl(lv);
    if showmodal=mrok then
      modified;

    free;
  end;
end;

function TCEListViewItemsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog, paReadOnly, paRevertable];
end;

end.

