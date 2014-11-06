unit frmSelectionlistunit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, LResources;

type

  { TfrmSelectionList }

  TfrmSelectionList = class(TForm)
    ListBox1: TListBox;
    Panel1: TPanel;
    Button1: TButton;
    Label1: TLabel;
    procedure ListBox1DblClick(Sender: TObject);
  private
    { Private declarations }
    function getSelectedIndex: integer;
    procedure setSelectedIndex(i: integer);
    function getItem(index: integer): string;
    function getSelection: string;
  public
    { Public declarations }
    constructor create(AOwner: TComponent; functionList: TStrings);  overload;
    property itemindex: integer read getselectedindex write setSelectedIndex;
    property items[index: integer]: string read getItem;
    property selected: string read getSelection;

  end;

implementation


constructor TfrmSelectionList.create(AOwner: TComponent; functionList: TStrings);
begin
  inherited create(AOwner);

  if functionList<>nil then
    listbox1.Items.AddStrings(functionlist);
end;

function tfrmSelectionList.getSelection: string;
begin
  if listbox1.itemindex<>-1 then
    result:=listbox1.items[listbox1.itemindex];
end;

function TfrmSelectionList.getItem(index: integer):string;
begin
  if (index<0) or (index>=listbox1.Count) then
  begin
    result:='';
    exit;
  end else result:=listbox1.Items[index];

end;

function TfrmSelectionList.getSelectedIndex: integer;
begin
  result:=listbox1.ItemIndex;
end;

procedure tfrmselectionlist.setSelectedIndex(i: integer);
begin
  if i<listbox1.items.count then
    listbox1.itemindex:=i;
end;

procedure TfrmSelectionList.ListBox1DblClick(Sender: TObject);
begin
  if (listbox1.itemindex=-1) and (listbox1.items.count>0) then
    listbox1.itemindex:=0;

  modalresult:=mrok;
end;

initialization
  {$i frmselectionlistunit.lrs}

end.
