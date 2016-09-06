unit frmSelectionlistunit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, LResources;

type

  { TfrmSelectionList }

  TSelectionToTextEvent=function(index: integer; listText: string): string of object;

  TfrmSelectionList = class(TForm)
    Edit1: TEdit;
    ListBox1: TListBox;
    Panel1: TPanel;
    Button1: TButton;
    Label1: TLabel;
    procedure Edit1KeyPress(Sender: TObject; var Key: char);
    procedure ListBox1DblClick(Sender: TObject);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
  private
    { Private declarations }
    fcustomInput: boolean;
    fSelectionToText: TSelectionToTextEvent;
    procedure setCustomInput(state: boolean);
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
    property customInput: boolean read fcustomInput write setCustomInput;
    property SelectionToText: TSelectionToTextEvent read fSelectionToText write fSelectionToText;
  end;

function ShowSelectionList(owner: TComponent; title, caption: string; list: TStrings; var output: string; AllowCustomInput: boolean=false; SelectionToText: TSelectionToTextEvent=nil): integer;

implementation

function ShowSelectionList(owner: TComponent; title, caption: string; list: TStrings; var output: string; AllowCustomInput: boolean=false; SelectionToText: TSelectionToTextEvent=nil): integer;
var sl: TfrmSelectionList;
begin
  sl:=TfrmSelectionList.create(owner, list);
  sl.caption:=title;
  sl.label1.Caption:=caption;

  sl.customInput:=AllowCustomInput;
  sl.SelectionToText:=SelectionToText;
  if output<>'' then
    sl.edit1.Text:=output;

  result:=-1;
  if sl.ShowModal=mrOK then
  begin
    result:=sl.ListBox1.ItemIndex;
    if result=-1 then exit;

    if AllowCustomInput then
      output:=sl.Edit1.text
    else
      output:=sl.ListBox1.Items[result];
  end;

  freeandnil(sl);
end;

constructor TfrmSelectionList.create(AOwner: TComponent; functionList: TStrings);
begin
  inherited create(AOwner);

  if functionList<>nil then
    listbox1.Items.AddStrings(functionlist);
end;


function tfrmSelectionList.getSelection: string;
begin
  if fcustomInput then
    result:=edit1.text
  else
  begin
    if listbox1.itemindex<>-1 then
      result:=listbox1.items[listbox1.itemindex];
  end;
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

procedure TfrmSelectionList.Edit1KeyPress(Sender: TObject; var Key: char);
begin
  listbox1.OnSelectionChange:=nil;
  listbox1.ItemIndex:=-1;
  listbox1.OnSelectionChange:=ListBox1SelectionChange;
end;

procedure TfrmSelectionList.ListBox1SelectionChange(Sender: TObject;
  User: boolean);
begin
  if edit1.Focused then exit;

  if fcustomInput and (listbox1.ItemIndex>=0) then
  begin
    if assigned(fSelectionToText) then
    begin
      edit1.text:=fSelectionToText(listbox1.ItemIndex, listbox1.Items[listbox1.ItemIndex]);
    end
    else
      edit1.text:=listbox1.Items[listbox1.ItemIndex];
  end;
end;

procedure TfrmSelectionList.setCustomInput(state: boolean);
begin
  edit1.Visible:=state;
  fcustomInput:=state;
end;

initialization
  {$i frmselectionlistunit.lrs}

end.
