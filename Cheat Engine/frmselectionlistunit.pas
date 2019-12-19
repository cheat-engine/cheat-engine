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
    btnOK: TButton;
    Label1: TLabel;
    procedure Edit1Change(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
  private
    { Private declarations }
    fcustomInput: boolean;
    fSelectionToText: TSelectionToTextEvent;
    fsearchbox: boolean;

    originalList: TStringlist;
    procedure setsearchbox(state: boolean);
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
    property searchbox: boolean read fsearchbox write setSearchBox;
    property customInput: boolean read fcustomInput write setCustomInput;
    property SelectionToText: TSelectionToTextEvent read fSelectionToText write fSelectionToText;
  end;

function ShowSelectionList(owner: TComponent; title, caption: string; list: TStrings; var output: string; AllowCustomInput: boolean=false; SelectionToText: TSelectionToTextEvent=nil): integer;

implementation

uses math;

function ShowSelectionList(owner: TComponent; title, caption: string; list: TStrings; var output: string; AllowCustomInput: boolean=false; SelectionToText: TSelectionToTextEvent=nil): integer;
var sl: TfrmSelectionList;
begin
  sl:=TfrmSelectionList.create(owner, list);
  sl.caption:=title;
  sl.label1.Caption:=caption;

  sl.searchbox:=true;
  sl.customInput:=AllowCustomInput;
  sl.SelectionToText:=SelectionToText;
  if output<>'' then
    sl.edit1.Text:=output;

  result:=-1;
  if sl.ShowModal=mrOK then
  begin
    result:=sl.ListBox1.ItemIndex;

    if AllowCustomInput then
      output:=sl.Edit1.text
    else
    begin
      if result<>-1 then
        output:=sl.ListBox1.Items[result];
    end;

    if result<>-1 then
      result:=list.IndexOf(output);
  end;

  freeandnil(sl);
end;

constructor TfrmSelectionList.create(AOwner: TComponent; functionList: TStrings);
begin
  inherited create(AOwner);

  if functionList<>nil then
  begin
    listbox1.Items.AddStrings(functionlist);
    originallist:=tstringlist.Create;
    originallist.AddStrings(functionlist);
  end;

  clientwidth:=max(clientwidth, canvas.TextWidth('XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'));
end;


function tfrmSelectionList.getSelection: string;
begin
  result:='';
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

  if customInput or (listbox1.itemindex<>-1) then
    modalresult:=mrok;
end;

procedure TfrmSelectionList.Edit1KeyPress(Sender: TObject; var Key: char);
begin
  listbox1.OnSelectionChange:=nil;
  listbox1.ItemIndex:=-1;
  listbox1.OnSelectionChange:=ListBox1SelectionChange;
end;

procedure TfrmSelectionList.FormShow(Sender: TObject);
begin
  if fsearchbox then
    edit1.SetFocus;

  if listbox1.height<listbox1.ItemHeight*4 then
    height:=height+(listbox1.ItemHeight*4)-listbox1.height;
end;

procedure TfrmSelectionList.Edit1Change(Sender: TObject);
var
  i: integer;
  s: integer;
begin
  if edit1.Focused then
  begin
    listbox1.OnSelectionChange:=nil;

    if edit1.text='' then
    begin
      listbox1.Items.Text:=originalList.Text;
      exit;
    end;

    listbox1.Items.BeginUpdate;
    listbox1.Items.Clear;
    s:=-1;
    for i:=0 to originalList.Count-1 do
    begin
      if pos(lowercase(edit1.text),lowercase(originallist[i]))>0 then
      begin
        listbox1.items.add(originallist[i]);
        if lowercase(originallist[i])=lowercase(edit1.text) then
          s:=listbox1.items.Count-1;
      end;
    end;
    listbox1.Items.EndUpdate;

    listbox1.ItemIndex:=s;

    btnok.enabled:=customInput or (listbox1.itemindex<>-1);

    listbox1.OnSelectionChange:=ListBox1SelectionChange;
  end;
end;

procedure TfrmSelectionList.ListBox1SelectionChange(Sender: TObject;
  User: boolean);
begin
  if edit1.Focused then exit;

  edit1.OnChange:=nil;
  if listbox1.ItemIndex>=0 then
  begin
    if assigned(fSelectionToText) then
    begin
      edit1.text:=fSelectionToText(originalList.indexof(listbox1.Items[listbox1.ItemIndex]), listbox1.Items[listbox1.ItemIndex]);
    end
    else
      edit1.text:=listbox1.Items[listbox1.ItemIndex];
  end;
  edit1.OnChange:=Edit1Change;

  btnok.enabled:=customInput or (listbox1.itemindex<>-1);
end;

procedure TfrmSelectionList.setsearchbox(state: boolean);
begin
  edit1.Visible:=state;
  fsearchbox:=state;
end;

procedure TfrmSelectionList.setCustomInput(state: boolean);
begin
  searchbox:=true;
  fcustomInput:=state;
end;

initialization
  {$i frmselectionlistunit.lrs}

end.
