unit frmDebugSymbolStructureListUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, symbolhandlerstructs, betterControls;

type

  { TfrmDebugSymbolStructureList }

  TfrmDebugSymbolStructureList = class(TForm)
    btnSelect: TButton;
    btnSearch: TButton;
    edtSearch: TEdit;
    lblCount: TLabel;
    Label2: TLabel;
    lvStructlist: TListView;
    procedure btnSearchClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvStructlistData(Sender: TObject; Item: TListItem);
    procedure lvStructlistDblClick(Sender: TObject);
  private
    foriginallist: tstringlist;
    fcurrentlist: tstringlist;
    procedure setList(l: TStringlist);
    function getList: TStringlist;
    function getSelected: TDBStructInfo;
    function getSelectedText: string;
  public
    property list: TStringList read getList write setList;
    property Selected: TDBStructInfo read getSelected;
    property SelectedText: string read getSelectedText;
  end;

implementation

{ TfrmDebugSymbolStructureList }

resourcestring
  rsThereAreStructuresInThisList = 'There are %d structures in this list';

procedure TfrmDebugSymbolStructureList.setList(l: TStringlist);
begin
  foriginallist:=l;
  lblCount.caption:=Format(rsThereAreStructuresInThisList, [l.Count]);
  lvStructlist.items.count:=l.Count;
end;

function TfrmDebugSymbolStructureList.getList: TStringlist;
begin
  if fcurrentlist<>nil then
    result:=fcurrentlist
  else
    result:=foriginallist;
end;

function TfrmDebugSymbolStructureList.getSelectedText: string;
begin
  result:='';
  if lvStructlist.itemindex<>-1 then
    result:=list[lvStructlist.itemindex];
end;

function TfrmDebugSymbolStructureList.getSelected:TDBStructInfo;
begin
  result:=nil;
  if lvStructlist.itemindex<>-1 then
    result:=TDBStructInfo(list.Objects[lvStructlist.itemindex]);
end;

procedure TfrmDebugSymbolStructureList.lvStructlistData(Sender: TObject;
  Item: TListItem);
begin
  if list<>nil then
    item.caption:=list[item.index]
  else
    item.caption:='?';
end;

procedure TfrmDebugSymbolStructureList.lvStructlistDblClick(Sender: TObject);
begin
  if lvStructlist.itemindex<>-1 then
    modalresult:=mrok;
end;

procedure TfrmDebugSymbolStructureList.FormShow(Sender: TObject);
var
  minwidth, minheight: integer;
  needed: integer;
begin
  autosize:=false;
  minwidth:=canvas.GetTextWidth('somemodule.someverydescriptiveclassname');

  minheight:=canvas.GetTextHeight('qqWwEeRrTtYy')*15;

  if width<minwidth then width:=minwidth;

  if lvStructlist.Height<minheight then
  begin
    needed:=minheight-lvStructlist.Height;

    height:=height+needed;
  end;
end;

procedure TfrmDebugSymbolStructureList.btnSearchClick(Sender: TObject);
var i: integer;
begin
  if trim(edtSearch.text)='' then
  begin
    if fcurrentlist<>nil then
      freeandnil(fcurrentlist);

    lvStructlist.items.count:=list.Count;
    exit;
  end;

  if fcurrentlist=nil then
    fcurrentlist:=tstringlist.create;

  fcurrentlist.clear;



  for i:=0 to foriginallist.Count-1 do
  begin
    if pos(lowercase(edtSearch.text), lowercase(foriginallist[i]))>0 then
      fcurrentlist.AddObject(foriginallist[i], foriginallist.Objects[i]);
  end;

  lvStructlist.items.count:=list.Count;
end;

initialization
  {$I frmDebugSymbolStructureListUnit.lrs}

end.

