unit frmgroupscanalgoritmgeneratorunit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, CustomTypeHandler, math, strutils, cefuncproc, groupscancommandparser;

type
  { TfrmGroupScanAlgoritmGenerator }
  TfrmGroupScanAlgoritmGenerator = class(TForm)
    Button1: TButton;
    Button2: TButton;
    cbTypeAligned: TCheckBox;
    cbOutOfOrder: TCheckBox;
    edtBlockalignment: TEdit;
    edtBlocksize: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblMin: TLabel;
    ScrollBox1: TScrollBox;
    procedure Button1Click(Sender: TObject);
    procedure cbOutOfOrderChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    Varinfolist: TList;
    procedure sizechange;
  public
    { public declarations }

    procedure parseParameters(p:string);
    function getParameters: string;


    procedure AddLine(valuetype: TVariableType; value: string);
  end;


  TVariableInfo=class(TPanel)
  private
    frm: TfrmGroupScanAlgoritmGenerator;
    cbVartype: TCombobox;
    edtValue: Tedit;
    procedure vartypeselect(Sender: TObject);
  public
    function getParameterPart: string;
    function bytesize: integer;
    procedure setPosition;
    constructor Create(frm: TfrmGroupScanAlgoritmGenerator);
    destructor destroy; override;
  end;


var
  frmGroupScanAlgoritmGenerator: TfrmGroupScanAlgoritmGenerator;

implementation

{$R *.lfm}

function TVariableInfo.bytesize: integer;
var i: integer;
  ct: TCustomType;
begin
  result:=0;
  i:=cbVartype.ItemIndex;
  if i<>0 then
  begin
    ct:=Tcustomtype(cbVartype.Items.Objects[i]);
    if ct<>nil then
      result:=ct.bytesize
    else
    begin
      case i of
        1: result:=1;
        2: result:=2;
        3: result:=4;
        4: result:=8;
        5: result:=4;
        6: result:=8;
      end;

    end;
  end;
end;

function TVariableInfo.getParameterPart: string;
var ct: TCustomType;
begin
  result:='';
  if edtValue.text='' then exit;

  case cbVartype.itemindex of
    0: exit;
    1: result:='1:';
    2: result:='2:';
    3: result:='4:';
    4: result:='8:';
    5: result:='f:';
    6: result:='d:';
    else
    begin
      //custom
      ct:=Tcustomtype(cbVartype.Items.Objects[cbVartype.itemindex]);
      if ct<>nil then
        result:='ct('+ct.name+'):'
      else
        exit;
    end;
  end;

  result:=result+edtValue.text;
end;

procedure TVariableInfo.vartypeselect(Sender: TObject);
begin

  if cbVartype.ItemIndex<>0 then
  begin
    //check if this was the last item in the list
    if frm.Varinfolist.IndexOf(self)=frm.Varinfolist.count-1 then
      TVariableInfo.create(frm); //add a new line
  end;

  edtValue.visible:=cbVartype.ItemIndex<>0;

  frm.sizechange;

end;

procedure TVariableInfo.setPosition;
var p: integer;
  previous: TVariableInfo;
begin
  p:=frm.Varinfolist.IndexOf(self);
  if p=0 then
  begin
    //place at top
    top:=0;
    left:=0;
  end
  else
  begin
    //place before the previous one
    previous:=frm.Varinfolist[p-1];
    if previous<>nil then
    begin
      top:=previous.Top+previous.Height;
      left:=0;
    end;
  end;
end;

constructor TVariableInfo.Create(frm: TfrmGroupScanAlgoritmGenerator);
var i: integer;
begin
  inherited create(frm);
  AutoSize:=false;

  i:=frm.Varinfolist.add(self);
  self.frm:=frm;

  parent:=frm.ScrollBox1;


  //lazarus bug, once scrolled down the clientsizewithbar will be different
  if i=0 then
    width:=frm.ScrollBox1.ClientWidth-frm.scrollbox1.VertScrollBar.Size-6
  else
    width:=TVariableInfo(frm.Varinfolist[0]).width;


  bevelouter:=bvNone;
  BorderStyle:=bsNone;
  left:=0;



  edtValue:=Tedit.create(self);
  cbVartype:=Tcombobox.create(self);

  cbvartype.Items.Add('');
  cbvartype.Items.Add('1 Byte');
  cbvartype.Items.Add('2 Bytes');
  cbvartype.Items.Add('4 Bytes');
  cbvartype.Items.Add('8 Bytes');
  cbvartype.Items.Add('Float');
  cbvartype.Items.Add('Double');

  for i:=0 to customTypes.count-1 do
    cbVartype.items.AddObject(TCustomType(customtypes[i]).name, customtypes[i]);

  cbvartype.Style:=csDropDownList;

  cbVartype.width:=(clientwidth div 2)-3;
  edtValue.width:=(clientwidth div 2)-5;

  cbVartype.left:=0;
  edtValue.Left:=(clientwidth div 2)+3;


  clientheight:=max(cbVartype.height, edtValue.height)+2;


  cbvartype.parent:=self;
  edtValue.parent:=self;

  cbvartype.itemindex:=0;
  cbvartype.OnChange:=vartypeselect;

  edtValue.visible:=false;

  setPosition;
end;

destructor TVariableInfo.destroy;
var i: integer;
  p: integer;
begin
  p:=frm.Varinfolist.IndexOf(self);
  frm.Varinfolist.remove(self);

  //update the position of all the items that came after it
  if p<>-1 then
  begin
    for i:=p to frm.Varinfolist.count-1 do
      TVariableInfo(frm.varinfolist[i]).setPosition;
  end;

  if cbVartype<>nil then
    freeandnil(cbVartype);

  if edtValue<>nil then
    freeandnil(edtValue);

  inherited destroy;
end;

{ TfrmGroupScanAlgoritmGenerator }

procedure TfrmGroupScanAlgoritmGenerator.FormCreate(Sender: TObject);
begin
  Varinfolist:=TList.Create;

  autosize:=false;

  //create the initial variableinfo
  TVariableInfo.create(self);
end;

procedure TfrmGroupScanAlgoritmGenerator.cbOutOfOrderChange(Sender: TObject);
begin
  lblMin.visible:=cbOutOfOrder.checked;
  cbTypeAligned.enabled:=cbOutOfOrder.checked;
  edtBlocksize.enabled:=cbOutOfOrder.checked;
  sizechange;
end;

procedure TfrmGroupScanAlgoritmGenerator.Button1Click(Sender: TObject);
begin
  getparameters; //test for validity

  modalresult:=mrok; //still alive so no exception
end;

procedure TfrmGroupScanAlgoritmGenerator.FormDestroy(Sender: TObject);
var i: integer;
begin
  if Varinfolist<>nil then
  begin
    while varinfolist.count>0 do
      TVariableInfo(varinfolist[0]).free;

    freeandnil(Varinfolist);
  end;
end;

procedure TfrmGroupScanAlgoritmGenerator.FormShow(Sender: TObject);
begin

end;

procedure TfrmGroupScanAlgoritmGenerator.sizechange;
var i: integer;
  s: integer;
begin
  s:=0;
  for i:=0 to varinfolist.count-1 do
    s:=s+TVariableInfo(varinfolist[i]).bytesize;

  if not cbOutOfOrder.checked then
  begin
    //make sure blocksize is AT LEAST
    edtBlocksize.Text:=inttostr(s);
  end
  else
  begin
    lblMin.Caption:='Should be at least '+inttostr(s);
  end;
end;


procedure TfrmGroupScanAlgoritmGenerator.AddLine(valuetype: TVariableType; value: string);
var x: TVariableInfo;
begin
  x:=TVariableInfo(Varinfolist[Varinfolist.count-1]);
  case valuetype of
    vtByte :  x.cbVartype.itemindex:=1;
    vtWord :  x.cbVartype.itemindex:=2;
    vtDword:  x.cbVartype.itemindex:=3;
    vtQword:  x.cbVartype.itemindex:=4;
    vtSingle: x.cbVartype.itemindex:=5;
    vtDouble: x.cbVartype.itemindex:=6;
    vtPointer: if processhandler.is64Bit then x.cbVartype.itemindex:=4 else x.cbVartype.itemindex:=3;
  end;

  x.vartypeselect(x.cbVartype);
  x.edtValue.text:=value;
end;

procedure TfrmGroupScanAlgoritmGenerator.parseParameters(p:string);
var i,j: integer;
  command, value: string;
  x: TVariableInfo;

  ctn: string;
  c: TCustomType;

  gcp: TGroupscanCommandParser;
begin
  gcp:=TGroupscanCommandParser.create(p);
  for i:=0 to length(gcp.elements)-1 do
  begin
    x:=TVariableInfo(Varinfolist[Varinfolist.count-1]);
    case gcp.elements[i].vartype of
      vtByte: x.cbVartype.itemindex:=1;
      vtWord: x.cbVartype.itemindex:=2;
      vtDword: x.cbVartype.itemindex:=3;
      vtQword: x.cbVartype.itemindex:=4;
      vtSingle: x.cbVartype.itemindex:=5;
      vtDouble: x.cbVartype.itemindex:=6;
      vtCustom: x.cbVartype.ItemIndex:=x.cbVartype.Items.IndexOf(gcp.elements[i].customtype.name);
    end;

    x.vartypeselect(x.cbVartype);
    x.edtValue.text:=gcp.elements[i].uservalue;
  end;

end;

function TfrmGroupScanAlgoritmGenerator.getParameters: string;
var
  bs: integer;
  i: integer;
  vi: TVariableInfo;
  s: string;
  ba: integer;
begin
  result:='';
  try

    ba:=strtoint(edtBlockalignment.text);
    if ba<>4 then
      result:=result+'BA:'+inttostr(ba);


    if cbOutOfOrder.checked then
    begin
      bs:=strtoint(edtBlocksize.text);
      result:=result+'BS:'+inttostr(bs)+' ';

      result:=result+'OOO:';
      if cbTypeAligned.checked then
        result:=result+'A'
      else
        result:=result+'U';
    end;

    for i:=0 to Varinfolist.count-1 do
    begin
      vi:=TVariableInfo(varinfolist[i]);
      s:=vi.getParameterPart;
      if s<>'' then
        result:=result+s+' ';
    end;


  except
    raise exception.create('blocksize must be provided');
  end;

end;

end.

