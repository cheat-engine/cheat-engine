unit frmgroupscanalgoritmgeneratorunit;

{$mode delphi}

interface

uses
  {$ifdef darwin}
  macport,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, CustomTypeHandler, math, strutils, cefuncproc, groupscancommandparser,
  vartypestrings, commonTypeDefs;

type
  { TfrmGroupScanAlgoritmGenerator }
  TfrmGroupScanAlgoritmGenerator = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    cbOutOfOrder: TCheckBox;
    cbTypeAligned: TCheckBox;
    edtBlockalignment: TEdit;
    edtBlocksize: TEdit;
    lblBlockAlignment: TLabel;
    lblBlocksize: TLabel;
    lblMin: TLabel;
    lblMustBeDividable: TLabel;
    lblWildcardExplanation: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    ScrollBox1: TScrollBox;
    procedure btnOKClick(Sender: TObject);
    procedure cbOutOfOrderChange(Sender: TObject);
    procedure cbTypeAlignedChange(Sender: TObject);
    procedure edtBlockalignmentChange(Sender: TObject);
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


    procedure AddWildcard(count: integer);
    procedure AddLine(valuetype: TVariableType; customtype: TCustomtype; value: string); overload;
    procedure AddLine(valuetype: TVariableType; value: string); overload;
  end;


  TVariableInfo=class(TPanel)
  private
    frm: TfrmGroupScanAlgoritmGenerator;
    cbVartype: TCombobox;
    edtValue: Tedit;
    cbPicked: Tcheckbox;
    procedure vartypeselect(Sender: TObject);
  public
    function getParameterPart(skipPicked: boolean=true): string;
    function bytesize: integer;
    procedure setPosition;
    constructor Create(frm: TfrmGroupScanAlgoritmGenerator);  overload;
    destructor destroy; override;
  end;


var
  frmGroupScanAlgoritmGenerator: TfrmGroupScanAlgoritmGenerator;

implementation

uses ProcessHandlerUnit;

resourcestring
  rsWildcard='Skip nr of bytes:';
  rsAdd='Add';
  rsPickedHint='When checked this element will get added to the addresslist. Note: If all checkboxes are disabled, ALL elements will be added';
  rsGSGShouldBeAtLeast = 'Should be at least %d';
  rsGSGBlocksizeMustBeProvided = 'blocksize must be provided';

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
        7: result:=length(edtvalue.text);
        8: result:=length(edtvalue.text)*2;
        9:
        begin
          try
            result:=strtoint(edtValue.text);
          except
          end;
        end;

      end;

    end;
  end;
end;

function TVariableInfo.getParameterPart(skipPicked: boolean=true): string;
var ct: TCustomType;
  p: string;
begin
  result:='';
  if edtValue.text='' then exit;

  p:='';
  if (not skipPicked) and cbPicked.checked then
    p:='p';


  case cbVartype.itemindex of
    0: exit;
    1: result:='1'+p+':';
    2: result:='2'+p+':';
    3: result:='4'+p+':';
    4: result:='8'+p+':';
    5: result:='f'+p+':';
    6: result:='d'+p+':';
    7: result:='s'+p+':''';
    8: result:='su'+p+':''';
    9: result:='w'+p+':';
    else
    begin
      //custom
      ct:=Tcustomtype(cbVartype.Items.Objects[cbVartype.itemindex]);
      if ct<>nil then
        result:='c('+ct.name+')'+p+':'
      else
        exit;
    end;
  end;

  result:=result+edtValue.text;

  if cbVartype.itemindex in [7,8] then
    result:=result+'''';

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
  cbPicked.visible:=edtValue.visible;

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
    AnchorSideTop.Control:=parent;
    AnchorSideTop.Side:=asrTop;
  end
  else
  begin
    //place before the previous one
    previous:=frm.Varinfolist[p-1];
    if previous<>nil then
    begin
      AnchorSideTop.Control:=previous;
      AnchorSideTop.Side:=asrBottom;
    end;
  end;
end;

constructor TVariableInfo.Create(frm: TfrmGroupScanAlgoritmGenerator);
var
  i: integer;
  maxwidth: integer;

  {$ifdef windows}
  cbi: TComboboxInfo;
  {$endif}
begin
  inherited create(frm);
  AutoSize:=false;

  i:=frm.Varinfolist.add(self);
  self.frm:=frm;

  parent:=frm.ScrollBox1;

  bevelouter:=bvNone;
  BorderStyle:=bsNone;
  left:=0;

  width:=10;

  autosize:=true;

  //Color:=clRed;




  AnchorSideRight.Control := frm.ScrollBox1;
  AnchorSideRight.Side := asrRight;

  AnchorSideLeft.Control:=parent;
  AnchorSideLeft.side:=asrLeft;


  Anchors := [akTop, akLeft, akRight];




  edtValue:=Tedit.create(self);
  cbVartype:=Tcombobox.create(self);

  cbvartype.Items.Add('');


  cbvartype.Items.Add(rs_vtByte);
  cbvartype.Items.Add(rs_vtWord);
  cbvartype.Items.Add(rs_vtDword);
  cbvartype.Items.Add(rs_vtQword);
  cbvartype.Items.Add(rs_vtSingle);
  cbvartype.Items.Add(rs_vtDouble);
  cbvartype.Items.Add(rs_vtString);
  cbvartype.Items.Add(rs_vtUnicodeString);
  cbvartype.Items.Add(rsWildcard);


  for i:=0 to customTypes.count-1 do
    cbVartype.items.AddObject(TCustomType(customtypes[i]).name, customtypes[i]);

  maxwidth:=0;
  for i:=0 to cbVartype.items.count-1 do
    maxwidth:=max(frm.Canvas.TextWidth(cbVartype.items[i]), maxwidth);



  cbvartype.Style:=csDropDownList;
  cbVartype.DropDownCount:=min(16,cbVartype.items.count);



  cbPicked:=TCheckBox.create(self);
  cbPicked.Caption:=rsAdd;
  cbPicked.checked:=true; //default action is yes
  cbPicked.visible:=false;
  cbPicked.parent:=self;
  cbPicked.hint:=rsPickedHint;
  cbPicked.ParentShowHint:=false;
  cbPicked.ShowHint:=true;


  cbVartype.AnchorSideLeft.Control:=self;
  cbVartype.AnchorSideLeft.Side:=asrLeft;
  cbVartype.BorderSpacing.left:=2;

  cbpicked.AnchorSideRight.Control:=self;
  cbpicked.AnchorSideRight.Side:=asrRight;
  cbpicked.AnchorSideTop.Control:=edtValue;
  cbPicked.AnchorSideTop.side:=asrCenter;
  cbpicked.BorderSpacing.Right:=2;


  edtValue.AnchorSideLeft.control:=cbVartype;
  edtValue.AnchorSideLeft.side:=asrRight;

  edtValue.AnchorSideRight.control:=cbPicked;
  edtValue.AnchorSideRight.side:=asrLeft;
  edtValue.BorderSpacing.Left:=6;
  edtValue.BorderSpacing.Right:=6;
  edtValue.BorderSpacing.Top:=1;

  cbVartype.anchors:=[akTop, akLeft];
  cbPicked.anchors:=[akTop, akRight];
  edtValue.anchors:=[akTop, akLeft, akRight];

  cbvartype.parent:=self;
  edtValue.parent:=self;
  cbpicked.parent:=self;

  cbvartype.itemindex:=0;
  cbvartype.OnChange:=vartypeselect;

  edtValue.visible:=false;


  {$ifdef windows}
  cbi.cbSize:=sizeof(cbi);
  if GetComboBoxInfo(cbVartype.Handle, @cbi) then
  begin
    i:=maxwidth-(cbi.rcItem.Right-cbi.rcItem.Left)+4;

    cbvartype.width:=cbvartype.width+i;
  end;
  {$endif}

  edtValue.Constraints.MinWidth:=frm.Canvas.TextWidth('XXXXXXXXXXX');

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

  TVariableInfo.create(self);
end;

procedure TfrmGroupScanAlgoritmGenerator.cbOutOfOrderChange(Sender: TObject);
begin
  lblMin.visible:=cbOutOfOrder.checked;
  cbTypeAligned.enabled:=cbOutOfOrder.checked;
  edtBlocksize.enabled:=cbOutOfOrder.checked;

  lblMustBeDividable.visible:=cbOutOfOrder.checked and cbTypeAligned.checked;

  sizechange;
end;

procedure TfrmGroupScanAlgoritmGenerator.cbTypeAlignedChange(Sender: TObject);
begin

end;

procedure TfrmGroupScanAlgoritmGenerator.edtBlockalignmentChange(Sender: TObject);
var i: integer;
  correct: boolean;
begin
  correct:=TryStrToInt(edtBlockalignment.text,i);
  if correct and ((i mod 4)=0) then
  begin
    lblMustBeDividable.font.color:=clWindowText
  end
  else
    lblMustBeDividable.font.color:=clRed; //error
end;

procedure TfrmGroupScanAlgoritmGenerator.btnOKClick(Sender: TObject);
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
var i: integer;
begin
//  clientheight:=panel1.top+btnOK.top+btnOK.height+10;
  autosize:=false;

  //create the initial variableinfo
  btnok.autosize:=false;
  btncancel.autosize:=false;

  i:=max(btnok.width, btncancel.width);
  btnok.width:=i;
  btncancel.width:=i;

  i:=(TVariableInfo(varinfolist[0]).cbVartype.Height*3+5)-scrollbox1.ClientHeight;
  if i>0 then
    height:=height+i+15;

  //width:=canvas.

  if Varinfolist.count>0 then //should be true
  begin
    i:=TVariableInfo(Varinfolist[0]).cbVartype.width*3;
    width:=i;
//    TVariableInfo(Varinfolist[0]).cbPicked.left+TVariableInfo(Varinfolist[0]).cbPicked.width
  end;

  position:=poDesigned;
  position:=poScreenCenter;

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
    lblMin.Caption:=format(rsGSGShouldBeAtLeast, [s]);
  end;
end;

procedure TfrmGroupScanAlgoritmGenerator.AddWildcard(count: integer);
var x: TVariableInfo;
begin
  x:=TVariableInfo(Varinfolist[Varinfolist.count-1]);
  x.cbVartype.ItemIndex:=9;
  x.vartypeselect(x.cbVartype);
  x.edtValue.text:=inttostr(count);
end;

procedure TfrmGroupScanAlgoritmGenerator.AddLine(valuetype: TVariableType; customtype: TCustomtype; value: string);
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
    vtString: x.cbVartype.ItemIndex:=7;
    vtUnicodeString: x.cbVartype.ItemIndex:=8;
    vtPointer: if processhandler.is64Bit then x.cbVartype.itemindex:=4 else x.cbVartype.itemindex:=3;
    vtCustom: x.cbVartype.ItemIndex:=x.cbVartype.Items.IndexOf(customtype.name);
  end;

  x.vartypeselect(x.cbVartype);
  x.edtValue.text:=value;
end;

procedure TfrmGroupScanAlgoritmGenerator.AddLine(valuetype: TVariableType; value: string);
begin
  addline(valuetype, nil, value);
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
      vtString:
      begin
        if gcp.elements[i].wildcard then
        begin
          x.cbVartype.itemindex:=9;
          gcp.elements[i].uservalue:=inttostr(gcp.elements[i].bytesize);
        end
        else
          x.cbVartype.ItemIndex:=7;

      end;
      vtUnicodeString: x.cbVartype.ItemIndex:=8;
      vtCustom: x.cbVartype.ItemIndex:=x.cbVartype.Items.IndexOf(gcp.elements[i].customtype.name);
    end;

    x.vartypeselect(x.cbVartype);
    x.edtValue.text:=gcp.elements[i].uservalue;

    x.cbPicked.checked:=gcp.elements[i].picked;
  end;


  edtBlocksize.Text:=inttostr(gcp.blocksize);
  edtBlockalignment.text:=inttostr(gcp.blockalignment);
  cbOutOfOrder.checked:=gcp.outOfOrder;
  cbTypeAligned.checked:=gcp.typeAligned;

  gcp.free;

end;

function TfrmGroupScanAlgoritmGenerator.getParameters: string;
var
  bs: integer;
  i: integer;
  vi: TVariableInfo;
  s: string;
  ba: integer;

  allpicked: boolean;
begin
  result:='';
  try

    ba:=strtoint(edtBlockalignment.text);
    if ba<>4 then
      result:=result+'BA:'+inttostr(ba)+' ';


    if cbOutOfOrder.checked then
    begin
      bs:=strtoint(edtBlocksize.text);
      result:=result+'BS:'+inttostr(bs)+' ';

      result:=result+'OOO:';
      if cbTypeAligned.checked then
        result:=result+'A '
      else
        result:=result+'U ';
    end;

    allpicked:=true;
    for i:=0 to varinfolist.count-1 do
    begin
      vi:=TVariableInfo(varinfolist[i]);

      if (not ((vi.cbVartype.itemindex = 0) or (vi.cbVartype.itemindex = -1))) and (vi.cbPicked.checked=false) then
        allpicked:=false;

//      if (vi.cbVartype.itemindex in [-1,0]=false) and (vi.cbPicked.checked=false) then allpicked:=false;
    end;

    for i:=0 to Varinfolist.count-1 do
    begin
      vi:=TVariableInfo(varinfolist[i]);
      s:=vi.getParameterPart(allpicked); //if all are checked you can ignore the p part
      if s<>'' then
        result:=result+s+' ';
    end;


  except
    raise exception.create(rsGSGBlocksizeMustBeProvided);
  end;

end;

end.

