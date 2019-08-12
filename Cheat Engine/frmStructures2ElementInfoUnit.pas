unit frmStructures2ElementInfoUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, cefuncproc, StructuresFrm2, vartypestrings, math, CustomTypeHandler, commonTypeDefs;

resourcestring
  rsS2EILocalStruct = 'Local struct:';
  rsS2EIIfYouContinueTheOldLocallyDefinedType = 'If you continue the old locally defined type %s will be deleted. Continue? (Tip: You can make this type into a global type so it can be re-used over again)';
type

  { TfrmStructures2ElementInfo }

  TfrmStructures2ElementInfo = class(TForm)
    Button1: TButton;
    Button2: TButton;
    cbStructType: TComboBox;
    cbType: TComboBox;
    cbHexadecimal: TCheckBox;
    cbSigned: TCheckBox;
    cbExpandChangesAddress: TCheckBox;
    ColorDialog1: TColorDialog;
    edtByteSize: TEdit;
    edtChildstart: TEdit;
    edtDescription: TEdit;
    edtOffset: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblOffsetInto: TLabel;
    pnlBackground: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure cbHexadecimalChange(Sender: TObject);
    procedure cbSignedChange(Sender: TObject);
    procedure cbStructTypeChange(Sender: TObject);
    procedure cbTypeChange(Sender: TObject);
    procedure edtByteSizeChange(Sender: TObject);
    procedure edtChildstartChange(Sender: TObject);
    procedure edtDescriptionChange(Sender: TObject);
    procedure edtOffsetChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pnlBackgroundClick(Sender: TObject);
  private
    { private declarations }
    fOffset: integer;
    fbytesize: integer;
    Fchildstructstart: integer;
    localChild: TDissectedStruct;
    procedure setOffset(i: integer);
    function getOffset: integer;
    procedure setDescription(d: string);
    function getDescription: string;
    procedure setHexadecimal(state: boolean);
    function getHexadecimal: boolean;
    procedure setSigned(state: boolean);
    function getSigned: boolean;
    procedure setVariableType(vt: TVariableType);
    function getVariableType: TVariableType;
    procedure setCustomType(ct: TCustomType);
    function getCustomType: TCustomType;
    procedure setBytesize(i: integer);
    function getBytesize: integer;
    procedure setBackgroundColor(c: TColor);
    function getBackgroundColor: TColor;
    procedure setChildStruct(s: TDissectedStruct);
    function getChildStruct: TDissectedStruct;
    procedure setChildStructStart(o: integer);
    function getChildStructStart: integer;

    function getExpandChangedAddress: boolean;
    procedure setExpandChangedaddress(s: boolean);
    procedure FillStructureList;
  public
    { public declarations }
    ChangedDescription: boolean;
    ChangedOffset: boolean;
    ChangedHexadecimal: boolean;
    ChangedSigned: boolean;
    ChangedVartype: boolean;
    ChangedByteSize: boolean;
    ChangedBackgroundColor: boolean;
    ChangedChildStruct: boolean;
    ChangedchildStructStart: boolean;


    property description: string read getDescription write setDescription;
    property offset: integer read getOffset write setOffset;
    property hexadecimal: boolean read getHexadecimal write setHexadecimal;
    property signed: boolean read getSigned write setsigned;
    property vartype: TVariableType read getVariableType write setVariableType;
    property customtype: TCustomtype read getCustomType write setCustomtype;
    property bytesize: integer read getBytesize write setBytesize;
    property backgroundColor: TColor read getBackgroundColor write setBackgroundColor;
    property childstruct: TDissectedStruct read getChildStruct write setChildStruct;
    property childstructstart: integer read getchildstructstart write setChildStructStart;
    property ExpandChangesAddress: boolean read getExpandChangedAddress write setExpandChangedAddress;
  end; 

var
  frmStructures2ElementInfo: TfrmStructures2ElementInfo;

implementation

{$R *.lfm}

uses ProcessHandlerUnit;



{ TfrmStructures2ElementInfo }

procedure TfrmStructures2ElementInfo.setChildStructStart(o: integer);
begin
  Fchildstructstart:=o;
  edtChildstart.Text:=inttohex(o,1);
end;

procedure TfrmStructures2ElementInfo.setChildStruct(s: TDissectedStruct);
var i: integer;
begin
  if s<>nil then
    cbExpandChangesAddress.enabled:=true;

  FillStructureList;
  for i:=0 to cbStructType.items.count-1 do
    if cbStructType.Items.Objects[i]=s then
    begin
      cbStructType.ItemIndex:=i;
      exit;
    end;

  //still here so it's a "local" type
  localChild:=s;
  cbStructType.ItemIndex:=cbStructType.Items.AddObject(rsS2EILocalStruct+s.name, s);


end;

function TfrmStructures2ElementInfo.getChildStruct: TDissectedStruct;
begin
  result:=TDissectedStruct(cbStructType.Items.Objects[cbStructType.ItemIndex]);
end;

function TfrmStructures2ElementInfo.getExpandChangedAddress: boolean;
begin
  result:=cbExpandChangesAddress.checked;
end;

procedure TfrmStructures2ElementInfo.setExpandChangedaddress(s: boolean);
begin
  cbExpandChangesAddress.checked:=s;
end;

function TfrmStructures2ElementInfo.getChildstructstart: integer;
begin
  if vartype=vtPointer then
    result:=Fchildstructstart
  else
    result:=0;
end;

procedure TfrmStructures2ElementInfo.setBytesize(i: integer);
begin
  if vartype in [vtString, vtUnicodeString, vtByteArray] then
  begin
    edtByteSize.text:=inttostr(i);
    fbytesize:=i;
  end;
end;

function TfrmStructures2ElementInfo.getBytesize: integer;
begin
 // if vartype in [vtString, vtUnicodeString, vtByteArray] then

  case vartype of
    vtByte: result:=1;
    vtWord: result:=2;
    vtDWord: result:=4;
    vtQWord: result:=8;
    vtSingle: result:=4;
    vtDouble: result:=8;
    vtPointer: result:=processhandler.pointersize;
    else
      result:=fbytesize;
  end;

end;

procedure TfrmStructures2ElementInfo.setBackgroundColor(c: TColor);
begin
  pnlBackground.Color:=c;
end;

function TfrmStructures2ElementInfo.getBackgroundColor: TColor;
begin
  result:=pnlBackground.color;
end;

procedure TfrmStructures2ElementInfo.setVariableType(vt: TVariableType);
begin
  case vt of
    vtByte: cbType.ItemIndex:=0;
    vtWord: cbType.ItemIndex:=1;
    vtDWord: cbType.ItemIndex:=2;
    vtQWord: cbType.ItemIndex:=3;
    vtSingle: cbType.ItemIndex:=4;
    vtDouble: cbType.ItemIndex:=5;
    vtString: cbType.ItemIndex:=6;
    vtUnicodeString: cbType.ItemIndex:=7;
    vtByteArray: cbType.ItemIndex:=8;
    vtPointer: cbType.ItemIndex:=9;
  end;

  label2.enabled:=vt in [vtString, vtUnicodeString, vtByteArray];
  edtByteSize.enabled:=label2.enabled;

  cbTypeChange(cbType);
end;

function TfrmStructures2ElementInfo.getVariableType: TVariableType;
begin
  case cbType.ItemIndex of
    0: result:=vtByte;
    1: result:=vtWord;
    2: result:=vtDword;
    3: result:=vtQword;
    4: result:=vtSingle;
    5: result:=vtDouble;
    6: result:=vtString;
    7: result:=vtUnicodeString;
    8: result:=vtByteArray;
    9: result:=vtPointer;
    else
      result:=vtCustom;
  end;
end;

procedure TfrmStructures2ElementInfo.setCustomType(ct: TCustomType);
var i: integer;
begin
  //find the index with the given ct and focus that
  if ct<>nil then
  begin
    for i:=9 to cbtype.Items.Count-1 do
      if cbType.items.Objects[i]=ct then
      begin
        cbType.ItemIndex:=i;
        exit;
      end;

    setVariableType(vtCustom); //triggers an graphical update
  end;


end;

function TfrmStructures2ElementInfo.getCustomType: TCustomType;
begin
  result:=nil;
  if (cbType.ItemIndex<>-1) then
    result:=TCustomType(cbType.Items.Objects[cbType.ItemIndex]); //returns nil or the custom type
end;

procedure TfrmStructures2ElementInfo.setHexadecimal(state: boolean);
begin
  if state then signed:=false;
  cbHexadecimal.checked:=state;
end;

function TfrmStructures2ElementInfo.getHexadecimal: boolean;
begin
  result:=cbHexadecimal.Checked;
end;

procedure TfrmStructures2ElementInfo.setSigned(state: boolean);
begin
  if state then hexadecimal:=false;
  cbSigned.checked:=state;
end;

function TfrmStructures2ElementInfo.getSigned: boolean;
begin
  result:=cbsigned.checked;
end;

procedure TfrmStructures2ElementInfo.setDescription(d: string);
begin
  edtDescription.text:=d;
end;

function TfrmStructures2ElementInfo.getDescription: string;
begin
  result:=edtDescription.text;
end;

procedure TfrmStructures2ElementInfo.setOffset(i: integer);
begin
  edtOffset.text:=inttohex(i,1);
  fOffset:=i;
end;

function TfrmStructures2ElementInfo.getOffset: integer;
begin
  result:=fOffset;
end;


procedure TfrmStructures2ElementInfo.FormCreate(Sender: TObject);
var i: integer;
begin
  while cbStructType.items.count>1 do
    cbStructType.Items.Delete(1);


  //fill the type combobox (for translations)

  cbtype.items.BeginUpdate;
  cbtype.Items.clear;
  //Note to others: Keep this order!
  cbtype.items.add(rs_vtByte);
  cbtype.items.add(rs_vtWord);
  cbtype.items.add(rs_vtDWord);
  cbtype.items.add(rs_vtQWord);
  cbtype.items.add(rs_vtSingle);
  cbtype.items.add(rs_vtDouble);
  cbtype.items.add(rs_vtString);
  cbtype.items.add(rs_vtUnicodeString);
  cbtype.items.add(rs_vtByteArray);
  cbtype.items.add(rs_vtPointer);

  //add custom types
  for i:=0 to customTypes.count-1 do
    cbType.items.AddObject(TCustomType(customTypes[i]).name, customtypes[i]);

  cbtype.items.EndUpdate;

  cbType.dropdowncount:=min(16, cbType.items.count);
end;

procedure TfrmStructures2ElementInfo.pnlBackgroundClick(Sender: TObject);
begin
  if colordialog1.execute then
  begin
    pnlbackground.color:=colordialog1.color;
    ChangedBackgroundColor:=true;
  end;
end;

procedure TfrmStructures2ElementInfo.FillStructureList;
var i: integer;
begin
  if cbStructType.Items.Count=1 then
  begin
    cbStructType.Items.BeginUpdate;

    for i:=0 to DissectedStructs.Count-1 do
      cbStructType.items.AddObject(TDissectedStruct(DissectedStructs[i]).name, DissectedStructs[i]);

    cbStructType.DropDownCount:=min(16, cbStructType.items.count);
    cbStructType.items.EndUpdate;
  end;

end;

procedure TfrmStructures2ElementInfo.cbTypeChange(Sender: TObject);
var i: integer;
begin
  i:=cbType.itemindex;
  edtBytesize.visible:=i in [6,7,8];
  edtBytesize.enabled:=i in [6,7,8];
  Label2.visible:=edtByteSize.visible;

  cbHexadecimal.enabled:=i in [0,1,2,3,4,5,8];
  cbSigned.enabled:=i in [0,1,2,3,8];


  label5.Enabled:=i=9;
  cbStructType.enabled:=i=9;
  lblOffsetInto.Enabled:=i=9;
  edtChildstart.enabled:=i=9;

  if i=9 then   //pointer, uncheck hex and signed
  begin
    cbHexadecimal.checked:=false;
    cbSigned.checked:=false;
  end;

  ChangedVartype:=true;

  if cbStructType.enabled then //fill the list of structures
    FillStructureList;
end;

procedure TfrmStructures2ElementInfo.edtByteSizeChange(Sender: TObject);
begin
  try
    fbytesize:=StrToInt(edtByteSize.text);
    edtByteSize.Font.color:=clWindowText;
  except
    edtByteSize.Font.color:=clRed;
  end;

  ChangedByteSize:=true;
end;

procedure TfrmStructures2ElementInfo.edtChildstartChange(Sender: TObject);
begin
  try
    Fchildstructstart:=StrToInt('$'+edtChildstart.text);
    edtChildstart.Font.color:=clWindowText;
  except
    edtChildstart.Font.color:=clRed;
  end;

  ChangedchildStructStart:=true;
end;

procedure TfrmStructures2ElementInfo.edtDescriptionChange(Sender: TObject);
begin
  ChangedDescription:=true;
end;

procedure TfrmStructures2ElementInfo.cbHexadecimalChange(Sender: TObject);
begin
  cbHexadecimal.OnChange:=nil;
  hexadecimal:=cbHexadecimal.checked;
  cbHexadecimal.onchange:=cbHexadecimalChange;

  ChangedHexadecimal:=true;;
end;

procedure TfrmStructures2ElementInfo.Button1Click(Sender: TObject);
begin
  if (localChild<>nil) and (localchild<>getChildStruct) then
    if MessageDlg(format(rsS2EIIfYouContinueTheOldLocallyDefinedType,[localChild.name]), mtWarning, [mbyes, mbno], 0)<>mryes then exit;

  modalresult:=mrok;
end;

procedure TfrmStructures2ElementInfo.cbSignedChange(Sender: TObject);
begin
  cbSigned.onchange:=nil;
  signed:=cbSigned.checked;
  cbSigned.onchange:=cbSignedChange;

  ChangedSigned:=true;
end;

procedure TfrmStructures2ElementInfo.cbStructTypeChange(Sender: TObject);
begin
  ChangedChildStruct:=true;
  cbExpandChangesAddress.enabled:=cbStructType.ItemIndex>=1;

  if cbExpandChangesAddress.enabled=false then
    cbExpandChangesAddress.checked:=false;
end;

procedure TfrmStructures2ElementInfo.edtOffsetChange(Sender: TObject);
begin
  try
    fOffset:=StrToInt('$'+edtOffset.text);
    edtOffset.Font.color:=clWindowText;
  except
    edtOffset.Font.color:=clRed;
  end;

  ChangedOffset:=true;
end;

end.

