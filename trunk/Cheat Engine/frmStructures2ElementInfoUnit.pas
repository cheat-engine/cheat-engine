unit frmStructures2ElementInfoUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, cefuncproc, StructuresFrm2;

type

  { TfrmStructures2ElementInfo }

  TfrmStructures2ElementInfo = class(TForm)
    Button1: TButton;
    Button2: TButton;
    cbStructType: TComboBox;
    cbType: TComboBox;
    cbHexadecimal: TCheckBox;
    cbSigned: TCheckBox;
    edtByteSize: TEdit;
    edtDescription: TEdit;
    edtOffset: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure cbHexadecimalChange(Sender: TObject);
    procedure cbSignedChange(Sender: TObject);
    procedure cbTypeChange(Sender: TObject);
    procedure edtOffsetChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    fOffset: integer;
    fbytesize: integer;
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
    procedure setBytesize(i: integer);
    function getBytesize: integer;
    procedure setChildStruct(s: TDissectedStruct);
    function getChildStruct: TDissectedStruct;
  public
    { public declarations }
    property description: string read getDescription write setDescription;
    property offset: integer read getOffset write setOffset;
    property hexadecimal: boolean read getHexadecimal write setHexadecimal;
    property signed: boolean read getSigned write setsigned;
    property vartype: TVariableType read getVariableType write setVariableType;
    property bytesize: integer read getBytesize write setBytesize;
    property childstruct: TDissectedStruct read getChildStruct write setChildStruct;
  end; 

var
  frmStructures2ElementInfo: TfrmStructures2ElementInfo;

implementation

{$R *.lfm}



{ TfrmStructures2ElementInfo }

procedure TfrmStructures2ElementInfo.setChildStruct(s: TDissectedStruct);
var i: integer;
begin
  for i:=0 to cbStructType.items.count-1 do
    if cbStructType.Items.Objects[i]=s then
    begin
      cbStructType.ItemIndex:=i;
      exit;
    end;

  //still here so it's a "local" type
  localChild:=s;
  cbStructType.ItemIndex:=cbStructType.Items.AddObject('Local struct:'+s.name, s);
end;

function TfrmStructures2ElementInfo.getChildStruct: TDissectedStruct;
begin
  result:=TDissectedStruct(cbStructType.Items.Objects[cbStructType.ItemIndex]);
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
  end;
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
  cbSigned.checked:=false;
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
  //fill the list of structures
  while cbStructType.items.count>1 do
    cbStructType.Items.Delete(1);

  for i:=0 to DissectedStructs.Count-1 do
    cbStructType.items.AddObject(TDissectedStruct(DissectedStructs[i]).name, DissectedStructs[i]);
end;

procedure TfrmStructures2ElementInfo.cbTypeChange(Sender: TObject);
begin
  edtBytesize.enabled:=cbType.itemindex in [6,7,8];
  Label2.enabled:=edtByteSize.enabled;
  cbHexadecimal.enabled:=cbtype.itemindex in [0,1,2,3,8];
  cbSigned.enabled:=cbHexadecimal.enabled;


  label5.Enabled:=cbtype.itemindex=9;
  cbStructType.enabled:=cbtype.itemindex=9;

end;

procedure TfrmStructures2ElementInfo.cbHexadecimalChange(Sender: TObject);
begin
  hexadecimal:=cbHexadecimal.checked;
end;

procedure TfrmStructures2ElementInfo.Button1Click(Sender: TObject);
begin
  if (localChild<>nil) and (localchild<>getChildStruct) then
    if MessageDlg('If you continue the old locally defined type '+localChild.name+' will be deleted. Continue? (Tip: You can make this type into a global type so it can be re-used over again)', mtWarning, [mbyes, mbno], 0)<>mryes then exit;

  modalresult:=mrok;
end;

procedure TfrmStructures2ElementInfo.cbSignedChange(Sender: TObject);
begin
  signed:=cbSigned.checked;
end;

procedure TfrmStructures2ElementInfo.edtOffsetChange(Sender: TObject);
begin
  try
    fOffset:=StrToInt('$'+edtOffset.text);
    edtOffset.Font.color:=clWindowText;
  except
    edtOffset.Font.color:=clRed;
  end;
end;

end.

