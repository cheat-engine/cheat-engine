unit formAddressChangeUnit;

{$MODE Delphi}

interface

uses
  windows, LCLIntf, LResources, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons, Arrow, Spin,
  CEFuncProc, NewKernelHandler, symbolhandler, memoryrecordunit, types, byteinterpreter;

const WM_disablePointer=WM_USER+1;

type
  TformAddressChange=class;
  TPointerInfo=class;

  TOffsetInfo=class
  private
    fowner: TPointerInfo;
    fBaseAddress: ptruint;
    fOffset: Integer; //signed integer
    fInvalidOffset: boolean;

    lblPointerAddressToValue: TLabel; //Address -> Value
    edtOffset: Tedit;
    sbDecrease, sbIncrease: TSpeedButton;
    istop: boolean;
    procedure setOffset(o: integer);
    procedure offsetchange(sender: TObject);
    procedure DecreaseClick(sender: TObject);
    procedure IncreaseClick(sender: TObject);
    procedure setBaseAddress(address: ptruint);

  public
    constructor create(parent: TPointerinfo);
    destructor destroy; override;
    function getAddressThisPointsTo(var address: ptruint): boolean;
    procedure setTop(var newtop: integer);
    procedure UpdateLabels;
    property owner: TPointerinfo read fowner;
    property offset: integer read foffset write setOffset;
    property invalidOffset: boolean read fInvalidOffset;
    property baseAddress: ptruint write setBaseAddress;
  end;


  TPointerInfo=class(TCustomPanel)
  private
    fowner: TformAddressChange;
    fBaseAddress: ptruint;
    fInvalidBaseAddress: boolean;
    fError: boolean;  //indicator for the child offsets, accessed by Error
    baseAddress: TEdit;  //the bottom line
    baseValue: Tlabel;
    offsets: Tlist; //the lines above it

    btnAddOffset: TButton;
    btnRemoveOffset: TButton;
    procedure selfdestruct;
    procedure basechange(sender: Tobject);
    procedure AddOffsetClick(sender: TObject);
    procedure RemoveOffsetClick(sender: TObject);
    function getValueLeft: integer;
    function getOffset(index: integer): TOffsetInfo;
    function getoffsetcount: integer;

    function getAddressThisPointsTo(var address: ptruint): boolean;

  public
    property owner: TformAddressChange read fowner;
    property valueLeft: integer read getValueLeft; //gets the basevalue.left
    property error: boolean read ferror;
    property invalidBaseAddress: boolean read fInvalidBaseAddress;
    property offsetcount: integer read getoffsetcount;
    property offset[Index: Integer]: TOffsetInfo read getOffset;

    procedure processAddress; //reads the base address and all the offsets and shows what it all does
    procedure setupPositionsAndSizes;


    constructor create(owner: TformAddressChange);
    destructor destroy; override;

  end;

  { TformAddressChange }

  TformAddressChange = class(TForm)
    edtDescription: TEdit;
    Label12: TLabel;
    Label3: TLabel;
    lblValue: TLabel;
    pnlBitinfo: TPanel;
    cbunicode: TCheckBox;
    cbvarType: TComboBox;
    edtSize: TEdit;
    editAddress: TEdit;
    btnOk: TButton;
    btnCancel: TButton;
    cbPointer: TCheckBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lengthlabel: TLabel;
    pnlExtra: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    RadioButton8: TRadioButton;
    Timer1: TTimer;
    Timer2: TTimer;
    procedure btnCancelClick(Sender: TObject);
    procedure cbvarTypeChange(Sender: TObject);
    procedure editAddressChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbPointerClick(Sender: TObject);
    procedure btnRemoveOffsetOldClick(Sender: TObject);
    procedure btnAddOffsetOldClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure editAddressKeyPress(Sender: TObject; var Key: Char);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure pcExtraChange(Sender: TObject);
    procedure tsStartbitContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    { Private declarations }
    pointerinfo: TPointerInfo;
    fMemoryRecord: TMemoryRecord;
    delayedpointerresize: boolean;
    procedure offsetKeyPress(sender: TObject; var key:char);
    procedure processaddress;
    procedure setMemoryRecord(rec: TMemoryRecord);
    procedure DelayedResize;
    procedure AdjustHeightAndButtons;
    procedure DisablePointerExternal(var m: TMessage); message WM_disablePointer;
    procedure setVarType(vt: TVariableType);
    function getVartype: TVariableType;
    procedure setLength(l: integer);
    function getLength: integer;
    procedure setStartbit(b: integer);
    function getStartbit: integer;
    procedure setUnicode(state: boolean);
    function getUnicode: boolean;
    procedure setDescription(s: string);
    function getDescription: string;
  public
    { Public declarations }
    index: integer;
    index2: integer;
    property memoryrecord: TMemoryRecord read fMemoryRecord write setMemoryRecord;
    property vartype: TVariableType read getVartype write setVartype;
    property length: integer read getLength write setLength;
    property startbit: integer read getStartbit write setStartbit;
    property unicode: boolean read getUnicode write setUnicode;
    property description: string read getDescription write setDescription;
  end;

var
  formAddressChange: TformAddressChange;

implementation

uses MainUnit, formsettingsunit;

resourcestring
  rsThisPointerPointsToAddress = 'This pointer points to address';
  rsTheOffsetYouChoseBringsItTo = 'The offset you chose brings it to';
  rsResultOfNextPointer = 'Result of next pointer';
  rsAddressOfPointer = 'Address of pointer';
  rsOffsetHex = 'Offset (Hex)';
  rsFillInTheNrOfBytesAfterTheLocationThePointerPoints = 'Fill in the nr. of bytes after the location the pointer points to';
  rsIsNotAValidOffset = '%s is not a valid offset';
  rsNotAllOffsetsHaveBeenFilledIn = 'Not all offsets have been filled in';

{ TOffsetInfo }


procedure TOffsetInfo.DecreaseClick(sender: TObject);
begin
  offset:=offset-4;
end;

procedure TOffsetInfo.IncreaseClick(sender: TObject);
begin
  offset:=offset+4;
end;


function TOffsetInfo.getAddressThisPointsTo(var address: ptruint): boolean;
var x: dword;
begin
  //use the baseaddress and offset to get to the address

  result:=false;
  if not invalidOffset then
  begin
    address:=0;
    result:=ReadProcessMemory(processhandle, pointer(fBaseAddress+fOffset), @address, processhandler.pointersize, x);
  end;
end;

procedure TOffsetInfo.UpdateLabels;
var Sbase: string;
  Soffset: string;
  Spointsto: string;
  sign: string;
  e: boolean;
  success: boolean;
  a: ptruint;
  newwidth: integer;
begin
  e:=false;
  if owner.error then
  begin
    Sbase:='????????';
    e:=true;
  end
  else
    Sbase:=inttohex(fBaseAddress,8);

  if invalidOffset then
  begin
    sign:='+';
    Soffset:='?';
    e:=true;
  end
  else
  begin
    if fOffset>=0 then
    begin
      sign:='+';
      Soffset:=inttohex(fOffset,1);
    end
    else
    begin
      sign:='-';
      Soffset:=inttohex(-fOffset,1);
    end;
  end;

  if not e then
  begin
    success:=getAddressThisPointsTo(a);
    if success then
      SPointsTo:=inttohex(a,8)
    else
      SPointsTo:='????????';
  end
  else
  begin
    SPointsTo:='????????';
  end;

  if istop then
  begin
    if e then
      lblPointerAddressToValue.Caption:=sbase+sign+soffset+' = ????????'
    else
      lblPointerAddressToValue.Caption:=sbase+sign+soffset+' = '+inttohex(fBaseAddress+offset,8)
  end
  else
    lblPointerAddressToValue.Caption:='['+sbase+sign+soffset+'] -> '+SPointsTo;

  //update positions
  newwidth:=lblPointerAddressToValue.left+lblPointerAddressToValue.Width;
  if newwidth>owner.ClientWidth then
  begin
    owner.ClientWidth:=newwidth+16;
    owner.owner.ClientWidth:=owner.left+owner.ClientWidth;
  end;
end;


procedure TOffsetInfo.setOffset(o: integer);
var s: string;
begin
  finvalidOffset:=false;


  s:=lowercase(IntToHexSigned(o,1));
  if lowercase(edtOffset.text)<>s then //needs to be updated
  begin
    edtOffset.OnChange:=nil; //disable the onchange
    edtOffset.text:=s;
    edtOffset.OnChange:=offsetchange; //set it back
  end;

  fOffset:=o;
  owner.processAddress;
end;

procedure TOffsetInfo.setBaseAddress(address: ptruint);
begin
  fBaseAddress:=address;
  UpdateLabels;
end;

procedure TOffsetInfo.offsetchange(sender: TObject);
begin
  try
    offset:=StrToQWordEx(ConvertHexStrToRealStr(tedit(sender).Text));
    edtOffset.Font.Color:=clDefault;
    finvalidOffset:=false;
  except
    edtOffset.Font.Color:=clRed;
    finvalidOffset:=true;
    UpdateLabels;
  end;
end;

procedure TOffsetInfo.setTop(var newtop: integer);
{
Sets the offset's position and returns the position for the new offsetline
}
begin
  if edtOffset.parent=nil then
  begin
    //only assign a parent when the positions ar finally set
    edtOffset.parent:=owner;
    lblPointerAddressToValue.parent:=owner;
    sbDecrease.parent:=owner;
    sbIncrease.parent:=owner;
  end;


  //only show the pointeraddresstovalue line if not the first line
  edtOffset.taborder:=owner.offsets.IndexOf(self);
  istop:=edtOffset.taborder=0;

  sbDecrease.top:=newtop;
  sbIncrease.top:=newtop;
  edtOffset.top:=newtop;

  sbDecrease.left:=0;
  edtOffset.left:=sbDecrease.left+sbDecrease.Width+1;
  sbIncrease.left:=edtOffset.Left+edtOffset.Width+1;

  lblPointerAddressToValue.top:=edtOffset.top + (edtOffset.Height div 2) - (lblPointerAddressToValue.Height div 2);
  lblPointerAddressToValue.left:=sbIncrease.Left+sbIncrease.Width+3;
  lblPointerAddressToValue.visible:=true;


  newtop:=sbDecrease.top+sbDecrease.height+3;
end;

destructor TOffsetInfo.destroy;
begin
  if lblPointerAddressToValue<>nil then
    freeandnil(lblPointerAddressToValue);

  if edtOffset<>nil then
    freeandnil(edtOffset);

  if sbDecrease<>nil then
    freeandnil(sbDecrease);

  if sbIncrease<>nil then
    freeandnil(sbIncrease);

  fowner.offsets.Remove(self);
  inherited destroy;
end;

constructor TOffsetInfo.create(parent: TPointerinfo);
var insertinsteadofadd: boolean;
begin
  fowner:=parent;

  //check if ctrl is pressed, if so, insert instead of append (or the other way depending on settings)

  insertinsteadofadd:=not formsettings.cbOldPointerAddMethod.checked; //append pointerline instead of insert
  if (((GetKeyState(VK_CONTROL) shr 15) and 1)=1) then
    insertinsteadofadd:=not insertinsteadofadd;

  if insertinsteadofadd then
    fowner.offsets.Insert(0, self)
  else
    fowner.offsets.Add(self);

  //create a pointeraddress label (visible if not first)
  lblPointerAddressToValue:=TLabel.Create(parent);
  lblPointerAddressToValue.Caption:=' ';
  lblPointerAddressToValue.parent:=parent;

  //an offset editbox
  fOffset:=0;
  edtOffset:=Tedit.create(parent);
  edtOffset.Text:='0';

  edtOffset.Alignment:=taCenter;
  edtOffset.OnChange:=OffsetChange;


  //two buttons, one for + and one for -
  sbDecrease:=TSpeedButton.create(parent);
  sbDecrease.height:=edtOffset.height;
  sbDecrease.width:=sbDecrease.height;
  sbDecrease.caption:='<';
  sbDecrease.OnClick:=DecreaseClick;

  sbIncrease:=TSpeedButton.create(parent);
  sbIncrease.height:=sbDecrease.height;
  sbIncrease.width:=sbDecrease.width;
  sbIncrease.caption:='>';
  sbIncrease.OnClick:=IncreaseClick;


  edtOffset.width:=owner.baseAddress.Width-2*sbIncrease.Height-2;


end;

{ TPointerInfo }

procedure TPointerInfo.AddOffsetClick(sender: TObject);
begin
  TOffsetInfo.Create(self);
  setupPositionsAndSizes;
end;

procedure TPointerInfo.RemoveOffsetClick(sender: TObject);
var insertinsteadofadd: boolean;
  o: TOffsetInfo;
begin
  insertinsteadofadd:=not formsettings.cbOldPointerAddMethod.checked; //append pointerline instead of insert
  if (((GetKeyState(VK_CONTROL) shr 15) and 1)=1) then
    insertinsteadofadd:=not insertinsteadofadd;

  if insertinsteadofadd then //remove the first offset in the list
    o:=TOffsetinfo(offsets[0])
  else
    o:=TOffsetInfo(offsets[offsets.Count-1]);

  o.free;

  if offsets.Count>0 then
    setupPositionsAndSizes
  else
    selfdestruct;
end;

procedure TPointerInfo.selfdestruct;
begin
  postmessage(owner.handle, WM_disablePointer, 0,0);
end;

function TPointerInfo.getValueLeft: integer;
begin
  result:=baseValue.left;
end;

function TPointerInfo.getOffset(index: integer): TOffsetInfo;
begin
  result:=TOffsetInfo(offsets[index]);
end;

function TPointerInfo.getoffsetcount: integer;
begin
  result:=offsets.Count;
end;

function TPointerInfo.getAddressThisPointsTo(var address: ptruint): boolean;
var x: dword;
begin
  result:=false;
  if not InvalidBaseAddress then
  begin
    address:=0; //clear all bits
    result:=ReadProcessMemory(processhandle, pointer(fBaseAddress), @address, processhandler.pointersize, x);
  end;
end;


procedure TPointerInfo.basechange(sender: Tobject);
var e: boolean;
begin
  fBaseAddress:=symhandler.getAddressFromName(baseAddress.text, false, e);
  fInvalidBaseAddress:=e;

  if fInvalidBaseAddress then
    baseAddress.Font.Color:=clRed
  else
    baseAddress.Font.Color:=clDefault;

  processAddress;
end;

procedure TPointerInfo.processAddress;
var base: PtrUInt;
  i: integer;
  e: boolean;
begin
  ferror:=not getAddressThisPointsTo(base);

  if error then
    baseValue.caption:='->????????'
  else
    baseValue.caption:='->'+inttohex(base,8);

  for i:=offsetcount-1 downto 1 do
  begin
    offset[i].baseaddress:=base;
    if not offset[i].getAddressThisPointsTo(base) then
      ferror:=true; //signal an error to all subsequent offsets
  end;

  //add the last offset
  offset[0].baseaddress:=base;
  base:=base+offset[0].offset;

  if error then
    owner.editAddress.text:='????????'
  else
    owner.editAddress.text:=inttohex(base,8);
end;

procedure TPointerInfo.setupPositionsAndSizes;
var
  currentTop: integer;
  i: integer;
  newwidth: integer;
begin
  //place offsets and set size


  currentTop:=0;
  for i:=0 to offsets.count-1 do
    TOffsetInfo(offsets[i]).setTop(currentTop);

  baseAddress.top:=currentTop;
  baseValue.top:=baseAddress.Top+(baseAddress.Height div 2)-(baseValue.height div 2);

  btnAddOffset.top:=baseAddress.top+baseAddress.Height+3;
  btnRemoveOffset.top:=btnAddOffset.top;

  ClientHeight:=btnAddOffset.Top+btnAddOffset.Height+3;
  //Width will be set using the UpdateLabels method of individial offsets when the current offset it too small

  //first set the initial width
  //newwidth:=offset[offsetcount-1].lblPointerAddressToValue.Left+ offset[offsetcount-1].lblPointerAddressToValue.Canvas.TextWidth('[XXXXXXXX + XXXX] -> XXXXXXXX  ')+16;
 // if newwidth>clientwidth then clientwidth:=newwidth;

  //update buttons of the form
  with owner do
  begin
    btnOk.top:=self.top+self.height+3;
    btnCancel.top:=btnOk.top;
    ClientHeight:=btnOk.top+btnOk.Height+3;
    ClientWidth:=self.ClientWidth+self.Left;
  end;

  processAddress;
end;

destructor TPointerInfo.destroy;
begin
  if offsets<>nil then
    while offsets.count>0 do //destruction of a offset removes it automagically from the list
      TOffsetInfo(offsets[0]).Free;

  owner.btnOk.top:=owner.cbPointer.Top+owner.cbPointer.Height+3;
  owner.btnCancel.top:=owner.btnOk.top;
  owner.ClientHeight:=owner.btnOk.top+owner.btnOk.Height+3;
  owner.editAddress.enabled:=true;

  if baseAddress<>nil then
    freeandnil(baseAddress);

  if baseValue<>nil then
    freeandnil(baseValue);

  if btnAddOffset<>nil then
    freeandnil(btnAddOffset);

  if btnRemoveOffset<>nil then
    freeandnil(btnRemoveOffset);


  inherited Destroy;
end;

constructor TPointerInfo.create(owner: TformAddressChange);
begin
  //create the objects
  inherited create(owner);


  fowner:=owner;
  offsets:=tlist.create;
  parent:=owner;

  BevelOuter:=bvNone;
  left:=owner.cbPointer.Left;
  top:=owner.cbPointer.Top+owner.cbPointer.Height+3;

  baseAddress:=tedit.create(self);
  baseAddress.parent:=self;
  baseAddress.left:=0;
  if ProcessHandler.is64Bit then
    baseAddress.Width:=128
  else
    baseAddress.Width:=88;

  baseAddress.OnChange:=basechange;


  baseValue:=tlabel.create(self);
  baseValue.caption:=' ';
  baseValue.parent:=self;
  baseValue.left:=baseAddress.left+baseAddress.Width+3;
  baseValue.top:=baseAddress.Top+(baseAddress.Height div 2)-(baseValue.height div 2);

  btnAddOffset:=Tbutton.Create(self);
  btnAddOffset.caption:='Add Offset';
  btnAddOffset.Left:=owner.btnOk.Left-left;
  btnAddOffset.Width:=owner.btnOk.Width;
  btnAddOffset.Height:=owner.btnOk.Height;
  btnAddOffset.OnClick:=AddOffsetClick;
  btnAddOffset.parent:=self;

  btnRemoveOffset:=TButton.create(self);
  btnRemoveOffset.caption:='Remove Offset';
  btnRemoveOffset.Left:=owner.btnCancel.left-left;
  btnRemoveOffset.Width:=btnAddOffset.Width;
  btnRemoveOffset.Height:=btnAddOffset.Height;
  btnRemoveOffset.OnClick:=RemoveOffsetClick;
  btnRemoveOffset.parent:=self;


  TOffsetInfo.Create(self);

  owner.editAddress.enabled:=false;
  setupPositionsAndSizes;
end;

{ Tformaddresschange }

procedure Tformaddresschange.setDescription(s: string);
begin
  edtDescription.Text:=s;
end;

function Tformaddresschange.getDescription: string;
begin
  result:=edtDescription.Text;
end;

procedure Tformaddresschange.setUnicode(state: boolean);
begin
  cbunicode.checked:=state;
end;

function Tformaddresschange.getUnicode: boolean;
begin
  result:=cbunicode.checked;
end;

procedure Tformaddresschange.setStartbit(b: integer);
begin
  case b of
    0: RadioButton1.checked:=true;
    1: RadioButton2.checked:=true;
    2: RadioButton3.checked:=true;
    3: RadioButton4.checked:=true;
    4: RadioButton5.checked:=true;
    5: RadioButton6.checked:=true;
    6: RadioButton7.checked:=true;
    7: RadioButton8.checked:=true;
  end;
end;

function Tformaddresschange.getStartbit: integer;
begin
  result:=0;
  if RadioButton1.checked then
    result:=0
  else
  if RadioButton2.checked then
    result:=1
  else
  if RadioButton3.checked then
    result:=2
  else
  if RadioButton4.checked then
    result:=3
  else
  if RadioButton5.checked then
    result:=4
  else
  if RadioButton6.checked then
    result:=5
  else
  if RadioButton7.checked then
    result:=6
  else
  if RadioButton8.checked then
    result:=7;

end;

procedure Tformaddresschange.setLength(l: integer);
begin
  edtSize.text:=inttostr(l);
end;

function Tformaddresschange.getLength: integer;
begin
  result:=StrToIntDef(edtSize.Text,0)
end;

procedure Tformaddresschange.setVarType(vt: TVariableType);
begin
  case vt of
    vtBinary: cbvarType.ItemIndex:=0;
    vtByte: cbvarType.ItemIndex:=1;
    vtWord: cbvarType.ItemIndex:=2;
    vtDword: cbvarType.ItemIndex:=3;
    vtQword: cbvarType.ItemIndex:=4;
    vtSingle: cbvarType.ItemIndex:=5;
    vtDouble: cbvarType.ItemIndex:=6;
    vtString: cbvarType.ItemIndex:=7;
    vtByteArray: cbvarType.ItemIndex:=8;
  end;
end;

function Tformaddresschange.getVartype: TVariableType;
var i: integer;
begin
  {
  Binary
  Byte
  2 Bytes
  4 Bytes
  8 Bytes
  Float
  Double
  Text
  Array of Bytes
  <custom types>
  }
  i:=cbvarType.ItemIndex;
  case i of
    0: result:=vtBinary;
    1: result:=vtByte;
    2: result:=vtWord;
    3: result:=vtDword;
    4: result:=vtQword;
    5: result:=vtSingle;
    6: result:=vtDouble;
    7: result:=vtString;
    8: result:=vtByteArray;
    else
      result:=vtCustom;
  end;
end;


procedure Tformaddresschange.processaddress;
var a: PtrUInt;
  e: boolean;
begin
  //read the address and display the value it points to

  a:=symhandler.getAddressFromName(editAddress.Text,false,e);
  if not e then
  begin
    //get the vartype and parse it
    lblValue.caption:='='+readAndParseAddress(a, vartype, nil,false, false, StrToIntDef(edtSize.text,1));
  end
  else
    lblValue.caption:='=???';


end;


procedure Tformaddresschange.offsetKeyPress(sender: TObject; var key:char);
begin
{  if key<>'-' then hexadecimal(key);
  if cbpointer.Checked then timer1.Interval:=1;   }

end;


procedure TformAddressChange.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin

end;

procedure TformAddressChange.FormActivate(Sender: TObject);
begin

end;

procedure TformAddressChange.cbvarTypeChange(Sender: TObject);
begin
  pnlExtra.visible:=cbvarType.itemindex in [0,7,8];
  pnlBitinfo.visible:=cbvarType.itemindex = 0;

  AdjustHeightAndButtons;

  processaddress;
end;

procedure TformAddressChange.btnCancelClick(Sender: TObject);
begin

end;

procedure TformAddressChange.editAddressChange(Sender: TObject);
begin
  processaddress;
end;



procedure TformAddressChange.setMemoryRecord(rec: TMemoryRecord);
var i: integer;
    tmp:string;
begin
  fMemoryRecord:=rec;

  edtDescription.Text:=rec.Description;
  vartype:=rec.VarType;

  if not rec.isPointer then
  begin
    editaddress.Text:=AnsiToUtf8(rec.interpretableaddress);
    if rec.VarType = vtBinary then
    begin
      pnlBitinfo.Visible:=true;
      case rec.Extra.bitData.Bit of
        0: radiobutton1.checked:=true;
        1: radiobutton2.checked:=true;
        2: radiobutton3.checked:=true;
        3: radiobutton4.checked:=true;
        4: radiobutton5.checked:=true;
        5: radiobutton6.checked:=true;
        6: radiobutton7.checked:=true;
        7: radiobutton8.checked:=true;
      end;
    end
  end;


  (*
  if rec.IsPointer then
  begin
    cbPointer.Checked:=true;

    for i:=1 to length(rec.pointeroffsets)-1 do btnAddOffsetOld.Click; //add lines  (-1 because checking the cbpointer already adds one)

    //fill the lines
    for i:=0 to length(rec.pointeroffsets)-1 do
      pointerinfo[i].offset.text:=IntToHex(rec.pointeroffsets[i],1);

    pointerinfo[length(pointerinfo)-1].address.text:=AnsiToUtf8(rec.interpretableaddress);
  end;    *)


  processaddress;
  AdjustHeightAndButtons

end;


procedure TformAddressChange.DelayedResize;
var i,a,b: integer;
begin
  AdjustHeightAndButtons;

  (*
  for i:=0 to length(pointerinfo)-1 do
  begin
    pointerinfo[i].ValueAtAddressText.left:=4;

    pointerinfo[i].FinalDestination.left:=pointerinfo[i].ValueAtAddressText.left+pointerinfo[i].ValueAtAddressText.width+20;
    pointerinfo[i].addresstext.left:=4;
    pointerinfo[i].address.left:=pointerinfo[i].addresstext.left+pointerinfo[i].addresstext.width+3;
    pointerinfo[i].offsettext.left:=pointerinfo[i].FinalDestination.left;
    pointerinfo[i].offset.left:=pointerinfo[i].offsettext.left+pointerinfo[i].offsettext.width+5;
  end;

  a:=pointerinfo[length(pointerinfo)-1].FinalDestination.left;
  b:=pointerinfo[length(pointerinfo)-1].FinalDestination.width;

  clientwidth:=a+b+5;  *)
end;

procedure TformAddressChange.cbPointerClick(Sender: TObject);
var i: integer;
    startoffset,inputoffset,rowheight: integer;

    a,b,c,d: integer;
begin
  if cbpointer.checked then
  begin
    if pointerinfo=nil then
      pointerinfo:=TPointerInfo.create(self); //creation will do the gui update
  end
  else
  begin
    if pointerinfo<>nil then
      freeandnil(pointerinfo);
  end;

end;

procedure TformAddressChange.DisablePointerExternal(var m: TMessage);
begin
  cbPointer.Checked:=false;
end;

procedure TformAddressChange.AdjustHeightAndButtons;
var i: integer;
begin
  if pnlExtra.visible then
  begin

    //check if pnlbits is visible
    if pnlBitinfo.visible then
      pnlExtra.height:=pnlBitinfo.Top+pnlBitinfo.Height+3
    else
      pnlExtra.height:=edtSize.top+edtSize.Height+3;


    cbPointer.top:=pnlExtra.top+pnlExtra.Height+3;
  end
  else
    cbPointer.top:=cbvarType.top+cbvarType.Height+3;


  if pointerinfo=nil then
    btnok.top:=cbPointer.Top+cbPointer.Height+3
  else
  begin
    pointerinfo.top:=cbPointer.Top+cbPointer.Height+3;
    btnok.top:=pointerinfo.Top+pointerinfo.Height+3;
  end;


  btnCancel.top:=btnok.top;
  clientheight:=btncancel.top+btnCancel.height+6;
end;

procedure TformAddressChange.btnRemoveOffsetOldClick(Sender: TObject);
begin

end;

procedure TformAddressChange.btnAddOffsetOldClick(Sender: TObject);
begin

end;

procedure TformAddressChange.btnOkClick(Sender: TObject);
var bit: integer;
    address: dword;
    err:integer;

    paddress: dword;
    offsets: array of integer;

    i: integer;
begin

  {
  if RadioButton1.checked then bit:=0 else
  if RadioButton2.checked then bit:=1 else
  if RadioButton3.checked then Bit:=2 else
  if RadioButton4.checked then Bit:=3 else
  if RadioButton5.checked then Bit:=4 else
  if RadioButton6.checked then Bit:=5 else
  if RadioButton7.checked then Bit:=6 else
                               Bit:=7;


  if memoryrecord.vartype=vtbinary then
    memoryrecord.Extra.bitData.Bit:=bit;

 (* if cbpointer.Checked then
  begin
    address:=0;
    paddress:=symhandler.getaddressfromname(utf8toansi(pointerinfo[length(pointerinfo)-1].address.text));
    memoryrecord.interpretableaddress:=utf8toansi(pointerinfo[length(pointerinfo)-1].address.text);
  end
  else
  begin
    paddress:=0;
    addresS:=symhandler.getaddressfromname(utf8toansi(editaddress.text));
    memoryrecord.interpretableaddress:=utf8toansi(editaddress.text);
  end;


  setlength(offsets,length(pointerinfo));

  for i:=0 to length(pointerinfo)-1 do
  begin
    if length(pointerinfo[i].offset.Text)>0 then
    begin
      if pointerinfo[i].offset.Text[1]='-' then
        val('-$'+copy(pointerinfo[i].offset.Text,2,length(pointerinfo[i].offset.Text)-1),offsets[i],err)
      else
        val('$'+pointerinfo[i].offset.Text,offsets[i],err);

      if err<>0 then raise exception.Create(Format(rsIsNotAValidOffset, [pointerinfo[i].offset.Text]));
    end else raise exception.Create(rsNotAllOffsetsHaveBeenFilledIn);
  end;

  setlength(memoryrecord.pointeroffsets, length(offsets));

  for i:=0 to length(offsets)-1 do
  begin
    memoryrecord.PointerOffsets[i]:=offsets[i];
    memoryrecord.active:=false;
  end;     *)

  modalresult:=mrok; }
end;

procedure TformAddressChange.editAddressKeyPress(Sender: TObject;
  var Key: Char);
begin

end;

procedure TformAddressChange.FormDestroy(Sender: TObject);
begin
  if pointerinfo<>nil then
    freeandnil(pointerinfo);
end;

procedure TformAddressChange.FormShow(Sender: TObject);
begin

end;

procedure TformAddressChange.FormWindowStateChange(Sender: TObject);
begin

end;

procedure TformAddressChange.pcExtraChange(Sender: TObject);
begin

end;

procedure TformAddressChange.tsStartbitContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin

end;

procedure TformAddressChange.Timer1Timer(Sender: TObject);
begin
  timer1.Interval:=1000;
  if visible and cbpointer.checked then
    processaddress;
end;

procedure TformAddressChange.Timer2Timer(Sender: TObject);
begin
  //lazarus bug bypass for not setting proper width when the window is not visible, and no event to signal when it's finally visible (onshow isn't one of them)
  DelayedResize;

  timer2.enabled:=false;
end;

initialization
  {$i formAddressChangeUnit.lrs}

end.
