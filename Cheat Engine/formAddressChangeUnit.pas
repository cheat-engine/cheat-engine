unit formAddressChangeUnit;

{$MODE Delphi}

interface

uses
  windows, LCLIntf, LResources, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons, Arrow, Spin,
  CEFuncProc, NewKernelHandler, symbolhandler, memoryrecordunit, types;

type
  TformAddressChange=class;
  TPointerInfo=class;

  TOffsetInfo=class
  private
    fowner: TPointerInfo;
    lblPointerAddressToValue: TLabel; //Address -> Value
    edtOffset: Tedit;
    sbDecrease, sbIncrease: TSpeedButton;
    ftop: integer;
  public
    constructor create(parent: TPointerinfo);
    procedure setTop(newtop: integer);
    property top: integer read fTop write setTop;
    property owner: TPointerinfo read fowner;
  end;


  TPointerInfo=class(TCustomPanel)
  private
    fowner: TformAddressChange;
    baseAddress: TEdit;  //the bottom line
    baseValue: Tlabel;
    offsets: Tlist; //the lines above it
  public
    property owner: TformAddressChange read fowner;
    procedure setupPositionsAndSizes;
    constructor create(owner: TformAddressChange);
    destructor destroy; override;

  end;

  { TformAddressChange }

  TformAddressChange = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Value: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label3: TLabel;
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
    btnAddOffset: TButton;
    btnRemoveOffset: TButton;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    RadioButton8: TRadioButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    Timer1: TTimer;
    Timer2: TTimer;
    procedure cbvarTypeChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbPointerClick(Sender: TObject);
    procedure btnRemoveOffsetClick(Sender: TObject);
    procedure btnAddOffsetClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure editAddressKeyPress(Sender: TObject; var Key: Char);
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
  public
    { Public declarations }
    index: integer;
    index2: integer;
    property memoryrecord: TMemoryRecord read fMemoryRecord write setMemoryRecord;
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

procedure TOffsetInfo.setTop(newtop: integer);
var currenttop: integer;
begin
  if edtOffset.parent<>nil then
  begin
    //only assign a parent when the positions ar finally set
    edtOffset.parent:=owner;
    lblPointerAddressToValue.parent:=owner;
    sbDecrease.parent:=owner;
    sbIncrease.parent:=owner;
  end;


  //only show the pointeraddresstovalue line if not the first line
  ftop:=newtop;
  currenttop:=newtop;
  if owner.offsets.IndexOf(self)>0 then
  begin
    //top starts with the pointeraddresstovalue line
    lblPointerAddressToValue.visible:=true;
    currenttop:=lblPointerAddressToValue.Top+lblPointerAddressToValue.Height+3;
  end
  else
    lblPointerAddressToValue.visible:=false;




end;

constructor TOffsetInfo.create(parent: TPointerinfo);
begin
  fowner:=parent;

  fowner.offsets.Add(self);

  //create a pointeraddress label (visible if not first)
  lblPointerAddressToValue:=TLabel.Create(parent);
  lblPointerAddressToValue.Caption:='[Pointeraddress+offset] -> valueitpointsto';
  lblPointerAddressToValue.parent:=parent;

  //an offset editbox
  edtOffset:=Tedit.create(parent);
  edtOffset.width:=50;


  //two buttons, one for + and one for -
  sbDecrease:=TSpeedButton.create(parent);
  sbDecrease.height:=edtOffset.height;
  sbDecrease.width:=sbDecrease.height;
  sbDecrease.caption:='<';


  sbIncrease:=TSpeedButton.create(parent);
  sbIncrease.height:=sbDecrease.height;
  sbIncrease.width:=sbDecrease.width;
  sbIncrease.caption:='>';


end;

{ TPointerInfo }

procedure TPointerInfo.setupPositionsAndSizes;
begin
  //place offsets and set size

  //update buttons of the form
  with owner do
  begin
    btnAddOffset.top:=self.top+self.height+3;
    btnRemoveOffset.top:=btnAddOffset.top;

    btnOk.top:=btnAddOffset.Top+btnAddOffset.Height+3;
    btnCancel.top:=btnOk.top;
    ClientHeight:=btnOk.top+btnOk.Height+3;
  end;
end;

destructor TPointerInfo.destroy;
begin
  if offsets<>nil then
    while offsets.count>0 do //destruction of a offset removes it automagically from the list
      TOffsetInfo(offsets[0]).Free;

  owner.pointerinfo.Remove(self);

  owner.btnOk.top:=owner.cbPointer.Top+owner.cbPointer.Height+3;
  owner.btnCancel.top:=owner.btnOk.top;
  ClientHeight:=btnOk.top+btnOk.Height+3;

  inherited Destroy;
end;

constructor TPointerInfo.create(owner: TformAddressChange);
begin
  //create the objects
  fowner:=owner;
  offsets:=tlist.create;

  TOffsetInfo.Create(self);

  parent:=owner;

  setupPositionsAndSizes;
end;

{ Tformaddresschange }

procedure Tformaddresschange.processaddress;
var i,j,err,err2: integer;
    currentaddress,currentaddress2: ptrUint;
    currentoffset: dword;
    read:dword;
    check: boolean;
begin
  {
  if length(pointerinfo)=0 then exit;


  for i:=length(pointerinfo)-1 downto 0 do
  begin
    try
      currentaddress:=symhandler.getaddressfromname(utf8toansi(pointerinfo[i].address.Text));
      err:=0; //no exception...
    except
      err:=1;
    end;

    if err>0 then
    begin
      //everything after this address is wrong
      editaddress.text:='????????';
      pointerinfo[i].ValueAtAddressText.Caption:=rsThisPointerPointsToAddress+' ????????';
      pointerinfo[i].FinalDestination.Caption:=rsTheOffsetYouChoseBringsItTo+' ????????';

      for j:=i-1 downto 0 do
      begin
        pointerinfo[j].address.text:=rsResultOfNextPointer;
        pointerinfo[j].ValueAtAddressText.Caption:=rsThisPointerPointsToAddress+' ????????';
        pointerinfo[j].FinalDestination.Caption:=rsTheOffsetYouChoseBringsItTo+' ????????';
      end;
      exit;
    end;

    //still here so that address was right


    if not processhandler.is64Bit then
      currentaddress:=currentaddress and $ffffffff;


    currentaddress2:=0;
    check:=readprocessmemory(processhandle,pointer(currentaddress),@currentaddress2,processhandler.pointersize,read);

    if length(pointerinfo[i].offset.text)>0 then
    begin
      if pointerinfo[i].offset.text[1]='-' then
        val('-$'+copy(pointerinfo[i].offset.text,2,length(pointerinfo[i].offset.text)),currentoffset,err2)
      else
        val('$'+pointerinfo[i].offset.text,currentoffset,err2);
    end else err2:=1;

    if (not check) or (read<processhandler.pointersize) or (err2>0) then
    begin
      //everything after this address is wrong
      if (check) or (read=processhandler.pointersize) then
        pointerinfo[i].ValueAtAddressText.Caption:=rsThisPointerPointsToAddress+' '+IntToHex(currentaddress2, 8)
      else
        pointerinfo[i].ValueAtAddressText.Caption:=rsThisPointerPointsToAddress+' ????????';

      pointerinfo[i].FinalDestination.Caption:=rsTheOffsetYouChoseBringsItTo+' ????????';
      editaddress.text:='????????';

      for j:=i-1 downto 0 do
      begin
        pointerinfo[j].address.text:=rsResultOfNextPointer;
        pointerinfo[j].ValueAtAddressText.Caption:=rsThisPointerPointsToAddress+' ????????';
        pointerinfo[j].FinalDestination.Caption:=rsTheOffsetYouChoseBringsItTo+' ????????';
      end;
      exit;
    end;


    //address+offset are correct AND the address is readable
    pointerinfo[i].ValueAtAddressText.Caption:=rsThisPointerPointsToAddress+' '+IntToHex(currentAddress2, 8);
    pointerinfo[i].FinalDestination.Caption:=rsTheOffsetYouChoseBringsItTo+' '+IntToHex(currentAddress2+currentoffset, 8);

    if i=0 then
      editaddress.text:=IntToHex(currentaddress2+currentoffset,8)
    else
      pointerinfo[i-1].address.text:=IntToHex(currentaddress2+currentoffset,8);

  end;   }
end;


procedure Tformaddresschange.offsetKeyPress(sender: TObject; var key:char);
begin
{  if key<>'-' then hexadecimal(key);
  if cbpointer.Checked then timer1.Interval:=1;   }

end;


procedure TformAddressChange.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
end;

procedure TformAddressChange.FormActivate(Sender: TObject);
begin

end;

procedure TformAddressChange.cbvarTypeChange(Sender: TObject);
begin
  pnlExtra.visible:=cbvarType.itemindex in [0,7,8];
  pnlBitinfo.visible:=cbvarType.itemindex = 0;

  AdjustHeightAndButtons;
end;



procedure TformAddressChange.setMemoryRecord(rec: TMemoryRecord);
var i: integer;
    tmp:string;
begin
  fMemoryRecord:=rec;

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
      clientwidth:=pnlBitinfo.Left+pnlBitinfo.width;
    end
    else
      clientwidth:=btnRemoveOffset.Left+btnRemoveOffset.Width+5;

  end;


  (*
  if rec.IsPointer then
  begin
    cbPointer.Checked:=true;

    for i:=1 to length(rec.pointeroffsets)-1 do btnAddOffset.Click; //add lines  (-1 because checking the cbpointer already adds one)

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

  (*
  if cbpointer.checked then
  begin

    editAddress.Enabled:=false;

    btnAddOffset.visible:=true;
    btnRemoveOffset.visible:=true;
    //create a address+offset combination and disable the normal address

    startoffset:=btnAddOffset.Top+btnAddOffset.Height+2;

    setlength(pointerinfo,1);
    pointerinfo[length(pointerinfo)-1].ValueAtAddressText:=TLabel.Create(self);
    with pointerinfo[length(pointerinfo)-1].ValueAtAddressText do
    begin
      top:=startoffset;
      left:=4;

      caption:=rsThisPointerPointsToAddress+' ????????.'; // The offset you chose brings it to ????????';
      showhint:=true;
      onkeypress:=offsetKeyPress;
      parent:=self;
    end;

    if pointerinfo[length(pointerinfo)-1].ValueAtAddressText.AutoSizeDelayed then delayedpointerresize:=true;


    pointerinfo[length(pointerinfo)-1].FinalDestination:=TLabel.Create(self);
    with pointerinfo[length(pointerinfo)-1].FinalDestination do
    begin
      top:=startoffset;
      left:=pointerinfo[length(pointerinfo)-1].ValueAtAddressText.left+pointerinfo[length(pointerinfo)-1].ValueAtAddressText.width+20;
      caption:=rsTheOffsetYouChoseBringsItTo+' ????????';
      showhint:=true;
      onkeypress:=offsetKeyPress;
      parent:=self;
    end;

    inputoffset:=startoffset+pointerinfo[length(pointerinfo)-1].ValueAtAddressText.height;

    pointerinfo[length(pointerinfo)-1].addresstext:=Tlabel.Create(self);
    with pointerinfo[length(pointerinfo)-1].addresstext do
    begin
      top:=inputoffset+2;
      left:=4;
      caption:=rsAddressOfPointer;
      parent:=self;
    end;  

    pointerinfo[length(pointerinfo)-1].address:=TEdit.create(self);
    with pointerinfo[length(pointerinfo)-1].address do
    begin
      top:=inputoffset;
      left:=pointerinfo[length(pointerinfo)-1].addresstext.left+pointerinfo[length(pointerinfo)-1].addresstext.width+3;
      width:=105;
      onkeypress:=editAddress.onkeypress;
      parent:=self;
    end;

    pointerinfo[length(pointerinfo)-1].offsettext:=Tlabel.create(self);
    with pointerinfo[length(pointerinfo)-1].offsettext do
    begin
      top:=inputoffset+2;
      left:=pointerinfo[length(pointerinfo)-1].FinalDestination.left;
      caption:=rsOffsetHex;
      parent:=self;
    end;

    pointerinfo[length(pointerinfo)-1].offset:=TEdit.create(self);
    with pointerinfo[length(pointerinfo)-1].offset do
    begin
      top:=inputoffset;
      left:=pointerinfo[length(pointerinfo)-1].offsettext.left+pointerinfo[length(pointerinfo)-1].offsettext.width+5;
      width:=70;
      text:='0';
      hint:=rsFillInTheNrOfBytesAfterTheLocationThePointerPoints;
      showhint:=true;
      onkeypress:=offsetKeyPress;
      parent:=self;
    end;

    a:=pointerinfo[length(pointerinfo)-1].FinalDestination.left;
    b:=pointerinfo[length(pointerinfo)-1].FinalDestination.width;

    clientwidth:=a+b+5;


  end
  else
  begin
    if memoryrecord.VarType=vtAutoAssembler then
    begin
      pnlBitinfo.Visible:=true;
      clientwidth:=pnlBitinfo.left+pnlBitinfo.Width+5;
    end else clientwidth:=editaddress.Left+editaddress.Width+5;

    clientheight:=cbPointer.Top+cbPointer.Height+8+btnOk.Height+8;
    editaddress.enabled:=true;
    btnAddOffset.visible:=false;
    btnRemoveOffset.visible:=false;

    for i:=0 to length(PointerInfo)-1 do
    begin
      pointerinfo[i].addresstext.free;
      pointerinfo[i].address.Free;
      pointerinfo[i].offsettext.Free;
      pointerinfo[i].offset.Free;
      pointerinfo[i].ValueAtAddressText.Free;
      pointerinfo[i].FinalDestination.Free;
    end;

    setlength(pointerinfo,0);

  end;
  AdjustHeightAndButtons;  *)
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

  if pointerinfo.count>0 then
  begin
  (*  //adjust the pointerline start addresses
    {for i:=0 to length(pointerinfo)-1 do
      pointerinfo[i].}

    btnok.top:=pointerinfo[length(pointerinfo)-1].address.top+pointerinfo[length(pointerinfo)-1].address.height+6  *)
  end
  else
    btnok.top:=cbPointer.top+cbPointer.height+6;

  btnCancel.top:=btnok.top;
  clientheight:=btncancel.top+btnCancel.height+6;
end;

procedure TformAddressChange.btnRemoveOffsetClick(Sender: TObject);
var
  rowheight: integer;
  i: integer;
  oldmethod: boolean;
begin
  (*
  if length(pointerinfo)=1 then cbPointer.checked:=false
  else
  begin
    rowheight:=pointerinfo[length(pointerinfo)-1].ValueAtAddressText.height;
    rowheight:=rowheight+pointerinfo[length(pointerinfo)-1].address.height;
    inc(rowheight,2);

    pointerinfo[length(pointerinfo)-2].address.enabled:=true;

    oldmethod:=formsettings.cbOldPointerAddMethod.checked;
    if (((GetKeyState(VK_CONTROL) shr 15) and 1)=1) then oldmethod:=not oldmethod;
    if not oldmethod then
    begin
      pointerinfo[length(pointerinfo)-2].address.text:=pointerinfo[length(pointerinfo)-1].address.text;
      for i:=0 to length(pointerinfo)-2 do
        pointerinfo[i].offset.text:=pointerinfo[i+1].offset.text;
    end
    else
      pointerinfo[length(pointerinfo)-2].address.text:='';

    pointerinfo[length(pointerinfo)-1].addresstext.free;
    pointerinfo[length(pointerinfo)-1].address.free;
    pointerinfo[length(pointerinfo)-1].offsettext.free;
    pointerinfo[length(pointerinfo)-1].offset.free;
    pointerinfo[length(pointerinfo)-1].valueataddresstext.free;
    pointerinfo[length(pointerinfo)-1].FinalDestination.free;


    setlength(pointerinfo,length(pointerinfo)-1);
    AdjustHeightAndButtons; //height:=height-rowheight;

  end;*)
end;

procedure TformAddressChange.btnAddOffsetClick(Sender: TObject);
var
  rowheight: integer;
  i: integer;
  oldaddress: string;
  oldmethod: boolean;
begin
  (*
  rowheight:=pointerinfo[length(pointerinfo)-1].ValueAtAddressText.height;
  rowheight:=rowheight+pointerinfo[length(pointerinfo)-1].address.height;
  inc(rowheight,2);

  oldaddress:=pointerinfo[length(pointerinfo)-1].address.text;
  pointerinfo[length(pointerinfo)-1].address.text:=rsResultOfNextPointer;
  pointerinfo[length(pointerinfo)-1].address.enabled:=false;

  setlength(pointerinfo,length(pointerinfo)+1);

  pointerinfo[length(pointerinfo)-1].addresstext:=Tlabel.Create(self);
  with pointerinfo[length(pointerinfo)-1].addresstext do
  begin
    top:=pointerinfo[length(pointerinfo)-2].addresstext.top+rowheight;
    left:=4;
    caption:=rsAddressOfPointer;
    parent:=self;
  end;

  pointerinfo[length(pointerinfo)-1].address:=TEdit.create(self);
  with pointerinfo[length(pointerinfo)-1].address do
  begin
    top:=pointerinfo[length(pointerinfo)-2].address.top+rowheight;
    left:=pointerinfo[length(pointerinfo)-2].address.left;
    width:=105;
    onkeypress:=editAddress.onkeypress;
    parent:=self;
  end;

  pointerinfo[length(pointerinfo)-1].offsettext:=Tlabel.create(self);
  with pointerinfo[length(pointerinfo)-1].offsettext do
  begin
    top:=pointerinfo[length(pointerinfo)-2].offsettext.top+rowheight;
    left:=pointerinfo[length(pointerinfo)-2].offsettext.left;
    caption:=rsOffsetHex;
    parent:=self;
  end;

  pointerinfo[length(pointerinfo)-1].offset:=TEdit.create(self);
  with pointerinfo[length(pointerinfo)-1].offset do
  begin
    top:=pointerinfo[length(pointerinfo)-2].offset.top+rowheight;
    left:=pointerinfo[length(pointerinfo)-2].offset.left;
    width:=70;
    text:='0';
    hint:=rsFillInTheNrOfBytesAfterTheLocationThePointerPoints;
    showhint:=true;
    onkeypress:=offsetKeyPress;
    parent:=self;
  end;

  pointerinfo[length(pointerinfo)-1].ValueAtAddressText:=TLabel.Create(self);
  with pointerinfo[length(pointerinfo)-1].ValueAtAddressText do
  begin
    top:=pointerinfo[length(pointerinfo)-2].ValueAtAddressText.top+rowheight;
    left:=pointerinfo[length(pointerinfo)-2].ValueAtAddressText.left;
    caption:=rsThisPointerPointsToAddress+' ????????.'; // The offset you chose brings it to ????????';
    showhint:=true;
    onkeypress:=offsetKeyPress;
    parent:=self;
  end;

  pointerinfo[length(pointerinfo)-1].FinalDestination:=TLabel.Create(self);
  with pointerinfo[length(pointerinfo)-1].FinalDestination do
  begin
    top:=pointerinfo[length(pointerinfo)-2].FinalDestination.top+rowheight;
    left:=pointerinfo[length(pointerinfo)-2].FinalDestination.left;
    caption:=rsTheOffsetYouChoseBringsItTo+' ????????';
    showhint:=true;
    onkeypress:=offsetKeyPress;
    parent:=self;
  end;


  oldmethod:=formsettings.cbOldPointerAddMethod.checked;
  if (((GetKeyState(VK_CONTROL) shr 15) and 1)=1) then oldmethod:=not oldmethod;
  if not oldmethod then
  begin
    for i:=length(pointerinfo)-1 downto 1 do
      pointerinfo[i].offset.Text:=pointerinfo[i-1].offset.text;

    pointerinfo[0].offset.Text:='0';
    pointerinfo[length(pointerinfo)-1].address.Text:=oldaddress;
  end;
    
  AdjustHeightAndButtons; //height:=height+rowheight;     *)
end;

procedure TformAddressChange.btnOkClick(Sender: TObject);
var bit: integer;
    address: dword;
    err:integer;

    paddress: dword;
    offsets: array of integer;

    i: integer;
begin
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

  modalresult:=mrok;
end;

procedure TformAddressChange.editAddressKeyPress(Sender: TObject;
  var Key: Char);
begin
  hexadecimal(key);
  if cbpointer.Checked then timer1.Interval:=1;
  
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
  if delayedpointerresize then
    DelayedResize;

  timer2.enabled:=false;
end;

initialization
  {$i formAddressChangeUnit.lrs}

end.
