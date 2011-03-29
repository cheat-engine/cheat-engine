unit AddAddress;

{$MODE Delphi}

interface

uses
  windows, LResources , LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Menus,CEFuncProc,NewKernelHandler,symbolhandler, customTypeHandler;

type TPointerInfo=record
  addresstext:tlabel;
  address:Tedit;
  offsettext: tlabel;
  offset: tedit;
  ValueAtAddressText:Tlabel;
  FinalDestination: Tlabel;
end;

type

  { TAddForm }

  TAddForm = class(TForm)
    VarType: TComboBox;
    btnOk: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Description: TEdit;
    Label3: TLabel;
    BitPanel: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    RadioButton8: TRadioButton;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    ValuePanel: TPanel;
    Label12: TLabel;
    Edit1: TEdit;
    NewAddress: TEdit;
    Edit2: TEdit;
    cbPointer: TCheckBox;
    btnAddOffset: TButton;
    btnRemoveOffset: TButton;
    Timer1: TTimer;
    cbUnicode: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure VarTypeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NewAddressKeyPress(Sender: TObject; var Key: Char);
    procedure Edit2KeyPress(Sender: TObject; var Key: Char);
    procedure cbPointerClick(Sender: TObject);
    procedure Edit3KeyPress(Sender: TObject; var Key: Char);
    procedure btnRemoveOffsetClick(Sender: TObject);
    procedure btnAddOffsetClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    PointerInfo: Array of TPointerInfo;
    procedure offsetKeyPress(Sender: TObject; var Key: Char);
    procedure processaddress;
    procedure RefreshCustomTypes;
    procedure DefaultNoPointerview;
    procedure AdjustForPointers;
  public
    { Public declarations }
  end;

var
  AddForm: TAddForm;

implementation

uses MainUnit, formsettingsunit;

resourcestring
  rsThisPointerPointsToAddress = 'This pointer points to address';
  rsTheOffsetYouChoseBringsItTo = 'The offset you chose brings it to';
  rsResultOfNextPointer = 'Result of next pointer';
  rsPleaseFillInAPointerAddress = 'Please fill in a pointer address';
  rsThePointerAddressYouFilledInIsnTAValidAddress = 'The pointer address you filled in isn''t a valid address';
  rsIsNotAValidValue = '%s is not a valid value';
  rsYouHavnTFilledInAllOffsets = 'You havn''t filled in all offsets';
  rsIsNotAValidNumber = '%s is not a valid number';
  rsNrOfCharacters = 'Nr. of Characters';
  rsNrOfBytes = 'Nr. of Bytes';
  rsNoDescription = 'No description';
  rsAddressOfPointer = 'Address of pointer';
  rsOffsetHex = 'Offset (Hex)';
  rsFillInTheNrOfBytesAfterTheLocationThePointerPoints = 'Fill in the nr. of bytes after the location the pointer points to';


procedure taddform.processaddress;
var i,j,err,err2: integer;
    currentaddress,currentaddress2: ptrUint;
    currentoffset: dword;
    read:dword;
    check: boolean;
begin
  if length(pointerinfo)=0 then exit;


  for i:=length(pointerinfo)-1 downto 0 do
  begin
    try
      currentaddress:=symhandler.getaddressfromname(pointerinfo[i].address.Text);
      err:=0;
    except
      err:=1;
    end;


    if err>0 then
    begin
      //everything after this address is wrong
      pointerinfo[i].ValueAtAddressText.Caption:=rsThisPointerPointsToAddress+' ????????';
      pointerinfo[i].FinalDestination.Caption:=rsTheOffsetYouChoseBringsItTo+' ????????';
      newaddress.text:='????????';

      for j:=i-1 downto 0 do
      begin
        pointerinfo[j].address.text:=rsResultOfNextPointer;
        pointerinfo[j].ValueAtAddressText.Caption:=rsThisPointerPointsToAddress+' ????????';
        pointerinfo[j].FinalDestination.Caption:=rsTheOffsetYouChoseBringsItTo+' ????????';
      end;
      exit;
    end;

    //still here so that address was right

    currentaddress2:=0;

    if not processhandler.is64Bit then
      currentaddress:=currentaddress and $ffffffff;

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
      newaddress.text:='????????';

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
      newaddress.text:=IntToHex(currentaddress2+currentoffset,8)
    else
      pointerinfo[i-1].address.text:=IntToHex(currentaddress2+currentoffset,8);

  end;

end;


procedure TaddForm.offsetKeyPress(sender: TObject; var key:char);
begin
  if key<>'-' then hexadecimal(key);
  if cbpointer.Checked then timer1.Interval:=1;
end;

procedure TAddForm.FormCreate(Sender: TObject);
begin
  vartype.ItemIndex:=3;
  edit2.Text:='1';
  NewAddress.text:='00400000';
  cbunicode.Checked:=false;
end;

procedure TAddForm.btnCancelClick(Sender: TObject);
begin
  AddForm.close;
end;

procedure TAddForm.Button1Click(Sender: TObject);
var i,j,error: Integer;
    address: ptrUint;
    bit: Byte;
    vartype2: byte;
    nrofbits: integer;

    pt: ptrUint;
    offsets: array of dword;

    interpretedpointer: string;
    list: tstringlist;
    baseaddress: string;

    off: string;
    tempoff: dword;

    vartype3: TVariableType;
    stringlength: integer;
    customtypename: string;
begin
  customtypename:='';
  {
  5.4: Parse the string and see if it complies to the [[[[xxxx+xx]+xx]+xx]+xx] form, and if so, change it to a pointer of this type before progressing
  }
  list:=tstringlist.Create;
  try
    if symhandler.ParseAsPointer(newaddress.text,list) then
    begin
      cbpointer.Checked:=true;

      baseaddress:=list[0];
      setlength(offsets,list.count-1);
      for i:=1 to list.Count-1 do //start from the first offset
      begin
        off:=copy(list[i],2,length(list[i]));
        try
          offsets[i-1]:=strtoint('$'+off);
        except
          exit;
        end;
        if list[i][1]='-' then
          offsets[i-1]:=-offsets[i-1];
      end;

      for i:=1 to length(offsets)-1 do
        btnAddOffset.Click;

      for i:=0 to length(offsets)-1 do
        pointerinfo[length(pointerinfo)-1-i].offset.Text:=inttohex(offsets[i],1);


      pointerinfo[length(pointerinfo)-1].address.text:=baseaddress;
      exit;
    end;
  finally
    list.free;
  end;

  setlength(offsets,0);
  pt:=0;

  if cbpointer.checked then
  begin
    if pointerinfo[length(pointerinfo)-1].address.text='' then raise exception.Create(rsPleaseFillInAPointerAddress);

    try
      pt:=symhandler.getaddressfromname(pointerinfo[length(pointerinfo)-1].address.text);
    except
      raise exception.Create(rsThePointerAddressYouFilledInIsnTAValidAddress+'');
    end;

    //make sure the offsets are valid
    setlength(offsets,length(pointerinfo));
    for i:=0 to length(pointerinfo)-1 do
    begin
      if (pointerinfo[i].offset.Text<>'') then
      begin
        if pointerinfo[i].offset.Text[1]='-' then
          val('-$'+copy(pointerinfo[i].offset.Text,2,length(pointerinfo[i].offset.Text)-1),offsets[i],error)
        else
          val('$'+pointerinfo[i].offset.Text,offsets[i],error);

        if error<>0 then raise exception.Create(Format(rsIsNotAValidValue, [pointerinfo[i].offset.Text]));
      end else raise exception.Create(rsYouHavnTFilledInAllOffsets);
    end;

    try
      address:=symhandler.getaddressfromname(NewAddress.text);
    except
      //don't complain
    end;
  end else address:=symhandler.getaddressfromname(NewAddress.text); //complain when not valid



  if RadioButton1.checked then bit:=0 else
  if RadioButton2.checked then bit:=1 else
  if RadioButton3.checked then Bit:=2 else
  if RadioButton4.checked then Bit:=3 else
  if RadioButton5.checked then Bit:=4 else
  if RadioButton6.checked then Bit:=5 else
  if RadioButton7.checked then Bit:=6 else
                               Bit:=7;

  try
    nrofbits:=StrToInt(edit2.Text);
  except
    raise exception.Create(Format(rsIsNotAValidNumber, [edit2.Text]));
  end;

  if valuepanel.visible then
  begin
    try
      bit:=strtoint(edit1.Text);
      stringlength:=strtoint(edit1.Text);
    except
      raise exception.Create(Format(rsIsNotAValidValue, [edit1.text]));
    end;
  end;

  {
  0:Binary
  1:Byte
  2:2 Bytes
  3:4 Bytes
  4:8 Bytes
  5:Float
  6:Double
  7:Text
  8:Array of Byte

  }

  case vartype.Itemindex of
    0  :  VarType2:=5;
    1  :  VarType2:=0;
    2  :  VarType2:=1;
    3  :  VarType2:=2;
    4  :  VarType2:=6;
    5  :  VarType2:=3;
    6  :  VarType2:=4;
    7  :  Vartype2:=7;
    8  :  VarType2:=8;
    else
    begin
      if vartype.Items.Objects[vartype.Itemindex]<>nil then
      begin
        vartype2:=10;
        customtypename:=TCustomType(vartype.Items.Objects[vartype.Itemindex]).name;
      end;
    end;
  end;

  vartype3:=OldVarTypeToNewVarType(VarType2);

  if length(pointerinfo)>0 then
    baseaddress:=pointerinfo[length(pointerinfo)-1].address.text
  else
    baseaddress:=newaddress.text;

  Mainform.addresslist.addaddress(description.text, baseaddress, offsets, length(offsets), vartype3,customtypename, stringlength,bit, cbunicode.checked );

  addform.close;

end;

procedure TAddForm.VarTypeChange(Sender: TObject);
begin
  bitpanel.Visible:=Vartype.itemindex=0;
  if vartype.ItemIndex=7 then
  begin
    valuepanel.visible:=true;
    valuepanel.BringToFront;
    label12.Caption:=rsNrOfCharacters;
  end
  else if vartype.itemindex=8 then
  begin
    valuepanel.visible:=true;
    label12.Caption:=rsNrOfBytes;
  end
  else valuepanel.visible:=false;

  cbUnicode.visible:=vartype.itemindex=7;
end;

procedure TAddForm.RefreshCustomTypes;
var old:  TNotifyEvent;
    i: integer;

    oldtype: string;
begin
  old:=VarType.OnChange;
  VarType.OnChange:=nil;

  oldtype:=vartype.text;

  i:=0;
  //first clear all custom types
  while i<vartype.Items.Count do
  begin
    if vartype.Items.Objects[i]<>nil then
      vartype.Items.Delete(i)
    else
      inc(i);
  end;

  //now add the custom types back
  for i:=0 to customtypes.Count-1 do
    vartype.Items.AddObject(TcustomType(customtypes[i]).name,customtypes[i]);


  //set the selected index back if possible
  i:=vartype.Items.IndexOf(oldtype);
  if i<>-1 then
    vartype.ItemIndex:=i
  else
    vartype.itemindex:=3; //4 byte

  VarType.OnChange:=old;
end;

procedure TAddForm.AdjustForPointers;
var pos: integer;
begin
  if length(pointerinfo)>0 then
  begin
    pos:=pointerinfo[length(pointerinfo)-1].address.top+pointerinfo[length(pointerinfo)-1].address.height+2;

    btnOk.top:=pos;
    btnCancel.top:=btnOk.top;

    clientheight:=btnOk.top+btnOk.height+8;
  end;

end;

procedure TAddForm.DefaultNoPointerView;
var i: integer;
begin
  btnOk.top:=cbPointer.top+cbPointer.height+6;
  btnCancel.top:=btnOk.top;

  clientheight:=btnOk.top+btnOk.height+8;
  newaddress.enabled:=true;
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

procedure TAddForm.FormShow(Sender: TObject);
begin
  cbPointer.checked:=false;
  NewAddress.SetFocus;
  NewAddress.SelectAll;
  description.Text:=rsNoDescription;

  RefreshCustomTypes;


  DefaultNoPointerView;
end;

procedure TAddForm.NewAddressKeyPress(Sender: TObject; var Key: Char);
begin
  if cbpointer.checked then timer1.Interval:=1;
end;

procedure TAddForm.Edit2KeyPress(Sender: TObject; var Key: Char);
begin
  decimal(key);
end;

procedure TAddForm.cbPointerClick(Sender: TObject);
var i: integer;
    startoffset,inputoffset,rowheight: integer;
begin
  if cbpointer.checked then
  begin
    newaddress.Enabled:=false;

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
      onkeypress:=newaddress.onkeypress;
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

    AdjustForPointers;

  end
  else
  begin
    DefaultNoPointerView;
  end;

end;

procedure TAddForm.Edit3KeyPress(Sender: TObject; var Key: Char);
begin
  if key<>'-' then hexadecimal(key);
end;

procedure TAddForm.btnRemoveOffsetClick(Sender: TObject);
var
  rowheight: integer;
  i: integer;
  oldmethod: boolean;
begin
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
   // height:=height-rowheight;
    AdjustForPointers;

  end;
end;

procedure TAddForm.btnAddOffsetClick(Sender: TObject);
var
  rowheight: integer;
  i: integer;
  oldaddress: string;
  oldmethod: boolean;
begin
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
    onkeypress:=newaddress.onkeypress;
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
  

  AdjustForPointers;
end;

procedure TAddForm.Timer1Timer(Sender: TObject);
begin
  timer1.Interval:=1000;
  if visible and cbpointer.checked then
    processaddress;
end;

initialization
  {$i AddAddress.lrs}

end.




