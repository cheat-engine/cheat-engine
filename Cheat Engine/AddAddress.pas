unit AddAddress;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Menus,cefuncproc,newkernelhandler,symbolhandler;

type TPointerInfo=record
  addresstext:tlabel;
  address:Tedit;
  offsettext: tlabel;
  offset: tedit;
  ValueAtAddressText:Tlabel;
  FinalDestination: Tlabel;
end;

type
  TAddForm = class(TForm)
    VarType: TComboBox;
    Button1: TButton;
    Button2: TButton;
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
    Button3: TButton;
    Button4: TButton;
    Timer1: TTimer;
    cbUnicode: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure VarTypeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NewAddressKeyPress(Sender: TObject; var Key: Char);
    procedure Edit2KeyPress(Sender: TObject; var Key: Char);
    procedure cbPointerClick(Sender: TObject);
    procedure Edit3KeyPress(Sender: TObject; var Key: Char);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    PointerInfo: Array of TPointerInfo;
    procedure offsetKeyPress(Sender: TObject; var Key: Char);
    procedure processaddress;
  public
    { Public declarations }
  end;

var
  AddForm: TAddForm;

implementation

uses MainUnit;

{$R *.DFM}

procedure taddform.processaddress;
var i,j,err,err2: integer;
    currentaddress,currentaddress2,currentoffset: dword;
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
      pointerinfo[i].ValueAtAddressText.Caption:='This pointer points to address ????????';
      pointerinfo[i].FinalDestination.Caption:='The offset you chose brings it to ????????';
      newaddress.text:='????????';

      for j:=i-1 downto 0 do
      begin
        pointerinfo[j].address.text:='Result of next pointer';
        pointerinfo[j].ValueAtAddressText.Caption:='This pointer points to address ????????';
        pointerinfo[j].FinalDestination.Caption:='The offset you chose brings it to ????????';
      end;
      exit;
    end;

    //still here so that address was right

    check:=readprocessmemory(processhandle,pointer(currentaddress),@currentaddress2,4,read);

    if length(pointerinfo[i].offset.text)>0 then
    begin
      if pointerinfo[i].offset.text[1]='-' then
        val('-$'+copy(pointerinfo[i].offset.text,2,length(pointerinfo[i].offset.text)),currentoffset,err2)
      else
        val('$'+pointerinfo[i].offset.text,currentoffset,err2);
    end else err2:=1;

    if (not check) or (read<4) or (err2>0) then
    begin
      //everything after this address is wrong
      if (check) or (read=4) then
        pointerinfo[i].ValueAtAddressText.Caption:='This pointer points to address '+IntToHex(currentaddress2,8)
      else
        pointerinfo[i].ValueAtAddressText.Caption:='This pointer points to address ????????';

      pointerinfo[i].FinalDestination.Caption:='The offset you chose brings it to ????????';
      newaddress.text:='????????';

      for j:=i-1 downto 0 do
      begin
        pointerinfo[j].address.text:='Result of next pointer';
        pointerinfo[j].ValueAtAddressText.Caption:='This pointer points to address ????????';
        pointerinfo[j].FinalDestination.Caption:='The offset you chose brings it to ????????';
      end;
      exit;
    end;


    //address+offset are correct AND the address is readable
    pointerinfo[i].ValueAtAddressText.Caption:='This pointer points to address '+IntToHex(currentAddress2,8);
    pointerinfo[i].FinalDestination.Caption:='The offset you chose brings it to '+IntToHex(currentAddress2+currentoffset,8);

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

procedure TAddForm.Button2Click(Sender: TObject);
begin
  AddForm.close;
end;

procedure TAddForm.Button1Click(Sender: TObject);
var i,j,error: Integer;
    address: Dword;
    bit: Byte;
    vartype2: byte;
    nrofbits: integer;

    pt: dword;
    offsets: array of integer;

    interpretedpointer: string;
    list: tstringlist;
    baseaddress: string;

    off: string;
    tempoff: dword;

begin
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
        button3.Click;

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
    if pointerinfo[length(pointerinfo)-1].address.text='' then raise exception.Create('Please fill in a pointer address');

    try
      pt:=symhandler.getaddressfromname(pointerinfo[length(pointerinfo)-1].address.text);
    except
      raise exception.Create('The pointer address you filled in isn''t a valid address');
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

        if error<>0 then raise exception.Create(pointerinfo[i].offset.Text+' is not a valid value');
      end else raise exception.Create('You havn''t filled in all offsets');
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
    raise exception.Create(edit2.Text+' is not a valid number');
  end;

  if valuepanel.visible then
  begin
    try
      bit:=strtoint(edit1.Text);
    except
      raise exception.Create(edit1.text+' is not a valid value');
    end;
  end;

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
    else vartype2:=0;
  end;

  i:=mainform.NumberOfRecords;

  

  if i=MainForm.NumberOfRecords then
  begin
    inc(MainForm.NumberOfRecords);

    mainform.reserveMem;

    if description.text='' then
      mainform.memrec[MainForm.NumberOfRecords-1].Description:='No description'
    else
      mainform.memrec[MainForm.NumberOfRecords-1].Description:=description.text;


    mainform.memrec[MainForm.NumberOfRecords-1].Address:=address;
    mainform.memrec[MainForm.NumberOfRecords-1].interpretableaddress:=newaddress.text;


   // mainform.memrec[MainForm.NumberOfRecords-1].VarType:=Vartype.itemindex;
    //
    mainform.memrec[MainForm.NumberOfRecords-1].VarType:=vartype2;

    mainform.memrec[MainForm.NumberOfRecords-1].Bit:=bit;
    mainform.memrec[MainForm.NumberOfRecords-1].bitlength:=nrofbits;
    mainform.memrec[MainForm.NumberOfRecords-1].unicode:=cbunicode.Checked;



    {add pointer stuff}
    mainform.memrec[MainForm.NumberOfRecords-1].IsPointer:=cbpointer.Checked;
    setlength(mainform.memrec[mainform.NumberOfRecords-1].pointers,length(pointerinfo));

    for i:=0 to length(pointerinfo)-1 do
    begin
      if i=length(pointerinfo)-1 then
      begin
        mainform.memrec[mainform.NumberOfRecords-1].pointers[i].Interpretableaddress:=pointerinfo[length(pointerinfo)-1].address.text;
        mainform.memrec[mainform.NumberOfRecords-1].pointers[i].Address:=pt;
      end;

      mainform.memrec[mainform.NumberOfRecords-1].pointers[i].offset:=offsets[i];
    end;



    mainform.memrec[MainForm.NumberOfRecords-1].Frozen:=false;
    mainform.memrec[MainForm.NumberOfRecords-1].FrozenValue:=0;
    MainForm.FrozenFvalue[MainForm.NumberOfRecords-1]:=0;
    mainform.FrozenStrings[MainForm.NumberOfRecords-1]:='';
    mainform.Hotkeystrings[Mainform.NumberOfRecords-1]:='';
    mainform.Hotkeys[MainForm.NumberOfRecords-1]:=-1;

    MainForm.UpdateScreen;
    MainForm.updatelist;

    addform.close;
  end else showmessage('This address is already in the list');
end;

procedure TAddForm.VarTypeChange(Sender: TObject);
begin
  bitpanel.Visible:=Vartype.itemindex=0;
  if vartype.ItemIndex=7 then
  begin
    valuepanel.visible:=true;
    valuepanel.BringToFront;
    label12.Caption:='Nr. of Characters';
  end
  else if vartype.itemindex=8 then
  begin
    valuepanel.visible:=true;
    label12.Caption:='Nr. of Bytes';
  end
  else valuepanel.visible:=false;
end;

procedure TAddForm.FormShow(Sender: TObject);
begin
  addform.cbPointer.checked:=false;
  NewAddress.SetFocus;
  NewAddress.SelectAll;
  description.Text:='No description';
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
begin
  if cbpointer.checked then
  begin

    newaddress.Enabled:=false;

    button3.visible:=true;
    button4.visible:=true;

    //create a address+offset combination and disable the normal address

    setlength(pointerinfo,1);
    pointerinfo[length(pointerinfo)-1].addresstext:=Tlabel.Create(self);
    with pointerinfo[length(pointerinfo)-1].addresstext do
    begin
      top:=142;
      left:=4;
      caption:='Address of pointer';
      parent:=self;
    end;

    pointerinfo[length(pointerinfo)-1].address:=TEdit.create(self);
    with pointerinfo[length(pointerinfo)-1].address do
    begin
      top:=140;
      left:=92;
      width:=105;
      onkeypress:=newaddress.onkeypress;
      parent:=self;
    end;

    pointerinfo[length(pointerinfo)-1].offsettext:=Tlabel.create(self);
    with pointerinfo[length(pointerinfo)-1].offsettext do
    begin
      top:=142;
      left:=212;
      caption:='Offset (Hex)';
      parent:=self;
    end;

    pointerinfo[length(pointerinfo)-1].offset:=TEdit.create(self);
    with pointerinfo[length(pointerinfo)-1].offset do
    begin
      top:=140;
      left:=275;
      width:=70;
      text:='0';
      hint:='Fill in the nr. of bytes after the location the pointer points to';
      showhint:=true;
      onkeypress:=offsetKeyPress;
      parent:=self;
    end;

    pointerinfo[length(pointerinfo)-1].ValueAtAddressText:=TLabel.Create(self);
    with pointerinfo[length(pointerinfo)-1].ValueAtAddressText do
    begin
      top:=126;
      left:=4;
//      width:=57;
      caption:='This pointer points to address ????????.';// The offset you chose brings it to ????????';
      showhint:=true;
      onkeypress:=offsetKeyPress;
      parent:=self;
    end;

    pointerinfo[length(pointerinfo)-1].FinalDestination:=TLabel.Create(self);
    with pointerinfo[length(pointerinfo)-1].FinalDestination do
    begin
      top:=126;
      left:=212;
//      width:=57;
      caption:='The offset you chose brings it to ????????';
      showhint:=true;
      onkeypress:=offsetKeyPress;
      parent:=self;
    end;



    height:=height+66;
  end
  else
  begin
    height:=170;
    newaddress.enabled:=true;
    button3.visible:=false;
    button4.visible:=false;

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

end;

procedure TAddForm.Edit3KeyPress(Sender: TObject; var Key: Char);
begin
  if key<>'-' then hexadecimal(key);
end;

procedure TAddForm.Button4Click(Sender: TObject);
begin
  if length(pointerinfo)=1 then cbPointer.checked:=false
  else
  begin
    pointerinfo[length(pointerinfo)-1].addresstext.free;
    pointerinfo[length(pointerinfo)-1].address.free;
    pointerinfo[length(pointerinfo)-1].offsettext.free;
    pointerinfo[length(pointerinfo)-1].offset.free;
    pointerinfo[length(pointerinfo)-1].valueataddresstext.free;
    pointerinfo[length(pointerinfo)-1].FinalDestination.free;
    pointerinfo[length(pointerinfo)-2].address.enabled:=true;
    pointerinfo[length(pointerinfo)-2].address.text:='';

    setlength(pointerinfo,length(pointerinfo)-1);
    height:=height-25;

  end;
end;

procedure TAddForm.Button3Click(Sender: TObject);
begin
  pointerinfo[length(pointerinfo)-1].address.text:='result of next pointer';
  pointerinfo[length(pointerinfo)-1].address.enabled:=false;

  setlength(pointerinfo,length(pointerinfo)+1);

  pointerinfo[length(pointerinfo)-1].addresstext:=Tlabel.Create(self);
  with pointerinfo[length(pointerinfo)-1].addresstext do
  begin
    top:=pointerinfo[length(pointerinfo)-2].addresstext.top+36;
    left:=4;
    caption:='Address of pointer';
    parent:=self;
  end;

  pointerinfo[length(pointerinfo)-1].address:=TEdit.create(self);
  with pointerinfo[length(pointerinfo)-1].address do
  begin
    top:=pointerinfo[length(pointerinfo)-2].address.top+36;
    left:=92;
    width:=105;
    onkeypress:=newaddress.onkeypress;
    parent:=self;
  end;

  pointerinfo[length(pointerinfo)-1].offsettext:=Tlabel.create(self);
  with pointerinfo[length(pointerinfo)-1].offsettext do
  begin
    top:=pointerinfo[length(pointerinfo)-2].offsettext.top+36;
    left:=212;
    caption:='Offset (Hex)';
    parent:=self;
  end;

  pointerinfo[length(pointerinfo)-1].offset:=TEdit.create(self);
  with pointerinfo[length(pointerinfo)-1].offset do
  begin
    top:=pointerinfo[length(pointerinfo)-2].offset.top+36;
    left:=275;
    width:=70;
    text:='0';
    hint:='Fill in the nr. of bytes after the location the pointer points to';
    showhint:=true;
    onkeypress:=offsetKeyPress;
    parent:=self;
  end;

  pointerinfo[length(pointerinfo)-1].ValueAtAddressText:=TLabel.Create(self);
  with pointerinfo[length(pointerinfo)-1].ValueAtAddressText do
  begin
    top:=pointerinfo[length(pointerinfo)-2].ValueAtAddressText.top+36;
    left:=4;
    caption:='This pointer points to address ????????.';// The offset you chose brings it to ????????';
    showhint:=true;
    onkeypress:=offsetKeyPress;
    parent:=self;
  end;

  pointerinfo[length(pointerinfo)-1].FinalDestination:=TLabel.Create(self);
  with pointerinfo[length(pointerinfo)-1].FinalDestination do
  begin
    top:=pointerinfo[length(pointerinfo)-2].FinalDestination.top+36;
    left:=212;
    caption:='The offset you chose brings it to ????????';
    showhint:=true;
    onkeypress:=offsetKeyPress;
    parent:=self;
  end;

  height:=height+36;
end;

procedure TAddForm.Timer1Timer(Sender: TObject);
begin
  timer1.Interval:=1000;
  if visible and cbpointer.checked then
    processaddress;
end;

end.














