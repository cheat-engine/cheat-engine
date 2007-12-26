unit formAddressChangeUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,cefuncproc,newkernelhandler,symbolhandler;

type TPointerInfo=record
  addresstext:tlabel;
  address:Tedit;
  offsettext: tlabel;
  offset: tedit;
  ValueAtAddressText:Tlabel;
  FinalDestination: Tlabel;
end;

type
  TformAddressChange = class(TForm)
    editAddress: TEdit;
    Button1: TButton;
    Button2: TButton;
    cbPointer: TCheckBox;
    Label1: TLabel;
    BitPanel: TPanel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    RadioButton8: TRadioButton;
    Label2: TLabel;
    Button3: TButton;
    Button4: TButton;
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure cbPointerClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure editAddressKeyPress(Sender: TObject; var Key: Char);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    pointerinfo: array of TPointerinfo; 
    procedure offsetKeyPress(sender: TObject; var key:char);
    procedure processaddress;
  public
    { Public declarations }
    index: integer;
    index2: integer;
  end;

var
  formAddressChange: TformAddressChange;

implementation

uses MainUnit;

{$R *.dfm}

procedure Tformaddresschange.processaddress;
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
      err:=0; //no exception...
    except
      err:=1;
    end;

    if err>0 then
    begin
      //everything after this address is wrong
      editaddress.text:='????????';
      pointerinfo[i].ValueAtAddressText.Caption:='This pointer points to address ????????';
      pointerinfo[i].FinalDestination.Caption:='The offset you chose brings it to ????????';

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
      editaddress.text:='????????';

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
      editaddress.text:=IntToHex(currentaddress2+currentoffset,8)
    else
      pointerinfo[i-1].address.text:=IntToHex(currentaddress2+currentoffset,8);

  end;
end;


procedure Tformaddresschange.offsetKeyPress(sender: TObject; var key:char);
begin
  if key<>'-' then hexadecimal(key);
  if cbpointer.Checked then timer1.Interval:=1;

end;


procedure TformAddressChange.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
end;

procedure TformAddressChange.FormShow(Sender: TObject);
var i: integer;
    tmp:string;
begin
 // if memrec[scrollbar1.position+(sender as TLabel).tag].VarType=5 then //bit
  //  adr:=inputbox('Address','Give the new address: (use ^ to specify the startbit)',address[(sender as TLabel).tag].caption);
  if mainform.memrec[index].IsPointer then
  begin
    tmp:=mainform.address[index2].Caption;
    if tmp='P->????????' then
      editaddress.text:='00000000'
    else
      editaddress.text:=copy(tmp,4,length(tmp));
      
    cbPointer.Checked:=true;
  end
  else
  begin
    if mainform.memrec[index].interpretableaddress<>'' then
      editaddress.Text:=mainform.memrec[index].interpretableaddress
    else
      editaddress.Text:=IntToHex(mainform.memrec[index].address,8);

    if mainform.memrec[index].VarType=5 then
    begin
      bitpanel.Visible:=true;

      case mainform.memrec[index].Bit of
        0: radiobutton1.checkeD:=true;
        1: radiobutton2.checkeD:=true;
        2: radiobutton3.checkeD:=true;
        3: radiobutton4.checkeD:=true;
        4: radiobutton5.checkeD:=true;
        5: radiobutton6.checkeD:=true;
        6: radiobutton7.checkeD:=true;
        7: radiobutton8.checkeD:=true;
      end;

      width:=288;
    end else width:=158;
  end;

  if mainform.memrec[index].IsPointer then
  begin
    cbPointer.Checked:=true;
    for i:=1 to length(mainform.memrec[index].pointers)-1 do button3.Click; //add lines

    //fill the lines
    for i:=0 to length(pointerinfo)-1 do
    begin
      pointerinfo[i].offset.text:=IntToHex(mainform.memrec[index].pointers[i].offset,1);
      if i=length(pointerinfo)-1 then
      begin
        if mainform.memrec[index].pointers[i].Interpretableaddress<>'' then
          pointerinfo[i].address.text:=mainform.memrec[index].pointers[i].Interpretableaddress
        else
          pointerinfo[i].address.text:=IntToHex(mainform.memrec[index].pointers[i].Address,8);
      end;
    end;

    if mainform.memrec[index].pointers[length(mainform.memrec[index].pointers)-1].Interpretableaddress<>'' then
      pointerinfo[length(pointerinfo)-1].address.text:=mainform.memrec[index].pointers[length(mainform.memrec[index].pointers)-1].Interpretableaddress;
  end;

  processaddress;  
end;

procedure TformAddressChange.cbPointerClick(Sender: TObject);
var i: integer;
begin
  if cbpointer.checked then
  begin
    width:=435;
    editAddress.Enabled:=false;

    button3.visible:=true;
    button4.visible:=true;

    //create a address+offset combination and disable the normal address

    setlength(pointerinfo,1);
    pointerinfo[length(pointerinfo)-1].addresstext:=Tlabel.Create(self);
    with pointerinfo[length(pointerinfo)-1].addresstext do
    begin
      top:=112;
      left:=4;
      caption:='Address of pointer';
      parent:=self;
    end;

    pointerinfo[length(pointerinfo)-1].address:=TEdit.create(self);
    with pointerinfo[length(pointerinfo)-1].address do
    begin
      top:=110;
      left:=92;
      width:=105;
      onkeypress:=editAddress.onkeypress;
      parent:=self;
    end;

    pointerinfo[length(pointerinfo)-1].offsettext:=Tlabel.create(self);
    with pointerinfo[length(pointerinfo)-1].offsettext do
    begin
      top:=112;
      left:=212;
      caption:='Offset (Hex)';
      parent:=self;
    end;

    pointerinfo[length(pointerinfo)-1].offset:=TEdit.create(self);
    with pointerinfo[length(pointerinfo)-1].offset do
    begin
      top:=110;
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
      top:=96;
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
      top:=96;
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
    if mainform.memrec[index].VarType=5 then
    begin
      bitpanel.Visible:=true;
      width:=288;
    end else width:=158;

    height:=134;
    editaddress.enabled:=true;
    button3.visible:=false;
    button4.visible:=false;

    for i:=0 to length(PointerInfo)-1 do
    begin
      pointerinfo[i].addresstext.free;
      pointerinfo[i].address.Free;
      pointerinfo[i].offsettext.Free;
      pointerinfo[i].offset.Free;
    end;

    setlength(pointerinfo,0);

  end;
end;

procedure TformAddressChange.Button4Click(Sender: TObject);
begin
  if length(pointerinfo)=1 then cbPointer.checked:=false
  else
  begin
    pointerinfo[length(pointerinfo)-1].addresstext.free;
    pointerinfo[length(pointerinfo)-1].address.free;
    pointerinfo[length(pointerinfo)-1].offsettext.free;
    pointerinfo[length(pointerinfo)-1].offset.free;
    pointerinfo[length(pointerinfo)-2].address.enabled:=true;
    pointerinfo[length(pointerinfo)-2].address.text:='';

    setlength(pointerinfo,length(pointerinfo)-1);
    height:=height-25;

  end;
end;

procedure TformAddressChange.Button3Click(Sender: TObject);
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
    onkeypress:=editAddress.onkeypress;
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

procedure TformAddressChange.Button1Click(Sender: TObject);
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

  if cbpointer.Checked then
  begin
    address:=0;
    paddress:=symhandler.getaddressfromname(pointerinfo[length(pointerinfo)-1].address.text);
  end
  else
  begin
    paddress:=0;
    addresS:=symhandler.getaddressfromname(editaddress.text);
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

      if error<>0 then raise exception.Create(pointerinfo[i].offset.Text+' is not a valid offset');
    end else raise exception.Create('Not all offsets have been filled in');
  end;

  mainform.memrec[index].Address:=address;
  mainform.memrec[index].Bit:=bit;
  setlength(mainform.memrec[index].pointers,length(offsets));
  for i:=0 to length(offsets)-1 do
  begin
    mainform.memrec[index].pointers[i].Address:=paddress;
    mainform.memrec[index].pointers[i].offset:=offsets[i];
  end;

  mainform.memrec[index].IsPointer:=cbpointer.Checked;
  mainform.memrec[index].Frozen:=false;

  if mainform.memrec[index].ispointer then
    mainform.memrec[index].pointers[length(mainform.memrec[index].pointers)-1].Interpretableaddress:=pointerinfo[length(pointerinfo)-1].address.text
  else
    mainform.memrec[index].interpretableaddress:=editaddress.text;

  modalresult:=mrok;
end;

procedure TformAddressChange.editAddressKeyPress(Sender: TObject;
  var Key: Char);
begin
  hexadecimal(key);
  if cbpointer.Checked then timer1.Interval:=1;
  
end;

procedure TformAddressChange.Timer1Timer(Sender: TObject);
begin
  timer1.Interval:=1000;
  if visible and cbpointer.checked then
    processaddress;
end;

end.
