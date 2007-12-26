unit Standaloneunit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtDlgs, Menus, ExtCtrls, CheckLst, Buttons,ceclient,unit2,shellapi;

type
  TStandalone = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    ImageSize: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Label5: TLabel;
    SpeedButton3: TSpeedButton;
    Image1: TImage;
    Label6: TLabel;
    Button1: TButton;
    CEList: TListBox;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
    Button6: TButton;
    TRlist: TCheckListBox;
    GroupBox1: TGroupBox;
    TrainerStyle1: TRadioButton;
    TrainerStyle2: TRadioButton;
    TrainerStyle3: TRadioButton;
    CheckBox1: TCheckBox;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Button7: TButton;
    TRpopuphelp: TPopupMenu;
    Help1: TMenuItem;
    CEListHelp: TPopupMenu;
    MenuItem1: TMenuItem;
    ColorDialog1: TColorDialog;
    OpenPictureDialog1: TOpenPictureDialog;
    OpenDialog1: TOpenDialog;
    Combobox1: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure CEListDblClick(Sender: TObject);
    procedure TRlistDblClick(Sender: TObject);
    procedure TRlistClickCheck(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Help1Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure Edit4KeyPress(Sender: TObject; var Key: Char);
    procedure TrainerStyle1Click(Sender: TObject);
    procedure TrainerStyle2Click(Sender: TObject);
    procedure TrainerStyle3Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
    InList: Array of MemoryRecord;
    FInList: Array of Double;
    OutList: array of MemoryRecord;
    FOutList: array of double;
    NumberOfAddressesIn,NumberOfAddressesOut: Integer;
  public
    { Public declarations }
    filename: string;
  end;

var
  Standalone: TStandalone;

implementation

uses exampletrainerstyle1unit, exampletrainerstyle2unit,
  Exampletrainerstyle3Unit;

{$R *.dfm}

procedure TStandalone.CEListDblClick(Sender: TObject);
var index: Integer;
    i,j: Integer;
    group: integer;
begin
  index:=CEList.ItemIndex;
  if index=-1 then exit;

  group:=inlist[index].Group;

  if group=0 then
  begin
    outList[NumberOfAddressesOut]:=InList[index];
    foutlist[NumberOfAddressesOut]:=FInlist[index];

    for i:=index to NumberOfAddressesIn-2 do
    begin
      InList[i]:=InList[i+1];
      finlist[i]:=finlist[i+1];
    end;


    inc(NumberOfAddressesOut);
    dec(NumberOfAddressesIn);

    celist.clear;
    for i:=0 to NumberOfAddressesIn-1 do
      CeList.Items.Add(InList[i].description);

    TRList.clear;
    for i:=0 to NumberOfAddressesOut-1 do
    begin
      TRList.Items.Add(OutList[i].description);
      if Outlist[i].Frozen then TRList.Checked[i]:=true;
    end;

    exit;
  end;

  i:=0;
  while i<numberofaddressesin do
  if inlist[i].Group=group then
  begin
    outList[NumberOfAddressesOut]:=InList[i];
    foutlist[NumberOfAddressesOut]:=FInlist[i];

    for j:=i to NumberOfAddressesIn-2 do
    begin
      InList[j]:=InList[j+1];
      finlist[j]:=finlist[j+1];
    end;


    inc(NumberOfAddressesOut);
    dec(NumberOfAddressesIn);
  end else inc(i);

  celist.clear;
  for j:=0 to NumberOfAddressesIn-1 do
    CeList.Items.Add(InList[j].description);

  TRList.clear;
  for j:=0 to NumberOfAddressesOut-1 do
  begin
    TRList.Items.Add(OutList[j].description);
    if Outlist[j].Frozen then TRList.Checked[j]:=true;
  end;

end;

procedure TStandalone.TRlistDblClick(Sender: TObject);
var index: Integer;
    i,j: Integer;
    group: integer;
begin
  index:=TRList.ItemIndex;
  if index=-1 then exit;

  group:=outlist[index].Group;

  if group=0 then
  begin
    inList[NumberOfAddressesin]:=outList[index];
    finlist[NumberOfAddressesin]:=Foutlist[index];

    for i:=index to NumberOfAddressesout-2 do
    begin
      outList[i]:=outList[i+1];
      foutlist[i]:=foutlist[i+1];
    end;


    inc(NumberOfAddressesIn);
    dec(NumberOfAddressesOut);

    celist.clear;
    for i:=0 to NumberOfAddressesIn-1 do
      CeList.Items.Add(InList[i].description);

    TRList.clear;
    for i:=0 to NumberOfAddressesOut-1 do
    begin
      TRList.Items.Add(OutList[i].description);
      if Outlist[i].Frozen then TRList.Checked[i]:=true;
    end;

    exit;
  end;

  i:=0;
  while i<numberofaddressesout do
  if outlist[i].Group=group then
  begin
    inList[NumberOfAddressesIn]:=outList[i];
    finlist[NumberOfAddressesIn]:=Foutlist[i];

    for j:=i to NumberOfAddressesOut-2 do
    begin
      outList[j]:=outList[j+1];
      foutlist[j]:=foutlist[j+1];
    end;


    inc(NumberOfAddressesIn);
    dec(NumberOfAddressesOut);
  end else inc(i);

  celist.clear;
  for j:=0 to NumberOfAddressesIn-1 do
    CeList.Items.Add(InList[j].description);

  TRList.clear;
  for j:=0 to NumberOfAddressesOut-1 do
  begin
    TRList.Items.Add(OutList[j].description);
    if Outlist[j].Frozen then TRList.Checked[j]:=true;
  end;
end;

procedure TStandalone.TRlistClickCheck(Sender: TObject);
var index: integer;
begin
  index:=TRlist.ItemIndex;

  if not outlist[index].Frozen then
  begin
    trlist.Checked[index]:=false;
    showmessage('Please freeze this address in Cheat Engine before checking this');
  end;

end;

procedure TStandalone.Button1Click(Sender: TObject);
const trainericon=402916;
var records: dword;
    UsePicture: Boolean;
    Processname: String[50];
    TrainerTitle: String[50];
    Usercomments: String[200];
    style: byte;

    TRpicture: TFilestream;
    TrainerDAT: TFileStream;
    Trainer: TFileStream;
    IconTrainer: TMemorystream;

    StartOfSettings: Int64;
    i: integer;

    groups: Grouptype;
    groupnames: array[1..4] of string[50];
    results: word;
begin
  for i:=1 to 4 do
  begin
    groupnames[i]:='';
    groups[i]:=false;
  end;

  for i:=0 to trlist.Items.Count-1 do
  begin
    if outlist[i].Group>0 then
      groups[outlist[i].group]:=true;
  end;


  for i:=1 to 4 do
  begin
    if groups[i] then
    begin
      results:=MessageDlg('Group group '+inttostr(i)+' to one selection?',mtConfirmation, [mbYes,mbNo], 0);
      if results=mrCancel then exit;
      if results=mrYes then
      begin
        groupnames[i]:=Inputbox('Groupname:','Type the name for this group:','Group '+IntTostr(i));
      end;
      if results=mrNo then groups[i]:=false;
    end;
  end;

  trpicture:=nil;
  if not fileexists('trainer.dat') then raise Exception.Create('Can''t find the file trainer.dat! Make sure it''s in the same directory as Cheat Engine');
  usepicture:=checkbox1.Checked;

  if usepicture then TRPicture:=TFileStream.Create(edit2.text,fmOpenread);
  TrainerDAT:=TFileStream.Create('trainer.dat',fmOpenRead);
  Trainer:=TFileStream.Create(filename,fmcreate);

  //copy all data from trainerdat to trainer
  Trainer.CopyFrom(trainerDAT,0);

  //send data to stream
  startofsettings:=trainer.size;

  processname:=ComboBox1.Text;
  trainer.WriteBuffer(processname,sizeof(processname));

  TrainerTitle:=Edit1.text;
  trainer.WriteBuffer(trainertitle,sizeof(trainertitle));

  UserComments:=Memo1.text;
  trainer.WriteBuffer(usercomments,sizeof(usercomments));

  records:=numberofaddressesout;
  trainer.WriteBuffer(records,4);

  style:=1;
  if trainerstyle1.checked then style:=1 else
  if trainerstyle2.checked then style:=2 else
  if trainerstyle3.checked then style:=3;

  trainer.WriteBuffer(style,sizeof(style));

  trainer.WriteBuffer(usepicture,sizeof(usepicture));

  trainer.WriteBuffer(groups,sizeof(groups));
  trainer.WriteBuffer(groupnames,sizeof(groupnames));

  //save the records
  for i:=0 to trlist.Items.Count-1 do
  begin
    outlist[i].Frozen:=trlist.checked[i];
  end;

  trainer.WriteBuffer(pointer(outlist)^,records*sizeof(memoryrecord));
  trainer.WriteBuffer(pointer(foutlist)^,records*sizeof(double));


  //save picture
  if usepicture then trainer.CopyFrom(TRPicture,0);

  //save position of the settings
  trainer.WriteBuffer(startofsettings,8);

  trainerdat.Free;
  TRPicture.Free;


  //now just set the icon
  trainer.Seek(trainericon,soFromBeginning);

  icontrainer:=TMemorystream.create;
  image1.Picture.Icon.SaveToStream(icontrainer);
  icontrainer.Seek(22,soFromBeginning);
  trainer.CopyFrom(icontrainer,744);

  icontrainer.free;
  trainer.free;

  showmessage('Trainer created!');
  standalone.close;
end;

procedure TStandalone.Help1Click(Sender: TObject);
begin
  showmessage('This shows the addresses that will be added to the trainer'+chr(13)+chr(10)+
              'If checked, it means the trainer will freeze the address to the current'+chr(13)+chr(10)+
               'state the address is right now.'+chr(13)+chr(10)+
               'If not it will get the value from the game and then freeze it when the user checks it');
end;


procedure TStandalone.MenuItem1Click(Sender: TObject);
begin
  showmessage('This shows the addresses that will be added to the trainer'+chr(13)+chr(10)+
              'If checked, it means the trainer will freeze the address to the current'+chr(13)+chr(10)+
               'state the address is right now.'+chr(13)+chr(10)+
               'If not it will get the value from the game and then freeze it when the user checks it');
end;


procedure TStandalone.Button2Click(Sender: TObject);
var index: Integer;
    i,j: Integer;
    group: integer;
begin
  index:=TRList.ItemIndex;
  if index=-1 then exit;

  group:=outlist[index].Group;

  if group=0 then
  begin
    inList[NumberOfAddressesin]:=outList[index];
    finlist[NumberOfAddressesin]:=Foutlist[index];

    for i:=index to NumberOfAddressesout-2 do
    begin
      outList[i]:=outList[i+1];
      foutlist[i]:=foutlist[i+1];
    end;


    inc(NumberOfAddressesIn);
    dec(NumberOfAddressesOut);

    celist.clear;
    for i:=0 to NumberOfAddressesIn-1 do
      CeList.Items.Add(InList[i].description);

    TRList.clear;
    for i:=0 to NumberOfAddressesOut-1 do
    begin
      TRList.Items.Add(OutList[i].description);
      if Outlist[i].Frozen then TRList.Checked[i]:=true;
    end;

    exit;
  end;

  i:=0;
  while i<numberofaddressesout do
  if outlist[i].Group=group then
  begin
    inList[NumberOfAddressesIn]:=outList[i];
    finlist[NumberOfAddressesIn]:=Foutlist[i];

    for j:=i to NumberOfAddressesOut-2 do
    begin
      outList[j]:=outList[j+1];
      foutlist[j]:=foutlist[j+1];
    end;


    inc(NumberOfAddressesIn);
    dec(NumberOfAddressesOut);
  end else inc(i);

  celist.clear;
  for j:=0 to NumberOfAddressesIn-1 do
    CeList.Items.Add(InList[j].description);

  TRList.clear;
  for j:=0 to NumberOfAddressesOut-1 do
  begin
    TRList.Items.Add(OutList[j].description);
    if Outlist[j].Frozen then TRList.Checked[j]:=true;
  end;
end;

procedure TStandalone.Button3Click(Sender: TObject);
var index: Integer;
    i,j: Integer;
    group: integer;
begin
  index:=CEList.ItemIndex;
  if index=-1 then exit;

  group:=inlist[index].Group;

  if group=0 then
  begin
    outList[NumberOfAddressesOut]:=InList[index];
    foutlist[NumberOfAddressesOut]:=FInlist[index];

    for i:=index to NumberOfAddressesIn-2 do
    begin
      InList[i]:=InList[i+1];
      finlist[i]:=finlist[i+1];
    end;


    inc(NumberOfAddressesOut);
    dec(NumberOfAddressesIn);

    celist.clear;
    for i:=0 to NumberOfAddressesIn-1 do
      CeList.Items.Add(InList[i].description);

    TRList.clear;
    for i:=0 to NumberOfAddressesOut-1 do
    begin
      TRList.Items.Add(OutList[i].description);
      if Outlist[i].Frozen then TRList.Checked[i]:=true;
    end;

    exit;
  end;

  i:=0;
  while i<numberofaddressesin do
  if inlist[i].Group=group then
  begin
    outList[NumberOfAddressesOut]:=InList[i];
    foutlist[NumberOfAddressesOut]:=FInlist[i];

    for j:=i to NumberOfAddressesIn-2 do
    begin
      InList[j]:=InList[j+1];
      finlist[j]:=finlist[j+1];
    end;


    inc(NumberOfAddressesOut);
    dec(NumberOfAddressesIn);
  end else inc(i);

  celist.clear;
  for j:=0 to NumberOfAddressesIn-1 do
    CeList.Items.Add(InList[j].description);

  TRList.clear;
  for j:=0 to NumberOfAddressesOut-1 do
  begin
    TRList.Items.Add(OutList[j].description);
    if Outlist[j].Frozen then TRList.Checked[j]:=true;
  end;

end;


procedure TStandalone.Button4Click(Sender: TObject);
var i:integer;
begin
  for i:=0 to numberofaddressesout do
  begin
    celist.ItemIndex:=0;
    button2.Click;
  end;
end;


procedure TStandalone.Button5Click(Sender: TObject);
var i: Integer;
begin
  for i:=0 to numberofaddressesin do
  begin
    celist.ItemIndex:=0;
    button3.Click;
  end;
end;

procedure TStandalone.Button6Click(Sender: TObject);
var i: Integer;
begin
  if trainerstyle1.checked then
  begin
    exampletrainerstyle1.Caption:=edit1.Text;
    exampletrainerstyle1.Label1.Caption:=memo1.Text;
    exampletrainerstyle1.Color:=panel1.color;
    exampletrainerstyle1.CheckListBox1.Color:=panel2.color;
    exampleTrainerstyle1.CheckListBox1.Font.Color:=panel3.color;
    exampletrainerstyle1.Label1.Font.Color:=panel3.color;
    if checkbox1.checked then
    begin
      exampletrainerstyle1.image1.Visible:=true;
      if not (fileexists(edit2.text)) or (uppercase(extractfileExt(edit2.Text))<>'.BMP') then raise exception.Create(edit2.text+' is not an valid image');
      exampletrainerstyle1.image1.Picture.LoadFromFile(edit2.text);
    end else exampletrainerstyle1.Visible:=false;

    exampletrainerstyle1.CheckListBox1.Clear;
    for i:=0 to TRList.Items.Count-1 do
      exampletrainerstyle1.CheckListBox1.Items.Add(TRList.items[i]);

    exampletrainerstyle1.showmodal;
  end else
  if trainerstyle2.checked then
  begin
    exampletrainerstyle2.Caption:=edit1.Text;
    exampletrainerstyle2.Label1.Caption:=memo1.Text;
    exampletrainerstyle2.Color:=panel1.color;
    exampletrainerstyle2.CheckListBox1.Color:=panel2.color;
    exampleTrainerstyle2.CheckListBox1.Font.Color:=panel3.color;
    exampletrainerstyle2.Label1.Font.Color:=panel3.color;
    if checkbox1.checked then
    begin
      exampletrainerstyle2.image1.Visible:=true;
      if not (fileexists(edit2.text)) or (uppercase(extractfileExt(edit2.Text))<>'.BMP') then raise exception.Create(edit2.text+' is not an valid image');
      exampletrainerstyle2.image1.Picture.LoadFromFile(edit2.text);
    end else exampletrainerstyle2.image1.Visible:=false;

    exampletrainerstyle2.CheckListBox1.Clear;
    for i:=0 to TRList.Items.Count-1 do
      exampletrainerstyle2.CheckListBox1.Items.Add(TRList.items[i]);

    exampletrainerstyle2.showmodal;
  end else
  if trainerstyle3.checked then
  begin
    exampletrainerstyle3.Caption:=edit1.Text;
    exampletrainerstyle3.Label1.Caption:=memo1.Text;
    exampletrainerstyle3.Color:=panel1.color;
    exampletrainerstyle3.CheckListBox1.Color:=panel2.color;
    exampleTrainerstyle3.CheckListBox1.Font.Color:=panel3.color;
    exampletrainerstyle3.Label1.Font.Color:=panel3.color;
    if checkbox1.checked then
    begin
      exampletrainerstyle3.image1.Visible:=true;
      if not (fileexists(edit2.text)) or (uppercase(extractfileExt(edit2.Text))<>'.BMP') then raise exception.Create(edit2.text+' is not an valid image');
      exampletrainerstyle3.image1.Picture.LoadFromFile(edit2.text);
    end else exampletrainerstyle3.Image1.Visible:=false;

    exampletrainerstyle3.CheckListBox1.Clear;
    for i:=0 to TRList.Items.Count-1 do
      exampletrainerstyle3.CheckListBox1.Items.Add(TRList.items[i]);

    exampletrainerstyle3.showmodal;
  end;


end;

procedure TStandalone.Button7Click(Sender: TObject);
var FIcon: TIcon;
    iconstream: TMemorystream;
begin
  OpenDialog1.Title:='Select a file with an ICON';
  Opendialog1.DefaultExt:='ICO';
  if opendialog1.Execute=true then
  begin
    FIcon := TIcon.Create;

    try
      FIcon.Handle := ExtractIcon(hInstance,Pchar(opendialog1.filename), 0);
    except
      FIcon.Free;
      Raise Exception.Create('The file you selected doesn''t have an icon that can be extracted');
      exit;
    end;

    Iconstream:=Tmemorystream.create;
    FIcon.SaveToStream(iconstream);

    if Iconstream.Size<>766 then raise Exception.Create('This is not an valid icon!');

    Iconstream.Seek(0,soFromBeginning);
    Image1.Picture.Icon.LoadFromStream(Iconstream);
    FIcon.Free;
    Iconstream.free;
  end;
end;

procedure TStandAlone.SpeedButton5Click(Sender: TObject);
var backcolor: TColor;
    err: Integer;
begin
  val('$'+edit4.Text,backcolor,err);
  colordialog1.Color:=backcolor;
  if Colordialog1.Execute then
    edit4.Text:=IntToHex(colordialog1.color,6);
end;

procedure TStandAlone.SpeedButton2Click(Sender: TObject);
var backcolor: TColor;
    err: Integer;
begin
  val('$'+edit3.Text,backcolor,err);
  colordialog1.Color:=backcolor;
  if Colordialog1.Execute then
    edit3.Text:=IntToHex(colordialog1.color,6);
end;

procedure TStandAlone.FormCreate(Sender: TObject);
begin
  Edit4.Text:=IntToHex(clBtnFace,6);
  Edit3.Text:=IntToHex(clWindow,6);
  edit5.text:=IntToHex(clWindowText,6);
end;

procedure TStandAlone.SpeedButton3Click(Sender: TObject);
var textcolor: TColor;
    err: Integer;
begin
  val('$'+edit5.Text,textcolor,err);
  colordialog1.Color:=textcolor;
  if Colordialog1.Execute then
    edit5.Text:=IntToHex(colordialog1.color,6);
end;

procedure TStandAlone.Edit4Change(Sender: TObject);
var err: Integer;
    backcolor: TColor;
begin
  val('$'+edit4.Text,backcolor,err);
  panel1.Color:=backcolor;
end;

procedure TStandAlone.Edit4KeyPress(Sender: TObject; var Key: Char);
begin
  case key of
    chr(65)..chr(70) : ;
    chr(97)..chr(102) : ;
    chr(8)        : ;
    chr(16)       : ;
    chr(48)..chr(57) : ;
    else key:=chr(0);
  end;
end;

procedure TStandAlone.Edit3Change(Sender: TObject);
var backcolor: TColor;
    err: Integer;
begin
  val('$'+edit3.Text,backcolor,err);
  panel2.Color:=backcolor;
end;

procedure TStandAlone.Edit5Change(Sender: TObject);
var textcolor: TColor;
    err: Integer;
begin
  val('$'+edit5.Text,textcolor,err);
  panel3.Color:=textcolor;
end;

procedure TStandAlone.TrainerStyle1Click(Sender: TObject);
begin
  imagesize.Caption:='50x50';
end;

procedure TStandAlone.TrainerStyle2Click(Sender: TObject);
begin
  imagesize.Caption:='90x50';
end;

procedure TStandAlone.TrainerStyle3Click(Sender: TObject);
begin
  checkbox1.checked:=true;
  if edit2.Text='' then edit2.Text:='style3def.bmp';
  imagesize.Caption:='280x280';
end;

procedure TStandalone.CheckBox1Click(Sender: TObject);
begin
  edit2.visible:=checkbox1.checked;
  speedbutton1.Visible:=checkbox1.checked;
  imagesize.Visible:=checkbox1.Checked;
end;

procedure TStandalone.SpeedButton1Click(Sender: TObject);
begin
  if openpicturedialog1.Execute then
  begin
    edit2.Text:=openpicturedialog1.FileName;
  end;
end;

end.
