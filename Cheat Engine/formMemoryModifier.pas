unit formMemoryModifier;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, ExtDlgs, ComCtrls, Buttons, ExtCtrls,shellapi,tlhelp32,
  cefuncproc,ExtraTrainerComponents;

const trainerversion=7;

type TcodeEntry = record
  address: dword;
  modulename: string;
  moduleoffset: dword;
  originalopcode: array of byte;
end;

type TAddressEntry = record
  address: dword;
  interpretableaddress: string;
  ispointer: boolean;
  pointers: array of TCEPointer;
  bit: byte;
  memtyp: integer;
  frozen: boolean;
  frozendirection: byte;
  setvalue: boolean;
  userinput: boolean;
  value: string;
  autoassemblescript: string;         
end;

type Ttrainerdata = record
  description: string;
  hotkeytext: string;
  hotkey: TKeyCombo;
  hasedit: boolean;
  editvalue: string;

  codeentrys: array of TCodeEntry;
  addressentrys: array of TAddressEntry;
end;

type
  TfrmMemoryModifier = class(TForm)
    Button1: TButton;
    Button2: TButton;
    lblWidthHeight: TLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    SaveDialog1: TSaveDialog;
    Panel1: TPanel;
    Label1: TLabel;
    spbUp: TSpeedButton;
    spbDown: TSpeedButton;
    recordview: TListView;
    PopupMenu1: TPopupMenu;
    Delete1: TMenuItem;
    OpenDialog1: TOpenDialog;
    Button3: TButton;
    Icon: TImage;
    Label4: TLabel;
    Button5: TButton;
    editTitle: TEdit;
    OpenDialog2: TOpenDialog;
    Edit2: TEdit;
    Label2: TLabel;
    ComboBox1: TComboBox;
    Label3: TLabel;
    LoadButton: TSpeedButton;
    Button4: TButton;
    Button6: TButton;
    OpenDialog3: TOpenDialog;
    CheckBox1: TCheckBox;
    EditHotkey: TEdit;
    Button7: TButton;
    CheckBox2: TCheckBox;
    Memo1: TMemo;
    editFreezeInterval: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    cbPreventReopening: TCheckBox;
    Button8: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure recordviewClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure editTitleChange(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure spbUpClick(Sender: TObject);
    procedure spbDownClick(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure EditHotkeyKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CheckBox1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EditHotkeyKeyPress(Sender: TObject; var Key: Char);
    procedure recordviewDblClick(Sender: TObject);
    procedure Button8Click(Sender: TObject);
  private
    { Private declarations }
    procedure changeicon(filename: string);
  public
    { Public declarations }
    trainerdata: array of TTrainerdata;
    changed: boolean;
    popuphotkey: tkeycombo;
    dontshowdefault:boolean;
  end;

var
  frmMemoryModifier: TfrmMemoryModifier;

implementation

uses formMemoryTrainerUnit, formMemoryTrainerAddEntry, MainUnit,
  MemoryTrainerDesignUnit;

{$R *.dfm}

procedure TfrmMemoryModifier.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
  frmMemoryModifier:=nil;
  frmMemoryTrainerPreview.Close;
  frmMemoryTrainerPreview.free;
  frmMemoryTrainerPreview:=nil;
end;

procedure TfrmMemoryModifier.Button2Click(Sender: TObject);
begin
  if openpicturedialog1.execute then
  begin
    if uppercase(extractfileext(openpicturedialog1.filename))<>'.BMP' then
      raise exception.create ('Sorry, but I cant use this filetype');

    frmMemorytrainerpreview.Image1.Picture.Bitmap.LoadFromFile(openpicturedialog1.filename);
    changed:=true;
  end;
end;

procedure TfrmMemoryModifier.Button4Click(Sender: TObject);
begin
  FrmMemoryTrainerAddEntry:=tFrmMemoryTrainerAddEntry.create(self);
  FrmMemoryTrainerAddEntry.showmodal;

  frmMemoryTrainerPreview.UpdateScreen;
  if frmTrainerDesigner<>nil then
    frmTrainerDesigner.UpdateCheats;
end;

procedure TfrmMemoryModifier.changeicon(filename: string);
var resh: thandle;
    s: tmemorystream;
begin
{    resh:=BeginUpdateResource(pchar(filename),false);
    if (resh<>0) then
    begin
      try
        // icon.p
        s:=tmemorystream.Create;
        try
          writeicon2(s, icon.Picture.Icon.Handle, false);

          if not updateResource(resh,pchar(RT_ICON),pchar(1),1033, @(s.Memory), s.size) then
            showmessage('Error changing the icon');
        finally
          s.Free;
        end;
      finally
        EndUpdateResource(resh,false);
      end;
    end;
   }
end;

procedure TfrmMemoryModifier.recordviewClick(Sender: TObject);
begin
  if recordview.ItemIndex<>-1 then
  begin
    spbUp.Enabled:=recordview.itemindex>0;
    spbDown.enabled:=recordview.itemindex<(recordview.Items.Count-1);
  end
  else
  begin
    spbUp.Enabled:=false;
    spbDown.enabled:=false;
  end;
end;

procedure TfrmMemoryModifier.Button1Click(Sender: TObject);
var res: TResourceStream;
    trainer,iconstream,imagestream: TMemorystream;
    searcher: ^char;
    scanstring: string;
    i,j,k,l: integer;
    iconbuf,x: pchar;
    temp: dword;
    tempi: integer;
    tempb: boolean;
    temps: string;

    protect: boolean;
begin
  protect:=cbPreventReopening.checked;
  if combobox1.Text='' then raise exception.Create('At least fill in a processname');

  if savedialog1.execute then
  begin
    //exctract exe
    res:=TResourceStream.Create(hinstance,'TRAINER','CEINCEXE');

    trainer:=TMemorystream.Create;
    res.SaveToStream(trainer);
    res.free;

    //search for the icon
    searcher:=trainer.memory;
    scanstring:='And so, Dark Byte, wrote the text into the icon, So he could find it back...';

    i:=0;
    j:=1;
    while (i<trainer.size) do
    begin
      if searcher^=scanstring[j] then
      begin
        if j=length(scanstring) then
        begin
//          dec(searcher,j);
          dec(i,j+199);
          break;
        end;
        inc(j);
      end else j:=1;

      inc(searcher);
      inc(i);
    end;

    if j<length(scanstring) then raise exception.create('Please, dont mess with the exe!');

    //replace it
    iconstream:=TMemorystream.Create;
    icon.Picture.Icon.SaveToStream(iconstream);

    getmem(iconbuf,744);

    iconstream.Position:=22;
    iconstream.ReadBuffer(iconbuf^,744);

    iconstream.Free;

    trainer.Position:=i;
    trainer.WriteBuffer(iconbuf^,744);

    freemem(iconbuf);
    //append the settingsdata behind that file

    temp:=trainer.Size;
    trainer.position:=80;
    trainer.Writebuffer(temp,4);

    //write the trainerdata to the end of the file
    trainer.position:=trainer.size;

    if protect then
      temp:=$22322
    else
      temp:=$22222;  //kind of trainer
      
    trainer.WriteBuffer(temp,4);

    temp:=trainerversion;
    trainer.WriteBuffer(temp,4);

    temp:=length(trainerdata);   //point to start of trainerdata

    trainer.WriteBuffer(temp,4);

    for i:=0 to length(trainerdata)-1 do
    begin
      temp:=length(trainerdata[i].description);
      trainer.WriteBuffer(temp,4);

      x:=pchar(trainerdata[i].description);
      trainer.WriteBuffer(pointer(x)^,temp);

      temp:=length(trainerdata[i].hotkeytext);
      trainer.WriteBuffer(temp,4);

      x:=pchar(trainerdata[i].hotkeytext);
      trainer.WriteBuffer(pointer(x)^,temp);

      trainer.WriteBuffer(trainerdata[i].hotkey,sizeof(trainerdata[i].hotkey));

      temp:=length(Trainerdata[i].codeEntrys);
      trainer.WriteBuffer(temp,4);
      for j:=0 to length(trainerdata[i].codeentrys)-1 do
      begin
        //address
        temp:=trainerdata[i].codeentrys[j].address;
        if protect then temp:=temp xor $11221122;

        trainer.WriteBuffer(temp,4);

        //modulename
        temp:=length(trainerdata[i].codeentrys[j].modulename);
        trainer.WriteBuffer(temp,4);

        x:=pchar(trainerdata[i].codeentrys[j].modulename);
        trainer.WriteBuffer(pointer(x)^,temp);

        temp:=trainerdata[i].codeentrys[j].moduleoffset;
        if protect then temp:=temp xor $22112211;
        trainer.WriteBuffer(temp,4);



        //original opcode
        temp:=length(trainerdata[i].codeentrys[j].originalopcode);
        trainer.WriteBuffer(temp,4);
        trainer.WriteBuffer(pointer(trainerdata[i].codeentrys[j].originalopcode)^,temp);
      end;

      temp:=length(Trainerdata[i].addressentrys);
      trainer.WriteBuffer(temp,4);
      for j:=0 to length(trainerdata[i].addressentrys)-1 do
      begin
        temp:=trainerdata[i].addressentrys[j].address;
        if protect then temp:=temp xor $dead1337;

        trainer.WriteBuffer(temp,sizeof(trainerdata[i].addressentrys[j].address));


        //interpretableaddress
        temp:=length(trainerdata[i].addressentrys[j].interpretableaddress);
        trainer.writebuffer(temp,sizeof(temps));

        temps:=trainerdata[i].addressentrys[j].interpretableaddress;
        if protect then //encrypt temps
          for k:=1 to length(temps) do
            temps[k]:=chr(ord(temps[k]) xor (k+1));
            
        trainer.writebuffer(temps[1],temp);




        trainer.WriteBuffer(trainerdata[i].addressentrys[j].ispointer,sizeof(trainerdata[i].addressentrys[j].ispointer));

        tempi:=length(trainerdata[i].addressentrys[j].pointers);
        trainer.WriteBuffer(tempi,4);
        for k:=0 to tempi-1 do
        begin
          trainer.WriteBuffer(trainerdata[i].addressentrys[j].pointers[k].address,sizeof(trainerdata[i].addressentrys[j].pointers[k].address));

          //interpretableaddress for pointer

          temp:=length(trainerdata[i].addressentrys[j].pointers[k].Interpretableaddress);
          trainer.writebuffer(temp,sizeof(temps));

          temps:=trainerdata[i].addressentrys[j].pointers[k].Interpretableaddress;
          if protect then //encrypt temps
            for l:=1 to length(temps) do
              temps[l]:=chr(ord(temps[l]) xor (l+2));

          trainer.writebuffer(temps[1],temp);

          trainer.WriteBuffer(trainerdata[i].addressentrys[j].pointers[k].offset,sizeof(trainerdata[i].addressentrys[j].pointers[k].offset));
        end;

        trainer.WriteBuffer(trainerdata[i].addressentrys[j].bit,sizeof(trainerdata[i].addressentrys[j].bit));
        trainer.WriteBuffer(trainerdata[i].addressentrys[j].memtyp,sizeof(trainerdata[i].addressentrys[j].memtyp));
        trainer.writebuffer(trainerdata[i].addressentrys[j].frozen,sizeof(trainerdata[i].addressentrys[j].frozen));
        trainer.writebuffer(trainerdata[i].addressentrys[j].frozendirection,sizeof(trainerdata[i].addressentrys[j].frozendirection));
        trainer.writebuffer(trainerdata[i].addressentrys[j].setvalue,sizeof(trainerdata[i].addressentrys[j].setvalue));
        trainer.WriteBuffer(trainerdata[i].addressentrys[j].userinput,sizeof(trainerdata[i].addressentrys[j].userinput));
        temp:=length(trainerdata[i].addressentrys[j].value);
        trainer.writebuffer(temp,4);
        trainer.writebuffer(trainerdata[i].addressentrys[j].value[1],temp);
        

        temp:=length(trainerdata[i].addressentrys[j].autoassemblescript);
        trainer.writebuffer(temp,sizeof(temp));

        temps:=trainerdata[i].addressentrys[j].autoassemblescript;
        if protect then //encrypt temps
          for k:=1 to length(temps) do
            temps[k]:=chr(ord(temps[k]) xor k);

        trainer.writebuffer(temps[1],temp);
      end;
    end;

    //title
    temp:=length(editTitle.text);
    trainer.writebuffer(temp,4);

    temps:=edittitle.text;
    if protect then
    begin
      for i:=1 to temp do
        temps[i]:=chr(ord(temps[i]) xor 56);
    end;

    x:=pchar(temps);
    trainer.writebuffer(pointer(x)^,temp);

    //launch filename
    temp:=length(edit2.text);
    trainer.writebuffer(temp,4);
    x:=pchar(edit2.text);
    trainer.writebuffer(pointer(x)^,temp);

    //autolaunch
    tempb:=checkbox2.checked;
    trainer.writebuffer(tempb,sizeof(tempb));

    //popup on keypress
    tempb:=checkbox1.Checked;
    trainer.writebuffer(tempb,sizeof(tempb));

    //process to modify
    temp:=length(combobox1.text);
    trainer.writebuffer(temp,4);
    x:=pchar(combobox1.text);
    trainer.writebuffer(pointer(x)^,temp);

    //hotkeytext
    temp:=length(edithotkey.text);
    trainer.writebuffer(temp,4);
    x:=pchar(edithotkey.text);
    trainer.writebuffer(pointer(x)^,temp);

    //hotkey
    trainer.WriteBuffer(popuphotkey,sizeof(popuphotkey));

    //aboutbox
    temp:=length(memo1.text);
    trainer.writebuffer(temp,4);

    temps:=memo1.Text;
    if protect then
    begin
      for i:=1 to temp do
        temps[i]:=chr(ord(temps[i]) xor 166);
    end;

    x:=pchar(temps);
    trainer.writebuffer(pointer(x)^,temp);

    //freeze interval
    tempi:=strtoint(editfreezeinterval.text);
    trainer.WriteBuffer(tempi,sizeof(tempi));

    if frmTrainerDesigner=nil then
    begin
      //default uid
      temp:=$666;
      trainer.WriteBuffer(temp,4);


      //leftside image
      imagestream:=TMemorystream.Create;
      frmMemorytrainerpreview.image1.Picture.Bitmap.SaveToStream(imagestream);

      temp:=imagestream.Size;
      trainer.WriteBuffer(temp,4);

      getmem(iconbuf,temp);
      imagestream.Position:=0;
      imagestream.ReadBuffer(pointer(iconbuf)^,temp);
      trainer.WriteBuffer(pointer(iconbuf)^,temp);
      imagestream.Free;
      freemem(iconbuf);

      //windowwidth
      trainer.writebuffer(frmMemorytrainerpreview.Width,sizeof(frmMemorytrainerpreview.Width));

      //windowheight
      trainer.writebuffer(frmMemorytrainerpreview.height,sizeof(frmMemorytrainerpreview.height));

      //leftsidewidth
      trainer.writebuffer(frmMemorytrainerpreview.Panel1.Width,sizeof(frmMemorytrainerpreview.Panel1.Width));

      //leftsideheight
      trainer.writebuffer(frmMemorytrainerpreview.Panel1.height,sizeof(frmMemorytrainerpreview.Panel1.height));
    end
    else
    begin
      //user defined UID
      temp:=$777;
      trainer.WriteBuffer(temp,4);

      //windowwidth
      trainer.writebuffer(frmTrainerDesigner.Width,sizeof(frmTrainerDesigner.Width));

      //windowheight
      trainer.writebuffer(frmTrainerDesigner.height,sizeof(frmTrainerDesigner.height));


      //note:make this oop

      for i:=0 to frmtrainerdesigner.ComponentCount-1 do
      begin
        if (frmtrainerdesigner.Components[i] is tbutton2) then
        begin
          temp:=0;      //tbutton
          trainer.WriteBuffer(temp,4);
          trainer.WriteBuffer(tbutton2(frmtrainerdesigner.Components[i]).left,sizeof(integer));
          trainer.WriteBuffer(tbutton2(frmtrainerdesigner.Components[i]).top,sizeof(integer));
          trainer.WriteBuffer(tbutton2(frmtrainerdesigner.Components[i]).width,sizeof(integer));
          trainer.WriteBuffer(tbutton2(frmtrainerdesigner.Components[i]).height,sizeof(integer));

          //caption
          temp:=length(tbutton2(frmtrainerdesigner.Components[i]).Caption);
          trainer.writebuffer(temp,4);
          x:=pchar(tbutton2(frmtrainerdesigner.Components[i]).Caption);
          trainer.writebuffer(pointer(x)^,temp);

          trainer.WriteBuffer(tbutton2(frmtrainerdesigner.Components[i]).wordwrap,sizeof(boolean));

          //onclick
          temp:=tbutton2(frmtrainerdesigner.Components[i]).Tag;
          trainer.WriteBuffer(temp,4);

          //command
          temp:=length(tbutton2(frmtrainerdesigner.Components[i]).Command);
          trainer.writebuffer(temp,4);
          x:=pchar(tbutton2(frmtrainerdesigner.Components[i]).Command);
          trainer.writebuffer(pointer(x)^,temp);
        end;

        if (frmtrainerdesigner.Components[i] is tcheatlist) then
        begin
          temp:=1;      //tcheatlist
          trainer.WriteBuffer(temp,4);
          trainer.WriteBuffer(tcheatlist(frmtrainerdesigner.Components[i]).left,sizeof(integer));
          trainer.WriteBuffer(tcheatlist(frmtrainerdesigner.Components[i]).top,sizeof(integer));
          trainer.WriteBuffer(tcheatlist(frmtrainerdesigner.Components[i]).width,sizeof(integer));
          trainer.WriteBuffer(tcheatlist(frmtrainerdesigner.Components[i]).height,sizeof(integer));

          trainer.Writebuffer(tcheatlist(frmtrainerdesigner.Components[i]).activationcolor,sizeof(tcolor));
          trainer.Writebuffer(tcheatlist(frmtrainerdesigner.Components[i]).color,sizeof(tcolor));
          trainer.Writebuffer(tcheatlist(frmtrainerdesigner.Components[i]).textcolor,sizeof(tcolor));
          trainer.Writebuffer(tcheatlist(frmtrainerdesigner.Components[i]).hotkeyleft,sizeof(integer));
          trainer.Writebuffer(tcheatlist(frmtrainerdesigner.Components[i]).descriptionleft,sizeof(integer));
          trainer.Writebuffer(tcheatlist(frmtrainerdesigner.Components[i]).editleft,sizeof(integer));
          trainer.Writebuffer(tcheatlist(frmtrainerdesigner.Components[i]).editwidth,sizeof(integer));

          trainer.Writebuffer(tcheatlist(frmtrainerdesigner.Components[i]).BevelInner,sizeof(Tbevelcut));
          trainer.Writebuffer(tcheatlist(frmtrainerdesigner.Components[i]).BevelOuter,sizeof(Tbevelcut));
          trainer.Writebuffer(tcheatlist(frmtrainerdesigner.Components[i]).BevelWidth,sizeof(integer));
          trainer.Writebuffer(tcheatlist(frmtrainerdesigner.Components[i]).bevelkind,sizeof(tbevelkind));

          trainer.Writebuffer(tcheatlist(frmtrainerdesigner.Components[i]).HasCheckbox,sizeof(boolean));
          trainer.Writebuffer(tcheatlist(frmtrainerdesigner.Components[i]).ShowHotkeys,sizeof(boolean));          
        end;

        if (frmtrainerdesigner.Components[i] is tcheat) then
        begin
          temp:=2;      //tcheat
          trainer.WriteBuffer(temp,4);
          trainer.WriteBuffer(tcheat(frmtrainerdesigner.Components[i]).left,sizeof(integer));
          trainer.WriteBuffer(tcheat(frmtrainerdesigner.Components[i]).top,sizeof(integer));
          trainer.WriteBuffer(tcheat(frmtrainerdesigner.Components[i]).width,sizeof(integer));
          trainer.WriteBuffer(tcheat(frmtrainerdesigner.Components[i]).height,sizeof(integer));

          trainer.WriteBuffer(tcheat(frmtrainerdesigner.Components[i]).cheatnr,sizeof(integer));
          trainer.Writebuffer(tcheat(frmtrainerdesigner.Components[i]).activationcolor,sizeof(tcolor));
          trainer.Writebuffer(tcheat(frmtrainerdesigner.Components[i]).color,sizeof(tcolor));
          trainer.Writebuffer(tcheat(frmtrainerdesigner.Components[i]).textcolor,sizeof(tcolor));

          temp:=tcheat(frmtrainerdesigner.Components[i]).hotkeyleft;
          trainer.Writebuffer(temp,4);
          temp:=tcheat(frmtrainerdesigner.Components[i]).descriptionleft;
          trainer.Writebuffer(temp,4);
          temp:=tcheat(frmtrainerdesigner.Components[i]).editleft;
          trainer.Writebuffer(temp,4);
          temp:=tcheat(frmtrainerdesigner.Components[i]).editwidth;
          trainer.Writebuffer(temp,4);

          trainer.Writebuffer(tcheat(frmtrainerdesigner.Components[i]).HasCheckbox,sizeof(boolean));
          trainer.Writebuffer(tcheat(frmtrainerdesigner.Components[i]).ShowHotkey,sizeof(boolean));
        end;

        if (frmtrainerdesigner.Components[i] is timage2) then
        begin
          temp:=3;      //image
          trainer.WriteBuffer(temp,4);
          trainer.WriteBuffer(timage2(frmtrainerdesigner.Components[i]).left,sizeof(integer));
          trainer.WriteBuffer(timage2(frmtrainerdesigner.Components[i]).top,sizeof(integer));
          trainer.WriteBuffer(timage2(frmtrainerdesigner.Components[i]).width,sizeof(integer));
          trainer.WriteBuffer(timage2(frmtrainerdesigner.Components[i]).height,sizeof(integer));

          trainer.WriteBuffer(timage2(frmtrainerdesigner.Components[i]).cursor,sizeof(tcursor));
          trainer.WriteBuffer(timage2(frmtrainerdesigner.Components[i]).stretch,sizeof(boolean));
          trainer.WriteBuffer(timage2(frmtrainerdesigner.Components[i]).transparent,sizeof(boolean));
          trainer.WriteBuffer(timage2(frmtrainerdesigner.Components[i]).tag,sizeof(integer));

          //image
          imagestream:=TMemorystream.Create;
          timage2(frmtrainerdesigner.Components[i]).Picture.Bitmap.SaveToStream(imagestream);

          temp:=imagestream.Size;
          trainer.WriteBuffer(temp,4);

          getmem(iconbuf,temp);
          imagestream.Position:=0;
          imagestream.ReadBuffer(pointer(iconbuf)^,temp);
          trainer.WriteBuffer(pointer(iconbuf)^,temp);
          imagestream.Free;
          freemem(iconbuf);

          //command
          temp:=length(timage2(frmtrainerdesigner.Components[i]).Command);
          trainer.writebuffer(temp,4);
          x:=pchar(timage2(frmtrainerdesigner.Components[i]).Command);
          trainer.writebuffer(pointer(x)^,temp);
        end;

        if (frmtrainerdesigner.Components[i] is tlabel2) then
        begin
          temp:=4;      //tlabel
          trainer.WriteBuffer(temp,4);
          trainer.WriteBuffer(tlabel2(frmtrainerdesigner.Components[i]).left,sizeof(integer));
          trainer.WriteBuffer(tlabel2(frmtrainerdesigner.Components[i]).top,sizeof(integer));
          trainer.WriteBuffer(tlabel2(frmtrainerdesigner.Components[i]).width,sizeof(integer));
          trainer.WriteBuffer(tlabel2(frmtrainerdesigner.Components[i]).height,sizeof(integer));

          //caption
          temp:=length(tlabel2(frmtrainerdesigner.Components[i]).Caption);
          trainer.writebuffer(temp,4);
          x:=pchar(tlabel2(frmtrainerdesigner.Components[i]).Caption);
          trainer.writebuffer(pointer(x)^,temp);

          //wordwrap
          trainer.WriteBuffer(tlabel2(frmtrainerdesigner.Components[i]).wordwrap,sizeof(boolean));


          //color
          trainer.Writebuffer(tlabel2(frmtrainerdesigner.Components[i]).Font.color,sizeof(tcolor));

          //command
          temp:=length(tlabel2(frmtrainerdesigner.Components[i]).Command);
          trainer.writebuffer(temp,4);
          x:=pchar(tlabel2(frmtrainerdesigner.Components[i]).Command);
          trainer.writebuffer(pointer(x)^,temp);

          //cursor
          trainer.WriteBuffer(tlabel2(frmtrainerdesigner.Components[i]).cursor,sizeof(tcursor));

          //tag
          trainer.WriteBuffer(tlabel2(frmtrainerdesigner.Components[i]).tag,sizeof(integer));

          //fontstyle
          tempb:=tlabel2(frmtrainerdesigner.Components[i]).Font.Style=[fsUnderline];
          trainer.WriteBuffer(tempb,sizeof(tempb));
        end;

      end;

      temp:=$ffffffff;  //indicate no other objects (although the eof would be a good indication too...)
      trainer.WriteBuffer(temp,4);
    end;

    //save to file
    trainer.SaveToFile(savedialog1.filename);
    trainer.free;

    {  //commented out because the trainer can't handle this
    if cefuncproc.GetSystemType >=4 then //nt+
      changeicon(savedialog1.filename);
      }

    showmessage('Trainer generated');
  end;
end;

procedure TfrmMemoryModifier.Button5Click(Sender: TObject);
var HI: HICON;
    test: TMemorystream;
    resp: pointer;

    ic: ticon;
begin

  if opendialog2.execute then
  begin
    HI:=ExtractIcon(hinstance,pchar(opendialog2.filename),0);
    if hi=1 then
      raise exception.Create('This file type is not supported');

    if hi=0 then
      raise exception.Create('No icon found in this file');

    ic:=ticon.Create;
    ic.Handle:=hi;

    icon.Picture.Icon.Handle:=HI;
    test:=TMemoryStream.Create;
    icon.Picture.Icon.SaveToStream(test);
    


    try
      if test.Size<>766 then
        raise exception.Create('The size of this icon is '+IntToStr(test.size)+'. It should be 766');
    finally
      test.free;
    end;

    frmMemoryTrainerPreview.Icon:=icon.Picture.Icon;
    if frmTrainerDesigner<>nil then frmtrainerdesigner.Icon:=icon.Picture.Icon;
  end;
end;

procedure TfrmMemoryModifier.editTitleChange(Sender: TObject);
begin
  frmMemoryTrainerPreview.Caption:=edittitle.Text;
  if frmTrainerDesigner<>nil then frmTrainerdesigner.Caption:=edittitle.Text;
  changed:=true;
end;

procedure TfrmMemoryModifier.Button6Click(Sender: TObject);
var index: integer;
    i,j: integer;
begin
  index:=recordview.ItemIndex;
  if index<>-1 then
  begin
    for i:=0 to length(trainerdata[index].codeentrys)-1 do
      setlength(trainerdata[index].codeentrys[i].originalopcode,0);

    setlength(trainerdata[index].codeentrys,0);
    setlength(trainerdata[index].addressentrys,0);

    for i:=index to length(trainerdata)-2 do
      trainerdata[i]:=trainerdata[i+1];

    setlength(trainerdata,length(trainerdata)-1);
    recordview.DeleteSelected;
  end;

  frmMemoryTrainerPreview.UpdateScreen;
  if frmtrainerdesigner<>nil then frmtrainerdesigner.updatecheats
end;

procedure TfrmMemoryModifier.spbUpClick(Sender: TObject);
var temp:TTrainerdata;
    indeX: integer;
    desc,hotkey: string;
begin
  index:=recordview.ItemIndex;
  if index>=1 then
  begin
    temp:=trainerdata[index];
    trainerdata[index]:=trainerdata[index-1];
    trainerdata[index-1]:=temp;

    desc:=recordview.Items[index].Caption;
    hotkey:=recordview.Items[index].SubItems[0];
    recordview.Items[index]:=recordview.items[index-1];

    recordview.Items[index-1].Caption:=desc;
    recordview.Items[index-1].SubItems[0]:=hotkey;

    recordview.Items[index-1].Selected:=true;
  end;

  spbUp.Enabled:=recordview.itemindex>0;
  spbDown.enabled:=recordview.itemindex<(recordview.Items.Count-1);

  frmMemoryTrainerPreview.UpdateScreen;
  if frmtrainerdesigner<>nil then frmtrainerdesigner.updatecheats
end;

procedure TfrmMemoryModifier.spbDownClick(Sender: TObject);
var temp:TTrainerdata;
    indeX: integer;
    desc,hotkey: string;
begin
  index:=recordview.ItemIndex;
  if index<recordview.Items.Count-1 then
  begin
    temp:=trainerdata[index];
    trainerdata[index]:=trainerdata[index+1];
    trainerdata[index+1]:=temp;

    desc:=recordview.Items[index].Caption;
    hotkey:=recordview.Items[index].SubItems[0];
    recordview.Items[index]:=recordview.items[index+1];

    recordview.Items[index+1].Caption:=desc;
    recordview.Items[index+1].SubItems[0]:=hotkey;

    recordview.Items[index+1].Selected:=true;
  end;

  spbUp.Enabled:=recordview.itemindex>0;
  spbDown.enabled:=recordview.itemindex<(recordview.Items.Count-1);

  frmMemoryTrainerPreview.UpdateScreen;
  if frmtrainerdesigner<>nil then frmtrainerdesigner.updatecheats
end;

procedure TfrmMemoryModifier.LoadButtonClick(Sender: TObject);
begin
  if opendialog3.Execute then
    edit2.text:=extractfilename(opendialog3.filename);
end;

procedure TfrmMemoryModifier.FormCreate(Sender: TObject);
Var SNAPHandle: THandle;
    ProcessEntry: ProcessEntry32;
    Check: Boolean;
    FullProcessName,ProcessName: String;
    I: Integer;
begin
  changed:=false;

  left:=mainform.left-180;
  if left<0 then left:=0;

  top:=mainform.top+( (mainform.clientheight div 2)-(height div 2));

  combobox1.clear;
  SNAPHandle:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
  If SnapHandle>0 then
  begin
    ProcessEntry.dwSize:=SizeOf(ProcessEntry);
    Check:=Process32First(SnapHandle,ProcessEntry);
    while check=true do
    begin
      ProcessName:='';
      FullProcessName:='';
      FullProcessName:=processentry.szExeFile;
      i:=Length(FullProcessName);
      while (i>0) and (FullProcessname[i-1]<>'\') do dec(i);
      processname:=copy(FullProcessName,i,length(FullProcessname)-i+1);
      combobox1.Items.Add(processname);
      check:=Process32Next(SnapHandle,ProcessEntry);
    end;
  end else raise exception.Create('I can''t get the process list.');

  frmMemoryTrainerPreview:=tfrmMemoryTrainerPreview.create(self);
  frmMemoryTrainerPreview.left:=left+width;
  frmMemoryTrainerPreview.Top:=top;
  frmMemoryTrainerPreview.Height:=height;
  

end;

procedure TfrmMemoryModifier.Edit2Change(Sender: TObject);
begin
  changed:=true;
end;

procedure TfrmMemoryModifier.ComboBox1Change(Sender: TObject);
begin
  changed:=true;
end;

procedure TfrmMemoryModifier.Button3Click(Sender: TObject);
begin
  if changed or (recordview.items.count>0) then
  begin
    if messagedlg('Are you sure?',mtConfirmation,[mbyes,mbno],0)=mrYes then modalresult:=mrcancel;
  end else modalresult:=mrcancel;

  if modalresult=mrcancel then close;
end;

procedure TfrmMemoryModifier.Button7Click(Sender: TObject);
begin
  if frmTrainerDesigner<>nil then
  begin
    frmTrainerDesigner.Close;
    exit;
  end;

  button2.Enabled:=false;

  frmMemoryTrainerPreview.Visible:=false;
  frmTrainerDesigner:=Tfrmtrainerdesigner.create(self);
  frmTrainerdesigner.Caption:=edittitle.Text;
  frmtrainerdesigner.Icon:=frmMemoryTrainerPreview.Icon;
  frmTrainerDesigner.BoundsRect:=frmMemoryTrainerPreview.BoundsRect;
  frmTrainerDesigner.show;

  button7.Caption:='Use default trainer layout';
end;

procedure TfrmMemoryModifier.EditHotkeyKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if popuphotkey[4]=0 then
  begin
    for i:=0 to 4 do
      if popuphotkey[i]=0 then
      begin
        popuphotkey[i]:=key;
        break;
      end else
      if popuphotkey[i]=key then break;  //already in list
  end;

  editHotkey.Text:=ConvertKeyComboToString(popuphotkey);
end;

procedure TfrmMemoryModifier.CheckBox1Click(Sender: TObject);
begin
  edithotkey.Enabled:=checkbox1.Checked;
end;

procedure TfrmMemoryModifier.FormShow(Sender: TObject);
begin
  if not dontshowdefault then frmMemoryTrainerPreview.Show;
end;

procedure TfrmMemoryModifier.EditHotkeyKeyPress(Sender: TObject;
  var Key: Char);
begin
  key:=#0;
end;

procedure TfrmMemoryModifier.recordviewDblClick(Sender: TObject);
var i,j: integer;
begin
{  i:=recordview.ItemIndex;
  if i<>-1 then
  begin
    FrmMemoryTrainerAddEntry:=tFrmMemoryTrainerAddEntry.create(self);
    FrmMemoryTrainerAddEntry.editmode:=true;

    with FrmMemoryTrainerAddEntry do
    begin
      editDescription.Text:=trainerdata[i].description;
      edithotkey.Text:=trainerdata[i].hotkeytext;
      laststate:=trainerdata[i].hotkey;
      lastshiftstate:=[];

      if (trainerdata[i].hotshift and MOD_CONTROL)=MOD_CONTROL then lastshiftstate:=lastshiftstate+[ssctrl];
      if (trainerdata[i].hotshift and MOD_ALT)=MOD_ALT then lastshiftstate:=lastshiftstate+[ssalt];
      if (trainerdata[i].hotshift and MOD_Shift)=MOD_Shift then lastshiftstate:=lastshiftstate+[ssshift];

      for j:=0 to length(trainerdata[i].codeentrys)-1 do
      begin
        trainerdata[i].codeentrys[j].address
        trainerdata[i].codeentrys[j].originalopcode


      end;
    end;

    FrmMemoryTrainerAddEntry.showmodal;

    frmMemoryTrainerPreview.UpdateScreen;
    if frmTrainerDesigner<>nil then
      frmTrainerDesigner.UpdateCheats;
  end;}
end;

procedure TfrmMemoryModifier.Button8Click(Sender: TObject);
begin
  zeromemory(@popuphotkey[0],10);
  editHotkey.Text:=ConvertKeyComboToString(popuphotkey);
  edithotkey.SetFocus;
end;

end.


