unit formPatcherMaker3;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, ExtCtrls,SHELLAPI;

type
  TfrmPatcherMaker3 = class(TForm)
    PatchList: TListBox;
    Button1: TButton;
    Button3: TButton;
    PopupMenu1: TPopupMenu;
    Edit1: TMenuItem;
    Remove1: TMenuItem;
    Button2: TButton;
    Icon: TImage;
    Button4: TButton;
    OpenDialog1: TOpenDialog;
    memoMessage: TMemo;
    Label1: TLabel;
    SaveDialog1: TSaveDialog;
    editTitle: TEdit;
    Label2: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure Remove1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure PatchListDblClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    madeatrainer: boolean;
  public
    { Public declarations }
    procedure FillList;
  end;

var
  frmPatcherMaker3: TfrmPatcherMaker3;

implementation

uses formPatcherMaker, formPatchEdit;

{$R *.dfm}

procedure TFrmPatcherMaker3.FillList;
var i: integer;
begin
  patchlist.clear;
  with frmPatcherMaker do
  begin

    for i:=0 to length(patches)-1 do
      patchlist.Items.add(patches[i].filename+'-'+IntToHex(patches[i].address,8)+'-'+IntToStr(length(patches[i].mem))+' bytes');
  end;
end;

procedure TfrmPatcherMaker3.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
end;

procedure TfrmPatcherMaker3.FormCreate(Sender: TObject);
begin
  madeatrainer:=false;
  filllist;
end;

procedure TfrmPatcherMaker3.Button1Click(Sender: TObject);
begin
  //add
  frmPatchEdit:=TFrmPatchEdit.create(self);
  frmPatchEdit.edit:=false;
  frmPatchEdit.showmodal;

end;

procedure TfrmPatcherMaker3.Edit1Click(Sender: TObject);
begin
  frmPatchEdit:=TFrmPatchEdit.create(self);
  frmPatchEdit.edit:=true;
  frmPatchEdit.showmodal;
end;

procedure TfrmPatcherMaker3.Remove1Click(Sender: TObject);
var i,index: integer;
begin
  with frmPatchermaker do
  begin
    index:=patchlist.itemindex;
    setlength(patches[index].mem,0);
    
    for i:=index to length(patches)-2 do
      patches[i]:=patches[i+1];

    setlength(patches,length(patches)-1);
  end;

  filllist;
end;

procedure TfrmPatcherMaker3.PopupMenu1Popup(Sender: TObject);
begin
  if patchlist.ItemIndex>-1 then
  begin
    edit1.visible:=true;
    remove1.visible:=true;
  end
  else
  begin
    edit1.visible:=false;
    remove1.visible:=false;
  end;
end;

procedure TfrmPatcherMaker3.PatchListDblClick(Sender: TObject);
begin
  if patchlist.itemindex>-1 then edit1.click;
end;

procedure TfrmPatcherMaker3.Button3Click(Sender: TObject);
var res: TResourceStream;
    trainer: TMemorystream;
    searcher: ^char;
    scanstring: string;
    i,j: integer;
    iconstream: Tmemorystream;
    iconbuf,x: pchar;
    temp: dword;
begin
  if savedialog1.execute then
  begin
    //extract the exe file resource

    res:=TResourceStream.Create(hinstance,'TRAINER','CEINCEXE');

    trainer:=TMemorystream.Create;
    res.SaveToStream(trainer);
    res.free;

    //search for the icon
    searcher:=trainer.Memory;
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

    if j<length(scanstring) then raise exception.create('Error with the trainermaker!');

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

    //write the patchdata to the end of the file
    trainer.position:=trainer.size;

    with frmPatcherMaker do
    begin
      temp:=$111111; //$111111 = patcher  ($222222 = memory changes)
      trainer.WriteBuffer(temp,4);

      temp:=length(patches);
      trainer.WriteBuffer(temp,4);

      for i:=0 to length(patches)-1 do
      begin
        temp:=patches[i].address;
        trainer.WriteBuffer(temp,4);

        temp:=length(patches[i].mem);
        trainer.WriteBuffer(temp,4);
        trainer.WriteBuffer(pointer(patches[i].mem)^,temp);

        temp:=length(patches[i].filename);
        trainer.WriteBuffer(temp,4);
        x:=pchar(patches[i].filename);
        trainer.writeBuffer(pointer(x)^,temp);
      end;
    end;

    temp:=length(memomessage.text);
    trainer.WriteBuffer(temp,4);
    x:=pchar(memomessage.Text);
    trainer.writeBuffer(pointer(x)^,temp);

    temp:=length(editTitle.text);
    trainer.writebuffer(temp,4);
    x:=pchar(edittitle.text);
    trainer.writebuffer(pointer(x)^,temp);

    //save to file
    trainer.SaveToFile(savedialog1.filename);
    trainer.free;

    button2.caption:='OK';

    madeatrainer:=true;
    showmessage('File patcher generated');
  end;

end;

procedure TfrmPatcherMaker3.Button2Click(Sender: TObject);
begin
  if madeatrainer then modalresult:=mrCancel
  else
  if messagedlg('You havn''t made a trainer, are you sure ?',mtConfirmation,[mbyes,mbno],0)=mryes then modalresult:=mrcancel;
end;

procedure TfrmPatcherMaker3.Button4Click(Sender: TObject);
var HI: HICON;
    test: TMemorystream;
begin
  if opendialog1.execute then
  begin
    HI:=ExtractIcon(hinstance,pchar(opendialog1.filename),0);
    if hi=1 then
      raise exception.Create('This file type is not supported');

    if hi=0 then
      raise exception.Create('No icon found in this file');


    icon.Picture.Icon.Handle:=HI;
    test:=TMemoryStream.Create;
    icon.Picture.Icon.SaveToStream(test);

    try
      if test.Size<>766 then
        raise exception.Create('The size of this icon is '+IntToStr(test.size)+'. It should be 766');
    finally
      test.free;
    end;
  end;
end;

end.
