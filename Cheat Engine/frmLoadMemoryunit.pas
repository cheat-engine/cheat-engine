unit frmLoadMemoryunit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CEFuncProc, StdCtrls, NewKernelHandler, LResources, ExtCtrls;

type tbarray=array of byte;

type

  { TfrmLoadMemory }

  TfrmLoadMemory = class(TForm)
    Button1: TButton;
    Button2: TButton;
    editAddress: TEdit;
    Label1: TLabel;
    ListBox1: TListBox;
    Button3: TButton;
    Panel1: TPanel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    size:integer;
    datafile: tfilestream;
    buf: array of tbarray;
  public
    { Public declarations }
    procedure Showmodal(filename:string); overload;
  end;

var
  frmLoadMemory: TfrmLoadMemory;

implementation

uses ProcessHandlerUnit, Parsers;

resourcestring
  strinvalidfile='This is a invalid memory region file. I''ll assume this file has no header data';
  rsLoadIntoMemory = 'Load %s into memory';
  rsWroteBytesOutOf = 'Wrote %d bytes out of %d';
  rsNotAllTheMemoryCanBeWrittenInTheCurrentMemoryRegion = 'Not all the memory '
    +'can be written in the current memory region. Do you want to force as '
    +'much as possible into the given region and just discard what can not be '
    +'written? (Really bad idea)';


type Tregion=class
  public
    fromaddress,toaddress:ptrUint;
    constructor create(fa,ta:ptrUint);
end;

constructor TRegion.create(fa,ta:ptrUint);
begin
  fromaddress:=fa;
  toaddress:=ta;
  inherited create;
end;


procedure tfrmLoadMemory.Showmodal(filename:string);
var check: pchar;
    temp: qword;
    i:integer;

begin

  caption:=Format(rsLoadIntoMemory, [extractfilename(filename)]);

  try
    datafile:=tfilestream.Create(filename,fmopenread);
    getmem(check,12);
    datafile.ReadBuffer(check^,11);
    check[11]:=#0;

    if check='CHEATENGINE' then
    begin
      //this is a file with a header. (if not the user has been lame enough to copy memory starting with CHEATENGINE)
      temp:=0;
      datafile.ReadBuffer(temp,4);
      case temp of
        1:
        begin
          datafile.ReadBuffer(temp,4);

          size:=datafile.Size-datafile.Position;
          listbox1.Items.AddObject(inttohex(temp,8)+'-'+inttohex(temp+size-1,8),tregion.create(temp,temp+size));

          setlength(buf,1);
          setlength(buf[0],size);
          datafile.ReadBuffer(buf[0][0],size);
        end;

        2:
        begin
          i:=0;
          while datafile.Position<datafile.Size do
          begin
            temp:=0;
            datafile.ReadBuffer(temp,4);
            datafile.readbuffer(size,4);
            setlength(buf,length(buf)+1);
            setlength(buf[length(buf)-1],size);
            datafile.ReadBuffer(buf[length(buf)-1][0],size);
            listbox1.items.addobject(inttohex(temp,8)+'-'+inttohex(temp+size-1,8),tregion.create(temp,temp+size));
          end;
        end;

        3:
        begin
          i:=0;
          while datafile.Position<datafile.Size do
          begin
            datafile.ReadBuffer(temp,8);
            datafile.readbuffer(size,4);
            setlength(buf,length(buf)+1);
            setlength(buf[length(buf)-1],size);
            datafile.ReadBuffer(buf[length(buf)-1][0],size);
            listbox1.items.addobject(inttohex(temp,8)+'-'+inttohex(temp+size-1,8),tregion.create(temp,temp+size));
          end;
        end
        else
        begin
          datafile.Position:=0; //NO HEADER, this is a professional user, or someone who doesn't know what he's doing
          size:=datafile.size;
          listbox1.items.addobject('00000000-'+inttohex(size-1,8),tregion.create(0,size));

          setlength(buf,1);
          setlength(buf[0],size);
          datafile.readbuffer(buf[0][0],size);
          raise exception.Create(strinvalidfile);
        end;

      end;

    end
    else
    begin
      datafile.Position:=0; //NO HEADER, this is a professional user, or someone who doesn't know what he's doing
      size:=datafile.size;
      listbox1.items.addobject('00000000-'+inttohex(size-1,8),tregion.create(0,size));

      setlength(buf,1);
      setlength(buf[0],size);
      datafile.readbuffer(buf[0][0],size);
    end;

  finally
    if datafile<>nil then
      datafile.Free;

    freememandnil(check);
  end;

  listbox1.itemindex:=0;
  listbox1.onclick(listbox1);

  inherited showmodal;
end;

procedure TfrmLoadMemory.FormClose(Sender: TObject;
  var Action: TCloseAction);
var i: integer;
begin
  for i:=0 to listbox1.items.count-1 do
    tregion(listbox1.Items.Objects[i]).Free;

  for i:=0 to length(buf)-1 do
    setlength(buf[i],0);
  setlength(buf,0);



  action:=cafree;
end;

procedure TfrmLoadMemory.Button1Click(Sender: TObject);
var address,check: dword;
    i: integer;
    s: dword;
    x: tbarray;
begin
  for i:=0 to length(buf)-1 do
  begin
    x:=buf[i];
    s:=length(buf[i]);
    if not RewriteCode(processhandle,tregion(listbox1.items.objects[i]).fromaddress,@x[0],s) then
    begin
      if MessageDlg(rsNotAllTheMemoryCanBeWrittenInTheCurrentMemoryRegion, mtConfirmation, [mbno, mbyes], 0)=mryes then
      begin
        s:=length(buf[i]);
        RewriteCode(processhandle,tregion(listbox1.items.objects[i]).fromaddress,@x[0],s, true);
        showmessage(Format(rsWroteBytesOutOf, [s, length(buf[i])]));
      end;
    end;
  end;


  modalresult:=mrok;
end;

procedure TfrmLoadMemory.ListBox1Click(Sender: TObject);
begin
  if listbox1.ItemIndex<>-1 then
  begin
    editaddress.text:=inttohex(tregion(listbox1.items.objects[listbox1.ItemIndex]).fromaddress,8);
  end;
end;

procedure TfrmLoadMemory.Button3Click(Sender: TObject);
var delta: ptruint;
begin
  if listbox1.ItemIndex<>-1 then
  begin
    delta:=StrToQWordEx('$'+editaddress.text)-tregion(listbox1.items.objects[listbox1.ItemIndex]).fromaddress;
    inc(tregion(listbox1.items.objects[listbox1.ItemIndex]).fromaddress,delta);
    inc(tregion(listbox1.items.objects[listbox1.ItemIndex]).toaddress,delta);
    listbox1.Items[listbox1.ItemIndex]:=inttohex(tregion(listbox1.items.objects[listbox1.ItemIndex]).fromaddress,8)+'-'+inttohex(tregion(listbox1.items.objects[listbox1.ItemIndex]).toaddress,8);
  end;
end;

initialization
  {$i frmLoadMemoryunit.lrs}

end.
