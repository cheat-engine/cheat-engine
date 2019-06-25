unit frmSaveMemoryRegionUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms, symbolhandler,
  Dialogs, StdCtrls, NewKernelHandler, CEFuncProc, ExtCtrls, LResources, Menus;

type

  { TfrmSaveMemoryRegion }

  TfrmSaveMemoryRegion = class(TForm)
    smrImageList: TImageList;
    miClearList: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    lbRegions: TListBox;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    DontInclude: TCheckBox;
    Panel3: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel4: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    editFrom: TEdit;
    editTo: TEdit;
    Button3: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure DontIncludeClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbRegionsDblClick(Sender: TObject);
    procedure miClearListClick(Sender: TObject);
  private
    { Private declarations }
    alreadywarned: boolean;
  public
    { Public declarations }
  end;

var
  frmSaveMemoryRegion: TfrmSaveMemoryRegion;

implementation

uses ProcessHandlerUnit, Parsers;

resourcestring
  rsPleaseAddAtLeastOneAddressRegionToTheList = 'Please add at least one address region to the list';
  rsNotAllTheMemoryWasReadableIn = 'Not all the memory was readable in';
  rsNoStartAddress = 'If you don''t include the header data you''ll have to specify the startaddress yourself when loading the file(That means Cheat Engine wont fill in the '
    +'startaddress text field when loaded for you)';
  rsIsNotAValidAddress = '%s is not a valid address';

type Tregion=class
  public
    fromaddress,toaddress:ptrint;
    constructor create(fa,ta:ptrint);
end;

constructor TRegion.create(fa,ta:ptrint);
begin
  fromaddress:=fa;
  toaddress:=ta;
  inherited create;
end;

procedure TfrmSaveMemoryRegion.Button2Click(Sender: TObject);
begin
  close;
end;

procedure TfrmSaveMemoryRegion.Button1Click(Sender: TObject);
var f: TFilestream;
    fromaddress,toaddress: qword;
    temp: ptrUint;
    size: dword;
    buf: array of pointer;

    i: integer;
    unreadable: array of integer;
    s: string;
begin
  if lbregions.Items.Count=0 then
  begin
    if (editfrom.Text<>'') and (editto.Text<>'') then
      button3.click
    else
      raise exception.Create(rsPleaseAddAtLeastOneAddressRegionToTheList);
  end;


  setlength(unreadable,0);
  setlength(buf,lbregions.items.count);

  for i:=0 to lbregions.items.count-1 do
  begin
    fromaddress:=tregion(lbregions.Items.Objects[i]).fromaddress;
    toaddress:=tregion(lbregions.Items.Objects[i]).toaddress;

    size:=toaddress-fromaddress+1;
    getmem(buf[i],size);

    if (not readprocessmemory(processhandle,pointer(ptrUint(fromaddress)),buf[i],size,temp)) then
    begin
      if (temp<>size) then
      begin
        setlength(unreadable,length(unreadable)+1);
        unreadable[length(unreadable)-1]:=i;
      end;
    end;
  end;

  if length(unreadable)>0 then
  begin
    for i:=0 to length(buf)-1 do
      freememandnil(buf[i]);

    s:=rsNotAllTheMemoryWasReadableIn;
    for i:=0 to length(unreadable)-1 do
      s:=s+' '+lbregions.items[unreadable[i]];

    raise exception.Create(s);
  end;

  if savedialog1.Execute then
  begin
    f:=TFilestream.Create(savedialog1.FileName,fmCreate);
    try
      if not dontinclude.checked then
      begin
        f.WriteBuffer(pchar('CHEATENGINE')^,11);
        temp:=3; //version
        f.WriteBuffer(temp,4);
      end;

      for i:=0 to length(buf)-1 do
      begin
        if not dontinclude.checked then
        begin
          fromaddress:=tregion(lbregions.Items.Objects[i]).fromaddress;
          toaddress:=tregion(lbregions.Items.Objects[i]).toaddress;
          size:=toaddress-fromaddress+1;

          f.WriteBuffer(fromaddress,8);
          f.WriteBuffer(size,4);
        end;
        f.WriteBuffer(buf[i]^,size);
        freememandnil(buf[i]);
      end;

      modalresult:=mrOK;
    finally
      f.Free;
    end;
  end;


end;

procedure TfrmSaveMemoryRegion.DontIncludeClick(Sender: TObject);
begin
  if not alreadywarned then
  if DontInclude.Checked then
  begin
    alreadywarned:=true;
    messagedlg(rsNoStartAddress, mtInformation, [mbok], 0);
  end;
end;

procedure TfrmSaveMemoryRegion.Button3Click(Sender: TObject);
var fromaddress,toaddress:qword;
begin
  try
    fromaddress:=StrToQWordEx('$'+editfrom.Text);
  except
    fromaddress:=symhandler.getAddressFromName(editfrom.Text);
  end;

  try
    toaddress:=StrToQWordEx('$'+editto.text);
  except
    toaddress:=symhandler.getAddressFromName(editto.text);
  end;

  if fromaddress>toaddress then
  begin  //xor swap
    fromaddress:=fromaddress xor toaddress;
    toaddress:=toaddress xor fromaddress;
    fromaddress:=fromaddress xor toaddress;
  end;

  lbregions.Items.AddObject(inttohex(fromaddress,8)+'-'+inttohex(toaddress,8),tregion.Create(fromaddress,toaddress));

  dontinclude.Enabled:=lbregions.Items.Count<=1;
  if lbregions.Items.Count>1 then dontinclude.Checked:=false;

end;

procedure TfrmSaveMemoryRegion.FormShow(Sender: TObject);
begin
  editfrom.ClientWidth:=canvas.textwidth('DDDDDDDDDDDD');
  editTo.ClientWidth:=editfrom.ClientWidth;

end;

procedure TfrmSaveMemoryRegion.lbRegionsDblClick(Sender: TObject);
var i: integer;
begin
  if lbregions.ItemIndex<>-1 then
  begin
    tregion(lbregions.Items.Objects[lbregions.ItemIndex]).Free;
    i:=0;
    while i<lbregions.Count do
    begin
      if lbregions.Selected[i] then
        lbregions.Items.Delete(i)
      else
        inc(i);
    end;
  end;

  dontinclude.Enabled:=lbregions.Items.Count<=1;
end;

procedure TfrmSaveMemoryRegion.miClearListClick(Sender: TObject);
begin
  while lbRegions.Count>0 do
  begin
    tregion(lbregions.Items.Objects[0]).Free;
    lbregions.Items.Delete(0);
  end;

end;

initialization
  {$i frmSaveMemoryRegionUnit.lrs}

end.

