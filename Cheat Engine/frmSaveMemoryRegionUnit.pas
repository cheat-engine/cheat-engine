unit frmSaveMemoryRegionUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  {$ifdef netclient}
  netapis,
  {$else}
  newkernelhandler,
  {$endif}
  cefuncproc;

type
  TfrmSaveMemoryRegion = class(TForm)
    Label1: TLabel;
    editFrom: TEdit;
    Button1: TButton;
    Label2: TLabel;
    Label3: TLabel;
    editTo: TEdit;
    Button2: TButton;
    SaveDialog1: TSaveDialog;
    DontInclude: TCheckBox;
    Button3: TButton;
    lbRegions: TListBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure DontIncludeClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure lbRegionsDblClick(Sender: TObject);
  private
    { Private declarations }
    alreadywarned: boolean;
  public
    { Public declarations }
  end;

var
  frmSaveMemoryRegion: TfrmSaveMemoryRegion;

implementation

{$R *.dfm}

type Tregion=class
  public
    fromaddress,toaddress:dword;
    constructor create(fa,ta:dword);
end;

constructor TRegion.create(fa,ta:dword);
begin
  fromaddress:=fa;
  toaddress:=ta;
  inherited create;
end;

procedure TfrmSaveMemoryRegion.FormClose(Sender: TObject;
  var Action: TCloseAction);
var i: integer;
begin
  for i:=0 to lbregions.items.count-1 do
    tregion(lbregions.Items.Objects[i]).Free;

  action:=cafree;
end;

procedure TfrmSaveMemoryRegion.Button1Click(Sender: TObject);
var f: TFilestream;
    fromaddress,toaddress: dword;
    temp: dword;
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
      raise exception.Create('Please add at least one address region to the list');
  end;


  setlength(unreadable,0);
  setlength(buf,lbregions.items.count);

  for i:=0 to lbregions.items.count-1 do
  begin
    fromaddress:=tregion(lbregions.Items.Objects[i]).fromaddress;
    toaddress:=tregion(lbregions.Items.Objects[i]).toaddress;

    size:=toaddress-fromaddress;
    getmem(buf[i],size);

    if (not readprocessmemory(processhandle,pointer(fromaddress),buf[i],size,temp)) or (temp<>size) then
    begin
      setlength(unreadable,length(unreadable)+1);
      unreadable[length(unreadable)-1]:=i;
    end;
  end;

  if length(unreadable)>0 then
  begin
    for i:=0 to length(buf)-1 do
      freemem(buf[i]);

    s:='Not all the memory was readable in';
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
        temp:=2; //version
        f.WriteBuffer(temp,4);
      end;

      for i:=0 to length(buf)-1 do
      begin
        if not dontinclude.checked then
        begin
          fromaddress:=tregion(lbregions.Items.Objects[i]).fromaddress;
          toaddress:=tregion(lbregions.Items.Objects[i]).toaddress;
          size:=toaddress-fromaddress;

          f.WriteBuffer(fromaddress,4);
          f.WriteBuffer(size,4);
        end;
        f.WriteBuffer(buf[i]^,size);
        freemem(buf[i]);
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
    messagedlg('If you don''t include the header data you''ll have to specify the startaddress yourself when loading the file(That means Cheat Engine wont fill in the startaddress text field when loaded for you)',mtInformation,[mbok],0);
  end;
end;

procedure TfrmSaveMemoryRegion.Button3Click(Sender: TObject);
var fromaddress,toaddress:dword;
    temp:dword;
begin
  try
    fromaddress:=StrToInt('$'+editFrom.Text);
  except
    raise exception.Create(editfrom.Text+' is not a valid address');
  end;

  try
    toaddress:=StrToInt('$'+editto.Text);
  except
    raise exception.Create(editto.Text+' is not a valid address');
  end;


  if toaddress<fromaddress then
  begin
    temp:=fromaddress;
    fromaddress:=toaddress;
    toaddress:=temp;
  end;

  lbregions.Items.AddObject(inttohex(fromaddress,8)+'-'+inttohex(toaddress,8),tregion.Create(fromaddress,toaddress));

  dontinclude.Enabled:=lbregions.Items.Count<=1;
  if lbregions.Items.Count>1 then dontinclude.Checked:=false;

end;

procedure TfrmSaveMemoryRegion.lbRegionsDblClick(Sender: TObject);
begin
  if lbregions.ItemIndex<>-1 then
  begin
    tregion(lbregions.Items.Objects[lbregions.ItemIndex]).Free;
    lbregions.DeleteSelected;
  end;

  dontinclude.Enabled:=lbregions.Items.Count<=1;
end;

end.

