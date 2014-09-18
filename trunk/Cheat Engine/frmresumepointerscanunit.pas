unit frmResumePointerscanUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, registry, multilineinputqueryunit, CEFuncProc, math,
  types;

type

  { TfrmResumePointerscan }

  TfrmResumePointerscan = class(TForm)
    btnNotifySpecificIPs: TButton;
    Button1: TButton;
    Button2: TButton;
    cbBroadcast: TCheckBox;
    cbDistributedScanning: TCheckBox;
    ComboBox1: TComboBox;
    edtDistributedPort: TEdit;
    edtThreadcount: TEdit;
    Label1: TLabel;
    Label9: TLabel;
    lblPort: TLabel;
    ListView1: TListView;
    odLoadPointermap: TOpenDialog;
    Panel1: TPanel;
    procedure btnNotifySpecificIPsClick(Sender: TObject);
    procedure cbDistributedScanningChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
  private
    { private declarations }
    procedure updateFileList;
  public
    { public declarations }
    iplist: TStringlist;
    instantrescanfiles: tstringlist;
  end;

implementation

{$R *.lfm}

{ TfrmResumePointerscan }

procedure TfrmResumePointerscan.updateFileList;
var
  oldindex: integer;
  i: integer;
  li: TListItem;
  fn: string;
begin
  oldindex:=listview1.ItemIndex;
  listview1.clear;
  for i:=0 to instantrescanfiles.count-1 do
  begin
    fn:=extractfilename(instantrescanfiles[i]);
    li:=listview1.Items.add;
    li.caption:=ansitoutf8(fn);
    li.SubItems.add(IntToHex(ptruint(instantrescanfiles.objects[i]), 8));

    if FileExists(instantrescanfiles[i]) then
      li.subitems.add('x')
  end;


  if (oldindex<>-1) and (oldindex<listview1.Items.count) then
    listview1.itemindex:=oldindex;
end;


procedure TfrmResumePointerscan.ListView1DblClick(Sender: TObject);
begin
  if (instantrescanfiles<>nil) and (listview1.ItemIndex<>-1) then
  begin
    odLoadPointermap.FileName:=instantrescanfiles[listview1.ItemIndex];

    if odLoadPointermap.execute then
      instantrescanfiles[listview1.ItemIndex]:=odLoadPointermap.FileName;


    updateFileList;

  end;


end;

procedure TfrmResumePointerscan.Panel1Resize(Sender: TObject);
begin
  listview1.Column[0].Width:=listview1.clientWidth-listview1.Column[1].Width-listview1.column[2].width;
end;

procedure TfrmResumePointerscan.FormCreate(Sender: TObject);
var
  cpucount: integer;
  reg: TRegistry;
begin
  cpucount:=GetCPUCount;

  if HasHyperthreading then
    cpucount:=ceil((cpucount / 2)+(cpucount / 4));

  edtThreadcount.text:=inttostr(cpucount);

  iplist:=TStringList.create;
  //load the ip list (if there is one)

  reg:=tregistry.create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('\Software\Cheat Engine',false) then
    begin
      if reg.ValueExists('Worker IP List') then
        iplist.Text:=reg.ReadString('Worker IP List');
    end;

  finally
    reg.free;
  end;

  instantrescanfiles:=TStringList.create;
end;

procedure TfrmResumePointerscan.FormDestroy(Sender: TObject);
begin
  if instantrescanfiles<>nil then
    instantrescanfiles.free;

  if iplist<>nil then
    iplist.free;
end;

procedure TfrmResumePointerscan.FormShow(Sender: TObject);
begin
  updateFileList;
end;

procedure TfrmResumePointerscan.cbDistributedScanningChange(Sender: TObject);
begin
  cbBroadcast.enabled:=cbDistributedScanning.checked;
end;

procedure TfrmResumePointerscan.btnNotifySpecificIPsClick(Sender: TObject);
var
  reg: Tregistry;
begin
  reg:=TRegistry.create;
  try
    if MultilineInputQuery('IP List','Enter the IP addresses to notify explicitly', iplist) then  //save the new ip list
    begin
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKey('\Software\Cheat Engine',true) then
        reg.WriteString('Worker IP List', iplist.text);
    end;

  finally
    reg.free;
  end;
end;

end.

