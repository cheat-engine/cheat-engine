unit frmSetupPSNNodeUnit;

{$mode delphi}

interface

uses
  {$ifdef darwin}
  macport, lclintf,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, math, registry, betterControls;

type

  { TfrmSetupPSNNode }

  TfrmSetupPSNNode = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    Button1: TButton;
    cbAllowParents: TCheckBox;
    cbMaxFoundResults: TCheckBox;
    cbMaxTimeToScan: TCheckBox;
    cbAllowChildren: TCheckBox;
    cbConnectToOtherNode: TCheckBox;
    cbPriority: TComboBox;
    cbAutoTrustChildren: TCheckBox;
    cbAllowTempFiles: TCheckBox;
    edtPort: TEdit;
    edtChildPassword: TEdit;
    edtConnectIP: TEdit;
    edtConnectPort: TEdit;
    edtConnectPassword: TEdit;
    edtParentPassword: TEdit;
    edtPublicname: TEdit;
    edtMaxResultsToFind: TEdit;
    edtMaxTimeToScan: TEdit;
    edtThreadCount: TEdit;
    lblPasswordParent: TLabel;
    lblListenPort: TLabel;
    lblIP: TLabel;
    lblPort: TLabel;
    lblPasswordChild: TLabel;
    lblPassword: TLabel;
    lblPublicName: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblPriority: TLabel;
    lblThreadCount: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    rbConnectAsParent: TRadioButton;
    rbConnectAsChild: TRadioButton;
    procedure btnOKClick(Sender: TObject);
    procedure cbConnectToOtherNodeChange(Sender: TObject);
    procedure edtConnectPasswordChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    threadcount: integer;
    listenport: integer;
    connectport: integer;
    maxresultstofind: integer;
    maxtimetoscan: integer;
    priority: TThreadPriority;
  end;

var
  frmSetupPSNNode: TfrmSetupPSNNode;

implementation

uses CEFuncProc, mainunit2;

{$R *.lfm}

{ TfrmSetupPSNNode }

procedure TfrmSetupPSNNode.FormCreate(Sender: TObject);
var
  cpucount: integer;
  reg: Tregistry;
  names: TStringlist;
  i: integer;
  compname: pchar;
  compnamesize: dword;
begin
  cpucount:=GetCPUCount;

  //assumption: when a core with hyperthreading core is running at 100% it's hyperthreaded processor will be running at 90%
  //This means that 10 cores are needed to provide an equivalent for one extra core when hyperthreading is used
  //In short, leave the hyperhtreaded processors alone so the user can use that hardly useful processing power to surf the web or move the mouse...
  //(at most use one)
  if HasHyperthreading then
    cpucount:=ceil((cpucount / 2)+(cpucount / 4));

  edtThreadCount.text:=inttostr(cpucount);

  getmem(compname, 256);
  compnamesize:=255;

  {$IFDEF windows}
  if GetComputerName(compname, compnamesize) then
  begin
    compname[compnamesize]:=#0;
    edtPublicname.text:=compname;
  end
  else
  {$ENDIF}
    edtPublicname.text:=GetUserNameFromPID(GetProcessID)+'-'+inttohex(random(65536),1);


  FreeMemAndNil(compname);

  reg:=tregistry.Create;
  Reg.RootKey := HKEY_CURRENT_USER;
  try
    if Reg.OpenKey('\Software\'+strCheatEngine+'\PSNNodeConfig', false) then
    begin
      if reg.ValueExists('ThreadCount') then
        edtThreadCount.Text:=IntToStr(reg.ReadInteger('ThreadCount'));

      if reg.ValueExists('ThreadPriority') then
        cbPriority.ItemIndex:=reg.ReadInteger('ThreadPriority');

      if reg.ValueExists('ListenPort') then
        edtPort.Text:=reg.ReadInteger('ListenPort').ToString;

      if reg.ValueExists('PublicName') then
        edtPublicname.text:=reg.ReadString('PublicName');

      if reg.ValueExists('AllowIncomingParents') then
        cbAllowParents.Checked:=reg.ReadBool('AllowIncomingParents');

      if reg.ValueExists('ParentPassword') then
        edtParentPassword.text:=reg.ReadString('ParentPassword');

      if reg.ValueExists('AllowIncomingChildren') then
        cbAllowChildren.checked:=reg.ReadBool('AllowIncomingChildren');

      if reg.ValueExists('ChildPassword') then
        edtChildPassword.text:=reg.ReadString('ChildPassword');

      if reg.ValueExists('AutoTrustChildren') then
        cbAutoTrustChildren.checked:=reg.ReadBool('AutoTrustChildren');

      if reg.ValueExists('DefaultConnect') then
        cbConnectToOtherNode.checked:=reg.ReadBool('DefaultConnect');

      if reg.ValueExists('DefaultConnectAsChild') then
        rbConnectAsChild.checked:=reg.ReadBool('DefaultConnectAsChild');

      rbConnectAsParent.checked:=not rbConnectAsChild.checked;

      if reg.ValueExists('DefaultConnectIP') then
        edtConnectIP.text:=reg.ReadString('DefaultConnectIP');

      if reg.ValueExists('DefaultConnectPort') then
        edtConnectPort.text:=inttostr(reg.ReadInteger('DefaultConnectPort'));

      if reg.ValueExists('DefaultConnectPassword') then
        edtConnectPassword.text:=reg.ReadString('DefaultConnectPassword');

      if reg.ValueExists('StopScansAfterResultsFound') then
        cbMaxFoundResults.Checked:=reg.ReadBool('StopScansAfterResultsFound');

      if reg.ValueExists('StopResultCount') then
        edtMaxResultsToFind.text:=IntToStr(reg.ReadInteger('StopResultCount'));


      if reg.ValueExists('StopScansAfterTime') then
        cbMaxTimeToScan.Checked:=reg.ReadBool('StopScansAfterTime');

      if reg.ValueExists('StopTime') then
        edtMaxTimeToScan.text:=IntToStr(reg.ReadInteger('StopTime'));

      if reg.ValueExists('AllowTempFiles') then
        cbAllowTempFiles.checked:=reg.ReadBool('AllowTempFiles');

    end;
  finally
    reg.free;
  end;

end;

procedure TfrmSetupPSNNode.FormShow(Sender: TObject);
var i: integer;
begin
  edtThreadCount.width:=max(lblThreadCount.Width, edtThreadCount.width);

end;

procedure TfrmSetupPSNNode.edtConnectPasswordChange(Sender: TObject);
begin

end;

procedure TfrmSetupPSNNode.btnOKClick(Sender: TObject);
var reg: TRegistry;
begin
  threadcount:=strtoint(edtThreadcount.text);
  listenport:=strtoint(edtPort.text);

  if cbConnectToOtherNode.checked then
    connectport:=strtoint(edtConnectPort.text)
  else
    TryStrToInt(edtConnectPort.text, connectport);

  if cbMaxFoundResults.checked then
    maxresultstofind:=strtoint(edtMaxResultsToFind.text)
  else
    maxresultstofind:=0;

  if cbMaxTimeToScan.checked then
    maxtimetoscan:=strtoint(edtMaxTimeToScan.text)
  else
    maxtimetoscan:=0;

  case cbPriority.itemindex of
    0: priority:=tpIdle;
    1: priority:=tpLowest;
    2: priority:=tpLower;
    3: priority:=tpNormal;
    4: priority:=tpHigher;
    5: priority:=tpHighest;
    6: priority:=tpTimeCritical;
  end;

  reg:=tregistry.Create;
  Reg.RootKey := HKEY_CURRENT_USER;
  try
    if Reg.OpenKey('\Software\'+strCheatEngine+'\PSNNodeConfig', true) then
    begin
      reg.WriteInteger('ThreadCount', threadcount);
      reg.WriteInteger('ThreadPriority', cbPriority.itemindex);
      reg.WriteInteger('ListenPort', listenport);
      reg.WriteString('PublicName', edtPublicname.text);
      reg.WriteBool('AllowIncomingParents', cbAllowParents.Checked);
      reg.WriteString('ParentPassword', edtParentPassword.text);
      reg.WriteBool('AllowIncomingChildren', cbAllowChildren.checked);
      reg.WriteString('ChildPassword', edtChildPassword.text);
      reg.WriteBool('AutoTrustChildren', cbAutoTrustChildren.checked);
      reg.WriteBool('DefaultConnect', cbConnectToOtherNode.checked);
      reg.WriteBool('DefaultConnectAsChild', rbConnectAsChild.checked);
      reg.WriteString('DefaultConnectIP', edtConnectIP.text);
      reg.WriteInteger('DefaultConnectPort', connectport);
      reg.WriteString('DefaultConnectPassword', edtConnectPassword.text);
      reg.WriteBool('StopScansAfterResultsFound', cbMaxFoundResults.Checked);
      reg.WriteInteger('StopResultCount', maxresultstofind);
      reg.WriteBool('StopScansAfterTime', cbMaxTimeToScan.Checked);
      reg.WriteInteger('StopTime', maxtimetoscan);
      reg.WriteBool('AllowTempFiles', cbAllowTempFiles.checked);
    end;
  finally
    reg.free;
  end;




  modalresult:=mrok;
end;

procedure TfrmSetupPSNNode.cbConnectToOtherNodeChange(Sender: TObject);
var newstate: boolean;
begin
  newstate:=cbConnectToOtherNode.checked;
  rbConnectAsParent.enabled:=newstate;
  rbConnectAsChild.enabled:=newstate;

  lblIP.enabled:=newstate;
  lblPort.enabled:=newstate;
  lblPassword.enabled:=newstate;

  edtConnectIP.enabled:=newstate;
  edtConnectPort.enabled:=newstate;
  edtConnectPassword.enabled:=newstate;
end;

end.

