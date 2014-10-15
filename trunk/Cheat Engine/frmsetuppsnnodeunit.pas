unit frmSetupPSNNodeUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, math;

type

  { TfrmSetupPSNNode }

  TfrmSetupPSNNode = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
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
    edtThreadCount: TEdit;
    edtPublicname: TEdit;
    edtMaxResultsToFind: TEdit;
    edtMaxTimeToScan: TEdit;
    lblPasswordParent: TLabel;
    lblListenPort: TLabel;
    lblIP: TLabel;
    lblPort: TLabel;
    lblPasswordChild: TLabel;
    lblPassword: TLabel;
    lblPublicName: TLabel;
    lblThreadCount: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblPriority: TLabel;
    rbConnectAsParent: TRadioButton;
    rbConnectAsChild: TRadioButton;
    procedure btnOKClick(Sender: TObject);
    procedure edtConnectPasswordChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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

uses CEFuncProc;

{$R *.lfm}

{ TfrmSetupPSNNode }

procedure TfrmSetupPSNNode.FormCreate(Sender: TObject);
var cpucount: integer;
begin
  cpucount:=GetCPUCount;

  //assumption: when a core with hyperthreading core is running at 100% it's hyperthreaded processor will be running at 90%
  //This means that 10 cores are needed to provide an equivalent for one extra core when hyperthreading is used
  //In short, leave the hyperhtreaded processors alone so the user can use that hardly useful processing power to surf the web or move the mouse...
  //(at most use one)
  if HasHyperthreading then
    cpucount:=ceil((cpucount / 2)+(cpucount / 4));

  edtThreadCount.text:=inttostr(cpucount);
  edtPublicname.text:=GetUserNameFromPID(GetProcessID)+'-'+inttohex(random(65536),1);
end;

procedure TfrmSetupPSNNode.edtConnectPasswordChange(Sender: TObject);
begin

end;

procedure TfrmSetupPSNNode.btnOKClick(Sender: TObject);
begin
  threadcount:=strtoint(edtThreadcount.text);
  listenport:=strtoint(edtPort.text);
  if cbConnectToOtherNode.checked then
    connectport:=strtoint(edtConnectPort.text);

  if cbMaxFoundResults.checked then
    maxresultstofind:=strtoint(edtMaxResultsToFind.text)
  else
    maxresultstofind:=0;

  if cbMaxTimeToScan.checked then
    maxtimetoscan:=strtoint(edtMaxTimeToScan.text)*1000
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


  modalresult:=mrok;
end;

end.

