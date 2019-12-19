unit frmPointerscanConnectDialogUnit;
{OBSOLETE}

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Sockets, resolve, CEFuncProc;

type

  { TfrmPointerscanConnectDialog }

  TfrmPointerscanConnectDialog = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    cbPriority: TComboBox;
    cbUseLoadedPointermap: TCheckBox;
    edtHost: TEdit;
    edtPort: TEdit;
    edtThreadcount: TEdit;
    lblPriority: TLabel;
    lblNrOfThread: TLabel;
    lblHost: TLabel;
    lblPort: TLabel;
    odLoadPointermap: TOpenDialog;
    Panel1: TPanel;
    procedure btnOkClick(Sender: TObject);
    procedure cbUseLoadedPointermapChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    host: THostAddr;
    port: UInt16;
    threadcount: integer;
    scannerpriority: TThreadPriority;
  end;

resourcestring
  rsHostCouldNotBeResolved='host: %s could not be resolved';
  rsUseLoadedPointermap = 'Use loaded pointermap:';

var
  frmPointerscanConnectDialog: TfrmPointerscanConnectDialog;

implementation

{$R *.lfm}

{ TfrmPointerscanConnectDialog }

procedure TfrmPointerscanConnectDialog.btnOkClick(Sender: TObject);
var hr:THostResolver;
begin
  hr:=THostResolver.Create(nil);
  try

    host:=StrToNetAddr(edtHost.text);

    if host.s_bytes[4]=0 then
    begin
      if hr.NameLookup(edtHost.text) then
        host:=hr.NetHostAddress
      else
        raise exception.create(Format(rsHostCouldNotBeResolved, [edtHost.text]));
    end;


  finally
    hr.free;
  end;


  port:=strtoint(edtport.text);

  threadcount:=strtoint(edtthreadcount.text);
  case cbpriority.itemindex of
    0: scannerpriority:=tpIdle;
    1: scannerpriority:=tpLowest;
    2: scannerpriority:=tpLower;
    3: scannerpriority:=tpNormal;
    4: scannerpriority:=tpHigher;
    5: scannerpriority:=tpHighest;
    6: scannerpriority:=tpTimeCritical;
  end;


  modalresult:=mrok;
end;

procedure TfrmPointerscanConnectDialog.cbUseLoadedPointermapChange(
  Sender: TObject);
begin
  if cbUseLoadedPointermap.checked and odLoadPointermap.Execute then
    cbUseLoadedPointermap.Caption:=rsUseLoadedPointermap+ExtractFileName(odLoadPointermap.FileName)
  else
    cbUseLoadedPointermap.checked:=false;


end;

procedure TfrmPointerscanConnectDialog.FormCreate(Sender: TObject);
var
  cpucount: integer;
begin
  {cpucount:=GetCPUCount;
  if HasHyperthreading then
    cpucount:=1+(cpucount div 2);

  edtThreadcount.text:=inttostr(cpucount); }
end;

end.

