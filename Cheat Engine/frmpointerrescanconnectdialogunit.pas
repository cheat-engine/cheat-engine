unit frmpointerrescanconnectdialogunit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, resolve, Sockets;

type

  { TfrmPointerrescanConnectDialog }

  TfrmPointerrescanConnectDialog = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    edtHost: TEdit;
    edtPort: TEdit;
    lblHost: TLabel;
    lblPort: TLabel;
    Panel1: TPanel;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    host: THostAddr;
    port: integer;
  end;

var
  frmPointerrescanConnectDialog: TfrmPointerrescanConnectDialog;

implementation

{$R *.lfm}

resourcestring
  rsHost = 'host:';
  rsCouldNotBeResolved = ' could not be resolved';

{ TfrmPointerrescanConnectDialog }

procedure TfrmPointerrescanConnectDialog.btnOKClick(Sender: TObject);
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
        raise exception.create(rsHost+edtHost.text+rsCouldNotBeResolved);
    end;


  finally
    hr.free;
  end;


  port:=strtoint(edtport.text);

  modalresult:=mrok;
end;

procedure TfrmPointerrescanConnectDialog.FormCreate(Sender: TObject);
begin

end;

end.

