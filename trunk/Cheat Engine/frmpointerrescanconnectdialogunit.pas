unit frmpointerrescanconnectdialogunit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, resolve, Sockets;

type

  { TfrmPointerrescanConnectDialog }

  TfrmPointerrescanConnectDialog = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    edtHost: TEdit;
    edtPort: TEdit;
    lblHost: TLabel;
    lblPort: TLabel;
    procedure btnOKClick(Sender: TObject);
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
        raise exception.create('host:'+edtHost.text+' could not be resolved');
    end;


  finally
    hr.free;
  end;


  port:=strtoint(edtport.text);

  modalresult:=mrok;
end;

end.

