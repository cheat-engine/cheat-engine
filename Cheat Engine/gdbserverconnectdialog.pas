//Copyright Cheat Engine 2023

unit gdbserverconnectdialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  bettercontrols;

type

  { TfrmGDBServerConnectDialog }

  TfrmGDBServerConnectDialog = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    function getHostname: string;
    procedure setHostname(h: string);
    function getPort: word;
    procedure setPort(w: word);
  public
    property hostname: string read getHostname write setHostname;
    property port: word read getPort write setPort;
  end;

var
  frmGDBSererConnectDialog: TfrmGDBServerConnectDialog;

function getGDBHostAndPort(var host: string; port: word): boolean;

implementation

{ TfrmGDBServerConnectDialog }

uses ceregistry;

function getGDBHostAndPort(var host: string; port: word): boolean;
var f: TfrmGDBServerConnectDialog;
begin
  f:=TfrmGDBServerConnectDialog.Create(Application);

  f.hostname:=cereg.readString('LastGDBServerHost','localhost');
  f.port:=cereg.readInteger('LastGDBServerPort',33453);

  result:=f.showmodal=mrok;
  if result then
  begin
    host:=f.hostname;
    port:=f.port;

    cereg.writeString('LastGDBServerHost',f.hostname);
    cereg.writeInteger('LastGDBServerPort',f.port);

  end;
  f.free;
end;

function TfrmGDBServerConnectDialog.getHostname: string;
begin
  result:=edit1.text;
end;

procedure TfrmGDBServerConnectDialog.setHostname(h: string);
begin
  edit1.text:=h;
end;

function TfrmGDBServerConnectDialog.getPort: word;
begin
  result:=strtoint(edit2.text);
end;

procedure TfrmGDBServerConnectDialog.setPort(w: word);
begin
  edit2.text:=inttostr(w);
end;

procedure TfrmGDBServerConnectDialog.Button1Click(Sender: TObject);
begin
  strtoint(edit2.text);

  modalresult:=mrok;
end;

initialization
  {$I gdbserverconnectdialog.lrs}

end.

