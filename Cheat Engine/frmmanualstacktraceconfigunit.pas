unit frmManualStacktraceConfigUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmManualStacktraceConfig }

  TfrmManualStacktraceConfig = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    cbUseShadow: TCheckBox;
    edtEIP: TEdit;
    edtESP: TEdit;
    edtEBP: TEdit;
    edtShadowOrig: TEdit;
    edtShadowNew: TEdit;
    edtShadowSize: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblEBP: TLabel;
    lblESP: TLabel;
    lblEIP: TLabel;
    procedure btnOKClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    eip: ptruint;
    esp: ptruint;
    ebp: ptruint;
    useshadow: boolean;
    shadoworig: ptruint;
    shadownew: ptruint;
    shadowsize: integer;
  end;


implementation

{$R *.lfm}

{ TfrmManualStacktraceConfig }

uses symbolhandler;

procedure TfrmManualStacktraceConfig.btnOKClick(Sender: TObject);
begin
  eip:=symhandler.getAddressFromName(edtEIP.text);
  esp:=symhandler.getAddressFromName(edtESP.text);
  ebp:=symhandler.getAddressFromName(edtEBP.text);
  useshadow:=cbUseShadow.checked;
  shadoworig:=symhandler.getAddressFromName(edtShadowOrig.text);
  shadownew:=symhandler.getAddressFromName(edtShadownew.text);
  shadowsize:=strtoint(edtShadowSize.text);

  modalresult:=mrok;
end;

end.

