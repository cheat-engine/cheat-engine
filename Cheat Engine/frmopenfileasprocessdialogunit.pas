unit frmOpenFileAsProcessDialogUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TfrmOpenFileAsProcessDialog }

  TfrmOpenFileAsProcessDialog = class(TForm)
    mbOK: TButton;
    mkCancel: TButton;
    edtBaseAddress: TEdit;
    Label1: TLabel;
    Panel1: TPanel;
    rb32: TRadioButton;
    rb64: TRadioButton;
    procedure mbOKClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    startaddress: ptruint;
  end;

var
  frmOpenFileAsProcessDialog: TfrmOpenFileAsProcessDialog;

implementation

{$R *.lfm}

{ TfrmOpenFileAsProcessDialog }

procedure TfrmOpenFileAsProcessDialog.mbOKClick(Sender: TObject);
begin
  startaddress:=StrToInt64('$'+edtBaseAddress.text);
  modalresult:=mrok;
end;

end.

