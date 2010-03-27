unit frmRescanPointerUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, LResources;

type

  { TfrmRescanPointer }

  TfrmRescanPointer = class(TForm)
    cbDelay: TCheckBox;
    edtDelay: TEdit;
    edtAddress: TEdit;
    cbValueType: TComboBox;
    Label1: TLabel;
    Panel2: TPanel;
    rbFindAddress: TRadioButton;
    rbFindValue: TRadioButton;
    Button1: TButton;
    Button2: TButton;
    procedure rbFindAddressClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation


procedure TfrmRescanPointer.rbFindAddressClick(Sender: TObject);
begin
  if rbFindAddress.Checked then
  begin
    edtAddress.Width:=cbValueType.Left+cbValueType.Width-edtAddress.Left;
    cbValueType.Visible:=false;
  end
  else
  begin
    edtAddress.Width:=rbFindAddress.Width;
    cbValueType.Visible:=true;
  end;
end;



procedure TfrmRescanPointer.FormCreate(Sender: TObject);
begin
  rbFindAddressClick(rbFindAddress);
end;

initialization
  {$i frmRescanPointerUnit.lrs}

end.
