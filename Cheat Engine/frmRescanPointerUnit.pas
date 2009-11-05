unit frmRescanPointerUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmRescanPointer = class(TForm)
    edtAddress: TEdit;
    cbValueType: TComboBox;
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

{$R *.dfm}

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

end.
