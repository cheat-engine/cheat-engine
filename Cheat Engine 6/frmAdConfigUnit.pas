unit frmAdConfigUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, cesupport;

resourcestring
  rsPercentageShown = 'Percentage shown';

type

  { TfrmAdConfig }

  TfrmAdConfig = class(TForm)
    cbOwnUrl: TCheckBox;
    cbCanClose: TCheckBox;
    ComboBox1: TComboBox;
    edtwidth: TEdit;
    edtHeight: TEdit;
    edtUrl: TEdit;
    edtExtra: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblUrl: TLabel;
    lblExtra: TLabel;
    lblPercentage: TLabel;
    rbTop: TRadioButton;
    rbRight: TRadioButton;
    rbBottom: TRadioButton;
    rbLeft: TRadioButton;
    Shape1: TShape;
    tbPercentage: TTrackBar;
    procedure cbCanCloseChange(Sender: TObject);
    procedure cbOwnUrlChange(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure edtHeightChange(Sender: TObject);
    procedure edtwidthChange(Sender: TObject);
    procedure edtwidthKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure PosChange(Sender: TObject);
    procedure tbPercentageChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    adPosition: integer;
    function ownUrl: string;
    function extraparam: string;
    function percentage: integer;
  end; 

var
  frmAdConfig: TfrmAdConfig;

implementation

{ TfrmAdConfig }

function TfrmAdConfig.ownurl: string;
begin
  if cbOwnUrl.checked then
    result:=edtUrl.text
  else
    result:='';
end;

function TfrmAdConfig.extraparam: string;
begin
  if cbOwnUrl.checked then
    result:=edtExtra.text
  else
    result:='';
end;

function TfrmAdConfig.percentage: integer;
begin
  if cbOwnUrl.checked then
    result:=tbPercentage.Position
  else
    result:=0;
end;

procedure TfrmAdConfig.cbOwnUrlChange(Sender: TObject);
begin
  lblUrl.enabled:=cbOwnUrl.checked;
  edtUrl.enabled:=cbOwnUrl.checked;
  lblPercentage.enabled:=cbOwnUrl.checked;
  tbPercentage.enabled:=cbOwnUrl.checked;
  lblExtra.enabled:=cbOwnUrl.checked;
  edtExtra.enabled:=cbOwnUrl.checked;
end;

procedure TfrmAdConfig.cbCanCloseChange(Sender: TObject);
begin
  adwindow.setCanClose(cbCanClose.checked);
  adwindow.handlemove;
end;

procedure TfrmAdConfig.ComboBox1Change(Sender: TObject);
begin
  if combobox1.itemindex<>-1 then
  begin
    edtWidth.text:=copy(combobox1.Text,1, pos('x',combobox1.text)-1);
    edtHeight.text:=copy(combobox1.Text,pos('x',combobox1.text)+1, length(combobox1.Text));
  end;
end;

procedure TfrmAdConfig.edtHeightChange(Sender: TObject);
begin
  try
    adwindow.ClientHeight:=strtoint(trim(edtHeight.text));
  except
  end;
  adwindow.handlemove;
end;

procedure TfrmAdConfig.edtwidthChange(Sender: TObject);
begin
  try
    adwindow.ClientWidth:=strtoint(edtwidth.text);
  except
  end;
  adwindow.handlemove;
end;

procedure TfrmAdConfig.edtwidthKeyPress(Sender: TObject; var Key: char);
begin
  combobox1.ItemIndex:=-1;
end;

procedure TfrmAdConfig.FormCreate(Sender: TObject);
begin
  adposition:=2;
end;

procedure TfrmAdConfig.PosChange(Sender: TObject);
begin
  if rbtop.checked then
  begin
    adwindow.setPosition(akTop);
    adposition:=0;
  end
  else
  if rbright.checked then
  begin
    adwindow.setPosition(akRight);
    adposition:=1;
  end
  else
  if rbBottom.checked then
  begin
    adwindow.setPosition(akBottom);
    adposition:=2;
  end
  else
  if rbLeft.checked then
  begin
    adWindow.setPosition(akLeft);
    adposition:=3
  end;
end;

procedure TfrmAdConfig.tbPercentageChange(Sender: TObject);
begin
  lblPercentage.Caption:=rsPercentageShown+': '+inttostr(tbPercentage.position)+ '%';
end;

initialization
  {$I frmAdConfigUnit.lrs}

end.

