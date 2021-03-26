unit frmFindDialogUnit;
{
The lazarus finddialog window has too many options I never use, AND when you
remove them, the up/down direction disappears as well, and overal shoddy look

Thus this version instead
}


{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, betterControls;

type

  { TfrmFindDialog }
  TFindDirection=(fdUp, fdDown);

  TfrmFindDialog = class(TForm)
    btnFind: TButton;
    Button1: TButton;
    cbCaseSensitive: TCheckBox;
    edtText: TEdit;
    gbDirection: TGroupBox;
    Label1: TLabel;
    lblDescription: TLabel;
    rbDirectionUp: TRadioButton;
    rbDirectionDown: TRadioButton;
  private
    { private declarations }
    function getFindText: string;
    procedure setFindText(s: string);
    function getDirection: TFindDirection;
    procedure setDirection(d: TFindDirection);
    function getCaseSensitive: boolean;
    procedure setCaseSensitive(s: boolean);

    function getShowDirection: boolean;
    procedure setShowDirection(s: boolean);
    function getShowCaseSensitive: boolean;
    procedure setShowCaseSensitive(s: boolean);

    function getDescription: string;
    procedure setDescription(s: string);
  public
    { public declarations }
    function execute:boolean;
    property FindText: string read getFindText write setFindText;
    property Description: string read getDescription write setDescription;
    property Direction: TFindDirection read getDirection write setDirection;
    property CaseSensitive: boolean read getCaseSensitive write setCaseSensitive;
    property ShowDirection: boolean read getShowDirection write setShowDirection;
    property ShowCaseSensitive: boolean read getShowCaseSensitive write setShowCaseSensitive;
  end;

implementation

{$R *.lfm}

function TfrmFindDialog.getDescription: string;
begin
  result:=lblDescription.caption;
end;

procedure TfrmFindDialog.setDescription(s: string);
begin
  lblDescription.caption:=s;
end;

function TfrmFindDialog.getShowDirection: boolean;
begin
  result:=gbDirection.Visible;
end;

procedure TfrmFindDialog.setShowDirection(s: boolean);
begin
  gbDirection.visible:=s;
end;

function TfrmFindDialog.getShowCaseSensitive: boolean;
begin
  result:=cbCaseSensitive.visible;
end;

procedure TfrmFindDialog.setShowCaseSensitive(s: boolean);
begin
  cbCaseSensitive.visible:=s;
end;

function TfrmFindDialog.getCaseSensitive: boolean;
begin
  result:=cbCaseSensitive.checked;
end;

procedure TfrmFindDialog.setCaseSensitive(s: boolean);
begin
  cbCaseSensitive.checked:=s;
end;

function TfrmFindDialog.getDirection: TFindDirection;
begin
  if rbDirectionUp.checked then
    result:=fdUp
  else
    result:=fdDown;
end;

procedure TfrmFindDialog.setDirection(d: TFindDirection);
begin
  if d=fdDown then
    rbDirectionDown.Checked:=true
  else
    rbDirectionUp.checked:=true;
end;

function TfrmFindDialog.getFindText: string;
begin
  result:=edtText.Text;
end;

procedure TfrmFindDialog.setFindText(s: string);
begin
  edtText.Text:=s;
end;

function TfrmFindDialog.execute:boolean;
begin
  result:=showmodal=mrok;
end;

end.

