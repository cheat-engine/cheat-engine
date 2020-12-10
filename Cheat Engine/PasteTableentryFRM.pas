unit PasteTableentryFRM;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, LResources, ExtCtrls, betterControls;

type

  { TfrmPasteTableentry }

  TfrmPasteTableentry = class(TForm)
    Button1: TButton;
    Button2: TButton;
    cbChildrenAsWell: TCheckBox;
    edtPointerLastOffset: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    edtFind: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edtReplace: TEdit;
    Label3: TLabel;
    edtOffset: TEdit;
    Label4: TLabel;
    Panel1: TPanel;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPasteTableentry: TfrmPasteTableentry;

implementation


{ TfrmPasteTableentry }


procedure TfrmPasteTableentry.FormShow(Sender: TObject);
begin
  Constraints.MinHeight:=height;
  Constraints.MaxHeight:=height;

  edtFind.constraints.MinWidth:=Label1.width * 2;
  edtReplace.constraints.MinWidth:=edtFind.constraints.MinWidth;
  edtOffset.Constraints.MinWidth:=edtFind.constraints.MinWidth;
  edtPointerLastOffset.Constraints.MinWidth:=edtFind.constraints.MinWidth;;
end;


initialization
  {$i PasteTableentryFRM.lrs}

end.
