unit PasteTableentryFRM;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, LResources, ExtCtrls;

type

  { TfrmPasteTableentry }

  TfrmPasteTableentry = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    edtFind: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edtReplace: TEdit;
    Label3: TLabel;
    edtOffset: TEdit;
    Button1: TButton;
    Button2: TButton;
    Panel1: TPanel;
    procedure FormShow(Sender: TObject);
    procedure GroupBox2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPasteTableentry: TfrmPasteTableentry;

implementation


{ TfrmPasteTableentry }

procedure TfrmPasteTableentry.GroupBox2Click(Sender: TObject);
begin

end;

procedure TfrmPasteTableentry.FormShow(Sender: TObject);
begin
  Constraints.MinHeight:=height;
  Constraints.MaxHeight:=height;
end;

initialization
  {$i PasteTableentryFRM.lrs}

end.
