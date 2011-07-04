unit PasteTableentryFRM;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
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
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPasteTableentry: TfrmPasteTableentry;

implementation

{$R *.dfm}

end.
