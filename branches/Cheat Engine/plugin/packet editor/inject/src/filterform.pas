unit filterform;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmFilter = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Button1: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmFilter: TfrmFilter;

implementation

{$R *.dfm}

end.
