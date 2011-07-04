unit ConfigUnrandomizerFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmConfigUnrandomizer = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    edtDefault: TEdit;
    cbIncremental: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmConfigUnrandomizer: TfrmConfigUnrandomizer;

implementation

{$R *.dfm}

end.
