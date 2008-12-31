unit filterform;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmFilter = class(TForm)
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
