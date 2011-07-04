unit ConfigUnrandomizerFrm;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, LResources;

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


initialization
  {$i ConfigUnrandomizerFrm.lrs}

end.
