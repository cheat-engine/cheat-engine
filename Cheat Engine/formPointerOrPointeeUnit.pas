unit formPointerOrPointeeUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, LResources;

type
  TformPointerOrPointee = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  formPointerOrPointee: TformPointerOrPointee;

implementation


initialization
  {$i formPointerOrPointeeUnit.lrs}

end.
