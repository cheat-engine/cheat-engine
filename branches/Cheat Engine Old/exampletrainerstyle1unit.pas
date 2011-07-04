unit exampletrainerstyle1unit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst, ExtCtrls;

type
  TExampletrainerstyle1 = class(TForm)
    CheckListBox1: TCheckListBox;
    Label1: TLabel;
    Image1: TImage;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Exampletrainerstyle1: TExampletrainerstyle1;

implementation

{$R *.DFM}

end.
