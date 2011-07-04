unit FormDebugStringsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TFormDebugStrings = class(TForm)
    ListBox1: TListBox;
    Panel1: TPanel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormDebugStrings: TFormDebugStrings;

implementation

{$R *.dfm}

procedure TFormDebugStrings.Button1Click(Sender: TObject);
begin
  close;
end;

end.
