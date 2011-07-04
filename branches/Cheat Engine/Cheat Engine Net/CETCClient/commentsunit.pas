unit commentsunit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TComments = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Comments: TComments;

implementation

{$R *.dfm}

procedure TComments.Button1Click(Sender: TObject);
begin
  Close;
end;

end.
