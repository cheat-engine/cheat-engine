unit CommentsUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TComments = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure WMGetMinMaxInfo(var Message: TMessage); message WM_GETMINMAXINFO;
  public
    { Public declarations }
  end;

var
  Comments: TComments;

implementation

{$R *.dfm}

procedure TComments.WMGetMinMaxInfo(var Message: TMessage);
var MMInfo: ^MINMAXINFO;
begin //the constraint function of the form behaves weird when draging from the top or left side, so I have to do this myself.
  MMInfo:=ptr(message.LParam);
  MMInfo.ptMinTrackSize:=point(300,240);
end;


procedure TComments.Button1Click(Sender: TObject);
begin
  close;
end;

end.
