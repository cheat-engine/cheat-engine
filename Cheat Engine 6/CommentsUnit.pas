unit CommentsUnit;

{$MODE Delphi}

interface

uses
  windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, LResources;

type
  TComments = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    procedure WMGetMinMaxInfo(var Message: TMessage); message WM_GETMINMAXINFO;
  public
    { Public declarations }
  end;

var
  Comments: TComments;

implementation

uses MainUnit;


procedure TComments.WMGetMinMaxInfo(var Message: TMessage);
var MMInfo: ^MINMAXINFO;
begin //the constraint function of the form behaves weird when draging from the top or left side, so I have to do this myself.
  MMInfo:=pointer(message.LParam);
  MMInfo.ptMinTrackSize:=point(300,240);
end;


procedure TComments.Button1Click(Sender: TObject);
begin
  close;
end;

procedure TComments.Panel1Resize(Sender: TObject);
begin
  button1.Left:=(panel1.ClientWidth div 2) - (button1.Width div 2);

end;

procedure TComments.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if memo1.Lines.Count>0 then
    mainform.Commentbutton.font.style:=mainform.Commentbutton.font.style+[fsBold]
  else
    mainform.Commentbutton.font.style:=mainform.Commentbutton.font.style-[fsBold]  
end;

initialization
  {$i CommentsUnit.lrs}

end.
