unit CommentsUnit;

{$MODE Delphi}

interface

uses
  windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, LResources, ComCtrls, LuaHandler;

type

  { TComments }

  TComments = class(TForm)
    btnExecuteScript: TButton;
    Label1: TLabel;
    Memo1: TMemo;
    mLuaScript: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Button1: TButton;
    Panel2: TPanel;
    Panel3: TPanel;
    tsComment: TTabSheet;
    tsLuaScript: TTabSheet;
    procedure btnExecuteScriptClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure mLuaScriptChange(Sender: TObject);
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

procedure TComments.mLuaScriptChange(Sender: TObject);
begin

end;

procedure TComments.btnExecuteScriptClick(Sender: TObject);
begin
  LUA_DoScript(mLuaScript.text);
  showmessage('Successfully executed');
end;

procedure TComments.Panel1Resize(Sender: TObject);
begin
  button1.Left:=(panel1.ClientWidth div 2) - (button1.Width div 2);
  btnExecuteScript.left:=(panel2.clientwidth div 2) - (btnExecuteScript.width div 2);
end;

procedure TComments.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (memo1.Lines.Count>0) or (mLuaScript.lines.count>0) then
    mainform.Commentbutton.font.style:=mainform.Commentbutton.font.style+[fsBold]
  else
    mainform.Commentbutton.font.style:=mainform.Commentbutton.font.style-[fsBold]  
end;

initialization
  {$i CommentsUnit.lrs}

end.
