unit CommentsUnit;

{$MODE Delphi}

interface

uses
  {$ifdef windows}
  windows,
  {$endif}
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, LResources, ComCtrls, LuaHandler, CEFuncProc;

type

  { TComments }

  TComments = class(TForm)
    Memo1: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Button1: TButton;
    tsComment: TTabSheet;
    procedure btnExecuteScriptClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mLuaScriptChange(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    {$ifdef windows}
    procedure WMGetMinMaxInfo(var Message: TMessage); message WM_GETMINMAXINFO;
    {$endif}
  public
    { Public declarations }
  end;

var
  Comments: TComments;

implementation

uses MainUnit;

{$ifdef windows}
procedure TComments.WMGetMinMaxInfo(var Message: TMessage);
var MMInfo: ^MINMAXINFO;
begin //the constraint function of the form behaves weird when draging from the top or left side, so I have to do this myself.
  MMInfo:=pointer(message.LParam);
  MMInfo.ptMinTrackSize:=point(300,240);
end;
{$endif}


procedure TComments.Button1Click(Sender: TObject);
begin
  close;

end;

procedure TComments.FormCreate(Sender: TObject);
begin
  pagecontrol1.ActivePage:=tsComment;
  LoadFormPosition(self);
end;

procedure TComments.FormDestroy(Sender: TObject);
begin
  SaveFormPosition(self);
end;

procedure TComments.FormShow(Sender: TObject);
begin
  memo1.font.height:=GetFontData(font.reference.Handle).Height;
end;

procedure TComments.mLuaScriptChange(Sender: TObject);
begin

end;

procedure TComments.btnExecuteScriptClick(Sender: TObject);
begin

end;

procedure TComments.Panel1Resize(Sender: TObject);
begin

end;

procedure TComments.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (memo1.Lines.Count>0) then
    mainform.Commentbutton.font.style:=mainform.Commentbutton.font.style+[fsBold]
  else
    mainform.Commentbutton.font.style:=mainform.Commentbutton.font.style-[fsBold]  
end;

initialization
  {$i CommentsUnit.lrs}

end.
