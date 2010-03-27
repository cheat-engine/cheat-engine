unit frmThreadlistunit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, StdCtrls, LResources,CEDebugger, debugHelper;

type
  TfrmThreadlist = class(TForm)
    PopupMenu1: TPopupMenu;
    Break1: TMenuItem;
    threadlistview: TListView;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Break1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure FillThreadlist;
  end;

var
  frmThreadlist: TfrmThreadlist;

implementation

uses debugeventhandler;

procedure TfrmThreadlist.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
  frmthreadlist:=nil;
end;

procedure TfrmThreadlist.FormCreate(Sender: TObject);
begin
  fillthreadlist;
end;

procedure TFrmthreadlist.FillThreadlist;
var i: integer;
    lastselected: integer;
    threadlist: tlist;
    li: TListitem;
begin
  lastselected:=threadlistview.ItemIndex;

  threadlistview.Clear;

  if debuggerthread<>nil then
  begin
    threadlist:=debuggerthread.lockThreadlist;
    try
      for i:=0 to threadlist.Count-1 do
      begin
        li:=threadlistview.Items.Add;
        li.Caption:=inttohex(TDebugThreadHandler(threadlist[i]).ThreadId,1);
      end;
    finally
      debuggerthread.unlockThreadlist;
    end;
  end;

  if (lastselected<>-1) and (threadlistview.Items.Count>lastselected) then
    threadlistview.ItemIndex:=lastselected;
end;

procedure TfrmThreadlist.Break1Click(Sender: TObject);
var threadlist: tlist;
i: integer;
begin
  if threadlistview.Selected<>nil then
  begin
    threadlist:=debuggerthread.lockThreadlist;
    try
      for i:=0 to threadlist.Count-1 do
      begin
        if TDebugThreadHandler(threadlist[i]).ThreadId=strtoint('$'+threadlistview.selected.Caption) then
        begin
          TDebugThreadHandler(threadlist[i]).break;
          break;
        end;
      end;
    finally
      debuggerthread.unlockThreadlist;
    end;

  end;

end;

initialization
  {$i frmThreadlistunit.lrs}

end.
