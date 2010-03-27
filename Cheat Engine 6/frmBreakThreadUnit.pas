unit frmBreakThreadUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, LResources,CEDebugger, debughelper;

type
  Tfrmbreakthread = class(TForm)
    Label1: TLabel;
    Threadlistbox: TListBox;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ThreadlistboxDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    threadhandle: thandle;
  end;

var
  frmbreakthread: Tfrmbreakthread;

implementation

uses debugeventhandler;

procedure Tfrmbreakthread.Button1Click(Sender: TObject);
var i: integer;
threadlist: TList;
begin
  if (debuggerthread<>nil) and (threadlistbox.ItemIndex<>-1) then
  begin
    threadlist:=debuggerthread.lockThreadlist;
    try
      for i:=0 to threadlist.Count-1 do
        if TDebugThreadHandler(threadlist[i]).ThreadId=strtoint('$'+threadlistbox.Items[threadlistbox.ItemIndex]) then
        begin
          threadhandle:=TDebugThreadHandler(threadlist[i]).handle;
          modalresult:=mrok;
        end;
    finally
      debuggerthread.unlockThreadlist;
    end;
  end;

end;

procedure Tfrmbreakthread.FormCreate(Sender: TObject);
var i: integer;
threadlist: TList;
begin
  threadlistbox.clear;

  if debuggerthread<>nil then
  begin
    threadlist:=debuggerthread.lockThreadlist;
    try
      for i:=0 to threadlist.Count-1 do
        threadlistbox.Items.Add(inttohex(TDebugThreadHandler(threadlist[i]).ThreadId,8));

    finally
      debuggerthread.unlockThreadlist;
    end;
  end;

end;

procedure Tfrmbreakthread.ThreadlistboxDblClick(Sender: TObject);
begin
  button1.Click;
end;

initialization
  {$i frmBreakThreadUnit.lrs}

end.
