unit frmBreakThreadUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, LResources, ExtCtrls,CEDebugger, debughelper;

type

  { Tfrmbreakthread }

  Tfrmbreakthread = class(TForm)
    Label1: TLabel;
    Panel1: TPanel;
    Threadlistbox: TListBox;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
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

procedure Tfrmbreakthread.FormShow(Sender: TObject);
begin
  button1.autosize:=false;
  button2.autosize:=false;
  if button2.Width>button1.width then
    button1.width:=button2.width else button2.width:=button1.width;
end;

procedure Tfrmbreakthread.ThreadlistboxDblClick(Sender: TObject);
begin
  button1.Click;
end;

initialization
  {$i frmBreakThreadUnit.lrs}

end.
