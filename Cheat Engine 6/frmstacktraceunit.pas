unit frmstacktraceunit;

{$MODE Delphi}

interface

uses
  jwawindows, windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs,NewKernelHandler, CEFuncProc, ComCtrls,imagehlp,CEDebugger, KernelDebugger,
  Menus, LResources, debughelper;

type
  TfrmStacktrace = class(TForm)
    ListView1: TListView;
    PopupMenu1: TPopupMenu;
    Refresh1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Refresh1Click(Sender: TObject);
  private
    { Private declarations }
    procedure refreshtrace;
  public
    { Public declarations }
    procedure stacktrace(threadhandle:thandle;context:_context);
  end;

var
  frmStacktrace: TfrmStacktrace;

implementation


procedure TfrmStacktrace.stacktrace(threadhandle:thandle;context:_context);
var stackframe: PStackframe;
    cxt:_context;
    a,b,c,d: dword;
    sa,sb,sc,sd:string;
begin
  (*
  cxt:=context;

  getmem(stackframe,sizeof(tstackframe));
  zeromemory(stackframe,sizeof(tstackframe));
  stackframe^.AddrPC.Offset:=context.eip;
  stackframe^.AddrPC.mode:=AddrModeFlat;

  stackframe^.AddrStack.Offset:=context.Esp;
  stackframe^.AddrStack.Mode:=addrmodeflat;

  stackframe^.AddrFrame.Offset:=context.Ebp;
  stackframe^.AddrFrame.Mode:=addrmodeflat;

  listview1.items.clear;

  //because I provide a readprocessmemory the threadhandle just needs to be the unique for each thread. e.g threadid instead of threadhandle
  while stackwalk(IMAGE_FILE_MACHINE_I386,processhandle,threadhandle,stackframe,@cxt, newkernelhandler.readprocessmemory ,SymFunctionTableAccess,SymGetModuleBase,nil) do
  begin
    listview1.Items.Add.Caption:=inttohex(stackframe^.AddrPC.Offset,8);
    listview1.items[listview1.Items.Count-1].SubItems.add(inttohex(stackframe^.AddrStack.Offset,8));
    listview1.items[listview1.Items.Count-1].SubItems.add(inttohex(stackframe^.AddrFrame.Offset,8));
    listview1.items[listview1.Items.Count-1].SubItems.add(inttohex(stackframe^.AddrReturn.Offset,8));

    a:=stackframe^.Params[0];
    b:=stackframe^.Params[1];
    c:=stackframe^.Params[2];
    d:=stackframe^.Params[3];

    if integer(a)>$400000 then sa:='0x'+inttohex(a,8) else sa:=inttostr(integer(a));
    if integer(b)>$400000 then sb:='0x'+inttohex(b,8) else sb:=inttostr(integer(b));
    if integer(c)>$400000 then sc:='0x'+inttohex(c,8) else sc:=inttostr(integer(c));
    if integer(d)>$400000 then sd:='0x'+inttohex(d,8) else sd:=inttostr(integer(d));

    listview1.items[listview1.Items.Count-1].SubItems.add(sa+','+sb+','+sc+','+sd+',...');
  end;
  *)
end;

procedure TfrmstackTrace.refreshtrace;
{
Called when the debugger is paused on a breakpoint
}
var c: _CONTEXT;
begin
  if debuggerthread<>nil then
    stacktrace(debuggerthread.CurrentThread.handle.pausedthreadhandle,debuggerthread.context)
  else
  begin
    if kdebugger <> nil then
    begin
      Kdebugger.GetContext(c);
      stacktrace(0, c);
    end;
  end;
end;


procedure TfrmStacktrace.FormCreate(Sender: TObject);
begin
  refreshtrace;
end;

procedure TfrmStacktrace.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  frmstacktrace:=nil;
  action:=cafree;
end;

procedure TfrmStacktrace.Refresh1Click(Sender: TObject);
begin
  refreshtrace;
end;

initialization
  {$i frmstacktraceunit.lrs}

end.
