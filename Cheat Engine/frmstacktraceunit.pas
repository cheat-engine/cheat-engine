unit frmstacktraceunit;

{$MODE Delphi}

interface

uses
  windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
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

uses MemoryBrowserFormUnit;

function rpm64(hProcess:THANDLE; qwBaseAddress:dword64; lpBuffer:pointer; nSize:dword; lpNumberOfBytesRead:lpdword):bool;stdcall;
begin
  result:=false;
  {$ifndef cpu64}
  if qwBaseAddress>$FFFFFFFF then exit;
  {$endif}
  result:=newkernelhandler.readprocessmemory(hProcess, pointer(ptrUint(qwBaseAddress)), lpBuffer, nSize, lpNumberOfBytesRead^);
end;

procedure TfrmStacktrace.stacktrace(threadhandle:thandle;context:_context);
var
    stackframe: PStackframe64;
    cxt:_context;
    wow64ctx: CONTEXT32;
    a,b,c,d: dword;
    sa,sb,sc,sd:string;
    machinetype: dword;

    cp: pointer;
begin

  cxt:=context;
  cp:=@cxt;

  getmem(stackframe,sizeof(tstackframe));
  zeromemory(stackframe,sizeof(tstackframe));
  stackframe^.AddrPC.Offset:=context.{$ifdef cpu64}rip{$else}eip{$endif};
  stackframe^.AddrPC.mode:=AddrModeFlat;

  stackframe^.AddrStack.Offset:=context.{$ifdef cpu64}rsp{$else}esp{$endif};
  stackframe^.AddrStack.Mode:=addrmodeflat;

  stackframe^.AddrFrame.Offset:=context.{$ifdef cpu64}rbp{$else}ebp{$endif};
  stackframe^.AddrFrame.Mode:=addrmodeflat;

  listview1.items.clear;


//function StackWalk64(MachineType:dword; hProcess:THANDLE; hThread:THANDLE; StackFrame:LPSTACKFRAME64; ContextRecord:pointer;  ReadMemoryRoutine:TREAD_PROCESS_MEMORY_ROUTINE64; FunctionTableAccessRoutine:TFUNCTION_TABLE_ACCESS_ROUTINE64; GetModuleBaseRoutine:TGET_MODULE_BASE_ROUTINE64; TranslateAddress:TTRANSLATE_ADDRESS_ROUTINE64):bool;stdcall;external External_library name 'StackWalk64';
{$ifdef cpu32}
  machinetype:=IMAGE_FILE_MACHINE_I386
{$else}

  if processhandler.is64Bit then
    machinetype:=IMAGE_FILE_MACHINE_AMD64
  else
  begin
    //   if (debuggerthread<>nil) and (debuggerthread.CurrentThread<>nil) then

    ZeroMemory(@wow64ctx, sizeof (wow64ctx));
    wow64ctx.Eip:=cxt.Rip;       //shouldn't be needed though
    wow64ctx.Ebp:=cxt.Rbp;
    wow64ctx.Esp:=cxt.Rsp;
    machinetype:=IMAGE_FILE_MACHINE_I386;


    cp:=@wow64ctx;

  end;
{$endif}

  //because I provide a readprocessmemory the threadhandle just needs to be the unique for each thread. e.g threadid instead of threadhandle
  while stackwalk64(machinetype,processhandle,threadhandle,stackframe,cp, rpm64 ,SymFunctionTableAccess64,SymGetModuleBase64,nil) do
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

end;

procedure TfrmstackTrace.refreshtrace;
{
Called when the debugger is paused on a breakpoint
}
var c: _CONTEXT;
begin

  if (debuggerthread<>nil) and (debuggerthread.CurrentThread<>nil) then
    stacktrace(debuggerthread.CurrentThread.handle,MemoryBrowser.lastdebugcontext);
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
