unit frmstacktraceunit;

{$MODE Delphi}

interface

uses
  {$ifdef darwin}
  macport, macportdefines,
  {$endif}
  {$ifdef windows}
  windows, imagehlp,
  {$endif}
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs,NewKernelHandler, CEFuncProc, ComCtrls,CEDebugger, KernelDebugger,
  Menus, LResources, debughelper, symbolhandler, betterControls;

type

  { TfrmStacktrace }

  TfrmStacktrace = class(TForm)
    stImageList: TImageList;
    ListView1: TListView;
    miManualStackwalk: TMenuItem;
    PopupMenu1: TPopupMenu;
    Refresh1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListView1DblClick(Sender: TObject);
    procedure miManualStackwalkClick(Sender: TObject);
    procedure Refresh1Click(Sender: TObject);
  private
    { Private declarations }
    procedure refreshtrace;
  public
    { Public declarations }
    procedure shadowstacktrace(context: _context; stackcopy: pointer; stackcopysize: integer);
    procedure stacktrace(threadhandle:thandle;context:pcontext);
  end;

var
  frmStacktrace: TfrmStacktrace;

implementation

uses MemoryBrowserFormUnit, frmManualStacktraceConfigUnit, ProcessHandlerUnit,
  DBK32functions, symbolhandlerstructs, PEInfoFunctions, contexthandler;

var
  useShadow: boolean;
  shadowOrig: ptruint;
  shadowNew: ptruint;
  shadowSize: integer;

  {$ifdef windows}

function rpm64(hProcess:THANDLE; qwBaseAddress:dword64; lpBuffer:pointer; nSize:dword; lpNumberOfBytesRead:lpdword):bool;stdcall; //should be lpptruint but the header file isn't correct
var
    br: ptruint;
begin

  result:=false;
  {$ifndef cpu64}
  if qwBaseAddress>$FFFFFFFF then exit;
  {$endif}

  if useShadow and InRangeQ(qwBaseAddress, shadowOrig, shadoworig+shadowSize) then
    qwBaseAddress:=shadowNew+(qwBaseAddress-shadowOrig); //adjust the base address to the copy location

  result:=DBK32functions.ReadProcessMemory64(hProcess, qwBaseAddress, lpBuffer, nSize, br);

  if lpNumberOfBytesRead<>nil then
    lpNumberOfBytesRead^:=br;
end;





function get_module_base_routine64(hProcess:THANDLE; Address:dword64):dword64;stdcall;
var mi: TModuleInfo;
begin
  if symhandler.getmodulebyaddress(address,mi) then
    result:=mi.baseaddress
  else
    result:=0;
end;

var
  exceptionlists: array of TExceptionList;
  exceptionlistPID: dword;

procedure cleanupExceptionList;
var i: integer;
begin
  if (length(exceptionlists)>0) then
  begin
    for i:=0 to length(exceptionlists)-1 do
      exceptionlists[i].free;

    setlength(exceptionlists,0);
  end;
end;

function function_table_access_routine64(hProcess:THANDLE; AddrBase:dword64):pointer;stdcall;
var
  mb: qword;
  mi: TModuleInfo;
  i: integer;
  rte: TRunTimeEntry;
  el: TExceptionList;
begin
  result:=nil;
  el:=nil;

  if symhandler.getmodulebyaddress(AddrBase,mi) then
  begin
    //find the module
    for i:=0 to length(exceptionlists)-1 do
    begin
      if exceptionlists[i].ModuleBase=mi.baseaddress then
      begin
        el:=exceptionlists[i];
        break;
      end;
    end;

    if el=nil then //not cached yet
    begin
      el:=peinfo_getExceptionList(mi.baseaddress);
      if el<>nil then
      begin
        setlength(exceptionlists,length(exceptionlists)+1);
        exceptionlists[length(exceptionlists)-1]:=el;
      end;

    end;

    if el<>nil then
      result:=el.getRunTimeEntry(addrbase);
  end;
end;

{$endif}

procedure TfrmStacktrace.stacktrace(threadhandle:thandle;context:pcontext);
{$ifdef windows}
var
    cxt:_context;
    stackframe: TSTACKFRAME_EX;

    wow64ctx: CONTEXT32;
    a,b,c,d: dword;
    sa,sb,sc,sd:string;
    machinetype: dword;

    cp: pointer;

    found: boolean;
    i: integer;

    li: TListitem;
        contexthandler: TContextInfo;
    {$endif}


begin
{$ifdef windows}
  contexthandler:=getBestContextHandler;

  if processhandler.SystemArchitecture=archX86 then
  begin

    if (exceptionlistPID<>processid) and (length(exceptionlists)>0) then
      cleanupExceptionList;

    exceptionlistPID:=processid;

    cp:=contexthandler.getCopy(context);
    try
      zeromemory(@stackframe,sizeof(TSTACKFRAME_EX));
      stackframe.StackFrameSize:=sizeof(TSTACKFRAME_EX);

      stackframe.AddrPC.Offset:=context^.{$ifdef cpu64}rip{$else}eip{$endif};
      stackframe.AddrPC.mode:=AddrModeFlat;

      stackframe.AddrStack.Offset:=context^.{$ifdef cpu64}rsp{$else}esp{$endif};
      stackframe.AddrStack.Mode:=addrmodeflat;

      stackframe.AddrFrame.Offset:=context^.{$ifdef cpu64}rbp{$else}ebp{$endif};
      stackframe.AddrFrame.Mode:=addrmodeflat;

      listview1.items.clear;


    //function StackWalk64(MachineType:dword; hProcess:THANDLE; hThread:THANDLE; StackFrame:LPSTACKFRAME64; ContextRecord:pointer;  ReadMemoryRoutine:TREAD_PROCESS_MEMORY_ROUTINE64; FunctionTableAccessRoutine:TFUNCTION_TABLE_ACCESS_ROUTINE64; GetModuleBaseRoutine:TGET_MODULE_BASE_ROUTINE64; TranslateAddress:TTRANSLATE_ADDRESS_ROUTINE64):bool;stdcall;external External_library name 'StackWalk64';
    {$ifdef cpu32}
      machinetype:=IMAGE_FILE_MACHINE_I386;
    {$else}

      if processhandler.is64Bit then
        machinetype:=IMAGE_FILE_MACHINE_AMD64
      else
      begin
        //   if (debuggerthread<>nil) and (debuggerthread.CurrentThread<>nil) then

        ZeroMemory(@wow64ctx, sizeof (wow64ctx));
        wow64ctx.Eip:=context^.Rip;       //shouldn't be needed though
        wow64ctx.Ebp:=context^.Rbp;
        wow64ctx.Esp:=context^.Rsp;
        machinetype:=IMAGE_FILE_MACHINE_I386;

        copymemory(cp,@wow64ctx,sizeof(wow64ctx));
      end;
    {$endif}

      //because I provide a readprocessmemory the threadhandle just needs to be the unique for each thread. e.g threadid instead of threadhandle
      while stackwalk64(machinetype,processhandle,threadhandle,@stackframe,cp, rpm64 ,function_table_access_routine64, get_module_base_routine64,nil) do
      begin

        li:=listview1.Items.Add;
        li.data:=pointer(stackframe.AddrReturn.Offset);
        li.caption:=symhandler.getNameFromAddress(stackframe.AddrPC.Offset, true, true, false);
        li.SubItems.add(inttohex(stackframe.AddrStack.Offset,8));
        li.SubItems.add(inttohex(stackframe.AddrFrame.Offset,8));
        li.SubItems.add(symhandler.getNameFromAddress(stackframe.AddrReturn.Offset,true,true, false));

        a:=stackframe.Params[0];
        b:=stackframe.Params[1];
        c:=stackframe.Params[2];
        d:=stackframe.Params[3];

        sa:=symhandler.getNameFromAddress(a, found);
        sb:=symhandler.getNameFromAddress(b, found);
        sc:=symhandler.getNameFromAddress(c, found);
        sd:=symhandler.getNameFromAddress(d, found);

        listview1.items[listview1.Items.Count-1].SubItems.add(sa+','+sb+','+sc+','+sd+',...');
      end;
    finally
      freememandnil(cp);
    end;
  end;
  {$endif}
end;

procedure TfrmstackTrace.refreshtrace;
{
Called when the debugger is paused on a breakpoint
}
begin

  if (debuggerthread<>nil) and (debuggerthread.CurrentThread<>nil) and (MemoryBrowser.context<>nil) then
    stacktrace(debuggerthread.CurrentThread.handle,MemoryBrowser.context);
end;


procedure TfrmStacktrace.FormCreate(Sender: TObject);
begin
  refreshtrace;
end;

procedure TfrmStacktrace.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if frmstacktrace=self then  //it can be something else as well
    frmstacktrace:=nil;

  action:=cafree;
end;

procedure TfrmStacktrace.ListView1DblClick(Sender: TObject);
begin
  if listview1.Selected<>nil then
  begin
    memorybrowser.disassemblerview.TopAddress:=symhandler.getAddressFromName(listview1.Selected.Caption);
    if memorybrowser.visible=false then
      memorybrowser.show();
  end;
end;

procedure TfrmStacktrace.shadowstacktrace(context: _context; stackcopy: pointer; stackcopysize: integer);
begin
  {$ifdef windows}
  useshadow:=true;
  shadowOrig:=context.{$ifdef cpu64}rsp{$else}esp{$endif};
  shadowNew:=ptruint(stackcopy);
  shadowSize:=stackcopysize;


  stacktrace(GetCurrentThread, @context);
  {$endif}
end;


procedure TfrmStacktrace.miManualStackwalkClick(Sender: TObject);
var c: _CONTEXT;
    frmManualStacktraceConfig: TfrmManualStacktraceConfig;
begin
  {$ifdef windows}
  zeromemory(@c, sizeof(_CONTEXT));

  frmManualStacktraceConfig:=tfrmManualStacktraceConfig.create(self);
  if frmManualStacktraceConfig.showmodal=mrok then
  begin
    c.{$ifdef cpu64}Rip{$else}eip{$endif}:=frmManualStacktraceConfig.eip;
    c.{$ifdef cpu64}Rbp{$else}ebp{$endif}:=frmManualStacktraceConfig.ebp;
    c.{$ifdef cpu64}Rsp{$else}esp{$endif}:=frmManualStacktraceConfig.esp;

    if frmManualStacktraceConfig.useshadow then
    begin
      useshadow:=true;
      shadowOrig:=frmManualStacktraceConfig.shadoworig;
      shadowNew:=frmManualStacktraceConfig.shadownew;
      shadowSize:=frmManualStacktraceConfig.shadowsize;
    end;
    stacktrace(GetCurrentThreadId, @c);

    useShadow:=false;
  end;
  frmManualStacktraceConfig.free;

  {$endif}
end;

procedure TfrmStacktrace.Refresh1Click(Sender: TObject);
begin
  refreshtrace;
end;


initialization
  {$i frmstacktraceunit.lrs}


finalization
  {$ifdef windows}
  cleanupExceptionList;
  {$endif}


end.
