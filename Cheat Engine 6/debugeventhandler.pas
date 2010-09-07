unit debugeventhandler;

{$MODE Delphi}

interface

uses
  jwawindows, Windows, Classes, SysUtils, syncobjs, GuiSafeCriticalSection,
  disassembler, cefuncproc, newkernelhandler,debuggertypedefinitions, frmTracerUnit,
  DebuggerInterfaceAPIWrapper, LuaHandler, lua, lauxlib, lualib;

type
  TDebugEventHandler = class;

  TDebugThreadHandler = class
  private
    debuggerthread: TObject;
    breakpointCS: TGuiSafeCriticalSection;

    onAttachEvent: TEvent;
    onContinueEvent: TEvent;
    realcontextpointer: pointer;
    threadlist: TList;
    breakpointList: TList;
    continueOption: TContinueOption;

    setInt3Back: boolean;
    Int3setbackAddress: ptrUint;

    singlestepping: boolean;

    //break and trace:
    isTracing: boolean;
    tracecount: integer;
    traceWindow: TfrmTracer;
    traceQuitCondition: string;
    //------------------

    WaitingToContinue: boolean; //set to true when it's waiting for the user to continue

    DebugEventString: string; //for outputdebugstring event

    function CheckIfConditionIsMet(bp: PBreakpoint; script: string=''): boolean;


    function HandleExceptionDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
    //even though it's private, it's accessible from this unit
    function CreateThreadDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
    function CreateProcessDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
    function ExitThreadDebugEvent(debugevent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
    function ExitProcessDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
    function LoadDLLDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
    function UnloadDLLDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
    function OutputDebugStringEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
    function RipEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
    function HandleUnknownEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;

    function DispatchBreakpoint(address: uint_ptr; var dwContinueStatus: dword): boolean;
    function singleStep(var dwContinueStatus: dword): boolean;

    procedure ModifyRegisters(bp: PBreakpoint);
    procedure handleTrace;
    procedure HandleBreak(bp: PBreakpoint);
    procedure ContinueFromBreakpoint(bp: PBreakpoint; continueoption: TContinueOption);
    //sync functions
    procedure visualizeBreak;
    procedure AddDebugEventString;
  public
    isHandled: boolean; //set to true if this thread is the current debug target
    ProcessId: dword;
    ThreadId:  dword;
    handle: THandle;
    context: PContext;  //PContext but it does belong to this thread. It's due to alignment issues

    procedure TracerQuit;
    procedure suspend;
    procedure resume;
    procedure fillContext;
    procedure setContext;
    procedure breakThread;
    procedure continueDebugging(continueOption: TContinueOption);
    constructor Create(debuggerthread: TObject; attachEvent: Tevent; continueEvent: Tevent; breakpointlist: Tlist; threadlist: Tlist; breakpointCS: TGuiSafeCriticalSection);
    destructor destroy; override;

    property isSingleStepping: boolean read singlestepping;
    property isWaitingToContinue: boolean read WaitingToContinue;
  end;

  TDebugEventHandler = class
  private
    debuggerthread: TObject;
    breakpointCS: TGuiSafeCriticalSection;
    threadlistCS: TGuiSafeCriticalSection;
    fOnAttachEvent: TEvent;
    fOnContinueEvent: TEvent;
    breakpointlist: TList;
    threadlist: TList;

    currentdebugEvent: TDEBUGEVENT;
    procedure updatethreadlist;
    procedure UpdateDebugEventWindow;
  public
    function HandleDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
    constructor Create(debuggerthread: TObject; OnAttachEvent: TEvent; OnContinueEvent: Tevent; breakpointlist: Tlist; threadlist: Tlist; breakpointCS: TGuiSafeCriticalSection; threadlistCS: TGuiSafeCriticalSection);
  end;

implementation

uses foundcodeunit, DebugHelper, MemoryBrowserFormUnit, frmThreadlistunit, frmDebugEventsUnit, formdebugstringsunit;

procedure TDebugThreadHandler.AddDebugEventString;
begin
  if formdebugstrings<>nil then
    formdebugstrings.ListBox1.Items.add(DebugEventString);
end;

procedure TDebugThreadHandler.VisualizeBreak;
begin
  MemoryBrowser.lastdebugcontext:=context^;
  MemoryBrowser.UpdateDebugContext(self.Handle, self.ThreadId);
end;

procedure TDebugThreadHandler.fillContext;
var
  i: integer;
begin
  if handle<>0 then
  begin
    context.ContextFlags := CONTEXT_ALL;// or CONTEXT_MMX_REGISTERS;
    outputdebugstring(pchar(format('GetThreadContext(%d,%p)',[handle,@context])));


    if not getthreadcontext(handle, context^,  isHandled) then
    begin
      i := getlasterror;
      outputdebugstring(PChar('getthreadcontext error:' + IntToStr(getlasterror)));
    end;
  end else outputdebugstring('fillContext: handle=0');
end;

procedure TDebugThreadHandler.setContext;
var
  i: integer;
begin
  outputdebugstring('TDebugThreadHandler.setContext');

  if handle<>0 then
  begin
    context:=self.context;
    context.ContextFlags := CONTEXT_ALL;
    if not setthreadcontext(self.handle, context^, isHandled) then
    begin
      i := getlasterror;
      outputdebugstring(PChar('setthreadcontext error:' + IntToStr(getlasterror)));
    end;
  end else outputdebugstring('fillContext: handle=0');
end;

procedure TDebugThreadHandler.suspend;
begin
  if handle<>0 then
    suspendthread(handle);
end;

procedure TDebugThreadHandler.resume;
begin
  if handle<>0 then
    resumethread(handle);
end;

procedure TDebugThreadHandler.breakThread;
begin
  suspend;
  fillContext;
  context.eflags:=eflags_setTF(context.eflags,1);
  SingleStepping:=true;

  setContext;
  resume;
end;

function booltoint(b: boolean):integer; inline;
begin
  if b then result:=1 else result:=0;
end;

procedure TDebugThreadHandler.ModifyRegisters(bp: PBreakpoint);
begin
  if bp.changereg.change_af then context.EFlags:=eflags_setAF(context.Eflags, booltoint(bp.changereg.new_af));
  if bp.changereg.change_cf then context.EFlags:=eflags_setCF(context.Eflags, booltoint(bp.changereg.new_cf));
  if bp.changereg.change_of then context.EFlags:=eflags_setOF(context.Eflags, booltoint(bp.changereg.new_of));
  if bp.changereg.change_pf then context.EFlags:=eflags_setPF(context.Eflags, booltoint(bp.changereg.new_pf));
  if bp.changereg.change_sf then context.EFlags:=eflags_setSF(context.Eflags, booltoint(bp.changereg.new_sf));
  if bp.changereg.change_zf then context.EFlags:=eflags_setZF(context.Eflags, booltoint(bp.changereg.new_zf));

  if bp.changereg.change_eax then context.{$ifdef cpu64}rax{$else}eax{$endif}:=bp.changereg.new_eax;
  if bp.changereg.change_ebx then context.{$ifdef cpu64}rbx{$else}ebx{$endif}:=bp.changereg.new_ebx;
  if bp.changereg.change_ecx then context.{$ifdef cpu64}rcx{$else}ecx{$endif}:=bp.changereg.new_ecx;
  if bp.changereg.change_edx then context.{$ifdef cpu64}rdx{$else}edx{$endif}:=bp.changereg.new_edx;
  if bp.changereg.change_esi then context.{$ifdef cpu64}rsi{$else}esi{$endif}:=bp.changereg.new_esi;
  if bp.changereg.change_edi then context.{$ifdef cpu64}rdi{$else}edi{$endif}:=bp.changereg.new_edi;
  if bp.changereg.change_esp then context.{$ifdef cpu64}rsp{$else}esp{$endif}:=bp.changereg.new_esp;
  if bp.changereg.change_eip then context.{$ifdef cpu64}rip{$else}eip{$endif}:=bp.changereg.new_eip;

  {$ifdef cpu64}
  if bp.changereg.change_r8 then context.r8:=bp.changereg.new_r8;
  if bp.changereg.change_r9 then context.r9:=bp.changereg.new_r9;
  if bp.changereg.change_r10 then context.r10:=bp.changereg.new_r10;
  if bp.changereg.change_r11 then context.r11:=bp.changereg.new_r11;
  if bp.changereg.change_r12 then context.r12:=bp.changereg.new_r12;
  if bp.changereg.change_r13 then context.r13:=bp.changereg.new_r13;
  if bp.changereg.change_r14 then context.r14:=bp.changereg.new_r14;
  if bp.changereg.change_r15 then context.r15:=bp.changereg.new_r15;
  {$endif}
end;

procedure TDebugThreadHandler.continueDebugging(continueOption: TContinueOption);
begin
  self.continueOption:=continueOption;
  onContinueEvent.SetEvent;
end;

procedure TDebugThreadHandler.ContinueFromBreakpoint(bp: PBreakpoint; continueoption: TContinueOption);
{
Continues the current thread from a debug event. Handles int3 breakpioints as well
BP can be nil if it's a single step breakpoint

}
var oldprotect,bw: dword;
begin
  if (bp<>nil) then
  begin
    if (bp.breakpointMethod=bpmInt3) then
    begin
      //bp is set and it's an int3 breakpoint
      VirtualProtectEx(Processhandle, pointer(bp.address), 1, PAGE_EXECUTE_READWRITE, oldprotect);
      WriteProcessMemory(processhandle, pointer(bp.address), @bp.originalbyte, 1, bw);
      VirtualProtectEx(Processhandle, pointer(bp.address), 1, oldprotect, oldprotect);

      if not bp.OneTimeOnly then //if it's not a one time only breakpoint then set it back on next instruction
      begin
        context.EFlags:=eflags_setTF(context.EFlags,1); //set the trap flag so it'll break on next instruction
        setInt3Back:=true;
        Int3setbackAddress:=bp.address;
      end;
    end;


    if (not singlestepping) and ((bp.ThreadID<>0) and (bp.threadid<>self.ThreadId)) then
    begin
      //not singlestepping and this breakpoint isn't set to break for this thread, so:
      context.EFlags:=eflags_setRF(context.EFlags,1);//don't break on the current instruction
      exit; //and exit
    end;
  end;


  case continueoption of
    co_run:
    begin
      //just continue
      singlestepping:=false;

      if (bp=nil) or (bp.breakpointMethod=bpmDebugRegister) then
      begin
        //it's a debug register breakpoint or single step, we can continue by just setting the RF flag so it won't break on next execution
        context.EFlags:=eflags_setRF(context.EFlags,1);
      end

    end;

    co_stepinto:
    begin
      //single step
      singlestepping:=true;
      if (bp=nil) or (bp.breakpointMethod=bpmDebugRegister) then
        context.EFlags:=eflags_setRF(context.EFlags,1);//don't break on the current instruction

      context.EFlags:=eflags_setTF(context.EFlags,1); //set the trap flag
    end;

    //the other event types are just setting of one time breakpoints

  end;
end;

procedure TDebugThreadHandler.TracerQuit;
begin
  tracewindow:=nil;
end;

procedure TDebugThreadHandler.handleTrace;
begin
  if tracewindow<>nil then
    TDebuggerthread(debuggerthread).Synchronize(TDebuggerthread(debuggerthread), tracewindow.AddRecord);

  dec(tracecount);
  if tracecount>0 then
  begin
    if traceQuitCondition<>'' then
    begin
      if CheckIfConditionIsMet(nil, 'return '+traceQuitCondition) then
      begin
        //quit condition is met
        ContinueFromBreakpoint(nil, co_run);
        isTracing:=false;
        exit;
      end;
    end;


    ContinueFromBreakpoint(nil, co_stepinto);
  end
  else
  begin
    ContinueFromBreakpoint(nil, co_run);
    isTracing:=false;
  end;
end;

procedure TDebugThreadHandler.HandleBreak(bp: PBreakpoint);
begin
  TDebuggerthread(debuggerthread).synchronize(TDebuggerthread(debuggerthread), VisualizeBreak);

  //go to sleep and wait for an event that wakes it up. No need to worry about deleted breakpoints, since the cleanup will not be called untill this routine exits
  onContinueEvent.ResetEvent;
  WaitingToContinue:=true;
  onContinueEvent.WaitFor(infinite);
  WaitingToContinue:=false;

  continueFromBreakpoint(bp, continueOption);
end;

function TDebugThreadHandler.singleStep(var dwContinueStatus: dword): boolean;
var
  hasSetInt3Back: boolean;
  oldprotect, bw: dword;
begin
  result:=true;

  if setInt3Back then
  begin
    VirtualProtectEx(Processhandle, pointer(Int3setbackAddress), 1, PAGE_EXECUTE_READWRITE, oldprotect);
    WriteProcessMemory(processhandle, pointer(Int3setbackAddress), @int3byte, 1, bw);
    VirtualProtectEx(Processhandle, pointer(Int3setbackAddress), 1, oldprotect, oldprotect);

    setInt3Back:=false;
    hasSetInt3Back:=true;
    dwContinueStatus:=DBG_CONTINUE;
  end else hasSetInt3Back:=false;

  if isTracing then
    handleTrace
  else
  if singlestepping then
    handlebreak(nil)
  else
    if not hasSetInt3Back then dwContinuestatus:=DBG_EXCEPTION_NOT_HANDLED; //if it wasn't a int3 set back or not expected single step, then raise an error
end;

function TDebugThreadHandler.CheckIfConditionIsMet(bp: PBreakpoint; script: string=''): boolean;
var
  i:integer;
begin
  result:=true;
  if (script<>'') or (bp<>nil) then
  begin
    if (script<>'') or (bp.conditonalbreakpoint.script<>nil) then
    begin
      if script='' then
      begin
        script:=bp.conditonalbreakpoint.script;
        if bp.conditonalbreakpoint.easymode then script:='return '+script;
      end;


      result:=CheckIfConditionIsMetContext(context, script);
    end;
  end;
end;

function TDebugThreadHandler.DispatchBreakpoint(address: uint_ptr; var dwContinueStatus: dword): boolean;
var
  found:  boolean;
  i:      integer;
  fcd: TFoundCodeDialog;
  c: PContext;

  bpp: PBreakpoint;
  bp: TBreakpoint;
begin
  outputdebugstring(format('DispatchBreakpoint(%x)',[address]));
  found := False;

  //check if it's an expected breakpoint
  //if not, DBG_EXCEPTION_NOT_HANDLED

  breakpointCS.enter;
  try
    for i := 0 to breakpointlist.Count - 1 do
    begin
      bpp:=PBreakpoint(breakpointlist.Items[i]);
      if (bpp.address = address) then
      begin
        found:=true;
        bp:=bpp^; //copy all the data of this breakpoint into a local copy (so when the lock is gone the user can fuck it up as much as he wants to, it won't affect the current handled bp

        if bpp^.OneTimeOnly then //delete it
          TdebuggerThread(debuggerthread).RemoveBreakpoint(bpp);

        break;
      end;
    end;
  finally
    breakpointCS.leave;
  end;

  if found then
  begin
    outputdebugstring('Handling breakpoint');

    OutputDebugString('Checking if condition is set');
    if not CheckIfConditionIsMet(@bp) then
    begin
      OutputDebugString('Condition was not met');

      continueFromBreakpoint(@bp, co_run);
      dwContinueStatus:=DBG_CONTINUE;
      Result:=true;
      exit;
    end;


    case bp.breakpointAction of
      bo_Break:
      begin
        //todo: check break conditions
        HandleBreak(@bp); //cause break in memory browser at address
      end;

      bo_BreakAndTrace:
      begin
        //remove the breakpoint and start tracing this thread X times
        if not isTracing then //don't handle it if already tracing
        begin
          isTracing:=true;
          tracecount:=bp.TraceCount;
          traceWindow:=bp.frmTracer;
          if bpp.traceendcondition<>nil then
            traceQuitCondition:=bpp.traceendcondition;

          TdebuggerThread(debuggerthread).RemoveBreakpoint(bpp);
        end;

        handleTrace;
      end;


      bo_ChangeRegister:
      begin
        //modify accordingly
        outputdebugstring('Handling bo_ChangeRegister breakpoint');

        ModifyRegisters(@bp);

        //and
        continueFromBreakpoint(@bp, co_run); //just continue running
      end;

      bo_FindCode:
      begin
        outputdebugstring('Save registers and continue');
        if bp.active then
        begin
          fcd:=bp.FoundcodeDialog;
          fcd.usesdebugregs:=bp.breakpointMethod=bpmDebugRegister;

          TDebuggerthread(debuggerthread).Synchronize(TDebuggerthread(debuggerthread), fcd.AddRecord);
        end;

        //nothing special is needed to continue
      end;

      bo_FindWhatCodeAccesses:
      begin
        TDebuggerthread(debuggerthread).Synchronize(TDebuggerthread(debuggerthread), bp.frmchangedaddresses.AddRecord);
        continueFromBreakpoint(@bp, co_run); //just continue running
      end;


    end;

    dwContinueStatus:=DBG_CONTINUE;
  end else dwContinueStatus:=DBG_EXCEPTION_NOT_HANDLED; //not an expected breakpoint

  Result := True;
end;

function TDebugThreadHandler.HandleExceptionDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
var
  exceptionAddress: ptrUint;
begin
  OutputDebugString('HandleExceptionDebugEvent:'+inttohex(debugEvent.Exception.ExceptionRecord.ExceptionCode,8));
  exceptionAddress := ptrUint(debugEvent.Exception.ExceptionRecord.ExceptionAddress);

  //if this is the first breakpoint exception check if it needs to tset the entry point bp
  if TDebuggerThread(debuggerthread).NeedsToSetEntryPointBreakpoint then
    TDebuggerthread(debuggerthread).Synchronize(TDebuggerthread(debuggerthread), TDebuggerthread(debuggerthread).SetEntryPointBreakpoint);


  case debugEvent.Exception.ExceptionRecord.ExceptionCode of
    EXCEPTION_BREAKPOINT:
    begin
      OutputDebugString('EXCEPTION_BREAKPOINT');
      Result := DispatchBreakpoint(exceptionAddress, dwContinueStatus);
    end;

    EXCEPTION_SINGLE_STEP, STATUS_WX86_SINGLE_STEP:
    begin
      OutputDebugString('EXCEPTION_SINGLE_STEP');

    //  debugEvent.Exception.ExceptionRecord.

      //find out what caused the breakpoint.
      //inspect DR6
      if (context.Dr6 and 1) = 1 then
        Result := DispatchBreakpoint(context.dr0, dwContinueStatus)
      else
      if ((context.Dr6 shr 1) and 1) = 1 then
        Result := DispatchBreakpoint(context.dr1, dwContinueStatus)
      else
      if ((context.Dr6 shr 2) and 1) = 1 then
        Result := DispatchBreakpoint(context.dr2, dwContinueStatus)
      else
      if ((context.Dr6 shr 3) and 1) = 1 then
        Result := DispatchBreakpoint(context.dr3, dwContinueStatus)
      else
        Result := SingleStep(dwContinueStatus);

      context.dr6:=0;
      setContext;
    end;

    else
      dwContinueStatus:=DBG_EXCEPTION_NOT_HANDLED;
  end;


  result:=true;
end;

function TDebugThreadHandler.CreateThreadDebugEvent(debugevent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
var i: integer;
begin
  OutputDebugString('CreateThreadDebugEvent');
  processid := debugevent.dwProcessId;
  threadid  := debugevent.dwThreadId;
  handle    := debugevent.CreateThread.hThread;

  Result    := true;

  //set all the debugregister breakpoints for this thread

  TDebuggerThread(debuggerthread).UpdateDebugRegisterBreakpointsForThread(self);

  dwContinueStatus:=DBG_CONTINUE;
end;

function TDebugThreadHandler.CreateProcessDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
var
  i: integer;
begin
  OutputDebugString('CreateProcessDebugEvent');

  handle    := debugevent.CreateProcessInfo.hThread;

  processid := debugevent.dwProcessId;
  threadid  := debugevent.dwThreadId;

  ProcessHandler.ProcessHandle := debugEvent.CreateProcessInfo.hProcess;
  ProcessHandler.processid     := debugEvent.dwProcessId;
  onAttachEvent.SetEvent;

  Result := true;
  dwContinueStatus:=DBG_CONTINUE;
end;

function TDebugThreadHandler.ExitThreadDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
var
  i: integer;
begin
  Outputdebugstring('ExitThreadDebugEvent');
  TDebuggerThread(debuggerthread).CurrentThread:=nil;
  Result := true;
  dwContinueStatus:=DBG_CONTINUE;
end;

function TDebugThreadHandler.ExitProcessDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
begin
  outputdebugstring('ExitProcessDebugEvent');
  dwContinueStatus:=DBG_CONTINUE;
  Result := False;
end;

function TDebugThreadHandler.LoadDLLDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
begin
  outputdebugstring('LoadDLLDebugEvent');
  Result := true;
  dwContinueStatus:=DBG_CONTINUE;
end;

function TDebugThreadHandler.UnloadDLLDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
begin
  outputdebugstring('UnloadDLLDebugEvent');
  Result := true;
  dwContinueStatus:=DBG_CONTINUE;
end;

function TDebugThreadHandler.OutputDebugStringEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
var s: pchar;
    ws: pwidechar;
    x: dword;
begin
  outputdebugstring('OutputDebugStringEvent');

  if FormDebugStrings<>nil then
  begin
    if debugEvent.DebugString.fUnicode>0 then
    begin
      ws:=getmem(debugEvent.DebugString.nDebugStringLength+2);
      try
        ReadProcessMemory(processhandle, debugEvent.DebugString.lpDebugStringData, ws, debugEvent.DebugString.nDebugStringLength,x);
        ws[debugEvent.DebugString.nDebugStringLength div 2]:=#0;
        ws[x div 2]:=#0;

        DebugEventString:=ws;

        TDebuggerthread(debuggerthread).Synchronize(TDebuggerthread(debuggerthread), AddDebugEventString);
      finally
        freemem(ws);
      end;
    end
    else
    begin
      s:=getmem(debugEvent.DebugString.nDebugStringLength+1);
      try
        ReadProcessMemory(processhandle, debugEvent.DebugString.lpDebugStringData, s, debugEvent.DebugString.nDebugStringLength,x);
        s[debugEvent.DebugString.nDebugStringLength]:=#0;
        s[x]:=#0;

        DebugEventString:=s;

        TDebuggerthread(debuggerthread).Synchronize(TDebuggerthread(debuggerthread), AddDebugEventString);
      finally
        freemem(s);
      end;
    end;


  end;

  Result := true;
  dwContinueStatus:=DBG_EXCEPTION_NOT_HANDLED;
end;

function TDebugThreadHandler.RipEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
begin
  outputdebugstring('RipEvent');
  Result := true;
  dwContinueStatus:=DBG_CONTINUE;

end;

function TDebugThreadHandler.HandleUnknownEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
begin
  OutputDebugString('Unknown event');
  Result := true;
  dwContinueStatus:=DBG_CONTINUE;
end;

destructor TDebugThreadHandler.destroy;
begin
  freemem(realcontextpointer);
  inherited destroy;
end;

constructor TDebugThreadHandler.Create(debuggerthread: TObject; attachEvent: Tevent; continueEvent: TEvent; breakpointlist: Tlist; threadlist: Tlist; breakpointCS: TGuiSafeCriticalSection);
begin
  //because fpc's structure is not alligned on a 16 byte base I have to allocate more memory and byteshift the structure if needed
  getmem(realcontextpointer,sizeof(TCONTEXT)+15);
  context:=pointer((ptrUint(realcontextpointer)+15) and ptrUint($fffffffffffffff0));

  self.debuggerthread := debuggerthread;
  onAttachEvent := attachEvent;
  onContinueEvent := continueEvent;
  self.breakpointList:=breakpointlist;
  self.threadlist:=threadlist;
  self.breakpointCS:=breakpointCS;
end;


function TDebugEventHandler.HandleDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
var
  currentThread: TDebugThreadHandler;
  i: integer;

begin
  OutputDebugString('HandleDebugEvent:'+inttostr(debugEvent.dwDebugEventCode));
  //find the TDebugThreadHandler class that belongs to this thread
  currentThread := nil;


  threadlistCS.enter;
  try
    for i := 0 to ThreadList.Count - 1 do
    begin
      if TDebugThreadHandler(ThreadList.Items[i]).threadid = debugEvent.dwThreadId then
      begin
        currentThread := ThreadList.Items[i];
        break;
      end;
    end;
  finally
    threadlistCS.leave;
  end;

  if currentThread = nil then //not found
  begin
    //so create and add it
    currentThread := TDebugThreadHandler.Create(debuggerthread, fonattachEvent, fOnContinueEvent, breakpointlist, threadlist, breakpointCS);
    currentThread.processid := debugevent.dwProcessId;
    currentThread.threadid := debugevent.dwThreadId;

    threadlistCS.enter;
    try
      ThreadList.Add(currentThread);
    finally
      threadlistCS.leave;
    end;

    if frmthreadlist<>nil then
    begin
      currentdebugEvent:=debugEvent;
      TDebuggerthread(debuggerthread).Synchronize(TDebuggerthread(debuggerthread), updatethreadlist);
    end;
  end;

  currentthread.isHandled:=true;
  currentthread.FillContext;
  TDebuggerthread(debuggerthread).currentThread:=currentThread;

  if frmDebugEvents<>nil then
  begin
    currentdebugEvent:=debugEvent;
    TDebuggerthread(debuggerthread).Synchronize(TDebuggerthread(debuggerthread), UpdateDebugEventWindow);
  end;


  case debugEvent.dwDebugEventCode of
    EXCEPTION_DEBUG_EVENT:      Result := currentThread.HandleExceptionDebugEvent(debugevent, dwContinueStatus);
    CREATE_THREAD_DEBUG_EVENT:  Result := currentThread.CreateThreadDebugEvent(debugEvent, dwContinueStatus);
    CREATE_PROCESS_DEBUG_EVENT: Result := currentThread.CreateProcessDebugEvent(debugEvent, dwContinueStatus);
    EXIT_THREAD_DEBUG_EVENT:
    begin
      threadlistCS.Enter;
      try
        Result := currentThread.ExitThreadDebugEvent(debugEvent, dwContinueStatus);
        ThreadList.Remove(currentThread);
        currentThread.Free;
        currentthread:=nil;
      finally
        threadlistCS.Leave;
      end;
      if frmthreadlist<>nil then
        TDebuggerthread(debuggerthread).Synchronize(TDebuggerthread(debuggerthread), updatethreadlist);
    end;
    EXIT_PROCESS_DEBUG_EVENT:   Result := currentThread.ExitProcessDebugEvent(debugEvent, dwContinueStatus);
    LOAD_DLL_DEBUG_EVENT:       Result := currentThread.LoadDLLDebugEvent(debugEvent, dwContinueStatus);
    UNLOAD_DLL_DEBUG_EVENT:     Result := currentThread.UnloadDLLDebugEvent(debugEvent, dwContinueStatus);
    OUTPUT_DEBUG_STRING_EVENT:  Result := currentThread.OutputDebugStringEvent(debugEvent, dwContinueStatus);
    RIP_EVENT:                  Result := currentThread.RipEvent(debugEvent, dwContinueStatus);

    else
                                Result := currentThread.HandleUnknownEvent(debugEvent, dwContinueStatus);
  end;

  if currentthread<>nil then //if it wasn't a thread destruction tell this thread it isn't being handled anymore
    currentthread.isHandled:=false;

  OutputDebugString('Returned from handleExceptionDebugEvent');
end;

procedure TDebugEventHandler.updatethreadlist;
{synchronize routine that updates the threadlist when a change has happened}
begin
  if frmthreadlist<>nil then
    frmThreadlist.FillThreadlist;
end;

procedure TDebugEventHandler.UpdateDebugEventWindow;
{synchronize routine that updates the debug event window}
var eventtext: string;
begin
  if frmDebugEvents<>nil then //check if it's still here
  begin
    eventtext:=inttohex(currentdebugevent.dwDebugEventCode,1);
    case currentdebugevent.dwDebugEventCode of
      CREATE_PROCESS_DEBUG_EVENT: eventtext:='CREATE_PROCESS_DEBUG_EVENT';
      CREATE_THREAD_DEBUG_EVENT: eventtext:='CREATE_THREAD_DEBUG_EVENT';
      EXCEPTION_DEBUG_EVENT: eventtext:='EXCEPTION_DEBUG_EVENT';
      EXIT_PROCESS_DEBUG_EVENT: eventtext:='EXIT_PROCESS_DEBUG_EVENT';
      EXIT_THREAD_DEBUG_EVENT: eventtext:='EXIT_THREAD_DEBUG_EVENT';
      LOAD_DLL_DEBUG_EVENT: eventtext:='LOAD_DLL_DEBUG_EVENT';
      OUTPUT_DEBUG_STRING_EVENT: eventtext:='OUTPUT_DEBUG_STRING_EVENT';
      RIP_EVENT: eventtext:='RIP_EVENT';
      UNLOAD_DLL_DEBUG_EVENT: eventtext:='UNLOAD_DLL_DEBUG_EVENT';
    end;
    eventtext:=format('pid:%x tid:%x - %s',[currentdebugEvent.dwProcessId, currentdebugevent.dwThreadId, eventtext]);
    frmDebugEvents.lbDebugEvents.Items.Add(eventtext);
  end;


end;

constructor TDebugEventHandler.Create(debuggerthread: TObject; OnAttachEvent: TEvent; OnContinueEvent: TEvent; breakpointlist: Tlist; threadlist: Tlist; breakpointCS: TGuiSafeCriticalSection; threadlistCS: TGuiSafeCriticalSection);
begin
  self.debuggerthread := debuggerthread;
  fOnAttachEvent      := OnAttachEvent;
  fOnContinueEvent    := OnContinueEvent;
  self.breakpointlist := breakpointlist;
  self.threadlist     := threadlist;
  self.breakpointCS   := breakpointCS;
  self.threadlistCS   := threadlistCS;
  inherited Create;
end;


end.

