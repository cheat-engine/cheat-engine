unit debugeventhandler;

{$MODE Delphi}

interface

uses
  jwawindows, Windows, Classes, SysUtils, syncobjs, GuiSafeCriticalSection,
  disassembler, cefuncproc, newkernelhandler,debuggertypedefinitions, frmTracerUnit,
  DebuggerInterfaceAPIWrapper, LuaHandler, lua, lauxlib, lualib, win32proc;

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

    {$ifdef cpu32}
    setInt1Back: boolean;
    Int1SetBackBP: PBreakpoint;
    {$endif}

    singlestepping: boolean;

    //break and trace:
    isTracing: boolean;
    tracecount: integer;
    traceWindow: TfrmTracer;
    traceQuitCondition: string;
    //------------------

    WaitingToContinue: boolean; //set to true when it's waiting for the user to continue

    DebugEventString: string; //for outputdebugstring event
    secondcreateprocessdebugevent: boolean;

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
    procedure clearDebugRegisters;
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

uses foundcodeunit, DebugHelper, MemoryBrowserFormUnit, frmThreadlistunit,
     KernelDebuggerInterface, frmDebugEventsUnit, formdebugstringsunit, symbolhandler;

procedure TDebugThreadHandler.AddDebugEventString;
begin
  if formdebugstrings<>nil then
    formdebugstrings.ListBox1.Items.add(DebugEventString);
end;

procedure TDebugThreadHandler.VisualizeBreak;
begin
  MemoryBrowser.lastdebugcontext:=context^;

  if lua_onBreakpoint(context)=false then //no lua script or it returned 0
    MemoryBrowser.UpdateDebugContext(self.Handle, self.ThreadId);
end;

procedure TDebugThreadHandler.fillContext;
var
  i: integer;
begin
  if handle<>0 then
  begin
    context.ContextFlags := CONTEXT_ALL or CONTEXT_EXTENDED_REGISTERS;
    outputdebugstring(pchar(format('GetThreadContext(%x, %x, %p)',[threadid, handle,@context])));


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
  outputdebugstring(pchar(format('setThreadContext(%x, %x, %p). dr0=%x dr1=%x dr2=%x dr3=%x dr7=%x',[threadid, handle,@context, context.dr0, context.dr1, context.dr2, context.dr3, context.dr7])));

  if handle<>0 then
  begin
    context:=self.context;
    context.ContextFlags := CONTEXT_ALL or CONTEXT_EXTENDED_REGISTERS;
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

procedure TDebugThreadHandler.clearDebugRegisters;
begin
  suspend;
  fillContext;
  context.dr0:=0;
  context.dr1:=0;
  context.dr2:=0;
  context.dr3:=0;
  context.dr6:=0;
  context.dr7:=0;
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
  if WaitingToContinue then
  begin
    self.continueOption:=continueOption;
    onContinueEvent.SetEvent;
  end;

end;

procedure TDebugThreadHandler.ContinueFromBreakpoint(bp: PBreakpoint; continueoption: TContinueOption);
{
Continues the current thread from a debug event. Handles int3 breakpioints as well
BP can be nil if it's a single step breakpoint

}
var oldprotect,bw: dword;
begin
  context.EFlags:=eflags_setTF(context.EFlags,0);

  try
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
      end
      else
      begin


  {$ifdef cpu32}
        //----XP HACK----
        if (WindowsVersion=wvXP) then
        begin
          if not (CurrentDebuggerInterface is TKernelDebugInterface) then
          begin

            if (bp.breakpointTrigger=bptExecute) and (not bp.markedfordeletion) then //if windows xp, and it is a hw bp, and it's an execute hw bp, and it's not marked for deletion, only THEN set the bp back
            begin
              setContext; //apply changes made by the user
              TdebuggerThread(debuggerthread).UnsetBreakpoint(bp);

              setInt1Back:=true;
              context.EFlags:=eflags_setTF(context.EFlags,1); //set the trap flag so it'll break on next instruction
              Int1SetBackBP:=bp;
            end;
          end;
        end;
       {$endif}
      end;



      if (not singlestepping) and ((bp.ThreadID<>0) and (bp.threadid<>self.ThreadId)) then
      begin
        //not singlestepping and this breakpoint isn't set to break for this thread, so:

        context.EFlags:=eflags_setRF(context.EFlags,1);//don't break on the current instruction

        exit; //and exit
      end;
    end;


    case continueoption of
      co_run, co_runtill:
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
  finally
    {$ifdef cpu32}
    if not (CurrentDebuggerInterface is TKernelDebugInterface) then
    begin
      if setInt1Back then
      begin
        eflags_setTF(context.Eflags,1);
        eflags_setRF(context.Eflags,0);
      end;
    end;
    {$endif}

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
        OutputDebugString('CheckIfConditionIsMet=true');
        ContinueFromBreakpoint(nil, co_run);
        isTracing:=false;
        exit;
      end;
    end;


    ContinueFromBreakpoint(nil, co_stepinto);
  end
  else
  begin
    outputdebugstring('tracecount=0');

    ContinueFromBreakpoint(nil, co_run);
    isTracing:=false;
  end;
end;

procedure TDebugThreadHandler.HandleBreak(bp: PBreakpoint);
begin
  Outputdebugstring('HandleBreak()');
  //go to sleep and wait for an event that wakes it up. No need to worry about deleted breakpoints, since the cleanup will not be called untill this routine exits
  onContinueEvent.ResetEvent;


  TDebuggerthread(debuggerthread).synchronize(TDebuggerthread(debuggerthread), VisualizeBreak);

  WaitingToContinue:=true;
  Outputdebugstring('updated gui');
  onContinueEvent.WaitFor(infinite);
  Outputdebugstring('returned from gui');
  WaitingToContinue:=false;

  continueFromBreakpoint(bp, continueOption);
end;

function TDebugThreadHandler.singleStep(var dwContinueStatus: dword): boolean;
var
  {$ifdef cpu32}
  hasSetInt1Back: boolean;
  {$endif}
  hasSetInt3Back: boolean;
  oldprotect, bw: dword;
begin
  OutputDebugString('Handling as a single step event');
  result:=true;

  {$ifdef cpu32}
  if not (CurrentDebuggerInterface is TKernelDebugInterface) then
  begin
    if setint1back then
    begin
      //set the breakpoint back
      TdebuggerThread(debuggerthread).SetBreakpoint(Int1SetBackBP);
      setInt1Back:=false;
      hasSetInt1Back:=true;
      dwContinueStatus:=DBG_CONTINUE;
    end
    else
      hasSetInt1Back:=false;
  end;

  {$endif}


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
    if (not (hasSetInt3Back {$ifdef cpu32} or hasSetInt1Back{$endif})) then
    begin
      OutputDebugString('Not handled');
      dwContinuestatus:=DBG_EXCEPTION_NOT_HANDLED; //if it wasn't a int3 set back or not expected single step, then raise an error
    end;
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
        if bp.conditonalbreakpoint.easymode then script:='return ('+script+')';
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

  bpp,bpp2: PBreakpoint;

  active: boolean;
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

        active:=bpp^.active;

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

    if bpp.breakpointMethod=bpmInt3 then //if it's a software breakpoint adjust eip to go back by 1
      dec(context.{$ifdef cpu64}rip{$else}eip{$endif});

    if (not active) or (not CheckIfConditionIsMet(bpp)) then
    begin
      OutputDebugString('bp was disabled or Condition was not met');

      continueFromBreakpoint(bpp, co_run);
      dwContinueStatus:=DBG_CONTINUE;
      Result:=true;
      exit;
    end;


    case bpp.breakpointAction of
      bo_Break:
      begin
        //check if there is a step over breakpoint and remove it
        breakpointCS.enter;
        for i:=0 to breakpointlist.count-1 do
        begin
          bpp2:=PBreakpoint(breakpointlist.Items[i]);
          if (bpp2.active) and (bpp2.StepOverBp) and (bpp2.markedfordeletion=false) then
            TdebuggerThread(debuggerthread).RemoveBreakpoint(bpp2);
        end;
        breakpointCS.leave;

        HandleBreak(bpp); //cause break in memory browser at address
      end;

      bo_BreakAndTrace:
      begin
        //remove the breakpoint and start tracing this thread X times
        if not isTracing then //don't handle it if already tracing
        begin
          isTracing:=true;
          tracecount:=bpp.TraceCount;
          traceWindow:=bpp.frmTracer;
          if bpp.traceendcondition<>nil then
            traceQuitCondition:=bpp.traceendcondition
          else
            traceQuitCondition:='';

          TdebuggerThread(debuggerthread).RemoveBreakpoint(bpp);
        end;

        handleTrace;
      end;


      bo_ChangeRegister:
      begin
        //modify accordingly
        outputdebugstring('Handling bo_ChangeRegister breakpoint');

        ModifyRegisters(bpp);

        //and
        continueFromBreakpoint(bpp, co_run); //just continue running
      end;

      bo_FindCode:
      begin
        outputdebugstring('Save registers and continue');
        if bpp.active then
        begin
          fcd:=bpp.FoundcodeDialog;
          fcd.usesdebugregs:=bpp.breakpointMethod=bpmDebugRegister;

          TDebuggerthread(debuggerthread).Synchronize(TDebuggerthread(debuggerthread), fcd.AddRecord);
        end;

        //nothing special is needed to continue
      end;

      bo_FindWhatCodeAccesses:
      begin
        TDebuggerthread(debuggerthread).Synchronize(TDebuggerthread(debuggerthread), bpp.frmchangedaddresses.AddRecord);
        continueFromBreakpoint(bpp, co_run); //just continue running
      end;


    end;

    dwContinueStatus:=DBG_CONTINUE;
  end else
  begin
    OutputDebugString('Unexpected breakpoint');
    if (CurrentDebuggerInterface.name='Windows Debugger') then
    begin
      onAttachEvent.SetEvent;

      if TDebuggerthread(debuggerthread).InitialBreakpointTriggered then
        dwContinueStatus:=DBG_EXCEPTION_NOT_HANDLED
      else
      begin
        dwContinueStatus:=DBG_CONTINUE;
        TDebuggerthread(debuggerthread).InitialBreakpointTriggered:=true;
      end;
    end
    else dwContinueStatus:=DBG_EXCEPTION_NOT_HANDLED; //not an expected breakpoint
  end;

  Result := True;
end;

function TDebugThreadHandler.HandleExceptionDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
var
  exceptionAddress: ptrUint;
begin


  OutputDebugString('HandleExceptionDebugEvent:'+inttohex(debugEvent.Exception.ExceptionRecord.ExceptionCode,8));
  exceptionAddress := ptrUint(debugEvent.Exception.ExceptionRecord.ExceptionAddress);



  case debugEvent.Exception.ExceptionRecord.ExceptionCode of
    EXCEPTION_BREAKPOINT, STATUS_WX86_BREAKPOINT:
    begin
      OutputDebugString('EXCEPTION_BREAKPOINT');

      //if this is the first breakpoint exception check if it needs to tset the entry point bp

      if TDebuggerThread(debuggerthread).NeedsToSetEntryPointBreakpoint then
      begin
        OutputDebugString('Calling SetEntryPointBreakpoint');
        TDebuggerthread(debuggerthread).Synchronize(TDebuggerthread(debuggerthread), TDebuggerthread(debuggerthread).SetEntryPointBreakpoint);
      end;




      Result := DispatchBreakpoint(context.{$ifdef cpu64}Rip-1{$else}eip-1{$endif}, dwContinueStatus);
      context.dr6:=0;
      setContext;
    end;

    EXCEPTION_SINGLE_STEP, STATUS_WX86_SINGLE_STEP:
    begin
      OutputDebugString('EXCEPTION_SINGLE_STEP. Dr6='+inttohex(context.dr6,8));

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
  handle    := OpenThread(THREAD_ALL_ACCESS, false, threadid ); //debugevent.CreateThread.hThread;

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

  if not secondcreateprocessdebugevent then
  begin

    handle    := debugevent.CreateProcessInfo.hThread;

    processid := debugevent.dwProcessId;
    threadid  := debugevent.dwThreadId;



    if ProcessHandler.processid<>debugevent.dwProcessId then
    begin
      ProcessHandler.ProcessHandle := debugEvent.CreateProcessInfo.hProcess;
      ProcessHandler.processid     := debugEvent.dwProcessId;

      Open_Process;
      symhandler.reinitialize;
    end;

    if (CurrentDebuggerInterface.name<>'Windows Debugger') then
      onAttachEvent.SetEvent;

    secondcreateprocessdebugevent:=true;
  end;
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
var m: string;
    mw: widestring;
    x: pchar;
    xw: pwidechar absolute x;
    br: dword;

    p: pointer;
begin
  outputdebugstring('LoadDLLDebugEvent');

  getmem(x,512);
  br:=0;

  m:='';
  p:=nil;
  readprocessmemory(processhandle, debugEvent.LoadDll.lpImageName, @p, processhandler.pointersize, br);
  if br>0 then
  begin
    br:=0;
    readprocessmemory(processhandle, p, x, 512, br);
    if br>0 then
    begin
      x[511]:=#0;
      x[510]:=#0;

      if debugEvent.LoadDll.fUnicode<>0 then
      begin
        mw:=xw;
        m:=mw;
      end
      else
        m:=x;

      if LUA_functioncall('debugger_onModuleLoad',[m, ptruint(debugevent.LoadDll.lpBaseOfDll)])=1 then
      begin
        //do a break
        HandleBreak(nil);

      end;

    end;
  end;

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

  OutputDebugString('Returned from HandleDebugEvent');



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

    frmDebugEvents.lbDebugEvents.TopIndex:=frmDebugEvents.lbDebugEvents.items.count-1
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

