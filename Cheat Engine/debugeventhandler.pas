unit debugeventhandler;

//handles the debug events

{$MODE Delphi}

interface

uses

  {$ifdef windows}
  jwawindows, Windows, win32proc,
  {$endif}
  Classes, SysUtils, syncobjs, GuiSafeCriticalSection,
  disassembler, CEFuncProc, newkernelhandler,debuggertypedefinitions, frmTracerUnit,
  DebuggerInterfaceAPIWrapper, lua, lauxlib, lualib,
  tracerIgnore, BreakpointTypeDef, LCLProc, contexthandler {$ifdef darwin}
  ,macport, macportdefines
  {$endif}  ;

type
  TContextFields=(cfAll,cfDebug, cfRegisters, cfFloat);

  TDebugThreadHandler=class;
  THandleBreakEvent=function(sender: TDebugThreadHandler; bp: PBreakpoint): boolean of object;

  TDebugEventHandler = class;

  TDebugThreadHandler = class
  private
    debuggerthread: TObject;
    debuggerCS: TGuiSafeCriticalSection;

    onAttachEvent: TEvent;
    onContinueEvent: TEvent;
    realcontextpointer: pointer;
    threadlist: TList;
    breakpointList: TList;
    continueOption: TContinueOption;
    continueHandled: boolean;

    setInt3Back: boolean;
    Int3setbackAddress: ptrUint;
    Int3SetBackBP: PBreakpoint;


    expectedUndefinedBreakpoint: ptruint; //ARM: When a breakpoint happens with this address handle it instead of ignoring it
    setInt1Back: boolean;
    Int1SetBackBP: PBreakpoint;

    singlestepping: boolean;

    isBranchMapping: boolean;
    branchMappingDisabled: qword;

    //break and trace:
    isTracing: boolean;
    tracecount: integer;
    traceWindow: TfrmTracer;
    traceQuitCondition: string;
    traceStepOver: boolean; //perhaps also trace branches ?
    traceStepOverRep: boolean;
    traceLastInstructionWasRep: boolean; //set by the addRecord function of the tracerform when adding.
    traceNoSystem: boolean;
    traceStayInsideModule: boolean;
    traceStartmodulebase: ptruint;
    traceStartmodulesize: dword;
    //------------------

    unhandledException: boolean;
    unhandledExceptionCode: dword;

    WaitingToContinue: boolean; //set to true when it's waiting for the user to continue

    DebugEventString: string; //for outputdebugstring event
    secondcreateprocessdebugevent: boolean;

    temporaryDisabledExceptionBreakpoints: Tlist;
    breakAddress: ptruint;


    currentBP: PBreakpoint;
    dbvm_currentCR3: qword;

    fOnHandleBreakAsync: THandleBreakEvent;

    lastContext: PContext; //for network contexts only


    hasiptlog: boolean;
    lastiptlog: pointer;
    lastiptlogsize: integer;

    function CheckIfConditionIsMet(bp: PBreakpoint; script: string=''): boolean;
    function InNoBreakList: boolean;

    function HandleAccessViolationDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
    function HandleExceptionDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
    function CreateThreadDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
    function CreateProcessDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
    function ExitThreadDebugEvent(debugevent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
    function ExitProcessDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
    function LoadDLLDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
    function UnloadDLLDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
    function OutputDebugStringEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
    function RipEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
    function HandleUnknownEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;

    function DispatchBreakpoint(address: uint_ptr; debugreg: integer; var dwContinueStatus: dword): boolean;
    function singleStep(var dwContinueStatus: dword): boolean;

    procedure ModifyRegisters(bp: PBreakpoint);
    procedure TraceWindowAddRecord;
    procedure removeAllTracerStepOverBreakpoints;
    procedure handleTrace;
    procedure mapBranch;
    procedure HandleBreak(bp: PBreakpoint; var dwContinueStatus: dword);
    procedure ContinueFromBreakpoint(bp: PBreakpoint; continueoption: TContinueOption; isTracerStepOver: boolean=false);
    function EnableOriginalBreakpointAfterThisBreakpointForThisThread(bp: Pbreakpoint; OriginalBreakpoint: PBreakpoint): boolean;

    procedure foundCodeDialog_AddRecord;
    procedure frmchangedaddresses_AddRecord;

    //sync functions
    procedure visualizeBreak;
    procedure AddDebugEventString;

    function changedcontext: boolean; //for network handlers only. returns true if the context was changed from last getContext

  public
    isHandled: boolean; //set to true if this thread is the current debug target
    needstocleanup: boolean;
    ProcessId: dword;
    ThreadId:  dword;
    handle: THandle;

    DebugRegistersUsedByCE: byte; //mask containing the bits for each DR used
    context: PContext;  //pointer to the context (can be x86, arm, of arm64, or whatever I add later)
    contexthandler: TContextInfo;
    {$ifdef windows}
    _contextsize: integer; //debug
    {$endif}

    fissuspended: boolean;

    procedure UpdateMemoryBrowserContext;
    procedure StartBranchMap;
    procedure StopBranchMap;
    procedure TracerQuit;
    procedure suspend;
    procedure resume;
    procedure fillContext;
    procedure setContext(fields: TContextFields=cfall);
    procedure breakThread;
    procedure clearDebugRegisters;
    procedure continueDebugging(continueOption: TContinueOption; handled: boolean=true);

    {$IFDEF WINDOWS}
    function getLastIPTLog(out log: pointer; out size: integer): boolean;
    {$ENDIF}


    constructor Create(debuggerthread: TObject; attachEvent: Tevent; continueEvent: Tevent; breakpointlist: TList; threadlist: Tlist; debuggerCS: TGuiSafeCriticalSection);
    destructor destroy; override;

    property isSingleStepping: boolean read singlestepping;
    property isWaitingToContinue: boolean read WaitingToContinue;
    property isUnhandledException: boolean read unhandledException;
    property lastUnhandledExceptionCode: dword read unhandledExceptionCode;
    property OnHandleBreakAsync: THandleBreakEvent read fOnHandleBreakAsync write fOnHandleBreakAsync;
    property issuspended: boolean read fissuspended;
  end;

  TDebugEventHandler = class
  private
    debuggerthread: TObject;
    debuggerCS: TGuiSafeCriticalSection;
    fOnAttachEvent: TEvent;
    fOnContinueEvent: TEvent;
    breakpointlist: TList;
    threadlist: TList;

    currentdebugEvent: TDEBUGEVENT;
    procedure updatethreadlist;
    procedure UpdateDebugEventWindow;
  public
    function HandleDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
    constructor Create(debuggerthread: TObject; OnAttachEvent: TEvent; OnContinueEvent: Tevent; breakpointlist: TList; threadlist: Tlist; debuggercs: TGuiSafeCriticalSection);
  end;

implementation

uses foundcodeunit, DebugHelper, MemoryBrowserFormUnit, frmThreadlistunit,
     WindowsDebugger, VEHDebugger, KernelDebuggerInterface, NetworkDebuggerInterface,
     frmDebugEventsUnit, formdebugstringsunit, symbolhandler,
     networkInterface, networkInterfaceApi, ProcessHandlerUnit, globals,
     UnexpectedExceptionsHelper, frmcodefilterunit, frmBranchMapperUnit, LuaHandler,
     LazLogger, Dialogs, vmxfunctions, debuggerinterface, DBVMDebuggerInterface,
     formChangedAddresses, iptnative;

resourcestring
  rsDebugHandleAccessViolationDebugEventNow = 'Debug HandleAccessViolationDebugEvent now';
  rsSpecialCase = 'Special case';

procedure TDebugThreadHandler.frmchangedaddresses_AddRecord;
var
  address: ptruint;
  haserror: boolean;
  e: TaddressEntry;

  s: string;

  f: Tfrmchangedaddresses;
begin
  TDebuggerthread(debuggerthread).execlocation:=44;
  {$ifdef darwin}
  outputdebugstring('frmchangedaddresses_AddRecord: Evaluating '+f.equation);
  {$endif}

  f:=currentbp^.frmchangedaddresses;
  address:=symhandler.getAddressFromName(f.equation, false, haserror, context);

  if not haserror then
  begin
    e:=nil;
    {$ifdef cpu64}
    if not processhandler.is64Bit then
      address:=address and $ffffffff;
    {$endif}


    f.addresslistCS.Enter;
    try
      if f.addresslist.GetData(address, e) then
      begin
        inc(e.count);
        e.changed:=true; //the gui sets this to false after reading/parsing the value
        exit;
      end;

      //still here so a new address
      s:=inttohex(address,8);
      if (f.foundcodedialog=nil) or (f.foundCount<8) then
      begin
        e:=TAddressEntry.create(f);
        e.context:=context; //(duplicates the context so it's ok if this thread gets killed)
        e.address:=address;
        e.count:=1;
        e.changed:=true;
        e.savestack;
        f.addresslist.Add(address,e);
      end;


    finally
      f.addresslistCS.Leave;
    end;

    if e<>nil then
    begin


      {$IFDEF WINDOWS}
      if systemSupportsIntelPT and useintelptfordebug and inteliptlogfindwhatroutines then
        hasiptlog:=TDebuggerthread(debuggerthread).getLastIPT(lastiptlog, lastiptlogsize);


      if hasiptlog then
      begin
        e.ipt.log:=getmem(lastiptlogsize);
        e.ipt.size:=lastiptlogsize;
        CopyMemory(e.ipt.log, lastiptlog, lastiptlogsize);
      end
      else
        e.ipt.log:=nil;
      {$ENDIF}
      f.newRecord:=e;

      TThread.Synchronize(TThread.CurrentThread, f.AddRecord);
    end;
  end;
end;

procedure TDebugThreadHandler.foundCodeDialog_AddRecord;
var
  address, address2: ptruint;
  desc: string;
  d: TDisassembler;
  hasAddress: boolean;

  cr: FoundcodeUnit.Tcoderecord;
begin
  if (((currentbp.breakpointMethod=bpmException) and not currentbp.markedfordeletion) or currentBP.active) and (currentbp.FoundcodeDialog<>nil) then  //it could have been deactivated
  begin
    address:=contexthandler.InstructionPointerRegister^.getValue(context);
    if processhandler.SystemArchitecture=archX86 then
    begin
      if (currentBP.breakpointMethod=bpmDebugRegister) or (currentBP.breakpointMethod=bpmException) then //find out the previous opcode
      begin
        address2:=address;
        d:=TDisassembler.Create;
        d.disassemble(address2,desc);
        if copy(d.LastDisassembleData.opcode,1,3)<>'REP' then
          address:=previousopcode(address,d);

        freeandnil(d);
      end;
    end;


    //check if this address is in the list, and if not, add it (sync), else increase the counter
    currentBP^.FoundcodeDialog.seenAddressListCS.Enter;

    hasAddress:=currentBP^.FoundcodeDialog.seenAddressList.GetData(address, cr);
    if hasAddress then
      cr.hitcount:=cr.hitcount+1;

    currentBP^.FoundcodeDialog.seenAddressListCS.Leave;

    //not in the list, add it:
    if hasAddress=false then
    begin
      {$IFDEF WINDOWS}
      if systemSupportsIntelPT and useintelptfordebug and inteliptlogfindwhatroutines then
        hasiptlog:=TDebuggerthread(debuggerthread).getLastIPT(lastiptlog, lastiptlogsize);
      {$ENDIF}


      currentBP^.FoundcodeDialog.addRecord_Address:=address;
      if hasiptlog then
      begin
        currentBP^.FoundcodeDialog.iptlog:=lastiptlog;
        currentBP^.FoundcodeDialog.iptlogsize:=lastiptlogsize;
      end
      else
        currentBP^.FoundcodeDialog.iptlog:=nil;

      TDebuggerthread(debuggerthread).Synchronize(TThread.CurrentThread,currentBP^.FoundcodeDialog.AddRecord);
    end;
  end;
end;

procedure TDebugThreadHandler.AddDebugEventString;
begin
  TDebuggerthread(debuggerthread).execlocation:=42;

  if formdebugstrings<>nil then
  begin
    formdebugstrings.ListBox1.Items.add(DebugEventString);
    if formdebugstrings.ListBox1.count>10000 then
      formdebugstrings.ListBox1.Items.Delete(0);
  end;
end;

procedure TDebugThreadHandler.UpdateMemoryBrowserContext;
begin
  if MemoryBrowser.Context<>nil then
    freememandnil(memorybrowser.context);

  MemoryBrowser.Context:=contexthandler.getCopy(context);
end;

procedure TDebugThreadHandler.VisualizeBreak;
begin
  WaitingToContinue:=true;


 // Outputdebugstring('HandleBreak()');

  onContinueEvent.ResetEvent;

  TDebuggerthread(debuggerthread).execlocation:=41;
  UpdateMemoryBrowserContext;

  TDebuggerthread(debuggerthread).execlocation:=411;


  try
    if (currentbp<>nil) and (assigned(currentbp.OnBreakpoint)) then
      WaitingToContinue:=currentbp.OnBreakpoint(currentbp, context)
    else
      WaitingToContinue:=not lua_onBreakpoint(Self.ThreadId, context);

  except
    on e:exception do
    begin
      DebugLn('Exception '+e.Message);

      showmessage('Debugger error while handling lua callbacks:'+e.Message);
      DumpExceptionBackTrace;
    end;
  end;

  TDebuggerthread(debuggerthread).execlocation:=412;

  if WaitingToContinue and (TDebuggerthread(debuggerthread).CurrentThread<>nil) then //no lua script or it returned 0, or it DID continue and returned 0...
  begin
    if currentdebuggerinterface is TDBVMDebugInterface then
      memorybrowser.cr3:=dbvm_currentCR3;


    TDebuggerthread(debuggerthread).execlocation:=413;
    MemoryBrowser.UpdateDebugContext(self.Handle, self.ThreadId, true, TDebuggerthread(debuggerthread));
  end;
  TDebuggerthread(debuggerthread).execlocation:=414;

end;

function TDebugThreadHandler.changedcontext: boolean;
begin
  result:=contexthandler.isDifferent(lastcontext, context);
end;

procedure TDebugThreadHandler.fillContext;
var
  i: integer;
  c: pointer;

  pb: PByteArray;
begin

  if (handle<>0) or ((currentdebuggerinterface.controlsTheThreadList=false) and ishandled) then
  begin
    if (handle<>0) and (not ishandled) and (not fissuspended) then
    begin
      exit;
    end;

    debuggercs.enter;

    if processhandler.SystemArchitecture=archArm then
    begin

      if processhandler.is64Bit then
      begin
        {$ifdef darwin}
        if processhandler.isNetwork=false then
          contexthandler.setcontextflags(context, $ffffffff);

        {$endif}
        DebuggerInterfaceAPIWrapper.GetThreadContextArm64(handle, PARM64CONTEXT(context)^, ishandled)
      end
      else
        DebuggerInterfaceAPIWrapper.GetThreadContextArm(handle, PARMCONTEXT(context)^, ishandled);
    end
    else
    begin
      contexthandler.setcontextflags(context, CONTEXT_ALL or CONTEXT_EXTENDED_REGISTERS);

      if not DebuggerInterfaceAPIWrapper.getthreadcontext(handle, context^,  isHandled) then
      begin
        i := getlasterror;
        outputdebugstring(PChar('getthreadcontext error:' + IntToStr(getlasterror)));
      end;


    end;

    if (CurrentDebuggerInterface is TNetworkDebuggerInterface) then
    begin
      if lastcontext<>nil then
        FreeMemAndNil(lastcontext);

      lastContext:=contexthandler.getCopy(context);
    end;

    debuggercs.leave;
  end
  else
    outputdebugstring('fillContext: handle=0');


end;

procedure TDebugThreadHandler.setContext(fields: TContextFields=cfAll);
var
  i: integer;
begin
  if (handle<>0) and (not ishandled) and (not fissuspended) then
  begin
    exit;
  end;

  if processhandler.SystemArchitecture=archArm then
  begin
    outputdebugstring('TDebugThreadHandler.setThreadContext() for ARM');
    if (handle<>0) or ((currentdebuggerinterface.controlsTheThreadList=false) and ishandled) then
    begin
      debuggercs.enter;
      if processhandler.is64Bit then
      begin
        {$ifdef darwin}
        PARM64CONTEXT(context)^.ContextFlags:=0;

        if fields in [cfAll, cfDebug] then
          PARM64CONTEXT(context)^.ContextFlags:=PARM64CONTEXT(context)^.ContextFlags or 1;
        {$endif}


        if not DebuggerInterfaceAPIWrapper.SetThreadContextArm64(self.handle, PARM64CONTEXT(context)^, isHandled) then
        begin
          outputdebugstring('setthreadcontextarm64 failed');
        end;

      end
      else
      begin
        if not DebuggerInterfaceAPIWrapper.SetThreadContextArm(self.handle, PARMCONTEXT(context)^, isHandled) then
        begin

          outputdebugstring('setthreadcontextarm failed');
        end;
      end;
      debuggercs.leave;
    end;
  end
  else
  begin
    outputdebugstring(pchar(format('setThreadContext(%x, %x, %p). dr0=%x dr1=%x dr2=%x dr3=%x dr7=%x',[threadid, handle,context, context^.dr0, context^.dr1, context^.dr2, context^.dr3, context^.dr7])));


    if (handle<>0) or ((currentdebuggerinterface.controlsTheThreadList=false) and ishandled) then
    begin
      debuggercs.enter;
      try



        {$ifdef windows}
        fields:=cfall; //just for vehdebug

        //experiment:
       // context.Dr7:=context.Dr7 or (1 shl 8); //DR7 LE
        //context.Dr7:=context.Dr7 or (1 shl 9); //DR7 GE
        //context.DebugControl:=qword($ffffffffffffffff);
        {$endif};


        case fields of
          cfAll: context^.ContextFlags := CONTEXT_ALL or CONTEXT_EXTENDED_REGISTERS;
          cfDebug: context^.ContextFlags := CONTEXT_DEBUG_REGISTERS;
          cfFloat: context^.ContextFlags := CONTEXT_FLOATING_POINT or CONTEXT_EXTENDED_REGISTERS;
          cfRegisters: context^.ContextFlags := CONTEXT_INTEGER or CONTEXT_CONTROL or CONTEXT_SEGMENTS;
        end;

        //context.dr7:=context.dr7 or $300;


        if not DebuggerInterfaceAPIWrapper.setthreadcontext(self.handle, PCONTEXT(context)^, isHandled) then
        begin
          i := getlasterror;
          outputdebugstring(PChar('setthreadcontext error:' + IntToStr(getlasterror)));
        end;
      finally
        debuggercs.leave;
      end;

    end else outputdebugstring('fillContext: handle=0');
  end;
end;

procedure TDebugThreadHandler.suspend;
begin
  if handle<>0 then
  begin
    suspendthread(handle);
    fissuspended:=true;
  end;


end;

procedure TDebugThreadHandler.resume;
begin
  if handle<>0 then
  begin
    resumethread(handle);
    fissuspended:=false;
  end;
end;

procedure TDebugThreadHandler.breakThread;
begin
  if dbcCanUseInt1BasedBreakpoints in CurrentDebuggerInterface.DebuggerCapabilities then
  begin
    debuggercs.enter;
    suspend;

    fillContext;
    context^.eflags:=eflags_setTF(context^.eflags,1);
    SingleStepping:=true;

    setContext;
    resume;
    debuggercs.leave;
  end
  else
    raise exception.create('The current debuggerinterface does not support int1 based breakpoints');
end;

procedure TDebugThreadHandler.clearDebugRegisters;
begin
  OutputDebugString('clearDebugRegisters');
  if CurrentDebuggerInterface.usesDebugRegisters then
  begin
    debuggerCS.enter;
    suspend;
    fillContext;
    context^.dr0:=0;
    context^.dr1:=0;
    context^.dr2:=0;
    context^.dr3:=0;
    context^.dr6:=0;
    context^.dr7:=0;
    SingleStepping:=true;

    setContext(cfDebug);
    resume;
    debuggercs.leave;
  end;
end;

function booltoint(b: boolean):integer; inline;
begin
  if b then result:=1 else result:=0;
end;

procedure TDebugThreadHandler.ModifyRegisters(bp: PBreakpoint);
var
  fplist: PRegisterModificationFloatList;
  i,j: integer;
  b: byte;
  n: pointer;
begin
  TDebuggerthread(debuggerthread).execlocation:=36;
  if bp.changereg.change_af then context^.EFlags:=eflags_setAF(context^.Eflags, booltoint(bp.changereg.new_af));
  if bp.changereg.change_cf then context^.EFlags:=eflags_setCF(context^.Eflags, booltoint(bp.changereg.new_cf));
  if bp.changereg.change_of then context^.EFlags:=eflags_setOF(context^.Eflags, booltoint(bp.changereg.new_of));
  if bp.changereg.change_pf then context^.EFlags:=eflags_setPF(context^.Eflags, booltoint(bp.changereg.new_pf));
  if bp.changereg.change_sf then context^.EFlags:=eflags_setSF(context^.Eflags, booltoint(bp.changereg.new_sf));
  if bp.changereg.change_zf then context^.EFlags:=eflags_setZF(context^.Eflags, booltoint(bp.changereg.new_zf));

  if bp.changereg.change_eax then context^.{$ifdef cpu64}rax{$else}eax{$endif}:=bp.changereg.new_eax;
  if bp.changereg.change_ebx then context^.{$ifdef cpu64}rbx{$else}ebx{$endif}:=bp.changereg.new_ebx;
  if bp.changereg.change_ecx then context^.{$ifdef cpu64}rcx{$else}ecx{$endif}:=bp.changereg.new_ecx;
  if bp.changereg.change_edx then context^.{$ifdef cpu64}rdx{$else}edx{$endif}:=bp.changereg.new_edx;
  if bp.changereg.change_esi then context^.{$ifdef cpu64}rsi{$else}esi{$endif}:=bp.changereg.new_esi;
  if bp.changereg.change_edi then context^.{$ifdef cpu64}rdi{$else}edi{$endif}:=bp.changereg.new_edi;
  if bp.changereg.change_esp then context^.{$ifdef cpu64}rsp{$else}esp{$endif}:=bp.changereg.new_esp;
  if bp.changereg.change_ebp then context^.{$ifdef cpu64}rbp{$else}ebp{$endif}:=bp.changereg.new_ebp;
  if bp.changereg.change_eip then context^.{$ifdef cpu64}rip{$else}eip{$endif}:=bp.changereg.new_eip;

  {$ifdef cpu64}
  if bp.changereg.change_r8 then context^.r8:=bp.changereg.new_r8;
  if bp.changereg.change_r9 then context^.r9:=bp.changereg.new_r9;
  if bp.changereg.change_r10 then context^.r10:=bp.changereg.new_r10;
  if bp.changereg.change_r11 then context^.r11:=bp.changereg.new_r11;
  if bp.changereg.change_r12 then context^.r12:=bp.changereg.new_r12;
  if bp.changereg.change_r13 then context^.r13:=bp.changereg.new_r13;
  if bp.changereg.change_r14 then context^.r14:=bp.changereg.new_r14;
  if bp.changereg.change_r15 then context^.r15:=bp.changereg.new_r15;
  {$endif}

  if bp.changereg.change_FP<>0 then
  begin
    b:=bp.changereg.change_FP;
    for i:=0 to 7 do
    begin
      if b and (1 shl i)>0 then
      begin
        n:=pointer(ptruint(@bp.changereg.new_FP0)+8*i);
        {$ifdef cpu64}
        copymemory(@context^.FltSave.FloatRegisters[i], n,10);
        {$else}
        copymemory(@context^.ext.FloatRegisters[i], n,10);
        copymemory(@context^.FloatSave.RegisterArea[10*i], n,10);
        {$endif}
      end;
    end;
  end;

  if bp.changereg.change_XMM<>0 then
  begin
    for i:=0 to {$ifdef cpu64}15{$else}7{$endif} do
    begin
      //get the nibble for the xmm register
      b:=(bp.changereg.change_XMM shr (i*4)) and $f;

      if b>0 then //bits are set
      begin
        for j:=0 to 3 do
        begin
          if b and (1 shl j)>0 then //bit is set
          begin
            //change part j of xmm register i
            {$ifdef cpu64}
            PXMMFIELDS(@context^.FltSave.XmmRegisters[i])^[j]:=PXMMFIELDS(ptruint(@bp.changereg.new_XMM0)+16*i)^[j];
            {$else}
            PXMMFIELDS(@context^.ext.XMMRegisters[j])^[j]:=bp.changereg.new_XMM0[j];
            {$endif}
          end;
        end;
      end;
    end;
  end;

end;

procedure TDebugThreadHandler.continueDebugging(continueOption: TContinueOption; handled: boolean=true);
begin
  if WaitingToContinue then
  begin
    self.continueOption:=continueOption;
    self.ContinueHandled:=handled;
    onContinueEvent.SetEvent;
  end;

end;

{$IFDEF WINDOWS}
function TDebugThreadHandler.getLastIPTLog(out log: pointer; out size: integer): boolean;
begin
  result:=false;
  if hasiptlog=false then
    hasiptlog:=TDebuggerthread(debuggerthread).getLastIPT(lastiptlog, lastiptlogsize);

  if hasiptlog then
  begin
    getmem(log, lastiptlogsize);
    size:=lastiptlogsize;
    copymemory(log, lastiptlog,lastiptlogsize);
    result:=true;
  end;
end;
{$ENDIF}

function TDebugThreadHandler.EnableOriginalBreakpointAfterThisBreakpointForThisThread(bp: Pbreakpoint; OriginalBreakpoint: PBreakpoint): boolean;
begin
  result:=true;
  TDebuggerthread(debuggerthread).execlocation:=40;
  debuggercs.enter;
  if bp.active then
  begin
    dec(OriginalBreakpoint.referencecount);
    result:=TdebuggerThread(debuggerthread).SetBreakpoint(Originalbreakpoint, self);
  end;
  debuggercs.leave;
end;

procedure TDebugThreadHandler.ContinueFromBreakpoint(bp: PBreakpoint; continueoption: TContinueOption; isTracerStepOver: boolean=false);
{
Continues the current thread from a debug event. Handles int3 breakpoints as well
BP can be nil if it's a single step breakpoint

}
var oldprotect: dword;
  bw: PtrUInt;
  d: TDisassembler=nil;
  nexteip: ptruint;
  t: string;

  b: PBreakpoint=nil;

  pc: ptruint;
  c: TCEConnection;
  vpe: boolean;

begin
  TDebuggerthread(debuggerthread).execlocation:=39;
  debuggercs.enter;

  if (processhandler.SystemArchitecture=archX86) and CurrentDebuggerInterface.usesDebugRegisters then
    context^.EFlags:=eflags_setTF(context^.EFlags,0);

  {$ifdef darwin}
  if (processhandler.SystemArchitecture=archArm) and processhandler.is64Bit and (setInt1Back=false) then
    parm64context(context)^.debugstate.mdscr_el1:=0;
  {$endif}

  try
    if (bp<>nil) then
    begin


      if (bp.breakpointMethod=bpmInt3) then
      begin
        //bp is set and it's an int3 breakpoint
        vpe:=(SkipVirtualProtectEx=false) and VirtualProtectEx(Processhandle, pointer(bp.address), 1, PAGE_EXECUTE_READWRITE, oldprotect);
        WriteProcessMemory(processhandle, pointer(bp.address), @bp.originalbyte, 1, bw);
        if vpe then
          VirtualProtectEx(Processhandle, pointer(bp.address), 1, oldprotect, oldprotect);

        if (not bp.markedfordeletion) and (not bp.OneTimeOnly) then //if it's not a one time only breakpoint then set it back on next instruction
        begin
          context^.EFlags:=eflags_setTF(context^.EFlags,1); //set the trap flag so it'll break on next instruction
          setInt3Back:=true;
          Int3setbackAddress:=bp.address;
          Int3setBackbp:=bp;

        end;
      end
      else
      begin

        if (CurrentDebuggerInterface is TNetworkDebuggerInterface) and (processhandler.SystemArchitecture=archarm) then
        begin
          //if it is a network breakpoint delete it first. Not needed for x86 but arm can't continue if the breakpoint doesn't get removed (at least my transformer tablet does)
          //it's like windows XP where the RF flag is borked, and the added fun that is also affects read/write watchpoints and arm doesn't do single stepping

          if bp.active then
            TdebuggerThread(debuggerthread).UnsetBreakpoint(bp, nil, ThreadId); //remove the breakpoint just for this thread

          if bp.markedfordeletion=false then
          begin
            setInt1Back:=true;
            Int1SetBackBP:=bp;
            TNetworkDebuggerInterface(CurrentDebuggerInterface).SingleStepNextContinue:=true; //if possible. Not all builds support it and it's bad...
          end;
        end;

{$ifdef cpu32}
        //----XP HACK----
        if (WindowsVersion=wvXP) then
        begin
          if not (CurrentDebuggerInterface is TKernelDebugInterface) then
          begin

            if (bp.breakpointTrigger=bptExecute) and (not bp.markedfordeletion) then //if windows xp, and it is a hw bp, and it's an execute hw bp, and it's not marked for deletion, only THEN set the bp back
            begin
              context^.Dr6:=0;  //unset breakpoint relies on this being 0 of ffff0ff0
              setContext; //apply changes made by the user
              TdebuggerThread(debuggerthread).UnsetBreakpoint(bp);

              setInt1Back:=true;
              context^.EFlags:=eflags_setTF(context^.EFlags,1); //set the trap flag so it'll break on next instruction
              Int1SetBackBP:=bp;
            end;
          end;
        end;
{$endif}
      end;

      if (not singlestepping) and ((bp.ThreadID<>0) and (bp.threadid<>self.ThreadId)) then
      begin
        //not singlestepping and this breakpoint isn't set to break for this thread, so:
        if dbcCanUseInt1BasedBreakpoints in CurrentDebuggerInterface.DebuggerCapabilities then
          context^.EFlags:=eflags_setRF(context^.EFlags,1);//don't break on the current instruction

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
          if dbcCanUseInt1BasedBreakpoints in CurrentDebuggerInterface.DebuggerCapabilities then
            context^.EFlags:=eflags_setRF(context^.EFlags,1);
        end

      end;



      co_stepinto, co_stepover:
      begin
        //single step
        if (CurrentDebuggerInterface is TNetworkDebuggerInterface) then
          TNetworkDebuggerInterface(CurrentDebuggerInterface).SingleStepNextContinue:=true;

        singlestepping:=true;

        if ProcessHandler.SystemArchitecture=archX86 then
        begin
          if (bp=nil) or (bp.breakpointMethod=bpmDebugRegister) then
            if dbcCanUseInt1BasedBreakpoints in CurrentDebuggerInterface.DebuggerCapabilities then
              context^.EFlags:=eflags_setRF(context^.EFlags,1);//don't break on the current instruction
        end; //arm doesn't seem to have a RF flag, so the bp has been disabled with a singlestep next to reenable it

        if continueoption=co_stepinto then
        begin
          {$ifdef darwin}
          if (processhandler.SystemArchitecture=archArm) and processhandler.is64Bit then
          begin
            parm64context(context)^.debugstate.mdscr_el1:=1;
            setContext;
          end
          else
          {$endif}
          if dbcCanUseInt1BasedBreakpoints in CurrentDebuggerInterface.DebuggerCapabilities then
            context^.EFlags:=eflags_setTF(context^.EFlags,1) //set the trap flag
        end
        else
        begin
          //step over
          //check if the current instruction is a call, if not, single step, else set a "run till" breakpoint (that doesn't cancel the stepping)
          d:=TDisassembler.Create;
          nexteip:=context^.{$ifdef cpu64}rip{$else}eip{$endif};
          d.disassemble(nexteip, t);
          if d.LastDisassembleData.iscall or d.LastDisassembleData.isrep then
          begin
            //set an execute breakpoint for this thread only at the next instruction and run till there
            if CurrentDebuggerInterface.usesDebugRegisters then
              setContext;

            b:=TDebuggerthread(debuggerthread).SetOnExecuteBreakpoint(nexteip , false, ThreadId);
            if b<>nil then
            begin
              b.OneTimeOnly:=true;
              b.isTracerStepOver:=isTracerStepOver;
            end;

            if CurrentDebuggerInterface is TDBVMDebugInterface then
              singlestepping:=false; //dbvm checks this var if it should be a single step or not
          end
          else  //if not, single step
          begin
            {$ifdef darwin}
            if (processhandler.SystemArchitecture=archArm) and processhandler.is64Bit then
              parm64context(context)^.debugstate.mdscr_el1:=1
            else
            {$endif}
            if dbcCanUseInt1BasedBreakpoints in CurrentDebuggerInterface.DebuggerCapabilities then
              context^.EFlags:=eflags_setTF(context^.EFlags,1);
          end;

          freeandnil(d);

        end;
      end;

      //the other event types are just setting of one time breakpoints

    end;
  finally
    {$ifdef cpu32}
    if not (CurrentDebuggerInterface is TKernelDebugInterface) then
    begin
      if setInt1Back then
      begin
        eflags_setTF(context^.Eflags,1);
        eflags_setRF(context^.Eflags,0);
      end;
    end;
    {$endif}

    if (bp<>nil) and (not bp.active) then
    begin
      //disable the breakpoint in the current context (in case it got disabled while the breakpoint was being handled)
      if bp.breakpointMethod=bpmDebugRegister then
      begin
        if processhandler.SystemArchitecture=archX86 then
        begin
          context^.Dr6:=0;  //unset breakpoint relies on this being 0 of ffff0ff0 is handled
          setContext(cfDebug);
          TdebuggerThread(debuggerthread).UnsetBreakpoint(bp, context, threadid);
        end;
      end;
    end;

    debuggercs.leave;
  end;
end;

procedure TDebugThreadHandler.mapBranch;
var hasid: boolean;
  b: byte=0;
begin
  if isBranchMapping and (frmBranchMapper<>nil) then
  begin
    frmBranchMapper.mapmrew.beginread;
    hasid:=frmBranchMapper.map.hasID(context^.{$ifdef cpu64}rip{$else}eip{$endif});
    frmBranchMapper.mapmrew.endread;

    if branchMappingDisabled=0 then
      context^.EFlags:=eflags_setTF(context^.EFlags,1);



    if hasid then exit;

    frmBranchMapper.mapmrew.beginwrite;
    if frmBranchMapper.map.hasID(context^.{$ifdef cpu64}rip{$else}eip{$endif})=false then
      frmBranchMapper.map.Add(context^.{$ifdef cpu64}rip{$else}eip{$endif},b);

    frmBranchMapper.mapmrew.endwrite;
  end;
end;

procedure TDebugThreadHandler.StartBranchMap;
begin
  if dbcCanUseInt1BasedBreakpoints in CurrentDebuggerInterface.DebuggerCapabilities then
  begin
    suspend;
    fillContext;
    context^.Dr7:=context^.Dr7 or $300;
    context^.EFlags:=context^.EFlags or EFLAGS_TF;
    setContext;
    isBranchMapping:=true;
    branchMappingDisabled:=0;
    resume;
  end
  else
    raise exception.create('StartBranchMap can not function with this debugger interface');

end;

procedure TDebugThreadHandler.StopBranchMap;
begin
  if dbcCanUseInt1BasedBreakpoints in CurrentDebuggerInterface.DebuggerCapabilities then
  begin
    suspend;
    fillContext;
    context^.Dr7:=context^.Dr7 and (not $300);
    context^.EFlags:=context^.EFlags and (not EFLAGS_TF);
    setContext;
    branchMappingDisabled:=getTickCount64;
    resume;
  end; //fail silently
end;

procedure TDebugThreadHandler.TracerQuit;
begin
  tracewindow:=nil;

  if isTracing then
  begin
    fillContext;
    if dbcCanUseInt1BasedBreakpoints in CurrentDebuggerInterface.DebuggerCapabilities then
      context^.EFlags:=eflags_setTF(context^.EFlags,0); //unset TF
  end;

  TDebuggerthread(debuggerthread).execlocation:=45;
end;

procedure TDebugThreadHandler.TraceWindowAddRecord;
begin
  if traceWindow<>nil then
  begin
    tracewindow.addRecord;
    traceLastInstructionWasRep:=tracewindow.LastDisassembleData.isrep;
  end;
end;

procedure TDebugThreadHandler.removeAllTracerStepOverBreakpoints;
var
  i: integer;
  bpp: PBreakpoint;
begin
  OutputDebugString('Deleting all tracer stepover breakpoints');
  for i := 0 to breakpointlist.Count - 1 do
  begin
    bpp:=breakpointlist[i];
    if bpp^.isTracerStepOver then
       TdebuggerThread(debuggerthread).RemoveBreakpoint(bpp);
  end;
end;

procedure TDebugThreadHandler.handleTrace;
var
  b: PBreakpoint;
  r: ptruint;
  x: PtrUInt;

  ignored: boolean;
begin
  if tracewindow=nil then
  begin
    OutputDebugString('Tracewindow closed. Stopping tracer');
    isTracing:=false;

    ContinueFromBreakpoint(nil, co_run);
    exit;
  end;

  ignored:=false;

  TDebuggerthread(debuggerthread).execlocation:=37;

  if IgnoredModuleListHandler<>nil then
    ignored:=IgnoredModuleListHandler.InIgnoredModuleRange(context^.{$ifdef cpu64}rip{$else}eip{$endif});

  if (not ignored) and traceNoSystem and symhandler.inSystemModule(context^.{$ifdef cpu64}rip{$else}eip{$endif}) then
    ignored:=true;

  if (not ignored) and traceStayInsideModule and (not InRangeQ(context^.{$ifdef cpu64}rip{$else}eip{$endif}, traceStartmodulebase, traceStartmodulebase+traceStartmodulesize)) then
    ignored:=true;


  TDebuggerthread(debuggerthread).execlocation:=371;
  if (tracewindow<>nil) and (not ignored) then
  begin
    TDebuggerthread(debuggerthread).Synchronize(TDebuggerthread(debuggerthread), TraceWindowAddRecord);
    TDebuggerthread(debuggerthread).guiupdate:=true;
  end;

  TDebuggerthread(debuggerthread).execlocation:=372;

  dec(tracecount);
  if tracecount>0 then
  begin
    if traceQuitCondition<>'' then
    begin
      if CheckIfConditionIsMet(nil, 'return '+traceQuitCondition) then
      begin
        TDebuggerthread(debuggerthread).execlocation:=373;
        //quit condition is met
        if tracewindow<>nil then
          TDebuggerthread(debuggerthread).Synchronize(TDebuggerthread(debuggerthread), tracewindow.Finish);

        OutputDebugString('CheckIfConditionIsMet=true. Stopping tracer');
        ContinueFromBreakpoint(nil, co_run);
        isTracing:=false;

        exit;
      end;
    end;

    TDebuggerthread(debuggerthread).execlocation:=374;

    if ignored then
    begin
      TDebuggerthread(debuggerthread).execlocation:=375;
      x:=0;
      r:=0;
      ReadProcessMemory(processhandle, pointer(context^.{$ifdef cpu64}rsp{$else}esp{$endif}), @r, processhandler.pointersize, x);
      if x=processhandler.pointersize then
      begin
        tracewindow.returnfromignore:=true;
        try
          b:=TDebuggerthread(debuggerthread).SetOnExecuteBreakpoint(r , false, ThreadId);
          b.OneTimeOnly:=true;
          TDebuggerthread(debuggerthread).execlocation:=376;
        except
          OutputDebugString('Trace step out set breakpoint error');
          isTracing:=false;
          TDebuggerthread(debuggerthread).Synchronize(TDebuggerthread(debuggerthread), tracewindow.Finish);
        end;
      end
      else
      begin
        //error reading
        OutputDebugString('Trace read stack error');
        isTracing:=false;
        TDebuggerthread(debuggerthread).Synchronize(TDebuggerthread(debuggerthread), tracewindow.Finish);
      end;

      ContinueFromBreakpoint(nil, co_run);
    end
    else
    begin
      TDebuggerthread(debuggerthread).execlocation:=377;
      if tracestepover or (tracestepoverrep and traceLastInstructionWasRep) then
        ContinueFromBreakpoint(nil, co_stepover, true)
      else
        ContinueFromBreakpoint(nil, co_stepinto);
    end;
  end
  else
  begin
    TDebuggerthread(debuggerthread).execlocation:=378;
    outputdebugstring('tracecount=0 stopping tracer');
    if tracewindow<>nil then
    begin
      TDebuggerthread(debuggerthread).execlocation:=379;
      TDebuggerthread(debuggerthread).Synchronize(TDebuggerthread(debuggerthread), tracewindow.Finish);
    end;

    TDebuggerthread(debuggerthread).execlocation:=380;

    ContinueFromBreakpoint(nil, co_run);
    isTracing:=false;
  end;
end;

procedure TDebugThreadHandler.HandleBreak(bp: PBreakpoint; var dwContinueStatus: dword);
var handledByOnHandleBreakAsync: boolean;
begin

  TDebuggerthread(debuggerthread).execlocation:=38;

  handledByOnHandleBreakAsync:=false;
  if assigned(fOnHandleBreakAsync) then
    handledByOnHandleBreakAsync:=fOnHandleBreakAsync(self, bp);


  //synchronize(VisualizeBreak);
  //go to sleep and wait for an event that wakes it up. No need to worry about deleted breakpoints, since the cleanup will not be called until this routine exits
  if handledByOnHandleBreakAsync=false then
  begin
    TDebuggerthread(debuggerthread).synchronize(TDebuggerthread(debuggerthread), VisualizeBreak);

    if WaitingToContinue then
    begin
      //Outputdebugstring('updated gui');
      onContinueEvent.WaitFor(infinite);
      //Outputdebugstring('returned from gui');
    end;
  end;

  if continueHandled then
    dwContinueStatus:=DBG_CONTINUE
  else
    dwContinueStatus:=DBG_EXCEPTION_NOT_HANDLED;

  WaitingToContinue:=false;
  continueFromBreakpoint(bp, continueOption);
end;

function TDebugThreadHandler.singleStep(var dwContinueStatus: dword): boolean;
var
  {$if defined(cpu32) or defined(darwin)}
  hasSetInt1Back: boolean;
  {$endif}
  hasSetInt3Back: boolean;
  oldprotect: dword;
  bw: PtrUInt;
  vpe: boolean;

  connection: TCEConnection;
begin
  TDebuggerthread(debuggerthread).execlocation:=35;
  OutputDebugString('Handling as a single step event');
  result:=true;
  {$if defined(cpu32) or defined(darwin)}
  hasSetInt1Back:=false;
  {$endif}

  if (setint1back) then
  begin
    connection:=getConnection;
    if (connection<>nil) {$ifdef darwin}or ((processhandler.SystemArchitecture=archArm) and (processhandler.is64Bit)) {$endif}then
    begin
      TdebuggerThread(debuggerthread).setBreakpoint(Int1SetBackBP, self);
      setint1back:=false;
      {$ifdef darwin}
      hasSetInt1Back:=true;
      {$endif}
    end;
  end;


  {$if defined(cpu32) or defined(darwin)}

  {$ifdef windows}
  if not (CurrentDebuggerInterface is TKernelDebugInterface) then
  {$endif}
  begin

    if setint1back {$ifdef darwin}and (processhandler.SystemArchitecture=archArm){$endif} then
    begin
      //set the breakpoint back
      TdebuggerThread(debuggerthread).SetBreakpoint(Int1SetBackBP, self);
      setInt1Back:=false;
      hasSetInt1Back:=true;
      dwContinueStatus:=DBG_CONTINUE;
    end;
  end;
  {$endif}



  if setInt3Back then
  begin
    if Int3setBackbp.markedfordeletion=false then
    begin
      vpe:=(SkipVirtualProtectEx=false) and VirtualProtectEx(Processhandle, pointer(Int3setbackAddress), 1, PAGE_EXECUTE_READWRITE, oldprotect);
      WriteProcessMemory(processhandle, pointer(Int3setbackAddress), @int3byte, 1, bw);
      if vpe then
        VirtualProtectEx(Processhandle, pointer(Int3setbackAddress), 1, oldprotect, oldprotect);
    end;

    setInt3Back:=false;
    hasSetInt3Back:=true;
    dwContinueStatus:=DBG_CONTINUE;
  end else hasSetInt3Back:=false;

  if isBranchMapping then
  begin
    MapBranch;
  end
  else
  if isTracing then
  begin
    outputdebugstring('Tracing is true so handle as a trace event');
    handleTrace;
    if isTracing=false then
    begin
      outputdebugstring('Tracing has ended');
      removeAllTracerStepOverBreakpoints;
    end;
  end
  else
  if singlestepping then
  begin
    handlebreak(nil, dwContinueStatus);
  end
  else
  begin
    //no known single step happening

    {$ifdef windows}
    if (CurrentDebuggerInterface is TKernelDebugInterface) then
    begin
      //could be dbvm
      if TKernelDebugInterface(CurrentDebuggerInterface).EventCausedByDBVM then
      begin
        handlebreak(nil, dwContinueStatus);
        exit;
      end;
    end;
    {$endif}

    if (not (hasSetInt3Back {$if defined(cpu32) or defined(darwin)} or hasSetInt1Back{$endif})) then
    begin
      OutputDebugString('Not handled');
      dwContinuestatus:=DBG_EXCEPTION_NOT_HANDLED; //if it wasn't a int3 set back or not expected single step, then raise an error
    end;
  end;
end;

function TDebugThreadHandler.InNoBreakList: boolean;
begin
  result:=CurrentDebuggerInterface.InNoBreakList(threadid);
end;

function TDebugThreadHandler.CheckIfConditionIsMet(bp: PBreakpoint; script: string=''): boolean;
var
  i:integer;
begin
  TDebuggerthread(debuggerthread).execlocation:=14;



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

      result:=CheckIfConditionIsMetContext(self.ThreadId, context, script);
    end;
  end;
end;

function TDebugThreadHandler.DispatchBreakpoint(address: uint_ptr; debugreg: integer; var dwContinueStatus: dword): boolean;
var
  found:  boolean;
  i:      integer;

  c: PContext;

  bpp,bpp2: PBreakpoint;

  active: boolean;
  oldprotect: dword;
  bw: PtrUInt;

  connection: TCEConnection;
  vpe: boolean;
  match: boolean;
  bpa: qword;
begin
  TDebuggerthread(debuggerthread).execlocation:=26;
  outputdebugstring(format('DispatchBreakpoint(%x)',[address]));
  found := False;

  //check if it's an expected breakpoint
  //if not, DBG_EXCEPTION_NOT_HANDLED

  active:=false;
  bpp2:=nil;
  bpp:=nil;

  //
  if (debugreg<0) and (frmCodeFilter<>nil) and frmCodeFilter.handleBreakpoint(address) then  //callfilter
  begin
    dwContinueStatus:=DBG_CONTINUE;
    exit(true);
  end;


  debuggercs.enter;

  for i := 0 to breakpointlist.Count - 1 do
  begin
    bpp:=PBreakpoint(breakpointlist.Items[i]);

    if processhandler.SystemArchitecture=archX86 then
      match:=InRangeX(address, bpp^.address, bpp^.address+bpp^.size-1)
    else
    begin
      if bpp^.breakpointTrigger=bptExecute then
      begin
        bpa:=bpp^.address and $fffffffffffffffe;
        match:=InRangeX(address, bpa, bpa+bpp^.size-1)
      end
      else
        match:=InRangeX(address, bpp^.address, bpp^.address+bpp^.size-1);
    end;

    if match then
    begin
      {$ifdef darwin}
      if bpp^.active then
        OutputDebugString('Checking breakpoint '+i.ToString+'Address='+bpp^.address.ToHexString(16)+' Active=true')
      else
        OutputDebugString('Checking breakpoint '+i.ToString+'Address='+bpp^.address.ToHexString(16)+' Active=false');
      {$endif}
      if (CurrentDebuggerInterface.canReportExactDebugRegisterTrigger) and (debugreg in [0..4]) and (bpp.breakpointMethod=bpmDebugRegister) and (bpp.debugRegister<>debugreg) then
        continue; //this is not the correct breakpoint. Skip it


      found:=true;
      bpp2:=bpp;
      active:=bpp^.active;

      if active and bpp^.OneTimeOnly then //mark for deletion
        TdebuggerThread(debuggerthread).RemoveBreakpoint(bpp);

      if ((bpp.breakpointMethod=bpmException) and (not bpp.markedfordeletion)) or active then
        break;

      //else continue looking for one that IS active and not deleted
    end;
  end;

  debuggercs.leave;


  TDebuggerthread(debuggerthread).execlocation:=27;


  if found then
  begin
    if bpp^.owner<>nil then
      currentbp:=bpp^.owner
    else
      currentbp:=bpp;   //always use the main, the children may get screwed over

    bpp:=bpp2;
    outputdebugstring('Handling breakpoint');

    //to handle a debug register being handled before the single step (since xp sucks and doesn't do rf)
    if setInt3Back then //on a fault this will set the state to as it was expected, on a trap this will set the breakpoint back. Both valid
    begin
      vpe:=(SkipVirtualProtectEx=false) and VirtualProtectEx(Processhandle, pointer(Int3setbackAddress), 1, PAGE_EXECUTE_READWRITE, oldprotect);
      WriteProcessMemory(processhandle, pointer(Int3setbackAddress), @int3byte, 1, bw);
      if vpe then
        VirtualProtectEx(Processhandle, pointer(Int3setbackAddress), 1, oldprotect, oldprotect);

      setInt3Back:=false;
    end;


    if isTracing then
    begin
      outputdebugstring('isTracing');
      handleTrace;
      if istracing=false then
      begin
        outputdebugstring('trace has ended');
        removeAllTracerStepOverBreakpoints;
      end;

      dwContinueStatus:=DBG_CONTINUE;
      Result:=true;
      exit;
    end;

    //outputdebugstring('not tracing');

    if currentBP.isTracerStepOver then
    begin
      outputdebugstring('dangling stepover BP detected');
      TdebuggerThread(debuggerthread).RemoveBreakpoint(bpp); //just being sure

      dwContinueStatus:=DBG_CONTINUE;
      Result:=true;
      exit;
    end;



    if (InNoBreakList) or ((bpp^.OneTimeOnly=false) and (((bpp^.breakpointMethod<>bpmException) and (not active)) or (not CheckIfConditionIsMet(bpp) or (bpp.markedfordeletion) ))) then
    begin
      TDebuggerthread(debuggerthread).execlocation:=28;
      OutputDebugString('bp was disabled or Condition was not met');
      debuggercs.enter;

      if bpp.markedfordeletion then
      begin
        bpp.deletecountdown:=10; //reset
        bpp.active:=false; //this should NEVER be needed, but just to be sure...
      end;

      if bpp.active=false then
      begin
        TdebuggerThread(debuggerthread).UnsetBreakpoint(bpp, context);  //make sure it's disabled
        setcontext(cfDebug);
      end;

      needstocleanup:=true;

      continueFromBreakpoint(bpp, co_run);
      dwContinueStatus:=DBG_CONTINUE;
      Result:=true;

      debuggercs.leave;
      exit;
    end;

    {$ifdef darwin}
    if (processhandler.SystemArchitecture=archArm) and (processhandler.is64Bit) then
    begin
      if bpp^.breakpointTrigger=bptExecute then
        parm64context(context)^.debugstate.bcr[bpp^.debugRegister].bits.enabled:=0
      else
        parm64context(context)^.debugstate.wcr[bpp^.debugRegister].bits.enabled:=0;

      //TdebuggerThread(debuggerthread).UnsetBreakpoint(bpp, nil, ThreadId);
      parm64context(context)^.debugstate.mdscr_el1:=parm64context(context)^.debugstate.mdscr_el1 or 1;   //single step
      setcontext(cfDebug);
      setInt1Back:=true;
      Int1SetBackBP:=bpp;


    end;
    {$endif}


    case bpp.breakpointAction of
      bo_Break:
      begin
        TDebuggerthread(debuggerthread).execlocation:=29;
        //check if there is a step over breakpoint and remove it
        debuggercs.enter;
        for i:=0 to breakpointlist.count-1 do
        begin
          bpp2:=PBreakpoint(breakpointlist.Items[i]);
          if (((bpp2.breakpointMethod=bpmException) and not bpp2.markedfordeletion) or bpp2.active) and (bpp2.StepOverBp) and (bpp2.markedfordeletion=false) then
            TdebuggerThread(debuggerthread).RemoveBreakpoint(bpp2);
        end;

        debuggercs.leave;

        HandleBreak(bpp, dwContinueStatus); //cause break in memory browser at address
      end;

      bo_BreakAndTrace:
      begin
        TDebuggerthread(debuggerthread).execlocation:=30;

        //remove the breakpoint and start tracing this thread X times
        if not isTracing then //don't handle it if already tracing
        begin
          debuggercs.enter;

          if bpp.active then
          begin
            isTracing:=true;
            tracecount:=bpp.TraceCount;
            traceWindow:=bpp.frmTracer;
            traceStepOver:=bpp.tracestepOver;
            traceStepOverRep:=bpp.traceStepOverRep;
            traceNoSystem:=bpp.traceNoSystem;
            traceStayInsideModule:=bpp.traceStayInsideModule;
            traceStartmodulebase:=bpp.traceStartmodulebase;
            traceStartmodulesize:=bpp.traceStartmodulesize;

            if bpp.traceendcondition<>nil then
              traceQuitCondition:=bpp.traceendcondition
            else
              traceQuitCondition:='';
          end;

          debuggercs.leave;
        end;

        TdebuggerThread(debuggerthread).RemoveBreakpoint(bpp); //there can be only one

        if istracing then
          handleTrace;
      end;


      bo_ChangeRegister:
      begin
        TDebuggerthread(debuggerthread).execlocation:=31;
        //modify accordingly
        //outputdebugstring('Handling bo_ChangeRegister breakpoint');

        ModifyRegisters(bpp);

        if CurrentDebuggerInterface is TDBVMDebugInterface then
          setContext;


        //and
        continueFromBreakpoint(bpp, co_run); //just continue running
      end;

      bo_FindCode:
      begin
        TDebuggerthread(debuggerthread).execlocation:=32;
        //outputdebugstring('Save registers and continue');

        if ((bpp.breakpointMethod=bpmException) and (not bpp.markedfordeletion)) or bpp.active then
        begin

          foundCodeDialog_AddRecord;
          //{$ifndef darwin}
          if CurrentDebuggerInterface is TNetworkDebuggerInterface then
          //{$endif}
            continueFromBreakpoint(bpp, co_run);  //explicitly continue from this breakpoint
        end;

        //nothing special is needed to continue
      end;

      bo_FindWhatCodeAccesses:
      begin
        TDebuggerthread(debuggerthread).execlocation:=33;
        frmchangedaddresses_AddRecord;

        {TDebuggerthread(debuggerthread).execlocation:=33;
        TDebuggerthread(debuggerthread).Synchronize(TDebuggerthread(debuggerthread), frmchangedaddresses_AddRecord);
        TDebuggerthread(debuggerthread).guiupdate:=true;  }

        continueFromBreakpoint(bpp, co_run); //just continue running
      end;


    end;

    dwContinueStatus:=DBG_CONTINUE;
  end else
  begin
    OutputDebugString('Not a specified breakpoint');
    TDebuggerthread(debuggerthread).execlocation:=34;
    if (setint1back) and (address<>0) then
    begin
      connection:=getConnection;
      if (connection<>nil) {$ifdef darwin}or ((processhandler.SystemArchitecture=archArm) and (processhandler.is64Bit)) {$endif}then
      begin
        TdebuggerThread(debuggerthread).setBreakpoint(Int1SetBackBP, self);

        setint1back:=false;

        dwContinueStatus:=DBG_CONTINUE;
        result:=true;
        exit;
      end;
    end;


    OutputDebugString('Unexpected breakpoint');
    {$ifdef windows}
    if not (CurrentDebuggerInterface is TKernelDebugInterface) then
    {$endif}
    begin

      dwContinueStatus:=DBG_EXCEPTION_NOT_HANDLED;

      if TDebuggerthread(debuggerthread).InitialBreakpointTriggered=false then
      begin

        {$ifdef windows}
        if (CurrentDebuggerInterface is TVEHDebugInterface) then
        begin
          if (context^.{$ifdef cpu64}Rip{$else}Eip{$endif}=$ffffffce) then
          begin
            dwContinueStatus:=DBG_CONTINUE;
            TDebuggerthread(debuggerthread).InitialBreakpointTriggered:=true;

            result:=false;
            onAttachEvent.SetEvent;
            exit;
          end;
        end
        else
        {$endif}
        begin
          dwContinueStatus:=DBG_CONTINUE;
          TDebuggerthread(debuggerthread).InitialBreakpointTriggered:=true;

          result:=false;
          onAttachEvent.SetEvent;
          exit;
        end;

      end;


    end
    {$ifdef windows}
    else dwContinueStatus:=DBG_EXCEPTION_NOT_HANDLED; //not an expected breakpoint
    {$endif}
  end;


  Result := True;
end;

function TDebugThreadHandler.HandleAccessViolationDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
var address: ptruint;

  bp: PBreakpoint;
  i: integer;
begin
  TDebuggerthread(debuggerthread).execlocation:=15;
  //check if the address that triggered it is in one of the active exception breakpoints and if so make the protection what it should be

  //thing to note:
  //if 0x2000 and 0x3000 are set to readonly, and 0x2fff is written to using a 4 byte write  accesses, then first unrptorect 0x2000 and execute.
  //that will cause the next exception at 0x3000 to trigger

  //solution: Disable ALL protections arround the given address

  //p2:
  //t1 writes address1
  //t1 gets an exception
  //t1 goes in the exception handler, makes it writable and continues with a single step
  //t2 runs and writes to the address
  //t1 runs and triggers the single step bp
  //s: freeze ALL other threads


  //s2:
  //freeze t2 and set it to continue normally, after t1 has been handled, resume t2, causing it to retrigger

  result:=true;
  dwContinueStatus:=DBG_EXCEPTION_NOT_HANDLED;

  if debugevent.Exception.ExceptionRecord.NumberParameters>=2 then
  begin
    //get the address
    address:=debugevent.Exception.ExceptionRecord.ExceptionInformation[1];

    //check if this thread was waiting for an int1 but got a pagefault instead
    if temporaryDisabledExceptionBreakpoints<>nil then
    begin
      //pagefault while waiting for single step
      {$ifdef DEBUG}
      Messagebox(0,rsDebugHandleAccessViolationDebugEventNow,rsSpecialCase,0);
      {$endif}
      for i:=0 to temporaryDisabledExceptionBreakpoints.Count-1 do
      begin

        bp:=PBreakpoint(temporaryDisabledExceptionBreakpoints[i]);
        if not bp^.markedfordeletion then
          TdebuggerThread(debuggerthread).setBreakpoint(bp);

        dec(bp^.referencecount); //decrease referencecount so they can be deleted
      end;

      freeandnil(temporaryDisabledExceptionBreakpoints);
      exit;   //raise the exception in the game and let it crash
    end
    else
      temporaryDisabledExceptionBreakpoints:=Tlist.create;

    //now remove the protections

    debuggercs.enter;
    for i:=0 to breakpointlist.count-1 do
    begin
      bp:=breakpointList[i];
      if (bp.breakpointMethod=bpmException) then  //don't check for active, as some breakpoint events might be stacked
      begin
        //check if the address is in this breakpoint range
        if inrangex(address, GetPageBase(bp.address), GetPageBase(bp.address+bp.size)+$fff) or
           inrangex(address+$1000, GetPageBase(bp.address), GetPageBase(bp.address+bp.size)+$fff)
        then
        begin
          TdebuggerThread(debuggerthread).UnsetBreakpoint(bp);
          inc(bp.referencecount);
          temporaryDisabledExceptionBreakpoints.Add(bp);
        end;
      end;
    end;

    debuggercs.leave;


    if temporaryDisabledExceptionBreakpoints.count=0 then
    begin
      //not caused by my pagechanges
      freeandnil(temporaryDisabledExceptionBreakpoints);
      exit; //continue unhandled
    end;



    breakAddress:=address;

    //freeze all threads except this one and do a single step

    context^.EFlags:=eflags_setTF(context^.EFlags,1);
    setContext;


    {$ifdef darwin}
    task_suspend(processhandle);
    {$endif}
    {$ifdef windows}
    NtSuspendProcess(processhandle);
    {$endif}

    ResumeThread(self.Handle);


   // suspendthread(self.Handle);

    //handled, continue till the next int1
    dwContinueStatus:=DBG_CONTINUE;

  end
  else
    dwContinueStatus:=DBG_EXCEPTION_NOT_HANDLED;
end;



function TDebugThreadHandler.HandleExceptionDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
var
  exceptionAddress: ptrUint;
  i: integer;
  bp: PBreakpoint;
  found: boolean;
begin
  TDebuggerthread(debuggerthread).execlocation:=16;
  unhandledException:=false;
  bp:=nil;

  OutputDebugString(inttohex(ThreadId,1)+'('+inttohex(contexthandler.InstructionPointerRegister^.getvalue(context),8)+')'+':HandleExceptionDebugEvent:'+inttohex(debugEvent.Exception.ExceptionRecord.ExceptionCode,8));
  exceptionAddress := ptrUint(debugEvent.Exception.ExceptionRecord.ExceptionAddress);



  case debugEvent.Exception.ExceptionRecord.ExceptionCode of
    EXCEPTION_DBVM_BREAKPOINT:
    begin
      outputdebugstring('EXCEPTION_DBVM_BREAKPOINT');


      if (debugevent.Exception.ExceptionRecord.NumberParameters>=6) and (debugevent.Exception.ExceptionRecord.ExceptionInformation[5]=1) then
        dbvm_currentCR3:=debugevent.Exception.ExceptionRecord.ExceptionInformation[1] and MAXPHYADDRMASKPB
      else
        dbvm_currentCR3:=0;


      //dwContinueStatus supports DBG_CONTINUE_SINGLESTEP
      if debugEvent.Exception.ExceptionRecord.ExceptionFlags=dword(-1) then
        result:= singleStep(dwContinueStatus)
      else
      begin
        found:=false;
        debuggercs.enter;
        for i := 0 to breakpointlist.Count - 1 do
        begin
          bp:=PBreakpoint(breakpointlist.Items[i]);
          if bp^.active and
             (bp^.breakpointMethod=bpmDBVMNative) and
             (bp^.dbvmwatchid=debugEvent.Exception.ExceptionRecord.ExceptionFlags)
          then
          begin
            found:=true;
            break;
          end;
        end;
        debuggerCS.leave;

        if found then
          Result := DispatchBreakpoint(bp^.address, -2, dwContinueStatus)
        else
          Result := DispatchBreakpoint(context^.{$ifdef cpu64}Rip{$else}eip{$endif}, -2, dwContinueStatus);
      end;

      if singlestepping then
        dwContinueStatus:=DBG_CONTINUE_SINGLESTEP
      else
        dwContinueStatus:=DBG_CONTINUE;

      setcontext;
    end;

    EXCEPTION_BREAKPOINT, STATUS_WX86_BREAKPOINT: //SW bp
    begin
      if processhandler.SystemArchitecture=archX86 then
      begin
        OutputDebugString('EXCEPTION_BREAKPOINT:'+inttohex(context^.{$ifdef cpu64}rip{$else}eip{$endif},8));


        //if this is the first breakpoint exception check if it needs to set the entry point bp

        if TDebuggerThread(debuggerthread).NeedsToSetEntryPointBreakpoint then
        begin
          OutputDebugString('Calling SetEntryPointBreakpoint');
          TDebuggerthread(debuggerthread).Synchronize(TDebuggerthread(debuggerthread), TDebuggerthread(debuggerthread).SetEntryPointBreakpoint);
          OutputDebugString('After synchronize for SetEntryPointBreakpoint');
        end;

        //it's a software breakpoint, adjust eip to go back by 1
        dec(context^.{$ifdef cpu64}rip{$else}eip{$endif});
        setContext;


        Result := DispatchBreakpoint(context^.{$ifdef cpu64}Rip{$else}eip{$endif}, -1, dwContinueStatus);

        if dwContinueStatus=DBG_CONTINUE then
        begin
          if result=false then
          begin
            //initial breakpoint
            inc(context^.{$ifdef cpu64}rip{$else}eip{$endif});
            result:=true;
          end;
          context^.dr6:=0; //handled
          setContext;



        end
        else
        begin
          {if CurrentDebuggerInterface.name='Windows Debugger' then
          begin
            //emulate a call to the unhandled exception handler


          end;  }


         // context.dr6:=0; //unhandled
          inc(context^.{$ifdef cpu64}rip{$else}eip{$endif}); //undo the -1
          setContext;
        end;
      end
      else
      begin
        //debuggerinterface.handleswbp
        Result := DispatchBreakpoint(exceptionAddress, -1, dwContinueStatus);
      end;
    end;

    EXCEPTION_SINGLE_STEP, STATUS_WX86_SINGLE_STEP:
    begin
      if processhandler.SystemArchitecture=archArm then
      begin
        OutputDebugString('EXCEPTION_SINGLE_STEP. Exception address = '+inttohex(exceptionaddress,8));


      end
      else
      begin
        OutputDebugString('EXCEPTION_SINGLE_STEP. Dr6='+inttohex(context^.dr6,8)+' Dr7='+inttohex(context^.dr7,8)+' RIP='+inttohex(context^.{$ifdef cpu32}eip{$else}rip{$endif},8));

        if context^.dr6=0 then
        asm
        nop
        end;
      end;

      if temporaryDisabledExceptionBreakpoints<>nil then
      begin
        //OutputDebugString('After the single step of an exception caused by my page');

        context^.EFlags:=eflags_setTF(context^.EFlags,0); //not needed in windows, but let's clear it anyhow
        setContext;

        if singlestepping then
          result:=SingleStep(dwContinueStatus)
        else
          result:=DispatchBreakpoint(breakAddress, -1, dwContinueStatus);

        //reprotect the memory

        for i:=0 to temporaryDisabledExceptionBreakpoints.Count-1 do
        begin

          bp:=PBreakpoint(temporaryDisabledExceptionBreakpoints[i]);
          if not bp^.markedfordeletion then
            TdebuggerThread(debuggerthread).setBreakpoint(bp);

          dec(bp^.referencecount); //decrease referencecount so they can be deleted
        end;

        context^.EFlags:=eflags_setRF(context^.EFlags,0);
        setContext;

        SuspendThread(handle);

        {$ifdef darwin}
        task_resume(processhandle);
        {$endif}
        {$ifdef windows}
        NtResumeProcess(processhandle);
        {$endif}
        dwContinueStatus:=DBG_CONTINUE;
        freeandnil(temporaryDisabledExceptionBreakpoints);
        exit;
      end;


      if (CurrentDebuggerInterface is TNetworkDebuggerInterface) then
      begin
        //the address that caused the break is stored in ExceptionRecord.exceptionaddress
        if singlestepping or (uint_ptr(debugEvent.Exception.ExceptionRecord.ExceptionAddress)=1) then
          Result := SingleStep(dwContinueStatus) //only x86 returns this (on a rare occasion)
        else
          DispatchBreakpoint(uint_ptr(debugEvent.Exception.ExceptionRecord.ExceptionAddress), -1, dwContinueStatus);


        if changedcontext then //(check the context for changes first before sending it to the server for no reason)
          setContext;
      end
      else
      begin
        //find out what caused the breakpoint.

        if processhandler.SystemArchitecture=archArm then
        begin
          if exceptionAddress=QWORD(-1) then
            Result := SingleStep(dwContinueStatus)
          else
            Result := DispatchBreakpoint(exceptionAddress, -1, dwContinueStatus) ;
        end
        else
        begin //x86
          //inspect DR6
          //Problem: if the last breakpoint was unset dr7 is 0. Meaning that DR6 will read out 0 as well...
          //Solution: DeleteBreakpoint must NOT call unsetBreakpoint. Only call it from the breakpoint handler and the breakpoint cleanup
          if (context^.Dr6 and 1) = 1 then
          begin
            log('caused by DR0: Context.DR0='+inttohex(context^.DR0,8));
            Result := DispatchBreakpoint(context^.dr0, 0, dwContinueStatus)
          end
          else
          if ((context^.Dr6 shr 1) and 1) = 1 then
          begin
            log('caused by DR1: Context.DR1='+inttohex(context^.DR1,8));
            Result := DispatchBreakpoint(context.dr1, 1, dwContinueStatus)
          end
          else
          if ((context^.Dr6 shr 2) and 1) = 1 then
          begin
            log('caused by DR2: Context.DR2='+inttohex(context^.DR2,8));
            Result := DispatchBreakpoint(context.dr2, 2, dwContinueStatus)
          end
          else
          if ((context^.Dr6 shr 3) and 1) = 1 then
          begin
            log('caused by DR3: Context.DR3='+inttohex(context^.DR3,8));
            Result := DispatchBreakpoint(context.dr3, 3, dwContinueStatus)
          end
          else
          begin
            log('Not caused by a debugreg');
            Result := SingleStep(dwContinueStatus);
          end;

          if dwContinueStatus=DBG_CONTINUE then
          begin
            context^.dr6:=0; //handled
            setContext;
          end;
        end;
      end;
    end;

    EXCEPTION_ACCESS_VIOLATION:
    begin
      //exception
      result:=HandleAccessViolationDebugEvent(debugEvent, dwContinueStatus);

    end

    else
      dwContinueStatus:=DBG_EXCEPTION_NOT_HANDLED;
  end;


  if dwContinueStatus=DBG_EXCEPTION_NOT_HANDLED then
  begin
    if (UnexpectedExceptionAction = ueaBreak) or ((UnexpectedExceptionAction = ueaBreakIfInRegion) and (IsInUnexpectedExceptionRegion(context^.{$ifdef cpu64}Rip{$else}Eip{$endif}))) then
    begin
      if IsIgnoredExceptionCode(debugEvent.Exception.ExceptionRecord.ExceptionCode)=false then
      begin
        unhandledException:=true;

        unhandledExceptionCode:=debugEvent.Exception.ExceptionRecord.ExceptionCode;
        handleBreak(nil, dwContinueStatus);
        unhandledException:=false;
      end;
    end;

  end;

  result:=true;
end;

function TDebugThreadHandler.CreateThreadDebugEvent(debugevent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
var i: integer;
begin
  TDebuggerthread(debuggerthread).execlocation:=17;
  OutputDebugString(format('CreateThreadDebugEvent processid=0x%x threadid=0x%x',[debugevent.dwProcessId, debugevent.dwThreadId]));
  processid := debugevent.dwProcessId;
  threadid  := debugevent.dwThreadId;

  if handle=0 then
  begin
    OutputDebugString('old Handle is 0');
    if currentdebuggerinterface is TNetworkDebuggerInterface then
      handle  := debugevent.CreateThread.hThread
    else
    begin
      if currentdebuggerinterface.controlsTheThreadList then
        handle  := OpenThread(THREAD_ALL_ACCESS, false, threadid )
      else
        handle := 0;
    end;

    OutputDebugString('old Handle is now '+handle.ToHexString);
  end;

  Result    := true;


  dwContinueStatus:=DBG_CONTINUE;
end;

function TDebugThreadHandler.CreateProcessDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
var
  i: integer;
begin
  TDebuggerthread(debuggerthread).execlocation:=18;
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
      symhandler.reinitialize(true);
    end;



    if {$ifdef windows} (CurrentDebuggerInterface is TKernelDebugInterface) or {$endif}
       (CurrentDebuggerInterface is TNetworkDebuggerInterface) then //the kerneldebuginterface and networkdebuginterface do not give a breakpoint as init so use create as attachevent
      onAttachEvent.SetEvent;

    {$ifdef windows}
    if (CurrentDebuggerInterface is TWindowsDebuggerInterface) and (debugEvent.CreateProcessInfo.hFile<>0) then
      closeHandle(debugEvent.CreateProcessInfo.hFile); //we don't need this
    {$endif}


    secondcreateprocessdebugevent:=true;
  end;
  Result := true;
  dwContinueStatus:=DBG_CONTINUE;
end;

function TDebugThreadHandler.ExitThreadDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
var
  i: integer;
begin
  TDebuggerthread(debuggerthread).execlocation:=19;
  Outputdebugstring('ExitThreadDebugEvent');
  TDebuggerThread(debuggerthread).CurrentThread:=nil;
  Result := true;
  dwContinueStatus:=DBG_CONTINUE;
end;

function TDebugThreadHandler.ExitProcessDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
begin
  TDebuggerthread(debuggerthread).execlocation:=20;
  outputdebugstring('ExitProcessDebugEvent');
  dwContinueStatus:=DBG_CONTINUE;
  Result := False;
end;

function TDebugThreadHandler.LoadDLLDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
var m: string;
    mw: widestring;
    x: pchar;
    xw: pwidechar absolute x;
    br: PtrUInt;

    p: pointer;
begin
  TDebuggerthread(debuggerthread).execlocation:=21;
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
        HandleBreak(nil, dwContinueStatus);

      end;

    end;
  end;


  if (debugEvent.LoadDll.hFile<>0) and (debugEvent.LoadDll.hFile<>INVALID_HANDLE_VALUE) then
    closehandle(debugEvent.LoadDll.hFile);

  Result := true;
  dwContinueStatus:=DBG_CONTINUE;
end;

function TDebugThreadHandler.UnloadDLLDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
begin
  TDebuggerthread(debuggerthread).execlocation:=22;
  outputdebugstring('UnloadDLLDebugEvent');
  Result := true;
  dwContinueStatus:=DBG_CONTINUE;
end;

function TDebugThreadHandler.OutputDebugStringEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
var s: pchar;
    ws: pwidechar;
    x: PtrUInt;
begin
  TDebuggerthread(debuggerthread).execlocation:=23;
  outputdebugstring('OutputDebugStringEvent');

  if FormDebugStrings<>nil then
  begin
    if debugEvent.DebugString.fUnicode>0 then
    begin
      ws:=getmem(debugEvent.DebugString.nDebugStringLength*2+2);
      try
        ReadProcessMemory(processhandle, debugEvent.DebugString.lpDebugStringData, ws, debugEvent.DebugString.nDebugStringLength*2,x);
        ws[debugEvent.DebugString.nDebugStringLength]:=#0;
        ws[x div 2]:=#0;

        DebugEventString:=ws;

        TDebuggerthread(debuggerthread).Synchronize(TDebuggerthread(debuggerthread), AddDebugEventString);
      finally
        freememandnil(ws);
      end;
    end
    else
    begin
      s:=getmem(debugEvent.DebugString.nDebugStringLength+2);
      try
        ReadProcessMemory(processhandle, debugEvent.DebugString.lpDebugStringData, s, debugEvent.DebugString.nDebugStringLength,x);
        s[debugEvent.DebugString.nDebugStringLength]:=#0;
        s[x]:=#0;

        DebugEventString:=s;

        TDebuggerthread(debuggerthread).Synchronize(TDebuggerthread(debuggerthread), AddDebugEventString);
      finally
        freememandnil(s);
      end;
    end;


  end;

  Result := true;
  dwContinueStatus:=DBG_EXCEPTION_NOT_HANDLED;
end;

function TDebugThreadHandler.RipEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
begin
  TDebuggerthread(debuggerthread).execlocation:=24;
  outputdebugstring('RipEvent');
  Result := true;
  dwContinueStatus:=DBG_CONTINUE;

end;

function TDebugThreadHandler.HandleUnknownEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
begin
  TDebuggerthread(debuggerthread).execlocation:=25;
  OutputDebugString('Unknown event');
  Result := true;
  dwContinueStatus:=DBG_CONTINUE;
end;

destructor TDebugThreadHandler.destroy;
begin
  freememandnil(realcontextpointer);

  {if (handle<>0) and (getConnection=nil) then
    closehandle(handle);} //do not close, handle is owned by the debuggerinterface

  if lastiptlog<>nil then
    freememandnil(lastiptlog);

  inherited destroy;
end;


constructor TDebugThreadHandler.Create(debuggerthread: TObject; attachEvent: Tevent; continueEvent: TEvent; breakpointlist: TList; threadlist: Tlist; debuggerCS: TGuiSafeCriticalSection);
var
  InitializeContext: function(buffer: pointer; contextflags: DWORD; outputcontext: pointer; var length: word): BOOL; stdcall;
  k: HModule;

  contextsize: word;
  e: integer;
  es: string;
begin
  //because fpc's structure is not alligned on a 16 byte base I have to allocate more memory and byteshift the structure if needed
  contexthandler:=getBestContextHandler;

  getmem(realcontextpointer,contexthandler.ContextSize+4096);
  zeromemory(realcontextpointer,contexthandler.ContextSize+4096);

  {$ifdef windows}
  if processhandler.SystemArchitecture=archX86 then
  begin
    k:=GetModuleHandle('kernel32.dll');
    InitializeContext:=getprocaddress(k, 'InitializeContext');
    if assigned(initializeContext) then
    begin
      contextsize:=sizeof(TCONTEXT)+4096;
      if InitializeContext(realcontextpointer, CONTEXT_ALL or CONTEXT_EXTENDED_REGISTERS {$ifdef cpu64}or CONTEXT_XSTATE{$endif}, @context, contextsize)=false then
      begin
        if getlasterror=ERROR_INSUFFICIENT_BUFFER then
        begin
          freememandnil(realcontextpointer);
          getmem(realcontextpointer, contextsize+1024);
          contextsize:=contextsize+1024;
          InitializeContext(realcontextpointer, CONTEXT_ALL or CONTEXT_EXTENDED_REGISTERS {$ifdef cpu64}or CONTEXT_XSTATE{$endif}, @context, contextsize);
        end
        else
        begin
          e:=GetLastError;
          es:=GetLastErrorText(e);
          OutputDebugString(es);
        end;
      end;
    end;

    _contextsize:=contextsize;
  end;
  {$endif}


  if context=nil then
  begin
    context:=Align(realcontextpointer, 16);
    contexthandler.setcontextflags(context, CONTEXT_ALL or CONTEXT_EXTENDED_REGISTERS)
  end;


  self.debuggerthread := debuggerthread;
  onAttachEvent := attachEvent;
  onContinueEvent := continueEvent;
  self.breakpointList:=breakpointlist;
  self.threadlist:=threadlist;
  self.debuggerCS:=debuggerCS;
end;


function TDebugEventHandler.HandleDebugEvent(debugEvent: TDEBUGEVENT; var dwContinueStatus: dword): boolean;
var
  currentThread: TDebugThreadHandler;

  newthread: boolean;
  i: integer;
  ActiveBPList: TList;

  //hasDebugEvent: boolean;
begin
  //OutputDebugString('HandleDebugEvent:'+inttostr(debugEvent.dwDebugEventCode));
  //find the TDebugThreadHandler class that belongs to this thread
  debuggercs.enter;

  TDebuggerthread(debuggerthread).execlocation:=10;

  currentThread := nil;


  for i := 0 to ThreadList.Count - 1 do
  begin
    if TDebugThreadHandler(ThreadList.Items[i]).threadid = debugEvent.dwThreadId then
    begin
      currentThread := ThreadList.Items[i];
      break;
    end;
  end;

  if debugEvent.dwDebugEventCode=EXIT_THREAD_DEBUG_EVENT then
  begin
    //don't touch anything

    if currentThread<>nil then
    begin
      Result := currentThread.ExitThreadDebugEvent(debugEvent, dwContinueStatus);
      ThreadList.Remove(currentThread);
      currentThread.Free;
      currentthread:=nil;
    end;

    debuggercs.leave;

    if frmthreadlist<>nil then
      TDebuggerthread(debuggerthread).Synchronize(TDebuggerthread(debuggerthread), updatethreadlist);

    exit(true);
  end;


  if currentThread = nil then //not found
  begin
    //so create and add it
    newthread:=true;
    currentThread := TDebugThreadHandler.Create(debuggerthread, fonattachEvent, fOnContinueEvent, breakpointlist, threadlist, debuggerCS);
    currentThread.processid := debugevent.dwProcessId;
    currentThread.threadid := debugevent.dwThreadId;
    currentThread.CreateThreadDebugEvent(debugevent,dwContinueStatus);

    ThreadList.Add(currentThread);
  end
  else
    newthread:=false;

  currentthread.hasiptlog:=false;
  currentthread.isHandled:=CurrentDebuggerInterface.IsInjectedEvent=false;
  currentThread.currentBP:=nil;


  currentthread.FillContext;
  TDebuggerthread(debuggerthread).currentThread:=currentThread;

  //hasDebugEvent:=not ((currentthread.context^.Dr6=0) or (word(urrentthread.context^.Dr6)=$0ff0));
  if processhandler.SystemArchitecture=archX86 then
    outputdebugstring(format('DE - %x: %.8x',  [currentThread.ThreadId, currentThread.context^.dr6]))
  else
    outputdebugstring(format('DE - %x',  [currentThread.ThreadId]));

  debuggercs.leave;

  //The most important data has been gathered (DR6 of the thread). it's safe from this point to occasionally release the lock
  currentdebugEvent:=debugEvent;

  if newthread and (frmthreadlist<>nil) then
    TDebuggerthread(debuggerthread).Synchronize(TDebuggerthread(debuggerthread), updatethreadlist);

  if frmDebugEvents<>nil then
    TDebuggerthread(debuggerthread).Synchronize(TDebuggerthread(debuggerthread), UpdateDebugEventWindow);


  TDebuggerthread(debuggerthread).execlocation:=11;

  case debugEvent.dwDebugEventCode of
    EXCEPTION_DEBUG_EVENT:      Result := currentThread.HandleExceptionDebugEvent(debugevent, dwContinueStatus);
    CREATE_THREAD_DEBUG_EVENT:  Result := currentThread.CreateThreadDebugEvent(debugEvent, dwContinueStatus);
    CREATE_PROCESS_DEBUG_EVENT: Result := currentThread.CreateProcessDebugEvent(debugEvent, dwContinueStatus);
    //EXIT_THREAD_DEBUG_EVENT: ;//don't handle here
    EXIT_PROCESS_DEBUG_EVENT:   Result := currentThread.ExitProcessDebugEvent(debugEvent, dwContinueStatus);
    LOAD_DLL_DEBUG_EVENT:       Result := currentThread.LoadDLLDebugEvent(debugEvent, dwContinueStatus);
    UNLOAD_DLL_DEBUG_EVENT:     Result := currentThread.UnloadDLLDebugEvent(debugEvent, dwContinueStatus);
    OUTPUT_DEBUG_STRING_EVENT:  Result := currentThread.OutputDebugStringEvent(debugEvent, dwContinueStatus);
    RIP_EVENT:                  Result := currentThread.RipEvent(debugEvent, dwContinueStatus);
    else
                                Result := currentThread.HandleUnknownEvent(debugEvent, dwContinueStatus);
  end;


  TDebuggerthread(debuggerthread).execlocation:=12;


  //cleanup time for this thread

  if (currentthread<>nil) and CurrentDebuggerInterface.usesDebugRegisters then //if it wasn't a thread destruction tell this thread it isn't being handled anymore
  begin
    debuggercs.enter; //wait till other threads are done with this

    //if this was a thread that caused a breakpoint unset problem last time call the breakpoint cleanup routine now
    //if currentthread.needstocleanup then
    currentthread.fillContext;

    if (not TDebuggerthread(debuggerthread).usesGlobalDebug) and (processhandler.SystemArchitecture=archX86) and ((dwContinueStatus=DBG_CONTINUE) or (currentThread.context^.Dr6=0) or (word(currentThread.context^.dr6)=$0ff0)) then
    begin
      //continued or not an unhandled debug register exception
      if processhandler.SystemArchitecture=archX86 then
        currentthread.context^.dr6:=0;

      //get the active bp list for this thread  (unsetting the breakpoint in safe mode sets active to false, which would break setting them again otherwise)
      ActiveBPList:=TList.create;
      for i:=0 to breakpointlist.count-1 do
      begin
        if PBreakpoint(breakpointlist[i])^.active and          //active
           (PBreakpoint(breakpointlist[i])^.breakpointMethod=bpmDebugRegister) and //it's a debug register bp
           ((PBreakpoint(breakpointlist[i])^.ThreadID=0) or (PBreakpoint(breakpointlist[i])^.ThreadID=currentthread.ThreadId)) and //this isn't a thread specific breakpoint, or this breakpoint affects this thread
           (not (currentthread.setInt1Back and (currentthread.Int1SetBackBP=PBreakpoint(breakpointlist[i])))) //this isn't an XP/Network hack that just disabled the bp for this thread so it can do a single step and re-enable next step
        then
          ActiveBPList.add(breakpointlist[i]);

      end;


      //remove all current breakpoints
      if BPOverride then
      begin
        if processhandler.SystemArchitecture=archX86 then
        begin
          //override, the debugregs are mine
          currentthread.context^.dr0:=0;
          currentthread.context^.dr1:=0;
          currentthread.context^.dr2:=0;
          currentthread.context^.dr3:=0;
          currentthread.context^.dr7:=$400;

          currentThread.setContext(cfDebug);
        end;
      end
      else
      begin
        //no override, let's be kind and only unset those that are actually used
        for i:=0 to ActiveBPList.count-1 do
          TDebuggerthread(debuggerthread).UnsetBreakpoint(breakpointlist[i], currentthread.context);

        currentthread.setContext(cfDebug);
      end;

      for i:=0 to ActiveBPList.count-1 do
        TDebuggerthread(debuggerthread).SetBreakpoint(ActiveBPList[i], currentthread);

      ActiveBPList.free;



      currentthread.needstocleanup:=false;
      TDebuggerthread(debuggerthread).cleanupDeletedBreakpoints(false);
    end;

    currentthread.isHandled:=false;
    debuggercs.leave;
  end;

  TDebuggerthread(debuggerthread).execlocation:=13;


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

  eventdata: PDebugEventData;
begin
  if frmDebugEvents<>nil then //check if it's still here
  begin
    eventtext:=inttohex(currentdebugevent.dwDebugEventCode,1);
    case currentdebugevent.dwDebugEventCode of
      CREATE_PROCESS_DEBUG_EVENT: eventtext:='CREATE_PROCESS_DEBUG_EVENT';
      CREATE_THREAD_DEBUG_EVENT: eventtext:='CREATE_THREAD_DEBUG_EVENT';
      EXCEPTION_DEBUG_EVENT: eventtext:='EXCEPTION_DEBUG_EVENT('+inttostr(currentdebugevent.Exception.dwFirstChance)+')';
      EXIT_PROCESS_DEBUG_EVENT: eventtext:='EXIT_PROCESS_DEBUG_EVENT';
      EXIT_THREAD_DEBUG_EVENT: eventtext:='EXIT_THREAD_DEBUG_EVENT';
      LOAD_DLL_DEBUG_EVENT: eventtext:='LOAD_DLL_DEBUG_EVENT';
      OUTPUT_DEBUG_STRING_EVENT: eventtext:='OUTPUT_DEBUG_STRING_EVENT';
      RIP_EVENT: eventtext:='RIP_EVENT';
      UNLOAD_DLL_DEBUG_EVENT: eventtext:='UNLOAD_DLL_DEBUG_EVENT';
      else
        eventtext:='Unknown event '+inttostr(currentdebugevent.dwDebugEventCode);
    end;

    eventtext:=format('pid:%x tid:%x - %s (eip:%x)',[currentdebugEvent.dwProcessId, currentdebugevent.dwThreadId, eventtext, TDebuggerthread(debuggerthread).currentThread.context^.{$ifdef cpu64}Rip{$else}eip{$endif}]);

    getmem(eventdata, sizeof(TDebugEventData));
    eventdata.context:=TDebuggerthread(debuggerthread).currentThread.context^;
    frmDebugEvents.lbDebugEvents.Items.AddObject(eventtext, tobject(eventdata));

    frmDebugEvents.lbDebugEvents.TopIndex:=frmDebugEvents.lbDebugEvents.items.count-1
  end;


end;

constructor TDebugEventHandler.Create(debuggerthread: TObject; OnAttachEvent: TEvent; OnContinueEvent: TEvent; breakpointlist: TList; threadlist: Tlist; debuggerCS: TGuiSafeCriticalSection);
begin
  self.debuggerthread := debuggerthread;
  fOnAttachEvent      := OnAttachEvent;
  fOnContinueEvent    := OnContinueEvent;
  self.breakpointlist := breakpointlist;
  self.threadlist     := threadlist;
  self.debuggerCS     := debuggercs;
  inherited Create;
end;


end.

