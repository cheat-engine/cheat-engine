unit DebugHelper;

{$mode DELPHI}

interface

uses
  Windows, Classes, SysUtils, Controls, forms, syncobjs, guisafecriticalsection, Dialogs,
  foundcodeunit, debugeventhandler, cefuncproc, newkernelhandler, comctrls,
  debuggertypedefinitions, formChangedAddresses, frmTracerUnit, KernelDebuggerInterface, VEHDebugger,
  WindowsDebugger, debuggerinterfaceAPIWrapper, debuggerinterface,symbolhandler,
  fgl, disassembler, NetworkDebuggerInterface, Clipbrd, commonTypeDefs ,BreakpointTypeDef;

{$warn 4056 off}


type


  TDebuggerthread = class(TThread)
  private
    eventhandler: TDebugEventHandler;
    ThreadList: TList; //only the debugger thread can add or remove from this list
    BreakpointList: TList;  //only the main thread can add or remove from this list

    debuggerCS: TGuiSafeCriticalSection;
    //breakpointCS: TGuiSafeCriticalSection;
    //ThreadListCS: TGuiSafeCriticalSection; //must never be locked before breakpointCS


    OnAttachEvent: Tevent; //event that gets set when a process has been created
    OnContinueEvent: TEvent; //event that gets set by the user when he/she wants to continue from a break

    //settings
    handlebreakpoints: boolean;
    hidedebugger: boolean;
    canusedebugregs: boolean;

    createProcess: boolean;

    fNeedsToSetEntryPointBreakpoint: boolean;
    filename,parameters: string;


    fcurrentThread: TDebugThreadHandler;
    globalDebug: boolean; //kernelmode debugger only

    fRunning: boolean;

    ResumeProcessWhenIdleCounter: dword; //suspend counter to tell the cleanup handler to resume the process

    hasSetDEPPolicy: boolean;

    function getDebugThreadHanderFromThreadID(tid: dword): TDebugThreadHandler;

    procedure GetBreakpointList(address: uint_ptr; size: integer; var bplist: TBreakpointSplitArray);
    procedure defaultConstructorcode;
    procedure lockSettings;
    procedure WaitTillAttachedOrError;
    procedure setCurrentThread(x: TDebugThreadHandler);
    function getCurrentThread: TDebugThreadHandler;
    procedure FindCodeByBP(address: uint_ptr; size: integer; bpt: TBreakpointTrigger; breakpointmethod: TBreakpointMethod); overload;
    procedure FindCodeByBP(address: uint_ptr; size: integer; bpt: TBreakpointTrigger); overload;

    function AddBreakpoint(owner: PBreakpoint; address: uint_ptr; size: integer; bpt: TBreakpointTrigger; bpm: TBreakpointMethod; bpa: TBreakpointAction; debugregister: integer=-1; foundcodedialog: Tfoundcodedialog=nil; threadID: dword=0; frmchangedaddresses: Tfrmchangedaddresses=nil; FrmTracer: TFrmTracer=nil; tcount: integer=0; changereg: pregistermodificationBP=nil; OnBreakpoint: TBreakpointEvent=nil): PBreakpoint;


    function AdjustAccessRightsWithActiveBreakpoints(ar: TAccessRights; base: ptruint; size: integer): TAccessRights;
    function getBestProtectionForExceptionBreakpoint(breakpointtrigger: TBreakpointTrigger; base: ptruint; size: integer): TaccessRights;
    function getOriginalProtectForExceptionBreakpoint(base: ptruint; size: integer): TaccessRights;
    procedure setDepPolicy;

  public
    InitialBreakpointTriggered: boolean; //set by a debugthread when the first unknown exception is dealth with causing all subsequent unexpected breakpoitns to become unhandled
    temporaryDisabledExceptionBreakpoints: Tlist;

    execlocation: integer; //debugging related to pinpoint problems
    guiupdate: boolean; //set by a thread handler when it has updated the gui. (waitafterguiupdate uses this)


    procedure cleanupDeletedBreakpoints(Idle: boolean=true; timeoutonly: boolean=true);

    function SetBreakpoint(breakpoint: PBreakpoint; UpdateForOneThread: TDebugThreadHandler=nil): boolean;
    procedure UnsetBreakpoint(breakpoint: PBreakpoint; specificContext: PContext=nil; threadid:integer=-1);

    procedure LockDebugger;
    procedure UnlockDebugger;

    function lockThreadlist: TList;
    procedure unlockThreadlist;

    procedure lockbplist;
    procedure unlockbplist;

    procedure updatebplist(lv: TListview; showshadow: boolean);
    procedure setbreakpointcondition(bp: PBreakpoint; easymode: boolean; script: string);
    function getbreakpointcondition(bp: PBreakpoint; var easymode: boolean):pchar;


    procedure getBreakpointAddresses(var AddressList: TAddressArray);
    function  isBreakpoint(address: uint_ptr; address2: uint_ptr=0; includeinactive: boolean=false): PBreakpoint;
    function  CodeFinderStop(codefinder: TFoundCodeDialog): boolean;
    function  setChangeRegBreakpoint(regmod: PRegisterModificationBP): PBreakpoint;
    procedure setBreakAndTraceBreakpoint(frmTracer: TFrmTracer; address: ptrUint; BreakpointTrigger: TBreakpointTrigger; breakpointmethod: TBReakpointmethod; bpsize: integer; count: integer; startcondition:string=''; stopcondition:string=''; stepover: boolean=false; nosystem: boolean=false);
    function  stopBreakAndTrace(frmTracer: TFrmTracer): boolean;
    function FindWhatCodeAccesses(address: uint_ptr; FoundCodeDialog:TFoundCodeDialog=nil): tfrmChangedAddresses;
    function  FindWhatCodeAccessesStop(frmchangedaddresses: Tfrmchangedaddresses): boolean;
    procedure FindWhatAccesses(address: uint_ptr; size: integer; breakpointmethod: TBreakpointMethod); overload;
    procedure FindWhatAccesses(address: uint_ptr; size: integer); overload;
    procedure FindWhatWrites(address: uint_ptr; size: integer; breakpointmethod: TBreakpointMethod); overload;
    procedure FindWhatWrites(address: uint_ptr; size: integer); overload;
    function  SetOnWriteBreakpoint(address: ptrUint; size: integer; bpm: TBreakpointMethod; tid: dword=0; OnBreakpoint: TBreakpointEvent=nil): PBreakpoint; overload;
    function  SetOnWriteBreakpoint(address: ptrUint; size: integer; tid: dword=0; OnBreakpoint: TBreakpointEvent=nil): PBreakpoint; overload;
    function  SetOnAccessBreakpoint(address: ptrUint; size: integer; bpm: TBreakpointMethod; tid: dword=0; OnBreakpoint: TBreakpointEvent=nil): PBreakpoint; overload;
    function  SetOnAccessBreakpoint(address: ptrUint; size: integer; tid: dword=0; OnBreakpoint: TBreakpointEvent=nil): PBreakpoint; overload;
    function  SetOnExecuteBreakpoint(address: ptrUint; bpm: TBreakpointMethod; askforsoftwarebp: boolean = false; tid: dword=0; OnBreakpoint: TBreakpointEvent=nil): PBreakpoint; overload;
    function  SetOnExecuteBreakpoint(address: ptrUint; askforsoftwarebp: boolean = false; tid: dword=0; OnBreakpoint: TBreakpointEvent=nil): PBreakpoint; overload;
    function  ToggleOnExecuteBreakpoint(address: ptrUint; breakpointmethod: TBreakpointMethod; tid: dword=0): PBreakpoint;

    procedure UpdateDebugRegisterBreakpointsForThread(t: TDebugThreadHandler);
    procedure RemoveBreakpoint(breakpoint: PBreakpoint);
    function GetUsableDebugRegister(breakpointTrigger: TBreakpointTrigger): integer;
    function GetMaxBreakpointCountForThisType(breakpointTrigger: TBreakpointTrigger): integer;
    function DoBreakpointTriggersUseSameDebugRegisterKind(bpt1: TBreakpointTrigger; bpt2: TBreakpointTrigger): boolean;

    procedure ContinueDebugging(continueOption: TContinueOption; runtillAddress: ptrUint=0; handled: boolean=true);

    procedure SetEntryPointBreakpoint;


    constructor MyCreate2(filename: string; parameters: string; breakonentry: boolean=true); overload;
    constructor MyCreate2(processID: THandle); overload;
    destructor Destroy; override;

    function isWaitingToContinue: boolean;

    function getrealbyte(address: ptrUint): byte;

    procedure startBranchMapper(tidlist: tlist=nil);
    procedure stopBranchMapper;

    property CurrentThread: TDebugThreadHandler read getCurrentThread write setCurrentThread;
    property NeedsToSetEntryPointBreakpoint: boolean read fNeedsToSetEntryPointBreakpoint;
    property running: boolean read fRunning;

    property usesGlobalDebug: boolean read globalDebug;

    procedure Terminate;
    procedure Execute; override;
  end;

var
  debuggerthread: TDebuggerthread = nil;

  PreventDebuggerDetection: boolean=false;
  preferedBreakpointMethod: TBreakpointMethod;
  BPOverride: boolean=true;

resourcestring
  rsTheFollowingOpcodesAccessed = 'The following opcodes accessed %s';
  rsTheFollowingOpcodesWriteTo = 'The following opcodes write to %s';


implementation

uses cedebugger, kerneldebugger, formsettingsunit, FormDebugStringsUnit,
     frmBreakpointlistunit, plugin, memorybrowserformunit, autoassembler,
     pluginexports, networkInterfaceApi, processhandlerunit, Globals, LuaCaller,
     vmxfunctions, LuaHandler, frmDebuggerAttachTimeoutUnit;

//-----------Inside thread code---------


resourcestring
  rsDebuggerCrash = 'Debugger Crash';
  rsCreateProcessFailed = 'CreateProcess failed:%s';

  rsOnlyTheDebuggerThreadIsAllowedToSetTheCurrentThread = 'Only the debugger '
    +'thread is allowed to set the current thread';
  rsUnreadableAddress = 'Unreadable address';
  rsDebuggerInterfaceDoesNotSupportSoftwareBreakpoints = 'Debugger interface %s'
    +' does not support software breakpoints';
  rsDebuggerInterfaceDoesNotSupportDBVMBreakpoints = 'Debugger interface %s'
    +' does not support DBVM breakpoints';
  rsAddBreakpointAnInvalidDebugRegisterIsUsed = 'AddBreakpoint: An invalid '
    +'debug register is used';
  rsAll4DebugRegistersAreCurrentlyUsedUpFreeOneAndTryA = 'All debug '
    +'registers are currently used up. Free one and try again';

  rsAllDebugRegistersAreUsedUpDoYouWantToUseASoftwareBP = 'All debug '
    +'registers are used up. Do you want to use a software breakpoint?';
  rsAllDebugRegistersAreUsedUp = 'All debug registers are used up';
  rsYes = 'Yes';
  rsNo = 'No';
  rsOutOfHWBreakpoints = 'All debug registers are used up and this debugger '
    +'interface does not support software Breakpoints. Remove some and try '
    +'again';
  rsUnreadableMemoryUnableToSetSoftwareBreakpoint = 'Unreadable memory. '
    +'Unable to set software breakpoint';
  rsDebuggerFailedToAttach = 'Debugger failed to attach';
  rsThisDebuggerInterfaceDoesnTSupportBreakOnEntryYet = 'This debugger '
    +'interface :''%s'' doesn''t support Break On Entry yet';
  rsLastLocation = ' (Last location:';
  rsCalledFromMainThread = 'Called from main thread';
  rsCalledFromDebuggerThread = 'Called from debugger thread';
  rsCalledFromAnUnexpectedThread = 'Called from an unexpected thread';
  rsDebuggerthreadIsAtPoint = 'debuggerthread is at point ';
  rsBreakpointError = 'Breakpoint error:';
  rsNoForm = 'No form';
  rsDebuggerAttachTimeout = 'Debugger attach timeout';
  rsTheDebuggerAttachHasTimedOut = 'The debugger attach is taking a while. This is normal when there are many symbols or the system is slow so please be patient. But if you don''t have the time to wait or the program has crashes to desktop, then you can cancel this wait. Beware though that you may have to restart the target process if you do wish to debug anyhow.'#13#10'Do you wish to wait longer?';
  rsNoExecutePageExceptionsForYou = 'Execute page exception breakpoints are '
    +'not possible on your system';
  rsFailureGettingDEPInformation = 'Failure getting DEP information for this '
    +'process. No Execute Breakpoint support';
  rsCantEnableDEP = 'Data execution prevention is not enabled in this process '
    +'and can not be enabled';
  rsAskToEnableNX = 'For execute page exceptions the target process must '
    +'support No-Execute page support. Currently this process doesn''t have '
    +'this enabled, but if you think the process itself does support it you '
    +'can enable it.'#13#10'Do you wish to enable No-Execute support for this process?';
  rsProcessSucksNoDEPSupport = 'Ooops, looks like the process does not support'
    +' No Execute';
  rsFailedDEPPermanently = 'Failed enabling No Execute AND blocked it from '
    +'every changing. Fuck';
  rsFailedDEP = 'Failed enabling No Execute';
  rsDepSettingTimeout = 'Timeout while trying to set DEP policy. Continue with'
    +' the breakpoint?';
  rsDebuggerAttachAborted = 'Debugger attach aborted';

procedure TDebuggerthread.Execute;
var
  debugEvent: _Debug_EVENT;
  debugging: boolean;
  currentprocesid: dword;
  ContinueStatus: dword;
  startupinfo: windows.STARTUPINFO;
  processinfo: windows.PROCESS_INFORMATION;
  dwCreationFlags: dword;
  error: integer;

  code,data: ptrUint;
  s: tstringlist;
  allocs: TCEAllocarray;

begin
  if IsDebuggerPresent() then
    self.NameThreadForDebugging('Debugger thread');

  if terminated then exit;

  execlocation:=0;

  try
    try
      currentprocesid := 0;
      DebugSetProcessKillOnExit(False); //do not kill the attached processes on exit



      if createprocess then
      begin
        dwCreationFlags:=DEBUG_PROCESS or DEBUG_ONLY_THIS_PROCESS;

        zeromemory(@startupinfo,sizeof(startupinfo));
        zeromemory(@processinfo,sizeof(processinfo));

        GetStartupInfo(@startupinfo);




        if windows.CreateProcess(
          pchar(filename),
          pchar('"'+filename+'" '+parameters),
          nil, //lpProcessAttributes
          nil, //lpThreadAttributes
          false, //bInheritHandles
          dwCreationFlags,
          nil, //lpEnvironment
          pchar(extractfilepath(filename)), //lpCurrentDirectory
          @startupinfo, //lpStartupInfo
          @processinfo //lpProcessInformation
        ) =false then
        begin
          error:=getlasterror;
          MessageBox(0, pchar(Format(utf8toansi(rsCreateProcessFailed), [inttostr(error)])
            ), pchar(utf8toansi(rsDebuggerCrash)), MB_ICONERROR or mb_ok);
          exit;
        end;


        processhandler.processid:=processinfo.dwProcessId;
        Open_Process;
        symhandler.reinitialize(true);

        closehandle(processinfo.hProcess);
      end else
      begin
        fNeedsToSetEntryPointBreakpoint:=false; //just be sure
        if not DebugActiveProcess(processid) then
        begin
          OutputDebugString('DebugActiveProcess failed');
          exit;
        end;
      end;

      currentprocesid := processid;

      debugging := True;


      while (not terminated) and debugging do
      begin

        execlocation:=1;


        if WaitForDebugEvent(debugEvent, 100) then
        begin
          ContinueStatus:=DBG_CONTINUE;
          execlocation:=2;

          if (pluginhandler<>nil) and (pluginhandler.handledebuggerplugins(@debugEvent)=1) then continue;

          debugging := eventhandler.HandleDebugEvent(debugEvent, ContinueStatus);

          if debugging then
          begin
            execlocation:=4;
            if ContinueStatus=DBG_EXCEPTION_NOT_HANDLED then //this can happen when the game itself is constantly raising exceptions
              cleanupDeletedBreakpoints(true, true); //only decrease the delete count if it's timed out (4 seconds in total)

            ContinueDebugEvent(debugEvent.dwProcessId, debugevent.dwThreadId, ContinueStatus);
          end;

          if waitafterguiupdate and guiupdate then
          begin
            guiupdate:=false;
            sleep(1);
          end;
        end
        else
        begin
          {
          no event has happened, for 100 miliseconds
          Do some maintenance in here
          }
          //remove the breakpoints that have been unset and are marked for deletion
          execlocation:=5;
          cleanupDeletedBreakpoints;
        end;


      end;

    except
      on e: exception do
        messagebox(0, pchar(utf8toansi(rsDebuggerCrash)+':'+e.message+rsLastLocation+inttostr(execlocation)+')'), '', 0);
    end;

  finally
    outputdebugstring('End of debugger');
    if currentprocesid <> 0 then
      debuggerinterfaceAPIWrapper.DebugActiveProcessStop(currentprocesid);

    terminate;
    OnAttachEvent.SetEvent;
  end;

  //end of the routine has been reached (only possingle on terminate, one of debug or exception)

end;

//-----------(mostly) Out of thread code---------

procedure TDebuggerThread.terminate;
var i: integer;
begin
  //remove all breakpoints
  for i:=0 to BreakpointList.Count-1 do
    RemoveBreakpoint(PBreakpoint(BreakpointList[i]));


  //tell all events to stop waiting and continue the debug loop. (that now has no breakpoints set)
  ContinueDebugging(co_run);

  fRunning:=false;
  inherited terminate; //and the normal terminate telling the thread to stop


end;

procedure TDebuggerThread.cleanupDeletedBreakpoints(Idle: boolean=true; timeoutonly: boolean=true);
{
remove the breakpoints that have been unset and are marked for deletion
that can be done safely since this routine is only called when no debug event has
happened, and the breakpoints have already been disabled

idle can be false if called from a thread that needed to clear it's breakpoint from an deleted breakpoint
}
var
  i: integer;
  bp: PBreakpoint;
  deleted: boolean;
  updated: boolean;
begin

  i:=0;
  updated:=false;
  debuggercs.enter;
  try
    while i<Breakpointlist.Count do
    begin
      deleted:=false;

      bp:=PBreakpoint(breakpointlist[i]);
      if bp^.markedfordeletion then
      begin
        if bp^.referencecount=0 then
        begin
          if not bp^.active then
          begin
            if (bp^.deletecountdown=0) or ((CurrentThread<>nil) and (currentthread.ThreadId=bp^.threadid)) then  //if countdown is 0, of it's a threadspecific bp and it's comming from the current threadid
            begin
              outputdebugstring('cleanupDeletedBreakpoints: deleting bp');
              breakpointlist.Delete(i);

              if bp^.conditonalbreakpoint.script<>nil then
                StrDispose(bp^.conditonalbreakpoint.script);

              if bp^.traceendcondition<>nil then
                Strdispose(bp^.traceendcondition);

              if assigned(bp^.OnBreakpoint) then
                LuaCaller.CleanupLuaCall(TMethod(bp^.OnBreakpoint));

              freememandnil(bp);

              deleted:=true;
              updated:=true;
            end
            else
            begin
              if idle then
              begin
                if (not timeoutonly) or (gettickcount>(bp^.deletetickcount+3000)) then
                  dec(bp^.deletecountdown);
              end;
            end;
          end
          else
          begin
            //Some douche forgot to disable it first, waste of processing cycle  (or windows 7+ default windows debugger)

            UnsetBreakpoint(bp);

            bp^.deletecountdown:=10;


          end;
        end;
      end;

      if not deleted then inc(i);

    end;
  finally
    debuggercs.leave;
  end;

  if idle and updated and (frmBreakpointlist<>nil) then
    postmessage(frmBreakpointlist.handle, WM_BPUPDATE,0,0); //tell the breakpointlist that there's been an update
end;




procedure TDebuggerThread.setCurrentThread(x: TDebugThreadHandler);
begin
  //no critical sections for the set and getcurrenthread.
  //routines that call this only call it when the debugger is already paused
  if GetCurrentThreadId <> self.ThreadID then
    raise Exception.Create(
      rsOnlyTheDebuggerThreadIsAllowedToSetTheCurrentThread);

  fCurrentthread := x;
end;

function TDebuggerThread.getCurrentThread: TDebugThreadHandler;
begin
  Result := fcurrentThread;
end;

function TDebuggerThread.isWaitingToContinue: boolean;
begin
  result:=(CurrentThread<>nil) and (currentthread.isWaitingToContinue);
end;

procedure TDebuggerThread.lockBPList;
begin
  LockDebugger
end;

procedure TDebuggerThread.unlockBPList;
begin
  UnlockDebugger
end;


function TDebuggerThread.lockThreadlist: TList;
//called from main thread
begin
  LockDebugger;
  result:=threadlist;
end;

procedure TDebuggerThread.unlockThreadlist;
begin
  UnlockDebugger
end;

procedure TDebuggerThread.LockDebugger;
begin
  debuggercs.enter;
end;

procedure TDebuggerThread.UnlockDebugger;
begin
  debuggercs.leave;
end;

function TDebuggerThread.getDebugThreadHanderFromThreadID(tid: dword): TDebugThreadHandler;
var
  i: integer;
begin
  result:=nil;
  debuggercs.Enter;
  try
    for i := 0 to threadlist.Count - 1 do
      if TDebugThreadHandler(threadlist.items[i]).ThreadId = tid then
      begin
        Result := TDebugThreadHandler(threadlist.items[i]);
        break;
      end;

  finally
    debuggercs.Leave;
  end;
end;

procedure TDebuggerThread.UpdateDebugRegisterBreakpointsForThread(t: TDebugThreadHandler);
var i: integer;
begin
  debuggercs.enter;
  try
    t.fillcontext;

    for i:=0 to BreakpointList.count-1 do
      if (PBreakpoint(breakpointlist[i])^.active) and (PBreakpoint(breakpointlist[i])^.breakpointMethod=bpmDebugRegister) then
        SetBreakpoint(PBreakpoint(breakpointlist[i]), t);
  finally
    debuggercs.Leave;
  end;
end;

function TDebuggerThread.getOriginalProtectForExceptionBreakpoint(base: ptruint; size: integer): TaccessRights;
//gets the protection of the given page  (Range is in case of overlap and the first page is read only and second page read/write. Make it read/Write then)
var
  bp: PBreakpoint;
  a: ptruint;

  pbase: ptruint;
  totalsize: integer;

  pbase2: ptruint;
  totalsize2: integer;
  mbi: TMEMORYBASICINFORMATION;
  i: integer;
begin
  pbase:=GetPageBase(base);
  totalsize:=(GetPageBase(base+size)-pbase)+$fff;

  //first get the current protection. If a breakpoint is set, it's access rights are less than what is required
  a:=pbase;
  result:=[];
  while (a<pbase+totalsize) and (VirtualQueryEx(processhandle, pointer(a), mbi, sizeof(mbi))=sizeof(mbi)) do
  begin
    result:=result+AllocationProtectToAccessRights(mbi.Protect);
    inc(a, mbi.RegionSize);
  end;

  //now check the breakpointlist
  debuggercs.enter;
  try
    for i:=0 to BreakpointList.Count-1 do
    begin
      bp:=PBreakpoint(breakpointlist[i]);
      if (bp^.active) and (bp^.breakpointMethod=bpmException) then
      begin
        //check if this address falls into this breakpoint range
        pbase2:=getPageBase(bp^.address);
        totalsize2:=(GetPageBase(bp^.address+size)-pbase)+$fff;
        if InRangeX(pbase, pbase2, pbase2+totalsize2) or
           InRangeX(pbase+totalsize2, pbase2, pbase2+totalsize2) then //it's overlapping
          result:=result+bp^.originalaccessrights;
      end;

    end;
  finally
    debuggercs.leave;
  end;
end;

function TDebuggerThread.AdjustAccessRightsWithActiveBreakpoints(ar: TAccessRights; base: ptruint; size: integer): TAccessRights;
var
  i: integer;
  bp:PBreakpoint;
  pbase: ptruint;
  totalsize: integer;

  pbase2: ptruint;
  totalsize2: integer;
begin
  pbase:=GetPageBase(base);
  totalsize:=(GetPageBase(base+size)-pbase)+$fff;
  result:=ar;

  debuggercs.enter;
  try
    for i:=0 to BreakpointList.Count-1 do
    begin
      bp:=PBreakpoint(breakpointlist[i]);
      if (bp^.active) and (bp^.breakpointMethod=bpmException) then
      begin
        //check if this address falls into this breakpoint range
        pbase2:=getPageBase(bp^.address);
        totalsize2:=(GetPageBase(bp^.address+size)-pbase)+$fff;
        if InRangeX(pbase, pbase2, pbase2+totalsize2) or
           InRangeX(pbase+totalsize2, pbase2, pbase2+totalsize2) then //it's overlapping
        begin
          case bp^.breakpointtrigger of
            bptExecute: result:=result-[arExecute];
            bptWrite: result:=result-[arWrite];
            bptAccess:
            begin
              result:=[];
              exit;
            end;
          end;
        end;
      end;

    end;


  finally
    debuggercs.leave;
  end;
end;

function TDebuggerThread.getBestProtectionForExceptionBreakpoint(breakpointtrigger: TBreakpointTrigger; base: ptruint; size: integer): TaccessRights;
//gets the protection required to cause an exception on this page for the wanted access
begin
  result:=[arExecute, arWrite, arRead];

  case breakpointtrigger of
    bptExecute: result:=result-[arExecute];
    bptWrite: result:=result-[arWrite];
    bptAccess:
    begin
      result:=[]; //no need to check, this means nothing can access it
      exit;
    end;
  end;

  //now check if other breakpoints overlap on these pages, and if so, adjust the protection to the one with the least rights (e.g one for execute and one for write leaves only read)
  result:=AdjustAccessRightsWithActiveBreakpoints(result, base, size);
end;




function TDebuggerThread.SetBreakpoint(breakpoint: PBreakpoint; UpdateForOneThread: TDebugThreadHandler=nil): boolean;
{
Will set the breakpoint.
either by setting the appropriate byte in the code to $cc, or setting the appropriate debug registers the thread(s)
}
var
  Debugregistermask: dword;
  ClearMask: dword; //mask used to whipe the original bits from DR7
  newprotect, oldprotect: dword;
  bw: ptruint;
  currentthread: TDebugThreadHandler;
  i: integer;
  AllThreadsAreSet: boolean;

  tid, bptype: integer;
  vpe: boolean;

  PA: int64;

  newdr7: qword;

procedure displayDebugInfo(reason: string);
var debuginfo:tstringlist;
begin
  beep;
  debuginfo:=tstringlist.create;

  if GetCurrentThreadId=MainThreadID then
    debuginfo.Add(rsCalledFromMainThread);

  if getCurrentThreadId=debuggerthread.ThreadID then
    debuginfo.Add(rsCalledFromDebuggerThread);

  if getCurrentThreadId=debuggerthread.ThreadID then
    debuginfo.Add(rsCalledFromAnUnexpectedThread);

  debuginfo.add('action='+breakpointActionToString(breakpoint.breakpointAction));
  debuginfo.add('method='+breakpointMethodToString(breakpoint.breakpointMethod));
  debuginfo.add('trigger='+breakpointTriggerToString(breakpoint.breakpointTrigger));
  debuginfo.add('debugreg='+inttostr(breakpoint.debugRegister));

  debuginfo.add(rsDebuggerthreadIsAtPoint+inttostr(debuggerthread.execlocation));

  Clipboard.AsText:=debuginfo.text;

  MessageBox(0,pchar(rsBreakpointError+reason), pchar(debuginfo.text), MB_OK);

  debuginfo.free;
end;

begin
  //issue: If a breakpoint is being handled and this is called, dr6 gets reset to 0 in windows 7, making it impossible to figure out what caused the breakpoint

  AllThreadsAreSet:=true;

  //debug code to find out why this one gets reactivated
  if (breakpoint^.breakpointAction=bo_FindCode) and (breakpoint^.FoundcodeDialog=nil) then
  begin
    DisplayDebugInfo(rsNoForm);
    result:=false;
    exit;
  end;

  if (breakpoint^.breakpointAction=bo_FindWhatCodeAccesses) and (breakpoint^.frmchangedaddresses=nil) then
  begin
    DisplayDebugInfo(rsNoForm);
    result:=false;
    exit;
  end;


  case breakpoint^.breakpointMethod of
    bpmDebugRegister:
    begin
      //Debug registers

      if CurrentDebuggerInterface is TNetworkDebuggerInterface then
      begin
        //network
        if UpdateForOneThread=nil then
          tid:=-1
        else
          tid:=UpdateForOneThread.ThreadId;

        case breakpoint.breakpointTrigger of
          bptExecute: bptype:=0;
          bptWrite: bptype:=1;
          bptAccess: bptype:=3;
        end;

        result:=networkSetBreakpoint(processhandle, tid, breakpoint.debugRegister, breakpoint.address, bptype, breakpoint.size );
        if result then
          breakpoint^.active := True;
        exit;
      end;


      Debugregistermask := 0;
      outputdebugstring(PChar('1:Debugregistermask=' + inttohex(Debugregistermask, 8)));

      case breakpoint.breakpointTrigger of
        bptWrite: Debugregistermask := $1 or Debugregistermask;
        bptAccess: Debugregistermask := $3 or Debugregistermask;
      end;


      case breakpoint.size of
        2: Debugregistermask := $4 or Debugregistermask;
        4: Debugregistermask := $c or Debugregistermask;
        8: Debugregistermask := $8 or Debugregistermask; //10 is defined as 8 byte
      end;


      outputdebugstring(PChar('2:Debugregistermask=' + inttohex(Debugregistermask, 8)));

      Debugregistermask := (Debugregistermask shl (16 + 4 * breakpoint.debugRegister));
      //set the RWx amd LENx to the proper position
      Debugregistermask := Debugregistermask or (1 shl (breakpoint.debugregister * 2));
      //and set the Lx bit
      Debugregistermask := Debugregistermask or (1 shl 10); //and set bit 10 to 1

      clearmask := (($F shl (16 + 4 * breakpoint.debugRegister)) or (1 shl (breakpoint.debugregister * 2))) xor $FFFFFFFF;
      //create a mask that can be used to undo the old settings

      outputdebugstring(PChar('3:Debugregistermask=' + inttohex(Debugregistermask, 8)));
      outputdebugstring(PChar('clearmask=' + inttohex(clearmask, 8)));

      breakpoint^.active := True;

      if (CurrentDebuggerInterface is TKernelDebugInterface) and globaldebug then
      begin
        //set the breakpoint using globaldebug
        DBKDebug_GD_SetBreakpoint(true, breakpoint.debugregister, breakpoint.address, BreakPointTriggerToBreakType(breakpoint.breakpointTrigger), SizeToBreakLength(breakpoint.size));
      end
      else
      begin
        if (breakpoint.ThreadID <> 0) or (UpdateForOneThread<>nil) then
        begin
          //only one thread
          if updateForOneThread=nil then
            currentthread := getDebugThreadHanderFromThreadID(breakpoint.ThreadID)
          else
            currentthread:=updateForOneThread;

          if currentthread = nil then //thread has been destroyed
            exit;



          currentthread.suspend;
          currentthread.fillContext;

          if CurrentDebuggerInterface is TWindowsDebuggerInterface then
          begin
            if (currentthread.context.Dr6<>0) and (word(currentthread.context.dr6)<>$0ff0) then
            begin
              //the breakpoint in this thread can not be touched yet. Leave it activated
              //(touching the DR registers with setthreadcontext clears DR6 in win7 )
              currentthread.needstocleanup:=true;
              currentthread.resume;
              //currentthread.needstosetbp:=true;
              exit;
            end;
          end;

          if BPOverride or ((byte(currentthread.context.Dr7) and byte(Debugregistermask))=0) then
          begin
            case breakpoint.debugregister of
              0: currentthread.context.Dr0 := breakpoint.address;
              1: currentthread.context.Dr1 := breakpoint.address;
              2: currentthread.context.Dr2 := breakpoint.address;
              3: currentthread.context.Dr3 := breakpoint.address;
            end;
            currentthread.DebugRegistersUsedByCE:=currentthread.DebugRegistersUsedByCE or (1 shl breakpoint.debugregister);
            currentthread.context.Dr7 :=(currentthread.context.Dr7 and clearmask) or Debugregistermask;
            currentthread.setContext;
          end
          else
            AllThreadsAreSet:=false;


          currentthread.resume;
        end
        else
        begin
          //update all threads with the new debug register data

          debuggercs.enter;
          try
            for i := 0 to ThreadList.Count - 1 do
            begin
              currentthread := threadlist.items[i];
              currentthread.suspend;
              currentthread.fillContext;

              if CurrentDebuggerInterface is TWindowsDebuggerInterface then
              begin
                if (currentthread.context.Dr6<>0) and (word(currentthread.context.dr6)<>$0ff0) then
                begin
                  //the breakpoint in this thread can not be touched yet. Leave it activated
                  currentthread.needstocleanup:=true;
                  currentthread.resume;
  //                currentthread.needstosetbp:=true;
                  continue;

                end;
              end;


              if BPOverride or ((byte(currentthread.context.Dr7) and byte(Debugregistermask))=0) then
              begin
                //make sure this bp spot bp is not used
                case breakpoint.debugregister of
                  0: currentthread.context.Dr0 := breakpoint.address;
                  1: currentthread.context.Dr1 := breakpoint.address;
                  2: currentthread.context.Dr2 := breakpoint.address;
                  3: currentthread.context.Dr3 := breakpoint.address;
                end;

                currentthread.DebugRegistersUsedByCE:=currentthread.DebugRegistersUsedByCE or (1 shl breakpoint.debugregister);
                newdr7:= (currentthread.context.Dr7 and clearmask) or Debugregistermask;     ;
                currentthread.context.Dr7 := newdr7;
                currentthread.setContext;
                currentthread.fillContext;
                if currentthread.context.Dr7<>newdr7 then
                begin
                  asm
                  nop
                  end;
                end;
              end
              else
                AllThreadsAreSet:=false;

              currentthread.resume;
            end;

          finally
            debuggercs.leave;
          end;

        end;

      end;
    end;

    bpmInt3:
    begin
      //int3 bp
      breakpoint^.active := True;
      vpe:=(SkipVirtualProtectEx=false) and VirtualProtectEx(processhandle, pointer(breakpoint.address), 1, PAGE_EXECUTE_READWRITE, oldprotect);
      WriteProcessMemory(processhandle, pointer(breakpoint.address), @int3byte, 1, bw);
      if vpe then
        VirtualProtectEx(processhandle, pointer(breakpoint.address), 1, oldprotect, oldprotect);
    end;

    bpmException:
    begin
      //exception bp (slow)

      if assigned(ntsuspendprocess) then
        ntSuspendProcess(processhandle);

      //Make the page(s) unreadable/unwritable based on the option and if other breakpoints are present


      breakpoint^.originalaccessrights:=getOriginalProtectForExceptionBreakpoint(breakpoint.address, breakpoint.size);
      newProtect:=AccessRightsToAllocationProtect(getBestProtectionForExceptionBreakpoint(breakpoint.breakpointTrigger, breakpoint.address, breakpoint.size));

      breakpoint^.active:=true;

      VirtualProtectEx(processhandle, pointer(breakpoint.address), breakpoint.size,newprotect, oldprotect); //throw oldprotect away

      if assigned(ntResumeProcess) then //Q: omg, but what if ntResumeProcess isn't available on the os but suspendprocess is? A:Then buy a new os
        ntResumeProcess(processhandle);
    end;

    bpmDBVM:
    begin
      Log('Setting DBVM Watch Breakpoint');
      loaddbvmifneeded;

      if GetPhysicalAddress(processhandle,pointer(breakpoint^.address),pa) then
      begin
        case breakpoint^.breakpointTrigger of
          bptExecute: breakpoint^.dbvmwatchid:=dbvm_watch_executes(PA,1,EPTO_INTERRUPT,0);
          bptAccess: breakpoint^.dbvmwatchid:=dbvm_watch_reads(PA,1,EPTO_INTERRUPT,0);
          bptWrite: breakpoint^.dbvmwatchid:=dbvm_watch_writes(PA,1,EPTO_INTERRUPT,0);
        end;

        if breakpoint^.dbvmwatchid=-1 then
          raise exception.create('Failure setting a memory watch')
        else
          breakpoint^.active:=true;
      end
      else
        raise exception.create(format('Failure obtaining physical address for %8x',[breakpoint^.address]));
    end;

    //bpmDBVM2: //todo: in case of execute then instead of a no-execute page, make it a cloaked page with an int3 bp

  end;

  result:=AllThreadsAreSet;

end;

procedure TDebuggerThread.UnsetBreakpoint(breakpoint: PBreakpoint; specificContext: PContext=nil; threadid: integer=-1);
var
  Debugregistermask: dword;
  oldprotect: dword;
  bw: PtrUInt;
  ClearMask: dword; //mask used to whipe the original bits from DR7
  currentthread: TDebugThreadHandler;
  i: integer;

  hasoldbp: boolean;

  ar: TAccessRights;

  tid: integer;
  vpe: boolean;
begin

  if breakpoint^.breakpointMethod = bpmDebugRegister then
  begin
    //debug registers
    if CurrentDebuggerInterface is TNetworkDebuggerInterface then
    begin
      //network
      NetworkRemoveBreakpoint(processhandle, threadid, breakpoint.debugRegister, BreakPointTriggerIsWatchpoint(breakpoint.breakpointTrigger));
      if threadid=-1 then
        breakpoint.active:=false;

      exit;
    end;



    Debugregistermask := $F shl (16 + 4 * breakpoint.debugRegister) + (3 shl (breakpoint.debugregister * 2));
    Debugregistermask := not Debugregistermask; //inverse the bits


    if (CurrentDebuggerInterface is TKernelDebugInterface) and globaldebug then
    begin
      DBKDebug_GD_SetBreakpoint(false, breakpoint.debugregister, breakpoint.address, BreakPointTriggerToBreakType(breakpoint.breakpointTrigger), SizeToBreakLength(breakpoint.size));
    end
    else
    begin
      if (specificContext<>nil) then
      begin


        case breakpoint.debugregister of
          0: specificContext.Dr0 := 0;
          1: specificContext.Dr1 := 0;
          2: specificContext.Dr2 := 0;
          3: specificContext.Dr3 := 0;
        end;
        specificContext.Dr7 := (specificContext.Dr7 and Debugregistermask);
      end
      else
      if breakpoint.ThreadID <> 0 then
      begin
        //only one thread
        breakpoint.active:=false;

        currentthread := getDebugThreadHanderFromThreadID(breakpoint.ThreadID);
        if currentthread = nil then //it's gone
          exit;

        currentthread.suspend;
        currentthread.fillContext;

        if CurrentDebuggerInterface is TWindowsDebuggerInterface then
        begin
          if (currentthread.context.Dr6<>0) and (word(currentthread.context.dr6)<>$0ff0) then
          begin
            //the breakpoint in this thread can not be deactivated yet. Leave it activated
            //(touching the DR registers with setthreadcontext clears DR6 in win7 )
            currentthread.needstocleanup:=true;
            currentthread.resume;


            exit;
          end;
        end;

        //check if this breakpoint was set in this thread
        if (BPOverride) or ((currentthread.DebugRegistersUsedByCE and (1 shl breakpoint.debugregister))>0) then
        begin
          currentthread.DebugRegistersUsedByCE:=currentthread.DebugRegistersUsedByCE and (not (1 shl breakpoint.debugregister));

          case breakpoint.debugregister of
            0: currentthread.context.Dr0 := 0;
            1: currentthread.context.Dr1 := 0;
            2: currentthread.context.Dr2 := 0;
            3: currentthread.context.Dr3 := 0;
          end;
          currentthread.context.Dr7 := (currentthread.context.Dr7 and Debugregistermask);
          currentthread.setContext;

        end;
        currentthread.resume;
      end
      else
      begin
        //do all threads
        begin
          for i := 0 to ThreadList.Count - 1 do
          begin
            currentthread := threadlist.items[i];
            currentthread.suspend;
            currentthread.fillContext;

            if CurrentDebuggerInterface is TWindowsDebuggerInterface then
            begin
              if (currentthread.context.Dr6<>0) and (word(currentthread.context.dr6)<>$0ff0) then
              begin
                //the breakpoint in this thread can not be deactivated yet. Leave it activated
                //(touching the DR registers with setthreadcontext clears DR6 in win7 )
                currentthread.needstocleanup:=true;
                currentthread.resume;
                continue;

              end;
            end;


            hasoldbp:=false; //now check if this thread actually has the breakpoint set (and not replaced or never even set)

            if (BPOverride) or ((currentthread.DebugRegistersUsedByCE and (1 shl breakpoint.debugregister))>0) then
            begin
              currentthread.DebugRegistersUsedByCE:=currentthread.DebugRegistersUsedByCE and (not (1 shl breakpoint.debugregister));

              case breakpoint.debugregister of
                0:
                begin
                  hasoldbp:=currentthread.context.Dr0=breakpoint.address;
                  if hasoldbp then
                    currentthread.context.Dr0 := 0;
                end;

                1:
                begin
                  hasoldbp:=currentthread.context.Dr1=breakpoint.address;
                  if hasoldbp then
                    currentthread.context.Dr1 := 0;
                end;

                2:
                begin
                  hasoldbp:=currentthread.context.Dr2=breakpoint.address;
                  if hasoldbp then
                    currentthread.context.Dr2 := 0;
                end;

                3:
                begin
                  hasoldbp:=currentthread.context.Dr3=breakpoint.address;
                  if hasoldbp then
                    currentthread.context.Dr3 := 0;
                end;
              end;

              if hasoldbp then
              begin
                currentthread.context.Dr7 := (currentthread.context.Dr7 and Debugregistermask);
                currentthread.setcontext;
              end;


            end;
            currentthread.resume;
          end;

        end;
      end;

    end;

  end
  else
  if breakpoint^.breakpointMethod=bpmInt3 then
  begin
    vpe:=(SkipVirtualProtectEx=false) and VirtualProtectEx(processhandle, pointer(breakpoint.address), 1, PAGE_EXECUTE_READWRITE, oldprotect);
    WriteProcessMemory(processhandle, pointer(breakpoint.address), @breakpoint.originalbyte, 1, bw);
    if vpe then
      VirtualProtectEx(processhandle, pointer(breakpoint.address), 1, oldprotect, oldprotect);
  end
  else
  if breakpoint^.breakpointMethod=bpmException then
  begin
    //check if there are other exception breakpoints
    if assigned(ntsuspendProcess) then
      ntSuspendProcess(ProcessHandle);

    breakpoint^.active := False;

    ar:=[arExecute, arRead, arWrite];
    ar:=AdjustAccessRightsWithActiveBreakpoints(ar, breakpoint^.address, breakpoint^.size);
    if ar=[arExecute, arRead, arWrite] then
      ar:=breakpoint^.originalaccessrights;

    VirtualProtectEx(processhandle, pointer(breakpoint^.address), breakpoint^.size, AccessRightsToAllocationProtect(ar), oldprotect);


    if assigned(ntResumeProcess) then
      ntResumeProcess(ProcessHandle);


  end
  else
  if breakpoint^.breakpointMethod=bpmDBVM then
    dbvm_watch_delete(breakpoint^.dbvmwatchid);

  breakpoint^.active := false;
end;

procedure TDebuggerThread.RemoveBreakpoint(breakpoint: PBreakpoint);
var
  i,j: integer;
  bp: PBreakpoint;
begin
  debuggercs.enter;
  try
    outputdebugstring('RemoveBreakpoint');
    outputdebugstring(PChar('breakpointlist.Count=' + IntToStr(breakpointlist.Count)));

    while breakpoint.owner <> nil do //it's a child, but we need the owner
      breakpoint := breakpoint.owner;



    //clean up all it's children
    for j:=0 to breakpointlist.Count-1 do
    begin
      BP := breakpointlist.items[j];
      if bp^.owner = breakpoint then
      begin
        UnsetBreakpoint(bp);
        bp^.deletecountdown:=10; //10*100=1000=1 second
        bp^.markedfordeletion := True; //set this flag so it gets deleted on next no-event
        bp^.deletetickcount:=GetTickCount;


      end
    end;

    //and finally itself
    //set this flag so it gets deleted on next no-event
    UnsetBreakpoint(breakpoint);


    breakpoint^.deletecountdown:=10;
    breakpoint^.markedfordeletion := True;
    breakpoint^.deletetickcount:=GetTickCount;




    OutputDebugString('Disabled the breakpoint');
  finally
    debuggercs.leave;
  end;

  if frmBreakpointlist<>nil then
    postmessage(frmBreakpointlist.handle, WM_BPUPDATE,0,0); //tell the breakpointlist that there's been an update
end;

function TDebuggerThread.AddBreakpoint(owner: PBreakpoint; address: uint_ptr; size: integer; bpt: TBreakpointTrigger; bpm: TBreakpointMethod; bpa: TBreakpointAction; debugregister: integer=-1; foundcodedialog: Tfoundcodedialog=nil; threadID: dword=0; frmchangedaddresses: Tfrmchangedaddresses=nil; FrmTracer: TFrmTracer=nil; tcount: integer=0; changereg: pregistermodificationBP=nil; OnBreakpoint: TBreakpointEvent=nil): PBreakpoint;
var
  newbp: PBreakpoint;
  originalbyte: byte;
  x: PtrUInt;
  i: integer;
  count: integer;
begin


  if bpm=bpmInt3 then
  begin
    if dbcSoftwareBreakpoint in CurrentDebuggerInterface.DebuggerCapabilities then
    begin
      if not ReadProcessMemory(processhandle, pointer(address), @originalbyte,
        1, x) then raise exception.create(rsUnreadableAddress);
    end else raise exception.create(Format(
      rsDebuggerInterfaceDoesNotSupportSoftwareBreakpoints, [
      CurrentDebuggerInterface.name]));

  end
  else
  if bpm=bpmDebugRegister then
  begin

    if (debugregister<0) or (debugregister>=GetMaxBreakpointCountForThisType(bpt)) then raise exception.create(
      rsAddBreakpointAnInvalidDebugRegisterIsUsed);
  end
  else
  if bpm=bpmDBVM then
  begin
    if dbcDBVMBreakpoint in CurrentDebuggerInterface.DebuggerCapabilities then //only kernelmode debugger (perhaps the other ones in the future, for execute only as it's just an unexpected single step)
    begin
      if not ReadProcessMemory(processhandle, pointer(address), @originalbyte,
        1, x) then raise exception.create(rsUnreadableAddress);
    end else raise exception.create(Format(
      rsDebuggerInterfaceDoesNotSupportDBVMBreakpoints, [
      CurrentDebuggerInterface.name]));

  end;

  getmem(newbp, sizeof(TBreakPoint));
  ZeroMemory(newbp, sizeof(TBreakPoint));
  newbp^.owner := owner;
  newbp^.address := address;
  newbp^.size := size;
  newbp^.originalbyte := originalbyte;
  newbp^.breakpointTrigger := bpt;
  newbp^.breakpointMethod := bpm;
  newbp^.breakpointAction := bpa;
  newbp^.debugRegister := debugregister;

  newbp^.foundcodedialog := foundcodedialog;
  newbp^.ThreadID := threadID;
  newbp^.frmchangedaddresses := frmchangedaddresses;
  newbp^.frmTracer:=frmtracer;
  newbp^.tracecount:=tcount;
  newbp^.OnBreakpoint:=OnBreakpoint;
  if changereg<>nil then
    newbp^.changereg:=changereg^;


  debuggercs.enter;
  try
    //add to the bp list
    BreakpointList.Add(newbp);
    //apply this breakpoint

    SetBreakpoint(newbp);
  finally
    debuggercs.leave;
  end;



  Result := newbp;

  if frmBreakpointlist<>nil then
    postmessage(frmBreakpointlist.handle, WM_BPUPDATE,0,0); //tell the breakpointlist that there's been an update
end;

procedure TDebuggerThread.GetBreakpointList(address: uint_ptr; size: integer; var bplist: TBreakpointSplitArray);
{
splits up the given address and size into a list of debug register safe breakpoints (alligned)
Do not confuse this with a function that returns all breakpoints urrently set
}
var
  i: integer;
begin
  while size > 0 do
  begin
    if (processhandler.is64bit) and (size >= 8) then
    begin
      if (address mod 8) = 0 then
      begin
        setlength(bplist, length(bplist) + 1);
        bplist[length(bplist) - 1].address := address;
        bplist[length(bplist) - 1].size := 8;
        Inc(address, 8);
        Dec(size, 8);
      end
      else
      if (address mod 4) = 0 then
      begin
        setlength(bplist, length(bplist) + 1);
        bplist[length(bplist) - 1].address := address;
        bplist[length(bplist) - 1].size := 4;
        Inc(address, 4);
        Dec(size, 4);
      end
      else
      if (address mod 2) = 0 then
      begin
        setlength(bplist, length(bplist) + 1);
        bplist[length(bplist) - 1].address := address;
        bplist[length(bplist) - 1].size := 2;
        Inc(address, 2);
        Dec(size, 2);
      end
      else
      begin
        setlength(bplist, length(bplist) + 1);
        bplist[length(bplist) - 1].address := address;
        bplist[length(bplist) - 1].size := 1;
        Inc(address);
        Dec(size);
      end;

    end
    else
    if size >= 4 then //smaller than 8 bytes or not a 64-bit process
    begin
      if (address mod 4) = 0 then
      begin
        setlength(bplist, length(bplist) + 1);
        bplist[length(bplist) - 1].address := address;
        bplist[length(bplist) - 1].size := 4;
        Inc(address, 4);
        Dec(size, 4);
      end
      else    //not aligned on a 4 byte boundary
      if (address mod 2) = 0 then
      begin
        setlength(bplist, length(bplist) + 1);
        bplist[length(bplist) - 1].address := address;
        bplist[length(bplist) - 1].size := 2;
        Inc(address, 2);
        Dec(size, 2);
      end
      else
      begin
        //also not aligned on a 2 byte boundary, so use a 1 byte bp
        setlength(bplist, length(bplist) + 1);
        bplist[length(bplist) - 1].address := address;
        bplist[length(bplist) - 1].size := 1;
        Inc(address);
        Dec(size);
      end;
    end
    else
    if size >= 2 then
    begin
      if (address mod 2) = 0 then
      begin
        setlength(bplist, length(bplist) + 1);
        bplist[length(bplist) - 1].address := address;
        bplist[length(bplist) - 1].size := 2;
        Inc(address, 2);
        Dec(size, 2);
      end
      else
      begin
        //not aligned on a 2 byte boundary, so use a 1 byte bp
        setlength(bplist, length(bplist) + 1);
        bplist[length(bplist) - 1].address := address;
        bplist[length(bplist) - 1].size := 1;
        Inc(address);
        Dec(size);
      end;
    end
    else
    if size >= 1 then
    begin
      setlength(bplist, length(bplist) + 1);
      bplist[length(bplist) - 1].address := address;
      bplist[length(bplist) - 1].size := 1;
      Inc(address);
      Dec(size);
    end;
  end;
end;

function TDebuggerThread.DoBreakpointTriggersUseSameDebugRegisterKind(bpt1: TBreakpointTrigger; bpt2: TBreakpointTrigger): boolean;
{
Check if the two breakpoint triggers would make use of the same kind of debug register
}
begin
  if CurrentDebuggerInterface.maxSharedBreakpointCount>0 then //breakpoint resources are shared, so yes
    result:=true
  else //not shared but split. Check if it's a watchpoint or instruction
    result:=BreakPointTriggerIsWatchpoint(bpt1)=BreakPointTriggerIsWatchpoint(bpt2);  //false=false returs true:true=true returns true:true=false returns false:false=true resturns false
end;

function TDebuggerThread.GetMaxBreakpointCountForThisType(breakpointTrigger: TBreakpointTrigger): integer;
{
Returns the number of breakpoints the current debuiggerinterface can handle for the given breakpoint trigger
}
begin
  if CurrentDebuggerInterface.maxSharedBreakpointCount>0 then
    result:=CurrentDebuggerInterface.maxSharedBreakpointCount
  else
  begin
    if breakpointTrigger=bptExecute then
      result:=CurrentDebuggerInterface.maxInstructionBreakpointCount
    else
      result:=CurrentDebuggerInterface.maxWatchpointBreakpointCount;
  end;
end;

function TDebuggerThread.GetUsableDebugRegister(breakpointTrigger: TBreakpointTrigger): integer;
{
will scan the current breakpoint list and see which debug register is unused.
if all are used up, return -1
}
var
  i: integer;
  available: array of boolean;

  maxBreakpointCountForThisType: integer;
begin
  Result := -1;
  maxBreakpointCountForThisType:=GetMaxBreakpointCountForThisType(breakpointtrigger);
  if maxBreakpointCountForThisType<=0 then
    exit;

  setlength(available, maxBreakpointCountForThisType);
  for i := 0 to maxBreakpointCountForThisType-1 do
    available[i] := True;


  debuggercs.enter;
  try
    for i := 0 to breakpointlist.Count - 1 do
    begin
      if (pbreakpoint(breakpointlist.Items[i])^.breakpointMethod = bpmDebugRegister) and //debug register bp
        (pbreakpoint(breakpointlist.Items[i])^.active) and //active
        (pbreakpoint(breakpointlist.Items[i])^.ThreadID=0) and //not a thread specific bp
        (DoBreakpointTriggersUseSameDebugRegisterKind(pbreakpoint(breakpointlist.Items[i])^.breakpointTrigger, breakpointtrigger)) //same breakpoint pool as used here
      then
        available[pbreakpoint(breakpointlist.Items[i])^.debugRegister] := False;

    end;

    for i := 0 to maxBreakpointCountForThisType-1 do
      if available[i] then
      begin
        Result := i;
        break;
      end;

  finally
    debuggercs.leave;
  end;

end;

procedure TDebuggerthread.FindWhatWrites(address: uint_ptr; size: integer; breakpointmethod: TBreakpointMethod);
begin
  if size>0 then
    FindCodeByBP(address, size, bptWrite, breakpointmethod);
end;


procedure TDebuggerthread.FindWhatWrites(address: uint_ptr; size: integer);
begin
  if size>0 then
    FindCodeByBP(address, size, bptWrite, preferedBreakpointMethod);
end;

procedure TDebuggerthread.FindWhatAccesses(address: uint_ptr; size: integer; breakpointmethod: TBreakpointMethod);
begin
  if size>0 then
    FindCodeByBP(address, size, bptAccess, breakpointmethod);
end;

procedure TDebuggerthread.FindWhatAccesses(address: uint_ptr; size: integer);
begin
  if size>0 then
    FindCodeByBP(address, size, bptAccess, preferedBreakpointMethod);
end;

procedure TDebuggerthread.FindCodeByBP(address: uint_ptr; size: integer; bpt: TBreakpointTrigger; breakpointmethod: TBreakpointmethod);
var
  usedDebugRegister: integer;
  bplist: array of TBreakpointSplit;
  newbp: PBreakpoint;
  i: integer;

  foundcodedialog: TFoundcodeDialog;
begin
  if size=0 then exit;

  //split up address and size into memory alligned sections


  setlength(bplist, 0);
  usedDebugRegister:=-1;
  if breakpointmethod=bpmDebugRegister then
  begin
    GetBreakpointList(address, size, bplist);

    usedDebugRegister := GetUsableDebugRegister(bpt);
    if usedDebugRegister = -1 then
      raise Exception.Create(
        rsAll4DebugRegistersAreCurrentlyUsedUpFreeOneAndTryA);

    address:=bplist[0].address;
    size:=bplist[0].size;
  end;

  //still here
  //create a foundcodedialog and add the breakpoint
  foundcodedialog := Tfoundcodedialog.Create(application);
  case bpt of
    bptAccess : foundcodedialog.Caption:=Format(rsTheFollowingOpcodesAccessed, [inttohex(address, 8)]);
    bptWrite : foundcodedialog.Caption:=Format(rsTheFollowingOpcodesWriteTo, [inttohex(address, 8)]);
  end;
  foundcodedialog.addresswatched:=address;
  foundcodedialog.Show;

  newbp := AddBreakpoint(nil, address, size, bpt, breakpointmethod,
    bo_FindCode, usedDebugRegister,  foundcodedialog, 0);


  if length(bplist) > 1 then
  begin
    for i := 1 to length(bplist) - 1 do
    begin
      usedDebugRegister := GetUsableDebugRegister(bpt);
      if usedDebugRegister = -1 then
        exit; //at least one has been set, so be happy...

      AddBreakpoint(newbp, bplist[i].address, bplist[i].size, bpt, breakpointmethod, bo_FindCode, usedDebugRegister, foundcodedialog, 0);
    end;
  end;
end;

procedure TDebuggerthread.FindCodeByBP(address: uint_ptr; size: integer; bpt: TBreakpointTrigger);
var method: TBreakpointMethod;
begin
  method:=preferedBreakpointMethod;
  if method=bpmint3 then //not possible for this
    method:=bpmDebugRegister;

  FindCodeByBP(address,size,bpt,method);
end;

function TDebuggerThread.stopBreakAndTrace(frmTracer: TFrmTracer): boolean;
var
  i: integer;
  bp: PBreakpoint;
begin
  Result := False;
  debuggercs.enter;
  try
    for i := 0 to BreakpointList.Count - 1 do
      if (not PBreakpoint(breakpointlist[i]).markedfordeletion) and (PBreakpoint(breakpointlist[i]).frmTracer = frmTracer) then
      begin
        bp := PBreakpoint(breakpointlist[i]);
        Result := True;
        break;
      end;

    if Result then
      RemoveBreakpoint(bp); //unsets and removes all breakpoints that belong to this

    for i := 0 to BreakpointList.Count - 1 do
      if (not PBreakpoint(breakpointlist[i]).markedfordeletion) and (PBreakpoint(breakpointlist[i]).isTracerStepOver) then
        RemoveBreakpoint(PBreakpoint(breakpointlist[i]));

    for i:=0 to ThreadList.Count-1 do
      TDebugThreadHandler(ThreadList[i]).TracerQuit;

  finally
    debuggercs.leave;
  end;

  //it doesn't really matter if it returns false, that would just mean the breakpoint got and it's tracing or has finished tracing
end;

procedure TDebuggerThread.startBranchMapper(tidlist: TList=nil);
var
  i,j: integer;
  currentthread: TDebugThreadHandler;
  tl: TList;
  pid: dword;
begin
  debuggercs.enter;
  try
    if tidlist<>nil then
    begin
      for i := 0 to tidlist.Count - 1 do
      begin
        pid:=dword(tidlist.items[i]);
        currentthread := getDebugThreadHanderFromThreadID(pid);
        if currentthread<>nil then
          currentthread.StartBranchMap;
      end;
    end
    else
    begin
      for i:=0 to ThreadList.count-1 do
        TDebugThreadHandler(threadlist[i]).StartBranchMap;
    end;


  finally
    debuggercs.leave;
  end;
end;

procedure TDebuggerThread.stopBranchMapper;
var i: integer;
begin
  for i:=0 to ThreadList.count-1 do
    TDebugThreadHandler(threadlist[i]).StopBranchMap;
end;

function TDebuggerThread.CodeFinderStop(codefinder: TFoundCodeDialog): boolean;
var
  i: integer;
  bp: PBreakpoint;
begin
  Result := False;


  debuggercs.enter;
  try
    for i := 0 to BreakpointList.Count - 1 do
      if (not PBreakpoint(breakpointlist[i]).markedfordeletion) and (PBreakpoint(breakpointlist[i]).FoundcodeDialog = codefinder) then
      begin
        bp := PBreakpoint(breakpointlist[i]);

        Result := True;
        break;
      end;

    if Result then
    begin
      RemoveBreakpoint(bp); //unsets and removes all breakpoints that belong to this
    end;

  finally
    debuggercs.leave;
  end;
end;


function TDebuggerthread.FindWhatCodeAccessesStop(frmchangedaddresses: Tfrmchangedaddresses): boolean;
var
  i: integer;
  bp: PBreakpoint;
begin
  if self=nil then exit;

  Result := False;
  debuggercs.enter;
  try
    for i := 0 to BreakpointList.Count - 1 do
      if (not PBreakpoint(breakpointlist[i]).markedfordeletion) and (PBreakpoint(breakpointlist[i]).frmchangedaddresses = frmchangedaddresses) then
      begin
        bp := PBreakpoint(breakpointlist[i]);
        Result := True;
        break;
      end;

    if Result then
    begin
      RemoveBreakpoint(bp); //unsets and removes all breakpoints that belong to this
      bp^.frmchangedaddresses:=nil;
    end;
  finally
    debuggercs.leave;
  end;
end;

function TDebuggerthread.setChangeRegBreakpoint(regmod: PRegisterModificationBP): PBreakpoint;
var
  method: TBreakpointMethod;
  useddebugregister: integer;
  address: ptruint;
  bp: pbreakpoint;
begin
  result:=nil;

  address:=regmod^.address;
  bp:=isBreakpoint(address);

  if bp<>nil then
    RemoveBreakpoint(bp);


  method:=preferedBreakpointMethod;
  usedDebugRegister:=-1;
  if method=bpmDebugRegister then
  begin
    usedDebugRegister := GetUsableDebugRegister(bptExecute);
    if usedDebugRegister = -1 then
    begin
      if MessageDlg(
        rsAllDebugRegistersAreUsedUpDoYouWantToUseASoftwareBP, mtConfirmation, [
          mbNo, mbYes], 0) = mrYes then
        method := bpmInt3
      else
        exit;

    end;
  end;

  //todo: Make this breakpoint show up in the memory view
  result:=AddBreakpoint(nil, regmod.address, 1, bptExecute, method, bo_ChangeRegister, usedDebugRegister, nil, 0, nil,nil,0, regmod);


end;

procedure TDebuggerthread.setBreakAndTraceBreakpoint(frmTracer: TFrmTracer; address: ptrUint; BreakpointTrigger: TBreakpointTrigger; breakpointmethod: TBreakpointmethod; bpsize: integer; count: integer; startcondition:string=''; stopcondition:string=''; stepover: boolean=false; nosystem: boolean=false);
var
  useddebugregister: integer;
  bp,bpsecondary: PBreakpoint;
  bplist: TBreakpointSplitArray;
  i: integer;
begin
  debuggercs.enter;
  try
    setlength(bplist,0);

    if breakpointmethod=bpmDebugRegister then
    begin
      GetBreakpointList(address, bpsize, bplist);

      address:=bplist[0].address;
      bpsize:=bplist[0].size;


      usedDebugRegister := GetUsableDebugRegister(breakpointtrigger);
      if usedDebugRegister = -1 then
      begin
        if (BreakpointTrigger=bptExecute) then
        begin
          if MessageDlg(
            rsAllDebugRegistersAreUsedUpDoYouWantToUseASoftwareBP,
              mtConfirmation, [mbNo, mbYes], 0) = mrYes then
            breakpointmethod := bpmInt3
          else
            exit;
        end
        else
          messagedlg(rsAllDebugRegistersAreUsedUp, mtError, [mbok], 0);

      end;
    end;

    if startcondition<>'' then
    begin
      if assigned(ntSuspendProcess) then
        ntSuspendProcess(processhandle);
    end;

    bp:=AddBreakpoint(nil, address, bpsize, BreakpointTrigger, breakpointmethod, bo_BreakAndTrace, usedDebugRegister,  nil, 0, nil,frmTracer,count);

    if startcondition<>'' then
    begin
      if bp<>nil then
        setbreakpointcondition(bp, true, startcondition);

      if assigned(ntResumeProcess) then
        ntResumeProcess(processhandle);
    end;

    if bp<>nil then
    begin
      bp^.traceendcondition:=strnew(pchar(stopcondition));
      bp^.traceStepOver:=stepover;
      bp^.traceNosystem:=nosystem;
    end;


    for i:=1 to length(bplist)-1 do
    begin
      useddebugregister:=GetUsableDebugRegister(breakpointtrigger);
      if useddebugregister=-1 then exit;

      bpsecondary:=AddBreakpoint(bp, bplist[i].address, bplist[i].size, BreakpointTrigger, breakpointmethod, bo_BreakAndTrace, usedDebugregister,  nil, 0, nil,frmTracer,count);
      bpsecondary.traceendcondition:=strnew(pchar(stopcondition));
      bpsecondary.traceStepOver:=stepover;
      bpsecondary.traceNosystem:=nosystem;
    end;


  finally
    debuggercs.leave;
  end;
end;

function TDebuggerthread.FindWhatCodeAccesses(address: uint_ptr; foundCodeDialog:TFoundCodeDialog=nil): tfrmChangedAddresses;
var
  method: TBreakpointMethod;
  frmChangedAddresses: tfrmChangedAddresses;
  useddebugregister: integer;
  i: integer;
  s: string;
  tempaddress: ptruint;
begin
  result:=nil;
  if foundCodeDialog<>nil then  //this is linked to a foundcode dialog
    method:=bpmInt3
  else
    method:=preferedBreakpointMethod;

  usedDebugRegister:=-1;
  if method=bpmDebugRegister then
  begin
    usedDebugRegister := GetUsableDebugRegister(bptExecute);
    if usedDebugRegister = -1 then
    begin
      if MessageDlg(
        rsAllDebugRegistersAreUsedUpDoYouWantToUseASoftwareBP, mtConfirmation, [
          mbNo, mbYes], 0) = mrYes then
        method := bpmInt3
      else
        exit;

    end;
  end;

  frmchangedaddresses:=tfrmChangedAddresses.Create(application) ;
  frmchangedaddresses.address:=address;

  tempaddress:=address;
  s:=disassemble(tempaddress); //tempaddress gets changed by this, so don't use the real one

  if defaultDisassembler.LastDisassembleData.isfloat then
    frmchangedaddresses.cbDisplayType.ItemIndex:=3;

  if defaultDisassembler.LastDisassembleData.isfloat64 then
    frmchangedaddresses.cbDisplayType.ItemIndex:=4;


  if uppercase(defaultDisassembler.LastDisassembleData.opcode)='RET' then
  begin
    if processhandler.is64Bit then
      s:='[RSP]'
    else
      s:='[ESP]';
  end
  else
  begin
    i:=pos('[',s)+1;
    if i<>0 then
      s:=copy(s,i,pos(']',s)-i)
    else
    begin
      //no [   ] part
      if processhandler.is64Bit then
        s:='RDI'
      else
        s:='EDI';
    end;
  end;


  frmchangedaddresses.equation:=s; //so no need to disassemble every single time...
  frmchangedaddresses.FoundCodeDialog:=foundCodeDialog;

  if foundcodedialog=nil then
    frmchangedaddresses.show;

  AddBreakpoint(nil, address, 1, bptExecute, method, bo_FindWhatCodeAccesses, usedDebugRegister, nil, 0, frmchangedaddresses);


  result:=frmChangedAddresses;
end;

procedure TDebuggerthread.setbreakpointcondition(bp: PBreakpoint; easymode: boolean; script: string);
begin
  debuggercs.enter;

  try
    if bp^.conditonalbreakpoint.script<>nil then
      StrDispose(bp^.conditonalbreakpoint.script);

    bp^.conditonalbreakpoint.script:=strnew(pchar(script));
    bp^.conditonalbreakpoint.easymode:=easymode;
  finally
    debuggercs.leave;
  end;

end;

function TDebuggerthread.getbreakpointcondition(bp: PBreakpoint; var easymode: boolean):pchar;
begin
  debuggercs.enter;
  result:=bp^.conditonalbreakpoint.script;
  easymode:=bp^.conditonalbreakpoint.easymode;
  debuggercs.leave;
end;



procedure TDebuggerThread.getBreakpointAddresses(var AddressList: TAddressArray);
var i: integer;
begin
  setlength(AddressList,0);
  debuggercs.enter;
  setlength(addresslist, BreakpointList.count);
  for i:=0 to BreakpointList.count-1 do
    addresslist[i]:=PBreakpoint(BreakpointList[i])^.address;

  debuggercs.leave;
end;


procedure TDebuggerthread.updatebplist(lv: TListview; showshadow: boolean);
{
Only called by the breakpointlist form running in the main thread. It's called after the WM_BPUPDATE is sent to the breakpointlist window
}
var
  i: integer;
  li: TListitem;
  bp: PBreakpoint;
  s: string;

  showcount: integer;
  selindex: integer;
begin
  if lv.Selected<>nil then
    selindex:=lv.selected.index
  else
    selindex:=-1;

  lv.items.Clear;

  debuggercs.enter;


  showcount:=0;
  for i := 0 to BreakpointList.Count - 1 do
  begin
    bp:=PBreakpoint(BreakpointList[i]);

    if bp^.active or showshadow then
    begin
      inc(showcount);

      if i<lv.Items.Count then
        li:=lv.items[i]
      else
        li:=lv.items.add;

      li.data:=bp;
      li.Caption:=inttohex(bp^.address,8);
      li.SubItems.Clear;

      li.SubItems.add(inttostr(bp^.size));
      li.SubItems.Add(breakpointTriggerToString(bp^.breakpointTrigger));
      s:=breakpointMethodToString(bp^.breakpointMethod);
      if bp^.breakpointMethod=bpmDebugRegister then
        s:=s+' ('+inttostr(bp^.debugRegister)+')';

      li.SubItems.Add(s);


      li.SubItems.Add(breakpointActionToString(bp^.breakpointAction));
      li.SubItems.Add(BoolToStr(bp^.active, rsYes, rsNo));
      if bp^.markedfordeletion then
        li.SubItems.Add(rsYes+' ('+inttostr(bp^.deletecountdown)+')');
    end;
  end;
            {
  for i:=lv.items.count-1 downto showcount do
    lv.items[i].Delete;    }

  if selindex>=lv.items.count then
    selindex:=lv.items.count-1;

  if (selindex<>-1) then
    lv.Selected:=lv.Items[selindex]
  else
    lv.selected:=nil;

  if lv.selected<>nil then
    lv.Selected.MakeVisible(false);


  debuggercs.leave;
end;

procedure TDebuggerthread.SetEntryPointBreakpoint;
{Only called from the main thread, or synchronize}
var code,data: ptruint;
  bp: PBreakpoint;
  oldstate: TBreakpointMethod;
begin
  OutputDebugString('SetEntryPointBreakpoint called');
  if fNeedsToSetEntryPointBreakpoint then
  begin
    fNeedsToSetEntryPointBreakpoint:=false;

    OutputDebugString('Initializing symbol handler');
    symhandler.reinitialize(true);

    OutputDebugString('Waiting for symbols loaded');
    symhandler.waitforsymbolsloaded(true);

    OutputDebugString('Fetching entrypoint');
    memorybrowser.GetEntryPointAndDataBase(code,data);

    //set the breakpoint preference to int3 for this breakpoint
    oldstate:=preferedBreakpointMethod;
    preferedBreakpointMethod:=bpmInt3;

    OutputDebugString('Going to toggle bp');

    try
      bp:=ToggleOnExecuteBreakpoint(code, bpmInt3);

      if bp<>nil then
        bp^.OneTimeOnly:=true;
    finally
      preferedBreakpointMethod:=oldstate;
    end;

  end;
end;

function TDebuggerthread.SetOnExecuteBreakpoint(address: ptrUint; askforsoftwarebp: boolean = false; tid: dword=0; OnBreakpoint: TBreakpointEvent=nil): PBreakpoint;
begin
  result:=SetOnExecuteBreakpoint(address, preferedBreakpointMethod, askforsoftwarebp, tid, OnBreakpoint);
end;

procedure TDebuggerThread.setDepPolicy;
begin
  LUA_DoScript('executeCodeEx(0,nil,''SetProcessDEPPolicy'',3)');
  hasSetDEPPolicy:=true;
end;

function TDebuggerthread.SetOnExecuteBreakpoint(address: ptrUint; bpm: TBreakpointMethod; askforsoftwarebp: boolean = false; tid: dword=0; OnBreakpoint: TBreakpointEvent=nil): PBreakpoint;
var
  i: integer;
  found: boolean;
  originalbyte: byte;
  oldprotect: dword;
  bw, br: PtrUInt;

  usableDebugReg: integer;

  depflags: dword;
  perm: BOOL;

  timeout: qword;

  ph: THandle;

begin
  found := False;

  result:=nil;
  if bpm=bpmException then
  begin
    if not processhandler.is64Bit then
    begin
      //32 bit: check if it has noexecute support
      if assigned(GetProcessDEPPolicy) and assigned(SetProcessDEPPolicy) then
      begin
        ph:=OpenProcess(PROCESS_ALL_ACCESS,false,processid);    //the debuggerhandle does not get this properly
        try
          if GetProcessDEPPolicy(ph, @depflags, @perm) then
          begin
            if (depflags and 1=0) then
            begin
              //dep is not on
              if perm then raise exception.create(rsCantEnableDEP);
              depflags:=depflags or 3;

              if MessageDlg(rsAskToEnableNX, mtConfirmation, [mbYes, mbNo], 0)=mrno then exit;

              hasSetDEPPolicy:=false;
              ExecuteInThread(SetDEPPolicy);

              timeout:=GetTickCount64+5000;
              while (hasSetDEPPolicy=false) and (gettickcount64<timeout) do
              begin
                if GetCurrentThreadId=MainThreadID then
                  CheckSynchronize(50)
                else
                  sleep(50);
              end;

              if (hasSetDEPPolicy=false) and (messagedlg(rsDepSettingTimeout, mtWarning, [mbyes, mbno], 0, mbno)=mrno) then exit;

              if GetProcessDEPPolicy(ph, @depflags, @perm) then
              begin
                if (depflags and 1=0) then
                begin
                  if perm then
                    raise exception.create(rsFailedDEPPermanently)
                  else
                    raise exception.create(rsFailedDEP);
                end;
              end
              else raise exception.create(rsProcessSucksNoDEPSupport);
            end;
          end
          else
            raise exception.create(rsFailureGettingDEPInformation);

        finally
          if (ph<>0) and (ph<>INVALID_HANDLE_VALUE) then
            closehandle(ph);
        end;
      end
      else
        raise exception.create(rsNoExecutePageExceptionsForYou);
    end;
  end;

  debuggercs.enter;
  try
    //set the breakpoint


    if bpm = bpmDebugRegister then
    begin
      usableDebugReg := GetUsableDebugRegister(bptExecute);

      if usableDebugReg = -1 then
      begin
        if askforsoftwarebp then
        begin
          if not (dbcSoftwareBreakpoint in CurrentDebuggerInterface.DebuggerCapabilities) then
          begin
            MessageDlg(rsOutOfHWBreakpoints, mtError, [mbok], 0);
            exit;
          end
          else
          begin
            if MessageDlg(
              rsAllDebugRegistersAreUsedUpDoYouWantToUseASoftwareBP,
                mtConfirmation, [mbNo, mbYes], 0) = mrYes then
            begin
              if readProcessMemory(processhandle, pointer(address), @originalbyte, 1, br) then
                bpm := bpmInt3
              else
                raise Exception.Create(
                  rsUnreadableMemoryUnableToSetSoftwareBreakpoint);
            end
            else
              exit;
          end

        end
        else
        begin
          if not (dbcSoftwareBreakpoint in CurrentDebuggerInterface.DebuggerCapabilities) then exit;
          bpm := bpmInt3;
        end;
      end;
    end;


    result:=AddBreakpoint(nil, address, 1, bptExecute, bpm, bo_Break, usableDebugreg, nil, tid, nil, nil, 0, nil, OnBreakpoint);
  finally
    debuggercs.leave;
  end;
end;

function TDebuggerthread.SetOnWriteBreakpoint(address: ptrUint; size: integer; tid: dword=0; OnBreakpoint: TBreakpointEvent=nil): PBreakpoint;
begin
  result:=SetOnWriteBreakpoint(address, size, preferedBreakpointMethod, tid, OnBreakpoint);
end;

function TDebuggerthread.SetOnWriteBreakpoint(address: ptrUint; size: integer; bpm: TBreakpointMethod; tid: dword=0; OnBreakpoint: TBreakpointEvent=nil): PBreakpoint;
var
  i: integer;
  found: boolean;
  originalbyte: byte;
  oldprotect, bw, br: dword;

  usableDebugReg: integer;
  bplist: TBreakpointSplitArray;
begin
  found := False;

  result:=nil;

  debuggercs.enter;
  try
    //set the breakpoint
    if bpm=bpmInt3 then
      bpm:=bpmDebugRegister; //stupid

    if bpm=bpmDebugRegister then
    begin
      usableDebugReg := GetUsableDebugRegister(bptWrite);

      if usableDebugReg = -1 then
        raise Exception.Create(rsAllDebugRegistersAreUsedUp);

      setlength(bplist,0);
      GetBreakpointList(address, size, bplist);


      result:=AddBreakpoint(nil, bplist[0].address, bplist[0].size, bptWrite, bpm, bo_Break, usableDebugreg,  nil, tid, nil, nil, 0, nil, OnBreakpoint);
      for i:=1 to length(bplist)-1 do
      begin
        usableDebugReg:=GetUsableDebugRegister(bptwrite);
        if usableDebugReg=-1 then exit;
        AddBreakpoint(result, bplist[i].address, bplist[i].size, bptWrite, bpm, bo_Break, usableDebugreg,  nil, tid, nil,nil, 0, nil, OnBreakpoint);
      end;
    end
    else
    if bpm=bpmException then
      result:=AddBreakpoint(nil, address, size, bptWrite, bpm, bo_Break, -1, nil,0,nil,nil,0,nil, OnBreakpoint);


  finally
    debuggercs.leave;
  end;

end;

function TDebuggerthread.SetOnAccessBreakpoint(address: ptrUint; size: integer; tid: dword=0; OnBreakpoint: TBreakpointEvent=nil): PBreakpoint;
begin
  result:=SetOnAccessBreakpoint(address, size, preferedBreakpointMethod, tid, OnBreakpoint);
end;

function TDebuggerthread.SetOnAccessBreakpoint(address: ptrUint; size: integer; bpm: TBreakpointMethod; tid: dword=0; OnBreakpoint: TBreakpointEvent=nil): PBreakpoint;
var
  i: integer;
  found: boolean;
  originalbyte: byte;
  oldprotect, bw, br: dword;

  usableDebugReg: integer;
  bplist: TBreakpointSplitArray;
begin
  found := False;

  result:=nil;

  debuggercs.enter;
  try
    //set the breakpoint
    setlength(bplist,0);
    if bpm=bpmInt3 then
      bpm:=bpmDebugRegister; //stupid


    if bpm=bpmDebugRegister then
    begin
      usableDebugReg := GetUsableDebugRegister(bptAccess);
      if usableDebugReg = -1 then
        raise Exception.Create(rsAllDebugRegistersAreUsedUp);

      GetBreakpointList(address, size, bplist);

      result:=AddBreakpoint(nil, bplist[0].address, bplist[0].size, bptAccess, bpmDebugRegister, bo_Break, usableDebugreg, nil, tid, nil, nil, 0, nil, OnBreakpoint);
      for i:=1 to length(bplist)-1 do
      begin
        usableDebugReg:=GetUsableDebugRegister(bptAccess);
        if usableDebugReg=-1 then exit;
        AddBreakpoint(result, bplist[i].address,  bplist[i].size, bptAccess, bpmDebugRegister, bo_Break, usableDebugreg, nil, tid, nil, nil, 0, nil, OnBreakpoint);
      end;
    end
    else
    if bpm=bpmException then
      result:=AddBreakpoint(nil, address, size, bptAccess, bpm, bo_Break,-1,nil,0,nil,nil,0,nil,OnBreakpoint);

  finally
    debuggercs.leave;
  end;

end;




function TDebuggerthread.ToggleOnExecuteBreakpoint(address: ptrUint; breakpointmethod: TBreakpointMethod; tid: dword=0): PBreakpoint;
{Only called from the main thread}
var
  i: integer;
  found: boolean;
  originalbyte: byte;
  oldprotect: dword;
  bw, br: PtrUInt;

  usableDebugReg: integer;
  method: TBreakpointMethod;
begin
  //find the breakpoint if it is already assigned and then remove it, else add the breakpoint
  found := False;

  result:=nil;

  debuggercs.enter;
  try
    for i := 0 to BreakpointList.Count - 1 do
      if (PBreakpoint(BreakpointList[i])^.address = address) and
        (PBreakpoint(BreakpointList[i])^.breakpointTrigger = bptExecute) and
        ((PBreakpoint(BreakpointList[i])^.breakpointAction = bo_break) or (PBreakpoint(BreakpointList[i])^.breakpointAction = bo_ChangeRegister) ) and
        (PBreakpoint(BreakpointList[i])^.active) then
      begin
        found := True;
        RemoveBreakpoint(PBreakpoint(BreakpointList[i]));
        //remove breakpoint doesn't delete it, but only disables it and marks it for deletion, the debugger thread deletes it when it has nothing to do
      end;

    if not found then
    begin
      method := breakpointmethod;

      if method = bpmDebugRegister then
      begin
        usableDebugReg := GetUsableDebugRegister(bptExecute);

        if usableDebugReg = -1 then
        begin

          if not (dbcSoftwareBreakpoint in CurrentDebuggerInterface.DebuggerCapabilities) then
          begin
            MessageDlg(rsOutOfHWBreakpoints, mtError, [mbok],0);
            exit;
          end
          else
          begin
            if MessageDlg(rsAllDebugRegistersAreUsedUpDoYouWantToUseASoftwareBP, mtConfirmation, [mbNo, mbYes], 0) = mrYes then
            begin
              if readProcessMemory(processhandle, pointer(address), @originalbyte, 1, br) then
                method := bpmInt3
              else
                raise Exception.Create(rsUnreadableMemoryUnableToSetSoftwareBreakpoint);
            end
            else
              exit;
          end

        end;
      end;

      result:=AddBreakpoint(nil, address, 1, bptExecute, method, bo_Break, usableDebugreg, nil, tid);
    end;

  finally
    debuggercs.leave;
  end;
end;

function TDebuggerthread.getrealbyte(address: ptrUint): byte;
{
Called when the byte is a $cc
}
var bp: PBreakpoint;
begin
  result:=$cc;

  bp:=isBreakpoint(address);
  if bp<>nil then
  begin
    if bp^.breakpointMethod=bpmInt3 then
      result:=bp^.originalbyte;
  end;
end;

function TDebuggerthread.isBreakpoint(address: uint_ptr; address2: uint_ptr=0; includeinactive: boolean=false): PBreakpoint;
  {Checks if the given address has a breakpoint, and if so, return the breakpoint. Else return nil}
var
  i,j,k: integer;
begin
  Result := nil;

  if address2=0 then
    j:=0
  else
    j:=address2-address;

  debuggercs.enter;
  try
    for i := 0 to BreakpointList.Count - 1 do
    begin
      if PBreakpoint(BreakpointList[i])^.markedfordeletion then continue;

      for k:=0 to j do
      begin
        if (InRangeX(address+k, PBreakpoint(BreakpointList[i])^.address, PBreakpoint(BreakpointList[i])^.address + PBreakpoint(BreakpointList[i])^.size-1)) and
           (includeinactive or (PBreakpoint(BreakpointList[i])^.active)) then
        begin
          Result := PBreakpoint(BreakpointList[i]);
          exit;
        end;

      end;
    end;
  finally
    debuggercs.leave;
  end;
end;

procedure TDebuggerthread.ContinueDebugging(continueOption: TContinueOption; runtillAddress: ptrUint=0; handled: boolean=true);
{
Sets the way the debugger should continue, and triggers the sleeping thread to wait up and handle this changed event
}
var bp: PBreakpoint;
 ct: TDebugThreadHandler;
begin

  ct:=fcurrentThread;
  if ct<>nil then
  begin

    if ct.isWaitingToContinue then
    begin
      fcurrentThread:=nil;

      case continueOption of
        co_run, co_stepinto, co_stepover: ct.continueDebugging(continueOption, handled);
        co_runtill:
        begin
          //set a 1 time breakpoint for this thread at the runtilladdress
          debuggercs.enter;
          try
            bp:=isBreakpoint(runtilladdress);
            if bp<>nil then
            begin
              if bp^.breakpointTrigger=bptExecute then
              begin
                if (bp^.ThreadID<>0) and (bp^.ThreadID<>ct.ThreadId) then //it's a thread specific breakpoint, but not for this thread
                  bp^.ThreadId:=0; //break on all, the user will have to change this himself
              end
              else
                bp:=nil; //a useless breakpoint
            end;

            if bp=nil then
            begin
              bp:=SetOnExecuteBreakpoint(runTillAddress, false, ct.threadid);
//              bp:=ToggleOnExecuteBreakpoint(runTillAddress,fcurrentThread.threadid);
              if bp=nil then
                exit; //error,failure setting the breakpoint so exit. don't continue

              bp^.OneTimeOnly:=true;
              bp^.StepOverBp:=true;
            end;

          finally
            debuggercs.leave;

          end;
          ct.continueDebugging(co_run);
        end;

        else ct.continueDebugging(continueOption);
      end;


    end;
  end;
end;

procedure TDebuggerthread.WaitTillAttachedOrError;
//wait till the OnAttachEvent has been set
//Because this routine runs in the main app thread do a CheckSynchronize (The debugger calls synchronize)
var
  i: integer;
  Result: TWaitResult;
  mresult: TModalResult;
  starttime: dword;
  currentloopstarttime: dword;
  timeout: dword;

  userWantsToAttach: boolean;

  frmDebuggerAttachTimeout: TfrmDebuggerAttachTimeout;

  seconds: dword;
begin


  //if IsDebuggerPresent then //when debugging the debugger 10 seconds is too short
  //  timeout:=5000000
  //else
    timeout:=5000;

  OutputDebugString('WaitTillAttachedOrError');
  result:=wrTimeout;

  userWantsToAttach:=true;
  while userWantsToAttach do
  begin
    starttime:=GetTickCount;

    while (gettickcount-starttime)<timeout do
    begin

      currentloopstarttime:=GetTickCount;
      while CheckSynchronize and ((GetTickCount-currentloopstarttime)<50) do
      begin
        OutputDebugString('After CheckSynchronize');
        //synchronize for 50 milliseconds long
      end;

      Result := OnAttachEvent.WaitFor(50); //wait for 50 milliseconds for the OnAttachEvent


      if result=wrSignaled then break;
    end;

    if result<>wrSignaled then
    begin
      frmDebuggerAttachTimeout:=tfrmDebuggerAttachTimeout.Create(application);
      frmDebuggerAttachTimeout.event:=OnAttachEvent;

      mresult:=frmDebuggerAttachTimeout.ShowModal;
      frmDebuggerAttachTimeout.free;

      if mresult=mrAbort then
        raise exception.create(rsDebuggerAttachAborted);

      if mresult=mrok then break;

      userWantsToAttach:=mresult<>mrCancel;
    end
    else
      break;
    //userWantsToAttach:=(result<>wrSignaled) and (MessageDlg(rsDebuggerAttachTimeout, rsTheDebuggerAttachHasTimedOut, mtConfirmation, [mbyes, mbNo],0 )=mryes);
  end;

  Result := OnAttachEvent.WaitFor(50);
  if result<>wrSignaled then
  begin
    raise exception.create(rsDebuggerFailedToAttach)
  end;


  if delayAfterDebuggerAttach>0 then
  begin
    starttime:=gettickcount;
    seconds:=starttime;
    while gettickcount<starttime+delayAfterDebuggerAttach do
    begin
      CheckSynchronize(100);

      if gettickcount>seconds+1000 then
      begin
        seconds:=seconds+1000;
        beep;
      end;
    end;
    beep;
  end;

  OutputDebugString('WaitTillAttachedOrError exit');

  {//wait just a little and wait for some threads
  sleep(100);
  i:=0;
  while (ThreadList.Count=0) and (i<10) do
  begin
    CheckSynchronize;
    sleep(100);

    inc(i);
  end; }


  if terminated then
  begin

    if CurrentDebuggerInterface.errorstring='' then
      raise exception.create(rsDebuggerFailedToAttach)
    else
      raise exception.create(CurrentDebuggerInterface.errorstring);


  end;
end;

procedure TDebuggerThread.lockSettings;
begin
  //prevent the user from changing this setting till next restart
  formsettings.cbUseWindowsDebugger.enabled:=false;
  formsettings.cbUseVEHDebugger.enabled:=false;
  formsettings.cbKDebug.enabled:=false;
end;

procedure TDebuggerthread.defaultConstructorcode;
begin
  debuggerCS := TGuiSafeCriticalSection.Create;

  OnAttachEvent := TEvent.Create(nil, True, False, '');
  OnContinueEvent := Tevent.Create(nil, true, False, '');
  threadlist := TList.Create;
  BreakpointList := TList.Create;
  eventhandler := TDebugEventHandler.Create(self, OnAttachEvent, OnContinueEvent, breakpointlist, threadlist, debuggerCS);


  //get config parameters
  handlebreakpoints := formsettings.cbHandleBreakpoints.Checked;
  hidedebugger := formsettings.checkbox1.Checked;
  canusedebugregs := formsettings.rbDebugAsBreakpoint.Checked;

  //setup the used debugger
  if getconnection<>nil then
    CurrentDebuggerInterface:=TNetworkDebuggerInterface.create
  else
  begin
    if formsettings.cbUseWindowsDebugger.checked then
      CurrentDebuggerInterface:=TWindowsDebuggerInterface.create
    else if formsettings.cbUseVEHDebugger.checked then
      CurrentDebuggerInterface:=TVEHDebugInterface.create
    else if formsettings.cbKDebug.checked then
    begin
      globalDebug:=formsettings.cbGlobalDebug.checked;
      CurrentDebuggerInterface:=TKernelDebugInterface.create(globalDebug, formsettings.cbCanStepKernelcode.checked);
    end;
  end;


  //clean up some debug views

  if formdebugstrings = nil then
    formdebugstrings := Tformdebugstrings.Create(application);

  formdebugstrings.listbox1.Clear;
end;


constructor TDebuggerthread.MyCreate2(filename: string; parameters: string; breakonentry: boolean=true); overload;
begin
  inherited Create(true);
  defaultconstructorcode;


  if not (dbcBreakOnEntry in CurrentDebuggerInterface.DebuggerCapabilities) then
  begin
    MessageDlg(Format(rsThisDebuggerInterfaceDoesnTSupportBreakOnEntryYet, [CurrentDebuggerInterface.name]), mtError, [mbok], 0);
    terminate;
    start;
    exit;
  end;

  fRunning:=true;


  createProcess:=true;
  self.filename:=filename;
  self.parameters:=parameters;
  self.fNeedsToSetEntryPointBreakpoint:=breakonentry;

  start;
  WaitTillAttachedOrError;

  if not terminated then lockSettings;
end;

constructor TDebuggerthread.MyCreate2(processID: THandle);
begin

  defaultconstructorcode;

  createProcess:=false;
  fRunning:=true;
  locksettings;

  inherited Create(true);

  Start;
  WaitTillAttachedOrError;

  if not terminated then lockSettings;
end;

destructor TDebuggerthread.Destroy;
var i: integer;
begin
  terminate;
  waitfor;


  if OnAttachEvent <> nil then
  begin
    OnAttachEvent.SetEvent;
    FreeAndNil(OnAttachEvent);
  end;

  if threadlist <> nil then
  begin
    for i := 0 to threadlist.Count - 1 do
      TDebugThreadHandler(threadlist.Items[i]).Free;
    FreeAndNil(threadlist);
  end;

  if breakpointlist <> nil then
  begin
    for i := 0 to breakpointlist.Count - 1 do
      freemem(breakpointlist.Items[i]);

    FreeAndNil(breakpointlist);
  end;

  if debuggerCS <> nil then
    FreeAndNil(debuggerCS);

  if eventhandler <> nil then
    FreeAndNil(eventhandler);

  inherited Destroy;
end;

end.

