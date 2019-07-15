unit KernelDebuggerInterface;

{$mode delphi}

interface

uses
  jwawindows, windows, Classes, SysUtils,cefuncproc, newkernelhandler,DebuggerInterface,contnrs;

type
  TEventType=(etCreateProcess, etCreateThread, etDestroyThread);
  TInjectedEvent=record
    eventType: TEventType;
    processid: dword;
    threadid: dword;
  end;
  PInjectedEvent=^TInjectedEvent;


type
  TKernelDebugInterface=class;
  TThreadPoller=class(tthread)
  private
    threadlist: TList;

    procedure UpdateList;
    procedure CreateThreadEvent(threadid: dword);
    procedure DestroyThreadEvent(threadid: dword);

  public
    pid: dword;
    di: TKernelDebugInterface;
    procedure GetCurrentList(list: tlist);
    procedure execute; override;
  end;

  TKernelDebugInterface=class(TDebuggerInterface)
  private
    pid: DWORD;
    currentdebuggerstate: TDebuggerstate;

    injectedEvents: Tqueue;
    threadpoller: TThreadPoller;
    NeedsToContinue: boolean;
    globalDebug: boolean;
  public
    function WaitForDebugEvent(var lpDebugEvent: TDebugEvent; dwMilliseconds: DWORD): BOOL; override;
    function ContinueDebugEvent(dwProcessId: DWORD; dwThreadId: DWORD; dwContinueStatus: DWORD): BOOL; override;
    function SetThreadContext(hThread: THandle; const lpContext: TContext; isFrozenThread: Boolean=false): BOOL; override;
    function GetThreadContext(hThread: THandle; var lpContext: TContext; isFrozenThread: Boolean=false):  BOOL; override;

    function GetLastBranchRecords(lbr: pointer): integer; override;
    function canReportExactDebugRegisterTrigger: boolean; override;

    procedure injectEvent(e: pointer);
    function DebugActiveProcess(dwProcessId: DWORD): WINBOOL; override;
    function EventCausedByDBVM: boolean;

    destructor destroy; override;
    constructor create(globalDebug, canStepKernelcode: boolean);
  end;

implementation


uses symbolhandler, ProcessHandlerUnit;

resourcestring
  rsDBKDebug_StartDebuggingFailed ='DBKDebug_StartDebugging failed';

procedure TThreadPoller.CreateThreadEvent(threadid: dword);
var ie: PInjectedEvent;
begin
  getmem(ie, sizeof(TInjectedEvent));
  ie.eventType:=etCreateThread;
  ie.threadid:=threadid;
  ie.processid:=pid;

  di.injectEvent(ie);
end;

procedure TThreadPoller.DestroyThreadEvent(threadid: dword);
var ie: PInjectedEvent;
begin
  getmem(ie, sizeof(TInjectedEvent));
  ie.eventType:=etDestroyThread;
  ie.threadid:=threadid;
  ie.processid:=pid;

  di.injectEvent(ie);
end;


procedure TThreadPoller.GetCurrentList(list: tlist);
var
  ths: thandle;
  lpte: TThreadEntry32;
  check: boolean;
begin
  ths:=CreateToolhelp32Snapshot(TH32CS_SNAPALL,pid);

  if ths<>INVALID_HANDLE_VALUE then
  begin
    zeromemory(@lpte,sizeof(lpte));
    lpte.dwSize:=sizeof(lpte);
    check:=Thread32First(ths, lpte);
    while check do
    begin
      if lpte.th32OwnerProcessID=pid then
        list.add(pointer(ptrUint(lpte.th32ThreadID)));

      check:=Thread32next(ths,lpte);
    end;

    closehandle(ths);
  end;
end;

procedure TThreadPoller.UpdateList;
var newlist: Tlist;
i: integer;
begin
  newlist:=tlist.create;
  GetCurrentList(newlist);

  //now try to find the differences

  //is there a threadid that's not in the current threadlist?
  for i:=0 to newlist.Count-1 do
    if threadlist.IndexOf(newlist[i])=-1 then //not found
      CreateThreadEvent(ptrUint(newlist[i]));

  for i:=0 to threadlist.count-1 do
    if newlist.IndexOf(threadlist[i])=-1 then //the new list doesn't contain this threadid
      DestroyThreadEvent(ptrUint(threadlist[i]));

  //free the old list and make the new list the current list
  threadlist.free;
  threadlist:=newlist;
end;

procedure TThreadPoller.execute;
begin
  threadlist:=TList.Create;
  try
    GetCurrentList(threadlist);

    while not terminated do
    begin

      sleep(1000);
      UpdateList;
    end;
  finally
    threadlist.free;
  end;
end;



//------------------------------------------------------------------------------

procedure TKernelDebugInterface.injectEvent(e: pointer);
begin
  if injectedEvents<>nil then
    injectedEvents.Push(e);
end;

function TKernelDebugInterface.DebugActiveProcess(dwProcessId: DWORD): WINBOOL;
{Start the kerneldebugger for the current process}
var cpe: PInjectedEvent;
    tl: tlist;
    i: integer;
begin
  loaddbk32;
  //if not loaddbvmifneeded then
  //  raise exception.Create('You can''t currently use the kernel debugger');

  outputdebugstring('Using the kernelmode debugger');

  result:=DBKDebug_StartDebugging(dwProcessId);

  if result then
  begin
    processhandler.processid:=dwProcessID;
    Open_Process;
    symhandler.reinitialize;
    symhandler.waitforsymbolsloaded(true);

    pid:=dwProcessID;

    threadpoller:=TThreadPoller.Create(true);
    threadpoller.pid:=pid;

    tl:=tlist.create;
    try
      threadpoller.GetCurrentList(tl);

      getmem(cpe, sizeof(TInjectedEvent));
      cpe.eventType:=etCreateProcess;
      cpe.processid:=pid;
      if tl.count>0 then
        cpe.threadid:=ptrUint(tl.items[0])
      else
        cpe.threadid:=0;

      injectEvent(cpe);

      for i:=0 to tl.count-1 do
      begin
        getmem(cpe, sizeof(TInjectedEvent));
        cpe.eventType:=etCreateThread;
        cpe.processid:=pid;
        cpe.threadid:=ptrUint(tl.items[i]);
        injectEvent(cpe);
      end;


    finally
      tl.free;
    end;


    threadpoller.Start;
  end
  else
    raise exception.create(rsDBKDebug_StartDebuggingFailed);


end;

function TKernelDebugInterface.SetThreadContext(hThread: THandle; const lpContext: TContext; isFrozenThread: Boolean=false): BOOL;
begin
  outputdebugstring('TKernelDebugInterface.SetThreadContext');
  if NeedsToContinue and isFrozenThread then
  begin
    //use the currentdebuggerstate
    currentdebuggerstate.eax:=lpContext.{$ifdef cpu64}Rax{$else}eax{$endif};
    currentdebuggerstate.ebx:=lpContext.{$ifdef cpu64}Rbx{$else}ebx{$endif};
    currentdebuggerstate.ecx:=lpContext.{$ifdef cpu64}Rcx{$else}ecx{$endif};
    currentdebuggerstate.edx:=lpContext.{$ifdef cpu64}Rdx{$else}edx{$endif};
    currentdebuggerstate.esi:=lpContext.{$ifdef cpu64}Rsi{$else}esi{$endif};
    currentdebuggerstate.edi:=lpContext.{$ifdef cpu64}Rdi{$else}edi{$endif};
    currentdebuggerstate.ebp:=lpContext.{$ifdef cpu64}Rbp{$else}ebp{$endif};
    currentdebuggerstate.esp:=lpContext.{$ifdef cpu64}Rsp{$else}esp{$endif};
    currentdebuggerstate.eip:=lpContext.{$ifdef cpu64}Rip{$else}eip{$endif};
    {$ifdef cpu64}
    currentdebuggerstate.r8:=lpContext.r8;
    currentdebuggerstate.r9:=lpContext.r9;
    currentdebuggerstate.r10:=lpContext.r10;
    currentdebuggerstate.r11:=lpContext.r11;
    currentdebuggerstate.r12:=lpContext.r12;
    currentdebuggerstate.r13:=lpContext.r13;
    currentdebuggerstate.r14:=lpContext.r14;
    currentdebuggerstate.r15:=lpContext.r15;
    {$endif}
    currentdebuggerstate.cs:=lpContext.SegCs;
    currentdebuggerstate.ss:=lpContext.SegSs;
    currentdebuggerstate.ds:=lpContext.SegDs;
    currentdebuggerstate.es:=lpContext.SegEs;
    currentdebuggerstate.fs:=lpContext.SegFs;
    currentdebuggerstate.gs:=lpContext.SegGs;
    currentdebuggerstate.eflags:=lpContext.EFlags;

    if not globalDebug then
    begin
      currentdebuggerstate.dr0:=lpContext.Dr0;
      currentdebuggerstate.dr1:=lpContext.Dr1;
      currentdebuggerstate.dr2:=lpContext.Dr2;
      currentdebuggerstate.dr3:=lpContext.Dr3;
      currentdebuggerstate.dr6:=lpContext.Dr6;
      currentdebuggerstate.dr7:=lpContext.Dr7;
    end;

    {$ifdef cpu64}

    CopyMemory(@currentdebuggerstate.fxstate, @lpContext.FltSave, 512);
    {$else}
    CopyMemory(@currentdebuggerstate.fxstate, @lpContext.ext, sizeof(lpContext.ext));
    {$endif}

    result:=DBKDebug_SetDebuggerState(@currentdebuggerstate);

  end else
    result:=newkernelhandler.SetThreadContext(hthread, lpContext);

end;

function TKernelDebugInterface.GetThreadContext(hThread: THandle; var lpContext: TContext; isFrozenThread: Boolean=false):  BOOL;
begin
  outputdebugstring('TKernelDebugInterface.GetThreadContext');
  if NeedsToContinue and isFrozenThread then
  begin
    outputdebugstring('This is the frozen thread so use the internal method');

    result:=DBKDebug_GetDebuggerState(@currentdebuggerstate);

    //use the currentdebuggerstate
    lpContext.{$ifdef cpu64}Rax{$else}eax{$endif}:=currentdebuggerstate.eax;
    lpContext.{$ifdef cpu64}Rbx{$else}ebx{$endif}:=currentdebuggerstate.ebx;
    lpContext.{$ifdef cpu64}Rcx{$else}ecx{$endif}:=currentdebuggerstate.ecx;
    lpContext.{$ifdef cpu64}Rdx{$else}edx{$endif}:=currentdebuggerstate.edx;
    lpContext.{$ifdef cpu64}Rsi{$else}esi{$endif}:=currentdebuggerstate.esi;
    lpContext.{$ifdef cpu64}Rdi{$else}edi{$endif}:=currentdebuggerstate.edi;
    lpContext.{$ifdef cpu64}Rbp{$else}ebp{$endif}:=currentdebuggerstate.ebp;
    lpContext.{$ifdef cpu64}Rsp{$else}esp{$endif}:=currentdebuggerstate.esp;
    lpContext.{$ifdef cpu64}Rip{$else}eip{$endif}:=currentdebuggerstate.eip;
    {$ifdef cpu64}
    lpContext.r8:=currentdebuggerstate.r8;
    lpContext.r9:=currentdebuggerstate.r9;
    lpContext.r10:=currentdebuggerstate.r10;
    lpContext.r11:=currentdebuggerstate.r11;
    lpContext.r12:=currentdebuggerstate.r12;
    lpContext.r13:=currentdebuggerstate.r13;
    lpContext.r14:=currentdebuggerstate.r14;
    lpContext.r15:=currentdebuggerstate.r15;
    {$endif}
    lpContext.SegCs:=currentdebuggerstate.cs;
    lpContext.SegSs:=currentdebuggerstate.ss;
    lpContext.SegDs:=currentdebuggerstate.ds;
    lpContext.SegEs:=currentdebuggerstate.es;
    lpContext.SegFs:=currentdebuggerstate.fs;
    lpContext.SegGs:=currentdebuggerstate.gs;
    lpContext.EFlags:=currentdebuggerstate.eflags;
    lpContext.Dr0:=currentdebuggerstate.dr0;
    lpContext.Dr1:=currentdebuggerstate.dr1;
    lpContext.Dr2:=currentdebuggerstate.dr2;
    lpContext.Dr3:=currentdebuggerstate.dr3;
    lpContext.Dr6:=currentdebuggerstate.dr6;
    lpContext.Dr7:=currentdebuggerstate.dr7;

    {$ifdef cpu64}
    CopyMemory(@lpContext.FltSave, @currentdebuggerstate.fxstate, 512);
    {$else}
    CopyMemory(@lpContext.ext, @currentdebuggerstate.fxstate, sizeof(lpContext.ext));
    {$endif}

    lpContext.ContextFlags:=0;

    if currentdebuggerstate.causedbydbvm<>0 then
      log('currentdebuggerstate.causedbydbvm<>0');
  end else
  begin
   // outputdebugstring('Use the default method');
    result:=newkernelhandler.GetThreadContext(hthread, lpContext);
  end;
end;

function TKernelDebugInterface.GetLastBranchRecords(lbr: pointer): integer;
type
  TQwordArray=array[0..0] of QWORD;
  PQwordArray=^TQWORDArray;
var l: PQWordarray;
    i: integer;
begin
  l:=PQWordarray(lbr);
  if NeedsToContinue then
  begin
    if lbr<>nil then //if nil then it's only a query of how many items there are
    begin
      for i:=0 to currentdebuggerstate.LBR_Count-1 do
        l[i]:=currentdebuggerstate.LBR[i];
    end;

    result:=currentdebuggerstate.LBR_Count-1;
  end
  else
    result:=-1;
end;

function TKernelDebugInterface.ContinueDebugEvent(dwProcessId: DWORD; dwThreadId: DWORD; dwContinueStatus: DWORD): BOOL;
begin
  outputdebugstring('TKernelDebugInterface.ContinueDebugEvent');
  if NeedsToContinue then
  begin
    outputdebugstring('NeedsToContinue=true');
    DBKDebug_SetDebuggerState(@currentdebuggerstate);
    result:=DBKDebug_ContinueDebugEvent(dwContinueStatus=DBG_CONTINUE);
    NeedsToContinue:=false;
  end
  else
  begin
    outputdebugstring('NeedsToContinue=false');
    result:=true;
  end;
end;

function TKernelDebugInterface.WaitForDebugEvent(var lpDebugEvent: TDebugEvent; dwMilliseconds: DWORD): BOOL;
var injectedEvent: PInjectedEvent;
begin
  ZeroMemory(@lpDebugEvent, sizeof(TdebugEvent));

  if injectedEvents.Count>0 then
  begin
    result:=true;
    injectedEvent:=injectedEvents.Pop;
    if injectedEvent<>nil then //just to be sure
    begin
      lpDebugEvent.dwProcessId:=injectedevent.processid;
      lpDebugEvent.dwThreadId:=injectedevent.threadid;

      case injectedevent.eventType of
        etCreateProcess:
        begin
          lpDebugEvent.dwDebugEventCode:=CREATE_PROCESS_DEBUG_EVENT;
          lpDebugEvent.CreateProcessInfo.hProcess:=processhandle;
          lpDebugEvent.CreateProcessInfo.hThread:=OpenThread(THREAD_ALL_ACCESS,false, injectedevent.threadid);
        end;

        etCreateThread:
        begin
          lpDebugEvent.dwDebugEventCode:=CREATE_THREAD_DEBUG_EVENT;
          lpDebugEvent.CreateThread.hThread:=OpenThread(THREAD_ALL_ACCESS,false, injectedevent.threadid);
        end;
        etDestroyThread: lpDebugEvent.dwDebugEventCode:=EXIT_THREAD_DEBUG_EVENT;

      end;

      NeedsToContinue:=false; //it's not really paused
      freememandnil(injectedEvent);
    end;
  end
  else
  begin

    NeedsToContinue:=true;
    result:=DBKDebug_WaitForDebugEvent(dwMilliseconds);
    if result then
    begin
      OutputDebugString('Received a debug event that wasn''t injected');

      //get the state and setup lpDebugEvent
      DBKDebug_GetDebuggerState(@currentdebuggerstate);

      Log(format('currentdebuggerstate.eip=%8x',[currentdebuggerstate.eip]));

      //this is only a bp hit event
      lpDebugEvent.dwDebugEventCode:=EXCEPTION_DEBUG_EVENT;

      lpDebugEvent.dwProcessId:=pid;
      lpDebugEvent.dwThreadId:=currentdebuggerstate.threadid;
      lpDebugEvent.Exception.dwFirstChance:=1;
      lpDebugEvent.Exception.ExceptionRecord.ExceptionCode:=EXCEPTION_SINGLE_STEP;
      lpDebugEvent.Exception.ExceptionRecord.ExceptionAddress:=pointer(ptrUint(currentdebuggerstate.eip));
    end;
  end;
end;

function TKernelDebugInterface.EventCausedByDBVM: boolean;
begin
  result:=currentdebuggerstate.causedbydbvm<>0;
end;

function TKernelDebugInterface.canReportExactDebugRegisterTrigger: boolean;
begin
  result:=not globalDebug;
end;

destructor TKernelDebugInterface.destroy;
begin
  if injectedEvents<>nil then
    injectedEvents.free;

  if threadpoller<>nil then
    threadpoller.free;

  if pid<>0 then
    DBKDebug_StopDebugging;

  inherited destroy;
end;

constructor TKernelDebugInterface.create(globalDebug, canStepKernelcode: boolean);
begin
  inherited create;

  self.globalDebug:=globalDebug;

  LoadDBK32;

  DBKDebug_SetAbilityToStepKernelCode(canStepKernelcode);
  DBKDebug_SetGlobalDebugState(globalDebug);
  injectedEvents:=TQueue.Create;

  fDebuggerCapabilities:=[dbcHardwareBreakpoint, dbcDBVMBreakpoint];
  name:='Kernelmode Debugger';

  fmaxSharedBreakpointCount:=4;
end;


end.

