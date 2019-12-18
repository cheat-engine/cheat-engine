unit NetworkDebuggerInterface;

{$mode delphi}

interface

uses

  {$ifdef windows}
  jwawindows, windows,
  {$endif}
  Classes, SysUtils,cefuncproc, newkernelhandler,
  DebuggerInterface, networkInterface, networkInterfaceApi, contnrs{$ifdef darwin},macport, macportdefines{$endif};

type
  TNetworkDebuggerInterface=class(TDebuggerInterface)
  private
    handle: THandle;
    lastevent: TNetworkDebugEvent;
    fsinglestepNextContinue: boolean;

  public
    function WaitForDebugEvent(var lpDebugEvent: TDebugEvent; dwMilliseconds: DWORD): BOOL; override;
    function ContinueDebugEvent(dwProcessId: DWORD; dwThreadId: DWORD; dwContinueStatus: DWORD): BOOL; override;
    function SetThreadContext(hThread: THandle; const lpContext: TContext; isFrozenThread: Boolean=false): BOOL; override;
    function GetThreadContext(hThread: THandle; var lpContext: TContext; isFrozenThread: Boolean=false):  BOOL; override;

    function SetThreadContextArm(hThread: THandle; const lpContext: TArmContext; isFrozenThread: Boolean=false): BOOL; override;
    function GetThreadContextArm(hThread: THandle; var lpContext: TArmContext; isFrozenThread: Boolean=false):  BOOL; override;


    function GetLastBranchRecords(lbr: pointer): integer; override;
    function canReportExactDebugRegisterTrigger: boolean; override;

    function DebugActiveProcess(dwProcessId: DWORD): WINBOOL; override;
    property SingleStepNextContinue: boolean read fSingleStepNextContinue write fSingleStepNextContinue;

    destructor destroy; override;
    constructor create;
  end;


implementation

uses debuggertypedefinitions, ProcessHandlerUnit;

type
  TNetworkX86_32Context=packed record
    ebx: dword;
    ecx: dword;
    edx: dword;
    esi: dword;
    edi: dword;
    ebp: dword;
    eax: dword;
    ds: integer;
    es: integer;
    fs: integer;
    gs: integer;
    orig_eax: dword;
    eip: dword;
    cs: integer;
    eflags: dword;
    esp: dword;
    ss: integer;
  end;

  PNetworkX86_32Context=^TNetworkX86_32Context;

  TNetworkX86_64Context=packed record
    r15: qword;
    r14: qword;
    r13: qword;
    r12: qword;
    rbp: qword;
    rbx: qword;
    r11: qword;
    r10: qword;
    r9:  qword;
    r8: qword;
    rax: qword;
    rcx: qword;
    rdx: qword;
    rsi: qword;
    rdi: qword;
    orig_rax: qword;
    rip: qword;
    cs: qword;
    eflags: qword;
    rsp: qword;
    ss: qword;
    fs_base: qword;
    gs_base: qword;
    ds: qword;
    es: qword;
    fs: qword;
    gs: qword;
  end;
  PNetworkX86_64Context=^TNetworkX86_64Context;


  TNetworkArmContext=packed record
    R0: DWORD;
    R1: DWORD;
    R2: DWORD;
    R3: DWORD;
    R4: DWORD;
    R5: DWORD;
    R6: DWORD;
    R7: DWORD;
    R8: DWORD;
    R9: DWORD;
    R10: DWORD;
    FP: DWORD;
    IP: DWORD;
    SP: DWORD;
    LR: DWORD;
    PC: DWORD;
    CPSR: DWORD;
    ORIG_R0: DWORD;
  end;

  PNetworkArmContext=^TNetworkArmContext;


function TNetworkDebuggerInterface.WaitForDebugEvent(var lpDebugEvent: TDebugEvent; dwMilliseconds: DWORD): BOOL;
var
  c: TCEConnection;
begin

  result:=false;
  c:=getConnection;
  if c<>nil then
  begin
    lastevent.signal:=5;

    result:=c.WaitForDebugEvent(handle, dwMilliseconds*5, lastevent);

    if result then
    begin
      //convert it to 'something' useful
      lpDebugEvent.dwThreadId:=lastevent.threadid;
      lpDebugEvent.dwProcessId:=processid;

      case lastevent.signal of
        -1 : //create thread
        begin
          lpDebugEvent.dwDebugEventCode:=CREATE_THREAD_DEBUG_EVENT;
          lpDebugEvent.CreateThread.hThread:=lastevent.threadid;
        end;

        -2 : //create process
        begin
          lpDebugEvent.dwDebugEventCode:=CREATE_PROCESS_DEBUG_EVENT;
          lpDebugEvent.CreateProcessInfo.hProcess:=handle;
          lpDebugEvent.CreateProcessInfo.hThread:=lastevent.threadid;

          //set the breakpoint capability
          fmaxInstructionBreakpointCount:=lastevent.createProcess.maxBreakpointCount;
          fmaxWatchpointBreakpointCount:=lastevent.createProcess.maxWatchpointCount;
          fmaxSharedBreakpointCount:=lastevent.createProcess.maxSharedBreakpoints;
        end;

        5: //SIGTRAP
        begin
          lpDebugEvent.dwDebugEventCode:=EXCEPTION_DEBUG_EVENT;
          lpDebugEvent.Exception.dwFirstChance:=1;
          lpDebugEvent.Exception.ExceptionRecord.NumberParameters:=0;

          lpDebugEvent.Exception.ExceptionRecord.ExceptionCode:=EXCEPTION_SINGLE_STEP;
          lpDebugEvent.Exception.ExceptionRecord.ExceptionAddress:=pointer(lastevent.address);

        end;

        19: //sigstop
        begin
          //just ignore. continue and return that no stop happened (timeout)
          ContinueDebugEvent(handle, lastevent.threadid, DBG_CONTINUE);
          result:=false;
        end;



        else
        begin
          //no idea

          ContinueDebugEvent(handle, lastevent.threadid, DBG_EXCEPTION_NOT_HANDLED);
          result:=false;
        end;
      end;
    end;
  end;

end;

function TNetworkDebuggerInterface.ContinueDebugEvent(dwProcessId: DWORD; dwThreadId: DWORD; dwContinueStatus: DWORD): BOOL;
var
  c: TCEConnection;
begin

  result:=false;
  c:=getConnection;
  if c<>nil then
  begin
    if dwContinueStatus=DBG_CONTINUE then
    begin
      if fSingleStepNextContinue then
        result:=c.ContinueDebugEvent(handle, dwThreadID, 2)  //ignore this signal and enter a single step mode
      else
        result:=c.ContinueDebugEvent(handle, dwThreadID, 1);  //ignore this signal

      fSingleStepNextContinue:=false;
    end
    else
      result:=c.ContinueDebugEvent(handle, dwThreadID, 0);
  end;

end;

function TNetworkDebuggerInterface.SetThreadContextArm(hThread: THandle; const lpContext: TArmContext; isFrozenThread: Boolean=false): BOOL;
//get the current context and apply the changes from this context
begin
  result:=false;
end;

function TNetworkDebuggerInterface.GetThreadContextArm(hThread: THandle; var lpContext: TArmContext; isFrozenThread: Boolean=false):  BOOL;
var
  carm: PNetworkArmContext;
  c: TCEConnection=nil;
begin
  result:=false;

  c:=getConnection;
  if c<>nil then
  begin
    carm:=c.AllocateAndGetContext(handle, hThread);


    if (carm<>nil) and (processhandler.SystemArchitecture=archARM) then
    begin
      if processhandler.is64Bit then
      begin
        lpContext.PC:=$64646464; //holder for 64 bit for now
      end
      else
      begin
        lpContext.R0:=carm.R0;
        lpContext.R1:=carm.R1;
        lpContext.R2:=carm.R2;
        lpContext.R3:=carm.R3;
        lpContext.R4:=carm.R4;
        lpContext.R5:=carm.R5;
        lpContext.R6:=carm.R6;
        lpContext.R7:=carm.R7;
        lpContext.R8:=carm.R8;
        lpContext.R9:=carm.R9;
        lpContext.R10:=carm.R10;
        lpContext.FP:=carm.FP;
        lpContext.IP:=carm.IP;
        lpContext.SP:=carm.SP;
        lpContext.LR:=carm.LR;
        lpContext.PC:=carm.PC;
        lpContext.CPSR:=carm.CPSR;
        lpContext.ORIG_R0:=carm.ORIG_R0;
      end;
    end; //else use GetThreadContext

    if (carm<>nil) then
      FreeMemAndNil(carm);
  end;
end;



function TNetworkDebuggerInterface.SetThreadContext(hThread: THandle; const lpContext: TContext; isFrozenThread: Boolean=false): BOOL;
begin
  //get the current context and apply the changes from this context
  result:=false;
end;

function TNetworkDebuggerInterface.GetThreadContext(hThread: THandle; var lpContext: TContext; isFrozenThread: Boolean=false):  BOOL;
var
  c32: PNetworkX86_32Context=nil;
  c64: PNetworkX86_64Context absolute c32;

  c: TCEConnection=nil;
begin
  result:=false;

  zeromemory(@lpContext, sizeof(TContext));

  c:=getConnection;
  if c<>nil then
  begin
    c32:=c.AllocateAndGetContext(handle, hThread);


    if (c32<>nil) and (processhandler.SystemArchitecture=archX86) then
    begin
      {$ifdef cpu64}
      if processhandler.is64Bit then
      begin
        lpcontext.r15:=c64.r15;
        lpcontext.r14:=c64.r14;
        lpcontext.r13:=c64.r13;
        lpcontext.r12:=c64.r12;
        lpcontext.rbp:=c64.rbp;
        lpcontext.Rbx:=c64.rbx;
        lpcontext.R11:=c64.r11;
        lpcontext.R10:=c64.r10;
        lpcontext.R9:=c64.r9;
        lpcontext.R8:=c64.r8;
        lpcontext.Rax:=c64.rax;
        lpcontext.Rcx:=c64.rcx;
        lpcontext.Rdx:=c64.rdx;
        lpcontext.Rsi:=c64.rsi;
        lpcontext.Rdi:=c64.rdi;

        lpcontext.P1Home:=c64.orig_rax;
        lpcontext.rip:=c64.rip;
        lpcontext.SegCs:=c64.cs;
        lpcontext.EFlags:=c64.eflags;
        lpcontext.rsp:=c64.rsp;
        lpcontext.Segss:=c64.ss;
        lpcontext.P2Home:=c64.fs_base;
        lpcontext.P3Home:=c64.gs_base;
        lpcontext.Segds:=c64.ds;
        lpcontext.Seges:=c64.es;
        lpcontext.Segfs:=c64.fs;
        lpcontext.Seggs:=c64.gs;
      end
      else
      {$endif}
      begin
        lpcontext.{$ifdef cpu64}rbx{$else}ebx{$endif}:=c32.ebx;
        lpcontext.{$ifdef cpu64}rcx{$else}ecx{$endif}:=c32.ecx;
        lpcontext.{$ifdef cpu64}rdx{$else}edx{$endif}:=c32.edx;
        lpcontext.{$ifdef cpu64}rsi{$else}esi{$endif}:=c32.esi;
        lpcontext.{$ifdef cpu64}rdi{$else}edi{$endif}:=c32.edi;
        lpcontext.{$ifdef cpu64}rbp{$else}ebp{$endif}:=c32.ebp;
        lpcontext.{$ifdef cpu64}rax{$else}eax{$endif}:=c32.eax;
        lpcontext.segds:=c32.ds;
        lpcontext.seges:=c32.es;
        lpcontext.segfs:=c32.fs;
        lpcontext.seggs:=c32.gs;

        lpcontext.{$ifdef cpu64}rip{$else}eip{$endif}:=c32.eip;
        lpcontext.segcs:=c32.cs;
        lpcontext.EFlags:=c32.eflags;
        lpcontext.{$ifdef cpu64}rsp{$else}esp{$endif}:=c32.esp;
        lpcontext.segss:=c32.ss;
      end;
    end; //you should use GetThreadContextArm

    if c32<>nil then
      FreeMemAndNil(c32);
  end;

  result:=lpContext.{$ifdef cpu64}rip{$else}eip{$endif}<>0;
end;


function TNetworkDebuggerInterface.GetLastBranchRecords(lbr: pointer): integer;
begin
  result:=0;
end;

function TNetworkDebuggerInterface.DebugActiveProcess(dwProcessId: DWORD): WINBOOL;
var c: TCEConnection;
begin
  result:=false;
  processhandler.processid:=dwProcessID;
  Open_Process;

  handle:=ProcessHandle;

  if (handle<>0) then
  begin
    c:=getConnection;
    if c<>nil then
      result:=c.StartDebug(handle);

  end;

end;

function TNetworkDebuggerInterface.canReportExactDebugRegisterTrigger: boolean;
begin
  result:=false;
end;


destructor TNetworkDebuggerInterface.destroy;
begin

  {
  if (handle)
    networkStopDebug();
    }
  inherited destroy;
end;

constructor TNetworkDebuggerInterface.create;
begin
  //no software breakpoint for now
  fDebuggerCapabilities:=[dbcHardwareBreakpoint];
end;


end.

