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
    function GetThreadContextArm64(hThread: THandle; var lpContext: TArm64Context; isFrozenThread: Boolean=false):  BOOL; override;
    function SetThreadContextArm64(hThread: THandle; const lpContext: TArm64Context; isFrozenThread: Boolean=false):  BOOL; override;


    function GetLastBranchRecords(lbr: pointer): integer; override;
    function canReportExactDebugRegisterTrigger: boolean; override;

    function DebugActiveProcess(dwProcessId: DWORD): WINBOOL; override;
    property SingleStepNextContinue: boolean read fSingleStepNextContinue write fSingleStepNextContinue;

    destructor destroy; override;
    constructor create;
  end;


function SetThreadContext(hThread: THandle; const lpContext: TContext): BOOL;
function GetThreadContext(hThread: THandle; var lpContext: TContext):  BOOL;
function SetThreadContextArm(hThread: THandle; const lpContext: TArmContext): BOOL;
function GetThreadContextArm(hThread: THandle; var lpContext: TArmContext):  BOOL;
function GetThreadContextArm64(hThread: THandle; var lpContext: TArm64Context):  BOOL;
function SetThreadContextArm64(hThread: THandle; const lpContext: TArm64Context):  BOOL;




implementation

uses debuggertypedefinitions, ProcessHandlerUnit;

const
  networkContextType_X86=0;
  networkContextType_X86_64=1;
  networkContextType_Arm=2;
  networkContextType_Arm64=3;

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

    fp: TXmmSaveArea;
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
    fp: TXmmSaveArea;
  end;
  PNetworkX86_64Context=^TNetworkX86_64Context;


  TNetworkARM_32Context=packed record
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

    fpu: record
      regs: array [0..31] of QWORD;
      control: DWORD;
    end;
  end;

  TNetworkARM_64Context=record
      regs: TARM64CONTEXT_REGISTERS;
      SP:  QWORD;
      PC:  QWORD;
      PSTATE: QWORD;

      fp: record
        vregs: array [0..31] of m128a; //  __uint128_t vregs[32];
        fpsr: UINT32; // __u32 fpsr;
        fpcr: UINT32; // __u32 fpcr;
        reserved: array [0..1] of UINT32; //             __u32 __reserved[2];
      end;
  end;


  TNetworkContext=packed record
    contextsize: uint32;
    contexttype: uint32; //0=x86, 1=x86_64, 2=arm, 3=arm64
    case integer of  //contexttype
      0: (contextx86:    TNetworkX86_32Context);
      1: (contextx86_64: TNetworkX86_64Context);
      2: (contextarm32:  TNetworkARM_32Context);
      3: (contextarm64:  TNetworkARM_64Context);
  end;
  PNetworkContext=^TNetworkContext;


function SetThreadContext(hThread: THandle; const lpContext: TContext): bool;
var
  context: TNetworkContext;
  c: TCEConnection=nil;
begin
  c:=getConnection;
  if c<>nil then
  begin
    {$ifdef cpu64}
    if processhandler.is64Bit then
    begin
      context.contextsize:=sizeof(TNetworkX86_64Context)+8;
      context.contexttype:=1; //x86_64

      context.contextx86_64.r15:=lpcontext.r15;
      context.contextx86_64.r14:=lpcontext.r14;
      context.contextx86_64.r13:=lpcontext.r13;
      context.contextx86_64.r12:=lpcontext.r12;
      context.contextx86_64.rbp:=lpcontext.rbp;
      context.contextx86_64.rbx:=lpcontext.Rbx;
      context.contextx86_64.r11:=lpcontext.R11;
      context.contextx86_64.r10:=lpcontext.R10;
      context.contextx86_64.r9:= lpcontext.R9;
      context.contextx86_64.r8:= lpcontext.R8;
      context.contextx86_64.rax:=lpcontext.Rax;
      context.contextx86_64.rcx:=lpcontext.Rcx;
      context.contextx86_64.rdx:=lpcontext.Rdx;
      context.contextx86_64.rsi:=lpcontext.Rsi;
      context.contextx86_64.rdi:=lpcontext.Rdi;

      context.contextx86_64.orig_rax:=lpcontext.P1Home;
      context.contextx86_64.rip:=lpcontext.rip;
      context.contextx86_64.cs:=lpcontext.SegCs;
      context.contextx86_64.eflags:=lpcontext.EFlags;
      context.contextx86_64.rsp:=lpcontext.rsp;
      context.contextx86_64.ss:=lpcontext.Segss;
      context.contextx86_64.fs_base:=lpcontext.P2Home;
      context.contextx86_64.gs_base:=lpcontext.P3Home;
      context.contextx86_64.ds:=lpcontext.Segds;
      context.contextx86_64.es:=lpcontext.Seges;
      context.contextx86_64.fs:=lpcontext.Segfs;
      context.contextx86_64.gs:=lpcontext.Seggs;
      context.contextx86_64.fp:=lpcontext.FltSave;
    end
    else
    {$endif}
    begin
      context.contextsize:=sizeof(TNetworkX86_64Context)+8;
      context.contexttype:=0; // x86

      context.contextx86.ebx:=lpcontext.{$ifdef cpu64}rbx{$else}ebx{$endif};
      context.contextx86.ecx:=lpcontext.{$ifdef cpu64}rcx{$else}ecx{$endif};
      context.contextx86.edx:=lpcontext.{$ifdef cpu64}rdx{$else}edx{$endif};
      context.contextx86.esi:=lpcontext.{$ifdef cpu64}rsi{$else}esi{$endif};
      context.contextx86.edi:=lpcontext.{$ifdef cpu64}rdi{$else}edi{$endif};
      context.contextx86.ebp:=lpcontext.{$ifdef cpu64}rbp{$else}ebp{$endif};
      context.contextx86.eax:=lpcontext.{$ifdef cpu64}rax{$else}eax{$endif};
      context.contextx86.ds:=lpcontext.segds;
      context.contextx86.es:=lpcontext.seges;
      context.contextx86.fs:=lpcontext.segfs;
      context.contextx86.gs:=lpcontext.seggs;
      context.contextx86.eip:=lpcontext.{$ifdef cpu64}rip{$else}eip{$endif};
      context.contextx86.cs:=lpcontext.segcs;
      context.contextx86.eflags:=lpcontext.EFlags;
      context.contextx86.esp:=lpcontext.{$ifdef cpu64}rsp{$else}esp{$endif};
      context.contextx86.ss:=lpcontext.segss;
      context.contextx86.fp:=lpcontext.{$ifdef cpu64}FltSave{$else}ext{$endif};
    end;


    result:=c.setContext(processhandle, hThread, @context, context.contextsize);
  end;
end;


function GetThreadContext(hThread: THandle; var lpContext: TContext):  BOOL;
var
  context: PNetworkContext=nil;
  c: TCEConnection=nil;
begin
  result:=false;

  zeromemory(@lpContext, sizeof(TContext));

  c:=getConnection;
  if c<>nil then
  begin
    context:=c.AllocateAndGetContext(processhandle, hThread);
    try
      if (context<>nil) and (processhandler.SystemArchitecture=archX86) then
      begin
        {$ifdef cpu64}
        if processhandler.is64Bit then
        begin
          if context^.contexttype<>networkContextType_X86_64 then
          begin
            OutputDebugString('Expected X86_64 context, received type '+context^.contexttype.ToString);
            exit(false);
          end;
          lpcontext.r15:=context^.contextx86_64.r15;
          lpcontext.r14:=context^.contextx86_64.r14;
          lpcontext.r13:=context^.contextx86_64.r13;
          lpcontext.r12:=context^.contextx86_64.r12;
          lpcontext.rbp:=context^.contextx86_64.rbp;
          lpcontext.Rbx:=context^.contextx86_64.rbx;
          lpcontext.R11:=context^.contextx86_64.r11;
          lpcontext.R10:=context^.contextx86_64.r10;
          lpcontext.R9:=context^.contextx86_64.r9;
          lpcontext.R8:=context^.contextx86_64.r8;
          lpcontext.Rax:=context^.contextx86_64.rax;
          lpcontext.Rcx:=context^.contextx86_64.rcx;
          lpcontext.Rdx:=context^.contextx86_64.rdx;
          lpcontext.Rsi:=context^.contextx86_64.rsi;
          lpcontext.Rdi:=context^.contextx86_64.rdi;

          lpcontext.P1Home:=context^.contextx86_64.orig_rax;
          lpcontext.rip:=context^.contextx86_64.rip;
          lpcontext.SegCs:=context^.contextx86_64.cs;
          lpcontext.EFlags:=context^.contextx86_64.eflags;
          lpcontext.rsp:=context^.contextx86_64.rsp;
          lpcontext.Segss:=context^.contextx86_64.ss;
          lpcontext.P2Home:=context^.contextx86_64.fs_base;
          lpcontext.P3Home:=context^.contextx86_64.gs_base;
          lpcontext.Segds:=context^.contextx86_64.ds;
          lpcontext.Seges:=context^.contextx86_64.es;
          lpcontext.Segfs:=context^.contextx86_64.fs;
          lpcontext.Seggs:=context^.contextx86_64.gs;
          lpcontext.FltSave:=context^.contextx86_64.fp;
        end
        else
        {$endif}
        begin
          if context^.contexttype<>networkContextType_X86 then
          begin
            OutputDebugString('Expected X86_32 context, received type '+context^.contexttype.ToString);
            exit(false);
          end;

          lpcontext.{$ifdef cpu64}rbx{$else}ebx{$endif}:=context^.contextx86.ebx;
          lpcontext.{$ifdef cpu64}rcx{$else}ecx{$endif}:=context^.contextx86.ecx;
          lpcontext.{$ifdef cpu64}rdx{$else}edx{$endif}:=context^.contextx86.edx;
          lpcontext.{$ifdef cpu64}rsi{$else}esi{$endif}:=context^.contextx86.esi;
          lpcontext.{$ifdef cpu64}rdi{$else}edi{$endif}:=context^.contextx86.edi;
          lpcontext.{$ifdef cpu64}rbp{$else}ebp{$endif}:=context^.contextx86.ebp;
          lpcontext.{$ifdef cpu64}rax{$else}eax{$endif}:=context^.contextx86.eax;
          lpcontext.segds:=context^.contextx86.ds;
          lpcontext.seges:=context^.contextx86.es;
          lpcontext.segfs:=context^.contextx86.fs;
          lpcontext.seggs:=context^.contextx86.gs;

          lpcontext.{$ifdef cpu64}rip{$else}eip{$endif}:=context^.contextx86.eip;
          lpcontext.segcs:=context^.contextx86.cs;
          lpcontext.EFlags:=context^.contextx86.eflags;
          lpcontext.{$ifdef cpu64}rsp{$else}esp{$endif}:=context^.contextx86.esp;
          lpcontext.segss:=context^.contextx86.ss;
          lpcontext.{$ifdef cpu64}FltSave{$else}ext{$endif}:=context^.contextx86.fp;
        end;
      end; //you should use GetThreadContextArm
    finally
      if context<>nil then
        FreeMemAndNil(context);
    end;
  end;

  result:=lpContext.{$ifdef cpu64}rip{$else}eip{$endif}<>0;
end;

function SetThreadContextArm(hThread: THandle; const lpContext: TArmContext): BOOL;
var
  carm: TNetworkContext;
  c: TCEConnection=nil;
begin
  c:=getConnection;
  if c<>nil then
  begin
    carm.contextsize:=sizeof(TNetworkARM_32Context)+8;
    carm.contexttype:=2; //arm 32



    carm.contextarm32.R0:=lpContext.R0;
    carm.contextarm32.R1:=lpContext.R1;
    carm.contextarm32.R2:=lpContext.R2;
    carm.contextarm32.R3:=lpContext.R3;
    carm.contextarm32.R4:=lpContext.R4;
    carm.contextarm32.R5:=lpContext.R5;
    carm.contextarm32.R6:=lpContext.R6;
    carm.contextarm32.R7:=lpContext.R7;
    carm.contextarm32.R8:=lpContext.R8;
    carm.contextarm32.R9:=lpContext.R9;
    carm.contextarm32.R10:=lpContext.R10;
    carm.contextarm32.FP:=lpContext.FP;
    carm.contextarm32.IP:=lpContext.IP;
    carm.contextarm32.SP:=lpContext.SP;
    carm.contextarm32.LR:=lpContext.LR;
    carm.contextarm32.PC:=lpContext.PC and $fffffffe;
    carm.contextarm32.CPSR:=lpContext.CPSR;
    carm.contextarm32.ORIG_R0:=lpContext.ORIG_R0;

    CopyMemory(@carm.contextarm32.fpu.regs[0], @lpContext.fpu[0], 32*sizeof(qword));
    carm.contextarm32.fpu.control:=lpContext.fpureg;

    result:=c.setContext(processhandle, hThread, @carm, carm.contextsize);
  end;
end;

function GetThreadContextArm(hThread: THandle; var lpContext: TArmContext):  BOOL;
var
  carm: PNetworkContext=nil;
  c: TCEConnection=nil;
begin
  result:=false;

  c:=getConnection;
  if c<>nil then
  begin
    carm:=c.AllocateAndGetContext(processhandle, hThread);
    try
      if (carm<>nil) and (processhandler.SystemArchitecture=archARM) then
      begin
        if processhandler.is64Bit then
        begin
          lpContext.PC:=$64646464; //holder for 64 bit for now
        end
        else
        begin
          if carm^.contexttype<>networkContextType_Arm then
          begin
            OutputDebugString('Expected ARM context, received type '+carm^.contexttype.ToString);
            exit(false);
          end;

          lpContext.R0:=carm^.contextarm32.R0;
          lpContext.R1:=carm^.contextarm32.R1;
          lpContext.R2:=carm^.contextarm32.R2;
          lpContext.R3:=carm^.contextarm32.R3;
          lpContext.R4:=carm^.contextarm32.R4;
          lpContext.R5:=carm^.contextarm32.R5;
          lpContext.R6:=carm^.contextarm32.R6;
          lpContext.R7:=carm^.contextarm32.R7;
          lpContext.R8:=carm^.contextarm32.R8;
          lpContext.R9:=carm^.contextarm32.R9;
          lpContext.R10:=carm^.contextarm32.R10;
          lpContext.FP:=carm^.contextarm32.FP;
          lpContext.IP:=carm^.contextarm32.IP;
          lpContext.SP:=carm^.contextarm32.SP;
          lpContext.LR:=carm^.contextarm32.LR;
          lpContext.PC:=carm^.contextarm32.PC;
          lpContext.CPSR:=carm^.contextarm32.CPSR;
          lpContext.ORIG_R0:=carm^.contextarm32.ORIG_R0;

          if (lpContext.CPSR and (1 shl 5))<>0 then //Thumb bit
            lpContext.PC:=lpContext.PC or 1; //quick hack to identify that thumb is used


          CopyMemory(@lpContext.fpu[0], @carm^.contextarm32.fpu.regs[0], 32*sizeof(qword) );
          lpcontext.fpureg:=carm^.contextarm32.fpu.control;


          result:=true;
        end;
      end; //else use GetThreadContext
    finally
      if (carm<>nil) then
        FreeMemAndNil(carm);
    end;
  end;
end;


function GetThreadContextArm64(hThread: THandle; var lpContext: TArm64Context):  BOOL;
var
  carm64: PNetworkContext=nil;
  c: TCEConnection=nil;
begin
  result:=false;

  c:=getConnection;
  if c<>nil then
  begin
    carm64:=c.AllocateAndGetContext(processhandle, hThread);
    try

      if (carm64<>nil) and (processhandler.SystemArchitecture=archArm) then
      begin
        if processhandler.is64Bit then
        begin
          if carm64^.contexttype<>networkContextType_Arm64 then
          begin
            OutputDebugString('Expected ARM64 context, received type '+carm64^.contexttype.ToString);
            exit(false);
          end;

          lpContext.regs:=carm64^.contextarm64.regs;
          lpContext.SP:=carm64^.contextarm64.SP;
          lpContext.PC:=carm64^.contextarm64.PC;
          lpContext.PSTATE:=carm64^.contextarm64.PSTATE;
          copymemory(@lpContext.fp.vregs[0], @carm64^.contextarm64.fp.vregs[0],32*16);
          lpContext.fp.fpsr:=carm64^.contextarm64.fp.fpsr;
          {$ifdef darwin}
          lpContext.fp.fpcs:=carm64^.contextarm64.fp.fpcr;
          {$else}
          lpContext.fp.fpcr:=carm64^.contextarm64.fp.fpcr;
          {$endif}


          result:=true;
        end; //else use GetThreadContextArm()
      end; //else use GetThreadContext
    finally
      if (carm64<>nil) then
        FreeMemAndNil(carm64);
    end;
  end;
end;

function SetThreadContextArm64(hThread: THandle; const lpContext: TArm64Context):  BOOL;
var
  carm64: TNetworkContext;
  c: TCEConnection=nil;
begin
  c:=getConnection;
  if c<>nil then
  begin
    carm64.contextsize:=sizeof(TNetworkARM_64Context)+8;
    carm64.contexttype:=3; //arm64

    carm64.contextarm64.regs:=lpcontext.regs;
    carm64.contextarm64.SP:=lpcontext.SP;
    carm64.contextarm64.PC:=lpcontext.PC;
    carm64.contextarm64.PSTATE:=lpcontext.PSTATE;
    copymemory(@carm64.contextarm64.fp.vregs[0], @lpcontext.fp.vregs[0],32*16);
    carm64.contextarm64.fp.fpsr:=lpcontext.fp.fpsr;
    {$ifdef darwin}
    carm64.contextarm64.fp.fpcr:=lpcontext.fp.fpcs;
    {$else}
    carm64.contextarm64.fp.fpcr:=lpcontext.fp.fpcr;
    {$endif}

    result:=c.setContext(processhandle, hThread, @carm64, carm64.contextsize);
  end;
end;




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
begin
  exit(NetworkDebuggerInterface.SetThreadContextArm(hThread, lpContext));
end;

function TNetworkDebuggerInterface.SetThreadContextArm64(hThread: THandle; const lpContext: TArm64Context; isFrozenThread: Boolean=false):  BOOL;
begin
   exit(NetworkDebuggerInterface.SetThreadContextArm64(hThread, lpContext));
end;

function TNetworkDebuggerInterface.GetThreadContextArm64(hThread: THandle; var lpContext: TArm64Context; isFrozenThread: Boolean=false):  BOOL;
begin
  exit(NetworkDebuggerInterface.GetThreadContextArm64(hThread, lpContext));
end;

function TNetworkDebuggerInterface.GetThreadContextArm(hThread: THandle; var lpContext: TArmContext; isFrozenThread: Boolean=false):  BOOL;
begin
  exit(NetworkDebuggerInterface.GetThreadContextArm(hthread, lpcontext));
end;



function TNetworkDebuggerInterface.SetThreadContext(hThread: THandle; const lpContext: TContext; isFrozenThread: Boolean=false): BOOL;
begin
  exit(NetworkDebuggerInterface.SetThreadContext(hthread, lpcontext));
end;

function TNetworkDebuggerInterface.GetThreadContext(hThread: THandle; var lpContext: TContext; isFrozenThread: Boolean=false):  BOOL;
begin
 exit(NetworkDebuggerInterface.GetThreadContext(hthread, lpcontext));
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
  inherited create;

  //no software breakpoint for now
  fDebuggerCapabilities:=[dbcHardwareBreakpoint];

end;


end.

