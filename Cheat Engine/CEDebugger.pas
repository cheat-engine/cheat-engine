unit CEDebugger;
//OLD debugger. Obsolete , just here till all old references have been removed

{$MODE Delphi}

interface

uses {$ifdef darwin}
     macport, macportdefines,
     {$endif}
     {$ifdef windows}
     windows,
     {$endif}
     Classes,LCLIntf,sysutils,CEFuncProc,Messages,forms,SyncObjs,
     dialogs,controls,Graphics,NewKernelHandler,symbolhandler,StrUtils,
     ComCtrls ,Assemblerunit,addressparser, vmxfunctions;


type TReadonly = record
  pagebase: uint_ptr;
  pagesize: uint_ptr;
  Address: uint_ptr;
  size: integer;
  originalprotection:  dword;
end;

type tThreadEntry=record
  threadHandle: thandle;
  address: uint_ptr;
end;


type Process=record
  ProcessID: dword;
  running: boolean;
end;







type TDbgUIDebugActiveProcess = function(processhandle:THandle):boolean; stdcall;
type TDebugBreakProcess = function(processhandle:THandle):boolean; stdcall;
type TDebugActiveProcessStop= function(pid: dword):boolean; stdcall;
type TDebugSetProcessKillOnExit=function(KillOnExit: boolean):boolean; stdcall;
type TIsDebuggerPresent=function:boolean; stdcall;
type TntSuspendProcess=function(ProcessID:HANDLE):DWORD; stdcall;
type TntResumeProcess=function(ProcessID:HANDLE):DWORD; stdcall;




type
    SYSTEM_HANDLE_TABLE_ENTRY_INFO=record
      ProcessId: DWORD; //USHORT
      ObjectTypeIndex: UCHAR;
      HandleAttributes: UCHAR;
      HandleValue: USHORT;
      obj: pointer;
      GrantedAccess: DWORD;
    end;

    SYSTEM_HANDLE_INFORMATION=record
      HandleCount: DWORD;
      list: array [0..0] of SYSTEM_HANDLE_TABLE_ENTRY_INFO;
    end;
    PSYSTEM_HANDLE_INFORMATION=^SYSTEM_HANDLE_INFORMATION;

  TProcessBasicInformation = record
    ExitStatus : Longint;
    PebBaseAddress : Pointer;
    AffinityMask : DWORD;
    BasePriority : Longint;
    UniqueProcessId : DWORD;
    InheritedFromUniqueProcessId : DWORD
  end;

  NTSTATUS = LONG;


  KPRIORITY = LONG;
  KAFFINITY = ULONG_PTR;

  _THREAD_BASIC_INFORMATION = record // Information Class 0
    ExitStatus: NTSTATUS;
    TebBaseAddress: pointer;
    ClientId: CLIENT_ID;
    AffinityMask: KAFFINITY;
    Priority: KPRIORITY;
    BasePriority: KPRIORITY;
  end;
  THREAD_BASIC_INFORMATION = _THREAD_BASIC_INFORMATION;
  PTHREAD_BASIC_INFORMATION = ^THREAD_BASIC_INFORMATION;
  TThreadBasicInformation = THREAD_BASIC_INFORMATION;
  PThreadBasicInformation = ^TThreadBasicInformation;

  TProcessInfoClass=(
  ProcessBasicInformation,ProcessQuotaLimits,ProcessIoCounters,ProcessVmCounters,ProcessTimes,
  ProcessBasePriority,ProcessRaisePriority,ProcessDebugPort,ProcessExceptionPort,ProcessAccessToken,
  ProcessLdtInformation,ProcessLdtSize,ProcessDefaultHardErrorMode,ProcessIoPortHandlers,
  ProcessPooledUsageAndLimits,ProcessWorkingSetWatch,ProcessUserModeIOPL,ProcessEnableAlignmentFaultFixup,
  ProcessPriorityClass,ProcessWx86Information,ProcessHandleCount,ProcessAffinityMask,ProcessPriorityBoost,
  ProcessDeviceMap,ProcessSessionInformation,ProcessForegroundInformation,ProcessWow64Information,
  MaxProcessInfoClass);

  _THREADINFOCLASS = (
      ThreadBasicInformation,
      ThreadTimes,
      ThreadPriority,
      ThreadBasePriority,
      ThreadAffinityMask,
      ThreadImpersonationToken,
      ThreadDescriptorTableEntry,
      ThreadEnableAlignmentFaultFixup,
      ThreadEventPair_Reusable,
      ThreadQuerySetWin32StartAddress,
      ThreadZeroTlsCell,
      ThreadPerformanceCount,
      ThreadAmILastThread,
      ThreadIdealProcessor,
      ThreadPriorityBoost,
      ThreadSetTlsArrayAddress,
      ThreadIsIoPending,
      ThreadHideFromDebugger,
      ThreadBreakOnTermination, // was added in XP - used by RtlSetThreadIsCritical()
      MaxThreadInfoClass);
    THREADINFOCLASS = _THREADINFOCLASS;
    {.$ENDIF JWA_INCLUDEMODE}
    THREAD_INFORMATION_CLASS = THREADINFOCLASS;


    TThreadInfoClass = THREADINFOCLASS;



const SystemHandleInformation=16;

type TNtQuerySystemInformation=function(infoClass : dword; systemInformation : Pointer; SystemInformationLength : ULONG; returnLength : PULONG) : DWORD; stdcall;
type TNtQueryInformationProcess=function(Handle : THandle; infoClass : TProcessInfoClass; processInformation : Pointer; processInformationLength : ULONG; returnLength : PULONG) : DWORD; stdcall;
type TNtQueryInformationThread=function(Handle : THandle; infoClass : TThreadinfoClass; ThreadInformation: pointer; processInformationLength : ULONG; returnLength : PULONG) : DWORD; stdcall;


var //DebuggerThread: TDebugger;
   {$ifdef windows}

    DbgUIDebugActiveProcess:TDbgUIDebugActiveProcess;
    DebugBreakProcess:TDebugBreakProcess;
    DebugActiveProcessStop:TDebugActiveProcessStop;
    DebugSetProcessKillOnExit:TDebugSetProcessKillOnExit;
    IsDebuggerPresent:TIsDebuggerPresent;
    ntSuspendProcess: TntSuspendProcess;
    ntResumeProcess: tntResumeProcess;

    NtQuerySystemInformation: TNtQuerySystemInformation;
    NtQueryInformationProcess: TNtQueryInformationProcess;
    NtQueryInformationThread: TNtQueryInformationThread;
    DbgBreakPointLocation:ptrUint;


    krn: thandle;
    ntdlllib: thandle;
     {$endif}
    CRDebugging: TCriticalSection;




  function startdebuggerifneeded: boolean; overload;
  function startdebuggerifneeded(ask:boolean): boolean; overload;
  function DebugActiveProcessStopProstitute(x: dword): boolean;

implementation

uses debughelper,debuggertypedefinitions, debugeventhandler, MainUnit,frmFloatingPointPanelUnit,
     Memorybrowserformunit,disassembler,frmTracerUnit,foundcodeunit,kerneldebugger,
     advancedoptionsunit,formChangedAddresses,frmstacktraceunit,frmThreadlistunit,
     formdebugstringsunit,formsettingsunit,processwindowunit,plugin,processhandlerunit(*,frmCreatedProcessListUnit*), mainunit2;


resourcestring
  rsPleaseTargetAnotherProcess = 'Please target another process';
  rsYouMustFirstOpenAProcess = 'You must first open a process';
  rsThisWillAttachTheDebuggerOfCheatEngineToTheCurrent = 'This will attach the debugger of '+strCheatEngine+' to the current process.';
  rsDoNotCloseCE = 'If you close '+strCheatEngine+' while the game is running, the game will close too. Are you sure you want to do this?';
  rsContinue = 'Continue?';
  rsDebugError = 'I couldn''t attach the debugger to this process! You could try to open the process using the processpicker and try that! If that also doesn''t work check if '
    +'you have debugging rights.';

function DebugBreakProstitute(x: Thandle):boolean;
begin
  result:=false;
end;

function DebugSetProcessKillOnExitProtitute(x: boolean):boolean;
begin
  result:=false;
end;

function DebugActiveProcessStopProstitute(x: dword): boolean;
begin
  result:=false;
end;

function startdebuggerifneeded(ask:boolean): boolean; overload;
var mes: string;
    reS:boolean;
    i: integer;
begin
  result:=false;
  if processid=GetCurrentProcessId then raise exception.create(rsPleaseTargetAnotherProcess);


  if processhandle=0 then raise exception.create(rsYouMustFirstOpenAProcess);

  if (debuggerthread=nil) then
  begin
    {$ifdef windows}
    if @DebugActiveProcessStop=@DebugActiveProcessStopProstitute then
      mes:=rsThisWillAttachTheDebuggerOfCheatEngineToTheCurrent+' '+rsDoNotCloseCE
    else
    {$endif}
      mes:=rsThisWillAttachTheDebuggerOfCheatEngineToTheCurrent+' '+rsContinue;

    if ask then
      res:=Messagedlg(mes,mtConfirmation,[mbYes, mbNo],0)=mrYes
    else
      res:=true;

    if res then
    begin

      //start the debugger on the current process
      //check for a debugger
      try
        Debuggerthread:=TDebuggerThread.MyCreate2(processid);
      except
        raise EDebuggerAttachException.Create(rsDebugError);
      end;

      result:=true;
      exit;
    end
    else
    begin
      result:=false;
      exit;
    end;
  end;
  result:=true;

end;

function startdebuggerifneeded: boolean;  overload;
begin
  result:=startdebuggerifneeded(true);
end;


initialization
  {$ifdef windows}
  CRDebugging:=TCriticalSection.Create;

  krn := LoadLibrary('Kernel32.dll');
  if krn <> 0 then
  begin

    @DebugBreakProcess := GetProcAddress(krn, 'DebugBreakProcess');
    @DebugSetProcessKillOnExit :=GetProcAddress(krn,'DebugSetProcessKillOnExit');
    @DebugActiveProcessStop :=GetProcAddress(krn,'DebugActiveProcessStop');
    @IsDebuggerPresent:=GetProcAddress(krn,'IsDebuggerPresent');

    if @DebugBreakProcess=nil then
      @DebugBreakProcess:=@DebugBreakProstitute;

    if @DebugSetProcessKillOnExit=nil then
      @DebugSetProcessKillOnExit:=@DebugSetProcessKillOnExitProtitute;

    if @DebugActiveProcessStop=nil then
      @DebugActiveProcessStop:=@DebugActiveProcessStopProstitute;

  end;

  ntdlllib:=LoadLibrary('ntdll.dll');
  if ntdlllib<>0 then
  begin
    DbgUIDebugActiveProcess:=getprocaddress(ntdlllib,'DbgUiDebugActiveProcess');
    DbgBreakPointLocation:=ptrUint(getprocaddress(ntdlllib,'DbgBreakPoint'));

    NtQueryInformationProcess:=nil;
    NtQueryInformationProcess:=GetProcAddress(ntdlllib,'NtQueryInformationProcess');

    NtQueryInformationThread:=nil;
    NtQueryInformationThread:=GetProcAddress(ntdlllib,'NtQueryInformationThread');

    NtQuerySystemInformation:=nil;
    NtQuerySystemInformation:=GetProcAddress(ntdlllib,'NtQuerySystemInformation');

    ntsuspendprocess:=nil;
    ntsuspendprocess:=GetProcAddress(ntdlllib,'NtSuspendProcess');
    ntresumeprocess:=GetProcAddress(ntdlllib,'NtResumeProcess');
    freelibrary(ntdlllib);
  end;

  {$endif}

finalization
  if CRDebugging<>nil then
    CRDebugging.Free;

end.
