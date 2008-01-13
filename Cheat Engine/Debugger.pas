unit Debugger;
//just my place to play with all kinds of threading methods
//when i find out the best suited method I'll use that....

interface

uses Classes,windows,sysutils,cefuncproc,Messages,forms,SyncObjs,
     dialogs,controls,Graphics,NewKernelHandler,symbolhandler,StrUtils
     {$ifndef net}
     ,undochanges,assemblerunit,addressparser  //handled in the client version or not at all
     {$endif}
     {$ifdef netserver}      
     ,unit1
     ,idtcpserver
     {$endif};

const reg0set= $3;          //(00000000000000000000000000000011)
      reg1set= $C;          //(00000000000000000000000000001100)
      reg2set= $30;         //(00000000000000000000000000110000)
      reg3set= $c0;         //(00000000000000000000000011000000)
                                               


      debugexact= $300;
      gd_flag= $2000;       //(00000000000000000010000000000000)
      reg0w=   $10000;      //(00000000000000010000000000000000)
      reg0rw=  $30000;      //(00000000000000110000000000000000)
      reg0len2=$40000;      //(00000000000001000000000000000000)
      reg0len4=$c0000;      //(00000000000011000000000000000000)
      reg1w=   $100000;     //(00000000000100000000000000000000)
      reg1rw=  $300000;     //(00000000001100000000000000000000)
      reg1len2=$400000;     //(00000000010000000000000000000000)
      reg1len4=$c00000;     //(00000000110000000000000000000000)
      reg2w=   $1000000;    //(00000001000000000000000000000000)
      reg2rw=  $3000000;    //(00000011000000000000000000000000)
      reg2len2=$4000000;    //(00000100000000000000000000000000)
      reg2len4=$c000000;    //(00001100000000000000000000000000)
      reg3w=   $10000000;   //(00010000000000000000000000000000)
      reg3rw=  $30000000;   //(00110000000000000000000000000000)
      reg3len2=$40000000;   //(01000000000000000000000000000000)
      reg3len4=$c0000000;   //(11000000000000000000000000000000)
      reg0len0=0;
      reg1len0=0;
      reg2len0=0;
      reg3len0=0;

type TReadonly = record
  pagebase: dword;
  pagesize: dword;
  Address: dword;
  size: integer;
  originalprotection:  dword;
end;

type tThreadEntry=record
  threadHandle: thandle;
  address: dword;
end;


type Process=record
  ProcessID: dword;
  running: boolean;
end;

type tbreakpoint = record
  address:dword;
  originalbyte: byte;
end;

type tregistermodificationBP=record
  address:dword; //addres to break on
  change_eax:boolean;
  change_ebx:boolean;
  change_ecx:boolean;
  change_edx:boolean;
  change_esi:boolean;
  change_edi:boolean;
  change_ebp:boolean;
  change_esp:boolean;
  change_eip:boolean;
  change_cf:boolean;
  change_pf:boolean;
  change_af:boolean;
  change_zf:boolean;
  change_sf:boolean;
  change_of:boolean;
  new_eax:dword;
  new_ebx:dword;
  new_ecx:dword;
  new_edx:dword;
  new_esi:dword;
  new_edi:dword;
  new_ebp:dword;
  new_esp:dword;
  new_eip:dword;
  new_cf:boolean;
  new_pf:boolean;
  new_af:boolean;
  new_zf:boolean;
  new_sf:boolean;
  new_of:boolean;
end;
type PRegisterModificationBP=^TRegisterModificationBP;

type TNewProcedureData=record
  processid: dword;
  processhandle: dword;
  filehandle: dword;
  EntryPoint: dword;
  OriginalEntryByte: byte;
  DotNet: boolean;
  FirstBreak: boolean;
end;


type TDebugger = class(TThread)
  private
    originalbyte: byte;
    filename: String;
    parameters: string;
    CreateAProcess: boolean;
    OpenProcessID: Thandle;

    AddressFound: Dword;
    HideDebugger: boolean;
    HandleBreakpoints: boolean;

    CanUseDebugRegs: boolean;
    createdusingprocesswindow:boolean;

    CurrentProcess: integer;


    {$ifdef netserver}
    coderecords: array of dword;
    {$endif}

    procedure addtochangeslist;
    procedure ProcessCreated;

    procedure RestoreNTOpenProcess;

    procedure ResetBreakpoint;
    procedure SetSingleStepping(Threadid: dword);
    procedure AddDebugString;
    function injectcode(AddressOfEntryPoint:dword;processhandle:thandle):dword;
    function handledebuggerplugins(devent:PDebugEvent):integer;

    procedure tracersync;
    function tracer(devent: _Debug_EVENT):boolean;

  public
    //semaphore: THandle;
    Processes: array of process;
    Newprocesses: array of TNewProcedureData;


    pausedthreadhandle: thandle;

    running: boolean;
    continueprocess: boolean;
    continuehow: integer;

    DRRegs: _CONTEXT;

    FindWriter2: boolean;


    attaching: boolean;
    attached: boolean;
    context: _CONTEXT;
    debugging: boolean;
    stepping: boolean;
    HowToContinue: integer;
    removed: boolean;

    readonly: TReadonly;
    readonlyset: boolean;
    readonlyremoved: boolean;

    findreaderset: boolean;
    findreader: TReadOnly;
    findreaderremoved:boolean;

    alsowrites: boolean;

    breakpointset: boolean;
    breakpointaddress: dword;
    lastbreakpoint: dword;
    temps: string;

    whattodo: integer; //ignore, continue, showwindow etc....

    userisdebugging: boolean;
    userbreakpoints: array of dword;
    registermodificationBPs: array of tregistermodificationBP;

    int3userbreakpoints: array of tbreakpoint;
    int3CEBreakpoint: TBreakpoint;

    threadlist: array of array [0..4] of dword;
    threadinfo: array of dword;


    testje: string;

    breakpointlistsection: TCriticalSection;
    threadentrys: array of tThreadEntry;

    traceaddress: dword;
    tracecount: integer;
    Constructor MyCreate(filenm: String);
    Constructor MyCreate2(processID: THandle);
    destructor Destroy; override;
    procedure Execute; override;
    procedure Debuginfo;

    procedure Updateregisters;
    procedure FoundOne;
    procedure RemoveBreakpoint;

    procedure suspendallthreads;
    procedure resumeallthreads;

  end;


type TDbgUIDebugActiveProcess = function(processhandle:THandle):boolean; stdcall;
type TDebugBreakProcess = function(processhandle:THandle):boolean; stdcall;
type TDebugActiveProcessStop= function(pid: dword):boolean; stdcall;
type TDebugSetProcessKillOnExit=function(KillOnExit: boolean):boolean; stdcall;
type TIsDebuggerPresent=function:boolean; stdcall;
type TntSuspendProcess=function(ProcessID:Dword):DWORD; stdcall;
type TntResumeProcess=function(ProcessID:Dword):DWORD; stdcall;




type
TProcessBasicInformation = record
ExitStatus : Longint;
PebBaseAddress : Pointer;
AffinityMask : DWORD;
BasePriority : Longint;
UniqueProcessId : DWORD;
InheritedFromUniqueProcessId : DWORD
end;


  TProcessInfoClass=(
  ProcessBasicInformation,ProcessQuotaLimits,ProcessIoCounters,ProcessVmCounters,ProcessTimes,
  ProcessBasePriority,ProcessRaisePriority,ProcessDebugPort,ProcessExceptionPort,ProcessAccessToken,
  ProcessLdtInformation,ProcessLdtSize,ProcessDefaultHardErrorMode,ProcessIoPortHandlers,
  ProcessPooledUsageAndLimits,ProcessWorkingSetWatch,ProcessUserModeIOPL,ProcessEnableAlignmentFaultFixup,
  ProcessPriorityClass,ProcessWx86Information,ProcessHandleCount,ProcessAffinityMask,ProcessPriorityBoost,
  ProcessDeviceMap,ProcessSessionInformation,ProcessForegroundInformation,ProcessWow64Information,
  MaxProcessInfoClass);
type TNtQueryInformationProcess=function(
Handle : THandle;
infoClass : TProcessInfoClass;
processInformation : Pointer;
processInformationLength : ULONG;
returnLength : PULONG
) : DWORD; stdcall;

var DebuggerThread: TDebugger;
    Semaphore: Thandle;

    DbgUIDebugActiveProcess:TDbgUIDebugActiveProcess;
    DebugBreakProcess:TDebugBreakProcess;
    DebugActiveProcessStop:TDebugActiveProcessStop;
    DebugSetProcessKillOnExit:TDebugSetProcessKillOnExit;
    IsDebuggerPresent:TIsDebuggerPresent;
    ntSuspendProcess: TntSuspendProcess;
    ntResumeProcess: tntResumeProcess;

    NtQueryInformationProcess: TNtQueryInformationProcess;
    IsDebuggerPresentLocation:dword;
    DbgBreakPointLocation:dword;


    krn: thandle;
    ntdlllib: thandle;

    CRDebugging: TCriticalSection;


  function startdebuggerifneeded: boolean; overload;
  function startdebuggerifneeded(ask:boolean): boolean; overload;
  function breakthread(threadhandle: thandle):boolean;
  function DebugActiveProcessStopProstitute(x: dword): boolean;
  function ToggleBreakpoint(address:dword):boolean;

implementation

uses {$ifndef net}Mainunit,Memorybrowserformunit,{$endif}disassembler{$ifndef net},frmTracerUnit,foundcodeunit,debugger2,advancedoptionsunit,formChangedAddresses,frmstacktraceunit,frmThreadlistunit,formdebugstringsunit,formsettingsunit,processwindowunit,plugin,frmCreatedProcessListUnit{$endif};

function ToggleBreakpoint(address:dword):boolean;
{$ifndef net}
procedure setbreakpoints;
var i: integer;
begin
  with debuggerthread do
  begin
    //set the debug breakpoint
    suspend;
    DRRegs.ContextFlags:=CONTEXT_DEBUG_REGISTERS;
    DRRegs.Dr7:=reg0set or reg1set or reg2set or reg3set;

    DRRegs.Dr0:=0;
    drregs.dr1:=0;
    drregs.dr2:=0;

    for i:=0 to length(userbreakpoints)-1 do
    begin
      if i=0 then DRRegs.Dr0:=userbreakpoints[0] else
      if i=1 then DRRegs.Dr1:=userbreakpoints[1] else
      if i=2 then DRRegs.Dr2:=userbreakpoints[2];
    end;

    for i:=0 to length(threadlist)-1 do
    begin
      suspendthread(debuggerthread.threadlist[i][1]);
      setthreadcontext(debuggerthread.threadlist[i][1],debuggerthread.DRRegs);
      resumethread(debuggerthread.threadlist[i][1]);
    end;

    Resume;
  end;

end;

var i,j,k: integer;
    a,original,written: dword;

    int3: byte;
{$endif}

begin
{$ifndef net}
  result:=false;

  if foundcodedialog<>nil then
    raise exception.Create('I can''t do that! You are currently using one of the code finder options, please, stop it first');  

  if debuggerthread2<>nil then
  begin
    for i:=0 to 3 do
    begin
      if debuggerthread2.breakpoints[i]=address then
      begin
        outputdebugstring('Going to disable this changereg breakpoint');
        if debuggerthread2.breakpointchanges[i].address<>0 then
        begin
          StopRegisterChange(i);
          debuggerthread2.breakpointchanges[i].address:=0;
        end;

        debuggerthread2.breakpoints[i]:=0;
        debuggerthread2.setbreakpoints;
        memorybrowser.updatebplist;

        result:=false;
        for j:=0 to 3 do
          if debuggerthread2.breakpoints[j]<>0 then
          begin
            result:=true;
            break;
          end;

        if not result then freeandnil(debuggerthread2);

        exit;
      end;
    end;
  end;


  if not startdebuggerifneeded then exit;
  int3:=$cc;


  //still here so the debugger has been attached to the process or i've fucked up again

  with debuggerthread do
  begin

    try
      WaitForSingleObject(semaphore,infinite);
      userisdebugging:=true;

      if formsettings.rbDebugAsBreakpoint.checked then
      begin
        //try to see if it already is in the list
        for i:=0 to length(userbreakpoints)-1 do
          if userbreakpoints[i]=address then
          begin
            //if it is then remove the breakpoint
            for j:=i to length(userbreakpoints)-2 do
              userbreakpoints[j]:=userbreakpoints[j+1];
            setlength(userbreakpoints,length(userbreakpoints)-1);

            //also remove the registermodificationBP
            for j:=0 to length(debuggerthread.registermodificationBPs)-1 do
              if registermodificationBPs[j].address=address then
              begin
                for k:=i to length(debuggerthread.registermodificationBPs)-2 do
                  registermodificationBPs[k]:=registermodificationBPs[k+1];

                setlength(registermodificationBPs,length(registermodificationBPs)-1);
                break;
              end;


            setbreakpoints;
            releasesemaphore(semaphore,1,nil);
            result:=false;
            exit;
          end;


        if length(userbreakpoints)=3 then
          raise exception.Create('The current implementation doesn''t support more than 3 breakpoints. (4 debug registers, but 1 is needed for stepping-over code)');

        setlength(userbreakpoints,length(userbreakpoints)+1);
        userbreakpoints[length(userbreakpoints)-1]:=address;

        setbreakpoints;
        result:=true;
      end
      else
      begin
        //int3 breakpoints
        //find it in the breakpointlist
        for i:=0 to length(int3userbreakpoints)-1 do
        begin
          if int3userbreakpoints[i].address=address then
          begin
            //remove it
            RewriteCode(processhandle,address,@int3userbreakpoints[i].originalbyte,1);

            for j:=i to length(int3userbreakpoints)-2 do
            begin
              int3userbreakpoints[j].address:=int3userbreakpoints[j+1].address;
              int3userbreakpoints[j].originalbyte:=int3userbreakpoints[j+1].originalbyte;
            end;

            setlength(int3userbreakpoints,length(int3userbreakpoints)-1);
            result:=false;
            exit;
          end;
        end;

        //no, it's not in the list, so ADD:
        setlength(int3userbreakpoints,length(int3userbreakpoints)+1);
        int3userbreakpoints[length(int3userbreakpoints)-1].address:=address;

        readprocessmemory(processhandle,pointer(address),@int3userbreakpoints[length(int3userbreakpoints)-1].originalbyte,1,a);
        if a=1 then
          RewriteCode(processhandle,address,@int3,1)
        else
          setlength(int3userbreakpoints,length(int3userbreakpoints)-1);

        result:=true;
      end;
    finally
      memorybrowser.updatebplist;
      releasesemaphore(semaphore,1,nil);
    end;
  end;

  {$endif}
end;

procedure RemoveDebuggerDetection(processhandle: thandle);
var Newfunction: array [0..2] of byte;
    original,written: dword;
begin
  if IsDebuggerPresentLocation=0 then exit;
  {
    xor eax,eax
    ret
  }
  Newfunction[0]:=$31;
  NewFunction[1]:=$c0;
  NewFunction[2]:=$c3;

  RewriteCode(processhandle,IsDebuggerPresentLocation,@newfunction[0],3);
end;

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
  if processhandle=0 then raise exception.create('You must first open a process');

  {$ifndef netserver}
  if debuggerthread2<>nil then
  begin
    if messagedlg('The kerneldebugger is currently active. Enabling the default windows debugger will cause the kernel debugger to terminate itself. Continue?',mtwarning,[mbyes,mbno],0)=mrno then exit;
    freeandnil(debuggerthread2);
  end;
  {$endif}

  if (debuggerthread=nil) or (not debuggerthread.attached) then
  begin
    if @DebugActiveProcessStop=@DebugActiveProcessStopProstitute then
      mes:='This will attach the debugger of Cheat Engine to the current process. If you close Cheat Engine while the game is running, the game will close too. Are you sure you want to do this?'
    else
      mes:='This will attach the debugger of Cheat Engine to the current process. Continue?';

    {$ifndef net}
    if ask then
      res:=Messagedlg(mes,mtConfirmation,[mbYes, mbNo],0)=mrYes
    else
      res:=true;

    if res then
    {$endif}
    begin
      {$ifndef net}
      if not advancedoptions.Pausebutton.Down then
        @ntsuspendprocess:=nil; //lets use the debugger for this
      {$endif}

      //start the debugger on the current process
      //check for a debugger
      Debuggerthread:=TDebugger.MyCreate2(processid);
      while (debuggerthread<>nil) and DebuggerThread.attaching do sleep(10);  //give him some time
      if not debuggerthread.attached then
      begin
        debuggerthread.Free;
        debuggerthread:=nil;
        raise exception.Create('I couldn''t attach the debugger to this process! You could try to open the process using the processpicker and try that! If that also doesn''t work check if you have debugging rights.');
      end;

      {$ifndef netserver}
      //Enable the debugger screen for the memorybrowser
      memorybrowser.splitter1.Visible:=true;
      memorybrowser.panel1.Visible:=true;

      memorybrowser.view1.Visible:=true;
      memorybrowser.Debug1.Visible:=true;
      memorybrowser.Splitter2.Visible:=true;
      memorybrowser.RegisterView.Visible:=true;

      Memorybrowser.UpdateRegisterview;
      {$endif}

      result:=true;
      exit;
    end
    {$ifndef net}
    else
    begin
      result:=false;
      exit;
    end;
    {$endif}
  end;
  result:=true;
end;

function startdebuggerifneeded: boolean;  overload;
begin
  result:=startdebuggerifneeded(true);
end;

function Breakthread(threadhandle:thandle):boolean;
var c: _context;
begin
  if GetSystemType<3 then raise exception.Create('You can''t break on this OS. Windows ME or later is required. (You CAN set breakpoints at key locations)');

  zeromemory(@c,sizeof(c));

  c.ContextFlags:=CONTEXT_FULL or CONTEXT_FLOATING_POINT or CONTEXT_DEBUG_REGISTERS;

  debuggerthread.suspend;
  suspendthread(threadhandle);
  if not getthreadcontext(threadhandle,c) then exit;

  debuggerthread.userisdebugging:=true;

  with debuggerthread do
  begin
    DRRegs.ContextFlags:=CONTEXT_DEBUG_REGISTERS;
    DRRegs.Dr7:=reg0set or reg1set or reg2set or reg3set;
    DRRegs.Dr0:=0;
    drregs.dr1:=0;
    drregs.dr2:=0;
    DRRegs.Dr3:=c.Eip;
    if setthreadcontext(threadhandle,drregs) then result:=true;
  end;


  resumethread(threadhandle);
  debuggerthread.Resume;
end;


Constructor TDebugger.MyCreate(filenm: String);
begin
  {$ifndef netserver}
  if debuggerthread2<>nil then raise exception.Create('Please stop the kernelmode debugging routines and breakpoints before starting this debugger');

  if formsettings.cbUndoMemoryChanges.checked then CheckForChanges; //place this line at important places

  if formsettings.cbBreakOnAttach.checked and (ProcessWindow<>nil) then    //created using the process list
    createdusingprocesswindow:=true;
  {$endif}

  userisdebugging:=false;
  {$ifndef netserver}
  handlebreakpoints:=formsettings.CheckBox1.Checked;
  hidedebugger:=formsettings.checkbox1.checked;
  canusedebugregs:=formsettings.rbDebugAsBreakpoint.checked;
  formdebugstrings.listbox1.clear;
  {$endif}
    
  attaching:=true;
  filename:=filenm;
  Inputquery(filename,'Parameters:',parameters);

  debugging:=true;
  processhandle:=2;
  howtocontinue:=0;
  createAprocess:=true;
  inherited Create(false);   //I know, I know, these initalizes could also be done in the execute part.
end;

Constructor TDebugger.MyCreate2(processID: THandle);
var
  debugportoffset:dword;
  debugport:dword;
  ar:dword;
  peprocess: dword;
begin

  {$ifndef netserver}
  if debuggerthread2<>nil then
  begin
    if messagedlg('The kerneldebugger is currently active. Enabling the default windows debugger will cause the kernel debugger to terminate itself. Continue?',mtwarning,[mbyes,mbno],0)=mrno then exit;
    freeandnil(debuggerthread2);
  end;
  
  if formsettings.cbUndoMemoryChanges.checked then CheckForChanges; //place this line at important places

  if formsettings.cbBreakOnAttach.checked and (ProcessWindow<>nil) then    //created using the process list
    createdusingprocesswindow:=true;
  {$endif}

  userisdebugging:=false;

  {$ifndef netserver}
  handlebreakpoints:=formsettings.cbHandleBreakpoints.checked;
  hidedebugger:=formsettings.checkbox1.checked;
  canusedebugregs:=formsettings.rbDebugAsBreakpoint.checked;
  formdebugstrings.listbox1.clear;
  {$endif}

  attaching:=true;
  debugging:=true;
  howtocontinue:=0;
  createAprocess:=false;
  OpenprocessID:=processid;

  processhandle:=OpenProcess(process_all_access,false,processid);

  {$ifndef net}
  //check for a debugger

  if (getsystemtype>5) and (formsettings.cbKernelOpenProcess.Checked) then
  begin
    debugportoffset:=GetDebugportOffset;

    if debugportoffset<>0 then
    begin
      peprocess:=getpeprocess(processid);
      if peprocess<>0 then
      begin
        debugport:=0;
        if KernelReadProcessMemory(processhandle,pointer(peprocess+debugportoffset),@debugport,4,ar) then
        begin
          //readable
          if (debugport<>0) and
             (messagedlg('There is already a debugger attached to this process. Do you want to override it ? (Only works in winxp+)',mtconfirmation,[mbyes,mbno],0)=mryes) then
          begin
            debugport:=0;
            KernelWriteProcessMemory(processhandle,pointer(peprocess+debugportoffset),@debugport,4,ar);
            //debugger gone
          end;
        end;
      end;
    end; //else no way to check or fix it
  end;

  {$endif}
  inherited Create(false);   //I know, I know, these initalizes could also be done in the execute part.
end;

destructor TDebugger.Destroy;
begin
  //clear all breakpoints
  RemoveBreakpoint;

  inherited Destroy;
end;

procedure TDebugger.tracersync;
{$ifndef net}
var s: string;
    i: integer;
    d: TTraceDebugInfo;
{$endif}
begin
{$ifndef net}
  if frmtracer<>nil then
  begin
    s:=disassemble(context.eip);
    i:=posex('-',s);
    i:=posex('-',s,i+1);
    s:=copy(s,i+2,length(s));
    s:=inttohex(context.eip,8)+' - '+s;

    d:=TTraceDebugInfo.Create;
    d.c:=context;
    frmtracer.ListBox1.Items.AddObject(s,d);
  end;
{$endif}
end;

function TDebugger.tracer(devent: _Debug_EVENT):boolean;
var i,c: integer;
    b: boolean;
    h: thandle;
    hid: dword;
begin
  c:=0;
  b:=true;
  result:=false;

  hid:=devent.dwThreadId;
  for i:=0 to length(threadlist)-1 do
  begin
    if threadlist[i,0]=devent.dwThreadId then
    begin
      h:=threadlist[i,1];
      break;
    end;
  end;


  while b and (c<tracecount) do
  begin
    for i:=0 to length(threadlist)-1 do
    begin
      if threadlist[i,0]=devent.dwThreadId then
      begin
        context.ContextFlags:=CONTEXT_FULL;
        getthreadcontext(threadlist[i,1],context);
        break;
      end;
    end;

    if devent.dwThreadId=hid then
      synchronize(tracersync);

    inc(c);
    if c<tracecount then
    begin
      if devent.dwThreadId=hid then
        SetSingleStepping(hid);
        
      b:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);
      if b then b:=waitfordebugevent(devent,4000);
    end;
  end;

  if b then
    result:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);
end;

procedure TDebugger.RestoreNTOpenProcess;
var NTOpenProcessPos: dword;
    MyNTOpenProcessPos: dword;
    x: dword;
    y: array [0..4] of byte;
    mykernel: thandle;
begin
  NTOpenProcessPos:=dword(getprocaddress(ntdlllib,'NtOpenProcess'));

  LoadDbk32;
  if GetLoadedState then
  begin
    MyNTOpenProcessPos:=dword(getprocaddress(DarkByteKernel,'NOP'));

    If MyNTOpenProcessPos<>0 then
    begin
      if VirtualProtect(pointer(NTOpenProcessPos-50),55,PAGE_EXECUTE_READWRITE,x) then
      begin
        y[0]:=$e9;
        pdword(@y[1])^:=MyNTOpenProcessPos-NTOpenProcessPos-5;

        try
          asm
            push edi
            push esi
            lea esi,y[0]
            mov edi,NTOpenProcessPos
            movsd
            movsb
            pop esi
            pop edi
          end;
        except

        end;
      end;
    end;
  end;
end;


procedure TDebugger.AddDebugString;
begin
  {$ifndef netserver}
  formdebugstrings.listbox1.Items.add(temps);
  {$endif}
end;

procedure TDebugger.addtochangeslist;
begin
  {$ifndef netserver}
  with memorybrowser do
  begin
    EAXv:=context.Eax;
    EBXv:=context.Ebx;
    ECXv:=context.Ecx;
    EDXv:=context.Edx;
    ESIv:=context.Esi;
    EDIv:=context.Edi;
    EBPv:=context.Ebp;
    ESPv:=context.Esp;
    EIPv:=context.Eip;
  end;

  try
    lastbreakpoint:=getaddress(temps);
  except
    //
  end;
  if frmchangedaddresses.Changedlist.Items.IndexOf(inttohex(lastbreakpoint,8))=-1 then
    frmchangedaddresses.Changedlist.Items.Add(inttohex(lastbreakpoint,8));
  {$endif}
end;

procedure TDebugger.debuginfo;
begin

end;

procedure TDebugger.ProcessCreated;
begin
  {$ifndef netserver}
  if frmCreatedProcessList=nil then
  begin
    frmCreatedProcesslist:=tfrmcreatedprocesslist.create(nil);
    frmCreatedProcesslist.show;
  end;

  frmcreatedprocesslist.ListBox1.Items.Add(IntToHex(newprocesses[currentprocess].processid,8));
  {$endif}

end;

procedure TDebugger.FoundOne;
var desc,opcode: string;
    address: dword;

    {$ifdef netserver}
    i: integer;
    threadlist: tlist;
    output: array [0..41] of byte;
    {$endif}
begin
  {$ifndef netserver}
  if foundcodedialog<>nil then
  with foundcodedialog do
  begin
    address:=addressfound;
    opcode:=disassemble(address,desc);

    setlength(coderecords,length(coderecords)+1);
    coderecords[length(coderecords)-1].address:=addressfound;
    coderecords[length(coderecords)-1].size:=address-addressfound;
    coderecords[length(coderecords)-1].opcode:=opcode;
    coderecords[length(coderecords)-1].desciption:=desc;


    coderecords[length(coderecords)-1].eax:=context.Eax;
    coderecords[length(coderecords)-1].ebx:=context.Ebx;
    coderecords[length(coderecords)-1].ecx:=context.Ecx;
    coderecords[length(coderecords)-1].edx:=context.Edx;
    coderecords[length(coderecords)-1].esi:=context.Esi;
    coderecords[length(coderecords)-1].edi:=context.Edi;
    coderecords[length(coderecords)-1].ebp:=context.Ebp;
    coderecords[length(coderecords)-1].esp:=context.Esp;
    coderecords[length(coderecords)-1].eip:=context.Eip;
    Foundcodelist.Items.Add(opcode);
  end;
  {$else}
  //send the state information to the client(s)
  //FoundCode(Address: dword;eax:dword; ebx:dword; ecx:dword; edx:dword;esi:dword;edi:dword;ebp:dword;esp:dword;eip:dword)

  setlength(coderecords,length(coderecords)+1);
  coderecords[length(coderecords)-1]:=addressfound;
  output[0]:=SC_FoundCode;
  pdword(@output[1])^:=addressfound;
  pdword(@output[5])^:=context.Eax;
  pdword(@output[9])^:=context.Ebx;
  pdword(@output[13])^:=context.Ecx;
  pdword(@output[17])^:=context.Edx;
  pdword(@output[21])^:=context.Esi;
  pdword(@output[25])^:=context.Edi;
  pdword(@output[29])^:=context.Ebp;
  pdword(@output[33])^:=context.Esp;
  pdword(@output[37])^:=context.Eip;


  threadlist:=form1.idtcpserver1.Threads.LockList;
  try
    for i:=0 to threadlist.count-1 do
      TIdPeerThread(threadlist[i]).Connection.WriteBuffer(output[0],41);
  finally
    form1.idtcpserver1.Threads.UnlockList;
  end;
  {$endif}
end;

procedure tdebugger.suspendallthreads;
var i: integer;
begin
  for i:=0 to length(threadlist)-1 do
    suspendthread(threadlist[i,1]);
end;

procedure tdebugger.resumeallthreads;
var i: integer;
begin
  for i:=0 to length(threadlist)-1 do
    resumethread(threadlist[i,1]);
end;


procedure TDebugger.UpdateRegisters;
var temp: string;
    i: integer;
begin
 //removed for a while
 {$ifndef netserver}
  with memorybrowser do
  begin
    //enable debug mode
    run1.Enabled:=true;
    step1.Enabled:=true;
    stepover1.Enabled:=true;
    runtill1.Enabled:=true;
    stacktrace1.Enabled:=true;

    for i:=0 to length(threadlist)-1 do
    begin
      if threadlist[i,1]=pausedthreadhandle then
      begin
        caption:='Memory Viewer - Currently debugging thread '+inttohex(threadlist[i,0],8);
        break;
      end;
    end;

    if frmstacktrace<>nil then
    begin
      frmstacktrace.stacktrace(debuggerthread.pausedthreadhandle,debuggerthread.context);
    end;

    Disassembleraddress:=context.Eip;
    dselected:=context.eip;

    temp:='EAX '+IntToHex(context.Eax,8);
    if temp<>eaxlabel.Caption then
    begin
      eaxlabel.Font.Color:=clred;
      eaxlabel.Caption:=temp;
    end else eaxlabel.Font.Color:=clWindowText;

    temp:='EBX '+IntToHex(context.Ebx,8);
    if temp<>ebxlabel.Caption then
    begin
      ebxlabel.Font.Color:=clred;
      ebxlabel.Caption:=temp;
    end else ebxlabel.Font.Color:=clWindowText;

    temp:='ECX '+IntToHex(context.ECx,8);
    if temp<>eCxlabel.Caption then
    begin
      eCXlabel.Font.Color:=clred;
      eCXlabel.Caption:=temp;
    end else eCXlabel.Font.Color:=clWindowText;

    temp:='EDX '+IntToHex(context.EDx,8);
    if temp<>eDxlabel.Caption then
    begin
      eDxlabel.Font.Color:=clred;
      eDxlabel.Caption:=temp;
    end else eDxlabel.Font.Color:=clWindowText;

    temp:='ESI '+IntToHex(context.ESI,8);
    if temp<>eSIlabel.Caption then
    begin
      eSIlabel.Font.Color:=clred;
      eSIlabel.Caption:=temp;
    end else eSIlabel.Font.Color:=clWindowText;

    temp:='EDI '+IntToHex(context.EDI,8);
    if temp<>eDIlabel.Caption then
    begin
      eDIlabel.Font.Color:=clred;
      eDIlabel.Caption:=temp;
    end else eDIlabel.Font.Color:=clWindowText;

    temp:='EBP '+IntToHex(context.EBP,8);
    if temp<>eBPlabel.Caption then
    begin
      eBPlabel.Font.Color:=clred;
      eBPlabel.Caption:=temp;
    end else eBPlabel.Font.Color:=clWindowText;

    temp:='ESP '+IntToHex(context.ESP,8);
    if temp<>eSPlabel.Caption then
    begin
      eSPlabel.Font.Color:=clred;
      eSPlabel.Caption:=temp;
    end else eSPlabel.Font.Color:=clWindowText;

    temp:='EIP '+IntToHex(context.EIP,8);
    if temp<>eIPlabel.Caption then
    begin
      eIPlabel.Font.Color:=clred;
      eIPlabel.Caption:=temp;
    end else eIPlabel.Font.Color:=clWindowText;

    temp:='CS '+IntToHex(context.SEGCS,4);
    if temp<>CSlabel.Caption then
    begin
      CSlabel.Font.Color:=clred;
      CSlabel.Caption:=temp;
    end else CSlabel.Font.Color:=clWindowText;

    temp:='DS '+IntToHex(context.SEGDS,4);
    if temp<>DSlabel.Caption then
    begin
      DSlabel.Font.Color:=clred;
      DSlabel.Caption:=temp;
    end else DSLabel.Font.Color:=clWindowText;

    temp:='SS '+IntToHex(context.SEGSS,4);
    if temp<>SSlabel.Caption then
    begin
      SSlabel.Font.Color:=clred;
      SSlabel.Caption:=temp;
    end else SSlabel.Font.Color:=clWindowText;

    temp:='ES '+IntToHex(context.SEGES,4);
    if temp<>ESlabel.Caption then
    begin
      ESlabel.Font.Color:=clred;
      ESlabel.Caption:=temp;
    end else ESlabel.Font.Color:=clWindowText;

    temp:='FS '+IntToHex(context.SEGFS,4);
    if temp<>FSlabel.Caption then
    begin
      FSlabel.Font.Color:=clred;
      FSlabel.Caption:=temp;
    end else FSlabel.Font.Color:=clWindowText;

    temp:='GS '+IntToHex(context.SEGGS,4);
    if temp<>GSlabel.Caption then
    begin
      GSlabel.Font.Color:=clred;
      GSlabel.Caption:=temp;
    end else GSlabel.Font.Color:=clWindowText;

    temp:='CF '+IntToStr(GetBitOf(context.EFLAgs,0));
    if temp<>cflabel.Caption then
    begin
      CFlabel.Font.Color:=clred;
      CFlabel.caption:=temp;
    end else cflabel.Font.Color:=clWindowText;

    temp:='PF '+IntToStr(GetBitOf(context.EFlags,2));
    if temp<>Pflabel.Caption then
    begin
      Pflabel.Font.Color:=clred;
      Pflabel.caption:=temp;
    end else Pflabel.Font.Color:=clWindowText;

    temp:='AF '+IntToStr(GetBitOf(context.EFlags,4));
    if temp<>Aflabel.Caption then
    begin
      Aflabel.Font.Color:=clred;
      Aflabel.caption:=temp;
    end else Aflabel.Font.Color:=clWindowText;

    temp:='ZF '+IntToStr(GetBitOf(context.EFlags,6));
    if temp<>Zflabel.Caption then
    begin
      Zflabel.Font.Color:=clred;
      Zflabel.caption:=temp;
    end else Zflabel.Font.Color:=clWindowText;

    temp:='SF '+IntToStr(GetBitOf(context.EFlags,7));
    if temp<>Sflabel.Caption then
    begin
      Sflabel.Font.Color:=clred;
      Sflabel.caption:=temp;
    end else Sflabel.Font.Color:=clWindowText;

    temp:='DF '+IntToStr(GetBitOf(context.EFlags,10));
    if temp<>Dflabel.Caption then
    begin
      Dflabel.Font.Color:=clred;
      Dflabel.caption:=temp;
    end else Dflabel.Font.Color:=clWindowText;

    temp:='OF '+IntToStr(GetBitOf(context.EFlags,11));
    if temp<>Oflabel.Caption then
    begin
      Oflabel.Font.Color:=clred;
      Oflabel.caption:=temp;
    end else Oflabel.Font.Color:=clWindowText;


    EAXv:=context.Eax;
    EBXv:=context.Ebx;
    ECXv:=context.Ecx;
    EDXv:=context.Edx;
    ESIv:=context.ESi;
    EDIv:=context.Edi;
    EBPv:=context.Ebp;
    ESPv:=context.Esp;
    EIPv:=context.Eip;

    updatedisassemblerview;
  end;
  {$endif}

end;

procedure TDebugger.SetSingleStepping(Threadid: dword);
var i: integer;
begin
  for i:=0 to length(threadlist)-1 do
    if threadlist[i,0]=ThreadId then
    begin
      context.ContextFlags:=CONTEXT_FULL;
      getthreadcontext(threadlist[i,1],context);

      context.ContextFlags:=CONTEXT_FULL; //CONTEXT_CONTROL;
      context.EFlags:=context.EFlags or $100;
      setthreadcontext(threadlist[i,1],context);
      break;
    end;
end;

procedure TDebugger.ResetBreakpoint;
var zerobreak: _context;
    i: integer;
    int3: byte;
    original,a,written:dword;
begin
  if canusedebugregs then
  for i:=0 to length(threadlist)-1 do
  begin
    suspendthread(threadlist[i,1]);
    setthreadcontext(threadlist[i,1],drregs);
    resumethread(threadlist[i,1]);
  end
  else
  begin
    int3:=$cc;
    for i:=0 to length(int3userbreakpoints)-1 do
      RewriteCode(processhandle,int3userbreakpoints[i].address,@int3,1);

    if int3CEBreakpoint.address>0 then
      RewriteCode(processhandle,int3CEBreakpoint.address,@int3,1);
      
  end;
end;

procedure TDebugger.RemoveBreakpoint;
var zerobreak: _context;
    i: integer;
    original,written,a:dword;
begin
  if canusedebugregs then
  begin
    zeromemory(@zerobreak,sizeof(zerobreak));
    zerobreak.ContextFlags:=CONTEXT_DEBUG_REGISTERS;
    zerobreak.Dr7:=reg0set or reg1set or reg2set or reg3set;

    for i:=0 to length(threadlist)-1 do
    begin
      suspendthread(threadlist[i,1]);
      setthreadcontext(threadlist[i,1],zerobreak);
      resumethread(threadlist[i,1]);
    end;
  end
  else
  begin
    //temporarily disable all breakpoints
    for i:=0 to length(int3userbreakpoints)-1 do
      RewriteCode(processhandle,int3userbreakpoints[i].address,@int3userbreakpoints[i].originalbyte,1);

    if int3CEBreakpoint.address>0 then
    begin
      RewriteCode(processhandle,int3CEbreakpoint.address,@int3cebreakpoint.originalbyte,1);
      int3CEBreakpoint.address:=0;
    end;
  end;
end;


function TDebugger.handledebuggerplugins(devent:PDebugEvent):integer;
begin
  {$ifndef net}
  //go through the list of debugger plugins
  result:=pluginhandler.handledebuggerplugins(devent);
  {$endif}
end;

//I always knew viruses are usefull
function TDebugger.injectcode(AddressOfEntryPoint:dword;processhandle:thandle):dword;
{$ifndef net}
var LoadLibraryPtr: pointer;
    GetProcAddressPtr: Pointer;

    injectedlocation: pointer;
    h: Thandle;

    inject: array [0..255] of byte;
    x:dword;

    outp:TAssemblerBytes;
    position,position2: dword;

    dllLocation: string;
    startaddresS: dword;
    IHWCIloc: dword;
{$endif}
begin
  {$ifndef net}
  h:=LoadLibrary('Kernel32.dll');
  if h=0 then raise exception.Create('No kernel32.dll loaded');

  try
    GetProcAddressPtr:=GetProcAddress(h,'GetProcAddress');
    if getprocaddressptr=nil then raise exception.Create('GetProcAddress not found');

    LoadLibraryPtr:=GetProcAddress(h,'LoadLibraryA');
    if LoadLibraryptr=nil then raise exception.Create('LoadLibraryA not found');

    injectedlocation:=VirtualAllocEx(processhandle,nil,4096,MEM_COMMIT,PAGE_EXECUTE_READWRITE);

    if injectedlocation=nil then raise exception.Create('Failed to allocate memory');

    dlllocation:=extractfilepath(application.exename)+'CEHOOK.DLL';

    position:=dword(injectedlocation);
    position2:=0;
    copymemory(@inject[0],pchar(dllLocation+#0),length(dllLocation)+1);
    inc(position,length(dllLocation)+1);
    inc(position2,length(dlllocation)+1);

    ihwciloc:=position;
    copymemory(@inject[position2],pchar('IHWCI'#0),6);
    inc(position,6);
    inc(position2,6);
    startaddress:=position;

    assemble('PUSHAD',position,outp);
    copymemory(@inject[position2],outp,length(outp));
    inc(position,length(outp));
    inc(position2,length(outp));

    //loadlibrary(cehook);
    assemble('PUSH '+IntToHex(dword(injectedlocation),8),position,outp);
    copymemory(@inject[position2],outp,length(outp));
    inc(position,length(outp));
    inc(position2,length(outp));

    assemble('CALL '+IntToHex(dword(LoadLibraryPtr),8),position,outp);
    copymemory(@inject[position2],outp,length(outp));
    inc(position,length(outp));
    inc(position2,length(outp));

    //getprocaddress
    assemble('PUSH '+IntToHex(ihwciloc,8),position,outp);
    copymemory(@inject[position2],outp,length(outp));
    inc(position,length(outp));
    inc(position2,length(outp));

    assemble('PUSH EAX',position,outp);
    copymemory(@inject[position2],outp,length(outp));
    inc(position,length(outp));
    inc(position2,length(outp));

    assemble('CALL '+IntToHex(dword(GetProcAddressPtr),8),position,outp);
    copymemory(@inject[position2],outp,length(outp));
    inc(position,length(outp));
    inc(position2,length(outp));

    assemble('CALL EAX',position,outp);
    copymemory(@inject[position2],outp,length(outp));
    inc(position,length(outp));
    inc(position2,length(outp));

    assemble('POPAD',position,outp);
    copymemory(@inject[position2],outp,length(outp));
    inc(position,length(outp));
    inc(position2,length(outp));


    //jump to original start address
    assemble('JMP '+IntToHex(AddressOfEntryPoint,8),position,outp);
    copymemory(@inject[position2],outp,length(outp));
    inc(position,length(outp));
    inc(position2,length(outp));


    //call the routine

    writeprocessmemory(processhandle,injectedlocation,@inject[0],position2,x);
    result:=startaddress;
    //showmessage('Injected code at: '+IntToHex(dword(injectedlocation),8)+' startaddress='+IntToHex(startaddress,8));
  finally
    FreeLibrary(h);
  end;


  {$endif}
end;


procedure TDebugger.Execute;
var startupinfo:_STARTUPINFOA;
    int3: byte;
    devent: _Debug_EVENT;
    processinfo:_Process_information;       //make an array of this
    i,j: integer;
    original: dword;
    tobefrozen: integer;  //-1 is none
    notinlist: boolean;
    tid,thnd: dword;

    //----
    temp: dword;
    desc,opcode: string;

    seperator: integer;
    fb: integer;
    nb: integer;
    address: string;
    offset:dword;

    errorlog: textfile;
    times: integer;
    debugpatch: integer;
    previousexception: dword;
    mbi : _MEMORY_BASIC_INFORMATION;

    hthr: thandle;

    asciistring: pansichar;
    unicodestring: pwidechar;
    bytesread: dword;
    err:string;

    found: boolean;

    a,b,c: dword;
    ab: byte;
    SelectorEntry:_LDT_ENTRY;

    oldcontext: _CONTEXT;

    exefile: File;
    dosheader: ^IMAGE_DOS_HEADER;
    Optionalheader: ^IMAGE_OPTIONAL_HEADER;
    ExeHeader: array [0..4095] of byte;
    AddressOfEntryPoint:dword;

    dotnet: boolean;
    firstbreak: boolean;
    creationhandleD: boolean;

begin
  AddressOfEntryPoint:=0;
  setlength(processes,0);
  creationhandleD:=false;

  zeromemory(@drregs,sizeof(drregs));
  drregs.ContextFlags:=CONTEXT_DEBUG_REGISTERS;

  previousexception:=0;
  times:=0;
  debugpatch:=0;

  int3:=$cc;
  breakpointset:=false;
  attached:=false;
  readonlyset:=false;

  processhandle:=0;

  tobefrozen:=-1;
  //GetStartUpInfo(STARTUPINFO);
  FillMemory(@STARTUPINFO,sizeof(STARTUPINFO),0);
  STARTUPINFO.cb :=sizeof(STARTUPINFO);

  try

  if createaprocess then
  begin
    //create a process
    filemode:=0;
    assignfile(exefile,filename);
    try
      reset(exefile,1);
    except
      attaching:=false;
      err:='The file can''t be opened!';
      messagebox(0,pchar(err),'Create failure',mb_ok);
      exit;
    end;

    try
      temp:=0;
      blockread(exefile,exeheader[0],4096,temp);
      if temp>0 then
      begin
        dosheader:=@exeheader[0];
        optionalheader:=pointer(dword(dosheader)+dosheader._lfanew+24);

        AddressOfEntryPoint:=optionalheader.ImageBase+optionalheader.AddressOfEntryPoint;
      end;
    finally
      closefile(exefile);
    end;

    if not Createprocess(nil,pchar(filename+' '+parameters),nil,nil,false,(CREATE_SUSPENDED or DEBUG_PROCESS or NORMAL_PRIORITY_CLASS),nil,nil,Startupinfo,processinfo) then
    begin
      attaching:=false;

      err:='Cheat Engine failed to create the process. (Error='+pchar(IntToStr(GetLastError))+')';
      messagebox(0,pchar(err),'Create failure',mb_ok);

      {$ifndef net}
      postmessage(mainform.Handle,WM_USER+1,0,0);
      {$endif}
      exit;
    end;

    readprocessmemory(processinfo.hProcess,pointer(addressofentrypoint),@originalbyte,1,temp);

 {   dotneT:=false;
    firstbreak:=true;
    if originalbyte=$ff then
    begin
      readprocessmemory(processinfo.hProcess,pointer(addressofentrypoint+1),@ab,1,temp);
      if ab=$25 then dotnet:=true;
    end;

    if not dotnet then writeprocessmemory(processinfo.hProcess,pointeR(AddressOfEntryPoint),@int3,1,temp);
       }
  //  messagebox(0,pchar(IntToHex(Context.Eax,8)),pchar(IntTOHex(Context.Eax,8)),MB_OK	);
    resumethread(processinfo.hThread);
  end
  else
  begin


//    BOOL DebugActiveProcess(DWORD dwProcessId)=
    {
      if(DbgUiConnectToDbg())
      {
        HANDLE hProcess = ProcessIdToHandle(dwProcessId);

        if(hProcess)
        {
          DbgUiDebugActiveProcess(hProcess);
          NtClose(hProcess);
        }
//      }
//      return FALSE;
//    }

    //attach to a running process
    //rewrite nt

    {$ifdef netserver}
    RestoreNTOpenProcess;
    {$else}
    if formsettings.cbKernelOpenProcess.checked then
      RestoreNTOpenProcess;
    {$endif}


    if not DebugActiveProcess(Openprocessid) then
    begin
      attaching:=false;

      {$ifndef net}
      err:='Cheat Engine failed to attach to the process. (Error='+IntToStr(GetLastError)+')';
      messagebox(0,pchar(err),'Attach failure',mb_ok);
      postmessage(mainform.Handle,WM_USER+1,0,0);  //set debuggerthread to nil
      {$endif}
      exit;
    end;

  end;

  DebugSetProcessKillOnExit(true);  //WHY DOESN'T IT WORK LIKE I WANT TO?
  running:=true;


  while not terminated and debugging do
  begin
    if WaitForDebugEvent(devent,4000) then  //4 seconds wont cause a hog I hope
    begin

        {$ifndef net}
        if handledebuggerplugins(@devent)=1 then
          continue;
        {$endif}



      //working on a debug thing

        currentprocess:=-1;
        for i:=0 to length(newprocesses)-1 do
        begin
          if newprocesses[i].processid=devent.dwProcessId then
          begin
            currentprocess:=i;
            break;
          end;
        end;

        if length(newprocesses)>0 then
          processhandle:=newprocesses[length(newprocesses)-1].processhandle;

        case devent.dwDebugEventCode of
          EXCEPTION_DEBUG_EVENT:
            begin
              if newprocesses[currentprocess].firstbreak then
              begin
                if newprocesses[currentprocess].dotnet then
                begin
                  //jump near  (I assume the next byte is a $25
                  if readprocessmemory(newprocesses[currentprocess].processhandle,pointer(addressofentrypoint+2),@a,4,temp) then
                  begin
                    readprocessmemory(newprocesses[currentprocess].processhandle,pointer(a),@b,4,temp);
                    newprocesses[currentprocess].EntryPoint:=b;

                    readprocessmemory(newprocesses[currentprocess].processhandle,pointer(b),@newprocesses[currentprocess].OriginalEntryByte,1,temp);
                    writeprocessmemory(newprocesses[currentprocess].processhandle,pointeR(b),@int3,1,temp);
                  end;
                end;

                if hidedebugger then RemoveDebuggerDetection(newprocesses[currentprocess].processhandle);
                newprocesses[currentprocess].firstbreak:=false;
              end;

            //  if devent.Excdeeption.dwFirstChance=0 then MessageBox(0,'seconde time','second time',MB_OK);
              //this is called after the threadlist has been initialised

              for i:=0 to length(threadlist)-1 do
                if threadlist[i,0]=devent.dwThreadId then
                begin
                  context.ContextFlags:=CONTEXT_FULL;
                  getthreadcontext(threadlist[i,1],context);
                  pausedthreadhandle:=threadlist[i,1];
                  break;
                end;



              if createaprocess and (context.eip=newprocesses[currentprocess].EntryPoint+1) then
              begin
                creationHandled:=true;

                writeprocessmemory(newprocesses[currentprocess].processhandle,pointeR(newprocesses[currentprocess].EntryPoint),@newprocesses[currentprocess].OriginalEntryByte  ,1,temp);
                context.ContextFlags:=context_full;
                outputdebugstring('Injecting dll');
                context.Eip:=injectcode(newprocesses[currentprocess].EntryPoint,newprocesses[currentprocess].processhandle)+1;
                SetThreadcontext(pausedthreadhandle,context);


                if createdusingprocesswindow and (currentprocess=0) then
                begin
                  //set a breakpoint at the AddressOfEntryPoint
                  if not attached then
                  begin
                    attached:=true;
                    attaching:=false;

                    synchronize(ProcessCreated);
                  end;

                  debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);
                  continue;
                end;
              end;


              if attached and {hidedebugger and} (not userisdebugging) then
              if readprocessmemory(newprocesses[currentprocess].processhandle,pointeR(context.eip-1),@ab,1,a) then
              begin
                if ab=int3 then
                begin
                  if (not createaprocess) or (creationHandled) then
                  begin
                    if (context.Eip-1)<>DbgBreakPointLocation then
                    begin
                      if handlebreakpoints then
                        debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE)
                      else
                        debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_EXCEPTION_NOT_HANDLED);

                      outputdebugstring('err1');
                      continue;
                    end;
                  end;
                end;
              end;

              if not attached then
              begin
                attached:=true;
                attaching:=false;
                synchronize(ProcessCreated);
              end;


              i:=0;
              //find out what exception it is
              if devent.Exception.ExceptionRecord.ExceptionCode=EXCEPTION_BREAKPOINT then
              begin
                
                if userisdebugging then
                begin
                  //check if this is a exception breakpoint that was made by cheat engine
                  addressfound:=dword(devent.Exception.ExceptionRecord.ExceptionAddress);

                  found:=(int3CEBreakpoint.address=addressfound);

                  crdebugging.enter;
                  try
                    for i:=0 to length(int3userbreakpoints)-1 do
                      if (int3userbreakpoints[i].address=addressfound) then found:=true;
                  finally
                    crdebugging.Leave;
                  end;

                  if found or createdusingprocesswindow then
                  begin
                    //we have a confirmation....
                    //set the byte back
                    if not createdusingprocesswindow then
                      removebreakpoint;

                    createdusingprocesswindow:=false;
                    if int3CEBreakpoint.address=addressfound then
                    begin
                      int3CEBreakpoint.address:=0;
                      int3CEBreakpoint.originalbyte:=0;
                    end;

                    //set eip back
                    dec(context.Eip);

                    notinlist:=true;
                    crdebugging.enter;
                    try

                      for i:=0 to length(registermodificationBPs)-1 do
                      begin
                        if registermodificationBPs[i].address=context.eip then
                        begin
                          notinlist:=false;

                          //modify the context of this thread according to the data in registermodificationBPs[i]
                          if registermodificationBPs[i].change_eax then context.Eax:=registermodificationBPs[i].new_eax;
                          if registermodificationBPs[i].change_ebx then context.Ebx:=registermodificationBPs[i].new_ebx;
                          if registermodificationBPs[i].change_ecx then context.Ecx:=registermodificationBPs[i].new_ecx;
                          if registermodificationBPs[i].change_edx then context.Edx:=registermodificationBPs[i].new_edx;
                          if registermodificationBPs[i].change_esi then context.Esi:=registermodificationBPs[i].new_esi;
                          if registermodificationBPs[i].change_edi then context.Edi:=registermodificationBPs[i].new_edi;
                          if registermodificationBPs[i].change_ebp then context.Ebp:=registermodificationBPs[i].new_ebp;
                          if registermodificationBPs[i].change_esp then context.Esp:=registermodificationBPs[i].new_esp;
                          if registermodificationBPs[i].change_eip then context.Eip:=registermodificationBPs[i].new_eip;

                          if registermodificationBPs[i].change_cf then
                            if registermodificationBPs[i].new_cf then
                              context.EFlags:=context.EFlags or $1 //enable the bit
                            else
                              context.EFlags:=context.EFlags and (not $1);

                          if registermodificationBPs[i].change_pf then
                            if registermodificationBPs[i].new_pf then
                              context.EFlags:=context.EFlags or $4 //enable the bit
                            else
                              context.EFlags:=context.EFlags and (not $4);

                          if registermodificationBPs[i].change_af then
                            if registermodificationBPs[i].new_af then
                              context.EFlags:=context.EFlags or $10 //enable the bit
                            else
                              context.EFlags:=context.EFlags and (not $10);

                          if registermodificationBPs[i].change_zf then
                            if registermodificationBPs[i].new_zf then
                              context.EFlags:=context.EFlags or $40 //enable the bit
                            else
                              context.EFlags:=context.EFlags and (not $40);

                          if registermodificationBPs[i].change_sf then
                            if registermodificationBPs[i].new_sf then
                              context.EFlags:=context.EFlags or $80 //enable the bit
                            else
                              context.EFlags:=context.EFlags and (not $80);

                          if registermodificationBPs[i].change_of then
                            if registermodificationBPs[i].new_of then
                              context.EFlags:=context.EFlags or $800 //enable the bit
                            else
                              context.EFlags:=context.EFlags and (not $800);

                          context.ContextFlags:=CONTEXT_FULL;
                          setthreadcontext(pausedthreadhandle,context);

                          SetSingleStepping(devent.dwThreadId);
                          debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);

                          j:=devent.dwThreadId;
                          if not WaitForDebugEvent(devent,10000) then application.MessageBox('userbreakpoint bug','Cheat Engine Debugger',0);

                          while j<>devent.dwthreadid do
                          begin
                            debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);
                            if not WaitForDebugEvent(devent,10000) then application.MessageBox('userbreakpoint bug','Cheat Engine Debugger',0);
                          end;
                          resetbreakpoint;

                          debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);
                          break;
                        end;
                      end; //registermodificiation loop

                    finally
                      crdebugging.Leave;
                    end;

                    if (traceaddress<>0) and (self.traceaddress=dword(devent.Exception.ExceptionRecord.ExceptionAddress)) then //do tracecount steps
                    begin
                      context.ContextFlags:=CONTEXT_FULL;
                      setthreadcontext(pausedthreadhandle,context);

                      if tracecount>0 then
                      begin
                        debugging:=tracer(devent);
                        tracecount:=0;
                      end else debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);
                      resetbreakpoint;

                      continue;
                    end;

                    if not notinlist then continue; //exit, we've done what was needed


                    context.ContextFlags:=CONTEXT_FULL;
                    setthreadcontext(pausedthreadhandle,context);


                    continueprocess:=false;
                    continuehow:=0;
                    suspendallthreads;

                    synchronize(updateregisters);

                    running:=false;
                    while not continueprocess do sleep(10);

                    WaitForSingleObject(semaphore,infinite);
                    running:=true; //meaning, keep your fucking hands of the registers

                    resumeallthreads;

                    //make a step
                    SetSingleStepping(devent.dwThreadId);
                    debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);

                    j:=devent.dwThreadId;
                    if not WaitForDebugEvent(devent,10000) then application.MessageBox('userbreakpoint bug','Cheat Engine Debugger',0);

                    while j<>devent.dwthreadid do
                    begin
                      debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);
                      if not WaitForDebugEvent(devent,10000) then application.MessageBox('userbreakpoint bug','Cheat Engine Debugger',0);
                    end;

                    //set the breakpoint back if needed
                    resetbreakpoint;
                    releasesemaphore(semaphore,1,nil);

                    if continuehow=1 then //it was a step, so
                    begin
                      continueprocess:=false;
                      continuehow:=0;
                      suspendallthreads;

                      for j:=0 to length(threadlist)-1 do
                        if threadlist[j,0]=devent.dwThreadId then
                        begin
                          context.ContextFlags:=CONTEXT_FULL;
                          getthreadcontext(threadlist[j,1],context);
                          pausedthreadhandle:=threadlist[j,1];
                          break;
                        end;


                      synchronize(updateregisters);

                      running:=true; //meaning, keep your fucking hands of the registers
                      while not continueprocess do sleep(10);
                      running:=true; //meaning, keep your fucking hands of the registers

                      resumeallthreads;

                      //make a step
                      if continuehow=1 then SetSingleStepping(devent.dwThreadId);
                    end;
                  end;

                  debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);
                  continue;
                end;


                debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);
                Outputdebugstring(pchar('Breakpoint:'+IntToHex(dword(devent.Exception.ExceptionRecord.ExceptionAddress),8)));
                continue;
              end;

              //check if it is because the single stepping mode is enabled
              if devent.Exception.ExceptionRecord.ExceptionCode=EXCEPTION_SINGLE_STEP then
              begin //it is in single step mode
              //--added
                if (traceaddress<>0) and (dword(devent.Exception.ExceptionRecord.ExceptionAddress)=traceaddress) then
                begin
                  RemoveBreakpoint;
                  if tracecount>0 then
                    debugging:=tracer(devent)
                  else
                    debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);

                  tracecount:=0;
                  ResetBreakpoint;
                  continue;
                end;

                if findwriter2 then
                begin
                  addressfound:=dword(devent.Exception.ExceptionRecord.ExceptionAddress);

                  offset:=addressfound;
                  opcode:=disassemble(offset,desc);


                  if pos('REP',opcode)>0 then
                  begin
                    if context.Ecx=0 then addressfound:=previousopcode(addressfound);

                  end else
                  addressfound:=previousopcode(addressfound);

                  //check if the address is in the list
                  notinlist:=true;
                  try
                    {$ifndef net}
                    for i:=0 to length(foundcodedialog.coderecords)-1 do
                      if foundcodedialog.coderecords[i].address=addressfound then //if it is in the list then set notinlist to false and go out of the loop
                      begin
                        notinlist:=false;
                        break;
                      end;
                    {$else}
                    //check if it is in the list
                    for i:=0 to length(coderecords)-1 do
                      if coderecords[i]=addressfound then
                      begin
                        notinlist:=false;
                        break;
                      end;
                    {$endif}

                  except
                    //list got shortened or invalid
                  end;

                  if notinlist then synchronize(foundone); //add this memory address to the foundcode window.
                end;

                if findreaderset then
                begin
                  WaitForSingleObject(semaphore,infinite);
                  if findreaderset then VirtualProtectEx(processhandle,pointer(findreader.Address),findreader.size,PAGE_NOACCESS,original);
                  releasesemaphore(semaphore,1,nil);
                end;

                if breakpointset then
                begin
                  if breakpointaddress=dword(devent.Exception.ExceptionRecord.ExceptionAddress) then
                  begin
                    //find out what the address points to
                    offset:=breakpointaddress;
                    opcode:=disassemble(offset,desc);

                    fb:=pos('[',opcode);
                    if fb>0 then
                    begin
                      nb:=pos(']',opcode);

                      if nb>fb then //just a simple check to verify the opcode is ok
                      begin
                        temps:=copy(opcode,fb+1,nb-fb-1);
                        //lastbreakpoint:=address;
                        synchronize(addtochangeslist);
                        //and add that address to a list

                        removebreakpoint;
                        SetSingleStepping(devent.dwThreadId);
                        debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);

                        if WaitForDebugEvent(devent,10000)=false then application.MessageBox('error2-3','error2-3',0);
                        while devent.Exception.ExceptionRecord.ExceptionCode<>EXCEPTION_SINGLE_STEP do
                        begin
                          if devent.dwDebugEventCode=EXCEPTION_DEBUG_EVENT then
                          begin
                            //I hate it when this happens
                            debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_EXCEPTION_NOT_HANDLED);
                            outputdebugstring('err2');

                            break;
                          end;
                        end;

                        if breakpointset then resetbreakpoint;
                      end;
                    end;

                  end;

                end;

                if userisdebugging then
                begin
                  //got a breakpoint
                  //set the breakpoint
                  if not canusedebugregs then
                    removebreakpoint;


                  if (canusedebugregs) and (drregs.dr3=context.Eip) then
                  begin
                    drregs.Dr3:=0;
                    resetbreakpoint;
                  end;

                  //check if it is a regmodification bp
                  crdebugging.Enter;
                  try
                    notinlist:=true;
                    for i:=0 to length(registermodificationBPs)-1 do
                    begin
                      if registermodificationBPs[i].address=context.eip then
                      begin
                        notinlist:=false;

                        //modify the context of this thread according to the data in registermodificationBPs[i]
                        if registermodificationBPs[i].change_eax then context.Eax:=registermodificationBPs[i].new_eax;
                        if registermodificationBPs[i].change_ebx then context.Ebx:=registermodificationBPs[i].new_ebx;
                        if registermodificationBPs[i].change_ecx then context.Ecx:=registermodificationBPs[i].new_ecx;
                        if registermodificationBPs[i].change_edx then context.Edx:=registermodificationBPs[i].new_edx;
                        if registermodificationBPs[i].change_esi then context.Esi:=registermodificationBPs[i].new_esi;
                        if registermodificationBPs[i].change_edi then context.Edi:=registermodificationBPs[i].new_edi;
                        if registermodificationBPs[i].change_ebp then context.Ebp:=registermodificationBPs[i].new_ebp;
                        if registermodificationBPs[i].change_esp then context.Esp:=registermodificationBPs[i].new_esp;
                        if registermodificationBPs[i].change_eip then context.Eip:=registermodificationBPs[i].new_eip;

                        if registermodificationBPs[i].change_cf then
                          if registermodificationBPs[i].new_cf then
                            context.EFlags:=context.EFlags or $1 //enable the bit
                          else
                            context.EFlags:=context.EFlags and (not $1);

                        if registermodificationBPs[i].change_pf then
                          if registermodificationBPs[i].new_pf then
                            context.EFlags:=context.EFlags or $4 //enable the bit
                          else
                            context.EFlags:=context.EFlags and (not $4);

                        if registermodificationBPs[i].change_af then
                          if registermodificationBPs[i].new_af then
                            context.EFlags:=context.EFlags or $10 //enable the bit
                          else
                            context.EFlags:=context.EFlags and (not $10);

                        if registermodificationBPs[i].change_zf then
                          if registermodificationBPs[i].new_zf then
                            context.EFlags:=context.EFlags or $40 //enable the bit
                          else
                            context.EFlags:=context.EFlags and (not $40);

                        if registermodificationBPs[i].change_sf then
                          if registermodificationBPs[i].new_sf then
                            context.EFlags:=context.EFlags or $80 //enable the bit
                          else
                            context.EFlags:=context.EFlags and (not $80);

                        if registermodificationBPs[i].change_of then
                          if registermodificationBPs[i].new_of then
                            context.EFlags:=context.EFlags or $800 //enable the bit
                          else
                            context.EFlags:=context.EFlags and (not $800);

                        context.ContextFlags:=CONTEXT_FULL;
                        setthreadcontext(pausedthreadhandle,context);

                        removebreakpoint;
                        SetSingleStepping(devent.dwThreadId);
                        debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);

                        j:=devent.dwThreadId;
                        if not WaitForDebugEvent(devent,10000) then application.MessageBox('userbreakpoint bug','Cheat Engine Debugger',0);

                        while j<>devent.dwthreadid do
                        begin
                          debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);
                          if not WaitForDebugEvent(devent,10000) then application.MessageBox('userbreakpoint bug','Cheat Engine Debugger',0);
                        end;
                        resetbreakpoint;

                        debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);
                        break;
                      end;
                    end;

                  finally
                    crdebugging.Leave;
                  end;

                  if not notinlist then continue;

                  //-----------------------------------


                  continueprocess:=false;
                  continuehow:=0;
                  suspendallthreads;

                  synchronize(updateregisters);

                  running:=false;
                  while not continueprocess do sleep(10);
                  running:=true; //meaning, keep your fucking hands of the registers

                  resumeallthreads;

                  removebreakpoint;
                  SetSingleStepping(devent.dwThreadId);
                  debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);

                  i:=devent.dwThreadId;
                  if not WaitForDebugEvent(devent,10000) then application.MessageBox('userbreakpoint bug','Cheat Engine Debugger',0);

                  while i<>devent.dwthreadid do
                  begin
                    debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);
                    if not WaitForDebugEvent(devent,10000) then application.MessageBox('userbreakpoint bug','Cheat Engine Debugger',0);

                  end;

                  resetbreakpoint;

                  //get new context info
                  for i:=0 to length(threadlist)-1 do
                    if threadlist[i,0]=devent.dwThreadId then
                    begin
                     context.ContextFlags:=CONTEXT_FULL;
                     getthreadcontext(threadlist[i,1],context);
                     pausedthreadhandle:=threadlist[i,1];
                     break;
                    end;

                  if ((canusedebugregs) and (drregs.Dr3=context.Eip)) or (continuehow=1) then
                  begin

                    if (canusedebugregs) and (drregs.dr3=context.Eip) then
                    begin
                      drregs.Dr3:=0;
                      resetbreakpoint;
                    end;

                    synchronize(updateregisters);
                    continueprocess:=false;
                    continuehow:=0;
                    running:=false;
                    while not continueprocess do;
                    running:=true;

                    if continuehow=1 then SetSingleStepping(devent.dwThreadId);
                  end;

                end;

                if not canusedebugregs then
                  resetbreakpoint;


                //it's a single step exception (most likely made by me, so I'd better fix it)
                debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);
                continue;
              end;

              //if it was a access violation, and it was a WRITE access violation then
              if readonlyset and
                 (devent.Exception.ExceptionRecord.ExceptionCode=EXCEPTION_ACCESS_VIOLATION) and
                 (devent.Exception.ExceptionRecord.ExceptionInformation[0]=1)
                 then
              begin
                //now check if this address is in our locked array?
                if (devent.Exception.ExceptionRecord.ExceptionInformation[1]>=readonly.pagebase) and
                   (devent.Exception.ExceptionRecord.ExceptionInformation[1]<=readonly.pagebase+readonly.pagesize) then
                begin
                  //the address that created an exception is in a page I locked

                  //check if the exception is at the same spot as the entry in the read-only list
                  if (devent.Exception.ExceptionRecord.ExceptionInformation[1]>=readonly.Address-readonly.size+1) and
                     (devent.Exception.ExceptionRecord.ExceptionInformation[1]<=readonly.Address+readonly.size-1) then
                  begin
                    //it is!!!
                    addressfound:=dword(devent.Exception.ExceptionRecord.ExceptionAddress);

                    //check if the address is in the list
                    notinlist:=true;
                    //go through the list

                    try
                      {$ifndef net}
                      for i:=0 to length(foundcodedialog.coderecords)-1 do
                        if foundcodedialog.coderecords[i].address=addressfound then //if it is in the list then set notinlist to false and go out of the loop
                        begin
                          notinlist:=false;
                          break;
                        end;
                     {$else}
                     //check if it is in the list
                     for i:=0 to length(coderecords)-1 do
                       if coderecords[i]=addressfound then
                       begin
                         notinlist:=false;
                         break;
                       end;
                    {$endif}
                    except

                    end;


                    if notinlist then synchronize(foundone); //add this memory address to the foundcode window.

                  end;

                  //set the protection back to the original, in single stepping mode
                  VirtualProtectEx(processhandle,pointer(readonly.Address),readonly.size,readonly.originalprotection,original);

                  //set the process to single stepping
                  //find the threadhandle

                  SetSingleStepping(devent.dwThreadId);

                  debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);

                  if WaitForDebugEvent(devent,30000) then
                  while debugging and readonlyset and (devent.Exception.ExceptionRecord.ExceptionCode<>EXCEPTION_SINGLE_STEP) do
                  begin
                    inc(debugpatch);
                    //This SHOULD never come up...
                    if devent.dwDebugEventCode=EXCEPTION_DEBUG_EVENT then   //in case of a thread that does something
                    begin
                      for i:=0 to length(threadlist)-1 do
                        if threadlist[i,0]=devent.dwThreadId then
                        begin
                          hthr:=threadlist[i,1];
                          context.ContextFlags:=CONTEXT_FULL;
                          getthreadcontext(hthr,context);

                          context.ContextFlags:=CONTEXT_FULL; //CONTEXT_CONTROL;
                          context.EFlags:=context.EFlags or $100;
                          setthreadcontext(hthr,context);
                          break;
                        end;
                      debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_EXCEPTION_NOT_HANDLED);
                      outputdebugstring('err3');

                      break;
                    end;

                    for i:=0 to length(threadlist)-1 do
                      if threadlist[i,0]=devent.dwThreadId then
                      begin
                        hthr:=threadlist[i,1];
                        context.ContextFlags:=CONTEXT_FULL;
                        getthreadcontext(hthr,context);

                        context.ContextFlags:=CONTEXT_FULL; //CONTEXT_CONTROL;
                        context.EFlags:=context.EFlags or $100;
                        setthreadcontext(hthr,context);
                        break;
                      end;

                    debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);
                    if WaitForDebugEvent(devent,30000)=false then application.MessageBox('Write Exception Error. (2)','Debugger error',0)
                  end; {while}

                  //set the protection back to read-only if the readonly flag is still needed
                  WaitForSingleObject(semaphore,infinite);
                  if readonlyset then VirtualProtectEx(processhandle,pointer(readonly.Address),readonly.size,PAGE_EXECUTE_READ,original);
                  releasesemaphore(semaphore,1,nil);

                  if devent.Exception.ExceptionRecord.ExceptionCode<>EXCEPTION_SINGLE_STEP then continue;

                  //debugloghelp
                  inc(times);
                  previousexception:=devent.Exception.ExceptionRecord.ExceptionInformation[1];
                end;

                //release the semaphore so other threads that want to use the rea-only list can access this

                //the access violation was created by me, so handle it myself, and not the app
                debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);
                continue;
              end; {access violation}


              //read access violations
              if findreaderset then
              begin
                if (devent.Exception.ExceptionRecord.ExceptionCode=EXCEPTION_ACCESS_VIOLATION) and
                   ((devent.Exception.ExceptionRecord.ExceptionInformation[0]=0) or (alsowrites))  //0=read
                   then //0=read
                begin
                  //check if the exception is at the same spot as the entry in the read-only list
                  if ((devent.Exception.ExceptionRecord.ExceptionInformation[1]>=findreader.Address) and
                     (devent.Exception.ExceptionRecord.ExceptionInformation[1]<findreader.Address+findreader.size))
                     or
                     ((devent.Exception.ExceptionRecord.ExceptionInformation[1]<=findreader.Address) and
                     (devent.Exception.ExceptionRecord.ExceptionInformation[1]>findreader.Address-findreader.size))
                  then
                  begin
                    //it is!!!
                    addressfound:=dword(devent.Exception.ExceptionRecord.ExceptionAddress);

                    //check if the address is in the list
                    notinlist:=true;
                    //go through the list

                    try
                      {$ifndef net}
                      for i:=0 to length(foundcodedialog.coderecords)-1 do
                        if foundcodedialog.coderecords[i].address=addressfound then //if it is in the list then set notinlist to false and go out of the loop
                        begin
                          notinlist:=false;
                          break;
                        end;
                     {$else}
                     //check if it is in the list
                     for i:=0 to length(coderecords)-1 do
                       if coderecords[i]=addressfound then
                       begin
                         notinlist:=false;
                         break;
                       end;
                    {$endif}
                    except

                    end;


                    if notinlist then synchronize(foundone); //add this memory address to the foundcode window.

                  end;

                  //set the protection back to the original, in single stepping mode
                  VirtualProtectEx(processhandle,pointer(findreader.Address),findreader.size,findreader.originalprotection,original);

                  //set the process to single stepping
                  SetSingleStepping(devent.dwThreadId);
                  debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);

                  if WaitForDebugEvent(devent,10000)=false then application.MessageBox('error1-3','error1-3',0);
                  while devent.Exception.ExceptionRecord.ExceptionCode<>EXCEPTION_SINGLE_STEP do
                  begin
                    if devent.dwDebugEventCode=EXCEPTION_DEBUG_EVENT then
                    begin
                      //I hate it when this happens
                      debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_EXCEPTION_NOT_HANDLED);
                      outputdebugstring('err4');

                      break;
                    end;

                    debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);
                    if WaitForDebugEvent(devent,30000)=false then application.MessageBox('error2','error2',0)
                  end;


                  //set the protection back to read-only if the readonly flag is still needed
                  WaitForSingleObject(semaphore,infinite);
                  if findreaderset then VirtualProtectEx(processhandle,pointer(findreader.Address),findreader.size,original,original);
                  releasesemaphore(semaphore,1,nil);

                  if devent.Exception.ExceptionRecord.ExceptionCode<>EXCEPTION_SINGLE_STEP then continue;

                  debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);
                  continue;
                end; {read access violation}

                //check if it was a write access violation
                if (devent.Exception.ExceptionRecord.ExceptionCode=EXCEPTION_ACCESS_VIOLATION) and
                   (devent.Exception.ExceptionRecord.ExceptionInformation[0]=1) then //1=write
                begin
                  VirtualProtectEx(processhandle,pointer(findreader.Address),findreader.size,findreader.originalprotection,original);

                  //set the process to single stepping
                  SetSingleStepping(devent.dwThreadId);
                  debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);

                  if WaitForDebugEvent(devent,10000)=false then application.MessageBox('error1-1','error1-1',0);
                  while devent.Exception.ExceptionRecord.ExceptionCode<>EXCEPTION_SINGLE_STEP do
                  begin
                    if devent.dwDebugEventCode=EXCEPTION_DEBUG_EVENT then
                    begin
                      debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_EXCEPTION_NOT_HANDLED);
                      outputdebugstring('err5');
                      continue;
                    end;

                    debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);
                    if WaitForDebugEvent(devent,30000)=false then application.MessageBox('error2','error2',0)
                  end;

                  //set the protection back to no access if the readonly flag is still needed
                  WaitForSingleObject(semaphore,infinite);
                  if findreaderset then VirtualProtectEx(processhandle,pointer(findreader.Address),findreader.size,original,original);
                  releasesemaphore(semaphore,1,nil);

                  if devent.Exception.ExceptionRecord.ExceptionCode<>EXCEPTION_SINGLE_STEP then continue;

                end;

                debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);
                continue;

              end {if findreaderset}
              else
              begin
                if (devent.Exception.ExceptionRecord.ExceptionCode=EXCEPTION_ACCESS_VIOLATION) then
                begin
                  if findreader.Address<>0 then
                  begin
                    //check if the exception is at the same spot as the entry in the read-only list
                    if ((devent.Exception.ExceptionRecord.ExceptionInformation[1]>=(findreader.Address div $1000) * $1000) and
                       (devent.Exception.ExceptionRecord.ExceptionInformation[1]<((findreader.Address+$1000 div $1000) * $1000)+findreader.size)) then
                    begin
                      //perhaps a leftover or not, lets handle it anyhow
                      VirtualProtectEx(processhandle,pointer(findreader.Address),findreader.size,findreader.originalprotection,original);
                      debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);
                      continue;
                    end;
                  end;
                end;

              end;


              //not captured

              if findreaderremoved or readonlyremoved then
              begin
                findreaderremoved:=false;
                readonlyremoved:=false;
                debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);
              end
              else
              begin
                debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_EXCEPTION_NOT_HANDLED);
                outputdebugstring(pchar('err6:'+IntToStr(currentprocess)+ '/'+IntToStr(length(newprocesses)-1)+ ' eip='+IntToHex(context.eip,8)+' esi='+IntToHex(context.esi,8)+' edi='+IntToHex(context.edi,8) ));
              end;

              continue;
            end;

          CREATE_THREAD_DEBUG_EVENT:
          begin

            //seems this is a way of unhooking the debugger
            processhandle:=newprocesses[currentprocess].processhandle;

            setlength(threadlist,length(threadlist)+1);
            threadlist[length(threadlist)-1,0]:=devent.dwThreadId;
            threadlist[length(threadlist)-1,1]:=devent.CreateThread.hThread;

            threadlist[length(threadlist)-1,2]:=dword(devent.CreateThread.lpStartAddress);
            threadlist[length(threadlist)-1,3]:=dword(devent.CreateThread.lpThreadLocalBase);


            if canusedebugregs and (findwriter2 or breakpointset or userisdebugging) then  //set the debug registers here too
              setthreadcontext(devent.CreateThread.hThread,drregs);

            {$ifndef net}
            if frmthreadlist<>nil then synchronize(frmthreadlist.fillthreadlist);
            {$endif}

            //let the application/OS handle this debug event
            debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_EXCEPTION_NOT_HANDLED);
          end;

          EXIT_THREAD_DEBUG_EVENT:
          begin
            //search the threadid in the threadid list
            tid:=devent.dwThreadId;
            for i:=0 to length(threadlist)-1 do
              if threadlist[i,0]=tid then
              begin
                for j:=i to length(threadlist)-2 do threadlist[j]:=threadlist[j+1];
                setlength(threadlist,length(threadlist)-1);
                break;
              end;

            debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_EXCEPTION_NOT_HANDLED);
          end;

          CREATE_PROCESS_DEBUG_EVENT:
          begin
            currentprocess:=length(newprocesses);
            setlength(newprocesses,length(newprocesses)+1);
            newprocesses[currentprocess].processid:=devent.dwProcessId;
            newprocesses[currentprocess].processhandle:=devent.CreateProcessInfo.hProcess;
            newprocesses[currentprocess].filehandle:=devent.CreateProcessInfo.hFile;
            newprocesses[currentprocess].EntryPoint:=dword(devent.CreateProcessInfo.lpStartAddress);
            newprocesses[currentprocess].FirstBreak:=true;

            if attached then synchronize(ProcessCreated);

            readprocessmemory(newprocesses[currentprocess].processhandle,pointer(newprocesses[currentprocess].EntryPoint),@newprocesses[currentprocess].OriginalEntryByte,1,temp);

            newprocesses[currentprocess].dotnet:=false;
            if newprocesses[currentprocess].OriginalEntryByte=$ff then
            begin
              readprocessmemory(newprocesses[currentprocess].processhandle,pointer(newprocesses[currentprocess].EntryPoint),@ab,1,temp);
              if ab=$25 then newprocesses[currentprocess].dotnet:=true;
            end;

            if not newprocesses[currentprocess].dotnet then if not writeprocessmemory(newprocesses[currentprocess].processhandle,pointeR(newprocesses[currentprocess].EntryPoint),@int3,1,temp) then outputdebugstring('failed to set break');




            setlength(threadlist,length(threadlist)+1);
            threadlist[length(threadlist)-1,0]:=devent.dwThreadId;
            threadlist[length(threadlist)-1,1]:=devent.CreateProcessInfo.hThread;

            setlength(Processes,length(processes)+1);
            processes[length(processes)-1].ProcessID:=devent.dwProcessId;
            processes[length(processes)-1].running:=true;

            if processhandle=0 then
            begin
              processhandle:=devent.CreateProcessInfo.hProcess;
              processid:=devent.dwProcessId;
              processinfo.hProcess:=processhandle;
              processinfo.hThread:=devent.CreateProcessInfo.hThread;
              if devent.CreateProcessInfo.hProcess<>processhandle then BEEP;
            end;
            debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);
            {$ifndef net}
            memorybrowser.thhandle:=devent.CreateProcessInfo.hThread;
            {$endif}

            symhandler.reinitialize;
          end;


          EXIT_PROCESS_DEBUG_EVENT:
          begin
            //a process stopped
            //find the process in the process list
            for i:=0 to length(processes)-1 do
            begin
              if devent.dwProcessId=Processes[i].ProcessID then
              begin
                processes[i].running:=false;
                break;
              end;
            end;

            for i:=0 to length(newprocesses)-1 do
              if devent.dwProcessId=newprocesses[i].processid then
              begin
                for j:=i to length(newprocesses)-2 do
                  newprocesses[i]:=newprocesses[i+1];
                break;
              end;

            debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_EXCEPTION_NOT_HANDLED);

            //check if there are processes left
            debugging:=false;
            for i:=0 to length(processes)-1 do
              if processes[i].running then debugging:=true;
          end;

          OUTPUT_DEBUG_STRING_EVENT:
          begin
            if devent.DebugString.fUnicode=0 then
            begin
              //8 bit ascii
              try
                getmem(asciistring,devent.DebugString.nDebugStringLength+1);
                try
                  ReadProcessMemory(newprocesses[currentprocess].processhandle,devent.DebugString.lpDebugStringData,asciistring,devent.DebugString.nDebugStringLength,bytesread);
                  asciistring[devent.DebugString.nDebugStringLength]:=#0;
                  temps:=asciistring;
                finally
                  freemem(asciistring);
                end;
              except
                ;
              end;
            end
            else
            begin
              //16 bit unicode
              try
                getmem(unicodestring,devent.DebugString.nDebugStringLength+1);
                try
                  ReadProcessMemory(newprocesses[currentprocess].processhandle,devent.DebugString.lpDebugStringData,unicodestring,devent.DebugString.nDebugStringLength,bytesread);
                  unicodestring[devent.DebugString.nDebugStringLength div 2]:=#0;
                  temps:=unicodestring;
                finally
                  freemem(unicodestring);
                end;
              except

              end;
            end;
            synchronize(AddDebugString);

            if hidedebugger then //double bevcause of the way it is implementedin the os
              debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_EXCEPTION_NOT_HANDLED)
            else
              debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE)

          end;

          else debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_EXCEPTION_NOT_HANDLED);
        end;


    end;

  end;

  except
    messagebox(0,'Debugger crash','CE Debugger',mb_ok);
  end;

//  application.MessageBox(pchar('devent.dwDebugEventCode='+inttohex(devent.dwDebugEventCode,8)),'error');
  setlength(userbreakpoints,0);
  setlength(int3userbreakpoints,0);


  for i:=0 to length(processes)-1 do
    if processes[i].running then DebugActiveProcessStop(processes[i].ProcessID); //this sometimes works

  attached:=false;
  setlength(threadlist,0);
  {$ifndef net}
  postmessage(mainform.Handle,WM_USER+1,0,0);
  {$endif}
end;

initialization
  CRDebugging:=TCriticalSection.Create;

  krn := LoadLibrary('Kernel32.dll');
  if krn <> 0 then
  begin

    @DebugBreakProcess := GetProcAddress(krn, 'DebugBreakProcess');
    @DebugSetProcessKillOnExit :=GetProcAddress(krn,'DebugSetProcessKillOnExit');
    @DebugActiveProcessStop :=GetProcAddress(krn,'DebugActiveProcessStop');
    @IsDebuggerPresent:=GetProcAddress(krn,'IsDebuggerPresent');
    IsDebuggerPresentLocation:=DworD(GetProcAddress(krn,'IsDebuggerPresent'));

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
    DbgBreakPointLocation:=dword(getprocaddress(ntdlllib,'DbgBreakPoint'));

    NtQueryInformationProcess:=nil;
    NtQueryInformationProcess:=GetProcAddress(ntdlllib,'NtQueryInformationProcess');

    ntsuspendprocess:=nil;
    ntsuspendprocess:=GetProcAddress(ntdlllib,'NtSuspendProcess');
    ntresumeprocess:=GetProcAddress(ntdlllib,'NtResumeProcess');
    freelibrary(ntdlllib);
  end;


finalization
  FreeLibrary(krn);
  CRDebugging.Free;

end.
