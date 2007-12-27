unit DBK32functions;

interface

uses windows,sysutils,winsvc,psapi,classes,types,registry;

//xp sp2
//ThreadsProcess=220
//ThreadListEntry=22c



const currentversion=2000011;

const FILE_ANY_ACCESS=0;
const FILE_SPECIAL_ACCESS=FILE_ANY_ACCESS;
const FILE_READ_ACCESS=$0001;
const FILE_WRITE_ACCESS=$0002;
const FILE_RW_ACCESS=FILE_READ_ACCESS or FILE_WRITE_ACCESS;

const METHOD_BUFFERED=    0;
const METHOD_IN_DIRECT=   1;
const METHOD_OUT_DIRECT=  2;
const METHOD_NEITHER=     3;
const FILE_DEVICE_UNKNOWN=$00000022;
const IOCTL_UNKNOWN_BASE=FILE_DEVICE_UNKNOWN;


const IOCTL_CE_READMEMORY             = (IOCTL_UNKNOWN_BASE shl 16) or ($0800 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_WRITEMEMORY            = (IOCTL_UNKNOWN_BASE shl 16) or ($0801 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_OPENPROCESS    		  	=	(IOCTL_UNKNOWN_BASE shl 16) or ($0802 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_QUERY_VIRTUAL_MEMORY  	=	(IOCTL_UNKNOWN_BASE shl 16) or ($0803 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_TEST							      = (IOCTL_UNKNOWN_BASE shl 16) or ($0804 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_GETPEPROCESS				    = (IOCTL_UNKNOWN_BASE shl 16) or ($0805 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_READPHYSICALMEMORY		  = (IOCTL_UNKNOWN_BASE shl 16) or ($0806 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_WRITEPHYSICALMEMORY	  = (IOCTL_UNKNOWN_BASE shl 16) or ($0807 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_GETPHYSICALADDRESS		  = (IOCTL_UNKNOWN_BASE shl 16) or ($0808 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_PROTECTME					    = (IOCTL_UNKNOWN_BASE shl 16) or ($0809 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_GETCR3 					      = (IOCTL_UNKNOWN_BASE shl 16) or ($080a shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_SETCR3 					      = (IOCTL_UNKNOWN_BASE shl 16) or ($080b shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_GETSDT 					      = (IOCTL_UNKNOWN_BASE shl 16) or ($080c shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_INITIALIZE     		    = (IOCTL_UNKNOWN_BASE shl 16) or ($080d shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_DONTPROTECTME			    = (IOCTL_UNKNOWN_BASE shl 16) or ($080e shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_GETIDT 					  	  = (IOCTL_UNKNOWN_BASE shl 16) or ($080f shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_HOOKINTS 					    = (IOCTL_UNKNOWN_BASE shl 16) or ($0810 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_DEBUGPROCESS 			    = (IOCTL_UNKNOWN_BASE shl 16) or ($0811 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_RETRIEVEDEBUGDATA		  = (IOCTL_UNKNOWN_BASE shl 16) or ($0812 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_STARTPROCESSWATCH  	  = (IOCTL_UNKNOWN_BASE shl 16) or ($0813 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_GETPROCESSEVENTS		    =	(IOCTL_UNKNOWN_BASE shl 16) or ($0814 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_GETTHREADEVENTS			  = (IOCTL_UNKNOWN_BASE shl 16) or ($0815 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_GETVERSION				      =	(IOCTL_UNKNOWN_BASE shl 16) or ($0816 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_GETCR4 					      = (IOCTL_UNKNOWN_BASE shl 16) or ($0817 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_OPENTHREAD	    	   	  = (IOCTL_UNKNOWN_BASE shl 16) or ($0818 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_MAKEWRITABLE			      =	(IOCTL_UNKNOWN_BASE shl 16) or ($0819 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_DEBUGPROCESS_CHANGEREG	=	(IOCTL_UNKNOWN_BASE shl 16) or ($081a shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_STOPDEBUGGING				  = (IOCTL_UNKNOWN_BASE shl 16) or ($081b shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const	IOCTL_CE_STOP_DEBUGPROCESS_CHANGEREG =	(IOCTL_UNKNOWN_BASE shl 16) or ($081c shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const	IOCTL_CE_USEALTERNATEMETHOD		  =	(IOCTL_UNKNOWN_BASE shl 16) or ($081d shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const	IOCTL_CE_ISUSINGALTERNATEMETHOD =	(IOCTL_UNKNOWN_BASE shl 16) or ($081e shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const	IOCTL_CE_ALLOCATEMEM				    =	(IOCTL_UNKNOWN_BASE shl 16) or ($081f shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_CREATEAPC					    =	(IOCTL_UNKNOWN_BASE shl 16) or ($0820 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_GETPETHREAD				    =	(IOCTL_UNKNOWN_BASE shl 16) or ($0821 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);

const IOCTL_CE_SUSPENDTHREAD			    =	(IOCTL_UNKNOWN_BASE shl 16) or ($0822 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_RESUMETHREAD				    =	(IOCTL_UNKNOWN_BASE shl 16) or ($0823 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_SUSPENDPROCESS			    =	(IOCTL_UNKNOWN_BASE shl 16) or ($0824 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_RESUMEPROCESS			    =	(IOCTL_UNKNOWN_BASE shl 16) or ($0825 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);

const IOCTL_CE_ALLOCATEMEM_NONPAGED   =	(IOCTL_UNKNOWN_BASE shl 16) or ($0826 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_GETPROCADDRESS			    =	(IOCTL_UNKNOWN_BASE shl 16) or ($0827 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_SETSDTADDRESS 			    =	(IOCTL_UNKNOWN_BASE shl 16) or ($0828 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_GETSDTADDRESS 			    =	(IOCTL_UNKNOWN_BASE shl 16) or ($0829 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);

const IOCTL_CE_GETGDT 					  	  = (IOCTL_UNKNOWN_BASE shl 16) or ($082a shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_SETCR4 					  	  = (IOCTL_UNKNOWN_BASE shl 16) or ($082b shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_VMXCONFIG				  	  = (IOCTL_UNKNOWN_BASE shl 16) or ($082d shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_GETCR0 					  	  = (IOCTL_UNKNOWN_BASE shl 16) or ($082e shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_MAKEKERNELCOPY		  	  = (IOCTL_UNKNOWN_BASE shl 16) or ($082f shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_SETGLOBALDEBUGSTATE 	  = (IOCTL_UNKNOWN_BASE shl 16) or ($0830 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);




type TDeviceIoControl=function(hDevice: THandle; dwIoControlCode: DWORD; lpInBuffer: Pointer; nInBufferSize: DWORD; lpOutBuffer: Pointer; nOutBufferSize: DWORD; var lpBytesReturned: DWORD; lpOverlapped: POverlapped): BOOL; stdcall;

type thandlelist=record
  processhandle: thandle;
  processid: dword;
  validhandle: boolean;
end;

type TClient_ID=record
  processid: thandle;
  threadid: thandle;
end;
type PClient_ID=^TClient_ID;

type THookIDTThread=class(tthread)
  public
    cpunr: byte;
    done: boolean;
    succeeded: boolean;
    procedure execute; override;
end;

type THookIDTConstantly=class(tthread)
  public
    procedure execute; override;
end;

var cpuidt: array of dword;
type TGetIDTThread=class(tthread)
  public
    cpunr: byte;
    done: boolean;
    procedure execute; override;
  end;

var hdevice: thandle; //handle to my the device driver
    handlelist: array of thandlelist;
    driverloc: string;
    iamprotected:boolean;
    SDTShadow: DWORD;
    debugport,processname: dword;

    ThreadsProcess,ThreadListEntry:dword;

    processeventname, threadeventname: string;
    processevent,threadevent:thandle;

    ownprocess: thandle=0; //needed for simple kernelmemory access
    Successfullyloaded:boolean;

    usealternatedebugmethod: boolean;



function CTL_CODE(DeviceType, Func, Method, Access : integer) : integer;
function IsValidHandle(hProcess:THandle):BOOL; stdcall;
Function {OpenProcess}OP(dwDesiredAccess:DWORD;bInheritHandle:BOOL;dwProcessId:DWORD):THANDLE; stdcall;
Function {OpenThread}OT(dwDesiredAccess:DWORD;bInheritHandle:BOOL;dwThreadId:DWORD):THANDLE; stdcall;
function {ReadProcessMemory}RPM(hProcess:THANDLE;lpBaseAddress:pointer;lpBuffer:pointer;nSize:DWORD;var NumberOfBytesRead:DWORD):BOOL; stdcall;
function {WriteProcessMemory}WPM(hProcess:THANDLE;lpBaseAddress:pointer;lpBuffer:pointer;nSize:DWORD;var NumberOfBytesWritten:DWORD):BOOL; stdcall;
function {VirtualQueryEx}VQE(hProcess: THandle; address: pointer; var mbi: _MEMORY_BASIC_INFORMATION; bufsize: DWORD):dword; stdcall;
Function {NtOpenProcess}NOP(var Handle: THandle; AccessMask: dword; objectattributes: pointer; clientid: PClient_ID):DWORD; stdcall;
Function {NtOpenThread}NtOT(var Handle: THandle; AccessMask: dword; objectattributes: pointer; clientid: PClient_ID):DWORD; stdcall;
Function {VirtualAllocEx}VAE(hProcess: THandle; lpAddress: Pointer; dwSize, flAllocationType: DWORD; flProtect: DWORD): Pointer; stdcall;
Function CreateRemoteAPC(threadid: dword; lpStartAddress: TFNAPCProc): THandle; stdcall;


Function GetPEProcess(ProcessID: dword):dword; stdcall;
Function GetPEThread(Threadid: dword):dword; stdcall;
function GetDebugportOffset: DWORD; stdcall;
function GetProcessnameOffset: dword; stdcall;
function GetThreadsProcessOffset: dword; stdcall;
function GetThreadListEntryOffset: dword; stdcall;

function ReadPhysicalMemory(hProcess:THANDLE;lpBaseAddress:pointer;lpBuffer:pointer;nSize:DWORD;var NumberOfBytesRead:DWORD):BOOL; stdcall;
function WritePhysicalMemory(hProcess:THANDLE;lpBaseAddress:pointer;lpBuffer:pointer;nSize:DWORD;var NumberOfBytesWritten:DWORD):BOOL; stdcall;
function GetPhysicalAddress(hProcess:THandle;lpBaseAddress:pointer;var Address:int64): BOOL; stdcall;

function ProtectMe(ProtectedProcessID: dword; denylist,globaldenylist:BOOL;list:pchar; listsize:dword):BOOL; stdcall; //or should I give it a array of processid's?
function UnprotectMe:bool; stdcall;
function MakeKernelCopy(Base: dword; size: dword): bool; stdcall;

function GetCR4:DWORD; stdcall;
function GetCR3(hProcess:THANDLE;var CR3:DWORD):BOOL; stdcall;
function SetCR3(hProcess:THANDLE;CR3: DWORD):BOOL; stdcall;
function GetCR0:DWORD; stdcall;
function GetSDT:DWORD; stdcall;
function GetSDTShadow:DWORD; stdcall;
function setAlternateDebugMethod(var int1apihook:dword; var OriginalInt1handler:dword):BOOL; stdcall;
function getAlternateDebugMethod:BOOL; stdcall;
function DebugProcess(processid:dword;address:DWORD;size: byte;debugtype:byte):BOOL; stdcall;
function SetGlobalDebugState(state: boolean): BOOL; stdcall;
function StopDebugging:BOOL; stdcall;
function StopRegisterChange(regnr:integer):BOOL; stdcall;
function RetrieveDebugData(Buffer: pointer):integer; stdcall;
function ChangeRegOnBP(Processid:dword; address: dword; debugreg: integer; changeEAX,changeEBX,changeECX,changeEDX,changeESI,changeEDI,changeEBP,changeESP,changeEIP,changeCF,changePF,changeAF,changeZF,changeSF,changeOF:BOOLEAN; newEAX,newEBX,newECX,newEDX,newESI,newEDI,newEBP,newESP,newEIP:DWORD; newCF,newPF,newAF,newZF,newSF,newOF:BOOLEAN):BOOLEAN; stdcall;
function StartProcessWatch:BOOL;stdcall;
function WaitForProcessListData(processpointer:pointer;threadpointer:pointer;timeout:dword):dword; stdcall;
function GetProcessNameFromPEProcess(peprocess:dword; buffer:pchar;buffersize:dword):integer; stdcall;
function GetProcessNameFromID(processid:dword; buffer:pointer;buffersize:dword):integer; stdcall;
function MakeWritable(Address,Size:dword;copyonwrite:boolean): boolean; stdcall;
function RewriteKernel32:boolean; stdcall;
function RestoreKernel32:boolean; stdcall;

function InitializeDriver(Address,size:dword):BOOL; stdcall;
function GetWin32KAddress(var address:DWORD;var size:dworD):boolean;
function GetDriverVersion: dword;

function GetIDTCurrentThread:dword; stdcall;
function GetIDTs(idtstore: pointer; maxidts: integer):integer; stdcall;

function GetLoadedState: BOOLEAN; stdcall;

function test: boolean; stdcall;
procedure useIOCTL(use: boolean); stdcall;

function DBKSuspendThread(ThreadID:dword):boolean; stdcall;
function DBKResumeThread(ThreadID:dword):boolean; stdcall;
function DBKSuspendProcess(ProcessID:dword):boolean; stdcall;
function DBKResumeProcess(ProcessID:dword):boolean; stdcall;

function KernelAlloc(size: dword):pointer; stdcall;
function GetKProcAddress(s: pwidechar):pointer; stdcall;

function GetSDTEntry(nr: integer; address: PDWORD; paramcount: PBYTE):boolean; stdcall;
function SetSDTEntry(nr: integer; address: DWORD; paramcount: BYTE):boolean; stdcall;
function GetSSDTEntry(nr: integer; address: PDWORD; paramcount: PBYTE):boolean; stdcall;
function SetSSDTEntry(nr: integer; address: DWORD; paramcount: BYTE):boolean; stdcall;

function GetGDT(limit: pword):dword; stdcall;

var hooker: THookIDTConstantly;
    kernel32dll: thandle;
    ioctl: boolean;

implementation

uses vmxfunctions;



procedure FSC;
asm
  mov edx,esp
  sysenter
end;

function GetLoadedState: BOOLEAN; stdcall;
begin
  result:=(hdevice<>INVALID_HANDLE_VALUE) and Successfullyloaded;
end;

procedure useIOCTL(use: boolean); stdcall;
begin
  ioctl:=use;
end;

function Test:boolean; stdcall;
{$W+}
procedure test_noioctl(p1:integer; p2: integer); stdcall;
asm
  pop ebp //used parameters so the stupid compiler added a push ebp
  mov eax,$2000
  call fsc
  ret 8
end;
{$W-}

var cc,br: dword;
    threadid: dword;
begin
  result:=true;
  if ioctl then
  begin
    cc:=IOCTL_CE_TEST;
    result:=deviceiocontrol(hdevice,cc,@threadid,sizeof(threadid),nil,0,br,nil);
  end
  else
  begin
    test_noioctl(1,2);
  end;
end;


function GetGDT(limit: pword):dword; stdcall;
var cc,br: dword;
    gdtdescriptor: packed record
                     wLimit:word;
                     vector: dword;
                   end;
begin
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_GETGDT;
    deviceiocontrol(hdevice,cc,nil,0,@gdtdescriptor,6,br,nil);
    result:=gdtdescriptor.vector;
    if (limit<>nil) then
      limit^:=gdtdescriptor.wlimit;
  end else result:=0;
end;

function GetIDTCurrentThread:dword;
var cc,br: dword;
    idtdescriptor: packed record
                     wLimit:word;
                     vector: dword;
                   end;
begin
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_GETIDT;
    deviceiocontrol(hdevice,cc,nil,0,@idtdescriptor,6,br,nil);
    result:=idtdescriptor.vector;
  end else result:=0;
end;

procedure TGetIDTThread.execute;
begin
  try
    cpuidt[cpunr]:=getidtcurrentthread;
  finally
    done:=true;
  end;
end;

function GetIDTs(idtstore: pointer; maxidts: integer):integer; stdcall;
var ec: dword;
    i:integer;
    cpunr,PA,SA:Dword;
    cpunr2:byte;
begin
  //max idt's should be 32, but may be less if you('re a retard!) don't want to allocate the enormous ammount of 32*4=128 bytes
  setlength(cpuidt,0);
  result:=0; //0 idt's returned

  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    GetProcessAffinityMask(getcurrentprocess,PA,SA);

    //first hook the interrupts if needed
    cpunr2:=0;
    cpunr:=1;
    while (cpunr<=PA) do
    begin
      if ((cpunr) and PA)>0 then
      begin
        setlength(cpuidt,length(cpuidt)+1);
        SetProcessAffinityMask(getcurrentprocess,cpunr);
        //create a new thread. (Gues on what cpu it will run at...)

        with TGetIDTThread.Create(true) do
        begin
          try
            cpunr:=cpunr2;
            resume;

            while not done do sleep(20); //the sleep should also cause a taskswitch but I'm not 100% sure
          finally
            free;
          end;
        end;

      end;
      if cpunr=$80000000 then break;
      inc(cpunr,cpunr);
      inc(cpunr2);//next cpu
    end;

    SetProcessAffinityMask(getcurrentprocess,PA); //multi processors are so fun. It'd be a waste not to use it

    if length(cpuidt)>maxidts then
      setlength(cpuidt,maxidts);
      
    for i:=0 to length(cpuidt)-1 do
      TCardinalDynArray(idtstore)[i]:=cpuidt[i];

    result:=length(cpuidt);
  end;

end;


procedure THookIDTThread.execute;
var cc,br: dword;
begin
  try
//    outputdebugstring('hooking IDT');
    cc:=IOCTL_CE_HOOKINTS;
    succeeded:=deviceiocontrol(hdevice,cc,@cpunr,1,@cpunr,0,br,nil);
  finally
    done:=true;
  end;
end;

procedure THookIDTConstantly.execute;
var input:TInput;
    br,cc: dword;
    i:integer;
    cpunr,PA,SA:Dword;
    cpunr2:byte;
begin
  freeonterminate:=true;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_HOOKINTS;

    while not terminated do
    begin
//      outputdebugstring('writing the idt');
      GetProcessAffinityMask(getcurrentprocess,PA,SA);

      cpunr2:=0;
      cpunr:=1;
      while (cpunr<=PA) do
      begin
        if ((cpunr) and PA)>0 then
        begin
          SetProcessAffinityMask(getcurrentprocess,cpunr);
          //create a new thread. (Gues on what cpu it will run at...)

          with THookIDTThread.Create(true) do
          begin
            try
              cpunr:=cpunr2;
              resume;

              while not done do sleep(10); //the sleep should also cause a taskswitch but I'm not 100% sure
            finally
              free;
            end;
          end;

        end;
        if cpunr=$80000000 then break;
        inc(cpunr,cpunr);
        inc(cpunr2);
      end;

      SetProcessAffinityMask(getcurrentprocess,PA); //multi processors are so fun. It'd be a waste not to use it

      if vmx_enabled then exit; //no rehook needed since idt changes don't matter
      
      sleep(5000); //wait a while before rewriting
    end;
  end;
end;


function GetProcessNameFromPEProcess(peprocess:dword; buffer:pchar;buffersize:dword):integer; stdcall;
var ar:dword;
    i:integer;
begin
  if buffersize>16 then buffersize:=16;

  result:=-1;
  if processname=0 then exit;
  if (hdevice<>INVALID_HANDLE_VALUE) and (ownprocess<>0) then
  begin
    if rpm(ownprocess,pointer(peprocess+processname),buffer,buffersize,ar) then
    begin
      for i:=0 to buffersize-1 do
        if buffer[i]=#0 then
        begin
          result:=i+i;
          exit;
        end;
    end;
  end;

end;

function GetCR0:DWORD; stdcall;
var x,res,cc:dword;
begin
  result:=0;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_GETCR0;
    if deviceiocontrol(hdevice,cc,@res,4,@res,4,x,nil) then
      result:=res;
  end;
end;

function GetCR4:DWORD; stdcall;
var x,res,cc:dword;
begin
  result:=0;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_GETCR4;
    if deviceiocontrol(hdevice,cc,@res,4,@res,4,x,nil) then
      result:=res;
  end;
end;


function GetDriverVersion:dword;
var x,res,cc,cc2,cc3:dword;
begin
  result:=0;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_GETVERSION;
    if cc<>cc2 then
      if cc<>cc3 then


    if deviceiocontrol(hdevice,cc,@res,4,@res,4,x,nil) then
      result:=res;

  end;
end;

function GetProcessNameFromID(processid:dword; buffer:pointer;buffersize:dword):integer; stdcall;
begin
  //just a simple stub
  result:=GetProcessNameFromPEProcess(GetPEProcess(processid),buffer,buffersize);
end;

function GetThreadsProcessOffset: dword; stdcall;
begin
  result:=ThreadsProcess;
end;

function GetThreadListEntryOffset: dword; stdcall;
begin
  result:=ThreadListEntry;
end;


function GetProcessnameOffset: dword; stdcall;
begin
  result:=processname;
end;

function GetDebugportOffset: DWORD; stdcall;
begin
  result:=debugport;
end;

function GetSDTShadow:DWORD; stdcall;
begin
  result:=SDTShadow;
end;

function GetSDT:DWORD; stdcall;
var res,x,cc:dword;
begin
  result:=0;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_GETSDT;
    if deviceiocontrol(hdevice,cc,@res,4,@res,4,x,nil) then
      result:=res;
  end;
end;

function GetCR3(hProcess:THANDLE;var CR3:DWORD):BOOL; stdcall;
var cc:dword;
    x,y:dword;
    i: integer;
begin
  result:=false;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    for i:=0 to length(handlelist)-1 do
      if handlelist[i].processhandle=hProcess then
      begin
        cc:=IOCTL_CE_GETCR3;
        x:=handlelist[i].processid;
        result:=deviceiocontrol(hdevice,cc,@x,4,@x,4,y,nil);

        if result then CR3:=x else cr3:=$11223344;
      end;
  end;
end;



function SetCR3(hProcess:THANDLE;CR3: DWORD):BOOL; stdcall;
var cc:dword;
    ar: array [0..7] of byte;
    x:dword;
    i: integer;
begin
  result:=false;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    for i:=0 to length(handlelist)-1 do
      if handlelist[i].processhandle=hProcess then
      begin
        cc:=IOCTL_CE_SETCR3;
        pdword(@ar[0])^:=handlelist[i].processid;
        pdword(@ar[4])^:=CR3;

        result:=deviceiocontrol(hdevice,cc,@ar[0],4,@ar[0],4,x,nil);
      end;
  end;
end;


function ProtectMe(ProtectedProcessID: dword; denylist,globaldenylist:BOOL;list:pchar; listsize:dword):BOOL; stdcall; //or should I give it a array of processid's?
type tinput=record
  processid: dword;
  DenyList: DWORD;
  GlobalDenyList: DWORD;
  ListSize:DWORD;
end;
var cc,x: dword;
    input: ^tinput;
    win32kaddress,win32size:dword;
begin
  OutputDebugString('Protectme called');
  result:=false;

  if GetWin32KAddress(win32kAddress,win32size) then
    if not InitializeDriver(win32kAddress,win32size) then exit;

  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    getmem(input,sizeof(tinput)+listsize);
    try
      input.processid:=ProtectedProcessID;
      if denylist then
        input.DenyList:=1 else input.DenyList:=0;

      if globaldenylist then
        input.GlobalDenyList:=1 else input.GlobalDenyList:=0;
        
      input.ListSize:=listsize;
      copymemory(pointer(dword(@input.listsize)+4),list,listsize);

      cc:=IOCTL_CE_PROTECTME;
      result:=deviceiocontrol(hdevice,cc,input,sizeof(tinput)+listsize,input,4,x,nil);
    finally
      freemem(input);
    end;
    Iamprotected:=result;
  end;
end;

function UnprotectMe:bool; stdcall;
var cc,x: dword;
begin
  result:=false;
  if (hdevice<>INVALID_HANDLE_VALUE) and (Iamprotected) then
  begin
    cc:=IOCTL_CE_DONTPROTECTME;
    result:=deviceiocontrol(hdevice,cc,nil,0,nil,0,x,nil);
  end;
end;

function MakeKernelCopy(Base: dword; size: dword): bool; stdcall;
var cc: dword;
    inp: record
      base: dword;
      size: dword;
    end;
    x: dword;
begin

  result:=false;
  if (hdevice<>INVALID_HANDLE_VALUE)  then
  begin
    cc:=IOCTL_CE_MAKEKERNELCOPY;
    inp.base:=base;
    inp.size:=size;
    result:=deviceiocontrol(hdevice,cc,@inp,sizeof(inp),nil,0,x,nil);
  end;
end;

function DBKSuspendThread(ThreadID:dword):boolean; stdcall;
var cc,x: dword;
begin
  outputdebugstring('DBKSuspendThread');
  result:=false;
  x:=ThreadId;
  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_SUSPENDTHREAD;
    result:=deviceiocontrol(hdevice,cc,@x,sizeof(x),nil,0,x,nil);
  end;

end;

function DBKResumeThread(ThreadID:dword):boolean; stdcall;
var cc,x: dword;
begin
  outputdebugstring('DBKResumeThread');
  result:=false;
  x:=threadid;
  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_RESUMETHREAD;
    result:=deviceiocontrol(hdevice,cc,@x,sizeof(x),nil,0,x,nil);
  end;

end;


function DBKSuspendProcess(ProcessID:dword):boolean; stdcall;
var cc,x: dword;
begin
  result:=false;
  x:=ProcessID;
  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_SUSPENDPROCESS;
    result:=deviceiocontrol(hdevice,cc,@x,sizeof(x),nil,0,x,nil);
  end;

end;

function DBKResumeProcess(ProcessID:dword):boolean; stdcall;
var cc,x: dword;
begin
  result:=false;
  x:=ProcessID;
  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_RESUMEPROCESS;
    result:=deviceiocontrol(hdevice,cc,@x,sizeof(x),nil,0,x,nil);
  end;

end;

function GetPhysicalAddress(hProcess:THandle;lpBaseAddress:pointer;var Address:int64): BOOL; stdcall;
type TInputstruct=record
  ProcessID: dword;
  BaseAddress: dword;
end;
var cc: dword;
    input: TInputStruct;
    physicaladdress: int64 absolute input;
    x: dword;
    i: integer;
begin
  result:=false;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_GETPHYSICALADDRESS;

    for i:=0 to length(handlelist)-1 do
      if handlelist[i].processhandle=hProcess then
      begin
        input.ProcessID:=handlelist[i].processid;
        input.BaseAddress:=dword(lpBaseAddresS);

        result:=deviceiocontrol(hdevice,cc,@input,8,@input,8,x,nil);
        if result then address:=physicaladdress else address:=0;
      end;
  end;
end;

function WritePhysicalMemory(hProcess:THANDLE;lpBaseAddress:pointer;lpBuffer:pointer;nSize:DWORD;var NumberOfBytesWritten:DWORD):BOOL; stdcall;
type TInputstruct=record
  startaddress: dword;
  bytestowrite: dword;
end;
var ao: array [0..511] of byte;
    input: TInputstruct absolute ao[0];
    cc:dword;

    i: integer;
    ok: boolean;
    br: dword;

    mempointer: dword;
    bufpointer: dword;
    bufpointer2: pointer;
    towrite: dword;
begin
  result:=false;
  NumberOfByteswritten:=0;
  //find the hprocess in the handlelist, if it isn't use the normal method (I could of course use NtQueryProcessInformation but it's undocumented and I'm too lazy to dig it up

  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_WRITEPHYSICALMEMORY;
    mempointer:=dword(lpBaseAddress);
    bufpointer:=dword(lpbuffer);

    ok:=true;
    while ok do
    begin
      zeromemory(@ao[0],512);

      if nSize-NumberOfByteswritten>=(512-sizeof(TInputstruct)) then
        towrite:=(512-sizeof(TInputstruct))
      else
        towrite:=nSize-NumberOfByteswritten;

      input.bytestowrite:=towrite;
      input.startaddress:=mempointer;

      bufpointer2:=pointer(bufpointer);
      copymemory(@ao[sizeof(tinputstruct)],bufpointer2,towrite);

      if not deviceiocontrol(hdevice,cc,@ao[0],512,@ao[0],512,br,nil) then exit;

      inc(mempointer,towrite);
      inc(bufpointer,towrite);
      inc(NumberOfByteswritten,towrite);

      if NumberOfByteswritten=nSize then
      begin
        result:=true;
        exit;
      end;
    end;

  end;
end;


function ReadPhysicalMemory(hProcess:THANDLE;lpBaseAddress:pointer;lpBuffer:pointer;nSize:DWORD;var NumberOfBytesRead:DWORD):BOOL; stdcall;
type TInputstruct=record
  startaddress: dword;
  bytestoread: dword
end;
var ao: array [0..600] of byte;
    input: TInputstruct absolute ao[0];
    br: dword;
    cc:dword;
    ok:boolean;
    toread:dword;
    toread2: dword;
    mempointeR:dword;
    bufpointer:dword;
begin
  //processhandle is just there for compatibility in case I want to quickly wrap it over read/writeprocessmemory
  result:=false;
  numberofbytesread:=0;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_READPHYSICALMEMORY;
    toread:=nSize;
    mempointer:=dword(lpBaseAddress);
    bufpointer:=dword(lpBuffer);
    ok:=true;

    while ok and (toread>0) do
    begin
      if toread>512 then
        toread2:=512
      else
        toread2:=toread;

      dec(toread,toread2);

      input.bytestoread:=toread2;
      input.startaddress:=mempointer;

      if deviceiocontrol(hdevice,cc,@ao[0],512,@ao[0],512,br,nil) then
      begin
        copymemory(pointer(bufpointer),@ao[0],toread2);
        inc(numberofbytesread,toread2);
        inc(bufpointer,toread2);
        mempointer:=mempointer+toread2;
      end
      else
        ok:=false;
    end;

    result:=ok;
  end;
end;

Function GetPEThread(Threadid: dword):dword; stdcall;
var cc:dword;
    x: dword;
    pethread: dword;
begin
  result:=0;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_GETPETHREAD;
    pethread:=threadid;
    if deviceiocontrol(hdevice,cc,@pethread,4,@pethread,4,x,nil) then result:=pethread;
  end;
end;


Function GetPEProcess(ProcessID: dword):dword; stdcall;
var cc:dword;
    x: dword;
    peprocess: dword;
begin
  result:=0;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_GETPEPROCESS;
    peprocess:=processid;
    if deviceiocontrol(hdevice,cc,@peprocess,4,@peprocess,4,x,nil) then result:=peprocess else result:=0;
  end;
end;


function IsValidHandle(hProcess:THandle):BOOL; stdcall;
var i: integer;
begin
  result:=false;
  for i:=0 to length(handlelist)-1 do
    if handlelist[i].processhandle=hProcess then
    begin
      result:=handlelist[i].validhandle;
      exit;
    end;
end;

function {ReadProcessMemory}RPM(hProcess:THANDLE;lpBaseAddress:pointer;lpBuffer:pointer;nSize:DWORD;var NumberOfBytesRead:DWORD):BOOL; stdcall;
type TInputstruct=record
  processid: dword;
  startaddress: dword;
  bytestoread: word;
end;
var //ao: array [0..600] of byte; //give it some space
    input: TInputstruct;
    cc:dword;

    i: integer;
    ok: boolean;
    br: dword;

    mempointer: dword;
    bufpointer: dword;
    bufpointer2: pointer;
    toread: dword;
begin
  result:=false;
  numberofbytesread:=0;
  //find the hprocess in the handlelist, if it isn't use the normal method (I could of course use NtQueryProcessInformation but it's undocumented and I'm too lazy to dig it up

  for i:=0 to length(handlelist)-1 do
    if handlelist[i].processhandle=hProcess then
    begin
      if hdevice<>INVALID_HANDLE_VALUE then
      begin
        cc:=IOCTL_CE_READMEMORY;
        mempointer:=dword(lpBaseAddress);
        bufpointer:=dword(lpbuffer);

        ok:=true;
        while ok do
        begin
          input.processid:=handlelist[i].processid;
          if (mempointer and $fff) > 0 then //uneven
          begin
            toread:=4096-(mempointer and $fff);
            if toread>(nSize-numberofbytesread) then toread:=nSize-numberofbytesread;
          end
          else
          begin
            if nSize-numberofbytesread>=4096 then
              toread:=4096
            else
              toread:=nSize-numberofbytesread;
          end;

          input.bytestoread:=toread;
          input.startaddress:=mempointer;

          if not deviceiocontrol(hdevice,cc,@input,sizeof(input),pointer(bufpointer),toread,br,nil) then
            exit;

          inc(mempointer,toread);
          inc(bufpointer,toread);
          inc(numberofbytesread,toread);

          if numberofbytesread=nSize then
          begin
            result:=true;
            exit;
          end;
        end;

        exit;
      end else if not handlelist[i].validhandle then exit; //else use the normal method...
    end;

  //not found so ....
  result:=windows.ReadProcessMemory(hProcess,lpBaseAddress,lpBuffer,nSize,NumberOfBytesRead);
end;


function {WriteProcessMemory}WPM(hProcess:THANDLE;lpBaseAddress:pointer;lpBuffer:pointer;nSize:DWORD;var NumberOfBytesWritten:DWORD):BOOL; stdcall;
type TInputstruct=record
  processid: dword;
  startaddress: dword;
  bytestowrite: word;
end;
var ao: array [0..511] of byte;
    input: TInputstruct absolute ao[0];
    cc:dword;

    i: integer;
    ok: boolean;
    br: dword;

    mempointer: dword;
    bufpointer: dword;
    bufpointer2: pointer;
    towrite: dword;
begin
  result:=false;
  NumberOfByteswritten:=0;
  //find the hprocess in the handlelist, if it isn't use the normal method (I could of course use NtQueryProcessInformation but it's undocumented and I'm too lazy to dig it up

  for i:=0 to length(handlelist)-1 do
    if handlelist[i].processhandle=hProcess then
    begin
      if hdevice<>INVALID_HANDLE_VALUE then
      begin
        cc:=IOCTL_CE_WRITEMEMORY;
        mempointer:=dword(lpBaseAddress);
        bufpointer:=dword(lpbuffer);

        ok:=true;
        while ok do
        begin
          zeromemory(@ao[0],512);

          input.processid:=handlelist[i].processid;
          if nSize-NumberOfByteswritten>=(512-sizeof(TInputstruct)) then
            towrite:=(512-sizeof(TInputstruct))
          else
            towrite:=nSize-NumberOfByteswritten;

          input.bytestowrite:=towrite;
          input.startaddress:=mempointer;

          bufpointer2:=pointer(bufpointer);
          copymemory(@ao[sizeof(tinputstruct)],bufpointer2,towrite);

          if not deviceiocontrol(hdevice,cc,@ao[0],512,@ao[0],512,br,nil) then exit;

          inc(mempointer,towrite);
          inc(bufpointer,towrite);
          inc(NumberOfByteswritten,towrite);

          if NumberOfByteswritten=nSize then
          begin
            result:=true;
            exit;
          end;
        end;

        exit;
      end else if not handlelist[i].validhandle then exit;
    end;

  //not found so ....
  result:=windows.writeProcessMemory(hProcess,lpBaseAddress,lpBuffer,nSize,NumberOfByteswritten);
end;

function {OpenThread}OT(dwDesiredAccess:DWORD;bInheritHandle:BOOL;dwThreadId:DWORD):THANDLE; stdcall;
var
  threadhandle: thandle;
  cc,x: dword;
begin
  result:=0;
  if dwThreadId=0 then exit;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_OPENTHREAD;
    threadhandle:=dwThreadId;
    if deviceiocontrol(hdevice,cc,@threadhandle,4,@threadhandle,4,x,nil) then
      result:=threadhandle
    else
      result:=0;
  end;
end;


function {OpenProcess}OP(dwDesiredAccess:DWORD;bInheritHandle:BOOL;dwProcessId:DWORD):THANDLE; stdcall;
var valid:boolean;
    Processhandle: thandle;
    i:integer;
    cc,x: dword;
begin
  valid:=true;
  if dwProcessId=0 then
  begin
    result:=0;
    exit;
  end;

  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_OPENPROCESS;

    processhandle:=dwProcessId; //rest is ignored

    if deviceiocontrol(hdevice,cc,@processhandle,4,@processhandle,4,x,nil) then
    begin
      result:=processhandle
    end
    else
      result:=0;
  end else result:=windows.OpenProcess(dwDesiredAccess,bInheritHandle,dwProcessID);

  if result=0 then //you can still access memory using the low level stuff, just not normal stuff
  begin
    valid:=false;
    //openprocess isn't working
    if length(handlelist)=0 then result:=100+random(32000)
    else
      result:=handlelist[length(handlelist)-1].processhandle+1;
  end;

  //check for a duplicate handle and replace it (closehandle/openproces gets you the same handle)
  for i:=0 to length(handlelist)-1 do
  begin
    if handlelist[i].processhandle=result then
    begin
      handlelist[i].processid:=dwProcessID;
      handlelist[i].validhandle:=valid;
      exit;
    end;

  end;

  setlength(handlelist,length(handlelist)+1);
  handlelist[length(handlelist)-1].processhandle:=result;
  handlelist[length(handlelist)-1].processid:=dwProcessID;
  handlelist[length(handlelist)-1].validhandle:=valid;
end;

Function {NtOpenThread}NtOT(var Handle: THandle; AccessMask: dword; objectattributes: pointer; clientid: PClient_ID):DWORD; stdcall;
begin
  handle:=OP(STANDARD_RIGHTS_REQUIRED or windows.synchronize or $3ff,true,clientid.processid);
  if handle<>0 then result:=0 else result:=$c000000e;
end;

Function {NtOpenProcess}NOP(var Handle: THandle; AccessMask: dword; objectattributes: pointer; clientid: PClient_ID):DWORD; stdcall;
begin
  Handle:=OP(process_all_access,true,clientid.processid);
  if handle<>0 then result:=0 else result:=$C000000E;
end;

function {VirtualQueryEx}VQE(hProcess: THandle; address: pointer; var mbi: _MEMORY_BASIC_INFORMATION; bufsize: DWORD):dword; stdcall;
type TOUTP=record
	length : DWORD ;
	protection : DWORD ;
end;
var buf: TOUTP;
    i: integer;
    br,cc: dword;
begin
  result:=0;
  for i:=0 to length(handlelist)-1 do
    if handlelist[i].processhandle=hProcess then
    begin
      if hdevice<>INVALID_HANDLE_VALUE then
      begin
        buf.length:=handlelist[i].processid;
        buf.protection:=dword(address);

        cc:=IOCTL_CE_QUERY_VIRTUAL_MEMORY;
        if deviceiocontrol(hdevice,cc,@buf,sizeof(buf),@buf,sizeof(buf),br,nil) then
        begin
          mbi.BaseAddress:=pointer((dword(address) div $1000) *$1000);
          mbi.AllocationBase:=mbi.BaseAddress;
          mbi.AllocationProtect:=buf.protection;
          mbi.RegionSize:=buf.length;
          mbi.State:=MEM_COMMIT;
          mbi.Protect:=buf.protection;
          mbi.Type_9:=MEM_PRIVATE;

          result:=sizeof(mbi);
        end;
        
        exit; //we're done here
      end;
    end;



  result:=windows.VirtualQueryEx(hProcess,address,mbi,bufsize);
end;

Function {VirtualAllocEx}VAE(hProcess: THandle; lpAddress: Pointer; dwSize, flAllocationType: DWORD; flProtect: DWORD): Pointer; stdcall;
var i: integer;
    br,cc: dword;
    x: record
      processid: dword;
      baseaddress: pointer;
      size: dword;
      AllocationType: dword;
      Protect: dword;
    end;
    r: pointer;
begin
  result:=0;
  for i:=0 to length(handlelist)-1 do
    if handlelist[i].processhandle=hProcess then
    begin
      if hdevice<>INVALID_HANDLE_VALUE then
      begin
        x.processid:=handlelist[i].processid;
        x.baseaddress:=lpAddress;
        x.size:=dwsize;
        x.AllocationType:=flAllocationType;
        x.Protect:=flProtect;

        cc:=IOCTL_CE_ALLOCATEMEM;
        deviceiocontrol(hdevice,cc,@x,sizeof(x),@r,sizeof(r),br,nil);

        result:=r;
        exit; //we're done here
      end;
    end;

  //still here
  result:=VirtualAllocEx(hprocess,lpAddress,dwSize,flAllocationType,flProtect);
end;

procedure testapc(  NormalContext:pointer; SystemArgument1:pointer; SystemArgument2:pointer);stdcall;
var tid: dword;
    s: string;
begin
  s:=inttohex(dword(NormalContext),8)+' - '+inttohex(dword(SystemArgument1),8)+' - '+inttohex(dword(SystemArgument2),8);

  //CreateThread(nil,0,systemArgument1,SystemArgument2,false,@tid);
  messagebox(0,pchar(s),'APC rules',mb_ok);
end;


Function CreateRemoteAPC(threadid: dword; lpStartAddress: TFNAPCProc): THandle; stdcall;
var i: integer;
    br,cc: dword;
    x:record
      threadid: dword;
      addresstoexecute: pointer;
    end;

begin
  result:=0;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    x.addresstoexecute:=lpStartAddress;
    x.threadid:=threadid;

    cc:=IOCTL_CE_CREATEAPC;
    if deviceiocontrol(hdevice,cc,@x,sizeof(x),nil,0,br,nil) then
      result:=666 //sorry dude, no threadid returned, and no way of checking if it succeeded or not
    else
      result:=0;
  end;
end;

function setAlternateDebugMethod(var int1apihook:dword; var OriginalInt1handler:dword):BOOL; stdcall;
var
  x:record
    int1apihook: dword;
    Originalint1handler: dword;
  end;
    br,cc: dword;
    i:integer;
begin
  outputdebugstring('setAlternateDebugMethod');
  result:=false;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_USEALTERNATEMETHOD;

    if deviceiocontrol(hdevice,cc,nil,0,@x,sizeof(x),br,nil) then
    begin
      //this data will be send to cheat engine. it will know what to do with it...
      int1apihook:=x.int1apihook;
      OriginalInt1handler:=x.Originalint1handler;
      result:=true;
    end;

    usealternatedebugmethod:=result; //once set you can't unset it
  end;
end;

function getAlternateDebugMethod:BOOL; stdcall;
var
    x: boolean;
    br,cc: dword;
begin
  outputdebugstring('getAlternateDebugMethod');
  
  //check the kernel if this method has been set
  result:=false;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_ISUSINGALTERNATEMETHOD;
    if deviceiocontrol(hdevice,cc,nil,0,@x,sizeof(x),br,nil) then
      result:=x;

    usealternatedebugmethod:=x;
  end else result:=false;
end;

function SetGlobalDebugState(state: boolean): BOOL; stdcall;
var
  x: dword;
  br,cc: dword;
begin
  outputdebugstring('SetGlobalDebugState');

  if state then
    x:=1
  else
    x:=0;   

  result:=false;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_SETGLOBALDEBUGSTATE;
    result:=deviceiocontrol(hdevice,cc,@x,sizeof(x),nil,0,br,nil);
  end else result:=false;
end;

function StartCEKernelDebug:BOOL; stdcall;
var
    br,cc: dword;
    i:integer;
    cpunr,PA,SA:Dword;
    cpunr2:byte;
begin
  result:=false;
  outputdebugstring('DebugProcess function');

  if usealternatedebugmethod then
  begin
    result:=true;
    exit;
  end;

  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_HOOKINTS;

    GetProcessAffinityMask(getcurrentprocess,PA,SA);

    //first hook the interrupts if needed
    cpunr2:=0;
    cpunr:=1;
    while (cpunr<=PA) do
    begin
      if ((cpunr) and PA)>0 then
      begin
        SetProcessAffinityMask(getcurrentprocess,cpunr);
        //create a new thread. (Gues on what cpu it will run at...)

        with THookIDTThread.Create(true) do
        begin
          try
            cpunr:=cpunr2;
            resume;

            while not done do sleep(10); //the sleep should also cause a taskswitch but I'm not 100% sure

            if not succeeded then
            begin
              SetProcessAffinityMask(getcurrentprocess,PA);
              messagebox(0,pchar('Failure when changing the interrupt handler on CPU '+inttostr(cpunr)),'',mb_ok);
              exit;
            end;
          finally
            free;
          end;
        end;

      end;
      if cpunr=$80000000 then break;
      inc(cpunr,cpunr);
      inc(cpunr2);
    end;

    SetProcessAffinityMask(getcurrentprocess,PA); //multi processors are so fun. It'd be a waste not to use it
    outputdebugstring('going to start the hooker');
    hooker:=thookidtconstantly.Create(false);

    result:=true;
  end;
end;

function SetMemoryAccessWatch(processid:dword;address:DWORD;size: byte;debugtype:byte):BOOL; stdcall;
type Tinput=record
  ProcessID:DWORD;
  Address:DWORD;
  Length:BYTE;
  RWE:BYTE;
end;
var input:TInput;
    br,cc: dword;
begin
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    result:=StartCEKernelDebug;
    input.Processid:=processid;
    input.Address:=address;
    input.length:=size;
    input.RWE:=debugtype;
    cc:=IOCTL_CE_DEBUGPROCESS;
    result:=result and deviceiocontrol(hdevice,cc,@input,sizeof(input),@input,0,br,nil);
  end;
end;

function StopRegisterChange(regnr:integer):BOOL; stdcall;
var x,cc: dword;
begin
  outputdebugstring('DBK32: StopRegisterChange called');
  result:=false;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_STOP_DEBUGPROCESS_CHANGEREG;
    result:=deviceiocontrol(hdevice,cc,@regnr,4,nil,0,x,nil);
  end;
end;

function StopDebugging:BOOL; stdcall;
var x,cc: dword;
begin
  outputdebugstring('DBK32: StopDebugging called');
  result:=false;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_STOPDEBUGGING;
    result:=deviceiocontrol(hdevice,cc,nil,0,nil,0,x,nil);
  end;
end;

function DebugProcess(processid:dword;address:DWORD;size: byte;debugtype:byte):BOOL; stdcall;
begin
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    result:=StartCEKernelDebug;
    result:=result and SetMemoryAccessWatch(processid,address,size,debugtype);
  end;
end;

function ChangeRegOnBP(Processid:dword; address: dword; debugreg: integer; changeEAX,changeEBX,changeECX,changeEDX,changeESI,changeEDI,changeEBP,changeESP,changeEIP,changeCF,changePF,changeAF,changeZF,changeSF,changeOF:BOOLEAN; newEAX,newEBX,newECX,newEDX,newESI,newEDI,newEBP,newESP,newEIP:DWORD; newCF,newPF,newAF,newZF,newSF,newOF:BOOLEAN):BOOLEAN; stdcall;
type TChangeReg=record
  BreakAddress: DWORD;
  newEAX,newEBX,newECX,newEDX,newESI,newEDI,newEBP,newESP,newEIP: DWORD;
  newCF,newPF,newAF,newZF,newSF,newOF:BOOLEAN;
  changeEAX,changeEBX,changeECX,changeEDX,changeESI,changeEDI,changeEBP,changeESP,changeEIP:BOOLEAN;
  changeCF,changePF,changeAF,changeZF,changeSF,changeOF:BOOLEAN;
  Active:BOOLEAN;
end;

type TBuf=record
  ProcessID: DWORD;
  debugreg: integer;
  ChangeReg: TChangeReg;
end;
var buf: TBuf;
    x,cc: dword;
begin
  outputdebugstring('DBK32: ChangeRegOnBP called');
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    result:=StartCEKernelDebug;

    if not result then exit;
    buf.ProcessID:=Processid;
    buf.debugreg:=debugreg;
    buf.ChangeReg.BreakAddress:=address;
    buf.ChangeReg.newEAX:=neweax;
    buf.ChangeReg.newEBX:=newebx;
    buf.ChangeReg.newECX:=newecx;
    buf.ChangeReg.newEDX:=newedx;
    buf.ChangeReg.newESI:=newesi;
    buf.ChangeReg.newEDI:=newedi;
    buf.ChangeReg.newEBP:=newebp;
    buf.ChangeReg.newESP:=newesp;
    buf.ChangeReg.newEIP:=neweip;
    buf.ChangeReg.newCF:=newcf;
    buf.ChangeReg.newPF:=newpf;
    buf.ChangeReg.newAF:=newaf;
    buf.ChangeReg.newZF:=newzf;
    buf.ChangeReg.newSF:=newsf;
    buf.ChangeReg.newOF:=newof;

    buf.ChangeReg.changeEAX:=changeeax;
    buf.ChangeReg.changeEBX:=changeebx;
    buf.ChangeReg.changeECX:=changeecx;
    buf.ChangeReg.changeEDX:=changeedx;
    buf.ChangeReg.changeESI:=changeesi;
    buf.ChangeReg.changeEDI:=changeedi;
    buf.ChangeReg.changeEBP:=changeebp;
    buf.ChangeReg.changeESP:=changeesp;
    buf.ChangeReg.changeEIP:=changeeip;
    buf.ChangeReg.changeCF:=changecf;
    buf.ChangeReg.changePF:=changepf;
    buf.ChangeReg.changeAF:=changeaf;
    buf.ChangeReg.changeZF:=changezf;
    buf.ChangeReg.changeSF:=changesf;
    buf.ChangeReg.changeOF:=changeof;

    cc:=IOCTL_CE_DEBUGPROCESS_CHANGEREG;
    result:=result and deviceiocontrol(hdevice,cc,@buf,sizeof(buf),@buf,0,x,nil);


  end;
end;


function RetrieveDebugData(Buffer: pointer):integer; stdcall; //buffer has to be at least 1800 bytes
var x,cc: dword;
    buf: pointer;
begin
//50*35 bytes=
  result:=-1; //-1=error
  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    getmem(buf,1801); //1801 because the first byte is the count
    cc:=IOCTL_CE_RETRIEVEDEBUGDATA;
    if deviceiocontrol(hdevice,cc,buf,1801,buf,1801,x,nil) then
    begin
      result:=pbyte(buf)^;
      copymemory(buffer,pointeR(dword(buf)+1),1801);
    end;

    freemem(buf);
  end;
end;

function WaitForProcessListData(processpointer:pointer;threadpointer:pointer;timeout:dword):dword; stdcall;
type tprocesseventstruct=record
  Created:BOOL;
  ProcessID:DWORD;
  PEProcess:DWORD;
end;
type tthreadeventstruct=record
  Created:BOOL;
  ProcessID:DWORD;
  ThreadID:dword;
end;
var cc,x:dword;
    eventarray: array of thandle;
begin
  if hdevice=INVALID_HANDLE_VALUE then
  begin
    result:=0;
    exit;
  end;

//assuming the buffer is at least the size of 50* the biggest struct (threadstruct in this case)
  //retrieve the processevents
  //wait for a process create event to be set
  setlength(eventarray,2);
  eventarray[0]:=processevent;
  eventarray[1]:=threadevent;
  result:=WaitForMultipleObjects(2,@eventarray[0],false,timeout);

//  result:=WaitForSingleObject(processevent,timeout);

  if result<>WAIT_FAILED then
  begin

    //processevent
    if (hdevice<>INVALID_HANDLE_VALUE) then
    begin
      cc:=IOCTL_CE_GETPROCESSEVENTS;
      deviceiocontrol(hdevice,cc,processpointer,sizeof(tprocesseventstruct)*50+1,processpointer,sizeof(tprocesseventstruct)*50+1,x,nil);
    end;

    //thread event
    if (hdevice<>INVALID_HANDLE_VALUE) then
    begin
      cc:=IOCTL_CE_GETTHREADEVENTS;
      deviceiocontrol(hdevice,cc,threadpointer,sizeof(tthreadeventstruct)*50+1,threadpointer,sizeof(tthreadeventstruct)*50+1,x,nil);
    end;

  end;
end;


function StartProcessWatch:BOOL;stdcall;
var cc,x: dword;
begin
  result:=false;
  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_STARTPROCESSWATCH;
    result:=deviceiocontrol(hdevice,cc,@x,0,@x,0,x,nil);
  end;
end;

function MakeWritable(Address,Size:dword;copyonwrite:boolean): boolean; stdcall;
type TMemoryDesignation=record
  StartAddress:DWORD;
  Size: DWORD;
  CopyOnWrite: BYTE;
end;
var cc: dword;
    x: TMemoryDesignation;
begin
  result:=false;
  x.StartAddress:=Address;
  x.Size:=Size;
  if copyonwrite then x.CopyOnWrite:=1 else x.CopyOnWrite:=0;

  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_MAKEWRITABLE;
    result:=deviceiocontrol(hdevice,cc,@x,sizeof(x),@x,0,cc,nil);
  end;
end;

function KernelAlloc(size: dword):pointer; stdcall;
type TInput=record
  Size: DWORD;
end;
var cc: dword;
    x: TInput;
    output: pointer;
begin
  result:=nil;
  x.Size:=size;

  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_ALLOCATEMEM_NONPAGED;
    if deviceiocontrol(hdevice,cc,@x,sizeof(x),@output,sizeof(output),cc,nil) then
      result:=output;
  end;
end;

function GetKProcAddress(s: pwidechar):pointer; stdcall;
var cc: dword;
    output: pointer;
    d: dword;
    err: integer;
    st: string;
begin
  result:=nil;

  st:=s;

  if length(st)<4 then exit;

  val('$'+st,d,err);
  if err=0 then exit;

  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_GETPROCADDRESS;
    output:=0;
    if deviceiocontrol(hdevice,cc,@s,sizeof(s),@output,sizeof(output),cc,nil) then
      result:=output;
  end;

end;

function GetSDTEntry(nr: integer; address: PDWORD; paramcount: PBYTE):boolean; stdcall;
type TInput=record
  table: dword;
  nr: dword;
end;
type Toutput=record
  address: dword;
  paramcount: byte;
end;

var cc: dword;
    x: TInput;
    y: toutput;
begin
  result:=false;
  x.table:=0;
  x.nr:=nr;

  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_GETSDTADDRESS;
    result:=deviceiocontrol(hdevice,cc,@x,sizeof(x),@y,sizeof(y),cc,nil);
    if result then
    begin
      address^:=y.address;
      paramcount^:=y.paramcount;
    end;
  end;
end;

function SetSDTEntry(nr: integer; address: DWORD; paramcount: BYTE):boolean; stdcall;
type TInput=record
  table: dword;
  nr: dword;
  address:dword;
  paramcount:byte;
end;

var cc: dword;
    x: TInput;
begin
  result:=false;
  x.table:=0;
  x.nr:=nr;
  x.address:=address;
  x.paramcount:=paramcount;

  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_SETSDTADDRESS;
    result:=deviceiocontrol(hdevice,cc,@x,sizeof(x),nil,0,cc,nil);
  end;
end;

function GetSSDTEntry(nr: integer; address: PDWORD; paramcount: PBYTE):boolean; stdcall;
type TInput=record
  table: dword;
  nr: dword;
end;
type Toutput=record
  address: dword;
  paramcount: byte;
end;

var cc: dword;
    x: TInput;
    y: toutput;
begin
  result:=false;
  x.table:=1;
  x.nr:=nr;

  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_GETSDTADDRESS;
    result:=deviceiocontrol(hdevice,cc,@x,sizeof(x),@y,sizeof(y),cc,nil);
    if result then
    begin
      address^:=y.address;
      paramcount^:=y.paramcount;
    end;
  end;
end;

function SetSSDTEntry(nr: integer; address: DWORD; paramcount: BYTE):boolean; stdcall;
type TInput=record
  table: dword;
  nr: dword;
  address:dword;
  paramcount:byte;
end;

var cc: dword;
    x: TInput;
begin
  result:=false;
  x.table:=1;
  x.nr:=nr;
  x.address:=address;
  x.paramcount:=paramcount;

  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_SETSDTADDRESS;
    result:=deviceiocontrol(hdevice,cc,@x,sizeof(x),nil,0,cc,nil);
  end;
end;


function RewriteKernel32:boolean; stdcall;
begin
  //modifies the code of NtOpenProcess,NtOpenThread,OpenProcess,OpenThread to point to this dll's functions
end;

function RestoreKernel32: boolean; stdcall;
begin

end;


function CTL_CODE(DeviceType, Func, Method, Access : integer) : integer;
begin
  Result := (DeviceType shl 16) or (Access shl 14) or (Func shl 2) or Method;
end;

function InitializeDriver(Address,size:dword):BOOL; stdcall;
type tinput=record
  address: dword;
  size:dword;
  NtUserBuildHwndList_callnumber: Dword;
  NtUserQueryWindow_callnumber:dword;
  NtUserFindWindowEx_callnumber:DWORD;
  NtUserGetForegroundWindow_callnumber:DWORD;
  activelinkoffset: dword;
  processnameoffset:dword;
  debugportoffset:dword;
  processevent: dword; //event handles (driver rev. 10+)
  threadevent: dword;
end;
var cc: dword;
    buf: tinput;
    res: dword absolute buf;
    x:dword;

    callnumberfile: tfilestream;
    windowsversion:_osversioninfoa;
    majorversion,minorversion,buildnumber: dword;
    CSDVersion: array [0..127] of char;
    a: boolean;
    i,j: integer;
begin
  result:=false;
  sdtshadow:=0;

  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    processevent:=CreateEvent(nil,false,false,nil);
    threadevent:=CreateEvent(nil,false,false,nil);

// old method--\/
//    processevent:=OpenEvent(SYNCHRONIZE,false,pchar(processeventname));
//    threadevent:=OpenEvent(SYNCHRONIZE,false,pchar(threadeventname));


    zeromemory(@buf,sizeof(buf));
    buf.address:=address;
    buf.size:=size;
    buf.NtUserBuildHwndList_callnumber:=0;
    buf.NtUserQueryWindow_callnumber:=0;
    buf.NtUserFindWindowEx_callnumber:=0;
    buf.NtUserGetForegroundWindow_callnumber:=0;

    buf.activelinkoffset:=0;
    buf.processnameoffset:=0;
    buf.debugportoffset:=0;

    buf.processevent:=processevent;
    buf.threadevent:=threadevent;


    //check if there is a callnumber.txt file in the rootdir, and if so use it
    if fileexists(extractfilepath(driverloc)+'kerneldata.dat') then
    begin
      //read the file, first 4 bytes is the callnumber of NtUserBuildHwndList_callnumber
      try
        callnumberfile:=tfilestream.create(extractfilepath(driverloc)+'kerneldata.dat',fmOpenRead,fmShareDenyNone	);
        try
          windowsversion.dwOSVersionInfoSize:=sizeof(windowsversion);
          getversionex(windowsversion);


          callnumberfile.ReadBuffer(MajorVersion,4);
          callnumberfile.ReadBuffer(MinorVersion,4);
          callnumberfile.ReadBuffer(BuildNumber,4);
          callnumberfile.ReadBuffer(CSDVersion,128);
        //  a:=comparemem(@CSDVersion[0],@windowsversion.szCSDVersion[0],128);

          a:=true;
          i:=0;
          while a and (i<128) and (windowsversion.szCSDVersion[i]<>#0) and (CSDVersion[i]<>#0) do
          begin
            a:=CSDVersion[i]=windowsversion.szCSDVersion[i];
            inc(i);
          end;

          if (not a) or (majorversion<>windowsversion.dwMajorVersion) or (MinorVersion<>windowsversion.dwMinorVersion) or (buildnumber<>windowsversion.dwBuildNumber) then
          begin
            messagebox(0,'It is recommended to run the systemcallretriever since the kerneldata.dat you have is outdated and will not be used. Of course, if this is the systemcallretriever, ignore this message...','Outdated kerneldata.dat',mb_ok);
//not a valid kerneldata.dat file            
          end
          else
          begin
            callnumberfile.ReadBuffer(x,4);
            buf.NtUserBuildHwndList_callnumber:=x;

            callnumberfile.ReadBuffer(x,4);
            buf.NtUserQueryWindow_callnumber:=x;

            callnumberfile.ReadBuffer(x,4);
            buf.NtUserFindWindowEx_callnumber:=x;

            callnumberfile.ReadBuffer(x,4);
            buf.NtUserGetForegroundWindow_callnumber:=x;

            callnumberfile.ReadBuffer(buf.activelinkoffset,4);
            callnumberfile.ReadBuffer(buf.processnameoffset,4);
            callnumberfile.ReadBuffer(buf.debugportoffset,4);

            debugport:=buf.debugportoffset;
            processname:=buf.processnameoffset;

            //----------------Add this part to the file---------
            ThreadsProcess:=$220;
            ThreadListEntry:=$3c;
          end;
        finally
          callnumberfile.free;
        end;
      except

      end;
    end;

    cc:=IOCTL_CE_INITIALIZE;
    if deviceiocontrol(hdevice,cc,@buf,sizeof(tinput),@buf,sizeof(tinput),x,nil) then
    begin
      result:=true;
      SDTShadow:=res;
    end;
    ownprocess:=OP(PROCESS_ALL_ACCESS,false,getcurrentprocessid);
  end;
end;


function GetWin32KAddress(var address:DWORD;var size:dworD):boolean;
var need:dword;
    p: pointer;
    oldx: dword;
    x: array of pointer;
    i,j: integer;
    count: integer;
    drivername: pchar;
    nearest: dword; //nearest other driver (AFTER win32k.sys)
begin
  result:=false;

  copymemory(@oldx,@x,4);

  EnumDevicedrivers(nil,0,need);
  count:=need div 4;
  getmem(p,need);
  try
    if enumDevicedrivers(p,need,need) then
    begin
      getmem(drivername,200);
      copymemory(@x,@p,4);
      try

        for i:=0 to count-1 do
        begin

          GetDevicedriverBaseName(x[i],drivername,200);
          if lowercase(drivername)='win32k.sys' then
          begin
            address:=dword(x[i]);

            nearest:=$ffffffff;
            for j:=0 to count-1 do
              if (dword(x[j])>dword(x[i])) and (dword(x[j])<nearest) then //it's bigger than winb32k.sys, but closer to it than the last closts I found
                nearest:=dword(x[j]);

            size:=nearest-address;

            result:=true;
            exit;
          end;
        end;


      finally
        copymemory(@x,@oldx,4);

        freemem(drivername);
      end;


    end;
  finally
    freemem(p);
  end;

end;

var hscManager: thandle;
    hservicE: thandle;

var sav: pchar;

    apppath: pchar;


    win32kaddress,win32size:dword;
    servicename,sysfile: string;
    dataloc: string;

    vmx_p1_txt,vmx_p2_txt: string;


    reg: tregistry;
    driverdat: textfile;

//    servicestatus: _service_status;
initialization
begin
  ioctl:=true;
  kernel32dll:=loadlibrary('kernel32.dll');

  usealternatedebugmethod:=false;
  Successfullyloaded:=false;
  iamprotected:=false;
  apppath:=nil;
  hooker:=nil;
  setlength(handlelist,0);
  hSCManager := OpenSCManager(nil, nil, GENERIC_READ or GENERIC_WRITE);
  try
    getmem(apppath,250);
    GetModuleFileName(0,apppath,250);

    dataloc:=extractfilepath(apppath)+'driver.dat';
    if not fileexists(dataloc) then
    begin
      servicename:='CEDRIVER53';
      processeventname:='DBKProcList53';
      threadeventname:='DBKThreadList53';
      sysfile:='dbk32.sys';
      vmx_p1_txt:='76543210';
      vmx_p2_txt:='fedcba98';
    end
    else
    begin
      assignfile(driverdat,dataloc);
      reset(driverdat);
      readln(driverdat,servicename);
      readln(driverdat,processeventname);
      readln(driverdat,threadeventname);
      readln(driverdat,sysfile);
      readln(driverdat,vmx_p1_txt);
      readln(driverdat,vmx_p2_txt);
      closefile(driverdat);

      
    end;



    driverloc:=extractfilepath(apppath)+sysfile;
  finally
    freemem(apppath);
  end;

  if not fileexists(driverloc) then
  begin
    messagebox(0,'You are missing the driver. Try reinstalling cheat engine, and try to disable your anti-virus before doing so.','Driver error',MB_ICONERROR or mb_ok);
    hDevice:=INVALID_HANDLE_VALUE;
    exit;
  end;



  if hscmanager<>0 then
  begin
    hService := OpenService(hSCManager, pchar(servicename), SERVICE_ALL_ACCESS);
    if hService=0 then
    begin
      hService:=CreateService(
         hSCManager,           // SCManager database
         pchar(servicename),   // name of service
         pchar(servicename),   // name to display
         SERVICE_ALL_ACCESS,   // desired access
         SERVICE_KERNEL_DRIVER,// service type
         SERVICE_DEMAND_START, // start type
         SERVICE_ERROR_NORMAL, // error control type
         pchar(driverloc),     // service's binary
         nil,                  // no load ordering group
         nil,                  // no tag identifier
         nil,                  // no dependencies
         nil,                  // LocalSystem account
         nil                   // no password
      );
    end
    else
    begin
      //make sure the service points to the right file
      ChangeServiceConfig(hservice,
                          SERVICE_KERNEL_DRIVER,
                          SERVICE_DEMAND_START,
                          SERVICE_ERROR_NORMAL,
                          pchar(driverloc),
                          nil,
                          nil,
                          nil,
                          nil,
                          nil,
                          pchar(servicename));


    end;

    if hservice<>0 then
    begin
      sav:=nil;

      //setup the configuration parameters before starting the driver
      reg:=tregistry.Create;
      reg.RootKey:=HKEY_LOCAL_MACHINE;
      if not reg.OpenKey('\SYSTEM\CurrentControlSet\Services\'+servicename,false) then
      begin
        messagebox(0,'Failure to configure the driver','Driver Error',MB_ICONERROR or mb_ok);
        hDevice:=INVALID_HANDLE_VALUE;
        exit;
      end;

      reg.WriteString('A','\Device\'+servicename);
      reg.WriteString('B','\DosDevices\'+servicename);
      reg.WriteString('C','\BaseNamedObjects\'+processeventname);
      reg.WriteString('D','\BaseNamedObjects\'+threadeventname);

      if not startservice(hservice,0,sav) then
      begin
        if getlasterror=577 then
        begin
          messagebox(0,'Please reboot and press F8 during boot. Then choose "allow unsigned drivers"','DBK32 error',MB_ICONERROR or mb_ok);

        end;
      end;

      closeservicehandle(hservice);
    end else
    begin
      messagebox(0,'The service couldn''t get opened and also couldn''t get created.'+' Check if you have the needed rights to create a service, or call your system admin (Who''ll probably beat you up for even trying this). Untill this is fixed you won''t be able to make use of the enhancements the driver gives you','DBK32 Error',MB_ICONERROR or mb_ok);
      hDevice:=INVALID_HANDLE_VALUE;
      exit;
    end;

    hdevice:=0;
    hDevice := CreateFile(pchar('\\.\'+servicename),
                  GENERIC_READ or GENERIC_WRITE,
                  FILE_SHARE_READ or FILE_SHARE_WRITE,
                  nil,
                  OPEN_EXISTING,
                  FILE_FLAG_OVERLAPPED,
                  0);


    if hdevice=INVALID_HANDLE_VALUE then
      messagebox(0,'The driver couldn''t be opened! It''s not loaded or not responding. I recommend to reboot your system and try again','DBK32.DLL Error',MB_ICONERROR or MB_OK)
    else
    begin
      //Get the address of win32k.sys
      if GetDriverVersion<>currentversion then
      begin
        closehandle(hdevice);
        messagebox(0,'The driver that is currently loaded belongs to a different version of Cheat Engine. Please unload this driver or reboot.','DBK32.dll',MB_ICONERROR or MB_OK);

        hdevice:=INVALID_HANDLE_VALUE;
      end
      else
      begin
        if GetWin32KAddress(win32kAddress,win32size) then
        begin
          if not InitializeDriver(win32kAddress,win32size) then
          begin
            messagebox(0,'The driver failed to successfully initialize. Some functions may not completly work','DBK32.dll',MB_ICONERROR or MB_OK);
          end;
        end
        else
          messagebox(0,'There was an error while trying to find the win32k.sys device driver. This means that some functions will not work','DBK32.dll',MB_ICONERROR or MB_OK);

        Successfullyloaded:=true;
      end;
    end;

    //successfully initialized, say goodbye to the init params
    reg.DeleteValue('A');
    reg.DeleteValue('B');
    reg.DeleteValue('C');
    reg.DeleteValue('D');


    closeservicehandle(hscmanager);
  end;


  try
    configure_vmx(strtoint('$'+vmx_p1_txt), strtoint('$'+vmx_p2_txt) );
  except
    //couldn't parse the password
  end;

end;


finalization
begin
  if ownprocess<>0 then
    closehandle(ownprocess);
    
  if hooker<>nil then hooker.Terminate;
end;
end.
