 unit DBK32functions;

{$MODE Delphi}

interface

{$ifdef windows}
uses
  jwawindows, windows, sysutils, classes, types, registry, multicpuexecution,
  forms,dialogs, controls, maps, globals;

//xp sp2
//ThreadsProcess=220
//ThreadListEntry=22c

{$endif}

const currentversion=2000027;

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
//const IOCTL_CE_PROTECTME					    = (IOCTL_UNKNOWN_BASE shl 16) or ($0809 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_GETCR3 					      = (IOCTL_UNKNOWN_BASE shl 16) or ($080a shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
//you really don't want to do this in usermode const IOCTL_CE_SETCR3 					      = (IOCTL_UNKNOWN_BASE shl 16) or ($080b shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_GETSDT 					      = (IOCTL_UNKNOWN_BASE shl 16) or ($080c shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_INITIALIZE     		    = (IOCTL_UNKNOWN_BASE shl 16) or ($080d shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
//const IOCTL_CE_DONTPROTECTME			    = (IOCTL_UNKNOWN_BASE shl 16) or ($080e shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
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
//const IOCTL_CE_DEBUGPROCESS_CHANGEREG	=	(IOCTL_UNKNOWN_BASE shl 16) or ($081a shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_STOPDEBUGGING				  = (IOCTL_UNKNOWN_BASE shl 16) or ($081b shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
//const	IOCTL_CE_STOP_DEBUGPROCESS_CHANGEREG =	(IOCTL_UNKNOWN_BASE shl 16) or ($081c shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
//const	IOCTL_CE_USEALTERNATEMETHOD		  =	(IOCTL_UNKNOWN_BASE shl 16) or ($081d shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
//const	IOCTL_CE_ISUSINGALTERNATEMETHOD =	(IOCTL_UNKNOWN_BASE shl 16) or ($081e shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const	IOCTL_CE_ALLOCATEMEM				    =	(IOCTL_UNKNOWN_BASE shl 16) or ($081f shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_CREATEAPC					    =	(IOCTL_UNKNOWN_BASE shl 16) or ($0820 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_GETPETHREAD				    =	(IOCTL_UNKNOWN_BASE shl 16) or ($0821 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);

const IOCTL_CE_SUSPENDTHREAD			    =	(IOCTL_UNKNOWN_BASE shl 16) or ($0822 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_RESUMETHREAD				    =	(IOCTL_UNKNOWN_BASE shl 16) or ($0823 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_SUSPENDPROCESS			    =	(IOCTL_UNKNOWN_BASE shl 16) or ($0824 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_RESUMEPROCESS			    =	(IOCTL_UNKNOWN_BASE shl 16) or ($0825 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);

const IOCTL_CE_ALLOCATEMEM_NONPAGED   =	(IOCTL_UNKNOWN_BASE shl 16) or ($0826 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_GETPROCADDRESS			    =	(IOCTL_UNKNOWN_BASE shl 16) or ($0827 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
//const IOCTL_CE_SETSDTADDRESS 			    =	(IOCTL_UNKNOWN_BASE shl 16) or ($0828 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_GETSDTADDRESS 			    =	(IOCTL_UNKNOWN_BASE shl 16) or ($0829 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);

const IOCTL_CE_GETGDT 					  	  = (IOCTL_UNKNOWN_BASE shl 16) or ($082a shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_SETCR4 					  	  = (IOCTL_UNKNOWN_BASE shl 16) or ($082b shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_VMXCONFIG				  	  = (IOCTL_UNKNOWN_BASE shl 16) or ($082d shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_GETCR0 					  	  = (IOCTL_UNKNOWN_BASE shl 16) or ($082e shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_USERDEFINEDINTERRUPTHOOK = (IOCTL_UNKNOWN_BASE shl 16) or ($082f shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
//const IOCTL_CE_MAKEKERNELCOPY		  	  = (IOCTL_UNKNOWN_BASE shl 16) or ($082f shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_SETGLOBALDEBUGSTATE    = (IOCTL_UNKNOWN_BASE shl 16) or ($0830 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);

const IOCTL_CE_CONTINUEDEBUGEVENT     = (IOCTL_UNKNOWN_BASE shl 16) or ($0831 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_WAITFORDEBUGEVENT      = (IOCTL_UNKNOWN_BASE shl 16) or ($0832 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);

const IOCTL_CE_GETDEBUGGERSTATE       = (IOCTL_UNKNOWN_BASE shl 16) or ($0833 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_SETDEBUGGERSTATE       = (IOCTL_UNKNOWN_BASE shl 16) or ($0834 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_GD_SETBREAKPOINT       = (IOCTL_UNKNOWN_BASE shl 16) or ($0835 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_TOUCHDEBUGREGISTER     = (IOCTL_UNKNOWN_BASE shl 16) or ($0836 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);

const IOCTL_CE_LAUNCHDBVM             = (IOCTL_UNKNOWN_BASE shl 16) or ($083a shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_UNHOOKALLINTERRUPTS    = (IOCTL_UNKNOWN_BASE shl 16) or ($083b shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_EXECUTE_CODE           = (IOCTL_UNKNOWN_BASE shl 16) or ($083c shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_GETPROCESSNAMEADDRESS  = (IOCTL_UNKNOWN_BASE shl 16) or ($083d shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_SETKERNELSTEPABILITY   = (IOCTL_UNKNOWN_BASE shl 16) or ($083e shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);

const IOCTL_CE_READMSR                = (IOCTL_UNKNOWN_BASE shl 16) or ($083f shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_WRITEMSR               = (IOCTL_UNKNOWN_BASE shl 16) or ($0840 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);

const IOCTL_CE_SETSTORELBR            = (IOCTL_UNKNOWN_BASE shl 16) or ($0841 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);

const IOCTL_CE_ULTIMAP                = (IOCTL_UNKNOWN_BASE shl 16) or ($0842 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_ULTIMAP_DISABLE        = (IOCTL_UNKNOWN_BASE shl 16) or ($0843 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_ULTIMAP_WAITFORDATA    = (IOCTL_UNKNOWN_BASE shl 16) or ($0844 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_ULTIMAP_CONTINUE       = (IOCTL_UNKNOWN_BASE shl 16) or ($0845 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_ULTIMAP_FLUSH          = (IOCTL_UNKNOWN_BASE shl 16) or ($0846 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);

const IOCTL_CE_GETMEMORYRANGES        = (IOCTL_UNKNOWN_BASE shl 16) or ($0847 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_STARTACCESMONITOR      = (IOCTL_UNKNOWN_BASE shl 16) or ($0848 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);

const IOCTL_CE_ENUMACCESSEDMEMORY     = (IOCTL_UNKNOWN_BASE shl 16) or ($0849 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_GETACCESSEDMEMORYLIST  = (IOCTL_UNKNOWN_BASE shl 16) or ($084a shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);

const IOCTL_CE_WRITESIGNOREWP         = (IOCTL_UNKNOWN_BASE shl 16) or ($084b shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_FREE_NONPAGED          = (IOCTL_UNKNOWN_BASE shl 16) or ($084c shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);

const IOCTL_CE_MAP_MEMORY             = (IOCTL_UNKNOWN_BASE shl 16) or ($084d shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_UNMAP_MEMORY           = (IOCTL_UNKNOWN_BASE shl 16) or ($084e shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);

const IOCTL_CE_ULTIMAP2               = (IOCTL_UNKNOWN_BASE shl 16) or ($084f shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_DISABLEULTIMAP2        = (IOCTL_UNKNOWN_BASE shl 16) or ($0850 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);

const IOCTL_CE_ULTIMAP2_WAITFORDATA   = (IOCTL_UNKNOWN_BASE shl 16) or ($0851 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_ULTIMAP2_CONTINUE      = (IOCTL_UNKNOWN_BASE shl 16) or ($0852 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_ULTIMAP2_FLUSH         = (IOCTL_UNKNOWN_BASE shl 16) or ($0853 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_ULTIMAP2_PAUSE         = (IOCTL_UNKNOWN_BASE shl 16) or ($0854 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_ULTIMAP2_RESUME        = (IOCTL_UNKNOWN_BASE shl 16) or ($0855 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_ULTIMAP2_LOCKFILE      = (IOCTL_UNKNOWN_BASE shl 16) or ($0856 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_ULTIMAP2_RELEASEFILE   = (IOCTL_UNKNOWN_BASE shl 16) or ($0857 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);

const IOCTL_CE_ULTIMAP_PAUSE          = (IOCTL_UNKNOWN_BASE shl 16) or ($0858 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_ULTIMAP_RESUME         = (IOCTL_UNKNOWN_BASE shl 16) or ($0859 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);

const IOCTL_CE_ULTIMAP2_GETTRACESIZE  = (IOCTL_UNKNOWN_BASE shl 16) or ($085a shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_ULTIMAP2_RESETTRACESIZE= (IOCTL_UNKNOWN_BASE shl 16) or ($085b shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);

const IOCTL_CE_ENABLE_DRM             = (IOCTL_UNKNOWN_BASE shl 16) or ($085c shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_GET_PEB                = (IOCTL_UNKNOWN_BASE shl 16) or ($085d shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_QUERYINFORMATIONPROCESS= (IOCTL_UNKNOWN_BASE shl 16) or ($085e shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);


const IOCTL_CE_LOCK_MEMORY            = (IOCTL_UNKNOWN_BASE shl 16) or ($0860 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_UNLOCK_MEMORY          = (IOCTL_UNKNOWN_BASE shl 16) or ($0861 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);

const IOCTL_CE_ALLOCATE_MEMORY_FOR_DBVM = (IOCTL_UNKNOWN_BASE shl 16) or ($0862 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);

{$ifdef windows}
type TDeviceIoControl=function(hDevice: THandle; dwIoControlCode: DWORD; lpInBuffer: Pointer; nInBufferSize: DWORD; lpOutBuffer: Pointer; nOutBufferSize: DWORD; var lpBytesReturned: DWORD; lpOverlapped: POverlapped): BOOL; stdcall;
type TFNAPCProc = TFarProc;
{$endif}



type
  TPhysicalMemoryRange=packed record
                         base: uint64;
                         size: uint64;
                       end;
  TPhysicalMemoryRanges=array of TPhysicalMemoryRange;

  TPhysicalMemoryRangesArray=array [0..10000] of TPhysicalMemoryRange;
  PPhysicalMemoryRangesArray=^TPhysicalMemoryRangesArray;



type
  THandleListEntry=record
    processid: dword;
    validhandle: boolean; //it's a real handle. Else a pseudo handle
    specialHandle: boolean; //do not close this
  end;

  PHandleListEntry=^THandleListEntry;



type TClient_ID=record
  processid: thandle;
  threadid: thandle;
end;
type PClient_ID=^TClient_ID;

type
  TQwordArray=array[0..9999] of QWORD;
  PQwordArray=^TQwordArray;

type
  TUltimapEvent=packed record
    DataReadyEvent: QWORD;
    DataHandledEvent: QWORD;
  end;
  TUltimapEventArray=array [0..0] of TUltimapEvent;
  PUltimapEventArray=^TUltimapEventArray;


  TMapMemoryResult=record
    address: uint64;
    mdladdress: uint64;
  end;

type       //The DataEvent structure contains the address and blockid. Use this when done handling the event
  TUltimapDataEvent=packed record
    Address: Qword;
    Size: Qword;
    BlockID: Qword;
    CpuID: Qword;
    KernelAddress: QWORD;
    Mdl: QWORD;
  end;
  PUltimapDataEvent= ^TUltimapDataEvent;

  TUltimap2DataEvent=packed record
    Address:Qword;
    Size: Qword;
    Cpunr: Qword;
  end;
  PUltimap2DataEvent= ^TUltimap2DataEvent;

type
  TURange=record
    startAddress: QWORD;
    endaddress: QWORD;
    isStopRange: QWORD;
  end;
  PURange=^TPRange;
  TURangeArray=array of TURange;
  PURangeArray=^TURangeArray;

  TPRange=record
    startAddress: QWORD;
    endaddress: QWORD;
  end;
  PPRange=^TPRange;

  TPRangeArray=array [0..9999] of TPRange;
  PPRangeArray=^TPRangeArray;

  TPRangeDynArray=array of TPRange;

{$ifdef windows}
var hdevice: thandle=INVALID_HANDLE_VALUE; //handle to my the device driver
    hUltimapDevice: thandle=INVALID_HANDLE_VALUE;
    handlemap: TMap;
    handlemapMREW: TMultiReadExclusiveWriteSynchronizer;
    driverloc, ultimapdriverloc: widestring;
    iamprotected:boolean;
    SDTShadow: DWORD;
    debugport: dword;

    ThreadsProcess,ThreadListEntry:dword;

    processeventname, threadeventname: widestring;
    processevent,threadevent:thandle;

    ownprocess: thandle=0; //needed for simple kernelmemory access
    //Successfullyloaded:boolean;
    iswow64: bool;
    //usealternatedebugmethod: boolean;


    saferQueryPhysicalMemory: boolean=true;

    oldZwClose: function (Handle: THandle): NTSTATUS; stdcall;
    oldNtQueryInformationProcess: function(ProcessHandle: HANDLE; ProcessInformationClass: PROCESSINFOCLASS; ProcessInformation: PVOID; ProcessInformationLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall;
    oldNtReadVirtualMemory: function(ProcessHandle : HANDLE; BaseAddress : PVOID; Buffer : PVOID; BufferLength : ULONG; ReturnLength : PSIZE_T): NTSTATUS; stdcall;
    oldNtOpenProcess: function(Handle: PHandle; AccessMask: dword; objectattributes: pointer; clientid: PClient_ID):DWORD; stdcall;

    NextPseudoHandle: integer=integer(dword($ce000000));
    DoNotOpenProcessHandles: Boolean=false;
    ProcessWatcherOpensHandles: Boolean=true;

function CTL_CODE(DeviceType, Func, Method, Access : integer) : integer;
function IsValidHandle(hProcess:THandle):BOOL; stdcall;
function IsDBKHandle(hProcess:THandle):BOOL; stdcall;
Function {OpenProcess}OP(dwDesiredAccess:DWORD;bInheritHandle:BOOL;dwProcessId:DWORD):THANDLE; stdcall;
Function {OpenThread}OT(dwDesiredAccess:DWORD;bInheritHandle:BOOL;dwThreadId:DWORD):THANDLE; stdcall;
function {ReadProcessMemory}RPM(hProcess:THANDLE;lpBaseAddress:pointer;lpBuffer:pointer;nSize:DWORD;var NumberOfBytesRead:PtrUInt):BOOL; stdcall;
function {ReadProcessMemory64}ReadProcessMemory64(hProcess:THANDLE;lpBaseAddress:UINT64;lpBuffer:pointer;nSize:DWORD;var NumberOfBytesRead:PtrUInt):BOOL; stdcall;
function {WriteProcessMemory}WPM(hProcess:THANDLE;lpBaseAddress:pointer;lpBuffer:pointer;nSize:DWORD;var NumberOfBytesWritten:PtrUInt):BOOL; stdcall;

function {WriteProcessMemory}WriteProcessMemory64(hProcess:THANDLE;BaseAddress:qword;lpBuffer:pointer;nSize:DWORD;var NumberOfBytesWritten:PtrUInt):BOOL; stdcall;

function {VirtualQueryEx}VQE(hProcess: THandle; address: pointer; var mbi: _MEMORY_BASIC_INFORMATION; bufsize: DWORD):dword; stdcall;
Function {NtOpenProcess}NOP(Handle: PHandle; AccessMask: dword; objectattributes: pointer; clientid: PClient_ID):DWORD; stdcall;
Function {ZwClose}ZC(Handle: THandle): NTSTATUS; stdcall;

function DBK_NtQueryInformationProcess(ProcessHandle: HANDLE; ProcessInformationClass: PROCESSINFOCLASS; ProcessInformation: PVOID; ProcessInformationLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall;
function DBK_NtReadVirtualMemory(ProcessHandle : HANDLE; BaseAddress : PVOID; Buffer : PVOID; BufferLength : ULONG; ReturnLength : PSIZE_T): NTSTATUS; stdcall;

Function {NtOpenThread}NtOT(var Handle: THandle; AccessMask: dword; objectattributes: pointer; clientid: PClient_ID):DWORD; stdcall;
Function {VirtualAllocEx}VAE(hProcess: THandle; lpAddress: Pointer; dwSize, flAllocationType: DWORD; flProtect: DWORD): Pointer; stdcall;
Function CreateRemoteAPC(threadid: dword; lpStartAddress: TFNAPCProc): THandle; stdcall;


Function GetPEProcess(ProcessID: dword):UINT64; stdcall;
Function GetPEThread(Threadid: dword):UINT64; stdcall;
function GetDebugportOffset: DWORD; stdcall;
function GetThreadsProcessOffset: dword; stdcall;
function GetThreadListEntryOffset: dword; stdcall;

function ReadPhysicalMemory(hProcess:THANDLE;lpBaseAddress:pointer;lpBuffer:pointer;nSize:DWORD;var NumberOfBytesRead:PTRUINT):BOOL; stdcall;
function WritePhysicalMemory(hProcess:THANDLE;lpBaseAddress:pointer;lpBuffer:pointer;nSize:DWORD;var NumberOfBytesWritten:PTRUINT):BOOL; stdcall;
function GetPhysicalAddress(hProcess:THandle;lpBaseAddress:pointer;var Address:qword): BOOL; stdcall;
function GetMemoryRanges(var ranges: TPhysicalMemoryRanges): boolean;
function VirtualQueryExPhysical(hProcess: THandle; lpAddress: Pointer; var lpBuffer: TMemoryBasicInformation; dwLength: DWORD): DWORD; stdcall;



function GetCR4:DWORD; stdcall;
function GetCR3(hProcess:THANDLE;var CR3:system.QWORD):BOOL; stdcall;
function GetCR3FromPID(pid: system.QWORD;var CR3:system.QWORD):BOOL; stdcall;

//function SetCR3(hProcess:THANDLE;CR3: DWORD):BOOL; stdcall;
function GetCR0:DWORD; stdcall;
function GetSDT:PtrUInt; stdcall;
function GetSDTShadow:PtrUInt; stdcall;

function StartProcessWatch:BOOL;stdcall;
function WaitForProcessListData(processpointer:pointer;threadpointer:pointer;timeout:dword):dword; stdcall;
function GetProcessNameFromPEProcess(peprocess:uint64; buffer:pchar;buffersize:dword):integer; stdcall;
function GetProcessNameFromID(processid:dword; buffer:pointer;buffersize:dword):integer; stdcall;
function MakeWritable(Address,Size:dword;copyonwrite:boolean): boolean; stdcall;
function RewriteKernel32:boolean; stdcall;
function RestoreKernel32:boolean; stdcall;

function InitializeDriver(Address: ptrUint; size:dword):BOOL; stdcall;
function GetWin32KAddress(var address:ptrUint;var size:dworD):boolean;
function GetDriverVersion: dword;

function GetIDTCurrentThread:QWORD; stdcall;
function GetIDTs(idtstore: pointer; maxidts: integer):integer; stdcall;

function GetLoadedState: BOOLEAN; stdcall;

function DBKSuspendThread(ThreadID:dword):boolean; stdcall;
function DBKResumeThread(ThreadID:dword):boolean; stdcall;
function DBKSuspendProcess(ProcessID:dword):boolean; stdcall;
function DBKResumeProcess(ProcessID:dword):boolean; stdcall;

function KernelAlloc(size: dword):pointer; stdcall;
function KernelAlloc64(size: dword):uint64; stdcall;
procedure KernelFree(address: uint64); stdcall;
function MapMemory(address: ptruint; size: dword; frompid: dword=0; topid: dword=0):TMapMemoryResult;
procedure UnmapMemory(r: TMapMemoryResult);

function LockMemory(processid: DWORD; address: ptruint; size: integer): QWORD;
procedure UnlockMemory(MDLAddress: QWORD);

function GetKProcAddress(s: pwidechar):pointer; stdcall;
function GetKProcAddress64(s: pwidechar):uint64; stdcall;

function GetSDTEntry(nr: integer; address: PDWORD; paramcount: PBYTE):boolean; stdcall;
function GetSSDTEntry(nr: integer; address: PDWORD; paramcount: PBYTE):boolean; stdcall;

function UserdefinedInterruptHook(interruptnr: integer; newCS: word; newEIP: uint64; addressofjumpback: uint64):boolean; stdcall;
function ExecuteKernelCode(address: uint64; parameters: uint64): BOOL; stdcall;
function ultimap(cr3: QWORD; debugctl_value: QWORD; DS_AREA_SIZE: integer; savetofile: boolean; filename: widestring; handlercount: integer): Boolean; stdcall;
function ultimap_disable: BOOLEAN; stdcall;
function ultimap_waitForData(timeout: dword; output: PUltimapDataEvent): boolean;
function ultimap_continue(previousdataresult: PUltimapDataEvent): boolean;
procedure ultimap_flush;

procedure ultimap_pause;
procedure ultimap_resume;


procedure ultimap2(processid: dword; size: dword; outputfolder: widestring; ranges: TURangeArray; noPMI: boolean=false; logUserMode: boolean=true; logKernelMode: boolean=false);
procedure ultimap2_disable;
function  ultimap2_waitForData(timeout: dword; var output: TUltimap2DataEvent): boolean;
procedure ultimap2_continue(cpunr: integer);
procedure ultimap2_flush;
procedure ultimap2_pause;
procedure ultimap2_resume;
procedure ultimap2_lockfile(cpunr: integer);
procedure ultimap2_releasefile(cpunr: integer);

function ultimap2_getTraceSize: UINT64;
procedure ultimap2_resetTraceSize;

function dbk_enabledrm(preferedAltitude: word=0; protectedEProcess: qword=0): boolean;
function dbk_getPEB(EProcess: qword): QWORD;

{
const IOCTL_CE_ULTIMAP2_WAITFORDATA   = (IOCTL_UNKNOWN_BASE shl 16) or ($0851 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_ULTIMAP2_CONTINUE      = (IOCTL_UNKNOWN_BASE shl 16) or ($0852 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_ULTIMAP2_FLUSH         = (IOCTL_UNKNOWN_BASE shl 16) or ($0853 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_ULTIMAP2_PAUSE         = (IOCTL_UNKNOWN_BASE shl 16) or ($0854 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTL_CE_ULTIMAP2_RESUME        = (IOCTL_UNKNOWN_BASE shl 16) or ($0855 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);

}

procedure dbk_test;

procedure LaunchDBVM(cpuid: integer); stdcall;
procedure allocateMemoryForDBVM(pagecount: QWORD);


function GetGDT(limit: pword):ptruint; stdcall;

function isDriverLoaded(SigningIsTheCause: PBOOL): BOOL; stdcall;

procedure DBK32Initialize;


function readMSR(msr: dword): QWORD;
procedure writeMSR(msr: dword; value: qword);


function MarkAllPagesAsNonAccessed(hProcess: THandle):boolean;
function EnumAndGetAccessedPages(hProcess: THandle; var r: TPRangeDynArray):integer;
function KernelWritesIgnoreWriteProtection(state: boolean): boolean;


type TIsWow64Process=function (processhandle: THandle; var isWow: BOOL): BOOL; stdcall;

function DeviceIoControl(hDevice: THandle; dwIoControlCode: DWORD; lpInBuffer: Pointer; nInBufferSize: DWORD; lpOutBuffer: Pointer; nOutBufferSize: DWORD; var lpBytesReturned: DWORD; lpOverlapped: POverlapped): BOOL; stdcall;

function ReadProcessMemory64_Internal(processid:dword;lpBaseAddress:UINT64;lpBuffer:pointer;nSize:DWORD;var NumberOfBytesRead:PtrUInt):BOOL; stdcall;



var kernel32dll: thandle;
    IsWow64Process: TIsWow64Process;
    failedduetodriversigning: boolean;

{$endif}

implementation

{$ifdef windows}
uses vmxfunctions, DBK64SecondaryLoader, NewKernelHandler, frmDriverLoadedUnit, CEFuncProc, Parsers, mainunit2;

resourcestring

  rsInvalidMsrAddress = 'Invalid MSR address:';
  rsMsrsAreUnavailable = 'msrs are unavailable';
  rsCouldNotLaunchDbvm = 'Could not launch DBVM: The Intel-VT feature has been disabled in your BIOS';
  rsYouAreMissingTheDriver = 'You are missing the driver. Try reinstalling '+strCheatEngine+', and try to disable your anti-virus before doing so.';
  rsDriverError = 'Driver error';
  rsFailureToConfigureTheDriver = 'Failure to configure the driver';
  rsFailureToConfigureTheUltimapDriver = 'Failure to configure the ultimap driver';
  rsPleaseRebootAndPressF8DuringBoot = 'The driver failed to load due to signing issues. If you have secure boot enabled in your BIOS, set it to "Other OS" or disable it. Alternatively, boot with driver signing policy disabled, or sign the driver yourself';
  rsDbk32Error = 'DBK32 error';
  rsTheServiceCouldntGetOpenedUltimap = 'The ultimap service couldn''t get opened and also couldn''t get created.  (No admin rights?)';
  rsTheServiceCouldntGetOpened = 'The service couldn''t get opened and also couldn''t get created.'+' Check if you have the needed rights to create a service, or call your system admin (Who''ll probably beat you up for even trying this). Until this is fixed you won''t be able to make use of the enhancements the driver gives you';
  rsTheDriverCouldntBeOpened = 'The driver couldn''t be opened! It''s not loaded or not responding. Luckely you are running dbvm so it''s not a total waste. Do you wish to force load the driver?';
  rsTheDriverCouldntBeOpenedTryAgain = 'The driver couldn''t be opened! It''s not loaded or not responding. I recommend to reboot your system and try again';
  rsTheDriverThatIsCurrentlyLoaded = 'The driver that is currently loaded belongs to a different version of '+strCheatEngine+'. Please unload this driver or reboot.';
  rsTheDriverFailedToSuccessfullyInitialize = 'The driver failed to successfully initialize. Some functions may not completely work';
  rsAPCRules = 'APC rules';
  rsPleaseRunThe64BitVersionOfCE = 'Please run the 64-bit version of '+strCheatEngine;
  rsDBKError = 'DBK Error';

var dataloc: widestring;
    applicationPath: widestring;

type TVirtualAllocEx=function(hProcess: THandle; lpAddress: Pointer; dwSize, flAllocationType: DWORD; flProtect: DWORD): Pointer; stdcall;
var VirtualAllocEx: TVirtualAllocEx;


function DeviceIoControl(hDevice: THandle; dwIoControlCode: DWORD; lpInBuffer: Pointer; nInBufferSize: DWORD; lpOutBuffer: Pointer; nOutBufferSize: DWORD; var lpBytesReturned: DWORD; lpOverlapped: POverlapped): BOOL; stdcall;
begin
  if hdevice=$fff00fff then
  begin
    //dbvm handle
    result:=SecondaryDeviceIoControl(dwIoControlCode, lpInBuffer, nInBufferSize, lpOutBuffer, nOutBufferSize, lpBytesReturned, lpOverlapped);
  end
  else
    result:=windows.DeviceIoControl(hDevice, dwIoControlCode, lpInBuffer,nInBufferSize, lpOutBuffer, nOutBufferSize, lpBytesReturned, lpOverlapped );

end;

function isDriverLoaded(SigningIsTheCause: PBOOL): BOOL; stdcall;
begin
  result:=true;
  if hdevice=INVALID_HANDLE_VALUE then
  begin
    if SigningIsTheCause<>nil then
      SigningIsTheCause^:=failedduetodriversigning;

    result:=false;
  end;
end;

function noIsWow64(processhandle: THandle; var isWow: BOOL): BOOL; stdcall;
begin
  if @isWow<>nil then
    isWow:=false;
    
  result:=false;
end;

procedure FSC;
asm
  mov edx,esp
  sysenter
end;

function GetLoadedState: BOOLEAN; stdcall;
begin
  result:=(hdevice<>INVALID_HANDLE_VALUE);
end;

{$W+}


procedure ultimap2_disable;
var
  cc,br: dword;
begin
  if (hUltimapDevice<>0) and (hUltimapDevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_DISABLEULTIMAP2;
    deviceiocontrol(hultimapdevice,cc,nil,0,nil,0,br,nil);
  end;
end;


procedure ultimap2(processid: dword; size: dword; outputfolder: widestring; ranges: TURangeArray; noPMI: boolean=false; logUserMode: boolean=true; logKernelMode: boolean=false);
var
  inp:record
    PID: UINT32;
    BufferSize: UINT32;
    rangecount: UINT32;
    noPMI:      UINT32;
    UserMode:   UINT32;
    KernelMode: UINT32;
    range: array[0..7] of TURange;
    filename: array [0..199] of WideChar;
  end;
  cc,br: dword;
  i: integer;
begin
  OutputDebugString('ultimap2:'+outputfolder);
  zeromemory(@inp, sizeof(inp));
  inp.PID:=processid;
  inp.BufferSize:=size;


  if outputfolder<>'' then
  begin
    if DirectoryExists(outputfolder) then
    begin
      outputfolder:='\DosDevices\'+outputfolder;

      if outputfolder[length(outputfolder)]<>PathDelim then
        outputfolder:=outputfolder+PathDelim;
    end
    else
    begin
      OutputDebugString(outputfolder+' could not be found');
      outputfolder:='';
    end;
  end;

  for i:=1 to length(outputfolder) do
    inp.filename[i-1]:=outputfolder[i];

  inp.filename[length(outputfolder)]:=#0;

  inp.rangecount:=min(8,length(ranges));

  if noPMI then
    inp.noPMI:=1
  else
    inp.noPMI:=0;

  if logusermode then
    inp.UserMode:=1
  else
    inp.UserMode:=0;

  if logkernelmode then
    inp.KernelMode:=1
  else
    inp.KernelMode:=0;

  OutputDebugString(format('logUserMode=%d logKernelMode=%d', [inp.UserMode, inp.KernelMode]));

  for i:=0 to inp.rangecount-1 do
  begin
    inp.range[i]:=ranges[i];
    OutputDebugString(format('r%d : %x - %x', [i, inp.range[i].startAddress, inp.range[i].endaddress]));
  end;

  outputdebugstring(format('Calling IOCTL_CE_ULTIMAP2(%x)\n',[IOCTL_CE_ULTIMAP2]));

  cc:=IOCTL_CE_ULTIMAP2;
  deviceiocontrol(hUltimapDevice,cc,@inp,sizeof(inp),nil,0,br,nil);
end;

function  ultimap2_waitForData(timeout: dword; var output: TUltimap2DataEvent): boolean;
var cc: dword;
begin
  if (hdevice<>INVALID_HANDLE_VALUE) then
    result:=deviceiocontrol(hUltimapDevice,IOCTL_CE_ULTIMAP2_WAITFORDATA,@timeout,sizeof(timeout),@output,sizeof(TUltimap2DataEvent),cc,nil)
  else
    result:=false;
end;


procedure ultimap2_continue(cpunr: integer);
var cc: dword;
begin
  if (hdevice<>INVALID_HANDLE_VALUE) then
    deviceiocontrol(hUltimapDevice,IOCTL_CE_ULTIMAP2_CONTINUE,@cpunr,sizeof(cpunr),nil,0,cc,nil);
end;

procedure ultimap2_flush;
var
  cc,br: dword;
begin
  cc:=IOCTL_CE_ULTIMAP2_FLUSH;
  deviceiocontrol(hUltimapDevice,cc,nil,0,nil,0,br,nil);
end;


procedure ultimap2_pause;
var
  cc,br: dword;
begin
  cc:=IOCTL_CE_ULTIMAP2_PAUSE;
  deviceiocontrol(hUltimapDevice,cc,nil,0,nil,0,br,nil);
end;

procedure ultimap2_resume;
var
  cc,br: dword;
begin
  cc:=IOCTL_CE_ULTIMAP2_RESUME;
  deviceiocontrol(hUltimapDevice,cc,nil,0,nil,0,br,nil);
end;

procedure ultimap2_lockfile(cpunr: integer);
var br: dword;
begin
  if (hdevice<>INVALID_HANDLE_VALUE) then
    deviceiocontrol(hUltimapDevice,IOCTL_CE_ULTIMAP2_LOCKFILE,@cpunr,sizeof(cpunr),nil,0,br,nil);
end;

procedure ultimap2_releasefile(cpunr: integer);
var br: dword;
begin
  if (hdevice<>INVALID_HANDLE_VALUE) then
    deviceiocontrol(hUltimapDevice,IOCTL_CE_ULTIMAP2_RELEASEFILE,@cpunr,sizeof(cpunr),nil,0,br,nil);
end;

function ultimap2_getTraceSize: UINT64;
var
  br: dword;
  size: uint64;
begin
  size:=0;
  if (hdevice<>INVALID_HANDLE_VALUE) then
    deviceiocontrol(hUltimapDevice,IOCTL_CE_ULTIMAP2_GETTRACESIZE,nil,0,@size,sizeof(size),br,nil);

  result:=size;
end;

procedure ultimap2_resetTraceSize;
var br: dword;
begin
  if (hdevice<>INVALID_HANDLE_VALUE) then
    deviceiocontrol(hUltimapDevice,IOCTL_CE_ULTIMAP2_RESETTRACESIZE,nil,0,nil,0,br,nil);
end;

function dbk_enabledrm(preferedAltitude: word=0; protectedEProcess: qword=0): boolean;
var
  inp:record
    preferedAltitude: qword;
    protectedEProcess: qword;
  end;
  br: dword;
begin
  inp.preferedAltitude:=preferedAltitude;
  inp.protectedEProcess:=protectedEProcess;
  result:=deviceiocontrol(hdevice,IOCTL_CE_ENABLE_DRM,@inp,sizeof(inp),nil,0,br,nil);
end;

function dbk_getPEB(EProcess: qword): QWORD;
var
  br: dword;
  r: qword;
begin
  r:=0;
  if deviceiocontrol(hdevice,IOCTL_CE_GET_PEB,@EProcess,sizeof(EProcess),@r,sizeof(r),br,nil) then
    result:=r
  else
    result:=0;
end;

procedure dbk_test;
var cc,br: dword;
begin
  OutputDebugString('dbk_test');
  cc:=IOCTL_CE_TEST;
  deviceiocontrol(hdevice,cc,nil,0,nil,0,br,nil);
end;

function GetGDT(limit: pword):ptruint; stdcall;
var cc,br: dword;
    gdtdescriptor: packed record
                     wLimit: word;
                     vector: uint64;
                   end;
begin
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_GETGDT;
    deviceiocontrol(hdevice,cc,nil,0,@gdtdescriptor,10,br,nil);
    result:=gdtdescriptor.vector;
    outputdebugstring(pchar(format('gdtdescriptor.wlimit=%d',[gdtdescriptor.wlimit])));
    if (limit<>nil) then
      limit^:=gdtdescriptor.wlimit;
  end else result:=0;
end;

function GetIDTCurrentThread:QWORD;
var cc,br: dword;
    idtdescriptor: packed record
                     wLimit: word;
                     vector: UINT64;
                   end;
begin
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_GETIDT;
    deviceiocontrol(hdevice,cc,nil,0,@idtdescriptor,10,br,nil);

    result:=idtdescriptor.vector;
    {$ifdef cpu32}
    if not iswow64 then
      result:=result and $ffffffff;
    {$endif}
  end else result:=0;
end;


type
  TptrUintArray=array[0..9999] of ptrUint;
  PptrUintArray=^TptrUintArray;
  TDwordArray=array[0..9999] of Dword;
  PDwordArray=^TDwordArray;

  TGetIDTParams=record
    idtstore: PQwordArray;
    maxidts: integer;
    currentindex: integer;
  end;
  PGetIDTParams=^TGetIDTParams;

function internal_GetIDTs(parameters: pointer): BOOL; stdcall;
var p: PGetIDTParams;
begin
  OutputDebugString('internal_GetIDTs');
  p:=parameters;

  result:=true; //always true, even if not big enough
  if p^.currentindex>=p^.maxidts then exit;

  p^.idtstore[p^.currentindex]:=GetIDTCurrentThread;
  inc(p^.currentindex);
end;

function GetIDTs(idtstore: pointer; maxidts: integer):integer; stdcall;
var
  p: TGetIDTParams;
begin
  OutputDebugString('GetIDTs');
  ZeroMemory(idtstore, 8*maxidts);
  p.idtstore:=idtstore;
  p.maxidts:=maxidts;
  p.currentindex:=0;
  foreachcpu(internal_getidts, @p);

  result:=p.currentindex;
end;

function GetProcessNameFromPEProcess(peprocess:uint64; buffer:pchar;buffersize:dword):integer; stdcall;
var x,cc: dword;
    ar:PtrUInt;
    i:integer;
    address: uint64;
begin
  result:=0;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    if buffersize>16 then buffersize:=16;

    cc:=IOCTL_CE_GETPROCESSNAMEADDRESS;
    if deviceiocontrol(hdevice,cc,@peprocess,8,@address,8,x,nil) then
    begin
      if ReadProcessMemory64(ownprocess,address,buffer,buffersize,ar) then
      begin
        for i:=0 to buffersize-1 do
          if buffer[i]=#0 then
          begin
            result:=i-1;
            exit;
          end;
      end;
    end;
  end;



end;

function GetCR0:DWORD; stdcall;
var x,cc:dword;
res: uint64;
begin
  result:=0;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_GETCR0;
    if deviceiocontrol(hdevice,cc,nil,0,@res,8,x,nil) then
      result:=res;
  end;
end;

function GetCR4:DWORD; stdcall;
var x,cc:dword;
  res: uint64;
begin
  result:=0;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_GETCR4;
    if deviceiocontrol(hdevice,cc,nil,0,@res,sizeof(res),x,nil) then
      result:=res;
  end
  else
  if isRunningDBVM then
    result:=dbvm_getRealCR4;
end;


function GetDriverVersion:dword;
var x,res,cc:dword;
begin
  result:=0;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_GETVERSION;

    if deviceiocontrol(hdevice,cc,nil,0,@res,4,x,nil) then
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


function GetDebugportOffset: DWORD; stdcall;
begin
  result:=debugport;
end;

function GetSDTShadow:ptruint; stdcall;
begin
  result:=SDTShadow;
end;

function GetSDT:ptruint; stdcall;
var x,cc:dword;
    res: uint64;
begin
  result:=0;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_GETSDT;
    if deviceiocontrol(hdevice,cc,nil,0,@res,8,x,nil) then
      result:=res;
  end;
end;

function GetCR3(hProcess:THANDLE;var CR3:system.QWORD):BOOL; stdcall;
var cc:dword;
    x,y:dword;
    _cr3: uint64;
    l: THandleListEntry;
begin
  result:=false;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    handlemapmrew.Beginread;
    try
      if handlemap.GetData(hProcess,l) then
      begin
        cc:=IOCTL_CE_GETCR3;
        x:=l.processid;
        result:=deviceiocontrol(hdevice,cc,@x,4,@_cr3,8,y,nil);

       // outputdebugstring(pchar('GetCR3: return '+inttohex(_cr3,16)));
        if result then CR3:=_cr3 else cr3:=$11223344;
      end;

    finally
      handlemapmrew.Endread;
    end;
  end;

  if (not result) and (isRunningDBVM) then
  begin
    _cr3:=dbvm_findCR3(hProcess);

    if _cr3<>0 then
    begin
      CR3:=_cr3;
      result:=true;
    end;

  end;
end;

function GetCR3FromPID(pid: system.QWORD;var CR3:system.QWORD):BOOL; stdcall;
var cc:dword;
    x,y:dword;
    _cr3: uint64;
    __cr3: uint64;

    z: ptruint;

    eprocess: uint64;
begin
  cr3:=0;
  result:=false;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_GETCR3;
    x:=pid;
    result:=deviceiocontrol(hdevice,cc,@x,4,@_cr3,8,y,nil);

    //outputdebugstring(pchar('GetCR3: return '+inttohex(_cr3,16)));

    if (_cr3 and $fff)>0 then
    begin
    //  RPM();
      //windows 10 usermode/kernelmode seperation
      eprocess:=GetPEProcess(pid);
      if ReadProcessMemory64_Internal(pid, eprocess+$278, @__cr3, 8,z) then
      begin
        if (__cr3 and qword($fffffffffffff000))<>0 then
          _cr3:=__cr3;
        //else it has no special usermode page (administrator level app)
      end;
      //readProcessMemory(processhandle,  GetPEProcess(pid);
    end;
    if result then CR3:=_cr3 else cr3:=0;
  end;


end;


{function SetCR3(hProcess:THANDLE;CR3: DWORD):BOOL; stdcall;
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
end;  }




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


function internal_UserdefinedInterruptHook(parameters: pointer): BOOL; stdcall;
type
  TParams=record
    interruptNumber: uint64;
    newcs: uint64;
    neweip: uint64;
    addressofjumpback: uint64;
  end;

  PParams=^TParams;

var
  cc: dword;
  pinput: PParams;
  x: dword;
begin
  outputdebugstring('internal_UserdefinedInterruptHook');
  result:=false;
  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_USERDEFINEDINTERRUPTHOOK;
    pinput:=parameters;
    result:=deviceiocontrol(hdevice,cc,pinput,sizeof(TParams),nil,0,x,nil);
  end;
end;

function UserdefinedInterruptHook(interruptnr: integer; newCS: word; newEIP: uint64; addressofjumpback: uint64):boolean; stdcall;
var params: record
  interruptNumber: uint64;
  newcs: uint64;
  neweip: uint64;
  addressofjumpback: uint64;
end;
begin
  params.interruptNumber:=interruptnr;
  params.newcs:=newcs;
  params.neweip:=neweip;
  params.addressofjumpback:=addressofjumpback;
  result:=foreachcpu(internal_UserdefinedInterruptHook, @params);
end;



function GetPhysicalAddress(hProcess:THandle;lpBaseAddress:pointer;var Address:qword): BOOL; stdcall;
type TInputstruct=record
  ProcessID: UINT64;
  BaseAddress: UINT64;
end;
var cc: dword;
    input: TInputStruct;
    physicaladdress: int64 absolute input;
    x: dword;
    l: THandleListEntry;

    b: byte;

    CR3: qword;
    pa: qword;
begin
  result:=false;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_GETPHYSICALADDRESS;

    handlemapmrew.Beginread;
    try
      if handlemap.GetData(hProcess,l) then
      begin
        input.ProcessID:=l.processid;
        input.BaseAddress:=ptrUint(lpBaseAddresS);
//        outputdebugstring(pchar(format('ProcessID(%p)=%x Baseaddress(%p)=%x',[@input.ProcessID, input.processid, @input.BaseAddress, input.baseaddress])));

        result:=deviceiocontrol(hdevice,cc,@input,sizeof(TInputstruct),@physicaladdress,8,x,nil);
        if result then address:=physicaladdress else address:=0;
      end;

    finally
      handlemapmrew.Endread;
    end;
  end;

  if (not result) and (isRunningDBVM) then
  begin
    cr3:=dbvm_findCR3(hProcess);

    if cr3<>0 then
    begin
      result:=VirtualToPhysicalCR3(cr3,qword(lpBaseAddress), pa);
      if result then
        address:=pa;
    end;

  end;
end;


function GetMemoryRanges(var ranges: TPhysicalMemoryRanges): boolean;

var cc: dword;
    x: ptruint;
    br: dword;

    r: packed record
      address: uint64;
      size: uint64;
    end;

    buf: PPhysicalMemoryRangesArray;
    i,j,k: integer;
    entrycount: integer;

    inserted: boolean;
begin
  setlength(ranges, 0);
  cc:=IOCTL_CE_GETMEMORYRANGES;
  result:=false;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    result:=deviceiocontrol(hdevice,cc,nil,0,@r,sizeof(r),br,nil);
    if result then
    begin
      getmem(buf, r.size);
      if DBK32functions.ReadProcessMemory64(ownprocess, r.address, buf, r.size, x) then
      begin
        entrycount:=r.size div sizeof(TPhysicalMemoryRange);
        for i:=0 to entrycount-1 do
        begin
          if buf[i].size<>0 then
          begin
            //add it.  I'm not 100% sure that this function returns the list sorted. So sort it to be sure
            inserted:=false;

            setlength(ranges, length(ranges)+1);

            for j:=0 to length(ranges)-2 do
            begin
              if buf[i].base<ranges[j].base then //insert it before this
              begin
                for k:=length(ranges)-2 downto j do
                  ranges[k+1]:=ranges[k];

                ranges[j]:=buf[i];

                inserted:=true;
                break;
              end;
            end;

            if not inserted then //add to the end
              ranges[length(ranges)-1]:=buf[i];

          end;

        end;


      end;

      freememandnil(buf);
    end;
  end;
end;

function VirtualQueryExPhysical(hProcess: THandle; lpAddress: Pointer; var lpBuffer: TMemoryBasicInformation; dwLength: DWORD): DWORD; stdcall;
var
  r: TPhysicalMemoryRanges;
  i: integer;

  maxaddress: uint64;
  buf:_MEMORYSTATUS;
  passed: boolean;
begin
  setlength(r,0);
  result:=0;

  if not saferQueryPhysicalMemory then
  begin
    maxaddress:=0;

    //just go from 0 to the last one
    if GetMemoryRanges(r) then
    begin
      if length(r)>0 then
        maxaddress:=r[length(r)-1].base+r[length(r)-1].size;
    end;

    if maxaddress=0 then //fallback
    begin
      GlobalMemoryStatus(buf);
      maxaddress:=buf.dwTotalPhys;
    end;

    if maxaddress>0 then
    begin
      lpBuffer.BaseAddress:=pointer((ptrUint(lpAddress) div $1000)*$1000);
      lpbuffer.AllocationBase:=lpbuffer.BaseAddress;
      lpbuffer.AllocationProtect:=PAGE_EXECUTE_READWRITE;
      lpbuffer.RegionSize:=maxaddress-ptrUint(lpBuffer.BaseAddress);
      lpbuffer.RegionSize:=lpbuffer.RegionSize+($1000-lpbuffer.RegionSize mod $1000);

      lpbuffer.State:=mem_commit;
      lpbuffer.Protect:=PAGE_EXECUTE_READWRITE;
      lpbuffer._Type:=MEM_PRIVATE;

      if (ptrUint(lpAddress)<buf.dwTotalPhys) then //smaller than the total amount of memory
        result:=dwlength;
    end
    else
      result:=0;
  end
  else
  begin
    //safer memory access
    if GetMemoryRanges(r) then
    begin
      lpBuffer.BaseAddress:=pointer((ptrUint(lpAddress) div $1000)*$1000);

      passed:=false;
      for i:=0 to length(r)-1 do
      begin
        if (ptruint(lpAddress)>=r[i].base) then
        begin
          if InRangeX(ptruint(lpAddress), r[i].base, r[i].base+r[i].size-1) then
          begin
            //found the range
            lpbuffer.AllocationBase:=pointer(r[i].base);
            lpbuffer.AllocationProtect:=PAGE_EXECUTE_READWRITE;
            lpbuffer.RegionSize:=r[i].base+r[i].size-ptrUint(lpBuffer.BaseAddress);


            lpbuffer.State:=mem_commit;
            lpbuffer.Protect:=PAGE_EXECUTE_READWRITE;
            lpbuffer._Type:=MEM_PRIVATE;

            result:=dwlength;
            exit;
          end;
        end
        else
          passed:=true;

        if passed then //the current address is smaller than r[i].base
        begin
          //not in the list (mark as not commited)


          if i>0 then
            lpbuffer.AllocationBase:=pointer(r[i-1].base+r[i-1].size)
          else
            lpbuffer.AllocationBase:=nil;

          lpbuffer.AllocationProtect:=PAGE_NOACCESS;

          lpbuffer.RegionSize:=r[i].base-ptrUint(lpBuffer.BaseAddress);


          lpbuffer.State:=MEM_FREE;
          lpbuffer.Protect:=PAGE_NOACCESS;
          lpbuffer._Type:=0;


          result:=dwlength;
          exit;
        end;
      end;
    end;

  end;
end;


function WritePhysicalMemory(hProcess:THANDLE;lpBaseAddress:pointer;lpBuffer:pointer;nSize:DWORD;var NumberOfBytesWritten:PTRUINT):BOOL; stdcall;
type TInputstruct=record
  startaddress: uint64;
  bytestowrite: uint64;
end;
var ao: array [0..511] of byte;
    input: TInputstruct absolute ao[0];
    cc:dword;
    
    ok: boolean;
    br: dword;

    mempointer: ptrUint;
    bufpointer: ptrUint;
    bufpointer2: pointer;
    towrite: dword;
begin
  if vmx_loaded and (dbvm_version>=$ce00000a) then
  begin
    NumberOfBytesWritten:=dbvm_write_physical_memory(qword(lpBaseAddress), lpBuffer, nSize);
    exit(NumberOfBytesWritten=nSize);
  end;


  result:=false;
  NumberOfByteswritten:=0;
  //find the hprocess in the handlelist, if it isn't use the normal method (I could of course use NtQueryProcessInformation but it's undocumented and I'm too lazy to dig it up

  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_WRITEPHYSICALMEMORY;
    mempointer:=ptrUint(lpBaseAddress);
    bufpointer:=ptrUint(lpbuffer);

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


function ReadPhysicalMemory(hProcess:THANDLE;lpBaseAddress:pointer;lpBuffer:pointer;nSize:DWORD;var NumberOfBytesRead:PTRUINT):BOOL; stdcall;
type TInputstruct=packed record
  startaddress: qword;
  bytestoread: qword;
end;
var ao: array [0..600] of byte;
    input: TInputstruct absolute ao[0];
    br: dword;
    cc:dword;
    ok:boolean;
    toread:dword;
    toread2: dword;
    mempointer:ptrUint;
    bufpointer:ptrUint;
begin
  //processhandle is just there for compatibility in case I want to quickly wrap it over read/writeprocessmemory
  if vmx_loaded and (dbvm_version>=$ce00000a) then
  begin
    numberofbytesread:=dbvm_read_physical_memory(qword(lpBaseAddress), lpBuffer, nSize);
    exit(numberofbytesread=nSize);
  end;

  //OutputDebugString('Using normal dbk method');

  result:=false;
  numberofbytesread:=0;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_READPHYSICALMEMORY;
    toread:=nSize;
    mempointer:=ptrUint(lpBaseAddress);
    bufpointer:=ptrUint(lpBuffer);
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

Function GetPEThread(Threadid: dword):UINT64; stdcall;
var cc:dword;
    x: dword;
    pethread: uint64;
begin
  result:=0;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_GETPETHREAD;
    pethread:=threadid;
    if deviceiocontrol(hdevice,cc,@threadid,4,@pethread,8,x,nil) then
      result:=pethread;
  end;
end;


Function GetPEProcess(ProcessID: dword):UINT64; stdcall;
var cc:dword;
    x: dword;
    peprocess: uint64;
begin
  result:=0;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_GETPEPROCESS;
    peprocess:=processid;
    if deviceiocontrol(hdevice,cc,@processid,4,@peprocess,8,x,nil) then result:=peprocess else result:=0;
  end;
end;

function IsDBKHandle(hProcess:THandle):BOOL; stdcall;
var l: THandleListEntry;
begin
  result:=false;
  handlemapmrew.Beginread;
  try
    result:=handlemap.HasId(hProcess);
  finally
    handlemapmrew.Endread;
  end;
end;

function IsValidHandle(hProcess:THandle):BOOL; stdcall;
var l: THandleListEntry;
begin
  //outputdebugstring('IsValidHandle');
  result:=true; //not in the list is ok
  handlemapmrew.Beginread;
  try
    if handlemap.GetData(hProcess,l) then
    begin
      result:=l.validhandle;
      exit;
    end;
  finally
    handlemapmrew.Endread;
  end;
end;

function RPM(hProcess:THANDLE;lpBaseAddress:pointer;lpBuffer:pointer;nSize:DWORD;var NumberOfBytesRead:PtrUInt):BOOL; stdcall;
begin
  result:=ReadProcessMemory64(hProcess, uint64(ptrUint(lpBaseAddress)), lpBuffer, nSize, NumberOfBytesRead);
end;

function ReadProcessMemory64_Internal(processid:dword;lpBaseAddress:UINT64;lpBuffer:pointer;nSize:DWORD;var NumberOfBytesRead:PtrUInt):BOOL; stdcall;
type TInputstruct=packed record
  processid: uint64;
  startaddress: uint64;
  bytestoread: word;
end;
var //ao: array [0..600] of byte; //give it some space
    input: TInputstruct;
    cc:dword;

    ok: boolean;
    br: dword;

    mempointer: qword;
    bufpointer: ptrUint;
    toread: dword;
begin
  result:=false;
  numberofbytesread:=0;
  //find the hprocess in the handlelist, if it isn't use the normal method (I could of course use NtQueryProcessInformation but it's undocumented and I'm too lazy to dig it up

  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_READMEMORY;
    mempointer:=lpBaseAddress;
    bufpointer:=ptrUint(lpbuffer);

    ok:=true;
    while ok do
    begin
      input.processid:=processid;
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
  end;
end;

function ReadProcessMemory64(hProcess:THANDLE;lpBaseAddress:UINT64;lpBuffer:pointer;nSize:DWORD;var NumberOfBytesRead:PtrUInt):BOOL; stdcall;
var
  l: THandleListEntry;
  validhandle: boolean;
begin
  result:=false;
  numberofbytesread:=0;
  //find the hprocess in the handlelist, if it isn't use the normal method (I could of course use NtQueryProcessInformation but it's undocumented and I'm too lazy to dig it up

  if handlemapmrew=nil then exit(windows.ReadProcessMemory(hProcess,pointer(ptrUint(lpBaseAddress)),lpBuffer,nSize,NumberOfBytesRead));

  handlemapmrew.Beginread;
  validhandle:=handlemap.GetData(hProcess,l);
  handlemapmrew.Endread;

  if validhandle then
  begin
    if (hdevice<>INVALID_HANDLE_VALUE) then
      exit(ReadProcessMemory64_Internal(l.processid,lpBaseAddress, lpBuffer, nSize,NumberOfBytesRead));

    if not l.validhandle then exit; //else use the normal method...
  end;

  //not found, or driver not loaded and a valid handle
  result:=windows.ReadProcessMemory(hProcess,pointer(ptrUint(lpBaseAddress)),lpBuffer,nSize,NumberOfBytesRead);
end;

function WPM(hProcess:THANDLE;lpBaseAddress:pointer;lpBuffer:pointer;nSize:DWORD;var NumberOfBytesWritten:PtrUInt):BOOL; stdcall;
begin
  result:=WriteProcessMemory64(hprocess, uint64(ptrUint(lpBaseAddress)), lpbuffer, nsize, NumberofbytesWritten);
end;

function WriteProcessMemory64(hProcess:THANDLE;BaseAddress:qword;lpBuffer:pointer;nSize:DWORD;var NumberOfBytesWritten:PtrUInt):BOOL; stdcall;
type TInputstruct=packed record
  processid: uint64;
  startaddress: uint64;
  bytestowrite: word;
end;
var ao: array [0..511] of byte;
    input: TInputstruct absolute ao[0];
    cc:dword;

    ok: boolean;
    br: dword;

    mempointer: qword;
    bufpointer: ptrUint;
    bufpointer2: pointer;
    towrite: dword;

    l: THandleListEntry;
    validhandle: boolean;
begin
  result:=false;
  NumberOfByteswritten:=0;
  //find the hprocess in the handlelist, if it isn't use the normal method (I could of course use NtQueryProcessInformation but it's undocumented and I'm too lazy to dig it up

  handlemapmrew.Beginread;
  validhandle:=handlemap.GetData(hProcess,l);
  handlemapmrew.Endread;

  if validhandle then
  begin
    if hdevice<>INVALID_HANDLE_VALUE then
    begin
      cc:=IOCTL_CE_WRITEMEMORY;
      mempointer:=BaseAddress;
      bufpointer:=ptrUint(lpbuffer);

      ok:=true;
      while ok do
      begin
        zeromemory(@ao[0],512);

        input.processid:=l.processid;
        if nSize-NumberOfByteswritten>=(512-sizeof(TInputstruct)) then
          towrite:=(512-sizeof(TInputstruct))
        else
          towrite:=nSize-NumberOfByteswritten;

        input.bytestowrite:=towrite;
        input.startaddress:=mempointer;

        bufpointer2:=pointer(bufpointer);
        copymemory(@ao[sizeof(tinputstruct)],bufpointer2,towrite);

//          OutputDebugString(pchar('sizeof(TInputstruct)='+inttostr(sizeof(TInputstruct))));
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
    end else if not l.validhandle then exit;
  end;

  //still here
  if (BaseAddress>=qword($8000000000000000)) and (vmx_enabled and (dbvm_version>=$ce000005)) then //if dbvm is running and it's a kernel accesses use dbvm
    result:=dbvm_copyMemory(pointer(BaseAddress), lpBuffer, nSize)
  else
    result:=windows.writeProcessMemory(hProcess,pointer(ptrUint(BaseAddress)),lpBuffer,nSize,NumberOfByteswritten);
end;

function DBK_NtReadVirtualMemory(ProcessHandle : HANDLE; BaseAddress : PVOID; Buffer : PVOID; BufferLength : ULONG; ReturnLength : PSIZE_T): NTSTATUS; stdcall;
var
  l: THandleListEntry;
  validhandle: boolean;
  br: ptruint;
begin
  //outputdebugstring('----DBK_NtReadVirtualMemory----');    (don't do this, inf loop)
  handlemapMREW.Beginread;
  validhandle:=handlemap.GetData(ProcessHandle, l);
  handlemapMREW.Endread;

  if validhandle then
  begin
    if hdevice<>INVALID_HANDLE_VALUE then
    begin
      //read/write using kernelmode rpm
      if ReturnLength<>nil then
        br:=returnlength^;

      try
        if ReadProcessMemory64_Internal(l.processid, qword(baseaddress), buffer,bufferlength, br) then
        begin
          result:=0;
          ReturnLength^:=br;
        end
        else
          result:=STATUS_ACCESS_DENIED;
      except
        result:=NTSTATUS(STATUS_ACCESS_VIOLATION);
      end;

      if result=0 then exit;
    end;
  end;

  result:=oldNtReadVirtualMemory(processhandle, BaseAddress, Buffer, BufferLength, ReturnLength);
end;

function DBK_NtQueryInformationProcess(ProcessHandle: HANDLE; ProcessInformationClass: PROCESSINFOCLASS; ProcessInformation: PVOID; ProcessInformationLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall;
type
  toutput=record
    result: QWORD;
    returnlength: QWORD;
    data: array [0..2000] of byte;
  end;
  poutput=^toutput;

var
  l: THandleListEntry;
  validhandle: boolean;
  cc: dword;

  inp: record
    processid: qword;
    ProcessInformationAddress: qword;
    ProcessInformationClass: qword;
    ProcessInformationLength: qword;
  end;

  outp: poutput;

  br: dword;
begin
  outp:=nil;

  if (handlemapMREW=nil) or (handlemap=nil) then
    exit(oldNtQueryInformationProcess(ProcessHandle, ProcessInformationClass, ProcessInformation, ProcessInformationLength, ReturnLength));

  //if (ProcessHandle=-1) or (ProcessInformationClass<>ProcessBasicInformation) then
//    exit(oldNtQueryInformationProcess(ProcessHandle, ProcessInformationClass, ProcessInformation, ProcessInformationLength, ReturnLength));

  handlemapMREW.Beginread;
  validhandle:=handlemap.GetData(ProcessHandle, l);
  handlemapMREW.Endread;

  if validhandle then
  begin
//    if not l.validhandle then
    begin

      outputdebugstring('Using kernelmode NtQueryInformationProcess');

      //try kernelmode
      if hdevice<>INVALID_HANDLE_VALUE then
      begin
        cc:=IOCTL_CE_QUERYINFORMATIONPROCESS;
        inp.processid:=l.processid;
        inp.ProcessInformationAddress:=qword(ProcessInformation);
        inp.ProcessInformationClass:=qword(ProcessInformationClass);
        inp.ProcessInformationLength:=qword(ProcessInformationLength);

        if ProcessInformationLength>65535 then
        begin
          //too big, possibly some bad code somewhere
          OutputDebugString('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Too big!!!!!!!!!!!!!!!!!!!!!!!!!');
          result:=DBK_NtQueryInformationProcess(ProcessHandle, ProcessInformationClass, ProcessInformation, 65535,ReturnLength);
          exit;
        end;

        //still here, so not too big

        OutputDebugString('DBK_NtQueryInformationProcess('+inttostr(integer(ProcessInformationClass))+')');

        getmem(outp, sizeof(qword)*2+ProcessInformationLength);

        try

          if deviceiocontrol(hdevice,cc,@inp,sizeof(inp),outp,sizeof(qword)*2+ProcessInformationLength,br,nil) then
          begin
            OutputDebugString('deviceiocontrol=true');
            OutputDebugString(format('outp.result=%x',[outp.result]));
            OutputDebugString(format('outp.returnlength=%x',[outp.returnlength]));

            result:=outp.result;
            if ReturnLength<>nil then
              ReturnLength^:=outp.returnlength;


            OutPutDebugString('Before Copy');

            if (ProcessInformation<>nil) and (DWORD(result)<$80000000) then
            try
              copymemory(ProcessInformation, @(outp^.data[0]),outp.returnlength);
            except
              result:=NTSTATUS(STATUS_ACCESS_VIOLATION);
            end;
            OutPutDebugString('After Copy');
          end
          else
          begin
            OutputDebugString('deviceiocontrol=false '+inttohex(getlasterror,8));
            if l.validhandle then
            begin
              OutputDebugString('Valid handle');
              result:=oldNtQueryInformationProcess(ProcessHandle, ProcessInformationClass, ProcessInformation, ProcessInformationLength, ReturnLength)
            end
            else
              result:=NTSTATUS(STATUS_ACCESS_VIOLATION);
          end;

        finally
          freememandnil(outp);
        end;

        exit;
      end;
    end;
  end;

//  OutputDebugString('Unknown handle. Using original code');
  result:=oldNtQueryInformationProcess(ProcessHandle, ProcessInformationClass, ProcessInformation, ProcessInformationLength, ReturnLength);
end;

function {OpenThread}OT(dwDesiredAccess:DWORD;bInheritHandle:BOOL;dwThreadId:DWORD):THANDLE; stdcall;
var
  threadhandle: uint64;
  cc,x: dword;
begin
  result:=0;
  if dwThreadId=0 then exit;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_OPENTHREAD;
    threadhandle:=dwThreadId;
    if deviceiocontrol(hdevice,cc,@dwThreadID,4,@threadhandle,8,x,nil) then
      result:=threadhandle
    else
      result:=0;
  end;
end;


function {OpenProcess}OP(dwDesiredAccess:DWORD;bInheritHandle:BOOL;dwProcessId:DWORD):THANDLE; stdcall;
var output: record
      Processhandle: uint64;
      Special: byte;
    end;

    cc,x: dword;
    l: thandlelistEntry;
begin
  result:=0;
  if dwProcessId=0 then
    exit;

  if (not DoNotOpenProcessHandles) then
  begin
    if hdevice<>INVALID_HANDLE_VALUE then
    begin
      cc:=IOCTL_CE_OPENPROCESS;

     // OutputDebugString(inttostr(dwProcessid)+' OpenProcess kernelmode');
      if deviceiocontrol(hdevice,cc,@dwProcessId,4,@output,sizeof(output),x,nil) then
      begin
        result:=output.Processhandle;

        if handlemapMREW.Beginwrite then
        try
          if handlemap.HasId(result) then
            handlemap.Delete(result);

          l.processid:=dwProcessID;
          l.specialHandle:=output.Special<>0;
          l.validhandle:=true;
          handlemap.Add(result,l);
        finally
          handlemapMREW.Endwrite;
        end;

       { z:=NtQueryObject(processhandle, ObjectBasicInformation, @pbi, sizeof(pbi),@x);
        OutputDebugString(inttostr(dwProcessid)+' NtQueryObject='+inttohex(z,8));

        if z<>0 then
          result:=0
        else
        if pbi.GrantedAccess and (PROCESS_VM_READ or PROCESS_VM_WRITE) <>(PROCESS_VM_READ or PROCESS_VM_WRITE) then
        begin
          result:=0;
          closehandle(processhandle);
          //OutputDebugString(inttostr(dwProcessid)+' failed access');
        end;  }

        //OutputDebugString(inttostr(dwProcessid)+' OpenProcess GrantedAccess='+inttohex(pbi.GrantedAccess,8));
      end
      else
      begin
        OutputDebugString(inttostr(dwProcessid)+' deviceiocontrol returned false');
        result:=0;
      end;
    end else result:=windows.OpenProcess(dwDesiredAccess,bInheritHandle,dwProcessID);
  end;
{$ifdef badopen}
  result:=0;
{$endif}

  if result=0 then //you can still access memory using the low level stuff, just not normal stuff
  begin
    //openprocess isn't working

    result:=InterLockedIncrement(NextPseudoHandle);
    if handlemapmrew.Beginwrite then
    try
      if handlemap.HasId(result) then
        handlemap.Delete(result);

      l.processid:=dwProcessID;
      l.specialHandle:=false;
      l.validhandle:=false;
      handlemap.Add(result,l);
    finally
      handlemapmrew.Endwrite;
    end;
  end;
end;

Function {NtOpenThread}NtOT(var Handle: THandle; AccessMask: dword; objectattributes: pointer; clientid: PClient_ID):DWORD; stdcall;
begin
  handle:=OT(STANDARD_RIGHTS_REQUIRED or windows.synchronize or $3ff,true,clientid.processid);
  if handle<>0 then result:=0 else result:=$c000000e;
end;

Function {NtOpenProcess}NOP(Handle: PHandle; AccessMask: dword; objectattributes: pointer; clientid: PClient_ID):DWORD; stdcall;
var h: thandle;
begin
  //comment this out to fix the c0000008 exception while debugging (do call oldNtOpenProcess)

  //OutputDebugString('NtOpenProcess hook');
  if ((hdevice<>INVALID_HANDLE_VALUE) and (clientid<>nil)) and (clientid^.processid<>GetCurrentProcessId) then
  begin
    h:=OP(ifthen<dword>(GetSystemType<=6,$1f0fff, process_all_access),true,clientid^.processid);
    if h<>0 then
    begin
      result:=0;
      if handle<>nil then
      begin
        try
          handle^:=h;
        except
          result:=STATUS_ACCESS_VIOLATION;
        end;
      end;
    end
    else result:=$C000000E;

  end
  else
    result:=oldNtOpenProcess(Handle, AccessMask, objectattributes, clientid);
end;

Function {ZwClose}ZC(Handle: THandle): NTSTATUS; stdcall;
var
  hl: PHandleListEntry;
  allow: boolean;
begin
  //check if the handle is a kernelmode opened one, and if so, don't
  allow:=true;
  if handlemapMREW<>nil then
  begin
    handlemapMREW.Beginread;
    if handlemap<>nil then
    begin
      hl:=handlemap.GetDataPtr(handle);
      if hl<>nil then
        allow:=not hl^.specialHandle;
    end;
    handlemapMREW.Endread;
  end;

  if not allow then exit(0);

  //still here
  result:=oldZwClose(Handle);
end;

function MarkAllPagesAsNonAccessed(hProcess: THandle):boolean;
var
  input: record
    ProcessID: QWORD;
  end;
  br,cc: dword;
  l: THandleListEntry;
  validhandle: boolean;
begin
  //OutputDebugString('MarkAllPagesAsNonAccessed');
  result:=false;

  handlemapMREW.Beginread;
  validhandle:=handlemap.GetData(hProcess, l);
  handlemapMREW.Endread;

  if validhandle then
  begin
    if hdevice<>INVALID_HANDLE_VALUE then
    begin
      //OutputDebugString('going to call IOCTL_CE_STARTACCESMONITOR');

      input.ProcessID:=l.processid;
      cc:=IOCTL_CE_STARTACCESMONITOR;
      if deviceiocontrol(hdevice,cc,@input,sizeof(input),nil,0,br,nil) then
        result:=true;

      exit;
    end;
  end;
end;

function EnumAccessedPages(hProcess: THandle):integer;
var
  input: record
  ProcessID: QWORD;
  end;
  br,cc: dword;
  sizeneeded: integer;

  l: THandleListEntry;
  validhandle: boolean;
begin
  //OutputDebugString('EnumAccessedPages');
  result:=0;
  handlemapMREW.Beginread;
  validhandle:=handlemap.GetData(hProcess, l);
  handlemapMREW.Endread;

  if validhandle then
  begin
    if hdevice<>INVALID_HANDLE_VALUE then
    begin
     // OutputDebugString('going to call IOCTL_CE_ENUMACCESSEDMEMORY');

      input.ProcessID:=l.processid;
      cc:=IOCTL_CE_ENUMACCESSEDMEMORY;
      if deviceiocontrol(hdevice,cc,@input,sizeof(input),@sizeneeded,sizeof(sizeneeded),br,nil) then
      begin
       // OutputDebugString('sizeneeded='+inttostr(sizeneeded));
        result:=sizeneeded;
      end
      else
      begin
        result:=0;
        //outputdebugstring('EnumAccessedPages:fail');
      end;

      exit;
    end;
  end;
end;

function IgnoredVirtualProtectEx(hProcess: THandle; lpAddress: Pointer; dwSize, flNewProtect: DWORD; var OldProtect: DWORD): BOOL; stdcall;
begin
  result:=true;
end;

function KernelWritesIgnoreWriteProtection(state: boolean): boolean;
var
  br,cc: dword;
  _state: byte;
begin
  result:=false;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTL_CE_WRITESIGNOREWP;

    if state then
      _state:=1
    else
      _state:=0;

    result:=deviceiocontrol(hdevice,cc,@_state,1,nil,0,br,nil);

    if result and (_state=1) then
    begin
      NewKernelHandler.VirtualProtectEx:=IgnoredVirtualProtectEx;
    end
    else
      NewKernelHandler.VirtualProtectEx:=GetProcAddress(WindowsKernel,'VirtualProtectEx');
  end;
end;



function EnumAndGetAccessedPages(hProcess: THandle; var r: TPRangeDynArray):integer;
var
  br,cc: dword;
  sizeneeded: integer;
  ranges: PPRangeArray;
  i: integer;
begin
  result:=0;

  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    sizeneeded:=EnumAccessedPages(hProcess);

    getmem(ranges, sizeneeded);

  //  OutputDebugString('going to call IOCTL_CE_GETACCESSEDMEMORYLIST');

    cc:=IOCTL_CE_GETACCESSEDMEMORYLIST;
    if deviceiocontrol(hdevice,cc,@sizeneeded,sizeof(sizeneeded),ranges,sizeneeded,br,nil) then
    begin
     // outputdebugstring('after IOCTL_CE_GETACCESSEDMEMORYLIST');
      result:=sizeneeded div sizeof(TPRange);

     // outputdebugstring('result='+inttostr(result));
      setlength(r, result);
      for i:=0 to result-1 do
        r[result-1-i]:=ranges[i];
    end
    else
    begin
      result:=0;
     // outputdebugstring('IOCTL_CE_GETACCESSEDMEMORYLIST failed');
    end;

    freememandnil(ranges);
  end;



end;

function {VirtualQueryEx}VQE(hProcess: THandle; address: pointer; var mbi: _MEMORY_BASIC_INFORMATION; bufsize: DWORD):dword; stdcall;
var
  input: record
    ProcessID: QWORD;
    StartAddress: QWORD;
  end;

  output: record
  	length : QWORD ;
  	protection : DWORD ;
  end;

  br,cc: dword;
  l: THandleListEntry;
  validhandle: boolean;
begin
  result:=0;

  handlemapMREW.Beginread;
  validhandle:=handlemap.GetData(hProcess, l);
  handlemapMREW.Endread;


  if validhandle then
  begin
    if hdevice<>INVALID_HANDLE_VALUE then
    begin
      input.ProcessID:=l.processid;
      input.StartAddress:=ptrUint(address);

      cc:=IOCTL_CE_QUERY_VIRTUAL_MEMORY;
      if deviceiocontrol(hdevice,cc,@input,sizeof(input),@output,sizeof(output),br,nil) then
      begin
        mbi.BaseAddress:=pointer((ptrUint(address) div $1000) *$1000);
        mbi.AllocationBase:=mbi.BaseAddress;
        mbi.AllocationProtect:=output.protection;
        mbi.RegionSize:=output.length;
        if output.protection=PAGE_NOACCESS then
          mbi.state:=MEM_FREE
        else
          mbi.State:=MEM_COMMIT;

        mbi.Protect:=output.protection;
        mbi._Type:=MEM_PRIVATE;

        result:=sizeof(mbi);
      end;

      exit; //we're done here
    end;
  end;



  result:=windows.VirtualQueryEx(hProcess,address,mbi,bufsize);
end;

Function {VirtualAllocEx}VAE(hProcess: THandle; lpAddress: Pointer; dwSize, flAllocationType: DWORD; flProtect: DWORD): Pointer; stdcall;
var br,cc: dword;
    x: record
      processid: uint64;
      baseaddress: uint64;
      size: uint64;
      AllocationType: uint64;
      Protect: uint64;
    end;
    r: uint64;

    l: THandleListEntry;
    validhandle: boolean;
begin
  OutputDebugString('Kernelmode VirtualAllocEx: lpAddress='+inttohex(ptruint(lpAddress),1));
  result:=nil;


  handlemapMREW.Beginread;
  validhandle:=handlemap.GetData(hProcess, l);
  handlemapMREW.Endread;

  if validhandle then
  begin
    if hdevice<>INVALID_HANDLE_VALUE then
    begin
      x.processid:=l.processid;
      x.baseaddress:=ptrUint(lpAddress);
      x.size:=dwsize;
      x.AllocationType:=flAllocationType;
      x.Protect:=flProtect;

      cc:=IOCTL_CE_ALLOCATEMEM;
      if deviceiocontrol(hdevice,cc,@x,sizeof(x),@r,sizeof(r),br,nil) then
      begin
        result:=pointer(r);
        exit;
      end;


    end;
  end;

  //still here
  result:=VirtualAllocEx(hprocess,lpAddress,dwSize,flAllocationType,flProtect);
end;

procedure testapc(  NormalContext:pointer; SystemArgument1:pointer; SystemArgument2:pointer);stdcall;
var
    s: string;
begin
  s:=inttohex(ptrUint(NormalContext),8)+' - '+inttohex(ptrUint(SystemArgument1),8)+' - '+inttohex(ptrUint(SystemArgument2),8);

  //CreateThread(nil,0,systemArgument1,SystemArgument2,false,@tid);
  messagebox(0,pchar(s),pchar(rsAPCRules),mb_ok);
end;


Function CreateRemoteAPC(threadid: dword; lpStartAddress: TFNAPCProc): THandle; stdcall;
var
    br,cc: dword;
    x:record
      threadid: uint64;
      addresstoexecute: uint64;
    end;

begin
  result:=0;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    x.addresstoexecute:=ptrUint(lpStartAddress);
    x.threadid:=threadid;

    cc:=IOCTL_CE_CREATEAPC;
    if deviceiocontrol(hdevice,cc,@x,sizeof(x),nil,0,br,nil) then
      result:=666 //sorry dude, no threadid returned, and no way of checking if it succeeded or not
    else
      result:=0;
  end;
end;

function WaitForProcessListData(processpointer:pointer;threadpointer:pointer;timeout:dword):dword; stdcall;
type tprocesseventstruct=record
  Created:UINT64;
  ProcessID:UINT64;
  PEProcess:UINT64;
end;
type tthreadeventstruct=record
  Created:UINT64;
  ProcessID:UINT64;
  ThreadID:UINT64;
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
var
  cc,x: dword;
  openhandles: byte;
begin
  if ProcessWatcherOpensHandles then
    openhandles:=1
  else
    openhandles:=0;

  result:=false;
  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    //OutputDebugString('StartProcessWatch');
    cc:=IOCTL_CE_STARTPROCESSWATCH;
    result:=deviceiocontrol(hdevice,cc,@openhandles,1,nil,0,x,nil);
  end;
end;

function MakeWritable(Address,Size:dword;copyonwrite:boolean): boolean; stdcall;
type TMemoryDesignation=record
  StartAddress:UINT64;
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
    result:=deviceiocontrol(hdevice,cc,@x,sizeof(x),nil,0,cc,nil);
  end;
end;

function KernelAlloc(size: dword):pointer; stdcall;
begin
  result:=pointer(ptrUint(kernelalloc64(size)));
end;

function KernelAlloc64(size: dword):uint64; stdcall;
var cc: dword;
    x: record
      Size: dword;
    end;
    output: uint64;
begin
  result:=0;
  x.Size:=size;

  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_ALLOCATEMEM_NONPAGED;
    if deviceiocontrol(hdevice,cc,@x,sizeof(x),@output,sizeof(output),cc,nil) then
      result:=output;
  end
  else
  if dbvm_version>=5 then
  begin
    //try allocating using dbvm
    result:=uint64(dbvm_kernelalloc(size));
  end;
end;

procedure KernelFree(address: uint64); stdcall;
var cc: dword;
    x: record
      address: uint64;
    end;
begin
  x.address:=address;

  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_FREE_NONPAGED;
    deviceiocontrol(hdevice,cc,@x,sizeof(x),@output,sizeof(output),cc,nil);
  end;
end;



function MapMemory(address: ptruint; size: dword; frompid: dword=0; topid: dword=0):TMapMemoryResult;
var cc: dword;
    input: record
      FromPID: uint64;
      ToPid: uint64;
      address: uint64;
      size: dword;
    end;
    output: record
      mdl: uint64;
      address: uint64;
    end;
begin
  result.address:=0;
  result.mdladdress:=0;
  input.frompid:=frompid;
  input.topid:=topid;
  input.address:=address;
  input.size:=size;

  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_MAP_MEMORY;
    if deviceiocontrol(hdevice,cc,@input,sizeof(input),@output,sizeof(output),cc,nil) then
    begin
      result.mdladdress:=output.mdl;
      result.address:=output.address;
    end;

  end;
end;

procedure UnmapMemory(r: TMapMemoryResult);
var
  input: record
    mdl: uint64;
    address: uint64;
  end;
  cc: dword;
begin
  input.mdl:=r.mdladdress;
  input.address:=r.address;

  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_UNMAP_MEMORY;
    deviceiocontrol(hdevice,cc,@input,sizeof(input),nil,0,cc,nil);
  end;
end;

function LockMemory(processid: DWORD; address: ptruint; size: integer): QWORD;
var cc: dword;
    input: packed record
      ProcessID: uint64;
      address: uint64;
      size: uint64;
    end;
    output: record
      mdl: uint64;
    end;
begin
  result:=0;
  input.processid:=processid;
  input.address:=address;
  input.size:=size;

  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_LOCK_MEMORY;
    if deviceiocontrol(hdevice,cc,@input,sizeof(input),@output,sizeof(output),cc,nil) then
      result:=output.mdl;
  end;

end;

procedure UnlockMemory(MDLAddress: QWORD);
var cc: dword;
    input: record
      mdl: uint64;
    end;
begin
  input.mdl:=MDLAddress;
  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_UNLOCK_MEMORY;
    deviceiocontrol(hdevice,cc,@input,sizeof(input),nil,0,cc,nil);
  end;
end;

function GetKProcAddress(s: pwidechar):pointer; stdcall;
begin
  result:=pointer(ptrUint(GetKProcAddress64(s)));
end;

function GetKProcAddress64(s: pwidechar):uint64; stdcall;
var cc: dword;
    output: uint64;
    st: string;
    sp: uint64;
    d: system.qword;
begin
  result:=0;

  st:=s;

  if length(st)<4 then exit;

  if TryStrToQWord('$'+st, d) then exit; //windows BSOD's if it's a hexadecimal value

  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_GETPROCADDRESS;
    output:=0;
    sp:=ptrUint(s);
    if deviceiocontrol(hdevice,cc,@sp,sizeof(sp),@output,sizeof(output),cc,nil) then
      result:=output;
  end
  else
  begin
    if dbvm_version>=5 then
      result:=uint64(dbvm_getProcAddress(st));
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

function ExecuteKernelCode(address: uint64; parameters: uint64): BOOL; stdcall;
var
  cc: dword;
  input: record
    address: uint64;
    parameters: uint64;
  end;
begin
  OutputDebugString(pchar('ExecuteKernelCode('+inttohex(address,8)+','+inttohex(parameters,8)+')'));
  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    input.address:=address;
    input.parameters:=parameters;
    cc:=IOCTL_CE_EXECUTE_CODE;
    result:=deviceiocontrol(hdevice,cc,@input,sizeof(input),nil,0,cc,nil);
  end
  else
    result:=false;
end;

procedure ultimap_flush;   //call this only when the workers are ACTIVE
var cc: dword;
begin

  if (hdevice<>INVALID_HANDLE_VALUE) then
    deviceiocontrol(hdevice,IOCTL_CE_ULTIMAP_FLUSH,nil,0,nil,0,cc,nil);
end;

function ultimap_continue(previousdataresult: PUltimapDataEvent): boolean;
var cc: dword;
begin
  if (hdevice<>INVALID_HANDLE_VALUE) then
    result:=deviceiocontrol(hdevice,IOCTL_CE_ULTIMAP_CONTINUE,previousdataresult,sizeof(TUltimapDataEvent),nil,0,cc,nil)
  else
    result:=false;
end;

function ultimap_waitForData(timeout: dword; output: PUltimapDataEvent): boolean;
var cc: dword;
begin
  if (hdevice<>INVALID_HANDLE_VALUE) then
    result:=deviceiocontrol(hdevice,IOCTL_CE_ULTIMAP_WAITFORDATA,@timeout,sizeof(timeout),output,sizeof(TUltimapDataEvent),cc,nil)
  else
    result:=false;
end;

function ultimap(cr3: QWORD; debugctl_value: QWORD; DS_AREA_SIZE: integer; savetofile: boolean; filename: widestring; handlercount: integer): Boolean; stdcall;
var
  cc: dword;
  input: packed record
    CR3: QWORD;
    DEBUGCTL: QWORD;
    DS_AREA_SIZE: QWORD;
    SaveToFile:BOOL;
    HandlerCount: integer;
    filename: array [0..199] of WideChar;
  end;
  i: integer;

begin
  outputdebugstring(pchar(Format('ultimap: %x,%x,%d',[cr3, debugctl_value, DS_AREA_SIZE])));

  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    input.cr3:=cr3;
    input.DEBUGCTL:=debugctl_value;
    input.DS_AREA_SIZE:=DS_AREA_SIZE;
    input.SaveToFile:=SaveToFile;
    input.HandlerCount:=HandlerCount;

    if savetofile then
    begin
      filename:='\DosDevices\'+filename;
      for i:=1 to length(filename)+1 do //include last byte as well (0 term)
        input.filename[i-1]:=filename[i];
    end;

    cc:=IOCTL_CE_ULTIMAP;

    result:=deviceiocontrol(hdevice,cc,@input,sizeof(input),nil,0,cc,nil);
  end
  else
    result:=false;
end;

function ultimap_disable: BOOLEAN; stdcall;
var cc: dword;
begin
  result:=false;

  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_ULTIMAP_DISABLE;
    result:=deviceiocontrol(hdevice,cc,nil,0,nil,0,cc,nil);
  end;
end;

procedure ultimap_pause;
var cc: dword;
begin
  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_ULTIMAP_PAUSE;
    deviceiocontrol(hdevice,cc,nil,0,nil,0,cc,nil);
  end;
end;

procedure ultimap_resume;
var cc: dword;
begin
  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_ULTIMAP_RESUME;
    deviceiocontrol(hdevice,cc,nil,0,nil,0,cc,nil);
  end;
end;

function readMSR(msr: dword): QWORD; //raises an exception if the msr is invalid
var
  cc: dword;
  msrvalue: qword;
begin
  result:=QWORD($ffffffffffffffff);

  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    cc:=IOCTL_CE_READMSR;
    //OutputDebugString(pchar('dbk32functions.pas: Reading from msr '+inttohex(msr,1)));
    if deviceiocontrol(hdevice,cc,@msr,sizeof(msr),@msrvalue,sizeof(msrvalue),cc,nil) then
      result:=msrvalue
    else
      raise exception.create(rsInvalidMsrAddress+inttohex(msr,1));
  end
  else
  begin
    if dbvm_version>=$ce000006 then
    begin
      try
        result:=dbvm_readMSR(msr); //will raise a GPF if it doesn't exist
      except
      end;
    end
    else
      raise exception.create(rsMsrsAreUnavailable);
  end;
end;

procedure writeMSR(msr: dword; value: qword);
var
  cc: dword;
  input: record
    msr: uint64;
    msrvalue: uint64;
  end;
begin
  if dbvm_version>=6 then
    dbvm_writeMSR(msr, value)
  else
  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    input.msr:=msr;
    input.msrvalue:=value;

    cc:=IOCTL_CE_WRITEMSR;
    deviceiocontrol(hdevice,cc,@input,sizeof(input),nil,0,cc,nil);
  end
  else
  raise exception.create(rsMsrsAreUnavailable);
end;


function AllCoresHaveDBVMLoaded: boolean;
var
  i: integer;
  proc, sys: DWORD_PTR;
begin
  result:=true;
  GetProcessAffinityMask(GetCurrentProcess, proc, sys);

  for i:=0 to {$ifdef cpu32}31{$else}63{$endif} do
  begin

    if getbit(i, sys)=1 then
    begin
      SetProcessAffinityMask(GetCurrentProcess, 1 shl i);
      sleep(10);

      if dbvm_version=0 then
        result:=false;
    end;
  end;


  SetProcessAffinityMask(GetCurrentProcess, proc);
end;

function internal_LaunchDBVM(parameters: pointer): BOOL; stdcall;
var
  cc: dword;
  Input: record
    dbvmimgpath: qword;
    cpuid: dword;
  end;

  temp: widestring;

  proc, sys, thread: DWORD_PTR;

  cpuid: integer;
  fc: dword;
begin


  if (hdevice<>INVALID_HANDLE_VALUE) then
  begin
    Outputdebugstring('LaunchDBVM');


    if parameters<>nil then
    begin
      OutputDebugString('Param is valid');

      cpuid:=pinteger(parameters)^;
      input.cpuid:=cpuid;
      Outputdebugstring('For cpu '+inttostr(cpuid));

      GetProcessAffinityMask(GetCurrentProcess, proc, sys);
      SetProcessAffinityMask(GetCurrentProcess, 1 shl cpuid);
      SetThreadAffinityMask(GetCurrentThread, 1 shl cpuid);
      sleep(10);
    end
    else
    begin
      input.cpuid:=$ffffffff;
      OutputDebugString('Param is invalid');
    end;

    if not isAMD then
    begin
      OutputDebugString('A');
      fc:=readMSR($3a); //get IA32_FEATURE_CONTROL

      OutputDebugString('B');
      if (fc and 1)=1 then
      begin
        //the feature control msr is locked
        if (fc and (1 shl 2))=0 then
          raise exception.create(rsCouldNotLaunchDbvm);
      end;
      OutputDebugString('C');
    end;

    cc:=IOCTL_CE_LAUNCHDBVM;
    temp:='\??\'+widestring(applicationpath)+'vmdisk.img';

    input.dbvmimgpath:=qword(ptrUint(@temp[1]));

    OutputDebugString('Calling deviceiocontrol');
    result:=deviceiocontrol(hdevice,cc,@input,sizeof(Input),nil,0,cc,nil);


    configure_vmx(vmx_password1, vmx_password2, vmx_password3);

    if parameters<>nil then
    begin
      SetProcessAffinityMask(GetCurrentProcess, proc);
      SetThreadAffinityMask(GetCurrentThread, proc);
    end;

  end else result:=false;
end;


procedure LaunchDBVM(cpuid: integer); stdcall;
begin
  LoadDBK32;

  OutputDebugString('LaunchDBVM('+inttostr(cpuid)+') Before check');

  if (not vmx_enabled) or (cpuid<>-1) then
  begin
    OutputDebugString('LaunchDBVM('+inttostr(cpuid)+')');

    OutputDebugString('calling internal_LaunchDBVM');

    internal_LaunchDBVM(@cpuid);


//    foreachcpu(internal_LaunchDBVM,nil);


  end;

  if (cpuid=-1) or (AllCoresHaveDBVMLoaded) then
  begin
    OutputDebugString('All cpu cores loaded. Activate kernelmode dbvm support');
    configure_vmx_kernel;
  end;
end;

procedure allocateMemoryForDBVM(pagecount: qword);
var br: dword;
begin
  if hdevice<>INVALID_HANDLE_VALUE then deviceiocontrol(hdevice,IOCTL_CE_ALLOCATE_MEMORY_FOR_DBVM,@pagecount,sizeof(pagecount),nil,0,br,nil);
end;

function RewriteKernel32:boolean; stdcall;
begin
  //modifies the code of NtOpenProcess,NtOpenThread,OpenProcess,OpenThread to point to this dll's functions
  result:=false;
end;

function RestoreKernel32: boolean; stdcall;
begin
  //
  result:=false;
end;


function CTL_CODE(DeviceType, Func, Method, Access : integer) : integer;
begin
  Result := (DeviceType shl 16) or (Access shl 14) or (Func shl 2) or Method;
end;

function InitializeDriver(Address: ptrUint; size:dword):BOOL; stdcall;
type tinput=record
  address: uint64;
  size:uint64;
  NtUserBuildHwndList_callnumber: uint64;
  NtUserQueryWindow_callnumber:uint64;
  NtUserFindWindowEx_callnumber:uint64;
  NtUserGetForegroundWindow_callnumber:uint64;
  activelinkoffset: uint64;
  processnameoffset:uint64;
  debugportoffset:uint64;
  processevent: uint64; //event handles (driver rev. 10+)
  threadevent: uint64;
end;
var cc: dword;
    buf: tinput;
    res: dword absolute buf;
    x:dword;

    callnumberfile: tfilestream;
    windowsversion: osversioninfo;
    majorversion,minorversion,buildnumber: dword;
    CSDVersion: array [0..127] of char;
    a: boolean;
    i: integer;
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
            //messagebox(0,'It is recommended to run the systemcallretriever since the kerneldata.dat you have is outdated and will not be used. Of course, if this is the systemcallretriever, ignore this message...','Outdated kerneldata.dat',mb_ok);
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
    if deviceiocontrol(hdevice,cc,@buf,sizeof(tinput),@buf,8,x,nil) then
    begin
      result:=true;
      SDTShadow:=res;
    end;
    ownprocess:=OP(ifthen<dword>(GetSystemType<=6,$1f0fff, process_all_access),false,getcurrentprocessid);
  end;
end;


function GetWin32KAddress(var address:ptrUint;var size:dworD):boolean;
{var need:dword;
    p: pointer;
    oldx: dword;
    x: array of pointer;
    i,j: integer;
    count: integer;
    drivername: pchar;
    nearest: dword; //nearest other driver (AFTER win32k.sys)
    }
begin


  result:=false;
{
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
            address:=ptrUint(x[i]);

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
}
end;



var hscManager: thandle;
    hservice, hUltimapService: thandle;

var sav: pchar;
    apppath: pwidechar;



 //   win32kaddress: ptrUint;
 //   win32size:dword;
    servicename,sysfile: widestring;
    ultimapservicename, ultimapsysfile: widestring;
    vmx_p1_txt,vmx_p2_txt,vmx_p3_txt: string;


    reg: tregistry;
    driverdat: textfile;


//    servicestatus: _service_status;
procedure DBK32Initialize;
var le: integer;
begin

  outputdebugstring('DBK32Initialize');
  try
    if hdevice=INVALID_HANDLE_VALUE then
    begin
      kernel32dll:=loadlibrary('kernel32.dll');
      IsWow64Process:=GetProcAddress(kernel32dll, 'IsWow64Process');
      if not assigned(IsWow64Process) then IsWow64Process:=noIsWow64;

      {$ifdef cpu64}
      iswow64:=true;
      {$else}
      IsWow64Process(getcurrentprocess,iswow64);
      {$endif}

    //  usealternatedebugmethod:=false;
      iamprotected:=false;
      apppath:=nil;
      hSCManager := OpenSCManager(nil, nil, GENERIC_READ or GENERIC_WRITE);
      try
        getmem(apppath,510);
        GetModuleFileNameW(0, apppath, 250);

        applicationpath:=extractfilepath(apppath);

        dataloc:=extractfilepath(apppath);
        if not iswow64 then
          dataloc:=dataloc+'driver.dat'
        else
          dataloc:=dataloc+'driver64.dat';

        if not fileexists(dataloc) then
        begin

          servicename:='CEDRIVER73';
          ultimapservicename:='ULTIMAP2';
          processeventname:='DBKProcList60';
          threadeventname:='DBKThreadList60';

          if iswow64 then
          begin
            sysfile:='dbk64.sys';
            ultimapsysfile:='ultimap2-64.sys';
          end
          else
          begin
            sysfile:='dbk32.sys';
            ultimapsysfile:='';
          end;

          vmx_p1_txt:='76543210';
          vmx_p2_txt:='fedcba98';
          vmx_p3_txt:='90909090';

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
          readln(driverdat,vmx_p3_txt);
          readln(driverdat,ultimapservicename);
          readln(driverdat,ultimapsysfile);
          closefile(driverdat);
        end;

        driverloc:=extractfilepath(apppath)+sysfile;
        ultimapdriverloc:=extractfilepath(apppath)+ultimapsysfile;
      finally
        freememandnil(apppath);
      end;


      try
        configure_vmx(strtoint64('$'+vmx_p1_txt), strtoint('$'+vmx_p2_txt), StrToInt64('$'+vmx_p3_txt)  );
      except
        //couldn't parse the password
      end;


      if (not fileexists(driverloc)) and (not fileexists(ultimapdriverloc)) then
      begin
        messagebox(0,PChar(rsYouAreMissingTheDriver),PChar(rsDriverError),MB_ICONERROR or mb_ok);
        hDevice:=INVALID_HANDLE_VALUE;
        hUltimapDevice:=INVALID_HANDLE_VALUE;
        exit;
      end;

      if hscmanager<>0 then
      begin
        //try loading ultimap
        hUltimapService:=0;
        hultimapdevice:=INVALID_HANDLE_VALUE;

        if fileexists(ultimapdriverloc) then
        begin
          hUltimapService := OpenServiceW(hSCManager, pwidechar(ultimapservicename), SERVICE_ALL_ACCESS);
          if hUltimapService=0 then
          begin
            hUltimapService:=CreateServiceW(
               hSCManager,           // SCManager database
               pwidechar(ultimapservicename),   // name of service
               pwidechar(ultimapservicename),   // name to display
               SERVICE_ALL_ACCESS,   // desired access
               SERVICE_KERNEL_DRIVER,// service type
               SERVICE_DEMAND_START, // start type
               SERVICE_ERROR_NORMAL, // error control type
               pwidechar(ultimapdriverloc),     // service's binary
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
            ChangeServiceConfigW(hultimapservice,
                                SERVICE_KERNEL_DRIVER,
                                SERVICE_DEMAND_START,
                                SERVICE_ERROR_NORMAL,
                                pwidechar(ultimapdriverloc),
                                nil,
                                nil,
                                nil,
                                nil,
                                nil,
                                pwidechar(ultimapservicename));
          end;

        end;


        if hUltimapService<>0 then
        begin
          sav:=nil;

          //setup the configuration parameters before starting the driver
          reg:=tregistry.Create;
          reg.RootKey:=HKEY_LOCAL_MACHINE;
          if reg.OpenKey('\SYSTEM\CurrentControlSet\Services\'+ultimapservicename,false) then
          begin
            reg.WriteString('A','\Device\'+ultimapservicename);
            reg.WriteString('B','\DosDevices\'+ultimapservicename);

            if startservice(hultimapservice,0,pointer(sav)) then
              OutputDebugString('started ultimap2');

            closeservicehandle(hUltimapService);
            hUltimapService:=0;
          end;

          hultimapDevice := CreateFileW(pwidechar('\\.\'+ultimapservicename),
                        GENERIC_READ or GENERIC_WRITE,
                        FILE_SHARE_READ or FILE_SHARE_WRITE,
                        nil,
                        OPEN_EXISTING,
                        FILE_FLAG_OVERLAPPED,
                        0);

          reg.DeleteValue('A');
          reg.DeleteValue('B');

          freeandnil(reg);
        end;


        //load DBK

        hService := OpenServiceW(hSCManager, pwidechar(servicename), SERVICE_ALL_ACCESS);
        if hService=0 then
        begin
          hService:=CreateServiceW(
             hSCManager,           // SCManager database
             pwidechar(servicename),   // name of service
             pwidechar(servicename),   // name to display
             SERVICE_ALL_ACCESS,   // desired access
             SERVICE_KERNEL_DRIVER,// service type
             SERVICE_DEMAND_START, // start type
             SERVICE_ERROR_NORMAL, // error control type
             pwidechar(driverloc),     // service's binary
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
          ChangeServiceConfigW(hservice,
                              SERVICE_KERNEL_DRIVER,
                              SERVICE_DEMAND_START,
                              SERVICE_ERROR_NORMAL,
                              pwidechar(driverloc),
                              nil,
                              nil,
                              nil,
                              nil,
                              nil,
                              pwidechar(servicename));


        end;


        if hservice<>0 then
        begin
          sav:=nil;

          //setup the configuration parameters before starting the driver
          reg:=tregistry.Create;
          reg.RootKey:=HKEY_LOCAL_MACHINE;
          if not reg.OpenKey('\SYSTEM\CurrentControlSet\Services\'+servicename,false) then
          begin
            messagebox(0,PChar(rsFailureToConfigureTheDriver),PChar(rsDriverError),MB_ICONERROR or mb_ok);
            hDevice:=INVALID_HANDLE_VALUE;
            exit;
          end;

          reg.WriteString('A','\Device\'+servicename);
          reg.WriteString('B','\DosDevices\'+servicename);
          reg.WriteString('C','\BaseNamedObjects\'+processeventname);
          reg.WriteString('D','\BaseNamedObjects\'+threadeventname);

          if not startservice(hservice,0,pointer(sav)) then
          begin
            le:=getlasterror;
            if le=577 then
            begin
              if dbvm_version=0 then
                messagebox(0,PChar(rsPleaseRebootAndPressF8DuringBoot),PChar(rsDbk32Error),MB_ICONERROR or mb_ok);
              failedduetodriversigning:=true;
            end; //else could already be started

            if le<>1056 then
            begin
              if dbvm_version=0 then
                messagebox(0,PChar('Failure starting dbk:'+inttostr(le)),PChar(rsDbk32Error),MB_ICONERROR or mb_ok);
            end;
          end;


          closeservicehandle(hservice);
          hservice:=0;
        end else
        begin
          messagebox(0,PChar(rsTheServiceCouldntGetOpened),PChar(rsDbk32Error),MB_ICONERROR or mb_ok);
          hDevice:=INVALID_HANDLE_VALUE;
          exit;
        end;

        hdevice:=INVALID_HANDLE_VALUE;
        hDevice := CreateFileW(pwidechar('\\.\'+servicename),
                      GENERIC_READ or GENERIC_WRITE,
                      FILE_SHARE_READ or FILE_SHARE_WRITE,
                      nil,
                      OPEN_EXISTING,
                      FILE_FLAG_OVERLAPPED,
                      0);


        if hdevice=INVALID_HANDLE_VALUE then
        begin
          if dbvm_version>$ce000000 then
          begin
            if MessageDlg(rsTheDriverCouldntBeOpened, mtconfirmation, [mbyes, mbno],0)=mryes then
            begin
              OutputDebugString('Calling SecondaryDriverLoad');
              {$ifdef cpu32}
              if iswow64 then
              begin
                ShowMessage(rsPleaseRunThe64BitVersionOfCE);
                exit;
              end;
              {$endif}
              hdevice:=SecondaryDriverLoad;
              OutputDebugString(pchar('SecondaryDriverLoad returned '+inttohex(hDevice,1)));
            end;

          end
          else
          begin
            messagebox(0,PChar(rsTheDriverCouldntBeOpenedTryAgain),pchar(rsDBKError),MB_ICONERROR or MB_OK)
          end;

        end
        else
        begin
          if Is64BitOS then
            ShowDriverLoaded;


          //Get the address of win32k.sys
          if GetDriverVersion<>currentversion then
          begin
            closehandle(hdevice);
            messagebox(0,PChar(rsTheDriverThatIsCurrentlyLoaded),'DBK',MB_ICONERROR or MB_OK);

            hdevice:=INVALID_HANDLE_VALUE;
          end
          else
          begin
            InitializeDriver(0,0);
            {
            if not InitializeDriver(0,0) then
              messagebox(0,rsTheDriverFailedToSuccessfullyInitialize,'DBK',MB_ICONERROR or MB_OK);
              }

          end;

          //.in case the ultimap driver is missing or fail to load, fall back on DBK (which has the same ioctl values)
          if (hdevice<>INVALID_HANDLE_VALUE) and (hUltimapDevice=INVALID_HANDLE_VALUE) then
          begin
            hUltimapDevice:=hDevice;
            OutputDebugString('Falling back on DBK for ultimap2');
          end;
        end;

        //successfully initialized, say goodbye to the init params
        reg.DeleteValue('A');
        reg.DeleteValue('B');
        reg.DeleteValue('C');
        reg.DeleteValue('D');


        closeservicehandle(hscmanager);
      end
      else
        OutputDebugString('hscmanager=0');
    end;

  finally
    configure_vmx_kernel;
  end;




end;



var w: thandle;

initialization
  w:=LoadLibrary('kernel32.dll');
  VirtualAllocEx:=GetProcAddress(w,'VirtualAllocEx');

  handlemapMREW:=TMultiReadExclusiveWriteSynchronizer.Create;
  handlemap:=tmap.Create(ituPtrSize,sizeof(THandleListEntry));

finalization
begin
  if ownprocess<>0 then
    closehandle(ownprocess);

  freeandnil(handlemap);
  freeandnil(handlemapmrew);
end;
{$endif}
end.
