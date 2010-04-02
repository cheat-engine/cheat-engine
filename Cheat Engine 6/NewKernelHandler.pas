unit NewKernelHandler;

{$MODE Delphi}

interface
uses jwawindows, windows,LCLIntf,sysutils,{tlhelp32,} dialogs, controls;

const dbkdll='DBK32.dll';


type
PPROCESSENTRY32 = ^PROCESSENTRY32;

tagPROCESSENTRY32 = record
  dwSize: DWORD;
  cntUsage: DWORD;
  th32ProcessID: DWORD;          // this process
  th32DefaultHeapID: ULONG_PTR;
  th32ModuleID: DWORD;           // associated exe
  cntThreads: DWORD;
  th32ParentProcessID: DWORD;    // this process's parent process
  pcPriClassBase: LONG;          // Base priority of process's threads
  dwFlags: DWORD;
  szExeFile: array [0..MAX_PATH - 1] of Char;    // Path
end;
PROCESSENTRY32 = tagPROCESSENTRY32;
LPPROCESSENTRY32 = ^PROCESSENTRY32;
TProcessEntry32 = PROCESSENTRY32;


PMODULEENTRY32 = ^MODULEENTRY32;
tagMODULEENTRY32 = record
  dwSize: DWORD;
  th32ModuleID: DWORD;       // This module
  th32ProcessID: DWORD;      // owning process
  GlblcntUsage: DWORD;       // Global usage count on the module
  ProccntUsage: DWORD;       // Module usage count in th32ProcessID's context
  modBaseAddr: LPBYTE;       // Base address of module in th32ProcessID's context
  modBaseSize: DWORD;        // Size in bytes of module starting at modBaseAddr
  hModule: HMODULE;          // The hModule of this module in th32ProcessID's context
  szModule: array [0..MAX_MODULE_NAME32] of Char;
  szExePath: array [0..MAX_PATH - 1] of Char;
end;
MODULEENTRY32 = tagMODULEENTRY32;
LPMODULEENTRY32 = ^MODULEENTRY32;
TModuleEntry32 = MODULEENTRY32;


{$ifndef cpu64}

const
  CONTEXT_EXTENDED_REGISTERS = (CONTEXT_i386 or $00000020);

//credits to jedi code library for filling in the "extended registers"
type
  TJclMMContentType = (mt8Bytes, mt4Words, mt2DWords, mt1QWord, mt2Singles, mt1Double);

  TJclMMRegister = packed record
    case TJclMMContentType of
      mt8Bytes:
        (Bytes: array [0..7] of Byte;);
      mt4Words:
        (Words: array [0..3] of Word;);
      mt2DWords:
        (DWords: array [0..1] of Cardinal;);
      mt1QWord:
        (QWords: Int64;);
      mt2Singles:
        (Singles: array [0..1] of Single;);
      mt1Double:
        (Doubles: double;);
  end;

  TJclFPUContentType = (ftExtended, ftMM);

  TJclFPUData = packed record
    case TJclFPUContentType of
      ftExtended:
        (FloatValue: Extended;);
      ftMM:
        (MMRegister: TJclMMRegister;
         Reserved: Word;);
  end;

  TJclFPURegister = packed record
    Data: TJclFPUData;
    Reserved: array [0..5] of Byte;
  end;

  TJclFPURegisters = array [0..7] of TJclFPURegister;

  TJclXMMContentType = (xt16Bytes, xt8Words, xt4DWords, xt2QWords, xt4Singles, xt2Doubles);

  TJclXMMRegister = packed record
    case TJclXMMContentType of
      xt16Bytes:
        (Bytes: array [0..15] of Byte;);
      xt8Words:
        (Words: array [0..7] of Word;);
      xt4DWords:
        (DWords: array [0..3] of Cardinal;);
      xt2QWords:
        (QWords: array [0..1] of Int64;);
      xt4Singles:
        (Singles: array [0..3] of Single;);
      xt2Doubles:
        (Doubles: array [0..1] of Double;);
  end;

  TJclProcessorSize = (ps32Bits, ps64Bits);

  TJclXMMRegisters = packed record
    case TJclProcessorSize of
      ps32Bits:
        (LegacyXMM: array [0..7] of TJclXMMRegister;
         LegacyReserved: array [0..127] of Byte;);
      ps64Bits:
        (LongXMM: array [0..15] of TJclXMMRegister;);
  end;

  TextendedRegisters = packed record
    //extended registers
    FCW: Word;                           // bytes from 0   to 1
    FSW: Word;                           // bytes from 2   to 3
    FTW: Byte;                           // byte 4
    Reserved1: Byte;                     // byte 5
    FOP: Word;                           // bytes from 6   to 7
    FpuIp: Cardinal;                     // bytes from 8   to 11
    CS: Word;                            // bytes from 12  to 13
    Reserved2: Word;                     // bytes from 14  to 15
    FpuDp: Cardinal;                     // bytes from 16  to 19
    DS: Word;                            // bytes from 20  to 21
    Reserved3: Word;                     // bytes from 22  to 23
    MXCSR: Cardinal;                     // bytes from 24  to 27
    MXCSRMask: Cardinal;                 // bytes from 28  to 31
    FPURegisters: TJclFPURegisters;      // bytes from 32  to 159
    XMMRegisters: TJclXMMRegisters;      // bytes from 160 to 415
    Reserved4: array [416..511] of Byte; // bytes from 416 to 511
  end;



  _CONTEXT = record
    ContextFlags: DWORD;
    Dr0: DWORD;
    Dr1: DWORD;
    Dr2: DWORD;
    Dr3: DWORD;
    Dr6: DWORD;
    Dr7: DWORD;

    FloatSave: TFloatingSaveArea;

    SegGs: DWORD;
    SegFs: DWORD;
    SegEs: DWORD;
    SegDs: DWORD;

    Edi: DWORD;
    Esi: DWORD;
    Ebx: DWORD;
    Edx: DWORD;
    Ecx: DWORD;
    Eax: DWORD;

    Ebp: DWORD;
    Eip: DWORD;
    SegCs: DWORD;
    EFlags: DWORD;
    Esp: DWORD;
    SegSs: DWORD;

    ext: TExtendedRegisters;
  end;
  CONTEXT=_CONTEXT;
  TContext=CONTEXT;
  PContext = ^TContext;
{$endif}


type TDebuggerstate=record
  threadid: uint64;
	eflags : uint64;
	eax : uint64;
	ebx : uint64;
	ecx : uint64;
	edx : uint64;
	esi : uint64;
	edi : uint64;
	ebp : uint64;
	esp : uint64;
	eip : uint64;
	r8  : uint64;
	r9  : uint64;
	r10 : uint64;
	r11 : uint64;
	r12 : uint64;
	r13 : uint64;
	r14 : uint64;
	r15 : uint64;
	cs  : uint64;
	ds  : uint64;
	es  : uint64;
	fs  : uint64;
	gs  : uint64;
	ss  : uint64;
  dr0 : uint64;
  dr1 : uint64;
  dr2 : uint64;
  dr3 : uint64;
  dr6 : uint64;
  dr7 : uint64;
end;
type PDebuggerstate=^TDebuggerstate;

type TBreakType=(bt_OnInstruction=0,bt_OnWrites=1, bt_OnIOAccess=2, bt_OnReadsAndWrites=3);
type TBreakLength=(bl_1byte=0, bl_2byte=1, bl_8byte=2{Only when in 64-bit}, bl_4byte=3);


type TReadProcessMemory=function(hProcess: THandle; lpBaseAddress, lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesRead: DWORD): BOOL; stdcall;
type TReadProcessMemory64=function(hProcess: THandle; lpBaseAddress: UINT64; lpBuffer: pointer; nSize: DWORD; var lpNumberOfBytesRead: DWORD): BOOL; stdcall;
type TWriteProcessMemory=function(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesWritten: DWORD): BOOL; stdcall;
type TWriteProcessMemory64=function(hProcess: THandle; BaseAddress: UINT64; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesWritten: DWORD): BOOL; stdcall;
type TGetThreadContext=function(hThread: THandle; var lpContext: TContext): BOOL; stdcall;


type TSetThreadContext=function(hThread: THandle; const lpContext: TContext): BOOL; stdcall;
type TSuspendThread=function(hThread: THandle): DWORD; stdcall;
type TResumeThread=function(hThread: THandle): DWORD; stdcall;
type TOpenProcess=function(dwDesiredAccess: DWORD; bInheritHandle: BOOL; dwProcessId: DWORD): THandle; stdcall;

type TCreateToolhelp32Snapshot=function(dwFlags, th32ProcessID: DWORD): THandle; stdcall;
type TProcess32First=function(hSnapshot: THandle; var lppe: TProcessEntry32): BOOL; stdcall;
type TProcess32Next=function(hSnapshot: THandle; var lppe: TProcessEntry32): BOOL; stdcall;
type TThread32First=function (hSnapshot: THandle; var lpte: TThreadEntry32): BOOL; stdcall;
type TThread32Next=function (hSnapshot: THandle; var lpte: TThreadENtry32): BOOL; stdcall;
type TModule32First=function (hSnapshot: THandle; var lpme: TModuleEntry32): BOOL; stdcall;
type TModule32Next=function (hSnapshot: THandle; var lpme: TModuleEntry32): BOOL; stdcall;
type THeap32ListFirst=function (hSnapshot: THandle; var lphl: THeapList32): BOOL; stdcall;
type THeap32ListNext=function (hSnapshot: THandle; var lphl: THeapList32): BOOL; stdcall;
type TIsWow64Process=function (processhandle: THandle; var isWow: BOOL): BOOL; stdcall;

type TWaitForDebugEvent=function(var lpDebugEvent: TDebugEvent; dwMilliseconds: DWORD): BOOL; stdcall;
type TContinueDebugEvent=function(dwProcessId, dwThreadId, dwContinueStatus: DWORD): BOOL; stdcall;
type TDebugActiveProcess=function(dwProcessId: DWORD): BOOL; stdcall;
type TVirtualProtect=function(lpAddress: Pointer; dwSize, flNewProtect: DWORD; var OldProtect: DWORD): BOOL; stdcall;
type TVirtualProtectEx=function(hProcess: THandle; lpAddress: Pointer; dwSize, flNewProtect: DWORD; var OldProtect: DWORD): BOOL; stdcall;
type TVirtualQueryEx=function(hProcess: THandle; lpAddress: Pointer; var lpBuffer: TMemoryBasicInformation; dwLength: DWORD): DWORD; stdcall;
type TVirtualAllocEx=function(hProcess: THandle; lpAddress: Pointer; dwSize, flAllocationType: DWORD; flProtect: DWORD): Pointer; stdcall;
type TCreateRemoteThread=function(hProcess: THandle; lpThreadAttributes: Pointer; dwStackSize: DWORD; lpStartAddress: TFNThreadStartRoutine; lpParameter: Pointer;  dwCreationFlags: DWORD; var lpThreadId: DWORD): THandle; stdcall;
type TOpenThread=function(dwDesiredAccess:DWORD;bInheritHandle:BOOL;dwThreadId:DWORD):THANDLE; stdcall;
type TGetPEProcess=function(ProcessID:DWORD):UINT64; stdcall;
type TGetPEThread=function(Threadid: dword):UINT64; stdcall;
type TGetDebugportOffset=function:DWORD; stdcall;
type TGetProcessnameOffset=function:DWORD; stdcall;
type TGetThreadsProcessOffset=function: dword; stdcall;
type TGetThreadListEntryOffset=function: dword; stdcall;



type TGetPhysicalAddress=function(hProcess:THandle;lpBaseAddress:pointer;var Address:int64): BOOL; stdcall;
type TGetCR4=function:ptrUint; stdcall;
type TGetCR3=function(hProcess:THANDLE;var CR3: ptrUint):BOOL; stdcall;
type TSetCR3=function(hProcess:THANDLE;CR3: ptrUint):BOOL; stdcall;
type TGetCR0=function:ptrUint; stdcall;
type TGetSDT=function:ptrUint; stdcall;
type TGetSDTShadow=function:ptrUint; stdcall;


type TCreateRemoteAPC=function(threadid: dword; lpStartAddress: pointer): THandle; stdcall;


//type TStopDebugging=function: BOOL; stdcall;
//type TStopRegisterChange=function(regnr:integer):BOOL; stdcall;

//type TSetGlobalDebugState=function(state: boolean): BOOL; stdcall;
//type TsetAlternateDebugMethod=function(var int1apihook:dword; var OriginalInt1handler:dword):BOOL; stdcall;
//type TgetAlternateDebugMethod=function:BOOL; stdcall;

//type TChangeRegOnBP=function(Processid:dword; address: dword; debugreg: integer; changeEAX,changeEBX,changeECX,changeEDX,changeESI,changeEDI,changeEBP,changeESP,changeEIP,changeCF,changePF,changeAF,changeZF,changeSF,changeOF:BOOLEAN; newEAX,newEBX,newECX,newEDX,newESI,newEDI,newEBP,newESP,newEIP:DWORD; newCF,newPF,newAF,newZF,newSF,newOF:BOOLEAN):BOOLEAN; stdcall;
//type TDebugProcess=function(processid:dword;address:DWORD;size: byte;debugtype:byte):BOOL; stdcall;
//type TRetrieveDebugData=function(Buffer: pointer):integer; stdcall;

type TGetProcessNameFromID=function(processid:dword; buffer:pchar;buffersize:dword):integer; stdcall;
type TGetProcessNameFromPEProcess=function(peprocess:uint64; buffer:pchar;buffersize:dword):integer; stdcall;

type TStartProcessWatch=function:BOOL;stdcall;
type TWaitForProcessListData=function(processpointer:pointer;threadpointer:pointer;timeout:dword):dword; stdcall;

type TIsValidHandle=function(hProcess:THandle):BOOL; stdcall;
type TGetIDTCurrentThread=function:dword; stdcall;
type TGetIDTs=function(idtstore: pointer; maxidts: integer):integer; stdcall;
type TMakeWritable=function(Address,Size:dword;copyonwrite:boolean): boolean; stdcall;
type TGetLoadedState=function : BOOLEAN; stdcall;

type TDBKSuspendThread=function(ThreadID:dword):boolean; stdcall;
type TDBKResumeThread=function(ThreadID:dword):boolean; stdcall;
type TDBKSuspendProcess=function(ProcessID:dword):boolean; stdcall;
type TDBKResumeProcess=function(ProcessID:dword):boolean; stdcall;

type TKernelAlloc=function(size: dword):pointer; stdcall;
type TKernelAlloc64=function(size: dword):UINT64; stdcall;
type TGetKProcAddress=function(s: pwidechar):pointer; stdcall;
type TGetKProcAddress64=function(s: pwidechar):UINT64; stdcall;

type TGetSDTEntry=function (nr: integer; address: PDWORD; paramcount: PBYTE):boolean; stdcall;
type TGetSSDTEntry=function (nr: integer; address: PDWORD; paramcount: PBYTE):boolean; stdcall;
type TGetGDT=function(var limit: word):dword; stdcall;

type TisDriverLoaded=function(SigningIsTheCause: PBOOL): BOOL; stdcall;
type TLaunchDBVM=procedure; stdcall;


type TDBKDebug_ContinueDebugEvent=function(handled: BOOL): boolean; stdcall;
type TDBKDebug_WaitForDebugEvent=function(timeout: dword): boolean; stdcall;
type TDBKDebug_GetDebuggerState=function(state: PDebuggerstate): boolean; stdcall;
type TDBKDebug_SetDebuggerState=function(state: PDebuggerstate): boolean; stdcall;
type TDBKDebug_SetGlobalDebugState=function(state: BOOL): BOOL; stdcall;
type TDBKDebug_StartDebugging=function(processid:dword):BOOL; stdcall;
type TDBKDebug_StopDebugging=function:BOOL; stdcall;
type TDBKDebug_GD_SetBreakpoint=function(active: BOOL; debugregspot: integer; Address: dword; breakType: TBreakType; breakLength: TbreakLength): BOOL; stdcall;

//-----------------------------------DBVM-------------------------------------//
type Tdbvm_version=function: dword; stdcall;
type Tdbvm_changeselectors=function(cs,ss,ds,es,fs,gs: dword): DWORD; stdcall;
type Tdbvm_restore_interrupts=function: DWORD; stdcall;
type Tdbvm_block_interrupts=function: DWORD; stdcall;
type Tdbvm_raise_privilege=function: DWORD; stdcall;


type Tdbvm_read_physical_memory=function(PhysicalAddress: UINT64; destination: pointer; size: integer): dword; stdcall;
type Tdbvm_write_physical_memory=function(PhysicalAddress: UINT64; source: pointer; size: integer): dword; stdcall;


procedure DONTUseDBKQueryMemoryRegion;
procedure DONTUseDBKReadWriteMemory;
procedure DONTUseDBKOpenProcess;
procedure UseDBKQueryMemoryRegion;
procedure UseDBKReadWriteMemory;
procedure UseDBKOpenProcess;

procedure DBKFileAsMemory(filename:string); overload;
procedure DBKFileAsMemory; overload;
procedure DBKPhysicalMemory;
procedure DBKPhysicalMemoryDBVM;
procedure DBKProcessMemory;
procedure LoadDBK32; stdcall;

procedure OutputDebugString(msg: string);


function loaddbvmifneeded: BOOL; stdcall;
function isRunningDBVM: boolean;
function isDBVMCapable: boolean;

function Is64bitOS: boolean;
function Is64BitProcess(processhandle: THandle): boolean;

//I could of course have made it a parameter thing, but I'm lazy

var
  ReadProcessMemory     :TReadProcessMemory;
  ReadProcessMemory64   :TReadProcessMemory64;  
  WriteProcessMemory    :TWriteProcessMemory;
  WriteProcessMemory64  :TWriteProcessMemory64;
  GetThreadContext      :TGetThreadContext;
  SetThreadContext      :TSetThreadContext;
  SuspendThread         :TSuspendThread;
  ResumeThread          :TResumeThread;
  OpenProcess           :TOpenProcess;

  CreateToolhelp32Snapshot: TCreateToolhelp32Snapshot;
  Process32First        :TProcess32First;
  Process32Next         :TProcess32Next;
  Thread32First         :TThread32First;
  Thread32Next          :TThread32Next;
  Module32First         :TModule32First;
  Module32Next          :TModule32Next;
  Heap32ListFirst       :THeap32ListFirst;
  Heap32ListNext        :THeap32ListNext;
  IsWow64Process        :TIsWow64Process;


  WaitForDebugEvent     :TWaitForDebugEvent;
  ContinueDebugEvent    :TContinueDebugEvent;
  DebugActiveProcess    :TDebugActiveProcess;


  VirtualProtect        :TVirtualProtect;
  VirtualProtectEx      :TVirtualProtectEx;
  VirtualQueryEx        :TVirtualQueryEx;
  VirtualAllocEx        :TVirtualAllocEx;
  CreateRemoteThread    :TCreateRemoteThread;
  OpenThread            :TOpenThread;
  GetPEProcess          :TGetPEProcess;
  GetPEThread           :TGetPEThread;
  GetThreadsProcessOffset:TGetThreadsProcessOffset;
  GetThreadListEntryOffset:TGetThreadListEntryOffset;
  GetProcessnameOffset  :TGetProcessnameOffset;

  GetDebugportOffset    :TGetDebugportOffset;
  GetPhysicalAddress    :TGetPhysicalAddress;
  GetCR4                :TGetCR4;
  GetCR3                :TGetCR3;
  SetCR3                :TSetCR3;
  GetCR0                :TGetCR0;
  GetSDT                :TGetSDT;
  GetSDTShadow          :TGetSDTShadow;

//  setAlternateDebugMethod: TsetAlternateDebugMethod;
//  getAlternateDebugMethod: TgetAlternateDebugMethod;

//  SetGlobalDebugState   :TSetGlobalDebugState;
//  DebugProcess          :TDebugProcess;
//  ChangeRegOnBP         :TChangeRegOnBP;
//  RetrieveDebugData     :TRetrieveDebugData;
//  StopDebugging         :TStopDebugging;
//  StopRegisterChange    :TStopRegisterChange;
  StartProcessWatch     :TStartProcessWatch;
  WaitForProcessListData:TWaitForProcessListData;
  GetProcessNameFromID  :TGetProcessNameFromID;
  GetProcessNameFromPEProcess:TGetProcessNameFromPEProcess;


  KernelOpenProcess       :TOpenProcess;
  KernelReadProcessMemory :TReadProcessMemory;
  KernelReadProcessMemory64 :TReadProcessMemory64;  
  KernelWriteProcessMemory:TWriteProcessMemory;
  KernelVirtualAllocEx    :TVirtualAllocEx;

  IsValidHandle           :TIsValidHandle;
  GetIDTCurrentThread     :TGetIDTCurrentThread;
  GetIDTs                 :TGetIDTs;
  MakeWritable            :TMakeWritable;
  GetLoadedState          :TGetLoadedState;

  DBKSuspendThread        :TDBKSuspendThread;
  DBKResumeThread         :TDBKResumeThread;
  DBKSuspendProcess       :TDBKSuspendProcess;
  DBKResumeProcess        :TDBKResumeProcess;

  KernelAlloc             :TKernelAlloc;
  KernelAlloc64           :TKernelAlloc64;  
  GetKProcAddress         :TGetKProcAddress;
  GetKProcAddress64       :TGetKProcAddress64;

  GetSDTEntry             :TGetSDTEntry;
  GetSSDTEntry            :TGetSSDTEntry;

  isDriverLoaded          :TisDriverLoaded;
  LaunchDBVM              :TLaunchDBVM;

  ReadPhysicalMemory      :TReadProcessMemory;
  WritePhysicalMemory     :TWriteProcessMemory;


  CreateRemoteAPC         :TCreateRemoteAPC;
  GetGDT                  :TGetGDT;


  DBKDebug_ContinueDebugEvent : TDBKDebug_ContinueDebugEvent;
  DBKDebug_WaitForDebugEvent  : TDBKDebug_WaitForDebugEvent;
  DBKDebug_GetDebuggerState   : TDBKDebug_GetDebuggerState;
  DBKDebug_SetDebuggerState   : TDBKDebug_SetDebuggerState;
  DBKDebug_SetGlobalDebugState: TDBKDebug_SetGlobalDebugState;
  DBKDebug_StartDebugging     : TDBKDebug_StartDebugging;
  DBKDebug_StopDebugging      : TDBKDebug_StopDebugging;
  DBKDebug_GD_SetBreakpoint   : TDBKDebug_GD_SetBreakpoint;


  //dbvm ce000000+
  dbvm_version            :Tdbvm_version;
  dbvm_changeselectors    :Tdbvm_changeselectors;
  dbvm_block_interrupts   :Tdbvm_block_interrupts;
  dbvm_restore_interrupts :Tdbvm_restore_interrupts;
  dbvm_raise_privilege    :Tdbvm_raise_privilege;
  //dbvm ce000004+
  dbvm_read_physical_memory: Tdbvm_read_physical_memory;
  dbvm_write_physical_memory: Tdbvm_write_physical_memory;

var WindowsKernel: Thandle;
    DarkByteKernel: Thandle;

    Usephysical: boolean;
    UseFileAsMemory: boolean;
    usephysicaldbvm: boolean;
    usedbkquery:boolean;
    DBKReadWrite: boolean;

    DenyList:boolean;
    DenyListGlobal: boolean;
    ModuleListSize: integer;
    ModuleList: pointer;


implementation

uses
     {$ifdef cemain}
     plugin,
     dbvmPhysicalMemoryHandler, //'' for physical mem
     {$endif}
     filehandler; //so I can let readprocessmemory point to ReadProcessMemoryFile in filehandler


function Is64bitOS: boolean;
var iswow64: BOOL;
begin
  {$ifndef CPU64}
  result:=false;
  if assigned(IsWow64Process) then
  begin

    iswow64:=false;
    if IsWow64Process(GetCurrentProcess,iswow64) and iswow64 then
      result:=true;
  end;
  {$else}
  result:=true; //only a 64-bit os can run 64-bit apps
  {$endif}
end;

function Is64BitProcess(processhandle: THandle): boolean;
var iswow64: BOOL;
begin
  result:=true;
  if Is64bitOS then
  begin
    iswow64:=false;
    if IsWow64Process(processhandle,iswow64) then
    begin
      if iswow64 then
        result:=false; //running in 32-bit mode

    end
    else
      result:=false; //IsWo64Process failed, happens on OS'es that don't have this api implemented

  end else result:=false; //32-bit can't run 64
end;


function loaddbvmifneeded: BOOL;  stdcall;
var signed: BOOL;
begin
  result:=false;
  if Is64bitOS and (not isRunningDBVM) then
  begin
    if isDBVMCapable then
    begin
      signed:=false;
      if isDriverLoaded(@signed) then
      begin
        if MessageDlg('To use this function in 64-bit you will need to run DBVM. There is a high chance running DBVM can crash your system and make you lose your data(So don''t forget to save first). Do you want to run DBVM?', mtWarning, [mbyes,mbno],0)=mryes then
        begin
          LaunchDBVM;
          if not isRunningDBVM then raise exception.Create('I don''t know what you did, you didn''t crash, but you also didn''t load DBVM');
          result:=true;
        end;
      end else
      begin
        //the driver isn't loaded
        if signed then
        begin
          raise exception.Create('Please reboot and press f8 before windows boots. Then enable unsigned drivers. Alternatively, you could buy yourself a business class certificicate and sign the driver yourself (or try debug signing)');
        end
        else
        begin
          raise exception.Create('The driver needs to be loaded to be able to use this function.');
        end;
      end;
    end else raise exception.Create('Your cpu must be able to run dbvm to use this function in 64-bit');
  end
  else result:=true;
end;

function isRunningDBVM: boolean;
begin
  result:=assigned(dbvm_version) and (dbvm_version>0);
end;

function isDBVMCapable: boolean;
var a,b,c,d: dword;
begin
  result:=false;
  if not isRunningDBVM then
  begin
    asm

      push {$ifdef cpu64}rax{$else}eax{$endif}
      push {$ifdef cpu64}rbx{$else}ebx{$endif}
      push {$ifdef cpu64}rcx{$else}ecx{$endif}
      push {$ifdef cpu64}rdx{$else}edx{$endif}
      mov eax,0
      cpuid
      mov a,eax
      mov b,ebx
      mov c,ecx
      mov d,edx
      pop {$ifdef cpu64}rdx{$else}edx{$endif}
      pop {$ifdef cpu64}rcx{$else}ecx{$endif}
      pop {$ifdef cpu64}rbx{$else}ebx{$endif}
      pop {$ifdef cpu64}rax{$else}eax{$endif}
    end;

    //GenuineIntel check
    if (b=$756e6547) and (d=$49656e69) and (c=$6c65746e) then
    begin
      //it's an intel
      asm
        push {$ifdef cpu64}rax{$else}eax{$endif}
        push {$ifdef cpu64}rbx{$else}ebx{$endif}
        push {$ifdef cpu64}rcx{$else}ecx{$endif}
        push {$ifdef cpu64}rdx{$else}edx{$endif}
        mov eax,1
        cpuid
        mov a,eax
        mov b,ebx
        mov c,ecx
        mov d,edx
        pop {$ifdef cpu64}rdx{$else}edx{$endif}
        pop {$ifdef cpu64}rcx{$else}ecx{$endif}
        pop {$ifdef cpu64}rbx{$else}ebx{$endif}
        pop {$ifdef cpu64}rax{$else}eax{$endif}
      end;

      if ((c shr 5) and 1)=1 then //check for the intel-vt flag
        result:=true;
    end;

  end
  else result:=true; //dbvm might tell the system it's not vm-x capable, getting the dbvm version will show you if that's fake or not

end;

procedure LoadDBK32; stdcall;
begin
  if DarkByteKernel=0 then
  begin
    DarkByteKernel:= LoadLibrary(dbkdll);
    if DarkByteKernel=0 then exit; //raise exception.Create('Failed to open DBK32.dll');

    //the driver is loaded (I hope)

    KernelVirtualAllocEx:=GetProcAddress(darkbytekernel,'VAE');
    KernelOpenProcess:=GetProcAddress(darkbytekernel,'OP');
    KernelReadProcessMemory:=GetProcAddresS(darkbytekernel,'RPM');
    KernelReadProcessMemory64:=GetProcAddresS(darkbytekernel,'RPM64');    
    KernelWriteProcessMemory:=GetProcAddress(darkbytekernel,'WPM');
    ReadProcessMemory64:=GetProcAddress(DarkByteKernel,'RPM64');
    WriteProcessMemory64:=GetProcAddress(DarkByteKernel,'WPM64');

    GetPEProcess:=GetProcAddress(DarkByteKernel,'GetPEProcess');
    GetPEThread:=GetProcAddress(DarkByteKernel,'GetPEThread');
    GetProcessnameOffset:=GetProcAddress(DarkByteKernel,'GetProcessnameOffset');
    GetThreadsProcessOffset:=GetProcAddress(DarkByteKernel,'GetThreadsProcessOffset');
    GetThreadListEntryOffset:=GetProcAddress(DarkByteKernel,'GetThreadListEntryOffset');
    GetDebugportOffset:=GetProcAddresS(DarkByteKernel,'GetDebugportOffset');
    GetPhysicalAddress:=GetProcAddresS(DarkByteKernel,'GetPhysicalAddress');
    GetCR4:=GetProcAddress(DarkByteKernel,'GetCR4');
    GetCR3:=GetProcAddress(DarkByteKernel,'GetCR3');
    SetCR3:=GetProcAddress(DarkByteKernel,'SetCR3');
    GetCR0:=GetProcAddress(DarkByteKernel,'GetCR0');    
    GetSDT:=GetProcAddress(DarkByteKernel,'GetSDT');
    GetSDTShadow:=GetProcAddress(DarkByteKernel,'GetSDTShadow');

//    setAlternateDebugMethod:=GetProcAddress(DarkByteKernel,'setAlternateDebugMethod');
//    getAlternateDebugMethod:=GetProcAddress(DarkByteKernel,'getAlternateDebugMethod');
//    DebugProcess:=GetProcAddress(DarkByteKernel,'DebugProcess');
//    StopDebugging:=GetProcAddress(DarkByteKernel,'StopDebugging');
//    StopRegisterChange:=GetProcAddress(DarkByteKernel,'StopRegisterChange');
//    RetrieveDebugData:=GetProcAddress(DarkByteKernel,'RetrieveDebugData');
//    ChangeRegOnBP:=GetProcAddress(DarkByteKernel,'ChangeRegOnBP');
    StartProcessWatch:=GetProcAddress(DarkByteKernel,'StartProcessWatch');
    WaitForProcessListData:=GetProcAddress(DarkByteKernel,'WaitForProcessListData');
    GetProcessNameFromID:=GetProcAddress(DarkByteKernel,'GetProcessNameFromID');
    GetProcessNameFromPEProcess:=GetProcAddress(DarkByteKernel,'GetProcessNameFromPEProcess');
    IsValidHandle:=GetProcAddress(DarkByteKernel,'IsValidHandle');
    GetIDTs:=GetProcAddress(DarkByteKernel,'GetIDTs');

    GetIDTCurrentThread:=GetProcAddress(DarkByteKernel,'GetIDTCurrentThread');
    GetGDT:=GetProcAddress(DarkByteKernel,'GetGDT');
    MakeWritable:=GetProcAddress(DarkByteKernel,'MakeWritable');
    GetLoadedState:=GetProcAddress(darkbytekernel,'GetLoadedState');

    DBKResumeThread:=GetProcAddress(darkByteKernel,'DBKResumeThread');
    DBKSuspendThread:=GetProcAddress(darkByteKernel,'DBKSuspendThread');

    DBKResumeProcess:=GetProcAddress(darkByteKernel,'DBKResumeProcess');
    DBKSuspendProcess:=GetProcAddress(darkByteKernel,'DBKSuspendProcess');

    KernelAlloc:=GetProcAddress(darkbyteKernel,'KernelAlloc');
    KernelAlloc64:=GetProcAddress(darkbyteKernel,'KernelAlloc64');    
    GetKProcAddress:=GetProcAddress(darkbytekernel,'GetKProcAddress');
    GetKProcAddress64:=GetProcAddress(darkbytekernel,'GetKProcAddress64');    

    GetSDTEntry:= GetProcAddress(darkbyteKernel,'GetSDTEntry');
    GetSSDTEntry:=GetProcAddress(darkbyteKernel,'GetSSDTEntry');

    isDriverLoaded:=GetProcAddress(darkbyteKernel,'isDriverLoaded');
    LaunchDBVM:=GetProcAddress(darkbyteKernel,'LaunchDBVM');

    ReadPhysicalMemory:=GetProcAddress(DarkByteKernel,'ReadPhysicalMemory');
    WritePhysicalMemory:=GetProcAddress(DarkByteKernel,'WritePhysicalMemory');

    CreateRemoteAPC:=GetProcAddress(darkByteKernel,'CreateRemoteAPC');
//    SetGlobalDebugState:=GetProcAddress(DarkByteKernel,'SetGlobalDebugState');

    DBKDebug_ContinueDebugEvent:=GetProcAddress(DarkByteKernel,'DBKDebug_ContinueDebugEvent');
    DBKDebug_WaitForDebugEvent:=GetProcAddress(DarkByteKernel,'DBKDebug_WaitForDebugEvent');
    DBKDebug_GetDebuggerState:=GetProcAddress(DarkByteKernel,'DBKDebug_GetDebuggerState');
    DBKDebug_SetDebuggerState:=GetProcAddress(DarkByteKernel,'DBKDebug_SetDebuggerState');

    DBKDebug_SetGlobalDebugState:=GetProcAddress(DarkByteKernel,'DBKDebug_SetGlobalDebugState');
    DBKDebug_StartDebugging:=GetProcAddress(DarkByteKernel,'DBKDebug_StartDebugging');
    DBKDebug_StopDebugging:=GetProcAddress(DarkByteKernel,'DBKDebug_StopDebugging');
    DBKDebug_GD_SetBreakpoint:=GetProcAddress(DarkByteKernel,'DBKDebug_GD_SetBreakpoint');

    dbvm_version:=GetProcAddress(DarkByteKernel,'dbvm_version');
    dbvm_changeselectors:=GetProcAddress(DarkByteKernel,'dbvm_changeselectors');
    dbvm_block_interrupts:=GetProcAddress(DarkByteKernel,'dbvm_block_interrupts');
    dbvm_restore_interrupts:=GetProcAddress(DarkByteKernel,'dbvm_restore_interrupts');

    dbvm_read_physical_memory:=GetProcAddress(DarkByteKernel,'dbvm_read_physical_memory');
    dbvm_write_physical_memory:=GetProcAddress(DarkByteKernel,'dbvm_write_physical_memory');

    dbvm_raise_privilege:=GetProcAddress(DarkByteKernel,'dbvm_raise_privilege');

    {$ifdef cemain}
    if pluginhandler<>nil then
      pluginhandler.handlechangedpointers(0);
    {$endif}

  end;
end;


procedure DBKFileAsMemory; overload;
{Changes the redirection of ReadProcessMemory, WriteProcessMemory and VirtualQueryEx to FileHandler.pas's ReadProcessMemoryFile, WriteProcessMemoryFile and VirtualQueryExFile }
begin
  UseFileAsMemory:=true;
  usephysical:=false;
  Usephysicaldbvm:=false;
  ReadProcessMemory:=@ReadProcessMemoryFile;
  WriteProcessMemory:=@WriteProcessMemoryFile;
  VirtualQueryEx:=@VirtualQueryExFile;


  {$ifdef cemain}
  if pluginhandler<>nil then
    pluginhandler.handlechangedpointers(3);
  {$endif}
end;

procedure DBKFileAsMemory(filename:string); overload;
begin
  filehandle:=CreateFile(pchar(filename),GENERIC_READ	or GENERIC_WRITE,FILE_SHARE_READ or FILE_SHARE_WRITE,nil,OPEN_EXISTING,FILE_FLAG_RANDOM_ACCESS,0);
  if filehandle=0 then raise exception.create(filename+' couldn''t be opened');
  DBKFileAsMemory;
end;

function VirtualQueryExPhysical(hProcess: THandle; lpAddress: Pointer; var lpBuffer: TMemoryBasicInformation; dwLength: DWORD): DWORD; stdcall;
var buf:_MEMORYSTATUS;
begin
  GlobalMemoryStatus(buf);

  lpBuffer.BaseAddress:=pointer((dword(lpAddress) div $1000)*$1000);
  lpbuffer.AllocationBase:=lpbuffer.BaseAddress;
  lpbuffer.AllocationProtect:=PAGE_EXECUTE_READWRITE;
  lpbuffer.RegionSize:=buf.dwTotalPhys-dword(lpBuffer.BaseAddress);
  lpbuffer.RegionSize:=lpbuffer.RegionSize+($1000-lpbuffer.RegionSize mod $1000);

  lpbuffer.State:=mem_commit;
  lpbuffer.Protect:=PAGE_EXECUTE_READWRITE;
  lpbuffer._Type:=MEM_PRIVATE;

  if (dword(lpAddress)>buf.dwTotalPhys) //bigger than the total ammount of memory
  then
  begin
    zeromemory(@lpbuffer,dwlength);
    result:=0
  end
  else
    result:=dwlength;

end;

procedure DBKPhysicalMemoryDBVM;
{Changes the redirection of ReadProcessMemory, WriteProcessMemory and VirtualQueryEx to dbvm's read/write physical memory}
begin
{$ifdef cemain}
  UseFileAsMemory:=false;
  usephysical:=false;
  usephysicaldbvm:=true;
  ReadProcessMemory:=@ReadProcessMemoryPhys;
  WriteProcessMemory:=@WriteProcessMemoryPhys;
  VirtualQueryEx:=@VirtualQueryExPhys;


  if pluginhandler<>nil then
    pluginhandler.handlechangedpointers(3);

{$endif}
end;

procedure DBKPhysicalMemory;
begin
  LoadDBK32;
  If DarkByteKernel=0 then exit;

  UsePhysical:=true;
  Usephysicaldbvm:=false;
  if usefileasmemory then closehandle(filehandle);
  usefileasmemory:=false;
  ReadProcessMemory:=GetProcAddress(DarkByteKernel,'ReadPhysicalMemory');
  WriteProcessMemory:=GetProcAddress(DarkByteKernel,'WritePhysicalMemory');
  VirtualQueryEx:=@VirtualQueryExPhysical;


  {$ifdef cemain}
  if pluginhandler<>nil then
    pluginhandler.handlechangedpointers(4);
  {$endif}

end;

procedure DBKProcessMemory;
begin
  if dbkreadwrite then
    UseDBKReadWriteMemory
  else
    dontUseDBKReadWriteMemory;

  if usedbkquery then
    Usedbkquerymemoryregion
  else
    dontusedbkquerymemoryregion;

  usephysical:=false;
  Usephysicaldbvm:=false;

  if usefileasmemory then closehandle(filehandle);
  usefileasmemory:=false;

end;



procedure DontUseDBKQueryMemoryRegion;
{Changes the redirection of VirtualQueryEx back to the windows API virtualQueryEx}
begin
  VirtualQueryEx:=GetProcAddress(WindowsKernel,'VirtualQueryEx');
  usedbkquery:=false;
  if usephysicaldbvm then DbkPhysicalMemoryDBVM;
  if usephysical then DbkPhysicalMemory;
  if usefileasmemory then dbkfileasmemory;

  {$ifdef cemain}
  if pluginhandler<>nil then
    pluginhandler.handlechangedpointers(5);
  {$endif}

end;

procedure UseDBKQueryMemoryRegion;
{Changes the redirection of VirtualQueryEx to the DBK32 equivalent}
begin
  LoadDBK32;
  If DarkByteKernel=0 then exit;
  UseDBKOpenProcess;
  VirtualQueryEx:=GetProcAddress(DarkByteKernel,'VQE');
  usedbkquery:=true;

  if usephysical then DbkPhysicalMemory;
  if usephysicaldbvm then DBKPhysicalMemoryDBVM;
  if usefileasmemory then dbkfileasmemory;


  {$ifdef cemain}
  if pluginhandler<>nil then
    pluginhandler.handlechangedpointers(6);
  {$endif}

end;

procedure DontUseDBKReadWriteMemory;
{Changes the redirection of ReadProcessMemory and WriteProcessMemory back to the windows API ReadProcessMemory and WriteProcessMemory }
begin
  DBKReadWrite:=false;
  ReadProcessMemory:=GetProcAddress(WindowsKernel,'ReadProcessMemory');
  WriteProcessMemory:=GetProcAddress(WindowsKernel,'WriteProcessMemory');
  VirtualAllocEx:=GetProcAddress(WindowsKernel,'VirtualAllocEx');
  if usephysical then DbkPhysicalMemory;
  if usephysicaldbvm then DBKPhysicalMemoryDBVM;
  if usefileasmemory then dbkfileasmemory;

  {$ifdef cemain}
  if pluginhandler<>nil then
    pluginhandler.handlechangedpointers(7);
  {$endif}

end;

procedure UseDBKReadWriteMemory;
{Changes the redirection of ReadProcessMemory, WriteProcessMemory and VirtualQueryEx to the DBK32 equiv: RPM, WPM and VAE }
begin
  LoadDBK32;
  If DarkByteKernel=0 then exit;
  UseDBKOpenProcess;
  ReadProcessMemory:=GetProcAddress(DarkByteKernel,'RPM');
  WriteProcessMemory:=GetProcAddress(DarkByteKernel,'WPM');
  VirtualAllocEx:=GetProcAddress(DarkByteKernel,'VAE');
  DBKReadWrite:=true;
  if usephysical then DbkPhysicalMemory;
  if usephysicaldbvm then DBKPhysicalMemoryDBVM;
  if usefileasmemory then dbkfileasmemory;

  {$ifdef cemain}
  if pluginhandler<>nil then
    pluginhandler.handlechangedpointers(8);
  {$endif}


end;

procedure DontUseDBKOpenProcess;
{Changes the redirection of OpenProcess and VirtualAllocEx  back to the windows API OpenProcess and VirtualAllocEx }
begin
  OpenProcess:=GetProcAddress(WindowsKernel,'OpenProcess');
  OpenThread:=GetProcAddress(WindowsKernel,'OpenThread');

  {$ifdef cemain}
  pluginhandler.handlechangedpointers(9);
  {$endif}

end;

procedure UseDBKOpenProcess;
begin
  LoadDBK32;
  If DarkByteKernel=0 then exit;
  OpenProcess:=GetProcAddress(DarkByteKernel,'OP'); //gives back the real handle, or if it fails it gives back a value only valid for the dll
  OpenThread:=GetProcAddress(DarkByteKernel,'OT');

  {$ifdef cemain}
  pluginhandler.handlechangedpointers(10);
  {$endif}

end;

procedure OutputDebugString(msg: string);
begin
//{$ifdef DEBUG}
  windows.outputdebugstring(pchar(msg));
//{$endif}
end;

var x: string;
initialization
  DarkByteKernel:=0;

  usephysical:=false;
  Usephysicaldbvm:=false;
  usefileasmemory:=false;
  usedbkquery:=false;

  DenyList:=true;
  DenyListGlobal:= false;
  ModuleListSize:= 0;
  ModuleList:= nil;
  Denylist:= false;
  //globaldenylist:= false;

  WindowsKernel:=LoadLibrary('Kernel32.dll'); //there is no kernel33.dll
  if WindowsKernel=0 then Raise Exception.create('Something is really messed up on your computer! You don''t seems to have a kernel!!!!');

  //by default point to these exports:
  ReadProcessMemory:=GetProcAddress(WindowsKernel,'ReadProcessMemory');
  WriteProcessMemory:=GetProcAddress(WindowsKernel,'WriteProcessMemory');

  OpenProcess:=GetProcAddress(WindowsKernel,'OpenProcess');

  VirtualQueryEx:=GetProcAddress(WindowsKernel,'VirtualQueryEx');
  VirtualAllocEx:=GetProcAddress(WindowsKernel,'VirtualAllocEx');


  GetThreadContext:=GetProcAddress(WindowsKernel,'GetThreadContext');
  SetThreadContext:=GetProcAddress(WindowsKernel,'SetThreadContext');
  SuspendThread:=GetProcAddress(WindowsKernel,'SuspendThread');
  ResumeThread:=GetProcAddress(WindowsKernel,'ResumeThread');
  WaitForDebugEvent:=GetProcAddress(WindowsKernel,'WaitForDebugEvent');
  ContinueDebugEvent:=GetProcAddress(WindowsKernel,'ContinueDebugEvent');
  DebugActiveProcess:=GetProcAddress(WindowsKernel,'DebugActiveProcess');
  VirtualProtect:=GetProcAddress(WindowsKernel,'VirtualProtect');
  VirtualProtectEx:=GetProcAddress(WindowsKernel,'VirtualProtectEx');
  CreateRemoteThread:=GetProcAddress(WindowsKernel,'CreateRemoteThread');
  OpenThread:=GetProcAddress(WindowsKernel,'OpenThread');

  CreateToolhelp32Snapshot:=GetProcAddress(WindowsKernel, 'CreateToolhelp32Snapshot');

  Process32First:=   GetProcAddress(WindowsKernel, 'Process32First');
  Process32Next:=    GetProcAddress(WindowsKernel, 'Process32Next');
  Thread32First:=    GetProcAddress(WindowsKernel, 'Thread32First');
  Thread32Next:=     GetProcAddress(WindowsKernel, 'Thread32Next');
  Module32First:=    GetProcAddress(WindowsKernel, 'Module32First');
  Module32Next:=     GetProcAddress(WindowsKernel, 'Module32Next');
  Heap32ListFirst:=  GetProcAddress(WindowsKernel, 'Heap32ListFirst');
  Heap32ListNext:=   GetProcAddress(WindowsKernel, 'Heap32ListNext');

  IsWow64Process:=   GetProcAddress(WindowsKernel, 'IsWow64Process');

finalization

end.
