unit NewKernelHandler;

interface
uses windows,sysutils,tlhelp32;

const dbkdll='DBK32.dll';

type TReadProcessMemory=function(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer;  nSize: DWORD; var lpNumberOfBytesRead: DWORD): BOOL; stdcall;
type TWriteProcessMemory=function(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesWritten: DWORD): BOOL; stdcall;
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
type TStopDebugging=function: BOOL; stdcall;
type TStopRegisterChange=function(regnr:integer):BOOL; stdcall;
type TVirtualProtect=function(lpAddress: Pointer; dwSize, flNewProtect: DWORD; var OldProtect: DWORD): BOOL; stdcall;
type TVirtualProtectEx=function(hProcess: THandle; lpAddress: Pointer; dwSize, flNewProtect: DWORD; var OldProtect: DWORD): BOOL; stdcall;
type TVirtualQueryEx=function(hProcess: THandle; lpAddress: Pointer; var lpBuffer: TMemoryBasicInformation; dwLength: DWORD): DWORD; stdcall;
type TVirtualAllocEx=function(hProcess: THandle; lpAddress: Pointer; dwSize, flAllocationType: DWORD; flProtect: DWORD): Pointer; stdcall;
type TCreateRemoteThread=function(hProcess: THandle; lpThreadAttributes: Pointer; dwStackSize: DWORD; lpStartAddress: TFNThreadStartRoutine; lpParameter: Pointer;  dwCreationFlags: DWORD; var lpThreadId: DWORD): THandle; stdcall;
type TOpenThread=function(dwDesiredAccess:DWORD;bInheritHandle:BOOL;dwThreadId:DWORD):THANDLE; stdcall;
type TGetPEProcess=function(ProcessID:DWORD):DWORD; stdcall;
type TGetPEThread=function(Threadid: dword):dword; stdcall;
type TGetDebugportOffset=function:DWORD; stdcall;
type TGetProcessnameOffset=function:DWORD; stdcall;
type TGetThreadsProcessOffset=function: dword; stdcall;
type TGetThreadListEntryOffset=function: dword; stdcall;
type TSetGlobalDebugState=function(state: boolean): BOOL; stdcall;

type TGetPhysicalAddress=function(hProcess:THandle;lpBaseAddress:pointer;var Address:int64): BOOL; stdcall;
type TProtectMe=function(ProtectedProcessID: dword; denylist,globaldenylist:BOOL;list:pchar; listsize:dword):BOOL; stdcall;
type TGetCR4=function:DWORD; stdcall;
type TGetCR3=function(hProcess:THANDLE;var CR3: DWORD):BOOL; stdcall;
type TSetCR3=function(hProcess:THANDLE;CR3: DWORD):BOOL; stdcall;
type TGetCR0=function:DWORD; stdcall;
type TGetSDT=function:DWORD; stdcall;
type TGetSDTShadow=function:DWORD; stdcall;


type TCreateRemoteAPC=function(threadid: dword; lpStartAddress: TFNAPCProc): THandle; stdcall;


type TsetAlternateDebugMethod=function(var int1apihook:dword; var OriginalInt1handler:dword):BOOL; stdcall;
type TgetAlternateDebugMethod=function:BOOL; stdcall;

type TChangeRegOnBP=function(Processid:dword; address: dword; debugreg: integer; changeEAX,changeEBX,changeECX,changeEDX,changeESI,changeEDI,changeEBP,changeESP,changeEIP,changeCF,changePF,changeAF,changeZF,changeSF,changeOF:BOOLEAN; newEAX,newEBX,newECX,newEDX,newESI,newEDI,newEBP,newESP,newEIP:DWORD; newCF,newPF,newAF,newZF,newSF,newOF:BOOLEAN):BOOLEAN; stdcall;
type TDebugProcess=function(processid:dword;address:DWORD;size: byte;debugtype:byte):BOOL; stdcall;
type TRetrieveDebugData=function(Buffer: pointer):integer; stdcall;

type TGetProcessNameFromID=function(processid:dword; buffer:pchar;buffersize:dword):integer; stdcall;
type TGetProcessNameFromPEProcess=function(peprocess:dword; buffer:pchar;buffersize:dword):integer; stdcall;

type TStartProcessWatch=function:BOOL;stdcall;
type TWaitForProcessListData=function(processpointer:pointer;threadpointer:pointer;timeout:dword):dword; stdcall;

type TIsValidHandle=function(hProcess:THandle):BOOL; stdcall;
type TGetIDTCurrentThread=function:dword; stdcall;
type TGetIDTs=function(idtstore: pointer; maxidts: integer):integer; stdcall;
type TMakeWritable=function(Address,Size:dword;copyonwrite:boolean): boolean; stdcall;
type TGetLoadedState=function : BOOLEAN; stdcall;

type TDBKTest=function:boolean; stdcall;
type TDBKSuspendThread=function(ThreadID:dword):boolean; stdcall;
type TDBKResumeThread=function(ThreadID:dword):boolean; stdcall;
type TDBKSuspendProcess=function(ProcessID:dword):boolean; stdcall;
type TDBKResumeProcess=function(ProcessID:dword):boolean; stdcall;

type TKernelAlloc=function(size: dword):pointer; stdcall;
type TGetKProcAddress=function(s: pwidechar):pointer; stdcall;

type TGetSDTEntry=function (nr: integer; address: PDWORD; paramcount: PBYTE):boolean; stdcall;
type TSetSDTEntry=function (nr: integer; address: DWORD; paramcount: BYTE):boolean; stdcall;
type TGetSSDTEntry=function (nr: integer; address: PDWORD; paramcount: PBYTE):boolean; stdcall;
type TSetSSDTEntry=function (nr: integer; address: DWORD; paramcount: BYTE):boolean; stdcall;
type TGetGDT=function(var limit: word):dword; stdcall;

type TuseIOCTL=procedure(use: boolean); stdcall;
type TMakeKernelCopy=function(Base: dword; size: dword): bool; stdcall;

//-----------------------------------DBVM-------------------------------------//
type Tdbvm_version=function: dword; stdcall;
type Tdbvm_changeselectors=function(cs,ss,ds,es,fs,gs: dword): DWORD; stdcall;
type Tdbvm_restore_interrupts=function: DWORD; stdcall;
type Tdbvm_block_interrupts=function: DWORD; stdcall;



procedure DONTUseDBKQueryMemoryRegion;
procedure DONTUseDBKReadWriteMemory;
procedure DONTUseDBKOpenProcess;
procedure UseDBKQueryMemoryRegion;
procedure UseDBKReadWriteMemory;
procedure UseDBKOpenProcess;

procedure DBKFileAsMemory(filename:string); overload;
procedure DBKFileAsMemory; overload;
procedure DBKPhysicalMemory;
procedure DBKProcessMemory;
procedure LoadDBK32;

Procedure ProtectProcess(processid: dword);
Procedure ProtectCE;

//I could of course have made it a parameter thing, but I'm lazy

var
  ReadProcessMemory     :TReadProcessMemory;
  WriteProcessMemory    :TWriteProcessMemory;
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
  StopDebugging         :TStopDebugging;
  StopRegisterChange    :TStopRegisterChange;
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
  ProtectMe             :TProtectMe;
  GetCR4                :TGetCR4;
  GetCR3                :TGetCR3;
  SetCR3                :TSetCR3;
  GetCR0                :TGetCR0;
  GetSDT                :TGetSDT;
  GetSDTShadow          :TGetSDTShadow;

  setAlternateDebugMethod: TsetAlternateDebugMethod;
  getAlternateDebugMethod: TgetAlternateDebugMethod;

  SetGlobalDebugState   :TSetGlobalDebugState;
  DebugProcess          :TDebugProcess;
  ChangeRegOnBP         :TChangeRegOnBP;
  RetrieveDebugData     :TRetrieveDebugData;
  StartProcessWatch     :TStartProcessWatch;
  WaitForProcessListData:TWaitForProcessListData;
  GetProcessNameFromID  :TGetProcessNameFromID;
  GetProcessNameFromPEProcess:TGetProcessNameFromPEProcess;


  KernelOpenProcess       :TOpenProcess;
  KernelReadProcessMemory :TReadProcessMemory;
  KernelWriteProcessMemory:TWriteProcessMemory;
  KernelVirtualAllocEx    :TVirtualAllocEx;

  IsValidHandle           :TIsValidHandle;
  GetIDTCurrentThread     :TGetIDTCurrentThread;
  GetIDTs                 :TGetIDTs;
  MakeWritable            :TMakeWritable;
  GetLoadedState          :TGetLoadedState;

  dbktest                 :TDBKTest;
  DBKSuspendThread        :TDBKSuspendThread;
  DBKResumeThread         :TDBKResumeThread;
  DBKSuspendProcess       :TDBKSuspendProcess;
  DBKResumeProcess        :TDBKResumeProcess;

  KernelAlloc             :TKernelAlloc;
  GetKProcAddress         :TGetKProcAddress;

  GetSDTEntry             :TGetSDTEntry;
  SetSDTEntry             :TSetSDTEntry;
  GetSSDTEntry            :TGetSSDTEntry;
  SetSSDTEntry            :TSetSSDTEntry;

  ReadPhysicalMemory      :TReadProcessMemory;
  WritePhysicalMemory     :TWriteProcessMemory;


  CreateRemoteAPC         :TCreateRemoteAPC;
  GetGDT                  :TGetGDT;

  useIOCTL                :TuseIOCTL;
  MakeKernelCopy          :TMakeKernelCopy;

  //dbvm
  dbvm_version            :Tdbvm_version;
  dbvm_changeselectors    :Tdbvm_changeselectors;
  dbvm_block_interrupts   :Tdbvm_block_interrupts;
  dbvm_restore_interrupts :Tdbvm_restore_interrupts;

var WindowsKernel: Thandle;
    DarkByteKernel: Thandle;

    Usephysical: boolean;
    UseFileAsMemory: boolean;
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
     {$endif}
     filehandler; //so I can let readprocessmemory point to ReadProcessMemoryFile in filehandler

procedure LoadDBK32;
begin
  if DarkByteKernel=0 then
  begin
    DarkByteKernel:= LoadLibrary(dbkdll);
    if DarkByteKernel=0 then exit; //raise exception.Create('Failed to open DBK32.dll');

    //the driver is loaded (I hope)

    KernelVirtualAllocEx:=GetProcAddress(darkbytekernel,'VAE');
    KernelOpenProcess:=GetProcAddress(darkbytekernel,'OP');
    KernelReadProcessMemory:=GetProcAddresS(darkbytekernel,'RPM');
    KernelWriteProcessMemory:=GetProcAddress(darkbytekernel,'WPM');

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

    setAlternateDebugMethod:=GetProcAddress(DarkByteKernel,'setAlternateDebugMethod');
    getAlternateDebugMethod:=GetProcAddress(DarkByteKernel,'getAlternateDebugMethod');
    DebugProcess:=GetProcAddress(DarkByteKernel,'DebugProcess');
    StopDebugging:=GetProcAddress(DarkByteKernel,'StopDebugging');
    StopRegisterChange:=GetProcAddress(DarkByteKernel,'StopRegisterChange');
    RetrieveDebugData:=GetProcAddress(DarkByteKernel,'RetrieveDebugData');
    ChangeRegOnBP:=GetProcAddress(DarkByteKernel,'ChangeRegOnBP');
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
    DBKTest:=GetProcAddress(darkbytekernel,'test');
    useIOCTL:=GetProcAddress(darkbytekernel,'useIOCTL');

    DBKResumeThread:=GetProcAddress(darkByteKernel,'DBKResumeThread');
    DBKSuspendThread:=GetProcAddress(darkByteKernel,'DBKSuspendThread');

    DBKResumeProcess:=GetProcAddress(darkByteKernel,'DBKResumeProcess');
    DBKSuspendProcess:=GetProcAddress(darkByteKernel,'DBKSuspendProcess');

    KernelAlloc:=GetProcAddress(darkbyteKernel,'KernelAlloc');
    GetKProcAddress:=GetProcAddress(darkbytekernel,'GetKProcAddress');

    GetSDTEntry:= GetProcAddress(darkbyteKernel,'GetSDTEntry');
    SetSDTEntry:= GetProcAddress(darkbyteKernel,'SetSDTEntry');
    GetSSDTEntry:=GetProcAddress(darkbyteKernel,'GetSSDTEntry');
    SetSSDTEntry:=GetProcAddress(darkbyteKernel,'SetSSDTEntry');

    ReadPhysicalMemory:=GetProcAddress(DarkByteKernel,'ReadPhysicalMemory');
    WritePhysicalMemory:=GetProcAddress(DarkByteKernel,'WritePhysicalMemory');

    MakeKernelCopy:=GetProcAddress(DarkByteKernel,'MakeKernelCopy');
    CreateRemoteAPC:=GetProcAddress(darkByteKernel,'CreateRemoteAPC');
    SetGlobalDebugState:=GetProcAddress(DarkByteKernel,'SetGlobalDebugState');

    dbvm_version:=GetProcAddress(DarkByteKernel,'dbvm_version');
    dbvm_changeselectors:=GetProcAddress(DarkByteKernel,'dbvm_changeselectors');
    dbvm_block_interrupts:=GetProcAddress(DarkByteKernel,'dbvm_block_interrupts');
    dbvm_restore_interrupts:=GetProcAddress(DarkByteKernel,'dbvm_restore_interrupts');    

    {$ifdef cemain}
    if pluginhandler<>nil then
      pluginhandler.handlechangedpointers(0);
    {$endif}

  end;
end;

Procedure ProtectProcess(processid: dword);
var list:pointer;
    listsize:pointer;
begin
  LoadDBK32;
  If DarkByteKernel=0 then LoadDBK32;
  If DarkByteKernel=0 then exit;

  ProtectMe:=GetProcAddress(DarkByteKernel,'ProtectMe');
  ProtectMe(processid,denylist,DenyListGlobal,modulelist,modulelistsize);

  {$ifdef cemain}
  if pluginhandler<>nil then
    pluginhandler.handlechangedpointers(1);
  {$endif}

end;


Procedure ProtectCE;
var list:pointer;
    listsize:pointer;
begin
  LoadDBK32;
  If DarkByteKernel=0 then LoadDBK32;
  If DarkByteKernel=0 then exit;

  ProtectMe:=GetProcAddress(DarkByteKernel,'ProtectMe');
  ProtectMe(getcurrentprocessid,denylist,DenyListGlobal,modulelist,modulelistsize);

  {$ifdef cemain}
  if pluginhandler<>nil then
    pluginhandler.handlechangedpointers(2);
  {$endif}

end;

procedure DBKFileAsMemory; overload;
{Changes the redirection of ReadProcessMemory, WriteProcessMemory and VirtualQueryEx to FileHandler.pas's ReadProcessMemoryFile, WriteProcessMemoryFile and VirtualQueryExFile }
begin
  UseFileAsMemory:=true;
  usephysical:=false;
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
  lpbuffer.Type_9:=MEM_PRIVATE;

  if (dword(lpAddress)>buf.dwTotalPhys) //bigger than the total ammount of memory
  then
  begin
    zeromemory(@lpbuffer,dwlength);
    result:=0
  end
  else
    result:=dwlength;

end;

procedure DBKPhysicalMemory;
begin
  LoadDBK32;
  If DarkByteKernel=0 then exit;

  UsePhysical:=true;
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

  if usefileasmemory then closehandle(filehandle);
  usefileasmemory:=false;

end;



procedure DontUseDBKQueryMemoryRegion;
{Changes the redirection of VirtualQueryEx back to the windows API virtualQueryEx}
begin
  VirtualQueryEx:=GetProcAddress(WindowsKernel,'VirtualQueryEx');
  usedbkquery:=false;
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
var x: pointer;
begin
  LoadDBK32;
  If DarkByteKernel=0 then exit;
  OpenProcess:=GetProcAddress(DarkByteKernel,'OP'); //gives back the real handle, or if it fails it gives back a value only valid for the dll
  OpenThread:=GetProcAddress(DarkByteKernel,'OT');

  {$ifdef cemain}
  pluginhandler.handlechangedpointers(10);
  {$endif}

end;
var x: string;

initialization
  DarkByteKernel:=0;

  usephysical:=false;
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
