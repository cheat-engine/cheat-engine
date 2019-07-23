unit vmxfunctions;

interface

{$mode DELPHI}

uses jwawindows, windows, classes, dialogs, sysutils;

const
  VMCALL_GETVERSION=0;
  VMCALL_CHANGEPASSWORD=1;
  VMCALL_READPHYSICALMEMORY=3;
  VMCALL_WRITEPHYSICALMEMORY=4;
  VMCALL_REDIRECTINT1=9;
  VMCALL_INT1REDIRECTED=10;
  VMCALL_CHANGESELECTORS=12;
  VMCALL_BLOCK_INTERRUPTS=13;
  VMCALL_RESTORE_INTERRUPTS=14;
  VMCALL_GETCR0=18;
  VMCALL_GETCR3=19;
  VMCALL_GETCR4=20;
  VMCALL_RAISEPRIVILEGE=21;
  VMCALL_REDIRECTINT14=22;
  VMCALL_INT14REDIRECTED=23;
  VMCALL_REDIRECTINT3=24;
  VMCALL_INT3REDIRECTED=25;
  VMCALL_READMSR=26;
  VMCALL_WRITEMSR=27;

  VMCALL_SWITCH_TO_KERNELMODE=30;

  VMCALL_DISABLE_DATAPAGEFAULTS= 31;
  VMCALL_ENABLE_DATAPAGEFAULTS= 32;
  VMCALL_GETLASTSKIPPEDPAGEFAULT =33;

  VMCALL_ULTIMAP_PAUSE =34;
  VMCALL_ULTIMAP_RESUME= 35;

  VMCALL_ULTIMAP_DEBUGINFO = 36;
  VMCALL_TESTPSOD = 37;

  //dbvm 11
  VMCALL_GETMEM = 38;
  VMCALL_JTAGBP = 39;
  VMCALL_GETNMICOUNT = 40;

  VMCALL_WATCH_WRITES = 41;
  VMCALL_WATCH_READS = 42;
  VMCALL_WATCH_RETRIEVELOG = 43;
  VMCALL_WATCH_DELETE = 44;

  VMCALL_CLOAK_ACTIVATE = 45;
  VMCALL_CLOAK_DEACTIVATE = 46;
  VMCALL_CLOAK_READORIGINAL = 47;
  VMCALL_CLOAK_WRITEORIGINAL = 48;

  VMCALL_CLOAK_CHANGEREGONBP = 49;
  VMCALL_CLOAK_REMOVECHANGEREGONBP = 50;

  VMCALL_EPT_RESET = 51;   //removes all watches cloaks, and changereg bp's

  VMCALL_LOG_CR3VALUES_START = 52;
  VMCALL_LOG_CR3VALUES_STOP = 53;

  VMCALL_REGISTERPLUGIN = 54;
  VMCALL_RAISEPMI = 55;
  VMCALL_ULTIMAP2_HIDERANGEUSAGE = 56;

  VMCALL_ADD_MEMORY = 57;
  VMCALL_DISABLE_EPT = 58;

  VMCALL_GET_STATISTICS = 59;
  VMCALL_WATCH_EXECUTES = 60;

  VMCALL_SETTSCADJUST = 61;
  VMCALL_SETSPEEDHACK = 62;



  //---
  //watch options:
  EPTO_MULTIPLERIP =1 shl 0; //log the same RIP multiple times (if different registers)
  EPTO_LOG_ALL     =1 shl 1; //log every access in the page
  EPTO_SAVE_FXSAVE =1 shl 2; //logs contain the xsave state
  EPTO_SAVE_STACK  =1 shl 3; //logs contain a 4kb stack snapshot
  EPTO_PMI_WHENFULL=1 shl 4; //Trigger a performance monitor interrupt when full (only use when you have a kernelmode driver)
  EPTO_GROW_WHENFULL=1 shl 5; //Grow if the given size is too small (beware, if DBVM runs out of memory, your system will crash)
  EPTO_INTERRUPT   =1 shl 6; //Trigger a debug interrupt when hit, no logging

type
  TOriginalState=packed record
    oldflags: dword;
    oldcs, oldss, oldds, oldes, oldfs, oldgs: word;
  end;
  POriginalState=^TOriginalState;

  TULTIMAPDEBUGINFO=packed record
    Active: QWORD; //set to 1 when active
    CR3: QWORD; //Holds the CR3 value to watch taskswitch to and from
    DEBUGCTL:QWORD; //Holds the DebugCTL value to set when inside the target process
    DS_AREA:QWORD; //Holds the DS_AREA to set when
    OriginalDebugCTL :QWORD; //When inside the target process this holds the debugctl that was set before entering. Return this on readMSR (and set with writeMSR when inside the process)
    OriginalDS_AREA :QWORD; //When inside the target process this holds the DS_AREA that was set before entering. Return this with readMSR ('''')
    CR3_switchcount:QWORD;
    CR3_switchcount2:QWORD;
    LastOldCR3:QWORD;
    LastNewCR3:QWORD;
    cpunr: QWORD;
  end;
  PULTIMAPDEBUGINFO=^TULTIMAPDEBUGINFO;

  TPageEventBasic=record
    VirtualAddress: QWORD;
    PhysicalAddress: QWORD;
    CR3: QWORD;
    FSBASE: QWORD;
    GSBASE: QWORD;
    FLAGS: QWORD;
    RAX: QWORD;
    RBX: QWORD;
    RCX: QWORD;
    RDX: QWORD;
    RSI: QWORD;
    RDI: QWORD;
    R8: QWORD;
    R9: QWORD;
    R10: QWORD;
    R11: QWORD;
    R12: QWORD;
    R13: QWORD;
    R14: QWORD;
    R15: QWORD;
    RBP: QWORD;
    RSP: QWORD;
    RIP: QWORD;
    CS: WORD;
    DS: WORD;
    ES: WORD;
    SS: WORD;
    FS: WORD;
    GS: WORD;
    Count: DWORD;
  end;

  TPageEventBasicArray=array [0..0] of TPageEventBasic;
  PPageEventBasic=^TPageEventBasic;

  TFXSAVE64=packed record
    FCW: WORD;
    FSW: WORD;
    FTW: BYTE;
    Reserved: BYTE;
    FOP: WORD;
    FPU_IP: UINT64;
    FPU_DP: UINT64;
    MXCSR: DWORD;
    MXCSR_MASK: DWORD;
    FP_MM0: QWORD;
    FP_MM0_H: QWORD;
    FP_MM1: QWORD;
    FP_MM1_H: QWORD;
    FP_MM2: QWORD;
    FP_MM2_H: QWORD;
    FP_MM3: QWORD;
    FP_MM3_H: QWORD;
    FP_MM4: QWORD;
    FP_MM4_H: QWORD;
    FP_MM5: QWORD;
    FP_MM5_H: QWORD;
    FP_MM6: QWORD;
    FP_MM6_H: QWORD;
    FP_MM7: QWORD;
    FP_MM7_H: QWORD;
    XMM0: QWORD;
    XMM0_H: QWORD;
    XMM1: QWORD;
    XMM1_H: QWORD;
    XMM2: QWORD;
    XMM2_H: QWORD;
    XMM3: QWORD;
    XMM3_H: QWORD;
    XMM4: QWORD;
    XMM4_H: QWORD;
    XMM5: QWORD;
    XMM5_H: QWORD;
    XMM6: QWORD;
    XMM6_H: QWORD;
    XMM7: QWORD;
    XMM7_H: QWORD;
    XMM8: QWORD;
    XMM8_H: QWORD;
    XMM9: QWORD;
    XMM9_H: QWORD;
    XMM10: QWORD;
    XMM10_H: QWORD;
    XMM11: QWORD;
    XMM11_H: QWORD;
    XMM12: QWORD;
    XMM12_H: QWORD;
    XMM13: QWORD;
    XMM13_H: QWORD;
    XMM14: QWORD;
    XMM14_H: QWORD;
    XMM15: QWORD;
    XMM15_H: QWORD;
    res1: QWORD;
    res1_H: QWORD;
    res2: QWORD;
    res2_H: QWORD;
    res3: QWORD;
    res3_H: QWORD;
    res4: QWORD;
    res4_H: QWORD;
    res5: QWORD;
    res5_H: QWORD;
    res6: QWORD;
    res6_H: QWORD;
  end;

  PFXSAVE64=^TFXSAVE64;

  TPageEventExtended=record
    basic: TPageEventBasic;
    fpudata: TFXSAVE64;
  end;
  TPageEventExtendedArray=array [0..0] of TPageEventExtended;
  PPageEventExtended=^TPageEventExtended;

  TPageEventBasicWithStack=record
    basic: TPageEventBasic;
    stack: array [0..4095] of byte;
  end;
  TPageEventBasicWithStackArray=array [0..0] of TPageEventBasicWithStack;
  PPageEventBasicWithStack=^TPageEventBasicWithStack;

  TPageEventExtendedWithStack=record
    basic: TPageEventBasic;
    fpudata: TFXSAVE64;
    stack: array [0..4095] of byte;
  end;
  TPageEventExtendedWithStackArray=array [0..0] of TPageEventExtendedWithStack;
  PPageEventExtendedWithStack=^TPageEventExtendedWithStack;

  PPageEventBasicArray=^TPageEventBasicArray;
  PPageEventExtendedArray=^TPageEventExtendedArray;
  PPageEventBasicWithStackArray=^TPageEventBasicWithStackArray;
  PPageEventExtendedWithStackArray=^TPageEventExtendedWithStackArray;

  TPageEventListDescriptor=packed record
    ID: DWORD;
    maxSize: DWORD ;
    numberOfEntries: DWORD ;
    missedEntries: DWORD;
    entryType: DWORD ;
    reserved: DWORD;
    //followed by results

    //case integer of
//      0: (basic:     array [0..0] of TPageEventBasic);
//      1: (extended:  TPageEventBasic);//TPageEventExtendedArray);
//      2: (basics:    TPageEventBasicStackArray);
//      3: (extendeds: TPageEventExtendedStackArray);
  end;

  PPageEventListDescriptor=^TPageEventListDescriptor;

  TChangeRegOnBPInfo=packed record
    Flags: bitpacked record
      changeRAX: 0..1;        //0
      changeRBX: 0..1;        //1
      changeRCX: 0..1;        //2
      changeRDX: 0..1;        //3
      changeRSI: 0..1;        //4
      changeRDI: 0..1;        //5
      changeRBP: 0..1;        //6
      changeRSP: 0..1;        //7
      changeRIP: 0..1;        //8
      changeR8:  0..1;        //9
      changeR9:  0..1;        //10
      changeR10: 0..1;        //11
      changeR11: 0..1;        //12
      changeR12: 0..1;        //13
      changeR13: 0..1;        //14
      changeR14: 0..1;        //15
      changeR15: 0..1;        //16
      changeCF: 0..1;         //17
      changePF: 0..1;         //18
      changeAF: 0..1;         //19
      changeZF: 0..1;         //20
      changeSF: 0..1;         //21
      changeOF: 0..1;         //22
      newCF: 0..1;            //23
      newPF: 0..1;            //24
      newAF: 0..1;            //25
      newZF: 0..1;            //26
      newSF: 0..1;            //27
      newOF: 0..1;            //28
      reserved: 0..7;         //29,30,31
    end;

    newRAX: QWORD;
    newRBX: QWORD;
    newRCX: QWORD;
    newRDX: QWORD;
    newRSI: QWORD;
    newRDI: QWORD;
    newRBP: QWORD;
    newRSP: QWORD;
    newRIP: QWORD;
    newR8:  QWORD;
    newR9:  QWORD;
    newR10: QWORD;
    newR11: QWORD;
    newR12: QWORD;
    newR13: QWORD;
    newR14: QWORD;
    newR15: QWORD;

  end;

  TDBVMStatistics=packed record
    eventCountersCurrentCPU: array [0..55] of integer;
    eventCountersAllCPUS: array [0..55] of integer;
  end;

  TDBVMBreakpoint=record
    VirtualAddress: qword;
    PhysicalAddress: qword;
    breakoption: integer; //1: changeregon bp , nothing else yet
    originalbyte: byte; //in case of breakoption0 (changeregonbp)
  end;
  PDBVMBreakpoint=^TDBVMBreakpoint;


function dbvm_version: dword; stdcall;
function dbvm_changepassword(password1,password2: dword):dword; stdcall;
function dbvm_changeselectors(cs,ss,ds,es,fs,gs: dword): DWORD; stdcall;
function dbvm_restore_interrupts: DWORD; stdcall;
function dbvm_block_interrupts: DWORD; stdcall;
function dbvm_redirect_interrupt1(redirecttype: integer; newintvector: dword; int1cs: dword; int1eip: dword): dword; stdcall;
function dbvm_read_physical_memory(PhysicalAddress: UINT64; destination: pointer; size: integer): dword; stdcall;
function dbvm_write_physical_memory(PhysicalAddress: UINT64; source: pointer; size: integer): dword; stdcall;
function dbvm_raise_privilege: DWORD; stdcall;

function dbvm_readMSR(msr: dword): QWORD;
procedure dbvm_writeMSR(msr: dword; value: qword);

function dbvm_getRealCR0: QWORD;
function dbvm_getRealCR3: QWORD;
function dbvm_getRealCR4: QWORD;

function dbvm_ultimap_pause: DWORD;
function dbvm_ultimap_resume: DWORD;
function dbvm_ultimap_debuginfo(debuginfo: PULTIMAPDEBUGINFO): DWORD;

procedure dbvm_switchToKernelMode(cs: word; rip: pointer; parameters: pointer);

function dbvm_getMemory(var pages: QWORD): QWORD;
function dbvm_jtagbp: boolean;
function dbvm_getNMIcount: QWORD;
procedure dbvm_psod;

procedure dbvm_enterkernelmode(originalstate: POriginalState);
procedure dbvm_returntousermode(originalstate: POriginalState);
function dbvm_kernelalloc(size: dword): pointer;
function dbvm_copyMemory(destination, target: pointer; size: integer): boolean;

//got lost since last harddisk crash. Not 'that' important, but will take a while to reimplement
function dbvm_executeDriverEntry(driverentry: pointer; DriverObject: pointer; RegistryPath: pointer): integer;


function dbvm_executeDispatchIoctl(DispatchIoctl: pointer; DriverObject: pointer; dwIoControlCode: DWORD; lpInBuffer: pointer; nInBufferSize:integer; lpOutBuffer: pointer; nOutBufferSize: integer; lpBytesReturned: pdword): BOOL;
function dbvm_testSwitchToKernelmode: integer; //returns 123 on success

function dbvm_getProcAddress(functionname: string): pointer;

procedure dbvm_testPSOD;

function dbvm_watch_writes(PhysicalAddress: QWORD; size: integer; Options: DWORD; MaxEntryCount: Integer): integer;
function dbvm_watch_reads(PhysicalAddress: QWORD; size: integer; Options: DWORD; MaxEntryCount: Integer): integer;
function dbvm_watch_executes(PhysicalAddress: QWORD; size: integer; Options: DWORD; MaxEntryCount: Integer): integer;
function dbvm_watch_retrievelog(ID: integer; results: PPageEventListDescriptor; var resultsize: integer): integer;
function dbvm_watch_delete(ID: integer): boolean;

function dbvm_cloak_activate(PhysicalBase: QWORD; virtualAddress: Qword=0): integer;
function dbvm_cloak_deactivate(PhysicalBase: QWORD): boolean;
function dbvm_cloak_readoriginal(PhysicalBase: QWORD; destination: pointer): integer;
function dbvm_cloak_writeoriginal(PhysicalBase: QWORD; source: pointer): integer;

function dbvm_cloak_changeregonbp(PhysicalAddress: QWORD; var changeregonbpinfo: TChangeRegOnBPInfo; VirtualAddress: qword=0): integer;
function dbvm_cloak_removechangeregonbp(PhysicalAddress: QWORD): integer;

procedure dbvm_ept_reset;

function dbvm_get_statistics(out statistics: TDBVMStatistics):qword;

procedure dbvm_setTSCAdjust(enabled: boolean; timeout: integer);
procedure dbvm_speedhack_setSpeed(speed: double);


function dbvm_log_cr3values_start: boolean;
function dbvm_log_cr3values_stop(log: pointer): boolean;

function dbvm_registerPlugin(pluginaddress: pointer; pluginsize: integer; plugintype: integer): integer;
procedure dbvm_raisePMI;
procedure dbvm_ultimap2_hideRangeUsage;

procedure configure_vmx(userpassword1,userpassword2: dword);
procedure configure_vmx_kernel;

function ReadProcessMemoryWithCloakSupport(hProcess: THandle; lpBaseAddress, lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesRead: PTRUINT): BOOL; stdcall;
function WriteProcessMemoryWithCloakSupport(hProcess: THandle; lpBaseAddress, lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesWritten: PTRUINT): BOOL; stdcall;
function hasCloakedRegionInRange(virtualAddress: qword; size: integer; out VA:qword; out PA: qword): boolean;

procedure dbvm_getBreakpointList(l: TStrings);
function dbvm_isBreakpoint(virtualAddress: ptruint; out physicalAddress: qword; out breakoption: integer; var originalbyte: byte): boolean;

var
  vmx_password1: dword;
  vmx_password2: dword;

  vmx_enabled: boolean;

  vmx_loaded: boolean;

  //dbvmversion: integer=0;

implementation

uses DBK32functions, cefuncproc, PEInfoFunctions, NewKernelHandler, syncobjs,
  ProcessHandlerUnit, Globals, AvgLvlTree, maps, debuggertypedefinitions,
  DebugHelper, frmBreakpointlistunit;

resourcestring
rsInvalidInstruction = 'Invalid instruction';
rsBigError = 'Error';
rsSmallError = 'error';

var vmcall :function(vmcallinfo:pointer; level1pass: dword): PtrUInt; stdcall;
var vmcall2 :function(vmcallinfo:pointer; level1pass: dword; secondaryOut: pptruint): PtrUInt; stdcall;


  cloakedregionsCS: TCriticalSection=nil;

  cloakedregions: array of record //using an array instead of map as there will likely be less than 1000 entries
        VirtualAddress: qword;
        PhysicalAddress: qword;
      end;

  cloakedregioncache: tmap;

  hassetbp: boolean;
  breakpoints: array of TDBVMBreakpoint;
  breakpointsCS: TCriticalSection=nil;

type
  TCloakedMemInfo=record
    memory: array [0..4095] of byte;
    time: qword;
  end;
  PCloakedMemInfo=^TCloakedMemInfo;

procedure flushCloakedMemoryCache(address: ptruint=0);
var
  mi: TMapIterator;
  cmi: PCloakedMemInfo;
  id: qword;
begin
  address:=address and MAXPHYADDRMASKPB;

  if cloakedregioncache<>nil then
  begin
    if address=0 then
    begin
      mi:=TMapIterator.Create(cloakedregioncache);
      mi.First;
      while not mi.EOM do
      begin
        mi.GetData(cmi);
        freemem(cmi);
        mi.Next;
      end;

      mi.free;
      cloakedregioncache.Clear;
    end
    else
    begin
      if cloakedregioncache.GetData(address,cmi) then
      begin
        freemem(cmi);
        cloakedregioncache.Delete(address);
      end;
    end;
  end;
end;

function getCloakedMemory(PhysicalAddress: qword; VirtualAddress: ptruint; destination: pointer; size: integer): integer;
//read the dbvm cloaked memory (assuming it is cloaked) and returns the number of bytes read. (can be less than size)
var
  d: PCloakedMemInfo;
  offset: integer;
begin
  //use a cache (2 sec old) if possible
  if cloakedregioncache=nil then
  begin
    cloakedregionsCS.enter;
    if cloakedregioncache=nil then
      cloakedregioncache:=TMap.Create(itu8,sizeof(PCloakedMemInfo));

    cloakedregionsCS.Leave;
  end;

  offset:=VirtualAddress and $fff;
  size:=min(size,4096-offset);

  if cloakedregioncache.GetData(PhysicalAddress, d)=false then
  begin
    //create
    cloakedregionsCS.enter;
    if cloakedregioncache.GetData(PhysicalAddress, d)=false then
    begin
      getmem(d,sizeof(TCloakedMemInfo));
      cloakedregioncache.Add(PhysicalAddress, d);
      d^.time:=0;
    end;
    cloakedregionsCS.Leave;
  end;

  if GetTickCount64>d^.time+2000 then
  begin
    //update
    dbvm_cloak_readoriginal(PhysicalAddress,@d^.memory[0]);
    d^.time:=GetTickCount64;
  end;

  copymemory(destination, @d^.memory[offset],size);
  exit(size);
end;

function hasCloakedRegionInRange(virtualAddress: qword; size: integer; out VA:qword; out PA: qword): boolean;
var i: integer;
begin
  result:=false;
  if cloakedregionsCS=nil then exit;

  cloakedregionsCS.enter;
  try
    for i:=0 to length(cloakedregions)-1 do
    begin
      if (cloakedregions[i].VirtualAddress<(virtualAddress+size)) and
         (virtualAddress<(cloakedregions[i].VirtualAddress+4096)) then
      begin
        //inside the region
        result:=true;
        VA:=cloakedregions[i].VirtualAddress;
        PA:=cloakedregions[i].PhysicalAddress;
        exit;
      end;
    end;
  finally
    cloakedregionsCS.leave;
  end;
end;

function ReadProcessMemoryWithCloakSupport(hProcess: THandle; lpBaseAddress, lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesRead: PTRUINT): BOOL; stdcall;
var
  VA,PA: qword;
  x: ptruint;
  i: integer;
begin
  if hasCloakedRegionInRange(qword(lpBaseAddress),nsize, VA, PA) then
  begin
    result:=true;
    if VA>qword(lpBaseAddress) then
    begin
      //the first bytes are not cloaked
      i:=VA-qword(lpBaseAddress);
      i:=min(nsize,i);
      x:=0;
      result:=ReadProcessMemory(processhandle,lpBaseAddress, lpBuffer, i, x);

      lpNumberOfBytesRead:=x;

      if (x<>i) or (result=false) then
        exit(result);

      lpBaseAddress:=pointer(ptruint(lpBaseAddress)+i);
      nSize:=nsize-i;
      lpBuffer:=pointer(ptruint(lpBuffer)+i);
    end;

    if nsize>0 then
    begin
      //get the executable memory
      x:=getCloakedMemory(PA, ptruint(lpBaseAddress), lpBuffer, min(4096,nsize));  //cached
      inc(lpNumberOfBytesRead, x);

      lpBaseAddress:=pointer(ptruint(lpBaseAddress)+x);
      nSize:=nsize-x;
      lpBuffer:=pointer(ptruint(lpBuffer)+x);

      if (nsize>0) then
      begin
        //get the memory after (use ReadProcessMemoryWithCloakSupport)
        x:=0;
        result:=ReadProcessMemoryWithCloakSupport(processhandle,lpBaseAddress, lpBuffer, nsize, x);
        inc(lpNumberOfBytesRead,x);
      end;
    end;
  end
  else
    result:=ReadProcessMemory(processhandle, lpBaseAddress, lpBuffer, nSize, lpNumberOfBytesRead);
end;

function WriteProcessMemoryWithCloakSupport(hProcess: THandle; lpBaseAddress, lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesWritten: PTRUINT): BOOL; stdcall;
var
  VA,PA: qword;
  x: ptruint;
  buf: pointer;
  i: integer;
begin
  if hasCloakedRegionInRange(qword(lpBaseAddress),nsize, VA, PA) then
  begin
    result:=true;
    lpNumberOfBytesWritten:=0;
    if nsize=0 then exit(true);

    if VA>qword(lpBaseAddress) then
    begin
      //the first bytes are not cloaked
      i:=VA-qword(lpBaseAddress);
      i:=min(nsize,i);
      x:=0;
      result:=WriteProcessMemory(processhandle,lpBaseAddress, lpBuffer, i, x);
      lpNumberOfBytesWritten:=x;

      if (x<>i) or (result=false) then
        exit(result);

      lpBaseAddress:=pointer(ptruint(lpBaseAddress)+i);
      nSize:=nsize-i;
      lpBuffer:=pointer(ptruint(lpBuffer)+i);
    end;

    if nsize>0 then
    begin
      //write the executable memory  (lpBaseAddress should be on a pageboundary here)
      getmem(buf,4096);
      dbvm_cloak_readoriginal(PA, buf);
      i:=min(4096,nsize);
      copymemory(pointer(ptruint(buf)+(ptruint(lpBaseAddress)-VA)), lpBuffer, i);
      dbvm_cloak_writeoriginal(PA, buf);

      inc(lpNumberOfBytesWritten, i);
      lpBaseAddress:=pointer(ptruint(lpBaseAddress)+i);
      nSize:=nsize-i;
      lpBuffer:=pointer(ptruint(lpBuffer)+i);

      if (nsize>0) then
      begin
        x:=0;
        result:=WriteProcessMemoryWithCloakSupport(processhandle,lpBaseAddress, lpBuffer, nsize, x);
        inc(lpNumberOfBytesWritten,x);
      end;
    end;
  end
  else
    result:=WriteProcessMemory(processhandle, lpBaseAddress, lpBuffer, nSize, lpNumberOfBytesWritten);
end;


function vmcall2UnSupported(vmcallinfo:pointer; level1pass: dword; secondaryOut: pptruint): PtrUInt; stdcall;
begin
  result:=0;
end;


function vmcallUnSupported(vmcallinfo:pointer; level1pass: dword): PtrUInt; stdcall;
begin
  result:=0;
end;


procedure invalidinstruction;
begin
 // ShowMessage('Invalid instruction');
  raise exception.create(rsInvalidInstruction);
end;

function vmcallexceptiontest(ExceptionInfo: PEXCEPTION_POINTERS): LONG; stdcall;
begin
  result:=EXCEPTION_CONTINUE_SEARCH;

  if ExceptionInfo.ExceptionRecord.ExceptionCode=EXCEPTION_ILLEGAL_INSTRUCTION then
  begin
    ExceptionInfo.ContextRecord.{$ifdef cpu64}rip{$else}eip{$endif}:=ptruint(@invalidinstruction);

    {$ifdef cpu64}
    ExceptionInfo.ContextRecord.rsp:=(ExceptionInfo.ContextRecord.rsp-$20) and qword($fffffffffffffff0)-8;
    {$endif}
    result:=EXCEPTION_CONTINUE_EXECUTION;
  end;

end;

function vmcallSupported_amd(vmcallinfo:pointer; level1pass: dword): PtrUInt; stdcall;
var
{$ifdef cpu64}
  originalrdx: ptruint;
{$endif}
  r: ptruint;
begin
  asm
{$ifdef cpu64}
    mov originalrdx,rdx
    mov rax,vmcallinfo
    mov edx,level1pass
    vmmcall

    mov rdx,originalrdx
    mov r,rax
{$else}
    mov eax,vmcallinfo
    mov edx,level1pass
    vmmcall     //should raise an UD if the cpu does not support it  (or the password is wrong)
    mov r,eax
{$endif}
  end;

  result:=r;
end;

function vmcallSupported2_amd(vmcallinfo:pointer; level1pass: dword; output2: pptruint): PtrUInt; stdcall;
var
{$ifdef cpu64}
  originalrdx: ptruint;
{$endif}
  r,r2: ptruint;
begin
  asm
{$ifdef cpu64}
    mov originalrdx,rdx
    mov rax,vmcallinfo
    mov edx,level1pass
    vmmcall

    mov r2,rdx

    mov rdx,originalrdx
    mov r,rax
{$else}
    mov eax,vmcallinfo
    mov edx,level1pass
    vmmcall     //should raise an UD if the cpu does not support it  (or the password is wrong)
    mov r,eax
    mov r2,edx
{$endif}
  end;

  result:=r;
  if output2<>nil then
    output2^:=r2;
end;


function vmcallSupported2_intel(vmcallinfo:pointer; level1pass: dword; output2: pptruint): PtrUInt; stdcall;
var
{$ifdef cpu64}
  originalrdx: ptruint;
{$endif}
  r: ptruint;
  r2: ptruint;
begin
  asm
{$ifdef cpu64}
    mov originalrdx,rdx
    mov rax,vmcallinfo
    mov edx,level1pass
    vmcall

    mov r2,rdx

    mov rdx,originalrdx
    mov r,rax
{$else}
    mov eax,vmcallinfo
    mov edx,level1pass
    vmcall     //should raise an UD if the cpu does not support it  (or the password is wrong)
    mov r,eax
    mov r2,edx
{$endif}
  end;

  result:=r;
  if output2<>nil then
    output2^:=r2;
end;

function vmcallSupported_intel(vmcallinfo:pointer; level1pass: dword): PtrUInt; stdcall;
var
{$ifdef cpu64}
  originalrdx: ptruint;
{$endif}
  r: ptruint;
begin
  asm
{$ifdef cpu64}
    mov originalrdx,rdx
    mov rax,vmcallinfo
    mov edx,level1pass
    vmcall

    mov rdx,originalrdx
    mov r,rax
{$else}
    mov eax,vmcallinfo
    mov edx,level1pass
    vmcall     //should raise an UD if the cpu does not support it  (or the password is wrong)
    mov r,eax
{$endif}
  end;

  result:=r;
end;



function dbvm_version: dword; stdcall;
var vmcallinfo: record
  structsize: dword;
  level2pass: dword;
  command: dword;
end;
begin

  if (vmx_password1=0) and (vmx_password2=0) then
  begin
    //set the password if it was not set
    vmx_password1:=$76543210;
    vmx_password2:=$fedcba98;
  end;


  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_GETVERSION;
  try
    result:=vmcall(@vmcallinfo,vmx_password1);

    if (result shr 24)<>$ce then
    begin
      //OutputDebugString('Invalid vmx');
      result:=0;
    end
    else
      vmx_loaded:=true;

  except
    result:=0;
  end;
end;

function dbvm_changepassword(password1,password2: dword): DWORD; stdcall;
var vmcallinfo: record
  structsize: dword;
  level2pass: dword;
  command: dword;
  password1: dword;
  password2: dword;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_CHANGEPASSWORD;
  vmcallinfo.password1:=password1;
  vmcallinfo.password2:=password2;
  try
    result:=vmcall(@vmcallinfo,vmx_password1);
    vmx_password1:=password1;
    vmx_password2:=password2;
  except
    result:=$ffffffff;
  end;
end;

function dbvm_changeselectors(cs,ss,ds,es,fs,gs: dword): DWORD; stdcall;
var vmcallinfo: record
  structsize: dword;
  level2pass: dword;
  command: dword;
  cs,ss,ds,es,fs,gs: dword;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_CHANGESELECTORS;
  vmcallinfo.cs:=cs;
  vmcallinfo.ss:=ss;
  vmcallinfo.ds:=ds;
  vmcallinfo.es:=es;
  vmcallinfo.fs:=fs;
  vmcallinfo.gs:=gs;

  result:=vmcall(@vmcallinfo,vmx_password1);
end;

function dbvm_redirect_interrupt1(redirecttype: integer; newintvector: dword; int1cs: dword; int1eip: dword): dword; stdcall;
var vmcallinfo: record
  structsize: dword;
  level2pass: dword;
  command: dword;
  redirecttype: dword;
  intvector: dword;
  eip: uint64;
  cs: dword;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_REDIRECTINT1;
  vmcallinfo.redirecttype:=redirecttype;
  vmcallinfo.intvector:=newintvector;
  vmcallinfo.eip:=int1eip;
  vmcallinfo.cs:=int1cs;
  
  try
    result:=vmcall(@vmcallinfo,vmx_password1);
  except
    result:=$ffffffff;
  end;
end;

function dbvm_block_interrupts: DWORD; stdcall;
var vmcallinfo: record
  structsize: dword;
  level2pass: dword;
  command: dword;
  end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_BLOCK_INTERRUPTS;

  result:=vmcall(@vmcallinfo,vmx_password1);
end;

function dbvm_restore_interrupts: DWORD; stdcall;
var vmcallinfo: record
  structsize: dword;
  level2pass: dword;
  command: dword;
  end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_RESTORE_INTERRUPTS;
  result:=vmcall(@vmcallinfo,vmx_password1);
end;


function dbvm_write_physical_memory(PhysicalAddress: UINT64; source: pointer; size: integer): dword; stdcall;
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
  destinationPA: UINT64;
  size: dword;
  sourceVA: UINT64;
  nopagefault: dword;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_WRITEPHYSICALMEMORY;
  vmcallinfo.destinationPA:=PhysicalAddress;
  vmcallinfo.size:=size;
  vmcallinfo.sourceVA:=ptrUint(source);
  vmcallinfo.nopagefault:=0; //I like pagefaults
  
  try
    result:=size-vmcall(@vmcallinfo,vmx_password1);
  except
    result:=0;
  end;
end;

function dbvm_read_physical_memory(PhysicalAddress: UINT64; destination: pointer; size: integer): dword; stdcall;
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
  sourcePA: UINT64;
  size: dword;
  destinationVA: UINT64;
  nopagefault: dword;
end;
begin
  ZeroMemory(@vmcallinfo,sizeof(vmcallinfo));
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_READPHYSICALMEMORY;
  vmcallinfo.sourcePA:=PhysicalAddress;
  vmcallinfo.size:=size;
  vmcallinfo.destinationVA:=ptrUint(destination);
  vmcallinfo.nopagefault:=0; //I like pagefaults
  
  try
    result:=size-vmcall(@vmcallinfo,vmx_password1);  //it returns the number of bytes left
  except
    result:=0; //read 0 bytes
   // messagebox(0,pchar(rsBigError),pchar(rsSmallError),mb_ok);
  end;
end;

function dbvm_raise_privilege: DWORD; stdcall;
{
NEEDS interrupts being disabled first (taskswitch would set it back to normal)
Returns 0 if success, 1 if interrupts are not disabled, -1 is no dbvm
}
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_RAISEPRIVILEGE;
  result:=vmcall(@vmcallinfo,vmx_password1);
end;

function dbvm_ultimap_debuginfo(debuginfo: PULTIMAPDEBUGINFO): DWORD;
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
  debuginfo: TULTIMAPDEBUGINFO;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_ULTIMAP_DEBUGINFO;
  result:=vmcall(@vmcallinfo,vmx_password1);

  debuginfo^:=vmcallinfo.debuginfo;
end;

function dbvm_ultimap_resume: DWORD;
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_ULTIMAP_RESUME;
  result:=vmcall(@vmcallinfo,vmx_password1);
end;

function dbvm_ultimap_pause: DWORD;
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_ULTIMAP_PAUSE;
  result:=vmcall(@vmcallinfo,vmx_password1);
end;

procedure dbvm_testPSOD;
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_TESTPSOD;
  vmcall(@vmcallinfo,vmx_password1);
end;

function dbvm_readMSR(msr: dword): QWORD;
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
  msr: dword;
  msrreturn: qword;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_READMSR;
  vmcallinfo.msr:=msr;
  result:=vmcall(@vmcallinfo,vmx_password1);
end;

procedure dbvm_writeMSR(msr: dword; value: qword);
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
  msr: dword;
  msrvalue: qword;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_WRITEMSR;
  vmcallinfo.msr:=msr;
  vmcallinfo.msrvalue:=value;
  vmcall(@vmcallinfo,vmx_password1);
end;

function dbvm_getMemory(var pages: QWORD): QWORD;
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_GETMEM;
  result:=vmcall2(@vmcallinfo,vmx_password1, @pages);
end;

function dbvm_jtagbp: boolean;
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_JTAGBP;
  result:=vmcall(@vmcallinfo,vmx_password1)<>0;
end;

procedure dbvm_psod;
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_TESTPSOD;
  vmcall(@vmcallinfo,vmx_password1);
end;


function dbvm_getNMIcount: QWORD;
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_GETNMICOUNT;
  result:=vmcall(@vmcallinfo,vmx_password1);
end;

function dbvm_getRealCR0: QWORD;
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_GETCR0;
  result:=vmcall(@vmcallinfo,vmx_password1);
end;

function dbvm_getRealCR3: QWORD;
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_GETCR3;
  result:=vmcall(@vmcallinfo,vmx_password1);
end;

function dbvm_getRealCR4: QWORD;
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_GETCR4;
  result:=vmcall(@vmcallinfo,vmx_password1);
end;

procedure dbvm_switchToKernelMode(cs: word; rip: pointer; parameters: pointer);
{
Will emulate a software interrupt that goes to the given cs:rip
Make sure cs:rip is paged in because paging is not possible until interrupts are enabled back again (so swapgs and sti as soon as possible)
}
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
  cs: dword;
  rip: qword;
  parameters: qword;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_SWITCH_TO_KERNELMODE;
  vmcallinfo.cs:=cs;
  vmcallinfo.rip:=ptruint(rip);
  vmcallinfo.parameters:=ptruint(parameters);
  vmcall(@vmcallinfo,vmx_password1);
end;


function dbvm_watch_writes(PhysicalAddress: QWORD; size: integer; Options: DWORD; MaxEntryCount: Integer): integer;
var vmcallinfo: packed record
      structsize: dword;   //0
      level2pass: dword;   //4
      command: dword;      //8
      PhysicalAddress: QWORD; //12
      Size: integer;          //20
      Options: DWORD;         //24
      MaxEntryCount: integer; //28
      ID: integer; //return value
    end;
    r: integer;
begin
  result:=-1;
  outputdebugstring('dbvm_watch_writes');
  options:=options and (not EPTO_PMI_WHENFULL); //make sure this is not used

  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_WATCH_WRITES;

  vmcallinfo.PhysicalAddress:=PhysicalAddress;
  vmcallinfo.Size:=size;
  vmcallinfo.Options:=Options;
  vmcallinfo.MaxEntryCount:=MaxEntryCount;
  vmcallinfo.ID:=-1;

  OutputDebugString('MaxEntryCount at offset '+inttostr(QWORD(@vmcallinfo.MaxEntryCount)-QWORD(@vmcallinfo)));

  OutputDebugString('vmcallinfo.MaxEntryCount='+inttostr(vmcallinfo.MaxEntryCount));
  r:=vmcall(@vmcallinfo,vmx_password1);
  OutputDebugString('r='+inttostr(r));

  if r=0 then
    result:=vmcallinfo.ID;

  OutputDebugString('returning '+inttostr(result));
end;

function dbvm_watch_reads(PhysicalAddress: QWORD; size: integer; Options: DWORD; MaxEntryCount: Integer): integer;
var vmcallinfo: packed record
      structsize: dword;   //0
      level2pass: dword;   //4
      command: dword;      //8
      PhysicalAddress: QWORD; //12
      Size: integer;          //20
      Options: DWORD;         //24
      MaxEntryCount: integer; //28
      ID: integer; //return value
    end;
    r: integer;
begin
  result:=-1;
  outputdebugstring('dbvm_watch_reads');
  options:=options and (not EPTO_PMI_WHENFULL); //make sure this is not used

  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_WATCH_READS;
  vmcallinfo.PhysicalAddress:=PhysicalAddress;
  vmcallinfo.Size:=size;
  vmcallinfo.Options:=Options;
  vmcallinfo.MaxEntryCount:=MaxEntryCount;
  vmcallinfo.ID:=-1;

  OutputDebugString('MaxEntryCount at offset '+inttostr(QWORD(@vmcallinfo.MaxEntryCount)-QWORD(@vmcallinfo)));

  OutputDebugString('vmcallinfo.MaxEntryCount='+inttostr(vmcallinfo.MaxEntryCount));
  r:=vmcall(@vmcallinfo,vmx_password1);
  OutputDebugString('r='+inttostr(r));

  if r=0 then
    result:=vmcallinfo.ID;

  OutputDebugString('returning '+inttostr(result));
end;

function dbvm_watch_executes(PhysicalAddress: QWORD; size: integer; Options: DWORD; MaxEntryCount: Integer): integer;
var vmcallinfo: packed record
      structsize: dword;   //0
      level2pass: dword;   //4
      command: dword;      //8
      PhysicalAddress: QWORD; //12
      Size: integer;          //20
      Options: DWORD;         //24
      MaxEntryCount: integer; //28
      ID: integer; //return value
    end;
    r: integer;
begin
  result:=-1;
  outputdebugstring(format('dbvm_watch_executes(%x,%d,%x,%d)',[PhysicalAddress, Size, Options, MaxEntryCount]));
  options:=options and (not EPTO_PMI_WHENFULL); //make sure this is not used

  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_WATCH_EXECUTES;
  vmcallinfo.PhysicalAddress:=PhysicalAddress;
  vmcallinfo.Size:=size;
  vmcallinfo.Options:=Options;
  vmcallinfo.MaxEntryCount:=MaxEntryCount;
  vmcallinfo.ID:=-1;

  OutputDebugString('MaxEntryCount at offset '+inttostr(QWORD(@vmcallinfo.MaxEntryCount)-QWORD(@vmcallinfo)));

  OutputDebugString('vmcallinfo.MaxEntryCount='+inttostr(vmcallinfo.MaxEntryCount));
  r:=vmcall(@vmcallinfo,vmx_password1);
  OutputDebugString('r='+inttostr(r));

  if r=0 then
    result:=vmcallinfo.ID;

  OutputDebugString('returning '+inttostr(result));
end;

function dbvm_watch_retrievelog(ID: integer; results: PPageEventListDescriptor; var resultsize: integer): integer;
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
  ID: DWORD;
  results: QWORD;
  resultssize: DWORD;
  copied: DWORD;
end;
begin
  //OutputDebugString('vmxfunctions.pas: dbvm_watch_retrievelog (results='+inttohex(QWORD(results),8)+' resultsize='+inttostr(resultsize)+')');
  result:=1;
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_WATCH_RETRIEVELOG;
  vmcallinfo.ID:=ID;
  vmcallinfo.results:=QWORD(results);
  vmcallinfo.resultssize:=resultsize;
  vmcallinfo.copied:=0;

  result:=vmcall(@vmcallinfo,vmx_password1);  //returns 2 on a too small size
  resultsize:=vmcallinfo.resultssize;

  //OutputDebugString('dbvm_watch_retrievelog vmcall returned '+inttostr(result)+'  (resultsize='+inttostr(resultsize)+')');

  resultsize:=vmcallinfo.resultssize;
end;

function dbvm_watch_delete(ID: integer): boolean;
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
  ID: DWORD;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_WATCH_DELETE;
  vmcallinfo.ID:=ID;
  result:=vmcall(@vmcallinfo,vmx_password1)=0;  //returns 0 on success
end;

function dbvm_cloak_activate(PhysicalBase: QWORD; virtualAddress: QWORD=0): integer;
var
  vmcallinfo: packed record
    structsize: dword;
    level2pass: dword;
    command: dword;
    PhysicalBase: QWORD;
  end;
  i: integer;
begin
  PhysicalBase:=PhysicalBase and MAXPHYADDRMASKPB;
  virtualAddress:=virtualAddress and qword($fffffffffffff000);

  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_CLOAK_ACTIVATE;
  vmcallinfo.PhysicalBase:=PhysicalBase;
  result:=vmcall(@vmcallinfo,vmx_password1);

  outputdebugstring('dbvm_cloak_activate: result='+inttostr(result));

  if ((result=0) or (result=1)) and (virtualAddress<>0) then
  begin


    cloakedregionscs.enter;

    outputdebugstring('It has a virtual address assigned');

    try
      if (result=1) then
      begin
        //already cloaked. First check if it's already in
        for i:=0 to length(cloakedregions)-1 do
          if cloakedregions[i].PhysicalAddress=PhysicalBase then exit;   //already in the list
      end;
      i:=length(cloakedregions);
      setlength(cloakedregions,i+1);
      cloakedregions[i].PhysicalAddress:=PhysicalBase;
      cloakedregions[i].virtualAddress:=virtualAddress;

      outputdebugstring('added it to entry '+inttostr(i));
    finally
      cloakedregionscs.leave;
    end;

  end;
end;

function dbvm_cloak_deactivate(PhysicalBase: QWORD): boolean;
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
  PhysicalBase: QWORD;
end;
i,j,k: integer;
begin
  PhysicalBase:=PhysicalBase and MAXPHYADDRMASKPB;
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_CLOAK_DEACTIVATE;
  vmcallinfo.PhysicalBase:=PhysicalBase;
  result:=vmcall(@vmcallinfo,vmx_password1)<>0;

  if result and (cloakedregionscs<>nil) then
  begin
    OutputDebugString('dbvm_cloak_deactivate');
    cloakedregionscs.enter;
    try
      for i:=0 to length(cloakedregions)-1 do
      begin
        if cloakedregions[i].PhysicalAddress=PhysicalBase then
        begin
          OutputDebugString(format('Found entry for virtual address %.8x and physical address %.8x',[cloakedregions[i].VirtualAddress, cloakedregions[i].PhysicalAddress]));
          for j:=i to length(cloakedregions)-2 do
            cloakedregions[i]:=cloakedregions[i+1];

          setlength(cloakedregions, length(cloakedregions)-1);

          //remove changeregonbp's from the list if it had one (already deleted in dbvm)
          outputdebugstring('Checking if there is a changereg at this location');

          breakpointscs.enter;
          try
            j:=0;
            outputdebugstring(format('length(breakpoints)=%d',[length(breakpoints)]));
            while j<length(breakpoints) do
            begin
              OutputDebugString(format('Is %.8x inside %.8x to %.8x? (BO=%d)',[breakpoints[j].PhysicalAddress, PhysicalBase,PhysicalBase+4095, breakpoints[j].breakoption]));

              if (breakpoints[j].breakoption=integer(bo_ChangeRegister)) and inrangex(breakpoints[j].PhysicalAddress, PhysicalBase, physicalbase+4095) then  //changeregonbp bp and inside this range
              begin
                OutputDebugString('Yes, deleting changeregonbp in this range');
                for k:=j to length(breakpoints)-2 do
                  breakpoints[k]:=breakpoints[k+1];

                setlength(breakpoints, length(breakpoints)-1);
              end
              else
              begin
                OutputDebugString('No');
                inc(j);
              end;
            end
          finally
            hassetbp:=length(breakpoints)<>0;
            breakpointscs.leave;
          end;

          exit;
        end;
      end;
    finally
      cloakedregionscs.leave;
    end;
  end;
end;

function dbvm_cloak_readoriginal(PhysicalBase: QWORD; destination: pointer): integer;
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
  PhysicalBase: QWORD;
  destination: qword;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_CLOAK_READORIGINAL;
  vmcallinfo.PhysicalBase:=PhysicalBase;
  vmcallinfo.destination:=qword(destination);
  result:=vmcall(@vmcallinfo,vmx_password1);
end;

function dbvm_cloak_writeoriginal(PhysicalBase: QWORD; source: pointer): integer;
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
  PhysicalBase: QWORD;
  source: qword;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_CLOAK_WRITEORIGINAL;
  vmcallinfo.PhysicalBase:=PhysicalBase;
  vmcallinfo.source:=qword(source);
  result:=vmcall(@vmcallinfo,vmx_password1);
end;


function dbvm_cloak_changeregonbp(PhysicalAddress: QWORD; var changeregonbpinfo: TChangeRegOnBPInfo; VirtualAddress: qword=0): integer;
var
  vmcallinfo: packed record
    structsize: dword;
    level2pass: dword;
    command: dword;
    PhysicalAddress: QWORD;
    changeregonbpinfo: TChangeRegOnBPInfo;
  end;

  PhysicalBase: qword;
  i: integer;

  ob: byte;
  br: size_t;
begin
  log('dbvm_cloak_changeregonbp');

  if virtualaddress<>0 then
    ReadProcessMemory(processhandle, pointer(virtualaddress), @ob,1,br);

  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_CLOAK_CHANGEREGONBP;
  vmcallinfo.PhysicalAddress:=PhysicalAddress;
  vmcallinfo.changeregonbpinfo:=changeregonbpinfo;
  result:=vmcall(@vmcallinfo,vmx_password1);

  if (result=0) then
  begin
    breakpointsCS.enter;
    setlength(breakpoints,length(breakpoints)+1);
    breakpoints[length(breakpoints)-1].PhysicalAddress:=PhysicalAddress;
    breakpoints[length(breakpoints)-1].VirtualAddress:=virtualAddress;
    breakpoints[length(breakpoints)-1].BreakOption:=integer(bo_ChangeRegister);
    breakpoints[length(breakpoints)-1].originalbyte:=ob;
    hassetbp:=true;
    breakpointscs.leave;

    if (VirtualAddress<>0) then
    begin
      cloakedregionscs.Enter;
      try
        PhysicalBase:=PhysicalAddress and MAXPHYADDRMASKPB;

        for i:=0 to length(cloakedregions)-1 do
          if cloakedregions[i].PhysicalAddress=PhysicalBase then exit;   //already in the list

        i:=length(cloakedregions);
        setlength(cloakedregions,i+1);
        cloakedregions[i].PhysicalAddress:=PhysicalBase;
        cloakedregions[i].virtualAddress:=virtualAddress and qword($fffffffffffff000);

        outputdebugstring('added it to entry '+inttostr(i));
      finally
        cloakedregionscs.leave;
      end;
    end;

    if (GetCurrentThreadId=MainThreadID) and (frmbreakPointList<>nil) and (frmbreakPointList.visible) then
      frmbreakPointList.updatebplist;
  end
  else
    log('VMCALL_CLOAK_CHANGEREGONBP failed. it returned '+inttostr(result));
end;

function dbvm_cloak_removechangeregonbp(PhysicalAddress: QWORD): integer;
var
  vmcallinfo: packed record
    structsize: dword;
    level2pass: dword;
    command: dword;
    PhysicalAddress: QWORD;
  end;
  i,j: integer;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_CLOAK_REMOVECHANGEREGONBP;
  vmcallinfo.PhysicalAddress:=PhysicalAddress;
  result:=vmcall(@vmcallinfo,vmx_password1);

  breakpointsCS.Enter;
  for i:=0 to length(breakpoints)-1 do
    if breakpoints[i].PhysicalAddress=PhysicalAddress then
    begin
      for j:=i to length(breakpoints)-2 do
        breakpoints[j]:=breakpoints[j+1];

      setlength(breakpoints, length(breakpoints)-1);
    end;

  if (GetCurrentThreadId=MainThreadID) and (frmbreakPointList<>nil) and (frmbreakPointList.visible) then
    frmbreakPointList.updatebplist;

  hassetbp:=length(breakpoints)<>0;

  breakpointsCS.Leave;

  flushCloakedMemoryCache(PhysicalAddress); //flush out that int3 which will confuse users for half a second

end;


function dbvm_isBreakpoint(virtualAddress: ptruint; out physicalAddress: qword; out breakoption: integer; var originalbyte: byte): boolean;
var i: integer;
begin
  result:=false;
  if hassetbp then
  begin
    breakpointsCS.Enter;
    try
      for i:=0 to length(breakpoints)-1 do
      begin
        if breakpoints[i].VirtualAddress=virtualAddress then
        begin
          physicaladdress:=breakpoints[i].PhysicalAddress;
          breakoption:=breakpoints[i].breakoption;
          originalbyte:=breakpoints[i].originalbyte;
          result:=true;
        end;
      end;
    finally
      breakpointscs.leave;
    end;

  end;
end;

procedure dbvm_getBreakpointList(l: TStrings);
//gets the breakpoint list, the caller will need to free the entries after call
var
  i: integer;
  s: string;
begin
  s:='';
  breakpointscs.enter;
  try
    for i:=0 to length(breakpoints)-1 do
    begin
      if breakpoints[i].VirtualAddress<>0 then
        s:=inttohex(breakpoints[i].VirtualAddress,8);

      s:=s+' ('+inttohex(breakpoints[i].physicaladdress,8)+')';
      l.Add(s);
    end
  finally
    breakpointscs.leave;
  end;
end;

function dbvm_get_statistics(out statistics: TDBVMStatistics):qword;
var
  vmcallinfo: packed record
    structsize: dword;
    level2pass: dword;
    command: dword;
    eventcountercpu: array [0..55] of integer;
    eventcounterall: array [0..55] of integer;
  end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_GET_STATISTICS;
  result:=vmcall(@vmcallinfo,vmx_password1);

  CopyMemory(@statistics.eventCountersCurrentCPU[0],@vmcallinfo.eventcountercpu,sizeof(int)*56);
  CopyMemory(@statistics.eventCountersAllCPUS[0],@vmcallinfo.eventcounterall,sizeof(int)*56);
end;

procedure dbvm_setTSCAdjust(enabled: boolean; timeout: integer);
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
  enabled: integer;
  timeout: integer;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_SETTSCADJUST;
  if enabled then
  begin
    vmcallinfo.enabled:=1;
    vmcallinfo.timeout:=timeout;
  end
  else
  begin
    vmcallinfo.enabled:=0;
    vmcallinfo.timeout:=2000;
  end;

  vmcall(@vmcallinfo,vmx_password1);
end;

procedure dbvm_speedhack_setSpeed(speed: double);
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
  speed: double;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_SETSPEEDHACK;
  vmcallinfo.speed:=speed;

  vmcall(@vmcallinfo,vmx_password1);
end;



procedure dbvm_ept_reset;
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_EPT_RESET;
  vmcall(@vmcallinfo,vmx_password1);

  if cloakedregionsCS<>nil then
  begin
    cloakedregionsCS.enter;
    setlength(cloakedregions,0);
    cloakedregionsCS.leave;
  end;
end;

function dbvm_log_cr3values_start: boolean;
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_LOG_CR3VALUES_START;
  result:=vmcall(@vmcallinfo,vmx_password1)<>0;
end;

function dbvm_log_cr3values_stop(log: pointer): boolean;
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
  destination: QWORD;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_LOG_CR3VALUES_STOP;
  vmcallinfo.destination:=ptruint(log);
  result:=vmcall(@vmcallinfo,vmx_password1)<>0;
end;

function dbvm_registerPlugin(pluginaddress: pointer; pluginsize: integer; plugintype: integer): integer;
{
registers a plugin with DBVM.  (The contents of pluginaddress to size are copied
into DBVM, so make sure there is enough RAM available inside DBVM, and that you
are inside DBVM)

plugintype: 0=vmexit_pre: Called before DBVM handles the current vmexit
            1=vmexit_post: After DBVM has handled the current vmexit

look at the DBVM source for more info
typedef BOOL DBVM_PLUGIN_EXIT_PRE(PDBVMExports exports, pcpuinfo currentcpuinfo, void *registers, void *fxsave);
typedef void DBVM_PLUGIN_EXIT_POST(PDBVMExports exports, pcpuinfo currentcpuinfo, void *registers, void *fxsave, int *DBVMResult);

Linux 64-Bit ABI calling convention
}
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
  plugintype: integer;
  virtualAddress: QWORD;
  byteSize: integer;
  reserved: QWORD; //must be 0
  reserved2: integer;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_REGISTERPLUGIN;
  vmcallinfo.plugintype:=plugintype;
  vmcallinfo.virtualAddress:=qword(pluginaddress);
  vmcallinfo.byteSize:=pluginsize;
  vmcallinfo.reserved:=0;
  vmcallinfo.reserved2:=0;
  result:=vmcall(@vmcallinfo,vmx_password1);
end;

procedure dbvm_raisePMI;
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_RAISEPMI;
  vmcall(@vmcallinfo,vmx_password1);
end;

procedure dbvm_ultimap2_hideRangeUsage; //hides thge range usage. You can call it as often as you like
var vmcallinfo: packed record
  structsize: dword;
  level2pass: dword;
  command: dword;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_ULTIMAP2_HIDERANGEUSAGE;
  vmcall(@vmcallinfo,vmx_password1);
end;

var kernelfunctions: Tstringlist;


var ExAllocatePool: function (alloctype: DWORD; size: SIZE_T): pointer; stdcall;

procedure setupKernelFunctionList;
var i,j: integer;
    d: tstringlist;
    base: ptruint;
    hal: tstringlist;
begin
  if kernelfunctions=nil then
  begin
    kernelfunctions:=tstringlist.create;
    peinfo_getExportList(WindowsDir+'\System32\ntoskrnl.exe', kernelfunctions);

    d:=tstringlist.create;
    try
      //adjust the addresses for the kernel base
      getDriverList(d);
      base:=ptruint(d.Objects[0]);
      for i:=0 to kernelfunctions.Count-1 do
        kernelfunctions.Objects[i]:=tobject(ptruint(kernelfunctions.objects[i])+base);


      //also add the hal.dll functions in the rare case it's imported by a driver...
      for i:=0 to d.count-1 do
        if pos(' hal.dll',lowercase(d[i]))>0 then
        begin
          base:=ptruint(d.Objects[i]);

          hal:=tstringlist.create;
          try
            peinfo_getExportList(WindowsDir+'\System32\hal.dll', hal);
            for j:=0 to hal.count-1 do
              hal.Objects[i]:=Tobject(ptruint(hal.objects[i])+base);

            kernelfunctions.AddStrings(hal);
          finally
            hal.free;
          end;

          break;
        end;

    finally
      d.free;
    end;


    i:=kernelfunctions.IndexOf('ExAllocatePool');
    if i<>-1 then
      ExAllocatePool:=pointer(kernelfunctions.Objects[i]);
  end;

end;


type TKernelmodeFunction=function (parameters: pointer): ptruint;


procedure dbvm_enterkernelmode(originalstate: POriginalState); //mainly used for 64-bit systems requiring to stealth load the driver
begin
  setupKernelFunctionList;

  dbvm_block_interrupts;

  {$ifdef cpu32}
  asm
    push ebx
    mov ebx, originalstate

    push eax
    pushfd
    pop eax

    mov TOriginalState(ebx).oldflags,eax
    pop eax

    push eax

    mov ax,cs
    mov TOriginalState(ebx).oldcs,ax

    mov ax,ss
    mov TOriginalState(ebx).oldss,ax

    mov ax,ds
    mov TOriginalState(ebx).oldds,ax

    mov ax,es
    mov TOriginalState(ebx).oldes,ax

    mov ax,fs
    mov TOriginalState(ebx).oldfs,ax

    mov ax,gs
    mov TOriginalState(ebx).oldgs,ax
    pop eax

    pop ebx
  end;
  {$endif}

  {$ifdef cpu64}
  asm
    push rbx
    mov rbx, originalstate

    push rax
    pushfq
    pop rax
    mov TOriginalState(rbx).oldflags,eax
    pop rax


    push rax

    mov ax,cs
    mov TOriginalState(rbx).oldcs,ax

    mov ax,ss
    mov TOriginalState(rbx).oldss,ax

    mov ax,ds
    mov TOriginalState(rbx).oldds,ax

    mov ax,es
    mov TOriginalState(rbx).oldes,ax

    mov ax,fs
    mov TOriginalState(rbx).oldfs,ax

    mov ax,gs
    mov TOriginalState(rbx).oldgs,ax
    pop rax

    pop rbx
  end;
  {$endif}

{$ifdef cpu32}
  //not really sure about the new ss, this is mainly meant for 64-bit users after all
  dbvm_changeselectors($8, $10,originalstate.oldds,originalstate.oldes, originalstate.oldfs, originalstate.oldgs);
{$else}
  dbvm_changeselectors($10, $18,originalstate.oldds,originalstate.oldes, originalstate.oldfs, originalstate.oldgs);
{$endif}


  {$ifdef cpu64}
  asm
    db $0f, $01, $f8 //swapgs
  end;
  {$endif}


  dbvm_restore_interrupts;
end;

procedure dbvm_returntousermode(originalstate: POriginalState);
begin

  dbvm_block_interrupts;


  {$ifdef cpu64}
  asm
    db $0f, $01, $f8 //swapgs
  end;
  {$endif}




  dbvm_changeselectors(originalstate.oldcs, originalstate.oldss, originalstate.oldds, originalstate.oldes, originalstate.oldfs, originalstate.oldgs);


  {$ifdef cpu32}
  asm
    push ebx
    mov ebx, originalstate

    mov eax,TOriginalState(ebx).oldflags
    push eax
    popfd

    pop ebx
  end;
  {$endif}

  {$ifdef cpu64}
  asm
    push rbx
    mov rbx, originalstate

    xor rax,rax
    mov eax,TOriginalState(rbx).oldflags
    push rax
    popfq

    pop rbx
  end;
  {$endif}

  dbvm_restore_interrupts;


end;


function dbvm_getProcAddress(functionname: string): pointer;
var i: integer;
begin
  result:=nil;
  setupKernelFunctionList;
  i:=kernelfunctions.IndexOf(functionname);
  if i<>-1 then
    result:=kernelfunctions.objects[i];
end;

type
  TCommand=record
    command: dword;
    result: pointer;
    param1: ptruint;
    param2: ptruint;
    param3: ptruint;
    param4: ptruint;
    param5: ptruint;
    param6: ptruint;
    param7: ptruint;
    param8: ptruint;
  end;
  PCommand=^TCommand;

type TDriverEntry=function(DriverObject: pointer; RegistryPath: pointer): integer; stdcall;
type TDispatchIOCTL=function(DriverObject: pointer; dwIoControlCode: DWORD; lpInBuffer: pointer; nInBufferSize:integer; lpOutBuffer: pointer; nOutBufferSize: integer; lpBytesReturned: pdword): BOOL; stdcall;

function executeDispatchIOCTL_fromKernelMode(DispatchIOCTL: TDispatchIOCTL; DriverObject: pointer; dwIoControlCode: DWORD; lpInBuffer: pointer; nInBufferSize:integer; lpOutBuffer: pointer; nOutBufferSize: integer; lpBytesReturned: pdword): BOOL;
begin
  result:=DispatchIOCTL(DriverObject, dwIoControlCode, lpInBuffer, nInBufferSize, lpOutBuffer, nOutBufferSize, lpBytesReturned);
end;

function executeDriverEntry_fromKernelmode(driverentry: TDriverEntry; DriverObject: pointer; RegistryPath: pointer): ptruint;
begin
  result:=driverEntry(driverObject, RegistryPath);
end;

function dbvm_localIntHandler(x: pointer): integer;
var c: PCommand;
begin
  result:=0;
  //kernelmode. IRQL=passive
  c:=x;
  case c.command of
    $FFFFFFFF: pinteger(c.result)^:=123;  //test
    0: pptruint(c.result)^:=ptruint(ExAllocatePool(0, c.param1)); //Allocate memory
    1:   //copy memory
    begin
      CopyMemory(pointer(c.param1), pointer(c.param2), c.param3);

      system.pboolean(c.result)^:=true; //still here and no bsod, so I guess it should return true...
    end;

    2: //execute driver entry
      pinteger(c.result)^:=executeDriverEntry_fromKernelmode(TDriverEntry(c.param1), pointer(c.param2), pointer(c.param3));

    3:  //Dipatch IOCTL
      PBOOL(c.result)^:=executeDispatchIOCTL_fromKernelMode(TDispatchIOCTL(c.param1), pointer(c.param2), dword(c.param3), pointer(c.param4), dword(c.param5), pointer(c.param6), dword(c.param7), pointer(c.param8));


  end;
end;

procedure dbvm_localIntHandler_entry; nostackframe; assembler; //usually 64-bit only
{$ifdef cpu32}
//not implemented for 32-bit
asm
  sub esp,4096

  mov [esp+$00],eax
  mov [esp+$08],ebx
  mov [esp+$10],ecx
  mov [esp+$18],edx
  mov [esp+$20],esi
  mov [esp+$28],edi
  mov [esp+$30],ebp
  mov ax,ds
  mov [esp+$38],ax

  mov ax,es
  mov [esp+$40],ax

  mov ax,fs
  mov [esp+$48],ax

  mov ax,gs
  mov [esp+$50],ax


  mov ecx,[esp+4096]
  sub esp,$30

  sti
  call dbvm_localIntHandler
  cli
  add esp,$30


  mov ax,[esp+$50]
  mov gs,ax

  mov ax,[esp+$48]
  mov fs,ax

  mov ax,[esp+$40]
  mov es,ax

  mov ax,[esp+$38]
  mov ds,ax

  mov ebp,[esp+$30]
  mov edi,[esp+$28]
  mov esi,[esp+$20]
  mov edx,[esp+$18]
  mov ecx,[esp+$10]
  mov ebx,[esp+$08]
  mov eax,[esp+$00]

  add esp,4096
  //no errorcode in the emulation for 32-bit
  iretd
end;
{$else}
asm

  sub rsp,4096

  mov [rsp+$00],rax
  mov [rsp+$08],rbx
  mov [rsp+$10],rcx
  mov [rsp+$18],rdx
  mov [rsp+$20],rsi
  mov [rsp+$28],rdi
  mov [rsp+$30],rbp
  mov ax,ds
  mov [rsp+$38],ax

  mov ax,es
  mov [rsp+$40],ax

  mov ax,fs
  mov [rsp+$48],ax

  mov ax,gs
  mov [rsp+$50],ax

  mov [rsp+$58],r8
  mov [rsp+$60],r9
  mov [rsp+$68],r10
  mov [rsp+$70],r11
  mov [rsp+$78],r12
  mov [rsp+$80],r13
  mov [rsp+$88],r14
  mov [rsp+$90],r15


  db $0f, $01, $f8 //swapgs

  mov rcx,[rsp+4096]
  sub rsp,$30

  sti
  call dbvm_localIntHandler
  cli
  add rsp,$30

  db $0f, $01, $f8 //swapgs

  mov r15, [rsp+$90]
  mov r14, [rsp+$88]
  mov r13, [rsp+$80]
  mov r12, [rsp+$78]
  mov r11, [rsp+$70]
  mov r10, [rsp+$68]
  mov r9, [rsp+$60]
  mov r8, [rsp+$58]

  mov ax,[rsp+$50]
  mov gs,ax

  mov ax,[rsp+$48]
  mov fs,ax

  mov ax,[rsp+$40]
  mov es,ax

  mov ax,[rsp+$38]
  mov ds,ax

  mov rbp,[rsp+$30]
  mov rdi,[rsp+$28]
  mov rsi,[rsp+$20]
  mov rdx,[rsp+$18]
  mov rcx,[rsp+$10]
  mov rbx,[rsp+$08]
  mov rax,[rsp+$00]

  add rsp,4096

  add rsp,8 //undo errorcode (64-bit ALWAYS pushes an errorcode, in this emulation)
  db $48, $cf //iretq
end;
{$endif}

function dbvm_testSwitchToKernelmode: integer;
var command: TCommand;
begin
  setupKernelFunctionList;

  command.command:=$ffffffff;
  command.result:=@result;
  dbvm_switchToKernelMode($10, @dbvm_localIntHandler_entry, @command);
end;



function dbvm_executeDispatchIoctl(DispatchIoctl: pointer; DriverObject: pointer; dwIoControlCode: DWORD; lpInBuffer: pointer; nInBufferSize:integer; lpOutBuffer: pointer; nOutBufferSize: integer; lpBytesReturned: pdword): BOOL;
var command: TCommand;
begin
  command.command:=3; //DispatchIOCTL
  command.result:=@result;
  command.param1:=ptruint(DispatchIoctl);
  command.param2:=ptruint(DriverObject);
  command.param3:=ptruint(dwIoControlCode);
  command.param4:=ptruint(lpInBuffer);
  command.param5:=ptruint(nInBufferSize);
  command.param6:=ptruint(lpOutBuffer);
  command.param7:=ptruint(nOutBufferSize);
  command.param8:=ptruint(lpBytesReturned);

  dbvm_switchToKernelMode($10, @dbvm_localIntHandler_entry, @command);
end;


function dbvm_executeDriverEntry(driverentry: pointer; DriverObject: pointer; RegistryPath: pointer): integer;
var command: TCommand;
begin
  setupKernelFunctionList;

  command.command:=2;
  command.result:=@result;
  command.param1:=ptruint(driverentry);
  command.param2:=ptruint(DriverObject);
  command.param3:=ptruint(RegistryPath);

  dbvm_switchToKernelMode($10, @dbvm_localIntHandler_entry, @command);
end;

function dbvm_copyMemory(destination, target: pointer; size: integer): boolean;
var command: TCommand;
begin
  result:=true; //anything else is a bsod

  command.command:=1;
  command.result:=@result;
  command.param1:=ptruint(destination);
  command.param2:=ptruint(target);
  command.param3:=ptruint(size);


  dbvm_switchToKernelMode($10, @dbvm_localIntHandler_entry, @command);
end;

function dbvm_kernelalloc(size: dword): pointer;
{
use dbvm to allocate kernelmode memory
}
var command: TCommand;
begin
  setupKernelFunctionList;

  command.command:=0;
  command.result:=@result;
  command.param1:=size;

  dbvm_switchToKernelMode($10, @dbvm_localIntHandler_entry, @command);
end;



procedure configure_vmx(userpassword1,userpassword2: dword); //warning: not multithreaded, take care to only run at init!
begin
  {$ifndef NOVMX}
  //configure dbvm if possible
  OutputDebugString('configure_vmx');

  //first try the default password and if it works change the password to the userdefined one
  vmx_password1:=$76543210;
  vmx_password2:=$fedcba98;

  if (dbvm_version>=$ce000000) and (userpassword1<>vmx_password1) and (userpassword2<>vmx_password2) then //this works, change the password
    dbvm_changepassword(userpassword1,userpassword2);

  vmx_password1:=userpassword1;
  vmx_password2:=userpassword2;
  {$endif}
end;


procedure configure_vmx_kernel;
type TInput=record
  Virtualization_Enabled: DWORD;
	Password1: DWORD;
  Password2: DWORD;
end;
var cc: dword;
    x: TInput;
begin
  if (dbvm_version>$ce000000) then //tell the driver it can use vmcall instructions
  begin
    OutputDebugString('vmx_enabled=TRUE');
    
    x.Virtualization_Enabled:=1;
    x.Password1:=vmx_password1;
    x.Password2:=vmx_password2;

    if (hdevice<>INVALID_HANDLE_VALUE) then
    begin
      cc:=IOCTL_CE_VMXCONFIG;
      deviceiocontrol(hdevice,cc,@x,sizeof(x),nil,0,cc,nil);
    end;

    vmx_enabled:=true;
  end else OutputDebugString('vmx_enabled=FALSE');
end;

initialization
  vmcall:=vmcallUnSupported;
  vmcall2:=vmcall2Unsupported;

  {$ifndef NOVMX}
  if isDBVMCapable then
  begin
    if isamd then
    begin
      vmcall:=vmcallSupported_amd;
      vmcall2:=vmcallSupported2_amd
    end
    else
    begin
      vmcall:=vmcallSupported_intel;
      vmcall2:=vmcallSupported2_intel;
    end;
  end;


  {$endif}
  cloakedregionscs:=TCriticalSection.Create;
  breakpointscs:=TCriticalSection.Create;
end.
