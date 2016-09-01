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

procedure configure_vmx(userpassword1,userpassword2: dword);
procedure configure_vmx_kernel;


var
  vmx_password1: dword;
  vmx_password2: dword;

  vmx_enabled: boolean;

  //dbvmversion: integer=0;

implementation

uses DBK32functions, cefuncproc, PEInfoFunctions, NewKernelHandler, Globals;

resourcestring
rsInvalidInstruction = 'Invalid instruction';
rsBigError = 'Error';
rsSmallError = 'error';

var vmcall :function(vmcallinfo:pointer; level1pass: dword): PtrUInt; stdcall;

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
      OutputDebugString('Invalid vmx');
      result:=0;
    end;

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
    result:=vmcall(@vmcallinfo,vmx_password1);
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
    result:=vmcall(@vmcallinfo,vmx_password1);
  except
    result:=0; //read 0 bytes
    messagebox(0,pchar(rsBigError),pchar(rsSmallError),mb_ok);
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

  {$ifndef NOVMX}
  if isDBVMCapable then
  begin
    if isamd then
      vmcall:=vmcallSupported_amd
    else
      vmcall:=vmcallSupported_intel;

  end;
  {$endif}
end.
