unit vmxfunctions;

interface

uses windows;

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


function dbvm_version: dword; stdcall;
function dbvm_changepassword(password1,password2: dword):dword; stdcall;
function dbvm_changeselectors(cs,ss,ds,es,fs,gs: dword): DWORD; stdcall;
function dbvm_restore_interrupts: DWORD; stdcall;
function dbvm_block_interrupts: DWORD; stdcall;
function dbvm_redirect_interrupt1(redirecttype: integer; newintvector: dword; int1cs: dword; int1eip: dword): dword; stdcall;
function dbvm_read_physical_memory(PhysicalAddress: UINT64; destination: pointer; size: integer): dword; stdcall;
function dbvm_write_physical_memory(PhysicalAddress: UINT64; source: pointer; size: integer): dword; stdcall;
function dbvm_raise_privilege: DWORD; stdcall;

procedure configure_vmx(userpassword1,userpassword2: dword);
procedure configure_vmx_kernel;


var
  vmx_password1: dword;
  vmx_password2: dword;

  vmx_enabled: boolean;

implementation

uses DBK32functions;

function vmcall(vmcallinfo:pointer; level1pass: dword): dword; stdcall;
{$ifndef NOVMX}
asm
  {$ifdef cpu64}
    push rdx
    mov rax,vmcallinfo
    mov edx,level1pass
    vmcall
    pop rdx
  {$else}
    push edx
    mov eax,vmcallinfo
    mov edx,level1pass
    vmcall
    pop edx
  {$endif}
end;
{$else}
begin
  result:=0;
end;
{$endif}

function dbvm_version: dword; stdcall;
var vmcallinfo: record
  structsize: dword;
  level2pass: dword;
  command: dword;
end;
begin
  vmcallinfo.structsize:=sizeof(vmcallinfo);
  vmcallinfo.level2pass:=vmx_password2;
  vmcallinfo.command:=VMCALL_GETVERSION;
  try
    result:=vmcall(@vmcallinfo,vmx_password1);
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

  try
    result:=vmcall(@vmcallinfo,vmx_password1);
  except
    result:=$ffffffff;
  end;
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
  try
    result:=vmcall(@vmcallinfo,vmx_password1);
  except
    result:=$ffffffff;
  end;
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
  try
    result:=vmcall(@vmcallinfo,vmx_password1);
  except
    result:=$ffffffff;
  end;
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
    messagebox(0,'Error','error',mb_ok);
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
  try
    result:=vmcall(@vmcallinfo,vmx_password1);
  except
    result:=$ffffffff;
  end;
end;



procedure configure_vmx(userpassword1,userpassword2: dword); //warning: not multithreaded, take care to only run at init!
begin
  {$ifndef NOVMX}
  //configure dbvm if possible
  OutputDebugString('configure_vmx');

  //first try the default password and if it works change the password to the userdefined one
  vmx_password1:=$76543210;
  vmx_password2:=$fedcba98;

  if dbvm_version>=$ce000000 then //this works, change the password
    dbvm_changepassword(userpassword1,userpassword2);

  vmx_password1:=userpassword1;
  vmx_password2:=userpassword2;
  if (dbvm_version>=$ce000000) then
    vmx_enabled:=true;
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
  if (vmx_enabled) then //tell the driver it can use vmcall instructions
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
  end else OutputDebugString('vmx_enabled=FALSE');
end;

end.
