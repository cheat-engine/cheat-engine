unit vmxfunctions;

interface

uses windows;

const
  VMCALL_GETVERSION=0;
  VMCALL_CHANGEPASSWORD=1;
  VMCALL_REDIRECTINT1=9;
  VMCALL_INT1REDIRECTED=10;
  VMCALL_CHANGESELECTORS=12;
  VMCALL_BLOCK_INTERRUPTS=13;
  VMCALL_RESTORE_INTERRUPTS=14;

function dbvm_version: dword; stdcall;
function dbvm_changepassword(password1,password2: dword):dword; stdcall;
function dbvm_changeselectors(cs,ss,ds,es,fs,gs: dword): DWORD; stdcall;
function dbvm_restore_interrupts: DWORD; stdcall;
function dbvm_block_interrupts: DWORD; stdcall;
function dbvm_redirect_interrupt1(redirecttype: integer; newintvector: dword; int1cs: dword; int1eip: dword): dword; stdcall;

procedure configure_vmx(userpassword1,userpassword2: dword);

var
  vmx_password1: dword;
  vmx_password2: dword;

  vmx_enabled: boolean;

implementation

uses DBK32functions;



function vmcall(vmcallinfo:pointer; level1pass: dword): dword; stdcall;
asm
  push edx
  mov eax,vmcallinfo
  mov edx,level1pass
  db $0f, $01,$c1   //vmcall
  pop edx
  //prolog is done by delphi
end;

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


procedure configure_vmx(userpassword1,userpassword2: dword); //warning: not multithreaded, take care to only run at init!
type TInput=record
  Virtualization_Enabled: DWORD;
	Password1: DWORD;
  Password2: DWORD;
end;
var cc: dword;
    x: TInput;
begin
  //configure dbvm if possible

  //first try the default password and if it works change the password to the userdefined one
  vmx_password1:=$76543210;
  vmx_password2:=$fedcba98;

  if dbvm_version>=$ce000000 then //this works, change the password
    dbvm_changepassword(userpassword1,userpassword2);

  vmx_password1:=userpassword1;
  vmx_password2:=userpassword2;
  if (dbvm_version>=$ce000000) then
    vmx_enabled:=true;


  if (vmx_enabled) then //tell the driver it can use vmcall instructions
  begin
    x.Virtualization_Enabled:=1;
    x.Password1:=vmx_password1;
    x.Password2:=vmx_password2;

    if (hdevice<>INVALID_HANDLE_VALUE) then
    begin
      cc:=IOCTL_CE_VMXCONFIG;
      deviceiocontrol(hdevice,cc,@x,sizeof(x),nil,0,cc,nil);
    end;
  end;
end;

end.
