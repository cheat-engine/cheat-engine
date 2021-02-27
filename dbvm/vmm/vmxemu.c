/*
 * vmxemu.c
 *
 *  Created on: Jan 15, 2018
 *      Author: erich
 *
 *  Emulates the intel VMX instruction set
 */


#include "vmxemu.h"
#include "main.h"
#include "vmcall.h"

#include "common.h"
#include "vmpaging.h"
#include "vmeventhandler.h"
#include "vmcall.h"
#include "mm.h"
#include "displaydebug.h"


int emulatevmx=1;

//todo: vmcs link pointer implementation


void setHostState(vmxhoststate *hoststate)
{
  vmwrite(vm_host_es, hoststate->ES);
  vmwrite(vm_host_cs, hoststate->CS);
  vmwrite(vm_host_ss, hoststate->SS);
  vmwrite(vm_host_ds, hoststate->DS);
  vmwrite(vm_host_fs, hoststate->FS);
  vmwrite(vm_host_gs, hoststate->GS);
  vmwrite(vm_host_tr, hoststate->TR);
  vmwrite(vm_host_IA32_PAT, hoststate->IA32_PAT);
  vmwrite(vm_host_IA32_EFER, hoststate->IA32_EFER);
  vmwrite(vm_host_IA32_PERF_GLOBAL_CTRL, hoststate->IA32_PERF_GLOBAL_CTRL);
  vmwrite(vm_host_IA32_SYSENTER_CS, hoststate->IA32_SYSENTER_CS);

  vmwrite(vm_host_cr0, hoststate->CR0);
  vmwrite(vm_host_cr3, hoststate->CR3);
  vmwrite(vm_host_cr4, hoststate->CR4);

  vmwrite(vm_host_fs_base, hoststate->FS_BASE);
  vmwrite(vm_host_gs_base, hoststate->GS_BASE);
  vmwrite(vm_host_tr_base, hoststate->TR_BASE);
  vmwrite(vm_host_gdtr_base, hoststate->GDTR_BASE);
  vmwrite(vm_host_idtr_base, hoststate->IDTR_BASE);

  vmwrite(vm_host_IA32_SYSENTER_ESP, hoststate->IA32_SYSENTER_ESP);
  vmwrite(vm_host_IA32_SYSENTER_EIP, hoststate->IA32_SYSENTER_EIP);

  vmwrite(vm_host_rsp, hoststate->RSP);
  vmwrite(vm_host_rip, hoststate->RIP);

}

void getHostState(vmxhoststate *hoststate)
/*
 * gets the host state of the currently loaded VMX and stores it into *hoststate
 */
{

  hoststate->ES=vmread(vm_host_es);
  hoststate->CS=vmread(vm_host_cs);
  hoststate->SS=vmread(vm_host_ss);
  hoststate->DS=vmread(vm_host_ds);
  hoststate->FS=vmread(vm_host_fs);
  hoststate->GS=vmread(vm_host_gs);
  hoststate->TR=vmread(vm_host_tr);
  hoststate->IA32_PAT=vmread(vm_host_IA32_PAT);
  hoststate->IA32_EFER=vmread(vm_host_IA32_EFER);
  hoststate->IA32_PERF_GLOBAL_CTRL=vmread(vm_host_IA32_PERF_GLOBAL_CTRL);
  hoststate->IA32_SYSENTER_CS=vmread(vm_host_IA32_SYSENTER_CS);

  hoststate->CR0=vmread(vm_host_cr0);
  hoststate->CR3=vmread(vm_host_cr3);
  hoststate->CR4=vmread(vm_host_cr4);

  hoststate->FS_BASE=vmread(vm_host_fs_base);
  hoststate->GS_BASE=vmread(vm_host_gs_base);
  hoststate->TR_BASE=vmread(vm_host_tr_base);
  hoststate->GDTR_BASE=vmread(vm_host_gdtr_base);
  hoststate->IDTR_BASE=vmread(vm_host_idtr_base);

  hoststate->IA32_SYSENTER_ESP=vmread(vm_host_IA32_SYSENTER_ESP);
  hoststate->IA32_SYSENTER_EIP=vmread(vm_host_IA32_SYSENTER_EIP);

  hoststate->RSP=vmread(vm_host_rsp);
  hoststate->RIP=vmread(vm_host_rip);
}

//handlebyDBVM(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)  //don't let the guest exit, just continue the host's guest

int handleByGuest(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{
  //switch to the default VMCS

  if (currentcpuinfo->vmxdata.runningvmx==0)
  {
    sendstringf("handleByGuest was called while runningvmx is 0");
    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
    while (1);
  }
  else
  if (currentcpuinfo->vmxdata.insideVMXRootMode==0)
  {
    sendstringf("runningvmx but insideVMXRootMode is 0\n");
    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
    while (1);
  }

  int ptrld=vmptrld(currentcpuinfo->vmcs_regionPA);

  if (ptrld)
  {
    sendstringf("failure loading the vmcs_regionPA\n");
    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
    while(1);
  }

  //sendstringf("ptrld=%d\n", ptrld);

  //set the guest state to the original host state (load the GDT, map it, and then set the segment register accesses and limits properly. Like vmmhelper launchtime)
  vmwrite(vm_guest_es, currentcpuinfo->vmxdata.originalhoststate.ES);
  vmwrite(vm_guest_cs, currentcpuinfo->vmxdata.originalhoststate.CS);
  vmwrite(vm_guest_ss, currentcpuinfo->vmxdata.originalhoststate.SS);
  vmwrite(vm_guest_ds, currentcpuinfo->vmxdata.originalhoststate.DS);
  vmwrite(vm_guest_fs, currentcpuinfo->vmxdata.originalhoststate.FS);
  vmwrite(vm_guest_gs, currentcpuinfo->vmxdata.originalhoststate.GS);
  vmwrite(vm_guest_tr, currentcpuinfo->vmxdata.originalhoststate.TR);






  vmwrite(vm_guest_IA32_PAT, currentcpuinfo->vmxdata.originalhoststate.IA32_PAT);
  vmwrite(vm_guest_IA32_EFER, currentcpuinfo->vmxdata.originalhoststate.IA32_EFER);
  vmwrite(vm_guest_IA32_PERF_GLOBAL_CTRL, currentcpuinfo->vmxdata.originalhoststate.IA32_PERF_GLOBAL_CTRL);
  vmwrite(vm_guest_IA32_SYSENTER_CS, currentcpuinfo->vmxdata.originalhoststate.IA32_SYSENTER_CS);

  vmwrite(vm_guest_cr0, currentcpuinfo->vmxdata.originalhoststate.CR0);
  vmwrite(vm_guest_cr3, currentcpuinfo->vmxdata.originalhoststate.CR3);
  vmwrite(vm_guest_cr4, currentcpuinfo->vmxdata.originalhoststate.CR4);

  currentcpuinfo->guestCR3=currentcpuinfo->vmxdata.originalhoststate.CR3;

  vmwrite(vm_guest_fs_base, currentcpuinfo->vmxdata.originalhoststate.FS_BASE);
  vmwrite(vm_guest_gs_base, currentcpuinfo->vmxdata.originalhoststate.GS_BASE);
  vmwrite(vm_guest_tr_base, currentcpuinfo->vmxdata.originalhoststate.TR_BASE);

  vmwrite(vm_guest_gdtr_base, currentcpuinfo->vmxdata.originalhoststate.GDTR_BASE);
  vmwrite(vm_guest_idtr_base, currentcpuinfo->vmxdata.originalhoststate.IDTR_BASE);

  vmwrite(vm_guest_IA32_SYSENTER_ESP, currentcpuinfo->vmxdata.originalhoststate.IA32_SYSENTER_ESP);
  vmwrite(vm_guest_IA32_SYSENTER_EIP, currentcpuinfo->vmxdata.originalhoststate.IA32_SYSENTER_EIP);

  vmwrite(vm_guest_rsp, currentcpuinfo->vmxdata.originalhoststate.RSP);
  vmwrite(vm_guest_rip, currentcpuinfo->vmxdata.originalhoststate.RIP);

  //now setup the segment limits/bases
  PGDT_ENTRY gdt=NULL; //no LDT
  //get the minimum size needed to map the GDT
  int maxsegment=currentcpuinfo->vmxdata.originalhoststate.ES | 7;
  maxsegment=max(maxsegment, currentcpuinfo->vmxdata.originalhoststate.CS | 7);
  maxsegment=max(maxsegment, currentcpuinfo->vmxdata.originalhoststate.SS | 7);
  maxsegment=max(maxsegment, currentcpuinfo->vmxdata.originalhoststate.DS | 7);
  maxsegment=max(maxsegment, currentcpuinfo->vmxdata.originalhoststate.FS | 7);
  maxsegment=max(maxsegment, currentcpuinfo->vmxdata.originalhoststate.GS | 7);
  maxsegment=max(maxsegment, currentcpuinfo->vmxdata.originalhoststate.TR | 7);


  int error=0;
  QWORD pagefault;

  gdt=mapVMmemory(currentcpuinfo, currentcpuinfo->vmxdata.originalhoststate.GDTR_BASE, maxsegment, &error, &pagefault);

  if (gdt==NULL)
  {
    nosendchar[getAPICID()]=0;
    sendstring("For some messed up reason the gdt is paged out...");
    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
    while (1);
  }

  if (error)
  {
    sendstring("OK, WTF is going on here!");
    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
    while (1);

  }

  vmwrite(vm_guest_es_base,getSegmentBase(gdt,NULL,currentcpuinfo->vmxdata.originalhoststate.ES & ~7));
  vmwrite(vm_guest_es_limit,getSegmentLimit(gdt,NULL,currentcpuinfo->vmxdata.originalhoststate.ES & ~7));
  vmwrite(vm_guest_es_access_rights,getSegmentAccessRights(gdt,NULL,currentcpuinfo->vmxdata.originalhoststate.ES & ~7) | 1);

  vmwrite(vm_guest_cs_base,getSegmentBase(gdt,NULL,currentcpuinfo->vmxdata.originalhoststate.CS & ~7));
  vmwrite(vm_guest_cs_limit,getSegmentLimit(gdt,NULL,currentcpuinfo->vmxdata.originalhoststate.CS & ~7));
  vmwrite(vm_guest_cs_access_rights,getSegmentAccessRights(gdt,NULL,currentcpuinfo->vmxdata.originalhoststate.CS & ~7) | 1);

  vmwrite(vm_guest_ss_base,getSegmentBase(gdt,NULL,currentcpuinfo->vmxdata.originalhoststate.SS & ~7));
  vmwrite(vm_guest_ss_limit,getSegmentLimit(gdt,NULL,currentcpuinfo->vmxdata.originalhoststate.SS & ~7));
  vmwrite(vm_guest_ss_access_rights,getSegmentAccessRights(gdt,NULL,currentcpuinfo->vmxdata.originalhoststate.SS & ~7) | 1);

  vmwrite(vm_guest_ds_base,getSegmentBase(gdt,NULL,currentcpuinfo->vmxdata.originalhoststate.DS & ~7));
  vmwrite(vm_guest_ds_limit,getSegmentLimit(gdt,NULL,currentcpuinfo->vmxdata.originalhoststate.DS & ~7));
  vmwrite(vm_guest_ds_access_rights,getSegmentAccessRights(gdt,NULL,currentcpuinfo->vmxdata.originalhoststate.DS & ~7) | 1);

  vmwrite(vm_guest_fs_access_rights,getSegmentAccessRights(gdt,NULL,currentcpuinfo->vmxdata.originalhoststate.FS & ~7) | 1);
  vmwrite(vm_guest_gs_access_rights,getSegmentAccessRights(gdt,NULL,currentcpuinfo->vmxdata.originalhoststate.GS & ~7) | 1);

  if (currentcpuinfo->vmxdata.originalhoststate.TR & ~7)
    vmwrite(vm_guest_tr_access_rights,getSegmentAccessRights(gdt,NULL,currentcpuinfo->vmxdata.originalhoststate.TR & ~7));
  else
    vmwrite(vm_guest_tr_access_rights,0x8b);

  if (vmread(vm_guest_tr_access_rights)==0)
    vmwrite(vm_guest_tr_access_rights,0x8b);

  vmwrite(vm_guest_tr_limit,getSegmentLimit(gdt,NULL,currentcpuinfo->vmxdata.originalhoststate.TR & ~7));
  //vmwrite(vm_guest_tr,0);


  if (gdt)
    unmapVMmemory(gdt,maxsegment);

  if (vmread(vm_guest_cr3)!=currentcpuinfo->vmxdata.originalhoststate.CR3)
  {
    sendvmstate(currentcpuinfo, vmregisters);

    sendstringf("vm_guest_cr3(%6) does not match currentcpuinfo->vmxdata.originalhoststate.CR3(%6)\n",vmread(vm_guest_cr3),currentcpuinfo->vmxdata.originalhoststate.CR3 );
    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
    while (1);
  }



  currentcpuinfo->vmxdata.runningvmx=0; //vmexit emulated

  return 0; //return handled
}

void vmsucceed(void)
/*
VMsucceed:
CF ← 0;
PF ← 0;
AF ← 0;
ZF ← 0;
SF ← 0;
OF ← 0;
 */
{
  RFLAGS f;
  f.value=vmread(vm_guest_rflags);
  f.CF=0;
  f.PF=0;
  f.AF=0;
  f.ZF=0;
  f.SF=0;
  f.OF=0;

  vmwrite(vm_guest_rflags, f.value);
}

void VMfailValid(pcpuinfo currentcpuinfo, int errornr)
/*
 * CF ← 0;
PF ← 0;
AF ← 0;
ZF ← 1;
SF ← 0;
OF ← 0;
Set the VM-instruction error field to ErrorNumber;
 */
{
  RFLAGS f;
  f.value=vmread(vm_guest_rflags);
  f.CF=0;
  f.PF=0;
  f.AF=0;
  f.ZF=1;
  f.SF=0;
  f.OF=0;

  currentcpuinfo->vmxdata.currenterrorcode=errornr;
  vmwrite(vm_guest_rflags, f.value);


}

void VMfailInvalid()
/*
MfailInvalid:
CF ← 1;
PF ← 0;
AF ← 0;
ZF ← 0;
SF ← 0;
OF ← 0;
 */
{
  RFLAGS f;
  f.value=vmread(vm_guest_rflags);
  f.CF=1;
  f.PF=0;
  f.AF=0;
  f.ZF=0;
  f.SF=0;
  f.OF=0;

  vmwrite(vm_guest_rflags, f.value);
}

void VMfail(pcpuinfo currentcpuinfo, int errornr)
/*
IF VMCS pointer is valid
THEN VMfailValid(ErrorNumber);
ELSE
VMfailInvalid;
 */
{
  if (currentcpuinfo->vmxdata.guest_activeVMCS != 0xffffffffffffffff)
    VMfailValid(currentcpuinfo, errornr);
  else
    VMfailInvalid();
}

QWORD regToVal(int reg, VMRegisters *vmregisters)
{
  switch (reg)
  {
    case 0: return vmregisters->rax;
    case 1: return vmregisters->rcx;
    case 2: return vmregisters->rdx;
    case 3: return vmregisters->rbx;
    case 4: return vmread(vm_guest_rsp);
    case 5: return vmregisters->rbp;
    case 6: return vmregisters->rsi;
    case 7: return vmregisters->rdi;
    case 8: return vmregisters->r8;
    case 9: return vmregisters->r9;
    case 10:return vmregisters->r10;
    case 11:return vmregisters->r11;
    case 12:return vmregisters->r12;
    case 13:return vmregisters->r13;
    case 14:return vmregisters->r14;
    case 15:return vmregisters->r15;
    default:
    {
      sendstringf("regToVal: reg=%d\n", reg);
      return 0;
    }
  }
}

void valToReg(int reg, QWORD val, VMRegisters *vmregisters)
{
  switch (reg)
  {
    case 0: vmregisters->rax=val; break;
    case 1: vmregisters->rcx=val; break;
    case 2: vmregisters->rdx=val; break;
    case 3: vmregisters->rbx=val; break;
    case 4: vmwrite(vm_guest_rsp, val); break;
    case 5: vmregisters->rbp=val; break;
    case 6: vmregisters->rsi=val; break;
    case 7: vmregisters->rdi=val; break;
    case 8: vmregisters->r8=val; break;
    case 9: vmregisters->r9=val; break;
    case 10:vmregisters->r10=val; break;
    case 11:vmregisters->r11=val; break;
    case 12:vmregisters->r12=val; break;
    case 13:vmregisters->r13=val; break;
    case 14:vmregisters->r14=val; break;
    case 15:vmregisters->r15=val; break;
  }
}

QWORD getDestinationAddressFromInstructionInfo(VMRegisters *vmregisters)
{
  //0x62614924  (should be [RSP])
  //0110 0 0100 1 1000 010 1001 0 010 0100 1 00

  QWORD baseaddress=0;
  QWORD index=0;
  QWORD segment=0;
  instruction_info_vm ii;
  ii.instructioninfo=vmread(vm_instruction_information);

  if (ii.usesreg)
  {
    sendstring("getDestinationAddressFromInstructionInfo():vm_instruction_information had usesreg==1");
    return 0;
  }

  if (ii.baseRegInvalid==0)
    baseaddress=regToVal(ii.baseReg, vmregisters);

  if (ii.indexRegInvalid==0)
  {
    index=regToVal(ii.indexReg, vmregisters);
    switch (ii.scaling)
    {
      case 1: index*=2; break;
      case 2: index*=4; break;
      case 3: index*=8; break;
    }
  }

  switch (ii.segmentReg)
  {
    case 0: segment=vmread(vm_guest_es_base); break;
    case 1: segment=vmread(vm_guest_cs_base); break;
    case 2: segment=vmread(vm_guest_ss_base); break;
    case 3: segment=vmread(vm_guest_ds_base); break;
    case 4: segment=vmread(vm_guest_fs_base); break;
    case 5: segment=vmread(vm_guest_gs_base); break;
  }

  return segment+baseaddress+index;
}

int handle_vmclear(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{
  //
  QWORD address=getDestinationAddressFromInstructionInfo(vmregisters);

  sendstringf("handle_vmclear(%6)\n", address);

  if ((currentcpuinfo->vmxdata.insideVMXRootMode==0) || (address==0))
  {
    sendstringf("invalid mode\n");
    return raiseInvalidOpcodeException(currentcpuinfo);
  }

  //make sure the vmcb points to theirs

  int error;
  QWORD pagefault;
  QWORD physicaladdress;
  QWORD *ppa=mapVMmemory(currentcpuinfo, address, 8, &error, &pagefault);

  if (error==2)
    return raisePagefault(currentcpuinfo, pagefault);

  physicaladdress=*ppa;
  unmapVMmemory(ppa,8);

  sendstringf("physical address=%6\n", physicaladdress);

  int r;
  if (currentcpuinfo->vmxdata.guest_activeVMCS==physicaladdress)
  {
    if (currentcpuinfo->vmxdata.guest_activeVMCS!=0xffffffffffffffff) //if there is an old VMCS, restore the hoststate with the original first
    {
      sendstringf("Restoring the guest vmx hoststate with original\n");
      r=vmptrld(currentcpuinfo->vmxdata.guest_activeVMCS);
      if (r==0)
        setHostState((void *)&currentcpuinfo->vmxdata.originalhoststate);
      else
      {
        sendstring("Failure restoring guest vmx hoststate");
        ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
        while (1);
      }

    }

    r=vmclear(currentcpuinfo->vmxdata.guest_activeVMCS);
  }
  else
    r=vmclear(physicaladdress); //vmclear on an address not the current vmcs. It shouldn't be modded so no need to change

  sendstringf("vmclear returned %d\n", r);

  if (r==0) //success
  {
    vmsucceed();

    if (currentcpuinfo->vmxdata.guest_activeVMCS==address)
    {
      currentcpuinfo->vmxdata.guest_activeVMCS=0xffffffffffffffffull;
    }
  }
  else
  {
    if (r==1)
    {
      VMfailInvalid();
    }
    else
    {
      int error=vmread(vm_errorcode);
      sendstringf("vmclear error: %d\n", error);
      VMfailValid(currentcpuinfo, error);
    }
  }

  //if (address==0)

  vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));

  return 0;
}

int handle_vmlaunchFail(pcpuinfo currentcpuinfo)
{
  sendstring("handle_vmlaunchFail\n");
  vmptrld(currentcpuinfo->vmcs_regionPA);
  return 0;
}

int handle_vmlaunch(pcpuinfo currentcpuinfo, VMRegisters *vmregisters UNUSED)
{
  if (currentcpuinfo->vmxdata.insideVMXRootMode==0)
    return raiseInvalidOpcodeException(currentcpuinfo);

  vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));

  if (currentcpuinfo->vmxdata.guest_activeVMCS==0xffffffffffffffffULL)
  {
    VMfailInvalid();
    return 0;
  }

  int r=vmptrld(currentcpuinfo->vmxdata.guest_activeVMCS);

  if (r)
  {
    sendstringf("handle_vmlaunch: failure to load guest vmcs\n");
    VMfailInvalid();
    return 0;
  }


  //no need to make modifications here, as they where made when ptrld was called
  if (currentcpuinfo->vmxdata.runningvmx)
  {
    sendstring("Assertion failed. runningvmx was not 0\n");
    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
    while (1);
  }

  currentcpuinfo->vmxdata.runningvmx=1;


  return 0xce00; //launch with the last entrystate
}

int handle_vmresumeFail(pcpuinfo currentcpuinfo)
{
  sendstring("handle_vmresumeFail\n");
  vmptrld(currentcpuinfo->vmcs_regionPA);
  return 0;
}

int handle_vmresume(pcpuinfo currentcpuinfo, VMRegisters *vmregisters UNUSED)
{
  sendstring("handle_vmresume\n");
  if (currentcpuinfo->vmxdata.insideVMXRootMode==0)
    return raiseInvalidOpcodeException(currentcpuinfo);

  vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));

  if (currentcpuinfo->vmxdata.guest_activeVMCS==0xffffffffffffffffULL)
  {
    VMfailInvalid();
    return 0;
  }


  int r=vmptrld(currentcpuinfo->vmxdata.guest_activeVMCS);

  if (r)
  {
    sendstringf("handle_vmresume: failure to load guest vmcs\n");
    VMfailInvalid();
    return 0;
  }

  if (currentcpuinfo->vmxdata.runningvmx)
  {
    sendstring("Assertion failed. runningvmx was not 0\n");
    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
    while (1);
  }

  currentcpuinfo->vmxdata.runningvmx=1;

  return 0xce01; //launch with the last entrystate and do a resume
}

int handle_invept(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{
  int vmcsinvepterror=0;
  int vmcsinvepterrorcode=0;

  sendstringf("handle_invept\n");
  if (currentcpuinfo->vmxdata.insideVMXRootMode==0)
    return raiseInvalidOpcodeException(currentcpuinfo);

  instruction_info_vm ii;
  ii.instructioninfo=vmread(vm_instruction_information); //e.g: 7361d130 for invept rdi,[rsi]
  /*  0111 0 0110 1 1000 011 1010 0 010 0110 0 00
   *
   * scaling=00     (0)
   * reserved=0     (0)
   * reg1=0110      (6)
   * addresSize=010 (2)
   * usesreg = 0    (0)
   * undefined =1010
   * segmentreg: 011 (3)
   * indexreg: 1000  (10)
   * indexreginvalid: 1
   * basereg: 0110 (6)
   * basereginvalid: 0
   * reg2: 0111 (7)
   *
   *reg1=rsi
   *basereg=rsi
   *reg2=rdi
   */
  int invepttype=regToVal(ii.reg2, vmregisters);
  QWORD inveptdataaddress=getDestinationAddressFromInstructionInfo(vmregisters);

  //read the pointerinfo
  int error;
  QWORD pagefault;

  sendstringf("invept: Type=%d data=%6\n", invepttype, inveptdataaddress);


  PINVEPTDESCRIPTOR inveptdescriptor=mapVMmemory(currentcpuinfo, inveptdataaddress, 16, &error, &pagefault);
  if (error==2)
    return raisePagefault(currentcpuinfo, pagefault);




  if (vmptrld(currentcpuinfo->vmxdata.guest_activeVMCS))
  {
    sendstringf("Failure to load guest active vmcs state");
    VMfailInvalid();
    unmapVMmemory(inveptdescriptor, 16);
    return 0;
  }

  //do the invept instruction

  vmcsinvepterror=_invept2(invepttype, inveptdescriptor);
  if (vmcsinvepterror==2) //error happened and it had an errorcode
    vmcsinvepterrorcode=vmread(vm_errorcode);

  //back to the original state
  vmptrld(currentcpuinfo->vmcs_regionPA);

  unmapVMmemory(inveptdescriptor, 16);

  vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));

  if (vmcsinvepterror)
  {
    if (vmcsinvepterror==1)
      VMfailInvalid();
    else
      VMfailValid(currentcpuinfo, vmcsinvepterrorcode);
  }
  else
    vmsucceed();

  return 0;
}

int handle_invvpid(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{
  int vmcsinvvpiderror=0;
  int vmcsinvvpiderrorcode=0;

  sendstringf("handle_invept\n");
  if (currentcpuinfo->vmxdata.insideVMXRootMode==0)
    return raiseInvalidOpcodeException(currentcpuinfo);

  instruction_info_vm ii;
  ii.instructioninfo=vmread(vm_instruction_information); //e.g: 7361d130 for invept rdi,[rsi]
  /*  0111 0 0110 1 1000 011 1010 0 010 0110 0 00
   *
   * scaling=00     (0)
   * reserved=0     (0)
   * reg1=0110      (6)
   * addresSize=010 (2)
   * usesreg = 0    (0)
   * undefined =1010
   * segmentreg: 011 (3)
   * indexreg: 1000  (10)
   * indexreginvalid: 1
   * basereg: 0110 (6)
   * basereginvalid: 0
   * reg2: 0111 (7)
   *
   *reg1=rsi
   *basereg=rsi
   *reg2=rdi
   */
  int invvpidtype=regToVal(ii.reg2, vmregisters);
  QWORD invvpiddataaddress=getDestinationAddressFromInstructionInfo(vmregisters);

  //read the pointerinfo
  int error;
  QWORD pagefault;

  sendstringf("invvpid: Type=%d data=%6\n", invvpidtype, invvpiddataaddress);


  PINVVPIDDESCRIPTOR invvpiddescriptor=mapVMmemory(currentcpuinfo, invvpiddataaddress, 16, &error, &pagefault);
  if (error==2)
    return raisePagefault(currentcpuinfo, pagefault);




  if (vmptrld(currentcpuinfo->vmxdata.guest_activeVMCS))
  {
    sendstringf("Failure to load guest active vmcs state");
    VMfailInvalid();
    unmapVMmemory(invvpiddescriptor, 16);
    return 0;
  }

  //do the invept instruction

  vmcsinvvpiderror=_invvpid2(invvpidtype, invvpiddescriptor);
  if (vmcsinvvpiderror==2) //error happened and it had an errorcode
    vmcsinvvpiderrorcode=vmread(vm_errorcode);

  //back to the original state
  vmptrld(currentcpuinfo->vmcs_regionPA);

  unmapVMmemory(invvpiddescriptor, 16);

  vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));

  if (vmcsinvvpiderror)
  {
    sendstringf("invept error");
    if (vmcsinvvpiderrorcode==1)
      VMfailInvalid();
    else
    {
      VMfailValid(currentcpuinfo, vmcsinvvpiderrorcode);
      sendstringf("%d", vmcsinvvpiderrorcode);
    }

    sendstring("\n");
  }
  else
    vmsucceed();

  return 0;
}



int handle_vmread(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{
  //read from the original
  //r/m64,r64
  int vmcsreaderror=0;
  int vmcsreaderrorcode=0;
  QWORD vmcsfield; //vmcs field
  QWORD destinationaddress=0;
  int destinationreg=-1;
  QWORD vmcsvalue;


  sendstring("handle_vmread\n");
  if (currentcpuinfo->vmxdata.insideVMXRootMode==0)
  {
    sendstringf("invalid mode\n");
    return raiseInvalidOpcodeException(currentcpuinfo);
  }


  if (currentcpuinfo->vmxdata.guest_activeVMCS==0xffffffffffffffffULL)
  {
    sendstring("currentcpuinfo->vmxdata.guest_activeVMCS==0xffffffffffffffff\n");
    VMfailInvalid();
    return 0;
  }



  instruction_info_vm ii;
  ii.instructioninfo=vmread(vm_instruction_information); //7061cd00  = RAX,RDI
  //0111 0 0000 1 1000 011 1001 1 010 0000 0 00                         r1,r2



  vmcsfield=regToVal(ii.reg2, vmregisters);

  sendstringf("vmread(%x)\n", vmcsfield);


  if (ii.usesreg==0)
  {
    //
    //calculate the address
    destinationaddress=getDestinationAddressFromInstructionInfo(vmregisters);
    sendstringf("instructioninfo=%8\n", ii.instructioninfo);
    sendstringf("destinationaddress=%6\n", destinationaddress);
  }
  else
  {
    destinationreg=ii.reg1;
  }


  if (vmptrld(currentcpuinfo->vmxdata.guest_activeVMCS))
  {
    sendstringf("Failure to load guest active vmcs state");
    VMfailInvalid();
    return 0;
  }


  //get the field value, and check if it's valid to read
  vmcsreaderror=vmread2(vmcsfield, &vmcsvalue);
  if (vmcsreaderror)
  {
    //update the eflags
    if (vmcsreaderror==1)
      VMfailInvalid();
    else
      vmcsreaderrorcode=vmread(vm_errorcode);
  }



  //check if it's a host field that needs to be hidden
  if ((((vmcsfield >> 10) & 3)==3) && (vmcsreaderror==0))
  {
    //change the result

    switch (vmcsfield)
    {
      case vm_host_es: vmcsvalue=currentcpuinfo->vmxdata.originalhoststate.ES; break;
      case vm_host_cs: vmcsvalue=currentcpuinfo->vmxdata.originalhoststate.CS; break;
      case vm_host_ss: vmcsvalue=currentcpuinfo->vmxdata.originalhoststate.SS; break;
      case vm_host_ds: vmcsvalue=currentcpuinfo->vmxdata.originalhoststate.DS; break;
      case vm_host_fs: vmcsvalue=currentcpuinfo->vmxdata.originalhoststate.FS; break;
      case vm_host_gs: vmcsvalue=currentcpuinfo->vmxdata.originalhoststate.GS; break;
      case vm_host_tr: vmcsvalue=currentcpuinfo->vmxdata.originalhoststate.TR; break;
      case vm_host_IA32_PAT: vmcsvalue=currentcpuinfo->vmxdata.originalhoststate.IA32_PAT; break;
      case vm_host_IA32_EFER: vmcsvalue=currentcpuinfo->vmxdata.originalhoststate.IA32_EFER; break;
      case vm_host_IA32_PERF_GLOBAL_CTRL: vmcsvalue=currentcpuinfo->vmxdata.originalhoststate.IA32_PERF_GLOBAL_CTRL; break;
      case vm_host_IA32_SYSENTER_CS: vmcsvalue=currentcpuinfo->vmxdata.originalhoststate.IA32_SYSENTER_CS; break;
      case vm_host_cr0: vmcsvalue=currentcpuinfo->vmxdata.originalhoststate.CR0; break;
      case vm_host_cr3: vmcsvalue=currentcpuinfo->vmxdata.originalhoststate.CR3; break;
      case vm_host_cr4: vmcsvalue=currentcpuinfo->vmxdata.originalhoststate.CR4; break;
      case vm_host_fs_base: vmcsvalue=currentcpuinfo->vmxdata.originalhoststate.FS_BASE; break;
      case vm_host_gs_base: vmcsvalue=currentcpuinfo->vmxdata.originalhoststate.GS_BASE; break;
      case vm_host_tr_base: vmcsvalue=currentcpuinfo->vmxdata.originalhoststate.TR_BASE; break;
      case vm_host_gdtr_base: vmcsvalue=currentcpuinfo->vmxdata.originalhoststate.GDTR_BASE; break;
      case vm_host_idtr_base: vmcsvalue=currentcpuinfo->vmxdata.originalhoststate.IDTR_BASE; break;
      case vm_host_IA32_SYSENTER_ESP: vmcsvalue=currentcpuinfo->vmxdata.originalhoststate.IA32_SYSENTER_ESP; break;
      case vm_host_IA32_SYSENTER_EIP: vmcsvalue=currentcpuinfo->vmxdata.originalhoststate.IA32_SYSENTER_EIP; break;
      case vm_host_rsp: vmcsvalue=currentcpuinfo->vmxdata.originalhoststate.RSP; break;
      case vm_host_rip: vmcsvalue=currentcpuinfo->vmxdata.originalhoststate.RIP; break;
    }
  }
  else
  {
    if (vmcsfield==vm_errorcode)
    {
      if (currentcpuinfo->vmxdata.currenterrorcode)
        vmcsvalue=currentcpuinfo->vmxdata.currenterrorcode;
    }
  }



  //back to the original state
  vmptrld(currentcpuinfo->vmcs_regionPA);


  //find the destination
  if (destinationreg!=-1)
  {
    if (vmcsreaderror==0)
      valToReg(destinationreg, vmcsvalue, vmregisters);
  }
  else
  {
    //map the destination address
    int error;
    QWORD pagefault;


    if (ii.addressSize==0)
    {
      //2 byte
      WORD *v=mapVMmemory(currentcpuinfo, destinationaddress, 2, &error, &pagefault);
      if (error==2)
        return raisePagefault(currentcpuinfo, pagefault);

      if (vmcsreaderror==0)
        *v=vmcsvalue;
      unmapVMmemory(v, 2);
    }
    else
    if (ii.addressSize==1)
    {
      DWORD *v=mapVMmemory(currentcpuinfo, destinationaddress, 4, &error, &pagefault);
      if (error==2)
        return raisePagefault(currentcpuinfo, pagefault);

      if (vmcsreaderror==0)
        *v=vmcsvalue;
      unmapVMmemory(v, 4);
    }
    else
    {
      QWORD *v=mapVMmemory(currentcpuinfo, destinationaddress, 8, &error, &pagefault);
      if (error==2)
        return raisePagefault(currentcpuinfo, pagefault);

      if (vmcsreaderror==0)
        *v=vmcsvalue;
      unmapVMmemory(v, 8);
    }
  }


  //still here so no pagefaults
  if (vmcsreaderror==0)
    vmsucceed();
  else
  {
    if (vmcsreaderror==1)
      VMfailInvalid();
    else
      VMfailValid(currentcpuinfo, vmcsreaderrorcode);
  }


  vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));
  return 0;
}

int handle_vmwrite(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{
  //r64,r/m64
  int vmcswriteerror=0;
  int vmcswriteerrorcode=0;
  QWORD vmcsfield; //vmcs field
  QWORD sourceaddress=0;
  int sourcereg=-1;
  QWORD vmcsvalue;

  sendstring("handle_vmwrite\n");

  if (currentcpuinfo->vmxdata.insideVMXRootMode==0)
    return raiseInvalidOpcodeException(currentcpuinfo);

  if (currentcpuinfo->vmxdata.guest_activeVMCS==0xffffffffffffffffULL)
  {
    VMfailInvalid();
    return 0;
  }



  instruction_info_vm ii;
  ii.instructioninfo=vmread(vm_instruction_information);

  vmcsfield=regToVal(ii.reg2, vmregisters);
  sendstringf("vmwrite(%x)\n", vmcsfield);

  if (ii.usesreg==0)
  {
    //
    //calculate the address
    sendstringf("ii.usesreg==0");

    sourceaddress=getDestinationAddressFromInstructionInfo(vmregisters);
    sendstringf("instructioninfo=%8\n", ii.instructioninfo);
    sendstringf("sourceaddress=%6\n", sourceaddress);
  }
  else
  {
    sourcereg=ii.reg1;
  }

  //find the source
  if (sourcereg!=-1)
  {
    vmcsvalue=regToVal(sourcereg, vmregisters);
  }
  else
  {
    //map the destination address
    int error;
    QWORD pagefault;


    if (ii.addressSize==0)
    {
      //2 byte
      WORD *v=mapVMmemory(currentcpuinfo, sourceaddress, 2, &error, &pagefault);
      if (error==2)
        return raisePagefault(currentcpuinfo, pagefault);

      vmcsvalue=*v;
      unmapVMmemory(v, 2);
    }
    else
    if (ii.addressSize==1)
    {
      DWORD *v=mapVMmemory(currentcpuinfo, sourceaddress, 4, &error, &pagefault);
      if (error==2)
        return raisePagefault(currentcpuinfo, pagefault);

      vmcsvalue=*v;
      unmapVMmemory(v, 4);
    }
    else
    {
      QWORD *v=mapVMmemory(currentcpuinfo, sourceaddress, 8, &error, &pagefault);
      if (error==2)
        return raisePagefault(currentcpuinfo, pagefault);

      vmcsvalue=*v;
      unmapVMmemory(v, 8);
    }
  }

  if (vmptrld(currentcpuinfo->vmxdata.guest_activeVMCS))
  {
    sendstringf("Failure to load the guest vmcs\n");
    VMfailInvalid();
    return 0;
  }

  if (((vmcsfield >> 10) & 3)==3) //host field
  {
    //update the original state instead of the vmcs

    switch (vmcsfield)
    {
      case vm_host_es: currentcpuinfo->vmxdata.originalhoststate.ES=vmcsvalue; break;
      case vm_host_cs: currentcpuinfo->vmxdata.originalhoststate.CS=vmcsvalue; break;
      case vm_host_ss: currentcpuinfo->vmxdata.originalhoststate.SS=vmcsvalue; break;
      case vm_host_ds: currentcpuinfo->vmxdata.originalhoststate.DS=vmcsvalue; break;
      case vm_host_fs: currentcpuinfo->vmxdata.originalhoststate.FS=vmcsvalue; break;
      case vm_host_gs: currentcpuinfo->vmxdata.originalhoststate.GS=vmcsvalue; break;
      case vm_host_tr: currentcpuinfo->vmxdata.originalhoststate.TR=vmcsvalue; break;
      case vm_host_IA32_PAT: currentcpuinfo->vmxdata.originalhoststate.IA32_PAT=vmcsvalue; break;
      case vm_host_IA32_EFER: currentcpuinfo->vmxdata.originalhoststate.IA32_EFER=vmcsvalue; break;
      case vm_host_IA32_PERF_GLOBAL_CTRL: currentcpuinfo->vmxdata.originalhoststate.IA32_PERF_GLOBAL_CTRL=vmcsvalue; break;
      case vm_host_IA32_SYSENTER_CS: currentcpuinfo->vmxdata.originalhoststate.IA32_SYSENTER_CS=vmcsvalue; break;
      case vm_host_cr0: currentcpuinfo->vmxdata.originalhoststate.CR0=vmcsvalue; break;
      case vm_host_cr3: currentcpuinfo->vmxdata.originalhoststate.CR3=vmcsvalue; break;
      case vm_host_cr4: currentcpuinfo->vmxdata.originalhoststate.CR4=vmcsvalue; break;
      case vm_host_fs_base: currentcpuinfo->vmxdata.originalhoststate.FS_BASE=vmcsvalue; break;
      case vm_host_gs_base: currentcpuinfo->vmxdata.originalhoststate.GS_BASE=vmcsvalue; break;
      case vm_host_tr_base: currentcpuinfo->vmxdata.originalhoststate.TR_BASE=vmcsvalue; break;
      case vm_host_gdtr_base: currentcpuinfo->vmxdata.originalhoststate.GDTR_BASE=vmcsvalue; break;
      case vm_host_idtr_base: currentcpuinfo->vmxdata.originalhoststate.IDTR_BASE=vmcsvalue; break;
      case vm_host_IA32_SYSENTER_ESP: currentcpuinfo->vmxdata.originalhoststate.IA32_SYSENTER_ESP=vmcsvalue; break;
      case vm_host_IA32_SYSENTER_EIP: currentcpuinfo->vmxdata.originalhoststate.IA32_SYSENTER_EIP=vmcsvalue; break;
      case vm_host_rsp: currentcpuinfo->vmxdata.originalhoststate.RSP=vmcsvalue; break;
      case vm_host_rip: currentcpuinfo->vmxdata.originalhoststate.RIP=vmcsvalue; break;
    }

    vmsucceed();
  }
  else //pass through
  {
    vmcswriteerror=vmwrite2(vmcsfield, vmcsvalue);
    if (vmcswriteerror==2)
      vmcswriteerrorcode=vmread(vm_errorcode);
  }

  vmptrld(currentcpuinfo->vmcs_regionPA);
  vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));

  if (vmcswriteerror)
  {
    if (vmcswriteerror==1)
      VMfailInvalid();
    else
      VMfailValid(currentcpuinfo, vmcswriteerrorcode);
  }
  else
    vmsucceed();

  return 0;
}

int handle_vmptrld(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{

  QWORD address=getDestinationAddressFromInstructionInfo(vmregisters);
  int error;
  QWORD pagefaultaddress;


  sendstringf("handle_vmptrld(%6)\n", address);



  if (currentcpuinfo->vmxdata.insideVMXRootMode==0)
  {
    sendstring("vmptrld while not inside vmxroot\n");
    return raiseInvalidOpcodeException(currentcpuinfo);
  }


  QWORD *mapped=mapVMmemory(currentcpuinfo, address, 8, &error, &pagefaultaddress);
  if (error==2)
  {
    sendstringf("vmptrld: failure to map %6.  Pagefault at %6\n", address, pagefaultaddress);
    return raisePagefault(currentcpuinfo, pagefaultaddress);
  }

  address=*mapped;
  unmapVMmemory(mapped, 8);


  if (currentcpuinfo->vmxdata.dbvmhoststate.FS_BASE==0) //only needs one time init
    getHostState((vmxhoststate *)&currentcpuinfo->vmxdata.dbvmhoststate);

  vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));

  int r;

  if (currentcpuinfo->vmxdata.guest_activeVMCS!=0xffffffffffffffff) //if there is an old VMCS, restore the hoststate with the original first
  {
    sendstringf("This guest already has a VMCS. Restore it\n");
    r=vmptrld(currentcpuinfo->vmxdata.guest_activeVMCS);
    if (r==0)
    {
      sendstringf("restore: vmptrld success\n");
      setHostState((void *)&currentcpuinfo->vmxdata.originalhoststate);

      sendstringf("CR3=%6\n", vmread(vm_guest_cr3));

      r=vmptrld(currentcpuinfo->vmcs_regionPA); //back to normal
      if (r)
      {
        sendstringf("restore: vmptrld failed: %d (error=%d)\n", vmread(vm_errorcode));
        ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
        while (1);
      }
    }
    else
    {
      sendstringf("restore: vmptrld failed: %d (error=%d)\n", vmread(vm_errorcode));
      //VMfailInvalid();
      //return 0;
      //it's a load, the old state could have been deleted
    }
  }

  //now load the new vmptr


  if (address==currentcpuinfo->vmxdata.guest_vmxonaddress)
  {
    sendstringf("address==currentcpuinfo->vmxdata.guest_vmxonaddress\n");
    VMfail(currentcpuinfo, 10); //VMPTRLD with VMXON pointer
    return 0;
  }


  sendstringf("calling vmptrld with address %6", address);
  r=vmptrld(address);
  sendstringf("vmptrld(%6) returned %d\n", address, r);

  if (r==0) //store the original hoststate
  {
    sendstringf("Storing the original hoststate\n");
    getHostState((void *)&currentcpuinfo->vmxdata.originalhoststate);
    currentcpuinfo->vmxdata.guest_activeVMCS=address;
  }
  else
  {
    if (r==1)
    {
      VMfailInvalid();
      return 0;
    }

    if (r==2)
    {
      int error=vmread(vm_errorcode);
      sendstringf("vmptrld error %d\n", error);
      VMfail(currentcpuinfo,error);
      return 0;
    }
  }

  //change the hoststate so it exits on dbvm
  sendstring("Editing the guest vmcs state so it exits on this host\n");
  setHostState((vmxhoststate *)&currentcpuinfo->vmxdata.dbvmhoststate);

  //debug code, can go
  vmxhoststate debugstate;
  getHostState(&debugstate);

  if (debugstate.CR3!=getCR3())
  {
    sendstringf("wrong cr3 was set in the hoststate");
    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
    while (1);
  }

  //restore to normal (until launch/resume)
  sendstring("Calling vmptrld to the default\n");
  vmptrld(currentcpuinfo->vmcs_regionPA);

  sendstringf("Calling vmsucceed\n");
  vmsucceed();


  return 0;
}

int handle_vmptrst(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{
  QWORD address=getDestinationAddressFromInstructionInfo(vmregisters);
  int error;
  QWORD pagefaultaddress;

  if (currentcpuinfo->vmxdata.insideVMXRootMode==0)
    return raiseInvalidOpcodeException(currentcpuinfo);

  QWORD *mapped=mapVMmemory(currentcpuinfo, address, 8, &error, &pagefaultaddress);

  if (error==2)
  {
    return raisePagefault(currentcpuinfo, pagefaultaddress);
  }

  *mapped=currentcpuinfo->vmxdata.guest_activeVMCS;

  unmapVMmemory(mapped, 8);

  vmsucceed();
  return 0;
}

int handle_vmxon(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{
  QWORD address=getDestinationAddressFromInstructionInfo(vmregisters);

  int error;
  QWORD pagefaultaddress;

  sendstringf("handle_vmxon %6\n", address);

  if ((vmread(vm_guest_cr4) & CR4_VMXE)==0)
  {
    sendstringf("CR4 VMXE is disabled\n");
    return raiseInvalidOpcodeException(currentcpuinfo);
  }

  QWORD *mapped=mapVMmemory(currentcpuinfo, address, 8, &error, &pagefaultaddress);

  if (error==2)
    return raisePagefault(currentcpuinfo, pagefaultaddress);



  currentcpuinfo->vmxdata.guest_vmxonaddress=*mapped;
  currentcpuinfo->vmxdata.guest_activeVMCS=0xffffffffffffffff;
  currentcpuinfo->vmxdata.insideVMXRootMode=1;

  unmapVMmemory(mapped, 8);

  vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));

  vmsucceed();
  return 0;
}

int handle_vmxoff(pcpuinfo currentcpuinfo, VMRegisters *vmregisters UNUSED)
{
  sendstringf("handle_vmxoff\n");
  if (currentcpuinfo->vmxdata.insideVMXRootMode==0)
    return raiseInvalidOpcodeException(currentcpuinfo);


  if (currentcpuinfo->vmxdata.guest_activeVMCS!=0xffffffffffffffff) //if there is an old VMCS, restore the hoststate with the original first
  {
    int r;
    sendstringf("Restoring the guest vmx hoststate with original\n");
    r=vmptrld(currentcpuinfo->vmxdata.guest_activeVMCS);
    if (r==0)
      setHostState((void *)&currentcpuinfo->vmxdata.originalhoststate);

    vmptrld(currentcpuinfo->vmcs_regionPA);

    currentcpuinfo->vmxdata.guest_activeVMCS=0xffffffffffffffffULL;
  }


  currentcpuinfo->vmxdata.insideVMXRootMode=0;
  vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));

  vmsucceed();
  return 0;
}





int handleIntelVMXInstruction(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{
  int r;
  nosendchar[getAPICID()]=0;
  sendstring(" - nested vm is not 100% working\n");

  sendstringf("handleIntelVMXInstruction. emulatevmx=%d\n", emulatevmx);
  if (emulatevmx==0)
    return raiseInvalidOpcodeException(currentcpuinfo);
  else
  {
    //try to handle it

    int exit_reason=currentcpuinfo->guest_error?currentcpuinfo->guest_error:vmread(vm_exit_reason) & 0x7fffffff;
    currentcpuinfo->guest_error=0;
    currentcpuinfo->vmxdata.currenterrorcode=0;



    try
    {

      switch (exit_reason)
      {
      /*
  19 VMCLEAR. Guest software attempted to execute VMCLEAR.
  20 VMLAUNCH. Guest software attempted to execute VMLAUNCH.
  21 VMPTRLD. Guest software attempted to execute VMPTRLD.
  22 VMPTRST. Guest software attempted to execute VMPTRST.
  23 VMREAD. Guest software attempted to execute VMREAD.
  24 VMRESUME. Guest software attempted to execute VMRESUME.
  25 VMWRITE. Guest software attempted to execute VMWRITE.
  26 VMXOFF. Guest software attempted to execute VMXOFF.
  27 VMXON. Guest software attempted to execute VMXON.
       */
        case 19: r=handle_vmclear(currentcpuinfo, vmregisters); break;
        case 20: r=handle_vmlaunch(currentcpuinfo, vmregisters); break;
        case 0xce00: r=handle_vmlaunchFail(currentcpuinfo); break;
        case 21: r=handle_vmptrld(currentcpuinfo, vmregisters); break;
        case 22: r=handle_vmptrst(currentcpuinfo, vmregisters); break;
        case 23: r=handle_vmread(currentcpuinfo, vmregisters); break;
        case 24: r=handle_vmresume(currentcpuinfo, vmregisters); break;
        case 0xce01: r=handle_vmresumeFail(currentcpuinfo); break;
        case 25: r=handle_vmwrite(currentcpuinfo, vmregisters); break;
        case 26: r=handle_vmxoff(currentcpuinfo, vmregisters); break;
        case 27: r=handle_vmxon(currentcpuinfo, vmregisters); break;
        case vm_exit_invept:  r=handle_invept(currentcpuinfo, vmregisters); break;
        case vm_exit_invvpid: r=handle_invvpid(currentcpuinfo, vmregisters); break;
        default:
        {
          sendstringf("vmxemu.c: Invalid exit_reason: %d\n", exit_reason);
          return 1;
        }
      }
    }
    except
    {
      int err=lastexception;

      sendstringf("Something shitty happened when emulating VMX (%6: %d)\n", ExceptionRIP, err);
      ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);

      while (1);
    }
    tryend

    return r;

  }
}
