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
#include "vmeventhandler_amd.h"
#include "vmcall.h"
#include "mm.h"
#include "displaydebug.h"

#include "vmxsetup.h"
#include "msrnames.h"
#include "apic.h"


int emulatevmx=1;

//todo: vmcs link pointer implementation

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


DWORD * getVMCS(pcpuinfo currentcpuinfo, QWORD PA)
//cached lookup for the VMCA PA to a mapped VA
{
  int i;
  DWORD *r;
  //first check if this is in the list
  for (i=0; i<10; i++)
  {
    if (currentcpuinfo->vmxdata.mappedVMCSBlocks[i].VMCS_PhysicalAddress==PA)
      return currentcpuinfo->vmxdata.mappedVMCSBlocks[i].VMCS_VirtualAddress;

    if (currentcpuinfo->vmxdata.mappedVMCSBlocks[i].VMCS_PhysicalAddress==0)
    {
      currentcpuinfo->vmxdata.mappedVMCSBlocks_nextIndex=i;
      break;
    }
  }
  //still here so not mapped. Map at mappedVMCSBlocks_nextIndex

  i=currentcpuinfo->vmxdata.mappedVMCSBlocks_nextIndex;
  if (currentcpuinfo->vmxdata.mappedVMCSBlocks[i].VMCS_VirtualAddress) //clean this one first
    unmapPhysicalMemory(currentcpuinfo->vmxdata.mappedVMCSBlocks[i].VMCS_VirtualAddress, 4096);

  r=(DWORD *)mapPhysicalMemory(PA,4096);
  currentcpuinfo->vmxdata.mappedVMCSBlocks[i].VMCS_PhysicalAddress=PA;
  currentcpuinfo->vmxdata.mappedVMCSBlocks[i].VMCS_VirtualAddress=r;

  currentcpuinfo->vmxdata.mappedVMCSBlocks_nextIndex=(currentcpuinfo->vmxdata.mappedVMCSBlocks_nextIndex+1) % 10;
  return r;

}

void emulateVMEntry(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{
  nosendchar[getAPICID()]=0;
  //sendstring("emulateVMEntry");

  if (currentcpuinfo->vmxdata.dbvmhoststate.RIP==0) //only needs one time init
    getHostState((vmxhoststate *)&currentcpuinfo->vmxdata.dbvmhoststate);


  if (hasVMCSShadowingSupport)
  {
    vmclear(currentcpuinfo->vmxdata.guest_activeVMCS);
   // sendstringf("Marking guest activeVMCS as normal");
    DWORD *vmcsheader=getVMCS(currentcpuinfo, currentcpuinfo->vmxdata.guest_activeVMCS);
    *vmcsheader &= ~(1<<31);
  }

  int r=vmptrld(currentcpuinfo->vmxdata.guest_activeVMCS);

  if (r)
  {
    sendstringf("emulateVMEntry: failure to load guest vmcs\n");
    VMfailInvalid();
    return;
  }


  //if (vmread(vm_guest_activity_state)==0) //debug code to force it to run 1 thing
  //  vmwrite(vm_guest_interruptability_state, BLOCKINGBYSTI);
  getHostState(&currentcpuinfo->vmxdata.originalhoststate);
  setHostState(&currentcpuinfo->vmxdata.dbvmhoststate);


  //sendvmstateFull(currentcpuinfo, vmregisters);

}

int emulateVMExit(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{
  //emulate a vmexit for the host (so switch to the default VMCS)
  QWORD vmexitcontrol=vmread(vm_exit_controls);
  QWORD vmexecutioncontrol=vmread(vm_execution_controls_cpu);

  int HostAddressSpaceSize=vmexitcontrol & VMEXITC_HOST_ADDRESS_SPACE_SIZE;

  //jtagbp();
  nosendchar[getAPICID()]=0;

  //sendstring("emulateVMExit\n");



  if ((hasVPIDSupport) && (vmexecutioncontrol & SECONDARY_EXECUTION_CONTROLS))
  {
    QWORD vmexecutioncontrol2=vmread(vm_execution_controls_cpu_secondary);

    if ((vmexecutioncontrol2 & SPBEF_ENABLE_VPID)==0) //27.5.5 VPID 0000 gets cleared when ENABLE_VPID is false
    {
      INVVPIDDESCRIPTOR desc;
      desc.LinearAddress=0;
      desc.zero=0;
      desc.VPID=0;
      _invvpid(1,&desc);
    }
  }



  if (currentcpuinfo->vmxdata.runningvmx==0)
  {
    sendstringf("emulateVMExit was called while runningvmx is 0");
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

  //restore the original state before continue
  setHostState(&currentcpuinfo->vmxdata.originalhoststate);


  int ptrld=vmptrld(currentcpuinfo->vmcs_regionPA); //leave the virtual virtual machine vmcs and go bnack to the virtual machine vmcs

  if (ptrld)
  {
    sendstringf("failure loading the vmcs_regionPA\n");
    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
    while(1);
  }

  //sendstringf("ptrld=%d\n", ptrld);


  if (hasVMCSShadowingSupport)
  {
    vmclear(currentcpuinfo->vmxdata.guest_activeVMCS);
   // sendstringf("Marking guest activeVMCS as shadow");
    DWORD *vmcsheader=getVMCS(currentcpuinfo, currentcpuinfo->vmxdata.guest_activeVMCS);
    *vmcsheader |= (1<<31);
  }



  //27.5.1:
  vmwrite(vm_guest_cr0, currentcpuinfo->vmxdata.originalhoststate.CR0);
  vmwrite(vm_guest_cr3, currentcpuinfo->vmxdata.originalhoststate.CR3);
  currentcpuinfo->guestCR3=currentcpuinfo->vmxdata.originalhoststate.CR3;


  if (HostAddressSpaceSize)
    vmwrite(vm_guest_cr4, currentcpuinfo->vmxdata.originalhoststate.CR4 | CR4_PAE);
  else
    vmwrite(vm_guest_cr4, currentcpuinfo->vmxdata.originalhoststate.CR4 & ~CR4_PCIDE);



  vmwrite(vm_guest_dr7,0x400);
  vmwrite(vm_guest_IA32_DEBUGCTL,0);
  vmwrite(vm_guest_IA32_SYSENTER_CS, currentcpuinfo->vmxdata.originalhoststate.IA32_SYSENTER_CS);
  vmwrite(vm_guest_IA32_SYSENTER_ESP, currentcpuinfo->vmxdata.originalhoststate.IA32_SYSENTER_ESP);
  vmwrite(vm_guest_IA32_SYSENTER_EIP, currentcpuinfo->vmxdata.originalhoststate.IA32_SYSENTER_EIP);


  vmwrite(vm_guest_fs_base, currentcpuinfo->vmxdata.originalhoststate.FS_BASE);
  vmwrite(vm_guest_gs_base, currentcpuinfo->vmxdata.originalhoststate.GS_BASE);

  if (HostAddressSpaceSize)
    vmwrite(vm_guest_IA32_EFER, vmread(vm_guest_IA32_EFER) | EFER_LME | EFER_LMA);
  else
    vmwrite(vm_guest_IA32_EFER, vmread(vm_guest_IA32_EFER) & ~(EFER_LME | EFER_LMA));


  if (vmexitcontrol & VMEXITC_LOAD_IA32_PERF_GLOBAL_CTRL)
    vmwrite(vm_guest_IA32_PERF_GLOBAL_CTRL, currentcpuinfo->vmxdata.originalhoststate.IA32_PERF_GLOBAL_CTRL);

  if (vmexitcontrol & VMEXITC_LOAD_IA32_PAT)
    vmwrite(vm_guest_IA32_PAT, currentcpuinfo->vmxdata.originalhoststate.IA32_PAT);

  if (vmexitcontrol & VMEXITC_LOAD_IA32_EFER)
    vmwrite(vm_guest_IA32_EFER, currentcpuinfo->vmxdata.originalhoststate.IA32_EFER);

  if (vmexitcontrol & VMEXITC_CLEAR_IA32_BNDCFGS)
    vmwrite(vm_guest_IA32_BNDCFGS,0);

  if (vmexitcontrol & VMEXITC_CLEAR_IA32_RTIT_CTL)
    vmwrite(vm_guest_IA32_RTIT_CTL,0);

  if (vmexitcontrol & VMEXITC_LOAD_CET)
  {
    vmwrite(vm_guest_IA32_S_CET, currentcpuinfo->vmxdata.originalhoststate.IA32_S_CET);
    vmwrite(vm_guest_IA32_INTERRUPT_SSP_TABLE_ADDR, currentcpuinfo->vmxdata.originalhoststate.IA32_INTERRUPT_SSP_TABLE_ADDR);
  }

  if (vmexitcontrol & VMEXITC_LOAD_PKRS)
    vmwrite(vm_guest_IA32_PKRS, currentcpuinfo->vmxdata.originalhoststate.IA32_PKRS);

  //27.5.2:
  vmwrite(vm_guest_cs, currentcpuinfo->vmxdata.originalhoststate.CS);
  vmwrite(vm_guest_cs_base,0);
  vmwrite(vm_guest_cs_limit,0xffffffff);
  if (HostAddressSpaceSize)
    vmwrite(vm_guest_cs_access_rights, 0xa09b); //type=11, S=1, DPL=0, P=1, L=1, D_B=0, G=1
  else
    vmwrite(vm_guest_cs_access_rights, 0xc09b); //type=11, S=1, DPL=0, P=1, L=0, D_B=1, G=1

  vmwrite(vm_guest_es, currentcpuinfo->vmxdata.originalhoststate.ES);
  if ((currentcpuinfo->vmxdata.originalhoststate.ES) || HostAddressSpaceSize)
  {
    vmwrite(vm_guest_es_base,0);
    vmwrite(vm_guest_es_limit,0xffffffff);
  }
  else
    vmwrite(vm_guest_es_access_rights,0x10000);


  vmwrite(vm_guest_ds, currentcpuinfo->vmxdata.originalhoststate.DS);
  if ((currentcpuinfo->vmxdata.originalhoststate.DS) || HostAddressSpaceSize)
  {
    vmwrite(vm_guest_ds_base,0);
    vmwrite(vm_guest_ds_limit,0xffffffff);
    vmwrite(vm_guest_ds_access_rights, 0xc093); //type=3, S=1, DPL=0, P=1, L=0, D_B=1, G=1
  }
  else
    vmwrite(vm_guest_ds_access_rights,0x10000);


  vmwrite(vm_guest_ss, currentcpuinfo->vmxdata.originalhoststate.SS);
  vmwrite(vm_guest_ss_base,0);
  vmwrite(vm_guest_ss_limit,0xffffffff);
  vmwrite(vm_guest_ss_access_rights, 0xc093); //type=3, S=1, DPL=0, P=1, L=0, D_B=1, G=1


  vmwrite(vm_guest_fs, currentcpuinfo->vmxdata.originalhoststate.FS);
  vmwrite(vm_guest_fs_limit,0xffffffff);
  if ((currentcpuinfo->vmxdata.originalhoststate.FS) || HostAddressSpaceSize)
    vmwrite(vm_guest_fs_access_rights, 0xc093); //type=3, S=1, DPL=0, P=1, L=0, D_B=1, G=1
  else
    vmwrite(vm_guest_fs_access_rights,0x10000);


  vmwrite(vm_guest_gs, currentcpuinfo->vmxdata.originalhoststate.GS);
  vmwrite(vm_guest_gs_limit,0xffffffff);
  if ((currentcpuinfo->vmxdata.originalhoststate.GS) || HostAddressSpaceSize)
    vmwrite(vm_guest_gs_access_rights, 0xc093); //type=3, S=1, DPL=0, P=1, L=0, D_B=1, G=1
  else
    vmwrite(vm_guest_gs_access_rights,0x10000);

  vmwrite(vm_guest_tr, currentcpuinfo->vmxdata.originalhoststate.TR);
  vmwrite(vm_guest_tr_base, currentcpuinfo->vmxdata.originalhoststate.TR_BASE);
  vmwrite(vm_guest_tr_limit, 0x67);
  vmwrite(vm_guest_tr_access_rights, 0x8b); //type=11, S=0, DPL=0, P=1, L=0, D_B=0, G=0


  vmwrite(vm_guest_ldtr,0);
  vmwrite(vm_guest_ldtr_base,0);
  vmwrite(vm_guest_ldtr_limit,0);
  vmwrite(vm_guest_ldtr_access_rights,0x10000);


  vmwrite(vm_guest_gdtr_base, currentcpuinfo->vmxdata.originalhoststate.GDTR_BASE);
  vmwrite(vm_guest_gdt_limit,0xffff);
  vmwrite(vm_guest_idtr_base, currentcpuinfo->vmxdata.originalhoststate.IDTR_BASE);
  vmwrite(vm_guest_idt_limit,0xffff);


  //27.5.3:
  vmwrite(vm_guest_rip, currentcpuinfo->vmxdata.originalhoststate.RIP);
  vmwrite(vm_guest_rsp, currentcpuinfo->vmxdata.originalhoststate.RSP);
  vmwrite(vm_guest_rflags,2);

  if (vmexitcontrol & VMEXITC_LOAD_CET)
    vmwrite(vm_guest_SSP, currentcpuinfo->vmxdata.originalhoststate.SSP);

  //27.5.5
  vmwrite(vm_guest_activity_state,0); //it was supposed to be in a vmlaunch or vmresume instruction, but just making sure
  vmwrite(vm_guest_interruptability_state, (vmread(vm_guest_interruptability_state) & (~(BLOCKINGBYSTI | BLOCKINGBYSS)))  );
  vmwrite(vm_pending_debug_exceptions,0);

  //27.6 : skip  (The exit came from this specific vmcs, so it loaded the MSR's back already if it used this feature)

  //27.7: nope nope nope

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
  QWORD offset=vmread(vm_exit_qualification);
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

  return segment+baseaddress+index+offset;
}


int handle_vmlaunchFail(pcpuinfo currentcpuinfo)
{
  sendstring("handle_vmlaunchFail\n");
  sendstringf("VM error code=%8\n\r",  vmread(vm_errorcode));
  sendstringf("Exit reason=%8\n\r", vmread(vm_exit_reason));

  vmptrld(currentcpuinfo->vmcs_regionPA);
  currentcpuinfo->vmxdata.runningvmx=0;

  VMfailInvalid();



  return 0;
}

int handle_vmlaunch(pcpuinfo currentcpuinfo, VMRegisters *vmregisters UNUSED)
{
  nosendchar[getAPICID()]=0;
  //sendstring("handle_vmlaunch\n");
  if (currentcpuinfo->vmxdata.insideVMXRootMode==0)
    return raiseInvalidOpcodeException(currentcpuinfo);

  incrementRIP(vmread(vm_exit_instructionlength));

  if (currentcpuinfo->vmxdata.guest_activeVMCS==0xffffffffffffffffULL)
  {
    VMfailInvalid();
    return 0;
  }

  if (vmread(vm_guest_interruptability_state) & BLOCKINGBYSS)
  {
    VMfailValid(currentcpuinfo, 26);
    return 0;
  }

  //no need to make modifications here, as they where made when ptrld was called
  if (currentcpuinfo->vmxdata.runningvmx)
  {
    sendstring("Assertion failed. runningvmx was not 0\n");
    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
    while (1);
  }

  emulateVMEntry(currentcpuinfo, vmregisters);

  currentcpuinfo->vmxdata.runningvmx=1;

  //sendvmstateFull(currentcpuinfo, vmregisters);


  return 0xce00; //launch with the last entrystate
}

int handle_vmresumeFail(pcpuinfo currentcpuinfo)
{
  sendstring("handle_vmresumeFail\n");
  vmptrld(currentcpuinfo->vmcs_regionPA);
  currentcpuinfo->vmxdata.runningvmx=0;
  VMfailInvalid();
  return 0;
}

int handle_vmresume(pcpuinfo currentcpuinfo, VMRegisters *vmregisters UNUSED)
{
  nosendchar[getAPICID()]=0;
 // sendstring("handle_vmresume\n");
  if (currentcpuinfo->vmxdata.insideVMXRootMode==0)
    return raiseInvalidOpcodeException(currentcpuinfo);

  incrementRIP(vmread(vm_exit_instructionlength));

  if (currentcpuinfo->vmxdata.guest_activeVMCS==0xffffffffffffffffULL)
  {
    VMfailInvalid();
    return 0;
  }

  if (vmread(vm_guest_interruptability_state) & BLOCKINGBYSS)
  {
    VMfailValid(currentcpuinfo, 26);
    return 0;
  }

  if (currentcpuinfo->vmxdata.runningvmx)
  {
    sendstring("Assertion failed. runningvmx was not 0\n");
    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
    while (1);
  }

  currentcpuinfo->vmxdata.runningvmx=1;

  emulateVMEntry(currentcpuinfo, vmregisters);

  //sendvmstateFull(currentcpuinfo, vmregisters);


  //vmwrite(vm_guest_interruptability_state,BLOCKINGBYSTI);
  if (hasVMCSShadowingSupport)
    return 0xce00; //do a launch instead of resume

  return 0xce01; //launch with the last entrystate and do a resume
}

int handle_invept(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{
  int vmcsinvepterror=0;
  int vmcsinvepterrorcode=0;

 // sendstringf("handle_invept\n");
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

  //sendstringf("invept: Type=%d data=%6\n", invepttype, inveptdataaddress);


  PINVEPTDESCRIPTOR inveptdescriptor=mapVMmemory(currentcpuinfo, inveptdataaddress, 16, &error, &pagefault);
  if (error==2)
    return raisePagefault(currentcpuinfo, pagefault);

  if ((currentcpuinfo->vmxdata.guest_activeVMCS!=0xffffffffffffffffULL) && (vmptrld(currentcpuinfo->vmxdata.guest_activeVMCS)))
  {
    sendstringf("Failure to load guest active vmcs state (%6)\n",currentcpuinfo->vmxdata.guest_activeVMCS);
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

  incrementRIP(vmread(vm_exit_instructionlength));

  if (vmcsinvepterror)
  {
    sendstringf("vmcsinvepterror=%d\n", vmcsinvepterror);
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

  sendstringf("handle_invvpid\n");
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


  incrementRIP(vmread(vm_exit_instructionlength));


  if ((currentcpuinfo->vmxdata.guest_activeVMCS!=0xffffffffffffffffULL) && (vmptrld(currentcpuinfo->vmxdata.guest_activeVMCS)))
  {
    sendstringf("Failure to load guest active vmcs state (%6)\n", currentcpuinfo->vmxdata.guest_activeVMCS);
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



  if (vmcsinvvpiderror)
  {
    sendstringf("vmcsinvvpiderror=%d\n", vmcsinvvpiderror);
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
  int r;


  //sendstring("handle_vmread\n");
  if (currentcpuinfo->vmxdata.insideVMXRootMode==0)
  {
    sendstringf("vmread: invalid mode\n");
    return raiseInvalidOpcodeException(currentcpuinfo);
  }


  if (currentcpuinfo->vmxdata.guest_activeVMCS==0xffffffffffffffffULL)
  {
    sendstring("currentcpuinfo->vmxdata.guest_activeVMCS==0xffffffffffffffff\n");
    VMfailInvalid();
    incrementRIP(vmread(vm_exit_instructionlength));
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
    //sendstringf("instructioninfo=%8\n", ii.instructioninfo);
    //sendstringf("destinationaddress=%6\n", destinationaddress);
  }
  else
  {
    destinationreg=ii.reg1;
  }


  r=vmptrld(currentcpuinfo->vmxdata.guest_activeVMCS);
  if (r)
  {
    sendstringf("Failure to load guest active vmcs state (%6)  r=%d\n", currentcpuinfo->vmxdata.guest_activeVMCS, r);
    VMfailInvalid();
    incrementRIP(vmread(vm_exit_instructionlength));
    return 0;
  }


  //get the field value, and check if it's valid to read
  vmcsreaderror=vmread2(vmcsfield, &vmcsvalue);
  if (vmcsreaderror)
  {
    sendstringf("vmread2 error\n");
    //update the eflags
    if (vmcsreaderror==1)
    {
      VMfailInvalid();
    }
    else
      vmcsreaderrorcode=vmread(vm_errorcode);
  }


  //the host fields are already restored, so no need to emulate that


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

  incrementRIP(vmread(vm_exit_instructionlength));
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


  if (ii.usesreg==0)
  {
    //
    //calculate the address
    //sendstringf("ii.usesreg==0");

    sourceaddress=getDestinationAddressFromInstructionInfo(vmregisters);
    //sendstringf("instructioninfo=%8\n", ii.instructioninfo);
    //sendstringf("sourceaddress=%6\n", sourceaddress);
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

  sendstringf("vmwrite(%x, %x)\n", vmcsfield, vmcsvalue);

  if (vmptrld(currentcpuinfo->vmxdata.guest_activeVMCS))
  {
    sendstringf("Failure to load the guest vmcs\n");
    VMfailInvalid();
    return 0;
  }

  vmcswriteerror=vmwrite2(vmcsfield, vmcsvalue);

  if (vmcswriteerror==2)
    vmcswriteerrorcode=vmread(vm_errorcode);

  if (vmcswriteerror==0)
  {
    QWORD temp=0;
    temp=vmread(vmcsfield);
    if (temp!=vmcsvalue)
    {
      //error
      sendstring("vmwrite failed without error\n");
      while (1);

    }
  }

  vmptrld(currentcpuinfo->vmcs_regionPA);

  incrementRIP(vmread(vm_exit_instructionlength));

  if (vmcswriteerror)
  {
    sendstringf("vmwrite error\n");
    if (vmcswriteerror==1)
      VMfailInvalid();
    else
      VMfailValid(currentcpuinfo, vmcswriteerrorcode);
  }
  else
    vmsucceed();

  return 0;
}

int handle_vmclear(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{
  //
  QWORD address=getDestinationAddressFromInstructionInfo(vmregisters);

  //sendstringf("handle_vmclear(%6)\n", address);

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

 // sendstringf("physical address=%6\n", physicaladdress);

  int r;

  r=vmclear(physicaladdress);



  if (r==0) //success
  {
  //  sendstringf("vmclear success\n");
    vmsucceed();


    if (currentcpuinfo->vmxdata.guest_activeVMCS==physicaladdress)
    {
     // sendstringf("This was the active VMCS. Setting it to 0xffffffffffffffff\n");
      if ((hasVMCSShadowingSupport) && (currentcpuinfo->vmxdata.guest_activeVMCS!=0xffffffffffffffff))
      {
        //sendstringf("Marking guest activeVMCS as normal");
        DWORD *vmcsheader=getVMCS(currentcpuinfo, currentcpuinfo->vmxdata.guest_activeVMCS);
        *vmcsheader &= ~(1<<31);

        vmwrite(vm_vmcs_link_pointer, 0xffffffffffffffffull);
      }

      currentcpuinfo->vmxdata.guest_activeVMCS=0xffffffffffffffffull;
    }
    else
    {
      //sendstringf("VMClear and this was not the active VMCS\n");
    }
  }
  else
  {
    sendstringf("vmclear fail\n");
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

  incrementRIP(vmread(vm_exit_instructionlength));

  return 0;
}


int handle_vmptrld(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{

  QWORD address=getDestinationAddressFromInstructionInfo(vmregisters);
  int error;
  QWORD pagefaultaddress;


 // sendstringf("%6: handle_vmptrld(%6)\n", vmread(vm_guest_rip), address);



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


 // sendstringf("address=%6\n", address);



  incrementRIP(vmread(vm_exit_instructionlength));

  int r;


  if (address==currentcpuinfo->vmxdata.guest_vmxonaddress)
  {
    sendstringf("address==currentcpuinfo->vmxdata.guest_vmxonaddress\n");
    VMfail(currentcpuinfo, 10); //VMPTRLD with VMXON pointer
    return 0;
  }


 // sendstringf("calling vmptrld with address %6", address);
  r=vmptrld(address);

  if (r==0)
  {
    //sendstringf("new guest_activeVMCS=%6", address);
    currentcpuinfo->vmxdata.guest_activeVMCS=address;
  }
  else
  {
    sendstringf("vmptrld error (r=%d)\n", r);
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

  vmptrld(currentcpuinfo->vmcs_regionPA);


  if (hasVMCSShadowingSupport)
  {
    //find this entries' shadow copy

    //create a copy
   // sendstringf("Marking guest activeVMCS as shadow");
    vmclear(currentcpuinfo->vmxdata.guest_activeVMCS);

    DWORD *vmcsheader=getVMCS(currentcpuinfo, currentcpuinfo->vmxdata.guest_activeVMCS);
    *vmcsheader |= (1<<31);


    vmwrite(vm_vmcs_link_pointer, currentcpuinfo->vmxdata.guest_activeVMCS);
  }



 // sendstringf("Calling vmsucceed\n");
  vmsucceed();


  return 0;
}

int handle_vmptrst(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{
  QWORD address=getDestinationAddressFromInstructionInfo(vmregisters);
  int error;
  QWORD pagefaultaddress;

  sendstring("handle_vmptrst");

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

  //sendstringf("handle_vmxon %6\n", address);

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

  //sendstringf("vmxonaddress=%6\n", *mapped);

  if (hasVMCSShadowingSupport)
    vmwrite(vm_vmcs_link_pointer, 0xffffffffffffffffull);

  unmapVMmemory(mapped, 8);

  incrementRIP(vmread(vm_exit_instructionlength));

  vmsucceed();
  return 0;
}

int handle_vmxoff(pcpuinfo currentcpuinfo, VMRegisters *vmregisters UNUSED)
{
  //sendstringf("handle_vmxoff\n");
  if (currentcpuinfo->vmxdata.insideVMXRootMode==0)
    return raiseInvalidOpcodeException(currentcpuinfo);


  currentcpuinfo->vmxdata.guest_activeVMCS=0xffffffffffffffffULL;
  if (hasVMCSShadowingSupport)
    vmwrite(vm_vmcs_link_pointer, 0xffffffffffffffffull);

  currentcpuinfo->vmxdata.insideVMXRootMode=0;
  incrementRIP(vmread(vm_exit_instructionlength));

  vmsucceed();
  return 0;
}





int handleIntelVMXInstruction(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{
  int r;
  nosendchar[getAPICID()]=0;
  //sendstring(" - nested vm is not 100% working\n");


  if (emulatevmx==0)
    return raiseInvalidOpcodeException(currentcpuinfo);
  else
  {
    //try to handle it

    int exit_reason=currentcpuinfo->guest_error?currentcpuinfo->guest_error:vmread(vm_exit_reason) & 0x7fffffff;
    currentcpuinfo->guest_error=0;
    currentcpuinfo->vmxdata.currenterrorcode=0;

    if (currentcpuinfo->guest_error)
    {
      if (currentcpuinfo->guest_error==0xce00)  sendstringf("%6: handleIntelVMXInstruction %d (VMLAUNCH ERROR)\n", vmread(vm_guest_rip), exit_reason);
      if (currentcpuinfo->guest_error==0xce01)  sendstringf("%6: handleIntelVMXInstruction %d (VMRESUME ERROR)\n", vmread(vm_guest_rip), exit_reason);
    }
    //else
    //  sendstringf("%6: handleIntelVMXInstruction %d (%s)\n", vmread(vm_guest_rip), exit_reason, getVMExitReassonString());


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
        case vm_exit_vmclear: r=handle_vmclear(currentcpuinfo, vmregisters); break;
        case vm_exit_vmlaunch: r=handle_vmlaunch(currentcpuinfo, vmregisters); break;
        case 0xce00: r=handle_vmlaunchFail(currentcpuinfo); break;
        case 21: r=handle_vmptrld(currentcpuinfo, vmregisters); break;
        case 22: r=handle_vmptrst(currentcpuinfo, vmregisters); break;
        case 23: r=handle_vmread(currentcpuinfo, vmregisters); break;
        case 24: r=handle_vmresume(currentcpuinfo, vmregisters); break;
        case 0xce01: r=handle_vmresumeFail(currentcpuinfo); break;
        case 25: r=handle_vmwrite(currentcpuinfo, vmregisters); break;
        case vm_exit_vmxoff: r=handle_vmxoff(currentcpuinfo, vmregisters); break;
        case vm_exit_vmxon: r=handle_vmxon(currentcpuinfo, vmregisters); break;
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

//AMD

extern QWORD doVMRUN(QWORD vmcb_pa, VMRegisters *vmregisters, UINT64 dbvm_hostPA, UINT64 hostPA);

#ifdef DEBUG
int debugcounter;
#endif
int handleAMDVMRUNInstruction(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, FXSAVE64 *fxsave)
{
  //call vmrun with the given physical address, and on return emulate a #VMEXIT on the original vmcb

  //todo: modify so it returns extra events, etc...


  nosendchar[getAPICID()]=1;
  QWORD PA=currentcpuinfo->vmcb->RAX;

  pvmcb guestvmcb=(pvmcb)mapPhysicalMemory(PA,4096);


  if (((PRFLAGS)(&currentcpuinfo->vmcb->RFLAGS))->IF)
  {
    //sendstringf("IF==1\n");
    setCR8(currentcpuinfo->vmcb->V_TPR);
    asm("sti"); //GIF is still 0 so this should be safe

  }
  else
  {
    nosendchar[getAPICID()]=0;
    sendstringf("VMRUN with IF==0\n");
    asm("cli");
  }

  UINT64 newPAT;
#ifdef DEBUG
  int skip=0;

  WORD oldcs=guestvmcb->cs_selector;
  QWORD oldrip=guestvmcb->RIP;

  UINT64 myrflags;

  UINT64 myPAT=readMSR(IA32_PAT_MSR);
  UINT64 vmcbPAT=currentcpuinfo->vmcb->G_PAT;



  sendstringf("VMRUN: TPR=%d  -  Guest's Guest V_INTR_MASKING=%d IF=%d TPR=%d AVIC_ENABLED=%d\n",currentcpuinfo->vmcb->V_TPR,  guestvmcb->V_INTR_MASKING, ((PRFLAGS)(&guestvmcb->RFLAGS))->IF, guestvmcb->V_TPR, guestvmcb->AVIC_ENABLED);
  myrflags=getRFLAGS();
#endif

  if (currentcpuinfo->vmcb_host_pa==0)
  {
    void *x=malloc2(4096);
    zeromemory(x,4096);
    currentcpuinfo->vmcb_host_pa=VirtualToPhysical(x);
  }


  setCR8(currentcpuinfo->vmcb->V_TPR);
  setDR6(0xfff0ff0);
  setDR7(0x401);

  nosendchar[getAPICID()]=0;


  UINT64 a=0, b=0, c=0,d=0;
  a=0x8000000a;
  _cpuid(&a,&b,&c,&d); //ebx gets the max ASID value


  if (currentcpuinfo->vmcb->GuestASID==guestvmcb->GuestASID)
  {
    //change the host ASID (better than changing the guest each time)
    nosendchar[getAPICID()]=0;
    sendstringf("Host and guest have same ASID (Host=%d Guest=%d)\n", currentcpuinfo->vmcb->GuestASID, guestvmcb->GuestASID);

    sendstringf("cpuid(0x8000000a).EBX=%d\n", b);

    if ((b) && (b-1))
    {
      currentcpuinfo->vmcb->GuestASID=(_rdtsc() % (b-1));  //1
      currentcpuinfo->vmcb->GuestASID=1+(currentcpuinfo->vmcb->GuestASID % b+1);

      sendstringf("new currentcpuinfo->vmcb->GuestASID=%d\n", currentcpuinfo->vmcb->GuestASID);
    }
    currentcpuinfo->vmcb->VMCB_CLEAN_BITS&=~CLEAN_ASID;
  }

  if (guestvmcb->V_INTR_MASKING==0)
  {
    nosendchar[getAPICID()]=0;
    sendstringf("Guest VMRUN with no INT_MASKING\n");
  }

  doVMRUN(PA, vmregisters, currentcpuinfo->vmcb_host_pa,  currentcpuinfo->vmcb_PA);


  newPAT=readMSR(IA32_PAT_MSR);
#ifdef DEBUG
  skip=0;
  switch (guestvmcb->EXITCODE)
  {
    case VMEXIT_VINTR:
    case VMEXIT_CR4_WRITE:
    case VMEXIT_HLT:
      skip=20;
      break;

    case VMEXIT_INTR:
    case VMEXIT_CPUID:
      skip=40;
      break;

    case VMEXIT_IOIO:
      skip=500;
      break;

    case VMEXIT_NPF:
      skip=200;
      break;

  }

  if (skip)
  {
    if (debugcounter%skip==0)
      skip=0;
  }

  debugcounter++;



  if (skip==0)
  {
    sendstringf("%d: (%6 (%d <-> %d)) %x:%x -> %x:%x Guest EXITCODE=%x  IF=%d TPR=%d", currentcpuinfo->cpunr, PA, guestvmcb->GuestASID, currentcpuinfo->vmcb->GuestASID, oldcs, oldrip, guestvmcb->cs_selector, guestvmcb->RIP, guestvmcb->EXITCODE, ((PRFLAGS)(&guestvmcb->RFLAGS))->IF, guestvmcb->V_TPR);
    if (guestvmcb->EXITCODE==0x400)
    {
      sendstringf(" CR0=%x", guestvmcb->CR0);
      sendstringf(" CR4=%x", guestvmcb->CR4);
      sendstringf(" PA=%6", guestvmcb->EXITINFO2);
      sendstringf(" Error=%x", guestvmcb->EXITINFO1);
    }


    if ((guestvmcb->EXITCODE==VMEXIT_INTR) || (guestvmcb->EXITCODE==VMEXIT_VINTR))
    {
      sendstring(" - ");
      ShowPendingInterrupts();
    }


    if (guestvmcb->EXITCODE==VMEXIT_MSR)
    {
      if (currentcpuinfo->vmcb->EXITINFO1)
        sendstringf(" write to ");
      else
        sendstringf(" read from ");

      sendstringf(" MSR %6",vmregisters->rcx & 0xffffffff);
    }

    if (guestvmcb->EXITCODE==0x7b)
    {
      sendstringf(" IO Port %x", guestvmcb->EXITINFO1 >> 16);
      if (guestvmcb->EXITINFO1 & 1)
        sendstringf(" read");
      else
        sendstringf(" write");
    }
    if (guestvmcb->EXITCODE==0x41)
    {
      RFLAGS *f=(RFLAGS *)&guestvmcb->RFLAGS;
     // regDR6 *dr6=&guestvmcb->DR6;
      sendstringf(" RFLAGS=%6 (RF=%d TF=%d)", guestvmcb->RFLAGS, f->RF, f->TF);
      sendstringf(" DR6=%6", guestvmcb->DR6);
      sendstringf(" DR7=%6", guestvmcb->DR7);

      sendstringf(" myDR6=%6", getDR6());
      sendstringf(" myDR7=%6", getDR7());

    }

    if (guestvmcb->EXITINTINFO)
      sendstringf(" EXITINTINFO=%x", guestvmcb->EXITINTINFO);

    sendstringf("\n");

    skip=0;
  }

#endif


  asm("cli");



  unmapPhysicalMemory((void*)guestvmcb,4096);

  if (AMD_hasNRIPS)
    currentcpuinfo->vmcb->RIP=currentcpuinfo->vmcb->nRIP;
  else
    currentcpuinfo->vmcb->RIP+=3;

  currentcpuinfo->vmcb->DR6=getDR6();
  currentcpuinfo->vmcb->DR7=0x400;
  currentcpuinfo->vmcb->CR2=getCR2();
  currentcpuinfo->vmcb->G_PAT=newPAT;


  writeMSR(IA32_PAT_MSR,readMSR(IA32_PAT_MSR));

  currentcpuinfo->vmcb->VMCB_CLEAN_BITS&=~(CLEAN_DRx | CLEAN_CR2);

  setCR3(getCR3());

  return 0;
}
