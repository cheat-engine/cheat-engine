#include "common.h"
#include "vmmhelper.h"
#include "main.h"
#include "neward.h"
#include "mm.h"

/*
#include "vmmemu.h"
*/
#include "vmcall.h"
#include "vmpaging.h"
#include "vmeventhandler.h"
#include "vmreadwrite.h"
#include "msrnames.h"
#include "vmxcontrolstructures.h"

#include "distorm.h"
#include "realmodeemu.h"
#include "multicore.h"
#include "offloados.h"

#include "vmeventhandler_amd.h"
#include "vmxsetup.h"
#include "epthandler.h"
#include "exports.h"
#include "luahandler.h"

#include "displaydebug.h"
#include "interrupthandler.h"


#ifndef DEBUG
#define sendstringf(s,x...)
#define sendstring(s)
#endif

//cpu specific stuff, put inside structure and give each cpu one


DBVM_PLUGIN_EXIT_PRE *dbvm_plugin_exit_pre;
DBVM_PLUGIN_EXIT_POST *dbvm_plugin_exit_post;


int ISPAGING(pcpuinfo currentcpuinfo)
{
  if (isAMD)
  {
    regCR0 cr0;
    cr0.CR0=currentcpuinfo->vmcb->CR0;
    return cr0.PG==1;
  }
  else
  {
    if (hasUnrestrictedSupport)
      return (vmread(vm_guest_cr0) & CR0_PG)==0;
    else
      return (vmread(vm_cr0_read_shadow) & CR0_PG)==1;
  }
}

int ISREALMODE(pcpuinfo currentcpuinfo)
{
  if (isAMD)
  {
    regCR0 cr0;
    cr0.CR0=currentcpuinfo->vmcb->CR0;
    return cr0.PE==0;
  }
  else
  {
    if (hasUnrestrictedSupport)
      return (vmread(vm_guest_cr0) & 1)==0;
    else
      return (vmread(vm_cr0_read_shadow) & 1)==0;
  }
}


int IS64BITPAGING(pcpuinfo currentcpuinfo)
{
  if (isAMD)
  {
    return (currentcpuinfo->vmcb->EFER & (1<<10))==(1<<10);
  }
  else
    return ((vmread(vm_entry_controls) & VMENTRYC_IA32E_MODE_GUEST) != 0);
}

int IS64BITCODE(pcpuinfo currentcpuinfo)
{
  if (isAMD)
  {
    Segment_Attribs cs;
    cs.SegmentAttrib=currentcpuinfo->vmcb->cs_attrib;
    return cs.L;
  }
  else
    return IS64BITPAGING(currentcpuinfo) && ((vmread(vm_guest_cs_access_rights) >> 13) & 1);
}


int isDebugFault(QWORD dr6, QWORD dr7)
//returns 1 if this results in a Fault
{
  regDR6 d6;
  regDR7 d7;

  d6.DR6=dr6;
  d7.DR7=dr7;

  if ((d6.BD) && (d7.GD))
    return 1; //general detect is a fault
  else
    return 0; //everything else is a trap

}

char * getVMExitReassonString(void)
{
  int i=vmread(vm_exit_reason) & 0x7fffffff;
  switch (i)
  {
	  case 0: return "Exception or NMI";
	  case 1: return "External interrupt";
	  case 2: return "Triple fault";
	  case 3: return "INIT Signal";
	  case 4: return "Start-up IPI (SIPI)";
	  case 5: return "SMI interrupt";
	  case 6: return "Other SMI";
	  case 7: return "Interrupt window";
	  case 8: return "NMI window";
	  case 9: return "Task switch";
	  case 10: return "CPUID";
	  case 14: return "INVLPG";
	  case 16: return "RDTSC";
	  case 17: return "VMREAD";
	  case 18: return "VMCALL";
	  case 19: return "VMCLEAR";
	  case 20: return "VMLAUNCH";
	  case 21: return "VMPTRLD";
	  case 23: return "VMREAD";
    case 24: return "VMRESUME";
	  case 25: return "VMWRITE";
	  case 27: return "VMXON"; //or: omg it's one bee
	  case 28: return "Controlregister access";
	  case vm_exit_io_access: return "IO Access";

	  case 31: return "RDMSR";
	  case 32: return "WRMSR";
	  case 33: return "Invalid guest state";
	  case 37: return "Monitor trap flag";
	  case vm_exit_ept_violation: return "EPT Violation";
	  case vm_exit_ept_misconfiguration: return "EPT Misconfiguration";
	  case 50: return "INVEPT";
	  case 51: return "RDTSCP";
	  case 52: return "Preemption timer";
	  case 53: return "INVVPID";
	  case 55: return "XSETBV";
	  default :return "NYI";
  }
}

char * getVMInstructionErrorString(void)
/*
 * get the last vm error and return a pointer to the string describing that error
 */
{
  int i=vmread(0x4400) & 0x1f;
  char *result;
  switch (i)
  {
    case 0: result="No error"; break;
    case 1: result="VMCALL executed in VMX root operation"; break;
    case 2: result="VMCLEAR with invalid physical address"; break;
    case 3: result="VMCLEAR with VMXON pointer"; break;
    case 4: result="VMLAUNCH with non-clear VMCS"; break;
    case 5: result="VMRESUME with non-launched VMCS"; break;
    case 6: result="VMRESUME with a corrupted VMCS"; break;
    case 7: result="VM entry with invalid fields"; break;
    case 8: result="VM entry with invalid host-state fields"; break;
    case 9: result="VMPTRLD with invalid physical address"; break;
    case 10: result="VMPTRLD with VMXON pointer"; break;
    case 11: result="VMPTRLD with incorrect VMCS revision identifier"; break;
    case 12: result="VMREAD/VMWRITE from/to unsupported VMCS component"; break;
    case 13: result="VMWRITE to read-only VMCS component"; break;
    case 15: result="VMXON executed in VMX root operation"; break;
    case 16: result="VM entry with invalid executive-VMCS pointer"; break;
    case 17: result="VM entry with non-launched executive-VMCS pointer"; break;
    case 18: result="VM entry with executive-VMCS pointer but not VMXON pointer"; break;
    case 19: result="VMCALL with non-clear VMCS"; break;
    case 20: result="VMCALL with invalid VM-exit control fields"; break;
    case 22: result="VMCALL with incorrect MSEG revision number"; break;
    case 23: result="VMXOFF under dual-monitor treatment of SMIs and SMM"; break;
    case 24: result="VMCALL with invalid SMM-monitor features"; break;
    case 25: result="VM entry with invalid VM-execution control fields in executive VMCS"; break;
    case 26: result="VM entry with events blocked by MOV SS"; break;

    default: result="Undefined"; break;
  }


  return result;
}



void setTrap(void)
{
  UINT64 guestrflags=vmread(vm_guest_rflags);
  PRFLAGS pguestrflags=(PRFLAGS)&guestrflags;
  pguestrflags->TF=1;
  vmwrite(vm_guest_rflags,(UINT64)guestrflags);
}

void setResumeFlag(void)
{
  UINT64 guestrflags=vmread(vm_guest_rflags);
  PRFLAGS pguestrflags=(PRFLAGS)&guestrflags;
  pguestrflags->RF=1;
  vmwrite(vm_guest_rflags,(UINT64)guestrflags);
}



void setupTSS8086(void)
// sets up the TSS used for realmode emulation
{
  unsigned char *c;
  sendstring("Seting up TSS (for VM8086)\n\r");

    // setup the TSS for virtual 8086 mode
  VirtualMachineTSS_V8086->Previous_Task_Link=0;
  VirtualMachineTSS_V8086->Reserved1=0;
  VirtualMachineTSS_V8086->ESP0=(ULONG)VirtualToPhysical((void *)((UINT64)RealmodeRing0Stack+4096-16));
  VirtualMachineTSS_V8086->SS0=8; //32-bit code segment
  VirtualMachineTSS_V8086->Reserved2=0;
  VirtualMachineTSS_V8086->ESP1=0;
  VirtualMachineTSS_V8086->SS1=0;
  VirtualMachineTSS_V8086->Reserved3=0;
  VirtualMachineTSS_V8086->ESP2=0;
  VirtualMachineTSS_V8086->SS2=0;
  VirtualMachineTSS_V8086->Reserved4=0;
  VirtualMachineTSS_V8086->CR3=(ULONG)VirtualToPhysical(nonpagedEmulationPagedir);
  VirtualMachineTSS_V8086->EIP=0;
  VirtualMachineTSS_V8086->EFLAGS=0x33000; //x86 vm
  VirtualMachineTSS_V8086->EAX=0;
  VirtualMachineTSS_V8086->ECX=0;
  VirtualMachineTSS_V8086->EDX=0;
  VirtualMachineTSS_V8086->EBX=0;
  VirtualMachineTSS_V8086->ESP=0xfff0;
  VirtualMachineTSS_V8086->EBP=0;
  VirtualMachineTSS_V8086->ESI=0;
  VirtualMachineTSS_V8086->EDI=0;
  VirtualMachineTSS_V8086->ES=0;
  VirtualMachineTSS_V8086->Reserved5=0;
  VirtualMachineTSS_V8086->CS=0;
  VirtualMachineTSS_V8086->Reserved6=0;
  VirtualMachineTSS_V8086->SS=0;
  VirtualMachineTSS_V8086->Reserved7=0;
  VirtualMachineTSS_V8086->DS=0;
  VirtualMachineTSS_V8086->Reserved8=0;
  VirtualMachineTSS_V8086->FS=0;
  VirtualMachineTSS_V8086->Reserved9=0;
  VirtualMachineTSS_V8086->GS=0;
  VirtualMachineTSS_V8086->Reserved10=0;
  VirtualMachineTSS_V8086->LDTss=0;
  VirtualMachineTSS_V8086->Reserved11=0;
  VirtualMachineTSS_V8086->Trap=0;
  VirtualMachineTSS_V8086->Reserved12=0;
  VirtualMachineTSS_V8086->IOBASE=sizeof(TSS)+32;

  zeromemory((void *)((UINT64)VirtualMachineTSS_V8086+ sizeof(TSS)),32);

  *(ULONG *)(VirtualMachineTSS_V8086+ sizeof(TSS))=0x200000; //int 0x15 break

  zeromemory((void *)((UINT64)VirtualMachineTSS_V8086+ sizeof(TSS)+31),8193);

  c=(unsigned char *)(VirtualMachineTSS_V8086);

  c[sizeof(TSS)+32+8192]=0xff;

  //c[sizeof(TSS)+0]=0xff; //ff; //break on 0 to 7
  //c[sizeof(TSS)+1]=0xff;
  //c[sizeof(TSS)+2]=0xff;
  //c[sizeof(TSS)+3]=0xff;
  //c[sizeof(TSS)+4]=0xff;

  c[sizeof(TSS)+2]=0x20; //0x20=break on int15*/  // 0x28; //break on int15 and int13
}
/*


void exportwholevmstate(void)
{
  //this routine will show every vmx register
  int orig=nosendchar[getAPICID()];
  unsigned int i;
  nosendchar[getAPICID()]=0;

  sendstring("\n\r\n\r");
  sendstring("---------------------------------------------\n\r");
  for (i=0x800; i<=0x80e; i+=2)
    sendstringf("%x : %8\n\r", i, vmread(i));

  for (i=0xc00; i<=0xc0c; i+=2)
    sendstringf("%x : %8\n\r", i, vmread(i));

  for (i=0x2000; i<=0x200d; i++)
    sendstringf("%x : %8\n\r", i, vmread(i));

  for (i=0x2010; i<=0x2013; i++)
    sendstringf("%x : %8\n\r", i, vmread(i));

  for (i=0x2800; i<=0x2803; i++)
    sendstringf("%x : %8\n\r", i, vmread(i));

  for (i=0x4000; i<=0x401c; i+=2)
    sendstringf("%x : %8\n\r", i, vmread(i));

  for (i=0x4400; i<=0x440e; i+=2)
    sendstringf("%x : %8\n\r", i, vmread(i));

  for (i=0x4800; i<=0x482a; i+=2)
    sendstringf("%x : %8\n\r", i, vmread(i));

  sendstringf("%x : %8\n\r", 0x4c00, vmread(0x4c00));

  for (i=0x6000; i<=0x600e; i+=2)
    sendstringf("%x : %8\n\r", i, vmread(i));

  for (i=0x6400; i<=0x640a; i+=2)
    sendstringf("%x : %8\n\r", i, vmread(i));

  for (i=vm_guest_cr0; i<=0x6826; i+=2)
    sendstringf("%x : %8\n\r", i, vmread(i));

  for (i=0x6c00; i<=0x6c16; i+=2)
    sendstringf("%x : %8\n\r", i, vmread(i));

  sendstring("---------------------------------------------\n\r");

  nosendchar[getAPICID()]=orig;
}

*/

void StoreVirtualMachineState(pcpuinfo currentcpuinfo UNUSED, VMRegisters *registers UNUSED)
{
#ifdef DEBUG
  vmstates[vmstates_pos].registers=*registers;
  vmstates[vmstates_pos].rsp=vmread(vm_guest_rsp);
  vmstates[vmstates_pos].rip=vmread(vm_guest_rip);
  vmstates[vmstates_pos].rflags=vmread(vm_guest_rflags);


  vmstates[vmstates_pos].efer=currentcpuinfo->efer;

  vmstates[vmstates_pos].es=vmread(vm_guest_es);
  vmstates[vmstates_pos].cs=vmread(vm_guest_cs);
  vmstates[vmstates_pos].ss=vmread(vm_guest_ss);
  vmstates[vmstates_pos].ds=vmread(vm_guest_ds);
  vmstates[vmstates_pos].fs=vmread(vm_guest_fs);
  vmstates[vmstates_pos].gs=vmread(vm_guest_gs);
  vmstates[vmstates_pos].ldtr=vmread(vm_guest_ldtr);
  vmstates[vmstates_pos].tr=vmread(vm_guest_tr);

  vmstates[vmstates_pos].es_base=vmread(vm_guest_es_base);
  vmstates[vmstates_pos].cs_base=vmread(vm_guest_cs_base);
  vmstates[vmstates_pos].ss_base=vmread(vm_guest_ss_base);
  vmstates[vmstates_pos].ds_base=vmread(vm_guest_ds_base);
  vmstates[vmstates_pos].fs_base=vmread(vm_guest_fs_base);
  vmstates[vmstates_pos].gs_base=vmread(vm_guest_gs_base);
  vmstates[vmstates_pos].ldtr_base=vmread(vm_guest_ldtr_base);
  vmstates[vmstates_pos].tr_base=vmread(vm_guest_tr_base);


  vmstates[vmstates_pos].exit_reason=vmread(vm_exit_reason);
  vmstates[vmstates_pos].exit_interruptioninfo=vmread(vm_exit_interruptioninfo);
  vmstates[vmstates_pos].exit_interruptionerror=vmread(vm_exit_interruptionerror);
  vmstates[vmstates_pos].idtvector_information=vmread(vm_idtvector_information);
  vmstates[vmstates_pos].idtvector_error=vmread(vm_idtvector_error);

 // vmstates[vmstates_pos].cr3=vmread(vm_guest_cr3);

  vmstates_pos++;
  vmstates_pos=vmstates_pos % 4;
#endif
}

void displayPreviousStates(void)
{
#ifdef DEBUG
  //find previous data
  int previous_state=(vmstates_pos+2) % 4; //I need the one 2 steps back
  int current_state=(vmstates_pos+3) % 4;
  int itterator;

  sendstringf("vmstates_pos=%d\n",vmstates_pos);
  sendstringf("previous_state=%d\n",previous_state);

  for (itterator=0; itterator<4; itterator++)
  {
    sendstringf("%s%d: CS:RIP=%x:%6 (base=%6)\n", ((itterator==previous_state)?"*":""), itterator, vmstates[itterator].cs, vmstates[itterator].rip, vmstates[itterator].cs_base);
    sendstringf("%s%d: SS:RSP=%x:%6 (base=%6)\n", ((itterator==previous_state)?"*":""), itterator, vmstates[itterator].ss, vmstates[itterator].rsp, vmstates[itterator].ss_base);
    sendstringf("%s%d: RFLAGS=%x\n", ((itterator==previous_state)?"*":""), itterator, vmstates[vmstates_pos].rflags);
    sendstringf("%s%d: RAX=%x\n", ((itterator==previous_state)?"*":""), itterator, vmstates[itterator].registers.rax);
    sendstringf("%s%d: Exit reason=%8 (%d) \n\r", ((itterator==previous_state)?"*":""),itterator, vmstates[itterator].exit_reason,vmstates[itterator].exit_reason & 0x0fffffff);


    sendstringf("%s%d: VM-exit interruption information=%x\n\r", ((itterator==previous_state)?"*":""), itterator, vmstates[itterator].exit_interruptioninfo);

    sendstringf("%s%d: VM-exit interruption error code=%x\n\r", ((itterator==previous_state)?"*":""), itterator, vmstates[itterator].exit_interruptionerror);
    sendstringf("%s%d: IDT-vectoring information field=%x\n\r", ((itterator==previous_state)?"*":""), itterator,vmstates[itterator].idtvector_information);
    sendstringf("%s%d: IDT-vectoring error code=%x\n\r", ((itterator==previous_state)?"*":""), itterator,vmstates[itterator].idtvector_error);


    if (itterator!=current_state)
    {
      sendstringf("%s%d:---exit---\n", ((itterator==previous_state)?"*":""), itterator);
      sendstringf("%s%d: CS:RIP=%x:%6 (base=%6)\n", ((itterator==previous_state)?"*":""), itterator, vmstates[itterator].exit_cs, vmstates[itterator].exit_rip, vmstates[itterator].exit_cs_base);
      sendstringf("%s%d: SS:RSP=%x:%6 (base=%6)\n", ((itterator==previous_state)?"*":""), itterator, vmstates[itterator].exit_ss, vmstates[itterator].exit_rsp, vmstates[itterator].exit_ss_base);
      sendstringf("%s%d: RFLAGS=%x\n", ((itterator==previous_state)?"*":""), itterator, vmstates[vmstates_pos].exit_rflags);

    }
  }
#endif
}


void sendvmstate(pcpuinfo currentcpuinfo UNUSED, VMRegisters *registers UNUSED)
{

#ifdef DEBUG
  if (isAMD)
  {
    UINT64 rflags=currentcpuinfo->vmcb->RFLAGS;

    PRFLAGS prflags=(PRFLAGS)&rflags;

    sendstringf("CPL=%d\n", currentcpuinfo->vmcb->CPL);

    if (registers)    // print registers
    {
      sendstringf("RAX=%6 RBX=%6   R8=%6\n\r", currentcpuinfo->vmcb->RAX, registers->rbx, registers->r8);
      sendstringf("RCX=%6 RDX=%6   R9=%6\n\r", registers->rcx, registers->rdx, registers->r9);
      sendstringf("RSI=%6 RDI=%6  R10=%6\n\r",registers->rsi, registers->rdi,  registers->r10);
      sendstringf("RBP=%6                       R11=%6\n\r",registers->rbp, registers->r11);


    }
    else
      sendstring("\n...no registers...\n\n");


    sendstringf("RSP=%6                       R12=%6\n\r",currentcpuinfo->vmcb->RSP, registers?registers->r12:0);
    sendstringf("RIP=%6                       R13=%6\n\r",currentcpuinfo->vmcb->RIP, registers?registers->r13:0);
    sendstringf("                                           R14=%6\n\r", registers?registers->r14:0);
    sendstringf("                                           R15=%6\n\r", registers?registers->r15:0);

    sendstringf("rflags=%6 (VM=%d RF=%d IOPL=%d NT=%d)\n\r",rflags,prflags->VM, prflags->RF, prflags->IOPL, prflags->NT);
    sendstringf("(CF=%d PF=%d AF=%d ZF=%d SF=%d TF=%d IF=%d DF=%d OF=%d)\n\r\n\r", prflags->CF, prflags->PF, prflags->AF, prflags->ZF, prflags->SF, prflags->TF, prflags->IF, prflags->DF, prflags->OF);


    sendstringf("cs=%8  (base=%6 , limit=%8, AT=%8)\n\r",currentcpuinfo->vmcb->cs_selector, currentcpuinfo->vmcb->cs_base, currentcpuinfo->vmcb->cs_limit, currentcpuinfo->vmcb->cs_attrib);
    sendstringf("ss=%8  (base=%6 , limit=%8, AT=%8)\n\r",currentcpuinfo->vmcb->ss_selector, currentcpuinfo->vmcb->ss_base, currentcpuinfo->vmcb->ss_limit, currentcpuinfo->vmcb->ss_attrib);
    sendstringf("ds=%8  (base=%6 , limit=%8, AT=%8)\n\r",currentcpuinfo->vmcb->ds_selector, currentcpuinfo->vmcb->ds_base, currentcpuinfo->vmcb->ds_limit, currentcpuinfo->vmcb->ds_attrib);
    sendstringf("es=%8  (base=%6 , limit=%8, AT=%8)\n\r",currentcpuinfo->vmcb->es_selector, currentcpuinfo->vmcb->es_base, currentcpuinfo->vmcb->es_limit, currentcpuinfo->vmcb->es_attrib);
    sendstringf("fs=%8  (base=%6 , limit=%8, AT=%8)\n\r",currentcpuinfo->vmcb->fs_selector, currentcpuinfo->vmcb->fs_base, currentcpuinfo->vmcb->fs_limit, currentcpuinfo->vmcb->fs_attrib);
    sendstringf("gs=%8  (base=%6 , limit=%8, AT=%8)\n\r",currentcpuinfo->vmcb->gs_selector, currentcpuinfo->vmcb->gs_base, currentcpuinfo->vmcb->gs_limit, currentcpuinfo->vmcb->gs_attrib);
    sendstringf("ldt=%8 (base=%6 , limit=%8, AT=%8)\n\r",currentcpuinfo->vmcb->ldtr_selector, currentcpuinfo->vmcb->ldtr_base, currentcpuinfo->vmcb->ldtr_limit, currentcpuinfo->vmcb->ldtr_attrib);
    sendstringf("tr=%8  (base=%6 , limit=%8, AT=%8)\n\r",currentcpuinfo->vmcb->tr_selector, currentcpuinfo->vmcb->tr_base, currentcpuinfo->vmcb->tr_limit, currentcpuinfo->vmcb->tr_attrib);
    sendstringf("\n\r");
    sendstringf("gdt: base=%6 limit=%x\n\r",currentcpuinfo->vmcb->gdtr_base, currentcpuinfo->vmcb->gdtr_limit);
    sendstringf("idt: base=%6 limit=%x\n\r",currentcpuinfo->vmcb->idtr_base, currentcpuinfo->vmcb->idtr_limit);

    sendstringf("cr0=%6 cr3=%6 cr4=%6\n\r",currentcpuinfo->vmcb->CR0, currentcpuinfo->vmcb->CR3, currentcpuinfo->vmcb->CR4);


  }
  else
  {
    UINT64 rflags=vmread(vm_guest_rflags);
    PRFLAGS prflags=(PRFLAGS)&rflags;

    sendstringf("cpunr=%d\n\r", currentcpuinfo->cpunr);
    sendstringf("getTaskRegister()=%x\n",getTaskRegister());

    sendstringf("Activity state : %d      interruptibility state : %d \n\r",vmread(vm_guest_activity_state), vmread(vm_guest_interruptability_state));

    sendstringf("IS64BITPAGING=%d IS64BITCODE=%d ISREALMODE=%d\n\r", IS64BITPAGING(currentcpuinfo), IS64BITCODE(currentcpuinfo), ISREALMODE(currentcpuinfo));
    if (hasUnrestrictedSupport)
      sendstringf("efer=%x\n\r",vmread(vm_guest_IA32_EFER));
    else
      sendstringf("efer=%x (%x)\n\r",currentcpuinfo->efer, vmread(vm_guest_IA32_EFER));

    sendstringf("ia32e mode guest=%d\n",((vmread(vm_entry_controls) & VMENTRYC_IA32E_MODE_GUEST) != 0) );

    sendstringf("IA32_SYSENTER_CS=%x IA32_SYSENTER_EIP=%x IA32_SYSENTER_ESP=%x\n",vmread(vm_guest_IA32_SYSENTER_CS), vmread(vm_guest_IA32_SYSENTER_EIP), vmread(vm_guest_IA32_SYSENTER_ESP) );


    if (registers) 		// print registers
    {
      sendstringf("RAX=%6 RBX=%6   R8=%6\n\r", registers->rax, registers->rbx, registers->r8);
      sendstringf("RCX=%6 RDX=%6   R9=%6\n\r", registers->rcx, registers->rdx, registers->r9);
      sendstringf("RSI=%6 RDI=%6  R10=%6\n\r",registers->rsi, registers->rdi,  registers->r10);
      sendstringf("RBP=%6                       R11=%6\n\r",registers->rbp, registers->r11);


    }
    else
      sendstring("\n...no registers...\n\n");


    sendstringf("RSP=%6                       R12=%6\n\r",vmread(vm_guest_rsp), registers?registers->r12:0);
    sendstringf("RIP=%6                       R13=%6\n\r",vmread(vm_guest_rip), registers?registers->r13:0);
    sendstringf("                                           R14=%6\n\r", registers?registers->r14:0);
    sendstringf("                                           R15=%6\n\r", registers?registers->r15:0);

    sendstringf("rflags=%6 (VM=%d RF=%d IOPL=%d NT=%d)\n\r",rflags,prflags->VM, prflags->RF, prflags->IOPL, prflags->NT);
    sendstringf("(CF=%d PF=%d AF=%d ZF=%d SF=%d TF=%d IF=%d DF=%d OF=%d)\n\r\n\r", prflags->CF, prflags->PF, prflags->AF, prflags->ZF, prflags->SF, prflags->TF, prflags->IF, prflags->DF, prflags->OF);

    if (currentcpuinfo->invalidcs)
    {
      sendstring("Invalid cs...\n\r");
    }

    sendstringf("cs=%8  (base=%6 , limit=%8, AR=%8)\n\r",vmread(0x802),vmread(vm_guest_cs_base),vmread(vm_guest_cs_limit), vmread(0x4816));
    sendstringf("ss=%8  (base=%6 , limit=%8, AR=%8)\n\r",vmread(0x804),vmread(vm_guest_ss_base),vmread(vm_guest_ss_limit), vmread(0x4818));
    sendstringf("ds=%8  (base=%6 , limit=%8, AR=%8)\n\r",vmread(0x806),vmread(0x680c),vmread(0x4806), vmread(0x481a));
    sendstringf("es=%8  (base=%6 , limit=%8, AR=%8)\n\r",vmread(0x800),vmread(0x6806),vmread(0x4800), vmread(0x4814));
    sendstringf("fs=%8  (base=%6 , limit=%8, AR=%8)\n\r",vmread(0x808),vmread(0x680e),vmread(0x4808), vmread(0x481c));
    sendstringf("gs=%8  (base=%6 , limit=%8, AR=%8)\n\r",vmread(0x80a),vmread(0x6810),vmread(0x480a), vmread(0x481e));
    sendstringf("ldt=%8 (base=%6 , limit=%8, AR=%8)\n\r",vmread(0x80c),vmread(0x6812),vmread(0x480c), vmread(0x4820));
    sendstringf("tr=%8  (base=%6 , limit=%8, AR=%8)\n\r",vmread(0x80e),vmread(0x6814),vmread(0x480e), vmread(0x4822));

    sendstringf("\n\r");
    sendstringf("gdt: base=%6 limit=%x\n\r",vmread(vm_guest_gdtr_base), vmread(vm_guest_gdt_limit));
    sendstringf("idt: base=%6 limit=%x\n\r",vmread(vm_guest_idtr_base), vmread(vm_guest_idt_limit));

    if (ISREALMODE(currentcpuinfo))
    {
      sendstringf("RM gdt: base=%6 limit=%x\n\r",currentcpuinfo->RealMode.GDTBase, currentcpuinfo->RealMode.GDTLimit);
      sendstringf("RM idt: base=%6 limit=%x\n\r",currentcpuinfo->RealMode.IDTBase, currentcpuinfo->RealMode.IDTLimit);
    }

    regDR7 dr7;
    dr7.DR7=vmread(vm_guest_dr7);
    sendstringf("guest: dr0=%6 dr1=%6 dr2=%6 \n\r       dr3=%6 dr6=%6 dr7=%6\n\r",getDR0(), getDR1(), getDR2(), getDR3(), getDR6(), dr7.DR7);
    if (dr7.DR7 != 0x400)
    {
      sendstringf("dr7:");
      if (dr7.G0)
        sendstringf("G0 ");

      if (dr7.L0)
        sendstringf("L0 ");

      if (dr7.G1)
        sendstringf("G1 ");

      if (dr7.L1)
        sendstringf("L1 ");

      if (dr7.G2)
        sendstringf("G2 ");

      if (dr7.L2)
          sendstringf("L2 ");

      if (dr7.G3)
        sendstringf("G3 ");

      if (dr7.L3)
          sendstringf("L3 ");

      if (dr7.LE)
        sendstringf("LE ");

      if (dr7.GE)
        sendstringf("GE ");

      if (dr7.RW0)
        sendstringf("RW0 ");

      if (dr7.LEN0)
        sendstringf("LEN0 ");

      if (dr7.RW1)
        sendstringf("RW1 ");

      if (dr7.LEN1)
        sendstringf("LEN1 ");

      if (dr7.RW2)
        sendstringf("RW2 ");

      if (dr7.LEN2)
        sendstringf("LEN2 ");

      if (dr7.RW3)
        sendstringf("RW3 ");

      if (dr7.LEN3)
        sendstringf("LEN3 ");


    }


    sendstringf("host dr7=%6\n\r", getDR7());
    sendstringf("cr2=%6\n\r",getCR2());

    sendstringf("real:\n\r");
    sendstringf("cr0=%6 cr3=%6 cr4=%6\n\r",vmread(vm_guest_cr0), vmread(vm_guest_cr3), vmread(vm_guest_cr4));

    sendstringf("fake (what vm sees):\n\r");

    QWORD fakeCR0=(vmread(vm_guest_cr0) & (~vmread(vm_cr0_guest_host_mask))) | (vmread(vm_cr0_read_shadow) & vmread(vm_cr0_guest_host_mask));
    QWORD fakeCR4=(vmread(vm_guest_cr4) & (~vmread(vm_cr4_guest_host_mask))) | (vmread(vm_cr4_read_shadow) & vmread(vm_cr4_guest_host_mask));
    if (hasUnrestrictedSupport)
    {
      sendstringf("vm_cr0_guest_host_mask=%6 vm_cr0_read_shadow=%6\n", vmread(vm_cr0_guest_host_mask), vmread(vm_cr0_read_shadow));
      sendstringf("vm_cr4_guest_host_mask=%6 vm_cr4_read_shadow=%6\n", vmread(vm_cr4_guest_host_mask), vmread(vm_cr4_read_shadow));
      sendstringf("cr0=%6 cr3=%6 cr4=%6\n\r",fakeCR0, currentcpuinfo->guestCR3, fakeCR4);
    }
    else
      sendstringf("cr0=%6 cr3=%6 cr4=%6\n\r",vmread(vm_cr0_read_shadow), currentcpuinfo->guestCR3, vmread(vm_cr4_read_shadow));
  }

  if (currentcpuinfo->vmxdata.insideVMXRootMode)
  {
    sendstringf("VMXON=%6\n\r", currentcpuinfo->vmxdata.guest_vmxonaddress);
    sendstringf("VMCS=%6\n\r", currentcpuinfo->vmxdata.guest_activeVMCS);
    sendstringf("running=%d\n\r", currentcpuinfo->vmxdata.runningvmx);
  }



#endif

  if (isAMD==0)
  {
    sendstringf("vm_execution_controls_cpu=%6\n", vmread(vm_execution_controls_cpu));
    if (vmread(vm_execution_controls_cpu) & SECONDARY_EXECUTION_CONTROLS)
    {
      sendstringf("vm_execution_controls_cpu_secondary=%6 (unrestricted=%d)\n", vmread(vm_execution_controls_cpu_secondary), (vmread(vm_execution_controls_cpu_secondary) & SPBEF_ENABLE_UNRESTRICTED)!=0);
    }
  }



}

//int autocont=8;
int twister=0;
//int guestwantstoknow=0;

#if DISPLAYDEBUG==1
int verbosity=10;
#else
int verbosity=0;
#endif
int rotations=0;
int cpu2=0; //debug to stop cpu1 when cpu2 is spawned

int vmeventcount=0;
criticalSection vmexitlock={.name="vmexitlock", .debuglevel=0};


int vmexit_amd(pcpuinfo currentcpuinfo, UINT64 *registers, void *fxsave UNUSED)
{
 // displayline("vmexit_amd called. currentcpuinfo=%p\n", currentcpuinfo);
 // displayline("cpunr=%d\n", currentcpuinfo->cpunr);
  int result=0;
  currentcpuinfo->insideHandler=1;

  nosendchar[getAPICID()]=1;

  if (readMSR(IA32_FS_BASE_MSR)==0)
  {
    nosendchar[getAPICID()]=0;
    sendstringf("Invalid FS base during exception (currentcpuinfo=%6 vmeventcount=%d)\n", currentcpuinfo, vmeventcount);
    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
    while (1) outportb(0x80,0xc5);
  }
  vmeventcount++;



#ifdef DEBUG
  csEnter(&vmexitlock);

  if ((int)(vmexitlock.apicid-1)!=(int)(currentcpuinfo->apicid))
  {
    nosendchar[getAPICID()]=0;
    while (1)
    {
      sendstringf("lockcount inconsistency 3.  %d != %d  (%d)\n",vmexitlock.apicid,currentcpuinfo->apicid, getAPICID() );
    }
  }


  //sendstringf("vmexit_amd for cpu %d\n", currentcpuinfo->cpunr);

#endif

  if (dbvm_plugin_exit_pre)
  {
    BOOL r=dbvm_plugin_exit_pre(exportlist, currentcpuinfo, registers, fxsave);
    if (r)
    {
      sendstring("dbvm_plugin_exit_pre returned TRUE");
#ifdef DEBUG
      csLeave(&vmexitlock);
#endif
      currentcpuinfo->insideHandler=0;
      return 0;
    }
  }

  result=handleVMEvent_amd(currentcpuinfo, (VMRegisters*)registers, fxsave);

  if (dbvm_plugin_exit_post)
    dbvm_plugin_exit_post(exportlist, currentcpuinfo, registers, fxsave, &result);



#ifdef DEBUG
  if (vmexitlock.lockcount>1)
  {
    nosendchar[getAPICID()]=0;
    while (1)
    {
      sendstringf("lockcount inconsistency");
    }

  }
  csLeave(&vmexitlock);
#endif

  currentcpuinfo->insideHandler=0;

  if ((vmexitlock.lockcount>0) && ((int)(vmexitlock.apicid-1)==(int)(currentcpuinfo->apicid)))
  {
    nosendchar[getAPICID()]=0;
    while (1)
    {
      sendstringf("lockcount inconsistency 2");
    }

  }

  return result;
}

#ifdef debuglastexits
int lastexits[10];
int lastexitsindex=0;

criticalSection lastexitsCS={.name="lastexitsCS", .debuglevel=1};
#endif


#ifdef DEBUG

QWORD lastbeat=0;

int vmexit2(pcpuinfo currentcpuinfo, UINT64 *registers, void *fxsave);

int vmexit(pcpuinfo currentcpuinfo, UINT64 *registers, void *fxsave)
{
  int result;

  if (_rdtsc()>(lastbeat+100000000ULL))
  {
	  nosendchar[getAPICID()]=0;
	  enableserial();
	  //sendstringf("*Alive*\n");
	  lastbeat=_rdtsc();
  }



  //debug code
  csEnter(&vmexitlock);
  int used_vmstates_pos=vmstates_pos;

  StoreVirtualMachineState(currentcpuinfo, (VMRegisters*)registers); //store the event and all other information


  result=vmexit2(currentcpuinfo, registers, fxsave);


  vmstates[used_vmstates_pos].exit_cs=vmread(vm_guest_cs);
  vmstates[used_vmstates_pos].exit_cs_base=vmread(vm_guest_cs_base);
  vmstates[used_vmstates_pos].exit_ss=vmread(vm_guest_ss);
  vmstates[used_vmstates_pos].exit_ss_base=vmread(vm_guest_ss_base);

  vmstates[used_vmstates_pos].exit_rip=vmread(vm_guest_rip);
  vmstates[used_vmstates_pos].exit_rsp=vmread(vm_guest_rsp);
  vmstates[used_vmstates_pos].exit_rflags=vmread(vm_guest_rflags);

  //vmstates[used_vmstates_pos].exit_cr3=vmread(vm_guest_cr3);



  if ((result) && ((result >> 8)!=0xce))
  {
    nosendchar[getAPICID()]=0;
    sendvmstate(currentcpuinfo, (VMRegisters*)registers);
  }


  csLeave(&vmexitlock);

  return result;
}

int vmexit2(pcpuinfo currentcpuinfo, UINT64 *registers, void *fxsave)
#else


int showlife=0;

int vmexit(pcpuinfo currentcpuinfo, UINT64 *registers, void *fxsave)
#endif
{

  int haspending=0;
  VMExit_idt_vector_information idtvectorinfo;
  idtvectorinfo.idtvector_info=vmread(vm_idtvector_information);




#ifdef debuglastexits
  csEnter(&lastexitscs);
  lastexits[lastexitsindex]=vmread(vm_exit_reason);
  lastexitsindex++; //<----multithreaded issues here
  lastexitsindex=lastexitsindex % 10;
  csLease(&lastexitscs);

 // if ((showlife % 2)==0)
  {
    ddDrawRectangle(0,DDVerticalResolution-10,10,10,0x0000ff);
  }
#endif




#ifdef CHECKAPICID
  if (currentcpuinfo)
  {
    if (getAPICID()!=currentcpuinfo->apicid)
    {
      sendstring("FUCK\n");
      while(1);
    }


  }
  else
  {
    sendstring("WTFOMG\n");
    while (1);
  }
#endif



  if (dbvm_plugin_exit_pre)
  {
    BOOL r=dbvm_plugin_exit_pre(exportlist, currentcpuinfo, registers, fxsave);
    if (r)
    {
      ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
      return 0;
    }
  }


  if (currentcpuinfo==NULL)
  {
    nosendchar[getAPICID()]=0;
    sendstringf("currentcpuinfo==NULL");

    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);

    while (1) outportb(0x80,0xdc);
  }

  if (currentcpuinfo->vmxdata.runningvmx)
  {
    nosendchar[getAPICID()]=1;
    int r=handleVMEvent(currentcpuinfo, (VMRegisters*)registers, fxsave);

    if (dbvm_plugin_exit_post)
      dbvm_plugin_exit_post(exportlist, currentcpuinfo, registers, fxsave, &r);

    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);

    return r;
  }

  if (hasUnrestrictedSupport) //do this till I have added support for all efer read spots
  {
    //currentcpuinfo->efer=vmread(vm_guest_IA32_EFER);
    currentcpuinfo->guestCR0=vmread(vm_guest_cr0);
    //currentcpuinfo->guestCR4=vmread(vm_guest_cr4);
  }


  //if (ISPAGING(currentcpuinfo)==1)
  //  currentcpuinfo->guestCR3=vmread(vm_guest_cr3);


 // nosendchar[getAPICID()]=1;
  //return handleVMEvent(currentcpuinfo, (VMRegisters*)registers, fxsave);


  int skip=0;
  vmeventcount++;
/*
  //check if it's a (dos)timer event
  if ((vmread(vm_exit_reason)==0) && (vmread(vm_exit_interruptioninfo)==0x80000b0d) && (vmread(vm_idtvector_information)==0x80000008))
  {
    return handleVMEvent(currentcpuinfo, (VMRegisters*)registers, fxsave);
  }

  //check if it's a pre-emptiontimer event
  if (vmread(vm_exit_reason)==vm_exit_vmx_preemptiontimer_reachedzero)
  {
    return handleVMEvent(currentcpuinfo, (VMRegisters*)registers, fxsave);
  }
*/

  int result;


#ifndef DEBUG
  volatile int wait=1; //debug var

//  currentdisplayline=currentcpuinfo->cpunr+1;
//  currentdisplayrow=0;
//  displayline("%d: %d:%x (%x,%x)                              \n",currentcpuinfo->cpunr,vmeventcount,vmread(vm_exit_reason),vmread(vm_guest_cs),vmread(vm_guest_rip));

  //nosendchar[getAPICID()]=1;
  result=handleVMEvent(currentcpuinfo, (VMRegisters*)registers, fxsave);

  if (dbvm_plugin_exit_post)
    dbvm_plugin_exit_post(exportlist, currentcpuinfo, registers, fxsave, &result);


  /*
  if (idtvectorinfo.valid)
  {
    VMEntry_interruption_information entryintinfo;

    entryintinfo.interruption_information=vmread(vm_entry_interruptioninfo);
    if (entryintinfo.valid==0)
    {
      while (1);
    }
  }*/





  if (currentcpuinfo->NMIOccured==2) //nmi occured but no NMI window support
  {
    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0x0000ff);

    currentcpuinfo->NMIOccured=0;
    return raiseNMI();
  }

  //currentcpuinfo->lastTSCTouch=_rdtsc();


  //if ((showlife % 2)==0)
  {
    ddDrawRectangle(0,DDVerticalResolution-10,10,10,0x00ff00);
  }

  showlife++;


  if ((result!=0) && ((result >> 8) != 0xce)  )//on release, if an unexpected event happens, just fail the instruction and hope the OS won't make a too big mess out of it
  {
    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
    while (wait) ; //remove for release

    if ((vmread(vm_exit_reason) & 0x7fffffff)==vm_exit_invalid_guest_state) //invalid state
      return raiseGeneralProtectionFault(0); //perhaps this can fix it, else fuck
    else
      return raiseInvalidOpcodeException(currentcpuinfo);
  }
  else
  {
    if (result)
    {
      ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);

    }
    return result;
  }


#else
  //nosendchar[getAPICID()]=0;
  //sendstringf("%x:%x\n",vmread(vm_guest_cs),vmread(vm_guest_rip));
  //nosendchar[getAPICID()]=1;
#endif



  UINT64 initialcount UNUSED;


  //char lastevent[15];
  int userbreak=0;
  //DWORD before=*tocheck;

  nosendchar[getAPICID()]=0;
  //sendstring("vmexit\n\r");


  initialcount=1;









  //_cpuid(&a,&b,&c,&d);
  /*
  if (a)
  {
    userbreak=1;
    nosendchar[getAPICID()]=1;
  }
  */



  if (vmread(vm_pending_debug_exceptions))
    userbreak=1;



  /*
  if (vmread(0x4402)==30)
  {
    nosendchar[getAPICID()]=1;
    result=handleVMEvent(currentcpuinfo, (VMRegisters*)registers);
    return result;

  }*/






  if (IntHandlerDebug==0)
  {
    enableserial();

    char b=getchar();
    if (b=='b')
      userbreak=1;
  }

  char command;





  //stop all timers
  /*

  nosendchar[getAPICID()]=0;

  if (i==0)
    printstring("-",0,2,15,0);
  else
  if (i==1)
    printstring("\\",0,2,15,0);
  else
  if (i==2)
    printstring("|",0,2,15,0);
  else
  if (i==3)
    printstring("/",0,2,15,0);
  else
  if (i==4)
    printstring("-",0,2,15,0);
  else
  if (i==5)
    printstring("\\",0,2,15,0);
  else
  if (i==6)
    printstring("|",0,2,15,0);
  else
  if (i==7)
    printstring("/",0,2,15,0);

  i++;
  if (i==8)
  {
    i=0;
    rotations++;

    if (rotations==500)
    {
      sendstring("+");
      rotations=0;
    }
  }

  twister=i;
  */



 /*
  nosendchar[getAPICID()]=1;
  result=handleVMEvent(currentcpuinfo, (VMRegisters*)registers);
  nosendchar[getAPICID()]=0;
  //sendstring("Handled event\n\r");
  return result;
  */


  //if (vmread(0x6808)==0x2b0000)
//    userbreak=1;


  //if (DidHLT==0)

  //debugbreak();

 // showall=1;





  if (userbreak)
  {
    sendstring("user wants to break\n\r");
    verbosity=10;
  }
  else
  if ((vmread(vm_exit_reason)==0) && ((vmread(vm_exit_interruptioninfo) & 0x8000000f)==0x80000001) )
  {
	  //int1 bp
	  //sendstringf("Int 1 bp");

  }
  else
  //if (!showall)
  {

    switch (vmread(vm_exit_reason))
    {

     // case vm_exit_init:
     //   verbosity=10;
     //   skip=0;
     //   break;
//
      case vm_exit_vmlaunch:
        verbosity=10;
        skip=0;
        break;

      case vm_exit_vmresume:
        skip=currentcpuinfo->cpunr!=0;
        break;

      case vm_exit_vmcall:
    	  //skip=currentcpuinfo->cpunr==0;
        //sendstring("VMCALL\n");
        skip=1;
    	break;

      case vm_exit_vmread:
        skip=1;
        break;

      case vm_exit_vmwrite:
        skip=1;
        break;

      case vm_exit_vmptrld:
        //

        break;

      case vm_exit_invept:
        skip=1;
        break;

      case vm_exit_invvpid:
        skip=1;
        break;


      case vm_exit_cpuid:
        skip=2; //REALLY verbose
        break;


      case vm_exit_vmx_preemptiontimer_reachedzero:
        break;

      case vm_exit_invlpg:
        //skip=1;
        break;

      case vm_exit_io_access:
        //skip=1;
        break;

      case vm_exit_ept_violation:
      {
        //int cs=vmread(vm_guest_cs);
        //unsigned long long rip=vmread(vm_guest_rip);

        skip=1; //3=ultra verbose
        break;
      }

      case vm_exit_monitor_trap_flag:
      {
        skip=1;
        break;
      }

      case vm_exit_interrupt: //interrupt
      {
        switch (vmread(vm_exit_interruptioninfo))
        {
          case 0x80000300: //div by 0
            //skip=1;
            break;

          case 0x80000301: //int1 bp
            skip=1;
            break;

          case 0x80000307: //fp exception
           // skip=1;
            break;

          case 0x80000b0e: //pagefault
            //if (currentcpuinfo->cpunr==0)
             skip=1;
            break;

          case 0x80000603:
        	//if (vmread(vmread(vm_idtvector_information))==0)
        	  //skip=1;

            break;

          case 0x80000306:
            //skip=1;

            break;

          case 0x80000b0d: //GPF
            //check if it's a CS:RIP that we know and can skip (for dosmode)
            break;

        }
      }




      case vm_exit_cr_access:
      {
        //int cs=vmread(vm_guest_cs);
        //unsigned long long rip=vmread(vm_guest_rip);

        //if ((vmread(vm_exit_qualification) & 0xf)==3)
        {
         // skip=1;
        }


        break;
      }


      case vm_exit_sipi:
      {
        //int cs=vmread(vm_guest_cs);
        //unsigned long long rip=vmread(vm_guest_rip);
        skip=1;
        //verbosity=10;

        break;
      }

      case vm_exit_rdmsr:
      {
    	  VMRegisters* r=(VMRegisters*)registers;
        switch (r->rcx)
        {
          case 0x10:
          case 0x176:
          case 0x175:
          case 0x174:
          case 0x3a:
          case 0xc0000080:
        	  skip=1;
        	  break;

        }

        skip=1; //skip all rdmsr

        break;

      }

      case vm_exit_wrmsr:
      {
      	VMRegisters* r=(VMRegisters*)registers;
        switch (r->rcx)
        {
          case 0x10:
          case 0x3a:
          case 0xc0000080:
            skip=1;
          break;

        }

        skip=1;

        break;
      }

      case vm_exit_xsetbv:
        skip=1;
        break;

      case vm_exit_rdtsc:
        skip=1;
        break;



      case vm_exit_invalid_guest_state:
      {
        //int cs=vmread(vm_guest_cs);
        //unsigned long long rip=vmread(vm_guest_rip);
        nosendchar[getAPICID()]=0;
        sendstringf("invalid guest\n");

        skip=verbosity; //never


        break;
      }



    }

   // if (currentcpuinfo->cpunr) //debug code to test AP cpu's (only use if cpu0 boots properly)
   //   skip-=5;


    skip-=verbosity;




    if (skip>0)
    {
      nosendchar[getAPICID()]=1;

      result=handleVMEvent(currentcpuinfo, (VMRegisters*)registers, fxsave);

      if (currentcpuinfo->NMIOccured==2) //nmi occured but no NMI window support
      {
        currentcpuinfo->NMIOccured=0;
        return raiseNMI();
      }

      if ((result==0) || ((result >> 8)==0xce))
      {
        if (debugmode)
          setTrap();

        return 0;
      }

      nosendchar[getAPICID()]=0;
    }

  }

  while (IntHandlerDebug) ;

  nosendchar[getAPICID()]=0;
  /*
      if (before!=*tocheck)
      {
        nosendchar[getAPICID()]=0;
        sendstring("It's before enableserial\n\r");
        asm("hlt");
        while (1);

      }
      */

  enableserial();
  sendstringf("\n\r------------(%d)------------------\n\r",vmeventcount);
  sendstringf("Hello from vmexit-(cpunr=%d)",currentcpuinfo->cpunr);


  sendstringf("currentcpuinfo = %6  : APICID=%d  :  RSP=%6\n\r",(UINT64)currentcpuinfo, getAPICID(), getRSP());

  sendstringf("VM error code=%x\n\r",vmread(vm_errorcode));
  sendstringf("Exit reason=%8 (%d=%s) \n\r",vmread(vm_exit_reason),(vmread(vm_exit_reason) & 0x0fffffff), getVMExitReassonString());
  sendstringf("VM-exit interruption information=%x\n\r",vmread(vm_exit_interruptioninfo));

  sendstringf("VM-exit interruption error code=%x\n\r",vmread(vm_exit_interruptionerror));
  sendstringf("IDT-vectoring information field=%x\n\r",vmread(vm_idtvector_information));
  sendstringf("IDT-vectoring error code=%x\n\r",vmread(vm_idtvector_error));
  sendstringf("VM-exit instruction length=%x\n\r",vmread(vm_exit_instructionlength));
  sendstringf("VMX-instruction information=%x\n\r",vmread(vm_instruction_information));


  sendstringf("Exit qualification=%6\n\r",vmread(vm_exit_qualification));
  /*
  sendstringf("I/O RCX=%6\n\r",vmread(vm_io_rcx));
  sendstringf("I/O RSI=%6\n\r",vmread(vm_io_rsi));
  sendstringf("I/O RDI=%6\n\r",vmread(vm_io_rdi));
  sendstringf("I/O RIP=%6\n\r",vmread(vm_io_rip));*/

  sendstringf("Pending debug exceptions = %x\n\r",vmread(vm_pending_debug_exceptions));
  sendstringf("Guest linear   address=%6\n\r",vmread(vm_guest_linear_address));
  sendstringf("Guest physical address=%6\n\r",vmread(vm_guest_physical_address));

  RFLAGS rflags;
  rflags.value=vmread(vm_guest_rflags);

  sendstringf("rflags=%x (IF=%d TF=%d RF=%d)\n",rflags.value, rflags.IF, rflags.TF, rflags.RF);



  sendstringf("csbase=%6\n",vmread(vm_guest_cs_base));
  sendstringf("rip=%6\n",vmread(vm_guest_rip));

  {
    UINT64 ripaddress UNUSED=vmread(vm_guest_cs_base)+vmread(vm_guest_rip);
    sendstringf("ripaddress=%x\n", ripaddress);

    sendstringf("Rip=%6", vmread(vm_guest_cs_base)+vmread(vm_guest_rip));

  }


  sendstringf("guest cs=%8\n\r",vmread(vm_guest_cs));

  if (IS64BITCODE(currentcpuinfo))
  {
	  sendstringf("guest rip=%6\n\r",vmread(vm_guest_rip));
  }
  else
  {
	  sendstringf("guest eip=%8\n\r",vmread(vm_guest_rip));
  }
  sendstring("Instruction = ");
  ShowCurrentInstruction(currentcpuinfo);

  /*
      if (before!=*tocheck)
      {
        nosendchar[getAPICID()]=0;
        sendstring("It's before the while loop\n\r");
        asm("hlt");
        while (1);

      }
      */


  while (1)
  {


    //menu
    sendstring("/-----------------VM-EXIT --------------\\\n\r");
    sendstring("|   1: resume virtual machine           |\n\r");
    sendstring("|   2: show state of virtual machine    |\n\r");
    sendstring("|   3: show memory of virtual machine   |\n\r");
    sendstringf("|   4: increase verbosity (%d)          |\n\r", verbosity);
    sendstringf("|   5: decrease verbosity (%d)          |\n\r", verbosity);
    sendstring("|   6: show instruction memory          |\n\r");
    sendstringf("|   7: toggle debugmode (%d)             |\n\r",debugmode);
    sendstring("|   8: set breakpoint                   |\n\r");
    sendstring("|   9: display physical memory          |\n\r");
    sendstring("|   l: Lua Engine                       |\n\r");
    sendstring("|   0: quit virtual machine             |\n\r");
  //sendstring("|   p: previous event                   |\n\r");
    sendstring("\\---------------------------------------/\n\r");
    sendstring("Your command:");

#ifdef DELAYEDSERIAL
    if (!useserial)
      command='1';
    else
#endif
    command=waitforchar();
    sendstring("\n\r");

    switch (command)
    {
      case  '1' :
        result=handleVMEvent(currentcpuinfo, (VMRegisters*)registers, fxsave);

        if (dbvm_plugin_exit_post)
          dbvm_plugin_exit_post(exportlist, currentcpuinfo, registers, fxsave, &result);



        if (currentcpuinfo->NMIOccured==2) //nmi occured but no NMI window support
        {
          currentcpuinfo->NMIOccured=0;
          return raiseNMI();
        }

        if (currentcpuinfo->cpunr)
        {
          //sendstring("cpunr!=0");
        }

        //setResumeFlag();
        //sendstringf("Returned from handleVMEvent. result=%d (CR0=%x)\n\r",result,vmread(vm_guest_cr0));

        //if (currentcpuinfo->cpunr==1)
        //  sendvmstate(currentcpuinfo, (VMRegisters*)registers);


        if ((result!=0) && ((result >> 8)!=0xce))
        {
          sendstring("EVENT DID NOT GET HANDLED\n");

        }

        return result;


      case  '2' :
  	sendvmstate(currentcpuinfo, (VMRegisters*)registers);
  	break;

      case  '3' :
        displayVMmemory(currentcpuinfo);
  	break;

      case  '4' :
        verbosity++;
        break;

      case '5':
        verbosity--;
        break;

      case  '6' :
      {
        sendstring("Going to show the instruction:\n\r");
        ShowCurrentInstructions(currentcpuinfo);


        break;
      }


      case  '7' :
        debugmode=!debugmode;
        break;

      case  '8' :
      {
        char temps[17];
        UINT64 BreakAddress;

        regDR7 dr7;
        dr7.DR7=vmread(vm_guest_dr7);

        sendstring("Startaddress:");
        readstring(temps,16,16);
        sendstring("\n\r");
        BreakAddress=atoi2(temps,16,NULL);
        setDR0(BreakAddress);

        //set DR7 of the guest to break on execution of DR0
        dr7.L0=1; //activate
        dr7.G0=1;
        dr7.RW0=0; //break on execution
        dr7.LEN0=0; //1 byte, used for exec breakpoints
        vmwrite(vm_guest_dr7,(UINT64)dr7.DR7); //dr7

        breakpointset=1;
        break;
      }

      case  'W' :
      {
        char temps[17];
        UINT64 BreakAddress;

        regDR7 dr7;
        dr7.DR7=vmread(vm_guest_dr7);

        sendstring("Startaddress:");
        readstring(temps,16,16);
        sendstring("\n\r");
        BreakAddress=atoi2(temps,16,NULL);
        setDR0(BreakAddress);

        //set DR7 of the guest to break on execution of DR0
        dr7.L0=1; //activate
        dr7.G0=1;
        dr7.RW0=1; //break on write
        dr7.LEN0=0; //1 byte, used for exec breakpoints
        vmwrite(vm_guest_dr7,(UINT64)dr7.DR7); //dr7

        breakpointset=1;
        break;
      }

      case  '9' :
      {
        displayPhysicalMemory();
        break;

      }

  		case  '0' :
        sendstringf("Leaving vmm mode (RSP=%x)\n\r",getRSP());

        sendstring("Returning 1\n\r");
  			return 1;

      case  'a' :
      {
        //disassemble
        char temps[17];
        int bits;
        UINT64 address;
        int size;
        int err1,err2,err3;
        _DecodedInst disassembled[16];
        _DecodeType dt=Decode16Bits;
        int i;
        unsigned int used=0;


        Access_Rights cs_accessright;
        cs_accessright.AccessRights=vmread(vm_guest_cs_access_rights);
        if (cs_accessright.D_B==0)
          dt=Decode16Bits;
        else
          dt=Decode32Bits;

        if (IS64BITPAGING(currentcpuinfo))
          dt=Decode32Bits;

        if (IS64BITCODE(currentcpuinfo))
          dt=Decode64Bits;

        if (ISREALMODE(currentcpuinfo))
          dt=Decode16Bits;

        sendstring("Bitmode(");
        if (dt==Decode16Bits)
        {
          sendstring("16):");
        }
        else
        if (dt==Decode32Bits)
        {
          sendstring("32):");
        }
        else
        if (dt==Decode64Bits)
        {
          sendstring("64):");
        }

        readstring(temps,2,16);
        bits=atoi2(temps,10,&err1);

        if (bits==16)
          dt=Decode16Bits;
        else
        if (bits==32)
          dt=Decode32Bits;
        else
        if (bits==64)
          dt=Decode64Bits;
        else
        {
          sendstring("default bits");
        }

        sendstring("\n\rAddress:");
        readstring(temps,16,17);
        sendstringf("Address= %s \n",temps);
        address=atoi2(temps,16,&err2);


        sendstring("\n\rNumber of bytes:");
        readstring(temps,16,17);
        size=atoi2(temps,10,&err3);

        sendstringf("\naddress=%6, size=%d: (e1=%d e2=%d e3=%d)\n", address, size,err1,err2,err3);

        {
          int readable=0;
          int disassemblecount=0;
          int oldcurrentaddress;

          unsigned char buf[size];
          readable=ReadVMMemory(currentcpuinfo, address,buf,size);
          if (readable)
          {
            UINT64 currentaddress=address;





            while ((UINT64)currentaddress<(UINT64)((UINT64)address+(UINT64)size))
            {
              used=0;
              distorm_decode(currentaddress,&buf[disassemblecount], size, dt, disassembled, 16, &used);

              if (used)
              {
                //printf("used=%d\n", used);
                for (i=0; (unsigned)i<used; i++)
                {
                  sendstringf("%8 : %s - %s %s\n\r",
                        disassembled[i].offset,
                        disassembled[i].instructionHex.p,
                        disassembled[i].mnemonic.p,
                        disassembled[i].operands.p);
                }

                oldcurrentaddress=currentaddress;

                currentaddress=(UINT64)disassembled[used-1].offset+(UINT64)disassembled[used-1].size;

                disassemblecount+=(currentaddress-oldcurrentaddress);
                sendstringf("(debug)currentaddress=%x\n\r",currentaddress);
              }
              else
              {
                sendstring("Disassemble failed\n\r");
                break; //exit while loop
              }
            }


          }
          else
          {
        	sendstring("\n\rError reading memory\n\r");
          }
        }

        break;

      }

      case  'c' :
      {
        CheckCRCValues();
        break;
      }

      case 'l' :
      {
#if (defined SERIALPORT) && (SERIALPORT != 0)
        enterLuaConsole();
#endif
        break;
      }

#ifdef DEBUG
      case  'p' :
      {
        displayPreviousStates();
        break;


      }
#endif

      case  'r' :
      {
        sendstring("retry\n\r");

        return 0;
      }



      case  'i' :
      {
        //test interrupt
        //vmx_enableNMIWindowExiting();
        //vmx_enableSingleStepMode();

        //setup a memory watch for physical address 0x7000
        //int ID;
        //int r;
        //sendstringf("Setting write watch at 0x7000 to 0x7fff\n");


        //r=ept_watch_activate(0x7000, 4096, 0,0,64, &ID);

        //sendstringf("ept_watch_activate returned %d and ID %d\n", r,ID);
        break;
      }




      case  's' :
      {
        vmx_enableSingleStepMode();
        /*
        UINT64 address;
        unsigned char bt;
        char temps[17];

        sendstring("\n\rAddress:");
        readstring(temps,16,16);
        address=atoi2(temps,16,NULL);

        sendstring("\n\rValue=");
        readstring(temps,2,16);
        bt=atoi2(temps,16,NULL);

        *(unsigned char *)address=bt;
        sendstring("\n\r");
        */
        break;
      }

      case 'm':
      {
        ept_hideDBVMPhysicalAddressesAllCPUs();
        break;
      }

      default:
        sendstring("Unknown command\n\r");
        break;
  	}
  }


  return 1;
}




void launchVMX_AMD(pcpuinfo currentcpuinfo, POriginalState originalstate)
{
  int result;

  displayline("Calling vmxloop_amd with currentcpuinfo=%6\n\r",(UINT64)currentcpuinfo);

  sendvmstate(currentcpuinfo, NULL);



  if (originalstate)
    result=vmxloop_amd(currentcpuinfo, currentcpuinfo->vmcb_PA, &originalstate->rax);
  else
    result=vmxloop_amd(currentcpuinfo, currentcpuinfo->vmcb_PA, NULL);

  displayline("Returned from vmxloop_amd. Result=%d\n\r", result);

}

void launchVMX(pcpuinfo currentcpuinfo)
{


  int result;
  OriginalState os;
  POriginalState originalstate=NULL;
  int restorestate=((loadedOS) && ((currentcpuinfo->isboot) || (APStartsInSIPI==0)));

  if (restorestate)
  {
    POriginalState pos=(POriginalState)mapPhysicalMemory(loadedOS,sizeof(OriginalState));
    os=*pos;
    originalstate=&os;
    unmapPhysicalMemory(pos, sizeof(OriginalState));
  }

  outportb(0x80,0x01);



  if (isAMD)
    return launchVMX_AMD(currentcpuinfo, originalstate);



  displayline("Calling vmxloop with currentcpuinfo=%6\n\r",(UINT64)currentcpuinfo);

  sendstring("Right before entering the loop:\n");


  if (currentcpuinfo->cpunr==0)
  {
    sendvmstate(currentcpuinfo,NULL);
    displayline("Last display before entering vmx\n");

  }

  outportb(0x80,0x02);

  if (restorestate)
    result=vmxloop(currentcpuinfo, &originalstate->rax);
  else
    result=vmxloop(currentcpuinfo, NULL);

  outportb(0x80,0xCF);

  displayline("VMXLOOP EXIT: APICID=%d\n\r",getAPICID());
  nosendchar[getAPICID()]=0;
  if (result==0)
    displayline("%d: vmxloop was successfull and returned normal (as in it quit the loop)\n\r", currentcpuinfo->cpunr);
  else
  if (result==1)
    displayline("%d: vmxloop returned 1, meaning vmlaunch failed completly\n\r", currentcpuinfo->cpunr);
  else
  if (result==2)
  {
    displayline("%d: vmxloop returned 2, meaning vmlaunch only failed half\n\r", currentcpuinfo->cpunr);
  }
  else
    displayline("%d: vmxloop returned %d. WEIRD ERROR!\n\r", currentcpuinfo->cpunr,result);

  displayline("%d: VM error code=%8\n\r", currentcpuinfo->cpunr, vmread(vm_errorcode));
  displayline("%d: Exit reason=%8\n\r", currentcpuinfo->cpunr, vmread(vm_exit_reason));
  displayline("%d: currentcpuinfo=%6\n\r", currentcpuinfo->cpunr, (UINT64)currentcpuinfo);

  if (vmread(vm_errorcode)==7)
  {
    sendstringf("Invalid control fields\n");
    QWORD VMX_BASIC UNUSED=readMSR(IA32_VMX_BASIC_MSR);
    QWORD VMX_PINBASED_CTLS UNUSED=readMSR(IA32_VMX_PINBASED_CTLS_MSR);
    QWORD VMX_PROCBASED_CTLS UNUSED=readMSR(IA32_VMX_PROCBASED_CTLS_MSR);

    QWORD VMX_EXIT_CTLS UNUSED=readMSR(IA32_VMX_EXIT_CTLS_MSR);
    QWORD VMX_ENTRY_CTLS UNUSED=readMSR(IA32_VMX_ENTRY_CTLS_MSR);
    QWORD VMX_MISC UNUSED=readMSR(IA32_VMX_MISC_CTLS_MSR);

    DWORD ctrl_pin UNUSED=vmread(vm_execution_controls_pin);
    sendstringf("VMX_PINBASED_CTLS=%6 ctrl_pin=%8\n", VMX_PINBASED_CTLS, ctrl_pin);

    DWORD ctrl_cpu UNUSED=vmread(vm_execution_controls_cpu);
    sendstringf("VMX_PROCBASED_CTLS=%6 ctrl_cpu=%8\n", VMX_PROCBASED_CTLS, ctrl_cpu);

    DWORD ctrl_entry UNUSED=vmread(vm_entry_controls);
    sendstringf("VMX_ENTRY_CTLS=%6 ctrl_entry=%8\n", VMX_ENTRY_CTLS, ctrl_entry);

    DWORD ctrl_exit UNUSED=vmread(vm_exit_controls);
    sendstringf("VMX_EXIT_CTLS=%6 ctrl_exit=%8\n", VMX_EXIT_CTLS, ctrl_exit);





  }

}
/*

void CheckGuest(void)
{
	ULONG IA32_VMX_CR0_FIXED0,IA32_VMX_CR0_FIXED1;
	ULONG IA32_VMX_CR4_FIXED0,IA32_VMX_CR4_FIXED1;
	ULONG guestcr0,guestcr4,guestrflags;
	ULONG guestCSlimit,guestSSlimit,guestDSlimit,guestESlimit,guestFSlimit,guestGSlimit,guestIDTlimit,guestGDTlimit;
	Selector_Field guest_ldtr,guest_tr,guest_cs,guest_ss,guest_ds,guest_es,guest_fs,guest_gs;
	Access_Rights guest_accessrights_ldtr,guest_accessrights_tr,guest_accessrights_cs,guest_accessrights_ss,guest_accessrights_ds,guest_accessrights_es,guest_accessrights_fs,guest_accessrights_gs;

  ULONG vmentry_ctls;
	UINT64 guest_IA32_DEBUGCTL;

	ULONG temp;
	UINT64 temp64;

	int v8086=0;

	sendstringf("checking state. The following are errors in the state that need to be fixed:\n\r");

	IA32_VMX_CR0_FIXED0=(ULONG)readMSR(0x486);
	IA32_VMX_CR0_FIXED1=(ULONG)readMSR(0x487);
	IA32_VMX_CR4_FIXED0=(ULONG)readMSR(0x488);
	IA32_VMX_CR4_FIXED1=(ULONG)readMSR(0x489);

	guest_IA32_DEBUGCTL=((UINT64)vmread(0x2803) << 32)+vmread(0x2802); //IA32_DEBUGCTL (low)
	vmentry_ctls=vmread(vm_entry_controls);
	guestcr0=vmread(vm_guest_cr0);
	guestcr4=vmread(vm_guest_cr4);
	guestrflags=vmread(vm_guest_rflags);

	guest_accessrights_es.AccessRights=vmread(0x4814);
	guest_accessrights_cs.AccessRights=vmread(0x4816);
	guest_accessrights_ss.AccessRights=vmread(0x4818);
	guest_accessrights_ds.AccessRights=vmread(0x481a);
	guest_accessrights_fs.AccessRights=vmread(0x481c);
	guest_accessrights_gs.AccessRights=vmread(0x481e);
	guest_accessrights_ldtr.AccessRights=vmread(0x4820);
	guest_accessrights_tr.AccessRights=vmread(0x4822);

	guest_es.Selectorvalue=vmread(0x800);
	guest_cs.Selectorvalue=vmread(0x802);
	guest_ss.Selectorvalue=vmread(0x804);
	guest_ds.Selectorvalue=vmread(0x806);
	guest_fs.Selectorvalue=vmread(0x808);
	guest_gs.Selectorvalue=vmread(0x80a);
	guest_ldtr.Selectorvalue=vmread(0x80c);
	guest_tr.Selectorvalue=vmread(0x80e);

	guestIDTlimit=vmread(0x4810);
	guestGDTlimit=vmread(0x4812);

	guestCSlimit=vmread(0x4802);
	guestSSlimit=vmread(0x4804);
	guestDSlimit=vmread(0x4806);
	guestESlimit=vmread(0x4800);
	guestFSlimit=vmread(0x4808);
	guestGSlimit=vmread(0x480a);



	//22.3.1.1:
	sendstringf("checking 22.3.1.1\n\r");
	temp=guestcr0 | IA32_VMX_CR0_FIXED0;
	if (temp != guestcr0)
		sendstringf("Error: not all required bits are set in cr0 (cr0=%8, cr0 should be %8)\n\r",guestcr0,temp);

	temp=guestcr0 & IA32_VMX_CR0_FIXED1;
	if (temp != guestcr0)
		sendstringf("Error: not all required bits are unset in cr0\n\r");


	temp=guestcr4 | IA32_VMX_CR4_FIXED0;
	if (temp != guestcr4)
		sendstringf("Error: not all required bits are set in cr4(cr4=%8 cr4 should be %8)\n\r",guestcr4,temp);

	temp=guestcr4 & IA32_VMX_CR4_FIXED1;
	if (temp != guestcr4)
		sendstringf("Error: not all required bits are unset in cr4 (cr4=%8 should be %8)\n\r",guestcr4,temp);


	temp64=(UINT64)((UINT64)0x0FFFFFFFFFFFFE03CULL & (UINT64)guest_IA32_DEBUGCTL);
	if (temp64 > 0)
		sendstringf("Error: IA32_DEBUGCTL has reserved bits set\n\r");

	if ((vmentry_ctls & (1 << 9)) == 1 )
		sendstringf("Error: IA32e mode guest in a 32-bit host\n\r");


	//22.3.1.2:
	sendstringf("checking 22.3.1.2\n\r");
	sendstringf("guestrflags=%x\n\r",guestrflags);
	if ((guestrflags & (1<<17))>0)
	{
		sendstringf("v8086 mode\n\r");
		v8086=1;
	}



	if (guest_tr.TI==1)
		sendstringf("Error: TR has the TI bit set\n\r");

	if ((guest_accessrights_ldtr.unusable==0) && (guest_ldtr.TI==1))
		sendstringf("Error: LDTR is usable and LDTR has the TI bit set\n\r");

	if (v8086==0)
	{
		if (( guest_cs.RPL != guest_ss.RPL ))
			sendstringf("Error: the RPL of ss does not matych the RPL of cs and not in virtual 8086 mode\n\r");

		if ((guest_accessrights_cs.Segment_type & (1 << 0)) == 0)
			sendstringf("Error: CS is not accessed\n\r");

		if ((guest_accessrights_cs.Segment_type & (1<<3)) == 0)
			sendstringf("Error: CS is no codesegment\n\r");

		if (guest_accessrights_cs.S!=1)
			sendstringf("Error: S is not 1 for cs\n\r");

		if (guest_accessrights_cs.P!=1)
			sendstringf("Error: P is not 1 for cs\n\r");

		if (guest_accessrights_cs.reserved!=0)
			sendstringf("Error: reserved (8 to 11) is not 0 for cs\n\r");

		if (guest_accessrights_cs.G==0)
			sendstringf("Error: cs.G==0\n\r");


		if (((guest_accessrights_cs.Segment_type>=8) && (guest_accessrights_cs.Segment_type<=11)) && (guest_accessrights_cs.DPL != guest_cs.RPL ))
			sendstringf("Error: CS: Codesegment is nonconforming and DPL does not match RPL\n\r");

		if (((guest_accessrights_cs.Segment_type>=13) && (guest_accessrights_cs.Segment_type<=15)) && (guest_accessrights_cs.DPL > guest_cs.RPL ))
			sendstringf("Error: CS: Codesegment is conforming and DPL is bigger than RPL\n\r");

		if (guest_accessrights_ss.unusable==0)
		{
			if (!((guest_accessrights_ss.Segment_type == 3) || (guest_accessrights_ss.Segment_type == 7) ))
				sendstringf("Error: SS type is not 3 or 7\n\r");

			if (guest_accessrights_ss.S!=1)
				sendstringf("Error: S is not 1 for ss\n\r");

			if (guest_accessrights_ss.P!=1)
				sendstringf("Error: P is not 1 for ss\n\r");

			if (guest_accessrights_ss.reserved!=0)
				sendstringf("Error: reserved (8 to 11) is not 0 for ss\n\r");

			if (guest_accessrights_ss.G==0)
				sendstringf("Error: ss.G==0\n\r");
		}

		if (guest_accessrights_ds.unusable==0)
		{
			if ((guest_accessrights_ds.Segment_type & 1)!=1)
				sendstringf("Error: DS type is not accessed\n\r");

			if (((guest_accessrights_ds.Segment_type & (1 << 3))>0) && ((guest_accessrights_ds.Segment_type & (1 << 1))==0))
				sendstringf("Error: DS has a code segment set but is not readable\n\r");

			if (guest_accessrights_ds.S!=1)
				sendstringf("Error: S is not 1 for ds\n\r");

			if (guest_accessrights_ds.P!=1)
				sendstringf("Error: P is not 1 for ds\n\r");

			if (guest_accessrights_ds.reserved!=0)
				sendstringf("Error: reserved (8 to 11) is not 0 for ds\n\r");

			if (guest_accessrights_ds.G==0)
				sendstringf("Error: ds.G==0\n\r");
		}

		if (guest_accessrights_es.unusable==0)
		{
			if ((guest_accessrights_es.Segment_type & 1)!=1)
				sendstringf("Error: es type is not accessed\n\r");

			if (((guest_accessrights_es.Segment_type & (1 << 3))>0) && ((guest_accessrights_es.Segment_type & (1 << 1))==0))
				sendstringf("Error: es has a code segment set but is not readable\n\r");

			if (guest_accessrights_es.S!=1)
				sendstringf("Error: S is not 1 for es\n\r");

			if (guest_accessrights_es.P!=1)
				sendstringf("Error: P is not 1 for es\n\r");

			if (guest_accessrights_es.reserved!=0)
				sendstringf("Error: reserved (8 to 11) is not 0 for es\n\r");

			if (guest_accessrights_es.G==0)
				sendstringf("Error: es.G==0\n\r");

		}

		if (guest_accessrights_fs.unusable==0)
		{
			if ((guest_accessrights_fs.Segment_type & 1)!=1)
				sendstringf("Error: fs type is not accessed\n\r");

			if (((guest_accessrights_fs.Segment_type & (1 << 3))>0) && ((guest_accessrights_fs.Segment_type & (1 << 1))==0))
				sendstringf("Error: fs has a code segment set but is not readable\n\r");

			if (guest_accessrights_fs.S!=1)
				sendstringf("Error: S is not 1 for fs\n\r");

			if (guest_accessrights_fs.P!=1)
				sendstringf("Error: P is not 1 for fs\n\r");

			if (guest_accessrights_fs.reserved!=0)
				sendstringf("Error: reserved (8 to 11) is not 0 for fs\n\r");

			if (guest_accessrights_fs.G==0)
				sendstringf("Error: fs.G==0\n\r");
		}

		if (guest_accessrights_gs.unusable==0)
		{
			sendstringf("GS is USABLE!\n\r");
			if ((guest_accessrights_gs.Segment_type & 1)!=1)
				sendstringf("Error: gs type is not accessed\n\r");

			if (((guest_accessrights_gs.Segment_type & (1 << 3))>0) && ((guest_accessrights_gs.Segment_type & (1 << 1))==0))
				sendstringf("Error: gs has a code segment set but is not readable\n\r");

			if (guest_accessrights_gs.S!=1)
				sendstringf("Error: S is not 1 for gs\n\r");

			if (guest_accessrights_gs.P!=1)
				sendstringf("Error: P is not 1 for gs\n\r");

			if (guest_accessrights_gs.reserved!=0)
				sendstringf("Error: reserved (8 to 11) is not 0 for gs\n\r");

			if (guest_accessrights_gs.G==0)
				sendstringf("Error: gs.G==0\n\r");
		}
	}
	else
	{
		if (guest_accessrights_cs.AccessRights!=0xf3)
			sendstringf("CS accessrights are not 0xf3\n\r");

		if (guest_accessrights_ss.AccessRights!=0xf3)
			sendstringf("SS accessrights are not 0xf3\n\r");

		if (guest_accessrights_ds.AccessRights!=0xf3)
			sendstringf("DS accessrights are not 0xf3\n\r");

		if (guest_accessrights_es.AccessRights!=0xf3)
			sendstringf("ES accessrights are not 0xf3\n\r");

		if (guest_accessrights_fs.AccessRights!=0xf3)
			sendstringf("FS accessrights are not 0xf3\n\r");

		if (guest_accessrights_gs.AccessRights!=0xf3)
			sendstringf("GS accessrights are not 0xf3\n\r");


	}



	if (guest_accessrights_tr.unusable==0)
	{

		if (v8086==0)
		{
			if ((guest_accessrights_tr.Segment_type !=11))
				sendstringf("Error: TR type is not 11\n\r");
		}
		else
		{
			if ((guest_accessrights_tr.Segment_type !=3) && (guest_accessrights_tr.Segment_type !=11))
				sendstringf("Error: TR type is not 3 or 11\n\r");
		}

		if (guest_accessrights_tr.S!=0)
			sendstringf("Error: S is not 0 for tr\n\r");

		if (guest_accessrights_tr.P!=1)
			sendstringf("Error: P is not 1 for tr\n\r");

		if (guest_accessrights_tr.reserved!=0)
			sendstringf("Error: reserved (8 to 11) is not 0 for tr\n\r");

		if (guest_accessrights_tr.G==1)
			sendstringf("Error: tr.G==1\n\r");
	}


	if ((v8086==1) && (guestCSlimit>0xffff))
		sendstringf("CS limit > 0xffff and in virtual 8086 mode\n\r");

	if ((v8086==1) && (guestSSlimit>0xffff))
		sendstringf("SS limit > 0xffff and in virtual 8086 mode\n\r");

	if ((v8086==1) && (guestDSlimit>0xffff))
		sendstringf("DS limit > 0xffff and in virtual 8086 mode\n\r");

	if ((v8086==1) && (guestESlimit>0xffff))
		sendstringf("ES limit > 0xffff and in virtual 8086 mode\n\r");

	if ((v8086==1) && (guestFSlimit>0xffff))
		sendstringf("FS limit > 0xffff and in virtual 8086 mode\n\r");

	if ((v8086==1) && (guestGSlimit>0xffff))
		sendstringf("GS limit > 0xffff and in virtual 8086 mode\n\r");

	if (guestIDTlimit>0xffff)
		sendstringf("IDT limit beyond 0xffff\n\r");

	if (guestGDTlimit>0xffff)
		sendstringf("GDT limit beyond 0xffff\n\r");


	sendstringf("check done\n\r");
}


*/
void displayVMmemory(pcpuinfo currentcpuinfo)
{
  char temps[17];
  UINT64 StartAddress;
  int nrofbytes;
  int i,j;
  unsigned char buf[17];
  int err=0;
#ifdef DEBUG

  int notpaged=0;
#endif

  sendstring("Startaddress:");
  readstring(temps,16,17);
  sendstring("\n\r");
  StartAddress=atoi2(temps,16,&err);

  sendstringf("%6(%s) has physical address %6\n\r",StartAddress,temps, getPhysicalAddressVM(currentcpuinfo, StartAddress, &notpaged));

  if (err)
  {
    sendstringf("err=%d\n");
  }


  sendstring("Number of bytes:");
  readstring(temps,8,8);
  sendstring("\n\r");
  nrofbytes=atoi2(temps,10,NULL);


  //if virtual machine check 0-00200000
  //else check pagetables
  for (i=0; i<nrofbytes; i+=16)
  {

    if (ReadVMMemory(currentcpuinfo, StartAddress+i,buf,16)==0)
    {
      sendstringf("Read error\n\r");
      return;
    }

    sendstringf("%6 : ",StartAddress+i);
    for (j=i; j<(i+16) && (j<nrofbytes); j++)
      sendstringf("%2 ",buf[j%16]);

    if ((i+16)>nrofbytes)
    {
      // Get the cursor to the right spot
      int currentcol=11+3*(nrofbytes-i);
      int wantedcol=11+3*16;
      for (j=0; j<(wantedcol-currentcol); j++)
        sendstring(" ");
    }

    for (j=i; j<(i+16) && (j<nrofbytes); j++)
    {
      unsigned char tempc=buf[j%16];
      if (tempc<32)
        tempc='.';

      sendstringf("%c",tempc);
    }

    sendstring("\n\r");
  }

}

void ShowCurrentInstruction(pcpuinfo currentcpuinfo)
{
  unsigned char buf[60];
  int is64bit=IS64BITCODE(currentcpuinfo);
  QWORD address;

  if (isAMD)
  {
    if (is64bit)
      address=currentcpuinfo->vmcb->RIP;
    else
      address=currentcpuinfo->vmcb->cs_base+currentcpuinfo->vmcb->RIP;
  }
  else
  {
    if (is64bit)
      address=vmread(vm_guest_rip);
    else
      address=vmread(vm_guest_cs_base)+vmread(vm_guest_rip);
  }

  //sendstringf("ShowCurrentInstruction for %6\n", address);


  int br=ReadVMMemory(currentcpuinfo, address, buf, 60);
 // sendstringf("br=%d\n",br);

  if (br)
  {

    _DecodedInst disassembled[22];
    _DecodeType dt=Decode16Bits;
    Access_Rights cs_accessright;
    unsigned int used=0;

    //find out in which context the system is operating
    if (isAMD)
    {
      Segment_Attribs csa;
      csa.SegmentAttrib=currentcpuinfo->vmcb->cs_attrib;
      cs_accessright.D_B=csa.D_B;
    }
    else
      cs_accessright.AccessRights=vmread(vm_guest_cs_access_rights);

    if (cs_accessright.D_B==0)
      dt=Decode16Bits;
    else
      dt=Decode32Bits;

    if (IS64BITPAGING(currentcpuinfo))
      dt=Decode32Bits;

    if (IS64BITCODE(currentcpuinfo))
      dt=Decode64Bits;

    if (ISREALMODE(currentcpuinfo))
      dt=Decode16Bits;


    distorm_decode(address,buf, 60, dt, disassembled, 22, &used);
    if (used)
    {
      sendstringf("%6 : %s - %s %s\n\r",
                    disassembled[0].offset,
                    disassembled[0].instructionHex.p,
                    disassembled[0].mnemonic.p,
                    disassembled[0].operands.p);

    }


  }
  else
  {
    sendstring("Unreadable memory\n");
  }
}

void ShowCurrentInstructions(pcpuinfo currentcpuinfo)
{
  unsigned char buf[60];
  int     readable;
  int     is64bit=IS64BITCODE(currentcpuinfo);
  UINT64 address;

  if (isAMD)
  {
    if (is64bit)
      address=currentcpuinfo->vmcb->RIP;
    else
      address=currentcpuinfo->vmcb->cs_base+currentcpuinfo->vmcb->RIP;
  }
  else
  {
    if (is64bit)
      address=vmread(vm_guest_rip);
    else
      address=vmread(vm_guest_cs_base)+vmread(vm_guest_rip);
  }

  int bytesinfront=30;
  UINT64 startaddress=address-bytesinfront;

  readable=ReadVMMemory(currentcpuinfo, startaddress,buf,60);

  while (!readable) //try till bytesinfront=0
  {
    startaddress=address-bytesinfront;
    readable=ReadVMMemory(currentcpuinfo, startaddress,buf,60);
    if (!readable)
    {
      if (bytesinfront==0)
      {
        //failed at even 0
        sendstringf("%6 is unreadable\n\r",address);
        break;
      }
      bytesinfront=bytesinfront / 2;
    }
  }

  if (readable)
  {
    //disassemble
    _DecodedInst disassembled[22];
    _DecodeType dt=Decode16Bits;
    Access_Rights cs_accessright;
    unsigned int i;
    unsigned int used=0;

    //find out in which context the system is operating

    if (isAMD)
    {
      Segment_Attribs csa;
      csa.SegmentAttrib=currentcpuinfo->vmcb->cs_attrib;
      cs_accessright.D_B=csa.D_B;
    }
    else
      cs_accessright.AccessRights=vmread(vm_guest_cs_access_rights);

    if (cs_accessright.D_B==0)
      dt=Decode16Bits;
    else
      dt=Decode32Bits;

    if (IS64BITPAGING(currentcpuinfo))
      dt=Decode32Bits;

    if (IS64BITCODE(currentcpuinfo))
      dt=Decode64Bits;

    if (ISREALMODE(currentcpuinfo))
      dt=Decode16Bits;


    distorm_decode(startaddress,buf, 60, dt, disassembled, 22, &used);

    if (used)
    {
      for (i=0; i<used; i++)
      {
        if (disassembled[i].offset==address)
        {
          sendstring(">>");
        }

        sendstringf("%6 : %s - %s %s\n\r",
              disassembled[i].offset,
              disassembled[i].instructionHex.p,
              disassembled[i].mnemonic.p,
              disassembled[i].operands.p);
      }
    }
    else
    {
    	sendstring("Disassemble failed\n\r");
    }



  }
}

void displayPhysicalMemory(void)
{
  unsigned int i;
  char temps[17];
  UINT64 StartAddress;
  unsigned int nrofbytes;

  sendstring("Startaddress:");
  readstring(temps,16,17);
  sendstring("\n\r");
  temps[16]=0;
  StartAddress=atoi2(temps,16,NULL);
  sendstringf("Startaddress=%6\n\r",StartAddress);


  sendstring("Number of bytes:");
  readstring(temps,8,17);
  sendstring("\n\r");
  nrofbytes=atoi2(temps,10,NULL);

  sendstringf("Going to show the memory region %6 to %6 \n\r",StartAddress,StartAddress+nrofbytes);

  unsigned char *memory=(unsigned char *)mapPhysicalMemory(StartAddress, nrofbytes);
  for (i=0; i<nrofbytes; i+=16)
  {
    int j;
    unsigned char bt;
    sendstringf("%6 : ",StartAddress+i);
    for (j=0; j<16; j++)
    {
      bt=memory[i+j];
      sendstringf("%2 ",bt);
    }

    sendstring("  ");

    for (j=0; j<16; j++)
    {
      bt=memory[i+j];
      if ((bt<32) || (bt>0x80) )
        bt='.';
      sendstringf("%c",bt);
    }

    sendstring("\n\r");
  }
  unmapPhysicalMemory(memory, nrofbytes);
}

