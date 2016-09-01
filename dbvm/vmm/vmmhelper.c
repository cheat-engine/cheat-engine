#include "common.h"
#include "vmmhelper.h"
#include "main.h"
#include "neward.h"
#include "mm.h"

/*
#include "vmmemu.h"
*/
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

#ifndef DEBUG
#define sendstringf(s,x...)
#define sendstring(s)
#endif

extern unsigned long long vmmstart;
extern unsigned long long nextstack;
extern PPDPTE_PAE pagedirptrvirtual;


//cpu specific stuff, put inside structure and give each cpu one

volatile unsigned char *MSRBitmap;
volatile unsigned char *IOBitmap;

extern int vmxstartup;
extern int vmxstartup_end;

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
    return (vmread(vm_cr0_fakeread) & 1)==0;
  }
}


int IS64BITPAGING(pcpuinfo currentcpuinfo)
{
  if (isAMD)
  {
    return (currentcpuinfo->vmcb->EFER & (1<<10))==(1<<10);
  }
  else
    return ((vmread(vm_entry_controls) & IA32E_MODE_GUEST) != 0);
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
  RFLAGS rf;

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
	  case 2: return "Tripple fault";
	  case 3: return "INIT Signal";
	  case 4: return "Start-up IPI (SIPI)";
	  case 5: return "SMI interrupt";
	  case 6: return "Other SMI";
	  case 7: return "Interrupt window";
	  case 9: return "Task switch";
	  case 10: return "CPUID";
	  case 14: return "INVLPG";
	  case 18: return "VMCALL";
	  case 28: return "Controlregister access";
	  case vm_exit_io_access: return "IO Access";

	  case 31: return "RDMSR";
	  case 32: return "WRMSR";
	  case 33: return "Invalid guest state";
	  case 52: return "Preemption timer";
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
  VirtualMachineTSS_V8086->ESP0=(ULONG)VirtualToPhysical((UINT64)RealmodeRing0Stack+4096-16);
  VirtualMachineTSS_V8086->SS0=8; //32-bit code segment
  VirtualMachineTSS_V8086->Reserved2=0;
  VirtualMachineTSS_V8086->ESP1=0;
  VirtualMachineTSS_V8086->SS1=0;
  VirtualMachineTSS_V8086->Reserved3=0;
  VirtualMachineTSS_V8086->ESP2=0;
  VirtualMachineTSS_V8086->SS2=0;
  VirtualMachineTSS_V8086->Reserved4=0;
  VirtualMachineTSS_V8086->CR3=(ULONG)VirtualToPhysical((UINT64)nonpagedEmulationPagedir);
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

void StoreVirtualMachineState(pcpuinfo currentcpuinfo, VMRegisters *registers)
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


void sendvmstate(pcpuinfo currentcpuinfo, VMRegisters *registers)
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
    sendstringf("efer=%x\n\r",currentcpuinfo->efer);

    sendstringf("ia32e mode guest=%d\n",((vmread(vm_entry_controls) & IA32E_MODE_GUEST) != 0) );

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
    sendstringf("cr0=%6 cr3=%6 cr4=%6\n\r",vmread(vm_cr0_fakeread), currentcpuinfo->guestCR3, vmread(vm_cr4_fakeread));
  }

#endif

}

//int autocont=8;
int twister=0;
//int guestwantstoknow=0;
int showall=0;
int rotations=0;
int cpu2=0; //debug to stop cpu1 when cpu2 is spawned

int vmeventcount=0;
criticalSection vmexitlock;




int vmexit_amd(pcpuinfo currentcpuinfo, UINT64 *registers)
{
 // displayline("vmexit_amd called. currentcpuinfo=%p\n", currentcpuinfo);
 // displayline("cpunr=%d\n", currentcpuinfo->cpunr);
  int result=0;

  nosendchar[getAPICID()]=1;


#ifdef DEBUG
  csEnter(&vmexitlock);


  sendstringf("vmexit_amd for cpu %d\n", currentcpuinfo->cpunr);

#endif
  result=handleVMEvent_amd(currentcpuinfo, (VMRegisters*)registers);

#ifdef DEBUG
  csLeave(&vmexitlock);
#endif



  return result;
}

#ifdef DEBUG

QWORD lastbeat=0;

int vmexit2(pcpuinfo currentcpuinfo, UINT64 *registers);

int vmexit(pcpuinfo currentcpuinfo, UINT64 *registers)
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


  result=vmexit2(currentcpuinfo, registers);


  vmstates[used_vmstates_pos].exit_cs=vmread(vm_guest_cs);
  vmstates[used_vmstates_pos].exit_cs_base=vmread(vm_guest_cs_base);
  vmstates[used_vmstates_pos].exit_ss=vmread(vm_guest_ss);
  vmstates[used_vmstates_pos].exit_ss_base=vmread(vm_guest_ss_base);

  vmstates[used_vmstates_pos].exit_rip=vmread(vm_guest_rip);
  vmstates[used_vmstates_pos].exit_rsp=vmread(vm_guest_rsp);
  vmstates[used_vmstates_pos].exit_rflags=vmread(vm_guest_rflags);

  if (result)
  {
    nosendchar[getAPICID()]=0;
    sendvmstate(currentcpuinfo, (VMRegisters*)registers);
  }


  csLeave(&vmexitlock);

  return result;
}

int vmexit2(pcpuinfo currentcpuinfo, UINT64 *registers)
#else
int vmexit(pcpuinfo currentcpuinfo, UINT64 *registers)
#endif
{


  if (currentcpuinfo==NULL)
  {
    nosendchar[getAPICID()]=0;
    sendstringf("currentcpuinfo==NULL");

    while (1);

  }


 // nosendchar[getAPICID()]=1;
  //return handleVMEvent(currentcpuinfo, (VMRegisters*)registers);


  int skip=0;
  vmeventcount++;
/*
  //check if it's a (dos)timer event
  if ((vmread(vm_exit_reason)==0) && (vmread(vm_exit_interruptioninfo)==0x80000b0d) && (vmread(vm_idtvector_information)==0x80000008))
  {
    return handleVMEvent(currentcpuinfo, (VMRegisters*)registers);
  }

  //check if it's a pre-emptiontimer event
  if (vmread(vm_exit_reason)==vm_exit_vmx_preemptiontimer_reachedzero)
  {
    return handleVMEvent(currentcpuinfo, (VMRegisters*)registers);
  }
*/

  int result;

#ifndef DEBUG

//  currentdisplayline=currentcpuinfo->cpunr+1;
//  currentdisplayrow=0;
//  displayline("%d: %d:%x (%x,%x)                              \n",currentcpuinfo->cpunr,vmeventcount,vmread(vm_exit_reason),vmread(vm_guest_cs),vmread(vm_guest_rip));

  nosendchar[getAPICID()]=1;
  result=handleVMEvent(currentcpuinfo, (VMRegisters*)registers);

  if (result!=0) //on release, if an unexpected event happens, just fail the instruction and hope the OS won't make a too big mess out of it
    return raiseInvalidOpcodeException(currentcpuinfo);
  else
    return result;

#else
  //nosendchar[getAPICID()]=0;
  //sendstringf("%x:%x\n",vmread(vm_guest_cs),vmread(vm_guest_rip));
  //nosendchar[getAPICID()]=1;
#endif



  UINT64 initialcount;


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








  enableserial();
  if (getchar()=='b')
    userbreak=1;




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



  if (userbreak)
  {
    sendstring("user wants to break\n\r");

  }
  else
  if ((vmread(0x4402)==0) && ((vmread(vm_exit_interruptioninfo) & 0x8000000f)==0x80000001) )
  {
	  //int1 bp
	  sendstringf("Int 1 bp");

  }
  else
  if (!showall)
  {

    switch (vmread(vm_exit_reason))
    {

      case vm_exit_vmcall:
    	  skip=1;
        sendstring("VMCALL\n");
    	break;


      case vm_exit_cpuid:
        //skip=1;
        break;


      case vm_exit_vmx_preemptiontimer_reachedzero:
        break;

      case vm_exit_invlpg:
        skip=1;
        break;

      case vm_exit_io_access:
        skip=1;
        break;



      case vm_exit_interrupt: //interrupt
      {
        switch (vmread(vm_exit_interruptioninfo))
        {
          case 0x80000300: //div by 0
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
        	if (vmread(vmread(vm_idtvector_information))==0)
        	  skip=1;

            break;

          case 0x80000306:
            skip=1;

            break;

          case 0x80000b0d: //GPF
            //check if it's a CS:RIP that we know and can skip (for dosmode)
            if (!skip)
            {
              int cs=vmread(vm_guest_cs);
              unsigned long long rip=vmread(vm_guest_rip);


              if (vmread(vmread(vm_idtvector_information))==0)
            	  skip=1; //'normal' gpf
              else
              switch (cs)
              {
                case 0x0:
                  switch (rip)
                  {
                    case 0xbffc:
                    case 0xc00c:
                    case 0xcee8:


                      skip=1;
                      break;
                  }
                  break;

                case 0x10:
                  switch (rip)
                  {
                    case 0xfffff80002a8e8fd:
                    case 0xfffff80002aec2bd:
                    case 0xfffff80002b34e78:
                    	skip=1;
                    	break;

                  }
                  break;


                  case 0x200:
                  switch (rip)
                  {
                    case 0x64f:
                    case 0x65f:
                    skip=1;
                    break;
                  }
                  break;

                  case 0x1000:
                  switch (rip)
                  {
                    case 0x11b:
                    case 0x16d:
                    case 0x1ad:
                    case 0x282c:
                    case 0x6e0b:

                    skip=1;
                    break;
                  }
                  break;

                case 0x2000:
                  //nosendchar[getAPICID()]=0;
                  //sendstring("cs: 0x2000\n");
                  switch (rip)
                  {

                    case 0xcc:
                    case 0x125:
                    case 0x190: //must execute gpf
                    case 0x826:
                    case 0x836:
                    case 0x839:
                    case 0x8cf:
                    case 0xa23:
                    case 0x507:
                    case 0xd79:



                      skip=1;
                      break;


                  }
                  break;

		case 0x3000:
		  switch (rip)
                  {
			case 0x2b1:
			  skip=1;
			  break;

		  }

		  break;

                case 0xc000:
                  switch (rip)
                  {
                    case 0xafe:

                      skip=1;
                      break;
                  }
                  break;

                case 0xea0e:
                  switch (rip)
                  {
                    case 0x0d35:
                    case 0x0d6c:
                    case 0x1944:
                    case 0x2216:
                    case 0x2e60:
                    case 0x300d:
                      skip=1;
                      break;

                  }


                  break;



                case 0xf000:
                  switch (rip)
                  {
                    case 0x3256:
                    case 0x361e:
		    case 0xd352:

                      skip=1;
                      break;
                  }
                  break;
              }
            }


            break;
        }
        break;
      }




      case vm_exit_cr_access:
      {
        int cs=vmread(vm_guest_cs);
        unsigned long long rip=vmread(vm_guest_rip);

        skip=1;

        switch (cs)
        {
          case 0x10:
          {
            switch (rip)
            {
              case 0x000000:


              {
                skip=1;
                break;
              }
            }
            break;
          }

            case 0x18:
            {
              switch (rip)
              {
               // case 0x87f6:
                case 0x000000:

                {
                  skip=1;
                  break;
                }
              }
              break;
            }


            case 0x20:
            {
              switch (rip)
              {
                case 0x00000:

                {
                   skip=1;
                   break;
                }

              }
              break;
            }

            case 0x58:
            {
              switch (rip)
              {
               case 0x3b9:
               {
                  skip=1;
                  break;
               }

              }
              break;
            }
        }
        break;
      }


      case vm_exit_sipi:
      {
        int cs=vmread(vm_guest_cs);
        unsigned long long rip=vmread(vm_guest_rip);

        switch (cs)
        {
          case 0x2000:
            switch (rip)
            {
              case 0x2fe:
                //skip=1;
                break;

            }
            break;
        }

        break;
      }

      //case vm_exit_taskswitch:
      //case vm_exit_cpuid:
      //case vm_exit_invlpg:


      case vm_exit_rdmsr:
      {
    	VMRegisters* r=(VMRegisters*)registers;
        switch (r->rcx)
        {
          case 0xc0000080:
        	  skip=1;
        	  break;

        }
        break;

      }

      case vm_exit_wrmsr:
      {
      	VMRegisters* r=(VMRegisters*)registers;
        switch (r->rcx)
        {
          case 0xc0000080:
            skip=1;
          break;

        }

        break;
      }



      case vm_exit_invalid_guest_state:
      {
        int cs=vmread(vm_guest_cs);
        unsigned long long rip=vmread(vm_guest_rip);



        switch (cs)
        {
          case 0x0:
          {

            switch (rip)
            {

            case 0xc060:
              skip=1;
              break;
            }

            break;
          }

          case 0x18:
          {

            switch (rip)
            {
              case 0xba:
            case 0xc04e:
              skip=1;
              break;
            }

            break;
          }

          case 0x20:
          {

            switch (rip)
            {

              case 0x6f:
              case 0x7c:
                skip=1;
                break;
            }

            break;
          }

          case 0x30:
          {

            switch (rip)
            {

              case 0x63a:
              case 0x645:
                skip=1;
                break;
            }

            break;
          }

          case 0x50:
          {

            switch (rip)
            {

              case 0x8a0:
              case 0x8af:

                skip=1;
                break;
            }

            break;
          }

          case 0x58:
          {

            switch (rip)
            {

              case 0x3bc:

                skip=1;
                break;
            }

            break;
          }

          case 0x2000:
          {

            switch (rip)
            {
              case 0x255:
              case 0x262:
              case 0x5a6:
              case 0x839:
              case 0x95a:
              case 0xb2e:
              case 0xc16:
              case 0xd4e:
              case 0xebe:
                skip=1;
                break;
            }

            break;
          }

          case 0x3000:
          {

            switch (rip)
            {
              case 0x661:
	      case 0x677:
                skip=1;
                break;
            }

            break;
          }


          case 0xea0e:
          {

            switch (rip)
            {
              case 0x1836:
              case 0x226a:
                skip=1;
                break;
            }

            break;
          }

          case 0xf000:
          {

            switch (rip)
            {
              case 0x4e35:
              case 0x9653:
              case 0x9ea7:
	      case 0xd2fd:
                skip=1;
                break;
            }

            break;
          }

        }

        break;
      }



    }

    if (skip)
    {
      nosendchar[getAPICID()]=1;

      result=handleVMEvent(currentcpuinfo, (VMRegisters*)registers);


      if (result==0)
      {
        if (debugmode)
          setTrap();

        return 0;
      }

      nosendchar[getAPICID()]=0;
    }

  }

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
  sendstringf("Hello from vmexit-(cpunr=%d) \n\r",currentcpuinfo->cpunr);


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
  sendstringf("Guest linear address=%x\n\r",vmread(0x640a));

  sendstringf("csbase=%6\n",vmread(vm_guest_cs_base));
  sendstringf("rip=%6\n",vmread(vm_guest_rip));

  {
    int notpaged=0;
    UINT64 ripaddress=vmread(vm_guest_cs_base)+vmread(vm_guest_rip);
    sendstringf("ripaddress=%x\n", ripaddress);

    sendstringf("Rip=%6", vmread(vm_guest_cs_base)+vmread(vm_guest_rip));

    if (notpaged)
       sendstring("(physical = invalid)\n\r");
    else
      sendstringf("(physical=%6)\n\r", ripaddress);

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
    sendstringf("|   4: show all (%d)                     |\n\r", showall);
    sendstring("|   5: show instruction memory          |\n\r");
    sendstringf("|   6: toggle debugmode (%d)             |\n\r",debugmode);
    sendstring("|   7: set breakpoint                   |\n\r");
    sendstring("|   8: display physical memory          |\n\r");
    sendstring("|   9: quit virtual machine             |\n\r");
  //sendstring("|   p: previous event                   |\n\r");
    sendstring("\\---------------------------------------/\n\r");
    sendstring("Your command:");
    command=waitforchar();
    sendstring("\n\r");

    switch (command)
    {
      case  '1' :
        result=handleVMEvent(currentcpuinfo, (VMRegisters*)registers);

        //setResumeFlag();
        sendstringf("Returned from handleVMEvent. result=%d (CR0=%x)\n\r",result,vmread(vm_guest_cr0));

        if (currentcpuinfo->cpunr==1)
          sendvmstate(currentcpuinfo, (VMRegisters*)registers);


        if (result!=0)
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
        if (showall)
          showall=0;
        else
          showall=1;

        //showall=!showall;
        break;


      case  '5' :
      {
        sendstring("Going to show the instruction:\n\r");
        ShowCurrentInstructions(currentcpuinfo);


        break;
      }


      case  '6' :
        debugmode=!debugmode;
        break;

      case  '7' :
      {
        char temps[17];
        UINT64 BreakAddress;

        regDR7 dr7;
        dr7.DR7=vmread(0x681a);

        sendstring("Startaddress:");
        readstring(temps,16,16);
        sendstring("\n\r");
        BreakAddress=atoi(temps,16,NULL);
        setDR0(BreakAddress);

        //set DR7 of the guest to break on execution of DR0
        dr7.L0=1; //activate
        dr7.G0=1;
        dr7.RW0=0; //break on execution
        dr7.LEN0=0; //1 byte, used for exec breakpoints
        vmwrite(0x681a,(UINT64)dr7.DR7); //dr7

        breakpointset=1;
        break;
      }

      case  'W' :
      {
        char temps[17];
        UINT64 BreakAddress;

        regDR7 dr7;
        dr7.DR7=vmread(0x681a);

        sendstring("Startaddress:");
        readstring(temps,16,16);
        sendstring("\n\r");
        BreakAddress=atoi(temps,16,NULL);
        setDR0(BreakAddress);

        //set DR7 of the guest to break on execution of DR0
        dr7.L0=1; //activate
        dr7.G0=1;
        dr7.RW0=1; //break on write
        dr7.LEN0=0; //1 byte, used for exec breakpoints
        vmwrite(0x681a,(UINT64)dr7.DR7); //dr7

        breakpointset=1;
        break;
      }

      case  '8' :
      {
        displayPhysicalMemory();
        break;

      }

  		case  '9' :
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
        bits=atoi(temps,10,&err1);

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
        address=atoi(temps,16,&err2);


        sendstring("\n\rNumber of bytes:");
        readstring(temps,16,17);
        size=atoi(temps,10,&err3);

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

      case 'm' :
      {
        sendstringf("MSRBitmap: %p (%x)\n",MSRBitmap, VirtualToPhysical((UINT64)MSRBitmap));
        sendstringf("VM MSRBitmap=%x\n",vmread(0x2004));
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


      case  's' :
      {
        UINT64 address;
        unsigned char bt;
        char temps[17];

        sendstring("\n\rAddress:");
        readstring(temps,16,16);
        address=atoi(temps,16,NULL);

        sendstring("\n\rValue=");
        readstring(temps,2,16);
        bt=atoi(temps,16,NULL);

        *(unsigned char *)address=bt;
        sendstring("\n\r");
        break;
      }

      default:
        sendstring("Unknown command\n\r");
        break;
  	}
  }


  return 1;
}


criticalSection setupVMX_lock;

void setupVMX_AMD(pcpuinfo currentcpuinfo)
{
  //setup the vmcb
  Segment_Attribs reg_csaccessrights;
  Segment_Attribs reg_traccessrights;

  if (currentcpuinfo->virtualTLB == NULL) //just always do it
    allocateVirtualTLB();


  if (currentcpuinfo->cpunr!=0)
  {
    sendstringf("setupVMX_AMD for AP cpu\n");
  }


  currentcpuinfo->vmcb->InterceptVMRUN=1;
  currentcpuinfo->vmcb->GuestASID=1;
  currentcpuinfo->vmcb->EFER=0x1500 | (1<<8) | (1<<10);

  reg_traccessrights.SegmentAttrib=0;
  reg_traccessrights.Segment_type=11; //11=32-bit 3=16-bit
  reg_traccessrights.S=0;
  reg_traccessrights.DPL=0;
  reg_traccessrights.P=1;
  reg_traccessrights.G=0;
  reg_traccessrights.D_B=1;

  reg_csaccessrights.SegmentAttrib=0;
  reg_csaccessrights.Segment_type=11;
  reg_csaccessrights.S=1;
  reg_csaccessrights.DPL=0;
  reg_csaccessrights.P=1;
  reg_csaccessrights.L=1;
  reg_csaccessrights.G=0;
  reg_csaccessrights.D_B=0;

  currentcpuinfo->vmcb->CR4=getCR4();
  currentcpuinfo->vmcb->CR3=getCR3();
  currentcpuinfo->vmcb->CR0=getCR0();


  currentcpuinfo->guestCR3=getCR3();
  currentcpuinfo->guestCR0=getCR0();
  currentcpuinfo->hasIF=0;

  currentcpuinfo->vmcb->gdtr_base=getGDTbase();
  currentcpuinfo->vmcb->idtr_base=getIDTbase();

  currentcpuinfo->vmcb->gdtr_limit, 0x50;
  currentcpuinfo->vmcb->idtr_limit, 8*256;

  currentcpuinfo->vmcb->cs_selector=80;
  //currentcpuinfo->vmcb->cs_limit=0;//0xffffffff;
  //currentcpuinfo->vmcb->ss_limit=0;//0xffffffff;
  currentcpuinfo->vmcb->cs_attrib=(WORD)reg_csaccessrights.SegmentAttrib;

  currentcpuinfo->vmcb->ds_selector=8;
  currentcpuinfo->vmcb->es_selector=8;
  currentcpuinfo->vmcb->ss_selector=8;
  currentcpuinfo->vmcb->fs_selector=8;
  currentcpuinfo->vmcb->gs_selector=8;

  sendstringf("cs_attrib(%x)  set to %x\n", ((UINT64)&currentcpuinfo->vmcb->cs_attrib-(UINT64)currentcpuinfo->vmcb), currentcpuinfo->vmcb->cs_attrib);
  sendstringf("gdtr_limit(%x)  set to %x\n", ((UINT64)&currentcpuinfo->vmcb->gdtr_limit-(UINT64)currentcpuinfo->vmcb), currentcpuinfo->vmcb->gdtr_limit);




/*
  currentcpuinfo->vmcb->tr_limit=(UINT64)sizeof(TSS)+32+8192+1;
  currentcpuinfo->vmcb->tr_base=(UINT64)mainTSS;
  currentcpuinfo->vmcb->tr_attrib=(WORD)reg_traccessrights.SegmentAttrib;*/

  currentcpuinfo->vmcb->DR7=0x400;

  currentcpuinfo->vmcb->RSP=0x8fffc;
  currentcpuinfo->vmcb->RFLAGS=getRFLAGS();

  if (currentcpuinfo->cpunr==0)
    currentcpuinfo->vmcb->RIP=(UINT64)quickboot;
  else
    currentcpuinfo->vmcb->RIP=(UINT64)infloop;





  if (!loadedOS)
    currentcpuinfo->vmcb->InterceptINT=1; //break on software interrupts (int 0x15 in realmode to tell the os not to mess with stuff)

  currentcpuinfo->vmcb->InterceptShutdown=1; //in case of a severe error
  currentcpuinfo->vmcb->InterceptVMMCALL=1;
  currentcpuinfo->vmcb->MSR_PROT=1; //some msr's need to be protected

  //currentcpuinfo->vmcb->InterceptExceptions=(1<<3);// | (1<<14); //intercept int1, 3 and 14
 // currentcpuinfo->vmcb->InterceptDR0_15Write=(1<<6); //dr6 so I can see what changed



 // currentcpuinfo->vmcb->InterceptINIT=1; //cpu init (init-sipi-sipi. I need to implement a virtual apic to suppot boot




  if (MSRBitmap==NULL)
  {
    int i;
    //allocate and MSR bitmap
    MSRBitmap=malloc(2*4096);
    //fill with 1's (the msr's that have a 1 do not cause an intercept)

    //bochsbp();
    for (i=0; i<4096*2; i++)
      MSRBitmap[i]=0;

    //Must protect 0xc0010117 (MSRPM_BASE_PA)
    MSRBitmap[0x1000+(0x0117*2)/8]|=3 << ((0x0117*2) % 8);

    //also 0xc0000080 (EFER)
    //if (hideEFER)
    MSRBitmap[0x800+(0x80*2)/8]|=3 << ((0x80*2) % 8);
  }
  currentcpuinfo->vmcb->MSRPM_BASE_PA=VirtualToPhysical((UINT64)MSRBitmap);

  currentcpuinfo->guest_VM_HSAVE_PA=0;


  globals_have_been_configured=1;

  SaveExtraHostState(currentcpuinfo->vmcb_PA); //save some of MSR's that are needed (init did touch the segment registers so those need to be overridden if loadedOS)


  if (loadedOS)
  {
    POriginalState originalstate=NULL;
    PGDT_ENTRY gdt=NULL,ldt=NULL;
    ULONG ldtselector=originalstate->ldt;
    int notpaged;

    RFLAGS rflags;




    sendstringf("Setting up guest based on loadedOS settings\n");
    originalstate=(POriginalState)MapPhysicalMemory(loadedOS,currentcpuinfo->AvailableVirtualAddress);


    sendstringf("originalstate=%6\n", originalstate);
    sendstringf("originalstate->cpucount(%x)=%d\n",&originalstate->cpucount, originalstate->cpucount);
    sendstringf("originalstate->cr0=%6\n",originalstate->cr0);
    sendstringf("originalstate->cr2=%6\n",originalstate->cr2);
    sendstringf("originalstate->cr3=%6\n",originalstate->cr3);
    sendstringf("originalstate->cr4=%6\n",originalstate->cr4);
    sendstringf("originalstate->rip(%x)=%6\n",&originalstate->rip, originalstate->rip);
    sendstringf("originalstate->cs=%x\n",originalstate->cs);
    sendstringf("originalstate->ss=%x\n",originalstate->ss);
    sendstringf("originalstate->ds=%x\n",originalstate->ds);
    sendstringf("originalstate->es=%x\n",originalstate->es);
    sendstringf("originalstate->fs=%x\n",originalstate->fs);
    sendstringf("originalstate->gs=%x\n",originalstate->gs);
    sendstringf("originalstate->ldt=%x\n",originalstate->ldt);
    sendstringf("originalstate->tr=%x\n",originalstate->tr);

    sendstringf("originalstate->dr7(%x)=%6\n",&originalstate->dr7, originalstate->dr7);
    sendstringf("originalstate->gdtbase=%6\n",originalstate->gdtbase);
    sendstringf("originalstate->gdtlimit=%x\n",originalstate->gdtlimit);
    sendstringf("originalstate->idtbase=%6\n",originalstate->idtbase);
    sendstringf("originalstate->idtlimit=%x\n",originalstate->idtlimit);
    sendstringf("originalstate->originalLME=%x\n",originalstate->originalLME);
    sendstringf("originalstate->rflags=%6\n",originalstate->rflags);

    sendstringf("originalstate->rax=%6\n",originalstate->rax);
    sendstringf("originalstate->rbx=%6\n",originalstate->rbx);
    sendstringf("originalstate->rcx=%6\n",originalstate->rcx);
    sendstringf("originalstate->rdx=%6\n",originalstate->rdx);
    sendstringf("originalstate->rsi=%6\n",originalstate->rsi);
    sendstringf("originalstate->rdi=%6\n",originalstate->rdi);
    sendstringf("originalstate->rbp=%6\n",originalstate->rbp);
    sendstringf("originalstate->rsp=%6\n",originalstate->rsp);
    sendstringf("originalstate->r8=%6\n",originalstate->r8);
    sendstringf("originalstate->r9=%6\n",originalstate->r9);
    sendstringf("originalstate->r10=%6\n",originalstate->r10);
    sendstringf("originalstate->r11=%6\n",originalstate->r11);
    sendstringf("originalstate->r12=%6\n",originalstate->r12);
    sendstringf("originalstate->r13=%6\n",originalstate->r13);
    sendstringf("originalstate->r14=%6\n",originalstate->r14);
    sendstringf("originalstate->r15=%6\n",originalstate->r15);



    currentcpuinfo->vmcb->CR4=originalstate->cr4;
    currentcpuinfo->vmcb->CR3=originalstate->cr3;
    currentcpuinfo->vmcb->CR0=originalstate->cr0;
    currentcpuinfo->vmcb->RFLAGS=originalstate->rflags;

    currentcpuinfo->efer=originalstate->originalEFER;
    currentcpuinfo->vmcb->EFER=currentcpuinfo->efer | (1<<12);


    currentcpuinfo->vmcb->gdtr_base=(UINT64)originalstate->gdtbase;
    currentcpuinfo->vmcb->gdtr_limit=(UINT64)originalstate->gdtlimit;

    currentcpuinfo->vmcb->idtr_base=(UINT64)originalstate->idtbase;
    currentcpuinfo->vmcb->idtr_limit=(UINT64)originalstate->idtlimit;


    //gdt MUST be paged in
    gdt=(PGDT_ENTRY)(UINT64)MapPhysicalMemory(
          getPhysicalAddressVM(currentcpuinfo, originalstate->gdtbase, &notpaged)
          ,currentcpuinfo->AvailableVirtualAddress+0x00200000
        );

#ifdef DEBUG
    {
      int i,j;
      sendstring("GDT=\n");
      i=0;
      j=0;
      while (i<=originalstate->gdtlimit)
      {
        for (j=0; (j<8 && (i<=originalstate->gdtlimit)); j++)
        {
          sendstringf("%2", ((unsigned char *)gdt)[i]);
          i++;
        }

        sendstring("\n");

      }
    }
#endif

    if ((UINT64)originalstate->ldt)
    {
      UINT64 ldtbase; //should be 0 in 64bit
      ULONG ldtlimit;

      sendstring("ldt is valid, so getting the information\n\r");

      ldtbase=(gdt[(ldtselector >> 3)].Base24_31 << 24) + gdt[(ldtselector >> 3)].Base0_23;
      ldtlimit=(gdt[(ldtselector >> 3)].Limit16_19 << 16) + gdt[(ldtselector >> 3)].Limit0_15;
      ldt=(PGDT_ENTRY)(UINT64)MapPhysicalMemory(getPhysicalAddressVM(currentcpuinfo, ldtbase, &notpaged), currentcpuinfo->AvailableVirtualAddress+0x00400000);
    }

    currentcpuinfo->vmcb->es_selector=originalstate->es;
    currentcpuinfo->vmcb->cs_selector=originalstate->cs;
    currentcpuinfo->vmcb->ss_selector=originalstate->ss;
    currentcpuinfo->vmcb->ds_selector=originalstate->ds;
    currentcpuinfo->vmcb->fs_selector=originalstate->fs;
    currentcpuinfo->vmcb->gs_selector=originalstate->gs;
    currentcpuinfo->vmcb->ldtr_selector=originalstate->ldt;
  //  currentcpuinfo->vmcb->tr_selector=originalstate->tr;

    currentcpuinfo->vmcb->es_limit=getSegmentLimit(gdt, ldt, originalstate->es);
    currentcpuinfo->vmcb->cs_limit=getSegmentLimit(gdt, ldt, originalstate->cs);
    currentcpuinfo->vmcb->ss_limit=getSegmentLimit(gdt, ldt, originalstate->ss);
    currentcpuinfo->vmcb->ds_limit=getSegmentLimit(gdt, ldt, originalstate->ds);
    currentcpuinfo->vmcb->fs_limit=getSegmentLimit(gdt, ldt, originalstate->fs);
    currentcpuinfo->vmcb->gs_limit=getSegmentLimit(gdt, ldt, originalstate->gs);
    currentcpuinfo->vmcb->ldtr_limit=getSegmentLimit(gdt, ldt, originalstate->ldt);
  //  currentcpuinfo->vmcb->tr_limit=getSegmentLimit(gdt, ldt, originalstate->tr);

    currentcpuinfo->vmcb->es_base=getSegmentBase(gdt, ldt, originalstate->es);
    currentcpuinfo->vmcb->cs_base=getSegmentBase(gdt, ldt, originalstate->cs);
    currentcpuinfo->vmcb->ss_base=getSegmentBase(gdt, ldt, originalstate->ss);
    currentcpuinfo->vmcb->ds_base=getSegmentBase(gdt, ldt, originalstate->ds);
    if (originalstate->originalLME)
    {
      //64-bit
      currentcpuinfo->vmcb->fs_base=originalstate->fsbase;
      currentcpuinfo->vmcb->gs_base=originalstate->gsbase;
    //  currentcpuinfo->vmcb->tr_base=getSegmentBaseEx(gdt,ldt,originalstate->tr, 1);

      sendstringf("Getting base of RT. getSegmentBaseEx(gdt,ldt,originalstate->tr, 1); would say: %6\n", getSegmentBaseEx(gdt,ldt,originalstate->tr, 1));
    }
    else
    {
      //32-bit
      currentcpuinfo->vmcb->fs_base=getSegmentBase(gdt, ldt, originalstate->fs);
      currentcpuinfo->vmcb->gs_base=getSegmentBase(gdt, ldt, originalstate->gs);
     // currentcpuinfo->vmcb->tr_base=getSegmentBase(gdt,ldt,originalstate->tr);
    }
    currentcpuinfo->vmcb->ldtr_base=getSegmentBase(gdt, ldt, originalstate->ldt);


    currentcpuinfo->vmcb->es_attrib=getSegmentAttrib(gdt,ldt,originalstate->es);
    currentcpuinfo->vmcb->cs_attrib=getSegmentAttrib(gdt,ldt,originalstate->cs);
    currentcpuinfo->vmcb->ss_attrib=getSegmentAttrib(gdt,ldt,originalstate->ss);
    currentcpuinfo->vmcb->ds_attrib=getSegmentAttrib(gdt,ldt,originalstate->ds);
    currentcpuinfo->vmcb->fs_attrib=getSegmentAttrib(gdt,ldt,originalstate->fs);
    currentcpuinfo->vmcb->gs_attrib=getSegmentAttrib(gdt,ldt,originalstate->gs);
    currentcpuinfo->vmcb->ldtr_attrib=getSegmentAttrib(gdt,ldt,originalstate->ldt);

   // currentcpuinfo->vmcb->tr_attrib=getSegmentAttrib(gdt,ldt,originalstate->tr);

    sendstringf("Getting attibs of RT. getSegmentAttrib(gdt,ldt,originalstate->tr) would say: %x\n", getSegmentAttrib(gdt,ldt,originalstate->tr));


    currentcpuinfo->vmcb->SYSENTER_CS=(UINT64)readMSR(IA32_SYSENTER_CS_MSR); //current msr
    currentcpuinfo->vmcb->SYSENTER_ESP=(UINT64)readMSR(IA32_SYSENTER_ESP_MSR);
    currentcpuinfo->vmcb->SYSENTER_EIP=(UINT64)readMSR(IA32_SYSENTER_EIP_MSR);

    currentcpuinfo->actual_sysenter_CS=currentcpuinfo->vmcb->SYSENTER_CS;
    currentcpuinfo->actual_sysenter_ESP=currentcpuinfo->vmcb->SYSENTER_ESP;
    currentcpuinfo->actual_sysenter_EIP=currentcpuinfo->vmcb->SYSENTER_EIP;



    currentcpuinfo->vmcb->DR7=originalstate->dr7;



    if (originalstate->originalLME)
    {
      //64-bit
      currentcpuinfo->vmcb->RSP=originalstate->rsp;
      currentcpuinfo->vmcb->RAX=originalstate->rax;
      currentcpuinfo->vmcb->RIP=originalstate->rip;

    }
    else
    {
      //32-bit (make sure unused bits are 0)
      currentcpuinfo->vmcb->RSP=(ULONG)originalstate->rsp;
      currentcpuinfo->vmcb->RAX=(ULONG)originalstate->rax;
      currentcpuinfo->vmcb->RIP=(ULONG)originalstate->rip;
    }
  }

  currentcpuinfo->vmxsetup=1;

  csLeave(&setupVMX_lock);


}

void setupVMX(pcpuinfo currentcpuinfo)
{

  Access_Rights reg_csaccessrights,reg_traccessrights;

  csEnter(&setupVMX_lock);

  currentcpuinfo->AvailableVirtualAddress=(UINT64)(currentcpuinfo->cpunr+1) << 28;
//  currentcpuinfo->AvailableVirtualAddress=(UINT64)(currentcpuinfo->cpunr+16) << 28;
  sendstringf("AvailableVirtualAddress=%6\n\r",currentcpuinfo->AvailableVirtualAddress);

  if (!loadedOS)
  {
    //copy the vmxloader to address 0x2000 (not needed for loadedOS since it won't need a boot setup)
    if (globals_have_been_configured==0)
    {
      //one time init, only one cpu has to do this
      zeromemory((void *)0x20000,0x1000);
      zeromemory((void *)0x50000,0x1000);
      zeromemory((void *)0x60000,0x1000);

      sendstringf("Copying gdt to low memory. gdtbase=%x\n\r",getGDTbase());
      copymem((void *)0x50000,(void *)(UINT64)GDT_IDT_BASE,0x50); //copy gdt to 0x50000

      sendstringf("copying movetoreal to 0x20000 (size=%d bytes)\n\r",(UINT64)&vmxstartup_end-(UINT64)&movetoreal);
      copymem((void *)0x20000,(void *)(UINT64)&movetoreal,(UINT64)&vmxstartup_end-(UINT64)&movetoreal);
    }
  }

  if (isAMD)
    return setupVMX_AMD(currentcpuinfo);


  if (MSRBitmap==NULL)
  {
    //setp the MSR bitmap (I'd like to know when a 64<->32 bit switch happens)
    MSRBitmap=malloc(4096);
    zeromemory(MSRBitmap,4096);

    //MSRBitmap layout:
    //0000-1023: Read bitmap for low MSRs
    //1024-2047: Write bitmap for low MSRs
    //2048-3071: Read bitmap for high MSRs
    //3072-4095: Write bitmap for high MSRs


   // MSRBitmap[0x17a/8]|=1 << (0x17a % 8);


    //set it to break on msr's handling sysenter
    //read for sysenter
    MSRBitmap[0x174/8]|=1 << (0x174 % 8);
    MSRBitmap[0x175/8]|=1 << (0x175 % 8);
    MSRBitmap[0x176/8]|=1 << (0x176 % 8);


    //write for sysenter
    MSRBitmap[2048+0x174/8]|=1 << (0x174 % 8);
    MSRBitmap[2048+0x175/8]|=1 << (0x175 % 8);
    MSRBitmap[2048+0x176/8]|=1 << (0x176 % 8);


    //read for 0xc0000080  (EFER)
    MSRBitmap[1024+0x80/8]=1 << (0x80 % 8);

    //write for 0xc0000080
    MSRBitmap[3072+0x80/8]=1 << (0x80 % 8);


    //break on IA32_FEATURE_CONTROL_MSR read and write
    //read
    MSRBitmap[IA32_FEATURE_CONTROL_MSR/8]|=1 << (IA32_FEATURE_CONTROL_MSR % 8);

    //write
    MSRBitmap[IA32_FEATURE_CONTROL_MSR/8]|=1 << (IA32_FEATURE_CONTROL_MSR % 8);





    /*
    //break on writes to mttr msr's
    for (i=0x200; i<0x20f; i++)
       MSRBitmap[1024+i/8]=1 << (i % 8);

    MSRBitmap[1024+0x250/8]=1 << (0x250 % 8);
    MSRBitmap[1024+0x258/8]=1 << (0x258 % 8);
    MSRBitmap[1024+0x259/8]=1 << (0x259 % 8);

    for (i=0x268; i<0x26f; i++)
       MSRBitmap[1024+i/8]=1 << (i % 8);

    MSRBitmap[1024+0x277/8]=1 << (0x277 % 8);
    MSRBitmap[1024+0x2ff/8]=1 << (0x2ff % 8);
    */
  }


  if (IOBitmap==NULL)
  {
    //configure IOBitmap
    sendstring("Allocating IOBitmap\n\r");
    IOBitmap=malloc(4096*2);
    zeromemory(IOBitmap,4096*2);

    #ifdef DEBUG
      #if (defined SERIALPORT) && (SERIALPORT != 0)

    //IOBitmap[0x7f]=0xff;
    //IOBitmap[22]=0xff;
    //IOBitmap[29]=0xff;
    /*
    IOBitmap[SERIALPORT / 8]|= 1 << (SERIALPORT % 8);
    IOBitmap[(SERIALPORT+1) / 8]|= 1 << ((SERIALPORT+1) % 8);
    IOBitmap[(SERIALPORT+2) / 8]|= 1 << ((SERIALPORT+2) % 8);
    IOBitmap[(SERIALPORT+3) / 8]|= 1 << ((SERIALPORT+3) % 8);
    IOBitmap[(SERIALPORT+4) / 8]|= 1 << ((SERIALPORT+4) % 8);
    IOBitmap[(SERIALPORT+5) / 8]|= 1 << ((SERIALPORT+5) % 8);
    IOBitmap[(SERIALPORT+6) / 8]|= 1 << ((SERIALPORT+6) % 8);
    IOBitmap[(SERIALPORT+7) / 8]|= 1 << ((SERIALPORT+7) % 8);
    */
      #endif
    #endif
  }

  sendstring("Setting up realmode paging\n"); //also for loadedOS in case of some weird event
  setupRealModePaging(currentcpuinfo);  //no more mallocs after this


  if (globals_have_been_configured==0)
  {
    sendstringf("before setupTSS8086. rsp=%6\n\r",getRSP());
    setupTSS8086();
    sendstringf("after setupTSS8086. rsp=%6\n\r",getRSP());

    sendstringf("Before configuring global VMX capability vars (%6)\n\r",getRSP());
    IA32_VMX_BASIC.IA32_VMX_BASIC=readMSR(0x480);
    IA32_VMX_PINBASED_CTLS=readMSR(IA32_VMX_PINBASED_CTLS_MSR);
    IA32_VMX_PROCBASED_CTLS=readMSR(0x482);
    if (IA32_VMX_PROCBASED_CTLS >> 63)
    {
      sendstringf("Has secondary procbased_ctls\n");
      IA32_VMX_SECONDARY_PROCBASED_CTLS=readMSR(0x48b);
    }
    else
    {
      sendstringf("Doesn't have secondary procbased_ctls\n");
    }

    IA32_VMX_EXIT_CTLS=readMSR(0x483);
    IA32_VMX_ENTRY_CTLS=readMSR(0x484);
    IA32_VMX_MISC.IA32_VMX_MISC=readMSR(0x485);
    sendstringf("After configuring global VMX capability vars (%6)\n\r",getRSP());


#ifdef DEBUG
    //setup fake comport:
    setCursorPos(0,1);
    zeromemory(&fakecom1,sizeof(fakecom1));
    fakecom1.Interrupt_Identification_Register=1; //no interrupt pending
#endif
  }

  sendstringf("after \"if (globals_have_been_configured==0)\" rsp=%6\n\r",getRSP());



  if (currentcpuinfo->cpunr==0)
  {
    displayline("IA32_VMX_BASIC=%6\n\r",IA32_VMX_BASIC.IA32_VMX_BASIC);
    displayline("IA32_VMX_PINBASED_CTLS=%6\n\r",IA32_VMX_PINBASED_CTLS);
    displayline("IA32_VMX_PROCBASED_CTLS=%6\n\r",IA32_VMX_PROCBASED_CTLS);
    displayline("IA32_VMX_SECONDARY_PROCBASED_CTLS=%6\n\r",IA32_VMX_SECONDARY_PROCBASED_CTLS);
    displayline("IA32_VMX_EXIT_CTLS=%6\n\r",IA32_VMX_EXIT_CTLS);
    displayline("IA32_VMX_ENTRY_CTLS=%6\n\r",IA32_VMX_ENTRY_CTLS);
    displayline("IA32_VMX_MISC=%6\n\r",IA32_VMX_MISC.IA32_VMX_MISC);


  }

  //
  /*
  core i7 values:
  IA32_VMX_BASIC=00da04000000000e
  IA32_VMX_PINBASED_CTLS=0000007f-00000016
  min=0010110
  max=1111111

  old
  min=0010110
  max=0011111

  0=external-interrupt exiting
  3=NMI Exiting
  *5=Virtual NMIs
  *6=Active vmx preemption timer


  IA32_VMX_PROCBASED_CTLS=fff9fffe-0401e172
  min=00000100000000011110000101110010
  max=11111111111110011111111111111110

  old

  min=00000100000000011110000101110010
  max=01110111101110011111111111111110

  2=Interrupt window exiting
  3=use tsc offsetting
  7=hlt exiting
  9=invlpg exiting
  10=mwait exiting
  11=rdpmc exiting
  12=rdtsc exiting
  19=cr8-load exiting
  20=cr8-store exiting
  21=use tpr shadowing
  *22=NMI window exiting
  23=MOV-DR exiting
  24=unconditional i/o exiting
  25=use i/o bitmaps
  *27=Monitor trap flag debugging
  28=use msr bitmaps
  29=monitor exiting
  30=pause exiting
  *31=Active secondary controls



  IA32_VMX_SECONDARY_PROCBASED_CTLS=0000007f-00000000
  i7:
  min=00000000000000000000000000000000
  max=00000000000000000000000001111111

  old:
  min=00000000000000000000000000000000
  max=00000000000000000000000000000000

  0*=Virtualize APIC access
  1*=Enable EPT
  2*=Descriptor table exits
  3*=Enable rdtsc
  4*=Virtualize 2xAPIC
  5*=Enable VPID
  6*=WBINVD exiting

  IA32_VMX_EXIT_CTLS=007fffff-00036dff
  min=00000110110110111111111
  max=11111111111111111111111

  old
  min=00000110110110111111111
  max=00000111110111111111111

  9=host address space size
  12*=Load IA32_PERF_GLOBAL
  15=Acknowledge interrupt on exit
  18*=Save PAT
  19*=Load PAT
  20*=Save efer
  21*=load efer
  22*=save vmx timer value


  IA32_VMX_ENTRY_CTLS=0000ffff-000011ff
  min=0001000111111111
  max=1111111111111111

  old
  min=0001000111111111
  max=0001111111111111

  9=IA-32e mode guest
  10=entry to smm
  11=deactivate dual-monitor treatment
  13*=Load perf_global
  14*=load pat
  15*=load efer

  IA32_VMX_MISC=00000000000401c5 new
  IA32_VMX_MISC=00000000000403c0 old

  new: 100 0000000 111 000101
  old: 100 0000001 111 000000
  :support hlt
  :support shutdown
  :support wait-for-sipi
  max cr3=4

  */
  //

  //compatibility mode with newer cpus that have 0 settings for features that I expect are 1
  if ((IA32_VMX_PINBASED_CTLS >> 32) & (1<<1))
    IA32_VMX_PINBASED_CTLS|=(1<<1);

  if ((IA32_VMX_PINBASED_CTLS >> 32) & (1<<2))
    IA32_VMX_PINBASED_CTLS|=(1<<2);

  if ((IA32_VMX_PINBASED_CTLS >> 32) & (1<<4))
    IA32_VMX_PINBASED_CTLS|=(1<<4);

  if ((IA32_VMX_PROCBASED_CTLS >> 32) & (1<<1))
    IA32_VMX_PROCBASED_CTLS|=(1<<1);
  else
    sendstring("Fail1\n");

  if (((IA32_VMX_PROCBASED_CTLS >> 32) & (7<<4)) ==(7<<4))
      IA32_VMX_PROCBASED_CTLS|=(7<<4);
    else
      sendstring("Fail2\n");

  if ((IA32_VMX_PROCBASED_CTLS >> 32) & (1<<8))
    IA32_VMX_PROCBASED_CTLS|=(1<<8);
  else
    sendstring("Fail3\n");

  if (((IA32_VMX_PROCBASED_CTLS >> 32) & (15<<13)) ==(15<<13))
      IA32_VMX_PROCBASED_CTLS|=(15<<13);
  else
    sendstring("Fail4\n");

  if ((IA32_VMX_PROCBASED_CTLS >> 32) & (1<<26))
    IA32_VMX_PROCBASED_CTLS|=(1<<26);
  else
    sendstring("Fail5\n");

  if ((IA32_VMX_PROCBASED_CTLS >> 32) & (1<<15))
      IA32_VMX_PROCBASED_CTLS|=(1<<15);
  else
    sendstring("CR3 load exiting fail\n");

  if ((IA32_VMX_PROCBASED_CTLS >> 32) & (1<<16))
    IA32_VMX_PROCBASED_CTLS|=(1<<16);
  else
    sendstring("CR3 store exiting fail\n");




 // IA32_VMX_PROCBASED_CTLS=IA32_VMX_PROCBASED_CTLS | (1<<1) | (7<<4) | (1<<8) | (15<<13) | (1<<26);

  //do a check for a secondary entry






  sendstringf("%d: Initializing vmcs region for launch\n\r",currentcpuinfo->cpunr);


  //32-bit control fields
  vmwrite(vm_execution_controls_pin,(ULONG)IA32_VMX_PINBASED_CTLS); //pin-based VM-execution controls

  sendstringf("Set vm_execution_controls_pin to %8 (became %8)\n", (ULONG)IA32_VMX_PINBASED_CTLS, (DWORD)vmread(vm_execution_controls_pin));


#ifdef DEBUG
/*

  //check if the system supports preemption, and if so, enable it
  {
    ULONG usablepinbasedBits=(IA32_VMX_PINBASED_CTLS >> 32);
    if (usablepinbasedBits & ACTIVATE_VMX_PREEMPTION_TIMER)
    {
      displayline("Preemption is possible\n");
      vmwrite(vm_execution_controls_pin,(ULONG)IA32_VMX_PINBASED_CTLS | ACTIVATE_VMX_PREEMPTION_TIMER);
      vmwrite(vm_preemption_timer_value,IA32_VMX_MISC.vmx_premption_timer_tsc_relation*100000);
    }

  }
*/

#endif




  vmwrite(0x4004,(UINT64)0xffff); //exception bitmap (0xffff=0-15 0xffffffff=0-31)
  vmwrite(0x4006,(UINT64)0); //page fault error-code mask
  vmwrite(0x4008,(UINT64)0); //page fault error-code match
  vmwrite(0x400a,(UINT64)1); //cr3-target count

  DWORD new_vm_exit_controls=(DWORD)IA32_VMX_EXIT_CTLS;

  sendstringf("IA32_VMX_EXIT_CTLS=%6\n", IA32_VMX_EXIT_CTLS);


  if ((IA32_VMX_EXIT_CTLS >> 32) & HOST_ADDRESS_SPACE_SIZE)
    new_vm_exit_controls|=HOST_ADDRESS_SPACE_SIZE;
  else
    sendstring("<<<<<<WARNING: This system does not support HOST_ADDRESS_SPACE_SIZE>>>>>>\n");



  if ((IA32_VMX_EXIT_CTLS >> 32) & ACKNOWLEDGE_INTERRUPT_ON_EXIT)
    new_vm_exit_controls|=ACKNOWLEDGE_INTERRUPT_ON_EXIT;
  else
    sendstring("<<<<<<WARNING: This system does not support acknowledge interrupt on exit>>>>>>\n");

  if ((IA32_VMX_EXIT_CTLS >> 32) & SAVE_DEBUG_CONTROLS)
    new_vm_exit_controls|=SAVE_DEBUG_CONTROLS;
  else
    sendstring("<<<<<<WARNING: This system does not support saving debug controls>>>>>>\n");

  vmwrite(vm_exit_controls, new_vm_exit_controls); //vm-exit controls , Host address-space size = 1

  sendstringf("Set vm_exit_controls to %8 (became %8)\n", new_vm_exit_controls, (DWORD)vmread(vm_exit_controls));


  vmwrite(0x400e,(UINT64)0); //vm-exit msr-store count
  vmwrite(0x4010,(UINT64)0); //vm-exit msr-load count


  vmwrite(0x4014,(UINT64)0); //vm-entry msr-load count
  vmwrite(0x4016,(UINT64)0); //vm-entry interruption-information field
  vmwrite(0x4018,(UINT64)0); //vm-entry exception error code
  vmwrite(0x401a,(UINT64)0); //vm-entry instruction length
  vmwrite(0x401c,(UINT64)0); //TPR threshold


  //64-bit control fields
  vmwrite(vm_iobitmap_a,(UINT64)VirtualToPhysical((UINT64)IOBitmap)); //IO Bitmap A
  vmwrite(vm_iobitmap_b,(UINT64)VirtualToPhysical((UINT64)&(IOBitmap[4096]))); //IO Bitmap B
  vmwrite(0x2004,(UINT64)VirtualToPhysical((UINT64)MSRBitmap)); //MSR bitmap
  vmwrite(0x2006,(UINT64)0xffffffffffffffff); //VM-Exit MSR store address(physical)
  vmwrite(0x2008,(UINT64)0xffffffffffffffff); //VM-Exit MSR-Load address
  vmwrite(0x200a,(UINT64)0xffffffffffffffff); //VM-Entry MSR-Load address
  vmwrite(0x200c,(UINT64)0xffffffffffffffff); //Executive-VMCS pointer
  vmwrite(0x2010,(UINT64)0); //TSC offset
  vmwrite(0x2012,(UINT64)0xffffffffffffffff); //Virtual-APIC page address



  //natural width control fields
  vmwrite(0x6000,(UINT64)0xffffffffffffffff); //cr0 guest/host mask
  vmwrite(0x6002,(UINT64)0xffffffffffffffff); //cr4 guest/host mask


  vmwrite(0x600a,(UINT64)0); //cr3-target value 1
  vmwrite(0x600c,(UINT64)0); //cr3-target value 2
  vmwrite(0x600e,(UINT64)0); //cr3-target value 3

  //----------------GUEST SETUP----------------
  if (loadedOS)
  {
    //set osstate, for both boot and app cpu's
    POriginalState originalstate=NULL;
    PGDT_ENTRY gdt=NULL,ldt=NULL;
    ULONG ldtselector=originalstate->ldt;
    int notpaged;
    RFLAGS rflags;

    sendstringf("Setting up guest based on loadedOS settings\n");
    originalstate=(POriginalState)MapPhysicalMemory(loadedOS,currentcpuinfo->AvailableVirtualAddress);

    sendstringf("originalstate->cpucount=%d\n",originalstate->cpucount);
    sendstringf("originalstate->cr0=%6\n",originalstate->cr0);
    sendstringf("originalstate->cr2=%6\n",originalstate->cr2);
    sendstringf("originalstate->cr3=%6\n",originalstate->cr3);
    sendstringf("originalstate->cr4=%6\n",originalstate->cr4);
    sendstringf("originalstate->rip=%6\n",originalstate->rip);
    sendstringf("originalstate->cs=%x\n",originalstate->cs);
    sendstringf("originalstate->ss=%x\n",originalstate->ss);
    sendstringf("originalstate->ds=%x\n",originalstate->ds);
    sendstringf("originalstate->es=%x\n",originalstate->es);
    sendstringf("originalstate->fs=%x\n",originalstate->fs);
    sendstringf("originalstate->gs=%x\n",originalstate->gs);
    sendstringf("originalstate->ldt=%x\n",originalstate->ldt);
    sendstringf("originalstate->tr=%x\n",originalstate->tr);

    sendstringf("originalstate->dr7=%6\n",originalstate->dr7);
    sendstringf("originalstate->gdtbase=%6\n",originalstate->gdtbase);
    sendstringf("originalstate->gdtlimit=%x\n",originalstate->gdtlimit);
    sendstringf("originalstate->idtbase=%6\n",originalstate->idtbase);
    sendstringf("originalstate->idtlimit=%x\n",originalstate->idtlimit);
    sendstringf("originalstate->originalLME=%x\n",originalstate->originalLME);
    sendstringf("originalstate->rflags=%6\n",originalstate->rflags);

    sendstringf("originalstate->rax=%6\n",originalstate->rax);
    sendstringf("originalstate->rbx=%6\n",originalstate->rbx);
    sendstringf("originalstate->rcx=%6\n",originalstate->rcx);
    sendstringf("originalstate->rdx=%6\n",originalstate->rdx);
    sendstringf("originalstate->rsi=%6\n",originalstate->rsi);
    sendstringf("originalstate->rdi=%6\n",originalstate->rdi);
    sendstringf("originalstate->rbp=%6\n",originalstate->rbp);
    sendstringf("originalstate->rsp=%6\n",originalstate->rsp);
    sendstringf("originalstate->r8=%6\n",originalstate->r8);
    sendstringf("originalstate->r9=%6\n",originalstate->r9);
    sendstringf("originalstate->r10=%6\n",originalstate->r10);
    sendstringf("originalstate->r11=%6\n",originalstate->r11);
    sendstringf("originalstate->r12=%6\n",originalstate->r12);
    sendstringf("originalstate->r13=%6\n",originalstate->r13);
    sendstringf("originalstate->r14=%6\n",originalstate->r14);
    sendstringf("originalstate->r15=%6\n",originalstate->r15);




    currentcpuinfo->guestCR3=originalstate->cr3;
    currentcpuinfo->guestCR0=originalstate->cr0;

    rflags.value=originalstate->rflags;
    currentcpuinfo->hasIF=rflags.IF;




    DWORD new_vm_execution_controls_cpu=(DWORD)IA32_VMX_PROCBASED_CTLS | INVLPG_EXITING | USE_IO_BITMAPS | USE_MSR_BITMAPS;

    if ((IA32_VMX_PROCBASED_CTLS >> 32) & (1<<31)) //secondary procbased ctl support
      new_vm_execution_controls_cpu=new_vm_execution_controls_cpu | (1<<31);

    vmwrite(vm_execution_controls_cpu, new_vm_execution_controls_cpu); //processor-based vm-execution controls
    sendstringf("Set vm_execution_controls_cpu to %8 (became %8)\n", new_vm_execution_controls_cpu, (DWORD)vmread(vm_execution_controls_cpu));


    if ((new_vm_execution_controls_cpu >> 31) & 1)
    {
      //it has a secondary entry
      //enable rdtscp
      QWORD secondarycpu=0;


      if ((IA32_VMX_SECONDARY_PROCBASED_CTLS >> 32) & SPBEF_ENABLE_RDTSCP) //can it enable rdtscp ?
      {
        sendstringf("Enabling rdtscp\n");
        secondarycpu|=SPBEF_ENABLE_RDTSCP;
      }



      if ((IA32_VMX_SECONDARY_PROCBASED_CTLS >> 32) & SPBEF_ENABLE_XSAVES) //can it enable XSAVES ?
      {
        sendstringf("Enabling xsaves\n");
        secondarycpu|=SPBEF_ENABLE_XSAVES;
      }

      if ((IA32_VMX_SECONDARY_PROCBASED_CTLS >> 32) & SPBEF_ENABLE_INVPCID) //can it enable INVPCID ?
      {
        sendstringf("Enabling INVPCID\n");
        secondarycpu|=SPBEF_ENABLE_INVPCID;
      }

      vmwrite(vm_execution_controls_cpu_secondary, secondarycpu);



    }



    currentcpuinfo->efer=originalstate->originalEFER;

    DWORD new_vm_entry_controls;

    if (originalstate->originalLME)
    {
      sendstringf("guest is 64bit\n");
      currentcpuinfo->efer|=(1<<8) | (1<<10);
      new_vm_entry_controls=(DWORD)IA32_VMX_ENTRY_CTLS | RESTORE_DEBUG_CONTROLS | IA32E_MODE_GUEST; //64-bit mode
    }
    else
      new_vm_entry_controls=(DWORD)IA32_VMX_ENTRY_CTLS | RESTORE_DEBUG_CONTROLS;


    vmwrite(vm_entry_controls, new_vm_entry_controls);
    sendstringf("Set vm_entry_controls to %8 (became %8)\n", new_vm_entry_controls, (DWORD)vmread(vm_entry_controls));





    vmwrite(vm_cr0_fakeread,originalstate->cr0); //cr0 read shadow
    vmwrite(vm_cr4_fakeread,originalstate->cr4); //cr4 read shadow
    vmwrite(vm_cr3_targetvalue0,0xffffffffffffffffULL); //cr3-target value 0

    currentcpuinfo->guestCR0=originalstate->cr0;
    currentcpuinfo->guestCR3=originalstate->cr3;

    vmwrite(vm_guest_cr0, (ULONG)IA32_VMX_CR0_FIXED0 | originalstate->cr0);
    vmwrite(vm_guest_cr3, originalstate->cr3);
    vmwrite(vm_guest_cr4, (ULONG)IA32_VMX_CR4_FIXED0 | originalstate->cr4);

    vmwrite(vm_guest_gdtr_base, (UINT64)originalstate->gdtbase);
    vmwrite(vm_guest_gdt_limit, (UINT64)originalstate->gdtlimit);
    vmwrite(vm_guest_idtr_base, (UINT64)originalstate->idtbase);
    vmwrite(vm_guest_idt_limit, (UINT64)originalstate->idtlimit);


    //gdt MUST be paged in
    gdt=(PGDT_ENTRY)(UINT64)MapPhysicalMemory(
          getPhysicalAddressVM(currentcpuinfo, originalstate->gdtbase, &notpaged)
          ,currentcpuinfo->AvailableVirtualAddress+0x00200000
        );

    if ((UINT64)originalstate->ldt)
    {
      UINT64 ldtbase; //should be 0 in 64bit
      ULONG ldtlimit;

      sendstring("ldt is valid, so getting the information\n\r");

      ldtbase=(gdt[(ldtselector >> 3)].Base24_31 << 24) + gdt[(ldtselector >> 3)].Base0_23;
      ldtlimit=(gdt[(ldtselector >> 3)].Limit16_19 << 16) + gdt[(ldtselector >> 3)].Limit0_15;
      ldt=(PGDT_ENTRY)(UINT64)MapPhysicalMemory(getPhysicalAddressVM(currentcpuinfo, ldtbase, &notpaged), currentcpuinfo->AvailableVirtualAddress+0x00400000);
    }


    vmwrite(vm_guest_es,(UINT64)originalstate->es); //es selector
    vmwrite(vm_guest_cs,(UINT64)originalstate->cs); //cs selector
    vmwrite(vm_guest_ss,(UINT64)originalstate->ss); //ss selector
    vmwrite(vm_guest_ds,(UINT64)originalstate->ds); //ds selector
    vmwrite(vm_guest_fs,(UINT64)originalstate->fs); //fs selector
    vmwrite(vm_guest_gs,(UINT64)originalstate->gs); //gs selector
    vmwrite(vm_guest_ldtr,(UINT64)originalstate->ldt); //ldtr selector
    vmwrite(vm_guest_tr,(UINT64)originalstate->tr); //tr selector


    //limits
    vmwrite(vm_guest_es_limit,getSegmentLimit(gdt,ldt,originalstate->es));
    vmwrite(vm_guest_cs_limit,getSegmentLimit(gdt,ldt,originalstate->cs));
    vmwrite(vm_guest_ss_limit,getSegmentLimit(gdt,ldt,originalstate->ss));
    vmwrite(vm_guest_ds_limit,getSegmentLimit(gdt,ldt,originalstate->ds));
    vmwrite(vm_guest_fs_limit,getSegmentLimit(gdt,ldt,originalstate->fs));
    vmwrite(vm_guest_gs_limit,getSegmentLimit(gdt,ldt,originalstate->gs));
    vmwrite(vm_guest_ldtr_limit,getSegmentLimit(gdt,ldt,originalstate->ldt));
    vmwrite(vm_guest_tr_limit,getSegmentLimit(gdt,ldt,originalstate->tr));

    //bases
    vmwrite(vm_guest_ldtr_base,getSegmentBase(gdt,ldt,originalstate->ldt));

    vmwrite(vm_guest_es_base,getSegmentBase(gdt,ldt,originalstate->es));
    vmwrite(vm_guest_cs_base,getSegmentBase(gdt,ldt,originalstate->cs));
    vmwrite(vm_guest_ds_base,getSegmentBase(gdt,ldt,originalstate->ss));
    vmwrite(vm_guest_ds_base,getSegmentBase(gdt,ldt,originalstate->ds));
    if (originalstate->originalLME)
    {
      sendstringf("64-bit\n");
      vmwrite(vm_guest_fs_base,originalstate->fsbase);
      vmwrite(vm_guest_gs_base,originalstate->gsbase);

      sendstringf("Have set fs base to %6 and gs base to %6\n",vmread(vm_guest_fs_base),vmread(vm_guest_gs_base));

      vmwrite(vm_guest_tr_base,getSegmentBaseEx(gdt,ldt,originalstate->tr, 1));
    }
    else
    {
      sendstringf("32-bit\n");
      vmwrite(vm_guest_fs_base,getSegmentBase(gdt,ldt,originalstate->fs));
      vmwrite(vm_guest_gs_base,getSegmentBase(gdt,ldt,originalstate->gs));
      vmwrite(vm_guest_tr_base,getSegmentBase(gdt,ldt,originalstate->tr));
    }



    //access rights
    vmwrite(vm_guest_es_access_rights,getSegmentAccessRights(gdt,ldt,originalstate->es));
    vmwrite(vm_guest_cs_access_rights,getSegmentAccessRights(gdt,ldt,originalstate->cs));
    vmwrite(vm_guest_ss_access_rights,getSegmentAccessRights(gdt,ldt,originalstate->ss));
    vmwrite(vm_guest_ds_access_rights,getSegmentAccessRights(gdt,ldt,originalstate->ds));
    vmwrite(vm_guest_fs_access_rights,getSegmentAccessRights(gdt,ldt,originalstate->fs));
    vmwrite(vm_guest_gs_access_rights,getSegmentAccessRights(gdt,ldt,originalstate->gs));
    vmwrite(vm_guest_ldtr_access_rights,getSegmentAccessRights(gdt,ldt,originalstate->ldt));
    vmwrite(vm_guest_tr_access_rights,getSegmentAccessRights(gdt,ldt,originalstate->tr));

    vmwrite(vm_guest_IA32_SYSENTER_CS,(UINT64)readMSR(IA32_SYSENTER_CS_MSR));
    vmwrite(vm_guest_IA32_SYSENTER_ESP,(UINT64)readMSR(IA32_SYSENTER_ESP_MSR));
    vmwrite(vm_guest_IA32_SYSENTER_EIP,(UINT64)readMSR(IA32_SYSENTER_EIP_MSR));

    currentcpuinfo->actual_sysenter_CS=vmread(vm_guest_IA32_SYSENTER_CS);
    currentcpuinfo->actual_sysenter_ESP=vmread(vm_guest_IA32_SYSENTER_ESP);
    currentcpuinfo->actual_sysenter_EIP=vmread(vm_guest_IA32_SYSENTER_EIP);



    vmwrite(vm_guest_dr7,(UINT64)originalstate->dr7); //dr7
    vmwrite(0x4826,(UINT64)0); //normal activity state
    if (originalstate->originalLME)
    {
      vmwrite(vm_guest_rsp,(UINT64)originalstate->rsp); //rsp
      vmwrite(vm_guest_rip,(UINT64)originalstate->rip); //rip
    }
    else
    {
      //force 32-bit
      vmwrite(vm_guest_rsp,(ULONG)originalstate->rsp); //rsp
      vmwrite(vm_guest_rip,(ULONG)originalstate->rip); //rip
    }
    vmwrite(vm_guest_rflags,(UINT64)(UINT64)originalstate->rflags); //rflags

  }
  else
  {
    sendstringf("booted of a disk, load os manually");
    reg_traccessrights.AccessRights=0;
    reg_traccessrights.Segment_type=11; //11=32-bit 3=16-bit
    reg_traccessrights.S=0;
    reg_traccessrights.DPL=0;
    reg_traccessrights.P=1;
    reg_traccessrights.G=0;
    reg_traccessrights.D_B=1;

    if (currentcpuinfo->cpunr==0)
    {
      //not loaded from OS and first cpu
      sendstringf("manually load for cpu 0\n");
      //entry to start from almost same as vmm exec state
      reg_csaccessrights.AccessRights=0;
      reg_csaccessrights.Segment_type=11;
      reg_csaccessrights.S=1;
      reg_csaccessrights.DPL=0;
      reg_csaccessrights.P=1;
      reg_csaccessrights.L=1;
      reg_csaccessrights.G=0;
      reg_csaccessrights.D_B=0;

      currentcpuinfo->guestCR3=getCR3();
      currentcpuinfo->guestCR0=getCR0();
      currentcpuinfo->hasIF=0;

      DWORD new_vm_execution_controls_cpu=(DWORD)IA32_VMX_PROCBASED_CTLS;

      if ((IA32_VMX_PROCBASED_CTLS >> 32) & HLT_EXITING)
        new_vm_execution_controls_cpu|=HLT_EXITING;
      else
        sendstring("<<<<<<WARNING: This system does not support HLT_EXITING>>>>>>\n");

      if ((IA32_VMX_PROCBASED_CTLS >> 32) & INVLPG_EXITING)
        new_vm_execution_controls_cpu|=INVLPG_EXITING;
      else
        sendstring("<<<<<<WARNING: This system does not support INVLPG_EXITING>>>>>>\n");

      /*
      if ((IA32_VMX_PROCBASED_CTLS >> 32) & USE_IO_BITMAPS)
        new_vm_execution_controls_cpu|=USE_IO_BITMAPS;
      else
        sendstring("<<<<<<WARNING: This system does not support USE_IO_BITMAPS>>>>>>\n");*/

      if ((IA32_VMX_PROCBASED_CTLS >> 32) & USE_MSR_BITMAPS)
        new_vm_execution_controls_cpu|=USE_MSR_BITMAPS;
      else
        sendstring("<<<<<<WARNING: This system does not support USE_MSR_BITMAPS>>>>>>\n");


      if ((IA32_VMX_PROCBASED_CTLS >> 32) & (1<<31)) //secondary procbased ctl support
        new_vm_execution_controls_cpu=new_vm_execution_controls_cpu | (1<<31);

      vmwrite(vm_execution_controls_cpu, new_vm_execution_controls_cpu); //processor-based vm-execution controls
      sendstringf("Set vm_execution_controls_cpu to %8 (became %8)\n", new_vm_execution_controls_cpu, (DWORD)vmread(vm_execution_controls_cpu));

      if ((new_vm_execution_controls_cpu >> 31) & 1)
      {
        //it has a secondary entry
        //enable rdtscp
        sendstringf("Enabling rdtscp\n");
        if ((IA32_VMX_SECONDARY_PROCBASED_CTLS >> 32) & SPBEF_ENABLE_RDTSCP) //can it enable rdtscp ?
          vmwrite(vm_execution_controls_cpu_secondary, SPBEF_ENABLE_RDTSCP); //enable rdtscp
      }



      DWORD new_vm_entry_controls=(DWORD)IA32_VMX_ENTRY_CTLS;

      if ((IA32_VMX_ENTRY_CTLS >> 32) & IA32E_MODE_GUEST)
        new_vm_entry_controls|=IA32E_MODE_GUEST;
      else
        sendstring("<<<<<<WARNING: This system does not support IA32E_MODE_GUEST>>>>>>\n");

      if ((IA32_VMX_ENTRY_CTLS >> 32) & RESTORE_DEBUG_CONTROLS)
        new_vm_entry_controls|=RESTORE_DEBUG_CONTROLS;
      else
        sendstring("<<<<<<WARNING: This system does not support the RESTORE_DEBUG_CONTROLS vm_entry control option>>>>>>\n");


      vmwrite(vm_entry_controls,new_vm_entry_controls); //vm-entry controls   bit9: ia-32e mode guest is guest controlled
      sendstringf("Set vm_entry_controls to %8 (became %8)\n", new_vm_entry_controls, (DWORD)vmread(vm_entry_controls));




      vmwrite(vm_cr0_fakeread,(UINT64)getCR0()); //cr0 read shadow
      vmwrite(vm_cr4_fakeread,(UINT64)getCR4()); //cr4 read shadow
      vmwrite(vm_cr3_targetvalue0,(UINT64)getCR3()); //cr3-target value 0

      vmwrite(vm_guest_gdtr_base,(UINT64)getGDTbase()); //gdtr base
      vmwrite(vm_guest_idtr_base,(UINT64)getIDTbase()); //idtr base
      vmwrite(vm_guest_gdt_limit,(UINT64)88); //gdtr limit
      vmwrite(vm_guest_idt_limit,(UINT64)8*256); //idtr limit


      vmwrite(vm_guest_cr0,getCR0()); //guest cr0
      vmwrite(vm_guest_cr3,getCR3()); //cr3
      vmwrite(vm_guest_cr4,getCR4() |(UINT64)IA32_VMX_CR4_FIXED0 | 1 | (1 << 4) | ( 1 << 5) | (1<<0)); //guest cr4

      vmwrite(vm_guest_es,(UINT64)8); //es selector
      vmwrite(vm_guest_cs,(UINT64)80); //cs selector
      vmwrite(vm_guest_ss,(UINT64)8); //ss selector
      vmwrite(vm_guest_ds,(UINT64)8); //ds selector
      vmwrite(vm_guest_fs,(UINT64)8); //fs selector
      vmwrite(vm_guest_gs,(UINT64)8); //gs selector
      vmwrite(vm_guest_ldtr,(UINT64)0); //ldtr selector
      vmwrite(vm_guest_tr,(UINT64)64); //tr selector

      vmwrite(vm_guest_es_limit,(UINT64)0); //es limit
      vmwrite(vm_guest_cs_limit,(UINT64)0); //cs limit
      vmwrite(vm_guest_ss_limit,(UINT64)0); //ss limit
      vmwrite(vm_guest_ds_limit,(UINT64)0); //ds limit
      vmwrite(vm_guest_fs_limit,(UINT64)0); //fs limit
      vmwrite(vm_guest_gs_limit,(UINT64)0); //gs limit
      vmwrite(vm_guest_ldtr_limit,(UINT64)0); //ldtr limit
      vmwrite(vm_guest_tr_limit,(UINT64)sizeof(TSS)+32+8192+1); //tr limit

      vmwrite(vm_guest_es_base,(UINT64)0); //es base
      vmwrite(vm_guest_cs_base,(UINT64)0); //cs base
      vmwrite(vm_guest_ss_base,(UINT64)0); //ss base
      vmwrite(vm_guest_ds_base,(UINT64)0); //ds base
      vmwrite(vm_guest_fs_base,(UINT64)0); //fs base
      vmwrite(vm_guest_gs_base,(UINT64)0); //gs base
      vmwrite(vm_guest_ldtr_base,(UINT64)0); //ldtr base
      vmwrite(vm_guest_tr_base,(UINT64)mainTSS); //tr base

      vmwrite(vm_guest_es_access_rights,(UINT64)(1<<16)); //es access rights
      vmwrite(vm_guest_cs_access_rights,(UINT64)reg_csaccessrights.AccessRights); //cs access rights
      vmwrite(vm_guest_ss_access_rights,(UINT64)(1<<16)); //ss access rights
      vmwrite(vm_guest_ds_access_rights,(UINT64)(1<<16)); //ds access rights
      vmwrite(vm_guest_fs_access_rights,(UINT64)(1<<16)); //fs access rights
      vmwrite(vm_guest_gs_access_rights,(UINT64)(1<<16)); //gs access rights
      vmwrite(vm_guest_ldtr_access_rights,(UINT64)(1<<16)); //ldtr access rights (bit 16 is unusable bit
      vmwrite(vm_guest_tr_access_rights,(UINT64)reg_traccessrights.AccessRights); //tr access rights

      vmwrite(vm_guest_IA32_SYSENTER_CS,(UINT64)0);
      vmwrite(vm_guest_IA32_SYSENTER_ESP,(UINT64)0);
      vmwrite(vm_guest_IA32_SYSENTER_EIP,(UINT64)0);

      vmwrite(vm_guest_dr7,(UINT64)0x400); //dr7

      vmwrite(0x4826,(UINT64)0); //guest activity state, normal
      //vmwrite(vm_guest_rsp,(UINT64)0x8fffc); //rsp
      vmwrite(vm_guest_rsp,((UINT64)malloc(4096))+0x1000-8); //rsp
      vmwrite(vm_guest_rip,(UINT64)quickboot); //rip
      vmwrite(vm_guest_rflags,(UINT64)getRFLAGS()); //rflags

    }
    else
    {
      //8086 entry
      DWORD gdtbase, idtbase;
      gdtbase=VirtualToPhysical(getGDTbase());
      idtbase=VirtualToPhysical((UINT64)idttable32);

      sendstringf("entering sleepmode for ap cpu\n");

      reg_csaccessrights.AccessRights=0;
      reg_csaccessrights.Segment_type=3;
      reg_csaccessrights.S=1;
      reg_csaccessrights.DPL=3;
      reg_csaccessrights.P=1;
      reg_csaccessrights.G=0;
      reg_csaccessrights.D_B=0;

      currentcpuinfo->guestCR3=0;
      currentcpuinfo->guestCR0=0;
      currentcpuinfo->hasIF=0;


      DWORD new_vm_execution_controls_cpu=(UINT64)IA32_VMX_PROCBASED_CTLS | INVLPG_EXITING | USE_IO_BITMAPS | USE_MSR_BITMAPS;
      if ((IA32_VMX_PROCBASED_CTLS >> 32) & (1<<31)) //secondary procbased ctl support
        new_vm_execution_controls_cpu=new_vm_execution_controls_cpu | (1<<31);


      vmwrite(vm_execution_controls_cpu, new_vm_execution_controls_cpu); //don't exit on hlt, it needs it
      if ((new_vm_execution_controls_cpu >> 31) & 1)
      {
        //it has a secondary entry
        //enable rdtscp
        sendstringf("Enabling rdtscp\n");
        if ((IA32_VMX_SECONDARY_PROCBASED_CTLS >> 32) & SPBEF_ENABLE_RDTSCP) //can it enable rdtscp ?
          vmwrite(vm_execution_controls_cpu_secondary, SPBEF_ENABLE_RDTSCP); //enable rdtscp
      }


      vmwrite(vm_entry_controls,(UINT64)IA32_VMX_ENTRY_CTLS ); //32bit/16bit init

      vmwrite(vm_cr0_fakeread,(UINT64)0); //cr0 read shadow
      vmwrite(vm_cr4_fakeread,(UINT64)0); //cr4 read shadow
      vmwrite(vm_cr3_targetvalue0,(UINT64)0xffffffffffffffffULL); //cr3-target value 0

      vmwrite(vm_guest_gdtr_base, gdtbase);
      vmwrite(vm_guest_gdt_limit, getGDTsize());
      vmwrite(vm_guest_idtr_base, idtbase);
      vmwrite(vm_guest_idt_limit, 256*8);

      vmwrite(vm_guest_cr0,(UINT64)IA32_VMX_CR0_FIXED0 | (1 << 16)); //guest cr0
      vmwrite(vm_guest_cr3,(UINT64)VirtualToPhysical((UINT64)nonpagedEmulationPagedir)); //cr3
      vmwrite(vm_guest_cr4,(UINT64)IA32_VMX_CR4_FIXED0 | 1 | (1 << 4) | ( 1 << 5) | (1<<0)); //guest cr4

      vmwrite(vm_guest_es,(UINT64)0); //es selector
      vmwrite(vm_guest_cs,(UINT64)0x2000); //cs selector
      vmwrite(vm_guest_ss,(UINT64)0); //ss selector
      vmwrite(vm_guest_ds,(UINT64)0); //ds selector
      vmwrite(vm_guest_fs,(UINT64)0); //fs selector
      vmwrite(vm_guest_gs,(UINT64)0); //gs selector
      vmwrite(vm_guest_ldtr,(UINT64)0); //ldtr selector
      vmwrite(vm_guest_tr,64); //the tss selector

      vmwrite(vm_guest_es_limit,(UINT64)0xffff); //es limit
      vmwrite(vm_guest_cs_limit,(UINT64)0xffff); //cs limit
      vmwrite(vm_guest_ss_limit,(UINT64)0xffff); //ss limit
      vmwrite(vm_guest_ds_limit,(UINT64)0xffff); //ds limit
      vmwrite(vm_guest_fs_limit,(UINT64)0xffff); //fs limit
      vmwrite(vm_guest_gs_limit,(UINT64)0xffff); //gs limit
      vmwrite(vm_guest_ldtr_limit,(UINT64)0); //ldtr limit
      vmwrite(vm_guest_tr_limit,(ULONG)sizeof(TSS)+32+8192+1); //tr limit

      vmwrite(vm_guest_es_base,(UINT64)0); //es base
      vmwrite(vm_guest_cs_base,(UINT64)0x20000); //cs base
      vmwrite(vm_guest_ss_base,(UINT64)0); //ss base
      vmwrite(vm_guest_ds_base,(UINT64)0); //ds base
      vmwrite(vm_guest_fs_base,(UINT64)0); //fs base
      vmwrite(vm_guest_gs_base,(UINT64)0); //gs base
      vmwrite(vm_guest_ldtr_base,(UINT64)0); //ldtr base
      vmwrite(vm_guest_tr_base,(UINT64)VirtualToPhysical((UINT64)VirtualMachineTSS_V8086)); //tr basebase

      vmwrite(vm_guest_es_access_rights,(UINT64)reg_csaccessrights.AccessRights); //es access rights
      vmwrite(vm_guest_cs_access_rights,(UINT64)reg_csaccessrights.AccessRights); //cs access rights
      vmwrite(vm_guest_ss_access_rights,(UINT64)reg_csaccessrights.AccessRights); //ss access rights
      vmwrite(vm_guest_ds_access_rights,(UINT64)reg_csaccessrights.AccessRights); //ds access rights
      vmwrite(vm_guest_fs_access_rights,(UINT64)reg_csaccessrights.AccessRights); //fs access rights
      vmwrite(vm_guest_gs_access_rights,(UINT64)reg_csaccessrights.AccessRights); //gs access rights
      vmwrite(vm_guest_ldtr_access_rights,(UINT64)(1<<16)); //ldtr access rights (bit 16 is unusable bit
      vmwrite(vm_guest_tr_access_rights,(UINT64)reg_traccessrights.AccessRights); //tr access rights

      currentcpuinfo->RealMode.IDTBase=0;
      currentcpuinfo->RealMode.GDTBase=0;
      currentcpuinfo->RealMode.IDTLimit=0x400;
      currentcpuinfo->RealMode.GDTBase=0;

      vmwrite(vm_guest_IA32_SYSENTER_CS,(UINT64)0);
      vmwrite(vm_guest_IA32_SYSENTER_ESP,(UINT64)0);
      vmwrite(vm_guest_IA32_SYSENTER_EIP,(UINT64)0);

      vmwrite(vm_guest_dr7,(UINT64)0x400); //dr7

      RFLAGS guestrflags;


      vmwrite(0x4826,(UINT64)3); //guest activity state, wait for sipi
      vmwrite(vm_guest_rsp,(UINT64)0xffc); //rsp
      vmwrite(vm_guest_rip,(UINT64)(&bochswaitforsipiloop)-(UINT64)(&movetoreal)); //rip //should never be executed

      guestrflags.value=0;
      guestrflags.reserved1=1;
      guestrflags.IOPL=3;
      guestrflags.VM=1;
      vmwrite(vm_guest_rflags,guestrflags.value ); //rflag
    }
  }



  vmwrite(0x2800,(UINT64)0xffffffffffffffff); //VMCS link pointer (low)
  vmwrite(0x2802,(UINT64)0); //IA32_DEBUGCTL (low)

  vmwrite(vm_guest_interruptability_state,(UINT64)0); //interruptibility state




  vmwrite(0x4828,(UINT64)0); //smbase


  vmwrite(0x6822,(UINT64)0); //pending debug exceptions

  //----------------HOST----------------
  //host 16-bit:
  sendstringf("Guest is setup to start at %x:%6\n",vmread(vm_guest_cs),vmread(vm_guest_rip));
  sendstring("host setup\n\r");
  vmwrite(0xc00,(UINT64)8); //es selector
  vmwrite(0xc02,(UINT64)80); //cs selector
  vmwrite(0xc04,(UINT64)8); //ss selector
  vmwrite(0xc06,(UINT64)8); //ds selector
  vmwrite(0xc08,(UINT64)8); //fs selector
  vmwrite(0xc0a,(UINT64)8); //gs selector
  vmwrite(0xc0c,(UINT64)96); //tr selector

  vmwrite(0x4c00,(UINT64)readMSR(0x174));  //IA32_SYSENTER_CS
  vmwrite(0x6c10,(UINT64)readMSR(0x175)); //sysenter_esp
  vmwrite(0x6c12,(UINT64)readMSR(0x176)); //sysenter_eip



  vmwrite(0x6c00,(UINT64)getCR0()); //cr0
  vmwrite(0x6c02,(UINT64)getCR3()); //cr3
  vmwrite(0x6c04,(UINT64)getCR4()); //cr4
  vmwrite(0x6c06,(UINT64)readMSR(0xc0000100));  //fs base
  vmwrite(0x6c08,(UINT64)readMSR(0xc0000101));  //gs base
  vmwrite(0x6c0a,(UINT64)ownTSS);  //tr base
  vmwrite(0x6c0c,(UINT64)getGDTbase()); //gdtr base
  vmwrite(0x6c0e,(UINT64)getIDTbase()); //idt base

  sendstring("Finished configuring\n\r");


  globals_have_been_configured=1;
  currentcpuinfo->vmxsetup=1;

  csLeave(&setupVMX_lock);
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

  POriginalState originalstate=NULL;
  if (loadedOS)
    originalstate=(POriginalState)MapPhysicalMemory(loadedOS,currentcpuinfo->AvailableVirtualAddress);

  if (isAMD)
    return launchVMX_AMD(currentcpuinfo, originalstate);


  displayline("Calling vmxloop with currentcpuinfo=%6\n\r",(UINT64)currentcpuinfo);

  sendstring("Right before entering the loop:\n");


  if (currentcpuinfo->cpunr==0)
  {
    sendvmstate(currentcpuinfo,NULL);
    displayline("Last display before entering vmx\n");
  }

  if (loadedOS)
    result=vmxloop(currentcpuinfo, &originalstate->rax);
  else
    result=vmxloop(currentcpuinfo, NULL);

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
    QWORD VMX_BASIC=readMSR(IA32_VMX_BASIC_MSR);
    QWORD VMX_PINBASED_CTLS=readMSR(IA32_VMX_PINBASED_CTLS_MSR);
    QWORD VMX_PROCBASED_CTLS=readMSR(IA32_VMX_PROCBASED_CTLS_MSR);

    QWORD VMX_EXIT_CTLS=readMSR(IA32_VMX_EXIT_CTLS_MSR);
    QWORD VMX_ENTRY_CTLS=readMSR(IA32_VMX_ENTRY_CTLS_MSR);
    QWORD VMX_MISC=readMSR(IA32_VMX_MISC_CTLS_MSR);

    DWORD ctrl_pin=vmread(vm_execution_controls_pin);
    sendstringf("VMX_PINBASED_CTLS=%6 ctrl_pin=%8\n", VMX_PINBASED_CTLS, ctrl_pin);

    DWORD ctrl_cpu=vmread(vm_execution_controls_cpu);
    sendstringf("VMX_PROCBASED_CTLS=%6 ctrl_cpu=%8\n", VMX_PROCBASED_CTLS, ctrl_cpu);

    DWORD ctrl_entry=vmread(vm_entry_controls);
    sendstringf("VMX_ENTRY_CTLS=%6 ctrl_entry=%8\n", VMX_ENTRY_CTLS, ctrl_entry);

    DWORD ctrl_exit=vmread(vm_exit_controls);
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
  StartAddress=atoi(temps,16,&err);

  sendstringf("%6(%s) has physical address %6\n\r",StartAddress,temps, getPhysicalAddressVM(currentcpuinfo, StartAddress, &notpaged));

  if (err)
  {
    sendstringf("err=%d\n");
  }


  sendstring("Number of bytes:");
  readstring(temps,8,8);
  sendstring("\n\r");
  nrofbytes=atoi(temps,10,NULL);


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
  else sendstring("Unreadable memory\n");
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
  unsigned int i,j,k;
  char temps[17];
  UINT64 StartAddress;
  unsigned int nrofbytes;

  sendstring("Startaddress:");
  readstring(temps,16,17);
  sendstring("\n\r");
  temps[16]=0;
  StartAddress=atoi(temps,16,NULL);
  sendstringf("Startaddress=%6\n\r",StartAddress);


  sendstring("Number of bytes:");
  readstring(temps,8,17);
  sendstring("\n\r");
  nrofbytes=atoi(temps,10,NULL);

  sendstringf("Going to show the memory region %6 to %6 \n\r",StartAddress,StartAddress+nrofbytes);

  i=0;
  while (i<nrofbytes)
  {
    unsigned long long toReadInPage=0x00200000-(StartAddress % 0x00200000);

    if (toReadInPage>(nrofbytes-i))
      toReadInPage=(nrofbytes-i);

    MapPhysicalMemory(StartAddress ,  0x00800000);


    for (j=0; j<toReadInPage; j+=16)
    {

      sendstringf("%6 : ",StartAddress+j);
      for (k=j; k<(j+16) && (k<toReadInPage); k++)
      {

        unsigned char *bt;
        bt=(unsigned char *)(0x00800000+((StartAddress % 0x00200000))+k);


        sendstringf("%2 ",*bt);
      }

      if ((j+16)>toReadInPage)
      {
        /* Get the cursor to the right spot */
        unsigned int currentcol=11+3*(toReadInPage-j);
        unsigned int wantedcol=11+3*16;
        for (k=0; k<(wantedcol-currentcol); k++)
          sendstring(" ");
      }

      for (k=j; k<(j+16) && (k<toReadInPage); k++)
      {
        unsigned char tempc=*(unsigned char *)(0x00800000+((StartAddress % 0x00200000))+k);
        if ((tempc<32) || (tempc>0x80) )
          tempc='.';

        sendstringf("%c",tempc);
      }

      sendstring("\n\r");

    }

    StartAddress+=toReadInPage;
    i+=toReadInPage;
  }
}

