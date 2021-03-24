/*
 * vmxsetup.c
 *
 *  Created on: Jan 26, 2018
 *      Author: eric heijnen
 */


#include "vmxsetup.h"
#include "vmreadwrite.h"
#include "msrnames.h"
#include "vmxcontrolstructures.h"
#include "main.h"
#include "mm.h"
#include "vmmhelper.h"
#include "offloados.h"
#include "vmpaging.h"
#include "vmeventhandler.h"

#include "epthandler.h"
#include "neward.h"
#include "displaydebug.h"
#include "common.h"


criticalSection setupVMX_lock={.name="setupVMX_lock", .debuglevel=2};

volatile unsigned char *MSRBitmap;
volatile unsigned char *IOBitmap;

int hasEPTsupport=0;
int TSCHooked=0;
int hasNPsupport=1;

int canToggleCR3Exit=0; //intel only flag


extern void realmode_inthooks();
extern void realmode_inthooks_end();

extern void realmode_inthook_new12();
extern void realmode_inthook_new15();

void realmode_inthook_calladdress();

extern DWORD realmode_inthook_original12;
extern DWORD realmode_inthook_original15;
extern WORD realmode_inthook_conventional_memsize;



void setupVMX_AMD(pcpuinfo currentcpuinfo)
{
  //setup the vmcb
  Segment_Attribs reg_csaccessrights;
  Segment_Attribs reg_traccessrights UNUSED;

  if (currentcpuinfo->cpunr!=0)
  {
    sendstringf("setupVMX_AMD for AP cpu\n");
  }

#ifdef AMDNP
  //nested paging, works. But using it for memory cloak is not as fast as on Intel (at best like stealthedit plugin on windows, which can be unstable)
  currentcpuinfo->vmcb->NP_ENABLE=1;
  currentcpuinfo->vmcb->G_PAT=readMSR(0x277);
  //setup a pagebase describing the physical memory

  volatile PPML4 pml4=(PPML4)malloc(4096);
  zeromemory((volatile void*)pml4,4096);


  currentcpuinfo->vmcb->N_CR3=VirtualToPhysical((void*)pml4);
  sendstringf("Setup nCR3 at %6\n", currentcpuinfo->vmcb->N_CR3);

  has_NP_1GBsupport=1;
  has_NP_2MBsupport=1;

  //copy the PML4 table and set a new CR3  (PML4 should never change after this, at least not on a global level)
  PPML4 newpml4=(PPML4)malloc(4096);
  copymem(newpml4, pml4table, 4096);

  sendstringf("Created new PML4 table at %6 (PA %6)\n", newpml4, VirtualToPhysical((void*)newpml4));


  *(QWORD*)(&newpml4[510])=currentcpuinfo->vmcb->N_CR3;
  newpml4[510].P=1;
  newpml4[510].RW=1;

  *(QWORD*)(&newpml4[511])=VirtualToPhysical((void*)newpml4); //point to this one
  newpml4[511].P=1;
  newpml4[511].RW=1;

  asm volatile ("": : :"memory");

  setCR3(VirtualToPhysical((void*)newpml4));

  sendstringf("Set CR3 to %6 . It is now %6\n", VirtualToPhysical((void*)newpml4), getCR3() );

  _invlpg(0xffffff0000000000ULL);

#endif

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

  currentcpuinfo->vmcb->gdtr_limit=0x58;
  currentcpuinfo->vmcb->idtr_limit=8*256;

  currentcpuinfo->vmcb->cs_selector=80;
  //currentcpuinfo->vmcb->cs_limit=0;//0xffffffff;
  //currentcpuinfo->vmcb->ss_limit=0;//0xffffffff;
  currentcpuinfo->vmcb->cs_attrib=(WORD)reg_csaccessrights.SegmentAttrib;

  currentcpuinfo->vmcb->ds_selector=8;
  currentcpuinfo->vmcb->es_selector=8;
  currentcpuinfo->vmcb->ss_selector=8;
  currentcpuinfo->vmcb->fs_selector=8;
  currentcpuinfo->vmcb->gs_selector=8;
  currentcpuinfo->vmcb->ldtr_selector=0;
  currentcpuinfo->vmcb->tr_selector=64;

  sendstringf("cs_attrib(%x)  set to %x\n", ((UINT64)&currentcpuinfo->vmcb->cs_attrib-(UINT64)currentcpuinfo->vmcb), currentcpuinfo->vmcb->cs_attrib);
  sendstringf("gdtr_limit(%x)  set to %x\n", ((UINT64)&currentcpuinfo->vmcb->gdtr_limit-(UINT64)currentcpuinfo->vmcb), currentcpuinfo->vmcb->gdtr_limit);




/*
  currentcpuinfo->vmcb->tr_limit=(UINT64)sizeof(TSS)+32+8192+1;
  currentcpuinfo->vmcb->tr_base=(UINT64)mainTSS;
  currentcpuinfo->vmcb->tr_attrib=(WORD)reg_traccessrights.SegmentAttrib;*/

  currentcpuinfo->vmcb->DR7=0x400;

  currentcpuinfo->vmcb->RSP=((UINT64)malloc(4096))+0x1000-0x28;
  currentcpuinfo->vmcb->RFLAGS=getRFLAGS();

  if (currentcpuinfo->cpunr==0)
    currentcpuinfo->vmcb->RIP=(UINT64)reboot;
  else
    currentcpuinfo->vmcb->RIP=(UINT64)apentryvmx;


  if (!loadedOS)
    currentcpuinfo->vmcb->InterceptINT=1; //break on software interrupts (int 0x15 in realmode to tell the os not to mess with stuff)

  currentcpuinfo->vmcb->InterceptShutdown=1; //in case of a severe error
  currentcpuinfo->vmcb->InterceptVMMCALL=1;
  currentcpuinfo->vmcb->MSR_PROT=1; //some msr's need to be protected

  currentcpuinfo->vmcb->InterceptExceptions=(1<<1) | (1<<3);// | (1<<14); //intercept int1, 3 and 14
 // currentcpuinfo->vmcb->InterceptDR0_15Write=(1<<6); //dr6 so I can see what changed



  /*
   if (currentcpuinfo->cpunr)
   {
     currentcpuinfo->vmcb->InterceptINIT=1; //cpu init (init-sipi-sipi. I need to implement a virtual apic to suppot boot
     currentcpuinfo->vmcb->InterceptCPUID=1;
     currentcpuinfo->vmcb->InterceptExceptions=0xffffffff;
     currentcpuinfo->vmcb->InstructionIntercept2=0xffffffff;

     setCR8(15);
   }*/


  if (MSRBitmap==NULL)
  {
    int i;
    //allocate a MSR bitmap
    MSRBitmap=allocateContiguousMemory(2); //

    if (MSRBitmap==NULL)
    {
      sendstringf("allocateContiguousMemory failed. MSRBitmap=NULL\n");
      while(1);
    }
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
  currentcpuinfo->vmcb->MSRPM_BASE_PA=VirtualToPhysical((void *)MSRBitmap);

  currentcpuinfo->guest_VM_HSAVE_PA=0;


  globals_have_been_configured=1;

  SaveExtraHostState(currentcpuinfo->vmcb_PA); //save some of MSR's that are needed (init did touch the segment registers so those need to be overridden if loadedOS)


  if (loadedOS)
  {
    POriginalState originalstate=(POriginalState)mapPhysicalMemory(loadedOS, sizeof(OriginalState));;
    PGDT_ENTRY gdt=NULL,ldt=NULL;
    ULONG ldtselector=originalstate->ldt;
    int notpaged;


    sendstringf("Setting up guest based on loadedOS settings\n");


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

    sendstringf("originalstate->fsbase=%6\n",originalstate->fsbase);
    sendstringf("originalstate->gsbase=%6\n",originalstate->gsbase);


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
    gdt=(PGDT_ENTRY)(UINT64)mapPhysicalMemory(getPhysicalAddressVM(currentcpuinfo, originalstate->gdtbase, &notpaged), originalstate->gdtlimit);


    ULONG ldtlimit;
    if ((UINT64)originalstate->ldt)
    {
      UINT64 ldtbase; //should be 0 in 64bit


      sendstring("ldt is valid, so getting the information\n\r");

      ldtbase=(gdt[(ldtselector >> 3)].Base24_31 << 24) + gdt[(ldtselector >> 3)].Base0_23;
      ldtlimit=(gdt[(ldtselector >> 3)].Limit16_19 << 16) + gdt[(ldtselector >> 3)].Limit0_15;
      ldt=(PGDT_ENTRY)(UINT64)mapPhysicalMemory(getPhysicalAddressVM(currentcpuinfo, ldtbase, &notpaged), ldtlimit);
    }

    currentcpuinfo->vmcb->es_selector=originalstate->es;
    currentcpuinfo->vmcb->es_attrib=convertSegmentAccessRightsToSegmentAttrib(originalstate->es_AccessRights);
    currentcpuinfo->vmcb->es_limit=originalstate->es_Limit;

    currentcpuinfo->vmcb->cs_selector=originalstate->cs;
    currentcpuinfo->vmcb->cs_attrib=convertSegmentAccessRightsToSegmentAttrib(originalstate->cs_AccessRights);
    currentcpuinfo->vmcb->cs_limit=originalstate->cs_Limit;

    currentcpuinfo->vmcb->ss_selector=originalstate->ss;
    currentcpuinfo->vmcb->ss_attrib=convertSegmentAccessRightsToSegmentAttrib(originalstate->ss_AccessRights);
    currentcpuinfo->vmcb->ss_limit=originalstate->ss_Limit;

    currentcpuinfo->vmcb->ds_selector=originalstate->ds;
    currentcpuinfo->vmcb->ds_attrib=convertSegmentAccessRightsToSegmentAttrib(originalstate->ds_AccessRights);
    currentcpuinfo->vmcb->ds_limit=originalstate->ds_Limit;

    currentcpuinfo->vmcb->fs_selector=originalstate->fs;
    currentcpuinfo->vmcb->fs_attrib=convertSegmentAccessRightsToSegmentAttrib(originalstate->fs_AccessRights);
    currentcpuinfo->vmcb->fs_limit=originalstate->fs_Limit;

    currentcpuinfo->vmcb->gs_selector=originalstate->gs;
    currentcpuinfo->vmcb->gs_attrib=convertSegmentAccessRightsToSegmentAttrib(originalstate->gs_AccessRights);
    currentcpuinfo->vmcb->gs_limit=originalstate->gs_Limit;

    currentcpuinfo->vmcb->ldtr_selector=originalstate->ldt;
    currentcpuinfo->vmcb->tr_selector=originalstate->tr;

    if (originalstate->originalLME)
    {
      //64-bit
      currentcpuinfo->vmcb->cs_base=0;
      currentcpuinfo->vmcb->ss_base=0;
      currentcpuinfo->vmcb->ds_base=0;
      currentcpuinfo->vmcb->es_base=0;
      currentcpuinfo->vmcb->fs_base=originalstate->fsbase;
      currentcpuinfo->vmcb->gs_base=originalstate->gsbase;
      currentcpuinfo->vmcb->tr_base=getSegmentBaseEx(gdt,ldt,originalstate->tr, 1);
    }
    else
    {
      //32-bit
      currentcpuinfo->vmcb->cs_base=getSegmentBase(gdt, ldt, originalstate->cs);
      currentcpuinfo->vmcb->ss_base=getSegmentBase(gdt, ldt, originalstate->ss);
      currentcpuinfo->vmcb->ds_base=getSegmentBase(gdt, ldt, originalstate->ds);
      currentcpuinfo->vmcb->es_base=getSegmentBase(gdt, ldt, originalstate->es);
      currentcpuinfo->vmcb->fs_base=getSegmentBase(gdt, ldt, originalstate->fs);
      currentcpuinfo->vmcb->gs_base=getSegmentBase(gdt, ldt, originalstate->gs);
      currentcpuinfo->vmcb->tr_base=getSegmentBase(gdt, ldt, originalstate->tr);
    }
    currentcpuinfo->vmcb->ldtr_limit=getSegmentLimit(gdt, ldt, originalstate->ldt);
    if (originalstate->tr==0)
      currentcpuinfo->vmcb->tr_limit=0xffff;
    else
      currentcpuinfo->vmcb->tr_limit=getSegmentLimit(gdt, ldt, originalstate->tr);

    currentcpuinfo->vmcb->ldtr_base=getSegmentBase(gdt, ldt, originalstate->ldt);
    currentcpuinfo->vmcb->ldtr_attrib=getSegmentAttrib(gdt,ldt,originalstate->ldt);

    if (originalstate->tr)
      currentcpuinfo->vmcb->tr_attrib=getSegmentAttrib(gdt,ldt,originalstate->tr);
    else
      currentcpuinfo->vmcb->tr_attrib=0x8b;

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



    if (gdt)
      unmapPhysicalMemory(gdt, originalstate->gdtlimit);

    if (ldt)
      unmapPhysicalMemory(ldt, ldtlimit);

    if (originalstate)
      unmapPhysicalMemory(originalstate, sizeof(OriginalState));
  }
  else
  {
    //no loadedinfo
    /*

    if (currentcpuinfo->cpunr)
    {
      UINT64 a,b,c,d;


      currentcpuinfo->vmcb->CR0=0x10;
      currentcpuinfo->vmcb->CR2=0;
      currentcpuinfo->vmcb->CR3=0;
      currentcpuinfo->vmcb->RFLAGS=2;
      currentcpuinfo->vmcb->EFER=0x1000;
      currentcpuinfo->vmcb->RIP=0;


      Segment_Attribs attrib;
      attrib.G=0;
      attrib.D_B=0;
      attrib.L=0;
      attrib.P=1;
      attrib.DPL=0;

      //cs:
      attrib.S=1;
      attrib.Segment_type=0b1010;
      currentcpuinfo->vmcb->cs_attrib=attrib.SegmentAttrib;
      currentcpuinfo->vmcb->cs_selector=0x1000;
      currentcpuinfo->vmcb->cs_base=0x10000;
      currentcpuinfo->vmcb->cs_limit=0xffff;
      {
      PPDPTE_PAE pml4entry;
      PPDPTE_PAE pagedirpointerentry;
      PPDE_PAE pagedirentry;
      PPTE_PAE pagetableentry;

      VirtualAddressToPageEntries(0, &pml4entry, &pagedirpointerentry, &pagedirentry, &pagetableentry);
      pagedirentry[0].RW=1;
      pagedirentry[1].RW=1;
      asm volatile ("": : :"memory");
    }
      *(unsigned char *)0x10000=0xf4; //hlt

      //data
      attrib.S=1;
      attrib.Segment_type=0b0010;
      currentcpuinfo->vmcb->ss_attrib=attrib.SegmentAttrib;
      currentcpuinfo->vmcb->ss_selector=0;
      currentcpuinfo->vmcb->ss_base=0;
      currentcpuinfo->vmcb->ss_limit=0xffff;

      currentcpuinfo->vmcb->ds_attrib=attrib.SegmentAttrib;
      currentcpuinfo->vmcb->ds_selector=0;
      currentcpuinfo->vmcb->ds_base=0;
      currentcpuinfo->vmcb->ds_limit=0xffff;

      currentcpuinfo->vmcb->es_attrib=attrib.SegmentAttrib;
      currentcpuinfo->vmcb->es_selector=0;
      currentcpuinfo->vmcb->es_base=0;
      currentcpuinfo->vmcb->es_limit=0xffff;

      currentcpuinfo->vmcb->fs_attrib=attrib.SegmentAttrib;
      currentcpuinfo->vmcb->fs_selector=0;
      currentcpuinfo->vmcb->fs_base=0;
      currentcpuinfo->vmcb->fs_limit=0xffff;

      currentcpuinfo->vmcb->gs_attrib=attrib.SegmentAttrib;
      currentcpuinfo->vmcb->gs_selector=0;
      currentcpuinfo->vmcb->gs_base=0;
      currentcpuinfo->vmcb->gs_limit=0xffff;

      //ldtr:
      attrib.S=0;
      attrib.Segment_type=0b0010;
      currentcpuinfo->vmcb->ldtr_attrib=attrib.SegmentAttrib;
      currentcpuinfo->vmcb->ldtr_selector=0;
      currentcpuinfo->vmcb->ldtr_base=0;
      currentcpuinfo->vmcb->ldtr_limit=0xffff;

      //tr:
      attrib.S=0;
      attrib.Segment_type=0b0011;
      currentcpuinfo->vmcb->tr_attrib=attrib.SegmentAttrib;
      currentcpuinfo->vmcb->tr_selector=0;
      currentcpuinfo->vmcb->tr_base=0;
      currentcpuinfo->vmcb->tr_limit=0xffff;


      currentcpuinfo->vmcb->gdtr_base=0;
      currentcpuinfo->vmcb->gdtr_limit=0xffff;

      currentcpuinfo->vmcb->idtr_base=0;
      currentcpuinfo->vmcb->idtr_limit=0xffff;


      a=1;
      _cpuid(&a,&b,&c,&d);

      currentcpuinfo->vmcb->RAX=0;
      currentcpuinfo->vmcb->RSP=0;


      setDR0(0);
      setDR1(0);
      setDR2(0);
      setDR3(0);

      currentcpuinfo->vmcb->DR6=0xffff0ff0;
      currentcpuinfo->vmcb->DR7=0x400;

      currentcpuinfo->vmcb->VMCB_CLEAN_BITS=0;
    }*/
  }





  sendstringf("Configured cpu %d\n", currentcpuinfo->cpunr);
  currentcpuinfo->vmxsetup=1;
  csLeave(&setupVMX_lock);

}


int vmx_enableProcBasedFeature(DWORD PBF)
{
  if (((IA32_VMX_PROCBASED_CTLS >> 32) & PBF) == PBF) //can it be set
  {
    vmwrite(vm_execution_controls_cpu, vmread(vm_execution_controls_cpu) | PBF);
    return 1;
  }
  else
    return 0;
}

int vmx_disableProcBasedFeature(DWORD PBF)
{
  QWORD PBCTLS=IA32_VMX_PROCBASED_CTLS;

  if (IA32_VMX_BASIC.default1canbe0)
    PBCTLS=readMSR(IA32_VMX_TRUE_PROCBASED_CTLS_MSR);


  if (PBCTLS & PBF) //can it be set to 0
    return 0; //nope
  else
  {
    vmwrite(vm_execution_controls_cpu, vmread(vm_execution_controls_cpu) & (~PBF));
    return 1;
  }
}

int vmx_enablePinBasedFeature(DWORD PINBF)
{
  if (((IA32_VMX_PINBASED_CTLS >> 32) & PINBF) == PINBF) //can it be set
  {
    vmwrite(vm_execution_controls_pin, vmread(vm_execution_controls_pin) | PINBF);
    return 1;
  }
  else
    return 0;
}

int vmx_disablePinBasedFeature(DWORD PINBF)
{
  QWORD PINBCTLS=IA32_VMX_PINBASED_CTLS;

  if (IA32_VMX_BASIC.default1canbe0)
    PINBCTLS=readMSR(IA32_VMX_TRUE_PINBASED_CTLS_MSR);


  if (PINBCTLS & PINBF) //can it be set to 0
    return 0; //nope
  else
  {
    vmwrite(vm_execution_controls_pin, vmread(vm_execution_controls_pin) & (~PINBF));
    return 1;
  }
}

int vmx_enableNMIWindowExiting(void)
{
  if (vmx_enableProcBasedFeature(PBEF_NMI_WINDOW_EXITING))
  {
    //PBEF_NMI_WINDOW_EXITING can be set
    //"If the “virtual NMIs” VM-execution control is 0, the “NMI-window exiting” VM-execution control must be 0."
    //so virtual NMIs must be 1 as well
    if (vmx_enablePinBasedFeature(PINBEF_VIRTUAL_NMIS))
    {
      //"If the “NMI exiting” VM-execution control is 0, the “virtual NMIs” VM-execution control must be 0"
      //so also enable NMI exiting
      if (vmx_enablePinBasedFeature(PINBEF_NMI_EXITING))
        return 1;

      vmx_disablePinBasedFeature(PINBEF_VIRTUAL_NMIS);
    }

    vmx_disableProcBasedFeature(PBEF_NMI_WINDOW_EXITING);
  }

  return 0;
}

int vmx_disableNMIWindowExiting(void)
{
  int a,b,c;
  a=vmx_disableProcBasedFeature(PBEF_NMI_WINDOW_EXITING);
  b=vmx_disablePinBasedFeature(PINBEF_VIRTUAL_NMIS);
  c=vmx_disablePinBasedFeature(PINBEF_NMI_EXITING);

  return (a+b+c==3);
}

int vmx_addSingleSteppingReasonEx(pcpuinfo currentcpuinfo, int reason, void *data)
{
  if (currentcpuinfo->singleStepping.ReasonsPos>=currentcpuinfo->singleStepping.ReasonsLength) //realloc
  {
    currentcpuinfo->singleStepping.ReasonsLength=(currentcpuinfo->singleStepping.ReasonsLength+2)*2;
    currentcpuinfo->singleStepping.Reasons=realloc(currentcpuinfo->singleStepping.Reasons, currentcpuinfo->singleStepping.ReasonsLength * sizeof(SingleStepReason));
  }

  //always add to the end
  if (currentcpuinfo->singleStepping.ReasonsPos>=1)
  {
    sendstringf("Multiple single stepping reasons\n");
  }

  currentcpuinfo->singleStepping.Reasons[currentcpuinfo->singleStepping.ReasonsPos].Reason=reason;
  currentcpuinfo->singleStepping.Reasons[currentcpuinfo->singleStepping.ReasonsPos].Data=data;
  currentcpuinfo->singleStepping.ReasonsPos++;

  return currentcpuinfo->singleStepping.ReasonsPos-1;

}

int vmx_addSingleSteppingReason(pcpuinfo currentcpuinfo, int reason, int ID)
{
  if (currentcpuinfo->singleStepping.ReasonsPos>=currentcpuinfo->singleStepping.ReasonsLength) //realloc
  {
    currentcpuinfo->singleStepping.ReasonsLength=(currentcpuinfo->singleStepping.ReasonsLength+2)*2;
    currentcpuinfo->singleStepping.Reasons=realloc(currentcpuinfo->singleStepping.Reasons, currentcpuinfo->singleStepping.ReasonsLength * sizeof(SingleStepReason));
  }

  //always add to the end
  if (currentcpuinfo->singleStepping.ReasonsPos>=1)
  {
    sendstringf("Multiple single stepping reasons\n");
  }

  currentcpuinfo->singleStepping.Reasons[currentcpuinfo->singleStepping.ReasonsPos].Reason=reason;
  currentcpuinfo->singleStepping.Reasons[currentcpuinfo->singleStepping.ReasonsPos].ID=ID;
  currentcpuinfo->singleStepping.ReasonsPos++;

  return currentcpuinfo->singleStepping.ReasonsPos-1;
}

int vmx_enableSingleStepMode(void)
{
  pcpuinfo c=getcpuinfo();
  //sendstringf("%d Enabling single step mode\n", c->cpunr);


  if (isAMD)
  {
   // sendstringf("%d CS:RIP=%x:%6 RCX=%d\n", c->cpunr, c->vmcb->cs_selector, c->vmcb->RIP);

    //break on external interrupts and exceptions
    c->vmcb->InterceptVINTR=1;
    c->vmcb->InterceptINTR=1;
    c->vmcb->InterceptExceptions=0x0000ffff;

    //perhaps enable INTERRUPT_SHADOW?

    //mark the intercepts as changed
    //sendstringf("b c->vmcb->VMCB_CLEAN_BITS=%6\n",c->vmcb->VMCB_CLEAN_BITS);
    c->vmcb->VMCB_CLEAN_BITS&=~(1<<0);
    c->vmcb->VMCB_CLEAN_BITS=0;
   //sendstringf("a c->vmcb->VMCB_CLEAN_BITS=%6\n",c->vmcb->VMCB_CLEAN_BITS);

    RFLAGS v;
    v.value=c->vmcb->RFLAGS;

    if (c->singleStepping.ReasonsPos==0) //first one
      c->singleStepping.PreviousTFState=v.TF;

    v.TF=1; //single step mode
    v.RF=1;
    if (v.IF)
      c->vmcb->INTERRUPT_SHADOW=1;

    //todo: intercept pushf/popf/iret and the original RF flag state (though for a single step that should have no effect

    c->vmcb->RFLAGS=v.value;
    c->singleStepping.Method=3; //Trap flag

    //turn of syscall, and when syscall is executed, capture the UD, re-enable it, but change the flags mask to keep the TF enabled, and the step after that adjust R11 so that the TF is gone and restore the flags mask.  Then continue as usual;
    if (c->singleStepping.ReasonsPos==0)
    {
      c->singleStepping.PreviousEFER=c->vmcb->EFER;
      c->singleStepping.PreviousFMASK=c->vmcb->SFMASK;
      c->singleStepping.LastInstructionWasSyscall=0;

      c->vmcb->EFER&=0xfffffffffffffffeULL;
      c->vmcb->VMCB_CLEAN_BITS&=~(1<< 5); //efer got changed
    }


    return 1;

  }
  else
  {
    sendstring("\n");

   /* if ((vmread(vm_entry_interruptioninfo) >> 31)==0)
      vmwrite(vm_guest_interruptability_state,2); //execute at least one instruction
    else
      sendstringf("Not setting the interruptability state\n");*/

    if (vmx_enableProcBasedFeature(PBEF_MONITOR_TRAP_FLAG))
    {
      sendstring("Using the monitor trap flag\n");
      c->singleStepping.Method=1;
      return 1;
    }
    else
    {
      c->singleStepping.Method=2;
      if (vmx_enableProcBasedFeature(PBEF_INTERRUPT_WINDOW_EXITING))
      {
        if ((vmread(vm_entry_interruptioninfo) >> 31)==0) //if no interrupt pending
          vmwrite(vm_guest_interruptability_state,2); //execute at least one instruction
        //else go to that interrupt that is pending and then stop

        sendstring("Using the interrupt window\n");
        return 1;
      }
    }
  }

  return 0;
}

int vmx_disableSingleStepMode(void)
{
  int r=0;
  pcpuinfo c=getcpuinfo();

  sendstringf("%d Disabling single step mode\n", c->cpunr);

  if (isAMD)
  {
    //shouldn't be needed but do it anyhow

    sendstringf("%d RFLAGS was %x\n", c->cpunr, c->vmcb->RFLAGS);


    RFLAGS v;
    v.value=c->vmcb->RFLAGS;
    v.TF=c->singleStepping.PreviousTFState;  // 0; //single step mode

    c->vmcb->RFLAGS=v.value;
    sendstringf("%d RFLAGS is %x\n", c->cpunr, c->vmcb->RFLAGS);



    c->singleStepping.Method=0;

    c->vmcb->InterceptVINTR=0;
    c->vmcb->InterceptINTR=0;
    c->vmcb->InterceptExceptions=(1<<1) | (1<<3); // todo: load current exceptions hooks


    //mark the intercepts as changed
    sendstringf("b c->vmcb->VMCB_CLEAN_BITS=%6\n",c->vmcb->VMCB_CLEAN_BITS);
    c->vmcb->VMCB_CLEAN_BITS&=~(1<<0);
    c->vmcb->VMCB_CLEAN_BITS=0;
    sendstringf("a c->vmcb->VMCB_CLEAN_BITS=%6\n",c->vmcb->VMCB_CLEAN_BITS);

    c->vmcb->EFER=c->singleStepping.PreviousEFER;
    c->vmcb->SFMASK=c->singleStepping.PreviousFMASK;
    c->singleStepping.LastInstructionWasSyscall=0;

    c->vmcb->VMCB_CLEAN_BITS&=~(1<< 5); //efer

    return 1;
  }
  else
  {
    if (c->singleStepping.Method==1)
      r=vmx_disableProcBasedFeature(PBEF_MONITOR_TRAP_FLAG);

    if (c->singleStepping.Method==2)
      r=vmx_disableProcBasedFeature(PBEF_INTERRUPT_WINDOW_EXITING);
  }
  return r;
}

//stealthedit:
//mark page as execute, but not as read/write
//on read/write undo change, set TF, run, wait till int1/first vm exit(what about ints? Enable external int exiting), unset TF, redo change, run

//on systems with no exec only
//alt1: mark as read/write but not exec. on ept exit change RIP to adjusted copy
//alt2: mark as no access. on ept exit set TF
int setupEPT(pcpuinfo currentcpuinfo)
{
  //check for Secondary Processor-Based VM-Execution Controls
  QWORD IA32_VMX_PROCBASED_CTLS=readMSR(IA32_VMX_PROCBASED_CTLS_MSR);

  sendstring("setupEPT\n");

  hasEPTsupport=0;
  //return 0; //<-------------DEBUG

  if (IA32_VMX_PROCBASED_CTLS >> 63)
  {
    //secondary procbased controls
    QWORD IA32_VMX_SECONDARY_PROCBASED_CTLS=readMSR(IA32_VMX_PROCBASED_CTLS2_MSR); //allowed1/allowed0

    vmwrite(vm_execution_controls_cpu, vmread(vm_execution_controls_cpu) | SECONDARY_EXECUTION_CONTROLS); //activate secondary controls



    sendstringf("IA32_VMX_SECONDARY_PROCBASED_CTLS=%6\n", IA32_VMX_SECONDARY_PROCBASED_CTLS);

    if ((IA32_VMX_SECONDARY_PROCBASED_CTLS>>32) & SPBEF_ENABLE_EPT)
    {
      DWORD secondarycontrols=vmread(vm_execution_controls_cpu_secondary);

      sendstringf("SPBEF_ENABLE_EPT can be set\n");
      secondarycontrols=secondarycontrols | SPBEF_ENABLE_EPT;

      if ((IA32_VMX_SECONDARY_PROCBASED_CTLS>>32) & SPBEF_ENABLE_VPID)
      {
    	  sendstringf("SPBEF_ENABLE_VPID can also be set\n");
    	  secondarycontrols=secondarycontrols | SPBEF_ENABLE_VPID;
    	  vmwrite(vm_vpid,1); //vpid

    	  hasVPIDSupport=1;
      }

      vmwrite(vm_execution_controls_cpu_secondary, secondarycontrols);

      //setup the EPT ptr

      PEPT_PML4E pml4map=malloc2(4096);
      if (pml4map==NULL)
      {
        sendstringf("failure allocating pml4map");
        return 0;
      }
      else
        zeromemory(pml4map,4096);

      QWORD pml4mapPA=VirtualToPhysical(pml4map);
      currentcpuinfo->EPTPML4=pml4mapPA;

      sendstringf("pml4map is at %6\n", pml4mapPA);


      TIA32_VMX_VPID_EPT_CAP eptinfo;
      eptinfo.IA32_VMX_VPID_EPT_CAP=readMSR(IA32_VMX_EPT_VPID_CAP_MSR);

      QWORD eptp=pml4mapPA;
      PEPTP x=(PEPTP)&eptp;
      x->PAGEWALKLENGTH=3;

      if (eptinfo.EPT_writeBackSupport)
        x->MEMTYPE=6;
      else
        x->MEMTYPE=0;


      vmwrite(vm_eptpointer, eptp);  //and set the EPTP field


      has_EPT_1GBsupport=eptinfo.EPT_1GBSupport;
      has_EPT_2MBSupport=eptinfo.EPT_2MBSupport;
      has_EPT_ExecuteOnlySupport=eptinfo.EPT_executeOnlySupport;

      has_EPT_INVEPTSingleContext=eptinfo.EPT_INVEPTSingleContext;
      has_EPT_INVEPTAllContext=eptinfo.EPT_INVEPTAllContext;

      has_VPID_INVVPIDIndividualAddress=eptinfo.VPID_INVVPIDIndividualAddress;
      has_VPID_INVVPIDSingleContext=eptinfo.VPID_INVVPIDSingleContext;
      has_VPID_INVVPIDAllContext=eptinfo.VPID_INVVPIDAllContext;
      has_VPID_INVVPIDSingleContextRetainingGlobals=eptinfo.VPID_INVVPIDAllContext;

      //eptinfo

      QWORD rax=1,rbx=0,rcx=0,rdx=0;
      _cpuid(&rax, &rbx, &rcx, &rdx);

      if (rdx & (1 << 12))
      {
        hasMTRRsupport=1;

        MTRRCapabilities.Value=readMSR(IA32_MTRRCAP_MSR);
        MTRRDefType.Value=readMSR(IA32_MTRR_DEF_TYPE_MSR);

        //setup callbacks for MTRR msr edits

        /*
        vmx_setMSRWriteExit(IA32_MTRR_PHYSBASE0);
        vmx_setMSRWriteExit(IA32_MTRR_PHYSBASE1);
        */

        initMemTypeRanges();
      }


      return 1;
    }
    else
      sendstring("That means that SPBEF_ENABLE_EPT(bit 1) can not be set\n");

  }
  else
  {
    sendstring("This cpu does not support secondary procbased controls\n");
  }

  return 0;
}

void setup8086WaitForSIPI(pcpuinfo currentcpuinfo, int setupvmcontrols)
{
  //8086 entry (wait-for-sipi)
  Access_Rights reg_csaccessrights,reg_segaccessrights;
  DWORD gdtbase, idtbase;

  sendstringf("entering sleepmode for ap cpu\n");

  //todo: use unrestricted mode if possible

  if (hasUnrestrictedSupport)
  { //ring0 segments
    reg_csaccessrights.AccessRights=0;
    reg_csaccessrights.Segment_type=11;
    reg_csaccessrights.S=1;
    reg_csaccessrights.DPL=0;
    reg_csaccessrights.P=1;
    reg_csaccessrights.G=0;
    reg_csaccessrights.D_B=0;
    reg_csaccessrights.L=0;
    reg_csaccessrights.unusable=0;

    reg_segaccessrights.AccessRights=0;
    reg_segaccessrights.Segment_type=3;
    reg_segaccessrights.S=1;
    reg_segaccessrights.DPL=0;
    reg_segaccessrights.P=1;
    reg_segaccessrights.G=0;
    reg_segaccessrights.D_B=0;
    reg_segaccessrights.unusable=0;

  }
  else
  {
    gdtbase=VirtualToPhysical((void *)getGDTbase());
    idtbase=VirtualToPhysical(idttable32);

    reg_csaccessrights.AccessRights=0;
    reg_csaccessrights.Segment_type=3;
    reg_csaccessrights.S=1;
    reg_csaccessrights.DPL=3;
    reg_csaccessrights.P=1;
    reg_csaccessrights.G=0;
    reg_csaccessrights.D_B=0;
    reg_csaccessrights.unusable=0;

    reg_segaccessrights=reg_csaccessrights;
  }

  currentcpuinfo->guestCR3=0;
  currentcpuinfo->guestCR0=0;
  currentcpuinfo->hasIF=0;


  if (setupvmcontrols) //not needed when receiving an INIT, and todo: shouldn't be needed anymore as the cpu is already running
  {
    DWORD new_vm_execution_controls_cpu=vmread(vm_execution_controls_cpu) | (UINT64)IA32_VMX_PROCBASED_CTLS | USE_IO_BITMAPS | USE_MSR_BITMAPS;

    if ((IA32_VMX_PROCBASED_CTLS >> 32) & (1<<31)) //secondary procbased ctl support
      new_vm_execution_controls_cpu=new_vm_execution_controls_cpu | (1<<31);


    vmwrite(vm_execution_controls_cpu, new_vm_execution_controls_cpu);
    if ((new_vm_execution_controls_cpu >> 31) & 1)
    {
      //it has a secondary entry
      //enable rdtscp
      QWORD secondarycpu=vmread(vm_execution_controls_cpu_secondary);


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



    vmwrite(vm_entry_controls,vmread(vm_entry_controls) | (UINT64)IA32_VMX_ENTRY_CTLS ); //32bit/16bit init

    vmwrite(vm_cr0_read_shadow,0x10); //cr0 read shadow
    vmwrite(vm_cr4_read_shadow,(UINT64)0); //cr4 read shadow
    vmwrite(vm_cr3_targetvalue0,(UINT64)0xffffffffffffffffULL); //cr3-target value 0
  }





  if (hasUnrestrictedSupport)
  {
    vmwrite(vm_guest_cr3, 0);
    vmwrite(vm_guest_cr0, 0x10 | (IA32_VMX_CR0_FIXED0 & 0xFFFFFFFF7FFFFFFEULL)); //no pg, or PE
    vmwrite(vm_guest_cr4, IA32_VMX_CR4_FIXED0);

    vmwrite(vm_guest_gdtr_base, 0);
    vmwrite(vm_guest_gdt_limit, 0xffff);
    vmwrite(vm_guest_idtr_base, 0);
    vmwrite(vm_guest_idt_limit, 0xffff);
  }
  else
  {
    vmwrite(vm_guest_cr0,(UINT64)IA32_VMX_CR0_FIXED0 | CR0_WP); //guest cr0
    vmwrite(vm_guest_cr4,(UINT64)IA32_VMX_CR4_FIXED0 | CR4_VME | CR4_PSE | CR4_PAE); //guest cr4
    vmwrite(vm_guest_gdtr_base, gdtbase);
    vmwrite(vm_guest_gdt_limit, getGDTsize());
    vmwrite(vm_guest_idtr_base, idtbase);
    vmwrite(vm_guest_idt_limit, 256*8);

  }




  vmwrite(vm_guest_es,(UINT64)0); //es selector
  vmwrite(vm_guest_es_limit,(UINT64)0xffff); //es limit
  vmwrite(vm_guest_es_base,(UINT64)0); //es base
  vmwrite(vm_guest_es_access_rights,(UINT64)reg_segaccessrights.AccessRights); //es access rights

  vmwrite(vm_guest_ss,(UINT64)0); //ss selector
   vmwrite(vm_guest_ss_limit,(UINT64)0xffff); //ss limit
   vmwrite(vm_guest_ss_base,(UINT64)0); //ss base
   vmwrite(vm_guest_ss_access_rights,(UINT64)reg_segaccessrights.AccessRights); //ss access rights

   vmwrite(vm_guest_ds,(UINT64)0); //ds selector
   vmwrite(vm_guest_ds_limit,(UINT64)0xffff); //ds limit
   vmwrite(vm_guest_ds_base,(UINT64)0); //ds base
   vmwrite(vm_guest_ds_access_rights,(UINT64)reg_segaccessrights.AccessRights); //ds access rights

   vmwrite(vm_guest_fs,(UINT64)0); //fs selector
   vmwrite(vm_guest_fs_limit,(UINT64)0xffff); //fs limit
   vmwrite(vm_guest_fs_base,(UINT64)0); //fs base
   vmwrite(vm_guest_fs_access_rights,(UINT64)reg_segaccessrights.AccessRights); //fs access rights

   vmwrite(vm_guest_gs,(UINT64)0); //gs selector
   vmwrite(vm_guest_gs_limit,(UINT64)0xffff); //gs limit
   vmwrite(vm_guest_gs_base,(UINT64)0); //gs base
   vmwrite(vm_guest_gs_access_rights,(UINT64)reg_segaccessrights.AccessRights); //gs access rights


  vmwrite(vm_guest_cs,(UINT64)0); //cs selector
  vmwrite(vm_guest_cs_limit,(UINT64)0xffff); //cs limit
  if (hasUnrestrictedSupport)
    vmwrite(vm_guest_cs_base,(UINT64)0xffff0000); //cs base
  else
    vmwrite(vm_guest_cs_base,(UINT64)0xf0000); //cs base
  vmwrite(vm_guest_cs_access_rights,(UINT64)reg_segaccessrights/*reg_csaccessrights*/.AccessRights); //cs access rights


  vmwrite(vm_guest_ldtr,(UINT64)0); //ldtr selector
  if (hasUnrestrictedSupport)
    vmwrite(vm_guest_ldtr_limit,(UINT64)0xffff); //ldtr limit
  else
    vmwrite(vm_guest_ldtr_limit,(UINT64)0); //ldtr limit
  vmwrite(vm_guest_ldtr_base,(UINT64)0); //ldtr base
  if (hasUnrestrictedSupport)
    vmwrite(vm_guest_ldtr_access_rights,0x82); //ldtr access rights
  else
    vmwrite(vm_guest_ldtr_access_rights,(UINT64)(1<<16)); //ldtr access rights (bit 16 is unusable bit)

  vmwrite(vm_guest_tr,0); //the tss selector

  if (hasUnrestrictedSupport)
    vmwrite(vm_guest_tr_limit,0xffff); //tr limit
  else
    vmwrite(vm_guest_tr_limit,(ULONG)sizeof(TSS)+32+8192+1); //tr limit

  if (hasUnrestrictedSupport)
    vmwrite(vm_guest_tr_base,0); //tr basebase
  else
    vmwrite(vm_guest_tr_base,(UINT64)VirtualToPhysical(VirtualMachineTSS_V8086)); //tr basebase

  vmwrite(vm_guest_tr_access_rights,0x8b); //tr access rights



  currentcpuinfo->efer=0;
  vmwrite(vm_entry_controls, vmread(vm_entry_controls) & (~VMENTRYC_IA32E_MODE_GUEST));
  vmwrite(vm_guest_IA32_EFER, vmread(vm_guest_IA32_EFER) & ~(1<<8)); //disable EFER.LME
  vmwrite(vm_guest_IA32_EFER, vmread(vm_guest_IA32_EFER) & ~(1<<10)); //disable EFER.LMA








  currentcpuinfo->RealMode.IDTBase=0;
  currentcpuinfo->RealMode.GDTBase=0;
  currentcpuinfo->RealMode.IDTLimit=0x400;
  currentcpuinfo->RealMode.GDTBase=0;

  vmwrite(vm_guest_IA32_SYSENTER_CS,(UINT64)0);
  vmwrite(vm_guest_IA32_SYSENTER_ESP,(UINT64)0);
  vmwrite(vm_guest_IA32_SYSENTER_EIP,(UINT64)0);

  vmwrite(vm_guest_dr7,(UINT64)0x400); //dr7

  RFLAGS guestrflags;


  vmwrite(vm_guest_rsp,(UINT64)0); //rsp
  vmwrite(vm_guest_rip,(UINT64)0); //rip //should never be executed

  guestrflags.value=0;
  guestrflags.reserved1=1;
  if (hasUnrestrictedSupport==0)
  {
    guestrflags.IOPL=3;
    guestrflags.VM=1;

    setupNonPagedPaging(currentcpuinfo);
  }
  else
  {
    vmwrite(vm_guest_cr3,0);
  }




  vmwrite(vm_guest_rflags,guestrflags.value ); //rflag
  vmwrite(vm_guest_activity_state,(UINT64)3); //guest activity state, wait for sipi
}

void vmx_setMSRReadExit(DWORD msrValue)
{
  if (isAMD)
  {
    /*
    The MSR permissions bitmap consists of four separate bit vectors of 16
Kbits (2 Kbytes) each. Each 16 Kbit vector controls guest access to a defined range of 8K MSRs. Each
MSR is covered by two bits defining the guest read and write access permissions. The lsb of the two
bits controls read access to the MSR and the msb controls write access. A value of 1 indicates that the
operation is intercepted. The four separate bit vectors must be packed together and located in two
contiguous physical pages of memory. If the MSR_PROT intercept is active any attempt to read or
write an MSR not covered by the MSRPM will automatically cause an intercept.

MSRPM Byte Offset   MSR Range
000h–7FFh           0000_0000h–0000_1FFFh
800h–FFFh           C000_0000h–C000_1FFFh
1000h–17FFh         C001_0000h–C001_1FFFh
1800h–1FFFh         Reserved
     */
    if (msrValue<=0x1fff)
    {
      MSRBitmap[(msrValue*2)/8]|=1 << ((msrValue*2) % 8);
      return;
    }

    if ((msrValue>=0xc0000000) && (msrValue<=0xc0001fff))
    {
      msrValue=msrValue-0xc0000000;
      MSRBitmap[0x800+(msrValue*2)/8]|=1 << ((msrValue*2) % 8);
      return;
    }

    if ((msrValue>=0xc0010000) && (msrValue<=0xc0011fff))
    {
      msrValue=msrValue-0xc0010000;
      MSRBitmap[0x800+(msrValue*2)/8]|=1 << ((msrValue*2) % 8);
      return;
    }
  }
  else
  {
    if (msrValue<0xc0000000)
      MSRBitmap[msrValue/8]|=1 << (msrValue % 8);
    else
    {
      msrValue=msrValue-0xc0000000;
      MSRBitmap[1024+msrValue/8]|=1 << (msrValue % 8);
    }
  }
}

void vmx_removeMSRReadExit(DWORD msrValue)
{
  if (isAMD)
  {
    if (msrValue<=0x1fff)
    {
      MSRBitmap[(msrValue*2)/8]&=~(1 << ((msrValue*2) % 8));
      return;
    }

    if ((msrValue>=0xc0000000) && (msrValue<=0xc0001fff))
    {
      msrValue=msrValue-0xc0000000;
      MSRBitmap[0x800+(msrValue*2)/8]&=~(1 << ((msrValue*2) % 8));
      return;
    }

    if ((msrValue>=0xc0010000) && (msrValue<=0xc0011fff))
    {
      msrValue=msrValue-0xc0010000;
      MSRBitmap[0x800+(msrValue*2)/8]&=~(1 << ((msrValue*2) % 8));
      return;
    }
  }
  else
  {
    if (msrValue<0xc0000000)
      MSRBitmap[msrValue/8]&=~(1 << (msrValue % 8));
    else
    {
      msrValue=msrValue-0xc0000000;
      MSRBitmap[1024+msrValue/8]&=~(1 << (msrValue % 8));
    }
  }
}

void vmx_setMSRWriteExit(DWORD msrValue)
{
  if (isAMD)
  {
    if (msrValue<=0x1fff)
    {
      MSRBitmap[(msrValue*2)/8]|=2 << ((msrValue*2) % 8);
      return;
    }

    if ((msrValue>=0xc0000000) && (msrValue<=0xc0001fff))
    {
      msrValue=msrValue-0xc0000000;
      MSRBitmap[0x800+(msrValue*2)/8]|=2 << ((msrValue*2) % 8);
      return;
    }

    if ((msrValue>=0xc0010000) && (msrValue<=0xc0011fff))
    {
      msrValue=msrValue-0xc0010000;
      MSRBitmap[0x800+(msrValue*2)/8]|=2 << ((msrValue*2) % 8);
      return;
    }
  }
  else
  {
    if (msrValue<0xc0000000)
      MSRBitmap[2048+msrValue/8]|=1 << (msrValue % 8);
    else
    {
      msrValue=msrValue-0xc0000000;
      MSRBitmap[3072+msrValue/8]|=1 << (msrValue % 8);
    }
  }
}

void vmx_removeMSRWriteExit(DWORD msrValue)
{
  if (isAMD)
  {
    if (msrValue<=0x1fff)
    {
      MSRBitmap[(msrValue*2)/8]&=~(2 << ((msrValue*2) % 8));
      return;
    }

    if ((msrValue>=0xc0000000) && (msrValue<=0xc0001fff))
    {
      msrValue=msrValue-0xc0000000;
      MSRBitmap[0x800+(msrValue*2)/8]&=~(2 << ((msrValue*2) % 8));
      return;
    }

    if ((msrValue>=0xc0010000) && (msrValue<=0xc0011fff))
    {
      msrValue=msrValue-0xc0010000;
      MSRBitmap[0x800+(msrValue*2)/8]&=~(2 << ((msrValue*2) % 8));
      return;
    }
  }
  else
  {
    if (msrValue<0xc0000000)
      MSRBitmap[2048+msrValue/8]&=~(1 << (msrValue % 8));
    else
    {
      msrValue=msrValue-0xc0000000;
      MSRBitmap[3072+msrValue/8]&=~(1 << (msrValue % 8));
    }
  }
}


void vmx_enableTSCHook(pcpuinfo currentcpuinfo)
{
  if (isAMD)
  {
    currentcpuinfo->vmcb->InterceptRDTSC=1;
    currentcpuinfo->vmcb->InterceptRDTSCP=1;
  }
  else
  {
    if ((readMSR(IA32_VMX_PROCBASED_CTLS_MSR)>>32) & RDTSC_EXITING)
      vmwrite(vm_execution_controls_cpu, vmread(vm_execution_controls_cpu) | RDTSC_EXITING);



  }

  vmx_setMSRReadExit(IA32_TIME_STAMP_COUNTER);
  vmx_setMSRWriteExit(IA32_TIME_STAMP_COUNTER);
  vmx_setMSRWriteExit(IA32_TSC_ADJUST);


  TSCHooked=1;
}

void vmx_disableTSCHook(pcpuinfo currentcpuinfo)
{
  if (useSpeedhack==0)
  {
    if (isAMD)
    {
      currentcpuinfo->vmcb->InterceptRDTSC=0;
    }
    else
    {
      if ((readMSR(IA32_VMX_PROCBASED_CTLS_MSR)>>32) & RDTSC_EXITING)
        vmwrite(vm_execution_controls_cpu, vmread(vm_execution_controls_cpu) & (QWORD)~(QWORD)RDTSC_EXITING);
    }

    vmx_removeMSRReadExit(IA32_TIME_STAMP_COUNTER);
    vmx_removeMSRWriteExit(IA32_TIME_STAMP_COUNTER);
    vmx_removeMSRWriteExit(IA32_TSC_ADJUST);

    TSCHooked=0;
  }
}

void setupVMX(pcpuinfo currentcpuinfo)
{

  Access_Rights reg_csaccessrights,reg_traccessrights;

  reg_traccessrights.AccessRights=0;
  reg_traccessrights.Segment_type=11; //11=32-bit 3=16-bit
  reg_traccessrights.S=0;
  reg_traccessrights.DPL=0;
  reg_traccessrights.P=1;
  reg_traccessrights.G=0;
  reg_traccessrights.D_B=1;


  csEnter(&setupVMX_lock);

  char *eptcsname=malloc(32);
  snprintf(eptcsname,64,"EPTPML4CS %d", currentcpuinfo->cpunr);

  currentcpuinfo->EPTPML4CS.name=eptcsname;


//  currentcpuinfo->AvailableVirtualAddress=(UINT64)(currentcpuinfo->cpunr+16) << 28;

  if (isAMD)
    return setupVMX_AMD(currentcpuinfo);


  if (MSRBitmap==NULL)
  {
    //setp the MSR bitmap (I'd like to know when a 64<->32 bit switch happens)
    MSRBitmap=malloc(4096);
    zeromemory(MSRBitmap,4096);

    //MSRBitmap layout:
    //0000-1023: Read bitmap for low MSRs
    //1024-2047: read bitmap for high  MSRs
    //2048-3071: write bitmap for low MSRs
    //3072-4095: Write bitmap for high MSRs

    //set it to break on msr's handling sysenter
    //read for sysenter

    vmx_setMSRReadExit(0x174);
    vmx_setMSRReadExit(0x175);
    vmx_setMSRReadExit(0x176);

    vmx_setMSRWriteExit(0x174);
    vmx_setMSRWriteExit(0x175);
    vmx_setMSRWriteExit(0x176);


    vmx_setMSRReadExit(0xc0000080);
    vmx_setMSRWriteExit(0xc0000080);

    //break on IA32_FEATURE_CONTROL_MSR read and write
    vmx_setMSRReadExit(IA32_FEATURE_CONTROL_MSR);
    vmx_setMSRWriteExit(IA32_FEATURE_CONTROL_MSR);





    /* todo perhaps
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

  SetPageToWriteThrough((void *)MSRBitmap); //for future updates


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

  //sendstring("Setting up realmode paging\n"); //also for loadedOS in case of some weird event
  //setupRealModePaging(currentcpuinfo);


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

  //compatibility mode with newer cpus that have 0 settings for features that I expect are 1 (for cpu's that didn't have the TRUE feature)
  if ((IA32_VMX_PINBASED_CTLS >> 32) & (1<<1))
    IA32_VMX_PINBASED_CTLS|=(1<<1);

  if ((IA32_VMX_PINBASED_CTLS >> 32) & (1<<2))
    IA32_VMX_PINBASED_CTLS|=(1<<2);

  if ((IA32_VMX_PINBASED_CTLS >> 32) & (1<<4))
    IA32_VMX_PINBASED_CTLS|=(1<<4);


  //proc based : 1, 4-6, 8, 13-16, 26
  if ((IA32_VMX_PROCBASED_CTLS >> 32) & (1<<1)) //1: CAN be on->MUST be one
    IA32_VMX_PROCBASED_CTLS|=(1<<1);
  else
  {
    sendstring("Fail1\n");
  }

  if (((IA32_VMX_PROCBASED_CTLS >> 32) & (7<<4)) ==(7<<4)) //4-6: CAN be on->MUST be one
      IA32_VMX_PROCBASED_CTLS|=(7<<4);
  else
  {
      sendstring("Fail2\n");
  }

  if ((IA32_VMX_PROCBASED_CTLS >> 32) & (1<<8)) //8
    IA32_VMX_PROCBASED_CTLS|=(1<<8);
  else
  {
    sendstring("Fail3\n");
  }

  if (((IA32_VMX_PROCBASED_CTLS >> 32) & (0xf<<13)) ==(0xf<<13)) //13,14,15,16
      IA32_VMX_PROCBASED_CTLS|=(0xf<<13);
  else
  {
    sendstring("Fail4\n");
  }

  if ((IA32_VMX_PROCBASED_CTLS >> 32) & (1<<26))
    IA32_VMX_PROCBASED_CTLS|=(1<<26);
  else
  {
    sendstring("Fail5\n");
  }


  sendstringf("%d: Initializing vmcs region for launch\n\r",currentcpuinfo->cpunr);


  //32-bit control fields
  vmwrite(vm_execution_controls_pin,(ULONG)IA32_VMX_PINBASED_CTLS); //pin-based VM-execution controls


  sendstringf("Set vm_execution_controls_pin to %8 (became %8)\n", (ULONG)IA32_VMX_PINBASED_CTLS, (DWORD)vmread(vm_execution_controls_pin));


#if DISPLAYDEBUG==1
  //check if the system supports preemption, and if so, enable it
  {
    ULONG usablepinbasedBits=(IA32_VMX_PINBASED_CTLS >> 32);
    if (usablepinbasedBits & ACTIVATE_VMX_PREEMPTION_TIMER)
    {
      displayline("Preemption is possible\n");
      vmwrite(vm_execution_controls_pin,vmread(vm_execution_controls_pin) | ACTIVATE_VMX_PREEMPTION_TIMER);
      vmwrite(vm_preemption_timer_value,10000);
    }
  }
#endif


  globalTSC=_rdtsc();




  vmwrite(vm_exception_bitmap,(UINT64)0xffff); //exception bitmap (0xffff=0-15 0xffffffff=0-31)
 // vmwrite(vm_exception_bitmap,(1<<1) | (1<<3) | (1<<14));


  vmwrite(0x4006,(UINT64)0); //page fault error-code mask
  vmwrite(0x4008,(UINT64)0); //page fault error-code match
  vmwrite(0x400a,(UINT64)1); //cr3-target count

  DWORD new_vm_exit_controls=(DWORD)IA32_VMX_EXIT_CTLS;

  sendstringf("IA32_VMX_EXIT_CTLS=%6\n", IA32_VMX_EXIT_CTLS);


  if ((IA32_VMX_EXIT_CTLS >> 32) & VMEXITC_HOST_ADDRESS_SPACE_SIZE)
    new_vm_exit_controls|=VMEXITC_HOST_ADDRESS_SPACE_SIZE;
  else
  {
    sendstring("<<<<<<WARNING: This system does not support HOST_ADDRESS_SPACE_SIZE>>>>>>\n");
  }



  if ((IA32_VMX_EXIT_CTLS >> 32) & VMEXITC_ACKNOWLEDGE_INTERRUPT_ON_EXIT)
    new_vm_exit_controls|=VMEXITC_ACKNOWLEDGE_INTERRUPT_ON_EXIT;
  else
  {
    sendstring("<<<<<<WARNING: This system does not support acknowledge interrupt on exit>>>>>>\n");
  }


  if ((IA32_VMX_EXIT_CTLS >> 32) & VMEXITC_SAVE_DEBUG_CONTROLS)
    new_vm_exit_controls|=VMEXITC_SAVE_DEBUG_CONTROLS;
  else
  {
    sendstring("<<<<<<WARNING: This system does not support saving debug controls>>>>>>\n");
  }

  //new_vm_exit_controls=new_vm_exit_controls & (QWORD)~(QWORD)SAVE_DEBUG_CONTROLS; //debug: Test with debug saving off




  vmwrite(vm_exit_controls, new_vm_exit_controls); //vm-exit controls , Host address-space size = 1

  sendstringf("Set vm_exit_controls to %8 (became %8)\n", new_vm_exit_controls, (DWORD)vmread(vm_exit_controls));


  vmwrite(0x400e,(UINT64)0); //vm-exit msr-store count
  vmwrite(0x4010,(UINT64)0); //vm-exit msr-load count


  vmwrite(0x4014,(UINT64)0); //vm-entry msr-load count
  vmwrite(vm_entry_interruptioninfo,(UINT64)0); //vm-entry interruption-information field
  vmwrite(0x4018,(UINT64)0); //vm-entry exception error code
  vmwrite(0x401a,(UINT64)0); //vm-entry instruction length
  vmwrite(0x401c,(UINT64)0); //TPR threshold


  //64-bit control fields
  vmwrite(vm_iobitmap_a,(UINT64)VirtualToPhysical((void *)IOBitmap)); //IO Bitmap A
  vmwrite(vm_iobitmap_b,(UINT64)VirtualToPhysical((void *)&(IOBitmap[4096]))); //IO Bitmap B
  vmwrite(0x2004,(UINT64)VirtualToPhysical((void *)MSRBitmap)); //MSR bitmap
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

  //if useEPT  (the user might want to save that memory)
  hasEPTsupport=setupEPT(currentcpuinfo); //needed for unrestricted guest and could be useful for other things (like protecting the memory of DBVM)


  if (hasEPTsupport)
  {
    //try setting unrestricted guest
    if ( ((IA32_VMX_SECONDARY_PROCBASED_CTLS>>32) & SPBEF_ENABLE_UNRESTRICTED ) &&
         ((readMSR(IA32_VMX_ENTRY_CTLS_MSR) >> 32) & VMENTRYC_LOAD_IA32_EFER) &&
         ((readMSR(IA32_VMX_EXIT_CTLS_MSR) >> 32) & VMEXITC_SAVE_IA32_EFER) &&
         ((readMSR(IA32_VMX_EXIT_CTLS_MSR) >> 32) & VMEXITC_LOAD_IA32_EFER) )
    {
      //can be 1
      sendstringf("Enabling unrestricted guest\n");
      hasUnrestrictedSupport=1;
      vmwrite(vm_execution_controls_cpu_secondary, vmread(vm_execution_controls_cpu_secondary) | SPBEF_ENABLE_UNRESTRICTED);

      //set load IA32_EFER vmentry control (should be possible, but check anyhow)


      vmwrite(vm_entry_controls, vmread(vm_entry_controls) | VMENTRYC_LOAD_IA32_EFER );
      vmwrite(vm_exit_controls, vmread(vm_exit_controls) | VMEXITC_SAVE_IA32_EFER );
      vmwrite(vm_exit_controls, vmread(vm_exit_controls) | VMEXITC_LOAD_IA32_EFER );

      vmwrite(vm_host_IA32_EFER, readMSR(EFER_MSR));

      //should be able to work without having to watch the EFER msr (undo those bits)
      MSRBitmap[1024+0x80/8]&=~(1 << (0x80 % 8)); //read
      MSRBitmap[3072+0x80/8]&=~(1 << (0x80 % 8)); //write

      //vmwrite(vm_cr0_guest_host_mask,(UINT64)IA32_VMX_CR0_FIXED0 & 0xFFFFFFFF7FFFFFFEULL); //cr0 guest/host mask 1=guest owned
      vmwrite(vm_cr0_guest_host_mask,(UINT64)0xFFFFFFFF7FFFFFFEULL);
      vmwrite(vm_cr4_guest_host_mask,(UINT64)IA32_VMX_CR4_FIXED0); //same with cr4 but do guard the VMX bit


      //needs less interrupt hooks
      vmwrite(vm_exception_bitmap,  (1<<1) | (1<<3));

      //todo: check if it can do with less cr3 exits  (can turn that on at runtime)
      //check the primary procbased capabilities if it can be set to 0

      sendstringf("Checking if it supports CR3 access exit to 0\n");


      QWORD procbasedcapabilities;
      if (readMSR(IA32_VMX_BASIC_MSR) & ((QWORD)1<<55))
        procbasedcapabilities=readMSR(IA32_VMX_TRUE_PROCBASED_CTLS_MSR);
      else
        procbasedcapabilities=readMSR(IA32_VMX_PROCBASED_CTLS_MSR);

      sendstringf("procbasedcapabilities=%6\n", procbasedcapabilities);

      canToggleCR3Exit=((procbasedcapabilities & (PPBEF_CR3LOAD_EXITING | PPBEF_CR3STORE_EXITING))==0); //0 means it can be set to 0

      sendstringf("canToggleCR3Exit=%d\n", canToggleCR3Exit);

      if (canToggleCR3Exit) //turn of cr3 exits
        IA32_VMX_PROCBASED_CTLS = IA32_VMX_PROCBASED_CTLS & (QWORD)(~(PPBEF_CR3LOAD_EXITING | PPBEF_CR3STORE_EXITING));
    }
    else
    {
      sendstringf("Unrestricted guest is not supported\n");
      hasUnrestrictedSupport=0;
    }
  }




  //----------------GUEST SETUP----------------
 //UINT64 oldloadedos=loadedOS;

  //TODO: Split up into functions


  if (loadedOS)
  {
    sendstring("loadedos is set\n");
    sendstringf("APStartsInSIPI=%d\n", APStartsInSIPI);
    sendstringf("currentcpuinfo->cpunr=%d\n", currentcpuinfo->cpunr);

    if ((APStartsInSIPI==0) || (currentcpuinfo->cpunr==0))
    {
      //set osstate, for both boot and app cpu's (if APStartsInSIPI=0)
      POriginalState originalstate=(POriginalState)mapPhysicalMemory(loadedOS,sizeof(OriginalState));
      PGDT_ENTRY gdt=NULL,ldt=NULL;
      ULONG ldtselector=originalstate->ldt;
      int notpaged;
      RFLAGS rflags;

      sendstringf("Setting up guest based on loadedOS settings\n");

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



      DWORD new_vm_execution_controls_cpu=vmread(vm_execution_controls_cpu) | (DWORD)IA32_VMX_PROCBASED_CTLS | USE_IO_BITMAPS | USE_MSR_BITMAPS;

      if (IA32_VMX_PROCBASED_CTLS & INVLPG_EXITING) //NEEDS to set INVLPG_EXITING
        new_vm_execution_controls_cpu|=INVLPG_EXITING;


      if ((IA32_VMX_PROCBASED_CTLS >> 32) & (1<<31)) //secondary procbased ctl support
        new_vm_execution_controls_cpu=new_vm_execution_controls_cpu | (1<<31);





      vmwrite(vm_execution_controls_cpu, new_vm_execution_controls_cpu); //processor-based vm-execution controls
      sendstringf("Set vm_execution_controls_cpu to %8 (became %8)\n", new_vm_execution_controls_cpu, (DWORD)vmread(vm_execution_controls_cpu));

      if ((new_vm_execution_controls_cpu >> 31) & 1)
      {
        //it has a secondary entry
        //enable rdtscp
        QWORD secondarycpu=vmread(vm_execution_controls_cpu_secondary);


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

      if (hasUnrestrictedSupport)
        vmwrite(vm_guest_IA32_EFER, originalstate->originalEFER);


      DWORD new_vm_entry_controls=vmread(vm_entry_controls);

      if (originalstate->originalLME)
      {
        sendstringf("guest is 64bit\n");
        currentcpuinfo->efer|=(1<<8) | (1<<10);
        new_vm_entry_controls=new_vm_entry_controls | (DWORD)IA32_VMX_ENTRY_CTLS | VMENTRYC_RESTORE_DEBUG_CONTROLS | VMENTRYC_IA32E_MODE_GUEST; //64-bit mode
      }
      else
        new_vm_entry_controls=new_vm_entry_controls | (DWORD)IA32_VMX_ENTRY_CTLS | VMENTRYC_RESTORE_DEBUG_CONTROLS;

      //new_vm_entry_controls=new_vm_entry_controls & (QWORD)~(QWORD)RESTORE_DEBUG_CONTROLS; //debug: Test with debug saving off


      vmwrite(vm_entry_controls, new_vm_entry_controls);
      sendstringf("Set vm_entry_controls to %8 (became %8)\n", new_vm_entry_controls, (DWORD)vmread(vm_entry_controls));



      if (hasUnrestrictedSupport)
      {
        //if my assumption is correct only the bits masking the guest/host mask will be read from this
        vmwrite(vm_cr0_read_shadow,originalstate->cr0 & vmread(vm_cr0_guest_host_mask)); //cr0 read shadow
        vmwrite(vm_cr4_read_shadow,originalstate->cr4 & vmread(vm_cr4_guest_host_mask)); //cr4 read shadow
      }
      else
      {
        vmwrite(vm_cr0_read_shadow,originalstate->cr0); //cr0 read shadow
        vmwrite(vm_cr4_read_shadow,originalstate->cr4); //cr4 read shadow
      }
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
      gdt=(PGDT_ENTRY)(UINT64)mapPhysicalMemory(getPhysicalAddressVM(currentcpuinfo, originalstate->gdtbase, &notpaged), originalstate->gdtlimit);

      ULONG ldtlimit;
      if ((UINT64)originalstate->ldt)
      {
        UINT64 ldtbase; //should be 0 in 64bit


        sendstring("ldt is valid, so getting the information\n\r");

        ldtbase=(gdt[(ldtselector >> 3)].Base24_31 << 24) + gdt[(ldtselector >> 3)].Base0_23;
        ldtlimit=(gdt[(ldtselector >> 3)].Limit16_19 << 16) + gdt[(ldtselector >> 3)].Limit0_15;
        ldt=(PGDT_ENTRY)(UINT64)mapPhysicalMemory(getPhysicalAddressVM(currentcpuinfo, ldtbase, &notpaged), ldtlimit);
      }


      vmwrite(vm_guest_es,(UINT64)originalstate->es); //es selector
      vmwrite(vm_guest_es_access_rights,originalstate->es_AccessRights);
      vmwrite(vm_guest_es_limit,originalstate->es_Limit);

      vmwrite(vm_guest_cs,(UINT64)originalstate->cs); //cs selector
      vmwrite(vm_guest_cs_access_rights,originalstate->cs_AccessRights);
      vmwrite(vm_guest_cs_limit,originalstate->cs_Limit);

      vmwrite(vm_guest_ss,(UINT64)originalstate->ss); //ss selector
      vmwrite(vm_guest_ss_access_rights,originalstate->ss_AccessRights);
      vmwrite(vm_guest_ss_limit,originalstate->ss_Limit);

      vmwrite(vm_guest_ds,(UINT64)originalstate->ds); //ds selector
      vmwrite(vm_guest_ds_access_rights,originalstate->ds_AccessRights);
      vmwrite(vm_guest_ds_limit,originalstate->ds_Limit);

      vmwrite(vm_guest_fs,(UINT64)originalstate->fs); //fs selector
      vmwrite(vm_guest_fs_access_rights,originalstate->fs_AccessRights);
      vmwrite(vm_guest_fs_limit,originalstate->fs_Limit);

      vmwrite(vm_guest_gs,(UINT64)originalstate->gs); //gs selector
      vmwrite(vm_guest_gs_access_rights,originalstate->gs_AccessRights);
      vmwrite(vm_guest_gs_limit,originalstate->gs_Limit);

      vmwrite(vm_guest_ldtr,(UINT64)originalstate->ldt); //ldtr selector
      vmwrite(vm_guest_tr,(UINT64)originalstate->tr); //tr selector


      if (originalstate->originalLME)
      {
        sendstringf("64-bit\n");
        vmwrite(vm_guest_cs_base,0);
        vmwrite(vm_guest_ss_base,0);
        vmwrite(vm_guest_ds_base,0);
        vmwrite(vm_guest_es_base,0);
        vmwrite(vm_guest_fs_base,originalstate->fsbase);
        vmwrite(vm_guest_gs_base,originalstate->gsbase);

        sendstringf("originalstate->fsbase=%6\n", originalstate->fsbase);
        sendstringf("originalstate->gsbase=%6\n", originalstate->gsbase);
        sendstringf("Have set fs base to %6 and gs base to %6\n",vmread(vm_guest_fs_base),vmread(vm_guest_gs_base));

        vmwrite(vm_guest_tr_base,getSegmentBaseEx(gdt,ldt,originalstate->tr, 1));
      }
      else
      {
        sendstringf("32-bit\n");
        vmwrite(vm_guest_cs_base,getSegmentBase(gdt,ldt,originalstate->cs));
        vmwrite(vm_guest_ss_base,getSegmentBase(gdt,ldt,originalstate->ss));
        vmwrite(vm_guest_ds_base,getSegmentBase(gdt,ldt,originalstate->ds));
        vmwrite(vm_guest_es_base,getSegmentBase(gdt,ldt,originalstate->es));
        vmwrite(vm_guest_fs_base,getSegmentBase(gdt,ldt,originalstate->fs));
        vmwrite(vm_guest_gs_base,getSegmentBase(gdt,ldt,originalstate->gs));
        vmwrite(vm_guest_tr_base,getSegmentBase(gdt,ldt,originalstate->tr));
      }


      vmwrite(vm_guest_ldtr_limit,getSegmentLimit(gdt,ldt,originalstate->ldt));
      if (originalstate->tr==0)
        vmwrite(vm_guest_tr_limit,0xffff);
      else
        vmwrite(vm_guest_tr_limit,getSegmentLimit(gdt,ldt,originalstate->tr));

      vmwrite(vm_guest_ldtr_base,getSegmentBase(gdt,ldt,originalstate->ldt));
      vmwrite(vm_guest_ldtr_access_rights,getSegmentAccessRights(gdt,ldt,originalstate->ldt));

      if (originalstate->tr)
        vmwrite(vm_guest_tr_access_rights,getSegmentAccessRights(gdt,ldt,originalstate->tr));
      else
        vmwrite(vm_guest_tr_access_rights,0x8b);

      vmwrite(vm_guest_IA32_SYSENTER_CS,(UINT64)readMSR(IA32_SYSENTER_CS_MSR));
      vmwrite(vm_guest_IA32_SYSENTER_ESP,(UINT64)readMSR(IA32_SYSENTER_ESP_MSR));
      vmwrite(vm_guest_IA32_SYSENTER_EIP,(UINT64)readMSR(IA32_SYSENTER_EIP_MSR));

      currentcpuinfo->actual_sysenter_CS=vmread(vm_guest_IA32_SYSENTER_CS);
      currentcpuinfo->actual_sysenter_ESP=vmread(vm_guest_IA32_SYSENTER_ESP);
      currentcpuinfo->actual_sysenter_EIP=vmread(vm_guest_IA32_SYSENTER_EIP);



      vmwrite(vm_guest_dr7,(UINT64)originalstate->dr7); //dr7
      vmwrite(vm_guest_activity_state,(UINT64)0); //normal activity state
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



      if (gdt)
        unmapPhysicalMemory(gdt, originalstate->gdtlimit);

      if (ldt)
        unmapPhysicalMemory(ldt, ldtlimit);

      if (originalstate)
        unmapPhysicalMemory(originalstate, sizeof(OriginalState));
    }
    else
    {
      setup8086WaitForSIPI(currentcpuinfo, 1);
    }


  }
  else
  {
    sendstringf("booted of a disk, without UEFI, load os manually");


    if (currentcpuinfo->cpunr==0)
    {
      //not loaded from OS and first cpu
      sendstringf("manually load for cpu 0\n"); //setup so it runs
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




      DWORD new_vm_execution_controls_cpu=vmread(vm_execution_controls_cpu) | (DWORD)IA32_VMX_PROCBASED_CTLS;


      if ((IA32_VMX_PROCBASED_CTLS >> 32) & HLT_EXITING)
        new_vm_execution_controls_cpu|=HLT_EXITING;
      else
      {
        sendstring("<<<<<<WARNING: This system does not support HLT_EXITING>>>>>>\n");
      }

      if (IA32_VMX_PROCBASED_CTLS & INVLPG_EXITING) //NEEDS to set INVLPG_EXITING
        new_vm_execution_controls_cpu|=INVLPG_EXITING;

      /*
      if ((IA32_VMX_PROCBASED_CTLS >> 32) & USE_IO_BITMAPS)
        new_vm_execution_controls_cpu|=USE_IO_BITMAPS;
      else
        sendstring("<<<<<<WARNING: This system does not support USE_IO_BITMAPS>>>>>>\n");*/

      if ((IA32_VMX_PROCBASED_CTLS >> 32) & USE_MSR_BITMAPS)
        new_vm_execution_controls_cpu|=USE_MSR_BITMAPS;
      else
      {
        sendstring("<<<<<<WARNING: This system does not support USE_MSR_BITMAPS>>>>>>\n");
      }


      if ((IA32_VMX_PROCBASED_CTLS >> 32) & (1<<31)) //secondary procbased ctl support
        new_vm_execution_controls_cpu=new_vm_execution_controls_cpu | (1<<31);

      vmwrite(vm_execution_controls_cpu, new_vm_execution_controls_cpu); //processor-based vm-execution controls
      sendstringf("Set vm_execution_controls_cpu to %8 (became %8)\n", new_vm_execution_controls_cpu, (DWORD)vmread(vm_execution_controls_cpu));

      if ((new_vm_execution_controls_cpu >> 31) & 1)
      {
        //it has a secondary entry
        QWORD secondarycpu=vmread(vm_execution_controls_cpu_secondary);


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




      DWORD new_vm_entry_controls=vmread(vm_entry_controls) |  (DWORD)IA32_VMX_ENTRY_CTLS;

      if ((IA32_VMX_ENTRY_CTLS >> 32) & VMENTRYC_IA32E_MODE_GUEST)
        new_vm_entry_controls|=VMENTRYC_IA32E_MODE_GUEST;
      else
      {
        sendstring("<<<<<<WARNING: This system does not support IA32E_MODE_GUEST>>>>>>\n");
      }

      if ((IA32_VMX_ENTRY_CTLS >> 32) & VMENTRYC_RESTORE_DEBUG_CONTROLS)
        new_vm_entry_controls|=VMENTRYC_RESTORE_DEBUG_CONTROLS;
      else
      {
        sendstring("<<<<<<WARNING: This system does not support the RESTORE_DEBUG_CONTROLS vm_entry control option>>>>>>\n");
      }


      vmwrite(vm_entry_controls,new_vm_entry_controls); //vm-entry controls   bit9: ia-32e mode guest is guest controlled
      sendstringf("Set vm_entry_controls to %8 (became %8)\n", new_vm_entry_controls, (DWORD)vmread(vm_entry_controls));




      vmwrite(vm_cr0_read_shadow,(UINT64)getCR0()); //cr0 read shadow
      vmwrite(vm_cr4_read_shadow,(UINT64)getCR4()); //cr4 read shadow
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

      vmwrite(vm_guest_activity_state,(UINT64)0); //guest activity state, normal
      //vmwrite(vm_guest_rsp,(UINT64)0x8fffc); //rsp
      vmwrite(vm_guest_rsp,((UINT64)malloc(4096))+0x1000-0x28); //rsp, 32 bytes scratch and 8 bytes for return value (so unaligned)
      vmwrite(vm_guest_rip,(UINT64)reboot); //rip
      vmwrite(vm_guest_rflags,(UINT64)getRFLAGS()); //rflags


      if (hasUnrestrictedSupport)
      {
        vmwrite(vm_guest_IA32_EFER, readMSR(EFER_MSR));
        vmwrite(vm_guest_tr,0);
        vmwrite(vm_guest_tr_limit,0);
        vmwrite(vm_guest_tr_base, 0);
        vmwrite(vm_guest_tr_access_rights,0x8b);

        //map a low memory address for the inthook
        QWORD size=(QWORD)&realmode_inthooks_end-(QWORD)&realmode_inthooks;
        if (size<=0x800)
          size=0x800;
        else
        {
          size+=0x800;
          size=size & 0xFFFFFFFFFFFFF800ULL;
        }


        QWORD hookaddress;
        int i,lowregion=-1;
        for (i=0; fakeARD[i].Type != 255; i++)
        {
          if ((fakeARD[i].BaseAddrHigh==0) && (fakeARD[i].BaseAddrLow<(0x100000-size)) && (fakeARD[i].Type==1))
          {
            lowregion=i;
          }
        }

        if (lowregion==-1)
        {
          nosendchar[getAPICID()]=0;
          sendstringf("No low region:\n");
          sendARD();
          ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
          while (1) outportb(0x80,0xde);
        }


        hookaddress=fakeARD[lowregion].BaseAddrLow+((QWORD)(fakeARD[lowregion].LengthHigh)<<32)+fakeARD[lowregion].LengthLow;
        if (hookaddress>=0x100000)
        {
          //buggy ARD
          fakeARD_InsertRange(0x98000,size, 2);
        }
        else
        {
          hookaddress=hookaddress-0x800;
          fakeARD_InsertRange(hookaddress,size, 2);
        }

        //copy inthooks to hookaddress

        RMIDT *idt=mapPhysicalMemory(0, 0x3ff); //map the realmode IDT
        //store the original interrupt addresses (within the realmode_inthooks block)
        realmode_inthook_original12=*(DWORD*)&idt[0x12];
        realmode_inthook_original15=*(DWORD*)&idt[0x15];

        realmode_inthook_conventional_memsize=fakeARD_getConventionalMemory() / 1024;


        void *mappedhookaddress=mapPhysicalMemory(hookaddress, size);

        copymem(mappedhookaddress, realmode_inthooks, (QWORD)realmode_inthooks_end-(QWORD)realmode_inthooks);
        unmapPhysicalMemory(mappedhookaddress,size);


        //adjust relative addresses
        QWORD new12=hookaddress+(QWORD)realmode_inthook_new12-(QWORD)realmode_inthooks;
        QWORD new15=hookaddress+(QWORD)realmode_inthook_new15-(QWORD)realmode_inthooks;

        realmode_inthook_calladdressPA=hookaddress+(QWORD)realmode_inthook_calladdress-(QWORD)realmode_inthooks;

        sendstringf("new12=%8\n", new12);
        sendstringf("new15=%6\n", new15);

        //adjust int 0x12 and 0x15 to point to their realmode_inthook counterpart
        idt[0x12].segment=((new12 & 0xfff00) >> 4);
        idt[0x12].offset=(new12 & 0x000ff);

        idt[0x15].segment=((new15 & 0xfff00) >> 4);
        idt[0x15].offset=(new15 & 0x000ff);


        unmapPhysicalMemory(idt, 0x3ff);

        //setup realmode_inthook_conventional_memsize


        //vmwrite(vm_execution_controls_pin, vmread(vm_execution_controls_pin) | EXTERNAL_INTERRUPT_EXITING);

        //vmwrite(vm_exit_controls, vmread(vm_exit_controls) & (~(1<<15)));
        //Earlier i've set HLT exiting to 1, now i've changed my mind
        QWORD Real_IA32_VMX_PROCBASED_CTLS=IA32_VMX_PROCBASED_CTLS;

        if (IA32_VMX_BASIC.default1canbe0)
          Real_IA32_VMX_PROCBASED_CTLS=readMSR(IA32_VMX_TRUE_PROCBASED_CTLS_MSR);

        if ((Real_IA32_VMX_PROCBASED_CTLS & HLT_EXITING)==0) //can be 0
        {
          sendstring("Disable HLT exiting\n");
          vmwrite(vm_execution_controls_cpu, vmread(vm_execution_controls_cpu) & (~HLT_EXITING));
        }
      }
    }
    else
    {
      //8086 entry (todo: make obsolete)
      setup8086WaitForSIPI(currentcpuinfo, 1);
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
  vmwrite(vm_host_es,(UINT64)8); //es selector
  vmwrite(vm_host_cs,(UINT64)80); //cs selector
  vmwrite(vm_host_ss,(UINT64)8); //ss selector
  vmwrite(vm_host_ds,(UINT64)8); //ds selector
  vmwrite(vm_host_fs,(UINT64)8); //fs selector
  vmwrite(vm_host_gs,(UINT64)8); //gs selector
  vmwrite(vm_host_tr,(UINT64)96); //tr selector

  vmwrite(vm_host_IA32_SYSENTER_CS,(UINT64)readMSR(IA32_SYSENTER_CS_MSR));  //IA32_SYSENTER_CS
  vmwrite(vm_host_IA32_SYSENTER_ESP,(UINT64)readMSR(IA32_SYSENTER_ESP_MSR)); //sysenter_esp
  vmwrite(vm_host_IA32_SYSENTER_EIP,(UINT64)readMSR(IA32_SYSENTER_EIP_MSR)); //sysenter_eip



  vmwrite(vm_host_cr0,(UINT64)getCR0()); //cr0
  vmwrite(vm_host_cr3,(UINT64)getCR3()); //cr3
  vmwrite(vm_host_cr4,(UINT64)getCR4()); //cr4
  vmwrite(vm_host_fs_base,(UINT64)readMSR(0xc0000100));  //fs base
  vmwrite(vm_host_gs_base,(UINT64)readMSR(0xc0000101));  //gs base
  vmwrite(vm_host_tr_base,(UINT64)ownTSS);  //tr base
  vmwrite(vm_host_gdtr_base,(UINT64)getGDTbase()); //gdtr base
  vmwrite(vm_host_idtr_base,(UINT64)getIDTbase()); //idt base

  sendstring("Finished configuring\n\r");


  globals_have_been_configured=1;
  currentcpuinfo->vmxsetup=1;

  csLeave(&setupVMX_lock);
}
