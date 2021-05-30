/*
vmeventhandler.c: This will handle the events
*/

#include "vmpaging.h"
#include "vmeventhandler.h"
#include "vmmhelper.h"
#include "neward.h"

#include "vmevent_invalidstate.h"
#include "realmodeemu.h"
#include "main.h"
#include "mm.h"
#include "common.h"
#include "vmcall.h"

#include "realmodeemu.h"
#include "msrnames.h"
#include "vmxcontrolstructures.h"
#include "ultimap.h"
#include "vmxemu.h"
#include "epthandler.h"
#include "vmxsetup.h"
#include "displaydebug.h"


#ifndef DEBUG
#define sendstringf(s,x...)
#define sendstring(s)
#endif

criticalSection CR3ValueLogCS={.name="CR3ValueLogCS", .debuglevel=2};
QWORD *CR3ValueLog; //if not NULL, record
int CR3ValuePos;

//volatile QWORD TSCOffset=0;
volatile QWORD globalTSC;
volatile QWORD lowestTSC=0;

int adjustTimestampCounters=1;
int adjustTimestampCounterTimeout=2000;

int useSpeedhack=0;
double speedhackSpeed=1.0f;
QWORD speedhackInitialOffset=0;
QWORD speedhackInitialTime=0;


//QWORD cpuidTime=6000; //todo: Make this changeable by the user after launch, or instead of using a TSCOffset just tell the next rdtsc calls to difference of 30 or less (focussing on the currentcpu, or just for 6000 actual ticks)
//QWORD rdtscTime=6000;
//QWORD rdtscpTime=10;

criticalSection TSCCS={.name="TSCCS", .debuglevel=2};

int handle_rdtsc(pcpuinfo currentcpuinfo, VMRegisters *vmregisters);


void incrementRIP(int count)
{
  if (!isAMD) //not used by AMD, it gets the new RIP explicitly
  {
    QWORD newRIP=vmread(vm_guest_rip)+count;

    if (!IS64BITCODE(getcpuinfo()))
      newRIP=newRIP & 0xffffffff;

    vmwrite(vm_guest_rip,newRIP);
    vmwrite(vm_guest_interruptability_state, vmread(vm_guest_interruptability_state) & 0xfffffffc); //instruction emulated. Skip the block by ss and block by sti
  }
}



int raiseNMI(void)
{
  VMEntry_interruption_information newintinfo;
  sendstring("Raising NMI\n\r");

  newintinfo.interruption_information=0;
  newintinfo.interruptvector=2; //NMI
  newintinfo.type=2; //NMI
  newintinfo.haserrorcode=0;
  newintinfo.valid=1;

  vmwrite(vm_entry_interruptioninfo, newintinfo.interruption_information); //entry info field
  vmwrite(vm_entry_exceptionerrorcode, 0); //entry errorcode
  vmwrite(vm_entry_instructionlength, vmread(vm_exit_instructionlength)); //entry instruction length
  return 0;
}

int raisePMI()
{
  //get the pmi interrupt number from the APIC
  int interruptvector;
  DWORD LVT_Perfmon_Counter_Register;
  VMEntry_interruption_information newintinfo;

  if (readMSR(IA32_APICBASE_MSR) & (1<<10))  //x2apic
  {
    LVT_Perfmon_Counter_Register=readMSR(IA32_X2APIC_LVT_PMI_MSR);

    writeMSR(IA32_X2APIC_LVT_PMI_MSR, LVT_Perfmon_Counter_Register | (1<<16)); //set bit 16 (the mask bit)
  }
  else
  {
    LVT_Perfmon_Counter_Register=*(volatile DWORD *)(IA32_APIC_BASE+0x340);

    *(volatile DWORD *)(IA32_APIC_BASE+0x340)=LVT_Perfmon_Counter_Register | (1 << 16);
  }

  interruptvector=LVT_Perfmon_Counter_Register & 0xff;

  sendstringf("Raising PMI (Vector %d)\n",interruptvector);

  newintinfo.interruption_information=0;
  newintinfo.interruptvector=interruptvector;
  newintinfo.type=itExternal; //external
  newintinfo.haserrorcode=0;
  newintinfo.valid=1;

  vmwrite(vm_entry_interruptioninfo, newintinfo.interruption_information); //entry info field
  vmwrite(vm_entry_exceptionerrorcode, 0); //entry errorcode
  vmwrite(vm_entry_instructionlength, vmread(vm_exit_instructionlength)); //entry instruction length
  return 0;
}

int raiseGeneralProtectionFault(UINT64 errorcode)
{
  VMEntry_interruption_information newintinfo;

  sendstring("Raising GPF\n\r");

  if (isAMD)
  {
    pcpuinfo c=getcpuinfo();

    c->vmcb->inject_Type=3; //exception fault/trap
    c->vmcb->inject_Vector=13; //#GPF
    c->vmcb->inject_Valid=1;
    c->vmcb->inject_EV=1;
    c->vmcb->inject_ERRORCODE=errorcode;
  }
  else
  {

    newintinfo.interruption_information=0;
    newintinfo.interruptvector=13;
    newintinfo.type=3; //hardware
    newintinfo.haserrorcode=1;
    newintinfo.valid=1;

    vmwrite(vm_entry_interruptioninfo, newintinfo.interruption_information); //entry info field
    vmwrite(vm_entry_exceptionerrorcode, errorcode); //entry errorcode
    vmwrite(vm_entry_instructionlength, vmread(vm_exit_instructionlength)); //entry instruction length
  }
  return 0;
}



int emulateExceptionInterrupt(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, unsigned int cs, UINT64 rip, int haserrorcode, UINT64 errorcode, int isFault)
{
  //this routine will let you call a routine as if it was called by a interrupt caused by a exception.
  //useful for idt1 bypass and can be used for other 'extra' interrupt types

  PGDT_ENTRY gdt=NULL,ldt=NULL;
  UINT64 gdtbase=isAMD?currentcpuinfo->vmcb->gdtr_base:vmread(vm_guest_gdtr_base);
  UINT64 gdtlimit=isAMD?currentcpuinfo->vmcb->gdtr_limit:vmread(vm_guest_gdt_limit);
  ULONG ldtselector=isAMD?currentcpuinfo->vmcb->ldtr_selector:vmread(vm_guest_ldtr);
  Access_Rights old_csaccessrights, new_csaccessrights; //intel
  Segment_Attribs /*old_csattribs,*/ new_csattribs; //amd
  int privilege_level_changed=0;
  void *_TSS=NULL;

  //nosendchar[getAPICID()]=1;

  sendstring("Emulation\n");
  if ((isAMD?currentcpuinfo->vmcb->cs_selector:vmread(vm_guest_cs))==0x10)
  {
    sendstring("!!!!!FROM KERNELMODE (assuming it\'s windows 64)!!!!!\n");
  }

  ULONG original_ss=isAMD?currentcpuinfo->vmcb->ss_selector:vmread(vm_guest_ss);
  UINT64 original_rsp=isAMD?currentcpuinfo->vmcb->RSP:vmread(vm_guest_rsp);
  ULONG original_cs=isAMD?currentcpuinfo->vmcb->cs_selector:vmread(vm_guest_cs);
  UINT64 original_rip=isAMD?currentcpuinfo->vmcb->RIP:vmread(vm_guest_rip);
  ULONG newSS=0;
  UINT64 newRSP=original_rsp;
  UINT64 rflags=isAMD?currentcpuinfo->vmcb->RFLAGS:vmread(vm_guest_rflags);
  PRFLAGS prflags=(PRFLAGS)&rflags;

  sendstringf("cs:rip=%x:%6\n", original_cs,original_rip);
  sendstringf("ss:rsp=%x:%6\n", original_ss,original_rsp);


  sendstringf("cpunr=%d\n",currentcpuinfo->cpunr);

  int notpaged=0;
  //int pushssrsp=0;   //if cpl change or 64-bit mode set ss/rsp on the stack as well

  sendstring("Mapping the gdt\n");
  sendstringf("gdtbase=%6\n",gdtbase);

  gdt=mapVMmemory(currentcpuinfo, gdtbase, gdtlimit, NULL, NULL);
  if (gdt==NULL)
  {
    nosendchar[getAPICID()]=0;
    sendstring("For some messed up reason the gdt is paged out...");
    return 1;
  }

  ULONG ldtlimit;


  if (ldtselector)
  {
    ULONG ldtbase;

    nosendchar[getAPICID()]=0;
    sendstring("ldt is valid, so getting the information\n\r");

    ldtbase=(gdt[(ldtselector >> 3)].Base24_31 << 24) | gdt[(ldtselector >> 3)].Base0_23;
    ldtlimit=(gdt[(ldtselector >> 3)].Limit16_19 << 16) | gdt[(ldtselector >> 3)].Limit0_15;
    ldt=(PGDT_ENTRY)mapVMmemory(currentcpuinfo, ldtbase, ldtlimit, NULL,NULL);

    sendstringf("ldt=%8\n\r",(UINT64)ldt);
  }

  if (IS64BITPAGING(currentcpuinfo)) //then also 64-bit interrupt handling
  {
    sendstring("IS64BITPAGING=1\n");

  }


  //priv change check

  sendstringf("old cs=%x\n",original_cs);
  sendstringf("new cs=%x\n",cs);
  sendstringf("old ss=%x\n",original_ss);

  if (!isAMD)
  {
    old_csaccessrights.AccessRights=vmread(vm_guest_cs_access_rights); //current ss access_rights
    new_csaccessrights.AccessRights=getSegmentAccessRights(gdt,ldt,cs); //new cs_accessrights

    sendstringf("old_cs accessrights=%x\n",old_csaccessrights.AccessRights);
    sendstringf("new_cs accessrights=%x\n",new_csaccessrights.AccessRights);
    privilege_level_changed=(old_csaccessrights.DPL != new_csaccessrights.DPL);
  }
  else
  {
    //old_csattribs.SegmentAttrib=currentcpuinfo->vmcb->cs_attrib;
    new_csattribs.SegmentAttrib=getSegmentAttrib(gdt, ldt, cs);
   // sendstringf("old_cs attribs=%x\n",old_csattribs.SegmentAttrib);
    sendstringf("new_cs attribs=%x\n",new_csattribs.SegmentAttrib);
    privilege_level_changed=(currentcpuinfo->vmcb->CPL != new_csattribs.DPL);

    sendstringf("new_csattribs.DPL=%d\n", new_csattribs.DPL);
  }


  sendstringf("privilege_level_changed=%d\n",privilege_level_changed);

  //look at the TSS to find out the new ss/rsp
  //map tss (tss base is can be found with vmread(0x6814))
  QWORD TSSBase=isAMD?currentcpuinfo->vmcb->tr_base:vmread(vm_guest_tr_base);
  QWORD TSSLimit=isAMD?currentcpuinfo->vmcb->tr_limit:vmread(vm_guest_tr_limit);


   _TSS=mapVMmemory(currentcpuinfo, TSSBase, TSSLimit, NULL, NULL);
  if (_TSS==NULL)
  {
    nosendchar[getAPICID()]=0;
    sendstring("TSS PAGING FAULT\n");

    //I have no idea what to do here, I'll pass it off to the original handler
    if (gdt)
      unmapVMmemory(gdt,gdtlimit);

    if (ldt)
      unmapVMmemory(ldt, ldtlimit);
    return 1;

  }
  sendstringf("Mapped TSS(vma=%6) at vmma=%6\n\r",TSSBase, (UINT64)_TSS);

  if (isAMD)
  {
    currentcpuinfo->vmcb->CPL=new_csattribs.DPL; //in windows this should always be 0, but you CAN make it
    currentcpuinfo->vmcb->VMCB_CLEAN_BITS&=~(1 << 8); //CPL change (also segments)

    new_csaccessrights.DPL=currentcpuinfo->vmcb->CPL; //init
  }

  if (privilege_level_changed)
  {
    if (IS64BITPAGING(currentcpuinfo)) //cs has been changed already, so this macro returns the target code type
    {

      PTSS64 TSS=_TSS;
      //64-bit tss
      sendstring("64-bit interrupt handling and privilege change\n");

      if (new_csaccessrights.DPL==0)
      {
        sendstringf("New DPL is 0. Setting RSP to RSP0 = %6\n", TSS->RSP0);
        newRSP=TSS->RSP0;

      }

      if (new_csaccessrights.DPL==1)
        newRSP=TSS->RSP1;

      if (new_csaccessrights.DPL==2)
        newRSP=TSS->RSP2;



    }
    else
    {
      //32-bit tss (16-bit is not supported)
      PTSS TSS=_TSS;

      sendstring("32-bit interrupt handling\n\r");

      if (new_csaccessrights.DPL==0)
      {
        newRSP=TSS->ESP0;
        newSS=TSS->SS0;
      }

      if (new_csaccessrights.DPL==1)
      {
        newRSP=TSS->ESP1;
        newSS=TSS->SS1;
      }

      if (new_csaccessrights.DPL==2)
      {
        newRSP=TSS->ESP2;
        newSS=TSS->SS2;
      }


    }
  }

  sendstringf("newSS=%6\n", newSS);
  sendstringf("newRSP=%6\n", newRSP);


  if (isFault)
    prflags->RF=1; //Set RF to 1 so it gets pushed in the stack's rflags

  //this is now up to the caller to se

  if (IS64BITPAGING(currentcpuinfo))
  {
    //64 bit pushes
    UINT64 rsp=newRSP;
    UINT64 *stack;
    int stackpos=0;

    sendstringf("Setting the stack for 64-bit. RSP=%6\n\r",rsp);




    if (!privilege_level_changed) //no stackswitch
    {
      //rsp needs to be on a 16 byte alignment
      rsp=rsp & 0xfffffffffffffff0ULL;
      sendstringf("After alignment RSP=%6\n", rsp);
    }


    rsp-=5*8; //ss+esp+eflags+cs+eip
    if (haserrorcode)
    {
      sendstring("Has errorcode\n");
      rsp-=8;
    }
    sendstringf("After adjusting rsp for the pushes RSP=%6\n",rsp);


    //map address of rsp and write the info there
    stack=mapVMmemory(currentcpuinfo, rsp, 6*8, NULL, NULL);

    if (stack==NULL)
    {
      nosendchar[getAPICID()]=0;
      sendstring("STACK FAILURE\n");
      sendstring("STACK FAILURE\n");
      sendstring("STACK FAILURE\n");
      sendstring("STACK FAILURE\n");
      sendstring("STACK FAILURE\n");
      sendstring("STACK FAILURE\n");
      sendstring("STACK FAILURE\n");
      sendstring("STACK FAILURE\n");
      sendstring("STACK FAILURE\n");


      sendstringf("SF-cs:rip=%x:%6\n", original_cs,original_rip);
      sendstringf("SF-ss:rsp=%x:%6\n", original_ss,original_rsp);
      sendstringf("SF-cr2=%6\n", getCR2());
      sendstringf("Errorcode=%6\n", errorcode);
      sendstringf("Mapped TSS(vma=%6) at vmma=%6\n\r",TSSBase, (UINT64)_TSS);

      sendvmstate(currentcpuinfo, vmregisters);

      if (gdt)
        unmapVMmemory(gdt,gdtlimit);

      if (ldt)
        unmapVMmemory(ldt, ldtlimit);

      if (_TSS)
        unmapVMmemory(_TSS, TSSLimit);

      return 1; //error
    }

    if (haserrorcode)
      stack[stackpos++]=errorcode;

    stack[stackpos++]=original_rip;
    stack[stackpos++]=original_cs;
    stack[stackpos++]=rflags; //eflags
    stack[stackpos++]=original_rsp;
    stack[stackpos++]=original_ss;

    if (isAMD)
      currentcpuinfo->vmcb->RSP=rsp;
    else
      vmwrite(vm_guest_rsp,rsp);

    sendstringf("[%6]=%6\n", rsp,stack[0]);
    sendstringf("[%6]=%6\n", rsp+8,stack[1]);
    sendstringf("[%6]=%6\n", rsp+2*8,stack[2]);
    sendstringf("[%6]=%6\n", rsp+3*8,stack[3]);
    sendstringf("[%6]=%6\n", rsp+4*8,stack[4]);
    if (haserrorcode)
    {
      sendstringf("[%6]=%6\n", rsp+5*8,stack[5]);
    }

    if (stack)
      unmapVMmemory(stack, 6*8);
  }
  else
  {
    //32 bit pushes
    ULONG esp=newRSP;
    ULONG *stack;
    int stackpos=0;
    sendstring("Setting the stack for 32-bit\n\r");

    esp=esp-3*4;  //eflags+cs+eip
    if (haserrorcode)
      esp-=4;

    if (privilege_level_changed) //ss-rsp
      esp-=2*4;

    stack=mapVMmemory(currentcpuinfo, esp, 6*4, NULL, NULL);

    if (notpaged)
    {
      nosendchar[getAPICID()]=0;
      sendstring("STACK FAILURE\n");
      sendstring("STACK FAILURE\n");
      sendstring("STACK FAILURE\n");
      sendstring("STACK FAILURE\n");
      sendstring("STACK FAILURE\n");
      sendstring("STACK FAILURE\n");
      sendstring("STACK FAILURE\n");
      sendstring("STACK FAILURE\n");
      sendstring("STACK FAILURE\n");

      sendstringf("SF-cs:rip=%x:%6\n", original_cs,original_rip);
      sendstringf("SF-ss:rsp=%x:%6\n", original_ss,original_rsp);
      sendstringf("SF-cr2=%6\n", getCR2());
      sendstringf("Errorcode=%6\n", errorcode);
      sendstringf("Mapped TSS(vma=%6) at vmma=%6\n\r",TSSBase, (UINT64)_TSS);

      sendvmstate(currentcpuinfo, vmregisters);

      if (gdt)
        unmapVMmemory(gdt,gdtlimit);

      if (ldt)
        unmapVMmemory(ldt, ldtlimit);

      if (_TSS)
        unmapVMmemory(_TSS, TSSLimit);

      return 1; //error


    }

    if (haserrorcode)
      stack[stackpos++]=errorcode;

    stack[stackpos++]=original_rip;
    stack[stackpos++]=original_cs;
    stack[stackpos++]=rflags; //eflags

    if (privilege_level_changed)
    {
      stack[stackpos++]=original_rsp;
      stack[stackpos++]=original_ss;
    }

    if (isAMD)
      currentcpuinfo->vmcb->RSP=esp;
    else
      vmwrite(vm_guest_rsp,esp);

    if (stack)
      unmapVMmemory(stack, 6*4);
  }


  //still here so all checks where successful
  //rsp has already been adjusted, now the flags,ss, cs and eip

  //disable TF flag in eflags
  prflags->TF=0;
  prflags->IF=0; //exception interrupt, so clear IF as well
  prflags->VM=0;
  prflags->RF=0;
  prflags->NT=0;

  if (isAMD)
    currentcpuinfo->vmcb->RFLAGS=2;
  else
    vmwrite(vm_guest_rflags,2/*rflags*/);

  if (privilege_level_changed)
  {
    if (IS64BITPAGING(currentcpuinfo))
    {
      //int64 int's set SS to NULL
      if (isAMD)
      {
        currentcpuinfo->vmcb->ss_selector=0;
        currentcpuinfo->vmcb->ss_base=0;
        currentcpuinfo->vmcb->ss_limit=0;
        currentcpuinfo->vmcb->ss_attrib=0;
      }
      else
      {
        vmwrite(vm_guest_ss,0);
        vmwrite(vm_guest_ss_base,getSegmentBase(gdt,ldt,0));
        vmwrite(vm_guest_ss_limit,getSegmentLimit(gdt,ldt,0));
        vmwrite(vm_guest_ss_access_rights,getSegmentAccessRights(gdt,ldt,0));
      }
    }
    else
    {
      //set new ss
      if (isAMD)
      {
        currentcpuinfo->vmcb->ss_selector=newSS;
        currentcpuinfo->vmcb->ss_base=getSegmentBase(gdt,ldt,newSS);
        currentcpuinfo->vmcb->ss_limit=getSegmentLimit(gdt,ldt,newSS);
        currentcpuinfo->vmcb->ss_attrib=getSegmentAttrib(gdt,ldt,newSS);
        setDescriptorAccessedFlag(gdt,ldt,newSS);
      }
      else
      {
        vmwrite(vm_guest_ss,newSS);
        vmwrite(vm_guest_ss_base,getSegmentBase(gdt,ldt,newSS));
        vmwrite(vm_guest_ss_limit,getSegmentLimit(gdt,ldt,newSS));
        setDescriptorAccessedFlag(gdt,ldt,newSS);
        vmwrite(vm_guest_ss_access_rights,getSegmentAccessRights(gdt,ldt,newSS));
      }
    }


  }


  //set cs
  sendstringf("Setting cs to %x\n\r", cs);
  if (isAMD)
  {
    currentcpuinfo->vmcb->cs_selector=cs;
    currentcpuinfo->vmcb->cs_base=getSegmentBase(gdt,ldt,cs);
    currentcpuinfo->vmcb->cs_limit=getSegmentLimit(gdt,ldt,cs);
    currentcpuinfo->vmcb->cs_attrib=getSegmentAttrib(gdt,ldt,cs);

    sendstringf("currentcpuinfo->vmcb->cs_selector=%x\n", currentcpuinfo->vmcb->cs_selector);
    sendstringf("currentcpuinfo->vmcb->cs_base=%x\n", currentcpuinfo->vmcb->cs_base);
    sendstringf("currentcpuinfo->vmcb->cs_limit=%x\n", currentcpuinfo->vmcb->cs_limit);
    sendstringf("currentcpuinfo->vmcb->cs_attrib=%x\n", currentcpuinfo->vmcb->cs_attrib);


    setDescriptorAccessedFlag(gdt,ldt,cs);

  }
  else
  {
    vmwrite(vm_guest_cs, cs);
    vmwrite(vm_guest_cs_limit, getSegmentLimit(gdt, ldt, cs));
    vmwrite(vm_guest_cs_base, getSegmentBase(gdt, ldt, cs));
    setDescriptorAccessedFlag(gdt, ldt, cs);
    vmwrite(vm_guest_cs_access_rights, getSegmentAccessRights(gdt, ldt, cs));
  }

  sendstringf("Setting rip to %6\n\r", rip);
  //rip
  if (isAMD)
    currentcpuinfo->vmcb->RIP=rip;
  else
    vmwrite(vm_guest_rip,rip);

  sendstringf("Returning\n\r");

  if (gdt)
    unmapVMmemory(gdt,gdtlimit);

  if (ldt)
    unmapVMmemory(ldt, ldtlimit);

  if (_TSS)
    unmapVMmemory(_TSS, TSSLimit);

  return 0;
}



void returnToRealmode(pcpuinfo currentcpuinfo) //obsolete with rm emu
/* Called when in 16bit protected mode used for prilileged instruction emulation */
{

  if (hasUnrestrictedSupport==0)
  {
    Access_Rights reg_csaccessrights;
    RFLAGS guestrflags=(RFLAGS)vmread(vm_guest_rflags);

    if (ISREALMODE(currentcpuinfo))
    {
      reg_csaccessrights.AccessRights=0;
      reg_csaccessrights.Segment_type=3;
      reg_csaccessrights.S=1;
      reg_csaccessrights.DPL=3;
      reg_csaccessrights.P=1;
      reg_csaccessrights.G=0;
      reg_csaccessrights.D_B=0;
      reg_csaccessrights.AVL=0; //mark 0
      vmwrite(0x4814,(ULONG)reg_csaccessrights.AccessRights); //es access rights
      vmwrite(0x4816,(ULONG)reg_csaccessrights.AccessRights); //cs access rights
      vmwrite(0x4818,(ULONG)reg_csaccessrights.AccessRights); //ss access rights
      vmwrite(0x481a,(ULONG)reg_csaccessrights.AccessRights); //ds access rights
      vmwrite(0x481c,(ULONG)reg_csaccessrights.AccessRights); //fs access rights
      vmwrite(0x481e,(ULONG)reg_csaccessrights.AccessRights); //gs access rights

      vmwrite(0x4800,(ULONG)0xffff); //es limit
      vmwrite(0x4802,(ULONG)0xffff); //cs limit
      vmwrite(0x4804,(ULONG)0xffff); //ss limit
      vmwrite(0x4806,(ULONG)0xffff); //ds limit
      vmwrite(0x4808,(ULONG)0xffff); //fs limit
      vmwrite(0x480a,(ULONG)0xffff); //gs limit

      vmwrite(0x800,vmread(0x6806) >> 4); //es selector gets the base of es shifted right 4 bits
      vmwrite(0x802,vmread(vm_guest_cs_base) >> 4); //cs selector
      vmwrite(0x804,vmread(vm_guest_ss_base) >> 4); //ss selector
      vmwrite(0x806,vmread(0x680c) >> 4); //ds selector
      vmwrite(0x808,vmread(0x680e) >> 4); //fs selector
      vmwrite(0x80a,vmread(0x6810) >> 4); //gs selector

      guestrflags.VM=1; //enable virtual 8086 mode
      guestrflags.TF=0; //disable the trap flag
      guestrflags.RF=0;
      guestrflags.IOPL=3;
      guestrflags.IF=currentcpuinfo->hasIF;
      vmwrite(vm_guest_rflags,guestrflags.value);

    }
    else
    {
      nosendchar[getAPICID()]=0;
      sendstringf("ERROR: Guest doesn't WANT to be in realmode\n\r");
    }
  }
}

int handleINIT(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{
  UINT64 a,b,c,d;
  zeromemory(vmregisters,sizeof(VMRegisters));


  //magic
  a=1;
  _cpuid(&a,&b,&c,&d);
  vmregisters->rdx=a;

  setup8086WaitForSIPI(currentcpuinfo,0);

  vmwrite(vm_guest_rip,0x0);

  vmwrite(vm_guest_activity_state,(UINT64)3);


  return 0;
}

int handleSIPI(void)
{
  UINT64 newcs,newcsbase,newip;

 // while (1) outportb(0x80,0xfe);

  sendstringf("Handling SIPI\n\r");


  //the exit qualification contains the address of the route
  newcs=(QWORD)(vmread(vm_exit_qualification)) << 8;
  newcsbase=newcs << 4;
  newip=0;

  //vmwrite(0x800,newcs);
  vmwrite(0x802,newcs); //
  //vmwrite(0x804,newcs);
  //vmwrite(0x806,newcs);
  //vmwrite(0x808,newcs);
  //vmwrite(0x80a,newcs);

  //vmwrite(0x6806,newcsbase);
  vmwrite(vm_guest_cs_base,newcsbase);//
  //vmwrite(vm_guest_ss_base,newcsbase);
  //vmwrite(0x680c,newcsbase);
  //vmwrite(0x680e,newcsbase);
  //vmwrite(0x6810,newcsbase);

  vmwrite(vm_guest_rip,newip);
  vmwrite(vm_guest_activity_state,(ULONG)0); //guest activity state, normal


  return 0;
}



int handle_cr3_callback(pcpuinfo currentcpuinfo,VMRegisters *vmregisters)
{
  PGDT_ENTRY gdt=NULL,ldt=NULL;
  UINT64 gdtbase=vmread(0x6816);
  UINT64 gdtlimit=vmread(vm_guest_gdt_limit);
  ULONG ldtselector=vmread(0x80c);

  nosendchar[getAPICID()]=0;
  sendstringf("Handling cr3 edit. Is %x wants to set it to %x:\n\r", vmread(0x6802), currentcpuinfo->guestCR3);
  sendstring("Also, currently not implemented so no idea how this happened\n\r");

  ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
  while (1) outportb(0x80,0xd4);


  //sendstringf("before:\n\r");
  //sendvmstate(currentcpuinfo,vmregisters);



  if (currentcpuinfo->cr3_callback.called_callback)
  {
    //allow this, but don't tell the guest, or caller it worked, programmer has to assume it did....
    //set cr3
    vmwrite(0x6802, currentcpuinfo->guestCR3); //set to what it wants it to be
    vmwrite(vm_guest_rip, vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));  //next instruction

    currentcpuinfo->guestCR3 = currentcpuinfo->cr3_callback.newcr3;
    return 0;
  }

  currentcpuinfo->cr3_callback.newcr3 = currentcpuinfo->guestCR3; //saved in case changed by callback routine

  //save the state
  currentcpuinfo->cr3_callback.rax=vmregisters->rax;
  currentcpuinfo->cr3_callback.rbx=vmregisters->rbx;
  currentcpuinfo->cr3_callback.rcx=vmregisters->rcx;
  currentcpuinfo->cr3_callback.rdx=vmregisters->rdx;
  currentcpuinfo->cr3_callback.rsi=vmregisters->rsi;
  currentcpuinfo->cr3_callback.rdi=vmregisters->rdi;
  currentcpuinfo->cr3_callback.rbp=vmregisters->rbp;
  currentcpuinfo->cr3_callback.r8=vmregisters->r8;
  currentcpuinfo->cr3_callback.r9=vmregisters->r9;
  currentcpuinfo->cr3_callback.r10=vmregisters->r10;
  currentcpuinfo->cr3_callback.r11=vmregisters->r11;
  currentcpuinfo->cr3_callback.r12=vmregisters->r12;
  currentcpuinfo->cr3_callback.r13=vmregisters->r13;
  currentcpuinfo->cr3_callback.r14=vmregisters->r14;
  currentcpuinfo->cr3_callback.r15=vmregisters->r15;

  //save selectors

  currentcpuinfo->cr3_callback.es_selector=vmread(0x800);
  currentcpuinfo->cr3_callback.cs_selector=vmread(0x802);
  currentcpuinfo->cr3_callback.ss_selector=vmread(0x804);
  currentcpuinfo->cr3_callback.ds_selector=vmread(0x806);
  currentcpuinfo->cr3_callback.fs_selector=vmread(0x808);
  currentcpuinfo->cr3_callback.gs_selector=vmread(0x80a);

  //limits
  currentcpuinfo->cr3_callback.es_limit=vmread(0x4800);
  currentcpuinfo->cr3_callback.cs_limit=vmread(0x4802);
  currentcpuinfo->cr3_callback.ss_limit=vmread(0x4804);
  currentcpuinfo->cr3_callback.ds_limit=vmread(0x4806);
  currentcpuinfo->cr3_callback.fs_limit=vmread(0x4808);
  currentcpuinfo->cr3_callback.gs_limit=vmread(0x480a);

  //base
  currentcpuinfo->cr3_callback.es_base=vmread(0x6806);
  currentcpuinfo->cr3_callback.cs_base=vmread(vm_guest_cs_base);
  currentcpuinfo->cr3_callback.ss_base=vmread(vm_guest_ss_base);
  currentcpuinfo->cr3_callback.ds_base=vmread(0x680c);
  currentcpuinfo->cr3_callback.fs_base=vmread(0x680e);
  currentcpuinfo->cr3_callback.gs_base=vmread(0x6810);


  //access rights
  currentcpuinfo->cr3_callback.es_accessrights=vmread(0x4814);
  currentcpuinfo->cr3_callback.cs_accessrights=vmread(0x4816);
  currentcpuinfo->cr3_callback.ss_accessrights=vmread(0x4818);
  currentcpuinfo->cr3_callback.ds_accessrights=vmread(0x481a);
  currentcpuinfo->cr3_callback.fs_accessrights=vmread(0x481c);
  currentcpuinfo->cr3_callback.gs_accessrights=vmread(0x481e);

  //rsp
  currentcpuinfo->cr3_callback.rsp=vmread(vm_guest_rsp);

  //rip
  currentcpuinfo->cr3_callback.rip=vmread(vm_guest_rip)+vmread(vm_exit_instructionlength); //restore rip AFTER the instructions, infinite loops might be fun, but not here

  //rflags
  currentcpuinfo->cr3_callback.rflags=vmread(vm_guest_rflags);


  //interrupt state
  currentcpuinfo->cr3_callback.interruptability_state=vmread(vm_guest_interruptability_state);


  //sendstringf("saved state: cs:eip=%x:%x\n\r",currentcpuinfo->cr3_callback.cs_selector, currentcpuinfo->cr3_callback.rip);
  //sendstringf("saved state: ss:esp=%x:%x\n\r",currentcpuinfo->cr3_callback.ss_selector, currentcpuinfo->cr3_callback.rsp);
  //sendstringf("saved state: rflags=%x",currentcpuinfo->cr3_callback.rflags);
  //sendstringf("saved state: interruptability_state=%x",currentcpuinfo->cr3_callback.interruptability_state);


//works here...


  //change state to callback location

  //and set the segment selectors to what it will be (cs and ss only)


  gdt=mapVMmemory(currentcpuinfo, gdtbase, gdtlimit, NULL, NULL);
  ULONG ldtlimit;


  if (ldtselector)
  {
    ULONG ldtbase;

    //sendstring("ldt is valid, so getting the information\n\r");

    ldtbase=(gdt[(ldtselector >> 3)].Base24_31 << 24) + gdt[(ldtselector >> 3)].Base0_23;
    ldtlimit=(gdt[(ldtselector >> 3)].Limit16_19 << 16) + gdt[(ldtselector >> 3)].Limit0_15;
    //ldt=(PGDT_ENTRY)(UINT64)MapPhysicalMemory(getPhysicalAddressVM(currentcpuinfo, ldtbase, &notpaged), currentcpuinfo->AvailableVirtualAddress+0x00200000);
    ldt=mapVMmemory(currentcpuinfo, ldtbase, ldtlimit, NULL, NULL);
  }

  vmwrite(0x802,currentcpuinfo->cr3_callback.callback_cs);
  vmwrite(0x804,currentcpuinfo->cr3_callback.callback_ss);
  vmwrite(0x4800,getSegmentLimit(gdt,ldt,currentcpuinfo->cr3_callback.callback_cs));
  vmwrite(0x4802,getSegmentLimit(gdt,ldt,currentcpuinfo->cr3_callback.callback_ss));
  vmwrite(vm_guest_cs_base,getSegmentBase(gdt,ldt,currentcpuinfo->cr3_callback.callback_cs));
  vmwrite(vm_guest_ss_base,getSegmentBase(gdt,ldt,currentcpuinfo->cr3_callback.callback_ss));
  setDescriptorAccessedFlag(gdt,ldt,currentcpuinfo->cr3_callback.callback_cs);
  setDescriptorAccessedFlag(gdt,ldt,currentcpuinfo->cr3_callback.callback_ss);
  vmwrite(0x4816,getSegmentAccessRights(gdt,ldt,currentcpuinfo->cr3_callback.callback_cs));
  vmwrite(0x4818,getSegmentAccessRights(gdt,ldt,currentcpuinfo->cr3_callback.callback_ss));

  if (gdt)
    unmapVMmemory(gdt, gdtlimit);

  if (ldt)
    unmapVMmemory(ldt, ldtlimit);

  //set the params for the callback to know whats going on
  if (currentcpuinfo->cr3_callback.calling_convention==0)
  {
    //32-bit stdcall
    //push 2 ulongs , first newcr3, then oldcr3

    //map currentcpuinfo->cr3_callback.callback_rsp-8
    //and write oldcr3 - newcr3
    ULONG *stack;
    //sendstring("calling convention=0\n\r");

    stack=mapVMmemory(currentcpuinfo, currentcpuinfo->cr3_callback.callback_rsp-12, 12, NULL, NULL);

    //sendstringf("mapped callback_rsp-12 to %8\n\r",stack);
    stack[0]=vmread(vm_guest_rip);           //eip
    stack[1]=vmread(0x6802);           //old cr3 (current one)
    stack[2]=currentcpuinfo->guestCR3; //this was already edited

    vmwrite(vm_guest_rsp, currentcpuinfo->cr3_callback.callback_rsp-12); //adjust esp

    unmapVMmemory(stack,12);
    //sendstringf("Set esp to %x\n\r",vmread(vm_guest_rsp));

    //sendstringf("stack[0]=%8 (return eip)\n\r",stack[0]);
    //sendstringf("stack[1]=%8 (old cr3)\n\r",stack[1]);
    //sendstringf("stack[2]=%8 (new cr3)\n\r",stack[2]);

  }
  else
  {
    //64-bit call
    //1=rdi (old)
    //2=rsi (new)
    currentcpuinfo->cr3_callback.rdi=vmread(0x6802);
    currentcpuinfo->cr3_callback.rsi=currentcpuinfo->guestCR3;

    UINT64 *stack;
    stack=mapVMmemory(currentcpuinfo, currentcpuinfo->cr3_callback.callback_rsp-8, 8,  NULL, NULL);
    stack[0]=vmread(vm_guest_rip);           //rip

    //todo: probably a stack alignment issue

    vmwrite(vm_guest_rsp, currentcpuinfo->cr3_callback.callback_rsp-8); //adjust rsp
    unmapVMmemory(stack,8);
  }




  vmwrite(vm_guest_interruptability_state, (1<<3)); //block by NMI, so even a nmi based taskswitch won't interrupt

  //and set IF to 0 in eflags
  currentcpuinfo->Previous_CLI=(vmread(vm_guest_rflags) >> 9) & 1;
  vmwrite(vm_guest_rflags,vmread(vm_guest_rflags) & 0xFFFFFFFFFFFFFDFF);



  //rip
  vmwrite(vm_guest_rip, currentcpuinfo->cr3_callback.callback_rip);


  currentcpuinfo->cr3_callback.called_callback=1;


 // currentcpuinfo->cr3_callback.cr3_change_callback=0; //temp test  to let the system work normal after first one



//  returnFromCR3Callback(currentcpuinfo,vmregisters, currentcpuinfo->guestCR3);
//  return 0;



  return 0;

}


int handleHLT(pcpuinfo currentcpuinfo)
{
  nosendchar[getAPICID()]=0;
  sendstringf("Handling HLT instruction\n\r");

  if (hasUnrestrictedSupport==0)
  {

    if (ISREALMODE(currentcpuinfo))
    {
      ULONG guestrflags=0;
      PRFLAGS pguestrflags=(PRFLAGS)&guestrflags;

      if (currentcpuinfo->hasIF)
      {
        vmwrite(vm_execution_controls_pin,vmread(vm_execution_controls_pin) | 1); //set the option to exit on external events
        //wait for event
      }
      else
      {
        nosendchar[getAPICID()]=0;
        sendstringf("WARNING: HLT with IF=0\n\r");
      }

      guestrflags=vmread(vm_guest_rflags);
      pguestrflags->VM=0; //vm off, somehow when vm is on, hlt just wont work
      pguestrflags->TF=0;
      pguestrflags->IOPL=0;
      pguestrflags->RF=0;
      pguestrflags->IF=currentcpuinfo->hasIF;

      sendstringf("settings guesteflags to %x\n\r",guestrflags);
      vmwrite(vm_guest_rflags,(ULONG)guestrflags); //rflags


      //RealEmulationIn16BitProtectedMode();
    }
    else
    {
      sendstringf("HLT in protected mode\n\r");

    }
  }

  vmwrite(vm_guest_activity_state,1);// set activity to HLT

  vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength)); //next instruction

  return 0;
}


ULONG getSegmentLimit(PGDT_ENTRY gdt, PGDT_ENTRY ldt, ULONG selector)
{
  //unsigned int RPLrpl=selector & 3;
  unsigned int TI=(selector >> 2) & 1;
  unsigned int index=(selector >> 3);
  PGDT_ENTRY usedtable=TI ? ldt : gdt;
  unsigned int limit;

  if (usedtable==NULL || selector==0)
    return 0;

  limit=((QWORD)usedtable[index].Limit16_19 << 16) | usedtable[index].Limit0_15;
  if (usedtable[index].G)
    limit=(limit+1)*4096-1;


  return limit;
}

UINT64 getSegmentBaseEx(PGDT_ENTRY gdt, PGDT_ENTRY ldt, ULONG selector, int expandto80bit)
{
  //unsigned int RPLrpl=selector & 3;
  unsigned int TI=(selector >> 2) & 1;
  unsigned int index=(selector >> 3);
  PGDT_ENTRY usedtable=TI ? ldt : gdt;

  //sendstringf("getSegmentBaseEx(%6, %6, %d, %d\n\r", gdt, ldt, selector, expandto80bit);


  if ((usedtable==NULL) || (selector==0))
    return 0;

  if (expandto80bit)
  {
    //it now consists out of 3 dwords
    UINT64 temp;
    ULONG *upperbase=(ULONG *)&usedtable[index];
    temp=upperbase[2];
    temp=((QWORD)temp << 32) | ((QWORD)usedtable[index].Base24_31 << 24) | ((QWORD)usedtable[index].Base0_23);
    return temp;
  }
  else
  {
    return (usedtable[index].Base24_31 << 24) + usedtable[index].Base0_23;
  }
}

ULONG getSegmentBase(PGDT_ENTRY gdt, PGDT_ENTRY ldt, ULONG selector)
{
  return (ULONG)getSegmentBaseEx(gdt,ldt,selector,0);

}

void setDescriptorAccessedFlag(PGDT_ENTRY gdt, PGDT_ENTRY ldt, ULONG selector)
{
  //unsigned int RPLrpl=selector & 3;
  unsigned int TI=(selector >> 2) & 1;
  unsigned int index=(selector >> 3);
  PGDT_ENTRY usedtable=TI ? ldt : gdt;

  if ((usedtable==NULL) || (selector==0))
    return;

  usedtable[index].Type=usedtable[index].Type | 1; //set accessed bit to 1
}


ULONG getSegmentAccessRights(PGDT_ENTRY gdt, PGDT_ENTRY ldt, ULONG selector)
{
  //unsigned int RPLrpl=selector & 3;
  unsigned int TI=(selector >> 2) & 1;
  unsigned int index=(selector >> 3);
  PGDT_ENTRY usedtable=TI ? ldt : gdt;

  if ((usedtable==NULL) || (selector==0))
    return 0x10000; //bit 16 set, unusable

  return (ULONG)((*(unsigned long long *)(&usedtable[index]) >> 40) & 0xe0ff);
}

WORD convertSegmentAccessRightsToSegmentAttrib(ULONG accessrights)
{
  Access_Rights ar; //intel
  Segment_Attribs sa; //amd

  ar.AccessRights=accessrights;

  if (ar.unusable)
    return 0;

  sa.Segment_type=ar.Segment_type;
  sa.S=ar.S;
  sa.DPL=ar.DPL;
  sa.P=ar.P;
  sa.AVL=ar.AVL;
  sa.L=ar.L;
  sa.D_B=ar.D_B;
  sa.G=ar.G;

  return sa.SegmentAttrib;
}

ULONG getSegmentAttrib(PGDT_ENTRY gdt, PGDT_ENTRY ldt, ULONG selector) //for AMD's
{
  return convertSegmentAccessRightsToSegmentAttrib(getSegmentAccessRights(gdt, ldt, selector));
}



int handleTaskswitch(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{
  sendstring("Handling taskswitch, can be done in 32-bit only\n\r");

  ULONG  exitqualification=vmread(vm_exit_qualification);
  ULONG  TScause=exitqualification>>30;
  UINT64 gdtbase=vmread(0x6816);
  DWORD gdtlimit=vmread(vm_guest_gdt_limit);
  ULONG  newTSSselector=exitqualification & 0xffff;
  ULONG  oldTSSselector=vmread(0x80e);

  PGDT_ENTRY gdt=NULL,ldt=NULL;


  PTSS oldTSS,newTSS;
  UINT64 oldTSSaddress;
  UINT64 newTSSaddress;

  RFLAGS guestrflags=(RFLAGS)vmread(vm_guest_rflags);



  //int oldsize=vmread(0x480e);
  int newsize;
  unsigned int oldTSSdescriptorEntry,newTSSdescriptorEntry;

  gdt=mapVMmemory(currentcpuinfo, gdtbase, gdtlimit, NULL, NULL);
  //sendstringf("gdt mapped at %8\n\r",(ULONG)gdt);

  oldTSSdescriptorEntry=oldTSSselector >> 3;
  newTSSdescriptorEntry=newTSSselector >> 3;

  //sendstringf("oldTSSdescriptorEntry=%d\n\r",oldTSSdescriptorEntry);
  //sendstringf("newTSSdescriptorEntry=%d\n\r",newTSSdescriptorEntry);

  //sendstringf("&gdt[oldTSSdescriptorEntry]=%8\n\r",(ULONG)&gdt[oldTSSdescriptorEntry]);
  //sendstringf("&gdt[newTSSdescriptorEntry]=%8\n\r",(ULONG)&gdt[newTSSdescriptorEntry]);

  if ((gdt[newTSSdescriptorEntry].Type==0xb) && (TScause!=1)) //not for iret
  {
    sendstring("TARGET TSS IS STILL BUSY. Should raise GP\n\r");
    unmapVMmemory(gdt, gdtlimit);
    return 1;
  }

  //if JMP or IRET clear the busy flag
  if (TScause==1 || TScause==2)
    gdt[oldTSSdescriptorEntry].Type=0x9; //TSS with busy flag unset  (1001)
  else
  {
    if (gdt[oldTSSdescriptorEntry].Type != 0xb)
    {
      sendstring("The old descriptor was not set busy!!!\n\r");

      unmapVMmemory(gdt, gdtlimit);
      return 1;
    }

    gdt[oldTSSdescriptorEntry].Type=0xb; //should be left busy
  }


  gdt[newTSSdescriptorEntry].Type=0xb; //TSS with busy flag set    (1011)




  //sendstringf("gdt[newTSSdescriptorEntry].Base24_31=%x\n\r",gdt[newTSSdescriptorEntry].Base24_31);
  //sendstringf("gdt[newTSSdescriptorEntry].Base0_23=%x\n\r",gdt[newTSSdescriptorEntry].Base0_23);

  newTSSaddress=(gdt[newTSSdescriptorEntry].Base24_31 << 24) + gdt[newTSSdescriptorEntry].Base0_23;
  oldTSSaddress=vmread(0x6814); //just ask the guest for the current tr base

  int oldsize=vmread(vm_guest_tr_limit);
  newsize=(gdt[newTSSdescriptorEntry].Limit16_19 << 16) + gdt[newTSSdescriptorEntry].Limit0_15;

  //sendstringf("oldTSSaddress=%8 (size=%x)\n\r", oldTSSaddress, oldsize);
  //sendstringf("newTSSaddress=%8 (size=%x)\n\r", newTSSaddress, newsize);

  //sendstringf("getPhysicalAddressVM(oldTSSaddress)=%8\n\r",getPhysicalAddressVM(oldTSSaddress));
  //sendstringf("getPhysicalAddressVM(newTSSaddress)=%8\n\r",getPhysicalAddressVM(newTSSaddress));


  //get a pointer to oldTSS and newTSS
//  oldTSS=(PTSS)(UINT64)MapPhysicalMemoryEx(getPhysicalAddressVM(currentcpuinfo, oldTSSaddress, &notpaged), currentcpuinfo->AvailableVirtualAddress+0x00400000,1);
//  newTSS=(PTSS)(UINT64)MapPhysicalMemoryEx(getPhysicalAddressVM(currentcpuinfo, newTSSaddress, &notpaged), currentcpuinfo->AvailableVirtualAddress+0x00800000,1);


  oldTSS=mapVMmemory(currentcpuinfo, oldTSSaddress, oldsize, NULL, NULL);
  newTSS=mapVMmemory(currentcpuinfo, newTSSaddress, newsize, NULL, NULL);


  //sendstringf("Mapped oldTSS at virtual address %8\n\r",(ULONG)oldTSS);
  //sendstringf("Mapped newTSS at virtual address %8\n\r",(ULONG)newTSS);


  //sendstring("Writing oldTSS state\n\r");
  //fill in oldTSS with the state of the guest (dynamic fields)

  //sendstringf("Saving eax (%8) at virtual address (%8)\n\r",vmregisters->eax, (ULONG)&(oldTSS->EAX));
  oldTSS->EAX=(ULONG)vmregisters->rax; //high part will be cut of anyhow)

  //sendstringf("Saving ecx (%8) at virtual address (%8)\n\r",vmregisters->ecx, (ULONG)&(oldTSS->ECX));
  oldTSS->ECX=(ULONG)vmregisters->rcx;
  oldTSS->EDX=(ULONG)vmregisters->rdx;
  oldTSS->EBX=(ULONG)vmregisters->rbx;
  oldTSS->EBP=(ULONG)vmregisters->rbp;
  oldTSS->ESI=(ULONG)vmregisters->rsi;
  oldTSS->EDI=(ULONG)vmregisters->rdi;

  oldTSS->ES=vmread(0x800);
  oldTSS->CS=vmread(0x802);
  oldTSS->SS=vmread(0x804);
  oldTSS->DS=vmread(0x806);
  oldTSS->FS=vmread(0x808);
  oldTSS->GS=vmread(0x80a);

  oldTSS->ESP=vmread(vm_guest_rsp);
  oldTSS->EIP=vmread(vm_guest_rip)+vmread(vm_exit_instructionlength);

  if ((TScause==3) && ((vmread(0x4408) >> 31)==1))
  {
    //interrupt
    VMExit_idt_vector_information idtvectorinfo;
    idtvectorinfo.idtvector_info=vmread(0x4408);

    if (idtvectorinfo.type!=4) //no software interrupt (0xcd 0x??)
      oldTSS->EIP=vmread(vm_guest_rip); //dont skip bytes
  }

  if (TScause==1) //IRET, so clear NT flag in eflags
    guestrflags.NT=0;

  oldTSS->EFLAGS=guestrflags.value;

  //fill in newTSS's Previous task link entry if needed (Call, int or exception. not for jmp or iret)

  sendstringf("Setting newTSS state (%x:%8)\n\r", newTSS->CS, newTSS->EIP);

  if (TScause==0 || TScause==3)
    newTSS->Previous_Task_Link=oldTSSselector;

  //set the current gueststate to this
  vmregisters->rax=newTSS->EAX;
  vmregisters->rcx=newTSS->ECX;
  vmregisters->rdx=newTSS->EDX;
  vmregisters->rbx=newTSS->EBX;
  vmregisters->rbp=newTSS->EBP;
  vmregisters->rsi=newTSS->ESI;
  vmregisters->rdi=newTSS->EDI;

  //selectors
  vmwrite(0x800,newTSS->ES);
  vmwrite(0x802,newTSS->CS);
  vmwrite(0x804,newTSS->SS);
  vmwrite(0x806,newTSS->DS);
  vmwrite(0x808,newTSS->FS);
  vmwrite(0x80a,newTSS->GS);
  vmwrite(0x80c,newTSS->LDTss);
  vmwrite(0x80e,newTSSselector);

  ULONG ldtlimit;
  if (newTSS->LDTss)
  {
    ULONG ldtbase;


    //get address and size of LDT and map it
    ldtbase=(gdt[(newTSS->LDTss >> 3)].Base24_31 << 24) + gdt[(newTSS->LDTss >> 3)].Base0_23;
    ldtlimit=(gdt[(newTSS->LDTss >> 3)].Limit16_19 << 16) + gdt[(newTSS->LDTss >> 3)].Limit0_15;
    ldt=mapVMmemory(currentcpuinfo, ldtbase, ldtlimit, NULL, NULL);
  }

  //limits
  vmwrite(0x4800,getSegmentLimit(gdt,ldt,newTSS->ES));
  vmwrite(0x4802,getSegmentLimit(gdt,ldt,newTSS->CS));
  vmwrite(0x4804,getSegmentLimit(gdt,ldt,newTSS->SS));
  vmwrite(0x4806,getSegmentLimit(gdt,ldt,newTSS->DS));
  vmwrite(0x4808,getSegmentLimit(gdt,ldt,newTSS->FS));
  vmwrite(0x480a,getSegmentLimit(gdt,ldt,newTSS->GS));
  vmwrite(0x480c,getSegmentLimit(gdt,ldt,newTSS->LDTss));
  vmwrite(0x480e,newsize); //taskregister size

  //bases
  vmwrite(0x6806,getSegmentBase(gdt,ldt,newTSS->ES));
  vmwrite(vm_guest_cs_base,getSegmentBase(gdt,ldt,newTSS->CS));
  vmwrite(vm_guest_ss_base,getSegmentBase(gdt,ldt,newTSS->SS));
  vmwrite(0x680c,getSegmentBase(gdt,ldt,newTSS->DS));
  vmwrite(0x680e,getSegmentBase(gdt,ldt,newTSS->FS));
  vmwrite(0x6810,getSegmentBase(gdt,ldt,newTSS->GS));
  vmwrite(0x6812,getSegmentBase(gdt,ldt,newTSS->LDTss));
  vmwrite(0x6814,newTSSaddress); //tr base

  //set accessed bits in segments
  setDescriptorAccessedFlag(gdt,ldt,newTSS->ES);
  setDescriptorAccessedFlag(gdt,ldt,newTSS->CS);
  setDescriptorAccessedFlag(gdt,ldt,newTSS->SS);
  setDescriptorAccessedFlag(gdt,ldt,newTSS->DS);
  setDescriptorAccessedFlag(gdt,ldt,newTSS->FS);
  setDescriptorAccessedFlag(gdt,ldt,newTSS->GS);
  setDescriptorAccessedFlag(gdt,ldt,newTSS->LDTss);
  setDescriptorAccessedFlag(gdt,ldt,newTSSselector);



  //access rights
  vmwrite(0x4814,getSegmentAccessRights(gdt,ldt,newTSS->ES));
  vmwrite(0x4816,getSegmentAccessRights(gdt,ldt,newTSS->CS));
  vmwrite(0x4818,getSegmentAccessRights(gdt,ldt,newTSS->SS));
  vmwrite(0x481a,getSegmentAccessRights(gdt,ldt,newTSS->DS));
  vmwrite(0x481c,getSegmentAccessRights(gdt,ldt,newTSS->FS));
  vmwrite(0x481e,getSegmentAccessRights(gdt,ldt,newTSS->GS));
  vmwrite(0x4820,getSegmentAccessRights(gdt,ldt,newTSS->LDTss));
  vmwrite(0x4822,getSegmentAccessRights(gdt,ldt,newTSSselector)); //tr base

  guestrflags.value=newTSS->EFLAGS;

  if (TScause==0 || TScause==3) //call or int
    guestrflags.NT=1; //set New task flag

  guestrflags.RF=1; //at least execute one instruction
  vmwrite(vm_guest_rflags,guestrflags.value);
  vmwrite(vm_guest_rip,newTSS->EIP);
  vmwrite(vm_guest_rsp,newTSS->ESP);

  //set CR3

  currentcpuinfo->guestCR3=newTSS->CR3;

  if ((vmread(vm_cr0_read_shadow) & 0x80000001) == 0x80000001) //if in protected mode with paging
    emulatePaging(currentcpuinfo); //restart paging


  //set the TS bit in cr0
  vmwrite(vm_cr0_read_shadow, vmread(0x6004) | 0x8);
  vmwrite(vm_guest_cr0, vmread(vm_guest_cr0) | 0x8);


  //set L flags on dr7 to 0

  regDR7 DR7;
  DR7.DR7=vmread(vm_guest_dr7);
  DR7.L0=0;
  DR7.L1=0;
  DR7.L2=0;
  DR7.L3=0;
  DR7.LE=0;
  vmwrite(vm_guest_dr7,DR7.DR7);

  if (gdt)
    unmapVMmemory(gdt, gdtlimit);

  if (ldt)
    unmapVMmemory(ldt, ldtlimit);

  if (oldTSS)
    unmapVMmemory(oldTSS, oldsize);

  if (newTSS)
    unmapVMmemory(newTSS, newsize);

  //sendstring("Emulated a taskswitch\n\r");
  return 0;

}



int handleIOAccess(VMRegisters *vmregisters UNUSED)
{
#if (defined SERIALPORT) && (SERIALPORT != 0)
  //nosendchar[getAPICID()]=0;
  //sendstringf("Handling IO access\n\r");


  IOExit_Qualification iodata;
  iodata.Exit_Qualification=vmread(vm_exit_qualification);
  //check Line Control Register to see if DLAB is on
  char dlab=fakecom1.Line_Control_Register >> 7;

  ULONG value8=vmregisters->rax & 0xff;
  //ULONG value16=vmregisters->rax & 0xffff;
  //ULONG value32=vmregisters->rax;





  if ( (iodata.isstring==0) && (iodata.hasrep==0) )
    vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));   //adjust eip to go after this instruction (we handled/emulated it)
  else
  {
    nosendchar[getAPICID()]=0;
    sendstringf("isstring or hasrep is set\n\r");
    return 1;
  }



  if (iodata.size>0)
  {
    nosendchar[getAPICID()]=0;
    sendstringf("iodata.size is bigger than 0. Not supported yet\n\r");
    return 1;
  }


  vmregisters->rax=vmregisters->rax | 0xff;
  return 0;

  switch (iodata.portnr)
  {



    case SERIALPORT: //3f8
    {
      //  Transmitter_Holding_Buffer; //baseport+0 write
      //  Receiver_Buffer; //baseport+0 read
      //  Devisor_Latch_Low; //baseport+0 read/write DLAB=1
      //

      if (iodata.direction==0) //write
      {

        if (dlab)
        {
          //write to Devisor_Latch_Low
          fakecom1.Devisor_Latch_Low=value8;
        }
        else
        {
          //write to Transmitter_Holding_Buffer
          char x;
          x=inportb(SERIALPORT+5);
          while ((x & 0x20) != 0x20)
            x=inportb(SERIALPORT+5);

          outportb(SERIALPORT,value8); //send to comport
        }
      }
      else  //read
      {
        if (dlab)
        {
          //read from Devisor_Latch_Low
          vmregisters->rax=(vmregisters->rax & 0xffffffffffffff00) + fakecom1.Devisor_Latch_Low;
        }
        else
        {
          //read from Receiver_Buffer
          vmregisters->rax=(vmregisters->rax & 0xffffffffffffff00) + inportb(SERIALPORT);
        }
      }


      return 0;
    }


    case SERIALPORT+1:  //3f9
    {
      // unsigned char Interrupt_Enable_Register; //baseport+1 read/write
      // unsigned char Devisor_Latch_High; //baseport+1 read/write DLAB=1
      //

      if (iodata.direction==0) //write
      {
        if (dlab)
        {
          fakecom1.Devisor_Latch_High=value8;
        }
        else
        {
          fakecom1.Interrupt_Enable_Register=value8;
        }
      }
      else //read
      {
        if (dlab)
        {
          vmregisters->rax=(vmregisters->rax & 0xffffffffffffff00) + fakecom1.Devisor_Latch_High;
        }
        else
        {
          vmregisters->rax=(vmregisters->rax & 0xffffffffffffff00) + fakecom1.Interrupt_Enable_Register;
        }
      }
      return 0;
    }

    case SERIALPORT+2: //3fa
    {
      //  unsigned char Interrupt_Identification_Register; //baseport+2 read
      //  unsigned char FIFO_Control_Register; //baseport+2 write

      if (iodata.direction==0) //write
      {
        fakecom1.FIFO_Control_Register=value8;
      }
      else //read
      {
        vmregisters->rax=(vmregisters->rax & 0xffffffffffffff00) + fakecom1.Interrupt_Identification_Register;
      }

      return 0;
    }

    case SERIALPORT+3: //3fb
    {
      //  unsigned char Line_Control_Register; //baseport+3 read/write

      if (iodata.direction==0) //write
      {
        fakecom1.Line_Control_Register=value8;
      }
      else //read
      {
        vmregisters->rax=(vmregisters->rax & 0xffffffffffffff00) + fakecom1.Line_Control_Register;
      }

      return 0;
    }

    case SERIALPORT+4: //3fc
    {
      //unsigned char Modem_Control_Register; //baseport+4 read/write

      if (iodata.direction==0) //write
      {
        fakecom1.Modem_Control_Register=value8;
      }
      else //read
      {
        vmregisters->rax=(vmregisters->rax & 0xffffffffffffff00) + fakecom1.Modem_Control_Register;
      }

      return 0;
    }

    case SERIALPORT+5: //3fd
    {
      //unsigned char Line_Status_Register; //baseport+5 read
      if (iodata.direction==1) //read (there is no write)
      {
        vmregisters->rax=(vmregisters->rax & 0xffffffffffffff00) + inportb(SERIALPORT+5);
      }

      return 0;
    }

    case SERIALPORT+6: //3fe
    {
      //unsigned char Modem_Status_Register; //baseport+6 read
      if (iodata.direction==1) //read (there is no write)
      {
        vmregisters->rax=(vmregisters->rax & 0xffffffffffffff00) + inportb(SERIALPORT+6);
      }

      return 0;

    }

    case SERIALPORT+7: //3ff
    {
      if (iodata.direction==0) //write
      {
        fakecom1.Scratch_Register=value8;
      }
      else //read
      {
        vmregisters->rax=(vmregisters->rax & 0xffffffffffffff00) + fakecom1.Scratch_Register;
      }

      return 0;
    }

    default:
    {
      if (iodata.direction==0)
      {
        //iodata.
        outportb(iodata.portnr,value8);
        return 0;
      }

      break;

    }

  }
  sendstring("Not supported port break\n\r");
  return 0; //not yet handled
#else
  return 0; //just let it slip...
#endif

}

int handleWRMSR(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{
  unsigned long long newvalue=((unsigned long long)vmregisters->rdx << 32)+vmregisters->rax;
  unsigned int msr=vmregisters->rcx;
  int error=0;
  int exceptionnr;

  sendstringf("emulating WRMSR(%x,%6)\n\r", msr, newvalue);


  switch (msr)
  {
    case IA32_TIME_STAMP_COUNTER:
      writeMSR(msr, newvalue);
      //while (1); //<--test
      currentcpuinfo->lowestTSC=0;
      globalTSC=newvalue;
      currentcpuinfo->lastTSCTouch=newvalue;

      lowestTSC=0;

      break;

    case IA32_FEATURE_CONTROL_MSR:
      return raiseGeneralProtectionFault(0); //this msr is locked
      break;

    case IA32_TSC_ADJUST:
      writeMSR(msr, newvalue);
      globalTSC=_rdtsc();
      currentcpuinfo->lasttsc=_rdtsc();
      currentcpuinfo->lastTSCTouch=globalTSC;
      currentcpuinfo->lowestTSC=0;
      lowestTSC=0;
      break;

    case 0x174: //sysenter_CS
      currentcpuinfo->sysenter_CS=newvalue;
      if (!currentcpuinfo->hidden_sysenter_modification)
      {
        currentcpuinfo->actual_sysenter_CS=newvalue;
        vmwrite(vm_guest_IA32_SYSENTER_CS,currentcpuinfo->actual_sysenter_CS); //not hidden, so allow this change
      }
      break;

    case 0x175: //sysenter_ESP
      currentcpuinfo->sysenter_ESP=newvalue;
      if (!currentcpuinfo->hidden_sysenter_modification)
      {
        currentcpuinfo->actual_sysenter_ESP=newvalue;
        vmwrite(vm_guest_IA32_SYSENTER_ESP,currentcpuinfo->actual_sysenter_ESP);
      }
      break;

    case 0x176: //sysenter_EIP
      currentcpuinfo->sysenter_EIP=newvalue;
      if (!currentcpuinfo->hidden_sysenter_modification)
      {
        currentcpuinfo->actual_sysenter_EIP=newvalue;
        vmwrite(vm_guest_IA32_SYSENTER_EIP,currentcpuinfo->actual_sysenter_EIP);
      }
      break;

    case IA32_DEBUGCTL_MSR:
    {
      ultimap_handleMSRWrite(currentcpuinfo, msr, newvalue);
      break;
    }

    case IA32_DS_AREA:
    {
      ultimap_handleMSRWrite(currentcpuinfo, msr, newvalue);
      break;
    }

    case 0xc0000080:
      {
        //unsigned long long new_efer=newvalue;
        //unsigned long long old_efer=readMSR(0xc0000080);
        //int orig=nosendchar[getAPICID()];
        //nosendchar[getAPICID()]=0;

        sendstringf("EFER edit. New value=%8\n\r",newvalue);
        currentcpuinfo->efer=newvalue & 0xfffffffffffffbff; //copy, except the LMA bit, lme stays to remember the users intention

        if (hasUnrestrictedSupport)
        {
          vmwrite(vm_guest_IA32_EFER, newvalue);
        }
        else
          writeMSR(0xc0000080,currentcpuinfo->efer | (1<<8) | (1<<10)); //write msr for the other bits, but keep LME/LMA 1

        if ((currentcpuinfo->efer >> 8) & 1)
        {
          //guest wants to enable 64-bit mode, allow it, but check if paging is used or not, and if so, reload paging system, else do nothing and handle it at cr0 edits
          sendstring("LME has been set to 1\n\r");

          if ((currentcpuinfo->guestCR0 & 0x80000001)==0x80000001) //paged and protected mode
          {
            //paging
            sendstring("Paging is enabled, so setting LMS\n\r");
            vmwrite(vm_entry_controls, vmread(vm_entry_controls) | (1<<9)); //set bit 9, ia32e mode
            emulatePaging(currentcpuinfo); //reload the tables with the new
          }
          else
          {
            //realmode or nonpaged mode, don't enable ia32e mode
            sendstring("In realmode or nonpaged mode. Don\'t enable yet\n\r");
            vmwrite(vm_entry_controls, vmread(vm_entry_controls) & 0xfffffffffffffdff);
          }
        }
        else
        {
          //undo ia32e mode if it was on
          vmwrite(vm_entry_controls, vmread(vm_entry_controls) & 0xfffffffffffffdff);
          sendstring("LME is 0\n\r");
        }

        //nosendchar[getAPICID()]=orig;
        break;
      }

    default:
      //probably a mttr access.
      //just do it but flush the tlb if paging is enabled
      nosendchar[getAPICID()]=0;
      sendstringf("Unexpected MSR write (%x,%6)\n",msr, newvalue);

      try
      {
        writeMSR(msr, newvalue);
      }
      except
      {
        error=1;
        exceptionnr=lastexception & 0xff;
      }
      tryend

      if (error)
      {
        nosendchar[getAPICID()]=0;
        sendstringf("Exception during MSR write (interrupt %d)\n",exceptionnr);
        return raiseGeneralProtectionFault(0);
      }


      if ((currentcpuinfo->guestCR0 & 0x80000001)==0x80000001)
        emulatePaging(currentcpuinfo); //flushes the virtual tlb

      break;

  }

  vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));   //adjust eip to go after this instruction (we handled/emulated it)
  return 0;

}

int RDMSRcounter=0;

int handleRDMSR(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{

  int error=0;
  int exceptionnr;

  sendstring("emulating RDMSR\n\r");
  RDMSRcounter++;


  unsigned long long result;
  unsigned int msr=vmregisters->rcx;

  sendstringf("msr=%x\n", msr);

  switch (msr)
  {
    case IA32_TIME_STAMP_COUNTER:
      return handle_rdtsc(currentcpuinfo, vmregisters); //handles the register setting and rip adjustment


	  case IA32_FEATURE_CONTROL_MSR:
	    result=readMSRSafe(IA32_FEATURE_CONTROL_MSR);
	    result=result | FEATURE_CONTROL_LOCK; //set the LOCK bit (so the system thinks it can't be changed anymore)

	    result=result & ~(FEATURE_CONTROL_VMXON_SMX); //unset the VMX capability in SMX mode

	    if (emulatevmx==0)
	      result=result & ~(FEATURE_CONTROL_VMXON); //unset the VMX capability
	    break;

    case 0x174: //sysenter_CS
      result=currentcpuinfo->sysenter_CS;
      break;

    case 0x175: //sysenter_ESP
      result=currentcpuinfo->sysenter_ESP;
      break;

    case 0x176: //sysenter_EIP
      result=currentcpuinfo->sysenter_EIP;
      break;

    case IA32_DEBUGCTL_MSR:
      result=ultimap_handleMSRRead(currentcpuinfo, msr);
      break;

    case IA32_DS_AREA:
      result=ultimap_handleMSRRead(currentcpuinfo, msr);
      break;



    case 0xc0000080: //IA32_EFER_LME
    {
      //int orig;
      if (hasUnrestrictedSupport)
        result=vmread(vm_guest_IA32_EFER);
      else
        result=currentcpuinfo->efer;

      //set the LMA bit to 1 if LME = 1 , protected mode=1, and paging is 1

      if (((result >> 8)&1)==1)
      {
        if ((currentcpuinfo->guestCR0 & 0x80000001)==0x80000001) //lme=1, and in paged protected mode
          result=result | (1<<10); //set lma to 1
        else
          result=result & (~(QWORD)(1<<10)); //unset LMA
      }

      //nosendchar[getAPICID()]=0;
      sendstringf("read efer. Returning %x\n\r",result);


      //nosendchar[getAPICID()]=orig;


      break;
    }

    case IA32_RTIT_CTL_MSR:
    {
      sendstringf("Exit for IA32_RTIT_CTL_MSR\n");
      result=readMSRSafe(IA32_RTIT_CTL_MSR);

      result=result & 0x00000000ffffffffULL; //for now just keep it simple and keep it at 0, later when windows does a check if it can set the bit, return a shadow value accordingly
      break;
    }

    default:
      sendstringf("MSR read event for msr that wasn\'t supposed to cause an exit (%x)!!!\n\r",msr);

      try
      {
        result=readMSR(msr);
        sendstringf("Unexpected MSR %x returned %6\n", msr, result);
      }
      except
      {
        error=1;
        exceptionnr=lastexception & 0xff;
      }
      tryend


      if (error)
      {
        //nosendchar[getAPICID()]=0;
        sendstringf("Exception(%d) during MSR %x read.  Emulating GPF(0)", exceptionnr, msr);
        return raiseGeneralProtectionFault(0);
      }

      break;
  }

  // ECX=index EDX:EAX is output
  vmregisters->rax=(ULONG)result;
  vmregisters->rdx=(ULONG)(result >> 32);

  vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));   //adjust eip to go after this instruction (we handled/emulated it)
  return 0;

}


criticalSection cpuidsourcesCS={.name="cpuidsourcesCS", .debuglevel=2};

int handleCPUID(VMRegisters *vmregisters)
{
//  sendstring("handling CPUID\n\r");

  //UINT64 oldeax=vmregisters->rax;
  RFLAGS flags;
  flags.value=vmread(vm_guest_rflags);

  if (flags.TF==1)
  {
    vmwrite(vm_pending_debug_exceptions,0x4000);
  }


  _cpuid(&(vmregisters->rax),&(vmregisters->rbx),&(vmregisters->rcx),&(vmregisters->rdx));

  /*
  if (oldeax==1)
  {
    //remove the hypervisor active bit (bit 31 in ecx)
    vmregisters->rcx=vmregisters->rcx & (~(1 << 31));

    if ((vmregisters->rcx & (1<<26)) && (vmread(vm_guest_cr4) & CR4_OSXSAVE)) //doe sit have OSXSave capabilities and is it enabled ?
      vmregisters->rcx=vmregisters->rcx | (1 << 27); //the guest has activated osxsave , represent that in cpuid
  }*/


  /*
  if (oldeax==1)
  {
    //remove vmx capability in ecx
    vmregisters->rcx=vmregisters->rcx & (~(1 << 5)); //set bit 5 to 0
  }*/

  //if (oldeax==0x80000001)
  //{
//    vmregisters->edx = vmregisters->edx & (0xffffffff ^ ((1<<19) | (1<<13)));

  //}

  /*
  if (oldeax==0x80000002)
  {
    char *x;
    x=(char *)&(vmregisters->rax);
    x[0]='I';
    x[1]='n';
    x[2]='t';
    x[3]='e';

    x=(char *)&(vmregisters->rbx);
    x[0]='l';
    x[1]='(';
    x[2]='R';
    x[3]=')';

    x=(char *)&(vmregisters->rcx);
    x[0]=' ';
    x[1]='F';
    x[2]='u';
    x[3]='c';

    x=(char *)&(vmregisters->rdx);
    x[0]='k';
    x[1]='(';
    x[2]='T';
    x[3]='M';
  }

  if (oldeax==0x80000003)
  {
    char *x;
    x=(char *)&(vmregisters->rax);
    x[0]=')';
    x[1]='1';
    x[2]='6';
    x[3]=' ';

    x=(char *)&(vmregisters->rbx);
    x[0]='C';
    x[1]='P';
    x[2]='U';
    x[3]=' ';

  }*/


  //lower the TSC


//  lockedQwordIncrement(&TSCOffset, cpuidTime);


  //TSCOffset+=cpuidTime;

  incrementRIP(vmread(vm_exit_instructionlength));



  getcpuinfo()->lastTSCTouch=_rdtsc();
  return 0;

}

void setRegister(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, int general_purpose_register, UINT64 value)
{
  if (!IS64BITCODE(currentcpuinfo))
    value=value & 0xffffffff;

	switch (general_purpose_register)
  {
    case 0: //RAX
      if (isAMD)
        currentcpuinfo->vmcb->RAX=value;
      else
        vmregisters->rax=value;
      break;

    case 1: //RCX
      vmregisters->rcx=value;
      break;

    case 2: //RDX
      vmregisters->rdx=value;
      break;

    case 3: //RBX
      vmregisters->rbx=value;
      break;

    case 4: //RSP
      if (isAMD)
        currentcpuinfo->vmcb->RSP=value;
      else
        vmwrite(vm_guest_rsp,value);
      break;

    case 5: //RBP
      vmregisters->rbp=value;
      break;

    case 6: //RSI
      vmregisters->rsi=value;
      break;

    case 7: //RDI
      vmregisters->rdi=value;
      break;

    case 8: //RDI
      vmregisters->r8=value;
      break;

    case 9: //RDI
      vmregisters->r9=value;
      break;

    case 10: //RDI
      vmregisters->r10=value;
      break;

    case 11: //RDI
      vmregisters->r11=value;
      break;

    case 12: //RDI
      vmregisters->r12=value;
      break;

    case 13: //RDI
      vmregisters->r13=value;
      break;

    case 14: //RDI
      vmregisters->r14=value;
      break;

    case 15: //RDI
      vmregisters->r15=value;
      break;

  }

}

UINT64 getRegister(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, int general_purpose_register)
/* called by handleCRaccess on the changeCR event */
{

	switch (general_purpose_register)
  {
  	case 0: //RAX
  	  if (isAMD)
  	    return currentcpuinfo->vmcb->RAX;
  	  else
  	    return vmregisters->rax;

    case 1: //RCX
      return vmregisters->rcx;

    case 2: //RDX
      return vmregisters->rdx;

    case 3: //RBX
			return vmregisters->rbx;

    case 4: //RSP
      if (isAMD)
        return currentcpuinfo->vmcb->RSP;
      else
        return vmread(vm_guest_rsp);

    case 5: //RBP
      return vmregisters->rbp;

    case 6: //RSI
      return vmregisters->rsi;

    case 7: //RDI
      return vmregisters->rdi;

    case 8: //R8
      return vmregisters->r8;

    case 9: //R9
      return vmregisters->r9;

    case 10: //R10
      return vmregisters->r10;

    case 11: //R11
      return vmregisters->r11;

    case 12: //R12
      return vmregisters->r12;

    case 13: //R13
      return vmregisters->r13;

    case 14: //R14
      return vmregisters->r14;

    case 15: //R15
      return vmregisters->r15;
	}

  return 0;

}

int setVM_CR0(pcpuinfo currentcpuinfo, UINT64 newcr0)
{
  QWORD cr4=vmread(vm_guest_cr4);
  UINT64 oldcr0=currentcpuinfo->guestCR0;//;vmread(0x6004); //fake cr0
  currentcpuinfo->guestCR0=newcr0;


  if (hasUnrestrictedSupport)
  {
    sendstring("setVM_CR0 in unrestricted mode\n");
    currentcpuinfo->efer=vmread(vm_guest_IA32_EFER);
  }

  //hardcode ET (bit4) to 1
  newcr0=newcr0 | 0x10;

  if ((cr4 & CR4_PCIDE) && ((newcr0 & CR0_PG)==0)) //check if PCIDE is enabled and that paging is on
  {
    raiseGeneralProtectionFault(0);
    return 2;
  }

  if (hasUnrestrictedSupport==0)
  {
    vmwrite(vm_cr0_read_shadow,newcr0); //adjust fake cr0 to this cr0




    //--------------------------------------------------------------//
    //                        ia32e mode check                      //
    //--------------------------------------------------------------//
    if ((oldcr0 & 0x80000001) != (currentcpuinfo->guestCR0 & 0x80000001))
    {
      sendstring("ia32e check: protectedmode or paging bit changed\n\r");



      //page and/or protectedmode bit got changed
      if  (((currentcpuinfo->guestCR0 & 0x80000001)==0x80000001) //protected mode
          &&                                                     //and
          ((currentcpuinfo->efer >> 8) & 1))                     //efer bit 8=1
      {
        //Access_Rights reg_traccessrights;
        sendstring("New state has paging and protectedmode and LME=1, switching to ia32e mode\n\r");
        //protectedmode with paging and LME is 1, set ia32e mode to 1


        vmwrite(vm_entry_controls, vmread(vm_entry_controls) | VMENTRYC_IA32E_MODE_GUEST); //set bit 9: ia32e guest

      }
      else
      {
        sendstring("New state doesn't have paging and protectedmode, or LME=0, setting ia32e mode to disabled\n\r");
        //unset it, doesn't matter if it was 0 or 1
        vmwrite(vm_entry_controls, vmread(vm_entry_controls) & (~(VMENTRYC_IA32E_MODE_GUEST)));

        if (hasUnrestrictedSupport)
        {
          sendstring("unrestricted guest, so turning of EFER.LMA\n");
          currentcpuinfo->efer=currentcpuinfo->efer & (~(1<<10));
          vmwrite(vm_guest_IA32_EFER, currentcpuinfo->efer);
          //vmwrite(vm_guest_IA32_EFER, 0);
        }


      }

    }
  }
  //--------------------------------------------------------------------

  if (hasUnrestrictedSupport)
  {
    QWORD ActualFixed0=IA32_VMX_CR0_FIXED0&0xFFFFFFFF7FFFFFFEULL;  //PG and PE can be 0 in unrestricted guest mode
    vmwrite(vm_guest_cr0, newcr0 | ActualFixed0);

    vpid_invalidate();
    ept_invalidate();


    if ((vmread(vm_guest_cr0) & 0x80000001)==0x80000001)
    {
      //PG and PE
      if ((vmread(vm_guest_IA32_EFER) >> 8)&1) //PGE
      {
        vmwrite(vm_entry_controls, vmread(vm_entry_controls) | VMENTRYC_IA32E_MODE_GUEST); //set bit 9: ia32e guest
        vmwrite(vm_guest_IA32_EFER, vmread(vm_guest_IA32_EFER) | (1 << 10));
      }

    }

    vmwrite(vm_cr0_read_shadow,newcr0 & vmread(vm_cr0_guest_host_mask) ); //set the host bits accordingly

    return 0;
  }


  sendstringf("Guest wants to set CR0 to %8 (from %8)\n\r",newcr0,oldcr0);

  //no need to set protected mode
  if ((IA32_VMX_CR0_FIXED1 & newcr0) != newcr0)
  {
    sendstringf("THE GUEST OS WANTS TO SET A BIT THAT SHOULD STAY 0\n\r");
    raiseGeneralProtectionFault(0);
    return 2; //not an error, but don't change RIP
  }

  sendstringf("oldcr0=%8\n\r",oldcr0);
  sendstringf("newcr0=%8\n\r",newcr0);


  if (((newcr0 & 1)==1) && ((oldcr0 & 1)==0))
  {
    //switched from realmode to protected
    sendstring("Switching from realmode to protectedmode\n\r");

    //set exit on HLT off, since no emulation is needed

    vmwrite(vm_execution_controls_cpu, vmread(vm_execution_controls_cpu) & (~HLT_EXITING)); // (ULONG)IA32_VMX_PROCBASED_CTLS | ( 1 << 9 ) | (1 << 25) | (1 << 28) );

    sendstringf("IA32_VMX_PROCBASED_CTLS=%8\n\r",IA32_VMX_PROCBASED_CTLS);


    //from realmode to protected
    //disable vm and set iopl to 0, and set some other needed states
    Access_Rights reg_tempcsaccessrights,tempaccessrights;
    UINT64 guestrflags;
    PRFLAGS pguestrflags=(PRFLAGS)&guestrflags;

    //set tss to the guest's tss

    vmwrite(vm_guest_tr_access_rights,currentcpuinfo->TSaccessRights);
    vmwrite(vm_guest_tr_limit,currentcpuinfo->TSlimit);
    vmwrite(vm_guest_tr_base,currentcpuinfo->TSbase);
    vmwrite(vm_guest_tr,currentcpuinfo->TSsegment);


    //16 bit segments, but leave base intact
    //dword
    reg_tempcsaccessrights.AccessRights=0; //unusable till loaded again

    reg_tempcsaccessrights.Segment_type=15; //conforming code segment
    reg_tempcsaccessrights.S=1;
    reg_tempcsaccessrights.DPL=0;
    reg_tempcsaccessrights.P=1;
    reg_tempcsaccessrights.G=0;
    reg_tempcsaccessrights.D_B=0;
    reg_tempcsaccessrights.unusable=0; //1;
    vmwrite(vm_guest_cs_access_rights,(ULONG)reg_tempcsaccessrights.AccessRights); //cs access rights
    vmwrite(vm_guest_cs,vmread(vm_guest_cs) & 0xfffc);

    tempaccessrights.AccessRights=0;
    tempaccessrights.Segment_type=3; //0011
    tempaccessrights.S=1;
    tempaccessrights.DPL=3;
    tempaccessrights.P=1;
    tempaccessrights.G=0;
    tempaccessrights.D_B=0;
    tempaccessrights.unusable=0;

    vmwrite(vm_guest_es_access_rights,(ULONG)tempaccessrights.AccessRights);
    vmwrite(vm_guest_ds_access_rights,(ULONG)tempaccessrights.AccessRights);
    vmwrite(vm_guest_fs_access_rights,(ULONG)tempaccessrights.AccessRights);
    vmwrite(vm_guest_gs_access_rights,(ULONG)tempaccessrights.AccessRights);

    tempaccessrights.DPL=vmread(vm_guest_ss) & 3;
    vmwrite(vm_guest_ss_access_rights,(ULONG)tempaccessrights.AccessRights);


    guestrflags=vmread(vm_guest_rflags);

    if ((pguestrflags->IF) || (currentcpuinfo->hasIF))
    {
      nosendchar[getAPICID()]=0;
      sendstringf("IF is not 0 when switching to protected mode\n\r");
      ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
      while (1) outportb(0x80,0xd4);
    }

    pguestrflags->VM=0;  //out of realmode
    pguestrflags->TF=0;  //trap flag can go off now
    pguestrflags->IF=currentcpuinfo->hasIF; //restore, but I bet it's 0
    pguestrflags->IOPL=0;
    vmwrite(vm_guest_rflags,(ULONG)guestrflags); //rflags

    //set GDT and IDT to what it should be
    vmwrite(vm_guest_gdtr_base, currentcpuinfo->RealMode.GDTBase);
    vmwrite(vm_guest_gdt_limit, currentcpuinfo->RealMode.GDTLimit);
    vmwrite(vm_guest_idtr_base, currentcpuinfo->RealMode.IDTBase);
    vmwrite(vm_guest_idt_limit, currentcpuinfo->RealMode.IDTLimit);



    if (newcr0 & (1<<31))
    {
      sendstringf("Emulating paging because the page bit is 1 as well\n\r");
      emulatePaging(currentcpuinfo);
      vmwrite(vm_guest_cr4,vmread(0x6006) | readMSR(0x488)); //no special overrides, set to what the guest thinks it was and will become
    }
    else
    {
      setupNonPagedPaging(currentcpuinfo);
      vmwrite(vm_guest_cr4,vmread(0x6006) | readMSR(0x488) | (1 << 4) | ( 1 << 5)); //guest's cr4, with fixed CR4 and PAE and PSE
    }

  }
  else
  if (((oldcr0 & 1)==1) && ((newcr0 & 1)==0))
  {

    //Access_Rights reg_csaccessrights,reg_traccessrights;
    RFLAGS guestrflags=(RFLAGS)(UINT64)0;


    sendstring("Switching from protected mode to real mode.  \n\r");

    //set exit on HLT on, since emulation is needed
    vmwrite(vm_execution_controls_cpu,vmread(vm_execution_controls_cpu) | HLT_EXITING  );





    //switch to realmode

    //save TSS (for when the TSS needs to be changed for virtual realmode to work)

    currentcpuinfo->TSaccessRights=vmread(0x4822);
    currentcpuinfo->TSlimit=vmread(0x480e);
    currentcpuinfo->TSbase=vmread(0x6814);
    currentcpuinfo->TSsegment=vmread(0x80e);

    guestrflags=(RFLAGS)vmread(vm_guest_rflags);

#ifdef DEBUG
    if ((guestrflags.IF) || (currentcpuinfo->hasIF))
    {
      nosendchar[getAPICID()]=0;
      sendstringf("IF is not 0 when switching to protected mode\n\r");
      while (1) outportb(0x80,0xd5);
    }
#endif


    guestrflags.IOPL=3;
    guestrflags.VM=1;
    currentcpuinfo->hasIF=guestrflags.IF;
    vmwrite(vm_guest_rflags,guestrflags.value);

    currentcpuinfo->RealMode.GDTBase=vmread(vm_guest_gdtr_base);
    currentcpuinfo->RealMode.GDTLimit=vmread(vm_guest_gdt_limit);
    currentcpuinfo->RealMode.IDTBase=vmread(vm_guest_idtr_base);
    currentcpuinfo->RealMode.IDTLimit=vmread(vm_guest_idt_limit);




    //set real CR0
    vmwrite(vm_guest_cr0,(ULONG)IA32_VMX_CR0_FIXED0 | /*(1 << 16) |*/ vmread(0x6004)); //forced+wp+guest

    //set real CR4
    vmwrite(vm_guest_cr4,(ULONG)IA32_VMX_CR4_FIXED0 | CR4_VME | CR4_PSE | CR4_PAE | vmread(vm_cr4_read_shadow)); //VME, PAE and PSE
    setupRealModePaging(currentcpuinfo); //normal realmode
    return 0;

  }

  if ((newcr0 & (1<<31)) && (!(oldcr0 & (1<<31))))
  {
    sendstring("Switching from nonpaged mode to paged mode\n\r");
    if ((newcr0 & 1)==1)
    {
      sendstring("In protected mode so enable paging\n\r");
      //switch from nonpaged to paged
      emulatePaging(currentcpuinfo);

      //set CR4 to what the guest wants it to be (so remove the forced PAE stuff and restore the disabled bits)
      vmwrite(vm_guest_cr4,(ULONG)IA32_VMX_CR4_FIXED0 | vmread(0x6006)); //gueststate + cr4 forced1
    }
    else
    {
      sendstring("Error we arn\'t in protected mode yet.\n\r");
    }

  }
  else
  if ((oldcr0 & (1<<31)) && (!(newcr0 & (1<<31))))
  {
    sendstring("Switching from pagedmode to nonpaged mode\n\r");
    if ((newcr0 & 1)==1)
    {
      Access_Rights reg_ssAccessRights, reg_csAccessRights;
      sendstring("In protected mode so disable paging\n\r");
      //switch from paged to nonpaged

      setupNonPagedPaging(currentcpuinfo);
      vmwrite(vm_guest_cr4,vmread(vm_guest_cr4) | (1 << 4) | ( 1 << 5)); //PAE and PSE enabled

      //needs a valid SS, so make sure SS is now valid
      reg_ssAccessRights.AccessRights=vmread(vm_guest_ss_access_rights);

      if (reg_ssAccessRights.unusable==1)
      {
        reg_ssAccessRights.unusable=0;
        reg_ssAccessRights.Segment_type=3; //0011
        reg_ssAccessRights.S=1;
        reg_ssAccessRights.DPL=0;
        reg_ssAccessRights.P=1;
        reg_ssAccessRights.G=1;
        reg_ssAccessRights.D_B=1;
        reg_ssAccessRights.unusable=0;
        vmwrite(vm_guest_ss,8);
        vmwrite(vm_guest_ss_base,0);
        vmwrite(vm_guest_ss_limit,0xffffffff);

        vmwrite(vm_guest_ss_access_rights,reg_ssAccessRights.AccessRights);


        reg_csAccessRights.AccessRights=vmread(vm_guest_cs_access_rights);
        reg_csAccessRights.L=0;
        vmwrite(vm_guest_cs_access_rights,reg_csAccessRights.AccessRights);
      }


    }
    else
    {
      sendstring("Error we arn\'t in protected mode yet.\n\r");
    }

  }



  newcr0=IA32_VMX_CR0_FIXED0 | newcr0 /*| ( 1 << 16)*/; //mix it with the required bits and set the WP bit to 1 so even ring0 is bound to readonly pages (used for marking regions dirty and can certainly be used for other 'interesting' stuff)
  vmwrite(vm_guest_cr0,newcr0);  //set new cr0 (real one)


  sendstringf("fake cr0 has been set to %8\n\r",vmread(0x6004));
  sendstringf("real cr0 has been set to %8 (%8)\n\r",newcr0,vmread(vm_guest_cr0));

  if  ((vmread(0x6004) & 0x80000001)==0x80000001)
  {
    ULONG difference = oldcr0 ^ vmread(0x6004);
    if (
        (difference & (1<<16)) || //WP
        (difference & (1<<29)) || //NW
        (difference & (1<<30)) //PE
       )
    {
      emulatePaging(currentcpuinfo); //flushes the virtual tlb
    }

  }


  sendstringf("CS-base=%x\n\r",vmread(vm_guest_cs_base));
  sendstringf("SS-base=%x\n\r",vmread(vm_guest_ss_base));
  sendstringf("DS-base=%x\n\r",vmread(vm_guest_ds_base));
  sendstringf("ES-base=%x\n\r",vmread(vm_guest_es_base));
  sendstringf("FS-base=%x\n\r",vmread(vm_guest_fs_base));
  sendstringf("GS-base=%x\n\r",vmread(vm_guest_gs_base));

  return 0;

}

int setVM_CR3(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, UINT64 newcr3)
/*
 * Called when the system changes the CR3 register
 */
{
  int shouldInvalidate=hasVPIDSupport || isAMD;
  sendstringf("setVM_CR3: Setting CR3 (%6)\n\r", newcr3);


  //This is likely a contextswitch. Update the lowestTSC to normal
  currentcpuinfo->lowestTSC=_rdtsc(); //todo: apply speedhack speed
  currentcpuinfo->lastTSCTouch=0;


  //Ultimap
  if ((currentcpuinfo->Ultimap.Active) && (!isAMD))
    ultimap_handleCR3Change(currentcpuinfo, currentcpuinfo->guestCR3, newcr3);


  /* From intel reference docs:
   * If CR4.PCIDE = 1, bit 63 of the source operand to MOV to CR3 determines whether the instruction invalidates
entries in the TLBs and the paging-structure caches (see Section 4.10.4.1, “Operations that Invalidate TLBs and
Paging-Structure Caches,” in the Intel® 64 and IA-32 Architectures Software Developer’s Manual, Volume 3A). The
instruction does not modify bit 63 of CR3, which is reserved and always 0.
   */

  QWORD cr4=isAMD?currentcpuinfo->vmcb->CR4:vmread(vm_guest_cr4);

  sendstringf("cr4=%6\n", cr4);

  if ((cr4 & CR4_PCIDE) && (newcr3 & 0x8000000000000000ULL))
  {
    sendstringf("CR4_PCIDE is enabled and CR3 bit 63 has been set\n");
    newcr3=newcr3&0x7FFFFFFFFFFFFFFFULL;
    //don't invalidate the TLB and caches

    shouldInvalidate=0;

    sendstringf("Skipping the cache invalidating\n");
  }

  //check if CR3 sets physical address bits it shouldn't, and if so, GPF, unless it's bit 63
  if (newcr3 & ~MAXPHYADDRMASK)
  {
    //has set bits beyond acceptable range, GPF
    sendstringf("Tried to set %6.  (Bits violating MAXPHYADDRMASK=%6)\n", newcr3, newcr3 & ~MAXPHYADDRMASK);
    raiseGeneralProtectionFault(3);
    return 0;
  }

  if (CR3ValueLog)
  {
    csEnter(&CR3ValueLogCS);
    if ((CR3ValueLog) && (CR3ValuePos<512)) //512*8=4096
    {
      int i;
      int found=0;
      for (i=0; i<CR3ValuePos; i++)
      {
        if (CR3ValueLog[i]==newcr3)
        {
          found=1;
          break;
        }
      }
      if (found==0)
      {
        CR3ValueLog[CR3ValuePos]=newcr3;
        CR3ValuePos++;
      }
    }
    csLeave(&CR3ValueLogCS);
  }

  currentcpuinfo->guestCR3=newcr3;

  if (!IS64BITPAGING(currentcpuinfo))
    currentcpuinfo->guestCR3=currentcpuinfo->guestCR3 & 0xffffffff;

  if ((hasUnrestrictedSupport) || (isAMD))
  {
    if (isAMD)
    {
      currentcpuinfo->vmcb->CR3=newcr3;
      currentcpuinfo->vmcb->VMCB_CLEAN_BITS&=~(1 << 5); //CR3 changed
    }
    else
      vmwrite(vm_guest_cr3,newcr3);

    if (shouldInvalidate)
    {
      if (isAMD)
      {
        currentcpuinfo->vmcb->TLB_CONTROL=1;
      }
      else
      {
        int type=3;
        INVVPIDDESCRIPTOR desc;
        desc.LinearAddress=0;
        desc.zero=0;
        desc.VPID=1;

        if (has_VPID_INVVPIDSingleContextRetainingGlobals)
          type=3;
        else
        if (has_VPID_INVVPIDSingleContext)
          type=2;
        else
          type=1; //all

        _invvpid(type, &desc);
      }
    }
    return 0;
  }

  //if paging is enabled then set the pagetable to this else just do it and ignore till it is enabled
  if (!isAMD)
  {
    if (vmread(vm_cr0_read_shadow) & (1<<31))
    {
      if (currentcpuinfo->cr3_callback.cr3_change_callback)
        return handle_cr3_callback(currentcpuinfo,vmregisters);

      //paging is enabled, emulate the pagetable update
      sendstringf("Paging is enabled, so emulate the pagetable update\n\r");

      emulatePaging(currentcpuinfo);
    }
    else
    {
      sendstringf("paging isn't enabled , so don't apply the update yet\n\r");
    }

    sendstringf("guestCR3=%8\n\r",currentcpuinfo->guestCR3);
    sendstringf("realguestCR3=%8\n\r",vmread(vm_guest_cr3));
  }

  return 0;
}

int setVM_CR4(pcpuinfo currentcpuinfo, UINT64 newcr4)
{

  UINT64 oldCR4=(vmread(vm_guest_cr4) & (~vmread(vm_cr0_guest_host_mask))) | (vmread(vm_cr4_read_shadow) & vmread(vm_cr4_guest_host_mask));
  UINT64 newCR4=newcr4;
  UINT64 IA32_VMX_CR4_FIXED0=readMSR(0x488);
  UINT64 IA32_VMX_CR4_FIXED1=readMSR(0x489);

  if (!IS64BITCODE(currentcpuinfo))
    newCR4=newCR4 & 0xffffffff;

  sendstring("setVM_CR4(...)\n\r");

  if ((IA32_VMX_CR4_FIXED1 & newCR4) != newCR4)
  {
    sendstringf("THE GUEST OS WANTS TO SET A BIT THAT SHOULD STAY 0\n\r");
    return 1;
  }

  if (hasVPIDSupport & (((newCR4 & (CR4_PGE))) != ((oldCR4 & (CR4_PGE)))))
  {
	  //invalidate tlb

	  INVVPIDDESCRIPTOR desc;
	  desc.LinearAddress=0;
	  desc.zero=0;
	  desc.VPID=1;
	  _invvpid(1,&desc);
  }

  if (((newCR4 & (CR4_PCIDE))) && ((oldCR4 & (CR4_PCIDE))==0))
  {
    //do some tests
    QWORD CR3;
    QWORD EFER;
    if (hasUnrestrictedSupport)
    {
      CR3=vmread(vm_guest_cr3);
      EFER=vmread(vm_guest_IA32_EFER);
    }
    else
    {
      CR3=currentcpuinfo->guestCR3;
      EFER=currentcpuinfo->efer;
    }

    if (((EFER & (1<<10))==0) || (CR3 & 0xfff)) //check if LMA=1
    {
      raiseGeneralProtectionFault(0);
      return 2; //not an error, but don't change RIP
    }
  }




  sendstringf("Set fake CR4 to %x  (old fake was %x)\n\r",newCR4, oldCR4);
  if (hasUnrestrictedSupport)
    vmwrite(vm_cr4_read_shadow,newCR4 & vmread(vm_cr4_guest_host_mask) ); //set the host bits accordingly
  else
    vmwrite(vm_cr4_read_shadow,newCR4); //set the fake CR4

  newCR4=IA32_VMX_CR4_FIXED0 | newCR4; //add the forced bits to it


  if (hasUnrestrictedSupport)
  {
    vmwrite(vm_guest_cr4, newCR4);
    return 0;
  }



  //if paging is enabled then set the pagetable to this else just do it and ignore till it is enabled

  if (vmread(vm_cr0_read_shadow) & CR0_PG)
  {
    //paging is enabled, check if the pae flag has been changed
    sendstringf("CR4 change and Paging is enabled\n\r");


    if ((oldCR4 & CR4_PAE) && ((newCR4 & CR4_PAE)==0))
    {
      //PAE flag got changed and paging is on. Reload paging
      sendstringf("PAE flag got changed, restart paging\n\r");
      emulatePaging(currentcpuinfo);
    }

  }
  else
  {
    //not paging, then make sure PAE is enabled (used for memory emu for real and protected non paged)
    sendstring("cr4 change, but paging is off");
    newCR4=newCR4 | CR4_PAE | CR4_PSE; //PSE and PAE bits set
  }

  if ((vmread(vm_cr0_read_shadow) & 1)==0) //not in protected mode, so emulate paging. We use PAE to identity map
    newCR4=newCR4 | (1<<4) | (1<<5);

  if (ISREALMODE(currentcpuinfo))
  {
    sendstringf("Inside realmode, so set VME\n\r");
    newCR4=newCR4 | 1; //enable VME
  }
  else
  {
    sendstringf("Not in realmode\n\r");
  }

  if (ISPAGING(currentcpuinfo)==0)
  {
    sendstringf("No paging yet: vmread(vm_cr0_fakeread)=%x \n", vmread(vm_cr0_read_shadow));
    newCR4=newCR4 & 0xFFCFFFFF; //disable SMEP/SMAP //debug
  }

  sendstringf("Setting real CR4 to %x\n\r",newCR4);
  vmwrite(vm_guest_cr4,newCR4);


  if ((vmread(vm_cr0_read_shadow) & 0x80000001) == 0x80000001) //protectedmode with paging enabled
  {
    //paging is on
    //check if one of the paging bis is changed
    UINT64 difference=oldCR4 ^ vmread(vm_cr4_read_shadow);

    if (
        (difference & (1<<4)) || //PSE
        (difference & (1<<5)) //PAE
       )
    {
      sendstring("Paging bits changed\n\r");

      emulatePaging(currentcpuinfo); //flushes the virtual tlb
    }

  }
  else
  {
    setupNonPagedPaging(currentcpuinfo);
  }

  return 0;

}


int handleCRaccess(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{
  int result;

	ULONG exit_qualification=vmread(vm_exit_qualification);
  unsigned char CR_number=exit_qualification & 0xf;
  unsigned char accesstype=(exit_qualification >> 4) & 3;
 // unsigned char LSMW_operandtype=(exit_qualification >> 6) & 1;
  unsigned char general_purpose_register=(exit_qualification >> 8) & 0xf;
  unsigned char LMSW_sourcedata=(exit_qualification >> 16) & 0xFFFF;

  sendstringf("Handling control register access (CR_number=%d)\n\r",CR_number);

  switch (CR_number)
  {

    case 0:
    {

      //cr0
      switch (accesstype)
      {

	      case 0: //move to CR0
        case 2: //clts
        case 3: //lmsw
        {

		      UINT64 newcr0;
          UINT64 oldcr0;
          oldcr0=currentcpuinfo->guestCR0;//;vmread(0x6004); //fake cr0


          if (accesstype==2) //clts
          {
            //disable TS flag
            sendstring("CLTS\n\r");
            newcr0=(oldcr0 & 0xFFFFFFFFFFFFFFF7ULL); //bit 4 set to 0
            sendstringf("PUTTING THE VALUE OF %8 INTO CR0\n\r", newcr0);
          }

          else
          if (accesstype==3)
          {
            sendstringf("LMSW\n\r");
            //only the first 4 bits are affected
            newcr0=(oldcr0 & 0xFFFFFFFFFFFFFFF0ULL) | (LMSW_sourcedata & 0xf);

            sendstringf("PUTTING THE VALUE OF %8 INTO CR0\n\r", newcr0);
          }
          else
          {
            sendstringf("0:PUTTING THE VALUE OF REGISTER %d INTO CR0\n\r", general_purpose_register);
					  newcr0=getRegister(currentcpuinfo, vmregisters,general_purpose_register);
            if (!IS64BITCODE(currentcpuinfo))
              newcr0=newcr0 & 0xffffffff; //32-bit mask
          }

          result=setVM_CR0(currentcpuinfo, newcr0);

          if (result==0)
            vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));   //adjust eip to go after this instruction (we handled/emulated it)

          sendstringf("new eip=%x\n\r",vmread(vm_guest_rip));
          return 0;
        }


        case 1: //move from CR0
        {
          sendstringf("THE OS REQUESTED CR0 INTO REGISTER %d \n\r",general_purpose_register);

					setRegister(currentcpuinfo, vmregisters,general_purpose_register,vmread(0x6004));

          vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));   //adjust eip to go after this instruction (we handled/emulated it)
          return 0;
        }

        default:
        {
          sendstring("Unknown CR0 access\n\r");
          return 1;
        }
      }

      break;


    }

    case 3:
    {
      //cr3
      sendstringf("Handling CR3 access\n\r");
      switch (accesstype)
      {
        case 0: //move to CR3
        {

          /*
          sendstringf("3:PUTTING THE VALUE OF REGISTER %d INTO CR3\n\r", general_purpose_register);


					currentcpuinfo->guestCR3=getRegister(currentcpuinfo, vmregisters,general_purpose_register);
          if (!IS64BITPAGING(currentcpuinfo))
            currentcpuinfo->guestCR3=currentcpuinfo->guestCR3 & 0xffffffff;

					//if paging is enabled then set the pagetable to this else just do it and ignore till it is enabled
					if (vmread(0x6004) & (1<<31))
					{
            if (currentcpuinfo->cr3_callback.cr3_change_callback)
              return handle_cr3_callback(currentcpuinfo,vmregisters);

						//paging is enabled, emulate the pagetable update
            sendstringf("Paging is enabled, so emulate the pagetable update\n\r");

						emulatePaging(currentcpuinfo);
					}
          else
            sendstringf("paging isn't enabled , so don't apply the update yet\n\r");


          sendstringf("guestCR3=%8\n\r",currentcpuinfo->guestCR3);
          */

          result = setVM_CR3(currentcpuinfo, vmregisters, getRegister(currentcpuinfo, vmregisters,general_purpose_register));
          if (result==0)
          {
            vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength)); //adjust eip to go after this instruction (we handled/emulated it)
          }



					return 0;
        }

        case 1: //move from CR3
        {
          sendstringf("THE OS REQUESTED CR3 INTO REGISTER %d \n\r",general_purpose_register);
          if (hasUnrestrictedSupport)
            setRegister(currentcpuinfo, vmregisters,general_purpose_register,vmread(vm_guest_cr3));
          else
            setRegister(currentcpuinfo, vmregisters,general_purpose_register,currentcpuinfo->guestCR3);
          vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));   //adjust eip to go after this instruction (we handled/emulated it)

          return 0;
        }

        default:
        {
          sendstring("Unknown CR3 access\n\r");
          return 1;
        }

      }

      break;
    }


    case 4:
    {

      //cr4
      sendstringf("Handling CR4 access\n\r");
      switch (accesstype)
      {
        case 0: //move to CR4
        {
          //UINT64 oldCR4=vmread(0x6006);
          UINT64 newCR4=getRegister(currentcpuinfo, vmregisters,general_purpose_register);


          sendstringf("PUTTING THE VALUE OF REGISTER %d INTO CR4 (%8)\n\r", general_purpose_register,newCR4);
          result=setVM_CR4(currentcpuinfo, newCR4);

          if (result==0)
            vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));   //adjust eip to go after this instruction (we handled/emulated it)


					return 0;
        }


        case 1: //move from CR4
        {
          sendstringf("THE OS REQUESTED CR4 INTO REGISTER %d \n\r",general_purpose_register);
					setRegister(currentcpuinfo, vmregisters,general_purpose_register,vmread(0x6004));
          vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));   //adjust eip to go after this instruction (we handled/emulated it)
          return 0;
        }


        default:
        {
          sendstring("Unknown CR4 access\n\r");
          return 1;
        }


      }


      break;
    }




    default:
    {
      sendstringf("CR_number=%x. Not supported!\n\r",CR_number);
      return 1;
    }


  }


  return 1;
}


int ex=0;

int isBenignInterrupt(int interrupt)
{
  switch (interrupt)
  {
    case 1 ... 7:
    case 9:
    case 16 ... 19:
      return 1;

  }

  return 0;
}

int isContributoryInterrupt(int interrupt)
{
  switch (interrupt)
  {
    case 0:
    case 10 ... 13:
      return 1;
  }

  return 0;
}

int handleRealModeInt0x15(pcpuinfo currentcpuinfo UNUSED, VMRegisters *vmregisters, int instructionsize)
{
  sendstring("Int 15h software interrupt\n\r");

  if (ARDcount==0)
    initARDcount();

  // Check if it is one of the functions needed to hook, if not, emulate and continue
  if (((WORD)(vmregisters->rax) & 0xff00)==0x8800)
  {
  //  nosendchar[getAPICID()]=0;
    sendstringf("Handling int 15h, AH=88 . instructionsize=%d\n\r", instructionsize);




    vmregisters->rax=(vmregisters->rax & 0xffffffff00000000ULL)+0xfc00; //64MB, if less, well, screw you, why even use dbvm ?

    vmwrite(vm_guest_rip,vmread(vm_guest_rip)+instructionsize); //eip to next
    vmwrite(vm_guest_rflags,vmread(vm_guest_rflags) & 0xFFFFFFFFFFFFFFFEULL); //clear carry flag
    return 0; //handled
  }

  if (((ULONG)(vmregisters->rax) & 0xffff)==(ULONG)0xe801)
  {
    int i;

    DWORD between1and16MB=0; //in KB, max 3c00  (between 0x100000 and 0x1000000)
    DWORD above16MB=0;




    nosendchar[getAPICID()]=0;
    sendstringf("Handling int 15h, AH=e801. ARDcount=%d \n\r",ARDcount);

    for (i=0; i<ARDcount; i++)
    {
      sendstringf("i=%d\n",i);
      sendstringf("between1and16MB=%x\n",between1and16MB);
      sendstringf("above16MB=%x\n",above16MB);

      if (fakeARD[i].BaseAddrHigh>0)
        continue;



      if ((fakeARD[i].Type==1) && ((fakeARD[i].BaseAddrLow+fakeARD[i].LengthLow)>0x100000))
      {
        //upper mem, and available
        DWORD start=fakeARD[i].BaseAddrLow;
        DWORD stop=fakeARD[i].BaseAddrLow+fakeARD[i].LengthLow;

        if (start<0x100000)
          start=0x100000;

        if (start<0x1000000)
        {
          DWORD tempstop=stop;
          if (tempstop>0x1000000)
            tempstop=0x1000000;

          between1and16MB+=tempstop-start;
          start=tempstop;
        }

        if (start>=0x1000000)
          above16MB+=stop-start;


      }
    }

    sendstringf("After for loop\n");
    sendstringf("between1and16MB=%x\n",between1and16MB);
    sendstringf("above16MB=%x\n",above16MB);

    vmregisters->rax=(vmregisters->rax & 0xffffffffffff0000ULL) + (between1and16MB / 1024);
    vmregisters->rbx=(vmregisters->rbx & 0xffffffffffff0000ULL) + (above16MB / (64*1024));
    vmregisters->rcx=(vmregisters->rcx & 0xffffffffffff0000ULL) + (between1and16MB / 1024);
    vmregisters->rdx=(vmregisters->rdx & 0xffffffffffff0000ULL) + (above16MB / (64*1024));

    vmwrite(vm_guest_rip,vmread(vm_guest_rip)+instructionsize); //eip to next
    vmwrite(vm_guest_rflags,vmread(vm_guest_rflags) & 0xFFFFFFFFFFFFFFFEULL); //clear carry flag
    return 0;

  }

  if (((ULONG)vmregisters->rax & 0xffff)==(ULONG)0xe820)
  {
    int startindex=(ULONG)vmregisters->rbx;

    //return 1;
    nosendchar[getAPICID()]=0;

    sendstringf("Handling int 15h, ax=E820 (maxindex=%d)\n\r",ARDcount-1);
    sendstringf("startindex=%d vmregisters->rcx=%d\n\r",startindex,vmregisters->rcx);


    if (((ULONG)vmregisters->rcx >= 20) && ((ULONG)vmregisters->rdx==0x534D4150) && (startindex<ARDcount))
    {
      //call=ok
      PARD output=(PARD)(vmread(vm_guest_es_base)+(vmregisters->rdi & 0xffff)); //es:di
      int totalentries=(ULONG)vmregisters->rcx/20;
      int o,i;
      vmregisters->rax=(vmregisters->rax & 0xffffffff00000000ULL) + 0x534D4150;

      sendstringf("totalentries=%d\n\r",totalentries);

      i=startindex;
      o=0;
      while ((o<totalentries) && (i<ARDcount) )
      {
        output[o]=fakeARD[i];
        if (output[o].Type==255)
          output[o].Type=2;

        o++;
        i++;
      }

      //set next index, i already contains the value of the next index
      if (i>=ARDcount)
      {
        vmregisters->rbx=(vmregisters->rbx & 0xffffffff00000000ULL) + 0;
      }
      else
      {
        vmregisters->rbx=(vmregisters->rbx & 0xffffffff00000000ULL) + i;
      }

      vmregisters->rcx=(vmregisters->rcx & 0xffffffff00000000ULL) + (o*20);
      vmwrite(vm_guest_rip,vmread(vm_guest_rip)+instructionsize); //eip to next
      vmwrite(vm_guest_rflags,vmread(vm_guest_rflags) & 0xFFFFFFFFFFFFFFFEULL); //clear carry flag

      sendstringf("Handled int15h ax=e820. ECX=%8 \n\r",(ULONG)vmregisters->rcx);
      return 0; //handled
    }
    else
    {
      //return error
      sendstringf("Returning error\n\r");
      vmwrite(vm_guest_rip,vmread(vm_guest_rip)+instructionsize); //eip to next
      vmwrite(vm_guest_rflags,vmread(vm_guest_rflags) | 1); //set carry flag
      return 0; //handled
    }

  }

  if (((ULONG)vmregisters->rax & 0xffff)==(ULONG)0xe881)
  {
    nosendchar[getAPICID()]=0;
    sendstring("int 0x15:ax=0xe881 is being used\n");
  }

  return 1; //unhandled
}

int handleInterruptRealMode(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{
  //gather data
  //Access_Rights reg_csaccessrights;


  //ULONG interrorcode;//,idtvectorerrorcode;
  //VMExit_interruption_information intinfo;
  VMExit_idt_vector_information idtvectorinfo;
  //VMEntry_interruption_information entry_intinfo;


  //intinfo.interruption_information=vmread(vm_exit_interruptioninfo);
  //interrorcode=vmread(vm_exit_interruptionerror);
  idtvectorinfo.idtvector_info=vmread(vm_idtvector_information);
  //idtvectorerrorcode=vmread(vm_idtvector_error);



  //reg_csaccessrights.AccessRights=vmread(vm_guest_cs_access_rights);
//todo: in unrestricted mode do a realmode int



  if (idtvectorinfo.valid)
  {
    //this was caused by an interrupt that happened in realmode and needs the vm to handle it, but there is no idt setup to handle it, so raises a gpf or other int
    //DWORD *idtvector=(DWORD*)vmread(vm_guest_idtr_base);

    DWORD issoftware;

    sendstringf("currentcpuinfo=%6\n\r", (UINT64)currentcpuinfo);
    sendstringf("currentcpuinfo->RealMode.IDTBase=%6\n\r", (UINT64)currentcpuinfo->RealMode.IDTBase);


    if (idtvectorinfo.type==4)
    {
      sendstringf("idtvectorinfo.type==4\n\r");
      issoftware=vmread(vm_exit_instructionlength);
      if (issoftware!=2)
      {
        unsigned char firstbyte=0;
        //bochs? check first byte of the instruction to confirm
        ReadVMMemory(currentcpuinfo,vmread(vm_guest_cs_base)+vmread(vm_guest_rip),&firstbyte,1);
        if (firstbyte==0xcd)
        {
          sendstring("BOCHS BUG. Instruction is CD xx, so size IS 2\n");
          issoftware=2;
        }


      }

    }
    else
      issoftware=0;

    //emulate the call to this int



    //read the idt and figure out where the interrupt is
    if ((idtvectorinfo.interruptvector==0x15) && issoftware )  //bios function call
    {
      int r=handleRealModeInt0x15(currentcpuinfo, vmregisters, issoftware);

      if (r==0)
        return r;
    }

    //still here so not a hooked routine

    //set eip to the next instruction if it was a software interrupt

    vmwrite(vm_guest_rip, vmread(vm_guest_rip)+issoftware);

    //and emulate the interrupt
    return emulateRMinterrupt(currentcpuinfo, vmregisters, idtvectorinfo.interruptvector);
  }
  else
  {
    //a normal exception happened. Most common:gpf  Try to emulate this instruction manually (happens with 'special case' instructions
    emulateRealMode(currentcpuinfo, vmregisters);
    //this might result in an invalud state, but the next vmresume iteration will then deal with that based on the state
    return 0;
  }
}

int handleInterruptProtectedMode(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{
  //UINT64 fakeCR0=vmread(vm_cr0_fakeread); //guestCR0
  //UINT64 fakeCR4=vmread(0x6006);

  //ULONG interrorcode,idtvectorerrorcode;
  VMExit_interruption_information intinfo;
  VMExit_idt_vector_information idtvectorinfo;
  //VMEntry_interruption_information entry_intinfo;
  int doublefault=0;
  int isFault=1;

  intinfo.interruption_information=vmread(vm_exit_interruptioninfo);
  //interrorcode=vmread(vm_exit_interruptionerror);
  idtvectorinfo.idtvector_info=vmread(vm_idtvector_information);
  //idtvectorerrorcode=vmread(0x440a);


  //protected mode int handling
  sendstring("protected mode interrupt handling\n\r");


  if (intinfo.interruptvector==1)
  {
	//emulate the breakpoint interrupt
    regDR7 dr7;
    dr7.DR7=vmread(vm_guest_dr7);

    regDR6 dr6;
    int orig=nosendchar[getAPICID()];


    isFault=0; //isDebugFault(vmread(vm_exit_qualification), dr7.DR7);

    nosendchar[getAPICID()]=0;
    sendstring("Interrupt 1:\n");

    dr6.DR6=getDR6();

    regDR6 dr6_exit_qualification;
    dr6_exit_qualification.DR6=vmread(vm_exit_qualification);


    //The documentation says about the exit qualification: Any of these bits may be set even if its corresponding enabling bit in DR7 is not set.
    //The documentation also says for dr6: They may or may not be set if the breakpoint is not enabled by the Ln or the Gn flags in register DR7.
    //therefore, just passing them 1 on 1

    //also: Certain debug exceptions may clear bits 0-3. The remaining contents of the DR6 register are never cleared by the processor.
    dr6.DR6&= ~(0xf); //zero the b0 to b3 flags

    /*
    RFLAGS rflags;
    rflags.value = vmread(vm_guest_rflags);
    if(rflags.TF)
    {
      sendstring("TF is 1");
      dr6.DR6 |= dr6_exit_qualification.DR6 & 0x600f; //the 4 b0-b3 flags, BS and BD
    }
    else
    {
      sendstring("TF is 0");
      dr6.DR6 |= dr6_exit_qualification.DR6 & 0x200f; //the 4 b0-b3 flags, BD
    }
    */

    dr6.DR6 |= dr6_exit_qualification.DR6 & 0x600f; //the 4 b0-b3 flags, BS and BD
    dr6.RTM=~dr6_exit_qualification.RTM;
    //if ((dr6_exit_qualification.RTM)==0) dr6.RTM=1; //if this is 0, set RTM to 1

    setDR6(dr6.DR6);

    if (currentcpuinfo->Ultimap.Active)
      ultimap_handleDB(currentcpuinfo);
    else
      vmwrite(vm_guest_IA32_DEBUGCTL, vmread(vm_guest_IA32_DEBUGCTL) & ~1); //disable the LBR bit ( if it isn't already disabled)


    //set GD to 0
    dr7.GD=0;
    vmwrite(vm_guest_dr7,dr7.DR7);

    nosendchar[getAPICID()]=orig;


    //interrupt redirection for int 1
    if (int1redirection_idtbypass==0)
    {
      //simple int1 redirection, or not even a different int
      sendstring("Normal\n\r");
      intinfo.interruptvector=int1redirection;
      currentcpuinfo->int1happened=(int1redirection!=1); //only set if redirection to something else than 1
    }
    else
    {
      int r;
      //emulate the interrupt completly, bypassing the idt vector and use what's given in
      //int14redirection_idtbypass_cs and int14redirection_idtbypass_rip




      nosendchar[getAPICID()]=1; //I believe this works
      r=emulateExceptionInterrupt(currentcpuinfo, vmregisters,
          int1redirection_idtbypass_cs, int1redirection_idtbypass_rip,
          intinfo.haserrorcode, vmread(vm_exit_interruptionerror), isFault);

      nosendchar[getAPICID()]=orig;

      if (r==0)
        return 0;

      //else failure to handle it

    }
  }
  else
  if (intinfo.interruptvector == 3)
  {

	  //software bp
      nosendchar[getAPICID()]=0;
      sendstring("Int3 bp\n");

      sendvmstate(currentcpuinfo, vmregisters);

      isFault=0;
      if (int3redirection_idtbypass == 0)
      {
        //simple int3 redirection, or not even a different int
        sendstring("Normal\n\r");
        intinfo.interruptvector=int3redirection;
        currentcpuinfo->int3happened=(int3redirection!=3); //only set if redirection to something else than 3
      }
      else
      {
        nosendchar[getAPICID()]=0;
        sendstring("int 3\n");

        vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength)); //adjust this automatically (probably 1)

        int r=emulateExceptionInterrupt(currentcpuinfo, vmregisters,
            int3redirection_idtbypass_cs, int3redirection_idtbypass_rip,
            intinfo.haserrorcode, vmread(vm_exit_interruptionerror), 0);

        if (r==0)
          return 0;
      }
  }
  else
  if (intinfo.interruptvector == 14)
  {
      //check if the IgnorePageFaults feature is enabled and if it's a non instruction fetch bp
      if ((currentcpuinfo->IgnorePageFaults.Active) && (vmread(vm_exit_interruptionerror) <=0xf ) )//NO instructiob fetch
      {
        currentcpuinfo->IgnorePageFaults.LastIgnoredPageFault=vmread(vm_exit_qualification);
        vmwrite(vm_guest_rip, vmread(vm_guest_rip)+vmread(vm_exit_instructionlength)); //set eip to the next instruction
        return 0;
      }

      setCR2(vmread(vm_exit_qualification));

      if (int14redirection_idtbypass == 0)
      {
        //simple int14 redirection, or not even a different int
        sendstring("Normal\n\r");
        intinfo.interruptvector=int14redirection;
        currentcpuinfo->int14happened=(int14redirection!=14); //only set if redirection to something else than 14
      }
      else
      {
        int r;

        if (intinfo.haserrorcode==0)
        {
          nosendchar[getAPICID()]=0;
          sendstring("int 14 without errorcode\n");
        }

        if (idtvectorinfo.valid)
        {
          nosendchar[getAPICID()]=0;
          sendstring("int 14 from an IDT\n");
        }

        if (idtvectorinfo.type!=0)
        {
          nosendchar[getAPICID()]=0;
          sendstringf("int 14 type is %d instead of 3\n", idtvectorinfo.type);
        }



        sendstring("Interrupt 14:");

        r=1;
        if (intinfo.haserrorcode)
        {
          int errorcode=vmread(vm_exit_interruptionerror);
          if ((errorcode & 0x15)==0x15)
          {
            //if it happens because of the NX bit
            nosendchar[getAPICID()]=0;

            sendstringf("Errorcode=%x\n", vmread(vm_exit_interruptionerror));
            sendstringf("CR2=%x\n", vmread(vm_exit_qualification));

            r=emulateExceptionInterrupt(currentcpuinfo, vmregisters,
                int14redirection_idtbypass_cs, int14redirection_idtbypass_rip,
                intinfo.haserrorcode, errorcode, 1);
          }


        }



        if (r==0) //it got handled?
        {
          sendstring("r==0, returning\n");
          return 0; //yes
        }

        intinfo.interruptvector=int14redirection;
        currentcpuinfo->int14happened=(int14redirection!=14);
      }
   }


  //do some double fault checking.
  sendstringf("idtvectorinfo.valid=%d\n\r",idtvectorinfo.valid);
  sendstringf("idtvectorinfo.type=%d\n\r",idtvectorinfo.type);
  sendstringf("idtvectorinfo.interruptvector=%d\n\r",idtvectorinfo.interruptvector);

  sendstringf("intinfo.valid=%d\n\r",intinfo.valid);
  sendstringf("intinfo.type=%d\n\r",intinfo.type);
  sendstringf("intinfo.interruptvector=%d\n\r",intinfo.interruptvector);

  if (idtvectorinfo.valid)
  {
    nosendchar[getAPICID()]=0;
    sendstring("idtvectorinfo is VALID\n");
    sendstringf("idtvectorinfo.type=%d\n",idtvectorinfo.type);
    //this interrupt is the result of a previous interrupt

    if (idtvectorinfo.type==3)
    {
      sendstring("idtvectorinfo.type==3\n");
      if (idtvectorinfo.interruptvector==8)
      {
        nosendchar[getAPICID()]=0;
        sendstring("TRIPPLEFAULT TRIPPLEFAULT!!! OMGWTF?\n");
        sendvmstate(currentcpuinfo, vmregisters);
        displayPreviousStates();
        ShowCurrentInstructions(currentcpuinfo);



        while (1)
        {
          outportb(0x80,0xd6);
          ddDrawRectangle(0,DDVerticalResolution-100,100,100,_rdtsc() & 0xffffff);
        }

      }

      doublefault=1;
      //now try to disprove that it is a doublefault
      if (isBenignInterrupt(idtvectorinfo.interruptvector))
      {
        sendstring("idtvector is benign\n");
        doublefault=0;
      }
      else
      if (isBenignInterrupt(intinfo.interruptvector))
      {
        sendstring("intvector is benign");
        doublefault=0;
      }


      if (intinfo.interruptvector==14)
      {
        sendstring("intvector is 14");
        if (isContributoryInterrupt(idtvectorinfo.interruptvector))
        {
          sendstring("idtvector is contributory\n");
          doublefault=0;
        }
      }

      if (doublefault)
      {
        //one last chance
        doublefault=0;

        sendstring("Likely a doublefault\n");

        if ((isContributoryInterrupt(idtvectorinfo.interruptvector)) &&
            (isContributoryInterrupt(intinfo.interruptvector)))
        {
          //both are contributory exceptions
          sendstring("BOTH are contributory\n");
          doublefault=1; //too bad
        }

        if (idtvectorinfo.interruptvector==14)
        {
          //idt indicated a pagefault
          sendstring("idtvector=14\n");

          if (isContributoryInterrupt(intinfo.interruptvector))
          {
            sendstring("is contributory\n");
            doublefault=1;
          }

          if (intinfo.interruptvector==14)
          {
            sendstring("intvector=14\n");
            doublefault=1;
          }

        }

      }
    }
    else
    {
      //else not a hardware exception to no doublefault

    }


  }




  //normal handling

  VMEntry_interruption_information newintinfo;
  newintinfo.interruption_information=0;
  //send event to guest


  if (doublefault)
  {
    //int originalnosendchar=nosendchar[getAPICID()];

    nosendchar[getAPICID()]=0;

    //pass the doublefault int to the guest
    newintinfo.interruptvector=8; //DF
    newintinfo.type=3; //hardware
    newintinfo.haserrorcode=1; //errorcode
    newintinfo.valid=1;
    vmwrite(vm_entry_exceptionerrorcode, 0); //entry errorcode

    sendstring("DOUBLEFAULT RAISED\n\r");
  }
  else
  {
    //pass the original int to the guest
    newintinfo.interruptvector=intinfo.interruptvector;
    newintinfo.type=intinfo.type;
    newintinfo.haserrorcode=intinfo.haserrorcode;
    newintinfo.valid=intinfo.valid; //should be 1...
    vmwrite(vm_entry_exceptionerrorcode, vmread(vm_exit_interruptionerror)); //entry errorcode
  }

  //nosendchar[getAPICID()]=0;
  sendstringf("CS:EIP=0x%x:0x%x",vmread(vm_guest_cs),vmread(vm_guest_rip));

  sendstringf("newintinfo.interruptvector=%d\n\r",newintinfo.interruptvector);
  sendstringf("newintinfo.type=%d\n\r",newintinfo.type);
  sendstringf("newintinfo.haserrorcode=%d\n\r",newintinfo.haserrorcode);
  if (newintinfo.haserrorcode)
  {
    sendstringf("newintinfo.errorcode=%x\n\r",vmread(0x4018));
  }

  sendstringf("newintinfo.valid=%d\n\r",newintinfo.valid);

  if (newintinfo.type!=5)
  {
    vmwrite(vm_entry_interruptioninfo, newintinfo.interruption_information); //entry info field
    vmwrite(0x401a, vmread(vm_exit_instructionlength)); //entry instruction length
  }
  else
  {
    //int1
    vmwrite(vm_pending_debug_exceptions, vmread(vm_pending_debug_exceptions) | (1<<14) );
    vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));
  }



  if (isFault)
  {
    //set RF flag
    UINT64 rflags=vmread(vm_guest_rflags);
    PRFLAGS prflags=(PRFLAGS)&rflags;
    prflags->RF=1;
    vmwrite(vm_guest_rflags,rflags);
  }

  sendstringf("Protected mode interrupt handled\n\r");
  return 0;
}

BOOL handleHardwareBreakpoint(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, FXSAVE64 *fxsave)
{
  //handle specialized software breakpoints
  sendstringf("Hardware breakpoint\n");
  if (hasEPTsupport || hasNPsupport)
  {
    if (ept_handleHardwareBreakpoint(currentcpuinfo, vmregisters, fxsave))
      return TRUE;
  }

  //future hardware reakpoint handlers here

  //still here
  return FALSE; //unhandled
}

BOOL handleSoftwareBreakpoint(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, FXSAVE64 *fxsave)
{
  //handle specialized software breakpoints
  nosendchar[getAPICID()]=0;
  //sendstringf("Software breakpoint\n");
  if (hasEPTsupport || hasNPsupport)
  {
    if (ept_handleSoftwareBreakpoint(currentcpuinfo, vmregisters, fxsave))
    {
      //sendstring("handleSoftwareBreakpoint: ept_handleSoftwareBreakpoint handled it. Returning TRUE\n");
      return TRUE;
    }
  }

  //future software breakpoint handlers here


  //still here
  nosendchar[getAPICID()]=0;
  sendstringf("Unhandled Software breakpoint\n");
  return FALSE; //unhandled
}

VMSTATUS handleInterrupt(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, FXSAVE64 *fxsave) //nightmare function. Needs rewrite
{
 // int origsc;

  UINT64 fakeCR0=(vmread(vm_guest_cr0) & (~vmread(vm_cr0_guest_host_mask))) | (vmread(vm_cr0_read_shadow) & vmread(vm_cr0_guest_host_mask)); //   vmread(vm_cr0_read_shadow); //guestCR0
  //UINT64 fakeCR4=vmread(0x6006);

  //ULONG interrorcode,idtvectorerrorcode;
  VMExit_interruption_information intinfo;
  //VMExit_idt_vector_information idtvectorinfo;
  //VMEntry_interruption_information enstry_intinfo;
  //int doublefault=0;

  intinfo.interruption_information=vmread(vm_exit_interruptioninfo);


  if ((intinfo.interruptvector==1) && (intinfo.type==itHardwareException))
  {
    if (handleHardwareBreakpoint(currentcpuinfo, vmregisters, fxsave))
      return VM_OK;
  }


  if ((intinfo.interruptvector==3) && (intinfo.type==itSoftwareException))
  {
    if (handleSoftwareBreakpoint(currentcpuinfo, vmregisters, fxsave))
      return VM_OK;
  }




 // interrorcode=vmread(vm_exit_interruptionerror);
  //idtvectorinfo.idtvector_info=vmread(vm_idtvector_information);
  //idtvectorerrorcode=vmread(0x440a);

  //check if according to the guest protected mode is enabled or not
  if ((fakeCR0 & 1)==0)
    return handleInterruptRealMode(currentcpuinfo, vmregisters);     //nope, so we're in real mode emulation
  else
    return handleInterruptProtectedMode(currentcpuinfo, vmregisters);
}


#pragma GCC push_options
#pragma GCC optimize ("O0")
int handleXSETBV(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{
  unsigned long long value=((unsigned long long)vmregisters->rdx << 32)+vmregisters->rax;
  int success=0;

  sendstring("handleXSETBV\n\r");

  currentcpuinfo->LastInterrupt=0;
  currentcpuinfo->OnInterrupt.RIP=(QWORD)((volatile void *)(&&InterruptFired)); //set interrupt location
  currentcpuinfo->OnInterrupt.RSP=getRSP();


  if (vmread(vm_guest_cr4) & CR4_OSXSAVE)
  {
    sendstring("Guest has OSXSAVE enabled\n\r");
    setCR4(getCR4() | CR4_OSXSAVE);
  }
  else
  {
    sendstring("Guest doesn't have OSXSAVE enabled\n\r");
    setCR4(getCR4() & (~CR4_OSXSAVE));
  }

  sendstring("Calling _xsetbv\n");

  _xsetbv(vmregisters->rcx, value);

  sendstring("Returned without exception\n");
  success=1;

InterruptFired:
  currentcpuinfo->OnInterrupt.RIP=0;

  if (!success)
  {
    sendstringf("Exception happened. Interrupt=%d\n\r", currentcpuinfo->LastInterrupt);
    if (currentcpuinfo->LastInterrupt==13)
      raiseGeneralProtectionFault(0);

    if (currentcpuinfo->LastInterrupt==6)
      raiseInvalidOpcodeException(currentcpuinfo);



  }
  else
  {
    vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));
  }
  return 0;
}




int handleSingleStep(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, FXSAVE64 *fxsave)
{
  //handle the reasons one by one. (Used by AMD as well)
  sendstringf("%d: handleSingleStep.  currentcpuinfo->singleStepping.ReasonsPos=%d\n", currentcpuinfo->cpunr, currentcpuinfo->singleStepping.ReasonsPos);


  while (currentcpuinfo->singleStepping.ReasonsPos)
  {
    int i=currentcpuinfo->singleStepping.ReasonsPos-1;
    int r=0;
    sendstringf("%d:  ID %d Reason %d\n", currentcpuinfo->cpunr, i, currentcpuinfo->singleStepping.Reasons[i].Reason);

    switch (currentcpuinfo->singleStepping.Reasons[i].Reason)
    {
      case SSR_HANDLEWATCH: r=ept_handleWatchEventAfterStep(currentcpuinfo, currentcpuinfo->singleStepping.Reasons[i].ID); break;
      case SSR_HANDLECLOAK: r=ept_handleCloakEventAfterStep(currentcpuinfo, (PCloakedPageData)currentcpuinfo->singleStepping.Reasons[i].Data); break;
      case SSR_HANDLESOFTWAREBREAKPOINT: r=ept_handleSoftwareBreakpointAfterStep(currentcpuinfo, currentcpuinfo->singleStepping.Reasons[i].ID); break;
      //todo: case SSR_STEPTILLINTERUPTABLE: return singleStepTillInteruptable
      case SSR_STEPANDBREAK: r=ept_handleStepAndBreak(currentcpuinfo, vmregisters, fxsave, currentcpuinfo->singleStepping.Reasons[i].ID); break;
      default:
      {
        nosendchar[getAPICID()]=0;

        sendstringf("singleStepping memory corruption\n");
        r=1;
      }

    }

    if (r)
    {
      nosendchar[getAPICID()]=0;
      sendstringf("handleSingleStep: r=%d\n",r);
      ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
      while (1) outportb(0x80,0xd7);
    }
    currentcpuinfo->singleStepping.ReasonsPos--;
  }

  sendstring("vmx_disableSingleStepMode\n");
  vmx_disableSingleStepMode();

  sendstringf("return from handleSingleStep.  currentcpuinfo->singleStepping.ReasonsPos=%d\n", currentcpuinfo->singleStepping.ReasonsPos);

  return 0;
}

void speedhack_setspeed(double speed)
{
  if (TSCHooked)
  {
    QWORD currentTime=_rdtsc();

    QWORD initialoffset=(currentTime-speedhackInitialTime)*speedhackSpeed+speedhackInitialOffset;
    speedhackInitialTime=currentTime;

    if (initialoffset<lowestTSC)
      initialoffset=lowestTSC+1000;

    lowestTSC=initialoffset;
    speedhackInitialOffset=initialoffset;
    speedhackSpeed=speed;

    useSpeedhack=1; //once used it's always on
  }
}


int handle_rdtsc(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{
  QWORD t;
  QWORD lTSC=lowestTSC;
  QWORD realtime;


  if (lTSC<currentcpuinfo->lowestTSC)
    lTSC=currentcpuinfo->lowestTSC;


  realtime=_rdtsc();
  //csEnter(&TSCCS);


  if (currentcpuinfo->lowestTSC==0)
    currentcpuinfo->lowestTSC=realtime;



  //speedhack:
  if (useSpeedhack) //(useSpeedhack)
  {
    t=(realtime-speedhackInitialTime)*speedhackSpeed+speedhackInitialOffset;
   // t=globalTSC+s;
  }
  else
    t=realtime; //speedhack disabled ATM globalTSC+s;

  if (lTSC==0)
    lTSC=t;

  if (t<lTSC)
  {
    lTSC=t; //overflow happened... (bp here)

  }


  if (adjustTimestampCounters)
  {
    if ((realtime-currentcpuinfo->lastTSCTouch)<(QWORD)adjustTimestampCounterTimeout) //todo: and not a forbidden RIP
    {

      int off=20+(realtime & 0xf);


      t=currentcpuinfo->lowestTSC+off;
      //t=lTSC+off;

      //writeMSR(0x838, readMSR(0x838));

      //lockedQwordIncrement(&lowestTSC,off);
      currentcpuinfo->lowestTSC=t;

    }
    else
    {
      if (lowestTSC<t)
        lowestTSC=t;
      currentcpuinfo->lowestTSC=t;
    }
  }

  if (isAMD)
    currentcpuinfo->vmcb->RAX = t & 0xffffffff;
  else
    vmregisters->rax=t & 0xffffffff;

  vmregisters->rdx=t >> 32;

  if (lowestTSC<t)
    lowestTSC=t;


  if (isAMD)
  {
    if (AMD_hasNRIPS)
    {
      currentcpuinfo->vmcb->RIP=currentcpuinfo->vmcb->nRIP;
    }
    else
    {
      int i,error;
      UINT64 pagefaultaddress;

     // sendstringf("DB:1:currentcpuinfo->AvailableVirtualAddress=%6\n", currentcpuinfo->AvailableVirtualAddress);

      unsigned char *bytes=(unsigned char *)mapVMmemory(currentcpuinfo, currentcpuinfo->vmcb->cs_base+currentcpuinfo->vmcb->RIP, 15, &error, &pagefaultaddress);
      if (!bytes)
          bytes=mapVMmemory(currentcpuinfo, currentcpuinfo->vmcb->cs_base+currentcpuinfo->vmcb->RIP, pagefaultaddress-currentcpuinfo->vmcb->cs_base+currentcpuinfo->vmcb->RIP, &error, &pagefaultaddress);

      for (i=0; i<15; i++)
      {
        sendstringf("%x ", bytes[i]);
        if (bytes[i]==0x0f)
        {
          sendstringf("%x ", bytes[i+1]);
          sendstringf("%x ", bytes[i+2]);
          sendstringf("%x ", bytes[i+3]);
          currentcpuinfo->vmcb->RIP+=i+2;
          break;
        }
      }

      unmapVMmemory(bytes,15);
    }

  }
  else
  {
    vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));

    RFLAGS flags;
    flags.value=vmread(vm_guest_rflags);

    if (flags.TF==1)
      vmwrite(vm_pending_debug_exceptions,0x4000);
  }

  return 0;
}


#pragma GCC pop_options

int handleVMEvent_internal(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, FXSAVE64 *fxsave)
{
  int result;
  int exit_reason=currentcpuinfo->guest_error?currentcpuinfo->guest_error:vmread(vm_exit_reason) & 0x7fffffff;


#ifdef STATISTICS
  if (exit_reason<=55)
    currentcpuinfo->eventcounter[exit_reason]++;
#endif

  if (currentcpuinfo->eptUpdated==1)
  {
    currentcpuinfo->eptUpdated=0;
    ept_invalidate();
  }


  if (hasUnrestrictedSupport && canToggleCR3Exit)
    currentcpuinfo->guestCR3=vmread(vm_guest_cr3); //for code that needs it



  switch (exit_reason) //exit reason
  {
    case 0: //interrupt
    {
      int result;

      result=handleInterrupt(currentcpuinfo, vmregisters, fxsave);

      //sendstringf("handleInterrupt returned %d", result);

      return result;
    }


    case 1: //
    {
      sendstring("received external interrupt\n\r");
      if (vmread(vm_guest_activity_state)==1)
      {
        sendstring("In HLT mode so become active and disable external event watching\n\r");
        vmwrite(vm_execution_controls_pin,vmread(0x4000) & 0xFFFFFFFE); //disable external event watching


        vmwrite(vm_guest_activity_state,(ULONG)0); //HLT mode off
        while (1) outportb(0x80,0xd8);



        if (ISREALMODE(currentcpuinfo))
        {
          sendstring("Guest is in realmode so go back to realmode\n\r");
          returnToRealmode(currentcpuinfo);
        }




        return 0;
      }
      else
      {
        sendstringf("External event received but not in HLT state\n\r");
        return 1;
      }

    }


    case 2: //tripple fault
    {

      nosendchar[getAPICID()]=0;
      sendstring("A TRIPPLE FAULT HAPPENED. NORMALLY THE SYSTEM WOULD REBOOT NOW\n\r");


      while (1) outportb(0x80,0xd7);

      return 1;
    }

    case 3: //INIT SIGNAL
    {
      //enter wait-for-sipi mode


      sendstring("Received an INIT signal\n\r"); //should enter wait-for-sipi mode
      handleINIT(currentcpuinfo, vmregisters);
      return VM_OK; //ignore?
    }

    case vm_exit_sipi: //SIPI
    {
      if (currentcpuinfo->vmxdata.insideVMXRootMode==1) //don't handle it
        return 0;
      else
        return handleSIPI();
    }


    case 5: //I/O system-management interrupt (SMI)
    {
      sendstring("I/O system-management interrupt (SMI)\n\r");
      return 1;
    }

    case 6: //other SMI
    {
      sendstring("An SMI arrived and caused an SMM VM exit (see Section 24.16.2) but not immediately after retirement of an I/O instruction.\n\r");
      return 1;
    }

    case 7: //interrupt window
    {
      if ((currentcpuinfo->singleStepping.ReasonsPos) && (currentcpuinfo->singleStepping.Method==2))
      {
        sendstring("Interrupt window event... And I am in single stepping mode\n");

        return handleSingleStep(currentcpuinfo, vmregisters, fxsave);
      }
      else
      {
        nosendchar[getAPICID()]=0;
        sendstring("Interrupt window event... I did NOT ask for this\n\r");
        sendstringf("vm_execution_controls_cpu=%6\n", vmread(vm_execution_controls_cpu));

#ifndef DEBUG
        ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
        while (1) outportb(0x80,0xd8);
#endif
      }
      return 0; //ignore for now
    }

    case 8: //NMI window
    {
      sendstring("NMI Window");
      if (currentcpuinfo->NMIOccured)
        raiseNMI();


      vmx_disableNMIWindowExiting(); //vmwrite(vm_guest_interruptability_state,2); for some single stepping fun

      return 0;
    }

    case 9:
    {
      ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
      while (1) outportb(0x80,0xd9);
      return handleTaskswitch(currentcpuinfo, vmregisters);
    }

    case 10: //CPUID
    {
      result=handleCPUID(vmregisters);

      return result;
    }

    case 11:
    {
      //currently not supported
      ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
      //while (1);

      sendstring("GETSEC\n\r");
      raiseInvalidOpcodeException(currentcpuinfo);
      return 0;
    }

    case 12: //HLT
    {
      result=handleHLT(currentcpuinfo);


      return result;
    }

    case 13: //INVD
    {
      nosendchar[getAPICID()]=0;
      sendstring("INVD called\n\r");

      vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));
      _wbinvd();
      //_invd();
      return 0;
    }

    case 14: //INVLPG
    {

      sendstring("Calling handleINVLPG(currentcpuinfo)\n\r");
      result = handleINVLPG(currentcpuinfo);


      sendstringf("Returned from handleINVLPG : %d\n\r",result);
      return result;



    }


    case 15: //RDPMC
    {
      sendstring("RDPMC called\n\r");
      return 1;
    }

    case 16: //RDTSC
    {
      //TSCOffset+=rdtscTime;
      //lockedQwordIncrement(&TSCOffset, rdtscTime);
      int r;



      r=handle_rdtsc(currentcpuinfo, vmregisters);
      currentcpuinfo->lastTSCTouch=_rdtsc();
      return r;
    }

    case 17: //RSM
    {
      sendstring("RSM called\n\r");
      return 1;
    }


    case 18: //VMCALL
    {

      nosendchar[getAPICID()]=0;
      //sendstring("vmcall\n");

      result = handleVMCall(currentcpuinfo, vmregisters);

      //sendstringf("Returned from handleVMCall, result=%d\n\r",result);
      return result;
    }

    case 19 ... 27 : //VMX instruction called
    case 0xce00: //special exit reasons (vmresume/vmlaunch failures)
    case 0xce01:
    {
      ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
      //while (1);

      //jtagbp

      sendstring("VMX instruction called...\n\r");
      return handleIntelVMXInstruction(currentcpuinfo, vmregisters);
      //return raiseInvalidOpcodeException(currentcpuinfo);
    }


    case 28: //Control register access
    {
      int result;

      result=handleCRaccess(currentcpuinfo, vmregisters);


      return result;
    }

    case 29: //Debug register access
    {
      sendstring("The debug registers got accesses\n\r");
      //interesting
      return 1;
    }

    case 30: //IO instruction
    {

      result=handleIOAccess(vmregisters);


      return result;
    }

    case 31: //RDMSR
    {
      result=handleRDMSR(currentcpuinfo, vmregisters);


      return result;
    }


  case 32: //WRMSR
    {
      result=handleWRMSR(currentcpuinfo, vmregisters);
      return result;
    }

  case 33: //inv. guest
    {
      ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
      sendstringf("VM-Entry failure due to invalid guest\n\r");
      result=handleInvalidEntryState(currentcpuinfo, vmregisters);

      if (result)
      {
        nosendchar[getAPICID()]=0;
        sendstringf("Unhandled invalid state\n");
        sendvmstate(currentcpuinfo, vmregisters);
      }



      return result;

    }

    case 34: //inv. MSR
    {
      sendstringf("VM-Entry failure due to MSR loading\n\r");
      return 1;
    }

    case 36: //MWAIT
    {
      sendstringf("MWAIT occured and mwait exiting=1\n\r");
      return 1;
    }


    case vm_exit_monitor_trap_flag:
    {
      if ((currentcpuinfo->singleStepping.ReasonsPos) && (currentcpuinfo->singleStepping.Method==1))
        return handleSingleStep(currentcpuinfo, vmregisters, fxsave);

      nosendchar[getAPICID()]=0;
      sendstring("(Un)expected monitor trap flag\n\r");
#ifndef DEBUG
      ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
      while (1) outportb(0x80,0xda);
#else
      return handleSingleStep(currentcpuinfo, vmregisters, fxsave);
      //return 0;
#endif


    }

    case 39: //MONITOR
    {
      sendstringf("MONITOR occured and MONITOR Exiting=1\n\r");
      return 1;
    }

    case 40: //PAUSE
    {
      sendstringf("PAUSE occured and PAUSE Exiting=1\n\r");
      return 1;
    }

    case 41: //machine check error
    {
      sendstring("MACHINE CHECK ERROR!!!!!!!!!!!!!!!!!!!!!!!!1\n\r");
      return 1;
    }

    case 43: //TPR below threshold
    {
      sendstring("TPR below threshold and TPR threshold set\n\r");
      return 1;
    }

    case 44: //APIC_access
    {
      sendstring("APIC access\n\r");
      return 1;
    }

    case 45: //Virtualized EOI
    {
      sendstring("Virtualized EOI\n\r");
      return 1;
    }

    case 46:
    {
      sendstring("GDT/IDT access\n\r");
      return 1;
    }

    case 47:
    {
      sendstring("LDTR/TR access\n\r");
      return 1;
    }

    case 48:
    {

      int r;


      sendstring("EPT violation\n\r");
      r=handleEPTViolation(currentcpuinfo, vmregisters, (PFXSAVE64)fxsave);

      ept_invalidate();


      return r;
    }

    case 49:
    {
      sendstring("EPT misconfig\n\r");
      return handleEPTMisconfig(currentcpuinfo, vmregisters);
    }

    case 50:
    {
      sendstring("INVEPT\n\r");
      return handleIntelVMXInstruction(currentcpuinfo, vmregisters);

      //return 1;
    }

    case 51:
    {
      QWORD t;
      //RDTSCP
      //TSCOffset+=rdtscpTime;
      //lockedQwordIncrement(&TSCOffset, rdtscpTime);

      //if (currentcpuinfo->cpunr!=0)
      //todo: Calibrate at start to find the system rdtscp calls and don't touch those

      //IS
      //  currentcpuinfo->lastTSCTouch=_rdtsc();

     // t=_rdtsc();
     // currentcpuinfo->lowestTSC=t;
     // lowestTSC=t;

      //vmregisters->rax=t & 0xffffffff;
      //vmregisters->rdx=t >> 32;


      int r=handle_rdtsc(currentcpuinfo, vmregisters);
      currentcpuinfo->lastTSCTouch=_rdtsc();

      vmregisters->rcx=readMSR(IA32_TSC_AUX_MSR);
      return r;
    }

    case vm_exit_vmx_preemptiontimer_reachedzero:
    {
      //IA32_VMX_MISC.IA32_VMX_MISC=readMSR(0x485);


      nosendchar[getAPICID()]=0;
      //sendstringf("%d: %x:%6 (vmm rsp=%6 , freemem=%x)\n", currentcpuinfo->cpunr, vmread(vm_guest_cs),vmread(vm_guest_rip), getRSP(), maxAllocatableMemory());

      vmwrite(vm_preemption_timer_value,10000);

      ddDrawRectangle(0,0,100,100,_rdtsc());
      return 0;
    }

    case vm_exit_invvpid:
    {
      sendstring("INVVPID\n\r");
      return handleIntelVMXInstruction(currentcpuinfo, vmregisters);

#ifdef DEBUG
      ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
      while (1) outportb(0x80,0xdb);
#endif
     // return 1;
    }

    case vm_exit_invpcid:
    {
      ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
      while(1);
      return 1;
    }

    case 54:
    {
      sendstring("WBINVD\n\r");
      return 1;
    }

    case 55:
    {
      sendstring("XSETBV\n\r");
      return handleXSETBV(currentcpuinfo, vmregisters);
    }

    default:
    {
      sendstring("The OMGWTF event happened. Please bury your head in the sand to avoid the nuclear blast\n\r");
      return 1;
    }

  }

  return 1;
}


int handleVMEvent(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, FXSAVE64 *fxsave)
{

 // if (currentcpuinfo->cpunr)
 //   outportb(0x80,exit_reason);
  int result;
  VMExit_idt_vector_information idtvectorinfo;
  idtvectorinfo.idtvector_info=vmread(vm_idtvector_information);

  if (currentcpuinfo->vmxdata.runningvmx)
  {
    //check if I should handle it, if not
    nosendchar[getAPICID()]=0;
    sendstring("nested vm is not 100% working\n");
    return handleByGuest(currentcpuinfo, vmregisters);
  }



  result=handleVMEvent_internal(currentcpuinfo, vmregisters, fxsave);


  if (idtvectorinfo.valid)
  {
    VMExit_interruption_information intinfo;
    intinfo.interruption_information=vmread(vm_entry_interruptioninfo);

    if ((intinfo.valid==0) || (intinfo.interruptvector!=idtvectorinfo.interruptvector) )
    {
      nosendchar[getAPICID()]=0;
      sendstringf("Here's one reason why you're crashing.  A pending interrupt never got respawned.  (Was supposed to spawn int %d) \n",intinfo.interruptvector);
      while (1);

    }
  }




  return result;
}
