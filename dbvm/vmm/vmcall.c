#include "main.h"
#include "vmpaging.h"
#include "vmmhelper.h"
#include "vmeventhandler.h"
#include "osspecific.h"
#include "common.h"
#include "mm.h"
#include "vmcall.h"
#include "msrnames.h"
#include "ultimap.h"


#pragma GCC push_options
#pragma GCC optimize ("O0")
QWORD readMSRSafe(pcpuinfo currentcpuinfo, DWORD msr)
{
  volatile QWORD result=0;

  currentcpuinfo->LastInterrupt=0;
  currentcpuinfo->OnInterrupt.RIP=(volatile void *)&&InterruptFired; //set interrupt location
  currentcpuinfo->OnInterrupt.RSP=getRSP();
  result=readMSR(msr);



InterruptFired:
  currentcpuinfo->OnInterrupt.RIP=0;

  return result;
}

#pragma GCC pop_options


int raisePagefault(pcpuinfo currentcpuinfo, UINT64 address)
{
  /*this will raise a non-present pagefault to the guest for the specified address
   *and set the usermode bit accordingly (not sure if needed, but better do it)
   */
  PFerrorcode errorcode;
  errorcode.errorcode=0;

  //get DPL of SS (or CS?), if 3, set US to 1, if anything else, 0
  if (isAMD)
  {
    if (((PSegment_Attribs)(&currentcpuinfo->vmcb->cs_attrib))->DPL==3)
      errorcode.US=1;
    else
      errorcode.US=0;
  }
  else
  {
    Access_Rights ssaccessrights;
    ssaccessrights.AccessRights=vmread(vm_guest_cs_access_rights);

    if (ssaccessrights.DPL==3)
      errorcode.US=1;
    else
      errorcode.US=0;
  }

  sendstring("Raising pagefault exception\n\r");
  if (isAMD)
  {
    currentcpuinfo->vmcb->CR2=address;
    currentcpuinfo->vmcb->inject_Type=3; //excption fault/trap
    currentcpuinfo->vmcb->inject_Vector=14; //#PF
    currentcpuinfo->vmcb->inject_Valid=1;

    currentcpuinfo->vmcb->inject_ERRORCODE=errorcode.errorcode;
    currentcpuinfo->vmcb->inject_EV=1;

    currentcpuinfo->vmcb->VMCB_CLEAN_BITS&=~(1 << 9); //cr2 got changed
  }
  else
  {

    VMEntry_interruption_information newintinfo;


    sendstringf("errorcode.errorcode=%d\n\r",errorcode.errorcode);

    newintinfo.interruption_information=0;
    newintinfo.interruptvector=14;
    newintinfo.type=3; //hardware
    newintinfo.haserrorcode=1;
    newintinfo.valid=1;

    vmwrite(0x4016, newintinfo.interruption_information); //entry info field
    vmwrite(0x4018, errorcode.errorcode); //entry errorcode
    vmwrite(0x401a, vmread(0x440c)); //entry instruction length

    //set CR2 to address
    setCR2(address);
  }

  return 0;
}

int raiseInvalidOpcodeException(pcpuinfo currentcpuinfo)
{
  sendstring("Raising Invalid opcode exception\n\r");
  if (isAMD)
  {
    currentcpuinfo->vmcb->inject_Type=3; //excption fault/trap
    currentcpuinfo->vmcb->inject_Vector=6; //#UD
    currentcpuinfo->vmcb->inject_Valid=1;
    currentcpuinfo->vmcb->inject_EV=0;

  }
  else
  {
    VMEntry_interruption_information newintinfo;

    newintinfo.interruption_information=0;
    newintinfo.interruptvector=6;
    newintinfo.type=3; //hardware
    newintinfo.haserrorcode=0; //no errorcode
    newintinfo.valid=1;

    vmwrite(0x4016, (ULONG)newintinfo.interruption_information); //entry info field
    vmwrite(0x4018, 0); //entry errorcode
    vmwrite(0x401a, vmread(0x440c)); //entry instruction length (not sure about this)
  }


  return 0;
}

int raisePrivilege(pcpuinfo currentcpuinfo)
{
  //Will change iopl to 0 and changes rpl and dlp of CS and SS to 0
  //return 0 if success
  //return 1 if interrupts are not disabled
  Access_Rights accessright;
  UINT64 guestrflags=vmread(vm_guest_rflags);
  PRFLAGS pguestrflags=(PRFLAGS)&guestrflags;
  if (pguestrflags->IF==1) //don't call when interrupts are enabled
    return 1;

  pguestrflags->IOPL=0;
  vmwrite(0x6820,(UINT64)guestrflags);


  vmwrite(vm_guest_cs,vmread(vm_guest_cs) & 0xfffc); //set RPL to 0
  vmwrite(vm_guest_ss,vmread(vm_guest_ss) & 0xfffc);

  accessright.AccessRights = vmread(vm_guest_cs_access_rights);
  accessright.DPL = 0;
  vmwrite(vm_guest_cs_access_rights, accessright.AccessRights);

  accessright.AccessRights = vmread(vm_guest_ss_access_rights);
  accessright.DPL = 0;
  vmwrite(vm_guest_ss_access_rights, accessright.AccessRights);


/*
  accessright.AccessRights = vmread(vm_guest_ds_access_rights);
  accessright.DPL = 0;
  vmwrite(vm_guest_ds_access_rights, accessright.AccessRights);

  accessright.AccessRights = vmread(vm_guest_es_access_rights);
  accessright.DPL = 0;
  vmwrite(vm_guest_es_access_rights, accessright.AccessRights);

  accessright.AccessRights = vmread(vm_guest_fs_access_rights);
  accessright.DPL = 0;
  vmwrite(vm_guest_fs_access_rights, accessright.AccessRights);

  accessright.AccessRights = vmread(vm_guest_gs_access_rights);
  accessright.DPL = 0;
  vmwrite(vm_guest_gs_access_rights, accessright.AccessRights);
  */

  if (currentcpuinfo==NULL)
    return 1;

  //the user will have to return to normal usermode himself and then call Restore interrupts
  return 0;

}

int change_selectors(pcpuinfo currentcpuinfo, ULONG cs, ULONG ss, ULONG ds, ULONG es, ULONG fs, ULONG gs)
{
  PGDT_ENTRY gdt=NULL,ldt=NULL;
  UINT64 gdtbase=vmread(0x6816);
  ULONG ldtselector=vmread(0x80c);
  int notpaged=0;

  sendstringf("Inside change_selectors\n\r");

  gdt=(PGDT_ENTRY)(UINT64)MapPhysicalMemory(
        getPhysicalAddressVM(currentcpuinfo, gdtbase, &notpaged)
        ,currentcpuinfo->AvailableVirtualAddress
      );

  if (ldtselector)
  {
    UINT64 ldtbase;
    WORD ldtlimit;

    sendstring("ldt is valid, so getting the information\n\r");

    ldtbase=(gdt[(ldtselector >> 3)].Base24_31 << 24) + gdt[(ldtselector >> 3)].Base0_23;
    ldtlimit=(gdt[(ldtselector >> 3)].Limit16_19 << 16) + gdt[(ldtselector >> 3)].Limit0_15;
    ldt=(PGDT_ENTRY)(UINT64)MapPhysicalMemory(getPhysicalAddressVM(currentcpuinfo, ldtbase, &notpaged), currentcpuinfo->AvailableVirtualAddress+0x00200000);
  }




  //selectors
  vmwrite(0x800,es);
  vmwrite(0x802,cs);
  vmwrite(0x804,ss);
  vmwrite(0x806,ds);
  vmwrite(0x808,fs);
  vmwrite(0x80a,gs);

  //limits
  vmwrite(0x4800,getSegmentLimit(gdt,ldt,es));
  vmwrite(0x4802,getSegmentLimit(gdt,ldt,cs));
  vmwrite(0x4804,getSegmentLimit(gdt,ldt,ss));
  vmwrite(0x4806,getSegmentLimit(gdt,ldt,ds));
  vmwrite(0x4808,getSegmentLimit(gdt,ldt,fs));
  vmwrite(0x480a,getSegmentLimit(gdt,ldt,gs));

  //bases
  vmwrite(0x6806,getSegmentBase(gdt,ldt,es));
  vmwrite(0x6808,getSegmentBase(gdt,ldt,cs));
  vmwrite(0x680a,getSegmentBase(gdt,ldt,ss));
  vmwrite(0x680c,getSegmentBase(gdt,ldt,ds));
  vmwrite(0x680e,getSegmentBase(gdt,ldt,fs));
  vmwrite(0x6810,getSegmentBase(gdt,ldt,gs));

  //set accessed bits in segments
  setDescriptorAccessedFlag(gdt,ldt,es);
  setDescriptorAccessedFlag(gdt,ldt,cs);
  setDescriptorAccessedFlag(gdt,ldt,ss);
  setDescriptorAccessedFlag(gdt,ldt,ds);
  setDescriptorAccessedFlag(gdt,ldt,fs);
  setDescriptorAccessedFlag(gdt,ldt,gs);

  //access rights
  vmwrite(0x4814,getSegmentAccessRights(gdt,ldt,es));
  vmwrite(0x4816,getSegmentAccessRights(gdt,ldt,cs));
  vmwrite(0x4818,getSegmentAccessRights(gdt,ldt,ss));
  vmwrite(0x481a,getSegmentAccessRights(gdt,ldt,ds));
  vmwrite(0x481c,getSegmentAccessRights(gdt,ldt,fs));
  vmwrite(0x481e,getSegmentAccessRights(gdt,ldt,gs));


  return 0;
}


void returnFromCR3Callback(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, unsigned long long newcr3)
{
   //restore state back to what it was
  //restore registers
  vmregisters->rax=currentcpuinfo->cr3_callback.rax;
  vmregisters->rbx=currentcpuinfo->cr3_callback.rbx;
  vmregisters->rcx=currentcpuinfo->cr3_callback.rcx;
  vmregisters->rdx=currentcpuinfo->cr3_callback.rdx;
  vmregisters->rsi=currentcpuinfo->cr3_callback.rsi;
  vmregisters->rdi=currentcpuinfo->cr3_callback.rdi;
  vmregisters->rbp=currentcpuinfo->cr3_callback.rbp;
  vmregisters->r8=currentcpuinfo->cr3_callback.r8;
  vmregisters->r9=currentcpuinfo->cr3_callback.r9;
  vmregisters->r10=currentcpuinfo->cr3_callback.r10;
  vmregisters->r11=currentcpuinfo->cr3_callback.r11;
  vmregisters->r12=currentcpuinfo->cr3_callback.r12;
  vmregisters->r13=currentcpuinfo->cr3_callback.r13;
  vmregisters->r14=currentcpuinfo->cr3_callback.r14;
  vmregisters->r15=currentcpuinfo->cr3_callback.r15;

  //restore selectors

  vmwrite(0x800,currentcpuinfo->cr3_callback.es_selector);
  vmwrite(0x802,currentcpuinfo->cr3_callback.cs_selector);
  vmwrite(0x804,currentcpuinfo->cr3_callback.ss_selector);
  vmwrite(0x806,currentcpuinfo->cr3_callback.ds_selector);
  vmwrite(0x808,currentcpuinfo->cr3_callback.fs_selector);
  vmwrite(0x80a,currentcpuinfo->cr3_callback.gs_selector);

  //limits
  vmwrite(0x4800,currentcpuinfo->cr3_callback.es_limit);
  vmwrite(0x4802,currentcpuinfo->cr3_callback.cs_limit);
  vmwrite(0x4804,currentcpuinfo->cr3_callback.ss_limit);
  vmwrite(0x4806,currentcpuinfo->cr3_callback.ds_limit);
  vmwrite(0x4808,currentcpuinfo->cr3_callback.fs_limit);
  vmwrite(0x480a,currentcpuinfo->cr3_callback.gs_limit);

  //base
  vmwrite(0x6806,currentcpuinfo->cr3_callback.es_base);
  vmwrite(0x6808,currentcpuinfo->cr3_callback.cs_base);
  vmwrite(0x680a,currentcpuinfo->cr3_callback.ss_base);
  vmwrite(0x680c,currentcpuinfo->cr3_callback.ds_base);
  vmwrite(0x680e,currentcpuinfo->cr3_callback.fs_base);
  vmwrite(0x6810,currentcpuinfo->cr3_callback.gs_base);


  //access rights
  vmwrite(0x4814,currentcpuinfo->cr3_callback.es_accessrights);
  vmwrite(0x4816,currentcpuinfo->cr3_callback.cs_accessrights);
  vmwrite(0x4818,currentcpuinfo->cr3_callback.ss_accessrights);
  vmwrite(0x481a,currentcpuinfo->cr3_callback.ds_accessrights);
  vmwrite(0x481c,currentcpuinfo->cr3_callback.fs_accessrights);
  vmwrite(0x481e,currentcpuinfo->cr3_callback.gs_accessrights);

  //rsp
  vmwrite(0x681c,currentcpuinfo->cr3_callback.rsp);

  //rip
  vmwrite(0x681e,currentcpuinfo->cr3_callback.rip);

  //rflags
  vmwrite(0x6820,currentcpuinfo->cr3_callback.rflags);


  //interrupt state
  vmwrite(0x4824,currentcpuinfo->cr3_callback.interruptability_state);

  //new cr3
  //set the real CR3 to what is stored in the parameter, guestcr3 has already been set
  vmwrite(0x6802, newcr3); //change real cr3
  currentcpuinfo->cr3_callback.newcr3=newcr3;

  if (currentcpuinfo->cr3_callback.newcr3 != currentcpuinfo->guestCR3)
    currentcpuinfo->cr3_callback.changedcr3=1;
  else
    currentcpuinfo->cr3_callback.changedcr3=0;



//  sendstringf("returned state: cs:eip=%x:%x\n\r",currentcpuinfo->cr3_callback.cs_selector, currentcpuinfo->cr3_callback.rip);
//  sendstringf("returned state: ss:esp=%x:%x\n\r",currentcpuinfo->cr3_callback.ss_selector, currentcpuinfo->cr3_callback.rsp);
//  sendstringf("returned state is: cs:eip=%x:%x\n\r",vmread(0x802), vmread(0x681e));
//  sendstringf("returned state is: ss:esp=%x:%x\n\r",vmread(0x804), vmread(0x681c));
//
//  sendstringf("returned state rflags: %x\n\r",vmread(0x6820));
//  sendstringf("returned state interruptability: %x\n\r",vmread(0x4824));

//  sendstringf("New cr3=%x\n\r",newcr3);
//  sendstringf("currentcpuinfo->guestCR3=%x\n\r",currentcpuinfo->guestCR3);
  currentcpuinfo->cr3_callback.called_callback = 0;


//  sendstringf("after:\n\r");
//  sendvmstate(currentcpuinfo,vmregisters);
}



int _handleVMCall(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
/*
 * vmcall:
 * eax=pointer to information structure
 * edx=level1pass (if false, not even a pagefault is raised)
 *
 * vmcall_instruction:
 * ULONG structsize
 * ULONG level2pass;
 * ULONG command
 * ... Extra data depending on command, see doc, "vmcall commands"
 *
 */
{

  int i;
  int error;
  UINT64 pagefaultaddress;
  ULONG *vmcall_instruction;

#ifdef DEBUG
  //enableserial();
#endif

  nosendchar[getAPICID()]=0;


  sendstringf("Handling vm(m)call on cpunr:%d \n\r", currentcpuinfo->cpunr);

  if (isAMD)
    vmregisters->rax=currentcpuinfo->vmcb->RAX; //fill it in, it may get used here


  //check password, if false, raise unknown opcode exception
  if ((ULONG)vmregisters->rdx != Password1)
  {
    int x;
    sendstringf("Invalid Password1. Given=%8 should be %8\n\r",(ULONG)vmregisters->rdx, Password1);
    x = raiseInvalidOpcodeException(currentcpuinfo);
    sendstringf("return = %d\n\r",x);
    return x;
  }

  sendstringf("Password1 is valid\n\r");


  //check if there is already a tlb, if not, allocate it. (it's needed for temp mem)
  if (currentcpuinfo->virtualTLB == NULL)
    allocateVirtualTLB();

  sendstringf("currentcpuinfo->AvailableVirtualAddress=%6\n", currentcpuinfo->AvailableVirtualAddress);

  sendstringf("vmregisters->rax=%8\n\r", vmregisters->rax);

  //still here, so password1 is valid
  //map the memory of the information structure


  vmcall_instruction=(ULONG *)mapVMmemory(currentcpuinfo, vmregisters->rax, 12, currentcpuinfo->AvailableVirtualAddress, &error, &pagefaultaddress);

  if (error)
  {
    sendstringf("1: Error. error=%d pagefaultaddress=%8\n\r",error,pagefaultaddress);

    if (error==2) //caused by pagefault, raise pagefault
      return raisePagefault(currentcpuinfo, pagefaultaddress);

    return raiseInvalidOpcodeException(currentcpuinfo);
  }

  sendstringf("Mapped vmcall instruction structure (vmcall_instruction=%x)\n\r",(UINT64)vmcall_instruction);
  sendstringf("vmcall_instruction[0]=%x\n\r",vmcall_instruction[0]);
  sendstringf("vmcall_instruction[1]=%x\n\r",vmcall_instruction[1]);
  sendstringf("vmcall_instruction[2]=%x\n\r",vmcall_instruction[2]);



  if ((vmcall_instruction[0]<12) || (vmcall_instruction[1]!=Password2))
  {
    sendstringf("Invalid password2 or structuresize. Given=%8 should be %8\n\r",vmcall_instruction[1], Password2);
    return raiseInvalidOpcodeException(currentcpuinfo);
  }



  //still here, so password valid and data structure paged in memory
  if (vmcall_instruction[0]>12) //remap to take the extra parameters into account
  {
//    sendstringf("Remapping to support size: %8\n\r",vmcall_instruction[0]);
    vmcall_instruction=(ULONG *)mapVMmemory(currentcpuinfo, vmregisters->rax, vmcall_instruction[0], currentcpuinfo->AvailableVirtualAddress, &error, &pagefaultaddress);
  }

#ifdef DEBUG
  int totaldwords = vmcall_instruction[0] / 4;
  for (i=3; i<totaldwords; i++)
  {
    sendstringf("vmcall_instruction[%d]=%x\n\r",i, vmcall_instruction[i]);
  }
#endif


  if (error)
  {
    sendstringf("2: Error. error=%d pagefaultaddress=%8\n\r",error,pagefaultaddress);

    if (error==2) //caused by pagefault, raise pagefault
      return raisePagefault(currentcpuinfo, pagefaultaddress);

    return raiseInvalidOpcodeException(currentcpuinfo);
  }


  sendstringf("Handling vmcall command %d\n\r",vmcall_instruction[2]);

  switch (vmcall_instruction[2])
  {
    case VMCALL_GETVERSION: //get version
      sendstring("Version request\n\r");
      vmregisters->rax=0xce000000 + dbvmversion;
      break;

    case VMCALL_CHANGEPASSWORD: //change password
      sendstring("Password change\n\r");
      Password1 = vmcall_instruction[3];
      Password2 = vmcall_instruction[4];

      sendstringf("Password1=%8\n\r",Password1);
      sendstringf("Password2=%8\n\r",Password2);

      vmregisters->rax=0;
      break;

    case 2: //toggle memory cloak
      if(isAMD)
      {
        vmregisters->rax = 0xcedead;
      }
      else
      {
        if (memorycloak)
        {
          //disable it, safely, take care of multiple cpu's

          break; //todo: Add disable memorycloak
        }

        memorycloak = !memorycloak;
        vmregisters->rax = memorycloak; //note that memorycloak will be activated after a CR3 change (taskswitch)
      }
      break;

    case VMCALL_READ_PHYSICAL_MEMORY: //read physical memory
    {
      UINT64 PhysicalAddressToReadFrom=*(UINT64 *)&vmcall_instruction[3];
      unsigned int size=vmcall_instruction[5];
      UINT64 VirtualAddressToWriteTo=*(UINT64 *)&vmcall_instruction[6];
      unsigned int noPageFault = vmcall_instruction[8];
      unsigned char *Destination;
      unsigned char *Source;

      //map physical memory
      sendstringf("Reading physical address %6 and writing it to %6\n\r",PhysicalAddressToReadFrom, VirtualAddressToWriteTo);
      sendstringf("noPageFault=%d\n\r",noPageFault);
      Source=(unsigned char *)MapPhysicalMemory(PhysicalAddressToReadFrom, currentcpuinfo->AvailableVirtualAddress);

      //map vm memory
      Destination=(unsigned char *)mapVMmemory(currentcpuinfo, VirtualAddressToWriteTo, size, currentcpuinfo->AvailableVirtualAddress+0x00400000, &error, &pagefaultaddress);
      if (error)
      {
    	  sendstringf("An error occurred while mapping %6 and size %d\n\r",VirtualAddressToWriteTo, size);

        if (error==2)
        {
          if (noPageFault)
            size=pagefaultaddress-VirtualAddressToWriteTo;
          else
            return raisePagefault(currentcpuinfo, pagefaultaddress); //raise pagefault
        }
        else
          size=0;

      }

      //copy memory from physical to vm
      copymem(Destination, Source, size);
      vmregisters->rax = size;

      break;
    }

    case VMCALL_WRITE_PHYSICAL_MEMORY: //write physical memory
    {
      UINT64 PhysicalAddressToWriteTo=*(UINT64 *)&vmcall_instruction[3];
      unsigned int size=vmcall_instruction[5];
      UINT64 VirtualAddressToReadFrom=*(UINT64 *)&vmcall_instruction[6];
      unsigned int noPageFault = vmcall_instruction[8];
      unsigned char *Destination;
      unsigned char *Source;

      //map physical memory
      Destination=(unsigned char *)MapPhysicalMemory(PhysicalAddressToWriteTo, currentcpuinfo->AvailableVirtualAddress);

      //map vm memory
      Source=(unsigned char *)mapVMmemory(currentcpuinfo, VirtualAddressToReadFrom, size, currentcpuinfo->AvailableVirtualAddress+0x00400000, &error, &pagefaultaddress);
      if (error)
      {
        if (error==2)
        {
          if (noPageFault)
            size=pagefaultaddress-VirtualAddressToReadFrom;
          else
            return raisePagefault(currentcpuinfo, pagefaultaddress); //raise pagefault
        }
        else
          size=0;
      }

      //copy memory from physical to vm
      copymem(Destination, Source, size);
      vmregisters->rax = size;

      break;
    }

    case 5: //Set fake sysentermsr state
    {
      if (isAMD)
      {
        //intercept these msr's as well

        vmregisters->rax = 0xcedead;
        break;
      }

      if (currentcpuinfo->hidden_sysenter_modification==0) //disabled, or was already disabled
      {
        //set the actual values to the guest os's state for compatibility when next time it gets started
        currentcpuinfo->actual_sysenter_CS=currentcpuinfo->sysenter_CS;
        currentcpuinfo->actual_sysenter_ESP=currentcpuinfo->sysenter_ESP;
        currentcpuinfo->actual_sysenter_EIP=currentcpuinfo->sysenter_EIP;
      }
      vmregisters->rax = 0;



      break;
    }

    case 6: //get real sysenter msr (not the guest's state, but what the user wants it to be, to get the guest's one use rdmsr)
    {
      ULONG *sysenter_CS;
      ULONG *sysenter_ESP;
      ULONG *sysenter_EIP;

      if (isAMD)
      {
        vmregisters->rax = 0xcedead;
        break;
      }


      if (currentcpuinfo->hidden_sysenter_modification==0)
        return raiseInvalidOpcodeException(currentcpuinfo);

      sysenter_CS=(ULONG *)mapVMmemory(currentcpuinfo, vmcall_instruction[3], 4, currentcpuinfo->AvailableVirtualAddress+0x00400000, &error, &pagefaultaddress);
      if (error==2)
        return raisePagefault(currentcpuinfo, pagefaultaddress); //raise pagefault

      sysenter_EIP=(ULONG *)mapVMmemory(currentcpuinfo, vmcall_instruction[4], 4, currentcpuinfo->AvailableVirtualAddress+0x00600000, &error, &pagefaultaddress);
      if (error==2)
        return raisePagefault(currentcpuinfo, pagefaultaddress); //raise pagefault

      sysenter_ESP=(ULONG *)mapVMmemory(currentcpuinfo, vmcall_instruction[5], 4, currentcpuinfo->AvailableVirtualAddress+0x00800000, &error, &pagefaultaddress);
      if (error==2)
        return raisePagefault(currentcpuinfo, pagefaultaddress); //raise pagefault

      //still here, so all memory is paged in.
      *sysenter_CS=currentcpuinfo->actual_sysenter_CS;
      *sysenter_ESP=currentcpuinfo->actual_sysenter_ESP;
      *sysenter_EIP=currentcpuinfo->actual_sysenter_EIP;

      vmregisters->rax = 0;
      break;
    }

    case 7: //set real sysenter msr (not the guest's state, but what the user wants it to be)
    {
      if (isAMD)
      {
        vmregisters->rax = 0xcedead;
        break;
      }

      ULONG sysenter_CS=vmcall_instruction[3];
      ULONG sysenter_ESP=vmcall_instruction[4];
      ULONG sysenter_EIP=vmcall_instruction[5];

      if (currentcpuinfo->hidden_sysenter_modification==0)
        return raiseInvalidOpcodeException(currentcpuinfo);

      currentcpuinfo->actual_sysenter_CS=sysenter_CS;
      currentcpuinfo->actual_sysenter_ESP=sysenter_ESP;
      currentcpuinfo->actual_sysenter_EIP=sysenter_EIP;

      //and adjust the vmsate to these new values
      vmwrite(0x6826,currentcpuinfo->actual_sysenter_EIP);
      vmwrite(0x6824,currentcpuinfo->actual_sysenter_ESP);
      vmwrite(0x482a,currentcpuinfo->actual_sysenter_CS);
      vmregisters->rax = 0;
      break;
    }

    case 8: //choose os
    {
      sendstring("Not yet implemented\n\r");
      vmregisters->rax = 0xcedead;
      break;
    }

    case VMCALL_REDIRECTINT1: //redirect int1
    {
      sendstring("VMCALL_REDIRECTINT1\n\r");
      if (vmcall_instruction[3] == 0)
      {
        int1redirection=vmcall_instruction[4];
        int1redirection_idtbypass=0;
        sendstringf("IDT1 redirection to int %d\n\r",int1redirection);
      }
      else
      {
        //rip
        int1redirection_idtbypass_cs = vmcall_instruction[7];  //cs
        int1redirection_idtbypass_rip = *(UINT64 *)&vmcall_instruction[5]; //rip
        int1redirection_idtbypass=1;

        sendstringf("IDT1 bypass to %x:%6\n\r",int1redirection_idtbypass_cs, int1redirection_idtbypass_rip);
      }

      if (isAMD)
      {
        //start intercepting
        if (vmcall_instruction[3] == 2) //2 is disable redirect alltogether
          currentcpuinfo->vmcb->InterceptExceptions&=~(1<<1); //unset bit 1 (int1 exception)
        else
          currentcpuinfo->vmcb->InterceptExceptions|=(1<<1); //set bit 1 (int1 exception)

        currentcpuinfo->vmcb->VMCB_CLEAN_BITS&=~(1 << 0); //the intercepts got changed
      }

      vmregisters->rax = 0;
      break;
    }

    case VMCALL_CHANGESELECTORS: //Change selectors
    {
      if (isAMD)
      {
        vmregisters->rax = 0xcedead;
        break;
      }
      ULONG cs=vmcall_instruction[3];
      ULONG ss=vmcall_instruction[4];
      ULONG ds=vmcall_instruction[5];
      ULONG es=vmcall_instruction[6];
      ULONG fs=vmcall_instruction[7];
      ULONG gs=vmcall_instruction[8];

      sendstringf("VMCALL_CHANGESELECTORS: cs=%x, ss=%x, ds=%x, es=%x, fs=%x, gs=%x\n\r", cs,ss,ds,es,fs,gs);
      sendstring("\n\rBefore:\n\r");
      sendvmstate(currentcpuinfo, vmregisters);
      vmregisters->rax = change_selectors(currentcpuinfo, cs,ss,ds,es,fs,gs);
      sendstring("After:\n\r\n\r");
      sendvmstate(currentcpuinfo, vmregisters);

      break;

    }

    case VMCALL_BLOCK_INTERRUPTS:
    {
      if (isAMD)
      {
        vmregisters->rax = 0xcedead;
        break;
      }

      sendstringf("VMCALL_BLOCK_INTERRUPTS\n");
      currentcpuinfo->Previous_Interuptability_State=vmread(0x4824);
      vmwrite(0x4824, (1<<3)); //block by NMI, so even a nmi based taskswitch won't interrupt

      //and set IF to 0 in eflags
      currentcpuinfo->Previous_CLI=(vmread(0x6820) >> 9) & 1;
      vmwrite(0x6820,vmread(0x6820) & 0xFFFFFFFFFFFFFDFF);

      vmregisters->rax = 0;
      break;
    }

    case VMCALL_RESTORE_INTERRUPTS:
    {
      if (isAMD)
      {
        vmregisters->rax = 0xcedead;
        break;
      }

      sendstringf("VMCALL_RESTORE_INTERRUPTS\n");
      vmwrite(vm_guest_interruptability_state,currentcpuinfo->Previous_Interuptability_State);

      vmwrite(vm_guest_rflags,(vmread(vm_guest_rflags) & 0xFFFFFFFFFFFFFDFF) | (currentcpuinfo->Previous_CLI << 9)); //reset IF to what it was
      vmregisters->rax = 0;
      break;
    }



    case 15: //get pid
    {
      if (isAMD)
      {
        vmregisters->rax = 0xcedead;
        break;
      }

      vmregisters->rax = getpid(currentcpuinfo);
      break;

    }

    case VMCALL_INT1REDIRECTED:
    {
      vmregisters->rax = currentcpuinfo->int1happened;
      currentcpuinfo->int1happened=0;
      break;
    }

    case VMCALL_REGISTER_CR3_EDIT_CALLBACK:
    {
      if (isAMD)
      {
        vmregisters->rax = 0xcedead;
        break;
      }

      sendstring("VMCALL_REGISTER_CR3_EDIT_CALLBACK\n\r");

      currentcpuinfo->cr3_callback.calling_convention=vmcall_instruction[3];
      currentcpuinfo->cr3_callback.callback_rip=*(UINT64*)&vmcall_instruction[4];
      currentcpuinfo->cr3_callback.callback_cs=vmcall_instruction[6];
      currentcpuinfo->cr3_callback.callback_rsp=*(UINT64*)&vmcall_instruction[7];
      currentcpuinfo->cr3_callback.callback_ss=vmcall_instruction[9];


      sendstringf("currentcpuinfo->cr3_callback.callback_rip=%x\n\r",currentcpuinfo->cr3_callback.callback_rip);
      sendstringf("currentcpuinfo->cr3_callback.callback_cs=%x\n\r",currentcpuinfo->cr3_callback.callback_cs);
      sendstringf("currentcpuinfo->cr3_callback.callback_rsp=%x\n\r",currentcpuinfo->cr3_callback.callback_rsp);
      sendstringf("currentcpuinfo->cr3_callback.callback_ss=%x\n\r",currentcpuinfo->cr3_callback.callback_ss);

      currentcpuinfo->cr3_callback.cr3_change_callback=1; //it has been registered
      break;

    }

    case VMCALL_RETURN_FROM_CR3_EDIT_CALLBACK:
    {
      UINT64 newcr3=*(UINT64 *)&vmcall_instruction[3];
      if (isAMD)
      {
        vmregisters->rax = 0xcedead;
        break;
      }

      //sendstring("VMCALL_RETURN_FROM_CR3_EDIT_CALLBACK:\n\r");

      returnFromCR3Callback(currentcpuinfo, vmregisters, newcr3);
      return 0;
    }

    case VMCALL_GETCR0:
    {

      vmregisters->rax = isAMD?currentcpuinfo->vmcb->CR0:vmread(vm_guest_cr0);
      break;
    }

    case VMCALL_GETCR3:
    {
      vmregisters->rax = isAMD?currentcpuinfo->vmcb->CR3:vmread(vm_guest_cr3);
      break;
    }

    case VMCALL_GETCR4:
    {
      vmregisters->rax = isAMD?currentcpuinfo->vmcb->CR4:vmread(vm_guest_cr4);
      break;
    }

    case VMCALL_RAISEPRIVILEGE:
    {
      if (isAMD)
      {
        vmregisters->rax = 0xcedead;
        break;
      }
      vmregisters->rax = raisePrivilege(currentcpuinfo);

      break;
    }

    case VMCALL_REDIRECTINT14: //redirect int1
    {

      sendstring("VMCALL_REDIRECTINT14\n\r");
      if (vmcall_instruction[3] == 0)
      {
        int14redirection=vmcall_instruction[4];
        int14redirection_idtbypass=0;
        sendstringf("IDT14 redirection to int %d\n\r",int14redirection);
      }
      else
      {
        //rip
        int14redirection_idtbypass_cs = vmcall_instruction[7];  //cs
        int14redirection_idtbypass_rip = *(UINT64 *)&vmcall_instruction[5]; //rip
        int14redirection_idtbypass=1;

        sendstringf("IDT14 bypass to %x:%6\n\r",int14redirection_idtbypass_cs, int14redirection_idtbypass_rip);
      }

      if (isAMD)
      {
        //start intercepting
        if (vmcall_instruction[3] == 2) //2 is disable redirect alltogether
          currentcpuinfo->vmcb->InterceptExceptions&=~(1<<14); //unset bit 1 (int1 exception)
        else
          currentcpuinfo->vmcb->InterceptExceptions|=(1<<14); //set bit 1 (int1 exception)

        currentcpuinfo->vmcb->VMCB_CLEAN_BITS&=~(1 << 0); //the intercepts got changed
      }

      vmregisters->rax = 0;
      break;
    }

    case VMCALL_INT14REDIRECTED:
    {
      vmregisters->rax = currentcpuinfo->int14happened;
      currentcpuinfo->int14happened=0;
      break;
    }

    case VMCALL_REDIRECTINT3: //redirect int3
    {
      sendstring("VMCALL_REDIRECTINT3\n\r");
      if (vmcall_instruction[3] == 0)
      {
        int3redirection=vmcall_instruction[4];
        int3redirection_idtbypass=0;
        sendstringf("IDT3 redirection to int %d\n\r",int3redirection);
      }
      else
      {
        //rip
        int3redirection_idtbypass_cs = vmcall_instruction[7];  //cs
        int3redirection_idtbypass_rip = *(UINT64 *)&vmcall_instruction[5]; //rip
        int3redirection_idtbypass=1;

        sendstringf("IDT3 bypass to %x:%6\n\r",int3redirection_idtbypass_cs, int3redirection_idtbypass_rip);
      }

      if (isAMD)
      {
        //start intercepting
        if (vmcall_instruction[3] == 2) //2 is disable redirect alltogether
          currentcpuinfo->vmcb->InterceptExceptions&=~(1<<3); //unset bit 1 (int1 exception)
        else
          currentcpuinfo->vmcb->InterceptExceptions|=(1<<3); //set bit 1 (int1 exception)

        currentcpuinfo->vmcb->VMCB_CLEAN_BITS&=~(1 << 0); //the intercepts got changed
      }


      vmregisters->rax = 0;
      break;
    }

    case VMCALL_INT3REDIRECTED:
    {
      vmregisters->rax = currentcpuinfo->int3happened;
      currentcpuinfo->int3happened=0;
      vmregisters->rax = 0;
      break;
    }

    case VMCALL_READMSR:
    {
      DWORD msr=vmcall_instruction[3];

      *(UINT64 *)&vmcall_instruction[4]=readMSRSafe(currentcpuinfo, msr);
      vmregisters->rax = *(UINT64 *)&vmcall_instruction[4];
      break;
    }

    case VMCALL_WRITEMSR:
    {
      DWORD msr=vmcall_instruction[3];
      QWORD msrvalue=*(UINT64 *)&vmcall_instruction[4];

      writeMSR(msr, msrvalue);
      break;
    }

    case VMCALL_ULTIMAP:
    {
      if (isAMD)
      {
        vmregisters->rax = 0xcedead;
        break;
      }

      //Ultimap will place a read/write watch on the IA32_DEBUGCTL msr, the IA32_DS_AREA and writes to CR3
      //When a taskswitch is done to the target CR3 the msr value will be set to the given value



      //Parameters:
      //CR3 of process to map out
      //Address to store the DS area (pre-configured)
      QWORD CR3=*(QWORD *)&vmcall_instruction[3];
      QWORD DEBUGCTL=*(QWORD *)&vmcall_instruction[5];
      QWORD DS_AREA=*(QWORD *)&vmcall_instruction[7];

      ultimap_setup(currentcpuinfo, CR3, DEBUGCTL, DS_AREA);
      vmregisters->rax = 0;

      break;
    }

    case VMCALL_ULTIMAP_DISABLE:
    {
      if (isAMD)
      {
        vmregisters->rax = 0xcedead;
        break;
      }

      ultimap_disable(currentcpuinfo);
      vmregisters->rax = 0;
      break;
    }

    case VMCALL_ULTIMAP_PAUSE:
    {
      if (isAMD)
      {
        vmregisters->rax = 0xcedead;
        break;
      }

      ultimap_pause(currentcpuinfo);
      vmregisters->rax = 0;
      break;
    }

    case VMCALL_ULTIMAP_RESUME:
    {
      if (isAMD)
      {
        vmregisters->rax = 0xcedead;
        break;
      }

      ultimap_resume(currentcpuinfo);
      vmregisters->rax = 0;
      break;
    }

    case VMCALL_ULTIMAP_DEBUGINFO:
    {
      if (isAMD)
      {
        vmregisters->rax = 0xcedead;
        break;
      }


#ifdef ULTIMAPDEBUG
      PULTIMAPDEBUGINFO Output=&vmcall_instruction[3];

      ultimap_debugoutput(currentcpuinfo, Output);
#endif
      vmregisters->rax = 0;
      break;
    }

    case VMCALL_DISABLE_DATAPAGEFAULTS:
    {
      if (isAMD)
      {
        vmregisters->rax = 0xcedead;
        break;
      }

      currentcpuinfo->IgnorePageFaults.Active=1;
      currentcpuinfo->IgnorePageFaults.LastIgnoredPageFault=0;
      vmregisters->rax = 0;
      break;

    }

    case VMCALL_ENABLE_DATAPAGEFAULTS:
    {
      if (isAMD)
      {
        vmregisters->rax = 0xcedead;
        break;
      }

      currentcpuinfo->IgnorePageFaults.Active=0;
      vmregisters->rax = 0;
      break;
    }

    case VMCALL_GETLASTSKIPPEDPAGEFAULT:
    {
      if (isAMD)
      {
        vmregisters->rax = 0xcedead;
        break;
      }

      vmregisters->rax = currentcpuinfo->IgnorePageFaults.LastIgnoredPageFault;
      currentcpuinfo->IgnorePageFaults.LastIgnoredPageFault=0;
      break;
    }

    case VMCALL_SWITCH_TO_KERNELMODE:
    {
      if (isAMD)
      {
        vmregisters->rax = 0xcedead;
        break;
      }

      DWORD cs=*(DWORD *)&vmcall_instruction[3];
      QWORD rip=*(QWORD *)&vmcall_instruction[4];
      QWORD parameters=*(QWORD *)&vmcall_instruction[6];

      sendstringf("VMCALL_SWITCH_TO_KERNELMODE(%x:%6 (%6)\n", cs, rip, parameters);

      //change CS and EIP (also setup the stack properly)
      vmwrite(0x681e,vmread(0x681e)+vmread(0x440c));
      emulateExceptionInterrupt(currentcpuinfo, vmregisters, cs,rip,1, parameters, 1);
      vmregisters->rax=0;
      return 0; //no eip change
    }

    case VMCALL_PSODTEST:
    {
      if (isAMD)
      {
        vmregisters->rax = 0xcedead;
        break;
      }
    	//PSOD("VMCALL_ULTIMAP_PSODTEST");
    	break;
    }




    default:
      vmregisters->rax = 0xcedead;
      break;


  }

  if (isAMD)
  {
    currentcpuinfo->vmcb->RAX=vmregisters->rax;
    currentcpuinfo->vmcb->RIP+=3; //screw you if you used prefixes
  }
  else
  {
    //handled, so increase eip to the next instruction and continue
    vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));
  }

  return 0;
}

//serialize these calls in case one makes an internal change that affects global (e.g alloc)
criticalSection vmcallCS;
int handleVMCall(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{
  int result;
  csEnter(&vmcallCS);
  result=_handleVMCall(currentcpuinfo, vmregisters);

  csLeave(&vmcallCS);

  return result;
}

