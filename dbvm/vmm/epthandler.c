/*
 * epthandler.c
 *
 *  Created on: Feb 2, 2018
 *      Author: erich
 */

#include "epthandler.h"
#include "main.h"
#include "mm.h"
#include "vmxsetup.h"
#include "msrnames.h"
#include "common.h"
#include "vmpaging.h"
#include "vmcall.h"

QWORD EPTMapPhysicalMemory(pcpuinfo currentcpuinfo, QWORD physicalAddress, int forcesmallpage);

criticalSection CloakedPagesCS; //1
CloakedPageInfo *CloakedPages;
int CloakedPagesSize;
int CloakedPagesPos;


criticalSection ChangeRegBPListCS; //2
ChangeRegBPEntry *ChangeRegBPList;
int ChangeRegBPListSize;
int ChangeRegBPListPos;

int ept_handleCloakEvent(pcpuinfo currentcpuinfo, QWORD Address)
/*
 * Checks if the physical address is cloaked, if so handle it and return 1, else return 0
 */
{
  int i,result=0;
  QWORD BaseAddress=Address & MAXPHYADDRMASKPB;
  csEnter(&CloakedPagesCS);
  for (i=0; i<CloakedPagesPos; i++)
  {
    if (currentcpuinfo->eptCloakListLength<=i)
      break;

    csEnter(&currentcpuinfo->EPTPML4CS);

    if (CloakedPages[i].PhysicalAddressExecutable==BaseAddress)
    {
      //it's a cloaked page
      EPT_VIOLATION_INFO evi;
      evi.ExitQualification=vmread(vm_exit_qualification);

      if (evi.X) //looks like this cpu does not support execute only
      {
        *(QWORD *)(currentcpuinfo->eptCloakList[i])=CloakedPages[i].PhysicalAddressExecutable;
        currentcpuinfo->eptCloakList[i]->WA=1;
        currentcpuinfo->eptCloakList[i]->RA=1;
        currentcpuinfo->eptCloakList[i]->XA=1;

        vmx_enableSingleStepMode();
        vmx_addSingleSteppingReason(currentcpuinfo, 2,i);
        currentcpuinfo->eptCloak_LastOperationWasWrite=evi.W;

        result=1;

        break;
      }
      else
      {
        *(QWORD *)(currentcpuinfo->eptCloakList[i])=CloakedPages[i].PhysicalAddressData; //todo: Let the user specify a bitmask so writes go through to the executable page as well
        currentcpuinfo->eptCloakList[i]->WA=1;
        currentcpuinfo->eptCloakList[i]->RA=1;
        currentcpuinfo->eptCloakList[i]->XA=1;

        vmx_enableSingleStepMode();
        vmx_addSingleSteppingReason(currentcpuinfo, 2,i);
        currentcpuinfo->eptCloak_LastOperationWasWrite=evi.W;


        result=1;

        break;

      }

      csLeave(&currentcpuinfo->EPTPML4CS);


    }

  }
  csLeave(&CloakedPagesCS);


  return result;
}

int ept_handleCloakEventAfterStep(pcpuinfo currentcpuinfo,  int ID)
{
  sendstringf("ept_handleCloakEventAfterStep\n");
  //back to execute only
  csEnter(&CloakedPagesCS);

  if (currentcpuinfo->eptCloak_LastOperationWasWrite)
  {
    //apply the write as well (TODO: let the user specify what memory should be skipped when writing)
    if ((*(QWORD *)(currentcpuinfo->eptCloakList[ID]) & MAXPHYADDRMASKPB) == CloakedPages[ID].PhysicalAddressExecutable)
    {
      //this was a write and execute at the same time
    }
    else
    {

    }
  }


  csEnter(&currentcpuinfo->EPTPML4CS);
  *(QWORD *)(currentcpuinfo->eptCloakList[ID])=CloakedPages[ID].PhysicalAddressExecutable; //back to the executable state
  currentcpuinfo->eptCloakList[ID]->WA=0;
  currentcpuinfo->eptCloakList[ID]->RA=0;
  if (has_EPT_ExecuteOnlySupport)
    currentcpuinfo->eptCloakList[ID]->XA=1;
  else
    currentcpuinfo->eptCloakList[ID]->XA=0;
  csLeave(&currentcpuinfo->EPTPML4CS);


  csLeave(&CloakedPagesCS);

  vmx_disableSingleStepMode();

  return 0;
}


int ept_cloak_activate(QWORD physicalAddress)
{
  int i;
  int ID=-1;

  sendstringf("ept_cloak_activate(%6)\n", physicalAddress);

  physicalAddress=physicalAddress & MAXPHYADDRMASKPB;
  csEnter(&CloakedPagesCS);

  //check if this physical address is already cloaked and find a free position while doing so
  for (i=0; i<CloakedPagesPos; i++)
  {
    if (CloakedPages[i].PhysicalAddressExecutable==physicalAddress)
    {
      csLeave(&CloakedPagesCS);
      return 1; //already cloaked
    }

    if ((ID==-1) && (CloakedPages[i].PhysicalAddressExecutable==0) && (CloakedPages[i].PhysicalAddressData==0))
      ID=i;
  }

  if (ID==-1) //new one
  {
    if (CloakedPagesPos==CloakedPagesSize) //need to reallocate the list
    {
      CloakedPagesSize=(CloakedPagesSize+2)*2;
      CloakedPages=realloc(CloakedPages, sizeof(CloakedPageInfo)*CloakedPagesSize);

      if (CloakedPages==NULL)
      {
        nosendchar[getAPICID()]=0;
        sendstringf("CloakedPages realloc failed\n");
        csLeave(&CloakedPagesCS);
        return 0xcedead00;
      }
    }

    ID=CloakedPagesPos;
    CloakedPagesPos++;
  }

  CloakedPages[ID].Executable=mapPhysicalMemoryGlobal(physicalAddress, 4096);
  CloakedPages[ID].Data=malloc(4096);


  if (CloakedPages[ID].Data==NULL)
  {
    if (CloakedPages[ID].Executable)
      unmapPhysicalMemoryGlobal(CloakedPages[ID].Executable, 4096);

    sendstringf("CloakedPages alloc data copy failed\n");
    csLeave(&CloakedPagesCS);
    return 0xcedead01;
  }
  copymem(CloakedPages[ID].Data, CloakedPages[ID].Executable, 4096);
  CloakedPages[ID].PhysicalAddressExecutable=physicalAddress;
  CloakedPages[ID].PhysicalAddressData=VirtualToPhysical(CloakedPages[ID].Data);

  //map in the physical address descriptor for all CPU's as execute only
  pcpuinfo currentcpuinfo=firstcpuinfo;
  while (currentcpuinfo)
  {
    csEnter(&currentcpuinfo->EPTPML4CS);

    //make sure that the eptCloakList is at least as big as CloakedPagesSize
    if (currentcpuinfo->eptCloakListLength<CloakedPagesSize)
      currentcpuinfo->eptCloakList=realloc(currentcpuinfo->eptCloakList, CloakedPagesSize);

    QWORD PA=EPTMapPhysicalMemory(currentcpuinfo, physicalAddress, 1);
    currentcpuinfo->eptCloakList[ID]=mapPhysicalMemory(PA, sizeof(EPT_PTE));

    //make it nonreadable
    currentcpuinfo->eptCloakList[ID]->RA=0;
    currentcpuinfo->eptCloakList[ID]->WA=0;

    if (has_EPT_ExecuteOnlySupport)
      currentcpuinfo->eptCloakList[ID]->XA=1;
    else
      currentcpuinfo->eptCloakList[ID]->XA=0; //going to be slow

    csLeave(&currentcpuinfo->EPTPML4CS);

    currentcpuinfo=currentcpuinfo->next;
  }




  csLeave(&CloakedPagesCS);
  return 0;
}

int ept_cloak_deactivate(QWORD physicalAddress)
{
  int i;
  int found=0;
  physicalAddress=physicalAddress & MAXPHYADDRMASKPB;
  csEnter(&CloakedPagesCS);

  for (i=0; i<CloakedPagesPos; i++)
  {
    if (CloakedPages[i].PhysicalAddressExecutable==physicalAddress)
    {
      //found it
      found=1;
      CloakedPages[i].PhysicalAddressExecutable=0;
      CloakedPages[i].PhysicalAddressData=0;
      free(CloakedPages[i].Data);
      CloakedPages[i].Data=NULL;
      unmapPhysicalMemoryGlobal(CloakedPages[i].Executable, 4096);
      CloakedPages[i].Executable=NULL;
    }
  }
  csLeave(&CloakedPagesCS);

  //if there where cloak event events pending, then next time they violate, the normal handler will make it RWX on the address it should
  return (found==1);
}

int ept_cloak_readOriginal(pcpuinfo currentcpuinfo,  VMRegisters *registers, QWORD physicalAddress, QWORD destination)
{
  int error;
  int i,ID=-1;
  physicalAddress=physicalAddress & MAXPHYADDRMASKPB;

  QWORD pagefault;

  void *dest=mapVMmemory(currentcpuinfo, destination, 4096,&error, &pagefault);

  if (error==2)
    return raisePagefault(currentcpuinfo, pagefault);

  csEnter(&CloakedPagesCS);
  for (i=0; i<CloakedPagesPos; i++)
    if (CloakedPages[i].PhysicalAddressExecutable==physicalAddress)
    {
      ID=i;
      break;
    }

  if (ID!=-1)
  {
    void *src=mapPhysicalMemory(CloakedPages[i].PhysicalAddressExecutable, 4096);
    copymem(dest,src,4096);
    registers->rax=0;

    unmapPhysicalMemory(src,4096);
  }
  else
    registers->rax=1;

  csLeave(&CloakedPagesCS);

  unmapVMmemory(dest,4096);


  vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));
  return 0;
}

int ept_cloak_writeOriginal(pcpuinfo currentcpuinfo,  VMRegisters *registers, QWORD physicalAddress, QWORD source)
{
  int error;
  int i,ID=-1;
  physicalAddress=physicalAddress & MAXPHYADDRMASKPB;

  QWORD pagefault;

  void *src=mapVMmemory(currentcpuinfo, source, 4096,&error, &pagefault);

  if (error==2)
    return raisePagefault(currentcpuinfo, pagefault);

  csEnter(&CloakedPagesCS);
  for (i=0; i<CloakedPagesPos; i++)
    if (CloakedPages[i].PhysicalAddressExecutable==physicalAddress)
    {
      ID=i;
      break;
    }

  if (ID!=-1)
  {
    void *dest=mapPhysicalMemory(CloakedPages[i].PhysicalAddressExecutable, 4096);
    copymem(dest,src,4096);
    registers->rax=0;

    unmapPhysicalMemory(dest,4096);
  }
  else
    registers->rax=1;

  csLeave(&CloakedPagesCS);

  unmapVMmemory(src,4096);


  vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));
  return 0;
}

int ept_cloak_changeregonbp(QWORD physicalAddress, PCHANGEREGONBPINFO changereginfo)
{
  int i;
  int result=1;
  QWORD physicalBase=physicalAddress & MAXPHYADDRMASKPB;
  ept_cloak_activate(physicalBase); //just making sure

  csEnter(&CloakedPagesCS);
  for (i=0; i<CloakedPagesPos; i++)
  {
    if (CloakedPages[i].PhysicalAddressExecutable==physicalBase)
    {
      //found it.  Create an int3 bp at that spot
      int ID=-1;
      int offset=physicalAddress & 0xfff;
      unsigned char *executable=CloakedPages[i].Executable;

      //
      csEnter(&ChangeRegBPListCS);
      int j;
      for (j=0; j<ChangeRegBPListPos; j++)
      {
        if (ChangeRegBPList[j].Active==0)
        {
          ID=j;
          break;
        }
      }
      if (ID==-1)
      {
        ID=ChangeRegBPListPos;
        ChangeRegBPListPos++;
        if (ChangeRegBPListPos>=ChangeRegBPListSize) //realloc the list
        {
          ChangeRegBPListSize=(ChangeRegBPListSize+2)*2;
          ChangeRegBPList=realloc(ChangeRegBPList, sizeof(ChangeRegBPEntry)*ChangeRegBPListSize);
        }
      }


      ChangeRegBPList[ID].PhysicalAddress=physicalAddress;
      ChangeRegBPList[ID].originalbyte=executable[offset];
      ChangeRegBPList[ID].changereginfo=*changereginfo;
      ChangeRegBPList[ID].CloakedRangeIndex=i;
      ChangeRegBPList[ID].Active=1;

      executable[offset]=0xcc; //int3 bp's will happen now (even on other CPU's)

      csLeave(&ChangeRegBPListCS);
      result=0;
    }
  }

  csLeave(&CloakedPagesCS);

  return result;
}

int ept_cloak_removechangeregonbp(QWORD physicalAddress)
{
  int i;
  int result=1;
  csEnter(&CloakedPagesCS);
  csEnter(&ChangeRegBPListCS);
  for (i=0; i<ChangeRegBPListPos; i++)
  {
    if ((ChangeRegBPList[i].Active) && (ChangeRegBPList[i].PhysicalAddress==physicalAddress))
    {
      unsigned char *executable=(unsigned char *)CloakedPages[ChangeRegBPList[i].CloakedRangeIndex].Executable;
      executable[physicalAddress & 0xfff]=ChangeRegBPList[i].originalbyte;
      ChangeRegBPList[i].Active=0;
      result=0;
    }
  }

  csLeave(&ChangeRegBPListCS);
  csLeave(&CloakedPagesCS);

  return result;
}

int ept_handleSoftwareBreakpoint(pcpuinfo currentcpuinfo)
{
  //check if it is a cloaked instruction
  int i;
  int result=1;


  //convert RIP into a physical address

  int notpaged;
  QWORD PA=getPhysicalAddressVM(currentcpuinfo, vmread(vm_guest_rip), &notpaged);

  if (notpaged==0) //should be since it's a software interrupt...
  {
    csEnter(&CloakedPagesCS);
    csEnter(&ChangeRegBPListCS);
    for (i=0; i<ChangeRegBPListPos; i++)
    {
      if (ChangeRegBPList[i].PhysicalAddress==PA)
      {
        if (ChangeRegBPList[i].Active)
        {
          //it's a match
          //Todo: Only change if the processID matches  (todo: Add a getProcessID option provided by the OS based caller)
          //For now, make sure that the physical page is not shared, or that the register change is compatible with different processes (e.g kernelmode only, or a Flag change)

          if ((ChangeRegBPList[i].changereginfo.Flags.changeRIP==0) || (ChangeRegBPList[i].changereginfo.newRIP==vmread(vm_guest_rip)))
          {
            //RIP does not change

            //restore the original byte
            int offset=ChangeRegBPList[i].PhysicalAddress & 0xfff;

            unsigned char *executable=(unsigned char *)CloakedPages[ChangeRegBPList[i].CloakedRangeIndex].Executable;
            executable[offset]=ChangeRegBPList[i].originalbyte;

            //setup single step mode
            vmx_enableSingleStepMode();
            vmx_addSingleSteppingReason(currentcpuinfo, 3,i); //change reg on bp, restore int3 bp

            vmwrite(vm_guest_rip, vmread(vm_guest_rip)-1);
            result=0;
            break;
          }
        }
        else
        {
          //probably a stale breakpoint event that was waiting for the spinlock (what are the changes that there was a 0xcc at the exact same spot a previous bp was set)
          //try again
          //todo: keep a try again counter
          vmwrite(vm_guest_rip, vmread(vm_guest_rip)-1);
          result=0;
        }
      }
    }

    csLeave(&ChangeRegBPListCS);
    csLeave(&CloakedPagesCS);
  }

  return result;
}


int ept_handleSoftwareBreakpointAfterStep(pcpuinfo currentcpuinfo UNUSED,  int ID)
{
  int result=1;
  csEnter(&CloakedPagesCS);
  csEnter(&ChangeRegBPListCS);
  if (ChangeRegBPList[ID].Active)
  {
    int i;
    QWORD PA=ChangeRegBPList[ID].PhysicalAddress;
    QWORD PABase=PA & MAXPHYADDRMASKPB;

    //find this PA in the cloaked pages list
    for (i=0; i<CloakedPagesPos; i++)
    {
      if (CloakedPages[i].PhysicalAddressExecutable==PABase)
      {
        int offset=PA-PABase;
        unsigned char *executable=(unsigned char*)CloakedPages[i].Executable;
        executable[offset]=0xcc; //set the breakpoint back
        result=0;
        break;
      }
    }


  }

  csLeave(&ChangeRegBPListCS);
  csLeave(&CloakedPagesCS);

  return result;
}


/*
 * WATCH
 */

int getFreeWatchID(pcpuinfo currentcpuinfo)
//scan through the watches for an unused spot, if not found, reallocate the list
{
  int i,j;
  for (i=0; i<currentcpuinfo->eptWatchlistLength; i++)
  {
    if (currentcpuinfo->eptWatchlist[i].Active==0)
      return i;
  }

  //still here, realloc

  i=currentcpuinfo->eptWatchlistLength;

  currentcpuinfo->eptWatchlistLength=(currentcpuinfo->eptWatchlistLength+2)*2;
  currentcpuinfo->eptWatchlist=realloc(currentcpuinfo->eptWatchlist, currentcpuinfo->eptWatchlistLength*sizeof(EPTWatchEntry));


  for (j=i; j<currentcpuinfo->eptWatchlistLength; j++)
    currentcpuinfo->eptWatchlist[j].Active=0;

  return i;
}

void saveStack(pcpuinfo currentcpuinfo, unsigned char *stack) //stack is 4096 bytes
{
  int error;
  QWORD pagefaultaddress;
  int size=4096;

  zeromemory(stack, 4096);
  //copy it but don't care about pagefaults (if there is a pagefault I 'could' trigger a pf and then wait and try again, but fuck it, it's not 'that' important
  unsigned char *gueststack=(unsigned char *)mapVMmemoryEx(currentcpuinfo, vmread(vm_guest_rsp), 4096, &error, &pagefaultaddress, 1);

  if (error)
  {
    if (error==2)
      size=pagefaultaddress-vmread(vm_guest_rsp);
    else
      return;
  }

  copymem(stack, gueststack, size);
  unmapVMmemory(gueststack, size);
}

void fillPageEventBasic(PageEventBasic *peb, VMRegisters *registers)
{
  peb->VirtualAddress=vmread(vm_guest_linear_address);
  peb->PhysicalAddress=vmread(vm_guest_physical_address);
  peb->CR3=vmread(vm_guest_cr3);
  peb->FSBASE=vmread(vm_guest_fs_base);
  peb->GSBASE=vmread(vm_guest_gs_base);
  peb->RAX=registers->rax;
  peb->RBX=registers->rbx;
  peb->RCX=registers->rcx;
  peb->RDX=registers->rdx;
  peb->RSI=registers->rsi;
  peb->RDI=registers->rdi;
  peb->R8=registers->r8;
  peb->R9=registers->r9;
  peb->R10=registers->r10;
  peb->R11=registers->r11;
  peb->R12=registers->r12;
  peb->R13=registers->r13;
  peb->R14=registers->r14;
  peb->R15=registers->r15;
  peb->RBP=registers->rbp;
  peb->RSP=vmread(vm_guest_rsp);
  peb->RIP=vmread(vm_guest_rip);
  peb->CS=vmread(vm_guest_cs);
  peb->DS=vmread(vm_guest_ds);
  peb->ES=vmread(vm_guest_es);
  peb->SS=vmread(vm_guest_ss);
  peb->FS=vmread(vm_guest_fs);
  peb->GS=vmread(vm_guest_gs);
  peb->Count=0;
}


inline int ept_doesAddressMatchWatchListEntryPerfectly(pcpuinfo currentcpuinfo, QWORD address, int ID)
{
  return ((currentcpuinfo->eptWatchlist[ID].Active) &&
          (
             (address>=currentcpuinfo->eptWatchlist[ID].PhysicalAddress) &&
             (address<currentcpuinfo->eptWatchlist[ID].PhysicalAddress+currentcpuinfo->eptWatchlist[ID].Size)
           )
          );
}

inline int ept_doesAddressMatchWatchListEntry(pcpuinfo currentcpuinfo, QWORD address, int ID)
/*
 * pre: address is already page aligned
 */
{
  return ((currentcpuinfo->eptWatchlist[ID].Active) && ((currentcpuinfo->eptWatchlist[ID].PhysicalAddress & 0xfffffffffffff000ULL) == address));
}

int ept_inWatchRegionPage(pcpuinfo currentcpuinfo, QWORD address)
/*
 * returns -1 if not in a page being watched
 * Note that there can be multiple active on the same page
 */
{
  int i;
  address=address & 0xfffffffffffff000ULL;
  for (i=0; i<currentcpuinfo->eptWatchlistLength; i++)
    if (ept_doesAddressMatchWatchListEntry(currentcpuinfo, address, i))
      return i;

  return -1;
}

int ept_handleWatchEvent(pcpuinfo currentcpuinfo, VMRegisters *registers, PFXSAVE64 fxsave, int ID)
{
  int logentrysize;
  int i;

  QWORD RIP=vmread(vm_guest_rip);
  QWORD RSP=vmread(vm_guest_rsp);
  QWORD PhysicalAddress=vmread(vm_guest_physical_address);
  QWORD PhysicalAddressBase=PhysicalAddress & 0xfffffffffffff000ULL;
  EPT_VIOLATION_INFO evi;
  evi.ExitQualification=vmread(vm_exit_qualification);

  //nosendchar[getAPICID()]=0;
  sendstringf("Handling something that resembles watch ID %d\n", ID);


  //figure out which access it is really (in case of multiple on the same page)

  for (i=ID; i<currentcpuinfo->eptWatchlistLength; i++)
  {
    if (ept_doesAddressMatchWatchListEntry(currentcpuinfo, PhysicalAddressBase, i))
    {
      if (currentcpuinfo->eptWatchlist[ID].Type==0)
      {
        //must be a write operation error
        if ((evi.W) && (evi.WasWritable==0)) //write operation and writable was 0
        {
          ID=i;

          if (ept_doesAddressMatchWatchListEntryPerfectly(currentcpuinfo, PhysicalAddress, i))
            break;
        }
      }
      else
      {
        //must be a read or write operation
        if (((evi.W) && (evi.WasWritable==0)) || ((evi.R) && (evi.WasReadable==0)))  //write operation and writable was 0 or read and readable was 0
        {
          ID=i;
          if (ept_doesAddressMatchWatchListEntryPerfectly(currentcpuinfo, PhysicalAddress, i))
            break;
        }
      }
    }
  }

  sendstringf("Handling watch ID %d\n", ID);

  //ID is now set to the most logical watch(usually there is no conflicts, and even if there is, no biggie. But still)

  //run once
  if ((currentcpuinfo->eptWatchlist[ID].EPTEntry->XA) && (currentcpuinfo->eptWatchlist[ID].EPTEntry->RA) && (currentcpuinfo->eptWatchlist[ID].EPTEntry->WA))
  {
    sendstringf("This entry was already marked with full access (check caches)\n");
  }

  currentcpuinfo->eptWatchlist[ID].EPTEntry->XA=1;
  currentcpuinfo->eptWatchlist[ID].EPTEntry->RA=1;
  currentcpuinfo->eptWatchlist[ID].EPTEntry->WA=1;

  sendstringf("Page is accessible. Doing single step\n");

  vmx_enableSingleStepMode();
  vmx_addSingleSteppingReason(currentcpuinfo, 1, ID);


  //save this state?
  if ((evi.R==0) && (evi.X==1))
  {
    sendstringf("This was an execute operation and no read. No need to log\n", ID);
    return 0; //execute operation (this cpu doesn't support execute only)
  }

  if (currentcpuinfo->eptWatchlist[ID].CopyInProgress) //a copy operation is in progress
  {
    sendstringf("This watchlist is currently being copied, not logging this\n");
    return 0;
  }

  if (((currentcpuinfo->eptWatchlist[ID].Options & EPTO_LOG_ALL)==0) &&
     (
      (PhysicalAddress<currentcpuinfo->eptWatchlist[ID].PhysicalAddress) ||
      (PhysicalAddress>=currentcpuinfo->eptWatchlist[ID].PhysicalAddress+currentcpuinfo->eptWatchlist[ID].Size)
      ))
  {
    sendstringf("Not logging all and the physical address is not in the exact range\n");
    return 0; //no need to log it
  }


  //scan if this RIP is already in the list

  switch (currentcpuinfo->eptWatchlist[ID].Log->entryType)
  {
    case 0: logentrysize=sizeof(PageEventBasic); break;
    case 1: logentrysize=sizeof(PageEventExtended); break;
    case 2: logentrysize=sizeof(PageEventBasicWithStack); break;
    case 3: logentrysize=sizeof(PageEventExtendedWithStack); break;
  }

  sendstringf("Want to log this. Type=%d EntrySize=%d\n", currentcpuinfo->eptWatchlist[ID].Log->entryType, logentrysize);

  for (i=0; (DWORD)i<currentcpuinfo->eptWatchlist[ID].Log->numberOfEntries; i++)
  {
    PageEventBasic *peb=(PageEventBasic *)((QWORD)(&currentcpuinfo->eptWatchlist[ID].Log->pe.basic[0])+i*logentrysize);
    //every type starts with a PageEventBasic

    if (peb->RIP==RIP)
    {
      sendstringf("This RIP is already logged");
      //it's already in the list
      if ((currentcpuinfo->eptWatchlist[ID].Options & EPTO_MULTIPLERIP)==0)
      {
        sendstringf(" and EPTO_MULTIPLERIP is 0.  Not logging (just increase count)\n");
        peb->Count++;
        return 0; //no extra RIP's
      }
      else
        sendstringf(" but EPTO_MULTIPLERIP is 1, so checking register states\n");

      //still here, so multiple RIP's are ok. check if it matches the other registers
      if (
          (peb->RSP==RSP) &&
          (peb->RBP==registers->rbp) &&
          (peb->RAX==registers->rax) &&
          (peb->RBX==registers->rbx) &&
          (peb->RCX==registers->rcx) &&
          (peb->RDX==registers->rdx) &&
          (peb->RSI==registers->rsi) &&
          (peb->RDI==registers->rdi) &&
          (peb->R8==registers->r8) &&
          (peb->R9==registers->r9) &&
          (peb->R10==registers->r10) &&
          (peb->R11==registers->r11) &&
          (peb->R12==registers->r12) &&
          (peb->R13==registers->r13) &&
          (peb->R14==registers->r14) &&
          (peb->R15==registers->r15)
        )
      {
        sendstringf("  The registers match the state so skipping the log. (Just increase count)\n");
        peb->Count++;
        return 0; //already in the list
      }

    }

  }

  //still here, so not in the list
  sendstringf("Checks out ok. Not yet in the list\n");

  if (currentcpuinfo->eptWatchlist[ID].Log->numberOfEntries>=currentcpuinfo->eptWatchlist[ID].Log->maxNumberOfEntries)
  {
    sendstringf("List is full. Discarding event\n");
    return 0; //can't add more
  }

  //still here, so not in the list, and still room
  //add it

  i=currentcpuinfo->eptWatchlist[ID].Log->numberOfEntries;
  switch (currentcpuinfo->eptWatchlist[ID].Log->entryType)
  {
    case PE_BASIC:
    {
      fillPageEventBasic(&currentcpuinfo->eptWatchlist[ID].Log->pe.basic[i], registers);
      break;
    }

    case PE_EXTENDED:
    {
      fillPageEventBasic(&currentcpuinfo->eptWatchlist[ID].Log->pe.extended[i].basic, registers);
      currentcpuinfo->eptWatchlist[ID].Log->pe.extended[i].fpudata=*fxsave;
      break;
    }

    case PE_BASICSTACK:
    {
      fillPageEventBasic(&currentcpuinfo->eptWatchlist[ID].Log->pe.basics[i].basic, registers);
      saveStack(currentcpuinfo, currentcpuinfo->eptWatchlist[ID].Log->pe.basics[i].stack);
      break;
    }

    case PE_EXTENDEDSTACK:
    {
      fillPageEventBasic(&currentcpuinfo->eptWatchlist[ID].Log->pe.extendeds[i].basic, registers);
      currentcpuinfo->eptWatchlist[ID].Log->pe.extended[i].fpudata=*fxsave;
      saveStack(currentcpuinfo, currentcpuinfo->eptWatchlist[ID].Log->pe.extendeds[i].stack);
      break;
    }
  }

  currentcpuinfo->eptWatchlist[ID].Log->numberOfEntries++;

  sendstringf("Added it to the list. numberOfEntries for ID %d is now %d\n", ID, currentcpuinfo->eptWatchlist[ID].Log->numberOfEntries);

  return 0;

}

int ept_handleWatchEventAfterStep(pcpuinfo currentcpuinfo,  int ID)
{
  sendstringf("ept_handleWatchEventAfterStep %d\n", ID);
  vmx_disableSingleStepMode();

  if (currentcpuinfo->eptWatchlist[ID].Type==0)
  {
    sendstringf("Write type. So making it unwritable\n");
    currentcpuinfo->eptWatchlist[ID].EPTEntry->WA=0;
  }
  else
  {
    sendstringf("read type. So making it unreadable\n");
    currentcpuinfo->eptWatchlist[ID].EPTEntry->RA=0;
    currentcpuinfo->eptWatchlist[ID].EPTEntry->WA=0;
    if (has_EPT_ExecuteOnlySupport)
      currentcpuinfo->eptCloakList[ID]->XA=1;
    else
      currentcpuinfo->eptCloakList[ID]->XA=0;
  }


  return 0;
}


int ept_activateWatch(pcpuinfo currentcpuinfo, int ID)
{
  //(re)map this physical memory and the page descriptors
  sendstringf("ept_activateWatch(%d)\n", ID);
  QWORD PA_EPTE=EPTMapPhysicalMemory(currentcpuinfo, currentcpuinfo->eptWatchlist[ID].PhysicalAddress, 1);

  sendstringf("PA_EPTE=%6\n", PA_EPTE);
  currentcpuinfo->eptWatchlist[ID].EPTEntry=(PEPT_PTE)mapPhysicalMemory(PA_EPTE, sizeof(EPT_PTE));

  //test if needed:  SetPageToWriteThrough(currentcpuinfo->eptwatchlist[ID].EPTEntry);

  //check out 28.3.3.4  (might not be needed due to vmexit, but do check anyhow)

  if (currentcpuinfo->eptWatchlist[ID].Type==0)
  {
    currentcpuinfo->eptWatchlist[ID].EPTEntry->WA=0;
    sendstringf("Make the entry for %6 non writable\n",currentcpuinfo->eptWatchlist[ID].PhysicalAddress);
  }
  else
  if (currentcpuinfo->eptWatchlist[ID].Type==1)
  {
    currentcpuinfo->eptWatchlist[ID].EPTEntry->WA=0;
    currentcpuinfo->eptWatchlist[ID].EPTEntry->RA=0;
    if (has_EPT_ExecuteOnlySupport)
      currentcpuinfo->eptCloakList[ID]->XA=1;
    else
      currentcpuinfo->eptCloakList[ID]->XA=0;

    sendstringf("Make the entry for %6 non readable/writable\n",currentcpuinfo->eptWatchlist[ID].PhysicalAddress);
  }
  //EPTINV

  currentcpuinfo->eptWatchlist[ID].Active=1;
  return 0;
}

int ept_disableWatch(pcpuinfo currentcpuinfo, int ID)
{
  int i;
  int hasAnotherOne=0;
  QWORD PhysicalBase=currentcpuinfo->eptWatchlist[ID].PhysicalAddress & 0xfffffffffffff000ULL;

  for (i=0; i<currentcpuinfo->eptWatchlistLength; i++)
  {
    if (ept_doesAddressMatchWatchListEntry(currentcpuinfo, PhysicalBase, i))
    {
      //matches
      if (currentcpuinfo->eptWatchlist[i].Type==currentcpuinfo->eptWatchlist[ID].Type) //don't undo
        hasAnotherOne=1;
    }
  }

  if (hasAnotherOne==0)
  {
    //undo
    if (currentcpuinfo->eptWatchlist[ID].Type==0)
      currentcpuinfo->eptWatchlist[ID].EPTEntry->WA=1;
    else
    {
      currentcpuinfo->eptWatchlist[ID].EPTEntry->RA=1;
      currentcpuinfo->eptWatchlist[ID].EPTEntry->WA=1;
    }

  }

  unmapPhysicalMemory(currentcpuinfo->eptWatchlist[ID].EPTEntry,sizeof(EPT_PTE));
  currentcpuinfo->eptWatchlist[ID].EPTEntry=NULL;
  currentcpuinfo->eptWatchlist[ID].Active=0;
  return 0;
}


inline int MTC_RPS(int mt1, int mt2)
{
  //memory type cache rock paper scissors
  if (mt1==mt2)
    return mt1;

  if ((mt1==MTC_UC) || (mt2==MTC_UC))
      return MTC_UC;

  if (((mt1==MTC_WT) && (mt2==MTC_WB)) || ((mt2==MTC_WT) && (mt1==MTC_WB)) )
    return MTC_WT;

  if ((mt1==MTC_WP) || (mt2==MTC_WP))
    return MTC_WP;

  if ((mt1==MTC_WB) || (mt2!=MTC_WB))
    return mt2;
  else
    return min(mt1,mt2);
}

typedef struct _MEMRANGE
{
  QWORD startaddress;
  QWORD size;
  int memtype;
} MEMRANGE, *PMEMRANGE;

criticalSection memoryrangesCS;
MEMRANGE *memoryranges;
int memoryrangesLength;
int memoryrangesPos;

void getMTRRMapInfo(QWORD startaddress, QWORD size, int *fullmap, int *memtype)
/*
 * pre: startaddress is aligned on the boundary you wish to map
 *
 * post: fullmap is 1 if the size can be fully mapped without conflicts (border crossings)
 */
{
  //note: the list is sorted
  QWORD starta=startaddress;
  QWORD stopa=startaddress+size-1;
  int morethan1=0;
  int i;

  *memtype=MTRRDefType.TYPE;
  *fullmap=1;

  csEnter(&memoryrangesCS);
  for (i=0; i<memoryrangesPos; i++)
  {
    QWORD startb,stopb;
    startb=memoryranges[i].startaddress;
    stopb=memoryranges[i].startaddress+memoryranges[i].size-1;

    if ((starta <= stopb) && (startb <= stopa))
    {
      //overlap, check the details
      if ((starta>=startb) && (stopa<=stopb)) //falls completely within the region, so can be fully mapped.
      {
        if (morethan1==0)
          *memtype=memoryranges[i].memtype;//set the memory type
        else
          *memtype=MTC_RPS(memoryranges[i].memtype, *memtype); //set the memory type based on the two combined types

        morethan1++;
      }
      else
      {
        *fullmap=0; //mark as not fully mappable, go one level lower(this also happens on overlaps where a second part doesn't fit, shouldn't happen often)

        break;
      }


    }

    if (stopa<startb) //reached a startaddress higher than my stopaddress, which means every other item will be as well
      break;
  }
  csLeave(&memoryrangesCS);
}

void addToMemoryRanges(QWORD address, QWORD size, int type)
/*
 * pre: memoryrangesCS lock has been aquired
 */
{
  int i;
  int insertpos=-1;


  if (size==0) return;

  //add memory for a new entry
  if (memoryrangesPos==memoryrangesLength)
  {
    realloc2(memoryranges, memoryrangesLength*sizeof(MEMRANGE), 2*memoryrangesLength*sizeof(MEMRANGE));
    memoryrangesLength=memoryrangesLength*2;
  }

  for (i=0; i<memoryrangesPos; i++)
  {
    if (memoryranges[i].startaddress>address)
    {
      //insert here
      insertpos=i;
      break;
    }
  }

  if (insertpos==-1)
    insertpos=memoryrangesPos;

  for (i=memoryrangesPos; i>insertpos; i--)
    memoryranges[i]=memoryranges[i-1];

  memoryranges[insertpos].startaddress=address;
  memoryranges[insertpos].size=size;
  memoryranges[insertpos].memtype=type;
  memoryrangesPos++;
}

void initMemTypeRanges()
//builds an array of memory ranges and their cache
{
  int i;
  csEnter(&memoryrangesCS);

  memoryrangesPos=0;

  if (memoryranges==NULL)
  {
    memoryrangesLength=32;
    memoryranges=malloc2(sizeof(MEMRANGE)*memoryrangesLength);
  }

  QWORD startaddress=0;
  QWORD size=0;
  int memtype=MTRRDefType.TYPE;

  if ((MTRRCapabilities.FIX && MTRRDefType.FE))
  {

    QWORD FIX64K_00000=readMSR(IA32_MTRR_FIX64K_00000);
    QWORD FIX16K_80000=readMSR(IA32_MTRR_FIX16K_80000);
    QWORD FIX16K_A0000=readMSR(IA32_MTRR_FIX16K_A0000);
    QWORD FIX4K_C0000 =readMSR(IA32_MTRR_FIX4K_C0000);
    QWORD FIX4K_C8000 =readMSR(IA32_MTRR_FIX4K_C8000);
    QWORD FIX4K_D0000 =readMSR(IA32_MTRR_FIX4K_D0000);
    QWORD FIX4K_D8000 =readMSR(IA32_MTRR_FIX4K_D8000);
    QWORD FIX4K_E0000 =readMSR(IA32_MTRR_FIX4K_E0000);
    QWORD FIX4K_E8000 =readMSR(IA32_MTRR_FIX4K_E8000);
    QWORD FIX4K_F0000 =readMSR(IA32_MTRR_FIX4K_F0000);
    QWORD FIX4K_F8000 =readMSR(IA32_MTRR_FIX4K_F8000);
    //check the fixed range mtrs

    while (startaddress+size<0x100000)
    {
      int type;
      int sizeinc=0;

      switch (startaddress+size)
      {
        case 0 ... 0x7ffff:
        {
          QWORD types=FIX64K_00000;
          int index=(startaddress+size) >> 16;
          type=(types >> (index*8)) & 0xf;
          sizeinc=64*1024;
          break;
        }

        case 0x80000 ... 0x9ffff:
        {
          QWORD types=FIX16K_80000;
          int index=((startaddress+size)-0x80000) >> 14;
          type=(types >> (index*8)) & 0xf;
          sizeinc=16*1024;
          break;
        }

        case 0xa0000 ... 0xbffff:
        {
          QWORD types=FIX16K_A0000;
          int index=((startaddress+size)-0xa0000) >> 14;
          type=(types >> (index*8)) & 0xf;
          sizeinc=16*1024;
          break;
        }

        case 0xc0000 ... 0xc7fff:
        {
          QWORD types=FIX4K_C0000;
          int index=((startaddress+size)-0xc0000) >> 12;
          type=(types >> (index*8)) & 0xf;
          sizeinc=4*1024;
          break;
        }

        case 0xc8000 ... 0xcffff:
        {
          QWORD types=FIX4K_C8000;
          int index=((startaddress+size)-0xc8000) >> 12;
          type=(types >> (index*8)) & 0xf;
          sizeinc=4*1024;
          break;
        }

        case 0xd0000 ... 0xd7fff:
        {
          QWORD types=FIX4K_D0000;
          int index=((startaddress+size)-0xd0000) >> 12;
          type=(types >> (index*8)) & 0xf;
          sizeinc=4*1024;
          break;
        }

        case 0xd8000 ... 0xdffff:
        {
          QWORD types=FIX4K_D8000;
          int index=((startaddress+size)-0xd8000) >> 12;
          type=(types >> (index*8)) & 0xf;
          sizeinc=4*1024;
          break;
        }

        case 0xe0000 ... 0xe7fff:
        {
          QWORD types=FIX4K_E0000;
          int index=((startaddress+size)-0xe0000) >> 12;
          type=(types >> (index*8)) & 0xf;
          sizeinc=4*1024;
          break;
        }

        case 0xe8000 ... 0xeffff:
        {
          QWORD types=FIX4K_E8000;
          int index=((startaddress+size)-0xe8000) >> 12;
          type=(types >> (index*8)) & 0xf;
          sizeinc=4*1024;
          break;
        }

        case 0xf0000 ... 0xf7fff:
        {
          QWORD types=FIX4K_F0000;
          int index=((startaddress+size)-0xf0000) >> 12;
          type=(types >> (index*8)) & 0xf;
          sizeinc=4*1024;
          break;
        }

        case 0xf8000 ... 0xfffff:
        {
          QWORD types=FIX4K_F8000;
          int index=((startaddress+size)-0xf8000) >> 12;
          type=(types >> (index*8)) & 0xf;
          sizeinc=4*1024;
          break;
        }


      }

      if (type==memtype) //same type, continue
        size+=sizeinc;
      else //type changed
      {
        if (memtype!=MTRRDefType.TYPE) //old type wasn't the default, add it to the list
          addToMemoryRanges(startaddress, size, memtype);


        //start a new region
        startaddress=startaddress+size;
        size=0;
        memtype=type;
      }
    }

    if ((size) && (memtype!=MTRRDefType.TYPE)) //last region needs to be added as well
      addToMemoryRanges(startaddress, size, memtype);
  }

  //check the var fields
  for (i=0; i<MTRRCapabilities.VCNT; i++)
  {
    QWORD base=readMSR(IA32_MTRR_PHYSBASE0+i*2);
    QWORD mask=readMSR(IA32_MTRR_PHYSMASK0+i*2);
    int memtype=base & 0xff;

    if ((mask & (1<<11)) && (memtype!=MTRRDefType.TYPE)) //valid
    {
      // Address_Within_Range AND PhysMask = PhysBase AND PhysMask

      //strip of useless bits
      base=base & MAXPHYADDRMASKPB;
      mask=mask & MAXPHYADDRMASKPB;

      //find the highest 0 bit in the mask to find the region (this allows for the shitty “discontinuous” ranges)
      int j;

      for (j=MAXPHYADDR-1; j>12; j--)
      {
        if ((mask & ((QWORD)1<<j))==0)
        {
          QWORD size=((QWORD)1<<(j+1)); //the last bit with 1
          addToMemoryRanges(base, size, memtype);
          break;
        }
      }
    }
  }

  if (loadedOS==0)
  {
    addToMemoryRanges(VirtualToPhysical((void *)0x00400000), 0x00400000, MTC_WP);
  }

  csLeave(&memoryrangesCS);
}



int remapMTRRTypes(QWORD address UNUSED, QWORD size UNUSED, int type UNUSED)
{
  //called by the MSR write handler when MTRR registers get changed
  return 1;
}

int handleMSRWrite_MTRR(void)
//called when an MTRR msr is written. Figures out what regions have been modified
{
  return 1;
}

QWORD EPTMapPhysicalMemory(pcpuinfo currentcpuinfo, QWORD physicalAddress, int forcesmallpage)
/*
 * Maps the physical address into the EPT map.
 * Returns the physical address of the EPT entry describing this page
 */
{
  int pml4index;
  int pagedirptrindex;
  int pagedirindex;
  int pagetableindex;
  QWORD PA;
  VirtualAddressToIndexes(physicalAddress, &pml4index, &pagedirptrindex, &pagedirindex, &pagetableindex);


  PEPT_PML4E pml4=NULL;
  PEPT_PDPTE pagedirptr=NULL;
  PEPT_PDE pagedir=NULL;
  PEPT_PTE pagetable=NULL;

  csEnter(&currentcpuinfo->EPTPML4CS);

  pml4=mapPhysicalMemory(currentcpuinfo->EPTPML4, 4096);
  if (pml4[pml4index].RA==0)
  {
    sendstringf("allocating pagedirptr\n");
    //allocate a pagedirptr table
    void *temp=malloc2(4096);
    zeromemory(temp,4096);
    *(QWORD*)(&pml4[pml4index])=VirtualToPhysical(temp) & MAXPHYADDRMASKPB;
    pml4[pml4index].RA=1;
    pml4[pml4index].WA=1;
    pml4[pml4index].XA=1;
  }

  PA=(*(QWORD*)(&pml4[pml4index])) & MAXPHYADDRMASKPB;
  pagedirptr=mapPhysicalMemory(PA, 4096);

  PA+=8*pagedirptrindex;

  if (pml4)
  {
    unmapPhysicalMemory(pml4, 4096);
    pml4=NULL;
  }

  if (forcesmallpage && pagedirptr[pagedirptrindex].RA && (pagedirptr[pagedirptrindex].BIG)) //it's a big page, so the physical address points to the actual memory. Clear everything
    *(QWORD *)&pagedirptr[pagedirptrindex]=0;

  if (pagedirptr[pagedirptrindex].RA==0)
  {
    //check if there is a MTRR in this 1GB range that doesn't set the MTRRDefType.TYPE, if not, map as 1GB using deftype
    if (has_EPT_1GBsupport && (forcesmallpage==0))
    {
      QWORD GuestAddress1GBAlign=(physicalAddress & 0xFFFFFFFFC0000000ULL) & MAXPHYADDRMASKPB;
      int fullmap, memtype;
      getMTRRMapInfo(GuestAddress1GBAlign,0x40000000, &fullmap, &memtype);

      if (fullmap)
      {
        //map as a 1GB page
        sendstringf("mapping %6 as a 1GB page with memtype %d\n", GuestAddress1GBAlign, memtype);
        *(QWORD*)(&pagedirptr[pagedirptrindex])=GuestAddress1GBAlign;
        pagedirptr[pagedirptrindex].RA=1;
        pagedirptr[pagedirptrindex].WA=1;
        pagedirptr[pagedirptrindex].XA=1;
        pagedirptr[pagedirptrindex].BIG=1;
        pagedirptr[pagedirptrindex].MEMTYPE=memtype;
        unmapPhysicalMemory(pagedirptr, 4096);

        csLeave(&currentcpuinfo->EPTPML4CS);
        return PA;
      }
    }

    //still here, try a pagedir
    void *temp=malloc2(4096);
    zeromemory(temp,4096);
    *(QWORD*)(&pagedirptr[pagedirptrindex])=VirtualToPhysical(temp) & MAXPHYADDRMASKPB;
    pagedirptr[pagedirptrindex].RA=1;
    pagedirptr[pagedirptrindex].WA=1;
    pagedirptr[pagedirptrindex].XA=1;
  }

  PA=(*(QWORD*)(&pagedirptr[pagedirptrindex])) & MAXPHYADDRMASKPB;
  pagedir=mapPhysicalMemory(PA, 4096);

  PA+=8*pagedirindex;

  if (pagedirptr)
  {
    unmapPhysicalMemory(pagedirptr, 4096);
    pagedirptr=NULL;
  }

  if (forcesmallpage && pagedir[pagedirindex].RA && (pagedir[pagedirindex].BIG)) //it's a big page, so the physical address points to the actual memory. Clear everything
    *(QWORD *)&pagedir[pagedirindex]=0;


  if (pagedir[pagedirindex].RA==0)
  {
    if (has_EPT_2MBSupport && (forcesmallpage==0))
    {
      QWORD GuestAddress2MBAlign=(physicalAddress & 0xFFFFFFFFFFE00000ULL) & MAXPHYADDRMASKPB;
      int memtype, fullmap;
      getMTRRMapInfo(GuestAddress2MBAlign,0x200000, &fullmap, &memtype);

      if (fullmap)
      {
        sendstringf("mapping %6 as a 2MB page with memtype %d\n", GuestAddress2MBAlign, memtype);
        *(QWORD*)(&pagedir[pagedirindex])=GuestAddress2MBAlign & MAXPHYADDRMASKPB;
        pagedir[pagedirindex].RA=1;
        pagedir[pagedirindex].WA=1;
        pagedir[pagedirindex].XA=1;
        pagedir[pagedirindex].BIG=1;
        pagedir[pagedirindex].MEMTYPE=memtype;
        unmapPhysicalMemory(pagedir, 4096);

        csLeave(&currentcpuinfo->EPTPML4CS);
        return PA;
      }
    }

    //still here, try a pagetable
    void *temp=malloc2(4096);
    zeromemory(temp,4096);
    *(QWORD*)(&pagedir[pagedirindex])=VirtualToPhysical(temp) & MAXPHYADDRMASKPB;
    pagedir[pagedirindex].RA=1;
    pagedir[pagedirindex].WA=1;
    pagedir[pagedirindex].XA=1;
  }

  //still here, so not mapped as a pagedir entry
  PA=(*(QWORD*)(&pagedir[pagedirindex])) & MAXPHYADDRMASKPB;
  pagetable=mapPhysicalMemory(PA, 4096);

  PA+=8*pagetableindex;

  if (pagedir)
  {
    unmapPhysicalMemory(pagedir,4096);
    pagedir=NULL;
  }

  if (pagetable[pagetableindex].RA==0)
  {
    int memtype, fullmap;
    getMTRRMapInfo(physicalAddress & MAXPHYADDRMASKPB,0x1000, &fullmap, &memtype);
    if (!fullmap)
    {
      sendstring("Assertion Fail: fullmap is false for a 1 page range");
      while (1);
    }

    sendstringf("mapping %6 as a 4KB page with memtype %d\n", physicalAddress & MAXPHYADDRMASKPB, memtype);
    *(QWORD*)(&pagetable[pagetableindex])=physicalAddress & MAXPHYADDRMASKPB;
    pagetable[pagetableindex].RA=1;
    pagetable[pagetableindex].WA=1;
    pagetable[pagetableindex].XA=1;
    pagetable[pagetableindex].MEMTYPE=memtype;
  }
  else
  {
    //else already mapped
    sendstringf("This physical address (%6) was already mapped\n", physicalAddress);
  }

  unmapPhysicalMemory(pagetable,4096);

  csLeave(&currentcpuinfo->EPTPML4CS);

  return PA;
}

int handleEPTViolation(pcpuinfo currentcpuinfo, VMRegisters *vmregisters UNUSED, PFXSAVE64 fxsave UNUSED)
{
  //EPT_VIOLATION_INFO vi;
  sendstring("handleEPTViolation\n");


  VMExit_idt_vector_information idtvectorinfo;
  idtvectorinfo.idtvector_info=vmread(vm_idtvector_information);

  if (idtvectorinfo.valid)
  {
    //handle this EPT event and reinject the interrupt
    VMEntry_interruption_information newintinfo;
    newintinfo.interruption_information=0;

    newintinfo.interruptvector=idtvectorinfo.interruptvector;
    newintinfo.type=idtvectorinfo.type;
    newintinfo.haserrorcode=idtvectorinfo.haserrorcode;
    newintinfo.valid=idtvectorinfo.valid; //should be 1...
    vmwrite(vm_entry_exceptionerrorcode, vmread(vm_idtvector_error)); //entry errorcode
    vmwrite(0x4016, newintinfo.interruption_information); //entry info field
    vmwrite(0x401a, vmread(vm_exit_instructionlength)); //entry instruction length
  }

 //vi.ExitQualification=vmread(vm_exit_qualification);

  QWORD GuestAddress=vmread(vm_guest_physical_address);

  int watchid=ept_inWatchRegionPage(currentcpuinfo, GuestAddress);
  if (watchid!=-1) //at least one page is being watched. (So it means it's already mapped, which means this violation is caused by me no matter which one)
  {
    sendstringf("Handling watch page (PA=%6 VA=%6)\n", GuestAddress, vmread(vm_guest_linear_address));
    return ept_handleWatchEvent(currentcpuinfo, vmregisters, fxsave, watchid);
  }

  //check for cloak
  if (ept_handleCloakEvent(currentcpuinfo, GuestAddress))
    return 0;

  //still here, so not a watch or cloak
  sendstringf("Mapping %6\n", GuestAddress);
  EPTMapPhysicalMemory(currentcpuinfo, GuestAddress, 0);


  return 0;

}

int handleEPTMisconfig(pcpuinfo currentcpuinfo UNUSED, VMRegisters *vmregisters UNUSED)
{
  sendstring("handleEPTMisconfig\n");
  return 1;
}

