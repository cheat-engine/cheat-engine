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

QWORD EPTMapPhysicalMemory(pcpuinfo currentcpuinfo, QWORD physicalAddress, int forcesmallpage);

int getFreeWatchID(pcpuinfo currentcpuinfo)
//scan through the watches for an unused spot, if not found, reallocate the list
{
  int i,j;
  for (i=0; i<currentcpuinfo->eptwatchlistLength; i++)
  {
    if (currentcpuinfo->eptwatchlist[i].Active==0)
      return i;
  }

  //still here, realloc

  i=currentcpuinfo->eptwatchlistLength;

  currentcpuinfo->eptwatchlistLength=(currentcpuinfo->eptwatchlistLength+2)*2;
  currentcpuinfo->eptwatchlist=realloc(currentcpuinfo->eptwatchlist, currentcpuinfo->eptwatchlistLength*sizeof(EPTWatchEntry));


  for (j=i; j<currentcpuinfo->eptwatchlistLength; j++)
    currentcpuinfo->eptwatchlist[j].Active=0;

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
  return ((currentcpuinfo->eptwatchlist[ID].Active) &&
          (
             (address>=currentcpuinfo->eptwatchlist[ID].PhysicalAddress) &&
             (address<currentcpuinfo->eptwatchlist[ID].PhysicalAddress+currentcpuinfo->eptwatchlist[ID].Size)
           )
          );
}

inline int ept_doesAddressMatchWatchListEntry(pcpuinfo currentcpuinfo, QWORD address, int ID)
/*
 * pre: address is already page aligned
 */
{
  return ((currentcpuinfo->eptwatchlist[ID].Active) && ((currentcpuinfo->eptwatchlist[ID].PhysicalAddress & 0xfffffffffffff000ULL) == address));
}

int ept_inWatchRegionPage(pcpuinfo currentcpuinfo, QWORD address)
/*
 * returns -1 if not in a page being watched
 * Note that there can be multiple active on the same page
 */
{
  int i;
  address=address & 0xfffffffffffff000ULL;
  for (i=0; i<currentcpuinfo->eptwatchlistLength; i++)
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

  sendstringf("Handling watch ID %d\n", ID);


  //figure out which access it is really (in case of multiple on the same page)

  for (i=ID; i<currentcpuinfo->eptwatchlistLength; i++)
  {
    if (ept_doesAddressMatchWatchListEntry(currentcpuinfo, PhysicalAddressBase, i))
    {
      if (currentcpuinfo->eptwatchlist[ID].Type==0)
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

  //ID is now set to the most logical watch(usually there is no conflicts, and even if there is, no biggie. But still)

  //run once
  currentcpuinfo->eptwatchlist[ID].EPTEntry->XA=1;
  currentcpuinfo->eptwatchlist[ID].EPTEntry->RA=1;
  currentcpuinfo->eptwatchlist[ID].EPTEntry->WA=1;

  vmx_enableSingleStepMode();
  currentcpuinfo->singleStepping.Reason=1; //EPT watch event
  currentcpuinfo->singleStepping.EPTWatch.ID=ID;

  //save this state?
  if ((evi.R==0) && (evi.X==1))
    return 0; //execute operation (this cpu doesn't support execute only)

  if (currentcpuinfo->eptwatchlist[ID].CopyInProgress) //a copy operation is in progress
    return 0;

  if (((currentcpuinfo->eptwatchlist[ID].Options & EPTO_LOG_ALL)==0) &&
     (
      (PhysicalAddress<currentcpuinfo->eptwatchlist[ID].PhysicalAddress) ||
      (PhysicalAddress>=currentcpuinfo->eptwatchlist[ID].PhysicalAddress+currentcpuinfo->eptwatchlist[ID].Size)
      ))
    return 0; //no need to log it


  //scan if this RIP is already in the list

  switch (currentcpuinfo->eptwatchlist[ID].Log->entryType)
  {
    case 0: logentrysize=sizeof(PageEventBasic); break;
    case 1: logentrysize=sizeof(PageEventExtended); break;
    case 2: logentrysize=sizeof(PageEventBasicWithStack); break;
    case 3: logentrysize=sizeof(PageEventExtendedWithStack); break;
  }

  for (i=0; (DWORD)i<currentcpuinfo->eptwatchlist[ID].Log->numberOfEntries; i++)
  {
    PageEventBasic *peb=(PageEventBasic *)((QWORD)(&currentcpuinfo->eptwatchlist[ID].Log->pe.basic[0])+i*logentrysize);
    //every type starts with a PageEventBasic

    if (peb->RIP==RIP)
    {
      //it's already in the list
      if ((currentcpuinfo->eptwatchlist[ID].Options & EPTO_MULTIPLERIP)==0)
      {
        peb->Count++;
        return 0; //no extra RIP's
      }

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
        peb->Count++;
        return 0; //already in the list
      }

    }

  }

  //still here, so not in the list
  if (currentcpuinfo->eptwatchlist[ID].Log->numberOfEntries>=currentcpuinfo->eptwatchlist[ID].Log->maxNumberOfEntries)
    return 0; //can't add more

  //still here, so not in the list, and still room
  //add it

  i=currentcpuinfo->eptwatchlist[ID].Log->numberOfEntries;
  switch (currentcpuinfo->eptwatchlist[ID].Log->entryType)
  {
    case PE_BASIC:
    {
      fillPageEventBasic(&currentcpuinfo->eptwatchlist[ID].Log->pe.basic[i], registers);
      break;
    }

    case PE_EXTENDED:
    {
      fillPageEventBasic(&currentcpuinfo->eptwatchlist[ID].Log->pe.extended[i].basic, registers);
      currentcpuinfo->eptwatchlist[ID].Log->pe.extended[i].fpudata=*fxsave;
      break;
    }

    case PE_BASICSTACK:
    {
      fillPageEventBasic(&currentcpuinfo->eptwatchlist[ID].Log->pe.basics[i].basic, registers);
      saveStack(currentcpuinfo, currentcpuinfo->eptwatchlist[ID].Log->pe.basics[i].stack);
      break;
    }

    case PE_EXTENDEDSTACK:
    {
      fillPageEventBasic(&currentcpuinfo->eptwatchlist[ID].Log->pe.extendeds[i].basic, registers);
      currentcpuinfo->eptwatchlist[ID].Log->pe.extended[i].fpudata=*fxsave;
      saveStack(currentcpuinfo, currentcpuinfo->eptwatchlist[ID].Log->pe.extendeds[i].stack);
      break;
    }


  }

  currentcpuinfo->eptwatchlist[ID].Log->numberOfEntries++;

  return 0;

}

int ept_handleWatchEventAfterStep(pcpuinfo currentcpuinfo,  int ID)
{
  sendstringf("ept_handleWatchEventAfterStep\n");
  if (currentcpuinfo->eptwatchlist[ID].Type==0)
    currentcpuinfo->eptwatchlist[ID].EPTEntry->WA=0;
  else
  {
    currentcpuinfo->eptwatchlist[ID].EPTEntry->RA=0;
    currentcpuinfo->eptwatchlist[ID].EPTEntry->WA=0;
  }
  vmx_disableSingleStepMode();
  currentcpuinfo->singleStepping.Reason=0;
  currentcpuinfo->singleStepping.EPTWatch.ID=-1;

  return 0;
}


int ept_activateWatch(pcpuinfo currentcpuinfo, int ID)
{
  //(re)map this physical memory and the page descriptors
  sendstringf("ept_activateWatch(%d)\n", ID);
  QWORD PA_EPTE=EPTMapPhysicalMemory(currentcpuinfo, currentcpuinfo->eptwatchlist[ID].PhysicalAddress, 1);

  sendstringf("PA_EPTE=%6\n", PA_EPTE);
  currentcpuinfo->eptwatchlist[ID].EPTEntry=(PEPT_PTE)mapPhysicalMemory(PA_EPTE, sizeof(EPT_PTE));

  //test if needed:  SetPageToWriteThrough(currentcpuinfo->eptwatchlist[ID].EPTEntry);

  //check out 28.3.3.4  (might not be needed due to vmexit, but do check anyhow)

  if (currentcpuinfo->eptwatchlist[ID].Type==0)
  {
    currentcpuinfo->eptwatchlist[ID].EPTEntry->WA=0;
    sendstringf("Make the entry for %6 non writable\n",currentcpuinfo->eptwatchlist[ID].PhysicalAddress);
  }
  //EPTINV

  currentcpuinfo->eptwatchlist[ID].Active=1;
  return 0;
}

int ept_disableWatch(pcpuinfo currentcpuinfo, int ID)
{
  int i;
  int hasAnotherOne=0;
  QWORD PhysicalBase=currentcpuinfo->eptwatchlist[ID].PhysicalAddress & 0xfffffffffffff000ULL;

  for (i=0; i<currentcpuinfo->eptwatchlistLength; i++)
  {
    if (ept_doesAddressMatchWatchListEntry(currentcpuinfo, PhysicalBase, i))
    {
      //matches
      if (currentcpuinfo->eptwatchlist[i].Type==currentcpuinfo->eptwatchlist[ID].Type) //don't undo
        hasAnotherOne=1;
    }
  }

  if (hasAnotherOne==0)
  {
    //undo
    if (currentcpuinfo->eptwatchlist[ID].Type==0)
      currentcpuinfo->eptwatchlist[ID].EPTEntry->WA=1;
    else
    {
      currentcpuinfo->eptwatchlist[ID].EPTEntry->RA=1;
      currentcpuinfo->eptwatchlist[ID].EPTEntry->WA=1;
    }

  }

  unmapPhysicalMemory(currentcpuinfo->eptwatchlist[ID].EPTEntry,sizeof(EPT_PTE));
  currentcpuinfo->eptwatchlist[ID].EPTEntry=NULL;
  currentcpuinfo->eptwatchlist[ID].Active=0;
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

  if (forcesmallpage && pagedirptr[pagedirptrindex].RA)
    pagedirptr[pagedirptrindex].BIG=0;

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

  if (forcesmallpage && pagedir[pagedirindex].RA)
    pagedir[pagedirindex].BIG=0;

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
    //nosendchar[getAPICID()]=0;
    sendstringf("Handling watch page (PA=%6 VA=%6)\n", GuestAddress, vmread(vm_guest_linear_address));
    return ept_handleWatchEvent(currentcpuinfo, vmregisters, fxsave, watchid);
  }

  //check for cloak

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

