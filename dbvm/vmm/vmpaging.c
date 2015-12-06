#include "vmpaging.h"
#include "vmmhelper.h"
#include "common.h"
#include "mm.h"
#include "multicore.h"
#include "main.h"
#include "vmreadwrite.h"

unsigned int pagetablesize;

volatile void *nonpagedInvalidEmulationPagedir; //basicly one pagetable that maps a invalid cs state page (caused by entering realmode from protected mode above 1mb)

volatile int TLBhasBeenSetup=0;

criticalSection setupTLB;

int allocateVirtualTLB(void)
/* will allocate memory for a virtual TLB
 * Out of the 4MB */
{
  int i;
  int maxAllocMem;
  int AvailableForPaging;
  int orig=nosendchar[getAPICID()];

  nosendchar[getAPICID()]=0;

  enableserial();
  sendstring("---------------------------------------------------->");


  sendstring("Allocating a virtual TLB\n\r");

  if (cpucount==0)
  {
    nosendchar[getAPICID()]=orig;
    return 666; //Please die!
  }

  //cpucount is set,

  csEnter(&setupTLB);
  if (!TLBhasBeenSetup)
  {
    TLBhasBeenSetup=1;


    maxAllocMem=maxAllocatableMemory();
    sendstringf("maxAllocatableMemory()=0x%x  (-32*1024=%x)\n", maxAllocMem, maxAllocMem-32*1024 );
    sendstringf("cpucount=%d\n", cpucount);
    sendstringf("(maxAllocMem - 32*1024) / cpucount = %d\n",(maxAllocMem - 32*1024) / cpucount, cpucount);

    AvailableForPaging = (maxAllocMem - 32*1024*cpucount) / cpucount;
    AvailableForPaging -= 4096; //keep some memory for some thing extra

    if (AvailableForPaging<0)
    {
      sendstringf("1: Not enough memory for DBVM functioning");
      while (1) ;

    }

    sendstringf("AvailableForPaging before alignment fix: 0x%x\n", AvailableForPaging);

    AvailableForPaging -= AvailableForPaging % 4096;

    sendstringf("AvailableForPaging after alignment fix: 0x%x\n", AvailableForPaging);

    if (AvailableForPaging<0)
    {
      sendstringf("2: Not enough memory for DBVM functioning");
      while (1) ;

    }



    for (i=0; i<cpucount; i++)
    {
        cpuinfo[i].virtualTLB= malloc(AvailableForPaging);
        sendstringf("allocated a virtualTLB for cpu %d at %p", i, cpuinfo[i].virtualTLB);

        if (cpuinfo[i].virtualTLB==NULL)
        {
          sendstringf("Allocation failed\n");
          while (1);
        }

        cpuinfo[i].virtualTLB_PA = VirtualToPhysical((UINT64)cpuinfo[i].virtualTLB);

        cpuinfo[i].virtualTLB_FreeSpot = cpuinfo[i].virtualTLB + 4096;
        cpuinfo[i].virtualTLB_Max = (AvailableForPaging / 2) & (0xfffff000);
        cpuinfo[i].virtualTLB_Lookup = cpuinfo[i].virtualTLB + cpuinfo[i].virtualTLB_Max;

        zeromemory(cpuinfo[i].virtualTLB,4096);



        sendstringf("Setup virtualTLB for cpu %d:\n\r",i);
        sendstringf("virtualTLB=%x\n\r", (UINT64)cpuinfo[i].virtualTLB);
        sendstringf("virtualTLB_FreeSpot=%x\n\r", (UINT64)cpuinfo[i].virtualTLB_FreeSpot);
        sendstringf("virtualTLB_Max=%x\n\r", (ULONG)cpuinfo[i].virtualTLB_Max);
        sendstringf("virtualTLB_Lookup=%x\n\r", (UINT64)cpuinfo[i].virtualTLB_Lookup);

    }

  }



  csLeave(&setupTLB);


  nosendchar[getAPICID()]=orig;
  return 0;
}

void handleFullTLB(pcpuinfo currentcpuinfo)
{
  if ((currentcpuinfo->virtualTLB_FreeSpot-currentcpuinfo->virtualTLB) > currentcpuinfo->virtualTLB_Max)
  //if ((currentcpuinfo->virtualTLB_FreeSpot-currentcpuinfo->virtualTLB) > 0x6000)
	{
		//lame method: Clean up whole tlb and restart

    sendstring("Whiping TLB\n\r");
    emulatePaging(currentcpuinfo); //will whipe tlb

	}
}




void fillTLB_PML4_64(pcpuinfo currentcpuinfo, PPDE_PAE activepml4entry, PPDE_PAE guestpml4entry)
{
  UINT64 GuestPA=*(UINT64 *)(guestpml4entry) & 0x0fffffff000ULL;

  if (guestpml4entry->A==0)
    return;

  currentcpuinfo->virtualTLB_Lookup[(currentcpuinfo->virtualTLB_FreeSpot-currentcpuinfo->virtualTLB)/4096]=GuestPA;


  //has no dirty bit, so no need to do something special for the W bit
  *(UINT64 *)(activepml4entry) = *(UINT64 *)(guestpml4entry);

  //has a pagedirptr, so allocate one for it
  zeromemory(currentcpuinfo->virtualTLB_FreeSpot,4096);
  activepml4entry->PFN=VirtualToPhysical((UINT64)currentcpuinfo->virtualTLB_FreeSpot) >> 12;
  currentcpuinfo->virtualTLB_FreeSpot+=4096;

  handleFullTLB(currentcpuinfo);
}

void fillTLB_PDPTR_64(pcpuinfo currentcpuinfo, PPDE_PAE activepagedirptrentry, PPDE_PAE guestpagedirptrentry)
{
  //has no dirty bit, so no need to do something special for the W bit
  UINT64 GuestPA=*(UINT64 *)(guestpagedirptrentry) & 0x0fffffff000ULL;

  if (guestpagedirptrentry->A==0)
    return;

  currentcpuinfo->virtualTLB_Lookup[(currentcpuinfo->virtualTLB_FreeSpot-currentcpuinfo->virtualTLB)/4096]=GuestPA;

  *(UINT64 *)(activepagedirptrentry) = *(UINT64 *)(guestpagedirptrentry);

  //has a pagedirptr, so allocate one for it
  zeromemory(currentcpuinfo->virtualTLB_FreeSpot,4096);
  activepagedirptrentry->PFN=VirtualToPhysical((UINT64)currentcpuinfo->virtualTLB_FreeSpot) >> 12;
  currentcpuinfo->virtualTLB_FreeSpot+=4096;

  handleFullTLB(currentcpuinfo);
}

void fillTLB_PDE_64(pcpuinfo currentcpuinfo, PPDE_PAE activepagedirentry, PPDE_PAE guestpagedirentry)
{
  if (guestpagedirentry->A==0)
    return;

  *(UINT64 *)(activepagedirentry) = *(UINT64 *)(guestpagedirentry);

  if (guestpagedirentry->PS==0)
  {
    //has a pageentry
    UINT64 GuestPA=*(UINT64 *)(guestpagedirentry) & 0x0fffffff000ULL;
    currentcpuinfo->virtualTLB_Lookup[(currentcpuinfo->virtualTLB_FreeSpot-currentcpuinfo->virtualTLB)/4096]=GuestPA;


    sendstringf("pagedir: It has a pageentry, so allocated a empty pageentry for it (%8)\n\r",(UINT64)VirtualToPhysical((UINT64)currentcpuinfo->virtualTLB_FreeSpot));
    zeromemory(currentcpuinfo->virtualTLB_FreeSpot,4096);
    activepagedirentry->PFN=VirtualToPhysical((UINT64)currentcpuinfo->virtualTLB_FreeSpot) >> 12;
    currentcpuinfo->virtualTLB_FreeSpot+=4096;
  }
  else
  {
    //big page
    if (guestpagedirentry->D==0)
    {
      sendstring("pagedir: D==0 && PS==1: set RW to 0\n\r");
      activepagedirentry->RW=0; //to be able to capture writes so dirty gets set
    }
  }

  handleFullTLB(currentcpuinfo);

}

void fillTLB_PTE_64(PPTE_PAE activepagetableentry, PPTE_PAE guestpagetableentry)
{
  if (guestpagetableentry->A==0)
    return;

  *(UINT64*)(activepagetableentry) = *(UINT64 *)(guestpagetableentry);

  if (guestpagetableentry->D==0)
    activepagetableentry->RW=0; //to be able to capture writes so dirty gets set
}



void fillTLB_PDPTE_PAE(pcpuinfo currentcpuinfo, PPDPTE_PAE activepagedirptrentry, PPDPTE_PAE guestpagedirptrentry)
{
  *(UINT64 *)(activepagedirptrentry) = *(UINT64 *)(guestpagedirptrentry);

  //has a pageentry
  sendstringf("pagedirptr: allocated a empty pagedir for it (%8)\n\r",(UINT64)VirtualToPhysical((UINT64)currentcpuinfo->virtualTLB_FreeSpot));
  zeromemory(currentcpuinfo->virtualTLB_FreeSpot,4096);
  activepagedirptrentry->PFN=VirtualToPhysical((UINT64)currentcpuinfo->virtualTLB_FreeSpot) >> 12;
  currentcpuinfo->virtualTLB_FreeSpot+=4096;

  handleFullTLB(currentcpuinfo); //check if the tlb is full and if so make adjustments
}

void fillTLB_PDE_PAE(pcpuinfo currentcpuinfo, PPDE_PAE activepagedirentry, PPDE_PAE guestpagedirentry)
{
  //retrieve data of guest

  if (guestpagedirentry->A==0)
  {
    activepagedirentry->P=0;
    return;
  }

  *(unsigned long long *)(activepagedirentry) = *(unsigned long long *)(guestpagedirentry);

  if ((*(unsigned long long *)(activepagedirentry)) >=0x100000000ULL)
  {
    nosendchar[getAPICID()]=0;
    sendstring("Have set a invalid PDE_PAE entry\n\r");
  }

  if (guestpagedirentry->PS==0)
  {
    //has a pageentry
    sendstringf("pagedir: It has a pageentry, so allocated a empty pageentry for it (%8)\n\r",(UINT64)VirtualToPhysical((UINT64)currentcpuinfo->virtualTLB_FreeSpot));
    zeromemory(currentcpuinfo->virtualTLB_FreeSpot,4096);
    activepagedirentry->PFN=VirtualToPhysical((UINT64)currentcpuinfo->virtualTLB_FreeSpot) >> 12;
    currentcpuinfo->virtualTLB_FreeSpot+=4096;
  }

  if ((guestpagedirentry->D==0) && (guestpagedirentry->PS==1) )
  {
    sendstring("pagedir: D==0 && PS==1 && no write operation, set RW to 0\n\r");
    activepagedirentry->RW=0; //to be able to capture writes so dirty gets set
  }
  handleFullTLB(currentcpuinfo); //check if the tlb is full and if so make adjustments
}

void fillTLB_PTE_PAE(PPTE_PAE activepagetableentry, PPTE_PAE guestpagetableentry)
{
  if (guestpagetableentry->A==0)
  {
    activepagetableentry->P=0;
    return;
  }

  *(unsigned long long *)(activepagetableentry) = *(unsigned long long *)(guestpagetableentry);

  if ((*(unsigned long long *)(activepagetableentry)) >=0x100000000ULL)
  {
    nosendchar[getAPICID()]=0;
    sendstring("Have set a invalid PTE_PAE entry\n\r");
  }


  //e
  if (guestpagetableentry->D==0)
    activepagetableentry->RW=0; //to be able to capture writes so dirty gets set
}




void fillTLB_PDE(pcpuinfo currentcpuinfo, PPDE activepagedirentry, PPDE guestpagedirentry)
{
  //retrieve data of guest

  if (guestpagedirentry->A==0)
  {
    activepagedirentry->P=0;
    return;
  }

  *(ULONG *)(activepagedirentry) = *(ULONG *)(guestpagedirentry);// & 0xfffff000;

  if (guestpagedirentry->PS==0)
  {
    //has a pageentry
    sendstringf("pagedir: It has a pageentry, so allocated a empty pageentry for it (%8)\n\r",(UINT64)VirtualToPhysical((UINT64)currentcpuinfo->virtualTLB_FreeSpot));
    zeromemory(currentcpuinfo->virtualTLB_FreeSpot,4096);
    activepagedirentry->PFN=VirtualToPhysical((UINT64)currentcpuinfo->virtualTLB_FreeSpot) >> 12;
    currentcpuinfo->virtualTLB_FreeSpot+=4096;
  }



  if ((guestpagedirentry->D==0) && (guestpagedirentry->PS==1) )
  {
    sendstring("pagedir: D==0 && PS==1 && no write operation, set RW to 0\n\r");
    activepagedirentry->RW=0; //to be able to capture writes so dirty gets set
  }

  //i

  handleFullTLB(currentcpuinfo); //check if the tlb is full and if so make adjustments


}

void fillTLB_PTE(PPTE activepagetableentry, PPTE guestpagetableentry)
{
  if (guestpagetableentry->A==0)
  {
    activepagetableentry->P=0;
    return;
  }

  *(ULONG *)(activepagetableentry) = *(ULONG *)(guestpagetableentry);// & 0xfffff000;

  if (guestpagetableentry->D==0)
    activepagetableentry->RW=0; //to be able to capture writes so dirty gets set


}

void * mapVMmemory(pcpuinfo currentcpuinfo, UINT64 address, int size,  UINT64 VirtualAddress, int *error, UINT64 *pagefaultaddress )
{
  /* Will map the virtual machine's virtual memory  to the vmm's virtual address  *
   * error=0: no error
   * error=1: not enough memory
   * error=2: pagefault, pagefaultaddress is then filled with the address that needs to be paged
   * error=3: too big size
   * returns the exact address for the vmm
   *
   * note: maxsize=0x001ffff
   */
  //allocate a pagedir (use the virtual tlb space)
  if (currentcpuinfo->virtualTLB == NULL)
    allocateVirtualTLB();

  unsigned int  availablememory = currentcpuinfo->virtualTLB_Max-(currentcpuinfo->virtualTLB_FreeSpot-currentcpuinfo->virtualTLB);
  int           Dirptr          = (UINT64)VirtualAddress >> 30;
  int           Dir             = (UINT64)(VirtualAddress >> 21) & 0x1ff;
  PPDE_PAE      usedpagedir     = (PPDE_PAE)((UINT64)pagedirvirtual+Dirptr*0x1000);

  UINT64         currentaddress  = address & 0xfffffffffffff000ULL;
  UINT64         lastaddress     = address+size;
  PPTE_PAE      newpagetable;
  int           i;

  void *r=NULL;

  *pagefaultaddress=0;


/*
  sendstring("Inside mapVMmemory\n\r");

  sendstringf("address=%6\n", address);
  sendstringf("VirtualAddress=%6\n", VirtualAddress);


  sendstringf("currentaddress=%6\n",currentaddress);
  sendstringf("lastaddress=%6\n",lastaddress);


  sendstringf("usedpagedir = %6\n\r",(UINT64)usedpagedir);
  sendstringf("Dirptr=%d Dir=%d \n\r",Dirptr,Dir);

*/

  //check if there is already a tlb, if not, allocate it. (it's needed for temp mem)


  if (size >= 0x00200000)
  {
    //sendstring("Size too big\n");
    *error=3;
    return 0;
  }

  if (currentcpuinfo->virtualTLB_Max<4096)
  {
    //sendstring("Not enough memory\n");
    *error=1;
    return 0;
  }

  if (availablememory<4096*2)  //*2 to keep some space for the paging system that first allocs, and then checks
  {
    //sorry for performance, but...
    zeromemory(currentcpuinfo->virtualTLB,4096);
    currentcpuinfo->virtualTLB_FreeSpot=currentcpuinfo->virtualTLB+4096;
    currentcpuinfo->virtualTLB_whiped=1;
  }



  //there is enough memory
  newpagetable=currentcpuinfo->virtualTLB_FreeSpot;
  currentcpuinfo->virtualTLB_FreeSpot=currentcpuinfo->virtualTLB_FreeSpot+4096;

  *(unsigned long long*)&usedpagedir[Dir]=(VirtualToPhysical((UINT64)newpagetable) & 0xFFFFFFFFFFFFF000ULL);
  usedpagedir[Dir].P=1;
  usedpagedir[Dir].PS=0; //has pagetable
  usedpagedir[Dir].RW=1;
  usedpagedir[Dir].US=1;



  for (i=0; i<512; i++)
    *(unsigned long long*)&newpagetable[i]=0;

  //fill in the pagetable
  for (i=0; currentaddress<lastaddress; i++, currentaddress+=4096)
  {
    int notpaged=0;
    UINT64 PhysAddress=getPhysicalAddressVM(currentcpuinfo,currentaddress, &notpaged);

    if (notpaged)
    {
      //sendstringf("notpaged is 1 (currentaddress=%8)\n\r", currentaddress);
      *error=2;
      *pagefaultaddress=currentaddress;
      return VirtualAddress+(address & 0xfff);
    }

    *(unsigned long long*)&newpagetable[i]=((UINT64)PhysAddress & 0xFFFFFFFFFFFFF000ULL);
    newpagetable[i].P=1;
    newpagetable[i].RW=1;
    newpagetable[i].US=1;

    //sendstringf("mapped %8 at pagetablentry %d\n\r",(ULONG)PhysAddress, i);

    _invlpg(VirtualAddress+i*4096);
    //sendstringf("Invalidating virtual address %8\n\r",VirtualAddress+i*4096);
  }



  _invlpg(VirtualAddress);

  //sendstring("Everything ok\n");

  *error=0;

  r=(void *)(VirtualAddress+((UINT64)address & 0xfff));

 // sendstringf("Returning %6\n", r);

  if (VirtualAddress==0)
    bochsbp(); //step to find the caller

  return r;
}

UINT64 getPhysicalAddressVM(pcpuinfo currentcpuinfo, UINT64 address, int *notpaged)
{
  regCR4 usedCR4;
  DWORD cr0;
  if (isAMD)
  {
    usedCR4.CR4=currentcpuinfo->vmcb->CR4;
    cr0=currentcpuinfo->vmcb->CR0;
  }
  else
  {
    usedCR4.CR4=vmread(vm_guest_cr4);
    cr0=vmread(vm_cr0_fakeread);
  }

  UINT64  pagebase;


  sendstringf("inside getPhysicalAddressVM , for address %6\n\r",address);
  *notpaged=0;

  if (isAMD)
  {
    regCR0 usedCR0;
    usedCR0.CR0=cr0;

    if (usedCR0.PG)
    {
      //paging, so look up the paging table
      pagebase=currentcpuinfo->vmcb->CR3;
    }
    else
    {
      //not paged, return the given address
      pagebase=0;
      return address;
    }
  }
  else
  {
    if ((cr0 & 0x80000001) == 0x80000001)
    {
      //sendstring("protectedmode and paging, so use guest's pagetable\n\r");
      pagebase=currentcpuinfo->guestCR3; //protected mode with paging on, so use the guest's pagetable
      //sendstringf("pagebase=%x\n\r",pagebase);
    }
    else
    {
      //sendstring("realmode or nonpaged, use emulated pagetable\n\r");
      pagebase=vmread(vm_guest_cr3); //realmode or nonpaged, so use the emulated nonpaged pagetable
    }
  }

  //sendstringf("pagebase=%8\n\r",pagebase);
  if (IS64BITPAGING(currentcpuinfo))
  {
    UINT64       pml4entry=(UINT64)address >> 39 & 0x1ff;
    PPDE_PAE    realpml4table=(PPDE_PAE)MapPhysicalMemory((UINT64)pagebase,currentcpuinfo->AvailableVirtualAddress+0x0f000000);
    //sendstring("In 64-bit paging mode\n\r");


    if (realpml4table[pml4entry].P)
    {
      UINT64     pagedirptrentry=(UINT64)address >> 30 & 0x1ff;
      PPDE_PAE  realpagedirptrtable=(PPDE_PAE)MapPhysicalMemory((UINT64)realpml4table[pml4entry].PFN << 12,  currentcpuinfo->AvailableVirtualAddress+0x0f200000);
      //sendstring("realpml4table[pml4entry].P==1\n\r");

      if (realpagedirptrtable[pagedirptrentry].P)
      {
        PPDE_PAE realpagedirtable=(PPDE_PAE)MapPhysicalMemory((UINT64)realpagedirptrtable[pagedirptrentry].PFN << 12,currentcpuinfo->AvailableVirtualAddress+0x0f400000);
        UINT64    pagedirentry=(UINT64)address >> 21 & 0x1ff;
        //sendstring("realpagedirptrtable[pagedirptrentry].P==1\n\r");

        if (realpagedirtable[pagedirentry].P)
        {
          //sendstring("realpagedirtable[pagedirentry].P==1\n\r");
          if (realpagedirtable[pagedirentry].PS)
          {
            //sendstring("realpagedirtable[pagedirentry].PS==1\n\r");
            return ((UINT64)realpagedirtable[pagedirentry].PFN << 12)+ (UINT64)(address & 0x1FFFFF);
          }
          else
          {
            UINT64    pagetableentry=address >> 12 & 0x1ff; //when it's not a big sized page
            PPTE_PAE realpagetable=(PPTE_PAE)MapPhysicalMemory((UINT64)realpagedirtable[pagedirentry].PFN << 12,  currentcpuinfo->AvailableVirtualAddress+0x0f600000);

            //sendstring("realpagedirtable[pagedirentry].PS==0\n\r");
            //sendstring("realpagetable[pagetableentry].P==1\n\r");

            if (realpagetable[pagetableentry].P)
              return ((UINT64)realpagetable[pagetableentry].PFN << 12)+ (UINT64)(address & 0xFFF);
          }
        }
      }
    }
  }
  else
  if (usedCR4.PAE)
  {
    //in pae mode
   // sendstring("PAE paging\n\r");

    UINT64      pagedirptrentry=(UINT64)address >> 30;
    //sendstringf("pagedirptrentry=%d\n\r", pagedirptrentry);

    PPDPTE_PAE realpagedirptrtable=(PPDPTE_PAE)MapPhysicalMemory(pagebase,currentcpuinfo->AvailableVirtualAddress+0x0f000000);

    if (realpagedirptrtable[pagedirptrentry].P)
    {
      UINT64    pagedirentry=(UINT64)address >> 21 & 0x1ff;

      //sendstringf("pagedirentry=%d\n\r", pagedirentry);
      //sendstringf("realpagedirptrtable[pagedirptrentry].PFN=%6\n\r",(UINT64)realpagedirptrtable[pagedirptrentry].PFN);

      PPDE_PAE realpagedirtable=(PPDE_PAE)MapPhysicalMemory((UINT64)realpagedirptrtable[pagedirptrentry].PFN << 12,(UINT64)currentcpuinfo->AvailableVirtualAddress+0x0f200000);

      if (realpagedirtable[pagedirentry].P)
      {
        //sendstring("realpagedirtable[pagedirentry].P==1\n\r");
        if (realpagedirtable[pagedirentry].PS)
        {
          //sendstringf("realpagedirtable[pagedirentry].PFN=%x\n\r",realpagedirtable[pagedirentry].PFN);
          return ((UINT64)realpagedirtable[pagedirentry].PFN << 12)+ ((UINT64)address & 0x1FFFFF);
        }
        else
        {
          UINT64    pagetableentry=(UINT64)address >> 12 & 0x1ff; //when it's not a big sized page
          //sendstringf("pagetableentry=%d\n\r",pagetableentry);
          PPTE_PAE realpagetable=(PPTE_PAE)MapPhysicalMemory((UINT64)realpagedirtable[pagedirentry].PFN << 12, currentcpuinfo->AvailableVirtualAddress+0x0f400000);

          if (realpagetable[pagetableentry].P==1)
            return ((UINT64)realpagetable[pagetableentry].PFN << 12)+ (address & 0xFFF);
        }
      }
    }
  }
  else
  {
    //nonpae mode
    ULONG pagedirentry=address >> 22;
    PPDE  realpagedirtable=(PPDE)MapPhysicalMemory(pagebase, currentcpuinfo->AvailableVirtualAddress+0x0f000000);
    sendstring("Non-PAE mode\n\r");

    if (realpagedirtable[pagedirentry].P)
    {
      if (realpagedirtable[pagedirentry].PS)
      {
        //big page
        return ((UINT64)realpagedirtable[pagedirentry].PFN << 12)+ (address & 0x3FFFFF);
      }
      else
      {
        ULONG pagetableentry=(UINT64)address >> 12 & 0x3ff; //when it's not a big sized page
        PPTE  realpagetable=(PPTE)MapPhysicalMemory((UINT64)realpagedirtable[pagedirentry].PFN << 12, currentcpuinfo->AvailableVirtualAddress+0x0f200000);

        //it has a pagetable
        if (realpagetable[pagetableentry].P==1)
          return ((UINT64)realpagetable[pagetableentry].PFN << 12)+ (address & 0xFFF);
      }
    }
  }

  //got here, so:
  sendstring("Not handled by anything\n\r");

  *notpaged=1;
  return 0;
}

int ReadBytesFromPage(pcpuinfo currentcpuinfo, UINT64 address, unsigned char *buf,int size)
/*
 * Reads the memory from a specific page, does not handle overlapped reads
 * Returns the total number of bytes read
 */
{
  int i;
  int notpaged=0;
  QWORD physicalAddress;
  physicalAddress=getPhysicalAddressVM(currentcpuinfo, address, &notpaged);

  if (!notpaged)
  {
    //map this pgysical address
    unsigned char *tempbuf=(unsigned char *)MapPhysicalMemory(physicalAddress, currentcpuinfo->AvailableVirtualAddress);
    int leftforthispage=0x1000-(address & 0xfff);

    if (leftforthispage>size)
      leftforthispage=size;

    //copy the data to the buffer
    for (i=0; i<leftforthispage; i++)
      buf[i]=tempbuf[i];

    return leftforthispage;
  }
  else
    return 0; //0 bytes read

}

int ReadVMMemory(pcpuinfo currentcpuinfo, UINT64 address, unsigned char *buf,int size)
/*
 * Same as ReadBytesFromPage but deals with multiple pages
 */
{
  int position=0;

  while (position<size)
  {
    int lastread;
    lastread=ReadBytesFromPage(currentcpuinfo, address+position, &buf[position], size-position);
    position+=lastread;

    if (lastread==0)
      break;
  }

  return position;
}


int handleINVLPG(pcpuinfo currentcpuinfo)
{
  UINT64 address=vmread(0x6400);
  regCR0 fakeCR0;
  regCR4 fakeCR4;

  //nosendchar[getAPICID()]=0;

  fakeCR0.CR0=vmread(0x6004);
  fakeCR4.CR4=vmread(0x6006);

  sendstringf("%8 :handling INVLPG for address %6\n\r",vmread(0x681e),address);
  vmwrite(0x681e,vmread(0x681e)+vmread(0x440c));

  if (!memorycloak)
    return 0;

  if (currentcpuinfo->virtualTLB==NULL)
  {
    //sendstring("No virtualTLB allocated\n\r");
    return 0;
  }

  sendstringf("There is a virtualTLB, so handling it\n\r");

  if (fakeCR0.PE==0 || fakeCR0.PG==0) //if protectedmode or paging is off then do nothing
    return 0;

  if (IS64BITPAGING(currentcpuinfo))
  {
    ULONG       pml4entry=address >> 39 & 0x1ff;
    PPDE_PAE    realpml4table=(PPDE_PAE)currentcpuinfo->virtualTLB;

    sendstring("In 64-bit paging mode\n\r");

    if (realpml4table[pml4entry].P)
    {
      ULONG     pagedirptrentry=(address >> 30) & 0x1ff;
      PPDE_PAE  realpagedirptrtable=(PPDE_PAE)MapPhysicalMemory(realpml4table[pml4entry].PFN << 12, currentcpuinfo->AvailableVirtualAddress+0x0f200000);

      if (realpagedirptrtable[pagedirptrentry].P)
      {
        PPDE_PAE realpagedirtable=(PPDE_PAE)MapPhysicalMemory(realpagedirptrtable[pagedirptrentry].PFN << 12, currentcpuinfo->AvailableVirtualAddress+0x0f400000);
        ULONG    pagedirentry=(address >> 21) & 0x1ff;

        if (realpagedirtable[pagedirentry].P)
        {
          if (realpagedirtable[pagedirentry].PS)
          {
            realpagedirtable[pagedirentry].P=0;
            return 0;
          }
          else
          {
            ULONG    pagetableentry=(address >> 12) & 0x1ff; //when it's not a big sized page
            PPTE_PAE realpagetable=(PPTE_PAE)MapPhysicalMemory(realpagedirtable[pagedirentry].PFN << 12,  currentcpuinfo->AvailableVirtualAddress+0x0f600000);

            realpagetable[pagetableentry].P=0;
            return 0;
          }
        }
      }
    }

//    nosendchar[getAPICID()]=0;
    //sendstring("Couldn\'t invalidate since it isn\'t paged\n\r");
  }
  else
  if (fakeCR4.PAE)
  {
    //----------------------------------------------------------------------------------
    //                                       PAE
    //----------------------------------------------------------------------------------
    ULONG       pagedirptrentry=address >> 30;
    ULONG       pagedirentry=address >> 21 & 0x1ff;
    ULONG       pagetableentry=address >> 12 & 0x1ff; //when it's not a big sized page
    PPDPTE_PAE  realpagedirptrtable=currentcpuinfo->virtualTLB;
    PPDE_PAE    realpagedirtable=NULL;
    PPTE_PAE    realpagetable=NULL;

    if (realpagedirptrtable[pagedirptrentry].P)
      realpagedirtable = (PPDE_PAE)MapPhysicalMemory(realpagedirptrtable[pagedirptrentry].PFN << 12, currentcpuinfo->AvailableVirtualAddress);

    if (realpagedirtable[pagedirentry].P && realpagedirtable[pagedirentry].PS==0)
      realpagetable = (PPTE_PAE)MapPhysicalMemory(realpagedirtable[pagedirentry].PFN << 12, currentcpuinfo->AvailableVirtualAddress+0x00200000);

    //set the present bit to 0 on the pagetable or pagedir (if it's a PS==1)
    sendstringf("pagedirptrtable=%8 pagedirtable=%8 pagetable=%8\n\r",(UINT64)VirtualToPhysical((UINT64)realpagedirptrtable), (UINT64)VirtualToPhysical((UINT64)realpagedirtable), (UINT64)VirtualToPhysical((UINT64)realpagetable));

    sendstringf("pagedirptrentry=%d (%x) pagedirentry=%d (%x) pagetableentry=%d (%x)\n\r",pagedirptrentry, pagedirptrentry*8 ,pagedirentry, pagedirentry*8, pagetableentry, pagetableentry*8);

    sendstringf("before:\n\r");
    sendstringf("realpagedirptrtable[pagedirptrentry]=%6\n\r",*(unsigned long long *)(&realpagedirptrtable[pagedirptrentry]));
    if (realpagedirtable)
    {
      sendstringf("realpagedirtable[pagedirentry]=%6\n\r",*(unsigned long long *)(&realpagedirtable[pagedirentry]));
    }

    if (realpagetable)
    {
      sendstringf("realpagetable[pagetableentry]=%6\n\r",*(unsigned long long *)(&realpagetable[pagetableentry]));
    }


    if (realpagedirptrtable[pagedirptrentry].P==1)
    {
      if (realpagedirtable[pagedirentry].P==1)
      {
        if (realpagedirtable[pagedirentry].PS==1)
          realpagedirtable[pagedirentry].P=0; //big page, so this is the offending entry
        else
          realpagetable[pagetableentry].P=0; //it has a pagetable
      }

    }
    else
    {
      sendstringf("realpagedirptrtable[pagedirptrentry].P==0\n\r");
    }

    sendstring("after:\n\r");
    sendstringf("realpagedirptrtable[pagedirentry]=%6\n\r",*(unsigned long long *)(&realpagedirptrtable[pagedirptrentry]));
    if (realpagedirtable)
    {
    	sendstringf("realpagedirtable[pagedirentry]=%6\n\r",*(unsigned long long *)(&realpagedirtable[pagedirentry]));
    }

    if (realpagetable)
    {
      sendstringf("realpagetable[pagetableentry]=%6\n\r",*(unsigned long long *)(&realpagetable[pagetableentry]));
    }

    return 0;
  }
  else
  {
    //non pae
    ULONG pagedirentry=address >> 22;
    ULONG pagetableentry=address >> 12 & 0x3ff; //when it's not a big sized page
    PPDE  realpagedirtable=currentcpuinfo->virtualTLB;
    PPTE  realpagetable=(PPTE)MapPhysicalMemory(realpagedirtable[pagedirentry].PFN << 12, currentcpuinfo->AvailableVirtualAddress);

    if (realpagedirtable[pagedirentry].P==1)
    {
      if (realpagedirtable[pagedirentry].PS==1)
        realpagedirtable[pagedirentry].P=0; //big page, so this is the offending entry
      else
        realpagetable[pagetableentry].P=0; //it has a pagetable
    }
    return 0;
  }

  return 0;
}


int handleVMPageException(pcpuinfo currentcpuinfo)
{
  regCR0 fakeCR0;
  regCR4 fakeCR4;
	PFerrorcode errorcode;
  UINT64 fakepagebase=currentcpuinfo->guestCR3 & 0xffffffffffffffe0ULL;
  UINT64 faultingaddress=vmread(0x6400); //on pagefaults the exitqualification contains the address

  int noexecute=(readMSR(0xc0000080) >> 11) & 1; //efer bit 11=noexecute ability
	errorcode.errorcode=vmread(0x4406);
  fakeCR0.CR0=vmread(0x6004);
  fakeCR4.CR4=vmread(0x6006);

  int writable=1;
  //int executable=1;


  if (!memorycloak)
    return 1;

  if (errorcode.RSVD)
    nosendchar[getAPICID()]=0;

  sendstringf("guest cs=%8\n\r",vmread(0x802));
  sendstringf("guest eip=%8\n\r",vmread(0x681e));
  sendstringf("address=%8\n\r",faultingaddress);
  sendstringf("errorcode P=%d\n\r",errorcode.P);
  sendstringf("errorcode W=%d\n\r",errorcode.W);
  sendstringf("errorcode US=%d\n\r",errorcode.US);
  sendstringf("errorcode RSVD=%d\n\r",errorcode.RSVD);
  sendstringf("errorcode ID=%d\n\r",errorcode.ID);




  //get the guest's CR3  (guestCR3 variable, of this cpu)
	//get the vmm's CR3 used for the guests (not hard, it's VirtualMachinePagingspace)
	//returns 0 if handled, 1 if it needs to be handled by guest , 2 if it's a huge bug

  if (IS64BITPAGING(currentcpuinfo))
  {
    sendstring("64-bit paging system\n\r");
    //normal

    //real table is that of the vmm, fake is the one of the virtual machine
    ULONG pml4entry=faultingaddress >> 39 & 0x1ff;
    ULONG pagedirptrentry=faultingaddress >> 30 & 0x1ff;
    ULONG pagedirentry=faultingaddress >> 21 & 0x1ff;
    ULONG pagetableentry=faultingaddress >> 12 & 0x1ff; //when it's not a big sized page
    PPDE_PAE realpml4table=currentcpuinfo->virtualTLB;
    PPDE_PAE realpagedirptrtable=NULL;
    PPDE_PAE realpagedirtable=NULL;
    PPTE_PAE realpagetable=NULL;

    PPDE_PAE fakepml4table=(PPDE_PAE)MapPhysicalMemoryEx(fakepagebase,  currentcpuinfo->AvailableVirtualAddress,1);
    PPDE_PAE fakepagedirptrtable=NULL;
    PPDE_PAE fakepagedirtable=NULL;
    PPTE_PAE fakepagetable=NULL;

    int i;

    sendstringf("pml4entry=%d (%x) pagedirptrentry=%d (%x) pagedirentry=%d (%x) pagetableentry=%d (%x)\n\r",pml4entry, pml4entry*8, pagedirptrentry, pagedirptrentry*8 ,pagedirentry, pagedirentry*8, pagetableentry, pagetableentry*8);

    //----------------------------------------------------------------------//
    //                 check if it's caused by the pml4entry                //
    //----------------------------------------------------------------------//
    sendstringf("realpml4table[pml4entry]=%6\n\r",*(UINT64 *)&realpml4table[pml4entry]);
    sendstringf("fakepml4table[pml4entry]=%6\n\r",*(UINT64 *)&fakepml4table[pml4entry]);


    if (realpml4table[pml4entry].P==0)
    {
      sendstring("realpml4table[pml4entry].P==0\n\r");
      //not present
      if (fakepml4table[pml4entry].P==0)
      {
        //not present, raise exception
        errorcode.P=0;
        vmwrite(0x4406,errorcode.errorcode);
        sendstring("fakepml4table[pml4entry].P==0, raising exception\n\r");
        return 1;
      }

      //still here so it's present
      //map it and retry
      sendstring("guest is present, filling virtual tlb entry\n\r");
      fakepml4table[pml4entry].A=1;


     /* for (i=0; i<512; i++)
      {
        if (fakepml4table[i].P)
          fillTLB_PML4_64(currentcpuinfo, &realpml4table[i], &fakepml4table[i]);
        else
          realpml4table[i].P=0;
      }*/

      fillTLB_PML4_64(currentcpuinfo, &realpml4table[pml4entry], &fakepml4table[pml4entry]);


      sendstring("after:\n\r");
      sendstringf("realpml4table[pml4entry]=%6\n\r",*(UINT64 *)&realpml4table[pml4entry]);
      sendstringf("fakepml4table[pml4entry]=%6\n\r",*(UINT64 *)&fakepml4table[pml4entry]);


      return 0;
    }
    else
    {
      //check consistency
      UINT64 GuestPA=*(UINT64 *)&fakepml4table[pml4entry] & 0x0fffffff000;
      UINT64 RealPA=*(UINT64 *)&realpml4table[pml4entry] & 0x0fffffff000;


    /*
      sendstringf("GuestPA=%6\n\r", GuestPA);
      sendstringf("RealPA =%6\n\r", RealPA);
      sendstringf("currentcpuinfo->virtualTLB_PA=%6\n\r", currentcpuinfo->virtualTLB_PA);
      sendstringf("(RealPA-currentcpuinfo->virtualTLB_PA) / 4096=%6\n\r",(RealPA-currentcpuinfo->virtualTLB_PA) / 4096);
      sendstringf("GuestPA should have been value=%6\n\r",currentcpuinfo->virtualTLB_Lookup[(RealPA-currentcpuinfo->virtualTLB_PA) / 4096]);
      */
      //find the virtual address


      if (currentcpuinfo->virtualTLB_Lookup[(RealPA-currentcpuinfo->virtualTLB_PA) / 4096] != GuestPA)
      {
        nosendchar[getAPICID()]=0;
        sendstring("Inconsistent pml4 entry\n\r");
        realpml4table[pml4entry].P=0;
        return 0;
      }
    }

    //still here so either a access rights or a level down (pagedir ptr)
    //check NX
    if ((errorcode.ID) && (noexecute) && (realpml4table[pml4entry].EXB))
    {
      //not executable
      return 1;   //raise this pagefault
    }

    //check RW
    if ((errorcode.W) && (realpml4table[pml4entry].RW==0))
    {
      //according to 4.11.3, paragraph 2: 3th sentence: "Supervisor pages which are read-only are not writable from any privilege level" regardless of the WP bit
      //for some reason I can write to readonly supervisor mode pages in supervisor mode

      //readonly and it's a write, I did not mess with RW here so guests fault
      if (fakeCR0.WP)
        return 1; //write protect is on, raise anyhow

      //check if in usermode
      if (errorcode.US) //usermode
        return 1;
    }

    //check US
    if ((errorcode.US) && (realpml4table[pml4entry].US==0))
    {
      return 1; //supervisor page access from usermode
    }

    //still here so the active pml4table is valid.

    //check the pagedirptr
    //----------------------------------------------------------------------//
    //                 check if it's caused by the pagedirptr               //
    //----------------------------------------------------------------------//

    //map the fake and real pagedirptrtable
    realpagedirptrtable = (PPDE_PAE)MapPhysicalMemoryEx(*(UINT64*)&realpml4table[pml4entry] & 0x0fffffff000ULL, currentcpuinfo->AvailableVirtualAddress+0x00200000,1);
    fakepagedirptrtable = (PPDE_PAE)MapPhysicalMemoryEx(*(UINT64*)&fakepml4table[pml4entry] & 0x0fffffff000ULL, currentcpuinfo->AvailableVirtualAddress+0x00400000,1);

    sendstringf("realpagedirptrtable[pagedirptrentry]=%6\n\r",*(UINT64 *)&realpagedirptrtable[pagedirptrentry]);
    sendstringf("fakepagedirptrtable[pagedirptrentry]=%6\n\r",*(UINT64 *)&fakepagedirptrtable[pagedirptrentry]);


    if (realpagedirptrtable[pagedirptrentry].P==0)
    {
      sendstring("realpagedirptrtable[pagedirptrentry].P==0\n\r");
      //not present
      if (fakepagedirptrtable[pagedirptrentry].P==0)
      {
        //not present, raise exception
        sendstring("fakepagedirptrtable[pagedirptrentry].P==0, raising pagefault\n\r");
        errorcode.P=0;
        vmwrite(0x4406,errorcode.errorcode);
        return 1;
      }

      //still here so it's present
      //map it and retry
      fakepagedirptrtable[pagedirptrentry].A=1;

/*
      for (i=0; i<512; i++)
      {
        if (fakepagedirptrtable[i].P)
          fillTLB_PDPTR_64(currentcpuinfo, &realpagedirptrtable[i], &fakepagedirptrtable[i]);
        else
          realpagedirptrtable[i].P=0;

      }
                  */
      fillTLB_PDPTR_64(currentcpuinfo, &realpagedirptrtable[pagedirptrentry], &fakepagedirptrtable[pagedirptrentry]);


      sendstring("After:\n\r");
      sendstringf("realpagedirptrtable[pagedirptrentry]=%6\n\r",*(UINT64 *)&realpagedirptrtable[pagedirptrentry]);
      sendstringf("fakepagedirptrtable[pagedirptrentry]=%6\n\r",*(UINT64 *)&fakepagedirptrtable[pagedirptrentry]);

      return 0;
    }
    else
    {
      //check consistency
      UINT64 GuestPA=*(UINT64 *)&fakepagedirptrtable[pagedirptrentry] & 0x0fffffff000ULL;
      UINT64 RealPA=*(UINT64 *)&realpagedirptrtable[pagedirptrentry] & 0x0fffffff000ULL;

      if (currentcpuinfo->virtualTLB_Lookup[(RealPA-currentcpuinfo->virtualTLB_PA) / 4096] != GuestPA)
      {
        nosendchar[getAPICID()]=0;
        sendstring("Inconsistent pagedirptrentry entry\n\r");
        realpagedirptrtable[pagedirptrentry].P=0;
        nosendchar[getAPICID()]=1;
        return 0;
      }
    }


    //still here so either a access rights or a level down (pagedir ptr)
    //check NX
    if ((errorcode.ID) && (noexecute) && (realpagedirptrtable[pagedirptrentry].EXB))
    {
      //not executable
      return 1;   //raise this pagefault
    }

    //check RW
    if ((errorcode.W) && (realpagedirptrtable[pagedirptrentry].RW==0))
    {
      //according to 4.11.3, paragraph 2: 3th sentence: "Supervisor pages which are read-only are not writable from any privilege level" regardless of the WP bit
      //for some reason I can write to readonly supervisor mode pages in supervisor mode

      //readonly and it's a write, I did not mess with RW here so guests fault
      if (fakeCR0.WP)
        return 1; //write protect is on, raise anyhow

      //check if in usermode
      if (errorcode.US) //usermode
        return 1;
    }

    //check US
    if ((errorcode.US) && (realpagedirptrtable[pagedirptrentry].US==0))
    {
      return 1; //supervisor page access from usermode
    }


    //----------------------------------------------------------------------//
    //                 check if it's caused by the pagedir                  //
    //----------------------------------------------------------------------//
    realpagedirtable = (PPDE_PAE)MapPhysicalMemoryEx(*(UINT64*)&realpagedirptrtable[pagedirptrentry] & 0x0fffffff000, currentcpuinfo->AvailableVirtualAddress+0x00600000,1);
    fakepagedirtable = (PPDE_PAE)MapPhysicalMemoryEx(*(UINT64*)&fakepagedirptrtable[pagedirptrentry] & 0x0fffffff000, currentcpuinfo->AvailableVirtualAddress+0x00800000,1);

    sendstringf("realpagedirtable[pagedirentry]=%6\n\r",*(UINT64 *)&realpagedirtable[pagedirentry]);
    sendstringf("fakepagedirtable[pagedirentry]=%6\n\r",*(UINT64 *)&fakepagedirtable[pagedirentry]);


    //check if it's caused by the pagetable entry
    if (realpagedirtable[pagedirentry].P==0)
    {
      //not present
      if (fakepagedirtable[pagedirentry].P==0)
      {
        //not present, raise exception
        errorcode.P=0;
        vmwrite(0x4406,errorcode.errorcode);
        return 1;
      }

      //still here so it's present
      //map it and retry
      fakepagedirtable[pagedirentry].A=1;
      for (i=0; i<512; i++)
      {
        if (fakepagedirtable[i].P)
          fillTLB_PDE_64(currentcpuinfo, &realpagedirtable[i], &fakepagedirtable[i]);
        else
          realpagedirtable[i].P=0;
      }

      sendstring("after\n\r");
      sendstringf("realpagedirtable[pagedirentry]=%6\n\r",*(UINT64 *)&realpagedirtable[pagedirentry]);
      sendstringf("fakepagedirtable[pagedirentry]=%6\n\r",*(UINT64 *)&fakepagedirtable[pagedirentry]);
      return 0;
    }
    else
    {
      //check consistency
      UINT64 GuestPA=*(UINT64 *)&fakepagedirtable[pagedirentry] & 0x0fffffff000;
      UINT64 RealPA=*(UINT64 *)&realpagedirtable[pagedirentry] & 0x0fffffff000;

      if (fakepagedirtable[pagedirentry].PS != realpagedirtable[pagedirentry].PS)
      {
        nosendchar[getAPICID()]=0;
        sendstring("Inconsistent pagedir entry\n\r");
        realpagedirtable[pagedirentry].P=0;
        nosendchar[getAPICID()]=1;
        return 0;
      }

      if ((realpagedirtable[pagedirentry].PS==0) && //pagedir with table, so look up
         (currentcpuinfo->virtualTLB_Lookup[(RealPA-currentcpuinfo->virtualTLB_PA) / 4096] != GuestPA))
      {
        nosendchar[getAPICID()]=0;
        sendstring("Inconsistent pagedir entry\n\r");
        realpagedirtable[pagedirentry].P=0;
        nosendchar[getAPICID()]=1;
        return 0; //retry
      }
    }

    //still here so either a access rights or a level down (pagedir ptr)
    //check NX
    if ((errorcode.ID) && (noexecute) && (realpagedirtable[pagedirentry].EXB))
    {
      //not executable
      return 1;   //raise this pagefault
    }

    //check RW
    if ((errorcode.W) && (realpagedirtable[pagedirentry].RW==0))
    {
      //according to 4.11.3, paragraph 2: 3th sentence: "Supervisor pages which are read-only are not writable from any privilege level" regardless of the WP bit
      //for some reason I can write to readonly supervisor mode pages in supervisor mode

      //readonly and it's a write

      //I did mess with the WP bit here, so:
      if (fakepagedirtable[pagedirentry].RW)
      {
        //should be writable
        fakepagedirtable[pagedirentry].D=1; //set dirty bit
        realpagedirtable[pagedirentry].RW=1;
        return 0; //retry
      }

      if (fakeCR0.WP)
        return 1; //write protect is on, raise anyhow

      //check if in usermode
      if (errorcode.US) //usermode
        return 1;
    }


    //check US
    if ((errorcode.US) && (realpagedirtable[pagedirentry].US==0))
    {
      return 1; //supervisor page access from usermode
    }


    //----------------------------------------------------------------------//
    //                 check if it's caused by the pagetable                //
    //----------------------------------------------------------------------//
    if (realpagedirtable[pagedirentry].PS==0)
    {
      realpagetable = (PPTE_PAE)MapPhysicalMemoryEx(*(UINT64*)&realpagedirtable[pagedirentry] & 0x0fffffff000, currentcpuinfo->AvailableVirtualAddress+0x00a00000,1);
      fakepagetable = (PPTE_PAE)MapPhysicalMemoryEx(*(UINT64*)&fakepagedirtable[pagedirentry] & 0x0fffffff000, currentcpuinfo->AvailableVirtualAddress+0x00c00000,1);

      sendstringf("realpagetable[pagetableentry]=%6\n\r",*(UINT64 *)&realpagetable[pagetableentry]);
      sendstringf("fakepagetable[pagetableentry]=%6\n\r",*(UINT64 *)&fakepagetable[pagetableentry]);


      //check if it's caused by the pagetable entry
      if (realpagetable[pagetableentry].P==0)
      {
        //not present
        if (fakepagetable[pagetableentry].P==0)
        {
          //not present, raise exception
          errorcode.P=0;
          vmwrite(0x4406,errorcode.errorcode);
          return 1;
        }

        //still here so it's present
        //map it and retry
        fakepagetable[pagetableentry].A=1;
        for (i=0; i<512; i++)
        {
          if (fakepagetable[i].P)
            fillTLB_PTE_64(&realpagetable[i], &fakepagetable[i]);
          else
            realpagetable[i].P=0;
        }

        //fillTLB_PTE_64(&realpagetable[pagetableentry], &fakepagetable[pagetableentry], errorcode.US, fakeCR0.WP );


        sendstring("After:\n\r");
        sendstringf("realpagetable[pagetableentry]=%6\n\r",*(UINT64 *)&realpagetable[pagetableentry]);
        sendstringf("fakepagetable[pagetableentry]=%6\n\r",*(UINT64 *)&fakepagetable[pagetableentry]);
        return 0;
      }
      else
      {
        //check consistency
        if (realpagetable[pagetableentry].PFN != fakepagetable[pagetableentry].PFN)
        {
          nosendchar[getAPICID()]=0;
          sendstring("Inconsistent pagetable entry\n\r");
          realpagetable[pagetableentry].P=0;
          nosendchar[getAPICID()]=1;
          return 0;
        }
      }

      //still here so either a access rights or a level down (page ptr)
      //check NX
      if ((errorcode.ID) && (noexecute) && (realpagetable[pagetableentry].EXB))
      {
        //not executable
        return 1;   //raise this pagefault
      }

      //check RW
      if ((errorcode.W) && (realpagetable[pagetableentry].RW==0))
      {
        //according to 4.11.3, paragraph 2: 3th sentence: "Supervisor pages which are read-only are not writable from any privilege level" regardless of the WP bit
        //for some reason I can write to readonly supervisor mode pages in supervisor mode

        //readonly and it's a write

        //I did mess with the WP bit here, so:
        if (fakepagetable[pagetableentry].RW)
        {
          //should be writable
          fakepagetable[pagetableentry].D=1; //set dirty bit
          realpagetable[pagetableentry].RW=1; //make writable
          return 0; //retry
        }

        if (fakeCR0.WP)
          return 1; //write protect is on, raise anyhow regardless of rinlevel

        //check if in usermode
        if (errorcode.US) //usermode
          return 1; //pagefault
      }

      //check US
      if ((errorcode.US) && (realpagetable[pagetableentry].US==0))
      {
        return 1; //supervisor page access from usermode
      }
    }

    nosendchar[getAPICID()]=0;

    if (errorcode.RSVD)
    {
      sendstring("RSVD bit = 1\n\r");
      return 1;
    }

    sendstring("No idea why this caused an exception...\n\r");
    return 0; //try again, perhaps a TLB error

  }
  else
  if (fakeCR4.PAE)
  {
    sendstring("PAE handling\n\r");
    sendstringf("currentcpuinfo->cpunr=%d\n\r", currentcpuinfo->cpunr);
    sendstringf("currentcpuinfo->virtualTLB=%8\n\r", currentcpuinfo->virtualTLB);
    sendstringf("currentcpuinfo->guestCR3=%8 fakepagebase=%8\n\r", currentcpuinfo->guestCR3, fakepagebase);
    sendstringf("currentcpuinfo->AvailableVirtualAddress=%8\n\r", currentcpuinfo->AvailableVirtualAddress);


    unsigned int pagedirptrentry=faultingaddress >> 30;
		unsigned int pagedirentry=faultingaddress >> 21 & 0x1ff;
		unsigned int pagetableentry=faultingaddress >> 12 & 0x1ff; //when it's not a big sized page

    PPDPTE_PAE realpagedirptrtable=currentcpuinfo->virtualTLB;
    PPDPTE_PAE fakepagedirptrtable=(PPDPTE_PAE)MapPhysicalMemoryEx(fakepagebase, currentcpuinfo->AvailableVirtualAddress,1);


		PPDE_PAE 	 realpagedirtable=NULL;
		PPDE_PAE   fakepagedirtable=NULL;

    PPTE_PAE   realpagetable=NULL;
    PPTE_PAE   fakepagetable=NULL;

//idea:
//if realpagedirptrtable is present then use virtualTLB_guest_PDPTR_lookup for the guest
//if realpagedirtable is present then use virtualTLB_guest_PD_lookup for the guest

    if (realpagedirptrtable[pagedirptrentry].P)
      realpagedirtable=(PPDE_PAE)MapPhysicalMemoryEx(realpagedirptrtable[pagedirptrentry].PFN << 12, currentcpuinfo->AvailableVirtualAddress+0x00200000,1);

    if (fakepagedirptrtable[pagedirptrentry].P)
      fakepagedirtable=(PPDE_PAE)MapPhysicalMemoryEx(fakepagedirptrtable[pagedirptrentry].PFN << 12,currentcpuinfo->AvailableVirtualAddress+0x00400000,1);

    if (realpagedirtable && (realpagedirtable[pagedirentry].P) && (realpagedirtable[pagedirentry].PS==0))
      realpagetable=(PPTE_PAE)MapPhysicalMemoryEx(realpagedirtable[pagedirentry].PFN << 12, currentcpuinfo->AvailableVirtualAddress+0x00600000,1);

    if (fakepagedirtable && (fakepagedirtable[pagedirentry].P) && (fakepagedirtable[pagedirentry].PS==0))
      fakepagetable=(PPTE_PAE)MapPhysicalMemoryEx(fakepagedirtable[pagedirentry].PFN << 12, currentcpuinfo->AvailableVirtualAddress+0x00800000,1);

    if ((fakepagedirtable[pagedirentry].PS==1) && (fakeCR4.PSE==0))
    {
      nosendchar[getAPICID()]=0;
      sendstringf("PS=%d and PSE=%d\n\r",fakepagedirtable[pagedirentry].PS, fakeCR4.PSE);
      nosendchar[getAPICID()]=1;
    }


    sendstringf("PAE paging handling (fakepagepage=%8):\n\r",(UINT64)fakepagebase);
    sendstringf("real: pagedirptrtable=%8 pagedirtable=%8 pagetable=%8\n\r",(UINT64)VirtualToPhysical((UINT64)realpagedirptrtable), (UINT64)VirtualToPhysical((UINT64)realpagedirtable), (UINT64)VirtualToPhysical((UINT64)realpagetable));
    sendstringf("fake: pagedirptrtable=%8 pagedirtable=%8 pagetable=%8\n\r",(UINT64)VirtualToPhysical((UINT64)fakepagedirptrtable), (UINT64)VirtualToPhysical((UINT64)fakepagedirtable), (UINT64)VirtualToPhysical((UINT64)fakepagetable));

    sendstringf("pagedirptrentry=%d (%x) pagedirentry=%d (%x) pagetableentry=%d (%x)\n\r",pagedirptrentry, pagedirptrentry*8 ,pagedirentry, pagedirentry*8, pagetableentry, pagetableentry*8);

    sendstringf("before:\n\r");
    sendstringf("fakepagedirptrtable[pagedirptrentry]=%6\n\r",*(unsigned long long *)(&fakepagedirptrtable[pagedirptrentry]));
    if (fakepagedirtable)
    {
      sendstringf("fakepagedirtable[pagedirentry]=      %6\n\r",*(unsigned long long *)(&fakepagedirtable[pagedirentry]));
    }
    if (fakepagetable)
    {
      sendstringf("fakepagetable[pagetableentry]=       %6\n\r",*(unsigned long long *)(&fakepagetable[pagetableentry]));
    }

    sendstringf("realpagedirptrtable[pagedirptrentry]=%6\n\r",*(unsigned long long *)(&realpagedirptrtable[pagedirptrentry]));
    if (realpagedirtable)
    {
      sendstringf("realpagedirtable[pagedirentry]=      %6\n\r",*(unsigned long long *)(&realpagedirtable[pagedirentry]));
    }
    if (realpagetable)
    {
      sendstringf("realpagetable[pagetableentry]=       %6\n\r",*(unsigned long long *)(&realpagetable[pagetableentry]));
    }


    sendstring("Checking PDPTR\n\r");



    if (realpagedirptrtable[pagedirptrentry].P)
    {
      sendstring("realpagedirptrtable entry is present, checking consistency\n\r");

      sendstringf("virtualTLB_guest_PDPTR_lookup[pagedirptrentry]=%6\n\r",*(unsigned long long *)&(currentcpuinfo->virtualTLB_guest_PDPTR_lookup[pagedirptrentry*8]));
      sendstringf("fakepagedirptrtable[pagedirptrentry]=          %6\n\r",*(unsigned long long *)&fakepagedirptrtable[pagedirptrentry]);

      //check consistency between old and new
      if (
         (fakepagedirptrtable[pagedirptrentry].P==0) ||

         ((*(unsigned long long *)&(currentcpuinfo->virtualTLB_guest_PDPTR_lookup[pagedirptrentry*8])) != (*(unsigned long long* )&fakepagedirptrtable[pagedirptrentry]))
      )
      {
        //it changed, retry from start
        int xxx=0;

        if ((*(unsigned long long *)&(currentcpuinfo->virtualTLB_guest_PDPTR_lookup[pagedirptrentry*8])) != (*(unsigned long long* )&fakepagedirptrtable[pagedirptrentry]))
          xxx=1;


        nosendchar[getAPICID()]=0;
        sendstringf("pagedirptr: Inconsistency detected (%8)  (xxx=%d)\n\r",faultingaddress,xxx);
        nosendchar[getAPICID()]=1;
        realpagedirptrtable[pagedirptrentry].P=0;
        return 0;
      }
    }


    if (errorcode.P==0)
    {
      sendstring("Exception due to present flag, check present flag\n\r");

      //1: consult if the active PDPTR is the cause
      if (fakepagedirptrtable[pagedirptrentry].P==0)
      {
        sendstring("Fake pagedirtable is not present\n\r");
        if (realpagedirptrtable[pagedirptrentry].P==0)
          return 1; //not present according to the guest, raise exception
        else
        {
          realpagedirptrtable[pagedirptrentry].P=0;
          return 0; //adjust to normal state, next go will cause a (normal) exception
        }
      }

      sendstring("Fake fakepagedirptrtable entry is present\n\r");
      if (realpagedirptrtable[pagedirptrentry].P==0)
      {

        sendstring("realpagedirtable entry is not present\n\r");

        //4: if active pde is not present then set active pde to guest pde (since the guest check inclused present checks, this HAS top be caused by the A bit reason)
        //fill in guest entry

//        for (i=0; i<4; i++)
//          if (fakepagedirptrtable[i].P)
//            fillTLB_PDPTE_PAE(currentcpuinfo, &realpagedirptrtable[i], &fakepagedirptrtable[i]);

        //if (pagedirptrentry>=4)
        fillTLB_PDPTE_PAE(currentcpuinfo, &realpagedirptrtable[pagedirptrentry], &fakepagedirptrtable[pagedirptrentry]);
        *(unsigned long long *)&(currentcpuinfo->virtualTLB_guest_PDPTR_lookup[pagedirptrentry*8])=*(unsigned long long* )&fakepagedirptrtable[pagedirptrentry];


        sendstringf("after:\n\r");
        sendstringf("fakepagedirptrtable[pagedirptrentry]=%6\n\r",*(unsigned long long *)(&fakepagedirptrtable[pagedirptrentry]));
        if (fakepagedirtable)
        {
          sendstringf("fakepagedirtable[pagedirentry]=      %6\n\r",*(unsigned long long *)(&fakepagedirtable[pagedirentry]));
        }
        if (fakepagetable)
        {
          sendstringf("fakepagetable[pagetableentry]=       %6\n\r",*(unsigned long long *)(&fakepagetable[pagetableentry]));
        }

        sendstringf("realpagedirptrtable[pagedirptrentry]=%6\n\r",*(unsigned long long *)(&realpagedirptrtable[pagedirptrentry]));
        if (realpagedirtable)
        {
          sendstringf("realpagedirtable[pagedirentry]=      %6\n\r",*(unsigned long long *)(&realpagedirtable[pagedirentry]));
        }
        if (realpagetable)
        {
          sendstringf("realpagetable[pagetableentry]=       %6\n\r",*(unsigned long long *)(&realpagetable[pagetableentry]));
        }

        return 0; //restart instruction
      }




    }
    else
    {
      sendstring("Exception caused by access violation, the pdptr doesn't care about it...\n\r");
    }

    sendstring("PDPTR looks fine\n\r");

    //-------------------------------------------------------------------
    //        still here, so not caused by the pdptr, check pde
    //-------------------------------------------------------------------
    sendstring("Checking pde\n\r");

    if (realpagedirtable[pagedirentry].P)
    {
      sendstring("realpagedirtable entry is present, checking consistency\n\r");

      sendstringf("virtualTLB_guest_PD_lookup[pagedirentry]=%6\n\r",*(unsigned long long *)&(currentcpuinfo->virtualTLB_guest_PD_lookup[pagedirptrentry*4096+pagedirentry*8]));
      sendstringf("fakepagedirtable[pagedirentry]=          %6\n\r",*(unsigned long long *)&fakepagedirtable[pagedirentry]);



      //check consistency between old and new
      if (
          (fakepagedirtable[pagedirentry].P==0) ||
          (realpagedirtable[pagedirentry].PS!=fakepagedirtable[pagedirentry].PS) ||
          ((realpagedirtable[pagedirentry].PS==1) && (realpagedirtable[pagedirentry].PFN!=fakepagedirtable[pagedirentry].PFN)) || //bigpage and PFN changed
          (realpagedirtable[pagedirentry].US!=fakepagedirtable[pagedirentry].US) ||
          ((*(unsigned long long *)&(currentcpuinfo->virtualTLB_guest_PD_lookup[pagedirptrentry*4096+pagedirentry*8])) != (*(unsigned long long* )&fakepagedirtable[pagedirentry]))

         )
      {
        int xxx=0;
        if ((*(unsigned long long *)&(currentcpuinfo->virtualTLB_guest_PD_lookup[pagedirptrentry*4096+pagedirentry*8])) != (*(unsigned long long* )&fakepagedirtable[pagedirentry]))
          xxx=1;
        //it changed, retry from start
        nosendchar[getAPICID()]=0;
        sendstringf("pagedir: Inconsistency detected (%8 - xxx=%d)\n\r",faultingaddress,xxx);
        nosendchar[getAPICID()]=1;
        realpagedirtable[pagedirentry].P=0;
        return 0;
      }
    }



    if (errorcode.P==0)
    {
      sendstring("Exception due to present flag, check present flag\n\r");

      //1: consult if the active PDE is the cause
      if (fakepagedirtable[pagedirentry].P==0)
      {
        sendstring("Fake pagedirtable is not present\n\r");
        if (realpagedirtable[pagedirentry].P==0)
          return 1; //not present according to the guest, raise exception
        else
        {
          realpagedirtable[pagedirentry].P=0;
          return 0; //adjust to normal state, next go will cause a (normal) exception
        }
      }

      sendstring("Fake pagedirtable is present\n\r");




      if (realpagedirtable[pagedirentry].P==0)
      {

        sendstring("realpagedirtable entry is not present\n\r");

        //4: if active pde is not present then set active pde to guest pde (since the guest check inclused present checks, this HAS top be caused by the A bit reason)
        //fill in guest entry
        fakepagedirtable[pagedirentry].A=1;

        /*
        for (i=0; i<512; i++)
          if (fakepagedirtable[i].P && fakepagedirtable[i].A)
            fillTLB_PDE_PAE(currentcpuinfo, &realpagedirtable[i], &fakepagedirtable[i], errorcode.US, fakeCR0.WP );
        */

        fillTLB_PDE_PAE(currentcpuinfo, &realpagedirtable[pagedirentry], &fakepagedirtable[pagedirentry] );
        *(unsigned long long *)&(currentcpuinfo->virtualTLB_guest_PD_lookup[pagedirptrentry*4096+pagedirentry*8])=*(unsigned long long* )&fakepagedirtable[pagedirentry];


        sendstringf("after:\n\r");
        sendstringf("fakepagedirptrtable[pagedirptrentry]=%6\n\r",*(unsigned long long *)(&fakepagedirptrtable[pagedirptrentry]));
        if (fakepagedirtable)
        {
          sendstringf("fakepagedirtable[pagedirentry]=      %6\n\r",*(unsigned long long *)(&fakepagedirtable[pagedirentry]));
        }
        if (fakepagetable)
        {
          sendstringf("fakepagetable[pagetableentry]=       %6\n\r",*(unsigned long long *)(&fakepagetable[pagetableentry]));
        }

        sendstringf("realpagedirptrtable[pagedirptrentry]=%6\n\r",*(unsigned long long *)(&realpagedirptrtable[pagedirptrentry]));
        if (realpagedirtable)
        {
          sendstringf("realpagedirtable[pagedirentry]=      %6\n\r",*(unsigned long long *)(&realpagedirtable[pagedirentry]));
        }
        if (realpagetable)
        {
          sendstringf("realpagetable[pagetableentry]=       %6\n\r",*(unsigned long long *)(&realpagetable[pagetableentry]));
        }

        return 0; //restart instruction
      }

    }
    else
    {
      sendstring("Exception caused by access violation, check writable bit\n\r");
      //not caused by present flag of the pde


      if (fakepagedirtable[pagedirentry].RW==0)
        writable=((errorcode.US==0) && (fakeCR0.WP==0)); //supervisor and wp=0? Then writable is still 1

      if (errorcode.W)
      {
        sendstring("It is a write operation, checking pde write bit for a 1\n\r");
        //not caused by pagedir present flag, check write flag, since it's a write
        if (!writable)
          return 1; //not writable, raise exception

        if (realpagedirtable[pagedirentry].RW==0)
        {
          sendstring("Writable, but not according to the active pde entry, make writable and set dirty\n\r");

          //write operation, but according to the active pagetable it's not, so make writable and set dirty
          if ((errorcode.US==0) && (fakeCR0.WP==0))
            realpagedirtable[pagedirentry].RW=1;
          else
            realpagedirtable[pagedirentry].RW=fakepagedirtable[pagedirentry].RW;

          if (realpagedirtable[pagedirentry].PS==1)
          {
            if (fakepagedirtable[pagedirentry].D==0)
            {
              nosendchar[getAPICID()]=0;
              sendstringf("Setting dirty bit on pagedir entry, this MUST cause a inconsistency\n\r");
            }

            fakepagedirtable[pagedirentry].D=1;
          }

          sendstringf("after:\n\r");
          sendstringf("fakepagedirptrtable[pagedirptrentry]=%6\n\r",*(unsigned long long *)(&fakepagedirptrtable[pagedirptrentry]));
          if (fakepagedirtable)
          {
            sendstringf("fakepagedirtable[pagedirentry]=      %6\n\r",*(unsigned long long *)(&fakepagedirtable[pagedirentry]));
          }
          if (fakepagetable)
          {
            sendstringf("fakepagetable[pagetableentry]=       %6\n\r",*(unsigned long long *)(&fakepagetable[pagetableentry]));
          }

          sendstringf("realpagedirptrtable[pagedirptrentry]=%6\n\r",*(unsigned long long *)(&realpagedirptrtable[pagedirptrentry]));
          if (realpagedirtable)
          {
            sendstringf("realpagedirtable[pagedirentry]=      %6\n\r",*(unsigned long long *)(&realpagedirtable[pagedirentry]));
          }
          if (realpagetable)
          {
            sendstringf("realpagetable[pagetableentry]=       %6\n\r",*(unsigned long long *)(&realpagetable[pagetableentry]));
          }

          return 0; //restart
        }
      }

      //fault due to access violation
      if ((errorcode.US==1) && (fakepagedirtable[pagedirentry].US==0))
      {
        sendstring("kernelmemory access from usermode\n\r");
        return 1; //kernelmemory access from usermode, raise exception
      }

    }

    sendstring("PDE looks fine\n\r");
    //still here, so not a problem with the guest's pde
/*
    if (((fakepagedirtable[pagedirentry].PFN << 12)>=vmmstart) && (fakepagedirtable[pagedirentry].PFN << 12)<(vmmstart+0x00400000))
    {
      nosendchar[getAPICID()]=0;
      sendstring("pagedir: VMM memory accessed\n\r");
      return 0;
    }
    */


    if (fakepagedirtable[pagedirentry].PS)
    {
      sendstring("This is a big page, and has no PTE, therefore, the fault was made by the guest\n\r");

      if (errorcode.P==0) //caused by a page not present fault, but it has been proven the pages exist
        return 0;

      return 1; //raise exception

    }

    //-------------------------------------------------------------------
    //        still here, so not caused by the pde, check pte
    //-------------------------------------------------------------------
    sendstring("--------------\n\rChecking pte\n\r");



    if (realpagetable[pagetableentry].P)
    {
      sendstring("realpagetable entry is present\n\r");
      sendstring("Checking consitency\n\r");
      if (
          (realpagetable[pagetableentry].reserved!=fakepagetable[pagetableentry].reserved) ||
          (realpagetable[pagetableentry].PFN!=fakepagetable[pagetableentry].PFN) ||
          (realpagetable[pagetableentry].US!=fakepagetable[pagetableentry].US)
         )
      {
        //it changed, retry from start
        nosendchar[getAPICID()]=0;
        sendstringf("pagetable: Inconsistency detected (%8)\n\r",faultingaddress);
        nosendchar[getAPICID()]=1;

        realpagetable[pagetableentry].P=0;
        return 0;
      }
    }



    if (errorcode.P==0)
    {
      sendstring("Exception due to present flag, check present flag\n\r");


      //8: consult if the active PTE is the cause
      if (fakepagetable[pagetableentry].P==0)
      {
        sendstring("Fakepagetable is not present\n\r");
        if (realpagetable[pagetableentry].P==0)
          return 1; //not present according to the guest, raise exception
        else
        {
          realpagetable[pagetableentry].P=0;
          return 0; //adjust to normal state, next go will cause a (normal) exception
        }
      }

      if (realpagetable[pagetableentry].P==0)
      {
        //int i;

        sendstring("Real pagetable entry is not present(As expected since it wasn't the pde\n\r)\n\r");
        //4: if active pte is not present then set active pte to guest pte (since the guest check inclused present checks, this HAS top be caused by the A bit reason)
        //fill in guest entry
        fakepagetable[pagetableentry].A=1;

        /*
        for (i=0; i<512; i++)
        {
          if (fakepagetable[i].P && fakepagetable[i].A)
            fillTLB_PTE_PAE(&realpagetable[i],&fakepagetable[i], errorcode.US, fakeCR0.WP );
        }*/

        fillTLB_PTE_PAE(&realpagetable[pagetableentry],&fakepagetable[pagetableentry] );

        sendstringf("after:\n\r");
        sendstringf("fakepagedirptrtable[pagedirptrentry]=%6\n\r",*(unsigned long long *)(&fakepagedirptrtable[pagedirptrentry]));
        if (fakepagedirtable)
        {
          sendstringf("fakepagedirtable[pagedirentry]=      %6\n\r",*(unsigned long long *)(&fakepagedirtable[pagedirentry]));
        }
        if (fakepagetable)
        {
          sendstringf("fakepagetable[pagetableentry]=       %6\n\r",*(unsigned long long *)(&fakepagetable[pagetableentry]));
        }

        sendstringf("realpagedirptrtable[pagedirptrentry]=%6\n\r",*(unsigned long long *)(&realpagedirptrtable[pagedirptrentry]));
        if (realpagedirtable)
        {
          sendstringf("realpagedirtable[pagedirentry]=      %6\n\r",*(unsigned long long *)(&realpagedirtable[pagedirentry]));
        }
        if (realpagetable)
        {
          sendstringf("realpagetable[pagetableentry]=       %6\n\r",*(unsigned long long *)(&realpagetable[pagetableentry]));
        }

        return 0; //restart instruction
      }
      else
      {
        sendstring("realpagetable[pagetableentry].P==1!!!\n\r");

      }
    }
    else
    {
      sendstring("Exception caused by access violation, check writable bit\n\r");
      if (fakepagetable[pagetableentry].RW==0)
        writable=((errorcode.US==0) && (fakeCR0.WP==0));

      //not caused by the present flag, check writable bit

      if (errorcode.W)
      {
        //not caused by pagetable present flag, check write flag, since it's a write
        if (!writable)
          return 1; //not writable, raise exception

        if (realpagetable[pagetableentry].RW==0)
        {
          sendstring("Write operation, writable=1 and WR==0, setting WR to 1\n\r");
          //write operation, but according to the active pagetable it's not, so make writable and set dirty
          if ((errorcode.US==0) && (fakeCR0.WP==0))
            realpagetable[pagetableentry].RW=1;
          else
            realpagetable[pagetableentry].RW=fakepagetable[pagetableentry].RW;

          fakepagetable[pagetableentry].D=1;

          sendstringf("after:\n\r");
          sendstringf("fakepagedirptrtable[pagedirptrentry]=%6\n\r",*(unsigned long long *)(&fakepagedirptrtable[pagedirptrentry]));
          if (fakepagedirtable)
          {
            sendstringf("fakepagedirtable[pagedirentry]=      %6\n\r",*(unsigned long long *)(&fakepagedirtable[pagedirentry]));
          }
          if (fakepagetable)
          {
            sendstringf("fakepagetable[pagetableentry]=       %6\n\r",*(unsigned long long *)(&fakepagetable[pagetableentry]));
          }

          sendstringf("realpagedirptrtable[pagedirentry]=%6\n\r",*(unsigned long long *)(&realpagedirptrtable[pagedirptrentry]));
          if (realpagedirtable)
          {
            sendstringf("realpagedirtable[pagedirentry]=      %6\n\r",*(unsigned long long *)(&realpagedirtable[pagedirentry]));
          }
          if (realpagetable)
          {
            sendstringf("realpagetable[pagetableentry]=       %6\n\r",*(unsigned long long *)(&realpagetable[pagetableentry]));
          }


          return 0; //restart
        }
      }
    }

    //still here, so not a problem with the guest's pte
    /*if (((fakepagetable[pagetableentry].PFN << 12)>=vmmstart) && (fakepagetable[pagetableentry].PFN << 12)<(vmmstart+0x00400000))
    {
      nosendchar[getAPICID()]=0;
      sendstring("pagetable: VMM memory accessed\n\r");
      return 0;
    }*/

    //still here, so not caused by vmm

    if (errorcode.P==0)
    {
      nosendchar[getAPICID()]=0;
      sendstring("Pagefault with present reason, while it has been proven that it is present, try again\n\r");
      return 0; //try again, it has been proven that P=1
    }

    nosendchar[getAPICID()]=0;

    if (errorcode.RSVD==0)
    {
      nosendchar[getAPICID()]=0;
      sendstring("Page fault due to reserved bits set...\n\r");

    }


    sendstring("Still here, so exception probably caused by guest\n\r");

  sendstringf("guest cs=%8\n\r",vmread(0x802));
  sendstringf("guest eip=%8\n\r",vmread(0x681e));
  sendstringf("address=%8\n\r",faultingaddress);
  sendstringf("errorcode P=%d\n\r",errorcode.P);
  sendstringf("errorcode W=%d\n\r",errorcode.W);
  sendstringf("errorcode US=%d\n\r",errorcode.US);
  sendstringf("errorcode RSVD=%d\n\r",errorcode.RSVD);
  sendstringf("errorcode ID=%d\n\r",errorcode.ID);
          sendstringf("fakepagedirptrtable[pagedirptrentry]=%6\n\r",*(unsigned long long *)(&fakepagedirptrtable[pagedirptrentry]));
          if (fakepagedirtable)
          {
            sendstringf("fakepagedirtable[pagedirentry]=      %6\n\r",*(unsigned long long *)(&fakepagedirtable[pagedirentry]));
          }
          if (fakepagetable)
          {
            sendstringf("fakepagetable[pagetableentry]=       %6\n\r",*(unsigned long long *)(&fakepagetable[pagetableentry]));
          }

          sendstringf("realpagedirptrtable[pagedirentry]=%6\n\r",*(unsigned long long *)(&realpagedirptrtable[pagedirptrentry]));
          if (realpagedirtable)
          {
            sendstringf("realpagedirtable[pagedirentry]=      %6\n\r",*(unsigned long long *)(&realpagedirtable[pagedirentry]));
          }
          if (realpagetable)
          {
            sendstringf("realpagetable[pagetableentry]=       %6\n\r",*(unsigned long long *)(&realpagetable[pagetableentry]));
          }



    return 1; //raise exception


  }
  else
  {
    //normal
    sendstring("Non-PAE handling\n\r");

    //real table is that of the vmm, fake is the one of the virtual machine

    ULONG pagedirentry=faultingaddress >> 22;
    ULONG pagetableentry=faultingaddress >> 12 & 0x3ff; //when it's not a big sized page
    PPDE  realpagedirtable=currentcpuinfo->virtualTLB;
    PPDE  fakepagedirtable=(PPDE)MapPhysicalMemoryEx(fakepagebase, currentcpuinfo->AvailableVirtualAddress,1);
    PPTE  realpagetable=NULL; //(PPTE)MapPhysicalMemory(realpagedirtable[pagedirentry].PFN << 12,4096,0x10200000);
    PPTE  fakepagetable=NULL; //(PPTE)MapPhysicalMemory(fakepagedirtable[pagedirentry].PFN << 12,4096,0x10400000);




    if (fakepagedirtable[pagedirentry].PS==0 || realpagedirtable[pagedirentry].PS==0)
    {
      realpagetable=(PPTE)MapPhysicalMemoryEx(realpagedirtable[pagedirentry].PFN << 12, currentcpuinfo->AvailableVirtualAddress+0x00200000,1);
      fakepagetable=(PPTE)MapPhysicalMemoryEx(fakepagedirtable[pagedirentry].PFN << 12, currentcpuinfo->AvailableVirtualAddress+0x00400000,1);
    }



    sendstringf("Normal pagingmode handling (fakepagepage=%8):\n\r",(UINT64)fakepagebase);
    sendstringf("realpagedirtable=%8 realpagetable=%8\n\r",(UINT64)VirtualToPhysical((UINT64)realpagedirtable),(UINT64)VirtualToPhysical((UINT64)realpagetable));
    sendstringf("fakepagedirtable=%8 fakepagetable=%8\n\r",(UINT64)VirtualToPhysical((UINT64)fakepagedirtable),(UINT64)VirtualToPhysical((UINT64)fakepagetable));
    sendstringf("pagedirentry=%d pagetableentry=%d\n\r",pagedirentry,pagetableentry);

    sendstringf("before:\n\r");
    sendstringf("fakepagedirtable[pagedirentry]=%8\n\r",*(ULONG*)(&fakepagedirtable[pagedirentry]));
    if (fakepagedirtable[pagedirentry].PS==0)
    {
      sendstringf("fakepagetable[pagetableentry]=%8\n\r",*(ULONG*)(&fakepagetable[pagetableentry]));
    }

    sendstringf("realpagedirtable[pagedirentry]=%8\n\r",*(ULONG*)(&realpagedirtable[pagedirentry]));
    if (realpagedirtable[pagedirentry].PS==0)
    {
      sendstringf("realpagetable[pagetableentry]=%8\n\r",*(ULONG*)(&realpagetable[pagetableentry]));
    }


    sendstring("Checking pde\n\r");

    if (realpagedirtable[pagedirentry].P)
    {
      sendstring("realpagedirtable entry is present, checking consistency\n\r");
      //check consistency between old and new
      if (
          (fakepagedirtable[pagedirentry].P==0) ||
          (realpagedirtable[pagedirentry].PS!=fakepagedirtable[pagedirentry].PS) ||
          ((realpagedirtable[pagedirentry].PS==1) && (realpagedirtable[pagedirentry].PFN!=fakepagedirtable[pagedirentry].PFN)) || //bigpage and PFN changed
          (realpagedirtable[pagedirentry].US!=fakepagedirtable[pagedirentry].US)
         )
      {
        //it changed, retry from start
        nosendchar[getAPICID()]=0;
        sendstringf("pagedir: Inconsistency detected (%8)\n\r",faultingaddress);
        nosendchar[getAPICID()]=1;
        realpagedirtable[pagedirentry].P=0;
        return 0;
      }
    }



    if (errorcode.P==0)
    {
      sendstring("Exception due to present flag, check present flag\n\r");

      //1: consult if the active PDE is the cause
      if (fakepagedirtable[pagedirentry].P==0)
      {
        sendstring("Fake pagedirtable is not present\n\r");
        if (realpagedirtable[pagedirentry].P==0)
          return 1; //not present according to the guest, raise exception
        else
        {
          realpagedirtable[pagedirentry].P=0;
          return 0; //adjust to normal state, next go will cause a (normal) exception
        }
      }

      sendstring("Fake pagedirtable is present\n\r");




      if (realpagedirtable[pagedirentry].P==0)
      {
        int i;
        sendstring("realpagedirtable entry is not present\n\r");

        //4: if active pde is not present then set active pde to guest pde (since the guest check inclused present checks, this HAS top be caused by the A bit reason)
        //fill in guest entry
        fakepagedirtable[pagedirentry].A=1;
        for (i=0; i<1024; i++)
          if (fakepagedirtable[i].P && fakepagedirtable[i].A)
            fillTLB_PDE(currentcpuinfo, &realpagedirtable[i], &fakepagedirtable[i] );


        sendstringf("after:\n\r");
        sendstringf("fakepagedirtable[pagedirentry]=%8\n\r",*(ULONG*)(&fakepagedirtable[pagedirentry]));
        if (fakepagedirtable[pagedirentry].PS==0)
        {
          sendstringf("fakepagetable[pagetableentry]=%8\n\r",*(ULONG*)(&fakepagetable[pagetableentry]));
        }

        sendstringf("realpagedirtable[pagedirentry]=%8\n\r",*(ULONG*)(&realpagedirtable[pagedirentry]));
        if (realpagedirtable[pagedirentry].PS==0)
        {
          sendstringf("realpagetable[pagetableentry]=%8\n\r",*(ULONG*)(&realpagetable[pagetableentry]));
        }

        return 0; //restart instruction
      }

    }
    else
    {
      sendstring("Exception caused by access violation, check writable bit\n\r");
      //not caused by present flag of the pde


      if (fakepagedirtable[pagedirentry].RW==0)
        writable=((errorcode.US==0) && (fakeCR0.WP==0));

      if (errorcode.W)
      {
        sendstring("It is a write operation, checking pde write bit for a 1\n\r");
        //not caused by pagedir present flag, check write flag, since it's a write
        if (!writable)
          return 1; //not writable, raise exception

        if (realpagedirtable[pagedirentry].RW==0)
        {
          sendstring("Writable, but not according to the active pde entry, make writable and set dirty\n\r");

          //write operation, but according to the active pagetable it's not, so make writable and set dirty
          if ((errorcode.US==0) && (fakeCR0.WP==0))
            realpagedirtable[pagedirentry].RW=1;
          else
            realpagedirtable[pagedirentry].RW=fakepagedirtable[pagedirentry].RW;

          if (realpagedirtable[pagedirentry].PS==1)
            fakepagedirtable[pagedirentry].D=1;

          sendstringf("after:\n\r");
          sendstringf("fakepagedirtable[pagedirentry]=%8\n\r",*(ULONG*)(&fakepagedirtable[pagedirentry]));
          if (fakepagedirtable[pagedirentry].PS==0)
          {
            sendstringf("fakepagetable[pagetableentry]=%8\n\r",*(ULONG*)(&fakepagetable[pagetableentry]));
          }

          sendstringf("realpagedirtable[pagedirentry]=%8\n\r",*(ULONG*)(&realpagedirtable[pagedirentry]));
          if (realpagedirtable[pagedirentry].PS==0)
          {
            sendstringf("realpagetable[pagetableentry]=%8\n\r",*(ULONG*)(&realpagetable[pagetableentry]));
          }

          return 0; //restart
        }
      }

      //fault due to access violation
      if ((errorcode.US==1) && (fakepagedirtable[pagedirentry].US==0))
      {
        sendstring("kernelmemory access from usermode\n\r");
        return 1; //kernelmemory access from usermode, raise exception
      }

    }

    sendstring("PDE looks fine\n\r");
    //still here, so not a problem with the guest's pde
/*
    if (((fakepagedirtable[pagedirentry].PFN << 12)>=vmmstart) && (fakepagedirtable[pagedirentry].PFN << 12)<(vmmstart+0x00400000))
    {
      nosendchar[getAPICID()]=0;
      sendstring("pagedir: VMM memory accessed\n\r");
      return 0;
    }
    */


    if (fakepagedirtable[pagedirentry].PS)
    {
      sendstring("This is a big page, and has no PTE, therefore, the fault was made by the guest\n\r");

      if (errorcode.P==0) //caused by a page not present fault, but it has been proven the pages exist
        return 0;



      return 1; //raise exception

    }

    //-------------------------------------------------------------------
    //        still here, so not caused by the pde, check pte
    //-------------------------------------------------------------------
    sendstring("--------------\n\rChecking pte\n\r");



    if (realpagetable[pagetableentry].P)
    {
      sendstring("realpagetable entry is present\n\r");
      sendstring("Checking consitency\n\r");
      if (
          (realpagetable[pagetableentry].PFN!=fakepagetable[pagetableentry].PFN) ||
          (realpagetable[pagetableentry].US!=fakepagetable[pagetableentry].US)
         )
      {
        //it changed, retry from start
        nosendchar[getAPICID()]=0;
        sendstringf("pagetable: Inconsistency detected (%8)\n\r",faultingaddress);
        nosendchar[getAPICID()]=1;

        realpagetable[pagetableentry].P=0;
        return 0;
      }
    }



    if (errorcode.P==0)
    {
      sendstring("Exception due to present flag, check present flag\n\r");


      //8: consult if the active PTE is the cause
      if (fakepagetable[pagetableentry].P==0)
      {
        sendstring("Fakepagetable is not present\n\r");
        if (realpagetable[pagetableentry].P==0)
          return 1; //not present according to the guest, raise exception
        else
        {
          realpagetable[pagetableentry].P=0;
          return 0; //adjust to normal state, next go will cause a (normal) exception
        }
      }

      if (realpagetable[pagetableentry].P==0)
      {
        int i;

        sendstring("Real pagetable entry is not present(As expected since it wasn't the pde\n\r)\n\r");
        //4: if active pte is not present then set active pte to guest pte (since the guest check inclused present checks, this HAS top be caused by the A bit reason)
        //fill in guest entry
        fakepagetable[pagetableentry].A=1;
        for (i=0; i<1024; i++)
        {
          if (fakepagetable[i].P && fakepagetable[i].A)
            fillTLB_PTE(&realpagetable[i],&fakepagetable[i]);
        }

        sendstringf("after:\n\r");
        sendstringf("fakepagedirtable[pagedirentry]=%8\n\r",*(ULONG*)(&fakepagedirtable[pagedirentry]));
        if (fakepagedirtable[pagedirentry].PS==0)
        {
          sendstringf("fakepagetable[pagetableentry]=%8\n\r",*(ULONG*)(&fakepagetable[pagetableentry]));
        }

        sendstringf("realpagedirtable[pagedirentry]=%8\n\r",*(ULONG*)(&realpagedirtable[pagedirentry]));
        if (realpagedirtable[pagedirentry].PS==0)
        {
          sendstringf("realpagetable[pagetableentry]=%8\n\r",*(ULONG*)(&realpagetable[pagetableentry]));
        }

        return 0; //restart instruction
      }
      else
      {
        sendstring("realpagetable[pagetableentry].P==1!!!\n\r");

      }
    }
    else
    {
      sendstring("Exception caused by access violation, check writable bit\n\r");
      if (fakepagetable[pagetableentry].RW==0)
        writable=((errorcode.US==0) && (fakeCR0.WP==0));

      //not caused by the present flag, check writable bit

      if (errorcode.W)
      {
        //not caused by pagetable present flag, check write flag, since it's a write
        if (!writable)
          return 1; //not writable, raise exception

        if (realpagetable[pagetableentry].RW==0)
        {
          sendstring("Write operation, writable=1 and WR==0, setting WR to 1\n\r");
          //write operation, but according to the active pagetable it's not, so make writable and set dirty
          if ((errorcode.US==0) && (fakeCR0.WP==0))
            realpagetable[pagetableentry].RW=1;
          else
            realpagetable[pagetableentry].RW=fakepagetable[pagetableentry].RW;
          fakepagetable[pagetableentry].D=1;

          sendstringf("after:\n\r");
          sendstringf("fakepagedirtable[pagedirentry]=%8\n\r",*(ULONG*)(&fakepagedirtable[pagedirentry]));
          if (fakepagedirtable[pagedirentry].PS==0)
          {
            sendstringf("fakepagetable[pagetableentry]=%8\n\r",*(ULONG*)(&fakepagetable[pagetableentry]));
          }

          sendstringf("realpagedirtable[pagedirentry]=%8\n\r",*(ULONG*)(&realpagedirtable[pagedirentry]));
          if (realpagedirtable[pagedirentry].PS==0)
          {
            sendstringf("realpagetable[pagetableentry]=%8\n\r",*(ULONG*)(&realpagetable[pagetableentry]));
          }


          return 0; //restart
        }
      }
    }

    //still here, so not a problem with the guest's pte
    /*if (((fakepagetable[pagetableentry].PFN << 12)>=vmmstart) && (fakepagetable[pagetableentry].PFN << 12)<(vmmstart+0x00400000))
    {
      nosendchar[getAPICID()]=0;
      sendstring("pagetable: VMM memory accessed\n\r");
      return 0;
    }*/

    //still here, so not caused by vmm

    if (errorcode.P==0)
    {
      return 0; //try again, it has been proven that P=1
    }


    sendstring("Still here, so exception probably caused by guest\n\r");
    return 1; //raise exception

  } //end of non pae mode

  nosendchar[getAPICID()]=0;
  sendstring("ALERT!!!!!! EOR REACHED!!!!You must have a quantum cpu since a bit isn't eitner 0 or 1\n\r");
  nosendchar[getAPICID()]=1;

  return 1;
}


int emulatePaging(pcpuinfo currentcpuinfo)
/*
Called when paging is set from 0 to 1
Called when CR3 is written to
Called when the pagetable is written to
*/
{
  regCR0 fakeCR0;
  regCR4 fakeCR4;

  fakeCR0.CR0=vmread(0x6004);
  fakeCR4.CR4=vmread(0x6006);

 	//read the guest's CR3 and use that to emulate the pagetable
  sendstring("emulatePaging\n");

  if (!memorycloak)
  {
    sendstringf("No memorycloak\n");
    if  ((vmread(vm_cr0_fakeread) & 0x80000001)==0x80000001) //is in paging/protected mode according to the guest?
    {
      if (currentcpuinfo->cr3_callback.changedcr3)
      {
        vmwrite(vm_guest_cr3, currentcpuinfo->cr3_callback.newcr3);
      }
      else
      {
        sendstringf("Changing the real CR3 from %6 to %6\n",vmread(vm_guest_cr3),currentcpuinfo->guestCR3);
        vmwrite(vm_guest_cr3, currentcpuinfo->guestCR3); //set cr3 to whatthe guest wants it to be
      }

      return 0;
    }
    else
    {
      sendstringf("Not in paging and protected mode. vm_cr0_fakeread=%6\n",vmread(vm_cr0_fakeread));

    }
  }
  else
    sendstring("memorycloak=1\n");


  //-------------------

  if (currentcpuinfo->virtualTLB==NULL)
  {
    sendstring("Allocating virtual TLB\n\r");
    allocateVirtualTLB();
  }

  sendstringf("1:emulatepaging: guestCR3=%6\n\r",currentcpuinfo->guestCR3);

  //set CR3
  vmwrite(vm_guest_cr3,(UINT64)VirtualToPhysical((UINT64)currentcpuinfo->virtualTLB));

  //clear base pde
  zeromemory(currentcpuinfo->virtualTLB,4096);
  currentcpuinfo->virtualTLB_FreeSpot=currentcpuinfo->virtualTLB+4096;
  currentcpuinfo->virtualTLB_whiped=1;

  return 0;

}

/*
int setupA20maskedPaging(void)
{
  int i;
  PPDPTE_PAE VirtualMachinePageDirPointer;
  PPDE_PAE   VirtualMachinePageDir;
  PPTE_PAE   VirtualMachinePageTable;

  sendstringf("setting up real mode paging\n\r");
//sets up a identity mapped memory with overflow to 0 after 1 MB
//uses PAE so set CR4 to pae mode
	if (VirtualMachinePagingspace==NULL)
    allocvirtualpagememory();

	VirtualMachinePageDirPointer=VirtualMachinePagingspace;
	VirtualMachinePageDir=VirtualMachinePagingspace+0x1000;
	VirtualMachinePageTable=VirtualMachinePagingspace+0x1000+2048*8;


  zeromemory(VirtualMachinePageDirPointer,4096);
  zeromemory(VirtualMachinePageDir,2048*8); //to map 00000000 to ffffffff (one entry can map 0x00200000, 2048=0x00200000*2048=0x100000000)
  zeromemory(VirtualMachinePageTable,4096); //4096=able to map 0x00200000 bytes, in this case 0x00200000 to 0x00400000

  sendstringf("VirtualMachinePageDirPointer=%8\n\r",(ULONG)VirtualMachinePageDirPointer);
  sendstringf("VirtualMachinePageDir=%8\n\r",(ULONG)VirtualMachinePageDir);
  sendstringf("VirtualMachinePageTable=%8\n\r",(ULONG)VirtualMachinePageTable);

  for (i=0; i<4; i++)
  {
    VirtualMachinePageDirPointer[i].P=1;
    VirtualMachinePageDirPointer[i].PFN=VirtualToPhysical((ULONG)VirtualMachinePageDir+(i*4096)) >> 12;
  }

  //00000000-001fffff (low memory, map this using seperate pages)
  VirtualMachinePageDir[0].P=1;
  VirtualMachinePageDir[0].A=0;
  VirtualMachinePageDir[0].RW=1;
  VirtualMachinePageDir[0].US=1;
  VirtualMachinePageDir[0].PS=0; //has a pagetable
  VirtualMachinePageDir[0].PFN=VirtualToPhysical((ULONG)VirtualMachinePageTable) >> 12;


  for (i=1; i<2048; i++)  //512 in one pagedir, 4 pagedirs=2048 entries
  {
    //identity map these regions (easy when switching to protected mode)
    VirtualMachinePageDir[i].P=1;
    VirtualMachinePageDir[i].A=0;
    VirtualMachinePageDir[i].RW=1;
    VirtualMachinePageDir[i].US=1;
    VirtualMachinePageDir[i].PS=1; //no pagetable
    VirtualMachinePageDir[i].PFN=(i*0x00200000) >> 12;
  }

  //now configure the first pagetable for the low memory of 00000000 to fffff and the wraparround after 0x100000 bytes
  for (i=0; i<(512); i++)
  {
    VirtualMachinePageTable[i].P=1;
    VirtualMachinePageTable[i].A=0;
    VirtualMachinePageTable[i].RW=1;
    VirtualMachinePageTable[i].US=1;
    VirtualMachinePageTable[i].PFN=i % 256;
  }

  vmwrite(vm_guest_cr3,(ULONG)VirtualToPhysical((ULONG)VirtualMachinePagingspace));
}
*/

int setupRealModePaging(pcpuinfo currentcpuinfo)
/* called on switch to realmode and when the a20 line is modified
 * This is always 32-bit */
{
  //PPDPTE_PAE VirtualMachinePageDirPointer;
  //PPDE_PAE   VirtualMachinePageDir;
  //ULONG      address;


  //check if the A20 line is enabled, if so
  return setupNonPagedPaging(currentcpuinfo);

  //else
  //setupA20maskedPaging();

  //but make the VMM accessible... (VirtualMachineTSS_V8086 has to be readable by the vm86 monitor)
  /*
  address=(ULONG)VirtualToPhysical((ULONG)VirtualMachineTSS_V8086);
  {
    ULONG       pagedirptrentry=address >> 30;
    ULONG       pagedirentry=address >> 21 & 0x1ff;
    PPDPTE_PAE  pagedirptrtable=VirtualMachinePagingspace;
    PPDE_PAE    pagedirtable=(PPDE_PAE)MapPhysicalMemory(pagedirptrtable[pagedirptrentry].PFN << 12, 0x10000000);

    pagedirtable[pagedirentry].PFN=address >> 12;

  }*/

}

int setupNonPagedPaging(pcpuinfo currentcpuinfo)
{
//sets up a identify mapped memory (faking the vmm memory as ffpages)
//uses PAE so set CR4 to pae mode
  PPDPTE_PAE VirtualMachinePageDirPointer;
  PPDE_PAE   VirtualMachinePageDir;
  //UINT64     pfn_vmmstart=vmmstart >> 12;
  //UINT64     pfn_vmmstop=(vmmstart+0x00400000) >> 12;
  int        is64bitpaging=IS64BITPAGING(currentcpuinfo);

	unsigned int i;

  sendstringf("Setting up protected mode paging for nonpaged emu\n\r");

  if (nonpagedEmulationPagedir==NULL)
  {
    nonpagedEmulationPagedir=malloc(4096+4096+4096*8); //pml4+pagedirpointer + pagedirs to map 00000000 till 1ffffffff, since one entry maps 00200000 bytes
    zeromemory(nonpagedEmulationPagedir,4096+4096+4096*8);
  }


  if (is64bitpaging) //eg the msr has been set, but not really switched yet, entering this state (gets called on msr edit)
  {
    //setup a real paging mechanism for 64-bit, basicly the same as 32-bit just one pml4 entry
    PPDE_PAE pml4table=nonpagedEmulationPagedir;
    VirtualMachinePageDirPointer=nonpagedEmulationPagedir+0x1000;
    pml4table[0].P=1;
    pml4table[0].RW=1;
    pml4table[0].PFN=VirtualToPhysical((UINT64)VirtualMachinePageDirPointer) >> 12;
    sendstring("in 64-bit mode paging, shouldn\'t be used, since it\'s only enabled when paging is on");
  }
  else
    VirtualMachinePageDirPointer=nonpagedEmulationPagedir;

  VirtualMachinePageDir=(void *)((UINT64)VirtualMachinePageDirPointer+0x1000);

  sendstringf("VirtualMachinePageDirPointer=%6\n\r",(UINT64)VirtualMachinePageDirPointer);
  sendstringf("VirtualMachinePageDir=%6\n\r",(UINT64)VirtualMachinePageDir);

  for (i=0; i<8; i++)
  {
    VirtualMachinePageDirPointer[i].P=1;
    if (is64bitpaging)
      VirtualMachinePageDirPointer[i].RW=1;

    VirtualMachinePageDirPointer[i].PFN=VirtualToPhysical((UINT64)VirtualMachinePageDir+(i*4096)) >> 12;
  }

  for (i=0; i<4096; i++)  //512 in one pagedir, 4 pagedirs=2048 entries
  {
    //identity map these regions (easy when switching to protected mode)
    VirtualMachinePageDir[i].P=1;
    VirtualMachinePageDir[i].A=0;
    VirtualMachinePageDir[i].RW=1;
    VirtualMachinePageDir[i].US=1;
    VirtualMachinePageDir[i].PS=1; //no pagetable

    VirtualMachinePageDir[i].PCD=0;
    VirtualMachinePageDir[i].PWT=0;
    VirtualMachinePageDir[i].PFN=(UINT64)(((UINT64)i*0x00200000) >> 12);

    if ((i!=0) && (VirtualMachinePageDir[i].PFN==0))
    {
      nosendchar[getAPICID()]=0;
      sendstringf("Invalid PFN set : %d\n\r", i);
      while (1);

    }


    /*

    //--------------------
		//check if this pfn points to the vmm
		if ((VirtualMachinePageDir[i].PFN>=pfn_vmmstart) && (VirtualMachinePageDir[i].PFN<pfn_vmmstop))
		{
      //if so, fake dir

      if ((vmread(0x6004) & 1)==1) //really protected mode
      {
        VirtualMachinePageDir[i].PS=0;
        VirtualMachinePageDir[i].RW=0; //readonly
			  VirtualMachinePageDir[i].PFN=VirtualToPhysical((UINT64)ffpagetable) >> 12;
      } //else actualy realmode, so allow, except writes
      else
      {
        //unless the guest is in realmode and this is just a move to execute privileged instructions, then only readonly and supervisormode
        VirtualMachinePageDir[i].RW=0;
        VirtualMachinePageDir[i].US=0; //ring0 only
      }
		}
		*/
//-----------
  }


  vmwrite(vm_guest_cr3,(UINT64)VirtualToPhysical((UINT64)nonpagedEmulationPagedir)); //set guest's cr3 (real one) to this address

  return 0;
}

int setupNonPagedPaging_invalidstate_c0000(pcpuinfo currentcpuinfo, UINT64 cs_base)
{
//sets up a page where the cs_base will be mapped at c0000
//uses PAE so set CR4 to pae mode
  volatile PPDPTE_PAE VirtualMachinePageDirPointer;
  volatile PPDE_PAE   VirtualMachinePageDir;
  volatile PPTE_PAE   VirtualMachinePageTable;
  //DWORD      pfn_vmmstart=vmmstart >> 12;
  //DWORD      pfn_vmmstop=(vmmstart+0x00400000) >> 12;
  int        is64bitpaging=IS64BITPAGING(currentcpuinfo);
  int        orig=nosendchar[getAPICID()];

  //unsigned int i;

  nosendchar[getAPICID()]=0;

  sendstring("Setting up protected mode page for nonpaged emu (INVALID STATE)\n\r");


  if (is64bitpaging)
  {
    if (nonpagedInvalidEmulationPagedir==NULL) //already exists, so no need to alloc
      nonpagedInvalidEmulationPagedir=malloc(4096+4096+4096+4096); //pml4 table+pagedirptr + pagedir + pagetable with 1 entry for c0000

    zeromemory(nonpagedInvalidEmulationPagedir,4096+4096+4096+4096);
  }
  else
  {
    if (nonpagedInvalidEmulationPagedir==NULL) //already exists, so no need to alloc
      nonpagedInvalidEmulationPagedir=malloc(4096+4096+4096); //pagedirptr + pagedir + pagetable with 1 entry for c0000

    zeromemory(nonpagedInvalidEmulationPagedir,4096+4096+4096);
  }


  sendstringf("Allocated nonpagedInvalidEmulationPagedir at %6\n\r",(UINT64)nonpagedInvalidEmulationPagedir);





  if (is64bitpaging) //eg the msr has been set, but not really switched yet, entering this state (gets called on msr edit)
  {
    //setup a real paging mechanism for 64-bit, basicly the same as 32-bit just one pml4 entry
    volatile PPDE_PAE pml4table=nonpagedInvalidEmulationPagedir;
    VirtualMachinePageDirPointer=nonpagedInvalidEmulationPagedir+0x1000;
    pml4table[0].P=1;
    pml4table[0].RW=1;
    pml4table[0].PFN=VirtualToPhysical((UINT64)nonpagedInvalidEmulationPagedir) >> 12;
    sendstring("in 64-bit mode paging, shouldn\'t be used, since it\'s only enabled when paging is on");
  }
  else
    VirtualMachinePageDirPointer=nonpagedInvalidEmulationPagedir;

  VirtualMachinePageDir=(void *)((UINT64)VirtualMachinePageDirPointer+0x1000);
  VirtualMachinePageTable=(void *)((UINT64)VirtualMachinePageDir+0x1000);

  sendstringf("VirtualMachinePageDirPointer=%6\n\r",(UINT64)VirtualMachinePageDirPointer);
  sendstringf("VirtualMachinePageDir=%6\n\r",(UINT64)VirtualMachinePageDir);

  VirtualMachinePageDirPointer[0].P=1;
  if (is64bitpaging)
    VirtualMachinePageDirPointer[0].RW=1;
  VirtualMachinePageDirPointer[0].PFN=VirtualToPhysical((UINT64)VirtualMachinePageDir) >> 12;


  VirtualMachinePageDir[0].P=1;
  VirtualMachinePageDir[0].A=0;
  VirtualMachinePageDir[0].RW=1;
  VirtualMachinePageDir[0].US=1;
  VirtualMachinePageDir[0].PS=0; //has pagetable
  VirtualMachinePageDir[0].PCD=0;
  VirtualMachinePageDir[0].PWT=0;
  VirtualMachinePageDir[0].PFN=VirtualToPhysical((UINT64)VirtualMachinePageTable) >> 12;

  VirtualMachinePageTable[0xc0].P=1;
  VirtualMachinePageTable[0xc0].A=0;
  VirtualMachinePageTable[0xc0].RW=1;
  VirtualMachinePageTable[0xc0].US=1;
  VirtualMachinePageTable[0xc0].PCD=0;
  VirtualMachinePageTable[0xc0].PWT=0;
  VirtualMachinePageTable[0xc0].PFN=cs_base >> 12;

  vmwrite(vm_guest_cr3,(UINT64)VirtualToPhysical((UINT64)nonpagedInvalidEmulationPagedir)); //set guest's cr3 (real one) to this address

  nosendchar[getAPICID()]=orig;

  return 0;
}

