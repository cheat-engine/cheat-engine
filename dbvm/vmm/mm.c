/*
mm.c:
This will manage the memory allocs and free's
Just used for basic initialization allocation, frees shouldn't happen too often

*/

#include "mm.h"
#include "main.h"
#include "multicore.h"
#include "common.h"
#include "vmmhelper.h"
#include "displaydebug.h"

//#define sendstringf(s,x...)
//#define sendstring(s)


/*
 * new mm
 *
 */

#define BASE_VIRTUAL_ADDRESS 0x1000000000ULL

//MAPPEDMEMORY is the range of virtual memory allocated for individual CPU threads mapping
#define MAPPEDMEMORY 0x08000000000ULL

//GLOBALMAPPEDMEMORY is the virtual memory allocated for the whole system for mapping
#define GLOBALMAPPEDMEMORY 0x07000000000ULL

//for virtual memory allocs
criticalSection AllocCS={.name="AllocCS", .debuglevel=2};
criticalSection GlobalMapCS={.name="GlobalMapCS", .debuglevel=2};


PageAllocationInfo *AllocationInfoList=(PageAllocationInfo *)BASE_VIRTUAL_ADDRESS;
int PhysicalPageListSize=1; //size in pages
int PhysicalPageListMaxSize=64; //if the size goes over this, reallocate the list

//for mapping

//each cpu gets one of these
/*
char *MappedAllocationInfoList=NULL;  //no need to bitfuck here, there's not going to be THAT much each byte is 1 page
int MappedAllocationInfoListSize=0; //size in pages
int MappedAllocationInfoListMax=0; //if the size goes over this, reallocate the list
*/

//host cr3 is mapped at 0xFFFFFF8000000000, which causes the following mappings:
PPDPTE_PAE        pml4table=(PPDPTE_PAE)0xfffffffffffff000ULL;
PPDPTE_PAE pagedirptrtables=(PPDPTE_PAE)0xffffffffffe00000ULL;
PPDE_PAE      pagedirtables=  (PPDE_PAE)0xffffffffc0000000ULL;
PPTE_PAE         pagetables=  (PPTE_PAE)0xffffff8000000000ULL;



QWORD FirstFreeAddress;

unsigned char MAXPHYADDR=0; //number of bits a physical address can be made up of
QWORD MAXPHYADDRMASK=  0x0000000fffffffffULL; //mask to AND with a physical address to strip invalid bits
QWORD MAXPHYADDRMASKPB=0x0000000ffffff000ULL; //same as MAXPHYADDRMASK but also aligns it to a page boundary


//alloc(not 2) keeps a list of allocs and their sizes.  This linked list (allocated using alloc2) is used to keep track of those allocs. Sorted by base
typedef struct
{
  unsigned long long base;
  ULONG size;
  /*
   *if size=0 then this block has actually been freed and can be reused (for an address that falls between the previous and next)
   *This reduces shifting operations when an item in the center gets freed and speeds up lots of small alloc/free routine
   *
   */
} MemlistItem2, *PMemlistItem2;


MemlistItem2 *AllocList=NULL;
int AllocListMax=0;
int AllocListPos=0;

UINT64 TotalAvailable;


void* contiguousMemory; //alloc once memory. Doesn't allow free yet
int contiguousMemoryPagesFree;


void free2(void *address, unsigned int size);


void* allocateContiguousMemory(int pagecount)
{
  void *result=NULL;
  csEnter(&AllocCS);

  sendstringf("allocateContiguousMemory(%d)\n", pagecount);
  sendstringf("contiguousMemoryPagesFree=%d\n", contiguousMemoryPagesFree);
  sendstringf("contiguousMemory=%p\n", contiguousMemory);


  if (contiguousMemoryPagesFree>=pagecount)
  {
    result=contiguousMemory;

    (*(QWORD*)&contiguousMemory)+=4096*pagecount;
    contiguousMemoryPagesFree-=pagecount;
  }
  else
  {
    nosendchar[getAPICID()]=0;
    sendstringf("contiguousMemoryPagesFree<pagecount");
    while (1)
    {
      sendstringf("contiguousMemoryPagesFree<pagecount");

      outportb(0x80,0x01);
      outportb(0x80,0x10);
    }
  }



  csLeave(&AllocCS);

  return result;
}

/*
 * There was a time where the memory manager would free blocks it shouldn't. this is a test for that scenario)
void wtftest(void)
{
  pcpuinfo c=getcpuinfo();
  int i;

  for (i=0; i<c->eptWatchlistLength; i++)
  {
    if (c->eptWatchlist[i].Active)
    {
      EPT_PTE x;

      try
      {
        PEPTWatchEntry pe=c->eptWatchlist;
        EPTWatchEntry e=pe[i];
        _invlpg((QWORD)e.EPTEntry);
        x=*(e.EPTEntry);

        if (x.Accessed)
          sendstringf("Valid and accesses\n");
      }
      except
      {
        nosendchar[getAPICID()]=0;
        sendstringf("EPTEntry for a watch got invalidated\n");
        while (1);
      }
      tryend

    }
  }
}
*/

void* mapMemory(void *destination, void *source, int size)
/*
 * This function will setup paging at destination matching the source
 */
{
  //get the physical addresses of the source
  int i;
  int offset=(QWORD)source & 0xfff;
  int totalsize=size+offset;

  void *result=(void *)((QWORD)destination+offset);

  int pagecount=totalsize / 4096;
  if (size & 0xfff)
    pagecount++;

  //just being lazy atm, this can be optimized a lot
  for (i=0; i<pagecount; i++)
  {
    //VirtualToPhysical()
    //getOrAllocatePageTableEntryForAddress()
    PPDE_PAE destinationpte=getPageTableEntryForAddressEx(destination,1);
    PPDE_PAE sourcepte=getPageTableEntryForAddress(source);

    *destinationpte=*sourcepte;

    source+=4096;
    destination+=4096;

    _invlpg((QWORD)destination);
  }

  return result;
}

int getFreePML4Index(void)
{
  int i;
  for (i=511; i>1; i--)
  {
    if (pml4table[i].P==0)
      return i;
  }

  return -1;
}

void unmapAddressAtPML4(PPTE_PAE base)
{
  int index=((QWORD)base >> 39) & 0x1ff;
  pml4table[index].P=0;
  _wbinvd();
}

PPTE_PAE mapAddressAtPML4(QWORD address)
{
  static criticalSection CS={.name="static mapAddressAtPML4 CS", .debuglevel=2};
  int index;

  csEnter(&CS);

  sendstringf("mapAddressAtPML4(%6)\n", address);

  index=getFreePML4Index();

  if (index==-1)
  {
    sendstring("No PML4 entries free\n");
    csLeave(&CS);
    return NULL;
  }

  *(QWORD*)(&pml4table[index])=address;
  pml4table[index].P=1;
  pml4table[index].RW=1;
  asm volatile ("": : :"memory");
  csLeave(&CS);


  QWORD result=((QWORD)index << 39);
  if (result>=0x800000000000ULL) //sign extend
    result|=0xffff000000000000ULL;

  _invlpg(result);

  _wbinvd();

  return (PPTE_PAE)result;
}

void VirtualAddressToPageEntries(QWORD address, PPDPTE_PAE *pml4entry, PPDPTE_PAE *pagedirpointerentry, PPDE_PAE *pagedirentry, PPTE_PAE *pagetableentry)
{
  QWORD PTE=address;
  PTE=PTE & 0x0000ffffffffffffULL;
  PTE=PTE >> 12;
  PTE=PTE * 8;
  PTE=PTE+0xffffff8000000000ULL;
  *pagetableentry=(PPTE_PAE)PTE;
  asm volatile ("": : :"memory");

  //*pagetableentry = (PPTE_PAE)((((QWORD)address & 0x0000ffffffffffffull) >> 12)*8) + 0xfffff80000000000ULL;

  QWORD PDE=PTE;
  PDE=PDE & 0x0000ffffffffffffULL;
  PDE=PDE >> 12;
  PDE=PDE * 8;
  PDE=PDE+0xffffff8000000000ULL;
  *pagedirentry = (PPDE_PAE)PDE;
  asm volatile ("": : :"memory");


  //*pagedirentry = (PPDE_PAE)((((QWORD)*pagetableentry & 0x0000ffffffffffffull )>> 12)*8) + 0xfffff80000000000ULL;

  QWORD PDPTR=PDE;
  PDPTR=PDPTR & 0x0000ffffffffffffULL;
  PDPTR=PDPTR >> 12;
  PDPTR=PDPTR * 8;
  PDPTR=PDPTR+0xffffff8000000000ULL;
  *pagedirpointerentry=(PPDPTE_PAE)PDPTR;
  asm volatile ("": : :"memory");

  //*pagedirpointerentry = (PPDPTE_PAE)((((QWORD)*pagedirentry & 0x0000ffffffffffffull )>> 12)*8) + 0xfffff80000000000ULL;

  QWORD PML4=PDPTR;
  PML4=PML4 & 0x0000ffffffffffffULL;
  PML4=PML4 >> 12;
  PML4=PML4 * 8;
  PML4=PML4+0xffffff8000000000ULL;
  *pml4entry=(PPDPTE_PAE)PML4;
  asm volatile ("": : :"memory");
}

void VirtualAddressToIndexes(QWORD address, int *pml4index, int *pagedirptrindex, int *pagedirindex, int *pagetableindex)
/*
 * Returns the indexes for the given address  (ia32e)
 */
{
  *pml4index=(address >> 39) & 0x1ff;
  *pagedirptrindex=(address >> 30) & 0x1ff;
  *pagedirindex=(address >> 21) & 0x1ff;
  *pagetableindex=(address >> 12) & 0x1ff;
}



void *getMappedMemoryBase()
/*
 * Returns the virtual address where this cpu's mapped regions are located
 */
{
  return (void *)(MAPPEDMEMORY+getcpunr()*0x400000);
}

int mmFindMapPositionForSize(pcpuinfo cpuinfo, int size)
{

  if (size==0)
    return -1;

  int pagecount=size / 4096;
  int i,pos;
  if (size % 4096)
    pagecount++;


  //get the pagedir entries (2) for this cpu (1024 entries)
  //allocate if needed
  PPTE_PAE pagetable=cpuinfo->mappagetables;


  if (pagetable==NULL)
  {
    PPDPTE_PAE pml4, pdptr;
    PPDE_PAE pagedir;

    VirtualAddressToPageEntries((QWORD)getMappedMemoryBase(),&pml4, &pdptr, &pagedir, &pagetable);
    if (pml4->P==0)
    {
      *(QWORD *)pml4=VirtualToPhysical(malloc2(4096));
      pml4->P=1;
      pml4->US=1;
      pml4->RW=1;

      asm volatile ("": : :"memory");
      _invlpg((QWORD)pdptr);
      asm volatile ("": : :"memory");
      zeromemory((void*)((QWORD)pdptr & 0xfffffffffffff000ULL), 4096);
    }

    if (pdptr->P==0)
    {
      *(QWORD *)pdptr=VirtualToPhysical(malloc2(4096));
      pdptr->P=1;
      pdptr->US=1;
      pdptr->RW=1;

      asm volatile ("": : :"memory");
      _invlpg((QWORD)pagedir);
      asm volatile ("": : :"memory");
      zeromemory((void*)((QWORD)pagedir & 0xfffffffffffff000ULL), 4096);
    }

    if (pagedir[0].P==0)
    {
      *(QWORD*)&pagedir[0]=VirtualToPhysical(malloc2(4096));
      pagedir[0].P=1;
      pagedir[0].US=1;
      pagedir[0].RW=1;
      asm volatile ("": : :"memory");
      _invlpg((QWORD)pagetable);
      asm volatile ("": : :"memory");
      zeromemory((void*)((QWORD)pagetable & 0xfffffffffffff000ULL), 4096);
    }

    if (pagedir[1].P==0)
    {
      *(QWORD*)&pagedir[1]=VirtualToPhysical(malloc2(4096));
      pagedir[1].P=1;
      pagedir[1].US=1;
      pagedir[1].RW=1;
      asm volatile ("": : :"memory");
      _invlpg((QWORD)pagetable+4096);
      asm volatile ("": : :"memory");
      zeromemory((void*)(((QWORD)pagetable+4096) & 0xfffffffffffff000ULL), 4096);
    }

    cpuinfo->mappagetables=pagetable;
  }

  pos=-1;
  for (i=0; i<1024; i++)
  {
    if (pagetable[i].P==0)
    {
      int j=i+1;
      int needed=pagecount-1;
      while (needed)
      {
        if (pagetable[j].P)
          break;

        needed--;
        j++;

        if (j>=1024)
          return -1;
      }

      if (needed==0)
      {
        pos=i;
        break;
      }
    }
  }

  return pos;
}

volatile int FreezeOnMapAllocFail=1;


void mapPhysicalAddressToVirtualAddress(QWORD PhysicalAddress, QWORD VirtualAddress)
{
  VirtualAddress=0xfffffffffffff000ULL;
  PhysicalAddress=PhysicalAddress & MAXPHYADDRMASKPB;
  PPTE_PAE pageentry=(PPTE_PAE)getPageTableEntryForAddressEx((void *)VirtualAddress,1);
  *(QWORD*)pageentry=PhysicalAddress;
  pageentry->P=1;
  pageentry->RW=1;
  pageentry->US=1;
  asm volatile ("": : :"memory");
  _invlpg(VirtualAddress);
  asm volatile ("": : :"memory");
}

QWORD mmFindGlobalMapAddressForSize(int size)
{
  int pagecount=size / 4096;
  if (size & 0xfff)
    pagecount++;

  PPTE_PAE pages;
  QWORD currentVirtualAddress=GLOBALMAPPEDMEMORY;

  //find a global page not used yet. Every 2MB call getPageTableEntryForAddressEx to make sure the pagetable is present
  while (currentVirtualAddress<MAPPEDMEMORY)
  {
    pages=(PPTE_PAE)getPageTableEntryForAddressEx((void *)currentVirtualAddress,1);
    //scan this page for pagecount number of pages
    int i;
    for (i=0; i<512; i++)
    {
      if (pages[i].P==0)
      {
        //scan for i to pagecount and make sure it's all 0
        //make sure that all pagetables between i and pagecount are present
        int j;
        int used=0;

        for (j=i+512; j<i+pagecount; j+=512)
          getPageTableEntryForAddressEx((void*)(currentVirtualAddress+4096*j),1);

        //now scan

        for (j=i; j<i+pagecount; j++)
        {
          if (pages[j].P)
          {
            used=1;
            break;
          }
        }
        if (used==0)
          return currentVirtualAddress+4096*i;
      }
    }
    currentVirtualAddress+=2*1024*1024; //next 2MB
  }

  return 0;
}

void* mapPhysicalMemoryGlobal(QWORD PhysicalAddress, int size) //heavy operation
{
  int i;
  unsigned int offset=PhysicalAddress & 0xfff;
  int totalsize=size+offset;
  int pagecount=totalsize / 4096;
  if (totalsize & 0xfff)
      pagecount++;

  PPTE_PAE pages;
  QWORD VirtualAddress;
  csEnter(&GlobalMapCS);


  VirtualAddress=mmFindGlobalMapAddressForSize(totalsize);
  if (VirtualAddress)
  {
    pages=(PPTE_PAE)getPageTableEntryForAddress((void *)VirtualAddress);
    for (i=0; i<pagecount; i++)
    {
      *(QWORD*)&pages[i]=(PhysicalAddress+(4096*i)) & MAXPHYADDRMASKPB;
      pages[i].P=1;
      pages[i].RW=1;
      pages[i].US=1;
      asm volatile ("": : :"memory");
      _invlpg(VirtualAddress+i*4096);
      asm volatile ("": : :"memory");
    }
  }

  _wbinvd();
  csLeave(&GlobalMapCS);

  return (void*)VirtualAddress+offset;
}

void* mapPhysicalMemoryAddresses(QWORD *addresses, int count)
/*
 * Maps the given physical addresses in the order given
 * addresses is an array of 4KB aligned physical memory addresses
 */
{
  int i;
  pcpuinfo c=getcpuinfo();
  QWORD VirtualAddressBase=MAPPEDMEMORY+c->cpunr*0x400000;
  int pos=mmFindMapPositionForSize(c, count*4096);
  if (pos==-1)
  {
    nosendchar[getAPICID()]=0;
    sendstring("mapPhysicalMemoryAddresses: Out of virtual memory to map region.  Check the size and make sure you unmap as well\n");
    while (FreezeOnMapAllocFail)
    {
      outportb(0x80,2);
    }
    return NULL;
  }

  for (i=0; i<count; i++)
  {
    *(QWORD*)&c->mappagetables[pos+i]=addresses[i] & MAXPHYADDRMASKPB;
    c->mappagetables[pos+i].P=1;
    c->mappagetables[pos+i].RW=1;
    c->mappagetables[pos+i].US=1;

    asm volatile ("": : :"memory");
    _invlpg(VirtualAddressBase+(pos+i)*4096);
    asm volatile ("": : :"memory");
  }

  return (void *)(VirtualAddressBase+pos*4096);
}

void* mapPhysicalMemory(QWORD PhysicalAddress, int size)
{
  //find a free virtual address in the range assigned to this cpu
  int i,pos;
  unsigned int offset=PhysicalAddress & 0xfff;
  int totalsize=size+offset;
  int pagecount=totalsize / 4096;
  if (totalsize & 0xfff)
      pagecount++;

  pcpuinfo c=getcpuinfo();

  QWORD VirtualAddressBase=MAPPEDMEMORY+c->cpunr*0x400000;

  //find non-present pages
  pos=mmFindMapPositionForSize(c, totalsize);

  if (pos==-1)
  {
    nosendchar[getAPICID()]=0;
    sendstring("mapPhysicalMemory: Out of virtual memory to map region.  Check the size and make sure you unmap as well\n");
    while (FreezeOnMapAllocFail)
    {
      outportb(0x80,0x02);
    }
    return NULL;
  }


  //0 everything from bit MAXPHYADDR to 63
  PhysicalAddress=PhysicalAddress & MAXPHYADDRMASKPB;


  //map at pos
  for (i=0; i<pagecount; i++)
  {
    *(QWORD*)&c->mappagetables[pos+i]=PhysicalAddress;
    c->mappagetables[pos+i].P=1;
    c->mappagetables[pos+i].RW=1;
    c->mappagetables[pos+i].US=1;

    PhysicalAddress+=4096;
    asm volatile ("": : :"memory");
    _invlpg(VirtualAddressBase+(pos+i)*4096);
    asm volatile ("": : :"memory");
  }

  asm volatile ("": : :"memory");


  return (void *)(VirtualAddressBase+pos*4096+offset);
}

void unmapPhysicalMemoryGlobal(void *virtualaddress, int size)
{


  if (((QWORD)virtualaddress>=GLOBALMAPPEDMEMORY) && ((QWORD)virtualaddress+size<MAPPEDMEMORY))
  {
    QWORD base=(QWORD)virtualaddress & 0xfffffffffffff000ULL;;
    unsigned int offset=(QWORD)virtualaddress & 0xfff;
    int totalsize=size+offset;
    int pagecount=totalsize / 4096;
    if (totalsize & 0xfff)
        pagecount++;

    csEnter(&GlobalMapCS);
    PPTE_PAE pages=(PPTE_PAE)getPageTableEntryForAddress(virtualaddress);

    int i;
    for (i=0; i<pagecount; i++)
    {
      pages[i].P=0;
      asm volatile ("": : :"memory");
      _invlpg((QWORD)base+i*4096);
      asm volatile ("": : :"memory");
    }

    _wbinvd();
    csLeave(&GlobalMapCS);
  }
  else
  {
    nosendchar[getAPICID()]=0;
    sendstringf("invalid global address (%6) given to unmapPhysicalMemoryGlobal\n",virtualaddress);
    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
    while (1) outportb(0x80,0x01);
  }


}

void unmapPhysicalMemory(void *virtualaddress, int size)
{
  pcpuinfo c=getcpuinfo();

  unsigned int offset=(QWORD)virtualaddress & 0xfff;
  int totalsize=size+offset;
  int pagecount=totalsize / 4096;
  if (totalsize & 0xfff)
      pagecount++;



  int pos=(((QWORD)virtualaddress & 0xfffffffffffff000ULL)-(MAPPEDMEMORY+(c->cpunr*0x400000)))/4096;

  if ((pos<0) || (pos>1024))
  {
    nosendchar[getAPICID()]=0;
    sendstringf("%d: invalid address given to unmapPhysicalMemory (%6)\n",c->cpunr, virtualaddress);
    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
    while (1) outportb(0x80,0xce);
  }

  int i;
  QWORD MappedBase=(QWORD)getMappedMemoryBase();
  for (i=pos; i<pos+pagecount; i++)
  {
    c->mappagetables[i].P=0;
    asm volatile ("": : :"memory");
    _invlpg((QWORD)MappedBase+4096*i);
    asm volatile ("": : :"memory");
  }

}



void markPageAsNotReadable(void *address)
/*
 * Marks the page as not accessible. (This makes the physical page unusable)
 * should only be done on full pages that have been allocated
 */
{
  int index=((QWORD)address-BASE_VIRTUAL_ADDRESS) / 4096;
  if ((index>0) && (index<PhysicalPageListSize))
      AllocationInfoList[index].BitMask=0xffffffffffffffff; //just in case it wasn't allocated...

  PPDE_PAE pagedescriptor=getPageTableEntryForAddress(address);
  if (pagedescriptor)
  {
    pagedescriptor->P=0;
    asm volatile ("": : :"memory");
    _invlpg((QWORD)address);
    asm volatile ("": : :"memory");
  }
}

void markPageAsReadOnly(void *address)
{
  PPDE_PAE pagedescriptor=getPageTableEntryForAddress(address);
  if (pagedescriptor)
  {
    pagedescriptor->RW=0;
    asm volatile ("": : :"memory");
    _invlpg((QWORD)address);
    asm volatile ("": : :"memory");
  }
}

void markPageAsWritable(void *address)
{
  //marks a virtual page that was set as read only back to writable
  PPDE_PAE pagedescriptor=getPageTableEntryForAddress(address);
  if (pagedescriptor)
  {
    pagedescriptor->RW=1;
    asm volatile ("": : :"memory");
    _invlpg((QWORD)address);
    asm volatile ("": : :"memory");
  }
}

void SetPageToWriteThrough(void *address)
{
  PPDE_PAE pagedescriptor=getPageTableEntryForAddress(address);
  if (pagedescriptor)
  {
    pagedescriptor->PWT=1;
    pagedescriptor->PCD=1;
    asm volatile ("": : :"memory");
    _invlpg((QWORD)address);
    asm volatile ("": : :"memory");
  }
}

void *addPhysicalPageToDBVM(QWORD address, int inuse)
/*
 * Adds a physical page to the physicalPage List.
 * they will be mapped as virtual addresses at 0x1000000000 and beyond
 *
 * pre: the AllocCS must be owned by the current thread
 */
{
  UINT64 VirtualAddress=BASE_VIRTUAL_ADDRESS+4096*PhysicalPageListSize;

  PPDPTE_PAE pml4entry;
  PPDPTE_PAE pagedirptrentry;
  PPDE_PAE pagedirentry;
  PPTE_PAE pagetableentry;

  VirtualAddressToPageEntries(VirtualAddress, &pml4entry, &pagedirptrentry, &pagedirentry, &pagetableentry);

  if (pml4entry->P==0)
  {
    //make this page the new pagedirptr entry
    *(QWORD *)pml4entry=address;
    pml4entry->P=1;
    pml4entry->RW=1;
    pml4entry->US=1;
    asm volatile ("": : :"memory");
    _invlpg((QWORD)pagedirptrentry);
    asm volatile ("": : :"memory");
    zeromemory((void*)((QWORD)pagedirptrentry & 0xfffffffffffff000ULL), 4096);
    return NULL;
  }

  if (pagedirptrentry->P==0)
  {
    *(QWORD *)pagedirptrentry=address;
    pagedirptrentry->P=1;
    pagedirptrentry->RW=1;
    pagedirptrentry->US=1;
    asm volatile ("": : :"memory");
    _invlpg((QWORD)pagedirentry);
    asm volatile ("": : :"memory");
    zeromemory((void*)((QWORD)pagedirentry & 0xfffffffffffff000ULL), 4096);
    return NULL;
  }

  if (pagedirentry->P==0)
  {
    *(QWORD *)pagedirentry=address;
    pagedirentry->P=1;
    pagedirentry->RW=1;
    pagedirentry->US=1;
    asm volatile ("": : :"memory");
    _invlpg((QWORD)pagetableentry);
    asm volatile ("": : :"memory");
    zeromemory((void*)((QWORD)pagetableentry & 0xfffffffffffff000ULL), 4096);
    return NULL;
  }

  if (pagetableentry->P)
  {
    nosendchar[getAPICID()]=0;
    sendstringf("!Assertion failure! Virtual address %6 was already present (PhysicalPageListSize=%d)\n", VirtualAddress, PhysicalPageListSize);
    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
    while (1) outportb(0x80,0xcf);
  }

  *(QWORD *)pagetableentry=address;
  pagetableentry->P=1;
  pagetableentry->RW=1;
  pagetableentry->US=1;
  asm volatile ("": : :"memory");
  _invlpg(VirtualAddress);
  asm volatile ("": : :"memory");
  zeromemory((void*)VirtualAddress, 4096);


  //now mark these 4096 bytes as available to the memory manager
  if (!inuse)
    AllocationInfoList[PhysicalPageListSize].BitMask=0;
  else
    AllocationInfoList[PhysicalPageListSize].BitMask=0xffffffffffffffffULL;

  PhysicalPageListSize++;

  if (PhysicalPageListSize>=PhysicalPageListMaxSize)
  {
    //reallocate the list
    int i;
    void *oldlist=AllocationInfoList;
    int oldsize=sizeof(PageAllocationInfo)*PhysicalPageListMaxSize;

    AllocationInfoList=realloc2(oldlist, oldsize, sizeof(PageAllocationInfo)*PhysicalPageListMaxSize*2);
    PhysicalPageListMaxSize=PhysicalPageListMaxSize*2;
    for (i=PhysicalPageListSize; i<PhysicalPageListMaxSize; i++)
      AllocationInfoList[i].BitMask=0xffffffffffffffffULL;

    free2(oldlist, oldsize); //realloc2 can't actually free it (still uses the old list) so free it here
  }

  return (void *)VirtualAddress;
}

void* addPhysicalPagesToDBVM(QWORD address, int count, int inuse)
{
  int i;
  void* result=NULL;
  address=address & 0xfffffffffffff000ULL; //sanitize

  csEnter(&AllocCS);
  for (i=0; i<count; i++)
  {
    void* va=addPhysicalPageToDBVM(address+i*4096, inuse);
    if (i==0)
      result=va;
  }
  csLeave(&AllocCS);

  return result;
}

void mmAddPhysicalPageListToDBVM(QWORD *pagelist, int count, int inuse)
{
  int i;
  csEnter(&AllocCS);
  for (i=0; i<count; i++)
    addPhysicalPageToDBVM(pagelist[i], inuse);
  csLeave(&AllocCS);
}

QWORD getTotalFreeMemory(QWORD *FullPages)
//scans the allocationinfolist for the total number of 0's, and full 4KB blocks
{
  QWORD pages=0;
  QWORD total=0;
  int i;
  csEnter(&AllocCS);
  for (i=0; i<PhysicalPageListSize; i++)
  {
    if (AllocationInfoList[i].BitMask==0)
    {
      pages++;
      total+=4096;
    }
    else
      total+=(64-popcnt(AllocationInfoList[i].BitMask))*64;
  }

  csLeave(&AllocCS);

  if (FullPages)
    *FullPages=pages;

  return total;
}


void *malloc2(unsigned int size)
//scans the allocationinfolist for at least a specific number of subsequent 0-bits
//this version of malloc does not keep a list of alloc sizes, so use free2 and realloc2 for these
{
  UINT64 bitmask=0;
  int bitcount=size / 64;
  if (size % 64)
    bitcount++;

  if (bitcount==0)
    return NULL;

  if (bitcount<56) //64-8, if bigger the 8 bit shift would cause loss of data, or the bits would not be checked anyhow
  {
    //build a bitmask  ((2^bitcount)-1 will resul in the bitmask we need

    //todo: optimizations like indexes where empty blocks are

    int i;
    bitmask=1;
    for (i=1; i<bitcount; i++)
      bitmask=(bitmask << 1) | 1;

    //bitmask=bitmask-1;

    //now shift it through the list. If value & bitmask returns anything else than 0, then it's in use
    //todo: use those new string scan cpu functions

    csEnter(&AllocCS);

    unsigned char *list=(unsigned char*)AllocationInfoList;

    i=0;
    while (i<(int)(PhysicalPageListSize*sizeof(PageAllocationInfo)-8))
    {
      if (list[i]!=0xff) //if it's not completely full
      {
        UINT64 currentbm=bitmask;
        UINT64 p=*(UINT64*)&(list[i]);
        int j;
        for (j=0; j<8; j++)
        {
          if (p & currentbm)
            currentbm=currentbm << 1;
          else
          {
            //found a block
            //calculate the virtual address and mark it as used (or it with the bitmask)
            *(UINT64*)&(list[i]) |= currentbm;

            csLeave(&AllocCS);
            return (void *)((UINT64)(BASE_VIRTUAL_ADDRESS+i*(4096/8)+(j*64)));
          }
        }
      }

      i++;
    }

    csLeave(&AllocCS);
  }
  else
  {
    //just find a full 0 entry and go from there
    int i;
    int minpagecount=size / 4096;
    if (size%4096)
      minpagecount++;

    csEnter(&AllocCS);
    for (i=0; i<PhysicalPageListSize; i++)
    {
      int j;

      if (AllocationInfoList[i].BitMask==0) //found a free 4K block, check if the neighbours are free
      {
        int usable=1;
        int sizeleft=size-4096;

        for (j=i+1; usable && (sizeleft>0) ; j++)
        {
          if (AllocationInfoList[j].BitMask) //if not 0
          {
            usable=0;

            if ((j==i+minpagecount-1) && (sizeleft<4096)) //if this is the last page, and the last page is not a full 4096 then
            {
              //check if the first few blocks can fit
              bitcount=sizeleft / 64;
              if (sizeleft % 64)
                bitcount++;

              if (bitcount)
              {
                int x;
                bitmask=1;
                for (x=1; x<bitcount; x++)
                  bitmask=(bitmask << 1) | 1;
              }

              if ((bitmask & AllocationInfoList[i].BitMask)==0) //it is usable,the first parts of this page are not used
                usable=1;
            }

            break;
          }
          else
            sizeleft-=4096;
        }

        if (usable)
        {
          //mark it all as allocated
          int x=i;
          while (size>=4096)
          {
            AllocationInfoList[x].BitMask=0xffffffffffffffffULL;
            size-=4096;
            x++;
          }

          //last part of the range if it doesn't fill a full page
          if (size)
          {
            bitcount=size / 64;
            if (size % 64)
              bitcount++;
            if (bitcount)
            {
              int bc;
              bitmask=1;
              for (bc=1; bc<bitcount; bc++)
                bitmask=(bitmask << 1) | 1;
            }

            *(UINT64*)&(AllocationInfoList[x]) |= bitmask;
          }

          csLeave(&AllocCS);
          return (void *)(BASE_VIRTUAL_ADDRESS+i*4096);
        }
      }
    }

    csLeave(&AllocCS);

  }

  nosendchar[getAPICID()]=0;
  sendstring("OUT OF MEMORY\n");

#ifdef DEBUG
  while (1)
  {
    sendstring("OUT OF MEMORY\n");

  }

#endif
  return NULL; //still here so no memory allocated
}

void free2(void *address, unsigned int size)
{
  //unset the bits

  UINT64 bitmask;
  int bitcount;
  bitcount=size / 64;
  if (size % 64)
    bitcount++;
  if (bitcount)
  {
    UINT64 offset=(UINT64)address-BASE_VIRTUAL_ADDRESS;
    int index=offset / 4096;

    while (bitcount>=64) //size of 64+ are aligned on a page boundary so no need to bitfuck
    {
      AllocationInfoList[index].BitMask=0;
      bitcount-=64;
      index++;
    }

    if (bitcount)
    {
      //still some bits left
      int bitoffset=(offset % 4096) / 64;
      int i;
      bitmask=1;
      for (i=1; i<bitcount; i++)
        bitmask=(bitmask << 1) | 1;

      //shift the bitmask to the start of the allocation (only for small allocs where bitoffset is not 0)
      bitmask=bitmask << bitoffset;

      bitmask=~bitmask; //invert the bitmask  (so 000011110000 turns into 111100001111)

      csEnter(&AllocCS);
      AllocationInfoList[index].BitMask&=bitmask;
      csLeave(&AllocCS);
    }

  }


}

void *realloc2(void *oldaddress, unsigned int oldsize, unsigned int newsize)
{

  void *newaddress=malloc2(newsize);

  if (newaddress) //copy the contents of the old block to the new block
  {
    copymem(newaddress, oldaddress, min(oldsize,newsize));
    free2(oldaddress, oldsize);
  }

  return newaddress;
}

int findClosestAllocRegion(UINT64 address)
/*
 * Find the closest point in the alloc list (allocCS must already have been obtained)
 */
{
  int low=0;
  int high=AllocListPos-1;
  int mid;

  while (low <= high)
  {
    int diff = high - low;

    mid = low + diff / 2;

    if (address == AllocList[mid].base)
    {
      return mid;
    }
    else
      if (address>AllocList[mid].base)
        low = mid + 1;
      else
        high = mid - 1;
  }

  return low;
}

void *realloc(void *old, size_t size)
{
  if (old==NULL)
    return malloc(size);

  if (size==0)
    return NULL;

  csEnter(&AllocCS);
  int i=findClosestAllocRegion((UINT64)old);
  if ((i<AllocListPos) && (AllocList[i].size) && (AllocList[i].base==(UINT64)old) )
  {
    int j;
    int oldsize=AllocList[i].size;

    //allocate size
    void *result=malloc(size);
    asm volatile ("": : :"memory");
    //memset(result,0xff,size);
    asm volatile ("": : :"memory");

    //copy from old to result (oldsize bytes)
    copymem(result, old, min(size,oldsize));
    asm volatile ("": : :"memory");

    unsigned char *x,*y;
    x=old;
    y=result;
    for (j=0; j<min(oldsize,size); j++)
    {
      if (x[j]!=y[j])
      {
        sendstring("realloc failed\n");
        jtagbp();
      }

    }

    free(old);
    asm volatile ("": : :"memory");



    csLeave(&AllocCS);
    return result;
  }
  else
  {
    nosendchar[getAPICID()]=0;
    sendstringf("realloc error\n");
    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
    while (1) outportb(0x80,0xd1);
  }
}

void free(void *address)
{
  if (address)
  {
    csEnter(&AllocCS);
    int i=findClosestAllocRegion((UINT64)address);
    if ((i<AllocListPos) && (AllocList[i].size) && (AllocList[i].base==(UINT64)address) )
    {
      int size=AllocList[i].size;
      AllocList[i].size=0;

      //memset(address,0xfe, size);
      free2(address, size);

    }
    csLeave(&AllocCS);
  }



}

void *malloc(size_t size)
{
  //if (size<4096)  size=4096;

  UINT64 result=(UINT64)malloc2(size);

  if (result)
  {
    //add this alloc to the list

    csEnter(&AllocCS);
    if (AllocList==NULL)
    {
      //allocate the initial list
      AllocList=malloc2(sizeof(MemlistItem2)*64);
      AllocListMax=64;
      AllocListPos=1;
      AllocList[0].base=result;
      AllocList[0].size=size;
    }
    else
    {
      int insertpoint=findClosestAllocRegion(result);

      if (insertpoint<AllocListPos)
      {
        //shift may be needed
        if (AllocList[insertpoint].size==0)
        {
          AllocList[insertpoint].base=result;
          AllocList[insertpoint].size=size;
          csLeave(&AllocCS);

          return (void *)result;
        }

        //shift
        int i;
        for (i=AllocListPos; i>insertpoint; i--)
          AllocList[i]=AllocList[i-1];
      }

      //set insertpoint to this alloc
      AllocList[insertpoint].base=result;
      AllocList[insertpoint].size=size;

      AllocListPos++;

      if (AllocListPos>=AllocListMax)
      {
        //reallocate the list
        AllocList=realloc2(AllocList, AllocListMax*sizeof(MemlistItem2), AllocListMax*sizeof(MemlistItem2)*2);
        AllocListMax=AllocListMax*2;
      }
    }

    csLeave(&AllocCS);
  }

  return (void *)result;
}


void InitializeMM(UINT64 FirstFreeVirtualAddress)
{
  int pml4index;
  int pagedirptrindex;
  int pagedirindex;
  int pagetableindex;

  PPDPTE_PAE pml4entry;
  PPDPTE_PAE pagedirpointerentry;
  PPDE_PAE pagedirentry;
  PPTE_PAE pagetableentry;

  FirstFreeVirtualAddress=FirstFreeVirtualAddress & 0xfffffffffffff000ULL;


  sendstringf("Mapping the CR3 value at 0xffffff8000000000");
  VirtualAddressToIndexes(0xffffff8000000000ULL, &pml4index, &pagedirptrindex, &pagedirindex, &pagetableindex);

  if (pagedirlvl4[pml4index].P) //pml4index should be 511
  {
    nosendchar[getAPICID()]=0;
    sendstring("Assertion failed. pagedirlvl4[pml4index].P is not 0. It should be\n");
    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
    while (1) outportb(0x80,0xd1);
  }
  *(QWORD*)(&pagedirlvl4[pml4index])=getCR3();
  pagedirlvl4[pml4index].P=1;
  pagedirlvl4[pml4index].RW=1;
  asm volatile ("": : :"memory");
  _invlpg(0xffffff8000000000ULL);
  //now I have access to all the paging info


  sendstring("Allocating the AllocationInfoList\n");
  //allocate memory for AllocationInfoList and map it at BASE_VIRTUAL_ADDRESS
  VirtualAddressToPageEntries(BASE_VIRTUAL_ADDRESS, &pml4entry, &pagedirpointerentry, &pagedirentry, &pagetableentry);

  if (pml4entry->P==0)
  {
    //no pml4 entry (Normally not possible by my design, but could be BASE_VIRTUAL_ADDRESS has been edited by someone)
    zeromemory((void*)FirstFreeVirtualAddress, 4096);
    QWORD pagedirptrPA=VirtualToPhysical((void *)FirstFreeVirtualAddress);
    FirstFreeVirtualAddress+=4096;

    *(QWORD *)pml4entry=pagedirptrPA;
    pml4entry->P=1;
    pml4entry->RW=1;
    asm volatile ("": : :"memory");
    _invlpg((QWORD)pagedirpointerentry);
  }

  if (pagedirpointerentry->P==0)
  {
    zeromemory((void*)FirstFreeVirtualAddress, 4096);
    QWORD pagedirPA=VirtualToPhysical((void *)FirstFreeVirtualAddress);
    FirstFreeVirtualAddress+=4096;

    *(QWORD *)pagedirpointerentry=pagedirPA;
    pagedirpointerentry->P=1;
    pagedirpointerentry->RW=1;
    asm volatile ("": : :"memory");
    _invlpg((QWORD)pagedirentry);
  }

  if (pagedirentry->P==0)
  {
    zeromemory((void*)FirstFreeVirtualAddress, 4096);
    QWORD pagetablePA=VirtualToPhysical((void *)FirstFreeVirtualAddress);
    FirstFreeVirtualAddress+=4096;

    *(QWORD *)pagedirentry=pagetablePA;
    pagedirentry->P=1;
    pagedirentry->RW=1;
    asm volatile ("": : :"memory");
    _invlpg((QWORD)pagetableentry);
  }

  if (pagetableentry->P==0)
  {
    zeromemory((void*)FirstFreeVirtualAddress, 4096);
    QWORD AllocationInfoListPA=VirtualToPhysical((void *)FirstFreeVirtualAddress);
    FirstFreeVirtualAddress+=4096;

    *(QWORD *)pagetableentry=AllocationInfoListPA;
    pagetableentry->P=1;
    pagetableentry->RW=1;
    asm volatile ("": : :"memory");
    _invlpg(BASE_VIRTUAL_ADDRESS);
  }

  //entry 0x1000000000 contains 1 page (4096 bytes)
  //it has reserved bitmaps for 64 pages  (64*8=512 bytes)
  //the first entry should therefore mark the first 512 bytes as allocated according to the bitmap rules
  //(should be one entry long)

  //one bit represents 64-bytes in a page (so min alloc granularity is 64-bytes)
  sendstring("Configuring AllocationInfoList\n");
  int i;

  AllocationInfoList=(PageAllocationInfo *)BASE_VIRTUAL_ADDRESS;

  AllocationInfoList[0].BitMask=0x00000000000000ffULL;
  for (i=1; i<PhysicalPageListMaxSize; i++)
    AllocationInfoList[i].BitMask=0xffffffffffffffffULL;

  TotalAvailable=4096-512;

  //use VirtualToPhysical for every page

  UINT64 currentaddress=FirstFreeVirtualAddress;
  while (currentaddress<0x007fffff)
  {
    addPhysicalPageToDBVM(VirtualToPhysical((void *)currentaddress),0);
    currentaddress+=4096;
  }

  sendstringf("extramemorysize=%d\n",extramemorysize);

  if (extramemory && extramemorysize) //this is contiguous memory originally intended for allocs
  {
    if (contiguousmemoryPA==0) //no dedicated contiguous memory specified. Nibble some of the extra memory
    {
      int contiguousSize=extramemorysize>8?8:extramemorysize;

      extramemory-=contiguousSize;
      contiguousMemory=addPhysicalPagesToDBVM(extramemory, contiguousmemorysize, 1);
      contiguousMemoryPagesFree=contiguousmemorysize;

    }
    addPhysicalPagesToDBVM(extramemory, extramemorysize,0);
  }

  sendstringf("contiguousmemorysize=%d\n",contiguousmemorysize);
  if (contiguousmemoryPA)
  {
    contiguousMemory=addPhysicalPagesToDBVM(contiguousmemoryPA, contiguousmemorysize, 1);
    contiguousMemoryPagesFree=contiguousmemorysize;
  }


  sendstringf("InitializeMM finished\n");
  //todo: add indexes to free memory blocks

  //use VMCALL_ADD_PHYSICAL_MEMORY for more
}

PPDE_PAE getPageTableEntryForAddressEx(void *address, int allocateIfNotPresent)
{
  PPDPTE_PAE pml4, pdptr;
  PPDE_PAE pagedir;
  PPTE_PAE pagetable;
  VirtualAddressToPageEntries((QWORD)address, &pml4, &pdptr, &pagedir, &pagetable);

  if (pml4->P==0)
  {
    if (allocateIfNotPresent)
    {
      void *temp=malloc2(4096);
      zeromemory(temp,4096);
      *(QWORD *)pml4=VirtualToPhysical(temp);
      pml4->RW=1;
      pml4->P=1;
      asm volatile ("": : :"memory");
      _invlpg((QWORD)pdptr);
    }
    else
      return NULL;
  }

  if (pdptr->P==0)
  {
    if (allocateIfNotPresent)
    {
      void *temp=malloc2(4096);
      zeromemory(temp,4096);
      *(QWORD *)pdptr=VirtualToPhysical(temp);
      pdptr->RW=1;
      pdptr->P=1;
      asm volatile ("": : :"memory");
      _invlpg((QWORD)pagedir);
    }
    else
      return NULL;
  }

  if (pagedir->P==0)
  {
    if (allocateIfNotPresent)
    {
      void *temp=malloc2(4096);
      zeromemory(temp,4096);
      *(QWORD *)pagedir=VirtualToPhysical(temp);
      pagedir->RW=1;
      pagedir->P=1;
      asm volatile ("": : :"memory");
      _invlpg((QWORD)pagetable);
    }
    else
      return NULL;
  }

  if (pagedir->PS==1)
    return pagedir;
  else
    return (PPDE_PAE)pagetable;
}

PPDE_PAE getPageTableEntryForAddress(void *address)
/*
 * Gets the pagetable or pagedir entry describing this virtual address
 */
{
  return getPageTableEntryForAddressEx(address,0);
}


UINT64 VirtualToPhysical(void* address)
{
  PPDE_PAE pagedescriptor=getPageTableEntryForAddress(address);

  if (pagedescriptor==NULL)
    return 0xFFFFFFFFFFFFFFFFULL;

  UINT64 r;


  if (pagedescriptor->PS)
  {
    r=*(UINT64 *)pagedescriptor & 0xffffffffffe00000ULL;
    r=r | ((UINT64)address & 0x1fffff);
  }
  else
  {
    r=*(UINT64 *)pagedescriptor & 0xfffffffffffff000ULL;
    r=r | ((UINT64)address & 0xfff);
  }

  return r;
}

void mmtest(void)
{
  int i,i2,size,oldsize;
  unsigned char *s[64];

  //s=malloc(sizeof(unsigned char*)*64);
  zeromemory(s,sizeof(unsigned char*)*64);


  sendstringf("Testing normal malloc\n");
  for (i=0; i<64; i++)
  {
    size=i*3;
    s[i]=malloc(size);
    for (i2=0; i2<size; i2++)
      s[i][i2]=i;
  }

  for (i=0; i<64; i++)
  {
    size=i*3;
    for (i2=0; i2<size; i2++)
      if (s[i][i2]!=i)
      {
        sendstringf("Error in mallocs. %d\n", i);
        return;
      }
  }

  sendstringf("Normal malloc success\n");
  sendstringf("Testing realloc\n");

  for (i=0; i<64; i++)
  {
    unsigned char *olds=s[i];
    oldsize=i*3;
    asm volatile ("": : :"memory");
    size=i*4;
    asm volatile ("": : :"memory");

    s[i]=realloc(s[i],size);
    asm volatile ("": : :"memory");

    for (i2=0; i2<oldsize; i2++)
    {

      if (s[i][i2]!=i)
      {
        sendstringf("Error in realloc part1 s[%d][%d] was %d while it should be %d (olds=%p)\n",i,i2,s[i][i2],i,olds);

        return;
      }
    }

    for (i2=oldsize; i2<size; i2++)
      s[i][i2]=i;
  }

  for (i=0; i<64; i++)
  {
    size=i*4;
    for (i2=0; i2<size; i2++)
      if (s[i][i2]!=i)
      {

        sendstringf("Error in realloc part2 %d\n", i);
        return;
      }
  }

  sendstringf("Reallocs success\n");
  sendstringf("Testing free and realloc with 0\n");
  for (i=0;i<64; i+=2)
  {
    free(s[i]);
    s[i]=NULL;
  }

  for (i=0;i<64; i+=2)
  {
    size=i*4;
    s[i]=realloc(s[i],size);
    for (i2=0; i2<size; i2++)
      s[i][i2]=i;
  }

  for (i=0; i<64; i++)
  {
    size=i*4;
    for (i2=0; i2<size; i2++)
      if (s[i][i2]!=i)
      {
        jtagbp();
        sendstringf("Error in free/realloc(0). %d\n", i);
        return;
      }
  }

  sendstringf("Also OK\n");
}

void mmEnumAllPageEntries(MMENUMPAGESCALLBACK callbackfunction, int selfonly, void *context)
//walks all DBVM pagetables and calls the callbackfunction for each entry
{
  QWORD pml4index;
  QWORD pagedirptrindex;
  QWORD pagedirindex;
  QWORD pagetableindex;


  QWORD LastAddedPhysicalMemory=BASE_VIRTUAL_ADDRESS+4096*PhysicalPageListSize;

  for (pml4index=0; pml4index<512; pml4index++)
  {
    if (pml4table[pml4index].P)
    {
      QWORD s1=pml4index << 9;
      for (pagedirptrindex=s1;  pagedirptrindex<s1+512; pagedirptrindex++)
      {
        if (pagedirptrtables[pagedirptrindex].P) //DBVM does not use 1GB pages for virtual memory (yet)
        {
          QWORD s3=(pagedirptrindex << 9);
          for (pagedirindex=s3; pagedirindex<s3+512; pagedirindex++)
          {
            if (pagedirtables[pagedirindex].P)
            {
              QWORD VirtualAddress;
              QWORD PhysicalAddress;
              if (pagedirtables[pagedirindex].PS) //UEFI could have mapped it as 2MB pages
              {
                VirtualAddress=IndexesToVirtualAddress(pml4index, pagedirptrindex, pagedirindex, 0);
                if (VirtualAddress & ((QWORD)1<<47))
                  VirtualAddress|=0xffff000000000000;


                if ((selfonly==0) || (((VirtualAddress>=0x00400000) && (VirtualAddress<LastAddedPhysicalMemory)) ) )
                {
                  PhysicalAddress=*(QWORD*)(&pagedirtables[pagedirindex]) & MAXPHYADDRMASKPB;
                  callbackfunction(VirtualAddress, PhysicalAddress, 2*1024*1024, (PPTE_PAE)&pagedirtables[pagedirindex], context);
                }
              }
              else
              {
                QWORD s4=((QWORD)pagedirindex << 9);

                for (pagetableindex=s4; pagetableindex<s4+512; pagetableindex++)
                {
                  if (pagetables[pagetableindex].P)
                  {
                    VirtualAddress=IndexesToVirtualAddress(pml4index, pagedirptrindex, pagedirindex, pagetableindex);
                    if (VirtualAddress & ((QWORD)1<<47))
                      VirtualAddress|=0xffff000000000000;

                    if ((selfonly==0) || (((VirtualAddress>=0x00400000) && (VirtualAddress<LastAddedPhysicalMemory)) ) )
                    {
                      PhysicalAddress=*(QWORD*)(&pagetables[pagetableindex]) & MAXPHYADDRMASKPB;
                      callbackfunction(VirtualAddress, PhysicalAddress, 4096, (PPTE_PAE)&pagetables[pagetableindex], context);
                    }
                  }
                }
              }

            }
          }
        }
      }
    }
  }

}
