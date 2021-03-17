#include "vmpaging.h"
#include "vmmhelper.h"
#include "common.h"
#include "mm.h"
#include "multicore.h"
#include "main.h"
#include "vmreadwrite.h"
#include "msrnames.h"
#include "vmxsetup.h"

unsigned int pagetablesize;

volatile void *nonpagedInvalidEmulationPagedir; //basicly one pagetable that maps a invalid cs state page (caused by entering realmode from protected mode above 1mb)


void unmapVMmemory(void *address, int size)
{
    unmapPhysicalMemory(address, size);
}

void * mapVMmemory(pcpuinfo currentcpuinfo, UINT64 address, int size, int *error, UINT64 *pagefaultaddress)
{
  return mapVMmemoryEx(currentcpuinfo, address, size, error, pagefaultaddress,0);
}

void * mapVMmemoryEx(pcpuinfo currentcpuinfo, UINT64 address, int size, int *error, UINT64 *pagefaultaddress, int donotunmaponfail )
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
  /*
  if (currentcpuinfo->virtualTLB == NULL)
    allocateVirtualTLB();
    */

  if (size==0)
  {
    *error=3;
    *pagefaultaddress=0;
    return NULL;
  }

  unsigned int offset=address & 0xfff;

  if (currentcpuinfo==NULL)
    currentcpuinfo=getcpuinfo();



  //take: 0x000000001017dfd0
  //size: 0x40

  //offset=fd0
  //totalsize=0x40+fd0=0x1010
  //pagecount=0x1010/0x1000=1
  //totalsize % 4096=0x10, so pagecount++= 2

  int totalsize=size+offset;
  int pagecount=(totalsize / 4096);
  int pml4index;
  int pagedirptrindex;
  int pagedirindex;
  int pagetableindex;
  VirtualAddressToIndexes(address, &pml4index, &pagedirptrindex, &pagedirindex, &pagetableindex);

  if (totalsize % 4096)
      pagecount++;


  int pos=mmFindMapPositionForSize(currentcpuinfo, totalsize);

  if (pos==-1)
  {
    if (error)
      *error=1;

    if (pagefaultaddress)
      *pagefaultaddress=0;
    return 0;
  }

  int i;
  int notpaged=0;

  void *VirtualAddress=(void *)(getMappedMemoryBase()+pos*4096+offset);

  address=address & 0xfffffffffffff000ULL;
  QWORD CurrentVirtualAddress=(QWORD)VirtualAddress & 0xfffffffffffff000ULL;

  for (i=0; i<pagecount; i++)
  {
    //sendstringf("mapVMmemory:\n  Mapping Guest virtual address %6\n", address);
    QWORD PhysicalAddress=getPhysicalAddressVM(currentcpuinfo,address, &notpaged);


    if (notpaged)
    {
      //error while mapping
      //sendstringf("  Which is currently not paged in\n");

      if (donotunmaponfail==0)
        unmapVMmemory(VirtualAddress, i*4096-offset);

      if (pagefaultaddress)
        *pagefaultaddress=address;

      if (error)
        *error=2;

      if (donotunmaponfail==0)
        return 0;
      else
        return VirtualAddress;
    }
    //sendstringf("  Which is located at physical address %6\n", PhysicalAddress);
    //sendstringf("  To Host virtual address %6\n", CurrentVirtualAddress);


    *(QWORD*)(&currentcpuinfo->mappagetables[pos])=PhysicalAddress;
    currentcpuinfo->mappagetables[pos].P=1;
    currentcpuinfo->mappagetables[pos].RW=1;
    currentcpuinfo->mappagetables[pos].US=1;

    asm volatile ("": : :"memory");
    _invlpg(CurrentVirtualAddress);
    asm volatile ("": : :"memory");

    pos++;
    address+=4096;
    CurrentVirtualAddress+=4096;
  }

  if (error)
    *error=0;

  if (pagefaultaddress)
    *pagefaultaddress=0;

  return VirtualAddress;
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
    if (hasUnrestrictedSupport)
      cr0=vmread(vm_guest_cr0);
    else
      cr0=vmread(vm_cr0_read_shadow);

  }

  UINT64  pagebase;


  //sendstringf("inside getPhysicalAddressVM , for address %6\n\r",address);
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
      if (hasUnrestrictedSupport)
      {
        //sendstring("realmode or nonpaged. Return same address");
        *notpaged=0;
        return address;
      }
      //sendstring("realmode or nonpaged, use emulated pagetable\n\r");
      pagebase=vmread(vm_guest_cr3); //realmode or nonpaged, so use the emulated nonpaged pagetable
    }
  }
#ifdef DEBUG
  if (pagebase==0xffffffffffffffffULL)
  {
    nosendchar[getAPICID()]=0;
    sendstringf("getPhysicalAddressVM for with cr3 of 0xffffffffffffffff\n");
    jtagbp();
    while (1) outportb(0x80,0xdd);
  }
#endif

  pagebase=pagebase & MAXPHYADDRMASKPB;

  //sendstringf("pagebase=%8\n\r",pagebase);
  if (IS64BITPAGING(currentcpuinfo))
  {
    UINT64   pml4entry=(UINT64)address >> 39 & 0x1ff;
    PPDE_PAE realpml4table=(PPDE_PAE)mapPhysicalMemory((UINT64)pagebase,4096);
    //sendstring("In 64-bit paging mode\n\r");


    if (realpml4table[pml4entry].P)
    {
      UINT64     pagedirptrentry=(UINT64)address >> 30 & 0x1ff;
      PPDE_PAE  realpagedirptrtable=(PPDE_PAE)mapPhysicalMemory(*(UINT64*)(&realpml4table[pml4entry]) & MAXPHYADDRMASKPB, 4096);
      //sendstring("realpml4table[pml4entry].P==1\n\r");

      if (realpagedirptrtable[pagedirptrentry].P)
      {
        PPDE_PAE realpagedirtable=(PPDE_PAE)mapPhysicalMemory(*(UINT64*)(&realpagedirptrtable[pagedirptrentry]) & MAXPHYADDRMASKPB,4096);
        UINT64    pagedirentry=(UINT64)address >> 21 & 0x1ff;
        //sendstring("realpagedirptrtable[pagedirptrentry].P==1\n\r");

        if (realpagedirtable[pagedirentry].P)
        {
          //sendstring("realpagedirtable[pagedirentry].P==1\n\r");
          if (realpagedirtable[pagedirentry].PS)
          {
            //sendstring("realpagedirtable[pagedirentry].PS==1\n\r");
            QWORD _address=(*(UINT64*)(&realpagedirtable[pagedirentry]) & MAXPHYADDRMASKPB )+ (UINT64)(address & 0x1FFFFF);
            unmapPhysicalMemory((void *)realpml4table, 4096);
            unmapPhysicalMemory((void *)realpagedirptrtable, 4096);
            unmapPhysicalMemory((void *)realpagedirtable, 4096);
            return _address;
          }
          else
          {
            UINT64    pagetableentry=address >> 12 & 0x1ff; //when it's not a big sized page
            PPTE_PAE realpagetable=(PPTE_PAE)mapPhysicalMemory(*(UINT64*)(&realpagedirtable[pagedirentry]) & MAXPHYADDRMASKPB,  4096);

            //sendstring("realpagedirtable[pagedirentry].PS==0\n\r");
            //sendstring("realpagetable[pagetableentry].P==1\n\r");

            if (realpagetable[pagetableentry].P)
            {
              QWORD _address=(*(UINT64*)(&realpagetable[pagetableentry]) & MAXPHYADDRMASKPB)+ (UINT64)(address & 0xFFF);
              unmapPhysicalMemory((void *)realpml4table, 4096);
              unmapPhysicalMemory((void *)realpagedirptrtable, 4096);
              unmapPhysicalMemory((void *)realpagedirtable, 4096);
              unmapPhysicalMemory((void *)realpagetable, 4096);
              return _address;
            }
            unmapPhysicalMemory((void *)realpagetable, 4096);
          }
        }
        unmapPhysicalMemory((void *)realpagedirtable, 4096);
      }
      unmapPhysicalMemory((void *)realpagedirptrtable, 4096);
    }
    unmapPhysicalMemory((void *)realpml4table, 4096);
  }
  else //32-bit
  if (usedCR4.PAE)
  {
    //in pae mode
    UINT64      pagedirptrentry=((UINT64)address >> 30) & 0x1ff;

    PPDPTE_PAE realpagedirptrtable=(PPDPTE_PAE)mapPhysicalMemory(pagebase,4096);

    if (realpagedirptrtable[pagedirptrentry].P)
    {
      UINT64    pagedirentry=((UINT64)address >> 21) & 0x1ff;
      PPDE_PAE realpagedirtable=(PPDE_PAE)mapPhysicalMemory(*(UINT64*)(&realpagedirptrtable[pagedirptrentry]) & MAXPHYADDRMASKPB,4096);

      if (realpagedirtable[pagedirentry].P)
      {

        if (realpagedirtable[pagedirentry].PS)
        {
          QWORD _address=(*(UINT64*)(&realpagedirtable[pagedirentry]) & MAXPHYADDRMASKPB) + ((UINT64)address & 0x1FFFFF);
          unmapPhysicalMemory((void *)realpagedirptrtable, 4096);
          unmapPhysicalMemory((void *)realpagedirtable, 4096);
          return _address;
        }
        else
        {
          UINT64    pagetableentry=(UINT64)address >> 12 & 0x1ff; //when it's not a big sized page

          PPTE_PAE realpagetable=(PPTE_PAE)mapPhysicalMemory(*(UINT64*)(&realpagedirtable[pagedirentry]) & MAXPHYADDRMASKPB, 4096);

          if (realpagetable[pagetableentry].P==1)
          {
            unmapPhysicalMemory((void *)realpagedirptrtable, 4096);
            unmapPhysicalMemory((void *)realpagedirtable, 4096);
            unmapPhysicalMemory((void *)realpagetable, 4096);
            return (*(UINT64*)(&realpagetable[pagetableentry]) & MAXPHYADDRMASKPB)+ (address & 0xFFF);
          }
          unmapPhysicalMemory((void *)realpagetable, 4096);
        }
      }
      unmapPhysicalMemory((void *)realpagedirtable, 4096);
    }
    unmapPhysicalMemory((void *)realpagedirptrtable, 4096);
  }
  else
  {
    //nonpae mode
    ULONG pagedirentry=address >> 22;
    PPDE  realpagedirtable=(PPDE)mapPhysicalMemory(pagebase, 4096);
    sendstring("Non-PAE mode\n\r");

    if (realpagedirtable[pagedirentry].P)
    {
      if (realpagedirtable[pagedirentry].PS)
      {
        //big page
        QWORD _address=(*(UINT64*)(&realpagedirtable[pagedirentry]) & MAXPHYADDRMASKPB)+ (address & 0x3FFFFF);
        unmapPhysicalMemory((void *)realpagedirtable, 4096);
        return _address;
      }
      else
      {
        ULONG pagetableentry=(UINT64)address >> 12 & 0x3ff; //when it's not a big sized page
        PPTE  realpagetable=(PPTE)mapPhysicalMemory(*(UINT64*)(&realpagedirtable[pagedirentry]) & MAXPHYADDRMASKPB, 4096);

        //it has a pagetable
        if (realpagetable[pagetableentry].P==1)
        {
          QWORD _address=(*(UINT64*)(&realpagetable[pagetableentry]) & MAXPHYADDRMASKPB)+ (address & 0xFFF);
          unmapPhysicalMemory((void *)realpagetable, 4096);
          return _address;
        }
        unmapPhysicalMemory((void *)realpagetable, 4096);
      }
    }
    unmapPhysicalMemory((void *)realpagedirtable, 4096);
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
    //map this physical address
    unsigned char *tempbuf=(unsigned char *)mapPhysicalMemory(physicalAddress, 4096);
    int leftforthispage=0x1000-(address & 0xfff);

    if (leftforthispage>size)
      leftforthispage=size;

    //copy the data to the buffer
    for (i=0; i<leftforthispage; i++)
      buf[i]=tempbuf[i];

    unmapPhysicalMemory(tempbuf, 4096);

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


int handleINVLPG(pcpuinfo currentcpuinfo UNUSED)
{
  //INVVPID and afterwards disable invlpg exiting on systems that do not need it
  vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));
  return 0;
}




int emulatePaging(pcpuinfo currentcpuinfo)
/*
Called when paging is set from 0 to 1
Called when CR3 is written to
Called when the pagetable is written to
*/
{
  //regCR0 fakeCR0;
  //regCR4 fakeCR4;

  //fakeCR0.CR0=vmread(0x6004);
  //fakeCR4.CR4=vmread(0x6006);

 	//read the guest's CR3 and use that to emulate the pagetable
  sendstring("emulatePaging\n");

  if  ((vmread(vm_cr0_read_shadow) & 0x80000001)==0x80000001) //is in paging/protected mode according to the guest?
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
    sendstringf("Not in paging and protected mode. vm_cr0_fakeread=%6\n",vmread(vm_cr0_read_shadow));

  }


  return 0;

}



int setupRealModePaging(pcpuinfo currentcpuinfo)
/* called on switch to realmode and when the a20 line is modified
 * This is always 32-bit */
{
  return setupNonPagedPaging(currentcpuinfo);
}

criticalSection setupNonPagedPagingCS={.name="setupNonPagedPagingCS", .debuglevel=2};
int setupNonPagedPaging(pcpuinfo currentcpuinfo)
{
//sets up a identify mapped memory (faking the vmm memory as ffpages)
//uses PAE so set CR4 to pae mode

  //UINT64     pfn_vmmstart=vmmstart >> 12;
  //UINT64     pfn_vmmstop=(vmmstart+0x00400000) >> 12;
  if (hasUnrestrictedSupport)
    return 0;

  int        is64bitpaging=IS64BITPAGING(currentcpuinfo);

	unsigned int i;

	//todo: check out 1GB pages


  if (nonpagedEmulationPagedir==NULL)
  {
    csEnter(&setupNonPagedPagingCS);
    if (nonpagedEmulationPagedir==NULL) //check if still NULL
    {
      PPDPTE_PAE VirtualMachinePageDirPointer;
      PPDE_PAE   VirtualMachinePageDir;

      sendstringf("Setting up protected mode paging for nonpaged emu\n\r");

      nonpagedEmulationPagedir=malloc(4096+4096+4096*8); //pml4+pagedirpointer + pagedirs to map 00000000 till 1ffffffff, since one entry maps 00200000 bytes
      zeromemory(nonpagedEmulationPagedir,4096+4096+4096*8);

      PPDE_PAE pml4table=nonpagedEmulationPagedir;
      VirtualMachinePageDirPointer=(PPDPTE_PAE)(((QWORD)nonpagedEmulationPagedir)+0x1000);
      nonpagedEmulationPagedir32BitPAE=(void *)VirtualMachinePageDirPointer;


      *(QWORD *)(&pml4table[0])=VirtualToPhysical((void *)VirtualMachinePageDirPointer);
      pml4table[0].P=1;
      pml4table[0].RW=1;

      VirtualMachinePageDir=(PPDE_PAE)(((QWORD)VirtualMachinePageDirPointer)+0x1000);
      nonpagedEmulationPagedir32Bit=(void *)VirtualMachinePageDir;

      sendstringf("VirtualMachinePML4=%6\n\r",(UINT64)pml4table);
      sendstringf("VirtualMachinePageDirPointer=%6\n\r",(UINT64)VirtualMachinePageDirPointer);
      sendstringf("VirtualMachinePageDir=%6\n\r",(UINT64)VirtualMachinePageDir);

      for (i=0; i<8; i++)
      {
        *(QWORD*)(&VirtualMachinePageDirPointer[i])=VirtualToPhysical((void *)((UINT64)VirtualMachinePageDir+(i*4096)));
        VirtualMachinePageDirPointer[i].P=1;
        if (is64bitpaging)
          VirtualMachinePageDirPointer[i].RW=1;
      }

      for (i=0; i<4096; i++)  //512 in one pagedir, 8 pagedirs=4096 entries
      {
        //identity map these regions (easy when switching to protected mode)
        *(QWORD *)(&VirtualMachinePageDir[i])=i*0x00200000;
        VirtualMachinePageDir[i].P=1;
        VirtualMachinePageDir[i].A=0;
        VirtualMachinePageDir[i].RW=1;
        VirtualMachinePageDir[i].US=1; //ring3 access
        VirtualMachinePageDir[i].PS=1; //no pagetable

        VirtualMachinePageDir[i].PCD=0;
        VirtualMachinePageDir[i].PWT=0;
      }

      nonpagedEmulationPagedirPA=(UINT64)VirtualToPhysical((void *)nonpagedEmulationPagedir);
      nonpagedEmulationPagedir32BitPAEPA=(UINT64)VirtualToPhysical((void *)nonpagedEmulationPagedir32BitPAE);
      nonpagedEmulationPagedir32BitPA=(UINT64)VirtualToPhysical((void *)nonpagedEmulationPagedir32Bit);


    }
    csLeave(&setupNonPagedPagingCS);
  }
  sendstring("Setting up nonpaged paging for ");

  if (is64bitpaging) //eg the msr has been set, but not really switched yet, entering this state (gets called on msr edit)
  {
    sendstring("64-bit mode\n");
    vmwrite(vm_guest_cr3,nonpagedEmulationPagedirPA); //set guest's cr3 to this address (pml4)
  }
  else
  {
    //check if PAE paging
    if (vmread(vm_guest_cr4) & CR4_PAE)
    {
      sendstring("32-bit PAE mode\n");
      vmwrite(vm_guest_cr3, nonpagedEmulationPagedir32BitPAEPA);  //32-bit PAE, start one level lower (pagedirptr)
    }
    else
    {
      sendstring("32-bit mode\n");
      vmwrite(vm_guest_cr3,nonpagedEmulationPagedir32BitPA);  //32-bit non-PAE, start two levels lower (pagedir)
    }
  }

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
    *(QWORD*)(&pml4table[0])=VirtualToPhysical((void *)nonpagedInvalidEmulationPagedir);
    pml4table[0].P=1;
    pml4table[0].RW=1;
    sendstring("in 64-bit mode paging, shouldn\'t be used, since it\'s only enabled when paging is on");
  }
  else
    VirtualMachinePageDirPointer=nonpagedInvalidEmulationPagedir;

  VirtualMachinePageDir=(void *)((UINT64)VirtualMachinePageDirPointer+0x1000);
  VirtualMachinePageTable=(void *)((UINT64)VirtualMachinePageDir+0x1000);

  sendstringf("VirtualMachinePageDirPointer=%6\n\r",(UINT64)VirtualMachinePageDirPointer);
  sendstringf("VirtualMachinePageDir=%6\n\r",(UINT64)VirtualMachinePageDir);

  *(QWORD*)(&VirtualMachinePageDirPointer[0])=VirtualToPhysical((void *)VirtualMachinePageDir);
  VirtualMachinePageDirPointer[0].P=1;
  if (is64bitpaging)
    VirtualMachinePageDirPointer[0].RW=1;



  *(QWORD*)(&VirtualMachinePageDir[0])=VirtualToPhysical((void *)VirtualMachinePageTable);
  VirtualMachinePageDir[0].P=1;
  VirtualMachinePageDir[0].A=0;
  VirtualMachinePageDir[0].RW=1;
  VirtualMachinePageDir[0].US=1;
  VirtualMachinePageDir[0].PS=0; //has pagetable
  VirtualMachinePageDir[0].PCD=0;
  VirtualMachinePageDir[0].PWT=0;


  *(QWORD*)(&VirtualMachinePageTable[0xc0])=cs_base;
  VirtualMachinePageTable[0xc0].P=1;
  VirtualMachinePageTable[0xc0].A=0;
  VirtualMachinePageTable[0xc0].RW=1;
  VirtualMachinePageTable[0xc0].US=1;
  VirtualMachinePageTable[0xc0].PCD=0;
  VirtualMachinePageTable[0xc0].PWT=0;

  vmwrite(vm_guest_cr3,(UINT64)VirtualToPhysical((void *)nonpagedInvalidEmulationPagedir)); //set guest's cr3 (real one) to this address

  nosendchar[getAPICID()]=orig;

  return 0;
}

