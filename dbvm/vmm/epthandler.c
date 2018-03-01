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

/*
 * example mtrr
base: 0x00000000BD000006  ==  10111101000000000000000000000110
mask: 0x00000000FF800800  ==  11111111100000000000100000000000


base: 0x00000000C0000000
mask: 0x0000007FC0000800

base: 0x00000000BE000000
mask: 0x0000007FFE000800

ex from book:
IA32_MTRR_PHYSBASE1 = 0000000004000006 == 000000000100000000000000000000000110
IA32_MTRR_PHYSMASK1 = 0000000FFE000800 == 111111111110000000000000000000000000

 */

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



int remapMTRRTypes(QWORD address, QWORD size, int type)
{
  //called by the MSR write handler when MTRR registers get changed
  return 1;
}

int handleMSRWrite_MTRR(void)
//called when an MTRR msr is written. Figures out what regions have been modified
{
  return 1;
}

int handleEPTViolation(pcpuinfo currentcpuinfo, VMRegisters *vmregisters UNUSED)
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
  sendstringf("GuestAddress=%6\n",GuestAddress);

  int pml4index;
  int pagedirptrindex;
  int pagedirindex;
  int pagetableindex;
  VirtualAddressToIndexes(GuestAddress, &pml4index, &pagedirptrindex, &pagedirindex, &pagetableindex);

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

  pagedirptr=mapPhysicalMemory((*(QWORD*)(&pml4[pml4index])) & MAXPHYADDRMASKPB, 4096);

  if (pml4)
  {
    unmapPhysicalMemory(pml4, 4096);
    pml4=NULL;
  }

  if (pagedirptr[pagedirptrindex].RA==0)
  {
    //check if there is a MTRR in this 1GB range that doesn't set the MTRRDefType.TYPE, if not, map as 1GB using deftype
    if (has_EPT_1GBsupport)
    {
      QWORD GuestAddress1GBAlign=(GuestAddress & 0xFFFFFFFFC0000000ULL) & MAXPHYADDRMASKPB;
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
        return 0;
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


  if (pagedirptr[pagedirptrindex].BIG)
  {
    sendstring("Assertion Fail: pagedirptr[pagedirptrindex].BIG==1");
    while (1);
  }



  pagedir=mapPhysicalMemory((*(QWORD*)(&pagedirptr[pagedirptrindex])) & MAXPHYADDRMASKPB, 4096);

  if (pagedirptr)
  {
    unmapPhysicalMemory(pagedirptr, 4096);
    pagedirptr=NULL;
  }

  if (pagedir[pagedirindex].RA==0)
  {
    if (has_EPT_2MBSupport)
    {
      QWORD GuestAddress2MBAlign=(GuestAddress & 0xFFFFFFFFFFE00000ULL) & MAXPHYADDRMASKPB;
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
        return 0;
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
  if (pagedir[pagedirindex].BIG)
  {
    sendstring("Assertion Fail: pagedir[pagedirindex].BIG==1");
    while (1);
  }

  pagetable=mapPhysicalMemory((*(QWORD*)(&pagedir[pagedirindex])) & MAXPHYADDRMASKPB, 4096);
  if (pagedir)
  {
    unmapPhysicalMemory(pagedir,4096);
    pagedir=NULL;
  }

  if (pagetable[pagetableindex].RA==0)
  {
    int memtype, fullmap;
    getMTRRMapInfo(GuestAddress & MAXPHYADDRMASKPB,0x1000, &fullmap, &memtype);
    if (!fullmap)
    {
      sendstring("Assertion Fail: fullmap is false for a 1 page range");
      while (1);
    }

    sendstringf("mapping %6 as a 4KB page with memtype %d\n", GuestAddress & MAXPHYADDRMASKPB, memtype);
    *(QWORD*)(&pagetable[pagetableindex])=GuestAddress & MAXPHYADDRMASKPB;
    pagetable[pagetableindex].RA=1;
    pagetable[pagetableindex].WA=1;
    pagetable[pagetableindex].XA=1;
    pagetable[pagetableindex].MEMTYPE=memtype;
  }

  unmapPhysicalMemory(pagetable,4096);
  return 0;

}

int handleEPTMisconfig(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{
  sendstring("handleEPTMisconfig\n");
  return 1;
}

