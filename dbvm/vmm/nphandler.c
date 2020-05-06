/*
 * nphandler.c
 *
 *  Created on: Apr 29, 2020
 *      Author: eric
 */

#include "vmmhelper.h"
#include "vmxsetup.h"
#include "mm.h"
#include "nphandler.h"
#include "epthandler.h"

QWORD NPMapPhysicalMemory(pcpuinfo currentcpuinfo, QWORD physicalAddress, int forcesmallpage)
/*
 * Maps the physical address into the NP map.
 * Returns the physical address of the NP entry describing this page
 */
{
  int pml4index;
  int pagedirptrindex;
  int pagedirindex;
  int pagetableindex;
  QWORD PA;
  VirtualAddressToIndexes(physicalAddress, &pml4index, &pagedirptrindex, &pagedirindex, &pagetableindex);


  PPML4 pml4=NULL;
  PPDPTE_PAE_BS pagedirptr=NULL;
  PPDE_PAE pagedir=NULL;
  PPTE_PAE pagetable=NULL;

  csEnter(&currentcpuinfo->EPTPML4CS);

  pml4=mapPhysicalMemory(currentcpuinfo->vmcb->N_CR3, 4096);
  if (pml4[pml4index].P==0)
  {
    sendstringf("allocating pagedirptr\n");
    //allocate a pagedirptr table
    void *temp=malloc2(4096);
    zeromemory(temp,4096);
    *(QWORD*)(&pml4[pml4index])=VirtualToPhysical(temp) & MAXPHYADDRMASKPB;
    pml4[pml4index].P=1;
    pml4[pml4index].RW=1;
    pml4[pml4index].US=1;
  }

  PA=(*(QWORD*)(&pml4[pml4index])) & MAXPHYADDRMASKPB;
  pagedirptr=mapPhysicalMemory(PA, 4096);

  PA+=8*pagedirptrindex;

  if (pml4)
  {
    unmapPhysicalMemory((void*)pml4, 4096);
    pml4=NULL;
  }

  if (forcesmallpage && pagedirptr[pagedirptrindex].P && (pagedirptr[pagedirptrindex].PS)) //it's a big page, so the physical address points to the actual memory. Clear everything
    *(QWORD *)&pagedirptr[pagedirptrindex]=0;

  if (pagedirptr[pagedirptrindex].P==0)
  {

    if (has_NP_1GBsupport && (forcesmallpage==0))
    {
      QWORD GuestAddress1GBAlign=(physicalAddress & 0xFFFFFFFFC0000000ULL) & MAXPHYADDRMASKPB;

      sendstringf("mapping %6 as a 1GB page\n", GuestAddress1GBAlign);
      *(QWORD*)(&pagedirptr[pagedirptrindex])=GuestAddress1GBAlign;
      pagedirptr[pagedirptrindex].P=1;
      pagedirptr[pagedirptrindex].RW=1;
      pagedirptr[pagedirptrindex].US=1;
      pagedirptr[pagedirptrindex].PS=1;
      unmapPhysicalMemory(pagedirptr, 4096);

      csLeave(&currentcpuinfo->EPTPML4CS);
      return PA;
    }

    //still here, try a pagedir
    void *temp=malloc2(4096);
    zeromemory(temp,4096);
    *(QWORD*)(&pagedirptr[pagedirptrindex])=VirtualToPhysical(temp) & MAXPHYADDRMASKPB;
    pagedirptr[pagedirptrindex].P=1;
    pagedirptr[pagedirptrindex].RW=1;
    pagedirptr[pagedirptrindex].US=1;
  }

  PA=(*(QWORD*)(&pagedirptr[pagedirptrindex])) & MAXPHYADDRMASKPB;
  pagedir=mapPhysicalMemory(PA, 4096);

  PA+=8*pagedirindex;

  if (pagedirptr)
  {
    unmapPhysicalMemory(pagedirptr, 4096);
    pagedirptr=NULL;
  }

  if (forcesmallpage && pagedir[pagedirindex].P && (pagedir[pagedirindex].PS)) //it's a big page, so the physical address points to the actual memory. Clear everything
    *(QWORD *)&pagedir[pagedirindex]=0;


  if (pagedir[pagedirindex].P==0)
  {
    if (has_NP_2MBsupport && (forcesmallpage==0))
    {
      QWORD GuestAddress2MBAlign=(physicalAddress & 0xFFFFFFFFFFE00000ULL) & MAXPHYADDRMASKPB;

      sendstringf("mapping %6 as a 2MB page\n", GuestAddress2MBAlign);
      *(QWORD*)(&pagedir[pagedirindex])=GuestAddress2MBAlign & MAXPHYADDRMASKPB;
      pagedir[pagedirindex].P=1;
      pagedir[pagedirindex].RW=1;
      pagedir[pagedirindex].US=1;
      pagedir[pagedirindex].PS=1;
      unmapPhysicalMemory(pagedir, 4096);

      csLeave(&currentcpuinfo->EPTPML4CS);
      return PA;

    }
    else
      sendstringf("Can't map as 2MB. has_NP_2MBsupport=%d and forcesmallpage=%d \n", has_NP_2MBsupport, forcesmallpage );

    //still here, try a pagetable
    void *temp=malloc2(4096);
    zeromemory(temp,4096);
    *(QWORD*)(&pagedir[pagedirindex])=VirtualToPhysical(temp) & MAXPHYADDRMASKPB;
    pagedir[pagedirindex].P=1;
    pagedir[pagedirindex].RW=1;
    pagedir[pagedirindex].US=1;
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

  if (pagetable[pagetableindex].P==0)
  {
    sendstringf("mapping %6 as a 4KB page\n", physicalAddress & MAXPHYADDRMASKPB);
    *(QWORD*)(&pagetable[pagetableindex])=physicalAddress & MAXPHYADDRMASKPB;
    pagetable[pagetableindex].P=1;
    pagetable[pagetableindex].RW=1;
    pagetable[pagetableindex].US=1;
  }
  else
  {
    //else already mapped
    sendstringf("This physical address (%6) was already mapped\n", physicalAddress);

    //change it to full access
    pagetable[pagetableindex].P=1;
    pagetable[pagetableindex].RW=1;
    pagetable[pagetableindex].US=1;
  }

  unmapPhysicalMemory(pagetable,4096);

  csLeave(&currentcpuinfo->EPTPML4CS);



  return PA;
}



VMSTATUS handleNestedPagingFault(pcpuinfo currentcpuinfo, VMRegisters *vmregisters UNUSED, PFXSAVE64 fxsave UNUSED)
{
  //handle the paging event

  nosendchar[getAPICID()]=0;
  QWORD PhysicalAddress=currentcpuinfo->vmcb->EXITINFO2;
  QWORD ErrorInfo=currentcpuinfo->vmcb->EXITINFO1;

  sendstringf("%x:%x handleNestedPagingFault. PA=%6 (Code %x)\n", currentcpuinfo->vmcb->cs_selector, currentcpuinfo->vmcb->RIP,  PhysicalAddress, ErrorInfo);
  sendstringf("EXITINTINFO=%x\n", currentcpuinfo->vmcb->EXITINTINFO);
  sendstringf("CR2=%6 vCR2=%6\n", getCR2(), currentcpuinfo->vmcb->CR2);

  if (ept_handleWatchEvent(currentcpuinfo, vmregisters, fxsave, PhysicalAddress))
    return 0;

  //map this physical address
  NPMapPhysicalMemory(currentcpuinfo, PhysicalAddress, 0);

  PEXITINTINFO eii=(PEXITINTINFO)&currentcpuinfo->vmcb->EXITINTINFO;


  if (eii->Valid)
  {
    sendstringf("Retriggering interrupt %d\n", eii->Vector);
    currentcpuinfo->vmcb->EVENTINJ=currentcpuinfo->vmcb->EXITINTINFO;
  }

  currentcpuinfo->vmcb->VMCB_CLEAN_BITS&=~(1 << 4);

  return VM_OK;
}
