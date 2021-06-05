/*
 * epthandler.c
 *
 *  Created on: Feb 2, 2018
 *      Author: erich
 */

#define MAXCLOAKLISTBEFORETRANFERTOMAP 40

//#define MEMORYCHECK

#include "epthandler.h"
#include "main.h"
#include "mm.h"
#include "vmxsetup.h"
#include "msrnames.h"
#include "common.h"
#include "vmpaging.h"
#include "vmcall.h"
#include "maps.h"
#include "list.h"
#include "vmeventhandler.h"
#include "displaydebug.h"
#include "nphandler.h"

void recordState(void *liststart, int datatype, int currentEntryNr, pcpuinfo currentcpuinfo, VMRegisters *vmregisters, PFXSAVE64 fxsave);
void fillPageEventBasic(PageEventBasic *peb, VMRegisters *registers);

EPTWatchLogData lastSeenEPTWatch; //debugging ept
EPTWatchLogData lastSeenEPTWatchVerySure;


QWORD EPTMapPhysicalMemory(pcpuinfo currentcpuinfo, QWORD physicalAddress, int forcesmallpage);

criticalSection eptWatchListCS={.name="eptWatchListCS", .debuglevel=2};
PEPTWatchEntry eptWatchList;
int eptWatchListSize;
int eptWatchListPos;


criticalSection CloakedPagesCS={.name="CloakedPagesCS", .debuglevel=2}; //1
PAddressList CloakedPagesList; //up to 40 entries can be found in 5 steps (worst case scenario)
PMapInfo CloakedPagesMap; //can be found in 5 steps, always (and eats memory) , so if CloakedPagesPos>40 then start using this (and move the old list over)
//todo: Create a MapList object that combines both into one




criticalSection ChangeRegBPListCS={.name="ChangeRegBPListCS", .debuglevel=2}; //2
ChangeRegBPEntry *ChangeRegBPList;
int ChangeRegBPListSize;
int ChangeRegBPListPos;

TraceOnBPEntry *TraceOnBP; //not NULL when active. (There can be only one active one at a time globally, as the trap flag can switch between cpu's)


criticalSection BrokenThreadListCS={.name="BrokenThreadListCS", .debuglevel=2}; //2
BrokenThreadEntry *BrokenThreadList;
int BrokenThreadListSize;
int BrokenThreadListPos;


#ifdef MEMORYCHECK
int checkmem(unsigned char *x, int len)
{
  int i;
  for (i=0; i<len; i++)
    if (x[i]!=0xce)
      return 1;

  return 0;

}
#endif

void vpid_invalidate()
{
  INVVPIDDESCRIPTOR vpidd;
  vpidd.zero=0;
  vpidd.LinearAddress=0;
  vpidd.VPID=1;

  _invvpid(2, &vpidd);
}

void ept_invalidate()
{
  if (isAMD)
  {
    pcpuinfo c=getcpuinfo();
    c->vmcb->VMCB_CLEAN_BITS&=~(1 << 4);
    c->vmcb->TLB_CONTROL=1;


  }
  else
  {
    //Intel
    INVEPTDESCRIPTOR eptd;
    eptd.Zero=0;
    eptd.EPTPointer=getcpuinfo()->EPTPML4;

    if (has_EPT_INVEPTAllContext)
    {
      _invept(2, &eptd);
    }
    else
    if (has_EPT_INVEPTSingleContext)
    {
      _invept(1, &eptd);
    }
    else
    {
      _invept(2, &eptd);//fuck it
    }
  }

	//vpid_invalidate();
}


int shownfirst=0;
void ept_hideDBVMPhysicalAddresses_callbackIntel(QWORD VirtualAddress UNUSED, QWORD PhysicalAddress, int size UNUSED, PPTE_PAE entry UNUSED, pcpuinfo currentcpuinfo)
{

  if (shownfirst==0)
  {
    sendstringf("%6", PhysicalAddress);
    shownfirst=1;
  }


  QWORD eptentryAddress=EPTMapPhysicalMemory(currentcpuinfo,PhysicalAddress,1);
  PEPT_PTE eptentry=mapPhysicalMemory(eptentryAddress,8);

  eptentry->PFN=MAXPHYADDRMASK >> 13; //unallocated memory (using 13 as sometimes accessing the most significant bit of the allowed PA will crash a system)
  eptentry->MEMTYPE = 0;

  unmapPhysicalMemory(eptentry,8);

  ept_invalidate();
  currentcpuinfo->eptUpdated=1;
}

void ept_hideDBVMPhysicalAddresses_callbackAMD(QWORD VirtualAddress UNUSED, QWORD PhysicalAddress, int size UNUSED, PPTE_PAE entry UNUSED, pcpuinfo currentcpuinfo)
{
  QWORD npentryAddress=NPMapPhysicalMemory(currentcpuinfo,PhysicalAddress,1);
  PPTE_PAE npentry=mapPhysicalMemory(npentryAddress,8);
  npentry->PFN=MAXPHYADDRMASK >> 13;

  unmapPhysicalMemory((void *)npentry,8);

  ept_invalidate();
}


void ept_hideDBVMPhysicalAddresses(pcpuinfo currentcpuinfo)
{
  shownfirst=0;

  nosendchar[getAPICID()]=0;
  sendstringf("  ept_hideDBVMPhysicalAddresses()\n");
  MMENUMPAGESCALLBACK callback=isAMD?(MMENUMPAGESCALLBACK)ept_hideDBVMPhysicalAddresses_callbackAMD:(MMENUMPAGESCALLBACK)ept_hideDBVMPhysicalAddresses_callbackIntel;
  csEnter(&currentcpuinfo->EPTPML4CS);
  sendstringf("    Calling mmEnumAllPageEntries\n");
  mmEnumAllPageEntries(callback, 1, (void*)currentcpuinfo);
  sendstringf("    Returned from mmEnumAllPageEntries\n");
  csLeave(&currentcpuinfo->EPTPML4CS);
}

void ept_hideDBVMPhysicalAddressesAllCPUs()
//walk the dbvm pagetables and map each physical address found to a random address until VA BASE_VIRTUAL_ADDRESS+4096*PhysicalPageListSize;
//todo: If for some reason this takes too long and triggers a timeout, switch to per cpu
{

  nosendchar[getAPICID()]=0;
  sendstringf("ept_hideDBVMPhysicalAddressesAllCPUs()\n");

  pcpuinfo c=firstcpuinfo;

  while (c)
  {
    sendstringf("cpu %d:\n", c->cpunr);

    ept_hideDBVMPhysicalAddresses(c);
    c=c->next;
  }

  sendstringf("done\n");
}


void ept_reset_cb(QWORD address, void *data UNUSED)
{
  map_setEntry(CloakedPagesMap, address, NULL);
}

void ept_reset()
/*
 * Removes all watches/breakpoints
 */
{
  int i;
  csEnter(&eptWatchListCS);
  for (i=0; i<eptWatchListPos; i++)
    if (eptWatchList[i].Active)
      ept_watch_deactivate(i);

  csLeave(&eptWatchListCS);

  csEnter(&ChangeRegBPListCS);
  for (i=0; i<ChangeRegBPListPos; i++)
    if (ChangeRegBPList[i].Active)
      ept_cloak_removechangeregonbp(ChangeRegBPList[i].PhysicalAddress);

  csLeave(&ChangeRegBPListCS);

  csEnter(&CloakedPagesCS);

  if (CloakedPagesMap)
    map_foreach(CloakedPagesMap,ept_reset_cb);
  else
  if (CloakedPagesList)
  {
    while (CloakedPagesList->size)
      ept_cloak_deactivate(CloakedPagesList->list[0].address);
  }

  csLeave(&CloakedPagesCS);

}

BOOL ept_handleCloakEvent(pcpuinfo currentcpuinfo, QWORD Address, QWORD AddressVA)
/*
 * Checks if the physical address is cloaked, if so handle it and return 1, else return 0
 */
{
  int result=0;
  QWORD BaseAddress=Address & MAXPHYADDRMASKPB;
  PCloakedPageData cloakdata;

  if ((CloakedPagesList==NULL) && (CloakedPagesMap==NULL))
    return FALSE;

  if (isAMD) //AMD marks the page as no-execute only, no read/write block
  {
    NP_VIOLATION_INFO nvi;
    nvi.ErrorCode=currentcpuinfo->vmcb->EXITINFO1;
    if (nvi.ID==0)
      return FALSE; //not an execute pagefault. Not a cloak for AMD

    if (currentcpuinfo->NP_Cloak.ActiveRegion)
    {

      cloakdata=currentcpuinfo->NP_Cloak.ActiveRegion;
     // sendstringf("Inside a cloaked region using mode 1 (which started in %6) and an execute fault happened (CS:RIP=%x:%6)\n", currentcpuinfo->NP_Cloak.LastCloakedVirtualBase, currentcpuinfo->vmcb->cs_selector, currentcpuinfo->vmcb->RIP);

      //means we exited the cloaked page (or at the page boundary)

      csEnter(&CloakedPagesCS);
      NPMode1CloakSetState(currentcpuinfo, 0); //marks all pages back as executable and the stealthed page(s) back as no execute
      csLeave(&CloakedPagesCS);

      //check if it's a page boundary
     // QWORD currentexecbase=currentcpuinfo->vmcb->RIP & 0xffffffffffff000ULL;

      if  (((currentcpuinfo->vmcb->RIP<currentcpuinfo->NP_Cloak.LastCloakedVirtualBase) &&
          ((currentcpuinfo->vmcb->RIP+32)>=currentcpuinfo->NP_Cloak.LastCloakedVirtualBase))
        ||
        ((currentcpuinfo->vmcb->RIP>=currentcpuinfo->NP_Cloak.LastCloakedVirtualBase+4096) &&
         (currentcpuinfo->vmcb->RIP<=currentcpuinfo->NP_Cloak.LastCloakedVirtualBase+4096+32)))
      {
        sendstringf("Pageboundary. Do a single step with the cloaked page decloaked\n");

        //page boundary. Do a single step with the cloaked page executable
        *(QWORD *)(cloakdata->npentry[currentcpuinfo->cpunr])=cloakdata->PhysicalAddressExecutable;
        cloakdata->npentry[currentcpuinfo->cpunr]->P=1;
        cloakdata->npentry[currentcpuinfo->cpunr]->RW=1;
        cloakdata->npentry[currentcpuinfo->cpunr]->US=1;
        cloakdata->npentry[currentcpuinfo->cpunr]->EXB=0;

        vmx_enableSingleStepMode();
        vmx_addSingleSteppingReasonEx(currentcpuinfo, 2,cloakdata);
      }

      currentcpuinfo->NP_Cloak.ActiveRegion=NULL;
      ept_invalidate();


      return TRUE;
    }
  }


  csEnter(&CloakedPagesCS);
  if (CloakedPagesMap)
    cloakdata=map_getEntry(CloakedPagesMap, BaseAddress);
  else
    cloakdata=addresslist_find(CloakedPagesList, BaseAddress);

  if (cloakdata)
  {
    csEnter(&currentcpuinfo->EPTPML4CS);


    //it's a cloaked page
    //sendstringf("ept_handleCloakEvent on the target(CS:RIP=%x:%6)\n", currentcpuinfo->vmcb->cs_selector, currentcpuinfo->vmcb->RIP);

    if (isAMD)
    {
      //AMD handling
      //swap the page and make it executable,
      *(QWORD *)(cloakdata->npentry[currentcpuinfo->cpunr])=cloakdata->PhysicalAddressExecutable;
      cloakdata->npentry[currentcpuinfo->cpunr]->P=1;
      cloakdata->npentry[currentcpuinfo->cpunr]->RW=1;
      cloakdata->npentry[currentcpuinfo->cpunr]->US=1;
      cloakdata->npentry[currentcpuinfo->cpunr]->EXB=0;

      if (cloakdata->CloakMode==0)
      {
        //do one step, and restore back to non-executable
        vmx_enableSingleStepMode();
        vmx_addSingleSteppingReasonEx(currentcpuinfo, 2,cloakdata);
      }
      else
      {
        //mark all pages as no execute except this one (and any potential still waiting to step cloak events)
        currentcpuinfo->NP_Cloak.ActiveRegion=cloakdata;
        currentcpuinfo->NP_Cloak.LastCloakedVirtualBase=currentcpuinfo->vmcb->RIP & 0xffffffffffff000ULL;
        NPMode1CloakSetState(currentcpuinfo, 1);
      }

    }
    else
    {
      //Intel handling
      EPT_VIOLATION_INFO evi;
      evi.ExitQualification=vmread(vm_exit_qualification);

      int isMegaJmp=0;
      QWORD RIP;

      if (!isAMD)
        RIP=vmread(vm_guest_rip);

      //todo: keep a special list for physical address regions that can see 'the truth' (e.g ntoskrnl.exe and hal.dll on exported data pointers, but anything else will see the fake pointers)
      //todo2: inverse cloak, always shows the real data except the list of physical address regions provided

      //check for megajmp edits
      //megajmp: ff 25 00 00 00 00 <address>
      //So, if 6 bytes before the given address is ff 25 00 00 00 00 , it's a megajmp, IF the RIP is 6 bytes before the given address (and the bytes have changed from original)

      if ((AddressVA-RIP)==6)
      {
        //check if the bytes have been changed here
        DWORD offset=RIP & 0xfff;
        int size=min(14,0x1000-offset);

        unsigned char *new=(unsigned char *)((QWORD)cloakdata->Executable+offset);
        unsigned char *original=(unsigned char *)((QWORD)cloakdata->Data+offset);


        if (new[0]==0xff) //starts with 0xff, so very likely, inspect more
        {
          if (memcmp(new, original, size))
          {
            //the memory in this range got changed, check if it's a full megajmp
            unsigned char megajmpbytes[6]={0xff,0x25,0x00,0x00,0x00,0x00};

            if (memcmp(new, megajmpbytes, min(6,size))==0)
            {
              sendstring("Is megajmp");
              isMegaJmp=1;
            }

          }
        }
      }



      //Check if this page has had a MEGAJUMP code edit, if so, check if this is a megajump and in that case on executable
      if (evi.X) //looks like this cpu does not support execute only
        *(QWORD *)(cloakdata->eptentry[currentcpuinfo->cpunr])=cloakdata->PhysicalAddressExecutable;
      else
      {
        if (isMegaJmp==0)
        {
          //read/write the data
          *(QWORD *)(cloakdata->eptentry[currentcpuinfo->cpunr])=cloakdata->PhysicalAddressData;
        }
        else
        {
          //read the executable code
          *(QWORD *)(cloakdata->eptentry[currentcpuinfo->cpunr])=cloakdata->PhysicalAddressExecutable;
        }

      }
      cloakdata->eptentry[currentcpuinfo->cpunr]->WA=1;
      cloakdata->eptentry[currentcpuinfo->cpunr]->RA=1;
      cloakdata->eptentry[currentcpuinfo->cpunr]->XA=1;

      vmx_enableSingleStepMode();
      vmx_addSingleSteppingReasonEx(currentcpuinfo, 2,cloakdata);

      currentcpuinfo->eptCloak_LastOperationWasWrite=evi.W;
      currentcpuinfo->eptCloak_LastWriteOffset=Address & 0xfff;
    }



    result=TRUE;



    csLeave(&currentcpuinfo->EPTPML4CS);
  }

  //still here so not in the map (or no map used yet)

  csLeave(&CloakedPagesCS);

  ept_invalidate();


  return result;
}

int ept_handleCloakEventAfterStep(pcpuinfo currentcpuinfo,  PCloakedPageData cloakdata)
{
  sendstringf("ept_handleCloakEventAfterStep\n");
  //back to execute only
  csEnter(&CloakedPagesCS);

  if (currentcpuinfo->eptCloak_LastOperationWasWrite)
  {
    //todo: apply the write as well


  }


  csEnter(&currentcpuinfo->EPTPML4CS);

  if (isAMD)
  {
    sendstringf("%d: ept_handleCloakEventAfterStep for AMD. cloakdata=%6\n", currentcpuinfo->cpunr, cloakdata);
    sendstringf("swapping the current page back with the data page\n", cloakdata);

    sendstringf("old npentry value = %6\n",*(QWORD *)(cloakdata->npentry[currentcpuinfo->cpunr]));

    *(QWORD *)(cloakdata->npentry[currentcpuinfo->cpunr])=cloakdata->PhysicalAddressData; //back to the non-executable state
    cloakdata->npentry[currentcpuinfo->cpunr]->EXB=1;

    cloakdata->npentry[currentcpuinfo->cpunr]->P=1;
    cloakdata->npentry[currentcpuinfo->cpunr]->RW=1;
    cloakdata->npentry[currentcpuinfo->cpunr]->US=1;


    sendstringf("new npentry value = %6\n",*(QWORD *)(cloakdata->npentry[currentcpuinfo->cpunr]));


  }
  else
  {
    *(QWORD *)(cloakdata->eptentry[currentcpuinfo->cpunr])=cloakdata->PhysicalAddressExecutable; //back to the executable state
    cloakdata->eptentry[currentcpuinfo->cpunr]->WA=0;
    cloakdata->eptentry[currentcpuinfo->cpunr]->RA=0;
    if (has_EPT_ExecuteOnlySupport)
      cloakdata->eptentry[currentcpuinfo->cpunr]->XA=1;
    else
      cloakdata->eptentry[currentcpuinfo->cpunr]->XA=0;
  }
  csLeave(&currentcpuinfo->EPTPML4CS);


  csLeave(&CloakedPagesCS);


  ept_invalidate();

  return 0;
}


int ept_cloak_activate(QWORD physicalAddress, int mode)
{
  int i;
  QWORD address;
  PCloakedPageData data;

  sendstringf("ept_cloak_activate(%6,%d)\n", physicalAddress, mode);

  physicalAddress=physicalAddress & MAXPHYADDRMASKPB;
  csEnter(&CloakedPagesCS);

  //first run check
  if (CloakedPagesMap==NULL)
  {
    if (CloakedPagesList==NULL)
      CloakedPagesList=addresslist_create();

    if (CloakedPagesList->size>=MAXCLOAKLISTBEFORETRANFERTOMAP)
    {
      //convert the list to a map
      if (CloakedPagesMap==NULL) //should be
      {
        CloakedPagesMap=createPhysicalMemoryMap();
        for (i=0; i<CloakedPagesList->size; i++)
        {
          address=CloakedPagesList->list[i].address;
          data=(PCloakedPageData)CloakedPagesList->list[i].data;

          map_setEntry(CloakedPagesMap, address, data);
          //just copy, no need to reactivate
        }

        addresslist_destroy(CloakedPagesList);
        CloakedPagesList=NULL;
      }
    }
  }


  PCloakedPageData cloakdata;

  if (CloakedPagesMap)
    cloakdata=map_getEntry(CloakedPagesMap, physicalAddress);
  else
    cloakdata=addresslist_find(CloakedPagesList, physicalAddress);


  if (cloakdata)
  {
    //already cloaked
    csLeave(&CloakedPagesCS);
    return 1;
  }

  //new one, allocate and fill in a cloakdata structure
  int cpucount=getCPUCount();
  cloakdata=malloc(sizeof(CloakedPageData)+cpucount*sizeof(PEPT_PTE));
  zeromemory(cloakdata,sizeof(CloakedPageData)+cpucount*sizeof(PEPT_PTE));


  //fill in the data
  cloakdata->Executable=mapPhysicalMemoryGlobal(physicalAddress, 4096);
  cloakdata->Data=malloc(4096);

  copymem(cloakdata->Data, cloakdata->Executable, 4096);


  cloakdata->PhysicalAddressExecutable=physicalAddress;
  cloakdata->PhysicalAddressData=VirtualToPhysical(cloakdata->Data);

  //Intel. The page is unreadable. Reads cause a fault and then get handled by a single step, executes run the modified code with no exception (fastest way possible)
  //In case Execute but no read is not supported single step through the whole page (slow)
  //Mode is ignored

  //AMD: The page is readable but non-executable
  //On execute the page gets swapped out with the modified one and made executable
  //mode 0: For every address make it executable, do a single step, and then made back to non-executable (see execute watch)
  //mode 1: All other pages will be made non-executable (511 PTE's 511 PDE, 511 PDPTE and 511 PML4's need to be made non-executable: 2044 entries )
  //        until a npf happens.
  //        Boundary situation: Do a single step when at an instruction that spans both pages
  //may mode 2?:same as mode 1 but instead of adjusting 2044 entries swap the NP pointer to a pagesystem with only that one executable page. And on memory access outside map them in as usual (as non executable) unless it's a sidepage
  //            pro: Faster after a few runs. con: eats up a ton more memory

  // Downside compared to Intel: Cloaked pages can see their own page as edited

  cloakdata->CloakMode=mode;



  //map in the physical address descriptor for all CPU's as execute only
  pcpuinfo currentcpuinfo=firstcpuinfo;

  //lock ANY other CPU from triggering (a cpu could trigger before this routine is done)


  sendstringf("Cloaking memory (initiated by cpu %d) \n", getcpunr());

  while (currentcpuinfo)
  {
    int cpunr=currentcpuinfo->cpunr;
    sendstringf("cloaking cpu %d\n", cpunr);

    if (cpunr>=cpucount)
    {
      //'issue' with the cpucount or cpunumber
      cpucount=cpunr*2;
      cloakdata=realloc(cloakdata, cpucount*sizeof(PEPT_PTE));
    }

    csEnter(&currentcpuinfo->EPTPML4CS);

    currentcpuinfo->eptUpdated=1;


    QWORD PA;

    if (isAMD)
      PA=NPMapPhysicalMemory(currentcpuinfo, physicalAddress, 1);
    else
      PA=EPTMapPhysicalMemory(currentcpuinfo, physicalAddress, 1);

    sendstringf("%d Cloak: After mapping the page as a 4KB page\n", cpunr);

    cloakdata->eptentry[cpunr]=mapPhysicalMemoryGlobal(PA, sizeof(EPT_PTE));

    sendstringf("%d Cloak old entry is %6\n", cpunr,  *(QWORD*)(cloakdata->eptentry[cpunr]));


    if (isAMD)
    {
      //Make it non-executable, and make the data read be the fake data
      _PTE_PAE temp;
      temp=*((PPTE_PAE)&cloakdata->PhysicalAddressData); //read data

      temp.P=1;
      temp.RW=1;
      temp.US=1;
      temp.EXB=1; //disable execute

      *(PPTE_PAE)(cloakdata->eptentry[cpunr])=temp;
    }
    else
    {
      //make it nonreadable
      EPT_PTE temp=*(cloakdata->eptentry[cpunr]);
      if (has_EPT_ExecuteOnlySupport)
        temp.XA=1;
      else
        temp.XA=0; //going to be slow

      temp.RA=0;
      temp.WA=0;

      *(cloakdata->eptentry[cpunr])=temp;
    }

    sendstringf("%d Cloak new entry is %6\n", cpunr, *(QWORD*)(cloakdata->eptentry[cpunr]));


    _wbinvd();
    currentcpuinfo->eptUpdated=1; //set this before unlock, so if a NP exception happens before the next vmexit is handled it knows not to remap it with full access

    csLeave(&currentcpuinfo->EPTPML4CS);

    currentcpuinfo=currentcpuinfo->next;
  }

  if (CloakedPagesMap)
    map_setEntry(CloakedPagesMap, physicalAddress, (void*)cloakdata);
  else
    addresslist_add(CloakedPagesList, physicalAddress, (void*)cloakdata);

  sendstringf("Invalidating ept\n");

  ept_invalidate();

  csLeave(&CloakedPagesCS);
  return 0;
}



int ept_cloak_deactivate(QWORD physicalAddress)
{
  int i;
  physicalAddress=physicalAddress & MAXPHYADDRMASKPB;

  PCloakedPageData cloakdata;


  csEnter(&CloakedPagesCS);

  if (CloakedPagesMap)
    cloakdata=map_getEntry(CloakedPagesMap, physicalAddress);
  else
    cloakdata=addresslist_find(CloakedPagesList, physicalAddress);

  if (cloakdata)
  {
    //check if there is a changereg on bp
    csEnter(&ChangeRegBPListCS);
    for (i=0; i<ChangeRegBPListPos; i++)
    {
      if ((ChangeRegBPList[i].Active) && (ChangeRegBPList[i].cloakdata==cloakdata)) //delete this one first
        ept_cloak_removechangeregonbp(ChangeRegBPList[i].PhysicalAddress);
    }

    csLeave(&ChangeRegBPListCS);


    copymem(cloakdata->Executable, cloakdata->Data, 4096);
    pcpuinfo currentcpuinfo=firstcpuinfo;
    while (currentcpuinfo)
    {
      EPT_PTE temp=*(cloakdata->eptentry[currentcpuinfo->cpunr]);
      temp.RA=1;
      temp.WA=1;
      temp.XA=1;
      *(cloakdata->eptentry[currentcpuinfo->cpunr])=temp;
      _wbinvd();
      currentcpuinfo->eptUpdated=1;

      unmapPhysicalMemoryGlobal(cloakdata->eptentry[currentcpuinfo->cpunr], sizeof(EPT_PTE));
      currentcpuinfo=currentcpuinfo->next;
    }

    unmapPhysicalMemoryGlobal(cloakdata->Executable, 4096);

    free(cloakdata->Data);
    cloakdata->Data=NULL;

    free(cloakdata);
  }

  if (CloakedPagesMap)
    map_setEntry(CloakedPagesMap, physicalAddress, NULL);
  else
    addresslist_remove(CloakedPagesList, physicalAddress);




  csLeave(&CloakedPagesCS);

  ept_invalidate();

  //if there where cloak event events pending, then next time they violate, the normal handler will make it RWX on the address it should
  return (cloakdata!=NULL);
}

int ept_cloak_readOriginal(pcpuinfo currentcpuinfo,  VMRegisters *registers, QWORD physicalAddress, QWORD destination)
/* Called by vmcall */
{
  int error;
  physicalAddress=physicalAddress & MAXPHYADDRMASKPB;

  QWORD pagefault;

  void *dest=mapVMmemory(currentcpuinfo, destination, 4096,&error, &pagefault);

  if (error==2)
    return raisePagefault(currentcpuinfo, pagefault);

  csEnter(&CloakedPagesCS);

  PCloakedPageData cloakdata;

  if (CloakedPagesMap)
    cloakdata=map_getEntry(CloakedPagesMap, physicalAddress);
  else
    cloakdata=addresslist_find(CloakedPagesList, physicalAddress);

  if (cloakdata)
  {
    void *src=mapPhysicalMemory(cloakdata->PhysicalAddressExecutable, 4096);
    copymem(dest,src,4096);
    registers->rax=0;

    unmapPhysicalMemory(src,4096);
  }
  else
  {
    registers->rax=1;
  }

  if (isAMD)
    currentcpuinfo->vmcb->RAX= registers->rax;

  csLeave(&CloakedPagesCS);

  unmapVMmemory(dest,4096);


  if (isAMD)
  {
    if (AMD_hasNRIPS)
      currentcpuinfo->vmcb->RIP=currentcpuinfo->vmcb->nRIP;
    else
      currentcpuinfo->vmcb->RIP+=3;
  }
  else
    vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));

  return 0;
}

int ept_cloak_writeOriginal(pcpuinfo currentcpuinfo,  VMRegisters *registers, QWORD physicalAddress, QWORD source)
{
  int error;
  physicalAddress=physicalAddress & MAXPHYADDRMASKPB;

  nosendchar[getAPICID()]=0;
  sendstring("ept_cloak_writeOriginal");

  QWORD pagefault;

  void *src=mapVMmemory(currentcpuinfo, source, 4096,&error, &pagefault);

  if (error==2)
    return raisePagefault(currentcpuinfo, pagefault);

  csEnter(&CloakedPagesCS);
  PCloakedPageData cloakdata;

  if (CloakedPagesMap)
    cloakdata=map_getEntry(CloakedPagesMap, physicalAddress);
  else
    cloakdata=addresslist_find(CloakedPagesList, physicalAddress);

  if (cloakdata)
  {
    void *dest=mapPhysicalMemory(cloakdata->PhysicalAddressExecutable, 4096);

    sendstringf("cloakdata->PhysicalAddressExecutable=%6\n", cloakdata->PhysicalAddressExecutable);
    sendstringf("cloakdata->PhysicalAddressData=%6\n", cloakdata->PhysicalAddressData);


    sendstringf("Writing to PA %6\n", cloakdata->PhysicalAddressExecutable);
    copymem(dest,src,4096);
    registers->rax=0;

    unmapPhysicalMemory(dest,4096);

  }
  else
    registers->rax=1;

  if (isAMD)
    currentcpuinfo->vmcb->RAX= registers->rax;

  csLeave(&CloakedPagesCS);

  unmapVMmemory(src,4096);


  if (isAMD)
  {
    if (AMD_hasNRIPS)
      currentcpuinfo->vmcb->RIP=currentcpuinfo->vmcb->nRIP;
    else
      currentcpuinfo->vmcb->RIP+=3;
  }
  else
    vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));

  return 0;
}

int ept_cloak_traceonbp_getstatus(DWORD *count, DWORD *maxcount)
//return: 0=no trace configured. 1=trace configured but not started yet, 2=trace configured and started, 3=trace done
{
  int result=0;
  *count=0;
  *maxcount=0;
  csEnter(&CloakedPagesCS);
  if (TraceOnBP)
  {

    sendstringf("TraceOnBP->numberOfEntries=%d\n", TraceOnBP->numberOfEntries);
    sendstringf("TraceOnBP->count=%d\n", TraceOnBP->count);
    *count=TraceOnBP->numberOfEntries;
    *maxcount=TraceOnBP->count+TraceOnBP->numberOfEntries;

    sendstringf("*maxcount=%d\n", *maxcount);
    result=1;

    if (TraceOnBP->triggered)
    {
      result=2;
      if (TraceOnBP->finished)
      {
        result=3;
      }
    }
  }
  csLeave(&CloakedPagesCS);

  return result;
}

int ept_cloak_traceonbp_stoptrace() //stops it, but doesn't delete it
//0=no trace going on
//1=trace configured, but not triggered yet
//2=trace was configured and already triggered, but not finished yet
//3=trace was finished
{
  int result=0;
  csEnter(&CloakedPagesCS);
  if (TraceOnBP)
  {
    result=1;
    TraceOnBP->shouldquit=1;

    if (TraceOnBP->triggered==0)
    {
      unsigned char *executable=(unsigned char *)TraceOnBP->cloakdata->Executable;
      executable[TraceOnBP->PhysicalAddress & 0xfff]=TraceOnBP->originalbyte;
    }
    else
      result=2;

    if (TraceOnBP->finished)
    {
      result=3;
    }
  }
  csLeave(&CloakedPagesCS);

  return result;
}

int ept_cloak_traceonbp_remove(int forcequit)
{
  if (TraceOnBP)
  {
    csEnter(&CloakedPagesCS);
    if (TraceOnBP)
    {
      if (TraceOnBP->triggered==FALSE)
      {
        //still needs to restore the byte
        unsigned char *executable=(unsigned char *)TraceOnBP->cloakdata->Executable;
        executable[TraceOnBP->PhysicalAddress & 0xfff]=TraceOnBP->originalbyte;
      }
      else
      {
        if (TraceOnBP->finished==FALSE)
        {
          //trace is still going
          TraceOnBP->shouldquit=1;

          if (forcequit==0)
          {
            csLeave(&CloakedPagesCS);
            return 2; //can not disable yet (tell CE to try again in a bit)
          }
        }
      }

      free(TraceOnBP);
      TraceOnBP=NULL;

      csLeave(&CloakedPagesCS);
      return 1;
    }

    csLeave(&CloakedPagesCS);
  }

  return 0; //no trace to delete
}

int ept_cloak_traceonbp(QWORD physicalAddress, DWORD flags, DWORD tracecount)
{
  int result=1;
  if (ept_cloak_traceonbp_remove(0)==2) return 2;

  QWORD physicalBase=physicalAddress & MAXPHYADDRMASKPB;
  ept_cloak_activate(physicalBase,0); //just making sure

  sendstringf("ept_cloak_traceonbp for %6", physicalAddress);

  csEnter(&CloakedPagesCS);

  PCloakedPageData cloakdata;
  if (CloakedPagesMap)
    cloakdata=map_getEntry(CloakedPagesMap, physicalBase);
  else
    cloakdata=addresslist_find(CloakedPagesList, physicalBase);


  if (cloakdata)
  {
    //found it.  Create an int3 bp at that spot
    int offset=physicalAddress & 0xfff;
    unsigned char *executable=cloakdata->Executable;

    //

    int entrytype=0;
    int entrysize=sizeof(PageEventBasic);
    int logfpu=(flags & 1);
    int logstack=(flags & 2);
    int logsize;

    if ((logfpu==0) && (logstack==0))
    {
      entrytype=0;
      entrysize=sizeof(PageEventBasic);
    }
    else
    if ((logfpu==1) && (logstack==0))
    {
      entrytype=1;
      entrysize=sizeof(PageEventExtended);
    }
    else
    if ((logfpu==0) && (logstack==1))
    {
      entrytype=2;
      entrysize=sizeof(PageEventBasicWithStack);
    }
    else //fpu=1 and stack=1
    {
      entrytype=3;
      entrysize=sizeof(PageEventExtendedWithStack);
    }

    logsize=sizeof(TraceOnBPEntry)*2+entrysize*tracecount;

    sendstringf("Going to allocate %d bytes for the log...", logsize);

    TraceOnBP=malloc(logsize);
    if (TraceOnBP)
    {
      zeromemory(TraceOnBP, logsize);
      sendstringf("Success. Allocated at %6\n", TraceOnBP);
      TraceOnBP->PhysicalAddress=physicalAddress;
      TraceOnBP->triggered=0;
      TraceOnBP->finished=0;
      TraceOnBP->shouldquit=0;
      TraceOnBP->count=tracecount;
      TraceOnBP->cloakdata=cloakdata;
      TraceOnBP->originalbyte=executable[offset];
      TraceOnBP->datatype=entrytype;

      executable[offset]=0xcc; //int3 bp's will happen now (even on other CPU's)

      result=0;
    }
    else
      result=3; //not enough memory free
  }

  csLeave(&CloakedPagesCS);

  return result;


}

int ept_cloak_changeregonbp(QWORD physicalAddress, PCHANGEREGONBPINFO changereginfo)
{
  int result=1;

  nosendchar[getAPICID()]=0;
  sendstringf("ept_cloak_changeregonbp(%6,%6)\n", physicalAddress, changereginfo);

  sendstringf("Removing old changeregonbp\n");
  ept_cloak_removechangeregonbp(physicalAddress);

  QWORD physicalBase=physicalAddress & MAXPHYADDRMASKPB;

  sendstringf("Activating cloak at base %6 (if not yet active)\n", physicalBase);
  ept_cloak_activate(physicalBase,0); //just making sure

  sendstringf("ept_cloak_changeregonbp:\n");
  sendstringf("  changeRAX:%d\n", changereginfo->Flags.changeRAX);
  sendstringf("  changeRBX:%d\n", changereginfo->Flags.changeRBX);
  sendstringf("  changeRCX:%d\n", changereginfo->Flags.changeRCX);
  sendstringf("  changeRDX:%d\n", changereginfo->Flags.changeRDX);
  sendstringf("  changeRSI:%d\n", changereginfo->Flags.changeRSI);
  sendstringf("  changeRDI:%d\n", changereginfo->Flags.changeRDI);
  sendstringf("  changeRBP:%d\n", changereginfo->Flags.changeRBP);
  sendstringf("  changeRSP:%d\n", changereginfo->Flags.changeRSP);
  sendstringf("  changeRIP:%d\n", changereginfo->Flags.changeRIP);
  sendstringf("  changeR8:%d\n", changereginfo->Flags.changeR8);
  sendstringf("  changeR9:%d\n", changereginfo->Flags.changeR9);
  sendstringf("  changeR10:%d\n", changereginfo->Flags.changeR10);
  sendstringf("  changeR11:%d\n", changereginfo->Flags.changeR11);
  sendstringf("  changeR12:%d\n", changereginfo->Flags.changeR12);
  sendstringf("  changeR13:%d\n", changereginfo->Flags.changeR13);
  sendstringf("  changeR14:%d\n", changereginfo->Flags.changeR14);
  sendstringf("  changeR15:%d\n", changereginfo->Flags.changeR15);
  sendstringf("  changeCF:%d\n", changereginfo->Flags.changeCF);
  sendstringf("  changePF:%d\n", changereginfo->Flags.changePF);
  sendstringf("  changeAF:%d\n", changereginfo->Flags.changeAF);
  sendstringf("  changeZF:%d\n", changereginfo->Flags.changeZF);
  sendstringf("  changeSF:%d\n", changereginfo->Flags.changeSF);
  sendstringf("  changeOF:%d\n", changereginfo->Flags.changeOF);
  sendstringf("  newCF:%d\n", changereginfo->Flags.newCF);
  sendstringf("  newPF:%d\n", changereginfo->Flags.newPF);
  sendstringf("  newAF:%d\n", changereginfo->Flags.newAF);
  sendstringf("  newZF:%d\n", changereginfo->Flags.newZF);
  sendstringf("  newSF:%d\n", changereginfo->Flags.newSF);
  sendstringf("  newOF:%d\n", changereginfo->Flags.newOF);

  sendstringf("  newRAX:%d\n", changereginfo->newRAX);
  sendstringf("  newRBX:%d\n", changereginfo->newRBX);
  sendstringf("  newRCX:%d\n", changereginfo->newRCX);
  sendstringf("  newRDX:%d\n", changereginfo->newRDX);
  sendstringf("  newRSI:%d\n", changereginfo->newRSI);
  sendstringf("  newRDI:%d\n", changereginfo->newRDI);
  sendstringf("  newRBP:%d\n", changereginfo->newRBP);
  sendstringf("  newRSP:%d\n", changereginfo->newRSP);
  sendstringf("  newRIP:%d\n", changereginfo->newRIP);
  sendstringf("  newR8:%d\n", changereginfo->newR8);
  sendstringf("  newR9:%d\n", changereginfo->newR9);
  sendstringf("  newR10:%d\n", changereginfo->newR10);
  sendstringf("  newR11:%d\n", changereginfo->newR11);
  sendstringf("  newR12:%d\n", changereginfo->newR12);
  sendstringf("  newR13:%d\n", changereginfo->newR13);
  sendstringf("  newR14:%d\n", changereginfo->newR14);
  sendstringf("  newR15:%d\n", changereginfo->newR15);


  csEnter(&CloakedPagesCS);

  PCloakedPageData cloakdata;
  if (CloakedPagesMap)
    cloakdata=map_getEntry(CloakedPagesMap, physicalBase);
  else
    cloakdata=addresslist_find(CloakedPagesList, physicalBase);


  if (cloakdata)
  {
    //found it.  Create an int3 bp at that spot
    int ID=-1;
    int offset=physicalAddress & 0xfff;
    unsigned char *executable=cloakdata->Executable;

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
    ChangeRegBPList[ID].cloakdata=cloakdata;
    ChangeRegBPList[ID].Active=1;

    executable[offset]=0xcc; //int3 bp's will happen now (even on other CPU's)

    csLeave(&ChangeRegBPListCS);
    result=0;
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
      unsigned char *executable=(unsigned char *)ChangeRegBPList[i].cloakdata->Executable;
      executable[physicalAddress & 0xfff]=ChangeRegBPList[i].originalbyte;
      ChangeRegBPList[i].Active=0;

      /*  _wbinvd();
      vpid_invalidate();
      ept_invalidate();*/
      result=0;
    }
  }

  csLeave(&ChangeRegBPListCS);
  csLeave(&CloakedPagesCS);

  return result;
}

BOOL ept_handleHardwareBreakpoint(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, FXSAVE64 *fxsave)
{
  int result=FALSE;
  if (TraceOnBP)
  {
    QWORD RIP=isAMD?currentcpuinfo->vmcb->RIP:vmread(vm_guest_rip);
    csEnter(&CloakedPagesCS);

    nosendchar[getAPICID()]=0;
    sendstringf("%6: ept_handleHardwareBreakpoint:\n", RIP);

    if (TraceOnBP && (TraceOnBP->triggered))
    {
      regDR6 dr6;
      QWORD cr3;
      QWORD fsbase,gsbase, kernelgsbase;
      kernelgsbase=readMSR(0xc0000102);
      int isDuetoSingleStep;

      if (isAMD)
      {
        dr6.DR6=currentcpuinfo->vmcb->DR6;
        cr3=currentcpuinfo->vmcb->CR3;
        fsbase=currentcpuinfo->vmcb->fs_base;
        gsbase=currentcpuinfo->vmcb->gs_base;
        isDuetoSingleStep=dr6.BS;
        //kernelgsbase=currentcpuinfo->vmcb->KernelGsBase; //maybe?
      }
      else
      {
        dr6.DR6=getDR6();

        cr3=vmread(vm_guest_cr3);
        fsbase=vmread(vm_guest_fs_base);
        gsbase=vmread(vm_guest_gs_base);

        isDuetoSingleStep=(vmread(vm_exit_qualification) & 0x4000)!=0;

      }



      sendstringf("Checking state:\n");
      sendstringf("DR6=%8  DR6.BS=%d isDuetoSingleStep=%d\n", dr6.DR6, dr6.BS, isDuetoSingleStep);
      sendstringf("TraceOnBP->triggeredcr3=%8\n" , TraceOnBP->triggeredcr3);
      sendstringf("TraceOnBP->triggeredfsbase=%8\n" , TraceOnBP->triggeredfsbase);
      sendstringf("TraceOnBP->triggeredgsbase=%8\n" , TraceOnBP->triggeredgsbase);
      sendstringf("TraceOnBP->cr3=%8\n" , cr3);
      sendstringf("TraceOnBP->fsbase=%8\n" , fsbase);
      sendstringf("TraceOnBP->gsbase=%8\n" , gsbase);
      sendstringf("TraceOnBP->gsbasekernel=%8\n" , kernelgsbase);


      if ((isDuetoSingleStep) && (TraceOnBP->triggeredcr3==cr3) && (TraceOnBP->triggeredfsbase==fsbase) && (TraceOnBP->triggeredgsbase==gsbase))
      {

        recordState(&TraceOnBP->pe, TraceOnBP->datatype, TraceOnBP->numberOfEntries, currentcpuinfo, vmregisters, fxsave);
        TraceOnBP->numberOfEntries++;


        TraceOnBP->count--;
        if (TraceOnBP->count<=0)
          TraceOnBP->shouldquit=1;



        //setup resume state
        RFLAGS flags;
        flags.value=isAMD?currentcpuinfo->vmcb->RFLAGS:vmread(vm_guest_rflags);
        flags.RF=1; //resume, but leave the TF flag

        if (TraceOnBP->shouldquit==0)
        {
          sendstringf("Setting TF\n");
          flags.TF=1;
        }
        else
        {
          sendstringf("Finishing trace\n");
          flags.TF=0;
          TraceOnBP->finished=1;
        }

        dr6.BS=0;
        if (isAMD)
        {
          currentcpuinfo->vmcb->RFLAGS=flags.value;
          currentcpuinfo->vmcb->DR6=dr6.DR6;
        }
        else
        {
          sendstringf("bla\n");
          flags.RF=1;
          vmwrite(vm_guest_rflags, flags.value);
          if (flags.TF)
          {
            //dr6.BS=1;
           // vmwrite(vm_pending_debug_exceptions, (1<<14)); //set the TF flag in pending debug registers

          }
          else
          {
            //dr6.BS=0;
           //vmwrite(vm_pending_debug_exceptions, vmread(vm_pending_debug_exceptions) & ~(1<<14)); //unset the single step flag
          }

          setDR6(dr6.DR6);
        }

        result=TRUE;
      }
      else
        sendstringf("unexpected hardware breakpoint while tracing. skipping\n");
    }
    else
      sendstringf("tracing hasn't started. skipping\n");

    csLeave(&CloakedPagesCS);
  }
  else
    sendstring("no tracing going on. skipping\n");

  return result;
}

BOOL ept_handleFrozenThread(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, FXSAVE64 *fxsave, int id)
//pre: brokenThreadListCS should be locked during this call
{
  int result=TRUE;
  RFLAGS v;
  v.value=BrokenThreadList[id].state.basic.FLAGS;
  QWORD RIP=isAMD?currentcpuinfo->vmcb->RIP:vmread(vm_guest_rip);

  nosendchar[getAPICID()]=0;
  //sendstringf("ept_handleFrozenThread: RIP=%6\n",RIP);

  BrokenThreadList[id].state.basic.Count++;//heartbeat to show it's still triggering the BP
  if (BrokenThreadList[id].continueMethod)
  {
    nosendchar[getAPICID()]=0;
    sendstringf("continueMethod is not 0\n");




    //restore the state according to the saved state (could have been changed) and do a single step or run  (this also undoes the state used value in rax, which is good)
    if (isAMD)
    {
      currentcpuinfo->vmcb->RIP=BrokenThreadList[id].state.basic.RIP;
      currentcpuinfo->vmcb->RAX=BrokenThreadList[id].state.basic.RAX;
      currentcpuinfo->vmcb->RSP=BrokenThreadList[id].state.basic.RSP;

      v.RF=1; //tell the watch handler to skip this if it returns at the same spot again
      currentcpuinfo->vmcb->RFLAGS=v.value;
    }
    else
    {
      vmregisters->rax=BrokenThreadList[id].state.basic.RAX;
      vmwrite(vm_guest_rip,BrokenThreadList[id].state.basic.RIP);
      vmwrite(vm_guest_rsp, BrokenThreadList[id].state.basic.RSP);

      vmwrite(vm_guest_interruptability_state,1); //tell the watch handler to skip this if it returns at the same spot again
    }
    vmregisters->rbx=BrokenThreadList[id].state.basic.RBX;
    vmregisters->rcx=BrokenThreadList[id].state.basic.RCX;
    vmregisters->rdx=BrokenThreadList[id].state.basic.RDX;
    vmregisters->rsi=BrokenThreadList[id].state.basic.RSI;
    vmregisters->rdi=BrokenThreadList[id].state.basic.RDI;
    vmregisters->rbp=BrokenThreadList[id].state.basic.RBP;
    vmregisters->r8=BrokenThreadList[id].state.basic.R8;
    vmregisters->r9=BrokenThreadList[id].state.basic.R9;
    vmregisters->r10=BrokenThreadList[id].state.basic.R10;
    vmregisters->r11=BrokenThreadList[id].state.basic.R11;
    vmregisters->r12=BrokenThreadList[id].state.basic.R12;
    vmregisters->r13=BrokenThreadList[id].state.basic.R13;
    vmregisters->r14=BrokenThreadList[id].state.basic.R14;
    vmregisters->r15=BrokenThreadList[id].state.basic.R15;

    *fxsave=BrokenThreadList[id].state.fpudata;

    if (BrokenThreadList[id].continueMethod==1)
    {
      sendstringf("This is a single step, so setting single step mode\n");
      //set single stepping
      vmx_enableSingleStepMode();
      vmx_addSingleSteppingReason(currentcpuinfo, SSR_STEPANDBREAK, id); //restore rip back to int3 bp after the step

      BrokenThreadList[id].watchid=-1; //set it as single stepping
    }
    else
    {
      BrokenThreadList[id].inuse=0; //continue (on purpuse)
      BrokenThreadList[id].continueMethod=0;

      sendstringf("Just continue.  It should continue at %2:%6\n",BrokenThreadList[id].state.basic.CS, BrokenThreadList[id].state.basic.RIP);
      if (isAMD)
      {
        sendstringf("It will continue at %2:%6\n",(unsigned char)currentcpuinfo->vmcb->cs_selector, currentcpuinfo->vmcb->RIP);
      }
      else
      {
        sendstringf("It will continue at %2:%6\n",(unsigned char)vmread(vm_guest_cs), vmread(vm_guest_rip));
      }


      //do one instruction at least
      if (isAMD)
      {
        if (v.IF)
          currentcpuinfo->vmcb->INTERRUPT_SHADOW=1;
      }
      else
      {
        vmwrite(vm_guest_interruptability_state,1); //blocking by sti
      }



    }
  }
  else
  {
    //RFLAGS v2;
    //v2.value=currentcpuinfo->vmcb->RFLAGS;

    //sendstringf("%d: Still frozen at %6  CR8=%x stored: IF=%d RF=%d current: IF=%d rd=%d INTERRUPT_SHADOW=%d EFER=%x FMASK=%x\n", currentcpuinfo->cpunr, BrokenThreadList[id].state.basic.RIP, getCR8(), v.IF, v.RF, v2.IF, v2.RF, currentcpuinfo->vmcb->INTERRUPT_SHADOW,
    //    currentcpuinfo->vmcb->EFER,
    //    currentcpuinfo->vmcb->SFMASK);


  }

  return result;
}

BOOL ept_handleSoftwareBreakpoint(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, FXSAVE64 *fxsave)
{
  //check if it is a cloaked instruction
  int i;
  int result=FALSE;
  QWORD RIP=isAMD?currentcpuinfo->vmcb->RIP:vmread(vm_guest_rip);

  nosendchar[getAPICID()]=0;


  //convert RIP into a physical address  (note that RIP has not been decreased by 1 yet)


  int notpaged;
  QWORD PA=getPhysicalAddressVM(currentcpuinfo, RIP, &notpaged);

 // sendstringf("ept_handleSoftwareBreakpoint. RFLAGS=%x\n", RIP, PA);

  if (notpaged==0) //should be since it's a software interrupt...
  {
    //sendstringf("paged\n");
    csEnter(&BrokenThreadListCS);
    if (BrokenThreadList && BrokenThreadListPos)
    {
      //sendstringf("Checking the broken threadlist");
      for (i=0; i<BrokenThreadListPos; i++)
      {
        if (BrokenThreadList[i].inuse)
        {
          QWORD cr3;
          QWORD rip,rax;

          if (isAMD)
          {
            cr3=currentcpuinfo->vmcb->CR3;
            rip=currentcpuinfo->vmcb->RIP;
            rax=currentcpuinfo->vmcb->RAX;
          }
          else
          {
            cr3=vmread(vm_guest_cr3);
            rip=vmread(vm_guest_rip);
            rax=vmregisters->rax;
          }

          if ((QWORD)rax!=(QWORD)i) continue; //rax should match the brokenthreadlist id

          //warning: In windows, kernelmode gsbase changes depending on the cpu so can not be used as identifier then

          //check if it's matches this thread
          if ((cr3==BrokenThreadList[i].state.basic.CR3) && ((rip==BrokenThreadList[i].KernelModeLoop) || (rip==BrokenThreadList[i].UserModeLoop)))
          {
            result=ept_handleFrozenThread(currentcpuinfo, vmregisters, fxsave, i);
            break;
          }
        }
      }
    }
    csLeave(&BrokenThreadListCS);
    if (result) return result; //it was a frozen thread


    csEnter(&CloakedPagesCS);

    //if (TraceOnBP)
    //  sendstringf("TraceOnBP->PhysicalAddres=%6  PA=%6\n", TraceOnBP->PhysicalAddress, PA);


    if (TraceOnBP && (TraceOnBP->PhysicalAddress==PA))
    {

      if (TraceOnBP->triggered)
      {
        sendstringf("already triggered\n");
        csLeave(&CloakedPagesCS);
        return TRUE; //try again (something else got it first and likely restored the byte)
      }

      //todo: if option is to step through interrupts use vmx_enableSingleStepMode() and just follow this cpu instead of process

      //for now, just set stepping (which is visible to interrupts and pushf in that code)
      sendstringf("setting TF\n");

      RFLAGS flags;
      flags.value=isAMD?currentcpuinfo->vmcb->RFLAGS:vmread(vm_guest_rflags);
      flags.TF=1;


      if (isAMD)
        currentcpuinfo->vmcb->RFLAGS=flags.value;
      else
        vmwrite(vm_guest_rflags, flags.value);


      int offset=TraceOnBP->PhysicalAddress & 0xfff;
      unsigned char *executable=(unsigned char *)TraceOnBP->cloakdata->Executable;
      executable[offset]=TraceOnBP->originalbyte;

      //save the first state

      recordState(&TraceOnBP->pe, TraceOnBP->datatype, TraceOnBP->numberOfEntries, currentcpuinfo, vmregisters, fxsave);
      TraceOnBP->numberOfEntries++;
      TraceOnBP->count--;


      TraceOnBP->triggered=1;
      TraceOnBP->triggeredcr3=isAMD?currentcpuinfo->vmcb->CR3:vmread(vm_guest_cr3);
      TraceOnBP->triggeredgsbase=isAMD?currentcpuinfo->vmcb->gs_base:vmread(vm_guest_gs_base);
      TraceOnBP->triggeredfsbase=isAMD?currentcpuinfo->vmcb->fs_base:vmread(vm_guest_fs_base);

      sendstringf("TraceOnBP->triggeredcr3=%6\n", TraceOnBP->triggeredcr3);
      sendstringf("TraceOnBP->triggeredfsbase=%6\n", TraceOnBP->triggeredfsbase);
      sendstringf("TraceOnBP->triggeredgsbase=%6\n", TraceOnBP->triggeredgsbase);

      csLeave(&CloakedPagesCS);

      sendstringf("returning true\n");
      sendstringf("currentcpuinfo->vmcb->InterceptExceptions=%6\n", currentcpuinfo->vmcb->InterceptExceptions);
      return TRUE;
    }



    csEnter(&ChangeRegBPListCS);
    for (i=0; i<ChangeRegBPListPos; i++)
    {
      if (ChangeRegBPList[i].PhysicalAddress==PA)
      {
        if (ChangeRegBPList[i].Active)
        {
          QWORD oldRIP=RIP;
          //it's a match
          //Todo: Only change if the processID matches  (todo: Add a getProcessID option provided by the OS based caller)
          //For now, make sure that the physical page is not shared, or that the register change is compatible with different processes (e.g kernelmode only, or a Flag change)

          //change regs

          if (ChangeRegBPList[i].changereginfo.Flags.changeRAX)
          {
            if (isAMD)
              currentcpuinfo->vmcb->RAX=ChangeRegBPList[i].changereginfo.newRAX;
            else
              vmregisters->rax=ChangeRegBPList[i].changereginfo.newRAX;
          }
          if (ChangeRegBPList[i].changereginfo.Flags.changeRBX) vmregisters->rbx=ChangeRegBPList[i].changereginfo.newRBX;
          if (ChangeRegBPList[i].changereginfo.Flags.changeRCX) vmregisters->rcx=ChangeRegBPList[i].changereginfo.newRCX;
          if (ChangeRegBPList[i].changereginfo.Flags.changeRDX) vmregisters->rdx=ChangeRegBPList[i].changereginfo.newRDX;
          if (ChangeRegBPList[i].changereginfo.Flags.changeRSI) vmregisters->rsi=ChangeRegBPList[i].changereginfo.newRSI;
          if (ChangeRegBPList[i].changereginfo.Flags.changeRDI) vmregisters->rdi=ChangeRegBPList[i].changereginfo.newRDI;
          if (ChangeRegBPList[i].changereginfo.Flags.changeRBP) vmregisters->rbp=ChangeRegBPList[i].changereginfo.newRBP;
          if (ChangeRegBPList[i].changereginfo.Flags.changeRSP)
          {
            if (isAMD)
              currentcpuinfo->vmcb->RSP=ChangeRegBPList[i].changereginfo.newRSP;
            else
              vmwrite(vm_guest_rsp, ChangeRegBPList[i].changereginfo.newRSP);
          }
          if (ChangeRegBPList[i].changereginfo.Flags.changeRIP)
          {
            if (isAMD)
              currentcpuinfo->vmcb->RIP=ChangeRegBPList[i].changereginfo.newRIP;
            else
              vmwrite(vm_guest_rip, ChangeRegBPList[i].changereginfo.newRIP);
          }
          if (ChangeRegBPList[i].changereginfo.Flags.changeR8)  vmregisters->r8=ChangeRegBPList[i].changereginfo.newR8;
          if (ChangeRegBPList[i].changereginfo.Flags.changeR9)  vmregisters->r9=ChangeRegBPList[i].changereginfo.newR9;
          if (ChangeRegBPList[i].changereginfo.Flags.changeR10) vmregisters->r10=ChangeRegBPList[i].changereginfo.newR10;
          if (ChangeRegBPList[i].changereginfo.Flags.changeR11) vmregisters->r11=ChangeRegBPList[i].changereginfo.newR11;
          if (ChangeRegBPList[i].changereginfo.Flags.changeR12) vmregisters->r12=ChangeRegBPList[i].changereginfo.newR12;
          if (ChangeRegBPList[i].changereginfo.Flags.changeR13) vmregisters->r13=ChangeRegBPList[i].changereginfo.newR13;
          if (ChangeRegBPList[i].changereginfo.Flags.changeR14) vmregisters->r14=ChangeRegBPList[i].changereginfo.newR14;
          if (ChangeRegBPList[i].changereginfo.Flags.changeR15) vmregisters->r15=ChangeRegBPList[i].changereginfo.newR15;

          if (ChangeRegBPList[i].changereginfo.changeFP)
          {
            int r;
            for (r=0; r<8; r++)
              if (ChangeRegBPList[i].changereginfo.changeFP & (1<<r))
              {
                copymem((void*)((QWORD)(&fxsave->FP_MM0)+10*r), (void*)((QWORD)(&fxsave->FP_MM0)+10*r),10);
              }

          }


          if (ChangeRegBPList[i].changereginfo.changeXMM)
          {
            int r;
            for (r=0; r<15; r++)
            {
              BYTE mask=(ChangeRegBPList[i].changereginfo.changeXMM >> (4*r)) & 0xf;
              if (mask)
              {
                DWORD *destparts=(DWORD *)((QWORD)(&fxsave->XMM0)+16*r);
                DWORD *sourceparts=(DWORD *)((QWORD)(&ChangeRegBPList[i].changereginfo.newXMM0)+16*r);
                int p;

                for (p=0; p<4; p++)
                {
                  if (mask & (1 << p))
                    destparts[p]=sourceparts[p];
                }
              }
            }

          }



          RFLAGS flags;
          flags.value=isAMD?currentcpuinfo->vmcb->RFLAGS:vmread(vm_guest_rflags);
          if (ChangeRegBPList[i].changereginfo.Flags.changeCF) flags.CF=ChangeRegBPList[i].changereginfo.Flags.newCF;
          if (ChangeRegBPList[i].changereginfo.Flags.changePF) flags.PF=ChangeRegBPList[i].changereginfo.Flags.newPF;
          if (ChangeRegBPList[i].changereginfo.Flags.changeAF) flags.AF=ChangeRegBPList[i].changereginfo.Flags.newAF;
          if (ChangeRegBPList[i].changereginfo.Flags.changeZF) flags.ZF=ChangeRegBPList[i].changereginfo.Flags.newZF;
          if (ChangeRegBPList[i].changereginfo.Flags.changeSF) flags.SF=ChangeRegBPList[i].changereginfo.Flags.newSF;
          if (ChangeRegBPList[i].changereginfo.Flags.changeOF) flags.OF=ChangeRegBPList[i].changereginfo.Flags.newOF;
          if (isAMD)
            currentcpuinfo->vmcb->RFLAGS=flags.value;
          else
            vmwrite(vm_guest_rflags, flags.value);


          //continue:
          if ((ChangeRegBPList[i].changereginfo.Flags.changeRIP==0) || (ChangeRegBPList[i].changereginfo.newRIP==oldRIP))
          {
            //RIP did not change

            //restore the original byte
            int offset=ChangeRegBPList[i].PhysicalAddress & 0xfff;


            unsigned char *executable=(unsigned char *)ChangeRegBPList[i].cloakdata->Executable;
            executable[offset]=ChangeRegBPList[i].originalbyte;

            //setup single step mode
            vmx_enableSingleStepMode();
            vmx_addSingleSteppingReason(currentcpuinfo, 3,i); //change reg on bp, restore int3 bp

            //no interrupts for one instruction (no other interrupts are pending, it was an int3 that caused this)
            if (isAMD)
            {
              //
              currentcpuinfo->vmcb->INTERRUPT_SHADOW=1;
            }
            else
            {
              vmwrite(vm_guest_interruptability_state,2);
            }

            /* on systems with no exec only support, this means there will be 2 single step reasons.
             * One for the breakpoint restore, and one to set the read disable back
             */

          }
          result=TRUE;
          break;
        }
        else
        {
          //probably a stale breakpoint event that was waiting for the spinlock (what are the changes that there was a 0xcc at the exact same spot a previous bp was set)
          //try again
          //todo: keep a try again counter
          result=TRUE;
        }
      }
    }

    csLeave(&ChangeRegBPListCS);
    csLeave(&CloakedPagesCS);
  }
  else
    sendstringf("Unreadable memory address for an int3 bp....\n");

  return result;
}

int ept_handleStepAndBreak(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, FXSAVE64 *fxsave, int brokenthreadid)
{

  //first check if you can break here. If not, goodbye (todo:step till you can)
  nosendchar[getAPICID()]=0;
  sendstringf("ept_handleStepAndBreak\n");
  DWORD CR8=getCR8();
  RFLAGS flags;
  flags.value=isAMD?currentcpuinfo->vmcb->RFLAGS:vmread(vm_guest_rflags);


  if ((CR8==0) && (flags.IF)) //if interruptable with no mask (on windows called passive mode) (not 100% if on win32, but who uses that...)
  {
    int kernelmode=0;
    if (isAMD)
    {
      Segment_Attribs csattrib;
      csattrib.SegmentAttrib=currentcpuinfo->vmcb->cs_attrib;
      kernelmode=csattrib.DPL==0;
    }
    else
    {
      Access_Rights csar;
      csar.AccessRights=vmread(vm_guest_cs_access_rights);
      kernelmode=csar.DPL==0;
    }

    csEnter(&BrokenThreadListCS);

    if (isAMD) //normally this gets reset after the single step handler. But it needs to be reset here already
    {
      flags.TF=currentcpuinfo->singleStepping.PreviousTFState;
      currentcpuinfo->vmcb->RFLAGS=flags.value;
    }


    QWORD newRIP=0;

    if (BrokenThreadList[brokenthreadid].continueMethod==1) //single step
      newRIP=kernelmode?BrokenThreadList[brokenthreadid].KernelModeLoop:BrokenThreadList[brokenthreadid].UserModeLoop; //anything else, will be a run

    if (newRIP) //e.g if no kernelmode is provided, skip kernelmode
    {
      //save the current state
      fillPageEventBasic(&BrokenThreadList[brokenthreadid].state.basic, vmregisters);
      BrokenThreadList[brokenthreadid].state.fpudata=*fxsave;

      //adjust RIP and RAX  (rip points to the parking spot, RAX contains the specific brokenthreadid (gets undone on resume anyhow)
      vmregisters->rax=brokenthreadid;
      if (isAMD)
      {
        currentcpuinfo->vmcb->RIP=newRIP;
        currentcpuinfo->vmcb->RAX=brokenthreadid;
      }
      else
        vmwrite(vm_guest_rip, newRIP);

      BrokenThreadList[brokenthreadid].continueMethod=0;
    }
    else
    {
      //delete
      BrokenThreadList[brokenthreadid].inuse=2; //mark it as lost
      BrokenThreadList[brokenthreadid].continueMethod=0;
    }

    csLeave(&BrokenThreadListCS);
  }
  else
  {
    csEnter(&BrokenThreadListCS);
    sendstringf("Can not be broken due to interrupt state. Deleting stepping mode\n");
    BrokenThreadList[brokenthreadid].inuse=2; //lost
    BrokenThreadList[brokenthreadid].continueMethod=0;
    csLeave(&BrokenThreadListCS);


    //else can't be broken here. bye bye
  }

  return 0;
}

int ept_getBrokenThreadListCount(void)
{
  return BrokenThreadListPos;
}



int ept_getBrokenThreadEntryShort(int id, int *WatchID, int *Status, QWORD *CR3, QWORD *FSBASE, QWORD *GSBASE, QWORD *GSBASE_KERNEL, DWORD *CS, QWORD *RIP, QWORD *heartbeat)
{
  int result=0;
  csEnter(&BrokenThreadListCS);
  if ((id>=0) && (id<BrokenThreadListPos))
  {
    if (BrokenThreadList[id].inuse)
    {
      *WatchID=BrokenThreadList[id].watchid;
      *Status=BrokenThreadList[id].inuse | (BrokenThreadList[id].continueMethod << 8);

      *CR3=BrokenThreadList[id].state.basic.CR3;
      *FSBASE=BrokenThreadList[id].state.basic.FSBASE;
      *GSBASE=BrokenThreadList[id].state.basic.GSBASE;
      *GSBASE_KERNEL=BrokenThreadList[id].state.basic.GSBASE_KERNEL;
      *CS=BrokenThreadList[id].state.basic.CS;
      *RIP=BrokenThreadList[id].state.basic.RIP;
      *heartbeat=BrokenThreadList[id].state.basic.Count;
    }
    else
      result=2;
  }
  else
    result=1;

  csLeave(&BrokenThreadListCS);
  return result;
}

int ept_getBrokenThreadEntryFull(int id, int *watchid, int *status, PPageEventExtended entry)
{
  int result=0;
  csEnter(&BrokenThreadListCS);
  if ((id>=0) && (id<BrokenThreadListPos))
  {
    if (BrokenThreadList[id].inuse)
    {
      //0..7:1=ok. 2=lost it
      //8..15: continuemethod (if not 0, still waiting to precess)
      *status=BrokenThreadList[id].inuse | (BrokenThreadList[id].continueMethod << 8);  //257=0x101 (inuse,continuemethod=step)   513=0x201  (inuse,run)

      *entry=BrokenThreadList[id].state;
      *watchid=BrokenThreadList[id].watchid;
    }
    else
      result=2;
  }
  else
    result=1;

  csLeave(&BrokenThreadListCS);
  return result;
}

int ept_setBrokenThreadEntryFull(int id, PPageEventExtended entry)
{
  int result=0;
  csEnter(&BrokenThreadListCS);
  if ((id>=0) && (id<BrokenThreadListPos))
  {
    if (BrokenThreadList[id].inuse)
      BrokenThreadList[id].state=*entry;
    else
      result=2;
  }
  else
    result=1;

  csLeave(&BrokenThreadListCS);
  return result;

}

int ept_resumeBrokenThread(int id, int continueMethod)
{
  int result=0;
  sendstringf("ept_resumeBrokenThread(%d,%d)\n",id, continueMethod);
  csEnter(&BrokenThreadListCS);
  if ((id>=0) && (id<BrokenThreadListPos))
  {
    if (BrokenThreadList[id].inuse)
    {
      if (BrokenThreadList[id].inuse==2)
      {
        sendstringf("This thread was abandoned. Releasing it's spot\n");

        //just release it
        BrokenThreadList[id].inuse=0;
        result=4;
      }
      else
      {
        if (BrokenThreadList[id].continueMethod==0)
        {
          sendstringf("Setting broken thread %d to continueMethod %d\n", id, continueMethod);
          BrokenThreadList[id].continueMethod=continueMethod;
          BrokenThreadList[id].watchid=-1;
        }
        else
        {
          sendstringf("already set to continue\n");
          result=3; //already set to continue
        }
      }
    }
    else
    {
      sendstringf("ID (%d) not in use\n", id);
      result=2; //not in use
    }
  }
  else
  {
    sendstringf("ID (%d) out of range\n", id);
    result=1; //out of range
  }

  csLeave(&BrokenThreadListCS);

  return result;
}



int ept_handleSoftwareBreakpointAfterStep(pcpuinfo currentcpuinfo UNUSED,  int ID)
{
  int result=1;
  csEnter(&CloakedPagesCS);
  csEnter(&ChangeRegBPListCS);
  if (ChangeRegBPList[ID].Active)
  {

    QWORD PA=ChangeRegBPList[ID].PhysicalAddress;
    QWORD PABase=PA & MAXPHYADDRMASKPB;
    int offset=PA-PABase;

    unsigned char *executable=(unsigned char*)ChangeRegBPList[ID].cloakdata->Executable;
    executable[offset]=0xcc; //set the breakpoint back
    result=0;
  }

  csLeave(&ChangeRegBPListCS);
  csLeave(&CloakedPagesCS);

  return result;
}



/*
 * WATCH
 */


int getFreeWatchID()
/*
 * scan through the watches for an unused spot, if not found, reallocate the list
 * pre: The watchlistCS has been locked
 */
{
  int i,j;
  sendstringf("+getFreeWatchID\n");
  for (i=0; i<eptWatchListPos; i++)
  {
    if (eptWatchList[i].Active==0)
    {
      sendstringf("Found a non active entry at index %d\n", i);
      return i;
    }
  }

  //still here
  if (eptWatchListPos<eptWatchListSize)
  {
    sendstringf("eptWatchListPos(%d)<eptWatchListSize(%d)\n", eptWatchListPos, eptWatchListSize);
    return eptWatchListPos++;
  }

  sendstringf("Reallocating the list\n");

  //still here, realloc
  i=eptWatchListSize;
  eptWatchListSize=(eptWatchListSize+2)*2;
  eptWatchList=realloc(eptWatchList, eptWatchListSize*sizeof(EPTWatchEntry));

  for (j=i; j<eptWatchListSize; j++)
    eptWatchList[j].Active=0;

  eptWatchListPos++;

  return i;
}

void saveStack(pcpuinfo currentcpuinfo, unsigned char *stack) //stack is 4096 bytes
{
  int error;
  QWORD pagefaultaddress;
  int size=4096;
  QWORD rsp=isAMD?currentcpuinfo->vmcb->RSP:vmread(vm_guest_rsp);



  zeromemory(stack, 4096);
  //copy it but don't care about pagefaults (if there is a pagefault I 'could' trigger a pf and then wait and try again, but fuck it, it's not 'that' important
  unsigned char *gueststack=(unsigned char *)mapVMmemoryEx(currentcpuinfo, rsp, 4096, &error, &pagefaultaddress, 1);

  if (error)
  {
    if (error==2)
      size=pagefaultaddress-rsp;
    else
      return;
  }

  copymem(stack, gueststack, size);
  unmapVMmemory(gueststack, size);
}

void fillPageEventBasic(PageEventBasic *peb, VMRegisters *registers)
{

  peb->GSBASE_KERNEL=readMSR(IA32_GS_BASE_KERNEL_MSR);
  if (isAMD)
  {
    pcpuinfo c=getcpuinfo();

    peb->VirtualAddress=0;
    peb->PhysicalAddress=c->vmcb->EXITINFO2;
    peb->CR3=c->vmcb->CR3;
    peb->FSBASE=c->vmcb->fs_base;
    peb->GSBASE=c->vmcb->gs_base;

    peb->FLAGS=c->vmcb->RFLAGS;
    peb->RAX=c->vmcb->RAX;
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
    peb->RSP=c->vmcb->RSP;
    peb->RIP=c->vmcb->RIP;
    peb->DR0=getDR0();
    peb->DR1=getDR1();
    peb->DR2=getDR2();
    peb->DR3=getDR3();
    peb->DR6=c->vmcb->DR6;
    peb->DR7=c->vmcb->DR7;
    peb->CS=c->vmcb->cs_selector;
    peb->DS=c->vmcb->ds_selector;
    peb->ES=c->vmcb->es_selector;
    peb->SS=c->vmcb->ss_selector;
    peb->FS=c->vmcb->fs_selector;
    peb->GS=c->vmcb->gs_selector;

  }
  else
  {

    peb->VirtualAddress=vmread(vm_guest_linear_address);
    peb->PhysicalAddress=vmread(vm_guest_physical_address);
    peb->CR3=vmread(vm_guest_cr3);
    peb->FSBASE=vmread(vm_guest_fs_base);
    peb->GSBASE=vmread(vm_guest_gs_base);
    peb->FLAGS=vmread(vm_guest_rflags);
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

    peb->DR0=getDR0();
    peb->DR1=getDR1();
    peb->DR2=getDR2();
    peb->DR3=getDR3();

    peb->DR6=getDR6();
    peb->DR7=vmread(vm_guest_dr7);
    peb->CS=vmread(vm_guest_cs);
    peb->DS=vmread(vm_guest_ds);
    peb->ES=vmread(vm_guest_es);
    peb->SS=vmread(vm_guest_ss);
    peb->FS=vmread(vm_guest_fs);
    peb->GS=vmread(vm_guest_gs);
  }
  peb->Count=0;
}

void recordState(void *liststart, int datatype, int currentEntryNr, pcpuinfo currentcpuinfo, VMRegisters *vmregisters, PFXSAVE64 fxsave)
{

  sendstringf("recordState(%p, %d, %d, %p, %p, %p)",liststart, datatype, currentEntryNr, currentcpuinfo, vmregisters, fxsave);
  int logentrysize=0;
  switch (datatype)
  {
    case PE_BASIC:
      logentrysize=sizeof(PageEventBasic);
      PageEventBasic *peb=(PageEventBasic *)((QWORD)(liststart)+currentEntryNr*logentrysize);
      fillPageEventBasic(peb, vmregisters); //physical and linear are ignored if a tracer log
      break;

    case PE_EXTENDED:
      logentrysize=sizeof(PageEventExtended);
      PageEventExtended *pee=(PageEventExtended *)((QWORD)(liststart)+currentEntryNr*logentrysize);
      fillPageEventBasic((PageEventBasic*)pee, vmregisters);
      pee->fpudata=*fxsave;
      break;

    case PE_BASICSTACK:
      logentrysize=sizeof(PageEventBasicWithStack);
      PageEventBasicWithStack *pebws=(PageEventBasicWithStack *)((QWORD)(liststart)+currentEntryNr*logentrysize);
      fillPageEventBasic((PageEventBasic*)pebws, vmregisters);
      saveStack(currentcpuinfo, pebws->stack);
      break;

    case PE_EXTENDEDSTACK:
      logentrysize=sizeof(PageEventExtendedWithStack);
      PageEventExtendedWithStack *peews=(PageEventExtendedWithStack *)((QWORD)(liststart)+currentEntryNr*logentrysize);
      fillPageEventBasic((PageEventBasic*)peews, vmregisters);
      saveStack(currentcpuinfo, peews->stack);
      peews->fpudata=*fxsave;
      break;
  }
  if (datatype<0)
    return;
}



int ept_isWatchIDPerfectMatch(QWORD address, int ID)
{
  return ((eptWatchList[ID].Active) &&
          (
             (address>=eptWatchList[ID].PhysicalAddress) &&
             (address<eptWatchList[ID].PhysicalAddress+eptWatchList[ID].Size)
           )
          );
}

int ept_isWatchIDMatch(QWORD address, int ID)
/*
 * pre: address is already page aligned
 */
{
  return ((eptWatchList[ID].Active) && ((eptWatchList[ID].PhysicalAddress & 0xfffffffffffff000ULL) == address));
}

int ept_getWatchID(QWORD address)
/*
 * returns -1 if not in a page being watched
 * Note that there can be multiple active on the same page
 */
{
  int i;
  //sendstringf("ept_getWatchID(%6)\n", address);
  address=address & 0xfffffffffffff000ULL;
  for (i=0; i<eptWatchListPos; i++)
    if (ept_isWatchIDMatch(address, i))
      return i;

  return -1;
}




BOOL ept_handleWatchEvent(pcpuinfo currentcpuinfo, VMRegisters *registers, PFXSAVE64 fxsave, QWORD PhysicalAddress)
//Used by Intel and AMD
{
  EPT_VIOLATION_INFO evi;
  NP_VIOLATION_INFO nvi;

  int ID;
  int logentrysize;
  int i;

  if (eptWatchListPos==0)
    return FALSE;

  if (isAMD)
  {


    nvi.ErrorCode=currentcpuinfo->vmcb->EXITINFO1;
    if (nvi.ID)
    {
      //instruction fetch.  Apparently, PA is not exact and on a 16 byte radius or worse
      sendstringf("ept_handleWatchEvent execute (ID) on AMD.  RIP=%6 PA=%6\n", currentcpuinfo->vmcb->RIP, PhysicalAddress);

      PhysicalAddress=(PhysicalAddress & 0xfffffffffffff000ULL) | (currentcpuinfo->vmcb->RIP & 0xfff);

      sendstringf("changed PhysicalAddress to %6\n", PhysicalAddress);



    }
  }
  else
  {
    evi.ExitQualification=vmread(vm_exit_qualification);

  }

  csEnter(&eptWatchListCS);

  ID=ept_getWatchID(PhysicalAddress);


  if (ID==-1)
  {
    csLeave(&eptWatchListCS);
    return FALSE;
  }

  if (isAMD)
    lastSeenEPTWatch.data=nvi.ErrorCode;

  else
    lastSeenEPTWatch.data=evi.ExitQualification;

  lastSeenEPTWatch.physicalAddress=PhysicalAddress;
  lastSeenEPTWatch.initialID=ID;


  QWORD RIP;
  QWORD RSP;

  sendstring("EPT/NP event and there is a watchlist entry\n");
  sendstringf("ept_getWatchID returned %d\n", ID);


  if (isAMD)
  {
    RIP=currentcpuinfo->vmcb->RIP;
    RSP=currentcpuinfo->vmcb->RSP;
  }
  else
  {
    RIP=vmread(vm_guest_rip);
    RSP=vmread(vm_guest_rsp);
  }

  lastSeenEPTWatch.skipped=-1;
  lastSeenEPTWatch.rip=RIP;

  QWORD PhysicalAddressBase=PhysicalAddress & 0xfffffffffffff000ULL;



  //nosendchar[getAPICID()]=0;
  sendstringf("Handling something that resembles watch ID %d\n", ID);


  //figure out which access it is really (in case of multiple on the same page)

  for (i=ID; i<eptWatchListPos; i++)
  {
    if (ept_isWatchIDMatch(PhysicalAddressBase, i))
    {
      if (eptWatchList[ID].Type==EPTW_WRITE)
      {
        //must be a write operation error
        if (((!isAMD) && (evi.W) && (evi.WasWritable==0)) || (isAMD && nvi.W))  //write operation and writable was 0
        {
          ID=i;

          if (ept_isWatchIDPerfectMatch(PhysicalAddress, i))
            break;
        }
      }
      else if (eptWatchList[ID].Type==EPTW_READWRITE)
      {
        //must be a read or write operation
        if ((isAMD && nvi.P==0) || ((!isAMD) && (((evi.W) && (evi.WasWritable==0)) || ((evi.R) && (evi.WasReadable==0)))) ) //write operation and writable was 0 or read and readable was 0
        {
          ID=i;
          if (ept_isWatchIDPerfectMatch(PhysicalAddress, i))
            break;
        }
      }
      else
      {
          if ((isAMD && nvi.ID) || ((!isAMD) && (evi.X) && (evi.WasExecutable==0))) //execute operation and executable was 0
          {
            ID=i;

            if (ept_isWatchIDPerfectMatch(PhysicalAddress, i))
              break;
          }
      }
    }
  }


  //nosendchar[getAPICID()]=0;

  lastSeenEPTWatch.actualID=ID;
  sendstringf("%d: handling watch ID %d\n", currentcpuinfo->cpunr, ID);
  sendstringf("%d: RIP=%6\n", currentcpuinfo->cpunr, currentcpuinfo->vmcb->RIP);


  //todo: release the eptWatchListCS and obtain only the log

  //ID is now set to the most logical watch(usually there is no conflicts, and even if there is, no biggie. But still)

  PPTE_PAE npte;
  PEPT_PTE epte;

  lastSeenEPTWatch.cacheIssue=0;
  if (isAMD)
  {
    npte=(PPTE_PAE)currentcpuinfo->eptWatchList[ID];
    if ((npte->EXB==0) && (npte->P) && (npte->RW))
    {
      sendstringf("This entry was already marked with full access (check caches) (AMD)\n");
      lastSeenEPTWatch.cacheIssue=1;
    }
  }
  else
  {
    epte=currentcpuinfo->eptWatchList[ID];
    if ((epte->XA) && (epte->RA) && (epte->WA))
    {
      sendstringf("This entry was already marked with full access (check caches)\n");
      lastSeenEPTWatch.cacheIssue=1;
    }
  }


  if ((eptWatchList[ID].Options & EPTO_DBVMBP) && (PhysicalAddress>=eptWatchList[ID].PhysicalAddress) && (PhysicalAddress<eptWatchList[ID].PhysicalAddress+eptWatchList[ID].Size))
  {
    nosendchar[getAPICID()]=0;
    sendstringf("%d: EPTO_DBVMBP hit (RIP=%6)\n", currentcpuinfo->cpunr, isAMD?currentcpuinfo->vmcb->RIP:vmread(vm_guest_rip));
    //This is the specific address that was being requested
    //if the current state has interrupts disabled or masked (cr8<>0) then skip (todo: step until it is)

    DWORD CR8=getCR8();
    RFLAGS flags;
    flags.value=isAMD?currentcpuinfo->vmcb->RFLAGS:vmread(vm_guest_rflags);
    int is;
    int canBreak=(CR8==0) && (flags.IF); //interruptable with no mask (on windows called passive mode)

    if (isAMD)
    {
      sendstringf("CR8=%6 IF=%d RF=%d\n", CR8,flags.IF,flags.RF);
      canBreak=canBreak && (flags.RF==0); //on AMD I use the TF flag to skip over dbvmbp watches
    }
    else
    {
      is=vmread(vm_guest_interruptability_state);
      sendstringf("CR8=%6 IF=%d RF=%d Interruptibility state=%d\n", CR8,flags.IF,flags.RF, is);
      canBreak=canBreak && ((is & (1<<0))==0); //on Intel I use the block by sti interruptability state to flag a skip (probably can't use the pop ss as it's used by the single step handler. But should test)
    }

    sendstringf("canBreak=%d\n", canBreak);

    if (canBreak)
    {
      int kernelmode=0;
      if (isAMD)
      {
        Segment_Attribs csattrib;
        csattrib.SegmentAttrib=currentcpuinfo->vmcb->cs_attrib;
        kernelmode=csattrib.DPL==0;
      }
      else
      {
        Access_Rights csar;
        csar.AccessRights=vmread(vm_guest_cs_access_rights);
        kernelmode=csar.DPL==0;
      }

      QWORD newRIP=kernelmode?eptWatchList[ID].LoopKernelMode:eptWatchList[ID].LoopUserMode;

      if (newRIP) //e.g if no kernelmode is provided, skip kernelmode (needed for read/write watches as those will also see CE. Just trigger a COW please...)
      {

        //nosendchar[getAPICID()]=0;

        lastSeenEPTWatch.skipped=-1;
        csLeave(&eptWatchListCS);


        sendstringf("EPTO_DBVMBP: Interruptable state. 'Breaking' this code (Saving the state and setting it to RIP %6 )\n", newRIP);

        //save this thread's data in a structure so that when the int3 keepalive happens dbvm knows to skip it
        BrokenThreadEntry e;
        e.inuse=1;
        e.continueMethod=0;
        e.watchid=ID;
        e.UserModeLoop=eptWatchList[ID].LoopUserMode;
        e.KernelModeLoop=eptWatchList[ID].LoopKernelMode;

        fillPageEventBasic(&e.state.basic, registers);
        e.state.fpudata=*fxsave;

        if (isAMD)
          currentcpuinfo->vmcb->RIP=newRIP;
        else
          vmwrite(vm_guest_rip, newRIP);


        csEnter(&BrokenThreadListCS);
        if (BrokenThreadList==NULL)
        {
          //allocate the list
          BrokenThreadList=malloc(sizeof(BrokenThreadEntry)*8);
          BrokenThreadListSize=8;
          BrokenThreadListPos=0;
        }

        if (BrokenThreadListPos>=BrokenThreadListSize) //list full
        {
          BrokenThreadList=realloc(BrokenThreadList, sizeof(BrokenThreadEntry)*BrokenThreadListSize+32); //add 32
          BrokenThreadListSize+=32;
        }

        //find an unused spot, else add to the end
        for (i=0; i<BrokenThreadListPos; i++)
        {
          if (BrokenThreadList[i].inuse==0)
          {
            BrokenThreadList[i]=e;
            if (isAMD)
              currentcpuinfo->vmcb->RAX=i; //rax was already saved in the pageeventbasic structure. Use rax as brokenthreadlist id
            else
              registers->rax=i;

            csLeave(&BrokenThreadListCS);
            return TRUE; //no need to log it or continue
          }
        }

        //still here, so add it to the end
        BrokenThreadList[BrokenThreadListPos]=e;
        if (isAMD)
          currentcpuinfo->vmcb->RAX=BrokenThreadListPos;
        else
          registers->rax=BrokenThreadListPos;

        BrokenThreadListPos++;
        csLeave(&BrokenThreadListCS);

        return TRUE; //no need to log it or continue
      }
    }
    else
    {
      sendstring("Not breaking this\n");
    }

  }


  //run once

  sendstringf("%d Making page fully accessible\n", currentcpuinfo->cpunr);

  if (isAMD)
  {
    npte->EXB=0;  //execute allow
    npte->P=1;    //read allow
    npte->RW=1;   //write allow
  }
  else
  {
    currentcpuinfo->eptWatchList[ID]->XA=1; //execute allow
    currentcpuinfo->eptWatchList[ID]->RA=1; //read allow
    currentcpuinfo->eptWatchList[ID]->WA=1; //write allow
  }
  sendstringf("Page is accessible. Doing single step\n");

  vmx_enableSingleStepMode();
  vmx_addSingleSteppingReason(currentcpuinfo, SSR_HANDLEWATCH, ID);

  if (eptWatchList[ID].Options & EPTO_DBVMBP)
  {
    sendstringf("%d: Returning from ept_handleevent\n", currentcpuinfo->cpunr);
    csLeave(&eptWatchListCS);
    return TRUE;
  }

  /*todo:
  if ((eptWatchList[ID].Options & EPTO_DBVMBP) && (PhysicalAddress>=eptWatchList[ID].PhysicalAddress) && (PhysicalAddress<eptWatchList[ID].PhysicalAddress+eptWatchList[ID].Size))
  {
    //still happened? And kernelloop/userloop is valid? Then it's a step until interruptable
  }
  */

  if ((eptWatchList[ID].Options & EPTO_INTERRUPT) && (PhysicalAddress>=eptWatchList[ID].PhysicalAddress) && (PhysicalAddress<eptWatchList[ID].PhysicalAddress+eptWatchList[ID].Size))
  {
    //This is the specific address that was being requested
    lastSeenEPTWatch.skipped=1; //it's an interrupt triggering one (rare)
    currentcpuinfo->BPAfterStep=1;
    csLeave(&eptWatchListCS);
    return TRUE; //no need to log it
  }

  //save this state?
  if ((isAMD==0) && (eptWatchList[ID].Type!=EPTW_EXECUTE) && (evi.R==0) && (evi.X==1))
  {
    sendstringf("This was an execute operation and no read. No need to log\n", ID);
    lastSeenEPTWatch.skipped=2;
    csLeave(&eptWatchListCS);
    return TRUE; //execute operation (this cpu doesn't support execute only)
  }

  if (eptWatchList[ID].CopyInProgress) //a copy operation is in progress
  {
    lastSeenEPTWatch.skipped=3; //copy was in progress.
    eptWatchList[ID].Log->missedEntries++;
    sendstringf("This watchlist is currently being copied, not logging this\n");
    csLeave(&eptWatchListCS);
    return TRUE;
  }

  if (((eptWatchList[ID].Options & EPTO_LOG_ALL)==0) &&
     (
      (PhysicalAddress<eptWatchList[ID].PhysicalAddress) ||
      (PhysicalAddress>=eptWatchList[ID].PhysicalAddress+eptWatchList[ID].Size)
      ))
  {
    QWORD RIP=isAMD?currentcpuinfo->vmcb->RIP:vmread(vm_guest_rip);
    lastSeenEPTWatch.skipped=4; //not a perfect physical address match
    sendstringf("%d: Not logging all and the physical address(%6) is not in the exact range (%p-%p)\n", currentcpuinfo->cpunr, PhysicalAddress, eptWatchList[ID].PhysicalAddress, eptWatchList[ID].PhysicalAddress+eptWatchList[ID].Size);
    sendstringf("RIP was %6\n", RIP);
    csLeave(&eptWatchListCS);
    return TRUE; //no need to log it
  }

  lastSeenEPTWatchVerySure=lastSeenEPTWatch;



  //scan if this RIP is already in the list

  switch (eptWatchList[ID].Log->entryType)
  {
    case 0: logentrysize=sizeof(PageEventBasic); break;
    case 1: logentrysize=sizeof(PageEventExtended); break;
    case 2: logentrysize=sizeof(PageEventBasicWithStack); break;
    case 3: logentrysize=sizeof(PageEventExtendedWithStack); break;
  }

  sendstringf("Want to log this. Type=%d EntrySize=%d\n", eptWatchList[ID].Log->entryType, logentrysize);

  for (i=0; (DWORD)i<eptWatchList[ID].Log->numberOfEntries; i++)
  {
    PageEventBasic *peb=(PageEventBasic *)((QWORD)(&eptWatchList[ID].Log->pe.basic[0])+i*logentrysize);
    //every type starts with a PageEventBasic

    if (peb->RIP==RIP)
    {
      sendstringf("This RIP is already logged");
      //it's already in the list
      if ((eptWatchList[ID].Options & EPTO_MULTIPLERIP)==0)
      {
        sendstringf(" and EPTO_MULTIPLERIP is 0.  Not logging (just increase count)\n");
        peb->Count++;
        csLeave(&eptWatchListCS);

        lastSeenEPTWatch.skipped=5;
        lastSeenEPTWatchVerySure.skipped=5; //already logged
        return TRUE; //no extra RIP's
      }
      else
        sendstringf(" but EPTO_MULTIPLERIP is 1, so checking register states\n");

      //still here, so multiple RIP's are ok. check if it matches the other registers
      if (isAMD)
        registers->rax=currentcpuinfo->vmcb->RAX;

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
        csLeave(&eptWatchListCS);

        lastSeenEPTWatch.skipped=6;
        lastSeenEPTWatchVerySure.skipped=6; //already logged and not different

        return TRUE; //already in the list
      }

    }

  }

  //still here, so not in the list
  sendstringf("Checks out ok. Not yet in the list\n");

  if (eptWatchList[ID].Log->numberOfEntries>=eptWatchList[ID].Log->maxNumberOfEntries)
  {

    sendstringf("List is full.  (%d / %d) ", eptWatchList[ID].Log->numberOfEntries, eptWatchList[ID].Log->maxNumberOfEntries);
    if ((eptWatchList[ID].Options & EPTO_GROW_WHENFULL)==0)
    {
      sendstringf(". Discarding event\n");
      eptWatchList[ID].Log->missedEntries++;
      csLeave(&eptWatchListCS);
      lastSeenEPTWatch.skipped=7;
      lastSeenEPTWatchVerySure.skipped=7; //list full
      return TRUE; //can't add more
    }

    //reallocate the buffer

    int newmax=eptWatchList[ID].Log->numberOfEntries*2;
    PPageEventListDescriptor temp=realloc(eptWatchList[ID].Log, sizeof(PageEventListDescriptor)+logentrysize*newmax);
    if (temp!=NULL)
    {
      sendstringf(" so growing it\n");
      eptWatchList[ID].Log=temp;
      eptWatchList[ID].Log->numberOfEntries=newmax;
    }
    else
    {
      sendstringf(" and out of memory (fuuuuu)\n");

      eptWatchList[ID].Options=eptWatchList[ID].Options & (~EPTO_GROW_WHENFULL); //stop trying
      eptWatchList[ID].Log->missedEntries++;
      csLeave(&eptWatchListCS);

      lastSeenEPTWatch.skipped=8; //list full and out of memory
      lastSeenEPTWatchVerySure.skipped=8;
      return TRUE; //can't add more
    }

  }

  //still here, so not in the list, and still room
  //add it



  i=eptWatchList[ID].Log->numberOfEntries;
  switch (eptWatchList[ID].Log->entryType)
  {
    case PE_BASIC:
    {
      fillPageEventBasic(&eptWatchList[ID].Log->pe.basic[i], registers);
      break;
    }

    case PE_EXTENDED:
    {
      fillPageEventBasic(&eptWatchList[ID].Log->pe.extended[i].basic, registers);
      eptWatchList[ID].Log->pe.extended[i].fpudata=*fxsave;
      break;
    }

    case PE_BASICSTACK:
    {
      fillPageEventBasic(&eptWatchList[ID].Log->pe.basics[i].basic, registers);
      saveStack(currentcpuinfo, eptWatchList[ID].Log->pe.basics[i].stack);
      break;
    }

    case PE_EXTENDEDSTACK:
    {
      fillPageEventBasic(&eptWatchList[ID].Log->pe.extendeds[i].basic, registers);
      eptWatchList[ID].Log->pe.extendeds[i].fpudata=*fxsave;
      saveStack(currentcpuinfo, eptWatchList[ID].Log->pe.extendeds[i].stack);
      break;
    }
  }

  eptWatchList[ID].Log->numberOfEntries++;

  lastSeenEPTWatch.skipped=0;
  lastSeenEPTWatchVerySure.skipped=0; //got added to the list

  sendstringf("Added it to the list. numberOfEntries for ID %d is now %d\n", ID, eptWatchList[ID].Log->numberOfEntries);

  csLeave(&eptWatchListCS);

  return TRUE;

}

int ept_handleWatchEventAfterStep(pcpuinfo currentcpuinfo,  int ID)
{
  sendstringf("%d ept_handleWatchEventAfterStep %d  Type=%d\n", currentcpuinfo->cpunr, ID, eptWatchList[ID].Type);

  if (isAMD)
    sendstringf("%d CS:RIP=%x:%6\n", currentcpuinfo->cpunr, currentcpuinfo->vmcb->cs_selector, currentcpuinfo->vmcb->RIP);

  if (ID>eptWatchListPos)
  {
    sendstring("Invalid ID\n");
    return 0;
  }


  if (eptWatchList[ID].Active==0)
  {
    sendstring("Inactive ID\n");
    return 0;
  }




  switch (eptWatchList[ID].Type)
  {
  	  case EPTW_WRITE:
  	  {
  	    sendstringf("Write type. So making it unwritable\n");

  	    if (isAMD)
  	    {
  	      PPTE_PAE pte;
  	      pte=(PPTE_PAE)currentcpuinfo->eptWatchList[ID];
  	      if (pte)
  	      {
            UINT64 oldvalue=*(UINT64 *)pte;
            pte->RW=0;

            UINT64 newvalue=*(UINT64 *)pte;

            sendstringf("%6 -> %6\n", oldvalue, newvalue);
  	      }

  	    }
  	    else
  	      currentcpuinfo->eptWatchList[ID]->WA=0;
  		  break;
  	  }

  	  case EPTW_READWRITE:
  	  {
  	    sendstringf("read type. So making it unreadable\n");
  	    if (isAMD)
  	    {
  	      PPTE_PAE pte;
  	      pte=(PPTE_PAE)currentcpuinfo->eptWatchList[ID];
  	      pte->P=0;
  	    }
  	    else
  	    {
          currentcpuinfo->eptWatchList[ID]->RA=0;
          currentcpuinfo->eptWatchList[ID]->WA=0;
          if (has_EPT_ExecuteOnlySupport)
            currentcpuinfo->eptWatchList[ID]->XA=1;
          else
            currentcpuinfo->eptWatchList[ID]->XA=0;
  	    }

  		  break;
  	  }

  	  case EPTW_EXECUTE:
  	  {

  	    sendstringf("%d: execute type. So making it non-executable\n", currentcpuinfo->cpunr);
        if (isAMD)
        {
          PPTE_PAE pte;
          pte=(PPTE_PAE)currentcpuinfo->eptWatchList[ID];
          pte->EXB=1;
        }
        else
          currentcpuinfo->eptWatchList[ID]->XA=0;

  		  break;
  	  }
  }

  //todo: If enabled , and the watch actually got hit, trigger a DBG interrupt
  //      Keep in mind that CE reading memory may also trigger access interrupts so those need to be
  //      ignored by the driver


  if (currentcpuinfo->BPAfterStep)
  {
    if (isAMD)
    {
      sendstringf("AMD: BP after STEP\n");
      currentcpuinfo->vmcb->inject_Type=3; //exception
      currentcpuinfo->vmcb->inject_Vector=int1redirection;
      currentcpuinfo->vmcb->inject_Valid=1;
      currentcpuinfo->vmcb->inject_EV=0;
    }
    else
    {
      if (int1redirection_idtbypass)
      {
        regDR7 dr7;
        //disable GD bit before int1
        dr7.DR7=vmread(vm_guest_dr7);
        dr7.GD=0;
        vmwrite(vm_guest_dr7,dr7.DR7);

        emulateExceptionInterrupt(currentcpuinfo, NULL, int1redirection_idtbypass_cs, int1redirection_idtbypass_rip, 0, 0, 0);
      }
      else
      {
        vmwrite(vm_pending_debug_exceptions,0x4000); //for OS'es without the need for int1 redirects
      }
    }

    currentcpuinfo->BPAfterStep=0;
    currentcpuinfo->BPCausedByDBVM=1;

  }

  sendstring("Calling ept_invalidate\n");

  ept_invalidate();
  return 0;
}



VMSTATUS ept_traceonbp_retrievelog(QWORD results, DWORD *resultSize, DWORD *offset, QWORD *errorcode)
//same as ept_watch_retrievelog, but there's only only one list
{

  //sendstringf("ept_watch_retrievelog(ID=%d)\n", ID);

  sendstring("ept_traceonbp_retrievelog\n");

  csEnter(&CloakedPagesCS);

  DWORD sizeneeded=8;
  int maxid=TraceOnBP->numberOfEntries;

  switch (TraceOnBP->datatype)
  {
    case PE_BASIC:
      sizeneeded+=(QWORD)(&TraceOnBP->pe.basic[maxid])-(QWORD)(&TraceOnBP->datatype);
      break;

    case PE_EXTENDED:
      sizeneeded+=(QWORD)(&TraceOnBP->pe.extended[maxid])-(QWORD)(&TraceOnBP->datatype);
      break;

    case 2:
      sizeneeded+=(QWORD)(&TraceOnBP->pe.basics[maxid])-(QWORD)(&TraceOnBP->datatype);
      break;

    case 3:
      sizeneeded+=(QWORD)(&TraceOnBP->pe.extendeds[maxid])-(QWORD)(&TraceOnBP->datatype);
      break;
  }

  sendstringf("sizeneeded=%d  *resultSize=%d\n", sizeneeded, *resultSize);


  if ((*resultSize) < sizeneeded)
  {
    sendstringf("Too small\n");
    *resultSize=sizeneeded;
    if (isAMD)
    {
      if (AMD_hasNRIPS)
        getcpuinfo()->vmcb->RIP=getcpuinfo()->vmcb->nRIP;
      else
        getcpuinfo()->vmcb->RIP+=3;
    }
    else
      vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));
    *errorcode=2; //invalid size
    csLeave(&CloakedPagesCS);
    return VM_OK;
  }



  if (results==0)
  {
    sendstringf("results==0\n");
    if (isAMD)
    {
      if (AMD_hasNRIPS)
        getcpuinfo()->vmcb->RIP=getcpuinfo()->vmcb->nRIP;
      else
        getcpuinfo()->vmcb->RIP+=3;
    }
    else
      vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));
    *errorcode=4; //results==0
    csLeave(&CloakedPagesCS);
    return VM_OK;
  }

  if ((*offset)>sizeneeded)
  {
    sendstringf("(*offset)>sizeneeded\n");
    if (isAMD)
    {
      if (AMD_hasNRIPS)
        getcpuinfo()->vmcb->RIP=getcpuinfo()->vmcb->nRIP;
      else
        getcpuinfo()->vmcb->RIP+=3;
    }
    else
      vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));
    *errorcode=6; //offset is too high
    csLeave(&CloakedPagesCS);
    return VM_OK;
  }



  int sizeleft=sizeneeded-(*offset); //decrease bytes left by bytes already copied
  sendstringf("*offset=%d\n", *offset);
  sendstringf("sizeleft=%d\n", sizeleft);

  int error;
  QWORD pagefaultaddress;
  QWORD destinationaddress=results+(*offset);
  int blocksize=sizeleft;
  if (blocksize>16*4096)
    blocksize=16*4096;


  sendstringf("TraceOnBP->datatype=%d\n",TraceOnBP->datatype);
  sendstringf("TraceOnBP->numberOfEntries=%d\n",TraceOnBP->numberOfEntries);

  unsigned char *source=(unsigned char *)(QWORD)(&TraceOnBP->datatype)+(*offset);
  unsigned char *destination=mapVMmemoryEx(NULL, destinationaddress, blocksize, &error, &pagefaultaddress,1);


  if (error)
  {
    sendstringf("Error during map (%d)\n", error);
    if (error==2)
    {
      sendstringf("Pagefault at address %x\n", pagefaultaddress);
      blocksize=pagefaultaddress-destinationaddress;
      sendstringf("blocksize=%d\n", blocksize);
    }
    else
    {
      sendstringf("Not a pagefault\n");
      if (isAMD)
      {
        if (AMD_hasNRIPS)
          getcpuinfo()->vmcb->RIP=getcpuinfo()->vmcb->nRIP;
        else
          getcpuinfo()->vmcb->RIP+=3;
      }
      else
        vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));
      *errorcode=0x1000+error; //map error
      csLeave(&CloakedPagesCS);
      return VM_OK;
    }
  }



  if (blocksize)
  {
    //sendstringf("Copying to destination\n");
    copymem(destination, source, blocksize);
    unmapVMmemory(destination, blocksize);

    *offset=(*offset)+blocksize;
  }

  if (error==2)
  {
    sendstringf("Raising the pagefault\n");
    csLeave(&CloakedPagesCS);
    return raisePagefault(getcpuinfo(), pagefaultaddress);
  }


  if ((*offset)>=sizeneeded)
  {
    //once all data has been copied
    sendstringf("All data has been copied\n");
    *resultSize=*offset;

   // sendstringf("Going to the next instruction\n");
    if (isAMD)
    {
      if (AMD_hasNRIPS)
        getcpuinfo()->vmcb->RIP=getcpuinfo()->vmcb->nRIP;
      else
        getcpuinfo()->vmcb->RIP+=3;
    }
    else
      vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));

    *errorcode=0;
  }
  else
  {
    sendstringf("not everything copied yet. Rerun\n");

  }

  csLeave(&CloakedPagesCS);
  return VM_OK;
}

VMSTATUS ept_watch_retrievelog(int ID, QWORD results, DWORD *resultSize, DWORD *offset, QWORD *errorcode)
/*
 * Retrieves the collected log
 * offset is the offset from what point the log should continue copying (works like a rep xxx instruction)
 */
{
  if (ID>=eptWatchListPos) //out of range
  {
    sendstringf("Invalid ID\n");


    if (isAMD)
    {
      if (AMD_hasNRIPS)
        getcpuinfo()->vmcb->RIP=getcpuinfo()->vmcb->nRIP;
      else
        getcpuinfo()->vmcb->RIP+=3;
    }
    else
      vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));

    *errorcode=1; //invalid ID
    return VM_OK;
  }

  csEnter(&eptWatchListCS);

  if (eptWatchList[ID].Active==0) //not active
  {
    sendstringf("Inactive ID\n");
    if (isAMD)
    {
      if (AMD_hasNRIPS)
        getcpuinfo()->vmcb->RIP=getcpuinfo()->vmcb->nRIP;
      else
        getcpuinfo()->vmcb->RIP+=3;
    }
    else
      vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));
    *errorcode=3; //inactive ID

    csLeave(&eptWatchListCS);
    return VM_OK;
  }


 // int entrysize=0;
  DWORD sizeneeded=sizeof(PageEventListDescriptor);
  int maxid=eptWatchList[ID].Log->numberOfEntries;

  switch (eptWatchList[ID].Log->entryType)
  {
    case 0:
      sizeneeded+=(QWORD)(&eptWatchList[ID].Log->pe.basic[maxid])-(QWORD)(&eptWatchList[ID].Log->pe.basic[0]);
      break;

    case 1:
      sizeneeded+=(QWORD)(&eptWatchList[ID].Log->pe.extended[maxid])-(QWORD)(&eptWatchList[ID].Log->pe.extended[0]);
      break;

    case 2:
      sizeneeded+=(QWORD)(&eptWatchList[ID].Log->pe.basics[maxid])-(QWORD)(&eptWatchList[ID].Log->pe.basics[0]);
      break;

    case 3:
      sizeneeded+=(QWORD)(&eptWatchList[ID].Log->pe.extendeds[maxid])-(QWORD)(&eptWatchList[ID].Log->pe.extendeds[0]);
      break;
  }

  if ((*resultSize) < sizeneeded)
  {
    sendstringf("Too small\n");
    *resultSize=sizeneeded;
    if (isAMD)
    {
      if (AMD_hasNRIPS)
        getcpuinfo()->vmcb->RIP=getcpuinfo()->vmcb->nRIP;
      else
        getcpuinfo()->vmcb->RIP+=3;
    }
    else
      vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));
    *errorcode=2; //invalid size
    csLeave(&eptWatchListCS);
    return VM_OK;
  }



  if (results==0)
  {
    sendstringf("results==0\n");
    if (isAMD)
    {
      if (AMD_hasNRIPS)
        getcpuinfo()->vmcb->RIP=getcpuinfo()->vmcb->nRIP;
      else
        getcpuinfo()->vmcb->RIP+=3;
    }
    else
      vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));
    *errorcode=4; //results==0
    csLeave(&eptWatchListCS);
    return VM_OK;
  }

#ifdef MEMORYCHECKNOLOGRETRIEVAL
  //skip
  if (isAMD)
  {
    if (AMD_hasNRIPS)
      getcpuinfo()->vmcb->RIP=getcpuinfo()->vmcb->nRIP;
    else
      getcpuinfo()->vmcb->RIP+=3;
  }
  else
    vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));
  *resultSize=0;
  *errorcode=0; //results==0
  csLeave(&eptWatchListCS);
  return VM_OK;
#endif

  if ((*offset) && (eptWatchList[ID].CopyInProgress==0))
  {
    if (isAMD)
    {
      if (AMD_hasNRIPS)
        getcpuinfo()->vmcb->RIP=getcpuinfo()->vmcb->nRIP;
      else
        getcpuinfo()->vmcb->RIP+=3;
    }
    else
      vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));
    *errorcode=5; //offset set but not copyinprogress
    csLeave(&eptWatchListCS);
    return VM_OK;
  }

  if ((*offset)>sizeneeded)
  {
    if (isAMD)
    {
      if (AMD_hasNRIPS)
        getcpuinfo()->vmcb->RIP=getcpuinfo()->vmcb->nRIP;
      else
        getcpuinfo()->vmcb->RIP+=3;
    }
    else
      vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));
    *errorcode=6; //offset is too high
    csLeave(&eptWatchListCS);
    return VM_OK;
  }



  int sizeleft=sizeneeded-(*offset); //decrease bytes left by bytes already copied

 // sendstringf("*offset=%d\n", *offset);
  //sendstringf("sizeleft=%d\n", sizeleft);
  eptWatchList[ID].CopyInProgress=1;

  int error;
  QWORD pagefaultaddress;
  QWORD destinationaddress=results+(*offset);
  int blocksize=sizeleft;
  if (blocksize>16*4096)
    blocksize=16*4096;

  //sendstringf("destinationaddress=%6\n", destinationaddress);

  unsigned char *source=(unsigned char *)(((QWORD)eptWatchList[ID].Log)+(*offset));
  unsigned char *destination=mapVMmemoryEx(NULL, destinationaddress, blocksize, &error, &pagefaultaddress,1);


  if (error)
  {
    sendstringf("Error during map (%d)\n", error);
    if (error==2)
    {
      sendstringf("Pagefault at address %x\n", pagefaultaddress);
      blocksize=pagefaultaddress-destinationaddress;
      sendstringf("blocksize=%d\n", blocksize);
    }
    else
    {
      sendstringf("Not a pagefault\n");
      if (isAMD)
      {
        if (AMD_hasNRIPS)
          getcpuinfo()->vmcb->RIP=getcpuinfo()->vmcb->nRIP;
        else
          getcpuinfo()->vmcb->RIP+=3;
      }
      else
        vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));
      *errorcode=0x1000+error; //map error
      csLeave(&eptWatchListCS);
      return VM_OK;
    }
  }



  if (blocksize)
  {
    //sendstringf("Copying to destination\n");
    copymem(destination, source, blocksize);
    unmapVMmemory(destination, blocksize);
#ifdef MEMORYCHECK
    //mark log as 0xce
    QWORD a,b;

    b=(QWORD)eptWatchList[ID].Log+sizeof(PageEventListDescriptor);
    int x;
    for (x=0; x<blocksize; x++)
    {
      a=(QWORD)source+x;
      if (a>=b)
        source[x]=0xce;
    }
#endif



    *offset=(*offset)+blocksize;
  }

  if (error==2)
  {
    sendstringf("Raising the pagefault\n");
    csLeave(&eptWatchListCS);
    return raisePagefault(getcpuinfo(), pagefaultaddress);
  }

 //sendstringf("new *offset=%d", *offset);
  //sendstringf("sizeneeded=%d", sizeneeded);

  if ((*offset)>=sizeneeded)
  {
    //once all data has been copied
   // sendstringf("All data has been copied\n");
    eptWatchList[ID].Log->numberOfEntries=0;
    eptWatchList[ID].CopyInProgress=0;

    *resultSize=*offset;

   // sendstringf("Going to the next instruction\n");
    if (isAMD)
    {
      if (AMD_hasNRIPS)
        getcpuinfo()->vmcb->RIP=getcpuinfo()->vmcb->nRIP;
      else
        getcpuinfo()->vmcb->RIP+=3;
    }
    else
      vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));
    *errorcode=0;
  }
  else
  {
    //sendstringf("not everything copied yet. Rerun\n");

  }

  csLeave(&eptWatchListCS);
  return VM_OK;
}


int ept_watch_activate(QWORD PhysicalAddress, int Size, int Type, DWORD Options, int MaxEntryCount, int *outID, QWORD OptionalField1, QWORD OptionalField2)
{
  int result=0;
  sendstringf("+ ept_watch_activate(%6, %d, %d, %x, %d, %6, %6,%6)\n", PhysicalAddress, Size, Options, MaxEntryCount, outID, OptionalField1, OptionalField2);
// ept_watch_activate(00000004030190d8, 8, 1, 20, 8390248, 0000000000000000, 0000000000000000,0000008000800668)

  if ((MaxEntryCount==0) && (((EPTO_INTERRUPT|EPTO_DBVMBP) & Options)==0) )
  {
    sendstringf("MaxEntryCount=0\n");
    return 1;
  }
  else
    sendstringf("MaxEntryCount=%d (this is ok)\n", MaxEntryCount);

  csEnter(&eptWatchListCS);

  int ID=getFreeWatchID();
  int structtype=(Options >> 2) & 3;
  int structsize;



  sendstringf("getFreeWatchID() returned %d .  eptWatchListPos=%d\n", ID, eptWatchListPos);
  switch (structtype)
  {
    case 0: structsize=sizeof(PageEventBasic); break;             //EPTO_SAVE_XSAVE=0 and EPTO_SAVE_STACK=0
    case 1: structsize=sizeof(PageEventExtended); break;          //EPTO_SAVE_XSAVE=1 and EPTO_SAVE_STACK=0
    case 2: structsize=sizeof(PageEventBasicWithStack); break;    //EPTO_SAVE_XSAVE=0 and EPTO_SAVE_STACK=1
    case 3: structsize=sizeof(PageEventExtendedWithStack); break; //EPTO_SAVE_XSAVE=1 and EPTO_SAVE_STACK=1
  }

  //make sure it doesn't pass a page boundary
  //todo: recursively spawn more watches if needed

  if (((PhysicalAddress+Size) & 0xfffffffffffff000ULL) > (PhysicalAddress & 0xfffffffffffff000ULL))
    eptWatchList[ID].Size=0x1000-(PhysicalAddress & 0xfff);
  else
    eptWatchList[ID].Size=Size;

  eptWatchList[ID].PhysicalAddress=PhysicalAddress;
  eptWatchList[ID].Type=Type;
  if (Options & EPTO_DBVMBP)
  {
    eptWatchList[ID].LoopUserMode=OptionalField1;
    eptWatchList[ID].LoopKernelMode=OptionalField2;
  }


  eptWatchList[ID].Log=malloc(sizeof(PageEventListDescriptor)*2+structsize*MaxEntryCount); //*2 because i'm not sure how the alignment of the final entry goes
  zeromemory(eptWatchList[ID].Log, sizeof(PageEventListDescriptor)*2+structsize*MaxEntryCount);


  eptWatchList[ID].Log->ID=ID;
  eptWatchList[ID].Log->entryType=structtype;
  eptWatchList[ID].Log->numberOfEntries=0;
  eptWatchList[ID].Log->maxNumberOfEntries=MaxEntryCount;
  eptWatchList[ID].Log->missedEntries=0;

  eptWatchList[ID].Options=Options;
  sendstringf("Configured ept watch. Activating ID %d\n", ID);

  eptWatchList[ID].Active=1;

  //for each CPU mark this page as non writable/readable
  pcpuinfo c=firstcpuinfo;
  while (c)
  {
    QWORD PA_PTE;

    sendstringf("Setting watch for CPU %d\n", c->cpunr);
    csEnter(&c->EPTPML4CS);

    if (isAMD)
    {
      PA_PTE=NPMapPhysicalMemory(c, PhysicalAddress, 1);
      sendstringf("PA_PTE=%6\n", PA_PTE);
    }
    else
      PA_PTE=EPTMapPhysicalMemory(c, PhysicalAddress, 1);


    if (c->eptWatchListLength<eptWatchListSize) //realloc
      c->eptWatchList=realloc(c->eptWatchList, eptWatchListSize*sizeof(EPT_PTE));

    c->eptWatchList[ID]=mapPhysicalMemoryGlobal(PA_PTE, sizeof(EPT_PTE)); //can use global as it's not a quick map/unmap procedure

    if (isAMD)
    {
      //AMD NP
      _PTE_PAE temp=*(_PTE_PAE *)c->eptWatchList[ID];
      sendstringf("Old page value was %6\n", *(QWORD *)c->eptWatchList[ID]);

      if (Type==EPTW_WRITE)
      {
        temp.RW=0;
      }
      else
      if (Type==EPTW_READWRITE)
      {
        temp.P=0; //not present, ANY access, including execute
      }
      else
      if (Type==EPTW_EXECUTE)
      {
        temp.EXB=1; //no execute flag
      }
      *(c->eptWatchList[ID])=*(EPT_PTE *)&temp;
      sendstringf("New page value is %6\n", *(QWORD *)c->eptWatchList[ID]);
    }
    else
    {

      //Intel EPT
      EPT_PTE temp=*c->eptWatchList[ID]; //using temp in case the cpu doesn't support a XA of 1 with an RA of 0

      if (Type==EPTW_WRITE) //Writes
        temp.WA=0;
      else
      if (Type==EPTW_READWRITE) //read and writes
      {
        if (has_EPT_ExecuteOnlySupport)
          temp.XA=1;
        else
          temp.XA=0;
        temp.WA=0;
        temp.RA=0;
      }

      if (Type==EPTW_EXECUTE) //executes
      {
        temp.XA=0;
      }
      *(c->eptWatchList[ID])=temp;

    }



    _wbinvd();
    c->eptUpdated=1;
    csLeave(&c->EPTPML4CS);

    c=c->next;
  }

  //test if needed:  SetPageToWriteThrough(currentcpuinfo->eptwatchlist[ID].EPTEntry);
  //check out 28.3.3.4  (might not be needed due to vmexit, but do check anyhow)
  //yes, is needed
  //EPTINV



  //everything ok, return success:

  sendstringf("Passing ID(%d) to the caller\n", ID);
  *outID=ID;


  csLeave(&eptWatchListCS);

  sendstringf("Invalidating pages\n");
  ept_invalidate();

  return result;
}

int ept_watch_deactivate(int ID)
/*
 * disable a watch
 * It's possible that a watch event is triggered between now and the end of this function.
 * That will result in a ept_violation event, which causes it to not see that there is a watch event for this
 * As a result, that DBVM thread will try to map the physical address , and if it's already mapped, set it to full access
 * In short: it's gonna be ok
 */
{
  int i;
  int hasAnotherOne=0;
  sendstringf("ept_watch_deactivate(%d)", ID);

  csEnter(&eptWatchListCS);

  if (ID>=eptWatchListPos)
  {
    sendstringf("  Invalid entry\n");
    csLeave(&eptWatchListCS);
    return 1;
  }

  if (eptWatchList[ID].Active==0)
  {
    sendstringf("  Inactive entry\n");
    csLeave(&eptWatchListCS);
    return 2;
  }

  QWORD PhysicalBase=eptWatchList[ID].PhysicalAddress & 0xfffffffffffff000ULL;

  for (i=0; i<eptWatchListPos; i++)
  {
    if ((i!=ID) && ept_isWatchIDMatch(PhysicalBase, i))
    {
      //matches
      if (eptWatchList[i].Type==eptWatchList[ID].Type) //don't undo
        hasAnotherOne=1;
    }
  }

  if (hasAnotherOne==0)
  {
    pcpuinfo c=firstcpuinfo;

    while (c)
    {
      //undo

      csEnter(&c->EPTPML4CS);

      if (isAMD)
      {
        _PTE_PAE temp=*(PPTE_PAE)(c->eptWatchList[ID]);
        if (eptWatchList[ID].Type==EPTW_WRITE)
        {
          temp.RW=1; //back to writable
        }
        else if (eptWatchList[ID].Type==EPTW_READWRITE)
        {
          temp.P=1;
          temp.RW=1;
          temp.EXB=0;
        }
        else
        {
          temp.EXB=0;
        }

        *(PPTE_PAE)(c->eptWatchList[ID])=temp;
      }
      else
      {

        EPT_PTE temp=*(c->eptWatchList[ID]);
        if (eptWatchList[ID].Type==EPTW_WRITE)
        {
          sendstringf("  This was a write entry. Making it writable\n");
          temp.WA=1;
        }
        else if (eptWatchList[ID].Type==EPTW_READWRITE)
        {
          sendstringf("  This was an access entry. Making it readable and writable");
          temp.RA=1;
          temp.WA=1;
          if (has_EPT_ExecuteOnlySupport==0)
          {
            sendstringf(" and executable as this cpu does not support execute only pages\n");
            temp.XA=1;
          }
        }
        else
        {
            sendstringf("  This was an execute entry. Making it executable");
            temp.XA=1;
        }



        *(c->eptWatchList[ID])=temp;
      }
      _wbinvd();
      c->eptUpdated=1;

      csLeave(&c->EPTPML4CS);

      unmapPhysicalMemoryGlobal(c->eptWatchList[ID], sizeof(EPT_PTE));
      c->eptWatchList[ID]=NULL;
      c=c->next;
    }

    ept_invalidate();
  }
  else
  {
    sendstringf("  hasAnotherOne is set\n");
  }

  eptWatchList[ID].Active=0;
  free(eptWatchList[ID].Log);
  eptWatchList[ID].Log=NULL;

  csLeave(&eptWatchListCS);
  return 0;
}


int MTC_RPS(int mt1, int mt2)
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

criticalSection memoryrangesCS={.name="memoryrangesCS", .debuglevel=2};
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
  int i;

  *memtype=MTRRDefType.TYPE; //if not found, this is the result (usually uncached)
  *fullmap=1;

 // sendstringf("getMTRRMapInfo(%6, %x)\n", startaddress, size);


  //csEnter(&memoryrangesCS); //currently addToMemoryRanges is only called BEFORE ept exceptions happen. So this cs is not neede
  for (i=0; i<memoryrangesPos; i++)
  {
    QWORD startb,stopb;
    startb=memoryranges[i].startaddress;
    stopb=memoryranges[i].startaddress+memoryranges[i].size-1;

    if ((starta <= stopb) && (startb <= stopa))
    {
      //overlap, check the details
      if ((starta>=startb) && (stopa<=stopb)) //falls completely within the region, so can be fully mapped.
        *memtype=memoryranges[i].memtype;//set the memory type
      else
        *fullmap=0; //mark as not fully mappable, go one level lower

      return;
    }

    if (stopa<startb) //reached a startaddress higher than my stopaddress, which means every other item will be as well
      return;
  }
  //csLeave(&memoryrangesCS);
}



void addToMemoryRanges(QWORD address, QWORD size, int type)
/*
 * pre: memoryrangesCS lock has been aquired
 */
{
  if (size==0) return;

  //add memory for a new entry
  if (memoryrangesPos==memoryrangesLength)
  {
    sendstringf("addToMemoryRanges realloc\n");
    memoryranges=realloc2(memoryranges, memoryrangesLength*sizeof(MEMRANGE), 2*memoryrangesLength*sizeof(MEMRANGE));
    memoryrangesLength=memoryrangesLength*2;
  }

  memoryranges[memoryrangesPos].startaddress=address;
  memoryranges[memoryrangesPos].size=size;
  memoryranges[memoryrangesPos].memtype=type;

  memoryrangesPos++;
}

void sanitizeMemoryRegions()
{
  //find overlapping regions and calculate the best memtype
  //----------------------------------------------------------
  //|
  //|
  int i=0,j;



  for (i=0; i<memoryrangesPos; i++)
  {
    QWORD starta=memoryranges[i].startaddress;
    QWORD stopa=memoryranges[i].startaddress+memoryranges[i].size-1;

    if (memoryranges[i].size==0)
      continue;

    if (i>100)
    {
      while (1)
      {
        outportb(0x80,0x10);
        sendstringf("It's OVER");
        outportb(0x80,0x00);
        sendstringf(" 100!!!!!!\n");
      }

    }

    sendstringf("Checking %d (%6 - %6):%d for overlap\n", i, starta, stopa, memoryranges[i].memtype);

    j=i+1;
    while (j<memoryrangesPos)
    {
      if (j==i) continue;
      if (memoryranges[j].size==0)
        continue;

      if (j>100)
      {
        while (1)
        {
          outportb(0x80,0x11);
          sendstringf("It's OVER");
          outportb(0x80,0x10);
          sendstringf(" 100 again!!!!!!\n");
        }
      }


      QWORD startb=memoryranges[j].startaddress;
      QWORD stopb=memoryranges[j].startaddress+memoryranges[j].size-1;

      if ((starta <= stopb) && (startb <= stopa))
      {
        //3 parts: left, overlap, right.  Left and right can be 0 width
        MEMRANGE left;
        MEMRANGE overlap;
        MEMRANGE right;
        left.size=0;
        left.memtype=0;
        left.startaddress=0;

        right.size=0;
        right.memtype=0;
        right.startaddress=0;

        //overlaps
        QWORD newstart;
        QWORD newstop;

        sendstringf("  Overlaps with %d (%6 - %6):%d\n", j, startb, stopb, memoryranges[j].memtype);

        //left:
        newstart=minq(starta,startb);
        newstop=maxq(starta,startb);

        left.startaddress=newstart;
        left.size=newstop-newstart;
        left.memtype=starta<startb?memoryranges[i].memtype:memoryranges[j].memtype;

        //right:
        newstart=minq(stopa,stopb)+1;
        newstop=maxq(stopa,stopb)+1;

        sendstringf("    debug: right: newstart=%6 newstop=%6\n", newstart, newstop);

        right.startaddress=newstart;
        right.size=newstop-newstart;
        right.memtype=stopb<stopa?memoryranges[i].memtype:memoryranges[j].memtype;

        overlap.startaddress=left.startaddress+left.size;
        overlap.size=right.startaddress-overlap.startaddress;
        overlap.memtype=MTC_RPS(memoryranges[i].memtype, memoryranges[j].memtype);


        if (left.size)
        {
          addToMemoryRanges(left.startaddress, left.size, left.memtype);
          sendstringf("    Left becomes (%6 - %6):%d\n", left.startaddress, left.startaddress+left.size-1, left.memtype);
        }
        else
          sendstringf("    Left is empty\n");


        if (right.size)
        {
          addToMemoryRanges(right.startaddress, right.size, right.memtype);
          sendstringf("    Right becomes (%6 - %6):%d\n", right.startaddress, right.startaddress+right.size-1, right.memtype);
        }
        else
          sendstringf("    Right is empty\n");

        //adjust the current entry
        memoryranges[i].startaddress=overlap.startaddress;
        memoryranges[i].size=overlap.size;
        memoryranges[i].memtype=overlap.memtype;
        sendstringf("    This becomes (%6 - %6):%d\n", memoryranges[i].startaddress, memoryranges[i].startaddress+memoryranges[i].size-1, memoryranges[i].memtype);

        starta = memoryranges[i].startaddress;
        stopa = memoryranges[i].startaddress + memoryranges[i].size - 1;


        //mark as handled
        int k;
        for (k=j; k<memoryrangesPos-1; k++)
          memoryranges[k]=memoryranges[k+1];

        memoryrangesPos--;

        continue;
      }
      j++;
    }
  }

  //now that the list has been sanitized delete entries with the same type as the default (not before)
  i=0;
  while (i<memoryrangesPos)
  {
    if (memoryranges[i].memtype==MTRRDefType.TYPE)
    {
      for (j=i; j<memoryrangesPos-1; j++)
        memoryranges[j]=memoryranges[j+1];

      memoryrangesPos--;
      continue;
    }
    i++;
  }


  //sort the list
  for (i=0; i<memoryrangesPos; i++)
  {
    for (j=i; j<memoryrangesPos; j++)
    {
      if (memoryranges[j].startaddress<memoryranges[i].startaddress)
      {
        //swap
        MEMRANGE temp=memoryranges[i];
        memoryranges[i]=memoryranges[j];
        memoryranges[j]=temp;
      }
    }
  }
}


void initMemTypeRanges()
//builds an array of memory ranges and their cache
{
  int i;
  if (memoryrangesPos)
    return; //already initialized

  csEnter(&memoryrangesCS);

  memoryrangesPos=0;

  if (memoryranges==NULL)
  {
    memoryrangesLength=32;
    memoryranges=malloc2(sizeof(MEMRANGE)*memoryrangesLength);
  }

  sendstringf("Memory ranges:\n");


  QWORD startaddress=0;
  QWORD size=0;
  int memtype=-1;

  if ((MTRRCapabilities.FIX && MTRRDefType.FE))
  {
    sendstringf("Using Fixed MTRRs\n");

    QWORD FIX64K_00000=readMSR(IA32_MTRR_FIX64K_00000);  //0606060606060606
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
      QWORD types;
      int type;
      int sizeinc=0;

      switch (startaddress+size)
      {
        case 0 ... 0x7ffff:
        {
          types=FIX64K_00000;
          int index=(startaddress+size) >> 16;
          type=(types >> (index*8)) & 0xf;
          sizeinc=64*1024;
          break;
        }

        case 0x80000 ... 0x9ffff:
        {
          types=FIX16K_80000;
          int index=((startaddress+size)-0x80000) >> 14;
          type=(types >> (index*8)) & 0xf;
          sizeinc=16*1024;
          break;
        }

        case 0xa0000 ... 0xbffff:
        {
          types=FIX16K_A0000;
          int index=((startaddress+size)-0xa0000) >> 14;
          type=(types >> (index*8)) & 0xf;
          sizeinc=16*1024;
          break;
        }

        case 0xc0000 ... 0xc7fff:
        {
          types=FIX4K_C0000;
          int index=((startaddress+size)-0xc0000) >> 12;
          type=(types >> (index*8)) & 0xf;
          sizeinc=4*1024;
          break;
        }

        case 0xc8000 ... 0xcffff:
        {
          types=FIX4K_C8000;
          int index=((startaddress+size)-0xc8000) >> 12;
          type=(types >> (index*8)) & 0xf;
          sizeinc=4*1024;
          break;
        }

        case 0xd0000 ... 0xd7fff:
        {
          types=FIX4K_D0000;
          int index=((startaddress+size)-0xd0000) >> 12;
          type=(types >> (index*8)) & 0xf;
          sizeinc=4*1024;
          break;
        }

        case 0xd8000 ... 0xdffff:
        {
          types=FIX4K_D8000;
          int index=((startaddress+size)-0xd8000) >> 12;
          type=(types >> (index*8)) & 0xf;
          sizeinc=4*1024;
          break;
        }

        case 0xe0000 ... 0xe7fff:
        {
          types=FIX4K_E0000;
          int index=((startaddress+size)-0xe0000) >> 12;
          type=(types >> (index*8)) & 0xf;
          sizeinc=4*1024;
          break;
        }

        case 0xe8000 ... 0xeffff:
        {
          types=FIX4K_E8000;
          int index=((startaddress+size)-0xe8000) >> 12;
          type=(types >> (index*8)) & 0xf;
          sizeinc=4*1024;
          break;
        }

        case 0xf0000 ... 0xf7fff:
        {
          types=FIX4K_F0000;
          int index=((startaddress+size)-0xf0000) >> 12;
          type=(types >> (index*8)) & 0xf;
          sizeinc=4*1024;
          break;
        }

        case 0xf8000 ... 0xfffff:
        {
          types=FIX4K_F8000;
          int index=((startaddress+size)-0xf8000) >> 12;
          type=(types >> (index*8)) & 0xf;
          sizeinc=4*1024;
          break;
        }
      }

      sendstringf("Checking fixed mtrr %6 - %6 : %d      (%6)\n", startaddress+size, startaddress+size+sizeinc-1, memtype, types);

      if (memtype==-1)
        memtype=type;

      if (type==memtype) //same type, continue
        size+=sizeinc;
      else //type changed
      {
        sendstringf("  -Adding %6 - %6 as type %d\n", startaddress, startaddress+size-1,memtype);
        addToMemoryRanges(startaddress, size, memtype);

        //start a new region
        startaddress=startaddress+size;
        size=sizeinc;
        memtype=type;
      }
    }

    if (size) //last region needs to be added as well
    {
      sendstringf("  -Adding %6 - %6 as type %d\n", startaddress, startaddress+size-1,memtype);
      addToMemoryRanges(startaddress, size, memtype);
    }
  }

  //check the var fields

  sendstringf("Checking var mtrrs\n");
  for (i=0; i<MTRRCapabilities.VCNT; i++)
  {
    QWORD base=readMSR(IA32_MTRR_PHYSBASE0+i*2);
    QWORD mask=readMSR(IA32_MTRR_PHYSMASK0+i*2);

    sendstringf("Base=%6 Mask=%6\n", base, mask);

    int memtype=base & 0xff;

    if (mask & (1<<11))// && (memtype!=MTRRDefType.TYPE)) //valid
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

          sendstringf("    var mttr %d: %6 - %6  %d\n", i, base,base+size-1, memtype);

          addToMemoryRanges(base, size, memtype);
          break;
        }
      }
    }
  }

  for (i=0; i<memoryrangesPos; i++)
  {
    QWORD address=memoryranges[i].startaddress;
    QWORD size=memoryranges[i].size;

    sendstringf("Memoryrange %d: %6 -> %6 : %d\n", i, address, address+size, memoryranges[i].memtype);
  }

  sanitizeMemoryRegions();

  sendstringf("\n\n\nAfter sanitization:\n");
  for (i=0; i<memoryrangesPos; i++)
  {
    QWORD address=memoryranges[i].startaddress;
    QWORD size=memoryranges[i].size;

    sendstringf("Memoryrange %d: %6 -> %6 : %d\n", i, address, address+size, memoryranges[i].memtype);
  }

  csLeave(&memoryrangesCS);
}



int remapMTRRTypes(QWORD address UNUSED, QWORD size UNUSED, int type UNUSED)
{
  //called by the MSR write handler when MTRR registers get changed
  initMemTypeRanges();

  return 1;
}

int handleMSRWrite_MTRR(void)
//called when an MTRR msr is written. Figures out what regions have been modified
{
  initMemTypeRanges();
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
      nosendchar[getAPICID()]=0;
      sendstring("Assertion Fail: fullmap is false for a 1 page range");
      ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
      while (1) outportb(0x80,0xc3);
    }

   //memtype=0;

    //sendstringf("mapping %6 as a 4KB page with memtype %d\n", physicalAddress & MAXPHYADDRMASKPB, memtype);
    *(QWORD*)(&pagetable[pagetableindex])=physicalAddress & MAXPHYADDRMASKPB;
    pagetable[pagetableindex].RA=1;
    pagetable[pagetableindex].WA=1;
    pagetable[pagetableindex].XA=1;
    pagetable[pagetableindex].MEMTYPE=memtype;
  }
  else
  {
    //else already mapped
    //sendstringf("This physical address (%6) was already mapped\n", physicalAddress);

    //change it to full access
    pagetable[pagetableindex].RA=1;
    pagetable[pagetableindex].WA=1;
    pagetable[pagetableindex].XA=1;
  }

  unmapPhysicalMemory(pagetable,4096);

  csLeave(&currentcpuinfo->EPTPML4CS);

  return PA;
}

VMSTATUS handleEPTViolation(pcpuinfo currentcpuinfo, VMRegisters *vmregisters UNUSED, PFXSAVE64 fxsave UNUSED)
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
    vmwrite(vm_entry_interruptioninfo, newintinfo.interruption_information); //entry info field
    vmwrite(vm_entry_instructionlength, vmread(vm_exit_instructionlength)); //entry instruction length
  }
  else
  {
    //problem: on intel this will set RF to 1.  So the handleWatchEvent cannot distinguish a resuming dbvmbp
    //solution: use a "skip next watchevent" for this cpu
  }

 //vi.ExitQualification=vmread(vm_exit_qualification);

  QWORD GuestAddress=vmread(vm_guest_physical_address);
  QWORD GuestAddressVA=vmread(vm_guest_linear_address);


  if (ept_handleWatchEvent(currentcpuinfo, vmregisters, fxsave, GuestAddress))
    return 0;



  //check for cloak
  if (ept_handleCloakEvent(currentcpuinfo, GuestAddress, GuestAddressVA))
    return 0;

  //still here, so not a watch or cloak
  sendstringf("Mapping %6\n", GuestAddress);
  EPTMapPhysicalMemory(currentcpuinfo, GuestAddress, 0);

  return 0;

}

VMSTATUS handleEPTMisconfig(pcpuinfo currentcpuinfo UNUSED, VMRegisters *vmregisters UNUSED)
{
  nosendchar[getAPICID()]=0;
  sendstring("handleEPTMisconfig\n");
  //could have been a timing misconfig, try again

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
    vmwrite(vm_entry_interruptioninfo, newintinfo.interruption_information); //entry info field
    vmwrite(0x401a, vmread(vm_exit_instructionlength)); //entry instruction length
    return VM_OK;
  }

  QWORD GuestAddress=vmread(vm_guest_physical_address);
  QWORD EPTAddress=EPTMapPhysicalMemory(currentcpuinfo, GuestAddress, 0);

  if (EPTAddress)
  {
    sendstringf("handleEPTMisconfig(%x) : %6\n",GuestAddress, EPTAddress);
  }
  else
  {
    sendstringf("handleEPTMisconfig(%x) : fuck\n", GuestAddress);
  }
  while (1) outportb(0x80,0xe0);

  return VM_ERROR;
}

