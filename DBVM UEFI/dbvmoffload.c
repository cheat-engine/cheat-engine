/*
 * dbvmoffload.c
 *
 * port of the windows offload of CE's driver
 *
 *  Created on: Nov 21, 2017
 *      Author: eric
 */
#include "dbvmoffload.h"
#include "helpers.h"

typedef UINT64 UINT_PTR, *PUINT64, *PUINT_PTR;
typedef UINT32 DWORD, *PDWORD;
typedef UINT16 WORD, *PWORD;
typedef void *PVOID;



extern void enterVMM( void ); //declared in vmxoffloada.asm
extern void enterVMMPrologue(void);
extern void enterVMMEpilogue(void);


#pragma pack(2)
struct
{
  WORD limit;
  UINT_PTR base;
} NewGDTDescriptor;
#pragma pack()

#pragma pack(1)
typedef struct
{ //ok, everything uint64, I hate these incompatibilities with alignment between gcc and ms c
  UINT64    cpucount;
  UINT64    originalEFER;
  UINT64    originalLME;
  UINT64    idtbase;
  UINT64    idtlimit;
  UINT64    gdtbase;
  UINT64    gdtlimit;
  UINT64    cr0;
  UINT64    cr2;
  UINT64    cr3;
  UINT64    cr4;
  UINT64    dr7;
  UINT64    rip;

  UINT64    rax;
  UINT64    rbx;
  UINT64    rcx;
  UINT64    rdx;
  UINT64    rsi;
  UINT64    rdi;
  UINT64    rbp;
  UINT64    rsp;
  UINT64    r8;
  UINT64    r9;
  UINT64    r10;
  UINT64    r11;
  UINT64    r12;
  UINT64    r13;
  UINT64    r14;
  UINT64    r15;

  UINT64    rflags;
  UINT64    cs;
  UINT64    ss;
  UINT64    ds;
  UINT64    es;
  UINT64    fs;
  UINT64    gs;
  UINT64    tr;
  UINT64    ldt;

  UINT64    fsbase;
  UINT64    gsbase;

} OriginalState, *POriginalState;
#pragma pack()

POriginalState originalstate;
UINT_PTR pagedirptrbasePA;
UINT_PTR NewGDTDescriptorVA;

unsigned char *enterVMM2;


UINT_PTR enterVMM2PA;

PVOID TemporaryPagingSetup;
UINT_PTR TemporaryPagingSetupPA;
UINT_PTR originalstatePA;
UINT_PTR NewGDTDescriptorVA;
UINT_PTR InitStackPA;
UINT_PTR vmmPA;

int initializedvmm=0;


void InitializeDBVM(UINT64 vmm, int vmmsize)
{
  //basic paging setup for the vmm, will get expanded by the vmm itself
  UINT64 *GDTBase=(UINT64 *)((UINT_PTR)vmm+vmmsize+4096);
  UINT_PTR  pagedirptrbase=(UINT_PTR)vmm+vmmsize+2*4096;
  PUINT64   PageMapLevel4=(PUINT64)pagedirptrbase;
  PUINT64   PageDirPtr=(PUINT64)(pagedirptrbase+4096);
  PUINT64   PageDir=(PUINT64)(pagedirptrbase+4096+4096);

  Print(L"pagedirptrbase=%lx\n",pagedirptrbase);
  pagedirptrbasePA=(UINT_PTR)pagedirptrbase;

  Print(L"Setting up initial paging table for vmm\n");

  PageMapLevel4[0]=(UINT_PTR)PageDirPtr;
  ((PPDPTE_PAE)(&PageMapLevel4[0]))->P=1;
  ((PPDPTE_PAE)(&PageMapLevel4[0]))->RW=1;

  Print(L"PageMapLevel4[0]=%lx\n",PageMapLevel4[0]);


  PageDirPtr[0]=(UINT_PTR)PageDir;
  ((PPDPTE_PAE)(&PageDirPtr[0]))->P=1;
  ((PPDPTE_PAE)(&PageDirPtr[0]))->RW=1;
  Print(L"PageDirPtr[0]=%lx\n",PageDirPtr[0]);

  PageDir[0]=0;
  ((PPDE2MB_PAE)(&PageDir[0]))->P=1;
  ((PPDE2MB_PAE)(&PageDir[0]))->US=1;
  ((PPDE2MB_PAE)(&PageDir[0]))->RW=1;
  ((PPDE2MB_PAE)(&PageDir[0]))->PS=1; //2MB
  Print(L"PageDir[0]=%lx\n",PageDir[0]);

  PageDir[1]=0x00200000;
  ((PPDE2MB_PAE)(&PageDir[1]))->P=1;
  ((PPDE2MB_PAE)(&PageDir[1]))->US=1;
  ((PPDE2MB_PAE)(&PageDir[1]))->RW=1;
  ((PPDE2MB_PAE)(&PageDir[1]))->PS=1; //2MB
  Print(L"PageDir[1]=%lx\n",PageDir[1]);

  {

    //create a pagetable for the first 2MB of vmm
    PUINT64 PageTable=AllocatePersistentMemory(4096);
    int i;

    PageDir[2]=(UINT64)PageTable;
    ((PPDE2MB_PAE)(&PageDir[2]))->P=1;
    ((PPDE2MB_PAE)(&PageDir[2]))->RW=1;
    ((PPDE2MB_PAE)(&PageDir[2]))->PS=0; //points to a pagetable

    Print(L"PageDir[2]=%lx\n",PageDir[2]);

    //fill in the pagetable
    for (i=0; i<512; i++)
    {
      PageTable[i]=(UINT_PTR)vmm+(4096*i);
      ((PPTE_PAE)(&PageTable[i]))->P=1;
      ((PPTE_PAE)(&PageTable[i]))->RW=1;
    }
  }

  {
     //create a pagetable for the 2nd 2MB of vmm
     PUINT64 PageTable=AllocatePersistentMemory(4096);
     int i;

     PageDir[3]=(UINT64)PageTable;
     ((PPDE2MB_PAE)(&PageDir[3]))->P=1;
     ((PPDE2MB_PAE)(&PageDir[3]))->RW=1;
     ((PPDE2MB_PAE)(&PageDir[3]))->PS=0;

     Print(L"PageDir[3]=%lx\n",PageDir[3]);

     //fill in the pagetable
     for (i=0; i<512; i++)
     {
       PageTable[i]=(UINT_PTR)vmm+0x00200000+(4096*i);
       ((PPTE_PAE)(&PageTable[i]))->P=1;
       ((PPTE_PAE)(&PageTable[i]))->RW=1;
     }
   }


  //setup GDT
  GDTBase[0 ]=0;            //0 :
  GDTBase[1 ]=0x00cf92000000ffffULL;  //8 : 32-bit data
  GDTBase[2 ]=0x00cf96000000ffffULL;  //16: test, stack, failed, unused
  GDTBase[3 ]=0x00cf9b000000ffffULL;  //24: 32-bit code
  GDTBase[4 ]=0x00009a000000ffffULL;  //32: 16-bit code
  GDTBase[5 ]=0x000092000000ffffULL;  //40: 16-bit data
  GDTBase[6 ]=0x00009a030000ffffULL;  //48: 16-bit code, starting at 0x30000
  GDTBase[7 ]=0;            //56: 32-bit task
  GDTBase[8 ]=0;            //64: 64-bit task
  GDTBase[9 ]=0;            //72:  ^   ^   ^
  GDTBase[10]=0x00a09e0000000000ULL;  //80: 64-bit code
  GDTBase[11]=0;            //88:  ^   ^   ^
  GDTBase[12]=0;            //96: 64-bit tss descriptor (2)
  GDTBase[13]=0;            //104: ^   ^   ^


  NewGDTDescriptor.limit=0x6f; //111
  NewGDTDescriptor.base=0x00400000+vmmsize+4096; //virtual address to the gdt

  Print(L"&NewGDTDescriptor=%lx, &NewGDTDescriptor.limit=%lx, &NewGDTDescriptor.base=%lx\n",(UINT64)&NewGDTDescriptor,(UINT64)&NewGDTDescriptor.limit, (UINT64)&NewGDTDescriptor.base);
  Print(L"NewGDTDescriptor.limit=%x\n",NewGDTDescriptor.limit);
  Print(L"NewGDTDescriptor.base=%lx\n",(UINT64)NewGDTDescriptor.base);

  NewGDTDescriptorVA=(UINT_PTR)&NewGDTDescriptor;

  /*
   * AllocatePages
   */
  {
    EFI_PHYSICAL_ADDRESS address=0x003fffff;
    EFI_STATUS s;

    address=0x003fffffULL; //allocate 4k at the lower 4MB
    Print(L"Before enterVMM2 alloc: maxPA=%lx, x\n", address);

    s=AllocatePages(AllocateMaxAddress,EfiRuntimeServicesCode, 1,&address);
    if (s!=EFI_SUCCESS)
    {
      Print(L"Failure allocating low memory region");
      return;
    }

    ZeroMem(address,4096);

    enterVMM2=(unsigned char *)address;

    //unsigned char *original=(unsigned char *)enterVMM;
    ZeroMem(enterVMM2,4096);
    Print(L"enterVMM is located at %lx\n", (UINT64)enterVMM);
    Print(L"enterVMM2 is located at %lx\n", (UINT64)enterVMM2);

    Print(L"Copying function till end\n");
    {
      int i;
      unsigned char *original=(unsigned char *)enterVMM;
      i=0;
      while ((i<4096) && ((original[i]!=0xce) || (original[i+1]!=0xce) || (original[i+2]!=0xce) || (original[i+3]!=0xce) || (original[i+4]!=0xce)))
        i++;

      Print(L"size is %d\n",i);

      CopyMem(enterVMM2, original, i);

      Print(L"copy done\n");

    }

  }

  //skip the setting up of identity mapped pages, because we're at that atm

  enterVMM2PA=(UINT_PTR)enterVMM2;
  originalstate=AllocatePersistentMemory(sizeof(OriginalState));
  ZeroMem(originalstate, sizeof(OriginalState));
  originalstatePA=(UINT_PTR)originalstate;
  Print(L"enterVMM2PA=%lx\n",enterVMM2PA);
  Print(L"originalstatePA=%lx\n",originalstatePA);

  //setup init vars
  {
    EFI_PHYSICAL_ADDRESS address=0x003fffff;
    EFI_STATUS s;
    s=AllocatePages(AllocateMaxAddress,EfiRuntimeServicesCode, 16*8,&address);
    if (s!=EFI_SUCCESS)
    {
      Print(L"Failure allocating low memory region for stack");
      return;
    }

    ZeroMem((void *)address,16*4096*8);

    InitStackPA=address+(16*4096*8)-16;

    unsigned char *vmimg=(unsigned char *)vmm;
    *(UINT64 *)(&vmimg[0x10])=originalstatePA;
    *(UINT64 *)(&vmimg[0x18])=vmmPA;
    *(UINT64 *)(&vmimg[0x20])=0x00400000+vmmsize+2*4096;
    *(UINT64 *)(&vmimg[0x28])=InitStackPA;
  }
  initializedvmm=TRUE;


}


/*            //now create a paging setup where enterVMM2 is identity mapped AND mapped at the current virtual address, needed to be able to go down to nonpaged mode
            //easiest way, make every page point to enterVMM2

            //allocate 4 pages
            DbgPrint("Allocating memory for the temp pagedir\n");
            minPA.QuadPart=0;
            maxPA.QuadPart=0xffffffffffff0000ULL;

            TemporaryPagingSetup=ExAllocatePool(NonPagedPool, 4*4096);

            if (TemporaryPagingSetup==NULL)
            {
              DbgPrint("TemporaryPagingSetup==NULL!!!\n");
              return;
            }

            RtlZeroMemory(TemporaryPagingSetup,4096*4);
            DbgPrint("TemporaryPagingSetup is located at %p (%I64x)\n", TemporaryPagingSetup, MmGetPhysicalAddress(TemporaryPagingSetup).QuadPart);


            TemporaryPagingSetupPA=(UINT_PTR)MmGetPhysicalAddress(TemporaryPagingSetup).QuadPart;

#ifdef AMD64
            DbgPrint("Setting up temporary paging setup for x64\n");
            {
              PUINT64 PML4Table=(PUINT64)TemporaryPagingSetup;
              PUINT64 PageDirPtr=(PUINT64)((UINT_PTR)TemporaryPagingSetup+4096);
              PUINT64 PageDir=(PUINT64)((UINT_PTR)TemporaryPagingSetup+2*4096);
              PUINT64 PageTable=(PUINT64)((UINT_PTR)TemporaryPagingSetup+3*4096);

              DbgPrint("PAE paging\n");
              for (i=0; i<512; i++)
              {
                PML4Table[i]=MmGetPhysicalAddress(PageDirPtr).QuadPart;
                ((PPDPTE_PAE)(&PML4Table[i]))->P=1;

                PageDirPtr[i]=MmGetPhysicalAddress(PageDir).QuadPart;
                ((PPDPTE_PAE)(&PageDirPtr[i]))->P=1;

                PageDir[i]=MmGetPhysicalAddress(PageTable).QuadPart;
                ((PPDE_PAE)(&PageDir[i]))->P=1;
                ((PPDE_PAE)(&PageDir[i]))->PS=0; //4KB

                PageTable[i]=MmGetPhysicalAddress(enterVMM2).QuadPart;
                ((PPTE_PAE)(&PageTable[i]))->P=1;
              }
            }

#else
            DbgPrint("Setting up temporary paging setup\n");
            if (PTESize==8) //PAE paging
            {
              PUINT64 PageDirPtr=(PUINT64)TemporaryPagingSetup;
              PUINT64 PageDir=(PUINT64)((UINT_PTR)TemporaryPagingSetup+4096);
              PUINT64 PageTable=(PUINT64)((UINT_PTR)TemporaryPagingSetup+2*4096);

              DbgPrint("PAE paging\n");
              for (i=0; i<512; i++)
              {
                PageDirPtr[i]=MmGetPhysicalAddress(PageDir).QuadPart;
                ((PPDPTE_PAE)(&PageDirPtr[i]))->P=1;
                //((PPDPTE_PAE)(&PageDirPtr[i]))->RW=1;


                PageDir[i]=MmGetPhysicalAddress(PageTable).QuadPart;
                ((PPDE_PAE)(&PageDir[i]))->P=1;
                //((PPDE_PAE)(&PageDir[i]))->RW=1;
                ((PPDE_PAE)(&PageDir[i]))->PS=0; //4KB

                PageTable[i]=MmGetPhysicalAddress(enterVMM2).QuadPart;
                ((PPTE_PAE)(&PageTable[i]))->P=1;
                //((PPTE_PAE)(&PageTable[i]))->RW=1;

              }

            }
            else
            {
              //normal(old) 4 byte page entries
              PDWORD PageDir=(PDWORD)TemporaryPagingSetup;
              PDWORD PageTable=(PDWORD)((DWORD)TemporaryPagingSetup+4096);
              DbgPrint("Normal paging\n");
              for (i=0; i<1024; i++)
              {
                PageDir[i]=MmGetPhysicalAddress(PageTable).LowPart;
                ((PPDE)(&PageDir[i]))->P=1;
                ((PPDE)(&PageDir[i]))->RW=1;
                ((PPDE)(&PageDir[i]))->PS=0; //4KB

                PageTable[i]=MmGetPhysicalAddress(enterVMM2).LowPart;
                ((PPTE)(&PageTable[i]))->P=1;
                ((PPTE)(&PageTable[i]))->RW=1;
              }

            }
#endif

            DbgPrint("Temp paging has been setup\n");



            enterVMM2PA=(UINT_PTR)MmGetPhysicalAddress(enterVMM2).QuadPart;

            minPA.QuadPart=0;
            maxPA.QuadPart=0xfffffffffffff000;
            boundary.QuadPart=0;
            if (sizeof(OriginalState)<4096)
              originalstate=MmAllocateContiguousMemory(4096, maxPA);
            else
              originalstate=MmAllocateContiguousMemory(sizeof(OriginalState), maxPA);

            RtlZeroMemory(originalstate, sizeof(OriginalState));

            originalstatePA=(UINT_PTR)MmGetPhysicalAddress(originalstate).QuadPart;
            DbgPrint("enterVMM2PA=%llx\n",enterVMM2PA);
            DbgPrint("originalstatePA=%llx\n",originalstatePA);
            DbgPrint("originalstatePA=%llx\n",originalstatePA);


            //setup init vars
            *(UINT64 *)(&vmm[0x10])=originalstatePA;
            *(UINT64 *)(&vmm[0x18])=vmmPA;
            *(UINT64 *)(&vmm[0x20])=0x00400000+vmmsize+2*4096;

            initializedvmm=TRUE;

 */

void LaunchDBVM()
{
   GDT gdt;
   IDT idt;
   EFLAGS eflags;
   CHAR16 something[200];

   if (!initializedvmm)
   {
     Print(L"First initialize DBVM\n");
     return;
   }

   Print(L"Storing original state\n");
   originalstate->cpucount=64;  //todo, use acpi tables to fill this in (or better yet, let it allocate the cpu structures at runtime)
   Print(L"originalstate->cpucount=%d",originalstate->cpucount);

   originalstate->originalEFER=readMSR(0xc0000080); //amd prefers this over an LME

   originalstate->originalLME=(int)(((DWORD)(readMSR(0xc0000080)) >> 8) & 1);
   Print(L"originalstate->originalLME=%d\n",originalstate->originalLME);

   originalstate->cr0=(UINT_PTR)getCR0();
   Print(L"originalstate->cr0=%lx\n",originalstate->cr0);

   originalstate->cr2=(UINT_PTR)getCR2();
   Print(L"originalstate->cr2=%lx\n",originalstate->cr2);

   originalstate->cr3=(UINT_PTR)getCR3();
   Print(L"originalstate->cr3=%lx\n",originalstate->cr3);

   originalstate->cr4=(UINT_PTR)getCR4();
   Print(L"originalstate->cr4=%lx\n",originalstate->cr4);

   originalstate->ss=getSS();
   Print(L"originalstate->ss=%lx\n",originalstate->ss);
   originalstate->cs=getCS();
   Print(L"originalstate->cs=%lx\n",originalstate->cs);
   originalstate->ds=getDS();
   Print(L"originalstate->ds=%lx\n",originalstate->ds);
   originalstate->es=getES();
   Print(L"originalstate->es=%lx\n",originalstate->es);
   originalstate->fs=getFS();
   Print(L"originalstate->fs=%lx\n",originalstate->fs);
   originalstate->gs=getGS();
   Print(L"originalstate->gs=%lx\n",originalstate->gs);
   originalstate->ldt=getLDT();
   Print(L"originalstate->ldt=%lx\n",originalstate->ldt);
   originalstate->tr=getTR();
   Print(L"originalstate->tr=%lx\n",originalstate->tr);

   originalstate->fsbase=readMSR(0xc0000100);
   originalstate->gsbase=readMSR(0xc0000101);


   Print(L"originalstate->fsbase=%lx originalstate->gsbase=%lx\n", originalstate->fsbase, originalstate->gsbase);

   originalstate->dr7=getDR7();

   gdt.vector=0;
   gdt.wLimit=0;
   getGDT(&gdt);

   originalstate->gdtbase=(UINT64)gdt.vector;
   originalstate->gdtlimit=gdt.wLimit;

   Print(L"originalstate->gdtbase=%lx\n",originalstate->gdtbase);
   Print(L"originalstate->gdtlimit=%lx\n",originalstate->gdtlimit);

   getIDT(&idt);
   originalstate->idtbase=(UINT64)idt.vector;
   originalstate->idtlimit=idt.wLimit;

   Print(L"originalstate->idtbase=%lx\n",originalstate->idtbase);
   Print(L"originalstate->idtlimit=%lx\n",originalstate->idtlimit);

   eflags=getEflags();
   originalstate->rflags=*(PUINT_PTR)&eflags;
   Print(L"originalstate->rflags was %lx\n",(UINT64)originalstate->rflags);

   eflags.IF=0;
   originalstate->rflags=*(PUINT_PTR)&eflags;

   Print(L"originalstate->rflags is %lx\n",(UINT64)originalstate->rflags);

   originalstate->rflags=*(PUINT_PTR)&eflags;

   originalstate->rsp=getRSP();
   Print(L"originalstate->rsp=%lx\n",originalstate->rsp);
   originalstate->rbp=getRBP();
   Print(L"originalstate->rbp=%lx\n",originalstate->rbp);


   originalstate->rax=getRAX();
   Print(L"originalstate->rax=%lx\n",originalstate->rax);
   originalstate->rbx=getRBX();
   Print(L"originalstate->rbx=%lx\n",originalstate->rbx);
   originalstate->rcx=getRCX();
   Print(L"originalstate->rcx=%lx\n",originalstate->rcx);
   originalstate->rdx=getRDX();
   Print(L"originalstate->rdx=%lx\n",originalstate->rdx);
   originalstate->rsi=getRSI();
   Print(L"originalstate->rsi=%lx\n",originalstate->rsi);
   originalstate->rdi=getRDI();
   Print(L"originalstate->rdi=%lx\n",originalstate->rdi);
   originalstate->r8=getR8();
   Print(L"originalstate->r8=%lx\n",originalstate->r8);
   originalstate->r9=getR9();
   Print(L"originalstate->r9=%lx\n",originalstate->r9);
   originalstate->r10=getR10();
   Print(L"originalstate->r10=%lx\n",originalstate->r10);
   originalstate->r11=getR11();
   Print(L"originalstate->r11=%lx\n",originalstate->r11);
   originalstate->r12=getR12();
   Print(L"originalstate->r12=%lx\n",originalstate->r12);
   originalstate->r13=getR13();
   Print(L"originalstate->r13=%lx\n",originalstate->r13);
   originalstate->r14=getR14();
   Print(L"originalstate->r14=%lx\n",originalstate->r14);
   originalstate->r15=getR15();
   Print(L"originalstate->r15=%lx\n",originalstate->r15);

    originalstate->rsp-=8; //adjust rsp for the "call entervmmprologue"
    originalstate->rip=(UINT_PTR)enterVMMEpilogue; //enterVMMEpilogue is an address inside the entervmmprologue function

    Print(L"originalstate->rip=%lx\n",originalstate->rip);





    Print(L"Calling entervmm2. (Originalstate=%lx (%lx))\n",originalstate,originalstatePA);

    //call to entervmmprologue, pushes the return value on the stack


    Input(L"Type something : ", something, 200);

    enterVMMPrologue();

    enableInterrupts();


    Print(L"Returned from enterVMMPrologue\n");

    //DbgPrint("cpunr=%d\n",cpunr());


    Print(L"Returning\n");

}
