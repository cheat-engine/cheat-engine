/*
sets up all the needed data structures 
copies dbvm into physical memory
jumps into dbvm's os entry point

*/
#pragma warning( disable: 4103)
#include "ntifs.h"
#include <windef.h>

#include "dbkfunc.h"
#include "vmxoffload.h"

unsigned char *vmm;

#pragma pack(2) 
struct
{
	WORD limit;
	UINT_PTR base;
} NewGDTDescriptor;
#pragma pack()

typedef struct
{ //ok, everything uint64, I hate these incompatibilities with alignment between gcc and ms c
	UINT64		cpucount;
	UINT64		originalLME;
	UINT64		idtbase;
	UINT64		idtlimit;
	UINT64		gdtbase;
	UINT64		gdtlimit;
	UINT64		cr0;
	UINT64		cr2;
	UINT64		cr3;
	UINT64		cr4;
	UINT64		dr7;
	UINT64		rip;

	UINT64		rax;
	UINT64		rbx;
	UINT64		rcx;
	UINT64		rdx;
	UINT64		rsi;
	UINT64		rdi;
	UINT64		rbp;
	UINT64		rsp;
	UINT64		r8;
	UINT64		r9;
	UINT64		r10;
	UINT64		r11;
	UINT64		r12;
	UINT64		r13;
	UINT64		r14;
	UINT64		r15;

	UINT64		rflags;
	UINT64		cs;
	UINT64		ss;
	UINT64		ds;
	UINT64		es;
	UINT64		fs;
	UINT64		gs;
	UINT64		tr;
	UINT64		ldt;
	
	UINT64		fsbase;
	UINT64		gsbase;

} OriginalState, *POriginalState;

unsigned char *enterVMM2;
POriginalState originalstate;

UINT_PTR enterVMM2PA;

PVOID TemporaryPagingSetup;
UINT_PTR TemporaryPagingSetupPA;
UINT_PTR pagedirptrbasePA;
UINT_PTR originalstatePA;
UINT_PTR NewGDTDescriptorVA;
UINT_PTR vmmPA;

int initializedvmm=0;

#ifdef AMD64
extern void enterVMM( void ); //declared in vmxoffloada.asm
extern void enterVMMPrologue(void);
extern void enterVMMEpilogue(void);
#else
_declspec( naked ) void enterVMM( void )
{

	__asm
	{
begin:
		xchg bx,bx //trigger bochs breakpoint

		//setup the GDT
		lgdt [ebx] //ebx is the 'virtual address' so just do that before disabling paging ok...

		//switch to identify mapped pagetable
		mov cr3,edx
		jmp short weee
weee:
		


		//now jump to the physical address (identity mapped to the same virtual address)
		mov eax,secondentry
	    sub eax,begin
		add eax,esi
		jmp eax

secondentry:

		

		//disable paging		
		mov eax,cr0
		and eax,0x7FFFFFFF
		mov cr0,eax
		//paging off
		jmp short weee2
weee2:

		

		//load paging for vmm (but don't apply yet, in nonpaged mode)
		mov cr3,ecx

		//enable PAE and PSE
		mov eax,0x30
		__emit 0x0f  //-|
		__emit 0x22  //-|-mov cr4,eax  (still WTF's me that visual studio doesn't know about cr4)
		__emit 0xe0  //-|


		mov ecx,0xc0000080 //enable efer_lme
		rdmsr
		or eax,0x100
		wrmsr

		//mov eax,cr0		
		//or eax,0x80000020 //re-enable pg (and ne to be sure)
		//edit, who cares, fuck the original state, it's my own state now
		mov eax,0x80000021
		mov cr0,eax

		mov eax,edi //tell dbvm it's an OS entry and a that location the start info is
		mov ebx,ebp //tell vmmPA

		__emit 0xea  //-|
		__emit 0x00  //-|
		__emit 0x00  //-|
		__emit 0x40  //-|JMP FAR 0x50:0x00400000
		__emit 0x00  //-|
		__emit 0x50  //-|
		__emit 0x00  //-|

		__emit 0xce
		__emit 0xce
		__emit 0xce
		__emit 0xce
		__emit 0xce
		__emit 0xce
		__emit 0xce

	}
}
#endif

void vmxoffload(PCWSTR dbvmimgpath)
{
	//save entry state for easy exit in ReturnFromvmxoffload
	EFLAGS eflags;

	int i;
	PHYSICAL_ADDRESS minPA, maxPA,boundary;
	GDT gdt;
	IDT idt;
	
	
	//allocate 4MB of contigues physical memory
	minPA.QuadPart=0;
	maxPA.QuadPart=0xffffffffff000000ULL;
	boundary.QuadPart=0x00400000ULL; //4 mb boundaries


	DbgPrint("vmxoffload\n");

	if (!initializedvmm)
	{			
		DbgPrint("First time run. Initializing vmm section");
	
		vmm=ExAllocatePool(NonPagedPool, 4*1024*1024);
		
		if (vmm)
		{	
			HANDLE dbvmimghandle;
			UNICODE_STRING filename;
			IO_STATUS_BLOCK statusblock;
			OBJECT_ATTRIBUTES oa;
			NTSTATUS OpenedFile;

			vmmPA=(UINT_PTR)MmGetPhysicalAddress(vmm).QuadPart;
			DbgPrint("Allocated memory at virtual address %p (physical address %llx)\n",vmm,MmGetPhysicalAddress(vmm));
			RtlZeroMemory(vmm,4*1024*1024); //initialize

			RtlInitUnicodeString(&filename, dbvmimgpath);
		
			
			//Load the .img file
			InitializeObjectAttributes(&oa, &filename, 0, NULL, NULL);
			OpenedFile=ZwCreateFile(&dbvmimghandle,SYNCHRONIZE|STANDARD_RIGHTS_READ , &oa, &statusblock, NULL, FILE_SYNCHRONOUS_IO_NONALERT| FILE_ATTRIBUTE_NORMAL, 0, FILE_OPEN, 0, NULL, 0);
			if (OpenedFile==STATUS_SUCCESS)
			{
				WORD startsector;
				LARGE_INTEGER byteoffset;
				FILE_STANDARD_INFORMATION fsi;
				NTSTATUS ReadFile;
				
				//Getting filesize
				ZwQueryInformationFile(dbvmimghandle, &statusblock, &fsi, sizeof(fsi),  FileStandardInformation);
				
				//fsi.EndOfFile contains the filesize

				if (fsi.EndOfFile.QuadPart>4*1024*1024)
				{
					DbgPrint("File bigger than 4MB. Big retard detected\n");
					return;
				}


				byteoffset.QuadPart=0x8; //offset containing sectornumber of the vmm location
				ReadFile=ZwReadFile(dbvmimghandle, NULL, NULL, NULL, &statusblock, &startsector, 2, &byteoffset, NULL);

				if (ReadFile==STATUS_PENDING)
				{							
					if (ZwWaitForSingleObject(dbvmimghandle, FALSE, NULL)!=STATUS_SUCCESS)
					{
					  DbgPrint("Read failure\n");
					  return;
					}					
				}

				if (statusblock.Status==STATUS_SUCCESS)
				{
					DWORD vmmsize=fsi.EndOfFile.LowPart-(startsector*512);

					//now read the vmdisk into the allocated memory
					DbgPrint("The startsector=%d (that's offset %d)\n",startsector,startsector*512);

					byteoffset.QuadPart=startsector*512; 
					ReadFile=ZwReadFile(dbvmimghandle, NULL, NULL, NULL, &statusblock, vmm, vmmsize, &byteoffset, NULL);
					if (ReadFile==STATUS_PENDING)
						ZwWaitForSingleObject(dbvmimghandle, FALSE, NULL);

					vmmsize=(vmmsize+4096) & 0xfffffffffffff000ULL; //adjust the size internally to a page boundary (sure, there's some mem loss, but it's predicted, dbvm assumes first 10 pages are scratch pages)

					DbgPrint("vmmsize=%x\n",vmmsize);

					if (statusblock.Status==STATUS_SUCCESS )
					{
						//basic paging setup for the vmm, will get expanded by the vmm itself
					
						UINT64		*GDTBase=(UINT64 *)((UINT_PTR)vmm+vmmsize+4096);
						UINT_PTR	pagedirptrbase=(UINT_PTR)vmm+vmmsize+2*4096;					
						PUINT64		PageMapLevel4=(PUINT64)pagedirptrbase;
						PUINT64		PageDirPtr=(PUINT64)(pagedirptrbase+4096);
						PUINT64		PageDir=(PUINT64)(pagedirptrbase+4096+4096);

						DbgPrint("pagedirptrbase=%llx (physical address %llx)\n",pagedirptrbase,MmGetPhysicalAddress((PVOID)pagedirptrbase));

						pagedirptrbasePA=(UINT_PTR)MmGetPhysicalAddress((PVOID)pagedirptrbase).QuadPart;
						
						//blame MS for making this hard to read
						DbgPrint("Setting up initial paging table for vmm\n");

						PageMapLevel4[0]=MmGetPhysicalAddress(PageDirPtr).QuadPart;					
						((PPDPTE_PAE)(&PageMapLevel4[0]))->P=1;
						((PPDPTE_PAE)(&PageMapLevel4[0]))->RW=1;

						DbgPrint("PageMapLevel4[0]=%llx\n",PageMapLevel4[0]);
						

						PageDirPtr[0]=MmGetPhysicalAddress(PageDir).QuadPart;
						((PPDPTE_PAE)(&PageDirPtr[0]))->P=1;
						((PPDPTE_PAE)(&PageDirPtr[0]))->RW=1;
						DbgPrint("PageDirPtr[0]=%llx\n",PageDirPtr[0]);
						

						PageDir[0]=0;
						((PPDE2MB_PAE)(&PageDir[0]))->P=1;
						((PPDE2MB_PAE)(&PageDir[0]))->US=1;
						((PPDE2MB_PAE)(&PageDir[0]))->RW=0;
						((PPDE2MB_PAE)(&PageDir[0]))->PS=1; //2MB*/
						DbgPrint("PageDir[0]=%llx\n",PageDir[0]);



						PageDir[1]=0x00200000;						
						((PPDE2MB_PAE)(&PageDir[1]))->P=1;
						((PPDE2MB_PAE)(&PageDir[1]))->US=1;
						((PPDE2MB_PAE)(&PageDir[1]))->RW=0;
						((PPDE2MB_PAE)(&PageDir[1]))->PS=1; //2MB
						DbgPrint("PageDir[1]=%llx\n",PageDir[1]);
						

						{
							//create a pagetable for the first 2MB of vmm
							PUINT64 PageTable=ExAllocatePool(NonPagedPool, 4096);
							int i;

							PageDir[2]=MmGetPhysicalAddress(PageTable).QuadPart;
							((PPDE2MB_PAE)(&PageDir[2]))->P=1;
							((PPDE2MB_PAE)(&PageDir[2]))->RW=1;
							((PPDE2MB_PAE)(&PageDir[2]))->PS=0; //points to a pagetable 

							DbgPrint("PageDir[2]=%llx\n",PageDir[2]);

							//fill in the pagetable
							for (i=0; i<512; i++)
							{
								PageTable[i]=MmGetPhysicalAddress((PVOID)(((UINT_PTR)vmm)+(4096*i))).QuadPart;
								((PPTE_PAE)(&PageTable[i]))->P=1;
								((PPTE_PAE)(&PageTable[i]))->RW=1;								
							}
						}

						{
							//create a pagetable for the 2nd 2MB of vmm
							PUINT64 PageTable=ExAllocatePool(NonPagedPool, 4096);
							int i;

							PageDir[3]=MmGetPhysicalAddress(PageTable).QuadPart;
							((PPDE2MB_PAE)(&PageDir[3]))->P=1;
							((PPDE2MB_PAE)(&PageDir[3]))->RW=1;
							((PPDE2MB_PAE)(&PageDir[3]))->PS=0; 

							DbgPrint("PageDir[3]=%llx\n",PageDir[3]);

							//fill in the pagetable
							for (i=0; i<512; i++)
							{
								PageTable[i]=MmGetPhysicalAddress((PVOID)(((UINT_PTR)vmm)+0x00200000+(4096*i))).QuadPart;
								((PPTE_PAE)(&PageTable[i]))->P=1;
								((PPTE_PAE)(&PageTable[i]))->RW=1;								
							}
						}

						//setup GDT
						GDTBase[0 ]=0;						//0 :
						GDTBase[1 ]=0x00cf92000000ffffULL;	//8 : 32-bit data
						GDTBase[2 ]=0x00cf96000000ffffULL;	//16: test, stack, failed, unused
						GDTBase[3 ]=0x00cf9b000000ffffULL;	//24: 32-bit code
						GDTBase[4 ]=0x00009a000000ffffULL;	//32: 16-bit code
						GDTBase[5 ]=0x000092000000ffffULL;	//40: 16-bit data
						GDTBase[6 ]=0x00009a030000ffffULL;	//48: 16-bit code, starting at 0x30000
						GDTBase[7 ]=0;						//56: 32-bit task	
						GDTBase[8 ]=0;						//64: 64-bit task
						GDTBase[9 ]=0;						//72:  ^   ^   ^
						GDTBase[10]=0x00a09e0000000000ULL;	//80: 64-bit code
						GDTBase[11]=0;						//88:  ^   ^   ^
						GDTBase[12]=0;						//96: 64-bit tss descriptor (2)
						GDTBase[13]=0;						//104: ^   ^   ^


						NewGDTDescriptor.limit=0x6f; //111
						NewGDTDescriptor.base=0x00400000+vmmsize+4096; //virtual address to the gdt

						DbgPrint("&NewGDTDescriptor=%p, &NewGDTDescriptor.limit=%p, &NewGDTDescriptor.base=%p\n",&NewGDTDescriptor,&NewGDTDescriptor.limit, &NewGDTDescriptor.base); 
						DbgPrint("NewGDTDescriptor.limit=%x\n",NewGDTDescriptor.limit);
						DbgPrint("NewGDTDescriptor.base=%p\n",NewGDTDescriptor.base);

						NewGDTDescriptorVA=(UINT_PTR)&NewGDTDescriptor;

						
						
						
						maxPA.QuadPart=0x003fffffULL; //allocate 4k at the lower 4MB
						DbgPrint("Before enterVMM2 alloc: maxPA=%llx, bam=%llx\n", maxPA.QuadPart);


						enterVMM2=MmAllocateContiguousMemory(4096,maxPA);
						if (enterVMM2)
						{
							unsigned char *original=(unsigned char *)enterVMM;
							RtlZeroMemory(enterVMM2,4096);
							DbgPrint("enterVMM is located at %p (%llx)\n", enterVMM, MmGetPhysicalAddress(enterVMM).QuadPart);
							DbgPrint("enterVMM2 is located at %p (%llx)\n", enterVMM2, MmGetPhysicalAddress(enterVMM2).QuadPart);


							DbgPrint("Copying function till end\n");
							//copy memory
							
							i=0;
							while ((i<4096) && ((original[i]!=0xce) || (original[i+1]!=0xce) || (original[i+2]!=0xce) || (original[i+3]!=0xce) || (original[i+4]!=0xce)))
								i++;

							DbgPrint("size is %d",i);

							RtlCopyMemory(enterVMM2,original, i);
							DbgPrint("Copy done\n");
						}
						else
						{
							DbgPrint("Failure allocating enterVMM2\n");
							return;
						}

						


						//now create a paging setup where enterVMM2 is identity mapped AND mapped at the current virtual address, needed to be able to go down to nonpaged mode
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
						DbgPrint("TemporaryPagingSetup is located at %p (%llx)\n", TemporaryPagingSetup, MmGetPhysicalAddress(TemporaryPagingSetup).QuadPart);


						TemporaryPagingSetupPA=(UINT_PTR)MmGetPhysicalAddress(TemporaryPagingSetup).QuadPart;
						
#ifdef AMD64			
						DbgPrint("Setting up temporary paging setup for x64\n");
						{
							PUINT64 PML4Table=(PUINT64)TemporaryPagingSetup;
							PUINT64	PageDirPtr=(PUINT64)((UINT_PTR)TemporaryPagingSetup+4096);						
							PUINT64	PageDir=(PUINT64)((UINT_PTR)TemporaryPagingSetup+2*4096);
							PUINT64	PageTable=(PUINT64)((UINT_PTR)TemporaryPagingSetup+3*4096);

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
							PUINT64	PageDirPtr=(PUINT64)TemporaryPagingSetup;						
							PUINT64	PageDir=(PUINT64)((UINT_PTR)TemporaryPagingSetup+4096);
							PUINT64	PageTable=(PUINT64)((UINT_PTR)TemporaryPagingSetup+2*4096);

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
						if (sizeof(OriginalState)>4096)
						{
							//too big for one page
							originalstate=MmAllocateContiguousMemory(sizeof(OriginalState), maxPA);
						}
						else
						{
							originalstate=ExAllocatePool(NonPagedPool,sizeof(OriginalState));
						}
						
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
					}
				}
				ZwClose(dbvmimghandle);
			}
			else
			{
				DbgPrint("Failure opening the file. Status=%x  (filename=%S)\n",OpenedFile, filename.Buffer);

			
			}
			//fill in some specific memory regions

		}
		else
		{
			DbgPrint("Failure allocating the required 4MB\n");
		}
	}

	if (initializedvmm)
	{
		KIRQL oldirql;
		
		DbgPrint("cpunr=%d\n",cpunr());
		KeRaiseIrql(HIGH_LEVEL,&oldirql);

		
			

		DbgPrint("Storing original state\n");
		originalstate->cpucount=getCpuCount();
		DbgPrint("originalstate->cpucount=%d",originalstate->cpucount);

		originalstate->originalLME=(int)(((DWORD)(readMSR(0xc0000080)) >> 8) & 1);
		DbgPrint("originalstate->originalLME=%d",originalstate->originalLME);

		originalstate->cr0=getCR0();
		DbgPrint("originalstate->cr0=%llx",originalstate->cr0);

		originalstate->cr2=getCR2();
		DbgPrint("originalstate->cr2=%llx",originalstate->cr2);

		originalstate->cr3=getCR3();
		DbgPrint("originalstate->cr3=%llx",originalstate->cr3);

		originalstate->cr4=getCR4();
		DbgPrint("originalstate->cr4=%llx",originalstate->cr4);

		originalstate->ss=getSS();
		DbgPrint("originalstate->ss=%llx",originalstate->ss);
		originalstate->cs=getCS();
		DbgPrint("originalstate->cs=%llx",originalstate->cs);
		originalstate->ds=getDS();
		DbgPrint("originalstate->ds=%llx",originalstate->ds);
		originalstate->es=getES();
		DbgPrint("originalstate->es=%llx",originalstate->es);
		originalstate->fs=getFS();
		DbgPrint("originalstate->fs=%llx",originalstate->fs);
		originalstate->gs=getGS();
		DbgPrint("originalstate->gs=%llx",originalstate->gs);
		originalstate->ldt=GetLDT();
		DbgPrint("originalstate->ldt=%llx",originalstate->ldt);
		originalstate->tr=GetTR();
		DbgPrint("originalstate->tr=%llx",originalstate->tr);

		originalstate->fsbase=readMSR(0xc0000100);
		originalstate->gsbase=readMSR(0xc0000101);

		DbgPrint("originalstate->fsbase=%llx originalstate->gsbase=%llx\n", originalstate->fsbase, originalstate->gsbase);


		originalstate->dr7=getDR7();

		
		gdt.vector=0;
		gdt.wLimit=0;
		GetGDT(&gdt);									
		originalstate->gdtbase=(ULONG_PTR)gdt.vector;
		originalstate->gdtlimit=gdt.wLimit;

		DbgPrint("originalstate->gdtbase=%llx",originalstate->gdtbase);
		DbgPrint("originalstate->gdtlimit=%llx",originalstate->gdtlimit);

		GetIDT(&idt);
		originalstate->idtbase=(ULONG_PTR)idt.vector;
		originalstate->idtlimit=idt.wLimit;

		DbgPrint("originalstate->idtbase=%llx",originalstate->idtbase);
		DbgPrint("originalstate->idtlimit=%llx",originalstate->idtlimit);
		
		
		eflags=getEflags();		
		eflags.IF=0;

		originalstate->rflags=*(PUINT_PTR)&eflags;



		DbgPrint("originalstate->rflags=%llx",originalstate->rflags);


		originalstate->rsp=getRSP();
		DbgPrint("originalstate->rsp=%llx",originalstate->rsp);
		originalstate->rbp=getRBP();
		DbgPrint("originalstate->rbp=%llx",originalstate->rbp);

		originalstate->rax=getRAX();
		DbgPrint("originalstate->rax=%llx",originalstate->rax);
		originalstate->rbx=getRBX();
		DbgPrint("originalstate->rbx=%llx",originalstate->rbx);
		originalstate->rcx=getRCX();
		DbgPrint("originalstate->rcx=%llx",originalstate->rcx);
		originalstate->rdx=getRDX();
		DbgPrint("originalstate->rdx=%llx",originalstate->rdx);
		originalstate->rsi=getRSI();
		DbgPrint("originalstate->rsi=%llx",originalstate->rsi);
		originalstate->rdi=getRDI();
		DbgPrint("originalstate->rdi=%llx",originalstate->rdi);
#ifdef AMD64
		originalstate->r8=getR8();
		DbgPrint("originalstate->r8=%llx",originalstate->r8);
		originalstate->r9=getR9();
		DbgPrint("originalstate->r9=%llx",originalstate->r9);
		originalstate->r10=getR10();
		DbgPrint("originalstate->r10=%llx",originalstate->r10);
		originalstate->r11=getR11();
		DbgPrint("originalstate->r11=%llx",originalstate->r11);
		originalstate->r12=getR12();
		DbgPrint("originalstate->r12=%llx",originalstate->r12);
		originalstate->r13=getR13();
		DbgPrint("originalstate->r13=%llx",originalstate->r13);
		originalstate->r14=getR14();
		DbgPrint("originalstate->r14=%llx",originalstate->r14);
		originalstate->r15=getR15();
		DbgPrint("originalstate->r15=%llx",originalstate->r15);
#endif
		

#ifdef AMD64
		
		originalstate->rsp-=8; //adjust rsp for the "call entervmmprologue"
 		originalstate->rip=(UINT_PTR)enterVMMEpilogue; //enterVMMEpilogue is an address inside the entervmmprologue function

		DbgPrint("originalstate->rip=%llx",originalstate->rip);

		DbgPrint("Calling entervmm2. (Originalstate=%p (%llx))\n",originalstate,originalstatePA);
		
		{//debug code
			LARGE_INTEGER wait;
			wait.QuadPart=-10000LL * 1000; //5 seconds should be enough time
			
			KeDelayExecutionThread(KernelMode, TRUE, &wait);
		}
		

		//call to entervmmprologue, pushes the return value on the stack
		enterVMMPrologue();

		disableInterrupts();
		disableInterrupts();
		disableInterrupts();
		disableInterrupts();
		disableInterrupts();
		disableInterrupts();
		disableInterrupts();
		disableInterrupts();
		disableInterrupts();
		disableInterrupts();
		disableInterrupts();
		disableInterrupts();
		disableInterrupts();
		disableInterrupts();
		disableInterrupts();
		
		
		DbgPrint("Returned from enterVMMPrologue\n");
		enableInterrupts();

		DbgPrint("cpunr=%d\n",cpunr());
		KeLowerIrql(oldirql);

		DbgPrint("cpunr=%d\n",cpunr());
#else
		
		{
			ULONG vmmentryeip;
			
			__asm
			{
				lea eax,[enterVMMEpilogue]
				mov vmmentryeip,eax 
			}	
			originalstate->rip=(UINT64)vmmentryeip;
		}
		


		__asm{
			
			cli //goodbye interrupts
			xchg bx,bx
			
			mov ebx,vmmPA
			__emit 0x8b
			__emit 0xeb //mov ebp,ebx
			

			lea ebx,NewGDTDescriptor
			mov ecx,pagedirptrbasePA
			mov edx,TemporaryPagingSetupPA //for the mov cr3,ecx
			mov esi,enterVMM2PA
			mov edi,originalstatePA
			
			call [enterVMM2]
			

enterVMMEpilogue:
			//cli //test
			nop
			nop
			nop						
			nop
			nop
			nop						
		}
		
#endif
		DbgPrint("Returning\n");

		return;



	}
	
}