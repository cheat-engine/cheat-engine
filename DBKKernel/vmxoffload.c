/*
sets up all the needed data structures 
copies dbvm into physical memory
jumps into dbvm's os entry point

*/
#pragma warning( disable: 4100 4103 4152 4189 4456)

#ifndef AMD64
#pragma warning( disable: 4740)
#endif

#include <ntifs.h>
#include <windef.h>

#include "dbkfunc.h"
#include "vmxoffload.h"
#include "vmxhelper.h"

#ifdef TOBESIGNED
#include "sigcheck.h"
#endif

unsigned char *vmm;

#pragma pack(2) 
struct
{
	WORD limit;
	UINT_PTR base;
} NewGDTDescriptor;
#pragma pack()

#pragma pack(1) 
typedef struct _INITVARS
{
	UINT64 loadedOS; //physical address of the loadedOS section
	UINT64 vmmstart; //physical address of virtual address 00400000 (obsoletish...)
	UINT64 pagedirlvl4; //Virtual address of the pml4 table (the virtual memory after this until the next 4MB alignment is free to use)
	UINT64 nextstack; //The virtual address of the stack for the next CPU (vmloader only sets it up when 0)
	UINT64 extramemory; //Physical address of some extra initial memory (physically contiguous)
	UINT64 extramemorysize; //the number of pages that extramemory spans
} INITVARS, *PINITVARS;


typedef struct
{ //ok, everything uint64, I hate these incompatibilities with alignment between gcc and ms c
	UINT64		cpucount;
	UINT64		originalEFER;
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

	UINT64		cs_AccessRights;
	UINT64		ss_AccessRights;
	UINT64		ds_AccessRights;
	UINT64		es_AccessRights;
	UINT64		fs_AccessRights;
	UINT64		gs_AccessRights;

	UINT64		cs_Limit;
	UINT64		ss_Limit;
	UINT64		ds_Limit;
	UINT64		es_Limit;
	UINT64		fs_Limit;
	UINT64		gs_Limit;
	
	UINT64		fsbase;
	UINT64		gsbase;

} OriginalState, *POriginalState;
#pragma pack() 

unsigned char *enterVMM2;
PMDL enterVMM2MDL;
POriginalState originalstate; //one of the reasons why multiple cpu's don't start at exactly the same time
PMDL originalstateMDL;

UINT_PTR enterVMM2PA;

PVOID TemporaryPagingSetup;
UINT_PTR TemporaryPagingSetupPA;
PMDL TemporaryPagingSetupMDL;

UINT_PTR DBVMPML4PA;
UINT_PTR originalstatePA;
UINT_PTR NewGDTDescriptorVA;
UINT_PTR vmmPA;

int initializedvmm=0;

KSPIN_LOCK LoadedOSSpinLock; //spinlock to prevent LoadedOS from being overwritten (should not be needed, but just being safe)

#ifdef AMD64
extern void enterVMM( void ); //declared in vmxoffloada.asm
extern void enterVMMPrologue(void);
extern void enterVMMEpilogue(void);
extern void JTAGBP(void);
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

PMDL DBVMMDL;

PINITVARS initvars;

void initializeDBVM(PCWSTR dbvmimgpath)
/*
Runs at passive mode
*/
{
	if (initializedvmm)
		return; //already initialized

	DbgPrint("First time run. Initializing vmm section");

	vmm = ExAllocatePool(PagedPool, 4 * 1024 * 1024);
	
	//default password when dbvm is just loaded (needed for adding extra ram)
	vmx_password1 = 0x76543210;
	vmx_password2 = 0xfedcba98;


	if (vmm)
	{
		int i;
		PHYSICAL_ADDRESS maxPA;
		HANDLE dbvmimghandle;
		UNICODE_STRING filename;
		IO_STATUS_BLOCK statusblock;
		OBJECT_ATTRIBUTES oa;
		NTSTATUS OpenedFile;

		DBVMMDL = IoAllocateMdl((PVOID)vmm, 4 * 1024 * 1024, FALSE, FALSE, NULL);
		if (DBVMMDL)
			MmProbeAndLockPages(DBVMMDL, KernelMode, IoReadAccess);

		vmmPA = (UINT_PTR)MmGetPhysicalAddress(vmm).QuadPart;

		DbgPrint("Allocated memory at virtual address %p (physical address %I64x)\n", vmm, MmGetPhysicalAddress(vmm));
		vmmPA = MmGetMdlPfnArray(DBVMMDL)[0] << 12;
		DbgPrint("(physical address %I64x)\n", vmmPA);

		RtlZeroMemory(vmm, 4 * 1024 * 1024); //initialize

		RtlInitUnicodeString(&filename, dbvmimgpath);


		//Load the .img file
		InitializeObjectAttributes(&oa, &filename, 0, NULL, NULL);
		OpenedFile = ZwCreateFile(&dbvmimghandle, SYNCHRONIZE | STANDARD_RIGHTS_READ, &oa, &statusblock, NULL, FILE_SYNCHRONOUS_IO_NONALERT | FILE_ATTRIBUTE_NORMAL, 0, FILE_OPEN, 0, NULL, 0);

#ifdef TOBESIGNED
		if (OpenedFile==STATUS_SUCCESS)
			OpenedFile=CheckSignatureOfFile(&filename);
#endif

		if (OpenedFile == STATUS_SUCCESS)
		{
			WORD startsector;
			LARGE_INTEGER byteoffset;
			FILE_STANDARD_INFORMATION fsi;
			NTSTATUS ReadFile;






			//Getting filesize
			ZwQueryInformationFile(dbvmimghandle, &statusblock, &fsi, sizeof(fsi), FileStandardInformation);

			//fsi.EndOfFile contains the filesize

			if (fsi.EndOfFile.QuadPart>4 * 1024 * 1024)
			{
				DbgPrint("File bigger than 4MB. Big retard detected\n");
				return;
			}


			byteoffset.QuadPart = 0x8; //offset containing sectornumber of the vmm location
			ReadFile = ZwReadFile(dbvmimghandle, NULL, NULL, NULL, &statusblock, &startsector, 2, &byteoffset, NULL);

			if (ReadFile == STATUS_PENDING)
			{
				if (ZwWaitForSingleObject(dbvmimghandle, FALSE, NULL) != STATUS_SUCCESS)
				{
					DbgPrint("Read failure\n");
					return;
				}
			}

			if (statusblock.Status == STATUS_SUCCESS)
			{

				DWORD vmmsize = fsi.EndOfFile.LowPart;// -(startsector * 512);

				//now read the vmdisk into the allocated memory
				DbgPrint("The startsector=%d (that's offset %d)\n", startsector, startsector * 512);

				byteoffset.QuadPart = startsector * 512;
				ReadFile = ZwReadFile(dbvmimghandle, NULL, NULL, NULL, &statusblock, vmm, vmmsize, &byteoffset, NULL);
				if (ReadFile == STATUS_PENDING)
					ZwWaitForSingleObject(dbvmimghandle, FALSE, NULL);

				vmmsize = (vmmsize + 4096) & 0xfffffffffffff000ULL; //adjust the size internally to a page boundary (sure, there's some mem loss, but it's predicted, dbvm assumes first 10 pages are scratch pages)

				DbgPrint("vmmsize=%x\n", vmmsize);

				if (statusblock.Status == STATUS_SUCCESS)
				{
					//basic paging setup for the vmm, will get expanded by the vmm itself
					UINT64		*GDTBase;
					PPDPTE_PAE	PageMapLevel4;
					PPDPTE_PAE	PageDirPtr;
					PPDE_PAE	PageDir;
					PPTE_PAE	PageTable1, PageTable2;
					UINT_PTR	FreeVA = (((UINT_PTR)vmm + vmmsize) & 0xfffffffffffff000ULL) + 4096; //next free virtual address

					UINT64		mainstack;
					initvars = (PINITVARS)&vmm[0x10];

					mainstack = FreeVA; FreeVA += 16 * 4096;

					GDTBase = (UINT64*)FreeVA; FreeVA += 4096;
					PageDirPtr = (PPDPTE_PAE)FreeVA; FreeVA += 4096;
					PageDir = (PPDE_PAE)FreeVA;  FreeVA += 4096;
					PageTable1 = (PPTE_PAE)FreeVA;  FreeVA += 4096;
					PageTable2 = (PPTE_PAE)FreeVA;  FreeVA += 4096;
					PageMapLevel4 = (PPDPTE_PAE)FreeVA;  FreeVA += 4096; //has to be the last alloc

					DBVMPML4PA = (UINT_PTR)MmGetPhysicalAddress(PageMapLevel4).QuadPart;


					//blame MS for making this hard to read
					DbgPrint("Setting up initial paging table for vmm\n");

					*(PUINT64)(&PageMapLevel4[0]) = MmGetPhysicalAddress(PageDirPtr).QuadPart;
					PageMapLevel4[0].P = 1;
					PageMapLevel4[0].RW = 1;

					*(PUINT64)(&PageDirPtr[0]) = MmGetPhysicalAddress(PageDir).QuadPart;
					PageDirPtr[0].P = 1;
					PageDirPtr[0].RW = 1;

					//DBVM 11 does no longer need the map at 0 to 00400000
					*(PUINT64)(&PageDir[0]) = 0; //00000000-00200000
					PageDir[0].P = 1;
					PageDir[0].RW = 0; //map as readonly (only for the jump to 0x00400000)
					PageDir[0].PS = 1;

					*(PUINT64)(&PageDir[1]) = 0x00200000; //00200000-00400000
					PageDir[1].P = 1;
					PageDir[1].RW = 0;
					PageDir[1].PS = 1;


					{
						*(PUINT64)(&PageDir[2]) = MmGetPhysicalAddress(PageTable1).QuadPart;
						PageDir[2].P = 1;
						PageDir[2].RW = 1;
						PageDir[2].PS = 0; //points to a pagetable 

						*(PUINT64)(&PageDir[3]) = MmGetPhysicalAddress(PageTable2).QuadPart;
						PageDir[3].P = 1;
						PageDir[3].RW = 1;
						PageDir[3].PS = 0;
					}

					//fill in the pagetables
					for (i = 0; i<1024; i++) //pagetable1 and 2 are allocated after eachother, so 1024 can be used here using pagetable1
					{
						*(PUINT64)(&PageTable1[i]) = MmGetPhysicalAddress((PVOID)(((UINT_PTR)vmm) + (4096 * i))).QuadPart;
						PageTable1[i].P = 1;
						PageTable1[i].RW = 1;
					}


					i = (int)((UINT64)((mainstack - (UINT64)vmm)) >> 12);
					PageTable1[i].P = 0; //mark the first page of the stack as unreadable



					//setup GDT
					GDTBase[0] = 0;						//0 :
					GDTBase[1] = 0x00cf92000000ffffULL;	//8 : 32-bit data
					GDTBase[2] = 0x00cf96000000ffffULL;	//16: test, stack, failed, unused
					GDTBase[3] = 0x00cf9b000000ffffULL;	//24: 32-bit code
					GDTBase[4] = 0x00009a000000ffffULL;	//32: 16-bit code
					GDTBase[5] = 0x000092000000ffffULL;	//40: 16-bit data
					GDTBase[6] = 0x00009a030000ffffULL;	//48: 16-bit code, starting at 0x30000
					GDTBase[7] = 0;						//56: 32-bit task	
					GDTBase[8] = 0;						//64: 64-bit task
					GDTBase[9] = 0;						//72:  ^   ^   ^
					GDTBase[10] = 0x00af9b000000ffffULL;	//80: 64-bit code
					GDTBase[11] = 0;						//88:  ^   ^   ^
					GDTBase[12] = 0;						//96: 64-bit tss descriptor (2)
					GDTBase[13] = 0;						//104: ^   ^   ^


					NewGDTDescriptor.limit = 0x6f; //111
					NewGDTDescriptor.base = 0x00400000 + (UINT64)GDTBase - (UINT64)vmm;

					DbgPrint("&NewGDTDescriptor=%p, &NewGDTDescriptor.limit=%p, &NewGDTDescriptor.base=%p\n", &NewGDTDescriptor, &NewGDTDescriptor.limit, &NewGDTDescriptor.base);
					DbgPrint("NewGDTDescriptor.limit=%x\n", NewGDTDescriptor.limit);
					DbgPrint("NewGDTDescriptor.base=%p\n", NewGDTDescriptor.base);

					NewGDTDescriptorVA = (UINT_PTR)&NewGDTDescriptor;


					maxPA.QuadPart = 0x003fffffULL; //allocate 4k at the lower 4MB
					DbgPrint("Before enterVMM2 alloc: maxPA=%I64x\n", maxPA.QuadPart);

					enterVMM2 = MmAllocateContiguousMemory(4096, maxPA);
					if (enterVMM2)
					{
						unsigned char *original = (unsigned char *)enterVMM;
						RtlZeroMemory(enterVMM2, 4096);

						enterVMM2MDL = IoAllocateMdl(enterVMM2, 4096, FALSE, FALSE, NULL);
						MmProbeAndLockPages(enterVMM2MDL, KernelMode, IoReadAccess);

						DbgPrint("enterVMM is located at %p (%I64x)\n", enterVMM, MmGetPhysicalAddress(enterVMM).QuadPart);
						DbgPrint("enterVMM2 is located at %p (%I64x)\n", enterVMM2, MmGetPhysicalAddress(enterVMM2).QuadPart);


						DbgPrint("Copying function till end\n");
						//copy memory

						i = 0;
						while ((i<4096) && ((original[i] != 0xce) || (original[i + 1] != 0xce) || (original[i + 2] != 0xce) || (original[i + 3] != 0xce) || (original[i + 4] != 0xce)))
							i++;

						DbgPrint("size is %d", i);

						RtlCopyMemory(enterVMM2, original, i);
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
					TemporaryPagingSetup = ExAllocatePool(PagedPool, 4 * 4096);
					if (TemporaryPagingSetup == NULL)
					{
						DbgPrint("TemporaryPagingSetup==NULL!!!\n");
						return;
					}

					TemporaryPagingSetupMDL = IoAllocateMdl(TemporaryPagingSetup, 4 * 4096, FALSE, FALSE, NULL);
					MmProbeAndLockPages(TemporaryPagingSetupMDL, KernelMode, IoReadAccess);



					RtlZeroMemory(TemporaryPagingSetup, 4096 * 4);
					DbgPrint("TemporaryPagingSetup is located at %p (%I64x)\n", TemporaryPagingSetup, MmGetPhysicalAddress(TemporaryPagingSetup).QuadPart);


					TemporaryPagingSetupPA = MmGetMdlPfnArray(TemporaryPagingSetupMDL)[0] << 12; // (UINT_PTR)MmGetPhysicalAddress(TemporaryPagingSetup).QuadPart;

					enterVMM2PA = MmGetMdlPfnArray(enterVMM2MDL)[0] << 12;
					DbgPrint("TemporaryPagingSetupPA = (%I64x) (Should be %I64x)\n", (UINT64)TemporaryPagingSetupPA, (UINT64)MmGetPhysicalAddress(TemporaryPagingSetup).QuadPart);
#ifdef AMD64			
					DbgPrint("Setting up temporary paging setup for x64\n");


					{
						PUINT64 PML4Table = (PUINT64)TemporaryPagingSetup;
						PUINT64	PageDirPtr = (PUINT64)((UINT_PTR)TemporaryPagingSetup + 4096);
						PUINT64	PageDir = (PUINT64)((UINT_PTR)TemporaryPagingSetup + 2 * 4096);
						PUINT64	PageTable = (PUINT64)((UINT_PTR)TemporaryPagingSetup + 3 * 4096);

						DbgPrint("PAE paging\n");
						for (i = 0; i<512; i++)
						{
							PML4Table[i] = MmGetPhysicalAddress(PageDirPtr).QuadPart;
							((PPDPTE_PAE)(&PML4Table[i]))->P = 1;

							PageDirPtr[i] = MmGetPhysicalAddress(PageDir).QuadPart;
							((PPDPTE_PAE)(&PageDirPtr[i]))->P = 1;

							PageDir[i] = MmGetPhysicalAddress(PageTable).QuadPart;
							((PPDE_PAE)(&PageDir[i]))->P = 1;
							((PPDE_PAE)(&PageDir[i]))->PS = 0; //4KB

							PageTable[i] = enterVMM2PA;
							((PPTE_PAE)(&PageTable[i]))->P = 1;
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


					//enterVMM2PA = (UINT_PTR)MmGetPhysicalAddress(enterVMM2).QuadPart;




					originalstate = ExAllocatePool(PagedPool, 4096);
					originalstateMDL = IoAllocateMdl(originalstate, 4096, FALSE, FALSE, NULL);
					MmProbeAndLockPages(originalstateMDL, KernelMode, IoReadAccess);

					RtlZeroMemory(originalstate, 4096);
					originalstatePA = MmGetMdlPfnArray(originalstateMDL)[0] << 12; //(UINT_PTR)MmGetPhysicalAddress(originalstate).QuadPart;					

					DbgPrint("enterVMM2PA=%llx\n", enterVMM2PA);
					DbgPrint("originalstatePA=%llx\n", originalstatePA);
					DbgPrint("originalstatePA=%llx\n", (UINT_PTR)MmGetPhysicalAddress(originalstate).QuadPart);

					//setup init vars	
					initvars->loadedOS = originalstatePA;
					initvars->vmmstart = vmmPA;
					initvars->pagedirlvl4 = 0x00400000 + ((UINT64)PageMapLevel4 - (UINT64)vmm);
					initvars->nextstack = 0x00400000 + ((UINT64)mainstack - (UINT64)vmm) + (16 * 4096) - 0x40;

					//add 64KB extra per CPU
					initializedvmm = TRUE;

					KeInitializeSpinLock(&LoadedOSSpinLock);

				}
			}
			ZwClose(dbvmimghandle);


			DbgPrint("Opened and processed: %S\n", filename.Buffer);
		}
		else
		{
			DbgPrint("Failure opening the file. Status=%x  (filename=%S)\n", OpenedFile, filename.Buffer);
		}
		//fill in some specific memory regions

	}
	else
	{
		DbgPrint("Failure allocating the required 4MB\n");
	}
}

void vmxoffload(void)
{
	//save entry state for easy exit in ReturnFromvmxoffload
	EFLAGS eflags;

	PHYSICAL_ADDRESS minPA, maxPA,boundary;
	GDT gdt;
	IDT idt;
	
	/*
	__try
	{
		DbgBreakPoint();
	}
	__except (1)
	{
		DbgPrint("No debugger\n");
	}*/
	
	
	//allocate 8MB of contigues physical memory
	minPA.QuadPart=0;
	maxPA.QuadPart=0xffffffffff000000ULL;
	boundary.QuadPart=0x00800000ULL; //8 mb boundaries


	DbgPrint("vmxoffload\n");



	DbgPrint("initializedvmm=%d\n", initializedvmm); 
	if (initializedvmm)
	{
		DbgPrint("cpunr=%d\n",cpunr());
		
		DbgPrint("Storing original state\n");
		originalstate->cpucount=getCpuCount();
		DbgPrint("originalstate->cpucount=%d",originalstate->cpucount);


		originalstate->originalEFER=readMSR(0xc0000080); //amd prefers this over an LME

		originalstate->originalLME=(int)(((DWORD)(readMSR(0xc0000080)) >> 8) & 1);		
		DbgPrint("originalstate->originalLME=%d",originalstate->originalLME);

		
		originalstate->cr0=(UINT_PTR)getCR0();
		

		DbgPrint("originalstate->cr0=%I64x",originalstate->cr0);

		/*
		{
			int xxx;
			unsigned char *x;
			x=&originalstate->cr0;
			for (xxx=0; xxx<8; xxx++)
			{
				DbgPrint("%x ",x[xxx]);
			}
		}
		*/

		originalstate->cr2=(UINT_PTR)getCR2();
		DbgPrint("originalstate->cr2=%I64x",originalstate->cr2);
		/*
		{
			int xxx;
			unsigned char *x;
			x=&originalstate->cr2;
			for (xxx=0; xxx<8; xxx++)
			{
				DbgPrint("%x ",x[xxx]);
			}
		}*/

		originalstate->cr3=(UINT_PTR)getCR3();
		//DbgPrint("originalstate->cr3=%I64x",originalstate->cr3);

		originalstate->cr4=(UINT_PTR)getCR4();
		//DbgPrint("originalstate->cr4=%I64x",originalstate->cr4);

		originalstate->ss=getSS();
		originalstate->ss_AccessRights = getAccessRights(originalstate->ss);
		originalstate->ss_Limit = getSegmentLimit(originalstate->ss);

		//DbgPrint("originalstate->ss=%I64x",originalstate->ss);
		originalstate->cs=getCS();
		originalstate->cs_AccessRights = getAccessRights(originalstate->cs);
		originalstate->cs_Limit = getSegmentLimit(originalstate->cs);
		//DbgPrint("originalstate->cs=%I64x",originalstate->cs);
		originalstate->ds=getDS();
		originalstate->ds_AccessRights = getAccessRights(originalstate->ds);
		originalstate->ds_Limit = getSegmentLimit(originalstate->ds);
		//DbgPrint("originalstate->ds=%I64x",originalstate->ds);
		originalstate->es=getES();
		originalstate->es_AccessRights = getAccessRights(originalstate->es);
		originalstate->es_Limit = getSegmentLimit(originalstate->es);
		//DbgPrint("originalstate->es=%I64x",originalstate->es);
		originalstate->fs=getFS();
		originalstate->fs_AccessRights = getAccessRights(originalstate->fs);
		originalstate->fs_Limit = getSegmentLimit(originalstate->fs);
		//DbgPrint("originalstate->fs=%I64x",originalstate->fs);
		originalstate->gs=getGS();
		originalstate->gs_AccessRights = getAccessRights(originalstate->gs);
		originalstate->gs_Limit = getSegmentLimit(originalstate->gs);
		//DbgPrint("originalstate->gs=%I64x",originalstate->gs);
		originalstate->ldt=GetLDT();
		//DbgPrint("originalstate->ldt=%I64x",originalstate->ldt);
		originalstate->tr=GetTR();
		//DbgPrint("originalstate->tr=%I64x",originalstate->tr);		


		originalstate->fsbase=readMSR(0xc0000100);
		originalstate->gsbase=readMSR(0xc0000101);

		//DbgPrint("originalstate->fsbase=%I64x originalstate->gsbase=%I64x\n", originalstate->fsbase, originalstate->gsbase);


		originalstate->dr7=getDR7();

		
		gdt.vector=0;
		gdt.wLimit=0;
		GetGDT(&gdt);									
		originalstate->gdtbase=(ULONG_PTR)gdt.vector;
		originalstate->gdtlimit=gdt.wLimit;

		//DbgPrint("originalstate->gdtbase=%I64x",originalstate->gdtbase);
		//DbgPrint("originalstate->gdtlimit=%I64x",originalstate->gdtlimit);

		GetIDT(&idt);
		originalstate->idtbase=(ULONG_PTR)idt.vector;
		originalstate->idtlimit=idt.wLimit;

		//DbgPrint("originalstate->idtbase=%I64x",originalstate->idtbase);
		//DbgPrint("originalstate->idtlimit=%I64x",originalstate->idtlimit);
		
		
		eflags=getEflags();		
		eflags.IF = 0;
		originalstate->rflags=*(PUINT_PTR)&eflags;

		originalstate->rsp=getRSP();
		//DbgPrint("originalstate->rsp=%I64x",originalstate->rsp);
		originalstate->rbp=getRBP();
		//DbgPrint("originalstate->rbp=%I64x",originalstate->rbp);

		originalstate->rax=getRAX();
		//DbgPrint("originalstate->rax=%I64x",originalstate->rax);
		originalstate->rbx=getRBX();
		//DbgPrint("originalstate->rbx=%I64x",originalstate->rbx);
		originalstate->rcx=getRCX();
		//DbgPrint("originalstate->rcx=%I64x",originalstate->rcx);
		originalstate->rdx=getRDX();
		//DbgPrint("originalstate->rdx=%I64x",originalstate->rdx);
		originalstate->rsi=getRSI();
		//DbgPrint("originalstate->rsi=%I64x",originalstate->rsi);
		originalstate->rdi=getRDI();
		//DbgPrint("originalstate->rdi=%I64x",originalstate->rdi);
#ifdef AMD64
		originalstate->r8=getR8();
		//DbgPrint("originalstate->r8=%I64x",originalstate->r8);
		originalstate->r9=getR9();
		//DbgPrint("originalstate->r9=%I64x",originalstate->r9);
		originalstate->r10=getR10();
		//DbgPrint("originalstate->r10=%I64x",originalstate->r10);
		originalstate->r11=getR11();
		//DbgPrint("originalstate->r11=%I64x",originalstate->r11);
		originalstate->r12=getR12();
		//DbgPrint("originalstate->r12=%I64x",originalstate->r12);
		originalstate->r13=getR13();
		//DbgPrint("originalstate->r13=%I64x",originalstate->r13);
		originalstate->r14=getR14();
		//DbgPrint("originalstate->r14=%I64x",originalstate->r14);
		originalstate->r15=getR15();
		//DbgPrint("originalstate->r15=%I64x",originalstate->r15);
#endif
		

#ifdef AMD64
		
		originalstate->rsp-=8; //adjust rsp for the "call entervmmprologue"
 		originalstate->rip=(UINT_PTR)enterVMMEpilogue; //enterVMMEpilogue is an address inside the entervmmprologue function

		//DbgPrint("originalstate->rip=%llx",originalstate->rip);

		//DbgPrint("Calling entervmm2. (Originalstate=%p (%llx))\n",originalstate,originalstatePA);


		

		//call to entervmmprologue, pushes the return value on the stack
		enterVMMPrologue();

		
		enableInterrupts();

		//DbgPrint("Returned from enterVMMPrologue\n");

		//DbgPrint("cpunr=%d\n",cpunr());

	
		

	
		

		//KeLowerIrql(oldirql);


		
		//DbgPrint("cpunr=%d\n",cpunr());
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
			mov ecx,DBVMPML4PA;
			mov edx,TemporaryPagingSetupPA //for the mov cr3,ecx
			
			mov esi,enterVMM2PA
			mov edi,originalstatePA
			
			call [enterVMM2]
			
			//Will never get here. NEVER
			FUUUUU:
			xchg bx,bx
			jmp FUUUUU

		

enterVMMEpilogue:
			//cli //test
			nop
			nop
			xchg bx,bx //bochs bp
			nop
			nop
			sti				
			nop
			nop
			nop						
			nop
			nop
			nop						
		}
		//KeLowerIrql(oldirql);
		
#endif
		//DbgPrint("Returning\n");

		return;



	}
	
}

void vmxoffload_override(CCHAR cpunr, PKDEFERRED_ROUTINE Dpc, PVOID DeferredContext, PVOID *SystemArgument1, PVOID *SystemArgument2)
{
	//runs at passive (in any unrelated cpu)

	//allocate 64KB of extra memory for this(and every other) cpu's DBVM
	PHYSICAL_ADDRESS LowAddress, HighAddress, SkipBytes;
	PMDL mdl;
	DbgPrint("vmxoffload_override\n");
	LowAddress.QuadPart = 0;
	HighAddress.QuadPart = 0xffffffffffffffffI64;
	SkipBytes.QuadPart = 0;
	mdl=MmAllocatePagesForMdl(LowAddress, HighAddress, SkipBytes, 64 * 1024); //do not free this, EVER

	if (mdl)
	{
		//convert the pfnlist to a list DBVM understands
		PDBVMOffloadMemInfo mi = ExAllocatePool(NonPagedPool, sizeof(DBVMOffloadMemInfo));
		int i;
		PFN_NUMBER *pfnlist;

		DbgPrint("vmxoffload_override: mi=%p\n", mi);
		
		mi->List = ExAllocatePool(NonPagedPool, sizeof(UINT64) * 16);

		DbgPrint("vmxoffload_override: mi->list=%p\n", mi->List);

		pfnlist = MmGetMdlPfnArray(mdl);
		
		for (i = 0; i < 16; i++)
		  mi->List[i] = pfnlist[i] << 12;

		mi->Count = 16;

		ExFreePool(mdl);

		*SystemArgument1 = mi;
	}
}

__drv_functionClass(KDEFERRED_ROUTINE) 
__drv_maxIRQL(DISPATCH_LEVEL) 
__drv_minIRQL(DISPATCH_LEVEL) 
__drv_requiresIRQL(DISPATCH_LEVEL)
__drv_sameIRQL VOID
vmxoffload_dpc(
__in struct _KDPC *Dpc,
__in_opt PVOID DeferredContext,
__in_opt PVOID SystemArgument1,
__in_opt PVOID SystemArgument2
)
{
	int c = cpunr();
	DbgPrint("vmxoffload_dpc: CPU %d\n", c);
	KeAcquireSpinLockAtDpcLevel(&LoadedOSSpinLock);
	vmxoffload();

	//still here so very likely DBVM is loaded
	if (SystemArgument1)
	{
		int x;
		PDBVMOffloadMemInfo mi = (PDBVMOffloadMemInfo)SystemArgument1;
		DbgPrint("mi->List=%p mi->Count=%d\n", mi->List, mi->Count);

		x=vmx_add_memory(mi->List, mi->Count);
		DbgPrint("vmx_add_memory returned %x\n", x);

		if (mi->List)
			ExFreePool(mi->List);

		ExFreePool(mi);
	}
	else
		DbgPrint("Error: SystemArgument1=NULL\n");
	KeReleaseSpinLockFromDpcLevel(&LoadedOSSpinLock);
}
