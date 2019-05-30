#pragma warning( disable: 4100 4103 4146 4213)

#include "ntifs.h"
#include <windef.h>
#ifdef CETC
#include "tdiwrapper.h"
#include "kfiles.h"
#endif
#include "memscan.h"
#include "DBKFunc.h"

#include "vmxhelper.h"
#include "vmxoffload.h" //PTE structs
#include "noexceptions.h"
/*#include "deepkernel.h"
*/

UINT_PTR KnownPageTableBase = 0;

void VirtualAddressToIndexes(QWORD address, int *pml4index, int *pagedirptrindex, int *pagedirindex, int *pagetableindex)
/*
* Returns the indexes for the given address  (ia32e)
*/
{
	if (PTESize == 8)
	{
		*pml4index = (address >> 39) & 0x1ff;
		*pagedirptrindex = (address >> 30) & 0x1ff;
		*pagedirindex = (address >> 21) & 0x1ff;
		*pagetableindex = (address >> 12) & 0x1ff;
	}
	else
	{
		*pml4index = 0;
		*pagedirptrindex = 0;
		*pagedirindex = (address >> 22) & 0x3ff;
		*pagetableindex = (address >> 12) & 0x3ff;
	}
}

QWORD IndexesToVirtualAddress(int pml4index, int pagedirptrindex, int pagedirindex, int pagetableindex, int offset)
{
	QWORD r;
#ifndef AMD64
	if (PTESize == 8)
	{
#endif
		r = ((QWORD)pml4index & 0x1ff) << 39;

		if (pml4index >= 256)
			r = r | 0xFFFF000000000000ULL;

		r |= ((QWORD)pagedirptrindex & 0x1ff) << 30;
		r |= ((QWORD)pagedirindex & 0x1ff) << 21;
		r |= ((QWORD)pagetableindex & 0x1ff) << 12;
		r |= offset & 0xfff;

		
#ifndef AMD64
	}
	else
	{
		r |= (pagedirindex & 0x3ff) << 22;
		r |= (pagetableindex & 0x3ff) << 12;
		r |= offset & 0xfff;
	}
#endif

	return r;

}

#ifndef AMD64
void VirtualAddressToPageEntries32(DWORD address, PPDE *pagedirentry, PPTE *pagetableentry)
{
	DWORD PTE = address;
	UINT_PTR PTB = (DWORD)getPageTableBase();
	PTE = PTE >> 12;
	PTE = PTE * 4;
	PTE = PTE + PTB;
	*pagetableentry = (PPTE)PTE;

	UINT_PTR PDE = PTE;
	PDE = PDE & 0x0000ffffffffffffULL;
	PDE = PDE >> 12;
	PDE = PDE * 4;
	PDE = PDE + PTB;
	*pagedirentry = (PPDE)PDE;	
}
#endif

void VirtualAddressToPageEntries64(QWORD address, PPDPTE_PAE *pml4entry, PPDPTE_PAE *pagedirpointerentry, PPDE_PAE *pagedirentry, PPTE_PAE *pagetableentry)
{
	QWORD PTE = address;

	QWORD PTB=getPageTableBase();

	PTE = PTE & 0x0000ffffffffffffULL;
	PTE = PTE >> 12;
	PTE = PTE * 8;
	PTE = PTE + PTB;
	*pagetableentry = (PPTE_PAE)(UINT_PTR)PTE;

	//*pagetableentry = (PPTE_PAE)((((QWORD)address & 0x0000ffffffffffffull) >> 12)*8) + 0xfffff80000000000ULL;

	QWORD PDE = PTE;
	PDE = PDE & 0x0000ffffffffffffULL;
	PDE = PDE >> 12;
	PDE = PDE * 8;
	PDE = PDE + PTB;
	*pagedirentry = (PPDE_PAE)(UINT_PTR)PDE;


	//*pagedirentry = (PPDE_PAE)((((QWORD)*pagetableentry & 0x0000ffffffffffffull )>> 12)*8) + 0xfffff80000000000ULL;

	QWORD PDPTR = PDE;
	PDPTR = PDPTR & 0x0000ffffffffffffULL;
	PDPTR = PDPTR >> 12;
	PDPTR = PDPTR * 8;
	PDPTR = PDPTR + PTB;
	*pagedirpointerentry = (PPDPTE_PAE)(UINT_PTR)PDPTR;

	//*pagedirpointerentry = (PPDPTE_PAE)((((QWORD)*pagedirentry & 0x0000ffffffffffffull )>> 12)*8) + 0xfffff80000000000ULL;
#ifdef AMD64
	QWORD PML4 = PDPTR;
	PML4 = PML4 & 0x0000ffffffffffffULL;
	PML4 = PML4 >> 12;
	PML4 = PML4 * 8;
	PML4 = PML4 + PTB;
	*pml4entry = (PPDPTE_PAE)PML4;
#else
	*pml4entry = NULL;
#endif
}

BOOLEAN IsAddressSafe(UINT_PTR StartAddress)
{
	#ifdef AMD64
	//cannonical check. Bits 48 to 63 must match bit 47
	UINT_PTR toppart=(StartAddress >> 47);
	if (toppart & 1)
	{
		//toppart must be 0x1ffff
		if (toppart != 0x1ffff)
			return FALSE;
	}
	else
	{
		//toppart must be 0
		if (toppart != 0)
			return FALSE;

	}

	#endif

	//return TRUE;
	if (loadedbydbvm)
	{
		BYTE x=0;
		UINT_PTR lasterror;
		disableInterrupts();
		vmx_disable_dataPageFaults();

		x=*(volatile BYTE *)StartAddress;

		vmx_enable_dataPageFaults();
		lasterror=vmx_getLastSkippedPageFault();
		enableInterrupts();

		DbgPrint("IsAddressSafe dbvm-mode: lastError=%p\n", lasterror);
		
		if (lasterror) return FALSE;		
	}


	{
#ifdef AMD64
		UINT_PTR kernelbase=0x7fffffffffffffffULL;

		
		if (StartAddress<kernelbase)
			return TRUE;
		else
		{
			PHYSICAL_ADDRESS physical;
			physical.QuadPart=0;
			physical=MmGetPhysicalAddress((PVOID)StartAddress);
			return (physical.QuadPart!=0);
		}
#else
	/*	MDL x;

		
		MmProbeAndLockPages(&x,KernelMode,IoModifyAccess);


		MmUnlockPages(&x);
		*/
		ULONG kernelbase=0x7ffe0000;

		if ((!HiddenDriver) && (StartAddress<kernelbase))
			return TRUE;

		{
			UINT_PTR PTE,PDE;
			struct PTEStruct *x;
			
			/*
			PHYSICAL_ADDRESS physical;
			physical=MmGetPhysicalAddress((PVOID)StartAddress);
			return (physical.QuadPart!=0);*/


			PTE=(UINT_PTR)StartAddress;
			PTE=PTE/0x1000*PTESize+0xc0000000;

    		//now check if the address in PTE is valid by checking the page table directory at 0xc0300000 (same location as CR3 btw)
			PDE=PTE/0x1000*PTESize+0xc0000000; //same formula

			x=(PVOID)PDE;
			if ((x->P==0) && (x->A2==0))
			{
				//Not present or paged, and since paging in this area isn't such a smart thing to do just skip it
				//perhaps this is only for the 4 mb pages, but those should never be paged out, so it should be 1
				//bah, I've got no idea what this is used for
				return FALSE;
			}

			if (x->PS==1)
			{
				//This is a 4 MB page (no pte list)
				//so, (startaddress/0x400000*0x400000) till ((startaddress/0x400000*0x400000)+(0x400000-1) ) ) is specified by this page
			}
			else //if it's not a 4 MB page then check the PTE
			{
				//still here so the page table directory agreed that it is a usable page table entry
				x=(PVOID)PTE;
				if ((x->P==0) && (x->A2==0))
					return FALSE; //see for explenation the part of the PDE
			}

			return TRUE;
		} 
#endif
	}

}

UINT_PTR getPEThread(UINT_PTR threadid)  
{	
    //UINT_PTR *threadid;
	PETHREAD selectedthread;
	UINT_PTR result=0;
	
	

	if (PsLookupThreadByThreadId((PVOID)(UINT_PTR)threadid,&selectedthread)==STATUS_SUCCESS)
	{
		result=(UINT_PTR)selectedthread;
		ObDereferenceObject(selectedthread);
	}

	return result;
}

BOOLEAN WriteProcessMemory(DWORD PID,PEPROCESS PEProcess,PVOID Address,DWORD Size, PVOID Buffer)
{
	PEPROCESS selectedprocess=PEProcess;
	KAPC_STATE apc_state;
	NTSTATUS ntStatus=STATUS_UNSUCCESSFUL;
		
	if (selectedprocess==NULL)
	{
		//DbgPrint("WriteProcessMemory:Getting PEPROCESS\n");
        if (!NT_SUCCESS(PsLookupProcessByProcessId((PVOID)(UINT_PTR)PID,&selectedprocess)))
		   return FALSE; //couldn't get the PID

		//DbgPrint("Retrieved peprocess");  
	}

	//selectedprocess now holds a valid peprocess value
	__try
	{
		RtlZeroMemory(&apc_state,sizeof(apc_state));					

    	KeAttachProcess((PEPROCESS)selectedprocess);				

        __try
        {
			char* target;
			char* source;
			unsigned int i;	

			//DbgPrint("Checking safety of memory\n");

			if ((IsAddressSafe((UINT_PTR)Address)) && (IsAddressSafe((UINT_PTR)Address+Size-1)))
			{			

	    		//still here, then I gues it's safe to read. (But I can't be 100% sure though, it's still the users problem if he accesses memory that doesn't exist)
				BOOL disabledWP = FALSE;

				target=Address;
				source=Buffer;

				if ((loadedbydbvm) || (KernelWritesIgnoreWP))  //add a extra security around it as the PF will not be handled
				{
					disableInterrupts();

					if (loadedbydbvm)
						vmx_disable_dataPageFaults();

					if (KernelWritesIgnoreWP)
					{
						DbgPrint("Disabling CR0.WP");
						setCR0(getCR0() & (~(1 << 16))); //disable the WP bit					
						disabledWP = TRUE;							
					}
				}

				
				if ((loadedbydbvm) || ((UINT_PTR)target < 0x8000000000000000ULL))
				{
					RtlCopyMemory(target, source, Size);
					ntStatus = STATUS_SUCCESS;
				}
				else
				{
					i = NoExceptions_CopyMemory(target, source, Size);
					if (i != (int)Size)
						ntStatus = STATUS_UNSUCCESSFUL;
					else
						ntStatus = STATUS_SUCCESS;
				}
				   
				if ((loadedbydbvm) || (disabledWP))
				{
					UINT_PTR lastError=0;

					if (disabledWP)
					{						
						setCR0(getCR0() | (1 << 16));
						DbgPrint("Enabled CR0.WP");
					}

					if (loadedbydbvm)
					{
						lastError = vmx_getLastSkippedPageFault();
						vmx_enable_dataPageFaults();
					}

					enableInterrupts();

					DbgPrint("lastError=%p\n", lastError);
					if (lastError)
						ntStatus=STATUS_UNSUCCESSFUL;
				}

			}

			
		}
		__finally
		{
			KeDetachProcess();
		}
	}			
	__except(1)
	{
		//DbgPrint("Error while writing\n");
		ntStatus = STATUS_UNSUCCESSFUL;
	}
	
	if (PEProcess==NULL) //no valid peprocess was given so I made a reference, so lets also dereference
		ObDereferenceObject(selectedprocess);

	return NT_SUCCESS(ntStatus);
}


BOOLEAN ReadProcessMemory(DWORD PID,PEPROCESS PEProcess,PVOID Address,DWORD Size, PVOID Buffer)
{
	PEPROCESS selectedprocess=PEProcess;
	//KAPC_STATE apc_state;
	NTSTATUS ntStatus=STATUS_UNSUCCESSFUL;

	if (PEProcess==NULL)
	{
		if (!NT_SUCCESS(PsLookupProcessByProcessId((PVOID)(UINT_PTR)PID,&selectedprocess)))
		   return FALSE; //couldn't get the PID
 
	}

	//selectedprocess now holds a valid peprocess value
	__try
	{
    	KeAttachProcess((PEPROCESS)selectedprocess);



        __try
        {
			char* target;
			char* source;
			int i;

		
			if ((IsAddressSafe((UINT_PTR)Address)) && (IsAddressSafe((UINT_PTR)Address+Size-1)))
			{
				


				target=Buffer;
				source=Address;

				if (loadedbydbvm) //add a extra security around it
				{
					disableInterrupts();
					vmx_disable_dataPageFaults();
				}

			
				if ((loadedbydbvm) || ((UINT_PTR)source < 0x8000000000000000ULL))
				{
					RtlCopyMemory(target, source, Size);
					ntStatus = STATUS_SUCCESS;
				}
				else
				{
					i=NoExceptions_CopyMemory(target, source, Size);
					if (i != (int)Size)
						ntStatus = STATUS_UNSUCCESSFUL;
					else
						ntStatus = STATUS_SUCCESS;
				}
				
				

				if (loadedbydbvm)
				{
					UINT_PTR lastError;
					lastError=vmx_getLastSkippedPageFault();
					vmx_enable_dataPageFaults();

					enableInterrupts();

					DbgPrint("lastError=%p\n", lastError);
					if (lastError)
						ntStatus=STATUS_UNSUCCESSFUL;
				}

				
			}
				
		}
		__finally
		{

			KeDetachProcess();
		}
	}			
	__except(1)
	{
		//DbgPrint("Error while reading: ReadProcessMemory(%x,%p, %p, %d, %p\n", PID, PEProcess, Address, Size, Buffer);

		ntStatus = STATUS_UNSUCCESSFUL;
	}
	
	if (PEProcess==NULL) //no valid peprocess was given so I made a reference, so lets also dereference
		ObDereferenceObject(selectedprocess);

	return NT_SUCCESS(ntStatus);
}

UINT64 maxPhysAddress = 0;
UINT64 getMaxPhysAddress(void)
{
	if (maxPhysAddress==0)			
	{
		int physicalbits;
		DWORD r[4];
		__cpuid(r, 0x80000008);

		//get max physical address
		physicalbits = r[0] & 0xff;

		maxPhysAddress = 0xFFFFFFFFFFFFFFFFULL;
		maxPhysAddress = maxPhysAddress >> physicalbits; //if physicalbits==36 then maxPhysAddress=0x000000000fffffff
		maxPhysAddress = ~(maxPhysAddress << physicalbits); //<< 36 = 0xfffffff000000000 .  after inverse : 0x0000000fffffffff		
	}
	
	return maxPhysAddress;
}


NTSTATUS ReadPhysicalMemory(char *startaddress, UINT_PTR bytestoread, void *output)
{
	HANDLE			physmem;
	UNICODE_STRING	physmemString;
	OBJECT_ATTRIBUTES attributes;
	WCHAR			physmemName[] = L"\\device\\physicalmemory";
	UCHAR*			memoryview;
	NTSTATUS		ntStatus = STATUS_UNSUCCESSFUL;
	PMDL			outputMDL;

	DbgPrint("ReadPhysicalMemory(%p, %d, %p)", startaddress, bytestoread, output);

	if (((UINT64)startaddress > getMaxPhysAddress()) || ((UINT64)startaddress + bytestoread > getMaxPhysAddress()))
	{
		DbgPrint("Invalid physical address\n");
		return ntStatus;
	}
	
	outputMDL = IoAllocateMdl(output, (ULONG)bytestoread, FALSE, FALSE, NULL);
	__try
	{
		MmProbeAndLockPages(outputMDL, KernelMode, IoWriteAccess);
	}
	__except (1)
	{
		IoFreeMdl(outputMDL);
		return STATUS_UNSUCCESSFUL;
	}

	__try
	{
		RtlInitUnicodeString( &physmemString, physmemName );	

		InitializeObjectAttributes( &attributes, &physmemString, OBJ_CASE_INSENSITIVE, NULL, NULL );	
		ntStatus=ZwOpenSection( &physmem, SECTION_ALL_ACCESS, &attributes );
		if (ntStatus==STATUS_SUCCESS)
		{
			//hey look, it didn't kill it
			SIZE_T length;
			PHYSICAL_ADDRESS	viewBase;
			UINT_PTR offset;
			UINT_PTR toread;

			viewBase.QuadPart = (ULONGLONG)(startaddress);					
			
			length=0x2000;//pinp->bytestoread; //in case of a overlapping region
			toread=bytestoread;

			memoryview=NULL;

			DbgPrint("ReadPhysicalMemory:viewBase.QuadPart=%x", viewBase.QuadPart); 


			ntStatus=ZwMapViewOfSection(
				physmem,  //sectionhandle
				NtCurrentProcess(), //processhandle (should be -1)
				&memoryview, //BaseAddress
				0L, //ZeroBits
				length, //CommitSize
				&viewBase, //SectionOffset
				&length, //ViewSize
				ViewShare,
				0,
				PAGE_READWRITE);

			if ((ntStatus == STATUS_SUCCESS) && (memoryview!=NULL))
			{
				if (toread > length)
					toread = length;

				if (toread)
				{
					__try
					{

						offset = (UINT_PTR)(startaddress)-(UINT_PTR)viewBase.QuadPart;

						if (offset + toread > length)
						{
							DbgPrint("Too small map");
						}
						else
						{
							RtlCopyMemory(output, &memoryview[offset], toread);
						}

						ZwUnmapViewOfSection(NtCurrentProcess(), memoryview);
					}
					__except (1)
					{
						DbgPrint("Failure mapping physical memory");
					}					
				}
			}
			else
			{
				DbgPrint("ReadPhysicalMemory error:ntStatus=%x", ntStatus); 
			}

			ZwClose(physmem);
		};
	}
	__except(1)
	{
		DbgPrint("Error while reading physical memory\n");
	}

	MmUnlockPages(outputMDL);
	IoFreeMdl(outputMDL);
	return ntStatus;
}

UINT_PTR SignExtend(UINT_PTR a)
{
#ifdef AMD64
	if ((a >> 47)==1)
		return a | 0xFFFF000000000000ULL; //add sign extended bits
	else
		return a;
#else
	return a;
#endif
}


UINT_PTR getPageTableBase()
{
#if (NTDDI_VERSION >= NTDDI_VISTA)
	if (KnownPageTableBase==0)
	{
		RTL_OSVERSIONINFOW v;
		v.dwOSVersionInfoSize = sizeof(v);
		if (RtlGetVersion(&v))
		{
			DbgPrint("RtlGetVersion failed");
			return 0;
		}

		if ((v.dwMajorVersion >= 10) && (v.dwBuildNumber >= 14393))
		{
			PHYSICAL_ADDRESS a;
			PVOID r;
			a.QuadPart = getCR3() & 0xFFFFFFFFFFFFF000ULL;
			r = MmGetVirtualForPhysical(a); //if this stops working, look for CR3 in the pml4 table

			KnownPageTableBase = ((UINT_PTR)r) & 0xFFFFFF8000000000ULL;

			MAX_PTE_POS = (UINT_PTR)((QWORD)KnownPageTableBase + 0x7FFFFFFFF8ULL);
			MAX_PDE_POS = (UINT_PTR)((QWORD)KnownPageTableBase + 0x7B7FFFFFF8ULL);
		

		}
		else
			KnownPageTableBase=PAGETABLEBASE;

		DbgPrint("PageTableBase at %p\n", KnownPageTableBase);
	}	

	return KnownPageTableBase;
#else
	return PAGETABLEBASE;
#endif

}

typedef void PRESENTPAGECALLBACK(UINT_PTR StartAddress, UINT_PTR EndAddress, struct PTEStruct *pageEntry);
BOOL walkPagingLayout(PEPROCESS PEProcess, UINT_PTR MaxAddress, PRESENTPAGECALLBACK OnPresentPage)
{
#ifdef AMD64
	UINT_PTR pagebase = getPageTableBase();
#else
	UINT_PTR pagebase = PAGETABLEBASE;
#endif

	if (pagebase == 0)
		return FALSE;

	if (OnPresentPage == NULL)
		return FALSE;

	__try
	{
		KeAttachProcess((PEPROCESS)PEProcess);
		__try
		{
			UINT_PTR currentAddress = 0; //start from address 0
			UINT_PTR lastAddress = 0;
			struct PTEStruct *PPTE, *PPDE, *PPDPE, *PPML4E;

			while ((currentAddress < MaxAddress) && (lastAddress<=currentAddress) )
			{
				//DbgPrint("currentAddress=%p\n", currentAddress);
				lastAddress = currentAddress;

				
				(UINT_PTR)PPTE = (UINT_PTR)(((currentAddress & 0xFFFFFFFFFFFFULL) >> 12) *PTESize + pagebase);
				(UINT_PTR)PPDE = (UINT_PTR)((((UINT_PTR)PPTE) & 0xFFFFFFFFFFFFULL) >> 12) *PTESize + pagebase;
				(UINT_PTR)PPDPE = (UINT_PTR)((((UINT_PTR)PPDE) & 0xFFFFFFFFFFFFULL) >> 12) *PTESize + pagebase;
				(UINT_PTR)PPML4E = (UINT_PTR)((((UINT_PTR)PPDPE) & 0xFFFFFFFFFFFFULL) >> 12) *PTESize + pagebase;
				if (PTESize == 8)
					(UINT_PTR)PPDPE = ((((UINT_PTR)PPDE) & 0xFFFFFFFFFFFFULL) >> 12) *PTESize + pagebase;
				else
					(UINT_PTR)PPDPE = 0;

#ifdef AMD64
				(UINT_PTR)PPML4E = ((((UINT_PTR)PPDPE) & 0xFFFFFFFFFFFFULL) >> 12) *PTESize + pagebase;
#else
				(UINT_PTR)PPML4E = 0;
#endif

	
#ifdef AMD64
				if ((PPML4E) && (PPML4E->P == 0))
				{
					currentAddress &= 0xffffff8000000000ULL;
					currentAddress += 0x8000000000ULL;	
					continue;
				}
#endif

				if ((PPDPE) && (PPDPE->P == 0))
				{		
					currentAddress &= 0xffffffffc0000000ULL;
					currentAddress += 0x40000000;						
					continue;
				}

				if (PPDE->P == 0)
				{
				
					if (PAGE_SIZE_LARGE == 0x200000)
						currentAddress &= 0xffffffffffe00000ULL;
					else
						currentAddress &= 0xffffffffffc00000ULL;

					currentAddress += PAGE_SIZE_LARGE;			
	
					continue;
				}

				if (PPDE->PS)
				{
					OnPresentPage(currentAddress, currentAddress + PAGE_SIZE_LARGE-1, PPDE);					
					currentAddress += PAGE_SIZE_LARGE;
					continue;
				}

				if (PPTE->P == 0)
				{		
				
					currentAddress &= 0xfffffffffffff000ULL;
					currentAddress += 0x1000;
					
					continue;
				}
				
				OnPresentPage(currentAddress, currentAddress + 0xfff, PPTE);				
				currentAddress += 0x1000;
			}

		}
		__finally
		{
			KeDetachProcess();
		}
	}
	__except (1)
	{
		DbgPrint("Excepion while walking the paging layout\n");
		return FALSE;
	}

	return TRUE;
}




PPENTRY AccessedList = NULL;
int AccessedListSize;

void CleanAccessedList()
{	
	PPENTRY e = AccessedList;
	PPENTRY previous;
	//DbgPrint("Cleaning list");

	while (e)
	{
		previous = e;
		e = e->Next;
		ExFreePool(previous);
	}

	AccessedList = NULL;
	AccessedListSize = 0;
}
	

void StoreAccessedRanges(UINT_PTR StartAddress, UINT_PTR EndAddress, struct PTEStruct *pageEntry)
{
	if (pageEntry->A)
	{		
		if ((AccessedList) && (AccessedList->Range.EndAddress == StartAddress - 1)) //group
			AccessedList->Range.EndAddress = EndAddress;
		else
		{
			//insert
			PPENTRY e;
			e = ExAllocatePool(PagedPool, sizeof(PENTRY));
			e->Range.StartAddress = StartAddress;
			e->Range.EndAddress = EndAddress;
			e->Next = AccessedList;

			AccessedList = e;
			AccessedListSize++;
		}

		
	}
}


int enumAllAccessedPages(PEPROCESS PEProcess)
{
#ifdef AMD64
	UINT_PTR MaxAddress = 0x80000000000ULL;
#else
	UINT_PTR MaxAddress = 0x80000000;
#endif

	CleanAccessedList();

	if (walkPagingLayout(PEProcess, MaxAddress, StoreAccessedRanges))
	{
		//DbgPrint("AccessedListSize=%d\n", AccessedListSize);
		return AccessedListSize*sizeof(PRANGE);
	}
	else
		return 0;
}

int getAccessedPageList(PPRANGE List, int ListSizeInBytes)
{
	PPENTRY e = AccessedList;
	int maxcount = ListSizeInBytes / sizeof(PRANGE);
	int i = 0;

//	DbgPrint("getAccessedPageList\n");

	while (e)
	{
		if (i >= maxcount)
		{
			//DbgPrint("%d>=%d", i, maxcount);
			break;
		}

		//DbgPrint("i=%d  (%p -> %p)\n", i, e->Range.StartAddress, e->Range.EndAddress);
		List[i] = e->Range;
		e = e->Next;

		i++;
	}

	CleanAccessedList();

	return i*sizeof(PRANGE);
}


void MarkPageAsNotAccessed(UINT_PTR StartAddress, UINT_PTR EndAddress, struct PTEStruct *pageEntry)
{
	pageEntry->A = 0;
}

NTSTATUS markAllPagesAsNeverAccessed(PEPROCESS PEProcess)
{
#ifdef AMD64
	UINT_PTR MaxAddress = 0x80000000000ULL;	
#else
	UINT_PTR MaxAddress = 0x80000000;
#endif

	if (walkPagingLayout(PEProcess, MaxAddress, MarkPageAsNotAccessed))
		return STATUS_SUCCESS;
	else
		return STATUS_UNSUCCESSFUL;

}



UINT_PTR FindFirstDifferentAddress(QWORD address, DWORD *protection)
//scans the pagetable system for the first fully present entry
//returns 0 when there is no other present address
{
	int i,ri;
	int pml4index, pagedirpointerindex, pagedirindex, pagetableindex;
	VirtualAddressToIndexes(address, &pml4index, &pagedirpointerindex, &pagedirindex, &pagetableindex);
#ifndef AMD64
	if (PTESize == 8)
#endif
	{
		PPDPTE_PAE pml4entry, pagedirpointerentry;
		PPDE_PAE pagedirentry;
		PPTE_PAE pagetableentry;
		*protection = 0;

		//scan till present or end of memory
		while (1)
		{
			VirtualAddressToPageEntries64(address, &pml4entry, &pagedirpointerentry, &pagedirentry, &pagetableentry);
			VirtualAddressToIndexes(address, &pml4index, &pagedirpointerindex, &pagedirindex, &pagetableindex);

			if (*protection == 0)
			{
				//get the initial protection
				if ((((pml4entry) && (pml4entry->P)) || (pml4entry == NULL)) && (pagedirpointerentry->P) && (pagedirentry->P) && ((pagedirentry->PS) || (pagetableentry->P)))
				{
					if (pagedirentry->PS)
					{
						if (pagedirentry->RW)
							*protection = PAGE_EXECUTE_READWRITE;
						else
							*protection = PAGE_EXECUTE_READ;
					}
					else
					{
						if (pagetableentry->RW)
							*protection = PAGE_EXECUTE_READWRITE;
						else
							*protection = PAGE_EXECUTE_READ;
					}
				}
				else
					*protection = PAGE_NOACCESS;
			}

#ifdef AMD64			
			if (pml4entry->P == 0)
			{
				//unreadable at PML4 level

				if (*protection != PAGE_NOACCESS)
					return address;

				pagedirpointerindex = 0;
				pagedirindex = 0;
				pagetableindex = 0;

				for (ri=1, i = pml4index + 1; i < 512; i++, ri++)
				{
					if ((ri >= 512) || (ri < 0))
						DbgBreakPointWithStatus(ri);
						
					if (pml4entry[ri].P)
					{
						//found a valid PML4 entry
						//scan for a valid pagedirpointerentry
						pml4index = i;
						address = IndexesToVirtualAddress(pml4index, pagedirpointerindex, pagedirindex, pagetableindex, 0);
						VirtualAddressToPageEntries64(address, &pml4entry, &pagedirpointerentry, &pagedirentry, &pagetableentry);
						break;
					}

					
				}
			}

			if (pml4entry->P == 0)
			{
				//nothing readable found
				return 0;
			}
#endif	

			if (pagedirpointerentry->P == 0)
			{
				if (*protection != PAGE_NOACCESS)
					return (UINT_PTR)address;

				pagedirindex = 0;
				pagetableindex = 0;

				for (ri=1, i = pagedirpointerindex + 1; i < 512; i++, ri++)
				{
					if ((ri >= 512) || (ri < 0))
						DbgBreakPointWithStatus(ri);

					if (pagedirpointerentry[ri].P)
					{
						//found a valid pagedirpointerentry
						pagedirpointerindex = i;
						address = IndexesToVirtualAddress(pml4index, pagedirpointerindex, pagedirindex, pagetableindex, 0);
						VirtualAddressToPageEntries64(address, &pml4entry, &pagedirpointerentry, &pagedirentry, &pagetableentry);
						break;
					}
				}

				if (pagedirpointerentry->P == 0)
				{
#ifdef AMD64
					pagedirpointerindex = 0;
					pml4index++;
					if (pml4index >= 512) return 0; //end of the list

					address = IndexesToVirtualAddress(pml4index, pagedirpointerindex, pagedirindex, pagetableindex, 0);
					continue; //try the next PML4 entry
#else
					//nothing readable found
					return 0;
#endif
				}
			}

			if (pagedirentry->P == 0)
			{
				if (*protection != PAGE_NOACCESS)
					return (UINT_PTR)address;

				pagetableindex = 0;
				for (ri=1, i = pagedirindex + 1; i < 512; i++, ri++)
				{
					if ((ri >= 512) || (ri < 0))
						DbgBreakPointWithStatus(ri);

					if (pagedirentry[ri].P)
					{
						//found a valid pagedirentry
						pagedirindex = i;
						address = IndexesToVirtualAddress(pml4index, pagedirpointerindex, pagedirindex, pagetableindex, 0);
						VirtualAddressToPageEntries64(address, &pml4entry, &pagedirpointerentry, &pagedirentry, &pagetableentry);
						break;
					}
				}

				if (pagedirentry->P == 0)
				{
					pagedirindex = 0;
					pagedirpointerindex++;
					if (pagedirpointerindex >= 512)
					{
						pagedirpointerindex = 0;
						pml4index++;
						if (pml4index >= 512) return 0; //end of the list
					}

					address = IndexesToVirtualAddress(pml4index, pagedirpointerindex, pagedirindex, pagetableindex, 0);
					continue; //try the next pagedirpointer entry
				}
			}

			if (pagedirentry->PS)
			{
				if (*protection == PAGE_NOACCESS)
					return (UINT_PTR)address;

				if ((pagedirentry->RW) && (*protection != PAGE_EXECUTE_READWRITE))
					return (UINT_PTR)address;

				//go to the next one
				pagedirindex++;
				if (pagedirindex >= 512)
				{
					pagedirindex = 0;
					pagedirpointerindex++;
					if (pagedirpointerindex >= 512)
					{
						pagedirpointerindex = 0;
						pml4index++;
						if (pml4index >= 512)
							return 0; //end of the list
					}
				}
				address = IndexesToVirtualAddress(pml4index, pagedirpointerindex, pagedirindex, pagetableindex, 0);
				continue;
			}


			if (pagetableentry->P == 0)
			{
				if (*protection != PAGE_NOACCESS)
					return (UINT_PTR)address;

				for (ri=1, i = pagetableindex + 1; i < 512; i++, ri++)
				{
					if ((ri >= 512) || (ri < 0))
						DbgBreakPointWithStatus(ri);

					if (pagetableentry[ri].P)
					{
						//found a valid pagetable entry
						pagetableindex = i;
						address = IndexesToVirtualAddress(pml4index, pagedirpointerindex, pagedirindex, pagetableindex, 0);
						VirtualAddressToPageEntries64(address, &pml4entry, &pagedirpointerentry, &pagedirentry, &pagetableentry);
						break;
					}
				}

				if (pagetableentry->P == 0)
				{
					pagetableindex = 0;
					pagedirindex++;
					if (pagedirindex >= 512)
					{
						pagedirindex = 0;
						pagedirpointerindex++;
						if (pagedirpointerindex >= 512)
						{
							pagedirpointerindex = 0;
							pml4index++;
							if (pml4index >= 512)
								return 0; //end of the list
						}
					}
					address = IndexesToVirtualAddress(pml4index, pagedirpointerindex, pagedirindex, pagetableindex, 0);
					continue; //try the next pagedir entry
				}
			}

			//still here so a present page
			if (*protection == PAGE_NOACCESS)
				return (UINT_PTR)address;

			if ((pagetableentry->RW) && (*protection != PAGE_EXECUTE_READWRITE))
				return (UINT_PTR)address;

			//next entry
			pagetableindex++;
			if (pagetableindex >= 512)
			{
				pagetableindex = 0;
				pagedirindex++;
				if (pagedirindex >= 512)
				{
					pagedirindex = 0;
					pagedirpointerindex++;
					if (pagedirpointerindex >= 512)
					{
						pagedirpointerindex = 0;
						pml4index++;
						if (pml4index >= 512)
							return 0;
					}
				}
			}
			address = IndexesToVirtualAddress(pml4index, pagedirpointerindex, pagedirindex, pagetableindex, 0);
		}
	}
#ifndef AMD64
	else
	{
		PPDE pagedirentry;
		PPTE pagetableentry;
		while (1)
		{
			VirtualAddressToPageEntries32((UINT_PTR)address, &pagedirentry, &pagetableentry);
			VirtualAddressToIndexes((UINT_PTR)address, &pml4index, &pagedirpointerindex, &pagedirindex, &pagetableindex);

			if (*protection == 0)
			{
				//get the initial protection
				if ((pagedirentry->P) && ((pagedirentry->PS) || (pagetableentry->P)))
				{
					if (pagedirentry->PS)
					{
						if (pagedirentry->RW)
							*protection = PAGE_EXECUTE_READWRITE;
						else
							*protection = PAGE_EXECUTE_READ;
					}
					else
					{
						if (pagetableentry->RW)
							*protection = PAGE_EXECUTE_READWRITE;
						else
							*protection = PAGE_EXECUTE_READ;
					}
				}
				else
					*protection = PAGE_NOACCESS;
			}

			if (pagedirentry->P == 0)
			{
				if (*protection != PAGE_NOACCESS)
					return (UINT_PTR)address;

				pagetableindex = 0;
				for (ri=1, i = pagedirindex + 1; i < 1024; i++, ri++)
				{
					if (pagedirentry[ri].P)
					{
						//found a valid pagedirentry
						pagedirindex = i;
						address = IndexesToVirtualAddress(0, 0, pagedirindex, pagetableindex, 0);
						VirtualAddressToPageEntries32((UINT_PTR)address, &pagedirentry, &pagetableentry);
						break;
					}
				}

				if (pagedirentry->P == 0)
					return 0; //end of the list
			}

			if (pagedirentry->PS)
			{
				if (*protection == PAGE_NOACCESS)
					return (UINT_PTR)address;

				if ((pagedirentry->RW) && (*protection != PAGE_EXECUTE_READWRITE))
					return (UINT_PTR)address;

				//go to the next one
				pagedirindex++;
				if (pagedirindex >= 1024)
					return 0;
				
				address = IndexesToVirtualAddress(0, 0, pagedirindex, pagetableindex, 0);
			}


			if (pagetableentry->P == 0)
			{
				if (*protection != PAGE_NOACCESS)
					return (UINT_PTR)address;

				for (ri=1, i = pagetableindex + 1; i < 1024; i++, ri++)
				{
					if (pagetableentry[ri].P)
					{
						//found a valid pagetable entry
						pagetableindex = i;
						address = IndexesToVirtualAddress(0, 0, pagedirindex, pagetableindex, 0);
						break;
					}
				}

				if (pagetableentry->P == 0)
				{
					pagetableindex = 0;
					pagedirindex++;
					if (pagedirindex >= 1024)
						return 0;

					address = IndexesToVirtualAddress(0, 0, pagedirindex, pagetableindex, 0);
					continue; //try the next pagedir entry
				}
			}

			//still here so a present page
			if (*protection == PAGE_NOACCESS)
				return (UINT_PTR)address;

			if ((pagetableentry->RW) && (*protection != PAGE_EXECUTE_READWRITE))
				return (UINT_PTR)address;

			//next entry
			pagetableindex++;
			if (pagetableindex >= 1024)
			{
				pagetableindex = 0;
				pagedirindex++;
				if (pagedirindex >= 1024)
					return 0;				
			}
			address = IndexesToVirtualAddress(pml4index, pagedirpointerindex, pagedirindex, pagetableindex, 0);

		}

	}
#endif	
}


BOOLEAN GetMemoryRegionData(DWORD PID,PEPROCESS PEProcess, PVOID mempointer,ULONG *regiontype, UINT_PTR *memorysize,UINT_PTR *baseaddress)
{
	UINT_PTR CurrentAddress;
	KAPC_STATE apc_state;
	PEPROCESS selectedprocess=PEProcess;

	if (getPageTableBase() == 0)
	{
		DbgPrint("GetMemoryRegionData failed because pagebase == 0");
		return FALSE;
	}

	if (PEProcess==NULL)
	{
        if (!NT_SUCCESS(PsLookupProcessByProcessId((PVOID)(UINT_PTR)PID,&selectedprocess)))
		   return FALSE; //couldn't get the PID
	}

	*baseaddress=(UINT_PTR)mempointer & (UINT_PTR)(~0xfff);
	*memorysize=0;
	*regiontype=0;
	//switch context to the target process

	RtlZeroMemory(&apc_state,sizeof(apc_state));

	__try
	{
		KeAttachProcess((PEPROCESS)selectedprocess);
		__try
		{
			CurrentAddress=FindFirstDifferentAddress(*baseaddress, regiontype);
			*memorysize = CurrentAddress-*baseaddress;			
		}
		__finally
		{
			KeDetachProcess();
			if (PEProcess==NULL) //no valid peprocess was given so I made a reference, so lets also dereference
				ObDereferenceObject(selectedprocess);
		}
	}
	__except(1)
	{
		DbgPrint("Exception in GetMemoryRegionData\n");
		DbgPrint("mempointer=%p",mempointer);
	}

	return 0; 
}