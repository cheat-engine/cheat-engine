#pragma warning( disable: 4103)

#include "ntifs.h"
#include <windef.h>
#ifdef CETC
#include "tdiwrapper.h"
#include "kfiles.h"
#endif
#include "memscan.h"
#include "DBKFunc.h"

#include "vmxhelper.h"
/*#include "deepkernel.h"
*/


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

	    

		return TRUE; //for now untill I ave figure out the win 4 paging scheme
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
		UINT_PTR temp=(UINT_PTR)Address;
						
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


				RtlCopyMemory(target, source, Size);
				   
				ntStatus = STATUS_SUCCESS;	

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
		UINT_PTR temp=(UINT_PTR)Address;

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

				RtlCopyMemory(target,source,Size);

				ntStatus = STATUS_SUCCESS;	

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

NTSTATUS ReadPhysicalMemory(char *startaddress, UINT_PTR bytestoread, void *output)
{
	HANDLE			physmem;
	UNICODE_STRING	physmemString;
	OBJECT_ATTRIBUTES attributes;
	WCHAR			physmemName[] = L"\\device\\physicalmemory";
	UCHAR*			memoryview;
	NTSTATUS		ntStatus = STATUS_UNSUCCESSFUL;

	DbgPrint("ReadPhysicalMemory(%p, %d, %p)", startaddress, bytestoread, output);

	__try
	{
		RtlInitUnicodeString( &physmemString, physmemName );	

		InitializeObjectAttributes( &attributes, &physmemString, OBJ_CASE_INSENSITIVE, NULL, NULL );	
		ntStatus=ZwOpenSection( &physmem, SECTION_ALL_ACCESS, &attributes );
		if (ntStatus==STATUS_SUCCESS)
		{
			//hey look, it didn't kill it


			UINT_PTR length;
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

			if (ntStatus==STATUS_SUCCESS)
			{
				offset=(UINT_PTR)(startaddress)-(UINT_PTR)viewBase.QuadPart;
				RtlCopyMemory(output,&memoryview[offset],toread);

				ZwUnmapViewOfSection( NtCurrentProcess(), memoryview);
			}
			else
			{
				DbgPrint("ReadPhysicalMemory:ntStatus=%x", ntStatus); 
			}

			ZwClose(physmem);
		};

	}
	__except(1)
	{
		DbgPrint("Error while reading physical memory\n");
	}

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

UINT_PTR KnownPageTableBase = 0;
UINT_PTR getPageTableBase()
{
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
			r = MmGetVirtualForPhysical(a);

			KnownPageTableBase = ((UINT_PTR)r) & 0xFFFFFF8000000000ULL;

		
			/*
			//0x00400000 should be readable by ce's design.			
			struct PTEStruct64 PML4Table[512];
			PHYSICAL_ADDRESS r400000;

			r400000=MmGetPhysicalAddress((PVOID)0x00400000);
			if (r400000.QuadPart == 0)
			{
				DbgPrint("CE is not loaded at 0x00400000");
				return 0;
			}

			
			DbgPrint("Searching for pagetable base");
			DbgPrint("PTEStruct64 = %d bytes", sizeof(PML4Table));

			if (ReadPhysicalMemory((char *)(getCR3() & 0xFFFFFFFFFFFFF000ULL), sizeof(PML4Table), &PML4Table)==STATUS_SUCCESS)
			{
				int i;
				for (i = 511; i >0; i++)
				{
					//Each entry describes a range of 0x0000008000000000
					//0   =0000000000000000->0000007fffffffff
					//1   =0000008000000000->000000ffffffffff
					//255 =00007F8000000000->ffff800000000000  (sign extend)
					//511 =FFFFFF8000000000->FFFFFFFFFFFFFFFF  (sign extend)
					UINT_PTR address = SignExtend(i * 0x0000008000000000ULL);
					
					if (MmIsAddressValid((PVOID)address))
					{
						


						PMDL mdl = IoAllocateMdl((PVOID)address, 4096, FALSE, FALSE, NULL);
						DbgPrint("%p is valid", (PVOID)address);

						if (mdl)
						{
							__try
							{
								MmProbeAndLockPages(mdl, KernelMode, IoReadAccess);

								//do stuff, profit

								//Ok, on hold, as MmGetVirtualForPhysical gives me exactly what I want

								MmUnlockPages(mdl);
							}
							__except (1)
							{
								DbgPrint("%d was wrong", i);
							}
							IoFreeMdl(mdl);
						}
					}
				}

			}
			else
				DbgPrint("Failure reading the PML4 table");
				*/

		}
		else
			KnownPageTableBase=PAGETABLEBASE;
	}	

	return KnownPageTableBase;
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

				
				(UINT_PTR)PPTE = ((currentAddress & 0xFFFFFFFFFFFFULL) >> 12) *PTESize + pagebase;
				(UINT_PTR)PPDE = ((((UINT_PTR)PPTE) & 0xFFFFFFFFFFFFULL) >> 12) *PTESize + pagebase;
				(UINT_PTR)PPDPE = ((((UINT_PTR)PPDE) & 0xFFFFFFFFFFFFULL) >> 12) *PTESize + pagebase;
				(UINT_PTR)PPML4E = ((((UINT_PTR)PPDPE) & 0xFFFFFFFFFFFFULL) >> 12) *PTESize + pagebase;
				if (PTESize == 8)
					(UINT_PTR)PPDPE = ((((UINT_PTR)PPDE) & 0xFFFFFFFFFFFFULL) >> 12) *PTESize + pagebase;
				else
					(UINT_PTR)PPDPE = 0;

#ifdef AMD64
				(UINT_PTR)PPML4E = ((((UINT_PTR)PPDPE) & 0xFFFFFFFFFFFFULL) >> 12) *PTESize + pagebase;
#else
				(UINT_PTR)PPML4E = 0;
#endif

	

				if ((PPML4E) && (PPML4E->P == 0))
				{
					currentAddress &= 0xffffff8000000000ULL;
					currentAddress += 0x8000000000ULL;	
					continue;
				}

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
			e = ExAllocatePoolWithTag(PagedPool, sizeof(PENTRY), 0);
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

BOOLEAN GetMemoryRegionData(DWORD PID,PEPROCESS PEProcess, PVOID mempointer,ULONG *regiontype, UINT_PTR *memorysize,UINT_PTR *baseaddress)
{
#ifdef AMD64
	UINT_PTR pagebase = getPageTableBase();
#else
	UINT_PTR pagebase = PAGETABLEBASE;
#endif



	UINT_PTR StartAddress;
	KAPC_STATE apc_state;
	NTSTATUS ntStatus=STATUS_SUCCESS;
	struct PTEStruct *PPTE,*PPDE, *PPDPE, * PPML4E;
	PEPROCESS selectedprocess=PEProcess;
	BOOL ShowResult=0;

	if (pagebase == 0)
	{
		DbgPrint("GetMemoryRegionData failed because pagebase == 0");
		return FALSE;
	}

	if ((UINT_PTR)mempointer==(UINT_PTR)0x12000)
		ShowResult=1;

	if (PEProcess==NULL)
	{
		//DbgPrint("GetMemoryRegionData:Getting PEPROCESS\n");
        if (!NT_SUCCESS(PsLookupProcessByProcessId((PVOID)(UINT_PTR)PID,&selectedprocess)))
		   return FALSE; //couldn't get the PID

		//DbgPrint("Retrieved peprocess");  
	}

	StartAddress=(UINT_PTR)mempointer;



	*baseaddress=StartAddress & (UINT_PTR)(~0xfff);


	*memorysize=0;
	*regiontype=0;
	//switch context to the target process

	RtlZeroMemory(&apc_state,sizeof(apc_state));

	__try
	{
		KeAttachProcess((PEPROCESS)selectedprocess);
		__try
		{
			//do my stuff here

			//address -> strip off signed extended bit  , shift left by 12 and increase by 0xfffff68000000000ULL
			(UINT_PTR)PPTE=((*baseaddress & 0xFFFFFFFFFFFFULL) >> 12) *PTESize + pagebase;

			while ((UINT_PTR)PPTE<MAX_PTE_POS)
			{
				(UINT_PTR)PPDE=((((UINT_PTR)PPTE) & 0xFFFFFFFFFFFFULL) >> 12) *PTESize + pagebase;
				if (PTESize==8)
					(UINT_PTR)PPDPE=((((UINT_PTR)PPDE) & 0xFFFFFFFFFFFFULL) >> 12) *PTESize + pagebase; //pagedir pointer entry
				else
					(UINT_PTR)PPDPE=0; 

#ifdef AMD64
				(UINT_PTR)PPML4E=((((UINT_PTR)PPDPE)& 0xFFFFFFFFFFFFULL) >> 12) *PTESize + pagebase; //pagedir pointer entry
#else
				(UINT_PTR)PPML4E=0;
#endif
				

#ifdef AMD64
				if ((PPML4E==0) || (PPML4E->P))
				{
					//DbgPrint("PML4E=valid: %p\n", *(UINT_PTR *)PPML4E);
					//return 0;
#endif
					//PML4E is valid or not needed
					if ((PPDPE==0) || (PPDPE->P))
					{
						//PPDPE is valid or not needed
						if (PPDE->P)
						{
							//PDE is valid
							if (PPDE->PS==1)
							{
								//2/4mb page
								if (*regiontype==0) //first time init
								{
									
									

									*baseaddress=SignExtend(((((UINT_PTR)PPDE)-pagebase) / PTESize) << 12); //PPDE->PPTE
									*baseaddress=SignExtend((((*baseaddress)-pagebase) / PTESize) << 12); //PPTE->address
											
									*memorysize=PAGE_SIZE_LARGE-((UINT_PTR)mempointer-*baseaddress); //size is relative to the start of the mempointer
									if (PPDE->RW==1)
										*regiontype=PAGE_EXECUTE_READWRITE;
									else
										*regiontype=PAGE_EXECUTE_READ;
								}
								else
								{
									//make sure the protection is the same, if not, exit
									if ((PPDE->RW==1) && (*regiontype!=PAGE_EXECUTE_READWRITE))
										return 0;


									if ((PPDE->RW==0) && (*regiontype!=PAGE_EXECUTE_READ))
										return 0;

									//still here so the same
									*memorysize+=4096;
								}

								//still here
								(UINT_PTR)PPDE+=PTESize; //next pagedir entry							
								(UINT_PTR)PPTE=SignExtend(((((UINT_PTR)PPDE)-pagebase) / PTESize) << 12); //set the pagetable to the start of the pagedir table
								continue;

							}
							else
							{
								//has a page table
								if (PPTE->P)
								{
									

									//valid pte
									if (*regiontype==0) //first time init
									{										
										//find the virtual address of this pagetable entry and use it as base address														
										*baseaddress=SignExtend(((((UINT_PTR)PPTE)-pagebase) / PTESize) << 12); //PPTE->address			

										*memorysize=4096-((UINT_PTR)mempointer-*baseaddress); //size is relative to the start of the mempointer

										if (PPTE->RW==1)
											*regiontype=PAGE_EXECUTE_READWRITE;
										else
											*regiontype=PAGE_EXECUTE_READ;

										
									}
									else
									{
										//make sure the protection is the same, if not, exit
										if ((PPTE->RW==1) && (*regiontype!=PAGE_EXECUTE_READWRITE))
											return 0;


										if ((PPTE->RW==0) && (*regiontype!=PAGE_EXECUTE_READ))
											return 0;

										//still here so the same
										*memorysize+=4096;

										//DbgPrint("Extending\n");
									}

									//still here
									(UINT_PTR)PPTE+=PTESize; //next pagetable entry	
									continue;

								}
								else
								{

									if (ShowResult)
										DbgPrint("PTE is not paged in\n");

									//PTE is not paged in
									if (*regiontype==0) //first occurance
									{
										//find the virtual address of this pagetable entry and use it as base address														
										*baseaddress=SignExtend(((((UINT_PTR)PPTE)-pagebase) / PTESize) << 12); //PPTE->address
		
										*memorysize=4096-((UINT_PTR)mempointer-*baseaddress); //size is relative to the start of the mempointer
										*regiontype = PAGE_NOACCESS;
									}
									else 
									if (*regiontype == PAGE_NOACCESS)
									{
										*memorysize=*memorysize+4096; 
									}
									else
										return 0; //new region

								
									//still here
									(UINT_PTR)PPTE+=PTESize; //next pagetable entry	
									continue;
								}


							}
						}
						else
						{
							//PDE is not paged in
							if (ShowResult)
								DbgPrint("PDE is not paged in\n");

							if (*regiontype==0) //first occurance
							{
								//find the virtual address of this pagedir entry and use it as base address														
								*baseaddress=SignExtend(((((UINT_PTR)PPDE)-pagebase) / PTESize) << 12); //PPDE->PPTE
								*baseaddress=SignExtend((((*baseaddress)-pagebase) / PTESize) << 12); //PPTE->address
	

								*memorysize=PAGE_SIZE_LARGE-((UINT_PTR)mempointer-*baseaddress); //size is relative to the start of the mempointer
								*regiontype = PAGE_NOACCESS;

							}
							else
							if (*regiontype == PAGE_NOACCESS)
							{
								*memorysize=*memorysize+PAGE_SIZE_LARGE; //increase with 2 or 4 MB
							}
							else
								return 0; //new section

							//still here
							(UINT_PTR)PPDE+=PTESize; //next pagedir entry							
							(UINT_PTR)PPTE=SignExtend(((((UINT_PTR)PPDE)-pagebase) / PTESize) << 12); //set the pagetable to the start of the pagedir table
							continue;

						}
					}
					else
					{
						//PPDPE is invalid
						if (ShowResult)
							DbgPrint("PDPE is not paged in\n");

						if (*regiontype==0) //first occurance
						{
							//find the virtual address of this pagedir pointer entry and use it as base address							
							*baseaddress=SignExtend(((((UINT_PTR)PPDPE)-pagebase) / PTESize) << 12); //>PPDPE->PPDE
							*baseaddress=SignExtend((((*baseaddress)-pagebase) / PTESize) << 12); //PPDE->PPTE
							*baseaddress=SignExtend((((*baseaddress)-pagebase) / PTESize) << 12); //PPTE->address

							*memorysize=0x40000000-((UINT_PTR)mempointer-*baseaddress); //size is relative to the start of the mempointer
							*regiontype = PAGE_NOACCESS;
						}
						else if (*regiontype==PAGE_NOACCESS)
						{
							*memorysize=*memorysize+0x40000000; //increase with 1GB 
						}
						else
							return 0; //new section

						//still here
						(UINT_PTR)PPDPE+=PTESize; //next pagedirptr entry
						(UINT_PTR)PPDE=SignExtend(((((UINT_PTR)PPDPE)-pagebase) / PTESize) << 12); //set the pagedir entry to the start of the pagedirptr table
						(UINT_PTR)PPTE=SignExtend(((((UINT_PTR)PPDE)-pagebase) / PTESize) << 12); //set the pagetable to the start of the pagedir table
						continue;
					}

#ifdef AMD64
				}

				//no need to compile this in for the 32-bit version
				else
				{
					if (ShowResult)
					    DbgPrint("PML4E is not paged in\n");
					
					//DbgPrint("PML4E=invalid: %p\n", *(UINT_PTR *)PPML4E);
					//return 0;

					//PML4 is invalid (but not 0)
					if (*regiontype==0) //first occurance
					{
						//find the virtual address of this pml4 entry and use it as base address
						*baseaddress=SignExtend(((((UINT_PTR)PPML4E)-pagebase) / PTESize) << 12); //PML4E->PDPTE
						*baseaddress=SignExtend((((*baseaddress)-pagebase) / PTESize) << 12); //PDPTE->PPDE					
						*baseaddress=SignExtend((((*baseaddress)-pagebase) / PTESize) << 12); //PPDE->PPTE
						*baseaddress=SignExtend((((*baseaddress)-pagebase) / PTESize) << 12); //PPTE->address

						*memorysize=0x8000000000ULL-((UINT_PTR)mempointer-*baseaddress); //size is relative to the start of the mempointer

						*regiontype = PAGE_NOACCESS;
					}
					else
					if (*regiontype == PAGE_NOACCESS)
						*memorysize=*memorysize+0x8000000000ULL; //increase with 512GB 
					else
						return 0; //New regiontype reached (inaccessible memory)

					//still here so continue.
					(UINT_PTR)PPML4E+=PTESize;
					(UINT_PTR)PPDPE=SignExtend(((((UINT_PTR)PPML4E)-pagebase) / PTESize) << 12); //set the pagedir ptr entry to the start of the pml4 table
					(UINT_PTR)PPDE=SignExtend(((((UINT_PTR)PPDPE)-pagebase) / PTESize) << 12); //set the pagedir entry to the start of the pagedirptr table
					(UINT_PTR)PPTE=SignExtend(((((UINT_PTR)PPDE)-pagebase) / PTESize) << 12); //set the pagetable to the start of the pagedir table

					continue;
				}
#endif


			}

			ntStatus=STATUS_SUCCESS;
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
		DbgPrint("PPML4E=%p\n", PPML4E);
		DbgPrint("PPDPE=%p\n", PPDPE);
		DbgPrint("PPDE=%p\n", PPDE);
		DbgPrint("PPTE=%p\n", PPTE);
		
		ntStatus=STATUS_UNSUCCESSFUL;
	}
	



	return 0; 

}

#ifdef CETC
//ooh woot, some real public CETC code.....

char ScanresultBuffer[4096];
int ScanresultBufferPos=0;

VOID FlushScanresultBuffer(void)
{
	Send(ScanresultBuffer,ScanresultBufferPos);
	ScanresultBufferPos=0;    
}

VOID ScanResult(ULONG Address,int Size)
{
	char *output;

	if (Size>=4096)
		return; //error, there's no fucking reason to do a scan like this....
	
	if ((ScanresultBufferPos+Size+6)>=4096)
		FlushScanresultBuffer();


	ScanresultBuffer[ScanresultBufferPos]=SC_ScanResult; //address:dword; valuesize:byte; value:array of bytes
	*(PULONG)(&ScanresultBuffer[ScanresultBufferPos+1])=Address;

	__try
	{
		ScanresultBuffer[ScanresultBufferPos+5]=(BYTE)Size;
		RtlCopyMemory(&ScanresultBuffer[ScanresultBufferPos+6],(PVOID)Address,Size);
	}
	__except(1)
	{
		//(unreadable)
	}
	//increase counter with 6+size
	ScanresultBufferPos+=6+Size;
}

VOID ScanResultCount(INT64 errorcode)
{
	char output[9];
	
	output[0]=SC_ScanResultCount;
	*(PINT64)(&output[1])=errorcode;
	Send(output,9);
}

VOID UpdateProgressBar(DWORD max,DWORD pos)
{
	//in case of network scan send , else set global var and let the usermode app poll
	char output[9];
	DWORD a=max / 2;//this way even if you go over 80000000 (signed negative) it wont go wrong
	DWORD b=pos / 2;

	output[0]=SC_UpdateProgressbar;
	*(PDWORD)(&output[1])=a;
	*(PDWORD)(&output[5])=b;
	Send(output,9);
}

VOID FixFPUcrash(void)
{
	__try
	{		
		
		DWORD New8087CW=0x133f; //fuck floating point exceptions
		PDWORD PtoNew8087CW=&New8087CW;
		DbgPrint("Going to set the ControlWord of the FP-Coprocessor\n");
		_asm
		{
			FNCLEX
			FLDCW   [PtoNew8087CW]
		}
	}
	__except(1)
	{
		//bleh

	}
}

VOID UnknownInitialValueScan(IN PVOID StartContext)
{

	__try
	{
		__try
		{
			//this is a unknown initial value scan.
			


		}
		__finally
		{
			CurrentScan.scanning=FALSE;
			CurrentScan.ThreadActive=FALSE;
			PsTerminateSystemThread(STATUS_SUCCESS);
		}
	}
	__except(1)
	{
		//nothing, just go on...

	}
}




VOID FirstScanThread(IN PVOID StartContext)
{
	KAPC_STATE apc_state;
	BYTE bytevalue;
	WORD wordvalue;
	DWORD dwordvalue;
	float floatvalue;  //yes, float. I know... don't use them. but this is intended for winxp, and even runs in it's own thread
	double doublevalue;
	INT64 int64value;
	char* stringvalue;

	BYTE   *ValueList0=NULL;
	WORD   *ValueList1=NULL;
	DWORD  *ValueList2=NULL;
	float  *ValueList3=NULL;
	double *ValueList4=NULL;
	INT64  *ValueList6=NULL;

	DWORD  *AddressList=NULL;
	int AddressListSize=0;
	int found=0,foundsaved=0;


	BOOLEAN error=FALSE;
	BYTE errorcode=-1; //none set
	
	
	PVOID mempointer;
	MEMREGION *memoryregion=NULL;	

	int memoryregionsize=0;
	int memoryregionentries=0;
	int i;

	IO_STATUS_BLOCK iosb;
	NTSTATUS ntStatus;
	BOOLEAN FastScan=((CurrentScan.ScanOptions & SO_FASTSCAN)!=0);



	//this thread scans the memory till it is done scanning or till Scanning equals FALSE
	//there will be a periodic scan for the state of Scanning
	DbgPrint("This is the FirstScanThread\n");	
	CurrentScan.ThreadActive=TRUE;

	FixFPUcrash();

	__try
	{
		ntStatus=CETC_CreateFile(&addressfile,L"\\DosDevices\\C:\\ADDRESS.DAT");
		if (!NT_SUCCESS(ntStatus)) return;

		ntStatus=CETC_CreateFile(&valuefile,L"\\DosDevices\\C:\\VALUES.DAT");
		if (!NT_SUCCESS(ntStatus)) return;
		
		if (!CurrentScan.scanning) return;

	    
		errorcode=SE_IncorrectType;
		DbgPrint("Checking memory type\nVartype=%d\n",CurrentScan.Vartype);	

		//Allocate memory for the addresslist
		AddressList=ExAllocatePoolWithTag(PagedPool,MemscanOptions.buffersize/4,0);
		if (AddressList==NULL)
		{
			DbgPrint("Failed to allocate memory for the AddressList\n");				
			return;
		}
		else
			AddressListSize=MemscanOptions.buffersize/4;

#define InitializeList(nr,varsize,value,type) { \
					DbgPrint("case "#nr"\n");\
					if (CurrentScan.scanvaluelength==varsize)\
					{\
						value=*(##type)(CurrentScan.scanvalue);\
						if (CurrentScan.Scantype==ST_Exact_value)\
						{\
						ValueList##nr=ExAllocatePoolWithTag(PagedPool,AddressListSize*varsize,0);\
						if (ValueList##nr==NULL) \
							{\
								DbgPrint("Failed to allocate memory for ValueList"#nr"\n");\
								return;\
							}\
						}\
					}\
					else\
						error=TRUE;\
}

		//initialize the list to put the values in	
		switch (CurrentScan.Vartype)
		{
			case 0:
				{
					//initialize the list for the byte value
					InitializeList(0,1,bytevalue,PBYTE);
					break;
				}

			case 1:
				{
					InitializeList(1,2,wordvalue,PWORD);
					break;
				}

			case 2:
				{
					InitializeList(2,4,dwordvalue,PDWORD);
					break;
				}

			case 3:
				{
					InitializeList(3,4,floatvalue,PFLOAT);
					break;
				}

			case 4:
				{
					InitializeList(4,8,doublevalue,double*);
					break;
				}


			case 6:
				{
					InitializeList(6,8,int64value,PINT64);
					break;
				}

			case 7:
				{
					//string
					DbgPrint("case 7\n");
					if (CurrentScan.scanvaluelength>1)
					{
						stringvalue=CurrentScan.scanvalue; //just give it the pointer
						CETC_Write(valuefile,stringvalue,CurrentScan.scanvaluelength,&iosb);
					}
					else
						error=TRUE;

					break;

				}
			default:
				{
					//not implemented yet
					DbgPrint("case default\n");
					errorcode=SE_NotSupported;
					error=TRUE;

				}

		}

		if (error)
		{			
			ScanResultCount(errorcode); //send a error count
			return;
		}


		DbgPrint("Trying to attach to process\n");
		
		KeAttachProcess((PEPROCESS)CurrentScan.process);
		__try
		{
			DWORD regiontype=0;
			DWORD memorysize=0x1234;		
			DWORD TotalSize=0;
			DWORD BytesRead=0;			

			DbgPrint("Inside the context of the process I hope\n");
			
			//first time go through to find out the memorysize of the memory to scan
			//by default allocate space for 512 entries (one page)
			memoryregion=ExAllocatePoolWithTag(PagedPool,512*sizeof(MEMREGION),0);
			if (memoryregion==NULL)
			{
				DbgPrint("Failed to allocate memory for the memoryregions\n");				
				return;
			}
			else
				memoryregionsize=512;  //512 entries
			
			__try
			{	
				DWORD baseaddress;
					
				mempointer=(PVOID)CurrentScan.Start;
				while (((ULONG)mempointer<CurrentScan.Stop) && (GetMemoryRegionData(0,CurrentScan.process,mempointer,&regiontype,&memorysize,&baseaddress)))
				{
					BOOLEAN ok=FALSE;
					if (regiontype != PAGE_NOACCESS)
					{
						if ((CurrentScan.ScanOptions & SO_READONLY)==0)
						{
							/*
							skip readonly
							only allow:
							PAGE_READWRITE
							PAGE_WRITECOPY
							PAGE_EXECUTE_READWRITE
							PAGE_EXECUTE_WRITECOPY
							*/
							if  ((regiontype == PAGE_READWRITE) ||
								(regiontype == PAGE_WRITECOPY) ||
								(regiontype == PAGE_EXECUTE_READWRITE) ||
								(regiontype == PAGE_EXECUTE_WRITECOPY))
							{
								ok=TRUE;
								
							}
		
						}
						else
						{
							ok=TRUE;
						}
		
					}

					if (ok)
					{												
						TotalSize+=memorysize;
						DbgPrint("%d:memorysize=%x   -  new TotalSize=%x\n",memoryregionentries,memorysize,TotalSize);

						if (memoryregionentries>=memoryregionsize) //should never be able to go above, but lets check it anyhow...
						{
							MEMREGION *newmemoryregion;
							//allocate another 512 entries
							DbgPrint("Need more regions\n");
							newmemoryregion=ExAllocatePoolWithTag(PagedPool,(memoryregionsize+512)*sizeof(MEMREGION),0);
							if (newmemoryregion!=NULL)
							{
								//copy the old memory to the new one and free the old one
								RtlCopyMemory(newmemoryregion,memoryregion,memoryregionsize*sizeof(MEMREGION));
								ExFreePool(memoryregion);
								memoryregion=newmemoryregion;
								memoryregionsize+=512;
							}
							else
							{
								ExFreePool(memoryregion);
								memoryregion=NULL;
								DbgPrint("Failed to re-allocate memory for the memoryregions\n");
								PsTerminateSystemThread(STATUS_SUCCESS);
								return;
							}
									

						};

						memoryregion[memoryregionentries].BaseAddress=baseaddress;
						memoryregion[memoryregionentries].Size=memorysize;
						memoryregionentries++;	

					}
		
		
					(ULONG)mempointer+=memorysize;
				}

				if (memoryregionentries>0)
				{
					DbgPrint("Adjusting regions\n");
					DbgPrint("before:TotalSize=%x\nmemoryregion[0].BaseAddress=%x\n",TotalSize,memoryregion[0].BaseAddress);
					//adjust start and stop
					if (memoryregion[0].BaseAddress<CurrentScan.Start)
					{
						memoryregion[0].Size-=CurrentScan.Start-memoryregion[0].BaseAddress;
						TotalSize-=CurrentScan.Start-memoryregion[0].BaseAddress;
						memoryregion[0].BaseAddress=CurrentScan.Start;
					}

					if (memoryregion[memoryregionentries-1].BaseAddress+memoryregion[memoryregionentries-1].Size>CurrentScan.Stop)
					{					
						TotalSize-=(memoryregion[memoryregionentries-1].BaseAddress+memoryregion[memoryregionentries-1].Size)-CurrentScan.Stop;
						memoryregion[memoryregionentries-1].Size-=(memoryregion[memoryregionentries-1].BaseAddress+memoryregion[memoryregionentries-1].Size)-CurrentScan.Stop;
					}

					DbgPrint("After:TotalSize=%x\nMemoryregion[0].BaseAddress=%x\n",TotalSize,memoryregion[0].BaseAddress);

				}
	
				/*
				//this works:
				mempointer=(PVOID)CurrentScan.Start;
				if (((PBYTE)mempointer)[0]==12)
					return;
				*/

				DbgPrint("Checking the TotalSize:%x\n",TotalSize);

				if ((TotalSize==0) || (TotalSize>0xf0000000))
				{
					//I don't accept 0 memory
					//and also not if there is more than 0xf0000000 bytes readable memory (more likely a signed underflow or other bug)
					DbgPrint("Error with memory regions\n");		
					ScanResultCount(SE_NoMemoryFound);
					return;
				}

				
	
				//now start the scan
				DbgPrint("Initializing the progressbar. Totalsize=%d (%x)\n",TotalSize,TotalSize);
				UpdateProgressBar(TotalSize,0); //max size of progressbar and the current pos (0) (both devided by 2)
				BytesRead=0;

				if (CurrentScan.Scantype==ST_Exact_value)
					DbgPrint("Scantype=Exact value\n");

				for (i=0; (i<memoryregionentries) && (CurrentScan.scanning);i++)
				{
					ULONG StopAddress;
					DbgPrint("Region %d\n",i);
					
					mempointer=(PVOID)(memoryregion[i].BaseAddress);
					StopAddress=memoryregion[i].BaseAddress+memoryregion[i].Size;

					//adjust the stopaddress so you dont scan unreadable memory
					StopAddress-=CurrentScan.scanvaluelength-1;

					UpdateProgressBar(TotalSize,BytesRead);


#define MemCheckAndHandle(nr,type,varsize,value) if ( /*it's faster to do this in a seperate code segment for each type, but lets try this first (less code)*/ \
	((CurrentScan.Scantype==ST_Exact_value) && (*(##type)mempointer==value)) ||\
	((CurrentScan.Scantype==ST_SmallerThan) && (*(##type)mempointer<value)) ||\
	((CurrentScan.Scantype==ST_BiggerThan) && (*(##type)mempointer>value))\
	)\
	{\
		/*found one*/ \
		DbgPrint("Found one!!!\n");\
		AddressList[found]=(ULONG)mempointer;\
		ValueList##nr[found]=*(##type)mempointer;\
		found++;\
		if (found==AddressListSize)\
		{\
			DbgPrint("Writing tempfile\n");\
			\
			/*write the addresses and values to disk*/ \
			CETC_Write(addressfile,AddressList,found*4,&iosb);\
			CETC_Write(valuefile,ValueList##nr,found*varsize,&iosb);\
\
			foundsaved+=found;\
			found=0;\
		}\
	}

				
					while ((ULONG)mempointer<StopAddress)
					{
						__try
						{
							switch (CurrentScan.Vartype)
							{
							case 0:
								{
									MemCheckAndHandle(0,PBYTE,1,bytevalue);
									((ULONG)mempointer)++;									
									break;
								}


							case 1: //2 byte
								{
									MemCheckAndHandle(1,PWORD,2,wordvalue);
									
									if (FastScan)
										((ULONG)mempointer)+=2;
									else
										((ULONG)mempointer)++;
									break;
								}

							case 2: //4 byte
								{
									MemCheckAndHandle(2,PDWORD,4,dwordvalue);
									
									if (FastScan)
										((ULONG)mempointer)+=4;
									else
										((ULONG)mempointer)++;
									break;
								}

							case 3: //float
								{
									MemCheckAndHandle(3,float*,4,floatvalue)
									
									if (FastScan)
										((ULONG)mempointer)+=4;
									else
										((ULONG)mempointer)++;
									break;
								}

							case 4: //double
								{
									MemCheckAndHandle(4,double*,8,doublevalue)
									
									if (FastScan)
										((ULONG)mempointer)+=8;
									else
										((ULONG)mempointer)++;
									break;
								}

							case 6: //int64
								{
									MemCheckAndHandle(6,PINT64,8,int64value)

									
									if (FastScan)
										((ULONG)mempointer)+=8;
									else
										((ULONG)mempointer)++;
									break;
								}


							case 7:
								{
									if (RtlCompareMemory(mempointer,stringvalue,CurrentScan.scanvaluelength)==CurrentScan.scanvaluelength)
									{
										found++;
										if (found==AddressListSize)
										{
											DbgPrint("Writing tempfile\n");
											
											/*write the addresses to disk*/ 
											CETC_Write(addressfile,AddressList,found*4,&iosb);																
											foundsaved+=found;
											found=0;
										}

										

									}


									break;
								}

							default:
								((ULONG)mempointer)++;

							}

						}
						__except(1)
						{
							//unreadable
							DbgPrint("unreadable %p\n",mempointer);
                            ((ULONG)mempointer)+=PAGE_SIZE; //try the next page
							//align on the base
							(ULONG)mempointer=(ULONG)mempointer/0x1000*0x1000; //shouldn't be neccesary, but lets do it anyhow
						}                       
						
					} //while


					BytesRead+=memoryregion[i].Size;

				}
				
				//now save the results
				if (found>0)
				{
					DbgPrint("Writing tempfile\n");

					ntStatus=CETC_Write(addressfile,AddressList,found*4,&iosb);
					switch (CurrentScan.Vartype)
					{
					case 0:
						{
							//write the addresses and values to disk
							ntStatus=CETC_Write(valuefile,ValueList0,found,&iosb);
							break;
						}

					case 1:
						{
							//write the addresses and values to disk
							ntStatus=CETC_Write(valuefile,ValueList1,found*2,&iosb);
							break;
						}

					case 2:
						{
							//write the addresses and values to disk
							ntStatus=CETC_Write(valuefile,ValueList2,found*4,&iosb);
							break;
						}

					case 3:
						{
							//write the addresses and values to disk
							ntStatus=CETC_Write(valuefile,ValueList3,found*4,&iosb);
							break;
						}

					case 4:
						{
							//write the addresses and values to disk
							ntStatus=CETC_Write(valuefile,ValueList4,found*8,&iosb);
							break;
						}

					case 6:
						{
							//write the addresses and values to disk
							ntStatus=CETC_Write(valuefile,ValueList6,found*8,&iosb);
							break;
						}


					}

					foundsaved+=found;
					found=0;
				}

				//and tell the client the results
				
				DbgPrint("found=%d and foundsaved=%d\n",found,foundsaved);
			    if ((found+foundsaved)<=MemscanOptions.max)
				{			
					
					//read the addresses and values and tell them to the client
					//first reposition the file pointer to the start
					IO_STATUS_BLOCK isb;
					FILE_POSITION_INFORMATION fpi;
					int j,k;					

					fpi.CurrentByteOffset.QuadPart=0;
					DbgPrint("Resetting the file position to 0\n");
					ntStatus=ZwSetInformationFile(addressfile,&isb,&fpi,sizeof(FILE_POSITION_INFORMATION),FilePositionInformation);
					DbgPrint("ntStatus=%d",ntStatus);

					//read the AddressFile
					i=0;
					while ((i<MemscanOptions.max) && (i<foundsaved))
					{	
						

						DbgPrint("Reading addressfile\n");
						DbgPrint("(foundsaved<AddressListSize)? (foundsaved*4):(AddressListSize*4)=%d\n",(foundsaved<AddressListSize)? (foundsaved*4):(AddressListSize*4));
						ntStatus=ZwReadFile(addressfile,NULL,NULL,NULL,&isb,AddressList,(foundsaved<AddressListSize)? (foundsaved*4):(AddressListSize*4),NULL,NULL);

						DbgPrint("ntStatus=%x\n",ntStatus);
						DbgPrint("isb.Information=%d\n",isb.Information);

						if (isb.Information>4)
						{						
							j=(int)isb.Information/4;
							for (k=0; k<j; k++)
							{
								switch (CurrentScan.Vartype)
								{
								case 0://byte
										ScanResult(AddressList[k],1);
										break;
								case 1:
										ScanResult(AddressList[k],2);
										break;

								case 2:
								case 3:
										ScanResult(AddressList[k],4);
										break;

								case 4:
								case 6:
										ScanResult(AddressList[k],8);
										break;

								case 7:
										ScanResult(AddressList[k],CurrentScan.scanvaluelength);
										break;								

								}
								//AddressList[k]


							}

							i+=j;
						}
						else
						{
							DbgPrint("Failed to read AddressList\n");
							break;
						}
						
					}

					FlushScanresultBuffer();
                   

				}		
				ScanResultCount(foundsaved);	
			}
			__finally
			{

				
			}
	
	
	
		}
		__finally
		{	
			DbgPrint("Detaching\n");
			KeDetachProcess();
		}

		DbgPrint("returning\n");
	}
	__finally
	{
		//End of the thread.
		//Free all the used memory and close the files		
		DbgPrint("Terminating\n");	
		DbgPrint("Free memory\n");	
		if (memoryregion!=NULL) ExFreePool(memoryregion);
		if (AddressList!=NULL) ExFreePool(AddressList);
		if (ValueList0!=NULL) ExFreePool(ValueList0);
		if (ValueList1!=NULL) ExFreePool(ValueList1);
		if (ValueList2!=NULL) ExFreePool(ValueList2);
		if (ValueList3!=NULL) ExFreePool(ValueList3);
		if (ValueList4!=NULL) ExFreePool(ValueList4);
		if (ValueList6!=NULL) ExFreePool(ValueList6);

		DbgPrint("Close files\n");
		if (addressfile!=0) ZwClose(addressfile);
		if (valuefile!=0) ZwClose(valuefile);
		
        
		CurrentScan.scanning=FALSE;
		CurrentScan.ThreadActive=FALSE;
		PsTerminateSystemThread(STATUS_SUCCESS);
	}
	return;
}

BOOLEAN FirstScan(PEPROCESS ActivePEPROCESS, DWORD start,DWORD stop,BYTE vartype,BYTE scantype,BYTE scanvaluesize,char *scanvalue,BYTE ScanOptions)
{
	BOOLEAN status=FALSE;
	CurrentScan.process=ActivePEPROCESS;
	CurrentScan.Start=start;
	CurrentScan.Stop=stop;
	CurrentScan.Vartype=vartype;
	CurrentScan.Scantype=scantype;
	CurrentScan.ScanOptions=ScanOptions;

	if (CurrentScan.scanvalue!=NULL)
	{
		//a leftover from last scan (e.g thread crashed...)
		ExFreePool(CurrentScan.scanvalue);
		CurrentScan.scanvalue=NULL;
	}
	CurrentScan.scanvalue=ExAllocatePoolWithTag(PagedPool,scanvaluesize,0);
	if (CurrentScan.scanvalue==NULL)
		return FALSE; //error

	RtlCopyMemory(CurrentScan.scanvalue,scanvalue,scanvaluesize);
	CurrentScan.scanvaluelength=scanvaluesize;

	__try
	{
		OBJECT_ATTRIBUTES oaCreateThread;
		HANDLE th;
		CurrentScan.scanning=TRUE;

		//start the scan
		
		if (scantype==ST_Advanced_Scan)
		{
			//unknown initial value scan
			InitializeObjectAttributes(&oaCreateThread, NULL, OBJ_KERNEL_HANDLE, NULL, NULL);
			DbgPrint("Creating scanthread\n");
			if (NT_SUCCESS(PsCreateSystemThread(&th,0L,&oaCreateThread,NULL,NULL,UnknownInitialValueScan,NULL)))
			{
				DbgPrint("Created thread\n");
				ZwClose(th); //I dont want this handle (useless anyhow except for setting priority)
			}
			else
				DbgPrint("Failed to create thread\n");
		}
		else
		{
			//first scan for value
			InitializeObjectAttributes(&oaCreateThread, NULL, OBJ_KERNEL_HANDLE, NULL, NULL);
			DbgPrint("Creating scanthread\n");
			if (NT_SUCCESS(PsCreateSystemThread(&th,0L,&oaCreateThread,NULL,NULL,FirstScanThread,NULL)))
			{
				DbgPrint("Created thread\n");
				ZwClose(th); //I dont want this handle (useless anyhow except for setting priority)
			}
			else
				DbgPrint("Failed to create thread\n");
		}

		//and resume the command listener
	}
	__except(1)
	{
		DbgPrint("Error\n");
	}

	return status;
}
#endif