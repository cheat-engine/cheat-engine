#include "ntifs.h"
#include <windef.h>
#ifdef CETC
#include "tdiwrapper.h"
#include "kfiles.h"
#endif
#include "memscan.h"
#include "DBKFunc.h"
#include "vmxhelper.h"
#include "rootkit.h"


BOOLEAN IsAddressSafe(UINT_PTR StartAddress)
{


	//note: Add support for PAE enabled systems
	//return TRUE;
#ifdef AMD64
	return TRUE; //for now
#endif
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

}

ULONG getPEThread(ULONG threadid)  
{	
    //UINT_PTR *threadid;
	PETHREAD selectedthread;
	ULONG result=0;
	

	if (PsLookupThreadByThreadId((PVOID)threadid,&selectedthread)==STATUS_SUCCESS)
	{
		result=(ULONG)selectedthread;
		ObDereferenceObject(selectedthread);
	}

	return result;
}

BOOLEAN WriteProcessMemory(DWORD PID,PEPROCESS PEProcess,PVOID Address,DWORD Size, PVOID Buffer)
{
	PEPROCESS selectedprocess=PEProcess;
	KAPC_STATE apc_state;
	NTSTATUS ntStatus=STATUS_SUCCESS;

	if (selectedprocess==NULL)
	{
		DbgPrint("WriteProcessMemory:Getting PEPROCESS\n");
        if (!NT_SUCCESS(PsLookupProcessByProcessId((PVOID)PID,&selectedprocess)))
		   return FALSE; //couldn't get the PID

		DbgPrint("Retrieved peprocess");  
	}

	//selectedprocess now holds a valid peprocess value
	__try
	{
		unsigned int temp=(unsigned int)Address;
						
		RtlZeroMemory(&apc_state,sizeof(apc_state));					

    	KeAttachProcess((PEPROCESS)selectedprocess);				

        __try
        {
			char* target;
			char* source;
			unsigned int i;	

			DbgPrint("Checking safety of memory\n");

			if ((!IsAddressSafe((ULONG)Address)) || (!IsAddressSafe((ULONG)Address+Size-1)))
				return FALSE; //if the first or last byte of this region is not safe then exit; //I know I should also check the regions inbetween, but since my own dll doesn't request more than 512 bytes it wont overlap

    		//still here, then I gues it's safe to read. (But I can't be 100% sure though, it's still the users problem if he accesses memory that doesn't exist)

			DbgPrint("Copying memory to target\n");
			target=Address;
			source=Buffer;
			for (i=0; i<Size; i++)
			{
               target[i]=source[i];
			}

			ntStatus = STATUS_SUCCESS;							
		}
		__finally
		{
			KeDetachProcess();
		}
	}			
	__except(1)
	{
		DbgPrint("Error while writing\n");
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
	NTSTATUS ntStatus=STATUS_SUCCESS;

	if (PEProcess==NULL)
	{
		//DbgPrint("ReadProcessMemory:Getting PEPROCESS\n");
        if (!NT_SUCCESS(PsLookupProcessByProcessId((PVOID)PID,&selectedprocess)))
		   return FALSE; //couldn't get the PID

		//DbgPrint("Retrieved peprocess");  
	}

	//DbgPrint("a");

	//selectedprocess now holds a valid peprocess value
	__try
	{
		unsigned int temp=(unsigned int)Address;
		ULONG currentcr3;
		//DbgPrint("b");
		
		/*				
		RtlZeroMemory(&apc_state,sizeof(apc_state));					

		RtlZeroMemory(Buffer,Size);*/

		//DbgPrint("c");
		/*
		__asm
		{
			mov eax,cr3
			mov currentcr3,eax
		}*/
		//DbgPrint("d");
		//DbgPrint("%d: Before: PEProcess=%x ProcessID=%x CR3=%x (real=%x)\n",cpunr(), (ULONG)PsGetCurrentProcess(), PsGetCurrentProcessId(), currentcr3, vmx_getRealCR3());
    	KeAttachProcess((PEPROCESS)selectedprocess);

		/*
		//DbgPrint("e");
		__asm
		{
			mov eax,cr3
			mov currentcr3,eax
		}
		//DbgPrint("%d: After: PEProcess=%x ProcessID=%x CR3=%x (real=%x)\n",cpunr(), (ULONG)PsGetCurrentProcess(), PsGetCurrentProcessId(), currentcr3, vmx_getRealCR3());
*/

        __try
        {
			char* target;
			char* source;
			unsigned int i;	

			//DbgPrint("Checking safety of memory\n");

			if ((!IsAddressSafe((ULONG)Address)) || (!IsAddressSafe((ULONG)Address+Size-1)))
				return FALSE; //if the first or last byte of this region is not safe then exit;

    		//still here, then I gues it's safe to read. (But I can't be 100% sure though, it's still the users problem if he accesses memory that doesn't exist)

			//DbgPrint("Copying memory to target\n");
			target=Buffer;
			source=Address;
			RtlCopyMemory(target,source,Size);
			ntStatus = STATUS_SUCCESS;	
		}
		__finally
		{
		/*	unsigned long long a;
			a=getTSC()+1000000000;
			//DbgPrint("a=%d getTSC()=%d",a,getTSC());
			while (getTSC() < a)
			{
				__asm
				{
					pushad
					pause
					cpuid					
					popad
				}

			}

			
			__asm
			{
				mov eax,cr3
				mov currentcr3,eax
			}*/
			//DbgPrint("%d: Before going back: PEProcess=%x ProcessID=%x CR3=%x (real=%x)\n",cpunr(), (ULONG)PsGetCurrentProcess(), PsGetCurrentProcessId(), currentcr3, vmx_getRealCR3());

			KeDetachProcess();
		}
	}			
	__except(1)
	{
		DbgPrint("Error while reading\n");
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

	__try
	{
		RtlInitUnicodeString( &physmemString, physmemName );	

		InitializeObjectAttributes( &attributes, &physmemString, OBJ_CASE_INSENSITIVE, NULL, NULL );	
		ntStatus=ZwOpenSection( &physmem, SECTION_MAP_READ, &attributes );
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
			};

			ZwClose(physmem);
		};

	}
	__except(1)
	{
		DbgPrint("Error while reading physical memory\n");
	}

	return ntStatus;
}

BOOLEAN GetMemoryRegionData(DWORD PID,PEPROCESS PEProcess, PVOID mempointer,ULONG *regiontype, DWORD *memorysize,DWORD *baseaddress)
{
	UINT_PTR StartAddress;
	KAPC_STATE apc_state;
	NTSTATUS ntStatus=STATUS_SUCCESS;
	struct PTEStruct *PPTE,*PPDE;
	PEPROCESS selectedprocess=PEProcess;

	if (PEProcess==NULL)
	{
		DbgPrint("GetMemoryRegionData:Getting PEPROCESS\n");
        if (!NT_SUCCESS(PsLookupProcessByProcessId((PVOID)PID,&selectedprocess)))
		   return FALSE; //couldn't get the PID

		DbgPrint("Retrieved peprocess");  
	}

	StartAddress=(UINT_PTR)mempointer;

	*baseaddress=((StartAddress) /0x1000) *0x1000;

	//switch context to the target process

	RtlZeroMemory(&apc_state,sizeof(apc_state));

	__try
	{
		KeAttachProcess((PEPROCESS)selectedprocess);
		__try
		{
			//do my stuff here


			(UINT_PTR)PPTE=*baseaddress / 0x1000 *PTESize+0xc0000000;
			(UINT_PTR)PPDE=((UINT_PTR)PPTE) / 0x1000 *PTESize+0xc0000000;

			//DbgPrint("PPTE=%p\nPPDE=%p\n",PPTE,PPDE);
			if ((PPDE->P==0) && (PPDE->A2==0))
			{
				//Not paged
    			//thats 4KB of PTE, wich is 1024 PTE's wich is 4096*1024 bytes wich is 4MB non-paged memory(in case of PAE obnlt 512 PTE's wich is 4096*512=2MB)
				UINT_PTR BaseAddressOfPDE;
					
				BaseAddressOfPDE=(((UINT_PTR)PPDE)-0xc0000000)/PTESize * 0x1000 ; //=address of pte (if it had one)
				BaseAddressOfPDE=((BaseAddressOfPDE)-0xc0000000)/PTESize * 0x1000 ; //=*baseaddress that this PDE points too . (Actually, just looking at the last 3 hex digits and filling the rest with 0's should also have worked)

				*memorysize=PAGE_SIZE_LARGE-(*baseaddress-BaseAddressOfPDE);
				*regiontype=PAGE_NOACCESS;
				(UINT_PTR)PPDE=(UINT_PTR)PPDE+PTESize;  //perhaps PPDE++ also works but at least I'm sure this works
				(UINT_PTR)PPTE=((UINT_PTR)(PPDE)-0xc0000000)/PTESize*0x1000; //point to the first PTE of the new PDE
			}
			else
			if (PPDE->PS) //it's a 4mb page meaning the PTE is invalid
			{
				UINT_PTR BaseAddressOfPDE;
					
				BaseAddressOfPDE=(((UINT_PTR)PPDE)-0xc0000000)/PTESize * 0x1000 ; //=address of pte (if it had one)
				BaseAddressOfPDE=((BaseAddressOfPDE)-0xc0000000)/PTESize * 0x1000 ; //=*baseaddress that this PDE points too . (Actually, just looking at the last 3 hex digits and filling the rest with 0's should also have worked)
				//find the *baseaddress in this 4 MB page

				*memorysize=PAGE_SIZE_LARGE-(*baseaddress-BaseAddressOfPDE);

				if ((PPDE->P)==0)
				{
					if (PPDE->A2==1)
                        *regiontype=PAGE_EXECUTE_READ;
					else
						*regiontype=PAGE_NOACCESS;
				}
				else
				{								
					if (PPDE->RW)
						*regiontype=PAGE_EXECUTE_READWRITE;
					else
		                *regiontype=PAGE_EXECUTE_READ;
				}
					

                //next PDE
				(UINT_PTR)PPDE=(UINT_PTR)PPDE+PTESize;  //perhaps PPDE++ also works but at least I'm sure this works
				(UINT_PTR)PPTE=((UINT_PTR)(PPDE)-0xc0000000)/PTESize*0x1000; //point to the first PTE of the new PDE
			}
			else
			{
				//4 KB
				*memorysize=0x1000;								

				//the PTE is readable
				if ((PPTE->P==0) && (PPTE->A2==0))
					*regiontype=PAGE_NOACCESS;
                else
				{						
					if (PPTE->P==1)
					{
						if (PPTE->RW==1)
							*regiontype=PAGE_EXECUTE_READWRITE;
						else
			                *regiontype=PAGE_EXECUTE_READ;
					}
					else
					{
						//not present, but paged
						//and since I don''t know if it's writable or not lets make it readonly
                        *regiontype=PAGE_EXECUTE_READ;
					}
				}

				(UINT_PTR)PPTE=(UINT_PTR)PPTE+PTESize; //next PTE in the list
    			(UINT_PTR)PPDE=((UINT_PTR)PPTE) / 0x1000 *PTESize+0xc0000000;
			}

			//now the location of the PDE and PTE are set as they should and I can scan the rest of the memory
			//DbgPrint("after first check: PPTE=%p\nPPDE=%p\n",PPTE,PPDE);

			while ((UINT_PTR)PPDE<MAX_PDE_POS)
			{
				//DbgPrint("PPTE=%p(%x)\nPPDE=%p(%x)\n",PPTE,(UINT_PTR)PPTE,PPDE,(UINT_PTR)PPDE);

				if (!((PPDE->P==0) && (PPDE->A2==0)))
				{
					//this is a valid PDE
					if (PPDE->PS==1)
					{
                        //it's a 4 MB PDE (so no PTE)								
						//now check the protection, if it is the same as *regiontype add 4 MB to the size
						//else break out of the loop
						if (*regiontype==PAGE_EXECUTE_READ)
						{
							if ((PPDE->RW==0) || ((PPDE->P==0) && (PPDE->A2==1)) )  //paged to disk, I gues it's read-only
								*memorysize+=PAGE_SIZE_LARGE;
							else
								break; //not the same protection so let's quit
						}
						
						if (*regiontype==PAGE_EXECUTE_READWRITE)
						{
							if ((PPDE->RW==1) && (PPDE->P==1) ) //only if it's present in memory.
								*memorysize+=PAGE_SIZE_LARGE;
							else
								break;
						}

						if (*regiontype==PAGE_NOACCESS)
						{
							if ((PPDE->P==0) && (PPDE->A2==0))
								*memorysize+=PAGE_SIZE_LARGE; 
							else
								break;
						}
						
						
					}
					else
					{
						//the 4MB bit wasn't set										
						//this means that we'll have to look through the PTEa PTE follows
						BOOLEAN EverythingOK=TRUE;
						while ((UINT_PTR)PPTE<((((UINT_PTR)(PPDE)+PTESize)-0xc0000000)/PTESize*0x1000)) //while the current PTE isn't in the memorylocation of the next PDE check the memory
						{											
							if (*regiontype==PAGE_NOACCESS)
							{									
								if ((PPTE->P==0) && (PPTE->A2==0)) //not readable so
									*memorysize+=0x1000;
								else
								{
									EverythingOK=FALSE;
									break; //the memory I found IS accessible																										
								}
								
								
							}

							if (*regiontype==PAGE_EXECUTE_READWRITE)
							{
								if ((PPTE->RW==1) || ((PPTE->P==1) || (PPTE->A2==1) )) 
									*memorysize+=0x1000; //writable or paged
								else
								{
									EverythingOK=FALSE;
									break;
								}										
									

							}

							if (*regiontype==PAGE_EXECUTE_READ)
							{
								if ((PPTE->RW==0) || ((PPTE->P==0) && (PPTE->A2==1) )) //read only or paged to disk (lets assume that the protection follows (just a gues)
									*memorysize+=0x1000;
								else
								{
									//if it's writable
									//or if it's not paged and the global bit is on
									//then it isn't read-only
									EverythingOK=FALSE;
									break;
								}	
							}            

							(UINT_PTR)PPTE=(UINT_PTR)PPTE+PTESize;
						}

						if (!EverythingOK) break;

					}
				}
				else
				{
					//4MB of non paged memory
					if (*regiontype==PAGE_NOACCESS)
						*memorysize+=PAGE_SIZE_LARGE; //increase the size of page_noaccess memory with 4 MB
					else
						break; //no, the previous wasn't PAGE_NOACCESS so break with the current length
				}

				(UINT_PTR)PPDE=(UINT_PTR)PPDE+PTESize;
				(UINT_PTR)PPTE=((UINT_PTR)(PPDE)-0xc0000000)/PTESize*0x1000; //point to the first PTE of the new PDE
			}

		
			if ((UINT_PTR)PPDE>=MAX_PDE_POS)
                ntStatus=STATUS_UNSUCCESSFUL;

		}
		__finally
		{
			KeDetachProcess();
		}

	}
	__except(1)
	{
		DbgPrint("Exception in GetMemoryRegionData\n");
		ntStatus=STATUS_UNSUCCESSFUL;
	}
	

	if (PEProcess==NULL) //no valid peprocess was given so I made a reference, so lets also dereference
		ObDereferenceObject(selectedprocess);

	return NT_SUCCESS(ntStatus);

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