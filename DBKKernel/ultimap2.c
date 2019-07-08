#pragma warning( disable: 4100 4706)

#include <ntifs.h>
#include <ntddk.h>
#include <minwindef.h>
#include <wdm.h>
#include <windef.h>

#include "Ntstrsafe.h"
#include "DBKFunc.h"

#include "ultimap2\apic.h"
#include "ultimap2.h"


PSSUSPENDPROCESS PsSuspendProcess;
PSSUSPENDPROCESS PsResumeProcess;
KDPC RTID_DPC;

BOOL LogKernelMode;
BOOL LogUserMode;

PEPROCESS CurrentTarget;
UINT64 CurrentCR3;
HANDLE Ultimap2Handle;
volatile BOOLEAN UltimapActive = FALSE;
volatile BOOLEAN isSuspended = FALSE;
volatile BOOLEAN flushallbuffers = FALSE; //set to TRUE if all the data should be flushed
KEVENT FlushData;

BOOL SaveToFile;
WCHAR OutputPath[200];

int Ultimap2RangeCount;
PURANGE Ultimap2Ranges = NULL;

PVOID *Ultimap2_DataReady;


#if (NTDDI_VERSION < NTDDI_VISTA)
//implement this function for XP
unsigned int KeQueryMaximumProcessorCount()
{
	CCHAR cpunr;
	KAFFINITY cpus, original;
	ULONG cpucount;

	cpucount = 0;
	cpus = KeQueryActiveProcessors();
	original = cpus;
	while (cpus)
	{
		if (cpus % 2)
			cpucount++;

		cpus = cpus / 2;
	}

	return cpucount;
}
#endif

typedef struct
{	
	PToPA_ENTRY ToPAHeader;
	PToPA_ENTRY ToPAHeader2;

	PVOID ToPABuffer;
	PVOID ToPABuffer2;

	PMDL ToPABufferMDL;
	PMDL ToPABuffer2MDL;

	PRTL_GENERIC_TABLE ToPALookupTable;
	PRTL_GENERIC_TABLE ToPALookupTable2;

	KEVENT Buffer2ReadyForSwap;
	KEVENT InitiateSave;

	KEVENT DataReady;
	KEVENT DataProcessed;

	UINT64 CurrentOutputBase;
	UINT64 CurrentSaveOutputBase;
	UINT64 CurrentSaveOutputMask;

	UINT64 MappedAddress; //set by WaitForData  , use with continue
	UINT64 Buffer2FlushSize; //used by WaitForData


	KDPC OwnDPC;
	HANDLE WriterThreadHandle;

	//for saveToFile mode
	HANDLE FileHandle;
	KEVENT FileAccess;
	UINT64 TraceFileSize;

	volatile BOOL Interrupted;
}  ProcessorInfo, *PProcessorInfo;
volatile PProcessorInfo *PInfo;

int Ultimap2CpuCount;


KMUTEX SuspendMutex;
KEVENT SuspendEvent;
HANDLE SuspendThreadHandle;
volatile int suspendCount;
BOOL ultimapEnabled = FALSE;
BOOL singleToPASystem = FALSE;
BOOL NoPMIMode = FALSE;


void suspendThread(PVOID StartContext)
/* Thread responsible for suspending the target process when the buffer is getting full */
{
	NTSTATUS wr;
	__try
	{
		while (UltimapActive)
		{
			wr = KeWaitForSingleObject(&SuspendEvent, Executive, KernelMode, FALSE, NULL);
			if (!UltimapActive) return;

			DbgPrint("suspendThread event triggered");
			KeWaitForSingleObject(&SuspendMutex, Executive, KernelMode, FALSE, NULL);
			if (!isSuspended)
			{
				if (CurrentTarget == 0)
				{
					if (PsSuspendProcess(CurrentTarget) == 0)
						isSuspended = TRUE;
					else
						DbgPrint("Failed to suspend target\n");
				}
			}
			KeReleaseMutex(&SuspendMutex, FALSE);
		}
	}
	__except (1)
	{
		DbgPrint("Exception in suspendThread thread\n");
	}
}

NTSTATUS ultimap2_continue(int cpunr)
{
	NTSTATUS r = STATUS_UNSUCCESSFUL;
	if ((cpunr < 0) || (cpunr >= Ultimap2CpuCount))
	{
		DbgPrint("ultimap2_continue(%d)", cpunr);
		return STATUS_UNSUCCESSFUL;
	}

	if (PInfo)
	{
		PProcessorInfo pi = PInfo[cpunr];

		if (pi->MappedAddress)
		{
			MmUnmapLockedPages((PVOID)(UINT_PTR)pi->MappedAddress, pi->ToPABuffer2MDL); //unmap this memory
			pi->MappedAddress = 0;
			r = STATUS_SUCCESS;
		}
		else
			DbgPrint("MappedAddress was 0");

		DbgPrint("%d DataProcessed", cpunr);
		KeSetEvent(&pi->DataProcessed, 0, FALSE); //let the next swap happen if needed

		
	}
	
	return r;
	
}

NTSTATUS ultimap2_waitForData(ULONG timeout, PULTIMAP2DATAEVENT data)
{
	NTSTATUS r=STATUS_UNSUCCESSFUL;




	//Wait for the events in the list
	//If an event is triggered find out which one is triggered, then map that block into the usermode space and return the address and block
	//That block will be needed to continue

	if (UltimapActive)
	{
		NTSTATUS wr = STATUS_UNSUCCESSFUL;
		LARGE_INTEGER wait;
		PKWAIT_BLOCK waitblock;

		int cpunr;

		waitblock = ExAllocatePool(NonPagedPool, Ultimap2CpuCount*sizeof(KWAIT_BLOCK));
		wait.QuadPart = -10000LL * timeout;

		if (timeout == 0xffffffff) //infinite wait
			wr = KeWaitForMultipleObjects(Ultimap2CpuCount, Ultimap2_DataReady, WaitAny, UserRequest, UserMode, TRUE, NULL, waitblock);
		else
			wr = KeWaitForMultipleObjects(Ultimap2CpuCount, Ultimap2_DataReady, WaitAny, UserRequest, UserMode, TRUE, &wait, waitblock);

		ExFreePool(waitblock);

		DbgPrint("ultimap2_waitForData wait returned %x", wr);

		cpunr = wr - STATUS_WAIT_0;


		if ((cpunr < Ultimap2CpuCount) && (cpunr>=0))
		{
			PProcessorInfo pi = PInfo[cpunr];

			


			if (pi->Buffer2FlushSize)
			{
				if (pi->ToPABuffer2MDL)
				{
					__try
					{

						data->Address = (UINT64)MmMapLockedPagesSpecifyCache(pi->ToPABuffer2MDL, UserMode, MmCached, NULL, FALSE, NormalPagePriority);

						DbgPrint("MmMapLockedPagesSpecifyCache returned address %p\n", data->Address);

						if (data->Address)
						{
							data->Size = pi->Buffer2FlushSize;
							data->CpuID = cpunr;

							pi->MappedAddress = data->Address;
							r = STATUS_SUCCESS;
						}

					}
					__except (1)
					{
						DbgPrint("ultimap2_waitForData: Failure mapping memory into waiter process. Count=%d", (int)MmGetMdlByteCount(pi->ToPABuffer2MDL));
					}
				}
				else
				{
					DbgPrint("ToPABuffer2MDL is NULL. Not even gonna try");
				}
			}
			else
			{
				DbgPrint("ultimap2_waitForData flushsize was 0");
			}
		}

	}

	DbgPrint("ultimap2_waitForData returned %x\n", r);
	return r;
}

void createUltimap2OutputFile(int cpunr)
{
	NTSTATUS r;
	PProcessorInfo pi = PInfo[cpunr];
	UNICODE_STRING usFile;
	OBJECT_ATTRIBUTES oaFile;
	IO_STATUS_BLOCK iosb;
	WCHAR Buffer[200];
	
#ifdef AMD64	
	DbgPrint("OutputPath=%S", OutputPath);
	swprintf_s(Buffer, 200, L"%sCPU%d.trace", OutputPath, cpunr);
#else
	RtlStringCbPrintfW(Buffer, 200, L"%sCPU%d.trace", OutputPath, cpunr);
#endif

	DbgPrint("Buffer=%S", Buffer);


	RtlInitUnicodeString(&usFile, Buffer);

	InitializeObjectAttributes(&oaFile, &usFile, OBJ_CASE_INSENSITIVE | OBJ_KERNEL_HANDLE, NULL, NULL);

	DbgPrint("Creating file %S", usFile.Buffer);

	pi->FileHandle = 0;
	ZwDeleteFile(&oaFile);
	r = ZwCreateFile(&pi->FileHandle, SYNCHRONIZE | FILE_READ_DATA | FILE_APPEND_DATA | GENERIC_ALL, &oaFile, &iosb, 0, FILE_ATTRIBUTE_NORMAL, 0, FILE_SUPERSEDE, FILE_SEQUENTIAL_ONLY | FILE_SYNCHRONOUS_IO_NONALERT, NULL, 0);
	DbgPrint("%d: ZwCreateFile=%x\n", (int)cpunr, r);



}

void WriteThreadForSpecificCPU(PVOID StartContext)
{
	int cpunr = (int)(UINT_PTR)StartContext;
	PProcessorInfo pi = PInfo[cpunr];
	


	IO_STATUS_BLOCK iosb;
	NTSTATUS r = STATUS_UNSUCCESSFUL;
	

	//DbgPrint("WriteThreadForSpecificCPU %d alive", (int)StartContext);



	if (SaveToFile)
	{
		if (KeWaitForSingleObject(&pi->FileAccess, Executive, KernelMode, FALSE, NULL) == STATUS_SUCCESS)
		{
			createUltimap2OutputFile(cpunr);
			KeSetEvent(&pi->FileAccess, 0, FALSE);
		}
		else
			createUltimap2OutputFile(cpunr);
	}

	
	KeSetSystemAffinityThread((KAFFINITY)(1 << cpunr));
	
	while (UltimapActive)
	{
		NTSTATUS wr = KeWaitForSingleObject(&pi->InitiateSave, Executive, KernelMode, FALSE, NULL);
		//DbgPrint("WriteThreadForSpecificCPU %d:  wr=%x", (int)StartContext, wr);
		if (!UltimapActive)
			break;
		
		if (wr == STATUS_SUCCESS)
		{
			UINT64 Size;
			ToPA_LOOKUP tl;
			PToPA_LOOKUP result;

			//DbgPrint("%d: writing buffer", (int)StartContext);

			//figure out the size
			tl.PhysicalAddress = pi->CurrentSaveOutputBase;
			tl.index = 0;
			result = RtlLookupElementGenericTable(pi->ToPALookupTable2, &tl);

			if (result)
			{
				//write...
				//DbgPrint("%d: result->index=%d CurrentSaveOutputMask=%p", (int)StartContext, result->index, pi->CurrentSaveOutputMask);
				if (singleToPASystem)
					Size = pi->CurrentSaveOutputMask >> 32;
				else
					Size = ((result->index * 511) + ((pi->CurrentSaveOutputMask & 0xffffffff) >> 7)) * 4096 + (pi->CurrentSaveOutputMask >> 32);

				if (Size > 0)
				{

					if (SaveToFile)
					{
						wr = KeWaitForSingleObject(&pi->FileAccess, Executive, KernelMode, FALSE, NULL);
						if (wr==STATUS_SUCCESS)
						{
							if (pi->FileHandle==0) //a usermode flush has happened
								createUltimap2OutputFile(cpunr); 

							r = ZwWriteFile(pi->FileHandle, NULL, NULL, NULL, &iosb, pi->ToPABuffer2, (ULONG)Size, NULL, NULL);

							pi->TraceFileSize += Size;
							//DbgPrint("%d: ZwCreateFile(%p, %d)=%x\n", (int)StartContext, pi->ToPABuffer2, (ULONG)Size, r);

							KeSetEvent(&pi->FileAccess, 0, FALSE);
						}
					}
					else
					{
						//map ToPABuffer2 into the CE process
						
						//wake up a worker thread
						pi->Buffer2FlushSize = Size;
						DbgPrint("%d: WorkerThread(%p, %d)=%x\n", (int)(UINT_PTR)StartContext, pi->ToPABuffer2, (ULONG)Size, r);
						KeSetEvent(&pi->DataReady, 0, TRUE); //a ce thread waiting in ultimap2_waitForData should now wake and process the data
						//and wait for it to finish
						r=KeWaitForSingleObject(&pi->DataProcessed, Executive, KernelMode, FALSE, NULL);	
						DbgPrint("KeWaitForSingleObject(DataProcessed)=%x", r);

					}
					//DbgPrint("%d: Writing %x bytes\n", (int)StartContext, Size);
				}


			}
			else
				DbgPrint("Unexpected physical address while writing results for cpu %d  (%p)", (int)(UINT_PTR)StartContext, pi->CurrentSaveOutputBase);
			

			KeSetEvent(&pi->Buffer2ReadyForSwap, 0, FALSE);
		}		
	}

	KeSetSystemAffinityThread(KeQueryActiveProcessors());

	if (pi->FileHandle)
		ZwClose(pi->FileHandle);

	KeSetEvent(&pi->Buffer2ReadyForSwap, 0, FALSE); 
}

void ultimap2_LockFile(int cpunr)
{
	if ((cpunr < 0) || (cpunr >= Ultimap2CpuCount))
		return;

	if (PInfo)
	{
		NTSTATUS wr;
		PProcessorInfo pi = PInfo[cpunr];

		//DbgPrint("AcquireUltimap2File()");
		wr = KeWaitForSingleObject(&pi->FileAccess, Executive, KernelMode, FALSE, NULL);
		if (wr == STATUS_SUCCESS)
		{
			//DbgPrint("Acquired");
			if (pi->FileHandle)
			{
				ZwClose(pi->FileHandle);
				pi->FileHandle = 0;
			}
		}
	}
}

void ultimap2_ReleaseFile(int cpunr)
{
	if ((cpunr < 0) || (cpunr >= Ultimap2CpuCount))
		return;

	if (PInfo)
	{
		PProcessorInfo pi = PInfo[cpunr];
		KeSetEvent(&pi->FileAccess, 0, FALSE);
		//DbgPrint("Released");
	}
}

UINT64 ultimap2_GetTraceFileSize()
//Gets an aproximation of the filesize.  Don't take this too exact
{
	UINT64 size = 0;
	
	if (PInfo)
	{
		int i;
		for (i = 0; i < Ultimap2CpuCount; i++)
			size += PInfo[i]->TraceFileSize;
	}
	
	return size;
}

void ultimap2_ResetTraceFileSize()
{
	if (PInfo)
	{
		int i;
		for (i = 0; i < Ultimap2CpuCount; i++)
			PInfo[i]->TraceFileSize = 0;
	}	
}


void SwitchToPABuffer(struct _KDPC *Dpc, PVOID DeferredContext, PVOID SystemArgument1, PVOID SystemArgument2)
/*
DPC routine that switches the Buffer pointer and marks buffer2 that it's ready for data saving
Only called when buffer2 is ready for flushing
*/
{
	//write the contents of the current cpu buffer
	PProcessorInfo pi = PInfo[KeGetCurrentProcessorNumber()];

	//DbgPrint("SwitchToPABuffer for cpu %d\n", KeGetCurrentProcessorNumber());

	if (pi)
	{		
		UINT64 CTL = __readmsr(IA32_RTIT_CTL);
		UINT64 Status = __readmsr(IA32_RTIT_STATUS);
		PVOID temp;

		if ((Status >> 5) & 1) //Stopped
			DbgPrint("%d Not all data recorded\n", KeGetCurrentProcessorNumber());


		if ((Status >> 4) & 1)
			DbgPrint("ALL LOST");

		//only if the buffer is bigger than 2 pages.  That you can check in IA32_RTIT_OUTPUT_MASK_PTRS and IA32_RTIT_OUTPUT_BASE 
		//if (KeGetCurrentProcessorNumber() == 0)
		//	DbgPrint("%d: pi->CurrentOutputBase=%p __readmsr(IA32_RTIT_OUTPUT_BASE)=%p __readmsr(IA32_RTIT_OUTPUT_MASK_PTRS)=%p", KeGetCurrentProcessorNumber(), pi->CurrentOutputBase, __readmsr(IA32_RTIT_OUTPUT_BASE), __readmsr(IA32_RTIT_OUTPUT_MASK_PTRS));


		if (pi->Interrupted == FALSE)
		{
			//return; //debug test. remove me when released
			
			if (!singleToPASystem)
			{
				if ((!flushallbuffers) && (((__readmsr(IA32_RTIT_OUTPUT_MASK_PTRS) & 0xffffffff) >> 7) < 2))
					return; //don't flush yet
			}
			else
			{
				INT64 offset = __readmsr(IA32_RTIT_OUTPUT_MASK_PTRS);

				/*if (KeGetCurrentProcessorNumber() == 0)
				{
					DbgPrint("pi->CurrentOutputBase=%p", pi->CurrentOutputBase);
					DbgPrint("offset=%p", offset);
				}*/

				offset = offset >> 32;

				//if (KeGetCurrentProcessorNumber() == 0)
				//	DbgPrint("offset=%p", offset);

				if ((!flushallbuffers) && (((pi->CurrentOutputBase == 0) || (offset < 8192))))
					return; //don't flush yet
			}
		}
		else
		{
			DbgPrint("%d:Flushing because of interrupt", KeGetCurrentProcessorNumber());
		}

		DbgPrint("%d: Flush this data (%p)", KeGetCurrentProcessorNumber(), __readmsr(IA32_RTIT_OUTPUT_MASK_PTRS));
		//DbgPrint("%d: pi->CurrentOutputBase=%p __readmsr(IA32_RTIT_OUTPUT_BASE)=%p __readmsr(IA32_RTIT_OUTPUT_MASK_PTRS)=%p", KeGetCurrentProcessorNumber(), pi->CurrentOutputBase, __readmsr(IA32_RTIT_OUTPUT_BASE), __readmsr(IA32_RTIT_OUTPUT_MASK_PTRS));

		__writemsr(IA32_RTIT_CTL, 0); //disable packet generation
		__writemsr(IA32_RTIT_STATUS, 0);


		//DbgPrint("%d: pi->CurrentOutputBase=%p __readmsr(IA32_RTIT_OUTPUT_BASE)=%p __readmsr(IA32_RTIT_OUTPUT_MASK_PTRS)=%p", KeGetCurrentProcessorNumber(), pi->CurrentOutputBase, __readmsr(IA32_RTIT_OUTPUT_BASE), __readmsr(IA32_RTIT_OUTPUT_MASK_PTRS));

		
		

		//switch the pointer to the secondary buffers
		KeClearEvent(&pi->Buffer2ReadyForSwap);

		//swap the buffer
		temp = pi->ToPABuffer;
		pi->ToPABuffer = pi->ToPABuffer2; 
		pi->ToPABuffer2 = temp;

		//swap the MDL that describes it
		temp = pi->ToPABufferMDL;
		pi->ToPABufferMDL = pi->ToPABuffer2MDL;
		pi->ToPABuffer2MDL = temp;

		//swap the header
		temp = pi->ToPAHeader;
		pi->ToPAHeader = pi->ToPAHeader2;
		pi->ToPAHeader2 = temp;

		//swap the lookup table
		temp = pi->ToPALookupTable;
		pi->ToPALookupTable = pi->ToPALookupTable2;
		pi->ToPALookupTable2 = temp;

		//lookup which entry it's pointing at
		pi->CurrentSaveOutputBase = __readmsr(IA32_RTIT_OUTPUT_BASE);
		pi->CurrentSaveOutputMask = __readmsr(IA32_RTIT_OUTPUT_MASK_PTRS);

		KeSetEvent(&pi->InitiateSave,0,FALSE);

		pi->Interrupted = FALSE;

		//reactivate packet generation
		pi->CurrentOutputBase = MmGetPhysicalAddress(pi->ToPAHeader).QuadPart;

		__writemsr(IA32_RTIT_OUTPUT_BASE, pi->CurrentOutputBase);
		__writemsr(IA32_RTIT_OUTPUT_MASK_PTRS, 0);

		__writemsr(IA32_RTIT_CTL, CTL);
	}
}

void WaitForWriteToFinishAndSwapWriteBuffers(BOOL interruptedOnly)
{
    int i;
	
	for (i = 0; i < Ultimap2CpuCount; i++)
	{
		PProcessorInfo pi = PInfo[i];
		if ((pi->ToPABuffer2) && ((pi->Interrupted) || (!interruptedOnly)))
		{
			KeWaitForSingleObject(&pi->Buffer2ReadyForSwap, Executive, KernelMode, FALSE, NULL);

			if (!UltimapActive) return;

			KeInsertQueueDpc(&pi->OwnDPC, NULL, NULL);
		}
		
	}

	KeFlushQueuedDpcs();
}

void bufferWriterThread(PVOID StartContext)
{
	//passive mode

	//wait for event
	LARGE_INTEGER Timeout;
	NTSTATUS wr;

	DbgPrint("bufferWriterThread active");

	
	while (UltimapActive)
	{
		if (NoPMIMode)
			Timeout.QuadPart = -1000LL;  //- 10000LL=1 millisecond //-100000000LL = 10 seconds   -1000000LL= 0.1 second
		else
			Timeout.QuadPart = -10000LL;  //- 10000LL=1 millisecond //-100000000LL = 10 seconds   -1000000LL= 0.1 second

		//DbgPrint("%d : Wait for FlushData", cpunr());
		wr = KeWaitForSingleObject(&FlushData, Executive, KernelMode, FALSE, &Timeout);
		//DbgPrint("%d : After wait for FlushData", cpunr());
		//wr = KeWaitForSingleObject(&FlushData, Executive, KernelMode, FALSE, NULL);

		//DbgPrint("bufferWriterThread: Alive (wr==%x)", wr);
		if (!UltimapActive)
		{
			DbgPrint("bufferWriterThread: Terminating");
			return;
		}

		//if (wr != STATUS_SUCCESS) continue; //DEBUG code so PMI's get triggered



		if ((wr == STATUS_SUCCESS) || (wr == STATUS_TIMEOUT))
		{
			if ((wr == STATUS_SUCCESS) && (!isSuspended))
			{
				//woken up by a dpc				
				DbgPrint("FlushData event set and not suspended. Suspending target process\n");
				KeWaitForSingleObject(&SuspendMutex, Executive, KernelMode, FALSE, NULL);
				if (!isSuspended)
				{
					DbgPrint("Still going to suspend target process");
					if (PsSuspendProcess(CurrentTarget)==0)
						isSuspended = TRUE;
				}
				KeReleaseMutex(&SuspendMutex, FALSE);

				DbgPrint("After the target has been suspended (isSuspended=%d)\n", isSuspended);
			}			

			if (wr == STATUS_SUCCESS) //the filled cpu's must take preference
			{
				unsigned int i;
				BOOL found = TRUE;

				//DbgPrint("bufferWriterThread: Suspended");


				//first flush the CPU's that complained their buffers are full
				DbgPrint("Flushing full CPU\'s");
				while (found)
				{
					WaitForWriteToFinishAndSwapWriteBuffers(TRUE);
					if (!UltimapActive) return;

					//check if no interrupt has been triggered while this was busy ('could' happen as useless info like core ratio is still recorded)
					found = FALSE;
					for (i = 0; i < KeQueryMaximumProcessorCount(); i++)
					{
						if (PInfo[i]->Interrupted)
						{
							DbgPrint("PInfo[%d]->Interrupted\n", PInfo[i]->Interrupted);
							found = TRUE;
							break;
						}
					}
				}
			}

			//wait till the previous buffers are done writing
			//DbgPrint("%d: Normal flush", cpunr());
			WaitForWriteToFinishAndSwapWriteBuffers(FALSE);
			//DbgPrint("%d : after flush", cpunr());

			if (isSuspended)
			{
				KeWaitForSingleObject(&SuspendMutex, Executive, KernelMode, FALSE, NULL);
				if (isSuspended)
				{
					DbgPrint("Resuming target process");
					PsResumeProcess(CurrentTarget);
					isSuspended = FALSE;
				}
				KeReleaseMutex(&SuspendMutex, FALSE);
			}
			//an interrupt could have fired while WaitForWriteToFinishAndSwapWriteBuffers was busy, pausing the process. If that happened, then the next KeWaitForSingleObject will exit instantly due to it being signaled 
		}
		else
			DbgPrint("Unexpected wait result");
		
	}
}


NTSTATUS ultimap2_flushBuffers()
{
	if (!UltimapActive)
		return STATUS_UNSUCCESSFUL;

	DbgPrint("ultimap2_flushBuffers");

	KeWaitForSingleObject(&SuspendMutex, Executive, KernelMode, FALSE, NULL);
	if (CurrentTarget)
	{
		if (!isSuspended)
		{
			PsSuspendProcess(CurrentTarget);
			isSuspended = TRUE;
		}
	}
	KeReleaseMutex(&SuspendMutex, FALSE);

	flushallbuffers = TRUE;
	
	DbgPrint("wait1");
	WaitForWriteToFinishAndSwapWriteBuffers(FALSE); //write the last saved buffer

	DbgPrint("wait2");
	WaitForWriteToFinishAndSwapWriteBuffers(FALSE); //write the current buffer

	flushallbuffers = FALSE;
	DbgPrint("after wait");
	KeWaitForSingleObject(&SuspendMutex, Executive, KernelMode, FALSE, NULL);
	if (CurrentTarget)
	{
		if (isSuspended)
		{
			PsResumeProcess(CurrentTarget);
			isSuspended = FALSE;
		}
	}
	KeReleaseMutex(&SuspendMutex, FALSE);	

	DbgPrint("ultimap2_flushBuffers exit");
	return STATUS_SUCCESS;
}



void RTIT_DPC_Handler(__in struct _KDPC *Dpc, __in_opt PVOID DeferredContext, __in_opt PVOID SystemArgument1,__in_opt PVOID SystemArgument2)
{
	//Signal the bufferWriterThread
	KeSetEvent(&SuspendEvent, 0, FALSE);
	KeSetEvent(&FlushData, 0, FALSE);
}
 

void PMI(__in struct _KINTERRUPT *Interrupt, __in PVOID ServiceContext)
{
	//check if caused by me, if so defer to dpc
	DbgPrint("PMI");
	__try
	{
		if ((__readmsr(IA32_PERF_GLOBAL_STATUS) >> 55) & 1)
		{
			UINT64 Status = __readmsr(IA32_RTIT_STATUS);

			DbgPrint("PMI: caused by me");	
			__writemsr(IA32_PERF_GLOBAL_OVF_CTRL, (UINT64)1 << 55); //clear ToPA full status

			if ((__readmsr(IA32_PERF_GLOBAL_STATUS) >> 55) & 1)
			{
				DbgPrint("PMI: Failed to clear the status\n");
			}

			DbgPrint("PMI: IA32_RTIT_OUTPUT_MASK_PTRS=%p\n", __readmsr(IA32_RTIT_OUTPUT_MASK_PTRS));
			DbgPrint("PMI: IA32_RTIT_STATUS=%p\n", Status);
			
			if ((Status >> 5) & 1) //Stopped
				DbgPrint("PMI %d: Not all data recorded (AT THE PMI!)\n", KeGetCurrentProcessorNumber());


			DbgPrint("PMI: IA32_RTIT_OUTPUT_MASK_PTRS %p\n", __readmsr(IA32_RTIT_OUTPUT_MASK_PTRS));

			PInfo[KeGetCurrentProcessorNumber()]->Interrupted = TRUE;

			KeInsertQueueDpc(&RTID_DPC, NULL, NULL);

			//clear apic state

			apic_clearPerfmon();
		}
		else
		{
			DbgPrint("Unexpected PMI");
		}
	}
	__except (0)
	{
		DbgPrint("PMI exception");
	}

}

void *pperfmon_hook2 = (void *)PMI;


void ultimap2_disable_dpc(struct _KDPC *Dpc, PVOID DeferredContext, PVOID SystemArgument1, PVOID SystemArgument2)
{
	DbgPrint("ultimap2_disable_dpc for cpu %d\n", KeGetCurrentProcessorNumber());

	__try
	{
		if (DeferredContext) //only pause
		{
			RTIT_CTL ctl;
			DbgPrint("temp disable\n");
			ctl.Value = __readmsr(IA32_RTIT_CTL);
			ctl.Bits.TraceEn = 0;
			__writemsr(IA32_RTIT_CTL, ctl.Value);
		}
		else
		{
			DbgPrint("%d: disable all\n", KeGetCurrentProcessorNumber());


			__writemsr(IA32_RTIT_CTL, 0);
			__writemsr(IA32_RTIT_STATUS, 0);
			__writemsr(IA32_RTIT_CR3_MATCH, 0);
			__writemsr(IA32_RTIT_OUTPUT_BASE, 0);
			__writemsr(IA32_RTIT_OUTPUT_MASK_PTRS, 0);
		}
	}
	__except (1)
	{
		DbgPrint("ultimap2_disable_dpc exception");
	}
}

void ultimap2_setup_dpc(struct _KDPC *Dpc, PVOID DeferredContext, PVOID SystemArgument1, PVOID SystemArgument2)
{
	RTIT_CTL ctl;
	RTIT_STATUS s;
	int i = -1;



	__try
	{
		ctl.Value = __readmsr(IA32_RTIT_CTL);

	}
	__except (1)
	{
		DbgPrint("ultimap2_setup_dpc: IA32_RTIT_CTL in unreadable");
		return;
	}

	ctl.Bits.TraceEn = 1;

	if (LogKernelMode)
		ctl.Bits.OS = 1;
	else
		ctl.Bits.OS = 0;

	if (LogUserMode)
		ctl.Bits.USER = 1;
	else
		ctl.Bits.USER = 0;

	if (CurrentCR3)
		ctl.Bits.CR3Filter = 1;
	else
		ctl.Bits.CR3Filter = 0;
		
	ctl.Bits.ToPA = 1;
	ctl.Bits.TSCEn = 0;
	ctl.Bits.DisRETC = 0;
	ctl.Bits.BranchEn = 1;

	if (PInfo == NULL)
		return;
	 
	if (PInfo[KeGetCurrentProcessorNumber()]->ToPABuffer == NULL)
	{
		DbgPrint("ToPA for cpu %d not setup\n", KeGetCurrentProcessorNumber());
		return;
	}
	
	__try
	{
		int cpunr = KeGetCurrentProcessorNumber();
		i = 0;

		PInfo[cpunr]->CurrentOutputBase = MmGetPhysicalAddress(PInfo[cpunr]->ToPAHeader).QuadPart;

		__writemsr(IA32_RTIT_OUTPUT_BASE, PInfo[cpunr]->CurrentOutputBase);
		i = 1;
		__writemsr(IA32_RTIT_OUTPUT_MASK_PTRS, 0);
		i = 2;


		__try
		{
			__writemsr(IA32_RTIT_CR3_MATCH, CurrentCR3);
		}
		__except (1)
		{
			CurrentCR3 = CurrentCR3 & 0xfffffffffffff000ULL;
			DbgPrint("Failed to set the actual CR3. Using a sanitized CR3: %llx\n", CurrentCR3);
		}

		i = 3;

		//ranges
		if (Ultimap2Ranges && Ultimap2RangeCount)
		{
			
			for (i = 0; i < Ultimap2RangeCount; i++)
			{
				ULONG msr_start = IA32_RTIT_ADDR0_A + (2 * i);
				ULONG msr_stop = IA32_RTIT_ADDR0_B + (2 * i);
				UINT64 bit = 32 + (i * 4);

				DbgPrint("Range %d: (%p -> %p)", i, (PVOID)(UINT_PTR)(Ultimap2Ranges[i].StartAddress), (PVOID)(UINT_PTR)(Ultimap2Ranges[i].EndAddress));
				DbgPrint("Writing range %d to msr %x and %x", i, msr_start, msr_stop);
				__writemsr(msr_start, Ultimap2Ranges[i].StartAddress);
				__writemsr(msr_stop, Ultimap2Ranges[i].EndAddress);

				DbgPrint("bit=%d", bit);
				DbgPrint("Value before=%llx", ctl.Value);
				if (Ultimap2Ranges[i].IsStopAddress)
					ctl.Value |= (UINT64)2ULL << bit; //TraceStop This stops all tracing on this cpu. Doesn't get reactivated
				else
					ctl.Value |= (UINT64)1ULL << bit; //FilterEn //not supported in the latest windows build

				DbgPrint("Value after=%llx", ctl.Value);
			}
		}
		i = 4;

		__writemsr(IA32_RTIT_STATUS, 0);
		i = 5;
		//if (KeGetCurrentProcessorNumber() == 0)
		__writemsr(IA32_RTIT_CTL, ctl.Value);
		i = 6;

	
			
		s.Value=__readmsr(IA32_RTIT_STATUS);
		if (s.Bits.Error)
			DbgPrint("Setup for cpu %d failed", KeGetCurrentProcessorNumber());
		else
			DbgPrint("Setup for cpu %d succesful", KeGetCurrentProcessorNumber());
	}
	__except (1)
	{
		DbgPrint("Error in ultimap2_setup_dpc.  i=%d",i);
		DbgPrint("ctl.Value=%p\n", ctl.Value);
		DbgPrint("CR3=%p\n", CurrentCR3);
		//DbgPrint("OutputBase=%p", __readmsr(IA32_RTIT_OUTPUT_BASE));
	}
	
}

int getToPAHeaderCount(ULONG _BufferSize)
{
	return 1 + (_BufferSize / 4096) / 511;
}

int getToPAHeaderSize(ULONG _BufferSize)
{
	//511 entries per ToPA header (4096*511=2093056 bytes per ToPA header)
	//BufferSize / 2093056 = Number of ToPA headers needed
	return getToPAHeaderCount(_BufferSize) * 4096;
}

RTL_GENERIC_COMPARE_RESULTS NTAPI ToPACompare(__in struct _RTL_GENERIC_TABLE *Table, __in PToPA_LOOKUP FirstStruct, __in PToPA_LOOKUP SecondStruct)
{
	//DbgPrint("Comparing %p with %p", FirstStruct->PhysicalAddress, FirstStruct->PhysicalAddress);

	if (FirstStruct->PhysicalAddress == SecondStruct->PhysicalAddress)
		return GenericEqual;
	else
	{
		if (SecondStruct->PhysicalAddress < FirstStruct->PhysicalAddress)
			return GenericLessThan;
		else
			return GenericGreaterThan;
	}
}

PVOID NTAPI ToPAAlloc(__in struct _RTL_GENERIC_TABLE *Table, __in CLONG ByteSize)
{
	return ExAllocatePool(NonPagedPool, ByteSize);
}

VOID NTAPI ToPADealloc(__in struct _RTL_GENERIC_TABLE *Table, __in __drv_freesMem(Mem) __post_invalid PVOID Buffer)
{
	ExFreePool(Buffer);
}

void* setupToPA(PToPA_ENTRY *Header, PVOID *OutputBuffer, PMDL *BufferMDL, PRTL_GENERIC_TABLE *gt, ULONG _BufferSize, int NoPMI)
{
	ToPA_LOOKUP tl;
	PToPA_ENTRY r;
	UINT_PTR Output, Stop;
	ULONG ToPAIndex = 0;
	int PABlockSize = 0;
	int BlockSize;


	PRTL_GENERIC_TABLE x;
	int i;

	if (singleToPASystem)
	{
		
		PHYSICAL_ADDRESS la,ha, boundary;
		ULONG newsize;

		BlockSize = _BufferSize; //yup, only 1 single entry	
		

		//get the closest possible
		if (BlockSize > 64 * 1024 * 1024)
			{
				PABlockSize = 15;
				BlockSize = 128 * 1024 * 1024;
			}
			else
				if (BlockSize > 32 * 1024 * 1024)
				{
					PABlockSize = 14;
					BlockSize = 64 * 1024 * 1024;
				}
				else
					if (BlockSize > 16 * 1024 * 1024)
					{
						PABlockSize = 13;
						BlockSize = 32 * 1024 * 1024;
					}
					else
						if (BlockSize > 8 * 1024 * 1024)
						{
							PABlockSize = 12;
							BlockSize = 16 * 1024 * 1024;
						}
						else
							if (BlockSize > 4 * 1024 * 1024)
							{
								PABlockSize = 11;
								BlockSize = 8 * 1024 * 1024;
							}
							else
								if (BlockSize > 2 * 1024 * 1024)
								{
									PABlockSize = 10;
									BlockSize = 4 * 1024 * 1024;
								}
								else
									if (BlockSize > 1 * 1024 * 1024)
									{
										PABlockSize = 9;
										BlockSize = 2 * 1024 * 1024;
									}
									else
										if (BlockSize > 512 * 1024)
										{
											PABlockSize = 8;
											BlockSize = 1 * 1024 * 1024;
										}
										else
											if (BlockSize > 256 * 1024)
											{
												PABlockSize = 7;
												BlockSize = 512 * 1024;
											}
											else
												if (BlockSize > 128 * 1024)
												{
													PABlockSize = 6;
													BlockSize = 256 * 1024;
												}
												else
													if (BlockSize > 64 * 1024)
													{
														PABlockSize = 5;
														BlockSize = 128 * 1024;
													}
													else
														if (BlockSize > 32 * 1024)
														{
															PABlockSize = 4;
															BlockSize = 64 * 1024;
														}
														else
															if (BlockSize > 16*1024)
															{
																PABlockSize = 3;
																BlockSize = 32 * 1024;
															}
															else
																if (BlockSize > 8 * 1024)
																{
																	PABlockSize = 2;
																	BlockSize = 16 * 1024;
																}
																else
																	if (BlockSize > 4 * 1024)
																	{
																		PABlockSize = 1;
																		BlockSize = 8 * 1024;
																	}
																	else
																	{
																		PABlockSize = 0;
																		BlockSize = 4096;
																	}

		//adjust the buffersize so it is dividable by the blocksize
		newsize = BlockSize;
			
		DbgPrint("BufferSize=%x\n", _BufferSize);
		DbgPrint("BlockSize=%x (PABlockSize=%d)\n", BlockSize, PABlockSize);
		DbgPrint("newsize=%x\n", newsize);

		
		la.QuadPart = 0;
		ha.QuadPart = 0xFFFFFFFFFFFFFFFFULL;
		boundary.QuadPart = BlockSize;

		*OutputBuffer=MmAllocateContiguousMemorySpecifyCache(newsize, la, ha, boundary, MmCached);
		//*OutputBuffer=MmAllocateContiguousMemory(newsize, ha);

		DbgPrint("Allocated OutputBuffer at %p", MmGetPhysicalAddress(*OutputBuffer).QuadPart);

		_BufferSize = newsize;

		if (*OutputBuffer == NULL)
		{
			DbgPrint("setupToPA (Single ToPA System): Failure allocating output buffer");
			return NULL;
		}

		r = ExAllocatePool(NonPagedPool, 4096);
		if (r == NULL)
		{
			MmFreeContiguousMemory(*OutputBuffer);
			*OutputBuffer = NULL;
			DbgPrint("setupToPA (Single ToPA System): Failure allocating header for buffer");
			return NULL;
		}

	}
	else
	{
		//Not a single ToPA system
		BlockSize = 4096;

		*OutputBuffer = ExAllocatePool(NonPagedPool, _BufferSize);
		if (*OutputBuffer == NULL)
		{
			DbgPrint("setupToPA: Failure allocating output buffer");
			return NULL;
		}

		r = ExAllocatePool(NonPagedPool, getToPAHeaderSize(_BufferSize));
		if (r == NULL)
		{
			ExFreePool(*OutputBuffer);
			*OutputBuffer = NULL;
			DbgPrint("setupToPA: Failure allocating header for buffer");
			return NULL;
		}
	}
	

	*Header = r;

	*gt=ExAllocatePool(NonPagedPool, sizeof(RTL_GENERIC_TABLE));

	if (*gt == NULL)
	{
		DbgPrint("Failure allocating table");
		if (singleToPASystem)
			MmFreeContiguousMemory(*OutputBuffer);
		else
			ExFreePool(*OutputBuffer);
		*OutputBuffer = NULL;

		ExFreePool(*Header);
		*Header = NULL;

		return NULL;
	}

	x = *gt;

	RtlInitializeGenericTable(x, ToPACompare, ToPAAlloc, ToPADealloc, NULL);


	tl.index = 0;
	tl.PhysicalAddress = MmGetPhysicalAddress(&r[0]).QuadPart;
	RtlInsertElementGenericTable(x, &tl, sizeof(tl), NULL);

	Output = (UINT_PTR)*OutputBuffer;
	Stop = Output+_BufferSize;
	
	*BufferMDL = IoAllocateMdl(*OutputBuffer, _BufferSize, FALSE, FALSE, NULL);
	MmBuildMdlForNonPagedPool(*BufferMDL);

	if (singleToPASystem)
	{
		r[0].Value = (UINT64)MmGetPhysicalAddress((PVOID)Output).QuadPart;
		r[0].Bits.Size = PABlockSize;
		if (NoPMI)
			r[0].Bits.INT = 0;
		else
		  r[0].Bits.INT = 1;
		r[0].Bits.STOP = 1;
		
		r[1].Value = MmGetPhysicalAddress(&r[0]).QuadPart;
		r[1].Bits.END = 1;
	}
	else
	{
		while (Output < Stop)
		{
			//fill in the topa entries pointing to eachother


			if ((ToPAIndex + 1) % 512 == 0)
			{
				//point it to the next ToPA table
				r[ToPAIndex].Value = MmGetPhysicalAddress(&r[ToPAIndex + 1]).QuadPart;
				r[ToPAIndex].Bits.END = 1;

				tl.index = tl.index++;
				tl.PhysicalAddress = MmGetPhysicalAddress(&r[ToPAIndex + 1]).QuadPart;
				RtlInsertElementGenericTable(x, &tl, sizeof(tl), NULL);
			}
			else
			{
				r[ToPAIndex].Value = (UINT64)MmGetPhysicalAddress((PVOID)Output).QuadPart;
				r[ToPAIndex].Bits.Size = 0;
				Output += 4096;
			}

			ToPAIndex++;
		}

		ToPAIndex--;
		r[ToPAIndex].Bits.STOP = 1;
		i = (ToPAIndex * 90) / 100; //90%

		if ((i == (int)ToPAIndex) && (i > 0)) //don't interrupt on the very last entry (if possible)
			i--;

		if ((i > 0) && ((i + 1) % 512 == 0))
			i--;


		DbgPrint("Interrupt at index %d", i);

		if (NoPMI)
			r[i].Bits.INT = 0;
		else
			r[i].Bits.INT = 1; //Interrupt after filling this entry 


		//and every 2nd page after this.  (in case of a rare situation where resume is called right after suspend)

		if (ToPAIndex > 0)
		{
			while (i < (int)(ToPAIndex - 1))
			{
				if (((i + 1) % 512) && (NoPMI==0))  //anything but 0
					r[i].Bits.INT = 1;

				i += 2;
			}
		}
	}

	return (void *)r;
}

NTSTATUS ultimap2_pause()
{
	if (ultimapEnabled)
	{
		forEachCpu(ultimap2_disable_dpc, (PVOID)1, NULL, NULL, NULL);
		if (UltimapActive)
		{
			flushallbuffers = TRUE;
			WaitForWriteToFinishAndSwapWriteBuffers(FALSE); //write the last saved buffer
			WaitForWriteToFinishAndSwapWriteBuffers(FALSE); //write the current buffer
			flushallbuffers = FALSE;
		}
	}

	return STATUS_SUCCESS; 
}

NTSTATUS ultimap2_resume()
{
	if ((ultimapEnabled) && (PInfo))
		forEachCpu(ultimap2_setup_dpc, NULL, NULL, NULL, NULL);

	return STATUS_SUCCESS;
}



void *clear = NULL;
BOOL RegisteredProfilerInterruptHandler;
void SetupUltimap2(UINT32 PID, UINT32 BufferSize, WCHAR *Path, int rangeCount, PURANGE Ranges, int NoPMI, int UserMode, int KernelMode)
{
	//for each cpu setup tracing
	//add the PMI interupt
	int i;
	NTSTATUS r= STATUS_UNSUCCESSFUL;
	int cpuid_r[4];

	if (Path)
		DbgPrint("SetupUltimap2(%x, %x, %S, %d, %p,%d,%d,%d\n", PID, BufferSize, Path, rangeCount, Ranges, NoPMI, UserMode, KernelMode);
	else
		DbgPrint("SetupUltimap2(%x, %x, %d, %p,%d,%d,%d\n", PID, BufferSize, rangeCount, Ranges, NoPMI, UserMode, KernelMode);


	__cpuidex(cpuid_r, 0x14, 0);

	if ((cpuid_r[2] & 2) == 0)
	{
		DbgPrint("Single ToPA System");
		singleToPASystem = TRUE;
	}

	NoPMIMode = NoPMI;
	LogKernelMode = KernelMode;
	LogUserMode = UserMode;



	DbgPrint("Path[0]=%d\n", Path[0]);

	SaveToFile = (Path[0] != 0);

	if (SaveToFile)
	{
		wcsncpy(OutputPath, Path, 199);
		OutputPath[199] = 0;
		DbgPrint("Ultimap2: SaveToFile==TRUE:  OutputPath=%S",OutputPath);
	}
	else
	{
		DbgPrint("Ultimap2: Runtime processing");
	}

	if (rangeCount)
	{
		if (Ultimap2Ranges)
		{
			ExFreePool(Ultimap2Ranges);
			Ultimap2Ranges = NULL;
		}

		Ultimap2Ranges = ExAllocatePool(NonPagedPool, rangeCount*sizeof(URANGE));

		for (i = 0; i < rangeCount; i++)
			Ultimap2Ranges[i] = Ranges[i];

		Ultimap2RangeCount = rangeCount;

	}
	else
		Ultimap2RangeCount = 0;


	//get the EProcess and CR3 for this PID
	if (PID)
	{
		if (PsLookupProcessByProcessId((PVOID)PID, &CurrentTarget) == STATUS_SUCCESS)
		{
			//todo add specific windows version checks and hardcode offsets/ or use scans
			if (getCR3() & 0xfff)
			{
				DbgPrint("Split kernel/usermode pages\n");
				//uses supervisor/usermode pagemaps			
				CurrentCR3 = *(UINT64 *)((UINT_PTR)CurrentTarget + 0x278);
				if ((CurrentCR3 & 0xfffffffffffff000ULL) == 0)
				{
					DbgPrint("No usermode CR3\n");
					CurrentCR3 = *(UINT64 *)((UINT_PTR)CurrentTarget + 0x28);
				}

				DbgPrint("CurrentCR3=%llx\n", CurrentCR3);
			}
			else
			{
				KAPC_STATE apc_state;
				RtlZeroMemory(&apc_state, sizeof(apc_state));
				__try
				{
					KeStackAttachProcess((PVOID)CurrentTarget, &apc_state);
					CurrentCR3 = getCR3();
					KeUnstackDetachProcess(&apc_state);
				}
				__except (1)
				{
					DbgPrint("Failure getting CR3 for this process");
					return;
				}
			}
		}
		else
		{
			DbgPrint("Failure getting the EProcess for pid %d", PID);
			return;
		}
	}
	else
	{
		CurrentTarget = 0;
		CurrentCR3 = 0;
	}

	DbgPrint("CurrentCR3=%llx\n", CurrentCR3);





	if ((PsSuspendProcess == NULL) || (PsResumeProcess == NULL))
	{
		DbgPrint("No Suspend/Resume support");
		return;
	}
		

	KeInitializeDpc(&RTID_DPC, RTIT_DPC_Handler, NULL);
	
	KeInitializeEvent(&FlushData, SynchronizationEvent, FALSE);
	KeInitializeEvent(&SuspendEvent, SynchronizationEvent, FALSE);
	KeInitializeMutex(&SuspendMutex, 0);


	Ultimap2CpuCount = KeQueryMaximumProcessorCount();

	PInfo = ExAllocatePool(NonPagedPool, Ultimap2CpuCount*sizeof(PProcessorInfo));
	Ultimap2_DataReady = ExAllocatePool(NonPagedPool, Ultimap2CpuCount*sizeof(PVOID));

	if (PInfo == NULL)
	{
		DbgPrint("PInfo alloc failed");
		return;
	}

	if (Ultimap2_DataReady == NULL)
	{
		DbgPrint("Ultimap2_DataReady alloc failed");
		return;
	}

	for (i = 0; i < Ultimap2CpuCount; i++)
	{
		PInfo[i] = ExAllocatePool(NonPagedPool, sizeof(ProcessorInfo));
		RtlZeroMemory(PInfo[i], sizeof(ProcessorInfo));
		
		KeInitializeEvent(&PInfo[i]->InitiateSave, SynchronizationEvent, FALSE);
		KeInitializeEvent(&PInfo[i]->Buffer2ReadyForSwap, NotificationEvent, TRUE);

		setupToPA(&PInfo[i]->ToPAHeader, &PInfo[i]->ToPABuffer, &PInfo[i]->ToPABufferMDL, &PInfo[i]->ToPALookupTable, BufferSize, NoPMI);
		setupToPA(&PInfo[i]->ToPAHeader2, &PInfo[i]->ToPABuffer2, &PInfo[i]->ToPABuffer2MDL, &PInfo[i]->ToPALookupTable2, BufferSize, NoPMI);

		DbgPrint("cpu %d:", i);
		DbgPrint("ToPAHeader=%p ToPABuffer=%p Size=%x", PInfo[i]->ToPAHeader, PInfo[i]->ToPABuffer, BufferSize);
		DbgPrint("ToPAHeader2=%p ToPABuffer2=%p Size=%x", PInfo[i]->ToPAHeader2, PInfo[i]->ToPABuffer2, BufferSize);


		KeInitializeEvent(&PInfo[i]->DataReady, SynchronizationEvent, FALSE);
		KeInitializeEvent(&PInfo[i]->DataProcessed, SynchronizationEvent, FALSE);

		KeInitializeEvent(&PInfo[i]->FileAccess, SynchronizationEvent, TRUE);

		Ultimap2_DataReady[i] = &PInfo[i]->DataReady;

		KeInitializeDpc(&PInfo[i]->OwnDPC, SwitchToPABuffer, NULL);
		KeSetTargetProcessorDpc(&PInfo[i]->OwnDPC, (CCHAR)i);
	}
	
	UltimapActive = TRUE;
	ultimapEnabled = TRUE;

	PsCreateSystemThread(&SuspendThreadHandle, 0, NULL, 0, NULL, suspendThread, NULL);	
	PsCreateSystemThread(&Ultimap2Handle, 0, NULL, 0, NULL, bufferWriterThread, NULL);

	for (i = 0; i < Ultimap2CpuCount; i++)
		PsCreateSystemThread(&PInfo[i]->WriterThreadHandle, 0, NULL, 0, NULL, WriteThreadForSpecificCPU, (PVOID)i); 

	if ((NoPMI == FALSE) && (RegisteredProfilerInterruptHandler == FALSE))
	{

		DbgPrint("Registering PMI handler\n");

		pperfmon_hook2 = (void *)PMI;

		r = HalSetSystemInformation(HalProfileSourceInterruptHandler, sizeof(PVOID*), &pperfmon_hook2); //hook the perfmon interrupt
		if (r == STATUS_SUCCESS)
			RegisteredProfilerInterruptHandler = TRUE;

		DbgPrint("HalSetSystemInformation returned %x\n", r);

		if (r != STATUS_SUCCESS)
			DbgPrint("Failure hooking the permon interrupt.  Ultimap2 will not be able to use interrupts until you reboot (This can happen when the perfmon interrupt is hooked more than once. It has no restore/undo hook)\n");
	}



	forEachCpu(ultimap2_setup_dpc, NULL, NULL, NULL, NULL);
	
}

void UnregisterUltimapPMI()
{
	NTSTATUS r;
	DbgPrint("UnregisterUltimapPMI()\n");
	if (RegisteredProfilerInterruptHandler)
	{		
	
		pperfmon_hook2 = NULL;
		r = HalSetSystemInformation(HalProfileSourceInterruptHandler, sizeof(PVOID*), &pperfmon_hook2); 
		DbgPrint("1: HalSetSystemInformation to disable returned %x\n", r);

		if (r == STATUS_SUCCESS)
			return;

		r = HalSetSystemInformation(HalProfileSourceInterruptHandler, sizeof(PVOID*), &clear); //unhook the perfmon interrupt
		DbgPrint("2: HalSetSystemInformation to disable returned %x\n", r);

		if (r == STATUS_SUCCESS)
			return;


		r = HalSetSystemInformation(HalProfileSourceInterruptHandler, sizeof(PVOID*), 0);
		DbgPrint("3: HalSetSystemInformation to disable returned %x\n", r);
		
	}
	else
		DbgPrint("UnregisterUltimapPMI() not needed\n");
}

void DisableUltimap2(void)
{
	int i;

	DbgPrint("-------------------->DisableUltimap2<------------------");

	if (!ultimapEnabled)
		return;

	DbgPrint("-------------------->DisableUltimap2:Stage 1<------------------");
	
	forEachCpuAsync(ultimap2_disable_dpc, NULL, NULL, NULL, NULL);

	
	UltimapActive = FALSE;
	
	if (SuspendThreadHandle)
	{
		DbgPrint("Waiting for SuspendThreadHandle");
		KeSetEvent(&SuspendEvent, 0, FALSE);
		ZwWaitForSingleObject(SuspendThreadHandle, FALSE, NULL);
		ZwClose(SuspendThreadHandle);
		SuspendThreadHandle = NULL;
	}

	if (PInfo)
	{
		for (i = 0; i < Ultimap2CpuCount; i++)
		{
			KeSetEvent(&PInfo[i]->DataProcessed, 0, FALSE);
			KeSetEvent(&PInfo[i]->DataReady, 0, FALSE);
		}
	}

	if (Ultimap2Handle)
	{
		DbgPrint("Waiting for Ultimap2Handle");
		KeSetEvent(&FlushData, 0, FALSE);
		ZwWaitForSingleObject(Ultimap2Handle, FALSE, NULL);
		ZwClose(Ultimap2Handle);
		Ultimap2Handle = NULL;
	}

	
	if (PInfo)
	{
		DbgPrint("going to deal with the PInfo data");
		for (i = 0; i < Ultimap2CpuCount; i++)
		{
			if (PInfo[i])
			{
				PToPA_LOOKUP li;


				KeSetEvent(&PInfo[i]->Buffer2ReadyForSwap, 0, FALSE);
				KeSetEvent(&PInfo[i]->InitiateSave, 0, FALSE);


				DbgPrint("Waiting for WriterThreadHandle[%d]",i);
				ZwWaitForSingleObject(PInfo[i]->WriterThreadHandle, FALSE, NULL);
				ZwClose(PInfo[i]->WriterThreadHandle);
				PInfo[i]->WriterThreadHandle = NULL;

				if (PInfo[i]->ToPABufferMDL)
				{
					IoFreeMdl(PInfo[i]->ToPABufferMDL);
					PInfo[i]->ToPABufferMDL = NULL;
				}

				if (PInfo[i]->ToPABuffer)
				{
					if (singleToPASystem)
						MmFreeContiguousMemory(PInfo[i]->ToPABuffer);
					else
						ExFreePool(PInfo[i]->ToPABuffer);
					PInfo[i]->ToPABuffer = NULL;
				}

				if (PInfo[i]->ToPABuffer2MDL)
				{
					IoFreeMdl(PInfo[i]->ToPABuffer2MDL);
					PInfo[i]->ToPABufferMDL = NULL;
				}
				
				if (PInfo[i]->ToPABuffer2)
				{
					if (singleToPASystem)
						MmFreeContiguousMemory(PInfo[i]->ToPABuffer2);
					else
						ExFreePool(PInfo[i]->ToPABuffer2);

					PInfo[i]->ToPABuffer2 = NULL;
				}

				if (PInfo[i]->ToPAHeader)
				{
					ExFreePool(PInfo[i]->ToPAHeader);
					PInfo[i]->ToPAHeader = NULL;
				}

				if (PInfo[i]->ToPAHeader2)
				{
					ExFreePool(PInfo[i]->ToPAHeader2);
					PInfo[i]->ToPAHeader2 = NULL;
				}

				while (li = RtlGetElementGenericTable(PInfo[i]->ToPALookupTable, 0))
					RtlDeleteElementGenericTable(PInfo[i]->ToPALookupTable, li);					
					
				ExFreePool(PInfo[i]->ToPALookupTable);
				PInfo[i]->ToPALookupTable = NULL;

				while (li = RtlGetElementGenericTable(PInfo[i]->ToPALookupTable2, 0))
					RtlDeleteElementGenericTable(PInfo[i]->ToPALookupTable2, li);

				ExFreePool(PInfo[i]->ToPALookupTable2);
				PInfo[i]->ToPALookupTable2 = NULL;
		

				ExFreePool(PInfo[i]);
				PInfo[i] = NULL;
			}

			
		}

		ExFreePool(PInfo);
		ExFreePool(Ultimap2_DataReady);
		
		PInfo = NULL;

		DbgPrint("Finished terminating ultimap2");
	}

	if (Ultimap2Ranges)
	{
		ExFreePool(Ultimap2Ranges);
		Ultimap2Ranges = NULL;

		Ultimap2RangeCount = 0;
	}

	DbgPrint("-------------------->DisableUltimap2:Finish<------------------");


}
