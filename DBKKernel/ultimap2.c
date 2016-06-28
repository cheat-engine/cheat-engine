#include <ntddk.h>
#include <wdm.h>
#include <windef.h>

#include "DBKFunc.h"
#include "ultimap.h"
#include "ultimap2.h"


typedef ULONG(NTUSERSETWINDOWSHOOKEX)(
	IN HANDLE hmod,
	IN PUNICODE_STRING pstrLib OPTIONAL,
	IN DWORD idThread,
	IN int nFilterType,
	IN PVOID pfnFilterProc,
	IN DWORD dwFlags
	);
NTUSERSETWINDOWSHOOKEX OldNtUserSetWindowsHookEx;


typedef NTSTATUS(*PSSUSPENDPROCESS)(PEPROCESS p);


PSSUSPENDPROCESS PsSuspendProcess;
PSSUSPENDPROCESS PsResumeProcess;
KDPC RTID_DPC;

PEPROCESS CurrentTarget;
UINT64 CurrentCR3;
HANDLE Ultimap2Handle;
volatile BOOLEAN UltimapActive = FALSE;
volatile BOOLEAN isSuspended = FALSE;
KEVENT FlushData;


typedef struct
{	
	PToPA_ENTRY ToPAHeader;
	PToPA_ENTRY ToPAHeader2;

	PVOID ToPABuffer;
	PVOID ToPABuffer2;

	PRTL_GENERIC_TABLE ToPALookupTable;
	PRTL_GENERIC_TABLE ToPALookupTable2;

	KEVENT Buffer2ReadyForSwap;
	KEVENT InitiateSave;
	UINT64 CurrentSaveOutputBase;
	UINT64 CurrentSaveOutputMask;


	KDPC OwnDPC;
	HANDLE WriterThreadHandle;
	volatile BOOL Interrupted;
}  ProcessorInfo, *PProcessorInfo;
volatile PProcessorInfo *PInfo;


KMUTEX SuspendMutex;
KEVENT SuspendEvent;
HANDLE SuspendThreadHandle;
volatile int suspendCount;

void suspendThread(PVOID StartContext)
{
	NTSTATUS wr;
	while (UltimapActive)
	{
		KIRQL old;
		wr = KeWaitForSingleObject(&SuspendEvent, Executive, KernelMode, FALSE, NULL);
		if (!UltimapActive) return;

		DbgPrint("suspendThread event triggered");
		KeWaitForSingleObject(&SuspendMutex, Executive, KernelMode, FALSE, NULL);
		if (!isSuspended)
		{
			PsSuspendProcess(CurrentTarget);
			isSuspended = TRUE;
		}
		KeReleaseMutex(&SuspendMutex, FALSE);
	}

}

void WriteThreadForSpecificCPU(PVOID StartContext)
{
	PProcessorInfo pi = PInfo[(int)StartContext];

	//DbgPrint("WriteThreadForSpecificCPU %d alive", (int)StartContext);

	
	KeSetSystemAffinityThread((KAFFINITY)(1 << (int)StartContext));
	
	while (UltimapActive)
	{
		NTSTATUS wr = KeWaitForSingleObject(&pi->InitiateSave, Executive, KernelMode, FALSE, NULL);
		//DbgPrint("WriteThreadForSpecificCPU %d:  wr=%x", (int)StartContext, wr);
		if (!UltimapActive) return;

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
				//DbgPrint("%d: result->index=%d", (int)StartContext, result->index);
				Size = ((result->index * 511) + ((pi->CurrentSaveOutputMask & 0xffffffff) >> 7)) * 4096 + (pi->CurrentSaveOutputMask >> 32);

				//DbgPrint("%d: Writing %x bytes\n", (int)StartContext, Size);


			}
			else
				DbgPrint("Unexpected physical address while writing results for cpu %d  (%p)", (int)StartContext, pi->CurrentSaveOutputBase);
			

			KeSetEvent(&pi->Buffer2ReadyForSwap, 0, FALSE);
		}		
	}

	KeSetSystemAffinityThread(KeQueryActiveProcessors());
}

void SwitchToPABuffer(struct _KDPC *Dpc, PVOID DeferredContext, PVOID SystemArgument1, PVOID SystemArgument2)
/*
DPC routine that switches the Buffer pointer and marks buffer2 that it's ready for data saving
*/
{
	//write the contents of the current cpu buffer
	PProcessorInfo pi = PInfo[KeGetCurrentProcessorNumber()];

	//DbgPrint("SwitchToPABuffer for cpu %d\n", KeGetCurrentProcessorNumber());

	if (pi)
	{		
		UINT64 CTL = readMSR(IA32_RTIT_CTL);
		UINT64 Status = readMSR(IA32_RTIT_STATUS);
		PVOID temp;

		if ((Status >> 5) & 1)
			DbgPrint("DATA LOSS");

		if ((Status >> 4) & 1)
			DbgPrint("ALL LOST");

		__writemsr(IA32_RTIT_CTL, 0); //disable packet generation
		__writemsr(IA32_RTIT_STATUS, 0);

				//switch the pointer to the secondary buffers
		KeClearEvent(&pi->Buffer2ReadyForSwap);

		temp = pi->ToPABuffer;
		pi->ToPABuffer = pi->ToPABuffer2; 
		pi->ToPABuffer2 = temp;

		temp = pi->ToPAHeader;
		pi->ToPAHeader = pi->ToPAHeader2;
		pi->ToPAHeader2 = temp;

		temp = pi->ToPALookupTable;
		pi->ToPALookupTable = pi->ToPALookupTable2;
		pi->ToPALookupTable2 = temp;

		//lookup which entry it's pointing at
		pi->CurrentSaveOutputBase = __readmsr(IA32_RTIT_OUTPUT_BASE);
		pi->CurrentSaveOutputMask = __readmsr(IA32_RTIT_OUTPUT_MASK_PTRS);

		KeSetEvent(&pi->InitiateSave,0,FALSE);

		pi->Interrupted = FALSE;

		//reactivate packet generation
		__writemsr(IA32_RTIT_OUTPUT_BASE, MmGetPhysicalAddress(pi->ToPAHeader).QuadPart);
		__writemsr(IA32_RTIT_OUTPUT_MASK_PTRS, 0);

		__writemsr(IA32_RTIT_CTL, CTL);
	}
}

void WaitForWriteToFinishAndSwapWriteBuffers(BOOL interruptedOnly)
{
    unsigned int i;
	for (i = 0; i < KeQueryMaximumProcessorCount(); i++)
	{
		PProcessorInfo pi = PInfo[i];
		if ((pi->ToPABuffer2) && ((pi->Interrupted) || (!interruptedOnly)))
		{
			KeWaitForSingleObject(&pi->Buffer2ReadyForSwap, Executive, KernelMode, FALSE, NULL);
			if (!UltimapActive) return;

			KeInitializeDpc(&pi->OwnDPC, SwitchToPABuffer, NULL);
			KeSetTargetProcessorDpc(&pi->OwnDPC, i);

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
		Timeout.QuadPart = -10000LL; //-100000000LL = 10 seconds   -1000000LL= 0.1 second
		wr = KeWaitForSingleObject(&FlushData, Executive, KernelMode, FALSE, &Timeout);
		//wr = KeWaitForSingleObject(&FlushData, Executive, KernelMode, FALSE, NULL);

		//DbgPrint("bufferWriterThread: Alive (wr==%x)", wr);
		if (!UltimapActive) return;

		if ((wr == STATUS_SUCCESS) || (wr == STATUS_TIMEOUT))
		{
			//if ((wr == STATUS_SUCCESS) && (!isSuspended))
			{
				//called by a dpc
				
				KeWaitForSingleObject(&SuspendMutex, Executive, KernelMode, FALSE, NULL);
				if (!isSuspended)
				{
					PsSuspendProcess(CurrentTarget);
					isSuspended = TRUE;
				}
				KeReleaseMutex(&SuspendMutex, FALSE);
			}			

			if (wr == STATUS_SUCCESS) //the filled cpu's must take preference
			{
				unsigned int i;
				BOOL found = TRUE;

				//DbgPrint("bufferWriterThread: Suspended");


				//first flush the CPU's that complained their buffers are full
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
							found = TRUE;
							break;
						}
					}

					if (!found)
					{
						KeWaitForSingleObject(&SuspendMutex, Executive, KernelMode, FALSE, NULL);
						if (isSuspended)
						{
							PsResumeProcess(CurrentTarget);
							isSuspended = FALSE;
						}
						KeReleaseMutex(&SuspendMutex, FALSE);
					}

				}

				//no interrupt for a while, resume the process





			}

			//wait till the previous buffers are done writing
			WaitForWriteToFinishAndSwapWriteBuffers(FALSE);

			KeWaitForSingleObject(&SuspendMutex, Executive, KernelMode, FALSE, NULL);
			if (isSuspended)
			{
				PsResumeProcess(CurrentTarget);
				isSuspended = FALSE;
			}
			KeReleaseMutex(&SuspendMutex, FALSE);
			//an interrupt could have fired while WaitForWriteToFinishAndSwapWriteBuffers was busy, pausing the process. If that happened, then the next KeWaitForSingleObject will exit instantly due to it being signaled 
		}
		else
			DbgPrint("Unexpected wait result");
		
	}
}




void RTIT_DPC_Handler(__in struct _KDPC *Dpc, __in_opt PVOID DeferredContext, __in_opt PVOID SystemArgument1,__in_opt PVOID SystemArgument2)
{
	//Signal the bufferWriterThread
	//KeSetEvent(&SuspendEvent, 0, FALSE);
	KeSetEvent(&FlushData, 0, FALSE);
}



void PMI(__in struct _KINTERRUPT *Interrupt, __in PVOID ServiceContext)
{
	//check if caused by me, if so defer to dpc
	//DbgPrint("PMI");
	if ((__readmsr(IA32_PERF_GLOBAL_STATUS) >> 55) & 1)
	{		
		//DbgPrint("caused by me");	
		__writemsr(IA32_PERF_GLOBAL_OVF_CTRL, (UINT64)1 << 55); //clear ToPA full status

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

void *pperfmon_hook2 = PMI;


void ultimap2_disable_dpc(struct _KDPC *Dpc, PVOID DeferredContext, PVOID SystemArgument1, PVOID SystemArgument2)
{
	__writemsr(IA32_RTIT_CTL, 0);
	__writemsr(IA32_RTIT_STATUS, 0);
	__writemsr(IA32_RTIT_CR3_MATCH, 0);
	__writemsr(IA32_RTIT_OUTPUT_BASE, 0);
	__writemsr(IA32_RTIT_OUTPUT_MASK_PTRS, 0);
}

void ultimap2_setup_dpc(struct _KDPC *Dpc, PVOID DeferredContext, PVOID SystemArgument1, PVOID SystemArgument2)
{
	RTIT_CTL ctl;
	int i;

	ctl.Value = __readmsr(IA32_RTIT_CTL);
	
	ctl.Bits.TraceEn = 1;
	ctl.Bits.OS = 0;
	ctl.Bits.USER = 1;
	ctl.Bits.CR3Filter = 1;
	ctl.Bits.ToPA = 1;
	ctl.Bits.TSCEn = 0;
	ctl.Bits.DisRETC = 0;
	ctl.Bits.BranchEn = 1;

	if (PInfo[KeGetCurrentProcessorNumber()]->ToPABuffer == NULL)
	{
		DbgPrint("ToPA for cpu %d not setup\n", KeGetCurrentProcessorNumber());
		return;
	}
	
	__try
	{
		i = 0;

		__writemsr(IA32_RTIT_OUTPUT_BASE, MmGetPhysicalAddress(PInfo[KeGetCurrentProcessorNumber()]->ToPAHeader).QuadPart);
		i = 1;
		__writemsr(IA32_RTIT_OUTPUT_MASK_PTRS, 0);
		i = 2;


		__writemsr(IA32_RTIT_CR3_MATCH, CurrentCR3);
		i = 3;
		__writemsr(IA32_RTIT_STATUS, 0);
		i = 4;
		__writemsr(IA32_RTIT_CTL, ctl.Value);
		i = 5;

		DbgPrint("Setup for cpu %d succesful", KeGetCurrentProcessorNumber());
	}
	__except (1)
	{
		DbgPrint("Error in ultimap2_setup_dpc.  i=%d",i);
		DbgPrint("ctl.Value=%p\n", ctl.Value);
		DbgPrint("CR3=%p\n", CurrentCR3);
		DbgPrint("OutputBase=%p", __readmsr(IA32_RTIT_OUTPUT_BASE));
	}
	
}

int getToPAHeaderCount(ULONG BufferSize)
{
	return 1 + (BufferSize / 4096) / 511;
}

int getToPAHeaderSize(ULONG BufferSize)
{
	//511 entries per ToPA header (4096*511=2093056 bytes per ToPA header)
	//BufferSize / 2093056 = Number of ToPA headers needed
	return getToPAHeaderCount(BufferSize) * 4096;
}

RTL_GENERIC_COMPARE_RESULTS NTAPI ToPACompare(__in struct _RTL_GENERIC_TABLE *Table, __in PToPA_LOOKUP FirstStruct, __in PToPA_LOOKUP SecondStruct)
{
	//DbgPrint("Comparing %p with %p", FirstStruct->PhysicalAddress, FirstStruct->PhysicalAddress);

	if (FirstStruct->PhysicalAddress == FirstStruct->PhysicalAddress)
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
	return ExAllocatePoolWithTag(NonPagedPool, ByteSize, 0);
}

VOID NTAPI ToPADealloc(__in struct _RTL_GENERIC_TABLE *Table, __in __drv_freesMem(Mem) __post_invalid PVOID Buffer)
{
	ExFreePoolWithTag(Buffer, 0);
}

void* setupToPA(PToPA_ENTRY *Header, PVOID *OutputBuffer, PRTL_GENERIC_TABLE *gt, ULONG BufferSize)
{
	ToPA_LOOKUP tl;
	PToPA_ENTRY r;
	UINT_PTR Output, Stop;
	ULONG ToPAIndex = 0;

	PRTL_GENERIC_TABLE x;
	int i;
	
	*OutputBuffer = ExAllocatePoolWithTag(NonPagedPool, BufferSize, 0);
	if (*OutputBuffer == NULL)
	{
		DbgPrint("setupToPA: Failure allocating output buffer");
		return NULL;
	}

	r = ExAllocatePoolWithTag(NonPagedPool, getToPAHeaderSize(BufferSize), 0);
	if (r == NULL)
	{
		ExFreePoolWithTag(*OutputBuffer,0);
		*OutputBuffer = NULL;
		DbgPrint("setupToPA: Failure allocating header for buffer");
		return NULL;
	}
	

	*Header = r;

	*gt=ExAllocatePoolWithTag(NonPagedPool, sizeof(RTL_GENERIC_TABLE), 0);

	if (*gt == NULL)
	{
		DbgPrint("Failure allocating table");
		ExFreePoolWithTag(*OutputBuffer,0);
		*OutputBuffer = NULL;

		ExFreePoolWithTag(*Header,0);
		*Header = NULL;

		return NULL;
	}

	x = *gt;

	RtlInitializeGenericTable(x, ToPACompare, ToPAAlloc, ToPADealloc, NULL);


	tl.index = 0;
	tl.PhysicalAddress = MmGetPhysicalAddress(&r[0]).QuadPart;
	RtlInsertElementGenericTable(x, &tl, sizeof(tl), NULL);

	Output = (UINT_PTR)*OutputBuffer;
	Stop = Output+BufferSize;
	

	while (Output<Stop)
	{
		//fill in the topa entries pointing to eachother
		if ((ToPAIndex+1) % 512 == 0)
		{
			//point it to the next ToPA table
			r[ToPAIndex].Value = MmGetPhysicalAddress(&r[ToPAIndex+1]).QuadPart;
			r[ToPAIndex].Bits.END = 1;

			tl.index = tl.index++;
			tl.PhysicalAddress = MmGetPhysicalAddress(&r[ToPAIndex + 1]).QuadPart;
			RtlInsertElementGenericTable(x, &tl, sizeof(tl), NULL);
		}
		else
		{
			r[ToPAIndex].Value = (UINT64)MmGetPhysicalAddress((PVOID)Output).QuadPart;
			Output += 4096;
		}

		ToPAIndex++;
	}

	ToPAIndex--;
	r[ToPAIndex].Bits.STOP = 1;
	i = (ToPAIndex * 90) / 100; //90%

	if ((i == ToPAIndex) && (i > 0)) //don't interrupt on the very last entry (if possible)
		i--;

	if ((i>0) && ((i+1) % 512 == 0))
		i--;


	DbgPrint("Interrupt at index %d", i);

	r[i].Bits.INT = 1; //Interrupt after filling this entry

	{
		PToPA_LOOKUP l;
		tl.PhysicalAddress = MmGetPhysicalAddress(&r[0]).QuadPart;
		tl.index = 100;
		l = RtlLookupElementGenericTable(x, &tl);
		if (l)
		{
			DbgPrint("yay %p=%d", l->PhysicalAddress, l->index);

		}
		else
		{
			DbgPrint("fuck");

		}

	}


	return (void *)r;
}

void SetupUltimap2(UINT32 PID, UINT32 BufferSize)
{
	//for each cpu setup tracing
	//add the PMI interupt
	unsigned int i;
	UNICODE_STRING s;
	
	DbgPrint("sizeof(RTIT_CTL)=%d", sizeof(RTIT_CTL));

	//get the EProcess and CR3 for this PID
	if (PsLookupProcessByProcessId((PVOID)PID, &CurrentTarget) == STATUS_SUCCESS)
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
	else
	{
		DbgPrint("Failure getting the EProcess for pid %d", PID);
		return; 
	}


	RtlInitUnicodeString(&s, L"PsSuspendProcess");
	PsSuspendProcess = MmGetSystemRoutineAddress(&s);

	RtlInitUnicodeString(&s, L"PsResumeProcess");
	PsResumeProcess = MmGetSystemRoutineAddress(&s);

	if ((PsSuspendProcess == NULL) || (PsResumeProcess == NULL))
	{
		DbgPrint("No Suspend/Resume support");
		return;
	}
		

	KeInitializeDpc(&RTID_DPC, RTIT_DPC_Handler, NULL);
	
	KeInitializeEvent(&FlushData, SynchronizationEvent, FALSE);
	KeInitializeEvent(&SuspendEvent, SynchronizationEvent, FALSE);
	KeInitializeMutex(&SuspendMutex, 0);



	PInfo = ExAllocatePoolWithTag(NonPagedPool, KeQueryMaximumProcessorCount()*sizeof(PProcessorInfo),0);
	for (i = 0; i < KeQueryMaximumProcessorCount(); i++)
	{
		PInfo[i] = ExAllocatePoolWithTag(NonPagedPool, sizeof(ProcessorInfo),0);
		RtlZeroMemory(PInfo[i], sizeof(ProcessorInfo));
		
		KeInitializeEvent(&PInfo[i]->InitiateSave, SynchronizationEvent, FALSE);
		KeInitializeEvent(&PInfo[i]->Buffer2ReadyForSwap, NotificationEvent, TRUE);
		
		setupToPA(&PInfo[i]->ToPAHeader, &PInfo[i]->ToPABuffer, &PInfo[i]->ToPALookupTable,  BufferSize);
		setupToPA(&PInfo[i]->ToPAHeader2, &PInfo[i]->ToPABuffer2, &PInfo[i]->ToPALookupTable2, BufferSize);

		DbgPrint("cpu %d:", i);
		DbgPrint("ToPAHeader=%p ToPABuffer=%p Size=%x", PInfo[i]->ToPAHeader, PInfo[i]->ToPABuffer, BufferSize);
		DbgPrint("ToPAHeader2=%p ToPABuffer2=%p Size=%x", PInfo[i]->ToPAHeader2, PInfo[i]->ToPABuffer2, BufferSize);

		
	}
	
	UltimapActive = TRUE;

	PsCreateSystemThread(&SuspendThreadHandle, 0, NULL, 0, NULL, suspendThread, NULL);
	
	PsCreateSystemThread(&Ultimap2Handle, 0, NULL, 0, NULL, bufferWriterThread, NULL);

	for (i = 0; i < KeQueryMaximumProcessorCount(); i++)
		PsCreateSystemThread(&PInfo[i]->WriterThreadHandle, 0, NULL, 0, NULL, WriteThreadForSpecificCPU, (PVOID)i); 

	

	{
		NTSTATUS r;
		r=HalSetSystemInformation(HalProfileSourceInterruptHandler, sizeof(PVOID*), &pperfmon_hook2); //hook the perfmon interrupt

		DbgPrint("HalSetSystemInformation returned %x\n", r);

		forEachCpu(ultimap2_setup_dpc, NULL, (PVOID)BufferSize, NULL);
	}
}

void DisableUltimap2(void)
{
	unsigned int i;
	void *clear = NULL;
	
	forEachCpuAsync(ultimap2_disable_dpc, NULL, NULL, NULL);
	HalSetSystemInformation(HalProfileSourceInterruptHandler, sizeof(PVOID*), &clear); //unhook the perfmon interrupt
	//HalSetSystemInformation(HalProfileSourceInterruptHandler, sizeof(PVOID*), 0);

	UltimapActive = FALSE;

	
	if (SuspendThreadHandle)
	{
		KeSetEvent(&SuspendEvent, 0, FALSE);
		ZwWaitForSingleObject(SuspendThreadHandle, FALSE, NULL);
		ZwClose(SuspendThreadHandle);
		SuspendThreadHandle = NULL;
	}

	if (Ultimap2Handle)
	{
		KeSetEvent(&FlushData, 0, FALSE);
		ZwWaitForSingleObject(Ultimap2Handle, FALSE, NULL);
		ZwClose(Ultimap2Handle);
		Ultimap2Handle = NULL;
	}

	if (PInfo)
	{
		for (i = 0; i < KeQueryMaximumProcessorCount(); i++)
		{
			if (PInfo[i])
			{
				PToPA_LOOKUP li;
				KeSetEvent(&PInfo[i]->Buffer2ReadyForSwap, 0, FALSE);
				KeSetEvent(&PInfo[i]->InitiateSave, 0, FALSE);

				ZwWaitForSingleObject(PInfo[i]->WriterThreadHandle, FALSE, NULL);
				ZwClose(PInfo[i]->WriterThreadHandle);
				PInfo[i]->WriterThreadHandle = NULL;

				if (PInfo[i]->ToPABuffer)
				{
					ExFreePoolWithTag(PInfo[i]->ToPABuffer, 0);
					PInfo[i]->ToPABuffer = NULL;
				}
				
				if (PInfo[i]->ToPABuffer2)
				{
					ExFreePoolWithTag(PInfo[i]->ToPABuffer2, 0);
					PInfo[i]->ToPABuffer2 = NULL;
				}

				if (PInfo[i]->ToPAHeader)
				{
					ExFreePoolWithTag(PInfo[i]->ToPAHeader, 0);
					PInfo[i]->ToPAHeader = NULL;
				}

				if (PInfo[i]->ToPAHeader2)
				{
					ExFreePoolWithTag(PInfo[i]->ToPAHeader2, 0);
					PInfo[i]->ToPAHeader2 = NULL;
				}

				while (li = RtlGetElementGenericTable(PInfo[i]->ToPALookupTable, 0))
					RtlDeleteElementGenericTable(PInfo[i]->ToPALookupTable, li);					
					
				ExFreePoolWithTag(PInfo[i]->ToPALookupTable,0);
				PInfo[i]->ToPALookupTable = NULL;

				while (li = RtlGetElementGenericTable(PInfo[i]->ToPALookupTable2, 0))
					RtlDeleteElementGenericTable(PInfo[i]->ToPALookupTable2, li);

				ExFreePoolWithTag(PInfo[i]->ToPALookupTable2, 0);
				PInfo[i]->ToPALookupTable2 = NULL;
		

				ExFreePoolWithTag(PInfo[i], 0);
				PInfo[i] = NULL;
			}

			
		}

		ExFreePoolWithTag(PInfo, 0);
		PInfo = NULL;
	}


}
