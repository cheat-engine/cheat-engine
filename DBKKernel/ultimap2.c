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
BOOLEAN UltimapActive = FALSE;
volatile BOOLEAN isSuspended = FALSE;
KSPIN_LOCK PauseLock;
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


void WriteThreadForSpecificCPU(PVOID StartContext)
{
	PProcessorInfo pi = PInfo[(int)StartContext];

	KeSetSystemAffinityThread((KAFFINITY)(1 << (int)StartContext));
	
	while (UltimapActive)
	{
		NTSTATUS wr = KeWaitForSingleObject(&pi->InitiateSave, Executive, KernelMode, FALSE, NULL);
		if (wr == STATUS_SUCCESS)
		{
			UINT64 Size;
			ToPA_LOOKUP tl;
			PToPA_LOOKUP result;

			DbgPrint("%d: writing buffer", (int)StartContext);

			//figure out the size
			tl.PhysicalAddress = pi->CurrentSaveOutputBase;
			result = RtlLookupElementGenericTable(pi->ToPALookupTable2, &tl);

			if (result)
			{
				//write...
				Size = ((tl.index * 511) + ((pi->CurrentSaveOutputBase & 0xffffffff) >> 7)) * 4096 + (pi->CurrentSaveOutputBase >> 32);

				DbgPrint("%d: Writing %x bytes\n", (int)StartContext, Size);


			}
			else
				DbgPrint("Unexpected physical address while writing results for cpu %d  (%p)", (int)StartContext, pi->CurrentSaveOutputBase);
			

			KeSetEvent(&pi->Buffer2ReadyForSwap, 0, FALSE);
		}		
	}
}

void SwitchToPABuffer(struct _KDPC *Dpc, PVOID DeferredContext, PVOID SystemArgument1, PVOID SystemArgument2)
/*
DPC routine that switches the Buffer pointer and marks buffer2 that it's ready for data saving
*/
{
	//write the contents of the current cpu buffer
	PProcessorInfo pi = PInfo[KeGetCurrentProcessorNumber()];
	if (pi)
	{		
		UINT64 CTL = readMSR(IA32_RTIT_CTL);

		__writemsr(IA32_RTIT_CTL, 0); //disable packet generation
		__writemsr(IA32_RTIT_STATUS, 0);

		//switch the pointer to the secondary buffers
		KeClearEvent(&pi->Buffer2ReadyForSwap);

		InterlockedExchangePointer(&pi->ToPABuffer, pi->ToPABuffer2);
		InterlockedExchangePointer(&pi->ToPAHeader, pi->ToPAHeader2);
		InterlockedExchangePointer(&pi->ToPALookupTable, pi->ToPALookupTable2);
		

		//lookup which entry it's pointing at
		pi->CurrentSaveOutputBase = __readmsr(IA32_RTIT_OUTPUT_BASE);
		pi->CurrentSaveOutputMask = __readmsr(IA32_RTIT_OUTPUT_MASK_PTRS);


		KeSetEvent(&pi->InitiateSave,0,FALSE);

		pi->Interrupted = FALSE;

		//reactivate packet generation
		__writemsr(IA32_RTIT_OUTPUT_BASE, MmGetPhysicalAddress(pi->ToPABuffer).QuadPart);
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

			KeInitializeDpc(&pi->OwnDPC, SwitchToPABuffer, NULL);
			KeSetTargetProcessorDpc(&pi->OwnDPC, i);				
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
	
	while (UltimapActive)
	{
		Timeout.QuadPart = -100000000LL; //100 milliseconds
		wr = KeWaitForSingleObject(&FlushData, Executive, KernelMode, FALSE, &Timeout);

		if ((wr == STATUS_SUCCESS) || (wr==STATUS_TIMEOUT))
		{

			if (isSuspended) //the filled cpu's must take preference
			{
				unsigned int i;
				KIRQL old;
				BOOL found = TRUE;

				//first flush the CPU's that complained their buffers are full
				while (found)
				{
					WaitForWriteToFinishAndSwapWriteBuffers(TRUE);

					KeAcquireSpinLock(&PauseLock, &old);

					//check if no interrupt has been triggered while this was busy ('could' happen as useless info like core ratio is still recorded)
					found = FALSE;
					for (i = 0; i < KeQueryMaximumProcessorCount(); i++)
					{
						if ((PInfo[i]->ToPABuffer2) && (PInfo[i]->Interrupted))
						{
							found = TRUE;
							break;
						}
					}
					KeReleaseSpinLock(&PauseLock, old);
				}

				//no interrupt for a while, resume the process

				
				PsResumeProcess(CurrentTarget);
				isSuspended = FALSE;	
				

			}

			//wait till the previous buffers are done writing
			WaitForWriteToFinishAndSwapWriteBuffers(FALSE);

			//an interrupt could have fired while WaitForWriteToFinishAndSwapWriteBuffers was busy, pausing the process. If that happened, then the next KeWaitForSingleObject will exit instantly due to it being signaled 


		}
		
	}
}




void RTIT_DPC_Handler(__in struct _KDPC *Dpc, __in_opt PVOID DeferredContext, __in_opt PVOID SystemArgument1,__in_opt PVOID SystemArgument2)
{
	//Acquire a spinlock so only one cpu can suspend it at a time	
	KeAcquireSpinLockAtDpcLevel(&PauseLock);
	
	if (!isSuspended)
	{
		PsSuspendProcess(CurrentTarget);
		isSuspended = TRUE;
	}
	KeReleaseSpinLockFromDpcLevel(&PauseLock);
	
	//Signal the bufferWriterThread
	KeSetEvent(&FlushData, 0, FALSE);
}



void PMI(__in struct _KINTERRUPT *Interrupt, __in PVOID ServiceContext)
{
	//check if caused by me, if so defer to dpc
	DbgPrint("PMI");
	if ((__readmsr(IA32_PERF_GLOBAL_STATUS) >> 55) & 1)
	{		
		DbgPrint("caused by me");	
		__writemsr(IA32_PERF_GLOBAL_STATUS, (UINT64)1 << 55); //clear ToPA full status

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

void ultimap2_setup_dpc(struct _KDPC *Dpc, PVOID DeferredContext, PVOID SystemArgument1, PVOID SystemArgument2)
{
	RTIT_CTL ctl;
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
	
	__writemsr(IA32_RTIT_OUTPUT_BASE, MmGetPhysicalAddress(PInfo[KeGetCurrentProcessorNumber()]->ToPABuffer).QuadPart);
	__writemsr(IA32_RTIT_OUTPUT_MASK_PTRS, 0);


	__writemsr(IA32_RTIT_CR3_MATCH, CurrentCR3);
	__writemsr(IA32_RTIT_STATUS, 0);
	__writemsr(IA32_RTIT_CTL, ctl.Value);
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
	if (FirstStruct->PhysicalAddress < SecondStruct->PhysicalAddress)
		return GenericLessThan;
	else
		return GenericGreaterThan;
}

PVOID NTAPI ToPAAlloc(__in struct _RTL_GENERIC_TABLE *Table, __in CLONG ByteSize)
{
	return ExAllocatePoolWithTag(NonPagedPool, ByteSize, 0);
}

VOID NTAPI ToPaDealloc(__in struct _RTL_GENERIC_TABLE *Table, __in __drv_freesMem(Mem) __post_invalid PVOID Buffer)
{
	ExFreePoolWithTag(Buffer, 0);
}

void* setupToPA(PToPA_ENTRY *Header, PVOID *OutputBuffer, PRTL_GENERIC_TABLE *gt, ULONG BufferSize)
{
	ToPA_LOOKUP tl;
	PToPA_ENTRY r;
	UINT_PTR Output, Stop;
	ULONG ToPAIndex = 0;
	int i;
	
	*OutputBuffer = ExAllocatePoolWithTag(NonPagedPool, BufferSize, 0);
	r = ExAllocatePoolWithTag(NonPagedPool, BufferSize, 0);
	*Header = r;

	*gt=ExAllocatePoolWithTag(NonPagedPool, sizeof(RTL_GENERIC_TABLE), 0);
	RtlInitializeGenericTable(*gt, ToPACompare, ToPAAlloc, ToPaDealloc, NULL);


	tl.index = 0;
	tl.PhysicalAddress = MmGetPhysicalAddress(&r[0]).QuadPart;
	RtlInsertElementGenericTable(*gt, &tl, sizeof(tl), NULL);

	Output = (UINT_PTR)*OutputBuffer;
	Stop = Output+BufferSize;
	

	while (Output<Stop)
	{
		//fill in the topa entries pointing to eachother
		if (ToPAIndex % 511 == 0)
		{
			//point it to the next ToPA table
			r[ToPAIndex].Value = MmGetPhysicalAddress(&r[ToPAIndex+1]).QuadPart;
			r[ToPAIndex].Bits.END = 1;

			tl.index = ToPAIndex + 1;
			tl.PhysicalAddress = MmGetPhysicalAddress(&r[ToPAIndex + 1]).QuadPart;
			RtlInsertElementGenericTable(*gt, &tl, sizeof(tl), NULL);
		}
		else
		{
			r[ToPAIndex].Value = (UINT_PTR)MmGetPhysicalAddress(Output).QuadPart;
			Output += 4096;
		}

		ToPAIndex++;
	}

	ToPAIndex--;
	r[ToPAIndex].Bits.STOP = 1;
	i = (ToPAIndex * 90) / 100;
	if (i % 511 == 0)
		i--;

	r[i].Bits.INT = 1; //Interrupt after 90 percent

	return (void *)r;
}

void SetupUltimap2(PEPROCESS target, UINT64 cr3, UINT32 BufferSize)
{
	//for each cpu setup tracing
	//add the PMI interupt
	int i;
	NTSTATUS r;
	UNICODE_STRING s;
	PVOID x;
	CurrentTarget = target;
	CurrentCR3 = cr3;


	RtlInitUnicodeString(&s, L"PsSuspendProcess");
	PsSuspendProcess = MmGetSystemRoutineAddress(&s);

	RtlInitUnicodeString(&s, L"PsResumeProcess");
	PsResumeProcess = MmGetSystemRoutineAddress(&s);

	if ((PsSuspendProcess == NULL) || (PsResumeProcess == NULL)) return;	
		

	KeInitializeDpc(&RTID_DPC, RTIT_DPC_Handler, NULL);
	KeInitializeSpinLock(&PauseLock);

	KeInitializeEvent(&FlushData, SynchronizationEvent, FALSE);


	PInfo = ExAllocatePoolWithTag(NonPagedPool, KeQueryMaximumProcessorCount()*sizeof(PProcessorInfo),0);

	for (i = 0; i < KeQueryMaximumProcessorCount(); i++)
	{
		PInfo[i] = ExAllocatePoolWithTag(NonPagedPool, sizeof(ProcessorInfo),0);
		RtlZeroMemory(PInfo[i], KeQueryMaximumProcessorCount()*sizeof(PProcessorInfo));

		KeInitializeEvent(&PInfo[i]->InitiateSave, SynchronizationEvent, FALSE);
		KeInitializeEvent(&PInfo[i]->Buffer2ReadyForSwap, NotificationEvent, TRUE);

		setupToPA(PInfo[i]->ToPAHeader, PInfo[i]->ToPABuffer, PInfo[i]->ToPALookupTable,  BufferSize);
		setupToPA(PInfo[i]->ToPAHeader2, PInfo[i]->ToPABuffer2, PInfo[i]->ToPALookupTable2, BufferSize);
	}
	
	UltimapActive = TRUE;
	PsCreateSystemThread(&Ultimap2Handle, 0, NULL, 0, NULL, bufferWriterThread, NULL);

	for (i = 0; i < KeQueryMaximumProcessorCount(); i++)
		PsCreateSystemThread(PInfo[i]->WriterThreadHandle, 0, NULL, 0, NULL, WriteThreadForSpecificCPU, (PVOID)i); 
	
	r = HalSetSystemInformation(HalProfileSourceInterruptHandler, sizeof(PVOID*), &pperfmon_hook2); //hook the perfmon interrupt

	forEachCpu(ultimap2_setup_dpc, NULL, (PVOID)BufferSize, NULL);
}
