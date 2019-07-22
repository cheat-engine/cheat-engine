#pragma warning( disable: 4100 4103 4706)

#include "ntifs.h"
#include "processlist.h"
#include "threads.h"
#include "memscan.h"

#include "ultimap2.h"

PRTL_GENERIC_TABLE InternalProcessList = NULL;

PEPROCESS WatcherProcess = NULL;
BOOLEAN ProcessWatcherOpensHandles = TRUE;



RTL_GENERIC_COMPARE_RESULTS NTAPI ProcessListCompare(__in struct _RTL_GENERIC_TABLE *Table, __in PProcessListData FirstStruct, __in PProcessListData SecondStruct)
{
	//DbgPrint("ProcessListCompate");

	if (FirstStruct->ProcessID  == SecondStruct->ProcessID)
		return GenericEqual;
	else
	{
		if (SecondStruct->ProcessID < FirstStruct->ProcessID)
			return GenericLessThan;
		else
			return GenericGreaterThan;
	}
}

PVOID NTAPI ProcessListAlloc(__in struct _RTL_GENERIC_TABLE *Table, __in CLONG ByteSize)
{
	PVOID r=ExAllocatePool(PagedPool, ByteSize);
	RtlZeroMemory(r, ByteSize);

	//DbgPrint("ProcessListAlloc %d",(int)ByteSize);
	return r;
}

VOID NTAPI ProcessListDealloc(__in struct _RTL_GENERIC_TABLE *Table, __in __drv_freesMem(Mem) __post_invalid PVOID Buffer)
{
	//DbgPrint("ProcessListDealloc");
	ExFreePool(Buffer);
}


VOID GetThreadData(IN PDEVICE_OBJECT  DeviceObject, IN PVOID  Context)
{
	struct ThreadData *tempThreadEntry;
	PETHREAD selectedthread;
	HANDLE tid;
	LARGE_INTEGER Timeout;
	PKAPC AP;
	tempThreadEntry=Context;
	

	DbgPrint("Gathering PEThread thread\n");

	Timeout.QuadPart = -1;
	KeDelayExecutionThread(KernelMode, TRUE, &Timeout);

	selectedthread=NULL;

	if (ExAcquireResourceSharedLite(&ProcesslistR, TRUE))
	{

		tid = tempThreadEntry->ThreadID;
		AP = &tempThreadEntry->SuspendApc;
		PsLookupThreadByThreadId((PVOID)tid, &selectedthread);

		if (selectedthread)
		{
			DbgPrint("PEThread=%p\n", selectedthread);
			KeInitializeApc(AP,
				(PKTHREAD)selectedthread,
				0,
				(PKKERNEL_ROUTINE)Ignore,
				(PKRUNDOWN_ROUTINE)NULL,
				(PKNORMAL_ROUTINE)SuspendThreadAPCRoutine,
				KernelMode,
				NULL);

			ObDereferenceObject(selectedthread);
		}
		else
		{
			DbgPrint("Failed getting the pethread.\n");
		}
	}
	ExReleaseResourceLite(&ProcesslistR);
}

VOID CreateThreadNotifyRoutine(IN HANDLE  ProcessId,IN HANDLE  ThreadId,IN BOOLEAN  Create)
{
	if (KeGetCurrentIrql()==PASSIVE_LEVEL)
	{
		/*if (DebuggedProcessID==(ULONG)ProcessId)
		{
		//	PsSetContextThread (bah, xp only)
		}*/

		if (ExAcquireResourceExclusiveLite(&ProcesslistR, TRUE))
		{
			if (ThreadEventCount < 50)
			{
				ThreadEventData[ThreadEventCount].Created = Create;
				ThreadEventData[ThreadEventCount].ProcessID = (UINT_PTR)ProcessId;
				ThreadEventData[ThreadEventCount].ThreadID = (UINT_PTR)ThreadId;

				/*	if (Create)
						DbgPrint("Create ProcessID=%x\nThreadID=%x\n",(UINT_PTR)ProcessId,(UINT_PTR)ThreadId);
						else
						DbgPrint("Destroy ProcessID=%x\nThreadID=%x\n",(UINT_PTR)ProcessId,(UINT_PTR)ThreadId);
						*/

				ThreadEventCount++;
			}
		}
		ExReleaseResourceLite(&ProcesslistR);

		KeSetEvent(ThreadEvent, 0, FALSE);
		KeClearEvent(ThreadEvent);
	}
}

VOID CreateProcessNotifyRoutine(IN HANDLE  ParentId, IN HANDLE  ProcessId, IN BOOLEAN  Create)
{
	PEPROCESS CurrentProcess = NULL;
	HANDLE ProcessHandle = 0;
	/*
	if (PsSuspendProcess)
	{
		DbgPrint("Suspending process %d", PsGetCurrentThreadId());
		PsSuspendProcess(PsGetCurrentProcess());
		DbgPrint("After PsGetCurrentProcess()");
	}
*/
	
	
	if (KeGetCurrentIrql()==PASSIVE_LEVEL)
	{
		struct ProcessData *tempProcessEntry;

		//aquire a spinlock
		if (ExAcquireResourceExclusiveLite(&ProcesslistR, TRUE))
		{

			if (PsLookupProcessByProcessId((PVOID)ProcessId, &CurrentProcess) != STATUS_SUCCESS)
			{
				ExReleaseResourceLite(&ProcesslistR);
				return;
			}

			if ((ProcessWatcherOpensHandles) && (WatcherProcess))
			{
				
				
				if (Create)
				{
					//Open a handle to this process

					/*
						
					HANDLE ph = 0;
					NTSTATUS r = ObOpenObjectByPointer(CurrentProcess, 0, NULL, PROCESS_ALL_ACCESS, *PsProcessType, KernelMode, &ph);

					DbgPrint("CreateProcessNotifyRoutine: ObOpenObjectByPointer=%x  ph=%x", r, ph);
					r = ZwDuplicateObject(ZwCurrentProcess(), ph, WatcherHandle, &ProcessHandle, PROCESS_ALL_ACCESS, 0, DUPLICATE_CLOSE_SOURCE);

					DbgPrint("CreateProcessNotifyRoutine: ZwDuplicateObject=%x (handle=%x)", r, ProcessHandle);
					*/
						
					KAPC_STATE oldstate;

						
					KeStackAttachProcess((PKPROCESS)WatcherProcess, &oldstate);						
					__try
					{
						__try
						{
							ObOpenObjectByPointer(CurrentProcess, 0, NULL, PROCESS_ALL_ACCESS, *PsProcessType, KernelMode, &ProcessHandle);
						}
						__except (1)
						{
							DbgPrint("Exception during ObOpenObjectByPointer");
						}
					}
					__finally
					{
						KeUnstackDetachProcess(&oldstate);
					}
					
				}
				

				if (InternalProcessList == NULL)
				{
					InternalProcessList = ExAllocatePool(PagedPool, sizeof(RTL_GENERIC_TABLE));
					if (InternalProcessList)
						RtlInitializeGenericTable(InternalProcessList, ProcessListCompare, ProcessListAlloc, ProcessListDealloc, NULL);
				}

				if (InternalProcessList)
				{
					ProcessListData d, *r;

					d.ProcessID = ProcessId;
					d.PEProcess = CurrentProcess;
					d.ProcessHandle = ProcessHandle;

					r = RtlLookupElementGenericTable(InternalProcessList, &d);

					if (Create)
					{
						//add it to the list
						BOOLEAN newElement = FALSE;
						if (r) //weird
						{
							DbgPrint("Duplicate PID detected...");
							RtlDeleteElementGenericTable(InternalProcessList, r);
						}

						r = RtlInsertElementGenericTable(InternalProcessList, &d, sizeof(d), &newElement);


						DbgPrint("Added handle %x for pid %d to the list (newElement=%d r=%p)", (int)(UINT_PTR)d.ProcessHandle, (int)(UINT_PTR)d.ProcessID, newElement, r);
					}
					else
					{
						//remove it from the list (if it's there)
						DbgPrint("Process %d destruction. r=%p", (int)(UINT_PTR)d.ProcessID, r);
						if (r)
						{
							DbgPrint("Process that was in the list has been closed");
							//if (r->ProcessHandle)
							//	ZwClose(r->ProcessHandle);

							//RtlDeleteElementGenericTable(InternalProcessList, r);
							r->Deleted = 1;
						}

						if (CurrentProcess == WatcherProcess)
						{
							DbgPrint("CE Closed");
							
							//ZwClose(WatcherHandle);

							CleanProcessList(); //CE closed
							WatcherProcess = 0;
						}
					}
				}
			}


			//fill in a processcreateblock with data
			if (ProcessEventCount < 50)
			{
				ProcessEventdata[ProcessEventCount].Created = Create;
				ProcessEventdata[ProcessEventCount].ProcessID = (UINT_PTR)ProcessId;
				ProcessEventdata[ProcessEventCount].PEProcess = (UINT_PTR)CurrentProcess;
				ProcessEventCount++;
			}

			//if (!HiddenDriver)
			if (FALSE) //moved till next version
			{
				if (Create)
				{

					//allocate a block of memory for the processlist

					tempProcessEntry = ExAllocatePool(PagedPool, sizeof(struct ProcessData));
					tempProcessEntry->ProcessID = ProcessId;
					tempProcessEntry->PEProcess = CurrentProcess;
					tempProcessEntry->Threads = NULL;

					DbgPrint("Allocated a process at:%p\n", tempProcessEntry);

					if (!processlist)
					{
						processlist = tempProcessEntry;
						processlist->next = NULL;
						processlist->previous = NULL;
					}
					else
					{
						tempProcessEntry->next = processlist;
						tempProcessEntry->previous = NULL;
						processlist->previous = tempProcessEntry;
						processlist = tempProcessEntry;
					}
				}
				else
				{
					//find this process and delete it
					tempProcessEntry = processlist;
					while (tempProcessEntry)
					{
						if (tempProcessEntry->ProcessID == ProcessId)
						{
							int i;
							if (tempProcessEntry->next)
								tempProcessEntry->next->previous = tempProcessEntry->previous;

							if (tempProcessEntry->previous)
								tempProcessEntry->previous->next = tempProcessEntry->next;
							else
								processlist = tempProcessEntry->next;	//it had no previous entry, so it's the root



							/*
							if (tempProcessEntry->Threads)
							{
							struct ThreadData *tempthread,*tempthread2;
							KIRQL OldIrql2;

							tempthread=tempProcessEntry->Threads;
							tempthread2=tempthread;

							DbgPrint("Process ended. Freeing threads\n");

							while (tempthread)
							{
							tempthread=tempthread->next;
							DbgPrint("Free thread %p (next thread=%p)\n",tempthread2,tempthread);
							ExFreePool(tempthread2);
							tempthread2=tempthread;
							}

							}


							ExFreePool(tempProcessEntry);*/

							i = 0;
							tempProcessEntry = processlist;
							while (tempProcessEntry)
							{
								i++;
								tempProcessEntry = tempProcessEntry->next;
							}

							DbgPrint("There are %d processes in the list\n", i);

							break;
						}
						tempProcessEntry = tempProcessEntry->next;
					}


				}

			}
		}
		ExReleaseResourceLite(&ProcesslistR);

		if (CurrentProcess!=NULL)
			ObDereferenceObject(CurrentProcess);

		//signal process event (if there's one waiting for a signal)
		if (ProcessEvent)
		{
			KeSetEvent(ProcessEvent, 0, FALSE);
			KeClearEvent(ProcessEvent);
		}
	}
}

VOID CreateProcessNotifyRoutineEx(IN HANDLE  ParentId, IN HANDLE  ProcessId, __in_opt PPS_CREATE_NOTIFY_INFO CreateInfo)
{
	DbgPrint("CreateProcessNotifyRoutineEx");
	CreateProcessNotifyRoutine(ParentId, ProcessId, CreateInfo!=NULL);
}

HANDLE GetHandleForProcessID(IN HANDLE ProcessID)
{
	if (InternalProcessList)
	{
		ProcessListData d, *r;

		d.ProcessID = ProcessID;
		r = RtlLookupElementGenericTable(InternalProcessList, &d);
		if (r)
		{
			DbgPrint("Found a handle for PID %d (%x)", (int)(UINT_PTR)ProcessID, (int)(UINT_PTR)r->ProcessHandle);
			return r->ProcessHandle; // r->ProcessHandle;
		}	
	}	

	return 0;
}

VOID CleanProcessList()
{
	if (InternalProcessList)
	{
		PProcessListData li;

		if (ExAcquireResourceExclusiveLite(&ProcesslistR, TRUE))
		{
			KAPC_STATE oldstate;
			BOOLEAN ChangedContext;

			if ((WatcherProcess) && (WatcherProcess != PsGetCurrentProcess()))
			{				
				KeStackAttachProcess((PKPROCESS)WatcherProcess, &oldstate);
				ChangedContext = TRUE;
			}

			while (li = RtlGetElementGenericTable(InternalProcessList, 0))
			{
				if ((li->ProcessHandle) && (WatcherProcess))
					ZwClose(li->ProcessHandle);

				RtlDeleteElementGenericTable(InternalProcessList, li);
			}
			
			ExFreePool(InternalProcessList);
			InternalProcessList = NULL;
		}
		ExReleaseResourceLite(&ProcesslistR);
	}

}