#include "ntifs.h"
#include "processlist.h"
#include "threads.h"
#include "memscan.h"

VOID GetThreadData(IN PDEVICE_OBJECT  DeviceObject, IN PVOID  Context)
{
	KIRQL OldIrql;
	struct ThreadData *tempThreadEntry;
	PETHREAD selectedthread;
	ULONG tid;
	LARGE_INTEGER Timeout;
	PKAPC AP;
	tempThreadEntry=Context;
	

	DbgPrint("Gathering PEThread thread\n");

	Timeout.QuadPart = -1;
	KeDelayExecutionThread(KernelMode, TRUE, &Timeout);

	selectedthread=NULL;

	KeAcquireSpinLock(&ProcesslistSL,&OldIrql);
	tid=(ULONG)tempThreadEntry->ThreadID;
	AP=&tempThreadEntry->SuspendApc;
	PsLookupThreadByThreadId((PVOID)tid,&selectedthread);
	
	if (selectedthread)
	{
		DbgPrint("PEThread=%p\n",selectedthread);
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
		//IoQueueWorkItem(newthreaddatafiller,GetThreadData,DelayedWorkQueue,Context);
	}

	KeReleaseSpinLock(&ProcesslistSL,OldIrql);
}

VOID CreateThreadNotifyRoutine(IN HANDLE  ProcessId,IN HANDLE  ThreadId,IN BOOLEAN  Create)
{
	KIRQL OldIrql;
	PETHREAD CurrentThread;	

	if (KeGetCurrentIrql()==PASSIVE_LEVEL)
	{
		/*if (DebuggedProcessID==(ULONG)ProcessId)
		{
		//	PsSetContextThread (bah, xp only)
		}*/

        KeAcquireSpinLock(&ProcesslistSL,&OldIrql);  //perhaps a check for winxp and then call KeAcquireInStackQueuedSpinLock instead....
		

		if (ThreadEventCount<50)
		{		
			ThreadEventData[ThreadEventCount].Created=Create;
			ThreadEventData[ThreadEventCount].ProcessID=(UINT_PTR)ProcessId;
			ThreadEventData[ThreadEventCount].ThreadID=(UINT_PTR)ThreadId;

		/*	if (Create)
                DbgPrint("Create ProcessID=%x\nThreadID=%x\n",(UINT_PTR)ProcessId,(UINT_PTR)ThreadId);
			else
				DbgPrint("Destroy ProcessID=%x\nThreadID=%x\n",(UINT_PTR)ProcessId,(UINT_PTR)ThreadId);
		*/			

			ThreadEventCount++;
		}


		//----------------------------threadlist2------------------------:

		//if (!HiddenDriver)	
		if (FALSE) //moved till next version
		{
			struct ProcessData *tempProcessEntry;
			struct ThreadData *tempThreadEntry;
			//find the process in the list

			tempProcessEntry=processlist;
			while (tempProcessEntry)
			{
				if (tempProcessEntry->ProcessID==ProcessId)
				{
					//it's from a thread in my list
					if (Create)
					{					
						
						//add thread to list
						tempThreadEntry=ExAllocatePoolWithTag(NonPagedPool,sizeof(struct ThreadData),0);

						tempThreadEntry->ThreadID=ThreadId;	
						tempThreadEntry->PEThread=NULL; //stupid PEThread isn't known here yet 
						tempThreadEntry->suspendcount=0;
						tempThreadEntry->next=NULL;
						tempThreadEntry->previous=NULL;

						IoQueueWorkItem(newthreaddatafiller,GetThreadData,DelayedWorkQueue,tempThreadEntry);

						KeInitializeSemaphore(&tempThreadEntry->SuspendSemaphore, 0, 1);




						if (!tempProcessEntry->Threads)
						{
							DbgPrint("First thread\n");
							tempProcessEntry->Threads=tempThreadEntry;
							tempProcessEntry->Threads->next=NULL;
							tempProcessEntry->Threads->previous=NULL;
						}
						else
						{
							DbgPrint("Adding to list\n");
							tempThreadEntry->next=tempProcessEntry->Threads;
							tempThreadEntry->previous=NULL;
							tempThreadEntry->next->previous=tempThreadEntry;
							tempProcessEntry->Threads=tempThreadEntry;
						}

					}
					else
					{
						
						//remove thread from list
						tempThreadEntry=tempProcessEntry->Threads;
						while (tempThreadEntry)
						{
							
							if (tempThreadEntry->ThreadID==ThreadId)
							{								
								//the entry after this gets it's previous field set to the one before this one
								if (tempThreadEntry->next)
									tempThreadEntry->next->previous=tempThreadEntry->previous;

								if (tempThreadEntry->previous)
									tempThreadEntry->previous->next=tempThreadEntry->next;
								else
									tempProcessEntry->Threads=tempThreadEntry->next;
							

								DbgPrint("Deallocating a thread at :%p\n",tempThreadEntry);
								//ExFreePool(tempThreadEntry);
								
                                break; //found it so exit
							}

							tempThreadEntry=tempThreadEntry->next;
						}

					


					}
					break;
				}
				tempProcessEntry=tempProcessEntry->next;
			}

		}



		KeReleaseSpinLock(&ProcesslistSL,OldIrql);
		
		/*
		if (CurrentThread!=NULL)
		{
			DbgPrint("Dereferencing thread\n");
		}*/

		//signal thread event (if there's one waiting for a signal)
		KeSetEvent(ThreadEvent, 0, FALSE);
		KeClearEvent(ThreadEvent);

	}
}

VOID CreateProcessNotifyRoutine( IN HANDLE  ParentId, IN HANDLE  ProcessId, IN BOOLEAN  Create)
{	
	//LARGE_INTEGER wt;
	//HANDLE TH;
	KIRQL OldIrql;
	PEPROCESS CurrentProcess;
	//CLIENT_ID CI;

	DbgPrint("CreateProcessNotifyRoutine called (ParentID=%x ProcessID=%d Create=%d\n",ParentId, ProcessId, Create);

	if (KeGetCurrentIrql()==PASSIVE_LEVEL)
	{
		struct ProcessData *tempProcessEntry;
	
		CurrentProcess=NULL;
		PsLookupProcessByProcessId((PVOID)ProcessId,&CurrentProcess);

		//aquire a spinlock
		KeAcquireSpinLock(&ProcesslistSL,&OldIrql);  //perhaps a check for winxp and then call KeAcquireInStackQueuedSpinLock instead....
		
		//fill in a processcreateblock with data
		if (ProcessEventCount<50)
		{		
			ProcessEventdata[ProcessEventCount].Created=Create;
			ProcessEventdata[ProcessEventCount].ProcessID=(UINT_PTR)ProcessId;
			ProcessEventdata[ProcessEventCount].PEProcess=(UINT_PTR)CurrentProcess;
			ProcessEventCount++;
		}

		//if (!HiddenDriver)
		if (FALSE) //moved till next version
		{		
			if (Create)
			{
				
				//allocate a block of memory for the processlist
	            
				tempProcessEntry=ExAllocatePoolWithTag(NonPagedPool,sizeof(struct ProcessData),0);
				tempProcessEntry->ProcessID=ProcessId;
				tempProcessEntry->PEProcess=CurrentProcess;
				tempProcessEntry->Threads=NULL;
				
				DbgPrint("Allocated a process at:%p\n",tempProcessEntry);

				if (!processlist)
				{
					processlist=tempProcessEntry;
					processlist->next=NULL;
					processlist->previous=NULL;
				}
				else
				{
					tempProcessEntry->next=processlist;
					tempProcessEntry->previous=NULL;
					processlist->previous=tempProcessEntry;
					processlist=tempProcessEntry;
				}
			}
			else
			{
				//find this process and delete it
				tempProcessEntry=processlist;
				while (tempProcessEntry)
				{
					if (tempProcessEntry->ProcessID==ProcessId)
					{
						int i;
						if (tempProcessEntry->next)
							tempProcessEntry->next->previous=tempProcessEntry->previous;

						if (tempProcessEntry->previous)
							tempProcessEntry->previous->next=tempProcessEntry->next;
						else 
							processlist=tempProcessEntry->next;	//it had no previous entry, so it's the root



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

						i=0;
						tempProcessEntry=processlist;
						while (tempProcessEntry)
						{
							i++;
							tempProcessEntry=tempProcessEntry->next;
						}

						DbgPrint("There are %d processes in the list\n",i);

						break;
					}
					tempProcessEntry=tempProcessEntry->next;
				}
				

			}
		
		}

		//release spinlock
		KeReleaseSpinLock(&ProcesslistSL,OldIrql);

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

