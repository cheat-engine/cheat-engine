#include "threads.h"
#include "processlist.h"
#include "memscan.h"

struct ThreadData* GetThreaddata(ULONG threadid)
{
	struct ProcessData *tempProcessData;
	struct ThreadData *tempThreadData;

	//PRE: Lock the list before calling this routine
	tempProcessData=processlist;
	while (tempProcessData)
	{
		tempThreadData=tempProcessData->Threads;
		while (tempThreadData)
		{
			if (tempThreadData->ThreadID==(HANDLE)threadid)
				return tempThreadData;
			
			tempThreadData=tempThreadData->next;
		}
        
		tempProcessData=tempProcessData->next;
	}

	return NULL;
}

void Ignore(PKAPC Apc, PKNORMAL_ROUTINE NormalRoutine, PVOID NormalContext, PVOID SystemArgument1, PVOID SystemArgument2)
{
	//ignore
	return;
}

void SuspendThreadAPCRoutine(PVOID arg1, PVOID arg2, PVOID arg3)
{
	LARGE_INTEGER Timeout;
	struct ThreadData *x;
	//DbgPrint("Inside SuspendThreadAPCRoutine\n");
	   
	
	x=arg1;

	//DbgPrint("x=%p",x);
	DbgPrint("Waiting...\n");
	Timeout.QuadPart = -999999999999999;	

		
	KeWaitForSingleObject(&(x->SuspendSemaphore), 5, KernelMode, FALSE, NULL);
	//KeDelayExecutionThread(KernelMode, FALSE, &Timeout);
	DbgPrint("Resuming...\n");
}

void DBKSuspendThread(ULONG ThreadID)
{
	KIRQL OldIrql;
	
	struct ThreadData *t_data;


    KeAcquireSpinLock(&ProcesslistSL,&OldIrql); 

	DbgPrint("Going to suspend this thread\n");

	//find the thread in the threadlist


	//find the threadid in the processlist
	t_data=GetThreaddata(ThreadID);
	if (t_data)
	{
		DbgPrint("Suspending thread....\n");	

		

		if (!t_data->PEThread)
		{
			//not yet initialized
			t_data->PEThread=(PETHREAD)getPEThread(ThreadID);
			KeInitializeApc(&t_data->SuspendApc,
							(PKTHREAD)t_data->PEThread,
							0,
							(PKKERNEL_ROUTINE)Ignore,
							(PKRUNDOWN_ROUTINE)NULL,
							(PKNORMAL_ROUTINE)SuspendThreadAPCRoutine,
							KernelMode,
							t_data);

		}	
		DbgPrint("x should be %p",t_data);
		t_data->suspendcount++;

		if (t_data->suspendcount==1) //not yet suspended so suspend it
            KeInsertQueueApc(&t_data->SuspendApc, t_data, t_data, 0);		
	}
	else
		DbgPrint("Thread not found in the list\n");

	KeReleaseSpinLock(&ProcesslistSL,OldIrql);
}

void DBKResumeThread(ULONG ThreadID)
{
	KIRQL OldIrql;
	
	struct ThreadData *t_data;


    KeAcquireSpinLock(&ProcesslistSL,&OldIrql); 


	DbgPrint("Going to resume this thread\n");

	//find the thread in the threadlist


	//find the threadid in the processlist
	t_data=GetThreaddata(ThreadID);
	if (t_data)
	{
		if (t_data->suspendcount)
		{
			t_data->suspendcount--;
			if (!t_data->suspendcount) //suspendcount=0 so resume
				KeReleaseSemaphore(&t_data->SuspendSemaphore,0,1,FALSE);
		}
	}
	else
		DbgPrint("Thread not found in the list\n");

	KeReleaseSpinLock(&ProcesslistSL,OldIrql);

}

void DBKSuspendProcess(ULONG ProcessID)
{
	KIRQL OldIrql;
	
	struct ThreadData *t_data=NULL;
	struct ProcessData *tempProcessData=NULL;


    KeAcquireSpinLock(&ProcesslistSL,&OldIrql); 
	

	DbgPrint("Going to suspend this process\n");

	//find the process in the threadlist

	tempProcessData=processlist;
	while (tempProcessData)
	{
		if (tempProcessData->ProcessID==(HANDLE)ProcessID)
		{
			t_data=tempProcessData->Threads;
			break;
		}
		tempProcessData=tempProcessData->next;
	}

	if (!t_data)
	{
		DbgPrint("This process was not found\n");
		return; //no process found
	}


	while (t_data)
	{
		DbgPrint("Suspending thread....\n");		

		if (!t_data->PEThread)
		{
			//not yet initialized
			t_data->PEThread=(PETHREAD)getPEThread((ULONG)t_data->ThreadID);
			KeInitializeApc(&t_data->SuspendApc,
							(PKTHREAD)t_data->PEThread,
							0,
							(PKKERNEL_ROUTINE)Ignore,
							(PKRUNDOWN_ROUTINE)NULL,
							(PKNORMAL_ROUTINE)SuspendThreadAPCRoutine,
							KernelMode,
							t_data);

		}	
		DbgPrint("x should be %p",t_data); 
		t_data->suspendcount++;

		if (t_data->suspendcount==1) //not yet suspended so suspend it
			KeInsertQueueApc(&t_data->SuspendApc, t_data, t_data, 0);

		t_data=t_data->next; //next thread
	}

	KeReleaseSpinLock(&ProcesslistSL,OldIrql);

}

void DBKResumeProcess(ULONG ProcessID)
{
	KIRQL OldIrql;
	
	struct ThreadData *t_data=NULL;
	struct ProcessData *tempProcessData=NULL;


	KeAcquireSpinLock(&ProcesslistSL,&OldIrql);
	

	DbgPrint("Going to suspend this process\n");

	//find the process in the threadlist

	tempProcessData=processlist;
	while (tempProcessData)
	{
		if (tempProcessData->ProcessID==(HANDLE)ProcessID)
		{
			t_data=tempProcessData->Threads;
			break;
		}
		tempProcessData=tempProcessData->next;
	}

	if (!t_data)
	{
		DbgPrint("This process was not found\n");
		return; //no process found
	}


	while (t_data)
	{
		DbgPrint("Resuming thread....\n");		

		if (t_data->suspendcount)
		{
			t_data->suspendcount--;
			if (!t_data->suspendcount) //suspendcount=0 so resume
				KeReleaseSemaphore(&t_data->SuspendSemaphore,0,1,FALSE);
		}


		t_data=t_data->next; //next thread
	}

	KeReleaseSpinLock(&ProcesslistSL,OldIrql);
}

