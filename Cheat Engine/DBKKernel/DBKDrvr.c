/*
  Build upon the MSJDrvr.C by James M. Finnegan - Microsoft Systems Journal (1998)  
*/

#include "DBKFunc.h"
#include "rootkit.h"
#include "processlist.h"
#include "memscan.h"
#include "threads.h"

#include "vmxhelper.h"
#include "newkernel.h"


#ifdef CETC
	#include "cetc.h"
#endif

#ifdef AMD64
	#define dbkversion 2640011
#else
	#define dbkversion 2000011
#endif

#define IOCTL_UNKNOWN_BASE					FILE_DEVICE_UNKNOWN

#define IOCTL_CE_READMEMORY						CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0800, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_WRITEMEMORY					CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0801, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_OPENPROCESS    				CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0802, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_QUERY_VIRTUAL_MEMORY			CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0803, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_TEST							CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0804, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_GETPEPROCESS					CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0805, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_READPHYSICALMEMORY				CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0806, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_WRITEPHYSICALMEMORY			CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0807, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_GETPHYSICALADDRESS				CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0808, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_PROTECTME						CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0809, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_GETCR3 						CTL_CODE(IOCTL_UNKNOWN_BASE, 0x080a, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_SETCR3 						CTL_CODE(IOCTL_UNKNOWN_BASE, 0x080b, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_GETSDT 						CTL_CODE(IOCTL_UNKNOWN_BASE, 0x080c, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_INITIALIZE     		    	CTL_CODE(IOCTL_UNKNOWN_BASE, 0x080d, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_DONTPROTECTME					CTL_CODE(IOCTL_UNKNOWN_BASE, 0x080e, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_GETIDT 						CTL_CODE(IOCTL_UNKNOWN_BASE, 0x080f, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_HOOKINTS 						CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0810, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_DEBUGPROCESS 					CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0811, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_RETRIEVEDEBUGDATA				CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0812, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_STARTPROCESSWATCH				CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0813, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_GETPROCESSEVENTS				CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0814, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_GETTHREADEVENTS				CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0815, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_GETVERSION						CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0816, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_GETCR4 						CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0817, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_OPENTHREAD	    				CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0818, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_MAKEWRITABLE					CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0819, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_DEBUGPROCESS_CHANGEREG			CTL_CODE(IOCTL_UNKNOWN_BASE, 0x081a, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_STOPDEBUGGING					CTL_CODE(IOCTL_UNKNOWN_BASE, 0x081b, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define	IOCTL_CE_STOP_DEBUGPROCESS_CHANGEREG	CTL_CODE(IOCTL_UNKNOWN_BASE, 0x081c, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define	IOCTL_CE_USEALTERNATEMETHOD				CTL_CODE(IOCTL_UNKNOWN_BASE, 0x081d, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define	IOCTL_CE_ISUSINGALTERNATEMETHOD			CTL_CODE(IOCTL_UNKNOWN_BASE, 0x081e, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define	IOCTL_CE_ALLOCATEMEM					CTL_CODE(IOCTL_UNKNOWN_BASE, 0x081f, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_CREATEAPC						CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0820, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_GETPETHREAD					CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0821, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)

#define IOCTL_CE_SUSPENDTHREAD					CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0822, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_RESUMETHREAD					CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0823, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_SUSPENDPROCESS					CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0824, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_RESUMEPROCESS					CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0825, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)

#define IOCTL_CE_ALLOCATEMEM_NONPAGED			CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0826, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_GETPROCADDRESS					CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0827, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_SETSDTADDRESS					CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0828, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_GETSDTADDRESS					CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0829, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)

#define IOCTL_CE_GETGDT 						CTL_CODE(IOCTL_UNKNOWN_BASE, 0x082a, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_SETCR4 						CTL_CODE(IOCTL_UNKNOWN_BASE, 0x082b, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_GETTR 							CTL_CODE(IOCTL_UNKNOWN_BASE, 0x082c, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_VMXCONFIG						CTL_CODE(IOCTL_UNKNOWN_BASE, 0x082d, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_GETCR0 						CTL_CODE(IOCTL_UNKNOWN_BASE, 0x082e, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_MAKEKERNELCOPY					CTL_CODE(IOCTL_UNKNOWN_BASE, 0x082f, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CE_SETGLOBALDEBUGSTATE			CTL_CODE(IOCTL_UNKNOWN_BASE, 0x0830, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)


void MSJUnloadDriver(PDRIVER_OBJECT DriverObject);

NTSTATUS MSJDispatchCreate(IN PDEVICE_OBJECT DeviceObject,
                       IN PIRP Irp);

NTSTATUS MSJDispatchClose(IN PDEVICE_OBJECT DeviceObject,
                       IN PIRP Irp);

NTSTATUS MSJDispatchIoctl(IN PDEVICE_OBJECT DeviceObject,
                            IN PIRP Irp);


//-----NtUserSetWindowsHookEx----- //prevent global hooks
typedef ULONG (NTUSERSETWINDOWSHOOKEX)(
    IN HANDLE hmod,
    IN PUNICODE_STRING pstrLib OPTIONAL,
    IN DWORD idThread,
    IN int nFilterType,
    IN PVOID pfnFilterProc,
    IN DWORD dwFlags
);
NTUSERSETWINDOWSHOOKEX OldNtUserSetWindowsHookEx;
ULONG NtUserSetWindowsHookEx_callnumber;
//HHOOK NewNtUserSetWindowsHookEx(IN HANDLE hmod,IN PUNICODE_STRING pstrLib OPTIONAL,IN DWORD idThread,IN int nFilterType, IN PROC pfnFilterProc,IN DWORD dwFlags);

//------------------------





typedef NTSTATUS (*ZWSUSPENDPROCESS)
(
    IN ULONG ProcessHandle  // Handle to the process
);
ZWSUSPENDPROCESS ZwSuspendProcess;




//PVOID GetApiEntry(ULONG FunctionNumber);
void Unhook(void);

NTSTATUS ZwCreateThread(
	OUT PHANDLE  ThreadHandle,
	IN ACCESS_MASK  DesiredAccess,
	IN POBJECT_ATTRIBUTES  ObjectAttributes,
	IN HANDLE  ProcessHandle,
	OUT PCLIENT_ID  ClientId,
	IN PCONTEXT  ThreadContext,
	IN PVOID  UserStack,
	IN BOOLEAN  CreateSuspended);


PSERVICE_DESCRIPTOR_TABLE KeServiceDescriptorTableShadow=NULL;
PSERVICE_DESCRIPTOR_TABLE KeServiceDescriptorTable=NULL;

UNICODE_STRING  uszDeviceString;
PVOID BufDeviceString=NULL;

#define SYSTEMSERVICE(_function)		KeServiceDescriptorTable->ServiceTable[ *(PULONG)((PUCHAR)_function+1)]
#define SYSTEMSERVICELINK(_function)	KeServiceDescriptorTable->ServiceTable[*((PUCHAR)(*(PULONG)*((PULONG)((PUCHAR)_function+2)))+1)]


void hideme(PDRIVER_OBJECT DriverObject)
{
	typedef struct _MODULE_ENTRY {
	LIST_ENTRY le_mod;
	DWORD  unknown[4];
	DWORD  base;
	DWORD  driver_start;
	DWORD  unk1;
	UNICODE_STRING driver_Path;
	UNICODE_STRING driver_Name;
} MODULE_ENTRY, *PMODULE_ENTRY;

	PMODULE_ENTRY pm_current;

	pm_current =  *((PMODULE_ENTRY*)((DWORD)DriverObject + 0x14)); //eeeeew

	*((PDWORD)pm_current->le_mod.Blink)        = (DWORD) pm_current->le_mod.Flink;
	pm_current->le_mod.Flink->Blink            = pm_current->le_mod.Blink;
	HiddenDriver=TRUE;
}

void mykapc2(PKAPC Apc, PKNORMAL_ROUTINE NormalRoutine, PVOID NormalContext, PVOID SystemArgument1, PVOID SystemArgument2)
{
	ExFreePool(Apc);
	DbgPrint("My second kernelmode apc!!!!\n");
	DbgPrint("SystemArgument1=%x\n",*(PULONG)SystemArgument1);
}

void nothing2(PVOID arg1, PVOID arg2, PVOID arg3)
{
	
	return;
}

void mykapc(PKAPC Apc, PKNORMAL_ROUTINE NormalRoutine, PVOID NormalContext, PVOID SystemArgument1, PVOID SystemArgument2)
{
	//kernelmode apc, always gets executed
	PKAPC      kApc;
	LARGE_INTEGER Timeout;

	kApc = ExAllocatePool(NonPagedPool, sizeof(KAPC));


	ExFreePool(Apc);
	DbgPrint("My kernelmode apc!!!!\n");
	
	DbgPrint("NormalRoutine=%x\n",*(PULONG)NormalRoutine);
	DbgPrint("NormalContext=%x\n",*(PULONG)NormalContext);
	DbgPrint("SystemArgument1=%x\n",*(PULONG)SystemArgument1);
	DbgPrint("SystemArgument1=%x\n",*(PULONG)SystemArgument2);
	
	
	KeInitializeApc(kApc,
		            (PKTHREAD)PsGetCurrentThread(),
                    0,
                    (PKKERNEL_ROUTINE)mykapc2,
                    NULL,
                    (PKNORMAL_ROUTINE)*(PULONG)SystemArgument1,
                    UserMode,
                    (PVOID)*(PULONG)NormalContext
                    );

	KeInsertQueueApc (kApc, (PVOID)*(PULONG)SystemArgument1, (PVOID)*(PULONG)SystemArgument2, 0);


	//wait in usermode (to interruptable by a usermode apc)
	Timeout.QuadPart = 0;
	KeDelayExecutionThread(UserMode, TRUE, &Timeout);

	return;
}

void nothing(PVOID arg1, PVOID arg2, PVOID arg3)
{
	return;
}

void CreateRemoteAPC(ULONG threadid,PVOID addresstoexecute)
{
	PKTHREAD   kThread;
	PKAPC      kApc;

	kApc = ExAllocatePool(NonPagedPool, sizeof(KAPC));

	kThread=(PKTHREAD)getPEThread(threadid);
	DbgPrint("(PVOID)KThread=%p\n",kThread);


   
	KeInitializeApc(kApc,
		            kThread,
                    0,
                    (PKKERNEL_ROUTINE)mykapc,
                    NULL,
                    (PKNORMAL_ROUTINE)nothing,
                    KernelMode,
                    0
                    );

	KeInsertQueueApc (kApc, addresstoexecute, addresstoexecute, 0);
}


/*
PVOID GetApiEntry(ULONG FunctionNumber)
{
	
#ifndef AMD64
	int direction=0; //0=down 1=up
	PUCHAR x;
	//Get the start address of a known function (ZwOpenProcess)
	ULONG LastFunctionNumber=*(PULONG)((PUCHAR)ZwOpenProcess+1);  //0x7a in xp pro sp1
	
	x=(PUCHAR)ZwOpenProcess;

	if (FunctionNumber==LastFunctionNumber) //If you're retarded this is usefull
		return (PVOID)ZwOpenProcess;

	if (FunctionNumber<LastFunctionNumber)
		direction=1;
	else
		direction=0;

    //now search for e8,functionnumber,  (and perhaps also 8d,54,24,04
	if (direction==1)
	{
		return (PVOID)0x12345678;
		//go back in the list
		while (LastFunctionNumber>0)
		{
			if (*x==0xb8)
			{
				//it's a mov eax,........
				//now get the function number if this address

				//make sure it's not function e8
				if (*(PULONG)((ULONG)(x)-1)==0xb8)
					x--;
				
				LastFunctionNumber=*(PULONG)((ULONG)(x)+1);

				if (LastFunctionNumber==FunctionNumber)
					return x; //found it				
			}
			x--;
		}
	}
	else
	{
		x++;		
		while (1)
		{
			//move forward
			if (*x==0xb8)
			{
				if (*(PULONG)((ULONG)(x)+1)==0xb8) //the previous byte was also a 0xb8, so this has to be the identifier
				{
					x++; //next;
					continue;
				}				
				
				LastFunctionNumber=*(PULONG)((ULONG)x+1);

				if (LastFunctionNumber==FunctionNumber)
					return x;
			}
			x++;
		}		
	}

#endif //amd64
	
	return NULL;
}*/

int testfunction(int p1,int p2)
{
	DbgPrint("Hello\nParam1=%d\nParam2=%d\n",p1,p2);
	return 0x666;
}


void* functionlist[1];
char  paramsizes[1];
int registered=0;


void AddSystemServices(void)
{
/*	if (registered)
		return; //no need to register again

	DbgPrint("Registering SystemServiceTable\n");
	
	
	functionlist[0] = (void *)&testfunction;
	paramsizes[0] = 2*4;

	if (KeServiceDescriptorTableShadow)
	{
		//override if any other exists
		KeServiceDescriptorTableShadow[1].ArgumentTable=NULL;
		KeServiceDescriptorTableShadow[1].CounterTable=NULL;
		KeServiceDescriptorTableShadow[1].ServiceTable=NULL;
		KeServiceDescriptorTableShadow[1].TableSize=0;

		KeServiceDescriptorTable[2].ArgumentTable=NULL;
		KeServiceDescriptorTable[2].CounterTable=NULL;
		KeServiceDescriptorTable[2].ServiceTable=NULL;
		KeServiceDescriptorTable[2].TableSize=0;
	}

  
	if (KeAddSystemServiceTable((PULONG_PTR)functionlist, 0, 1, (PUCHAR)paramsizes, 2))
	{
		registered=1;
		DbgPrint("Register successfull\n");
	}
	else
	{
		DbgPrint("Register failed.\n");
	}

	

	DbgPrint("KeServiceDescriptorTable[0]=%p",&KeServiceDescriptorTable[0]);
	DbgPrint("KeServiceDescriptorTable[1]=%p",&KeServiceDescriptorTable[1]);
	DbgPrint("KeServiceDescriptorTable[2]=%p",&KeServiceDescriptorTable[2]);
	DbgPrint("KeServiceDescriptorTable[3]=%p",&KeServiceDescriptorTable[3]);

	*/

}






NTSTATUS DriverEntry(IN PDRIVER_OBJECT DriverObject,
                     IN PUNICODE_STRING RegistryPath)
/*++

Routine Description:

    This routine is called when the driver is loaded by NT.

Arguments:

    DriverObject - Pointer to driver object created by system.
    RegistryPath - Pointer to the name of the services node for this driver.

Return Value:

    The function value is the final status from the initialization operation.

--*/
{
    NTSTATUS        ntStatus;
    PVOID           BufDriverString=NULL,BufProcessEventString=NULL,BufThreadEventString=NULL;
    UNICODE_STRING  uszDriverString;
    
    UNICODE_STRING  uszProcessEventString;
	UNICODE_STRING	uszThreadEventString;
    PDEVICE_OBJECT  pDeviceObject;
	int				i;
	ULONG cr4reg;

	HANDLE reg;
	OBJECT_ATTRIBUTES oa;

	UNICODE_STRING temp; 


	//DbgPrint("%S",oa.ObjectName.Buffer);  


	WORD this_cs, this_ss, this_ds, this_es, this_fs, this_gs;
	__asm
	{
		mov ax,cs
		mov [this_cs],ax

		mov ax,ss
		mov [this_ss],ax

		mov ax,ds
		mov [this_ds],ax

		mov ax,es
		mov [this_es],ax

		mov ax,fs
		mov [this_fs],ax

		mov ax,gs
		mov [this_gs],ax
	}
	DbgPrint("cs=%x ss=%x ds=%x es=%x fs=%x gs=%x\n",this_cs, this_ss, this_ds, this_es, this_fs, this_gs);



	//lame antiviruses and more lamer users that keep crying rootkit virus....
	RtlInitUnicodeString(&temp, L"KeServiceDescriptorTable"); 
	KeServiceDescriptorTable=MmGetSystemRoutineAddress(&temp);         

	DbgPrint("Loading driver\n");
	DbgPrint("Registry path = %S\n", RegistryPath->Buffer);

	InitializeObjectAttributes(&oa,RegistryPath,OBJ_KERNEL_HANDLE ,NULL,NULL);
	ntStatus=ZwOpenKey(&reg,KEY_QUERY_VALUE,&oa);
	if (ntStatus == STATUS_SUCCESS)
	{
		UNICODE_STRING A,B,C,D;
		PVOID buf;
		PKEY_VALUE_PARTIAL_INFORMATION bufA,bufB,bufC,bufD;
		ULONG ActualSize;

		DbgPrint("Opened the key\n");

		BufDriverString=ExAllocatePool(PagedPool,sizeof(KEY_VALUE_PARTIAL_INFORMATION)+100);
		BufDeviceString=ExAllocatePool(PagedPool,sizeof(KEY_VALUE_PARTIAL_INFORMATION)+100);
		BufProcessEventString=ExAllocatePool(PagedPool,sizeof(KEY_VALUE_PARTIAL_INFORMATION)+100);
		BufThreadEventString=ExAllocatePool(PagedPool,sizeof(KEY_VALUE_PARTIAL_INFORMATION)+100);

		bufA=BufDriverString;
		bufB=BufDeviceString;
		bufC=BufProcessEventString;
		bufD=BufThreadEventString;

		RtlInitUnicodeString(&A, L"A");
		RtlInitUnicodeString(&B, L"B");
		RtlInitUnicodeString(&C, L"C");
		RtlInitUnicodeString(&D, L"D");

		if (ntStatus == STATUS_SUCCESS)
			ntStatus=ZwQueryValueKey(reg,&A,KeyValuePartialInformation ,bufA,sizeof(KEY_VALUE_PARTIAL_INFORMATION)+100,&ActualSize);
		if (ntStatus == STATUS_SUCCESS)
			ntStatus=ZwQueryValueKey(reg,&B,KeyValuePartialInformation ,bufB,sizeof(KEY_VALUE_PARTIAL_INFORMATION)+100,&ActualSize);
		if (ntStatus == STATUS_SUCCESS)
			ntStatus=ZwQueryValueKey(reg,&C,KeyValuePartialInformation ,bufC,sizeof(KEY_VALUE_PARTIAL_INFORMATION)+100,&ActualSize);
		if (ntStatus == STATUS_SUCCESS)
			ntStatus=ZwQueryValueKey(reg,&D,KeyValuePartialInformation ,bufD,sizeof(KEY_VALUE_PARTIAL_INFORMATION)+100,&ActualSize);

		if (ntStatus == STATUS_SUCCESS)
		{
			DbgPrint("Read ok\n");
			RtlInitUnicodeString(&uszDriverString,(PCWSTR) bufA->Data);
			RtlInitUnicodeString(&uszDeviceString,(PCWSTR) bufB->Data);
			RtlInitUnicodeString(&uszProcessEventString,(PCWSTR) bufC->Data);
			RtlInitUnicodeString(&uszThreadEventString,(PCWSTR) bufD->Data);
		}
		else
		{
			ExFreePool(bufA);
			ExFreePool(bufB);
			ExFreePool(bufC);
			ExFreePool(bufD);

			DbgPrint("Failed reading the value\n");
			ZwClose(reg);
			return STATUS_UNSUCCESSFUL;;
		}

	}
	else
	{
		DbgPrint("Failed opening the key\n");
		return STATUS_UNSUCCESSFUL;;
	}

	ntStatus = STATUS_SUCCESS;

    // Point uszDriverString at the driver name
#ifndef CETC
	
	
	// Create and initialize device object
    ntStatus = IoCreateDevice(DriverObject,
                              0,
                              &uszDriverString,
                              FILE_DEVICE_UNKNOWN,
                              0,
                              FALSE,
                              &pDeviceObject);

    if(ntStatus != STATUS_SUCCESS)
	{
		ExFreePool(BufDriverString);
		ExFreePool(BufDeviceString);
		ExFreePool(BufProcessEventString);
		ExFreePool(BufThreadEventString);
		
		ZwClose(reg);
        return ntStatus;
	}

    // Point uszDeviceString at the device name
	
    // Create symbolic link to the user-visible name
    ntStatus = IoCreateSymbolicLink(&uszDeviceString, &uszDriverString);

    if(ntStatus != STATUS_SUCCESS)
    {
        // Delete device object if not successful
        IoDeleteDevice(pDeviceObject);

		ExFreePool(BufDriverString);
		ExFreePool(BufDeviceString);
		ExFreePool(BufProcessEventString);
		ExFreePool(BufThreadEventString);
		

		ZwClose(reg);
        return ntStatus;
    }

#endif


    // Load structure to point to IRP handlers...
    DriverObject->DriverUnload                         = MSJUnloadDriver;
    DriverObject->MajorFunction[IRP_MJ_CREATE]         = MSJDispatchCreate;
    DriverObject->MajorFunction[IRP_MJ_CLOSE]          = MSJDispatchClose;
    DriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL] = MSJDispatchIoctl;

	DebuggedProcessID=0;				
	
	ProtectOn=FALSE;
	ImageNotifyRoutineLoaded=FALSE;
	LastForegroundWindow=0;
	ProtectedProcessID=0;
	ModuleList=NULL;
	ModuleListSize=0;
	KernelCopy=0;

	globaldebug=0;

	newthreaddatafiller=IoAllocateWorkItem(pDeviceObject);

	//

	//Processlist init
#ifndef CETC
/*	DbgPrint("Creating ProcessEvent with name : %S",uszProcessEventString.Buffer);
	ProcessEvent=IoCreateNotificationEvent(&uszProcessEventString, &ProcessEventHandle);
	if (ProcessEvent==NULL)
		DbgPrint("Failed creating ProcessEvent");

	KeClearEvent(ProcessEvent);*/

	ProcessEventCount=0;
	KeInitializeSpinLock(&ProcesslistSL);
#endif

	CreateProcessNotifyRoutineEnabled=FALSE;

	//threadlist init
#ifndef CETC
/*	DbgPrint("Creating ThreadEvent with name : %S",uszThreadEventString.Buffer);
	ThreadEvent=IoCreateNotificationEvent(&uszThreadEventString, &ThreadEventHandle);
	if (ThreadEvent==NULL)
		DbgPrint("Failed creating ThreadEvent\n");

	KeClearEvent(ThreadEvent);	*/
#endif

	ThreadEventCount=0;
	for (i=0; i<32;i++)
		IDTAddresses[i]=0; //init. I dont know for sure if it gets set to NULL by default so let's be sure

	RtlZeroMemory(&DebugEvents[0],50*sizeof(DebugEvent));
	
	BufferSize=0;
	processlist=NULL;

	OriginalInt1.wHighOffset=0;
	OriginalInt3.wHighOffset=0;

	ChangeRegistersOnBP=FALSE;
	for (i=0;i<4;i++)
		ChangeRegs[i].Active=FALSE;

    //determine if PAE is used
	cr4reg=getCR4();

	if ((cr4reg & 0x20)==0x20)
	{
		PTESize=8; //pae
		PAGE_SIZE_LARGE=0x200000;
		MAX_PDE_POS=0xC0604000;
		
	}
	else
	{
		PTESize=4;
		PAGE_SIZE_LARGE=0x400000;
		MAX_PDE_POS=0xC0301000;
	}

#ifdef CETC
	DbgPrint("Going to initialice CETC\n");
	InitializeCETC();
#endif


	UsesAlternateMethod=FALSE;

    //hideme(DriverObject); //ok, for those that see this, enabling this WILL fuck up try except routines, even in usermode you'll get a blue sreen
	

	// Return success (don't do the devicestring, I need it for unload)
	ExFreePool(BufDriverString);
	ExFreePool(BufProcessEventString);
	ExFreePool(BufThreadEventString);
		
	ZwClose(reg);    
    return ntStatus;
}


NTSTATUS MSJDispatchCreate(IN PDEVICE_OBJECT DeviceObject,
                       IN PIRP Irp)
{
    Irp->IoStatus.Status = STATUS_SUCCESS;
    Irp->IoStatus.Information=0;

    IoCompleteRequest(Irp, IO_NO_INCREMENT);
    return(STATUS_SUCCESS);
}


NTSTATUS MSJDispatchClose(IN PDEVICE_OBJECT DeviceObject,
                       IN PIRP Irp)
{
    Irp->IoStatus.Status = STATUS_SUCCESS;
    Irp->IoStatus.Information=0;

    IoCompleteRequest(Irp, IO_NO_INCREMENT);
    return(STATUS_SUCCESS);
}


NTSTATUS MSJDispatchIoctl(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp)
{
    NTSTATUS ntStatus;
    PIO_STACK_LOCATION     irpStack = IoGetCurrentIrpStackLocation(Irp);

	
    switch(irpStack->Parameters.DeviceIoControl.IoControlCode)
    {
        case IOCTL_CE_READMEMORY:			
			__try
			{
				struct input
				{
					UINT_PTR processid;
					char *startaddress;
					unsigned short int bytestoread;
				} *pinp,inp;
				PEPROCESS selectedprocess;			

				pinp=Irp->AssociatedIrp.SystemBuffer;

				ntStatus=ReadProcessMemory(pinp->processid,NULL,pinp->startaddress,pinp->bytestoread,pinp) ? STATUS_SUCCESS : STATUS_UNSUCCESSFUL;
			}
			__except(1)
			{
				ntStatus = STATUS_UNSUCCESSFUL;
			};
				
            break;

        case IOCTL_CE_WRITEMEMORY:
			__try
			{
				struct input
				{
					UINT_PTR processid;
					void *startaddress;
					unsigned short int bytestowrite;
				} *pinp,inp;
				PEPROCESS selectedprocess;

				pinp=Irp->AssociatedIrp.SystemBuffer;
				ntStatus=WriteProcessMemory(pinp->processid,NULL,pinp->startaddress,pinp->bytestowrite,(PVOID)((UINT_PTR)pinp+sizeof(inp))) ? STATUS_SUCCESS : STATUS_UNSUCCESSFUL;
			}
			__except(1)
			{
				//something went wrong and I don't know what
				ntStatus = STATUS_UNSUCCESSFUL;
			};


				
            break;


		case IOCTL_CE_OPENPROCESS:
			{					
				PEPROCESS selectedprocess;
				PHANDLE pid=Irp->AssociatedIrp.SystemBuffer;
				HANDLE ProcessHandle=0;

				ntStatus=STATUS_SUCCESS;

				__try
				{
					ProcessHandle=0;

					if (PsLookupProcessByProcessId((PVOID)(*pid),&selectedprocess)==STATUS_SUCCESS)
					{		

							DbgPrint("Calling ObOpenObjectByPointer\n");
							ntStatus=ObOpenObjectByPointer ( 
										selectedprocess,
										0,
										NULL,
										PROCESS_ALL_ACCESS,
										*PsProcessType,
										KernelMode, //UserMode,
										&ProcessHandle);

							DbgPrint("ntStatus=%x",ntStatus);
					}
				}
				__except(1)
				{
					ntStatus=STATUS_UNSUCCESSFUL;
				}			
				*pid=ProcessHandle;
				break;
			}

		case IOCTL_CE_OPENTHREAD:
			{
				HANDLE ThreadHandle;
				CLIENT_ID ClientID;
				OBJECT_ATTRIBUTES ObjectAttributes;
				PHANDLE tid;
	
				RtlZeroMemory(&ObjectAttributes,sizeof(OBJECT_ATTRIBUTES));

				ntStatus=STATUS_SUCCESS;
				tid=Irp->AssociatedIrp.SystemBuffer;

				ClientID.UniqueProcess=0;
				ClientID.UniqueThread=*tid;
				ThreadHandle=0;

				__try
				{
					ThreadHandle=0;
					ntStatus=ZwOpenThread(&ThreadHandle,PROCESS_ALL_ACCESS,&ObjectAttributes,&ClientID);									
				}
				__except(1)
				{
					ntStatus=STATUS_UNSUCCESSFUL;
				}
			
				*tid=ThreadHandle;
				

				break;
			}

		case IOCTL_CE_MAKEWRITABLE:
			{
				struct InputBuf
				{
				    PVOID StartAddress;
					ULONG Size;
					BYTE CopyOnWrite;
				} *PInputBuf;

				PInputBuf=Irp->AssociatedIrp.SystemBuffer;
				
				ntStatus=MakeWritable(PInputBuf->StartAddress,PInputBuf->Size,(PInputBuf->CopyOnWrite==1)) ? STATUS_SUCCESS : STATUS_UNSUCCESSFUL; 
				break;
			}


		case IOCTL_CE_QUERY_VIRTUAL_MEMORY:
			{
				struct InputBuf
				{
				    UINT_PTR ProcessID;
					UINT_PTR StartAddress;
				} *PInputBuf;

				struct OutputBuf
				{				
					UINT_PTR length;
					UINT_PTR protection;
				} *POutputBuf;

				
			     
				UINT_PTR BaseAddress;
				PEPROCESS selectedprocess;
				
                ntStatus=STATUS_SUCCESS;
				PInputBuf=Irp->AssociatedIrp.SystemBuffer;
				POutputBuf=Irp->AssociatedIrp.SystemBuffer;

				ntStatus=GetMemoryRegionData(PInputBuf->ProcessID,NULL,(PVOID)(PInputBuf->StartAddress),&(POutputBuf->protection),&(POutputBuf->length),&BaseAddress) ? STATUS_SUCCESS : STATUS_UNSUCCESSFUL;

				
				break;
			}

		case IOCTL_CE_TEST: //just a test to see it's working
			{
				PEPROCESS selectedprocess=NULL;

				DbgPrint("test\n");

				__try
				{
					PMDL mdl=NULL;
					char *buffer;

					mdl = IoAllocateMdl((PVOID)0x00400000, 0x4096, FALSE, TRUE, NULL);
					if (!mdl)
					{
						DbgPrint("Not enough memory dude!!!!\n");
						ntStatus = STATUS_INSUFFICIENT_RESOURCES;
						break;
					}

			        //PsLookupProcessByProcessId((PVOID)696,&selectedprocess);

					DbgPrint("Before\n");
					DbgPrint("mdl->Process=%x",mdl->Process);
					DbgPrint("mdl->MappedSystemVa=%x",mdl->MappedSystemVa);
					DbgPrint("mdl->StartVa=%x",mdl->StartVa);


					//KeAttachProcess((PEPROCESS)selectedprocess);
					MmProbeAndLockPages(mdl, UserMode, IoReadAccess);
					
					DbgPrint("After\n");
					DbgPrint("mdl->Process=%x",mdl->Process);
					DbgPrint("mdl->MappedSystemVa=%x",mdl->MappedSystemVa);
					DbgPrint("mdl->StartVa=%x",mdl->StartVa);
					

					buffer = MmGetSystemAddressForMdlSafe(mdl, NormalPagePriority );
					//KeDetachProcess();

					
					DbgPrint("buffer=%x\n",(ULONG)buffer);
					//MmUnlockPages(mdl);
					//IoFreeMdl(mdl); 
				}
				__except(1)
				{
					DbgPrint("Damn\n");

				}
			
				/*//allocate memory for stack
				unsigned char *x;				
				ULONG cr3callbackstack;
				ULONG cr3;
				int i;

				unsigned long long *PDPTable;
				unsigned long long *PDTable;
				PHYSICAL_ADDRESS physical;


				//allocate memory for the pagetables of the process
				//scan through the pagetables 
				//Get the CR3
				//check if PAE is enabled or not (if 64-bit, yes+pml4)
				//traverse the page tables to find out how many there are
				
				
				cr3=getCR3();
				cr3=cr3 & 0xfffffff0; //cr3 now contains the physical base address

				//from 00000000 to 7fffffff is fake
				//from 80000000 to ffffffff is real
				if (FakeCR3==0)
				{				
					//allocate a pagedirptr table
					PDPTable=ExAllocatePoolWithTag(NonPagedPool,4096,0); //first 2 entries are fake, other 2 copies
					RtlZeroMemory(PDPTable,4096);
					ReadPhysicalMemory((char *)cr3,32,PDPTable);

					//allocate 2 pagedir tables
					PDTable=ExAllocatePoolWithTag(NonPagedPool,4096*2,0);
					RtlZeroMemory(PDTable,4096*2);

					for (i=0; i<((4096*2)/8); i++)
						PDTable[i]=0x83;


					physical=MmGetPhysicalAddress(&PDTable[0]);
					PDPTable[0]=physical.QuadPart;
					PDPTable[1]=physical.QuadPart;

					PDPTable[0]++;
					PDPTable[1]++;

					physical=MmGetPhysicalAddress(&PDPTable[0]);
					FakeCR3=(ULONG)(physical.QuadPart);
					DbgPrint("FakeCR3=%x\n\r",FakeCR3);
				}


				ProtectedProcessID=PsGetCurrentProcessId();
				ProtectedPEProcess=PsGetCurrentProcess();
				ProtectedCR3=getCR3();

				
				x=ExAllocatePoolWithTag(NonPagedPool,4096*4,0);
				RtlZeroMemory(x,4096*4);
				
				cr3callbackstack=(ULONG)x;

				__try
				{
					vmx_register_cr3_callback(8,(ULONG)cr3_change_callback,0x10,cr3callbackstack+(4096*4)-4);
					DbgPrint("cr3 callback registered. cr3callbackstack=%x\n",cr3callbackstack);
				}
				__except(1)
				{
					DbgPrint("Failed registering a cr3 callback\n");
				}
*/


				break;
			}

		case IOCTL_CE_GETPETHREAD:
			{
				
				*(PULONG)Irp->AssociatedIrp.SystemBuffer=getPEThread(*(PULONG)Irp->AssociatedIrp.SystemBuffer);
				ntStatus= STATUS_SUCCESS;
				break;
			}

		case IOCTL_CE_GETPEPROCESS:
			{
				UINT_PTR *processid;
				PEPROCESS selectedprocess;
				processid=Irp->AssociatedIrp.SystemBuffer;

				if (processid==0)
				{
					ntStatus=STATUS_UNSUCCESSFUL;
				}
				else
				{
					if (PsLookupProcessByProcessId((PVOID)(*processid),&selectedprocess)==STATUS_SUCCESS)
						*(PULONG)Irp->AssociatedIrp.SystemBuffer=(ULONG)selectedprocess;
					else
						*(PULONG)Irp->AssociatedIrp.SystemBuffer=0;
				}

				ObDereferenceObject(selectedprocess);

				ntStatus= STATUS_SUCCESS;				
				break;
			}

		case IOCTL_CE_READPHYSICALMEMORY:
			{
				struct input
				{
					char *startaddress;
					UINT_PTR bytestoread;
				} *pinp;
				pinp=Irp->AssociatedIrp.SystemBuffer;

				ntStatus = ReadPhysicalMemory(pinp->startaddress, pinp->bytestoread, pinp);
				break;



			}

		case IOCTL_CE_WRITEPHYSICALMEMORY:
			{
				HANDLE			physmem;
				UNICODE_STRING	physmemString;
				OBJECT_ATTRIBUTES attributes;
				WCHAR			physmemName[] = L"\\device\\physicalmemory";
				UCHAR*			memoryview;

				RtlInitUnicodeString( &physmemString, physmemName );	

            	InitializeObjectAttributes( &attributes, &physmemString, OBJ_CASE_INSENSITIVE, NULL, NULL );	
			    ntStatus=ZwOpenSection( &physmem, SECTION_MAP_READ, &attributes );
				if (ntStatus==STATUS_SUCCESS)
				{
					//hey look, it didn't kill it
					struct input
					{
						char *startaddress;
						UINT_PTR bytestoread;
					} *pinp;

					UCHAR* pinp2;

					UINT_PTR length;
					PHYSICAL_ADDRESS	viewBase;
					UINT_PTR offset;
					UINT_PTR toread;

					
					pinp=Irp->AssociatedIrp.SystemBuffer;
					pinp2=(UCHAR *)pinp;
					viewBase.QuadPart = (ULONGLONG)(pinp->startaddress);					
					
					length=0x2000;//pinp->bytestoread;
					toread=pinp->bytestoread;

					memoryview=NULL;
					ntStatus=ZwMapViewOfSection(
						physmem,  //sectionhandle
						NtCurrentProcess(), //processhandle
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
                        offset=(UINT_PTR)(pinp->startaddress)-(UINT_PTR)viewBase.QuadPart;
						RtlCopyMemory(&memoryview[offset],&pinp2[8],toread);

						ZwUnmapViewOfSection(
							NtCurrentProcess(), //processhandle
							memoryview);
					}

					ZwClose(physmem);
				}

				break;
			}

		case IOCTL_CE_GETPHYSICALADDRESS:
			{
				struct input
				{
					UINT_PTR ProcessID;
					PVOID BaseAddress; 
				} *pinp;
				PEPROCESS selectedprocess;
				PHYSICAL_ADDRESS physical;

				
				ntStatus=STATUS_SUCCESS;
				pinp=Irp->AssociatedIrp.SystemBuffer;

				__try
				{
					//switch to the selected process
					if (PsLookupProcessByProcessId((PVOID)(pinp->ProcessID),&selectedprocess)==STATUS_SUCCESS)	
					{
						KAPC_STATE apc_state;
						RtlZeroMemory(&apc_state,sizeof(apc_state));					
    					KeStackAttachProcess((PVOID)selectedprocess,&apc_state);
                 
						__try
						{
							physical=MmGetPhysicalAddress(pinp->BaseAddress);
						}
						__finally
						{
							KeUnstackDetachProcess(&apc_state);
						}
	

						ObDereferenceObject(selectedprocess);

					}
				}
				__except(1)
				{
						ntStatus=STATUS_UNSUCCESSFUL;
				}

				if (ntStatus==STATUS_SUCCESS)
                    RtlCopyMemory(Irp->AssociatedIrp.SystemBuffer,&physical.QuadPart,8);
				
				
				break;
			}

		case IOCTL_CE_PROTECTME:
			{
#ifdef AMD64
				ntStatus=STATUS_UNSUCCESSFUL;
#else
				struct input
				{
					HANDLE ProcessID; 
					ULONG DenyList;
					ULONG GlobalDenyList; //ignored if it is a includelist
					ULONG ListSize;
				} *pinp;

				UINT_PTR NextProcess;
				UINT_PTR PreviousProcess;


				pinp=Irp->AssociatedIrp.SystemBuffer;

				
				if (ModuleList!=NULL)
					MmFreeNonCachedMemory(ModuleList,ModuleListSize);

				ModuleList=NULL;
				ModuleListSize=0;

				if (pinp->ListSize>0)
				{
					ModuleList=MmAllocateNonCachedMemory(pinp->ListSize);
					if (ModuleList!=NULL)
					{
						__try
						{
							
                            RtlCopyMemory(ModuleList,(PVOID)((UINT_PTR)(&(pinp->ListSize))+sizeof(pinp->ListSize)),pinp->ListSize);
							ModuleListSize=pinp->ListSize;
						}
						__except(1)
						{
						}
					}
					
				}

				DenyList=pinp->DenyList==1;
				GlobalDenyList=pinp->GlobalDenyList==1;

				ProtectedProcessID=pinp->ProcessID;
				PsLookupProcessByProcessId((PVOID)(pinp->ProcessID),&ProtectedPEProcess);			

				if (ActiveLinkOffset!=0)
				{
					NextProcess=*(PUINT_PTR)((UINT_PTR)ProtectedPEProcess+ActiveLinkOffset)-ActiveLinkOffset;
					PreviousProcess=*(PUINT_PTR)((UINT_PTR)ProtectedPEProcess+ActiveLinkOffset+4)-ActiveLinkOffset;
	
					*(PUINT_PTR)(PreviousProcess+ActiveLinkOffset)=*(PULONG)((UINT_PTR)ProtectedPEProcess+ActiveLinkOffset); //the previous process points to me next process
					*(PUINT_PTR)(NextProcess+ActiveLinkOffset+4)=*(PULONG)((UINT_PTR)ProtectedPEProcess+ActiveLinkOffset+4); //the next process points to the previous process

					*(PUINT_PTR)((UINT_PTR)ProtectedPEProcess+ActiveLinkOffset)=(UINT_PTR)ProtectedPEProcess+ActiveLinkOffset;
					*(PUINT_PTR)((UINT_PTR)ProtectedPEProcess+ActiveLinkOffset+4)=(UINT_PTR)ProtectedPEProcess+ActiveLinkOffset;			
				}


				if (!ProtectOn)
				{
					//unlink this process from the activeprocess list

					if (!ImageNotifyRoutineLoaded)
						ImageNotifyRoutineLoaded=(PsSetLoadImageNotifyRoutine(LoadImageNotifyRoutine)==STATUS_SUCCESS);


					//Hook
					OldZwOpenProcess=(ZWOPENPROCESS)SYSTEMSERVICE(ZwOpenProcess);
					OldZwQuerySystemInformation=(ZWQUERYSYSTEMINFORMATION)SYSTEMSERVICE(ZwQuerySystemInformation);


					if ((KeServiceDescriptorTableShadow!=NULL) && (NtUserBuildHwndList_callnumber!=0) && (NtUserBuildHwndList_callnumber!=0) && (NtUserFindWindowEx_callnumber!=0) && (NtUserGetForegroundWindow_callnumber!=0))
					{
						OldNtUserQueryWindow=(NTUSERQUERYWINDOW)KeServiceDescriptorTableShadow->ServiceTable[NtUserQueryWindow_callnumber];						
						OldNtUserBuildHwndList=(NTUSERBUILDHWNDLIST)KeServiceDescriptorTableShadow->ServiceTable[NtUserBuildHwndList_callnumber];
						OldNtUserFindWindowEx=(NTUSERFINDWINDOWEX)KeServiceDescriptorTableShadow->ServiceTable[NtUserFindWindowEx_callnumber];
                        OldNtUserGetForegroundWindow=(NTUSERGETFOREGROUNDWINDOW)KeServiceDescriptorTableShadow->ServiceTable[NtUserGetForegroundWindow_callnumber];

						//now a extra check before I screw up the system
						if (((UCHAR)KeServiceDescriptorTableShadow->ServiceTable[NtUserBuildHwndList_callnumber]!=0x1c) || 
						    ((UCHAR)KeServiceDescriptorTableShadow->ServiceTable[NtUserQueryWindow_callnumber]!=0x08)  ||
							((UCHAR)KeServiceDescriptorTableShadow->ServiceTable[NtUserFindWindowEx_callnumber]!=0x14) ||
							((UCHAR)KeServiceDescriptorTableShadow->ServiceTable[NtUserGetForegroundWindow_callnumber]!=0x0)
							)
							
						{
							//NOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO!
							KeServiceDescriptorTableShadow=NULL; //disable it
							NtUserBuildHwndList_callnumber=0;
							NtUserQueryWindow_callnumber=0;
							NtUserFindWindowEx_callnumber=0;
							NtUserGetForegroundWindow_callnumber=0;							
						}	
					} else KeServiceDescriptorTableShadow=NULL; //do not enable hooking. All have to work, else none
					ProtectOn=TRUE;
				}

				
				__asm
				{
					cli 
					mov eax,CR0
					and eax,not 0x10000
					mov CR0,eax
				}
				(ZWOPENPROCESS)(SYSTEMSERVICE(ZwOpenProcess))=NewZwOpenProcess;
				(ZWQUERYSYSTEMINFORMATION)(SYSTEMSERVICE(ZwQuerySystemInformation))=NewZwQuerySystemInformation;


		        if ((NtUserQueryWindow_callnumber!=0) && (KeServiceDescriptorTableShadow!=NULL))
				  (NTUSERQUERYWINDOW)(KeServiceDescriptorTableShadow->ServiceTable[NtUserQueryWindow_callnumber])=NewNtUserQueryWindow;

		        if ((NtUserFindWindowEx_callnumber!=0) && (KeServiceDescriptorTableShadow!=NULL))
				  (NTUSERFINDWINDOWEX)(KeServiceDescriptorTableShadow->ServiceTable[NtUserFindWindowEx_callnumber])=NewNtUserFindWindowEx;

		        if ((NtUserGetForegroundWindow_callnumber!=0) && (KeServiceDescriptorTableShadow!=NULL))
				  (NTUSERGETFOREGROUNDWINDOW)(KeServiceDescriptorTableShadow->ServiceTable[NtUserGetForegroundWindow_callnumber])=NewNtUserGetForegroundWindow;

				if ((NtUserBuildHwndList_callnumber!=0) && (KeServiceDescriptorTableShadow!=NULL))
                  (NTUSERBUILDHWNDLIST)(KeServiceDescriptorTableShadow->ServiceTable[NtUserBuildHwndList_callnumber])=NewNtUserBuildHwndList;


				__asm
				{
					mov eax,CR0
					xor eax,0x10000
					mov CR0,eax
					sti
				}						

				ntStatus=STATUS_SUCCESS;
#endif //not amd64
				break;
			}	

		case IOCTL_CE_DONTPROTECTME:
			{
				//Unhook();
				if (ProtectOn)
					ntStatus=STATUS_UNSUCCESSFUL;
				else
					ntStatus=STATUS_SUCCESS;

				//ProtectOn=FALSE;

				break;
			}

		case IOCTL_CE_SETSDTADDRESS:
			{
				struct input
				{
					int table; //0=SDT, 1=SSDT
				  	int nr;	
					ULONG address;
					UCHAR paramcount;
				} *pinp;
				pinp=Irp->AssociatedIrp.SystemBuffer;
			

				__asm
				{
					cli 
					mov eax,CR0
					and eax,not 0x10000
					mov CR0,eax
				}
				if (pinp->table==0)
				{
					(ULONG)(KeServiceDescriptorTable->ServiceTable[pinp->nr])=pinp->address;
					(UCHAR)(KeServiceDescriptorTable->ArgumentTable[pinp->nr])=pinp->paramcount;
				}
				else if (pinp->table==1)
				{
					(ULONG)(KeServiceDescriptorTableShadow->ServiceTable[pinp->nr])=pinp->address;
					(UCHAR)(KeServiceDescriptorTableShadow->ArgumentTable[pinp->nr])=pinp->paramcount;
				}

				__asm
				{
					mov eax,CR0
					xor eax,0x10000
					mov CR0,eax
					sti
				}
				ntStatus=STATUS_SUCCESS;
				break;
			}


		case IOCTL_CE_GETSDTADDRESS:
			{
				struct input
				{
					int table; //0=SDT, 1=SSDT
				  	int nr;	
				} *pinp;
				struct output
				{
					ULONG address;
					UCHAR paramcount;
				} *poutp;
				int table;
				int nr;
				pinp=Irp->AssociatedIrp.SystemBuffer;
				table=pinp->table;
				nr=pinp->nr;
				poutp=Irp->AssociatedIrp.SystemBuffer;


				if (table==0)
				{
					poutp->address=(ULONG)(KeServiceDescriptorTable->ServiceTable[nr]);
					poutp->paramcount=(UCHAR)(KeServiceDescriptorTable->ArgumentTable[nr]);
				}
				else if (table==1)
				{
					poutp->address=(ULONG)(KeServiceDescriptorTableShadow->ServiceTable[nr]);
					poutp->paramcount=(UCHAR)(KeServiceDescriptorTableShadow->ArgumentTable[nr]);
				}

				ntStatus=STATUS_SUCCESS;
				break;
			}

		case IOCTL_CE_GETCR0:
			{
				ULONG cr0reg=0;
				__asm
				{
					mov eax,cr0
					mov cr0reg,eax
				}
				

				*(ULONG*)Irp->AssociatedIrp.SystemBuffer=cr0reg;
				ntStatus=STATUS_SUCCESS;

				break;
			}

		case IOCTL_CE_GETCR4:
			{
				//seems CR4 isn't seen as a register...
				ULONG cr4reg=0;
				cr4reg=getCR4();
				*(ULONG*)Irp->AssociatedIrp.SystemBuffer=cr4reg;
				ntStatus=STATUS_SUCCESS;

				break;
			}

		case IOCTL_CE_SETCR4:
			{
				//seems CR4 isn't seen as a register...
				ULONG cr4reg=*(ULONG*)Irp->AssociatedIrp.SystemBuffer;
				setCR4(cr4reg);
				ntStatus=STATUS_SUCCESS;
				break;
			}

		case IOCTL_CE_GETCR3:
			{
#ifndef AMD64
				ULONG cr3reg=0;
				PEPROCESS selectedprocess;


				ntStatus=STATUS_SUCCESS;

				//switch context to the selected process.  (processid is stored in the systembuffer)
				if (PsLookupProcessByProcessId((PVOID)(*(ULONG*)Irp->AssociatedIrp.SystemBuffer),&selectedprocess)==STATUS_SUCCESS)	
				{
					__try
					{
						KAPC_STATE apc_state;
						RtlZeroMemory(&apc_state,sizeof(apc_state));					
    					KeStackAttachProcess((PVOID)selectedprocess,&apc_state);

						__try
						{
							cr3reg=getCR3();

						}
						__finally
						{
							KeUnstackDetachProcess(&apc_state);
						}

					}
					__except(1)
					{
						ntStatus=STATUS_UNSUCCESSFUL;
						break;
					}

				}

				*(ULONG*)Irp->AssociatedIrp.SystemBuffer=cr3reg;

#else
				ntStatus=STATUS_UNSUCCESSFUL; //not supported yet
#endif
				break;
			}

		case IOCTL_CE_SETCR3:
			{
#ifndef AMD64
				struct input
				{
					ULONG ProcessID;
					ULONG NewCR3; 
				} *pinp;
				ULONG cr3reg;

    			PEPROCESS selectedprocess;


				ntStatus=STATUS_SUCCESS;
				pinp=Irp->AssociatedIrp.SystemBuffer;
                cr3reg=pinp->NewCR3;

				//switch context to the selected process.  (processid is stored in the systembuffer)
				if (PsLookupProcessByProcessId((PVOID)(pinp->ProcessID),&selectedprocess)==STATUS_SUCCESS)	
				{
					__try
					{
						KAPC_STATE apc_state;
						RtlZeroMemory(&apc_state,sizeof(apc_state));					
    					KeStackAttachProcess((PKPROCESS)selectedprocess,&apc_state);

						__try
						{
							__asm
							{
								mov eax,cr3reg
								mov CR3,eax
							}
						}
						__finally
						{
							KeUnstackDetachProcess(&apc_state);
						}

					}
					__except(1)
					{
						ntStatus=STATUS_UNSUCCESSFUL;
						break;
					}

				}

				
#else
				ntStatus=STATUS_UNSUCCESSFUL; //not supported yet
#endif
				break;
			}

		case IOCTL_CE_GETSDT:
			{
				//returns the address of KeServiceDescriptorTable
				ntStatus=STATUS_SUCCESS;
				*(UINT_PTR*)Irp->AssociatedIrp.SystemBuffer=(UINT_PTR)KeServiceDescriptorTable;
				break;
			}	


		case IOCTL_CE_GETIDT:
			{
				//returns the address of the IDT of the current CPU
				IDT idt;
				RtlZeroMemory(&idt,sizeof(IDT));
				GetIDT(&idt);
				RtlCopyMemory(Irp->AssociatedIrp.SystemBuffer,&idt,sizeof(IDT)); //copy idt
				ntStatus=STATUS_SUCCESS;
			
				break;
			}	

		case IOCTL_CE_GETGDT:
			{
				//returns the address of the IDT of the current CPU
				GDT gdt;
				RtlZeroMemory(&gdt,sizeof(GDT));
				GetGDT(&gdt);
				RtlCopyMemory(Irp->AssociatedIrp.SystemBuffer,&gdt,sizeof(GDT)); //copy gdt
				ntStatus=STATUS_SUCCESS;
			
				break;
			}	

		case IOCTL_CE_HOOKINTS:
			{
				IDT idt;
				BYTE Processor;
				GetIDT(&idt);

				//DbgPrint("IOCTL_CE_HOOKINTS\n");

				Processor=*(PCHAR)Irp->AssociatedIrp.SystemBuffer;
				ntStatus=STATUS_SUCCESS;

				if (IDTAddresses[Processor]==0)
				{					
					//DbgPrint("Stored the IDT of this cpu\n");
					IDTAddresses[Processor]=(UINT_PTR)idt.vector;
				}

				//DbgPrint("Calling HookInt1()\n");
				if (HookInt1() /*&& HookInt3()*/)
					ntStatus=STATUS_SUCCESS;
				else
				    ntStatus=STATUS_UNSUCCESSFUL;

				break;
			}

		case IOCTL_CE_ISUSINGALTERNATEMETHOD:
			{
				*(PBOOLEAN)(Irp->AssociatedIrp.SystemBuffer)=UsesAlternateMethod;
				break;

			}

		case IOCTL_CE_USEALTERNATEMETHOD:
			{
				struct output
				{
					ULONG int1apihook; //address of the in1apihook function
					ULONG OriginalInt1handler; //space to write the int1 handler
				} *poutp;

				DbgPrint("IOCTL_CE_USEALTERNATEMETHOD: (ULONG)int1apihook=%x (ULONG)OriginalInt1handler=%x",(ULONG)int1apihook,(ULONG)OriginalInt1handler);

				poutp=Irp->AssociatedIrp.SystemBuffer;
				poutp->int1apihook=(ULONG)int1apihook;
				poutp->OriginalInt1handler=(ULONG)OriginalInt1handler;

				UsesAlternateMethod=TRUE;

				ntStatus=STATUS_SUCCESS;
				break;
			}
		case IOCTL_CE_SETGLOBALDEBUGSTATE:
			{
				struct intput
				{
					ULONG newstate;
				} *pinp;
				pinp=Irp->AssociatedIrp.SystemBuffer;

				globaldebug=pinp->newstate;
			}

		case IOCTL_CE_STOPDEBUGGING:
			{
				StopDebugging();
				ntStatus=STATUS_SUCCESS;
				break;
			}

		case IOCTL_CE_STOP_DEBUGPROCESS_CHANGEREG:
			{
				struct input
				{
					int debugreg;					
				} *pinp;

				pinp=Irp->AssociatedIrp.SystemBuffer;

				StopChangeRegOnBP(pinp->debugreg);
				break;
			}

		case IOCTL_CE_DEBUGPROCESS_CHANGEREG:
			{
				struct input
				{
					DWORD ProcessID;
					int debugreg;
					ChangeReg CR;
				} *pinp;

				pinp=Irp->AssociatedIrp.SystemBuffer;
				ChangeRegOnBP(pinp->ProcessID, pinp->debugreg, &(pinp->CR));
				ntStatus=STATUS_SUCCESS; //always succeeds, else the memory was unwritable and thus a blue screen of death

				break;
			}

		case IOCTL_CE_DEBUGPROCESS:
			{
				struct input
				{					
					DWORD	ProcessID;
					DWORD	Address;
					BYTE	Length;
					BYTE	RWE;
				} *pinp;

			
				pinp=Irp->AssociatedIrp.SystemBuffer;
				if (DebugProcess(pinp->ProcessID, pinp->Address, pinp->Length,pinp->RWE))
				{
					ntStatus=STATUS_SUCCESS;
				}
				else
				{
					ntStatus=STATUS_UNSUCCESSFUL;
				}

				break;

			}

		case IOCTL_CE_RETRIEVEDEBUGDATA:
			{
				
				
				*(PUCHAR)Irp->AssociatedIrp.SystemBuffer=BufferSize;	
				RtlCopyMemory((PVOID)((UINT_PTR)Irp->AssociatedIrp.SystemBuffer+1),&DebugEvents[0],BufferSize*sizeof(DebugEvent));
				BufferSize=0; //there's room for new events
				ntStatus=STATUS_SUCCESS;
				
				
				break;
			}

		case IOCTL_CE_STARTPROCESSWATCH:
			{
				KIRQL OldIrql;


				KeAcquireSpinLock(&ProcesslistSL,&OldIrql);
				ProcessEventCount=0;				
				KeReleaseSpinLock(&ProcesslistSL,OldIrql);
				

				DbgPrint("IOCTL_CE_STARTPROCESSWATCH\n");
				
				if (CreateProcessNotifyRoutineEnabled==FALSE)
				{
					DbgPrint("calling PsSetCreateProcessNotifyRoutine\n");
				    CreateProcessNotifyRoutineEnabled=(PsSetCreateProcessNotifyRoutine(CreateProcessNotifyRoutine,FALSE)==STATUS_SUCCESS);
					CreateThreadNotifyRoutineEnabled=(PsSetCreateThreadNotifyRoutine(CreateThreadNotifyRoutine)==STATUS_SUCCESS);
				}

				ntStatus=(CreateProcessNotifyRoutineEnabled) ? STATUS_SUCCESS : STATUS_UNSUCCESSFUL;

				if (ntStatus==STATUS_SUCCESS)
					DbgPrint("CreateProcessNotifyRoutineEnabled worked\n");
				else
					DbgPrint("CreateProcessNotifyRoutineEnabled failed\n");
					

				break;
			}

		case IOCTL_CE_GETPROCESSEVENTS:
			{
				KIRQL OldIrql;
				
				KeAcquireSpinLock(&ProcesslistSL,&OldIrql);

				*(PUCHAR)Irp->AssociatedIrp.SystemBuffer=ProcessEventCount;	
				RtlCopyMemory((PVOID)((UINT_PTR)Irp->AssociatedIrp.SystemBuffer+1),&ProcessEventdata[0],ProcessEventCount*sizeof(ProcessEventdta));
				ProcessEventCount=0; //there's room for new events

				KeReleaseSpinLock(&ProcesslistSL,OldIrql);

				ntStatus=STATUS_SUCCESS;
				break;
			}

		case IOCTL_CE_GETTHREADEVENTS:
			{
				KIRQL OldIrql;
				
				KeAcquireSpinLock(&ProcesslistSL,&OldIrql);

				*(PUCHAR)Irp->AssociatedIrp.SystemBuffer=ThreadEventCount;	
				RtlCopyMemory((PVOID)((UINT_PTR)Irp->AssociatedIrp.SystemBuffer+1),&ThreadEventData[0],ThreadEventCount*sizeof(ThreadEventDta));
				ThreadEventCount=0; //there's room for new events

				KeReleaseSpinLock(&ProcesslistSL,OldIrql);

				ntStatus=STATUS_SUCCESS;
				break;
			}


		case IOCTL_CE_CREATEAPC:
			{
				struct input
				{
					ULONG threadid;
					PVOID addresstoexecute;										
				} *inp;
				inp=Irp->AssociatedIrp.SystemBuffer;

				CreateRemoteAPC(inp->threadid,inp->addresstoexecute);
				ntStatus=STATUS_SUCCESS;
				break;
			}

		case IOCTL_CE_SUSPENDTHREAD:
			{
				struct input
				{
					ULONG threadid;							
				} *inp;
				inp=Irp->AssociatedIrp.SystemBuffer;

				DbgPrint("CE_SUSPENDTHREAD\n");

				DBKSuspendThread(inp->threadid);
				ntStatus=STATUS_SUCCESS;
				break;
			}

		case IOCTL_CE_RESUMETHREAD:            
			{
				struct input
				{
					ULONG threadid;							
				} *inp;
				inp=Irp->AssociatedIrp.SystemBuffer;

				DbgPrint("CE_RESUMETHREAD\n");

				DBKResumeThread(inp->threadid);
				ntStatus=STATUS_SUCCESS;
				break;
            }

		case IOCTL_CE_SUSPENDPROCESS:
			{
				struct input
				{
					ULONG processid;							
				} *inp;
				inp=Irp->AssociatedIrp.SystemBuffer;

				DbgPrint("IOCTL_CE_SUSPENDPROCESS\n");
				DBKSuspendProcess(inp->processid);
				ntStatus=STATUS_SUCCESS;
				break;
			}

		case IOCTL_CE_RESUMEPROCESS:            
			{
				struct input
				{
					ULONG processid;							
				} *inp;
				inp=Irp->AssociatedIrp.SystemBuffer;

                DbgPrint("IOCTL_CE_RESUMEPROCESS\n");

				DBKResumeProcess(inp->processid);
				ntStatus=STATUS_SUCCESS;
				break;
            }

case IOCTL_CE_ALLOCATEMEM:
			{
				struct input
				{
					ULONG ProcessID;
					PVOID BaseAddress;
					ULONG Size;
					ULONG AllocationType;
					ULONG Protect;
				} *inp;
				PEPROCESS selectedprocess;

				PVOID BaseAddress;
				SIZE_T RegionSize;


				inp=Irp->AssociatedIrp.SystemBuffer;
				BaseAddress=inp->BaseAddress;
				RegionSize=inp->Size;




				if (PsLookupProcessByProcessId((PVOID)(inp->ProcessID),&selectedprocess)==STATUS_SUCCESS)	
				{
					__try
					{
						KAPC_STATE apc_state;
						RtlZeroMemory(&apc_state,sizeof(apc_state));					
    						KeAttachProcess((PVOID)selectedprocess); //local process is much more fun!!!!

						DbgPrint("Switched Process\n");
						__try
						{
							DbgPrint("Calling ZwAllocateVirtualMemory\n");
							ntStatus=ZwAllocateVirtualMemory((HANDLE)-1,&BaseAddress,0,  &RegionSize,      inp->AllocationType,    inp->Protect);

							if ((ntStatus==STATUS_SUCCESS) && (HiddenDriver))
							{
								//initialize the memory with crap so it becomes paged
								int i;
								char *x;
								x=BaseAddress;
								for (i=0; i < (int)RegionSize;i++)
									x[i]=(unsigned char)i;
							}
							
							DbgPrint("ntStatus=%x\n");
							DbgPrint("BaseAddress=%p\n",BaseAddress);
							DbgPrint("RegionSize=%x\n",RegionSize);
							*(PULONG)Irp->AssociatedIrp.SystemBuffer=(ULONG)BaseAddress;

						}
						__finally
						{
							KeDetachProcess();
						}

					}
					__except(1)
					{
						ntStatus=STATUS_UNSUCCESSFUL;
						break;
					}


					ObDereferenceObject(selectedprocess);
				}

				break;
			}

		case IOCTL_CE_ALLOCATEMEM_NONPAGED:
			{
				struct input
				{
					ULONG Size;
				} *inp;
				PVOID address;
				char *x;
				int i;
				int size;

				inp=Irp->AssociatedIrp.SystemBuffer;
				size=inp->Size;

				address=ExAllocatePoolWithTag(NonPagedPool,size,0);
				*(PULONG)Irp->AssociatedIrp.SystemBuffer=(ULONG)address;

				if (address==0)
					ntStatus=STATUS_UNSUCCESSFUL;
				else
				{
					DbgPrint("Alloc success. Cleaning memory... (size=%d)\n",size);					
					
					x=address;
					DbgPrint("x=%p\n",x);
					for (i=0; i<size; i++)					
						x[i]=0;
					
					ntStatus=STATUS_SUCCESS;
				}

				break;
			}

		case IOCTL_CE_GETPROCADDRESS:
			{
				struct input
				{
					PCWSTR s;
				} *inp;
				UNICODE_STRING y;
				PVOID x;

				inp=Irp->AssociatedIrp.SystemBuffer;

				RtlInitUnicodeString(&y, inp->s);
				x=MmGetSystemRoutineAddress(&y);			

				RtlCopyMemory(Irp->AssociatedIrp.SystemBuffer,&x,4);
				ntStatus=STATUS_SUCCESS;

				break;
			}

		case IOCTL_CE_MAKEKERNELCOPY:
			{
				struct input
				{
					ULONG Base;
					ULONG KernelSize;
				} *inp;
				DbgPrint("IOCTL_CE_MAKEKERNELCOPY");
				inp=Irp->AssociatedIrp.SystemBuffer;
				ntStatus=makeKernelCopy(inp->Base, inp->KernelSize);			
				break;
			}

		case IOCTL_CE_GETVERSION:
			{
				*(PULONG)Irp->AssociatedIrp.SystemBuffer=dbkversion;	
				ntStatus=STATUS_SUCCESS;
				break;
			}

		case IOCTL_CE_INITIALIZE:
			{
				//find the KeServiceDescriptorTableShadow 
				struct input
				{
					ULONG AddressOfWin32K;
					ULONG SizeOfWin32K;
					ULONG NtUserBuildHwndList_callnumber;
					ULONG NtUserQueryWindow_callnumber;
					ULONG NtUserFindWindowEx_callnumber;
					ULONG NtUserGetForegroundWindow_callnumber;
					ULONG ActiveLinkOffset;
					ULONG ProcessNameOffset;
					ULONG DebugportOffset;	
					ULONG ProcessEvent;
					ULONG ThreadEvent;
  				} *pinp;

		
				int i;

				PSERVICE_DESCRIPTOR_TABLE PossibleKeServiceDescriptorTableShow; //long name's are FUN!!!!
				PossibleKeServiceDescriptorTableShow=KeServiceDescriptorTable;

				ntStatus=STATUS_UNSUCCESSFUL;                
				pinp=Irp->AssociatedIrp.SystemBuffer;
				NtUserBuildHwndList_callnumber=pinp->NtUserBuildHwndList_callnumber;
				NtUserQueryWindow_callnumber=pinp->NtUserQueryWindow_callnumber;
				NtUserFindWindowEx_callnumber=pinp->NtUserFindWindowEx_callnumber;
				NtUserGetForegroundWindow_callnumber=pinp->NtUserGetForegroundWindow_callnumber;

				ActiveLinkOffset=pinp->ActiveLinkOffset;
				ProcessNameOffset=pinp->ProcessNameOffset;
				DebugportOffset=pinp->DebugportOffset;


				//referencing event handles to objects
				ObReferenceObjectByHandle((HANDLE)pinp->ProcessEvent, EVENT_ALL_ACCESS, NULL,KernelMode, &ProcessEvent, NULL); 
				ObReferenceObjectByHandle((HANDLE)pinp->ThreadEvent, EVENT_ALL_ACCESS, NULL,KernelMode, &ThreadEvent, NULL); 
				

				//in win2k sp4 the distance is even bigger than -6, at least 21 entries down to find it

				i=-25;//takes some longer to load now....
				while (i<25)
				{
					if (IsAddressSafe((UINT_PTR)&PossibleKeServiceDescriptorTableShow[i])) //dont want to crash for a page pault now do we?
 					{
						/*
						look for a entry that looks like:
						unsigned int *ServiceTable=Region of Win32K.sys
						unsigned int *ServiceCounterTableBase=00000000 but lets be safe and dont check it in case of a checked build
						unsigned int NumberOfServices=smaller than 0xffff;
						unsigned char *ParamTableBase=Region of Win32K.sys;
						*/
						if (((UINT_PTR)PossibleKeServiceDescriptorTableShow[i].ServiceTable>=pinp->AddressOfWin32K) &&
							((UINT_PTR)PossibleKeServiceDescriptorTableShow[i].ServiceTable<(pinp->AddressOfWin32K+pinp->SizeOfWin32K)) &&
							
							((UINT_PTR)PossibleKeServiceDescriptorTableShow[i].ArgumentTable>=pinp->AddressOfWin32K) &&
							((UINT_PTR)PossibleKeServiceDescriptorTableShow[i].ArgumentTable<(pinp->AddressOfWin32K+pinp->SizeOfWin32K)) &&

							(PossibleKeServiceDescriptorTableShow[i].TableSize<0xffff)


							)
						{
							//found it!!!!!!
							KeServiceDescriptorTableShadow=&PossibleKeServiceDescriptorTableShow[i];
							ntStatus=STATUS_SUCCESS;							
                            *(UINT_PTR*)Irp->AssociatedIrp.SystemBuffer=(UINT_PTR)KeServiceDescriptorTableShadow;

							DbgPrint("KeServiceDescriptorTableShadow[0]=%p",&KeServiceDescriptorTableShadow[0]);
							DbgPrint("KeServiceDescriptorTableShadow[1]=%p",&KeServiceDescriptorTableShadow[1]);
							DbgPrint("KeServiceDescriptorTableShadow[2]=%p",&KeServiceDescriptorTableShadow[2]);
							DbgPrint("KeServiceDescriptorTableShadow[3]=%p",&KeServiceDescriptorTableShadow[3]);

							AddSystemServices();
							break;
						}


					}
					i++;
				}				                

				break;
			}

		case IOCTL_CE_VMXCONFIG:
			{
				struct input
				{
					ULONG Virtualization_Enabled;
					ULONG Password1;
					ULONG Password2;
  				} *pinp;
				

				DbgPrint("IOCTL_CE_VMXCONFIG called\n");	
				ntStatus=STATUS_SUCCESS;

				pinp=Irp->AssociatedIrp.SystemBuffer;

				if (pinp->Virtualization_Enabled)
				{
					vmx_password1=pinp->Password1;
					vmx_password2=pinp->Password2;
					__try
					{
						vmx_version=vmx_getversion();
						DbgPrint("Still here, so vmx is loaded. vmx_version=%d\n",vmx_version);	
						vmxusable = 1;
					}
					__except(1)
					{
						DbgPrint("Exception happened. This means no vmx installed, or one of the passwords is wrong\n");
						ntStatus = STATUS_UNSUCCESSFUL;

						vmxusable = 0;
					};
				}
				else
				{
					DbgPrint("Virtualization_Enabled=0\n");
					vmxusable=0;
				}
				
				break;
			}



        default:
            break;
    }

    Irp->IoStatus.Status = ntStatus;
    
    // Set # of bytes to copy back to user-mode...
    if(ntStatus == STATUS_SUCCESS)
        Irp->IoStatus.Information = irpStack->Parameters.DeviceIoControl.OutputBufferLength;
    else
        Irp->IoStatus.Information = 0;

    IoCompleteRequest(Irp, IO_NO_INCREMENT);
    return ntStatus;
}


typedef NTSTATUS (*PSRCTNR)(__in PCREATE_THREAD_NOTIFY_ROUTINE NotifyRoutine);
PSRCTNR PsRemoveCreateThreadNotifyRoutine2;

typedef NTSTATUS (*PSRLINR)(__in PLOAD_IMAGE_NOTIFY_ROUTINE NotifyRoutine);
PSRLINR PsRemoveLoadImageNotifyRoutine2;



void MSJUnloadDriver(PDRIVER_OBJECT DriverObject)
{
	if (ProtectOn)
		return;

	if (KeServiceDescriptorTableShadow && registered) //I can't unload without a shadotw table (system service registered)
	{
		//1 since my routine finds the address of the 2nd element
		KeServiceDescriptorTableShadow[1].ArgumentTable=NULL;
		KeServiceDescriptorTableShadow[1].CounterTable=NULL;
		KeServiceDescriptorTableShadow[1].ServiceTable=NULL;
		KeServiceDescriptorTableShadow[1].TableSize=0;

		KeServiceDescriptorTable[2].ArgumentTable=NULL;
		KeServiceDescriptorTable[2].CounterTable=NULL;
		KeServiceDescriptorTable[2].ServiceTable=NULL;
		KeServiceDescriptorTable[2].TableSize=0;
	}
	
	
	if (OriginalInt1.wHighOffset!=0) //hidden feature: unloading WILL be able to stop the hook so it can be enabled a second time (e.g something overwrote my hook)
	{
		int	i;		
		for (i=0;i<32;i++)
		{
			if (IDTAddresses[i]!=0)
			{							
				((PINT_VECTOR)(IDTAddresses[i]))[1]=OriginalInt1;
				//((PINT_VECTOR)(IDTAddresses[i]))[3]=OriginalInt3;
			};
		};
	}


	if ((CreateProcessNotifyRoutineEnabled) || (ImageNotifyRoutineLoaded)) 
	{
		PVOID x;
		RtlInitUnicodeString(&uszDeviceString, L"PsRemoveCreateThreadNotifyRoutine");
		PsRemoveCreateThreadNotifyRoutine2=MmGetSystemRoutineAddress(&uszDeviceString);

		RtlInitUnicodeString(&uszDeviceString, L"PsRemoveCreateThreadNotifyRoutine");
		PsRemoveLoadImageNotifyRoutine2=MmGetSystemRoutineAddress(&uszDeviceString);


		
		RtlInitUnicodeString(&uszDeviceString, L"ObOpenObjectByName");
		x=MmGetSystemRoutineAddress(&uszDeviceString);
		
		DbgPrint("ObOpenObjectByName=%p\n",x);
			

		if ((PsRemoveCreateThreadNotifyRoutine2) && (PsRemoveLoadImageNotifyRoutine2))
		{
			DbgPrint("Stopping processwatch\n");

			if (CreateProcessNotifyRoutineEnabled)
			{
				PsSetCreateProcessNotifyRoutine(CreateProcessNotifyRoutine,TRUE);
				PsRemoveCreateThreadNotifyRoutine2(CreateThreadNotifyRoutine);
			}

			if (ImageNotifyRoutineLoaded)
				PsRemoveLoadImageNotifyRoutine2(LoadImageNotifyRoutine);
		}
		else return;  //leave now!!!!!		
	}

	//Unhook();



    IoDeleteDevice(DriverObject->DeviceObject);
	//ZwClose(ProcessEventHandle);

#ifdef CETC
#ifndef CETC_RELEASE
	UnloadCETC(); //not possible in the final build
#endif
#endif

#ifndef CETC_RELEASE
	IoDeleteSymbolicLink(&uszDeviceString);
	ExFreePool(BufDeviceString);
#endif

}

void Unhook(void)
{
#ifndef AMD64
    if (ProtectOn)
	{
        __asm
		{
			cli 
			mov eax,CR0
			and eax,not 0x10000 //disable bit
			mov CR0,eax
		}
		(ZWOPENPROCESS)(SYSTEMSERVICE(ZwOpenProcess))=OldZwOpenProcess;
		(ZWQUERYSYSTEMINFORMATION)(SYSTEMSERVICE(ZwQuerySystemInformation))=OldZwQuerySystemInformation;

        if ((NtUserBuildHwndList_callnumber!=0) && (KeServiceDescriptorTableShadow!=NULL))
          (NTUSERBUILDHWNDLIST)(KeServiceDescriptorTableShadow->ServiceTable[NtUserBuildHwndList_callnumber])=OldNtUserBuildHwndList;

        if ((NtUserQueryWindow_callnumber!=0) && (KeServiceDescriptorTableShadow!=NULL))
          (NTUSERQUERYWINDOW)(KeServiceDescriptorTableShadow->ServiceTable[NtUserQueryWindow_callnumber])=OldNtUserQueryWindow;

        if ((NtUserFindWindowEx_callnumber!=0) && (KeServiceDescriptorTableShadow!=NULL))
          (NTUSERFINDWINDOWEX)(KeServiceDescriptorTableShadow->ServiceTable[NtUserFindWindowEx_callnumber])=OldNtUserFindWindowEx;

        if ((NtUserGetForegroundWindow_callnumber!=0) && (KeServiceDescriptorTableShadow!=NULL))
		  (NTUSERGETFOREGROUNDWINDOW)(KeServiceDescriptorTableShadow->ServiceTable[NtUserGetForegroundWindow_callnumber])=OldNtUserGetForegroundWindow;

		__asm
		{
			mov eax,CR0
			or  eax,0x10000 //re-enable this bit
			mov CR0,eax
			sti
		}
		ProtectOn=FALSE;
	}
#endif
}
