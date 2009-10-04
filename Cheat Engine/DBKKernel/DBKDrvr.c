#pragma warning( disable: 4103)

#include "DBKFunc.h"
#include <ntddk.h>
#include <windef.h>
#include "DBKDrvr.h"

#include "rootkit.h"
#include "processlist.h"
#include "memscan.h"
#include "threads.h"
#include "vmxhelper.h"
#include "debugger.h"



#include "newkernel.h"

#include "stealthedit.h"

#include "IOPLDispatcher.h"



#ifdef CETC
	#include "cetc.h"
#endif





void UnloadDriver(PDRIVER_OBJECT DriverObject);

NTSTATUS DispatchCreate(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp);
NTSTATUS DispatchClose(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp);

#ifndef AMD64
//no api hooks for x64

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


typedef NTSTATUS (*ZWSUSPENDPROCESS)
(
    IN ULONG ProcessHandle  // Handle to the process
);
ZWSUSPENDPROCESS ZwSuspendProcess;

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

//PVOID GetApiEntry(ULONG FunctionNumber);
#endif



UNICODE_STRING  uszDeviceString;
PVOID BufDeviceString=NULL;



void hideme(PDRIVER_OBJECT DriverObject)
{
#ifndef AMD64
	
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

#endif
}


int testfunction(int p1,int p2)
{
	DbgPrint("Hello\nParam1=%d\nParam2=%d\n",p1,p2);
	return 0x666;
}


void* functionlist[1];
char  paramsizes[1];
int registered=0;



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
	char wbuf[100]; 
	WORD this_cs, this_ss, this_ds, this_es, this_fs, this_gs;

	
	criticalSection csTest;
	
	

	//DbgPrint("%S",oa.ObjectName.Buffer); 
	

	

	

	this_cs=getCS();
	this_ss=getSS();
	this_ds=getDS();
	this_es=getES();
	this_fs=getFS();
	this_gs=getGS();	

	DbgPrint("cs=%x ss=%x ds=%x es=%x fs=%x gs=%x\n",this_cs, this_ss, this_ds, this_es, this_fs, this_gs);

	DbgPrint("fsbase=%llx gsbase=%llx\n", readMSR(0xc0000100), readMSR(0xc0000101));
	DbgPrint("rbp=%llx\n", getRBP());
	
	

	DbgPrint("Test critical section routines\n");
	RtlZeroMemory(&csTest,sizeof(criticalSection));
	DbgPrint("csTest.locked=%d\n",csTest.locked);
	csEnter(&csTest);
	DbgPrint("After enter\n");
	DbgPrint("csTest.locked=%d\n",csTest.locked);
	csLeave(&csTest);
	
	DbgPrint("After leave\n");
	DbgPrint("csTest.locked=%d\n",csTest.locked);
	
	

	

	
	//lame antiviruses and more lamer users that keep crying rootkit virus....
	temp.Buffer=(PWCH)wbuf;
	temp.Length=0;
	temp.MaximumLength=100;
	
	RtlAppendUnicodeToString(&temp, L"Ke"); //KeServiceDescriptorTable 
	RtlAppendUnicodeToString(&temp, L"Service");
	RtlAppendUnicodeToString(&temp, L"Descriptor");
	RtlAppendUnicodeToString(&temp, L"Table");

	DbgPrint("temp.buffer=%S\n",temp.Buffer);

	

	KeServiceDescriptorTable=MmGetSystemRoutineAddress(&temp);         
	
	



	DbgPrint("Loading driver\n");
	DbgPrint("Registry path = %S\n", RegistryPath->Buffer);

	InitializeObjectAttributes(&oa,RegistryPath,OBJ_KERNEL_HANDLE ,NULL,NULL);
	ntStatus=ZwOpenKey(&reg,KEY_QUERY_VALUE,&oa);
	if (ntStatus == STATUS_SUCCESS)
	{
		UNICODE_STRING A,B,C,D;
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
    DriverObject->DriverUnload                         = UnloadDriver;
    DriverObject->MajorFunction[IRP_MJ_CREATE]         = DispatchCreate;
    DriverObject->MajorFunction[IRP_MJ_CLOSE]          = DispatchClose;
	
    DriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL] = DispatchIoctl;


		
#ifndef AMD64
	ProtectOn=FALSE;
	ImageNotifyRoutineLoaded=FALSE;

	LastForegroundWindow=0;
	ProtectedProcessID=0;

	ModuleList=NULL;
	ModuleListSize=0;
	KernelCopy=0;
#endif
	newthreaddatafiller=IoAllocateWorkItem(pDeviceObject);

	//Processlist init
#ifndef CETC

	ProcessEventCount=0;
	KeInitializeSpinLock(&ProcesslistSL);
#endif

	CreateProcessNotifyRoutineEnabled=FALSE;

	//threadlist init
	ThreadEventCount=0;
	
	BufferSize=0;
	processlist=NULL;

#ifndef AMD64
    //determine if PAE is used
	cr4reg=(ULONG)getCR4();

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
#else
	PTESize=8; //pae
	PAGE_SIZE_LARGE=0x200000;
	//MAX_PDE_POS=to be filled in
#endif

#ifdef CETC
	DbgPrint("Going to initialice CETC\n");
	InitializeCETC();
#endif


    //hideme(DriverObject); //ok, for those that see this, enabling this WILL fuck up try except routines, even in usermode you'll get a blue sreen

	debugger_initialize();
	
	stealthedit_initialize();
	

	

	// Return success (don't do the devicestring, I need it for unload)
	ExFreePool(BufDriverString);
	ExFreePool(BufProcessEventString);
	ExFreePool(BufThreadEventString);
		
	ZwClose(reg); 

	
    return STATUS_SUCCESS;
}


NTSTATUS DispatchCreate(IN PDEVICE_OBJECT DeviceObject,
                       IN PIRP Irp)
{
    Irp->IoStatus.Status = STATUS_SUCCESS;
    Irp->IoStatus.Information=0;

    IoCompleteRequest(Irp, IO_NO_INCREMENT);
    return(STATUS_SUCCESS);
}


NTSTATUS DispatchClose(IN PDEVICE_OBJECT DeviceObject,
                       IN PIRP Irp)
{
    Irp->IoStatus.Status = STATUS_SUCCESS;
    Irp->IoStatus.Information=0;

    IoCompleteRequest(Irp, IO_NO_INCREMENT);
    return(STATUS_SUCCESS);
}




typedef NTSTATUS (*PSRCTNR)(__in PCREATE_THREAD_NOTIFY_ROUTINE NotifyRoutine);
PSRCTNR PsRemoveCreateThreadNotifyRoutine2;

typedef NTSTATUS (*PSRLINR)(__in PLOAD_IMAGE_NOTIFY_ROUTINE NotifyRoutine);
PSRLINR PsRemoveLoadImageNotifyRoutine2;



void UnloadDriver(PDRIVER_OBJECT DriverObject)
{
	/*x
	if (ProtectOn) //can't unload when protection is enabled
		return;

	if (!debugger_stopDebugging())
	{
		DbgPrint("Can not unload the driver because of debugger\n");
		return; //
	}

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


*/
	DbgPrint("Driver unloading\n");

    IoDeleteDevice(DriverObject->DeviceObject);

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
