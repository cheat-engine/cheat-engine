#pragma warning( disable: 4103)

#include "DBKFunc.h"
#include <ntddk.h>
#include <windef.h>
#include "DBKDrvr.h"

#include "deepkernel.h"
#include "processlist.h"
#include "memscan.h"
#include "threads.h"
#include "vmxhelper.h"
#include "debugger.h"


#include "IOPLDispatcher.h"
#include "interruptHook.h"
#include "ultimap.h"
#include "ultimap2.h"

#if (AMD64 && TOBESIGNED)
#include "sigcheck.h"
#endif


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

VOID TestPassive(UINT_PTR param)
{
	DbgPrint("passive cpu call for cpu %d\n", KeGetCurrentProcessorNumber());
}


VOID TestDPC(IN struct _KDPC *Dpc, IN PVOID  DeferredContext, IN PVOID  SystemArgument1, IN PVOID  SystemArgument2)
{
	EFLAGS e=getEflags();
	
    DbgPrint("Defered cpu call for cpu %d (Dpc=%p  IF=%d IRQL=%d)\n", KeGetCurrentProcessorNumber(), Dpc, e.IF, KeGetCurrentIrql());
}


VOID TestThread(__in PVOID StartContext)
{
	PEPROCESS x = (PEPROCESS)StartContext;
	DbgPrint("Hello from testthread");

	//PsSuspendProcess((PEPROCESS)StartContext);

	

	DbgPrint("x=%p\n", x);


	
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
	HANDLE reg=0;
	OBJECT_ATTRIBUTES oa;

	UNICODE_STRING temp; 
	char wbuf[100]; 
	WORD this_cs, this_ss, this_ds, this_es, this_fs, this_gs;
	ULONG cr4reg;

	
	
	criticalSection csTest;

	HANDLE Ultimap2Handle;

	
	KernelCodeStepping=0;
	KernelWritesIgnoreWP = 0;

	

	this_cs=getCS();
	this_ss=getSS();
	this_ds=getDS();
	this_es=getES();
	this_fs=getFS();
	this_gs=getGS();	



	//InitializeObjectAttributes(&ao, NULL, OBJ_KERNEL_HANDLE, NULL, NULL);
	//PsCreateSystemThread(&Ultimap2Handle, 0, NULL, 0, NULL, TestThread, PsGetCurrentProcess());

	DbgPrint("DBK loading...");
#ifdef TOBESIGNED
	DbgPrint("Signed version");
#endif

	
	//lame antiviruses and more lamer users that keep crying rootkit virus....
	temp.Buffer=(PWCH)wbuf;
	temp.Length=0;
	temp.MaximumLength=100;
	
	RtlAppendUnicodeToString(&temp, L"Ke"); //KeServiceDescriptorTable 
	RtlAppendUnicodeToString(&temp, L"Service");
	RtlAppendUnicodeToString(&temp, L"Descriptor");
	RtlAppendUnicodeToString(&temp, L"Table");
	
	KeServiceDescriptorTable=MmGetSystemRoutineAddress(&temp);         

	DbgPrint("Loading driver\n");
	if (RegistryPath)
	{	
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

				DbgPrint("DriverString=%S\n",uszDriverString.Buffer);
				DbgPrint("DeviceString=%S\n",uszDeviceString.Buffer);
				DbgPrint("ProcessEventString=%S\n",uszProcessEventString.Buffer);
				DbgPrint("ThreadEventString=%S\n",uszThreadEventString.Buffer);
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
	}
	else
	  loadedbydbvm=TRUE;

	ntStatus = STATUS_SUCCESS;


	


	if (!loadedbydbvm)
	{

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
			DbgPrint("IoCreateDevice failed\n");
			ExFreePool(BufDriverString);
			ExFreePool(BufDeviceString);
			ExFreePool(BufProcessEventString);
			ExFreePool(BufThreadEventString);

			
			if (reg)
			  ZwClose(reg);

			return ntStatus;
		}

		// Point uszDeviceString at the device name
		
		// Create symbolic link to the user-visible name
		ntStatus = IoCreateSymbolicLink(&uszDeviceString, &uszDriverString);

		if(ntStatus != STATUS_SUCCESS)
		{
			DbgPrint("IoCreateSymbolicLink failed: %x\n",ntStatus);
			// Delete device object if not successful
			IoDeleteDevice(pDeviceObject);

			ExFreePool(BufDriverString);
			ExFreePool(BufDeviceString);
			ExFreePool(BufProcessEventString);
			ExFreePool(BufThreadEventString);
			

			if (reg)
			  ZwClose(reg);

			return ntStatus;
		}

#endif
	}

	//when loaded by dbvm driver object is 'valid' so store the function addresses


	DbgPrint("DriverObject=%p\n", DriverObject);

    // Load structure to point to IRP handlers...
    DriverObject->DriverUnload                         = UnloadDriver;
    DriverObject->MajorFunction[IRP_MJ_CREATE]         = DispatchCreate;
    DriverObject->MajorFunction[IRP_MJ_CLOSE]          = DispatchClose;	

	if (loadedbydbvm)
		DriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL] = (PDRIVER_DISPATCH)DispatchIoctlDBVM;		
	else
		DriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL] = DispatchIoctl;



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
		MAX_PTE_POS=0xC07FFFF8;

		
	}
	else
	{
		PTESize=4;
		PAGE_SIZE_LARGE=0x400000;
		MAX_PDE_POS=0xC0301000;
		MAX_PTE_POS=0xC03FFFFC;
	}
#else
	PTESize=8; //pae
	PAGE_SIZE_LARGE=0x200000;
	MAX_PTE_POS=0xFFFFF6FFFFFFFFF8ULL;
	MAX_PDE_POS=0xFFFFF6FB7FFFFFF8ULL;
#endif

	

#ifdef CETC
	DbgPrint("Going to initialice CETC\n");
	InitializeCETC();
#endif


    //hideme(DriverObject); //ok, for those that see this, enabling this WILL fuck up try except routines, even in usermode you'll get a blue sreen

	DbgPrint("Initializing debugger\n");
	debugger_initialize();


	// Return success (don't do the devicestring, I need it for unload)
	DbgPrint("Cleaning up initialization buffers\n");
	if (BufDriverString)
	{
		ExFreePool(BufDriverString);
		BufDriverString=NULL;
	}

	if (BufProcessEventString)
	{
		ExFreePool(BufProcessEventString);
		BufProcessEventString=NULL;
	}

	if (BufThreadEventString)
	{
		ExFreePool(BufThreadEventString);
		BufThreadEventString=NULL;
	}

	if (reg)
	{
		ZwClose(reg); 
		reg=0;
	}

	

	//fetch cpu info
	{
		DWORD r[4];
		DWORD a;

		__cpuid(r,0);
		DbgPrint("cpuid.0: r[1]=%x", r[1]);
		if (r[1]==0x756e6547) //GenuineIntel
		{

			__cpuid(r,1);

			a=r[0];
			
			cpu_stepping=a & 0xf;
			cpu_model=(a >> 4) & 0xf;
			cpu_familyID=(a >> 8) & 0xf;
			cpu_type=(a >> 12) & 0x3;
			cpu_ext_modelID=(a >> 16) & 0xf;
			cpu_ext_familyID=(a >> 20) & 0xff;

			cpu_model=cpu_model + (cpu_ext_modelID << 4);
			cpu_familyID=cpu_familyID + (cpu_ext_familyID << 4);

			if ((r[2]<<9) & 1)
			{
				DbgPrint("Intel cpu. IA32_FEATURE_CONTROL MSR=%x", readMSR(0x3a));		
			}
			else
			{
				DbgPrint("Intel cpu without IA32_FEATURE_CONTROL MSR");		
			}

			vmx_init_dovmcall(1);
			setup_APIC_BASE(); //for ultimap

		}
		else
		{
			DbgPrint("Not an intel cpu");
			if (r[1]==0x68747541)
			{
				DbgPrint("This is an AMD\n");
				vmx_init_dovmcall(0);
			}

		}



	}

	{
		APIC y;
		
		DebugStackState x;
		DbgPrint("offset of LBR_Count=%d\n", (UINT_PTR)&x.LBR_Count-(UINT_PTR)&x);

		DbgPrint("Testing forEachCpu(...)\n");
		forEachCpu(TestDPC, NULL, NULL, NULL);
		forEachCpuAsync(TestDPC, NULL, NULL, NULL);

		forEachCpuPassive(TestPassive, 0);

		DbgPrint("LVT_Performance_Monitor=%x\n", (UINT_PTR)&y.LVT_Performance_Monitor-(UINT_PTR)&y);
	}


	
    return STATUS_SUCCESS;
}


NTSTATUS DispatchCreate(IN PDEVICE_OBJECT DeviceObject,
                       IN PIRP Irp)
{
	// Check for SeDebugPrivilege. (So only processes with admin rights can use it)

	LUID sedebugprivUID;
	sedebugprivUID.LowPart=SE_DEBUG_PRIVILEGE;
	sedebugprivUID.HighPart=0;

	Irp->IoStatus.Status = STATUS_UNSUCCESSFUL;



	if (SeSinglePrivilegeCheck(sedebugprivUID, UserMode))
	{
#ifdef TOBESIGNED
		NTSTATUS s=SecurityCheck();	
		Irp->IoStatus.Status = s; 		
	//	DbgPrint("Returning %x (and %x)\n", Irp->IoStatus.Status, s);
#else
		Irp->IoStatus.Status = STATUS_SUCCESS;
#endif

	}
	else
	{
		DbgPrint("A process without SeDebugPrivilege tried to open the dbk driver\n");
		Irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	}

    Irp->IoStatus.Information=0;

    IoCompleteRequest(Irp, IO_NO_INCREMENT);
    return Irp->IoStatus.Status;
}


NTSTATUS DispatchClose(IN PDEVICE_OBJECT DeviceObject,
                       IN PIRP Irp)
{
    Irp->IoStatus.Status = STATUS_SUCCESS;
    Irp->IoStatus.Information=0;

    IoCompleteRequest(Irp, IO_NO_INCREMENT);
    return Irp->IoStatus.Status;
}


typedef NTSTATUS (*PSRCTNR)(__in PCREATE_THREAD_NOTIFY_ROUTINE NotifyRoutine);
PSRCTNR PsRemoveCreateThreadNotifyRoutine2;

typedef NTSTATUS (*PSRLINR)(__in PLOAD_IMAGE_NOTIFY_ROUTINE NotifyRoutine);
PSRLINR PsRemoveLoadImageNotifyRoutine2;



void UnloadDriver(PDRIVER_OBJECT DriverObject)
{
	if (!debugger_stopDebugging())
	{
		DbgPrint("Can not unload the driver because of debugger\n");
		return; //
	}

	ultimap_disable();
	DisableUltimap2();

	clean_APIC_BASE();
	

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
		

	if ((CreateProcessNotifyRoutineEnabled) || (ImageNotifyRoutineLoaded)) 
	{
		PVOID x;
		UNICODE_STRING temp;

		RtlInitUnicodeString(&temp, L"PsRemoveCreateThreadNotifyRoutine");
		PsRemoveCreateThreadNotifyRoutine2=MmGetSystemRoutineAddress(&temp);

		RtlInitUnicodeString(&temp, L"PsRemoveCreateThreadNotifyRoutine");
		PsRemoveLoadImageNotifyRoutine2=MmGetSystemRoutineAddress(&temp);
		
		RtlInitUnicodeString(&temp, L"ObOpenObjectByName");
		x=MmGetSystemRoutineAddress(&temp);
		
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


	DbgPrint("Driver unloading\n");

    IoDeleteDevice(DriverObject->DeviceObject);

#ifdef CETC
#ifndef CETC_RELEASE
	UnloadCETC(); //not possible in the final build
#endif
#endif

#ifndef CETC_RELEASE
	DbgPrint("DeviceString=%S\n",uszDeviceString.Buffer);
	DbgPrint("IoDeleteSymbolicLink: %x\n", IoDeleteSymbolicLink(&uszDeviceString));
	ExFreePool(BufDeviceString);
#endif

}