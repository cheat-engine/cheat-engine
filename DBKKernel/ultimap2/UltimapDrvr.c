#pragma warning( disable: 4103)

#include <ntifs.h>
#include <ntddk.h>
#include <windef.h>
#include "UltimapDrvr.h"


#include "IOPLDispatcher.h"
#include "..\ultimap2.h"
#include "apic.h"

#if (AMD64 && TOBESIGNED)
#include "..\sigcheck.h"
#endif


void UnloadDriver(PDRIVER_OBJECT DriverObject);

NTSTATUS DispatchCreate(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp);
NTSTATUS DispatchClose(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp);
NTSTATUS DispatchPnP(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp);

UNICODE_STRING  uszDeviceString;
PVOID BufDeviceString=NULL;




NTSTATUS DriverEntry(IN PDRIVER_OBJECT DriverObject, IN PUNICODE_STRING RegistryPath)
{
	NTSTATUS        ntStatus;
	PVOID           BufDriverString = NULL;
	UNICODE_STRING  uszDriverString;
	PDEVICE_OBJECT  pDeviceObject;
	HANDLE reg = 0;
	OBJECT_ATTRIBUTES oa;

	UNICODE_STRING temp;
	//char wbuf[100];
	//WORD this_cs, this_ss, this_ds, this_es, this_fs, this_gs;
	//ULONG cr4reg;

	__try
	{

		DbgPrint("Ultimap2 driver loading...\n");

	#ifdef TOBESIGNED
		DbgPrint("Signed version");
	#endif
		{
			int r[4];
			//DWORD a;

			__cpuid(r, 0);
			DbgPrint("cpuid.0: r[1]=%x", r[1]);
			if (r[1] == 0x756e6547) //GenuineIntel
			{
				__cpuidex(r, 7,0);
				DbgPrint("cpuid.7.0: r[0]=%x", r[0]);
				DbgPrint("cpuid.7.0: r[1]=%x", r[1]);
				DbgPrint("cpuid.7.0: r[2]=%x", r[2]);
				DbgPrint("cpuid.7.0: r[3]=%x", r[3]);
				if ((r[1] & (1 << 25))==0)
				{
					DbgPrint("Failure: CPU does not support the intel process trace feature\n");
					return STATUS_DRIVER_UNABLE_TO_LOAD;
				}
			}
			else
			{
				DbgPrint("Failure: Not an intel cpu");
				return STATUS_DRIVER_UNABLE_TO_LOAD;

			}
		}


		if (RegistryPath)
		{
			DbgPrint("Registry path = %S\n", RegistryPath->Buffer);

			InitializeObjectAttributes(&oa, RegistryPath, OBJ_KERNEL_HANDLE, NULL, NULL);
			ntStatus = ZwOpenKey(&reg, KEY_QUERY_VALUE, &oa);
			if (ntStatus == STATUS_SUCCESS)
			{
				UNICODE_STRING A, B, C, D;
				PKEY_VALUE_PARTIAL_INFORMATION bufA, bufB;
				ULONG ActualSize;

				DbgPrint("Opened the key\n");

				BufDriverString = ExAllocatePool(PagedPool, sizeof(KEY_VALUE_PARTIAL_INFORMATION) + 100);
				BufDeviceString = ExAllocatePool(PagedPool, sizeof(KEY_VALUE_PARTIAL_INFORMATION) + 100);

				bufA = BufDriverString;
				bufB = BufDeviceString;

				RtlInitUnicodeString(&A, L"A");
				RtlInitUnicodeString(&B, L"B");
				RtlInitUnicodeString(&C, L"C");
				RtlInitUnicodeString(&D, L"D");

				if (ntStatus == STATUS_SUCCESS)
					ntStatus = ZwQueryValueKey(reg, &A, KeyValuePartialInformation, bufA, sizeof(KEY_VALUE_PARTIAL_INFORMATION) + 100, &ActualSize);
				if (ntStatus == STATUS_SUCCESS)
					ntStatus = ZwQueryValueKey(reg, &B, KeyValuePartialInformation, bufB, sizeof(KEY_VALUE_PARTIAL_INFORMATION) + 100, &ActualSize);

				ZwClose(reg);

				if (ntStatus == STATUS_SUCCESS)
				{
					DbgPrint("Read ok\n");
					RtlInitUnicodeString(&uszDriverString, (PCWSTR)bufA->Data);
					RtlInitUnicodeString(&uszDeviceString, (PCWSTR)bufB->Data);

					DbgPrint("DriverString=%S\n", uszDriverString.Buffer);
					DbgPrint("DeviceString=%S\n", uszDeviceString.Buffer);
				}
				else
				{
					ExFreePool(bufA);
					ExFreePool(bufB);

					DbgPrint("Incorrect environment to load this driver. Exiting\n");

					return STATUS_DRIVER_UNABLE_TO_LOAD;;
				}

			}
			else
			{
				DbgPrint("Incorrect environment to load this driver. Exiting\n");
				return STATUS_DRIVER_UNABLE_TO_LOAD;;
			}
		}
		else
		{
			DbgPrint("Incorrect environment to load this driver. Exiting\n");
			return STATUS_DRIVER_UNABLE_TO_LOAD;
		}

		ntStatus = STATUS_SUCCESS;


		// Create and initialize device object
		ntStatus = IoCreateDevice(DriverObject,
				0,
				&uszDriverString,
				FILE_DEVICE_UNKNOWN,
				0,
				FALSE,
				&pDeviceObject);

		if (ntStatus != STATUS_SUCCESS)
		{
			DbgPrint("IoCreateDevice failed\n");
			ExFreePool(BufDriverString);
			ExFreePool(BufDeviceString);

			if (reg)
				ZwClose(reg);

			return ntStatus;
		}

		// Point uszDeviceString at the device name

		// Create symbolic link to the user-visible name
		ntStatus = IoCreateSymbolicLink(&uszDeviceString, &uszDriverString);

		if (ntStatus != STATUS_SUCCESS)
		{
			DbgPrint("IoCreateSymbolicLink failed: %x\n", ntStatus);
			// Delete device object if not successful
			IoDeleteDevice(pDeviceObject);

			ExFreePool(BufDriverString);
			ExFreePool(BufDeviceString);

			if (reg)
				ZwClose(reg);

			return ntStatus;
		}

		//when loaded by dbvm driver object is 'valid' so store the function addresses

		DbgPrint("DriverObject=%p\n", DriverObject);

		// Load structure to point to IRP handlers...
		DriverObject->DriverUnload = UnloadDriver;
		DriverObject->MajorFunction[IRP_MJ_CREATE] = DispatchCreate;
		DriverObject->MajorFunction[IRP_MJ_CLOSE] = DispatchClose;
	 	DriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL] = DispatchIoctl;


		// Return success (don't do the devicestring, I need it for unload)
		DbgPrint("Cleaning up initialization buffers\n");
		if (BufDriverString)
		{
			ExFreePool(BufDriverString);
			BufDriverString = NULL;
		}

		RtlInitUnicodeString(&temp, L"PsSuspendProcess");
		PsSuspendProcess = MmGetSystemRoutineAddress(&temp);

		RtlInitUnicodeString(&temp, L"PsResumeProcess");
		PsResumeProcess = MmGetSystemRoutineAddress(&temp);

		setup_APIC_BASE();


		return STATUS_SUCCESS;
	}
	__except (1)
	{
		DbgPrint("Exception while loading the ultimap2 driver\n");
		return STATUS_DRIVER_BLOCKED;
	}
	
}

NTSTATUS DispatchCreate(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp)
{
	// Check for SeDebugPrivilege. (So only processes with admin rights can use it)

	LUID sedebugprivUID;
	sedebugprivUID.LowPart=SE_DEBUG_PRIVILEGE;
	sedebugprivUID.HighPart=0;

	Irp->IoStatus.Status = STATUS_UNSUCCESSFUL;



	if (SeSinglePrivilegeCheck(sedebugprivUID, UserMode))
	{
		Irp->IoStatus.Status = STATUS_SUCCESS;
#ifdef AMD64
#ifdef TOBESIGNED
		{
			NTSTATUS s=SecurityCheck();	
			Irp->IoStatus.Status = s; 	
		}
	//	DbgPrint("Returning %x (and %x)\n", Irp->IoStatus.Status, s);
#endif
#endif


	}
	else
	{
		DbgPrint("A process without SeDebugPrivilege tried to open the dbk driver\n");
		Irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	}

    Irp->IoStatus.Information=0;

    IoCompleteRequest(Irp, IO_NO_INCREMENT);

	if (Irp->IoStatus.Status!=STATUS_SUCCESS)
		DbgPrint("Access denied. Unlicensed application\n");

	if (APIC_BASE == NULL)
	{
		DbgPrint("APIC_BASE is NULL. The driver failed to load properly\n");
		Irp->IoStatus.Status = STATUS_UNSUCCESSFUL;
	}

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

NTSTATUS DispatchPnP(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp)
{
	PIO_STACK_LOCATION stack;
	DbgPrint("DBK:DispatchPnP\n");
	stack = IoGetCurrentIrpStackLocation(Irp);

	DbgPrint("MinorFunction=%d\n", stack->MinorFunction);

	Irp->IoStatus.Status = STATUS_SUCCESS;
	Irp->IoStatus.Information = 0;

	IoCompleteRequest(Irp, IO_NO_INCREMENT);
	return Irp->IoStatus.Status;
}


void UnloadDriver(PDRIVER_OBJECT DriverObject)
{
	DisableUltimap2();
	UnregisterUltimapPMI();
	clean_APIC_BASE();

	DbgPrint("Ultimap2 unloading\n");

    IoDeleteDevice(DriverObject->DeviceObject);

	DbgPrint("DeviceString=%S\n",uszDeviceString.Buffer);
	DbgPrint("IoDeleteSymbolicLink: %x\n", IoDeleteSymbolicLink(&uszDeviceString));
	ExFreePool(BufDeviceString);
}