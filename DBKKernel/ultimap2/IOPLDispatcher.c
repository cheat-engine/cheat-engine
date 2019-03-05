#pragma warning( disable: 4103)


//Ultimap iopl dispatcher
#include <ntifs.h>
#include <ntddk.h>
#include "IOPLDispatcher.h"
#include "UltimapDrvr.h"
#include "..\ultimap2.h"

NTSTATUS DispatchIoctl(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp)
{
	NTSTATUS ntStatus=STATUS_UNSUCCESSFUL;

    PIO_STACK_LOCATION     irpStack=NULL;
	LUID sedebugprivUID;
	ULONG IoControlCode;

	irpStack = IoGetCurrentIrpStackLocation(Irp);
	IoControlCode = irpStack->Parameters.DeviceIoControl.IoControlCode;
		
    switch(IoControlCode)
    {
		
		case IOCTL_CE_ULTIMAP2:
			{
				struct input
				{
					UINT32 PID;	
					UINT32 Size;
					UINT32 RangeCount;
					UINT32 NoPMI;
					UINT32 UserMode;
					UINT32 KernelMode;
					URANGE Ranges[8];
					WCHAR OutputPath[200];
				} *inp = Irp->AssociatedIrp.SystemBuffer;
				int i;

				DbgPrint("IOCTL_CE_ULTIMAP2 V2");
				for (i = 0; i < (int)(inp->RangeCount); i++)
					DbgPrint("%d=%p -> %p", i, (PVOID)inp->Ranges[i].StartAddress, (PVOID)inp->Ranges[i].EndAddress);

				SetupUltimap2(inp->PID, inp->Size, inp->OutputPath, inp->RangeCount, inp->Ranges, inp->NoPMI, inp->UserMode, inp->KernelMode);

				ntStatus = STATUS_SUCCESS;
				break;
			}

		case IOCTL_CE_ULTIMAP2_WAITFORDATA:
		{			
			ULONG timeout = *(ULONG *)Irp->AssociatedIrp.SystemBuffer;
			PULTIMAP2DATAEVENT output = Irp->AssociatedIrp.SystemBuffer;
			DbgPrint("IOCTL_CE_ULTIMAP2_WAITFORDATA\n");
			output->Address = 0;

			ntStatus = ultimap2_waitForData(timeout, output);

			break;
		}

		case IOCTL_CE_ULTIMAP2_LOCKFILE:
		{
			
			int cpunr = *(int *)Irp->AssociatedIrp.SystemBuffer;
			DbgPrint("IOCTL_CE_ULTIMAP2_LOCKFILE\n");
			ultimap2_LockFile(cpunr);

			ntStatus = STATUS_SUCCESS;
			break;
		}

		case IOCTL_CE_ULTIMAP2_RELEASEFILE:
		{			
			int cpunr = *(int *)Irp->AssociatedIrp.SystemBuffer;
			DbgPrint("IOCTL_CE_ULTIMAP2_RELEASEFILE\n");
			ultimap2_ReleaseFile(cpunr);

			ntStatus = STATUS_SUCCESS;				
			break;
		}

		case IOCTL_CE_ULTIMAP2_GETTRACESIZE:
		{
			DbgPrint("IOCTL_CE_ULTIMAP2_GETTRACESIZE\n");
			*(UINT64*)Irp->AssociatedIrp.SystemBuffer = ultimap2_GetTraceFileSize();
			ntStatus = STATUS_SUCCESS;
			break;
		}

		case IOCTL_CE_ULTIMAP2_RESETTRACESIZE:
		{
			DbgPrint("IOCTL_CE_ULTIMAP2_RESETTRACESIZE\n");
			ultimap2_ResetTraceFileSize();
			ntStatus = STATUS_SUCCESS;
			break;
		}


		case IOCTL_CE_ULTIMAP2_CONTINUE:
		{			
			int cpunr=*(int*)Irp->AssociatedIrp.SystemBuffer;
			DbgPrint("IOCTL_CE_ULTIMAP2_CONTINUE\n");
			ntStatus = ultimap2_continue(cpunr);

			break;
		}

		case IOCTL_CE_ULTIMAP2_FLUSH:
		{			
			DbgPrint("IOCTL_CE_ULTIMAP2_FLUSH\n");
			ntStatus = ultimap2_flushBuffers();
			break;
		}

		case IOCTL_CE_ULTIMAP2_PAUSE:
		{
			DbgPrint("IOCTL_CE_ULTIMAP2_PAUSE\n");
			ntStatus = ultimap2_pause();
			break;
		}

		case IOCTL_CE_ULTIMAP2_RESUME:
		{
			DbgPrint("IOCTL_CE_ULTIMAP2_RESUME\n");
			ntStatus = ultimap2_resume();
			break;
		}

		case IOCTL_CE_DISABLEULTIMAP2:
		{
			DbgPrint("IOCTL_CE_DISABLEULTIMAP2\n");
			DisableUltimap2();
			break;
		}

        default:
			DbgPrint("Ultimap driver: Unhandled IO request: %x\n", IoControlCode);			
            break;
    }

	
    Irp->IoStatus.Status = ntStatus;
    
    // Set # of bytes to copy back to user-mode...
	if (irpStack) //only NULL when loaded by dbvm
	{
		if (ntStatus == STATUS_SUCCESS)
			Irp->IoStatus.Information = irpStack->Parameters.DeviceIoControl.OutputBufferLength;
		else
			Irp->IoStatus.Information = 0;

		IoCompleteRequest(Irp, IO_NO_INCREMENT);
	}

    
    return ntStatus;
}
