#include "newkernel.h"   
#include "ntifs.h"

NTSTATUS makeKernelCopy(ULONG KernelBase, ULONG KernelSize)
{
	NTSTATUS ntStatus=STATUS_UNSUCCESSFUL;

	__try
	{
		PMDL mdl=NULL;
		char *buffer=NULL;

		mdl = IoAllocateMdl((PVOID)KernelBase, KernelSize, FALSE, TRUE, NULL);
		if (!mdl)
		{
			DbgPrint("Not enough memory dude!!!!\n");
			ntStatus = STATUS_INSUFFICIENT_RESOURCES;
			return ntStatus;
		}

		try
		{
			MmProbeAndLockPages(mdl, KernelMode, IoReadAccess);
			buffer = MmGetSystemAddressForMdlSafe(mdl, NormalPagePriority );
			if (buffer)
			{
				DbgPrint("mapped the kernel at %p\n",buffer);
				//allocate memory for the copy
				KernelCopy=ExAllocatePool(NonPagedPool, KernelSize);
				RtlCopyMemory(KernelCopy,buffer,KernelSize);
				DbgPrint("KernelCopy allocated at %p",KernelCopy);

				//change the functions to make use of the copy

			}

		}
		__except(1)
		{
			DbgPrint("Exception during copy\n");
		}
				
		
		

		
		
	}
	__except(0)
	{
		DbgPrint("Failed to map the kernel\n");
	}

	return ntStatus;
};