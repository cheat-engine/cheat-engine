/*
 Parts of this code are completly copy/pasted from Toby Opferman driver development articles
 Other parts have been modified slightly
 */



#include "ntifs.h"
#include <windef.h>
#include <tdi.h>
#include <tdikrnl.h>
#include "tdiwrapper.h"

KSPIN_LOCK SendLock;

NTSTATUS TdiFuncs_OpenTransportAddress(PHANDLE pTdiHandle, PFILE_OBJECT *pFileObject)
{
		NTSTATUS ntStatus = STATUS_INSUFFICIENT_RESOURCES;	
		UNICODE_STRING usTdiDriverNameString;
		OBJECT_ATTRIBUTES oaTdiDriverNameAttributes;
		char DataBlob[sizeof(FILE_FULL_EA_INFORMATION) + TDI_TRANSPORT_ADDRESS_LENGTH + 300] = {0};
		PFILE_FULL_EA_INFORMATION pExtendedAttributesInformation = (PFILE_FULL_EA_INFORMATION)&DataBlob;
		PTRANSPORT_ADDRESS pTransportAddress = NULL;
		PTDI_ADDRESS_IP pTdiAddressIp = NULL;
		UINT dwEASize = 0;
		IO_STATUS_BLOCK IoStatusBlock;


		RtlInitUnicodeString(&usTdiDriverNameString, L"\\Device\\TCP");
		InitializeObjectAttributes(&oaTdiDriverNameAttributes,&usTdiDriverNameString,
			OBJ_CASE_INSENSITIVE | OBJ_KERNEL_HANDLE, 
			NULL,NULL);

		RtlCopyMemory(&pExtendedAttributesInformation->EaName,TdiTransportAddress,TDI_TRANSPORT_ADDRESS_LENGTH);
		pExtendedAttributesInformation->EaNameLength = TDI_TRANSPORT_ADDRESS_LENGTH;
		pExtendedAttributesInformation->EaValueLength= TDI_TRANSPORT_ADDRESS_LENGTH+sizeof(TRANSPORT_ADDRESS)+sizeof(TDI_ADDRESS_IP);

		pTransportAddress =(PTRANSPORT_ADDRESS)(&pExtendedAttributesInformation->EaName+TDI_TRANSPORT_ADDRESS_LENGTH + 1);
		pTransportAddress->TAAddressCount = 1;

		pTransportAddress->Address[0].AddressType    = TDI_ADDRESS_TYPE_IP;
		pTransportAddress->Address[0].AddressLength  = sizeof(TDI_ADDRESS_IP);
		pTdiAddressIp = (TDI_ADDRESS_IP *)&pTransportAddress->Address[0].Address;

		RtlZeroMemory(pTdiAddressIp, sizeof(TDI_ADDRESS_IP));
		pTdiAddressIp->sin_port=0x4444;

		dwEASize = sizeof(DataBlob);

		ntStatus = ZwCreateFile(pTdiHandle, FILE_READ_EA | FILE_WRITE_EA | FILE_READ_DATA , 
									&oaTdiDriverNameAttributes, 
									&IoStatusBlock, NULL, FILE_ATTRIBUTE_NORMAL, 0, FILE_OPEN_IF, 0, 
									pExtendedAttributesInformation, dwEASize);

		if (NT_SUCCESS(ntStatus))
		{

			ntStatus = ObReferenceObjectByHandle(*pTdiHandle,
                        GENERIC_READ | GENERIC_WRITE, 
                        NULL, 
                        KernelMode, 
                        (PVOID *)pFileObject, NULL);  
			if (!NT_SUCCESS(ntStatus))
				ZwClose(*pTdiHandle);
		}

		return ntStatus;

}

NTSTATUS TdiFuncs_OpenConnection(PHANDLE pTdiHandle, PFILE_OBJECT *pFileObject)
{
    NTSTATUS ntStatus = STATUS_INSUFFICIENT_RESOURCES;
    UNICODE_STRING usTdiDriverNameString;
    OBJECT_ATTRIBUTES oaTdiDriverNameAttributes;
    IO_STATUS_BLOCK IoStatusBlock;
    char DataBlob[sizeof(FILE_FULL_EA_INFORMATION) + 
              TDI_CONNECTION_CONTEXT_LENGTH + 300] = {0};
    PFILE_FULL_EA_INFORMATION pExtendedAttributesInformation = 
                    (PFILE_FULL_EA_INFORMATION)&DataBlob;
    UINT dwEASize = 0;
        
	RtlInitUnicodeString(&usTdiDriverNameString, L"\\Device\\TCP");
    InitializeObjectAttributes(&oaTdiDriverNameAttributes, 
            &usTdiDriverNameString, 
            OBJ_CASE_INSENSITIVE | OBJ_KERNEL_HANDLE, 
            NULL, NULL);

	
    RtlCopyMemory(&pExtendedAttributesInformation->EaName, 
                        TdiConnectionContext, TDI_CONNECTION_CONTEXT_LENGTH);

    pExtendedAttributesInformation->EaNameLength = 
                                          TDI_CONNECTION_CONTEXT_LENGTH;
    pExtendedAttributesInformation->EaValueLength = 
                                       TDI_CONNECTION_CONTEXT_LENGTH; 
 
    dwEASize = sizeof(DataBlob);

	ntStatus = ZwCreateFile(pTdiHandle, 
        FILE_READ_DATA | FILE_READ_EA | FILE_WRITE_EA, &oaTdiDriverNameAttributes,
         &IoStatusBlock, NULL, 
         FILE_ATTRIBUTE_NORMAL, 0, FILE_OPEN_IF, 0, 
         pExtendedAttributesInformation, dwEASize);

	DbgPrint("status= %x\n",ntStatus);


     if(NT_SUCCESS(ntStatus))
     {
         ntStatus = ObReferenceObjectByHandle(*pTdiHandle, 
                          GENERIC_READ | GENERIC_WRITE, 
                          NULL, KernelMode, 
                         (PVOID *)pFileObject, NULL);      

		  
         if(!NT_SUCCESS(ntStatus))
         {
             DbgPrint("ObReferenceObjectByHandle failed\n");
             ZwClose(*pTdiHandle);
         }
     }
	 
     return ntStatus;
}

NTSTATUS TdiFuncs_AssociateTransportAndConnection(HANDLE hTransportAddress, PFILE_OBJECT pfoConnection)
{
    NTSTATUS ntStatus = STATUS_INSUFFICIENT_RESOURCES;
    PIRP pIrp;
    IO_STATUS_BLOCK IoStatusBlock = {0};
    PDEVICE_OBJECT pTdiDevice;
	KEVENT TdiCompleteEvent;

    KeInitializeEvent(&TdiCompleteEvent,  NotificationEvent, FALSE);

    pTdiDevice = IoGetRelatedDeviceObject(pfoConnection);
	pIrp = TdiBuildInternalDeviceControlIrp(TDI_ASSOCIATE_ADDRESS, pTdiDevice, pfoConnection, &TdiCompleteEvent, &IoStatusBlock);

    if(pIrp)
    {
        TdiBuildAssociateAddress(pIrp, pTdiDevice, pfoConnection, NULL, NULL, hTransportAddress);
        ntStatus = IoCallDriver(pTdiDevice, pIrp);
        
        if(ntStatus == STATUS_PENDING) //no completion routine given so use the event to wait
        {
            KeWaitForSingleObject(&TdiCompleteEvent, Executive, KernelMode, FALSE, NULL);
            ntStatus = IoStatusBlock.Status;
        }
    }
    return ntStatus;
}

NTSTATUS TdiFuncs_DisAssociateTransportAndConnection(PFILE_OBJECT pfoConnection)
{
    NTSTATUS ntStatus = STATUS_INSUFFICIENT_RESOURCES;
    PIRP pIrp;
    IO_STATUS_BLOCK IoStatusBlock = {0};
    PDEVICE_OBJECT pTdiDevice;
	KEVENT TdiCompleteEvent;

    KeInitializeEvent(&TdiCompleteEvent,  NotificationEvent, FALSE);

    pTdiDevice = IoGetRelatedDeviceObject(pfoConnection);
	pIrp = TdiBuildInternalDeviceControlIrp(TDI_DISASSOCIATE_ADDRESS, pTdiDevice, pfoConnection, &TdiCompleteEvent, &IoStatusBlock);

    if(pIrp)
    {
        TdiBuildDisassociateAddress(pIrp, pTdiDevice, pfoConnection, NULL, NULL);
        ntStatus = IoCallDriver(pTdiDevice, pIrp);
        
        if(ntStatus == STATUS_PENDING)
        {
            KeWaitForSingleObject(&TdiCompleteEvent, Executive, KernelMode, FALSE, NULL);
            ntStatus = IoStatusBlock.Status;
        }
    }

    return ntStatus;
}

NTSTATUS TdiFuncs_SetEventHandler(PFILE_OBJECT pfoTdiFileObject, LONG InEventType, PVOID InEventHandler, PVOID InEventContext)
{
    NTSTATUS ntStatus = STATUS_INSUFFICIENT_RESOURCES;
    PIRP pIrp;
    IO_STATUS_BLOCK IoStatusBlock = {0};
    PDEVICE_OBJECT pTdiDevice;
    KEVENT TdiCompletionContext;

	DbgPrint("SetEventHandler:\n");
    KeInitializeEvent(&TdiCompletionContext, NotificationEvent, FALSE);

    pTdiDevice = IoGetRelatedDeviceObject(pfoTdiFileObject);    
	DbgPrint("pTdiDevice=%p\n",pTdiDevice);
    pIrp = TdiBuildInternalDeviceControlIrp(TDI_SET_EVENT_HANDLER, pTdiDevice, pfoConnection, &TdiCompletionContext, &IoStatusBlock);

    if(pIrp)
    {        
        TdiBuildSetEventHandler(pIrp, pTdiDevice, pfoTdiFileObject, NULL, NULL, InEventType, InEventHandler, InEventContext);

		DbgPrint("Calling IoCallDriver\n");
        ntStatus = IoCallDriver(pTdiDevice, pIrp);

        if(ntStatus == STATUS_PENDING)
        {
            KeWaitForSingleObject(&TdiCompletionContext, Executive, KernelMode, FALSE, NULL);
            ntStatus = IoStatusBlock.Status;
        }

		DbgPrint("Set event handler finished\n");

    }
	else DbgPrint("TdiBuildInternalDeviceControlIrp failed\n");

	DbgPrint("Status=%x\n",ntStatus);

    return ntStatus;
}

NTSTATUS TdiFuncs_Disconnect(PFILE_OBJECT pfoConnection)
{
    NTSTATUS ntStatus = STATUS_INSUFFICIENT_RESOURCES;
    PIRP pIrp;
    IO_STATUS_BLOCK IoStatusBlock = {0};
    PDEVICE_OBJECT pTdiDevice;
	KEVENT TdiCompleteEvent;
	TDI_CONNECTION_INFORMATION remotenoderequest;

    KeInitializeEvent(&TdiCompleteEvent,  NotificationEvent, FALSE);

    pTdiDevice = IoGetRelatedDeviceObject(pfoConnection);
	pIrp = TdiBuildInternalDeviceControlIrp(TDI_Disconnect, pTdiDevice, pfoConnection, &TdiCompleteEvent, &IoStatusBlock);

    if(pIrp)
    {
		RtlZeroMemory(&remotenoderequest,sizeof(TDI_CONNECTION_INFORMATION));
		remotenoderequest.Options=0;
		remotenoderequest.OptionsLength=sizeof(ULONG);
	
        TdiBuildDisconnect(pIrp, pTdiDevice,pfoConnection, NULL,NULL,NULL,TDI_DISCONNECT_ABORT,NULL,NULL);

        ntStatus = IoCallDriver(pTdiDevice, pIrp);
        if(ntStatus == STATUS_PENDING)
        {
            KeWaitForSingleObject(&TdiCompleteEvent, Executive, KernelMode, FALSE, NULL);
            ntStatus = IoStatusBlock.Status;
        }
    }

    return ntStatus;
}


NTSTATUS TdiFuncs_Listen(PFILE_OBJECT pfoConnection)
{
    NTSTATUS ntStatus = STATUS_INSUFFICIENT_RESOURCES;
    PIRP pIrp;
    IO_STATUS_BLOCK IoStatusBlock = {0};
    PDEVICE_OBJECT pTdiDevice;
	KEVENT TdiCompleteEvent;
	TDI_CONNECTION_INFORMATION remotenoderequest;

    KeInitializeEvent(&TdiCompleteEvent,  NotificationEvent, FALSE);

    pTdiDevice = IoGetRelatedDeviceObject(pfoConnection);
	pIrp = TdiBuildInternalDeviceControlIrp(TDI_LISTEN, pTdiDevice, pfoConnection, &TdiCompleteEvent, &IoStatusBlock);

    if(pIrp)
    {
		RtlZeroMemory(&remotenoderequest,sizeof(TDI_CONNECTION_INFORMATION));
		remotenoderequest.Options=0;
		remotenoderequest.OptionsLength=sizeof(ULONG);
	
        TdiBuildListen(pIrp, pTdiDevice,pfoConnection, NULL, NULL,0,&remotenoderequest,NULL);

        ntStatus = IoCallDriver(pTdiDevice, pIrp);
        if(ntStatus == STATUS_PENDING)
        {
            KeWaitForSingleObject(&TdiCompleteEvent, Executive, KernelMode, FALSE, NULL);
            ntStatus = IoStatusBlock.Status;
        }

		DbgPrint("Done waiting:%x\n",ntStatus);		
    }

    return ntStatus;
}


NTSTATUS TdiFuncs_Receive(PFILE_OBJECT pfoConnection, PVOID pBuffer, UINT uiReceiveLength, UINT *pDataReceived)
{
    NTSTATUS ntStatus = STATUS_INSUFFICIENT_RESOURCES;
    PIRP pIrp;
    IO_STATUS_BLOCK IoStatusBlock = {0};
    PDEVICE_OBJECT pTdiDevice;
    PMDL pReceiveMdl;
	
    KeInitializeEvent(&TdiListenCompleteEvent, NotificationEvent, FALSE);

    pTdiDevice = IoGetRelatedDeviceObject(pfoConnection);
	*pDataReceived = 0;

    pReceiveMdl = IoAllocateMdl((PCHAR)pBuffer, uiReceiveLength, FALSE, FALSE, NULL);
    if(pReceiveMdl)
    {
        __try {

            MmProbeAndLockPages(pReceiveMdl, KernelMode, IoModifyAccess);

        } __except (EXCEPTION_EXECUTE_HANDLER) {
                IoFreeMdl(pReceiveMdl);
                pReceiveMdl = NULL;
        };

        if(pReceiveMdl)
        {    
            pIrp = TdiBuildInternalDeviceControlIrp(TDI_RECEIVE, pTdiDevice, pfoConnection,  &TdiListenCompleteEvent, &IoStatusBlock);
        
            if(pIrp)
            {
                TdiBuildReceive(pIrp, pTdiDevice, pfoConnection, NULL, NULL, pReceiveMdl, TDI_RECEIVE_NORMAL , uiReceiveLength);

                ntStatus = IoCallDriver(pTdiDevice, pIrp);
        
                if(ntStatus == STATUS_PENDING)
                    KeWaitForSingleObject(&TdiListenCompleteEvent, Executive, KernelMode, FALSE, NULL);

                ntStatus = IoStatusBlock.Status;
                *pDataReceived = (UINT)IoStatusBlock.Information;
            }
        }
    }
    return ntStatus;
}

NTSTATUS TdiFuncs_Send(PFILE_OBJECT pfoConnection, PVOID pData, UINT uiSendLength, UINT *pDataSent)
{
    NTSTATUS ntStatus = STATUS_INSUFFICIENT_RESOURCES;
    PIRP pIrp;
    IO_STATUS_BLOCK IoStatusBlock = {0};
    PDEVICE_OBJECT pTdiDevice;
    PMDL pSendMdl;
	KEVENT TdiCompleteEvent;

    KeInitializeEvent(&TdiCompleteEvent, NotificationEvent, FALSE);

    pTdiDevice = IoGetRelatedDeviceObject(pfoConnection);
	*pDataSent = 0;

    pSendMdl = IoAllocateMdl((PCHAR )pData, uiSendLength, FALSE, FALSE, NULL);

    if(pSendMdl)
    {		
        __try 
		{
            MmProbeAndLockPages(pSendMdl, KernelMode, IoModifyAccess);
        }
		__except (EXCEPTION_EXECUTE_HANDLER) 
		{
                IoFreeMdl(pSendMdl);
                pSendMdl = NULL;
        };

        if(pSendMdl)
        {    
			pIrp = TdiBuildInternalDeviceControlIrp(TDI_SEND, pTdiDevice, pfoConnection,  &TdiCompleteEvent, &IoStatusBlock);
        
            if(pIrp)
            {
                TdiBuildSend(pIrp, pTdiDevice, pfoConnection, NULL, NULL, pSendMdl, 0, uiSendLength);
                ntStatus = IoCallDriver(pTdiDevice, pIrp);
        
                if(ntStatus == STATUS_PENDING)
                    KeWaitForSingleObject(&TdiCompleteEvent, Executive, KernelMode, FALSE, NULL);

				ntStatus   = IoStatusBlock.Status;
                *pDataSent = (UINT)IoStatusBlock.Information;
            }
        }
    }

    return ntStatus;
}

NTSTATUS ClientEventDisconnect(PVOID TdiEventContext, CONNECTION_CONTEXT ConnectionContext, IN LONG DisconnectDataLength, IN PVOID DisconnectData, IN LONG DisconnectInformationLength, IN PVOID DisconnectInformation, IN ULONG  DisconnectFlags)
{
	DbgPrint("Disconnect\n");
	connected=FALSE;
	KeSetEvent(&TdiListenCompleteEvent,0,FALSE);
	return STATUS_SUCCESS;
}

void InitServer(void)
{
	NTSTATUS	ntStatus;
    TdiHandleTransport=NULL;
    FileObjectTransport=NULL;

	KeInitializeSpinLock(&SendLock);

	ntStatus=TdiFuncs_OpenTransportAddress(&TdiHandleTransport,&FileObjectTransport);

	if (NT_SUCCESS(ntStatus))
	{
	    TdiHandleConnection=NULL;
	    FileObjectConnection=NULL;

		ntStatus=TdiFuncs_OpenConnection(&TdiHandleConnection,&FileObjectConnection);
		if (NT_SUCCESS(ntStatus))
		{
			DbgPrint("OpenConnection successful\n");

			ntStatus=TdiFuncs_AssociateTransportAndConnection(TdiHandleTransport,FileObjectConnection);
			if (NT_SUCCESS(ntStatus))
			{
				
				DbgPrint("AssociateTransportAndConnection successfull:%d\n",KeGetCurrentIrql());
				ntStatus=TdiFuncs_SetEventHandler(FileObjectTransport,TDI_EVENT_DISCONNECT,ClientEventDisconnect,NULL);
				if (NT_SUCCESS(ntStatus))
					DbgPrint("Registered Disconnect Event\n");				
			}
			else
				DbgPrint("AssociateTransportAndConnection failed!\n");


		}
		else
			DbgPrint("OpenConnection Failed\n");            


	}
	DbgPrint("Exit InitServer\n");
	return;
}

BOOLEAN Listen()
{
	connected=NT_SUCCESS(TdiFuncs_Listen(FileObjectConnection)); 	
	return connected;
}


BOOLEAN Send(PVOID Buffer,ULONG size)
{
	//only call with paged memory, do not point directly to a address in the memory of a process
	ULONG DataSent=0,DataSent2=0;
	PCHAR b=Buffer;
	NTSTATUS ntStatus;

	if ((ULONG)Buffer<0x80000000)
		return FALSE;

	ntStatus=STATUS_SUCCESS;

	if (!connected) return FALSE;

	ntStatus=ZwWaitForSingleObject(SendEvent,FALSE,NULL);
	DbgPrint("ZwWaitForSingleObject:ntStatus=%x\n",ntStatus);

	if (NT_SUCCESS(ntStatus))
	{		
		__try
		{
			__try
			{
				if (!connected) return FALSE;

				while ((connected) && (NT_SUCCESS(ntStatus)) && (DataSent<size))
				{
					ntStatus=TdiFuncs_Send(FileObjectConnection,&b[DataSent],size-DataSent,&DataSent2);
					DataSent+=DataSent2;
				}
			}
			__finally
			{
				ntStatus=ZwSetEvent(SendEvent,NULL);
				DbgPrint("ZwSetEvent:ntStatus=%x\n",ntStatus);

				if (!NT_SUCCESS(ntStatus))
					DbgPrint("Failed to Set Event\n");
			}
		}
		__except(1)
		{
			return FALSE;
		}
	}
	else
	{
        DbgPrint("Failed to wait\n");
		return FALSE;
	}


	if (!connected) return FALSE;
	return (NT_SUCCESS(ntStatus) && connected);
}

BOOLEAN Receive(PVOID Buffer,ULONG size)
{
	ULONG DataReceived=0,DataReceived2=0;
	NTSTATUS ntStatus;
	PCHAR b=Buffer;
	ntStatus=STATUS_SUCCESS;

	while ((connected) && (NT_SUCCESS(ntStatus)) && (DataReceived<size))
	{
		ntStatus=TdiFuncs_Receive(FileObjectConnection,&b[DataReceived],size-DataReceived,&DataReceived2);
		DataReceived+=DataReceived2;
	}
	if (!connected) return FALSE;

    return NT_SUCCESS(ntStatus);
}

BOOLEAN Disconnect()
{
    NTSTATUS ntStatus;
	ntStatus=TdiFuncs_Disconnect(FileObjectConnection);

	//if NT_SUCCESS(ntStatus)
		connected=FALSE;
	return NT_SUCCESS(ntStatus);
}
