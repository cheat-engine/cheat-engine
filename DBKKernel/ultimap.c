/*
Ultimap implements the recording of all the branches in the target process
Requires dbvm for process selection
*/

#include "ntifs.h"
#include "ultimap.h"
#include "vmxhelper.h"
#include "DBKFunc.h"
#include <windef.h>


JUMPBACK perfmonJumpBackLocation;



#ifdef AMD64
PAPIC APIC_BASE=(PAPIC)0xfffffffffffe0000;
#else
PAPIC APIC_BASE=(PAPIC)0xfffe0000;
#endif

BOOL SaveToFile; //If set it will save the results to a file instead of sending a message to the usermode app that is watching the data
HANDLE FileHandle;

int perfmon_interrupt_centry(void)
{
	KIRQL old;
	
	void *temp;

	if (DS_AREA[cpunr()]->BTS_IndexBaseAddress>=DS_AREA[cpunr()]->BTS_InterruptThresholdAddress)
	{
		//undo the system flags that got set by this interrupt
		APIC_BASE->LVT_Performance_Monitor.a=APIC_BASE->LVT_Performance_Monitor.a & 0xff;
		APIC_BASE->EOI.a=0;
	
		old=KeRaiseIrqlToDpcLevel();
		enableInterrupts();

		DbgPrint("Entry cpunr=%d\n", cpunr());
		DbgPrint("Entry threadid=%d\n", PsGetCurrentThreadId());
		
		
		temp=ExAllocatePool(NonPagedPool, DS_AREA_SIZE);
		RtlCopyMemory(temp, DS_AREA[cpunr()], DS_AREA_SIZE);

		DbgPrint("temp=%p\n", temp);


		DS_AREA[cpunr()]->BTS_IndexBaseAddress=DS_AREA[cpunr()]->BTS_BufferBaseAddress;	

		


		
		KeLowerIrql(old);
		//passive mode, taskswitches and cpu switches will happen now (When this returns, I may not be on the same interrupt as I was when I started)


		if (SaveToFile)
		{
			IO_STATUS_BLOCK iosb;
			NTSTATUS r;

			//Instead of sending the data to a usermode app it was chosen to store the data to a file for later usage
			DbgPrint("Writing buffer to disk\n");			
			r=ZwWriteFile(FileHandle, NULL, NULL, NULL, &iosb,  temp, DS_AREA_SIZE, NULL, NULL); 
			DbgPrint("Done Writing. Result=%x\n", r);

			
		}
		else
		{
			//Wait till the handler has dealt with the buffer (This way the maximum allocated memory is only affected by the number of threads)
		}


		ExFreePool(temp);

		//and return to the caller process
		disableInterrupts();
		return 1;

	}
	else
		return 0; //not caused by my interrupt
}


#ifdef AMD64
extern void perfmon_interrupt();
#else
_declspec( naked ) void perfmon_interrupt( void )
{
	__asm{
		push ebp
		mov ebp,esp

		//store state
		pushad
		xor eax,eax
		mov ax,ds
		push eax

		mov ax,es
		push eax

		mov ax,fs
		push eax

		mov ax,gs
		push eax

		mov ax,0x23 //0x10 should work too, but even windows itself is using 0x23
		mov ds,ax
		mov es,ax
		mov gs,ax
		mov ax,0x30
		mov fs,ax

		call perfmon_interrupt_centry
		cmp eax,1	//set flag


		//restore state
		pop gs
		pop fs
		pop es
		pop ds
		popad

		pop ebp		

		je skip_original_perfmon

		jmp far [perfmonJumpBackLocation]

skip_original_perfmon:
		add esp,4 //undo errorcode push
		iretd
	}
}
#endif


VOID ultimap_disable_dpc(IN struct _KDPC *Dpc, IN PVOID DeferredContext, IN PVOID SystemArgumen1, IN PVOID SystemArgument2)
{
	int i;
	DbgPrint("ultimap_disable()\n");
	i=vmx_ultimap_disable();

	if (DS_AREA[cpunr()])
	{
		ExFreePool(DS_AREA[cpunr()]);
		DS_AREA[cpunr()]=NULL;
	}
}

void ultimap_disable(void)
{
	forEachCpu(ultimap_disable_dpc, NULL, NULL, NULL);

	if (SaveToFile)
	  ZwClose(FileHandle);
}

#ifdef GOATSEISFUN
BOOLEAN
 ultimap_Service(
	PKINTERRUPT InterruptObject,
	PVOID Context
	)
{
	DbgPrint("Perfmon int\n");
	return FALSE;
}
#endif

VOID ultimap_setup_dpc(IN struct _KDPC *Dpc, IN PVOID  DeferredContext, IN PVOID  SystemArgument1, IN PVOID  SystemArgument2)
/*
initializes ultimap. If the DS_AREA_SIZE is bigger than 0 it will allocate the required region (the usual option, but if not used it could be a LBR only thing)
Call this for each processor
*/
{
	struct
	{
		UINT64 cr3;
		UINT64 dbgctl_msr;
		int DS_AREA_SIZE;
	} *params;

	params=DeferredContext;

	DS_AREA_SIZE=params->DS_AREA_SIZE;
	

	DbgPrint("ultimap(%llx, %llx, %d)", params->cr3, params->dbgctl_msr, params->DS_AREA_SIZE);
	DS_AREA[cpunr()]=NULL;

	if (params->DS_AREA_SIZE)
	{
		DS_AREA[cpunr()]=ExAllocatePool(NonPagedPool, params->DS_AREA_SIZE);

		DbgPrint("DS_AREA[%d]=%p", cpunr(), DS_AREA[cpunr()]);

		//Initialize the DS_AREA 

		DS_AREA[cpunr()]->BTS_BufferBaseAddress=(QWORD)DS_AREA[cpunr()]+sizeof(DS_AREA_MANAGEMENT);
        DS_AREA[cpunr()]->BTS_BufferBaseAddress+=sizeof(BTS);

        DS_AREA[cpunr()]->BTS_BufferBaseAddress-=DS_AREA[cpunr()]->BTS_BufferBaseAddress % sizeof(BTS);

        DS_AREA[cpunr()]->BTS_IndexBaseAddress=DS_AREA[cpunr()]->BTS_BufferBaseAddress;
        DS_AREA[cpunr()]->BTS_AbsoluteMaxAddress=(QWORD)DS_AREA[cpunr()]+params->DS_AREA_SIZE-sizeof(BTS);
        DS_AREA[cpunr()]->BTS_AbsoluteMaxAddress-=DS_AREA[cpunr()]->BTS_AbsoluteMaxAddress % sizeof(BTS);
        DS_AREA[cpunr()]->BTS_AbsoluteMaxAddress++;

        DS_AREA[cpunr()]->BTS_InterruptThresholdAddress=(DS_AREA[cpunr()]->BTS_AbsoluteMaxAddress-1) - 4*sizeof(BTS);
	}
	

	if (params->dbgctl_msr & (1 << 8))
	{
		//hook the perfmon interrupt. First get the interrupt assigned (usually 0xfe, but let's be sure and read it from the apic)		

		int perfmonIVT=(APIC_BASE->LVT_Performance_Monitor.a) & 0xff;

		APIC_BASE->LVT_Performance_Monitor.a=perfmonIVT; //clear mask flag if it was set

		//now the option to hook the interrupt the 'normal' way or the official way 
		
		#ifdef GOATSEISFUN
		//Let's give the official way a try	
		int vector;
		NTSTATUS r;
		
		vector=HalGetInterruptVector (Internal, 0, 0, perfmonIVT, &Irql, &Affinity);
		DbgPrint("perfmonIVT=%d\n", perfmonIVT);
		
		DbgPrint("vector=%d\n", vector);
		DbgPrint("Irql=%d\n", Irql);
		DbgPrint("Affinity=%d\n", Affinity);

		
		r=IoConnectInterrupt(&ultimapint, ultimap_Service, NULL, NULL, vector, Irql, Irql, Latched, TRUE, Affinity, FALSE); 
		DbgPrint("IoConnectInterrupt returned %x\n", r);

		//aaand fuck it. 
		#endif
		

		if (inthook_HookInterrupt((unsigned char)perfmonIVT, getCS(), (ULONG_PTR)perfmon_interrupt, &perfmonJumpBackLocation))
			DbgPrint("Interrupt hooked\n");
		else
			DbgPrint("Failed to hook interrupt\n");

	}

	//and finally activate the mapping
	vmx_ultimap((UINT_PTR)params->cr3, params->dbgctl_msr, DS_AREA[cpunr()]);
}

void ultimap(UINT64 cr3, UINT64 dbgctl_msr, int DS_AREA_SIZE, BOOL savetofile, WCHAR *filename)
{
	struct
	{
		UINT64 cr3;
		UINT64 dbgctl_msr;
		int DS_AREA_SIZE;
	} params;



	//create file
	SaveToFile=savetofile;

	if (SaveToFile)
	{
		UNICODE_STRING usFile;	
		OBJECT_ATTRIBUTES oaFile;
		IO_STATUS_BLOCK iosb;
		NTSTATUS r;

		RtlInitUnicodeString(&usFile, filename);
		InitializeObjectAttributes(&oaFile,&usFile, OBJ_CASE_INSENSITIVE | OBJ_KERNEL_HANDLE, NULL,NULL);

		DbgPrint("Creating file %S", usFile.Buffer);

		FileHandle=0;
		r=ZwCreateFile(&FileHandle,SYNCHRONIZE|FILE_READ_DATA|FILE_APPEND_DATA | GENERIC_ALL,&oaFile,&iosb,0,FILE_ATTRIBUTE_NORMAL,0,FILE_SUPERSEDE, FILE_SEQUENTIAL_ONLY | FILE_SYNCHRONOUS_IO_NONALERT,NULL,0);
		DbgPrint("ZwCreateFile=%x\n", r);


	}

	
	params.cr3=cr3;
	params.dbgctl_msr=dbgctl_msr;
	params.DS_AREA_SIZE=DS_AREA_SIZE;

	//forEachCpu(ultimap_setup_dpc, &params, NULL, NULL);

}