#include "rootkit.h"
#include "DBKFunc.h"
#include <windef.h>

#include "vmxhelper.h"


BOOLEAN MakeWritableKM(PVOID StartAddress,UINT_PTR size)
{
	UINT_PTR PTE,PDE;
	struct PTEStruct *x;
	UINT_PTR CurrentAddress=(UINT_PTR)StartAddress;	

	while (CurrentAddress<((UINT_PTR)StartAddress+size))
	{
		//find the PTE or PDE of the selected address
		PTE=(UINT_PTR)CurrentAddress;
		PTE=PTE/0x1000*PTESize+0xc0000000;

		PTE=(UINT_PTR)StartAddress;
		PTE=PTE/0x1000*PTESize+0xc0000000;

    	//now check if the address in PTE is valid by checking the page table directory at 0xc0300000 (same location as CR3 btw)
	    PDE=PTE/0x1000*PTESize+0xc0000000; //same formula

		x=(PVOID)PDE;
		if ((x->P==0) && (x->A2==0))
		{
			CurrentAddress+=PAGE_SIZE_LARGE;
			continue;
		}

		if (x->PS==1)
		{
			//big page, no pte
			x->RW=1;
			CurrentAddress+=PAGE_SIZE_LARGE;
			continue;
		}

		CurrentAddress+=0x1000;
		x=(PVOID)PTE;
		if ((x->P==0) && (x->A2==0))
			continue; //see for explenation the part of the PDE

		x->RW=1;
	}

	return TRUE;
}

BOOLEAN MakeWritable(PVOID StartAddress,UINT_PTR size,BOOLEAN usecopyonwrite)
{
#ifndef AMD64
	struct PTEStruct *x;
	unsigned char y;
	UINT_PTR CurrentAddress=(UINT_PTR)StartAddress;	

	//Makes usermode <0x80000000 writable
	if (((UINT_PTR)StartAddress>=0x80000000) || ((UINT_PTR)StartAddress+size>=0x80000000)) 
		return MakeWritableKM(StartAddress,size); //safety check: don't do kernelmemory with this routine

	//4kb pages (assumption, I know, but thats the system i'm working with)
	//PTE/0x1000*4+0xc0000000;

	while (CurrentAddress<((UINT_PTR)StartAddress+size))
	{
		__try
		{
			y=*(PCHAR)CurrentAddress; //page it in if it wasn't loaded already
			x=(PVOID)(CurrentAddress/0x1000*PTESize+0xc0000000);
			if (x->RW==0) //if it's read only then
			{
				if (usecopyonwrite)
                    x->A1=1;  //set the copy-on-write bit to 1
				else
					x->RW=1; //just writable
			}
		}
		__except(1)
		{
			//ignore and continue
		}	

        CurrentAddress+=0x1000;
	}	

	return TRUE;
#else
	return FALSE;
#endif
}


//this unit will contain the functions and other crap used by the hider function
BOOLEAN CheckImageName(IN PUNICODE_STRING FullImageName, IN char* List,int listsize)
{
	/*
	pre:List has been initialized and all entries are UPPERCASE. Each entry is seperated
	    by a 0-marker so just setting the pointer ro the start and doing a compare will work

	*/
	ANSI_STRING tempstring;
	int i;

	DbgPrint("Checking this image name...\n");
	RtlZeroMemory(&tempstring,sizeof(ANSI_STRING));
	if (RtlUnicodeStringToAnsiString(&tempstring,FullImageName,TRUE)== STATUS_SUCCESS)
	{
		char *p;
		INT_PTR modulesize;
		__try
		{
			RtlUpperString(&tempstring,&tempstring);

			p=List;
	
			for (i=0;i<listsize;i++)
			{
				if (List[i]=='\0')
				{
					modulesize=i-(INT_PTR)(p-List);
					if (modulesize>=0)
					{	
						DbgPrint("Checking %s with %s\n",&tempstring.Buffer[tempstring.Length-modulesize],p);

						if ((tempstring.Length>=modulesize) && (strcmp(p,&tempstring.Buffer[tempstring.Length-modulesize])==0))
						{
							//we have a match!!!
							DbgPrint("It's a match with %s\n",p);
							return TRUE;	
						}						
	
					}
					p=&List[i+1];
				}
	
			}
		
			
		}
		__finally
		{
			RtlFreeAnsiString(&tempstring);	
		}
	}

	DbgPrint("No match\n");
	return FALSE;

}

VOID LoadImageNotifyRoutine(IN PUNICODE_STRING  FullImageName, IN HANDLE  ProcessId, IN PIMAGE_INFO  ImageInfo)
{
	BOOLEAN RemoveIt=FALSE;

	if ((ProtectOn) && (GlobalDenyList) && (DenyList)) //check for denylist to be %100 sure
		RemoveIt=CheckImageName(FullImageName,ModuleList,ModuleListSize);

	if ((ProtectOn) && (!RemoveIt) && (ProcessId==(HANDLE)ProtectedProcessID))
	{
		if (DenyList)
			RemoveIt=CheckImageName(FullImageName,ModuleList,ModuleListSize);
		else //it's an accept list
			RemoveIt=!CheckImageName(FullImageName,ModuleList,ModuleListSize);
	}

	
	if ((RemoveIt) && (!ImageInfo->SystemModeImage))
	{
		ULONG i;
		char* p;

		MakeWritable(ImageInfo->ImageBase,ImageInfo->ImageSize,TRUE);

		p=ImageInfo->ImageBase;
		__try
		{
			for(i=0;i<ImageInfo->ImageSize;i++)
				p[i]=0xc3;
		}
		__except(1)
		{
		}
	}
		
}


NTSTATUS NewZwOpenProcess(OUT PHANDLE ProcessHandle,  IN ACCESS_MASK DesiredAccess,  IN POBJECT_ATTRIBUTES ObjectAttributes,  IN PCLIENT_ID ClientId)
{	
#ifndef AMD64
	if ((ProtectOn) && (PsGetCurrentProcessId()!=ProtectedProcessID) && (((UINT_PTR)(ClientId->UniqueProcess)>=(UINT_PTR)ProtectedProcessID)) && ((UINT_PTR)(ClientId->UniqueProcess)<(UINT_PTR)ProtectedProcessID+4))
	{
		if ((UINT_PTR)ProcessHandle<=0x80000000) //make it more genuine
		{
			__try
			{
                *ProcessHandle=0;
			}
			__except(1)
			{

			}
		}


		return 0xC0000005;		
	
	}
	else
#endif
		return OldZwOpenProcess(ProcessHandle,DesiredAccess,ObjectAttributes,ClientId);
}

NTSTATUS NewZwQuerySystemInformation(IN SYSTEM_INFORMATION_CLASS SystemInformationClass, OUT PVOID SystemInformation,IN ULONG SystemInformationLength,OUT PULONG ReturnLength OPTIONAL )
{
	typedef struct _SYSTEM_THREAD_INFORMATION {
	    LARGE_INTEGER KernelTime;
	    LARGE_INTEGER UserTime;
	    LARGE_INTEGER CreateTime;
	    ULONG         WaitTime;
	    PVOID         StartAddress;
	    CLIENT_ID     ClientId;
	    KPRIORITY     Priority;
	    KPRIORITY     BasePriority;
	    ULONG         ContextSwitchCount;
	    LONG          State;
	    LONG          WaitReason;
	} SYSTEM_THREAD_INFORMATION, * PSYSTEM_THREAD_INFORMATION;


	typedef struct _SYSTEM_PROCESS_INFORMATION {
	    ULONG             NextEntryDelta; //long live the delta, without it i'd be lost
	    ULONG             ThreadCount;
	    ULONG             Reserved1[6];
	    LARGE_INTEGER     CreateTime;
	    LARGE_INTEGER     UserTime;
	    LARGE_INTEGER     KernelTime;
	    UNICODE_STRING    ProcessName;
	    KPRIORITY         BasePriority;
	    ULONG             ProcessId;
	    ULONG             InheritedFromProcessId;
	    ULONG             HandleCount;
	    ULONG             Reserved2[2];
	    ULONG		      VmCounters;
	    ULONG		      IoCounters;
		SYSTEM_THREAD_INFORMATION Threads[1];
	} SYSTEM_PROCESS_INFORMATION, * PSYSTEM_PROCESS_INFORMATION;

	
	NTSTATUS result;

	result=OldZwQuerySystemInformation(SystemInformationClass,SystemInformation,SystemInformationLength,ReturnLength);;

#ifndef AMD64	
	if ((ProtectOn) && (PsGetCurrentProcessId()!=ProtectedProcessID) && (SystemInformationClass==5) && (NT_SUCCESS(result))) //process/thread info
	{
		PSYSTEM_PROCESS_INFORMATION pspi;
		PSYSTEM_PROCESS_INFORMATION pspi2;
		PSYSTEM_PROCESS_INFORMATION oldpspi=NULL;
		ULONG LastDelta=0;

		pspi=SystemInformation;
		
		while (1)
		{
			if (pspi->ProcessId==(ULONG)ProtectedProcessID)
			{
	
                pspi2= (PSYSTEM_PROCESS_INFORMATION)(((PUCHAR)pspi)+pspi->NextEntryDelta);
				
				if (pspi==pspi2) //it's the last entry in the list
				{
					//just delete it and make the length shorter					
					oldpspi->NextEntryDelta=0; //mark the previous one as last entry in the list
					RtlZeroMemory(pspi,LastDelta);					
					break;
				}
				else
				{
					//link over it
					oldpspi->NextEntryDelta+=pspi->NextEntryDelta;
					RtlZeroMemory(pspi,pspi->NextEntryDelta);
					break;
				}
				
				break;
			}

            if (pspi->NextEntryDelta == 0) //end of list
				break;

			LastDelta=pspi->NextEntryDelta;
			oldpspi=pspi;
			pspi= (PSYSTEM_PROCESS_INFORMATION)(((PUCHAR)pspi)+pspi->NextEntryDelta);
		}
	}
#endif
	
	return result;
}

UINT_PTR NewNtUserQueryWindow(IN ULONG WindowHandle,IN ULONG TypeInformation)
{
#ifndef AMD64
	ULONG WindowHandleProcessID;

	if ((ProtectOn) && (PsGetCurrentProcessId()!=ProtectedProcessID))
	{
		WindowHandleProcessID=OldNtUserQueryWindow(WindowHandle,0);
		if (WindowHandleProcessID==(ULONG)ProtectedProcessID)
			return 0;
	}
#endif

	return OldNtUserQueryWindow(WindowHandle,TypeInformation);
}


NTSTATUS NewNtUserBuildHwndList(IN HDESK hdesk, IN HWND hwndNext, IN ULONG fEnumChildren, IN DWORD idThread, IN UINT cHwndMax, OUT HWND *phwndFirst, OUT ULONG* pcHwndNeeded)
{
#ifndef AMD64
	NTSTATUS result;

	if ((ProtectOn) && (PsGetCurrentProcessId()!=ProtectedProcessID))
	{
		ULONG ProcessID;
		//scan the results for windowhandles of the protected processid
		if (fEnumChildren==1)
		{
            ProcessID=OldNtUserQueryWindow((ULONG)hwndNext,0);
			if (ProcessID==(ULONG)ProtectedProcessID)
				return STATUS_UNSUCCESSFUL;
		}
		result=OldNtUserBuildHwndList(hdesk,hwndNext,fEnumChildren,idThread,cHwndMax,phwndFirst,pcHwndNeeded);

		if (result==STATUS_SUCCESS)
		{
		//not go through the list and check for windows with a process that is protected
		//if it's found, remove it from the list
			ULONG i=0;
			ULONG j;

			while (i<*pcHwndNeeded)
			{
				ProcessID=OldNtUserQueryWindow((ULONG)phwndFirst[i],0);
				if (ProcessID==(ULONG)ProtectedProcessID)
				{
					//Whoa there baby, don't tell the user this exists
					for (j=i; j<(*pcHwndNeeded)-1; j++)					
						phwndFirst[j]=phwndFirst[j+1]; //shift all handles after this one place

					phwndFirst[*pcHwndNeeded-1]=0; //just make it empty

					(*pcHwndNeeded)--; //return less 
					continue; //continue the loop and check the current i
				}
                i++;				
			}
			
		}

		return result;
	}
#endif
	return OldNtUserBuildHwndList(hdesk,hwndNext,fEnumChildren,idThread,cHwndMax,phwndFirst,pcHwndNeeded);
}

ULONG NewNtUserFindWindowEx(IN HWND hwndParent, IN HWND hwndChild, IN PUNICODE_STRING pstrClassName OPTIONAL, IN PUNICODE_STRING pstrWindowName OPTIONAL, IN DWORD dwType)
{
	ULONG result;
	result=OldNtUserFindWindowEx(hwndParent,hwndChild,pstrClassName,pstrWindowName,dwType);
#ifndef AMD64	
	if ((ProtectOn) && (PsGetCurrentProcessId()!=ProtectedProcessID))
	{
		ULONG ProcessID;
		ProcessID=OldNtUserQueryWindow(result,0);
		if (ProcessID==(ULONG)ProtectedProcessID)
			return 1230;
	}
#endif

	return result;
}

ULONG NewNtUserGetForegroundWindow(VOID)
{
	ULONG result;
	result=OldNtUserGetForegroundWindow();
#ifndef AMD64	
	if ((ProtectOn) && (PsGetCurrentProcessId()!=ProtectedProcessID))
	{
		ULONG ProcessID;
		ProcessID=OldNtUserQueryWindow(result,0);
		if (ProcessID==(ULONG)ProtectedProcessID)
			result=LastForegroundWindow;
		else
            LastForegroundWindow=result;
	}
#endif
	
	return result;
}

_declspec( naked ) NTSTATUS OriginalObOpenObjectByPointer(IN PVOID Object, IN ULONG HandleAttributes, IN PACCESS_STATE PassedAccessState OPTIONAL, IN ACCESS_MASK DesiredAccess,
													  IN POBJECT_TYPE ObjectType, IN KPROCESSOR_MODE AccessMode, OUT PHANDLE Handle )
{
	__asm
	{
		mov edi,edi
		push ebp
		mov ebp,esp
		//jmp back to the originalcode AFTER the jmp
		mov ebp,esp
		mov ebp,esp
		mov ebp,esp
		mov ebp,esp
		mov ebp,esp
		mov ebp,esp
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
	}


}


NTSTATUS NewObOpenObjectByPointer (IN PVOID Object, IN ULONG HandleAttributes, IN PACCESS_STATE PassedAccessState OPTIONAL, IN ACCESS_MASK DesiredAccess,
								   IN POBJECT_TYPE ObjectType, IN KPROCESSOR_MODE AccessMode, OUT PHANDLE Handle )
{
	/*if (testprotect==Object)
	{	
		*Handle=NULL;
		return 0;
	}*/

	return OriginalObOpenObjectByPointer(Object,HandleAttributes,PassedAccessState,DesiredAccess,ObjectType,AccessMode,Handle);

}


void cr3_change_callback(ULONG oldcr3, ULONG newcr3)
{
	/*	ULONG *stack;

	HANDLE pid=PsGetCurrentProcessId();
	ULONG pep=(ULONG)PsGetCurrentProcess();
	HANDLE tid=PsGetCurrentThreadId();
	HANDLE pet=PsGetCurrentThread();


   // if newcr3 = protectedprocess and pid isn't protected process then block
	
	stack=(ULONG *)(((ULONG)&oldcr3)-4);

	//DbgPrint("stack=%p ...\n",stack);
	//DbgPrint("stack[0]=%x\n",stack[0]);
	//DbgPrint("stack[1]=%x\n",stack[1]);
	//DbgPrint("stack[2]=%x\n",stack[2]);	

	DbgPrint("%d: CR3 changed at %x : oldcr3=%x newcr3=%x - PID=%x PEProcess=%x TID=%x PEThread=%x\n", cpunr(), stack[0], oldcr3, newcr3, pid,pep,tid,pet);

    //KeBugCheckEx(0xce, (ULONG)stack,stack[0],stack[1],stack[2]);

	//if ((newcr3==ProtectedCR3) && ( stack[0]!=0x8086ddbc ) ) //&& (pid != ProtectedProcessID)) //csrss.exe + explorer.exe
	if ((newcr3==ProtectedCR3) && (pid != ProtectedProcessID) && (pid != 0))
	{
		DbgPrint("%d:This would be blocked. FakeCR3 = %x\n",cpunr(), FakeCR3);
		vmx_exit_cr3_callback(FakeCR3);
	}


	vmx_exit_cr3_callback(newcr3);*/
	DbgPrint("OMGWTF I'M STILL HERE! WAAAAAAAAAAAAH!!!!!\n");
}