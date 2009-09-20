#pragma warning( disable: 4103)

#include "IOPLDispatcher.h"
#include "DBKFunc.h"
#include "DBKDrvr.h"


#include "memscan.h"

#include "rootkit.h"

#include "processlist.h"
#include "threads.h"

#include "newkernel.h"
#include "vmxhelper.h"
#include "interruptHook.h"
#include "debugger.h"
#include "stealthedit.h"
#include "vmxoffload.h"



PSERVICE_DESCRIPTOR_TABLE KeServiceDescriptorTableShadow=NULL;
PSERVICE_DESCRIPTOR_TABLE KeServiceDescriptorTable=NULL;


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
	
	DbgPrint("NormalRoutine=%x\n",*(PUINT_PTR)NormalRoutine);
	DbgPrint("NormalContext=%x\n",*(PUINT_PTR)NormalContext);
	DbgPrint("SystemArgument1=%x\n",*(PUINT_PTR)SystemArgument1);
	DbgPrint("SystemArgument1=%x\n",*(PUINT_PTR)SystemArgument2);
	
	
	KeInitializeApc(kApc,
		            (PKTHREAD)PsGetCurrentThread(),
                    0,
                    (PKKERNEL_ROUTINE)mykapc2,
                    NULL,
                    (PKNORMAL_ROUTINE)*(PUINT_PTR)SystemArgument1,
                    UserMode,
                    (PVOID)*(PUINT_PTR)NormalContext
                    );

	KeInsertQueueApc (kApc, (PVOID)*(PUINT_PTR)SystemArgument1, (PVOID)*(PUINT_PTR)SystemArgument2, 0);


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

NTSTATUS DispatchIoctl(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp)
{
	NTSTATUS ntStatus=STATUS_UNSUCCESSFUL;
    PIO_STACK_LOCATION     irpStack = IoGetCurrentIrpStackLocation(Irp);

	
	
    switch(irpStack->Parameters.DeviceIoControl.IoControlCode)
    {
		
        case IOCTL_CE_READMEMORY:			
			__try
			{
				struct input
				{
					DWORD processid;
					UINT64 startaddress;
					WORD bytestoread;
				} *pinp;

				pinp=Irp->AssociatedIrp.SystemBuffer;

				ntStatus=ReadProcessMemory(pinp->processid,NULL,(PVOID)pinp->startaddress,pinp->bytestoread,pinp) ? STATUS_SUCCESS : STATUS_UNSUCCESSFUL;
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
					DWORD processid;
					UINT64 startaddress;
					WORD bytestowrite;
				} *pinp,inp;

				pinp=Irp->AssociatedIrp.SystemBuffer;
				ntStatus=WriteProcessMemory(pinp->processid,NULL,(PVOID)pinp->startaddress,pinp->bytestowrite,(PVOID)((UINT_PTR)pinp+sizeof(inp))) ? STATUS_SUCCESS : STATUS_UNSUCCESSFUL;
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
				ULONG processid=*(PULONG)Irp->AssociatedIrp.SystemBuffer;
				HANDLE ProcessHandle;


				ntStatus=STATUS_SUCCESS;

				__try
				{
					ProcessHandle=0;

					if (PsLookupProcessByProcessId((PVOID)(UINT_PTR)(processid),&selectedprocess)==STATUS_SUCCESS)
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
				
				*(PUINT64)Irp->AssociatedIrp.SystemBuffer=(UINT64)ProcessHandle;
				break;
			}
			

		case IOCTL_CE_OPENTHREAD:
			{
				HANDLE ThreadHandle;
				CLIENT_ID ClientID;
				OBJECT_ATTRIBUTES ObjectAttributes;
	
				RtlZeroMemory(&ObjectAttributes,sizeof(OBJECT_ATTRIBUTES));

				ntStatus=STATUS_SUCCESS;

				ClientID.UniqueProcess=0;
				ClientID.UniqueThread=(HANDLE)(UINT64)*(PULONG)Irp->AssociatedIrp.SystemBuffer;
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
			
				*(PUINT64)Irp->AssociatedIrp.SystemBuffer=(UINT64)ThreadHandle;
				

				break;
			}

			
		case IOCTL_CE_MAKEWRITABLE:
			{
#ifdef AMD64
				//untill I know how win64 handles paging, not implemented
#else
				struct InputBuf
				{
				    UINT64 StartAddress;
					ULONG Size;
					BYTE CopyOnWrite;
				} *PInputBuf;

				PInputBuf=Irp->AssociatedIrp.SystemBuffer;
				
				ntStatus=MakeWritable((PVOID)(UINT_PTR)PInputBuf->StartAddress,PInputBuf->Size,(PInputBuf->CopyOnWrite==1)) ? STATUS_SUCCESS : STATUS_UNSUCCESSFUL; 
#endif
				break;
			}
			


		case IOCTL_CE_QUERY_VIRTUAL_MEMORY:
			{
				struct InputBuf
				{
				    DWORD ProcessID;
					UINT64 StartAddress;
				} *PInputBuf;

				struct OutputBuf
				{				
					DWORD length;
					DWORD protection;
				} *POutputBuf;

				
			     
				UINT_PTR BaseAddress;
				UINT_PTR length;

				
                ntStatus=STATUS_SUCCESS;
				PInputBuf=Irp->AssociatedIrp.SystemBuffer;
				POutputBuf=Irp->AssociatedIrp.SystemBuffer;

				ntStatus=GetMemoryRegionData(PInputBuf->ProcessID,NULL,(PVOID)(PInputBuf->StartAddress),&(POutputBuf->protection),&length,&BaseAddress) ? STATUS_SUCCESS : STATUS_UNSUCCESSFUL;
				POutputBuf->length=(DWORD)length;

				
				break;
			}
			

		case IOCTL_CE_TEST: //just a test to see it's working
			{
				//PEPROCESS selectedprocess=NULL;

				DbgPrint("test\n");

				/*
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
				*/
			


				break;
			}

		case IOCTL_CE_GETPETHREAD:
			{
				
				*(PUINT64)Irp->AssociatedIrp.SystemBuffer=(UINT64)getPEThread((UINT_PTR)*(PULONG)Irp->AssociatedIrp.SystemBuffer);
				ntStatus= STATUS_SUCCESS;
				break;
			}
			

		case IOCTL_CE_GETPEPROCESS:
			{
				DWORD processid=*(PDWORD)Irp->AssociatedIrp.SystemBuffer;
				PEPROCESS selectedprocess;
			

				if (processid==0)
				{
					ntStatus=STATUS_UNSUCCESSFUL;
				}
				else
				{
					if (PsLookupProcessByProcessId((PVOID)(UINT_PTR)(processid),&selectedprocess)==STATUS_SUCCESS)
						*(PUINT64)Irp->AssociatedIrp.SystemBuffer=(UINT64)selectedprocess;
					else
						*(PUINT64)Irp->AssociatedIrp.SystemBuffer=0;
				}

				ObDereferenceObject(selectedprocess);

				ntStatus= STATUS_SUCCESS;				
				break;
			}

			
		case IOCTL_CE_READPHYSICALMEMORY:
			{
				struct input
				{
					UINT64 startaddress;
					UINT64 bytestoread;
				} *pinp;
				pinp=Irp->AssociatedIrp.SystemBuffer;

				DbgPrint("IOCTL_CE_READPHYSICALMEMORY:pinp->startaddress=%p, pinp->bytestoread=%d", pinp->startaddress, pinp->bytestoread); 


				ntStatus = ReadPhysicalMemory((PVOID)(UINT_PTR)pinp->startaddress, (UINT_PTR)pinp->bytestoread, pinp);
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
			    ntStatus=ZwOpenSection( &physmem, SECTION_ALL_ACCESS, &attributes );
				if (ntStatus==STATUS_SUCCESS)
				{
					//hey look, it didn't kill it
					struct input
					{
						char *startaddress;
						UINT64 bytestoread;
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
					toread=(UINT_PTR)pinp->bytestoread;

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
					DWORD  ProcessID;
					UINT64 BaseAddress; 
				} *pinp;

				PEPROCESS selectedprocess;
				PHYSICAL_ADDRESS physical;

				ntStatus=STATUS_SUCCESS;
				pinp=Irp->AssociatedIrp.SystemBuffer;

				DbgPrint("IOCTL_CE_GETPHYSICALADDRESS. ProcessID(%p)=%x BaseAddress(%p)=%x\n",&pinp->ProcessID, pinp->ProcessID, &pinp->BaseAddress, pinp->BaseAddress);

				__try
				{
					//switch to the selected process
					if (PsLookupProcessByProcessId((PVOID)(UINT64)(pinp->ProcessID),&selectedprocess)==STATUS_SUCCESS)	
					{
						KAPC_STATE apc_state;
						RtlZeroMemory(&apc_state,sizeof(apc_state));					
    					KeStackAttachProcess((PVOID)selectedprocess,&apc_state);
                 
						__try
						{
							physical=MmGetPhysicalAddress((PVOID)pinp->BaseAddress);
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
				{
					DbgPrint("physical.LowPart=%x",physical.LowPart);
                    RtlCopyMemory(Irp->AssociatedIrp.SystemBuffer,&physical.QuadPart,8);

				}
				
				
				break;
			}
			

		case IOCTL_CE_PROTECTME:
			{
#ifdef AMD64
				DbgPrint("Stealthmode is not available for 64-bit\n");
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
#ifdef AMD64
				DbgPrint("What doesn't go up, can't crash down\n");
				ntStatus=STATUS_SUCCESS;
#else
				//Unhook();
				if (ProtectOn)
					ntStatus=STATUS_UNSUCCESSFUL;
				else
					ntStatus=STATUS_SUCCESS;

				//ProtectOn=FALSE;
#endif

				break;
			}


		case IOCTL_CE_SETSDTADDRESS:
			{
#ifdef AMD64
				DbgPrint("Changing the sdt is not allowed in 64-bit windows\n");
				ntStatus=STATUS_UNSUCCESSFUL;
#else
				struct input
				{
					int table; //0=SDT, 1=SSDT
				  	int nr;	
					ULONG address;
					UCHAR paramcount;
				} *pinp;
				pinp=Irp->AssociatedIrp.SystemBuffer;
			

				disableInterrupts();
				setCR0(getCR0() & (~(0x10000)));

				
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
				setCR0(getCR0() ^ 0x10000);
				enableInterrupts();

				ntStatus=STATUS_SUCCESS;
#endif
				break;
			}


		case IOCTL_CE_GETSDTADDRESS:
			{
#ifdef AMD64
				DbgPrint("I have no idea what the sdt is in 64-bit\n");
				ntStatus=STATUS_UNSUCCESSFUL;
#else
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
#endif
				break;
			}
			

		case IOCTL_CE_GETCR0:
			{
				*(UINT64*)Irp->AssociatedIrp.SystemBuffer=getCR0();
				ntStatus=STATUS_SUCCESS;

				break;
			}
			

		case IOCTL_CE_GETCR4:
			{
				//seems CR4 isn't seen as a register...
				*(UINT64*)Irp->AssociatedIrp.SystemBuffer=(UINT64)getCR4();
				ntStatus=STATUS_SUCCESS;

				break;
			}
			

		case IOCTL_CE_SETCR4:
			{
				//seems CR4 isn't seen as a register...
				ULONG cr4reg=*(ULONG*)Irp->AssociatedIrp.SystemBuffer;
				setCR4((UINT64)cr4reg);
				ntStatus=STATUS_SUCCESS;
				break;
			}
			

		case IOCTL_CE_GETCR3:
			{
				UINT64 cr3reg=0;
				PEPROCESS selectedprocess;


				ntStatus=STATUS_SUCCESS;

				//switch context to the selected process.  (processid is stored in the systembuffer)
				if (PsLookupProcessByProcessId((PVOID)(UINT64)(*(ULONG*)Irp->AssociatedIrp.SystemBuffer),&selectedprocess)==STATUS_SUCCESS)	
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

				*(UINT64*)Irp->AssociatedIrp.SystemBuffer=cr3reg;

				break;
			}

			

		case IOCTL_CE_GETSDT:
			{
				//returns the address of KeServiceDescriptorTable
				ntStatus=STATUS_SUCCESS;
				*(UINT64*)Irp->AssociatedIrp.SystemBuffer=(UINT64)KeServiceDescriptorTable;
				break;
			}	
			


		case IOCTL_CE_GETIDT:
			{
				//returns the address of the IDT of the current CPU
				IDT idt;
				RtlZeroMemory(&idt,sizeof(IDT));				
				GetIDT(&idt);
				RtlZeroMemory(Irp->AssociatedIrp.SystemBuffer,2+8); //so that the 32-bit version doesn't have to deal with garbage at the end
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
				RtlZeroMemory(Irp->AssociatedIrp.SystemBuffer,2+8); 
				RtlCopyMemory(Irp->AssociatedIrp.SystemBuffer,&gdt,sizeof(GDT)); //copy gdt
				ntStatus=STATUS_SUCCESS;
			
				break;
			}
			


		case IOCTL_CE_HOOKSTEALTHEDITINTS:
			{
				DbgPrint("IOCTL_CE_HOOKSTEALTHEDITINTS\n");


				if (stealthedit_initStealthEditHooksForCurrentCPU())
				{
					DbgPrint("stealthedit_initStealthEditHooksForCurrentCPU returned TRUE\n");
					ntStatus=STATUS_SUCCESS;
				}
				else
				{
					DbgPrint("stealthedit_initStealthEditHooksForCurrentCPU returned FALSE\n");
					ntStatus=STATUS_UNSUCCESSFUL;
				}

				break;

			}
		



		case IOCTL_CE_ADDCLOAKEDSECTION:
			{
				struct intput
				{
					DWORD ProcessID;
					UINT64 pagebase;
					UINT64 relocatedpagebase;
					int	  size;
				} *pinp;

				DbgPrint("IOCTL_CE_ADDCLOAKEDSECTION\n");
				pinp=Irp->AssociatedIrp.SystemBuffer;
				if (stealthedit_AddCloakedSection(pinp->ProcessID, (UINT_PTR)pinp->pagebase, (UINT_PTR)pinp->relocatedpagebase, pinp->size))
					ntStatus=STATUS_SUCCESS;
				else
					ntStatus=STATUS_UNSUCCESSFUL;

				break;
			}

		case IOCTL_CE_REMOVECLOAKEDSECTION:
			{
				struct intput
				{
					DWORD ProcessID;
					UINT64 pagebase;
				} *pinp;
				pinp=Irp->AssociatedIrp.SystemBuffer;

				if (stealthedit_RemoveCloakedSection(pinp->ProcessID, (UINT_PTR)pinp->pagebase))
					ntStatus=STATUS_SUCCESS;
				else
					ntStatus=STATUS_UNSUCCESSFUL;
				break;
			}

			
		case IOCTL_CE_LAUNCHDBVM:
			{
				struct intput
				{
					UINT_PTR dbvmimgpath;				
				} *pinp;
				pinp=Irp->AssociatedIrp.SystemBuffer;
				DbgPrint("IOCTL_CE_LAUNCHDBVM\n");

				vmxoffload((PCWSTR)pinp->dbvmimgpath);

				DbgPrint("Returned from vmxofload()\n");
				break;
			}
			

		case IOCTL_CE_HOOKINTS:
			{
				DbgPrint("IOCTL_CE_HOOKINTS for cpu %d\n", cpunr());
				if (debugger_initHookForCurrentCPU())
					ntStatus=STATUS_SUCCESS;
				else
				    ntStatus=STATUS_UNSUCCESSFUL;

				break;
			}

		case IOCTL_CE_SETGLOBALDEBUGSTATE:
			{
				struct intput
				{
					BOOL newstate;
				} *pinp;
				pinp=Irp->AssociatedIrp.SystemBuffer;

				debugger_setGlobalDebugState(pinp->newstate);
				ntStatus=STATUS_SUCCESS;
				break;
			}

		case IOCTL_CE_DEBUGPROCESS:
			{
				struct input
				{					
					DWORD	ProcessID;
				} *pinp;

			
				pinp=Irp->AssociatedIrp.SystemBuffer;
				debugger_startDebugging(pinp->ProcessID);

				ntStatus=STATUS_SUCCESS;

				break;

			}

		case IOCTL_CE_STOPDEBUGGING:
			{
				debugger_stopDebugging();
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
					UINT64 addresstoexecute;										
				} *inp;
				inp=Irp->AssociatedIrp.SystemBuffer;

				CreateRemoteAPC(inp->threadid,(PVOID)(UINT_PTR)inp->addresstoexecute);
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
					UINT64 BaseAddress;
					ULONG Size;
					ULONG AllocationType;
					ULONG Protect;
				} *inp;
				PEPROCESS selectedprocess;

				PVOID BaseAddress;
				SIZE_T RegionSize;


				inp=Irp->AssociatedIrp.SystemBuffer;
				BaseAddress=(PVOID)(UINT_PTR)inp->BaseAddress;
				RegionSize=inp->Size;




				if (PsLookupProcessByProcessId((PVOID)(UINT64)(inp->ProcessID),&selectedprocess)==STATUS_SUCCESS)	
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
							*(PUINT64)Irp->AssociatedIrp.SystemBuffer=(UINT64)BaseAddress;

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
				*(PUINT64)Irp->AssociatedIrp.SystemBuffer=(UINT64)address;

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
					UINT64 s;
				} *inp;
				UNICODE_STRING y;
				UINT64 result;
				PVOID x;



				inp=Irp->AssociatedIrp.SystemBuffer;

				RtlInitUnicodeString(&y, (PCWSTR)(UINT_PTR)(inp->s));
				x=MmGetSystemRoutineAddress(&y);
				result=(UINT64)x;


				RtlCopyMemory(Irp->AssociatedIrp.SystemBuffer,&result,8);
				ntStatus=STATUS_SUCCESS;

				break;
			}
/*x
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
*/
			/*x

		case IOCTL_CE_CONTINUEDEBUGEVENT:
			{
				struct input
				{
					BOOL handled;
				} *inp=Irp->AssociatedIrp.SystemBuffer;

				DbgPrint("IOCTL_CE_CONTINUEDEBUGEVENT\n");
				ntStatus=debugger_continueDebugEvent(inp->handled);
				break;

			}

		case IOCTL_CE_WAITFORDEBUGEVENT:
			{
				struct input
				{
					ULONG timeout;
				} *inp=Irp->AssociatedIrp.SystemBuffer;

				ntStatus=debugger_waitForDebugEvent(inp->timeout);

				break;

			}

		case IOCTL_CE_GETDEBUGGERSTATE:
			{	
				DbgPrint("IOCTL_CE_GETDEBUGGERSTATE\n");
				ntStatus=debugger_getDebuggerState((PDebugStackState)(Irp->AssociatedIrp.SystemBuffer));
				
				DbgPrint("ntStatus=%x eax=%x\n",ntStatus, ((PDebugStackState)(Irp->AssociatedIrp.SystemBuffer))->eax);
				break;
			}

		case IOCTL_CE_SETDEBUGGERSTATE:
			{	
				DbgPrint("IOCTL_CE_SETDEBUGGERSTATE: state->eax=%x\n", ((PDebugStackState)(Irp->AssociatedIrp.SystemBuffer))->eax);
				ntStatus=debugger_setDebuggerState((PDebugStackState)Irp->AssociatedIrp.SystemBuffer);
				break;
			}

		
		case IOCTL_CE_GD_SETBREAKPOINT:
			{
				struct input
				{
					BOOL active;
					int debugregspot;
					DWORD address;
					DWORD breakType;
					DWORD breakLength;
				} *inp=Irp->AssociatedIrp.SystemBuffer;
				
				DbgPrint("sizeof(struct input)=%d\n",sizeof(struct input));
				DbgPrint("breakType=%d breakLength=%d\n",inp->breakType,inp->breakLength);
				ntStatus=debugger_setGDBreakpoint(inp->debugregspot, inp->address, (BreakType)inp->breakType, (BreakLength)inp->breakLength);
				break;
			}

		case IOCTL_CE_TOUCHDEBUGREGISTER: //used after setting a global debug breakpoint
			{
				debugger_touchDebugRegister();
				ntStatus=STATUS_SUCCESS;
				break;

			}

*/
		case IOCTL_CE_GETVERSION:
			{
				DbgPrint("IOCTL_CE_GETVERSION. Version=%d\n",dbkversion);
				*(PULONG)Irp->AssociatedIrp.SystemBuffer=dbkversion;	
				ntStatus=STATUS_SUCCESS;
				break;
			}
			

		case IOCTL_CE_INITIALIZE:
			{
				//find the KeServiceDescriptorTableShadow 
				struct input
				{
					UINT64 AddressOfWin32K;
					UINT64 SizeOfWin32K;
					UINT64 NtUserBuildHwndList_callnumber;
					UINT64 NtUserQueryWindow_callnumber;
					UINT64 NtUserFindWindowEx_callnumber;
					UINT64 NtUserGetForegroundWindow_callnumber;
					UINT64 ActiveLinkOffset;
					UINT64 ProcessNameOffset;
					UINT64 DebugportOffset;	
					UINT64 ProcessEvent;
					UINT64 ThreadEvent;
  				} *pinp;

		
				int i;
				PSERVICE_DESCRIPTOR_TABLE PossibleKeServiceDescriptorTableShow; //long name's are FUN!!!!

				DbgPrint("IOCTL_CE_INITIALIZE\n");
				pinp=Irp->AssociatedIrp.SystemBuffer;
				ntStatus=STATUS_SUCCESS;

				//no stealth for x64
				
				PossibleKeServiceDescriptorTableShow=KeServiceDescriptorTable;

				               
#ifndef AMD64
				ntStatus=STATUS_UNSUCCESSFUL; 
				NtUserBuildHwndList_callnumber=(ULONG)pinp->NtUserBuildHwndList_callnumber;
				NtUserQueryWindow_callnumber=(ULONG)pinp->NtUserQueryWindow_callnumber;
				NtUserFindWindowEx_callnumber=(ULONG)pinp->NtUserFindWindowEx_callnumber;
				NtUserGetForegroundWindow_callnumber=(ULONG)pinp->NtUserGetForegroundWindow_callnumber;
#endif
				ActiveLinkOffset=(UINT_PTR)pinp->ActiveLinkOffset;
				ProcessNameOffset=(UINT_PTR)pinp->ProcessNameOffset;
				DebugportOffset=(UINT_PTR)pinp->DebugportOffset;



				//referencing event handles to objects

				ObReferenceObjectByHandle((HANDLE)(UINT_PTR)pinp->ProcessEvent, EVENT_ALL_ACCESS, NULL,KernelMode, &ProcessEvent, NULL); 
				ObReferenceObjectByHandle((HANDLE)(UINT_PTR)pinp->ThreadEvent, EVENT_ALL_ACCESS, NULL,KernelMode, &ThreadEvent, NULL); 
				
#ifndef AMD64
				//lookup the Shadow table.

				//in win2k sp4 the distance is even bigger than -6, at least 21 entries down to find it

				i=-25;//takes some longer to load now....
				while (i<25)
				{
					if (IsAddressSafe((UINT_PTR)&PossibleKeServiceDescriptorTableShow[i])) //dont want to crash for a page pault now do we?
 					{
						
						//look for a entry that looks like:
						//unsigned int *ServiceTable=Region of Win32K.sys
						//unsigned int *ServiceCounterTableBase=00000000 but lets be safe and dont check it in case of a checked build
						//unsigned int NumberOfServices=smaller than 0xffff;
						//unsigned char *ParamTableBase=Region of Win32K.sys;
						
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

							
							//AddSystemServices();
							
							break;
						}


					}
					i++;
				}	
#endif

				break;
			}
			/*

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


			*/

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
