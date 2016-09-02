#pragma warning( disable: 4103)

#include "IOPLDispatcher.h"
#include "DBKFunc.h"
#include "DBKDrvr.h"


#include "memscan.h"

#include "deepkernel.h"

#include "processlist.h"
#include "threads.h"

#include "interruptHook.h"
#include "debugger.h"

#include "vmxhelper.h"
#include "vmxoffload.h"
#include "ultimap.h"
#include "ultimap2.h"

UINT64 PhysicalMemoryRanges=0; //initialized once, and used thereafter. If the user adds/removes ram at runtime, screw him and make him the reload the driver
UINT64 PhysicalMemoryRangesListSize=0;

PSERVICE_DESCRIPTOR_TABLE KeServiceDescriptorTableShadow=NULL;
PSERVICE_DESCRIPTOR_TABLE KeServiceDescriptorTable=NULL;

typedef PCHAR (*GET_PROCESS_IMAGE_NAME) (PEPROCESS Process); 
GET_PROCESS_IMAGE_NAME PsGetProcessImageFileName; 


/*
typedef struct
{
	int listcount;
	char cpunrs[255];
} CPULISTFILLSTRUCT, *PCPULISTFILLSTRUCT;

VOID GetCPUIDS_all(PCPULISTFILLSTRUCT p)
{
	DbgPrint("GetCPUIDS_all(for cpu %d)\n", cpunr());
	if (p->listcount<255)
	{
		p->cpunrs[p->listcount]=cpunr();
		p->listcount++;
	}
}
*/


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

BOOL DispatchIoctlDBVM(IN PDEVICE_OBJECT DeviceObject, ULONG IoControlCode, PVOID lpInBuffer, DWORD nInBufferSize, PVOID lpOutBuffer, DWORD nOutBufferSize, PDWORD lpBytesReturned)
/*
Called if dbvm has loaded the driver. Use this to setup a fake irp
*/
{
	//allocate a in and out buffer
	//setup a fake IRP
	IRP FakeIRP;
	BOOL r;
	PVOID buffer;
	buffer=ExAllocatePool(PagedPool, max(nInBufferSize, nOutBufferSize));
	RtlCopyMemory(buffer, lpInBuffer, nInBufferSize);	


	DbgPrint("DispatchIoctlDBVM\n");

	FakeIRP.AssociatedIrp.SystemBuffer=buffer;
	FakeIRP.Flags=IoControlCode; //(ab)using an unused element

	r=DispatchIoctl(DeviceObject, &FakeIRP)==STATUS_SUCCESS;


	RtlCopyMemory(lpOutBuffer, buffer, nOutBufferSize);

	ExFreePool(buffer);

	return r;
}

NTSTATUS DispatchIoctl(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp)
{
	NTSTATUS ntStatus=STATUS_UNSUCCESSFUL;

    PIO_STACK_LOCATION     irpStack=NULL;
	LUID sedebugprivUID;
	ULONG IoControlCode;

	if (!loadedbydbvm)
	{
		irpStack=IoGetCurrentIrpStackLocation(Irp);
		IoControlCode=irpStack->Parameters.DeviceIoControl.IoControlCode;
	}
	else
		IoControlCode=Irp->Flags;
		
	//DbgPrint("DispatchIoctl. IoControlCode=%x\n", IoControlCode);
/*
	sedebugprivUID.LowPart=SE_DEBUG_PRIVILEGE;
	sedebugprivUID.HighPart=0;

	
	if (SeSinglePrivilegeCheck(sedebugprivUID, UserMode)==FALSE)
	{
		DbgPrint("DispatchIoctl called by a process without SeDebugPrivilege");
		return STATUS_UNSUCCESSFUL;
	}
	*/
	
	
    switch(IoControlCode)
    {
		
        case IOCTL_CE_READMEMORY:			
			__try
			{
				struct input
				{
					UINT64 processid;
					UINT64 startaddress;
					WORD bytestoread;
				} *pinp;

				pinp=Irp->AssociatedIrp.SystemBuffer;

				ntStatus=ReadProcessMemory((DWORD)pinp->processid,NULL,(PVOID)pinp->startaddress,pinp->bytestoread,pinp) ? STATUS_SUCCESS : STATUS_UNSUCCESSFUL;
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
					UINT64 processid;
					UINT64 startaddress;
					WORD bytestowrite;
				} *pinp,inp;

				DbgPrint("sizeof(inp)=%d\n",sizeof(inp));
				pinp=Irp->AssociatedIrp.SystemBuffer;
				ntStatus=WriteProcessMemory((DWORD)pinp->processid,NULL,(PVOID)pinp->startaddress,pinp->bytestowrite,(PVOID)((UINT_PTR)pinp+sizeof(inp))) ? STATUS_SUCCESS : STATUS_UNSUCCESSFUL;
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

							//DbgPrint("Calling ObOpenObjectByPointer\n");
							ntStatus=ObOpenObjectByPointer ( 
										selectedprocess,
										0,
										NULL,
										PROCESS_ALL_ACCESS,
										*PsProcessType,
										KernelMode, //UserMode,
										&ProcessHandle);

							//DbgPrint("ntStatus=%x",ntStatus);
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
				    UINT64 ProcessID;
					UINT64 StartAddress;
				} *PInputBuf;

				struct OutputBuf
				{				
					UINT64 length;
					DWORD protection;
				} *POutputBuf;

				
			     
				UINT_PTR BaseAddress;
				UINT_PTR length;
				BOOL ShowResult=0;

				
                ntStatus=STATUS_SUCCESS;
				PInputBuf=Irp->AssociatedIrp.SystemBuffer;
				POutputBuf=Irp->AssociatedIrp.SystemBuffer;


				if (PInputBuf->StartAddress==(UINT64)0x12000)
					ShowResult=1;
				

				__try
				{
					ntStatus = GetMemoryRegionData((DWORD)PInputBuf->ProcessID, NULL, (PVOID)(PInputBuf->StartAddress), &(POutputBuf->protection), &length, &BaseAddress);
				}
				__except(1)
				{
					DbgPrint("GetMemoryRegionData error");
					ntStatus = STATUS_UNSUCCESSFUL;
					break;
				}

				POutputBuf->length=(UINT64)length;

				if (ShowResult)
				{									
				  DbgPrint("GetMemoryRegionData returned %x\n",ntStatus);
				  DbgPrint("protection=%x\n",POutputBuf->protection);
				  DbgPrint("length=%p\n",POutputBuf->length);
				  DbgPrint("BaseAddress=%p\n", BaseAddress);
				}


				
				break;
			}
			

		case IOCTL_CE_TEST: //just a test to see it's working
			{
				//PEPROCESS selectedprocess=NULL;

				KIRQL old;
				DbgPrint("test\n");

				old=KeRaiseIrqlToDpcLevel();
				//PsSuspendProcess(PsGetCurrentProcess());				
				//DbgPrint("after suspend\n");

				KeLowerIrql(old);

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
					{
#ifdef AMD64
						*(PUINT64)Irp->AssociatedIrp.SystemBuffer=(UINT64)selectedprocess;
#else
						*(PUINT64)Irp->AssociatedIrp.SystemBuffer=(DWORD)selectedprocess;
#endif
						//DbgPrint("PEProcess=%llx\n", *(PUINT64)Irp->AssociatedIrp.SystemBuffer);
					}
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

				DbgPrint("IOCTL_CE_READPHYSICALMEMORY:pinp->startaddress=%x, pinp->bytestoread=%d", pinp->startaddress, pinp->bytestoread); 


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
						UINT64 startaddress;
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
						RtlCopyMemory(&memoryview[offset],&pinp2[16],toread);

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
					UINT64  ProcessID;
					UINT64 BaseAddress; 
				} *pinp;

				PEPROCESS selectedprocess;
				PHYSICAL_ADDRESS physical;

				ntStatus=STATUS_SUCCESS;
				pinp=Irp->AssociatedIrp.SystemBuffer;

				//DbgPrint("IOCTL_CE_GETPHYSICALADDRESS. ProcessID(%p)=%x BaseAddress(%p)=%x\n",&pinp->ProcessID, pinp->ProcessID, &pinp->BaseAddress, pinp->BaseAddress);

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
					//DbgPrint("physical.LowPart=%x",physical.LowPart);
                    RtlCopyMemory(Irp->AssociatedIrp.SystemBuffer,&physical.QuadPart,8);

				}
				
				
				break;
			}

		case IOCTL_CE_GETMEMORYRANGES:
			{
				
				struct output
				{
					UINT64 address;
					UINT64 size;
				} *poutp=Irp->AssociatedIrp.SystemBuffer;

				DbgPrint("IOCTL_CE_GETMEMORYRANGES\n");


				if (PhysicalMemoryRanges==0)
				{
					__try
					{
						PPHYSICAL_MEMORY_RANGE mr=MmGetPhysicalMemoryRanges();
						
						if (mr)
						{
							//find the end
							int i;
							PhysicalMemoryRanges=(UINT64)mr;
							for (i=0; mr[i].NumberOfBytes.QuadPart || mr[i].BaseAddress.QuadPart; i++);

							PhysicalMemoryRangesListSize=(UINT64)(&mr[i])-(UINT64)(&mr[0]);
						}
						


					}
					__except(1)
					{
						//just in case this function decides to bug out in the future
					}
				}
				
				poutp->address=PhysicalMemoryRanges;
				poutp->size=PhysicalMemoryRangesListSize;

				ntStatus=STATUS_SUCCESS;

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
				UINT_PTR cr3reg=0;
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

				DbgPrint("cr3reg=%p\n",cr3reg);

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
			
		case IOCTL_CE_LAUNCHDBVM:
			{
				struct intput
				{
					UINT64 dbvmimgpath;	
					DWORD32 cpuid;
				} *pinp;
				pinp=Irp->AssociatedIrp.SystemBuffer;
				DbgPrint("IOCTL_CE_LAUNCHDBVM\n");

				if (pinp->cpuid == 0xffffffff)
				{
					DbgPrint("cpuid=0xffffffff\n");
					forEachCpuPassive(vmxoffload_passive, (UINT_PTR)pinp->dbvmimgpath);
				}
				else
				{					
					KAFFINITY newaffinity=(KAFFINITY)(1 << pinp->cpuid);					
					//offload just for this cpu

#if (NTDDI_VERSION >= NTDDI_VISTA)					
					KAFFINITY oldaffinity;
					oldaffinity = KeSetSystemAffinityThreadEx(newaffinity);
#else
					//XP and earlier (this routine is not called often, only when the user asks explicitly
					{
						LARGE_INTEGER delay;
						delay.QuadPart = -50; //short wait just to be sure... (the docs do not say that a switch happens imeadiatly for the no Ex version)

						KeSetSystemAffinityThread(newaffinity);
						KeDelayExecutionThread(UserMode, FALSE, &delay);
					}
#endif

					DbgPrint("cpuid=%d\n", pinp->cpuid);
		
					vmxoffload_passive((UINT_PTR)pinp->dbvmimgpath);
#if (NTDDI_VERSION >= NTDDI_VISTA)
					KeRevertToUserAffinityThreadEx(oldaffinity);
#endif
				}
				//vmxoffload_passive((UINT_PTR)pinp->dbvmimgpath);

				

				//vmxoffload((PCWSTR)pinp->dbvmimgpath);

				DbgPrint("Returned from vmxoffload()\n");
				break;
			}
			

		case IOCTL_CE_HOOKINTS: //hooks the DEBUG interrupts
			{
				DbgPrint("IOCTL_CE_HOOKINTS\n");
				forEachCpu(debugger_initHookForCurrentCPU_DPC, NULL, NULL, NULL);
				ntStatus=STATUS_SUCCESS;

				/*
				DbgPrint("IOCTL_CE_HOOKINTS for cpu %d\n", cpunr());
				if (debugger_initHookForCurrentCPU())
					ntStatus=STATUS_SUCCESS;
				else
				    ntStatus=STATUS_UNSUCCESSFUL;*/

				break;
			}

		case IOCTL_CE_USERDEFINEDINTERRUPTHOOK:
			{
				struct intput
				{
					UINT64 interruptnumber;
					UINT64 newCS;
					UINT64 newRIP;
					UINT64 addressofjumpback;
				} *pinp;
				DbgPrint("IOCTL_CE_USERDEFINEDINTERRUPTHOOK\n");

				pinp=Irp->AssociatedIrp.SystemBuffer;


				inthook_HookInterrupt((unsigned char)(pinp->interruptnumber), (int)pinp->newCS, (ULONG_PTR)pinp->newRIP, (PJUMPBACK)(pinp->addressofjumpback));
				DbgPrint("After the hook\n");
				ntStatus=STATUS_SUCCESS;
				break;
			}


		case IOCTL_CE_UNHOOKALLINTERRUPTS:
			{
				int i;
				DbgPrint("IOCTL_CE_UNHOOKALLINTERRUPTS for cpu %d\n",cpunr());
				for (i=0; i<256; i++)
					inthook_UnhookInterrupt((unsigned char)i);

				ntStatus=STATUS_SUCCESS;
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

				DbgPrint("IOCTL_CE_DEBUGPROCESS\n");			
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
					UINT64 ProcessID;
					UINT64 BaseAddress;
					UINT64 Size;
					UINT64 AllocationType;
					UINT64 Protect;
				} *inp;
				PEPROCESS selectedprocess;

				PVOID BaseAddress;
				SIZE_T RegionSize;


				inp=Irp->AssociatedIrp.SystemBuffer;
				BaseAddress=(PVOID)(UINT_PTR)inp->BaseAddress;
				RegionSize=(SIZE_T)(inp->Size);


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
							DbgPrint("Before call: BaseAddress=%p\n", BaseAddress);		
							DbgPrint("Before call: RegionSize=%x\n", RegionSize);

							ntStatus=ZwAllocateVirtualMemory((HANDLE)-1, &BaseAddress, 0, &RegionSize, (ULONG)inp->AllocationType, (ULONG)inp->Protect);

							if ((ntStatus==STATUS_SUCCESS) && (HiddenDriver))
							{
								//initialize the memory with crap so it becomes paged
								int i;
								char *x;
								x=BaseAddress;
								for (i=0; i < (int)RegionSize;i++)
									x[i]=(unsigned char)i;
							}
							
							DbgPrint("ntStatus=%x\n", ntStatus);
							DbgPrint("BaseAddress=%p\n",BaseAddress);
							DbgPrint("RegionSize=%x\n",RegionSize);
							*(PUINT64)Irp->AssociatedIrp.SystemBuffer=0;
							*(PUINT_PTR)Irp->AssociatedIrp.SystemBuffer=(UINT_PTR)BaseAddress;

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
				int size;

				inp=Irp->AssociatedIrp.SystemBuffer;
				size=inp->Size;

				address=ExAllocatePool(NonPagedPool,size);
				*(PUINT64)Irp->AssociatedIrp.SystemBuffer=0;
				*(PUINT_PTR)Irp->AssociatedIrp.SystemBuffer=(UINT_PTR)address;
				

				if (address==0)
					ntStatus=STATUS_UNSUCCESSFUL;
				else
				{
					DbgPrint("Alloc success. Cleaning memory... (size=%d)\n",size);					
					
					DbgPrint("address=%p\n", address);
					RtlZeroMemory(address, size);
				
					ntStatus=STATUS_SUCCESS;
				}

				break;
			}

		case IOCTL_CE_FREE_NONPAGED:
			{
				struct input
				{
					UINT64 Address;
				} *inp;

				inp = Irp->AssociatedIrp.SystemBuffer;

				ExFreePool((PVOID)inp->Address);

				ntStatus = STATUS_SUCCESS;

				break;
			}


		case IOCTL_CE_MAP_MEMORY:
			{
				struct input
				{
					UINT64 FromPID;
					UINT64 ToPID;
					UINT64 address;
					DWORD size;
				} *inp;

				struct output
				{
					UINT64 FromMDL;
					UINT64 Address;
				} *outp;

				KAPC_STATE apc_state;
				PEPROCESS selectedprocess;
				PMDL FromMDL=NULL;

				inp = Irp->AssociatedIrp.SystemBuffer;
				outp = Irp->AssociatedIrp.SystemBuffer;

				DbgPrint("IOCTL_CE_MAP_MEMORY\n");
				DbgPrint("address %x size %d\n", inp->address, inp->size);
				ntStatus = STATUS_UNSUCCESSFUL;				

				if (inp->FromPID)			
				{
					//switch
					DbgPrint("From PID %d\n", inp->FromPID);
					if (PsLookupProcessByProcessId((PVOID)(UINT64)(inp->FromPID), &selectedprocess) == STATUS_SUCCESS)
					{
						__try
						{
							RtlZeroMemory(&apc_state, sizeof(apc_state));
							KeStackAttachProcess((PVOID)selectedprocess, &apc_state); 

							__try
							{
								FromMDL=IoAllocateMdl((PVOID)inp->address, inp->size, FALSE, FALSE, NULL);
								if (FromMDL)
									MmProbeAndLockPages(FromMDL, KernelMode, IoReadAccess);
							}
							__finally
							{
								KeUnstackDetachProcess(&apc_state);
							}

						}
						__except (1)
						{
							DbgPrint("Exception\n");
							ntStatus = STATUS_UNSUCCESSFUL;
							break;
						}						
					}
				}
				else
				{
					DbgPrint("From kernel or self\n", inp->FromPID);
					__try
					{
						FromMDL = IoAllocateMdl((PVOID)inp->address, inp->size, FALSE, FALSE, NULL);
						if (FromMDL)
						{
							DbgPrint("IoAllocateMdl success\n");
							MmProbeAndLockPages(FromMDL, KernelMode, IoReadAccess);
						}
					}
					__except (1)
					{
						DbgPrint("Exception\n");

						if (FromMDL)
						{
							IoFreeMdl(FromMDL);
							FromMDL = NULL;
						}
					}
				}

				if (FromMDL)
				{
					DbgPrint("FromMDL is valid\n");

					if (inp->ToPID)
					{
						//switch
						DbgPrint("To PID %d\n", inp->ToPID);
						if (PsLookupProcessByProcessId((PVOID)(UINT64)(inp->ToPID), &selectedprocess) == STATUS_SUCCESS)
						{
							__try
							{
								RtlZeroMemory(&apc_state, sizeof(apc_state));
								KeStackAttachProcess((PVOID)selectedprocess, &apc_state);

								__try
								{
									outp->Address = (UINT64)MmMapLockedPagesSpecifyCache(FromMDL, UserMode, MmWriteCombined, NULL, FALSE, NormalPagePriority);
									outp->FromMDL = (UINT64)FromMDL;
									ntStatus = STATUS_SUCCESS;
								}
								__finally
								{
									KeUnstackDetachProcess(&apc_state);
								}

							}
							__except (1)
							{
								DbgPrint("Exception part 2\n");
								ntStatus = STATUS_UNSUCCESSFUL;
								break;
							}
						}
					}
					else
					{
						DbgPrint("To kernel or self\n", inp->FromPID);

						__try
						{
							outp->Address = (UINT64)MmMapLockedPagesSpecifyCache(FromMDL, UserMode, MmWriteCombined, NULL, FALSE, NormalPagePriority);
							outp->FromMDL = (UINT64)FromMDL;
							ntStatus = STATUS_SUCCESS;
						}
						__except (1)
						{
							DbgPrint("Exception part 2\n");
						}
					}



					

				}
				else
					DbgPrint("FromMDL==NULL\n");


				
				break;
			}

		case IOCTL_CE_UNMAP_MEMORY:
			{
				struct output
				{
					UINT64 FromMDL;
					UINT64 Address;
				} *inp;

				PMDL mdl;

				inp = Irp->AssociatedIrp.SystemBuffer;
				mdl = (PMDL)inp->FromMDL;

				MmUnmapLockedPages((PMDL)inp->Address, mdl);

				IoFreeMdl(mdl);

				ntStatus = STATUS_SUCCESS; //no BSOD means success ;)

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

		case IOCTL_CE_GETPROCESSNAMEADDRESS:
			{
				struct input
				{
					UINT64 PEPROCESS;
				} *inp;

				struct output
				{
					UINT64 Address;
				} *outp;

				UNICODE_STRING temp;

				inp=Irp->AssociatedIrp.SystemBuffer;
				outp=Irp->AssociatedIrp.SystemBuffer;

				RtlInitUnicodeString(&temp, L"PsGetProcessImageFileName");
				PsGetProcessImageFileName=MmGetSystemRoutineAddress(&temp);
				if (PsGetProcessImageFileName!=NULL)
				{
					outp->Address=(UINT_PTR)PsGetProcessImageFileName((PEPROCESS)((UINT_PTR)(inp->PEPROCESS)));
					ntStatus=STATUS_SUCCESS;
				}
				else 
				{
					DbgPrint("PsGetProcessImageFileName==NULL");
					ntStatus=STATUS_UNSUCCESSFUL;
				}

				
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
				__try
				{
					ntStatus=debugger_getDebuggerState((PDebugStackState)(Irp->AssociatedIrp.SystemBuffer));					
				}
				__except(1)
				{
					DbgPrint("Exception happened\n");
					ntStatus=STATUS_UNSUCCESSFUL;
				}
				
				DbgPrint("ntStatus=%x rax=%x\n",ntStatus, ((PDebugStackState)(Irp->AssociatedIrp.SystemBuffer))->rax);
				break;
			}

		case IOCTL_CE_SETDEBUGGERSTATE:
			{	
				DbgPrint("IOCTL_CE_SETDEBUGGERSTATE: state->rax=%x\n", ((PDebugStackState)(Irp->AssociatedIrp.SystemBuffer))->rax);
				__try
				{
					ntStatus=debugger_setDebuggerState((PDebugStackState)Irp->AssociatedIrp.SystemBuffer);
				}
				__except(1)
				{
					DbgPrint("Exception happened\n");
					ntStatus=STATUS_UNSUCCESSFUL;
				}
				break;
			}

		case IOCTL_CE_SETKERNELSTEPABILITY:
			{
				struct input
				{
					int state;
					
				} *inp=Irp->AssociatedIrp.SystemBuffer;
				KernelCodeStepping=inp->state;
				ntStatus=STATUS_SUCCESS;
				break;
			}

		case IOCTL_CE_WRITESIGNOREWP:
		{
			KernelWritesIgnoreWP = *(BYTE*)Irp->AssociatedIrp.SystemBuffer;
			ntStatus = STATUS_SUCCESS;
			break;
		}

		
		case IOCTL_CE_GD_SETBREAKPOINT:
			{
				struct input
				{
					BOOL active;
					int debugregspot;
					UINT64 address;
					DWORD breakType;
					DWORD breakLength;
				} *inp=Irp->AssociatedIrp.SystemBuffer;
				
				DbgPrint("sizeof(struct input)=%d\n",sizeof(struct input));
				//DbgPrint("address=%llx breakType=%d breakLength=%d\n",inp->address, inp->breakType,inp->breakLength);

				if (inp->active)
				{
					DbgPrint("activating breapoint %d\n", inp->debugregspot);
					ntStatus=debugger_setGDBreakpoint(inp->debugregspot, (UINT_PTR)inp->address, (BreakType)inp->breakType, (BreakLength)inp->breakLength);
				}
				else
				{					
					DbgPrint("Deactivating breakpoint :%d\n", inp->debugregspot);
					ntStatus=debugger_unsetGDBreakpoint(inp->debugregspot);
				}
				break;
			}

		case IOCTL_CE_TOUCHDEBUGREGISTER: //used after setting a global debug breakpoint
			{
				debugger_touchDebugRegister(0);
				ntStatus=STATUS_SUCCESS;
				break;
			}

		case IOCTL_CE_SETSTORELBR:
			{
				BOOL newstate=*(PBOOL)Irp->AssociatedIrp.SystemBuffer;

				DbgPrint("Calling debugger_setStoreLBR(%d)\n", newstate);
				debugger_setStoreLBR(newstate);
				ntStatus=STATUS_SUCCESS;
				break;
			}	


		case IOCTL_CE_EXECUTE_CODE:
			{
			

#ifndef TOBESIGNED		
				typedef NTSTATUS (*PARAMETERLESSFUNCTION)(UINT64 parameters);
				PARAMETERLESSFUNCTION functiontocall;

				struct input
				{
					UINT64	functionaddress; //function address to call
					UINT64	parameters;
				} *inp=Irp->AssociatedIrp.SystemBuffer;
				DbgPrint("IOCTL_CE_EXECUTE_CODE\n");

				functiontocall=(PARAMETERLESSFUNCTION)(inp->functionaddress);

				__try
				{
					ntStatus=functiontocall(inp->parameters);
					DbgPrint("Still alive\n");
					ntStatus=STATUS_SUCCESS;
				}
				__except(1)
				{
					DbgPrint("Exception occured\n");
					ntStatus=STATUS_UNSUCCESSFUL;
				}
#endif
				break;
			}


		case IOCTL_CE_GETVERSION:
			{
				DbgPrint("IOCTL_CE_GETVERSION. Version=%d\n",dbkversion);
				*(PULONG)Irp->AssociatedIrp.SystemBuffer=dbkversion;	
				ntStatus=STATUS_SUCCESS;
				break;
			}

		case IOCTL_CE_READMSR:
			{
				DWORD msr=*(PDWORD)Irp->AssociatedIrp.SystemBuffer;

				DbgPrint("IOCTL_CE_READMSR: msr=%x\n", msr);

				__try
				{
					*(PUINT64)Irp->AssociatedIrp.SystemBuffer=__readmsr(msr);
					DbgPrint("Output: %llx\n",*(PUINT64)Irp->AssociatedIrp.SystemBuffer); 

					ntStatus=STATUS_SUCCESS;
				}
				__except(1)
				{
					ntStatus=STATUS_UNSUCCESSFUL;
				}

				break;
			}	

		case IOCTL_CE_WRITEMSR:
			{
				struct input
				{
					UINT64 msr;			
					UINT64 value;
				} *inp=Irp->AssociatedIrp.SystemBuffer;

				DbgPrint("IOCTL_CE_WRITEMSR:\n");
				DbgPrint("msr=%x\n", inp->msr);
				DbgPrint("value=%x\n", inp->value);

				__try
				{
					__writemsr(inp->msr, inp->value );							
					ntStatus=STATUS_SUCCESS;
				}
				__except(1)
				{
					ntStatus=STATUS_UNSUCCESSFUL;
				}
				break;
			}

		case IOCTL_CE_ULTIMAP2:
			{
				struct input
				{
					UINT32 PID;	
					UINT32 Size;
					UINT32 RangeCount;
					UINT32 Reserved;
					URANGE Ranges[8];
					WCHAR OutputPath[200];
				} *inp = Irp->AssociatedIrp.SystemBuffer;
				int i;

				DbgPrint("IOCTL_CE_ULTIMAP2");
				for (i = 0; i < inp->RangeCount; i++)
					DbgPrint("%d=%p -> %p", i, (PVOID)inp->Ranges[i].StartAddress, (PVOID)inp->Ranges[i].EndAddress);

				SetupUltimap2(inp->PID, inp->Size, inp->OutputPath, inp->RangeCount, inp->Ranges);
				break;
			}

		case IOCTL_CE_ULTIMAP2_WAITFORDATA:
		{

			ULONG timeout = *(ULONG *)Irp->AssociatedIrp.SystemBuffer;
			PULTIMAP2DATAEVENT output = Irp->AssociatedIrp.SystemBuffer;
			output->Address = 0;

			ntStatus = ultimap2_waitForData(timeout, output);

			break;
		}

		case IOCTL_CE_ULTIMAP2_LOCKFILE:
		{
			int cpunr = *(int *)Irp->AssociatedIrp.SystemBuffer;
			ultimap2_LockFile(cpunr);
			break;
		}

		case IOCTL_CE_ULTIMAP2_RELEASEFILE:
		{
			int cpunr = *(int *)Irp->AssociatedIrp.SystemBuffer;
			ultimap2_ReleaseFile(cpunr);
			break;
		}


		case IOCTL_CE_ULTIMAP2_CONTINUE:
		{
			int cpunr=*(int*)Irp->AssociatedIrp.SystemBuffer;
			ntStatus = ultimap2_continue(cpunr);

			break;
		}

		case IOCTL_CE_ULTIMAP2_FLUSH:
		{			
			ntStatus = ultimap2_flushBuffers();
			break;
		}

		case IOCTL_CE_ULTIMAP2_PAUSE:
		{
			ntStatus = ultimap2_pause();
			break;
		}

		case IOCTL_CE_ULTIMAP2_RESUME:
		{
			ntStatus = ultimap2_resume();
			break;
		}

		case IOCTL_CE_DISABLEULTIMAP2:
			{
				DisableUltimap2();
				break;
			}

		case IOCTL_CE_ULTIMAP:
			{
				#pragma pack(1)
				struct input
				{
					UINT64 targetCR3;
					UINT64 dbgctl;			
					UINT64 dsareasize;
					BOOL savetofile;
					int HandlerCount;
					WCHAR filename[200];				
				} *inp=Irp->AssociatedIrp.SystemBuffer;
				#pragma pack()

				
				DbgPrint("IOCTL_CE_ULTIMAP:\n");
				DbgPrint("ultimap(%I64x, %I64x, %d):\n", (UINT64)inp->targetCR3, (UINT64)inp->dbgctl, inp->dsareasize);

				if (inp->savetofile)
					DbgPrint("filename=%S\n", &inp->filename[0]);

				ntStatus=ultimap(inp->targetCR3, inp->dbgctl, (int)inp->dsareasize, inp->savetofile, &inp->filename[0], inp->HandlerCount);
				
			

				break;
			}

		case IOCTL_CE_ULTIMAP_DISABLE:
			{
				ultimap_disable();
				ntStatus=STATUS_SUCCESS;
				break;
			}

		case IOCTL_CE_ULTIMAP_WAITFORDATA:
			{
				
				ULONG timeout=*(ULONG *)Irp->AssociatedIrp.SystemBuffer;				
				PULTIMAPDATAEVENT output=Irp->AssociatedIrp.SystemBuffer;
				ntStatus=ultimap_waitForData(timeout, output);

				break;
			}

		case IOCTL_CE_ULTIMAP_CONTINUE:
			{
				PULTIMAPDATAEVENT input=Irp->AssociatedIrp.SystemBuffer;				
				ntStatus=ultimap_continue(input);

				break;
			}

		case IOCTL_CE_ULTIMAP_FLUSH:
			{
				ultimap_flushBuffers();
				ntStatus=STATUS_SUCCESS;
				break;
			}

			/*
		case IOCTL_CE_GETCPUIDS:
			{
				CPULISTFILLSTRUCT x;	

				forEachCpuPassive(GetCPUIDS_all,&x);
			}*/

		case IOCTL_CE_STARTACCESMONITOR:
			{
				//this is used instead of writeProcessMemory for speed reasons (the reading out is still done with readProcessMemory because of easier memory management)
				struct input
				{
					UINT64 ProcessID;
				} *inp;
				PEPROCESS selectedprocess;

				PVOID BaseAddress;
				SIZE_T RegionSize;

				inp=Irp->AssociatedIrp.SystemBuffer;
				DbgPrint("IOCTL_CE_STARTACCESMONITOR(%d)\n", inp->ProcessID);


				ntStatus = STATUS_UNSUCCESSFUL;

				if (PsLookupProcessByProcessId((PVOID)(UINT64)(inp->ProcessID), &selectedprocess) == STATUS_SUCCESS)	
					ntStatus = markAllPagesAsNeverAccessed(selectedprocess);

				break;
			}

		case IOCTL_CE_ENUMACCESSEDMEMORY:
			{
				struct input
				{
					UINT64 ProcessID;
				} *inp;
				PEPROCESS selectedprocess;

				PVOID BaseAddress;
				SIZE_T RegionSize;

				inp = Irp->AssociatedIrp.SystemBuffer;
				DbgPrint("IOCTL_CE_ENUMACCESSEDMEMORY(%d)\n", inp->ProcessID);


				ntStatus = STATUS_UNSUCCESSFUL;

				if (PsLookupProcessByProcessId((PVOID)(UINT64)(inp->ProcessID), &selectedprocess) == STATUS_SUCCESS)
					*(int *)Irp->AssociatedIrp.SystemBuffer=enumAllAccessedPages(selectedprocess);

				ntStatus = STATUS_SUCCESS;
				break;
			}

		case IOCTL_CE_GETACCESSEDMEMORYLIST:
			{
				int ListSizeInBytes = *(int *)Irp->AssociatedIrp.SystemBuffer;
				PPRANGE List = (PPRANGE)Irp->AssociatedIrp.SystemBuffer;

				DbgPrint("IOCTL_CE_GETACCESSEDMEMORYLIST\n"); 

				getAccessedPageList(List, ListSizeInBytes);

				DbgPrint("return from IOCTL_CE_GETACCESSEDMEMORYLIST\n");
				ntStatus = STATUS_SUCCESS;
				break;
			}

		case IOCTL_CE_INITIALIZE:
			{
				//find the KeServiceDescriptorTableShadow 
#ifndef AMD64
				int i;
#endif
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

	
				PSERVICE_DESCRIPTOR_TABLE PossibleKeServiceDescriptorTableShow; //long name's are FUN!!!!

				DbgPrint("IOCTL_CE_INITIALIZE\n");
				pinp=Irp->AssociatedIrp.SystemBuffer;
				ntStatus=STATUS_SUCCESS;

				//no stealth for x64
				
				PossibleKeServiceDescriptorTableShow=KeServiceDescriptorTable;

				               
#ifndef AMD64
				ntStatus=STATUS_UNSUCCESSFUL; 
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
#else
				*(UINT_PTR*)Irp->AssociatedIrp.SystemBuffer=(UINT_PTR)0;
#endif

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
						DbgPrint("Still here, so vmx is loaded. vmx_version=%x\n",vmx_version);	
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
			DbgPrint("Unhandled IO request: %x\n", IoControlCode);			
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
