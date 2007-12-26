#ifndef CETC
ALLERT! DO NOT PUT THIS FILE IN THE STANDARD CE DISTRIBUTION
#endif

#include "ntifs.h"
#include "extraimports.h"
#include "memscan.h"
#include "tdiwrapper.h"
#include <windef.h>
#include "rootkit.h"

VOID CETC_CORE(IN PVOID StartContext)
{
	KLOCK_QUEUE_HANDLE lqh;
	PEPROCESS ActivePEPROCESS=NULL;

	InitServer();


	if (FileObjectConnection!=NULL)
	{
		StopListener=FALSE;

		while (!StopListener)
		{
			AddressListEntries=0;

			DbgPrint("Start listening\n");
			if (Listen())
			{
				ULONG DataSent=0;
				BOOLEAN PasswordCorrect=FALSE;
				int i;
				unsigned char command;
				char a;				
				char *buffer;
				char defaultpass[9]="defaultpw";
				DbgPrint("Listen=success\n");

				connected=TRUE;

				DbgPrint("Waiting for password\n");				

				while ((!PasswordCorrect) && (connected))
				{
					if (Receive(&command,1))
					{		


						if (command==CS_PASSWORD) //only one possible so use a if
						{							
							if (Receive(&a,1)) //length
							{
								if (a!=0)
								{
									buffer=ExAllocatePoolWithTag(NonPagedPool,(ULONG)a,0);
									
									DbgPrint("received passsize=%d\n",a);
									//allocate a buffer big enough to receive the password
									if (Receive(buffer,a))
									{
										if (a<=9)
										{
		                                    for (i=0; i<a; i++)
												if (buffer[i]!=defaultpass[i])
													break;
		
											PasswordCorrect=(i==9);
										}								
		
										if (PasswordCorrect)
											DbgPrint("Password correct\n");
										else
											DbgPrint("Password incorrect\n");
									}							
		
									ExFreePool(buffer);
								}						
							}
						}
					}
				}

				if (!connected)
					continue;

						
				DataSent=0;

				while (connected)
				{
					DbgPrint("Going to read a new command\n");
					if (Receive(&command,1))
					{
						DbgPrint("command=%d\n",command);
						switch (command)
						{

							case CS_SetTimerSpeed: //(freezeinterval:word)
							{
								WORD fi;
								DbgPrint("Set Timer Speed\n");
                                Receive(&fi,2);

								FreezeInterval.QuadPart=fi*-10000;

								//-50000000=5 seconds=5000 ms
								//-5000000=500 ms
								break;
							}

							case CS_KERNELDATA:
							{
								DbgPrint("Kerneldata received\n");
								Receive(&ActiveLinkOffset,4);
								Receive(&ProcessNameOffset,4);
								Receive(&DebugportOffset,4);
								Receive(&PIDOffset,4);
								break;
							}

							case CS_SetConfig:
							{
								//UseDebugRegs:byte;UseDBKQueryMemoryRegion:byte;UseDBKReadWriteMemory:byte;UseDBKOpenProcess:byte)
								DbgPrint("SetConfig\n");
								
								Receive(&MemscanOptions.ShowAsSigned,1);
								Receive(&MemscanOptions.BinariesAsDecimal,1);
								Receive(&MemscanOptions.max,2);
								Receive(&MemscanOptions.buffersize,4);
								Receive(&MemscanOptions.skip_page_no_cache,1);
								Receive(&MemscanOptions.UseDebugRegs,1);
								Receive(&MemscanOptions.UseDBKQueryMemoryRegion,1);
								Receive(&MemscanOptions.UseDBKReadWriteMemory,1);
								Receive(&MemscanOptions.UseDBKOpenProcess,1);
								break;

							}



							case CS_PROCESSLIST:
							{
								BOOLEAN First;
								PEPROCESS pf,p;
								KAPC_STATE apc_state;
								int pos=0;
	
								char *processname;
								char EndList[1]={SC_ENDPROCESSLIST};	
								char* TempBuffer=ExAllocatePoolWithTag(NonPagedPool,512,0);

								__try
								{
								DbgPrint("ProcessList\n");
								
								RtlZeroMemory(&apc_state,sizeof(apc_state));


								pf=PsGetCurrentProcess();
								p=pf;


								First=TRUE;
								while (((p!=NULL) && (p!=pf)) || (First))
								{									
									
									
								
									First=FALSE;
									processname=(PVOID)p;
									processname=&processname[ProcessNameOffset];
									DbgPrint("peprocess=%p processname=%s\n",p,processname);
									
									//get size of processname (less than 32 bytes)
									for (i=0; (i<32)&&(processname[i]!=0) ; i++) ;
								
									//(processid:dword;stringlength:byte;processname:array of char)
									if ((pos+6+i)<512)
									{
										//add it to the buffer
										TempBuffer[pos]=SC_PROCESSLISTITEM;
										pos++;

										*(PULONG)(&TempBuffer[pos])=(ULONG)p;
										pos+=4;

										TempBuffer[pos]=(UCHAR)i;
										pos++;

										RtlCopyMemory(&TempBuffer[pos],processname,i);
										pos+=i;
									}
									else
									{										
										//buffer is too full, so send it, and start filling again
										Send(TempBuffer,pos);	
										pos=0;
									}
								
									
									p=(PEPROCESS)(*(PULONG)((ULONG)p+ActiveLinkOffset)-ActiveLinkOffset);
									
								}

								if (pos>0)
									Send(TempBuffer,pos);
								
								ExFreePool(TempBuffer);
	
								
								Send(EndList,1);
								}
								__except(1)
								{
									DbgPrint("Error in processlist\n");

								}
								

								break;
								
							}

							case CS_OPENPROCESS:
							{
								unsigned char openedprocess=SC_OPENPROCESSSUCCESS;
								

								DbgPrint("Open Process\n");
								

							    if (Receive(&ActivePEPROCESS,4))
                                    DbgPrint("ActivePEPROCESS=%p\n",ActivePEPROCESS);
								else
									DbgPrint("OpenProcess failed");

								Send(&openedprocess,1);
								break;
							}


							case CS_CancelScan:
							{
                                CurrentScan.scanning=FALSE;
								addressfile=0;
								valuefile=0;

							}

							case CS_FirstScan:
							{
								//start,stop:dword;vartype:byte;Scantype:byte;scanvaluelength:byte;scanvalue:array of bytes;scanoptions:byte
								DWORD start;
								DWORD stop;								
								BYTE VarType;
								BYTE Scantype;
								BYTE ScanvalueSize;
								char *scanvalue;
								BYTE ScanOptions;

								Receive(&start,4);
								Receive(&stop,4);
								Receive(&VarType,1);
								Receive(&Scantype,1);								
								Receive(&ScanvalueSize,1);

								scanvalue=ExAllocatePoolWithTag(NonPagedPool,ScanvalueSize,0);
								if (scanvalue!=NULL)
								{
									__try
									{
										Receive(scanvalue,ScanvalueSize);
										Receive(&ScanOptions,1);

										//all parameters received, start the scan....
										DbgPrint("start=%x  stop=%x  vartype=%d  scantype=%d  scanvaluesize=%d  scanoptions=%d\n",start,stop,VarType,Scantype,ScanvalueSize);
										FirstScan(ActivePEPROCESS,start,stop,VarType,Scantype,ScanvalueSize,scanvalue,ScanOptions);	
									}
									__finally
									{	
										ExFreePool(scanvalue);
									}
								}

                                break;								
							}

							case CS_ReadProcessMemory:
							{
								//ReadProcessMemory(address:dword; length: word);
								
								BOOLEAN ok=FALSE;
								ULONG address;
								WORD size;
								WORD bytesread=0;
								char* outputbuffer;								

								DbgPrint("ReadProcessMemory\n");
								Receive(&address,4);
								Receive(&size,2);

								DbgPrint("address=%x size=%d\n",address,size);
                                //output SC_ReadProcessMemoryResult(successboolean: byte; actualread: word; bytesread: array of byte)
								//allocate memory to hold the data								
								outputbuffer=ExAllocatePoolWithTag(NonPagedPool,(ULONG)size+4,0);
								if (outputbuffer!=NULL)
								{
									DbgPrint("Allocated memory\n");
									
									DbgPrint("ActivePEPROCESS=%p\n",ActivePEPROCESS);
									ok=ReadProcessMemory(0,ActivePEPROCESS,(PVOID)address,size,(PVOID)&outputbuffer[4]);
									if (ok) 
										DbgPrint("ReadProcessMemory successful\n");
									else
										DbgPrint("ReadProcessMemory failed\n");

									bytesread=ok ? (WORD)size : 0;
									outputbuffer[0]=SC_ReadProcessMemoryResult;
									outputbuffer[1]=ok ? 1:0;
									*(PWORD)(&outputbuffer[2])=bytesread;

									Send(outputbuffer,bytesread+4);

									ExFreePool(outputbuffer);
								}								
								break;

							}

							case CS_WriteProcessMemory:
							{
								//WriteProcessMemory(address:dword; length: word; bytes: array of byte);
								BOOLEAN ok=FALSE;
								ULONG address;
								WORD size;
								WORD bytesread=0;
								char* inputbuffer;								

								DbgPrint("WriteProcessMemory\n");
								Receive(&address,4);
								Receive(&size,2);

								DbgPrint("address=%x size=%d\n",address,size);

								inputbuffer=ExAllocatePoolWithTag(NonPagedPool, (size<=4) ? 4 : (ULONG)size,0);
								if (inputbuffer!=NULL)
								{
									Receive(inputbuffer,size);
									ok=WriteProcessMemory(0,ActivePEPROCESS,(PVOID)address,size,inputbuffer);
									if (ok) 
										DbgPrint("WriteProcessMemory successful\n");
									else
										DbgPrint("WriteProcessMemory failed\n");

									inputbuffer[0]=SC_WriteProcessMemoryResult;
									inputbuffer[1]=ok ? 1:0;
									*(PWORD)(&inputbuffer[2])=ok ? (WORD)size: 0;

									Send(inputbuffer,4);

									ExFreePool(inputbuffer);
								}

								

								break;
							}

							case CS_ClearRecordList:
							{
								AddressListEntries=0;
								break;
							}

							case CS_UpdateList: //update list(start:word stop:word)  //request a updated list
							{
								WORD start,stop;
								WORD i,j;
								DWORD buffersize=1,pos;
								char *buffer;
								ADDRESSENTRY *templist;

								DbgPrint("UpdateList\n");
                                Receive(&start,2);
								Receive(&stop,2);

								DbgPrint("start=%d\nstop=%d\n",start,stop);

								//read the addresses and send the data to the client


								start=start>AddressListEntries-1 ? 0:start;
								stop=stop>AddressListEntries-1 ? AddressListEntries:stop;
								DbgPrint("After adjusting:\nstart=%d\nstop=%d\n",start,stop);


								templist=ExAllocatePoolWithTag(NonPagedPool, (start-stop+1)*sizeof(ADDRESSENTRY),0);
								if (templist!=NULL)
								{

									j=0;
									for (i=start;i<stop;i++)
									{
										buffersize+=AddressList[i].size+4;
										templist[j]=AddressList[i];
										j++;
									}

								}


								if (templist==NULL)
									break;


								DbgPrint("Total buffer to read=%d\n",buffersize);
								
								buffer=ExAllocatePoolWithTag(NonPagedPool, buffersize+1,0);
								if (buffer!=NULL)
								{
									DbgPrint("Allocated buffer\n");
									buffer[0]=SC_ValueList;
									pos=1;
									for (i=start;i<stop;i++)
									{
										DbgPrint("pos=%d\n",pos);
										DbgPrint("i=%d\n",i);
										DbgPrint("Address=%x\nSize=%x\n",AddressList[i].Address,AddressList[i].size);

										*(PWORD)&buffer[pos]=i;
										
										if (ReadProcessMemory(0,ActivePEPROCESS,(PVOID)AddressList[i].Address,(DWORD)AddressList[i].size,&buffer[pos+4]))
										{
											DbgPrint("Readable\n");
											*(PWORD)&buffer[pos+2]=AddressList[i].size;

											DbgPrint("Bufferdata:\n");
											DbgPrint("Itemnr=%d\n",*(PWORD)&buffer[pos]);
											DbgPrint("read=%d bytes\n",*(PWORD)&buffer[pos+2]);
											pos+=4+AddressList[i].size;
										}
										else
										{
											DbgPrint("Unreadable\n");
											*(PWORD)&buffer[pos+2]=0;
											pos+=4;
										}

										//pos+=AddressList[i].size;
									}

									DbgPrint("Sending data to client\n");
									buffer[pos]=SC_ValueListDone; //also tell it's the end of the list (safety first....)
									Send(buffer,pos+1);
									ExFreePool(buffer);
								}
								else DbgPrint("Failed to allocate buffer\n");

								DbgPrint("End of UpdateList\n");
								break;
							}

							case CS_FreezeAddress:
							{						
								//Freeze Address (recid: word)
								WORD recid;
								Receive(&recid,2);

								if (recid<AddressListEntries)
								{									
									if (AddressList[i].frozen)
									{
										//cleanup
										KeAcquireInStackQueuedSpinLock(&AddressListSpinlock,&lqh);
										AddressList[i].frozen=FALSE;
										KeReleaseInStackQueuedSpinLock(&lqh);
										
										//freemem
										if (AddressList[i].frozendata!=NULL)
											ExFreePool(AddressList[i].frozendata);
									}								

									AddressList[i].frozendata=ExAllocatePoolWithTag(NonPagedPool, AddressList[i].size,0);									
									AddressList[i].frozen=TRUE; //no spinlock required here
								}

								break;
							}

							case CS_AddAddress:	//AddAddress(address:dword ,size:byte)
							{
								ADDRESSENTRY x;	
								ADDRESSENTRY *oldlist=NULL;	
								BYTE recordreceived=SC_AddressReceived;

								Receive(&(x.Address),4);
								Receive(&(x.size),1);
								x.frozen=FALSE;
								x.frozendata=NULL;
								
								DbgPrint("Address=%x  -  size=%d\n",x.Address,x.size);

								KeAcquireInStackQueuedSpinLock(&AddressListSpinlock,&lqh);
								//add to the list
								//first check if there is already a list or not
								if (AddressList==NULL)
								{									
									DbgPrint("Allocating Addresslist buffer\n");
									AddressListEntries=0;
									AddressListSize=0;

									//allocate a buffer to store the list
									AddressList=ExAllocatePoolWithTag(NonPagedPool, PAGE_SIZE,0);
									if (AddressList!=NULL)
									{
										DbgPrint("Allocated a AddressList. Setting AddressListSize to PAGE_SIZE(%d)\n",PAGE_SIZE);
										AddressListSize=PAGE_SIZE;
										DbgPrint("AddressListSize=%d\n",AddressListSize);
									}
									else
									{
										DbgPrint("couldn\'t allocate buffer");										
										break;
									}									
								} else DbgPrint("AddressList was already allocated. AddressListSize=%d\n",AddressListSize); 

								if (AddressListSize<(AddressListEntries+1)*sizeof(ADDRESSENTRY))
								{
									DbgPrint("AddressListSize too small, allocating more memory\n");
									//allocate a new buffer
									oldlist=AddressList;
									AddressList=ExAllocatePoolWithTag(NonPagedPool,AddressListSize+PAGE_SIZE,0);
									if (AddressList!=NULL)
									{
										DbgPrint("Copying old list to new list\n");
										RtlCopyMemory(AddressList,oldlist,AddressListSize);
										AddressListSize+=PAGE_SIZE;
										ExFreePool(oldlist);

										DbgPrint("Allocated more memory and copied the list\n");
									}
									else
									{
										DbgPrint("Couldn\'t reallocate list\n");
										AddressList=oldlist;
										break;
									}

								}	

								DbgPrint("Going to add entry\n");
								AddressList[AddressListEntries]=x;
								AddressListEntries++;
								KeReleaseInStackQueuedSpinLock(&lqh);

								DbgPrint("Entry added\n");
								DbgPrint("AddressListSize=%d\nAddressListEntries=%d\n",AddressListSize,AddressListEntries);


								Send(&recordreceived,1);
								break;
							}

							case CS_ChangeValueOfAddress:
							{
								//change the address of recid x (recid: dword; bufsize:byte; buf: arrau of byte)
								WORD recid;
								BYTE bufsize;
								char *buf;

								Receive(&recid,2);
								Receive(&bufsize,1);
								
								if (bufsize!=0)
								{
									buf=(NonPagedPool,bufsize,0);
									Receive(buf,bufsize);
									if (AddressList[recid].frozen)
									{
										RtlCopyMemory(AddressList[recid].frozendata,buf,bufsize);
									}

									WriteProcessMemory(0,ActivePEPROCESS,(PVOID)AddressList[recid].Address,bufsize,buf);

									ExFreePool(buf);
								}
								
								break;
							}

							

							case CS_TERMINATESERVER:
							{
								DbgPrint("Terminating server\n");
								Disconnect();
								StopListener=TRUE;
								break;
							}
	
						}

						


					}

				}

				
			}
			else 
			{
				LARGE_INTEGER timeout;
				KEVENT WaitEvent;
				//wait a few seconds and try again
				KeInitializeEvent(&WaitEvent,  NotificationEvent, FALSE);

				DbgPrint("waiting\n");
				timeout.QuadPart=-50000000; //5 seconds
				KeWaitForSingleObject(&WaitEvent, Executive, KernelMode, FALSE, &timeout);
				DbgPrint("returned\n");

				DbgPrint("Error while listening\n");

				Disconnect();
				//break; //error while listening
			}
		}
	}

	StopListener=TRUE; //in case i got out using a break (e.g bug...)
	//free the AddressList
	if (AddressList!=NULL)
		ExFreePool(AddressList);
	
	DbgPrint("Exit thread\n");
	PsTerminateSystemThread(STATUS_SUCCESS);
    return;
}


void InitializeCETC(void)
{
	OBJECT_ATTRIBUTES oaCreateThread;
	HANDLE th;

    AddressList=NULL;
	AddressListSize=0;
	AddressListEntries=0;


	ProcessNameOffset=372;
	ActiveLinkOffset=136;
	DebugportOffset=188;
	
	StopListener=FALSE;
	CurrentScan.process=NULL; //indicates there is no scan
	CurrentScan.scanning=FALSE;
	CurrentScan.scanvalue=NULL;

	KeInitializeEvent(&ListenerStopped,  NotificationEvent, FALSE);
	KeInitializeSpinLock(&AddressListSpinlock);		

	{
		OBJECT_ATTRIBUTES oaSendEvent;

		InitializeObjectAttributes(&oaSendEvent, NULL, OBJ_KERNEL_HANDLE, NULL, NULL);			
		if NT_SUCCESS(ZwCreateEvent(&SendEvent,0,&oaSendEvent,SynchronizationEvent,TRUE))
			DbgPrint("Created the event\n");
		else
			DbgPrint("Failed to create the event\n");
			
	}

	addressfile=0;
	valuefile=0;


	DbgPrint("Spawning thread\n");
	th=0;
	InitializeObjectAttributes(&oaCreateThread, NULL, OBJ_KERNEL_HANDLE, NULL, NULL);
	PsCreateSystemThread(&th,0L,&oaCreateThread,NULL,NULL,CETC_CORE,NULL);

	if (th!=NULL)
		ZwClose(th);
	
}

NTSTATUS UnloadCETC(void)
{
	NTSTATUS ntStatus;
	DbgPrint("Unload CE-TC\n");
	StopListener=TRUE;
	if (FileObjectConnection!=NULL)
	{	
        ntStatus=TdiFuncs_DisAssociateTransportAndConnection(FileObjectConnection);
		if (NT_SUCCESS(ntStatus))
			DbgPrint("Disassociating worked\n");
		else
			DbgPrint("Disassociating Failed!\n");
	}


	if (FileObjectConnection!=NULL)
	{
		DbgPrint("Dereferencing Connection Object\n");
		ObDereferenceObject(FileObjectConnection);
	}

	if (TdiHandleConnection!=NULL)
	{
		DbgPrint("Dereferencing Connection Handle\n");
		ZwClose(TdiHandleConnection);
	}

	if (FileObjectTransport!=NULL)
	{
		DbgPrint("Dereferencing Transport Object\n");
		ObDereferenceObject(FileObjectTransport);
	}
	
	if (TdiHandleTransport!=NULL)
	{
		DbgPrint("Dereferencing Transport handle\n");
		ZwClose(TdiHandleTransport);
	}


	ntStatus=ZwWaitForSingleObject(ListenThread,FALSE,NULL);

	if (NT_SUCCESS(ntStatus))
		DbgPrint("Failed to wait for the thread\n");

	return ntStatus;
}