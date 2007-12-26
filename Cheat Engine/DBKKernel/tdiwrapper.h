/*
x=description(parameters)
x=1 bytes
parameters are defined by the types

requesting a process list is a client->server command
so you send 1 byte, a 0

sending a process list item back to the client is a bit longer:
2,processid,stringlength,chars
1 byte,4 bytes,1 byte, stringlength bytes


Server->Client:
0/1= get timer speeds.  (removed since it sets the timer speed on connect)

2=Process List item (processid:dword;stringlength:byte;processname:array of char)
3=End of process listing ()
4=Open Process Success ()
5=Open Process Failed ()
6=Record received ()
7=valuelist(Entry:word; memorysize:word; readmemory: array of bytes)

8=ValueList Done()
9=The server can use Debug Registers
10=Value Changed(status: byte)  //0=changed ok 1=incorrect value 2=unwritable
11=ReadProcessMemoryResult(successboolean: byte; actualread: word; bytesread: array of byte)
12=WriteProcessMemoryResult(successboolean: byte; actualwritten: word)
13=ScanResultCount(count: int64  (8 bytes));
14=ScanResult(stringlength:byte; result:string)
15=AddressUnfrozen(recnr:word)
16=UpdateProgressbar(max: word; position:word);
17=ScanFailed
18=Disconnect

19=Hyperscanstatus(status:byte) //0=off 1=on
20=SpeedhackStatus(status:byte)

21=Debuggerstatus(status:byte) //0=off 1=on
22=FoundCode(Address: dword;eax:dword; ebx:dword; ecx:dword; edx:dword;esi:dword;edi:dword;ebp:dword;esp:dword;eip:dword)

23=VirtualProtectExResult(status:byte; oldprotecT:dword); //status 0=failed 1=success

253=Something
// 254=Disassemblereturn(lines: byte;  array of (length:byte; string: array of bytes) )
255=Are you alive?  ()




Client->Server:
0=Give a process list ()
1=Give a window list ()
2=Open process (procid dword)
3=AddAddress(AddAddress(address:dword ,size:byte)
4=update list(start:word stop:word)  //request a updated list
5=SetConfiguration(ShowAsSigned:byte BinariesAsDecimal:byte max:word; buffersize:dword;skip_page_no_cache: byte;UseDebugRegs:byte;UseDBKQueryMemoryRegion:byte;UseDBKReadWriteMemory:byte;UseDBKOpenProcess:byte)
6=Clear record list
7=Change value of address x (recnr: word; bufsize: byte; buf: array of bytes);
8=Freeze address(recnr: word;);
9=ReadProcessMemory(address:dword; length: word);
10=WriteProcessMemory(address:dword; length: word; bytes: array of byte);

11=FirstScan(start:dword;stop:dword;scantype:byte; vartype: byte; scanvaluelength: byte; scanvalue: string; scanoptions: byte)  ;//scanoptions is a array of bits: bit0=fastscan bit1=hex bit2=readonly bit3=findonlyone bit4=bit/dec(1=bit/0=dec) bit5=unicode 
12=NextScan(scantype:byte;  scanvaluelength: byte; scanvalue: string; scanoptions: byte)
13=NewScan
14=CancelScan

15=DeleteAddress(recnr:word)
16=SetTimerSpeed(freezeinterval:word)
17=Unfreeze address(recnr:word)
18=ProcesslistitemAck()

19=SetHyperscanState(state: byte); //0=off 1=on
20=EnableSpeedhack(speed:single;sleeptime:dword);
21=DisableSpeedhack();

22=EnableDebugger();
23=Find Out What Writes To This Address(address: dword);
24=Find Out What Reads From This Address(address: dword);
25=Find Out What Accesses This Address(address: dword);
26=Stop Codefinder; 

27=VirtualProtectEx(Address: dword; dwSize:dword; NewProtect: DWORD);
28=PauseProcess;
29=ResumeProcess;

30=Password(passwordlength:byte;password:string);

253=Say something
//254=Disassemble(address: dword; nroflines: byte); 
255=I'm Alive!
*/

#define CS_PROCESSLIST  0
//#define CS_WINDOWLIST  1
#define CS_OPENPROCESS  2
#define CS_AddAddress  3 //AddAddress(address:dword ,valuetype:byte ,bitnr:byte,length:byte )
#define CS_UpdateList 4 //update list(start:word stop:word)  //request a updated list
#define CS_SetConfig 5 //set configuration (ShowAsSigned:byte BinariesAsDecimal:byte max:word; buffersize:dword;skip_page_no_cache: byte;UseDebugRegs:byte;UseDBKQueryMemoryRegion:byte;UseDBKReadWriteMemory:byte;UseDBKOpenProcess:byte)
#define CS_ClearRecordList 6 //6=Clear record list
#define CS_ChangeValueOfAddress 7//7=change the address of recid x (recid: dword; bufsize:byte; buf: arrau of byte)
#define CS_FreezeAddress 8 //Freeze Address (recid: word)
#define CS_ReadProcessMemory 9 //ReadProcessMemory(address:dword; length: word);
#define CS_WriteProcessMemory 10 //WriteProcessMemory(address:dword; length: word; bytes: array of byte);
#define CS_FirstScan 11 //start,stop:dword;Scantype:byte;vartype:byte;scanvaluelength:byte;scanbuffer:array of bytes;scanoptions:byte
#define CS_CancelScan 14 //Cancels the scan ()
#define CS_SetTimerSpeed 16 //(freezeinterval:word)

#define CS_PASSWORD		30
#define CS_KERNELDATA	31
#define CS_TERMINATESERVER 252

#define SC_PROCESSLISTITEM 2 //(processid:dword;stringlength:byte;processname:array of char)
#define SC_ENDPROCESSLIST 3 //End of process listing ()
#define SC_OPENPROCESSSUCCESS 4 //=Open Process Success ()
#define SC_OPENPROCESSFAILED 5 //=Open Process Failed ()
#define SC_AddressReceived 6 //Record received()
#define SC_ValueList 7 //valuelist(Entry:word; memorysize:word; readmemory: array of bytes)
#define SC_ValueListDone 8
#define SC_ReadProcessMemoryResult 11 //(successboolean: byte; actualread: word; bytesread: array of byte)
#define SC_WriteProcessMemoryResult 12 //(successboolean: byte; actualwritten: word)
#define SC_ScanResultCount 13 //count: int64
#define SC_ScanResult 14 //address:dword; valuesize:byte; value:array of bytes

#define SC_UpdateProgressbar 16 //(max: dword; position:dword)

HANDLE TdiHandleTransport;
PFILE_OBJECT FileObjectTransport;
HANDLE TdiHandleConnection;
PFILE_OBJECT FileObjectConnection;
HANDLE ListenThread;
BOOLEAN StopListener,connected;
KEVENT ListenerStopped;

PIRP AcceptIRP;
KEVENT AcceptIRPComplete;
PDEVICE_OBJECT pTdiDevice;
IO_STATUS_BLOCK AcceptIRPIoStatusBlock;
KEVENT TdiListenCompleteEvent;

NTSTATUS TdiFuncs_OpenTransportAddress(PHANDLE pTdiHandle, PFILE_OBJECT *pFileObject);
NTSTATUS TdiFuncs_OpenConnection(PHANDLE pTdiHandle, PFILE_OBJECT *pFileObject);
NTSTATUS TdiFuncs_AssociateTransportAndConnection(HANDLE hTransportAddress, PFILE_OBJECT pfoConnection);
NTSTATUS TdiFuncs_DisAssociateTransportAndConnection(PFILE_OBJECT pfoConnection);
NTSTATUS TdiFuncs_SetEventHandler(PFILE_OBJECT pfoTdiFileObject, LONG InEventType, PVOID InEventHandler, PVOID InEventContext);
NTSTATUS TdiFuncs_Listen(PFILE_OBJECT pfoConnection);
NTSTATUS TdiFuncs_Receive(PFILE_OBJECT pfoConnection, PVOID pBuffer, UINT uiReceiveLength, UINT *pDataReceived);
NTSTATUS TdiFuncs_Send(PFILE_OBJECT pfoConnection, PVOID pData, UINT uiSendLength, UINT *pDataSent);
NTSTATUS TdiFuncs_Disconnect(PFILE_OBJECT pfoConnection);
void InitServer(void);
BOOLEAN Listen();
BOOLEAN Disconnect();
BOOLEAN Send(PVOID Buffer,ULONG size);
BOOLEAN Receive(PVOID Buffer,ULONG size);

HANDLE SendEvent;