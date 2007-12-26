struct PluginVersion
{
  unsigned int version; //write here the minimum version this dll is compatible with (Currently only 1 is supported)
  char *pluginname;  //make this point to a 0-terminated string (allocated memory or static addressin your dll, not stack)
};

struct PLUGINTYPE0_RECORD
{
  char *interpretedaddress; //pointer to a 255 bytes long string (0 terminated)
  ULONG address;//this is a read-only representaion of the address. Change interpretedaddress if you want to change this
  BOOL ispointer; //readonly
  int countoffsets; //readonly
  ULONG *offsets; //array of dwords ranging from 0 to countoffsets-1 (readonly)
  char *description; //pointer to a 255 bytes long string
  char valuetype; //0=byte, 1=word, 2=dword, 3=float, 4=double, 5=bit, 6=int64, 7=string
  char size; //stringlength or bitlength;
};

//callback routines efinitions for registered plugin functions:
typedef BOOL (__stdcall *CEP_PLUGINTYPE0)(struct PLUGINTYPE0_RECORD *SelectedRecord);
typedef BOOL (__stdcall *CEP_PLUGINTYPE1)(ULONG *disassembleraddress, ULONG *selected_disassembler_address, ULONG *hexviewaddress);
typedef int (__stdcall *CEP_PLUGINTYPE2)(LPDEBUG_EVENT DebugEvent);
typedef void (__stdcall *CEP_PLUGINTYPE3)(ULONG processid, ULONG peprocess);
typedef void (__stdcall *CEP_PLUGINTYPE4)(int reserved);

struct PLUGINTYPE0_INIT
{
  char* name; //0 terminated string describing the name for the user's menu item
  CEP_PLUGINTYPE0 callbackroutine; //pointer to a callback routine of the type 0 plugin
};

struct PLUGINTYPE1_INIT
{
  char* name; //0 terminated string describing the name for the user's menu item
  CEP_PLUGINTYPE1 callbackroutine; //pointer to a callback routine of the type 1 plugin
};

struct PLUGINTYPE2_INIT
{
  CEP_PLUGINTYPE2 callbackroutine; //pointer to a callback routine of the type 2 plugin
};

struct PLUGINTYPE3_INIT
{
  CEP_PLUGINTYPE3 callbackroutine; //pointer to a callback routine of the type 3 plugin
};

struct PLUGINTYPE4_INIT
{
  CEP_PLUGINTYPE4 callbackroutine; //pointer to a callback routine of the type 3 plugin
};

#pragma pack(push)
#pragma pack(1)
struct REGISTERMODIFICATIONINFO
{
  ULONG address; //addres to break on
  char change_eax;
  char change_ebx;
  char change_ecx;
  char change_edx;
  char change_esi;
  char change_edi;
  char change_ebp;
  char change_esp;
  char change_eip;
  char change_cf;
  char change_pf;
  char change_af;
  char change_zf;
  char change_sf;
  char change_of;
  char fill;
  ULONG new_eax;
  ULONG new_ebx;
  ULONG new_ecx;
  ULONG new_edx;
  ULONG new_esi;
  ULONG new_edi;
  ULONG new_ebp;
  ULONG new_esp;
  ULONG new_eip;
  char new_cf;
  char new_pf;
  char new_af;
  char new_zf;
  char new_sf;
  char new_of;
};
#pragma pack(pop)


//the __stdcall stuff isn't really needed since I've set compiler options to force stdcall, but this makes it clear that stdcall is used to the reader
typedef void (__stdcall *CEP_SHOWMESSAGE)(char* message);
typedef int (__stdcall *CEP_REGISTERFUNCTION) (int pluginid, int functiontype, PVOID init);
typedef BOOL (__stdcall *CEP_UNREGISTERFUNCTION) (int pluginid, int functionid);
typedef HANDLE (__stdcall *CEP_GETMAINWINDOWHANDLE) (void);
typedef BOOL (__stdcall *CEP_AUTOASSEMBLE) (char *script);
typedef BOOL (__stdcall *CEP_ASSEMBLER) (ULONG address, char* instruction, BYTE *output, int maxlength, int *returnedsize);
typedef BOOL (__stdcall *CEP_DISASSEMBLER) (ULONG address, char* output, int maxsize);
typedef BOOL (__stdcall *CEP_CHANGEREGATADDRESS) (ULONG address,struct REGISTERMODIFICATIONINFO *changereg);
typedef BOOL (__stdcall *CEP_INJECTDLL) (char *dllname, char *functiontocall);
typedef int (__stdcall *CEP_FREEZEMEM) (ULONG address, int size);
typedef BOOL (__stdcall *CEP_UNFREEZEMEM) (int freezeID);
typedef BOOL (__stdcall *CEP_FIXMEM) (void);
typedef BOOL (__stdcall *CEP_PROCESSLIST) (char *listbuffer, int listsize);
typedef BOOL (__stdcall *CEP_RELOADSETTINGS) (void);
typedef DWORD (__stdcall *CEP_GETADDRESSFROMPOINTER) (ULONG baseaddress, int offsetcount, int* offsets);


struct ExportedFunctions
{
  int sizeofExportedFunctions;
  CEP_SHOWMESSAGE ShowMessage; //Pointer to the ce showmessage function
  CEP_REGISTERFUNCTION RegisterFunction; //Use this to register a specific type of plugin
  CEP_UNREGISTERFUNCTION UnregisterFunction; //unregisters a function registered with registerfunction
  PULONG OpenedProcessID; //pointer to the currently selected processid
  PHANDLE OpenedProcessHandle; //pointer to the currently selected processhandle

  CEP_GETMAINWINDOWHANDLE GetMainWindowHandle; //returns the handle of the main window (for whatever reason, it is recommended to use delphi to make a real userinterface upgrade)
  CEP_AUTOASSEMBLE AutoAssemble; //Pointer to the AutoAssemble function
  CEP_ASSEMBLER Assembler; //pointer to the assembler function
  CEP_DISASSEMBLER Disassembler; //pointer to the disassembler function
  CEP_CHANGEREGATADDRESS ChangeRegistersAtAddress; //pointer to the ChangeRegAtBP function
  CEP_INJECTDLL InjectDLL; //pointer to ce's Inject DLL function
  CEP_FREEZEMEM FreezeMem; //pointer to the FreezeMem routine
  CEP_UNFREEZEMEM UnfreezeMem; //pointer to the UnfreezeMem routine (use this to undo freezes with FreezeMem)
  CEP_FIXMEM FixMem; //pointer to the fixmem routine
  CEP_PROCESSLIST ProcessList; //pointer to the processlist routine
  CEP_RELOADSETTINGS ReloadSettings; //pointer to the ReloadSettings routine
  CEP_GETADDRESSFROMPOINTER GetAddressFromPointer; //pointer to the GetAddressFromPointer routine

  //pointers to the address that contains the pointers to the functions
  PVOID ReadProcessMemory;			//pointer to the pointer of ReadProcessMemory (Change it to hook that api, or use it yourself)
  PVOID WriteProcessMemory;			//pointer to the pointer of WriteProcessMemory (Change it to hook that api, or use it yourself)
  PVOID GetThreadContext;			//   ...
  PVOID SetThreadContext;			//   ...
  PVOID SuspendThread;				//   ...
  PVOID ResumeThread;				//   ...
  PVOID OpenProcess;				//   ...
  PVOID WaitForDebugEvent;			//   ...
  PVOID ContinueDebugEvent;			//   ...
  PVOID DebugActiveProcess;			//   ...
  PVOID StopDebugging;				//   ...
  PVOID StopRegisterChange;			//   ...
  PVOID VirtualProtect;				//   ...
  PVOID VirtualProtectEx;			//   ...
  PVOID VirtualQueryEx;				//   ...
  PVOID VirtualAllocEx;				//   ...
  PVOID CreateRemoteThread;			//   ...
  PVOID OpenThread;					//   ...
  PVOID GetPEProcess;				//   ...
  PVOID GetPEThread;				//   ...
  PVOID GetThreadsProcessOffset;	//   ...
  PVOID GetThreadListEntryOffset;	//   ...
  PVOID GetProcessnameOffset;		//   ...
  PVOID GetDebugportOffset;			//   ...
  PVOID GetPhysicalAddress;			//   ...
  PVOID ProtectMe;					//   ...
  PVOID GetCR4;						//   ...
  PVOID GetCR3;						//   ...
  PVOID SetCR3;						//   ...
  PVOID GetSDT;						//   ...
  PVOID GetSDTShadow;				//   ...
  PVOID setAlternateDebugMethod;	//   ...
  PVOID getAlternateDebugMethod;	//   ...
  PVOID DebugProcess;				//   ...
  PVOID ChangeRegOnBP;				//   ...
  PVOID RetrieveDebugData;			//   ...
  PVOID StartProcessWatch;			//   ...
  PVOID WaitForProcessListData;		//   ...
  PVOID GetProcessNameFromID;		//   ...
  PVOID GetProcessNameFromPEProcess;//   ...
  PVOID KernelOpenProcess;			//   ...
  PVOID KernelReadProcessMemory;	//   ...
  PVOID KernelWriteProcessMemory;	//   ...
  PVOID KernelVirtualAllocEx;		//   ...
  PVOID IsValidHandle;				//   ...
  PVOID GetIDTCurrentThread;		//   ...
  PVOID GetIDTs;					//   ...
  PVOID MakeWritable;				//   ...
  PVOID GetLoadedState;				//   ...
  PVOID DBKSuspendThread;			//   ...
  PVOID DBKResumeThread;			//   ...
  PVOID DBKSuspendProcess;			//   ...
  PVOID DBKResumeProcess;			//   ...
  PVOID KernelAlloc;				//   ...
  PVOID GetKProcAddress;			//   ...
  PVOID CreateToolhelp32Snapshot;	//   ...
  PVOID Process32First;          	//   ...
  PVOID Process32Next;           	//   ...
  PVOID Thread32First;           	//   ...
  PVOID Thread32Next;            	//   ...
  PVOID Module32First;           	//   ...
  PVOID Module32Next;            	//   ...
  PVOID Heap32ListFirst;         	//   ...
  PVOID Heap32ListNext;          	//   ...


  //advanced for delphi 7 enterprise dll programmers only
  PVOID mainform; //pointer to the Tmainform object. (main window) Compatible with TFORM of D7 Enterprise
  PVOID memorybrowser; //pointer to the TMemoryBrowser object (memory view windows), same as mainform
};

struct ExportedFunctions Exported;

BOOL __stdcall GetVersion(struct PluginVersion *pv , int sizeofpluginversion);
BOOL __stdcall InitializePlugin(struct ExportedFunctions *ef , int pluginid);
BOOL __stdcall DisablePlugin(void);
