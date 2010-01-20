/*
 cepluginsdk.h
 Updated January 9, 2010

 v3.0.1
*/
#include <windows.h>
#define CESDK_VERSION 3

typedef enum {ptAddressList=0, ptMemoryView=1, ptOnDebugEvent=2, ptProcesswatcherEvent=3, ptFunctionPointerchange=4, ptMainMenu=5, ptDisassemblerContext=6, ptDisassemblerRenderLine=7, ptAutoAssembler=8} PluginType;
typedef enum {aaInitialize=0, aaPhase1=1, aaPhase2=2, aaFinalize=3} AutoAssemblerPhase;

typedef struct _PluginVersion
{
  unsigned int version; //write here the minimum version this dll is compatible with (Current supported version: 1 and 2: this SDK only describes 2)
  char *pluginname;  //make this point to a 0-terminated string (allocated memory or static addressin your dll, not stack)
} PluginVersion, *PPluginVersion;

typedef struct _PLUGINTYPE0_RECORD
{
  char *interpretedaddress; //pointer to a 255 bytes long string (0 terminated)
  ULONG address;//this is a read-only representaion of the address. Change interpretedaddress if you want to change this
  BOOL ispointer; //readonly
  int countoffsets; //readonly
  ULONG *offsets; //array of dwords ranging from 0 to countoffsets-1 (readonly)
  char *description; //pointer to a 255 bytes long string
  char valuetype; //0=byte, 1=word, 2=dword, 3=float, 4=double, 5=bit, 6=int64, 7=string
  char size; //stringlength or bitlength;
} PLUGINTYPE0_RECORD, ADDRESSLIST_RECORD, *PPLUGINTYPE0_RECORD, *PADDRESSLIST_RECORD;

//callback routines efinitions for registered plugin functions:
typedef BOOL (__stdcall *CEP_PLUGINTYPE0)(PPLUGINTYPE0_RECORD SelectedRecord);
typedef BOOL (__stdcall *CEP_PLUGINTYPE1)(ULONG *disassembleraddress, ULONG *selected_disassembler_address, ULONG *hexviewaddress);
typedef int (__stdcall *CEP_PLUGINTYPE2)(LPDEBUG_EVENT DebugEvent);
typedef void (__stdcall *CEP_PLUGINTYPE3)(ULONG processid, ULONG peprocess, BOOL Created);
typedef void (__stdcall *CEP_PLUGINTYPE4)(int reserved);
typedef void (__stdcall *CEP_PLUGINTYPE5)(void);
typedef BOOL (__stdcall *CEP_PLUGINTYPE6ONPOPUP)(ULONG selectedAddress, char **addressofname);
typedef BOOL (__stdcall *CEP_PLUGINTYPE6)(ULONG *selectedAddress);
typedef void (__stdcall *CEP_PLUGINTYPE7)(ULONG address, char **addressStringPointer, char **bytestringpointer, char **opcodestringpointer, char **specialstringpointer, ULONG *textcolor);
typedef void (__stdcall *CEP_PLUGINTYPE8)(char **line, AutoAssemblerPhase phase);



typedef struct _PLUGINTYPE0_INIT
{
  char* name; //0 terminated string describing the name for the user's menu item
  CEP_PLUGINTYPE0 callbackroutine; //pointer to a callback routine of the type 0 plugin  
} PLUGINTYPE0_INIT, ADDRESSLISTPLUGIN_INIT, *PPLUGINTYPE0_INIT, *PADDRESSLISTPLUGIN_INIT;

typedef struct _PLUGINTYPE1_INIT
{
  char* name; //0 terminated string describing the name for the user's menu item
  CEP_PLUGINTYPE1 callbackroutine; //pointer to a callback routine of the type 1 plugin
  char* shortcut; //0 terminated string containing the shortcut in textform. CE will try it's best to parse it to a valid shortcut
} PLUGINTYPE1_INIT, MEMORYVIEWPLUGIN_INIT, *PPLUGINTYPE1_INIT, *PMEMORYVIEWPLUGIN_INIT;

typedef struct _PLUGINTYPE2_INIT
{
  CEP_PLUGINTYPE2 callbackroutine; //pointer to a callback routine of the type 2 plugin
} PLUGINTYPE2_INIT, DEBUGEVENTPLUGIN_INIT, *PPLUGINTYPE2_INIT, *PDEBUGEVENTPLUGIN_INIT;

typedef struct _PLUGINTYPE3_INIT
{
  CEP_PLUGINTYPE3 callbackroutine; //pointer to a callback routine of the type 3 plugin
} PLUGINTYPE3_INIT, PROCESSWATCHERPLUGIN_INIT, *PPLUGINTYPE3_INIT, *PPROCESSWATCHERPLUGIN_INIT;

typedef struct _PLUGINTYPE4_INIT
{
  CEP_PLUGINTYPE4 callbackroutine; //pointer to a callback routine of the type 4 plugin
} PLUGINTYPE4_INIT, POINTERREASSIGNMENTPLUGIN_INIT, *PPLUGINTYPE4_INIT, *PPOINTERREASSIGNMENTPLUGIN_INIT; 

typedef struct _PLUGINTYPE5_INIT
{
  char* name; //0 terminated string describing the name for the user's menu item
  CEP_PLUGINTYPE5 callbackroutine; 
  char* shortcut; //0 terminated string containing the shortcut in textform. CE will try it's best to parse it to a valid shortcut
} PLUGINTYPE5_INIT, MAINMENUPLUGIN_INIT, *PPLUGINTYPE5_INIT, *PMAINMENUPLUGIN_INIT;

typedef struct _PLUGINTYPE6_INIT
{
  char* name; //0 terminated string describing the name for the user's menu item
  CEP_PLUGINTYPE6ONPOPUP callbackroutineOnPopup; 
  CEP_PLUGINTYPE6 callbackroutine; 
  char* shortcut; //0 terminated string containing the shortcut in textform. CE will try it's best to parse it to a valid shortcut
} PLUGINTYPE6_INIT, DISASSEMBLERCONTEXT_INIT, *PPLUGINTYPE6_INIT, *PDISASSEMBLERCONTEXT_INIT;

typedef struct _PLUGINTYPE7_INIT
{
  CEP_PLUGINTYPE7 callbackroutine; //pointer to a callback routine of the type 7 plugin
} PLUGINTYPE7_INIT, DISASSEMBLERLINEPLUGIN_INIT, *PPLUGINTYPE7_INIT, *PDISASSEMBLERLINEPLUGIN_INIT; 

typedef struct _PLUGINTYPE8_INIT
{
  CEP_PLUGINTYPE8 callbackroutine; //pointer to a callback routine of the type 8 plugin
} PLUGINTYPE8_INIT, AUTOASSEMBLERPLUGIN_INIT, *PPLUGINTYPE8_INIT, *PAUTOASSEMBLERPLUGIN_INIT; 

typedef struct _REGISTERMODIFICATIONINFO
{
  ULONG address; //addres to break on
  BOOL change_eax;
  BOOL change_ebx;
  BOOL change_ecx;
  BOOL change_edx;
  BOOL change_esi;
  BOOL change_edi;
  BOOL change_ebp;
  BOOL change_esp;
  BOOL change_eip;
  BOOL change_cf;
  BOOL change_pf;
  BOOL change_af;
  BOOL change_zf;
  BOOL change_sf;
  BOOL change_of;
  ULONG new_eax;
  ULONG new_ebx;
  ULONG new_ecx;
  ULONG new_edx;
  ULONG new_esi;
  ULONG new_edi;
  ULONG new_ebp;
  ULONG new_esp;
  ULONG new_eip;
  BOOL new_cf;
  BOOL new_pf;
  BOOL new_af;
  BOOL new_zf;
  BOOL new_sf;
  BOOL new_of;
} REGISTERMODIFICATIONINFO, *PREGISTERMODIFICATIONINFO;

//the __stdcall stuff isn't really needed since I've set compiler options to force stdcall, but this makes it clear that stdcall is used to the reader
typedef void (__stdcall *CEP_SHOWMESSAGE)(char* message);
typedef int (__stdcall *CEP_REGISTERFUNCTION) (int pluginid, PluginType functiontype, PVOID init);
typedef BOOL (__stdcall *CEP_UNREGISTERFUNCTION) (int pluginid, int functionid);
typedef HANDLE (__stdcall *CEP_GETMAINWINDOWHANDLE) (void);
typedef BOOL (__stdcall *CEP_AUTOASSEMBLE) (char *script);
typedef BOOL (__stdcall *CEP_ASSEMBLER) (ULONG address, char* instruction, BYTE *output, int maxlength, int *returnedsize);
typedef BOOL (__stdcall *CEP_DISASSEMBLER) (ULONG address, char* output, int maxsize);
typedef BOOL (__stdcall *CEP_CHANGEREGATADDRESS) (ULONG address,PREGISTERMODIFICATIONINFO changereg);
typedef BOOL (__stdcall *CEP_INJECTDLL) (char *dllname, char *functiontocall);
typedef int (__stdcall *CEP_FREEZEMEM) (ULONG address, int size);
typedef BOOL (__stdcall *CEP_UNFREEZEMEM) (int freezeID);
typedef BOOL (__stdcall *CEP_FIXMEM) (void);
typedef BOOL (__stdcall *CEP_PROCESSLIST) (char *listbuffer, int listsize);
typedef BOOL (__stdcall *CEP_RELOADSETTINGS) (void);
typedef DWORD (__stdcall *CEP_GETADDRESSFROMPOINTER) (ULONG baseaddress, int offsetcount, int* offsets);  
typedef BOOL (__stdcall *CEP_GENERATEAPIHOOKSCRIPT) (char *address, char *addresstojumpto, char *addresstogetnewcalladdress, char *script, int maxscriptsize);
typedef BOOL (__stdcall *CEP_ADDRESSTONAME) (DWORD address, char *name, int maxnamesize);
typedef BOOL (__stdcall *CEP_NAMETOADDRESS) (char *name, DWORD *address);

typedef VOID (__stdcall *CEP_LOADDBK32)(void);
typedef BOOL (__stdcall *CEP_LOADDBVMIFNEEDED)(void);
typedef DWORD (__stdcall *CEP_PREVIOUSOPCODE)(ULONG address);
typedef DWORD (__stdcall *CEP_NEXTOPCODE)(ULONG address);
typedef BOOL (__stdcall *CEP_LOADMODULE)(char *modulepath, char *exportlist, int *maxsize);
typedef BOOL (__stdcall *CEP_DISASSEMBLEEX)(ULONG address, char *output, int maxsize);
typedef VOID (__stdcall *CEP_AA_ADDCOMMAND)(char *command);
typedef VOID (__stdcall *CEP_AA_DELCOMMAND)(char *command);

typedef struct _ExportedFunctions
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
  //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
  //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  //advanced for delphi 7 enterprise dll programmers only
  PVOID mainform; //pointer to the Tmainform object. (main window) Compatible with TFORM of D7 Enterprise
  PVOID memorybrowser; //pointer to the TMemoryBrowser object (memory view windows), same as mainform

  //Plugin Version 2+
  CEP_GENERATEAPIHOOKSCRIPT sym_nameToAddress;
  CEP_ADDRESSTONAME sym_addressToName;
  CEP_NAMETOADDRESS sym_generateAPIHookScript;

  //Plugin version 3+
  CEP_LOADDBK32 loadDBK32;
  CEP_LOADDBVMIFNEEDED loaddbvmifneeded;
  CEP_PREVIOUSOPCODE previousOpcode;
  CEP_NEXTOPCODE nextOpcode;
  CEP_DISASSEMBLEEX disassembleEx;
  CEP_LOADMODULE loadModule;
  CEP_AA_ADDCOMMAND aa_AddExtraCommand;
  CEP_AA_DELCOMMAND aa_RemoveExtraCommand;


} ExportedFunctions, *PExportedFunctions;


BOOL __stdcall GetVersion(PPluginVersion pv , int sizeofpluginversion);
BOOL __stdcall InitializePlugin(PExportedFunctions ef , int pluginid);
BOOL __stdcall DisablePlugin(void);
