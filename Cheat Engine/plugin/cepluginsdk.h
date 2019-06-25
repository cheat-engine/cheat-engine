/*
 cepluginsdk.h
 Updated July 4, 2017

 v5.0.0
*/
#ifndef CEPLUGINSDK_H
#define CEPLUGINSDK_H

#include <windows.h>
#include "lua.h"
#include "lualib.h"
#include "lauxlib.h"


#define CESDK_VERSION 6

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
  UINT_PTR address;//this is a read-only representaion of the address. Change interpretedaddress if you want to change this
  BOOL ispointer; //readonly
  int countoffsets; //readonly
  ULONG *offsets; //array of dwords ranging from 0 to countoffsets-1 (readonly)
  char *description; //pointer to a 255 bytes long string
  char valuetype; //0=byte, 1=word, 2=dword, 3=float, 4=double, 5=bit, 6=int64, 7=string
  char size; //stringlength or bitlength;
} PLUGINTYPE0_RECORD, ADDRESSLIST_RECORD, *PPLUGINTYPE0_RECORD, *PADDRESSLIST_RECORD;

//callback routines efinitions for registered plugin functions:
typedef BOOL (__stdcall *CEP_PLUGINTYPE0)(PPLUGINTYPE0_RECORD SelectedRecord);
typedef BOOL (__stdcall *CEP_PLUGINTYPE1)(UINT_PTR *disassembleraddress, UINT_PTR *selected_disassembler_address, UINT_PTR *hexviewaddress);
typedef int (__stdcall *CEP_PLUGINTYPE2)(LPDEBUG_EVENT DebugEvent);
typedef void (__stdcall *CEP_PLUGINTYPE3)(ULONG processid, ULONG peprocess, BOOL Created);
typedef void (__stdcall *CEP_PLUGINTYPE4)(int reserved);
typedef void (__stdcall *CEP_PLUGINTYPE5)(void);
typedef BOOL (__stdcall *CEP_PLUGINTYPE6ONPOPUP)(UINT_PTR selectedAddress, char **addressofname, BOOL *show);
typedef BOOL (__stdcall *CEP_PLUGINTYPE6)(UINT_PTR *selectedAddress);
typedef void (__stdcall *CEP_PLUGINTYPE7)(UINT_PTR address, char **addressStringPointer, char **bytestringpointer, char **opcodestringpointer, char **specialstringpointer, ULONG *textcolor);
typedef void (__stdcall *CEP_PLUGINTYPE8)(char **line, AutoAssemblerPhase phase, int id);



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
  CEP_PLUGINTYPE6 callbackroutine; 
  CEP_PLUGINTYPE6ONPOPUP callbackroutineOnPopup; 
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
  UINT_PTR address; //addres to break on
  BOOL change_eax;
  BOOL change_ebx;
  BOOL change_ecx;
  BOOL change_edx;
  BOOL change_esi;
  BOOL change_edi;
  BOOL change_ebp;
  BOOL change_esp;
  BOOL change_eip;
#ifdef _AMD64_
  BOOL change_r8;
  BOOL change_r9;
  BOOL change_r10;
  BOOL change_r11;
  BOOL change_r12;
  BOOL change_r13;
  BOOL change_r14;
  BOOL change_r15;
#endif
  BOOL change_cf;
  BOOL change_pf;
  BOOL change_af;
  BOOL change_zf;
  BOOL change_sf;
  BOOL change_of;
  UINT_PTR new_eax;
  UINT_PTR new_ebx;
  UINT_PTR new_ecx;
  UINT_PTR new_edx;
  UINT_PTR new_esi;
  UINT_PTR new_edi;
  UINT_PTR new_ebp;
  UINT_PTR new_esp;
  UINT_PTR new_eip;
#ifdef _AMD64_
  UINT_PTR new_r8;
  UINT_PTR new_r9;
  UINT_PTR new_r10;
  UINT_PTR new_r11;
  UINT_PTR new_r12;
  UINT_PTR new_r13;
  UINT_PTR new_r14;
  UINT_PTR new_r15;
#endif
	 

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
typedef BOOL (__stdcall *CEP_ASSEMBLER) (UINT_PTR address, char* instruction, BYTE *output, int maxlength, int *returnedsize);
typedef BOOL (__stdcall *CEP_DISASSEMBLER) (UINT_PTR address, char* output, int maxsize);
typedef BOOL (__stdcall *CEP_CHANGEREGATADDRESS) (UINT_PTR address,PREGISTERMODIFICATIONINFO changereg);
typedef BOOL (__stdcall *CEP_INJECTDLL) (char *dllname, char *functiontocall);
typedef int (__stdcall *CEP_FREEZEMEM) (UINT_PTR address, int size);
typedef BOOL (__stdcall *CEP_UNFREEZEMEM) (int freezeID);
typedef BOOL (__stdcall *CEP_FIXMEM) (void);
typedef BOOL (__stdcall *CEP_PROCESSLIST) (char *listbuffer, int listsize);
typedef BOOL (__stdcall *CEP_RELOADSETTINGS) (void);
typedef UINT_PTR (__stdcall *CEP_GETADDRESSFROMPOINTER) (UINT_PTR baseaddress, int offsetcount, int* offsets);  
typedef BOOL (__stdcall *CEP_GENERATEAPIHOOKSCRIPT) (char *address, char *addresstojumpto, char *addresstogetnewcalladdress, char *script, int maxscriptsize);
typedef BOOL (__stdcall *CEP_ADDRESSTONAME) (UINT_PTR address, char *name, int maxnamesize);
typedef BOOL (__stdcall *CEP_NAMETOADDRESS) (char *name, UINT_PTR *address);

typedef VOID (__stdcall *CEP_LOADDBK32)(void);
typedef BOOL (__stdcall *CEP_LOADDBVMIFNEEDED)(void);
typedef DWORD (__stdcall *CEP_PREVIOUSOPCODE)(UINT_PTR address);
typedef DWORD (__stdcall *CEP_NEXTOPCODE)(UINT_PTR address);
typedef BOOL (__stdcall *CEP_LOADMODULE)(char *modulepath, char *exportlist, int *maxsize);
typedef BOOL (__stdcall *CEP_DISASSEMBLEEX)(UINT_PTR address, char *output, int maxsize);
typedef VOID (__stdcall *CEP_AA_ADDCOMMAND)(char *command);
typedef VOID (__stdcall *CEP_AA_DELCOMMAND)(char *command);

typedef PVOID (__stdcall *CEP_CREATETABLEENTRY)(void);
typedef PVOID (__stdcall *CEP_GETTABLEENTRY)(char *description);
typedef BOOL (__stdcall *CEP_MEMREC_SETDESCRIPTION)(PVOID memrec, char *description);
typedef PCHAR (__stdcall *CEP_MEMREC_GETDESCRIPTION)(PVOID memrec);
typedef BOOL (__stdcall *CEP_MEMREC_GETADDRESS)(PVOID memrec, UINT_PTR *address, DWORD *offsets, int maxoffsets, int *neededOffsets);
typedef BOOL (__stdcall *CEP_MEMREC_SETADDRESS)(PVOID memrec, char *address, DWORD *offsets, int offsetcount);
typedef int (__stdcall *CEP_MEMREC_GETTYPE)(PVOID memrec);
typedef BOOL (__stdcall *CEP_MEMREC_SETTYPE)(PVOID memrec, int vtype);
typedef BOOL (__stdcall *CEP_MEMREC_GETVALUETYPE)(PVOID memrec, char *value, int maxsize);
typedef BOOL (__stdcall *CEP_MEMREC_SETVALUETYPE)(PVOID memrec, char *value);
typedef char* (__stdcall *CEP_MEMREC_GETSCRIPT)(PVOID memrec);
typedef BOOL (__stdcall *CEP_MEMREC_SETSCRIPT)(PVOID memrec, char *script);
typedef BOOL (__stdcall *CEP_MEMREC_ISFROZEN)(PVOID memrec);
typedef BOOL (__stdcall *CEP_MEMREC_FREEZE)(PVOID memrec, int direction);
typedef BOOL (__stdcall *CEP_MEMREC_UNFREEZE)(PVOID memrec);
typedef BOOL (__stdcall *CEP_MEMREC_SETCOLOR)(PVOID memrec, DWORD color);
typedef BOOL (__stdcall *CEP_MEMREC_APPENDTOENTRY)(PVOID memrec1, PVOID memrec2);
typedef BOOL (__stdcall *CEP_MEMREC_DELETE)(PVOID memrec);

typedef DWORD (__stdcall *CEP_GETPROCESSIDFROMPROCESSNAME)(char *name);
typedef DWORD (__stdcall *CEP_OPENPROCESS)(DWORD pid);
typedef DWORD (__stdcall *CEP_DEBUGPROCESS)(int debuggerinterface);
typedef VOID (__stdcall *CEP_PAUSE)(void);
typedef VOID (__stdcall *CEP_UNPAUSE)(void);
typedef BOOL (__stdcall *CEP_DEBUG_SETBREAKPOINT)(UINT_PTR address, int size, int trigger);
typedef BOOL (__stdcall *CEP_DEBUG_REMOVEBREAKPOINT)(UINT_PTR address);
typedef BOOL (__stdcall *CEP_DEBUG_CONTINUEFROMBREAKPOINT)(int continueoption);

typedef VOID (__stdcall *CEP_CLOSECE)(void);
typedef VOID (__stdcall *CEP_HIDEALLCEWINDOWS)(void);
typedef VOID (__stdcall *CEP_UNHIDEMAINCEWINDOW)(void);

typedef PVOID (__stdcall *CEP_CREATEFORM)(void);
typedef void (__stdcall *CEP_FORM_CENTERSCREEN)(PVOID form);
typedef void (__stdcall *CEP_FORM_HIDE)(PVOID form);
typedef void (__stdcall *CEP_FORM_SHOW)(PVOID form);
typedef void (__stdcall *CEP_FORM_ONCLOSE)(PVOID form, PVOID function);

typedef PVOID (__stdcall *CEP_CREATEPANEL)(PVOID owner);
typedef PVOID (__stdcall *CEP_CREATEGROUPBOX)(PVOID owner);
typedef PVOID (__stdcall *CEP_CREATEBUTTON)(PVOID owner);
typedef PVOID (__stdcall *CEP_CREATEIMAGE)(PVOID owner);

typedef BOOL (__stdcall *CEP_IMAGE_LOADIMAGEFROMFILE)(PVOID image, char *filename);
typedef VOID (__stdcall *CEP_IMAGE_TRANSPARENT)(PVOID image, BOOL transparent);
typedef VOID (__stdcall *CEP_IMAGE_STRETCH)(PVOID image, BOOL stretch);

typedef PVOID (__stdcall *CEP_CREATELABEL)(PVOID owner);
typedef PVOID (__stdcall *CEP_CREATEEDIT)(PVOID owner);
typedef PVOID (__stdcall *CEP_CREATEMEMO)(PVOID owner);
typedef PVOID (__stdcall *CEP_CREATETIMER)(PVOID owner);

typedef VOID (__stdcall *CEP_TIMER_SETINTERVAL)(PVOID timer, int interval);
typedef VOID (__stdcall *CEP_TIMER_ONTIMER)(PVOID timer, PVOID function);

typedef VOID (__stdcall *CEP_CONTROL_SETCAPTION)(PVOID control, char *caption);
typedef BOOL (__stdcall *CEP_CONTROL_GETCAPTION)(PVOID control, char *caption, int maxsize);

typedef VOID (__stdcall *CEP_CONTROL_SETPOSITION)(PVOID control, int x, int y);
typedef int (__stdcall *CEP_CONTROL_GETX)(PVOID control);
typedef int (__stdcall *CEP_CONTROL_GETY)(PVOID control);

typedef VOID (__stdcall *CEP_CONTROL_SETSIZE)(PVOID control, int width, int height);
typedef int (__stdcall *CEP_CONTROL_GETWIDTH)(PVOID control);
typedef int (__stdcall *CEP_CONTROL_GETHEIGHT)(PVOID control);

typedef VOID (__stdcall *CEP_CONTROL_SETALIGN)(PVOID control, int align);
typedef VOID (__stdcall *CEP_CONTROL_ONCLICK)(PVOID control, PVOID function);

typedef VOID (__stdcall *CEP_OBJECT_DESTROY)(PVOID object);

typedef int (__stdcall *CEP_MESSAGEDIALOG)(char *massage, int messagetype, int buttoncombination);
typedef BOOL (__stdcall *CEP_SPEEDHACK_SETSPEED)(float speed);
typedef lua_State *(__fastcall *CEP_GETLUASTATE)();


typedef BOOL(__stdcall **CEP_READPROCESSMEMORY)(HANDLE hProcess, LPCVOID lpBaseAddress, LPVOID lpBuffer, SIZE_T nSize, SIZE_T * lpNumberOfBytesRead);

/*
function ce_messageDialog(message: pchar; messagetype: integer; buttoncombination: integer): integer; stdcall;
function ce_speedhack_setSpeed(speed: single): BOOL; stdcall;       
*/

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
  CEP_READPROCESSMEMORY ReadProcessMemory;			//pointer to the pointer of ReadProcessMemory (Change it to hook that api, or use it yourself)
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
  PVOID mainform; //pointer to the Tmainform object.
  PVOID memorybrowser; //pointer to the TMemoryBrowser object (memory view windows), same as mainform

  //Plugin Version 2+
  CEP_NAMETOADDRESS sym_nameToAddress;
  CEP_ADDRESSTONAME sym_addressToName;
  CEP_GENERATEAPIHOOKSCRIPT sym_generateAPIHookScript;

  //Plugin version 3+
  CEP_LOADDBK32 loadDBK32;
  CEP_LOADDBVMIFNEEDED loaddbvmifneeded;
  CEP_PREVIOUSOPCODE previousOpcode;
  CEP_NEXTOPCODE nextOpcode;
  CEP_DISASSEMBLEEX disassembleEx;
  CEP_LOADMODULE loadModule;
  CEP_AA_ADDCOMMAND aa_AddExtraCommand;
  CEP_AA_DELCOMMAND aa_RemoveExtraCommand;

  //version 4 extension
  CEP_CREATETABLEENTRY createTableEntry;
  CEP_GETTABLEENTRY getTableEntry;
  CEP_MEMREC_SETDESCRIPTION memrec_setDescription;
  CEP_MEMREC_GETDESCRIPTION memrec_getDescription;
  CEP_MEMREC_GETADDRESS memrec_getAddress;
  CEP_MEMREC_SETADDRESS memrec_setAddress;
  CEP_MEMREC_GETTYPE memrec_getType;
  CEP_MEMREC_SETTYPE memrec_setType;
  CEP_MEMREC_GETVALUETYPE memrec_getValue;
  CEP_MEMREC_SETVALUETYPE memrec_setValue;
  CEP_MEMREC_GETSCRIPT memrec_getScript;
  CEP_MEMREC_SETSCRIPT memrec_setScript;
  CEP_MEMREC_ISFROZEN memrec_isfrozen;
  CEP_MEMREC_FREEZE memrec_freeze;
  CEP_MEMREC_UNFREEZE memrec_unfreeze;
  CEP_MEMREC_SETCOLOR memrec_setColor;
  CEP_MEMREC_APPENDTOENTRY memrec_appendtoentry;
  CEP_MEMREC_DELETE memrec_delete;

  CEP_GETPROCESSIDFROMPROCESSNAME getProcessIDFromProcessName;
  CEP_OPENPROCESS openProcessEx;
  CEP_DEBUGPROCESS debugProcessEx;
  CEP_PAUSE pause;
  CEP_UNPAUSE unpause;

  CEP_DEBUG_SETBREAKPOINT debug_setBreakpoint;
  CEP_DEBUG_REMOVEBREAKPOINT debug_removeBreakpoint;
  CEP_DEBUG_CONTINUEFROMBREAKPOINT debug_continueFromBreakpoint;

  CEP_CLOSECE closeCE;
  CEP_HIDEALLCEWINDOWS hideAllCEWindows;
  CEP_UNHIDEMAINCEWINDOW unhideMainCEwindow;
  CEP_CREATEFORM createForm;
  CEP_FORM_CENTERSCREEN form_centerScreen;
  CEP_FORM_HIDE form_hide;
  CEP_FORM_SHOW form_show;
  CEP_FORM_ONCLOSE form_onClose;

  CEP_CREATEPANEL createPanel;
  CEP_CREATEGROUPBOX createGroupBox;
  CEP_CREATEBUTTON createButton;
  CEP_CREATEIMAGE createImage;
  CEP_IMAGE_LOADIMAGEFROMFILE image_loadImageFromFile;
  CEP_IMAGE_TRANSPARENT image_transparent;
  CEP_IMAGE_STRETCH image_stretch;

  CEP_CREATELABEL createLabel;
  CEP_CREATEEDIT createEdit;
  CEP_CREATEMEMO createMemo;
  CEP_CREATETIMER createTimer;
  CEP_TIMER_SETINTERVAL timer_setInterval;
  CEP_TIMER_ONTIMER timer_onTimer;
  CEP_CONTROL_SETCAPTION control_setCaption;
  CEP_CONTROL_GETCAPTION control_getCaption;
  CEP_CONTROL_SETPOSITION control_setPosition;
  CEP_CONTROL_GETX control_getX;
  CEP_CONTROL_GETY control_getY;
  CEP_CONTROL_SETSIZE control_setSize;
  CEP_CONTROL_GETWIDTH control_getWidth;
  CEP_CONTROL_GETHEIGHT control_getHeight;
  CEP_CONTROL_SETALIGN control_setAlign;
  CEP_CONTROL_ONCLICK control_onClick;

  CEP_OBJECT_DESTROY object_destroy;
  CEP_MESSAGEDIALOG messageDialog;
  CEP_SPEEDHACK_SETSPEED speedhack_setSpeed;  

//V5: Todo, implement function declarations
  VOID *ExecuteKernelCode;
  VOID *UserdefinedInterruptHook;
  CEP_GETLUASTATE GetLuaState;
  VOID *MainThreadCall;

} ExportedFunctions, *PExportedFunctions;


BOOL __stdcall CEPlugin_GetVersion(PPluginVersion pv , int sizeofpluginversion);
BOOL __stdcall CEPlugin_InitializePlugin(PExportedFunctions ef , int pluginid);
BOOL __stdcall CEPlugin_DisablePlugin(void);
//old versions without CEPlugin_ in front also work but are not recommended due to bugbrained compilers...


#endif //CEPLUGINSDK_H
