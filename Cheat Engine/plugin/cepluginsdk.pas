unit cepluginsdk;        //more an api than sdk

{$MODE Delphi}

interface

uses windows, sysutils;

type
  TColor=dword;
  PColor=^TColor;

{$ifndef fpc}
//if old delphi then define the ptruint type
type ptruint=dword;
type pptruint=^ptruint'
{$endif}

const PluginVersionSDK=6;

type TAutoAssemblerPhase=(aaInitialize=0, aaPhase1=1, aaPhase2=2, aaFinalize=3);
type TPluginType=(ptAddressList=0, ptMemoryView=1, ptOnDebugEvent=2, ptProcesswatcherEvent=3, ptFunctionPointerchange=4, ptMainMenu=5, ptDisassemblerContext=6, ptDisassemblerRenderLine=7, ptAutoAssembler=8);

type TDWordArray = array[0..0] of DWord;
     PDWordArray = ^TDWordArray;

type
  TContinueOption = (co_run=0, co_stepinto=1, co_stepover=2, co_runtill=3);

type
  TBreakpointMethod = (bpmInt3, bpmDebugRegister);

type
  TBreakOption = (bo_Break = 0, bo_ChangeRegister = 1, bo_FindCode = 2, bo_FindWhatCodeAccesses = 3, bo_BreakAndTrace=4);
  TBreakPointAction = TBreakOption;

type
  TBreakpointTrigger = (bptExecute=0, bptAccess=1, bptWrite=2);

type
  tregistermodificationBP32 = record
    address: uint_ptr; //addres to break on
    change_eax: BOOL;
    change_ebx: BOOL;
    change_ecx: BOOL;
    change_edx: BOOL;
    change_esi: BOOL;
    change_edi: BOOL;
    change_ebp: BOOL;
    change_esp: BOOL;
    change_eip: BOOL;
    change_cf: BOOL;
    change_pf: BOOL;
    change_af: BOOL;
    change_zf: BOOL;
    change_sf: BOOL;
    change_of: BOOL;
    new_eax: dword;
    new_ebx: dword;
    new_ecx: dword;
    new_edx: dword;
    new_esi: dword;
    new_edi: dword;
    new_ebp: dword;
    new_esp: dword;
    new_eip: dword;
    new_cf: BOOL;
    new_pf: BOOL;
    new_af: BOOL;
    new_zf: BOOL;
    new_sf: BOOL;
    new_of: BOOL;
  end;

type
  PRegisterModificationBP32 = ^TRegisterModificationBP32;

type
  tregistermodificationBP64 = record
    address: uint_ptr; //addres to break on
    change_eax: BOOL;
    change_ebx: BOOL;
    change_ecx: BOOL;
    change_edx: BOOL;
    change_esi: BOOL;
    change_edi: BOOL;
    change_ebp: BOOL;
    change_esp: BOOL;
    change_eip: BOOL;
    change_r8: BOOL;
    change_r9: BOOL;
    change_r10: BOOL;
    change_r11: BOOL;
    change_r12: BOOL;
    change_r13: BOOL;
    change_r14: BOOL;
    change_r15: BOOL;
    change_cf: BOOL;
    change_pf: BOOL;
    change_af: BOOL;
    change_zf: BOOL;
    change_sf: BOOL;
    change_of: BOOL;
    new_eax: ptrUint;
    new_ebx: ptrUint;
    new_ecx: ptrUint;
    new_edx: ptrUint;
    new_esi: ptrUint;
    new_edi: ptrUint;
    new_ebp: ptrUint;
    new_esp: ptrUint;
    new_eip: ptrUint;
    new_r8: ptrUint;
    new_r9: ptrUint;
    new_r10: ptrUint;
    new_r11: ptrUint;
    new_r12: ptrUint;
    new_r13: ptrUint;
    new_r14: ptrUint;
    new_r15: ptrUint;
    new_cf: BOOL;
    new_pf: BOOL;
    new_af: BOOL;
    new_zf: BOOL;
    new_sf: BOOL;
    new_of: BOOL;
  end;

type
  PRegisterModificationBP64 = ^TRegisterModificationBP64;

{$ifdef CPU64}
type
  TRegisterModificationBP = TRegisterModificationBP64;

type
  PRegisterModificationBP = PRegisterModificationBP64;
{$else}
type
  TRegisterModificationBP = TRegisterModificationBP32;

type
  PRegisterModificationBP = PRegisterModificationBP32;
{$endif}


 type TSelectedRecord=record
   interpretedaddress: pchar; //pointer to a 255 bytes long string (0 terminated)
   address: ptruint; //this is a read-only representaion of the address. Change interpretedaddress if you want to change this
   ispointer: boolean;
   countoffsets: integer;
   offsets: PDWordArray; //pointer to a array of dwords randing from 0 to countoffsets-1
   description: pchar; //pointer to a 255 bytes long string
   valuetype: byte;
   size: byte; //stringlenth or bitlength (max 255);
 end;
 type PSelectedRecord=^TSelectedRecord;


 //callback function declarations:
 type TPlugin0_SelectedRecord=record
   interpretedaddress: pchar; //pointer to a 255 bytes long string (0 terminated)
   address: dword; //this is a read-only representaion of the address. Change interpretedaddress if you want to change this
   ispointer: BOOL; //readonly
   countoffsets: integer; //readonly
   offsets: PDWordArray; //pointer to a array of dwords randing from 0 to countoffsets-1 (readonly)
   description: pchar; //pointer to a 255 bytes long string
   valuetype: byte;
   size: byte; //stringlenth or bitlength (max 255);
 end;
 type PPlugin0_SelectedRecord=^TPlugin0_SelectedRecord;
 type TPluginfunction0=function(selectedrecord: PPlugin0_SelectedRecord):bool; stdcall;
 type TPluginfunction1=function(disassembleraddress: pptruint; selected_disassembler_address: pptruint; hexviewaddress:pptruint ):bool; stdcall;
 type TPluginFunction2=function(debugevent: PDebugEvent):integer; stdcall;
 type TPluginFunction3=function(processid: dword; peprocess:ptruint; created: BOOL):integer; stdcall;
 type TPluginFunction4=function(section: integer):boolean; stdcall;
 type TPluginfunction5=procedure; stdcall;
 type TPluginfunction6OnPopup=function(selectedAddress: ptruint; addressofname: pointer; show: pbool):bool; stdcall;
 type TPluginfunction6=function(selectedAddress: pptruint):bool; stdcall;
 type TPluginFunction7=procedure(address: ptruint; addressStringPointer: pointer; bytestringpointer: pointer; opcodestringpointer: pointer; specialstringpointer: pointer; textcolor: PColor); stdcall;
 type TPluginFunction8=procedure(line: ppchar; phase: TAutoAssemblerPhase; id:integer); stdcall;


type Tfunction0=record
  name: pchar;
  callbackroutine: pointer;
end;
type Tfunction1=record
  name: pchar;
  callbackroutine: TPluginfunction1;
  shortcut: pchar;
end;
type Tfunction2=record
  callbackroutine: pointer;
end;
type Tfunction6=record
  name: pchar;
  callbackroutine: pointer;
  callbackroutineOnContext: pointer;
  shortcut: pchar;
end;
type TFunction3=TFunction2;
type TFunction4=TFunction2;
type TFunction5=record
  name: pchar;
  callbackroutine: TPluginfunction5;
  shortcut: pchar;
end;
type TFunction7=TFunction2;
type TFunction8=TFunction2;


type PFunction0=^TFunction0;
type PFunction1=^TFunction1;
type PFunction2=^TFunction2;
type PFunction3=^TFunction3;
type PFunction4=^TFunction4;
type PFunction5=^TFunction5;
type PFunction6=^TFunction6;
type PFunction7=^TFunction7;
type PFunction8=^TFunction8;

type Tce_showmessage=procedure (s: pchar); stdcall;
type Tce_registerfunction=function (pluginid: integer; functiontype:TPluginType; init: pointer):integer; stdcall;
type Tce_unregisterfunction=function (pluginid,functionid: integer): BOOL; stdcall;
type Tce_AutoAssembler=function (s: pchar):BOOL; stdcall;
type Tce_assembler=function(address:ptruint; instruction: pchar; output: PByteArray; maxlength: integer; actualsize: pinteger):BOOL; stdcall;
type Tce_disassembler=function(address: ptruint; output: pchar; maxsize: integer): BOOL; stdcall;


type Tce_ChangeRegistersAtAddress=function(address:ptruint; changereg: pregistermodificationBP):BOOL; stdcall;

type Tce_InjectDLL=function(dllname: pchar; functiontocall: pchar):BOOL; stdcall;

type Tce_freezemem= function (address: ptruint; size: integer):integer; stdcall;
type Tce_unfreezemem =function (id: integer):BOOL; stdcall;
//type Tce_fixmem=function:BOOL; stdcall; //obsolete, don't use it
type Tce_processlist=function(listbuffer: pchar; listsize: integer):BOOL; stdcall;
type Tce_reloadsettings=function:BOOL; stdcall;
type Tce_getaddressfrompointer=function(baseaddress: ptruint; offsetcount: integer; offsets: PDwordArray):dword; stdcall;


type Tce_generateAPIHookScript=function(address, addresstojumpto, addresstogetnewcalladdress, script: pchar; maxscriptsize: integer): BOOL; stdcall;
type Tce_sym_addressToName=function(address:ptruint; name: pchar; maxnamesize: integer):BOOL; stdcall;
type Tce_sym_nameToAddress=function(name: pchar; address: Pptruint):BOOL; stdcall;


type Tce_GetMainWindowHandle=function:thandle; stdcall;

type TReadProcessMemory=function (hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer;
  nSize: DWORD; var lpNumberOfBytesRead: DWORD): BOOL; stdcall;

type TWriteProcessMemory=function (hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer;
  nSize: DWORD; var lpNumberOfBytesWritten: DWORD): BOOL; stdcall;

type TGetProcessNameFromPEProcess=function(peprocess:ptruint; buffer:pchar;buffersize:dword):integer; stdcall;
type TOpenProcess=function(dwDesiredAccess: DWORD; bInheritHandle: BOOL; dwProcessId: DWORD): THandle; stdcall;


type TLoadDBK32=procedure; stdcall;
type TLoadDBVMifneeded=function: BOOL; stdcall;
type TPreviousOpcode=function(address:ptruint): dword; stdcall;
type TNextOpcode=function(address:ptruint): dword; stdcall;
type TloadModule=function(modulepath: pchar; exportlist: pchar; maxsize: pinteger): BOOL; stdcall;
type TDisassembleEx=function(address: pptruint; output: pchar; maxsize: integer): BOOL; stdcall;
type Taa_AddExtraCommand=procedure(command:pchar);
type Taa_RemoveExtraCommand=procedure(command:pchar);



//version 4 functiontype declarations
type TcreateTableEntry=function: pointer; stdcall;
type TgetTableEntry=function(description: pchar): pointer; stdcall;
type Tmemrec_setDescription=function(memrec: pointer; description: pchar): BOOL; stdcall;
type Tmemrec_getDescription=function(memrec: pointer): pchar; stdcall;
type Tmemrec_getAddress=function(memrec: pointer; address: pptruint; offsets: PDwordArray; maxoffsets: integer; neededOffsets: pinteger): BOOL; stdcall;
type Tmemrec_setAddress=function(memrec: pointer; address: pchar; offsets: PDwordArray; offsetcount: integer): BOOL; stdcall;
type Tmemrec_getType=function(memrec: pointer): integer; stdcall;
type Tmemrec_setType=function(memrec: pointer; vtype: integer): BOOL; stdcall;
type Tmemrec_getValue=function(memrec: pointer; value: pchar; maxsize: integer): BOOL; stdcall;
type Tmemrec_setValue=function(memrec: pointer; value: pchar): BOOL; stdcall;
type Tmemrec_getScript=function(memrec: pointer): pchar; stdcall;
type Tmemrec_setScript=function(memrec: pointer; script: pchar): BOOL; stdcall;
type Tmemrec_isFrozen=function(memrec: pointer): BOOL; stdcall;
type Tmemrec_freeze=function(memrec: pointer; direction: integer): BOOL; stdcall;
type Tmemrec_unfreeze=function(memrec: pointer): BOOL; stdcall;
type Tmemrec_setColor=function(memrec: pointer; color: TColor): BOOL; stdcall;
type Tmemrec_appendtoentry=function(memrec1: pointer; memrec2: pointer): BOOL; stdcall;
type Tmemrec_delete=function(memrec: pointer): BOOL; stdcall;
type TgetProcessIDFromProcessName=function(name: pchar): DWORD; stdcall;
type TopenProcessex=function(pid: dword): BOOL; stdcall;
type TdebugProcessex=function(debuggerinterface: integer): BOOL; stdcall;
type Tpause=procedure; stdcall;
type Tunpause=procedure; stdcall;

type Tdebug_setBreakpoint=function(address: ptruint; size: integer; trigger: TBreakpointTrigger): BOOL; stdcall;
type Tdebug_removeBreakpoint=function(address: ptruint): BOOL; stdcall;
type Tdebug_continueFromBreakpoint=function(ContinueOption: TContinueOption): BOOL; stdcall;

type TcloseCE=procedure; stdcall;
type ThideAllCEWindows=procedure; stdcall;
type TunhideMainCEwindow=procedure; stdcall;
type TcreateForm=function(visible: boolean): pointer; stdcall;
type Tform_centerScreen=procedure(f: pointer); stdcall;
type Tform_hide=procedure(f: pointer); stdcall;
type Tform_show=procedure(f: pointer); stdcall;
type Tform_onClose=procedure(frm: pointer; f: pointer); stdcall;

type TcreatePanel=function(owner: pointer): pointer; stdcall;
type TcreateGroupBox=function(owner: pointer): pointer; stdcall;
type TcreateButton=function(owner: pointer): pointer; stdcall;
type TcreateImage=function(owner: pointer): pointer; stdcall;
type Timage_loadImageFromFile=function(image: pointer; filename: pchar): BOOL; stdcall;
type Timage_transparent=procedure(image: pointer; transparent: boolean); stdcall;
type Timage_stretch=procedure(image: pointer; stretch: boolean); stdcall;

type TcreateLabel=function(owner: pointer): pointer; stdcall;
type TcreateEdit=function(owner: pointer): pointer; stdcall;
type TcreateMemo=function(owner: pointer): pointer; stdcall;
type TcreateTimer=function(owner: pointer): pointer; stdcall;
type Ttimer_setInterval=procedure(timer: pointer; interval: integer); stdcall;
type Ttimer_onTimer=procedure(t: pointer; f: pointer); stdcall;
type Tcontrol_setCaption=procedure(control: pointer; caption: pchar); stdcall;
type Tcontrol_getCaption=function(control: pointer; caption: pchar; maxsize: integer): BOOL; stdcall;
type Tcontrol_setPosition=procedure(control: pointer; x,y: integer); stdcall;
type Tcontrol_getX=function(control: pointer): integer; stdcall;
type Tcontrol_getY=function(control: pointer): integer; stdcall;
type Tcontrol_setSize=procedure(control: pointer; width,height: integer); stdcall;
type Tcontrol_getWidth=function(control: pointer): integer; stdcall;
type Tcontrol_getHeight=function(control: pointer): integer; stdcall;
type Tcontrol_setAlign=procedure(control: pointer; align: integer); stdcall;
type Tcontrol_onClick=procedure(c: pointer; f: pointer); stdcall;

type Tobject_destroy=procedure(o: pointer); stdcall;
type TmessageDialog=function(message: pchar; messagetype: integer; buttoncombination: integer): integer; stdcall;
type Tspeedhack_setSpeed=function(speed: single): BOOL; stdcall;

type TUserdefinedInterruptHook=function(interruptnr: integer; newCS: word; newEIP: system.qword; addressofjumpback: system.qword):boolean; stdcall;
type TExecuteKernelCode=function(address: system.qword; parameters: system.qword): BOOL; stdcall;
type TGetLoadedState=function: BOOLEAN; stdcall;
type TGetLuaState=function: pointer; stdcall;

type TPluginFunc=function(parameters: pointer): pointer; //note, no stdcall. It's a "pascal" calling convention
type TMainThreadCall=function(func: TPluginFunc; parameters: pointer): pointer; stdcall;

type TPluginVersion =record
  version : integer; //write here the minimum version this dll is compatible with
  pluginname: pchar;  //make this point to a 0-terminated string (allocated memory, not stack)
end;



type TExportedFunctions = record
  sizeofTExportedFunctions: integer;
  showmessage: Tce_showmessage;
  registerfunction: Tce_registerfunction;
  unregisterfunction: Tce_unregisterfunction;
  OpenedProcessID: ^dword;
  OpenedProcessHandle: ^thandle;

  GetMainWindowHandle: pointer;
  AutoAssemble: Tce_AutoAssembler;
  //this is just an example plugin, fill theswe missing function pointers in yourself ok...
  assembler: Tce_assembler;
  disassembler: Tce_disassembler;
  ChangeRegistersAtAddress: Tce_ChangeRegistersAtAddress;
  InjectDLL: Tce_InjectDLL;
  freezemem: Tce_freezemem;
  unfreezemem: Tce_unfreezemem;
  fixmem: pointer;
  processlist: Tce_processlist;
  reloadsettings: Tce_reloadsettings;
  getaddressfrompointer: Tce_getaddressfrompointer;

  //pointers to the address that contains the pointers to the functions
  ReadProcessMemory     :ppointer;
  WriteProcessMemory    :ppointer;
  GetThreadContext      :ppointer;
  SetThreadContext      :ppointer;
  SuspendThread         :ppointer;
  ResumeThread          :ppointer;
  OpenProcess           :ppointer;
  WaitForDebugEvent     :ppointer;
  ContinueDebugEvent    :ppointer;
  DebugActiveProcess    :ppointer;
  StopDebugging         :ppointer;
  StopRegisterChange    :ppointer;
  VirtualProtect        :ppointer;
  VirtualProtectEx      :ppointer;
  VirtualQueryEx        :ppointer;
  VirtualAllocEx        :ppointer;
  CreateRemoteThread    :ppointer;
  OpenThread            :ppointer;
  GetPEProcess          :ppointer;
  GetPEThread           :ppointer;
  GetThreadsProcessOffset:ppointer;
  GetThreadListEntryOffset:ppointer;
  GetProcessnameOffset  :ppointer;
  GetDebugportOffset    :ppointer;
  GetPhysicalAddress    :ppointer;
  ProtectMe             :ppointer;
  GetCR4                :ppointer;
  GetCR3                :ppointer;
  SetCR3                :ppointer;
  GetSDT                :ppointer;
  GetSDTShadow          :ppointer;
  setAlternateDebugMethod: ppointer;
  getAlternateDebugMethod: ppointer;
  DebugProcess          :ppointer;
  ChangeRegOnBP         :ppointer;
  RetrieveDebugData     :ppointer;
  StartProcessWatch     :ppointer;
  WaitForProcessListData:ppointer;
  GetProcessNameFromID  :ppointer;
  GetProcessNameFromPEProcess:ppointer;
  KernelOpenProcess       :ppointer;
  KernelReadProcessMemory :ppointer;
  KernelWriteProcessMemory:ppointer;
  KernelVirtualAllocEx    :ppointer;
  IsValidHandle           :ppointer;
  GetIDTCurrentThread     :ppointer;
  GetIDTs                 :ppointer;
  MakeWritable            :ppointer;
  GetLoadedState          :TGetLoadedState;
  DBKSuspendThread        :ppointer;
  DBKResumeThread         :ppointer;
  DBKSuspendProcess       :ppointer;
  DBKResumeProcess        :ppointer;
  KernelAlloc             :ppointer;
  GetKProcAddress         :ppointer;
  CreateToolhelp32Snapshot:ppointer;
  Process32First          :ppointer;
  Process32Next           :ppointer;
  Thread32First           :ppointer;
  Thread32Next            :ppointer;
  Module32First           :ppointer;
  Module32Next            :ppointer;
  Heap32ListFirst         :ppointer;
  Heap32ListNext          :ppointer;

  //advanced for delphi 7 enterprise dll programmers only
  mainform                :pointer;
  memorybrowser           :pointer;

  //version 2 extension:
  sym_nameToAddress         : Tce_sym_NameToAddress;
  sym_addressToName         : Tce_sym_addressToName;
  ce_generateAPIHookScript  : Tce_generateAPIHookScript;

  //version 3
  loadDBK32         : TLoadDBK32;
  loaddbvmifneeded  : TLoadDBVMifneeded;
  previousOpcode    : TPreviousOpcode;
  nextopcode        : TNextOpcode;
  disassembleEx     : TDisassembleEx;
  loadModule        : TloadModule;

  aa_AddExtraCommand: Taa_AddExtraCommand;
  aa_RemoveExtraCommand:Taa_RemoveExtraCommand;

  //version 4 extension
  createTableEntry: TcreateTableEntry;
  getTableEntry: TgetTableEntry;
  memrec_setDescription: Tmemrec_setDescription;
  memrec_getDescription: Tmemrec_getDescription;
  memrec_getAddress: Tmemrec_getAddress;
  memrec_setAddress: Tmemrec_setAddress;
  memrec_getType: Tmemrec_getType;
  memrec_setType: Tmemrec_setType;
  memrec_getValue: Tmemrec_getValue;
  memrec_setValue: Tmemrec_setValue;
  memrec_getScript: Tmemrec_getScript;
  memrec_setScript: Tmemrec_setScript;
  memrec_isfrozen: Tmemrec_isfrozen;
  memrec_freeze: Tmemrec_freeze;
  memrec_unfreeze: Tmemrec_unfreeze;
  memrec_setColor: Tmemrec_setColor;
  memrec_appendtoentry: Tmemrec_appendtoentry;
  memrec_delete: Tmemrec_delete;

  getProcessIDFromProcessName: TgetProcessIDFromProcessName;
  openProcessEx: TopenProcessEx;
  debugProcessEx: TdebugProcessEx;
  pause: Tpause;
  unpause: Tunpause;

  debug_setBreakpoint: Tdebug_setBreakpoint;
  debug_removeBreakpoint: Tdebug_removeBreakpoint;
  debug_continueFromBreakpoint: Tdebug_continueFromBreakpoint;

  closeCE: TcloseCE;
  hideAllCEWindows: ThideAllCEWindows;
  unhideMainCEwindow: TunhideMainCEwindow;
  createForm: TcreateForm;
  form_centerScreen: Tform_centerScreen;
  form_hide: Tform_hide;
  form_show: Tform_show;
  form_onClose: Tform_onClose;

  createPanel: TcreatePanel;
  createGroupBox: TcreateGroupBox;
  createButton: TcreateButton;
  createImage: TcreateImage;
  image_loadImageFromFile: Timage_loadImageFromFile;
  image_transparent: Timage_transparent;
  image_stretch: Timage_stretch;

  createLabel: TcreateLabel;
  createEdit: TcreateEdit;
  createMemo: TcreateMemo;
  createTimer: TcreateTimer;
  timer_setInterval: Ttimer_setInterval;
  timer_onTimer: Ttimer_onTimer;
  control_setCaption: Tcontrol_setCaption;
  control_getCaption: Tcontrol_getCaption;
  control_setPosition: Tcontrol_setPosition;
  control_getX: Tcontrol_getX;
  control_getY: Tcontrol_getY;
  control_setSize: Tcontrol_setSize;
  control_getWidth: Tcontrol_getWidth;
  control_getHeight: Tcontrol_getHeight;
  control_setAlign: Tcontrol_setAlign;
  control_onClick: Tcontrol_onClick;

  object_destroy: Tobject_destroy;
  messageDialog: TmessageDialog;
  speedhack_setSpeed: Tspeedhack_setSpeed;

  //version 5
  ExecuteKernelCode: TExecuteKernelCode;
  UserdefinedInterruptHook: TUserdefinedInterruptHook;
  GetLuaState: TGetLuaState;
  MainThreadCall: TMainThreadCall;


end;

type PExportedFunctions=^TExportedFunctions;

implementation

end.
