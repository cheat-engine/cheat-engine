unit plugin;

{$MODE Delphi}

interface

uses lclproc,
     {$ifdef darwin}
     macport, dynlibs,
     {$endif}
     {$ifdef windows}
     windows,
     {$endif}
     classes, sysutils,LCLIntf,checklst,menus,dialogs,CEFuncProc,
     NewKernelHandler, graphics, syncobjs, commonTypeDefs;

const CurrentPluginVersion=6;

//todo: Move the type definitions to a different unit

//structures
type TPluginVersion = record
  version: dword; //version number of ce plugin it is developed for (compatibility for the future)
  pluginname: pchar; //pointer to a 0-terminated string in the dll
end;
type PPluginVersion=^TPluginVersion;

type
  TPluginDotNetInitResult=packed record
    name: pchar;
    GetVersion: pointer;
    EnablePlugin: pointer;
    DisablePlugin: pointer;
    version: dword;

  end;

  TExportedFunctionsDotNetV1=record
    sizeofExportedFunctions: integer;
    GetLuaState: pointer;  //rest is kinda obsolete with lua, e.g hooking happens with AA scripts in the local CE
    LuaRegister: pointer;
    LuaPushClassInstance: pointer;
    ProcessMessages: pointer; //in case it's a messy plugin that wants to run in the main thread
    CheckSynchronize: pointer;
  end;

  TExportedFunctions5 = record
    sizeofExportedFunctions: integer;
    showmessage: pointer;
    registerfunction: pointer;
    unregisterfunction: pointer;
    OpenedProcessID: ^dword;
    OpenedProcessHandle: ^thandle;

    GetMainWindowHandle: pointer;
    AutoAssemble: pointer;
    assembler: pointer;
    disassembler: pointer;
    ChangeRegistersAtAddress: pointer;
    InjectDLL: pointer;
    freezemem: pointer;
    unfreezemem: pointer;
    fixmem: pointer;
    processlist: pointer;
    reloadsettings: pointer;
    getaddressfrompointer: pointer;

    //pointers to the address that contains the pointers to the functions
    ReadProcessMemory     :pointer;
    WriteProcessMemory    :pointer;
    GetThreadContext      :pointer;
    SetThreadContext      :pointer;
    SuspendThread         :pointer;
    ResumeThread          :pointer;
    OpenProcess           :pointer;
    WaitForDebugEvent     :pointer;
    ContinueDebugEvent    :pointer;
    DebugActiveProcess    :pointer;
    StopDebugging         :pointer;
    StopRegisterChange    :pointer;
    VirtualProtect        :pointer;
    VirtualProtectEx      :pointer;
    VirtualQueryEx        :pointer;
    VirtualAllocEx        :pointer;
    CreateRemoteThread    :pointer;
    OpenThread            :pointer;
    GetPEProcess          :pointer;
    GetPEThread           :pointer;
    GetThreadsProcessOffset:pointer;
    GetThreadListEntryOffset:pointer;
    GetProcessnameOffset  :pointer;
    GetDebugportOffset    :pointer;
    GetPhysicalAddress    :pointer;
    ProtectMe             :pointer;
    GetCR4                :pointer;
    GetCR3                :pointer;
    SetCR3                :pointer;
    GetSDT                :pointer;
    GetSDTShadow          :pointer;
    setAlternateDebugMethod: pointer;
    getAlternateDebugMethod: pointer;
    DebugProcess          :pointer;
    ChangeRegOnBP         :pointer;
    RetrieveDebugData     :pointer;
    StartProcessWatch     :pointer;
    WaitForProcessListData:pointer;
    GetProcessNameFromID  :pointer;
    GetProcessNameFromPEProcess:pointer;
    KernelOpenProcess       :pointer;
    KernelReadProcessMemory :pointer;
    KernelWriteProcessMemory:pointer;
    KernelVirtualAllocEx    :pointer;
    IsValidHandle           :pointer;
    GetIDTCurrentThread     :pointer;
    GetIDTs                 :pointer;
    MakeWritable            :pointer;
    GetLoadedState          :pointer;
    DBKSuspendThread        :pointer;
    DBKResumeThread         :pointer;
    DBKSuspendProcess       :pointer;
    DBKResumeProcess        :pointer;
    KernelAlloc             :pointer;
    GetKProcAddress         :pointer;
    CreateToolhelp32Snapshot:pointer;
    Process32First          :pointer;
    Process32Next           :pointer;
    Thread32First           :pointer;
    Thread32Next            :pointer;
    Module32First           :pointer;
    Module32Next            :pointer;
    Heap32ListFirst         :pointer;
    Heap32ListNext          :pointer;

    //advanced for delphi 7 enterprise dll programmers only
    mainform                :pointer;
    memorybrowser           :pointer;

    //version 2 extension:
    sym_nameToAddress         : pointer;
    sym_addressToName         : pointer;
    sym_generateAPIHookScript : pointer;

    //version 3 extension
    loadDBK32         : pointer;
    loaddbvmifneeded  : pointer;
    previousOpcode    : pointer;
    nextOpcode        : pointer;
    disassembleEx     : pointer;
    loadModule        : pointer;
    aa_AddExtraCommand: pointer;
    aa_RemoveExtraCommand: pointer;

    //version 4 extension
    createTableEntry: pointer;
    getTableEntry: pointer;
    memrec_setDescription: pointer;
    memrec_getDescription: pointer ;
    memrec_getAddress: pointer;
    memrec_setAddress: pointer;
    memrec_getType: pointer;
    memrec_setType: pointer;
    memrec_getValue: pointer;
    memrec_setValue: pointer;
    memrec_getScript: pointer;
    memrec_setScript: pointer;
    memrec_isfrozen: pointer;
    memrec_freeze: pointer;
    memrec_unfreeze: pointer;
    memrec_setColor: pointer;
    memrec_appendtoentry: pointer;
    memrec_delete: pointer;

    getProcessIDFromProcessName: pointer;
    openProcessEx: pointer;
    debugProcessEx: pointer;
    pause: pointer;
    unpause: pointer;

    debug_setBreakpoint: pointer;
    debug_removeBreakpoint: pointer;
    debug_continueFromBreakpoint: pointer;

    closeCE: pointer;
    hideAllCEWindows: pointer;
    unhideMainCEwindow: pointer;
    createForm: pointer;
    form_centerScreen: pointer;
    form_hide: pointer;
    form_show: pointer;
    form_onClose: pointer;

    createPanel: pointer;
    createGroupBox: pointer;
    createButton: pointer;
    createImage: pointer;
    image_loadImageFromFile: pointer;
    image_transparent: pointer;
    image_stretch: pointer;

    createLabel: pointer;
    createEdit: pointer;
    createMemo: pointer;
    createTimer: pointer;
    timer_setInterval: pointer;
    timer_onTimer: pointer;
    control_setCaption: pointer;
    control_getCaption: pointer;
    control_setPosition: pointer;
    control_getX: pointer;
    control_getY: pointer;
    control_setSize: pointer;
    control_getWidth: pointer;
    control_getHeight: pointer;
    control_setAlign: pointer;
    control_onClick: pointer;

    object_destroy: pointer;
    messageDialog: pointer;
    speedhack_setSpeed: pointer;

    //version 5
    ExecuteKernelCode: pointer;
    UserdefinedInterruptHook: pointer;
    GetLuaState: pointer;
    MainThreadCall: pointer;
  end;

type PExportedFunctions5 = ^TExportedFunctions5;

type TExportedFunctions = TExportedFunctions5; //<----adjust on new version


type TExportedFunctions4 = record
  sizeofExportedFunctions: integer;
  showmessage: pointer;
  registerfunction: pointer;
  unregisterfunction: pointer;
  OpenedProcessID: ^dword;
  OpenedProcessHandle: ^thandle;

  GetMainWindowHandle: pointer;
  AutoAssemble: pointer;
  assembler: pointer;
  disassembler: pointer;
  ChangeRegistersAtAddress: pointer;
  InjectDLL: pointer;
  freezemem: pointer;
  unfreezemem: pointer;
  fixmem: pointer;
  processlist: pointer;
  reloadsettings: pointer;
  getaddressfrompointer: pointer;

  //pointers to the address that contains the pointers to the functions
  ReadProcessMemory     :pointer;
  WriteProcessMemory    :pointer;
  GetThreadContext      :pointer;
  SetThreadContext      :pointer;
  SuspendThread         :pointer;
  ResumeThread          :pointer;
  OpenProcess           :pointer;
  WaitForDebugEvent     :pointer;
  ContinueDebugEvent    :pointer;
  DebugActiveProcess    :pointer;
  StopDebugging         :pointer;
  StopRegisterChange    :pointer;
  VirtualProtect        :pointer;
  VirtualProtectEx      :pointer;
  VirtualQueryEx        :pointer;
  VirtualAllocEx        :pointer;
  CreateRemoteThread    :pointer;
  OpenThread            :pointer;
  GetPEProcess          :pointer;
  GetPEThread           :pointer;
  GetThreadsProcessOffset:pointer;
  GetThreadListEntryOffset:pointer;
  GetProcessnameOffset  :pointer;
  GetDebugportOffset    :pointer;
  GetPhysicalAddress    :pointer;
  ProtectMe             :pointer;
  GetCR4                :pointer;
  GetCR3                :pointer;
  SetCR3                :pointer;
  GetSDT                :pointer;
  GetSDTShadow          :pointer;
  setAlternateDebugMethod: pointer;
  getAlternateDebugMethod: pointer;
  DebugProcess          :pointer;
  ChangeRegOnBP         :pointer;
  RetrieveDebugData     :pointer;
  StartProcessWatch     :pointer;
  WaitForProcessListData:pointer;
  GetProcessNameFromID  :pointer;
  GetProcessNameFromPEProcess:pointer;
  KernelOpenProcess       :pointer;
  KernelReadProcessMemory :pointer;
  KernelWriteProcessMemory:pointer;
  KernelVirtualAllocEx    :pointer;
  IsValidHandle           :pointer;
  GetIDTCurrentThread     :pointer;
  GetIDTs                 :pointer;
  MakeWritable            :pointer;
  GetLoadedState          :pointer;
  DBKSuspendThread        :pointer;
  DBKResumeThread         :pointer;
  DBKSuspendProcess       :pointer;
  DBKResumeProcess        :pointer;
  KernelAlloc             :pointer;
  GetKProcAddress         :pointer;
  CreateToolhelp32Snapshot:pointer;
  Process32First          :pointer;
  Process32Next           :pointer;
  Thread32First           :pointer;
  Thread32Next            :pointer;
  Module32First           :pointer;
  Module32Next            :pointer;
  Heap32ListFirst         :pointer;
  Heap32ListNext          :pointer;

  //advanced for delphi 7 enterprise dll programmers only
  mainform                :pointer;
  memorybrowser           :pointer;

  //version 2 extension:
  sym_nameToAddress         : pointer;
  sym_addressToName         : pointer;
  sym_generateAPIHookScript : pointer;

  //version 3 extension
  loadDBK32         : pointer;
  loaddbvmifneeded  : pointer;
  previousOpcode    : pointer;
  nextOpcode        : pointer;
  disassembleEx     : pointer;
  loadModule        : pointer;
  aa_AddExtraCommand: pointer;
  aa_RemoveExtraCommand: pointer;

  //version 4 extension
  createTableEntry: pointer;
  getTableEntry: pointer;
  memrec_setDescription: pointer;
  memrec_getDescription: pointer ;
  memrec_getAddress: pointer;
  memrec_setAddress: pointer;
  memrec_getType: pointer;
  memrec_setType: pointer;
  memrec_getValue: pointer;
  memrec_setValue: pointer;
  memrec_getScript: pointer;
  memrec_setScript: pointer;
  memrec_isfrozen: pointer;
  memrec_freeze: pointer;
  memrec_unfreeze: pointer;
  memrec_setColor: pointer;
  memrec_appendtoentry: pointer;
  memrec_delete: pointer;

  getProcessIDFromProcessName: pointer;
  openProcessEx: pointer;
  debugProcessEx: pointer;
  pause: pointer;
  unpause: pointer;

  debug_setBreakpoint: pointer;
  debug_removeBreakpoint: pointer;
  debug_continueFromBreakpoint: pointer;

  closeCE: pointer;
  hideAllCEWindows: pointer;
  unhideMainCEwindow: pointer;
  createForm: pointer;
  form_centerScreen: pointer;
  form_hide: pointer;
  form_show: pointer;
  form_onClose: pointer;

  createPanel: pointer;
  createGroupBox: pointer;
  createButton: pointer;
  createImage: pointer;
  image_loadImageFromFile: pointer;
  image_transparent: pointer;
  image_stretch: pointer;

  createLabel: pointer;
  createEdit: pointer;
  createMemo: pointer;
  createTimer: pointer;
  timer_setInterval: pointer;
  timer_onTimer: pointer;
  control_setCaption: pointer;
  control_getCaption: pointer;
  control_setPosition: pointer;
  control_getX: pointer;
  control_getY: pointer;
  control_setSize: pointer;
  control_getWidth: pointer;
  control_getHeight: pointer;
  control_setAlign: pointer;
  control_onClick: pointer;

  object_destroy: pointer;
  messageDialog: pointer;
  speedhack_setSpeed: pointer;

end;
type PExportedFunctions4 = ^TExportedFunctions4;

type TExportedFunctions3 = record
  sizeofExportedFunctions: integer;
  showmessage: pointer;
  registerfunction: pointer;
  unregisterfunction: pointer;
  OpenedProcessID: ^dword;
  OpenedProcessHandle: ^thandle;

  GetMainWindowHandle: pointer;
  AutoAssemble: pointer;
  assembler: pointer;
  disassembler: pointer;
  ChangeRegistersAtAddress: pointer;
  InjectDLL: pointer;
  freezemem: pointer;
  unfreezemem: pointer;
  fixmem: pointer;
  processlist: pointer;
  reloadsettings: pointer;
  getaddressfrompointer: pointer;

  //pointers to the address that contains the pointers to the functions
  ReadProcessMemory     :pointer;
  WriteProcessMemory    :pointer;
  GetThreadContext      :pointer;
  SetThreadContext      :pointer;
  SuspendThread         :pointer;
  ResumeThread          :pointer;
  OpenProcess           :pointer;
  WaitForDebugEvent     :pointer;
  ContinueDebugEvent    :pointer;
  DebugActiveProcess    :pointer;
  StopDebugging         :pointer;
  StopRegisterChange    :pointer;
  VirtualProtect        :pointer;
  VirtualProtectEx      :pointer;
  VirtualQueryEx        :pointer;
  VirtualAllocEx        :pointer;
  CreateRemoteThread    :pointer;
  OpenThread            :pointer;
  GetPEProcess          :pointer;
  GetPEThread           :pointer;
  GetThreadsProcessOffset:pointer;
  GetThreadListEntryOffset:pointer;
  GetProcessnameOffset  :pointer;
  GetDebugportOffset    :pointer;
  GetPhysicalAddress    :pointer;
  ProtectMe             :pointer;
  GetCR4                :pointer;
  GetCR3                :pointer;
  SetCR3                :pointer;
  GetSDT                :pointer;
  GetSDTShadow          :pointer;
  setAlternateDebugMethod: pointer;
  getAlternateDebugMethod: pointer;
  DebugProcess          :pointer;
  ChangeRegOnBP         :pointer;
  RetrieveDebugData     :pointer;
  StartProcessWatch     :pointer;
  WaitForProcessListData:pointer;
  GetProcessNameFromID  :pointer;
  GetProcessNameFromPEProcess:pointer;
  KernelOpenProcess       :pointer;
  KernelReadProcessMemory :pointer;
  KernelWriteProcessMemory:pointer;
  KernelVirtualAllocEx    :pointer;
  IsValidHandle           :pointer;
  GetIDTCurrentThread     :pointer;
  GetIDTs                 :pointer;
  MakeWritable            :pointer;
  GetLoadedState          :pointer;
  DBKSuspendThread        :pointer;
  DBKResumeThread         :pointer;
  DBKSuspendProcess       :pointer;
  DBKResumeProcess        :pointer;
  KernelAlloc             :pointer;
  GetKProcAddress         :pointer;
  CreateToolhelp32Snapshot:pointer;
  Process32First          :pointer;
  Process32Next           :pointer;
  Thread32First           :pointer;
  Thread32Next            :pointer;
  Module32First           :pointer;
  Module32Next            :pointer;
  Heap32ListFirst         :pointer;
  Heap32ListNext          :pointer;

  //advanced for delphi 7 enterprise dll programmers only
  mainform                :pointer;
  memorybrowser           :pointer;

  //version 2 extension:
  sym_nameToAddress         : pointer;
  sym_addressToName         : pointer;
  sym_generateAPIHookScript : pointer;

  //version 3 extension
  loadDBK32         : pointer;
  loaddbvmifneeded  : pointer;
  previousOpcode    : pointer;
  nextOpcode        : pointer;
  disassembleEx     : pointer;
  loadModule        : pointer;
  aa_AddExtraCommand: pointer;
  aa_RemoveExtraCommand: pointer;




end;
type PExportedFunctions3 = ^TExportedFunctions3;

type TExportedFunctions2 = record
  sizeofExportedFunctions: integer;
  showmessage: pointer;
  registerfunction: pointer;
  unregisterfunction: pointer;
  OpenedProcessID: ^dword;
  OpenedProcessHandle: ^thandle;

  GetMainWindowHandle: pointer;
  AutoAssemble: pointer;
  assembler: pointer;
  disassembler: pointer;
  ChangeRegistersAtAddress: pointer;
  InjectDLL: pointer;
  freezemem: pointer;
  unfreezemem: pointer;
  fixmem: pointer;
  processlist: pointer;
  reloadsettings: pointer;
  getaddressfrompointer: pointer;

  //pointers to the address that contains the pointers to the functions
  ReadProcessMemory     :pointer;
  WriteProcessMemory    :pointer;
  GetThreadContext      :pointer;
  SetThreadContext      :pointer;
  SuspendThread         :pointer;
  ResumeThread          :pointer;
  OpenProcess           :pointer;
  WaitForDebugEvent     :pointer;
  ContinueDebugEvent    :pointer;
  DebugActiveProcess    :pointer;
  StopDebugging         :pointer;
  StopRegisterChange    :pointer;
  VirtualProtect        :pointer;
  VirtualProtectEx      :pointer;
  VirtualQueryEx        :pointer;
  VirtualAllocEx        :pointer;
  CreateRemoteThread    :pointer;
  OpenThread            :pointer;
  GetPEProcess          :pointer;
  GetPEThread           :pointer;
  GetThreadsProcessOffset:pointer;
  GetThreadListEntryOffset:pointer;
  GetProcessnameOffset  :pointer;
  GetDebugportOffset    :pointer;
  GetPhysicalAddress    :pointer;
  ProtectMe             :pointer;
  GetCR4                :pointer;
  GetCR3                :pointer;
  SetCR3                :pointer;
  GetSDT                :pointer;
  GetSDTShadow          :pointer;
  setAlternateDebugMethod: pointer;
  getAlternateDebugMethod: pointer;
  DebugProcess          :pointer;
  ChangeRegOnBP         :pointer;
  RetrieveDebugData     :pointer;
  StartProcessWatch     :pointer;
  WaitForProcessListData:pointer;
  GetProcessNameFromID  :pointer;
  GetProcessNameFromPEProcess:pointer;
  KernelOpenProcess       :pointer;
  KernelReadProcessMemory :pointer;
  KernelWriteProcessMemory:pointer;
  KernelVirtualAllocEx    :pointer;
  IsValidHandle           :pointer;
  GetIDTCurrentThread     :pointer;
  GetIDTs                 :pointer;
  MakeWritable            :pointer;
  GetLoadedState          :pointer;
  DBKSuspendThread        :pointer;
  DBKResumeThread         :pointer;
  DBKSuspendProcess       :pointer;
  DBKResumeProcess        :pointer;
  KernelAlloc             :pointer;
  GetKProcAddress         :pointer;
  CreateToolhelp32Snapshot:pointer;
  Process32First          :pointer;
  Process32Next           :pointer;
  Thread32First           :pointer;
  Thread32Next            :pointer;
  Module32First           :pointer;
  Module32Next            :pointer;
  Heap32ListFirst         :pointer;
  Heap32ListNext          :pointer;

  //advanced for delphi 7 enterprise dll programmers only
  mainform                :pointer;
  memorybrowser           :pointer;

  //version 2 extension:
  sym_nameToAddress         : pointer;
  sym_addressToName         : pointer;
  sym_generateAPIHookScript : pointer;
end;
type PExportedFunctions2 = ^TExportedFunctions2;

type TExportedFunctions1 = record
  sizeofExportedFunctions: integer;
  showmessage: pointer;
  registerfunction: pointer;
  unregisterfunction: pointer;
  OpenedProcessID: ^dword;
  OpenedProcessHandle: ^thandle;

  GetMainWindowHandle: pointer;
  AutoAssemble: pointer;
  assembler: pointer;
  disassembler: pointer;
  ChangeRegistersAtAddress: pointer;
  InjectDLL: pointer;
  freezemem: pointer;
  unfreezemem: pointer;
  fixmem: pointer;
  processlist: pointer;
  reloadsettings: pointer;
  getaddressfrompointer: pointer;

  //pointers to the address that contains the pointers to the functions
  ReadProcessMemory     :pointer;
  WriteProcessMemory    :pointer;
  GetThreadContext      :pointer;
  SetThreadContext      :pointer;
  SuspendThread         :pointer;
  ResumeThread          :pointer;
  OpenProcess           :pointer;
  WaitForDebugEvent     :pointer;
  ContinueDebugEvent    :pointer;
  DebugActiveProcess    :pointer;
  StopDebugging         :pointer;
  StopRegisterChange    :pointer;
  VirtualProtect        :pointer;
  VirtualProtectEx      :pointer;
  VirtualQueryEx        :pointer;
  VirtualAllocEx        :pointer;
  CreateRemoteThread    :pointer;
  OpenThread            :pointer;
  GetPEProcess          :pointer;
  GetPEThread           :pointer;
  GetThreadsProcessOffset:pointer;
  GetThreadListEntryOffset:pointer;
  GetProcessnameOffset  :pointer;
  GetDebugportOffset    :pointer;
  GetPhysicalAddress    :pointer;
  ProtectMe             :pointer;
  GetCR4                :pointer;
  GetCR3                :pointer;
  SetCR3                :pointer;
  GetSDT                :pointer;
  GetSDTShadow          :pointer;
  setAlternateDebugMethod: pointer;
  getAlternateDebugMethod: pointer;
  DebugProcess          :pointer;
  ChangeRegOnBP         :pointer;
  RetrieveDebugData     :pointer;
  StartProcessWatch     :pointer;
  WaitForProcessListData:pointer;
  GetProcessNameFromID  :pointer;
  GetProcessNameFromPEProcess:pointer;
  KernelOpenProcess       :pointer;
  KernelReadProcessMemory :pointer;
  KernelWriteProcessMemory:pointer;
  KernelVirtualAllocEx    :pointer;
  IsValidHandle           :pointer;
  GetIDTCurrentThread     :pointer;
  GetIDTs                 :pointer;
  MakeWritable            :pointer;
  GetLoadedState          :pointer;
  DBKSuspendThread        :pointer;
  DBKResumeThread         :pointer;
  DBKSuspendProcess       :pointer;
  DBKResumeProcess        :pointer;
  KernelAlloc             :pointer;
  GetKProcAddress         :pointer;
  CreateToolhelp32Snapshot:pointer;
  Process32First          :pointer;
  Process32Next           :pointer;
  Thread32First           :pointer;
  Thread32Next            :pointer;
  Module32First           :pointer;
  Module32Next            :pointer;
  Heap32ListFirst         :pointer;
  Heap32ListNext          :pointer;

  //advanced for delphi 7 enterprise dll programmers only
  mainform                :pointer;
  memorybrowser           :pointer;
  previousOpcode          :pointer;
end;
type PExportedFunctions1 = ^TExportedFunctions1;


type PExportedFunctions = ^TExportedFunctions; //same


//exported functions of the plugin
type TGetVersion=function(var PluginVersion:TPluginVersion; TPluginVersionSize: integer):BOOL; stdcall;
type TInitializePlugin=function(var ExportedFunctions: TExportedFunctions; pluginid: dword):BOOL; stdcall;
type TDisablePlugin=function:BOOL; stdcall;


//plugin type 0:
//where: rightclick on the address list in the menu plugin, user activated
type TPlugin0_SelectedRecord=record
  interpretedaddress: pchar; //pointer to a 255 bytes long string (0 terminated)
  address: ptrUint; //this is a read-only representaion of the address. Change interpretedaddress if you want to change this
  ispointer: BOOL; //readonly
  countoffsets: integer; //readonly
  offsets: PDWordArray; //pointer to a array of dwords randing from 0 to countoffsets-1 (readonly)
  description: pchar; //pointer to a 255 bytes long string
  valuetype: byte;
  size: byte; //stringlenth or bitlength (max 255);
end;
type PPlugin0_SelectedRecord=^TPlugin0_SelectedRecord;
type TPluginfunction0=function(selectedrecord: PPlugin0_SelectedRecord):bool; stdcall;

//private plugin data
type TPluginfunctionType0=class
  public
    pluginid: integer;
    functionid: integer;
    name:string;
    callback: TPluginfunction0;
    menuitem: TMenuItem;
end;

//plugin type 1:
//where: menu bar under plugins in memory view, user activated
type TPluginfunction1=function(disassembleraddress: pptruint; selected_disassembler_address: pptruint; hexviewaddress:pptruint ):bool; stdcall;

//private plugin data
type TPluginfunctionType1=class
  public
    pluginid: integer;
    functionid: integer;
    name:string;
    callback: TPluginfunction1;
    menuitem: TMenuItem;
end;


//plugin type 2:
//where: when a debug event happens
type TPluginFunction2=function(debugevent: PDebugEvent):integer; stdcall; //return 0 if you want to let ce handle it as well, 1 if you don't want to let ce handle it as well  (in case of not handling, do ContinueDebugEvent yourself)
type TPluginfunctionType2=class
  public
    pluginid: integer;
    functionid: integer;
    callback: TPluginFunction2;
end;

//plugin type 3:
//where: a new process created according to the processwatcher
type TPluginFunction3=function(processid: dword; peprocess:ptruint; created: BOOL):integer; stdcall;
type TPluginFunction3Version1=function(processid: dword; peprocess:ptruint):integer; stdcall;
type TPluginfunctionType3=class
  public
    pluginid: integer;
    functionid: integer;
    callback: TPluginfunction3;
end;

//plugin type 4:
//where: Functionpointerchange notification
type TPluginFunction4=function(section: integer):boolean; stdcall;
type TPluginfunctionType4=class
  public
    pluginid: integer;
    functionid: integer;
    callback: TPluginFunction4;
end;

//plugin type 5:
//where: Main form's menu, plugin
type TPluginfunction5=procedure; stdcall;

//private plugin data
type TPluginfunctionType5=class
  public
    pluginid: integer;
    functionid: integer;
    name:string;
    callback: TPluginfunction5;
    menuitem: TMenuItem;
end;


//plugin type 6:
//where: rightclick context of the disassembler
type TPluginfunction6=function(selectedAddress: pptruint):bool; stdcall;
type Tpluginfuntion6OnContext=function(selectedAddress: ptruint; addressofname: pointer; show: pbool):bool; stdcall;
type Tpluginfuntion6OnContextVersion5=function(selectedAddress: ptruint; addressofname: pointer):bool; stdcall;

//private plugin data
type TPluginfunctionType6=class
  public
    pluginid: integer;
    functionid: integer;
    name:string;
    callback: TPluginfunction6;
    callbackOnContext: Tpluginfuntion6OnContext;
    menuitem: TMenuItem;
end;

//plugin type 7:
//where: when a disassembler line is being rendered
type TPluginFunction7=procedure(address: ptruint; addressStringPointer: pointer; bytestringpointer: pointer; opcodestringpointer: pointer; specialstringpointer: pointer; textcolor: PColor); stdcall;
type TPluginfunctionType7=class
  public
    pluginid: integer;
    functionid: integer;
    callback: TPluginFunction7;
end;

//plugin type 8
//where: when the autoassembler is used in the first and 2nd stage
type TPluginFunction8=procedure(line: ppchar; phase: integer; id: integer); stdcall;
type TPluginFunction8Version5=procedure(line: ppchar; phase: integer); stdcall;

type TPluginfunctionType8=class
  public
    pluginid: integer;
    functionid: integer;
    callback: TPluginFunction8;
end;


type TPlugin = record
  dllname: string;
  filepath: string;
  hmodule: thandle;
  name: string;
  pluginversion: integer;
  dotnet: boolean;
  enabled: boolean;
  GetVersion: TGetVersion;
  EnablePlugin: TInitializePlugin;
  DisablePlugin: TDisablePlugin;
  nextid: integer;
  RegisteredFunctions0: array of TPluginfunctionType0;
  RegisteredFunctions1: array of TPluginfunctionType1;
  RegisteredFunctions2: array of TPluginfunctionType2;
  RegisteredFunctions3: array of TPluginfunctionType3;
  RegisteredFunctions4: array of TPluginfunctionType4;
  RegisteredFunctions5: array of TPluginfunctionType5;
  RegisteredFunctions6: array of TPluginfunctionType6;
  RegisteredFunctions7: array of TPluginfunctionType7;
  RegisteredFunctions8: array of TPluginfunctionType8;
end;


type TPluginHandler=class
  private
    pluginCS: TCriticalSection;
    plugins: array of TPlugin;
    function GetDLLFilePath(pluginid: integer):string;
    function DotNetPluginGetPluginName(dllname: string): string;
    function DotNetLoadPlugin(dllname:string):integer;
  public
    function GetPluginID(dllname:string):integer;
    function GetPluginName(dllname:string):string;
    function LoadPlugin(dllname: string):integer;
    procedure UnloadPlugin(pluginid: integer);
    procedure FillCheckListBox(clb: TCheckListbox);
    procedure EnablePlugin(pluginid: integer);
    procedure DisablePlugin(pluginid: integer);
    procedure handleAutoAssemblerPlugin(line: ppchar; phase: integer; id: integer);
    procedure handledisassemblerContextPopup(address: ptruint);
    procedure handledisassemblerplugins(address: ptruint; addressStringPointer: pointer; bytestringpointer: pointer; opcodestringpointer: pointer; specialstringpointer: pointer; textcolor: PColor);
    function handledebuggerplugins(devent:PDebugEvent):integer;
    function handlenewprocessplugins(processid: dword; peprocess:ptruint; created: boolean):boolean;
    function handlechangedpointers(section: integer):boolean;
    function registerfunction(pluginid,functiontype:integer; init: pointer):integer;
    function unregisterfunction(pluginid,functionid: integer): boolean;
    property dllfilepath[pluginid: integer]: string read getdllfilepath;
    constructor create;
    destructor destroy; override;
end;




var pluginhandler: TPluginhandler;
    exportedfunctions: TExportedFunctions;
    exportedfunctionsdotnet: TExportedFunctionsDotNetV1;


    onAPIPointerChange: TNotifyEvent;



implementation

uses MainUnit,memorybrowserformunit,formsettingsunit, pluginexports, SynHighlighterAA,
     {$ifdef windows}DBK32functions,{$endif} luahandler, processhandlerunit
     {$ifdef windows}, BetterDLLSearchPath{$endif}, dotnethost, PEInfoFunctions, luaclass;

resourcestring
  rsErrorEnabling = 'Error enabling %s';
  rsErrorDisabling = 'Error disabling %s';
  rsErrorLoadingOnlyDLLFilesAreAllowed = 'Error loading %s. Only DLL files are allowed';
  rsErrorLoadingTheDllIsMissingTheCEPlugin_GetVersionF = 'Error loading %s. The dll is missing the CEPlugin_GetVersion function';
  rsIsMissingTheCEPlugin_InitializePluginExport = '%s is missing the CEPlugin_InitializePlugin export';
  rsIsMissingTheCEPlugin_DisablePluginExport = '%s is missing the CEPlugin_DisablePlugin export';
  rsErrorLoadingTheDllIsMissingTheCEPlugin_GetVersionE = 'Error loading %s. The dll is missing the CEPlugin_GetVersion export';
  rsErrorLoadingThisDllRequiresANewerVersionOfCeToFunc = 'Error loading %s. This dll requires a newer version of ce to function properly';
  rsErrorLoadingTheGetVersionFunctionReturnedFALSE = 'Error loading %s. The GetVersion function returned FALSE';
  rsPlugThePluginDllCouldNotBeLoaded = 'The plugin dll could not be loaded:';

function TPluginHandler.GetDLLFilePath(pluginid: integer):string;
begin
  pluginCS.Enter;
  result:=plugins[pluginid].filepath;
  pluginCS.Leave;
end;

function TPluginHandler.registerfunction(pluginid,functiontype:integer; init: pointer):integer;
type Tfunction0=record
  name: pchar;
  callbackroutine: pointer;
end;
type Tfunction1=record
  name: pchar;
  callbackroutine: pointer;
  shortcut: pchar;
end;
type TFunction2=record
  callbackroutine: pointer;
end;
type Tfunction6=record
  name: pchar;
  callbackroutine: pointer;
  callbackroutineOnContext: pointer;
  shortcut: pchar;
end;
type PFunction0=^TFunction0;
type Pfunction1=^TFunction1;   //same
type Pfunction2=^TFunction2;
type PFunction3=^TFunction2;
type PFunction4=^TFunction2;
type PFunction5=^TFunction1;
type PFunction6=^TFunction6;
type PFunction7=^TFunction2;
type PFunction8=^TFunction2;

var i: integer;
    newmenuitem: TMenuItem;
    f0: TPluginfunctionType0;
    f1: TPluginfunctionType1;
    f2: TPluginfunctionType2;
    f3: TPluginfunctionType3;
    f4: TPluginfunctionType4;
    f5: TPluginfunctionType5;
    f6: TPluginfunctionType6;
    f7: TPluginfunctionType7;
    f8: TPluginfunctionType8;
begin
  result:=-1;


  pluginCS.enter;
  try
    if pluginid>=length(plugins) then exit;

    case functiontype of
      0: begin
           //plugin for the rightclick on the addresslist
           f0:=TPluginfunctionType0.Create;
           f0.pluginid:=pluginid;
           f0.functionid:=plugins[pluginid].nextid;
           f0.name:=PFunction0(init).name;
           f0.callback:=PFunction0(init).callbackroutine;

           if not mainform.Plugins1.Visible then
             mainform.Plugins1.Visible:=true;

           newmenuitem:=tmenuitem.Create(mainform);
           newmenuitem.Caption:=f0.name;
           newmenuitem.Tag:=ptruint(f0);
           newmenuitem.onclick:=mainform.plugintype0click;
           mainform.Plugins1.Add(newmenuitem);

           f0.menuitem:=newmenuitem;

           setlength(plugins[pluginid].RegisteredFunctions0,length(plugins[pluginid].RegisteredFunctions0)+1);
           plugins[pluginid].RegisteredFunctions0[length(plugins[pluginid].RegisteredFunctions0)-1]:=f0;

           result:=plugins[pluginid].nextid;
         end;

      1: begin
           //plugin for the memorybrowser
           f1:=TPluginfunctionType1.Create;
           f1.pluginid:=pluginid;
           f1.functionid:=plugins[pluginid].nextid;
           f1.name:=Pfunction1(init).name;
           f1.callback:=Pfunction1(init).callbackroutine;

           if not memorybrowser.Plugins1.Visible then
             memorybrowser.Plugins1.Visible:=true;

           newmenuitem:=tmenuitem.Create(mainform);
           newmenuitem.Caption:=f1.name;
           newmenuitem.Tag:=ptruint(f1);
           newmenuitem.onclick:=memorybrowser.plugintype1click;

           if plugins[pluginid].pluginversion>1 then
           begin
             try

               newmenuitem.ShortCut:=TextToShortCut(PFunction1(init).shortcut);
             except

             end;
           end;           
           memorybrowser.Plugins1.Add(newmenuitem);

           f1.menuitem:=newmenuitem;

           setlength(plugins[pluginid].Registeredfunctions1,length(plugins[pluginid].Registeredfunctions1)+1);
           plugins[pluginid].Registeredfunctions1[length(plugins[pluginid].Registeredfunctions1)-1]:=f1;

           result:=plugins[pluginid].nextid;
         end;

      2: begin
           //debugger for the memorybrowser
           f2:=TPluginfunctionType2.Create;
           f2.pluginid:=pluginid;
           f2.functionid:=plugins[pluginid].nextid;
           f2.callback:=Pfunction2(init).callbackroutine;

           setlength(plugins[pluginid].RegisteredFunctions2,length(plugins[pluginid].RegisteredFunctions2)+1);
           plugins[pluginid].RegisteredFunctions2[length(plugins[pluginid].RegisteredFunctions2)-1]:=f2;

           result:=plugins[pluginid].nextid;
         end;

      3: begin
           //process created
           f3:=TPluginfunctionType3.Create;
           f3.pluginid:=pluginid;
           f3.functionid:=plugins[pluginid].nextid;
           f3.callback:=Pfunction2(init).callbackroutine;

           setlength(plugins[pluginid].Registeredfunctions3, length(plugins[pluginid].Registeredfunctions3)+1);
           plugins[pluginid].Registeredfunctions3[length(plugins[pluginid].Registeredfunctions3)-1]:=f3;

           result:=plugins[pluginid].nextid;
         end;

      4: begin
           //function pointers changed
           f4:=TPluginfunctionType4.Create;
           f4.pluginid:=pluginid;
           f4.functionid:=plugins[pluginid].nextid;
           f4.callback:=Pfunction2(init).callbackroutine;

           setlength(plugins[pluginid].RegisteredFunctions4, length(plugins[pluginid].Registeredfunctions4)+1);
           plugins[pluginid].Registeredfunctions4[length(plugins[pluginid].Registeredfunctions4)-1]:=f4;

           result:=plugins[pluginid].nextid;
         end;

      5: begin
           //main menu
           f5:=TPluginfunctionType5.Create;
           f5.pluginid:=pluginid;
           f5.functionid:=plugins[pluginid].nextid;
           f5.name:=Pfunction5(init).name;
           f5.callback:=Pfunction5(init).callbackroutine;

           if not mainform.Plugins2.Visible then
             mainform.Plugins2.Visible:=true;

           newmenuitem:=tmenuitem.Create(mainform);
           newmenuitem.Caption:=f5.name;
           newmenuitem.Tag:=ptruint(f5);
           newmenuitem.onclick:=mainform.plugintype5click;

           try
             newmenuitem.ShortCut:=TextToShortCut(PFunction5(init).shortcut);
           except

           end;

           mainform.Plugins2.Add(newmenuitem);

           f5.menuitem:=newmenuitem;

           setlength(plugins[pluginid].Registeredfunctions5,length(plugins[pluginid].Registeredfunctions5)+1);
           plugins[pluginid].Registeredfunctions5[length(plugins[pluginid].Registeredfunctions5)-1]:=f5;

           result:=plugins[pluginid].nextid;




         end;

      6: begin
           //memorybrowser rightclick on disassembler
           f6:=TPluginfunctionType6.Create;
           f6.pluginid:=pluginid;
           f6.functionid:=plugins[pluginid].nextid;
           f6.name:=Pfunction6(init).name;
           f6.callback:=Pfunction6(init).callbackroutine;
           f6.callbackOnContext:=Pfunction6(init).callbackroutineOnContext;

           newmenuitem:=tmenuitem.Create(memorybrowser);
           newmenuitem.Caption:=f6.name;
           newmenuitem.Tag:=ptruint(f6);
           newmenuitem.onclick:=memorybrowser.plugintype6click;

           memorybrowser.debuggerpopup.Items.Add(newmenuitem);
           try
             newmenuitem.ShortCut:=TextToShortCut(PFunction6(init).shortcut);
           except

           end;

           f6.menuitem:=newmenuitem;

           setlength(plugins[pluginid].Registeredfunctions6,length(plugins[pluginid].Registeredfunctions6)+1);
           plugins[pluginid].Registeredfunctions6[length(plugins[pluginid].Registeredfunctions6)-1]:=f6;

           result:=plugins[pluginid].nextid;


         end;

      7: begin
           //disassemblerlines render
           f7:=TPluginfunctionType7.Create;
           f7.pluginid:=pluginid;
           f7.functionid:=plugins[pluginid].nextid;
           f7.callback:=Pfunction7(init).callbackroutine;

           setlength(plugins[pluginid].RegisteredFunctions7,length(plugins[pluginid].RegisteredFunctions7)+1);
           plugins[pluginid].RegisteredFunctions7[length(plugins[pluginid].RegisteredFunctions7)-1]:=f7;

           result:=plugins[pluginid].nextid;
         end;

      8: begin
           //autoassembler
           f8:=TPluginfunctionType8.Create;
           f8.pluginid:=pluginid;
           f8.functionid:=plugins[pluginid].nextid;
           f8.callback:=Pfunction8(init).callbackroutine;

           setlength(plugins[pluginid].RegisteredFunctions8,length(plugins[pluginid].RegisteredFunctions8)+1);
           plugins[pluginid].RegisteredFunctions8[length(plugins[pluginid].RegisteredFunctions8)-1]:=f8;

           result:=plugins[pluginid].nextid;
         end;


    end;

    inc(plugins[pluginid].nextid);
  finally
    pluginCS.leave;
  end;
end;

function TPluginHandler.unregisterfunction(pluginid,functionid: integer): boolean;
var i,j: integer;
    f: ^TPluginfunctionType0;
begin
  //remove it
  result:=false;
  pluginCS.enter;
  try
    if pluginid>=length(plugins) then exit;

    //function0 check
    for i:=0 to length(plugins[pluginid].RegisteredFunctions0)-1 do
      if plugins[pluginid].RegisteredFunctions0[i].functionid=functionid then
      begin
        if plugins[pluginid].RegisteredFunctions0[i].menuitem.Parent<>nil then
        begin
          if plugins[pluginid].RegisteredFunctions0[i].menuitem.Parent.Count=1 then
            plugins[pluginid].RegisteredFunctions0[i].menuitem.Parent.Visible:=false;
        end;
        plugins[pluginid].RegisteredFunctions0[i].menuitem.Free;
        plugins[pluginid].RegisteredFunctions0[i].Free;

        for j:=i to length(plugins[pluginid].RegisteredFunctions0)-2 do
          plugins[pluginid].RegisteredFunctions0[j]:=plugins[pluginid].RegisteredFunctions0[j+1];

        setlength(plugins[pluginid].RegisteredFunctions0,length(plugins[pluginid].RegisteredFunctions0)-1);

        result:=true;
        exit;
      end;

    //function1 check
    for i:=0 to length(plugins[pluginid].RegisteredFunctions1)-1 do
      if plugins[pluginid].RegisteredFunctions1[i].functionid=functionid then
      begin
        if plugins[pluginid].RegisteredFunctions1[i].menuitem.Parent<>nil then
        begin
          if plugins[pluginid].RegisteredFunctions1[i].menuitem.Parent.Count=1 then
            plugins[pluginid].RegisteredFunctions1[i].menuitem.Parent.Visible:=false;
        end;
        plugins[pluginid].RegisteredFunctions1[i].menuitem.Free;
        plugins[pluginid].RegisteredFunctions1[i].Free;

        for j:=i to length(plugins[pluginid].RegisteredFunctions1)-2 do
          plugins[pluginid].RegisteredFunctions1[j]:=plugins[pluginid].RegisteredFunctions1[j+1];

        setlength(plugins[pluginid].RegisteredFunctions1,length(plugins[pluginid].RegisteredFunctions1)-1);

        result:=true;
        exit;
      end;

    //function2 check
    for i:=0 to length(plugins[pluginid].RegisteredFunctions2)-1 do
      if plugins[pluginid].RegisteredFunctions2[i].functionid=functionid then
      begin
        plugins[pluginid].RegisteredFunctions2[i].Free;

        for j:=i to length(plugins[pluginid].RegisteredFunctions2)-2 do
          plugins[pluginid].RegisteredFunctions2[j]:=plugins[pluginid].RegisteredFunctions2[j+1];

        setlength(plugins[pluginid].RegisteredFunctions2,length(plugins[pluginid].RegisteredFunctions2)-1);

        result:=true;
        exit;
      end;

    //function3 check (processwatcher)
    for i:=0 to length(plugins[pluginid].Registeredfunctions3)-1 do
      if plugins[pluginid].Registeredfunctions3[i].functionid=functionid then
      begin
        plugins[pluginid].Registeredfunctions3[i].Free;

        for j:=i to length(plugins[pluginid].Registeredfunctions3)-2 do
          plugins[pluginid].Registeredfunctions3[j]:=plugins[pluginid].Registeredfunctions3[j+1];

        setlength(plugins[pluginid].Registeredfunctions3,length(plugins[pluginid].Registeredfunctions3)-1);

        result:=true;
        exit;
      end;

    //function4 check (changed pointers)
    for i:=0 to length(plugins[pluginid].RegisteredFunctions4)-1 do
      if plugins[pluginid].RegisteredFunctions4[i].functionid=functionid then
      begin
        plugins[pluginid].RegisteredFunctions4[i].Free;

        for j:=i to length(plugins[pluginid].RegisteredFunctions4)-2 do
          plugins[pluginid].RegisteredFunctions4[j]:=plugins[pluginid].RegisteredFunctions4[j+1];

        setlength(plugins[pluginid].RegisteredFunctions4,length(plugins[pluginid].RegisteredFunctions4)-1);

        result:=true;
        exit;
      end;

    //function5 check
    for i:=0 to length(plugins[pluginid].RegisteredFunctions5)-1 do
      if plugins[pluginid].RegisteredFunctions5[i].functionid=functionid then
      begin
        if plugins[pluginid].RegisteredFunctions5[i].menuitem.Parent<>nil then
        begin
          if plugins[pluginid].RegisteredFunctions5[i].menuitem.Parent.Count=1 then
            plugins[pluginid].RegisteredFunctions5[i].menuitem.Parent.Visible:=false;
        end;
        plugins[pluginid].RegisteredFunctions5[i].menuitem.Free;
        plugins[pluginid].RegisteredFunctions5[i].Free;

        for j:=i to length(plugins[pluginid].RegisteredFunctions5)-2 do
          plugins[pluginid].RegisteredFunctions5[j]:=plugins[pluginid].RegisteredFunctions5[j+1];

        setlength(plugins[pluginid].RegisteredFunctions5,length(plugins[pluginid].RegisteredFunctions5)-1);

        result:=true;
        exit;
      end;

    //function6 check
    for i:=0 to length(plugins[pluginid].RegisteredFunctions6)-1 do
      if plugins[pluginid].RegisteredFunctions6[i].functionid=functionid then
      begin
        if plugins[pluginid].RegisteredFunctions6[i].menuitem.Parent<>nil then
        begin
          if plugins[pluginid].RegisteredFunctions6[i].menuitem.Parent.Count=1 then
            plugins[pluginid].RegisteredFunctions6[i].menuitem.Parent.Visible:=false;
        end;
        plugins[pluginid].RegisteredFunctions6[i].menuitem.Free;
        plugins[pluginid].RegisteredFunctions6[i].Free;

        for j:=i to length(plugins[pluginid].RegisteredFunctions6)-2 do
          plugins[pluginid].RegisteredFunctions6[j]:=plugins[pluginid].RegisteredFunctions6[j+1];

        setlength(plugins[pluginid].RegisteredFunctions6,length(plugins[pluginid].RegisteredFunctions6)-1);

        result:=true;
        exit;
      end;

    //function7 check
    for i:=0 to length(plugins[pluginid].RegisteredFunctions7)-1 do
      if plugins[pluginid].RegisteredFunctions7[i].functionid=functionid then
      begin
        plugins[pluginid].RegisteredFunctions7[i].Free;

        for j:=i to length(plugins[pluginid].RegisteredFunctions7)-2 do
          plugins[pluginid].RegisteredFunctions7[j]:=plugins[pluginid].RegisteredFunctions7[j+1];

        setlength(plugins[pluginid].RegisteredFunctions7,length(plugins[pluginid].RegisteredFunctions7)-1);

        result:=true;
        exit;
      end;

    //function8 check
    for i:=0 to length(plugins[pluginid].RegisteredFunctions8)-1 do
      if plugins[pluginid].RegisteredFunctions8[i].functionid=functionid then
      begin
        plugins[pluginid].RegisteredFunctions8[i].Free;

        for j:=i to length(plugins[pluginid].RegisteredFunctions8)-2 do
          plugins[pluginid].RegisteredFunctions8[j]:=plugins[pluginid].RegisteredFunctions8[j+1];

        setlength(plugins[pluginid].RegisteredFunctions8,length(plugins[pluginid].RegisteredFunctions8)-1);

        result:=true;
        exit;
      end;


  finally
    pluginCS.leave;
  end;
end;

procedure TPluginHandler.EnablePlugin(pluginid: integer);
var e: texportedfunctions;
    enet: TExportedFunctionsDotNetV1 absolute e;
    x: boolean;
begin
  e:=exportedfunctions;  //save it to prevent plugins from fucking it up


  case plugins[pluginid].pluginversion of
    1: e.sizeofExportedFunctions:=sizeof(Texportedfunctions1);
    2: e.sizeofExportedFunctions:=sizeof(Texportedfunctions2);
    3: e.sizeofExportedFunctions:=sizeof(Texportedfunctions3);
    4: e.sizeofExportedFunctions:=sizeof(Texportedfunctions4);
    5: e.sizeofExportedFunctions:=sizeof(Texportedfunctions5);
    else
    begin
      if plugins[pluginid].dotnet then
      begin
        enet:=exportedfunctionsdotnet;
        enet.sizeofExportedFunctions:=sizeof(TExportedFunctionsDotNetV1)
      end
      else
        e.sizeofExportedFunctions:=sizeof(Texportedfunctions);
    end;
  end;

  pluginCS.Enter;
  try
    if pluginid>=length(plugins) then exit;

    if not plugins[pluginid].enabled then
    begin
      OutputDebugString('Calling EnablePlugin');

      x:=plugins[pluginid].EnablePlugin(e,pluginid);

      if not x then raise exception.Create(Format(rsErrorEnabling, [plugins[pluginid].dllname]));
      plugins[pluginid].enabled:=true;
    end;
  finally
    pluginCS.Leave;
  end;
end;

procedure TPluginHandler.UnloadPlugin(pluginid: integer);
//will disable and unload the plugin
begin
  pluginCS.Enter;
  try
    DisablePlugin(pluginid); //disable the plugin if it was active



    FreeLibrary(plugins[pluginid].hmodule);
    plugins[pluginid].dllname:='';
    plugins[pluginid].filepath:='';
    plugins[pluginid].name:='';
  finally
    pluginCS.Leave;
  end;
end;

procedure TPluginHandler.DisablePlugin(pluginid: integer);
var i: integer;
begin
  pluginCS.Enter;
  try
    if plugins[pluginid].enabled then
    begin
      if not plugins[pluginid].DisablePlugin() then raise exception.Create(Format(rsErrorDisabling, [plugins[pluginid].dllname]));
      plugins[pluginid].enabled:=false;

      //unregister all functions
//      for i:=0 to

      while length(plugins[pluginid].Registeredfunctions0)>0 do
        unregisterfunction(pluginid,plugins[pluginid].Registeredfunctions0[0].functionid);

      while length(plugins[pluginid].Registeredfunctions1)>0 do
        unregisterfunction(pluginid,plugins[pluginid].Registeredfunctions1[0].functionid);

      while length(plugins[pluginid].Registeredfunctions2)>0 do
        unregisterfunction(pluginid,plugins[pluginid].Registeredfunctions2[0].functionid);

      while length(plugins[pluginid].Registeredfunctions3)>0 do
        unregisterfunction(pluginid,plugins[pluginid].Registeredfunctions3[0].functionid);

      while length(plugins[pluginid].Registeredfunctions4)>0 do
        unregisterfunction(pluginid,plugins[pluginid].Registeredfunctions4[0].functionid);

      while length(plugins[pluginid].Registeredfunctions5)>0 do
        unregisterfunction(pluginid,plugins[pluginid].Registeredfunctions5[0].functionid);

      while length(plugins[pluginid].Registeredfunctions6)>0 do
        unregisterfunction(pluginid,plugins[pluginid].Registeredfunctions6[0].functionid);

      while length(plugins[pluginid].Registeredfunctions7)>0 do
        unregisterfunction(pluginid,plugins[pluginid].Registeredfunctions7[0].functionid);

      while length(plugins[pluginid].Registeredfunctions8)>0 do
        unregisterfunction(pluginid,plugins[pluginid].Registeredfunctions8[0].functionid);

    end;
  finally
    pluginCS.Leave;
  end;
end;


function TPluginHandler.DotNetPluginGetPluginName(dllname: string): string;
var PInit: TPluginDotNetInitResult;
begin
  DotNetExecuteClassMethod(dllname,'CESDK','CESDK','CEPluginInitialize', inttostr(ptruint(@PInit)));
  result:=PInit.name;
end;

function TPluginHandler.GetPluginName(dllname:string):string;
var hmodule: thandle;
    GetVersion: TGetVersion;
    PluginVersion: TPluginVersion;
    path: widestring;
    isdotnet: boolean;
begin

  result:='';
  if uppercase(extractfileext(dllname))<>'.DLL' then raise exception.Create(Format(rsErrorLoadingOnlyDLLFilesAreAllowed, [dllname]));

    {$ifdef windows}
  if peinfo_isdotnetfile(dllname, isdotnet) then
    if isdotnet then
      exit(DotNetPluginGetPluginName(dllname));
    {$endif}

  hmodule:=loadlibrary(pchar(dllname));
  {$ifdef windows}
  if (hmodule=0) and assigned(AddDllDirectory) then
  begin
    path:=extractfiledir(dllname);
    AddDllDirectory(pwidechar(@path[1]));
    hmodule:=loadlibrary(pchar(dllname));
  end;
  {$endif}

  if hmodule=0 then
    raise exception.create(rsPlugThePluginDllCouldNotBeLoaded+inttostr(getlasterror));


  GetVersion:=getprocaddress(hmodule,'CEPlugin_GetVersion');
  if not assigned(GetVersion) then
    getVersion:=getprocaddress(hmodule,'GetVersion');

  if not assigned(GetVersion) then
    raise exception.Create(Format(rsErrorLoadingTheDllIsMissingTheCEPlugin_GetVersionF, [dllname]));


  if (getprocaddress(hmodule, 'CEPlugin_InitializePlugin')=nil) and (getprocaddress(hmodule, 'InitializePlugin')=nil) then raise exception.Create(Format(rsIsMissingTheCEPlugin_InitializePluginExport, [dllname]));
  if (getprocaddress(hmodule, 'CEPlugin_DisablePlugin')=nil) and (getprocaddress(hmodule, 'DisablePlugin')=nil) then raise exception.Create(Format(rsIsMissingTheCEPlugin_DisablePluginExport, [dllname]));

  if GetVersion(PluginVersion,sizeof(TPluginVersion)) then
    result:=PluginVersion.pluginname;

  freelibrary(hmodule);
end;

function TPluginHandler.GetPluginID(dllname:string):integer;
var dname: string;
    i: integer;
begin
  result:=-1;

  dname:=uppercase(extractfilename(dllname));
  pluginCS.Enter;
  try
    for i:=0 to length(plugins)-1 do
    begin
      if uppercase(plugins[i].dllname)=dname then
      begin
        result:=i;
        exit;
      end;
    end;
  finally
    pluginCS.Leave;
  end;


end;

function TPluginHandler.DotNetLoadPlugin(dllname:string):integer;
var initresult: TPluginDotNetInitResult;
begin
  DotNetExecuteClassMethod(dllname,'CESDK','CESDK','CEPluginInitialize',inttostr(ptruint(@initresult)));
  if initresult.version>currentpluginversion then
    raise exception.Create(Format(rsErrorLoadingThisDllRequiresANewerVersionOfCeToFunc, [dllname]));

  pluginCS.enter;
  try
    try
      setlength(plugins,length(plugins)+1);
      plugins[length(plugins)-1].dotnet:=true;
      plugins[length(plugins)-1].pluginversion:=initresult.version;
      plugins[length(plugins)-1].dllname:=extractfilename(dllname);
      plugins[length(plugins)-1].filepath:=GetRelativeFilePath(dllname);
      plugins[length(plugins)-1].hmodule:=0;
      plugins[length(plugins)-1].name:=initresult.name;

      plugins[length(plugins)-1].GetVersion:=initresult.GetVersion;
      plugins[length(plugins)-1].EnablePlugin:=initresult.EnablePlugin;
      plugins[length(plugins)-1].DisablePlugin:=initresult.DisablePlugin;
      plugins[length(plugins)-1].nextid:=1;

      result:=length(plugins)-1;
    except
      on e: exception do
      begin
        setlength(plugins,length(plugins)-1);
        raise exception.create(e.message);
      end;
    end;
  finally
    pluginCS.leave;
  end;
end;

function TPluginHandler.LoadPlugin(dllname:string):integer;
var hmodule: thandle;
    GetVersion: TGetVersion;
    PluginVersion: TPluginVersion;
    s: string;
    i: integer;
    path: widestring;
    isdotnet: boolean;
begin
  result:=-1;
  if uppercase(extractfileext(dllname))<>'.DLL' then raise exception.Create(Format(rsErrorLoadingOnlyDLLFilesAreAllowed, [dllname]));

  s:=uppercase(extractfilename(dllname));
  pluginCS.Enter;
  try
    for i:=0 to length(plugins)-1 do
    begin
      //check if it was loaded already or not
      if s=uppercase(plugins[length(plugins)-1].dllname) then
      begin
        result:=i;
        exit; //already in the list so no need to load again
      end;
    end;
  finally
    pluginCS.Leave;
  end;

  {$ifdef windows}
  if peinfo_isdotnetfile(dllname, isdotnet) then
    if isdotnet then
      exit(DotNetLoadPlugin(dllname));

  {$endif}


  hmodule:=loadlibrary(pchar(dllname));
  {$ifdef windows}
  if (hmodule=0) and assigned(AddDllDirectory) then
  begin
    path:=ExtractFiledir(dllname);
    AddDllDirectory(pwidechar(@path[1]));
    hmodule:=loadlibrary(pchar(dllname));
  end;
  {$endif}

  if hmodule=0 then
    exit;


  GetVersion:=getprocaddress(hmodule,'CEPlugin_GetVersion');
  if not assigned(GetVersion) then
    GetVersion:=getprocaddress(hmodule,'GetVersion');


  if not assigned(GetVersion) then raise exception.Create(Format(rsErrorLoadingTheDllIsMissingTheCEPlugin_GetVersionE, [dllname]));
  if GetVersion(PluginVersion,sizeof(TPluginVersion)) then
  begin
    if PluginVersion.version>currentpluginversion then
      raise exception.Create(Format(rsErrorLoadingThisDllRequiresANewerVersionOfCeToFunc, [dllname]));

    pluginCS.enter;
    try
      try
        setlength(plugins,length(plugins)+1);
        plugins[length(plugins)-1].dotnet:=false;
        plugins[length(plugins)-1].pluginversion:=PluginVersion.version;
        plugins[length(plugins)-1].dllname:=extractfilename(dllname);
        plugins[length(plugins)-1].filepath:=GetRelativeFilePath(dllname);
        plugins[length(plugins)-1].hmodule:=hmodule;
        plugins[length(plugins)-1].name:=PluginVersion.pluginname;

        plugins[length(plugins)-1].GetVersion:=getprocaddress(hmodule,'CEPlugin_GetVersion');
        if not assigned(plugins[length(plugins)-1].GetVersion) then
          plugins[length(plugins)-1].GetVersion:=GetProcAddress(hmodule, 'GetVersion');


        plugins[length(plugins)-1].EnablePlugin:=getprocaddress(hmodule,'CEPlugin_InitializePlugin');
        if not assigned(plugins[length(plugins)-1].EnablePlugin) then
          plugins[length(plugins)-1].EnablePlugin:=GetProcAddress(hmodule, 'InitializePlugin');

        plugins[length(plugins)-1].DisablePlugin:=getprocaddress(hmodule,'CEPlugin_DisablePlugin');
        if not assigned(plugins[length(plugins)-1].DisablePlugin) then
          plugins[length(plugins)-1].DisablePlugin:=GetProcAddress(hmodule, 'DisablePlugin');


        plugins[length(plugins)-1].nextid:=1;

        if not assigned(plugins[length(plugins)-1].EnablePlugin) then raise exception.Create(Format(rsIsMissingTheCEPlugin_InitializePluginExport, [dllname]));
        if not assigned(plugins[length(plugins)-1].DisablePlugin) then raise exception.Create(Format(rsIsMissingTheCEPlugin_DisablePluginExport, [dllname]));
        result:=length(plugins)-1;
      except
        on e: exception do
        begin
          setlength(plugins,length(plugins)-1);
          raise exception.create(e.message);
        end;
      end;
    finally
      pluginCS.leave;
    end;

  end else raise exception.Create(Format(rsErrorLoadingTheGetVersionFunctionReturnedFALSE, [dllname]));
end;


procedure TPluginHandler.FillCheckListBox(clb: TCheckListbox);
var i,j: integer;
    x:Tpathspecifier;
begin
  if self=nil then exit;
  j:=clb.count;

  for i:=0 to clb.Count-1 do
    Tpathspecifier(clb.Items.Objects[i]).Free;

  clb.Clear;
  pluginCS.Enter;
  for i:=0 to length(plugins)-1 do
  begin
    x:=TPathSpecifier.Create;
    x.path:=plugins[i].filepath;
    j:=clb.Items.AddObject(plugins[i].dllname+':'+plugins[i].name,x);
    clb.Checked[j]:=plugins[i].enabled;
  end;
  pluginCS.Leave;
end;

procedure TPluginHandler.handleAutoAssemblerPlugin(line: ppchar; phase: integer; id: integer);
var i,j: integer;
begin
  pluginCS.Enter;
  try
    for i:=0 to length(plugins)-1 do
      for j:=0 to length(plugins[i].RegisteredFunctions8)-1 do
      begin
        if plugins[i].pluginversion<=5 then
          TPluginFunction8Version5(plugins[i].Registeredfunctions8[j].callback)(line,phase)
        else
          plugins[i].RegisteredFunctions8[j].callback(line,phase,id);

      end;
  finally
    pluginCS.Leave;
  end;
end;

procedure TPluginHandler.handledisassemblerContextPopup(address: ptruint);
var i,j: integer;
    addressofmenuitemstring: pchar;
    s: string;
    show: bool;
begin
  pluginCS.Enter;
  try
    for i:=0 to length(plugins)-1 do
      for j:=0 to length(plugins[i].RegisteredFunctions6)-1 do
      begin
        s:=plugins[i].RegisteredFunctions6[j].menuitem.Caption;
        addressofmenuitemstring:=@s[1];
        show:=true;
        if assigned(plugins[i].RegisteredFunctions6[j].callbackOnContext) then
        begin
          if plugins[i].pluginversion<=5 then
            Tpluginfuntion6OnContextVersion5(plugins[i].RegisteredFunctions6[j].callbackOnContext)(address, @addressofmenuitemstring)
          else
            plugins[i].RegisteredFunctions6[j].callbackOnContext(address, @addressofmenuitemstring, @show);
        end;

        plugins[i].RegisteredFunctions6[j].menuitem.Caption:=addressofmenuitemstring;
        plugins[i].RegisteredFunctions6[j].menuitem.Visible:=show;
      end;
  finally
    pluginCS.Leave;
  end;
end;

procedure TPluginHandler.handledisassemblerplugins(address: ptruint; addressStringPointer: pointer; bytestringpointer: pointer; opcodestringpointer: pointer; specialstringpointer: pointer; textcolor: PColor);
var i,j: integer;
begin
  pluginCS.Enter;
  try
    for i:=0 to length(plugins)-1 do
      for j:=0 to length(plugins[i].RegisteredFunctions7)-1 do
        if assigned(plugins[i].RegisteredFunctions7[j].callback) then
          plugins[i].RegisteredFunctions7[j].callback(address, addressStringPointer, bytestringpointer, opcodestringpointer, specialstringpointer, textcolor);
  finally
    pluginCS.Leave;
  end;
end;

function TPluginHandler.handledebuggerplugins(devent: PDebugEvent):integer;
var i,j: integer;
begin
  result:=0;
  pluginCS.Enter;
  try
    for i:=0 to length(plugins)-1 do
      for j:=0 to length(plugins[i].RegisteredFunctions2)-1 do
        if assigned(plugins[i].RegisteredFunctions2[j].callback) then
          if plugins[i].RegisteredFunctions2[j].callback(devent)=1 then result:=1;
  finally
    pluginCS.Leave;
  end;
end;

function TPluginHandler.handlenewprocessplugins(processid: dword; peprocess:ptruint; created: boolean):boolean;
var i,j: integer;
begin
  result:=true;
  pluginCS.Enter;
  try
    for i:=0 to length(plugins)-1 do
      for j:=0 to length(plugins[i].Registeredfunctions3)-1 do
      begin
        if plugins[i].pluginversion=1 then
          TPluginFunction3Version1(plugins[i].Registeredfunctions3[j].callback)(processid,peprocess)
        else
          plugins[i].Registeredfunctions3[j].callback(processid,peprocess, created);
      end;
  finally
    pluginCS.Leave;
  end;
end;

function TPluginHandler.handlechangedpointers(section: integer):boolean;
var i,j: integer;
begin
  if self=nil then exit(false);

  if assigned(onAPIPointerChange) then
    onAPIPointerChange(nil);


  result:=true;
  pluginCS.Enter;
  try
    for i:=0 to length(plugins)-1 do
      for j:=0 to length(plugins[i].RegisteredFunctions4)-1 do
        plugins[i].RegisteredFunctions4[j].callback(section);
  finally
    pluginCS.Leave;
  end;
end;

destructor TPluginHandler.destroy;
var i,j: integer;
begin
  pluginCS.Enter;
  try
    for i:=0 to length(plugins)-1 do
    begin
      try
        UnloadPlugin(i);
      except
      end;
    end;
  finally
    pluginCS.Leave;
  end;

  inherited destroy;
end;

constructor TPluginHandler.create;
var test: pchar;
begin
  pluginCS:=TCriticalSection.Create;
  exportedfunctions.sizeofExportedFunctions:=sizeof(TExportedFunctions);
  exportedfunctions.showmessage:=@ce_showmessage;
  exportedfunctions.registerfunction:=@ce_registerfunction;
  exportedfunctions.unregisterfunction:=@ce_unregisterfunction;
  exportedfunctions.OpenedProcessID:=@processhandler.processid;
  exportedfunctions.OpenedProcessHandle:=@processhandler.processhandle;


  exportedfunctions.GetMainWindowHandle:=@ce_GetMainWindowHandle;
  exportedfunctions.AutoAssemble:=@ce_autoassemble;
  exportedfunctions.assembler:=@ce_assembler;
  exportedfunctions.disassembler:=@ce_disassembler;
  exportedfunctions.ChangeRegistersAtAddress:=@ce_ChangeRegistersAtAddress;
  exportedfunctions.InjectDLL:=@ce_injectdll;
  exportedfunctions.freezemem:=@ce_freezemem;
  exportedfunctions.unfreezemem:=@ce_unfreezemem;
  exportedfunctions.fixmem:=nil; //obsolete
  exportedfunctions.processlist:=@ce_processlist;
  exportedfunctions.reloadsettings:=@ce_reloadsettings;
  exportedfunctions.getaddressfrompointer:=@ce_getaddressfrompointer;


  //pointers to the address that contains the pointers to the functions
  {$ifdef windows}
  exportedfunctions.ReadProcessMemory:=@@ReadProcessMemory;
  exportedfunctions.WriteProcessMemory:=@@WriteProcessMemoryActual;
  exportedfunctions.GetThreadContext:=@@GetThreadContext;
  exportedfunctions.SetThreadContext:=@@SetThreadContext;
  exportedfunctions.SuspendThread:=@@SuspendThread;
  exportedfunctions.ResumeThread:=@@ResumeThread;
  exportedfunctions.OpenProcess:=@@OpenProcess;
  exportedfunctions.WaitForDebugEvent:=@@WaitForDebugEvent;
  exportedfunctions.ContinueDebugEvent:=@@ContinueDebugEvent;
  exportedfunctions.DebugActiveProcess:=@@DebugActiveProcess;
  exportedfunctions.StopDebugging:=nil;
  exportedfunctions.StopRegisterChange:=nil; //@@StopRegisterChange;
  exportedfunctions.VirtualProtect:=@@VirtualProtect;
  exportedfunctions.VirtualProtectEx:=@@VirtualProtectEx;
  exportedfunctions.VirtualQueryEx:=@@VirtualQueryEx;
  exportedfunctions.VirtualAllocEx:=@@VirtualAllocEx;
  exportedfunctions.CreateRemoteThread:=@@CreateRemoteThread;
  exportedfunctions.OpenThread:=@@OpenThread;
  exportedfunctions.GetPEProcess:=@GetPEProcess;
  exportedfunctions.GetPEThread:=@GetPEThread;
  exportedfunctions.GetThreadsProcessOffset:=@GetThreadsProcessOffset;
  exportedfunctions.GetThreadListEntryOffset:=@GetThreadListEntryOffset;
  exportedfunctions.GetProcessnameOffset:=nil; //obsolete
  exportedfunctions.GetDebugportOffset:=@GetDebugportOffset;
  exportedfunctions.GetPhysicalAddress:=@GetPhysicalAddress;
  exportedfunctions.ProtectMe:=nil;
  exportedfunctions.GetCR4:=@GetCR4;
  exportedfunctions.GetCR3:=@GetCR3;
  exportedfunctions.SetCR3:=nil; //@@SetCR3;
  exportedfunctions.GetSDT:=@GetSDT;
  exportedfunctions.GetSDTShadow:=@GetSDTShadow;
  exportedfunctions.setAlternateDebugMethod:=nil; //@@setAlternateDebugMethod;
  exportedfunctions.getAlternateDebugMethod:=nil; //@@getAlternateDebugMethod;
  exportedfunctions.DebugProcess:=nil; //@@DebugProcess;
  exportedfunctions.ChangeRegOnBP:=nil; //@@ChangeRegOnBP;
  exportedfunctions.RetrieveDebugData:=nil; //@@RetrieveDebugData;
  exportedfunctions.StartProcessWatch:=@StartProcessWatch;
  exportedfunctions.WaitForProcessListData:=@WaitForProcessListData;
  exportedfunctions.GetProcessNameFromID:=@GetProcessNameFromID;
  exportedfunctions.GetProcessNameFromPEProcess:=@GetProcessNameFromPEProcess;
  exportedfunctions.KernelOpenProcess:=@dbk32functions.OP;
  exportedfunctions.KernelReadProcessMemory:=@dbk32functions.RPM;
  exportedfunctions.KernelWriteProcessMemory:=@dbk32functions.WPM;
  exportedfunctions.KernelVirtualAllocEx:=@VQE;
  exportedfunctions.IsValidHandle:=@IsValidHandle;
  exportedfunctions.GetIDTCurrentThread:=@GetIDTCurrentThread;
  exportedfunctions.GetIDTs:=@GetIDTs;
  exportedfunctions.MakeWritable:=@MakeWritable;
  exportedfunctions.GetLoadedState:=@GetLoadedState;
  exportedfunctions.DBKSuspendThread:=@DBKSuspendThread;
  exportedfunctions.DBKResumeThread:=@DBKResumeThread;
  exportedfunctions.DBKSuspendProcess:=@DBKSuspendProcess;
  exportedfunctions.DBKResumeProcess:=@DBKResumeProcess;
  exportedfunctions.KernelAlloc:=@KernelAlloc;
  exportedfunctions.GetKProcAddress:=@GetKProcAddress;

  exportedfunctions.CreateToolhelp32Snapshot:=@@CreateToolhelp32Snapshot;
  exportedfunctions.Process32First:=@@Process32First;
  exportedfunctions.Process32Next:=@@Process32Next;
  exportedfunctions.Thread32First:=@@Thread32First;
  exportedfunctions.Thread32Next:=@@Thread32Next;
  exportedfunctions.Module32First:=@@Module32First;
  exportedfunctions.Module32Next:=@@Module32Next;
  exportedfunctions.Heap32ListFirst:=@@Heap32ListFirst;
  exportedfunctions.Heap32ListNext:=@@Heap32ListNext;
  {$endif}



  //give the address of the variable since there is a change they arn't initialized just yet...
  exportedfunctions.mainform:=@mainform;
  exportedfunctions.memorybrowser:=@memorybrowser;

  //version2 init:
  exportedfunctions.sym_nameToAddress:=@ce_sym_nameToAddress;
  exportedfunctions.sym_addressToName:=@ce_sym_addressToName;
  exportedfunctions.sym_generateAPIHookScript:=@ce_generateAPIHookScript;

  //version3 init
  {$ifdef windows}
  exportedfunctions.loadDBK32:=@LoadDBK32;
  exportedfunctions.loaddbvmifneeded:=@loaddbvmifneeded;
  {$endif}
  exportedfunctions.previousOpcode:=@ce_previousOpcode;
  exportedfunctions.nextOpcode:=@ce_nextOpcode;
  exportedfunctions.disassembleEx:=@ce_disassemble;
  exportedfunctions.loadModule:=@ce_loadModule;

  exportedfunctions.aa_AddExtraCommand:=@aa_AddExtraCommand;
  exportedfunctions.aa_RemoveExtraCommand:=@aa_RemoveExtraCommand;

  //version 4 init
  exportedfunctions.createTableEntry:=@ce_createTableEntry;
  exportedfunctions.getTableEntry:=@ce_getTableEntry;
  exportedfunctions.memrec_setDescription:=@ce_memrec_setDescription;
  exportedfunctions.memrec_getDescription:=@ce_memrec_getDescription;
  exportedfunctions.memrec_getAddress:=@ce_memrec_getAddress;
  exportedfunctions.memrec_setAddress:=@ce_memrec_setAddress;
  exportedfunctions.memrec_getType:=@ce_memrec_getType;
  exportedfunctions.memrec_setType:=@ce_memrec_setType;
  exportedfunctions.memrec_getValue:=@ce_memrec_getValue;
  exportedfunctions.memrec_setValue:=@ce_memrec_setValue;
  exportedfunctions.memrec_getScript:=@ce_memrec_getScript;
  exportedfunctions.memrec_setScript:=@ce_memrec_setScript;
  exportedfunctions.memrec_isfrozen:=@ce_memrec_isfrozen;
  exportedfunctions.memrec_freeze:=@ce_memrec_freeze;
  exportedfunctions.memrec_unfreeze:=@ce_memrec_unfreeze;
  exportedfunctions.memrec_setColor:=@ce_memrec_setColor;
  exportedfunctions.memrec_appendtoentry:=@ce_memrec_appendtoentry;
  exportedfunctions.memrec_delete:=@ce_memrec_delete;

  exportedfunctions.getProcessIDFromProcessName:=@ce_getProcessIDFromProcessName;
  exportedfunctions.openProcessEx:=@ce_openProcess;
  exportedfunctions.debugProcessEx:=@ce_debugProcess;
  exportedfunctions.pause:=@ce_pause;
  exportedfunctions.unpause:=@ce_unpause;

  exportedfunctions.debug_setBreakpoint:=@ce_debug_setBreakpoint;
  exportedfunctions.debug_removeBreakpoint:=@ce_debug_removeBreakpoint;
  exportedfunctions.debug_continueFromBreakpoint:=@ce_debug_continueFromBreakpoint;

  exportedfunctions.closeCE:=@ce_closeCE;
  exportedfunctions.hideAllCEWindows:=@ce_hideAllCEWindows;
  exportedfunctions.unhideMainCEwindow:=@ce_unhideMainCEwindow;
  exportedfunctions.createForm:=@ce_createForm;
  exportedfunctions.form_centerScreen:=@ce_form_centerScreen;
  exportedfunctions.form_hide:=@ce_form_hide;
  exportedfunctions.form_show:=@ce_form_show;
  exportedfunctions.form_onClose:=@ce_form_onClose;

  exportedfunctions.createPanel:=@ce_createPanel;
  exportedfunctions.createGroupBox:=@ce_createGroupBox;
  exportedfunctions.createButton:=@ce_createButton;
  exportedfunctions.createImage:=@ce_createImage;
  exportedfunctions.image_loadImageFromFile:=@ce_image_loadImageFromFile;
  exportedfunctions.image_transparent:=@ce_image_transparent;
  exportedfunctions.image_stretch:=@ce_image_stretch;

  exportedfunctions.createLabel:=@ce_createLabel;
  exportedfunctions.createEdit:=@ce_createEdit;
  exportedfunctions.createMemo:=@ce_createMemo;
  exportedfunctions.createTimer:=@ce_createTimer;
  exportedfunctions.timer_setInterval:=@ce_timer_setInterval;
  exportedfunctions.timer_onTimer:=@ce_timer_onTimer;
  exportedfunctions.control_setCaption:=@ce_control_setCaption;
  exportedfunctions.control_getCaption:=@ce_control_getCaption;
  exportedfunctions.control_setPosition:=@ce_control_setPosition;
  exportedfunctions.control_getX:=@ce_control_getX;
  exportedfunctions.control_getY:=@ce_control_getY;
  exportedfunctions.control_setSize:=@ce_control_setSize;
  exportedfunctions.control_getWidth:=@ce_control_getWidth;
  exportedfunctions.control_getHeight:=@ce_control_getHeight;
  exportedfunctions.control_setAlign:=@ce_control_setAlign;
  exportedfunctions.control_onClick:=@ce_control_onClick;

  exportedfunctions.object_destroy:=@ce_object_destroy;
  exportedfunctions.messageDialog:=@ce_messageDialog;
  exportedfunctions.speedhack_setSpeed:=@ce_speedhack_setSpeed;

  //version 5
  {$ifdef windows}
  exportedfunctions.ExecuteKernelCode:=@ExecuteKernelCode;
  exportedfunctions.UserdefinedInterruptHook:=@UserdefinedInterruptHook;
  {$endif}
  exportedfunctions.GetLuaState:=@plugin_GetLuaState;
  exportedfunctions.MainThreadCall:=@pluginsync;


  exportedfunctionsdotnet.sizeofExportedFunctions:=sizeof(exportedfunctionsdotnet);
  exportedfunctionsdotnet.GetLuaState:=@plugin_GetLuaState;
  exportedfunctionsdotnet.LuaRegister:=@lua_register;
  exportedfunctionsdotnet.LuaPushClassInstance:=@luaclass_pushClass;
  exportedfunctionsdotnet.ProcessMessages:=@plugin_processMessages;
  exportedfunctionsdotnet.CheckSynchronize:=@plugin_CheckSynchronize;
end;


end.


