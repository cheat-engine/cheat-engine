unit plugin;

{$MODE Delphi}

interface

uses lclproc, windows, sysutils,LCLIntf,checklst,menus,dialogs,CEFuncProc,NewKernelHandler, graphics;

const CurrentPluginVersion=3;

//structures
type TPluginVersion = record
  version: dword; //version number of ce plugin it is developed for (compatibility for the future)
  pluginname: pchar; //pointer to a 0-terminated string in the dll
end;
type PPluginVersion=^TPluginVersion;

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

//exported functions of the plugin
type TGetVersion=function(var PluginVersion:TPluginVersion; TPluginVersionSize: integer):BOOL; stdcall;
type TInitializePlugin=function(var ExportedFunctions: TExportedFunctions3; pluginid: dword):BOOL; stdcall;
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
type TPluginfunction1=function(disassembleraddress: pdword; selected_disassembler_address: pdword; hexviewaddress:pdword ):bool; stdcall;

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
type TPluginFunction3=function(processid: dword; peprocess:dword; created: BOOL):integer; stdcall;
type TPluginFunction3Version1=function(processid: dword; peprocess:dword):integer; stdcall;
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
type TPluginfunction6=function(selectedAddress: pdword):bool; stdcall;
type Tpluginfuntion6OnContext=function(selectedAddress: dword; addressofname: pointer):bool; stdcall;

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
type TPluginFunction7=procedure(address: dword; addressStringPointer: pointer; bytestringpointer: pointer; opcodestringpointer: pointer; specialstringpointer: pointer; textcolor: PColor); stdcall;
type TPluginfunctionType7=class
  public
    pluginid: integer;
    functionid: integer;
    callback: TPluginFunction7;
end;

//plugin type 8
//where: when the autoassembler is used in the first and 2nd stage
type TPluginFunction8=procedure(line: ppchar; phase: integer); stdcall;
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
    pluginMREW: TMultiReadExclusiveWriteSynchronizer;
    plugins: array of TPlugin;
    function GetDLLFilePath(pluginid: integer):string;
  public
    function GetPluginID(dllname:string):integer;
    function GetPluginName(dllname:string):string;
    function LoadPlugin(dllname: string):integer;
    procedure FillCheckListBox(clb: TCheckListbox);
    procedure EnablePlugin(pluginid: integer);
    procedure DisablePlugin(pluginid: integer);
    procedure handleAutoAssemblerPlugin(line: ppchar; phase: integer);
    procedure handledisassemblerContextPopup(address: dword); 
    procedure handledisassemblerplugins(address: dword; addressStringPointer: pointer; bytestringpointer: pointer; opcodestringpointer: pointer; specialstringpointer: pointer; textcolor: PColor);
    function handledebuggerplugins(devent:PDebugEvent):integer;
    function handlenewprocessplugins(processid: dword; peprocess:dword; created: boolean):boolean;
    function handlechangedpointers(section: integer):boolean;
    function registerfunction(pluginid,functiontype:integer; init: pointer):integer;
    function unregisterfunction(pluginid,functionid: integer): boolean;
    property dllfilepath[pluginid: integer]: string read getdllfilepath;
    constructor create;
end;

var pluginhandler: TPluginhandler;
    exportedfunctions: TExportedFunctions3;

implementation

uses MainUnit,memorybrowserformunit,formsettingsunit, pluginexports, SynHighlighterAA;

function TPluginHandler.GetDLLFilePath(pluginid: integer):string;
begin
  pluginMREW.BeginRead;
  result:=plugins[pluginid].filepath;
  pluginMREW.EndRead;
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

  pluginmrew.BeginWrite;
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
           newmenuitem.Tag:=dword(f0);
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
           newmenuitem.Tag:=dword(f1);
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
           newmenuitem.Tag:=dword(f5);
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
           newmenuitem.Tag:=dword(f6);
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
    pluginmrew.EndWrite;
  end;
end;

function TPluginHandler.unregisterfunction(pluginid,functionid: integer): boolean;
var i,j: integer;
    f: ^TPluginfunctionType0;
begin
  //remove it
  result:=false;
  pluginmrew.BeginWrite;
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
    pluginmrew.EndWrite;
  end;
end;

procedure TPluginHandler.EnablePlugin(pluginid: integer);
var e: texportedfunctions3;
    x: boolean;
begin
  e:=exportedfunctions;  //save it to prevent plugins from fucking it up

  if plugins[pluginid].pluginversion=1 then
    e.sizeofExportedFunctions:=sizeof(Texportedfunctions1); //Just say it's smaller (order stays the same)

  if plugins[pluginid].pluginversion=2 then
    e.sizeofExportedFunctions:=sizeof(Texportedfunctions2);

  pluginMREW.BeginRead;
  if pluginid>=length(plugins) then exit;
  
  try
    if not plugins[pluginid].enabled then
    begin
      x:=plugins[pluginid].EnablePlugin(e,pluginid);
      if not x then raise exception.Create('Error enabling '+plugins[pluginid].dllname);
      plugins[pluginid].enabled:=true;
    end;
  finally
    pluginMREW.EndRead;
  end;
end;

procedure TPluginHandler.DisablePlugin(pluginid: integer);
var i: integer;
begin
  pluginMREW.BeginRead;
  try
    if plugins[pluginid].enabled then
    begin
      if not plugins[pluginid].DisablePlugin() then raise exception.Create('Error disabling '+plugins[pluginid].dllname);
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
    pluginMREW.EndRead;
  end;
end;

function TPluginHandler.GetPluginName(dllname:string):string;
var hmodule: thandle;
    GetVersion: TGetVersion;
    PluginVersion: TPluginVersion;
begin
  result:='';
  if uppercase(extractfileext(dllname))<>'.DLL' then raise exception.Create('Error loading '+dllname+'. Only DLL files are allowed');
  hmodule:=loadlibrary(pchar(dllname));
  GetVersion:=getprocaddress(hmodule,'GetVersion');

  if getprocaddress(hmodule,'InitializePlugin')=nil then raise exception.Create(dllname+' is missing the InitializePlugin export');
  if getprocaddress(hmodule,'DisablePlugin')=nil then raise exception.Create(dllname+' is missing the DisablePlugin export');

  if @GetVersion=nil then raise exception.Create('Error loading '+dllname+'. The dll is missing the GetVersion function');
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
  pluginMREW.BeginRead;
  for i:=0 to length(plugins)-1 do
  begin
    if uppercase(plugins[i].dllname)=dname then
    begin
      result:=i;
      exit;
    end;
  end;

  pluginMREW.EndRead;
end;


function TPluginHandler.LoadPlugin(dllname:string):integer;
var hmodule: thandle;
    GetVersion: TGetVersion;
    PluginVersion: TPluginVersion;
    s: string;
    i: integer;
begin
  result:=0;
  if uppercase(extractfileext(dllname))<>'.DLL' then raise exception.Create('Error loading '+dllname+'. Only DLL files are allowed');

  s:=uppercase(extractfilename(dllname));
  pluginMREW.BeginRead;
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
    pluginMREW.EndRead;
  end;

  hmodule:=loadlibrary(pchar(dllname));
  GetVersion:=getprocaddress(hmodule,'GetVersion');

  if @GetVersion=nil then raise exception.Create('Error loading '+dllname+'. The dll is missing the GetVersion function');
  if GetVersion(PluginVersion,sizeof(TPluginVersion)) then
  begin
    if PluginVersion.version>currentpluginversion then
      raise exception.Create('Error loading '+dllname+'. This dll requires a newer version of ce to function properly');

    pluginMREW.BeginWrite;
    try
      try
        setlength(plugins,length(plugins)+1);
        plugins[length(plugins)-1].pluginversion:=PluginVersion.version;
        plugins[length(plugins)-1].dllname:=extractfilename(dllname);
        plugins[length(plugins)-1].filepath:=GetRelativeFilePath(dllname);
        plugins[length(plugins)-1].hmodule:=hmodule;
        plugins[length(plugins)-1].name:=PluginVersion.pluginname;
        plugins[length(plugins)-1].GetVersion:=getprocaddress(hmodule,'GetVersion');
        plugins[length(plugins)-1].EnablePlugin:=getprocaddress(hmodule,'InitializePlugin');
        plugins[length(plugins)-1].DisablePlugin:=getprocaddress(hmodule,'DisablePlugin');
        plugins[length(plugins)-1].nextid:=1;

        if @plugins[length(plugins)-1].EnablePlugin=nil then raise exception.Create(dllname+' is missing the InitializePlugin export');
        if @plugins[length(plugins)-1].DisablePlugin=nil then raise exception.Create(dllname+' is missing the DisablePlugin export');
        result:=length(plugins)-1;
      except
        on e: exception do
        begin
          setlength(plugins,length(plugins)-1);
          raise e;
        end;
      end;
    finally
      pluginMREW.EndWrite;
    end;

  end else raise exception.Create('Error loading '+dllname+'. The GetVersion function returned FALSE');
end;


procedure TPluginHandler.FillCheckListBox(clb: TCheckListbox);
var i,j: integer;
    x:Tpathspecifier;
begin
  for i:=0 to clb.Count-1 do
    Tpathspecifier(clb.Items.Objects[i]).Free;

  clb.Clear;
  pluginMREW.BeginRead;
  for i:=0 to length(plugins)-1 do
  begin
    x:=TPathSpecifier.Create;
    x.path:=plugins[i].filepath;
    j:=clb.Items.AddObject(plugins[i].dllname+':'+plugins[i].name,x);
    clb.Checked[j]:=plugins[i].enabled;
  end;
  pluginMREW.EndRead;
end;

procedure TPluginHandler.handleAutoAssemblerPlugin(line: ppchar; phase: integer);
var i,j: integer;
begin
  pluginMREW.BeginRead;
  try
    for i:=0 to length(plugins)-1 do
      for j:=0 to length(plugins[i].RegisteredFunctions8)-1 do
        plugins[i].RegisteredFunctions8[j].callback(line,phase);
  finally
    pluginMREW.EndRead;
  end;
end;

procedure TPluginHandler.handledisassemblerContextPopup(address: dword);
var i,j: integer;
    addressofmenuitemstring: pchar;
    s: string;
begin
  pluginMREW.BeginRead;
  try
    for i:=0 to length(plugins)-1 do
      for j:=0 to length(plugins[i].RegisteredFunctions6)-1 do
      begin
        s:=plugins[i].RegisteredFunctions6[j].menuitem.Caption;
        addressofmenuitemstring:=@s[1];
        plugins[i].RegisteredFunctions6[j].callbackOnContext(address, @addressofmenuitemstring);
        plugins[i].RegisteredFunctions6[j].menuitem.Caption:=addressofmenuitemstring;
      end;
  finally
    pluginMREW.EndRead;
  end;
end;

procedure TPluginHandler.handledisassemblerplugins(address: dword; addressStringPointer: pointer; bytestringpointer: pointer; opcodestringpointer: pointer; specialstringpointer: pointer; textcolor: PColor);
var i,j: integer;
begin
  pluginMREW.BeginRead;
  try
    for i:=0 to length(plugins)-1 do
      for j:=0 to length(plugins[i].RegisteredFunctions7)-1 do
        plugins[i].RegisteredFunctions7[j].callback(address, addressStringPointer, bytestringpointer, opcodestringpointer, specialstringpointer, textcolor);
  finally
    pluginMREW.EndRead;
  end;
end;

function TPluginHandler.handledebuggerplugins(devent: PDebugEvent):integer;
var i,j: integer;
begin
  result:=0;
  pluginMREW.BeginRead;
  try
    for i:=0 to length(plugins)-1 do
      for j:=0 to length(plugins[i].RegisteredFunctions2)-1 do
        if plugins[i].RegisteredFunctions2[j].callback(devent)=1 then result:=1;
  finally
    pluginMREW.EndRead;
  end;
end;

function TPluginHandler.handlenewprocessplugins(processid: dword; peprocess:dword; created: boolean):boolean;
var i,j: integer;
begin
  result:=true;
  pluginMREW.BeginRead;
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
    pluginMREW.EndRead;
  end;
end;

function TPluginHandler.handlechangedpointers(section: integer):boolean;
var i,j: integer;
begin
  result:=true;
  pluginMREW.BeginRead;
  try
    for i:=0 to length(plugins)-1 do
      for j:=0 to length(plugins[i].RegisteredFunctions4)-1 do
        plugins[i].RegisteredFunctions4[j].callback(section);
  finally
    pluginMREW.EndRead;
  end;
end;


constructor TPluginHandler.create;
var test: pchar;
begin
  pluginMREW:=TMultiReadExclusiveWriteSynchronizer.Create;
  exportedfunctions.sizeofExportedFunctions:=sizeof(TExportedFunctions3);
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
  exportedfunctions.ReadProcessMemory:=@@ReadProcessMemory;
  exportedfunctions.WriteProcessMemory:=@@WriteProcessMemory;
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
  exportedfunctions.GetPEProcess:=@@GetPEProcess;
  exportedfunctions.GetPEThread:=@@GetPEThread;
  exportedfunctions.GetThreadsProcessOffset:=@@GetThreadsProcessOffset;
  exportedfunctions.GetThreadListEntryOffset:=@@GetThreadListEntryOffset;
  exportedfunctions.GetProcessnameOffset:=@@GetProcessnameOffset;
  exportedfunctions.GetDebugportOffset:=@@GetDebugportOffset;
  exportedfunctions.GetPhysicalAddress:=@@GetPhysicalAddress;
  exportedfunctions.ProtectMe:=nil;
  exportedfunctions.GetCR4:=@@GetCR4;
  exportedfunctions.GetCR3:=@@GetCR3;
  exportedfunctions.SetCR3:=nil; //@@SetCR3;
  exportedfunctions.GetSDT:=@@GetSDT;
  exportedfunctions.GetSDTShadow:=@@GetSDTShadow;
  exportedfunctions.setAlternateDebugMethod:=nil; //@@setAlternateDebugMethod;
  exportedfunctions.getAlternateDebugMethod:=nil; //@@getAlternateDebugMethod;
  exportedfunctions.DebugProcess:=nil; //@@DebugProcess;
  exportedfunctions.ChangeRegOnBP:=nil; //@@ChangeRegOnBP;
  exportedfunctions.RetrieveDebugData:=nil; //@@RetrieveDebugData;
  exportedfunctions.StartProcessWatch:=@@StartProcessWatch;
  exportedfunctions.WaitForProcessListData:=@@WaitForProcessListData;
  exportedfunctions.GetProcessNameFromID:=@@GetProcessNameFromID;
  exportedfunctions.GetProcessNameFromPEProcess:=@@GetProcessNameFromPEProcess;
  exportedfunctions.KernelOpenProcess:=@@KernelOpenProcess;
  exportedfunctions.KernelReadProcessMemory:=@@KernelReadProcessMemory;
  exportedfunctions.KernelWriteProcessMemory:=@@KernelWriteProcessMemory;
  exportedfunctions.KernelVirtualAllocEx:=@@KernelVirtualAllocEx;
  exportedfunctions.IsValidHandle:=@@IsValidHandle;
  exportedfunctions.GetIDTCurrentThread:=@@GetIDTCurrentThread;
  exportedfunctions.GetIDTs:=@@GetIDTs;
  exportedfunctions.MakeWritable:=@@MakeWritable;
  exportedfunctions.GetLoadedState:=@@GetLoadedState;
  exportedfunctions.DBKSuspendThread:=@@DBKSuspendThread;
  exportedfunctions.DBKResumeThread:=@@DBKResumeThread;
  exportedfunctions.DBKSuspendProcess:=@@DBKSuspendProcess;
  exportedfunctions.DBKResumeProcess:=@@DBKResumeProcess;
  exportedfunctions.KernelAlloc:=@@KernelAlloc;
  exportedfunctions.GetKProcAddress:=@@GetKProcAddress;

  exportedfunctions.CreateToolhelp32Snapshot:=@@CreateToolhelp32Snapshot;
  exportedfunctions.Process32First:=@@Process32First;
  exportedfunctions.Process32Next:=@@Process32Next;
  exportedfunctions.Thread32First:=@@Thread32First;
  exportedfunctions.Thread32Next:=@@Thread32Next;
  exportedfunctions.Module32First:=@@Module32First;
  exportedfunctions.Module32Next:=@@Module32Next;
  exportedfunctions.Heap32ListFirst:=@@Heap32ListFirst;
  exportedfunctions.Heap32ListNext:=@@Heap32ListNext;



  //give the address of the variable since there is a change they arn't initialized just yet...
  exportedfunctions.mainform:=@mainform;
  exportedfunctions.memorybrowser:=@memorybrowser;

  //version2 init:
  exportedfunctions.sym_nameToAddress:=@ce_sym_nameToAddress;
  exportedfunctions.sym_addressToName:=@ce_sym_addressToName;
  exportedfunctions.sym_generateAPIHookScript:=@ce_generateAPIHookScript;

  //version3 init
  exportedfunctions.loadDBK32:=@LoadDBK32;
  exportedfunctions.loaddbvmifneeded:=@loaddbvmifneeded;
  exportedfunctions.previousOpcode:=@ce_previousOpcode;
  exportedfunctions.nextOpcode:=@ce_nextOpcode;
  exportedfunctions.disassembleEx:=@ce_disassemble;
  exportedfunctions.loadModule:=@ce_loadModule;

  exportedfunctions.aa_AddExtraCommand:=@aa_AddExtraCommand;
  exportedfunctions.aa_RemoveExtraCommand:=@aa_RemoveExtraCommand;

end;

end.


