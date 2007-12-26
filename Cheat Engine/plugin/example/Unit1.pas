unit Unit1;

interface

uses windows,sysutils,forms,StdCtrls,ExtCtrls;

type Tfunction1=record
  name: pchar;
  callbackroutine: pointer;
end;
type Tfunction3=record
  callbackroutine: pointer;
end;
type PFunction1=^TFunction1;
type PFunction2=^TFunction1;
type PFunction3=^TFunction3;
type PFunction4=^TFunction3;
type PFunction5=^TFunction3;

type Tce_showmessage=procedure (s: pchar); stdcall;
type Tce_registerfunction=function (pluginid,functiontype:integer; init: pointer):integer; stdcall;
type Tce_unregisterfunction=function (pluginid,functionid: integer): boolean; stdcall;
type Tce_AutoAssembler=function (s: pchar):boolean; stdcall;
type Tce_GetMainWindowHandle=function:thandle; stdcall;

type TReadProcessMemory=function (hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer;
  nSize: DWORD; var lpNumberOfBytesRead: DWORD): BOOL; stdcall;

type TWriteProcessMemory=function (hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer;
  nSize: DWORD; var lpNumberOfBytesWritten: DWORD): BOOL; stdcall;

type TGetProcessNameFromPEProcess=function(peprocess:dword; buffer:pchar;buffersize:dword):integer; stdcall;
type TOpenProcess=function(dwDesiredAccess: DWORD; bInheritHandle: BOOL; dwProcessId: DWORD): THandle; stdcall;

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
end;

type PExportedFunctions=^TExportedFunctions;

type TDWordArray = array[0..0] of DWord;
     PDWordArray = ^TDWordArray;


type TSelectedRecord=record
  interpretedaddress: pchar; //pointer to a 255 bytes long string (0 terminated)
  address: dword; //this is a read-only representaion of the address. Change interpretedaddress if you want to change this
  ispointer: boolean;
  countoffsets: integer;
  offsets: PDWordArray; //pointer to a array of dwords randing from 0 to countoffsets-1
  description: pchar; //pointer to a 255 bytes long string
  valuetype: byte;
  size: byte; //stringlenth or bitlength (max 255);
end;
type PSelectedRecord=^TSelectedRecord;

function GetVersion(var PluginVersion:TpluginVersion; sizeofpluginversion:integer):BOOL; stdcall;
function InitializePlugin(ExportedFunctions: PExportedFunctions; pluginid: dword):BOOL; stdcall;
function DisablePlugin:BOOL; stdcall;


implementation

var versionname: pchar;
    ce_exported: TExportedFunctions;

var processwatchevent: integer;


procedure processwatcher_event(processid: DWORD; peprocess: DWORD); stdcall;
var pname: pchar;
    h: thandle;
begin
getmem(pname,100);
TGetProcessNameFromPEProcess(ce_exported.GetProcessNameFromPEProcess^)(peprocess,pname,100);

if (pname='winmine.exe') then
begin
  h:=TOpenProcess(ce_exported.OpenProcess^)(PROCESS_ALL_ACCESS,false, processid);


  ce_exported.OpenedProcessID^:=processid;
  ce_exported.OpenedProcessHandle^:=h;

  messageboxa(0,pname,'bla',mb_ok);
end;



end;


function GetVersion(var PluginVersion:TpluginVersion; sizeofpluginversion:integer):BOOL; stdcall;
var s: string;
begin
  result:=false;
  if sizeofpluginversion<>sizeof(TPluginVersion) then exit;


  s:='Test plugin for ce';
  getmem(versionname,length(s)+1);
  copymemory(versionname,@s[1],length(s));
  versionname[length(s)]:=#0;
  
  PluginVersion.version:=1;
  PluginVersion.pluginname:=VersionName;


  result:=true;
end;

function InitializePlugin(ExportedFunctions: PExportedFunctions; pluginid: dword):BOOL; stdcall;
var f: PFunction3;
begin
 // messagebox(0,'InitializePlugin Called','Auto Attach for maple plugin',mb_ok);
  ce_exported:=ExportedFunctions^;

 // ce_exported.showmessage('test. This was called from ce''s showmessage. (Not really usefull, but it works)');

  getmem(f,sizeof(TFunction3));
  f.callbackroutine:=pointer(@processwatcher_event);

  processwatchevent:=ce_exported.registerfunction(pluginid,3,f);
  if processwatchevent=-1 then
    ce_exported.showmessage('Failed registering the processwatcher event registerfunction');

  freemem(f);
  result:=true;
end;

function DisablePlugin:BOOL; stdcall;
begin
  messagebox(0,'DisablePlugin Called','Example plugin',mb_ok);
  result:=true;
end;

end.
 