unit plugin;

interface

uses sysutils,windows,checklst,menus,dialogs,pluginexports,cefuncproc,newkernelhandler;

const CurrentPluginVersion=2;

//structures
type TPluginVersion = record
  version: dword; //version number of ce plugin it is developed for (compatibility for the future)
  pluginname: pchar; //pointer to a 0-terminated string in the dll
end;
type PPluginVersion=^TPluginVersion;

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
end;
type PExportedFunctions1 = ^TExportedFunctions1; 

//exported functions of the plugin
type TGetVersion=function(var PluginVersion:TPluginVersion; TPluginVersionSize: integer):BOOL; stdcall;
type TInitializePlugin=function(var ExportedFunctions: TExportedFunctions2; pluginid: dword):BOOL; stdcall;
type TDisablePlugin=function:BOOL; stdcall;


//plugin type 1:
//where: rightclick on the address list in the menu plugin, user activated
type TPlugin1_SelectedRecord=record
  interpretedaddress: pchar; //pointer to a 255 bytes long string (0 terminated)
  address: dword; //this is a read-only representaion of the address. Change interpretedaddress if you want to change this
  ispointer: boolean; //readonly
  countoffsets: integer; //readonly
  offsets: PDWordArray; //pointer to a array of dwords randing from 0 to countoffsets-1 (readonly)
  description: pchar; //pointer to a 255 bytes long string
  valuetype: byte;
  size: byte; //stringlenth or bitlength (max 255);
end;
type PPlugin1_SelectedRecord=^TPlugin1_SelectedRecord;
type TPluginfunction1=function(selectedrecord: PPlugin1_SelectedRecord):bool; stdcall;

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
//where: menu bar under plugins in memory view, user activated
type TPluginfunction2=function(disassembleraddress: pdword; selected_disassembler_address: pdword; hexviewaddress:pdword ):bool; stdcall;

//private plugin data
type TPluginfunctionType2=class
  public
    pluginid: integer;
    functionid: integer;
    name:string;
    callback: TPluginfunction2;
    menuitem: TMenuItem;
end;


//plugin type 3:
//where: when a debug event happens
type TPluginFunction3=function(debugevent: PDebugEvent):integer; stdcall; //return 0 if you want to let ce handle it as well, 1 if you don't want to let ce handle it as well  (in case of not handling, do ContinueDebugEvent yourself)
type TPluginfunctionType3=class
  public
    pluginid: integer;
    functionid: integer;
    callback: TPluginfunction3;
end;

//plugin type 4:
//where: a new process created according to the processwatcher
type TPluginFunction4=function(processid: dword; peprocess:dword):integer; stdcall;
type TPluginfunctionType4=class
  public
    pluginid: integer;
    functionid: integer;
    callback: TPluginfunction4;
end;

//plugin type 5:
//where: Functionpointerchange notification
type TPluginFunction5=function(section: integer):boolean; stdcall;
type TPluginfunctionType5=class
  public
    pluginid: integer;
    functionid: integer;
    callback: TPluginfunction5;
end;

type TPlugin = record
  dllname: string;
  filepath: string;
  hmodule: thandle;
  name: string;
  enabled: boolean;
  GetVersion: TGetVersion;
  EnablePlugin: TInitializePlugin;
  DisablePlugin: TDisablePlugin;
  nextid: integer;
  RegisteredFunctions1: array of TPluginfunctionType1;
  RegisteredFunctions2: array of TPluginfunctionType2;
  RegisteredFunctions3: array of TPluginfunctionType3;
  RegisteredFunctions4: array of TPluginfunctionType4;
  RegisteredFunctions5: array of TPluginfunctionType5;  
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
    function handledebuggerplugins(devent:PDebugEvent):integer;
    function handlenewprocessplugins(processid: dword; peprocess:dword):boolean;
    function handlechangedpointers(section: integer):boolean;
    function registerfunction(pluginid,functiontype:integer; init: pointer):integer;
    function unregisterfunction(pluginid,functionid: integer): boolean;
    property dllfilepath[pluginid: integer]: string read getdllfilepath;
    constructor create;
end;

var pluginhandler: TPluginhandler;
    exportedfunctions: TExportedFunctions2;

implementation

uses mainunit,memorybrowserformunit,formsettingsunit;

function TPluginHandler.GetDLLFilePath(pluginid: integer):string;
begin
  pluginMREW.BeginRead;
  result:=plugins[pluginid].filepath;
  pluginMREW.EndRead;
end;

function TPluginHandler.registerfunction(pluginid,functiontype:integer; init: pointer):integer;
type Tfunction1=record
  name: pchar;
  callbackroutine: pointer;
end;
type Tfunction3=record
  callbackroutine: pointer;
end;
type PFunction1=^TFunction1;
type PFunction2=^TFunction1;   //same
type PFunction3=^TFunction3;
type PFunction4=^TFunction3;
type PFunction5=^TFunction3;

var i: integer;
    newmenuitem: TMenuItem;
    f1: TPluginfunctionType1;
    f2: TPluginfunctionType2;
    f3: TPluginfunctionType3;
    f4: TPluginfunctionType4;
    f5: TPluginfunctionType5;
begin
  result:=-1;

  pluginmrew.BeginWrite;
  try
    if pluginid>=length(plugins) then exit;

    case functiontype of
      0: begin
           //plugin for the rightclick on the addresslist
           f1:=TPluginfunctionType1.Create;
           f1.pluginid:=pluginid;
           f1.functionid:=plugins[pluginid].nextid;
           f1.name:=PFunction1(init).name;
           f1.callback:=PFunction1(init).callbackroutine;

           if not mainform.Plugins1.Visible then
             mainform.Plugins1.Visible:=true;

           newmenuitem:=tmenuitem.Create(mainform);
           newmenuitem.Caption:=f1.name;
           newmenuitem.Tag:=dword(f1);
           newmenuitem.onclick:=mainform.plugintype1click;
           mainform.Plugins1.Add(newmenuitem);

           f1.menuitem:=newmenuitem;

           setlength(plugins[pluginid].RegisteredFunctions1,length(plugins[pluginid].RegisteredFunctions1)+1);
           plugins[pluginid].RegisteredFunctions1[length(plugins[pluginid].RegisteredFunctions1)-1]:=f1;

           result:=plugins[pluginid].nextid;
         end;

      1: begin
           //plugin for the memorybrowser
           f2:=TPluginfunctionType2.Create;
           f2.pluginid:=pluginid;
           f2.functionid:=plugins[pluginid].nextid;
           f2.name:=Pfunction2(init).name;
           f2.callback:=Pfunction2(init).callbackroutine;

           if not memorybrowser.Plugins1.Visible then
             memorybrowser.Plugins1.Visible:=true;

           newmenuitem:=tmenuitem.Create(mainform);
           newmenuitem.Caption:=f2.name;
           newmenuitem.Tag:=dword(f2);
           newmenuitem.onclick:=memorybrowser.plugintype2click;
           memorybrowser.Plugins1.Add(newmenuitem);

           f2.menuitem:=newmenuitem;

           setlength(plugins[pluginid].Registeredfunctions2,length(plugins[pluginid].Registeredfunctions2)+1);
           plugins[pluginid].Registeredfunctions2[length(plugins[pluginid].Registeredfunctions2)-1]:=f2;

           result:=plugins[pluginid].nextid;
         end;

      2: begin
           //debugger for the memorybrowser
           f3:=TPluginfunctionType3.Create;
           f3.pluginid:=pluginid;
           f3.functionid:=plugins[pluginid].nextid;
           f3.callback:=Pfunction3(init).callbackroutine;

           setlength(plugins[pluginid].Registeredfunctions3,length(plugins[pluginid].Registeredfunctions3)+1);
           plugins[pluginid].Registeredfunctions3[length(plugins[pluginid].Registeredfunctions3)-1]:=f3;

           result:=plugins[pluginid].nextid;
         end;

      3: begin
           //process created
           f4:=TPluginfunctionType4.Create;
           f4.pluginid:=pluginid;
           f4.functionid:=plugins[pluginid].nextid;
           f4.callback:=Pfunction3(init).callbackroutine;

           setlength(plugins[pluginid].RegisteredFunctions4, length(plugins[pluginid].Registeredfunctions4)+1);
           plugins[pluginid].Registeredfunctions4[length(plugins[pluginid].Registeredfunctions4)-1]:=f4;

           result:=plugins[pluginid].nextid;
         end;

      4: begin
           //function pointers changed
           f5:=TPluginfunctionType5.Create;
           f5.pluginid:=pluginid;
           f5.functionid:=plugins[pluginid].nextid;
           f5.callback:=Pfunction3(init).callbackroutine;

           setlength(plugins[pluginid].RegisteredFunctions5, length(plugins[pluginid].Registeredfunctions5)+1);
           plugins[pluginid].Registeredfunctions5[length(plugins[pluginid].Registeredfunctions5)-1]:=f5;

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
    f: ^TPluginfunctionType1;
begin
  //remove it
  result:=false;
  pluginmrew.BeginWrite;
  try
    if pluginid>=length(plugins) then exit;

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
        if plugins[pluginid].RegisteredFunctions2[i].menuitem.Parent<>nil then
        begin
          if plugins[pluginid].RegisteredFunctions2[i].menuitem.Parent.Count=1 then
            plugins[pluginid].RegisteredFunctions2[i].menuitem.Parent.Visible:=false;
        end;
        plugins[pluginid].RegisteredFunctions2[i].menuitem.Free;
        plugins[pluginid].RegisteredFunctions2[i].Free;

        for j:=i to length(plugins[pluginid].RegisteredFunctions2)-2 do
          plugins[pluginid].RegisteredFunctions2[j]:=plugins[pluginid].RegisteredFunctions2[j+1];

        setlength(plugins[pluginid].RegisteredFunctions2,length(plugins[pluginid].RegisteredFunctions2)-1);

        result:=true;
        exit;
      end;

    //function3 check
    for i:=0 to length(plugins[pluginid].RegisteredFunctions3)-1 do
      if plugins[pluginid].RegisteredFunctions3[i].functionid=functionid then
      begin
        plugins[pluginid].RegisteredFunctions3[i].Free;

        for j:=i to length(plugins[pluginid].RegisteredFunctions3)-2 do
          plugins[pluginid].RegisteredFunctions3[j]:=plugins[pluginid].RegisteredFunctions3[j+1];

        setlength(plugins[pluginid].RegisteredFunctions3,length(plugins[pluginid].RegisteredFunctions3)-1);

        result:=true;
        exit;
      end;

    //function4 check (processwatcher)
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

    //function5 check (changed pointers)
    for i:=0 to length(plugins[pluginid].RegisteredFunctions5)-1 do
      if plugins[pluginid].RegisteredFunctions5[i].functionid=functionid then
      begin
        plugins[pluginid].RegisteredFunctions5[i].Free;

        for j:=i to length(plugins[pluginid].RegisteredFunctions5)-2 do
          plugins[pluginid].RegisteredFunctions5[j]:=plugins[pluginid].RegisteredFunctions5[j+1];

        setlength(plugins[pluginid].RegisteredFunctions5,length(plugins[pluginid].RegisteredFunctions5)-1);

        result:=true;
        exit;
      end;

  finally
    pluginmrew.EndWrite;
  end;
end;

procedure TPluginHandler.EnablePlugin(pluginid: integer);
var e: texportedfunctions2;
    x: boolean;
begin
  e:=exportedfunctions;  //save it to prevent plugins from fucking it up

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
begin
  pluginMREW.BeginRead;
  try
    if plugins[pluginid].enabled then
    begin
      if not plugins[pluginid].DisablePlugin() then raise exception.Create('Error disabling '+plugins[pluginid].dllname);
      plugins[pluginid].enabled:=false;
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

function TPluginHandler.handledebuggerplugins(devent: PDebugEvent):integer;
var i,j: integer;
begin
  result:=0;
  pluginMREW.BeginRead;
  try
    for i:=0 to length(plugins)-1 do
      for j:=0 to length(plugins[i].RegisteredFunctions3)-1 do
        if plugins[i].RegisteredFunctions3[j].callback(devent)=1 then result:=1;
  finally
    pluginMREW.EndRead;
  end;
end;

function TPluginHandler.handlenewprocessplugins(processid: dword; peprocess:dword):boolean;
var i,j: integer;
begin
  result:=true;
  pluginMREW.BeginRead;
  try
    for i:=0 to length(plugins)-1 do
      for j:=0 to length(plugins[i].RegisteredFunctions4)-1 do
        plugins[i].RegisteredFunctions4[j].callback(processid,peprocess);
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
      for j:=0 to length(plugins[i].RegisteredFunctions5)-1 do
        plugins[i].RegisteredFunctions5[j].callback(section);
  finally
    pluginMREW.EndRead;
  end;
end;


constructor TPluginHandler.create;
var test: pchar;
begin
  pluginMREW:=TMultiReadExclusiveWriteSynchronizer.Create;
  exportedfunctions.sizeofExportedFunctions:=sizeof(TExportedFunctions2);
  exportedfunctions.showmessage:=@ce_showmessage;
  exportedfunctions.registerfunction:=@ce_registerfunction;
  exportedfunctions.unregisterfunction:=@ce_unregisterfunction;
  exportedfunctions.OpenedProcessID:=@processid;
  exportedfunctions.OpenedProcessHandle:=@processhandle;

  exportedfunctions.GetMainWindowHandle:=@ce_GetMainWindowHandle;
  exportedfunctions.AutoAssemble:=@ce_autoassemble;
  exportedfunctions.assembler:=@ce_assembler;
  exportedfunctions.disassembler:=@ce_disassembler;
  exportedfunctions.ChangeRegistersAtAddress:=@ce_ChangeRegistersAtAddress;
  exportedfunctions.InjectDLL:=@ce_ChangeRegistersAtAddress;
  exportedfunctions.freezemem:=@ce_freezemem;
  exportedfunctions.unfreezemem:=@ce_unfreezemem;
  exportedfunctions.fixmem:=@ce_fixmem;
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
  exportedfunctions.StopDebugging:=@@StopDebugging;
  exportedfunctions.StopRegisterChange:=@@StopRegisterChange;
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
  exportedfunctions.ProtectMe:=@@ProtectMe;
  exportedfunctions.GetCR4:=@@GetCR4;
  exportedfunctions.GetCR3:=@@GetCR3;
  exportedfunctions.SetCR3:=@@SetCR3;
  exportedfunctions.GetSDT:=@@GetSDT;
  exportedfunctions.GetSDTShadow:=@@GetSDTShadow;
  exportedfunctions.setAlternateDebugMethod:=@@setAlternateDebugMethod;
  exportedfunctions.getAlternateDebugMethod:=@@getAlternateDebugMethod;
  exportedfunctions.DebugProcess:=@@DebugProcess;
  exportedfunctions.ChangeRegOnBP:=@@ChangeRegOnBP;
  exportedfunctions.RetrieveDebugData:=@@RetrieveDebugData;
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
end;

end.
