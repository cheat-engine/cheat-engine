unit pluginconfig;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses windows, sysutils, cepluginsdk, Contnrs, syncobjs, StrUtils,
  classes, lua, lauxlib, lualib;

function CEPlugin_GetVersion(var PluginVersion:TpluginVersion; sizeofpluginversion:integer):BOOL; stdcall;
function CEPlugin_InitializePlugin(ExportedFunctions: PExportedFunctions; pluginid: dword):BOOL; stdcall;
function CEPlugin_DisablePlugin:BOOL; stdcall;


function GetModuleFileNameEx(hProcess: HANDLE; hModule: HMODULE; lpFilename: pchar; nSize: DWORD): DWORD; stdcall; external 'psapi.dll' name 'GetModuleFileNameExA';


type PHModule=^HModule;

type TModuleEnumCallback=function (ModuleName:PSTR; BaseOfDll:dword64; UserContext:pointer):bool;stdcall;
function FixedSymEnumerateModules64(hProcess:THANDLE; callback: TModuleEnumCallback; UserContext:pointer):BOOL;stdcall;

type TSymbolEnumCallback=function (SymbolName:PSTR; SymbolAddress:dword64; SymbolSize:ULONG; UserContext:pointer):bool;stdcall;

//function SymEnumerateSymbols64(hProcess:THANDLE; BaseOfDll:dword64; EnumSymbolsCallback:TSYM_ENUMSYMBOLS_CALLBACK64; UserContext:pointer):BOOL;stdcall;external External_library name 'SymEnumerateSymbols64';




type TCreateRemoteThread=function(hProcess: THandle; lpThreadAttributes: Pointer; dwStackSize: DWORD; lpStartAddress: TFNThreadStartRoutine; lpParameter: Pointer;  dwCreationFlags: DWORD; var lpThreadId: DWORD): THandle; stdcall;
type TVirtualAllocEx=function(hProcess: THandle; lpAddress: Pointer; dwSize, flAllocationType: DWORD; flProtect: DWORD): Pointer; stdcall;

var versionname: pchar;
    ce_exported: TExportedFunctions;
    luavm: Plua_State;
    thispluginid: integer;

    OriginalCRT: TCreateRemoteThread;
    OriginalVAE: TVirtualAllocEx;
    ModuleBase: pointer;

    isWow64Process: function (processhandle: THandle; var isWow: BOOL): BOOL; stdcall;

function forceModule(path: string): boolean;

implementation

function MyVirtualAllocEx(hProcess: THandle; lpAddress: Pointer; dwSize, flAllocationType: DWORD; flProtect: DWORD): Pointer; stdcall;
begin
  result:=OriginalVAE(hProcess, lpAddress, dwSize, flAllocationType, flProtect);

  if (ModuleBase=nil) and (dwSize>4096) and (hProcess<>-1) then
    modulebase:=result;
end;

function myCreateRemoteThread(hProcess: THandle; lpThreadAttributes: Pointer; dwStackSize: DWORD; lpStartAddress: TFNThreadStartRoutine; lpParameter: Pointer;  dwCreationFlags: DWORD; var lpThreadId: DWORD): THandle; stdcall;
var base: PtrUInt;
    path: pchar;
    br: dword;
    script: tstringlist;
    success: boolean;

    header: PImageDosHeader;
    ImageNTHeader: PImageNtHeaders;

    entrypoint: ptruint;

begin
  //check the injected code to see if it's an dll injection or a control command
  //how: The injectdll routine allocates a block of memory and in there first writes the path to the dll, followed by the code to load it.
  //This means that if we check the beginning of this block for a file path, and it points to an existing file, it's a dll injection

  //Problem1: the pluginhandler's loadModule does not automatically execute the entrypoint.
  //Solution, use the file to read the pe header and find out where the entrypoint is

  //Problem2: The base address is unknown
  //Solution: Hook VirtualAllocEx and watch when loadModule is executed for allocs in the OpenedProcessHandle (should be 1)

  //Problem3: .sys files do not use VirtualAllocEx but KernelAlloc(64)
  //Solution: Screw them



  //ce_exported.showmessage('Creating a new thread in the target process');

 // r.Add('registerSymbol([['+symbolname+']], '+address+')');



  base:=ptruint(lpStartAddress) and $fffffffffffff000;

  getmem(path,MAX_PATH);

  if ReadProcessMemory(ce_exported.OpenedProcessHandle^, pointer(base), path, MAX_PATH, br) then
  begin
    if FileExists(path) then
    begin
      //it's a dll injection

      //hook VirtualAllocEx to find out where it allocates it. (todo: Update the plugin handler to tell me that info for next version, it's already there in the moduleloader class)




      ModuleBase:=nil;

      OriginalVAE:=TVirtualAllocEx(ce_exported.VirtualAllocEx^);
      TVirtualAllocEx(ce_exported.VirtualAllocEx^):=@MyVirtualAllocEx;

      success:=ForceModule(path);


      TVirtualAllocEx(ce_exported.VirtualAllocEx^):=OriginalVAE; //remove the hook


      if success and (modulebase<>nil) then
      begin
       // ce_exported.showmessage(pchar('Module loaded at :'+inttohex(dword(modulebase),8)));
        //get the entrypoint

        getmem(header, 4096);
        if ReadProcessMemory(ce_exported.OpenedProcessHandle^, modulebase, header, 4096, br) then
        begin
          if header.e_magic=IMAGE_DOS_SIGNATURE then
          begin
            if header._lfanew<4096 then
            begin
              ImageNTHeader:=PImageNtHeaders(ptruint(header)+header._lfanew);

              entrypoint:=ptruint(modulebase)+ImageNTHeader.OptionalHeader.AddressOfEntryPoint;
              if entrypoint>base then
              begin

                script:=TStringList.create;
                script.add('alloc(dllentry,1024)');
                script.Add('createThread(dllentry)');
                script.add('');
                script.add('dllentry:');
                script.add('push 0'); //lpvreserved
                script.add('push 1'); //fdwReason (DLL_PROCESS_ATTACH)
                script.add('push '+inttohex(ptruint(modulebase),8));    //hinstance/hmodule
                script.add('call '+inttohex(entrypoint,8));
                script.add('ret');

                ce_exported.AutoAssemble(pchar(script.text));
               //ce_exported.showmessage(pchar(script.text));

                script.free;
              end;
            end;

           // IMAGE_OPTIONAL_HEADER32
           // ce_exported.showmessage('ok');;

          end; //else no valid header (bug?)
        end;
      end;






      //replace the injection script with a script that returns success (exitcode 1)
      script:=TStringList.create;
      script.Add(IntToHex(ptruint(lpStartAddress),8)+':');
      if success then
        script.add('mov eax,1')
      else
        script.add('mov eax,2');

      script.add('ret');
      ce_exported.AutoAssemble(pchar(script.text));

      script.free;
    end;
  end;

  freemem(path);



  //execute the thread (either the original, or modified one)
  result:=OriginalCRT(hProcess, lpThreadAttributes, dwStackSize, lpStartAddress, lpParameter, dwCreationFlags, lpThreadId);


end;

function forceModule(path: string): boolean;
var
  exportlist: pchar;
  i,j: integer;
  maxsize: integer;

  exlist: tstringlist;
  r: tstringlist;
  symbolname, address: string;
  s: string;
  mname: string;
  is64bit: boolean;

begin
  result:=false;



  mname:=ExtractFileName(path);
  maxsize:=64*1024; //64kb should be enough for most exports...
  getmem(exportlist, maxsize);
  if ce_exported.loadModule(pchar(path), exportlist, @maxsize) then
  begin

    exportlist[maxsize]:=#0;

    //parse the exportlist and add these addresses to the symbollist
//        ce_exported.showMessage(exportlist);
    exlist:=tstringlist.create;
    exlist.Text:=exportlist;



    r:=tstringlist.create;

    for i:=0 to exlist.count-1 do
    begin
      s:=exlist[i];

      //ce_exported.showmessage(pchar(s));

      j:=Pos(' - ', s);
      if j<>0 then
      begin
        address:='0x'+copy(s, 1, j);
        symbolname:=copy(s,j+3, length(s));

       // ce_exported.showmessage(pchar(address+' - '+symbolname));

        r.Add('unregisterSymbol([['+symbolname+']])');
        r.Add('unregisterSymbol([['+mname+'!'+symbolname+']])');
        r.Add('unregisterSymbol([['+mname+'.'+symbolname+']])');

        r.Add('registerSymbol([['+symbolname+']], '+address+')');
        r.Add('registerSymbol([['+mname+'!'+symbolname+']], '+address+')');
        r.Add('registerSymbol([['+mname+'.'+symbolname+']], '+address+')');
      end ;

    end;

    s:=r.text;

   // ce_exported.showmessage(pchar(s));
    lua_dostring(luavm, pchar(s));

    result:=true;

    r.free;
    exlist.free;


  end
  else
    ce_exported.showmessage(pchar('Failure loading module: '+path));

  freemem(exportlist);
end;

function lua_forceModule(L: PLua_State): integer; cdecl;
var
  parameters: Qword;
  p: string;

begin
  result:=0;
  try
    parameters:=lua_gettop(L);
    if parameters=1 then
    begin
      p:=lua_tostring(L,1);
      lua_pop(L, parameters);

      lua_pushboolean(L, forceModule(p));

      result:=1;
    end;
  except
    //this should really never happen
    result:=0;
    lua_pop(L, lua_gettop(L));
  end;
end;


function CEPlugin_GetVersion(var PluginVersion:TpluginVersion; sizeofpluginversion:integer):BOOL; stdcall;
var s: string;
begin
  result:=false;
  if sizeofpluginversion<>sizeof(TPluginVersion) then exit;

  s:='Forced Injection: Replaces the normal dll injector with a manual load version';

  getmem(versionname,length(s)+1);
  copymemory(versionname,@s[1],length(s));
  versionname[length(s)]:=#0;
  
  PluginVersion.version:=PluginVersionSDK;
  PluginVersion.pluginname:=VersionName;


  result:=true;
end;


type TSymEnumerateModules64=function(hProcess:THANDLE; EnumModulesCallback:TModuleEnumCallback; UserContext:pointer):BOOL;stdcall;


type TSymEnumerateSymbols64=function(hProcess:THANDLE; BaseOfDll:dword64; EnumSymbolsCallback:TSymbolEnumCallback; UserContext:pointer):BOOL;stdcall;



var
  OriginalSymEnumerateModules64: TSymEnumerateModules64;
  POriginalSymEnumerateModules64: pointer absolute OriginalSymEnumerateModules64;   //for some reason @OriginalSymEnumerateModules64 returns the value

  OriginalSymEnumerateSymbols64: TSymEnumerateSymbols64;
  POriginalSymEnumerateSymbols64: pointer absolute OriginalSymEnumerateSymbols64;   //for some reason @OriginalSymEnumerateModules64 returns the value



type TSymbolEnumcontext=record
       OriginalFunction: TSymbolEnumCallback;
       OriginalUserContext: pointer;
       modulename: string;
       hModule: dword64;
       modulesize: integer;
       moduledata: pbytearray;
     end;

  PSymbolEnumcontext=^TSymbolEnumcontext;


function MySymbolEnum(SymbolName:PSTR; SymbolAddress:dword64; SymbolSize:ULONG; UserContext:PSymbolEnumcontext):bool;stdcall;
var p: pchar;
  i: integer;
begin
  if usercontext.moduledata<>nil then
  begin
    //check if this symboladdress is a string to another symbol, or actually what I need

    if (Symboladdress-usercontext.hModule)<UserContext.modulesize then
    begin

      p:=@UserContext.moduledata[Symboladdress-usercontext.hModule];



      if uppercase(copy(p, 1, 6))='NTDLL.' then
      begin
        i:=lua_gettop(luavm);
        lua_dostring(Luavm, pchar('return getAddress("'+p+'")'));
        if lua_gettop(luavm)>i then
          SymbolAddress:=lua_tointeger(luavm, -1);

         lua_settop(luavm, i);
      end;

    end
    //else
    //  ce_exported.showmessage(pchar('Invalid offset. Modulesize='+inttohex(UserContext.modulesize,1)+' Offset='+inttohex(Symboladdress-usercontext.hModule,1)));

  end;

  result:=usercontext.OriginalFunction(SymbolName, SymbolAddress, SymbolSize, UserContext.OriginalUserContext);
end;

function FixedSymEnumerateSymbols64(hProcess:THANDLE; BaseOfDll:dword64; EnumSymbolsCallback:TSymbolEnumCallback; UserContext:pointer):BOOL;stdcall;
var c: TSymbolEnumcontext;
  i: integer;
  s: string;
  br,tr: dword;
  mbi: TMemoryBasicInformation;

  size: integer;
begin
  //make a copy of this module here for lookup
  c.OriginalFunction:=EnumSymbolsCallback;
  c.OriginalUserContext:=UserContext;
  c.hModule:=BaseOfDll;
  c.moduledata:=nil;
  c.modulesize:=0;


  i:=lua_gettop(Luavm);
  lua_dostring(Luavm, pchar('return getModuleSize(getNameFromAddress(0x'+intToHex(BaseOfDll,8)+'))'));
  if lua_gettop(luavm)>i then
  begin
    c.ModuleSize:=lua_tointeger(luavm, -1);
    lua_settop(luavm, i);

    if c.modulesize>0 then
    begin
      getmem(c.moduledata, c.modulesize);
      ZeroMemory(c.moduledata, c.modulesize);
      br:=0;

      ZeroMemory(@mbi, sizeof(mbi));

      size:=0;
      tr:=0;
      while VirtualQueryEx(hProcess, pointer(baseofdll+size), mbi, sizeof(mbi))>0 do
      begin
        br:=0;
        ReadProcessMemory(hProcess, pointer(baseofdll+size), @c.moduledata[size], min(mbi.RegionSize, c.modulesize-size), br);
        inc(tr,br);
        inc(size, mbi.RegionSize);
        if size>=c.modulesize then break;
      end;




      if tr=0 then
      begin
        ce_exported.showmessage(pchar('rpm fail. '+inttohex(baseofdll,8)+' : '+inttostr(c.modulesize)));
        freemem(c.moduledata);
        c.moduledata:=nil;
        c.modulesize:=0;
      end
      else
      begin
        //get the modulename seperately

        lua_dostring(Luavm, pchar('return getNameFromAddress(0x'+intToHex(BaseOfDll,8)+')'));
        if lua_gettop(luavm)>i then
        begin
          c.modulename:=ChangeFileExt(lua_tostring(luavm, -1),'');
          lua_settop(luavm, i);


         // ce_exported.showmessage(pchar(c.modulename));

          if uppercase(c.modulename)='NTDLL' then
          begin
            freemem(c.moduledata);
            c.moduledata:=nil;
            c.modulesize:=0;
          end;
        end;
      end;

    end;
  end;

  result:=OriginalSymEnumerateSymbols64(hProcess, BaseOfDll, @MySymbolEnum, @c);

  if c.moduledata<>nil then
    freemem(c.moduledata);
end;

type TModuleEnumcontext=record
       OriginalFunction: TModuleEnumCallback;
       OriginalUserContext: pointer;
       hProcess: Thandle;
       is32bit: BOOL;
     end;

  PModuleEnumcontext=^TModuleEnumcontext;


function MyModuleEnum(ModuleName:PSTR; BaseOfDll:dword64; UserContext:PModuleEnumcontext):bool;stdcall;
var path: pchar ;
  s: string;
begin
  {$ifdef cpu64}
  //if the target is 32-bit then get the modulepath of this dll and adjust the name if it's not the wow64
  if UserContext.is32bit then
  begin
   // ce_exported.showmessage(Modulename);
    if uppercase(modulename)='NTDLL' then
    begin
      getmem(path, 200);
      GetModuleFileNameEx(usercontext.hProcess,BaseOfDll,path,200);
      s:=uppercase(path);
      freemem(path);

      if pos('WOW64', s)=0 then
      begin
        //it's not the wow64 ntdll
        result:=UserContext.OriginalFunction('NTDLL64', BaseOfDll, UserContext.OriginalUserContext);
        exit;
      end;

    end;
  end;
  {$endif}

  result:=UserContext.OriginalFunction(ModuleName, BaseOfDll, UserContext.OriginalUserContext);

end;

function FixedSymEnumerateModules64(hProcess:THANDLE; callback: TModuleEnumCallback; UserContext:pointer):BOOL;stdcall;
var c: TModuleenumcontext;
  i: integer;
begin
  c.OriginalFunction:=callback;
  c.OriginalUserContext:=usercontext;
  c.hprocess:=hProcess;

  c.is32bit:=false;

  {$ifdef cpu64}
  isWow64Process(ce_exported.OpenedProcessHandle^, c.is32bit);
  {$endif}



  result:=OriginalSymEnumerateModules64(hProcess, @MyModuleEnum, @c);
end;

function CEPlugin_InitializePlugin(ExportedFunctions: PExportedFunctions; pluginid: dword):BOOL; stdcall;
var script: tstringlist;
  originalpid: dword;
  s: pchar;
  s2: string;
  z: pointer;
  z2: ptruint;
begin
  ce_exported:=ExportedFunctions^;
  thispluginid:=pluginid;

  luavm:=ce_exported.GetLuaState();

  IsWow64Process:=GetProcAddress(GetModuleHandle('kernel32.dll'), 'IsWow64Process');


  lua_register(LuaVM, 'forceModule', lua_forceModule);

  //hook createRemoteThread
  OriginalCRT:=TCreateRemoteThread(ce_exported.CreateRemoteThread^);
  TCreateRemoteThread(ce_exported.CreateRemoteThread^):=@MyCreateRemoteThread;


  //hook EnumProcessModulesEx to fix a bug in ce


  //Tce_generateAPIHookScript=function(address, addresstojumpto, addresstogetnewcalladdress: string; script: pchar; maxscriptsize: integer): BOOL; stdcall;

  originalpid:=ce_exported.OpenedProcessID^;
  ce_exported.openProcessEx(GetCurrentProcessId);

  script:=TStringList.create;
  script.add('reinitializeSymbolhandler()');


  z2:=ptruint(@POriginalSymEnumerateModules64);
  s2:=inttohex(z2,8);

  script.add('s=generateAPIHookScript("SymEnumerateModules64", "'+inttohex(ptruint(@FixedSymEnumerateModules64),8)+'", "'+s2+'")');
  script.add('autoAssemble(s)');


  z2:=ptruint(@POriginalSymEnumerateSymbols64);
  s2:=inttohex(z2,8);
  script.add('s=generateAPIHookScript("SymEnumerateSymbols64", "'+inttohex(ptruint(@FixedSymEnumerateSymbols64),8)+'", "'+s2+'")');
  script.add('autoAssemble(s)');




  lua_dostring(luavm,pchar(script.text));

  //ce_exported.showmessage(pchar(script.text));


  getmem(s,16*1024);

  //ce_exported.ce_generateAPIHookScript('SymEnumerateModules64', , , s, 16*1024);


  script.free;


  //ce_exported.openProcessEx(originalpid);
  lua_dostring(luavm,'reinitializeSymbolhandler()');




  result:=true;
end;

function CEPlugin_DisablePlugin:BOOL; stdcall;
begin
  result:=true;
end;

end.
