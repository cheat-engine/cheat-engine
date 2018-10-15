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
    br: ptruint;
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
        r.Add('unregisterSymbol([['+ChangeFileExt(mname,'')+'.'+symbolname+']])');

        r.Add('registerSymbol([['+symbolname+']], '+address+')');
        r.Add('registerSymbol([['+mname+'!'+symbolname+']], '+address+')');
        r.Add('registerSymbol([['+ChangeFileExt(mname,'')+'.'+symbolname+']], '+address+')');
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
  {
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
  }

  //ce_exported.showmessage(pchar(script.text));


 // getmem(s,16*1024);

  //ce_exported.ce_generateAPIHookScript('SymEnumerateModules64', , , s, 16*1024);


 // script.free;


  //ce_exported.openProcessEx(originalpid);
  lua_dostring(luavm,'reinitializeSymbolhandler()');




  result:=true;
end;

function CEPlugin_DisablePlugin:BOOL; stdcall;
begin
  result:=true;
end;

end.
