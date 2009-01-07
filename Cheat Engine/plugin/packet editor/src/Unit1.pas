unit Unit1;

interface

uses windows,sysutils,forms,StdCtrls,ExtCtrls, cepluginsdk;


function GetVersion(var PluginVersion:TpluginVersion; sizeofpluginversion:integer):BOOL; stdcall;
function InitializePlugin(ExportedFunctions: PExportedFunctions; pluginid: dword):BOOL; stdcall;
function DisablePlugin:BOOL; stdcall;

var versionname: pchar;
    ce_exported: TExportedFunctions;

var processwatchevent: integer;

implementation

uses injector;

function GetVersion(var PluginVersion:TpluginVersion; sizeofpluginversion:integer):BOOL; stdcall;
var s: string;
begin
  result:=false;
  if sizeofpluginversion<>sizeof(TPluginVersion) then exit;


  s:='Packet editor plugin for CE v5.5';
  getmem(versionname,length(s)+1);
  copymemory(versionname,@s[1],length(s));
  versionname[length(s)]:=#0;
  
  PluginVersion.version:=2;
  PluginVersion.pluginname:=VersionName;


  result:=true;
end;

function InitializePlugin(ExportedFunctions: PExportedFunctions; pluginid: dword):BOOL; stdcall;
var init: TFunction5;
begin
  ce_exported:=ExportedFunctions^;

  //spawn a new thread that'll show the controlwindow
  init.name:='Attach packet editor to process';
  init.shortcut:='Ctrl+Alt+P';
  init.callbackroutine:=@InjectPacketEditor;
  ce_exported.registerfunction(pluginid, ptMainMenu, @init);
  //controlwindowthread:=TControlwindowThread.create(false);

  result:=true;
end;

function DisablePlugin:BOOL; stdcall;
begin
  messagebox(0,'DisablePlugin Called','Example PE plugin',mb_ok);
  result:=true;
end;

end.
 