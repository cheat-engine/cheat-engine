unit exportimplementation;

interface

uses windows,sysutils,forms,StdCtrls,ExtCtrls, cepluginsdk;


function GetVersion(var PluginVersion:TpluginVersion; sizeofpluginversion:integer):BOOL; stdcall;
function InitializePlugin(ExportedFunctions: PExportedFunctions; pluginid: dword):BOOL; stdcall;
function DisablePlugin:BOOL; stdcall;

var versionname: pchar;
    ce_exported: TExportedFunctions;
    thispluginid: integer;
    
implementation

uses frmEventLogUnit;



function Menuitemclick(disassembleraddress: DWORD; selected_disassembler_address: PDWORD; hexviewaddress: PDWORD): BOOL; stdcall;
begin
  if frmEventLog=nil then
  begin
    TControlwindowThread.create(false);
  end
  else
  begin
    SetForegroundWindow(frmEventLog.Handle);
  end;

  result:=false;
end;

function GetVersion(var PluginVersion:TpluginVersion; sizeofpluginversion:integer):BOOL; stdcall;
var s: string;
begin
  result:=false;
  if sizeofpluginversion<>sizeof(TPluginVersion) then exit;


  s:='Basic debug event list for CE v5.5';
  getmem(versionname,length(s)+1);
  copymemory(versionname,@s[1],length(s));
  versionname[length(s)]:=#0;
  
  PluginVersion.version:=2;
  PluginVersion.pluginname:=VersionName;


  result:=true;
end;

function InitializePlugin(ExportedFunctions: PExportedFunctions; pluginid: dword):BOOL; stdcall;
var init: Tfunction1;
begin
  ce_exported:=ExportedFunctions^;
  thispluginid:=pluginid;


  init.callbackroutine:=@Menuitemclick;
  init.name:='View debug events';
  init.shortcut:='Shift+Ctrl+D';
  ce_exported.registerfunction(thispluginid, ptMemoryView, @init);

  result:=true;
end;

function DisablePlugin:BOOL; stdcall;
begin
  if frmEventLog<>nil then frmEventLog.Close;    
  result:=true;
end;

end.

