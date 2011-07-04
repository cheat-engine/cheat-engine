unit exportimplementation;

{$MODE Delphi}

interface

uses windows,sysutils,forms,ExtCtrls, cepluginsdk;


function GetVersion(var PluginVersion:TpluginVersion; sizeofpluginversion:integer):BOOL; stdcall;
function InitializePlugin(ExportedFunctions: PExportedFunctions; pluginid: dword):BOOL; stdcall;
function DisablePlugin:BOOL; stdcall;

var versionname: pchar;
    ce_exported: TExportedFunctions;
    thispluginid: integer;
    
implementation

uses frmEventLogUnit;


function Menuitemclick(disassembleraddress: Pptruint; selected_disassembler_address: Pptruint; hexviewaddress: pptruint): BOOL; stdcall;
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

function MenuitemclickFromDisassembler(selectedaddress: pptruint): BOOL; stdcall;
begin
  result:=Menuitemclick(0, selectedaddress, selectedaddress);
end;


function GetVersion(var PluginVersion:TpluginVersion; sizeofpluginversion:integer):BOOL; stdcall;
var s: string;
begin
  result:=false;
  if sizeofpluginversion<>sizeof(TPluginVersion) then exit;


  s:='Basic debug event list for CE v6.0+';
  getmem(versionname,length(s)+1);
  copymemory(versionname,@s[1],length(s));
  versionname[length(s)]:=#0;
  
  PluginVersion.version:=PluginVersionSDK;
  PluginVersion.pluginname:=VersionName;


  result:=true;
end;

function InitializePlugin(ExportedFunctions: PExportedFunctions; pluginid: dword):BOOL; stdcall;
var init, init2: Tfunction1;
begin
  ce_exported:=ExportedFunctions^;
  thispluginid:=pluginid;


  init.callbackroutine:=@Menuitemclick;
  init.name:='View debug events';
  init.shortcut:='Shift+Ctrl+D';

  init2.callbackroutine:=@MenuitemclickFromDisassembler;
  init2.name:='View debug events';
  init2.shortcut:=nil;
  ce_exported.registerfunction(thispluginid, ptMemoryView, @init);
  ce_exported.registerfunction(thispluginid, ptDisassemblerContext, @init2);

  result:=true;
end;

function DisablePlugin:BOOL; stdcall;
begin
  if frmEventLog<>nil then frmEventLog.Close;    
  result:=true;
end;

end.

