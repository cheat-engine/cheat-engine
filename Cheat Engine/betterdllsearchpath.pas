unit BetterDLLSearchPath;

{$mode delphi}

interface

{$ifdef windows}

uses
  windows, sysutils;

const
  LOAD_LIBRARY_SEARCH_DEFAULT_DIRS=$1000;
  LOAD_LIBRARY_SEARCH_USER_DIRS=$400;

var
  AddDllDirectory: function(dir: PWideChar):pointer; stdcall;
  RemoveDllDirectory: function(cookie: pointer): BOOL; stdcall;
  SetDefaultDllDirectories: function(v: dword):BOOL; stdcall;

  DLLDirectoryCookie: pointer=nil;
{$endif}

implementation

{$ifdef windows}
function AddDllDirectoryNI(dir: PWideChar):pointer; stdcall;
begin
  result:=nil;
end;

function RemoveDllDirectoryNI(cookie: pointer): BOOL; stdcall;
begin
  result:=true;
end;

function SetDefaultDllDirectoriesNI(v:dword):BOOL; stdcall;
begin
  result:=false;
end;

procedure BetterDLLSearchPathInit;
var
  k: THandle;
  p: widestring;
begin
  k:=LoadLibrary('kernel32.dll');
  SetDefaultDllDirectories:=GetProcAddress(k,'SetDefaultDllDirectories');
  AddDllDirectory:=GetProcAddress(k,'AddDllDirectory');
  RemoveDllDirectory:=GetProcAddress(k,'AddDllDirectory');

  if assigned(SetDefaultDllDirectories) then
    SetDefaultDllDirectories(LOAD_LIBRARY_SEARCH_DEFAULT_DIRS or LOAD_LIBRARY_SEARCH_USER_DIRS)
  else
    SetDefaultDllDirectories:=@SetDefaultDllDirectoriesNI;

{$warn 4104 off}
  if assigned(AddDllDirectory) then
  begin
{$ifdef cpu32}
    p:=ExtractFilePath(ParamStr(0))+'win32';
{$else}
    p:=ExtractFilePath(ParamStr(0))+'win64';
{$endif}
    DLLDirectoryCookie:=AddDllDirectory(@p[1]);
  end
  else
    AddDllDirectory:=@AddDllDirectoryNI;

  if assigned(RemoveDllDirectory)=false then
    RemoveDllDirectory:=@RemoveDllDirectoryNI;

{$warn 4104 on}
end;

initialization
  BetterDLLSearchPathInit;

{$endif}

end.

