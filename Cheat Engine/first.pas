unit first;

{$mode objfpc}{$H+}

interface

uses
  betterDLLSearchPath, Classes, SysUtils;

implementation

uses windows, registry, Win32Int;


procedure setDPIAware;   //won't work in windows 10 anymore
type
  PROCESS_DPI_AWARENESS=(PROCESS_DPI_UNAWARE=0, PROCESS_SYSTEM_DPI_AWARE=1, PROCESS_PER_MONITOR_DPI_AWARE=2);

var
  SetProcessDpiAwareness:function(value: PROCESS_DPI_AWARENESS):HRESULT; stdcall;
  SetProcessDPIAware:function: BOOL; stdcall;
  l: HModule;
begin
  OutputDebugString('setDPIAware');
  l:=LoadLibrary('Shcore.dll');
  if l<>0 then
  begin
    farproc(SetProcessDpiAwareness):=GetProcAddress(l,'SetProcessDpiAwareness');

    if assigned(SetProcessDpiAwareness) then
    begin
     // OutputDebugString('p1');
      SetProcessDpiAwareness(PROCESS_SYSTEM_DPI_AWARE);
      exit;
    end;
  end;


  //still here, probably win8.0 or 7
  l:=LoadLibrary('user32.dll');
  if l<>0 then
  begin
   // OutputDebugString('p2');
    farproc(SetProcessDPIAware):=GetProcAddress(l,'SetProcessDPIAware');
    if assigned(SetProcessDPIAware) then
      SetProcessDPIAware;
  end;

  OutputDebugString('p3');
end;

var
  i: integer;
  istrainer: boolean;
  r: TRegistry;
  hassetdpiaware: boolean;
initialization
  //todo, check registry if not a trainer

  istrainer:=false;
  hassetdpiaware:=false;

  for i:=1 to Paramcount do
  begin
    if ParamStr(i)='DPIAWARE' then
    begin
      setDPIAware;
      hassetdpiaware:=true;
    end;

    if pos('.CETRAINER', uppercase(ParamStr(i)))>0 then
      istrainer:=true;
  end;

  if not (istrainer or hassetdpiaware) then
  begin
    //check the registry
    r := TRegistry.Create;
    r.RootKey := HKEY_CURRENT_USER;
    if r.OpenKey('\Software\Cheat Engine',false) then
    begin
      if (r.ValueExists('DPI Aware')=false) or r.ReadBool('DPI Aware') then
        setDPIAware;
    end
    else
    begin
      //first time CE is ran, and not a trainer.
      if r.OpenKey('\Software\Cheat Engine',true) then
      begin
        //I do have access
        setDPIAware; //default config is enabled
        r.WriteBool('DPI Aware', true);
      end;
    end;

    r.free;
    r:=nil;
  end;


end.

