program cheatengine;

{$mode DELPHI}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  windows,
  sysutils,
  ShellApi
  { you can add units after this };

{$ifdef cpu64}
{$ERROR You MUST compile this code as 32-bit!}
{$endif}


{$R *.res}
{$R ..\manifest.res}

type TIsWow64Process=function (processhandle: THandle; var isWow: BOOL): BOOL; stdcall;
var IsWow64Process        :TIsWow64Process;
    WindowsKernel: THandle;

    launch32bit: boolean;
    isWow: BOOL;

    self: thandle;
    selfname: pchar;
    selfpath: string;

    param: string;
    i: integer;
begin
  {$ifdef cpu64}
  MessageBox(0,'A fucking retard thought that removing an earlier $ERROR line would be enough to run this','',0);
  exit;
  {$endif}


  WindowsKernel:=LoadLibrary('Kernel32.dll'); //there is no kernel33.dll
  IsWow64Process:=   GetProcAddress(WindowsKernel, 'IsWow64Process');

  launch32bit:=true;
  if assigned(IsWow64Process) then
  begin
    isWow:=true;
    if IsWow64Process(GetCurrentProcess(), isWow) then
      launch32bit:=not isWow;
  end;

  self:=GetModuleHandle(0);

  getmem(selfname,512);
  if GetModuleFileName(self, selfname, 512)>0 then
    selfpath:=ExtractFilePath(selfname)
  else
    selfpath:=''; //fuck it if it fails

  param:='';
  if Paramcount>0 then
  begin
    param:='"'+paramstr(1)+'"';
    for i:=2 to Paramcount do
      param:=param+' "'+paramstr(i)+'"';
  end;

  if launch32bit then
    ShellExecute(0, 'open', pchar(selfpath+'cheatengine-i386.exe'), pchar(param), pchar(selfpath), sw_show)
  else
    ShellExecute(0, 'open', pchar(selfpath+'cheatengine-x86_64.exe'), pchar(param), pchar(selfpath), sw_show)

end.

