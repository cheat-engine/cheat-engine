program cheatengine;

{$mode DELPHI}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  windows,
  ShellApi
  { you can add units after this };

{$ifdef cpu64}
//{$ERROR You MUST compile this code as 32-bit!}
{$endif}


{$R *.res}
{$R ..\manifest.res}

type TIsWow64Process=function (processhandle: THandle; var isWow: BOOL): BOOL; stdcall;
var IsWow64Process        :TIsWow64Process;
    WindowsKernel: THandle;

    launch32bit: boolean;
    isWow: BOOL;
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

  if launch32bit then
    ShellExecute(0, 'open', 'cheatengine-i386.exe', nil, nil, sw_show)
  else
    ShellExecute(0, 'open', 'cheatengine-x86_64.exe', nil, nil, sw_show)

end.

