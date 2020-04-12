program cheatengine;

{$mode DELPHI}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  windows,
  LazUTF8,
  sysutils,

  ShellApi
  { you can add units after this };

{$ifdef cpu64}
{$ERROR You MUST compile this code as 32-bit!}
{$endif}


{$R *.res}

type TIsWow64Process=function (processhandle: THandle; var isWow: BOOL): BOOL; stdcall;
var IsWow64Process        :TIsWow64Process;
    WindowsKernel: THandle;

    launch32bit: boolean;
    isWow: BOOL;

    self: thandle;
    selfname: pwidechar;
    selfpath: widestring;

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
  if GetModuleFileNameW(self, selfname, 512)>0 then
    selfpath:=ExtractFilePath(selfname)
  else
    selfpath:=''; //fuck it if it fails

  param:='';
  for i:=1 to paramcount do
   param:=param+'"'+ParamStrUTF8(i)+'" ';

  //MessageBox(0, pchar(param),'bla',0);

  if launch32bit then
  begin
    if fileexists(selfpath+'cheatengine-i386.exe') then
      ShellExecuteW(0, 'open', pwidechar(selfpath+'cheatengine-i386.exe'), pwidechar(widestring(param)), pwidechar(selfpath), sw_show)
    else
      MessageBox(0, 'cheatengine-i386.exe could not be found. Please disable/uninstall your anti virus and reinstall Cheat Engine to fix this','Cheat Engine launch error',MB_OK or MB_ICONERROR);
  end
  else
  begin
    if FileExists(selfpath+'cheatengine-x86_64.exe') then
      ShellExecuteW(0, 'open', pwidechar(selfpath+'cheatengine-x86_64.exe'), pwidechar(widestring(param)), pwidechar(selfpath), sw_show)
    else
      MessageBox(0, 'cheatengine-x86_64.exe could not be found. Please disable/uninstall your anti virus and reinstall Cheat Engine to fix this','Cheat Engine launch error',MB_OK or MB_ICONERROR);
  end;

end.

