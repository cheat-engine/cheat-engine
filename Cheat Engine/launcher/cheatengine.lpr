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

    cpuid7ebx: dword;
    exename: widestring;

    s: string;
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

  self:=GetModuleHandle(nil);

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
    exename:='cheatengine-i386.exe'
  else
  begin
    asm
      push ecx
      mov ecx,0
      mov eax,7
      cpuid
      mov cpuid7ebx,ebx
      pop ecx
    end;

    if (cpuid7ebx and (1 shl 5))>0 then
      exename:='cheatengine-x86_64-SSE4-AVX2.exe'
    else
      exename:='cheatengine-x86_64.exe';
  end;

  
  if FileExists(selfpath+exename) then
    ShellExecuteW(0, 'open', pwidechar(selfpath+exename), pwidechar(widestring(param)), pwidechar(selfpath), sw_show)
  else
  begin
    s:=exename;
    MessageBoxW(0, pwidechar(exename+' could not be found. Please disable/uninstall your anti virus and reinstall Cheat Engine to fix this'),'Cheat Engine launch error',MB_OK or MB_ICONERROR);
  end;


end.

