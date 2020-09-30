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

function HasAVX2(): BOOL;
begin
    {$asmMode att}
    var leaf_7_0_b: longint;
    asm
        movl $7, %eax
        movl $0, %ecx
        cpuid
        movl %ebx, leaf_7_0_b
    end ['eax', 'ebx', 'ecx', 'edx'];
    HasAVX2 := (leaf_7_0_b and (1 shl 5)) <> 0
end;

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

procedure Launch(const exe: widestring);
begin
    if fileexists(selfpath+exe) then
      ShellExecuteW(0, 'open', pwidechar(selfpath+exe), pwidechar(widestring(param)), pwidechar(selfpath), sw_show)
    else
      MessageBox(0, exe+' could not be found. Please disable/uninstall your anti virus and reinstall Cheat Engine to fix this','Cheat Engine launch error',MB_OK or MB_ICONERROR);
end;

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

  // This might bite us with quotes not being escaped but who does that
  // we will use GetCommandLineW if that comes up
  param:='';
  for i:=1 to paramcount do
   param:=param+'"'+ParamStrUTF8(i)+'" ';

  //MessageBox(0, pchar(param),'bla',0);

  if launch32bit then
    Launch('cheatengine-i386.exe')
  else if HasAVX2() then
    Launch('cheatengine-x86_64-SSE4-AVX2.exe')
  else
    Launch('cheatengine-x86_64.exe');
end.

