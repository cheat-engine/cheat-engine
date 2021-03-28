//Copyright Cheat Engine


unit CSharpCompiler;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef windows}
  jwawindows, windows, Classes, SysUtils, dotnethost{$ifndef standalonetest}, newkernelhandler{$endif};
  {$else}
  classes, SysUtils;
  //todo: mono?
  {$endif}

type
  TCSharpCompilerError=class(exception);

  TCSharpCompiler=class(TObject)
  private
    dCompileCode: function(script: pchar; path: string; userdata: pointer; errorcallback: pointer): boolean; stdcall;
    dAddReference: procedure(path: pchar); stdcall;
    dSetCoreAssembly: procedure(coreAssembly: pchar); stdcall;
    dRelease: procedure();  stdcall;
  public
    procedure addAssemblyReference(path: string);
    procedure setCoreAssembly(path: string);
    function compile(script: string; path: string; errorlog: tstrings): boolean;

    constructor create;
    destructor destroy; override;
  end;

function compilecsharp(script: string; references: tstringlist; coreAssembly: string=''): string;  //compile the script and return the name of the generated file.  Will raise exception on error

implementation

{$ifndef standalonetest}
uses cefuncproc, globals;
{$endif}

var counter: integer;

{$ifdef standalonetest}
type
PPROCESSENTRY32 = ^PROCESSENTRY32;
{$EXTERNALSYM PPROCESSENTRY32}
tagPROCESSENTRY32 = record
  dwSize: DWORD;
  cntUsage: DWORD;
  th32ProcessID: DWORD;          // this process
  th32DefaultHeapID: ULONG_PTR;
  th32ModuleID: DWORD;           // associated exe
  cntThreads: DWORD;
  th32ParentProcessID: DWORD;    // this process's parent process
  pcPriClassBase: LONG;          // Base priority of process's threads
  dwFlags: DWORD;
  szExeFile: array [0..MAX_PATH - 1] of Char;    // Path
end;
{$EXTERNALSYM tagPROCESSENTRY32}
PROCESSENTRY32 = tagPROCESSENTRY32;
{$EXTERNALSYM PROCESSENTRY32}
LPPROCESSENTRY32 = ^PROCESSENTRY32;
{$EXTERNALSYM LPPROCESSENTRY32}
TProcessEntry32 = PROCESSENTRY32;

{$endif}

{$ifdef windows}
procedure cleanupcecsfiles;
var
DirInfo: TSearchRec;
{$ifdef standalonetest}
  x: PROCESSENTRY32;
  pe32: jwawindows.PROCESSENTRY32 absolute x;
{$else}
  pe32: PROCESSENTRY32;
{$endif}
  r : Integer;
  usedtempdir: string;
  i: integer;




  sa: array of string;

  pidlist: tstringlist;
  SNAPHandle: THandle;
begin
  {$ifdef standalonetest}
  usedtempdir:=GetTempDir;
  {$else}
  if (length(trim(tempdiralternative))>2) and dontusetempdir then
    usedtempdir:=trim(tempdiralternative)
  else
    usedtempdir:=GetTempDir;
  {$endif}

  usedtempdir:=IncludeTrailingPathDelimiter(usedtempdir)+'Cheat Engine'+pathdelim;

  //first clean up my own crap
  for i:=counter-1 downto 0 do
    DeleteFile(usedtempdir+'ce-cscode-'+inttostr(getcurrentprocessid)+'-'+inttostr(i)+'.dll');

  //and now search for other files

  SNAPHandle:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
  zeromemory(@pe32, sizeof({$ifdef standalonetest}x{$else}pe32{$endif}));
  pe32.dwSize:=sizeof({$ifdef standalonetest}x{$else}pe32{$endif});
  pidlist:=tstringlist.create;
  pidlist.sorted:=true; //speeds up indexof
  if (snaphandle<>0) and (snaphandle<>INVALID_HANDLE_VALUE) then
  begin
    if Process32First(snaphandle, pe32) then
    repeat
      pidlist.Add(inttostr(pe32.th32ProcessID));
    until Process32Next(snaphandle, pe32)=false;

    closehandle(SNAPHandle);
  end;


  ZeroMemory(@DirInfo,sizeof(DirInfo));
  DirInfo.Size:=sizeof(DirInfo);

  r := FindFirst(usedtempdir+'ce-cscode-*.dll', FaAnyfile, DirInfo);
  while (r = 0) do
  begin
    if ((DirInfo.Attr and FaVolumeId) <> FaVolumeID) and
       ((DirInfo.Attr and FaDirectory) <> FaDirectory) then
    begin
      sa:=string(dirinfo.name).Split('-');
      if length(sa)=4 then
      begin
        try
          if pidlist.indexof(sa[2])=-1 then //does not exist (anymore) try to delete it
            DeleteFile(usedtempdir + DirInfo.Name);
        except
        end;
      end;
    end;
    r := FindNext(DirInfo);
  end;
  FindClose(DirInfo);

  pidlist.free;
end;

{$endif}

function compilecsharp(script: string; references: tstringlist; coreAssembly:string=''): string;
{$ifdef windows}
var
  c: TCSharpCompiler;
  errorlog: Tstringlist;
  i: integer;
  usedtempdir: string;
  filename: string;
  {$endif}
begin
  {$ifdef windows}
  {$ifdef standalonetest}
  usedtempdir:=GetTempDir;
  {$else}
  if (length(trim(tempdiralternative))>2) and dontusetempdir then
    usedtempdir:=trim(tempdiralternative)
  else
    usedtempdir:=GetTempDir;
  {$endif}

  usedtempdir:=IncludeTrailingPathDelimiter(usedtempdir)+'Cheat Engine'+pathdelim;

  inc(counter);
  filename:=usedtempdir+'ce-cscode-'+inttostr(getcurrentprocessid)+'-'+inttostr(counter)+'.dll';

  c:=TCSharpCompiler.create;
  errorlog:=tstringlist.create;
  try
    for i:=0 to references.count-1 do
      c.addAssemblyReference(references[i]);

    if coreAssembly<>'' then
      c.setCoreAssembly(coreAssembly);

    if c.compile(script, filename, errorlog)=false then
      raise TCSharpCompilerError.create(errorlog.text);

    result:=filename;
  finally
    errorlog.free;
    c.free;
  end;
  {$endif}
end;

procedure OnErrorCallback(errorlog: tstrings; s: pchar);
begin
  if errorlog<>nil then
    errorlog.add(s);
end;

procedure TCSharpCompiler.addAssemblyReference(path: string);
begin
  if assigned(dAddReference) then
    dAddReference(pchar(path));
end;

procedure TCSharpCompiler.setCoreAssembly(path: string);
begin
  if assigned(dSetCoreAssembly) then
    dSetCoreAssembly(pchar(path));
end;

function TCSharpCompiler.compile(script: string; path: string; errorlog: tstrings): boolean;
begin
  result:=dCompileCode(pchar(script), pchar(path), errorlog, @OnErrorCallback);
end;

constructor TCSharpCompiler.create;
var
  delegates: record
    CompileCode: pointer;
    AddReference: pointer;
    SetCoreAssembly: pointer;
    Release: pointer;
  end;
  r: integer;
begin
  {$ifdef windows}

  r:=DotNetExecuteClassMethod({$ifdef standalonetest}'D:\git\cheat-engine\Cheat Engine\bin\CSCompiler.dll'{$else}CheatEngineDir+'CSCompiler.dll'{$endif},'CSCompiler','Compiler','NewCompiler',inttostr(ptruint(@delegates)));

  //r:=DotNetExecuteClassMethod({$ifdef standalonetest}'D:\git\cheat-engine\Cheat Engine\bin\CSCompiler.dll'{$else}CheatEngineDir+'CSCompiler.dll'{$endif},'CSCompiler','Compiler','NewCompiler',inttostr(ptruint(@delegates)));
  if r<>1 then raise exception.create('C-Sharp compiler creation failed');

  pointer(dCompileCode):=delegates.CompileCode;
  pointer(dAddReference):=delegates.AddReference;
  pointer(dSetCoreAssembly):=delegates.SetCoreAssembly;
  pointer(dRelease):=delegates.Release;
  {$else}
  raise exception.create('C# compiler not implemented yet');
  {$endif}
end;

destructor TCSharpCompiler.destroy;
begin
  if assigned(dRelease) then
    dRelease();

  inherited destroy;
end;

finalization
  {$ifdef windows}
  cleanupcecsfiles;
  {$endif}

end.

