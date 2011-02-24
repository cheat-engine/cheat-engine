unit main;

{$mode objfpc}{$H+}

interface

uses
  windows, Classes, SysUtils, zstream;

procedure launch;

implementation

procedure launch;
var launchdir: string;
  archivename: string;

  s: TFileStream;

  z: Tdecompressionstream;
  size: dword;

  filename: pchar;
  outfile: Tfilestream;

  filelist: TStringList;
  i: integer;

  ceexe: string;

  startupinfo: TSTARTUPINFO;
  ProcessInformation: TPROCESSINFORMATION;

begin
  size:=0;
  launchdir:=ExtractFilePath(GetModuleName(0)){$ifndef release}+'testtrainer\'{$endif};
  archivename:=launchdir+'CET_Archive.dat';

  filelist:=TStringList.create;

  if FileExists(archivename) then
  begin
    s:=TFileStream.Create(archivename, fmOpenRead);
    z:=Tdecompressionstream.create(s, true);

    while z.read(size, sizeof(size))>0 do //still has a file
    begin
      getmem(filename, size+1);
      z.read(filename^, size);
      filename[size]:=#0;

      filelist.add(launchdir+filename);
      {$ifndef release}
      deletefile(launchdir+filename); //in case it didn't get deleted
      {$endif}
      outfile:=tfilestream.Create(launchdir+filename, fmCreate);

      size:=0;
      z.read(size, sizeof(size));

      outfile.CopyFrom(z, size);
      outfile.free;

      freemem(filename);
    end;

    z.free;
    s.free;

    ceexe:=launchdir+'cheatengine-x86_64.exe';
    if not FileExists(ceexe) then
      ceexe:=launchdir+'cheatengine-i386.exe';


    if FileExists(launchdir+'CET_TRAINER.CETRAINER') then
    begin
      ZeroMemory(@StartupInfo, sizeof(StartupInfo));
      ZeroMemory(@ProcessInformation, sizeof(ProcessInformation));
      StartupInfo.cb:=sizeof(StartupInfo);

      if (CreateProcess(nil, pchar(ceexe+' "'+launchdir+'CET_TRAINER.CETRAINER"'), nil, nil, FALSE, 0, nil, pchar(launchdir), StartupInfo, ProcessInformation)) then
        WaitForSingleObject(ProcessInformation.hProcess, INFINITE);

    end;


  end;


  if filelist<>nil then
  begin
    for i:=0 to filelist.count-1 do
      deletefile(filelist[i]);
  end;


end;


end.

