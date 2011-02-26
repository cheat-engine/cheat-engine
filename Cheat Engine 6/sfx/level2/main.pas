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

  temp: pchar;
  outfile: Tfilestream;

  filelist: TStringList;
  i: integer;

  ceexe: string;

  startupinfo: TSTARTUPINFO;
  ProcessInformation: TPROCESSINFORMATION;
  is32bit: boolean;
  filename: string;
  selfpath: string;
begin
  selfpath:=ExtractFilePath(GetModuleName(0));
  size:=0;
  launchdir:=selfpath+'extracted\';
  CreateDir(launchdir);
  archivename:=selfpath+'CET_Archive.dat';

  filelist:=TStringList.create;
  is32bit:=false;


  if FileExists(archivename) then
  begin


    s:=TFileStream.Create(archivename, fmOpenRead);
    z:=Tdecompressionstream.create(s, true);

    while z.read(size, sizeof(size))>0 do //still has a file
    begin
      getmem(temp, size+1);
      z.read(temp^, size);
      temp[size]:=#0;

      filename:=temp;
      if filename='cheatengine-i386.exe' then
      begin
        is32bit:=true;
        filename:=ExtractFileName(GetModuleName(0)); //give it the same name as the trainer
      end;

      if filename='cheatengine-x86_64.exe' then
      begin
        is32bit:=false;
        filename:=ExtractFileName(GetModuleName(0)); //give it the same name as the trainer
      end;


      filelist.add(launchdir+filename);
      {$ifndef release}
      deletefile(launchdir+filename); //in case it didn't get deleted
      {$endif}
      outfile:=tfilestream.Create(launchdir+filename, fmCreate);

      size:=0;
      z.read(size, sizeof(size));

      outfile.CopyFrom(z, size);
      outfile.free;

      freemem(temp);
    end;

    z.free;
    s.free;


    ceexe:=launchdir+ExtractFileName(GetModuleName(0));


    if is32bit then
    begin
      //dbghelp32.dll needs to be in win32
      CreateDir(launchdir+'win32');
      MoveFile(pchar(launchdir+'dbghelp.dll'), pchar(launchdir+'win32\dbghelp.dll'));
    end;


    if FileExists(launchdir+'CET_TRAINER.CETRAINER') then
    begin
      ZeroMemory(@StartupInfo, sizeof(StartupInfo));
      ZeroMemory(@ProcessInformation, sizeof(ProcessInformation));
      StartupInfo.cb:=sizeof(StartupInfo);

      if (CreateProcess(nil, pchar(ceexe+' "'+launchdir+'CET_TRAINER.CETRAINER"'), nil, nil, FALSE, 0, nil, pchar(launchdir), StartupInfo, ProcessInformation)) then
        WaitForSingleObject(ProcessInformation.hProcess, INFINITE);

    end;


  end;

  if is32bit then
  begin
    DeleteFile(launchdir+'win32\dbghelp.dll');
    RemoveDir(launchdir+'win32');
  end;

  if filelist<>nil then
  begin
    for i:=0 to filelist.count-1 do
      deletefile(filelist[i]);
  end;

  removedir(launchdir);
end;


end.

