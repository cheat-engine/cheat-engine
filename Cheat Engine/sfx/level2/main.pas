unit main;

{$mode objfpc}{$H+}

interface

uses
  windows, Classes, SysUtils, zstream;

procedure launch;

implementation

function DeleteFolder(dir: string) : boolean;
var
  DirInfo: TSearchRec;
  r : Integer;
begin
  ZeroMemory(@DirInfo,sizeof(TSearchRec));
  result := true;

  while dir[length(dir)]=pathdelim do //cut of \
    dir:=copy(dir,1,length(dir)-1);

  r := FindFirst(dir + pathdelim+'*.*', FaAnyfile, DirInfo);
  while (r = 0) do
  begin
    if (DirInfo.Attr and FaVolumeId <> FaVolumeID) then
    begin
      if ((DirInfo.Attr and FaDirectory) <> FaDirectory) then
        result := DeleteFile(dir + pathdelim + DirInfo.Name)
      else
      begin
        if (DirInfo.Name<>'.') and (DirInfo.Name<>'..') then
          DeleteFolder(dir+pathdelim+DirInfo.name);
      end;
    end;
    r := FindNext(DirInfo);
  end;
  FindClose(DirInfo);

  if Result then
    result := RemoveDir(dir);
end;

procedure launch;
var launchdir: string;
  archivename: string;

  s: TFileStream;

  z: Tdecompressionstream;
  size: dword;

  temp: pchar;
  outfile: Tfilestream;


  filelist: TStringList;

  ceexe: string;

  startupinfo: TSTARTUPINFO;
  ProcessInformation: TPROCESSINFORMATION;
  filename, folder: string;
  selfpath: string;

  secondaryparams: string;
  i: integer;

  filenumber: integer;
  filecount: integer;
begin
  selfpath:=ExtractFilePath(GetModuleName(0));
  size:=0;
  filecount:=0;
  filenumber:=0;

  launchdir:=selfpath+'extracted\';
  CreateDir(launchdir);
  archivename:=selfpath+'CET_Archive.dat';

  filelist:=TStringList.create;


  if FileExists(archivename) then
  begin
    if Paramcount=0 then
      secondaryparams:=''
    else
    begin
      secondaryparams:=' ';
      for i:=1 to Paramcount do
        secondaryparams:=secondaryparams+'"'+ParamStr(i)+'"'; //quotes got removed


    end;

    s:=TFileStream.Create(archivename, fmOpenRead);
    s.ReadBuffer(filecount, sizeof(filecount));

    z:=Tdecompressionstream.create(s, true);

    while filenumber<filecount do //still has a file
    begin
      //get the filename
      z.read(size, sizeof(size));
      getmem(temp, size+1);
      z.read(temp^, size);
      temp[size]:=#0;

      filename:=temp;
      freemem(temp);

      //get the folder
      z.read(size, sizeof(size));
      getmem(temp, size+1);
      z.read(temp^, size);
      temp[size]:=#0;

      folder:=temp;
      freemem(temp);

      if folder<>'' then
      begin
        ForceDirectories(launchdir+folder);
        if folder[length(folder)]<>pathdelim then
          folder:=folder+pathdelim;
      end;


      if filename='cheatengine-i386.exe' then
        filename:=ExtractFileName(GetModuleName(0)); //give it the same name as the trainer

      if filename='cheatengine-x86_64.exe' then
        filename:=ExtractFileName(GetModuleName(0)); //give it the same name as the trainer

      if filename='cheatengine-i386.exe.sig' then
        filename:=ExtractFileName(GetModuleName(0))+'.sig';

      if filename='cheatengine-x86_64.exe.sig' then
        filename:=ExtractFileName(GetModuleName(0))+'.sig';

      filelist.add(launchdir+filename);
      {$ifndef release}
      deletefile(launchdir+filename); //in case it didn't get deleted
      {$endif}
      outfile:=tfilestream.Create(launchdir+folder+filename, fmCreate);

      size:=0;
      z.read(size, sizeof(size));

      outfile.CopyFrom(z, size);
      outfile.free;

      inc(filenumber);
    end;

    z.free;
    s.free;


    ceexe:=launchdir+ExtractFileName(GetModuleName(0));


    if FileExists(launchdir+'CET_TRAINER.CETRAINER') then
    begin
      ZeroMemory(@StartupInfo, sizeof(StartupInfo));
      ZeroMemory(@ProcessInformation, sizeof(ProcessInformation));
      StartupInfo.cb:=sizeof(StartupInfo);

      if (CreateProcess(nil, pchar(ceexe+' "'+launchdir+'CET_TRAINER.CETRAINER"'+secondaryparams), nil, nil, FALSE, 0, nil, pchar(launchdir), StartupInfo, ProcessInformation)) then
      begin
        WaitForSingleObject(ProcessInformation.hProcess, INFINITE);
        sleep(2000);
      end;
    end; //else file does not exist

  end;

  while not deleteFolder(launchdir) do //deletion failed, try again in 10 seconds
    sleep(10000);
end;


end.

