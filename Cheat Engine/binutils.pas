unit binutils;

//responsible for launching and retrieving data from the binutil tools

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, LastDisassembleData, strutils;

type
  TSection=record
    name: string;
    size: dword;
  end;
  TSectionArray=array of TSection;

  TImportListEntry=record
    name: string;
    address: ptruint;
  end;

  TImportList=array of TImportListEntry;


  TBinarySection=record
    sectionname: string;
    data: array of byte;
  end;
  TBinarySections=array of TBinarySection;

type
  TBinUtils=class
  private
    fpath: string;
    procedure setPath(path: string);
  public

    prefix: string;
    lasterror: string;
    arch: string;
    function assemble(script: tstringlist; out filename: string): boolean;
    procedure nm(filename: string; undefined: tstringlist);
    procedure GetSections(filename: string; var sections: TSectionArray);
    procedure ldAndExtract(objectfile, linkfilename: string; imports: TImportList; var sections: TBinarySections);
    procedure disassemble(var ldd: TLastDisassembleData); //address and bytes are filled in
  published
    property path: string read fpath write setPath;
  end;


var currentBinutils: TBinUtils;



implementation

uses dialogs;

procedure TBinUtils.setPath(path: string);
begin
  fpath:=path;
  if length(fpath)>0 then
  begin
    if fpath[length(fpath)]<>DirectorySeparator then
      fpath:=fpath+DirectorySeparator;
  end;
end;

procedure TBinUtils.disassemble(var ldd: TLastDisassembleData);
begin

end;

procedure TBinUtils.ldAndExtract(objectfile, linkfilename: string; imports: TImportList; var sections: TBinarySections);
var
  p: TProcess;
  i: integer;

  linkedfilename: string;
  bindata: TMemoryStream;
begin
  p:=TProcess.create(nil);
  try
    p.CurrentDirectory:=fpath;
    p.executable:=fpath+prefix+'ld';
    p.Options:=[poUsePipes, poNoConsole];
    p.Parameters.Add('-T "'+linkfilename+'"');
    p.Parameters.Add('"'+objectfile+'"');

    for i:=0 to length(imports)-1 do
      p.Parameters.Add('--defsym='+imports[i].name+'=0x'+inttohex(imports[i].address,8));

    linkedfilename:=GetTempFileName;

    p.parameters.add('-o "'+linkedfilename+'"');

    p.Execute;
    p.WaitOnExit;

    if p.ExitCode<>0 then
    begin
      setlength(lasterror, p.Stderr.NumBytesAvailable+1);
      p.Stderr.ReadBuffer(lasterror[1], length(lasterror)-1);
      lasterror[length(lasterror)]:=#0;

      raise exception.create(lasterror);
    end;

  finally
    p.free;
  end;

  for i:=0 to length(sections)-1 do
  begin
    p:=TProcess.Create(nil);
    try
      p.CurrentDirectory:=fpath;
      p.executable:=fpath+prefix+'objcopy';
      p.Options:=[poUsePipes, poNoConsole];

      p.Parameters.Add('-Obinary');
      p.Parameters.Add('-j'+sections[i].sectionname);
      p.Parameters.Add(linkedfilename);
      p.Parameters.Add(linkedfilename+'.bin');

     // showmessage(p.Parameters.Text);

      p.Execute;
      p.WaitOnExit;

      if p.ExitCode<>0 then
      begin
        setlength(lasterror, p.Stderr.NumBytesAvailable+1);
        p.Stderr.ReadBuffer(lasterror[1], length(lasterror)-1);
        lasterror[length(lasterror)]:=#0;

        raise exception.create(lasterror);
      end;

      bindata:=TMemoryStream.create;
      try
        bindata.LoadFromFile(linkedfilename+'.bin');
        setlength(sections[i].data, bindata.Size);
        bindata.Position:=0;
        bindata.ReadBuffer(sections[i].data[0], bindata.Size);
      finally
        bindata.free;
      end;
    finally
      p.free;
    end;
  end;
end;

procedure TBinUtils.GetSections(filename: string; var sections: TSectionArray);
var
  output: TStringStream;
  p: TProcess;
  ec: integer;

  i,j: integer;

  r: TStringList;

  idx, name, size: string;

  s: string;
begin
  p:=TProcess.Create(nil);
  output:=nil;
  r:=nil;

  try
    p.CurrentDirectory:=fpath;
    p.Executable:=fpath+prefix+'objdump';
    p.Options:=[poUsePipes, poNoConsole];

    p.Parameters.Add('-h');
    p.Parameters.Add('"'+filename+'"');

    p.Execute;


    output:=TStringStream.create('');

    repeat
      if p.Output.NumBytesAvailable>0 then
        output.CopyFrom(p.Output, p.Output.NumBytesAvailable)
      else
        sleep(1);
    until not p.Running;

    p.WaitOnExit; //just to be sure

    if p.Output.NumBytesAvailable>0 then
        output.CopyFrom(p.Output, p.Output.NumBytesAvailable);

    if p.ExitCode<>0 then
    begin
      setlength(lasterror, p.Stderr.NumBytesAvailable+1);
      p.Stderr.ReadBuffer(lasterror[1], length(lasterror)-1);
      lasterror[length(lasterror)]:=#0;

      raise exception.create(lasterror);
    end;

    r:=TStringList.create;
    output.position:=0;
    r.LoadFromStream(output);

    //search for lines with: integer, string, hexvalue
    for i:=0 to r.Count-1 do
    begin
      s:=trim(r[i]);

      if WordCount(r[i],[' ',#8])>3 then
      begin
        idx:=ExtractWord(1, s,[' ',#8]);

        if TryStrToInt(idx, j) then
        begin
          name:=ExtractWord(2, s,[' ',#8]);
          size:=ExtractWord(3, s,[' ',#8]);

          if TryStrToInt('0x'+size, j) then
          begin
            if (j>0) then
            begin
              setlength(sections, length(sections)+1);
              sections[length(sections)-1].name:=name;
              sections[length(sections)-1].size:=j;
            end;
          end;

        end;
      end;


    end;

  finally
    if r<>nil then
      r.free;

    if output<>nil then
      output.free;

    if p<>nil then
      p.free;
  end;


end;

procedure TBinUtils.nm(filename: string; undefined: tstringlist);
var
  p: TProcess;
  ec: integer;

  output: TStringStream;
  s: tstringlist;
  i: integer;
  line: string;
begin
  output:=TStringStream.Create('');
  p:=TProcess.Create(nil);
  try
    p.CurrentDirectory:=fpath;
    p.Executable:=fpath+prefix+'nm';
    p.Options:=[poUsePipes, poNoConsole];

    p.Parameters.Add('-u');
    p.Parameters.Add('"'+filename+'"');

    p.Execute;

    repeat
      if p.Output.NumBytesAvailable>0 then
        output.CopyFrom(p.Output, p.Output.NumBytesAvailable)
      else
        sleep(1);
    until not p.Running;

    p.WaitOnExit; //just to be sure

    if p.Output.NumBytesAvailable>0 then
        output.CopyFrom(p.Output, p.Output.NumBytesAvailable);


    ec:=p.ExitCode;
    if ec=0 then
    begin
      s:=tstringlist.create;
      output.Position:=0;
      s.LoadFromStream(output);
      for i:=0 to s.Count-1 do
      begin
        line:=trim(s[i]);
        if (length(line)>2) and (line[1]='U') then
          undefined.add(copy(line, 3, length(line)));


      end;
      s.free;
    end
    else
    begin
      setlength(lasterror, p.Stderr.NumBytesAvailable+1);
      p.Stderr.ReadBuffer(lasterror[1], length(lasterror)-1);
      lasterror[length(lasterror)]:=#0;

      raise exception.create(lasterror);
    end;
  finally
    p.free;
    output.free;
  end;
end;

function TBinUtils.assemble(script: tstringlist; out filename: string): boolean;
var
  p: TProcess;
  origfile: string;
  ec: integer;
begin
  p:=TProcess.Create(nil);
  origfile:=GetTempFileName;
  script.SaveToFile(origfile);

  filename:=GetTempFileName;


  try
    p.CurrentDirectory:=fpath;
    p.Executable:=fpath+prefix+'as';
    p.Options:=[poUsePipes, poWaitOnExit, poNoConsole];

    p.Parameters.Add('"'+origfile+'"');
    p.Parameters.Add('-o "'+filename+'"');

    p.Execute;
    p.WaitOnExit;

    ec:=p.ExitCode;

    result:=ec=0;

    if not result then
    begin
      setlength(lasterror, p.Stderr.NumBytesAvailable+1);
      p.Stderr.ReadBuffer(lasterror[1], length(lasterror)-1);
      lasterror[length(lasterror)]:=#0;

      raise exception.create(lasterror);
    end;

  finally
    p.free;
    deletefile(origfile);
  end;
end;

initialization
  //temporary:

  currentbinutils:=TBinUtils.create;
  {
  currentbinutils.path:='D:\SysGCC\arm-eabi\bin';
  currentbinutils.prefix:='arm-eabi-';
  }

  currentbinutils.path:='C:\FPC\3.0.0rc1\bin\i386-win32';
  currentbinutils.prefix:='x86_64-win64-';




end.

