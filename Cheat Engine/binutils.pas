unit binutils;

//responsible for launching and retrieving data from the binutil tools

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, LastDisassembleData, strutils, maps;

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
    disassemblercache: TMap;
    fpath: string;
    procedure setPath(path: string);
    procedure cleanEntry(_entry: Pointer);
  public
    name: string;
    description: string;
    prefix: string;
    lasterror: string;
    arch: string;
    OnDisassemble: integer; //lua function reference

    ASParam: string;
    LDParam: string;
    OBJDUMPParam: string;
    DisassemblerCommentChar: string;

    constructor create;
    destructor destroy; override;

    procedure clearDisassemblerCache;

    function assemble(script: tstringlist; extraparams: string; out filename: string): boolean;
    procedure nm(filename: string; undefined: tstringlist);
    procedure GetSections(filename: string; var sections: TSectionArray);
    procedure ldAndExtract(objectfile, linkfilename, extraparams: string; imports: TImportList; var sections: TBinarySections);
    procedure disassemble(var ldd: TLastDisassembleData); //address and bytes are filled in
  published
    property path: string read fpath write setPath;
  end;


var
  binutilslist: TList;
  defaultBinutils: TBinUtils; //used by the disassembler and single line assember if set (gui thing)


implementation

uses dialogs, lua, LuaHandler, NewKernelHandler, ProcessHandlerUnit;

type
  TDisassemblerCacheEntry=record
    bytes: pbyte;
    bytesize: integer;
    instruction: pchar;
    parameter: pchar;
    extra: pchar;
  end;

  PDisassemblerCacheEntry=^TDisassemblerCacheEntry;

constructor TBinUtils.create;
begin
  disassemblercache:=TMap.Create(ituPtrSize,sizeof(TDisassemblerCacheEntry));

end;

destructor TBinUtils.destroy;
begin
  clearDisassemblerCache;
  disassemblercache.free;
  inherited destroy;
end;

procedure TBinUtils.cleanEntry(_entry: pointer);
var  entry: PDisassemblerCacheEntry;
begin
  if _entry=nil then exit;

  entry:=_entry;
  if entry^.bytes<>nil then
    freemem(entry^.bytes);

  if entry^.instruction<>nil then
    strdispose(entry^.instruction);

  if entry^.parameter<>nil then
    strdispose(entry^.parameter);

  if entry^.extra<>nil then
    strdispose(entry^.extra);
end;

procedure TBinUtils.clearDisassemblerCache;
var it: TMapIterator;
  entry: PDisassemblerCacheEntry;
begin
  it:=TMapIterator.Create(disassemblercache);
  try
    it.First;
    while not it.EOM do
    begin
      entry:=it.DataPtr;
      cleanEntry(entry);
      it.Next;
    end;

  finally
    it.free;
  end;
end;

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
var entry: TDisassemblerCacheEntry;
  p: TProcess;

  i,j: integer;

  output: TStringStream;
  r: tstringlist;
  t: integer;

  s: string;

  buffer: pbyte;
  br: ptruint;

  f: string;
  fs: tfilestream;

  line1, line2: string;

  a1, a2: string;
  address1, address2: int64;
  instr: string;
  param: string;
  comment: string;
  found: boolean;
begin
  //check if this address is in the disassembled cache, and if so, add it

  getmem(buffer, 512);
  try
    FillByte(buffer^, 512,0);
    ReadProcessMemory(processhandle, pointer(ldd.address), buffer, 512, br);


    if disassemblercache.GetData(ldd.address, entry) then
    begin
      //return this data
      if (entry.bytesize<512) and CompareMem(buffer, entry.bytes, entry.bytesize) then
      begin
        //no change in bytes, so accept it
        ldd.opcode:=entry.instruction;
        setlength(ldd.Bytes, entry.bytesize);
        for i:=0 to entry.bytesize-1 do
          ldd.bytes[i]:=entry.bytes[i];

        ldd.parameters:=entry.parameter;

        exit;
      end;
    end;

    if disassemblercache.Count>10000 then
      clearDisassemblerCache;


    //still here, disassemble this address and a couple of instructions after it as well
    f:=GetTempFileName;
    fs:=tfilestream.Create(f, fmCreate);
    fs.WriteBuffer(buffer[0], 512);
    fs.Free;


    p:=TProcess.Create(nil);
    output:=nil;

    try
      p.CurrentDirectory:=fpath;
      p.executable:='"'+fpath+prefix+'objdump'+'"';
      p.Options:=[poUsePipes, poNoConsole];

      p.parameters.add(OBJDUMPParam);
      p.parameters.add('--adjust-vma=0x'+inttohex(ldd.address,8));
      p.parameters.Add('--stop-address=0x'+inttohex(ldd.address+512,8));
      p.Parameters.add('--prefix-addresses');

      p.Parameters.add('-z');
      p.Parameters.add('-bbinary');
      p.Parameters.add('-D');
      p.Parameters.add('-m'+arch);
      p.parameters.add('"'+f+'"');

      //lua
      if OnDisassemble<>0 then
      begin
        LuaCS.enter;
        try
          t:=lua_gettop(luavm);
          lua_rawgeti(luavm, LUA_REGISTRYINDEX, OnDisassemble);
          lua_pushinteger(Luavm, ldd.address);
          if lua_pcall(luavm, 1, 1, 0)=0 then
          begin
            s:=Lua_ToString(Luavm, -1);

            for i:=1 to WordCount(s, [' ']) do
              p.parameters.Add(ExtractWord(i,s,[' ']));
          end;

          lua_settop(luavm, t);
        finally
          luacs.leave;
        end;
      end;

      try
        p.Execute;
      except
        on e: exception do
        begin
          OutputDebugString(e.message);
          raise;
        end;
      end;


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

      //parse the output
      r:=tstringlist.create;
      try
        output.position:=0;
        r.loadfromstream(output);


        found:=false;
        for i:=0 to r.count-2 do
        begin
          line1:=r[i];
          line2:=r[i+1];

          if (copy(line1, 1,2)='0x') and (copy(line1, 1,2)='0x') then
          begin
            a1:=ExtractWord(1, line1, [' ',#8,#9]);
            a2:=ExtractWord(1, line2, [' ',#8,#9]);

            if TryStrToInt64(a1,address1) and TryStrToInt64(a2,address2) then
            begin
              instr:=ExtractWord(2, line1, [' ',#8,#9]);
              param:='';
              comment:='';
              j:=WordPosition(3, line1, [' ',#8,#9]);
              if j>0 then
                param:=copy(line1, j, length(line1));


              if DisassemblerCommentChar<>'' then
              begin
                j:=pos(DisassemblerCommentChar, param);
                if j>0 then
                begin
                  comment:=copy(param, j, length(param));
                  param:=copy(param,1, j-1);
                end;
              end;

              //valid
              if disassemblercache.GetData(address1, entry) then
              begin
                cleanEntry(disassemblercache.GetDataPtr(address1));
                disassemblercache.Delete(address1);
              end;

              entry.bytesize:=address2-address1;
              getmem(entry.bytes, entry.bytesize);
              for j:=0 to entry.bytesize-1 do
                entry.bytes[j]:=buffer[address1-ldd.address+j];

              entry.instruction:=strnew(pchar(instr));
              entry.parameter:=strnew(pchar(param));
              entry.extra:=strnew(pchar(comment));

              disassemblercache.Add(address1, entry);

              if not found then
              begin
                //fill it in
                ldd.opcode:=instr;
                setlength(ldd.Bytes, entry.bytesize);
                for j:=0 to entry.bytesize-1 do
                  ldd.bytes[j]:=entry.bytes[j];

                ldd.parameters:=param;
                found:=true;
              end;
            end;
          end;
        end;



      finally
        r.free;
      end;



    finally
      deletefile(f);
      if output<>nil then
        output.free;

      p.free;
    end;


  finally
    if buffer<>nil then
      freemem(buffer);
  end;

end;

procedure TBinUtils.ldAndExtract(objectfile, linkfilename, extraparams: string; imports: TImportList; var sections: TBinarySections);
var
  p: TProcess;
  i: integer;

  linkedfilename: string;
  bindata: TMemoryStream;
begin
  p:=TProcess.create(nil);
  try
    p.CurrentDirectory:=fpath;
    p.executable:='"'+fpath+prefix+'ld'+'"';
    p.Options:=[poUsePipes, poNoConsole];
    p.Parameters.add(extraparams);
    p.parameters.add(LDParam);
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
      p.executable:='"'+fpath+prefix+'objcopy'+'"';
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

  idx, _name, size: string;

  s: string;
begin
  p:=TProcess.Create(nil);
  output:=nil;
  r:=nil;

  try
    p.CurrentDirectory:=fpath;
    p.Executable:='"'+fpath+prefix+'objdump'+'"';
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
          _name:=ExtractWord(2, s,[' ',#8]);
          size:=ExtractWord(3, s,[' ',#8]);

          if TryStrToInt('0x'+size, j) then
          begin
            if (j>0) then
            begin
              setlength(sections, length(sections)+1);
              sections[length(sections)-1].name:=_name;
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
    p.Executable:='"'+fpath+prefix+'nm'+'"';
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

function TBinUtils.assemble(script: tstringlist; extraparams: string; out filename: string): boolean;
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
    p.Executable:='"'+fpath+prefix+'as'+'"';
    p.Options:=[poUsePipes, poWaitOnExit, poNoConsole];

    p.Parameters.add(extraparams);
    p.parameters.add(ASParam);
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
  binutilslist:=Tlist.create;

  {
  --[[
  Registers the ARM binutils with CE
  --]]

  local armconfig={}
  armconfig.Name="ARM" --set the name if there are multiple binutils for whatever reason
  armconfig.Description="BinUtils for ARM" -- ^
  armconfig.Architecture="arm" --used by the objdump -m<architecture>
  armconfig.OnDisassemble=function(address)
    if (address & 1)==1 then
      return "-Mforce-thumb"
    end
  end
  armconfig.Path=getCheatEngineDir()..[[binutils\arm\bin]]
  armconfig.Prefix="arm-linux-androideabi-"

  registerBinUtil(armconfig)
  }

end.

