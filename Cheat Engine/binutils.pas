unit binutils;

//responsible for launching and retrieving data from the binutil tools

{$mode objfpc}{$H+}
{$warn 2005 off}

interface

uses
  windows, newkernelhandler, forms, Classes, SysUtils, process, LastDisassembleData, strutils, maps;

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
    procedure clearLongRangeDisassemblerCache(address: ptruint);


    function compile(script: tstringlist): boolean;
    function compileproject(filelist: tstringlist): boolean;

    procedure LinkObjectFiles(objectfiles: tstringlist; const imports: TImportList; targetfilename: string; targetaddress:ptruint=0);
    procedure ObjectToBinary(objectfile: string; binfile: string);
    procedure stripSection(objectfile: string; section: string);

    function assemble(script: tstringlist; extraparams: string; out filename: string): boolean;
    procedure nm(filename: string; defined: tstringlist; undefined: tstringlist; registersymbols: boolean=false);
    procedure GetSections(filename: string; var sections: TSectionArray);
    procedure ldAndExtract(objectfile, linkfilename, extraparams: string; imports: TImportList; var sections: TBinarySections);
    procedure ldAndInject(objectfiles: tstringlist);
    procedure disassemble(var ldd: TLastDisassembleData); //address and bytes are filled in
  published
    property path: string read fpath write setPath;
  end;


var
  binutilslist: TList;
  defaultBinutils: TBinUtils; //used by the disassembler and single line assember if set (gui thing)


implementation

uses dialogs, lua, LuaHandler, ProcessHandlerUnit, symbolhandler;

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
  begin
    FreeMemAndNil(entry^.bytes);

  end;

  if entry^.instruction<>nil then
  begin
    strdispose(entry^.instruction);
    entry^.instruction:=nil;
  end;

  if entry^.parameter<>nil then
  begin
    strdispose(entry^.parameter);
    entry^.parameter:=nil;
  end;

  if entry^.extra<>nil then
  begin
    strdispose(entry^.extra);
    entry^.extra:=nil;
  end;
end;

procedure TBinUtils.clearLongRangeDisassemblerCache(address: ptruint);
var
  it: TMapIterator;
  entry: PDisassemblerCacheEntry;

  id: ptruint;

  mina: ptruint;
  maxa: ptruint;

begin
  //clear all entries that are 25000 bytes before after address (leaving about 50000 out of 100000 entries)
  if address>25000 then
    mina:=address-25000;

  if mina>address then //overflow
    mina:=0;

  maxa:=address+25000;
  if maxa<address then
    maxa:={$ifdef cpu32}qword($ffffffff){$else}qword($ffffffffffffffff){$endif};

  it:=TMapIterator.Create(disassemblercache);
  try
    it.first;
    while not it.eom do
    begin
      entry:=it.DataPtr;
      it.GetID(id);

      if (id<mina) or (id>maxa) then
      begin
        cleanentry(entry);
        disassemblercache.Delete(id);
      end;

      it.next;
    end;
  finally
    it.free;
  end;
end;

procedure TBinUtils.clearDisassemblerCache;   //full clear
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

  disassemblercache.Clear;
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

  luavm: PLua_state;

  os: string;
  params: array of string;
begin
  //check if this address is in the disassembled cache, and if so, add it
  luavm:=GetLuaState;

  getmem(buffer, 512);
  try
    FillByte(buffer^, 512,0);
    br:=0;

    ReadProcessMemory(processhandle, pointer(ldd.address), buffer, 512, br);

    if br=0 then
    begin
      ldd.opcode:='??';
      ldd.parameters:='';
      setlength(ldd.bytes,0);
      exit;
    end;

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

    if disassemblercache.Count>100000 then
      clearLongRangeDisassemblerCache(ldd.address);


    //still here, disassemble this address and a couple of instructions after it as well
    f:=GetTempFileName;
    fs:=tfilestream.Create(f, fmCreate);
    fs.WriteBuffer(buffer[0], 512);
    fs.Free;


    p:=TProcess.Create(Application);
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
        //LuaCS.enter;
        t:=lua_gettop(luavm);
        try
          lua_rawgeti(luavm, LUA_REGISTRYINDEX, OnDisassemble);
          lua_pushinteger(Luavm, ldd.address);
          if lua_pcall(luavm, 1, 1, 0)=0 then
          begin
            s:=Lua_ToString(Luavm, -1);

            for i:=1 to WordCount(s, [' ']) do
              p.parameters.Add(ExtractWord(i,s,[' ']));
          end;
        finally
          lua_settop(luavm, t);
        end;
      end;

     { try
        p.Execute;
      except
        on e: exception do
        begin
          OutputDebugString(e.message);
          raise;
        end;
      end; }

      setlength(params, p.parameters.Count);
      for i:=0 to p.parameters.count-1 do
        params[i]:=p.Parameters[i];
        
      if RunCommand(p.Executable, params, os,[poNoConsole]) then
      begin
        output:=TStringStream.create('');
        output.WriteString(os);
      end
      else
      begin
        //failure to run
        ldd.opcode:='Invalid';
        ldd.parameters:='binutils';
        setlength(ldd.bytes,0);
        exit;
      end;

{
      output:=TStringStream.create('');

      repeat
        if p.Output.NumBytesAvailable>0 then
          output.CopyFrom(p.Output, p.Output.NumBytesAvailable)
        else
          sleep(1);
      until not p.Running;

      p.WaitOnExit; //just to be sure

      if p.Output.NumBytesAvailable>0 then
          output.CopyFrom(p.Output, p.Output.NumBytesAvailable);   }

      {if p.ExitCode<>0 then
      begin
        setlength(lasterror, p.Stderr.NumBytesAvailable+1);
        p.Stderr.ReadBuffer(lasterror[1], length(lasterror)-1);
        lasterror[length(lasterror)]:=#0;


        raise exception.create(lasterror);
      end;}

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
                cleanEntry(@entry);
                disassemblercache.Delete(address1);
              end;

              FillByte(entry,sizeof(entry),0);

              entry.bytesize:=address2-address1;
              if (entry.bytesize>64) or (entry.bytesize<=0) then
                break;

              getmem(entry.bytes, entry.bytesize);

              if entry.bytes=nil then
                break;

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
                  ldd.bytes[j]:=$90; //entry.bytes[j];

                ldd.parameters:=param;
                found:=true;
              end;
            end;
          end;
        end;  //for

        if not found then
        begin
          ldd.opcode:='??';
          ldd.parameters:='';
          setlength(ldd.bytes,0);
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
      FreeMemAndNil(buffer);
  end;

end;

procedure TbinUtils.ObjectToBinary(objectfile: string; binfile: string);
var objcopyprocess: tprocess;
begin
  objcopyprocess:=TProcess.create(nil);
  objcopyprocess.CurrentDirectory:=fpath;
  objcopyprocess.executable:='"'+fpath+prefix+'objcopy'+'"';
  objcopyprocess.Options:=[poUsePipes, poNoConsole];

  objcopyprocess.Parameters.Add('-Obinary');
  objcopyprocess.Parameters.Add(objectfile);
  objcopyprocess.Parameters.Add(binfile);

  objcopyprocess.Execute;
  if objcopyprocess.WaitOnExit then
  begin
    if objcopyprocess.ExitCode<>0 then
    begin
      setlength(lasterror, objcopyprocess.Stderr.NumBytesAvailable+1);
      objcopyprocess.Stderr.ReadBuffer(lasterror[1], length(lasterror)-1);
      lasterror[length(lasterror)]:=#0;

      raise exception.create('ObjectToBinary: objcopy:'+lasterror);
    end;
  end
  else
    raise exception.create('ObjectToBinary: Wait error on objcopy');
end;

procedure TBinUtils.LinkObjectFiles(objectfiles: tstringlist; const imports: TImportList; targetfilename: string; targetaddress:ptruint=0);
var
  p: tprocess;
  i: integer;

begin
  if targetaddress=$00400000 then targetaddress:=$00400010; //does not work with 0x00400000
  p:=TProcess.create(nil);
  p.CurrentDirectory:=fpath;
  p.executable:='"'+fpath+prefix+'ld'+'"';
  p.Options:=[poUsePipes, poNoConsole];
  p.Parameters.Add('-static');
  p.parameters.add(LDParam);      //-mi386pe for 32-bit
  p.Parameters.Add('-T NUL');
  if targetaddress<>0 then
    p.parameters.add('-Ttext 0x'+inttohex(targetaddress,1));

  for i:=0 to objectfiles.count-1 do
    p.Parameters.Add(objectfiles[i]);

  for i:=0 to length(imports)-1 do
    p.Parameters.Add('--defsym='+imports[i].name+'=0x'+inttohex(imports[i].address,8));

  p.Parameters.Add('-o "'+targetfilename+'"');

  p.Execute;
  if p.WaitOnExit then
  begin
    if p.ExitCode<>0 then
    begin
      setlength(lasterror, p.Stderr.NumBytesAvailable+1);
      p.Stderr.ReadBuffer(lasterror[1], length(lasterror)-1);
      lasterror[length(lasterror)]:=#0;
        raise exception.create(lasterror);
    end;
  end;

end;

procedure TBinUtils.ldAndInject(objectfiles: tstringlist);
var
  p: tprocess;

  m: tmemorystream;
  z: tfilestream;
  size,size2: integer;
  i,j: integer;
  tempfile: string='';
  imports: TImportList;

  defined, undefined: tstringlist;
  found: boolean;
  mem: pointer=nil;
  x: ptruint;
begin
  //first get all the imports
  defined:=tstringlist.create;
  undefined:=tstringlist.create;
  try
    for i:=0 to objectfiles.count-1 do
      nm(objectfiles[i], defined, undefined);

    //go through the undefined list and fill them in (where possible)
    setlength(imports,0);
    for i:=0 to undefined.count-1 do
    begin
      found:=false;
      for j:=0 to length(imports)-1 do
      begin
        if imports[j].name=undefined[i] then
        begin
          found:=true;
          break;
        end;
      end;

      if not found then
      begin
        j:=length(imports);
        setlength(imports, j+1);
        imports[j].name:=undefined[i];
        imports[j].address:=symhandler.getAddressFromName(undefined[i]); //error out if not found
      end;
    end;
  finally
    defined.free;
    undefined.free;
  end;

  //still here so the undefinedlist has been handled

  tempfile:=GetTempFileName;
  LinkObjectFiles(objectfiles, imports, tempfile );

  ObjectToBinary(tempfile, tempfile+'.bin');
  z:=tfilestream.create(tempfile+'.bin', fmOpenRead);
  size:=z.Size+4096;
  z.free;

  if size=0 then raise exception.create('Failure getting filesize');


  //still here so linking was succesful

  repeat
    size2:=size;
    if mem<>nil then VirtualFreeEx(processhandle, mem, 0,MEM_RELEASE);
    mem:=VirtualAllocEx(processhandle, nil,size,mem_reserve or mem_commit, PAGE_EXECUTE_READWRITE);
    if mem=nil then raise exception.create('Failure allocating memory');
    LinkObjectFiles(objectfiles, imports, tempfile, ptruint(mem) );
    ObjectToBinary(tempfile, tempfile+'.bin');

    z:=tfilestream.create(tempfile+'.bin' ,fmOpenRead);
    size:=z.Size+4096;
    z.free;
  until size<=size2;  //repeat linking until the proper size is found (at most 2)


  m:=tmemorystream.create;
  m.loadfromfile(tempfile+'.bin');
  WriteProcessMemory(processhandle, mem, m.memory, m.size,x);
  m.free;

  defined.clear;
  undefined.clear;
  nm(tempfile, defined, undefined, true);
end;

procedure TBinUtils.ldAndExtract(objectfile, linkfilename, extraparams: string; imports: TImportList; var sections: TBinarySections);  //for gnu assembler
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

procedure TBinUtils.nm(filename: string; defined: tstringlist; undefined: tstringlist; registersymbols: boolean=false);
{
Executes nm and keeps a list of all defined and undefined symbols
The call can be done with a defined and undefined list already filled in. In that case, just update
}
var
  p: TProcess;
  ec: integer;

  output: TStringStream;
  s: tstringlist;
  i,j: integer;
  line: string;

  str: string;
  a: ptruint;
  e: integer;
begin
  output:=TStringStream.Create('');
  p:=TProcess.Create(nil);
  try
    p.CurrentDirectory:=fpath;
    p.Executable:='"'+fpath+prefix+'nm'+'"';
    p.Options:=[poUsePipes, poNoConsole];
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
        if (length(line)>2) then
        begin
          if (line[1]='U') then
          begin
            str:=copy(line, 3, length(line));

            if (defined.IndexOf(str)<>-1) or (undefined.IndexOf(str)<>-1) then continue; //it's already defined somewhere else or already in the undefined list
            undefined.add(str); //add it to the undefined list
          end
          else
          begin
            str:=copy(line, RPos(' ',line)+1, length(line));
            j:=undefined.IndexOf(str); //check if it was undefined previously
            if j<>-1 then undefined.Delete(j); //yes, found it

            defined.add(str);
           {
            if registersymbols then
            begin
              j:=pos(' ',line);
              if j>0 then
              begin
                str:=copy(line,1,j-1);
                a:=StrToInt64('$'+str);


              end;
            end; }
          end;
        end;
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

procedure TBinUtils.stripSection(objectfile: string; section: string);
var
  p: TProcess;
begin
  p:=tprocess.create(nil);
  p.CurrentDirectory:=fpath;
  p.Executable:='"'+fpath+prefix+'objcopy'+'"';
  p.Options:=[poUsePipes, poWaitOnExit, poNoConsole];
  p.parameters.add('--remove-section '+section);
  p.Execute;
  p.WaitOnExit;

  p.free;
end;

function TBinUtils.compile(script: tstringlist): boolean;
//compiles the given C code , allocates memory for it, links it, and then copies it to the allocated code, and registers the symbols
//single scripts only. (no multifile supported. Use compileproject for that)

//On hold. This needs to be rewritten to be dynamic instead of static and then fill in the relocation table and importtable  (my ld can't link above 0x100000000)

//fuck it, just compile a dll and then force load the dll by parsing the import table
var
  p: TProcess;
  inputfile: string='';
  outputfile: string='';

  i,j: integer;
  found: boolean;
  outputfilelist: tstringlist;
begin
  result:=false;
  p:=tprocess.create(nil);
  inputfile:=GetTempFileName;
  DeleteFile(inputfile+'.c');
  DeleteFile(outputfile+'.o');
  if renamefile(inputfile,inputfile+'.c') then
  begin
    outputfile:=inputfile+'.o';
    inputfile:=inputfile+'.c';

    try
      script.SaveToFile(inputfile);
      p.CurrentDirectory:=fpath;
      p.Executable:='"'+fpath+prefix+'gcc'+'"';
      p.Options:=[poUsePipes, poWaitOnExit, poNoConsole];
      p.parameters.add('-c "'+inputfile+'"');
      p.parameters.add('-o "'+outputfile+'"');
      p.parameters.add(outputfile);

      p.Execute;
      if p.WaitOnExit then
      begin
        if p.exitcode=0 then //compilation success
        begin
          stripSection(outputfile,'.pdata'); //the mingw I use adds a .pdata section which does not like 64-bit

          outputfilelist:=tstringlist.create;
          outputfilelist.add(outputfile);
          ldAndInject(outputfilelist);
          outputfilelist.free;
        end;
      end;
    finally
      if inputfile<>'' then
        deletefile(inputfile);

      if outputfile<>'' then
        deletefile(outputfile);
    end;

  end;
end;

function TBinUtils.compileproject(filelist: tstringlist): boolean;
begin
  result:=false;
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

finalization
  if binutilslist <> nil then
    binutilslist.Free;  

end.

