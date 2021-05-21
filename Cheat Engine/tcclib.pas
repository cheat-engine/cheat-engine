unit tcclib;

{$mode objfpc}{$H+}


interface

uses
  {$ifdef windows}windows,{$endif}
  {$ifdef darwin}macport, dl,macportdefines, {$endif}
  Classes, SysUtils, syncobjs, maps;


type
  TTCCTarget=(x86_64,i386);
  PTCCState=pointer;
  {$ifdef standalonetest}
  TSymbolListHandler=pointer;
  {$endif}


  TTCCMemorystream=class(TMemoryStream)
  private
    base: ptruint; //this unit owns the TTCCMemorystream so it can access base no problem, the caller units not so much
  protected
    function Realloc(var NewCapacity: PtrInt): Pointer; override;
  public
  end;




  TLineNumberInfo=record
    address: ptruint;
    functionaddress: ptruint;
    linenr: integer;
    sourcecode: pchar; //just this line
    sourcefile: tstrings; //the sourcecode this line belongs to
  end;
  PLineNumberInfo=^TLineNumberInfo;

  TSourceCodeInfo=class(TObject)
  private
    AddressToLineNumberInfo: tmap;
    sources: TStringlist;
    minaddress: ptruint;
    maxaddress: ptruint;
  public
    procedure outputDebugInfo(o: tstrings);
    procedure add(functionaddress, address: ptruint; linenr: integer; sourceline: string; sourcefile: tstrings);
    function get(address: ptruint): PLineNumberInfo;

    procedure AddSource(sourcefilename: string; sourcecode: tstrings); //sourcecode string objects passed become owned by TSourceCodeInfo and will be destroyed when it gets destroyed
    function GetSource(sourcefilename: string): tstrings;

    procedure getRange(out start: ptruint; out stop: ptruint);

    procedure register;
    procedure unregister;

    constructor create;
    destructor destroy; override;
  end;

  TTCC=class(TObject)
  private
    cs: TCriticalSection; static;
    working: boolean;

    new: function():PTCCState; cdecl;
    parse_args: function(s:PTCCState; pargc: pinteger; pargv: pchar; optind: integer):integer; cdecl;//  //(TCCState *s, int *pargc, char ***pargv, int optind)
    set_options: procedure(s: PTCCState; str: pchar); cdecl;
    set_lib_path: procedure(s: PTCCState; path: pchar); cdecl;
    add_include_path: procedure(s: PTCCState; path: pchar); cdecl;
    set_error_func:procedure(s: PTCCState; error_opaque: pointer; functionToCall: pointer); cdecl;
    set_symbol_lookup_func:procedure(s:PTCCState; userdata: pointer; functionToCall: pointer); cdecl;
    set_binary_writer_func:procedure(s:PTCCState; userdata: pointer; functionToCall: pointer); cdecl;

    set_output_type:function(s: PTCCState; output_type: integer): integer; cdecl;
    compile_string:function(s: PTCCState; buf: pchar): integer; cdecl;


    get_symbol:function(s: PTCCState; name: pchar):pointer; cdecl;
    get_symbols:function(s: PTCCState; userdata: pointer; functionToCall: pointer):pointer; cdecl;

    get_stab: function(s: PTCCState; output: pointer; out outputlength: integer): integer; cdecl;

    delete:procedure(s: PTCCState); cdecl;
    add_file:function(s: PTCCState; filename: pchar): integer; cdecl;
    output_file:function(s: PTCCState; filename: pchar): integer; cdecl;

    relocate:function(s: PTCCState; address: ptruint): integer; //address=0 gets size, address=1 let's tcc decide (nope) address>1 write there using the binary writer


    add_symbol:function(s: PTCCState; name: pchar; val: pointer): integer; cdecl;

    procedure setupCompileEnvironment(s: PTCCState; textlog: tstrings; targetself: boolean=false; nodebug: boolean=false);
    procedure parseStabData(s: PTCCState; symbols: Tstrings; sourcecodeinfo: TSourceCodeInfo; stringsources: tstrings=nil);
  public
    function testcompileScript(script: string; var bytesize: integer; referencedSymbols: TStrings; symbols: TStrings; sourcecodeinfo: TSourceCodeInfo=nil; textlog: tstrings=nil): boolean;
    function compileScript(script: string; address: ptruint; output: tstream; symbollist: TStrings; sourcecodeinfo: TSourceCodeInfo=nil; textlog: tstrings=nil; secondaryLookupList: tstrings=nil; targetself: boolean=false): boolean;
    function compileScripts(scripts: tstrings; address: ptruint; output: tstream; symbollist: TStrings; sourcecodeinfo: TSourceCodeInfo=nil; textlog: tstrings=nil; targetself: boolean=false): boolean;
    function compileProject(files: tstrings; address: ptruint; output: tstream; symbollist: TStrings; sourcecodeinfo: TSourceCodeInfo=nil; textlog: tstrings=nil; targetself: boolean=false): boolean;

    constructor create(target: TTCCTarget);
  end;


  function tcc: TTCC;
  function tccself: TTCC;

  procedure tcc_addCIncludePath(path: string);
  procedure tcc_removeCIncludePath(path: string);

implementation

uses forms,dialogs{$ifndef standalonetest}, symbolhandler, ProcessHandlerUnit, newkernelhandler, CEFuncProc, sourcecodehandler{$endif};
const
  TCC_RELOCATE_AUTO=pointer(1); //relocate
  TCC_OUTPUT_MEMORY  = 1; { output will be run in memory (default) }
  TCC_OUTPUT_EXE     = 2; { executable file }
  TCC_OUTPUT_DLL     = 3; { dynamic library }
  TCC_OUTPUT_OBJ     = 4; { object file }
  TCC_OUTPUT_PREPROCESS = 5; { only preprocess (used internally) }


var
  initDone: boolean;
  tcc32: TTCC;
  {$ifdef cpu64}
  tcc64: TTCC;
  {$endif}
  additonalIncludePaths: tstringlist;


procedure tcc_addCIncludePath(path: string);
begin
  if additonalIncludePaths=nil then
    additonalIncludePaths:=tstringlist.create;

  additonalIncludePaths.add(path);
end;

procedure tcc_removeCIncludePath(path: string);
var i: integer;
begin
  if additonalIncludePaths<>nil then
  begin
    i:=additonalIncludePaths.IndexOf(path);
    if i<>-1 then
      additonalIncludePaths.Delete(i);

    if additonalIncludePaths.count=0 then
      freeandnil(additonalIncludePaths);
  end;
end;

function tcc: TTCC;
begin
  {$ifndef standalonetest}
    {$ifdef cpu64}
    if processhandler.is64bit then
      result:=tcc64
    else
    {$endif}
      result:=tcc32;
  {$else}
    result:=tcc64;
  {$endif}
end;

function tccself: TTCC;
begin
  {$ifdef cpu64}
  result:=tcc64;
  {$else}
  result:=tcc32;
  {$endif}
end;

function TTCCMemorystream.Realloc(var NewCapacity: PtrInt): Pointer;
var
  oldcapacity: PtrInt;
  p: pbytearray;
begin
  oldCapacity:=Capacity;
  result:=inherited ReAlloc(NewCapacity);

  if newCapacity>oldcapacity then
  begin
    p:=Memory;
    zeromemory(@p^[oldcapacity], newCapacity-oldcapacity);
  end;
end;

procedure TSourceCodeInfo.outputDebugInfo(o: tstrings);
var
  mi: TMapIterator;
  e: TLineNumberInfo;
begin
  mi:=TMapIterator.Create(AddressToLineNumberInfo);
  mi.First;
  while not mi.EOM do
  begin
    mi.GetData(e);
    o.AddText(inttohex(e.address,8)+':'+inttostr(e.linenr)+' - '+e.sourcecode);
    mi.Next;
  end;

  mi.free;
end;

procedure TSourceCodeInfo.add(functionaddress, address: ptruint; linenr: integer; sourceline: string; sourcefile: tstrings);
var e: TLineNumberInfo;
begin
  e.functionAddress:=functionaddress;
  e.address:=address;
  e.linenr:=linenr;
  e.sourcecode:=strnew(pchar(sourceline));
  e.sourcefile:=sourcefile;
  AddressToLineNumberInfo.Add(address, e);

  {$ifndef standalonetest}
  if minaddress=0 then
    minaddress:=address
  else
    minaddress:=minx(minaddress,address);

  if maxaddress=0 then
    maxaddress:=address
  else
    maxaddress:=maxx(maxaddress,address);
  {$endif}
end;

function TSourceCodeInfo.get(address: ptruint): PLineNumberInfo;
begin
  result:=AddressToLineNumberInfo.GetDataPtr(address);
end;

procedure TSourceCodeInfo.AddSource(sourcefilename: string; sourcecode: tstrings); //sourcecode string objects passed become owned by TSourceCodeInfo and will be destroyed when it gets destroyed
begin
  sources.AddObject(sourcefilename, sourcecode);
end;

function TSourceCodeInfo.GetSource(sourcefilename: string): tstrings;
var i: integer;
begin
  i:=sources.IndexOf(sourcefilename);
  if i<>-1 then
    result:=tstrings(sources.Objects[i])
  else
    result:=nil;
end;

procedure TSourceCodeInfo.getRange(out start: ptruint; out stop: ptruint);
//returns the range of addresses this code encompasses
begin
  start:=minaddress;
  stop:=maxaddress;
end;

procedure TSourceCodeInfo.register;
begin
  {$ifndef standalonetest}
  if SourceCodeInfoCollection=nil then
    SourceCodeInfoCollection:=TSourceCodeInfoCollection.create;

  SourceCodeInfoCollection.addSourceCodeInfo(self);
  {$endif}
end;

procedure TSourceCodeInfo.unregister;
begin
  {$ifndef standalonetest}
  if SourceCodeInfoCollection<>nil then
    SourceCodeInfoCollection.removeSourceCodeInfo(self);
  {$endif}
end;

constructor TSourceCodeInfo.create;
begin
  AddressToLineNumberInfo:=TMap.Create(ituPtrSize,sizeof(TLineNumberInfo));
  sources:=TStringList.create;
end;

destructor TSourceCodeInfo.destroy;
var
  mi: TMapIterator;
  i: integer;
begin
  unregister;

  mi:=TMapIterator.Create(AddressToLineNumberInfo);
  mi.First;
  while not mi.EOM do
  begin
    StrDispose(PLineNumberInfo(mi.DataPtr)^.sourcecode);
    PLineNumberInfo(mi.DataPtr)^.sourcecode:=nil;
    mi.Next;
  end;

  mi.free;

  AddressToLineNumberInfo.Free;
  AddressToLineNumberInfo:=nil;

  for i:=0 to sources.count-1 do
    sources.Objects[i].Free;

  sources.free;
  inherited destroy;
end;



constructor TTCC.create(target: TTCCTarget);
var
  module: HModule;
  p: string;
begin
  if initDone=true then raise exception.create('Do not create more compilers after init');
  if cs=nil then
    cs:=TCriticalSection.create;

  {$ifdef windows}

  {$ifdef cpu32}
  module:=LoadLibrary({$ifdef standalonetest}'D:\git\cheat-engine\Cheat Engine\bin\'+{$endif}'tcc32-32.dll'); //generates 32-bit code
  {$else}
  if target=x86_64 then
    module:=loadlibrary({$ifdef standalonetest}'D:\git\cheat-engine\Cheat Engine\bin\'+{$endif}'tcc64-64.dll')
  else
    module:=loadlibrary({$ifdef standalonetest}'D:\git\cheat-engine\Cheat Engine\bin\'+{$endif}'tcc64-32.dll'); //generates 32-bit code
  {$endif}
  {$else}
  module:=loadlibrary({$ifdef standalonetest}'D:\git\cheat-engine\Cheat Engine\bin\'+{$endif}'libtcc.dylib');
  if module=0 then
  begin
    p:=ExtractFilePath(application.ExeName)+'libtcc.dylib';


    module:=loadlibrary(p);
  end;
  {$endif}

  working:=false;

  pointer(new):=GetProcAddress(module,'tcc_new');
  pointer(parse_args):=GetProcAddress(module,'tcc_parse_args');
  pointer(set_options):=GetProcAddress(module,'tcc_set_options');
  pointer(set_lib_path):=GetProcAddress(module,'tcc_set_lib_path');
  pointer(add_include_path):=GetProcAddress(module,'tcc_add_include_path');
  pointer(set_error_func):=GetProcAddress(module,'tcc_set_error_func');

  pointer(set_output_type):=GetProcAddress(module,'tcc_set_output_type');
  pointer(set_symbol_lookup_func):=GetProcAddress(module,'tcc_set_symbol_lookup_func');
  pointer(set_binary_writer_func):=GetProcAddress(module,'tcc_set_binary_writer_func');
  pointer(compile_string):=GetProcAddress(module,'tcc_compile_string');

  pointer(add_symbol):=GetProcAddress(module,'tcc_add_symbol');
  pointer(add_file):=GetProcAddress(module,'tcc_add_file');
  pointer(output_file):=GetProcAddress(module,'tcc_output_file');
  pointer(relocate):=GetProcAddress(module,'tcc_relocate');
  pointer(get_symbol):=GetProcAddress(module,'tcc_get_symbol');
  pointer(get_symbols):=GetProcAddress(module,'tcc_get_symbols');
  pointer(delete):=GetProcAddress(module,'tcc_delete');

  pointer(get_stab):=GetProcAddress(module,'tcc_get_stab');


  working:=(module<>0) and
           assigned(new) and
           assigned(set_options) and
           assigned(add_include_path) and
           assigned(compile_string) and
           assigned(output_file) and
           assigned(delete) and
           assigned(get_stab);
end;

procedure ErrorLogger(opaque: pointer; msg: pchar); cdecl;
begin
  {$ifdef standalonetest}
  showmessage(msg);
  {$endif}
  tstrings(opaque).Add(msg);
end;

function symbolLookupFunctionTestCompile( log: tstrings; name: pchar): pointer; cdecl;
begin
  result:=pointer($00400000);

  if log<>nil then
    log.add(name);
end;

function symbolLookupFunction(secondaryLookup: tstrings; name: pchar): pointer; cdecl;
var
  error: boolean;
  i: integer;
begin
  {$ifdef standalonetest}
  result:=pointer($1234);
  {$else}

  if secondaryLookup<>nil then
  begin
    i:=secondaryLookup.IndexOf(name);
    if i<>-1 then
      exit(secondaryLookup.Objects[i]);
  end;

  result:=pointer(symhandler.GetAddressFromName(name,true,error));
  {$endif}
end;

{$ifndef standalonetest}
function symbolLookupFunctionSelf(secondaryLookup: tstrings; name: pchar): pointer; cdecl;
var
  error: boolean;
  i: integer;
begin


  if secondaryLookup<>nil then
  begin
    i:=secondaryLookup.IndexOf(name);
    if i<>-1 then
      exit(secondaryLookup.Objects[i]);
  end;

  result:=pointer(selfsymhandler.GetAddressFromName(name,true,error));

end;
{$endif}


procedure SelfWriter(userdata: tobject; address: ptruint; data: pointer; size: integer);  cdecl; //writes to the local process
begin
  CopyMemory(pointer(address), data, size);
end;

procedure NullWriter(userdata: tobject; address: ptruint; data: pointer; size: integer);  cdecl; //writes nothing
begin
end;

procedure TCCMemorystreamWriter(m: TTCCMemorystream; address: ptruint; data: pointer; size: integer);  cdecl; //Writes to a TTCCMemorystreamWriter based on the base address stored within
begin
  m.position:=address-m.base;
  m.WriteBuffer(data^,size);
end;

{$ifndef standalonetest}
procedure MemoryWriter(userdata: tobject; address: ptruint; data: pointer; size: integer);  cdecl; //Writes directly to the target process memory
var bw: size_t;
begin
  WriteProcessMemory(processhandle,pointer(address),data,size,bw);
end;
{$endif}


procedure symbolCallback(sl: TStrings; address: qword; name: pchar); cdecl;
var s: string;
begin
  if trim(name)='' then exit;

  if (length(name)>=4) then
  begin
    case name[0] of
      '.': if name='.uw_base' then exit;
      '_':
      begin

        case name[1] of
          'e': if (name = '_etext') or (name='_edata') or (name='_end') then exit;
          '_':
          begin
            s:=name;
            if s.EndsWith('array_start') or s.EndsWith('array_end') then exit;
          end;
        end;
      end;
    end;
  end;

  if sl<>nil then
    sl.AddObject(name, tobject(address));

end;

procedure ttcc.setupCompileEnvironment(s: PTCCState; textlog: tstrings; targetself: boolean=false; nodebug: boolean=false);
var
  params: string;
  i: integer;
begin
  add_include_path(s,{$ifdef standalonetest}'D:\git\cheat-engine\Cheat Engine\bin\'+{$endif}'include');
  {$ifdef windows}
  add_include_path(s,{$ifdef standalonetest}'D:\git\cheat-engine\Cheat Engine\bin\'+{$endif}'include\winapi');
  {$endif}
  add_include_path(s,{$ifdef standalonetest}'D:\git\cheat-engine\Cheat Engine\bin\'+{$endif}'include\sys');
  add_include_path(s,pchar(ExtractFilePath(application.exename)+'include'));
  {$ifdef windows}
  add_include_path(s,pchar(ExtractFilePath(application.exename)+'include\winapi'));
  {$endif}
  add_include_path(s,pchar(ExtractFilePath(application.exename)+'include\sys'));


  if additonalIncludePaths<>nil then
    for i:=0 to additonalIncludePaths.count-1 do
      add_include_path(s,pchar(additonalIncludePaths[i]));


  if textlog<>nil then set_error_func(s,textlog,@ErrorLogger);

  params:='-nostdlib Wl,-section-alignment=4';
  if nodebug=false then
    params:='-g '+params;


  set_options(s,pchar(params));
  set_output_type(s,TCC_OUTPUT_MEMORY);


{$ifndef standalonetest}
  if targetself then
    set_symbol_lookup_func(s,nil,@symbolLookupFunctionSelf)
  else
{$endif}
    set_symbol_lookup_func(s,nil,@symbolLookupFunction);
end;

procedure ttcc.parseStabData(s: PTCCState; symbols: Tstrings; sourcecodeinfo: TSourceCodeInfo; stringsources: tstrings=nil );
type
  {
      unsigned int n_strx;         /* index into string table of name */
      unsigned char n_type;         /* type of symbol */
      unsigned char n_other;        /* misc info (usually empty) */
      unsigned short n_desc;        /* description field */
      unsigned int n_value;        /* value of symbol */
  }
  TCCStabEntry=packed record
    n_strx: integer;
    n_type: byte;
    n_other: byte;
    n_desc: word;
    n_value: dword;
  end;
  PCCStabEntry=^TCCStabEntry;
var
  stabdatasize: integer;
  stabdata: pointer;
  stabsize: integer;
  stabstrsize: integer;

  stab: PCCStabEntry;
  stabstr: pchar;
  count: integer;
  i,j,ln, si: integer;

  address: ptruint;

  currentSourceFile: string;
  currentFunction: record
    valid: boolean;
    name: string;
    address: ptruint;
  end;

  str: string;

  sl: Tstringlist;
  source: TStrings;
begin
  source:=nil;
  stabdatasize:=0;

  if get_stab(s,nil, stabdatasize)=-2 then
  begin
    getmem(stabdata, stabdatasize*2);
   // FillMemory(stabdata,stabdatasize, $cc);
    try
      stabdatasize:=stabdatasize*2;
      if get_stab(s, stabdata, stabdatasize)=0 then
      begin
        //parse the stabdata and output linenumbers and sourcecode
        stabsize:=pdword(stabdata)^;
        stab:=pointer(ptruint(stabdata)+4);
        stabstr:=pointer(ptruint(stabdata)+4+stabsize);
        stabstrsize:=stabdatasize-4-stabsize;

        count:=stabsize div (sizeof(TCCStabEntry)); //12

        for i:=0 to count-1 do
        begin
          case stab[i].n_type of
          //$a0/$80=  local vars

            $24:
            begin
              //symbol/function
              if stab[i].n_strx>=stabstrsize then
              begin
                continue;
              end;
              str:=pchar(@stabstr[stab[i].n_strx]);
              str:=str.Split(':')[0];

              si:=symbols.IndexOf(str);
              if si<>-1 then
              begin
                currentFunction.valid:=true;
                currentFunction.name:=str;
                currentFunction.address:=ptruint(symbols.Objects[si]);
              end;
            end;

            $44: //Line number info
            begin
              if currentFunction.valid then
              begin
                ln:=stab[i].n_desc;
                if ln>0 then
                begin

                  address:=currentFunction.address+stab[i].n_value;

                  if (source<>nil) and (ln<=source.Count) then
                  begin
                    sl:=tstringlist.create;

                    sl.add(source[ln-1]);
                    source.Objects[ln-1]:=tobject(address);

                    //deal with some code layout preferences
                    if (ln<source.count) and (source[ln]='}') then
                    begin
                      sl.add(source[ln]);
                      source.Objects[ln]:=tobject(address);
                    end;

                    if (ln>1) and (trim(source[ln-1])='{') then
                    begin
                      sl.Insert(0,source[ln-2]);
                      source.Objects[ln-2]:=tobject(address);
                    end;

                    sl.insert(0, format('%s:%2d', [extractfilename(currentSourceFile),ln]));

                    sourcecodeinfo.add(currentFunction.address, address, ln, sl.text, source);
                  end;
                end;


              end;
            end;

            $84:
            begin
              currentFunction.valid:=false;
              if stab[i].n_strx>=stabstrsize then
              begin
                source:=nil;
                currentSourceFile:='';
                continue;
              end;

              currentSourceFile:=pchar(@stabstr[stab[i].n_strx]);

              if trim(currentSourceFile)='' then continue;

              source:=sourcecodeinfo.GetSource(currentSourceFile);
              if source=nil then
              begin
                source:=tstringlist.create;
                if (stringsources<>nil) and (currentsourcefile.StartsWith('<string')) then
                begin
                  str:='';
                  for j:=length(currentSourceFile)-1 downto 1 do
                  begin
                    if currentsourcefile[j] in ['0'..'9'] then
                      str:=currentsourcefile[j]+str
                    else
                      break;
                  end;

                  j:=strtoint(str);
                  if j<stringsources.Count then
                    source.Text:=stringsources[j];
                end
                else
                begin
                  try
                    source.LoadFromFile(currentSourceFile);
                  except
                    freeandnil(source);
                    currentSourceFile:='';
                    continue;
                  end;
                end;

                sourcecodeinfo.AddSource(currentSourceFile, source);
              end;
            end;
          end;
        end;

      end;
    finally
      freemem(stabdata);
    end;
  end;
end;

function ttcc.testcompileScript(script: string; var bytesize: integer; referencedSymbols: TStrings; symbols: TStrings; sourcecodeinfo: TSourceCodeInfo; textlog: tstrings=nil): boolean;
var s: PTCCState;
  r: pointer;
  ms: Tmemorystream;

begin
  result:=false;
  if not working then
  begin
    if textlog<>nil then textlog.add('Incorrect tcc library');
    exit(false);
  end;


  cs.enter;
  s:=new();
  ms:=tmemorystream.create;
  try
    setupCompileEnvironment(s, textlog,false, sourcecodeinfo=nil);

    set_binary_writer_func(s,nil,@NullWriter);
    set_symbol_lookup_func(s,referencedSymbols, @symbolLookupFunctionTestCompile);

    if compile_string(s,pchar(script))=-1 then exit(false);
    bytesize:=relocate(s,0);
    if bytesize<=0 then exit(false);

    relocate(s,$00400000);

    if symbols<>nil then
      get_symbols(s, symbols, @symbolCallback);


    result:=true;
  finally
    delete(s);
    cs.leave;

    if ms<>nil then
      freeandnil(ms);
  end;
end;

function ttcc.compileScript(script: string; address: ptruint; output: tstream; symbollist: TStrings; sourcecodeinfo: TSourceCodeInfo=nil; textlog: tstrings=nil; secondaryLookupList: tstrings=nil; targetself: boolean=false): boolean;
var s: PTCCState;
  r: pointer;

  size: integer;
  tms: TTCCMemorystream=nil;

  sources: tstringlist;
begin
  result:=false;
  if not working then
  begin
    if textlog<>nil then textlog.add('Incorrect tcc library');
    exit(false);
  end;

  if address=0 then
  begin
    if textlog<>nil then textlog.add('Can not compile at address 0');
    exit(false);
  end;


  cs.enter;
  s:=new();
  try
    setupCompileEnvironment(s, textlog, targetself, sourcecodeinfo=nil);

    if output is TTCCMemorystream then
      tms:=TTCCMemorystream(output)
    else
      tms:=TTCCMemorystream.create;

    tms.base:=address;

    set_binary_writer_func(s,tms,@TCCMemorystreamWriter);

    if secondaryLookupList<>nil then   //AA scripts can provide some extra addresses
      set_symbol_lookup_func(s,secondaryLookupList, @symbolLookupFunction);

    if compile_string(s,pchar(script))=-1 then exit(false);

    if relocate(s,0)=-1 then exit(false);
    if relocate(s,address)=-1 then exit(false);

    if symbollist<>nil then
      get_symbols(s, symbollist, @symbolCallback);

    if (output is TTCCMemorystream)=false then
    begin
      tms.position:=0;
      tms.SaveToStream(output);
    end;

    if (symbollist<>nil) and (sourcecodeinfo<>nil) then
    begin
      sources:=tstringlist.create;
      sources.Add(script);
      parseStabData(s, symbollist, sourcecodeinfo, sources);
      sources.free;
    end;

    result:=true;
  finally
    delete(s);
    cs.leave;

    if (tms<>nil) and ((output is TTCCMemorystream)=false) then
      tms.free;
  end;
end;

function ttcc.compileScripts(scripts: tstrings; address: ptruint; output: tstream; symbollist: TStrings; sourcecodeinfo: TSourceCodeInfo=nil; textlog: tstrings=nil; targetself: boolean=false):boolean;
var
  s: PTCCState;
  i: integer;
  tms: TTCCMemorystream=nil;
begin
  result:=false;
  if not working then
  begin
    if textlog<>nil then textlog.add('Incorrect tcc library');
    exit(false);
  end;

  if address=0 then
  begin
    if textlog<>nil then textlog.add('Can not compile at address 0');
    exit(false);
  end;

  cs.enter;
  s:=new();
  try
    setupCompileEnvironment(s, textlog, targetself, sourcecodeinfo=nil);

    if output is TTCCMemorystream then
      tms:=TTCCMemorystream(output)
    else
      tms:=TTCCMemorystream.create;

    tms.base:=address;

    set_binary_writer_func(s,tms,@TCCMemorystreamWriter);


    for i:=0 to scripts.count-1 do
      if compile_string(s,pchar(scripts[i]))=-1 then exit(false);


    if relocate(s,0)=-1 then exit(false);
    if relocate(s,address)=-1 then exit(false);

    //still alive, get the symbols
    if symbollist<>nil then
      get_symbols(s, symbollist, @symbolCallback);

    if (output is TTCCMemorystream)=false then
    begin
      tms.position:=0;
      tms.SaveToStream(output);
    end;

    if (symbollist<>nil) and (sourcecodeinfo<>nil) then
      parseStabData(s, symbollist, sourcecodeinfo, scripts);


    result:=true;
  finally
    delete(s);
    cs.leave;

    if (tms<>nil) and ((output is TTCCMemorystream)=false) then
      tms.free;
  end;

end;


function ttcc.compileProject(files: tstrings; address: ptruint; output: tstream; symbollist: TStrings; sourcecodeinfo: TSourceCodeInfo=nil; textlog: tstrings=nil; targetself: boolean=false):boolean;
var
  s: PTCCState;
  i: integer;
  tms: TTCCMemorystream=nil;
begin
  result:=false;
  if not working then
  begin
    if textlog<>nil then textlog.add('Incorrect tcc library');
    exit(false);
  end;

  if address=0 then
  begin
    if textlog<>nil then textlog.add('Can not compile at address 0');
    exit(false);
  end;

  cs.enter;
  s:=new();
  try
    setupCompileEnvironment(s, textlog, targetself, sourcecodeinfo=nil);

    if output is TTCCMemorystream then
      tms:=TTCCMemorystream(output)
    else
      tms:=TTCCMemorystream.create;

    tms.base:=address;

    set_binary_writer_func(s,tms,@TCCMemorystreamWriter);

    for i:=0 to files.count-1 do
      if add_file(s, pchar(files[i]))=-1 then exit(false);

    if relocate(s,0)=-1 then exit(false);
    if relocate(s,address)=-1 then exit(false);


    //still alive, get the symbols
    if symbollist<>nil then
      get_symbols(s, symbollist, @symbolCallback);

    if (output is TTCCMemorystream)=false then
    begin
      tms.position:=0;
      tms.SaveToStream(output);
    end;

    if (symbollist<>nil) and (sourcecodeinfo<>nil) then
      parseStabData(s, symbollist, sourcecodeinfo);



    result:=true;
  finally
    delete(s);
    cs.leave;

    if (tms<>nil) and ((output is TTCCMemorystream)=false) then
      tms.free;
  end;

end;


function initTCCLib: boolean;
begin
{$ifdef windows}
  {$ifndef standalonetest}
  tcc32:=ttcc.create(i386);
 {$endif}

  {$ifdef cpu64}
  tcc64:=ttcc.create(x86_64);
  {$endif}
{$else}
  tcc32:=ttcc.create(x86_64);
  tcc64:=ttcc.create(x86_64);
{$endif}

  initDone:=true;
  result:=initdone;
end;

initialization
  initTCCLib;

end.

