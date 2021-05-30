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


  TCCStabEntry=packed record
    {
         unsigned int n_strx;         /* index into string table of name */
         unsigned char n_type;         /* type of symbol */
         unsigned char n_other;        /* misc info (usually empty) */
         unsigned short n_desc;        /* description field */
         unsigned int n_value;        /* value of symbol */
     }
    n_strx: integer;
    n_type: byte;
    n_other: byte;
    n_desc: word;
    n_value: dword;
  end;
  PCCStabEntry=^TCCStabEntry;


  TLineNumberInfo=record
    address: ptruint;
    functionaddress: ptruint;
    linenr: integer;
    sourcecode: pchar; //just this line
    sourcefile: tstrings; //the sourcecode this line belongs to   (contains address as well)
  end;
  PLineNumberInfo=^TLineNumberInfo;

  TLocalVariableInfo=record
    name: string;
    offset: integer;//stackframe offset
    vartype: integer;
    ispointer: boolean;
  end;

  TSourceCodeInfo=class(TObject)
  private
    AddressToLineNumberInfo: tmap;
    sources: TStringlist;
    minaddress: ptruint;
    maxaddress: ptruint;

    stabdata: pointer; //for later whan parsing types and local vars
    stabsize: integer;
    stabstrsize: integer;

    stab: PCCStabEntry;
    stabstr: pchar;

    fullyParsed: boolean;


    parsedsource: array of record
      sourcefile: string;
      minaddress: ptruint;
      maxaddress: ptruint;

      functions: array of record
        functionname: string;
        functionaddress: ptruint;
        functionstop: ptruint;


        //the following is only filled in on a full parse
        lexblocks: array of record
          startaddress: ptruint;
          stopaddress: ptruint;
          level: integer;
        end;

        stackvars: array of record
          varstr: string;
          varname: string;
          offset: integer;
          lexblock: integer;
          ispointer: boolean;

          typenr: integer;
        end;

        parameters: array of record
          varstr: string;
          varname: string;
          offset: integer;

          typenr: integer;
          ispointer: boolean;
        end;
      end;
    end;


    procedure AddSource(sourcefilename: string; sourcecode: tstrings); //sourcecode string objects passed become owned by TSourceCodeInfo and will be destroyed when it gets destroyed
    function GetSource(sourcefilename: string): tstrings;


    procedure parseLineNumbers(symbols: tstrings; stringsources: tstrings);
    procedure addLineInfo(functionaddress, address: ptruint; linenr: integer; sourceline: string; sourcefile: tstrings);
  public
    procedure outputDebugInfo(o: tstrings);
    procedure parseFullStabData;

    function getLineInfo(address: ptruint): PLineNumberInfo;
    function getVariableInfo(varname: string; currentaddress: ptruint; out varinfo: TLocalVariableInfo): boolean;

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

uses forms,dialogs, StrUtils {$ifndef standalonetest}, symbolhandler, ProcessHandlerUnit,
  newkernelhandler, CEFuncProc, sourcecodehandler{$endif};
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

procedure UpdateMinMax(address: ptruint; var minaddress: ptruint; var maxaddress: ptruint);
begin
  if minaddress=0 then
    minaddress:=address
  else
    if address<minaddress then
      minaddress:=address;

  if maxaddress=0 then
    maxaddress:=address
  else
    if address>maxaddress then
      maxaddress:=address;
end;

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
  i,j,k: integer;
  s: string;
begin
  o.addtext('Linenumbers');
  mi:=TMapIterator.Create(AddressToLineNumberInfo);
  mi.First;
  while not mi.EOM do
  begin
    mi.GetData(e);
    o.AddText(inttohex(e.address,8)+':'+inttostr(e.linenr)+' - '+e.sourcecode);
    mi.Next;
  end;

  mi.free;

  if fullyParsed then
  begin
   { o.add('Types:');
   for i:=0 to length(types)-1 do
      o.add(format('%d: %s  (%s)  %d',[types[i].typenr, types[i].name, types[i].full, types[i].desc]));}


    for i:=0 to length(parsedsource)-1 do
    with parsedsource[i] do
    begin
      s:=format('Sourcefile:%s (%.8x-%.8x)',[sourcefile, minaddress, maxaddress]);
      o.add(s);
      for j:=0 to length(functions)-1 do
      with functions[j] do
      begin
        o.add(format('  Function: %s (%.8x-%.8x)',[functionname, functionaddress, functionstop]));

        for k:=0 to length(parameters)-1 do
        with parameters[k] do
        begin
          if ispointer then
            o.add('  Parameter: %s:*%d (%s) %.2x',[varname, typenr, varstr, offset])
          else
            o.add('  Parameter: %s:%d (%s) %.2x',[varname, typenr, varstr, offset]);
        end;


        for k:=0 to length(lexblocks)-1 do
        with lexblocks[k] do
          o.add(format('  LexBlock: %.8x-%.8x %d',[startaddress, stopaddress, level]));

        for k:=0 to length(stackvars)-1 do
        with stackvars[k] do
        begin
          if ispointer then
            o.add(format('  StackVar: %s:*%d (%s) %.2x  (%.8x-%.8x)',[varname, typenr, varstr, offset, lexblocks[lexblock].startaddress, lexblocks[lexblock].stopaddress]))
          else
            o.add(format('  StackVar: %s:%d (%s) %.2x  (%.8x-%.8x)',[varname, typenr, varstr, offset, lexblocks[lexblock].startaddress, lexblocks[lexblock].stopaddress]));
        end;

      end;

    end;


  end

end;

procedure TSourceCodeInfo.parseFullStabData;
var
  i,j: integer;
  p: integer;
  count: integer;



  currentlevel: integer;

  parsedSourceIndex: integer;
  parsedFunctionIndex: integer;

  currentFunctionAddress: ptruint;
  currentSourceFile: string;

  str, varname: string;

  found: boolean;
  sa: TStringArray;

  ispointer: boolean;
begin
  if fullyParsed=false then
  begin
    fullyParsed:=true;
    count:=stabsize div (sizeof(TCCStabEntry)); //12

    parsedSourceIndex:=-1;
    parsedFunctionIndex:=-1;
    currentlevel:=0;

    for i:=0 to count-1 do
    begin
      case stab[i].n_type of
        $24: //function
        begin
          parsedFunctionIndex:=-1;

          if parsedSourceIndex=-1 then continue;
          if stab[i].n_strx>=stabstrsize then
            continue;

          str:=pchar(@stabstr[stab[i].n_strx]);
          if str='' then continue;

          if pos(':',str)>0 then
            str:=str.Split(':')[0]
          else
            str:='';


          for j:=0 to length(parsedsource[parsedSourceIndex].functions)-1 do
            if parsedSource[parsedSourceIndex].functions[j].functionname=str then
            begin
              currentFunctionAddress:=parsedSource[parsedSourceIndex].functions[j].functionaddress;
              parsedSource[parsedSourceIndex].functions[j].stackvars:=[];
              parsedFunctionIndex:=j;
              break;
            end;
        end;



        $80:
        begin
          //stack var
          if stab[i].n_strx>=stabstrsize then
            continue;

          str:=pchar(@stabstr[stab[i].n_strx]);

          if stab[i].n_value=0 then
          begin
            //typedef
            {
            if pos(':',str)>0 then
            begin
              sa:=str.split(':');
              typename:=sa[0];

              found:=false;
              for j:=0 to length(types)-1 do
              begin
                if types[j].name=typename then
                begin
                  found:=true;
                  break; //do not do duplicates
                end;
              end;

              if not found then
              begin
                j:=length(types);
                setlength(types,j+1);
                types[j].name:=typename;
                types[j].full:=str;
                types[j].extra:=sa[1];
                types[j].typenr:=0;


                //parse the type line
                if length(types[j].extra)>2 then
                begin
                  case types[j].extra[1] of
                    't':
                    begin
                      //followed by a number
                      str:='';
                      p:=2;
                      while types[j].extra[p] in ['0'..'9'] do
                      begin
                        str:=str+types[j].extra[p];
                        inc(p);
                      end;

                      if str<>'' then
                      begin
                        types[j].typenr:=strtoint(str);
                      end;
                    end;

                  end;
                end;

              end;


            end;}

          end
          else
          begin
            //var declaration
            if (parsedSourceIndex=-1) or (parsedFunctionIndex=-1) then continue;

            sa:=str.Split(':');
            varname:=sa[0];
            if varname='' then continue;

            with parsedSource[parsedSourceIndex].functions[parsedFunctionIndex] do
            begin
              j:=length(stackvars);
              setlength(stackvars, j+1);
              stackvars[j].varstr:=str;
              stackvars[j].varname:=varname;
              stackvars[j].offset:=stab[i].n_value;
              stackvars[j].lexblock:=length(lexblocks);
              stackvars[j].ispointer:=false;

              ispointer:=false;

              p:=1;
              while (sa[1][p] in ['0'..'9']=false) and (p<=length(sa[1])) do inc(p);

              if p<=length(sa[1]) then
              begin
                str:='';
                while (p<=length(sa[1])) and (sa[1][p] in ['0'..'9']) do
                begin
                  str:=str+sa[1][p];
                  inc(p);
                end;

                stackvars[j].typenr:=strtoint(str);
              end;

              //try to get the final type if there is one:

              p:=RPos('=',sa[1]);
              if p<>-1 then
              begin
                inc(p);
                if sa[1][p]='*' then
                begin
                  ispointer:=true;
                  inc(p)
                end;

                if sa[1][p] in ['0'..'9'] then
                begin
                  //=##
                  str:='';
                  while (p<=length(sa[1])) and (sa[1][p] in ['0'..'9']) do
                  begin
                    str:=str+sa[1][p];
                    inc(p);
                  end;
                  stackvars[j].typenr:=strtoint(str);
                  stackvars[j].ispointer:=ispointer;
                end;
              end;
            end;
          end;
        end;

        $84: //sub source
        begin
          parsedFunctionIndex:=-1;
          parsedSourceIndex:=-1;

          currentSourceFile:=pchar(@stabstr[stab[i].n_strx]);
          if trim(currentSourceFile)='' then continue;

          for j:=0 to length(parsedsource)-1 do
            if parsedsource[j].sourcefile=currentSourceFile then
            begin
              parsedSourceIndex:=j;
              break;
            end;

          if parsedSourceIndex=-1 then  //should never happen as the linenumber parse should have filled this in
          begin
            parsedSourceIndex:=length(parsedsource);
            setlength(parsedsource, parsedSourceIndex+1);

            parsedsource[parsedSourceIndex].sourcefile:=currentSourceFile;
            parsedsource[parsedSourceIndex].functions:=[];
          end;

        end;

        $a0: //function parameter
        begin
          if (parsedSourceIndex=-1) or (parsedFunctionIndex=-1) then continue;
          if stab[i].n_strx>=stabstrsize then
            continue;

          str:=pchar(@stabstr[stab[i].n_strx]);
          if pos(':',str)=0 then continue;

          sa:=str.Split(':');
          varname:=sa[0];
          if varname='' then continue;

          with parsedSource[parsedSourceIndex].functions[parsedFunctionIndex] do
          begin
            j:=length(parameters);
            setlength(parameters,j+1);
            parameters[j].offset:=stab[i].n_value;
            parameters[j].varstr:=str;
            parameters[j].varname:=varname;
            parameters[j].ispointer:=false;


            p:=1;
            while (sa[1][p] in ['0'..'9']=false) and (p<=length(sa[1])) do inc(p);

            if p<=length(sa[1]) then
            begin
              str:='';
              while (p<=length(sa[1])) and (sa[1][p] in ['0'..'9']) do
              begin
                str:=str+sa[1][p];
                inc(p);
              end;

              parameters[j].typenr:=strtoint(str);
            end;

            //try to get the final type if there is one:

            p:=RPos('=',sa[1]);
            if p<>-1 then
            begin
              inc(p);
              if sa[1][p]='*' then
              begin
                ispointer:=true;
                inc(p)
              end;

              if sa[1][p] in ['0'..'9'] then
              begin
                //=##
                str:='';
                while (p<=length(sa[1])) and (sa[1][p] in ['0'..'9']) do
                begin
                  str:=str+sa[1][p];
                  inc(p);
                end;
                parameters[j].typenr:=strtoint(str);
                parameters[j].ispointer:=ispointer;
              end;
            end;


          end;
        end;


        $c0:
        begin
          if (parsedSourceIndex=-1) or (parsedFunctionIndex=-1) then continue;

          //start of lex block
          with parsedSource[parsedSourceIndex].functions[parsedFunctionIndex] do
          begin
            j:=length(lexblocks);
            setlength(lexblocks, j+1);
            lexblocks[j].startaddress:=currentFunctionAddress+stab[i].n_value;
            lexblocks[j].level:=currentlevel;
            lexblocks[j].stopaddress:=0;
          end;

          inc(currentlevel);
        end;

        $e0:
        begin
          if (parsedSourceIndex=-1) or (parsedFunctionIndex=-1) then continue;
          //end of lex block
          dec(currentlevel);

          with parsedSource[parsedSourceIndex].functions[parsedFunctionIndex] do
          begin
            for j:=length(lexblocks)-1 downto 0 do
            begin
              if (lexblocks[j].stopaddress = 0)  and (lexblocks[j].level=currentlevel) then
              begin
                lexblocks[j].stopaddress := currentFunctionAddress+stab[i].n_value; ;
                break;
              end;
            end;
          end;


        end;

      end;

    end;

  end;

end;

procedure TSourceCodeInfo.parseLineNumbers(symbols: tstrings; stringsources: tstrings);
var
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


  parsedSourceIndex: integer;
  parsedFunctionIndex: integer;
begin
  parsedSourceIndex:=-1;
  parsedFunctionIndex:=-1;

  source:=nil;
  currentSourceFile:='';

  count:=stabsize div (sizeof(TCCStabEntry)); //12

  parsedsource:=[];

  for i:=0 to count-1 do
  begin
    case stab[i].n_type of
      $24:
      begin
        if parsedSourceIndex=-1 then continue; //broken stabs file

        parsedFunctionIndex:=-1;
        //symbol/function
        //setlength(stackvars,0);

        if stab[i].n_strx>=stabstrsize then
          continue;

        str:=pchar(@stabstr[stab[i].n_strx]);
        if pos(':',str)>0 then
          str:=str.Split(':')[0];

        si:=symbols.IndexOf(str);
        if si<>-1 then
        begin
          currentFunction.valid:=true;
          currentFunction.name:=str;
          currentFunction.address:=ptruint(symbols.Objects[si]);
        end;


        updateMinMax(currentFunction.address, minaddress, maxaddress);
        updateMinMax(currentFunction.address, parsedSource[parsedSourceIndex].minaddress, parsedSource[parsedSourceIndex].maxaddress);



        if currentFunction.valid then
        begin
          parsedFunctionIndex:=length(parsedsource[parsedSourceIndex].functions);
          setlength(parsedsource[parsedSourceIndex].functions, parsedFunctionIndex+1);

          parsedSource[parsedSourceIndex].functions[parsedFunctionIndex].functionname:=currentfunction.name;
          parsedSource[parsedSourceIndex].functions[parsedFunctionIndex].functionaddress:=currentFunction.address;
          parsedSource[parsedSourceIndex].functions[parsedFunctionIndex].functionstop:=0;
          parsedSource[parsedSourceIndex].functions[parsedFunctionIndex].lexblocks:=[];
          parsedSource[parsedSourceIndex].functions[parsedFunctionIndex].stackvars:=[];
        end;
      end;

      $44: //Line number info
      begin
        if (parsedSourceIndex=-1) or (parsedFunctionIndex=-1) then continue;

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

              addLineInfo(currentFunction.address, address, ln, sl.text, source);


              updateMinMax(address, minaddress, maxaddress);
              updateMinMax(address, parsedSource[parsedSourceIndex].minaddress, parsedSource[parsedSourceIndex].maxaddress);
            end;
          end;


        end;
      end;



      $84: //sub source file
      begin
        parsedFunctionIndex:=-1;
        parsedSourceIndex:=-1;

        currentFunction.valid:=false;
        if stab[i].n_strx>=stabstrsize then
        begin
          source:=nil;
          currentSourceFile:='';
          continue;
        end;

        currentSourceFile:=pchar(@stabstr[stab[i].n_strx]);
        if trim(currentSourceFile)='' then continue;

        for j:=0 to length(parsedsource)-1 do
          if parsedsource[j].sourcefile=currentSourceFile then
          begin
            parsedSourceIndex:=j;
            break;
          end;

        if parsedSourceIndex=-1 then
        begin
          parsedSourceIndex:=length(parsedsource);
          setlength(parsedsource, parsedSourceIndex+1);

          parsedsource[parsedSourceIndex].sourcefile:=currentSourceFile;
          parsedsource[parsedSourceIndex].functions:=[];
          parsedsource[parsedSourceIndex].minaddress:=0;
          parsedsource[parsedSourceIndex].maxaddress:=0;
        end;


        if trim(currentSourceFile)='' then continue;

        source:=GetSource(currentSourceFile);
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

          AddSource(currentSourceFile, source);
        end;
      end;

      $e0: //end of lex block (last address)
      begin
        if currentFunction.valid then
        begin
          address:=currentFunction.address+stab[i].n_value;
          UpdateMinMax(address, minaddress, maxaddress);
          if parsedSourceIndex<>-1 then
          begin
            UpdateMinMax(address, parsedSource[parsedSourceIndex].minaddress, parsedSource[parsedSourceIndex].maxaddress);
            if parsedFunctionIndex<>-1 then
              UpdateMinMax(address, address, parsedSource[parsedSourceIndex].functions[parsedFunctionIndex].functionstop);
          end;
        end;

      end;

    end;
  end;
end;

procedure TSourceCodeInfo.addLineInfo(functionaddress, address: ptruint; linenr: integer; sourceline: string; sourcefile: tstrings);
var e: TLineNumberInfo;
begin
  e.functionAddress:=functionaddress;
  e.address:=address;
  e.linenr:=linenr;
  e.sourcecode:=strnew(pchar(sourceline));
  e.sourcefile:=sourcefile;
  AddressToLineNumberInfo.Add(address, e);




end;

function TSourceCodeInfo.getLineInfo(address: ptruint): PLineNumberInfo;
begin
  result:=AddressToLineNumberInfo.GetDataPtr(address);
end;


function TSourceCodeInfo.getVariableInfo(varname: string; currentaddress: ptruint; out varinfo: TLocalVariableInfo): boolean;
var i,j,k: integer;
begin
  result:=false;
  //find the function this address belongs to

  //first find the sourcefile
  for i:=0 to length(parsedsource)-1 do
    if (currentaddress>=parsedsource[i].minaddress) and (currentaddress<=parsedsource[i].maxaddress) then
    begin
      //found the sourcefile for this address
      with parsedsource[i] do
      begin
        for j:=0 to length(functions)-1 do
        begin
          if (currentaddress>=functions[j].functionaddress) and (currentaddress<=functions[j].functionstop) then
          begin
            with functions[j] do
            begin
              for k:=0 to length(stackvars)-1 do
              begin
                if (currentaddress>=lexblocks[stackvars[k].lexblock].startaddress) and (currentaddress<=lexblocks[stackvars[k].lexblock].stopaddress) and (stackvars[k].varname=varname) then
                begin
                  varinfo.name:=stackvars[k].varname;
                  varinfo.offset:=stackvars[k].offset;
                  varinfo.vartype:=stackvars[k].typenr;
                  varinfo.ispointer:=stackvars[k].ispointer;
                  exit(true);
                end;
              end;

              for k:=0 to length(parameters)-1 do
              begin
                if (parameters[k].varname=varname) then
                begin
                  varinfo.name:=parameters[k].varname;
                  varinfo.offset:=parameters[k].offset;
                  varinfo.vartype:=parameters[k].typenr;
                  varinfo.ispointer:=parameters[k].ispointer;
                  exit(true);
                end;
              end;
            end;

            break;
          end;
        end;
      end;
      break;
    end;
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

  if stabdata<>nil then
    freemem(stabdata);
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
    sl.AddObject(name, tobject(ptruint(address)));

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
var
  stabdatasize: integer;
  stabdata: pointer;
  stabsize: integer;
  stabstrsize: integer;

  stab: PCCStabEntry;
  stabstr: pchar;

begin
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

        sourcecodeinfo.stabdata:=stabdata;
        sourcecodeinfo.stab:=stab;
        sourcecodeinfo.stabstr:=stabstr;
        sourcecodeinfo.stabsize:=stabsize;
        sourcecodeinfo.stabstrsize:=stabstrsize;

        //quickly parse the lines
        sourcecodeinfo.parseLineNumbers(symbols, stringsources);



      end;
    finally
      if sourcecodeinfo.stabdata<>stabdata then
        freemem(stabdata); //else it's owned by sourcecodeinfo
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

