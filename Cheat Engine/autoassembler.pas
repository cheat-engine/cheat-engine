unit autoassembler;

{$MODE Delphi}

interface

uses jwawindows, windows, Assemblerunit, classes,
LCLIntf,symbolhandler,sysutils,dialogs,controls, CEFuncProc, NewKernelHandler ,plugin;



function getenableanddisablepos(code:tstrings;var enablepos,disablepos: integer): boolean;
function autoassemble(code: tstrings;popupmessages: boolean):boolean; overload;
function autoassemble(code: Tstrings; popupmessages,enable,syntaxcheckonly, targetself: boolean;var CEAllocarray: TCEAllocArray; registeredsymbols: tstringlist=nil): boolean; overload;

implementation

uses simpleaobscanner, StrUtils, LuaHandler;

resourcestring
  rsForwardJumpWithNoLabelDefined = 'Forward jump with no label defined';
  rsThereIsCodeDefinedWithoutSpecifyingTheAddressItBel = 'There is code defined without specifying the address it belongs to';
  rsIsNotAValidBytestring = '%s is not a valid bytestring';
  rsTheBytesAtAreNotWhatWasExpected = 'The bytes at %s are not what was expected';
  rsTheMemoryAtCanNotBeRead = 'The memory at +%s can not be read';
  rsWrongSyntaxASSERTAddress1122335566 = 'Wrong syntax. ASSERT(address,11 22 33 ** 55 66)';
  rsIsNotAValidSize = '%s is not a valid size';
  rsWrongSyntaxGLOBALALLOCNameSize = 'Wrong syntax. GLOBALALLOC(name,size)';
  rsCouldNotBeFound = '%s could not be found';
  rsWrongSyntaxIncludeFilenameCea = 'Wrong syntax. Include(filename.cea)';
  rsWrongSyntaxCreateThreadAddress = 'Wrong syntax. CreateThread(address)';
  rsCouldNotBeInjected = '%s could not be injected';
  rsWrongSyntaxLoadLibraryFilename = 'Wrong syntax. LoadLibrary(filename)';
  rsWrongSyntaxLuaCall = 'Wrong Syntax. LuaCall(luacommand)';
  rsInvalidAddressForReadMem = 'Invalid address for ReadMem';
  rsInvalidSizeForReadMem = 'Invalid size for ReadMem';
  rsTheMemoryAtCouldNotBeFullyRead = 'The memory at %s could not be fully read';
  rsWrongSyntaxReadMemAddressSize = 'Wrong syntax. ReadMem(address,size)';
  rsTheFileDoesNotExist = 'The file %s does not exist';
  rsWrongSyntaxLoadBinaryAddressFilename = 'Wrong syntax. LoadBinary(address,filename)';
  rsSyntaxError = 'Syntax error';
  rsTheArrayOfByteCouldNotBeFound = 'The array of byte ''%s'' could not be found';
  rsWrongSyntaxAOBSCANName11223355 = 'Wrong syntax. AOBSCAN(name,11 22 33 ** 55)';
  rsDefineAlreadyDefined = 'Define %s already defined';
  rsWrongSyntaxDEFINENameWhatever = 'Wrong syntax. DEFINE(name,whatever)';
  rsSyntaxErrorFullAccessAddressSize = 'Syntax error. FullAccess(address,size)';
  rsIsNotAValidIdentifier = '%s is not a valid identifier';
  rsIsBeingRedeclared = '%s is being redeclared';
  rsLabelIsBeingDefinedMoreThanOnce = 'label %s is being defined more than once';
  rsLabelIsNotDefinedInTheScript = 'label %s is not defined in the script';
  rsTheIdentifierHasAlreadyBeenDeclared = 'The identifier %s has already been declared';
  rsWrongSyntaxALLOCIdentifierSizeinbytes = 'Wrong syntax. ALLOC(identifier,sizeinbytes)';
  rsNeedToUseKernelmodeReadWriteprocessmemory = 'You need to use kernelmode read/writeprocessmemory if you want to use KALLOC';
  rsSorryButWithoutTheDriverKALLOCWillNotFunction = 'Sorry, but without the driver KALLOC will not function';
  rsWrongSyntaxKallocIdentifierSizeinbytes = 'Wrong syntax. kalloc(identifier,sizeinbytes)';
  rsThisAddressSpecifierIsNotValid = 'This address specifier is not valid';
  rsThisInstructionCanTBeCompiled = 'This instruction can''t be compiled';
  rsErrorInLine = 'Error in line %s (%s) :%s';
  rsWasSupposedToBeAddedToTheSymbollistButItIsnTDeclar = '%s was supposed to be added to the symbollist, but it isn''t declared';
  rsTheAddressInCreatethreadIsNotValid = 'The address in createthread(%s) is not valid';
  rsTheAddressInLoadbinaryIsNotValid = 'The address in loadbinary(%s,%s) is not valid';
  rsThisCodeCanBeInjectedAreYouSure = 'This code can be injected. Are you sure?';
  rsFailureToAllocateMemory = 'Failure to allocate memory';
  rsNotAllInstructionsCouldBeInjected = 'Not all instructions could be injected';
  rsTheFollowingKernelAddressesWhereAllocated = 'The following kernel addresses where allocated';
  rsTheCodeInjectionWasSuccessfull = 'The code injection was successfull';
  rsYouCanOnlyHaveOneEnableSection = 'You can only have one enable section';
  rsYouCanOnlyHaveOneDisableSection = 'You can only have one disable section';
  rsYouHavnTSpecifiedAEnableSection = 'You havn''t specified a enable section';
  rsYouHavnTSpecifiedADisableSection = 'You havn''t specified a disable section';
  rsWrongSyntaxSHAREDALLOCNameSize = 'Wrong syntax. SHAREDALLOC(name,size)';

procedure tokenize(input: string; tokens: tstringlist);
var i: integer;
    a: integer;
begin

  tokens.clear;
  a:=-1;
  for i:=1 to length(input) do
  begin
    case input[i] of
      'a'..'z','A'..'Z','0'..'9','_','#','@': if a=-1 then a:=i;
      else
      begin
        if a<>-1 then
          tokens.AddObject(copy(input,a,i-a),tobject(a));
        a:=-1;
      end;
    end;
  end;

  if a<>-1 then
    tokens.AddObject(copy(input,a,length(input)),tobject(a));
end;

function tokencheck(input,token:string):boolean;
var tokens: tstringlist;
    i: integer;
begin
  tokens:=tstringlist.Create;
  try
    tokenize(input,tokens);
    result:=false;

    for i:=0 to tokens.Count-1 do
      if tokens[i]=token then
      begin
        result:=true;
        break;
      end;
  finally
    tokens.free;
  end;
end;

function replacetoken(input: string;token:string;replacewith:string):string;
var tokens: tstringlist;
    i,j: integer;
begin
  result:=input;
  tokens:=tstringlist.Create;
  try
    tokenize(input,tokens);
    for i:=0 to tokens.Count-1 do
      if tokens[i]=token then
      begin
        j:=integer(tokens.Objects[i]);
        result:=copy(input,1,j-1)+replacewith+copy(input,j+length(token),length(input));
      end;

  finally
    tokens.free;
  end;
end;


procedure removecomments(code: tstrings);
var i,j: integer;
    currentline: string;
    instring: boolean;
    incomment: boolean;
begin
  //remove comments
  instring:=false;
  incomment:=false;
  for i:=0 to code.count-1 do
  begin
    currentline:=code[i];
    
    for j:=1 to length(currentline) do
    begin
      if incomment then
      begin
        //inside a comment, remove everything till a } is encountered
        if (currentline[j]='}') or
           ((currentline[j]='*') and (j<length(currentline)) and (currentline[j+1]='/')) then
        begin
          incomment:=false; //and continue parsing the code...

          if ((currentline[j]='*') and (j<length(currentline)) and (currentline[j+1]='/')) then
            currentline[j+1]:=' ';
        end;

        currentline[j]:=' ';
      end
      else
      begin
        if currentline[j]='''' then instring:=not instring;
        if currentline[j]=#9 then currentline[j]:=' '; //tabs are basicly comments 

        if not instring then
        begin
          //not inside a string, so comment markers need to be dealt with
          if (currentline[j]='/') and (j<length(currentline)) and (currentline[j+1]='/') then //- comment (only the rest of the line)
          begin
            //cut off till the end of the line (and might as well jump out now)
            currentline:=copy(currentline,1,j-1);
            break;
          end;

          if (currentline[j]='{') or
             ((currentline[j]='/') and (j<length(currentline)) and (currentline[j+1]='*')) then
          begin
            incomment:=true;
            currentline[j]:=' '; //replace from here till the first } with spaces, this goes on for multiple lines
          end;
        end;
      end;
    end;

    code[i]:=trim(currentline);
  end;

end;


procedure unlabeledlabels(code: tstrings);
var i,j: integer;
    lastseenlabel: integer;
    labels: array of string; //sorted in order of definition
    currentline: string;

begin
  //unlabeled label support
  //For those reading the source, PLEASE , try not to code scripts like that
  //the scripts you make look like crap, and are hard to read. (like using goto in a c app)
  //this is just to make one guy happy

  setlength(labels,0);
  i:=0;
  while i<code.count do
  begin
    currentline:=code[i];

    if length(currentline)>1 then
    begin
      if currentline='@@:' then
      begin
        currentline:='RandomLabel'+chr(ord('A')+random(26))+
                 chr(ord('A')+random(26))+
                 chr(ord('A')+random(26))+
                 chr(ord('A')+random(26))+
                 chr(ord('A')+random(26))+
                 chr(ord('A')+random(26))+
                 chr(ord('A')+random(26))+
                 chr(ord('A')+random(26))+
                 chr(ord('A')+random(26))+
                 ':';
        code[i]:=currentline;

        code.Insert(0,'label('+copy(currentline,1,length(currentline)-1)+')');
        inc(i);

        setlength(labels,length(labels)+1);
        labels[length(labels)-1]:=copy(currentline,1,length(currentline)-1);
      end else
      if currentline[length(currentline)]=':' then
      begin
        setlength(labels,length(labels)+1);
        labels[length(labels)-1]:=copy(currentline,1,length(currentline)-1);
      end;
    end;

    inc(i);
  end;

  //all label definitions have been filled in
  //now change @F (forward) and @B (back) to the labels in front and behind
  lastseenlabel:=-1;
  for i:=0 to code.Count-1 do
  begin
    currentline:=code[i];
    if length(currentline)>1 then
    begin
      if currentline[length(currentline)]=':' then
      begin
        //find this in the array
        currentline:=copy(currentline,1,length(currentline)-1);
        for j:=(lastseenlabel+1) to length(labels)-1 do  //lastseenlabel+1 since it is ordered in definition
        begin
          if uppercase(currentline)=uppercase(labels[j]) then
          begin
            lastseenlabel:=j;
            break;
          end;
        end;
        currentline:=currentline+':';
        //lastseenlabel is now updated to the current pos
      end else
      if pos('@f',lowercase(currentline))>0 then  //forward
      begin
        //forward label, so labels[lastseenlabel+1]
        if lastseenlabel+1 >= length(labels) then
          raise exception.Create(rsForwardJumpWithNoLabelDefined);

        currentline:=replacetoken(currentline,'@f',labels[lastseenlabel+1]);
        currentline:=replacetoken(currentline,'@F',labels[lastseenlabel+1]);
      end else
      if pos('@b',lowercase(currentline))>0 then //back
      begin
        //forward label, so labels[lastseenlabel]
        if lastseenlabel=-1 then
          raise exception.Create(rsThereIsCodeDefinedWithoutSpecifyingTheAddressItBel);

        currentline:=replacetoken(currentline,'@b',labels[lastseenlabel]);
        currentline:=replacetoken(currentline,'@B',labels[lastseenlabel]);
      end;
    end;
    code[i]:=currentline;

  end;
end;


function autoassemble2(code: tstrings;popupmessages: boolean;syntaxcheckonly:boolean; targetself: boolean ;var ceallocarray:TCEAllocArray; registeredsymbols: tstringlist=nil):boolean;
{
registeredsymbols is a stringlist that is initialized by the caller as case insensitive and no duplicates
}
type tassembled=record
  address: ptrUint;
  bytes: TAssemblerbytes;
end;


type tlabel=record
  defined: boolean;
  insideAllocatedMemory: boolean;
  address:ptrUint;
  labelname: string;
  assemblerline: integer;
  references: array of integer; //index of assembled array
  references2: array of integer; //index of assemblerlines array
end;
type tfullaccess=record
  address: ptrUint;
  size: dword;
end;
type tdefine=record
  name: string;
  whatever: string;
end;
var i,j,k,l,e: integer;
    currentline: string;
    currentlinenr: integer;
    currentlinep: pchar;

    currentaddress: ptrUint;
    assembled: array of tassembled;
    x,y,op,op2:dword;
    ok1,ok2:boolean;
    loadbinary: array of record
      address: string; //string since it might be a label/alloc/define
      filename: string;
    end;


    globalallocs, allocs, kallocs, sallocs: array of tcealloc;
    labels: array of tlabel;
    defines: array of tdefine;
    fullaccess: array of tfullaccess;
    dealloc: array of PtrUInt;
    addsymbollist: array of string;
    deletesymbollist: array of string;
    createthread: array of string;

    a,b,c,d: integer;
    s1,s2,s3: string;

    assemblerlines: array of string;

    varsize: integer;
    tokens: tstringlist;
    baseaddress: ptrUint;

    include: tstringlist;
    testdword,bw: dword;
    testPtr: ptrUint;
    binaryfile: tmemorystream;

    incomment: boolean;

    bytebuf: PByteArray;

    processhandle: THandle;
    ProcessID: DWORD;

    bytes: tbytes;

    symhandler: TSymhandler;
    prefered: ptrUint;

    oldhandle: thandle;
begin
  setlength(allocs,0);
  setlength(kallocs,0);
  setlength(globalallocs,0);
  setlength(sallocs,0);
  setlength(createthread,0);

  currentaddress:=0;


  if syntaxcheckonly and (registeredsymbols<>nil) then
  begin
    //add the symbols as defined labels
    setlength(labels,registeredsymbols.count);
    for i:=0 to registeredsymbols.count-1 do
    begin
      labels[i].labelname:=registeredsymbols[i];
      labels[i].defined:=true;
      labels[i].address:=0;
      labels[i].assemblerline:=0;
      setlength(labels[i].references,0);
      setlength(labels[i].references2,0);
    end;
  end;

  if targetself then
  begin
    //get this function to use the symbolhandler that's pointing to CE itself and the self processid/handle
    oldhandle:=cefuncproc.ProcessHandle;
    processhandle:=getcurrentprocess;
    processid:=getcurrentprocessid;
    symhandler:=symbolhandler.selfsymhandler;
    processhandler.processhandle:=processhandle;
  end
  else
  begin
    processhandle:=cefuncproc.ProcessHandle;
    processid:=cefuncproc.ProcessID;
    symhandler:=symbolhandler.symhandler;
  end;

  symhandler.waitforsymbolsloaded;

{$ifndef standalonetrainer}
  pluginhandler.handleAutoAssemblerPlugin(@currentlinep, 0); //tell the plugins that an autoassembler script is about to get executed
{$endif}

//2 pass scanner
  try
    setlength(assembled,1);
    setlength(kallocs,0);
    setlength(allocs,0);
    setlength(dealloc,0);
    setlength(assemblerlines,0);
    setlength(fullaccess,0);
    setlength(addsymbollist,0);
    setlength(deletesymbollist,0);
    setlength(defines,0);
    setlength(loadbinary,0);

    tokens:=tstringlist.Create;

    incomment:=false;



    removecomments(code);
    unlabeledlabels(code);


    //first pass

    i:=0;
    while i<code.Count do
    begin
      try
        try
          currentline:=code[i];
          currentlinenr:=ptrUint(code.Objects[i]);

          currentline:=trim(currentline);

          for j:=0 to length(defines)-1 do
            currentline:=replacetoken(currentline,defines[j].name,defines[j].whatever);

          if length(currentline)=0 then continue;
          if copy(currentline,1,2)='//' then continue; //skip

          setlength(assemblerlines,length(assemblerlines)+1);
          assemblerlines[length(assemblerlines)-1]:=currentline;

          //plugins
          {$ifndef standalonetrainer}
          currentlinep:=@currentline[1];
          pluginhandler.handleAutoAssemblerPlugin(@currentlinep, 1);
          currentline:=currentlinep;
          {$endif}

          //if the newline is empty then it has been handled and the plugin doesn't want it to be added for phase2
          if length(currentline)=0 then
          begin
            setlength(assemblerlines,length(assemblerlines)-1);
            continue;
          end;
          //otherwhise it hasn't been handled, or it has been handled and the string is a compatible string that passes the phase1 tests (so variablenames converted to 00000000 and whatever else is needed)
          //plugins^^^

          if uppercase(copy(currentline,1,7))='ASSERT(' then //assert(address,aob)
          begin
            if not syntaxcheckonly then
            begin
              a:=pos('(',currentline);
              b:=pos(',',currentline);
              c:=pos(')',currentline);
              if (a>0) and (b>0) and (c>0) then
              begin
                s1:=trim(copy(currentline,a+1,b-a-1));
                s2:=trim(copy(currentline,b+1,c-b-1));

                testPtr:= symhandler.getAddressFromName(s1,false);

                setlength(bytes,0);
                try
                  ConvertStringToBytes(s2, true, bytes);
                except
                  raise exception.create(Format(rsIsNotAValidBytestring, [s2]));
                end;

                if length(bytes)>0 then
                begin
                  getmem(bytebuf,length(bytes));
                  try
                    if ReadProcessMemory(processhandle, pointer(testPtr), bytebuf, length(bytes),x) then
                    begin

                        for j:=0 to length(bytes)-1 do
                        begin
                          if bytes[j]>=0 then
                            if byte(bytes[j])<>bytebuf[j] then
                              raise exception.Create(Format(rsTheBytesAtAreNotWhatWasExpected, [s1]));
                        end;
                    end else raise exception.Create(Format(rsTheMemoryAtCanNotBeRead, [s1]));
                  finally
                    freemem(bytebuf);
                  end;

                end
                else raise exception.Create(Format(rsIsNotAValidBytestring, [s2]));

              end
              else
                raise exception.Create(rsWrongSyntaxASSERTAddress1122335566);
            end;

            setlength(assemblerlines,length(assemblerlines)-1);
            continue;
          end;
              {
          if uppercase(copy(currentline,1,12))='SHAREDALLOC(' then
          begin
            a:=pos('(',currentline);
            b:=pos(',',currentline);
            c:=pos(')',currentline);
            if (a>0) and (b>0) and (c>0) then
            begin
              s1:=trim(copy(currentline,a+1,b-a-1));
              s2:=trim(copy(currentline,b+1,c-b-1));

              try
                x:=strtoint(s2);
              except
                raise exception.Create(Format(rsIsNotAValidSize, [s2]));
              end;

              setlength(sallocs,length(sallocs)+1);
              sallocs[length(sallocs)-1].address:=allocateSharedMemoryIntoTargetProcess(s1,x);
              sallocs[length(sallocs)-1].varname:=s1;
              sallocs[length(sallocs)-1].size:=x;

              setlength(assemblerlines,length(assemblerlines)-1);
              continue;

            end
            else raise exception.Create(rsWrongSyntaxSHAREDALLOCNameSize);
          end;  }

          if uppercase(copy(currentline,1,12))='GLOBALALLOC(' then
          begin
            a:=pos('(',currentline);
            b:=pos(',',currentline);
            c:=pos(')',currentline);
            if (a>0) and (b>0) and (c>0) then
            begin
              s1:=trim(copy(currentline,a+1,b-a-1));
              s2:=trim(copy(currentline,b+1,c-b-1));

              try
                x:=strtoint(s2);
              except
                raise exception.Create(Format(rsIsNotAValidSize, [s2]));
              end;

              //define it here already
              symhandler.SetUserdefinedSymbolAllocSize(s1,x);              

              setlength(globalallocs,length(globalallocs)+1);
              globalallocs[length(globalallocs)-1].address:=symhandler.GetUserdefinedSymbolByName(s1);
              globalallocs[length(globalallocs)-1].varname:=s1;
              globalallocs[length(globalallocs)-1].size:=x;

              setlength(assemblerlines,length(assemblerlines)-1);
              continue;

            end
            else raise exception.Create(rsWrongSyntaxGLOBALALLOCNameSize);
          end;

          if uppercase(copy(currentline,1,8))='INCLUDE(' then
          begin
            a:=pos('(',currentline);
            b:=pos(')',currentline);

            if (a>0) and (b>0) then
            begin
              s1:=trim(copy(currentline,a+1,b-a-1));

              if ExtractFileExt(uppercase(s1))='.' then
                s1:=s1+'CEA';

              if ExtractFileExt(uppercase(s1))='' then
                s1:=s1+'.CEA';

              if not fileexists(s1) then //check if it's inside the current location
              begin
                //if not, check the default paths
                s2:=cheatenginedir+'includes'+pathdelim+'s1';
                if fileexists(s2) then s1:=s2 else
                begin
                  s2:=cheatenginedir+s1;
                  if fileexists(s2) then s1:=s2
                  else
                  begin
                    s2:=tablesdir+s1;
                    if fileexists(s2) then s1:=s2;
                  end;
                end;

                if not fileexists(s1) then
                  raise exception.Create(Format(rsCouldNotBeFound, [s1]));
              end;







              include:=tstringlist.Create;
              try
                include.LoadFromFile(s1);
                removecomments(include);
                unlabeledlabels(include);

                for j:=i+1 to (i+1)+(include.Count-1) do
                  code.Insert(j,include[j-(i+1)]);
              finally
                include.Free;
              end;

              setlength(assemblerlines,length(assemblerlines)-1);
              continue;
            end
            else raise exception.Create(rsWrongSyntaxIncludeFilenameCea);
          end;

          if uppercase(copy(currentline,1,13))='CREATETHREAD(' then
          begin
            //load a binary file into memory , this one already executes BEFORE the 2nd pass to get addressnames correct
            a:=pos('(',currentline);
            b:=pos(')',currentline);
            if (a>0) and (b>0) then
            begin
              s1:=trim(copy(currentline,a+1,b-a-1));
            
              setlength(createthread,length(createthread)+1);
              createthread[length(createthread)-1]:=s1;

              setlength(assemblerlines,length(assemblerlines)-1);
              continue;
            end else raise exception.Create(rsWrongSyntaxCreateThreadAddress);
          end;

          if uppercase(copy(currentline,1,12))='LOADLIBRARY(' then
          begin
            //load a library into memory , this one already executes BEFORE the 2nd pass to get addressnames correct
            a:=pos('(',currentline);
            b:=pos(')',currentline);

            if (a>0) and (b>0) then
            begin
              s1:=trim(copy(currentline,a+1,b-a-1));

              if pos(':',s1)=0 then
              begin
                s2:=extractfilename(s1);

                if fileexists(cheatenginedir+s2) then s1:=cheatenginedir+s2 else
                  if fileexists(getcurrentdir+'\'+s2) then s1:=getcurrentdir+'\'+s2 else
                    if fileexists(cheatenginedir+s1) then s1:=cheatenginedir+s1;

                //else just hope it's in the dll searchpath
              end; //else direct file path

              try
                InjectDll(s1,'');
                symhandler.reinitialize;
                symhandler.waitforsymbolsloaded;
              except
                raise exception.create(Format(rsCouldNotBeInjected, [s1]));
              end;

              setlength(assemblerlines,length(assemblerlines)-1);
              continue;
            end else raise exception.Create(rsWrongSyntaxLoadLibraryFilename);
          end;

          if uppercase(copy(currentline,1,8))='LUACALL(' then
          begin
            //execute a given lua command
            a:=pos('(',currentline);
            b:=length(currentline);

            if currentline[b]<>')' then b:=-1;

            if (a>0) and (b>0) then
            begin
              s1:=trim(copy(currentline,a+1,b-a-1));

              LUA_DoScript(s1); //raises an exception on error, which is exactly what we want here

              setlength(assemblerlines,length(assemblerlines)-1);
              continue;
            end else raise exception.Create(rsWrongSyntaxLuaCall);
          end;

          if uppercase(copy(currentline,1,8))='READMEM(' then
          begin
            //read memory and place it here (readmem(address,size) )
            a:=pos('(',currentline);
            b:=pos(',',currentline);
            c:=pos(')',currentline);
            if (a>0) and (b>0) and (c>0) then
            begin
              s1:=trim(copy(currentline,a+1,b-a-1));
              s2:=trim(copy(currentline,b+1,c-b-1));

              //read memory and replace with lines of DB xx xx xx xx xx xx xx xx
              try
                testptr:=symhandler.getAddressFromName(s1);
              except
                raise exception.Create(rsInvalidAddressForReadMem);
              end;

              try
                a:=strtoint(s2);
              except
                raise exception.Create(rsInvalidSizeForReadMem);
              end;

              if a=0 then
                raise exception.Create(rsInvalidSizeForReadMem);


              getmem(bytebuf,a);
              try
                if (not ReadProcessMemory(processhandle, pointer(testptr),bytebuf,a,x)) or (x<a) then
                  raise exception.Create(Format(rsTheMemoryAtCouldNotBeFullyRead, [s1]));

                //still here so everything ok
                setlength(assemblerlines,length(assemblerlines)-1);

                s1:='db';

                for j:=0 to a-1 do
                begin
                  s1:=s1+' '+inttohex(bytebuf[j],2);
                  if (j mod 16)=15 then
                  begin
                    setlength(assemblerlines,length(assemblerlines)+1);
                    assemblerlines[length(assemblerlines)-1]:=s1;

                    s1:='db';
                  end;
                end;

                if length(s1)>2 then
                begin
                  setlength(assemblerlines,length(assemblerlines)+1);
                  assemblerlines[length(assemblerlines)-1]:=s1;
                end;

              finally
                freemem(bytebuf);
              end;



            end else raise exception.Create(rsWrongSyntaxReadMemAddressSize);

            continue;
          end;

          if uppercase(copy(currentline,1,11))='LOADBINARY(' then
          begin
            //load a binary file into memory
            a:=pos('(',currentline);
            b:=pos(',',currentline);
            c:=pos(')',currentline);
            if (a>0) and (b>0) and (c>0) then
            begin
              s1:=trim(copy(currentline,a+1,b-a-1));
              s2:=trim(copy(currentline,b+1,c-b-1));

              if not fileexists(s2) then raise exception.Create(Format(rsTheFileDoesNotExist, [s2]));

              setlength(loadbinary,length(loadbinary)+1);
              loadbinary[length(loadbinary)-1].address:=s1;
              loadbinary[length(loadbinary)-1].filename:=s2;

              setlength(assemblerlines,length(assemblerlines)-1);
              continue;
            end else raise exception.Create(rsWrongSyntaxLoadBinaryAddressFilename);
          end;

          if uppercase(copy(currentline,1,15))='REGISTERSYMBOL(' then
          begin
            //add this symbol to the register symbollist
            a:=pos('(',currentline);
            b:=pos(')',currentline);

            if (a>0) and (b>0) then
            begin
              s1:=trim(copy(currentline,a+1,b-a-1));

              setlength(addsymbollist,length(addsymbollist)+1);
              addsymbollist[length(addsymbollist)-1]:=s1;

              if registeredsymbols<>nil then
                registeredsymbols.Add(s1);
            end
            else raise exception.Create(rsSyntaxError);

            setlength(assemblerlines,length(assemblerlines)-1);
            continue;
          end;

          if uppercase(copy(currentline,1,17))='UNREGISTERSYMBOL(' then
          begin
            //add this symbol to the register symbollist
            a:=pos('(',currentline);
            b:=pos(')',currentline);

            if (a>0) and (b>0) then
            begin
              s1:=trim(copy(currentline,a+1,b-a-1));

              setlength(deletesymbollist,length(deletesymbollist)+1);
              deletesymbollist[length(deletesymbollist)-1]:=s1;
            end
            else raise exception.Create(rsSyntaxError);

            setlength(assemblerlines,length(assemblerlines)-1);
            continue;
          end;

          //AOBSCAN(variable,aobstring)  (works like define)

          if uppercase(copy(currentline,1,8))='AOBSCAN(' then
          begin
            //convert this line from AOBSCAN(varname,bytestring) to DEFINE(varname,address)
            a:=pos('(',currentline);
            b:=pos(',',currentline);
            c:=pos(')',currentline);

            if (a>0) and (b>0) and (c>0) then
            begin
              s1:=trim(copy(currentline,a+1,b-a-1));
              s2:=trim(copy(currentline,b+1,c-b-1));


              //s1=varname
              //s2=AOBstring
              testPtr:=0;
              if (not syntaxcheckonly) then
              begin
                testPtr:=findaob(s2);
                if (testPtr=0) then
                  raise exception.Create(Format(rsTheArrayOfByteCouldNotBeFound, [s2]));
              end;

              //currentline:='DEFINE('+s1+','+inttohex(testPtr,8)+')';
              l:=length(labels);
              setlength(labels, l+1);
              labels[l].labelname:=s1;
              labels[l].address:=testPtr;
              labels[l].defined:=true;
              labels[l].insideAllocatedMemory:=false;

              setlength(assemblerlines,length(assemblerlines)-1);
              setlength(labels[l].references,0);
              setlength(labels[l].references2,0);

              continue;
            end else raise exception.Create(rsWrongSyntaxAOBSCANName11223355);
          end;

          //define
          if uppercase(copy(currentline,1,7))='DEFINE(' then
          begin
            //syntax: alloc(x,size)    x=variable name size=bytes
            //allocate memory
            a:=pos('(',currentline);
            b:=pos(',',currentline);
            c:=pos(')',currentline);
            if (a>0) and (b>0) and (c>0) then
            begin
              s1:=trim(copy(currentline,a+1,b-a-1));
              s2:=copy(currentline,b+1,c-b-1);

              for j:=0 to length(defines)-1 do
                if uppercase(defines[j].name)=uppercase(s1) then
                  raise exception.Create(Format(rsDefineAlreadyDefined, [s1]));

              setlength(defines,length(defines)+1);
              defines[length(defines)-1].name:=s1;
              defines[length(defines)-1].whatever:=s2;

              setlength(assemblerlines,length(assemblerlines)-1);   //don't bother with this in the 2nd pass
              continue;
            end else raise exception.Create(rsWrongSyntaxDEFINENameWhatever);
          end;

          if uppercase(copy(currentline,1,11))='FULLACCESS(' then
          begin
            a:=pos('(',currentline);
            b:=pos(',',currentline);
            c:=pos(')',currentline);

            if (a>0) and (b>0) and (c>0) then
            begin
              s1:=trim(copy(currentline,a+1,b-a-1));
              s2:=trim(copy(currentline,b+1,c-b-1));

              setlength(fullaccess,length(fullaccess)+1);
              fullaccess[length(fullaccess)-1].address:=symhandler.getAddressFromName(s1);
              fullaccess[length(fullaccess)-1].size:=strtoint(s2);
            end else raise exception.Create(rsSyntaxErrorFullAccessAddressSize);

            setlength(assemblerlines,length(assemblerlines)-1);
            continue;
          end;


          if uppercase(copy(currentline,1,6))='LABEL(' then
          begin
            //syntax: label(x)  x=name of the label
            //later on in the code there has to be a line with "labelname:"
            a:=pos('(',currentline);
            b:=pos(')',currentline);

            if (a>0) and (b>0) then
            begin
              s1:=trim(copy(currentline,a+1,b-a-1));


              val('$'+s1,j,a);
              if a=0 then raise exception.Create(Format(rsIsNotAValidIdentifier, [s1]));

              varsize:=length(s1);

              while (j<length(labels)) and (length(labels[j].labelname)>varsize) do
              begin
                if labels[j].labelname=s1 then
                  raise exception.Create(Format(rsIsBeingRedeclared, [s1]));
                inc(j);
              end;

              j:=length(labels);//quickfix
              l:=j;


              //check for the line "labelname:"
              ok1:=false;
              for j:=0 to code.Count-1 do
                if trim(code[j])=s1+':' then
                begin
                  if ok1 then raise exception.Create(Format(rsLabelIsBeingDefinedMoreThanOnce, [s1]));
                  ok1:=true;
                end;

              if not ok1 then raise exception.Create(Format(rsLabelIsNotDefinedInTheScript, [s1]));


              //still here so ok
              //insert it
              setlength(labels,length(labels)+1);
              for k:=length(labels)-1 downto j+1 do
                labels[k]:=labels[k-1];


              labels[l].labelname:=s1;
              labels[l].defined:=false;
              setlength(assemblerlines,length(assemblerlines)-1);
              setlength(labels[l].references,0);
              setlength(labels[l].references2,0);

              continue;
            end else raise exception.Create(rsSyntaxError);
          end;

          if (uppercase(copy(currentline,1,8))='DEALLOC(') then
          begin
            if (ceallocarray<>nil) then//memory dealloc=possible
            begin

              //syntax: dealloc(x)  x=name of region to deallocate
              //later on in the code there has to be a line with "labelname:"
              a:=pos('(',currentline);
              b:=pos(')',currentline);

              if (a>0) and (b>0) then
              begin
                s1:=trim(copy(currentline,a+1,b-a-1));

                //find s1 in the ceallocarray
                for j:=0 to length(ceallocarray)-1 do
                begin
                  if uppercase(ceallocarray[j].varname)=uppercase(s1) then
                  begin
                    setlength(dealloc,length(dealloc)+1);
                    dealloc[length(dealloc)-1]:=ceallocarray[j].address;
                  end;
                end;
              end;
            end;
            setlength(assemblerlines,length(assemblerlines)-1);
            continue;
          end;

          //memory alloc
          if uppercase(copy(currentline,1,6))='ALLOC(' then
          begin
            //syntax: alloc(x,size)    x=variable name size=bytes
            //or
            //syntax: alloc(x,size,prefered region)    x=variable name size=bytes
            //allocate memory
            a:=pos('(',currentline);
            b:=pos(',',currentline);
            c:=PosEx(',',currentline,b+1);
            d:=pos(')',currentline);



            if (a>0) and (b>0) and (d>0) then
            begin
              s1:=trim(copy(currentline,a+1,b-a-1));

              if c>0 then
              begin
                s2:=trim(copy(currentline,b+1,c-b-1));
                s3:=trim(copy(currentline,c+1,d-c-1));
              end
              else
              begin
                s2:=trim(copy(currentline,b+1,d-b-1));
                s3:='';
              end;

              val('$'+s1,j,a);
              if a=0 then raise exception.Create(Format(rsIsNotAValidIdentifier, [s1]));

              varsize:=length(s1);

              //check for duplicate identifiers
              j:=0;
              while (j<length(allocs)) and (length(allocs[j].varname)>varsize) do
              begin
                if allocs[j].varname=s1 then
                  raise exception.Create(Format(rsTheIdentifierHasAlreadyBeenDeclared, [s1]));

                inc(j);
              end;

              j:=length(allocs);//quickfix

              setlength(allocs,length(allocs)+1);

              //longest varnames first so the rename of a shorter matching var wont override the longer one
              //move up the other allocs so I can inser this element (A linked list might have been better)
              for k:=length(allocs)-1 downto j+1 do
                allocs[k]:=allocs[k-1];

              allocs[j].varname:=s1;
              allocs[j].size:=StrToInt(s2);
              if s3<>'' then
              begin

                allocs[j].prefered:=symhandler.getAddressFromName(s3);
              end
              else
                allocs[j].prefered:=0;


              setlength(assemblerlines,length(assemblerlines)-1);   //don't bother with this in the 2nd pass
              continue;
            end else raise exception.Create(rsWrongSyntaxALLOCIdentifierSizeinbytes);
          end;


          //replace ALLOC identifiers with values so the assemble error check doesnt crash on that
          if processhandler.is64bit then
          begin
            for j:=0 to length(allocs)-1 do
              currentline:=replacetoken(currentline,allocs[j].varname,'ffffffffffffffff');
          end
          else
          begin
            for j:=0 to length(allocs)-1 do
              currentline:=replacetoken(currentline,allocs[j].varname,'00000000');
          end;

          {$ifndef net}
          //memory kalloc
          if uppercase(copy(currentline,1,7))='KALLOC(' then
          begin
            if not DBKReadWrite then raise exception.Create(rsNeedToUseKernelmodeReadWriteprocessmemory);

            if DBKLoaded=false then
              raise exception.Create(rsSorryButWithoutTheDriverKALLOCWillNotFunction);

            //syntax: kalloc(x,size)    x=variable name size=bytes
            //kallocate memory
            a:=pos('(',currentline);
            b:=pos(',',currentline);
            c:=pos(')',currentline);

            if (a>0) and (b>0) and (c>0) then
            begin
              s1:=trim(copy(currentline,a+1,b-a-1));
              s2:=trim(copy(currentline,b+1,c-b-1));

              val('$'+s1,j,a);
              if a=0 then raise exception.Create(Format(rsIsNotAValidIdentifier, [s1]));

              varsize:=length(s1);

              //check for duplicate identifiers
              j:=0;
              while (j<length(kallocs)) and (length(kallocs[j].varname)>varsize) do
              begin
                if kallocs[j].varname=s1 then
                  raise exception.Create(Format(rsTheIdentifierHasAlreadyBeenDeclared, [s1]));

                inc(j);
              end;

              j:=length(kallocs);//quickfix

              setlength(kallocs,length(kallocs)+1);

              //longest varnames first so the rename of a shorter matching var wont override the longer one
              //move up the other kallocs so I can inser this element (A linked list might have been better)
              for k:=length(kallocs)-1 downto j+1 do
                kallocs[k]:=kallocs[k-1];

              kallocs[j].varname:=s1;
              kallocs[j].size:=StrToInt(s2);

              setlength(assemblerlines,length(assemblerlines)-1);   //don't bother with this in the 2nd pass
              continue;
            end else raise exception.Create(rsWrongSyntaxKallocIdentifierSizeinbytes);
          end;

          {$endif}

          //replace KALLOC identifiers with values so the assemble error check doesnt crash on that
          if processhandler.is64bit then
          begin
            for j:=0 to length(kallocs)-1 do
              currentline:=replacetoken(currentline,kallocs[j].varname,'ffffffffffffffff');
          end
          else
          begin
            for j:=0 to length(kallocs)-1 do
              currentline:=replacetoken(currentline,kallocs[j].varname,'00000000');
          end;



          //check for assembler errors
          //address

          if currentline[length(currentline)]=':' then
          begin
            try
              ok1:=false;
              for j:=0 to length(labels)-1 do
                if currentline=labels[j].labelname+':' then
                begin
                  labels[j].assemblerline:=length(assemblerlines)-1;
                  ok1:=true;
                  continue;
                end;

              if ok1 then continue; //no check

              try
                j:=symhandler.getAddressFromName(copy(currentline,1,length(currentline)-1));
              except
                currentline:=inttohex(symhandler.getaddressfromname(copy(currentline,1,length(currentline)-1)),8)+':';
                assemblerlines[length(assemblerlines)-1]:=currentline;
              end;

              continue; //next line
            except
              raise exception.Create(rsThisAddressSpecifierIsNotValid);
            end;
          end;

          //replace label references with 00000000 so the assembler check doesn't complain about it

          if processhandler.is64bit then
          begin
            for j:=0 to length(labels)-1 do
              currentline:=replacetoken(currentline,labels[j].labelname,'ffffffffffffffff');
          end
          else
          begin
            for j:=0 to length(labels)-1 do
              currentline:=replacetoken(currentline,labels[j].labelname,'00000000');
          end;


          try
            //replace identifiers in the line with their address
            if not assemble(currentline,currentaddress,assembled[0].bytes, apNone, true) then raise exception.Create('bla');
          except
            raise exception.Create(rsThisInstructionCanTBeCompiled);
          end;

        finally
          inc(i);
        end;

      except
        on E:exception do
          raise exception.Create(Format(rsErrorInLine, [IntToStr(currentlinenr), currentline, e.Message]));

      end;
    end;

    if length(addsymbollist)>0 then
    begin
      //now scan the addsymbollist entries for allocs and labels and see if they exist
      for i:=0 to length(addsymbollist)-1 do
      begin
        ok1:=false;
        for j:=0 to length(allocs)-1 do  //scan allocs
          if uppercase(addsymbollist[i])=uppercase(allocs[j].varname) then
          begin
            ok1:=true;
            break;
          end;

        if not ok1 then //scan labels
          for j:=0 to length(labels)-1 do
            if uppercase(addsymbollist[i])=uppercase(labels[j].labelname) then
            begin
              ok1:=true;
              break;
            end;


        if not ok1 then raise exception.Create(Format(rsWasSupposedToBeAddedToTheSymbollistButItIsnTDeclar, [addsymbollist[i]]));
      end;
    end;

    //check to see if the addresses are valid (label, alloc, define)
    if length(createthread)>0 then
      for i:=0 to length(createthread)-1 do
      begin
        ok1:=true;

        try
          testptr:=symhandler.getAddressFromName(createthread[i]);
        except
          ok1:=false;
        end;

        if not ok1 then
          for j:=0 to length(labels)-1 do
            if uppercase(labels[j].labelname)=uppercase(createthread[i]) then
            begin
              ok1:=true;
              break;
            end;

        if not ok1 then
          for j:=0 to length(allocs)-1 do
            if uppercase(allocs[j].varname)=uppercase(createthread[i]) then
            begin
              ok1:=true;
              break;
            end;

        {$ifndef net}
        if not ok1 then
          for j:=0 to length(kallocs)-1 do
            if uppercase(kallocs[j].varname)=uppercase(createthread[i]) then
            begin
              ok1:=true;
              break;
            end;
        {$endif}

        if not ok1 then
          for j:=0 to length(defines)-1 do
            if uppercase(defines[j].name)=uppercase(createthread[i]) then
            begin
              try
                testptr:=symhandler.getAddressFromName(defines[j].whatever);
                ok1:=true;
              except
              end;
              break;
            end;

        if not ok1 then raise exception.Create(Format(rsTheAddressInCreatethreadIsNotValid, [createthread[i]]));

      end;

    if length(loadbinary)>0 then
      for i:=0 to length(loadbinary)-1 do
      begin
        ok1:=true;

        try
          testptr:=symhandler.getAddressFromName(loadbinary[i].address);
        except
          ok1:=false;
        end;

        if not ok1 then
          for j:=0 to length(labels)-1 do
            if uppercase(labels[j].labelname)=uppercase(loadbinary[i].address) then
            begin
              ok1:=true;
              break;
            end;

        if not ok1 then
          for j:=0 to length(allocs)-1 do
            if uppercase(allocs[j].varname)=uppercase(loadbinary[i].address) then
            begin
              ok1:=true;
              break;
            end;

        {$ifndef net}
        if not ok1 then
          for j:=0 to length(kallocs)-1 do
            if uppercase(kallocs[j].varname)=uppercase(loadbinary[i].address) then
            begin
              ok1:=true;
              break;
            end;
        {$endif}

        if not ok1 then
          for j:=0 to length(defines)-1 do
            if uppercase(defines[j].name)=uppercase(loadbinary[i].address) then
            begin
              try
                testptr:=symhandler.getAddressFromName(defines[j].whatever);
                ok1:=true;
              except
              end;
              break;
            end;

        if not ok1 then raise exception.Create(Format(rsTheAddressInLoadbinaryIsNotValid, [loadbinary[i].address, loadbinary[i].filename]));

      end;


    if syntaxcheckonly then
    begin
      result:=true;
      exit;
    end;

    if popupmessages and (messagedlg(rsThisCodeCanBeInjectedAreYouSure, mtConfirmation	, [mbyes, mbno], 0)<>mryes) then exit;

    //allocate the memory

    if length(allocs)>0 then
    begin

      j:=0; //entry to go from
      prefered:=allocs[0].prefered;
      x:=allocs[0].size;

      for i:=1 to length(allocs)-1 do
      begin
        //does this entry have a prefered location?
        if allocs[i].prefered<>0 then
        begin
          //if yes, is it the same as the previous entry? (or was the previous one that doesn't care?)
          if (prefered<>allocs[i].prefered) and (prefered<>0) then
          begin
            //different prefered address

            if x>0 then //it has some previous entries with compatible locations
            begin


              k:=10;
              allocs[j].address:=0;
              while (k>0) and (allocs[j].address=0) do
              begin
                //try allocating untill a memory region has been found (e.g due to quick allocating by the game)
                allocs[j].address:=ptrUint(virtualallocex(processhandle,FindFreeBlockForRegion(prefered,x),x, MEM_RESERVE or MEM_COMMIT,page_execute_readwrite));
                if allocs[j].address=0 then OutputDebugString(rsFailureToAllocateMemory+' 1');

                dec(k);
              end;

              if allocs[j].address=0 then
                allocs[j].address:=ptrUint(virtualallocex(processhandle,nil,x, MEM_RESERVE or MEM_COMMIT,page_execute_readwrite));

              if allocs[j].address=0 then OutputDebugString(rsFailureToAllocateMemory+' 2');

              //adjust the addresses of entries that are part of this block
              for k:=j+1 to i-1 do
                allocs[k].address:=allocs[k-1].address+allocs[k-1].size;
              x:=0;
            end;


            //new prefered address
            j:=i;
            prefered:=allocs[i].prefered;
          end;
        end;

        //no prefered location specified, OR same prefered location

        inc(x,allocs[i].size);
      end; //after the loop


      if x>0 then
      begin
        //adjust the address of entries that are part of this final block
        k:=10;
        allocs[j].address:=0;
        while (k>0) and (allocs[j].address=0) do
        begin
          i:=0;
          prefered:=ptrUint(FindFreeBlockForRegion(prefered,x));


          allocs[j].address:=ptrUint(virtualallocex(processhandle,pointer(prefered),x, MEM_RESERVE or MEM_COMMIT,page_execute_readwrite));
          if allocs[j].address=0 then OutputDebugString(rsFailureToAllocateMemory+' 3');
          dec(k);
        end;

        if allocs[j].address=0 then
          allocs[j].address:=ptrUint(virtualallocex(processhandle,nil,x, MEM_RESERVE or MEM_COMMIT,page_execute_readwrite));

        if allocs[j].address=0 then raise exception.create(rsFailureToAllocateMemory+' 4');

        for i:=j+1 to length(allocs)-1 do
          allocs[i].address:=allocs[i-1].address+allocs[i-1].size;


      end;
    end;

    {$ifndef net}
    //kernel alloc
    if length(kallocs)>0 then
    begin
      x:=0;
      for i:=0 to length(kallocs)-1 do
       inc(x,kallocs[i].size);

      kallocs[0].address:=ptrUint(KernelAlloc(x));

      for i:=1 to length(kallocs)-1 do
        kallocs[i].address:=kallocs[i-1].address+kallocs[i-1].size;
    end;
    {$endif}

    //-----------------------2nd pass------------------------
    //assemblerlines only contains label specifiers and assembler instructions
    
    setlength(assembled,0);
    for i:=0 to length(assemblerlines)-1 do
    begin
      currentline:=assemblerlines[i];

{$ifndef standalonetrainer}
      //plugin
      if length(currentline)>0 then
      begin
        currentlinep:=@currentline[1];
        pluginhandler.handleAutoAssemblerPlugin(@currentlinep, 2);
        currentline:=currentlinep;
        //if handled currentline will have it's identifiers regarding the plugin's previously registered stuff replaced
        //note that this can be called in a multithreaded situation, so the plugin must hld storage containers on a threadid base and handle the locking itself
      end;
      //plugin
{$endif}


      tokenize(currentline,tokens);
      //if alloc then replace with the address
      for j:=0 to length(allocs)-1 do
        currentline:=replacetoken(currentline,allocs[j].varname,IntToHex(allocs[j].address,8));

      //if kalloc then replace with the address
      for j:=0 to length(kallocs)-1 do
        currentline:=replacetoken(currentline,kallocs[j].varname,IntToHex(kallocs[j].address,8));

      for j:=0 to length(defines)-1 do
        currentline:=replacetoken(currentline,defines[j].name,defines[j].whatever);

      ok1:=false;
      if currentline[length(currentline)]<>':' then //if it's not a definition then
      for j:=0 to length(labels)-1 do
        if tokencheck(currentline,labels[j].labelname) then
        begin
          if not labels[j].defined then
          begin
            //the address hasn't been found yet
            //this is the part that causes those nops after a short jump below the current instruction

            //close
            s1:=replacetoken(currentline,labels[j].labelname,IntToHex(currentaddress,8));

            //far and big

            if (processhandler.is64Bit) then //and not in region
              currentline:=replacetoken(currentline,labels[j].labelname,IntToHex(currentaddress+$1000FFFFF,8))
            else
            currentline:=replacetoken(currentline,labels[j].labelname,IntToHex(currentaddress+$FFFFF,8));

            setlength(assembled,length(assembled)+1);
            assembled[length(assembled)-1].address:=currentaddress;
            assemble(currentline,currentaddress,assembled[length(assembled)-1].bytes, apnone, true);
            a:=length(assembled[length(assembled)-1].bytes);

            assemble(s1,currentaddress,assembled[length(assembled)-1].bytes, apnone, true);
            b:=length(assembled[length(assembled)-1].bytes);

            if a>b then //pick the biggest one
              assemble(currentline,currentaddress,assembled[length(assembled)-1].bytes);

            setlength(labels[j].references,length(labels[j].references)+1);
            labels[j].references[length(labels[j].references)-1]:=length(assembled)-1;

            setlength(labels[j].references2,length(labels[j].references2)+1);
            labels[j].references2[length(labels[j].references2)-1]:=i;

            inc(currentaddress,length(assembled[length(assembled)-1].bytes));
            ok1:=true;
          end else currentline:=replacetoken(currentline,labels[j].labelname,IntToHex(labels[j].address,8));

          break;
        end;

      if ok1 then continue;

      if currentline[length(currentline)]=':' then
      begin
        ok1:=false;
        for j:=0 to length(labels)-1 do
        begin
          if i=labels[j].assemblerline then
          begin
            if labels[j].defined=false then
            begin
              labels[j].address:=currentaddress;
              labels[j].defined:=true;
              ok1:=true;
            end
            else
            begin
              currentaddress:=labels[j].address;
              ok1:=true;
            end;


            //reassemble the instructions that had no target
            for k:=0 to length(labels[j].references)-1 do
            begin
              a:=length(assembled[labels[j].references[k]].bytes); //original size of the assembled code
              s1:=replacetoken(assemblerlines[labels[j].references2[k]],labels[j].labelname,IntToHex(labels[j].address,8));
              {$ifdef cpu64}
              if processhandler.is64Bit then
                assemble(s1,assembled[labels[j].references[k]].address,assembled[labels[j].references[k]].bytes)
              else
              {$endif}
              assemble(s1,assembled[labels[j].references[k]].address,assembled[labels[j].references[k]].bytes, apLong);


              b:=length(assembled[labels[j].references[k]].bytes); //new size

              setlength(assembled[labels[j].references[k]].bytes,a); //original size (original size is always bigger or equal than newsize)
              //fill the difference with nops (not the most efficient approach, but it should work)
              for l:=b to a-1 do
                assembled[labels[j].references[k]].bytes[l]:=$90;
            end;


            break;
          end;
        end;
        if ok1 then continue;

        try
          currentaddress:=symhandler.getAddressFromName(copy(currentline,1,length(currentline)-1));
          continue; //next line
        except
          raise exception.Create(rsThisAddressSpecifierIsNotValid);
        end;
      end;


      setlength(assembled,length(assembled)+1);
      assembled[length(assembled)-1].address:=currentaddress;
      assemble(currentline,currentaddress,assembled[length(assembled)-1].bytes);

      inc(currentaddress,length(assembled[length(assembled)-1].bytes));
    end;
    //end of loop
    
    ok2:=true;

    //unprotectmemory
    for i:=0 to length(fullaccess)-1 do
    begin
      virtualprotectex(processhandle,pointer(fullaccess[i].address),fullaccess[i].size,PAGE_EXECUTE_READWRITE,op);

      if (fullaccess[i].address>80000000) and (DBKLoaded) then
        MakeWritable(fullaccess[i].address,(fullaccess[i].size div 4096)*4096,false);
    end;

    //load binaries
    if length(loadbinary)>0 then
      for i:=0 to length(loadbinary)-1 do
      begin
        ok1:=true;
        try
          testptr:=symhandler.getAddressFromName(loadbinary[i].address);
        except
          ok1:=false;
        end;

        if not ok1 then
          for j:=0 to length(labels)-1 do
            if uppercase(labels[j].labelname)=uppercase(loadbinary[i].address) then
            begin
              ok1:=true;
              testptr:=labels[j].address;
              break;
            end;

        if not ok1 then
          for j:=0 to length(allocs)-1 do
            if uppercase(allocs[j].varname)=uppercase(loadbinary[i].address) then
            begin
              ok1:=true;
              testptr:=allocs[j].address;
              break;
            end;

        if not ok1 then
          for j:=0 to length(kallocs)-1 do
            if uppercase(kallocs[j].varname)=uppercase(loadbinary[i].address) then
            begin
              ok1:=true;
              testptr:=kallocs[j].address;
              break;
            end;

        if not ok1 then
          for j:=0 to length(defines)-1 do
            if uppercase(defines[j].name)=uppercase(loadbinary[i].address) then
            begin
              try
                testptr:=symhandler.getAddressFromName(defines[j].whatever);
                ok1:=true;
              except
              end;

              break;
            end;

        if ok1 then
        begin
          binaryfile:=tmemorystream.Create;
          try
            binaryfile.LoadFromFile(loadbinary[i].filename);
            ok2:=writeprocessmemory(processhandle,pointer(testptr),binaryfile.Memory,binaryfile.Size,bw);
          finally
            binaryfile.free;
          end;
        end;
      end;

    //we're still here so, inject it
    for i:=0 to length(assembled)-1 do
    begin
      testptr:=assembled[i].address;
      ok1:=virtualprotectex(processhandle,pointer(testptr),length(assembled[i].bytes),PAGE_EXECUTE_READWRITE,op);
      ok1:=WriteProcessMemory(processhandle,pointeR(testptr),@assembled[i].bytes[0],length(assembled[i].bytes),op2);
      virtualprotectex(processhandle,pointer(testptr),length(assembled[i].bytes),op,op2);

      if not ok1 then ok2:=false;
    end;

    if not ok2 then
    begin
      if popupmessages then showmessage(rsNotAllInstructionsCouldBeInjected)
    end
    else
    begin
      //if ceallocarray<>nil then
      begin
        //see if all allocs are deallocated
        if length(dealloc)=length(ceallocarray) then //free everything
        begin
          {$ifdef cpu64}
          baseaddress:=ptrUint($FFFFFFFFFFFFFFFF);
          {$else}
          baseaddress:=$FFFFFFFF;
          {$endif}

          for i:=0 to length(ceallocarray)-1 do
          begin
            if ceallocarray[i].address<baseaddress then
              baseaddress:=ceallocarray[i].address;
          end;
          virtualfreeex(processhandle,pointer(baseaddress),0,MEM_RELEASE);
        end;

        setlength(ceallocarray,length(allocs));
        for i:=0 to length(allocs)-1 do
          ceallocarray[i]:=allocs[i];
      end;





      //check the addsymbollist array and deletesymbollist array

      //first delete
      for i:=0 to length(deletesymbollist)-1 do
        symhandler.DeleteUserdefinedSymbol(deletesymbollist[i]);

      //now scan the addsymbollist array and add them to the userdefined list
      for i:=0 to length(addsymbollist)-1 do
      begin
        ok1:=false;
        for j:=0 to length(allocs)-1 do
          if uppercase(addsymbollist[i])=uppercase(allocs[j].varname) then
          begin
            try
              symhandler.DeleteUserdefinedSymbol(addsymbollist[i]); //delete old one so you can add the new one
              symhandler.AddUserdefinedSymbol(inttohex(allocs[j].address,8),addsymbollist[i], true);
              ok1:=true;
            except
              //don't crash when it's already defined or address=0
            end;

            break;
          end;

        if not ok1 then
          for j:=0 to length(labels)-1 do
            if uppercase(addsymbollist[i])=uppercase(labels[j].labelname) then
            begin
              try
                symhandler.DeleteUserdefinedSymbol(addsymbollist[i]); //delete old one so you can add the new one
                symhandler.AddUserdefinedSymbol(inttohex(labels[j].address,8),addsymbollist[i]);
                ok1:=true;
              except
                //don't crash when it's already defined or address=0
              end;

            end;
      end;

      //still here, so create threads if needed
      if length(createthread)>0 then
        for i:=0 to length(createthread)-1 do
        begin
          ok1:=true;
          try
            testptr:=symhandler.getAddressFromName(createthread[i]);
          except
            ok1:=false;
          end;

          if not ok1 then
            for j:=0 to length(labels)-1 do
              if uppercase(labels[j].labelname)=uppercase(createthread[i]) then
              begin
                ok1:=true;
                testptr:=labels[j].address;
                break;
              end;

          if not ok1 then
            for j:=0 to length(allocs)-1 do
              if uppercase(allocs[j].varname)=uppercase(createthread[i]) then
              begin
                ok1:=true;
                testptr:=allocs[j].address;
                break;
              end;

          if not ok1 then
            for j:=0 to length(kallocs)-1 do
              if uppercase(kallocs[j].varname)=uppercase(createthread[i]) then
              begin
                ok1:=true;
                testptr:=kallocs[j].address;
                break;
              end;

          if not ok1 then
            for j:=0 to length(defines)-1 do
              if uppercase(defines[j].name)=uppercase(createthread[i]) then
              begin
                try
                  testptr:=symhandler.getAddressFromName(defines[j].whatever);
                  ok1:=true;
                except
                end;

                break;
              end;

          if ok1 then //address found
          begin
            try
              ok2:=createremotethread(processhandle,nil,0,pointer(testptr),nil,0,bw)>0;
            finally
            end;
          end;
        end;

      if popupmessages then
      begin
        s1:='';
        for i:=0 to length(globalallocs)-1 do
          s1:=s1+#13#10+globalallocs[i].varname+'='+IntToHex(globalallocs[i].address,8);


        for i:=0 to length(allocs)-1 do
          s1:=s1+#13#10+allocs[i].varname+'='+IntToHex(allocs[i].address,8);

        if length(kallocs)>0 then
        begin
          s1:=#13#10+rsTheFollowingKernelAddressesWhereAllocated+':';
          for i:=0 to length(kallocs)-1 do
            s1:=s1+#13#10+kallocs[i].varname+'='+IntToHex(kallocs[i].address,8);
        end;

        showmessage(rsTheCodeInjectionWasSuccessfull+s1);
      end;
    end;

    result:=ok2;

  finally
    for i:=0 to length(assembled)-1 do
      setlength(assembled[i].bytes,0);

    setlength(assembled,0);
    tokens.free;

{$ifndef standalonetrainer}
    pluginhandler.handleAutoAssemblerPlugin(@currentlinep, 3); //tell the plugins to free their data
{$endif}

    if targetself then
      processhandler.processhandle:=oldhandle;
  end;
end;


function getenableanddisablepos(code:tstrings;var enablepos,disablepos: integer): boolean;
var i,j: integer;
    currentline: string;
begin
  result:=false;
  enablepos:=-1;
  disablepos:=-1;

  for i:=0 to code.Count-1 do
  begin
    currentline:=code[i];
    j:=pos('//',currentline);
    if j>0 then
      currentline:=copy(currentline,1,j-1);

    while (length(currentline)>0) and (currentline[1]=' ') do currentline:=copy(currentline,2,length(currentline)-1);
    while (length(currentline)>0) and (currentline[length(currentline)]=' ') do currentline:=copy(currentline,1,length(currentline)-1);

    if length(currentline)=0 then continue;
    if copy(currentline,1,2)='//' then continue; //skip

    if (uppercase(currentline))='[ENABLE]' then
    begin
      result:=true; //there's at least a enable section, so it's ok
      if enablepos<>-1 then
      begin
        enablepos:=-2;
        exit;
      end;

      enablepos:=i;
    end;

    if (uppercase(currentline))='[DISABLE]' then
    begin
      if disablepos<>-1 then
      begin
        disablepos:=-2;
        exit;
      end;

      disablepos:=i;
    end;

  end;
end;


procedure getScript(code: TStrings; newscript: tstrings; enablescript: boolean);
{
removes the enable or disable section from a script leaving only the outer code and the selected script routine
}
var
  i: integer;
  insideenable: boolean;
  insidedisable: boolean;
begin
  insideenable:=false;
  insidedisable:=false;

  for i:=0 to code.Count-1 do
  begin
    if (uppercase(code[i]))='[ENABLE]' then
    begin
      insideenable:=true;
      insidedisable:=false;
      continue;
    end;

    if (uppercase(code[i]))='[DISABLE]' then
    begin
      insideenable:=false;
      insidedisable:=true;
      continue;
    end;

    //
    if ((not insideenable) and (not insidedisable)) or
       (insideenable and enablescript) or
       (insidedisable and not enablescript) then newscript.AddObject(code[i], code.Objects[i]);



  end;

end;

procedure stripCPUspecificCode(code: tstrings);
var i: integer;
  s: string;
  inexcludedbitblock: boolean;

begin
  inexcludedbitblock:=false;
  for i:=0 to code.Count-1 do
  begin
    s:=uppercase(Trim(code[i]));

    if s='[32-BIT]' then
    begin
      {$ifdef cpu64}
      inexcludedbitblock:=true;
      {$endif}
      code[i]:=' ';
    end;

    if s='[/32-BIT]' then
    begin
      {$ifdef cpu64}
      inexcludedbitblock:=false;
      {$endif}
      code[i]:=' ';
    end;

    if s='[64-BIT]' then
    begin
      {$ifdef cpu32}
      inexcludedbitblock:=true;
      {$endif}
      code[i]:=' ';
    end;

    if s='[/64-BIT]' then
    begin
      {$ifdef cpu32}
      inexcludedbitblock:=false;
      {$endif}
      code[i]:=' ';
    end;

    if inexcludedbitblock then
      code[i]:=' ';



  end;
end;

function autoassemble(code: Tstrings; popupmessages,enable,syntaxcheckonly, targetself: boolean;var CEAllocarray: TCEAllocArray; registeredsymbols: tstringlist=nil): boolean; overload;
{
targetself defines if the process that gets injected to is CE itself or the target process
}
var tempstrings: tstringlist;
    i,j: integer;
    currentline: string;
    enablepos,disablepos: integer;
begin
  //add line numbers to the code
  for i:=0 to code.Count-1 do
    code.Objects[i]:=pointer(i+1);

  getenableanddisablepos(code,enablepos,disablepos);

  result:=false;
  
  if enablepos=-2 then
  begin
    if not popupmessages then exit;
    raise exception.Create(rsYouCanOnlyHaveOneEnableSection);
  end;

  if disablepos=-2 then
  begin
    if not popupmessages then exit;
    raise exception.Create(rsYouCanOnlyHaveOneDisableSection);
  end;

  tempstrings:=tstringlist.create;
  try
    if (enablepos=-1) and (disablepos=-1) then
    begin
      //everything
      tempstrings.AddStrings(code);
    end
    else
    begin
      if (enablepos=-1) then
      begin
        if not popupmessages then exit;
        raise exception.Create(rsYouHavnTSpecifiedAEnableSection);

      end;

      if (disablepos=-1) then
      begin
        if not popupmessages then exit;
        raise exception.Create(rsYouHavnTSpecifiedADisableSection);
        
      end;

      if enable then
      begin
        getscript(code, tempstrings, true);
      end
      else
      begin
        getscript(code, tempstrings,false);
      end;
    end;

    if targetself then
      Stripcpuspecificcode(tempstrings);

    result:=autoassemble2(tempstrings,popupmessages,syntaxcheckonly,targetself,ceallocarray, registeredsymbols);
  finally
    tempstrings.Free;
  end;
end;


function autoassemble(code: tstrings;popupmessages: boolean):boolean; overload;
var aa: TCEAllocArray;
begin
  setlength(aa,0);
  result:=autoassemble(code,popupmessages,true,false,false,aa,nil);
end;


end.




