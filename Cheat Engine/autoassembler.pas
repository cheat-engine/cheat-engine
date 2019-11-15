// Copyright Cheat Engine. All Rights Reserved.

unit autoassembler;

{$MODE Delphi}

interface


{$ifdef jni}
uses unixporthelper, Assemblerunit, classes, symbolhandler, sysutils,
     NewKernelHandler, ProcessHandlerUnit, commonTypeDefs;
{$endif}

{$ifdef windows}
uses jwawindows, windows, Assemblerunit, classes, LCLIntf,symbolhandler, symbolhandlerstructs,
     sysutils,dialogs,controls, CEFuncProc, NewKernelHandler ,plugin,
     ProcessHandlerUnit, lua, lualib, lauxlib, luaclass, commonTypeDefs;
{$endif}




function getenableanddisablepos(code:tstrings;var enablepos,disablepos: integer): boolean;
function autoassemble(code: tstrings;popupmessages: boolean):boolean; overload;
function autoassemble(code: Tstrings; popupmessages,enable,syntaxcheckonly, targetself: boolean):boolean; overload;
function autoassemble(code: Tstrings; popupmessages,enable,syntaxcheckonly, targetself: boolean;var CEAllocarray: TCEAllocArray; var exceptionlist:TCEExceptionListArray; registeredsymbols: tstringlist=nil; memrec: pointer=nil): boolean; overload;

type TAutoAssemblerPrologue=procedure(code: TStrings; syntaxcheckonly: boolean) of object;
type TAutoAssemblerCallback=function(parameters: string; syntaxcheckonly: boolean): string of object;
type TRegisteredAutoAssemblerCommand=class
  command: string;
  callback: TAutoAssemblerCallback;
end;

type EAutoAssembler=class(exception);

procedure RegisterAutoAssemblerCommand(command: string; callback: TAutoAssemblerCallback);
procedure UnregisterAutoAssemblerCommand(command: string);
function registerAutoAssemblerPrologue(m: TAutoAssemblerPrologue; postAOBSCAN: boolean=false): integer;
procedure unregisterAutoAssemblerPrologue(id: integer);

var oldaamessage: boolean;

implementation

{$ifdef jni}
uses strutils, memscan, disassembler, networkInterface, networkInterfaceApi,
     Parsers, Globals, memoryQuery;
{$endif}


{$ifdef windows}
uses simpleaobscanner, StrUtils, LuaHandler, memscan, disassembler, networkInterface,
     networkInterfaceApi, LuaCaller, SynHighlighterAA, Parsers, Globals, memoryQuery,
     MemoryBrowserFormUnit, MemoryRecordUnit, vmxfunctions, autoassemblerexeptionhandler,
     UnexpectedExceptionsHelper;
{$endif}


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
  rsWrongSyntaxReAssemble = 'Wrong syntax. Reassemble(address)';
  rsSyntaxError = 'Syntax error';
  rsTheArrayOfByteCouldNotBeFound = 'The array of byte ''%s'' could not be found';
  rsWrongSyntaxAOBSCANName11223355 = 'Wrong syntax. AOBSCAN(name,11 22 33 ** 55)';
  rsWrongSyntaxAOBSCANMODULEName11223355 = 'Wrong syntax. AOBSCANMODULE(name, module, 11 22 33 ** 55)';
  rsWrongSyntaxAOBSCANREGION = 'Wrong syntax. AOBSCANREGION(name, startaddress, stopaddress, 11 22 33 ** 55)';


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
  rsTheAddressInCreatethreadAndWaitIsNotValid = 'The address in createthreadandwait(%s) is not valid';
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
  rsAAErrorInTheStructureDefinitionOf = 'Error in the structure definition of %s at line %d';
  rsAAIsAReservedWord = '%s is a reserved word';
  rsAANoIdeaWhatXis = 'No idea what %s is';
  rsAANoEndFound = 'No end found';
  rsAATheArrayOfByteNamed = 'The array of byte named %s could not be found';
  rsXCouldNotBeFound = '%s could not be found';
  rsAAErrorWhileSacnningForAobs = 'Error while scanning for AOB''s : ';
  rsAAError = 'Error: ';
  rsAAModuleNotFound = 'module not found:';
  rsAALuaErrorInTheScriptAtLine = 'Lua error in the script at line ';
  rsGoTo = 'Go to ';
  rsMissingExcept = 'The {$TRY} at line %d has no matching {$EXCEPT}';

//type
//  TregisteredAutoAssemblerCommands =  TFPGList<TRegisteredAutoAssemblerCommand>;

type
  TAutoAssemblerPrologues=array of TAutoAssemblerPrologue;
  PAutoAssemblerPrologues=^TAutoAssemblerPrologues;
var
  registeredAutoAssemblerCommands: TList;

  AutoAssemblerPrologues: TAutoAssemblerPrologues;
  AutoAssemblerProloguesPostAOBSCAN: TAutoAssemblerPrologues;

function registerAutoAssemblerPrologue(m: TAutoAssemblerPrologue; postAOBSCAN: boolean=false): integer;
var i: integer;
    prologues: PAutoAssemblerPrologues;
begin
  if postAOBSCAN then prologues:=@AutoAssemblerProloguesPostAOBSCAN
                 else prologues:=@AutoAssemblerPrologues;
  for i:=0 to length(prologues^)-1 do
  begin
    if assigned(prologues^[i])=false then
    begin
      prologues^[i]:=m;
      result:=i+1;

      if postAOBSCAN then
        result:=-result;

      exit;
    end
  end;

  result:=length(prologues^)+1;  //first one is id 1
  setlength(prologues^, result);
  prologues^[result-1]:=m;

  if postAOBSCAN then
    result:=-result; //first one is id -1
end;

procedure unregisterAutoAssemblerPrologue(id: integer);
var
  prologues: PAutoAssemblerPrologues;
begin
  if id=0 then exit; //does not exist

  if id<0 then
  begin
    prologues:=@AutoAssemblerProloguesPostAOBSCAN;
    id:=-id;
  end
  else
    prologues:=@AutoAssemblerPrologues;

  if id<=length(prologues^) then
  begin
    {$ifndef unix}
    CleanupLuaCall(TMethod(prologues^[id-1]));
    {$endif}
    prologues^[id-1]:=nil;
  end;
end;

procedure RegisterAutoAssemblerCommand(command: string; callback: TAutoAssemblerCallback);
var i: integer;
    c:TRegisteredAutoAssemblerCommand;
begin
  {$ifndef jni}
  if registeredAutoAssemblerCommands=nil then
    registeredAutoAssemblerCommands:=TList.Create;

  UnregisterAutoAssemblerCommand(command);

  command:=uppercase(command);
  c:=TRegisteredAutoAssemblerCommand.create;
  c.command:=command;
  c.callback:=callback;
  registeredAutoAssemblerCommands.Add(c);

  aa_AddExtraCommand(pchar(command));
  {$endif}
end;

procedure UnregisterAutoAssemblerCommand(command: string);
var i,j: integer;
    c:TRegisteredAutoAssemblerCommand;
begin
{$ifndef jni}
  command:=uppercase(command);
  i:=0;
  while i<registeredAutoAssemblerCommands.count do
  begin
    if TRegisteredAutoAssemblerCommand(registeredAutoAssemblerCommands[i]).command=command then
    begin
      c:=registeredAutoAssemblerCommands[i];
      CleanupLuaCall(tmethod(c.callback));
      registeredAutoAssemblerCommands.Delete(i);

      c.free;
    end
    else
      inc(i);
  end;

  aa_RemoveExtraCommand(pchar(command));
{$endif jni}
end;



//----------------------------

procedure tokenize(input: string; tokens: tstringlist);
var i: integer;
    a: integer;
    inquote: boolean=false;
    inquote2: boolean=false;
begin

  tokens.clear;
  a:=-1;
  for i:=1 to length(input) do
  begin
    if inquote and (input[i]<>'''') then continue;
    if inquote2 and (input[i]<>'"') then continue;

    case input[i] of
      'a'..'z','A'..'Z','0'..'9','.', '_','#','@': if a=-1 then a:=i;
      else
      begin
        if (input[i]='''') then
        begin
          if inquote then
          begin
            if a<>-1 then
              tokens.AddObject(copy(input,a,i-a),tobject(a));

            a:=-1;
            inquote:=false;
          end
          else
          begin
            inquote:=true;
            a:=i;
          end;

          continue;
        end;

        if (input[i]='"') then
        begin
          if inquote2 then
          begin
            if a<>-1 then
              tokens.AddObject(copy(input,a,i-a),tobject(a));

            a:=-1;
            inquote2:=false;
          end
          else
          begin
            inquote2:=true;
            a:=i;
          end;

          continue;
        end;

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
    tokenize(result,tokens);
    for i:=tokens.Count-1 downto 0 do
      if tokens[i]=token then
      begin
        j:=integer(tokens.Objects[i]);
        result:=copy(result,1,j-1)+replacewith+copy(result,j+length(token),length(result));
      end;

  finally
    tokens.free;
  end;
end;

procedure tokenizeStruct(input: string; tokens: tstringlist);
//a version of tokenize using strutils to split up the strings. (I don't want to replace it yet since it is slightly different, probably better, but let's be safe for now till it's fully tested)
var delims: TSysCharSet;
    i: integer;
begin
  delims:=[' '];

  tokens.clear;
  for i:=1 to WordCount(input, delims) do
    tokens.add(ExtractWord(i, input, delims));

end;

procedure replaceStructWithDefines(code: Tstrings; linenr: integer);
{
parses the structure starting at the given line number.
removes the structure definition from the code
writes a define(xxxx,xxxxx) inplace starting from the linenumber

precondition: linenr is valid and code[linenr] is indeed a STRUCT name line

Structure format:
struct name
  db ?  db ? db ? db ?
  db ? ? ?
  dw ?
  dd ?
  dq ?
  resb 4 resb 4
  resw 4
  resd 2
  resq 1
elementname1:
  db ?
  resw 1
  dw ?
elementname2: dw ?

endstruct

Res is an alternate to the usual "d* ?" but is specifically for no fill  (Res stands for reserve)
Usage res* #  where # is the number of copies.  This is better suited for strings which can not be defined with db ?

when done the name.elementname1 and just elementname1 will be defined at the spot the structure was placed. Subsequent structure definitions can override these defines. Elements may NOT have reserved words as they will get replaced. (like assembler commands)
}


var
  currentOffset: integer;
  structname: string;
  elementname: string;
  i,j,k: integer;
  tokens: Tstringlist;

  elements: tstringlist;
  starttoken: integer;

  bytesize: integer;
  endfound: boolean;
  lastlinenr: integer;
  procedure structError(reason: string='');
  var error: string;
  begin
    error:=format(rsAAErrorInTheStructureDefinitionOf, [structname, lastlinenr+1]);
    if reason<>'' then
      error:=error+' :'+reason
    else
      error:=error+'.';

    raise exception.create(error);
  end;

begin
  lastlinenr:=linenr;
  endfound:=false;
  structname:=trim(copy(code[linenr], 7, length(code[linenr])));

  currentOffset:=0;
  tokens:=tstringlist.create;
  elements:=tstringlist.create;

  for i:=linenr+1 to code.count-1 do
  begin
    lastlinenr:=i;
    tokenizeStruct(code[i], tokens);

    j:=0;

    if tokens.count>0 then
    begin
      //first check if it's a label definition
      if tokens[0][length(tokens[0])]=':' then
      begin
        elementname:=copy(tokens[0], 1, Length(tokens[0])-1);
        if GetOpcodesIndex(elementname)<>-1 then
          structError(format(rsAAIsAReservedWord, [elementname]));

        elements.AddObject(elementname, tobject(currentOffset));

        j:=1;
      end;



      //then check if it's the end of the struct
      if (uppercase(tokens[0])='ENDSTRUCT') or (uppercase(tokens[0])='ENDS') then
      begin
        endfound:=true;
        break; //all done
      end;
    end;


    //if it's neither a label or structure end then it's a size defining token

    while j < tokens.count do
    begin
      tokens[j]:=uppercase(tokens[j]);

      case tokens[j][1] of
        'R':
        begin
          //could be res*
          if (length(tokens[j])=4) and (copy(tokens[j],1,3)='RES') then
          begin
            case tokens[j][4] of
              'B': bytesize:=1;
              'W': bytesize:=2;
              'D': bytesize:=4;
              'Q': bytesize:=8;
              else
                StructError;
            end;
            //now get the count
            inc(j);
            if j>=tokens.count then
              structError;

            try
              inc(currentOffset, bytesize*StrToInt(tokens[j]));
            except
              structerror;
            end;
          end else structerror;
        end;

        'D':
        begin
          //could be d* ?
          if length(tokens[j])=2 then
          begin
            case tokens[j][2] of
              'B': bytesize:=1;
              'W': bytesize:=2;
              'D': bytesize:=4;
              'Q': bytesize:=8;
              else
                StructError;
            end;

            inc(j);
            if j>=tokens.count then
              structError;

            inc(currentOffset, bytesize);

            //check if there are more ?'s after this (in case of dw ? ? ?)
            while j<tokens.count-1 do
            begin
              if tokens[j+1]='?' then  //check from the spot in front
              begin
                inc(currentOffset, bytesize);
                inc(j);
              end
              else
                break; //nope
            end;

          end else structerror;
        end;

        else
          structError(format(rsAANoIdeaWhatXis, [tokens[j]])); //we already dealth with labels, so this is wrong
      end;


      inc(j); //next token
    end;


  end;

  if endfound=false then
    structerror(rsAANoEndFound);

  //the elements have been filled in, delete the structure (between linenr and lastlinenr) and inject define(element,offset) and define(structname.element,offset)
  for i:=lastlinenr downto linenr do
    code.Delete(i);

  for i:=0 to elements.count-1 do
  begin
    code.Insert(linenr,'define('+elements[i]+','+inttohex(ptruint(elements.objects[i]),1)+')');
    code.Insert(linenr,'define('+structname+'.'+elements[i]+','+inttohex(ptruint(elements.objects[i]),1)+')');
  end;


  code.insert(linenr, 'define('+structname+'_size,'+inttohex(currentOffset,1)+')');



  tokens.Free;
  elements.free;



end;


procedure getPotentialLabels(code: Tstrings; labels: TStrings);
//parse the script for xxxx: lines and store them in labels
//this list gets used when it's about to error out on an undefined symbol, and if found in here, add it as a label

//pre: called after comments are removed
var
  i: integer;
  currentline: string;
begin
  for i:=0 to code.count-1 do
  begin
    currentline:=trim(code[i]);
    if (currentline<>'') and (currentline[length(currentline)]=':') then
    begin
      if (pos('+', currentline)=0) and (pos('.', currentline)=0) then
        labels.add(copy(currentline,1,length(currentline)-1));
    end;
  end;
end;

procedure removecomments(code: tstrings);
var i,j: integer;
    currentline: string;
    instring: boolean;
    incomment: boolean;
    bracecomment: boolean;
begin
  //remove comments


  instring:=false;
  incomment:=false;
  bracecomment:=false;
  for i:=0 to code.count-1 do
  begin
    currentline:=code[i];

    for j:=1 to length(currentline) do
    begin
      if incomment then
      begin


        //inside a comment, remove everything till a } is encountered
        if (bracecomment and ((currentline[j]='}') and (processhandler.SystemArchitecture<>archArm))) or
           ((not bracecomment) and (currentline[j]='*') and (j<length(currentline)) and (currentline[j+1]='/')) then
        begin
          incomment:=false; //and continue parsing the code...

          if (not bracecomment) then
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

          if ((currentline[j]='{') and (processhandler.SystemArchitecture<>archArm)) or
             ((currentline[j]='/') and (j<length(currentline)) and (currentline[j+1]='*')) then
          begin
            incomment:=true;
            bracecomment:=currentline[j]='{';

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

procedure aobscans(code: tstrings; syntaxcheckonly: boolean);
//Replaces all AOBSCAN lines with DEFINE(NAME,ADDRESS)
type
  TAOBEntry = record
    name: string;
    aobstring: string;
    linenumber: integer;
  end;

  PAOBEntry=^TAOBEntry;

var i,j,k, m: integer;


    aobscanmodules: array of record
      name: string;
      entries: array of TAOBEntry;
      minaddress, maxaddress: ptruint;
      memscan: TMemScan;
    end;

    a,b,c,d,e: integer;
    currentline: string;
    s1,s2, s3,s4: string;
    testptr: ptruint;

    cpucount: integer;
    threads: integer;
    mi: TModuleInfo;
    aobstrings: string;

    error: boolean;
    errorstring: string;

    aob1, aob2: dword;

    startaddress, stopaddress: ptruint;

  procedure finished(f: integer);
  //cleanup a memscan and fill in the results
  var i,j: integer;
      results: TAddresses;
      aoblist: string;
  begin
    setlength(results,0);
    if length(aobscanmodules[f].entries)=1 then
    begin
      //if only 1 entry a normal aobscan was done, so use GetOnlyOneResult instead
      if aobscanmodules[f].memscan.GetOnlyOneResult(testptr) then
      begin
        setlength(results,1);
        results[0]:=testptr;
      end;
    end
    else
      aobscanmodules[f].memscan.GetOnlyOneResults(results);

    if length(results)=length(aobscanmodules[f].entries) then
    begin
      for i:=0 to length(aobscanmodules[f].entries)-1 do
      begin
        if results[i]=0 then
        begin
          error:=true;
          errorstring:=format(rsAATheArrayOfByteNamed, [aobscanmodules[f].entries[i].name]);
        end
        else
          code[aobscanmodules[f].entries[i].linenumber]:='DEFINE('+aobscanmodules[f].entries[i].name+', '+inttohex(results[i],8)+')';
      end;
    end
    else
    begin
      error:=true;
      aoblist:='';
      for i:=0 to length(aobscanmodules[f].entries)-1 do
        aoblist:=aoblist+aobscanmodules[f].entries[i].name+' ';

      if aobscanmodules[f].memscan.GetErrorString<>'' then
        errorstring:=rsAAErrorWhileSacnningForAobs+aoblist+#13#10#13#10+rsAAError+aobscanmodules[f].memscan.GetErrorString
      else
        errorstring:=rsAAErrorWhileSacnningForAobs+aoblist+#13#10#13#10+rsAAError+'Not all results found';


    end;

    aobscanmodules[f].memscan.Free;
    aobscanmodules[f].memscan:=nil;

    dec(threads);
  end;

begin
  error:=false;
  setlength(aobscanmodules,0);


  cpucount:=GetCPUCount;
  threads:=0;

  for i:=0 to code.Count-1 do
  begin
    //AOBSCAN(variable,aobtring)  (works like define)
    currentline:=code[i];

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

          //find the '' module
          m:=-1;
          for j:=0 to length(aobscanmodules)-1 do
            if aobscanmodules[j].name='' then
            begin
              m:=j;
              break;
            end;

          if m=-1 then
          begin
            setlength(aobscanmodules, length(aobscanmodules)+1);
            m:=length(aobscanmodules)-1;

            aobscanmodules[m].name:='';
            aobscanmodules[m].minaddress:=0;

            {$ifdef cpu64}
            if processhandler.is64Bit then
              aobscanmodules[m].maxaddress:=qword($7fffffffffffffff)
            else
            {$endif}
            begin
              if Is64bitOS then
                aobscanmodules[m].maxaddress:=$ffffffff
              else
                aobscanmodules[m].maxaddress:=$7fffffff;
            end;

            aobscanmodules[m].maxaddress:=qword($ffffffffffffffff);
            setlength(aobscanmodules[m].entries,0); //shouldn't be needed, but do it anyhow
          end;

          j:=length(aobscanmodules[m].entries);
          setlength(aobscanmodules[m].entries, j+1);
          aobscanmodules[m].entries[j].name:=s1;
          aobscanmodules[m].entries[j].aobstring:=s2;
          aobscanmodules[m].entries[j].linenumber:=i;
        end
        else
          code[i]:='DEFINE('+s1+', 00000000)';


      end else raise exception.Create(rsWrongSyntaxAOBSCANName11223355);
    end;

    if uppercase(copy(currentline,1,14))='AOBSCANMODULE(' then //AOBSCANMODULE(varname, modulename, bytestring)
    begin
      a:=pos('(',currentline);
      b:=pos(',',currentline);
      c:=PosEx(',',currentline,b+1);
      d:=pos(')',currentline);

      if d<=a then raise exception.create(rsWrongSyntaxAOBSCANMODULEName11223355);



      if (a>0) and (b>0) and (c>0) then
      begin
        s1:=trim(copy(currentline,a+1,b-a-1));
        s2:=trim(copy(currentline,b+1,c-b-1));
        s3:=trim(copy(currentline,c+1,d-c-1));

        //s1=varname
        //s2=MODULE
        //s3=aob
        testPtr:=0;
        if (not syntaxcheckonly) then
        begin
          //find the s2 module
          m:=-1;
          for j:=0 to length(aobscanmodules)-1 do
            if aobscanmodules[j].name=uppercase(s2) then
            begin
              m:=j;
              break;
            end;

          if m=-1 then
          begin
            setlength(aobscanmodules, length(aobscanmodules)+1);
            m:=length(aobscanmodules)-1;

            aobscanmodules[m].name:=uppercase(s2);
            if symhandler.getmodulebyname(s2, mi) then
            begin
              aobscanmodules[m].minaddress:=mi.baseaddress;
              aobscanmodules[m].maxaddress:=mi.baseaddress+mi.basesize;
            end
            else
            begin
              //modulename not found. Perhaps a symbol was used
              try
                testptr:=symhandler.getAddressFromName(s2);
                if symhandler.getmodulebyaddress(testptr, mi) then
                begin
                  aobscanmodules[m].minaddress:=mi.baseaddress;
                  aobscanmodules[m].maxaddress:=mi.baseaddress+mi.basesize;
                end;
              except
                raise exception.create(rsAAModuleNotFound+s2);
              end;
            end;

            setlength(aobscanmodules[m].entries,0);
          end;

          j:=length(aobscanmodules[m].entries);
          setlength(aobscanmodules[m].entries, j+1);
          aobscanmodules[m].entries[j].name:=s1;
          aobscanmodules[m].entries[j].aobstring:=s3;
          aobscanmodules[m].entries[j].linenumber:=i;
        end
        else
          code[i]:='DEFINE('+s1+', 00000000)';

      end else raise exception.Create(rsWrongSyntaxAOBSCANMODULEName11223355);
    end;

    if uppercase(copy(currentline,1,14))='AOBSCANREGION(' then //AOBSCANREGION(varname, startaddress, stopaddress, bytestring)
    begin
      a:=pos('(',currentline);
      b:=pos(',',currentline);
      c:=PosEx(',',currentline,b+1);
      d:=PosEx(',',currentline,c+1);
      e:=pos(')',currentline);

      if (d<=a) or (b<=a) or (c<=a) or (d<=a) or (e<=a) then raise exception.create(rsWrongSyntaxAOBSCANREGION);

      s1:=trim(copy(currentline,a+1,b-a-1));
      s2:=trim(copy(currentline,b+1,c-b-1));
      s3:=trim(copy(currentline,c+1,d-c-1));
      s4:=trim(copy(currentline,d+1,e-d-1));

      if (not syntaxcheckonly) then
      begin
        startaddress:=symhandler.getAddressFromName(s2);
        stopaddress:=symhandler.getAddressFromName(s3);

        //see if this region is already being scanned
        m:=-1;
        for j:=0 to length(aobscanmodules)-1 do
        begin
          //exact address only. No widening/appending of the groups is possible (Users may want to scan for 00 00 in a 16 byte region)
          if (startaddress=aobscanmodules[j].minaddress) and (stopaddress=aobscanmodules[j].maxaddress) then
          begin
            m:=j;
            break;
          end;
        end;

        if m=-1 then
        begin
          setlength(aobscanmodules, length(aobscanmodules)+1);
          m:=length(aobscanmodules)-1;
          aobscanmodules[m].name:='<REGION>';
          aobscanmodules[m].minaddress:=startaddress;
          aobscanmodules[m].maxaddress:=stopaddress;
          setlength(aobscanmodules[m].entries,0);
        end;

        j:=length(aobscanmodules[m].entries);
        setlength(aobscanmodules[m].entries, j+1);
        aobscanmodules[m].entries[j].name:=s1;
        aobscanmodules[m].entries[j].aobstring:=s4;
        aobscanmodules[m].entries[j].linenumber:=i;
      end
      else
        code[i]:='DEFINE('+s1+', 00000000)';

    end;
  end;
  //do simultaneous scans for the selected modules
  for i:=0 to length(aobscanmodules)-1 do
  begin

      //wait for one to finish
    j:=0;
    while threads>=cpucount do
    begin
      if (aobscanmodules[j].memscan<>nil) and (aobscanmodules[j].memscan.waittilldone(50)) then
      begin
        finished(j);
        break;
      end;

      j:=(j+1) mod i;
    end;

    inc(threads);
    aobscanmodules[i].memscan:=TMemScan.create(nil);
    aobscanmodules[i].memscan.OnlyOne:=true;

    aobstrings:='';
    for j:=0 to length(aobscanmodules[i].entries)-1 do
      aobstrings:=aobstrings+'('+aobscanmodules[i].entries[j].aobstring+')';

    if length(aobscanmodules[i].entries)=1 then //bytearrays is slightly slower, so only use it if more than one entry is to be scanned
      aobscanmodules[i].memscan.firstscan(soExactValue, vtByteArray, rtRounded, aobscanmodules[i].entries[0].aobstring, '', aobscanmodules[i].minaddress, aobscanmodules[i].maxaddress, true, false, false, false, fsmNotAligned)
    else
      aobscanmodules[i].memscan.firstscan(soExactValue, vtByteArrays, rtRounded, aobstrings, '', aobscanmodules[i].minaddress, aobscanmodules[i].maxaddress, true, false, false, false, fsmNotAligned);
  end;

  //now wait till all are finished
  for i:=0 to length(aobscanmodules)-1 do
    if aobscanmodules[i].memscan<>nil then
    begin
      aobscanmodules[i].memscan.waittilldone;
      finished(i);
    end;


  if error then raise exception.create(errorstring);


end;



procedure parseTryExcept(code: tstrings; var exceptionlist: TAAExceptionInfoList);
//Find and replace {$TRY} , {$EXCEPT} with labels
var
  i,j: integer;
  trynr: integer;
  trylist: array of record
    linenr: integer;
    trynr: integer;
    hasexcept: boolean;
    trylabel, exceptlabel: string;
  end;

  found: boolean;
begin
  trynr:=0;
  setlength(trylist,0);

  for i:=0 to code.Count-1 do
  begin
    if uppercase(code[i])='{$TRY}' then
    begin
      inc(trynr);

      j:=length(trylist);
      setlength(trylist,j+1);
      trylist[j].trynr:=trynr;
      trylist[j].hasexcept:=false;
      trylist[j].linenr:=integer(code.Objects[i]);
      trylist[j].trylabel:='tryoperation_'+inttostr(trynr);
      code[i]:=trylist[j].trylabel+':';
    end;

    if uppercase(code[i])='{$EXCEPT}' then
    begin
      //find the last try that doesn't have an except filled in
      found:=false;
      for j:=length(trylist)-1 downto 0 do
      begin
        if trylist[j].hasexcept=false then
        begin
          trylist[j].hasexcept:=true;
          trylist[j].exceptlabel:='tryoperation'+inttostr(trylist[j].trynr)+'_except';
          code[i]:=trylist[j].exceptlabel+':';
          found:=true;
          break;
        end;
      end;

      if not found then
        raise exception.create(format('Found an {$EXCEPT} at line %d with no matching {$TRY}',[integer(code.Objects[i])]));
    end;
  end;

  setlength(exceptionlist, length(trylist));

  for i:=0 to length(trylist)-1 do
  begin
    code.Insert(0,'label('+trylist[i].trylabel+')');
    code.Insert(0,'label('+trylist[i].exceptlabel+')');
    exceptionlist[i].trylabel:=trylist[i].trylabel;
    exceptionlist[i].exceptlabel:=trylist[i].exceptlabel;

    if trylist[i].hasexcept=false then raise exception.create(format(rsMissingExcept, [trylist[i].linenr]));
  end;
end;

procedure luacode(code: TStrings; syntaxcheckonly: boolean; memrec: TMemoryRecord=nil);
{
Find and execute the LUA parts:
function (syntaxcheck)
  <code>
end

If the functions return a string, substitute the code with the given string
}
var
  i,j,k: integer;
  s: tstringlist;
  stack: integer;
  str: string;
  error: boolean;
  L: Plua_State;
begin
  i:=0;

  while i<code.Count do
  begin
    //search for {$LUA}

    str:=uppercase(TrimRight(code[i]));
    if str='{$LUA}' then
    begin
      //search for {$ASM} or the end
      j:=i+1;
      while j<=code.count do
      begin

        if (j=code.count) or (uppercase(TrimRight(code[j]))='{$ASM}') then
        begin
          s:=TStringList.create;

          s.add('local syntaxcheck,memrec=...');

          code[i]:='';
          for k:=i+1 to j-1 do
          begin
            s.Add(code[k]);
            code[k]:=''; //empty
          end;

          if j<>code.count then
            code[j]:='';


{$ifndef NOLUA}
          L:=GetLuaState;


          try
            stack:=lua_Gettop(L);

            error:=false;

            luaL_loadstring(L, pchar(s.text));
            if lua_isfunction(L, -1) then
            begin
              lua_pushboolean(L, syntaxcheckonly);
              luaclass_newClass(L, memrec);

              if lua.lua_pcall(L, 2, 1, 0)=0 then
              begin
                if lua_isstring(L,-1) then
                begin
                  str:=Lua_ToString(L, -1);
                  s:=tstringlist.create;
                  s.text:=str;

                  for k:=0 to s.count-1 do
                    code.Insert(i+k, s[k]);

                  s.free;

                end;
              end
              else
                error:=true;
            end
            else
              error:=true;

            if error then
            begin
              if lua_isstring(L, -1) then
                raise exception.create(rsAALuaErrorInTheScriptAtLine+inttostr(integer(code.Objects[i]))+':'+lua_tostring(L, -1))
              else
                raise exception.create(rsAALuaErrorInTheScriptAtLine+inttostr(integer(code.Objects[i])));

            end;

          finally
            lua_settop(L, stack);
          end;
{$endif}
          break;

        end;
        inc(j);
      end;


    end
    else
      inc(i);
  end;

  //one more time getting rid of {$ASM} lines that have been added while they shouldn't be required
  for i:=0 to code.count-1 do
    if uppercase(TrimRight(code[i]))='{$ASM}' then
      code[i]:='';

end;


var nextaaid: longint;

function autoassemble2(code: tstrings;popupmessages: boolean;syntaxcheckonly:boolean; targetself: boolean ;var ceallocarray:TCEAllocArray; var ceexceptionlist: TCEExceptionListArray; registeredsymbols: tstringlist=nil; memrec: TMemoryRecord=nil):boolean;
{
registeredsymbols is a stringlist that is initialized by the caller as case insensitive and no duplicates
}

type tassembled=record
  address: ptrUint;
  bytes: TAssemblerbytes;
  createthreadandwait: integer;
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
    currentline, currentline2: string;
    currentlinenr: integer;
    currentlinep: pchar;

    currentaddress: ptrUint;
    assembled: array of tassembled;
    x: ptruint;
    y,op,op2:dword;
    ok1,ok2:boolean;
    loadbinary: array of record
      address: string; //string since it might be a label/alloc/define
      filename: string;
    end;

    readmems: array of record
      bytelength: integer;
      bytes: PByteArray;
    end;

    globalallocs, allocs, kallocs, sallocs: array of tcealloc;
    labels: array of tlabel;
    defines: array of tdefine;
    fullaccess: array of tfullaccess;
    dealloc: array of PtrUInt;
    addsymbollist: array of string;
    deletesymbollist: array of string;
    createthread: array of string;

    createthreadandwait: array of record
      name: string;
      position: integer; //after what position should the call happen (This is so that the exception handlers can be registered before the final hookcode is written)
    end;

//    aoblist: array of TAOBEntry;

    a,b,c,d: integer;
    s1,s2,s3: string;
    diff: ptruint;


    assemblerlines: array of record
      linenr: integer;
      line: string;
    end;

    exceptionlist: TAAExceptionInfoList;

    varsize: integer;
    tokens: tstringlist;
    baseaddress: ptrUint;

    multilineinjection: tstringlist;
    include: tstringlist;
    testdword,bw: dword;
    testPtr: ptrUint;
    binaryfile: tmemorystream;

    incomment: boolean;

    bytebuf: PByteArray;

    processhandle: THandle;
    ProcessID: DWORD;

    bytes: tbytes;
    prefered: ptrUint;
    protection: dword;

    oldhandle: thandle;
    oldsymhandler: TSymHandler;


    disassembler: TDisassembler;

    threadhandle: THandle;

    potentiallabels: TStringlist;


    connection: TCEConnection;

    mi: TModuleInfo;
    aaid: longint;
    strictmode: boolean;

    hastryexcept: boolean;
    createthreadandwaitid: integer;

    vpe: boolean;

    nops: Tassemblerbytes;
    mustbefar: boolean;

    function getAddressFromScript(name: string): ptruint;
    var
      found: boolean;
      j: integer;
    begin
      found:=false;
      try
        result:=symhandler.getAddressFromName(name);
        exit;
      except
      end;

      name:=uppercase(name);

      for j:=0 to length(labels)-1 do
        if uppercase(labels[j].labelname)=name then
          exit(labels[j].address);

      for j:=0 to length(allocs)-1 do
        if uppercase(allocs[j].varname)=name then
          exit(allocs[j].address);

      for j:=0 to length(kallocs)-1 do
         if uppercase(kallocs[j].varname)=name then
           exit(kallocs[j].address);

      for j:=0 to length(defines)-1 do
        if uppercase(defines[j].name)=name then
        begin
          try
            testptr:=symhandler.getAddressFromName(defines[j].whatever);
            exit;
          except
          end;
        end;
    end;

begin
  setlength(readmems,0);
  setlength(allocs,0);
  setlength(kallocs,0);
  setlength(globalallocs,0);
  setlength(sallocs,0);
  setlength(createthread,0);
  setlength(createthreadandwait,0);

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

  {$ifndef jni}
  if targetself then
  begin
    //get this function to use the symbolhandler that's pointing to CE itself and the self processid/handle
    oldhandle:=processhandlerunit.ProcessHandle;
    processid:=getcurrentprocessid;
    processhandle:=getcurrentprocess;
    oldsymhandler:=symhandler;
    symhandler:=selfsymhandler;
    processhandler.processhandle:=processhandle;
  end
  else
  {$endif}
  begin
    processid:=processhandlerunit.ProcessID;
    processhandle:=processhandlerunit.ProcessHandle;
  end;

  symhandler.waitforsymbolsloaded(true);

{$ifndef jni}
  if pluginhandler=nil then exit; //Error. Cheat Engine is not properly configured

  aaid:=InterLockedIncrement(nextaaid);
  pluginhandler.handleAutoAssemblerPlugin(@currentlinep, 0, aaid); //tell the plugins that an autoassembler script is about to get executed
{$endif}


  potentiallabels:=tstringlist.create;
  potentiallabels.CaseSensitive:=false;

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
    setlength(exceptionlist,0);
//    setlength(aoblist,0);

    tokens:=tstringlist.Create;

    incomment:=false;

    if not targetself then
      for i:=0 to length(AutoAssemblerPrologues)-1 do
        if assigned(AutoAssemblerPrologues[i]) then
          AutoAssemblerPrologues[i](code, syntaxcheckonly);

    luacode(code, syntaxcheckonly, memrec);

    //still here

    strictmode:=false;
    for i:=0 to code.count-1 do
    begin
      currentline:=uppercase(TrimRight(code[i]));
      if currentline='{$STRICT}' then
        strictmode:=true;

      if currentline='{$TRY}' then
        hastryexcept:=true;
    end;


    if hastryexcept then
      parseTryExcept(code, exceptionlist);


    removecomments(code);  //also trims each line
    unlabeledlabels(code);

    if not strictmode then
      getPotentialLabels(code, potentiallabels);


    //6.3: do the aobscans first
    //this will break scripts that use define(state,33) aobscan(name, 11 22 state 44 55), but really, live with it

    aobscans(code, syntaxcheckonly);

    if not targetself then
      for i:=0 to length(AutoAssemblerProloguesPostAOBSCAN)-1 do
        if assigned(AutoAssemblerProloguesPostAOBSCAN[i]) then
          AutoAssemblerProloguesPostAOBSCAN[i](code, syntaxcheckonly);

    //first pass

    i:=0;
    while i<code.Count do
    begin
      try
        try
          currentline:=code[i];
          currentlinenr:=ptrUint(code.Objects[i]);

          //check if useless
          if length(currentline)=0 then continue;
          if copy(currentline,1,2)='//' then continue; //skip



          //do this first. Do not touch registersymbol with any kind of define/label/whatsoever
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

            continue;
          end;



          //apply defines (before DEFINE since define(bla, 123) and define(xxx, bla+123) should work


          //also, do not touch define with any previous define
          if uppercase(copy(currentline,1,7))='DEFINE(' then
          begin
            //syntax: alloc(x,size)    x=variable name size=bytes
            //allocate memory



            a:=pos('(',currentline);
            b:=pos(',',currentline);
            c:=rpos(')',currentline);
            if (a>0) and (b>0) and (c>0) then
            begin
              s1:=trim(copy(currentline,a+1,b-a-1));
              s2:=copy(currentline,b+1,c-b-1);


              //apply earlier defines to the second part
              for j:=0 to length(defines)-1 do
                 s2:=replacetoken(s2,defines[j].name,defines[j].whatever);



              ok1:=true;
              for j:=0 to length(defines)-1 do
                if uppercase(defines[j].name)=uppercase(s1) then
                begin
                  //redefined from here on
                  ok1:=false;
                  defines[length(defines)-1].whatever:=s2
                end;

              if ok1 then //not duplicate, create it
              begin
                setlength(defines,length(defines)+1);
                defines[length(defines)-1].name:=s1;
                defines[length(defines)-1].whatever:=s2;
              end;

              continue;
            end else raise exception.Create(rsWrongSyntaxDEFINENameWhatever+' Got '+currentline);
          end;


          //normal loop code

          for j:=0 to length(defines)-1 do
             currentline:=replacetoken(currentline,defines[j].name,defines[j].whatever);



          setlength(assemblerlines,length(assemblerlines)+1);
          assemblerlines[length(assemblerlines)-1].linenr:=currentlinenr;
          assemblerlines[length(assemblerlines)-1].line:=currentline;

          //plugins
          currentlinep:=@currentline[1];
          {$ifndef jni}
          pluginhandler.handleAutoAssemblerPlugin(@currentlinep, 1,aaid);
          {$endif}
          currentline:=currentlinep;

          //lua extensions
          if registeredAutoAssemblerCommands<>nil then
          begin
            j:=pos('(', currentline);
            if j>0 then
            begin
              s1:=uppercase(copy(currentline, 1, j-1));
              for j:=0 to registeredAutoAssemblerCommands.count-1 do
              begin
                if TRegisteredAutoAssemblerCommand(registeredAutoAssemblerCommands[j]).command=s1 then
                begin
                  a:=pos('(',currentline);
                  b:=RPos(')',currentline);
                  s1:=copy(currentline, a+1, b-a-1);

                  currentline:=TRegisteredAutoAssemblerCommand(registeredAutoAssemblerCommands[j]).callback(s1, syntaxcheckonly);

                  //insert the current text as lines into the codelist
                  multilineinjection:=TStringList.create;
                  try
                    multilineinjection.Text:=currentline;

                    for k:=0 to multilineinjection.Count-1 do
                      code.InsertObject(i+1+k, multilineinjection[k], pointer(ptruint(currentlinenr)));
                  finally
                    multilineinjection.Free;
                  end;

                  //showmessage(code.text);

                  currentline:='';
                  break;
                end;
              end;
            end;
          end;

          //if the newline is empty then it has been handled and the plugin doesn't want it to be added for phase2
          if length(currentline)=0 then
          begin
            setlength(assemblerlines,length(assemblerlines)-1);
            continue;
          end;
          //otherwise it hasn't been handled, or it has been handled and the string is a compatible string that passes the phase1 tests (so variablenames converted to 00000000 and whatever else is needed)

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
                    freememandnil(bytebuf);

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


              try
                x:=strtoint(s2);
              except
                raise exception.Create(Format(rsIsNotAValidSize, [s2]));
              end;

              //define it here already
              if s3<>'' then
                symhandler.SetUserdefinedSymbolAllocSize(s1,x, symhandler.getAddressFromName(s3))
              else
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
                include.LoadFromFile(s1, true);
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

          if uppercase(copy(currentline,1,12))='CREATETHREAD' then
          begin
            if currentline[13]='(' then //CREATETHREAD(
            begin
              //create a thread
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
            end
            else
            begin
              //could be createthreadandwait
              if uppercase(copy(currentline,13,8))='ANDWAIT(' then //CREATETHREADANDWAIT(
              begin
                a:=pos('(',currentline);
                b:=pos(')',currentline);
                if (a>0) and (b>0) then
                begin
                  s1:=trim(copy(currentline,a+1,b-a-1));

                  setlength(createthreadandwait,length(createthreadandwait)+1);
                  createthreadandwait[length(createthreadandwait)-1].name:=s1;
                  createthreadandwait[length(createthreadandwait)-1].position:=length(assemblerlines)-1;

                  setlength(assemblerlines,length(assemblerlines)-1);
                  continue;
                end else raise exception.Create(rsWrongSyntaxCreateThreadAddress);
              end;
            end;
          end;



          {$ifndef jni}

          if uppercase(copy(currentline,1,12))='LOADLIBRARY(' then
          begin
            //load a library into memory , this one already executes BEFORE the 2nd pass to get addressnames correct
            a:=pos('(',currentline);
            b:=pos(')',currentline);

            if (a>0) and (b>0) then
            begin
              s1:=trim(copy(currentline,a+1,b-a-1));

              if (length(s1)>1) and ((s1[1]='''') or (s1[1]='"')) then
                s1:=AnsiDequotedStr(s1,s1[1]);

              if pos(':',s1)=0 then
              begin
                s2:=extractfilename(s1);

                if getConnection=nil then //no connection, so local. Check if the file can be found locally and if so, set the specific path
                begin
                  if fileexists(cheatenginedir+s2) then s1:=cheatenginedir+s2 else
                    if fileexists(getcurrentdir+'\'+s2) then s1:=getcurrentdir+'\'+s2 else
                      if fileexists(cheatenginedir+s1) then s1:=cheatenginedir+s1;
                end;

                //else just hope it's in the dll searchpath
              end; //else direct file path

              try
                if symhandler.getmodulebyname(extractfilename(s1), mi)=false then //check if it's already injected
                begin
                  InjectDll(s1,'');
                  symhandler.reinitialize;
                end;
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
          {$endif}

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
                if not syntaxcheckonly then
                  raise exception.Create(rsInvalidAddressForReadMem)
                else
                  testptr:=0;
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
                if not syntaxcheckonly then
                begin
                  if (not ReadProcessMemory(processhandle, pointer(testptr),bytebuf,a,x)) or (x<a) then
                    raise exception.Create(Format(rsTheMemoryAtCouldNotBeFullyRead, [s1]));
                end;
              except
                on e:exception do
                begin
                  if bytebuf<>nil then
                  begin
                    freememandnil(bytebuf);

                  end;

                  raise exception.create(e.Message);
                end;
              end;


              //still here so everything ok
              assemblerlines[length(assemblerlines)-1].linenr:=currentlinenr;
              assemblerlines[length(assemblerlines)-1].line:='<READMEM'+IntToStr(length(readmems))+'>';
              setlength(readmems, length(readmems)+1);
              readmems[length(readmems)-1].bytelength:=a;
              readmems[length(readmems)-1].bytes:=bytebuf;
              bytebuf:=nil;

              continue;



            end else raise exception.Create(rsWrongSyntaxReadMemAddressSize);

            continue;
          end;

          if uppercase(copy(currentline,1,11))='REASSEMBLE(' then
          begin
            a:=pos('(',currentline);
            b:=pos(')',currentline);

            if (a>0) and (b>0) then
            begin
              s1:=trim(copy(currentline,a+1,b-a-1));

              try
                testptr:=symhandler.getAddressFromName(s1);
              except
                raise exception.Create(format(rsXCouldNotBeFound, [s1]));
              end;

              disassembler:=TDisassembler.create;
              disassembler.dataOnly:=true;
              disassembler.disassemble(testptr, s1);

              if syntaxcheckonly then currentline:='nop' else
                currentline:=disassembler.LastDisassembleData.prefix+' '+Disassembler.LastDisassembleData.opcode+' '+disassembler.LastDisassembleData.parameters;;

              assemblerlines[length(assemblerlines)-1].linenr:=currentlinenr;
              assemblerlines[length(assemblerlines)-1].line:=currentline;
              disassembler.free;
            end else raise exception.Create(rsWrongSyntaxReAssemble);

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

          //AOBSCAN used to live here, but he moved up

          //define
          if uppercase(copy(currentline,1,7))='STRUCT ' then
          begin
            replaceStructWithDefines(code, i);
            setlength(assemblerlines,length(assemblerlines)-1);
            dec(i); //repeat from this line
            continue;
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

              j:=0;
              while (j<length(labels)) and (length(labels[j].labelname)>=varsize) do
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
          if (uppercase(copy(currentline,1,5))='ALLOC') and
             (
               (uppercase(copy(currentline,1,6))='ALLOC(') or
               (uppercase(copy(currentline,1,8))='ALLOCNX(') or
               (uppercase(copy(currentline,1,8))='ALLOCXO(')
             )
          then
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

              allocs[j].protection:=PAGE_EXECUTE_READWRITE;
              if uppercase(copy(currentline,1,8))='ALLOCNX(' then
                allocs[j].protection:=PAGE_READWRITE
              else
              if uppercase(copy(currentline,1,8))='ALLOCXO(' then
                allocs[j].protection:=PAGE_EXECUTE_READ;


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


              //still here, so more complex
              if syntaxcheckonly and (registeredsymbols<>nil) then
              begin
                //replace tokens with registered symbols from the enable part
                for j:=0 to registeredsymbols.count-1 do
                  currentline:=replacetoken(currentline, registeredsymbols[j], '00000000');
              end;

              try
                s1:=copy(currentline,1,length(currentline)-1);

                if s1<>'' then
                  testPtr:=symhandler.getAddressFromName(s1);


              except
                currentline:=inttohex(symhandler.getaddressfromname(copy(currentline,1,length(currentline)-1)),8)+':';
                assemblerlines[length(assemblerlines)-1].linenr:=currentlinenr;
                assemblerlines[length(assemblerlines)-1].line:=currentline;
              end;


            except
              //add this as a label if a potential label
              if potentiallabels.IndexOf(copy(currentline,1,length(currentline)-1))=-1 then
                raise symexception.Create(rsThisAddressSpecifierIsNotValid);

              j:=length(labels);
              setlength(labels,j+1);

              labels[j].labelname:=copy(currentline,1,length(currentline)-1);
              labels[j].assemblerline:=length(assemblerlines)-1;
              labels[j].defined:=false;

              setlength(labels[j].references,0);
              setlength(labels[j].references2,0);

              //setlength(assemblerlines, length(assemblerlines)-1);
//              assemblerlines[length(assemblerlines)-1]:='';

//              continue;

              //raise exception.Create(rsThisAddressSpecifierIsNotValid);
            end;

            continue; //next line
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
            ok1:=false;
            try
              ok1:=assemble(currentline,currentaddress,assembled[0].bytes, apNone, true);
            except
            end;

            if not ok1 then //the instruction could not be assembled as it is right now
            begin
              //try potential labels
              ok1:=false;

              for j:=0 to potentiallabels.count-1 do
              begin
                if processhandler.is64bit then
                  currentline:=replacetoken(currentline,potentiallabels[j],'ffffffffffffffff')
                else
                  currentline:=replacetoken(currentline,potentiallabels[j],'00000000');

                try
                  ok1:=assemble(currentline,currentaddress,assembled[0].bytes, apNone, true);
                  if ok1 then
                  begin
                    //define this potential label as a full label
                    k:=length(labels);
                    setlength(labels, k+1);
                    labels[k].labelname:=potentiallabels[j];
                    labels[k].defined:=false;
                    setlength(labels[k].references,0);
                    setlength(labels[k].references2,0);

                    break;
                  end;
                except
                  //don't quit yet
                end;
              end;


              if not ok1 then
                raise EAutoAssembler.Create('bla');

            end;
          except
            raise EAutoAssembler.Create(rsThisInstructionCanTBeCompiled);
          end;

        finally
          inc(i);
        end;

      except
        on E:exception do
          raise EAutoAssembler.Create(Format(rsErrorInLine, [IntToStr(currentlinenr), currentline, e.Message]));

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

        if not ok1 then //scan defines
          for j:=0 to length(defines)-1 do
            if uppercase(addsymbollist[i])=uppercase(defines[j].name) then
            begin
              ok1:=true;
              break;
            end;

        if not ok1 then raise EAssemblerException.create(Format(rsWasSupposedToBeAddedToTheSymbollistButItIsnTDeclar, [addsymbollist[i]]));
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

        if not ok1 then raise EAssemblerException.create(Format(rsTheAddressInCreatethreadIsNotValid, [createthread[i]]));

      end;

    if length(createthreadandwait)>0 then
      for i:=0 to length(createthreadandwait)-1 do
      begin
        ok1:=true;

        try
          testptr:=symhandler.getAddressFromName(createthreadandwait[i].name);
        except
          ok1:=false;
        end;

        if not ok1 then
          for j:=0 to length(labels)-1 do
            if uppercase(labels[j].labelname)=uppercase(createthreadandwait[i].name) then
            begin
              ok1:=true;
              break;
            end;

        if not ok1 then
          for j:=0 to length(allocs)-1 do
            if uppercase(allocs[j].varname)=uppercase(createthreadandwait[i].name) then
            begin
              ok1:=true;
              break;
            end;

        {$ifndef net}
        if not ok1 then
          for j:=0 to length(kallocs)-1 do
            if uppercase(kallocs[j].varname)=uppercase(createthreadandwait[i].name) then
            begin
              ok1:=true;
              break;
            end;
        {$endif}

        if not ok1 then
          for j:=0 to length(defines)-1 do
            if uppercase(defines[j].name)=uppercase(createthreadandwait[i].name) then
            begin
              try
                testptr:=symhandler.getAddressFromName(defines[j].whatever);
                ok1:=true;
              except
              end;
              break;
            end;

        if not ok1 then raise EAssemblerException.create(Format(rsTheAddressInCreatethreadAndWaitIsNotValid, [createthread[i]]));

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

        if not ok1 then raise EAssemblerException.create(Format(rsTheAddressInLoadbinaryIsNotValid, [loadbinary[i].address, loadbinary[i].filename]));

      end;


    if syntaxcheckonly then
    begin
      result:=true;
      exit;
    end;

    {$ifndef jni}
    if popupmessages and (messagedlg(rsThisCodeCanBeInjectedAreYouSure, mtConfirmation	, [mbyes, mbno], 0)<>mryes) then exit;
    {$endif}

    //allocate the memory

    if length(allocs)>0 then
    begin

      j:=0; //entry to go from
      prefered:=allocs[0].prefered;
      protection:=allocs[0].protection;
      x:=allocs[0].size;

      for i:=1 to length(allocs)-1 do
      begin
        //does this entry have a prefered location or a non default protection

        if (allocs[i].prefered<>0) or (allocs[i].protection<>PAGE_EXECUTE_READWRITE) then
        begin
          //if yes, is it the same as the previous entry? (or was the previous one that doesn't care?)
          if prefered=0 then
            prefered:=allocs[i].prefered;

          if (prefered<>allocs[i].prefered) or (protection<>allocs[i].protection) then
          begin
            //different prefered address or protection

            if x>0 then //it has some previous entries with compatible locations
            begin
              k:=10;
              allocs[j].address:=0;
              while (k>0) and (allocs[j].address=0) do
              begin
                //try allocating until a memory region has been found (e.g due to quick allocating by the game)

                if (prefered=0) and (j>0) then //if not a prefered address but there is a previous alloc, allocate near there
                  prefered:=allocs[j-1].address;

                prefered:=ptrUint(FindFreeBlockForRegion(prefered,x));

                allocs[j].address:=ptrUint(virtualallocex(processhandle,pointer(prefered),x, MEM_RESERVE or MEM_COMMIT,protection));
                if allocs[j].address=0 then
                begin
                  OutputDebugString(rsFailureToAllocateMemory+' 1');
                  inc(prefered,65536);
                end;

                dec(k);
              end;

              if allocs[j].address=0 then
              begin
                allocs[j].address:=ptrUint(virtualallocex(processhandle,nil,x, MEM_RESERVE or MEM_COMMIT,protection));
                OutputDebugString(rsFailureToAllocateMemory+' 2');
              end;

              //adjust the addresses of entries that are part of this block
              for k:=j+1 to i-1 do
                allocs[k].address:=allocs[k-1].address+allocs[k-1].size;
              x:=0;
            end;

            //new prefered address
            j:=i;
            prefered:=allocs[i].prefered;
            protection:=allocs[i].protection;
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

          if (prefered=0) and (j>0) then //if not a prefered address but there is a previous alloc, allocate near there
            prefered:=allocs[j-1].address;

          prefered:=ptrUint(FindFreeBlockForRegion(prefered,x));


          allocs[j].address:=ptrUint(virtualallocex(processhandle,pointer(prefered),x, MEM_RESERVE or MEM_COMMIT,protection));
          if allocs[j].address=0 then
          begin
            OutputDebugString(rsFailureToAllocateMemory+' 3 (prefered='+inttohex(prefered,8)+')');
            inc(prefered, 65536);
          end;
          dec(k);
        end;

        if allocs[j].address=0 then
          allocs[j].address:=ptrUint(virtualallocex(processhandle,nil,x, MEM_RESERVE or MEM_COMMIT,protection));

        if allocs[j].address=0 then raise EAssemblerException.create(rsFailureToAllocateMemory+' 4');

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
    currentlinenr:=0;
    try
      for i:=0 to length(assemblerlines)-1 do
      begin
        currentline:=assemblerlines[i].line;
        currentlinenr:=assemblerlines[i].linenr;

        createthreadandwaitid:=-1;
        for j:=0 to length(createthreadandwait)-1 do //there can be multiple at the time of assembly.  All entries up to the higest value will be picked at a blockwrite (and made 0 so next blockwrite won't do them)
        begin
          if (i>createthreadandwait[j].position) or (i=length(Assemblerlines)-1) then //if it's the last line, then do all remaining
            createthreadandwaitid:=j;
        end;

        //plugin
        {$ifndef jni}
        if length(currentline)>0 then
        begin
          currentlinep:=@currentline[1];
          pluginhandler.handleAutoAssemblerPlugin(@currentlinep, 2,aaid);
          currentline:=currentlinep;
          //if handled currentline will have it's identifiers regarding the plugin's previously registered stuff replaced
          //note that this can be called in a multithreaded situation, so the plugin must hld storage containers on a threadid base and handle the locking itself
        end;
        {$endif}
        //plugin



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
        begin
          for j:=0 to length(labels)-1 do
          begin
            if tokencheck(currentline,labels[j].labelname) then
            begin
              if not labels[j].defined then
              begin
                //the address hasn't been found yet
                //this is the part that causes those nops after a short jump below the current instruction

                //problem: The size of these instructions determine where this label will be defined

                //close
                s1:=replacetoken(currentline,labels[j].labelname,IntToHex(currentaddress,8));

                //far and big

                if processhandler.SystemArchitecture=archarm then
                begin
                  currentline:=replacetoken(currentline,labels[j].labelname,IntToHex(currentaddress+$4FFFFF8,8));
                end
                else
                begin
                  if (processhandler.is64Bit) then //and not in region
                  begin
                    //check if between here and the definition of labels[j].labelname is an write pointer change specifier to a region too far away from currentaddress, if not, LONG will suffice

                    //tip: you 'could' disassemble everything inbetween and see if a small jmp is possible as well (just a lot slower)

                    mustbefar:=false;
                    for l:=i+1 to length(assemblerlines)-1 do
                    begin
                      currentline2:=assemblerlines[l].line;
                      if currentline2=labels[j].labelname+':' then break; //reached the label

                      if currentline2[length(currentline2)]=':' then
                      begin
                        //check if it's just a label or alloc in the same group
                        for k:=0 to length(defines)-1 do
                          currentline2:=replacetoken(currentline2,defines[k].name,defines[k].whatever);


                        s2:=copy(currentline2,1,length(currentline2)-1);
                        for k:=0 to length(allocs)-1 do
                        begin
                          if allocs[k].varname=s2 then
                          begin
                            if currentaddress>allocs[k].address then
                              diff:=currentaddress-allocs[k].address
                            else
                              diff:=allocs[k].address-currentaddress;

                            if diff>=$80000000 then
                            begin
                              mustbefar:=true;
                              break;
                            end;
                          end;
                        end;

                        if mustbefar then break;

                        for k:=0 to length(kallocs)-1 do
                        begin
                          if kallocs[k].varname=s2 then
                          begin
                            if currentaddress>kallocs[k].address then
                              diff:=currentaddress-kallocs[k].address
                            else
                              diff:=kallocs[k].address-currentaddress;

                            if diff>=$80000000 then
                            begin
                              mustbefar:=true;
                              break;
                            end;
                          end;
                        end;

                        if mustbefar then break;

                        //if it's a label it's ok
                        ok1:=false;
                        for k:=0 to length(labels)-1 do
                        begin
                          if labels[k].labelname=s2 then
                          begin
                            ok1:=true;
                            break;
                          end;
                        end;

                        if ok1 then continue; //it's a label, no need to do a heavy symbol lookup

                        //not an alloc or kalloc




                        try
                          testptr:=symhandler.getAddressFromName(copy(currentline2,1,length(currentline2)-1));

                          if currentaddress>testptr then
                            diff:=currentaddress-testptr
                          else
                            diff:=testptr-currentaddress;

                          if diff>=$80000000 then
                          begin
                            mustbefar:=true;
                            break;
                          end;

                        except
                          mustbefar:=true;
                        end;


                        if mustbefar then break;
                      end;
                    end;

                    if mustbefar then
                      currentline:=replacetoken(currentline,labels[j].labelname,IntToHex(currentaddress+$2000FFFFF,8))
                    else
                      currentline:=replacetoken(currentline,labels[j].labelname,IntToHex(currentaddress+$FFFFF,8));
                  end
                  else
                    currentline:=replacetoken(currentline,labels[j].labelname,IntToHex(currentaddress+$FFFFF,8));
                end;


                setlength(assembled,length(assembled)+1);
                assembled[length(assembled)-1].createthreadandwait:=createthreadandwaitid;
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

              //break;
            end;
          end;
        end;

        if ok1 then continue;

        if currentline[length(currentline)]=':' then
        begin
          ok1:=false;
          for j:=0 to length(labels)-1 do
          begin
            if i=labels[j].assemblerline then
            begin
              labels[j].address:=currentaddress;
              labels[j].defined:=true;
              ok1:=true;


              //reassemble the instructions that had no target
              for k:=0 to length(labels[j].references)-1 do
              begin
                a:=length(assembled[labels[j].references[k]].bytes); //original size of the assembled code
                s1:=replacetoken(assemblerlines[labels[j].references2[k]].line,labels[j].labelname,IntToHex(labels[j].address,8));
                {$ifdef cpu64}
                if processhandler.is64Bit then
                  assemble(s1,assembled[labels[j].references[k]].address,assembled[labels[j].references[k]].bytes)
                else
                {$endif}
                assemble(s1,assembled[labels[j].references[k]].address,assembled[labels[j].references[k]].bytes, apLong);

                b:=length(assembled[labels[j].references[k]].bytes); //new size
                setlength(assembled[labels[j].references[k]].bytes,a); //original size (original size is always bigger or equal than newsize)

                if (b<a) and (a<12) then //try to grow the instruction as some people cry about nops (unless it was a megajmp/call as those are less efficient)
                begin
                  //try a bigger one
                  assemble(s1,assembled[labels[j].references[k]].address,nops, apLong);
                  if length(nops)=a then //found a match size
                  begin
                    copymemory(@assembled[labels[j].references[k]].bytes[0], @nops[0], a);
                    b:=a;
                  end;

                end;


                //fill the difference with nops (not the most efficient approach, but it should work)
                if processhandler.SystemArchitecture=archarm then
                begin
                  for l:=0 to ((a-b+3) div 4)-1 do
                    pdword(@assembled[labels[j].references[k]].bytes[b+l*4])^:=$e1a00000;      //<mov r0,r0: (nop equivalent)
                end
                else
                begin
  //              todo:  if a-b>8 then replace with the far version
                  assemble('nop '+inttohex(a-b,1),0,nops);

                  for l:=b to a-1 do
                    assembled[labels[j].references[k]].bytes[l]:=nops[l-b];

  //                for l:=b to a-1 do
  //                  assembled[labels[j].references[k]].bytes[l]:=$90; //nop

                end;
              end;


              break;
            end;
          end;
          if ok1 then continue;

          try
            currentaddress:=symhandler.getAddressFromName(copy(currentline,1,length(currentline)-1));
            continue; //next line
          except
            raise EAssemblerException.create(rsThisAddressSpecifierIsNotValid);
          end;
        end;


        setlength(assembled,length(assembled)+1);
        assembled[length(assembled)-1].address:=currentaddress;
        assembled[length(assembled)-1].createthreadandwait:=createthreadandwaitid;

        if (currentline<>'') and (currentline[1]='<') then //special assembler instruction
        begin

          if copy(currentline,1,8)='<READMEM' then
          begin
            //lets try this for once
            sscanf(currentline, '<READMEM%d>', [@l]);
            setlength(assembled[length(assembled)-1].bytes, readmems[l].bytelength);
            CopyMemory(@assembled[length(assembled)-1].bytes[0], readmems[l].bytes, readmems[l].bytelength);
          end
          else
            assemble(currentline,currentaddress,assembled[length(assembled)-1].bytes);
        end
        else
          assemble(currentline,currentaddress,assembled[length(assembled)-1].bytes);

        inc(currentaddress,length(assembled[length(assembled)-1].bytes));
      end;

    except
      on e:exception do
        raise EAssemblerException.create(inttostr(currentlinenr)+':'+e.message);
    end;
    //end of loop

    ok2:=true;

    //unprotectmemory
    for i:=0 to length(fullaccess)-1 do
    begin
      virtualprotectex(processhandle,pointer(fullaccess[i].address),fullaccess[i].size,PAGE_EXECUTE_READWRITE,op);

      if (fullaccess[i].address>$80000000) and (DBKLoaded) then
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
            ok2:=writeprocessmemory(processhandle,pointer(testptr),binaryfile.Memory,binaryfile.Size,x);
          finally
            binaryfile.free;
          end;
        end;
      end;

    //we're still here so, inject it

    //addresses are known here, so parse the exception list if there is one
    if length(exceptionlist)>0 then
    begin
      InitializeAutoAssemblerExceptionHandler;
      for i:=length(exceptionlist)-1 downto 0 do //add it in the reverse order so the nested try/excepts come first
        AutoAssemblerExceptionHandlerAddExceptionRange(getAddressFromScript(exceptionlist[i].trylabel), getAddressFromScript(exceptionlist[i].exceptlabel));

      AutoAssemblerExceptionHandlerApplyChanges;
    end;


    connection:=getconnection;
    if connection<>nil then
      connection.beginWriteProcessMemory; //group all writes

    //combine assembly lines
    j:=0;
    for i:=1 to length(assembled)-1 do
    begin
      if assembled[i].address=assembled[j].address+length(assembled[j].bytes) then //matches the previous entry
      begin
        //group
        k:=length(assembled[j].bytes);
        setlength(assembled[j].bytes, k+length(assembled[i].bytes));
        copymemory(@assembled[j].bytes[k], @assembled[i].bytes[0], length(assembled[i].bytes));

        assembled[j].createthreadandwait:=max(assembled[j].createthreadandwait, assembled[i].createthreadandwait); //should always pick i


        //mark it as empty
        setlength(assembled[i].bytes,0);
        assembled[i].address:=0;
        assembled[i].createthreadandwait:=-1;
      end
      else
      begin
        j:=i; //new block
      end;
    end;

    for i:=0 to length(assembled)-1 do
    begin
      if length(assembled[i].bytes)=0 then continue;

      testptr:=assembled[i].address;

      vpe:=(SkipVirtualProtectEx=false) and virtualprotectex(processhandle,pointer(testptr),length(assembled[i].bytes),PAGE_EXECUTE_READWRITE,op);
      ok1:=WriteProcessMemoryWithCloakSupport(processhandle, pointer(testptr),@assembled[i].bytes[0],length(assembled[i].bytes),x);
      if vpe then
        virtualprotectex(processhandle,pointer(testptr),length(assembled[i].bytes),op,op2);

      if not ok1 then ok2:=false;

      if ok2 and (assembled[i].createthreadandwait<>-1) then
      begin
        //create threads
        for j:=0 to assembled[i].createthreadandwait do
        begin
          if createthreadandwait[j].position<>-1 then
          begin
            //create the thread and wait for it's result
            testptr:=getAddressFromScript(createthreadandwait[j].name);

            threadhandle:=createremotethread(processhandle,nil,0,pointer(testptr),nil,0,bw);
            ok2:=threadhandle>0;

            if ok2 then
            begin
              try
                if WaitForSingleObject(threadhandle, 5000)<>WAIT_OBJECT_0 then
                  raise EAssemblerException.create('createthreadandwait did not execute properly');
              finally
                closehandle(threadhandle);
              end;
            end;

            createthreadandwait[j].position:=-1; //mark it as handled
          end;
        end;
      end;
    end;

    if connection<>nil then  //group all writes
    begin
      if connection.endWriteProcessMemory=false then
        ok2:=false;
    end;




    if not ok2 then
    begin
      {$ifndef jni}
      if popupmessages then showmessage(rsNotAllInstructionsCouldBeInjected)
      else
      begin
        if memrec<>nil then //there is an memrec provided, so also an ewxception handler
          raise exception.create(rsNotAllInstructionsCouldBeInjected);
      end;
      {$endif}


    end
    else
    begin
      //if ceallocarray<>nil then
      begin
        //see if all allocs are deallocated
        if (length(dealloc)>0) and (length(dealloc)=length(ceallocarray)) then //free everything
        begin
          {$ifdef cpu64}
          baseaddress:=ptrUint($FFFFFFFFFFFFFFFF);
          {$else}
          baseaddress:=$FFFFFFFF;
          {$endif}

          for i:=0 to length(ceallocarray)-1 do
          begin
            virtualfreeex(processhandle,pointer(dealloc[i]),0,MEM_RELEASE);
            if (targetself=false) and allocsAddToUnexpectedExceptionList then
              RemoveUnexpectedExceptionRegion(dealloc[i],0);
{            if ceallocarray[i].address<baseaddress then
              baseaddress:=ceallocarray[i].address;}
          end;

          //virtualfreeex(processhandle,pointer(baseaddress),0,MEM_RELEASE);
        end;

        setlength(ceallocarray,length(allocs));
        for i:=0 to length(allocs)-1 do
          ceallocarray[i]:=allocs[i];
      end;

      if (length(ceexceptionlist)>0) and (AutoAssemblerExceptionHandlerHasEntries) then
      begin
        for i:=0 to length(ceexceptionlist)-1 do
          AutoAssemblerExceptionHandlerRemoveExceptionRange(ceexceptionlist[i]);

        AutoAssemblerExceptionHandlerApplyChanges;
      end;

      setlength(ceexceptionlist, length(exceptionlist));
      for i:=0 to length(ceexceptionlist)-1 do
        ceexceptionlist[i]:=getAddressFromScript(exceptionlist[i].trylabel);

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
                symhandler.AddUserdefinedSymbol(inttohex(labels[j].address,8),addsymbollist[i], true);
                ok1:=true;
              except
                //don't crash when it's already defined or address=0
              end;

            end;

        if not ok1 then
          for j:=0 to length(defines)-1 do
            if uppercase(addsymbollist[i])=uppercase(defines[j].name) then
            begin
              try
                symhandler.DeleteUserdefinedSymbol(addsymbollist[i]); //delete old one so you can add the new one
                symhandler.AddUserdefinedSymbol(defines[j].whatever, addsymbollist[i], true);
                ok1:=true;
              except
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
              threadhandle:=createremotethread(processhandle,nil,0,pointer(testptr),nil,0,bw);
              ok2:=threadhandle>0;

              if ok2 then
                closehandle(threadhandle);
            finally
            end;
          end;
        end;

      {$IFNDEF UNIX}
      if popupmessages then
      begin
        testPtr:=0;

        s1:='';
        for i:=0 to length(globalallocs)-1 do
        begin
          if testPtr=0 then testPtr:=globalallocs[i].address;

          s1:=s1+#13#10+globalallocs[i].varname+'='+IntToHex(globalallocs[i].address,8);
        end;


        for i:=0 to length(allocs)-1 do
        begin
          if testPtr=0 then testPtr:=allocs[i].address;
          s1:=s1+#13#10+allocs[i].varname+'='+IntToHex(allocs[i].address,8);
        end;

        if length(kallocs)>0 then
        begin
          if testPtr=0 then testPtr:=kallocs[i].address;

          s1:=#13#10+rsTheFollowingKernelAddressesWhereAllocated+':';
          for i:=0 to length(kallocs)-1 do
            s1:=s1+#13#10+kallocs[i].varname+'='+IntToHex(kallocs[i].address,8);
        end;

       // if messagedl
        if (testPtr=0) or (oldaamessage) then
          showmessage(rsTheCodeInjectionWasSuccessfull+s1)
        else
        begin
          if MessageDlg(rsTheCodeInjectionWasSuccessfull+s1+#13#10+rsGoTo+inttohex(testptr,8)+'?', mtInformation,[mbYes, mbNo], 0, mbno)=mrYes then
          begin
            memorybrowser.backlist.Push(pointer(memorybrowser.disassemblerview.SelectedAddress));
            memorybrowser.disassemblerview.selectedaddress:=testptr;
            memorybrowser.show;
          end;
        end;
      end;
      {$ENDIF}
    end;

    result:=ok2;

    if result and allocsAddToUnexpectedExceptionList and (not targetself) then
    begin
      for i:=0 to length(allocs)-1 do
        AddUnexpectedExceptionRegion(allocs[i].address,allocs[i].size);
    end;

  finally
    for i:=0 to length(assembled)-1 do
      setlength(assembled[i].bytes,0);

    setlength(assembled,0);

    for i:=0 to length(readmems)-1 do
      if readmems[i].bytes<>nil then
      begin
        freememandnil(readmems[i].bytes);

      end;

    setlength(readmems,0);


    if tokens<>nil then
      freeandnil(tokens);

    {$IFNDEF UNIX}
    pluginhandler.handleAutoAssemblerPlugin(@currentlinep, 3,aaid); //tell the plugins to free their data

    if targetself then
    begin
      processhandler.processhandle:=oldhandle;
      symhandler:=oldsymhandler;
    end;
    {$ENDIF}

    if potentiallabels<>nil then
      freeandnil(potentiallabels);
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
    if (uppercase(trim(code[i])))='[ENABLE]' then
    begin
      insideenable:=true;
      insidedisable:=false;
      continue;
    end;

    if (uppercase(trim(code[i])))='[DISABLE]' then
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

function autoassemble(code: Tstrings; popupmessages,enable,syntaxcheckonly, targetself: boolean;var CEAllocarray: TCEAllocArray; var exceptionlist:TCEExceptionListArray; registeredsymbols: tstringlist=nil; memrec: pointer=nil): boolean; overload;
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
    raise EAssemblerException.create(rsYouCanOnlyHaveOneEnableSection);
  end;

  if disablepos=-2 then
  begin
    if not popupmessages then exit;
    raise EAssemblerException.create(rsYouCanOnlyHaveOneDisableSection);
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
        raise EAssemblerException.create(rsYouHavnTSpecifiedAEnableSection);

      end;

      if (disablepos=-1) then
      begin
        if not popupmessages then exit;
        raise EAssemblerException.create(rsYouHavnTSpecifiedADisableSection);

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

    result:=autoassemble2(tempstrings,popupmessages,syntaxcheckonly,targetself,ceallocarray, exceptionlist, registeredsymbols, memrec);
  finally
    tempstrings.Free;
  end;
end;

function autoassemble(code: Tstrings; popupmessages,enable,syntaxcheckonly, targetself: boolean):boolean; overload;
var
  aa: TCEAllocArray;
  ae: TCEExceptionListArray;
begin
  setlength(aa,0);
  result:=autoassemble(code,popupmessages,enable,syntaxcheckonly,targetself,aa,ae);
end;

function autoassemble(code: tstrings;popupmessages: boolean):boolean; overload;
var
  aa: TCEAllocArray;
  ae: TCEExceptionListArray;
begin
  setlength(aa,0);
  result:=autoassemble(code,popupmessages,true,false,false,aa,ae,nil);
end;


end.





