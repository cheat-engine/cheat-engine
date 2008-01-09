unit autoassembler;

interface

uses assemblerunit,classes,{$ifndef autoassemblerdll}cefuncproc,{$endif}windows,symbolhandler,sysutils,dialogs,controls
{$ifdef netclient}
,netapis;
{$else}
,NewKernelHandler;
{$endif}

{$ifdef autoassemblerdll}
type TCEAlloc=record
  address: dword;
  varname: string;
  size: dword;
end;

type PCEAlloc=^TCEAlloc;
type TCEAllocArray=array of TCEAlloc;
{$endif}

procedure getenableanddisablepos(code:tstrings;var enablepos,disablepos: integer);
function autoassemble(code: tstrings;popupmessages: boolean):boolean; overload;
function autoassemble(code: Tstrings; popupmessages,enable,syntaxcheckonly, targetself: boolean;var CEAllocarray: TCEAllocArray): boolean; overload;

implementation


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


function removecomments(code: tstrings):string;
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
      if currentline[length(currentline)-1]=':' then
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
      if currentline[length(currentline)-1]=':' then
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
        //lastseenlabel is now updated to the current pos
      end else
      if pos('@f',lowercase(currentline))>0 then  //forward
      begin
        //forward label, so labels[lastseenlabel+1]
        if lastseenlabel+1 >= length(labels) then
          raise exception.Create('Forward jump with no label defined');

        currentline:=replacetoken(currentline,'@f',labels[lastseenlabel+1]);
        currentline:=replacetoken(currentline,'@F',labels[lastseenlabel+1]);
      end else
      if pos('@b',lowercase(currentline))>0 then //back
      begin
        //forward label, so labels[lastseenlabel]
        if lastseenlabel=-1 then
          raise exception.Create('There is code defined without specifying the address it belongs to');

        currentline:=replacetoken(currentline,'@b',labels[lastseenlabel]);
        currentline:=replacetoken(currentline,'@B',labels[lastseenlabel]);
      end;
    end;
    code[i]:=currentline;

  end;
end;


function autoassemble2(code: tstrings;popupmessages: boolean;syntaxcheckonly:boolean; targetself: boolean ;var ceallocarray:TCEAllocArray ):boolean;
type tassembled=record
  address: dword;
  bytes: TAssemblerbytes;
end;


type tlabel=record
  defined: boolean;
  address:dword;
  labelname: string;
  assemblerline: integer;
  references: array of integer; //index of assembled array
  references2: array of integer; //index of assemblerlines array
end;
type tfullaccess=record
  address: dword;
  size: dword;
end;
type tdefine=record
  name: string;
  whatever: string;
end;
var i,j,k,l,e: integer;
    currentline: string;

    currentaddress: dword;
    assembled: array of tassembled;
    x,y,op,op2:dword;
    ok1,ok2:boolean;
    loadbinary: array of record
      address: string; //string since it might be a label/alloc/define
      filename: string;
    end;

    allocs,kallocs: array of tcealloc;
    labels: array of tlabel;
    defines: array of tdefine;
    fullaccess: array of tfullaccess;
    dealloc: array of integer;
    addsymbollist: array of string;
    deletesymbollist: array of string;
    createthread: array of string;

    a,b,c: integer;
    s1,s2: string;

    assemblerlines: array of string;

    varsize: integer;
    tokens: tstringlist;
    baseaddress: dword;

    include: tstringlist;
    testdword,bw: dword;
    binaryfile: tmemorystream;

    incomment: boolean;

    bytebuf: PByteArray;

    processhandle: THandle;
    ProcessID: DWORD;

    symhandler: TSymhandler;   
begin
  if targetself then
  begin
    //get this function to use the symbolhandler that's pointing to CE itself and the self processid/handle
    processhandle:=getcurrentprocess;
    processid:=getcurrentprocessid;
    symhandler:=symbolhandler.selfsymhandler;
  end
  else
  begin
    processhandle:=cefuncproc.ProcessHandle;
    processid:=cefuncproc.ProcessID;
    symhandler:=symbolhandler.symhandler;
  end;

  symhandler.waitforsymbolsloaded;


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



          currentline:=trim(currentline);

          if length(currentline)=0 then continue;
          if copy(currentline,1,2)='//' then continue; //skip

          setlength(assemblerlines,length(assemblerlines)+1);
          assemblerlines[length(assemblerlines)-1]:=currentline;

          if uppercase(copy(currentline,1,8))='INCLUDE(' then
          begin
            a:=pos('(',currentline);
            b:=pos(')',currentline);

            if (a>0) and (b>0) then
            begin
              s1:=copy(currentline,a+1,b-a-1);

              if ExtractFileExt(uppercase(s1))='.' then
                s1:=s1+'CEA';

              if ExtractFileExt(uppercase(s1))='' then
                s1:=s1+'.CEA';

              if not fileexists(s1) then
                raise exception.Create(s1+' could not be found');


              include:=tstringlist.Create;
              try
                include.LoadFromFile(s1);
                for j:=i+1 to (i+1)+(include.Count-1) do
                  code.Insert(j,include[j-(i+1)]);
              finally
                include.Free;
              end;

              setlength(assemblerlines,length(assemblerlines)-1);
              continue;
            end
            else raise exception.Create('Wrong syntax. Include(filename.cea)');
          end;

          if uppercase(copy(currentline,1,13))='CREATETHREAD(' then
          begin
            //load a binary file into memory , this one already executes BEFORE the 2nd pass to get addressnames correct
            a:=pos('(',currentline);
            b:=pos(')',currentline);
            if (a>0) and (b>0) then
            begin
              s1:=copy(currentline,a+1,b-a-1);
            
              setlength(createthread,length(createthread)+1);
              createthread[length(createthread)-1]:=s1;

              setlength(assemblerlines,length(assemblerlines)-1);
              continue;
            end else raise exception.Create('Wrong syntax. CreateThread(address)');          
          end;

          if uppercase(copy(currentline,1,12))='LOADLIBRARY(' then
          begin
            //load a library into memory , this one already executes BEFORE the 2nd pass to get addressnames correct
            a:=pos('(',currentline);
            b:=pos(')',currentline);

            if (a>0) and (b>0) then
            begin
              s1:=copy(currentline,a+1,b-a-1);

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
                raise exception.create(s1+' could not be injected');
              end;

              setlength(assemblerlines,length(assemblerlines)-1);
              continue;
            end else raise exception.Create('Wrong syntax. LoadLibrary(filename)');
          end;

          if uppercase(copy(currentline,1,8))='READMEM(' then
          begin
            //read memory and place it here (readmem(address,size) )
            a:=pos('(',currentline);
            b:=pos(',',currentline);
            c:=pos(')',currentline);
            if (a>0) and (b>0) and (c>0) then
            begin
              s1:=copy(currentline,a+1,b-a-1);
              s2:=copy(currentline,b+1,c-b-1);

              //read memory and replace with lines of DB xx xx xx xx xx xx xx xx
              try
                x:=symhandler.getAddressFromName(s1);
              except
                raise exception.Create('Invalid address for ReadMem');
              end;

              try
                a:=strtoint(s2);
              except
                raise exception.Create('Invalid size for ReadMem');
              end;

              if a=0 then
                raise exception.Create('Invalid size for ReadMem');


              getmem(bytebuf,a);
              try
                if (not ReadProcessMemory(processhandle, pointer(x),bytebuf,a,x)) or (x<a) then
                  raise exception.Create('The memory at '+s1+' could not be fully read');

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



            end else raise exception.Create('Wrong syntax. ReadMem(address,size)');

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
              s1:=copy(currentline,a+1,b-a-1);
              s2:=copy(currentline,b+1,c-b-1);

              if not fileexists(s2) then raise exception.Create('The file '+s2+' does not exist');

              setlength(loadbinary,length(loadbinary)+1);
              loadbinary[length(loadbinary)-1].address:=s1;
              loadbinary[length(loadbinary)-1].filename:=s2;

              setlength(assemblerlines,length(assemblerlines)-1);
              continue;
            end else raise exception.Create('Wrong syntax. LoadBinary(address,filename)');
          end;

          if uppercase(copy(currentline,1,15))='REGISTERSYMBOL(' then
          begin
            //add this symbol to the register symbollist
            a:=pos('(',currentline);
            b:=pos(')',currentline);

            if (a>0) and (b>0) then
            begin
              s1:=copy(currentline,a+1,b-a-1);

              setlength(addsymbollist,length(addsymbollist)+1);
              addsymbollist[length(addsymbollist)-1]:=s1;
            end
            else raise exception.Create('Syntax error');

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
              s1:=copy(currentline,a+1,b-a-1);

              setlength(deletesymbollist,length(deletesymbollist)+1);
              deletesymbollist[length(deletesymbollist)-1]:=s1;
            end
            else raise exception.Create('Syntax error');

            setlength(assemblerlines,length(assemblerlines)-1);
            continue;
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
              s1:=copy(currentline,a+1,b-a-1);
              s2:=copy(currentline,b+1,c-b-1);

              for j:=0 to length(defines)-1 do
                if uppercase(defines[j].name)=uppercase(s1) then
                  raise exception.Create('Define '+s1+' already defined');

              setlength(defines,length(defines)+1);
              defines[length(defines)-1].name:=s1;
              defines[length(defines)-1].whatever:=s2;

              setlength(assemblerlines,length(assemblerlines)-1);   //don't bother with this in the 2nd pass
              continue;          
            end else raise exception.Create('Wrong syntax. DEFINE(name,whatever)');
          end;


          if uppercase(copy(currentline,1,11))='FULLACCESS(' then
          begin
            a:=pos('(',currentline);
            b:=pos(',',currentline);
            c:=pos(')',currentline);

            if (a>0) and (b>0) and (c>0) then
            begin
              s1:=copy(currentline,a+1,b-a-1);
              s2:=copy(currentline,b+1,c-b-1);

              setlength(fullaccess,length(fullaccess)+1);
              fullaccess[length(fullaccess)-1].address:=symhandler.getAddressFromName(s1);
              fullaccess[length(fullaccess)-1].size:=strtoint(s2);
            end else raise exception.Create('Syntax error. FullAccess(address,size)');

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
              s1:=copy(currentline,a+1,b-a-1);


              val('$'+s1,j,a);
              if a=0 then raise exception.Create(s1+' is not a valid identifier');

              varsize:=length(s1);

              while (j<length(labels)) and (length(labels[j].labelname)>varsize) do
              begin
                if labels[j].labelname=s1 then
                  raise exception.Create(s1+' is being redeclared');
                inc(j);
              end;

              j:=length(labels);//quickfix
              l:=j;


              //check for the line "labelname:"
              ok1:=false;
              for j:=0 to code.Count-1 do
                if trim(code[j])=s1+':' then
                begin
                  if ok1 then raise exception.Create('label '+s1+' is being defined more than once');
                  ok1:=true;
                end;

              if not ok1 then raise exception.Create('label '+s1+' is not defined in the script');


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
            end else raise exception.Create('Syntax Error');
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
                s1:=copy(currentline,a+1,b-a-1);

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
            //allocate memory
            a:=pos('(',currentline);
            b:=pos(',',currentline);
            c:=pos(')',currentline);

            if (a>0) and (b>0) and (c>0) then
            begin
              s1:=copy(currentline,a+1,b-a-1);
              s2:=copy(currentline,b+1,c-b-1);

              val('$'+s1,j,a);
              if a=0 then raise exception.Create(s1+' is not a valid identifier');

              varsize:=length(s1);

              //check for duplicate identifiers
              j:=0;
              while (j<length(allocs)) and (length(allocs[j].varname)>varsize) do
              begin
                if allocs[j].varname=s1 then
                  raise exception.Create('The identifier '+s1+' has already been declared');

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

              setlength(assemblerlines,length(assemblerlines)-1);   //don't bother with this in the 2nd pass
              continue;
            end else raise exception.Create('Wrong syntax. ALLOC(identifier,sizeinbytes)');
          end;

          //replace identifiers with values so the assemble error check doesnt crash on that
          for j:=0 to length(allocs)-1 do
            currentline:=replacetoken(currentline,allocs[j].varname,'00000000');

          for j:=0 to length(defines)-1 do
            currentline:=replacetoken(currentline,defines[j].name,defines[j].whatever);


          {$ifndef net}
          //memory kalloc
          if uppercase(copy(currentline,1,7))='KALLOC(' then
          begin
            if not DBKReadWrite then raise exception.Create('You need to use kernelmode read/writeprocessmemory if you want to use KALLOC');

            if DarkByteKernel=0 then
              raise exception.Create('Sorry, but without the driver KALLOC will not function');

            //syntax: kalloc(x,size)    x=variable name size=bytes
            //kallocate memory
            a:=pos('(',currentline);
            b:=pos(',',currentline);
            c:=pos(')',currentline);

            if (a>0) and (b>0) and (c>0) then
            begin
              s1:=copy(currentline,a+1,b-a-1);
              s2:=copy(currentline,b+1,c-b-1);

              val('$'+s1,j,a);
              if a=0 then raise exception.Create(s1+' is not a valid identifier');

              varsize:=length(s1);

              //check for duplicate identifiers
              j:=0;
              while (j<length(kallocs)) and (length(kallocs[j].varname)>varsize) do
              begin
                if kallocs[j].varname=s1 then
                  raise exception.Create('The identifier '+s1+' has already been declared');

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
            end else raise exception.Create('Wrong syntax. kalloc(identifier,sizeinbytes)');
          end;

          //replace identifiers with values so the assemble error check doesnt crash on that
          for j:=0 to length(kallocs)-1 do
            currentline:=replacetoken(currentline,kallocs[j].varname,'00000000');

          {$endif}

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
              raise exception.Create('This address specifier is not valid');
            end;
          end;

          //replace label references with 00000000 so the assembler check doesn't complain about it
          for j:=0 to length(labels)-1 do
            currentline:=replacetoken(currentline,labels[j].labelname,'00000000');

          try
            //replace identifiers in the line with their address
            if not assemble(currentline,currentaddress,assembled[0].bytes) then raise exception.Create('bla');
          except
            raise exception.Create('This instruction can''t be compiled');
          end;

        finally
          inc(i);
        end;

      except
        on E:exception do
          raise exception.Create('Error in line '+IntToStr(i+1)+' ('+currentline+')'+' :'+e.Message);

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

        if not ok1 then raise exception.Create(addsymbollist[i]+' was supposed to be added to the symbollist, but it isn''t declared');
      end;
    end;

    //check to see if the addresses are valid (label, alloc, define)
    if length(createthread)>0 then
      for i:=0 to length(createthread)-1 do
      begin
        ok1:=true;

        try
          testdword:=symhandler.getAddressFromName(createthread[i]);
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
                testdword:=symhandler.getAddressFromName(defines[j].whatever);
                ok1:=true;
              except
              end;
              break;
            end;

        if not ok1 then raise exception.Create('The address in createthread('+createthread[i]+') is not valid');

      end;

    if length(loadbinary)>0 then
      for i:=0 to length(loadbinary)-1 do
      begin
        ok1:=true;

        try
          testdword:=symhandler.getAddressFromName(loadbinary[i].address);
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
                testdword:=symhandler.getAddressFromName(defines[j].whatever);
                ok1:=true;
              except
              end;
              break;
            end;

        if not ok1 then raise exception.Create('The address in loadbinary('+loadbinary[i].address+','+loadbinary[i].filename+') is not valid');

      end;


    if syntaxcheckonly then
    begin
      result:=true;
      exit;
    end;

    if popupmessages and (messagedlg('This code can be injected. Are you sure?',mtConfirmation	,[mbyes,mbno],0)<>mryes) then exit;

    //allocate the memory
    //first find out how much I should allocate
    if length(allocs)>0 then
    begin
      x:=0;
      for i:=0 to length(allocs)-1 do
       inc(x,allocs[i].size);

      allocs[0].address:=dword(virtualallocex(processhandle,nil,x,MEM_COMMIT,page_execute_readwrite));

      for i:=1 to length(allocs)-1 do
        allocs[i].address:=allocs[i-1].address+allocs[i-1].size;
    end;

    {$ifndef net}
    //kernel alloc
    if length(kallocs)>0 then
    begin
      x:=0;
      for i:=0 to length(kallocs)-1 do
       inc(x,kallocs[i].size);

      kallocs[0].address:=dword(KernelAlloc(x));

      for i:=1 to length(kallocs)-1 do
        kallocs[i].address:=kallocs[i-1].address+kallocs[i-1].size;
    end;
    {$endif}

    //-----------------------2nd pass------------------------
    setlength(assembled,0);
    for i:=0 to length(assemblerlines)-1 do
    begin
      currentline:=assemblerlines[i];

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
            //FFFFF, goes over the $FF boundary, and $FFFF boundary, and is a big value (address)\
            currentline:=replacetoken(currentline,labels[j].labelname,IntToHex(currentaddress+$FFFFF,8));

            setlength(assembled,length(assembled)+1);
            assembled[length(assembled)-1].address:=currentaddress;
            assemble(currentline,currentaddress,assembled[length(assembled)-1].bytes);
            a:=length(assembled[length(assembled)-1].bytes);

            assemble(s1,currentaddress,assembled[length(assembled)-1].bytes);
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
            labels[j].address:=currentaddress;
            labels[j].defined:=true;
            ok1:=true;

            //fill in the undefined opcodes
            for k:=0 to length(labels[j].references)-1 do
            begin
              a:=length(assembled[labels[j].references[k]].bytes); //original size of the assembled code
              s1:=replacetoken(assemblerlines[labels[j].references2[k]],labels[j].labelname,IntToHex(labels[j].address,8));
              assemble(s1,assembled[labels[j].references[k]].address,assembled[labels[j].references[k]].bytes);

              b:=length(assembled[labels[j].references[k]].bytes); //new size

              setlength(assembled[labels[j].references[k]].bytes,a);
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
          raise exception.Create('This address specifier is not valid');
        end;
      end;


      setlength(assembled,length(assembled)+1);
      assembled[length(assembled)-1].address:=currentaddress;
      assemble(currentline,currentaddress,assembled[length(assembled)-1].bytes);

      inc(currentaddress,length(assembled[length(assembled)-1].bytes));
    end;


    ok2:=true;

    //unprotectmemory
    for i:=0 to length(fullaccess)-1 do
    begin
      virtualprotectex(processhandle,pointer(fullaccess[i].address),fullaccess[i].size,PAGE_EXECUTE_READWRITE,op);

      if (fullaccess[i].address>80000000) and (DarkByteKernel<>0) then
        MakeWritable(fullaccess[i].address,(fullaccess[i].size div 4096)*4096,false);
    end;

    //load binaries
    if length(loadbinary)>0 then
      for i:=0 to length(loadbinary)-1 do
      begin
        ok1:=true;
        try
          testdword:=symhandler.getAddressFromName(loadbinary[i].address);
        except
          ok1:=false;
        end;

        if not ok1 then
          for j:=0 to length(labels)-1 do
            if uppercase(labels[j].labelname)=uppercase(loadbinary[i].address) then
            begin
              ok1:=true;
              testdword:=labels[j].address;
              break;
            end;

        if not ok1 then
          for j:=0 to length(allocs)-1 do
            if uppercase(allocs[j].varname)=uppercase(loadbinary[i].address) then
            begin
              ok1:=true;
              testdword:=allocs[j].address;
              break;
            end;

        if not ok1 then
          for j:=0 to length(kallocs)-1 do
            if uppercase(kallocs[j].varname)=uppercase(loadbinary[i].address) then
            begin
              ok1:=true;
              testdword:=kallocs[j].address;
              break;
            end;

        if not ok1 then
          for j:=0 to length(defines)-1 do
            if uppercase(defines[j].name)=uppercase(loadbinary[i].address) then
            begin
              try
                testdword:=symhandler.getAddressFromName(defines[j].whatever);
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
            ok2:=writeprocessmemory(processhandle,pointer(testdword),binaryfile.Memory,binaryfile.Size,bw);
          finally
            binaryfile.free;
          end;
        end;
      end;

    //we're still here so, inject it
    for i:=0 to length(assembled)-1 do
    begin
      virtualprotectex(processhandle,pointer(assembled[i].address),length(assembled[i].bytes),PAGE_EXECUTE_READWRITE,op);
      ok1:=WriteProcessMemory(processhandle,pointeR(assembled[i].address),@assembled[i].bytes[0],length(assembled[i].bytes),op2);
      virtualprotectex(processhandle,pointer(assembled[i].address),length(assembled[i].bytes),op,op2);

      if not ok1 then ok2:=false;
    end;

    if not ok2 then
    begin
      if popupmessages then showmessage('Not all instructions could be injected')
    end
    else
    begin
      //if ceallocarray<>nil then
      begin
        //see if all allocs are deallocated
        if length(dealloc)=length(ceallocarray) then //free everything
        begin
          baseaddress:=$FFFFFFFF;

          for i:=0 to length(dealloc)-1 do
          begin
            if ceallocarray[i].address<baseaddress then
              baseaddress:=dealloc[i];
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
              symhandler.AddUserdefinedSymbol(allocs[j].address,addsymbollist[i]);
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
                symhandler.AddUserdefinedSymbol(labels[j].address,addsymbollist[i]);
                ok1:=true;
              except
                //don't crash when it's already defined or address=0
              end;

            end;
      end;

      //still here, so create threads if needed

      //load binaries
      if length(createthread)>0 then
        for i:=0 to length(createthread)-1 do
        begin
          ok1:=true;
          try
            testdword:=symhandler.getAddressFromName(createthread[i]);
          except
            ok1:=false;
          end;

          if not ok1 then
            for j:=0 to length(labels)-1 do
              if uppercase(labels[j].labelname)=uppercase(createthread[i]) then
              begin
                ok1:=true;
                testdword:=labels[j].address;
                break;
              end;

          if not ok1 then
            for j:=0 to length(allocs)-1 do
              if uppercase(allocs[j].varname)=uppercase(createthread[i]) then
              begin
                ok1:=true;
                testdword:=allocs[j].address;
                break;
              end;

          if not ok1 then
            for j:=0 to length(kallocs)-1 do
              if uppercase(kallocs[j].varname)=uppercase(createthread[i]) then
              begin
                ok1:=true;
                testdword:=kallocs[j].address;
                break;
              end;

          if not ok1 then
            for j:=0 to length(defines)-1 do
              if uppercase(defines[j].name)=uppercase(createthread[i]) then
              begin
                try
                  testdword:=symhandler.getAddressFromName(defines[j].whatever);
                  ok1:=true;
                except
                end;

                break;
              end;

          if ok1 then //address found
          begin
            binaryfile:=tmemorystream.Create;
            try
              ok2:=createremotethread(processhandle,nil,0,pointer(testdword),nil,0,bw)>0;
            finally
              binaryfile.free;
            end;
          end;
        end;

      if popupmessages then
      begin
        s1:='';
        for i:=0 to length(allocs)-1 do
          s1:=s1+#13#10+allocs[i].varname+'='+IntToHex(allocs[i].address,8);

        if length(kallocs)>0 then
        begin
          s1:=#13#10+'The following kernel addresses where allocated:';
          for i:=0 to length(kallocs)-1 do
            s1:=s1+#13#10+kallocs[i].varname+'='+IntToHex(kallocs[i].address,8);
        end;

        showmessage('The code injection was successfull'+s1);
      end;
    end;

    result:=ok2;

  finally
    for i:=0 to length(assembled)-1 do
      setlength(assembled[i].bytes,0);

    setlength(assembled,0);
    tokens.free;

  end;
end;


procedure getenableanddisablepos(code:tstrings;var enablepos,disablepos: integer);
var i,j: integer;
    currentline: string;
begin
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


function autoassemble(code: Tstrings; popupmessages,enable,syntaxcheckonly, targetself: boolean;var CEAllocarray: TCEAllocArray): boolean; overload;
{
targetself defines if the process that gets injected to is CE itself or the target process
}
var tempstrings: tstringlist;
    i,j: integer;
    currentline: string;
    enablepos,disablepos: integer;
begin
  getenableanddisablepos(code,enablepos,disablepos);

  result:=false;
  
  if enablepos=-2 then
  begin
    if not popupmessages then exit;
    raise exception.Create('You can only have one enable section');
  end;

  if disablepos=-2 then
  begin
    if not popupmessages then exit;
    raise exception.Create('You can only have one disable section');
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
        raise exception.Create('You havn''t specified a enable section');

      end;

      if (disablepos=-1) then
      begin
        if not popupmessages then exit;
        raise exception.Create('You havn''t specified a disable section');
        
      end;

      if enable then
      begin
        if enablepos>disablepos then
        begin
          //copy everything from enablepos to end
          for i:=enablepos+1 to code.count-1  do
            tempstrings.add(code[i]);
        end
        else
        begin
          for i:=enablepos+1 to disablepos-1 do
            tempstrings.add(code[i]);
        end;
      end
      else
      begin
        if disablepos>enablepos then
        begin
          //copy everything from disablepos to end
          for i:=disablepos+1 to code.count-1  do
            tempstrings.add(code[i]);
        end
        else
        begin
          for i:=disablepos+1 to enablepos-1 do
            tempstrings.add(code[i]);
        end;
      end;
    end;

    result:=autoassemble2(tempstrings,popupmessages,syntaxcheckonly,targetself,ceallocarray);
  finally
    tempstrings.Free;
  end;
end;

function autoassemble(code: tstrings;popupmessages: boolean):boolean; overload;
var aa: TCEAllocArray;
begin
  setlength(aa,0);
  result:=autoassemble(code,popupmessages,true,false,false,aa);
end;


end.



