unit GnuAssembler;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef darwin}
  macport,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}
  Classes, SysUtils, NewKernelHandler, ProcessHandlerUnit, strutils,
  dialogs, commonTypeDefs;

{
      //scan for .extraparams_as <string>
      //scan for .extraparams_ld <string>

      //scan the script for .asection , .msection and .aobscan
      //1:
      //.aobscan <name> "ceaobscanformat"
      //Internally ->:
      //.msection <name> <aobscanresult>
      //->see .msection

      //2:
      //.asection <name> <0xpreferedaddress>
      //Internally ->:
      //.section _<name>,"xa" :  after assembly check the size and allocate at least the specific amount
      //name:

      //3:
      //.msection <name> <address or ce symbol> <expectedsize OPT> <overridesize OPT>  (error out if after linking the assumed size is bigger than expected. E.g veneers and sometimes alignments)
      //Internally ->:
      //.section _<name>,"xa" : after assembly add it to the section list with the given address
      //name:

      //4:
      //scan for <xxx>: and add ".type <xxx>, %function" for each label found (

      //assemble the script

      //parse the ELF header
      //enumerate the sections. Crossreference them with the list of asection and msection.  Unknown sections will be handled as .asection with no prefered address
      //allocate the sections (that need to be allocated.  .msection sections are already allocated)

      //write a linker script that sets these sections to their specific addresses

      //parse the ELF header further and figure out which symbols are undefined
      //call the linker with --defsym=symbolname=0xaddress for each symbol

      //after the linker:
      //extract the binary data for each section:
      //objcopy -j<sectionname> -O binary <tempresultfile> <sectionbinary>

      //write the sections to memory from top to bottom. So write your code injection destinations before the jump/branch to it. (Just like the auto assembler)


}


procedure gnuassemble(originalscript: tstrings);

implementation

uses simpleaobscanner, symbolhandler, binutils, elftypes, elfconsts, MemoryQuery,
  globals, UnexpectedExceptionsHelper;

resourcestring
  rsInvalidELFFile = 'Invalid ELF file';
  rsDoesntSeemToBeAValidELFFile = ' doesn''t seem to be a valid ELF file';
  rsConfigureAValidBinutilsSetupFirst = 'Configure a valid binutils setup first';
  rsTheAOBFor = 'The AOB for ';
  rsCouldNotBeFound = ' could not be found';
  rsTheProperFormatOfAsectionIs = 'The proper format of .asection is : .asection <name> <preferedaddress>';
  rsTheProperFormatOfMsectionIs = 'The proper format of .msection is : .msection <name> <address> <optional expected size>';
  rsNoBinutilsInstalled = 'No binutils installed';
  rsTook = ' took ';
  rsBytesWhile = ' bytes while ';
  rsWhereExpected = ' where expected';
  rsGNUAssemblerError = 'GNU Assembler error';

type


  PElf32Hdr=^TElf32Hdr;
  PElf64Hdr=^TElf64Hdr;
  PElfIdent=^TElfIdent;
  PElf32SectHdr=^TElf32SectHdr;

procedure GetSectionsFromElf32(mem: PByte; size: integer; var sections: TSectionArray);
var
  Header: PElf32Hdr;
  SectionHeaders: PElf32SectHdr;
  i: integer;

  stringsection: integer;
  sectionstrings: pchar;

  sectionname: string;
begin
  Header:=@mem[16];

  if Header^.SectHdrOffset>size then raise exception.create(rsInvalidELFFile);
  SectionHeaders:=@mem[Header^.SectHdrOffset];

  if ptruint(@SectionHeaders[Header^.SectHdrNum-1])-ptruint(mem)>size then raise exception.create(rsInvalidELFFile);

  stringsection:=Header^.NameTableIndex;
  if stringsection>Header^.SectHdrNum then raise exception.create(rsInvalidELFFile);

  sectionstrings:=@mem[SectionHeaders[stringsection].Offset];


  for i:=0 to Header^.SectHdrNum-1 do
  begin
    sectionname:=pchar(@sectionstrings[SectionHeaders[i].NameIdx]);

    if (SectionHeaders[i].Size>0) and ((SectionHeaders[i].Flags and SHF_ALLOC)=SHF_ALLOC) then
    begin
      setlength(sections, length(sections)+1);
      sections[length(sections)-1].name:=sectionname;
      sections[length(sections)-1].size:=SectionHeaders[i].Size;
    end;
  end;
end;

procedure GetSectionsFromElf64(mem: PByte; size: integer; var sections: TSectionArray);
var
  Header: PElf64Hdr;
  SectionHeaders: PElf64SectHdr;
  i: integer;

  stringsection: integer;
  sectionstrings: pchar;

  sectionname: string;
begin
  Header:=@mem[16];

  if Header^.SectHdrOffset>size then raise exception.create(rsInvalidELFFile);
  SectionHeaders:=@mem[Header^.SectHdrOffset];

  if ptruint(@SectionHeaders[Header^.SectHdrNum-1])-ptruint(mem)>size then raise exception.create(rsInvalidELFFile);

  stringsection:=Header^.NameTableIndex;
  if stringsection>Header^.SectHdrNum then raise exception.create(rsInvalidELFFile);

  sectionstrings:=@mem[SectionHeaders[stringsection].Offset];


  for i:=0 to Header^.SectHdrNum-1 do
  begin
    sectionname:=pchar(@sectionstrings[SectionHeaders[i].NameIdx]);

    if (SectionHeaders[i].Size>0) and ((SectionHeaders[i].Flags and SHF_ALLOC)=SHF_ALLOC) then
    begin
      setlength(sections, length(sections)+1);
      sections[length(sections)-1].name:=sectionname;
      sections[length(sections)-1].size:=SectionHeaders[i].Size;
    end;
  end;
end;


procedure GetSectionsFromElf(elffile: string; var sections: TSectionArray);

var
  ms: TMemorystream;
  buf: PElfIdent;
begin
  ms:=tmemorystream.Create;
  try
    ms.LoadFromFile(elffile);
    buf:=PElfIdent(ms.Memory);

    if buf^.Magic<>ELFMAGIC then raise exception.create(elffile+rsDoesntSeemToBeAValidELFFile);

    if buf^.ElfClass=ELFCLASS32 then GetSectionsFromElf32(PByte(buf), ms.size, sections) else
    if buf^.ElfClass=ELFCLASS64 then GetSectionsFromElf64(PByte(buf), ms.size, sections);
  finally
    ms.free;
  end;
end;

function gnuassemble2(originalscript: tstrings; targetself: boolean; var CEAllocarray: TCEAllocArray): boolean;
var
  nmdisabled: boolean;
  i,j,k,l: integer;
  line: string;

  p1, p2, p3: string;

  a: ptruint;

  sections: array of record
    name: string;
    allocate: boolean;
    address: ptruint;
    expectedsize: dword;
    actualsize: dword;
  end;

  binarysections: TBinarySections;

  imports: TImportList;


  elfsections: TSectionArray;

  script: Tstringlist;
  o: string;
  found: boolean;

  lnkfile: Tstringlist;
  lnkfilename: string;

  defined, undefinedimports: Tstringlist;
  x: ptruint;
  op: dword;

  extraparams_as, extraparams_ld: string;

  bu: TBinUtils;
  vpe: boolean;
begin
  result:=false;
  if (binutilslist.count=0) then
    raise exception.create(rsConfigureAValidBinutilsSetupFirst);

  bu:=nil;

  script:=tstringlist.create;
  script.Assign(originalscript);
  nmdisabled:=false;

  setlength(imports,0);
  setlength(sections,0);

  try
    try

      i:=0;

     // if processhandler.SystemArchitecture=archX86 then
     //   script.insert(0,'.intel_syntax');

      while i<script.count do
      begin
        line:=lowercase(trim(script[i]));
        if (length(line)>1) then
        begin
          if (line[1]='.') then
          begin
            case line[2] of
              'a':   //.aobscan / .asection
              begin
                if copy(line, 1, 9)='.aobscan ' then
                begin
                  if wordcount(line, [' '])>=3 then  //.aobscan counts as one too
                  begin
                    p1:=ExtractWord(2, line, [' ']);
                    j:=WordPosition(3, line, [' ']);
                    p2:=copy(line, j, length(line)); //the rest is the aob (using ce format aob text)
                  end;
                  a:=findaob(p2);

                  if a=0 then raise exception.create(rsTheAOBFor+p1+rsCouldNotBeFound);

                  script[i]:='.msection '+p1+' 0x'+inttohex(a,8);
                  continue;
                end
                else
                if copy(line, 1,10)='.asection ' then
                begin
                  j:=wordcount(line, [' ']);
                  if (j<=1) or (j>3) then
                    raise exception.create(rsTheProperFormatOfAsectionIs);

                  p1:=extractword(2, line, [' ']);

                  if j=3 then
                    p2:=extractword(3, line, [' '])
                  else
                    p2:='';

                  //add this section to the list
                  j:=length(sections);
                  setlength(sections, j+1);
                  sections[j].name:='_'+p1;

                  if p2<>'' then //has a prefered address
                  begin
                    try
                      sections[j].address:=StrToQWord(p2)
                    except
                      sections[j].address:=symhandler.getAddressFromName(p2);
                    end;
                  end
                  else
                    sections[j].address:=0;

                  sections[j].allocate:=true;
                  sections[j].expectedsize:=0;

                  script[i]:='.section _'+p1+',"xa"';
                  script.Insert(i+1,p1+':');
                end;
              end;

              'b': //.binutils <name> (optional)
              begin
                if copy(line,1,10)='.binutils ' then
                begin
                  p1:=trim(copy(line,11, length(line)-1));

                  for i:=0 to binutilslist.count-1 do
                  begin
                    if TBinUtils(binutilslist[i]).name=p1 then
                    begin
                      bu:=TBinUtils(binutilslist[i]);
                      break;
                    end;
                  end;

                end;
              end;

              'e':
              begin
                if copy(line,1,13)='.extraparams_' then
                begin
                  if length(line)>15 then
                  begin
                    if copy(line,14,3)='as ' then
                      extraparams_as:=copy(line,17,length(line))
                    else
                    if copy(line,14,3)='ld ' then
                      extraparams_ld:=copy(line,17,length(line));
                  end;
                end;
              end;



              'i': //.import
              begin
                if copy(line,1,8)='.import ' then
                begin
                  //add to the import table
                  j:=WordCount(line,[' ',',']);
                  for k:=2 to j do
                  begin
                    l:=length(imports);
                    setlength(imports, l+1);
                    imports[l].name:=ExtractWord(k, line, [' ',',']);
                    imports[l].address:=symhandler.getAddressFromName(imports[l].name);
                  end;



                  setlength(imports,j+1);
                end;
              end;

              'm': //.msection
              begin
                if copy(line, 1,10)='.msection ' then
                begin
                  j:=wordcount(line, [' ']);
                  if (j<=1) or (j>4) then
                    raise exception.create(rsTheProperFormatOfMsectionIs);

                  p1:=extractword(2, line, [' ']);
                  p2:=extractword(3, line, [' ']);

                  if j=4 then
                    p3:=extractword(4, line, [' '])
                  else
                    p3:='';

                  j:=length(sections);
                  setlength(sections, j+1);
                  sections[j].name:='_'+p1;

                  try
                    sections[j].address:=strtoint(p2)
                  except
                    sections[j].address:=symhandler.getAddressFromName(p2);
                  end;

                  sections[j].allocate:=false;
                  if p3<>'' then
                    sections[j].expectedsize:=strtoint(p3)
                  else
                    sections[j].expectedsize:=0;


                  script[i]:='.section _'+p1+',"xa"';
                  script.Insert(i+1,p1+':');
                end;

              end;

              'n': if line='.nonm' then nmdisabled:=true;

            end;
          end;
            {
          if line[length(line)]=':' then
          begin
            //label definition
            p1:=copy(line, 1, length(line)-1);
            script.insert(0,'.type '+p1+', %function');
            inc(i);
          end;}
        end;


        inc(i);
      end;


      if (bu=nil) then
        bu:=defaultBinutils;

      if (bu=nil) and (binutilslist.count>0) then
        bu:=TBinUtils(binutilslist[0]);

      if bu=nil then
        raise exception.create(rsNoBinutilsInstalled);


      bu.assemble(script, extraparams_as, o);

      //parse o to get the sections and their size

      setlength(elfsections,0);

      try
        GetSectionsFromElf(o, elfsections);   //elfsections now contains all allocatable sections
      except
        //I work best with ELF obj files, but I'll add some fallback by calling objdump to get the data needed
        bu.GetSections(o, elfsections);
      end;



      //add the unknown ones
      for i:=0 to length(elfsections)-1 do
      begin
        found:=false;
        for j:=0 to length(sections)-1 do
        begin
          if sections[j].name=elfsections[i].name then
          begin
            sections[j].actualsize:=elfsections[i].size; //just temporarily
            found:=true;
            break;
          end;
        end;

        if not found then
        begin
          //add it
          j:=length(sections);
          setlength(sections, j+1);
          sections[j].name:=elfsections[i].name;
          sections[j].actualsize:=elfsections[i].size;
          sections[j].allocate:=true;
          sections[j].expectedsize:=0;
          sections[j].address:=0;
        end;
      end;

      //allocate the ones that need allocating
      for i:=0 to length(sections)-1 do
      begin
        if sections[i].allocate then
        begin
          if sections[i].address=0 then //any random address will do
            sections[i].address:=ptrUint(virtualallocex(processhandle,nil,sections[i].actualsize+1024, MEM_RESERVE or MEM_COMMIT,page_execute_readwrite))
          else //allocate this memory at the given address
            sections[i].address:=ptrUint(virtualallocex(processhandle,FindFreeBlockForRegion(sections[i].address,sections[i].actualsize+1024),sections[i].actualsize+1024, MEM_RESERVE or MEM_COMMIT,page_execute_readwrite));

          if allocsAddToUnexpectedExceptionList then
            AddUnexpectedExceptionRegion(sections[i].address, sections[i].actualsize+1024);
        end;
      end;

      //build the link file
      lnkfile:=TStringList.create;
      lnkfile.add('SECTIONS');
      lnkfile.add('{');
      for i:=0 to length(sections)-1 do
        lnkfile.add(sections[i].name+' 0x'+inttohex(sections[i].address,8)+' : { *. }');

      lnkfile.add('}');

      lnkfilename:=GetTempFileName;
      lnkfile.SaveToFile(lnkfilename);
      lnkfile.Free;


      if not nmdisabled then
      begin
        //if nm hasn't been disabled call nm and then check which symbols are undefined and add them to the import list (if they aren't in there already)
        undefinedimports:=tstringlist.create;
        defined:=tstringlist.create;
        bu.nm(o, defined, undefinedimports);

        for i:=0 to undefinedimports.count-1 do
        begin
          found:=false;
          for j:=0 to length(imports)-1 do
          begin
            if imports[j].name=undefinedimports[i] then
              found:=true;
          end;

          if not found then
          begin
            j:=length(imports);
            setlength(imports, j+1);
            imports[j].name:=undefinedimports[i];
            imports[j].address:=symhandler.getAddressFromName(imports[j].name);
          end;
        end;
        undefinedimports.free;
        defined.free;

      end;

      //link the assembler file with the provided linker file and import list
      setlength(binarysections, length(sections));
      for i:=0 to length(sections)-1 do
        binarysections[i].sectionname:=sections[i].name;

      bu.ldAndExtract(o, lnkfilename, extraparams_ld, imports, binarysections);

      //before writing, first check that the sections are of compatible size
      for i:=0 to length(sections)-1 do
      begin
        if (sections[i].expectedsize>0) then
        begin
          for j:=0 to length(binarysections)-1 do
          begin
            if sections[i].name=binarysections[j].sectionname then
            begin
              //write it
              if (sections[i].expectedsize>0) and (sections[i].expectedsize<length(binarysections[j].data)) then
                raise exception.create(sections[i].name+rsTook+inttostr(length(binarysections[j].data))+rsBytesWhile+inttostr(sections[i].expectedsize)+rsWhereExpected);
              break;
            end;

          end;
        end;
      end;

      //write the binary sections to memory
      for i:=0 to length(sections)-1 do
      begin
        for j:=0 to length(binarysections)-1 do
        begin
          if sections[i].name=binarysections[j].sectionname then
          begin
            vpe:=(SkipVirtualProtectEx=false) and virtualprotectex(processhandle,  pointer(sections[i].address), length(binarysections[j].data),PAGE_EXECUTE_READWRITE,op);
            WriteProcessMemory(processhandle, pointer(sections[i].address), @binarysections[j].data[0], length(binarysections[j].data), x);
            if vpe then virtualprotectex(processhandle,  pointer(sections[i].address), length(binarysections[j].data),op,op);
          end;
        end;
      end;



      deletefile(lnkfilename);
      deletefile(o);

      result:=true;
    finally
      script.free;
    end;


  except
    on e: exception do
    begin
      MessageDlg(rsGNUAssemblerError,e.message,mtError, [mbok],0);
    end;
  end;


end;


{
example for x86_64:
.intel_syntax noprefix
.msection bla 0x00400500
duh:
nop
nop
nop
jmp [rip+xxx]
xxx:
.quad test

.asection mycode 0x200000000
nop
nop
test:
nop
nop
jmp test
nop
mov rax,qword ptr [rip+test2]
nop
nop

test2:
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop

}

procedure gnuassemble(originalscript: tstrings);
var a: TCEAllocArray;
begin
  //let's do something similar to autoassembler
  gnuassemble2(originalscript, false, a);
end;

end.
