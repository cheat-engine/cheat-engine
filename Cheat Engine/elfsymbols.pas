unit elfsymbols;

{$mode OBJFPC}

interface

uses
{$ifdef JNI}
  Classes, SysUtils, elftypes, elfconsts, networkInterface, unixporthelper, newkernelhandler, processhandlerunit;
{$else}
  {$ifdef darwin}
  macport,
  {$endif}
  Classes, SysUtils, elftypes, elfconsts, networkInterface, cefuncproc, newkernelhandler, processhandlerunit;
{$endif}

function EnumElfSymbols(modulename: string; modulebase: ptruint; callback: TNetworkEnumSymCallback): boolean;

implementation

type
  PElf64Hdr    =^TElf64Hdr;
  PElf32SectHdr=^TElf32SectHdr;
  PElf32Symbol =^TElf32Symbol;


procedure loadStringTable32(modulebase: ptruint; sectionHeaders: PElf32SectHdr; stringTable: ppchar; index: integer);
var br: ptruint;
begin
  if (stringtable[index]=nil) and (sectionHeaders[index]._Type=SHT_STRTAB) then
  begin
    stringtable[index]:=getmem(sectionheaders[index].Size);
    if not readProcessMemory(processhandle, pointer(modulebase+sectionHeaders[index].Offset), stringTable[index],sectionHeaders[index].Size,br) then
      FreeMemAndNil(stringtable[index]);
  end;
end;

function EnumElfSymbols32(modulename: string; modulebase: ptruint; callback: TNetworkEnumSymCallback): boolean;
var
  ELFHeader: TElf32Hdr;
  SectionHeaders: PElf32SectHdr;

  i,j: integer;
  maxindex: integer;

  symboltable: PElf32Symbol;

  name: pchar;

  address: ptruint;
  br: PTRUINT;

  stringtable: ppchar;
begin
  result:=false;
  if readprocessmemory(processhandle, pointer(modulebase+sizeof(TElfIdent)),@ELFHeader,sizeof(ELFHeader),br) then
  begin
    if ELFHeader.SectHdrOffset=0 then exit;
    if ELFHeader.SectHdrEntrySize<>sizeof(TElf32SectHdr) then exit;

    getmem(SectionHeaders, ELFHeader.SectHdrEntrySize*ELFHeader.SectHdrNum);
    getmem(stringtable, ELFHeader.SectHdrNum*sizeof(pchar));
    FillByte(stringtable^, ELFHeader.SectHdrNum*sizeof(pchar), 0);

    try
      if ReadProcessMemory(processhandle, pointer(modulebase+ELFHeader.SectHdrOffset), sectionheaders, ELFHeader.SectHdrEntrySize*ELFHeader.SectHdrNum, br)=false then exit;

      loadStringTable32(modulebase, sectionheaders, stringtable, ELFHeader.NameTableIndex);

      for i:=0 to ELFHeader.SectHdrNum-1 do
      begin
        if ((sectionHeaders[i]._Type=SHT_SYMTAB) or (sectionHeaders[i]._Type=SHT_DYNSYM)) and (sectionHeaders[i].Size>0) then
        begin
          if sectionheaders[i].EntSize<>sizeof(TElf32Symbol) then continue; //invalid data
          if sectionheaders[i].Link>=ELFHeader.SectHdrNum then continue; //invalid

          getmem(symboltable, sectionHeaders[i].size);
          try
            if readProcessMemory(processhandle, pointer(modulebase+sectionheaders[i].Offset), symboltable, sectionheaders[i].Size, br)=false then continue;

            //get the stringtable of this section
            if sectionHeaders[sectionheaders[i].Link]._Type=SHT_STRTAB then
              loadStringTable32(modulebase, sectionheaders, stringtable, sectionheaders[i].Link);

            if stringtable[sectionHeaders[i].Link]=nil then continue; //no stringdata

            maxindex:=sectionHeaders[i].Size div sectionheaders[i].EntSize;
            for j:=0 to maxindex-1 do
            begin
              if symbolTable[j].Value=0 then continue;

              //char *symbolname=(char *)&stringTable[sectionHeaders[i].sh_link][symbolTable[j].st_name];
              name:=@(stringTable[sectionHeaders[i].Link][symbolTable[j].Name]);
              address:=modulebase+symboltable[j].value;

              if name<>'' then
                callback(modulename, name, address, symboltable[j].Size, false);
            end;
          finally
            freemem(symboltable);
          end
        end;
      end;
    finally
      freemem(SectionHeaders);
      for i:=0 to ELFHeader.SectHdrNum-1 do
      begin
        if stringtable[i]<>nil then
          FreeMemAndNil(stringtable[i]);
      end;
      FreeMemAndNil(stringtable);
    end;
  end;
end;

procedure loadStringTable64(modulebase: ptruint; sectionHeaders: PElf64SectHdr; stringTable: ppchar; index: integer);
var br: ptruint;
begin
  if (stringtable[index]=nil) and (sectionHeaders[index]._Type=SHT_STRTAB) then
  begin
    stringtable[index]:=getmem(sectionheaders[index].Size);
    if not readProcessMemory(processhandle, pointer(modulebase+sectionHeaders[index].Offset), stringTable[index],sectionHeaders[index].Size,br) then
      FreeMemAndNil(stringtable[index]);
  end;
end;

function EnumElfSymbols64(modulename: string; modulebase: ptruint; callback: TNetworkEnumSymCallback): boolean;
var
  ELFHeader: TElf64Hdr;
  SectionHeaders: PElf64SectHdr;

  i,j: integer;
  maxindex: integer;

  symboltable: PElf64Symbol;

  name: pchar;

  address: ptruint;
  br: PTRUINT;

  stringtable: ppchar;
begin
  result:=false;
  if readprocessmemory(processhandle, pointer(modulebase+sizeof(TElfIdent)),@ELFHeader,sizeof(ELFHeader),br) then
  begin
    if ELFHeader.SectHdrOffset=0 then exit;
    if ELFHeader.SectHdrEntrySize<>sizeof(TElf64SectHdr) then exit;

    getmem(SectionHeaders, ELFHeader.SectHdrEntrySize*ELFHeader.SectHdrNum);
    getmem(stringtable, ELFHeader.SectHdrNum*sizeof(pchar));
    FillByte(stringtable^, ELFHeader.SectHdrNum*sizeof(pchar), 0);

    try
      if ReadProcessMemory(processhandle, pointer(modulebase+ELFHeader.SectHdrOffset), sectionheaders, ELFHeader.SectHdrEntrySize*ELFHeader.SectHdrNum, br)=false then exit;

      loadStringTable64(modulebase, sectionheaders, stringtable, ELFHeader.NameTableIndex);

      for i:=0 to ELFHeader.SectHdrNum-1 do
      begin
        if ((sectionHeaders[i]._Type=SHT_SYMTAB) or (sectionHeaders[i]._Type=SHT_DYNSYM)) and (sectionHeaders[i].Size>0) then
        begin
          if sectionheaders[i].EntSize<>sizeof(TElf64Symbol) then continue; //invalid data
          if sectionheaders[i].Link>=ELFHeader.SectHdrNum then continue; //invalid

          getmem(symboltable, sectionHeaders[i].size);
          try
            if readProcessMemory(processhandle, pointer(modulebase+sectionheaders[i].Offset), symboltable, sectionheaders[i].Size, br)=false then continue;

            //get the stringtable of this section
            if sectionHeaders[sectionheaders[i].Link]._Type=SHT_STRTAB then
              loadStringTable64(modulebase, sectionheaders, stringtable, sectionheaders[i].Link);

            if stringtable[sectionHeaders[i].Link]=nil then continue; //no stringdata

            maxindex:=sectionHeaders[i].Size div sectionheaders[i].EntSize;
            for j:=0 to maxindex-1 do
            begin
              if symbolTable[j].Value=0 then continue;

              //char *symbolname=(char *)&stringTable[sectionHeaders[i].sh_link][symbolTable[j].st_name];
              name:=@(stringTable[sectionHeaders[i].Link][symbolTable[j].Name]);
              address:=modulebase+symboltable[j].value;

              if name<>'' then
                callback(modulename, name, address, symboltable[j].Size, false);
            end;
          finally
            freemem(symboltable);
          end
        end;
      end;
    finally
      freemem(SectionHeaders);
      for i:=0 to ELFHeader.SectHdrNum-1 do
      begin
        if stringtable[i]<>nil then
          FreeMemAndNil(stringtable[i]);
      end;
      FreeMemAndNil(stringtable);
    end;
  end;
end;

function EnumElfSymbols(modulename: string; modulebase: ptruint; callback: TNetworkEnumSymCallback): boolean;
var
  ElfIdent: TElfIdent;
  magic: string;
  br: ptruint;
begin
  result:=false;
  if ReadProcessMemory(processhandle, pointer(modulebase), @ElfIdent, sizeof(ElfIdent), br) then
  begin
    if ElfIdent.Magic=ELFMAGIC then
    begin
      if ElfIdent.ElfClass=ELFCLASS32 then
        result:=EnumElfSymbols32(modulename, modulebase, callback)
      else
        result:=EnumElfSymbols64(modulename, modulebase, callback);
    end;
  end;
end;


end.

