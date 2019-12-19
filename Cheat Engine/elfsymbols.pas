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

function EnumElfSymbols32(modulename: string; modulebase: ptruint; mem: pbyte; callback: TNetworkEnumSymCallback): boolean;
begin
  result:=false;
end;

function EnumElfSymbols64(modulename: string; modulebase: ptruint; mem: pbyte; callback: TNetworkEnumSymCallback): boolean;
type
  PElf64Hdr=^TElf64Hdr;

var
  Header: PElf64Hdr;

  SectionHeaders: PElf64SectHdr;

  CurrentSectionHeader: PElf64SectHdr;

  i,j: integer;
  maxindex: integer;

  symbol: PElf64Symbol;

  name: pchar;

  address: ptruint;
begin
  result:=false;
  Header:=@mem[sizeof(TElfIdent)];

  SectionHeaders:=@mem[Header^.SectHdrOffset];

  for i:=0 to Header^.SectHdrNum-1 do
  begin
    if ptruint(@SectionHeaders[i])-ptruint(mem)>4096 then break; //unexpected

    CurrentSectionHeader:=@SectionHeaders[i];

    if (CurrentSectionHeader^._Type=SHT_SYMTAB) or (CurrentSectionHeader^._Type=SHT_DYNSYM) then
    begin

      maxindex:=CurrentSectionHeader^.Size div sizeof(TElf64Symbol);
      symbol:=@mem[CurrentSectionHeader^.Offset];

      for j:=0 to maxindex-1 do
      begin
        name:=@mem[SectionHeaders[CurrentSectionHeader^.Link].Offset+symbol[j].Name];

        address:=modulebase+(symbol[j].value-CurrentSectionHeader^.Address+CurrentSectionHeader^.Offset);


        if (symbol[j].Value<>0) and (name<>'') then
          callback(modulename, name, address, symbol[j].Size, false);
      end;

    end;
  end;
end;

function EnumElfSymbols(modulename: string; modulebase: ptruint; callback: TNetworkEnumSymCallback): boolean;
type
  PElfIdent=^TElfIdent;
var
  br: ptruint;
  mem: pbyte;
begin
  result:=false;
  mem:=getmem(4096);
  if ReadProcessMemory(processhandle, pointer(modulebase), mem, 4096, br) then
  begin
    if PElfIdent(mem)^.ElfClass=ELFCLASS32 then
      result:=EnumElfSymbols32(modulename, modulebase, mem, callback)
    else
      result:=EnumElfSymbols64(modulename, modulebase, mem, callback);
  end;

  FreeMemAndNil(mem);
end;


end.

