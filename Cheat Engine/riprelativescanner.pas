unit RipRelativeScanner;
{
This class will scan a given module and return the rip relative instructions
}

{$mode delphi}

interface

uses
  Windows, Classes, SysUtils, disassembler, symbolhandler, processhandlerunit,
  NewKernelHandler, CEFuncProc;

type
  TRIPRelativeScanner=class
  private
    modulebase: ptruint;
    modulesize: dword;

    executablelist: array of record
                          base: ptruint;
                          size: dword;
                    end;


    addresslist: array of ptruint;
    valuelist: array of ptruint;
    addresslistpos: integer;
    addresslistsize: integer;
    function getAddress(i: integer): ptruint;
  public
    constructor create(modulename: string);
    property Address[i: integer]: ptruint read getAddress;
  published
    property Count: integer read addresslistpos;
  end;

implementation

function TRIPRelativeScanner.getAddress(i: integer): ptruint;
begin
  result:=0;
  if (i<0) or (i>=addresslistpos) then exit;

  result:=addresslist[i];
end;

constructor TRIPRelativeScanner.create(modulename: string);
var
  mi: TModuleInfo;
  currentbase: ptruint;
  mbi: TMemoryBasicInformation;

  d: TDisassembler;
  a: ptruint;
  i: integer;
  desc: string;
begin
  if processhandler.is64bit and symhandler.getmodulebyname(modulename, mi) then
  begin
    modulebase:=mi.baseaddress;
    modulesize:=mi.basesize;

    //querry the executable memory regions

    currentbase:=modulebase;
    zeromemory(@mbi, sizeof(mbi));
    while (currentbase<modulebase+modulesize) and (VirtualQueryEx(processhandle, pointer(currentbase), mbi, sizeof(mbi))=sizeof(mbi)) do
    begin
      if (mbi.Protect and (PAGE_EXECUTE or PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE or PAGE_EXECUTE_WRITECOPY))<>0 then
      begin
        setlength(executablelist, length(executablelist)+1);
        executablelist[length(executablelist)-1].base:=ptruint(mbi.BaseAddress);
        executablelist[length(executablelist)-1].size:=mbi.RegionSize;
      end;

      currentbase:=ptruint(mbi.BaseAddress)+mbi.RegionSize;
    end;

    setlength(addresslist, 512);
    addresslistsize:=512;
    addresslistpos:=0;

    d:=TDisassembler.Create;
    try
      for i:=0 to length(executablelist)-1 do
      begin
        a:=executablelist[i].base;

        while a<executablelist[i].base+executablelist[i].size do
        begin
          d.disassemble(a, desc);


          if not ((length(d.LastDisassembleData.Bytes)=2) and ((d.LastDisassembleData.Bytes[0]=0) and (d.LastDisassembleData.Bytes[1]=0))) then
          begin
            if (d.LastDisassembleData.riprelative>0) and InRangeX(d.LastDisassembleData.modrmValue, modulebase, modulebase+modulesize) then
            begin
              //found one
              addresslist[addresslistpos]:=d.LastDisassembleData.address+d.LastDisassembleData.riprelative;

              inc(addresslistpos);
              if addresslistpos>=addresslistsize then
              begin
                addresslistsize:=addresslistsize*2;
                setlength(addresslist, addresslistsize);
              end;
            end;
          end
          else
            inc(a, 8-(a mod 8)); //this can cause an alignment issue

        end;
      end;
    finally
      d.free;
    end;
  end;
end;


end.

