unit MemFuncs;

{$mode delphi}

interface

uses
  {$ifdef darwin}
  macport,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}
  Classes, SysUtils, cefuncproc, newkernelhandler, commonTypeDefs;

procedure getexecutablememoryregionsfromregion(start: ptrUint; stop:ptrUint; var memoryregions: TMemoryRegions);
function getallmemoryregions(var memoryregions: tmemoryregions): qword;

implementation

uses ProcessHandlerUnit;


function getallmemoryregions(var memoryregions: tmemoryregions): qword;
var address: ptrUint;
    mbi: memory_basic_information;
    stop: ptruint;
begin
  result:=0;

  setlength(memoryregions,0);
  address:=0;
  if processhandler.is64Bit then
    stop:=$7fffffffffffffff
  else
    stop:=$7fffffff;

  while (address<stop) and (VirtualQueryEx(processhandle,pointer(address),mbi,sizeof(mbi))<>0) and ((address+mbi.RegionSize)>address) do
  begin
    if (mbi.state=MEM_COMMIT) and
       ((mbi.Protect and PAGE_NOACCESS)<>PAGE_NOACCESS) and
       ((mbi.Protect and PAGE_GUARD)<>PAGE_GUARD) and
       ((mbi.Protect and PAGE_NOCACHE)<>PAGE_NOCACHE) then
    begin
      //readable
      setlength(memoryregions,length(memoryregions)+1);
      memoryregions[length(memoryregions)-1].BaseAddress:=ptrUint(mbi.baseaddress);
      memoryregions[length(memoryregions)-1].MemorySize:=mbi.RegionSize;

      inc(result, mbi.RegionSize);
    end;

    inc(address,mbi.RegionSize);
  end;
end;

procedure getexecutablememoryregionsfromregion(start: ptrUint; stop:ptrUint; var memoryregions: tmemoryregions);
var address: ptrUint;
    mbi: memory_basic_information;
begin
  setlength(memoryregions,0);
  address:=start;
  while (address<stop) and (VirtualQueryEx(processhandle,pointer(address),mbi,sizeof(mbi))<>0) and ((address+mbi.RegionSize)>address) do
  begin
    if (mbi.state=MEM_COMMIT) and
    (
       ((mbi.Protect and PAGE_EXECUTE)=PAGE_EXECUTE) or
       ((mbi.Protect and PAGE_EXECUTE_READ)=PAGE_EXECUTE_READ) or
       ((mbi.Protect and PAGE_EXECUTE_READWRITE)=PAGE_EXECUTE_READWRITE) or
       ((mbi.Protect and PAGE_EXECUTE_WRITECOPY)=PAGE_EXECUTE_WRITECOPY)
    )
    then
    begin
      //executable
      setlength(memoryregions,length(memoryregions)+1);
      memoryregions[length(memoryregions)-1].BaseAddress:=ptrUint(mbi.baseaddress);
      memoryregions[length(memoryregions)-1].MemorySize:=mbi.RegionSize;
    end;

    inc(address,mbi.RegionSize);
  end;
end;

end.

