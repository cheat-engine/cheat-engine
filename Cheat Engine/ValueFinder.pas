unit ValueFinder;

{$MODE Delphi}

{
Designed fro use with the pointerscan.
With some fiddling it might be used for some other stuff as well
}

interface

uses windows, LCLIntf, classes, sysutils, symbolhandler, math, cefuncproc,newkernelhandler, commonTypeDefs;

type TValueFinder=class
  private
    buffer: pointer;
    memregionofBuffer: integer;
    memregions: TMemoryRegions;
    stopaddress: ptrUint;
    function ismatchtovalue(p: pointer): boolean;
    procedure quicksortmemoryregions(lo,hi: integer);
  public
    valuetype: TVariableType;
    valuescandword: dword;
    valuescansingle: single;
    valuescandouble: double;
    valuescansinglemax: single;
    valuescandoublemax: double;
    alligned: boolean;  

    function FindValue(startaddress: ptrUint): ptrUint;
    constructor create(startaddress, stopaddress: ptrUint);
    destructor destroy; override;
end;

implementation

uses ProcessHandlerUnit, Globals;

procedure TValueFinder.quicksortmemoryregions(lo,hi: integer);
var i,j: integer;
    x,h: TMemoryRegion;
begin
  i:=lo;
  j:=hi;

  x:=memregions[(lo+hi) div 2];

  repeat
    while (memregions[i].BaseAddress<x.BaseAddress) do inc(i);
    while (memregions[j].BaseAddress>x.BaseAddress) do dec(j);

    if i<=j then
    begin
      h:=memregions[i];
      memregions[i]:=memregions[j];
      memregions[j]:=h;
      inc(i);
      dec(j);
    end;

  until i>j;

  if (lo<j) then quicksortmemoryregions(lo,j);
  if (i<hi) then quicksortmemoryregions(i,hi);
end;

function TValueFinder.ismatchtovalue(p: pointer): boolean;
begin
  result:=false;
  case valuetype of
    vtDword: result:=pdword(p)^=valuescandword;
    vtSingle: result:=(psingle(p)^>=valuescansingle) and (psingle(p)^<valuescansinglemax);
    vtDouble: result:=(pdouble(p)^>=valuescandouble) and (pdouble(p)^<valuescandoublemax);
  end;
end;


function TValueFinder.FindValue(startaddress: ptrUint): ptrUint;
{
Pre: Called from lowest to highest value. NO RANDOM
}
var currentaddress: ptrUint;
    i: integer;
    valuesize: integer;
    x: ptruint;
    offset: dword;
begin
  if valuetype=vtdouble then valuesize:=8 else valuesize:=4;
  
  result:=0;
  if memregionofbuffer = -1 then exit; //failure loading ANY region at create time

  currentaddress:=startaddress;
  while currentaddress<stopaddress do
  begin
    if currentaddress<memregions[memregionofbuffer].baseaddress then
      currentaddress:=memregions[memregionofbuffer].baseaddress;
    
    if (currentaddress>(memregions[memregionofbuffer].baseaddress+memregions[memregionofbuffer].memorysize)) then
    begin
      if memregionofbuffer+1>=length(memregions) then exit; //nothing else found

      i:=memregionofbuffer+1;
      while (i<length(memregions)) do
      begin
        if readprocessmemory(processhandle, pointer(memregions[i].baseaddress), buffer, memregions[i].MemorySize,x) then
        begin
          memregionofbuffer:=i;
          currentaddress:=memregions[memregionofbuffer].baseaddress;
          break;
        end
        else
          inc(i);
      end;

      if i>=length(memregions) then exit; //no readable memory found
    end;

    //still here so the current address falls in the currently loaded buffer
    offset:=currentaddress-memregions[memregionofbuffer].baseaddress;
    if offset+valuesize<memregions[memregionofbuffer].memorysize then
    begin
      //it falls withing the region
      if ismatchtovalue(@pbytearray(buffer)[offset]) then
      begin
        //found one
        result:=currentaddress;
        exit;
      end;
    end;

    if alligned then
      inc(currentaddress,processhandler.pointersize)
    else
      inc(currentaddress);
  end;
  
end;

constructor TValueFinder.create(startaddress, stopaddress: ptrUint);
var address: ptrUint;
    i: integer;
    maxsize: dword;
    mbi: MEMORY_BASIC_INFORMATION;
    x: ptruint;
begin
  memregionofbuffer:=-1;
  self.stopaddress:=stopaddress;
    
  //enumerate all memory regions
  address:=0;



  while (Virtualqueryex(processhandle,pointer(address),mbi,sizeof(mbi))<>0) and ((address+mbi.RegionSize)>address) do
  begin
    if (not symhandler.inSystemModule(ptrUint(mbi.baseAddress))) and (not (not scan_mem_private and (mbi._type=mem_private))) and (not (not scan_mem_image and (mbi._type=mem_image))) and (not (not scan_mem_mapped and ((mbi._type and mem_mapped)>0))) and (mbi.State=mem_commit) and ((mbi.Protect and page_guard)=0) and ((mbi.protect and page_noaccess)=0) then  //look if it is commited
    begin
      if Skip_PAGE_NOCACHE then
        if (mbi.AllocationProtect and PAGE_NOCACHE)=PAGE_NOCACHE then
        begin
          address:=ptrUint(mbi.BaseAddress)+mbi.RegionSize;
          continue;
        end;

      if Skip_PAGE_WRITECOMBINE then
        if (mbi.AllocationProtect and PAGE_WRITECOMBINE)=PAGE_WRITECOMBINE then
        begin
          address:=ptrUint(mbi.BaseAddress)+mbi.RegionSize;
          continue;
        end;

      setlength(memregions,length(memregions)+1);

      memregions[length(memregions)-1].BaseAddress:=ptrUint(mbi.baseaddress);  //just remember this location
      memregions[length(memregions)-1].MemorySize:=mbi.RegionSize;
    end;


    address:=ptrUint(mbi.baseaddress)+mbi.RegionSize;
  end;

  //split up the memory regions into small chunks of max 512KB (so don't allocate a fucking 1GB region)
  i:=0;
  while i<length(memregions) do
  begin
    if memregions[i].MemorySize>512*1024 then
    begin
      //too big, so cut into pieces
      //create new entry with 512KB less
      setlength(memregions,length(memregions)+1);
      memregions[length(memregions)-1].BaseAddress:=memregions[i].BaseAddress+512*1024;
      memregions[length(memregions)-1].MemorySize:=memregions[i].MemorySize-512*1024;
      memregions[i].MemorySize:=512*1024; //set the current region to be 512KB

    end;
    inc(i); //next item. Eventually the new items will be handled, and they will also be split untill the list is finally cut into small enough chunks    
  end;

  //sort memoryregions from small to high
  quicksortmemoryregions(0,length(memregions)-1);  


  //let's load the first region (assuming the user wants to start from the beginning)
  maxsize:=0;
  for i:=0 to length(memregions)-1 do
    maxsize:=max(maxsize,memregions[i].MemorySize);

  getmem(buffer,maxsize);

  i:=0;
  while (i<length(memregions)) and (memregionofbuffer=-1) do
  begin
    if readprocessmemory(processhandle, pointer(memregions[i].baseaddress), buffer, memregions[i].MemorySize,x) then
      memregionofbuffer:=i
    else
      inc(i);
  end;
end;

destructor TValueFinder.destroy;
begin
  if buffer<>nil then
    freememandnil(buffer);
  inherited destroy;
end;

end.
