unit ValueFinder;
{
Designed fro use with the pointerscan.
With some fiddling it might be used for some other stuff as well
}

interface

uses windows, classes, sysutils, symbolhandler, math, cefuncproc,newkernelhandler;

type TValueFinder=class
  private
    buffer: pointer;
    memregionofBuffer: integer;
    memregions: TMemoryRegions;
    stopaddress: dword;
    function ismatchtovalue(p: pointer): boolean;
  public
    valuetype: TVariableType;
    valuescandword: dword;
    valuescansingle: single;
    valuescandouble: double;
    valuescansinglemax: single;
    valuescandoublemax: double;
    alligned: boolean;  

    function FindValue(startaddress: dword): dword;
    constructor create(startaddress, stopaddress: dword);
    destructor destroy; override;
end;

implementation

function TValueFinder.ismatchtovalue(p: pointer): boolean;
begin
  result:=false;
  case valuetype of
    vtDword: result:=pdword(p)^=valuescandword;
    vtSingle: result:=(psingle(p)^>=valuescansingle) and (psingle(p)^<valuescansinglemax);
    vtDouble: result:=(pdouble(p)^>=valuescandouble) and (pdouble(p)^<valuescandoublemax);
  end;
end;


function TValueFinder.FindValue(startaddress: dword): dword;
{
Pre: Called from lowest to highest value. NO RANDOM
}
var currentaddress: dword;
    i: integer;
    valuesize: integer;
    x: dword;
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
      inc(currentaddress,4)
    else
      inc(currentaddress);
  end;
  
end;

constructor TValueFinder.create(startaddress, stopaddress: dword);
var address: dword;
    i: integer;
    maxsize: dword;
    mbi: MEMORY_BASIC_INFORMATION;
    x: dword;
begin
  memregionofbuffer:=-1;
  self.stopaddress:=stopaddress;
    
  //enumerate all memory regions
  address:=0;



  while (Virtualqueryex(processhandle,pointer(address),mbi,sizeof(mbi))<>0) and (address<$FFFFFFFF) and ((address+mbi.RegionSize)>address) do
  begin
    if (not symhandler.inSystemModule(dword(mbi.baseAddress))) and (not (not scan_mem_private and (mbi.type_9=mem_private))) and (not (not scan_mem_image and (mbi.type_9=mem_image))) and (not (not scan_mem_mapped and ((mbi.type_9 and mem_mapped)>0))) and (mbi.State=mem_commit) and ((mbi.Protect and page_guard)=0) and ((mbi.protect and page_noaccess)=0) then  //look if it is commited
    begin
      if Skip_PAGE_NOCACHE then
        if (mbi.AllocationProtect and PAGE_NOCACHE)=PAGE_NOCACHE then
        begin
          address:=dword(mbi.BaseAddress)+mbi.RegionSize;
          continue;
        end;

      setlength(memregions,length(memregions)+1);

      memregions[length(memregions)-1].BaseAddress:=dword(mbi.baseaddress);  //just remember this location
      memregions[length(memregions)-1].MemorySize:=mbi.RegionSize;
    end;


    address:=dword(mbi.baseaddress)+mbi.RegionSize;
  end;


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
    freemem(buffer);
  inherited destroy;
end;

end.
