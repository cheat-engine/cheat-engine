unit rescanhelper;

{$MODE Delphi}

interface

uses windows, LCLIntf, classes, symbolhandler, CEFuncProc,NewKernelHandler, maps, sysutils, syncobjs;

type
  TPageInfo=record
    data: PByteArray;
  end;

  PPageInfo=^TPageInfo;

  TRescanHelper=class
  private
    pagemap: TMap;
    pagemapcs: TCriticalsection;

    memoryregion: TMemoryRegions;

    function BinSearchMemRegions(address: ptrUint): integer;
    procedure quicksortmemoryregions(lo,hi: integer);
  public
    function ispointer(address: ptrUint): boolean;
    function GetMemoryRegion(Address: ptruint): TMemoryRegion;

    function FindPage(index: ptruint): TPageInfo;

    function getMemoryRegions: TMemoryRegions;


    constructor create(valuesize: integer; memoryregions: PMemoryRegions=nil);
    destructor destroy; override;
end;

implementation

function TRescanHelper.BinSearchMemRegions(address: ptrUint): integer;
var
  First: Integer;
  Last: Integer;
  Found: Boolean;
  Pivot: integer;
begin
  First  := 0; //Sets the first item of the range
  Last   := length(memoryregion)-1; //Sets the last item of the range
  Found  := False; //Initializes the Found flag (Not found yet)
  Result := -1; //Initializes the Result

  while (First <= Last) and (not Found) do
  begin

    //Gets the middle of the selected range
    Pivot := (First + Last) div 2;
    //Compares the String in the middle with the searched one
    if (address >= memoryregion[Pivot].BaseAddress) and (address<memoryregion[Pivot].BaseAddress+memoryregion[Pivot].MemorySize) then
    begin
      Found  := True;
      Result := Pivot;
    end
    //If the Item in the middle has a bigger value than
    //the searched item, then select the first half
    else if memoryregion[Pivot].BaseAddress > address then
      Last := Pivot - 1
        //else select the second half
    else
      First := Pivot + 1;
  end;
end;

function TRescanHelper.ispointer(address: ptrUint): boolean;
{
Check the memoryregion array for this address. If it's in, return true
}
begin
  result:=BinSearchMemRegions(address)<>-1;
end;

function TRescanHelper.GetMemoryRegion(Address: ptruint): TMemoryRegion;
var i: integer;
begin
  i:=BinSearchMemRegions(address);
  if i<>-1 then
    result:=memoryregion[BinSearchMemRegions(address)]
  else
    result.BaseAddress:=0;
end;

function TRescanHelper.findPage(Index: ptrUint): TPageInfo;
{
will find the pageinfo. If not found, it will add it
}
var
  pi: TPageInfo;
  x: dword;

  r: PPageInfo;
begin

  //the old method is faster, but has a small threading issue where adding new entries would lead to a memory leak if multiple threads added the same thing

  r:=pagemap.GetDataPtr(index);


  if r=nil then
  begin
    //not yet added, add it to the list
    pi.data:=nil;
    pagemapcs.enter; //adding on the other hand does require a lock, as it needs to fidn out where to add it, and what variables to initialize to what value (next/previous)

    r:=pagemap.GetDataPtr(index);

    if r=nil then
    begin
      //not yet added by another thread
      if ispointer(index shl 12) then
      begin
        //save the data
        getmem(pi.data, 4096);
        if ReadProcessMemory(ProcessHandle, pointer(index shl 12), pi.data, 4096, x)=false then //unexpected failure reading the memory
          freemem(pi.data);
      end;

      //add it
      pagemap.Add(index, pi);

      result:=pi;
    end
    else
    begin
      //another thread added it
      result:=r^;
    end;


    pagemapcs.Leave;

  end
  else //found:
    result:=r^;
end;

function TRescanHelper.getMemoryRegions: TMemoryRegions;
begin
  result:=memoryregion;
end;

procedure TRescanHelper.quicksortmemoryregions(lo,hi: integer);
var i,j: integer;
    x,h: TMemoryRegion;
begin
  i:=lo;
  j:=hi;

  x:=memoryregion[(lo+hi) div 2];

  repeat
    while (memoryregion[i].BaseAddress<x.BaseAddress) do inc(i);
    while (memoryregion[j].BaseAddress>x.BaseAddress) do dec(j);

    if i<=j then
    begin
      h:=memoryregion[i];
      memoryregion[i]:=memoryregion[j];
      memoryregion[j]:=h;
      inc(i);
      dec(j);
    end;

  until i>j;

  if (lo<j) then quicksortmemoryregions(lo,j);
  if (i<hi) then quicksortmemoryregions(i,hi);
end;

constructor TRescanHelper.create(valuesize: integer; memoryregions: PMemoryRegions=nil);
var
  mbi : _MEMORY_BASIC_INFORMATION;
  address: ptrUint;
  i: integer;
begin

  pagemap:=TMap.Create(ituPtrSize, sizeof(TPageInfo));
  pagemapcs:=TCriticalSection.create;




          {
  getmem(level0list, sizeof(TPointerListArray));
  ZeroMemory(level0list, sizeof(TPointerListArray));

  getmem(addresslist, sizeof(TAddressListArray));
  ZeroMemory(addresslist, sizeof(TAddressListArray));

  if processhandler.is64Bit then maxlevel:=15 else maxlevel:=7;
                    }

  //enumerate all memory regions
  address:=0;


  if memoryregions=nil then
  begin

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

        setlength(memoryregion,length(memoryregion)+1);

        memoryregion[length(memoryregion)-1].BaseAddress:=ptrUint(mbi.baseaddress);  //just remember this location
        memoryregion[length(memoryregion)-1].MemorySize:=mbi.RegionSize;
      end;


      address:=ptrUint(mbi.baseaddress)+mbi.RegionSize;
    end;
  end
  else
    self.memoryregion:=memoryregions^;


  //sort memoryregions from small to high
  quicksortmemoryregions(0,length(memoryregion)-1);

end;

destructor TRescanHelper.destroy;
var i: integer;
  pmi: TMapIterator;
begin
  setlength(memoryregion,0);

  pagemapcs.free;

  pmi:=TMapIterator.Create(pagemap);
  pmi.First;
  while not pmi.EOM do
  begin
    if pmi.DataPtr<>nil then
    begin
      if (PPageInfo(pmi.DataPtr).data<>nil) then
        freemem(PPageInfo(pmi.DataPtr).data);
    end;
    pmi.Next;
  end;

  pagemap.Clear;
  pagemap.Free;


  inherited destroy;
end;

end.
