unit rescanhelper;

{$MODE Delphi}

interface

uses windows, LCLIntf, classes, symbolhandler, CEFuncProc,NewKernelHandler, maps,
  sysutils, syncobjs, pagemap, Sockets, CELazySocket, PointerscanNetworkCommands,
  zstream;

type


  TRescanHelper=class
  private
    pagemap: TPageMap;
    pagemapcs: TCriticalsection;

    memoryregion: TMemoryRegions;

    socket: TSocket;
    socketcs: TCriticalSection;

    procedure downloadMemoryRegions;
    function DownloadPages(page: PtrUInt): TPageInfo;

    function BinSearchMemRegions(address: ptrUint): integer;
    procedure quicksortmemoryregions(lo,hi: integer);
  public
    function ispointer(address: ptrUint): boolean;
    function GetMemoryRegion(Address: ptruint): TMemoryRegion;

    function FindPage(index: ptruint): TPageInfo;

    function getMemoryRegions: TMemoryRegions;


    constructor create(socket: Tsocket=INVALID_SOCKET; socketcs: TCriticalSection=nil );
    destructor destroy; override;
end;

implementation

uses ProcessHandlerUnit;

function TRescanHelper.DownloadPages(page: ptruint): TPageInfo;
var
  getPages: packed record
    command: byte;
    base: qword;
    count: byte;
  end;
  base: ptruint;
  r: PPageInfo;
  i: integer;
  pageindex, originalpageindex: ptruint;

  pagecount: dword;
  pages: array of TPageInfo;

  readable: byte;
  compressedSize: dword;

  ns: TNetworkStream;
  ds: TDecompressionStream;

  buffer: pointer;

begin
  result.data:=nil;
  socketcs.enter;
  try
    base:=page and qword($fffffffffffff000);

    r:=pagemap.GetPageInfo(base shr 12);
    if r=nil then
    begin
      //still needs to be added

      getpages.command:=RCMD_GETPAGES;
      getpages.base:=base;
      getpages.count:=1;

      //check if there are more pages to download
      if pagemap.GetPageInfo((base shr 12)-1)=nil then
      begin
        dec(getpages.base, 4096);
        inc(getpages.count);

        if pagemap.GetPageInfo((base shr 12)-2)=nil then
        begin
          dec(getpages.base, 4096);
          inc(getpages.count);
        end;
      end;

      if pagemap.GetPageInfo((base shr 12)+1)=nil then
      begin
        inc(getpages.count);
        if pagemap.GetPageInfo((base shr 12)+2)=nil then
          inc(getpages.count);
      end;


      send(socket, @getpages, sizeof(getpages));
      setlength(pages, getpages.count);

      receive(socket, @pagecount, sizeof(pagecount));

      ns:=tnetworkstream.create;
      try

        for i:=0 to pagecount-1 do
        begin
          receive(socket, @readable, sizeof(readable));
          if readable=0 then
            pages[i].data:=nil
          else
          begin
            receive(socket, @compressedsize, sizeof(compressedsize));

            ns.Clear;
            ns.ReadFromSocket(socket, compressedsize);

            ns.position:=0;
            ds:=Tdecompressionstream.create(ns);

            getmem(pages[i].data, 4096);
            ds.ReadBuffer(pages[i].data^, 4096);
            ds.free;
          end;
        end;

      finally
        ns.free;
      end;


      originalpageindex:=page shr 12;
      pagemapcs.enter;
      for i:=0 to pagecount-1 do
      begin
        pageindex:=(getpages.base+i*4096) shr 12;
        r:=pagemap.GetPageInfo(pageindex);
        if r=nil then //add it
          r:=pagemap.Add(pageindex, pages[i].data);

        if pageindex=originalpageindex then
          result:=r^;
      end;
      pagemapcs.leave;

    end
    else
    begin
      //already added
      result:=r^;
    end;


  finally
    socketcs.Leave;
  end;

end;

procedure TRescanHelper.downloadMemoryRegions;
var
  command: byte;
  count: dword;
  i: integer;

  baseaddress, size: qword;
begin
  //download the memory regions from the socket
  socketcs.enter;
  try
    command:=RCMD_GETMEMORYREGIONS;
    send(socket, @command, sizeof(command));
    receive(socket, @count, sizeof(count));

    setlength(memoryregion, count);
    for i:=0 to count-1 do
    begin
      receive(socket, @baseaddress, sizeof(baseaddress));
      receive(socket, @size, sizeof(size));

      memoryregion[i].baseaddress:=baseaddress;
      memoryregion[i].MemorySize:=size;
    end;

  finally
    socketcs.leave;
  end;
end;

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
  x: ptruint;

  r: PPageInfo;
begin

  //the old method is faster, but has a small threading issue where adding new entries would lead to a memory leak if multiple threads added the same thing

  r:=pagemap.GetPageInfo(index);


  if r=nil then
  begin
    //not yet added, add it to the list
    pi.data:=nil;
    if ispointer(index shl 12) then
    begin
      //save the data
      if socket>=0 then
      begin
        result:=DownloadPages(index shl 12);
        exit;
      end
      else
      begin
        getmem(pi.data, 4096);
        if ReadProcessMemory(ProcessHandle, pointer(index shl 12), pi.data, 4096, x)=false then
        begin
          //unexpected failure reading the memory
          freemem(pi.data);
          pi.data:=nil;
        end;
      end;
    end;

    pagemapcs.enter; //adding on the other hand does require a lock, as it needs to fidn out where to add it, and what variables to initialize to what value (next/previous)

    r:=pagemap.GetPageInfo(index);
    if r=nil then
    begin
      //not yet added by another thread

      //add it
      pagemap.Add(index, pi.data);

      result:=pi;
    end
    else
    begin
      //another thread added it, abort
      if pi.data<>nil then
        freemem(pi.data);

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

  if hi<0 then exit;

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

constructor TRescanHelper.create(socket: Tsocket=INVALID_SOCKET; socketcs: TCriticalSection=nil );
var
  mbi : _MEMORY_BASIC_INFORMATION;
  address: ptrUint;
  i: integer;
begin

  pagemap:=TPageMap.Create;
  pagemapcs:=TCriticalSection.create;

  self.socket:=socket;
  self.socketcs:=socketcs;


  //enumerate all memory regions
  address:=0;

  if socket>=0 then
    downloadMemoryRegions
  else
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
  end;



  //sort memoryregions from small to high
  quicksortmemoryregions(0,length(memoryregion)-1);

end;

destructor TRescanHelper.destroy;
var i: integer;
  pmi: TMapIterator;
begin
  setlength(memoryregion,0);

  pagemapcs.free;
  pagemap.Free;


  inherited destroy;
end;

end.
