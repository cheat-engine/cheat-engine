unit VirtualMemory;

{$MODE Delphi}

{obsolete}

interface

uses windows, SysUtils,LCLIntf,NewKernelHandler,CEFuncProc,ComCtrls, symbolhandler, commonTypeDefs;

type TMemoryRegion2 = record
  Address: ptrUint;
  Size: ptrUint;
  Protect: dword;
end;

type TVirtualMemory = class(tobject)
  private
    buffer: pointer; //base address of all the memory
    buffersize: ptrUint;
    memoryregion: array of TMemoryRegion;
    memoryregion2: array of TMemoryRegion2;
    procedure quicksortmemoryregions(lo,hi: integer);
  public
    function AddressToPointer(address: ptrUint): pointer;
    function PointerToAddress(p: pointer): ptrUint;
    function isvalid(address: ptrUint): boolean;
    function IsBadReadPtr(x: pointer;size: integer):boolean;
    function IsBadWritePtr(x: pointer;size: integer):boolean;
    function GetBuffer: pointer;
    function GetBufferSize: ptrUint;
    constructor Create(Start,Stop: ptrUint; progressbar: tprogressbar); //will load the current process in memory (say goodbye to memory....)
    destructor destroy; override;
end;


implementation

uses ProcessHandlerUnit, Globals;

resourcestring
  rsNoMemoryFoundInTheSpecifiedRegion = 'No memory found in the specified '
    +'region';
  rsNotEnoughMemoryFreeToScan = 'Not enough memory free to scan';

function TVirtualMemory.GetBuffer:pointer;
begin
  result:=buffer;
end;

function TVirtualMemory.GetBufferSize: ptrUint;
begin
  result:=self.buffersize;
end;

function TVirtualMemory.isvalid(address: ptrUint): boolean;
begin
  result:=AddressToPointer(address)<>nil;
end;

function TVirtualMemory.IsBadReadPtr(x: pointer;size: integer):boolean;
begin
  result:=AddressToPointer(ptrUint(x))=nil;
end;

function TVirtualMemory.IsBadWritePtr(x: pointer;size: integer):boolean;
var i: integer;
    address: ptrUint;
begin
  result:=true;
  address:=ptrUint(x);

  for i:=0 to length(memoryregion2)-1 do
  begin
    if memoryregion2[i].Address>address then exit;
    if (memoryregion2[i].Address+memoryregion2[i].Size)>address then
    begin
      if ((memoryregion2[i].Protect and PAGE_READWRITE)=PAGE_READWRITE) or ((memoryregion2[i].Protect and PAGE_EXECUTE_READWRITE)=PAGE_EXECUTE_READWRITE) then result:=false;
      exit;
    end;
  end;
end;

function TVirtualMemory.PointerToAddress(p: pointer): ptrUint;
{
returns the address the pointer points to
}
var offset: ptrUint;
    i: integer;
begin
  result:=0;
  offset:=ptrUint(p)-ptrUint(buffer); //difference from start of the buffer till p
  for i:=0 to length(memoryregion)-1 do
  begin
    if ptrUint(memoryregion[i].startaddress) > (ptrUint(memoryregion[0].startaddress)+offset) then
    begin
//      raise exception.Create('Invalid address is being accessed');

      exit;
    end;

    if (
         (ptrUint(p) >= ptrUint(memoryregion[i].startaddress) ) and
         (ptrUint(p) < (ptrUint(memoryregion[i].startaddress)+memoryregion[i].MemorySize)  )
       )
    then
    begin
      result:=memoryregion[i].BaseAddress+(ptrUint(p)-ptrUint(memoryregion[i].startaddress));
      exit;
    end;

  end;
end;

function TVirtualMemory.AddressToPointer(address: ptrUint): pointer;
var i: integer;
begin
  result:=nil;
  try


    for i:=0 to length(memoryregion)-1 do
    begin
      if memoryregion[i].BaseAddress>address then exit; //sorted so higher will never be found
      if (memoryregion[i].BaseAddress+memoryregion[i].MemorySize)>address then
      begin
        result:=pointer(ptrUint(memoryregion[i].startaddress)+(address-memoryregion[i].BaseAddress));
        exit;
      end;
    end;

  except
    //messagebox(0,pchar(format('address=%.8x i=%d length(memoryregion)=%d',[address,i,length(memoryregion)])),'error',mb_ok);
    //raise exception.create('unnhandled error');
  end;

  //still here

end;

procedure TVirtualMemory.quicksortmemoryregions(lo,hi: integer);
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

constructor TVirtualMemory.Create(Start,Stop: ptrUint; progressbar: tprogressbar);
var RMPointer: ^Byte;

    mbi : _MEMORY_BASIC_INFORMATION;
    address: ptrUint;
    size:       ptrUint;

    i: Integer;
    j: Integer;

    actualread: ptruint;
    TotalToRead: ptrUint;
begin
  address:=start;

  while (Virtualqueryex(processhandle,pointer(address),mbi,sizeof(mbi))<>0) and (address<stop) and ((address+mbi.RegionSize)>address) do
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

      setlength(memoryregion,length(memoryregion)+1);

      memoryregion[length(memoryregion)-1].BaseAddress:=ptrUint(mbi.baseaddress);  //just remember this location
      memoryregion[length(memoryregion)-1].MemorySize:=mbi.RegionSize;
    end;


    address:=ptrUint(mbi.baseaddress)+mbi.RegionSize;
  end;

  setlength(memoryregion2,length(memoryregion));
  for i:=0 to length(memoryregion2)-1 do
  begin
    memoryregion2[i].Address:=memoryregion[i].BaseAddress;
    memoryregion2[i].Size:=memoryregion[i].MemorySize;


    if Virtualqueryex(processhandle,pointer(memoryregion2[i].Address),mbi,sizeof(mbi))<>0 then
      memoryregion2[i].Protect:=mbi.Protect
    else
      memoryregion2[i].Protect:=page_readonly;
  end;


  if length(memoryregion)=0 then raise exception.create(
    rsNoMemoryFoundInTheSpecifiedRegion);

  //lets search really at the start of the location the user specified
  if (memoryregion[0].BaseAddress<start) and (memoryregion[0].MemorySize-(start-memoryregion[0].BaseAddress)>0) then
  begin
    memoryregion[0].MemorySize:=memoryregion[0].MemorySize-(start-memoryregion[0].BaseAddress);
    memoryregion[0].BaseAddress:=start;
  end;

  //also the right end
  if (memoryregion[length(memoryregion)-1].BaseAddress+memoryregion[length(memoryregion)-1].MemorySize)>stop then
    dec(memoryregion[length(memoryregion)-1].MemorySize,(memoryregion[length(memoryregion)-1].BaseAddress+memoryregion[length(memoryregion)-1].MemorySize)-stop-1);

  //if anything went ok memoryregions should now contain all the addresses and sizes
  //to speed it up combine the regions that are attached to eachother.

  j:=0;
  address:=memoryregion[0].BaseAddress;
  size:=memoryregion[0].MemorySize;

  for i:=1 to length(memoryregion)-1 do
  begin
    if memoryregion[i].BaseAddress=address+size then
      inc(size,memoryregion[i].MemorySize)
    else
    begin
      memoryregion[j].BaseAddress:=address;
      memoryregion[j].MemorySize:=size;

      address:=memoryregion[i].BaseAddress;
      size:=memoryregion[i].MemorySize;
      inc(j);
    end;
  end;

  memoryregion[j].BaseAddress:=address;
  memoryregion[j].MemorySize:=size;
  setlength(memoryregion,j+1);

  //sort memoryregions from small to high
  quicksortmemoryregions(0,length(memoryregion)-1);

  TotalToRead:=0;
  For i:=0 to length(memoryregion)-1 do
    inc(TotalToRead,Memoryregion[i].MemorySize);

  progressbar.Min:=0;
  progressbar.Step:=1;
  progressbar.Position:=0;
  progressbar.max:=length(memoryregion)+1;

  try
    getmem(buffer,TotalToRead);
  except
    raise exception.Create(rsNotEnoughMemoryFreeToScan);
  end;

  self.buffersize:=totaltoread;

  RMPointer:=pointer(buffer);

  for i:=0 to length(memoryregion)-1 do
  begin
    actualread:=0;
    readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),RMPointer,Memoryregion[i].MemorySize,actualread);

    Memoryregion[i].MemorySize:=actualread;
    Memoryregion[i].startaddress:=RMPointer;
    inc(RMPointer,actualread);

    progressbar.StepIt;
  end;
end;

destructor TVirtualMemory.destroy;
begin
  if buffer<>nil then freememandnil(buffer);
  setlength(memoryregion,0);
  setlength(memoryregion2,0);
  inherited destroy;
end;

end.




