unit VirtualMemory;

interface

uses SysUtils,windows,newkernelhandler,cefuncproc,ComCtrls;

type TMemoryRegion2 = record
  Address: dword;
  Size: dword;
  Protect: dword;
end;

type TVirtualMemory = class(tobject)
  private
    buffer: pointer; //base address of all the memory
    buffersize: dword;
    memoryregion: array of TMemoryRegion;
    memoryregion2: array of TMemoryRegion2;
    procedure quicksortmemoryregions(lo,hi: integer);
  public
    function AddressToPointer(address: dword): pointer;
    function PointerToAddress(p: pointer): dword;
    function isvalid(address: dword): boolean;
    function IsBadReadPtr(x: pointer;size: integer):boolean;
    function IsBadWritePtr(x: pointer;size: integer):boolean;
    function GetBuffer: pointer;
    function GetBufferSize: dword;
    constructor Create(Start,Stop: dword; progressbar: tprogressbar); //will load the current process in memory (say goodbye to memory....)
    destructor destroy;
end;


implementation



function TVirtualMemory.GetBuffer:pointer;
begin
  result:=buffer;
end;

function TVirtualMemory.GetBufferSize: dword;
begin
  result:=buffersize;
end;

function TVirtualMemory.isvalid(address: dword): boolean;
begin
  result:=AddressToPointer(address)<>nil;
end;

function TVirtualMemory.IsBadReadPtr(x: pointer;size: integer):boolean;
begin
  result:=AddressToPointer(dword(x))=nil;
end;

function TVirtualMemory.IsBadWritePtr(x: pointer;size: integer):boolean;
var i: integer;
    address: dword;
begin
  result:=true;
  address:=dword(x);

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

function TVirtualMemory.PointerToAddress(p: pointer): dword;
{
returns the address the pointer points to
}
var offset: dword;
    i: integer;
begin
  result:=0;
  offset:=dword(p)-dword(buffer); //difference from start of the buffer till p
  for i:=0 to length(memoryregion)-1 do
  begin
    if dword(memoryregion[i].startaddress) > (dword(memoryregion[0].startaddress)+offset) then exit;

    if (
         (dword(p) >= dword(memoryregion[i].startaddress) ) and
         (dword(p) < (dword(memoryregion[i].startaddress)+memoryregion[i].MemorySize)  )
       )
    then
    begin
      result:=memoryregion[i].BaseAddress+(dword(p)-dword(memoryregion[i].startaddress));
      exit;
    end;

  end;
end;

function TVirtualMemory.AddressToPointer(address: dword): pointer;
var i: integer;
begin
try
  result:=nil;

  for i:=0 to length(memoryregion)-1 do
  begin
    if memoryregion[i].BaseAddress>address then exit; //sorted so higher will never be found
    if (memoryregion[i].BaseAddress+memoryregion[i].MemorySize)>address then
    begin
      result:=pointer(dword(memoryregion[i].startaddress)+(address-memoryregion[i].BaseAddress));
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

constructor TVirtualMemory.Create(Start,Stop: dword; progressbar: tprogressbar);
var RMPointer: ^Byte;

    mbi : _MEMORY_BASIC_INFORMATION;
    address: Dword;
    size:       dword;

    i: Integer;
    j: Integer;

    actualread: dword;
    TotalToRead: Dword;
    Total: dword;

    NO: boolean;
begin
  address:=start;

  while (Virtualqueryex(processhandle,pointer(address),mbi,sizeof(mbi))<>0) and (address<stop) and ((address+mbi.RegionSize)>address) do
  begin
    if (not (not scan_mem_private and (mbi.type_9=mem_private))) and (not (not scan_mem_image and (mbi.type_9=mem_image))) and (not (not scan_mem_mapped and (mbi.type_9=mem_mapped))) and (mbi.State=mem_commit) and ((mbi.Protect and page_guard)=0) and ((mbi.protect and page_noaccess)=0) then  //look if it is commited
    begin
      if Skip_PAGE_NOCACHE then
        if (mbi.AllocationProtect and PAGE_NOCACHE)=PAGE_NOCACHE then
        begin
          address:=dword(mbi.BaseAddress)+mbi.RegionSize;
          continue;
        end;

      setlength(memoryregion,length(memoryregion)+1);

      memoryregion[length(memoryregion)-1].BaseAddress:=dword(mbi.baseaddress);  //just remember this location
      memoryregion[length(memoryregion)-1].MemorySize:=mbi.RegionSize;
    end;


    address:=dword(mbi.baseaddress)+mbi.RegionSize;
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


  if length(memoryregion)=0 then raise exception.create('No memory found in the specified region');

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

  for i:=1 to length(memoryregion) do
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
    on EOutOfMemory do
      begin
      //not enough free memory for the 2nd
        raise exception.Create('Not enough memory free to scan');
      end;
  end;

  buffersize:=totaltoread;

  RMPointer:=pointer(buffer);

  total:=0;
  for i:=0 to length(memoryregion)-1 do
  begin
    actualread:=0;
    readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),RMPointer,Memoryregion[i].MemorySize,actualread);

    inc(total,actualread);
    Memoryregion[i].MemorySize:=actualread;
    Memoryregion[i].startaddress:=RMPointer;
    inc(RMPointer,actualread);

    progressbar.StepIt;
  end;
end;

destructor TVirtualMemory.destroy;
begin
  freemem(buffer);
  setlength(memoryregion,0);
  setlength(memoryregion2,0);
  inherited destroy;
end;

end.


