unit rescanhelper;

interface

uses windows, classes, symbolhandler, cefuncproc,newkernelhandler;

type TPointerAddress=record
  value: dword;
end;


type
  PPointerAddress=^TPointerAddress;

  PPointerTable=^TPointerTable;
  PPointerListArray=^TPointerListArray;
  TPointerTable=record
    case integer of
      1: (pointeraddress: PPointerAddress); //if this is the last level (7) this is an PPointerList
      2: (PointerlistArray: PPointerListArray);   //else it's a PReversePointerListArray
  end;
  TPointerListArray=array [0..15] of TPointerTable;



type
  PAddressTable=^TAddressTable;
  PAddressListArray=^TAddressListArray;
  TAddressTable=record
    case integer of
      1: (addressvalue: pointer); //if this is the last level (7) this is a pointer to a type of the value to scan for (4 byute and single are 4 byte, double is 8 byte)
      2: (AddresslistArray: PAddressListArray);   //else it's a PReversePointerListArray
  end;
  TAddressListArray=array [0..15] of TAddressTable;

type TRescanHelper=class
  private
    level0list: PPointerListArray;
    addresslist: PAddressListArray;
    dne: TPointerAddress;
    memoryregion: array of TMemoryRegion;
    function BinSearchMemRegions(address: dword): integer;
    procedure DeletePath(addresslist: PPointerListArray; level: integer);
    procedure DeleteAddressPath(addresslist: PAddressListArray; level: integer);
    procedure quicksortmemoryregions(lo,hi: integer);
  public
    function ispointer(address: dword): boolean;
    function AddPointer(Address: dword; value: dword): PPointerAddress;
    function findPointer(Address: dword): PPointerAddress;

    function AddAddress(Address: dword; value: pointer; valuesize: integer): pointer;
    function findAddress(Address: dword): pointer;

    constructor create;
    destructor destroy; override;
end;

implementation

function TRescanHelper.BinSearchMemRegions(address: dword): integer;
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

function TRescanHelper.ispointer(address: dword): boolean;
{
Check the memoryregion array for this address. If it's in, return true
}
begin
  result:=BinSearchMemRegions(address)<>-1;
end;


function TRescanHelper.findPointer(Address: dword): PPointerAddress;
{
Called by a rescanworker
If it returns nil, it isn't in the list and the rescanworker will have to read it itself
if it's not nil the PointerAddress contains a dword. If it's 0, the address is not readable
}
var
  level: integer;
  entrynr: integer;
  currentarray: PPointerListArray;
begin
  //first check if this address actually exists inside a memory region, if not, just tell it the "does not exist" pointeraddress
  result:=nil;
  if not ispointer(address) then
    result:=@dne
  else
  begin
    //find the exact address
    level:=0;
    currentarray:=level0list;
    while level<7 do
    begin
      entrynr:=address shr ((7-level)*4) and $f;
      if currentarray[entrynr].PointerlistArray=nil then exit; //not found

      currentarray:=currentarray[entrynr].PointerlistArray;
      inc(level);
    end;

    entrynr:=address shr ((7-level)*4) and $f;
    if currentarray[entrynr].pointeraddress<>nil then
      result:=currentarray[entrynr].pointeraddress;
  end;
end;

function TRescanHelper.AddPointer(Address: dword; value: dword): PPointerAddress;
{Called by the rescanworker when it has read a pointer and findpointer returned NIL }
{No mutex is used here as no thread is allowed to REMOVE items, and the pointers are only set after the afterlying objects have been created}
var
  level: integer;
  entrynr: integer;
  temp, currentarray: PPointerListArray;
  temp2: PPointeraddress;
begin
  currentarray:=level0list;

  level:=0;
  while level<7 do
  begin
    //add the path if needed
    entrynr:=address shr ((7-level)*4) and $f;
    if currentarray[entrynr].PointerlistArray=nil then //allocate
    begin
      getmem(temp, sizeof(TPointerListArray));
      ZeroMemory(temp, sizeof(TPointerListArray));
      //the list has been created and initialized, tell the others:
      currentarray[entrynr].PointerlistArray:=temp;
    end;

    currentarray:=currentarray[entrynr].PointerlistArray;
    inc(level);
  end;

  //got till level 7
  entrynr:=address shr ((7-level)*4) and $f;
  if currentarray[entrynr].pointeraddress=nil then //allocate one
  begin
    getmem(temp2, sizeof(TPointerAddress));
    temp2.value:=value;
    currentarray[entrynr].pointeraddress:=temp2; //publish
    result:=temp2;
  end else result:=currentarray[entrynr].pointeraddress;
end;

function TRescanHelper.findAddress(Address: dword): pointer;
var
  level: integer;
  entrynr: integer;
  currentarray: PAddressListArray;
begin
  //first check if this address actually exists inside a memory region, if not, just tell it the "does not exist" pointeraddress
  result:=nil;

  //find the exact address
  level:=0;
  currentarray:=addresslist;
  while level<7 do
  begin
    entrynr:=address shr ((7-level)*4) and $f;
    if currentarray[entrynr].AddresslistArray=nil then exit; //not found

    currentarray:=currentarray[entrynr].AddresslistArray;
    inc(level);
  end;

  entrynr:=address shr ((7-level)*4) and $f;
  if currentarray[entrynr].addressvalue<>nil then
    result:=currentarray[entrynr].addressvalue;
end;

function TRescanHelper.AddAddress(Address: dword; value: pointer; valuesize: integer): pointer;
var
  level: integer;
  entrynr: integer;
  temp, currentarray: PAddressListArray;
  temp2: pointer;
begin
  currentarray:=addresslist;

  level:=0;
  while level<7 do
  begin
    //add the path if needed
    entrynr:=address shr ((7-level)*4) and $f;
    if currentarray[entrynr].addresslistArray=nil then //allocate
    begin
      getmem(temp, sizeof(TaddressListArray));
      ZeroMemory(temp, sizeof(TaddressListArray));
      //the list has been created and initialized, tell the others:
      currentarray[entrynr].addresslistArray:=temp;
    end;

    currentarray:=currentarray[entrynr].addresslistArray;
    inc(level);
  end;

  //got till level 7
  entrynr:=address shr ((7-level)*4) and $f;
  if currentarray[entrynr].addressvalue=nil then //allocate one
  begin
    getmem(temp2, valuesize);
    copymemory(temp2,value,valuesize);
    currentarray[entrynr].addressvalue:=temp2; //publish
    result:=temp2;
  end else result:=currentarray[entrynr].addressvalue;
end;


procedure TRescanHelper.DeletePath(addresslist: PPointerListArray; level: integer);
var
  i: integer;
begin
  if level=7 then
  begin
    for i:=0 to 15 do
    begin
      if addresslist[i].pointeraddress<>nil then
      begin
        freemem(addresslist[i].pointeraddress);
        addresslist[i].pointeraddress:=nil;
      end;
    end;
  end
  else
  begin
    for i:=0 to 15 do
    begin
      if addresslist[i].PointerlistArray<>nil then
      begin
        deletepath(addresslist[i].PointerlistArray,level+1);
        freemem(addresslist[i].PointerlistArray);
        addresslist[i].PointerlistArray:=nil;
      end;
    end;
  end;
end;

procedure TRescanHelper.DeleteAddressPath(addresslist: PAddressListArray; level: integer);
var
  i: integer;
begin
  if level=7 then
  begin
    for i:=0 to 15 do
    begin
      if addresslist[i].addressvalue<>nil then
      begin
        freemem(addresslist[i].addressvalue);
        addresslist[i].addressvalue:=nil;
      end;
    end;
  end
  else
  begin
    for i:=0 to 15 do
    begin
      if addresslist[i].AddresslistArray<>nil then
      begin
        deleteAddresspath(addresslist[i].AddresslistArray,level+1);
        freemem(addresslist[i].AddresslistArray);
        addresslist[i].AddresslistArray:=nil;
      end;
    end;
  end;
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

constructor TRescanHelper.create;
var
  mbi : _MEMORY_BASIC_INFORMATION;
  address: Dword;
  i: integer;
begin
  dne.value:=0;

  getmem(level0list, sizeof(TPointerListArray));
  ZeroMemory(level0list, sizeof(TPointerListArray));

  getmem(addresslist, sizeof(TAddressListArray));
  ZeroMemory(addresslist, sizeof(TAddressListArray));


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

      setlength(memoryregion,length(memoryregion)+1);

      memoryregion[length(memoryregion)-1].BaseAddress:=dword(mbi.baseaddress);  //just remember this location
      memoryregion[length(memoryregion)-1].MemorySize:=mbi.RegionSize;
    end;


    address:=dword(mbi.baseaddress)+mbi.RegionSize;
  end;

  //split up the memory regions into small chunks of max 512KB (so don't allocate a fucking 1GB region)
  i:=0;
  while i<length(memoryregion) do
  begin
    if memoryregion[i].MemorySize>512*1024 then
    begin
      //too big, so cut into pieces
      //create new entry with 512KB less
      setlength(memoryregion,length(memoryregion)+1);
      memoryregion[length(memoryregion)-1].BaseAddress:=memoryregion[i].BaseAddress+512*1024;
      memoryregion[length(memoryregion)-1].MemorySize:=memoryregion[i].MemorySize-512*1024;
      memoryregion[i].MemorySize:=512*1024; //set the current region to be 512KB

    end;
    inc(i); //next item. Eventually the new items will be handled, and they will also be split untill the list is finally cut into small enough chunks    
  end;

  //sort memoryregions from small to high
  quicksortmemoryregions(0,length(memoryregion)-1);  
  
end;

destructor TRescanHelper.destroy;
begin
  setlength(memoryregion,0);
  deletepath(level0list,0);

  deleteAddressPath(addresslist,0);

  inherited destroy;
end;

end.
