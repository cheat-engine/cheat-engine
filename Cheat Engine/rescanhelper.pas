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


type TRescanHelper=class
  private
    level0list: PPointerListArray;
    dne: TPointerAddress;
    memoryregion: array of TMemoryRegion;
    function BinSearchMemRegions(address: dword): integer;
    procedure DeletePath(addresslist: PPointerListArray; level: integer);
  public
    function ispointer(address: dword): boolean;
    function AddPointer(Address: dword; value: dword): PPointerAddress;
    function findPointer(Address: dword): PPointerAddress;

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
    if (memoryregion[Pivot].BaseAddress >= address) and (address<memoryregion[Pivot].BaseAddress+memoryregion[Pivot].MemorySize) then
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
  result:=@dne;

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
    currentarray[entrynr].pointeraddress:=temp2; //publish (sure, there might be a small memleak here, but it's worth it)
    result:=temp2;
  end else result:=currentarray[entrynr].pointeraddress;
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

constructor TRescanHelper.create;
var
  mbi : _MEMORY_BASIC_INFORMATION;
  address: Dword;
begin
  dne.value:=0;

  getmem(level0list, sizeof(TPointerListArray));
  ZeroMemory(level0list, sizeof(TPointerListArray));

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
  
end;

destructor TRescanHelper.destroy;
begin
  setlength(memoryregion,0);
  deletepath(level0list,0);

  inherited destroy;
end;

end.
