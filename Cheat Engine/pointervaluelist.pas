unit pointervaluelist;

{
The pointerlist will hold a map of all possible pointer values, and the addresses that link to them
it also contains some extra information like if it's static just so the pointerscan can save some calls doing it itself eachtime
}

interface

uses windows, dialogs, SysUtils, classes, ComCtrls, cefuncproc, newkernelhandler,
     symbolhandler, math;

type
  PStaticData=^TStaticData;
  TStaticData=record
    moduleindex: dword; //for searching the saved modulelist
    offset: dword;
  end;

  TPointerDataArray=array [0..0] of record
    address: dword;
    staticdata: PStaticData; //is set to not nil if it is a static address
  end;

  PPointerDataArray=^TPointerDataArray;



  PPointerList=^TPointerList;
  TPointerlist=record
    maxsize: integer;
    pos: integer;
    list: PPointerDataArray;
  end;

  PReversePointerTable=^TReversePointerTable;
  PReversePointerListArray=^TReversePointerListArray;
  TReversePointerTable=record
    case integer of
      1: (pointerlist: PPointerList); //if this is the last level (7) this is an PPointerList
      2: (ReversePointerlistArray: PReversePointerListArray);   //else it's a PReversePointerListArray
  end;
  TReversePointerListArray=array [0..15] of TReversePointerTable;

  TReversePointerListHandler=class
  private
    memoryregion: array of TMemoryRegion;
    level0list: PReversePointerListArray;
    function BinSearchMemRegions(address: dword): integer;
    function ispointer(address: dword): boolean;
    procedure quicksortmemoryregions(lo,hi: integer);

    procedure addpointer(pointervalue: dword; pointerwiththisvalue: dword);


  public
    count: dword;

    modulelist: tstringlist;

    procedure saveModuleListToResults(s: TStream);
    function findPointerValue(var startvalue: dword; stopvalue: dword): PPointerList;
    constructor create(start, stop: dword; alligned: boolean; progressbar: tprogressbar);
  end;

implementation


type TMemoryRegion2 = record
  Address: dword;
  Size: dword;
  Protect: dword;
end;


function TReversePointerListHandler.BinSearchMemRegions(address: dword): integer;
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

function TReversePointerListHandler.ispointer(address: dword): boolean;
{
Check the memoryregion array for this address. If it's in, return true
}
begin
  result:=BinSearchMemRegions(address)<>-1;
end;

procedure TReversePointerListHandler.quicksortmemoryregions(lo,hi: integer);
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

procedure TReversePointerListHandler.addpointer(pointervalue: dword; pointerwiththisvalue: dword);
var
  level: integer;
  entrynr: integer;
  temp, currentarray: PReversePointerListArray;
  mi: Tmoduleinfo;

  templist: PPointerDataArray;
begin
  currentarray:=level0list;

  level:=0;
  while level<7 do
  begin
    //add the path if needed
    entrynr:=pointervalue shr ((7-level)*4) and $f;
    if currentarray[entrynr].ReversePointerlistArray=nil then //allocate
    begin
      getmem(temp, sizeof(TReversePointerListArray));
      ZeroMemory(temp, sizeof(TReversePointerListArray));
      currentarray[entrynr].ReversePointerlistArray:=temp;
    end;

    currentarray:=currentarray[entrynr].ReversePointerlistArray;
    inc(level);
  end;

  //got till level 7
  entrynr:=pointervalue shr ((7-level)*4) and $f;
  if currentarray[entrynr].pointerlist=nil then //allocate one
  begin
    getmem(currentarray[entrynr].pointerlist, sizeof(TPointerlist));
    ZeroMemory(currentarray[entrynr].pointerlist, sizeof(TPointerlist));

    getmem(templist, 2*sizeof(TPointerDataArray));
    ZeroMemory(templist, 2*sizeof(TPointerDataArray));

    currentarray[entrynr].pointerlist.list:=templist;
    currentarray[entrynr].pointerlist.pos:=0;
    currentarray[entrynr].pointerlist.maxsize:=2;
  end;

  if currentarray[entrynr].pointerlist.pos>=currentarray[entrynr].pointerlist.maxsize then
  begin
    currentarray[entrynr].pointerlist.maxsize:=currentarray[entrynr].pointerlist.maxsize*2; //double the storage
    ReallocMem(currentarray[entrynr].pointerlist.list, currentarray[entrynr].pointerlist.maxsize*sizeof(TPointerDataArray)); //realloc
  end;

  currentarray[entrynr].pointerlist.list[currentarray[entrynr].pointerlist.pos].address:=pointerwiththisvalue;
  if symhandler.getmodulebyaddress(pointerwiththisvalue, mi) then
  begin
    //it's a static, so create and fill in the static data
    getmem(currentarray[entrynr].pointerlist.list[currentarray[entrynr].pointerlist.pos].staticdata, sizeof(TStaticData));
    currentarray[entrynr].pointerlist.list[currentarray[entrynr].pointerlist.pos].staticdata.moduleindex:=modulelist.IndexOf(mi.modulename);
    currentarray[entrynr].pointerlist.list[currentarray[entrynr].pointerlist.pos].staticdata.offset:=pointerwiththisvalue-mi.baseaddress;
  end
  else
    currentarray[entrynr].pointerlist.list[currentarray[entrynr].pointerlist.pos].staticdata:=nil;

  inc(currentarray[entrynr].pointerlist.pos);
end;

procedure TReversePointerListHandler.saveModuleListToResults(s: TStream);
var i: integer;
  x: dword;
begin
  //save the number of modules
  x:=modulelist.Count;
  s.Write(x,sizeof(x));

  for i:=0 to modulelist.Count-1 do
  begin
    //for each module
    //save the length
    x:=length(modulelist[i]);
    s.Write(x,sizeof(x));

    //and the name
    s.Write(modulelist[i][1],x);
  end;
end;

type TMemrectablearraylist = array [0..7] of record
 arr:PReversePointerListArray;
 entrynr: integer;
 end;


function TReversePointerListHandler.findPointerValue(var startvalue: dword; stopvalue: dword): PPointerList;
var
  a: TMemrectablearraylist;
  level: integer;
  currentarray: PReversePointerListArray;
  entrynr: integer;
begin
  result:=nil;
  level:=0;
  a[0].arr:=level0list;

  while startvalue<=stopvalue do
  begin
    currentarray:=a[level].arr;
    entrynr:=startvalue shr ((7-level)*4) and $f;
    if currentarray[entrynr].ReversePointerlistArray=nil then
    begin
      //select next entry
      startvalue:=startvalue shr ((7-level)*4);
      startvalue:=startvalue+1;
      startvalue:=startvalue shl ((7-level)*4);

      if entrynr=15 then
      begin
        if level>0 then
          dec(level)
        else
          exit;
      end;

    end
    else
    begin
      if level=7 then
      begin
        result:=currentarray[entrynr].pointerlist;
        break;
      end
      else
      begin
        a[level+1].arr:=currentarray[entrynr].ReversePointerlistArray;
        a[level].entrynr:=entrynr;
        inc(level);
      end;
    end;

  end;

end;



constructor TReversePointerListHandler.create(start, stop: dword; alligned: boolean; progressbar: tprogressbar);
var bytepointer: PByte;
    dwordpointer: PDword absolute bytepointer;


    mbi : _MEMORY_BASIC_INFORMATION;
    address: Dword;
    size:       dword;

    i: Integer;
    j: Integer;

    actualread: dword;
    TotalToRead: Dword;

    maxsize: dword;

    buffer: pointer;

    memoryregion2: array of TMemoryRegion2;
    lastaddress: dword;


begin
  modulelist:=tstringlist.create;
  symhandler.getModuleList(modulelist);


  count:=0;

  address:=start;

  getmem(level0list, sizeof(TReversePointerListArray));
  ZeroMemory(level0list, sizeof(TReversePointerListArray));

  while (Virtualqueryex(processhandle,pointer(address),mbi,sizeof(mbi))<>0) and (address<stop) and ((address+mbi.RegionSize)>address) do
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


  maxsize:=0;
  for i:=0 to length(memoryregion)-1 do
    maxsize:=max(maxsize, memoryregion[i].MemorySize);

  try
    getmem(buffer,maxsize);
  except
    raise exception.Create('Not enough memory free to scan');
  end;

  buffersize:=totaltoread;



  for i:=0 to length(memoryregion)-1 do
  begin
    actualread:=0;
    if readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),buffer,Memoryregion[i].MemorySize,actualread) then
    begin
      bytepointer:=buffer;    
      lastaddress:=dword(buffer)+Memoryregion[i].MemorySize;

      while dword(bytepointer)<lastaddress do
      begin
        if (alligned and ((dwordpointer^ mod 4)=0) and ispointer(dwordpointer^)) or
           ((not alligned) and ispointer(dwordpointer^) ) then
        begin
          //add it
          addpointer(dwordpointer^, Memoryregion[i].BaseAddress+(dword(dwordpointer)-dword(buffer)));
          inc(count);
        end;

        if alligned then
          inc(dwordpointer)
        else
          inc(bytepointer);
      end;
    end;

    progressbar.StepIt;
  end;

  progressbar.Position:=0;
 // showmessage('count='+inttostr(count));
end;

end.
