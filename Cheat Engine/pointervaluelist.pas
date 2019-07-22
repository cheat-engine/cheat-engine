unit pointervaluelist;

{$MODE Delphi}

{
The pointerlist will hold a map of all possible pointer values, and the addresses that link to them
it also contains some extra information like if it's static just so the pointerscan can save some calls doing it itself eachtime

todo: Try a tree/map thing.
result: tree/map was slower than my own non threadsafe implementation. After initialization reads don't need to be safe since nothing adds to it, so a default implementation with locks is a slowdown
}

interface

uses windows, LCLIntf, dialogs, SysUtils, classes, ComCtrls, CEFuncProc,
     NewKernelHandler, symbolhandler, symbolhandlerstructs, math,
     bigmemallochandler, maps, luahandler, lua, lauxlib, lualib, luaclass,
     LuaObject, zstream;

const scandataversion=1;

type
  PStaticData=^TStaticData;
  TStaticData=record
    moduleindex: dword; //for searching the saved modulelist
    offset: PtrInt; //converted from integer to ptrint as static addresses can point to high values as well.(Interpret as unsigned when moduleindex=-1)
  end;

  TPointerDataArray=array [0..0] of record
    address: ptrUint;
    staticdata: PStaticData; //is set to not nil if it is a static address
  end;

  PPointerDataArray=^TPointerDataArray;


  TMemoryRegion2 = record
    BaseAddress: ptrUint;
    MemorySize: dword;
    InModule: boolean;
    ValidPointerRange: boolean;
  end;


  PPointerList=^TPointerList;
  TPointerlist=record
    maxsize: integer;
    expectedsize: integer;
    pos: integer;
    list: PPointerDataArray;

    //Linked list
    PointerValue: ptrUint;
    Previous: PPointerList;
    Next: PPointerList;
  end;

  PReversePointerTable=^TReversePointerTable;
  PReversePointerListArray=^TReversePointerListArray;
  TReversePointerTable=record
    case integer of
      1: (pointerlist: PPointerList); //if this is the last level (maxlevel) this is an PPointerList
      2: (ReversePointerlistArray: PReversePointerListArray);   //else it's a PReversePointerListArray
  end;
  TReversePointerListArray=array [0..15] of TReversePointerTable;



  //-
  TMemrectablearraylist = array [0..15] of record
     arr:PReversePointerListArray;
     entrynr: ValUInt;
  end;

  PMemrectablearraylist=^TMemrectablearraylist;
  //^

  TReversePointerListHandler=class
  private
    memoryregion: array of TMemoryRegion2;
    level0list: PReversePointerListArray;
    maxlevel: ValUInt;

    firstPointerValue: PPointerList;
    lastPointerValue: PPointerList;


    bigalloc: TBigMemoryAllocHandler;

    specificBaseAsStaticOnly: boolean;
    basestart: ptruint;
    basestop: ptruint;


    useStacks: boolean;
    threadStacks: integer;
    stacksAsStaticOnly: boolean;
    stacksize: integer;

    stacklist: array of ptruint;

    ScannablePages: TMap;

    function BinSearchMemRegions(address: ptrUint): integer;
    function isModulePointer(address: ptrUint): boolean;
    function ispointer(address: ptrUint): boolean;
    function isStatic(address: ptruint; var mi: TModuleInfo; var moduleindex: integer): boolean;
    function isStatic2(address: ptruint; var mi: TModuleInfo; var moduleindex: integer): boolean;
    procedure quicksortmemoryregions(lo,hi: integer);

    procedure addpointer(pointervalue: ptrUint; pointerwiththisvalue: ptrUint; add: boolean);
    function findoraddpointervalue(pointervalue: ptrUint): PPointerList;
    procedure DeletePath(addresslist: PReversePointerListArray; level: integer);

    //--
    function findMaxOfPath(lvl: PMemrectablearraylist; a: PReversePointerListArray; level: ValUInt):PPointerList;
    function findprevious(lvl: PMemrectablearraylist; level: ValUInt):PPointerList;
    function findClosestPointer(addresslist: PReversePointerListArray; entrynr: integer; level: integer; maxvalue: ptrUint): PPointerList;

    procedure fillList(addresslist: PReversePointerListArray; level: integer; var prev: PPointerList);
    procedure fillLinkedList;

    procedure LoadModuleList(s: TStream);
    function  LoadHeader(s: TStream): qword;
  public
    count: qword;


    modulelist: tstringlist;


    function is64bit: boolean;
    procedure exportToStream(s: TStream; pb: TProgressbar=nil);

    procedure saveModuleListToResults(s: TStream);

    function findPointerValue(startvalue: ptrUint; var stopvalue: ptrUint): PPointerList;
    constructor create(start, stop: ptrUint; alligned: boolean; progressbar: tprogressbar; noreadonly: boolean; mustbeclasspointers, allowNonModulePointers: boolean; useStacks: boolean; stacksAsStaticOnly: boolean; threadstacks: integer; stacksize: integer; specificBaseAsStaticOnly: boolean; baseStart: ptruint; baseStop: ptruint; includeSystemModules: boolean=false; regionfilename: string='');
    constructor createFromStream(s: TStream; progressbar: tprogressbar=nil);
    constructor createFromStreamHeaderOnly(s: TStream);
    destructor destroy; override;

    property CanHaveStatic: boolean read specificBaseAsStaticOnly;
  end;

  procedure initializeLuaPointerValueList;

implementation

uses ProcessHandlerUnit, globals, DBK32functions;

resourcestring
  rsPointerValueSetupError = 'Pointer value setup error';
  rsNoMemoryFoundInTheSpecifiedRegion = 'No memory found in the specified '
    +'region';
  rsAllocatingBytesToBuffer = 'Allocating %s bytes to ''buffer''';
  rsPVInvalidScandataFile = 'Invalid scandata file';
  rsPVInvalidScandataVersion = 'Invalid scandata version';
  rsPVNotEnoughMemoryFreeToScan = 'Not enough memory free to scan';

function TReversePointerListHandler.BinSearchMemRegions(address: ptrUint): integer;
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
    if (address>=memoryregion[Pivot].BaseAddress ) and (address<memoryregion[Pivot].BaseAddress+memoryregion[Pivot].MemorySize) then
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

function TReversePointerListHandler.isModulePointer(address: ptrUint): boolean;
var i: integer;
begin
  i:=BinSearchMemRegions(address);
  result:=(i<>-1) and (memoryregion[i].InModule);
end;

function TReversePointerListHandler.ispointer(address: ptrUint): boolean;
{
Check the memoryregion array for this address. If it's in, return true
}
var
  i: integer;
  pfn: ptruint;

begin
  if address=0 then exit(false);

  i:=BinSearchMemRegions(address);
  result:=(i<>-1) and (memoryregion[i].ValidPointerRange);

  if result and (scannablepages<>nil) then
  begin
    pfn:=address shr 12;
    result:=scannablepages.HasId(pfn);
  end;
end;

procedure TReversePointerListHandler.quicksortmemoryregions(lo,hi: integer);
var i,j: integer;
    x,h: TMemoryRegion2;
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

function TReversePointerListHandler.isStatic2(address: ptruint; var mi: TModuleInfo; var moduleindex: integer): boolean;
var i: integer;
begin
  result:=false;
  if useStacks then
  begin
    for i:=0 to threadStacks-1 do
    begin
      if InRangeQ(address, stacklist[i]-stacksize, stacklist[i]) then
      begin
        mi.baseaddress:=stacklist[i];    //fills mi.baseaddress
        result:=true;
      end;
    end;
  end;

  if (result=false) and (stacksAsStaticOnly=false) then
    result:=symhandler.getmodulebyaddress(address, mi);  //fills mi.baseaddress

  if result then
  begin
    for i:=0 to modulelist.count-1 do
    begin
      if ptruint(modulelist.Objects[i])=mi.baseaddress then
      begin
        moduleindex:=i;    //fills moduleindex
        exit;
      end;
    end;
  end;
end;

function TReversePointerListHandler.isStatic(address: ptruint; var mi: TModuleInfo; var moduleindex: integer): boolean;
begin
  if specificBaseAsStaticOnly then
  begin
    result:=(address>=basestart) and (address<=basestop);
    if result then
    begin
      //setup as a direct address
      moduleindex:=-1;
      mi.baseaddress:=0;

      //now lookup if it's actually a static
      isStatic2(address, mi, moduleindex);
    end;
  end
  else
    result:=isStatic2(address, mi, moduleindex);
end;

function TReversePointerListHandler.findoraddpointervalue(pointervalue: ptrUint): PPointerList;
var
  level: integer;
  entrynr: integer;
  temp, currentarray: PReversePointerListArray;
  plist: PPointerList;

  size: integer;
begin
  currentarray:=level0list;

  level:=0;

  while level<maxlevel do
  begin
    //add the path if needed
    entrynr:=pointervalue shr ((maxlevel-level)*4) and $f;
    if currentarray[entrynr].ReversePointerlistArray=nil then //allocate
    begin
      size:=sizeof(TReversePointerListArray);

      temp:=bigalloc.alloc(size);
      ZeroMemory(temp, size);
      currentarray[entrynr].ReversePointerlistArray:=temp;
    end;

    currentarray:=currentarray[entrynr].ReversePointerlistArray;
    inc(level);
  end;

  //got till level (maxlevel)

  entrynr:=pointervalue shr ((maxlevel-level)*4) and $f;
  plist:=currentarray[entrynr].pointerlist;

  if plist=nil then //allocate one
  begin
    currentarray[entrynr].pointerlist:=bigalloc.alloc(sizeof(TPointerlist));
    plist:=currentarray[entrynr].pointerlist;
    plist.PointerValue:=pointervalue;

    plist.list:=nil;
    plist.pos:=0;
    plist.maxsize:=0;

    //guess the number of pointers there will be with this exact value

    plist.expectedsize:=1;

    if pointervalue mod $10=0 then
    begin
      plist.expectedsize:=5;
      if pointervalue mod $100=0 then
      begin
        plist.expectedsize:=10;
        if pointervalue mod $1000=0 then
        begin
          plist.expectedsize:=20;
          if pointervalue mod $10000=0 then
            plist.expectedsize:=50;
        end;
      end;

    end;

  end;

  result:=plist;
end;


procedure TReversePointerListHandler.addpointer(pointervalue: ptrUint; pointerwiththisvalue: ptrUint; add: boolean);
var
  mi: Tmoduleinfo;

  plist: PPointerList;

  size: integer;
  moduleindex: integer;

begin
  plist:=findoraddpointervalue(pointervalue);

  if not add then
    inc(plist.expectedsize)
  else
  begin
    //actually add a pointer address for this value
    if plist.list=nil then //create the list
    begin
      plist.list:=bigalloc.alloc(plist.expectedsize*sizeof(TPointerDataArray));
      ZeroMemory(plist.list, plist.expectedsize*sizeof(TPointerDataArray));

      plist.maxsize:=plist.expectedsize;
    end;

    if plist.pos>=plist.maxsize then  //the new entry will be over the maximum. Reallocate
    begin
      //quadrupple the storage
      plist.list:=bigalloc.realloc(plist.list, plist.maxsize*sizeof(TPointerDataArray), plist.maxsize*sizeof(TPointerDataArray)*4);
      plist.maxsize:=plist.maxsize*4;
    end;

    plist.list[plist.pos].address:=pointerwiththisvalue;

    if isStatic(pointerwiththisvalue, mi, moduleindex) then
    begin
      //it's a static, so create and fill in the static data
      plist^.list[plist.pos].staticdata:=bigalloc.alloc(sizeof(TStaticData));
      plist^.list[plist.pos].staticdata.moduleindex:=moduleindex;
      plist^.list[plist.pos].staticdata.offset:=pointerwiththisvalue-mi.baseaddress;
    end
    else
      plist.list[plist.pos].staticdata:=nil;

    inc(plist.pos);
  end;
end;

procedure TReversePointerListHandler.saveModuleListToResults(s: TStream);
var i: integer;
begin
  //save the number of modules
  s.WriteDWord(modulelist.Count);

  for i:=0 to modulelist.Count-1 do
  begin
    //for each module
    //save the length
    s.WriteDWord(length(modulelist[i]));

    //and the name
    s.Write(modulelist[i][1],length(modulelist[i]));

    //and the module base (in case of rescans that only use this info)
    s.WriteQWord(qword(modulelist.Objects[i]));
  end;
end;





function TReversePointerListHandler.findMaxOfPath(lvl: PMemrectablearraylist; a: PReversePointerListArray; level: ValUInt):PPointerList;
var
  i: ValSInt;
begin
  result:=nil;
  if level<maxlevel then
  begin

    for i:=$F downto 0 do
    begin
      if a[i].ReversePointerlistArray<>nil then
      begin
        lvl[level].entrynr:=i;
        lvl[level+1].arr:=a[i].ReversePointerlistArray;
        result:=findMaxOfPath(lvl, a[i].ReversePointerlistArray,level+1);
        if result<>nil then exit;
      end;
    end;
  end
  else
  begin
    //end reached
    for i:=$F downto 0 do
    begin
      if a[i].pointerlist<>nil then
      begin
        lvl[level].entrynr:=i;
        result:=a[i].pointerlist;
        exit;
      end;
    end;
  end

end;

function TReversePointerListHandler.findprevious(lvl: PMemrectablearraylist; level: ValUInt):PPointerList;
var
  i: ValSInt;
  currentarray: PReversePointerListArray;
begin
  result:=nil;

  if level<maxlevel then
  begin
    currentarray:=lvl[level].arr;
    for i:=lvl[level].entrynr-1 downto 0 do
    begin
      if currentarray[i].ReversePointerlistArray<>nil then
      begin
        lvl[level].entrynr:=i;
        lvl[level+1].arr:=currentarray[i].ReversePointerlistArray;
        result:=findMaxOfPath(lvl, currentarray[i].ReversePointerlistArray,level+1);
        if result<>nil then exit;
      end;
    end;
  end
  else
  begin
    //max level reached
    currentarray:=lvl[level].arr;
    for i:=lvl[level].entrynr-1 downto 0 do
    begin
      if currentarray[i].pointerlist<>nil then
      begin
        lvl[level].entrynr:=i;
        result:=currentarray[i].pointerlist;
        exit;
      end;
    end;
  end;

  //still here, so try a higher level
  if level>0 then
  begin
    lvl[level].entrynr:=$f;
    result:=findprevious(lvl,level-1);
  end;
end;


function TReversePointerListHandler.findClosestPointer(addresslist: PReversePointerListArray; entrynr: integer; level: integer; maxvalue: ptrUint): PPointerList;
{
The pointer was not found exactly, but we are in an addresslist that has been allocated, so something is filled in at least
}
var i: integer;
begin
  //first try the top
  result:=nil;

  for i:=entrynr+1 to $F do
  begin
    if addresslist[i].ReversePointerlistArray<>nil then
    begin
      if level=maxlevel then
      begin
        result:=addresslist[i].pointerlist;
        while (result<>nil) and (result.pointervalue>maxvalue) do //should only run one time
          result:=result.previous;

        if result=nil then
          result:=firstPointerValue;

        exit;
      end
      else //dig deeper
      begin
        result:=findClosestPointer(addresslist[i].ReversePointerlistArray, -1, level+1, maxvalue); //so it will be found by the next top scan
        if result<>nil then exit;
      end;
    end;
  end;


  //nothing at the top, try the bottom
  for i:=entrynr-1 downto 0 do
  begin
    if addresslist[i].ReversePointerlistArray<>nil then
    begin
      if level=maxlevel then
      begin
        result:=addresslist[i].pointerlist;
        while (result<>nil) and (result.pointervalue>maxvalue) do //should never happen
          result:=result.previous;

        if result=nil then
          result:=firstPointerValue;

        exit;
      end
      else //dig deeper
      begin
        result:=findClosestPointer(addresslist[i].ReversePointerlistArray, $10, level+1, maxvalue); //F downto 0
        if result<>nil then exit;
      end;
    end;
  end;
end;

function TReversePointerListHandler.findPointerValue(startvalue: ptrUint; var stopvalue: ptrUint): PPointerList;
var
  _maxlevel: ValSInt;
  level: ValSInt;
  currentarray: PReversePointerListArray;
  entrynr: ValSInt;

  _stopvalue: ptrUint;
begin
  result:=nil;

  //find a node that falls in the region of stopvalue and startvalue
  _maxlevel:=maxlevel;
  _stopvalue:=stopvalue;
  currentarray:=level0list;


  for level:=0 to _maxlevel do
  begin
    entrynr:=_stopvalue shr ((maxlevel-level)*4) and $f;
    if currentarray[entrynr].ReversePointerlistArray=nil then //not found
    begin
      result:=findClosestPointer(currentarray, entrynr, level, _stopvalue);
      break;
    end
    else
    begin
      if level=_maxlevel then
      begin
        result:=currentarray[entrynr].pointerlist;
        break;
      end;
    end;
    currentarray:=currentarray[entrynr].ReversePointerlistArray;
  end;

  stopvalue:=result.pointervalue;

  //clean up bad results
  if (result.pointerValue<startvalue) then
    result:=nil
end;

procedure TReversePointerListHandler.DeletePath(addresslist: PReversePointerListArray; level: integer);
//var
//  i,j: integer;
begin
  //obsolete, freeing the bigmem alloc handler frees this now

  {
  if level=maxlevel then
  begin
    for i:=0 to $F do
    begin
      if addresslist[i].pointerlist<>nil then
      begin
        for j:=0 to addresslist[i].pointerlist.pos-1 do
          freemem(addresslist[i].pointerlist.list[j].staticdata);

        freemem(addresslist[i].pointerlist.list);

        freemem(addresslist[i].pointerlist);
        addresslist[i].pointerlist:=nil;
      end;
    end;
  end
  else
  begin
    for i:=0 to $F do
    begin
      if addresslist[i].ReversePointerlistArray<>nil then
      begin
        deletepath(addresslist[i].ReversePointerlistArray,level+1);
        freemem(addresslist[i].ReversePointerlistArray);
        addresslist[i].ReversePointerlistArray:=nil;
      end;
    end;
  end;

  }
end;

destructor TReversePointerListHandler.destroy;
begin
  setlength(memoryregion,0);
  deletepath(level0list,0);

  if bigalloc<>nil then
    bigalloc.free;

  if ScannablePages<>nil then
    freeandnil(ScannablePages);

  inherited destroy;
end;


procedure TReversePointerListHandler.fillList(addresslist: PReversePointerListArray; level: integer; var prev: PPointerList);
var i: integer;
begin
  if level=maxlevel then
  begin
    for i:=0 to $f do
    begin
      if addresslist[i].pointerlist<>nil then
      begin
        if prev<>nil then
          prev.next:=addresslist[i].pointerlist
        else
          firstPointerValue:=addresslist[i].pointerlist;

        addresslist[i].pointerlist.previous:=prev;
        prev:=addresslist[i].pointerlist;

      end;
    end;
  end
  else
  begin
    for i:=0 to $F do
    begin
      if addresslist[i].ReversePointerlistArray<>nil then
        fillList(addresslist[i].ReversePointerlistArray,level+1, prev);
    end;
  end;
end;

procedure TReversePointerListHandler.fillLinkedList;
var current: PPointerList;
begin
  current:=nil;
  fillList(level0list,0,current);

  lastPointerValue:=current;

  if lastPointerValue=nil then
    raise exception.create(rsPointerValueSetupError);
end;

procedure TReversePointerListHandler.exportToStream(s: TStream; pb: TProgressbar=nil);
var i,c: integer;

  pv: PPointerList;
  lastupdate: qword;
begin
  s.WriteByte($ce);
  s.WriteByte(ScanDataVersion);


  saveModuleListToResults(s); //save the module list (not important for worker threads/systems, but used for saving the main .ptr file)

  if specificBaseAsStaticOnly then
  begin
    s.WriteByte(1);
    s.WriteQWord(basestart);
    s.WriteQWord(basestop);
  end
  else
  begin
    s.WriteByte(0);
  end;

  s.WriteDWord(maxlevel);
  s.WriteQWord(count);

  lastupdate:=GetTickCount64;
  if pb<>nil then
    pb.position:=0;

  c:=0;

  pv:=firstPointerValue;
  while (pv<>nil) do
  begin
    if (pv^.list<>nil) then
    begin
      s.writeQword(pv^.PointerValue);
      s.WriteDWord(pv^.pos);

      for i:=0 to pv^.pos-1 do
      begin
        s.writeQword(pv^.list[i].address);

        if pv^.list[i].staticdata=nil then
          s.WriteByte(0)
        else
        begin
          s.WriteByte(1);
          s.WriteDWord(pv^.list[i].staticdata.moduleindex);
          s.WriteDWord(pv^.list[i].staticdata.offset);
        end;

        c:=c+1;
      end;


    end;

    pv:=pv^.next;

    if (pb<>nil) and (gettickcount64>lastupdate+1000) then
    begin
      pb.position:=ceil((c/count)*100);
      lastupdate:=GetTickCount64;
    end;

  end;

  if pb<>nil then
    pb.position:=100;


end;

procedure TReversePointerListHandler.LoadModuleList(s: TStream);
var
  i: integer;
  x: integer;
  mname: pchar;
  mbase: qword;
  mlistlength: integer;
begin
  modulelist:=TStringList.create;
  mlistlength:=s.ReadDWord;
  for i:=0 to mlistlength-1 do
  begin
    x:=s.ReadDWord;
    getmem(mname, x+1);
    s.ReadBuffer(mname^, x);
    mname[x]:=#0;
    mbase:=s.ReadQWord;

    modulelist.AddObject(mname, tobject(mbase));
    freememandnil(mname);
  end;
end;

function TReversePointerListHandler.LoadHeader(s: TStream) : qword;
begin
  //check the header
  if s.ReadByte<>$ce then
    raise exception.create(rsPVInvalidScandataFile);

  if s.Readbyte<>ScanDataVersion then
    raise exception.create(rsPVInvalidScandataVersion);


  //first read the modulelist. Not used for the scan itself, but needed when saving as the base maintainer


  LoadModuleList(s);

  specificBaseAsStaticOnly:=s.ReadByte=1;
  if specificBaseAsStaticOnly then
  begin
    basestart:=s.ReadQWord;
    basestop:=s.ReadQWord;
  end;


  maxlevel:=s.ReadDWord;
  result:=s.ReadQWord;
end;

function TReversePointerListHandler.is64bit: boolean;
begin
  result:=maxlevel=15;
end;

constructor TReversePointerListHandler.createFromStreamHeaderOnly(s: TStream);
begin
  //only loads the header part
  LoadHeader(s);
end;

constructor TReversePointerListHandler.createFromStream(s: Tstream; progressbar: tprogressbar=nil);
var
  i: integer;
  numberofpointers: integer;
  plist: PPointerList;
  address: ptruint;

 // mlistlength: integer;

  //x: integer;
  //mname: pchar;
  lastcountupdate: qword;

  //mbase: qword;
  pvalue: qword;
  totalcount: qword;
begin
  OutputDebugString('TReversePointerListHandler.createFromStream');

  totalcount:=LoadHeader(s);

  bigalloc:=TBigMemoryAllocHandler.create;

  if progressbar<>nil then
  begin
    progressbar.Min:=0;
    progressbar.Position:=0;
    progressbar.max:=100;
  end;

  level0list:=bigalloc.alloc(sizeof(TReversePointerListArray));
  ZeroMemory(level0list, sizeof(TReversePointerListArray));

  count:=0;
  lastcountupdate:=0;

  while (count<totalcount) do
  begin
    pvalue:=s.ReadQWord;
    plist:=findoraddpointervalue(pvalue);



    if plist<>nil then //should always be the case
    begin
      numberofpointers:=s.ReadDWord;

      plist.pos:=numberofpointers;
      plist.list:=bigalloc.alloc(numberofpointers*sizeof(TPointerDataArray));

      for i:=0 to numberofpointers-1 do
      begin
        plist.list[i].address:=s.ReadQWord;
        if s.ReadByte=1 then //has staticdata
        begin
          plist.list[i].staticdata:=bigalloc.alloc(sizeof(TStaticData));
          plist.list[i].staticdata.moduleindex:=s.ReadDWord;
          plist.list[i].staticdata.offset:=s.readDword;
        end
        else
          plist.list[i].staticdata:=nil;
      end;
      inc(count, numberofpointers);

      if (progressbar<>nil) and ((count-lastcountupdate)>1000) then
      begin
        progressbar.position:=trunc(count/totalcount*100);
        lastcountupdate:=count;
      end;
    end;
  end;


  //finally when done, fill in the linked list
  fillLinkedList;

end;

constructor TReversePointerListHandler.create(start, stop: ptrUint; alligned: boolean; progressbar: tprogressbar; noreadonly: boolean; mustbeclasspointers, allowNonModulePointers: boolean; useStacks: boolean; stacksAsStaticOnly: boolean; threadstacks: integer; stacksize: integer; specificBaseAsStaticOnly: boolean; baseStart: ptruint; baseStop: ptruint; includeSystemModules: boolean=false; regionfilename: string='');
var bytepointer: PByte;
    dwordpointer: PDword absolute bytepointer;
    qwordpointer: PQword absolute bytepointer;

    tempqword: qword;
    tempdword: dword;


    mbi : _MEMORY_BASIC_INFORMATION;
    address: ptrUint;
    pfn: ptruint;
    size:       dword;

    i: Integer;
    j: Integer;

    actualread: ptruint;
    TotalToRead: qword;

    maxsize: dword;

    buffer: pointer;

 //   memoryregion2: array of TMemoryRegion2;
    lastaddress: ptrUint;

    inmodule: boolean;
    valid: boolean;
    InModulePointerMap: TMap;

    regionfile: TFilestream;
    prangelist: TPRangeDynArray;

begin
  OutputDebugString('TReversePointerListHandler.create');
  try
    bigalloc:=TBigMemoryAllocHandler.create;


    modulelist:=tstringlist.create;
    symhandler.getModuleList(modulelist);

    self.useStacks:=useStacks;
    self.threadStacks:=threadStacks;
    self.stacksAsStaticOnly:=stacksAsStaticOnly;
    self.stacksize:=stacksize;

    self.specificBaseAsStaticOnly:=specificBaseAsStaticOnly;
    self.baseStart:=baseStart;
    self.baseStop:=baseStop;

    //fill the stacklist
    if useStacks then
    begin
      setlength(stacklist, threadstacks);
      for i:=0 to threadstacks-1 do
      begin
        stacklist[i]:=GetStackStart(i);
        if stacklist[i]=0 then
        begin
          self.threadstacks:=i;
          break;
        end;
        modulelist.AddObject('THREADSTACK'+inttostr(i), pointer(stacklist[i]));
      end;


    end;

    if processhandler.is64Bit then
    begin
      maxlevel:=15;
     // pointermask:=7; //AND the value/address with this value. If the result=0 it's aligned
    end
    else
    begin
      maxlevel:=7;
     // pointermask:=3;
    end;

    count:=0;



    size:=sizeof(TReversePointerListArray);

    level0list:=bigalloc.alloc(size);
    ZeroMemory(level0list, size);

    OutputDebugString('Querying memoryregions');

    if regionfilename<>'' then
    begin
      //load this file and map every single page
      regionfile:=tfilestream.create(regionfilename, fmOpenRead);
      setlength(prangelist, regionfile.Size div sizeof(TPRange));
      regionfile.ReadBuffer(prangelist[0], regionfile.Size);
      regionfile.free;

      //go through the list and add every page to a map
      valid:=true;
      ScannablePages:=TMap.Create(ituPtrSize,0);

      for i:=0 to length(prangelist)-1 do
      begin
        address:=prangelist[i].startAddress;

        while address<prangelist[i].endaddress do
        begin
          pfn:=address shr 12;
          scannablepages.Add(pfn, valid);
          inc(address, 4096);
        end;
      end;
    end;

    address:=start;


    while (Virtualqueryex(processhandle,pointer(address),mbi,sizeof(mbi))<>0) and (address<stop) and ((address+mbi.RegionSize)>address) do
    begin
      if (includeSystemModules or (not symhandler.inSystemModule(ptrUint(mbi.baseAddress))) ) and (not (not scan_mem_private and (mbi._type=mem_private))) and (not (not scan_mem_image and (mbi._type=mem_image))) and (not (not scan_mem_mapped and ((mbi._type and mem_mapped)>0))) and (mbi.State=mem_commit) and ((mbi.Protect and page_guard)=0) and ((mbi.protect and page_noaccess)=0) then  //look if it is commited
      begin
        if (Skip_PAGE_NOCACHE and ((mbi.AllocationProtect and PAGE_NOCACHE)=PAGE_NOCACHE)) or
           (Skip_PAGE_WRITECOMBINE and ((mbi.AllocationProtect and PAGE_WRITECOMBINE)=PAGE_WRITECOMBINE)) or
           (noreadonly and (mbi.protect in [PAGE_READONLY, PAGE_EXECUTE, PAGE_EXECUTE_READ]))  then
          valid:=false
        else
          valid:=true;

        setlength(memoryregion,length(memoryregion)+1);

        memoryregion[length(memoryregion)-1].BaseAddress:=ptrUint(mbi.baseaddress);  //just remember this location
        memoryregion[length(memoryregion)-1].MemorySize:=mbi.RegionSize;
        memoryregion[length(memoryregion)-1].InModule:=symhandler.inModule(ptrUint(mbi.baseaddress));

        memoryregion[length(memoryregion)-1].ValidPointerRange:=valid;

       // outputdebugstring(inttohex(ptrUint(mbi.baseaddress),8));
      end;


      address:=ptrUint(mbi.baseaddress)+mbi.RegionSize;
    end;


    if length(memoryregion)=0 then
    begin
      OutputDebugString('No memory found in the specified region');
      raise exception.create(rsNoMemoryFoundInTheSpecifiedRegion);
    end;

    //lets search really at the start of the location the user specified
    if (memoryregion[0].BaseAddress<start) and (memoryregion[0].MemorySize-(start-memoryregion[0].BaseAddress)>0) then
    begin
      memoryregion[0].MemorySize:=memoryregion[0].MemorySize-(start-memoryregion[0].BaseAddress);
      memoryregion[0].BaseAddress:=start;
    end;

    //also the right end
    if (memoryregion[length(memoryregion)-1].BaseAddress+memoryregion[length(memoryregion)-1].MemorySize)>stop then
      dec(memoryregion[length(memoryregion)-1].MemorySize,(memoryregion[length(memoryregion)-1].BaseAddress+memoryregion[length(memoryregion)-1].MemorySize)-stop-1);

    //if everything went ok memoryregions should now contain all the addresses and sizes
    //to speed it up combine the regions that are attached to eachother.


    j:=0;
    address:=memoryregion[0].BaseAddress;
    size:=memoryregion[0].MemorySize;
    InModule:=memoryregion[0].InModule;
    valid:=memoryregion[0].ValidPointerRange;

    for i:=1 to length(memoryregion)-1 do
    begin                                                            //only concatenate if classpointers is false, or the same type of executable field is used
      if (memoryregion[i].BaseAddress=address+size) and (memoryregion[i].ValidPointerRange=valid) and ((mustbeclasspointers=false) or (memoryregion[i].InModule=InModule)) then
        inc(size,memoryregion[i].MemorySize)
      else
      begin
        memoryregion[j].BaseAddress:=address;
        memoryregion[j].MemorySize:=size;
        memoryregion[j].InModule:=InModule;
        memoryregion[j].ValidPointerRange:=valid;

        address:=memoryregion[i].BaseAddress;
        size:=memoryregion[i].MemorySize;
        InModule:=memoryregion[i].InModule;
        valid:=memoryregion[i].ValidPointerRange;
        inc(j);
      end;
    end;

    memoryregion[j].BaseAddress:=address;
    memoryregion[j].MemorySize:=size;
    memoryregion[j].InModule:=InModule;
    memoryregion[j].ValidPointerRange:=valid;
    setlength(memoryregion,j+1);



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
        memoryregion[length(memoryregion)-1].InModule:=memoryregion[i].InModule;
        memoryregion[length(memoryregion)-1].ValidPointerRange:=memoryregion[i].ValidPointerRange;
        memoryregion[i].MemorySize:=512*1024; //set the current region to be 512KB

      end;
      inc(i); //next item. Eventually the new items will be handled, and they will also be split untill the list is finally cut into small enough chunks
    end;

    //sort memoryregions from small to high
    quicksortmemoryregions(0,length(memoryregion)-1);

    TotalToRead:=0;
    For i:=0 to length(memoryregion)-1 do
      inc(TotalToRead,Memoryregion[i].MemorySize);

    progressbar.Min:=0;
    progressbar.Step:=1;
    progressbar.Position:=0;
    progressbar.max:=length(memoryregion)*2+1;


    maxsize:=0;
    for i:=0 to length(memoryregion)-1 do
      maxsize:=max(maxsize, memoryregion[i].MemorySize);

    outputdebugstring(Format(rsAllocatingBytesToBuffer, [inttostr(maxsize)]));

    buffer:=nil;
    try
      getmem(buffer,maxsize);
    except
      outputdebugstring('Not enough memory free to scan');
      raise exception.Create(rsPVNotEnoughMemoryFreeToScan);
    end;

    if mustbeclasspointers then
    begin

      if processhandler.is64bit then
        InModulePointerMap:=TMap.Create(itu8,sizeof(valid))
      else
        InModulePointerMap:=TMap.Create(itu4,sizeof(valid));
    end
    else
      InModulePointerMap:=nil;


    try

      //initial scan to fetch the counts of memory

      outputdebugstring('Initial scan to determine the memory needed');


      for i:=0 to length(memoryregion)-1 do
      begin
        actualread:=0;
        if Memoryregion[i].ValidPointerRange and readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),buffer,Memoryregion[i].MemorySize,actualread) then
        begin
          bytepointer:=buffer;



          lastaddress:=ptrUint(buffer)+Memoryregion[i].MemorySize-processhandler.pointersize;

          if processhandler.is64Bit then
          begin
            while ptrUint(bytepointer)<=lastaddress do
            begin


              if (alligned and ((qwordpointer^ mod 4)=0) and ispointer(qwordpointer^)) or
                 ((not alligned) and ispointer(qwordpointer^) ) then
              begin
                valid:=true;

                //initial add
                if mustbeclasspointers then
                begin
                  //check if we havn't previously marked this address as invalid
                  if InModulePointerMap.GetData(qwordpointer^, valid)=false then //not in list yet
                  begin
                    //check that the memory it points to contains a pointer to executable code
                    if ReadProcessMemory(processhandle, pointer(qwordpointer^), @tempqword, 8, actualread) then
                      valid:=(allowNonModulePointers and (ReadProcessMemory(processhandle, pointer(tempqword), @tempqword, 8, actualread))) or isModulePointer(tempqword)
                    else
                      valid:=false;

                    InModulePointerMap.Add(qwordpointer^, valid);
                  end;
                end;

                if valid and (scannablepages<>nil) then
                begin
                  pfn:=(Memoryregion[i].BaseAddress+(ptrUint(qwordpointer)-ptrUint(buffer))) shr 12;
                  valid:=scannablepages.HasId(pfn);
                end;

                if valid then
                  addpointer(qwordpointer^, Memoryregion[i].BaseAddress+(ptrUint(qwordpointer)-ptrUint(buffer)),false);
              end;

              if alligned then
                inc(dwordpointer) //increases qwordpointer
              else
                inc(bytepointer);
            end;
          end
          else
          begin
            while ptrUint(bytepointer)<=lastaddress do
            begin


              if (alligned and ((dwordpointer^ mod 4)=0) and ispointer(dwordpointer^)) or
                 ((not alligned) and ispointer(dwordpointer^) ) then
              begin
                //initial add
                valid:=true;

                if mustbeclasspointers then
                begin
                  //check if we havn't previously marked this address as invalid
                  if InModulePointerMap.GetData(dwordpointer^, valid)=false then //not in list yet
                  begin
                    //check that the memory it points to contains a pointer to executable code
                    if ReadProcessMemory(processhandle, pointer(ptruint(dwordpointer^)), @tempdword, 4, actualread) then
                      valid:=(allowNonModulePointers and (ReadProcessMemory(processhandle, pointer(ptruint(tempdword)), @tempdword,4, actualread))) or isModulePointer(tempdword)
                    else
                      valid:=false;

                    InModulePointerMap.Add(dwordpointer^, valid);
                  end;
                end;

                if valid and (scannablepages<>nil) then
                begin
                  pfn:=(Memoryregion[i].BaseAddress+(ptrUint(dwordpointer)-ptrUint(buffer))) shr 12;
                  valid:=scannablepages.HasId(pfn);
                end;

                if valid then
                  addpointer(dwordpointer^, Memoryregion[i].BaseAddress+(ptrUint(dwordpointer)-ptrUint(buffer)),false);
              end;

              if alligned then
                inc(dwordpointer)
              else
                inc(bytepointer);
            end;
          end;


        end;

        progressbar.StepIt;
      end;

      //actual add
      OutputDebugString('Secondary scan actually allocating the memory and filling in the data');

      for i:=0 to length(memoryregion)-1 do
      begin
        actualread:=0;
        if readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),buffer,Memoryregion[i].MemorySize,actualread) then
        begin
          bytepointer:=buffer;
          lastaddress:=ptrUint(buffer)+Memoryregion[i].MemorySize;

          if processhandler.is64Bit then
          begin
            while ptrUint(bytepointer)<lastaddress do
            begin


              if (alligned and ((qwordpointer^ mod 4)=0) and ispointer(qwordpointer^)) or
                 ((not alligned) and ispointer(qwordpointer^) ) then
              begin
                //initial add
                valid:=true;

                if mustbeclasspointers then
                begin

                  //check if we havn't previously marked this address as invalid
                  if InModulePointerMap.GetData(qwordpointer^, valid)=false then //not in list yet
                  begin
                    //check that the memory it points to contains a pointer to executable code
                    if ReadProcessMemory(processhandle, pointer(qwordpointer^), @tempqword, 8, actualread) then
                      valid:=(allowNonModulePointers and (ReadProcessMemory(processhandle, pointer(tempqword), @tempqword, 8, actualread))) or isModulePointer(tempqword)
                    else
                      valid:=false;

                    InModulePointerMap.Add(qwordpointer^, valid);
                  end;
                end;

                if valid and (scannablepages<>nil) then
                begin
                  pfn:=(Memoryregion[i].BaseAddress+(ptrUint(qwordpointer)-ptrUint(buffer))) shr 12;
                  valid:=scannablepages.HasId(pfn);
                end;

                if valid then
                begin
                  addpointer(qwordpointer^, Memoryregion[i].BaseAddress+(ptrUint(qwordpointer)-ptrUint(buffer)),true);
                  inc(count);
                end;
              end;

              if alligned then
                inc(dwordpointer) //increase with 4
              else
                inc(bytepointer);
            end;
          end
          else
          begin
            while ptrUint(bytepointer)<lastaddress do
            begin


              if (alligned and ((dwordpointer^ mod 4)=0) and ispointer(dwordpointer^)) or
                 ((not alligned) and ispointer(dwordpointer^) ) then
              begin
                //initial add
                valid:=true;

                if mustbeclasspointers then
                begin
                  //check if we havn't previously marked this address as invalid
                  if InModulePointerMap.GetData(dwordpointer^, valid)=false then //not in list yet
                  begin
                    //check that the memory it points to contains a pointer to executable code
                    if ReadProcessMemory(processhandle, pointer(ptruint(dwordpointer^)), @tempdword, 4, actualread) then
                      valid:=(allowNonModulePointers and (ReadProcessMemory(processhandle, pointer(ptruint(tempdword)), @tempdword, 4, actualread))) or isModulePointer(tempdword)
                    else
                      valid:=false;

                    InModulePointerMap.Add(dwordpointer^, valid);
                  end;
                end;

                if valid and (scannablepages<>nil) then
                begin
                  pfn:=(Memoryregion[i].BaseAddress+(ptrUint(dwordpointer)-ptrUint(buffer))) shr 12;
                  valid:=scannablepages.HasId(pfn);
                end;

                if valid then
                begin
                  addpointer(dwordpointer^, Memoryregion[i].BaseAddress+(ptrUint(dwordpointer)-ptrUint(buffer)),true);
                  inc(count);
                end;
              end;

              if alligned then
                inc(dwordpointer) //increase with 4
              else
                inc(bytepointer);
            end;

          end;
        end;

        progressbar.StepIt;
      end;

      //and fill in the linked list
      OutputDebugString('filling linked list');
      fillLinkedList;

      progressbar.Position:=0;

    finally
      //OutputDebugString('Freeing the buffer');
      if buffer<>nil then
        freememandnil(buffer);

      if InModulePointerMap<>nil then
      begin
        InModulePointerMap.Clear;
        freeandnil(InModulePointerMap);
      end;

    end;

    //OutputDebugString('TReversePointerListHandler.create: Finished without an exception');

  except
    on e: exception do
    begin
      outputdebugstring('TReversePointerListHandler.create: Exception:'+e.message+'  (count='+inttostr(count)+')');
      raise Exception.Create(e.message);
    end;
  end;
end;

//Lua support/testing

function lua_createReversePointerListHandlerFromFile(L: PLua_State): integer; cdecl;
var
  filename: string='';
  progressbar: tprogressbar=nil;
  fs: tfilestream=nil;
  ds: Tdecompressionstream=nil;
  rplh: TReversePointerListHandler;
begin
  if lua_gettop(L)<0 then exit(0);
  filename:=lua_tostring(L,1);

  try
    try
      fs:=tfilestream.Create(filename, fmOpenRead);
      ds:=Tdecompressionstream.create(fs);

      if lua_gettop(L)>=2 then
        progressbar:=tprogressbar(lua_touserdata(L,2));

      rplh:=TReversePointerListHandler.createFromStream(ds,progressbar);
      luaclass_newClass(L,rplh);
      result:=1;
    finally
      if ds<>nil then
        freeandnil(ds);

      if fs<>nil then
        freeandnil(fs);
    end;
  except
    on e:exception do
    begin
      lua_pushnil(L);
      lua_pushstring(L,e.message);
      exit(2);
    end;
  end;
end;

function ReversePointerListHandler_findPointerValue(L: PLua_State): integer; cdecl;
var
  startvalue, stopvalue: ptruint;
  list: TReversePointerListHandler;
  pl: PPointerList;
  pli: integer;
  i: integer;
begin
  list:=luaclass_getClassObject(L);

  startvalue:=lua_tointeger(L,1);
  stopvalue:=lua_tointeger(L,2);

  pl:=list.findPointerValue(startvalue, stopvalue);
  if pl<>nil then
  begin
    lua_newtable(L);
    pli:=lua_gettop(L);

    for i:=0 to pl^.pos-1 do
    begin
      lua_pushinteger(L,i+1);
      lua_pushinteger(L,pl^.list[i].address);
      lua_settable(L,pli);
    end;

    pl:=pl^.previous;
    if pl<>nil then
    begin
      stopvalue:=pl^.pointervalue;
      lua_pushinteger(L,stopvalue);
    end
    else
      lua_pushnil(L);

    result:=2;
  end
  else
    exit(0);
end;

procedure ReversePointerListHandler_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'findPointerValue', ReversePointerListHandler_findPointerValue);
end;

procedure initializeLuaPointerValueList;
begin
  Lua_register(LuaVM, 'createReversePointerListHandlerFromFile', lua_createReversePointerListHandlerFromFile);
end;

initialization

  luaclass_register(TReversePointerListHandler, ReversePointerListHandler_addMetaData);

end.

