unit savedscanhandler;

{$MODE Delphi}

{
12 december 2010: Firsthandler should be renamed to "previousscanhandler" as it's being used for saved scans as well now
}

{
First scan handler is a class that will help with scanning the results of the
first scan.
It'll read the results of the first scan and provides an inteface for the
scanroutines for quick lookup of the previous value of a specific address
}


{
function BinSearchEntry(Strings: TStrings; address: dword; var Pivot: integer): integer;
var
  First: Integer;
  Last: Integer;
  Found: Boolean;
begin
  try
    First  := 0; //Sets the first item of the range
    Last   := Strings.Count-1; //Sets the last item of the range
    Found  := False; //Initializes the Found flag (Not found yet)
    Result := -1; //Initializes the Result

    while (First <= Last) and (not Found) do
    begin

      //Gets the middle of the selected range
      Pivot := (First + Last) div 2;
      //Compares the String in the middle with the searched one
      if TMemoryAllocEvent(strings.Objects[Pivot]).BaseAddress = address then
      begin
        Found  := True;
        Result := Pivot;
      end
      //If the Item in the middle has a bigger value than
      //the searched item, then select the first half
      else if TMemoryAllocEvent(strings.Objects[Pivot]).BaseAddress > address then
        Last := Pivot - 1
          //else select the second half
      else
        First := Pivot + 1;
    end;
  except
    outputdebugstring('Exception in BinSearchEntry');
  end;
end;   }


interface

uses windows, LCLIntf,classes,sysutils,syncobjs;

type TSavedScantype= (fs_advanced,fs_addresslist);
type TValueType= (vt_byte,vt_word, vt_dword, vt_single, vt_double, vt_int64, vt_all);     //todo: Make compatible wioth the rest of ce's vartype


type TSavedScanHandler = class
  private
    SavedScanmemoryFS: TFileStream;
    SavedScanaddressFS: TFileStream;
    SavedScantype: tSavedScantype;

    maxaddresslistcount: integer;
    addresslistmemory: pointer;  //can be an array of regions, an array of pointers or an array of tbitaddress definitions
    addresslistoffset: qword; //offset into the savedscanaddressFS file
    SavedScanmemory: pointer;
    SavedScanMemoryOffset: qword; //the offset in the SavedScanmemoryFS file

    maxnumberofregions: integer;
    scandir: string;
    savedresultsname: string;

    SavedScanaddressSizeWithoutHeader: qword;
    SavedScanaddressCountAll, SavedScanaddressCountNormal: qword; //saves on a div each time


    LastAddressAccessed: record
      address: ptruint; //holds the last address that was read
      index: integer; //holds the index in the addresslist array
    end;

    currentRegion: integer;
    procedure cleanup;
    function loadIfNotLoadedRegion(p: pointer): pointer;

    procedure LoadNextChunk(valuetype: TValuetype);
    procedure LoadMemoryForCurrentChunk(valuetype: TValueType);
    procedure loadCurrentRegionMemory;
    procedure InitializeScanHandler;
  public
    AllowRandomAccess: boolean; //set this if you wish to allow random access through the list. (EXTREMELY INEFFICIENT IF IT HAPPENS, addresslist purposes only)
    AllowNotFound: boolean; //set this if you wish to return nil instead of an exception if the address can't be found in the list
    function getpointertoaddress(address:ptruint;valuetype:tvaluetype): pointer;

    constructor create(scandir: string; savedresultsname: string);
    destructor destroy; override;
end;


implementation

uses cefuncproc, Math;

resourcestring
  rsMaxaddresslistcountIs0MeansTheAddresslistIsBad = 'maxaddresslistcount is 0 (Means: the addresslist is bad)';
  rsFailureInFindingInThePreviousScanResults = 'Failure in finding %s in the previous scan results';
  rsInvalidOrderOfCallingGetpointertoaddress = 'Invalid order of calling getpointertoaddress';
  rsFailureInFindingInTheFirstScanResults = 'Failure in finding %s in the first scan results';
  rsNoFirstScanDataFilesFound = 'No first scan data files found';


type TArrMemoryRegion= array [0..0] of TMemoryRegion;

function TSavedScanHandler.loadIfNotLoadedRegion(p: pointer): pointer;
{
Will load in a section from the memory file
p is a pointer in the memory buffer as if it was completly loaded
This will effectivly decrease reads to the file. Of course, there is still
unused memory which is kinda a waste, but it's the most efficient way
}
var index: integer;
    base: pointer;
begin
  result:=p;

  {
  adding a multireadexclusivewrite or not...
  might result in memory being written multiple times to exactly the same value
  but besides that no real problem.
  decision: no need to block other threads. Besides, the way threadjobs are made
  all have a seperate region to scan, so usually shouldn't have much overlap
  }
  (*
  if not LoadedFromList[(ptrUint(p)-ptrUint(SavedScanmemory)) shr 12] then
  begin
    //not loaded yet, load this section
    index:=(ptrUint(p)-ptrUint(SavedScanmemory)) shr 12;

    base:=pointer(ptrUint(SavedScanmemory)+(index shl 12));
    SavedScanmemoryfile.Seek((index shl 12),soFromBeginning);

    //read 8KB (2 entries)
    SavedScanmemoryfile.Read(base^,$2000);

    loadedfromlistMREW.BeginWrite;
    LoadedFromList[index]:=true;
    LoadedFromList[index+1]:=true;
    loadedfromlistMREW.EndWrite;
  end; *)
end;

procedure TSavedScanHandler.loadCurrentRegionMemory;
{Loads the memory region designated by the current region}
var pm: ^TArrMemoryRegion;
begin
  pm:=addresslistmemory;
  SavedScanmemoryfs.position:=ptruint(pm[currentRegion].startaddress);

  savedscanmemoryfs.readbuffer(SavedScanmemory^, pm[currentRegion].memorysize);

end;

procedure TSavedScanHandler.LoadMemoryForCurrentChunk(valuetype: TValueType);
{
Loads the savedscanmemory block for the current adddresslist block
}
var addressliststart: qword;
    index: qword;
    varsize: integer;
begin
  if valuetype<>vt_all then
  begin
    //find the start of this region
    addressliststart:=(savedscanaddressfs.Position)-maxaddresslistcount*sizeof(ptruint);
    index:=(addressliststart-7) div sizeof(ptruint);
  end
  else
  begin
    addressliststart:=(savedscanaddressfs.Position)-maxaddresslistcount*sizeof(TBitAddress);
    index:=(addressliststart-7) div sizeof(TBitAddress);
  end;

  case valuetype of
    vt_byte: varsize:= 1;
    vt_word: varsize:= 2;
    vt_dword, vt_single: varsize:= 4;
    vt_double, vt_int64, vt_all: varsize:= 8;
  end;


  SavedScanmemoryFS.Position:=index * varsize;
  SavedScanmemoryFS.ReadBuffer(SavedScanmemory^, maxaddresslistcount*varsize);
end;

procedure TSavedScanHandler.LoadNextChunk(valuetype: TValuetype);
{
For the addresslist specific type: Loads in the next region based on the addresslist
}
begin
//savedscanaddressfs.ReadBuffer(addresslistmemory, maxaddresslistcount*sizeof(ptruint));

  if valuetype<>vt_all then
  begin

    maxaddresslistcount:=min(maxaddresslistcount, (savedscanaddressfs.size-savedscanaddressfs.Position) div sizeof(ptruint)); //limit to the addresslist file size

    if addresslistmemory=nil then
      getmem(addresslistmemory, maxaddresslistcount*sizeof(ptruint));

    //load the results
    savedscanaddressfs.ReadBuffer(addresslistmemory^, maxaddresslistcount*sizeof(ptruint));
  end
  else
  begin
    maxaddresslistcount:=min(maxaddresslistcount, (savedscanaddressfs.size-savedscanaddressfs.Position) div sizeof(TBitAddress)); //limit to the addresslist file size

    getmem(addresslistmemory, maxaddresslistcount*sizeof(TBitAddress));
    savedscanaddressfs.ReadBuffer(addresslistmemory^, maxaddresslistcount*sizeof(TBitAddress));
  end;
  if maxaddresslistcount=0 then raise exception.create(rsMaxaddresslistcountIs0MeansTheAddresslistIsBad);

  LastAddressAccessed.index:=0; //reset the index
end;

function TSavedScanHandler.getpointertoaddress(address:ptruint;valuetype:tvaluetype): pointer;
var i,j: integer;
    pm: ^TArrMemoryRegion;
    pa: PptruintArray;
    pab: PBitAddressArray;
    p: pbyte;
    p1: PByteArray;
    p2: PWordArray absolute p1;
    p3: PDwordArray absolute p1;
    p4: PSingleArray absolute p1;
    p5: PDoubleArray absolute p1;
    p6: PInt64Array absolute p1;

    first,last: integer;

    pivot: integer;
begin
  result:=nil;
  if AllowRandomAccess then //no optimization if random access is used
  begin
    LastAddressAccessed.address:=0;
    LastAddressAccessed.index:=0;
  end;



  //6.1 Only part of the addresslist and memory results are loaded
  if SavedScantype=fs_advanced then
  begin
    {the addressfile exists out of a list of memoryregions started with the text
     REGION or NORMAL, so skip the first 7 bytes
    }


    pm:=addresslistmemory;


    //if no region is set or the current region does not fall in the current list
    if (currentRegion=-1) or (address>pm[currentregion].baseaddress+pm[currentregion].memorysize) then
    begin
      //find the startregion, becaue it's a sequential read just go through it in order
      inc(currentRegion);
      while (address>pm[currentregion].baseaddress+pm[currentregion].memorysize) and (currentregion<maxnumberofregions) do
        inc(currentRegion);

      if currentregion>=maxnumberofregions then
      begin
        if AllowNotFound = false then
          raise exception.create(Format(rsFailureInFindingInThePreviousScanResults, [inttohex(address, 8)]))
        else
          exit;
      end;

      loadCurrentRegionMemory;
    end;


    result:=pointer(ptruint(savedscanmemory)+(address-pm[currentregion].baseaddress));
    exit;

  end
  else
  begin

    if addresslistmemory=nil then
    begin
      //the memoryblock is only 20*4096=81920 bytes big set the addresslist size to be able to address that
      case valuetype of
        //(vt_byte,vt_word, vt_dword, vt_single, vt_double, vt_int64, vt_all);
        vt_byte: maxaddresslistcount:= 20*4096 div 1;
        vt_word: maxaddresslistcount:= 20*4096 div 2;
        vt_dword, vt_single: maxaddresslistcount:= 20*4096 div 4;
        vt_double, vt_int64, vt_all: maxaddresslistcount:= 20*4096 div 8;
      end;

      loadnextchunk(valuetype); //will load the initial addresslist
      LoadMemoryForCurrentChunk(valuetype);
    end;


    pa:=addresslistmemory;
    pab:=addresslistmemory;
    p1:=SavedScanmemory;

    if (valuetype <> vt_all) then
    begin
      //addresslist is a list of pointers

      if pa[0]>address then  //out of order access
      begin
        if AllowRandomAccess then
        begin
          //random access allowed. Start all over
          InitializeScanHandler;
          result:=getpointertoaddress(address, valuetype);
        end
        else
          raise exception.create(rsInvalidOrderOfCallingGetpointertoaddress);
      end;

      if pa[maxaddresslistcount-1]<address then
      begin
        while pa[maxaddresslistcount-1]<address do //load in the next chunk
          LoadNextChunk(valuetype);

        LoadMemoryForCurrentChunk(valuetype);
      end;

      //we now have an addresslist and memory region and we know that the address we need is in here
      j:=maxaddresslistcount;

      //the list is sorted so do a quickscan



      first:=LastAddressAccessed.index;

      last:=j-1;

      while (First <= Last) do
      begin

        //Gets the middle of the selected range
        Pivot := (First + Last) div 2;
        //Compares the String in the middle with the searched one
        if address=pa[pivot] then
        begin
          //found it
          case valuetype of
            vt_byte : result:=@p1[pivot];
            vt_word : result:=@p2[pivot];
            vt_dword : result:=@p3[pivot];
            vt_single: result:=@p4[pivot];
            vt_double: result:=@p5[pivot];
            vt_int64: result:=@p6[pivot];
          end;
          LastAddressAccessed.address:=address;
          LastAddressAccessed.index:=pivot;
          exit;
        end
        //If the Item in the middle has a bigger value than
        //the searched item, then select the first half
        else if pa[pivot] > address then
          Last := Pivot - 1
            //else select the second half
        else
          First := Pivot + 1;
      end;


      //not found
      if not AllowNotFound then
        raise exception.create(Format(rsFailureInFindingInTheFirstScanResults, [inttohex(address, 8)]));
    end
    else
    begin
      //addresslist is a list of 2 items, address and vartype, what kind of vartype is not important because each type stores it's full max variablecount

      if pab[0].address>address then
      begin
        if AllowRandomAccess then
        begin
          //random access allowed. Start all over
          InitializeScanHandler;
          result:=getpointertoaddress(address, valuetype);
        end
        else
          raise exception.create(rsInvalidOrderOfCallingGetpointertoaddress);
      end;

      if pab[maxaddresslistcount-1].address<address then
      begin
        while pab[maxaddresslistcount-1].address<address do //load in the next chunk
          LoadNextChunk(valuetype);

        LoadMemoryForCurrentChunk(valuetype);
      end;

      //we now have an addresslist and memory region and we know that the address we need is in here
      j:=maxaddresslistcount;


      //the list is sorted so do a quickscan
      first:=LastAddressAccessed.index;
      last:=j-1;

      while (First <= Last) do
      begin

        //Gets the middle of the selected range
        Pivot := (First + Last) div 2;
        //Compares the String in the middle with the searched one
        if address=pab[pivot].address then
        begin
          //found it
          result:=@p6[pivot]; //8 byte entries, doesnt have to match the same type, since it is the same 8 byte value that's stored
          LastAddressAccessed.address:=address;
          LastAddressAccessed.index:=pivot;
          exit;
        end
        //If the Item in the middle has a bigger value than
        //the searched item, then select the first half
        else if pab[pivot].address > address then
          Last := Pivot - 1
            //else select the second half
        else
          First := Pivot + 1;
      end;

    end;

  end;

  if not AllowNotFound then
    raise exception.create(Format(rsFailureInFindingInThePreviousScanResults, [inttohex(address, 8)]));

end;

procedure TSavedScanHandler.InitializeScanHandler;
var datatype: string[6];
    pm: ^TArrMemoryRegion;
    i: integer;
    p: ptrUint;

    maxregionsize: integer;
begin
  cleanup;
  maxregionsize:=20*4096;
  try
    try
      SavedScanaddressFS:=tfilestream.Create(scandir+'ADDRESSES.'+savedresultsname, fmopenread or fmsharedenynone);
    except
      raise exception.Create(rsNoFirstScanDataFilesFound);
    end;
    SavedScanaddressFS.ReadBuffer(datatype,7);

    if datatype='REGION' then
    begin
      SavedScantype:=fs_advanced;
      maxnumberofregions:=(SavedScanaddressFS.Size-7) div sizeof(TMemoryRegion); //max number of regions

      //fill the addresslist with the regions
      getmem(addresslistmemory, (SavedScanaddressFS.Size-7));
      SavedScanaddressFS.ReadBuffer(addresslistmemory^, (SavedScanaddressFS.Size-7));


      //find the max region
      pm:=addresslistmemory;

      p:=0;
      for i:=0 to maxnumberofregions-1 do
      begin
        maxregionsize:=max(maxregionsize, pm[i].memorysize);
        pm[i].startaddress:=pointer(p); //set the offset in the file (if it wasn't set already)
        inc(p, pm[i].MemorySize);
      end;


      currentRegion:=-1;
    end
    else
    begin
      SavedScantype:=fs_addresslist;
      SavedScanaddressSizeWithoutHeader:=(SavedScanaddressFS.Size-7);
      SavedScanaddressCountNormal:=SavedScanaddressSizeWithoutHeader div sizeof(ptruint);
      SavedScanaddressCountAll:=SavedScanaddressSizeWithoutHeader div sizeof(tbitaddress);

      //allocate addresslistmemory on first access
    end;


    try
      SavedScanmemoryFS:=Tfilestream.Create(scandir+'MEMORY.'+savedresultsname,fmOpenRead or fmsharedenynone);
      getmem(SavedScanmemory, maxregionsize);
    except
      raise exception.Create(rsNoFirstScanDataFilesFound);
    end;


  except
    on e: exception do
    begin
      //clean up and raise the exception
      cleanup;
      raise exception.Create(e.Message);
    end;
 end;
end;

constructor TSavedScanHandler.create(scandir: string; savedresultsname: string);
begin
  self.scandir:=scandir;
  self.savedresultsname:=savedresultsname;
  InitializeScanHandler;
end;

destructor TSavedScanHandler.destroy;
begin
  cleanup;
  inherited destroy;
end;

procedure TSavedScanHandler.cleanup;
{Cleanup routine, for use by create when failure and destroy}
begin
  if SavedScanmemory<>nil then
  begin
    freemem(SavedScanmemory);
    SavedScanmemory:=nil;
  end;

  if addresslistmemory<>nil then
  begin
    freemem(addresslistmemory);
    addresslistmemory:=nil;
  end;

  freeandnil(SavedScanaddressFS);
  freeandnil(SavedScanmemoryFS);
end;

end.

