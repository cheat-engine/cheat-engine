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
type TValueType= (vt_byte,vt_word, vt_dword, vt_single, vt_double, vt_int64, vt_all);


type TSavedScanHandler = class
  private
    SavedScanmemoryfile: TFileStream;
    SavedScanaddress: tmemorystream;
    SavedScantype: tSavedScantype;

    SavedScanmemory: pointer;
    LoadedFromList: array of boolean;
    LoadedFromListMREW: TMultiReadExclusiveWriteSynchronizer;
    maxnumberofregions: integer;
    scandir: string;

    procedure cleanup;
    function loadIfNotLoadedRegion(p: pointer): pointer;
  public
    function getSavedScanbyte(address: ptruint): byte;
    function getSavedScanword(address: ptruint): word;
    function getSavedScandword(address: ptruint): dword;
    function getSavedScansingle(address: ptruint): single;
    function getSavedScandouble(address: ptruint): double;
    function getSavedScanint64(address: ptruint): int64;
    function getpointertoaddress(address:ptruint;valuetype:tvaluetype): pointer;

    constructor create(scandir: string; savedresultsname: string);
    destructor destroy; override;
end;


implementation

uses cefuncproc, Math;

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
  end;
end;

function TSavedScanHandler.getpointertoaddress(address:ptruint;valuetype:tvaluetype): pointer;
var j: integer;
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

  //5.4: change routine to only read in pages of 4KB if it wasn't paged in yet (does require a page table like list of course)

  p:=pointer(ptrUint(SavedScanaddress.Memory)+7);
  

  if SavedScantype=fs_advanced then
  begin
    {the addressfile exists out of a list of memoryregions started with the text
     REGION or NORMAL, so skip the first 7 bytes
    }
    pm:=pointer(ptrUint(SavedScanaddress.Memory)+7);

    //find the region this address belongs to
    //the region list should be sorted


    first:=0;
    last:=maxnumberofregions-1;

    while (First <= Last) do
    begin

      //Gets the middle of the selected range
      Pivot := (First + Last) div 2;
      //Compares the String in the middle with the searched one
      if InRange(address, pm[pivot].BaseAddress, pm[pivot].BaseAddress + pm[pivot].MemorySize) then
      begin
        //found it
        result:=loadifnotloadedRegion(pointer(ptrUint(pm[pivot].startaddress)+(address-pm[pivot].baseaddress)));
        exit;
      end
      //If the Item in the middle has a bigger value than
      //the searched item, then select the first half
      else if pm[pivot].baseaddress > address then
        Last := Pivot - 1
          //else select the second half
      else
        First := Pivot + 1;
    end;
  end
  else
  begin
    pa:=pointer(p);
    pab:=pointer(p);
    p1:=SavedScanmemory;
   { p2:=SavedScanmemory;
    p3:=SavedScanmemory;
    p4:=SavedScanmemory;
    p5:=SavedScanmemory;
    p6:=SavedScanmemory; }

    if (valuetype <> vt_all) then
    begin
      //addresslist is a list of dword

      j:=(SavedScanaddress.Size-7) div sizeof(ptruint); //max number of addresses , no same as first for binary

      //the list is sorted so do a quickscan
      first:=0;
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
            vt_byte : result:=loadifnotloadedregion(@p1[pivot]);
            vt_word : result:=loadifnotloadedregion(@p2[pivot]);
            vt_dword : result:=loadifnotloadedregion(@p3[pivot]);
            vt_single: result:=loadifnotloadedregion(@p4[pivot]);
            vt_double: result:=loadifnotloadedregion(@p5[pivot]);
            vt_int64: result:=loadifnotloadedregion(@p6[pivot]);
          end;
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
      raise exception.create('Failure in finding '+inttohex(address,8)+' in the first scan results');
    end
    else
    begin
      //addresslist is a list of 2 dwords, address and vartype, what kind of vartype is not important

      j:=(SavedScanaddress.Size-7) div sizeof(tbitaddress); //max number of addresses , no same as first for binary

      //the list is sorted so do a quickscan
      first:=0;
      last:=j-1;

      while (First <= Last) do
      begin

        //Gets the middle of the selected range
        Pivot := (First + Last) div 2;
        //Compares the String in the middle with the searched one
        if address=pab[pivot].address then
        begin
          //found it
          result:=loadifnotloadedregion(@p6[pivot]); //8 byte entries, doesnt have to match the same type, since it is the same 8 byte value that's stored
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

  raise exception.create('Failure in finding '+inttohex(address,8)+' in the previous scan results');
end;

function TSavedScanHandler.getSavedScanbyte(address: ptruint): byte;
begin
  result:=pbyte(getpointertoaddress(address,vt_byte))^; //tries to read nil is not found, which should never happen, so I should get a bug report if it does
end;

function TSavedScanHandler.getSavedScanword(address: ptruint): word;
begin
  result:=pword(getpointertoaddress(address,vt_word))^; //tries to read nil is not found, which should never happen, so I should get a bug report if it does
end;

function TSavedScanHandler.getSavedScandword(address: ptruint): dword;
begin
  result:=pdword(getpointertoaddress(address,vt_dword))^; //tries to read nil is not found, which should never happen, so I should get a bug report if it does
end;

function TSavedScanHandler.getSavedScansingle(address: ptruint): single;
begin
  result:=psingle(getpointertoaddress(address,vt_single))^; //tries to read nil is not found, which should never happen, so I should get a bug report if it does
end;

function TSavedScanHandler.getSavedScandouble(address: ptruint): double;
begin
  result:=pdouble(getpointertoaddress(address,vt_double))^; //tries to read nil is not found, which should never happen, so I should get a bug report if it does
end;

function TSavedScanHandler.getSavedScanint64(address: ptruint): int64;
begin
  result:=pint64(getpointertoaddress(address,vt_int64))^; //tries to read nil is not found, which should never happen, so I should get a bug report if it does
end;


constructor TSavedScanHandler.create(scandir: string; savedresultsname: string);
var datatype: string[6];
    pm: ^TArrMemoryRegion;
    i: integer;
    p: ptrUint;
begin
  self.scandir:=scandir;
  try
    try

      SavedScanmemoryfile:=Tfilestream.Create(scandir+'MEMORY.'+savedresultsname,fmOpenRead or fmsharedenynone);
      SavedScanmemory:=virtualalloc(nil, SavedScanmemoryfile.Size+$2000, mem_commit, page_readwrite);

      //make an array to store the previous memory in blocks of 4KB
      setlength(loadedfromlist, 2+(SavedScanmemoryfile.Size shr 12)); //+2 to keep some extra room, so less checking
      zeromemory(@loadedfromlist[0], length(loadedfromlist));

      loadedfromlistMREW:=TMultiReadExclusiveWriteSynchronizer.create;
    except
      raise exception.Create('No first scan data files found');
    end;

    SavedScanaddress:=tmemorystream.Create;
    try
      SavedScanaddress.LoadFromFile(scandir+'ADDRESSES.'+savedresultsname);
    except
      raise exception.Create('No first scan data files found');
    end;

    SavedScanaddress.ReadBuffer(datatype,7);
    if datatype='REGION' then
    begin
      SavedScantype:=fs_advanced;
      maxnumberofregions:=(SavedScanaddress.Size-7) div sizeof(TMemoryRegion); //max number of regions

      //fill in startaddress elements
      pm:=pointer(ptrUint(SavedScanaddress.Memory)+7);
      p:=ptrUint(SavedScanmemory);
      for i:=0 to maxnumberofregions-1 do
      begin
        pm[i].startaddress:=pointer(p);
        inc(p, pm[i].MemorySize);
      end;
      
    end
    else
    begin
      SavedScantype:=fs_addresslist;
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
    virtualfree(SavedScanmemory,0,MEM_RELEASE);
    SavedScanmemory:=nil;
  end;

  freeandnil(loadedfromlistMREW);
  freeandnil(SavedScanaddress);
  freeandnil(SavedScanmemoryfile);
end;

end.

