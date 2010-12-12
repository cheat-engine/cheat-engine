unit firstscanhandler;

{$MODE Delphi}

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

type TFirstscantype= (fs_advanced,fs_addresslist);
type TValueType= (vt_byte,vt_word, vt_dword, vt_single, vt_double, vt_int64, vt_all);


type TFirstScanHandler = class
  private
    firstscanmemoryfile: TFileStream;
    firstscanaddress: tmemorystream;
    firstscantype: tfirstscantype;

    firstscanmemory: pointer;
    LoadedFromList: array of boolean;
    LoadedFromListMREW: TMultiReadExclusiveWriteSynchronizer;
    maxnumberofregions: integer;
    scandir: string;

    procedure cleanup;
    function loadIfNotLoadedRegion(p: pointer): pointer;
  public
    function getfirstscanbyte(address: dword): byte;
    function getfirstscanword(address: dword): word;
    function getfirstscandword(address: dword): dword;
    function getfirstscansingle(address: dword): single;
    function getfirstscandouble(address: dword): double;
    function getfirstscanint64(address: dword): int64;
    function getpointertoaddress(address:dword;valuetype:tvaluetype): pointer;

    constructor create(scandir: string);
    destructor destroy; override;
end;


implementation

uses cefuncproc, Math;

type TArrMemoryRegion= array [0..0] of TMemoryRegion;


function TFirstScanHandler.loadIfNotLoadedRegion(p: pointer): pointer;
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
  if not LoadedFromList[(ptrUint(p)-ptrUint(firstscanmemory)) shr 12] then
  begin
    //not loaded yet, load this section
    index:=(ptrUint(p)-ptrUint(firstscanmemory)) shr 12;

    base:=pointer(ptrUint(firstscanmemory)+(index shl 12));
    firstscanmemoryfile.Seek((index shl 12),soFromBeginning);

    //read 8KB (2 entries)
    firstscanmemoryfile.Read(base^,$2000);

    loadedfromlistMREW.BeginWrite;
    LoadedFromList[index]:=true;
    LoadedFromList[index+1]:=true;
    loadedfromlistMREW.EndWrite;
  end;
end;

function TFirstScanHandler.getpointertoaddress(address:dword;valuetype:tvaluetype): pointer;
var j: integer;
    pm: ^TArrMemoryRegion;
    pa: PDwordArray;
    pab: PBitAddressArray;
    p: pbyte;
    p1: PByteArray;
    p2: PWordArray;
    p3: PDwordArray;
    p4: PSingleArray;
    p5: PDoubleArray;
    p6: PInt64Array;

    first,last: integer;

    pivot: integer;
begin
  result:=nil;

  //5.4: change routine to only read in pages of 4KB if it wasn't paged in yet (does require a page table like list of course)

  p:=pointer(ptrUint(firstscanaddress.Memory)+7);
  

  if firstscantype=fs_advanced then
  begin
    {the addressfile exists out of a list of memoryregions started with the text
     REGION or NORMAL, so skip the first 7 bytes
    }
    pm:=pointer(ptrUint(firstscanaddress.Memory)+7);

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
    p1:=firstscanmemory;
    p2:=firstscanmemory;
    p3:=firstscanmemory;
    p4:=firstscanmemory;
    p5:=firstscanmemory;
    p6:=firstscanmemory;

    if (valuetype <> vt_all) then
    begin
      //addresslist is a list of dword

      j:=(firstscanaddress.Size-7) div sizeof(dword); //max number of addresses , no same as first for binary

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


      exit; //not found
    end
    else
    begin
      //addresslist is a list of 2 dwords, address and vartype, what kind of vartype is not important

      j:=(firstscanaddress.Size-7) div sizeof(tbitaddress); //max number of addresses , no same as first for binary

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

  raise exception.create('Failure in finding '+inttohex(address,8)+' in the first scan results');
end;

function TFirstScanHandler.getfirstscanbyte(address: dword): byte;
begin
  result:=pbyte(getpointertoaddress(address,vt_byte))^; //tries to read nil is not found, which should never happen, so I should get a bug report if it does
end;

function TFirstScanHandler.getfirstscanword(address: dword): word;
begin
  result:=pword(getpointertoaddress(address,vt_word))^; //tries to read nil is not found, which should never happen, so I should get a bug report if it does
end;

function TFirstScanHandler.getfirstscandword(address: dword): dword;
begin
  result:=pdword(getpointertoaddress(address,vt_dword))^; //tries to read nil is not found, which should never happen, so I should get a bug report if it does
end;

function TFirstScanHandler.getfirstscansingle(address: dword): single;
begin
  result:=psingle(getpointertoaddress(address,vt_single))^; //tries to read nil is not found, which should never happen, so I should get a bug report if it does
end;

function TFirstScanHandler.getfirstscandouble(address: dword): double;
begin
  result:=pdouble(getpointertoaddress(address,vt_double))^; //tries to read nil is not found, which should never happen, so I should get a bug report if it does
end;

function TFirstScanHandler.getfirstscanint64(address: dword): int64;
begin
  result:=pint64(getpointertoaddress(address,vt_int64))^; //tries to read nil is not found, which should never happen, so I should get a bug report if it does
end;


constructor TFirstScanHandler.create(scandir: string);
var datatype: string[6];
    pm: ^TArrMemoryRegion;
    i: integer;
    p: ptrUint;
begin
  self.scandir:=scandir;
  try
    try

      firstscanmemoryfile:=Tfilestream.Create(scandir+'MEMORYFIRST.TMP',fmOpenRead or fmsharedenynone);
      firstscanmemory:=virtualalloc(nil, firstscanmemoryfile.Size+$2000, mem_commit, page_readwrite);

      //make an array to store the previous memory in blocks of 4KB
      setlength(loadedfromlist, 2+(firstscanmemoryfile.Size shr 12)); //+2 to keep some extra room, so less checking
      zeromemory(@loadedfromlist[0], length(loadedfromlist));

      loadedfromlistMREW:=TMultiReadExclusiveWriteSynchronizer.create;
    except
      raise exception.Create('No first scan data files found');
    end;

    firstscanaddress:=tmemorystream.Create;
    try
      firstscanaddress.LoadFromFile(scandir+'ADDRESSESFIRST.TMP');
    except
      raise exception.Create('No first scan data files found');
    end;

    firstscanaddress.ReadBuffer(datatype,7);
    if datatype='REGION' then
    begin
      firstscantype:=fs_advanced;
      maxnumberofregions:=(firstscanaddress.Size-7) div sizeof(TMemoryRegion); //max number of regions

      //fill in startaddress elements
      pm:=pointer(ptrUint(firstscanaddress.Memory)+7);
      p:=ptrUint(firstscanmemory);
      for i:=0 to maxnumberofregions-1 do
      begin
        pm[i].startaddress:=pointer(p);
        inc(p, pm[i].MemorySize);
      end;
      
    end
    else
    begin
      firstscantype:=fs_addresslist;
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

destructor TFirstScanHandler.destroy;
begin
  cleanup;
  inherited destroy;
end;

procedure TFirstScanHandler.cleanup;
{Cleanup routine, for use by create when failure and destroy}
begin
  if firstscanmemory<>nil then
  begin
    virtualfree(firstscanmemory,0,MEM_RELEASE);
    firstscanmemory:=nil;
  end;

  freeandnil(loadedfromlistMREW);
  freeandnil(firstscanaddress);
  freeandnil(firstscanmemoryfile);
end;

end.

