unit firstscanhandler;

{
First scan handler is a class that will help with scanning the results of the
first scan.
It'll read the results of the first scan and provides an inteface for the
scanroutines for quick lookup of the previous value of a specific address
}

interface

uses windows,classes,sysutils,syncobjs;

type TFirstscantype= (fs_advanced,fs_addresslist);
type TValueType= (vt_byte,vt_word, vt_dword, vt_single, vt_double, vt_int64);


type TFirstScanHandler = class
  private
    firstscanmemoryfile: TFileStream;
    firstscanaddress: tmemorystream;
    firstscantype: tfirstscantype;

    firstscanmemory: pointer;
    LoadedFromList: array of boolean;
    LoadedFromListMREW: TMultiReadExclusiveWriteSynchronizer;
    maxnumberofregions: integer;


    procedure cleanup;
    function loadIfNotLoaded(p: pointer): pointer;
    function getpointertoaddress(address:dword;valuetype:tvaluetype): pointer;
  public
    function getfirstscanbyte(address: dword): byte;
    function getfirstscanword(address: dword): word;
    function getfirstscandword(address: dword): dword;
    function getfirstscansingle(address: dword): single;
    function getfirstscandouble(address: dword): double;
    function getfirstscanint64(address: dword): int64;

    constructor create;
    destructor destroy; override;
end;


implementation

uses cefuncproc;

type TArrMemoryRegion= array [0..0] of TMemoryRegion;

function TFirstScanHandler.loadIfNotLoaded(p: pointer): pointer;
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
  if not LoadedFromList[(dword(p)-dword(firstscanmemory)) shr 12] then
  begin
    //not loaded yet, load this section
    index:=(dword(p)-dword(firstscanmemory)) shr 12;

    base:=pointer(dword(firstscanmemory)+(index shl 12));
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

var i,j,k: integer;
    pm: ^TArrMemoryRegion;
    pa: PDwordArray;
    p: pbyte;
    p1: PByteArray;
    p2: PWordArray;
    p3: PDwordArray;
    p4: PSingleArray;
    p5: PDoubleArray;
    p6: PInt64Array;

    first,last: dword;
    found: boolean;
begin
  result:=nil;

  //5.4: change routine to only read in pages of 4KB if it wasn't paged in yet (does require a page table like list of course)

  p:=firstscanMemory;
  

  if firstscantype=fs_advanced then
  begin
    {the addressfile exists out of a list of memoryregions started with the text
     REGION or NORMAL, so skip the first 7 bytes
    }
    pm:=pointer(dword(firstscanaddress.Memory)+7);

    //find the region this address belongs to
    //the region list should be sorted

    found:=false;

    first:=0;
    last:=maxnumberofregions-1;
    while (first<last) do
    begin
      j:=first+((last-first) div 2);
      if k=j then
      begin
        if k=first then
        begin
          first:=last;
          j:=last;
        end;

        if k=last then
        begin
          last:=first;
          j:=last;
        end;
      end;

      k:=j;


      if address<pm[j].baseaddress then
        last:=j
      else
      begin
        if (address>=pm[j].baseaddress) and (address < (pm[j].BaseAddress + pm[j].MemorySize)) then
        begin
          //found it
          result:=loadifnotloaded(pointer(dword(pm[j].startaddress)+(address-pm[j].baseaddress)));
          exit;

        end;

        first:=j;
      end;
    end;

    asm
      db $cc;
    end;


  end
  else
  begin
    //the addressfile exists out of a list of addresses
    pa:=pointer(p);
    j:=(firstscanaddress.Size-7) div sizeof(dword); //max number of addresses , no same as first for binary
    p1:=firstscanmemory;
    p2:=firstscanmemory;
    p3:=firstscanmemory;
    p4:=firstscanmemory;
    p5:=firstscanmemory;
    p6:=firstscanmemory;
    for i:=0 to j-1 do
    begin
      if (address<=pa[i]) then
      begin
        if address=pa[i] then
        begin
          case valuetype of
            vt_byte : result:=@p1[i];
            vt_word : result:=@p2[i];
            vt_dword : result:=@p3[i];
            vt_single: result:=@p4[i];
            vt_double: result:=@p5[i];
            vt_int64: result:=@p6[i];
          end;

          exit;
        end
        else exit; //not found
      end;
    end;  
  end;
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


constructor TFirstScanHandler.create;
var datatype: string[6];
    pm: ^TArrMemoryRegion;
    i: integer;
    p: dword;
begin
  try
    try
      firstscanmemoryfile:=Tfilestream.Create('MEMORYFIRST.TMP',fmOpenRead);
      firstscanmemory:=virtualalloc(nil, firstscanmemoryfile.Size+$2000, mem_commit, page_readwrite);

      setlength(loadedfromlist, 2+(firstscanmemoryfile.Size shr 12)); //+2 to keep some extra room, so less checking
      zeromemory(@loadedfromlist[0], length(loadedfromlist));

      loadedfromlistMREW:=TMultiReadExclusiveWriteSynchronizer.create;
    except
      raise exception.Create('No first scan data files found');
    end;

    firstscanaddress:=tmemorystream.Create;
    try
      firstscanaddress.LoadFromFile(cheatenginedir+'ADDRESSESFIRST.TMP');
    except
      raise exception.Create('No first scan data files found');
    end;

    firstscanaddress.ReadBuffer(datatype,7);
    if datatype='REGION' then
    begin
      firstscantype:=fs_advanced;
      maxnumberofregions:=(firstscanaddress.Size-7) div sizeof(TMemoryRegion); //max number of regions

      //fill in startaddress elements
      pm:=pointer(dword(firstscanaddress.Memory)+7);
      p:=dword(firstscanmemory);
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
