unit SaveFirstScan;

{$MODE Delphi}


interface

uses windows, LCLIntf, classes, CEFuncProc;

type TSaveFirstScanThread=class(tthread)
  public
    newscanmethod: boolean;
    memRegion: PMemoryRegions;
    pmemRegionPos: pinteger;
    buffer: pointer;  
    procedure execute; override;
    constructor create(suspended: boolean; memRegion: PMemoryRegions; pmemRegionPos: pinteger; buffer: pointer); overload;
end;

implementation

procedure TSaveFirstScanThread.execute;
var //addressfile,memoryfile,newaddressfile,newmermoryfile: file;
    datatype: string[6];
    i: integer;
    x: dword;
    p: ^byte;
    regioncount: integer;

    oldAddressFile,oldMemoryFile, newAddressFile, newMemoryFile: Tfilestream;
begin
  if terminated then exit;


  regioncount:=pmemregionPos^;

  if newscanmethod then
    dec(regioncount);

  //open the files
  oldAddressFile:=nil;
  oldMemoryFile:=nil;
  newAddressFile:=nil;
  newMemoryFile:=nil;


  try
    oldAddressFile:=TFileStream.Create(Cheatenginedir+'ADDRESSES.TMP', fmopenread or fmShareDenyNone);
    oldMemoryFile:=TFileStream.Create(cheatenginedir+'MEMORY.TMP', fmopenread or fmShareDenyNone);

    if terminated then exit;

    oldaddressfile.ReadBuffer(datatype, sizeof(datatype));

    if datatype='REGION' then
    begin
      //the scan was a unknown initial value scan, so the memory is stored in memory and not on the disk
      //save the memoryregions and memory to disk
      NewAddressFile:=TFileStream.Create(Cheatenginedir+'ADDRESSESFIRST.TMP', fmCreate);
      NewMemoryFile:=TFileStream.Create(cheatenginedir+'MEMORYFIRST.TMP', fmCreate);

      newAddressfile.WriteBuffer(datatype,sizeof(datatype));

      p:=buffer;
      for i:=0 to regioncount do  //no, not a bug
      begin
        newaddressfile.WriteBuffer(memregion^[i], sizeof(memregion^[i]));
        newmemoryfile.WriteBuffer(p^, memregion^[i].MemorySize);
        inc(p,memregion^[i].MemorySize);

        if terminated then exit;
      end;
    end
    else
    begin
      //exact value scan or other scan that gives addresses
      //copy the results to addressesfirst.tmp and memoryfirst.tmp
      copyfile(pchar(Cheatenginedir+'ADDRESSES.TMP'),pchar(Cheatenginedir+'ADDRESSESFIRST.TMP'),false);
      if terminated then exit;
      copyfile(pchar(Cheatenginedir+'MEMORY.TMP'),pchar(Cheatenginedir+'MEMORYFIRST.TMP'),false);
    end;

  finally
    if oldAddressFile<>nil then
      oldAddressFile.free;

    if oldmemoryfile<>nil then
      oldmemoryfile.free;

    if newaddressfile<>nil then
      newaddressfile.free;

    if newmemoryfile<>nil then
      newmemoryfile.free;
  end;
end;


constructor TSaveFirstScanThread.create(suspended: boolean; memRegion: PMemoryRegions; pmemRegionPos: pinteger; buffer: pointer);
begin
  newscanmethod:=true;
  self.memRegion:=memRegion;
  self.pmemRegionPos:=pmemRegionPos;
  self.buffer:=buffer;
  inherited create(suspended);
end;

end.
