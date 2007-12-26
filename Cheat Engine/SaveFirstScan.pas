unit SaveFirstScan;


interface

uses windows, classes, cefuncproc;

type TSaveFirstScanThread=class(tthread)
  public
    newscanmethod: boolean;
    memRegion: PMemoryRegions;
    pmemRegionPos: pinteger;
    buffer: pointer;  
    procedure execute; override;
    constructor create(suspended: boolean); overload;
    constructor create(suspended: boolean; memRegion: PMemoryRegions; pmemRegionPos: pinteger; buffer: pointer); overload;
end;

implementation

procedure TSaveFirstScanThread.execute;
var addressfile,memoryfile,newaddressfile,newmermoryfile: file;
    datatype: string[6];
    i: integer;
    x: dword;
    p: ^byte;
    regioncount: integer;
begin
  if terminated then exit;

  regioncount:=pmemregionPos^;

  if newscanmethod then
    dec(regioncount);

  //open the address file to
  assignfile(AddressFile,Cheatenginedir+'ADDRESSES.TMP');
  assignfile(Memoryfile,cheatenginedir+'MEMORY.TMP');
  assignfile(newaddressfile,cheatenginedir+'ADDRESSESFIRST.TMP');
  assignfile(NewMemoryFile,cheatenginedir+'MEMORYFIRST.TMP');
  reset(Addressfile,1);
  reset(memoryfile,1);

  try
    if terminated then exit;

    //datatype:='NORMAL';
    blockread(Addressfile,datatype,sizeof(datatype));
    if datatype='REGION' then
    begin
      //the scan was a unknown initial value scan, so the memory is stored in memory and not on the disk
      //save the memoryregions and memory to disk
      rewrite(NewAddressFile,1);
      rewrite(NewMemoryfile,1);
      blockwrite(NewAddressfile,datatype,sizeof(datatype));
      try
        p:=buffer;
        for i:=0 to regioncount do
        begin
          blockwrite(newaddressfile,memregion^[i],sizeof(memregion^[i]),x);
          blockwrite(newmemoryfile,p^,memregion^[i].MemorySize,x);
          inc(p,memregion^[i].MemorySize);
          if terminated then exit;
        end;

      finally
        closefile(newaddressfile);
        closefile(NewMemoryFile);
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
    closefile(addressfile);
    closefile(Memoryfile);
  end;
end;

constructor TSaveFirstScanThread.create(suspended: boolean);
begin
  self.memRegion:=@cefuncproc.memoryregion; //old method, so point to cefuncproc.memoryregion
  self.pmemRegionPos:=@cefuncproc.memoryregions;
  self.buffer:=cefuncproc.memory;
  inherited create(suspended);
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
