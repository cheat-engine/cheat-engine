unit SaveFirstScan;

{$MODE Delphi}


interface

uses
  {$ifdef darwin}
  macport, LCLType,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}
  sysutils, fileutil, LCLIntf, classes, CEFuncProc, commonTypeDefs;

type TSaveFirstScanThread=class(tthread)
  private

  public
    folder: string;
    newscanmethod: boolean;
    memRegion: PMemoryRegions;
    pmemRegionPos: pinteger;
    buffer: pointer;  
    procedure execute; override;
    constructor create(folder: string; suspended: boolean; memRegion: PMemoryRegions; pmemRegionPos: pinteger; buffer: pointer); overload;
end;

implementation

procedure TSaveFirstScanThread.execute;
var //addressfile,memoryfile,newaddressfile,newmermoryfile: file;
    datatype: string[6];
    i: integer;

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
    oldAddressFile:=TFileStream.Create(folder+'ADDRESSES.TMP', fmopenread or fmShareDenyNone);
    oldMemoryFile:=TFileStream.Create(folder+'MEMORY.TMP', fmopenread or fmShareDenyNone);

    if terminated then exit;

    oldaddressfile.ReadBuffer(datatype, sizeof(datatype));

    if datatype='REGION' then
    begin
      //the scan was a unknown initial value scan, so the memory is stored in memory and not on the disk
      //save the memoryregions and memory to disk
      NewAddressFile:=TFileStream.Create(folder+'ADDRESSES.FIRST', fmCreate);
      NewMemoryFile:=TFileStream.Create(folder+'MEMORY.FIRST', fmCreate);

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

      copyfile(pchar(folder+'ADDRESSES.TMP'),pchar(folder+'ADDRESSES.FIRST'),false);
      if terminated then exit;
      copyfile(pchar(folder+'MEMORY.TMP'),pchar(folder+'MEMORY.FIRST'),false);
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


constructor TSaveFirstScanThread.create(folder: string; suspended: boolean; memRegion: PMemoryRegions; pmemRegionPos: pinteger; buffer: pointer);
begin
  self.folder:=folder;
  newscanmethod:=true;
  self.memRegion:=memRegion;
  self.pmemRegionPos:=pmemRegionPos;
  self.buffer:=buffer;
  inherited create(suspended);
end;

end.
