unit SaveFirstScan;


interface

uses windows, classes, cefuncproc;

type TSaveFirstScanThread=class(tthread)
  public
    procedure execute; override;
end;

implementation

procedure TSaveFirstScanThread.execute;
var addressfile,memoryfile,newaddressfile,newmermoryfile: file;
    datatype: string[6];
    i: integer;
    x: dword;
    p: ^byte;
begin
  if terminated then exit;

  //open the address file to
  assignfile(AddressFile,Cheatenginedir+'ADDRESSES.TMP');
  assignfile(Memoryfile,cheatenginedir+'MEMORY.TMP');
  assignfile(newaddressfile,cheatenginedir+'ADDRESSESFIRST.TMP');
  assignfile(NewMemoryFile,cheatenginedir+'MEMORYFIRST.TMP');
  reset(Addressfile,1);
  reset(memoryfile,1);

  try
    if terminated then exit;

    datatype:='NORMAL';
    blockread(Addressfile,datatype,sizeof(datatype));
    if datatype='REGION' then
    begin
      //the scan was a unknown initial value scan, so the memory is stored in memory and not on the disk
      //save the memoryregions and memory to disk
      rewrite(NewAddressFile,1);
      rewrite(NewMemoryfile,1);
      blockwrite(NewAddressfile,datatype,sizeof(datatype));
      try
        p:=pointer(memory);
        for i:=0 to memoryregions do
        begin
          blockwrite(newaddressfile,memoryregion[i],sizeof(memoryregion[i]),x);
          blockwrite(newmemoryfile,p^,memoryregion[i].MemorySize,x);
          inc(p,memoryregion[i].MemorySize);
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

end.
