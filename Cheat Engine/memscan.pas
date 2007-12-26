unit memscan;
{
This unit will hold the class object used to control scanning
The old scanning routines will be moved out of cefuncproc and made object oriented into this class
Special care should be taken to add multithreaded scanning routines
}

interface

uses windows,sysutils, classes,foundlisthelper,cefuncproc,newkernelhandler;

type TScanOption=(soUnknownValue,soExactValue,soValueBetween,soBiggerThan,soSmallerThan,soChanged,soUnchanged,soCustom);
type TScanType=(stNewScan, stFirstScan, stNextScan);
type TVariableType=(vtByte,vtWord,vtDword,vtQword,vtSingle,vtDouble,vtByteArray,vtBinary,vtCustom);

type
  TMemScan=class;
  TScanController=class;

  TScanner=class(tthread)
  {
    The scanner class will scan a specified range of memory
  }
  private
    procedure firstscan; //copy the given range to the memory region
//    procedure firstnextscan; //routine used when the results list contains nothing but a indicator a unknown scan was done
//    procedure nextnextscan; //routine used when the results list contains addresses
  public
    OwningScanController: TScanController;

    hexadecimal: boolean;
    binaryStringAsDecimal: boolean;
    readonly: boolean;
    fastscan: boolean;
    fastscanalignsize: integer;
    variablesize: integer;
    scanvalue1,scanvalue2: string;
    startaddress: dword; //specific start for this this thread
    stopaddress: dword; //specific stop for this thread, if not fastscan and another thread continue from here, may add some overlopping bytes
    scanOption: TScanOption;
    variableType: TVariableType;
    scanType: TScanType; //defines if it's a firstscan or next scan. (newscan is ignored)


    //thread controlling variables:
    isdone: boolean; //will get set to true when the thread finishes normally
    haserror: boolean;
    errorstring: string;

    //recreated memory region list for this specific range, can be used to see which regions where only half read
    memRegions: TMemoryregions;
    memRegionPos: integer;

    procedure execute; override;
  end;

  TScanController=class(tthread)
  {
    The ScanController will configure the scanners and wait till they are done, mainly a idle thread
  }
  private

    procedure firstScan;
//    procedure firstnextscan; //rotine used when the results list contains nothing but a indicator a unknown scan was done
//    procedure nextnextscan; //routine used when the results list contains addresses
    
  public
    OwningMemScan: TMemScan;
    scanners: array of tscanner;


    hexadecimal: boolean;
    binaryStringAsDecimal: boolean;
    readonly: boolean;
    fastscan: boolean;
    fastscanalignsize: integer;
    variablesize: integer;
    scanvalue1,scanvalue2: string;
    startaddress: dword; //start for the whole scan
    stopaddress: dword; //stop of the whole scan
    scanOption: TScanOption;
    variableType: TVariableType;
    scanType: TScanType; //defines if it's a firstscan or next scan. (newscan is ignored)


    //memregion info
    memregion: TMemoryregions;  //scanners have access to this, but make sure to NOT WRITE it
    memRegionPos: Integer;


    //thread controlling variables:
    isdone: boolean; //will get set to true when the thread finishes normally
    haserror: boolean;
    errorstring: string;
    procedure execute; override;
  end;

  TMemScan=class
  {
    Configures the gui and related objects and launch TScanner objects with those objects
  }
  private
    previousMemoryBuffer: pointer;
    scanController: TScanController;
    memRegion: TMemoryRegions;  //after a scan the contents of controller gets copied to here
    memRegionPos: integer;

    foundlist: TFoundlist;

  public
    function GetProgress(var totalmemory:dword; currentmemoryscanned: dword):integer;
    procedure newscan; //will clean up the memory and files
    procedure firstscan(scanOption: TScanOption; VariableType: TVariableType; scanvalue1, scanvalue2: string; startaddress,stopaddress: dword; fastscan,readonly,hexadecimal,binaryStringAsDecimal: boolean); //first scan routine, e.g unknown initial value, or exact scan
//    procedure nextscan; //next scan, determine what kind of scan and give to firstnextscan/nextnextscan
    constructor create(foundlist: TFoundlist);
    destructor destroy; override;
  end;

implementation


//===============Local function================//
function getBytecountArrayOfByteString(st: string): integer;
var bytes: tbytes;
begin
  ConvertStringToBytes(st,false,bytes);
  result:=length(bytes);
end;

function convertValueToBinaryString(v: dword): string;
var i: integer;
    dwordvalue: dword;
begin
  result:='';
  while v>0 do
  begin
    if (v mod 2)>0 then result:=result+'1'
                   else result:=result+'0';

    v:=v div 2;
  end;
end;

function getBytecountBinaryString(st:string; scanvalueisdecimal: boolean): integer;
var value: dword;
    i: integer;
begin
  if scanvalueisdecimal then //first convert do binarystring
    st:=convertValueToBinaryString(strtoint(st));

  if not scanvalueisdecimal then
  begin
    result:=0;
    for i:=1 to length(st) do
    begin
      case st[i] of
        '0','1','?','*': inc(result);
        ' ',#8: ; //ignore
        else raise exception.Create(st[i]+' is not a valid character inside a binary string');
      end;
    end;
  end;

  if result=0 then raise exception.Create('Invalid binary notation');
  if (result mod 8=0) then
    result:=result div 8
  else
    result:=1+(result div 8);

end;


//==================TScanner===================//

procedure TScanner.firstscan;
var i: integer;
    startregion: integer;
    stopregion: integer;
    currentbase: dword;
    size: dword;
    actualread: dword;
    previousmemorybuffer: ^byte;
begin
  //first find out where in the previousmemory of this thread starts
  previousmemorybuffer:=nil;
  for i:=0 to self.OwningScanController.memRegionPos-1 do
  begin
    //find the region where startaddress belongs
    if (OwningScanController.memregion[i].BaseAddress>=startaddress) and
       (OwningScanController.memregion[i].BaseAddress+OwningScanController.memregion[i].MemorySize<startaddress) then       //region found
    begin
      previousmemorybuffer:=pointer(dword(OwningScanController.memregion[i].startaddress)+startaddress-OwningScanController.memregion[i].BaseAddress);
      startregion:=i;
    end;

    if (OwningScanController.memregion[i].BaseAddress>=stopaddress) and
       (OwningScanController.memregion[i].BaseAddress+OwningScanController.memregion[i].MemorySize<stopaddress) then       //region found
    begin
      previousmemorybuffer:=pointer(dword(OwningScanController.memregion[i].startaddress)+startaddress-OwningScanController.memregion[i].BaseAddress);
      stopregion:=i;
      break; //because for some unknown reason stop is always after start i can break now...
    end;
  end;
  if previousmemorybuffer=nil then raise exception.Create('startregion not found');


  //now save the region between startaddress and stopaddress and create own memregion list
  setlength(memregions,16);

  currentbase:=startaddress;
  size:=OwningScanController.memregion[startregion].MemorySize-(startaddress-OwningScanController.memregion[i].BaseAddress);
  memregions[0].BaseAddress:=currentbase;
  memregions[0].MemorySize:=size;
  memregions[0].startaddress:=previousmemorybuffer;

  actualread:=0;
  ReadProcessMemory(processhandle,pointer(currentbase),previousmemorybuffer,size,actualread);
  memregions[0].MemorySize:=actualread;
  inc(previousmemorybuffer,size);
  inc(memregionpos);

  for i:=startregion+1 to stopregion do
  begin
    if terminated then exit;
    
    currentbase:=OwningScanController.memregion[i].BaseAddress;
    size:=OwningScanController.memregion[i].MemorySize;
    if currentbase+size>stopaddress then
      size:=stopaddress-currentbase;

    memregions[i].BaseAddress:=currentbase;
    memregions[i].MemorySize:=size;
    memregions[i].startaddress:=previousmemorybuffer;

    actualread:=0;
    ReadProcessMemory(processhandle,pointer(currentbase),previousmemorybuffer,size,actualread);
    memregions[i].MemorySize:=actualread;

    inc(previousmemorybuffer,size);
    inc(memregionpos);
    if (memregionpos mod 16) = 0 then
      setlength(memregions,length(memregions)+16);
  end;
end;

procedure TScanner.execute;
var i: integer;
begin
  try
    if scantype=stFirstScan then firstscan;
  except
    on e: exception do
    begin
      haserror:=true;
      errorstring:=e.message;

      //tell all siblings to terminate, something messed up
      //and I can just do this, since the ScanController is waiting for us
      for i:=0 to length(OwningScanController.scanners)-1 do
        OwningScanController.Terminate;

    end;
  end;

  isdone:=true;
end;

//===============TScanController===============//

procedure TScanController.firstScan;
var
  currentBaseAddress: dword;
  mbi : TMemoryBasicInformation;
  totalProcessMemorySize: dword;
  i,j: integer;
  threadcount: integer;
  Blocksize: dword;
  currentblocksize: dword;
begin
  threadcount:=1;
  {
  ScanController plan:
  spawn idle scanner threads , ammount=maxthreadcount in settings
  enumerate all regions and split up into jobs for the threads
  if scanoption=soUnknownValue make a buffer  big enough to hold all the memory, and give the threads the startbase where in the buffer their first region will start
  start scanner threads
  }

  //determine the size in bytes for this variable. If one is provided, and fill in the fastscan alignment as well
  if scanOption<>soUnknownValue then
    case variableType of
      vtByte:
      begin
        fastscanalignsize:=1;
        variablesize:=1;
      end;

      vtWord:
      begin
        fastscanalignsize:=2;
        variablesize:=2;
      end;

      vtDWord:
      begin
        fastscanalignsize:=4;
        variablesize:=4;
      end;

      vtQWord:
      begin
        fastscanalignsize:=4;
        variablesize:=8;
      end;

      vtSingle:
      begin
        fastscanalignsize:=4;
        variablesize:=4;
      end;

      vtDouble:
      begin
        fastscanalignsize:=4;
        variablesize:=8;
      end;

      vtByteArray:
      begin
        variablesize:=getBytecountArrayOfByteString(scanvalue1);
        fastscanalignsize:=1;
      end;

      vtBinary:
      begin
        variablesize:=getBytecountBinaryString(scanvalue1,binaryStringAsDecimal);
        fastscanalignsize:=1;
      end;

      vtCustom:
      begin
        variablesize:=1; //customtypecallback(scanvalue1);
        fastscanalignsize:=1;
      end;
    end;

  //else it's ignored and never used



  setlength(memRegion,16);
  memRegionPos:=0;

  if (startaddress mod 8)>0 then //align on a 8 byte base
    startaddress:=startaddress-(startaddress mod 8);

  currentBaseAddress:=startaddress;
  while (Virtualqueryex(processhandle,pointer(currentBaseAddress),mbi,sizeof(mbi))<>0) and (currentBaseAddress<stopaddress) and ((currentBaseAddress+mbi.RegionSize)>currentBaseAddress) do   //last check is done to see if it wasn't a 64-bit overflow.
  begin
    if (not (not scan_mem_private and (mbi.type_9=mem_private))) and (not (not scan_mem_image and (mbi.type_9=mem_image))) and (not (not scan_mem_mapped and (mbi.type_9=mem_mapped))) and (mbi.State=mem_commit) and ((mbi.Protect and page_guard)=0) and ((mbi.protect and page_noaccess)=0) then  //look if it is commited
    begin
      if //no cache check
         (Skip_PAGE_NOCACHE and ((mbi.AllocationProtect and PAGE_NOCACHE)=PAGE_NOCACHE))
         or
         //no readonly check
         ((not readonly) and (not ((((mbi.AllocationProtect) and (page_readonly or page_execute_read))=0) and
           (((mbi.Protect) and (page_readonly or PAGE_EXECUTE_READ))=0))))
       then
      begin
        //skip it
        currentBaseAddress:=dword(mbi.BaseAddress)+mbi.RegionSize;
        continue;
      end;

      inc(totalProcessMemorySize,mbi.RegionSize); //add this size to the total

      if memRegionPos>0 then
      begin
        //check if it can be appended to the previous region
        if memRegion[memRegionPos-1].BaseAddress+memRegion[memRegionPos].MemorySize=dword(mbi.baseaddress) then //yes, append
        begin
          //yes, so append
          memRegion[memRegionPos-1].MemorySize:=memRegion[memRegionPos-1].MemorySize+mbi.RegionSize;
          continue;
        end;
      end;

      //still here, so a new region
      memRegion[memRegionPos].BaseAddress:=dword(mbi.baseaddress);  //just remember this location
      memRegion[memRegionPos].MemorySize:=mbi.RegionSize;
      memRegion[memRegionPos].startaddress:=pointer(totalProcessMemorySize); //starts from 0, for unknown scans

      inc(memRegionPos);
      if (memRegionPos mod 16)=0 then //add another 16 to it
        setlength(memRegion,length(memRegion)+16);
    end;

    currentBaseAddress:=dword(mbi.baseaddress)+mbi.RegionSize;
  end;

  if memRegionPos=0 then raise exception.Create('No readable memory found');

  //if soUnknown, make a buffer where it can store all the 'previous' memory
  if scanOption=soUnknownValue then
  begin
    //extra check to make sure the previous scan was cleared
    if OwningMemScan.previousMemoryBuffer<>nil then freemem(OwningMemScan.previousMemoryBuffer);
    getmem(OwningMemScan.previousMemoryBuffer,totalProcessMemorySize);
  end;


  //split up into seperate workloads, if scanOption=soUnknownValue no overlap
  Blocksize:=totalProcessMemorySize div threadcount;
  Blocksize:=blocksize-(blocksize mod 4096); //lastblock gets the missing bytes


  setlength(scanners,threadcount);
  j:=0; //start at memregion 0
  for i:=0 to threadcount-1 do
  begin
    scanners[i]:=tscanner.Create(true);
    scanners[i].OwningScanController:=self;
    
    scanners[i].startaddress:=memRegion[j].BaseAddress;

//    if scanOption=soUnknownValue then //tell it where it should start writing
//      scanners[i].previousMemoryBuffer:=pointer(dword(OwningMemScan.previousMemoryBuffer)+);


    if i=(threadcount-1) then
      scanners[i].stopaddress:=stopaddress
    else
    begin
      currentblocksize:=0;
      repeat
        inc(currentblocksize,memregion[j].MemorySize);
        inc(j);
      until (currentblocksize>blocksize) or (j>=memregionpos); //j>=memregionpos should not happen often
      dec(j);

      scanners[i].stopaddress:=memregion[j].BaseAddress+memregion[j].MemorySize;
      dec(scanners[i].stopaddress,(scanners[i].stopaddress-scanners[i].startaddress) mod blocksize);

      if scanOption<>soUnknownValue then //not a unknown intiial value scan, so it doesn't need overlap
        inc(scanners[i].stopaddress,variablesize-1); //add some overlap

    end;

    //now configure the scanner thread with the same info this thread got, with some extra info
    scanners[i].scanType:=scanType; //stFirstScan obviously
    scanners[i].scanoption:=scanoption;
    scanners[i].scantype:=scanType;
    scanners[i].variableType:=VariableType;
    scanners[i].fastscan:=fastscan;
    scanners[i].scanValue1:=scanvalue1; //usual scanvalue
    scanners[i].scanValue2:=scanValue2; //2nd value for between scan
    scanners[i].readonly:=readonly;
    scanners[i].hexadecimal:=hexadecimal;
    scanners[i].binaryStringAsDecimal:=binaryStringAsDecimal;

    scanners[i].fastscanalignsize:=fastscanalignsize;
    scanners[i].variablesize:=variablesize;


    //and run it
    scanners[i].Resume;
  end;

  //and now we wait
  for i:=0 to threadcount-1 do
  begin
    scanners[i].WaitFor; //if the mainthread has to cancel, it has to tell the child scanners to terminate instead
    if scanners[i].haserror then
    begin
      haserror:=true;
      errorstring:=scanners[i].errorstring;
      break;
    end;
  end;

  if haserror then exit;

  //all threads are done
  if scanOption=soUnknownValue then
  begin
    //read the scanner memregions and adapt this memregion to it
    for i:=0 to threadcount-1 do
    begin

    end;
  end;

  OwningMemScan.memRegion:=memRegion;
  OwningMemScan.memRegionPos:=memRegionPos;
end;

procedure TScanController.execute;
begin
  //check what it is, and call first/next/nextnext- scan
  try
    if scantype=stFirstScan then firstscan;

  except
    on e: exception do
    begin
      haserror:=true;
      errorstring:=e.message;
    end;
  end;

  isdone:=true;
  //send message saying it's done
end;

function TMemscan.GetProgress(var totalmemory:dword; currentmemoryscanned: dword):integer;
{returns a value between 1 and 1000 representing how far the scan is}
var i: integer;
begin
  //Take care of memory
  if self.scanController<>nil then
  begin
    result:=(currentmemoryscanned div totalmemory) * 1000;
  end
  else
  begin
    result:=0;
    totalmemory:=0;
    currentmemoryscanned:=0;
  end;
end;

procedure TMemscan.newscan;
begin
  if scanController<>nil then
  begin
    scanController.terminate;
    scanController.WaitFor;
    FreeAndNil(scanController);
  end;

  if previousMemoryBuffer<>nil then freemem(previousMemoryBuffer);
end;

procedure TMemscan.firstscan(scanOption: TScanOption; VariableType: TVariableType; scanvalue1, scanvalue2: string; startaddress,stopaddress: dword; fastscan,readonly,hexadecimal,binaryStringAsDecimal: boolean);
{
Spawn the controller thread and fill it with the required data
Popup the wait window, or not ?
}
begin

  scanController:=TscanController.Create(true);
  scanController.scantype:=stFirstScan;
  scanController.scanOption:=scanOption;
  scanController.variableType:=VariableType;
  scanController.fastscan:=fastscan;
  scanController.scanValue1:=scanvalue1; //usual scanvalue
  scanController.scanValue2:=scanValue2; //2nd value for between scan
  scanController.readonly:=readonly;
  scanController.startaddress:=startaddress;
  scanController.stopaddress:=stopaddress;

  scancontroller.hexadecimal:=hexadecimal;
  scancontroller.binaryStringAsDecimal:=binaryStringAsDecimal;

  scanController.Resume;
end;

constructor TMemScan.create(foundlist: TFoundList);
begin
  self.foundlist:=foundlist;
  
end;
     
destructor TMemScan.destroy;
begin
  if previousMemoryBuffer<>nil then freemem(previousMemoryBuffer);
  inherited Destroy;
end;

end.
