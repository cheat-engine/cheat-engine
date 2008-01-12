unit memscan;
{
This unit will hold the class object used to control scanning
The old scanning routines will be moved out of cefuncproc and made object oriented into this class
Special care should be taken to add multithreaded scanning routines
}

interface

uses windows,sysutils, classes,ComCtrls,dialogs, cefuncproc,
     newkernelhandler, math, SyncObjs, SaveFirstScan, firstscanhandler,
     autoassembler;

type TScanOption=(soUnknownValue,soExactValue,soValueBetween,soBiggerThan,soSmallerThan, soIncreasedValue, soIncreasedValueBy, soDecreasedValue, soDecreasedValueBy, soChanged, soUnchanged, soSameAsFirst, soCustom);
type TScanType=(stNewScan, stFirstScan, stNextScan);
type TRoundingType=(rtRounded,rtExtremerounded,rtTruncated);
type TVariableType=(vtByte, vtWord, vtDword, vtQword, vtSingle, vtDouble, vtString, vtByteArray, vtBinary, vtAll, vtCustom);
type TCustomScanType=(cstNone, cstAutoAssembler, cstCPP, cstDLLFunction);

type TCheckRoutine=function(newvalue,oldvalue: pointer):boolean of object;
type TStoreResultRoutine=procedure(address: dword; oldvalue: pointer) of object;
type TFlushRoutine=procedure of object;



type
  TMemScan=class;
  TScanController=class;

  Tscanfilewriter=class(tthread)
  {
  workerthread:
  This class is used because it is more efficient to write only one file at a
  time. It will aquire a critical section when writing a file to acomplish that

  also, not that it matters much, since you eventually have to wait for the
  write anyhow, a double buffered result list so it can keep scanning while the
  list is being saved.
  }
  private
    scancontroller: TScanController;
    addressfile: TFilestream;
    memoryfile: TFilestream;
//    cAddressFile,cMemoryFile: TCompressionStream;

    addressbuffer: pointer;
    memorybuffer: pointer;
    addressSize: dword;
    memorySize: dword;



    datawritten: tevent; //event that is set when the thread has finished writing
    dataavailable:tevent; //event that is set when there is a buffer to save
  public
    procedure execute; override;
    procedure writeresults(addressbuffer,memorybuffer: pointer; addressSize,memorySize: dword); //writes the results of address and memory
    procedure flush;
    constructor create(scancontroller:TScanController; addressfile,memoryfile:TFileStream);
    destructor destroy; override;
  end;


  TScanner=class(tthread)
  {
    The scanner class will scan a specified range of memory
  }
  private
    CheckRoutine: TCheckRoutine;
    StoreResultRoutine: TStoreResultRoutine;
    FlushRoutine: TFlushRoutine; //pointer to routine used to flush the buffer, generic, string, etc...
    scanWriter: Tscanfilewriter;
    firstscanhandler: TFirstscanhandler;

    customprologue: procedure; stdcall; //customscan call before scan starts
    customepilogue: procedure; stdcall; //customscan call after scan ends

    found :dword;

    value,value2: int64;
    svalue,svalue2: single;
    dvalue,dvalue2: double;
    minsvalue,maxsvalue: single;
    mindvalue,maxdvalue: double;
    floataccuracy: integer; //number of digits after the decimal seperator

    CurrentAddressBuffer: pointer;
    CurrentFoundBuffer: pointer; //generic buffer that points to where the memory can be found
    SecondaryFoundBuffer: pointer;   //current gets swapped with these ones after a write
    SecondaryAddressBuffer: pointer;

    //binary scan
    bitmask: int64; //contains the bit string where * is replaced with a 0
    andmask: int64; //contains the bit string where 1 and 0 is 1 and * is 0
    binaryresults: array [0..7] of boolean; //after a byte is checked this contains which startbits match

    //array of bytes
    abs_arraylength: integer; //optimization so no need to call length()
    abs_arraytofind: TBytes;

    //all
    typesmatch: array [vtByte..vtDouble] of boolean;  //will get set depending if that type matches the current address or not

    //check routines:
    function ByteExact(newvalue,oldvalue: pointer): boolean;
    function ByteBetween(newvalue,oldvalue: pointer): boolean;
    function ByteBiggerThan(newvalue,oldvalue: pointer): boolean;
    function ByteSmallerThan(newvalue,oldvalue: pointer): boolean;
    function ByteIncreasedValue(newvalue,oldvalue: pointer): boolean;
    function ByteIncreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function ByteDecreasedValue(newvalue,oldvalue: pointer): boolean;
    function ByteDecreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function ByteChanged(newvalue,oldvalue: pointer): boolean;
    function ByteUnChanged(newvalue,oldvalue: pointer): boolean;

    function WordExact(newvalue,oldvalue: pointer): boolean;
    function WordBetween(newvalue,oldvalue: pointer): boolean;
    function WordBiggerThan(newvalue,oldvalue: pointer): boolean;
    function WordSmallerThan(newvalue,oldvalue: pointer): boolean;
    function WordIncreasedValue(newvalue,oldvalue: pointer): boolean;
    function WordIncreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function WordDecreasedValue(newvalue,oldvalue: pointer): boolean;
    function WordDecreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function WordChanged(newvalue,oldvalue: pointer): boolean;
    function WordUnChanged(newvalue,oldvalue: pointer): boolean;

    function DWordExact(newvalue,oldvalue: pointer): boolean;
    function DWordBetween(newvalue,oldvalue: pointer): boolean;
    function DWordBiggerThan(newvalue,oldvalue: pointer): boolean;
    function DWordSmallerThan(newvalue,oldvalue: pointer): boolean;
    function DWordIncreasedValue(newvalue,oldvalue: pointer): boolean;
    function DWordIncreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function DWordDecreasedValue(newvalue,oldvalue: pointer): boolean;
    function DWordDecreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function DwordChanged(newvalue,oldvalue: pointer): boolean;
    function DwordUnChanged(newvalue,oldvalue: pointer): boolean;


    function QWordExact(newvalue,oldvalue: pointer): boolean;
    function QWordBetween(newvalue,oldvalue: pointer): boolean;
    function QWordBiggerThan(newvalue,oldvalue: pointer): boolean;
    function QWordSmallerThan(newvalue,oldvalue: pointer): boolean;
    function QWordIncreasedValue(newvalue,oldvalue: pointer): boolean;
    function QWordIncreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function QWordDecreasedValue(newvalue,oldvalue: pointer): boolean;
    function QWordDecreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function QWordChanged(newvalue,oldvalue: pointer): boolean;
    function QwordUnChanged(newvalue,oldvalue: pointer): boolean;

    function SingleExact(newvalue,oldvalue: pointer): boolean;
    function SingleBetween(newvalue,oldvalue: pointer): boolean;
    function SingleBiggerThan(newvalue,oldvalue: pointer): boolean;
    function SingleSmallerThan(newvalue,oldvalue: pointer): boolean;
    function SingleIncreasedValue(newvalue,oldvalue: pointer): boolean;
    function SingleIncreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function SingleDecreasedValue(newvalue,oldvalue: pointer): boolean;
    function SingleDecreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function SingleChanged(newvalue,oldvalue: pointer): boolean;
    function singleUnChanged(newvalue,oldvalue: pointer): boolean;

    function DoubleExact(newvalue,oldvalue: pointer): boolean;
    function DoubleBetween(newvalue,oldvalue: pointer): boolean;
    function DoubleBiggerThan(newvalue,oldvalue: pointer): boolean;
    function DoubleSmallerThan(newvalue,oldvalue: pointer): boolean;
    function DoubleIncreasedValue(newvalue,oldvalue: pointer): boolean;
    function DoubleIncreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function DoubleDecreasedValue(newvalue,oldvalue: pointer): boolean;
    function DoubleDecreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function DoubleChanged(newvalue,oldvalue: pointer): boolean;
    function DoubleUnChanged(newvalue,oldvalue: pointer): boolean;

    function AllExact(newvalue,oldvalue: pointer):boolean; //check byte,word,dword,qword,single and float
    function AllBetween(newvalue,oldvalue: pointer): boolean;
    function AllBiggerThan(newvalue,oldvalue: pointer): boolean;
    function AllSmallerThan(newvalue,oldvalue: pointer): boolean;
    function AllIncreasedValue(newvalue,oldvalue: pointer): boolean;
    function AllIncreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function AllDecreasedValue(newvalue,oldvalue: pointer): boolean;
    function AllDecreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function AllChanged(newvalue,oldvalue: pointer): boolean;
    function AllUnchanged(newvalue,oldvalue: pointer): boolean;

    //following types only have exact: Array of byte, binary and string
    function ArrayOfByteExact(newvalue,oldvalue: pointer):boolean;
    function BinaryExact(newvalue,oldvalue: pointer):boolean;
    function CaseSensitiveAnsiStringExact(newvalue,oldvalue: pointer):boolean;
    function CaseInsensitiveAnsiStringExact(newvalue,oldvalue: pointer):boolean;
    function CaseSensitiveUnicodeStringExact(newvalue,oldvalue: pointer):boolean;
    function CaseInsensitiveUnicodeStringExact(newvalue,oldvalue: pointer):boolean;



    //save macthing address routines:
    procedure GenericSaveResult(address: dword; oldvalue: pointer); //only use as last resort : Call to copymemory just to store one entry
    procedure allSaveResult(address: dword; oldvalue: pointer);
    procedure binarySaveResult(address: dword; oldvalue: pointer);
    procedure arrayOfByteSaveResult(address: dword; oldvalue: pointer);
    procedure ByteSaveResult(address: dword; oldvalue: pointer);
    procedure WordSaveResult(address: dword; oldvalue: pointer);
    procedure DWordSaveResult(address: dword; oldvalue: pointer);    
    procedure QWordSaveResult(address: dword; oldvalue: pointer);
    procedure SingleSaveResult(address: dword; oldvalue: pointer);
    procedure DoubleSaveResult(address: dword; oldvalue: pointer);    

    //flush routines
    procedure genericFlush; //generic routine for flushing the buffer
    procedure stringFlush; //don't save the memory
    procedure binaryFlush; //don't save memory AND use foundaddressb for results
    procedure allFlush;


    procedure configurescanroutine; //parses scanvalue1,2 according to the VariableType and scanoptions
    procedure FirstScanmem(base:dword; buffer: pointer; size: integer); //routine that gets a buffer and saves all the results it finds in the buffer (For firstscan)
    procedure FirstNextScanmem(base:dword; buffer,oldbuffer: pointer; size: integer);

    procedure nextnextscanmemall(addresslist: pointer; oldmemory: pointer; chunksize: integer);
    procedure nextnextscanmembinary(addresslist: pointer; chunksize: integer);
    procedure nextnextscanmem(addresslist: pointer; oldmemory: pointer; chunksize: integer);

    procedure firstscan; //copy the given range to the memory region
    procedure firstnextscan; //routine used when the results list contains nothing but a indicator a unknown scan was done
    procedure nextnextscan; //routine used when the results list contains addresses

  public
    OwningScanController: TScanController;
    Addressfile: TFilestream; //CheatEngineDir+'Addresses'+ThreadID.TMP'
    MemoryFile: TFileStream;  //CheatEngineDir+'Memory'+ThreadID.TMP'
    Addressfilename: string;
    MemoryFilename: string;

    roundingtype: TRoundingtype;
    hexadecimal: boolean;
    binaryStringAsDecimal: boolean;
    readonly: boolean;
    fastscan: boolean;
    unicode: boolean;
    caseSensitive: boolean;
    fastscanalignsize: integer;
    variablesize: integer;
    scanvalue1,scanvalue2: string;
    widescanvalue1: widestring;
    scanOption: TScanOption;
    variableType: TVariableType;
    scanType: TScanType; //defines if it's a firstscan or next scan. (newscan is ignored)
    useNextNextscan: boolean; //determines to use the nextNextScan or firstNextScan

    customscanscript: tstrings; //holds the info for the custom type, script for AA, script for CScript, 2 lines for dll: dllname+function
    customscantype: TCustomScanType;
    customscanAllocArray: TCEAllocArray;




    //thread controlling variables:
    isdone: boolean; //will get set to true when the thread finishes normally
    haserror: boolean;
    errorstring: string;

    //region related scans:
    //startregion and stopregion
    _startregion: integer;
    _stopregion: integer;
    maxregionsize: dword; //max size of buffer to be allocated when not unknown scan

    //recreated memory region list for this specific range, can be used to see which regions where only half read
    memRegions: TMemoryregions;
    memRegionPos: integer;

    startaddress: dword; //specific start for this this thread
    stopaddress: dword; //specific stop for this thread, if not fastscan and another thread continue from here, may add some overlopping bytes

    //exact address scans:
    startentry: integer; //index in the address list
    stopentry: integer; //"   "

    //general:
    scanned: dword; //total memory/addresses scanned by this routine
    totalfound: dword;

    scannernr: integer;

    procedure execute; override;
    constructor create(suspended: boolean);
    destructor destroy; override;
  end;

  TScanController=class(tthread)
  {
    The ScanController will configure the scanners and wait till they are done, mainly a idle thread
  }
  private
    threadcount: integer;
    addressFile: TFileStream;
    memoryFile: TFileStream;
    savescannerresults: boolean; //tells the epilogue to save the results to addressfile and memoryfile

    procedure updategui;
    procedure errorpopup;
    procedure firstScan;
    procedure nextScan;
    procedure firstnextscan; //rotine used when the results list contains nothing but a indicator a unknown scan was done
    procedure nextnextscan; //routine used when the results list contains addresses
    procedure fillVariableAndFastScanAlignSize; //fills in the variablesize and fastscanalignsizevariables, used for firstscan and firstnextscan
  public
    OwningMemScan: TMemScan;
    
    resultsaveCS: tcriticalsection; //critical section the scanfilewriter objects use to determine if there is another scanthread writing
    scannersCS: TCriticalSection; //Critican section used aty the end of the scancontroller thread, when the scanners are being destroyed
    scanners: array of tscanner;

    totalAddresses: dword; //how many addresses it will have to scan

    roundingtype: TRoundingType;
    hexadecimal: boolean;
    binaryStringAsDecimal: boolean;
    readonly: boolean;
    fastscan: boolean;
    unicode: boolean;
    casesensitive: boolean;
    fastscanalignsize: integer;
    variablesize: integer;
    scanvalue1,scanvalue2: string;
    startaddress: dword; //start for the whole scan
    stopaddress: dword; //stop of the whole scan
    scanOption: TScanOption;
    variableType: TVariableType;
    scanType: TScanType; //defines if it's a firstscan or next scan. (newscan is ignored)

    customscanscript: tstrings; //holds the info for the custom type, script for AA, script for CScript, 2 lines for dll: dllname+function
    customscantype: TCustomScanType;    

    //memregion info
    memregion: TMemoryregions;  //scanners have access to this, but make sure to NOT WRITE it
    memRegionPos: Integer;



    //thread controlling variables:
    isdone: boolean; //will get set to true when the thread finishes normally
    haserror: boolean;
    errorstring: string;

    //messages
    notifywindow: thandle;
    notifymessage: integer;
    procedure execute; override;
    constructor create(suspended: boolean);
    destructor destroy; override;
  end;

  TMemScan=class
  {
    Configures the gui and related objects and launch TScanner objects with those objects
  }
  private
    previousMemoryBuffer: pointer;
    scanController: TScanController; //thread that configures the scanner threads and wait till they are done
    SaveFirstScanThread: TSaveFirstScanThread; //thread that will save the results of the first scan that can be used for "same as first scan" scans
    memRegion: TMemoryRegions;  //after a scan the contents of controller gets copied to here
    memRegionPos: integer;

    progressbar: TProgressBar;
    notifywindow: thandle;
    notifymessage: integer;

    currentVariableType: TVariableType;
    found: uint64;

    //string stuff:
    stringUnicode: boolean;
    stringLength:  integer;

    //binary stuff:
    binaryLength:  integer;

    //array stuff:
    arrayLength:   integer;


    FLastScanType: TScanType;
  public
    function GetProgress(var totaladdressestoscan:dword; var currentlyscanned: dword):integer;
    function GetErrorString: string;
    function GetFoundCount: uint64;
    function Getbinarysize: int64; //returns the number of bits of the current type
    procedure TerminateScan;
    procedure newscan; //will clean up the memory and files
    procedure firstscan(scanOption: TScanOption; VariableType: TVariableType; roundingtype: TRoundingType; scanvalue1, scanvalue2: string; startaddress,stopaddress: dword; fastscan,readonly,hexadecimal,binaryStringAsDecimal,unicode,casesensitive: boolean; customscanscript: tstrings; customscantype: TCustomScanType); //first scan routine, e.g unknown initial value, or exact scan
    procedure NextScan(scanOption: TScanOption; roundingtype: TRoundingType; scanvalue1, scanvalue2: string; startaddress,stopaddress: dword; fastscan,readonly,hexadecimal,binaryStringAsDecimal,unicode,casesensitive: boolean; customscanscript: tstrings; customscantype: TCustomScanType); //next scan, determine what kind of scan and give to firstnextscan/nextnextscan

    constructor create(progressbar: TProgressbar;notifywindow: thandle; notifymessage: integer);
    destructor destroy; override;

    property LastScanType: TScanType read FLastScanType;
  end;

implementation

uses StrUtils;

var foundaddress: byte;
    foundaddressb: byte;


//===============Local functions================//
function getBytecountArrayOfByteString(st: string): integer;
var bytes: tbytes;
begin
  ConvertStringToBytes(st,false,bytes);
  result:=length(bytes);
end;


function getBytecountBinaryString(st:string; scanvalueisdecimal: boolean): integer;
var value: dword;
    i: integer;
begin
  if scanvalueisdecimal then //first convert do binarystring
    st:=inttobin(strtoint(st));


  result:=0;
  for i:=1 to length(st) do
  begin
    case st[i] of
      '0','1','?','*': inc(result);
      ' ',#8: ; //ignore
      else raise exception.Create(st[i]+' is not a valid character inside a binary string');
    end;
  end;

  if result=0 then raise exception.Create('Invalid binary notation');
  if (result mod 8=0) then
    result:=1+result div 8
  else
    result:=2+(result div 8);

end;



//==================TScanner===================//


//-----------=====Scanner check routines=====--------------//

function TScanner.AllExact(newvalue,oldvalue: pointer):boolean;
var i: TVariableType;
begin
  typesmatch[vtByte]:=typesmatch[vtByte] and ByteExact(newvalue,oldvalue); //oldvalue=nil, but give it anyhow
  typesmatch[vtWord]:=typesmatch[vtWord] and WordExact(newvalue,oldvalue);
  typesmatch[vtDword]:=typesmatch[vtDword] and DwordExact(newvalue,oldvalue);
  typesmatch[vtQword]:=typesmatch[vtQword] and qwordExact(newvalue,oldvalue);
  typesmatch[vtSingle]:=typesmatch[vtSingle] and singleExact(newvalue,oldvalue);
  typesmatch[vtDouble]:=typesmatch[vtDouble] and doubleExact(newvalue,oldvalue);

  result:=false;
  for i:=vtbyte to vtdouble do
    if typesmatch[i] then
    begin
      result:=true;
      exit;
    end;
end;

function TScanner.AllBetween(newvalue,oldvalue: pointer):boolean;
var i: TVariableType;
begin
  typesmatch[vtByte]:=typesmatch[vtByte] and ByteBetween(newvalue,oldvalue);
  typesmatch[vtWord]:=typesmatch[vtWord] and WordBetween(newvalue,oldvalue);
  typesmatch[vtDword]:=typesmatch[vtDword] and DwordBetween(newvalue,oldvalue);
  typesmatch[vtQword]:=typesmatch[vtQword] and qwordBetween(newvalue,oldvalue);
  typesmatch[vtSingle]:=typesmatch[vtSingle] and singleBetween(newvalue,oldvalue);
  typesmatch[vtDouble]:=typesmatch[vtDouble] and doubleBetween(newvalue,oldvalue);

  result:=false;
  for i:=vtbyte to vtdouble do
    if typesmatch[i] then
    begin
      result:=true;
      exit;
    end;
end;

function TScanner.AllBiggerThan(newvalue,oldvalue: pointer):boolean;
var i: TVariableType;
begin
  typesmatch[vtByte]:=typesmatch[vtByte] and ByteBiggerThan(newvalue,oldvalue);
  typesmatch[vtWord]:=typesmatch[vtWord] and WordBiggerThan(newvalue,oldvalue);
  typesmatch[vtDword]:=typesmatch[vtDword] and DwordBiggerThan(newvalue,oldvalue);
  typesmatch[vtQword]:=typesmatch[vtQword] and qwordBiggerThan(newvalue,oldvalue);
  typesmatch[vtSingle]:=typesmatch[vtSingle] and singleBiggerThan(newvalue,oldvalue);
  typesmatch[vtDouble]:=typesmatch[vtDouble] and doubleBiggerThan(newvalue,oldvalue);

  result:=false;
  for i:=vtbyte to vtdouble do
    if typesmatch[i] then
    begin
      result:=true;
      exit;
    end;
end;

function TScanner.AllSmallerThan(newvalue,oldvalue: pointer):boolean;
var i: TVariableType;
begin
  typesmatch[vtByte]:=typesmatch[vtByte] and ByteSmallerThan(newvalue,oldvalue);
  typesmatch[vtWord]:=typesmatch[vtWord] and WordSmallerThan(newvalue,oldvalue);
  typesmatch[vtDword]:=typesmatch[vtDword] and DwordSmallerThan(newvalue,oldvalue);
  typesmatch[vtQword]:=typesmatch[vtQword] and qwordSmallerThan(newvalue,oldvalue);
  typesmatch[vtSingle]:=typesmatch[vtSingle] and singleSmallerThan(newvalue,oldvalue);
  typesmatch[vtDouble]:=typesmatch[vtDouble] and doubleSmallerThan(newvalue,oldvalue);

  result:=false;
  for i:=vtbyte to vtdouble do
    if typesmatch[i] then
    begin
      result:=true;
      exit;
    end;
end;

function TScanner.AllIncreasedValue(newvalue,oldvalue: pointer):boolean;
var i: TVariableType;
begin
  typesmatch[vtByte]:=typesmatch[vtByte] and ByteIncreasedValue(newvalue,oldvalue);
  typesmatch[vtWord]:=typesmatch[vtWord] and WordIncreasedValue(newvalue,oldvalue);
  typesmatch[vtDword]:=typesmatch[vtDword] and DwordIncreasedValue(newvalue,oldvalue);
  typesmatch[vtQword]:=typesmatch[vtQword] and qwordIncreasedValue(newvalue,oldvalue);
  typesmatch[vtSingle]:=typesmatch[vtSingle] and singleIncreasedValue(newvalue,oldvalue);
  typesmatch[vtDouble]:=typesmatch[vtDouble] and doubleIncreasedValue(newvalue,oldvalue);

  result:=false;
  for i:=vtbyte to vtdouble do
    if typesmatch[i] then
    begin
      result:=true;
      exit;
    end;
end;

function TScanner.AllIncreasedValueBy(newvalue,oldvalue: pointer):boolean;
var i: TVariableType;
begin
  typesmatch[vtByte]:=typesmatch[vtByte] and ByteIncreasedValueBy(newvalue,oldvalue);
  typesmatch[vtWord]:=typesmatch[vtWord] and WordIncreasedValueBy(newvalue,oldvalue);
  typesmatch[vtDword]:=typesmatch[vtDword] and DwordIncreasedValueBy(newvalue,oldvalue);
  typesmatch[vtQword]:=typesmatch[vtQword] and qwordIncreasedValueBy(newvalue,oldvalue);
  typesmatch[vtSingle]:=typesmatch[vtSingle] and singleIncreasedValueBy(newvalue,oldvalue);
  typesmatch[vtDouble]:=typesmatch[vtDouble] and doubleIncreasedValueBy(newvalue,oldvalue);

  result:=false;
  for i:=vtbyte to vtdouble do
    if typesmatch[i] then
    begin
      result:=true;
      exit;
    end;
end;

function TScanner.AllDecreasedValue(newvalue,oldvalue: pointer):boolean;
var i: TVariableType;
begin
  typesmatch[vtByte]:=typesmatch[vtByte] and ByteDecreasedValue(newvalue,oldvalue);
  typesmatch[vtWord]:=typesmatch[vtWord] and WordDecreasedValue(newvalue,oldvalue);
  typesmatch[vtDword]:=typesmatch[vtDword] and DwordDecreasedValue(newvalue,oldvalue);
  typesmatch[vtQword]:=typesmatch[vtQword] and qwordDecreasedValue(newvalue,oldvalue);
  typesmatch[vtSingle]:=typesmatch[vtSingle] and singleDecreasedValue(newvalue,oldvalue);
  typesmatch[vtDouble]:=typesmatch[vtDouble] and doubleDecreasedValue(newvalue,oldvalue);

  result:=false;
  for i:=vtbyte to vtdouble do
    if typesmatch[i] then
    begin
      result:=true;
      exit;
    end;
end;

function TScanner.AllDecreasedValueBy(newvalue,oldvalue: pointer):boolean;
var i: TVariableType;
begin
  typesmatch[vtByte]:=typesmatch[vtByte] and ByteDecreasedValueBy(newvalue,oldvalue);
  typesmatch[vtWord]:=typesmatch[vtWord] and WordDecreasedValueBy(newvalue,oldvalue);
  typesmatch[vtDword]:=typesmatch[vtDword] and DwordDecreasedValueBy(newvalue,oldvalue);
  typesmatch[vtQword]:=typesmatch[vtQword] and qwordDecreasedValueBy(newvalue,oldvalue);
  typesmatch[vtSingle]:=typesmatch[vtSingle] and singleDecreasedValueBy(newvalue,oldvalue);
  typesmatch[vtDouble]:=typesmatch[vtDouble] and doubleDecreasedValueBy(newvalue,oldvalue);

  result:=false;
  for i:=vtbyte to vtdouble do
    if typesmatch[i] then
    begin
      result:=true;
      exit;
    end;
end;

function TScanner.Allchanged(newvalue,oldvalue: pointer):boolean;
var i: TVariableType;
begin
  typesmatch[vtByte]:=typesmatch[vtByte] and ByteChanged(newvalue,oldvalue);
  typesmatch[vtWord]:=typesmatch[vtWord] and WordChanged(newvalue,oldvalue);
  typesmatch[vtDword]:=typesmatch[vtDword] and DwordChanged(newvalue,oldvalue);
  typesmatch[vtQword]:=typesmatch[vtQword] and qwordChanged(newvalue,oldvalue);
  typesmatch[vtSingle]:=typesmatch[vtSingle] and singleChanged(newvalue,oldvalue);
  typesmatch[vtDouble]:=typesmatch[vtDouble] and doubleChanged(newvalue,oldvalue);

  result:=false;
  for i:=vtbyte to vtdouble do
    if typesmatch[i] then
    begin
      result:=true;
      exit;
    end;
end;

function TScanner.AllUnchanged(newvalue,oldvalue: pointer):boolean;
var i: TVariableType;
begin
  typesmatch[vtByte]:=typesmatch[vtByte] and ByteUnchanged(newvalue,oldvalue);
  typesmatch[vtWord]:=typesmatch[vtWord] and WordUnchanged(newvalue,oldvalue);
  typesmatch[vtDword]:=typesmatch[vtDword] and DwordUnchanged(newvalue,oldvalue);
  typesmatch[vtQword]:=typesmatch[vtQword] and qwordUnchanged(newvalue,oldvalue);
  typesmatch[vtSingle]:=typesmatch[vtSingle] and singleUnchanged(newvalue,oldvalue);
  typesmatch[vtDouble]:=typesmatch[vtDouble] and doubleUnchanged(newvalue,oldvalue);

  result:=false;
  for i:=vtbyte to vtdouble do
    if typesmatch[i] then
    begin
      result:=true;
      exit;
    end;
end;

function TScanner.CaseSensitiveAnsiStringExact(newvalue,oldvalue: pointer):boolean;
var i: integer;
begin
  result:=false;
  for i:=1 to length(scanvalue1) do
    if scanvalue1[i]<>(pchar(newvalue)[i-1]) then exit;

  result:=true;
end;

function TScanner.CaseInsensitiveAnsiStringExact(newvalue,oldvalue: pointer):boolean;
var i: integer;
begin
  //scanvalue1 has already been converted to uppercase in config
  result:=false;
  for i:=1 to length(scanvalue1) do
  begin
    if pchar(newvalue)[(i-1)] in ['a'..'z'] then //change to uppercase
      dec(pbytearray(newvalue)[(i-1)],$20);

    if scanvalue1[i]<>(pchar(newvalue)[i-1]) then exit;
  end;

  result:=true; //it got here, so a match
end;

function TScanner.CaseSensitiveUnicodeStringExact(newvalue,oldvalue: pointer):boolean;
var i: integer;
begin
  result:=false;

  for i:=1 to length(scanvalue1) do
    if widescanvalue1[i]<>(pwidechar(newvalue)[i-1]) then exit;

  result:=true;
end;

function TScanner.CaseInsensitiveUnicodeStringExact(newvalue,oldvalue: pointer):boolean;
var i: integer;
    scanvaluellength: integer;
    pwidescanvalue: pchar;
begin
  result:=false;
  i:=0;
  scanvaluellength:=length(scanvalue1)*sizeof(widechar);
  pwidescanvalue:=@widescanvalue1[1];

  for i:=1 to length(scanvalue1) do
  begin
    if pchar(newvalue)[(i-1)*sizeof(wchar)] in ['a'..'z'] then //change to uppercase
      dec(pbytearray(newvalue)[(i-1)*sizeof(wchar)],$20);

    if widescanvalue1[i]<>(pwidechar(newvalue)[i-1]) then exit;
  end;

  result:=true; //it got here, so a match
end;


function TScanner.ArrayOfByteExact(newvalue,oldvalue: pointer):boolean;
var i: integer;
begin
  for i:=0 to abs_arraylength-1 do
    if (abs_arraytofind[i]<>-1) and (pbytearray(newvalue)[i]<>abs_arraytofind[i]) then
    begin
      result:=false; //no match
      exit;
    end;

  result:=true; //still here, so a match
end;

function TScanner.BinaryExact(newvalue,oldvalue: pointer):boolean;
var i: integer;
begin
  for i:=0 to 7 do
    binaryresults[i]:=((puint64(newvalue)^ shr i) and andmask)=bitmask;

  //no need for a result here, for binary that isn't checked (special case)
  result:=true; //let the store result routine deal with it
end;

//byte:
function TScanner.ByteExact(newvalue,oldvalue: pointer):boolean;
begin
  result:=pbyte(newvalue)^=byte(value);
end;

function TScanner.ByteBetween(newvalue,oldvalue: pointer):boolean;
begin
  result:=(pbyte(newvalue)^>=byte(value)) and (pbyte(newvalue)^<=byte(value2));
end;

function TScanner.ByteBiggerThan(newvalue,oldvalue: pointer):boolean;
begin
  result:=pbyte(newvalue)^>byte(value);
end;

function TScanner.ByteSmallerThan(newvalue,oldvalue: pointer):boolean;
begin
  result:=pbyte(newvalue)^<byte(value);
end;

function TScanner.ByteIncreasedValue(newvalue,oldvalue: pointer):boolean;
begin
  result:=pbyte(newvalue)^>pbyte(oldvalue)^;
end;

function TScanner.ByteIncreasedValueBy(newvalue,oldvalue: pointer):boolean;
begin
  result:=pbyte(newvalue)^=pbyte(oldvalue)^+byte(value);
end;

function TScanner.ByteDecreasedValue(newvalue,oldvalue: pointer):boolean;
begin
  result:=pbyte(newvalue)^<pbyte(oldvalue)^;
end;

function TScanner.ByteDecreasedValueBy(newvalue,oldvalue: pointer):boolean;
begin
  result:=pbyte(newvalue)^=pbyte(oldvalue)^-byte(value);
end;

function TScanner.ByteChanged(newvalue,oldvalue: pointer):boolean;
begin
  result:=pbyte(newvalue)^<>pbyte(oldvalue)^;
end;

function TScanner.ByteUnchanged(newvalue,oldvalue: pointer):boolean; //also used for same as first
begin
  result:=pbyte(newvalue)^=pbyte(oldvalue)^;
end;


//word:
function TScanner.WordExact(newvalue,oldvalue: pointer): boolean;
begin
  result:=pword(newvalue)^=word(value);
end;

function TScanner.WordBetween(newvalue,oldvalue: pointer):boolean;
begin
  result:=(pword(newvalue)^>=word(value)) and (pword(newvalue)^<=word(value2));
end;

function TScanner.WordBiggerThan(newvalue,oldvalue: pointer):boolean;
begin
  result:=pword(newvalue)^>word(value);
end;

function TScanner.WordSmallerThan(newvalue,oldvalue: pointer):boolean;
begin
  result:=pword(newvalue)^<word(value);
end;

function TScanner.WordIncreasedValue(newvalue,oldvalue: pointer):boolean;
begin
  result:=pword(newvalue)^>pword(oldvalue)^;
end;

function TScanner.WordIncreasedValueBy(newvalue,oldvalue: pointer):boolean;
begin
  result:=pword(newvalue)^=pword(oldvalue)^+word(value);
end;

function TScanner.WordDecreasedValue(newvalue,oldvalue: pointer):boolean;
begin
  result:=pword(newvalue)^<pword(oldvalue)^;
end;

function TScanner.WordDecreasedValueBy(newvalue,oldvalue: pointer):boolean;
begin
  result:=pword(newvalue)^=pword(oldvalue)^-word(value);
end;

function TScanner.WordChanged(newvalue,oldvalue: pointer):boolean;
begin
  result:=pword(newvalue)^<>pword(oldvalue)^;
end;

function TScanner.WordUnchanged(newvalue,oldvalue: pointer):boolean;
begin
  result:=pword(newvalue)^=pword(oldvalue)^;
end;


//dword:
function TScanner.DWordExact(newvalue,oldvalue: pointer): boolean;
begin
  result:=pdword(newvalue)^=dword(value);
end;

function TScanner.DWordBetween(newvalue,oldvalue: pointer):boolean;
begin
  result:=(pdword(newvalue)^>=dword(value)) and (pdword(newvalue)^<=dword(value2));
end;

function TScanner.DWordBiggerThan(newvalue,oldvalue: pointer):boolean;
begin
  result:=pdword(newvalue)^>dword(value);
end;

function TScanner.DWordSmallerThan(newvalue,oldvalue: pointer):boolean;
begin
  result:=pdword(newvalue)^<dword(value);
end;

function TScanner.DWordIncreasedValue(newvalue,oldvalue: pointer):boolean;
begin
  result:=pdword(newvalue)^>pdword(oldvalue)^;
end;

function TScanner.DWordIncreasedValueBy(newvalue,oldvalue: pointer):boolean;
begin
  result:=pdword(newvalue)^=pdword(oldvalue)^+dword(value);
end;

function TScanner.DWordDecreasedValue(newvalue,oldvalue: pointer):boolean;
begin
  result:=pdword(newvalue)^<pdword(oldvalue)^;
end;

function TScanner.DWordDecreasedValueBy(newvalue,oldvalue: pointer):boolean;
begin
  result:=pdword(newvalue)^=pdword(oldvalue)^-dword(value);
end;

function TScanner.DWordChanged(newvalue,oldvalue: pointer):boolean;
begin
  result:=pdword(newvalue)^<>pdword(oldvalue)^;
end;

function TScanner.DWordUnchanged(newvalue,oldvalue: pointer):boolean;
begin
  result:=pdword(newvalue)^=pdword(oldvalue)^;
end;


//int64
function TScanner.QWordExact(newvalue,oldvalue: pointer): boolean;
begin
  result:=puint64(newvalue)^=uint64(value);
end;

function TScanner.QWordBetween(newvalue,oldvalue: pointer):boolean;
begin
  result:=(puint64(newvalue)^>=uint64(value)) and (puint64(newvalue)^<=uint64(value2));
end;

function TScanner.QWordBiggerThan(newvalue,oldvalue: pointer):boolean;
begin
  result:=puint64(newvalue)^>uint64(value);
end;

function TScanner.QWordSmallerThan(newvalue,oldvalue: pointer):boolean;
begin
  result:=puint64(newvalue)^<uint64(value);
end;

function TScanner.QWordIncreasedValue(newvalue,oldvalue: pointer):boolean;
begin
  result:=puint64(newvalue)^>puint64(oldvalue)^;
end;

function TScanner.QWordIncreasedValueBy(newvalue,oldvalue: pointer):boolean;
begin
  result:=puint64(newvalue)^=puint64(oldvalue)^+value;
end;

function TScanner.QWordDecreasedValue(newvalue,oldvalue: pointer):boolean;
begin
  result:=puint64(newvalue)^<puint64(oldvalue)^;
end;

function TScanner.QWordDecreasedValueBy(newvalue,oldvalue: pointer):boolean;
begin
  result:=puint64(newvalue)^=puint64(oldvalue)^-value;
end;

function TScanner.QWordChanged(newvalue,oldvalue: pointer):boolean;
begin
  result:=puint64(newvalue)^<>puint64(oldvalue)^;
end;

function TScanner.QWordUnchanged(newvalue,oldvalue: pointer):boolean;
begin
  result:=puint64(newvalue)^=puint64(oldvalue)^;
end;


//single:

function TScanner.SingleExact(newvalue,oldvalue: pointer): boolean;
var min,max: single;
begin
  result:=false;
  case roundingtype of
    rtRounded:
      result:=(RoundTo(psingle(newvalue)^,-floataccuracy)=svalue);

    rtExtremerounded:
      result:=(psingle(newvalue)^>minsvalue) and (psingle(newvalue)^<maxsvalue);

    rtTruncated:
      result:=(psingle(newvalue)^>=svalue) and (psingle(newvalue)^<maxsvalue);
  end;

end;

function TScanner.SingleBetween(newvalue,oldvalue: pointer):boolean;
begin
  result:=(psingle(newvalue)^>=svalue) and (psingle(newvalue)^<=svalue2);
end;

function TScanner.SingleBiggerThan(newvalue,oldvalue: pointer):boolean;
begin
  result:=psingle(newvalue)^>svalue;
end;

function TScanner.SingleSmallerThan(newvalue,oldvalue: pointer):boolean;
begin
  result:=psingle(newvalue)^<svalue;
end;

function TScanner.SingleIncreasedValue(newvalue,oldvalue: pointer):boolean;
begin
  result:=psingle(newvalue)^>psingle(oldvalue)^;
end;

function TScanner.SingleIncreasedValueBy(newvalue,oldvalue: pointer):boolean;
begin
  result:=RoundTo(psingle(newvalue)^,-floataccuracy)=RoundTo(psingle(oldvalue)^+svalue,-floataccuracy);
end;

function TScanner.SingleDecreasedValue(newvalue,oldvalue: pointer):boolean;
begin
  result:=psingle(newvalue)^<psingle(oldvalue)^;
end;

function TScanner.SingleDecreasedValueBy(newvalue,oldvalue: pointer):boolean;
begin
  result:=RoundTo(psingle(newvalue)^,-floataccuracy)=RoundTo(psingle(oldvalue)^-svalue,-floataccuracy);
end;

function TScanner.SingleChanged(newvalue,oldvalue: pointer):boolean;
begin
  result:=pSingle(newvalue)^<>pSingle(oldvalue)^;
end;

function TScanner.SingleUnchanged(newvalue,oldvalue: pointer):boolean;
begin
  result:=pSingle(newvalue)^=pSingle(oldvalue)^;
end;

//double:
function TScanner.DoubleExact(newvalue,oldvalue: pointer): boolean;
begin
  result:=false;
  case roundingtype of
    rtRounded:
      result:=(RoundTo(pdouble(newvalue)^,-floataccuracy)=dvalue);

    rtExtremerounded:
      result:=(pdouble(newvalue)^>mindvalue) and (pdouble(newvalue)^<maxdvalue);

    rtTruncated:
      result:=(pdouble(newvalue)^>=dvalue) and (pdouble(newvalue)^<maxdvalue);
  end;
end;

function TScanner.DoubleBetween(newvalue,oldvalue: pointer):boolean;
begin
  result:=(pdouble(newvalue)^>=dvalue) and (pdouble(newvalue)^<=dvalue2);
end;

function TScanner.DoubleBiggerThan(newvalue,oldvalue: pointer):boolean;
begin
  result:=pdouble(newvalue)^>dvalue;
end;

function TScanner.DoubleSmallerThan(newvalue,oldvalue: pointer):boolean;
begin
  result:=pdouble(newvalue)^<dvalue;
end;

function TScanner.DoubleIncreasedValue(newvalue,oldvalue: pointer):boolean;
begin
  result:=pdouble(newvalue)^>pdouble(oldvalue)^;
end;

function TScanner.DoubleIncreasedValueBy(newvalue,oldvalue: pointer):boolean;
begin
  result:=RoundTo(pdouble(newvalue)^,-floataccuracy)=RoundTo(pdouble(oldvalue)^+svalue,-floataccuracy);
end;

function TScanner.DoubleDecreasedValue(newvalue,oldvalue: pointer):boolean;
begin
  result:=pdouble(newvalue)^<pdouble(oldvalue)^;
end;

function TScanner.DoubleDecreasedValueBy(newvalue,oldvalue: pointer):boolean;
begin
  result:=RoundTo(pdouble(newvalue)^,-floataccuracy)=RoundTo(pdouble(oldvalue)^-svalue,-floataccuracy);
end;

function TScanner.DoubleChanged(newvalue,oldvalue: pointer):boolean;
begin
  result:=pDouble(newvalue)^<>pDouble(oldvalue)^;
end;

function TScanner.DoubleUnchanged(newvalue,oldvalue: pointer):boolean;
begin
  result:=pDouble(newvalue)^=pDouble(oldvalue)^;
end;

//=============Save result routines===========//

procedure TScanner.GenericSaveResult(address: dword; oldvalue: pointer);
{
Generic routine for storing results. Use as last resort. E.g custom scans
}
begin
  //save varsize
  pdwordarray(CurrentAddressBuffer)[found]:=address;
  copyMemory(pointer(dword(CurrentFoundBuffer)+(variablesize*found)),oldvalue,variablesize);

  inc(found);
  if found>=buffersize then
    flushroutine;
end;

procedure TScanner.ByteSaveResult(address: dword; oldvalue: pointer);
begin
  //save address and current value
  pdwordarray(CurrentAddressBuffer)[found]:=address;
  pbytearray(CurrentFoundBuffer)[found]:=pbyte(oldvalue)^;

  inc(found);
  if found>=buffersize then
    flushroutine;
end;

procedure TScanner.WordSaveResult(address: dword; oldvalue: pointer);
begin
  pdwordarray(CurrentAddressBuffer)[found]:=address;
  pwordarray(CurrentFoundBuffer)[found]:=pword(oldvalue)^;

  inc(found);
  if found>=buffersize then
    flushroutine;
end;

procedure TScanner.DWordSaveResult(address: dword; oldvalue: pointer);
begin
  pdwordarray(CurrentAddressBuffer)[found]:=address;
  pdwordarray(CurrentFoundBuffer)[found]:=pdword(oldvalue)^;

  inc(found);
  if found>=buffersize then
    flushroutine;
end;

procedure TScanner.QWordSaveResult(address: dword; oldvalue: pointer);
begin
  pdwordarray(CurrentAddressBuffer)[found]:=address;
  puint64array(CurrentFoundBuffer)[found]:=puint64(oldvalue)^;

  inc(found);
  if found>=buffersize then
    flushroutine;
end;

procedure TScanner.SingleSaveResult(address: dword; oldvalue: pointer);
begin
  if not (isnan(psingle(oldvalue)^) or IsInfinite(psingle(oldvalue)^))  then
  begin
    pdwordarray(CurrentAddressBuffer)[found]:=address;
    psinglearray(CurrentFoundBuffer)[found]:=psingle(oldvalue)^;

    inc(found);
    if found>=buffersize then
      flushroutine;
  end;
end;

procedure TScanner.DoubleSaveResult(address: dword; oldvalue: pointer);
begin
  if not (isnan(pdouble(oldvalue)^) or IsInfinite(pdouble(oldvalue)^))  then
  begin
    pdwordarray(CurrentAddressBuffer)[found]:=address;
    pdoublearray(CurrentFoundBuffer)[found]:=pdouble(oldvalue)^;

    inc(found);
    if found>=buffersize then
      flushroutine;
  end;
end;


procedure TScanner.arrayOfByteSaveResult(address: dword; oldvalue: pointer);
begin
  pdwordarray(CurrentAddressBuffer)[found]:=address;
  inc(found);
  if found>=buffersize then
    flushroutine;  
end;

procedure TScanner.binarySaveResult(address: dword; oldvalue: pointer);
var i: integer;
begin
  for i:=0 to 7 do
  begin
    if binaryresults[i] then
    begin
      PBitAddressArray(CurrentAddressBuffer)[found].address:=address;
      PBitAddressArray(CurrentAddressBuffer)[found].bit:=i;
      inc(found);
      if found>=buffersize then
        flushroutine;
    end;
  end;

end;

procedure TScanner.allSaveResult(address: dword; oldvalue: pointer);
{
note: eventually replace bit with a binary representation of all types that match
BUT, don't forget to change the foundlisthelper to handle this (since it'd be
multiple addresses in one entry, which isn't handled right now... 
}
var i: TVariabletype;
begin
  for i:=vtByte to vtDouble do
  begin
    if typesmatch[i] then
    begin
      if (i=vtsingle) then
      begin
        //filter out NAN and INF
        if isnan(psingle(oldvalue)^) or IsInfinite(psingle(oldvalue)^) then
          continue; //skip, don't save
      end;

      if (i=vtdouble) then
      begin
        if isnan(pdouble(oldvalue)^) or IsInfinite(pdouble(oldvalue)^) then
          continue; //skip, don't save
      end;

      //using the bitaddressarray since it holds a address and a value big enough to hold all types
      PBitAddressArray(CurrentAddressBuffer)[found].address:=address;
      PBitAddressArray(CurrentAddressBuffer)[found].bit:=integer(i);

      //save the current address, max variablesize is 8 bytes, so just store that
      puint64array(CurrentFoundBuffer)[found]:=puint64(oldvalue)^;

      inc(found);
      if found>=buffersize then
        flushroutine;
    end;
  end;
end;


//=============Flush result routines===========//
procedure TScanner.genericFlush;
var tempaddress,tempfound: pointer;
begin
  scanwriter.writeresults(CurrentAddressBuffer,currentfoundbuffer,4*found,found*variablesize);
  tempaddress:=CurrentAddressBuffer;
  tempfound:=CurrentFoundBuffer;
  CurrentAddressBuffer:=SecondaryAddressBuffer;
  CurrentFoundBuffer:=SecondaryFoundBuffer;
  SecondaryAddressBuffer:=tempaddress;
  SecondaryFoundBuffer:=tempfound;

  inc(totalfound,found);
  found:=0;
end;

procedure TScanner.stringFlush;
begin
  AddressFile.WriteBuffer(CurrentAddressBuffer^,4*found);
  inc(totalfound,found);
  found:=0;
end;

procedure TScanner.binaryFlush;
begin
  AddressFile.WriteBuffer(CurrentAddressBuffer^,sizeof(TBitAddress)*found);
  inc(totalfound,found);
  found:=0;
end;

procedure TScanner.allFlush;
var tempaddress,tempfound: pointer;
begin
  scanwriter.writeresults(CurrentAddressBuffer,currentfoundbuffer,sizeof(tbitaddress)*found,found*8);
  tempaddress:=CurrentAddressBuffer;
  tempfound:=CurrentFoundBuffer;
  CurrentAddressBuffer:=SecondaryAddressBuffer;
  CurrentFoundBuffer:=SecondaryFoundBuffer;
  SecondaryAddressBuffer:=tempaddress;
  SecondaryFoundBuffer:=tempfound;

  inc(totalfound,found);
  found:=0;
end;

//==================Tscanfilewriter=================//
procedure Tscanfilewriter.execute;
begin
  repeat
    dataavailable.WaitFor(infinite);
    try
      if terminated then exit;



      scancontroller.resultsaveCS.Enter;
      try
        //note: Compressing before writing using TCompressStream=failure
        //on fast compression speed it still took longer to compess and write
        //than to only write the uncompressed buffer (no compress=2.8 secs,
        //compressed=7.1 sec)

//        cAddressFile.WriteBuffer(addressbuffer^,addressSize);
//        cMemoryFile.WriteBuffer(memorybuffer^,memorySize);

        AddressFile.WriteBuffer(addressbuffer^,addressSize);
        MemoryFile.WriteBuffer(memorybuffer^,memorySize);
      finally
        scancontroller.resultsaveCS.Leave;
      end;

    finally
      datawritten.SetEvent; //tell the others that you're ready to write again
    end;

  until terminated;
end;

procedure Tscanfilewriter.writeresults(addressbuffer,memorybuffer: pointer; addressSize,memorySize: dword);
{
check if the thread is currently saving
If yes, wait, if not, start the thread, give it the buffer, and continue
}
begin
  datawritten.WaitFor(infinite); //only gets set when the thread is done writing
  //got past the wait, so it's done writing, so it has no need for the current variables
  self.addressbuffer:=addressbuffer;
  self.memorybuffer:=memorybuffer;
  self.addressSize:=addressSize;
  self.memorySize:=memorySize;

  dataavailable.SetEvent;  //tell the thread to start saving

  //and return to the scanner, who should now swap scanbuffers
end;

procedure Tscanfilewriter.flush;
begin
  datawritten.WaitFor(infinite);
  datawritten.SetEvent;
end;

destructor Tscanfilewriter.destroy;
begin
  if not terminated then
  begin
    terminate;
    dataavailable.SetEvent;
    waitfor;
  end;
  if datawritten<>nil then datawritten.Free;
  if dataavailable<>nil then dataavailable.Free;
{
  if cAddressFile<>nil then cAddressFile.free;
  if cMemoryFile<>nil then cMemoryFile.free;
}
  inherited destroy;
end;

constructor Tscanfilewriter.create(scancontroller:TScanController; addressfile,memoryfile:TFileStream);
begin
  self.scancontroller:=scancontroller;
  self.addressfile:=addressfile;
  self.memoryfile:=memoryfile;

  //create the events
  datawritten:=tevent.Create(nil,false,true,'');
  dataavailable:=tevent.create(nil,false,false,'');
   {
  cAddressFile:=TCompressionStream.Create(clNone,AddressFile);
  cMemoryFile:=TCompressionStream.Create(clNone,MemoryFile);
         }
  inherited create(false); //start it  
end;

//=============TScanner===========//


procedure TScanner.FirstScanmem(base:dword; buffer: pointer; size: integer);
{
Scan the given buffer
Excludes the previousvalue buffer
}
var stepsize: integer;
    lastmem: dword;
    p: pbyte;
    i: TVariableType;
    _fastscan: boolean;
    dividableby2: boolean;
    dividableby4: boolean;
begin
  _fastscan:=fastscan;
  if _fastscan then
    stepsize:=fastscanalignsize
  else
    stepsize:=1;

  p:=buffer;

  lastmem:=dword(p)+(size-variablesize); //optimizes compile to use reg if possible


  if variableType=vtAll then
  begin
    //reset typesmatch array for each check
    while dword(p)<=lastmem do
    begin
      if _fastscan then
      begin
        dividableby2:=dword(p) mod 2=0;
        dividableby4:=dword(p) mod 4=0;
        typesmatch[vtByte]:=true;
        typesmatch[vtWord]:=dividableby2;
        typesmatch[vtDWord]:=dividableby4;
        typesmatch[vtQWord]:=dividableby4;
        typesmatch[vtSingle]:=dividableby4;
        typesmatch[vtDouble]:=dividableby4;
      end
      else
        for i:=vtByte to vtDouble do typesmatch[i]:=true;

      if checkroutine(p,nil) then //found one
        StoreResultRoutine(base+dword(p)-dword(buffer),p);

      inc(p,stepsize);
    end;    
  end
  else
  begin
    while dword(p)<=lastmem do
    begin
      if checkroutine(p,nil) then //found one
        StoreResultRoutine(base+dword(p)-dword(buffer),p);

      inc(p,stepsize);
    end;
  end;
end;

procedure TScanner.FirstNextScanmem(base:dword; buffer,oldbuffer: pointer; size: integer);
{
Scan the given buffer
}
var stepsize:  integer;
    lastmem:   dword;
    p,oldp:    pbyte;
    valuetype: TValueType;
    _fastscan: boolean;
    dividableby2: boolean;
    dividableby4: boolean;
    i:         TVariableType;   
begin
  _fastscan:=fastscan;
  if _fastscan then
    stepsize:=fastscanalignsize
  else
    stepsize:=1;

  p:=buffer;
  oldp:=oldbuffer;

  lastmem:=dword(p)+(size-variablesize); //optimizes compile to use reg if possible

  if scanOption=soSameAsFirst then //stupid, but ok...
  begin
    case self.variableType of
      vtByte:   valuetype:=vt_byte;
      vtWord:   valuetype:=vt_word;
      vtDWord:  valuetype:=vt_dword;
      vtdouble: valuetype:=vt_double;
      vtQword:  valuetype:=vt_int64;
      vtAll:    valuetype:=vt_all;
    end;

    if valuetype=vt_all then
    begin
      while dword(p)<=lastmem do
      begin
        if _fastscan then
        begin
          dividableby2:=dword(p) mod 2=0;
          dividableby4:=dword(p) mod 4=0;
          typesmatch[vtByte]:=true;
          typesmatch[vtWord]:=dividableby2;
          typesmatch[vtDWord]:=dividableby4;
          typesmatch[vtQWord]:=dividableby4;
          typesmatch[vtSingle]:=dividableby4;
          typesmatch[vtDouble]:=dividableby4;
        end
        else
          for i:=vtByte to vtDouble do typesmatch[i]:=true;

        if checkroutine(p,firstscanhandler.getpointertoaddress(base+dword(p)-dword(buffer),valuetype )) then //found one
          StoreResultRoutine(base+dword(p)-dword(buffer),p);

        inc(p, stepsize);
      end;
    end
    else
    begin
      while dword(p)<=lastmem do
      begin
        if checkroutine(p,firstscanhandler.getpointertoaddress(base+dword(p)-dword(buffer),valuetype )) then //found one
          StoreResultRoutine(base+dword(p)-dword(buffer),p);

        inc(p, stepsize);
      end;
    end;
  end
  else
  begin
    if variableType=vtall then
    begin
      while dword(p)<=lastmem do
      begin
        if _fastscan then
        begin
          dividableby2:=dword(p) mod 2=0;
          dividableby4:=dword(p) mod 4=0;
          typesmatch[vtByte]:=true;
          typesmatch[vtWord]:=dividableby2;
          typesmatch[vtDWord]:=dividableby4;
          typesmatch[vtQWord]:=dividableby4;
          typesmatch[vtSingle]:=dividableby4;
          typesmatch[vtDouble]:=dividableby4;
        end
        else
          for i:=vtByte to vtDouble do typesmatch[i]:=true;

        if checkroutine(p,oldp) then //found one
          StoreResultRoutine(base+dword(p)-dword(buffer),p);

        inc(p, stepsize);
        inc(oldp, stepsize);
      end;
    end
    else
    begin
      while dword(p)<=lastmem do
      begin
        if checkroutine(p,oldp) then //found one
          StoreResultRoutine(base+dword(p)-dword(buffer),p);

        inc(p, stepsize);
        inc(oldp, stepsize);
      end;
    end;
  end;
end;

procedure TScanner.nextnextscanmemAll(addresslist: pointer; oldmemory: pointer; chunksize: integer);
var stepsize: dword;
    i,j,k: dword;
    l: TVariableType;
    maxindex: dword;
    vsize: dword;
    currentbase: dword;
    newmemory: array [0..4095] of byte;
    oldmem: pbytearray;
    alist: pbitaddressarray;
    actualread: dword;

    so: Tscanoption;
    valuetype: TValuetype;
    currentaddress: dword;
begin
  i:=0;
  so:=scanoption;
  maxindex:=chunksize;
  vsize:=variablesize; //=8
  oldmem:=oldmemory;
  alist:=addresslist;
  valuetype:=vt_all;

  while i<maxindex do
  begin
    j:=i+1;
    currentbase:=alist[i].address and $FFFFF000;
    while j<=maxindex do
    begin


        
      if (currentbase)=((alist[j].address+vsize-1) and $fffff000) then //same page
        inc(j)
      else
      begin
        dec(j);  //now points to the last valid one, or the first one
        break;
      end;
    end;

    currentbase:=alist[i].address;
    if readprocessmemory(processhandle,pointer(currentbase),@newmemory[0],(alist[j].address-currentbase)+vsize,actualread) then
    begin
      if so=soSameAsFirst then
      begin
        //clear typesmatch and set current address
        for l:=vtByte to vtDouble do
          typesmatch[l]:=false;
          
        currentaddress:=currentbase;

        for k:=i to j do
        begin
          if alist[k].address=currentaddress then
          begin
            typesmatch[tvariabletype(alist[k].bit)]:=true;
          end
          else
          begin
            if checkroutine(@newmemory[currentaddress-currentbase],firstscanhandler.getpointertoaddress(currentaddress,valuetype )) then
              StoreResultRoutine(currentaddress,@newmemory[currentaddress-currentbase]);

            //clear typesmatch and set current address
            for l:=vtByte to vtDouble do
              typesmatch[l]:=false;

            currentaddress:=alist[k].address;
            typesmatch[tvariabletype(alist[k].bit)]:=true;
          end;

        end;

        if checkroutine(@newmemory[currentaddress-currentbase],firstscanhandler.getpointertoaddress(currentaddress,valuetype )) then
          StoreResultRoutine(currentaddress,@newmemory[currentaddress-currentbase]);

      end
      else
      begin
        //clear typesmatch and set current address
        for l:=vtByte to vtDouble do
          typesmatch[l]:=false;
          
        currentaddress:=currentbase;

        for k:=i to j do
        begin
          if alist[k].address=currentaddress then
          begin
            //add this one to the list
            typesmatch[tvariabletype(alist[k].bit)]:=true;
            continue;
          end
          else
          begin
            //we now have a list of entries with all the same address, k-1 points to the last one
            if CheckRoutine(@newmemory[currentaddress-currentbase],@oldmem[(k-1)*vsize]) then
              StoreResultRoutine(currentaddress,@newmemory[currentaddress-currentbase]);

            //clear typesmatch and set current address
            for l:=vtByte to vtDouble do
              typesmatch[l]:=false;

            currentaddress:=alist[k].address;

            typesmatch[tvariabletype(alist[k].bit)]:=true;
          end;

        end;

        if CheckRoutine(@newmemory[currentaddress-currentbase],@oldmem[j*vsize]) then
          StoreResultRoutine(currentaddress,@newmemory[currentaddress-currentbase]);
      end;
    end;

    i:=j+1;
  end;
end;


procedure TScanner.nextnextscanmembinary(addresslist: pointer; chunksize: integer);
var stepsize: dword;
    i,j,k,l: dword;
    maxindex: dword;
    vsize: dword;
    currentbase: dword;
    newmemory: array [0..4095] of byte;
    oldmem: pbytearray;
    alist: PBitAddressArray;
    actualread: dword;
    lastaddress: dword;
    scannedbitlist: array [0..7] of boolean;
begin
  i:=0;
  maxindex:=chunksize;
  vsize:=variablesize;
  alist:=addresslist;
  currentbase:=0;

  while i<maxindex do
  begin
    j:=i+1;
              
    currentbase:=alist[i].address and $FFFFF000;
    while j<=maxindex do
    begin
      if (currentbase)=((alist[j].address+vsize-1) and $fffff000) then //same page
        inc(j)
      else
      begin
        dec(j);  //now points to the last valid one, or the first one
        break;
      end;
    end;



    currentbase:=alist[i].address;
    if readprocessmemory(processhandle,pointer(currentbase),@newmemory[0],(alist[j].address-currentbase)+vsize,actualread) then
    begin
      k:=i;
      while k<=j do
      begin
        if CheckRoutine(@newmemory[alist[k].address-currentbase],nil) then
        begin
          for l:=0 to 7 do
            scannedbitlist[l]:=false;

          lastaddress:=alist[k].address;

          while alist[k].address=lastaddress do
          begin
            //add bits to the scanned bitlist
            scannedbitlist[alist[k].bit]:=true;
            inc(k);
          end;
          dec(k);

          for l:=0 to 7 do
            if not scannedbitlist[l] then binaryresults[l]:=false; //if it wasn't scanned, but the result was true, then set it to false

          StoreResultRoutine(alist[k].address,@newmemory[alist[k].address-currentbase]);
        end;
        inc(k); //next
      end;
    end;

    i:=j+1;
  end;
end;

procedure TScanner.nextnextscanmem(addresslist: pointer; oldmemory: pointer; chunksize: integer);
{
gather addresses in a 4K region and readprocessmemory the difference between (so if only 2 addresses in a 4K block and address 1 is at 0 and address 2 at 2k, then only read 2k)
note: Do a test to see how many cpu cycles it costs to read 4K and 4bytes and find out the minimum number for mos toptimal speed
done: 4 byte scan takes an average of 4000 cycles (due to caches etc...)
   4096 byte scan takes an average of 6500 cycles ...
   2048 byte scan takes an average of 5900 cycles ...

conclusion: only when there is 1 address in the list, scan 1 address, else scan more

}

var stepsize: dword;
    i,j,k: dword;
    maxindex: dword;
    vsize: dword;
    currentbase: dword;
    newmemory: array [0..4095] of byte;
    oldmem: pbytearray;
    alist: pdwordarray;
    actualread: dword;

    so: Tscanoption;
    valuetype: TValuetype;
    currentaddress: dword;
begin
  i:=0;
  so:=scanoption;
  maxindex:=chunksize;
  vsize:=variablesize;
  oldmem:=oldmemory;
  alist:=addresslist;

  case variableType of
    vtByte:   valuetype:=vt_byte;
    vtWord:   valuetype:=vt_word;
    vtDWord:  valuetype:=vt_dword;
    vtdouble: valuetype:=vt_double;
    vtQword:  valuetype:=vt_int64;
    vtAll:    valuetype:=vt_all;
    vtCustom:
    begin
      case vsize of
        1: valuetype:=vt_byte;
        2: valuetype:=vt_word;
        4: valuetype:=vt_dword;
        8: valuetype:=vt_int64;
        else valuetype:=vt_dword;
      end;

    end;
  end;


  while i<maxindex do
  begin
    j:=i+1;
    currentbase:=alist[i] and $FFFFF000;
    while j<=maxindex do
    begin
      if (currentbase)=((alist[j]+vsize-1) and $fffff000) then //same page
        inc(j)
      else
      begin
        dec(j);  //now points to the last valid one, or the first one
        break;
      end;
    end;


    currentbase:=alist[i];
    if readprocessmemory(processhandle,pointer(currentbase),@newmemory[0],(alist[j]-currentbase)+vsize,actualread) then
    begin
      if so=soSameAsFirst then
      begin
        for k:=i to j do
          if checkroutine(@newmemory[alist[k]-currentbase],firstscanhandler.getpointertoaddress(alist[k],valuetype )) then
            StoreResultRoutine(alist[k],@newmemory[alist[k]-currentbase])
      end
      else
      begin
        for k:=i to j do
          if CheckRoutine(@newmemory[alist[k]-currentbase],@oldmem[k*vsize]) then
            StoreResultRoutine(alist[k],@newmemory[alist[k]-currentbase]);
      end;
    end;

    i:=j+1;
  end;
end;

procedure TScanner.configurescanroutine;
{
Parse scanvalue1 and scanvalue2 accordingly to the scanoptions and type
and fill in class variables that will be used for the scans
}
var FloatSettings: TFormatSettings;
    BinaryString,andmask,bitmask: string;
    i: integer;
    b: Tbytes;
    foundbuffersize: integer;
    p: pointer;
begin
  //fill FloatSettings with formatting data (e.g difference between , and . for decimal)
  GetLocaleFormatSettings(GetThreadLocale, FloatSettings);


  if scanOption in [soCustom, soExactValue,soValueBetween,soBiggerThan,soSmallerThan, soDecreasedValueBy, soIncreasedValueBy] then
  begin
    //user input is given
    if scanvalue1='' then raise exception.Create('Please fill something in');

    if variableType in [vtByte,vtWord,vtDWord,vtQword,vtAll,vtCustom] then
    begin
      //parse scanvalue1
      try
        if hexadecimal then
          value:=strtoint64('$'+scanvalue1)
        else
          value:=strtoint64(scanvalue1);
      except
        if variableType=vtAll then
        begin
          try
            dvalue:=strtofloat(scanvalue1,FloatSettings);
          except
            if FloatSettings.DecimalSeparator=',' then
              FloatSettings.DecimalSeparator:='.'
            else
              FloatSettings.DecimalSeparator:=',';

            //try again
            try
              dvalue:=strtofloat(scanvalue1,FloatSettings);
            except
              if variabletype<>vtCustom then
                raise exception.Create(scanvalue1+' is not a valid value');
            end;
          end;
          value:=trunc(dvalue);

        end else
          if variabletype<>vtCustom then
            raise exception.Create(scanvalue1+' is an invalid value');
      end;

      if scanOption=soValueBetween then
      begin
        //also parse scanvalue2
        try
          if hexadecimal then
            value2:=strtoint64('$'+scanvalue2)
          else
            value2:=strtoint64(scanvalue2);

        except
          if variableType=vtAll then
          begin
            try
              dvalue:=strtofloat(scanvalue2,FloatSettings);
            except
              if FloatSettings.DecimalSeparator=',' then
                FloatSettings.DecimalSeparator:='.'
              else
                FloatSettings.DecimalSeparator:=',';

              //try again
              try
                dvalue:=strtofloat(scanvalue1,FloatSettings);
              except
                if variabletype<>vtCustom then
                  raise exception.Create(scanvalue1+' is not a valid value');
              end;
            end;
            value:=trunc(dvalue);
          end
          else
            if variabletype<>vtCustom then
              raise exception.Create(scanvalue2+' is an invalid value');
        end;
      end;
    end;

    if variableType in [vtsingle,vtDouble,vtAll,vtCustom] then
    begin
      try
        dvalue:=strtofloat(scanvalue1,FloatSettings);
      except
        if FloatSettings.DecimalSeparator=',' then
          FloatSettings.DecimalSeparator:='.'
        else
          FloatSettings.DecimalSeparator:=',';

        //try again
        try
          dvalue:=strtofloat(scanvalue1,FloatSettings);
        except
          if variabletype<>vtCustom then
            raise exception.Create(scanvalue1+' is not a valid value');
        end;
      end;

      if scanoption=soValueBetween then
      begin
        try
          dvalue2:=strtofloat(scanvalue2,FloatSettings);
        except
          if FloatSettings.DecimalSeparator=',' then
            FloatSettings.DecimalSeparator:='.'
          else
            FloatSettings.DecimalSeparator:=',';

          //try again
          try
            dvalue2:=strtofloat(scanvalue1,FloatSettings);
          except
            if variabletype<>vtCustom then
              raise exception.Create(scanvalue1+' is not a valid value');
          end;
        end;

      end;

      svalue:=dvalue;
      svalue2:=dvalue2;

      floataccuracy:=pos(FloatSettings.DecimalSeparator,scanvalue1);
      if floataccuracy>0 then
        floataccuracy:=length(scanvalue1)-floataccuracy;

      svalue:=RoundTo(svalue,-floataccuracy);
      svalue2:=RoundTo(svalue2,-floataccuracy);
      dvalue:=RoundTo(dvalue,-floataccuracy);
      dvalue2:=RoundTo(dvalue2,-floataccuracy);


      mindvalue:=dvalue-(1/(power(10,floataccuracy)));
      maxdvalue:=dvalue+(1/(power(10,floataccuracy)));
      minsvalue:=svalue-(1/(power(10,floataccuracy)));
      maxsvalue:=svalue+(1/(power(10,floataccuracy)));
    end;

    if variableType = vtString then
    begin
      widescanvalue1:=scanvalue1;
    end;    

    if variabletype = vtByteArray then
    begin
      ConvertStringToBytes(scanvalue1,hexadecimal,abs_arraytofind);
      abs_arraylength:=length(abs_arraytofind);
    end;

    if variableType = vtBinary then
    begin
      if binaryStringAsDecimal then
      begin
        if hexadecimal then
        begin
          scanvalue1:=scanvalue1+'$';
          scanvalue2:=scanvalue2+'$';
        end;

        binarystring:=inttobin(strtoint(scanvalue1))
      end
      else
        binarystring:=scanvalue1;

      andmask:='';
      bitmask:='';

      for i:=1 to length(binarystring) do
      begin
        if binarystring[i] in ['?','*'] then
        begin
          andmask:=andmask+'0';
          bitmask:=bitmask+'0';
        end
        else
        if binarystring[i] in ['0','1'] then
        begin
          andmask:=andmask+'1';
          bitmask:=bitmask+binarystring[i];
        end
        else
        if not (binarystring[i] in [' ',#8]) then
          raise exception.Create(binarystring+' is not a valid notation');
      end;

      self.andmask:=BinToInt(andmask);
      self.bitmask:=bintoint(bitmask);


    end;


  end;

  FlushRoutine:=genericFlush; //change if not so

  if variableType in [vtbinary,vtall] then
  begin
    getmem(CurrentAddressBuffer,buffersize*sizeof(Tbitaddress));
    getmem(SecondaryAddressBuffer,buffersize*sizeof(Tbitaddress));
  end
  else
  begin
    getmem(CurrentAddressBuffer,buffersize*4);
    getmem(SecondaryAddressBuffer,buffersize*4);
  end;

  if scanOption=soSameAsFirst then //create a first scan handler
    FirstScanHandler:=TFirstscanhandler.create;

  case variableType of
    vtByte:
    begin
      //byte config
      FoundBufferSize:=buffersize*1;
      StoreResultRoutine:=ByteSaveResult;

      case scanOption of
        soExactValue:       checkRoutine:=byteExact;
        soValueBetween:     checkroutine:=byteBetween;
        soBiggerThan:       checkroutine:=byteBiggerThan;
        soSmallerThan:      checkroutine:=byteSmallerThan;
        soIncreasedValue:   checkroutine:=byteIncreasedValue;
        soIncreasedValueBy: checkroutine:=byteIncreasedValueBy;
        soDecreasedValue:   checkroutine:=byteDecreasedValue;
        soDecreasedValueBy: checkroutine:=byteDecreasedValueBy;
        soChanged:          checkroutine:=byteChanged;
        soUnChanged:        checkroutine:=byteUnchanged;
        soSameAsFirst:      checkroutine:=byteUnchanged;
      end;
    end;

    vtWord:
    begin
      //word config
      FoundBufferSize:=buffersize*2;
      StoreResultRoutine:=WordSaveResult;

      case scanOption of
        soExactValue:       checkRoutine:=wordExact;
        soValueBetween:     checkroutine:=wordBetween;
        soBiggerThan:       checkroutine:=wordBiggerThan;
        soSmallerThan:      checkroutine:=wordSmallerThan;
        soIncreasedValue:   checkroutine:=wordIncreasedValue;
        soIncreasedValueBy: checkroutine:=wordIncreasedValueBy;
        soDecreasedValue:   checkroutine:=wordDecreasedValue;
        soDecreasedValueBy: checkroutine:=wordDecreasedValueBy;
        soChanged:          checkroutine:=wordChanged;
        soUnChanged:        checkroutine:=wordUnchanged;
        soSameAsFirst:      checkroutine:=wordUnchanged;
      end;
    end;

    vtDWord:
    begin
      //dword config
      FoundBufferSize:=buffersize*4;
      StoreResultRoutine:=DWordSaveResult;

      case scanOption of
        soExactValue:       checkRoutine:=dwordExact;
        soValueBetween:     checkroutine:=dwordBetween;
        soBiggerThan:       checkroutine:=dwordBiggerThan;
        soSmallerThan:      checkroutine:=dwordSmallerThan;
        soIncreasedValue:   checkroutine:=dwordIncreasedValue;
        soIncreasedValueBy: checkroutine:=dwordIncreasedValueBy;
        soDecreasedValue:   checkroutine:=dwordDecreasedValue;
        soDecreasedValueBy: checkroutine:=dwordDecreasedValueBy;
        soChanged:          checkroutine:=dwordChanged;
        soUnChanged:        checkroutine:=dwordUnchanged;
        soSameAsFirst:      checkroutine:=dwordUnchanged;
      end;
    end;

    vtQWord:
    begin
      //qword config
      FoundBufferSize:=buffersize*8;
      StoreResultRoutine:=QWordSaveResult;

      case scanOption of
        soExactValue:       checkRoutine:=qwordExact;
        soValueBetween:     checkroutine:=qwordBetween;
        soBiggerThan:       checkroutine:=qwordBiggerThan;
        soSmallerThan:      checkroutine:=qwordSmallerThan;
        soIncreasedValue:   checkroutine:=qwordIncreasedValue;
        soIncreasedValueBy: checkroutine:=qwordIncreasedValueBy;
        soDecreasedValue:   checkroutine:=qwordDecreasedValue;
        soDecreasedValueBy: checkroutine:=qwordDecreasedValueBy;
        soChanged:          checkroutine:=qwordChanged;
        soUnChanged:        checkroutine:=qwordUnchanged;
        soSameAsFirst:      checkroutine:=qwordUnchanged;
      end;
    end;

    vtSingle:
    begin
      //single config
      FoundBufferSize:=buffersize*4;
      StoreResultRoutine:=SingleSaveResult;

      case scanOption of
        soExactValue:       checkRoutine:=singleExact;
        soValueBetween:     checkroutine:=singleBetween;
        soBiggerThan:       checkroutine:=singleBiggerThan;
        soSmallerThan:      checkroutine:=singleSmallerThan;
        soIncreasedValue:   checkroutine:=singleIncreasedValue;
        soIncreasedValueBy: checkroutine:=singleIncreasedValueBy;
        soDecreasedValue:   checkroutine:=singleDecreasedValue;
        soDecreasedValueBy: checkroutine:=singleDecreasedValueBy;
        soChanged:          checkroutine:=singleChanged;
        soUnChanged:        checkroutine:=singleUnchanged;
        soSameAsFirst:      checkroutine:=singleUnchanged;
      end;
    end;

    vtDouble:
    begin
      //double config
      FoundBufferSize:=buffersize*8;
      StoreResultRoutine:=doubleSaveResult;

      case scanOption of
        soExactValue:       checkRoutine:=doubleExact;
        soValueBetween:     checkroutine:=doubleBetween;
        soBiggerThan:       checkroutine:=doubleBiggerThan;
        soSmallerThan:      checkroutine:=doubleSmallerThan;
        soIncreasedValue:   checkroutine:=doubleIncreasedValue;
        soIncreasedValueBy: checkroutine:=doubleIncreasedValueBy;
        soDecreasedValue:   checkroutine:=doubleDecreasedValue;
        soDecreasedValueBy: checkroutine:=doubleDecreasedValueBy;
        soChanged:          checkroutine:=doubleChanged;
        soUnChanged:        checkroutine:=doubleUnchanged;
        soSameAsFirst:      checkroutine:=doubleUnchanged;
      end;
    end;
      
    vtByteArray:
    begin
      CheckRoutine:=ArrayOfByteExact;
      flushroutine:=stringFlush;
      StoreResultRoutine:=ArrayOfByteSaveResult;
      FoundBufferSize:=0;
    end;



    vtBinary:
    begin
      CheckRoutine:=BinaryExact;
      StoreResultRoutine:=binarySaveResult;
      flushroutine:=binaryFlush;
    end;

    vtString:
    begin
      //check if unicode or casesensitive is used
      if not casesensitive then
      begin
        scanvalue1:=uppercase(scanvalue1);
        widescanvalue1:=uppercase(widescanvalue1);
      end;

      flushroutine:=stringFlush;
      
      case scanOption of
        soExactValue:
        begin
          if casesensitive and unicode then CheckRoutine:=CaseSensitiveUnicodeStringExact;
          if casesensitive and not unicode then CheckRoutine:=CaseSensitiveAnsiStringExact;
          if not casesensitive and unicode then CheckRoutine:=CaseInsensitiveUnicodeStringExact;
          if not casesensitive and not unicode then CheckRoutine:=CaseInsensitiveAnsiStringExact;
        end;

      end;

      StoreResultRoutine:=arrayOfByteSaveResult; //arrayOfByteSaveResult is compatible since it saves only the address
    end;

    vtAll:
    begin
      //almost like binary
      variablesize:=8; //override these variables
      fastscanalignsize:=1;

      FoundBufferSize:=buffersize*8;
      StoreResultRoutine:=allSaveResult;
      FlushRoutine:=allFlush;
      case scanOption of
        soExactValue:       checkRoutine:=allExact;
        soValueBetween:     checkroutine:=allBetween;
        soBiggerThan:       checkroutine:=allBiggerThan;
        soSmallerThan:      checkroutine:=allSmallerThan;
        soIncreasedValue:   checkroutine:=allIncreasedValue;
        soIncreasedValueBy: checkroutine:=allIncreasedValueBy;
        soDecreasedValue:   checkroutine:=allDecreasedValue;
        soDecreasedValueBy: checkroutine:=allDecreasedValueBy;
        soChanged:          checkroutine:=allChanged;
        soUnChanged:        checkroutine:=allUnchanged;
        soSameAsFirst:      checkroutine:=allUnchanged;
      end;
      //the other types have to be filled in by the nextscan routines
    end;

    vtCustom:
    begin
      case customscantype of
        cstAutoAssembler:
        begin
          //execute autoassemblescript and read out the alloced addresses and vars
          if not autoassemble(customscanscript,false,true,false,true,CustomScanAllocArray) then
            raise exception.Create('Invalid Auto Assembler script');

          for i:=0 to length(CustomScanAllocArray)-1 do
          begin
            if lowercase(CustomScanAllocArray[i].varname)='checkroutine' then
              pointer(@checkroutine):=pointer(CustomScanAllocArray[i].address)
            else
            if lowercase(CustomScanAllocArray[i].varname)='prologue' then
              customprologue:=pointer(CustomScanAllocArray[i].address)
            else
            if lowercase(CustomScanAllocArray[i].varname)='epilogue' then
              customepilogue:=pointer(CustomScanAllocArray[i].address)
            else
            if lowercase(CustomScanAllocArray[i].varname)='fastscanstepsize' then
              fastscanalignsize:=pdword(CustomScanAllocArray[i].address)^
            else
            if lowercase(CustomScanAllocArray[i].varname)='variablesize' then
              variablesize:=pdword(CustomScanAllocArray[i].address)^
            else
            if lowercase(CustomScanAllocArray[i].varname)='scantext' then
              pdword(CustomScanAllocArray[i].address)^:=dword(@scanvalue1[1])
            else
            if lowercase(CustomScanAllocArray[i].varname)='scanvalue' then
              puint64(CustomScanAllocArray[i].address)^:=value
            else
            if lowercase(CustomScanAllocArray[i].varname)='singlescanvalue' then
              psingle(CustomScanAllocArray[i].address)^:=svalue
            else
            if lowercase(CustomScanAllocArray[i].varname)='doublescanvalue' then
              pdouble(CustomScanAllocArray[i].address)^:=dvalue
            else
            if lowercase(CustomScanAllocArray[i].varname)='firstscan' then
              if pinteger(CustomScanAllocArray[i].address)^=1 then
                scanoption:=soSameAsFirst;
          end;
          //set the buffersizes to what they will be

          scanOption:=soCustom;

          FoundBufferSize:=buffersize*variablesize;

          //use the generic save routine
          StoreResultRoutine:=GenericSaveResult;

          //and flush
          FlushRoutine:=genericFlush;


          //what better time to call customprologue then the present
          customprologue();
        end;

      end;


    end;


  end;

  getmem(CurrentFoundBuffer,FoundBufferSize);
  getmem(SecondaryFoundBuffer,FoundBufferSize);
end;

procedure TScanner.nextNextscan;
var oldAddressfile: TFileStream;
    oldMemoryfile: TFileStream;
    i,j: integer;
    stopindex: integer;
    chunksize: integer;

    oldaddresses: array of dword;
    oldaddressesb: array of tbitaddress;
    oldmemory: pointer;

    part: integer;
begin
  configurescanroutine;
  oldAddressFile:=nil;
  oldMemoryFile:=nil;
  oldmemory:=nil;
  try
    oldAddressFile:=TFileStream.Create(CheatEngineDir+'Addresses.TMP',fmOpenRead or fmShareDenyNone);
    oldMemoryFile:=TFileStream.Create(CheatEngineDir+'Memory.TMP',fmOpenRead or fmShareDenyNone);

    //set the current index to startentry
    stopindex:=stopentry-startentry;  //going from 0 to stopindex

    if self.variableType in [vtBinary,vtall] then
    begin
      //addressfile of tbitaddresstype
      setlength(oldaddressesb,buffersize);
      oldAddressFile.seek(7+sizeof(TBitAddress)*startentry,soFromBeginning);
      if self.variableType=vtall then
        oldmemory:=virtualAlloc(nil,buffersize*variablesize,MEM_COMMIT	or MEM_TOP_DOWN	, PAGE_READWRITE);

      if oldmemory=nil then raise exception.Create('Error allocating '+inttostr(chunksize*variablesize)+' bytes for the old results. chunksize='+inttostr(chunksize)+' variablesize='+inttostr(variablesize));

      oldMemoryFile.seek(variablesize*startentry,soFromBeginning);
    end
    else
    begin
      setlength(oldaddresses,buffersize);
      oldmemory:=virtualAlloc(nil,buffersize*variablesize,MEM_COMMIT	or MEM_TOP_DOWN	, PAGE_READWRITE);
      if oldmemory=nil then raise exception.Create('Error allocating '+inttostr(chunksize*variablesize)+' bytes for the old results. chunksize='+inttostr(chunksize)+' variablesize='+inttostr(variablesize));

      oldAddressFile.seek(7+sizeof(DWORD)*startentry,soFromBeginning);
      if not (self.variableType in [vtString,vtByteArray]) then
        oldMemoryFile.seek(variablesize*startentry,soFromBeginning);
    end;


    //read in chunks of buffersize
    i:=0;
    while i<=stopindex do
    begin
      chunksize:=stopindex-i+1;
      if chunksize>buffersize then
        chunksize:=buffersize;

      case variabletype of
        vtBinary:
        begin
          oldAddressFile.ReadBuffer(oldaddressesb[0],chunksize*sizeof(tbitaddress));
          nextnextscanmembinary(@oldaddressesb[0],chunksize);
        end;

        vtAll:
        begin
          oldAddressFile.ReadBuffer(oldaddressesb[0],chunksize*sizeof(tbitaddress));
          if scanoption<>soSameAsFirst then
            oldMemoryFile.ReadBuffer(oldmemory^,chunksize*variablesize);
            
          nextnextscanmemall(@oldaddressesb[0],oldmemory,chunksize);
        end;

        else
        begin

          oldAddressFile.ReadBuffer(oldaddresses[0],chunksize*4);

          if scanoption<>soSameAsFirst then
          begin
            if not (self.variableType in [vtString,vtByteArray]) then //skip the types with no previous result stored
              oldMemoryFile.ReadBuffer(oldmemory^,chunksize*variablesize);
          end;


          nextnextscanmem(@oldaddresses[0],oldmemory,chunksize);



        end;
      end;

      inc(scanned,chunksize);
      inc(i,chunksize);
    end;


    flushroutine; //save all results temporarily stored in memory

  finally
    if oldAddressFile<>nil then oldAddressFile.free;
    if oldMemoryFile<>nil then oldMemoryFile.free;
    if oldmemory<>nil then virtualfree(oldmemory,0,MEM_RELEASE);
  end;
end;

procedure TScanner.firstNextscan;
var
  i: integer;
  size: dword;
  currentbase: dword;
  startregion: integer;
  stopregion: integer;
  memorybuffer: ^byte;
  oldbuffer: ^byte;
  toread: dword;
  actualread: dword;
begin
  startregion:=_startregion; //using a variable so stack can be used, with possibility of register
  stopregion:=_stopregion;

  //allocate a buffer for reading the new memory buffer
  memorybuffer:=virtualAlloc(nil,maxregionsize+variablesize,MEM_COMMIT	or MEM_TOP_DOWN	, PAGE_READWRITE);
  try
    //configure some variables and function pointers
    configurescanroutine;

    for i:=startregion to stopregion do
    begin
      if terminated or OwningScanController.Terminated then exit;
      if i=startregion then
      begin
        currentbase:=startaddress;
        toread:=OwningScanController.memregion[i].MemorySize-(startaddress-OwningScanController.memregion[i].BaseAddress);

        //set oldbuffer and point to the exact start
        oldbuffer:=OwningScanController.memregion[i].startaddress;
        inc(oldbuffer, startaddress-OwningScanController.memregion[i].BaseAddress);
      end
      else
      begin
        currentbase:=OwningScanController.memregion[i].BaseAddress;
        toread:=OwningScanController.memregion[i].MemorySize;

        oldbuffer:=OwningScanController.memregion[i].startaddress;
      end;

      if (i=stopregion) and ((currentbase+toread)>stopaddress) then
        toread:=stopaddress-currentbase;

      repeat
        size:=toread;
        if (size>buffersize) then size:=buffersize;

        actualread:=0;

        if size<toread then //there's something left to scan, so I can add the variablesize to it
          ReadProcessMemory(processhandle,pointer(currentbase),memorybuffer,size+variablesize-1,actualread)
        else
          ReadProcessMemory(processhandle,pointer(currentbase),memorybuffer,size,actualread);


        firstnextscanmem(currentbase,memorybuffer,oldbuffer,actualread);

        inc(scanned,size); //for the progressbar
        dec(toread,size);
        inc(oldbuffer,size);
        inc(currentbase,size);

      until toread=0;

    end;
    flushroutine;
  finally
    virtualfree(memorybuffer,0,MEM_RELEASE);
  end;
end;

procedure TScanner.firstscan;
var i,j: integer;

    currentbase: dword;
    size: dword;
    actualread: dword;
    memorybuffer: ^byte;
    toread: dword;
    startregion: integer;
    stopregion: integer;    
begin
  //first find out where in the previousmemory of this thread starts
  try

    startregion:=_startregion; //using a variable so stack can be used, with possibility of register
    stopregion:=_stopregion;

    if scanOption<>soUnknownValue then
    begin
      //not unknown initial
      memorybuffer:=virtualAlloc(nil,maxregionsize+variablesize,MEM_COMMIT	or MEM_TOP_DOWN	, PAGE_READWRITE);
      configurescanroutine;
    end
    else //it is a unknown initial value, so use the previousmemorybuffer instead
    begin
      memorybuffer:=pointer(dword(OwningScanController.OwningMemScan.previousMemoryBuffer)+dword(OwningScanController.memregion[startregion].startaddress)+(startaddress-OwningScanController.memregion[startregion].BaseAddress));
      variablesize:=1; //ignore
    end;



    //now save the region between startaddress and stopaddress and create own memregion list
    setlength(memregions,16);

    for i:=startregion to stopregion do
    begin
      if terminated or OwningScanController.Terminated then exit;

      if i=startregion then
      begin
        currentbase:=startaddress;
        toread:=OwningScanController.memregion[i].MemorySize-(startaddress-OwningScanController.memregion[i].BaseAddress);
      end
      else
      begin
        currentbase:=OwningScanController.memregion[i].BaseAddress;
        toread:=OwningScanController.memregion[i].MemorySize;
      end;

      if (i=stopregion) and ((currentbase+toread)>stopaddress) then
        toread:=stopaddress-currentbase;

      repeat
        //05955958
        size:=toread;

        if (size>buffersize) then size:=buffersize;

        actualread:=0;
        //variablesize:=0;
        if size<toread then
          ReadProcessMemory(processhandle,pointer(currentbase),memorybuffer,size+variablesize-1,actualread)
        else
          ReadProcessMemory(processhandle,pointer(currentbase),memorybuffer,size,actualread);

        //+variablesize for overlap, only when not unknown var

        if scanOption=soUnknownValue then
        begin
          //unknown initial value, so create a memregion for this

          memregions[memregionpos].BaseAddress:=currentbase;
          memregions[memregionpos].MemorySize:=actualread;
          memregions[memregionpos].startaddress:=memorybuffer;

          inc(memorybuffer,size);

          inc(memregionpos);
          if (memregionpos mod 16) = 0 then
            setlength(memregions,length(memregions)+16);
        end
        else
        begin
          //scan the buffer
          firstscanmem(currentbase,memorybuffer,actualread);

        end;

        currentbase:=currentbase+size;
        
        inc(scanned,size); //for the progressbar
        dec(toread,size);
      until toread=0;


    end;

    if (scanOption<>soUnknownValue) then flushroutine; //save results
  finally
    if (scanOption<>soUnknownValue) and (memorybuffer<>nil) then
      virtualfree(memorybuffer,0,MEM_RELEASE);
  end;
end;

procedure TScanner.execute;
var i: integer;
begin
  Set8087CW($133f); //disable floating point exceptions in this thread

  try
    scanwriter:=TScanfilewriter.create(self.OwningScanController,addressfile,memoryfile);
    if scantype=stFirstScan then firstscan;
    if scantype=stNextScan then
    begin
      if useNextNextScan then
        nextnextscan
      else
        firstnextscan;
    end;

    //tell scanwriter to stop
    scanwriter.flush;
  except
    on e: exception do
    begin
      haserror:=true;
      errorstring:='thread '+inttostr(scannernr)+':'+e.message;

      //tell all siblings to terminate, something messed up
      //and I can just do this, since the ScanController is waiting for us, and terminate is pretty much atomic
      for i:=0 to length(OwningScanController.scanners)-1 do
        OwningScanController.Terminate;

    end;
  end;


  if (variableType=vtCustom) and (length(CustomScanAllocArray)>0) then
  begin
    try
      //custom scantype, and the script has been executed successfully (CustomScanAllocArray has been filled)
      //call customepilogue
      if assigned(customepilogue) then //customepilogue isn't nil so has been set
        CustomEpilogue; //call it

      //and dealloc the memory
      autoassemble(customscanscript,false,false,false,true,CustomScanAllocArray);



    except
      on e: exception do
      begin
        if not haserror then
        begin
          haserror:=true;
          errorstring:='Custom scan cleanup error:'+e.Message;
        end;
      end;

    end;

  end;

  isdone:=true;
end;

destructor TScanner.destroy;
var i: integer;
begin
  if AddressFile<>nil then //can be made nil by the scancontroller
  begin
    Addressfile.free;
    DeleteFile(CheatEngineDir+'Addresses-'+inttostr(ThreadID)+'.TMP');
  end;

  if MemoryFile<>nil then
  begin
    MemoryFile.free;
    DeleteFile(CheatEngineDir+'Memory-'+inttostr(ThreadID)+'.TMP');
  end;

  if scanwriter<>nil then
    scanwriter.free;


  if CurrentFoundBuffer<>nil then freemem(CurrentFoundBuffer);
  if SecondaryFoundBuffer<>nil then freemem(SecondaryFoundBuffer);
  if CurrentAddressBuffer<>nil then freemem(CurrentAddressBuffer);
  if SecondaryAddressBuffer<>nil then freemem(SecondaryAddressBuffer);

  if firstscanhandler<>nil then firstscanhandler.free;

  inherited destroy;
end;

constructor TScanner.create(suspended: boolean);
begin
  inherited create(true); //do create the thread, I need the threadid
  AddressFilename:=CheatEngineDir+'Addresses-'+inttostr(ThreadID)+'.TMP';
  MemoryFilename:=CheatEngineDir+'Memory-'+inttostr(ThreadID)+'.TMP';
  AddressFile:=TFileStream.Create(AddressFilename,fmCreate or fmSharedenynone);
  MemoryFile:=TFileStream.Create(MemoryFilename,fmCreate or fmSharedenynone);

  Priority:=cefuncproc.scanpriority;

  if not suspended then resume;   //would be stupid, but ok...
end;

//===============TScanController===============//

procedure TScanController.updategui;
var totaladdressestoscan, currentlyscanned: dword;
begin
  //runs in mainthread
  OwningMemScan.progressbar.Position:=OwningMemScan.GetProgress(totaladdressestoscan,currentlyscanned);
end;

procedure TScanController.errorpopup;
begin
  messagedlg(errorstring,mtError,[mbok],0);
end;

procedure TScanController.fillVariableAndFastScanAlignSize;
var s: string;
begin
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
      if scanoption<>soUnknownValue then
        variablesize:=getBytecountArrayOfByteString(scanvalue1)
      else
        variablesize:=1;

      fastscanalignsize:=1;

      self.OwningMemScan.arrayLength:=variablesize;
    end;

    vtBinary:
    begin
      if scanoption<>soUnknownValue then
        variablesize:=getBytecountBinaryString(scanvalue1,binaryStringAsDecimal)
      else
        variablesize:=1;

      fastscanalignsize:=1;

      //store some info for lookup
      if binaryStringAsDecimal then //first convert do binarystring
        s:=inttobin(strtoint(scanvalue1))
      else
        s:=trim(scanvalue1);

      self.OwningMemScan.binaryLength:=length(s);
    end;

    vtString:
    begin
      if scanoption<>soUnknownValue then
      begin
        variablesize:=length(scanvalue1);
        if unicode then variablesize:=variablesize*2;
      end
      else variablesize:=1;
      
      fastscanalignsize:=1;

      self.OwningMemScan.stringUnicode:=unicode;
      self.OwningMemScan.stringLength:=length(scanvalue1);
    end;

    vtAll:
    begin
      variablesize:=8;
      fastscanalignsize:=1;
    end;


    vtCustom:
    begin
      variablesize:=1; //customtypecallback(scanvalue1);
      fastscanalignsize:=1;
    end;
  end;
end;


procedure TScanController.NextNextScan;
{
NextNextScan will read results of the previous scan, and pass it off to scanner threads
}
var 
    AddressFile,MemoryFile: TFileStream;
    blocksize: integer;
    i: integer;

    currententry: integer;
    datatype: string[6];
begin
  threadcount:=getcpucount;

  
  //read the results and split up
  AddressFile:=TFileStream.Create(CheatEngineDir+'Addresses.TMP',fmOpenRead or fmShareDenyNone);
  try
    if variableType in [vtbinary,vtall] then //it uses a specific TBitAddress instead of a dword
      totalAddresses:=(addressfile.size-7) div 8
    else
      totalAddresses:=(addressfile.size-7) div 4;

      //split up into blocks
    //if totaladdresses>0 then
    begin
      blocksize:=totaladdresses div threadcount;


      if (blocksize=0) and (totaladdresses>0) then blocksize:=1; //in case of 4 threads and totaladdresses is 3 or less

      scannersCS.Enter; //block access by the mainthread on the scanners object, could scanner[14] has not yet been created when doing a progress request
      try
        setlength(scanners,threadcount);
        //setup the scanner threads

        currententry:=0;
        for i:=0 to threadcount-1 do
        begin
          scanners[i]:=tscanner.Create(true);
          scanners[i].scannernr:=i;
          scanners[i].OwningScanController:=self;

          scanners[i].startentry:=currententry;

          if i=threadcount-1 then
            scanners[i].stopentry:=totalAddresses-1
          else
            scanners[i].stopentry:=currententry+blocksize;

          if scanners[i].stopentry>=totaladdresses then
            scanners[i].stopentry:=totalAddresses-1;

          currententry:=scanners[i].stopentry+1; //next thread will start at the next one

          scanners[i].scanType:=scanType; //stNextScan obviously
          scanners[i].scanoption:=scanoption;
          scanners[i].variableType:=VariableType;
          scanners[i].roundingtype:=roundingtype;
          scanners[i].fastscan:=fastscan;
          scanners[i].scanValue1:=scanvalue1; //usual scanvalue
          scanners[i].scanValue2:=scanValue2; //2nd value for between scan
          scanners[i].readonly:=readonly;
          scanners[i].unicode:=unicode;
          scanners[i].caseSensitive:=caseSensitive;
          scanners[i].hexadecimal:=hexadecimal;
          scanners[i].binaryStringAsDecimal:=binaryStringAsDecimal;

          scanners[i].fastscanalignsize:=fastscanalignsize;
          scanners[i].variablesize:=variablesize;
          scanners[i].useNextNextscan:=true; //address result scan so nextnextscan
          scanners[i].customscanscript:=customscanscript;
          scanners[i].customscantype:=customscantype;

          if i=0 then //first thread gets the header part
          begin
            datatype:='NORMAL';
            scanners[i].AddressFile.WriteBuffer(datatype,sizeof(datatype));
          end;
        end;


      finally
        scannersCS.leave;
      end;


      for i:=0 to length(scanners)-1 do
        scanners[i].Resume;


      OwningMemScan.found:=0;
      //and now we wait
      for i:=0 to threadcount-1 do
      begin
        repeat
          WaitForSingleObject(scanners[i].Handle,25); //25ms, an eternity for a cpu
          synchronize(updategui);
        until scanners[i].isdone;

        if scanners[i].haserror then
        begin
          haserror:=true;
          errorstring:=scanners[i].errorstring;
          break;
        end;

        inc(OwningMemScan.found,scanners[i].totalfound);
      end;

      synchronize(updategui);
      if haserror then
      begin
        //synchronize(errorpopup);
        exit;
      end;

    end;

  finally
    addressfile.free; //release the file so it can be overwritten
  end;

  savescannerresults:=true;

end;


procedure TScanController.FirstNextScan;
{
FirstNextScan will read first scan regions, and pass it off to scanner threads
}
var

  i,j: integer;
  totalProcessMemorySize: dword;
  blocksize: dword;
  leftfromprevious: dword;
  currentblocksize: dword;
  memoryfile,addressfile: tfilestream;
  datatype: string[6];
begin
  threadcount:=GetCPUCount;
  totalProcessMemorySize:=0;

  memregion:=OwningMemscan.memRegion;
  memregionpos:=OwningMemscan.memRegionPos;


  {
  read the addressfile and split it up into chunks for the scanner threads
  important variables:
  OwningMemScan.previousMemoryBuffer
  OwningMemscan.memRegion
  OwningMemscan.memRegionPos
  }
  //find total memoryammount
  for i:=0 to memRegionPos do
    inc(totalProcessMemorySize, memRegion[i].MemorySize);

  totalAddresses:=totalProcessMemorySize;

  //now split up into workloads
  Blocksize:=totalProcessMemorySize div threadcount;
  Blocksize:=blocksize-(blocksize mod 4096); //lastblock gets the missing bytes


  //spawn the thread, but don't run till configured
  scannersCS.Enter; //block access by the mainthread on the scanners object, could scanner[14] has not yet been created when doing a progress request
  try
    setlength(scanners,threadcount);
    j:=0; //start at memregion 0
    leftfromprevious:=0;

    for i:=0 to threadcount-1 do
    begin
      scanners[i]:=tscanner.Create(true);
      scanners[i].scannernr:=i;
      scanners[i].OwningScanController:=self;

      scanners[i]._startregion:=j;
      scanners[i].startaddress:=memRegion[j].BaseAddress+leftfromprevious;

      scanners[i].maxregionsize:=0;

      if i=(threadcount-1) then
      begin
        //this is the last scanner
        scanners[i].stopaddress:=stopaddress;  //let it go till the end
        scanners[i]._stopregion:=memregionpos-1;

        if scanOption<>soUnknownValue then
        begin
          //define maxregionsize
          while j<memregionpos do
          begin
            if scanners[i].maxregionsize<memregion[j].MemorySize then
              scanners[i].maxregionsize:=memregion[j].MemorySize;
              
            inc(j);
          end;
        end;
      end
      else
      begin
        currentblocksize:=0;
        inc(currentblocksize,memregion[j].MemorySize+leftfromprevious);
        inc(j);

        while (currentblocksize<blocksize) and (j<memregionpos) do
        begin
          if scanners[i].maxregionsize<memregion[j].MemorySize+variablesize then
            scanners[i].maxregionsize:=memregion[j].MemorySize+variablesize;

          inc(currentblocksize,memregion[j].MemorySize);
          inc(j);
        end;
        dec(j);

        scanners[i]._stopregion:=j;
        scanners[i].stopaddress:=memregion[j].BaseAddress+memregion[j].MemorySize;

        leftfromprevious:=currentblocksize-blocksize;

        if leftfromprevious<=0 then inc(j);

        dec(scanners[i].stopaddress,leftfromprevious);
      end;

      if scanners[i].maxregionsize>buffersize then
        scanners[i].maxregionsize:=buffersize;

      //now configure the scanner thread with the same info this thread got, with some extra info

      scanners[i].scanType:=scanType; //stNextScan obviously
      scanners[i].scanoption:=scanoption;
      scanners[i].variableType:=VariableType;
      scanners[i].roundingtype:=roundingtype;
      scanners[i].fastscan:=fastscan;
      scanners[i].scanValue1:=scanvalue1; //usual scanvalue
      scanners[i].scanValue2:=scanValue2; //2nd value for between scan
      scanners[i].readonly:=readonly;
      scanners[i].unicode:=unicode;
      scanners[i].caseSensitive:=caseSensitive;
      scanners[i].hexadecimal:=hexadecimal;
      scanners[i].binaryStringAsDecimal:=binaryStringAsDecimal;

      scanners[i].fastscanalignsize:=fastscanalignsize;
      scanners[i].variablesize:=variablesize;
      scanners[i].useNextNextscan:=false; //region scan so firstnextscan
      scanners[i].customscanscript:=customscanscript;
      scanners[i].customscantype:=customscantype;

      if i=0 then //first thread gets the header part
      begin
        datatype:='NORMAL';
        scanners[i].AddressFile.WriteBuffer(datatype,sizeof(datatype));
      end;

    end;
  finally
    scannersCS.Leave;
  end;

  //all threads are created, so start them
  for i:=0 to length(scanners)-1 do
    scanners[i].Resume;


  savescannerresults:=true;


  OwningMemScan.found:=0;
  //and now we wait
  for i:=0 to threadcount-1 do
  begin
    repeat
      WaitForSingleObject(scanners[i].Handle,25); //25ms, an eternity for a cpu
      synchronize(updategui);
    until scanners[i].isdone;


    //scanners[i].WaitFor; //if the mainthread has to cancel, it has to tell the child scanners to terminate instead
    if scanners[i].haserror then
    begin
      haserror:=true;
      errorstring:=scanners[i].errorstring;
      break;
    end;

    inc(OwningMemScan.found,scanners[i].totalfound);
  end;

  synchronize(updategui);
  if haserror then
  begin
    //synchronize(errorpopup);
    exit;
  end;

  //scan is successfull.



  //now clean up some mem, it's not needed anymore
  if OwningMemScan.previousMemoryBuffer<>nil then
  begin
    virtualfree(OwningMemScan.previousMemoryBuffer,0,MEM_RELEASE);
    OwningMemscan.previousMemoryBuffer:=nil;
  end;
end;


procedure TScanController.nextScan;
var AddressFile: TFilestream;
    datatype: string[6];
begin
  //open the address file and determine if it's a region scan or result scan
  AddressFile:=TFileStream.Create(CheatEngineDir+'Addresses.TMP',fmOpenRead or fmSharedenynone);
  try
    Addressfile.ReadBuffer(datatype,sizeof(datatype));
  finally
    addressFile.free;
  end;

  if datatype='REGION' then
    FirstNextScan
  else
    NextNextScan;
end;

procedure TScanController.firstScan;
{
first scan will gather the memory regions, open the files, and spawn scanners
}
var
  currentBaseAddress: dword;
  mbi : TMemoryBasicInformation;

  i,j: integer;

  Blocksize: dword;
  currentblocksize: dword;
  totalProcessMemorySize: dword;
  leftfromprevious: dword;


  datatype: string[6];
begin
  threadcount:=GetCPUCount;
  totalProcessMemorySize:=0;
  {
  ScanController plan:
  spawn idle scanner threads , ammount=maxthreadcount in settings
  enumerate all regions and split up into jobs for the threads
  if scanoption=soUnknownValue make a buffer  big enough to hold all the memory, and give the threads the startbase where in the buffer their first region will start
  start scanner threads
  }

  //determine the size in bytes for this variable. If one is provided.
  //also fill in the fastscan alignment as well





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

      //still here, so valid
      try
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

      finally
        inc(totalProcessMemorySize,mbi.RegionSize); //add this size to the total

      end;
    end;


    currentBaseAddress:=dword(mbi.baseaddress)+mbi.RegionSize;
  end;

  totalAddresses:=totalProcessMemorySize;

  if memRegionPos=0 then raise exception.Create('No readable memory found');


  //if soUnknown, make a buffer where it can store all the 'previous' memory
  if scanOption=soUnknownValue then
  begin
    //extra check to make sure the previous scan was cleared
    if OwningMemScan.previousMemoryBuffer<>nil then virtualfree(OwningMemScan.previousMemoryBuffer,0,MEM_RELEASE);

    OwningMemScan.previousMemoryBuffer:=VirtualAlloc(nil,totalProcessMemorySize, MEM_COMMIT	or MEM_TOP_DOWN, PAGE_READWRITE); //top down to try to prevent memory fragmentation
  end;


  //split up into seperate workloads
  Blocksize:=totalProcessMemorySize div threadcount;
  Blocksize:=blocksize-(blocksize mod 4096); //lastblock gets the missing bytes


  

  scannersCS.Enter; //block access by the mainthread on the scanners object, could scanner[14] has not yet been created when doing a progress request
  try
    setlength(scanners,threadcount);
    j:=0; //start at memregion 0
    leftfromprevious:=0;


    for i:=0 to threadcount-1 do
    begin
      scanners[i]:=tscanner.Create(true);
      scanners[i].scannernr:=i;
      scanners[i].OwningScanController:=self;

      scanners[i]._startregion:=j;
      scanners[i].startaddress:=memRegion[j].BaseAddress+leftfromprevious;
      
      scanners[i].maxregionsize:=0;

      if i=(threadcount-1) then
      begin
        scanners[i].stopaddress:=stopaddress;
        scanners[i]._stopregion:=memregionpos-1;

        if scanOption<>soUnknownValue then
        begin
          //define maxregionsize
          while j<memregionpos do
          begin
            if scanners[i].maxregionsize<memregion[j].MemorySize then
              scanners[i].maxregionsize:=memregion[j].MemorySize;
            inc(j);
          end;
        end;
      end
      else
      begin
        currentblocksize:=0;
        inc(currentblocksize,memregion[j].MemorySize+leftfromprevious);
        inc(j);

        while (currentblocksize<blocksize) and (j<memregionpos) do
        begin
          if scanOption<>soUnknownValue then //not a unknown initial value scan, so it doesn't need overlap
          begin
            if scanners[i].maxregionsize<memregion[j].MemorySize+variablesize then
              scanners[i].maxregionsize:=memregion[j].MemorySize+variablesize;
          end;

          inc(currentblocksize,memregion[j].MemorySize);
          inc(j);
        end;
        dec(j);

        scanners[i]._stopregion:=j;
        scanners[i].stopaddress:=memregion[j].BaseAddress+memregion[j].MemorySize;

        leftfromprevious:=currentblocksize-blocksize;
        dec(scanners[i].stopaddress,leftfromprevious);

        if leftfromprevious<=0 then inc(j); //nothing left in this region
      end;

      if scanners[i].maxregionsize>buffersize then
        scanners[i].maxregionsize:=buffersize;      

      //now configure the scanner thread with the same info this thread got, with some extra info
      scanners[i].scanType:=scanType; //stFirstScan obviously
      scanners[i].scanoption:=scanoption;
      scanners[i].variableType:=VariableType;
      scanners[i].roundingtype:=roundingtype;
      scanners[i].fastscan:=fastscan;
      scanners[i].scanValue1:=scanvalue1; //usual scanvalue
      scanners[i].scanValue2:=scanValue2; //2nd value for between scan
      scanners[i].readonly:=readonly;
      scanners[i].unicode:=unicode;
      scanners[i].caseSensitive:=caseSensitive;
      scanners[i].hexadecimal:=hexadecimal;
      scanners[i].binaryStringAsDecimal:=binaryStringAsDecimal;

      scanners[i].fastscanalignsize:=fastscanalignsize;
      scanners[i].variablesize:=variablesize;
      scanners[i].customscanscript:=customscanscript;
      scanners[i].customscantype:=customscantype;

      if i=0 then //first thread gets the header part
      begin
        if scanoption=soUnknownValue then
          datatype:='REGION'
        else
          datatype:='NORMAL';

        scanners[i].AddressFile.WriteBuffer(datatype,sizeof(datatype));
      end;

    end;
  finally
    scannersCS.Leave;
  end;

  //all threads are created, so start them
  for i:=0 to length(scanners)-1 do
    scanners[i].Resume;

  //prepare the result files
  try

    OwningMemScan.found:=0;
    //and now we wait
    for i:=0 to threadcount-1 do
    begin
      repeat
        WaitForSingleObject(scanners[i].Handle,25); //25ms, an eternity for a cpu
        synchronize(updategui);
      until scanners[i].isdone;


      if scanners[i].haserror then
      begin
        OwningMemScan.found:=0;
        haserror:=true;
        errorstring:=scanners[i].errorstring;
        break;
      end;

      inc(OwningMemScan.found,scanners[i].totalfound);
    end;

    synchronize(updategui);
    if haserror then
    begin
      OwningMemScan.found:=0;
      //synchronize(errorpopup);
      exit;
    end;

    //all threads are done
    //combine all results and write them too the AddressFile and MemoryFile



    if scanOption=soUnknownValue then
    begin
      //read the scanner memregions and adapt this memregion to it

      //first find out how many regions it got
      memRegionPos:=0;
      for i:=0 to threadcount-1 do
        inc(memRegionPos,scanners[i].memRegionPos);

      setlength(OwningMemScan.memRegion,memRegionPos); //setting the size accordingly (max, can end up smaller due to appending)

      OwningMemScan.memRegionPos:=0;
      for i:=0 to threadcount-1 do
      begin
        for j:=0 to scanners[i].memRegionPos-1 do
        begin
          inc(OwningMemScan.found,scanners[i].memregions[j].MemorySize);
          
          if OwningMemScan.memregionpos>0 then
          begin
            if OwningMemScan.memregion[OwningMemScan.memregionpos-1].BaseAddress+OwningMemScan.memregion[OwningMemScan.memregionpos-1].MemorySize=scanners[i].memregions[j].BaseAddress then
            begin
              //append
              inc(OwningMemScan.memregion[OwningMemScan.memregionpos-1].MemorySize,scanners[i].memregions[j].MemorySize);
              continue;
            end;
          end;

          //new one
          OwningMemScan.memregion[OwningMemScan.memregionpos]:=scanners[i].memregions[j];
          inc(OwningMemScan.memregionpos);
        end;

      end;

      if fastscan then //divide by alignsize of fastscan
        OwningMemScan.found:=OwningMemScan.found div fastscanalignsize;

      savescannerresults:=true;
    end
    else
    begin
      savescannerresults:=true;
    end;
    
  finally
  end;



end;

procedure TScanController.execute;
var err: dword;
    i: integer;
    oldpos,oldmempos: integer;
begin
  //check what it is, and call first/next/nextnext- scan
  err:=0;
  errorstring:='';


  try
    fillVariableAndFastScanAlignSize;
    if scantype=stFirstScan then firstscan;
    if scantype=stNextScan then nextscan;
  except
    on e: exception do
    begin
      haserror:=true;
      errorstring:='controller:'+e.message;
    end;
  end;

  if haserror then err:=1;


  if savescannerresults then //prepare saving. Set the filesize
  begin
    freeandnil(scanners[0].Addressfile);
    freeandnil(scanners[0].Memoryfile);

    //addresses
    deletefile(CheatEngineDir+'Addresses.UNDO');
    renamefile(CheatEngineDir+'Addresses.TMP',CheatEngineDir+'Addresses.UNDO');
    renamefile(scanners[0].Addressfilename, CheatEngineDir+'Addresses.TMP');

    //memory
    deletefile(CheatEngineDir+'Memory.UNDO');
    renamefile(CheatEngineDir+'Memory.TMP',CheatEngineDir+'Memory.UNDO');
    renamefile(scanners[0].Memoryfilename, CheatEngineDir+'Memory.TMP');
    try
      AddressFile:=TFileStream.Create(CheatEngineDir+'Addresses.TMP',fmOpenWrite or fmShareDenyNone);
      MemoryFile:=TFileStream.Create(CheatEngineDir+'Memory.TMP',fmOpenWrite or fmsharedenynone);
    except
      haserror:=true;
      errorstring:='Error while loading results';
      exit;
    end;

    oldpos:=addressfile.Size;
    oldmempos:=memoryfile.size;

    for i:=1 to threadcount-1 do
      AddressFile.size:=AddressFile.size+scanners[i].Addressfile.Size;
  end;

  //send message saying it's done
  isdone:=true;
  postMessage(notifywindow,notifymessage,err,0);
  
  //sleep(random(2000)); //stress emulation test

  if savescannerresults and (addressfile<>nil) then //now actually save the scanner results
  begin

    //AddressFile should already have been created with the correct datatype and opened as denynone
    AddressFile.Seek(oldpos,soFromBeginning);
    Memoryfile.seek(oldmempos,soFromBeginning);

    //save the exact results, and copy it to the AddressesFirst.tmp and Memoryfirst.tmp files
    for i:=1 to length(scanners)-1 do
    begin
      addressfile.CopyFrom(scanners[i].Addressfile,0);
      Memoryfile.CopyFrom(scanners[i].MemoryFile,0);
    end;

  end;

  if scantype=stFirstScan then
    OwningMemScan.SaveFirstScanThread:=TSaveFirstScanThread.create(false,@OwningMemScan.memregion,@OwningMemScan.memregionpos, OwningMemScan.previousMemoryBuffer);

  
  //sleep(random(2000)); //stress emulation test

  //clean up secondary scanner threads, their destructor will close and delete their files
  scannersCS.enter;
  try
    for i:=0 to length(scanners)-1 do
      scanners[i].Free;

    setlength(scanners,0);
  finally
    scannersCS.leave;
  end;


  //cleanup the files
  if addressfile<>nil then addressfile.Free;
  if MemoryFile<>nil then Memoryfile.Free;
end;

constructor TScanController.create(suspended: boolean);
begin
  scannersCS:=TCriticalSection.Create;
  resultsaveCS:=TCriticalsection.create;
  
  inherited create(suspended);
end;

destructor TScancontroller.destroy;
begin
  scannersCS.free;
  resultsaveCS.free;
  inherited destroy;
end;


//----------------memscan--------------//
procedure TMemscan.TerminateScan;
var i: integer;
begin
  if scancontroller<>nil then
    scanController.Terminate;

end;

function TMemscan.GetErrorString: string;
begin
  result:='';
  if scancontroller<>nil then
  begin
    scancontroller.WaitFor;
    if scancontroller.haserror then
      result:=scancontroller.errorstring;
  end;

end;

function TMemscan.GetProgress(var totaladdressestoscan:dword; var currentlyscanned: dword):integer;
{returns a value between 1 and 1000 representing how far the scan is}
var i: integer;
begin
  //Take care of memory
  if self.scanController<>nil then
  begin
    totaladdressestoscan:=self.scanController.totalAddresses;
    currentlyscanned:=0;
    scanController.scannersCS.enter;
    try
      for i:=0 to length(self.scanController.scanners)-1 do
        inc(currentlyscanned,self.scanController.scanners[i].scanned);

    finally
      scanController.scannersCS.Leave;
    end;
    
    result:=trunc((currentlyscanned / totaladdressestoscan) * 1000);
  end
  else
  begin
    result:=0;
    totaladdressestoscan:=0;
    currentlyscanned:=0;
  end;
end;

function TMemscan.GetFoundCount: uint64;
begin
  result:=found;
end;

function TMemscan.Getbinarysize: int64;
begin
  case self.currentVariableType of
    vtByte:      result:=8;
    vtWord:      result:=16;
    vtDWord:     result:=32;
    vtQWord:     result:=64;
    vtSingle:    result:=32;
    vtDouble:    result:=64;
    vtAll:       result:=64;
    vtString:    if stringUnicode then result:=16*stringLength else result:=8*stringLength;
    vtBinary:    result:=binaryLength;
    vtByteArray: result:=arrayLength*8;
    else result:=8;
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

  if SaveFirstScanThread<>nil then
  begin
    SaveFirstScanThread.Terminate;
    SaveFirstScanThread.WaitFor; //wait till it's done
    freeandnil(SaveFirstScanThread);
  end;

  if previousMemoryBuffer<>nil then virtualfree(previousMemoryBuffer,0,MEM_RELEASE);
  fLastscantype:=stNewScan;
end;

procedure TMemscan.NextScan(scanOption: TScanOption; roundingtype: TRoundingType; scanvalue1, scanvalue2: string; startaddress,stopaddress: dword; fastscan,readonly,hexadecimal,binaryStringAsDecimal, unicode, casesensitive: boolean; customscanscript: tstrings; customscantype: TCustomScanType);
begin

  if scanController<>nil then
  begin
    scancontroller.WaitFor; //could be it's still saving the results of the previous scan
    freeandnil(scanController);
  end;

  if SaveFirstScanThread<>nil then
  begin
    SaveFirstScanThread.WaitFor; //wait till it's done
    freeandnil(SaveFirstScanThread);
  end;

  scanController:=TscanController.Create(true);
  scanController.OwningMemScan:=self;
  scanController.scantype:=stNextScan;
  scanController.scanOption:=scanOption;
  scanController.variableType:=CurrentVariableType;
  scanController.roundingtype:=roundingtype;
  scanController.fastscan:=fastscan;
  scanController.scanValue1:=scanvalue1; //usual scanvalue
  scanController.scanValue2:=scanValue2; //2nd value for between scan
  scanController.readonly:=readonly;
  scanController.startaddress:=startaddress;
  scanController.stopaddress:=stopaddress;

  scancontroller.hexadecimal:=hexadecimal;
  scancontroller.binaryStringAsDecimal:=binaryStringAsDecimal;
  scancontroller.unicode:=unicode;
  scancontroller.casesensitive:=casesensitive;
  scancontroller.notifywindow:=notifywindow;
  scancontroller.notifymessage:=notifymessage;
  
  scancontroller.CustomScanScript:=CustomScanScript;
  scanController.CustomScanType:=CustomScanType;

  fLastscantype:=stNextScan;
  scanController.Resume;

end;

procedure TMemscan.firstscan(scanOption: TScanOption; VariableType: TVariableType; roundingtype: TRoundingType; scanvalue1, scanvalue2: string; startaddress,stopaddress: dword; fastscan,readonly,hexadecimal,binaryStringAsDecimal,unicode,casesensitive: boolean; customscanscript: tstrings; customscantype: TCustomScanType);
{
Spawn the controller thread and fill it with the required data
Popup the wait window, or not ?
}
begin
  if scanController<>nil then freeandnil(scanController);
  if SaveFirstScanThread<>nil then
  begin
    SaveFirstScanThread.Terminate; //it should quit, saving took to long and the user already started a new one
    SaveFirstScanThread.WaitFor; //wait till it has fully terminated
    freeandnil(SaveFirstScanThread);
  end;

  currentVariableType:=VariableType;
  
  scanController:=TscanController.Create(true);
  scanController.OwningMemScan:=self;
  scanController.scantype:=stFirstScan;
  scanController.scanOption:=scanOption;
  scanController.variableType:=VariableType;
  scanController.roundingtype:=roundingtype;
  scanController.fastscan:=fastscan;
  scanController.scanValue1:=scanvalue1; //usual scanvalue
  scanController.scanValue2:=scanValue2; //2nd value for between scan
  scanController.readonly:=readonly;
  scanController.startaddress:=startaddress;
  scanController.stopaddress:=stopaddress;

  scancontroller.hexadecimal:=hexadecimal;
  scancontroller.binaryStringAsDecimal:=binaryStringAsDecimal;
  scancontroller.unicode:=unicode;
  scancontroller.casesensitive:=casesensitive;
  scancontroller.notifywindow:=notifywindow;
  scancontroller.notifymessage:=notifymessage;

  scancontroller.CustomScanScript:=CustomScanScript;
  scanController.CustomScanType:=CustomScanType;

  flastscantype:=stFirstScan;
  scanController.Resume;


end;

constructor TMemScan.create(progressbar: TProgressbar; notifywindow: thandle; notifymessage: integer);
begin
  self.progressbar:=progressbar;
  self.notifywindow:=notifywindow;
  self.notifymessage:=notifymessage;
end;
     
destructor TMemScan.destroy;
begin
  if previousMemoryBuffer<>nil then virtualfree(previousMemoryBuffer,0,MEM_RELEASE);
  if SaveFirstScanThread<>nil then SaveFirstScanThread.Free;

  inherited Destroy;
end;

end.
























