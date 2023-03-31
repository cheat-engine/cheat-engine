// Copyright Cheat Engine. All Rights Reserved.

unit memscan;

{$MODE Delphi}

{
This unit will hold the class object used to control scanning
The old scanning routines will be moved out of cefuncproc and made object oriented into this class
Special care should be taken to add multithreaded scanning routines
}



interface


{$ifdef jni}
uses sysutils, unixporthelper, customtypehandler, commonTypeDefs, classes,
     syncobjs, math, groupscancommandparser, NewKernelHandler, strutils,
     savedscanhandler;
{$else}
uses
     {$ifdef darwin}
     macport, macportdefines, LCLType,
     {$endif}
     {$ifdef windows}
     windows,
     {$endif}
     FileUtil, LCLIntf,sysutils, classes,ComCtrls,dialogs, NewKernelHandler,math,
     SyncObjs, SyncObjs2 {$ifdef windows},windows7taskbar{$endif},SaveFirstScan, savedscanhandler, autoassembler,
     symbolhandler, CEFuncProc{$ifdef windows},shellapi{$endif}, CustomTypeHandler, lua,lualib,lauxlib,
     LuaHandler, {$ifdef windows}fileaccess,{$endif} groupscancommandparser, commonTypeDefs, LazUTF8,
     forms, LazFileUtils, LCLProc, LCLVersion, AvgLvlTree, Laz_AVL_Tree;
{$define customtypeimplemented}
{$endif}


type
  TMemScanGuiUpdateRoutine=procedure(sender: TObject; totaladdressestoscan: qword; currentlyscanned: qword; foundcount: qword) of object;
  TCheckRoutine=function(newvalue,oldvalue: pointer):boolean of object;
  TMultiAOBCheckRoutine=function(newvalue: pointer; mabsindex: integer):boolean of object;
  TStoreResultRoutine=procedure(address: ptruint; oldvalue: pointer) of object;
  TFlushRoutine=procedure of object;

  Tscanregionpreference=(scanDontCare, scanExclude, scanInclude);
  TAddresses=array of PtrUInt;


  TPostScanState=(psJustFinished, psOptimizingScanResults, psTerminatingThreads, psSavingFirstScanResults, psShouldBeFinished, psSavingFirstScanResults2);

type
  PAvgLvlTree = ^TAvgLvlTree;

type
  TMemScan=class;
  TScanController=class;
  TScanner=class;

  TGroupData=class  //separate for each scanner object
  private
    is64bit: boolean;

    fblocksize: integer;
    fAlignsize: integer;
    outoforder: boolean;
    outoforder_aligned: boolean;
    groupdata: array of record
      wildcard: boolean;   //if set this is just used for offset calculations for non ooo scans
      offset: integer; //filled during out of order scans to determine if multiple items have been found (If you do OOO scans and use byte and 4 byte together, define byte later...)
      vartype: TVariableType;
      customtype: TCustomtype;
      value: string;
      widevalue: widestring;
      valuei, valuei2: qword;
    //  valuef, valuef2: double;
      minfvalue: double;
      maxfvalue: double;
      floataccuracy: integer;
      range: boolean;
      signed: boolean;

      bytesize: integer;
      pointertypes: TPointerTypes;
    end;

    groupdatalength: integer;  //saves a getLenghth lookup call

    fscanner: TScanner;



    function ByteScan(value: byte; buf: Pbytearray; var startoffset: integer): boolean;
    function ByteScanRange(value,value2: byte; signed: boolean; buf: Pbytearray; var startoffset: integer): boolean;
    function WordScan(value: word; buf: pointer; var startoffset: integer): boolean;
    function WordScanRange(value,value2: word; signed: boolean; buf: pointer; var startoffset: integer): boolean;
    function DWordScan(value: dword; buf: pointer; var startoffset: integer): boolean;
    function DWordScanRange(value,value2: dword; signed: boolean; buf: pointer; var startoffset: integer): boolean;
    function QWordScan(value: qword; buf: pointer; var startoffset: integer): boolean;
    function QWordScanRange(value,value2: qword; signed: boolean; buf: pointer; var startoffset: integer): boolean;
    function Valid32BitPointerScan(value: qword; buf: pointer; var startoffset: integer; pointertypes: TPointerTypes): boolean;
    function Valid64BitPointerScan(value: qword; buf: pointer; var startoffset: integer; pointertypes: TPointerTypes): boolean;
    function SingleScan(minf,maxf: double; buf: pointer; var startoffset: integer): boolean;
    function DoubleScan(minf,maxf: double; buf: pointer; var startoffset: integer): boolean;
    function CustomScan(ct: Tcustomtype; value: integer; buf: pointer; var startoffset: integer): boolean;
    function CustomScanFloat(ct: Tcustomtype; minf, maxf: single; buf: pointer; var startoffset: integer): boolean;

    function StringScan(st: pchar; buf: Pbytearray; var startoffset: integer): boolean;
    function WideStringScan(st: pwidechar; buf: Pbytearray; var startoffset: integer): boolean;


    function testString(buf: PChar; ts: pchar): boolean;
    function testWideString(buf: PWideChar; ts: pwidechar): boolean;

  public
    floatscanWithoutExponents: boolean;
    constructor create(parameters: string; scanner: TScanner);
    function compareblock(newvalue,oldvalue: pointer): boolean; //Check if the values are at their specific offsets
    function compareblock_outoforder(newvalue,oldvalue: pointer): boolean; //Scan the blocks for the values

    //use genericsaveresult for saving the original memory
    //use a custom address file for

    property blocksize: integer read fblocksize;
    property alignsize: integer read fAlignsize;
  end;

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
    scanner: TScanner;
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
    writeError: boolean; //gets set if an exception happened
    errorString: string; //exception description
    procedure execute; override;
    procedure writeresults(addressbuffer,memorybuffer: pointer; addressSize,memorySize: dword); //writes the results of address and memory
    procedure flush;
    constructor create(scanner: TScanner; scancontroller:TScanController; addressfile,memoryfile:TFileStream);
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
    savedscanhandler: Tsavedscanhandler;
    scandir: string;

    L: Plua_State;

    previousmemoryfile: TFilestream;

    found :dword;
    maxfound: dword; //the number of entries before flushing is demanded

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
    nibbleSupport: boolean;

    //multi array of byte. This is for vtByteArrays. vtByteArrays can only be used together with the "OnlyOne" variable. Found in this case means all elements have been found
    mabs: array of record
      arraytofind: TBytes;
      arraylength: integer;
      foundaddress: ptruint; //0 if not found, anything else if found.
    end;
    mabs_arraylength: integer; //optimization

    //all
    typesmatch: array [vtByte..vtDouble] of boolean;  //will get set depending if that type matches the current address or not
    customtypesmatch: array of boolean;
    customtypecount: integer;

    //some variables to hold what types to scan for all (faster then checking the set)
    allByte: boolean;
    allWord: boolean;
    allDword: boolean;
    allQword: boolean;
    allFloat: boolean;
    allDouble: boolean;
    allCustom: boolean;

    floatscanWithoutExponents: boolean;
    inverseScan: boolean;

    //groupdata
    groupdata: TGroupData;

    //custom data
    currentAddress: PtrUInt;

    isStaticPointerLookupTree: TAvgLvlTree;
    isDynamicPointerLookupTree: TAvgLvlTree;
    isExecutablePointerLookupTree: TAvgLvlTree;

    //check routines:

    function Unknown(newvalue,oldvalue: pointer): boolean;

    function ByteExact(newvalue,oldvalue: pointer): boolean;
    function ByteBetween(newvalue,oldvalue: pointer): boolean;
    function SignedByteBetween(newvalue,oldvalue: pointer): boolean;
    function ByteBetweenPercentage(newvalue,oldvalue: pointer): boolean;
    function ByteBiggerThan(newvalue,oldvalue: pointer): boolean;
    function ByteSmallerThan(newvalue,oldvalue: pointer): boolean;
    function ByteIncreasedValue(newvalue,oldvalue: pointer): boolean;
    function ByteIncreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function ByteIncreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
    function ByteDecreasedValue(newvalue,oldvalue: pointer): boolean;
    function ByteDecreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function ByteDecreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
    function ByteChanged(newvalue,oldvalue: pointer): boolean;
    function ByteUnChanged(newvalue,oldvalue: pointer): boolean;
    function ByteLuaFormula(newvalue,oldvalue: pointer): boolean;

    function WordExact(newvalue,oldvalue: pointer): boolean;
    function WordBetween(newvalue,oldvalue: pointer): boolean;
    function SignedWordBetween(newvalue,oldvalue: pointer): boolean;
    function WordBetweenPercentage(newvalue,oldvalue: pointer): boolean;
    function WordBiggerThan(newvalue,oldvalue: pointer): boolean;
    function WordSmallerThan(newvalue,oldvalue: pointer): boolean;
    function WordIncreasedValue(newvalue,oldvalue: pointer): boolean;
    function WordIncreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function WordIncreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
    function WordDecreasedValue(newvalue,oldvalue: pointer): boolean;
    function WordDecreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function WordDecreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
    function WordChanged(newvalue,oldvalue: pointer): boolean;
    function WordUnChanged(newvalue,oldvalue: pointer): boolean;
    function WordLuaFormula(newvalue,oldvalue: pointer): boolean;

    function DWordExact(newvalue,oldvalue: pointer): boolean;
    function DWordBetween(newvalue,oldvalue: pointer): boolean;
    function SignedDWordBetween(newvalue,oldvalue: pointer): boolean;
    function DWordBetweenPercentage(newvalue,oldvalue: pointer): boolean;
    function DWordBiggerThan(newvalue,oldvalue: pointer): boolean;
    function DWordSmallerThan(newvalue,oldvalue: pointer): boolean;
    function DWordIncreasedValue(newvalue,oldvalue: pointer): boolean;
    function DWordIncreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function DWordIncreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
    function DWordDecreasedValue(newvalue,oldvalue: pointer): boolean;
    function DWordDecreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function DWordDecreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
    function DwordChanged(newvalue,oldvalue: pointer): boolean;
    function DwordUnChanged(newvalue,oldvalue: pointer): boolean;
    function DWordLuaFormula(newvalue,oldvalue: pointer): boolean;

    function QWordExact(newvalue,oldvalue: pointer): boolean;
    function QWordBetween(newvalue,oldvalue: pointer): boolean;
    function SignedQWordBetween(newvalue,oldvalue: pointer): boolean;
    function QWordBetweenPercentage(newvalue,oldvalue: pointer): boolean;
    function QWordBiggerThan(newvalue,oldvalue: pointer): boolean;
    function QWordSmallerThan(newvalue,oldvalue: pointer): boolean;
    function QWordIncreasedValue(newvalue,oldvalue: pointer): boolean;
    function QWordIncreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function QWordIncreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
    function QWordDecreasedValue(newvalue,oldvalue: pointer): boolean;
    function QWordDecreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function QWordDecreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
    function QWordChanged(newvalue,oldvalue: pointer): boolean;
    function QwordUnChanged(newvalue,oldvalue: pointer): boolean;
    function QWordLuaFormula(newvalue,oldvalue: pointer): boolean;

    function SingleExact(newvalue,oldvalue: pointer): boolean;
    function SingleBetween(newvalue,oldvalue: pointer): boolean;
    function SingleBetweenPercentage(newvalue,oldvalue: pointer): boolean;
    function SingleBiggerThan(newvalue,oldvalue: pointer): boolean;
    function SingleSmallerThan(newvalue,oldvalue: pointer): boolean;
    function SingleIncreasedValue(newvalue,oldvalue: pointer): boolean;
    function SingleIncreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function SingleIncreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
    function SingleDecreasedValue(newvalue,oldvalue: pointer): boolean;
    function SingleDecreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function SingleDecreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
    function SingleChanged(newvalue,oldvalue: pointer): boolean;
    function singleUnChanged(newvalue,oldvalue: pointer): boolean;
    function SingleLuaFormula(newvalue,oldvalue: pointer): boolean;

    function DoubleExact(newvalue,oldvalue: pointer): boolean;
    function DoubleBetween(newvalue,oldvalue: pointer): boolean;
    function DoubleBetweenPercentage(newvalue,oldvalue: pointer): boolean;
    function DoubleBiggerThan(newvalue,oldvalue: pointer): boolean;
    function DoubleSmallerThan(newvalue,oldvalue: pointer): boolean;
    function DoubleIncreasedValue(newvalue,oldvalue: pointer): boolean;
    function DoubleIncreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function DoubleIncreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
    function DoubleDecreasedValue(newvalue,oldvalue: pointer): boolean;
    function DoubleDecreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function DoubleDecreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
    function DoubleChanged(newvalue,oldvalue: pointer): boolean;
    function DoubleUnChanged(newvalue,oldvalue: pointer): boolean;
    function DoubleLuaFormula(newvalue,oldvalue: pointer): boolean;

    function AllUnknown(newvalue,oldvalue: pointer):boolean; //check byte,word,dword,qword,single and float
    function AllExact(newvalue,oldvalue: pointer):boolean; //check byte,word,dword,qword,single and float
    function AllBetween(newvalue,oldvalue: pointer): boolean;
    function SignedAllBetween(newvalue,oldvalue: pointer): boolean;
    function AllBetweenPercentage(newvalue,oldvalue: pointer): boolean;
    function AllBiggerThan(newvalue,oldvalue: pointer): boolean;
    function AllSmallerThan(newvalue,oldvalue: pointer): boolean;
    function AllIncreasedValue(newvalue,oldvalue: pointer): boolean;
    function AllIncreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function AllIncreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
    function AllDecreasedValue(newvalue,oldvalue: pointer): boolean;
    function AllDecreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function AllDecreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
    function AllChanged(newvalue,oldvalue: pointer): boolean;
    function AllUnchanged(newvalue,oldvalue: pointer): boolean;
    function AllLuaFormula(newvalue,oldvalue: pointer):boolean; //check byte,word,dword,qword,single and float

    function CustomExact(newvalue,oldvalue: pointer): boolean;
    function CustomBetween(newvalue,oldvalue: pointer): boolean;
    function SignedCustomBetween(newvalue,oldvalue: pointer): boolean;
    function CustomBetweenPercentage(newvalue,oldvalue: pointer): boolean;
    function CustomBiggerThan(newvalue,oldvalue: pointer): boolean;
    function CustomSmallerThan(newvalue,oldvalue: pointer): boolean;
    function CustomIncreasedValue(newvalue,oldvalue: pointer): boolean;
    function CustomIncreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function CustomIncreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
    function CustomDecreasedValue(newvalue,oldvalue: pointer): boolean;
    function CustomDecreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function CustomDecreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
    function CustomChanged(newvalue,oldvalue: pointer): boolean;
    function CustomUnChanged(newvalue,oldvalue: pointer): boolean;
    function CustomLuaFormula(newvalue,oldvalue: pointer): boolean;

    function CustomFloatExact(newvalue,oldvalue: pointer): boolean;
    function CustomFloatBetween(newvalue,oldvalue: pointer): boolean;
    function CustomFloatBetweenPercentage(newvalue,oldvalue: pointer): boolean;
    function CustomFloatBiggerThan(newvalue,oldvalue: pointer): boolean;
    function CustomFloatSmallerThan(newvalue,oldvalue: pointer): boolean;
    function CustomFloatIncreasedValue(newvalue,oldvalue: pointer): boolean;
    function CustomFloatIncreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function CustomFloatIncreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
    function CustomFloatDecreasedValue(newvalue,oldvalue: pointer): boolean;
    function CustomFloatDecreasedValueBy(newvalue,oldvalue: pointer): boolean;
    function CustomFloatDecreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
    function CustomFloatChanged(newvalue,oldvalue: pointer): boolean;
    function CustomFloatUnChanged(newvalue,oldvalue: pointer): boolean;
    function CustomFloatLuaFormula(newvalue,oldvalue: pointer): boolean;


    function ArrayOfBytesExact_NibbleWildcardSupport(newvalue: pointer; mabsindex: integer):boolean;
    function ArrayOfBytesExact(newvalue: pointer; mabsindex: integer):boolean;

    //following types only have exact: Array of byte, binary and string
    function ArrayOfByteExact_NibbleWildcardSupport(newvalue,oldvalue: pointer):boolean;
    function ArrayOfByteExact(newvalue,oldvalue: pointer):boolean;
    function BinaryExact(newvalue,oldvalue: pointer):boolean;
    function CaseSensitiveAnsiStringExact(newvalue,oldvalue: pointer):boolean;
    function CaseInsensitiveAnsiStringExact(newvalue,oldvalue: pointer):boolean;
    function CaseSensitiveUnicodeStringExact(newvalue,oldvalue: pointer):boolean;
    function CaseInsensitiveUnicodeStringExact(newvalue,oldvalue: pointer):boolean;

    function CustomCaseSensitiveAnsiStringExact(newvalue,oldvalue: pointer):boolean;
    function CustomCaseInsensitiveAnsiStringExact(newvalue,oldvalue: pointer):boolean;


    //save macthing address routines:
    procedure GenericSaveResult(address: ptruint; oldvalue: pointer); //only use as last resort : Call to copymemory just to store one entry
    procedure allSaveResult(address: ptruint; oldvalue: pointer);
    procedure binarySaveResult(address: ptruint; oldvalue: pointer);
    procedure groupSaveResult(address: ptruint; oldvalue: pointer);
    procedure arrayOfByteSaveResult(address: ptruint; oldvalue: pointer);
    procedure ByteSaveResult(address: ptruint; oldvalue: pointer);
    procedure WordSaveResult(address: ptruint; oldvalue: pointer);
    procedure DWordSaveResult(address: ptruint; oldvalue: pointer);
    procedure QWordSaveResult(address: ptruint; oldvalue: pointer);
    procedure SingleSaveResult(address: ptruint; oldvalue: pointer);
    procedure DoubleSaveResult(address: ptruint; oldvalue: pointer);

    //flush routines
    procedure genericFlush; //generic routine for flushing the buffer
    procedure stringFlush; //don't save the memory
    procedure binaryFlush; //don't save memory AND use foundaddressb for results
    procedure groupFlush; //don't save memory
    procedure allFlush;


    procedure configurescanroutine; //parses scanvalue1,2 according to the VariableType and scanoptions
    procedure FirstScanmem(base:ptruint; buffer: pointer; size: integer); //routine that gets a buffer and saves all the results it finds in the buffer (For firstscan)
    procedure FirstNextScanmem(base:ptruint; buffer,oldbuffer: pointer; size: integer);

    procedure nextnextscanmemall(addresslist: pointer; oldmemory: pointer; chunksize: integer);
    procedure nextnextscanmembinary(addresslist: pointer; chunksize: integer);
    procedure nextnextscanmem(addresslist: pointer; oldmemory: pointer; chunksize: integer);

    procedure firstscan; //copy the given range to the memory region
    procedure firstnextscan; //routine used when the results list contains nothing but a indicator a unknown scan was done
    procedure nextnextscan; //routine used when the results list contains addresses

  public
    OwningScanController: TScanController;
    Addressfile: TFilestream; //tempscandir+'Addresses'+ThreadID.TMP'
    MemoryFile: TFileStream;  //tempscandir+'Memory'+ThreadID.TMP'
    Addressfilename: string;
    MemoryFilename: string;

    roundingtype: TRoundingtype;
    hexadecimal: boolean;
    binaryStringAsDecimal: boolean;

    PreviousOffsetCount: integer; //holds the offsecount of the previous scan (for calculating the entry position)

    luaformula: boolean;
    newluastate: boolean;
    unicode: boolean;
    caseSensitive: boolean;
    percentage: boolean;
    fastscanalignsize: integer;
    fastscanmethod: TFastScanMethod;
    fastscandigitcount: integer;
    variablesize: integer;
    scanvalue1,scanvalue2: string;
    widescanvalue1: widestring;

    compareToSavedScan: boolean;
    savedscanname: string;

    scanOption: TScanOption;
    variableType: TVariableType;
    customType: TCustomType;
    scanType: TScanType; //defines if it's a firstscan or next scan. (newscan is ignored)
    useNextNextscan: boolean; //determines to use the nextNextScan or firstNextScan

    //thread controlling variables:
    isdone: boolean; //will get set to true when the thread finishes normally
    haserror: boolean;
    errorstring: string;

    //region related scans:
    //startregion and stopregion
    _startregion: integer;
    _stopregion: integer;
    maxregionsize: qword; //max size of buffer to be allocated when not unknown scan

    //recreated memory region list for this specific range, can be used to see which regions where only half read
    memRegions: TMemoryregions;
    memRegionPos: integer;

    startaddress: PtrUint; //specific start for this this thread
    stopaddress: PtrUint; //specific stop for this thread, if not fastscan and another thread continue from here, may add some overlopping bytes

    //exact address scans:
    startentry: qword; //index in the address list
    stopentry: qword; //"   "

    //general:
    scanned: qword; //total memory/addresses scanned by this routine
    totalfound: dword;

    OnlyOne: boolean;
    AddressFound: PtrUInt;
    scannernr: integer;

    lastpart: integer;

    procedure execute; override;
    constructor create(suspended: boolean; scandir: string);
    destructor destroy; override;
  end;

  TVQEValidCacheEntry=class
  private
    address: ptruint;
    size: size_t;
    valid: boolean;
    function containsaddress(a: ptruint): boolean;
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
    isdoneEvent: TEvent; //gets set when the scan has finished
    isReallyDoneEvent: TEvent; //gets set when the results have been completely written

    isStaticPointerLookupTree: TAvgLvlTree; //init once and reuse by all threads
    isDynamicPointerLookupTree: TAvgLvlTree; // same ^
    isExecutablePointerLookupTree: TAvgLvlTree; // same ^

    vqevalidcache: TAvgLvlTree;
    vqecache_lastregion: TVQEValidCacheEntry;

    function isValidregion(address: ptruint): boolean;
    procedure FillPointerLookupTrees(pointertypes: TPointertypes);
    function isPointer(address: ptruint; pointertypes: TPointerTypes): boolean;
    procedure CleanupIsPointerLookupTree(var lookupTree: TAvgLvlTree);
    procedure CleanupIsPointerLookupTrees;

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

    totalAddresses: qword; //how many addresses it will have to scan

    scanWritable: Tscanregionpreference;
    scanExecutable: Tscanregionpreference;
    scanCopyOnWrite: Tscanregionpreference;
    {$ifdef darwin}
    scanDirty: Tscanregionpreference;
    {$endif}

    roundingtype: TRoundingType;
    hexadecimal: boolean;
    binaryStringAsDecimal: boolean;

    fastscan: boolean;
    fastscanalignment: integer;
    fastscanmethod: TFastscanmethod;
    fastscandigitcount: integer;
    unicode: boolean;
    casesensitive: boolean;
    percentage: boolean;
    fastscanalignsize: integer;
    variablesize: integer;
    scanvalue1,scanvalue2: string;

    startaddress: ptruint; //start for the whole scan
    stopaddress: ptruint; //stop of the whole scan

    compareToSavedScan: boolean;
    savedscanname: string;

    scanOption: TScanOption;
    variableType: TVariableType;
    customType: TCustomType;
    scanType: TScanType; //defines if it's a firstscan or next scan. (newscan is ignored)

    //memregion info
    memregion: TMemoryregions;  //scanners have access to this, but make sure to NOT WRITE it
    memRegionPos: Integer;



    //thread controlling variables:
    isdone: boolean; //will get set to true when the thread finishes normally
    haserror: boolean;
    errorstring: string;

    //messages
    //notifywindow: thandle;
    //notifymessage: integer;

    //OnlyOne vars
    OnlyOne: boolean;
    FoundSomething: Boolean;
    AddressFound: ptruint;
    AddressesFound: TAddresses; //for multi aob scans

    floatscanWithoutExponents: boolean;
    inverseScan: boolean;
    luaformula: boolean;
    newluastate: boolean;
    isUnique: boolean;

    workingsetonly: boolean;

    procedure execute; override;
    constructor create(suspended: boolean);
    destructor destroy; override;
  end;

  TMemScan=class
  {
    Configures the gui and related objects and launch TScanner objects with those objects
  }
  private
    {$ifndef lowmemoryusage}
    previousMemoryBuffer: pointer;
    {$endif}

    scanController: TScanController; //thread that configures the scanner threads and wait till they are done
    {$IFNDEF lowmemoryusage}
    SaveFirstScanThread: TSaveFirstScanThread; //thread that will save the results of the first scan that can be used for "same as first scan" scans
    {$ENDIF}
    memRegion: TMemoryRegions;  //after a scan the contents of controller gets copied to here
    memRegionPos: integer;

    progressbar: TCustomProgressBar;
    //notifywindow: thandle;
   // notifymessage: integer;

    found: uint64;

    //fastscan options (only set by firstscan)
    //Alignment: integer;
    fastscanalignment: integer;
    fastscandigitcount: integer;

    //string stuff:
    stringUnicode: boolean;
    stringLength:  integer;

    //binary stuff:
    binaryLength:  integer;

    //array stuff:
    arrayLength:   integer;

    fLastScanType: TScanType;
    fLastScanValue: String;
    fscanresultfolder: string; //the location where all the scanfiles will be stored

    fCodePage: boolean;
    fLuaFormula: boolean;
    fNewLuaState: boolean;

    fnextscanCount: integer;


    savedresults: tstringlist;
    fonlyOne: boolean;
    fIsUnique: boolean;

    fworkingsetonly: boolean;


    ffloatscanWithoutExponents: boolean;
    fInverseScan: boolean;
    fGUIScanner: boolean;
    fbusyformIsModal: boolean;

    //scan params
    fScanOption: TScanoption;
    fVariableType: TVariableType;
    froundingtype: TRoundingType;
    fscanvalue1: string;
    fscanvalue2: string;
    fstartaddress: ptruint;
    fstopaddress: ptruint;
    fhexadecimal: boolean;
    fbinaryStringAsDecimal: boolean;
    funicode: boolean;
    fcasesensitive: boolean;
    ffastscanmethod: TFastScanMethod;
    ffastscanparameter: string;
    fcustomtype: TCustomType;

    //next scan params
    fpercentage: boolean;
    fcompareToSavedScan: boolean;
    fsavedscanname: string;

    fscanWritable: Tscanregionpreference;
    fscanExecutable: Tscanregionpreference;
    fscanCopyOnWrite: Tscanregionpreference;


    procedure DeleteScanfolder;
    procedure createScanfolder;
    function DeleteFolder(dir: string) : boolean;
    procedure setVariableType(t: TVariableType);
    function getSavedScanCount: integer;
  protected
    fOnScanStart: TNotifyEvent;
    fOnScanDone: TNotifyEvent;
    fOnInitialScanDone: TNotifyEvent;
    fOnGuiUpdate: TMemScanGuiUpdateRoutine;
    procedure ScanDone; virtual; //called by the scancontroller
    procedure InitialScanDone; virtual;
  public
    postScanState: TPostScanState;



    {$ifdef darwin}
    scanDirty: Tscanregionpreference;
    {$endif}

    attachedFoundlist: TObject;


    function GetLastScanWasRegionScan: boolean;

    procedure parseProtectionflags(protectionflags: string);
    function GetProgress(var totaladdressestoscan:qword; var currentlyscanned: qword; var resultsfound: qword):integer;
    function hasError: boolean;
    function GetErrorString: string;
    function GetFoundCount: uint64;
    function Getbinarysize: int64; //returns the number of bits of the current type
    function GetOnlyOneResult(var address: ptruint):boolean;
    function GetOnlyOneResults(var addresses: Taddresses):boolean; //multi aob version
    function GetScanFolder: string;
    procedure TerminateScan(forceTermination: boolean);
    procedure newscan; //will clean up the memory and files
    procedure firstscan(_scanOption: TScanOption; _VariableType: TVariableType; _roundingtype: TRoundingType; _scanvalue1, _scanvalue2: string; _startaddress,_stopaddress: ptruint; _hexadecimal,_binaryStringAsDecimal,_unicode,_casesensitive: boolean; _fastscanmethod: TFastScanMethod=fsmNotAligned; _fastscanparameter: string=''; _customtype: TCustomType=nil); overload;
    procedure NextScan(_scanOption: TScanOption; _roundingtype: TRoundingType; _scanvalue1, _scanvalue2: string; _hexadecimal,_binaryStringAsDecimal, _unicode, _casesensitive, _percentage, _compareToSavedScan: boolean; _savedscanname: string); overload; //next scan, determine what kind of scan and give to firstnextscan/nextnextscan
    procedure FirstScan; overload;
    procedure NextScan; overload;
    function waittilldone(timeout: dword=INFINITE): boolean;
    function waittillreallydone(timeout: dword=INFINITE): boolean;

//    procedure setScanDoneCallback(notifywindow: thandle; notifymessage: integer);

    function canUndo: boolean;
    procedure undoLastScan;

    constructor create(progressbar: TCustomProgressbar);
    destructor destroy; override;

    procedure saveresults(resultname: string);
    function getsavedresults(r: tstrings): integer;
    function deleteSavedResult(resultname: string): boolean;

    function canWriteResults: boolean;


    property nextscanCount: integer read fnextscanCount;
  published
    property GUIScanner: Boolean read fGUIScanner write fGUIScanner;
    property inverseScan: boolean read fInverseScan write fInverseScan;
    property floatscanWithoutExponents: boolean read ffloatscanWithoutExponents write ffloatscanWithoutExponents;
    property OnlyOne: boolean read fOnlyOne write fOnlyOne;
    property VarType: TVariableType read fVariableType write setVariableType;
    property codePage: boolean read fCodePage write fCodePage;
    property LuaFormula: boolean read fLuaFormula write fLuaFormula;
    property NewLuaState: boolean read fNewLuaState write fNewLuaState;
    property isUnique: boolean read fIsUnique write fIsUnique; //for AOB scans only
    property lastScanWasRegionScan: boolean read getLastScanWasRegionScan;
    property isUnicode: boolean read stringUnicode;
    property isHexadecimal: boolean read fHexadecimal; //gui
    property LastScanValue: string read fLastScanValue;
    property LastScanType: TScanType read FLastScanType;
    property ScanresultFolder: string read fScanResultFolder; //read only, it's configured during creation
    property BusyformIsModal: boolean read fbusyformIsModal write fbusyformIsModal;
    property OnScanDone: TNotifyEvent read fOnScanDone write fOnScanDone;
    property OnScanStart: TNotifyEvent read fOnScanStart write fOnScanStart;
    property OnInitialScanDone: TNotifyEvent read fOnInitialScanDone write fOnInitialScanDone;
    property OnGuiUpdate: TMemscanGuiUpdateRoutine read fOnGuiUpdate write fOnGuiUpdate;

    //scan properties
    property ScanOption: TScanoption read fScanOption write fScanOption;
    property VariableType: TVariableType read fVariableType write setVariableType;
    property Roundingtype: TRoundingType read froundingtype write froundingtype;
    property Scanvalue: string read fscanvalue1 write fscanvalue1;
    property Scanvalue1: string read fscanvalue1 write fscanvalue1;
    property Scanvalue2: string read fscanvalue2 write fscanvalue2;
    property Startaddress: ptruint read fstartaddress write fstartaddress;
    property Stopaddress: ptruint read fstopaddress write fstopaddress;
    property Hexadecimal: boolean read fhexadecimal write fhexadecimal;
    property BinaryStringAsDecimal: boolean read fbinaryStringAsDecimal write fbinaryStringAsDecimal;
    property Unicode: boolean read funicode write funicode;
    property UTF16: boolean read funicode write funicode;
    property Casesensitive: boolean read fcasesensitive write fcasesensitive;
    property Fastscanmethod: TFastScanMethod read ffastscanmethod write ffastscanmethod;
    property Fastscanparameter: string read ffastscanparameter write ffastscanparameter;
    property Customtype: TCustomType read fcustomtype write fcustomtype;
    property WorkingSetOnly: boolean read fworkingsetonly write fworkingsetonly;
    property PresentOnly: boolean read fworkingsetonly write fworkingsetonly;


    //next scan specific:
    property Percentage: boolean read fPercentage write fPercentage;
    property CompareToSavedScan: boolean read fcompareToSavedScan write fcompareToSavedScan;
    property SavedScanName: string read fsavedscanname write fsavedscanname;
    property SavedScanCount: integer read getSavedScanCount;

    property scanWritable: Tscanregionpreference read fscanWritable write fscanWritable;
    property scanExecutable: Tscanregionpreference read fscanExecutable write fscanExecutable;
    property scanCopyOnWrite: Tscanregionpreference read fscanCopyOnWrite write fscanCopyOnWrite;

//    property percentage
  end;





implementation


{$ifdef android}
uses ProcessHandlerUnit, parsers, Globals;
{$else}
uses formsettingsunit, StrUtils, foundlisthelper, ProcessHandlerUnit, parsers,
     Globals, frmBusyUnit, controls, mainunit2;
{$endif}

resourcestring
  rsIsNotAValidCharacterInsideABinaryString = '%s is not a valid character inside a binary string';
  rsInvalidBinaryNotation = 'Invalid binary notation';
  rsThreadSynchronizer = 'Thread synchronizer';
  rsUnknown = 'Unknown';
  rsPleaseFillSomethingIn = 'Please fill something in';
  rsIsNotAValidValue = '%s is not a valid value';
  rsIsAnInvalidValue = '%s is an invalid value';
  rsIsNotAValidNotation = '%s is an invalid notation';
  rsErrorAllocatingBytesForTheOldResults = 'Error allocating %s bytes for the old results. buffersize=%s variablesize=%s';
  rsDiskWriteError = 'Disk write error:%s';
  rsNoReadableMemoryFound = 'No readable memory found';
  rsFailureAllocatingMemoryForCopyTriedAllocatingKB = 'Failure allocating memory for copy. Tried allocating %s KB';
  rsErrorWhenWhileLoadingResult = 'Error while loading result';
  rsNotEnoughDiskspaceForTheAddressFile = 'Not enough diskspace for the address file';
  rsNotEnoughDiskspaceForTheMemoryFile = 'Not enough diskspace for the memory file';
  rsDiskWriteError2 = 'Disk Write Error:%s';
  rsFailedSpawningTheSaveFirstScanThread = 'Failed spawning the Save First Scan thread';
  rsForSomeReasonTheScanControllerDoesNotExist = 'For some reason the scanController does not exist';
  rsAResultSetWithThisNameAlreadyExists = 'A result set with this name(%s) already exists';
  rsTMPAndUNDOAreNamesThatMayNotBeUsedTryAnotherName = 'TMP and UNDO are names that may not be used. Try another name';
  rsTheTemporaryScanDirectoryDoesNotExistCheckYourScan = 'The temporary scan directory %s does not exist. Check your scan settings';
  rsFailureCreatingTheScanDirectory = 'Failure creating the scan directory';
  rsMSNothingToScanFor = 'Nothing to scan for';
  rsMStupidAlignsize = 'Stupid alignsize';
  rsMSCustomTypeIsNil = 'Custom type is nil';
  rsMSTheScanWasForcedToTerminateSubsequentScansMayNotFunctionProperlyEtc = 'The scan was forced to terminate. Subsequent scans may not function properly. It''s recommended to restart '+strCheatEngine;
  rsThread = 'thread ';
  rsMSPointerTypeNotRecognised = 'Pointer type not recognised: ';
//===============Local functions================//
function getBytecountArrayOfByteString(st: string): integer;
var bytes: tbytes;
begin
  ConvertStringToBytes(st,true,bytes);
  result:=length(bytes);
end;


function getBytecountBinaryString(st:string; scanvalueisdecimal: boolean): integer;
var i: integer;
begin
  if scanvalueisdecimal then //first convert to binarystring
    st:=parsers.inttobin(strtoint(st));


  result:=0;
  for i:=1 to length(st) do
  begin
    case st[i] of
      '0','1','?','*': inc(result);
      ' ',#8: ; //ignore
      else raise exception.Create(Format(rsIsNotAValidCharacterInsideABinaryString, [st[i]]));
    end;
  end;

  if result=0 then raise exception.Create(rsInvalidBinaryNotation);
  if (result>1) then
  begin
    if (result mod 8=0) then
      result:=1+result div 8
    else
      result:=2+(result div 8);
  end;

end;

//==================TGroupData===============//

constructor TGroupData.create(parameters: string; scanner: TScanner);
//todo: convert groupscancommandparser to unix
{$ifndef jni}
var start, i: integer;
  p,s: string;

  gcp: TGroupscanCommandParser;

  floatsettings: TFormatSettings;

  fvalue: double;
  tempq: qword;
{$endif}
begin
{$ifndef jni}
  is64bit:=processhandler.is64Bit;
  floatsettings:=DefaultFormatSettings;
  fscanner:=scanner;

  gcp:=TGroupscanCommandParser.create;
  try
    gcp.parse(parameters);
    fblocksize:=gcp.blocksize;
    fAlignsize:=gcp.blockalignment;

    if fblocksize<=0 then
      raise exception.create(rsMSNothingToScanFor);

    if fAlignsize<=0 then
      raise exception.create(rsMStupidAlignsize);


    outoforder:=gcp.outOfOrder;
    outoforder_aligned:=gcp.typeAligned;

    groupdatalength:=length(gcp.elements);
    setlength(groupdata, groupdatalength);
    for i:=0 to Length(gcp.elements)-1 do
    begin
      groupdata[i].wildcard:=gcp.elements[i].wildcard;
      groupdata[i].offset:=gcp.elements[i].offset;
      groupdata[i].vartype:=gcp.elements[i].vartype;
      groupdata[i].customtype:=gcp.elements[i].customtype;
      groupdata[i].valuei:=gcp.elements[i].valueint;
     // groupdata[i].valuef:=gcp.elements[i].valuefloat;

      groupdata[i].range:=gcp.elements[i].range;
      if groupdata[i].range then
      begin
        groupdata[i].valuei2:=gcp.elements[i].valueint2;
        groupdata[i].minfvalue:=gcp.elements[i].valuefloat;
        groupdata[i].maxfvalue:=gcp.elements[i].valuefloat2;
        groupdata[i].signed:=gcp.elements[i].signed;


      end
      else
      begin
        fvalue:=gcp.elements[i].valuefloat;
        groupdata[i].floataccuracy:=pos(gcp.FloatSettings.DecimalSeparator,gcp.elements[i].uservalue);
        if groupdata[i].floataccuracy>0 then
          groupdata[i].floataccuracy:=length(gcp.elements[i].uservalue)-groupdata[i].floataccuracy;

        groupdata[i].minfvalue:=fvalue-(1/(power(10,groupdata[i].floataccuracy)));
        groupdata[i].maxfvalue:=fvalue+(1/(power(10,groupdata[i].floataccuracy)));
      end;

      groupdata[i].value:=uppercase(gcp.elements[i].uservalue);
      groupdata[i].widevalue:=UnicodeUpperCase(gcp.elements[i].uservalue);

      groupdata[i].bytesize:=gcp.elements[i].bytesize;

      groupdata[i].pointertypes:=gcp.elements[i].pointertypes;
    end;



  finally
    gcp.free;
  end;

{$endif}


end;

function TGroupData.testString(buf: PChar; ts: pchar): boolean;
var i: integer;
begin
  result:=false;
  for i:=0 to length(ts)-1 do
  begin
    if pchar(buf)[i] in ['a'..'z'] then //change to uppercase
      dec(pbytearray(buf)[i],$20);

    if ts[i]<>(pchar(buf)[i]) then exit;
  end;

  result:=true;
end;


function TGroupData.testWideString(buf: PWideChar; ts: pwidechar): boolean;
var i: integer;
begin
  result:=false;
  i:=0;

  for i:=0 to length(ts)-1 do
  begin
    if pchar(buf)[i*sizeof(wchar)] in ['a'..'z'] then //change to uppercase
      dec(pbytearray(buf)[i*sizeof(wchar)],$20);

    if ts[i]<>(pwidechar(buf)[i]) then exit;
  end;

  result:=true;
end;

function TGroupData.compareblock(newvalue,oldvalue: pointer): boolean;
//ordered scan
var i: integer;
  f: single;
  s: string;
begin
  result:=true;
  for i:=0 to groupdatalength-1 do
  begin
    if result=false then exit;

    case groupdata[i].vartype of
      vtByte:
      begin
        result:=groupdata[i].wildcard or
                (not groupdata[i].range and (pbyte(newvalue)^=byte(groupdata[i].valuei))) or
                (groupdata[i].range and
                   (groupdata[i].signed and (PSmallInt(newvalue)^>=smallint(groupdata[i].valuei)) and (PSmallInt(newvalue)^<=smallint(groupdata[i].valuei2))) or
                   (not groupdata[i].signed and (PByte(newvalue)^>=Byte(groupdata[i].valuei)) and (PByte(newvalue)^<=Byte(groupdata[i].valuei2)))
                );

        inc(newvalue, 1);
      end;

      vtWord:
      begin
        result:=groupdata[i].wildcard or
                (not groupdata[i].range and (pword(newvalue)^=word(groupdata[i].valuei))) or
                (groupdata[i].range and
                   (groupdata[i].signed and (PShortint(newvalue)^>=Shortint(groupdata[i].valuei)) and (PShortint(newvalue)^<=Shortint(groupdata[i].valuei2))) or
                   (not groupdata[i].signed and (PWord(newvalue)^>=Word(groupdata[i].valuei)) and (PWord(newvalue)^<=Word(groupdata[i].valuei2)))
                );
        inc(newvalue, 2);
      end;

      vtDWord:
      begin
        result:=groupdata[i].wildcard or
                (not groupdata[i].range and (pdword(newvalue)^=dword(groupdata[i].valuei))) or
                (groupdata[i].range and
                   (groupdata[i].signed and (Pinteger(newvalue)^>=integer(groupdata[i].valuei)) and (Pinteger(newvalue)^<=integer(groupdata[i].valuei2))) or
                   (not groupdata[i].signed and (PDWord(newvalue)^>=DWord(groupdata[i].valuei)) and (PDWord(newvalue)^<=DWord(groupdata[i].valuei2)))
                );
        inc(newvalue, 4);
      end;

      vtQWord:
      begin
        result:=groupdata[i].wildcard or
                (not groupdata[i].range and (pqword(newvalue)^=qword(groupdata[i].valuei))) or
                (groupdata[i].range and
                   (groupdata[i].signed and (Pint64(newvalue)^>=int64(groupdata[i].valuei)) and (Pint64(newvalue)^<=int64(groupdata[i].valuei2))) or
                   (not groupdata[i].signed and (PQWord(newvalue)^>=QWord(groupdata[i].valuei)) and (PQWord(newvalue)^<=QWord(groupdata[i].valuei2)))
                );
        inc(newvalue, 8);
      end;

      vtSingle:
      begin
        result:=groupdata[i].wildcard or ((psingle(newvalue)^>=groupdata[i].minfvalue) and (psingle(newvalue)^<=groupdata[i].maxfvalue)); //default extreme rounded

        if result and (floatscanWithoutExponents and (pdword(newvalue)^>0) and (abs(127-(pdword(newvalue)^ shr 23) and $ff)>10)) then
          result:=false;

        inc(newvalue, 4);
      end;

      vtDouble:
      begin
        result:=groupdata[i].wildcard or ((pdouble(newvalue)^>=groupdata[i].minfvalue) and (pdouble(newvalue)^<=groupdata[i].maxfvalue));

        if result and (floatscanWithoutExponents and (pqword(newvalue)^>0) and (abs(integer(1023-(pqword(newvalue)^ shr 52) and $7ff))>10)) then
          result:=false;

        inc(newvalue, 8);
      end;

      vtString:
      begin
        result:=groupdata[i].wildcard or testString(newvalue, @groupdata[i].value[1]);
        inc(newvalue, groupdata[i].bytesize);
      end;

      vtUnicodeString:
      begin
        result:=groupdata[i].wildcard or testWideString(newvalue, @groupdata[i].widevalue[1]);
        inc(newvalue, groupdata[i].bytesize);
      end;

      vtPointer: //only vtPointer if it's a wildcard pointer
      begin
        if is64bit then
          result:=fscanner.OwningScanController.isPointer(pqword(newvalue)^, groupdata[i].pointertypes)
        else
          result:=fscanner.OwningScanController.isPointer(pdword(newvalue)^, groupdata[i].pointertypes);

        inc(newvalue, groupdata[i].bytesize);
      end;
      //todo: Convert customtype to unix
      {$ifdef customtypeimplemented}

      vtCustom:
      begin
        if groupdata[i].customType.scriptUsesString then
        begin
          s:=groupdata[i].customtype.ConvertDataToString(newvalue, fscanner.currentAddress);
          result:=groupdata[i].wildcard or testString(pchar(s), @groupdata[i].value[1]);
        end
        else
        if groupdata[i].customType.scriptUsesFloat then
        begin
          f:=groupdata[i].customType.ConvertDataToFloat(newvalue, fscanner.currentAddress);
          result:=groupdata[i].wildcard or ((f>groupdata[i].minfvalue) and (f<groupdata[i].maxfvalue));
        end
        else
          result:=groupdata[i].wildcard or (groupdata[i].customType.ConvertDataToInteger(newvalue, fscanner.currentAddress)=groupdata[i].valuei);

        inc(newvalue, groupdata[i].customType.bytesize);
      end;
      {$endif}
    end;
  end;

end;

function TGroupData.ByteScan(value: byte; buf: Pbytearray; var startoffset: integer): boolean;
var i: integer;
begin
  result:=false;
  for i:=startoffset to blocksize-1 do
    if buf[i]=value then
    begin
      startoffset:=i+1;
      result:=true;
      exit;
    end;
end;

function TGroupData.ByteScanRange(value,value2: byte; signed: boolean; buf: Pbytearray; var startoffset: integer): boolean;
var i: integer;
begin
  result:=false;

  for i:=startoffset to blocksize-1 do
    if ((not signed) and (buf[i]>=value) and (buf[i]<=value2)) or
       ((signed) and (Smallint(buf[i])>=Smallint(value)) and (Smallint(buf[i])<=Smallint(value2)))
    then
    begin
      startoffset:=i+1;
      result:=true;
      exit;
    end;
end;

function TGroupData.WordScan(value: word; buf: pointer; var startoffset: integer): boolean;
var current: pointer;
  i: integer;

  align: integer;
begin
  result:=false;
  if outoforder_aligned then
    align:=2
  else
    align:=1;

  current:=buf;
  inc(current, startoffset);
  i:=startoffset;

  while i<blocksize-1 do
  begin
    if pword(current)^=value then
    begin
      startoffset:=i+1;
      result:=true;
      exit;
    end;

    inc(current, align);
    inc(i, align);
  end;
end;

function TGroupData.WordScanRange(value,value2: word; signed: boolean; buf: pointer; var startoffset: integer): boolean;
var current: pointer;
  i: integer;

  align: integer;
begin
  result:=false;
  if outoforder_aligned then
    align:=2
  else
    align:=1;

  current:=buf;
  inc(current, startoffset);
  i:=startoffset;

  while i<blocksize-1 do
  begin
    if ((not signed) and (pword(current)^>=value) and (pword(current)^<=value2)) or
       ((signed) and (PShortint(current)^>=Shortint(value)) and (PShortint(current)^<=Shortint(value2)))
    then
    begin
      startoffset:=i+1;
      result:=true;
      exit;
    end;

    inc(current, align);
    inc(i, align);
  end;
end;

function TGroupData.DWordScan(value: dword; buf: pointer; var startoffset: integer): boolean;
var current: pointer;
  i: integer;
  align: integer;
begin
  result:=false;
  if outoforder_aligned then
    align:=4
  else
    align:=1;

  current:=buf;
  inc(current, startoffset);
  i:=startoffset;

  while i<blocksize-3 do
  begin
    if pdword(current)^=value then
    begin
      startoffset:=i+1;
      result:=true;
      exit;
    end;

    inc(current,align);
    inc(i,align);
  end;
end;

function TGroupData.DWordScanRange(value,value2: dword; signed: boolean; buf: pointer; var startoffset: integer): boolean;
var current: pointer;
  i: integer;
  align: integer;
begin
  result:=false;
  if outoforder_aligned then
    align:=4
  else
    align:=1;

  current:=buf;
  inc(current, startoffset);
  i:=startoffset;

  while i<blocksize-3 do
  begin
    if ((not signed) and (pdword(current)^>=value) and (pdword(current)^<=value2)) or
       ((signed) and (Pinteger(current)^>=integer(value)) and (PInteger(current)^<=Integer(value2)))
    then
    begin
      startoffset:=i+1;
      result:=true;
      exit;
    end;

    inc(current,align);
    inc(i,align);
  end;
end;

function TGroupData.QWordScan(value: qword; buf: pointer; var startoffset: integer): boolean;
var current: pointer;
  i: integer;
  align: integer;
begin
  result:=false;
  if outoforder_aligned then
    align:=4
  else
    align:=1;

  current:=buf;
  inc(current, startoffset);
  i:=startoffset;

  while i<blocksize-7 do
  begin
    if pqword(current)^=value then
    begin
      startoffset:=i+1;
      result:=true;
      exit;
    end;

    inc(current,align);
    inc(i,align);
  end;
end;

function TGroupData.QWordScanRange(value,value2: qword; signed: boolean; buf: pointer; var startoffset: integer): boolean;
var current: pointer;
  i: integer;
  align: integer;
begin
  result:=false;
  if outoforder_aligned then
    align:=4
  else
    align:=1;

  current:=buf;
  inc(current, startoffset);
  i:=startoffset;

  while i<blocksize-7 do
  begin
    if ((not signed) and (pqword(current)^>=value) and (pqword(current)^<=value2)) or
       ((signed) and (Pint64(current)^>=int64(value)) and (Pint64(current)^<=int64(value2)))
    then
    begin
      startoffset:=i+1;
      result:=true;
      exit;
    end;

    inc(current,align);
    inc(i,align);
  end;
end;

function TGroupData.Valid32BitPointerScan(value: qword; buf: pointer; var startoffset: integer; pointertypes: TPointerTypes): boolean;
var current: pointer;
  i: integer;
  align: integer;
begin
  result:=false;
  if outoforder_aligned then
    align:=4
  else
    align:=1;

  current:=buf;
  inc(current, startoffset);
  i:=startoffset;

  while i<blocksize-3 do
  begin
    if fScanner.OwningScanController.isPointer(pdword(current)^, pointertypes) then
    begin
      startoffset:=i+1;
      result:=true;
      exit;
    end;

    inc(current,align);
    inc(i,align);
  end;
end;

function TGroupData.Valid64BitPointerScan(value: qword; buf: pointer; var startoffset: integer; pointertypes: TPointerTypes): boolean;
var current: pointer;
  i: integer;
  align: integer;
begin
  result:=false;
  if outoforder_aligned then
    align:=4
  else
    align:=1;

  current:=buf;
  inc(current, startoffset);
  i:=startoffset;

  while i<blocksize-7 do
  begin
    if fScanner.OwningScanController.isPointer(pqword(current)^, pointertypes) then
    begin
      startoffset:=i+1;
      result:=true;
      exit;
    end;

    inc(current,align);
    inc(i,align);
  end;
end;

function TGroupData.SingleScan(minf,maxf: double; buf: pointer; var startoffset: integer): boolean;
var current: pointer;
  i: integer;
  align: integer;
begin
  result:=false;
  if outoforder_aligned then
    align:=4
  else
    align:=1;

  current:=buf;
  inc(current, startoffset);
  i:=startoffset;

  while i<blocksize-3 do
  begin
    if ((psingle(current)^>minf) and (psingle(current)^<maxf)) then
    begin
      if not (floatscanWithoutExponents and (pdword(current)^>0) and (abs(127-(pdword(current)^ shr 23) and $ff)>10)) then
      begin
        startoffset:=i+1;
        result:=true;
        exit;
      end;
    end;

    inc(current,align);
    inc(i,align);
  end;
end;

function TGroupData.DoubleScan(minf,maxf: double; buf: pointer; var startoffset: integer): boolean;
var current: pointer;
  i: integer;
  align: integer;
begin
  result:=false;
  if outoforder_aligned then
    align:=4
  else
    align:=1;

  current:=buf;
  inc(current, startoffset);
  i:=startoffset;

  while i<blocksize-7 do
  begin
    if (pdouble(current)^>minf) and (pdouble(current)^<maxf) then
    begin
      if not (floatscanWithoutExponents and (pqword(current)^>0) and (abs(integer(1023-(pqword(current)^ shr 52) and $7ff))>10)) then
      begin
        startoffset:=i+1;
        result:=true;
        exit;
      end;
    end;

    inc(current,align);
    inc(i,align);
  end;
end;

function TGroupData.CustomScan(ct: Tcustomtype; value: integer; buf: pointer; var startoffset: integer): boolean;
var current: pointer;
  i: integer;
  align: integer;
begin

  {$IFNDEF jni}
  result:=false;
  if outoforder_aligned then
    align:=4
  else
    align:=1;

  current:=buf;
  inc(current, startoffset);
  i:=startoffset;

  while i<(blocksize-(ct.bytesize-1)) do
  begin
    if ct.ConvertDataToInteger(current, fscanner.currentAddress)=value then
    begin
      startoffset:=i+1;
      result:=true;
      exit;
    end;

    inc(current,align);
    inc(i,align);
  end;
  {$ENDIF}

end;

function TGroupData.CustomScanFloat(ct: Tcustomtype; minf, maxf: single; buf: pointer; var startoffset: integer): boolean;
var current: pointer;
  i: integer;
  align: integer;
  f: single;
begin
  {$ifdef customtypeimplemented}
  result:=false;
  if outoforder_aligned then
    align:=4
  else
    align:=1;

  current:=buf;
  inc(current, startoffset);
  i:=startoffset;

  while i<(blocksize-(ct.bytesize-1)) do
  begin
    f:=ct.ConvertDataToFloat(current, fscanner.currentAddress);
    if (f>minf) and (f<maxf) then
    begin
      if not (floatscanWithoutExponents and (pdword(@f)^>0) and (abs(127-(pdword(@f)^ shr 23) and $ff)>10)) then
      begin
        startoffset:=i+1;
        result:=true;
        exit;
      end;
    end;

    inc(current,align);
    inc(i,align);
  end;
  {$ENDIF}

end;


function TGroupData.StringScan(st: pchar; buf: Pbytearray; var startoffset: integer): boolean;
var i: integer;
begin
  result:=false;
  for i:=startoffset to blocksize-1 do
    if testString(pchar(@buf[i]), st) then
    begin
      startoffset:=i+1;
      result:=true;
      exit;
    end;
end;

function TGroupData.WideStringScan(st: pwidechar; buf: Pbytearray; var startoffset: integer): boolean;
var i: integer;
begin
  result:=false;
  for i:=startoffset to blocksize-1 do
    if testWideString(pwidechar(@buf[i]), st) then
    begin
      startoffset:=i+1;
      result:=true;
      exit;
    end;
end;


function TGroupData.compareblock_outoforder(newvalue,oldvalue: pointer): boolean; //oldvalue is kinda ignored
var i,j: integer;
  currentoffset: integer;
  isin: boolean;
  currentoffsetEqualToZeroExist: boolean;

function isinlist: boolean; //check if currently in the list of offsets, if so, return true and adjust the current offset so the scan starts from next offset
var c: integer;
begin
  result:=false;
  for c:=0 to i-1 do
    if groupdata[c].offset=(currentoffset-1) then
    begin
      result:=true;

      //if outoforder_aligned then //increase by the alignment
      //  inc(currentoffset, groupdata[i].alignment);

      exit;
    end;
end;

begin
  result:=true;
  currentoffsetEqualToZeroExist:=false;

  for i:=0 to groupdatalength-1 do
  begin
    if result=false then exit;

    if groupdata[i].wildcard then continue; //really, it's useless for out of order scans

    isin:=true;

    currentoffset:=0;

    case groupdata[i].vartype of
      vtByte:
      begin
        while result and isin do
        begin
          if not groupdata[i].range then
            result:=ByteScan(groupdata[i].valuei, newvalue, currentoffset)
          else
            result:=ByteScanRange(groupdata[i].valuei, groupdata[i].valuei2, groupdata[i].signed, newvalue, currentoffset);

          isin:=result and isinlist;
        end;
      end;

      vtWord:
      begin
        while result and isin do
        begin
          if outoforder_aligned then //adjust currentoffset to be aligned on the current type alignment
            currentoffset:=(currentoffset+1) and $fffffffe;

          if not groupdata[i].range then
            result:=WordScan(groupdata[i].valuei, newvalue, currentoffset)
          else
            result:=WordScanRange(groupdata[i].valuei, groupdata[i].valuei2, groupdata[i].signed, newvalue, currentoffset);

          isin:=result and isinlist;
        end;
      end;

      vtDWord:
      begin
        while result and isin do
        begin
          if outoforder_aligned then //adjust currentoffset to be aligned on the current type alignment
            currentoffset:=(currentoffset+3) and $fffffffc;

          if not groupdata[i].range then
            result:=DWordScan(groupdata[i].valuei, newvalue, currentoffset)
          else
            result:=DWordScanRange(groupdata[i].valuei, groupdata[i].valuei2, groupdata[i].signed, newvalue, currentoffset);
          isin:=result and isinlist;
        end;
      end;

      vtQWord:
      begin
        while result and isin do
        begin
          if outoforder_aligned then //adjust currentoffset to be aligned on the current type alignment
            currentoffset:=(currentoffset+3) and $fffffffc;

          if not groupdata[i].range then
            result:=QWordScan(groupdata[i].valuei, newvalue, currentoffset)
          else
            result:=QWordScanRange(groupdata[i].valuei, groupdata[i].valuei2, groupdata[i].signed, newvalue, currentoffset);

          isin:=result and isinlist;
        end;
      end;

      vtSingle:
      begin
        while result and isin do
        begin
          if outoforder_aligned then //adjust currentoffset to be aligned on the current type alignment
            currentoffset:=(currentoffset+3) and $fffffffc;

          result:=SingleScan(groupdata[i].minfvalue, groupdata[i].maxfvalue, newvalue, currentoffset);

          isin:=result and isinlist;
        end;
      end;

      vtDouble:
      begin
        while result and isin do
        begin
          if outoforder_aligned then //adjust currentoffset to be aligned on the current type alignment
            currentoffset:=(currentoffset+3) and $fffffffc;

          result:=DoubleScan(groupdata[i].minfvalue, groupdata[i].maxfvalue, newvalue, currentoffset);
          isin:=result and isinlist;
        end;
      end;

      vtString:
      begin
        while result and isin do
        begin
          result:=StringScan(@groupdata[i].value[1], newvalue, currentoffset);
          isin:=result and isinlist;
        end;
      end;

      vtUnicodeString:
      begin
        while result and isin do
        begin
          result:=WideStringScan(@groupdata[i].widevalue[1], newvalue, currentoffset);
          isin:=result and isinlist;
        end;
      end;

      vtPointer:
      begin
        while result and isin do
        begin
          if is64bit then
            result:=Valid64BitPointerScan(0,newvalue, currentoffset, groupdata[i].pointertypes)
          else
            result:=Valid32BitPointerScan(0,newvalue, currentoffset, groupdata[i].pointertypes);

          isin:=result and isinlist;
        end;
      end;
{$ifdef customtypeimplemented}
      vtCustom:
      begin
        while result and isin do
        begin
          if outoforder_aligned then //adjust currentoffset to be aligned on the current type alignment
            currentoffset:=(currentoffset+3) and $fffffffc;

          if groupdata[i].customtype.scriptUsesString then
            result:=stringscan(pchar(groupdata[i].customtype.ConvertDataToString(newvalue, currentoffset)), newvalue, currentoffset)
          else
          if groupdata[i].customtype.scriptUsesFloat then
            result:=CustomScanFloat(groupdata[i].customtype, groupdata[i].minfvalue, groupdata[i].maxfvalue, newvalue, currentoffset)
          else
            result:=CustomScan(groupdata[i].customtype, groupdata[i].valuei, newvalue, currentoffset);

          isin:=result and isinlist;
        end;
      end;
 {$ENDIF}

    end;
    groupdata[i].offset:=currentoffset-1;
    if groupdata[i].offset=0 then currentoffsetEqualToZeroExist:=true;
  end;

  if result and outoforder then // at least one current offset must be zero
    result:=currentoffsetEqualToZeroExist;
end;


//==================TScanner===================//


//-----------=====Scanner check routines=====--------------//

function TScanner.AllUnknown(newvalue, oldvalue: pointer): boolean;
begin
  typesmatch[vtByte]:=typesmatch[vtByte];
  typesmatch[vtWord]:=typesmatch[vtWord];
  typesmatch[vtDword]:=typesmatch[vtDword];
  typesmatch[vtQword]:=typesmatch[vtQword];
  typesmatch[vtSingle]:=typesmatch[vtSingle];
  typesmatch[vtDouble]:=typesmatch[vtDouble];
  result:=true;
end;

function TScanner.AllExact(newvalue,oldvalue: pointer):boolean;
var i: TVariableType;
  j: integer;
begin
  typesmatch[vtByte]:=typesmatch[vtByte] and (ByteExact(newvalue,oldvalue) xor inverseScan); //oldvalue=nil, but give it anyhow
  typesmatch[vtWord]:=typesmatch[vtWord] and (WordExact(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDword]:=typesmatch[vtDword] and (DwordExact(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtQword]:=typesmatch[vtQword] and (qwordExact(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtSingle]:=typesmatch[vtSingle] and (singleExact(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDouble]:=typesmatch[vtDouble] and (doubleExact(newvalue,oldvalue) xor inverseScan);

  {$ifdef customtypeimplemented}
  if allCustom then
  begin
    //also scan custom types
    for j:=0 to customtypecount-1 do
    begin
      customtype:=tcustomtype(customTypes[j]);
      if customtype.scriptUsesString then
        customtypesmatch[j]:=false
      else
      if customtype.scriptUsesFloat then
        customtypesmatch[j]:=customtypesmatch[j] and (CustomFloatExact(newvalue,oldvalue) xor inverseScan)
      else
        customtypesmatch[j]:=customtypesmatch[j] and (CustomExact(newvalue,oldvalue) xor inverseScan)
    end;
  end;
  {$ENDIF}

  result:=false;
  for i:=vtbyte to vtdouble do
    if typesmatch[i] then
    begin
      result:=true;
      exit;
    end;

  if allCustom then
    for j:=0 to customtypecount-1 do
      if customtypesmatch[j] then
      begin
        result:=true;
        exit;
      end;

end;

function TScanner.AllLuaFormula(newvalue,oldvalue: pointer):boolean;
var i: TVariableType;
  j: integer;
begin
  typesmatch[vtByte]:=typesmatch[vtByte] and (ByteLuaFormula(newvalue,oldvalue) xor inverseScan); //oldvalue=nil, but give it anyhow
  typesmatch[vtWord]:=typesmatch[vtWord] and (WordLuaFormula(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDword]:=typesmatch[vtDword] and (DwordLuaFormula(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtQword]:=typesmatch[vtQword] and (qwordLuaFormula(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtSingle]:=typesmatch[vtSingle] and (singleLuaFormula(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDouble]:=typesmatch[vtDouble] and (doubleLuaFormula(newvalue,oldvalue) xor inverseScan);

  {$ifdef customtypeimplemented}
  if allCustom then
  begin
    //also scan custom types
    for j:=0 to customtypecount-1 do
    begin
      customtype:=tcustomtype(customTypes[j]);
      if customtype.scriptUsesString then
        customtypesmatch[j]:=false
      else
      if customtype.scriptUsesFloat then
        customtypesmatch[j]:=customtypesmatch[j] and (CustomFloatLuaFormula(newvalue,oldvalue) xor inverseScan)
      else
        customtypesmatch[j]:=customtypesmatch[j] and (CustomLuaFormula(newvalue,oldvalue) xor inverseScan)
    end;
  end;
  {$ENDIF}

  result:=false;
  for i:=vtbyte to vtdouble do
    if typesmatch[i] then
    begin
      result:=true;
      exit;
    end;

  if allCustom then
    for j:=0 to customtypecount-1 do
      if customtypesmatch[j] then
      begin
        result:=true;
        exit;
      end;

end;

function TScanner.AllBetween(newvalue,oldvalue: pointer):boolean;
var i: TVariableType;
  j: integer;
begin
  typesmatch[vtByte]:=typesmatch[vtByte] and (ByteBetween(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtWord]:=typesmatch[vtWord] and (WordBetween(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDword]:=typesmatch[vtDword] and (DwordBetween(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtQword]:=typesmatch[vtQword] and (qwordBetween(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtSingle]:=typesmatch[vtSingle] and (singleBetween(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDouble]:=typesmatch[vtDouble] and (doubleBetween(newvalue,oldvalue) xor inverseScan);

  {$ifdef customtypeimplemented}
  if allCustom then
  begin
    //also scan custom types
    for j:=0 to customtypecount-1 do
    begin
      customtype:=tcustomtype(customTypes[j]);
      if customtype.scriptUsesString then
        customtypesmatch[j]:=false
      else
      if customtype.scriptUsesFloat then
        customtypesmatch[j]:=customtypesmatch[j] and (CustomFloatBetween(newvalue,oldvalue) xor inverseScan)
      else
        customtypesmatch[j]:=customtypesmatch[j] and (CustomBetween(newvalue,oldvalue) xor inverseScan)
    end;
  end;
  {$ENDIF}

  result:=false;
  for i:=vtbyte to vtdouble do
    if typesmatch[i] then
    begin
      result:=true;
      exit;
    end;

  if allCustom then
    for j:=0 to customtypecount-1 do
      if customtypesmatch[j] then
      begin
        result:=true;
        exit;
      end;
end;

function TScanner.SignedAllBetween(newvalue,oldvalue: pointer):boolean;
var i: TVariableType;
  j: integer;
begin
  typesmatch[vtByte]:=typesmatch[vtByte] and (SignedByteBetween(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtWord]:=typesmatch[vtWord] and (SignedWordBetween(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDword]:=typesmatch[vtDword] and (SignedDwordBetween(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtQword]:=typesmatch[vtQword] and (SignedqwordBetween(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtSingle]:=typesmatch[vtSingle] and (singleBetween(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDouble]:=typesmatch[vtDouble] and (doubleBetween(newvalue,oldvalue) xor inverseScan);

  {$ifdef customtypeimplemented}
  if allCustom then
  begin
    //also scan custom types
    for j:=0 to customtypecount-1 do
    begin
      customtype:=tcustomtype(customTypes[j]);
      if customtype.scriptUsesString then
        customtypesmatch[j]:=false
      else
      if customtype.scriptUsesFloat then
        customtypesmatch[j]:=customtypesmatch[j] and (CustomFloatBetween(newvalue,oldvalue) xor inverseScan)
      else
        customtypesmatch[j]:=customtypesmatch[j] and (SignedCustomBetween(newvalue,oldvalue) xor inverseScan)
    end;
  end;
  {$ENDIF}

  result:=false;
  for i:=vtbyte to vtdouble do
    if typesmatch[i] then
    begin
      result:=true;
      exit;
    end;

  if allCustom then
    for j:=0 to customtypecount-1 do
      if customtypesmatch[j] then
      begin
        result:=true;
        exit;
      end;
end;

function TScanner.AllBetweenPercentage(newvalue,oldvalue: pointer):boolean;
var i: TVariableType;
    j: integer;
begin
  typesmatch[vtByte]:=typesmatch[vtByte] and (ByteBetweenPercentage(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtWord]:=typesmatch[vtWord] and (WordBetweenPercentage(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDword]:=typesmatch[vtDword] and (DwordBetweenPercentage(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtQword]:=typesmatch[vtQword] and (qwordBetweenPercentage(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtSingle]:=typesmatch[vtSingle] and (singleBetweenPercentage(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDouble]:=typesmatch[vtDouble] and (doubleBetweenPercentage(newvalue,oldvalue) xor inverseScan);

  {$ifdef customtypeimplemented}
  if allCustom then
  begin
    //also scan custom types
    for j:=0 to customtypecount-1 do
    begin
      customtype:=tcustomtype(customTypes[j]);
      if customtype.scriptUsesString then
        customtypesmatch[j]:=false
      else
      if customtype.scriptUsesFloat then
        customtypesmatch[j]:=customtypesmatch[j] and (CustomFloatBetweenPercentage(newvalue,oldvalue) xor inverseScan)
      else
        customtypesmatch[j]:=customtypesmatch[j] and (CustomBetweenPercentage(newvalue,oldvalue) xor inverseScan)
    end;
  end;
  {$ENDIF}

  result:=false;
  for i:=vtbyte to vtdouble do
    if typesmatch[i] then
    begin
      result:=true;
      exit;
    end;

  if allCustom then
    for j:=0 to customtypecount-1 do
      if customtypesmatch[j] then
      begin
        result:=true;
        exit;
      end;
end;

function TScanner.AllBiggerThan(newvalue,oldvalue: pointer):boolean;
var i: TVariableType;
      j: integer;
begin
  typesmatch[vtByte]:=typesmatch[vtByte] and (ByteBiggerThan(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtWord]:=typesmatch[vtWord] and (WordBiggerThan(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDword]:=typesmatch[vtDword] and (DwordBiggerThan(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtQword]:=typesmatch[vtQword] and (qwordBiggerThan(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtSingle]:=typesmatch[vtSingle] and (singleBiggerThan(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDouble]:=typesmatch[vtDouble] and (doubleBiggerThan(newvalue,oldvalue) xor inverseScan);

  {$ifdef customtypeimplemented}
  if allCustom then
  begin
    //also scan custom types
    for j:=0 to customtypecount-1 do
    begin
      customtype:=tcustomtype(customTypes[j]);
      if customtype.scriptUsesString then
        customtypesmatch[j]:=false
      else
      if customtype.scriptUsesFloat then
        customtypesmatch[j]:=customtypesmatch[j] and (CustomFloatBiggerThan(newvalue,oldvalue) xor inverseScan)
      else
        customtypesmatch[j]:=customtypesmatch[j] and (CustomBiggerThan(newvalue,oldvalue) xor inverseScan)
    end;
  end;
  {$ENDIF}

  result:=false;
  for i:=vtbyte to vtdouble do
    if typesmatch[i] then
    begin
      result:=true;
      exit;
    end;

  if allCustom then
    for j:=0 to customtypecount-1 do
      if customtypesmatch[j] then
      begin
        result:=true;
        exit;
      end;
end;

function TScanner.AllSmallerThan(newvalue,oldvalue: pointer):boolean;
var i: TVariableType;
        j: integer;
begin
  typesmatch[vtByte]:=typesmatch[vtByte] and (ByteSmallerThan(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtWord]:=typesmatch[vtWord] and (WordSmallerThan(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDword]:=typesmatch[vtDword] and (DwordSmallerThan(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtQword]:=typesmatch[vtQword] and (qwordSmallerThan(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtSingle]:=typesmatch[vtSingle] and (singleSmallerThan(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDouble]:=typesmatch[vtDouble] and (doubleSmallerThan(newvalue,oldvalue) xor inverseScan);

  {$ifdef customtypeimplemented}
  if allCustom then
  begin
    //also scan custom types
    for j:=0 to customtypecount-1 do
    begin
      customtype:=tcustomtype(customTypes[j]);
      if customtype.scriptUsesString then
        customtypesmatch[j]:=false
      else
      if customtype.scriptUsesFloat then
        customtypesmatch[j]:=customtypesmatch[j] and (CustomFloatSmallerThan(newvalue,oldvalue) xor inverseScan)
      else
        customtypesmatch[j]:=customtypesmatch[j] and (CustomSmallerThan(newvalue,oldvalue) xor inverseScan)
    end;
  end;
  {$ENDIF}

  result:=false;
  for i:=vtbyte to vtdouble do
    if typesmatch[i] then
    begin
      result:=true;
      exit;
    end;

  if allCustom then
    for j:=0 to customtypecount-1 do
      if customtypesmatch[j] then
      begin
        result:=true;
        exit;
      end;
end;

function TScanner.AllIncreasedValue(newvalue,oldvalue: pointer):boolean;
var i: TVariableType;
  j: integer;
begin
  typesmatch[vtByte]:=typesmatch[vtByte] and (ByteIncreasedValue(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtWord]:=typesmatch[vtWord] and (WordIncreasedValue(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDword]:=typesmatch[vtDword] and (DwordIncreasedValue(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtQword]:=typesmatch[vtQword] and (qwordIncreasedValue(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtSingle]:=typesmatch[vtSingle] and (singleIncreasedValue(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDouble]:=typesmatch[vtDouble] and (doubleIncreasedValue(newvalue,oldvalue) xor inverseScan);

  {$ifdef customtypeimplemented}
  if allCustom then
  begin
    //also scan custom types
    for j:=0 to customtypecount-1 do
    begin
      customtype:=tcustomtype(customTypes[j]);
      if customtype.scriptUsesString then
        customtypesmatch[j]:=false
      else
      if customtype.scriptUsesFloat then
        customtypesmatch[j]:=customtypesmatch[j] and (CustomFloatIncreasedValue(newvalue,oldvalue) xor inverseScan)
      else
        customtypesmatch[j]:=customtypesmatch[j] and (CustomIncreasedValue(newvalue,oldvalue) xor inverseScan)
    end;
  end;
  {$ENDIF}

  result:=false;
  for i:=vtbyte to vtdouble do
    if typesmatch[i] then
    begin
      result:=true;
      exit;
    end;

  if allCustom then
    for j:=0 to customtypecount-1 do
      if customtypesmatch[j] then
      begin
        result:=true;
        exit;
      end;
end;

function TScanner.AllIncreasedValueBy(newvalue,oldvalue: pointer):boolean;
var i: TVariableType;
  j: integer;
begin
  typesmatch[vtByte]:=typesmatch[vtByte] and (ByteIncreasedValueBy(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtWord]:=typesmatch[vtWord] and (WordIncreasedValueBy(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDword]:=typesmatch[vtDword] and (DwordIncreasedValueBy(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtQword]:=typesmatch[vtQword] and (qwordIncreasedValueBy(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtSingle]:=typesmatch[vtSingle] and (singleIncreasedValueBy(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDouble]:=typesmatch[vtDouble] and (doubleIncreasedValueBy(newvalue,oldvalue) xor inverseScan);

  {$ifdef customtypeimplemented}
  if allCustom then
  begin
    //also scan custom types
    for j:=0 to customtypecount-1 do
    begin
      customtype:=tcustomtype(customTypes[j]);
      if customtype.scriptUsesString then
        customtypesmatch[j]:=false
      else
      if customtype.scriptUsesFloat then
        customtypesmatch[j]:=customtypesmatch[j] and (CustomFloatIncreasedValueBy(newvalue,oldvalue) xor inverseScan)
      else
        customtypesmatch[j]:=customtypesmatch[j] and (CustomIncreasedValueBy(newvalue,oldvalue) xor inverseScan)
    end;
  end;
  {$ENDIF}

  result:=false;
  for i:=vtbyte to vtdouble do
    if typesmatch[i] then
    begin
      result:=true;
      exit;
    end;

  if allCustom then
    for j:=0 to customtypecount-1 do
      if customtypesmatch[j] then
      begin
        result:=true;
        exit;
      end;
end;

function TScanner.AllIncreasedValueByPercentage(newvalue,oldvalue: pointer):boolean;
var i: TVariableType;
  j: integer;
begin
  typesmatch[vtByte]:=typesmatch[vtByte] and (ByteIncreasedValueByPercentage(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtWord]:=typesmatch[vtWord] and (WordIncreasedValueByPercentage(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDword]:=typesmatch[vtDword] and (DwordIncreasedValueByPercentage(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtQword]:=typesmatch[vtQword] and (qwordIncreasedValueByPercentage(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtSingle]:=typesmatch[vtSingle] and (singleIncreasedValueByPercentage(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDouble]:=typesmatch[vtDouble] and (doubleIncreasedValueByPercentage(newvalue,oldvalue) xor inverseScan);

  {$ifdef customtypeimplemented}
  if allCustom then
  begin
    //also scan custom types
    for j:=0 to customtypecount-1 do
    begin
      customtype:=tcustomtype(customTypes[j]);
      if customtype.scriptUsesString then
        customtypesmatch[j]:=false
      else
      if customtype.scriptUsesFloat then
        customtypesmatch[j]:=customtypesmatch[j] and (CustomFloatIncreasedValueByPercentage(newvalue,oldvalue) xor inverseScan)
      else
        customtypesmatch[j]:=customtypesmatch[j] and (CustomIncreasedValueByPercentage(newvalue,oldvalue) xor inverseScan)
    end;
  end;
  {$ENDIF}

  result:=false;
  for i:=vtbyte to vtdouble do
    if typesmatch[i] then
    begin
      result:=true;
      exit;
    end;

  if allCustom then
    for j:=0 to customtypecount-1 do
      if customtypesmatch[j] then
      begin
        result:=true;
        exit;
      end;
end;


function TScanner.AllDecreasedValue(newvalue,oldvalue: pointer):boolean;
var i: TVariableType;
  j: integer;
begin
  typesmatch[vtByte]:=typesmatch[vtByte] and (ByteDecreasedValue(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtWord]:=typesmatch[vtWord] and (WordDecreasedValue(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDword]:=typesmatch[vtDword] and (DwordDecreasedValue(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtQword]:=typesmatch[vtQword] and (qwordDecreasedValue(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtSingle]:=typesmatch[vtSingle] and (singleDecreasedValue(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDouble]:=typesmatch[vtDouble] and (doubleDecreasedValue(newvalue,oldvalue) xor inverseScan);

  {$ifdef customtypeimplemented}
  if allCustom then
  begin
    //also scan custom types
    for j:=0 to customtypecount-1 do
    begin
      customtype:=tcustomtype(customTypes[j]);
      if customtype.scriptUsesString then
        customtypesmatch[j]:=false
      else
      if customtype.scriptUsesFloat then
        customtypesmatch[j]:=customtypesmatch[j] and (CustomFloatDecreasedValue(newvalue,oldvalue) xor inverseScan)
      else
        customtypesmatch[j]:=customtypesmatch[j] and (CustomDecreasedValue(newvalue,oldvalue) xor inverseScan)
    end;
  end;
  {$ENDIF}

  result:=false;
  for i:=vtbyte to vtdouble do
    if typesmatch[i] then
    begin
      result:=true;
      exit;
    end;

  if allCustom then
    for j:=0 to customtypecount-1 do
      if customtypesmatch[j] then
      begin
        result:=true;
        exit;
      end;
end;

function TScanner.AllDecreasedValueBy(newvalue,oldvalue: pointer):boolean;
var i: TVariableType;
  j: integer;
begin
  typesmatch[vtByte]:=typesmatch[vtByte] and (ByteDecreasedValueBy(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtWord]:=typesmatch[vtWord] and (WordDecreasedValueBy(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDword]:=typesmatch[vtDword] and (DwordDecreasedValueBy(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtQword]:=typesmatch[vtQword] and (qwordDecreasedValueBy(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtSingle]:=typesmatch[vtSingle] and (singleDecreasedValueBy(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDouble]:=typesmatch[vtDouble] and (doubleDecreasedValueBy(newvalue,oldvalue) xor inverseScan);

  {$ifdef customtypeimplemented}
  if allCustom then
  begin
    //also scan custom types
    for j:=0 to customtypecount-1 do
    begin
      customtype:=tcustomtype(customTypes[j]);
      if customtype.scriptUsesString then
        customtypesmatch[j]:=false
      else
      if customtype.scriptUsesFloat then
        customtypesmatch[j]:=customtypesmatch[j] and (CustomFloatDecreasedValueBy(newvalue,oldvalue) xor inverseScan)
      else
        customtypesmatch[j]:=customtypesmatch[j] and (CustomDecreasedValueBy(newvalue,oldvalue) xor inverseScan)
    end;
  end;
  {$ENDIF}

  result:=false;
  for i:=vtbyte to vtdouble do
    if typesmatch[i] then
    begin
      result:=true;
      exit;
    end;

  if allCustom then
    for j:=0 to customtypecount-1 do
      if customtypesmatch[j] then
      begin
        result:=true;
        exit;
      end;
end;

function TScanner.AllDecreasedValueByPercentage(newvalue,oldvalue: pointer):boolean;
var i: TVariableType;
  j: integer;
begin
  typesmatch[vtByte]:=typesmatch[vtByte] and (ByteDecreasedValueByPercentage(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtWord]:=typesmatch[vtWord] and (WordDecreasedValueByPercentage(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDword]:=typesmatch[vtDword] and (DwordDecreasedValueByPercentage(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtQword]:=typesmatch[vtQword] and (qwordDecreasedValueByPercentage(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtSingle]:=typesmatch[vtSingle] and (singleDecreasedValueByPercentage(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDouble]:=typesmatch[vtDouble] and (doubleDecreasedValueByPercentage(newvalue,oldvalue) xor inverseScan);

  {$ifdef customtypeimplemented}
  if allCustom then
  begin
    //also scan custom types
    for j:=0 to customtypecount-1 do
    begin
      customtype:=tcustomtype(customTypes[j]);
      if customtype.scriptUsesString then
        customtypesmatch[j]:=false
      else
      if customtype.scriptUsesFloat then
        customtypesmatch[j]:=customtypesmatch[j] and (CustomFloatDecreasedValueByPercentage(newvalue,oldvalue) xor inverseScan)
      else
        customtypesmatch[j]:=customtypesmatch[j] and (CustomDecreasedValueByPercentage(newvalue,oldvalue) xor inverseScan)
    end;
  end;
  {$ENDIF}

  result:=false;
  for i:=vtbyte to vtdouble do
    if typesmatch[i] then
    begin
      result:=true;
      exit;
    end;

  if allCustom then
    for j:=0 to customtypecount-1 do
      if customtypesmatch[j] then
      begin
        result:=true;
        exit;
      end;
end;

function TScanner.Allchanged(newvalue,oldvalue: pointer):boolean;
var i: TVariableType;
  j: integer;
begin
  typesmatch[vtByte]:=typesmatch[vtByte] and (ByteChanged(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtWord]:=typesmatch[vtWord] and (WordChanged(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDword]:=typesmatch[vtDword] and (DwordChanged(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtQword]:=typesmatch[vtQword] and (qwordChanged(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtSingle]:=typesmatch[vtSingle] and (singleChanged(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDouble]:=typesmatch[vtDouble] and (doubleChanged(newvalue,oldvalue) xor inverseScan);

  {$ifdef customtypeimplemented}
  if allCustom then
  begin
    //also scan custom types
    for j:=0 to customtypecount-1 do
    begin
      customtype:=tcustomtype(customTypes[j]);
      if customtype.scriptUsesString then
        customtypesmatch[j]:=false
      else
      if customtype.scriptUsesFloat then
        customtypesmatch[j]:=customtypesmatch[j] and (CustomFloatChanged(newvalue,oldvalue) xor inverseScan)
      else
        customtypesmatch[j]:=customtypesmatch[j] and (CustomChanged(newvalue,oldvalue) xor inverseScan)
    end;
  end;
  {$ENDIF}

  result:=false;
  for i:=vtbyte to vtdouble do
    if typesmatch[i] then
    begin
      result:=true;
      exit;
    end;

  if allCustom then
    for j:=0 to customtypecount-1 do
      if customtypesmatch[j] then
      begin
        result:=true;
        exit;
      end;
end;

function TScanner.AllUnchanged(newvalue,oldvalue: pointer):boolean;
var i: TVariableType;
  j: integer;
begin
  typesmatch[vtByte]:=typesmatch[vtByte] and (ByteUnchanged(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtWord]:=typesmatch[vtWord] and (WordUnchanged(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDword]:=typesmatch[vtDword] and (DwordUnchanged(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtQword]:=typesmatch[vtQword] and (qwordUnchanged(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtSingle]:=typesmatch[vtSingle] and (singleUnchanged(newvalue,oldvalue) xor inverseScan);
  typesmatch[vtDouble]:=typesmatch[vtDouble] and (doubleUnchanged(newvalue,oldvalue) xor inverseScan);

  {$ifdef customtypeimplemented}
  if allCustom then
  begin
    //also scan custom types
    for j:=0 to customtypecount-1 do
    begin
      customtype:=tcustomtype(customTypes[j]);
      if customtype.scriptUsesString then
        customtypesmatch[j]:=false
      else
      if customtype.scriptUsesFloat then
        customtypesmatch[j]:=customtypesmatch[j] and (CustomFloatUnchanged(newvalue,oldvalue) xor inverseScan)
      else
        customtypesmatch[j]:=customtypesmatch[j] and (CustomUnchanged(newvalue,oldvalue) xor inverseScan)
    end;
  end;
  {$ENDIF}

  result:=false;
  for i:=vtbyte to vtdouble do
    if typesmatch[i] then
    begin
      result:=true;
      exit;
    end;

  if allCustom then
    for j:=0 to customtypecount-1 do
      if customtypesmatch[j] then
      begin
        result:=true;
        exit;
      end;
end;


function TScanner.CustomCaseSensitiveAnsiStringExact(newvalue,oldvalue: pointer):boolean;
begin
  result:=customType.ConvertDataToString(newvalue, currentAddress)=scanvalue1;
end;

function TScanner.CustomCaseInsensitiveAnsiStringExact(newvalue,oldvalue: pointer):boolean;
begin
  //scanvalue1 has already been converted to uppercase in config
  result:=uppercase(customType.ConvertDataToString(newvalue, currentAddress))=scanvalue1;
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
  i:=length(widescanvalue1);
 // result:=false;

 { for i:=1 to length(scanvalue1) do
    if widescanvalue1[i]<>(pwidechar(newvalue)[i-1]) then exit;   }

  result:=strlcomp(@widescanvalue1[1], pwidechar(newvalue),i)=0;

 // result:=true;
end;

function TScanner.CaseInsensitiveUnicodeStringExact(newvalue,oldvalue: pointer):boolean;
var i: integer;
begin
  i:=length(widescanvalue1);

  result:=true;

  for i:=1 to length(widescanvalue1) do
    if system.UpCase(widescanvalue1[i])<>system.UpCase(pwidechar(newvalue)[i-1]) then exit(false);

  //result:=strlicomp(@widescanvalue1[1], pwidechar(newvalue), i)=0;
 { i:=0;

  for i:=1 to length(scanvalue1) do
  begin
    if pchar(newvalue)[(i-1)*sizeof(wchar)] in ['a'..'z'] then //change to uppercase
      dec(pbytearray(newvalue)[(i-1)*sizeof(wchar)],$20);

    if widescanvalue1[i]<>(pwidechar(newvalue)[i-1]) then exit;
  end;

  result:=true; //it got here, so a match }
end;

function TScanner.ArrayOfByteExact_NibbleWildcardSupport(newvalue,oldvalue: pointer):boolean;
//sperate function as support for this will cause it to be slower
var i: integer;
begin
  for i:=0 to abs_arraylength-1 do
  begin
    if abs_arraytofind[i]=-1 then continue; //full wildcard
    if abs_arraytofind[i]<0 then
    begin
      //it's a nibble wildcard
      //check if it matches
      if ((pbytearray(newvalue)[i] and ((abs_arraytofind[i] shr 8) and $ff))<> (abs_arraytofind[i] and $ff) ) then
      begin
        result:=false; //no match
        exit;
      end;
    end
    else
    if (pbytearray(newvalue)[i]<>abs_arraytofind[i]) then
    begin
      result:=false; //no match
      exit;
    end;
  end;

  result:=true; //still here, so a match
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

function TScanner.ArrayOfBytesExact_NibbleWildcardSupport(newvalue: pointer; mabsindex: integer):boolean;
var i: integer;
  arraylength: integer;
  a: PIntegerArray;
begin
  arraylength:=mabs[mabsindex].arraylength;
  a:=@mabs[mabsindex].arraytofind[0];

  for i:=0 to arraylength-1 do
  begin
    if a[i]=-1 then continue; //full wildcard
    if a[i]<0 then
    begin
      //it's a nibble wildcard
      //check if it matches
      if ((pbytearray(newvalue)[i] and ((a[i] shr 8) and $ff))<> (a[i] and $ff) ) then
      begin
        result:=false; //no match
        exit;
      end;
    end
    else
    if (pbytearray(newvalue)[i]<>a[i]) then
    begin
      result:=false; //no match
      exit;
    end;
  end;

  result:=true; //still here, so a match
end;

function TScanner.ArrayOfBytesExact(newvalue: pointer; mabsindex: integer):boolean;
var i: integer;
  arraylength: integer;
  a: PIntegerArray;
begin
  arraylength:=mabs[mabsindex].arraylength;
  a:=@mabs[mabsindex].arraytofind[0];

  for i:=0 to arraylength-1 do
    if (a[i]<>-1) and (pbytearray(newvalue)[i]<>a[i]) then
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
    binaryresults[i]:=((PQWORD(newvalue)^ shr i) and andmask)=bitmask;

  //no need for a result here, for binary that isn't checked (special case)
  result:=true; //let the store result routine deal with it
end;

function TScanner.Unknown(newvalue,oldvalue: pointer): boolean;
begin
  result:=true;
end;

//byte:
function TScanner.ByteExact(newvalue,oldvalue: pointer):boolean;
begin
  result:=pbyte(newvalue)^=byte(value);
end;

function TScanner.ByteLuaFormula(newvalue,oldvalue: pointer): boolean;
begin
  lua_pushvalue(L,-1);
  lua_pushinteger(L,pbyte(newvalue)^);
  if oldvalue<>nil then
    lua_pushinteger(L,pbyte(oldvalue)^)
  else
    lua_pushnil(L);

  lua_call(L,2,1);

  result:=lua_toboolean(L,-1);
  lua_pop(L,1);
end;

function TScanner.ByteBetween(newvalue,oldvalue: pointer):boolean;
begin
  result:=(pbyte(newvalue)^>=byte(value)) and (pbyte(newvalue)^<=byte(value2));
end;

function TScanner.SignedByteBetween(newvalue,oldvalue: pointer):boolean;
begin
  result:=(PSmallInt(newvalue)^>=SmallInt(value)) and (PSmallInt(newvalue)^<=SmallInt(value2));
end;

function TScanner.ByteBetweenPercentage(newvalue,oldvalue: pointer):boolean;
begin
  result:=(pbyte(newvalue)^>trunc(pbyte(oldvalue)^*svalue)) and (pbyte(newvalue)^<=trunc(pbyte(oldvalue)^*svalue2));
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

function TScanner.ByteIncreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
begin
  result:=(pbyte(newvalue)^>trunc(pbyte(oldvalue)^+pbyte(oldvalue)^*svalue)) and (pbyte(newvalue)^<trunc(pbyte(oldvalue)^+pbyte(oldvalue)^*svalue2));
end;

function TScanner.ByteDecreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
begin
  result:=(pbyte(newvalue)^>trunc(pbyte(oldvalue)^-pbyte(oldvalue)^*svalue2)) and (pbyte(newvalue)^<trunc(pbyte(oldvalue)^-pbyte(oldvalue)^*svalue));
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

function TScanner.WordLuaFormula(newvalue,oldvalue: pointer): boolean;
begin
  lua_pushvalue(L,-1);
  lua_pushinteger(L,pword(newvalue)^);
  if oldvalue<>nil then
    lua_pushinteger(L,pword(oldvalue)^)
  else
    lua_pushnil(L);

  lua_call(L,2,1);

  result:=lua_toboolean(L,-1);
  lua_pop(L,1);
end;

function TScanner.WordBetween(newvalue,oldvalue: pointer):boolean;
begin
  result:=(pword(newvalue)^>=word(value)) and (pword(newvalue)^<=word(value2));
end;

function TScanner.SignedWordBetween(newvalue,oldvalue: pointer):boolean;
begin
  result:=(PShortInt(newvalue)^>=shortint(value)) and (PShortInt(newvalue)^<=shortint(value2));
end;

function TScanner.WordBetweenPercentage(newvalue,oldvalue: pointer):boolean;
begin
  result:=(pword(newvalue)^>trunc(pword(oldvalue)^*svalue)) and (pword(newvalue)^<=trunc(pword(oldvalue)^*svalue2));
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

function TScanner.WordIncreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
begin
  result:=(pword(newvalue)^>trunc(pword(oldvalue)^+pword(oldvalue)^*svalue)) and (pword(newvalue)^<trunc(pword(oldvalue)^+pword(oldvalue)^*svalue2));
end;

function TScanner.WordDecreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
begin
  result:=(pword(newvalue)^>trunc(pword(oldvalue)^-pword(oldvalue)^*svalue2)) and (pword(newvalue)^<trunc(pword(oldvalue)^-pword(oldvalue)^*svalue));
end;

function TScanner.WordChanged(newvalue,oldvalue: pointer):boolean;
begin
  result:=pword(newvalue)^<>pword(oldvalue)^;
end;

function TScanner.WordUnchanged(newvalue,oldvalue: pointer):boolean;
begin
  result:=pword(newvalue)^=pword(oldvalue)^;
end;


//--------------\/custom\/
function TScanner.CustomExact(newvalue,oldvalue: pointer): boolean;
begin
  {$ifdef customtypeimplemented}
  result:=customType.ConvertDataToInteger(newvalue, currentAddress)=integer(value);
  {$ENDIF}
end;

function TScanner.CustomLuaFormula(newvalue,oldvalue: pointer): boolean;
begin
  lua_pushvalue(L,-1);
  lua_pushinteger(L,customType.ConvertDataToInteger(newvalue, currentAddress));
  if oldvalue<>nil then
    lua_pushinteger(L,customType.ConvertDataToInteger(oldvalue, currentAddress))
  else
    lua_pushnil(L);

  lua_call(L,2,1);

  result:=lua_toboolean(L,-1);
  lua_pop(L,1);
end;

function TScanner.CustomBetween(newvalue,oldvalue: pointer): boolean;
begin
  {$ifdef customtypeimplemented}
  result:=(DWORD(customType.ConvertDataToInteger(newvalue, currentAddress))>=DWORD(value)) and (dword(customType.ConvertDataToInteger(newvalue, currentAddress))<=dword(value2));
  {$ENDIF}
end;

function TScanner.SignedCustomBetween(newvalue,oldvalue: pointer): boolean;
begin
  {$ifdef customtypeimplemented}
  result:=(customType.ConvertDataToInteger(newvalue, currentAddress)>=integer(value)) and (customType.ConvertDataToInteger(newvalue, currentAddress)<=integer(value2));
  {$ENDIF}
end;


function TScanner.CustomBetweenPercentage(newvalue,oldvalue: pointer):boolean;
begin
  {$ifdef customtypeimplemented}
  result:=(customType.ConvertDataToInteger(newvalue, currentAddress)>trunc(customType.ConvertDataToInteger(oldvalue, currentAddress)*svalue)) and (customType.ConvertDataToInteger(newvalue, currentAddress)<=trunc(customType.ConvertDataToInteger(oldvalue, currentAddress)*svalue2));
  {$ENDIF}
end;

function TScanner.CustomBiggerThan(newvalue,oldvalue: pointer): boolean;
begin
  {$ifdef customtypeimplemented}
  result:=customType.ConvertDataToInteger(newvalue, currentAddress)>integer(value);
  {$ENDIF}
end;

function TScanner.CustomSmallerThan(newvalue,oldvalue: pointer): boolean;
begin
  {$ifdef customtypeimplemented}
  result:=customType.ConvertDataToInteger(newvalue, currentAddress)<integer(value);
  {$ENDIF}
end;

function TScanner.CustomIncreasedValue(newvalue,oldvalue: pointer): boolean;
begin
  {$ifdef customtypeimplemented}
  result:=customType.ConvertDataToInteger(newvalue, currentAddress)>customType.ConvertDataToInteger(oldvalue, currentAddress);
  {$ENDIF}
end;

function TScanner.CustomIncreasedValueBy(newvalue,oldvalue: pointer): boolean;
begin
  {$ifdef customtypeimplemented}
  result:=customType.ConvertDataToInteger(newvalue, currentAddress)=customType.ConvertDataToInteger(oldvalue, currentAddress)+dword(value);
  {$ENDIF}
end;

function TScanner.CustomIncreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
begin
  {$ifdef customtypeimplemented}
  result:=(customType.ConvertDataToInteger(newvalue, currentAddress)>trunc(customType.ConvertDataToInteger(oldvalue, currentAddress)+customType.ConvertDataToInteger(oldvalue, currentAddress)*svalue)) and (customType.ConvertDataToInteger(newvalue,currentAddress)<trunc(customType.ConvertDataToInteger(oldvalue, currentAddress)+customType.ConvertDataToInteger(oldvalue, currentAddress)*svalue2));
  {$ENDIF}
end;

function TScanner.CustomDecreasedValue(newvalue,oldvalue: pointer): boolean;
begin
  {$ifdef customtypeimplemented}
  result:=customType.ConvertDataToInteger(newvalue, currentAddress)<customType.ConvertDataToInteger(oldvalue, currentAddress);
  {$ENDIF}
end;

function TScanner.CustomDecreasedValueBy(newvalue,oldvalue: pointer): boolean;
begin
  {$ifdef customtypeimplemented}
  result:=customType.ConvertDataToInteger(newvalue, currentAddress)=customType.ConvertDataToInteger(oldvalue, currentAddress)-dword(value);
  {$ENDIF}
end;

function TScanner.CustomDecreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
begin
  {$ifdef customtypeimplemented}
  result:=(customType.ConvertDataToInteger(newvalue, currentAddress)>trunc(customType.ConvertDataToInteger(oldvalue, currentAddress)-customType.ConvertDataToInteger(oldvalue, currentAddress)*svalue2)) and (customType.ConvertDataToInteger(newvalue, currentAddress)<trunc(customType.ConvertDataToInteger(oldvalue, currentAddress)-customType.ConvertDataToInteger(oldvalue, currentAddress)*svalue));
  {$ENDIF}
end;

function TScanner.CustomChanged(newvalue,oldvalue: pointer): boolean;
begin
  {$ifdef customtypeimplemented}
  result:=customType.ConvertDataToInteger(newvalue, currentAddress)<>customType.ConvertDataToInteger(oldvalue, currentAddress);
  {$ENDIF}
end;

function TScanner.CustomUnChanged(newvalue,oldvalue: pointer): boolean;
begin
  {$ifdef customtypeimplemented}
  result:=customType.ConvertDataToInteger(newvalue, currentAddress)=customType.ConvertDataToInteger(oldvalue, currentAddress);
  {$ENDIF}
end;
//--------------/\custom/\


//--------------Custom Float-------------

function TScanner.CustomFloatExact(newvalue,oldvalue: pointer): boolean;
var f: single;
begin
  result:=false;
  {$ifdef customtypeimplemented}
  f:=customType.ConvertDataToFloat(newvalue, currentAddress);
  case roundingtype of
    rtRounded:
      result:=(RoundTo(f,-floataccuracy)=svalue);

    rtExtremerounded:
      result:=(f>minsvalue) and (f<maxsvalue);

    rtTruncated:
      result:=(f>=svalue) and (f<maxsvalue);
  end;
  {$ENDIF}

end;

function TScanner.CustomFloatLuaFormula(newvalue,oldvalue: pointer): boolean;
begin
  lua_pushvalue(L,-1);
  lua_pushnumber(L,customType.ConvertDataToFloat(newvalue, currentAddress));
  if oldvalue<>nil then
    lua_pushnumber(L,customType.ConvertDataToFloat(oldvalue, currentAddress))
  else
    lua_pushnil(L);

  lua_call(L,2,1);

  result:=lua_toboolean(L,-1);
  lua_pop(L,1);
end;


function TScanner.CustomFloatBetween(newvalue,oldvalue: pointer):boolean;
var f: single;
begin
  {$ifdef customtypeimplemented}
  f:=customType.ConvertDataToFloat(newvalue, currentAddress);
  result:=(f>=svalue) and (f<=svalue2);
  {$ENDIF}
end;

function TScanner.CustomFloatBetweenPercentage(newvalue,oldvalue: pointer): boolean;
var new: single;
    old: single;
begin
  {$ifdef customtypeimplemented}
  new:=customType.ConvertDataToFloat(newvalue, currentAddress);
  old:=customType.ConvertDataToFloat(oldvalue, currentAddress);
  result:=(new>old*svalue) and (new<=old*svalue2);
  {$ENDIF}
end;

function TScanner.CustomFloatBiggerThan(newvalue,oldvalue: pointer):boolean;
begin
  {$ifdef customtypeimplemented}
  result:=customType.ConvertDataToFloat(newvalue, currentAddress)>svalue;
  {$ENDIF}
end;

function TScanner.CustomFloatSmallerThan(newvalue,oldvalue: pointer):boolean;
begin
  {$ifdef customtypeimplemented}
  result:=customType.ConvertDataToFloat(newvalue, currentAddress)<svalue;
  {$ENDIF}
end;

function TScanner.CustomFloatIncreasedValue(newvalue,oldvalue: pointer):boolean;
begin
  {$ifdef customtypeimplemented}
  result:=customType.ConvertDataToFloat(newvalue, currentAddress)>customType.ConvertDataToFloat(oldvalue, currentAddress);
  {$ENDIF}
end;

function TScanner.CustomFloatIncreasedValueBy(newvalue,oldvalue: pointer):boolean;
begin
  {$ifdef customtypeimplemented}

  result:=(not CompareMem(newvalue, oldvalue, customtype.bytesize)) and (RoundTo(customType.ConvertDataToFloat(newvalue, currentAddress),-floataccuracy)=RoundTo(customType.ConvertDataToFloat(oldvalue, currentAddress)+svalue,-floataccuracy));
  {$ENDIF}
end;

function TScanner.CustomFloatDecreasedValue(newvalue,oldvalue: pointer):boolean;
begin
  {$ifdef customtypeimplemented}
  result:=customType.ConvertDataToFloat(newvalue, currentAddress)<customType.ConvertDataToFloat(oldvalue, currentAddress);
  {$ENDIF}
end;

function TScanner.CustomFloatDecreasedValueBy(newvalue,oldvalue: pointer):boolean;
begin
  {$ifdef customtypeimplemented}
  result:=(not CompareMem(newvalue, oldvalue, customtype.bytesize)) and (RoundTo(customType.ConvertDataToFloat(newvalue, currentAddress),-floataccuracy)=RoundTo(customType.ConvertDataToFloat(oldvalue, currentAddress)-svalue,-floataccuracy));
  {$ENDIF}
end;

function TScanner.CustomFloatIncreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
var new, old: single;
begin
  {$ifdef customtypeimplemented}
  new:=customType.ConvertDataToFloat(newvalue, currentAddress);
  old:=customType.ConvertDataToFloat(oldvalue, currentAddress);
  result:=(new>old+old*svalue) and (new<old+old*svalue2);
  {$ENDIF}
end;

function TScanner.CustomFloatDecreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
var new, old: single;
begin
  {$ifdef customtypeimplemented}
  new:=customType.ConvertDataToFloat(newvalue, currentAddress);
  old:=customType.ConvertDataToFloat(oldvalue, currentAddress);
  result:=(new>old-old*svalue2) and (new<old-old*svalue);
  {$ENDIF}
end;

function TScanner.CustomFloatChanged(newvalue,oldvalue: pointer):boolean;
begin
  {$ifdef customtypeimplemented}
  result:=customType.ConvertDataToFloat(newvalue, currentAddress)<>customType.ConvertDataToFloat(oldvalue, currentAddress);
  {$ENDIF}
end;

function TScanner.CustomFloatUnchanged(newvalue,oldvalue: pointer):boolean;
begin
  {$ifdef customtypeimplemented}
  result:=customType.ConvertDataToFloat(newvalue, currentAddress)=customType.ConvertDataToFloat(oldvalue, currentAddress);
  {$ENDIF}
end;
//   ^^^^CustomFloat^^^^

//dword:
{todo:
function TScanner.DWordExactFormula(newvalue,oldvalue: pointer): boolean;
begin
  result:=doforumula(pdword(newvalue)^, pdword(oldvalue)^);
end;

or

function TScanner.Custom(newvalue,oldvalue: pointer): boolean;
begin
  result:=doforumula(pdword(newvalue)^, pdword(oldvalue)^);
end;
}



function TScanner.DWordExact(newvalue,oldvalue: pointer): boolean;
begin
  result:=pdword(newvalue)^=dword(value);
end;

function TScanner.DWordLuaFormula(newvalue,oldvalue: pointer): boolean;
var t: integer;
begin
  lua_pushvalue(L,-1);

  lua_pushinteger(L,pdword(newvalue)^);
  if oldvalue<>nil then
    lua_pushinteger(L,pdword(oldvalue)^)
  else
    lua_pushnil(L);

  lua_call(L,2,1);

  result:=lua_toboolean(L,-1);
  lua_pop(L,1);
end;

function TScanner.DWordBetween(newvalue,oldvalue: pointer):boolean;
begin
  result:=(pdword(newvalue)^>=dword(value)) and (pdword(newvalue)^<=dword(value2));
end;

function TScanner.SignedDWordBetween(newvalue,oldvalue: pointer):boolean;
begin
  result:=(PInteger(newvalue)^>=integer(value)) and (PInteger(newvalue)^<=integer(value2));
end;


function TScanner.DwordBetweenPercentage(newvalue,oldvalue: pointer):boolean;
begin
  result:=(pdword(newvalue)^>trunc(pdword(oldvalue)^*svalue)) and (pdword(newvalue)^<=trunc(pdword(oldvalue)^*svalue2));
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

function TScanner.DWordIncreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
begin
  result:=(pdword(newvalue)^>trunc(pdword(oldvalue)^+pdword(oldvalue)^*svalue)) and (pdword(newvalue)^<trunc(pdword(oldvalue)^+pdword(oldvalue)^*svalue2));
end;

function TScanner.DWordDecreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
begin
  result:=(pdword(newvalue)^>trunc(pdword(oldvalue)^-pdword(oldvalue)^*svalue2)) and (pdword(newvalue)^<trunc(pdword(oldvalue)^-pdword(oldvalue)^*svalue));
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
  result:=PQWORD(newvalue)^=QWORD(value);
end;

function TScanner.QWordLuaFormula(newvalue,oldvalue: pointer): boolean;
begin
  lua_pushvalue(L,-1);
  lua_pushinteger(L,pqword(newvalue)^);
  if oldvalue<>nil then
    lua_pushinteger(L,pqword(oldvalue)^)
  else
    lua_pushnil(L);

  lua_call(L,2,1);

  result:=lua_toboolean(L,-1);
  lua_pop(L,1);
end;

function TScanner.QWordBetween(newvalue,oldvalue: pointer):boolean;
begin
  result:=(PQWORD(newvalue)^>=QWORD(value)) and (PQWORD(newvalue)^<=QWORD(value2));
end;

function TScanner.SignedQWordBetween(newvalue,oldvalue: pointer):boolean;
begin
  result:=(PINT64(newvalue)^>=int64(value)) and (PINT64(newvalue)^<=int64(value2));
end;


function TScanner.QwordBetweenPercentage(newvalue,oldvalue: pointer):boolean;
begin
  result:=(PQWORD(newvalue)^>trunc(PQWORD(oldvalue)^*svalue)) and (PQWORD(newvalue)^<=trunc(PQWORD(oldvalue)^*svalue2));
end;

function TScanner.QWordBiggerThan(newvalue,oldvalue: pointer):boolean;
begin
  result:=PQWORD(newvalue)^>QWORD(value);
end;

function TScanner.QWordSmallerThan(newvalue,oldvalue: pointer):boolean;
begin
  result:=PQWORD(newvalue)^<QWORD(value);
end;

function TScanner.QWordIncreasedValue(newvalue,oldvalue: pointer):boolean;
begin
  result:=PQWORD(newvalue)^>PQWORD(oldvalue)^;
end;

function TScanner.QWordIncreasedValueBy(newvalue,oldvalue: pointer):boolean;
begin
  result:=PQWORD(newvalue)^=PQWORD(oldvalue)^+value;
end;

function TScanner.QWordDecreasedValue(newvalue,oldvalue: pointer):boolean;
begin
  result:=PQWORD(newvalue)^<PQWORD(oldvalue)^;
end;

function TScanner.QWordDecreasedValueBy(newvalue,oldvalue: pointer):boolean;
begin
  result:=PQWORD(newvalue)^=PQWORD(oldvalue)^-value;
end;

function TScanner.QWordIncreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
begin
  result:=(PQWORD(newvalue)^>trunc(PQWORD(oldvalue)^+PQWORD(oldvalue)^*svalue)) and (PQWORD(newvalue)^<trunc(PQWORD(oldvalue)^+PQWORD(oldvalue)^*svalue2));
end;

function TScanner.QWordDecreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
begin
  result:=(PQWORD(newvalue)^>trunc(PQWORD(oldvalue)^-PQWORD(oldvalue)^*svalue2)) and (PQWORD(newvalue)^<trunc(PQWORD(oldvalue)^-PQWORD(oldvalue)^*svalue));
end;

function TScanner.QWordChanged(newvalue,oldvalue: pointer):boolean;
begin
  result:=PQWORD(newvalue)^<>PQWORD(oldvalue)^;
end;

function TScanner.QWordUnchanged(newvalue,oldvalue: pointer):boolean;
begin
  result:=PQWORD(newvalue)^=PQWORD(oldvalue)^;
end;


//single:

function TScanner.SingleExact(newvalue,oldvalue: pointer): boolean;
begin
  result:=false;
  case roundingtype of
    rtRounded:
      result:=(RoundTo(psingle(newvalue)^,-floataccuracy)=svalue);

    rtExtremerounded:
      result:=(psingle(newvalue)^>=minsvalue) and (psingle(newvalue)^<=maxsvalue);

    rtTruncated:
      result:=(psingle(newvalue)^>=svalue) and (psingle(newvalue)^<maxsvalue);
  end;

end;

function TScanner.SingleLuaFormula(newvalue,oldvalue: pointer): boolean;
begin
  lua_pushvalue(L,-1);
  lua_pushnumber(L,psingle(newvalue)^);
  if oldvalue<>nil then
    lua_pushnumber(L,psingle(oldvalue)^)
  else
    lua_pushnil(L);

  lua_call(L,2,1);

  result:=lua_toboolean(L,-1);
  lua_pop(L,1);
end;

function TScanner.SingleBetween(newvalue,oldvalue: pointer):boolean;
begin
  result:=(psingle(newvalue)^>=svalue) and (psingle(newvalue)^<=svalue2);
end;

function TScanner.SingleBetweenPercentage(newvalue,oldvalue: pointer): boolean;
begin
  result:=(psingle(newvalue)^>psingle(oldvalue)^*svalue) and (psingle(newvalue)^<=psingle(oldvalue)^*svalue2);
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
  result:=(pdword(newvalue)^<>pdword(oldvalue)^) and (RoundTo(psingle(newvalue)^,-floataccuracy)=RoundTo(psingle(oldvalue)^+svalue,-floataccuracy));
end;

function TScanner.SingleDecreasedValue(newvalue,oldvalue: pointer):boolean;
begin
  result:=psingle(newvalue)^<psingle(oldvalue)^;
end;

function TScanner.SingleDecreasedValueBy(newvalue,oldvalue: pointer):boolean;
begin
  result:=(pdword(newvalue)^<>pdword(oldvalue)^) and (RoundTo(psingle(newvalue)^,-floataccuracy)=RoundTo(psingle(oldvalue)^-svalue,-floataccuracy));
end;

function TScanner.SingleIncreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
begin
  result:=(psingle(newvalue)^>psingle(oldvalue)^+psingle(oldvalue)^*svalue) and (psingle(newvalue)^<psingle(oldvalue)^+psingle(oldvalue)^*svalue2);
end;

function TScanner.SingleDecreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
begin
  result:=(psingle(newvalue)^>psingle(oldvalue)^-psingle(oldvalue)^*svalue2) and (psingle(newvalue)^<psingle(oldvalue)^-psingle(oldvalue)^*svalue);
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

function TScanner.DoubleLuaFormula(newvalue,oldvalue: pointer): boolean;
begin
  lua_pushvalue(L,-1);
  lua_pushnumber(L,pdouble(newvalue)^);
  if oldvalue<>nil then
    lua_pushnumber(L,pdouble(oldvalue)^)
  else
    lua_pushnil(L);

  lua_call(L,2,1);

  result:=lua_toboolean(L,-1);
  lua_pop(L,1);
end;

function TScanner.DoubleBetween(newvalue,oldvalue: pointer):boolean;
begin
  result:=(pdouble(newvalue)^>=dvalue) and (pdouble(newvalue)^<=dvalue2);
end;

function TScanner.DoubleBetweenPercentage(newvalue,oldvalue: pointer): boolean;
begin
  result:=(pdouble(newvalue)^>pdouble(oldvalue)^*dvalue) and (pdouble(newvalue)^<=pdouble(oldvalue)^*dvalue2);
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
  result:=(pqword(newvalue)^<>pqword(oldvalue)^) and (RoundTo(pdouble(newvalue)^,-floataccuracy)=RoundTo(pdouble(oldvalue)^+svalue,-floataccuracy));
end;

function TScanner.DoubleDecreasedValue(newvalue,oldvalue: pointer):boolean;
begin
  result:=pdouble(newvalue)^<pdouble(oldvalue)^;
end;

function TScanner.DoubleDecreasedValueBy(newvalue,oldvalue: pointer):boolean;
begin
  result:=(pqword(newvalue)^<>pqword(oldvalue)^) and (RoundTo(pdouble(newvalue)^,-floataccuracy)=RoundTo(pdouble(oldvalue)^-svalue,-floataccuracy));
end;

function TScanner.DoubleIncreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
begin
  result:=(pdouble(newvalue)^>pdouble(oldvalue)^+pdouble(oldvalue)^*dvalue) and (pdouble(newvalue)^<pdouble(oldvalue)^+pdouble(oldvalue)^*dvalue2);
end;

function TScanner.DoubleDecreasedValueByPercentage(newvalue,oldvalue: pointer): boolean;
begin
  result:=(pdouble(newvalue)^>pdouble(oldvalue)^-pdouble(oldvalue)^*dvalue2) and (pdouble(newvalue)^<pdouble(oldvalue)^-pdouble(oldvalue)^*dvalue);
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

procedure TScanner.GenericSaveResult(address: ptruint; oldvalue: pointer);
{
Generic routine for storing results. Use as last resort. E.g custom scans
}
var f: single;
begin
  //save varsize
  {$ifdef customtypeimplemented}
  if (variableType = vtCustom) and (customtype<>nil) and (customtype.scriptUsesFloat) then
  begin
    //check if it's a valid float result
    f:=customType.ConvertDataToFloat(oldvalue, currentAddress); //get value
    if isnan(f) or IsInfinite(f) then exit; //check if valid, if not, exit

  end;
  {$ENDIF}

  PPtrUintArray(CurrentAddressBuffer)[found]:=address;
  copyMemory(pointer(ptruint(CurrentFoundBuffer)+ptruint(variablesize*found)),oldvalue,variablesize);

  inc(found);
  if found>=maxfound then
    flushroutine;
end;

procedure TScanner.ByteSaveResult(address: ptruint; oldvalue: pointer);
begin
  //save address and current value
  PPtrUintArray(CurrentAddressBuffer)[found]:=address;
  pbytearray(CurrentFoundBuffer)[found]:=pbyte(oldvalue)^;

  inc(found);
  if found>=maxfound then
    flushroutine;
end;

procedure TScanner.WordSaveResult(address: ptruint; oldvalue: pointer);
begin
  PPtrUintArray(CurrentAddressBuffer)[found]:=address;
  pwordarray(CurrentFoundBuffer)[found]:=pword(oldvalue)^;

  inc(found);
  if found>=maxfound then
    flushroutine;
end;

procedure TScanner.DWordSaveResult(address: ptruint; oldvalue: pointer);
begin
  PPtrUintArray(CurrentAddressBuffer)[found]:=address;
  pdwordarray(CurrentFoundBuffer)[found]:=pdword(oldvalue)^;

  inc(found);
  if found>=maxfound then
    flushroutine;
end;

procedure TScanner.QWordSaveResult(address: ptruint; oldvalue: pointer);
begin
  PPtrUintArray(CurrentAddressBuffer)[found]:=address;
  puint64array(CurrentFoundBuffer)[found]:=PQWORD(oldvalue)^;

  inc(found);
  if found>=maxfound then
    flushroutine;
end;

procedure TScanner.SingleSaveResult(address: ptruint; oldvalue: pointer);
var
  exp: integer;
begin
  if not (isnan(psingle(oldvalue)^) or IsInfinite(psingle(oldvalue)^)) then
  begin
    if floatscanWithoutExponents and (pdword(oldvalue)^>0) and (abs(127-(pdword(oldvalue)^ shr 23) and $ff)>10) then exit;

    PPtrUintArray(CurrentAddressBuffer)[found]:=address;
    psinglearray(CurrentFoundBuffer)[found]:=psingle(oldvalue)^;

    inc(found);
    if found>=maxfound then
      flushroutine;
  end;
end;

procedure TScanner.DoubleSaveResult(address: ptruint; oldvalue: pointer);

begin
  if not (isnan(pdouble(oldvalue)^) or IsInfinite(pdouble(oldvalue)^))  then
  begin
    if floatscanWithoutExponents and (pqword(oldvalue)^>0) and (abs(integer(1023-(pqword(oldvalue)^ shr 52) and $7ff))>10) then exit;

    PPtrUintArray(CurrentAddressBuffer)[found]:=address;
    pdoublearray(CurrentFoundBuffer)[found]:=pdouble(oldvalue)^;

    inc(found);
    if found>=maxfound then
      flushroutine;
  end;
end;


procedure TScanner.arrayOfByteSaveResult(address: ptruint; oldvalue: pointer);
begin
  PPtrUintArray(CurrentAddressBuffer)[found]:=address;
  inc(found);
  if found>=maxfound then
    flushroutine;  
end;

procedure TScanner.binarySaveResult(address: ptruint; oldvalue: pointer);
var i: integer;
begin
  for i:=0 to 7 do
  begin
    if binaryresults[i] then
    begin
      PBitAddressArray(CurrentAddressBuffer)[found].address:=address;
      PBitAddressArray(CurrentAddressBuffer)[found].bit:=i;
      inc(found);
      if found>=maxfound then
        flushroutine;
    end;
  end;

end;

procedure TScanner.groupSaveResult(address: ptruint; oldvalue: pointer);
var i: integer;
  entry: PGroupAddress;
begin
  entry:=PGroupAddress(ptruint(CurrentAddressBuffer)+ found*(sizeof(ptruint)+sizeof(dword)*groupdata.groupdatalength));
  entry.address:=address;


  for i:=0 to groupdata.groupdatalength-1 do
    entry.offsets[i]:=groupdata.groupdata[i].offset;

  inc(found);
  if found>=maxfound then
      flushroutine;
end;

procedure TScanner.allSaveResult(address: ptruint; oldvalue: pointer);
{
note: eventually replace bit with a binary representation of all types that match
BUT, don't forget to change the foundlisthelper to handle this (since it'd be
multiple addresses in one entry, which isn't handled right now... 
}
var i: TVariabletype;
  j: integer;
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

        if floatscanWithoutExponents and (pdword(oldvalue)^>0) and (abs(127-(pdword(oldvalue)^ shr 23) and $ff)>10) then
          continue;
      end;

      if (i=vtdouble) then
      begin
        if isnan(pdouble(oldvalue)^) or IsInfinite(pdouble(oldvalue)^) then
          continue; //skip, don't save

        if floatscanWithoutExponents and (pqword(oldvalue)^>0) and (abs(integer(1023-(pqword(oldvalue)^ shr 52) and $7ff))>10) then
          continue;
      end;

      //using the bitaddressarray since it holds a address and a value big enough to hold all types
      PBitAddressArray(CurrentAddressBuffer)[found].address:=address;
      PBitAddressArray(CurrentAddressBuffer)[found].bit:=integer(i);

      //save the current address, max variablesize is x bytes, so just store that
      copyMemory(pointer(ptruint(CurrentFoundBuffer)+ptruint(variablesize*found)),oldvalue,variablesize);

      inc(found);
      if found>=maxfound then
        flushroutine;
    end;
  end;

  if allCustom then
  begin
    for j:=0 to customtypecount-1 do
    begin
      if customtypesmatch[j] then
      begin
        PBitAddressArray(CurrentAddressBuffer)[found].address:=address;
        PBitAddressArray(CurrentAddressBuffer)[found].bit:=j+$1000; //+$1000 to distinguish between custom and default types. When I add more than 4095 default types I'll have to change this.

        copyMemory(pointer(ptruint(CurrentFoundBuffer)+ptruint(variablesize*found)),oldvalue,variablesize);

        inc(found);
        if found>=maxfound then
          flushroutine;
      end;
    end;
  end;
end;


//=============Flush result routines===========//
procedure TScanner.genericFlush;
var tempaddress,tempfound: pointer;
begin
  scanwriter.writeresults(CurrentAddressBuffer,currentfoundbuffer,sizeof(ptruint)*found,found*variablesize);  //address
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
  AddressFile.WriteBuffer(CurrentAddressBuffer^,sizeof(ptruint)*found); //address
  inc(totalfound,found);
  found:=0;
end;

procedure TScanner.binaryFlush;
begin
  AddressFile.WriteBuffer(CurrentAddressBuffer^,sizeof(TBitAddress)*found);
  inc(totalfound,found);
  found:=0;
end;

procedure TScanner.groupflush;
begin                                                         //address, offset,offset,...
  AddressFile.WriteBuffer(CurrentAddressBuffer^,found*(sizeof(ptruint)+sizeof(dword)*groupdata.groupdatalength));
  inc(totalfound,found);
  found:=0;
end;

procedure TScanner.allFlush;
var tempaddress,tempfound: pointer;
begin
  scanwriter.writeresults(CurrentAddressBuffer,currentfoundbuffer,sizeof(tbitaddress)*found,found*variablesize);
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
var
  part: integer;
  wr: twaitresult;
begin
  part:=0;
  if writeError then exit;
  
  try

    
    repeat
      repeat
        wr:=dataavailable.WaitFor(1000);
        if terminated or writeError or (wr in [wrAbandoned, wrError]) then exit;
      until wr=wrSignaled;

      try
        scancontroller.resultsaveCS.Enter;
        try
          part:=1;
          AddressFile.WriteBuffer(addressbuffer^,addressSize);

          part:=2;
          MemoryFile.WriteBuffer(memorybuffer^,memorySize);
        finally
          scancontroller.resultsaveCS.Leave;
        end;

      finally
        datawritten.SetEvent; //tell the others that you're ready to write again
      end;

    until terminated;
  except
    on e: exception do
    begin
      case part of
        0: errorstring:=rsThreadSynchronizer;
        1: errorstring:=scanner.Addressfilename;
        2: errorstring:=scanner.MemoryFilename;
        else errorstring:=rsUnknown;
      end;

      errorstring:=errorstring+':'+e.message;
      writeError:=true;
      dataavailable.SetEvent;
      datawritten.SetEvent;
    end;
  end;



end;

procedure Tscanfilewriter.writeresults(addressbuffer,memorybuffer: pointer; addressSize,memorySize: dword);
{
check if the thread is currently saving
If yes, wait, if not, start the thread, give it the buffer, and continue
}
var wr: TWaitResult;
begin

  repeat
    wr:=datawritten.WaitFor(1000); //only gets set when the thread is done writing
    if terminated or writeError or (wr in [wrAbandoned, wrError]) then exit;
  until wr=wrSignaled;

  //got past the wait, so it's done writing, so it has no need for the current variables
  self.addressbuffer:=addressbuffer;
  self.memorybuffer:=memorybuffer;
  self.addressSize:=addressSize;
  self.memorySize:=memorySize;

  dataavailable.SetEvent;  //tell the thread to start saving

  //and return to the scanner, who should now swap scanbuffers
end;

procedure Tscanfilewriter.flush;
var wr: TWaitResult;
begin
  if writeerror then exit;

  repeat
    wr:=datawritten.WaitFor(1000);
    if terminated or writeError or (wr in [wrAbandoned, wrError]) then exit;

    if writeError then exit;
  until wr=wrSignaled;

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

constructor Tscanfilewriter.create(scanner: TScanner; scancontroller:TScanController; addressfile,memoryfile:TFileStream);
begin
  self.scancontroller:=scancontroller;
  self.scanner:=scanner;
  self.addressfile:=addressfile;
  self.memoryfile:=memoryfile;

  //create the events
  datawritten:=tevent.Create(nil,false,true,'');
  dataavailable:=tevent.create(nil,false,false,'');

  inherited create(false); //start it  
end;

//=============TScanner===========//


procedure TScanner.FirstScanmem(base:ptruint; buffer: pointer; size: integer);
{
Scan the given buffer
Excludes the previousvalue buffer
}
var stepsize: integer;
    lastmem: ptruint;
    p: pbyte;
    i: TVariableType;
    j,k: integer;
    _fastscan: boolean;
    dividableby2: boolean;
    dividableby4: boolean;
    allfound: boolean;

    aob_checkroutine: TMultiAOBCheckRoutine;

    inv: boolean;
begin
  inv:=inverseScan;
  _fastscan:=fastscanmethod<>fsmNotAligned;
  p:=buffer;
  lastmem:=ptruint(p)+(size-variablesize); //optimizes compile to use reg if possible


  if _fastscan then
  begin
    if fastscanmethod=fsmAligned then
      stepsize:=fastscanalignsize
    else
    begin
      stepsize:=trunc(power(16, fastscandigitcount));
      inc(p, fastscanalignsize);
    end;
  end
  else
    stepsize:=1;

  case variableType of
    vtByteArrays:
    begin
      //do a ArrayOfBytesExact scan for every aob in the list
      if nibbleSupport then
        aob_checkroutine:=ArrayOfBytesExact_NibbleWildcardSupport
      else
        aob_checkroutine:=ArrayOfBytesExact;


      while (ptruint(p)<=lastmem) do
      begin
        for j:=0 to mabs_arraylength-1 do
        begin
          if (mabs[j].foundaddress=0) and aob_checkroutine(p,j) then //found one
          begin
            mabs[j].foundaddress:=base+ptruint(p)-ptruint(buffer);

            //check if all have been found
            allfound:=true;
            for k:=0 to mabs_arraylength-1 do
              if mabs[k].foundaddress=0 then
              begin
                allfound:=false;
                break;
              end;

            if allfound then
            begin
              found:=1;
              exit;
            end;
          end;

        end;

        inc(p,stepsize);
      end;
    end;

    vtAll:
    begin
      //reset typesmatch array for each check
      while (ptruint(p)<=lastmem) do
      begin
        typesmatch[vtByte]:=allByte;

        if _fastscan then
        begin
          dividableby2:=ptruint(p) mod 2=0;
          dividableby4:=ptruint(p) mod 4=0;

          typesmatch[vtWord]:=allWord and dividableby2;
          typesmatch[vtDWord]:=allDword and dividableby4;
          typesmatch[vtQWord]:=allQword and dividableby4;
          typesmatch[vtSingle]:=allFloat and dividableby4;
          typesmatch[vtDouble]:=allDouble and dividableby4;
        end
        else
        begin
          typesmatch[vtWord]:=allWord;
          typesmatch[vtDWord]:=allDword;
          typesmatch[vtQWord]:=allQword;
          typesmatch[vtSingle]:=allFloat;
          typesmatch[vtDouble]:=allDouble;
        end;

        if allCustom then
        begin
          currentaddress:=base+ptruint(p)-ptruint(buffer);
          for j:=0 to customtypecount-1 do customtypesmatch[j]:=true;
        end;

        if checkroutine(p,nil) then //found one
          StoreResultRoutine(base+ptruint(p)-ptruint(buffer),p);

        inc(p,stepsize);
      end;
    end;

    vtCustom:
    begin
      while (ptruint(p)<=lastmem) do
      begin
        currentAddress:=base+ptruint(p)-ptruint(buffer);
        if checkroutine(p,nil) xor inv then //found one
        begin
          StoreResultRoutine(currentAddress,p);
          if OnlyOne then
          begin
            AddressFound:=currentAddress;
            exit;
          end;
        end;

        inc(p,stepsize);
      end;
    end
    else
    begin

      lastpart:=10210;
      while (ptruint(p)<=lastmem) do
      begin
        if checkroutine(p,nil) xor inv then //found one
        begin
          StoreResultRoutine(base+ptruint(p)-ptruint(buffer),p);
          if OnlyOne then
          begin
            AddressFound:=base+ptruint(p)-ptruint(buffer);
            exit;
          end;
        end;

        inc(p,stepsize);
      end;
      lastpart:=10211;
    end;

  end;

end;

procedure TScanner.FirstNextScanmem(base:ptruint; buffer,oldbuffer: pointer; size: integer);
{
Scan the given buffer
}
var stepsize:  integer;
    lastmem:   ptruint;
    p,oldp:    pbyte;
    valuetype: TVariableType;
    _fastscan: boolean;
    dividableby2: boolean;
    dividableby4: boolean;
    i:         TVariableType;
    j:         integer;
    inv: boolean;
begin
  inv:=inverseScan;
  p:=buffer;
  oldp:=oldbuffer;
  lastmem:=ptruint(p)+(size-variablesize); //optimizes compile to use reg if possible


  _fastscan:=fastscanmethod<>fsmNotAligned;
  if _fastscan then
  begin
    if fastscanmethod=fsmAligned then
      stepsize:=fastscanalignsize
    else
    begin
      stepsize:=trunc(power(16, fastscandigitcount));
      inc(p, fastscanalignsize);
      inc(oldp, fastscanalignsize);
    end;
  end
  else
    stepsize:=1;


  lastpart:=303;

  if compareToSavedScan then //stupid, but ok...    (actually useful for lowmem scans)
  begin
    case self.variableType of
      vtByte:   valuetype:=vtbyte;
      vtWord:   valuetype:=vtword;
      vtDWord:  valuetype:=vtdword;
      vtSingle: valuetype:=vtsingle;
      vtdouble: valuetype:=vtdouble;
      vtQword:  valuetype:=vtQword;
      vtAll:    valuetype:=vtall;
      else
        valuetype:=vtDword;
    end;

    if valuetype=vtall then
    begin
      lastpart:=304;
      while ptruint(p)<=lastmem do
      begin

        if _fastscan then
        begin
          dividableby2:=ptrUint(p) mod 2=0;
          dividableby4:=ptrUint(p) mod 4=0;
          typesmatch[vtByte]:=allbyte;
          typesmatch[vtWord]:=allWord and dividableby2;
          typesmatch[vtDWord]:=allDword and dividableby4;
          typesmatch[vtQWord]:=allQword and dividableby4;
          typesmatch[vtSingle]:=allFloat and dividableby4;
          typesmatch[vtDouble]:=allDouble and dividableby4;
        end
        else
        begin
          typesmatch[vtByte]:=allByte;
          typesmatch[vtWord]:=allWord;
          typesmatch[vtDWord]:=allDword;
          typesmatch[vtQWord]:=allQword;
          typesmatch[vtSingle]:=allFloat;
          typesmatch[vtDouble]:=allDouble;
        end;

        if allCustom then
        begin
          currentaddress:=base+ptruint(p)-ptruint(buffer);
          for j:=0 to customtypecount-1 do customtypesmatch[j]:=true;
        end;


        if checkroutine(p,savedscanhandler.getpointertoaddress(base+ptruint(p)-ptruint(buffer),valuetype,nil )) then //found one
          StoreResultRoutine(base+ptruint(p)-ptruint(buffer),p);

        inc(p, stepsize);
      end;

      lastpart:=305;
    end
    else
    begin
      lastpart:=306;
      while ptruint(p)<=lastmem do
      begin
        currentaddress:=base+ptrUint(p)-ptrUint(buffer);
        if checkroutine(p,savedscanhandler.getpointertoaddress(currentaddress,valuetype,customtype )) xor inv then //found one
          StoreResultRoutine(currentaddress,p);

        inc(p, stepsize);
      end;
      lastpart:=307;
    end;
  end
  else
  begin
    lastpart:=308;
    if variableType=vtall then
    begin
      lastpart:=309;
      while ptruint(p)<=lastmem do
      begin
        if _fastscan then
        begin
          dividableby2:=ptruint(p) mod 2=0;
          dividableby4:=ptruint(p) mod 4=0;
          typesmatch[vtByte]:=allbyte;
          typesmatch[vtWord]:=allWord and dividableby2;
          typesmatch[vtDWord]:=allDword and dividableby4;
          typesmatch[vtQWord]:=allQword and dividableby4;
          typesmatch[vtSingle]:=allFloat and dividableby4;
          typesmatch[vtDouble]:=allDouble and dividableby4;
        end
        else
        begin
          typesmatch[vtByte]:=allByte;
          typesmatch[vtWord]:=allWord;
          typesmatch[vtDWord]:=allDword;
          typesmatch[vtQWord]:=allQword;
          typesmatch[vtSingle]:=allFloat;
          typesmatch[vtDouble]:=allDouble;
        end;

        if allCustom then
          for j:=0 to customtypecount-1 do customtypesmatch[j]:=true;


        if checkroutine(p,oldp) then //found one
          StoreResultRoutine(base+ptruint(p)-ptruint(buffer),p);

        inc(p, stepsize);
        inc(oldp, stepsize);
      end;
    end
    else
    if variableType=vtCustom then
    begin
      lastpart:=310;

      while ptruint(p)<=lastmem do
      begin
        currentaddress:=base+ptruint(p)-ptruint(buffer);
        if checkroutine(p,oldp) xor inv then //found one
          StoreResultRoutine(currentaddress,p);

        inc(p, stepsize);
        inc(oldp, stepsize);
      end;
    end
    else
    begin
      lastpart:=311;
      while ptruint(p)<=lastmem do
      begin
{$ifdef overflowdebug}
        if (ptruint(p)=lastmem) then
          OutputDebugString(format('scanner %d: p=%x lastmem=%x',[scannernr, ptruint(p),lastmem]));
{$endif}
        if checkroutine(p,oldp) xor inv then //found one
          StoreResultRoutine(base+ptruint(p)-ptruint(buffer),p);

        inc(p, stepsize);
        inc(oldp, stepsize);
      end;
    end;
  end;

  lastpart:=312;
end;

procedure TScanner.nextnextscanmemAll(addresslist: pointer; oldmemory: pointer; chunksize: integer);
var i,j,k: dword;
    l: TVariableType;
    m: integer;
    maxindex: dword;
    vsize: dword;
    currentbase: ptruint;
    newmemory: array [0..4095] of byte;
    oldmem: pbytearray;
    alist: pbitaddressarray;
    actualread: ptrUint;

    so: Tscanoption;
    valuetype: TVariableType;
    phandle: thandle;
    inv: boolean;
begin
  inv:=inverseScan;
  i:=0;
  phandle:=processhandle;
  so:=scanoption;
  maxindex:=chunksize-1;
  vsize:=variablesize; //=8
  oldmem:=oldmemory;
  alist:=addresslist;
  valuetype:=vtall;

  while i<=maxindex do
  begin
    currentbase:=alist[i].address and qword($FFFFFFFFFFFFF000);
    if (i<maxindex) then
    begin
      j:=i+1;
      while j<=maxindex do
      begin
        if currentbase=(qword(alist[j].address+vsize-1) and qword($fffffffffffff000)) then //same page
          inc(j)
        else
        begin
          dec(j);  //now points to the last valid one, or the first one
          break;
        end;
      end;
    end
    else
      j:=i;  //all alone

    if j>maxindex then
      j:=maxindex;

    currentbase:=alist[i].address;
    if readprocessmemory(phandle,pointer(currentbase),@newmemory[0],(alist[j].address-currentbase)+vsize,actualread) then
    begin


      if compareToSavedScan then
      begin
        //clear typesmatch and set current address
        for l:=vtByte to vtDouble do
          typesmatch[l]:=false;

        if allCustom then
          for m:=0 to customtypecount-1 do customtypesmatch[m]:=false;

        currentaddress:=currentbase;

        for k:=i to j do
        begin
          if alist[k].address=currentaddress then
          begin
            if alist[k].bit>=$1000 then
              customtypesmatch[alist[k].bit-$1000]:=false
            else
              typesmatch[tvariabletype(alist[k].bit)]:=true;
          end
          else
          begin
            //new address reached
            if checkroutine(@newmemory[currentaddress-currentbase],savedscanhandler.getpointertoaddress(currentaddress,valuetype, nil )) then
              StoreResultRoutine(currentaddress,@newmemory[currentaddress-currentbase]);

            //clear typesmatch and set current address
            for l:=vtByte to vtDouble do
              typesmatch[l]:=false;

            for m:=0 to customtypecount-1 do
              customtypesmatch[m]:=false;


            currentaddress:=alist[k].address;
            if alist[k].bit>=$1000 then
              customtypesmatch[alist[k].bit-$1000]:=true
            else
              typesmatch[tvariabletype(alist[k].bit)]:=true;
          end;

        end;

        if checkroutine(@newmemory[currentaddress-currentbase],savedscanhandler.getpointertoaddress(currentaddress,valuetype, nil )) then
          StoreResultRoutine(currentaddress,@newmemory[currentaddress-currentbase]);

      end
      else
      begin
        //clear typesmatch and set current address
        for l:=vtByte to vtDouble do
          typesmatch[l]:=false;

        if allCustom then
           for m:=0 to customtypecount-1 do customtypesmatch[m]:=false;

        currentaddress:=currentbase;

        for k:=i to j do
        begin
          if alist[k].address=currentaddress then
          begin
            //add this one to the list
            if alist[k].bit>=$1000 then
              customtypesmatch[alist[k].bit-$1000]:=true
            else
              typesmatch[tvariabletype(alist[k].bit)]:=true;

            continue;
          end
          else
          begin
            //new address
            //we now have a list of entries with all the same address, k-1 points to the last one
            if CheckRoutine(@newmemory[currentaddress-currentbase],@oldmem[(k-1)*vsize]) then
              StoreResultRoutine(currentaddress,@newmemory[currentaddress-currentbase]);

            //clear typesmatch and set current address
            for l:=vtByte to vtDouble do
              typesmatch[l]:=false;

            for m:=0 to customtypecount-1 do
              customtypesmatch[m]:=false;

            currentaddress:=alist[k].address;

            if alist[k].bit>=$1000 then
              customtypesmatch[alist[k].bit-$1000]:=true
            else
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
var
    i,j,k,l: dword;
    maxindex: dword;
    vsize: dword;
    currentbase: ptruint;
    newmemory: array [0..4095] of byte;

    alist: PBitAddressArray;
    actualread: ptrUint;
    lastaddress: ptruint;
    scannedbitlist: array [0..7] of boolean;
    phandle: thandle;
begin
  i:=0;
  maxindex:=chunksize-1;
  vsize:=variablesize;
  alist:=addresslist;
  currentbase:=0;
  phandle:=processhandle;

  while i<=maxindex do
  begin
    currentbase:=alist[i].address and qword($FFFFFFFFFFFFF000);

    if i<maxindex then
    begin
      j:=i+1;
      while j<=maxindex do
      begin
        if (currentbase)=(qword(alist[j].address+vsize-1) and qword($fffffffffffff000)) then //same page
          inc(j)
        else
        begin
          dec(j);  //now points to the last valid one, or the first one
          break;
        end;
      end;
    end
    else
      j:=i; //all alone

    if j>maxindex then
      j:=maxindex;




    currentbase:=alist[i].address;
    if readprocessmemory(phandle,pointer(currentbase),@newmemory[0],(alist[j].address-currentbase)+vsize,actualread) then
    begin
      k:=i;
      while k<=j do
      begin
        if CheckRoutine(@newmemory[alist[k].address-currentbase],nil) then
        begin
          for l:=0 to 7 do
            scannedbitlist[l]:=false;

          lastaddress:=alist[k].address;

          while (alist[k].address=lastaddress) and (k<=j) do //nugfix might cause performance loss, check it later
          begin
            //add bits to the scanned bitlist
            scannedbitlist[alist[k].bit]:=true;
            inc(k);
          end;
          dec(k);

          for l:=0 to 7 do
            if not scannedbitlist[l] then binaryresults[l]:=false; //if it wasn't scanned, but the result was true, then set it to false

          StoreResultRoutine(lastaddress,@newmemory[lastaddress-currentbase]);
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

var
    i,j,k: dword;
    maxindex: dword;
    vsize: dword;
    currentbase: ptruint;
    newmemory: array [0..4095] of byte;
    oldmem: pbytearray;
    alist: PPtrUintArray;
    actualread: ptrUint;

    so: Tscanoption;
    valuetype: TVariableType;
    phandle: thandle;
    inv: boolean;
begin
  inv:=inverseScan;
  i:=0;
  so:=scanoption;
  maxindex:=chunksize-1;
  vsize:=variablesize;
  oldmem:=oldmemory;
  alist:=addresslist;
  phandle:=processhandle;

  valuetype:=vtdword;

  case variableType of
    vtByte:   valuetype:=vtbyte;
    vtWord:   valuetype:=vtword;
    vtDWord:  valuetype:=vtdword;
    vtsingle: valuetype:=vtsingle;
    vtdouble: valuetype:=vtdouble;
    vtQword:  valuetype:=vtQword;
    vtAll:    valuetype:=vtall;
    vtCustom:
    begin
      case vsize of
        1: valuetype:=vtbyte;
        2: valuetype:=vtword;
        4: valuetype:=vtdword;
        8: valuetype:=vtQword;
        else valuetype:=vtdword;
      end;

    end;
  end;


  while i<=maxindex do
  begin
    currentbase:=alist[i] and qword($FFFFFFFFFFFFF000);

    if i<maxindex then  //If this isn't the last item in the list.
    begin
      j:=i+1;
      while j<=maxindex do  //first iteration is always true
      begin
        if (currentbase)=(qword(alist[j]+vsize-1) and qword($fffffffffffff000)) then //same page
          inc(j)
        else
        begin
          dec(j);  //now points to the last valid one, or the first one
          break;
        end;
      end;
    end
    else
      j:=i; //all alone

    if j>maxindex then
      j:=maxindex;


    currentbase:=alist[i];
    if readprocessmemory(phandle,pointer(currentbase),@newmemory[0],(alist[j]-currentbase)+vsize,actualread) then
    begin
      if compareToSavedScan then
      begin
        for k:=i to j do
        begin
          currentaddress:=alist[k];

          if checkroutine(@newmemory[alist[k]-currentbase],savedscanhandler.getpointertoaddress(alist[k],valuetype, customType )) xor inv then
            StoreResultRoutine(alist[k],@newmemory[alist[k]-currentbase]);

          inc(scanned);
        end;
      end
      else
      begin
        for k:=i to j do
        begin
          currentaddress:=alist[k];

          if CheckRoutine(@newmemory[alist[k]-currentbase],@oldmem[k*vsize]) xor inv then
            StoreResultRoutine(alist[k],@newmemory[alist[k]-currentbase]);

          inc(scanned);
        end;
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
    i,j: integer;
    foundbuffersize: integer;
    td: double;
    s: string;

    signed: boolean;

    tempvalue: int64;
    tempsvalue: single;
    tempdvalue: double;
begin
  signed:=false;
  s:=copy(trim(scanvalue1),1,1);
  if s='-' then signed:=true;

  s:=copy(trim(scanvalue2),1,1);
  if s='-' then signed:=true;



  value:=0;
  dvalue:=0;
  maxfound:=buffersize;
  {$ifdef customtypeimplemented}
  if variableType = vtCustom then
  begin
    //possible override
    if customtype.bytesize>16 then
    begin
      maxfound:=(buffersize*16) div customtype.bytesize; //get decent max size but not a redicilous size
      if maxfound<=0 then maxfound:=1;
    end;
  end
  else
  if (variableType = vtAll) and (vtCustom in ScanAllTypes) then
  begin
    i:=max(8, MaxCustomTypeSize);
    if i>16 then
    begin
      maxfound:=(buffersize*16) div i;
      if maxfound<=0 then maxfound:=1;
    end;

  end;
  {$ENDIF}

  //OutputDebugString('configurescanroutine');

  //OutputDebugString('Config 1');


  foundbuffersize:=0;

  //fill FloatSettings with formatting data (e.g difference between , and . for decimal)
  //GetLocaleFormatSettings(GetThreadLocale, FloatSettings);
  FloatSettings:=DefaultFormatSettings;

  if variableType=vtGrouped then
  begin
    groupdata:=TGroupData.create(scanvalue1, self);
    groupdata.floatscanWithoutExponents:=floatscanWithoutExponents;
  end
  else
  if scanOption in [soCustom, soExactValue,soValueBetween,soBiggerThan,soSmallerThan, soDecreasedValueBy, soIncreasedValueBy] then
  begin
    //user input is given
    if scanvalue1='' then raise exception.Create(rsPleaseFillSomethingIn);


    if (not luaformula) and
       (variableType in [vtByte,vtWord,vtDWord,vtQword,vtAll,vtCustom]) and
       ((variableType<>vtCustom) or (customType.scriptUsesString=false))


    then
    begin
      //parse scanvalue1


      scanvalue1:=trim(scanvalue1);
      scanvalue2:=trim(scanvalue2);
      try
        if hexadecimal then
          value:=StrToQWord('$'+scanvalue1)
        else
          value:=strtoqwordex(scanvalue1);

      except
        if (variableType=vtAll) or (percentage) then
        begin
          try
            dvalue:=strtofloat(scanvalue1,FloatSettings);
          except
            if FloatSettings.DecimalSeparator=',' then
              FloatSettings.DecimalSeparator:='.'
            else
              FloatSettings.DecimalSeparator:=',';

            //try again
            //todo: Add lua support for unix
            try
              dvalue:=strtofloat(scanvalue1,FloatSettings);
            except
              //see if lua knows better
              {$IFNDEF jni}
              try
                dvalue:=lua_strtofloat(scanvalue1);
              except
              {$ENDIF}
                raise exception.Create(Format(rsIsNotAValidValue, [scanvalue1]));
              {$IFNDEF jni}
              end;
              {$ENDIF}

            end;
          end;
          value:=trunc(dvalue);

        end else
        begin
          //not a float type, perhaps lua knows how to handle it
          {$IFNDEF jni}
          try
            value:=lua_strtoint(scanvalue1);
          except
          {$ENDIF}
            raise exception.Create(Format(rsIsAnInvalidValue, [scanvalue1]));
          {$IFNDEF jni}
          end;
          {$ENDIF}
        end;
      end;

      if scanOption=soValueBetween then
      begin
        //also parse scanvalue2
        try
          if hexadecimal then
            value2:=StrToQWord('$'+scanvalue2)
          else
            value2:=StrToQwordEx(scanvalue2);

        except
          if (variableType=vtAll) or (percentage) then
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
                dvalue:=strtofloat(scanvalue2,FloatSettings);
              except
                //see if lua knows better
                {$IFNDEF jni}
                try
                  dvalue:=lua_strtofloat(scanvalue2);
                except
                {$ENDIF}
                  raise exception.Create(Format(rsIsNotAValidValue, [scanvalue2]));
                {$IFNDEF jni}
                end;
                {$ENDIF}
              end;
            end;
            value2:=trunc(dvalue);
          end
          else
          begin
            //perhaps lua knows what it is
            {$IFNDEF jni}
            try
              value2:=lua_strtoint(scanvalue2);
            except
            {$ENDIF}
              raise exception.Create(Format(rsIsAnInvalidValue, [scanvalue2]));
            {$IFNDEF jni}
            end;
            {$ENDIF}

          end;
        end;
      end;
    end;

    if (not luaformula) and (percentage or (variableType in [vtsingle,vtDouble,vtAll, vtCustom])) and
       ((variableType<>vtCustom) or (customType.scriptUsesString=false))
    then
    begin
      try
        if hexadecimal then
          dvalue:=nan
        else
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
          //try lua
          {$IFNDEF jni}
          try
            dvalue:=lua_strtofloat(scanvalue1);
          except
          {$ENDIF}
            raise exception.Create(Format(rsIsNotAValidValue, [scanvalue1]));
          {$IFNDEF jni}
          end;
          {$ENDIF}
        end;
      end;

      if percentage or (scanoption=soValueBetween) then
      begin
        try
          if hexadecimal then
            dvalue2:=nan
          else
            dvalue2:=strtofloat(scanvalue2,FloatSettings);
        except
          if FloatSettings.DecimalSeparator=',' then
            FloatSettings.DecimalSeparator:='.'
          else
            FloatSettings.DecimalSeparator:=',';

          //try again
          try
            dvalue2:=strtofloat(scanvalue2,FloatSettings);
          except
            //and again
            {$IFNDEF jni}
            try
              dvalue2:=lua_strtofloat(scanvalue2);
            except
            {$ENDIF}
              raise exception.Create(Format(rsIsNotAValidValue, [scanvalue2]));
            {$IFNDEF jni}
            end;
            {$ENDIF}
          end;
        end;

      end;

      if percentage then
      begin
        if dvalue>dvalue2 then
        begin

          td:=dvalue;
          dvalue:=dvalue2;
          dvalue2:=td;

        end;
        dvalue:=dvalue / 100;
        dvalue2:=dvalue2 / 100;


      end;


      svalue:=dvalue;
      svalue2:=dvalue2;


      if (pos('E',uppercase(scanvalue1))=0) then
      begin
        floataccuracy:=pos(FloatSettings.DecimalSeparator,scanvalue1);
        if floataccuracy>0 then
          floataccuracy:=length(scanvalue1)-floataccuracy;
      end
      else
        floataccuracy:=0;

      if (floataccuracy<>0) and not percentage then
      begin
        svalue:=RoundTo(svalue,-floataccuracy);
        svalue2:=RoundTo(svalue2,-floataccuracy);
        dvalue:=RoundTo(dvalue,-floataccuracy);
        dvalue2:=RoundTo(dvalue2,-floataccuracy);
      end;

      if floataccuracy<>0 then
      begin
        mindvalue:=dvalue-(1/(power(10,floataccuracy)));
        maxdvalue:=dvalue+(1/(power(10,floataccuracy)));
        minsvalue:=svalue-(1/(power(10,floataccuracy)));
        maxsvalue:=svalue+(1/(power(10,floataccuracy)));
      end
      else
      begin
        mindvalue:=dvalue-1;
        maxdvalue:=dvalue+1;

        minsvalue:=svalue-1;
        maxsvalue:=svalue+1;
      end;

    end;
                  
    if (variableType = vtString) or ((variabletype=vtCustom) and (customtype.scriptUsesString) ) then
      widescanvalue1:=UTF8ToUTF16(scanvalue1);


    nibbleSupport:=false;
    if variabletype = vtByteArray then
    begin
      ConvertStringToBytes(trim(scanvalue1),hexadecimal,abs_arraytofind, true);
      abs_arraylength:=length(abs_arraytofind);
      for i:=0 to abs_arraylength-1 do
        if (abs_arraytofind[i]<0) and (abs_arraytofind[i]<>-1) then
        begin
          nibbleSupport:=true;
          break;
        end;

    end;

    if variableType = vtByteArrays then
    begin
      //(bytearray1)(bytearray2)(bytearray3)(...)(...)(...)(...).....
      i:=WordCount(scanvalue1, ['(',')']);
      setlength(mabs, i);

      for i:=0 to length(mabs)-1 do
      begin
        s:=ExtractWord(i+1, scanvalue1, ['(',')']);
        ConvertStringToBytes(trim(s),hexadecimal,mabs[i].arraytofind, true);
        mabs[i].arraylength:=length(mabs[i].arraytofind);
        mabs[i].foundaddress:=0;

        for j:=0 to mabs[i].arraylength-1 do
          if (mabs[i].arraytofind[j]<0) and (mabs[i].arraytofind[j]<>-1) then
          begin
            nibbleSupport:=true;
            break;
          end;
      end;

      mabs_arraylength:=length(mabs);



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

        binarystring:=parsers.inttobin(strtoint(trim(scanvalue1)))
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
          raise exception.Create(Format(rsIsNotAValidNotation, [binarystring]));
      end;

      self.andmask:=BinToInt(andmask);
      self.bitmask:=bintoint(bitmask);


    end;


  end;



  //OutputDebugString('Config 2');

  FlushRoutine:=genericFlush; //change if not so

  if variableType in [vtbinary,vtall] then
  begin
    getmem(CurrentAddressBuffer,maxfound*sizeof(Tbitaddress));
    getmem(SecondaryAddressBuffer,maxfound*sizeof(Tbitaddress));
  end
  else
  if variabletype = vtGrouped then
  begin
    //stored as:
    //Address, offset1, offset2, offset3.....
    //assuming the rare occasion of a blocksize bigger than 65535 dword offset size is chosen
    getmem(CurrentAddressBuffer,maxfound*(sizeof(ptruint)+sizeof(dword)*groupdata.groupdatalength));
    getmem(SecondaryAddressBuffer,maxfound*(sizeof(ptruint)+sizeof(dword)*1+groupdata.groupdatalength));
  end
  else
  begin
    getmem(CurrentAddressBuffer,maxfound*sizeof(ptruint));
    getmem(SecondaryAddressBuffer,maxfound*sizeof(ptruint));
  end;

  //OutputDebugString('Config 3');
  if compareToSavedScan then //create a first scan handler
  begin
    //OutputDebugString('Compare to saved scan');
    savedscanhandler:=Tsavedscanhandler.create(scandir,savedscanname);
  end;

  //OutputDebugString('Config 3.1');
 // OutputDebugString('scanOption='+inttostr(integer(scanOption)));


  if (scanOption in [soIncreasedValueBy, soDecreasedValueBy]) and (value=0) and (dvalue=0) then
    scanOption:=soUnchanged;


  if scanOption=soValueBetween then  //make sure the values are from low to high
  begin
    if signed and (value>value2) then
    begin
      tempvalue:=value;
      value:=value2;
      value2:=tempvalue;
    end
    else
    if (not signed) and (uint64(value)>uint64(value2)) then
    begin
      tempvalue:=value;
      value:=value2;
      value2:=tempvalue;
    end;

    if svalue>svalue2 then
    begin
      tempsvalue:=svalue;
      svalue:=svalue2;
      svalue2:=tempsvalue;
    end;

    if dvalue>dvalue2 then
    begin
      tempdvalue:=dvalue;
      dvalue:=dvalue2;
      dvalue2:=tempdvalue;
    end;
  end;

  case variableType of
    vtByte:
    begin
      //byte config
      FoundBufferSize:=buffersize*1;
      StoreResultRoutine:=ByteSaveResult;

      case scanOption of
        soForgot:           CheckRoutine:=Unknown;
        soExactValue:       if luaformula then
                              CheckRoutine:=ByteLuaFormula
                            else
                              checkRoutine:=byteExact;
        soValueBetween:     if percentage then
                              checkroutine:=byteBetweenPercentage
                            else
                            begin
                              if signed then
                                checkroutine:=SignedByteBetween
                              else
                                checkroutine:=byteBetween;
                            end;
        soBiggerThan:       checkroutine:=byteBiggerThan;
        soSmallerThan:      checkroutine:=byteSmallerThan;
        soIncreasedValue:   checkroutine:=byteIncreasedValue;
        soIncreasedValueBy: if percentage then
                              checkroutine:=byteIncreasedValueByPercentage
                            else
                              checkroutine:=byteIncreasedValueBy;
        soDecreasedValue:   checkroutine:=byteDecreasedValue;
        soDecreasedValueBy: if percentage then
                              checkroutine:=byteDecreasedValueByPercentage
                            else
                              checkroutine:=byteDecreasedValueBy;
        soChanged:          checkroutine:=byteChanged;
        soUnChanged:        checkroutine:=byteUnchanged;
      end;
    end;

    vtWord:
    begin
      //word config
      FoundBufferSize:=buffersize*2;
      StoreResultRoutine:=WordSaveResult;

      case scanOption of
        soForgot:           CheckRoutine:=Unknown;
        soExactValue:       if luaformula then
                              checkroutine:=WordLuaFormula
                            else
                              checkRoutine:=wordExact;
        soValueBetween:     if percentage then
                              checkroutine:=wordBetweenPercentage
                            else
                            begin
                              if signed then
                                checkroutine:=SignedWordBetween
                              else
                                checkroutine:=wordBetween;
                            end;
        soBiggerThan:       checkroutine:=wordBiggerThan;
        soSmallerThan:      checkroutine:=wordSmallerThan;
        soIncreasedValue:   checkroutine:=wordIncreasedValue;
        soIncreasedValueBy: if percentage then
                              checkroutine:=wordIncreasedValueByPercentage
                            else
                              checkroutine:=wordIncreasedValueBy;
        soDecreasedValue:   checkroutine:=wordDecreasedValue;
        soDecreasedValueBy: if percentage then
                              checkroutine:=wordDecreasedValueByPercentage
                            else
                              checkroutine:=wordDecreasedValueBy;
        soChanged:          checkroutine:=wordChanged;
        soUnChanged:        checkroutine:=wordUnchanged;
      end;
    end;

    vtDWord:
    begin
      //dword config
      //OutputDebugString('Config 4');

      FoundBufferSize:=buffersize*4;
      StoreResultRoutine:=DWordSaveResult;

      case scanOption of
        soForgot:           CheckRoutine:=Unknown;
        soExactValue:       if luaformula then
                              checkroutine:=DWordLuaFormula
                            else
                              checkRoutine:=dwordExact;
        soValueBetween:     if percentage then
                              checkroutine:=dwordBetweenPercentage
                            else
                            begin
                              if signed then
                                checkroutine:=SignedDwordBetween
                              else
                                checkroutine:=dwordBetween;
                            end;
        soBiggerThan:       checkroutine:=dwordBiggerThan;
        soSmallerThan:      checkroutine:=dwordSmallerThan;
        soIncreasedValue:   checkroutine:=dwordIncreasedValue;
        soIncreasedValueBy: if percentage then
                              checkroutine:=dwordIncreasedValueByPercentage
                            else
                              checkroutine:=dwordIncreasedValueBy;
        soDecreasedValue:   checkroutine:=dwordDecreasedValue;
        soDecreasedValueBy: if percentage then
                              checkroutine:=dwordDecreasedValueByPercentage
                            else
                              checkroutine:=dwordDecreasedValueBy;
        soChanged:          checkroutine:=dwordChanged;
        soUnChanged:        checkroutine:=dwordUnchanged;
      end;

      //OutputDebugString('Config 5');
    end;

    vtQWord:
    begin
      //qword config
      FoundBufferSize:=buffersize*8;
      StoreResultRoutine:=QWordSaveResult;

      case scanOption of
        soForgot:           CheckRoutine:=Unknown;
        soExactValue:       if luaformula then
                              checkRoutine:=QWordLuaFormula
                            else
                              checkRoutine:=qwordExact;
        soValueBetween:     if percentage then
                              checkroutine:=qwordBetweenPercentage
                            else
                            begin
                              if signed then
                                checkroutine:=SignedqwordBetween
                              else
                                checkroutine:=QwordBetween;
                            end;
        soBiggerThan:       checkroutine:=qwordBiggerThan;
        soSmallerThan:      checkroutine:=qwordSmallerThan;
        soIncreasedValue:   checkroutine:=qwordIncreasedValue;
        soIncreasedValueBy: if percentage then
                              checkroutine:=qwordIncreasedValueByPercentage
                            else
                              checkroutine:=qwordIncreasedValueBy;
        soDecreasedValue:   checkroutine:=qwordDecreasedValue;
        soDecreasedValueBy: if percentage then
                              checkroutine:=qwordDecreasedValueByPercentage
                            else
                              checkroutine:=qwordDecreasedValueBy;
        soChanged:          checkroutine:=qwordChanged;
        soUnChanged:        checkroutine:=qwordUnchanged;
      end;
    end;

    vtSingle:
    begin
      //single config
      FoundBufferSize:=buffersize*4;
      StoreResultRoutine:=SingleSaveResult;

      case scanOption of
        soForgot:           CheckRoutine:=Unknown;
        soExactValue:       if luaformula then
                              checkroutine:=SingleLuaFormula
                            else
                              checkRoutine:=singleExact;
        soValueBetween:     if percentage then
                              checkroutine:=singleBetweenPercentage
                            else
                              checkroutine:=singleBetween;
        soBiggerThan:       checkroutine:=singleBiggerThan;
        soSmallerThan:      checkroutine:=singleSmallerThan;
        soIncreasedValue:   checkroutine:=singleIncreasedValue;
        soIncreasedValueBy: if percentage then
                              checkroutine:=singleIncreasedValueByPercentage
                            else
                              checkroutine:=singleIncreasedValueBy;

        soDecreasedValue:   checkroutine:=singleDecreasedValue;
        soDecreasedValueBy: if percentage then
                              checkroutine:=singleDecreasedValueByPercentage
                            else
                              checkroutine:=singleDecreasedValueBy;
        soChanged:          checkroutine:=singleChanged;
        soUnChanged:        checkroutine:=singleUnchanged;
      end;
    end;

    vtDouble:
    begin
      //double config
      FoundBufferSize:=buffersize*8;
      StoreResultRoutine:=doubleSaveResult;

      case scanOption of
        soForgot:           CheckRoutine:=Unknown;
        soExactValue:       if luaformula then
                              checkRoutine:=DoubleLuaFormula
                            else
                              checkRoutine:=doubleExact;
        soValueBetween:     if percentage then
                              checkroutine:=DoubleBetweenPercentage
                            else
                              checkroutine:=doubleBetween;
        soBiggerThan:       checkroutine:=doubleBiggerThan;
        soSmallerThan:      checkroutine:=doubleSmallerThan;
        soIncreasedValue:   checkroutine:=doubleIncreasedValue;
        soIncreasedValueBy: if percentage then
                               checkroutine:=doubleIncreasedValueByPercentage
                            else
                               checkroutine:=doubleIncreasedValueBy;
        soDecreasedValue:   checkroutine:=doubleDecreasedValue;
        soDecreasedValueBy: if percentage then
                               checkroutine:=doubleDecreasedValueByPercentage
                            else
                               checkroutine:=doubleDecreasedValueBy;
        soChanged:          checkroutine:=doubleChanged;
        soUnChanged:        checkroutine:=doubleUnchanged;
      end;
    end;
      
    vtByteArray, vtByteArrays:
    begin
      if nibbleSupport then
        CheckRoutine:=ArrayOfByteExact_NibbleWildcardSupport
      else
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
      FoundBufferSize:=0;

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

      variablesize:=8; //override these variables (8 is big enough for even the double type)
      {$ifdef customtypeimplemented}
      if allCustom then  //find out the biggest customtype size
      begin
        customtypecount:=customtypes.count;
        setlength(customtypesmatch, customtypecount);
        for i:=0 to customTypes.count-1 do
          variablesize:=max(variablesize, TCustomType(customtypes[i]).bytesize);
      end;
      {$ENDIF}


      fastscanalignsize:=1;

      FoundBufferSize:=maxfound*variablesize;
      StoreResultRoutine:=allSaveResult;
      FlushRoutine:=allFlush;
      case scanOption of
        soForgot:           CheckRoutine:=allUnknown;
        soExactValue:       if luaformula then
                              checkRoutine:=AllLuaFormula
                            else
                              checkRoutine:=allExact;
        soValueBetween:     if percentage then
                              checkroutine:=allBetweenPercentage
                            else
                            begin
                              if signed then
                                checkroutine:=SignedAllBetween
                              else
                                checkroutine:=allBetween;
                            end;
        soBiggerThan:       checkroutine:=allBiggerThan;
        soSmallerThan:      checkroutine:=allSmallerThan;
        soIncreasedValue:   checkroutine:=allIncreasedValue;
        soIncreasedValueBy: if percentage then
                              checkroutine:=allIncreasedValueByPercentage
                            else
                              checkroutine:=allIncreasedValueBy;
        soDecreasedValue:   checkroutine:=allDecreasedValue;
        soDecreasedValueBy: if percentage then
                              checkroutine:=allDecreasedValueByPercentage
                            else
                              checkroutine:=allDecreasedValueBy;
        soChanged:          checkroutine:=allChanged;
        soUnChanged:        checkroutine:=allUnchanged;
      end;
      //the other types have to be filled in by the nextscan routines
    end;

    {$ifdef customtypeimplemented}
    vtCustom:
    begin
      //dword config
      FoundBufferSize:=maxfound*customtype.bytesize;

      if FoundBufferSize>16*1024*1024 then
        foundbuffersize:=16*1024*1024;


      StoreResultRoutine:=GenericSaveResult;

      if customType.scriptUsesString then
      begin
        case scanOption of
          soExactValue:
          begin
            if casesensitive then CheckRoutine:=CustomCaseSensitiveAnsiStringExact;
            if not casesensitive then CheckRoutine:=CustomCaseInsensitiveAnsiStringExact;
          end;
        end;
      end
      else
      if customType.scriptUsesFloat then
      begin
        case scanOption of
          soForgot:           CheckRoutine:=Unknown;
          soExactValue:       if luaformula then
                                checkRoutine:=customFloatExact
                              else
                                checkRoutine:=customFloatExact;
          soValueBetween:     if percentage then
                                checkroutine:=customFloatBetweenPercentage
                              else
                                checkroutine:=customFloatBetween;
          soBiggerThan:       checkroutine:=customFloatBiggerThan;
          soSmallerThan:      checkroutine:=customFloatSmallerThan;
          soIncreasedValue:   checkroutine:=customFloatIncreasedValue;
          soIncreasedValueBy: if percentage then
                                checkroutine:=customFloatIncreasedValueByPercentage
                              else
                                checkroutine:=customFloatIncreasedValueBy;
          soDecreasedValue:   checkroutine:=customFloatDecreasedValue;
          soDecreasedValueBy: if percentage then
                                checkroutine:=customFloatDecreasedValueByPercentage
                              else
                                checkroutine:=customFloatDecreasedValueBy;
          soChanged:          checkroutine:=customFloatChanged;
          soUnChanged:        checkroutine:=customFloatUnchanged;
        end;
      end
      else
      begin
        case scanOption of
          soForgot:           CheckRoutine:=Unknown;
          soExactValue:       if luaformula then
                                checkRoutine:=CustomLuaFormula
                              else
                                checkRoutine:=customExact;
          soValueBetween:     if percentage then
                                checkroutine:=customBetweenPercentage
                              else
                              begin
                                if signed then
                                  checkroutine:=SignedCustomBetween
                                else
                                  checkroutine:=customBetween;
                              end;
          soBiggerThan:       checkroutine:=customBiggerThan;
          soSmallerThan:      checkroutine:=customSmallerThan;
          soIncreasedValue:   checkroutine:=customIncreasedValue;
          soIncreasedValueBy: if percentage then
                                checkroutine:=customIncreasedValueByPercentage
                              else
                                checkroutine:=customIncreasedValueBy;
          soDecreasedValue:   checkroutine:=customDecreasedValue;
          soDecreasedValueBy: if percentage then
                                checkroutine:=customDecreasedValueByPercentage
                              else
                                checkroutine:=customDecreasedValueBy;
          soChanged:          checkroutine:=customChanged;
          soUnChanged:        checkroutine:=customUnchanged;
        end;
      end;
    end;
    {$ENDIF}

    vtGrouped:
    begin
      if groupdata.outoforder then
        CheckRoutine:=groupdata.compareblock_outoforder
      else
        checkroutine:=groupdata.compareblock;

      fastscanmethod:=fsmAligned;
      fastscanalignsize:=groupdata.alignsize;


      StoreResultRoutine:=groupSaveResult;
      flushroutine:=groupFlush;


      FoundBufferSize:=0;

      variablesize:=groupdata.blocksize;   //this is why there is no nextscan data to compare against (varsize of 4096)


      if scannernr=0 then //write the header for groupdata (after the normal header comes the number of offsets)
        Addressfile.WriteBuffer(groupdata.groupdatalength, sizeof(groupdata.groupdatalength));

    end;

  end;

 // OutputDebugString('Config 6');
  getmem(CurrentFoundBuffer,FoundBufferSize);
  getmem(SecondaryFoundBuffer,FoundBufferSize);

  //OutputDebugString('configurescanroutine: Normal exit');

  if luaformula then
  begin
    if newluastate then
      l:=luaL_newstate
    else
      l:=LuaVM;

    i:=luaL_loadstring(L, pchar('return function(value,previousvalue) return ('+scanvalue1+') end')); //pushed this function on the lua stack which will be reused indefinitrelly
    if i=0 then
    begin
      if lua_isfunction(L,-1)=false then
        raise exception.create('Invalid formula ( '+Lua_ToString(L,-1)+' )');

      if lua_pcall(L,0,1,0)<>0 then
        raise exception.create('Invalid formula ( '+Lua_ToString(L,-1)+' )');

      if lua_isfunction(L,-1)=false then
        raise exception.create('Invalid formula ( '+Lua_ToString(L,-1)+' )');
    end
    else
    begin
      if i=LUA_ERRSYNTAX then
        raise exception.create('Invalid formula Syntax Error: '+Lua_ToString(L,-1))
      else
        raise exception.create('Invalid formula Unknown '+Lua_ToString(L,-1))

    end;
  end;
end;

procedure TScanner.nextNextscan;
var oldAddressfile: TFileStream;
    oldMemoryfile: TFileStream;
    i: qword;
    j: integer;
    stopindex: qword;
    chunksize: integer;

    oldaddresses: array of PtrUint;
    oldaddressesb: array of tbitaddress;
    oldaddressesGroup: PByteArray;
    oldmemory: pointer;
    groupelementsize: integer;

begin
  lastpart:=200;
  if startentry>stopentry then //don't bother
    exit;

  oldAddressesGroup:=nil;

  configurescanroutine;
  oldAddressFile:=nil;
  oldMemoryFile:=nil;
  oldmemory:=nil;
  try

    oldAddressFile:=TFileStream.Create(scandir+'ADDRESSES.TMP',fmOpenRead or fmShareDenyNone);
    oldMemoryFile:=TFileStream.Create(scandir+'MEMORY.TMP',fmOpenRead or fmShareDenyNone);

    //set the current index to startentry


    stopindex:=stopentry-startentry;  //going from 0 to stopindex

    if self.variableType in [vtBinary,vtall] then
    begin
      //addressfile of tbitaddresstype
      setlength(oldaddressesb,buffersize);
      oldAddressFile.seek(7+sizeof(TBitAddress)*startentry,soFromBeginning); //header+addresssize*startentry
      if self.variableType=vtall then
      begin
        oldmemory:=virtualAlloc(nil,buffersize*variablesize,MEM_COMMIT or MEM_RESERVE or MEM_TOP_DOWN	, PAGE_READWRITE);
        if oldmemory=nil then raise exception.Create(Format(rsErrorAllocatingBytesForTheOldResults, [inttostr(buffersize*variablesize), inttostr(buffersize), inttostr(variablesize)]));
      end;

      oldMemoryFile.seek(variablesize*startentry,soFromBeginning);
    end
    else
    if self.variableType = vtGrouped then
    begin
      //addressfile of 7+offsetcount+(address+offsetcount)+....
      setlength(oldaddresses,buffersize); //normal addresslist, just a special addressfile

      groupelementsize:=(sizeof(ptruint)+sizeof(dword)*PreviousOffsetCount);
      getmem(oldaddressesGroup, buffersize*groupelementsize);


      oldAddressFile.seek(7+sizeof(dword)+startentry*groupelementsize,soFromBeginning);   //header+offsetcount+startentry*addressEntrysize (address followed by offsets)
    end
    else
    begin
      setlength(oldaddresses,buffersize);
      oldmemory:=virtualAlloc(nil,buffersize*variablesize,MEM_COMMIT or MEM_RESERVE or MEM_TOP_DOWN	, PAGE_READWRITE);
      if oldmemory=nil then raise exception.Create(Format(rsErrorAllocatingBytesForTheOldResults, [inttostr(buffersize*variablesize), inttostr(buffersize), inttostr(variablesize)]));

      oldAddressFile.seek(7+sizeof(ptruint)*startentry,soFromBeginning);   //header+addresssize*startentry
      if not (self.variableType in [vtString,vtByteArray, vtByteArrays]) then
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

          if not compareToSavedScan then
            oldMemoryFile.ReadBuffer(oldmemory^,chunksize*variablesize);
            
          nextnextscanmemall(@oldaddressesb[0],oldmemory,chunksize);
        end;

        else
        begin

          if variableType = vtGrouped then
          begin
            //load the grouped address list and convert to a regular addresslist
            oldAddressFile.ReadBuffer(oldaddressesGroup[0],chunksize*groupelementsize);
            for j:=0 to chunksize-1 do
              oldAddresses[j]:=PGroupAddress(@OldaddressesGroup[j*groupelementsize]).address;
          end
          else  //normal addresslist, no need to convert
          begin
            oldAddressFile.ReadBuffer(oldaddresses[0],chunksize*sizeof(ptruint));

            if not compareToSavedScan then
            begin
              if not (self.variableType in [vtString,vtByteArray, vtByteArrays]) then //skip the types with no previous result stored
                oldMemoryFile.ReadBuffer(oldmemory^,chunksize*variablesize);
            end;
          end;


          nextnextscanmem(@oldaddresses[0],oldmemory,chunksize);
        end;
      end;

      //inc(scanned,chunksize);
      inc(i,chunksize);
    end;

    lastpart:=297;
    flushroutine; //save all results temporarily stored in memory
  finally
    if oldAddressFile<>nil then oldAddressFile.free;
    if oldMemoryFile<>nil then oldMemoryFile.free;
    if oldmemory<>nil then virtualfree(oldmemory,0,MEM_RELEASE);

    if oldaddressesGroup<>nil then
    begin
      freememandnil(oldaddressesGroup);
      oldaddressesGroup:=nil;
    end;
  end;
end;

procedure TScanner.firstNextscan;
var
  i: integer;
  size: dword;
  currentbase: PtrUint;
  startregion: integer;
  stopregion: integer;
  memorybuffer: ^byte;
  oldbuffer: ^byte;
  toread: qword;
  actualread: ptrUint;
  phandle: thandle;
begin
  lastpart:=300;
  phandle:=processhandle;
  startregion:=_startregion; //using a variable so stack can be used, with possibility of register
  stopregion:=_stopregion;

  //allocate a buffer for reading the new memory buffer
  memorybuffer:=virtualAlloc(nil,maxregionsize+variablesize,MEM_COMMIT or MEM_RESERVE or MEM_TOP_DOWN, PAGE_READWRITE);
  if memorybuffer=nil then raise exception.create('Failure allocating memory ('+inttostr(maxregionsize+variablesize)+' bytes)');

  lastpart:=301;
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

      //also try to read the last few bytes and add variablesize if needed
      if (currentbase+toread)<(OwningScanController.memregion[i].BaseAddress+OwningScanController.memregion[i].MemorySize-variablesize) then
        inc(toread, variablesize-1);

      repeat
        size:=toread;
        if (size>buffersize) then size:=buffersize;

        actualread:=0;

        if size<toread then //there's something left to scan, so I can add the variablesize to it
          ReadProcessMemory(phandle,pointer(currentbase),memorybuffer,size+variablesize-1,actualread)
        else
          ReadProcessMemory(phandle,pointer(currentbase),memorybuffer,size,actualread);


        lastpart:=302;

        firstnextscanmem(currentbase,memorybuffer,oldbuffer,actualread);

        lastpart:=398;

        inc(scanned,size); //for the progressbar
        dec(toread,size);
        inc(oldbuffer,size);
        inc(currentbase,size);

      until toread=0;

    end;
    flushroutine;

    lastpart:=399;
  finally
    if memorybuffer<>nil then
      virtualfree(memorybuffer,0,MEM_RELEASE);

  end;
end;

procedure TScanner.firstscan;
var i: integer;
    x: ptruint;

    currentbase: ptruint;
    size, _size: qword;
    actualread: ptrUint;
    previousActualRead: ptruint;
    memorybuffer: ^byte;
    toread: qword;
    startregion: integer;
    stopregion: integer;
    phandle: thandle;

    canOverlap: boolean;
begin
  lastpart:=100;
  phandle:=processhandle;

  //first find out where in the previousmemory of this thread starts
  try

    startregion:=_startregion; //using a variable so stack can be used, with possibility of register
    stopregion:=_stopregion;

    if scanOption<>soUnknownValue then
    begin
      //not unknown initial
      memorybuffer:=virtualAlloc(nil,maxregionsize+variablesize+16,MEM_COMMIT or MEM_RESERVE or MEM_TOP_DOWN	, PAGE_READWRITE);
      //test:
      //FillMemory(memorybuffer, maxregionsize+variablesize+16, $CE);

      configurescanroutine;
    end
    else //it is a unknown initial value
    begin
      {$ifdef lowmemoryusage}
      //use the previousmemoryfile
      //the memory.tmp file must have been generated with the correct size before calling this
      previousmemoryfile:=Tfilestream.create(scandir+'MEMORY.TMP', fmOpenWrite or fmShareDenyNone);

      memorybuffer:=virtualAlloc(nil,maxregionsize,MEM_COMMIT or MEM_RESERVE or MEM_TOP_DOWN	, PAGE_READWRITE);
      {$else}
      //use the previousmemorybuffer instead
      memorybuffer:=pointer(PtrUint(OwningScanController.OwningMemScan.previousMemoryBuffer)+PtrUint(OwningScanController.memregion[startregion].startaddress)+(startaddress-OwningScanController.memregion[startregion].BaseAddress));
      {$endif}
      variablesize:=1; //ignore
    end;

    lastpart:=101;

    //now save the region between startaddress and stopaddress and create own memregion list
    setlength(memregions,16);

    for i:=startregion to stopregion do
    begin
      canOverlap:=false;

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

        if ((i+1)<OwningScanController.memRegionPos-1) and (currentbase+toread=OwningScanController.memregion[i+1].BaseAddress) then  //next region is connected, include it if possible
          canOverlap:=(scanOption<>soUnknownValue);
      end;

      if (i=stopregion) and ((currentbase+toread)>stopaddress) then
        toread:=stopaddress-currentbase;

    //  OutputDebugString(format('%.16x - %.16x (%.16x-%.16x)',[currentbase, currentbase+toread,OwningScanController.memregion[i].BaseAddress, OwningScanController.memregion[i].MemorySize ]));


      lastpart:=102;

      repeat
        //05955958
        size:=toread;

        if (size>buffersize) then
        begin
          size:=buffersize;
          canOverlap:=(scanOption<>soUnknownValue);
        end;

        actualread:=0;

        if canOverlap then
          _size:=size+(variablesize-1)
        else
          _size:=size;



        ReadProcessMemory(phandle,pointer(currentbase),memorybuffer,_size,actualread);

        if (actualread<>_size) then
        begin
          if canOverlap then  //try without overlap
          begin
            _size:=size;
            previousActualRead:=actualread;
            actualread:=0;
            ReadProcessMemory(phandle,pointer(currentbase+previousActualRead),pointer(ptruint(memorybuffer)+previousActualRead),_size-previousActualRead,actualread);

            inc(actualread, previousActualRead);
          end;
        end;

        //sanitize the results
        if actualread>_size then actualread:=_size;

        if scanOption=soUnknownValue then
        begin
          //unknown initial value, so create a memregion for this

          memregions[memregionpos].BaseAddress:=currentbase;
          memregions[memregionpos].MemorySize:=actualread;

          {$ifdef lowmemoryusage}
          //PtrUint(OwningScanController.memregion[i].startaddress) = offset in the file where OwningScanController.memregion[i].BaseAddress starts
          //currentbase is the current address read
          x:=PtrUint(OwningScanController.memregion[i].startaddress)+(currentbase-OwningScanController.memregion[i].BaseAddress);
          previousmemoryfile.Position:=x;

          memregions[memregionpos].startaddress:=pointer(previousmemoryfile.Position);

          previousmemoryfile.WriteBuffer(memorybuffer^, actualread);


          {$else}
          memregions[memregionpos].startaddress:=memorybuffer;
          inc(memorybuffer,size);
          {$endif}



          inc(memregionpos);
          if (memregionpos mod 16) = 0 then
            setlength(memregions,length(memregions)+16);
        end
        else
        begin
          //scan the buffer

          lastpart:=1021;
          firstscanmem(currentbase,memorybuffer,actualread);
          lastpart:=1022;

        end;


        currentbase:=currentbase+size;
        
        inc(scanned,size); //for the progressbar


        dec(toread,size);

        if (OnlyOne and (found>0)) then exit;


      until terminated or (toread<=0);
    end;

    lastpart:=103;

    if (scanOption<>soUnknownValue) then flushroutine; //save results
    lastpart:=104;
  finally
    {$ifdef LOWMEMORYUSAGE}
    if previousmemoryfile<>nil then
      freeandnil(previousmemoryfile);

    if memorybuffer<>nil then
      virtualfree(memorybuffer,0,MEM_RELEASE);
    {$else}
    if (scanOption<>soUnknownValue) and (memorybuffer<>nil) then
      virtualfree(memorybuffer,0,MEM_RELEASE);
    {$endif}
  end;
end;

procedure TScanner.execute;
var i: integer;
begin
  (*
  {$if defined(cpui386) or defined(cpux86_64)}
  Set8087CW($133f); //disable floating point exceptions in this thread
  SetSSECSR($1f80);
  {$endif}
  *)

  tthread.NameThreadForDebugging('Memscan TScanner thread '+inttostr(scannernr));


  try
    lastpart:=0;
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);


    try
      scanwriter:=TScanfilewriter.create(self,self.OwningScanController,addressfile,memoryfile);
      lastpart:=1;

      if scantype=stFirstScan then firstscan;
      if scantype=stNextScan then
      begin
        if useNextNextScan then
          nextnextscan
        else
          firstnextscan;
      end;

      //tell scanwriter to stop

      if savedscanhandler<>nil then freeandnil(savedscanhandler);

      lastpart:=2;
      scanwriter.flush;
      lastpart:=3;

      if scanwriter.writeError then
        raise exception.Create(Format(rsDiskWriteError, [scanwriter.errorString]));

      if OnlyOne and (AddressFound<>0) then
      begin
        //tell siblings to go kill themselves. This one won the price
        for i:=0 to length(OwningScanController.scanners)-1 do
          OwningScanController.scanners[i].Terminate;
      end;

    except
      on e: exception do
      begin
        haserror:=true;
        errorstring:=rsThread+inttostr(scannernr)+':'+e.message+' ('+inttostr(lastpart)+')';

        log('Scanner exception:'+errorstring);


        {$if lcl_fullversion < 2000000}
        DebugLn('Scanner exception:'+errorstring);
        DumpExceptionBackTrace;
        {$endif}

        //tell all siblings to terminate, something messed up
        //and I can just do this, since the ScanController is waiting for us, and terminate is pretty much atomic
        //for i:=0 to length(OwningScanController.scanners)-1 do

        OwningScanController.Terminate;

      end;
    end;


  finally
    isdone:=true;
  end;

end;

destructor TScanner.destroy;
begin

  //outputdebugstring('Destroying a scanner');

  if groupdata<>nil then
    freeandnil(groupdata);


  if AddressFile<>nil then //can be made nil by the scancontroller
  begin
    freeandnil(Addressfile);
    DeleteFile(scandir+'ADDRESSES-'+inttostr(ptruint(ThreadID))+'.TMP');
  end;

  if MemoryFile<>nil then
  begin
    freeandnil(MemoryFile);
    DeleteFile(scandir+'MEMORY-'+inttostr(ptruint(ThreadID))+'.TMP');
  end;

  if scanwriter<>nil then
    freeandnil(scanwriter);



  if CurrentFoundBuffer<>nil then freememandnil(CurrentFoundBuffer);
  if SecondaryFoundBuffer<>nil then freememandnil(SecondaryFoundBuffer);
  if CurrentAddressBuffer<>nil then freememandnil(CurrentAddressBuffer);
  if SecondaryAddressBuffer<>nil then freememandnil(SecondaryAddressBuffer);

  if savedscanhandler<>nil then freeandnil(savedscanhandler);

  if luaformula and (L<>nil) then
  begin
    lua_pop(L, lua_gettop(L));

    if newluastate then
      lua_close(L);
  end;

  inherited destroy;
end;

constructor TScanner.create(suspended: boolean; scandir: string);
begin
  inherited create(true); //do create the thread, I need the threadid

  self.scandir:=scandir;

  AddressFilename:=scandir+'ADDRESSES-'+inttostr(ptruint(ThreadID))+'.TMP';
  MemoryFilename:=scandir+'MEMORY-'+inttostr(ptruint(ThreadID))+'.TMP';
  AddressFile:=TFileStream.Create(AddressFilename,fmCreate or fmSharedenynone);
  MemoryFile:=TFileStream.Create(MemoryFilename,fmCreate or fmSharedenynone);

  Priority:=Globals.scanpriority;

  allByte:=vtByte in ScanAllTypes;
  allWord:=vtWord in ScanAllTypes;
  allDword:=vtDword in ScanAllTypes;
  allQword:=vtQword in ScanAllTypes;
  allFloat:=vtSingle in ScanAllTypes;
  allDouble:=vtDouble in ScanAllTypes;
  allCustom:=vtCustom in ScanAllTypes;


  if not suspended then start;   //would be stupid, but ok...
end;

//===============TScanController===============//

procedure TScanController.updategui;
var
  progress: integer;
  totaladdressestoscan, currentlyscanned, foundcount: qword;
begin
  //runs in mainthread
  progress:=OwningMemScan.GetProgress(totaladdressestoscan,currentlyscanned, foundcount);
  if OwningMemScan.progressbar<>nil then
  begin
    OwningMemScan.progressbar.Position:=progress;
    {$ifdef windows}
    SetProgressValue(OwningMemScan.progressbar.Position, OwningMemScan.progressbar.Max);
    {$endif}
  end;

  if assigned(owningmemscan.OnGuiUpdate) then
    owningmemscan.OnGuiUpdate(OwningMemScan, totaladdressestoscan,currentlyscanned, foundcount);
end;

procedure TScanController.errorpopup;
begin
  {$IFNDEF jni}
  messagedlg(errorstring,mtError,[mbok],0);
  {$ENDIF}
end;

procedure TScanController.fillVariableAndFastScanAlignSize;
var s: string;
    i,c: integer;

    g: TGroupscanCommandParser;

    pointertypes: TPointerTypes;
begin
  fastscan:=fastscanmethod<>fsmNotAligned;


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
      //OutputDebugString('aobscan for '+scanvalue1);

      if scanoption<>soUnknownValue then
        variablesize:=getBytecountArrayOfByteString(scanvalue1)
      else
        variablesize:=1;

      fastscanalignsize:=1;

      self.OwningMemScan.arrayLength:=variablesize;
    end;

    vtByteArrays:
    begin
     // OutputDebugString('maobscan for '+scanvalue1);
      if scanoption<>soUnknownValue then
      begin
        //find the max size of the aob's
        c:=WordCount(scanvalue1, ['(',')']);
        variablesize:=1;

        for i:=0 to c-1 do
        begin
          s:=ExtractWord(i+1, scanvalue1, ['(',')']);
          variablesize:=max(variablesize, getBytecountArrayOfByteString(s));
        end;
      end
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
        s:=parsers.inttobin(strtoint(scanvalue1))
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
      {$ifdef customtypeimplemented}
      if vtcustom in ScanAllTypes then  //find out the biggest customtype size
      begin
        for i:=0 to customTypes.count-1 do
          variablesize:=max(variablesize, TCustomType(customtypes[i]).bytesize);
      end;
      {$ENDIF}

      fastscanalignsize:=1;
    end;

    {$ifdef customtypeimplemented}
    vtCustom:
    begin
      if customtype=nil then
        raise exception.create(rsMSCustomTypeIsNil);

      variablesize:=customtype.bytesize;
      fastscanalignsize:=1;
    end;
    {$ENDIF}

    vtGrouped:
    begin
      //groupscan, check if it will use vtPointer (wildcard pointerscan)
      g:=TGroupscanCommandParser.create(scanvalue1);

      PointerTypes:=[];

      for i:=0 to length(g.elements)-1 do
      begin
        if g.elements[i].vartype=vtPointer then
          pointertypes:=pointertypes+g.elements[i].pointertypes;
      end;


      FillPointerLookupTrees(pointertypes);



      g.free;

    end;

  end;


  if fastscan then //override the alignment if given
  begin
    if (fastscanmethod=fsmLastDigits) or (fastscanalignment<>0) then
      fastscanalignsize:=fastscanalignment;
  end;


 // OutputDebugString('fillVariableAndFastScanAlignSize:');

 // OutputDebugString(format('variableType=%d',[integer(variableType)]));
 // OutputDebugString(format('variablesize=%d',[variablesize]));
 // OutputDebugString(format('fastscanalignsize=%d',[fastscanalignsize]));
end;



type
  TMemoryRegionInfo=record
    baseaddress: ptruint;
    size: size_t;
  end;

  PMemoryRegionInfo=^TMemoryRegionInfo;

function RegionCompare(Item1, Item2: Pointer): Integer;
var e1,e2: PMemoryRegionInfo;
begin
  e1:=item1;
  e2:=item2;

  if InRangeQ(e1^.baseaddress, e2^.baseaddress, e2^.baseaddress+e2^.size) or
     InRangeQ(e2^.baseaddress, e1^.baseaddress, e1^.baseaddress+e1^.size) then
    exit(0);

  if e1^.baseaddress<e2^.baseaddress then exit(-1) else exit(1);
end;

procedure TScanController.FillPointerLookupTrees(pointertypes: TPointertypes);
var
  a: ptruint;
  mbi: TMEMORYBASICINFORMATION;
  e: PMemoryRegionInfo;
  matchingPointerTypes: TPointertypes;
begin
  if isExecutablePointerLookupTree=nil then
  begin
    isExecutablePointerLookupTree:=TAvgLvlTree.Create(@RegionCompare);

    a:=0;
    zeromemory(@mbi,sizeof(mbi));
    while (Virtualqueryex(processhandle,pointer(a),mbi,sizeof(mbi))<>0) do //There is a setting which causes the whole virtualquerylookup to go very slow. Therefore, do it all in one loop
    begin
      if (ptruint(mbi.BaseAddress)<a) or (qword(mbi.baseaddress)>QWORD($8000000000000000)) then break;

      //check if it matches a pointertype
      matchingPointerTypes:=[];
      if (ptExecutable in pointertypes) and ((mbi.State=mem_commit) and ((mbi.Protect=PAGE_EXECUTE) or (mbi.Protect=PAGE_EXECUTE_READ) or (mbi.Protect=PAGE_EXECUTE_READWRITE) or (mbi.Protect=PAGE_EXECUTE_WRITECOPY))) then
        matchingPointerTypes:=[ptExecutable];


      if (ptDynamic in pointertypes) and ( (mbi.State=mem_commit) and not ((mbi._type=mem_mapped) or (mbi._type=mem_image))) then
        matchingPointerTypes:=matchingPointerTypes+[ptDynamic];

      if (ptStatic in pointertypes) and (mbi.State=mem_commit) and ((mbi._type=mem_mapped) or (mbi._type=mem_image)) then
        matchingPointerTypes:=matchingPointerTypes+[ptStatic];

      if matchingPointerTypes<>[] then
      begin
        getmem(e,sizeof(TMemoryRegionInfo));
        e^.baseaddress:=ptruint(mbi.BaseAddress);
        e^.size:=mbi.RegionSize;



        if ptExecutable in matchingPointerTypes then
        begin
          if isExecutablePointerLookupTree=nil then
            isExecutablePointerLookupTree:=TAvgLvlTree.Create(@RegionCompare);

          isExecutablePointerLookupTree.Add(e);
        end;

        if ptDynamic in matchingPointerTypes then
        begin
          if isDynamicPointerLookupTree=nil then
            isDynamicPointerLookupTree:=TAvgLvlTree.Create(@RegionCompare);

          isDynamicPointerLookupTree.Add(e);
        end;

        if ptStatic in matchingPointerTypes then
        begin
          if isStaticPointerLookupTree=nil then
            isStaticPointerLookupTree:=TAvgLvlTree.Create(@RegionCompare);

          isStaticPointerLookupTree.Add(e);
        end;
      end;

      a:=PtrUint(mbi.baseaddress)+mbi.RegionSize;
    end;
  end;
end;



function TScanController.isPointer(address: ptruint; pointertypes: TPointerTypes): boolean;
{
Will return true/false depending on if the address is a pointer or not
called by mutiple threads
}
var e: TMemoryRegionInfo;
begin
  e.baseaddress:=address;
  e.size:=4;
  result:=((ptDynamic in pointertypes) and (isDynamicPointerLookupTree.Find(@e)<>nil)) or
          ((ptStatic in pointertypes) and (isStaticPointerLookupTree.Find(@e)<>nil)) or
          ((ptExecutable in pointertypes) and (isExecutablePointerLookupTree.Find(@e)<>nil));
end;

procedure TScanController.CleanupIsPointerLookupTree(var lookupTree: TAvgLvlTree);
var e: TAVLTreeNodeEnumerator;
  n: TAvgLvlTreeNode;
begin
  if lookupTree<>nil then
  begin
    e:=lookupTree.GetEnumerator;

    while e.MoveNext do
    begin
      n:=e.Current;
      if n<>nil then
      begin
        freemem(n.Data);
        n.data:=nil;
      end;
    end;

    freemem(e);
    freemem(lookupTree);

  end;

  lookupTree:=nil;
end;

procedure TScanController.CleanupIsPointerLookupTrees;
begin
  CleanupIsPointerLookupTree(isStaticPointerLookupTree);
  CleanupIsPointerLookupTree(isDynamicPointerLookupTree);
  CleanupIsPointerLookupTree(isExecutablePointerLookupTree);
end;

procedure TScanController.NextNextScan;
{
NextNextScan will read results of the previous scan, and pass it off to scanner threads
}
var 
  AddressFile: TFileStream;
  blocksize: qword;
  i: integer;

  currententry: qword;
  datatype: string[6];
  offsetcount: integer;
begin
  offsetcount:=0;

  {$ifdef customtypeimplemented}
  if (variableType=vtCustom) and (customType<>nil) and (customtype.CustomTypeType=cttLuaScript) then
    threadcount:=1
  else
  {$ENDIF}
    threadcount:=GetCPUCount;

  if luaformula and (newluastate=false) then
    threadcount:=1;

  
  //read the results and split up

  AddressFile:=TFileStream.Create(OwningMemScan.ScanresultFolder+'ADDRESSES.TMP',fmOpenRead or fmShareDenyNone);
  try
    if variableType in [vtbinary,vtall] then //it uses a specific TBitAddress instead of a dword
      totalAddresses:=(addressfile.size-7) div sizeof(TBitAddress)
    else
    if variableType = vtGrouped then
    begin
      //read the number of offsets used previously (can be different from the current input)
      addressFile.Position:=7;
      addressFile.ReadBuffer(offsetcount, sizeof(offsetcount));
      addressfile.Position:=0; //reset position

      //the addresslist is buildup of address,offset1,offset2,...,offsetcount-1, address, offset1, offset2, ...., offsetcount-1, address,.....
      totalAddresses:=(addressfile.size-7-sizeof(offsetcount)) div (sizeof(ptruint)+offsetcount*sizeof(dword));
    end
    else
      totalAddresses:=(addressfile.size-7) div sizeof(ptruint);

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
          scanners[i]:=tscanner.Create(true,OwningMemScan.ScanresultFolder);
          scanners[i].scannernr:=i;
          scanners[i].OwningScanController:=self;

          if totalAddresses>0 then
          begin
            scanners[i].startentry:=currententry;


            if i=threadcount-1 then
              scanners[i].stopentry:=totalAddresses-1
            else
              scanners[i].stopentry:=currententry+blocksize;

            if scanners[i].stopentry>=totaladdresses then
              scanners[i].stopentry:=totalAddresses-1;
          end
          else
          begin
            scanners[i].startentry:=1;
            scanners[i].stopEntry:=0; //signals the scanner thread to quit

          end;

          currententry:=scanners[i].stopentry+1; //next thread will start at the next one

          scanners[i].compareToSavedScan:=compareToSavedScan;
          scanners[i].savedscanname:=savedscanname;
          scanners[i].scanType:=scanType; //stNextScan obviously
          scanners[i].scanoption:=scanoption;
          scanners[i].variableType:=VariableType;
          scanners[i].customType:=CustomType;
          scanners[i].roundingtype:=roundingtype;
          scanners[i].scanValue1:=scanvalue1; //usual scanvalue
          scanners[i].scanValue2:=scanValue2; //2nd value for between scan
          scanners[i].unicode:=unicode;
          scanners[i].caseSensitive:=caseSensitive;
          scanners[i].percentage:=percentage;
          scanners[i].hexadecimal:=hexadecimal;
          scanners[i].binaryStringAsDecimal:=binaryStringAsDecimal;

          scanners[i].fastscanalignsize:=fastscanalignsize;
          scanners[i].fastscanmethod:=fastscanmethod;
          scanners[i].fastscandigitcount:=fastscandigitcount;
          scanners[i].variablesize:=variablesize;
          scanners[i].useNextNextscan:=true; //address result scan so nextnextscan
          scanners[i].floatscanWithoutExponents:=floatscanWithoutExponents;
          scanners[i].inverseScan:=inverseScan;
          scanners[i].luaformula:=luaformula;
          scanners[i].newluastate:=newluastate;

          if variableType=vtGrouped then
            scanners[i].PreviousOffsetCount:=offsetcount;

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
        scanners[i].start;


      OwningMemScan.found:=0;
      //and now we wait
      for i:=0 to threadcount-1 do
      begin
        while not (terminated or scanners[i].isdone or scanners[i].Finished) do
        begin
          scanners[i].WaitTillDone(25);
          if (OwningMemScan.progressbar<>nil) or (assigned(owningmemscan.OnGuiUpdate)) then
            synchronize(updategui);
        end;

        //If terminated then stop the scanner thread and wait for it to finish
        if terminated then
        begin
          scanners[i].Terminate;
          scanners[i].WaitFor;
        end;

        if scanners[i].haserror then
        begin
          haserror:=true;
          errorstring:=scanners[i].errorstring;
          break;
        end;

        inc(OwningMemScan.found,scanners[i].totalfound);
      end;

      if (OwningMemScan.progressbar<>nil) or (assigned(owningmemscan.OnGuiUpdate)) then
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
  totalProcessMemorySize: qword;
  blocksize: qword;
  leftfromprevious: qword;
  offsetincurrentregion: PtrUint;
  
  currentblocksize: qword;
  datatype: string[6];
begin
  threadcount:=GetCPUCount;
  {$ifdef customtypeimplemented}
  if (variableType=vtCustom) and (customType<>nil) and (customtype.CustomTypeType=cttLuaScript) then
    threadcount:=1;
  {$ENDIF}

  if luaformula then
    threadcount:=1;


  totalProcessMemorySize:=0;

  memregion:=OwningMemscan.memRegion;
  memregionpos:=OwningMemscan.memRegionPos;

  {$ifdef LOWMEMORYUSAGE}
  if compareToSavedScan=false then
  begin
    compareToSavedScan:=true;
    savedscanname:='First';
  end;
  {$endif}




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
    offsetincurrentregion:=0;

    for i:=0 to threadcount-1 do
    begin
      scanners[i]:=tscanner.Create(true, OwningMemScan.ScanresultFolder);
      scanners[i].scannernr:=i;
      scanners[i].OwningScanController:=self;

      scanners[i]._startregion:=j;
      scanners[i].startaddress:=memRegion[j].BaseAddress+offsetincurrentregion;

      scanners[i].maxregionsize:=0;

      if i=(threadcount-1) then
      begin
        //this is the last scanner
        scanners[i].stopaddress:=stopaddress;  //let it go till the end
        scanners[i]._stopregion:=memregionpos-1;


        //define maxregionsize
        while j<memregionpos do
        begin
          if scanners[i].maxregionsize<memregion[j].MemorySize then
            scanners[i].maxregionsize:=memregion[j].MemorySize;

          inc(j);
        end;

      end
      else
      begin
        currentblocksize:=0;
        inc(currentblocksize,memregion[j].MemorySize-offsetincurrentregion);
        inc(j);

        scanners[i].maxregionsize:=currentblocksize; 

        //find the maximum blocksize
        while (currentblocksize<blocksize) and (j<memregionpos) do
        begin
          if scanners[i].maxregionsize<memregion[j].MemorySize then
            scanners[i].maxregionsize:=memregion[j].MemorySize;

          inc(currentblocksize,memregion[j].MemorySize);
          inc(j);
        end;
        dec(j);

        if scanners[i].maxregionsize=0 then //(currentblocksize<blocksize)
          scanners[i].maxregionsize:=memregion[j].MemorySize;

        scanners[i]._stopregion:=j;
        scanners[i].stopaddress:=memregion[j].BaseAddress+memregion[j].MemorySize;

        leftfromprevious:=currentblocksize-blocksize;
        dec(scanners[i].stopaddress,leftfromprevious);

        if leftfromprevious=0 then
        begin
          inc(j); //nothing left in this region
          offsetincurrentregion:=0;
        end else offsetincurrentregion:=memregion[j].MemorySize-leftfromprevious;



      end;

      if scanners[i].maxregionsize>buffersize then
        scanners[i].maxregionsize:=buffersize;

      //now configure the scanner thread with the same info this thread got, with some extra info


      scanners[i].compareToSavedScan:=compareToSavedScan;
      scanners[i].savedscanname:=savedscanname;

      scanners[i].scanType:=scanType; //stNextScan obviously
      scanners[i].scanoption:=scanoption;
      scanners[i].variableType:=VariableType;
      scanners[i].customType:=CustomType;
      scanners[i].roundingtype:=roundingtype;
      scanners[i].scanValue1:=scanvalue1; //usual scanvalue
      scanners[i].scanValue2:=scanValue2; //2nd value for between scan
      scanners[i].unicode:=unicode;
      scanners[i].caseSensitive:=caseSensitive;
      scanners[i].percentage:=percentage;
      scanners[i].hexadecimal:=hexadecimal;
      scanners[i].binaryStringAsDecimal:=binaryStringAsDecimal;

      scanners[i].fastscanalignsize:=fastscanalignsize;
      scanners[i].fastscanmethod:=fastscanmethod;
      scanners[i].fastscandigitcount:=fastscandigitcount;
      scanners[i].variablesize:=variablesize;
      scanners[i].useNextNextscan:=false; //region scan so firstnextscan
      scanners[i].floatscanWithoutExponents:=floatscanWithoutExponents;
      scanners[i].inverseScan:=inverseScan;
      scanners[i].luaformula:=luaformula;
      scanners[i].newluastate:=newluastate;

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
    scanners[i].start;


  savescannerresults:=true;


  OwningMemScan.found:=0;
  //and now we wait
  for i:=0 to threadcount-1 do
  begin
    while not (terminated or scanners[i].isdone or scanners[i].Finished) do
    begin
      scanners[i].WaitTillDone(25);
      if (OwningMemScan.progressbar<>nil) or (assigned(owningmemscan.OnGuiUpdate))  then
        synchronize(updategui);
    end;

    //If terminated then stop the scanner thread and wait for it to finish
    if terminated then
    begin
      scanners[i].Terminate;
      scanners[i].WaitFor;
    end;


    //scanners[i].WaitFor; //if the mainthread has to cancel, it has to tell the child scanners to terminate instead
    if scanners[i].haserror then
    begin
      haserror:=true;
      errorstring:=scanners[i].errorstring;
      break;
    end;

    inc(OwningMemScan.found,scanners[i].totalfound);
  end;

  if (OwningMemScan.progressbar<>nil) or (assigned(owningmemscan.OnGuiUpdate)) then
    synchronize(updategui);
    
  if haserror then
  begin
    synchronize(errorpopup);
    exit;
  end;

  //scan is successfull.



  //now clean up some mem, it's not needed anymore
  {$ifndef LOWMEMORYUSAGE}
  if OwningMemScan.previousMemoryBuffer<>nil then
  begin
    virtualfree(OwningMemScan.previousMemoryBuffer,0,MEM_RELEASE);
    OwningMemscan.previousMemoryBuffer:=nil;
  end;
  {$endif}

end;



procedure TScanController.nextScan;
begin
  if owningmemscan.LastScanWasRegionScan then
    FirstNextScan
  else
    NextNextScan;
end;




function vqevalidcachecompare(Item1, Item2: Pointer): Integer;
begin
  if InRangeX(TVQEValidCacheEntry(Item1).address, TVQEValidCacheEntry(Item2).address, TVQEValidCacheEntry(Item2).address+TVQEValidCacheEntry(Item2).size-1) then
    exit(0)
  else
    result:=CompareValue(TVQEValidCacheEntry(Item1).address, TVQEValidCacheEntry(Item2).address);
end;


function TVQEValidCacheEntry.containsaddress(a: ptruint): boolean;
begin
  result:=(a>=address) and (a<address+size);
end;

function TScanController.isValidregion(address: ptruint): boolean;
var
  mbi : TMemoryBasicInformation;
  isWritable, isExecutable, isCopyOnWrite: boolean;

  e: TVQEValidCacheEntry;
  n: TAVLTreeNode;
begin
  result:=false;

  if address<startaddress then exit(false);
  if address>stopaddress then exit(false);

  if (vqecache_lastregion<>nil) and (vqecache_lastregion.containsaddress(address)) then
    exit(vqecache_lastregion.valid);

  e:=TVQEValidCacheEntry.Create;
  e.address:=address;
  n:=vqevalidcache.Find(e);

  e.free;

  if n<>nil then
  begin
    vqecache_lastregion:=TVQEValidCacheEntry(n.Data);
    exit(TVQEValidCacheEntry(n.Data).valid);
  end;

  if VirtualQueryEx(processhandle, pointer(address), mbi, sizeof(mbi))<>0 then
  begin
    e:=TVQEValidCacheEntry.Create;
    e.address:=ptruint(mbi.BaseAddress);
    e.size:=mbi.RegionSize;
    e.valid:=false;


    result:=(mbi.State=mem_commit);
    if not result then
    begin
      vqevalidcache.Add(e);
      exit;
    end;

    result:=result and (PtrUint(mbi.BaseAddress)<stopaddress);
    result:=result and ((mbi.Protect and page_guard)=0);
    result:=result and ((mbi.protect and page_noaccess)=0);
    result:=result and (not (not scan_mem_private and (mbi._type=mem_private)));
    result:=result and (not (not scan_mem_image and (mbi._type=mem_image)));
    result:=result and (not (not scan_mem_mapped and (mbi._type=mem_mapped)));
    result:=result and (not (Skip_PAGE_NOCACHE and ((mbi._type and PAGE_NOCACHE)>0)));
    result:=result and (not (Skip_PAGE_WRITECOMBINE and ((mbi._type and PAGE_WRITECOMBINE)>0)));

    if result then
    begin
      //initial check passed, check the other protection flags to see if it should be scanned

      //fill in isWritable, isExecutable, isCopyOnWrite: boolean;
      isWritable:=((mbi.protect and PAGE_READWRITE)>0) or
                  ((mbi.protect and PAGE_WRITECOPY)>0) or //writecopy IS writable
                  ((mbi.protect and PAGE_EXECUTE_READWRITE)>0) or
                  ((mbi.protect and PAGE_EXECUTE_WRITECOPY)>0);

      isExecutable:=((mbi.protect and PAGE_EXECUTE)>0) or
                    ((mbi.protect and PAGE_EXECUTE_READ)>0) or
                    ((mbi.protect and PAGE_EXECUTE_READWRITE)>0) or
                    ((mbi.protect and PAGE_EXECUTE_WRITECOPY)>0);

      isCopyOnWrite:=((mbi.protect and PAGE_WRITECOPY)>0) or
                     ((mbi.protect and PAGE_EXECUTE_WRITECOPY)>0);

      case scanWritable of
        scanInclude: result:=result and isWritable;
        scanExclude: result:=result and (not isWritable);
      end;

      case scanExecutable of
        scanInclude: result:=result and isExecutable;
        scanExclude: result:=result and (not isExecutable);
      end;

      case scanCopyOnWrite of
        scanInclude: result:=result and isCopyOnWrite;
        scanExclude: result:=result and (not isCopyOnWrite);
      end;
    end;

    e.valid:=result;
    vqevalidcache.Add(e);
  end;
end;

procedure TScanController.firstScan;
{
first scan will gather the memory regions, open the files, and spawn scanners
}
var
  currentBaseAddress: PtrUint;
  mbi : TMemoryBasicInformation;

  i,j: integer;

  Blocksize: qword;
  currentblocksize: qword;
  totalProcessMemorySize: qword;
  leftfromprevious: qword;
  offsetincurrentregion: qword;

  isWritable, isExecutable, isCopyOnWrite{$ifdef darwin}, isDirty{$endif}: boolean;

  validRegion: boolean;

  datatype: string[6];

  f: TFilestream;

  vqecacheflag: dword;

  starta,startb, stopa,stopb: ptruint;

  wsisize: dword;
  wsi: PPSAPI_WORKING_SET_INFORMATION;

  getmemtimestart: qword;
  getmemtimestop: qword;

  parseregiontimestart: qword;
  parseregiontimestop: qword;
begin
 // OutputDebugString('TScanController.firstScan');



  if (OnlyOne and (not isUnique)) or (luaformula and (newluastate=false)) then
    threadcount:=1
  else
    threadcount:=GetCPUCount;


  //if it's a custom scan with luascript as type just use one cpu so there is less overhead
  {$ifdef customtypeimplemented}
  if (variableType=vtCustom) and (customType<>nil) and (customtype.CustomTypeType=cttLuaScript) then
    threadcount:=1;
  {$ENDIF}

  totalProcessMemorySize:=0;


  //OutputDebugString(format('threadcount=%d',[threadcount]));
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


  if OnlyOne or isUnique then //don't align at all. Some users want a byte perfect range...
  begin
    //if (startaddress mod 8)>0 then //align on a 8 byte base
    // startaddress:=startaddress-(startaddress mod 8)+8;
  end
  else
  begin
    if (startaddress mod 8)>0 then //align on a 8 byte base
     startaddress:=startaddress-(startaddress mod 8);
  end;

 // OutputDebugString('processhandle='+inttostr(processhandle));
 // OutputDebugString(format('startaddress=%x',[startaddress]));
 // OutputDebugString(format('stopaddress=%x',[stopaddress]));

 // OutputDebugString('Finding out memory size');
  currentBaseAddress:=startaddress;
  ZeroMemory(@mbi,sizeof(mbi));

 // OutputDebugString('scanWritable='+inttostr(integer(scanWritable)));
 // OutputDebugString('scanExecutable='+inttostr(integer(scanExecutable)));
 // OutputDebugString('scanCopyOnWrite='+inttostr(integer(scanCopyOnWrite)));

  {$ifndef darwin}
  vqecacheflag:=0;

  if not Scan_MEM_MAPPED then
    vqecacheflag:=vqecacheflag or VQE_NOSHARED;   //4

  if scan_pagedonly then
    vqecacheflag:=vqecacheflag or VQE_PAGEDONLY;   //1

  if scan_dirtyonly and (scanWritable=scanInclude) then
    vqecacheflag:=vqecacheflag or VQE_DIRTYONLY;  //2


  VirtualQueryEx_StartCache(processhandle, vqecacheflag);
  {$endif}

  {$ifdef windows}
  if workingsetonly and assigned(QueryWorkingSet) then
  begin
    getmemtimestart:=GetTickCount64;
    vqevalidcache:=TAvgLvlTree.Create(@vqevalidcachecompare);

    wsisize:=sizeof(PSAPI_WORKING_SET_INFORMATION);
    getmem(wsi, sizeof(PSAPI_WORKING_SET_INFORMATION));
    while (QueryWorkingSet(processhandle, wsi, wsisize)=false) do
    begin
      if GetLastError<>ERROR_BAD_LENGTH then
        raise exception.create('Failure querying present memory: unexpected error');

      wsisize:=(wsi^.NumberOfEntries+(wsi^.NumberOfEntries shr 1))*sizeof(ptruint);  //add a little bit extra
      freemem(wsi);
      if wsisize=0 then raise exception.create('Failure querying present memory: invalid size');
      getmem(wsi, wsisize);
    end;

    getmemtimestop:=GetTickCount64;

    parseregiontimestart:=GetTickCount64;
    validregion:=false;
    for i:=0 to wsi^.NumberOfEntries-1 do
    begin
     // if (wsi^.WorkingSetInfo[i] and (1 shl 8)) <>0 then continue;

      if (not validregion) or ((wsi^.WorkingSetInfo[i-1] and $fff)<>(wsi^.WorkingSetInfo[i] and $fff)) or ((wsi^.WorkingSetInfo[i-1] shr 12)+1<>(wsi^.WorkingSetInfo[i-1] shr 12)) then
      begin
        //new section or became valid ?

        if isValidRegion(wsi^.WorkingSetInfo[i] and qword($fffffffffffff000)) then
        begin
          memRegion[memRegionPos].BaseAddress:=wsi^.WorkingSetInfo[i] and qword($fffffffffffff000);
          memRegion[memRegionPos].MemorySize:=4096;
          memRegion[memRegionPos].startaddress:=pointer(ptrUint(totalProcessMemorySize));

          inc(totalProcessMemorySize, 4096);
          inc(memRegionPos);
          validregion:=true;


          if (memRegionPos mod 16)=0 then //add another 16 to it
            setlength(memRegion,length(memRegion)+16);
        end
        else
          validregion:=false;
      end
      else
      begin
        if validregion then //append to the current section
        begin
          inc(memRegion[memRegionPos-1].MemorySize,4096);
          inc(totalProcessMemorySize, 4096);
        end;
      end;


    end;

    parseregiontimestop:=GetTickCount64;

    //cleanup vqe valid cache
    vqevalidcache.FreeAndClear;
    vqevalidcache.free;

    vqecache_lastregion:=nil;
  end
  else
  {$endif}
  while (Virtualqueryex(processhandle,pointer(currentBaseAddress),mbi,sizeof(mbi))<>0) and (currentBaseAddress<stopaddress) and ((currentBaseAddress+mbi.RegionSize)>currentBaseAddress) do   //last check is done to see if it wasn't a 64-bit overflow.
  begin
  //  OutputDebugString(format('R=%x-%x',[ptruint(mbi.BaseAddress), ptruint(mbi.BaseAddress)+mbi.RegionSize]));


   // if (not (not scan_mem_private and (mbi._type=mem_private))) and (not (not scan_mem_image and (mbi._type=mem_image))) and (not (not scan_mem_mapped and (mbi._type=mem_mapped))) and (mbi.State=mem_commit) and ((mbi.Protect and page_guard)=0) and ((mbi.protect and page_noaccess)=0) then  //look if it is commited
    begin



      if PtrUint(mbi.BaseAddress)<startaddress then
      begin
        dec(mbi.RegionSize, startaddress-PtrUint(mbi.BaseAddress));
        mbi.BaseAddress:=pointer(startaddress);
      end;

      if PtrUint(mbi.BaseAddress)+mbi.RegionSize>=stopaddress then
        mbi.RegionSize:=stopaddress-PtrUint(mbi.BaseAddress);


      validRegion:=(mbi.State=mem_commit);
      validRegion:=validregion and (PtrUint(mbi.BaseAddress)<stopaddress);
      validregion:=validregion and ((mbi.Protect and page_guard)=0);
      validregion:=validregion and ((mbi.protect and page_noaccess)=0);
      validRegion:=validRegion and (not (not scan_mem_private and (mbi._type=mem_private)));
      validRegion:=validregion and (not (not scan_mem_image and (mbi._type=mem_image)));
      validRegion:=validregion and (not (not scan_mem_mapped and (mbi._type=mem_mapped)));
      validRegion:=validregion and (not (Skip_PAGE_NOCACHE and ((mbi._type and PAGE_NOCACHE)>0)));
      validRegion:=validregion and (not (Skip_PAGE_WRITECOMBINE and ((mbi._type and PAGE_WRITECOMBINE)>0)));


      {$ifdef windows}

      if usedbkquery and DBKLoaded then //small patch to fix an issue with the driver where it somehow sees a really big memory block
        validRegion:=validRegion and (mbi.RegionSize<qword($2ffffffff));
      {$endif}

      if validregion then
      begin
        //initial check passed, check the other protection flags to see if it should be scanned

        //fill in isWritable, isExecutable, isCopyOnWrite: boolean;
        isWritable:=((mbi.protect and PAGE_READWRITE)>0) or
                    ((mbi.protect and PAGE_WRITECOPY)>0) or //writecopy IS writable
                    ((mbi.protect and PAGE_EXECUTE_READWRITE)>0) or
                    ((mbi.protect and PAGE_EXECUTE_WRITECOPY)>0);

        isExecutable:=((mbi.protect and PAGE_EXECUTE)>0) or
                      ((mbi.protect and PAGE_EXECUTE_READ)>0) or
                      ((mbi.protect and PAGE_EXECUTE_READWRITE)>0) or
                      ((mbi.protect and PAGE_EXECUTE_WRITECOPY)>0);

        isCopyOnWrite:=((mbi.protect and PAGE_WRITECOPY)>0) or
                       ((mbi.protect and PAGE_EXECUTE_WRITECOPY)>0);

        {$ifdef darwin}
        isdirty:=(mbi.protect and PAGE_DIRTY)>0;
        {$endif}



        case scanWritable of
          scanInclude: validregion:=validregion and isWritable;
          scanExclude: validregion:=validregion and (not isWritable);
        end;

        case scanExecutable of
          scanInclude: validregion:=validregion and isExecutable;
          scanExclude: validregion:=validregion and (not isExecutable);
        end;

        case scanCopyOnWrite of
          scanInclude: validregion:=validregion and isCopyOnWrite;
          scanExclude: validregion:=validregion and (not isCopyOnWrite);
        end;

        {$ifdef darwin}
        case scanDirty of
          scanInclude: validregion:=validregion and isDirty;
          scanExclude: validregion:=validregion and (not isDirty);
        end;
        {$endif}
      end;

      if not validregion then
      begin
        //next
        currentBaseAddress:=PtrUint(mbi.BaseAddress)+mbi.RegionSize;

        continue;
      end;


      //still here, so valid

     { if (memRegionPos=0) or (memRegion[memRegionPos-1].BaseAddress+memRegion[memRegionPos-1].MemorySize<>PtrUint(mbi.baseaddress)) then
      begin}
        //new region
        memRegion[memRegionPos].BaseAddress:=PtrUint(mbi.baseaddress);  //just remember this location
        memRegion[memRegionPos].MemorySize:=mbi.RegionSize;
        memRegion[memRegionPos].startaddress:=pointer(ptrUint(totalProcessMemorySize)); //starts from 0, for unknown scans

        inc(memRegionPos);
        if (memRegionPos mod 16)=0 then //add another 16 to it
          setlength(memRegion,length(memRegion)+16);
     { end
      else
      begin
        //append
        memRegion[memRegionPos-1].MemorySize:=memRegion[memRegionPos-1].MemorySize+mbi.RegionSize;
      end; }

      inc(totalProcessMemorySize,mbi.RegionSize); //add this size to the total


    end;


    currentBaseAddress:=PtrUint(mbi.baseaddress)+mbi.RegionSize;

  end;

  {$ifndef darwin}
  VirtualQueryEx_EndCache(processhandle);
  {$endif}

   {
  OutputDebugString(format('memRegionPos=%d',[memRegionPos]));
  for i:=0 to memRegionPos-1 do
  BEGIN
    OutputDebugString(format('i: %d R=%x-%x S=%x SA=%p',[i, memRegion[i].BaseAddress, memRegion[i].BaseAddress+memRegion[i].MemorySize, memRegion[i].MemorySize, memRegion[i].startaddress]));

    for j:=0 to memregionpos-1 do
    begin
      if i<>j then
      begin
        starta:=memRegion[i].BaseAddress;
        startb:=memRegion[j].BaseAddress;
        stopa:=memregion[i].BaseAddress+memregion[i].MemorySize;
        stopb:=memregion[j].BaseAddress+memregion[j].MemorySize;

        if ((starta < stopb) and (startb < stopa)) then
        begin
          OutputDebugString('  : overlaps with '+inttostr(j));
        end;
      end;
    end;
  end;
  }


  totalAddresses:=totalProcessMemorySize;

  if memRegionPos=0 then raise exception.Create(rsNoReadableMemoryFound);


  //if soUnknown, make a buffer where it can store all the 'previous' memory
  if scanOption=soUnknownValue then
  begin
   // OutputDebugString('scanOption=soUnknownValue');

    {$ifdef lowmemoryusage}
    //create a file to store the previous memory in
    OutputDebugString(format('Creating a memory.tmp file : %dKB',[totalProcessMemorySize div 1024]));
    f:=Tfilestream.Create(OwningMemScan.ScanresultFolder+'MEMORY.TMP', fmCreate);
    f.Size:=totalProcessMemorySize;
    f.free;

    {$else}
    //extra check to make sure the previous scan was cleared
    if OwningMemScan.previousMemoryBuffer<>nil then virtualfree(OwningMemScan.previousMemoryBuffer,0,MEM_RELEASE);

    //OutputDebugString(format('Allocating %dKB for previousMemoryBuffer',[totalProcessMemorySize div 1024]));
    OwningMemScan.previousMemoryBuffer:=VirtualAlloc(nil,totalProcessMemorySize+8192, MEM_COMMIT or MEM_RESERVE or MEM_TOP_DOWN, PAGE_READWRITE); //top down to try to prevent memory fragmentation
    if OwningMemScan.previousMemoryBuffer=nil then
      raise exception.Create(Format(rsFailureAllocatingMemoryForCopyTriedAllocatingKB, [inttostr(8+totalProcessMemorySize div 1024)]));

  //  OutputDebugString(format('Allocated at %p',[OwningMemScan.previousMemoryBuffer]));
    {$endif}


  end;


  //split up into separate workloads
  if totalProcessMemorySize<threadcount*4096 then
    i:=1+(totalProcessMemorySize div 4096) //in case of mini scans don't wate too much time creating threads
  else
    i:=threadcount;

  if i<threadcount then threadcount:=i;

  //OutputDebugString(format('Splitting up the workload between %d threads',[threadcount]));


  Blocksize:=totalProcessMemorySize div threadcount;
  if (Blocksize mod 4096) > 0 then
    Blocksize:=blocksize-(blocksize mod 4096); //lastblock gets the missing bytes

  //OutputDebugString(format('Blocksize = %x',[Blocksize]));



  scannersCS.Enter; //block access by the mainthread on the scanners object, could scanner[14] has not yet been created when doing a progress request
  try
    setlength(scanners,threadcount);
    j:=0; //start at memregion 0
    leftfromprevious:=0;
    offsetincurrentregion:=0;

    for i:=0 to threadcount-1 do
    begin
    //  OutputDebugString(format('Creating scanner %d',[i]));
      scanners[i]:=tscanner.Create(true, OwningMemScan.ScanresultFolder);
      scanners[i].scannernr:=i;
      scanners[i].OwningScanController:=self;


      scanners[i]._startregion:=j;
      scanners[i].startaddress:=memRegion[j].BaseAddress+offsetincurrentregion;

      //scanners[i].maxregionsize:=0; //Original Code, no longer needed
      currentblocksize:=0;
      inc(currentblocksize,memregion[j].MemorySize-offsetincurrentregion);
      scanners[i].maxregionsize:=currentblocksize;


      if i=(threadcount-1) then
      begin
        //if it's the last thread, just give it what's left
        scanners[i].stopaddress:=stopaddress;
        scanners[i]._stopregion:=memregionpos-1;

        //define maxregionsize , go from current till end (since it'll scan everything that's left)
        while j<memregionpos do
        begin
          if scanners[i].maxregionsize<memregion[j].MemorySize then
            scanners[i].maxregionsize:=memregion[j].MemorySize;

          inc(j);
        end;
      end
      else
      begin
        //not the last thread
        inc(j);

        while (currentblocksize<blocksize) and (j<memregionpos) do
        begin
          if scanOption<>soUnknownValue then //not a unknown initial value scan, so it doesn't need overlap
          begin
            if scanners[i].maxregionsize<memregion[j].MemorySize then
              scanners[i].maxregionsize:=memregion[j].MemorySize;
          end;

          inc(currentblocksize,memregion[j].MemorySize);
          inc(j);
        end;
        dec(j);

        if scanners[i].maxregionsize=0 then //(currentblocksize<blocksize)
          scanners[i].maxregionsize:=memregion[j].MemorySize;

        scanners[i]._stopregion:=j;
        scanners[i].stopaddress:=(memregion[j].BaseAddress+memregion[j].MemorySize);

        //take off the bytes that are too many for this block
        leftfromprevious:=currentblocksize-blocksize;
        dec(scanners[i].stopaddress,leftfromprevious);

        if leftfromprevious=0 then
        begin
          inc(j); //nothing left in this region
          offsetincurrentregion:=0;
        end else offsetincurrentregion:=memregion[j].MemorySize-leftfromprevious;


      end;
    //  OutputDebugString(format('startregion = %d',[scanners[i]._startregion]));
    //  OutputDebugString(format('stopregion = %d',[scanners[i]._stopregion]));
    //  OutputDebugString(format('startaddress = %x',[scanners[i].startaddress]));
    //  OutputDebugString(format('stopaddress = %x',[scanners[i].stopaddress]));

    //  OutputDebugString(format('j = %d',[j]));
    //  OutputDebugString(format('leftfromprevious = %x',[leftfromprevious]));
    //  OutputDebugString(format('offsetincurrentregion = %x',[offsetincurrentregion]));



      if scanners[i].maxregionsize>buffersize then
        scanners[i].maxregionsize:=buffersize;

   //   OutputDebug String(format('maxregionsize = %x',[scanners[i].maxregionsize]));
              

      //now configure the scanner thread with the same info this thread got, with some extra info
      scanners[i].compareToSavedScan:=compareToSavedScan;
      scanners[i].savedscanname:=savedscanname;
      scanners[i].scanType:=scanType; //stFirstScan obviously
      scanners[i].scanoption:=scanoption;
      scanners[i].variableType:=VariableType;
      scanners[i].customType:=CustomType;
      scanners[i].roundingtype:=roundingtype;
      scanners[i].scanValue1:=scanvalue1; //usual scanvalue
      scanners[i].scanValue2:=scanValue2; //2nd value for between scan
      scanners[i].unicode:=unicode;
      scanners[i].OnlyOne:=OnlyOne or isUnique;
      scanners[i].caseSensitive:=caseSensitive;
      scanners[i].percentage:=percentage;
      scanners[i].hexadecimal:=hexadecimal;
      scanners[i].binaryStringAsDecimal:=binaryStringAsDecimal;

      scanners[i].fastscanalignsize:=fastscanalignsize;
      scanners[i].fastscanmethod:=fastscanmethod;
      scanners[i].fastscandigitcount:=fastscandigitcount;
      scanners[i].variablesize:=variablesize;
      scanners[i].floatscanWithoutExponents:=floatscanWithoutExponents;
      scanners[i].inverseScan:=inverseScan;
      scanners[i].luaformula:=luaformula;
      scanners[i].newluastate:=newluastate;



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
    scanners[i].start;

  //prepare the result files
  try

    OwningMemScan.found:=0;


    //and now we wait
    for i:=0 to threadcount-1 do
    begin
      while not (terminated or scanners[i].isdone or scanners[i].Finished) do
      begin
        scanners[i].WaitTillDone(25);
        if (OwningMemScan.progressbar<>nil) or (assigned(owningmemscan.OnGuiUpdate)) then
          synchronize(updategui);
      end;



      //If terminated then stop the scanner thread and wait for it to finish
      if terminated then
      begin
        scanners[i].Terminate;
        scanners[i].WaitFor;
      end;

      if scanners[i].haserror then
      begin
        OwningMemScan.found:=0;
        haserror:=true;
        errorstring:=scanners[i].errorstring;
        break;
      end;

      inc(OwningMemScan.found,scanners[i].totalfound);

      if IsUnique and (scanners[i].AddressFound<>0) then
      begin
        FoundSomething:=true;
        AddressFound:=scanners[i].AddressFound;
      end;
    end;


    if OnlyOne then
    begin
      if (scanners[0].found) or (scanners[0].totalfound)>0 then
      begin
        FoundSomething:=true;
        AddressFound:=scanners[0].AddressFound;
      end;

      //even fill in the data if nothing was found
      setlength(AddressesFound, length(scanners[0].mabs));
      for j:=0 to length(scanners[0].mabs)-1 do
        AddressesFound[j]:=scanners[0].mabs[j].foundaddress;
    end;


    if (OwningMemScan.progressbar<>nil) or (assigned(owningmemscan.OnGuiUpdate)) then
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
      begin
        if fastscanmethod=fsmAligned then
          OwningMemScan.found:=OwningMemScan.found div fastscanalignsize
        else
        begin
          OwningMemScan.found:=OwningMemScan.found div trunc(power(16, fastscandigitcount));
        end;
      end;

      savescannerresults:=true;
    end
    else
    begin
      savescannerresults:=true; 
    end;
    
  finally
   // OutputDebugString('Scan ended');
  end;



end;

procedure TScanController.execute;
var err: dword;
    i: integer;
    oldpos,oldmempos: qword;

    wantsize: qword;

    haserror2: boolean;
    datatype: string[6];
begin
 // OutputDebugString('TScanController.execute');
  tthread.NameThreadForDebugging('Memscan TScanController thread');

  try


    //check what it is, and call first/next/nextnext- scan
    err:=0;
    errorstring:='';


    try
      fillVariableAndFastScanAlignSize;
      if scantype=stFirstScan then firstscan;
      if scantype=stNextScan then nextscan;
     // OutputDebugString('No exception on controller');
    except
      on e: exception do
      begin
        OutputDebugString(pchar('controller exception happened during the scan:'+e.message));
        haserror:=true;
        errorstring:='controller:'+e.message;
      end;
    end;




    if OnlyOne or isUnique then savescannerresults:=false; //DO NOT INTERFERE


    {$ifdef LOWMEMORYUSAGE}
    if scanOption=soUnknownValue then
    begin
      if scanners[0].Addressfile<>nil then
        freeandnil(scanners[0].Addressfile);

      AddressFile:=TFileStream.Create(scanners[0].Addressfilename,fmCreate or fmShareDenyNone);
      datatype:='REGION';

      AddressFile.WriteBuffer(datatype,sizeof(datatype));

      for i:=0 to OwningMemScan.memregionpos do
        AddressFile.WriteBuffer(OwningMemScan.memregion[i], sizeof(OwningMemScan.memRegion[i]));

      freeandnil(addressfile);

      //make the state compatible with the original code  (double rename, but whatever)
      for i:=0 to length(scanners)-1 do
        if scanners[i].MemoryFile<>nil then
          freeandnil(scanners[i].MemoryFile);

      deletefile(scanners[0].Memoryfilename);
      renamefile(OwningMemScan.ScanresultFolder+'MEMORY.TMP', scanners[0].Memoryfilename);
    end;
    {$endif}

    if savescannerresults then //prepare saving. Set the filesize
    begin
      try
      //  OutputDebugString('ScanController: creating undo files');
        if scanners[0].Addressfile<>nil then
          freeandnil(scanners[0].Addressfile);

        if scanners[0].Memoryfile<>nil then
          freeandnil(scanners[0].Memoryfile);


        deletefile(OwningMemScan.ScanresultFolder+'ADDRESSES.UNDO');
        renamefile(OwningMemScan.ScanresultFolder+'ADDRESSES.TMP',OwningMemScan.ScanresultFolder+'ADDRESSES.UNDO');
        if not renamefile(scanners[0].Addressfilename, OwningMemScan.ScanresultFolder+'ADDRESSES.TMP') then
          RenameFileUTF8(scanners[0].Addressfilename, OwningMemScan.ScanresultFolder+'ADDRESSES.TMP');

        //memory
        deletefile(OwningMemScan.ScanresultFolder+'MEMORY.UNDO');
        renamefile(OwningMemScan.ScanresultFolder+'MEMORY.TMP',OwningMemScan.ScanresultFolder+'MEMORY.UNDO');
        renamefile(scanners[0].Memoryfilename, OwningMemScan.ScanresultFolder+'MEMORY.TMP');


        try
          AddressFile:=TFileStream.Create(OwningMemScan.ScanresultFolder+'ADDRESSES.TMP',fmOpenWrite or fmShareDenyNone);
          MemoryFile:=TFileStream.Create(OwningMemScan.ScanresultFolder+'MEMORY.TMP',fmOpenWrite or fmsharedenynone);
        except
          raise exception.create(rsErrorWhenWhileLoadingResult+' ('+OwningMemScan.ScanresultFolder+'ADDRESSES.TMP'+')');
        end;


        oldpos:=addressfile.Size;
        oldmempos:=memoryfile.size;

        wantsize:=AddressFile.size;
        for i:=1 to threadcount-1 do
          wantsize:=wantsize+scanners[i].Addressfile.Size;

        addressfile.size:=wantsize;

        if addressfile.size<>wantsize then
          raise exception.create(rsNotEnoughDiskspaceForTheAddressFile);


        wantsize:=memoryfile.size;
        for i:=1 to threadcount-1 do
          wantsize:=wantsize+scanners[i].MemoryFile.size;

        memoryfile.size:=wantsize;

        if memoryfile.size<>wantsize then
          raise exception.create(rsNotEnoughDiskspaceForTheMemoryFile);



       // outputdebugstring(format('ScanController: Have set AddressFile.size to %d',[AddressFile.size]));
       // outputdebugstring(format('ScanController: Have set MemoryFile.size to %d',[memoryFile.size]));
      except
        on e: exception do
        begin
          OutputDebugString(pchar(Format(rsDiskWriteError2, [e.message])));
          haserror:=true;
          errorstring:='controller:Cleanup:ResultsPrepare:'+e.message;
        end;
      end;
    end;


    //send message saying it's done

    if haserror then err:=1;

    isdone:=true;

    //todo: notify the caller the scan is done
  //  OutputDebugString('It actually finished');

    owningmemscan.postScanState:=psJustFinished;

    {$ifdef windows}
    SetProgressState(tbpsNone);
    {$endif}

    isdoneevent.setevent;

    haserror2:=false;

    if assigned(OwningMemScan.OnInitialScanDone) then
      Queue(OwningMemScan.InitialScanDone);

    //todo: instead of appending the results, link to the result files instead
    if scanOption<>soUnknownValue then
    begin
      try
        if savescannerresults and (addressfile<>nil) then //now actually save the scanner results
        begin
          owningmemscan.postScanState:=psOptimizingScanResults;

          //AddressFile should already have been created with the correct datatype and opened as denynone
          AddressFile.Seek(oldpos,soFromBeginning);
          Memoryfile.seek(oldmempos,soFromBeginning);

          //save the exact results, and copy it to the AddressesFirst.tmp and Memoryfirst.tmp files
          for i:=1 to length(scanners)-1 do
          begin
        //    outputdebugstring(format('ScanController: Writing results from scanner %d',[i]));
            if (scanners[i].Addressfile<>nil) and (scanners[i].MemoryFile<>nil) then
            begin
              addressfile.CopyFrom(scanners[i].Addressfile,0);
              Memoryfile.CopyFrom(scanners[i].MemoryFile,0);
            end;
          end;
        end;
      except
        on e: exception do
        begin
          OutputDebugString(pchar('Disk Write Error:'+e.message));
          haserror2:=true;
          errorstring:='controller:Cleanup:ResultsWrite:'+e.message;
        end;
      end;
    end;



    isreallydoneevent.setEvent;


    //clean up secondary scanner threads, their destructor will close and delete their files
   // outputdebugstring('ScanController: Destroying scanner threads');

    scannersCS.enter;
    owningmemscan.postScanState:=psTerminatingThreads;

    try

     // outputdebugstring('ScanController: Critical section "scannersCS" aquired');
      for i:=0 to length(scanners)-1 do
      begin
       // outputdebugstring(format('ScanController: Freeing scanner %d',[i]));
        freeandnil(scanners[i]);
      end;

      setlength(scanners,0);
    finally
      scannersCS.leave;
     // outputdebugstring('ScanController: Critical section "scannersCS" released');
    end;


    //cleanup the files
    if addressfile<>nil then addressfile.Free;
    if MemoryFile<>nil then Memoryfile.Free;


    //save the first scan results if needed
    try
      if scantype=stFirstScan then
      begin

        if not (OnlyOne or isUnique) then
        begin
          owningmemscan.postScanState:=psSavingFirstScanResults;

        //  outputdebugstring('ScanController: This was a first scan, so saving the First Scan results');
         // outputdebugstring('to:'+OwningMemScan.ScanresultFolder+'ADDRESSES.First');

          {$IFDEF LOWMEMORYUSAGE}
          copyfile(OwningMemScan.ScanresultFolder+'ADDRESSES.TMP', OwningMemScan.ScanresultFolder+'ADDRESSES.First');
          copyfile(OwningMemScan.ScanresultFolder+'MEMORY.TMP', OwningMemScan.ScanresultFolder+'MEMORY.First')
          {$else}
          OwningMemScan.SaveFirstScanThread:=TSaveFirstScanThread.create(OwningMemScan.ScanresultFolder, false,@OwningMemScan.memregion,@OwningMemScan.memregionpos, OwningMemScan.previousMemoryBuffer);
          {$ENDIF}
        end;
        //else
        //  OutputDebugString('This was an single result scan only. No need to save the first scan state');
      end;
    except
      on e: exception do
      begin
        OutputDebugString(pchar('First Scan Create:'+e.message));
        haserror2:=true;
        errorstring:='controller:Cleanup:'+rsFailedSpawningTheSaveFirstScanThread+':'+e.message;
      end;
    end;
  except
    on e: exception do
    begin
      OutputDebugString(pchar('controller exception happened:Unknown!'+e.message));
      haserror2:=true;
      errorstring:='controller:Unknown!'+e.message;
    end;
  end;

  owningmemscan.postScanState:=psShouldBeFinished;


  {$IFNDEF jni}
  if haserror2 then
    MessageBox(0, pchar(errorstring),'Scancontroller cleanup error',  MB_ICONERROR or mb_ok);
  {$ENDIF}

  //outputdebugstring('end of scancontroller reached');
  isreallydoneevent.setEvent;   //just set it again if it wasn't set





  {$IFNDEF jni}
  if assigned(OwningMemScan.OnScanDone) then
  {$endif}
  begin
   // outputdebugstring('Queue OwningMemScan.ScanDone');
    Queue(OwningMemScan.ScanDone);
  end;

  CleanupIsPointerLookupTrees;
end;

constructor TScanController.create(suspended: boolean);
begin
  {$IFDEF WINDOWS}
  SetProgressstate(tbpsNormal);
  {$ENDIF}

  isdoneevent:=TEvent.create(nil,true,false,'');
  isreallydoneevent:=TEvent.create(nil,true,false,'');
  scannersCS:=TCriticalSection.Create;
  resultsaveCS:=TCriticalsection.create;
  
  inherited create(suspended);
end;

destructor TScancontroller.destroy;
var i: integer;
begin
  terminate;
  WaitFor;

  scannersCS.Enter;
  try
    for i:=0 to length(scanners)-1 do
      scanners[i].Free;

    setlength(scanners,0);
  finally
    scannersCS.leave;
  end;

  isdoneEvent.Free;
  isReallyDoneEvent.Free;

  scannersCS.free;
  resultsaveCS.free;
  inherited destroy;
end;


//----------------memscan--------------//

function TMemscan.GetLastScanWasRegionScan:boolean;
var AddressFile: TFilestream;
    datatype: string[6];
begin
  //open the address file and determine if it's a region scan or result scan
  result:=false;
  if fileexists(ScanresultFolder+'ADDRESSES.TMP') then
  begin
    try
      AddressFile:=TFileStream.Create(ScanresultFolder+'ADDRESSES.TMP',fmOpenRead or fmSharedenynone);
      try
        Addressfile.ReadBuffer(datatype,sizeof(datatype));
      finally
        addressFile.free;
      end;

      result:=datatype='REGION';
    except
      result:=false;
    end;
  end;
end;

procedure TMemscan.TerminateScan(forceTermination: boolean);
var i: integer;
    lastwait: TWaitResult;
begin
  if scancontroller<>nil then
  begin
    if not forceTermination then
    begin
      scanController.Terminate
    end
    else
    begin
      //scancontroller.forceTerminate:=true;

      for i:=0 to length(scancontroller.scanners)-1 do
      begin
        scanController.scanners[i].Terminate;
        scanController.scanners[i].isdone:=true;
      end;


      scancontroller.Terminate;

      for i:=0 to 100 do
      begin
        lastwait:=scancontroller.isdoneEvent.WaitFor(50);

        if lastwait<>wrTimeout then
          break
        else
        begin
          if GetCurrentThreadID=MainThreadID then
            CheckSynchronize;
        end;
      end;

      if lastwait=wrTimeout then
      begin
        {$IFNDEF jni}
        {$ifdef windows}
        TerminateThread(scancontroller.Handle, $dead);
        {$endif}
        {$ifdef darwin}
        KillThread(scancontroller.handle);
        {$endif}
        messagedlg(rsMSTheScanWasForcedToTerminateSubsequentScansMayNotFunctionProperlyEtc, mtWarning, [mbok], 0);
        {$else}
        KillThread(scancontroller.handle);
        {$ENDIF}

        scanController.isDoneEvent.SetEvent;
        if assigned(fOnScanDone) then
          fOnScanDone(self);

      end;




    end;
  end
  else
    raise exception.Create(rsForSomeReasonTheScanControllerDoesNotExist);
  //and now the caller has to wait
end;

function TMemscan.hasError;
begin
  result:=false;
  if scancontroller<>nil then
    result:=scancontroller.haserror;
end;

function TMemscan.GetErrorString: string;
begin
  result:='';
  if scancontroller<>nil then
    result:=scancontroller.errorstring;
end;

function TMemscan.canUndo: boolean;
var u: TFileStream;
begin
  result:=false;
  try
    u:=tfilestream.create(fScanResultFolder+'MEMORY.UNDO', fmopenread or fmShareDenyNone);
    try
      result:=u.Size>0;
    finally
      u.free;
    end;
  except
  end;
//  LastScanType;
end;

function TMemscan.getsavedresults(r: tstrings): integer;
begin
  r.clear;
  if savedresults=nil then
  begin
    r.add('First');
    result:=1;
  end
  else
    r.AddStrings(savedresults);

  result:=r.count;
end;

function TMemscan.getSavedScanCount: integer;
begin
  if savedresults=nil then exit(0);
  result:=savedresults.Count-1;
end;

function TMemscan.deleteSavedResult(resultname: string): boolean;
var i: integer;
begin
  if resultname='' then exit(false);

  if (resultname='TMP') or (resultname='UNDO') then
    raise exception.create(rsTMPAndUNDOAreNamesThatMayNotBeUsedTryAnotherName);

  if savedresults=nil then exit(false);

  i:=savedresults.IndexOf(resultname);
  if i=-1 then exit(false);

  savedresults.Delete(i);

  DeleteFile(pchar(fScanResultFolder+'MEMORY.'+resultname));
  DeleteFile(pchar(fScanResultFolder+'ADDRESSES.'+resultname));
  result:=true;
end;

procedure TMemscan.saveresults(resultname: string);
var fname: string;
begin
  //check if it already exists or if it's a 'special' name

  fname:=uppercase(resultname);
  if (fname='TMP') or (fname='UNDO') then
    raise exception.create(rsTMPAndUNDOAreNamesThatMayNotBeUsedTryAnotherName);

  if savedresults=nil then
  begin
    savedresults:=tstringlist.create;
    savedresults.CaseSensitive:=false;
    savedresults.Duplicates:=dupError;
    savedresults.Add('First');
  end;

  if savedresults.IndexOf(resultname)<>-1 then
    raise exception.create(Format(rsAResultSetWithThisNameAlreadyExists, [resultname]));


  //everything looks ok
  waittillreallydone;



  //copy the current scanresults to memory.savedscan and addresses.savedscan
  CopyFile(pchar(fScanResultFolder+'MEMORY.TMP'), pchar(fScanResultFolder+'MEMORY.'+resultname), false,false);
  CopyFile(pchar(fScanResultFolder+'ADDRESSES.TMP'), pchar(fScanResultFolder+'ADDRESSES.'+resultname), false, false);

  savedresults.Add(resultname);

end;

procedure TMemscan.undoLastScan;
begin
  {$IFNDEF jni}
  if attachedFoundlist<>nil then
    TFoundList(Attachedfoundlist).Deinitialize;
  {$ENDIF}


  if canUndo then
  begin
    deletefile(fScanResultFolder+'MEMORY.TMP');
    deletefile(fScanResultFolder+'ADDRESSES.TMP');

    renamefile(fScanResultFolder+'MEMORY.UNDO',fScanResultFolder+'MEMORY.TMP');
    renamefile(fScanResultFolder+'ADDRESSES.UNDO',fScanResultFolder+'ADDRESSES.TMP');
  end;
end;

function TMemscan.waittilldone(timeout: DWORD=INFINITE): boolean;
begin
  result:=true;
  if scancontroller<>nil then
    result:=scancontroller.isdoneEvent.WaitFor(timeout)<>wrTimeout;
end;

function TMemscan.waittillreallydone(timeout: DWORD=INFINITE): boolean;
begin
  result:=true;
  if scancontroller<>nil then
    result:=scancontroller.isreallydoneEvent.WaitFor(timeout)<>wrTimeout;
end;


procedure TMemscan.InitialScanDone;
//called by the scancontroller when the scan has finished
begin
  if assigned(fOnInitialScanDone) then
    fOnInitialScanDone(self);
end;

procedure TMemscan.ScanDone;
//called by the scancontroller when the scan has finished
begin
  if assigned(fOnScanDone) then
    fOnScanDone(self);
end;

function TMemscan.GetOnlyOneResults(var addresses: Taddresses):boolean;
begin
  if self.scanController<>nil then
  begin
    result:=scancontroller.FoundSomething;
    addresses:=scancontroller.AddressesFound;
  end
  else
    result:=false;
end;

function TMemscan.GetOnlyOneResult(var address: ptruint):boolean;
begin
  if self.scanController<>nil then
  begin
    result:=scancontroller.FoundSomething;
    address:=scancontroller.AddressFound;
  end
  else
    result:=false;
end;

function TMemscan.canWriteResults: boolean;
var
  f: tfilestream=nil;
  s: string;
begin
  try
    try
      f:=TFileStream.Create(ScanresultFolder+'ADDRESSES.TMP.FILETEST',fmCreate);
      f.WriteAnsiString('This file can be written');
      freeandnil(f);

      f:=TFileStream.Create(ScanresultFolder+'ADDRESSES.TMP.FILETEST',fmOpenRead or fmShareDenyNone);
      s:=f.ReadAnsiString;
      if s<>'This file can be written' then raise exception.create('Invalid data read');
      freeandnil(f);

      f:=TFileStream.Create(ScanresultFolder+'ADDRESSES.TMP.FILETEST',fmOpenWrite or fmShareDenyNone);
      f.WriteAnsiString('Overwrite works properly');
      freeandnil(f);

      f:=TFileStream.Create(ScanresultFolder+'ADDRESSES.TMP.FILETEST',fmOpenRead or fmShareDenyNone);
      s:=f.ReadAnsiString;
      if s<>'Overwrite works properly' then raise exception.create('Invalid data read2');
      freeandnil(f);





      DeleteFile(ScanresultFolder+'ADDRESSES.TMP.FILETEST');

      f:=TFileStream.Create(ScanresultFolder+'ADDRESSES.TMP.FILETEST',fmCreate);
      f.WriteAnsiString('This file can be recreated');
      freeandnil(f);

      f:=TFileStream.Create(ScanresultFolder+'ADDRESSES.TMP.FILETEST',fmOpenRead or fmShareDenyNone);
      s:=f.ReadAnsiString;
      if s<>'This file can be recreated' then raise exception.create('Invalid data read 3');
      freeandnil(f);


      result:=true;
    finally
      if f<>nil then
        f.free;

      DeleteFile(ScanresultFolder+'ADDRESSES.TMP.FILETEST');
    end;

  except
    result:=false;
  end;
end;

function TMemscan.GetScanFolder: string;
begin
  result:=ScanresultFolder;
end;

function TMemscan.GetProgress(var totaladdressestoscan:qword; var currentlyscanned: qword; var resultsfound: qword):integer;
{returns a value between 1 and 1000 representing how far the scan is}
var i: integer;
begin
  result:=0;
  totaladdressestoscan:=0;
  currentlyscanned:=0;
  resultsfound:=0;

  //Take care of memory
  if self.scanController<>nil then
  begin
    totaladdressestoscan:=self.scanController.totalAddresses;
    currentlyscanned:=0;
    scanController.scannersCS.enter;
    try
      for i:=0 to length(self.scanController.scanners)-1 do
      begin
        inc(currentlyscanned,self.scanController.scanners[i].scanned);
        inc(resultsfound, self.scanController.scanners[i].totalfound+self.scanController.scanners[i].found);
      end;

    finally
      scanController.scannersCS.Leave;
    end;

    if totaladdressestoscan>0 then
      result:=trunc((currentlyscanned / totaladdressestoscan) * 1000);
  end;
end;

function TMemscan.GetFoundCount: uint64;
begin
  result:=found;
end;


function TMemscan.Getbinarysize: int64;
var i: integer;
begin
  case fVariableType of
    vtByte:      result:=8;
    vtWord:      result:=16;
    vtDWord:     result:=32;
    vtQWord:     result:=64;
    vtSingle:    result:=32;
    vtDouble:    result:=64;
    vtAll:
    begin
      result:=8;  //bytes

      {$ifdef customtypeimplemented}
      if vtcustom in ScanAllTypes then
        for i:=0 to customTypes.count-1 do
          result:=max(result, TCustomType(customtypes[i]).bytesize);
      {$ENDIF}

      result:=result*8; //get the binary size

    end;
    vtString:    if stringUnicode then result:=16*stringLength else result:=8*stringLength;
    vtBinary:    result:=binaryLength;
    vtByteArray: result:=arrayLength*8;
    {$ifdef customtypeimplemented}
    vtCustom:    result:=customtype.bytesize*8;
    {$ENDIF}
    else result:=8;
  end;
end;

procedure TMemscan.newscan;
begin
  //OutputDebugString('TMemscan.newscan');
  {$IFNDEF JNI}
  if attachedFoundlist<>nil then
    TFoundList(Attachedfoundlist).Deinitialize;
  {$ENDIF}

  if scanController<>nil then
  begin
    scanController.terminate;
    scanController.WaitFor;
    FreeAndNil(scanController);
  end;

  {$IFNDEF LOWMEMORYUSAGE}
  if SaveFirstScanThread<>nil then
  begin
    SaveFirstScanThread.Terminate;
    SaveFirstScanThread.WaitFor; //wait till it's done
    freeandnil(SaveFirstScanThread);
  end;

  if previousMemoryBuffer<>nil then virtualfree(previousMemoryBuffer,0,MEM_RELEASE);
  {$endif}

  fLastscantype:=stNewScan;
  fLastScanValue:='';

  if savedresults<>nil then
    savedresults.Clear;

  deletescanfolder;
  createscanfolder;

  fnextscanCount:=0;

  //scan params
  fScanOption:=soExactValue;
  fpercentage:=false;
  fcompareToSavedScan:=false;
  fsavedscanname:='';


end;

procedure TMemscan.NextScan(_scanOption: TScanOption; _roundingtype: TRoundingType; _scanvalue1, _scanvalue2: string; _hexadecimal,_binaryStringAsDecimal, _unicode, _casesensitive,_percentage,_compareToSavedScan: boolean; _savedscanname: string);

begin
  self.Hexadecimal:=_hexadecimal;
  self.scanOption:=_scanOption;
  self.roundingtype:=_roundingtype;
  self.Scanvalue1:=_scanvalue1;
  self.ScanValue2:=_scanvalue2;
  self.Hexadecimal:=_hexadecimal;
  self.BinaryStringAsDecimal:=_binaryStringAsDecimal;
  self.unicode:=_unicode;
  self.Casesensitive:=_casesensitive;
  self.Percentage:=_percentage;
  self.compareToSavedScan:=_compareToSavedScan;
  self.savedScanName:=_savedscanname;
  nextscan;
end;

procedure TMemscan.NextScan;
var
  frmBusy: TfrmBusy;
  r: TModalResult;
begin

  if assigned(fOnScanStart) then
    fOnScanStart(self);

  {$IFNDEF jni}
   if attachedFoundlist<>nil then
     TFoundList(Attachedfoundlist).Deinitialize;
   {$ENDIF}


   inc(fnextscanCount);


   if scanController<>nil then
   begin
     if GUIScanner and (not scancontroller.WaitTillDone(500)) then
     begin
       frmBusy:=TfrmBusy.create(nil);
       frmBusy.WaitForThread:=scancontroller;
       frmBusy.memscan:=self;
       frmBusy.Reason:=postScanState;

       if busyformIsModal then
         r:=frmBusy.Showmodal
       else
       begin
         frmBusy.FormStyle:=fsStayOnTop;
         frmBusy.Show;

         while frmbusy.visible do
         begin
           Application.ProcessMessages;
           CheckSynchronize(10);
         end;
       end;

       frmBusy.free;
     end;



     scancontroller.WaitFor; //could be it's still saving the results of the previous scan
     freeandnil(scanController);
   end;

   {$IFNDEF LOWMEMORYUSAGE}
   if SaveFirstScanThread<>nil then
   begin

     if GUIScanner and (not SaveFirstScanThread.WaitTillDone(500)) then
     begin
       postscanstate:=psSavingFirstScanResults2;
       frmBusy:=TfrmBusy.create(nil);
       frmBusy.WaitForThread:=SaveFirstScanThread;
       frmBusy.memscan:=self;
       frmBusy.Reason:=postScanState;

       if busyformIsModal then
         r:=frmBusy.Showmodal
       else
       begin
         frmBusy.FormStyle:=fsStayOnTop;
         frmBusy.Show;

         while frmbusy.visible do
         begin
           Application.ProcessMessages;
           CheckSynchronize(10);
         end;
       end;

       frmBusy.free;
     end;


     SaveFirstScanThread.WaitFor; //wait till it's done
     freeandnil(SaveFirstScanThread);
   end;
   {$ENDIF}

   scanController:=TscanController.Create(true);
   scanController.OwningMemScan:=self;
   scanController.scantype:=stNextScan;
   scanController.scanOption:=scanOption;

   scanController.compareToSavedScan:=compareToSavedScan;
   scanController.savedscanname:=savedscanname;
   scanController.variableType:=fVariableType;
   scancontroller.customType:=customtype;


   scanController.roundingtype:=roundingtype;

   scanController.fastscanalignment:=fastscanalignment;
   scanController.fastscanmethod:=fastscanmethod;
   scancontroller.fastscandigitcount:=fastscandigitcount;

   if codepage then
   begin
     scanvalue1:=UTF8ToWinCP(scanvalue1);
     scanValue2:=UTF8ToWinCP(scanvalue1);
   end;

   scanController.scanValue1:=scanvalue1; //usual scanvalue
   scanController.scanValue2:=scanValue2; //2nd value for between scan
   scanController.startaddress:=self.startaddress;
   scanController.stopaddress:=self.stopaddress;

   scancontroller.hexadecimal:=hexadecimal;
   scancontroller.binaryStringAsDecimal:=binaryStringAsDecimal;
   scancontroller.unicode:=unicode;
   scancontroller.casesensitive:=casesensitive;
   scancontroller.floatscanWithoutExponents:=floatscanWithoutExponents;
   scancontroller.inverseScan:=inverseScan;
   scancontroller.percentage:=percentage;
   scancontroller.luaformula:=fLuaFormula;
   scancontroller.newluastate:=fNewLuaState;

   fLastscantype:=stNextScan;
   fLastScanValue:=scanvalue1;

   scanController.start;

end;

procedure TMemscan.firstscan(_scanOption: TScanOption; _VariableType: TVariableType; _roundingtype: TRoundingType;
  _scanvalue1, _scanvalue2: string; _startaddress,_stopaddress: ptruint; _hexadecimal,_binaryStringAsDecimal,_unicode,_casesensitive: boolean;
  _fastscanmethod: TFastScanMethod=fsmNotAligned; _fastscanparameter: string=''; _customtype: TCustomType=nil);
begin

  Hexadecimal:=_hexadecimal;

  self.fastscanparameter:=_fastscanparameter;
  self.fastscanmethod:=_fastscanmethod;


  self.startaddress:=_startaddress;
  self.stopaddress:=_stopaddress;

  self.scanoption:=_scanOption;
  self.VariableType:=_VariableType;
  self.Roundingtype:=_roundingtype;
  self.Scanvalue1:=_scanvalue1;
  self.Scanvalue2:=_scanvalue2;
  self.Startaddress:=_startaddress;
  self.Stopaddress:=_stopaddress;
  self.BinaryStringAsDecimal:=_binaryStringAsDecimal;
  self.Unicode:=_unicode;
  self.Casesensitive:=_CaseSensitive;
  self.fastscanmethod:=_fastscanmethod;
  self.fastscanparameter:=_fastscanparameter;
  self.customtype:=_customtype;

  firstscan;
end;

procedure TMemScan.FirstScan;
begin
  if assigned(fOnScanStart) then
    fOnScanStart(self);

  if (variableType=vtCustom) and (customtype=nil) then
    raise exception.create('customType=nil');

  {$IFNDEF jni}
  if attachedFoundlist<>nil then
    TFoundList(Attachedfoundlist).Deinitialize;
  {$ENDIF}


  if scanController<>nil then freeandnil(scanController);

  {$IFNDEF LOWMEMORYUSAGE}
  if SaveFirstScanThread<>nil then
  begin
    SaveFirstScanThread.Terminate; //it should quit, saving took to long and the user already started a new one
    SaveFirstScanThread.WaitFor; //wait till it has fully terminated
    freeandnil(SaveFirstScanThread);
  end;
  {$ENDIF}


  if fastscanparameter<>'' then
    self.fastscanalignment:=strtoint('$'+fastscanparameter)
  else
    self.fastscanalignment:=1;

  self.fastscandigitcount:=length(fastscanparameter);

  //OutputDebugString('Vartype='+inttostr(integer(VariableType)));

  scanController:=TscanController.Create(true);
  scanController.OwningMemScan:=self;
  scanController.scantype:=stFirstScan;
  scanController.scanOption:=scanoption;
  scanController.variableType:=VariableType;
  scancontroller.customType:=customtype;

  scancontroller.scanWritable:=scanWritable;
  scancontroller.scanExecutable:=scanExecutable;
  scancontroller.scanCopyOnWrite:=scanCopyOnWrite;
  {$ifdef darwin}
  scancontroller.scanDirty:=scanDirty;
  {$endif}

  scanController.roundingtype:=roundingtype;

  scanController.fastscanalignment:=fastscanalignment;
  scanController.fastscanmethod:=fastscanmethod;
  scancontroller.fastscandigitcount:=fastscandigitcount;

  if codepage then
  begin
    scanvalue1:=UTF8ToWinCP(scanvalue1);
    scanValue2:=UTF8ToWinCP(scanvalue1);
  end;

  scanController.scanValue1:=scanvalue1; //usual scanvalue
  scanController.scanValue2:=scanValue2; //2nd value for between scan


  scanController.startaddress:=startaddress;
  scanController.stopaddress:=stopaddress;

  scancontroller.hexadecimal:=hexadecimal;
  scancontroller.binaryStringAsDecimal:=binaryStringAsDecimal;
  scancontroller.unicode:=unicode;
  scancontroller.casesensitive:=casesensitive;
  scancontroller.floatscanWithoutExponents:=floatscanWithoutExponents;
  scancontroller.inverseScan:=InverseScan;
  scancontroller.percentage:=false; //first scan does not have a percentage scan
  scancontroller.luaformula:=fLuaFormula;
  scancontroller.newluastate:=fNewLuaState;
  scancontroller.isUnique:=fIsUnique;
  scanController.OnlyOne:=fOnlyOne;
  scancontroller.workingsetonly:=fworkingsetonly;

  fLastscantype:=stFirstScan;
  fLastScanValue:=scanValue1;

  scanController.start;

end;

{procedure TMemscan.setScanDoneCallback(notifywindow: thandle; notifymessage: integer);
begin
  //self.notifywindow:=notifywindow;
  //self.notifymessage:=notifymessage;
end;  }

procedure TMemScan.parseProtectionflags(protectionflags: string);
var i: integer;
    currentstate: Tscanregionpreference;
begin
  //parse the protectionflags string and set scanWritable, scanExecutable and scanCopyOnWrite ;
  scanWritable:=scanDontCare;
  scanCopyOnWrite:=scanDontCare;
  scanExecutable:=scanDontCare;
  {$ifdef darwin}
  scanDirty:=scanDontCare;
  {$endif}

  protectionflags:=uppercase(protectionflags);

  currentstate:=scanDontCare;
  for i:=1 to length(protectionflags) do
  begin
    case protectionflags[i] of
      '-': currentState:=scanExclude;
      '+': currentState:=scanInclude;
      '*': currentstate:=scanDontCare;
      'W':
      begin
        scanWritable:=currentState;
        currentState:=scanDontCare;
      end;

      'C':
      begin
        scanCopyOnWrite:=currentState;
        currentState:=scanDontCare;
      end;

      'X':
      begin
        scanExecutable:=currentState;
        currentState:=scanDontCare;
      end;

      {$ifdef darwin}
      'D':
      begin
        scanDirty:=currentState;
        currentState:=scanDontCare;
      end;
      {$endif}
    end;
  end;
end;

procedure TMemScan.setVariableType(t: TVariableType);
begin
  if fLastScanType=stNewScan then //only allow change on a new scan
    fVariableType:=t;
end;

constructor TMemScan.create(progressbar: TCustomProgressbar);
begin
  self.progressbar:=progressbar;
  //setup the location of the scan results

  busyformIsModal:=true;

  CreateScanfolder;

  self.VariableType:=vtDword;
  self.ScanOption:=soExactValue;
  self.Startaddress:=0;
  self.Stopaddress:={$ifdef cpu32}DWORD($ffffffff){$else}QWORD($ffffffffffffffff){$endif};
end;

procedure TMemscan.CreateScanfolder;
var guid: TGUID;
    usedtempdir: string;

    utf8: boolean;
begin
  //OutputDebugString('CreateScanfolder');
  CreateGUID(guid);
  if (length(trim(tempdiralternative))>2) and dontusetempdir then
    usedtempdir:=trim(tempdiralternative)
  else
    usedtempdir:=GetTempDir;

  usedtempdir:=IncludeTrailingPathDelimiter(usedtempdir);

  fScanResultFolder:=usedtempdir+strCheatEngine+pathdelim;

 // OutputDebugString('fScanResultFolder='+fScanResultFolder);


  if DirectoryExistsUTF8(usedtempdir) then
    utf8:=true
  else
  if DirectoryExists(usedtempdir) then
    utf8:=false
  else
    raise exception.create(Format(rsTheTemporaryScanDirectoryDoesNotExistCheckYourScan, [usedtempdir]));


  if (utf8 and (not DirectoryExistsUTF8(fScanResultFolder))) or ((not utf8) and (not DirectoryExists(fScanResultFolder))) then
  begin
    if (utf8 and (not CreateDirUTF8(fScanResultFolder))) or ((not utf8) and (not CreateDir(fScanResultFolder))) then
    begin
      //failure in creating the dir
      {$IFDEF windows}
      MakePathAccessible(fScanResultFolder);
      {$ENDIF}
      if (utf8 and (not CreateDirUTF8(fScanResultFolder))) or ((not utf8) and (not CreateDirUTF8(fScanResultFolder))) then
        raise exception.create(rsFailureCreatingTheScanDirectory);
    end;
  end;

  {$IFDEF windows}
  MakePathAccessible(fScanResultFolder);
  {$ENDIF}



  fScanResultFolder:=fScanResultFolder+GUIDToString(guid)+pathdelim;
  CreateDir(fScanResultFolder);
end;

procedure TMemscan.DeleteScanfolder;
var usedtempdir: string;
    Info : TSearchRec;
    f: string;
    age: longint;
    currenttime: longint;

begin
 // OutputDebugString('TMemscan.DeleteScanfolder');
 { if (attachedFoundlist<>nil) then
    TFoundList(attachedFoundlist).Deinitialize; }

  if fScanResultFolder<>'' then
  begin
    try
      if DeleteFolder(fScanResultFolder)=false then outputdebugstring('Failure deleting the scanresults');



      //check if there are folders that are older than 2 days
      if (length(tempdiralternative)>2) and dontusetempdir then
      begin
        usedtempdir:=tempdiralternative;

        tempdiralternative:=trim(tempdiralternative);
        if tempdiralternative[length(tempdiralternative)]<>pathdelim then
          tempdiralternative:=tempdiralternative+pathdelim;
      end
      else
        usedtempdir:=GetTempDir;


      if FindFirst(usedtempdir+strCheatEngine+pathdelim+'{*}',  faDirectory , info)=0 then
      begin
        repeat
          if (info.Attr and faDirectory) = faDirectory then
          begin
            if length(info.Name)>5 then
            begin
              //if found, delete them if older than 2 days
              f:=usedtempdir+strCheatEngine+pathdelim+info.name;


              age:=info.time; //FileAge('"'+f+'"');

              if age>0 then
              begin
                currenttime:=DateTimeToFileDate(now);

                if (currenttime-age) > 60*60*24*2 then //if older than 2 days  then
                  deletefolder(f);
              end;
            end;
          end;

        until FindNext(info)<>0;
        FindClose(info);
      end;


    except
      outputdebugstring('Fatal error while trying to delete the scanresults');
    end;
  end;
end;

function TMemScan.DeleteFolder(dir: string) : boolean;
var
  DirInfo: TSearchRec;
  r : Integer;
begin
 // OutputDebugString('TMemScan.DeleteFolder('+dir+')');
  ZeroMemory(@DirInfo,sizeof(TSearchRec));
  result := true;

  while dir[length(dir)]=pathdelim do //cut of \
    dir:=copy(dir,1,length(dir)-1);


  {$warn 5044 off}

  r := FindFirst(dir + pathdelim+'*.*', FaAnyfile, DirInfo);
  while (r = 0) and result do
  begin
    if (DirInfo.Attr and FaVolumeId <> FaVolumeID) then
    begin
      if ((DirInfo.Attr and FaDirectory) <> FaDirectory) then
        result := DeleteFile(dir + pathdelim + DirInfo.Name);
    end;
    r := FindNext(DirInfo);
  end;
  FindClose(DirInfo);

  if Result then
    result := RemoveDir(dir);
end;

destructor TMemScan.destroy;
begin
  if scanController<>nil then
    freeandnil(scancontroller);

  {$IFNDEF LOWMEMORYUSAGE}
  if SaveFirstScanThread<>nil then
  begin
    //OutputDebugString('SaveFirstScanThread exists. Cleaning it up');
    SaveFirstScanThread.Terminate;

    //if not SaveFirstScanThread.Finished then
   //   OutputDebugString('The thread was not yet finished. Waiting for it');

    SaveFirstScanThread.WaitFor;

   // OutputDebugString('Done waiting');

    SaveFirstScanThread.Free;
  end;
 // else
  //  OutputDebugString('SaveFirstScanThread is nil');

  if previousMemoryBuffer<>nil then virtualfree(previousMemoryBuffer,0,MEM_RELEASE);
  {$endif}




  DeleteScanfolder;

  inherited Destroy;
end;


end.
