unit CEFuncProc;
//This version of CEFuncProc has been COPIED to the server dir
//Cheat Engine regular WONT look at this

interface

uses Windows,StdCtrls,Classes,SysUtils,dialogs,tlhelp32,forms,messages,
Graphics,
ComCtrls,
reinit,
assemblerunit,
imagehlp,
registry,

{$ifdef netclient}
netapis,
{$else}
NewKernelHandler,
{$ifndef standalonetrainer}
{$ifndef netserver}
hypermode,
firstscanhandler,
{$endif}
{$endif}
{$endif}
math,syncobjs, shellapi, ProcessHandlerUnit;

//memscan
type TScanOption=(soUnknownValue,soExactValue,soValueBetween,soBiggerThan,soSmallerThan, soIncreasedValue, soIncreasedValueBy, soDecreasedValue, soDecreasedValueBy, soChanged, soUnchanged, soSameAsFirst, soCustom);
type TScanType=(stNewScan, stFirstScan, stNextScan);
type TRoundingType=(rtRounded,rtExtremerounded,rtTruncated);
type TVariableType=(vtByte, vtWord, vtDword, vtQword, vtSingle, vtDouble, vtString, vtUnicodeString, vtByteArray, vtBinary, vtAll, vtCustom, vtPointer);
type TCustomScanType=(cstNone, cstAutoAssembler, cstCPP, cstDLLFunction);


type PUINT64 = ^uint64;
Type TBytes = array of integer; //An array that represents a row of byte. Ints are used to be able to represent wildcards (-1)
type tfloatscan=(rounded,extremerounded,truncated);
Type TMemoryRegion = record
  BaseAddress: Dword;
  MemorySize: Dword;
  IsChild: boolean;  //means there is a region before it
  startaddress: pointer; //pointer to a spot in the whole memory copy, it means the start of this region
  end;
type TMemoryRegions = array of TMemoryRegion;
type PMemoryRegions = ^TMemoryRegions;

type TBitAddress = record
  address: dword;
  bit: dword;
end;

type TBitAddressArray=array [0..0] of TBitAddress;
type PBitAddressArray=^TBitAddressArray;

type TProcessListInfo=record
  processID: dword;
  processIcon: HICON;
end;
PProcessListInfo=^TProcessListInfo;

type TFreememoryThread = class(tthread)
  public
    Bitscan: array of byte;
    tempbits: array of byte;
    FoundAddress: array of dword;
    FoundValue1:Array of Byte;
    FoundValue2:Array of word;
    FoundValue3:Array of Dword;
    FoundValue7:Array of Int64;
    FoundValue8:array of byte;
    FoundValue4:Array of Single;
    FoundValue5:Array of Double;
    foundValue6:Array of int64;  //byte ?????
    FoundValue1switch:Array of Byte;
    FoundValue2switch:Array of word;
    FoundValue3switch:Array of Dword;
    FoundValue7switch:Array of Int64;
    FoundValue8switch:array of byte;
    FoundValue4switch:Array of Single;
    FoundValue5switch:Array of Double;
    foundValue6switch:Array of int64;

    foundaddressB: array of TBitAddress;
    previousmemory: array of byte;

    SearchAddress: array of dword;
    SearchAddressB: array of TBitAddress;

    previousmemory1: array of Byte;
    previousmemory2: array of word;
    previousmemory3: array of dword;
    previousmemory4: array of Single;
    previousmemory5: array of Double;
    previousmemory6: array of int64; //Byte;
    PreviousMemory7: Array of Int64;
    PreviousMemory8: array of byte;
    previousmemory1switch: array of Byte;
    previousmemory2switch: array of word;
    previousmemory3switch: array of dword;
    previousmemory4switch: array of Single;
    previousmemory5switch: array of Double;
    previousmemory6switch: array of int64; //Byte;
    PreviousMemory7switch: Array of Int64;
    PreviousMemory8switch: array of byte;
    
    Memory: ^Byte;
    memory2: ^byte;
    bytes: array of integer;  //-1=wildcard
    bytearray: array of byte;

    procedure execute; override;
end;

function ConvertHexStrToRealStr(const s: string): string;
function HexStrToInt(const S: string): Integer;
function HexStrToInt64(const S: string): Int64;

function readAndParseAddress(address: dword; variableType: TVariableType): string;
function isjumporcall(address: dword; var addresstojumpto: dword): boolean;

procedure quicksortmemoryregions(lo,hi: integer);

procedure rewritecode(processhandle: thandle; address:dword; buffer: pointer; size:dword);
procedure rewritedata(processhandle: thandle; address:dword; buffer: pointer; size:dword);

procedure GetProcessList(ProcessList: TListBox); overload;
procedure GetProcessList(ProcessList: TStrings); overload;
procedure GetWindowList(ProcessList: TListBox{; var ArrIcons: TBytes});
function AvailMem:dword;
function isreadable(address:dword):boolean;


procedure RemoveAddress(address: Dword;bit: Byte; vartype: Integer);

function GetCEdir:string;
procedure Open_Process;
Procedure Shutdown;
function KeyToStr(key:word):string;

function undolastscan(valtype: integer;hexadecimal:boolean): integer;

procedure ConvertStringToBytes(scanvalue:string; hex:boolean;var bytes: TBytes);
function getbit(bitnr: integer; bt: dword):integer;
procedure setbit(bitnr: integer; var bt: Byte;state:integer); overload;
procedure setbit(bitnr: integer; var bt: dword;state:integer); overload;

function eflags_setCF(flagvalue: dword; value: integer): DWORD;
function eflags_setPF(flagvalue: dword; value: integer): DWORD;
function eflags_setAF(flagvalue: dword; value: integer): DWORD;
function eflags_setZF(flagvalue: dword; value: integer): DWORD;
function eflags_setSF(flagvalue: dword; value: integer): DWORD;
function eflags_setTF(flagvalue: dword; value: integer): DWORD;
function eflags_setIF(flagvalue: dword; value: integer): DWORD;
function eflags_setDF(flagvalue: dword; value: integer): DWORD;
function eflags_setOF(flagvalue: dword; value: integer): DWORD;
function eflags_setIOPL(flagvalue: dword; value: integer): DWORD;
function eflags_setNT(flagvalue: dword; value: integer): DWORD;
function eflags_setRF(flagvalue: dword; value: integer): DWORD;
function eflags_setVM(flagvalue: dword; value: integer): DWORD;
function eflags_setAC(flagvalue: dword; value: integer): DWORD;
function eflags_setVIF(flagvalue: dword; value: integer): DWORD;
function eflags_setVIP(flagvalue: dword; value: integer): DWORD;
function eflags_setID(flagvalue: dword; value: integer): DWORD;




function ByteStringToText(s: string;hex: boolean):string;
function ByteStringToDouble(s: string;hex: boolean):double;
function ByteStringToSingle(s: string;hex: boolean):single;
function ByteStringToInt(s: string;hex: boolean):int64;
function VarToBytes(v: pointer; size: integer): string;
function RawToString(const buf: array of byte; vartype: integer;showashex: boolean; bufsize: integer):string;
function IntToBin(i: int64):string;
function BinToInt(s: string): int64;

procedure decimal(var key: char);
procedure hexadecimal(var key: char);

function scanbits(var found: dword;number:dword;var bytep: pbyte;nrofbits,i,actualread: integer): boolean;
function GetSystemType: Integer;
function Is64bitOS: boolean;

procedure ToggleOtherWindows;

procedure EnableStealth;
procedure DisableStealth;
Procedure InjectDll(dllname: string; functiontocall: string='');
Function GetRelativeFilePath(filename: string):string;

function GetCPUCount: integer;
procedure SaveFormPosition(form: Tform; extra: array of integer);
function LoadFormPosition(form: Tform; var x: array of integer):boolean; 

function allocationtypetostring(alloctype: dword): string;
function allocationprotecttostring(protect: dword): string;
function freetypetostring(freetype: dword):string;

{$ifndef standalonetrainer}
Procedure CreateCodeCave(address:dword; sourceaddress:dword; sizeofcave: integer);
{$endif}

procedure errorbeep;



{$ifndef net}
procedure SetLanguage;

{$endif}

{$ifndef standalonetrainer}
procedure DetachIfPossible;
{$endif}

procedure getexecutablememoryregionsfromregion(start: dword; stop:dword; var memoryregions: TMemoryRegions);

const
  Exact_value = 0;
  Increased_value = 1;
  Increased_value_by = 2;
  Decreased_value = 3;
  Decreased_value_by = 4;
  Changed_value = 5;
  Unchanged_value = 6;
  Advanced_Scan = 7;
  String_Scan = 8;
  SmallerThan = 9;
  BiggerThan = 10;
  Userdefined = 11; //not used
  ValueBetween = 12;
  SameAsFirst = 13;

  splitvalue=400000;
  number=600;      //is my using the new value on my system arround 580000

  WM_HOTKEY2=WM_USER+$800;

type
  MemoryRecordcet3 = record
        Description : string[50];
        Address : dword;
        VarType : byte;
        Bit     : Byte;
        Frozen : boolean;
        FrozenValue : Int64;
        Group:  Byte;
  end;

type TCEPointer=record
  Address: Dword;  //only used when last pointer in list
  Interpretableaddress: string; //same as address
  offset: integer;
end;

type TCEAlloc=record
  address: dword;
  varname: string;
  size: dword;
end;
type PCEAlloc=^TCEAlloc;
type TCEAllocArray=array of TCEAlloc;

type
  MemoryRecord = record
        Description : string;
        Address : dword;
        interpretableaddress: string;
        VarType : byte;
        unicode : boolean;
        IsPointer: Boolean;
        pointers: array of TCEPointer;
        Bit     : Byte;
        bitlength: integer;
        Frozen : boolean;
        FrozenValue : Int64;
        OldValue: string;   //not saved
        Frozendirection: integer; //0=always freeze,1=only freeze when going up,2=only freeze when going down
        Group:  Byte;
        ShowAsHex: boolean;
        autoassemblescript: string;
        allocs: TCEAllocArray;
  end;

type
  MemoryRecordOld = record
        Description : string[50];
        Address : dword;
        VarType : byte;
        Frozen : boolean;
        FrozenValue : Dword;
  end;

type TDwordArray=array[0..0] of dword;
type PDwordArray=^TDwordArray;

type TSingleArray=array[0..0] of single;
type PSingleArray=^TSingleArray;

type TdoubleArray=array[0..0] of double;
type PdoubleArray=^TdoubleArray;

type Tint64Array=array[0..0] of int64;
type Pint64Array=^Tint64Array;

type Tuint64Array=array[0..100] of uint64;
type Puint64Array=^Tuint64Array;



type TScanSettings = record
  UseHyperscan: boolean;
  scanning: boolean;
  CEProcessID: dword;
  CEMainThreadID: Dword;
  applicantionhandle: thandle;
  mainformHandle: THandle;
  formscanningHandle: THandle;
  hyperscanwindow: Thandle;
  StartAddress: Dword;
  StopAddress: Dword;
  Scantype: Integer;
  ValueType: Integer;
  roundingtype: tfloatscan;
  scan:byte;
  readonly: boolean;
  FastScan: boolean;
  Hexadecimal: boolean;
  unicode: boolean;
  percentage: boolean;
  LowMemoryUsage: boolean;
  Skip_PAGE_NOCACHE:boolean;
  scan_mem_private:boolean;
  scan_mem_image:boolean;
  scan_mem_mapped: boolean;
  scanvalue: string[255];
  scanvalue2: string[255];
  CheatEngineDir: string[255];
  buffersize:dword;
  priority:integer;
  nrofbits:integer;
  bitstring: string[255];
  bitoffsetchange: integer;
  asktocontinue: boolean;
  HookDirect3d: boolean;
  HookOpenGL:   boolean;
  PacketEditor: boolean;
  Stealthed: boolean;
  hooknewprocesses: boolean;
end;

type tspeedhackspeed=record
  speed: single;
  sleeptime: dword;
end;

type TKeyCombo=array [0..4] of word;
type TKeys=record
  configured: boolean;
  CEDir: string[255];
  cewindow: thandle;

  callibrationmode: boolean;  //false=no textureselect hud
  callibrationkey: TKeycombo;

  setcallibration: boolean;
  mousecallibrationhorizontal1point: single;
  mousecallibrationvertical1point: single;

  mousecallibrationhorizontal2point: single;
  mousecallibrationvertical2point: single;

  mousecallibrationhorizontal5point: single;
  mousecallibrationvertical5point: single;

  mousecallibrationhorizontal10point: single;
  mousecallibrationvertical10point: single;

  mousecallibrationhorizontal20point: single;
  mousecallibrationvertical20point: single;

  mousecallibrationhorizontal40point: single;
  mousecallibrationvertical40point: single;

  loadaimsettingsfile: tkeycombo;
  saveaimsettingsfile: tkeycombo;
  aimsettings1: string[255];
  Aimsettings2: string[255];
  Aimsettings3: string[255];

  setaimsetting1: tkeycombo;
  setaimsetting2: tkeycombo;
  setaimsetting3: tkeycombo;

  nexttexture: tkeycombo;
  previoustexture: tkeycombo;
  locktexture: tkeycombo;

  IncreaseX: tkeycombo;
  DecreaseX: TKeyCombo;
  Increasey: tkeycombo;
  Decreasey: TKeyCombo;
  Increasez: tkeycombo;
  Decreasez: TKeyCombo;

  HoldAutoaimtoggle: boolean;
  autoshoot: boolean;
  autoaimtoggle: tKeycombo;
  increaselag: tkeycombo;
  decreaselag: tkeycombo;

  zoomin,zoomout: TKeyCombo;
  nozoom: tKeyCombo;
  zoom1: tKeyCombo;
  zoomlevel1: single;
  zoom2: tkeycombo;
  zoomlevel2: single;
  zoom3: tkeycombo;
  zoomlevel3: single;
  zoom4: tkeycombo;
  zoomlevel4: single;
  zoom5: tkeycombo;
  zoomlevel5: single;

  zoomdelta: single;
  lagdelta: integer;

  setlag: boolean;
  lagtoset: dword;
  usefpslag: boolean;

  rotateleft: tKeycombo;
  rotateright: tkeycombo;
  rotateup: tkeycombo;
  rotatedown: tkeycombo;
  moveleft: tkeycombo;
  moveright: tkeycombo;
  moveup: tkeycombo;
  movedown: tkeycombo;
  moveforward: tkeycombo;
  movebackwards: tkeycombo;

  movespeed: single;
  rotatespeed: single;

  setcameraback: tkeycombo;

  zbuffer: tkeycombo;
  fog: tkeycombo;
  lighting: tkeycombo;
  wireframe: tkeycombo;

  ShowKeylist: tkeycombo;

  SaveAlltextures: TKeycombo;

  selectedlagrecord: string[50];
  lagmemorytype: byte;
  getlagfrommemory: boolean;
  nrofoffsets: dword;
  lagaddress: dword;
  offset1: dword;
  offset2: dword;
  offset3: dword;
  offset4: dword;
  offset5: dword;
  offset6: dword;
  offset7: dword;
  offset8: dword;
  offset9: dword;
  offset10: dword;
  offset11: dword;
  offset12: dword;
  offset13: dword;
  offset14: dword;
  offset15: dword;


  pollinginterval: integer;
end;
type PKeys= ^TKeys;

type TKeys2=record
  configured: boolean;
  CEDir: string[255];
  cewindow: thandle;

  textures: tkeycombo;
  lighting: tkeycombo;
  depthtest: tkeycombo;
  fog: tkeycombo;


  zoomin,zoomout: TKeyCombo;
  nozoom: tKeyCombo;
  zoom1: tKeyCombo;
  zoomlevel1: single;
  zoom2: tkeycombo;
  zoomlevel2: single;
  zoom3: tkeycombo;
  zoomlevel3: single;
  zoom4: tkeycombo;
  zoomlevel4: single;
  zoom5: tkeycombo;
  zoomlevel5: single;

  zoomdelta: single;


  pollinginterval: integer;
end;
type PKeys2= ^TKeys2;



function ConvertKeyComboToString(x: tkeycombo):string;

{
ProcessID and ProcessHandle as functions untill all code has been converted to
make use of ProcessHandlerUnit
}
function ProcessID: dword;
function ProcessHandle: THandle;

//Global vars:
var
  old8087CW: word;  //you never know...
  ProcessSelected: Boolean;
  //ProcessID: Dword; //deperecated
  //ProcessHandle: Thandle;

  Skip_PAGE_NOCACHE: boolean;
  Scan_MEM_PRIVATE: boolean;
  Scan_MEM_IMAGE: boolean;
  Scan_MEM_MAPPED: boolean;

  CheatEngineDir: String;
  WindowsDir: string;
  GetProcessIcons: Boolean;
  ProcessesWithIconsOnly: boolean;

//scanhelpers
  nrofbits: integer;
  Bitscan: array of byte;
  tempbits: array of byte;

  bitoffsetchange: integer;

  FoundValue1:Array of Byte;
  FoundValue1switch:Array of Byte;

  FoundValue2:Array of word;
  FoundValue2switch:Array of word;

  FoundValue3:Array of Dword;
  FoundValue3switch:Array of Dword;

  FoundValue4:Array of Single;
  FoundValue4switch:Array of Single;

  FoundValue5:Array of Double;
  FoundValue5switch:Array of Double;

  foundValue6:Array of int64;
  foundValue6switch:Array of int64;

  FoundValue7:Array of Int64;
  FoundValue7switch:Array of Int64;

  FoundValue8:array of byte;
  FoundValue8switch:array of byte;

  FoundAddress: array of dword;
  FoundAddressSwitch: array of dword;

  foundaddressB: array of TBitAddress;
  foundaddressBswitch: array of TBitAddress;  


  tempbytearray: array of byte;
  tempwordarray: array of word;
  tempdwordarray: array of dword;
  tempsinglearray: array of single;
  tempdoublearray: array of double;
  tempint64array: array of int64;


//--------
  previousmemory: array of byte;

  SearchAddress: array of dword;
  searchaddressswitch: array of dword;

  SearchAddressB: array of TBitAddress;

  previousmemory1,previousmemory1switch: array of Byte;
  previousmemory2,previousmemory2switch: array of word;
  previousmemory3,previousmemory3switch: array of dword;
  previousmemory4,previousmemory4switch: array of Single;
  previousmemory5,previousmemory5switch: array of Double;
  previousmemory6,previousmemory6switch: array of int64; //Byte;
  PreviousMemory7,previousmemory7switch: Array of Int64;
  PreviousMemory8,previousmemory8switch: array of byte;

//---------
  helpstr,helpstr2: string;
  bytes: array of integer;  //-1=wildcard
  bytearray: array of byte;



  MemoryRegion: array of TMemoryRegion;
  MemoryRegions: Integer;
  
//  Memory: Array of Byte;
  Memory: ^Byte;
  memory2: ^byte;


  advanced: boolean;
  //global files, so when an exception happens I can close them
  addressfile, memoryfile: File;
  newAddressfile,newmemoryfile: File;

  buffersize: dword;
  overridedebug: boolean;

  totalbytes: dword;
  currentbyte: dword;


  //hide/show windows
  windowlist: array of thandle;
  lastforeground,lastactive: thandle;
  donthidelist: array of string;
  onlyfront: boolean;
  allwindowsareback:boolean;

  HyperscanFileMapping: THandle;
  HyperscanView: ^TScanSettings;
  
  hookedin:boolean;
  keys: PKeys;
  keys2: PKeys2;
  keysfilemapping: THandle;

  //stealth globals
  le: dword;
  ownprocesshandle: THandle;
  stealthhook: thandle;

  //windows version data
  iswin2kplus: boolean;
  scanpriority: TThreadPriority; 

  {$ifndef standalonetrainer}
  {$ifndef net}
  hypermode: thypermode;
  useAPCtoInjectDLL: boolean;
  {$endif}
  {$endif}


  processhandler: TProcessHandler;

implementation

{$ifdef net}
uses disassembler,debugger;
{$endif}

{$ifndef net}

{$ifndef standalonetrainer}
uses disassembler,debugger,symbolhandler,frmProcessWatcherUnit,kerneldebugger;
{$else}
uses symbolhandler;
{$endif}

{$endif}

function ProcessID: dword;
begin
  result:=ProcessHandler.Processid;
end;

function ProcessHandle: THandle;
begin
  result:=ProcessHandler.ProcessHandle;
end;


procedure TFreememorythread.execute;
begin
  setlength(bitscan,0);
  setlength(tempbits,0);
  setlength(FoundAddress,0);

  setlength(FoundValue1,0);
  setlength(FoundValue2,0);
  setlength(FoundValue3,0);
  setlength(FoundValue4,0);
  setlength(FoundValue5,0);
  setlength(foundValue6,0);
  setlength(FoundValue7,0);
  setlength(FoundValue8,0);
  setlength(FoundValue1switch,0);
  setlength(FoundValue2switch,0);
  setlength(FoundValue3switch,0);
  setlength(FoundValue4switch,0);
  setlength(FoundValue5switch,0);
  setlength(foundValue6switch,0);
  setlength(FoundValue7switch,0);
  setlength(FoundValue8switch,0);


    //floating point notations


  setlength(foundaddressB,0);

  setlength(previousmemory,0);

  setlength(SearchAddress,0);
  setlength(SearchAddressB,0);

  setlength(previousmemory1,0);
  setlength(previousmemory2,0);
  setlength(previousmemory3,0);
  setlength(previousmemory4,0);
  setlength(previousmemory5,0);
  setlength(previousmemory6,0);
  setlength(PreviousMemory7,0);
  setlength(PreviousMemory8,0);

  setlength(previousmemory1switch,0);
  setlength(previousmemory2switch,0);
  setlength(previousmemory3switch,0);
  setlength(previousmemory4switch,0);
  setlength(previousmemory5switch,0);
  setlength(previousmemory6switch,0);
  setlength(PreviousMemory7switch,0);
  setlength(PreviousMemory8switch,0);

  if not advanced then     //if the lastscan was a advanced, better not whipe the memory (yet!)
  begin
    if memory<>nil then freemem(memory);
    memory:=nil;

    if memory2<>nil then freemem(memory2);
    memory2:=nil;
  end;

  setlength(bytes,0);
  setlength(bytearray,0);
end;

type TSaveDataThread=class(tthread)
  public
    buffer1: pointer;
    buffer2: pointer;
    buffersize1: dword;
    buffersize2: dword;
    file1: ^file;
    file2: ^file;
    datawritten: tevent;
    dataavailable:tevent;

    procedure execute; override;
    constructor create(suspended:boolean);
    destructor destroy; override;
end;

procedure TSaveDataThread.execute;
var ignore: dword;
begin
  dataavailable.WaitFor(infinite);
  while not terminated do
  begin
    blockwrite(file1^,buffer1^,buffersize1,ignore);
    blockwrite(file2^,buffer2^,buffersize2,ignore);

    datawritten.SetEvent; //tell the others that you're ready to write again
    dataavailable.WaitFor(infinite); //wait for the 'ready to write' event
  end;
end;

destructor TSaveDataThread.destroy;
begin
  datawritten.Free;
  dataavailable.free;
  inherited destroy;
end;

constructor TSaveDataThread.create(suspended:boolean);
begin
  datawritten:=tevent.Create(nil,false,true,'');
  dataavailable:=tevent.create(nil,false,false,'');

  inherited create(suspended);
end;

var FlushThread:TSaveDataThread;

procedure flushbuffer(var file1, file2:file; buffer1:pointer; buffer1size:integer; buffer2:pointer; buffer2size:integer);
begin
  flushthread.datawritten.ResetEvent;
  
  flushthread.file1:=@file1;
  flushthread.file2:=@file2;
  flushthread.buffer1:=buffer1;
  flushthread.buffer2:=buffer2;
  flushthread.buffersize1:=buffer1size;
  flushthread.buffersize2:=buffer2size;
  flushthread.dataavailable.SetEvent;
end;

procedure finishflushing;
begin
  flushthread.datawritten.WaitFor(infinite);
  flushthread.datawritten.SetEvent;
end;

//------------------------------------

type TPrefetchDataThread=class(tthread)
  public
    buffer1: pointer;
    buffer2: pointer;
    buffersize1: dword;
    buffersize2: dword;
    file1: ^file;
    file2: ^file;
    dataread: tevent;
    startreading:tevent;

    actualread: dword;

    procedure execute; override;
    constructor create(suspended:boolean);
    destructor destroy; override;
end;

procedure TPrefetchDataThread.execute;
var ignore,tmp: dword;
begin
  startreading.WaitFor(infinite);

  while not terminated do
  begin
    self.actualread:=0;
    blockread(file1^,buffer1^,buffersize1,tmp);
    blockread(file2^,buffer2^,buffersize2,ignore);

    self.actualread:=tmp;

    dataread.SetEvent; //tell the others that you're ready to read again
    startreading.WaitFor(infinite);
  end;
end;

destructor TPrefetchDataThread.destroy;
begin
  dataread.Free;
  startreading.free;
  inherited destroy;
end;

constructor TPrefetchDataThread.create(suspended:boolean);
begin
  dataread:=tevent.Create(nil,false,true,'');
  startreading:=tevent.create(nil,false,false,'');

  inherited create(suspended);
end;

var PrefetchThread:TPrefetchDataThread;

procedure prefetchbuffer(var file1, file2:file; buffer1:pointer; buffer1size:integer; buffer2:pointer; buffer2size:integer);
begin
  PrefetchThread.dataread.ResetEvent;

  PrefetchThread.file1:=@file1;
  PrefetchThread.file2:=@file2;
  PrefetchThread.buffer1:=buffer1;
  PrefetchThread.buffer2:=buffer2;
  PrefetchThread.buffersize1:=buffer1size;
  PrefetchThread.buffersize2:=buffer2size;
  PrefetchThread.startreading.SetEvent;
end;

function finishprefetching:dword;
begin
  PrefetchThread.dataread.WaitFor(infinite);//wait for the datread event to be set
  result:=prefetchthread.actualread;
end;

procedure errorbeep;
begin
  beep;
  sleep(100);
  beep;
  sleep(100);
  beep;
  sleep(100);
end;

function isreadable(address:dword):boolean;
var mbi: _MEMORY_BASIC_INFORMATION;
begin
  VirtualQueryEx(processhandle,pointer(address),mbi,sizeof(mbi));
  result:=mbi.State=mem_commit;
end;

function RawToString(const buf: array of byte; vartype: integer;showashex: boolean; bufsize: integer):string;
var x: pchar;
    i: integer;
begin
  //buffsize has to match the type else error
  if bufsize=0 then
  begin
    result:='???';
    exit;
  end;

  try
  case vartype of
    0: if bufsize<>1 then result:='???' else if showashex then result:=inttohex(buf[0],2) else result:=inttostr(buf[0]);
    1: if bufsize<>2 then result:='???' else if showashex then result:=inttohex(pshortint(@buf[0])^,2) else result:=inttostr(pshortint(@buf[0])^);
    2: if bufsize<>4 then result:='???' else if showashex then result:=inttohex(pint(@buf[0])^,4) else result:=inttostr(pint(@buf[0])^);
    3: if bufsize<>4 then result:='???' else result:=floattostr(psingle(@buf[0])^);
    4: if bufsize<>8 then result:='???' else result:=floattostr(pdouble(@buf[0])^);
    6: if bufsize<>4 then result:='???' else if showashex then result:=inttohex(pint64(@buf[0])^,8) else result:=inttostr(pint64(@buf[0])^);
    7:
    begin
      getmem(x,bufsize+1);
      x[bufsize]:=#0;
      result:=x;
      freemem(x);
    end;

    8: //array of bytes
    begin
      result:='';
      for i:=0 to bufsize-1 do
        result:=result+'-'+inttohex(buf[bufsize],2);
    end;

    else result:='not supported in this version';
  end;
  except
    result:='Not convertable';
  end;
end;

function ConvertKeyComboToString(x: tkeycombo):string;
var i: integer;
    newstr: string;
begin
  result:='';
  for i:=0 to 4 do
    if x[i]=0 then
      break
    else
    begin
      newstr:='';
      case x[i] of
        vk_lbutton: newstr:='Left MB';
        vk_mbutton: newstr:='Middle MB';
        vk_rbutton: newstr:='Right MB';
        VK_BACK	: newstr:='Backspace';
        VK_SHIFT: newstr:='Shift';
        VK_CONTROL: newstr:='Ctrl';
        VK_MENU: newstr:='Alt';
        VK_TAB	: newstr:='Tab';
        VK_CLEAR	: newstr:='Clear';
        VK_RETURN	: newstr:='Enter';
        VK_PAUSE	: newstr:='Pause';
        VK_CAPITAL	: newstr:='Caps Lock';
        VK_ESCAPE	: newstr:='Esc';
        VK_SPACE	: newstr:='Space bar';
        VK_PRIOR	: newstr:='Page Up';
        VK_NEXT	: newstr:='Page Down';
        VK_END	: newstr:='End';
        VK_HOME	: newstr:='Home';
        VK_LEFT	: newstr:='Left Arrow';
        VK_UP	: newstr:='Up Arrow';
        VK_RIGHT	: newstr:='Right Arrow';
        VK_DOWN	: newstr:='Down Arrow';
        VK_SELECT	: newstr:='Select';
        VK_PRINT	: newstr:='Print';
        VK_EXECUTE	: newstr:='Execute';
        VK_SNAPSHOT	: newstr:='Print Screen';
        VK_INSERT	: newstr:='Insert';
        VK_DELETE	: newstr:='Delete';
        VK_HELP	: newstr:='Help';
        VK_LWIN	: newstr:='Left Windows key';
        VK_RWIN	: newstr:='Right Windows key';
        VK_APPS	: newstr:='Applications key';
        VK_NUMPAD0	: newstr:='numeric 0';
        VK_NUMPAD1	: newstr:='numeric 1';
        VK_NUMPAD2	: newstr:='numeric 2';
        VK_NUMPAD3	: newstr:='numeric 3';
        VK_NUMPAD4	: newstr:='numeric 4';
        VK_NUMPAD5	: newstr:='numeric 5';
        VK_NUMPAD6	: newstr:='numeric 6';
        VK_NUMPAD7	: newstr:='numeric 7';
        VK_NUMPAD8	: newstr:='numeric 8';
        VK_NUMPAD9	: newstr:='numeric 9';
        VK_MULTIPLY	: newstr:='numeric *';
        VK_ADD	: newstr:='numeric +';
        VK_SEPARATOR : newstr:='numeric Separator';
        VK_SUBTRACT	: newstr:='numeric -';
        VK_DECIMAL	: newstr:='numeric .';
        VK_DIVIDE	: newstr:='numeric /';
        VK_F1	: newstr:='F1';
        VK_F2	: newstr:='F2';
        VK_F3	: newstr:='F3';
        VK_F4	: newstr:='F4';
        VK_F5	: newstr:='F5';
        VK_F6	: newstr:='F6';
        VK_F7	: newstr:='F7';
        VK_F8	: newstr:='F8';
        VK_F9	: newstr:='F9';
        VK_F10	: newstr:='F10';
        VK_F11	: newstr:='F11';
        VK_F12	: newstr:='F12';
        VK_F13	: newstr:='F13';
        VK_F14	: newstr:='F14';
        VK_F15	: newstr:='F15';
        VK_F16	: newstr:='F16';
        VK_F17	: newstr:='F17';
        VK_F18	: newstr:='F18';
        VK_F19	: newstr:='F19';
        VK_F20	: newstr:='F20';
        VK_F21	: newstr:='F21';
        VK_F22	: newstr:='F22';
        VK_F23	: newstr:='F23';
        VK_F24	: newstr:='F24';
        VK_NUMLOCK	: newstr:='Num Lock';
        VK_SCROLL	: newstr:='Scroll Lock';
        48..57      : newstr:=chr(x[i]);
        65..90      : newstr:=chr(x[i]);
        else  newstr:='#'+inttostr(x[i]);
      end;

      result:=result+newstr+'+';
    end;

  result:=copy(result,1,length(result)-1);
end;

procedure getexecutablememoryregionsfromregion(start: dword; stop:dword; var memoryregions: tmemoryregions);
var address: dword;
    mbi: memory_basic_information;
begin
  setlength(memoryregions,0);
  address:=start;
  while (address<stop) and (VirtualQueryEx(processhandle,pointer(address),mbi,sizeof(mbi))<>0) and ((address+mbi.RegionSize)>address) do
  begin
    if ((mbi.AllocationProtect and PAGE_EXECUTE)=PAGE_EXECUTE) or
       ((mbi.AllocationProtect and PAGE_EXECUTE_READ)=PAGE_EXECUTE_READ) or
       ((mbi.AllocationProtect and PAGE_EXECUTE_READWRITE)=PAGE_EXECUTE_READWRITE) or
       ((mbi.AllocationProtect and PAGE_EXECUTE_WRITECOPY)=PAGE_EXECUTE_WRITECOPY) then
    begin
      //executable
      setlength(memoryregions,length(memoryregions)+1);
      memoryregions[length(memoryregions)-1].BaseAddress:=dword(mbi.baseaddress);
      memoryregions[length(memoryregions)-1].MemorySize:=mbi.RegionSize;
    end;

    inc(address,mbi.RegionSize);
  end;

end;


{$ifndef standalonetrainer}
procedure FillMemoryProcess(start:dword;count:dword;fillvalue:byte);
var buf: array of byte;
    original,actualwritten:dword;
begin
  setlength(buf,count);
  try
    fillmemory(@buf[0],count,fillvalue);
    rewritedata(processhandle,start,@buf[0],count);
  finally
    setlength(buf,0);
  end;
end;


Procedure CreateCodeCave(address:dword; sourceaddress:dword; sizeofcave: integer);
var x,y: dword;
    i:integer;
    ignore,temp: string;
    replacedopcodes: array of string;
    jumpback: array [0..4] of byte;
    reassembledinstruction:tassemblerbytes;

    overwritesize: dword;
begin
  x:=sourceaddress;

  while x<(sourceaddress+5) do
  begin
    setlength(replacedopcodes,length(replacedopcodes)+1);
    temp:=disassemble(x,ignore);
    temp:=copy(temp,pos('-',temp)+1,length(temp));
    temp:=copy(temp,pos('-',temp)+2,length(temp));

    replacedopcodes[length(replacedopcodes)-1]:=temp;
  end;

  overwritesize:=x-sourceaddress;

  x:=address+sizeofcave-5;
  y:=x;

  jumpback[0]:=$e9;
  pdword(@jumpback[1])^:=sourceaddress-x;

  virtualprotectex(processhandle,pointer(address),sizeofcave,PAGE_EXECUTE_READWRITE ,x);
  fillmemoryprocess(address,sizeofcave,$90);

  if sizeofcave>5 then
    writeprocessmemory(processhandle,pointer(y),@jumpback[0],5,x);

  x:=address;
  for i:=0 to length(replacedopcodes)-1 do
  begin
    assemble(replacedopcodes[i],x,reassembledinstruction);
    writeprocessmemory(processhandle,pointer(x),@reassembledinstruction[0],length(reassembledinstruction),y);
    inc(x,length(reassembledinstruction));
  end;

  virtualprotectex(processhandle,pointer(sourceaddress),overwritesize,x,y);


  //now place the jmp
  setlength(reassembledinstruction,overwritesize);
  reassembledinstruction[0]:=$e9;
  pdword(@reassembledinstruction[1])^:=address-sourceaddress-5;

  for i:=5 to overwritesize-1 do
    reassembledinstruction[i]:=$90;

  RewriteData(processhandle,sourceaddress,@reassembledinstruction[0],overwritesize);
end;
{$endif}



{$ifndef net}
procedure SetLanguage;
begin
  {$ifdef DEU}if LoadNewResourceModule(LANG_GERMAN) <> 0 then ReinitializeForms{$endif}
  {$ifdef RUS}if LoadNewResourceModule(LANG_RUSSIAN) <> 0 then ReinitializeForms{$endif}
  {$ifdef NLD}if LoadNewResourceModule(LANG_DUTCH) <> 0 then ReinitializeForms{$endif}
end;
{$endif}

{$ifndef standalonetrainer}
{$ifndef net}
//Returns a random threadid owned by the target process
function getathreadid(processid:dword):dword;
var i: integer;
    ths: thandle;
    tE: threadentry32;
begin
  if frmProcessWatcher<>nil then
  begin
    //first find a processid using the processwatcher

    frmProcessWatcher.processesMREW.BeginRead;
    try
      for i:=0 to length(frmProcessWatcher.processes)-1 do
        if frmProcessWatcher.processes[i].processid=processid then
        begin
          if length(frmProcessWatcher.processes[i].threadlist)>0 then
          begin
            result:=frmProcessWatcher.processes[i].threadlist[0].threadid;
            exit;
          end;
        end;
    finally
      frmProcessWatcher.processesMREW.EndRead;
    end;

  end;

  //no exit yet, so use a enumeration of all threads and this processid
  ths:=CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD,0);
  if ths<>0 then
  begin
    te.dwSize:=sizeof(te);
    if Thread32First(ths,te) then
    begin
      repeat
        if te.th32OwnerProcessID=processid then
        begin
          result:=te.th32ThreadID;
          closehandle(ths);
          exit;
        end;


      until not thread32Next(ths,te);
    end;
  end;

  closehandle(ths);
end;
{$endif}

procedure DetachIfPossible;
var crashcounter: integer;
    waitform: TForm;
    msg: tlabel;
begin
  //detach the debugger

  waitform:=nil;
  crashcounter:=0;

  {$ifndef net}
  if debuggerthread=nil then exit else debuggerthread.Terminate;

  if @DebugActiveProcessStop=@DebugActiveProcessStopProstitute then //lets help it a hand if it cant detach gracefully
    terminateprocess(processhandle,0)
  else
  begin
    if debuggerthread<>nil then
    begin
      //show a window asking the user to wait and not to freak out and think CE crashed!
      waitform:=TForm.Create(nil);
      with waitform do
      begin
        waitform.Caption:='Detaching...';
        msg:=TLabel.Create(waitform);
        msg.Caption:='Please wait while Cheat Engine tries to detach from the current process(This wont take longer than 30 seconds)';

        waitform.Width:=msg.Width+20;
        waitform.clientHeight:=50;

        msg.Left:=7;
        msg.Top:=(waitform.ClientHeight div 2) - (msg.Height div 2);
        msg.Parent:=waitform;

        //waitform.Parent:=self;
        waitform.BorderStyle:=bsDialog;
        waitform.BorderIcons:=[];

        waitform.Position:=poScreenCenter;
        waitform.Show;
        waitform.Repaint;
      end;
    end;
  end;

  while (debuggerthread<>nil) and (debuggerthread.attached) and (crashcounter<30) do
  begin
    inc(crashcounter);
    sleep(1000);
  end;

  if crashcounter=30 then messagedlg('Detaching failed!!! Your process is lost!',mtError,[mbok],0);

  if waitform<>nil then
  begin
    waitform.Hide;
    waitform.Free;
    waitform:=nil;
  end;


  {$endif}

end;
{$endif}


Procedure InjectDll(dllname: string; functiontocall: string='');
var LoadLibraryPtr: pointer;
    GetProcAddressPtr: Pointer;


    h: Thandle;

    inject: array [0..4095] of byte;
    x:dword;

    outp:TAssemblerBytes;
    counter: integer;
    position,position2: dword;

    dllLocation: string;
    startaddresS: dword;
    functionloc: dword;
    injectionlocation: pointer;
    threadhandle: thandle;
begin
  h:=LoadLibrary('Kernel32.dll');
  if h=0 then raise exception.Create('No kernel32.dll loaded');

  injectionlocation:=nil;
  try
    try
      getprocaddressptr:=pointer(symhandler.getAddressFromName('GetProcAddress',true));
    except
      GetProcAddressPtr:=GetProcAddress(h,'GetProcAddress');
    end;

    if getprocaddressptr=nil then raise exception.Create('GetProcAddress not found');

    try
      LoadLibraryPtr:=pointer(symhandler.getAddressFromName('LoadLibraryA',true));
    except
      //failed getting the address of LoadLibraryA, use old method
      LoadLibraryPtr:=GetProcAddress(h,'LoadLibraryA');
    end;


    if LoadLibraryptr=nil then raise exception.Create('LoadLibraryA not found');

    injectionlocation:=VirtualAllocEx(processhandle,nil,4096,MEM_COMMIT,PAGE_EXECUTE_READWRITE);

    if injectionlocation=nil then raise exception.Create('Failed to allocate memory');

    dlllocation:=dllname;

    position:=dword(injectionlocation);
    position2:=0;
    copymemory(@inject[0],pchar(dllLocation+#0),length(dllLocation)+1);
    inc(position,length(dllLocation)+1);
    inc(position2,length(dllLocation)+1);

    functionloc:=position;
    copymemory(@inject[position2],pchar(functiontocall+#0),length(functiontocall)+1);
    inc(position,length(functiontocall)+1);
    inc(position2,length(functiontocall)+1);
    startaddress:=position;

    //loadlibrary(cehook);
    assemble('PUSH '+IntToHex(dword(injectionlocation),8),position,outp);
    copymemory(@inject[position2],outp,length(outp));
    inc(position,length(outp));
    inc(position2,length(outp));

    assemble('CALL '+IntToHex(dword(LoadLibraryPtr),8),position,outp);
    copymemory(@inject[position2],outp,length(outp));
    inc(position,length(outp));
    inc(position2,length(outp));


    //safetycode, test if the dll was actually loaded and skip if not
    assemble('TEST EAX,EAX',position,outp);
    copymemory(@inject[position2],outp,length(outp));
    inc(position,length(outp));
    inc(position2,length(outp));

    assemble('JNE '+inttohex(position+3+5,8),position,outp); //jump over the ret
    copymemory(@inject[position2],outp,length(outp));
    inc(position,length(outp));
    inc(position2,length(outp));

    assemble('MOV EAX,2',position,outp); //exitcode=2
    copymemory(@inject[position2],outp,length(outp));
    inc(position,length(outp));
    inc(position2,length(outp));

    assemble('RET',position,outp);
    copymemory(@inject[position2],outp,length(outp));
    inc(position,length(outp));
    inc(position2,length(outp));


    if functiontocall<>'' then
    begin
      //getprocaddress
      assemble('PUSH '+IntToHex(functionloc,8),position,outp);
      copymemory(@inject[position2],outp,length(outp));
      inc(position,length(outp));
      inc(position2,length(outp));

      assemble('PUSH EAX',position,outp);
      copymemory(@inject[position2],outp,length(outp));
      inc(position,length(outp));
      inc(position2,length(outp));

      assemble('CALL '+IntToHex(dword(GetProcAddressPtr),8),position,outp);
      copymemory(@inject[position2],outp,length(outp));
      inc(position,length(outp));
      inc(position2,length(outp));

      assemble('TEST EAX,EAX',position,outp);
      copymemory(@inject[position2],outp,length(outp));
      inc(position,length(outp));
      inc(position2,length(outp));

      assemble('JNE '+inttohex(position+3+5,8),position,outp);
      copymemory(@inject[position2],outp,length(outp));
      inc(position,length(outp));
      inc(position2,length(outp));

      assemble('MOV EAX,3',position,outp); //exitcode=3
      copymemory(@inject[position2],outp,length(outp));
      inc(position,length(outp));
      inc(position2,length(outp));

      assemble('RET',position,outp);
      copymemory(@inject[position2],outp,length(outp));
      inc(position,length(outp));
      inc(position2,length(outp));

      //call function
      assemble('CALL EAX',position,outp);
      copymemory(@inject[position2],outp,length(outp));
      inc(position,length(outp));
      inc(position2,length(outp));
    end;


    assemble('MOV EAX,1',position,outp); //causes the exitcode of the thread be 1
    copymemory(@inject[position2],outp,length(outp));
    inc(position,length(outp));
    inc(position2,length(outp));

    assemble('RET',position,outp);
    copymemory(@inject[position2],outp,length(outp));
    inc(position,length(outp));
    inc(position2,length(outp));


    //call the routine

    if not writeprocessmemory(processhandle,injectionlocation,@inject[0],position2,x) then raise exception.Create('Failed to inject the dll loader');
    
    {$ifndef standalonetrainer}
    {$ifndef net}   

    useapctoinjectdll:=false;
    if useapctoinjectdll then
    begin
      showmessage('injected code at:'+inttohex(startaddress,8));
      
      //suspend , message, resume is needed to prevent a crash when it is in a message loop 
      ntsuspendprocess(processid);
      x:=getathreadid(processid);
      PostThreadMessage(x,wm_paint,0,0);
      CreateRemoteAPC(x,pointer(startaddress));
      ntresumeprocess(processid);
    end
    else

    {$endif}
    {$endif}
    begin      
      threadhandle:=createremotethread(processhandle,nil,0,pointer(startaddress),nil,0,x);
      if threadhandle=0 then raise exception.Create('Failed to execute the dll loader');

      counter:=10000 div 10;
      while (waitforsingleobject(threadhandle,10)=WAIT_TIMEOUT) and (counter>0) do
      begin
        if GetCurrentThreadID = MainThreadID then
          CheckSynchronize; //handle sychronize calls while it's waiting
           
        dec(counter);
      end;

      if (counter=0) then
        raise exception.Create('The injection thread took longer than 10 seconds to execute. Injection routine not freed');

      if getexitcodethread(threadhandle,x) then
      begin
        case x of
          1: ;//success
          2: raise exception.Create('Failed injecting the DLL');
          3: raise exception.Create('Failed executing the function of the dll');
          else raise exception.Create('Unknown error during injection');
        end;
      end; //else unsure, did it work or not , or is it crashing?

    end;
  finally
    FreeLibrary(h);
    if injectionlocation<>nil then
      virtualfreeex(processhandle,injectionlocation,0,MEM_RELEASE	);
  end;

end;

procedure EnableStealth;
var hookloc: pointer;
    dll: thandle;
    dllloc: string;
    dllloc1: string;
    dllloc2: string;
    i: integer;
begin
  if stealthhook>0 then exit;

  createdir(cheatenginedir+'temp'); //in case it doesn't exist

  dllloc1:=cheatenginedir+'temp\';
  dllloc2:=cheatenginedir+'temp\';


   //random sized name

  for i:=1 to 3+random(6) do
    dllloc1:=dllloc1+chr(64+random(26));

  dllloc1:=dllloc1+'.DLL';
  dllloc:=cheatenginedir+'stealth.dll';


  if copyfile(pchar(dllloc),pchar(dllloc1),false) then
    dll:=LoadLibrary(pchar(dllloc1))
  else
    dll:=loadlibrary(pchar(dllloc));


  hookloc:=getprocaddress(dll,'hook');
  hyperscanview.formscanningHandle:=setwindowshookex(WH_CALLWNDPROCRET	,hookloc,DLL,0); //just to get the dll inside the process
  stealthhook:=hyperscanview.formscanningHandle;
end;

procedure DisableStealth;
var i: integer;
    filedata:_WIN32_FIND_DATAA;
    f: thandle;
    filelist: array of string;
begin
  if stealthhook<>0 then Unhookwindowshookex(stealthhook);
  stealthhook:=0;
  //delete tempdir

  f:=FindFirstFile(pchar(cheatenginedir+'temp\*.DLL'),filedata);
  if f<>invalid_handle_value then
  begin
    setlength(filelist,1);
    filelist[length(filelist)-1]:=cheatenginedir+'temp\'+filedata.cFileName;
    while findnextfile(f,filedata) do
    begin
      setlength(filelist,length(filelist)+1);
      filelist[length(filelist)-1]:=cheatenginedir+'temp\'+filedata.cFileName;
    end;

    windows.FindClose(f);
  end;

  for i:=0 to length(filelist)-1 do
    deletefile(filelist[i]);

  setlength(filelist,0);
  removedirectory(pchar(cheatenginedir+'temp'));
end;

procedure ToggleOtherWindows;
type Tprocesslistitem = record
  processid: dword;
  processname: string;
end;
var winhandle: Hwnd;
    winprocess: Dword;
    i,j: integer;
    SNAPHandle: THandle;
    ProcessEntry: ProcessEntry32;
    Check: Boolean;
    processlist: array of Tprocesslistitem;
    hideall,hidethisone: boolean;
begin
  hideall:=false;

  allwindowsareback:=false;

  if length(windowlist)<>0 then
  begin
    for i:=0 to length(windowlist)-1 do
      showwindow(windowlist[i],SW_SHOW);

    setlength(windowlist,0);
    allwindowsareback:=true;
    exit;
  end;

  lastactive:=getactivewindow;
  lastforeground:=GetForegroundWindow;

  if onlyfront then
  begin
    GetWindowThreadProcessId(lastforeground,addr(winprocess));
    if getcurrentprocessid=winprocess then
    begin
      beep;
      sleep(100);
      beep;
      sleep(100);
      exit;
    end;

    setlength(windowlist,1);
    windowlist[0]:=lastforeground;
    showwindow(lastforeground,sw_hide);
    exit;
  end;


  if length(donthidelist)>0 then
  begin
    //first get a process list
    SNAPHandle:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
    If SnapHandle>0 then
    begin
      ProcessEntry.dwSize:=SizeOf(ProcessEntry);
      Check:=Process32First(SnapHandle,ProcessEntry);
      while check do
      begin
        if processentry.th32ProcessID<>0 then
        begin
          setlength(processlist,length(processlist)+1);
          processlist[length(processlist)-1].processid:=processentry.th32ProcessID;
          processlist[length(processlist)-1].processname:=lowercase(ExtractFilename(processentry.szExeFile));
        end;
        check:=Process32Next(SnapHandle,ProcessEntry);
      end;
    end else hideall:=true; //else sorry dude, but no exceptions for you, say goodbye to ALL your windows
  end else hideall:=true;

  winhandle:=getwindow(GetForegroundWindow,GW_HWNDFIRST);

  while winhandle<>0 do
  begin
    GetWindowThreadProcessId(winhandle,addr(winprocess));

    if (winprocess<>getCurrentProcessID) {and (winprocess<>3600) }then
    begin
      if isWindowVisible(winhandle) then
      begin
        hidethisone:=true;
        if not hideall then
        begin
          //see if you can hide it or not
          //check this window process with the process list
          //and then see if the processname equals an item from the donthide list
          for i:=0 to length(processlist)-1 do
            if processlist[i].processid=winprocess then
            begin
              //found the process id, now check if the processname of this process equals an item from the list
              for j:=0 to length(donthidelist)-1 do
                if processlist[i].processname=donthidelist[j] then //it's in so do not hide
                begin
                  hidethisone:=false;
                  break;
                end;
              break;
            end;
        end;



        if hidethisone then
        begin
          showwindow(winhandle,SW_HIDE);
//          setwindowpos(winhandle,0,0,0,0,0,SWP_HIDEWINDOW or SWP_NOREPOSITION or SWP_NOSIZE	or SWP_NOZORDER	or SWP_NOACTIVATE	or SWP_NOREDRAW	or SWP_NOSENDCHANGING);
          setlength(windowlist,length(windowlist)+1);
          windowlist[length(windowlist)-1]:=winhandle;
        end;
     //   showwindow(winhandle,sw_show); //remove this for real version
      end;
    end;

    winhandle:=getwindow(winhandle,GW_HWNDNEXT);
  end;

 // application.BringToFront;
end;

function GetSystemType: Integer;  //from Stuart Johnson with a little change by me
const
 { operating system constants }

 cOsUnknown = 999999;
 cOsWin95 = 0;
 cOsWin98 = 1;
 cOsWin98SE = 2;
 cOsWinME = 3;
 cOsWinNT = 4;
 cOsWin2000 = 5;
 cOsWinXP = 6;
 cOsNewer = 7;

var
 osVerInfo : TOSVersionInfo;
 majorVer, minorVer : Integer;

begin
 if overridedebug then
 begin
   result:=cOsWinXP;
   exit;
 end;

{ set operating system type flag }
 osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
 if GetVersionEx(osVerInfo) then
   begin
     majorVer := osVerInfo.dwMajorVersion;
     minorVer := osVerInfo.dwMinorVersion;
     case osVerInfo.dwPlatformId of
       VER_PLATFORM_WIN32_NT : { Windows NT/2000 }
         begin
           if majorVer <= 4 then
             result := cOsWinNT
           else
             if (majorVer = 5) AND (minorVer= 0) then
               result := cOsWin2000
             else
               if (majorVer = 5) AND (minorVer = 1) then
                 result := cOsWinXP
             else if (majorver > 5) then result:=cOsNewer
           else
           result := cOsUnknown;
         end; {case }
     VER_PLATFORM_WIN32_WINDOWS : { Windows 9x/ME }
       begin
         if (majorVer = 4) AND (minorVer = 0) then
           result := cOsWin95
         else
           if (majorVer = 4) AND (minorVer = 10) then
             begin
               if osVerInfo.szCSDVersion[1] = 'A' then
                 result := cOsWin98SE
               else
                  result := cOsWin98;
               end {if Version = 'A'}
             else
               if (majorVer = 4) AND (minorVer = 90) then
                 result := cOsWinME
               else
                  result := cOsUnknown;
       end; {case VER_PLATFORM_WIN32_WINDOWS}
     else
      result := cOsUnknown;
   end;
 end
else
  result := cOsUnknown;
end;

function Is64bitOS: boolean;
var iswow64: BOOL;
begin
  result:=false;
  if assigned(IsWow64Process) then
  begin
    iswow64:=false;
    if IsWow64Process(GetCurrentProcess,iswow64) and iswow64 then
      result:=true;
  end;
end;

function KeyToStr(key:word):string;
begin
  case key of
    VK_BACK	: result:='Backspace';
    VK_TAB	: result:='Tab';
    VK_CLEAR	: result:='Clear';
    VK_RETURN	: result:='Enter';
    VK_PAUSE	: result:='Pause';
    VK_CAPITAL	: result:='Caps Lock';
    VK_ESCAPE	: result:='Esc';
    VK_SPACE	: result:='Space bar';
    VK_PRIOR	: result:='Page Up';
    VK_NEXT	: result:='Page Down';
    VK_END	: result:='End';
    VK_HOME	: result:='Home';
    VK_LEFT	: result:='Left Arrow';
    VK_UP	: result:='Up Arrow';
    VK_RIGHT	: result:='Right Arrow';
    VK_DOWN	: result:='Down Arrow';
    VK_SELECT	: result:='Select';
    VK_PRINT	: result:='Print';
    VK_EXECUTE	: result:='Execute';
    VK_SNAPSHOT	: result:='Print Screen';
    VK_INSERT	: result:='Insert';
    VK_DELETE	: result:='Delete';
    VK_HELP	: result:='Help';
    VK_LWIN	: result:='Left Windows key';
    VK_RWIN	: result:='Right Windows key';
    VK_APPS	: result:='Applications key';
    VK_NUMPAD0	: result:='numeric 0';
    VK_NUMPAD1	: result:='numeric 1';
    VK_NUMPAD2	: result:='numeric 2';
    VK_NUMPAD3	: result:='numeric 3';
    VK_NUMPAD4	: result:='numeric 4';
    VK_NUMPAD5	: result:='numeric 5';
    VK_NUMPAD6	: result:='numeric 6';
    VK_NUMPAD7	: result:='numeric 7';
    VK_NUMPAD8	: result:='numeric 8';
    VK_NUMPAD9	: result:='numeric 9';
    VK_MULTIPLY	: result:='numeric *';
    VK_ADD	: result:='numeric +';
    VK_SEPARATOR : result:='numeric Separator';
    VK_SUBTRACT	: result:='numeric -';
    VK_DECIMAL	: result:='numeric .';
    VK_DIVIDE	: result:='numeric /';
    VK_F1	: result:='F1';
    VK_F2	: result:='F2';
    VK_F3	: result:='F3';
    VK_F4	: result:='F4';
    VK_F5	: result:='F5';
    VK_F6	: result:='F6';
    VK_F7	: result:='F7';
    VK_F8	: result:='F8';
    VK_F9	: result:='F9';
    VK_F10	: result:='F10';
    VK_F11	: result:='F11';
    VK_F12	: result:='F12';
    VK_F13	: result:='F13';
    VK_F14	: result:='F14';
    VK_F15	: result:='F15';
    VK_F16	: result:='F16';
    VK_F17	: result:='F17';
    VK_F18	: result:='F18';
    VK_F19	: result:='F19';
    VK_F20	: result:='F20';
    VK_F21	: result:='F21';
    VK_F22	: result:='F22';
    VK_F23	: result:='F23';
    VK_F24	: result:='F24';
    VK_NUMLOCK	: result:='Num Lock';
    VK_SCROLL	: result:='Scroll Lock';
    48..57      : result:=chr(key);
    65..90      : result:=chr(key);
    else  result:='#'+IntToStr(key);
  end;

end;

procedure decimal(var key: char); //removed
begin
{
  case key of
    chr(8)   : ;
    chr(16)  : ;
    '0'..'9' : ;
    else key:=chr(0);
  end;
  }
end;

procedure hexadecimal(var key: char);  //removed
begin
  {case key of
    chr(8)   : ;
    chr(16)  : ;
    'A'..'F' : ;
    'a'..'f' : key:=uppercase(key)[1];
    '0'..'9' : ;
    else key:=chr(0);
  end; }
end;

function ByteStringToText(s: string;hex: boolean):string;
var temp: tbytes;
    i,j: integer;
begin
  ConvertStringToBytes(s,hex,temp);
  result:='';

  for i:=0 to length(temp)-1 do
    if temp[i]>$13 then
      result:=result+chr(temp[i]);
end;


function ByteStringToDouble(s: string;hex: boolean):double;
var temp: tbytes;
    temp2: double;
    p: ^byte;
    i,j: integer;
begin
  ConvertStringToBytes(s,hex,temp);
  p:=@temp2;

  if length(temp)<8 then
  begin
    j:=length(temp);
    setlength(temp,8);
    for i:=j to 7 do
      temp[i]:=0;
  end;

  for i:=0 to length(temp)-1 do
  begin
    if temp[i]=-1 then temp[i]:=0;

    p^:=byte(temp[i]);
    inc(p);
  end;

  result:=temp2;
end;


function ByteStringToSingle(s: string;hex: boolean):single;
var temp: tbytes;
    temp2: single;
    p: ^byte;
    i,j: integer;
begin
  ConvertStringToBytes(s,hex,temp);
  p:=@temp2;

  if length(temp)<4 then
  begin
    j:=length(temp);
    setlength(temp,4);
    for i:=j to 3 do
      temp[i]:=0;
  end;

  for i:=0 to length(temp)-1 do
  begin
    if temp[i]=-1 then temp[i]:=0;

    p^:=byte(temp[i]);
    inc(p);
  end;

  result:=temp2;
end;

function ByteStringToInt(s: string;hex: boolean):int64;
var temp: tbytes;
    i: integer;
    power: integer;

begin
  ConvertStringToBytes(s,hex,temp);
  power:=0;
  result:=0;

  for i:=0 to length(temp)-1 do
  begin
    result:=result+(temp[i]*trunc(math.power(256,power)));
    inc(power);
  end;
end;

function VarToBytes(v: pointer; size: integer): string;
var p: ^byte;
    j,k: integer;
    res: array of string;
begin
  result:='';
  p:=v;

  setlength(res,size);

  for k:=0 to size-1 do
  begin
    res[k]:=inttohex(p^,2);
    inc(p);
  end;

  j:=size;
  for k:=size-1 to 1 do
    if res[k]='00' then dec(j);

  for k:=0 to j-1 do
    result:=result+res[k]+' ';

  result:=copy(result,1,length(result)-1);
end;

function BinToInt(s: string): int64;
var i: integer;
begin
  result:=0;
  for i:=length(s) downto 1 do
    if s[i]='1' then result:=result+trunc(power(2,length(s)-i ));
end;

function Inttobin(i: int64): string;
var temp,temp2: string;
    j: integer;
begin
  temp:='';
  while i>0 do
  begin
    if (i mod 2)>0 then temp:=temp+'1'
                   else temp:=temp+'0';
    i:=i div 2;
  end;

  temp2:='';
  for j:=length(temp) downto 1 do
    temp2:=temp2+temp[j];
  result:=temp2;
end;



function scanbits(var found: dword;number:dword;var bytep: pbyte;nrofbits,i,actualread: integer): boolean;
var j,k,l,m: integer;
    actualwrite: dword;
    tempcount: integer;
    tempj,tempk: integer;
    tempb: pbyte;
begin
  for j:=0 to actualread-1 do
  begin

    //scan each bit till you find  bitarray[bittofind]
    for k:=0 to 7 do
    begin
      tempb:=bytep;
      //see if there are enough bits to scan, if not, save the bits to be scanned and exit
      //bitsleft=((actualread-j)*8) - k
      if nrofbits>(((actualread-j)*8) - k) then
      begin
        tempcount:=(((actualread-j)*8) - k);     //should always be nrofbits-1
        setlength(tempbits,nrofbits);

        tempk:=k;
        for l:=1 to tempcount do
        begin
          tempbits[l]:=getbit(tempk,bytep^);
          inc(tempk);
          if tempk>7 then
          begin
            inc(bytep);
            tempk:=0;
          end;
        end;
        result:=true;
        exit;
      end;

      m:=k;
      for l:=0 to nrofbits-1 do
      begin
        if bitscan[l]<>2 then
          if getbit(m,tempb^)<>bitscan[l] then break;

        if l=nrofbits-1 then
        begin
          //foundit
          foundaddressb[found].address:=memoryregion[i].BaseAddress+j;
          foundaddressb[found].bit:=k;
          inc(found);
          if found=number then
          begin
            blockwrite(Addressfile,pointer(foundaddressB)^,found*(sizeof(Tbitaddress)),actualwrite);
            found:=0;
          end;
        end;

        inc(m);
        if m>7 then
        begin
          m:=0;
          inc(tempb);
        end;
      end;

    end;
    inc(bytep);
  end;


  result:=true;
end;

function getbit(bitnr: integer; bt: dword):integer;
begin
  result:=(bt shr bitnr) and 1;
end;

procedure setbit(bitnr: integer; var bt: dword;state:integer); overload;
{
 pre: bitnr=bit between 0 and 7
         bt=pointer to the byte
 post: bt has the bit set specified in state
 result: bt has a bit set or unset
}
begin
  bt:=bt and (not (1 shl bitnr));
  bt:=bt or (state shl bitnr);
end;

procedure setbit(bitnr: integer; var bt: Byte;state:integer); overload;
{
 pre: bitnr=bit between 0 and 7
         bt=pointer to the byte
 post: bt has the bit set specified in state
 result: bt has a bit set or unset
}
var d: dword;
begin
  d:=bt;
  setbit(bitnr,d,state);
  bt:=d;
end;

function eflags_setCF(flagvalue: dword; value: integer): DWORD;
begin
  result:=flagvalue and (not (1 shl 0)) or (value shl 0);
end;

function eflags_setPF(flagvalue: dword; value: integer): DWORD;
begin
  result:=flagvalue and (not (1 shl 2)) or (value shl 2);
end;

function eflags_setAF(flagvalue: dword; value: integer): DWORD;
begin
  result:=flagvalue and (not (1 shl 4)) or (value shl 4);
end;

function eflags_setZF(flagvalue: dword; value: integer): DWORD;
begin
  result:=flagvalue and (not (1 shl 6)) or (value shl 6);
end;

function eflags_setSF(flagvalue: dword; value: integer): DWORD;
begin
  result:=flagvalue and (not (1 shl 7)) or (value shl 7);
end;

function eflags_setTF(flagvalue: dword; value: integer): DWORD;
begin
  result:=flagvalue and (not (1 shl 8)) or (value shl 8);
end;

function eflags_setIF(flagvalue: dword; value: integer): DWORD;
begin
  result:=flagvalue and (not (1 shl 9)) or (value shl 9);
end;

function eflags_setDF(flagvalue: dword; value: integer): DWORD;
begin
  result:=flagvalue and (not (1 shl 10)) or (value shl 10);
end;

function eflags_setOF(flagvalue: dword; value: integer): DWORD;
begin
  result:=flagvalue and (not (1 shl 11)) or (value shl 11);
end;

function eflags_setIOPL(flagvalue: dword; value: integer): DWORD;
begin
  result:=flagvalue and (not (3 shl 12)) or (value shl 12);
end;

function eflags_setNT(flagvalue: dword; value: integer): DWORD;
begin
  result:=flagvalue and (not (1 shl 14)) or (value shl 14);
end;

function eflags_setRF(flagvalue: dword; value: integer): DWORD;
begin
  result:=flagvalue and (not (1 shl 16)) or (value shl 16);
end;

function eflags_setVM(flagvalue: dword; value: integer): DWORD;
begin
  result:=flagvalue and (not (1 shl 17)) or (value shl 17);
end;

function eflags_setAC(flagvalue: dword; value: integer): DWORD;
begin
  result:=flagvalue and (not (1 shl 18)) or (value shl 18);
end;

function eflags_setVIF(flagvalue: dword; value: integer): DWORD;
begin
  result:=flagvalue and (not (1 shl 19)) or (value shl 19);
end;

function eflags_setVIP(flagvalue: dword; value: integer): DWORD;
begin
  result:=flagvalue and (not (1 shl 20)) or (value shl 20);
end;

function eflags_setID(flagvalue: dword; value: integer): DWORD;
begin
  result:=flagvalue and (not (1 shl 21)) or (value shl 21);
end;

function undolastscan(valtype: integer;hexadecimal:boolean): integer;
begin
  deletefile(CheatEngineDir+'Memory.tmp');
  deletefile(CheatEngineDir+'Addresses.tmp');

  renamefile(CheatEngineDir+'Memory.UNDO',CheatEngineDir+'Memory.tmp');
  renamefile(CheatEngineDir+'Addresses.UNDO',CheatEngineDir+'Addresses.tmp');

  assignfile(addressfile,CheatEngineDir+'Addresses.tmp');
  assignfile(memoryfile,CheatEngineDir+'Memory.tmp');
end;

function AvailMem:dword;
var x: _MEMORYSTATUS;
begin
  x.dwLength:=sizeof(x);
  GlobalMemoryStatus(x);

  if x.dwAvailVirtual>(x.dwAvailPhys+x.dwAvailPageFile) then
    result:=x.dwAvailPhys+x.dwAvailPageFile
  else
    result:=x.dwAvailVirtual;

end;

procedure RemoveAddress(address: Dword;bit: Byte; vartype: Integer);
type bitaddress = record
  address: dword;
  bit: dword;
end;

var

    Addresses: Array [1..number] of Dword;
    BAddress: Array [1..number] of BitAddress;
    Memory: Array [1..8*number] of Byte;
    i,j: Integer;

    str: pchar;

    found: boolean;
    check,check2: Integer;


begin
  assignfile(memoryfile,CheatEngineDir+'Memory.TMP');
  assignfile(addressfile,CheatEngineDir+'Addresses.TMP');
  reset(memoryfile,1);
  reset(addressfile,1);

  assignfile(newmemoryfile,CheatEngineDir+'Memory2.TMP');
  assignfile(newaddressfile,CheatEngineDir+'Address2.TMP');
  rewrite(newmemoryfile,1);
  rewrite(newaddressfile,1);

  blockread(addressfile,memory,7,check);
  blockwrite(newaddressfile,memory,7,check);

  found:=false;
  i:=0;

  if vartype=7 then  //text scan
  begin
    i:=filesize(memoryfile);

    getmem(str,i+1);
    blockread(memoryfile,pointer(str)^,i,check);
    str[i]:=chr(0);
    blockwrite(newmemoryfile,pointer(str)^,i,check);

    check:=4*number;
    while (check=4*number) do
    begin
      blockread(addressfile,addresses,4*number,check);
      i:=0;
      j:=0;
      while (i<check div 4) do
      begin
        inc(i);
        if addresses[i]<>address then //if it's not the selected address write it else dont write it.
          blockwrite(newaddressfile,addresses[i],4,check2);
      end;
    end;
  end
  else
  if vartype<>5 then
  begin
    check:=4*number;
    while ((not found) and (check=4*number)) do
    begin
      blockread(addressfile,addresses,4*number,check);
      i:=0;
      while (not found) and (i<(check div 4)) do
      begin
        inc(i);
        if addresses[i]=address then
        begin
          found:=true;
          break;
        end;
      end;

      if not found then
      begin
        blockwrite(newaddressfile,addresses,4*number,check2);
        case vartype of
          0:    begin //byte
                  blockread(memoryfile,memory,number,check2);
                  blockwrite(newmemoryfile,memory,check2,check2);
                end;

          1:    begin //word
                  blockread(memoryfile,memory,2*number,check2);
                  blockwrite(newmemoryfile,memory,check2,check2);
                end;

          2:    begin //dword
                  blockread(memoryfile,memory,4*number,check2);
                  blockwrite(newmemoryfile,memory,check2,check2);
                end;


          3:    begin //float
                  blockread(memoryfile,memory,4*number,check2);
                  blockwrite(newmemoryfile,memory,check2,check2);
                end;

          4:    begin //double
                  blockread(memoryfile,memory,8*number,check2);
                  blockwrite(newmemoryfile,memory,check2,check2);
                end;

          6:    begin //int64
                  blockread(memoryfile,memory,8*number,check2);
                  blockwrite(newmemoryfile,memory,check2,check2);
                end;


          //bit doesnt come here
        end;
      end;
    end;

    if found then
    begin
      for j:=i to number-1 do
        addresses[j]:=addresses[j+1];

      blockwrite(newaddressfile,addresses,check-4,check2);

      case vartype of
        0:    begin //byte
                blockread(memoryfile,memory,number,check2);
                for j:=i to number-1 do memory[j]:=memory[j+1];
                blockwrite(newmemoryfile,memory,check2-1,check2);
              end;

        1:    begin //word
                blockread(memoryfile,memory,2*number,check2);
                for j:=i to number-4 do
                begin
                  memory[j]:=memory[j+2];
                  memory[j+1]:=memory[j+3];
                end;
                blockwrite(newmemoryfile,memory,check2-2,check2);
              end;

        2:    begin //dword
                blockread(memoryfile,memory,4*number,check2);
                for j:=i to number-8 do
                begin
                  memory[j]:=memory[j+4];
                  memory[j+1]:=memory[j+5];
                  memory[j+2]:=memory[j+6];
                  memory[j+3]:=memory[j+7];
                end;

                blockwrite(newmemoryfile,memory,check2-4,check2);
              end;


        3:    begin //float
                blockread(memoryfile,memory,4*number,check2);
                for j:=i to number-8 do
                begin
                  memory[j]:=memory[j+4];
                  memory[j+1]:=memory[j+5];
                  memory[j+2]:=memory[j+6];
                  memory[j+3]:=memory[j+7];
                end;
                blockwrite(newmemoryfile,memory,check2-4,check2);
              end;

        4:    begin //double
                blockread(memoryfile,memory,8*number,check2);
                for j:=i to number-16 do
                begin
                  memory[j]:=memory[j+8];
                  memory[j+1]:=memory[j+9];
                  memory[j+2]:=memory[j+10];
                  memory[j+3]:=memory[j+11];
                  memory[j+4]:=memory[j+12];
                  memory[j+5]:=memory[j+13];
                  memory[j+6]:=memory[j+14];
                  memory[j+7]:=memory[j+15];
                end;
                blockwrite(newmemoryfile,memory,check2-8,check2);
              end;

        6:    begin //Int64
                blockread(memoryfile,memory,8*number,check2);
                for j:=i to number-16 do
                begin
                  memory[j]:=memory[j+8];
                  memory[j+1]:=memory[j+9];
                  memory[j+2]:=memory[j+10];
                  memory[j+3]:=memory[j+11];
                  memory[j+4]:=memory[j+12];
                  memory[j+5]:=memory[j+13];
                  memory[j+6]:=memory[j+14];
                  memory[j+7]:=memory[j+15];
                end;
                blockwrite(newmemoryfile,memory,check2-8,check2);
              end;


        end;

        //and now just copy the addresses till the end
        check:=4*number;
        while (check=4*number) do
        begin
          blockread(addressfile,addresses,4*number,check);
          blockwrite(newaddressfile,addresses,check,check);
        end;

        //and same for memory
        check:=4*number;
        while (check=4*number) do
        begin
          blockread(memoryfile,addresses,4*number,check);
          blockwrite(newaddressfile,addresses,check,check);
        end;
    end;

  end;

  closefile(memoryfile);
  closefile(addressfile);
  closefile(newmemoryfile);
  closefile(newaddressfile);

  deletefile(CheatEngineDir+'Memory.UNDO');
  deletefile(CheatEngineDir+'Addresses.UNDO');
  renamefile(CheatEngineDir+'Memory.tmp',cheatenginedir+'Memory.UNDO');
  renamefile(CheatEngineDir+'Addresses.tmp',CheatEngineDir+'Addresses.UNDO');
  renamefile(CheatEngineDir+'Memory2.tmp',CheatEngineDir+'Memory.TMP');
  Renamefile(CheatengineDir+'Address2.TMP',CheatEngineDir+'Addresses.TMP');


end;

procedure Open_Process;
begin
  {$ifndef netclient}
  ProcessHandler.ProcessHandle:=NewKernelHandler.OpenProcess(PROCESS_ALL_ACCESS,false,ProcessID);
  le:=GetLastError;
  {$endif}
end;
          {
function MakeAddressWritable(address: dword):boolean;
var buf,x:dword;
begin
  result:=false;
  if ReadProcessMemory(processhandle,pointeR(address),@buf,4,x) then
  begin
    if ReadProcessMemory(processhandle,pointer(((address div $1000)*4)+$c0000000),@buf,4,x) then
    begin
      if copyonwrite then
        buf:=(buf or $200) //when you write to it it will copy the page and give that to the process in writable state
      else
        buf:=(buf or $2);  //just make it writable, even if it is shared

      result:=WriteProcessMemory(processhandle,pointer(((address div $1000)*4)+$c0000000),@buf,4,x);
    end;
  end;
end;
           }


procedure quicksortmemoryregions(lo,hi: integer);
var i,j: integer;
    x,h: TMemoryRegion;
begin
  i:=lo;
  j:=hi;

  x:=memoryregion[(lo+hi) div 2];

  repeat
    while (memoryregion[i].BaseAddress<x.BaseAddress) do inc(i);
    while (memoryregion[j].BaseAddress>x.BaseAddress) do dec(j);

    if i<=j then
    begin
      h:=memoryregion[i];
      memoryregion[i]:=memoryregion[j];
      memoryregion[j]:=h;
      inc(i);
      dec(j);
    end;

  until i>j;

  if (lo<j) then quicksortmemoryregions(lo,j);
  if (i<hi) then quicksortmemoryregions(i,hi);
end;


     {
procedure GetProcessListSmall(ProcessList: TListBox);
Var SNAPHandle: THandle;
    ProcessEntry: ProcessEntry32;
    Check: Boolean;
begin
  processlist.clear;

  processlist.Sorted:=false;
  SNAPHandle:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
  If SnapHandle>0 then
  begin
    ProcessEntry.dwSize:=SizeOf(ProcessEntry);
    Check:=Process32First(SnapHandle,ProcessEntry);
    while check do
    begin
      if processentry.th32ProcessID<>0 then
        ProcessList.Items.Add(ExtractFilename(processentry.szExeFile));

      check:=Process32Next(SnapHandle,ProcessEntry);
    end;
  end else raise exception.Create('I can''t get the process list. You are propably using windows NT. Use the window list instead!');
end;   }


procedure GetProcessList(ProcessList: TListBox);
var sl: tstringlist;
    i: integer;
begin
  sl:=tstringlist.create;
  try
    processlist.Sorted:=false;
    for i:=0 to processlist.Items.count-1 do
      if processlist.Items.Objects[i]<>nil then
        freemem(pointer(processlist.Items.Objects[i]));

    processlist.Items.Clear;

    
    GetProcessList(sl);
    processlist.Items.AddStrings(sl);
  finally
    sl.free;
  end;
end;


function GetFirstModuleName(processid: dword): string;
var
  SNAPHandle: THandle;
  check: boolean;
  ModuleEntry: MODULEENTRY32;
begin
  SNAPHandle:=CreateToolhelp32Snapshot(TH32CS_SNAPMODULE,processid);
  if SNAPHandle<>0 then
  begin
    ModuleEntry.dwSize:=sizeof(moduleentry);
    if Module32First(snaphandle,ModuleEntry) then
      result:=moduleentry.szExePath
    else
      result:='';

    closehandle(SNAPHandle);
  end;
end;

procedure GetProcessList(ProcessList: TStrings);
Var SNAPHandle: THandle;
    ProcessEntry: ProcessEntry32;
    Check: Boolean;

    HI: HICON;
    ProcessListInfo: PProcessListInfo;
    i: integer;
    s: string;
begin
  HI:=0;

  for i:=0 to processlist.count-1 do
    if processlist.Objects[i]<>nil then
      freemem(pointer(processlist.Objects[i]));

  processlist.clear;


  SNAPHandle:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
  If SnapHandle>0 then
  begin
    ProcessEntry.dwSize:=SizeOf(ProcessEntry);
    Check:=Process32First(SnapHandle,ProcessEntry);
    while check do
    begin
      if getprocessicons then
      begin
        HI:=ExtractIcon(hinstance,ProcessEntry.szExeFile,0);
        if HI=0 then
        begin
          //alternative method:
          if processentry.th32ProcessID>0 then
          begin
            s:=GetFirstModuleName(processentry.th32ProcessID);
            HI:=ExtractIcon(hinstance,pchar(s),0);
          end;
        end;
      end;


      if not (ProcessesWithIconsOnly and (hi=0)) then
      begin
        if processentry.th32ProcessID<>0 then
        begin
         // processinfo
          getmem(ProcessListInfo,sizeof(TProcessListInfo));
          ProcessListInfo.processID:=processentry.th32ProcessID;
          ProcessListInfo.processIcon:=HI;

          ProcessList.AddObject(IntTohex(processentry.th32ProcessID,8)+'-'+ExtractFilename(processentry.szExeFile), TObject(ProcessListInfo));
        end;
      end;

      check:=Process32Next(SnapHandle,ProcessEntry);
    end;
    closehandle(snaphandle);
  end else raise exception.Create('I can''t get the process list. You are propably using windows NT. Use the window list instead!');
end;

procedure GetWindowList(ProcessList: TListBox{; var ArrIcons: TBytes});
var previouswinhandle, winhandle: Hwnd;
    winprocess: Dword;
    temp: Pchar;
    wintitle: string;

    x: tstringlist;
    i:integer;

    ProcessListInfo: PProcessListInfo;
begin
  getmem(temp,101);
  try
    x:=tstringlist.Create;

    for i:=0 to processlist.items.count-1 do
      if processlist.items.Objects[i]<>nil then
        freemem(pointer(processlist.items.Objects[i]));
    processlist.clear;

    winhandle:=getwindow(getforegroundwindow,GW_HWNDFIRST);

    i:=0;
    while (winhandle<>0) and (i<10000) do
    begin
      GetWindowThreadProcessId(winhandle,addr(winprocess));
      temp[0]:=#0;
      getwindowtext(winhandle,temp,100);
      temp[100]:=#0;
      wintitle:=temp;

      if length(wintitle)>0 then
      begin
        getmem(ProcessListInfo,sizeof(TProcessListInfo));
        ProcessListInfo.processID:=winprocess;
        ProcessListInfo.processIcon:=SendMessage(winhandle,WM_GETICON,ICON_SMALL,0);
        if ProcessListInfo.processIcon=0 then
          ProcessListInfo.processIcon:=SendMessage(winhandle,WM_GETICON,ICON_BIG,0);

        x.AddObject(IntTohex(winprocess,8)+'-'+wintitle,TObject(ProcessListInfo));
      end;

      previouswinhandle:=winhandle;
      winhandle:=getwindow(winhandle,GW_HWNDNEXT);

      if winhandle=previouswinhandle then break;
      
      inc(i);
    end;

    x.Sort;
    processlist.Items.Assign(x);
  finally
    freemem(temp);
  end;
end;

function GetCEdir:string;
begin
  CheatEngineDir:=ExtractFilePath(application.ExeName);
  result:=CheatEngineDir;
end;

function GetWinDir:string;
var x: pchar;
begin
  getmem(x,200);
  if GetWindowsDirectory(x,200)>0 then
  begin
    result:=x;
    WindowsDir:=x;
  end;
  freemem(x);
end;

Procedure Shutdown;
//This will erase the temporary files and close the processhandle (In case it doesnt happen automatically)
begin
  deletefile(CheatEngineDir+'Memory.TMP');
  deletefile(CheatEngineDir+'Addresses.TMP');
  deletefile(CheatEngineDir+'Memory.UNDO');
  deletefile(CheatEngineDir+'Addresses.UNDO');
  freemem(memory);
 // Closehandle(processhandle);

end;

procedure ConvertStringToBytes(scanvalue:string; hex:boolean;var bytes: TBytes);
{
Converts a given string into a array of TBytes.
TBytes are not pure bytes, they can hold -1, which indicates a wildcard
}
var i,j,k: integer;
    helpstr:string;
begin
  setlength(bytes,0);
  if length(scanvalue)=0 then exit;

  while scanvalue[length(scanvalue)]=' ' do
    scanvalue:=copy(scanvalue,1,length(scanvalue)-1);

  if (pos('-',scanvalue)>0) or (pos(' ',scanvalue)>0) then
  begin
    //syntax is xx-xx-xx or xx xx xx
    j:=1;
    k:=0;
    scanvalue:=scanvalue+' ';

    for i:=1 to length(scanvalue) do
    begin
      if (scanvalue[i]=' ') or (scanvalue[i]='-') then
      begin
        helpstr:=copy(scanvalue,j,i-j);
        j:=i+1;
        setlength(bytes,k+1);
        try
          if hex then bytes[k]:=strtoint('$'+helpstr)
                 else bytes[k]:=strtoint(helpstr);
        except
          bytes[k]:=-1;
          //if it is not a '-' or ' ' or a valid value then I assume it is a wildcard.(I know, retarded)
        end;
        inc(k);
      end;
    end;
  end else
  begin
    //syntax is xxxxxx
    k:=0;
    j:=1;
    for i:=1 to length(scanvalue) do
    begin
      if (i mod 2)=0 then
      begin
        helpstr:=copy(scanvalue,j,i-j+1);
        j:=i+1;
        setlength(bytes,k+1);
        try
          bytes[k]:=strtoint('$'+helpstr);
        except
          bytes[k]:=-1;
        end;
        inc(k);
      end;
    end;
  end;
end;



procedure rewritedata(processhandle: thandle; address:dword; buffer: pointer; size:dword);
var written: dword;
    original,a: dword;
begin
//make writable, write, restore, flush
  VirtualProtectEx(processhandle,  pointer(address),size,PAGE_EXECUTE_READWRITE,original);
  writeprocessmemory(processhandle,pointer(address),buffer,size,written);
  VirtualProtectEx(processhandle,pointer(address),size,original,a);
end;

procedure rewritecode(processhandle: thandle; address:dword; buffer: pointer; size:dword);
begin
  rewritedata(processhandle,address,buffer,size);
  FlushInstructionCache(processhandle,pointer(address),size);
end;


function GetCPUCount: integer;
{
this function will return how many active cpu cores there are at your disposal
}
var cpucount: integer;
    PA,SA: dword;
begin
  //get the cpu and system affinity mask, only processmask is used
  GetProcessAffinityMask(getcurrentprocess,PA,SA);

  cpucount:=0; 
  while pa>0 do
  begin
    if (pa mod 2)=1 then inc(cpucount);
    pa:=pa div 2;
  end;

  result:=cpucount;

  if result=0 then result:=1;
end;

function LoadFormPosition(form: Tform; var x: array of integer):boolean;
var reg: tregistry;
    s: string;
    buf: array of integer;
    buf2: array [0..100] of byte;
    i: integer;
    z: integer;
begin
  result:=false;
  reg:=tregistry.create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('\Software\Cheat Engine',false) then
    begin
      if reg.valueexists('Save window positions') then
        if reg.readbool('Save window positions') = false then exit;
    end;

    if Reg.OpenKey('\Software\Cheat Engine\Window Positions',false) then
    begin
      s:=form.Name;
      s:=s+' Position';

      if reg.ValueExists(s) then
      begin

        setlength(buf,4+length(x)); //for some reason it checks if

        z:=reg.ReadBinaryData(s,buf[0],length(buf)*sizeof(integer));

        form.position:=poDesigned;
        form.top:=buf[0];
        form.Left:=buf[1];
        form.width:=buf[2];
        form.height:=buf[3];

        for i:=0 to length(x)-1 do
          x[i]:=buf[4+i];

        setlength(buf,0);

        result:=true;
      end;
    end;
  finally
    reg.free;
  end;
end;

procedure SaveFormPosition(form: Tform; extra: array of integer);
{
This function will save the position and the optional data in extra to an array element in the registry
}
var reg: tregistry;
    buf: tmemorystream;
    temp: integer;
    i: integer;
    s: string;
begin
  //save window pos
  reg:=tregistry.create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    //make sure the option to save is enabled
    if Reg.OpenKey('\Software\Cheat Engine',false) then
    begin
      if reg.valueexists('Save window positions') then
        if reg.readbool('Save window positions') = false then exit;
    end;

    
    if Reg.OpenKey('\Software\Cheat Engine\Window Positions',true) then
    begin
      //registry is open, gather data
      buf:=tmemorystream.Create;
      try
        temp:=form.top;
        buf.Write(temp,sizeof(temp));

        temp:=form.left;
        buf.Write(temp,sizeof(temp));

        temp:=form.width;
        buf.Write(temp,sizeof(temp));

        temp:=form.height;
        buf.Write(temp,sizeof(temp));


        //save extra data
        for i:=0 to length(extra)-1 do
          buf.Write(extra[i],sizeof(extra[i]));

        //and now save buf to the registry
        s:=form.Name;
        s:=s+' Position';

        reg.WriteBinaryData(s,buf.Memory^,buf.Size);
      finally
        buf.free;
      end;
    end;
  finally
    reg.free;
  end;
end;

function GetRelativeFilePath(filename: string):string;
begin
  result:=filename;
  if pos(uppercase(CheatEngineDir),uppercase(filename))=1 then
    result:='.\'+copy(filename,length(CheatEnginedir)+1,length(filename));
end;

function ConvertHexStrToRealStr(const s: string): string;
{
Converts a string meant to be a hexadeimcal string to the real way delphi reads
it
e.g:
123 > $123
-123 > -$123
+123 > +$123
#123 > 123
+#123 > +123
}
var ishex: string;
    start: integer;
    i,j,k: integer;

    bytes: string;
begin
  if s='' then
  begin
    result:=s;
    exit;
  end;
  start:=1;

  ishex:='$';
  for i:=start to length(s) do
    case s[i] of
      '''' , '"' :
      begin
        //char
        if (i+2)<=length(s) then
        begin
          bytes:='';
          for j:=i+2 to length(s) do
            if s[j] in ['''','"'] then
            begin
              bytes:=copy(s,i+1,j-(i+1));

              result:='$';
              for k:=length(bytes) downto 1 do
                result:=result+inttohex(byte(bytes[k]),2);

              //result := '$'+inttohex(byte(s[i+1]),2);
              exit; //this is it, no further process required, or appreciated...

            end;



        end;
      end;

      '#' :
      begin
        ishex:='';
        start:=2;
        break;
      end;
    end;


  if s[1]='-' then
  begin
    result:='-'+ishex+copy(s,start+1,length(s))
  end
  else
  if s[1]='+' then
  begin
    result:='+'+ishex+copy(s,start+1,length(s));
  end
  else result:=ishex+copy(s,start,length(s));
end;

function HexStrToInt(const S: string): Integer;
begin
  result:=StrToint(ConvertHexStrToRealStr(s));
end;

function HexStrToInt64(const S: string): Int64;
begin
  result:=StrToint64(ConvertHexStrToRealStr(s));
end;

function isjumporcall(address: dword; var addresstojumpto: dword): boolean;
var buf: array [0..31] of byte;
    actualread: dword;
    i,j: integer;
    st: string;
    offset: dword;
begin
{$ifndef standalonetrainer}
  result:=false;

  if readprocessmemory(processhandle,pointer(address),@buf[0],32,actualread) then
  begin
    if buf[0] in [$0f,$70..$7f,$e3,$e8,$e9,$eb,$ff] then //possible
    begin
      case buf[0] of
        $0f:
        begin
          if (not (buf[1] in [$80..$8f])) then exit; //not one of them
          result:=true;
          addresstojumpto:=address+plongint(@buf[2])^+6;
        end;

        $70..$7f,$e3,$eb:  //(un)conditional jump (1 byte)
        begin
          result:=true;
          addresstojumpto:=address+pshortint(@buf[1])^+2;
        end;

        $e8,$e9: //jump or call unconditional (4 byte)
        begin
          result:=true;
          addresstojumpto:=address+plongint(@buf[1])^+5;
        end;

        $ff: //disassemble to see what it is
        begin
          st:=disassemble(address);
          st:=copy(st,pos('-',st)+2,length(st));
          st:=copy(st,pos('-',st)+2,length(st));

          i:=pos('jmp',st);
          j:=pos('call',st);
          if (i=1) or (j=1) then
          begin
            //now determine where it jumps to
            i:=pos('[',st);
            if i>0 then
            begin
              st:=copy(st,i,pos(']',st)-i+1);

              try
                addresstojumpto:=symhandler.getAddressFromName(st); //the pointer interpreter code can do this
                result:=true;
              except

              end;
            end;

          end;
        end;


      end;
    end;

  end;
{$endif}

end;


function readAndParseAddress(address: dword; variableType: TVariableType): string;
var buf: array [0..7] of byte;
    x: dword;
    check: boolean;
begin
  result:='???';
  case variableType of
    vtByte:
    begin
      if ReadProcessMemory(processhandle,pointer(address),@buf[0],1,x) then
        result:=inttostr(buf[0]);
    end;

    vtWord:
    begin
      if ReadProcessMemory(processhandle,pointer(address),@buf[0],2,x) then
        result:=inttostr(pword(@buf[0])^);
    end;

    vtDWord:
    begin
      if ReadProcessMemory(processhandle,pointer(address),@buf[0],4,x) then
        result:=inttostr(pdword(@buf[0])^);
    end;

    vtSingle:
    begin
      if ReadProcessMemory(processhandle,pointer(address),@buf[0],4,x) then
        result:=floattostr(psingle(@buf[0])^);
    end;

    vtDouble:
    begin
      if ReadProcessMemory(processhandle,pointer(address),@buf[0],8,x) then
        result:=floattostr(pdouble(@buf[0])^);
    end;
  end;
end;

function allocationtypetostring(alloctype: dword): string;
begin
  result:='';
  if (alloctype and MEM_COMMIT) > 0 then result:='MEM_COMMIT+';
  if (alloctype and MEM_RESERVE) > 0 then result:=result+'MEM_RESERVE+';
  if (alloctype and MEM_RESET) > 0 then result:=result+'MEM_RESET+';
  if (alloctype and MEM_TOP_DOWN)	> 0 then result:=result+'MEM_TOP_DOWN+';
  if (alloctype and $400000) > 0 then result:=result+'MEM_PHYSICAL+';
  if (alloctype and $20000000) > 0 then result:=result+'MEM_LARGE_PAGES+';

  if length(result)>0 then
    result:=Copy(result,1,length(result)-1)+'('+inttohex(alloctype,1)+')';
end;

function allocationprotecttostring(protect: dword): string;
begin
  result:='';
  if (protect and PAGE_EXECUTE) > 0 then result:='PAGE_EXECUTE+';
  if (protect and PAGE_EXECUTE_READ) > 0 then result:=result+'PAGE_EXECUTE_READ+';
  if (protect and PAGE_EXECUTE_READWRITE) > 0 then result:=result+'PAGE_EXECUTE_READWRITE+';
  if (protect and PAGE_EXECUTE_WRITECOPY) > 0 then result:=result+'PAGE_EXECUTE_WRITECOPY+';
  if (protect and PAGE_NOACCESS) > 0 then result:=result+'PAGE_NOACCESS+';
  if (protect and PAGE_READONLY) > 0 then result:=result+'PAGE_READONLY+';
  if (protect and PAGE_READWRITE) > 0 then result:=result+'PAGE_READWRITE+';
  if (protect and PAGE_WRITECOPY) > 0 then result:=result+'PAGE_WRITECOPY+';
  if (protect and PAGE_GUARD) > 0 then result:=result+'PAGE_GUARD+';
  if (protect and PAGE_NOCACHE) > 0 then result:=result+'PAGE_NOCACHE+';
  if (protect and $400) > 0 then result:=result+'PAGE_WRITECOMBINE+';

  if length(result)>0 then
    result:=Copy(result,1,length(result)-1)+'('+inttohex(protect,1)+')';
end;

function freetypetostring(freetype: dword):string;
begin
  result:='';
  
  if (freetype and MEM_DECOMMIT) > 0 then result:='MEM_DECOMMIT+';
  if (freetype and MEM_RELEASE) > 0 then result:='MEM_RELEASE+';  
  result:=Copy(result,1,length(result)-1)+'('+inttohex(freetype,1)+')';
end;


initialization
  GetWindir;
  keysfilemapping:=0;
  keys:=nil;

  setlength(windowlist,0);
  setlength(donthidelist,0);
  allwindowsareback:=true;
  stealthhook:=0;
  iswin2kplus:=GetSystemType>=5;

  flushthread:=TSaveDataThread.Create(false); //used for scanning, starts idled because the event isn't triggered
  prefetchthread:=TPrefetchDataThread.create(false);

  processhandler:=TProcessHandler.create;

finalization
  if flushthread<>nil then
  begin
    flushthread.Terminate;
    flushthread.dataavailable.SetEvent;
    flushthread.WaitFor;
    flushthread.free;
  end;

end.




