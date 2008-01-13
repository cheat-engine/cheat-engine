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
math,syncobjs;



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

procedure quicksortmemoryregions(lo,hi: integer);
//function NextScan(var Results: TListBox; value: dword; FValue: Double; ScanWay, ValType,max: Integer; ProgressBar: TProgressbar): Integer;
procedure rewritecode(processhandle: thandle; address:dword; buffer: pointer; size:dword);
procedure rewritedata(processhandle: thandle; address:dword; buffer: pointer; size:dword);

function FindPointer(start,stop:dword; AddressToFind:Dword; progressbar: Tprogressbar; offset: Tbytes):int64;
function NextScan2(scantext,scantext2:string; ScanWay, ValType: Integer; roundingtype:tfloatscan; option:boolean; ProgressBar: TProgressbar;fastscan:boolean;unicode,percentage: boolean): dword;
procedure GetProcessListSmall(ProcessList: TListBox);
procedure GetProcessList(ProcessList: TListBox); overload;
procedure GetProcessList(ProcessList: TStrings); overload;
procedure GetWindowList(ProcessList: TListBox{; var ArrIcons: TBytes});
function AvailMem:dword;
function isreadable(address:dword):boolean;


function GetMemoryRanges2(Start,Stop: Dword; readonly: boolean; Progressbar: TProgressBar;vartype:integer;fastscan:boolean):int64;
function GetMemoryRangesAndScanValue2(var firstresult: dword; Start,Stop: Dword; readonly,findonlyone: boolean; ScanType,VarType: Integer; scanvalue,scanvalue2: string;roundingtype:tfloatscan ; extra: boolean;progressbar: Tprogressbar;fastscan:boolean;unicode: boolean): Int64; //integer;
procedure RemoveAddress(address: Dword;bit: Byte; vartype: Integer);

function GetCEdir:string;
procedure Open_Process;
Procedure Shutdown;
function KeyToStr(key:word):string;

procedure Freememory;
procedure closefiles;
function undolastscan(valtype: integer;hexadecimal:boolean): integer;

procedure ConvertStringToBytes(scanvalue:string; hex:boolean;var bytes: TBytes);
function getbit(bitnr: integer; bt: Byte):integer;
procedure setbit(bitnr: integer; var bt: Byte;state:integer); overload;
procedure setbit(bitnr: integer; var bt: dword;state:integer); overload;

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
Procedure InjectDll(dllname: string; functiontocall: string);
Function GetRelativeFilePath(filename: string):string;

function GetCPUCount: integer;

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

type Tuint64Array=array[0..0] of uint64;
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
var
  old8087CW: word;  //you never know...
  ProcessSelected: Boolean;
  ProcessID: Dword;
  ProcessHandle: Thandle;

  Skip_PAGE_NOCACHE: boolean;
  Scan_MEM_PRIVATE: boolean;
  Scan_MEM_IMAGE: boolean;
  Scan_MEM_MAPPED: boolean;

  CheatEngineDir: String;

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

  

implementation

{$ifdef net}
uses disassembler,debugger;
{$endif}

{$ifndef net}

{$ifndef standalonetrainer}
uses disassembler,debugger,symbolhandler,frmProcessWatcherUnit,debugger2;
{$else}
uses symbolhandler;
{$endif}

{$endif}



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

  if debuggerthread2<>nil then
  begin
    debuggerthread2.Terminate;
    debuggerthread2.WaitFor;
  end;

  {$endif}

end;
{$endif}


Procedure InjectDll(dllname: string; functiontocall: string);
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


    //safetycode, test if the dll was actually loaded and scip if not
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
        CheckSynchronize; //handle sychronize while it's waiting 
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

function getbit(bitnr: integer; bt: Byte):integer;
begin
  if (trunc(power(2,bitnr)) and bt)>0 then result:=1 else result:=0;
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

procedure setbit(bitnr: integer; var bt: dword;state:integer); overload;
{
 pre: bitnr=bit between 0 and 7
         bt=pointer to the byte
 post: bt has the bit set specified in state
 result: bt has a bit set or unset
}
begin
  if state=1 then
    bt:=bt or trunc(power(2,bitnr))  //set that bit to 1
  else
    bt:=bt and ($ffffffff xor trunc(power(2,bitnr))); //set the bit to 0
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

function undolastscan(valtype: integer;hexadecimal:boolean): integer;
begin
  deletefile(CheatEngineDir+'Memory.tmp');
  deletefile(CheatEngineDir+'Addresses.tmp');

  renamefile(CheatEngineDir+'Memory.UNDO',CheatEngineDir+'Memory.tmp');
  renamefile(CheatEngineDir+'Addresses.UNDO',CheatEngineDir+'Addresses.tmp');

  assignfile(addressfile,CheatEngineDir+'Addresses.tmp');
  assignfile(memoryfile,CheatEngineDir+'Memory.tmp');
end;

procedure closefiles;
begin
  try closefile(addressfile); except end;
  try closefile(memoryfile); except end;
  try closefile(newAddressfile); except end;
  try closefile(newmemoryfile); except end;
end;

procedure Freememory;
var i: integer;
    fm: tfreememorythread;
begin
//  freeingmemory:=true;

  fm:=tfreememorythread.Create(true);
  fm.FreeOnTerminate:=true;

  fm.Bitscan:=pointer(bitscan);
  bitscan:=nil;

  fm.tempbits:=pointer(tempbits);
  tempbits:=nil;

  fm.FoundAddress:=pointer(foundaddress);
  foundaddress:=nil;
  foundaddressswitch:=nil;

  fm.FoundValue1:=pointer(foundvalue1);
  fm.FoundValue1switch:=pointer(foundvalue1switch);
  foundvalue1:=nil;
  foundvalue1switch:=nil;

  fm.FoundValue2:=pointer(foundvalue2);
  fm.FoundValue2switch:=pointer(foundvalue2switch);
  FoundValue2:=nil;
  FoundValue2switch:=nil;

  fm.FoundValue3:=pointer(foundvalue3);
  fm.foundvalue3switch:=pointer(foundvalue3switch);
  FoundValue3:=nil;
  foundvalue3switch:=nil;

  fm.FoundValue4:=pointer(foundvalue4);
  fm.foundvalue4switch:=pointer(foundvalue4switch);
  FoundValue4:=nil;
  foundvalue4switch:=nil;

  fm.FoundValue5:=pointer(foundvalue5);
  fm.foundvalue5switch:=pointer(foundvalue5switch);
  FoundValue5:=nil;
  FoundValue5switch:=nil;

  fm.foundValue6:=pointer(foundvalue6);
  fm.foundvalue6switch:=pointer(foundvalue6switch);
  foundValue6:=nil;
  foundvalue6switch:=nil;

  fm.FoundValue7:=pointer(foundvalue7);
  fm.foundvalue7switch:=pointer(foundvalue7switch);
  FoundValue7:=nil;
  foundvalue7switch:=nil;

  fm.FoundValue8:=pointer(foundvalue8);
  fm.foundvalue8switch:=pointer(foundvalue8switch);
  FoundValue8:=nil;
  foundvalue8switch:=nil;  

  fm.foundaddressB:=pointer(foundaddressb);
  foundaddressB:=nil;
  foundaddressbswitch:=nil;


  fm.previousmemory:=pointer(previousmemory);
  previousmemory:=nil;

  fm.SearchAddress:=pointer(searchaddress);
  SearchAddress:=nil;

  fm.SearchAddressB:=pointer(searchaddressb);
  SearchAddressB:=nil;

  fm.previousmemory1:=pointer(previousmemory1);
  fm.previousmemory1switch:=pointer(previousmemory1switch);
  previousmemory1:=nil;

  fm.previousmemory2:=pointer(previousmemory2);
  fm.previousmemory2switch:=pointer(previousmemory2switch);
  previousmemory2:=nil;

  fm.previousmemory3:=pointer(previousmemory3);
  fm.previousmemory3switch:=pointer(previousmemory3switch);
  previousmemory3:=nil;

  fm.previousmemory4:=pointer(previousmemory4);
  fm.previousmemory4switch:=pointer(previousmemory4switch);
  previousmemory4:=nil;

  fm.previousmemory5:=pointer(previousmemory5);
  fm.previousmemory5switch:=pointer(previousmemory5switch);
  previousmemory5:=nil;

  fm.previousmemory6:=pointer(previousmemory6);
  fm.previousmemory6switch:=pointer(previousmemory6switch);
  previousmemory6:=nil;

  fm.PreviousMemory7:=pointer(previousmemory7);
  fm.previousmemory7switch:=pointer(previousmemory7switch);
  PreviousMemory7:=nil;

  fm.PreviousMemory8:=pointer(previousmemory8);
  fm.previousmemory8switch:=pointer(previousmemory8switch);
  PreviousMemory8:=nil;

  fm.Memory:=pointer(memory);
  Memory:=nil;

  fm.memory2:=pointer(memory2);
  memory2:=nil;

  fm.bytes:=pointer(bytes);
  bytes:=nil;

  fm.bytearray:=pointer(bytearray);
  bytearray:=nil;

  fm.resume;
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
  Processhandle:=NewKernelHandler.OpenProcess(PROCESS_ALL_ACCESS,false,ProcessID);
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
function FindPointer(start,stop:dword; AddressToFind:Dword; progressbar: Tprogressbar; offset: Tbytes):int64;
var mbi : _MEMORY_BASIC_INFORMATION;
    address: Dword;
    size:       dword;

    found: dword;

    Decim:       dword;
    Decimhelp:   Integer;



    ByteValue: Byte;
    WordValue: Word;
    DWordValue: Dword;
    SingleValue: Single;
    doubleValue: double;
    Int64Value: Int64;

    helpword:  word;
    helpdword:  dword;
    HelpInt64: Int64;
    helpsingle,helpsingle2: Single;
    helpdouble,helpdouble2: double;


    i,k,l: integer;
    j: dword;

    actualread: dword;
    actualwrite: dword;


    //save variables
    dataType:  String[6];  //REGION or NORMAL  (Always region in this procedure)


    //string search part

  //  buf: array [1..512] of byte;
      CharToFind: Dword;
   //   region: TMemoryregion;
      memoryposition: Dword;
      position: Dword;
      searching: Boolean;

      scanlength: Dword;

      TotalToRead: Dword;
      check:    boolean;

//x:integer
      MPoint: ^Byte;

      ByteP: pbyte;
      bytep2: pbyte;
      WordP: ^Word;
      DwordP: ^Dword;
      Int64P: ^Int64;

      SingleP: ^Single;
      DoubleP: ^double;


      number: dword;
      maxmem: dword;

      FoundIsFilled: Boolean;

      //arrayofbyte vars
      nrofbytes: dword;
      bytes: TBytes;  //-1=wildcard

      BitToFind: integer;

      traceback: boolean;

      resulthelper: tfilestream;
      tempstring: pchar;
      readonly: boolean;
      tempdword: dword;

      P: dword;
      currentcheck: integer;
begin

  readonly:=true;
  advanceD:=false;
  nrofbits:=999;

  Int64Value:=0;
  FoundIsFilled:=false;

 // setlength(memory,splitvalue+1);
  found:=0;


  address:=start;
  try
//-------------------------------------------------
  while (Virtualqueryex(processhandle,pointer(address),mbi,sizeof(mbi))<>0) and (address<stop) and ((address+mbi.RegionSize)>address) do
  begin
    if (mbi.State=mem_commit) and ((mbi.Protect and page_guard)=0) and (mbi.protect<>PAGE_NOACCESS) then  //look if it is commited
    begin
      if not readonly then  //if the settings are set to not read read-only memory then
      begin
        if (((mbi.AllocationProtect) and (page_readonly or page_execute_read))=0) and
           (((mbi.Protect) and (page_readonly or PAGE_EXECUTE_READ))=0) then //look if the memory is not read-only  , if 0 it means it's not read-only
        begin
          if Skip_PAGE_NOCACHE then
            if (mbi.AllocationProtect and PAGE_NOCACHE)=PAGE_NOCACHE then
            begin
              address:=dword(mbi.BaseAddress)+mbi.RegionSize;
              continue;
            end;

          setlength(memoryregion,memoryregions+1);
          memoryregion[memoryregions].BaseAddress:=dword(mbi.baseaddress);  //remember if it's not read only
          memoryregion[memoryregions].MemorySize:=mbi.RegionSize;
          inc(memoryregions);
        end;
      end else  //if the settings are to also read the read only then:
      begin
        if Skip_PAGE_NOCACHE then
          if (mbi.AllocationProtect and PAGE_NOCACHE)=PAGE_NOCACHE then
          begin
            address:=dword(mbi.BaseAddress)+mbi.RegionSize;
            continue;
          end;

        setlength(memoryregion,memoryregions+1);
        memoryregion[memoryregions].BaseAddress:=dword(mbi.baseaddress);  //just remember this location
        memoryregion[memoryregions].MemorySize:=mbi.RegionSize;
        inc(memoryregions);
      end;
    end;


    address:=dword(mbi.baseaddress)+mbi.RegionSize;
  end;

  
  if memoryregions=0 then
  begin
    closefile(memoryfile);
    closefile(addressfile);
    raise Exception.create('No readable memory found in the region you specified!');
  end;

  //lets search really at the start of the location the user specified
  if (memoryregion[0].BaseAddress<start) and (memoryregion[0].MemorySize-(start-memoryregion[0].BaseAddress)>0) then
  begin
    memoryregion[0].MemorySize:=memoryregion[0].MemorySize-(start-memoryregion[0].BaseAddress);
    memoryregion[0].BaseAddress:=start;
  end;

  //also the right end
  if (memoryregion[memoryregions-1].BaseAddress+memoryregion[memoryregions-1].MemorySize)>stop then
    dec(memoryregion[memoryregions-1].MemorySize,(memoryregion[memoryregions-1].BaseAddress+memoryregion[memoryregions-1].MemorySize)-stop-1);


  j:=0;
  address:=memoryregion[0].BaseAddress;
  size:=memoryregion[0].MemorySize;

  for i:=1 to memoryregions-1 do
  begin
    if memoryregion[i].BaseAddress=address+size then
    begin
      inc(size,memoryregion[i].MemorySize);
    end
    else
    begin
      memoryregion[j].BaseAddress:=address;
      memoryregion[j].MemorySize:=size;

      address:=memoryregion[i].BaseAddress;
      size:=memoryregion[i].MemorySize;
      inc(j);
    end;
  end;

  memoryregion[j].BaseAddress:=address;
  memoryregion[j].MemorySize:=size;

  memoryregions:=j+1;
  setlength(memoryregion,memoryregions);


  //re-added due to complaints about speed:
  //split up into smaller chunks

  if buffersize>0 then
  begin
    i:=0;
    while i<=memoryregions-1 do
    begin
      if memoryregion[i].MemorySize>dword(buffersize) then
      begin
        inc(memoryregions);
        setlength(memoryregion,memoryregions);

        //copy the next element to the back, and split up the current one
        //(unless this is the item at the back, and not needed)
        if i<memoryregions-2 then
        begin
          //i isnt the last element of the array so do a semi-shift
          memoryregion[memoryregions-1].BaseAddress:=memoryregion[i+1].baseaddress;
          memoryregion[memoryregions-1].MemorySize:=memoryregion[i+1].MemorySize;
        end;

        memoryregion[i+1].IsChild:=true;
        memoryregion[i+1].BaseAddress:=memoryregion[i].BaseAddress+buffersize;
        memoryregion[i+1].MemorySize:=memoryregion[i].MemorySize-buffersize;

        memoryregion[i].MemorySize:=buffersize;
      end;
      inc(i);
    end;
    dec(memoryregions);
  end else memoryregions:=j;

  TotalToRead:=0;
  maxmem:=0;
  For i:=0 to Memoryregions do
  begin
    inc(TotalToRead,Memoryregion[i].MemorySize);
    if maxmem<Memoryregion[i].MemorySize then maxmem:=Memoryregion[i].MemorySize;
  end;

  totalbytes:=totaltoread;

  progressbar.max:=totaltoread;;
 // setlength(memory,maxmem+1);  //+1 just because I want to be sure, you never know in delphi)
  if memory<>nil then
  begin
    freemem(memory);
    memory:=nil;
  end;

  getmem(memory,maxmem);
  number:=buffersize;

  setlength(foundaddress,number);

  except
    raise exception.Create('Something went wrong');
  end;

  assignfile(Addressfile,CheatEngineDir+'Addresses.TMP');
  datatype:='NORMAL';
  rewrite(Addressfile,1);
  blockwrite(Addressfile,datatype,sizeof(datatype));

  for i:=0 to memoryregions do
  begin
    dwordp:=pointer(memory);
    readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);

    if actualread<4 then
    begin
//      raise exception.create(IntToStR(getlasterror));
      inc(currentbyte,Memoryregion[i].MemorySize);
      progressbar.Position:=currentbyte;
      continue;
    end;

    currentcheck:=0;
    for j:=0 to actualread-1 do
    begin
      p:=Memoryregion[i].BaseAddress+j; //dwordp^;

      while currentcheck<>length(offset) do
      begin
        readprocessmemory(processhandle,pointer(p),@p,4,nrofbytes);
        if nrofbytes<>4 then
        begin
          currentcheck:=0;
          break
        end
        else
        begin
          inc(p,offset[currentcheck]);
          inc(currentcheck);
        end;
      end;

      if currentcheck=length(offset) then
      begin
        if p=addresstofind then
        begin
          tempdword:=Memoryregion[i].BaseAddress+j;
          blockwrite(addressfile,tempdword,sizeof(dword),tempdword);
        end;

        currentcheck:=0;
      end;

      asm
        inc [dwordp]
      end;
      inc(currentbyte);

    end;
    if actualread<>Memoryregion[i].MemorySize then inc(currentbyte,Memoryregion[i].MemorySize-actualread);
    progressbar.Position:=currentbyte;
  end;


  result:=filesize(addressfile) div 4;

  closefile(addressfile);
  freememory;
end;



function GetMemoryRangesAndScanValue2(var firstresult: dword; Start,Stop: Dword; readonly,findonlyone: boolean; ScanType,VarType: Integer; scanvalue,scanvalue2: string;roundingtype:tfloatscan ;extra: boolean;progressbar: Tprogressbar;fastscan:boolean;unicode: boolean): Int64; //integer;
var mbi : _MEMORY_BASIC_INFORMATION;
    address: Dword;
    size:       dword;

    found: dword;

    Decim:       dword;
    Decimhelp:   Integer;



    ByteValue,ByteValue2: Byte;
    WordValue,WordValue2: Word;
    DWordValue,DWordValue2: Dword;
    SingleValue,SingleValue2: Single;
    doubleValue,doublevalue2: double;
    Int64Value,int64value2: Int64;

    helpword:  word;
    helpdword:  dword;
    HelpInt64: Int64;
    helpsingle,helpsingle2,helpsingle3: Single;
    helpdouble,helpdouble2,helpdouble3: double;


    i,k,l: integer;
    j: dword;

    actualread: dword;
    actualwrite: dword;


    //save variables
    dataType:  String[6];  //REGION or NORMAL  (Always region in this procedure)


    //string search part

  //  buf: array [1..512] of byte;
      CharToFind: Dword;
      unicode2: boolean;
   //   region: TMemoryregion;
      memoryposition: Dword;
      position: Dword;
      searching: Boolean;

      scanlength: Dword;

      TotalToRead: Dword;
      check:    boolean;

//x:integer
      MPoint: ^Byte;

      ByteP: pbyte;
      bytep2: pbyte;
      WordP: ^Word;
      DwordP: ^Dword;
      Int64P: ^Int64;

      SingleP: ^Single;
      DoubleP: ^double;


      number: dword;
      maxmem: dword;

      FoundIsFilled: Boolean;


      //arrayofbyte vars
      nrofbytes: dword;
      bytes: TBytes;  //-1=wildcard

      BitToFind: integer;

      traceback: boolean;

      resulthelper: tfilestream;
      tempstring: pchar;
      widetempstring: pwidechar;
begin
  advanceD:=false;
  nrofbits:=999;

  Int64Value:=0;
  FoundIsFilled:=false;

 // setlength(memory,splitvalue+1);
  found:=0;


  if scanvalue='' then scanvalue:='0';
  if scanvalue2='' then scanvalue2:='0';

  if scantype in [Exact_value,Increased_value_by,decreased_value_by,SmallerThan,BiggerThan,valuebetween] then
  begin


    if vartype=0 then
    begin
      if extra then val('$'+scanvalue,bytevalue,i)
               else val(scanvalue,bytevalue,i);

      if i>0 then raise Exception.Create(scanvalue+' is not a valid byte notation');
    end;

    if vartype=1 then
    begin
      if extra then val('$'+scanvalue,wordvalue,i)
               else val(scanvalue,wordvalue,i);
      if i>0 then raise Exception.Create(scanvalue+' is not an valid 2-byte notation');
    end;

    if vartype=2 then
    begin
      if extra then val('$'+scanvalue,dwordvalue,i)
               else val(scanvalue,dwordvalue,i);
      if i>0 then raise Exception.Create(scanvalue+' is not an valid 4-byte notation');
    end;

    if vartype=3 then //doesnt have the hexadecimal option
    begin
      val(scanvalue,singlevalue,i);
      if i>0 then
        if scanvalue[i]=',' then scanvalue[i]:='.';

      val(scanvalue,singlevalue,i);
      if i>0 then raise Exception.Create(scanvalue+' is not a valid floating point notation');
    end;

    if vartype=4 then //same as 3
    begin
      val(scanvalue,doublevalue,i);
      if i>0 then
        if scanvalue[i]=',' then scanvalue[i]:='.';
      val(scanvalue,doublevalue,i);

      if i>0 then raise Exception.create(scanvalue+' is not a valid floating point notation');
    end;

    if vartype=5 then
    begin
      if pos('-',scanvalue)>0 then raise exception.create(scanvalue+' is not a valid notation!');
      if not extra then
      begin
        setlength(bitscan,length(scanvalue));
        j:=0;
        for i:=length(scanvalue) downto 1 do
        begin
          if scanvalue[i]='0' then bitscan[length(scanvalue)-i]:=0
          else
          if scanvalue[i]='1' then bitscan[length(scanvalue)-i]:=1
          else
          if scanvalue[i]='?' then bitscan[length(scanvalue)-i]:=2
          else
          if scanvalue[i]='*' then bitscan[length(scanvalue)-i]:=2
          else
          begin
            freememory;
            raise Exception.create(scanvalue+' is not a valid binary notation');
          end;
          inc(j);
        end;
      end
      else
      begin
        try
          dwordvalue:=StrToInt(scanvalue);
          i:=0;

          if dwordvalue=0 then
          begin
            setlength(bitscan,1);
            bitscan[0]:=0;
          end
          else
          while dwordvalue>0 do
          begin
            setlength(bitscan,i+1);

            if (dwordvalue mod 2)>0 then bitscan[i]:=1
                                    else bitscan[i]:=0;

            inc(i);
            dwordvalue:=dwordvalue div 2;
          end;
        except
          raise Exception.create(scanvalue+' is not a valid notation');
        end;
      end;

      nrofbits:=length(bitscan);
      if nrofbits=0 then raise exception.Create(scanvalue+' did not result in a string of bits');
    end;

    if vartype=6 then
    begin
      if extra then
        val('$'+scanvalue,int64value,i)
      else
        val(scanvalue,Int64value,i);
      if i>0 then raise Exception.Create(scanvalue+' is not an valid 8-byte notation');
    end;

    if scantype=valuebetween then
    begin
      //also do it for the 2nd value
      if vartype=0 then
      begin
        if extra then val('$'+scanvalue2,bytevalue2,i)
                 else val(scanvalue2,bytevalue2,i);

        if i>0 then raise Exception.Create(scanvalue2+' is not a valid byte notation');
      end;

      if vartype=1 then
      begin
        if extra then val('$'+scanvalue2,wordvalue2,i)
                 else val(scanvalue2,wordvalue2,i);
        if i>0 then raise Exception.Create(scanvalue2+' is not an valid 2-byte notation');
      end;

      if vartype=2 then
      begin
        if extra then val('$'+scanvalue2,dwordvalue2,i)
                 else val(scanvalue2,dwordvalue2,i);
        if i>0 then raise Exception.Create(scanvalue2+' is not an valid 4-byte notation');
      end;

      if vartype=3 then //doesnt have the hexadecimal option
      begin
        val(scanvalue2,singlevalue2,i);
        if i>0 then
          if scanvalue2[i]=',' then scanvalue2[i]:='.';

        val(scanvalue2,singlevalue2,i);
        if i>0 then raise Exception.Create(scanvalue2+' is not a valid floating point notation');
      end;

      if vartype=4 then //same as 3
      begin
        val(scanvalue2,doublevalue2,i);
        if i>0 then
          if scanvalue2[i]=',' then scanvalue2[i]:='.';
        val(scanvalue2,doublevalue2,i);

        if i>0 then raise Exception.create(scanvalue2+' is not a valid floating point notation');
      end;

      if vartype=6 then
      begin
        if extra then
          val('$'+scanvalue2,int64value2,i)
        else
          val(scanvalue2,Int64value2,i);
        if i>0 then raise Exception.Create(scanvalue2+' is not an valid 8-byte notation');
      end;

    end;

  end;


  helpword:=wordvalue xor 1+random(65534);
  helpdword:=dwordvalue xor helpword;

  //find the digits
  decim:=0;

  val(scanvalue,decimhelp,i);

  //i:=pos('.',scanvalue);
  if i>0 then decim:=length(scanvalue)-i;

  datatype:='NORMAL';
  MemoryRegions:=0;

  assignfile(memoryfile,CheatEngineDir+'Memory.TMP');
  rewrite(memoryfile,1);

  assignfile(Addressfile,CheatEngineDir+'Addresses.TMP');
  rewrite(Addressfile,1);
  blockwrite(Addressfile,datatype,sizeof(datatype));

  address:=start;


  try
//-------------------------------------------------
  while (Virtualqueryex(processhandle,pointer(address),mbi,sizeof(mbi))<>0) and (address<stop) and ((address+mbi.RegionSize)>address) do
  begin
    if (not (not scan_mem_private and (mbi.type_9=mem_private))) and (not (not scan_mem_image and (mbi.type_9=mem_image))) and (not (not scan_mem_mapped and (mbi.type_9=mem_mapped))) and (mbi.State=mem_commit) and ((mbi.Protect and page_guard)=0) and ((mbi.protect and page_noaccess)=0) then  //look if it is commited
    begin
      if not readonly then  //if the settings are set to not read read-only memory then
      begin
        if (((mbi.AllocationProtect) and (page_readonly or page_execute_read))=0) and
           (((mbi.Protect) and (page_readonly or PAGE_EXECUTE_READ))=0) then //look if the memory is not read-only  , if 0 it means it's not read-only
        begin
          if Skip_PAGE_NOCACHE then
            if (mbi.AllocationProtect and PAGE_NOCACHE)=PAGE_NOCACHE then
            begin
              address:=dword(mbi.BaseAddress)+mbi.RegionSize;
              continue;
            end;

          setlength(memoryregion,memoryregions+1);
          memoryregion[memoryregions].BaseAddress:=dword(mbi.baseaddress);  //remember if it's not read only
          memoryregion[memoryregions].MemorySize:=mbi.RegionSize;
          inc(memoryregions);
        end;
      end else  //if the settings are to also read the read only then:
      begin
        if Skip_PAGE_NOCACHE then
          if (mbi.AllocationProtect and PAGE_NOCACHE)=PAGE_NOCACHE then
          begin
            address:=dword(mbi.BaseAddress)+mbi.RegionSize;
            continue;
          end;

        setlength(memoryregion,memoryregions+1);
        memoryregion[memoryregions].BaseAddress:=dword(mbi.baseaddress);  //just remember this location
        memoryregion[memoryregions].MemorySize:=mbi.RegionSize;
        inc(memoryregions);
      end;
    end;


    address:=dword(mbi.baseaddress)+mbi.RegionSize;
  end;

  if memoryregions=0 then
  begin
    closefile(memoryfile);
    closefile(addressfile);
    raise Exception.create('No readable memory found in the region you specified!');
  end;

  //lets search really at the start of the location the user specified
  if (memoryregion[0].BaseAddress<start) and (memoryregion[0].MemorySize-(start-memoryregion[0].BaseAddress)>0) then
  begin
    memoryregion[0].MemorySize:=memoryregion[0].MemorySize-(start-memoryregion[0].BaseAddress);
    memoryregion[0].BaseAddress:=start;
  end;

  //also the right end
  if (memoryregion[memoryregions-1].BaseAddress+memoryregion[memoryregions-1].MemorySize)>stop then
    dec(memoryregion[memoryregions-1].MemorySize,(memoryregion[memoryregions-1].BaseAddress+memoryregion[memoryregions-1].MemorySize)-stop-1);


  j:=0;
  address:=memoryregion[0].BaseAddress;
  size:=memoryregion[0].MemorySize;

  for i:=1 to memoryregions-1 do
  begin
    if memoryregion[i].BaseAddress=address+size then
    begin
      inc(size,memoryregion[i].MemorySize);
    end
    else
    begin
      memoryregion[j].BaseAddress:=address;
      memoryregion[j].MemorySize:=size;

      address:=memoryregion[i].BaseAddress;
      size:=memoryregion[i].MemorySize;
      inc(j);
    end;
  end;

  memoryregion[j].BaseAddress:=address;
  memoryregion[j].MemorySize:=size;

  memoryregions:=j+1;
  setlength(memoryregion,memoryregions);

  //re-added due to complaints about speed:
  //split up into smaller chunks

  if buffersize>0 then
  begin
    i:=0;
    while i<=memoryregions-1 do
    begin
      if memoryregion[i].MemorySize>dword(buffersize) then
      begin
        inc(memoryregions);
        setlength(memoryregion,memoryregions);

        //copy the next element to the back, and split up the current one
        //(unless this is the item at the back, and not needed)
        if i<memoryregions-2 then
        begin
          //i isnt the last element of the array so do a semi-shift
          memoryregion[memoryregions-1].BaseAddress:=memoryregion[i+1].baseaddress;
          memoryregion[memoryregions-1].MemorySize:=memoryregion[i+1].MemorySize;
        end;

        memoryregion[i+1].IsChild:=true;
        if fastscan then
        case vartype of
        0: begin  //byte
             memoryregion[i+1].BaseAddress:=memoryregion[i].BaseAddress+buffersize;
             memoryregion[i+1].MemorySize:=memoryregion[i].MemorySize-buffersize;
           end;

        1: begin  //word
             memoryregion[i+1].BaseAddress:=memoryregion[i].BaseAddress+buffersize-1;
             memoryregion[i+1].MemorySize:=memoryregion[i].MemorySize-buffersize+1;
           end;

        2,3,4,6: begin  //dword+float
             memoryregion[i+1].BaseAddress:=memoryregion[i].BaseAddress+buffersize-3;
             memoryregion[i+1].MemorySize:=memoryregion[i].MemorySize-buffersize+3;
           end;

     {   4,6: begin  //double+int64
             memoryregion[i+1].BaseAddress:=memoryregion[i].BaseAddress+buffersize-7;
             memoryregion[i+1].MemorySize:=memoryregion[i].MemorySize-buffersize+7;
           end;  }

        else
           begin
             memoryregion[i+1].BaseAddress:=memoryregion[i].BaseAddress+buffersize;
             memoryregion[i+1].MemorySize:=memoryregion[i].MemorySize-buffersize;
           end;

        end
        else
        begin
          memoryregion[i+1].BaseAddress:=memoryregion[i].BaseAddress+buffersize;
          memoryregion[i+1].MemorySize:=memoryregion[i].MemorySize-buffersize;
        end;

        memoryregion[i].MemorySize:=buffersize;
      end;
      inc(i);
    end;
    dec(memoryregions);
  end else memoryregions:=j;

  if fastscan then
  begin
    //optimize regions
    for i:=0 to memoryregions do
    //make it so the regions all start at a location that can be devided by the size of the valtype
    case vartype of
      1: begin  //word (mod 2=0)
           j:=(memoryregion[i].BaseAddress mod 2);
           if j<>0 then
           if memoryregion[i].IsChild then
           begin
             memoryregion[i].BaseAddress:=memoryregion[i].BaseAddress-j;
             inc(memoryregion[i].MemorySize,j);
           end
           else
           begin
             memoryregion[i].BaseAddress:=memoryregion[i].BaseAddress+2-j;
             dec(memoryregion[i].MemorySize,2-j);
           end;
         end;

    2,3,4,6: begin  //dword+float
           j:=(memoryregion[i].BaseAddress mod 4);
           if j<>0 then
           if memoryregion[i].IsChild then
           begin
             memoryregion[i].BaseAddress:=memoryregion[i].BaseAddress-j;
             inc(memoryregion[i].MemorySize,j);
           end
           else
           begin
             memoryregion[i].BaseAddress:=memoryregion[i].BaseAddress+4-j;
             dec(memoryregion[i].MemorySize,4-j);
           end;
         end;

   { 4,6: begin //double+int64
           j:=(memoryregion[i].BaseAddress mod 8);
           if j<>0 then
           if memoryregion[i].IsChild then
           begin
             memoryregion[i].BaseAddress:=memoryregion[i].BaseAddress-j;
             inc(memoryregion[i].MemorySize,j);
           end
           else
           begin
             memoryregion[i].BaseAddress:=memoryregion[i].BaseAddress+8-j;
             dec(memoryregion[i].MemorySize,8-j);
           end;
         end;    }
    end;
  end;

  quicksortmemoryregions(0,length(memoryregion)-1);
  
  TotalToRead:=0;
  maxmem:=0;
  For i:=0 to Memoryregions do
  begin
    inc(TotalToRead,Memoryregion[i].MemorySize);
    if maxmem<Memoryregion[i].MemorySize then maxmem:=Memoryregion[i].MemorySize;
  end;

  //application.MessageBox(pchar(IntToStr(totaltoread)),pchar(IntToStr(totaltoread)),0);

  progressbar.max:=memoryregions+1;
 // setlength(memory,maxmem+1);  //+1 just because I want to be sure, you never know in delphi)
  if memory<>nil then
  begin
    freemem(memory);
    memory:=nil;
  end;

  getmem(memory,maxmem);
  number:=buffersize;

  setlength(foundaddress,number);
  setlength(foundaddressswitch,number);

  if scantype=UserDefined then
  begin


    //check scanvalue
    //x=newvalue

    //x=2y
  end;


  if scantype=SmallerThan then
  begin
    if vartype=0 then  //byte
    begin
      setlength(foundvalue1,number);
      setlength(foundvalue1switch,number);

      for i:=0 to memoryregions do
      begin
        bytep:=pointer(memory);
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        begin
          if actualread>0 then
          for j:=0 to actualread-1 do
          begin
            if bytep^<ByteValue then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+j;
              foundvalue1[found]:=bytep^;
              inc(found);

              if found=number then
              begin
                //wait till it's done with the last write opperation
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempbytearray:=pointer(foundvalue1switch);
                foundvalue1switch:=pointer(foundvalue1);
                foundvalue1:=pointer(tempbytearray);

                flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue1switch,number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointer(foundvalue1)^,number,actualwrite);
                found:=0;
              end;
            end;
            asm
              inc [bytep]
            end;
          end;
        end;
        progressbar.stepit;
      end;

      flushthread.datawritten.WaitFor(infinite);
      flushbuffer(addressfile,memoryfile,foundaddress,4*found,foundvalue1,found);

//      blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//      blockwrite(Memoryfile,pointer(foundvalue1)^,found,actualwrite);
    end;

    if vartype=1 then //word
    begin
      setlength(foundvalue2,number);
      setlength(foundvalue2switch,number);

      if fastscan then  //fastscan
      for i:=0 to memoryregions do
      begin
        wordp:=pointer(Memory);
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);

        for j:=1 to (actualread div 2) do
        begin
          if WordP^<WordValue then
          begin
            foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(wordp)-dword(memory));
            foundvalue2[found]:=WordP^;
            inc(found);
            if found=number then
            begin
              flushthread.datawritten.WaitFor(infinite);

              tempdwordarray:=pointer(foundaddressswitch);
              foundaddressswitch:=pointer(foundaddress);
              foundaddress:=pointer(tempdwordarray);

              tempwordarray:=pointer(foundvalue2switch);
              foundvalue2switch:=pointer(foundvalue2);
              foundvalue2:=pointer(tempwordarray);

              flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);


              //blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
              //blockwrite(Memoryfile,pointer(foundvalue2)^,number*2,actualwrite);
              found:=0;
            end;
          end;

          inc(wordp);
        end;

        progressbar.stepit;
      end
      else
      for i:=0 to memoryregions do
      begin
        wordp:=pointer(Memory);
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        if actualread>1 then
        begin
          for j:=0 to actualread-2 do
          begin
            if WordP^<WordValue then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+j;
              foundvalue2[found]:=WordP^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempwordarray:=pointer(foundvalue2switch);
                foundvalue2switch:=pointer(foundvalue2);
                foundvalue2:=pointer(tempwordarray);

                flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

             //   blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
             //   blockwrite(Memoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                found:=0;
              end;
            end;

            asm
              inc [Wordp];
            end;
          end;

          progressbar.stepit;
        end;
      end;

      flushthread.datawritten.WaitFor(infinite);
      flushbuffer(addressfile,memoryfile,foundaddress,4*found,foundvalue2,2*found);

      //blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
      //blockwrite(Memoryfile,pointer(foundvalue2)^,2*found,actualwrite);
    end;

    if vartype=2 then //dword
    begin
      setlength(foundvalue3,number);
      setlength(foundvalue3switch,number);

      if fastscan then
      for i:=0 to memoryregions do
      begin
        Dwordp:=pointer(memory);

        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        for j:=1 to (actualread div 4) do
        begin
          if DwordP^<DwordValue then
          begin
            foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(dwordp)-dword(memory));
            foundvalue3[found]:=DwordP^;
            inc(found);
            if found=number then
            begin
              flushthread.datawritten.WaitFor(infinite);

              tempdwordarray:=pointer(foundaddressswitch);
              foundaddressswitch:=pointer(foundaddress);
              foundaddress:=pointer(tempdwordarray);

              tempdwordarray:=pointer(foundvalue3switch);
              foundvalue3switch:=pointer(foundvalue3);
              foundvalue3:=pointer(tempdwordarray);

              flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//              blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//              blockwrite(Memoryfile,pointer(foundvalue3)^,number*4,actualwrite);
              found:=0;
            end;
          end;
          inc(dwordp);
        end;
        progressbar.stepit;
      end
      else
      for i:=0 to memoryregions do
      begin
        Dwordp:=pointer(memory);

        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        if actualread>=4 then
        begin
          for j:=0 to actualread-4 do
          begin
            if DwordP^<DwordValue then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+j;
              foundvalue3[found]:=DwordP^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempdwordarray:=pointer(foundvalue3switch);
                foundvalue3switch:=pointer(foundvalue3);
                foundvalue3:=pointer(tempdwordarray);

                flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                found:=0;
              end;
            end;
            asm
              inc [DwordP]
            end;

          end;
          progressbar.stepit;
        end;
      end;

      flushthread.datawritten.WaitFor(infinite);
      flushbuffer(addressfile,memoryfile,foundaddress,4*found,foundvalue3,4*found);

//      blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//      blockwrite(Memoryfile,pointer(foundvalue3)^,4*found,actualwrite);
    end;

    if vartype=3 then //float
    begin
      setlength(foundvalue4,number);
      setlength(foundvalue4switch,number);
      SingleValue:=RoundTo(singlevalue,-decim);   //shouldn't be needed...

      if fastscan then
      for i:=0 to memoryregions do
      begin
        Singlep:=pointer(memory);
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);

        for j:=1 to (actualread div 4) do
        begin
          helpsingle:=RoundTo(SingleP^,-decim);

          check:=(not (isnan(singlep^) or isinfinite(singlep^))) and (helpsingle<SingleValue);

          if check then
          begin
            foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(singlep)-dword(memory));
            foundvalue4[found]:=SingleP^;
            inc(found);
            if found=number then
            begin
              flushthread.datawritten.WaitFor(infinite);

              tempdwordarray:=pointer(foundaddressswitch);
              foundaddressswitch:=pointer(foundaddress);
              foundaddress:=pointer(tempdwordarray);

              tempsinglearray:=pointer(foundvalue4switch);
              foundvalue4switch:=pointer(foundvalue4);
              foundvalue4:=pointer(tempsinglearray);

              flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//              blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//              blockwrite(Memoryfile,pointer(foundvalue4)^,number*4,actualwrite);
              found:=0;
            end;
          end;
          inc(singlep);
        end;

        progressbar.stepit;
      end
      else
      for i:=0 to memoryregions do
      begin
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        if actualread>=4 then
        begin
          Singlep:=pointer(memory);
          for j:=0 to actualread-1 do
          begin
            helpsingle:=RoundTo(SingleP^,-decim);
            if (not (isnan(singlep^) or isinfinite(singlep^))) and (helpsingle<SingleValue) then
            begin
              foundvalue4[found]:=SingleP^;
              foundaddress[found]:=Memoryregion[i].BaseAddress+j;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempsinglearray:=pointer(foundvalue4switch);
                foundvalue4switch:=pointer(foundvalue4);
                foundvalue4:=pointer(tempsinglearray);

                flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                found:=0;
              end;
            end;

            asm
              inc [SingleP]
            end;
          end;

        end;

        progressbar.stepit;
      end;

      flushthread.datawritten.WaitFor(infinite);
      flushbuffer(addressfile,memoryfile,foundaddress,4*found,foundvalue4,4*found);


//      blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//      blockwrite(Memoryfile,pointer(foundvalue4)^,4*found,actualwrite);
    end;

    if vartype=4 then //double
    begin
      setlength(foundvalue5,number);
      setlength(foundvalue5switch,number);
      DoubleValue:=RoundTo(doublevalue,-decim);

      if fastscan then
      for i:=0 to memoryregions do
      begin
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        Doublep:=pointer(memory);

        for j:=1 to (actualread div 4) do
        begin
          if (not (isnan(doublep^) or isinfinite(doublep^))) and (RoundTo(DoubleP^,-decim)<doublevalue) then
          begin
            foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(doublep)-dword(memory));
            foundvalue5[found]:=doublep^;
            inc(found);
            if found=number then
            begin
              flushthread.datawritten.WaitFor(infinite);

              tempdwordarray:=pointer(foundaddressswitch);
              foundaddressswitch:=pointer(foundaddress);
              foundaddress:=pointer(tempdwordarray);

              tempdoublearray:=pointer(foundvalue5switch);
              foundvalue5switch:=pointer(foundvalue5);
              foundvalue5:=pointer(tempdoublearray);

              flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//              blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//              blockwrite(Memoryfile,pointer(foundvalue5)^,number*8,actualwrite);
              found:=0;
            end;
          end;
          asm
            add [Doublep],4
          end;
        end;
        progressbar.StepIt;
      end
      else
      for i:=0 to memoryregions do
      begin
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        if actualread>7 then
        begin
          Doublep:=pointer(memory);
          for j:=0 to actualread-8 do
          begin
            if (not (isnan(doublep^) or isinfinite(doublep^))) and (RoundTo(DoubleP^,-decim)<doublevalue) then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+j;
              foundvalue5[found]:=doublep^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempdoublearray:=pointer(foundvalue5switch);
                foundvalue5switch:=pointer(foundvalue5);
                foundvalue5:=pointer(tempdoublearray);

                flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                found:=0;
              end;
            end;
            asm
              inc [Doublep]
            end;
          end;
        end;
        progressbar.StepIt;
      end;

      flushthread.datawritten.WaitFor(infinite);
      flushbuffer(addressfile,memoryfile,foundaddress,4*found,foundvalue5,8*found);

//      blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//      blockwrite(Memoryfile,pointer(foundvalue5)^,8*found,actualwrite);
    end;

    if vartype=6 then //Int64
    begin
      bytep2:=@helpint64;
      inc(bytep2,7);
      setlength(foundvalue7,number);
      setlength(foundvalue7switch,number);

      if fastscan then
      for i:=0 to memoryregions do
      begin
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        Int64p:=pointer(memory);

        for j:=1 to (actualread div 4) do
        begin
          if Int64P^<Int64Value then
          begin
            foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(int64p)-dword(memory));
            foundvalue7[found]:=Int64P^;
            inc(found);

            if found=number then
            begin
              flushthread.datawritten.WaitFor(infinite);

              tempdwordarray:=pointer(foundaddressswitch);
              foundaddressswitch:=pointer(foundaddress);
              foundaddress:=pointer(tempdwordarray);

              tempint64array:=pointer(foundvalue7switch);
              foundvalue7switch:=pointer(foundvalue7);
              foundvalue7:=pointer(tempint64array);

              flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue7switch,8*number);

//              blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//              blockwrite(Memoryfile,pointeR(foundvalue7)^,number*8,actualwrite);
              found:=0;
            end;
          end;
          asm
            add [int64p],4
          end;
        end;
        progressbar.stepit;
      end
      else
      for i:=0 to memoryregions do
      begin
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        if actualread>=8 then
        begin
          Int64p:=pointer(memory);
          for j:=0 to actualread-8 do
          begin
            if Int64P^<Int64Value then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+j;
              foundvalue7[found]:=Int64P^;
              inc(found);

              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempint64array:=pointer(foundvalue7switch);
                foundvalue7switch:=pointer(foundvalue7);
                foundvalue7:=pointer(tempint64array);

                flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue7switch,8*number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointeR(foundvalue7)^,number*8,actualwrite);
                found:=0;
              end;
            end;
            asm
              inc [int64p]
            end;
          end;
        end;
        progressbar.stepit;
      end;
      flushthread.datawritten.WaitFor(infinite);
      flushbuffer(addressfile,memoryfile,foundaddress,4*found,foundvalue7,8*found);


//      blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//      blockwrite(Memoryfile,pointer(foundvalue7)^,8*found,actualwrite);
    end;

  end
  else //----------------------------------bigger than---------------------
  if scantype=BiggerThan then
  begin
    if vartype=0 then  //byte
    begin
      setlength(foundvalue1,number);
      setlength(foundvalue1switch,number);

      for i:=0 to memoryregions do
      begin
        bytep:=pointer(memory);
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        begin
          if actualread>0 then
          for j:=0 to actualread-1 do
          begin
            if bytep^>ByteValue then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+j;
              foundvalue1[found]:=bytep^;
              inc(found);

              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempbytearray:=pointer(foundvalue1switch);
                foundvalue1switch:=pointer(foundvalue1);
                foundvalue1:=pointer(tempbytearray);

                flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue1switch,number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointer(foundvalue1)^,number,actualwrite);
                found:=0;
              end;
            end;
            asm
              inc [bytep]
            end;
          end;
        end;
        progressbar.stepit;
      end;

      flushthread.datawritten.WaitFor(infinite);
      flushbuffer(addressfile,memoryfile,foundaddress,4*found,foundvalue1,found);

//      blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//      blockwrite(Memoryfile,pointer(foundvalue1)^,found,actualwrite);
    end;

    if vartype=1 then //word
    begin
      setlength(foundvalue2,number);
      setlength(foundvalue2switch,number);
      bytep2:=@helpword;
      inc(bytep2);

      if fastscan then
      for i:=0 to memoryregions do
      begin
        wordp:=pointer(Memory);
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        for j:=1 to (actualread div 2) do
        begin
          if WordP^>WordValue then
          begin
            foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(wordp)-dword(memory));
            foundvalue2[found]:=WordP^;
            inc(found);
            if found=number then
            begin
              flushthread.datawritten.WaitFor(infinite);

              tempdwordarray:=pointer(foundaddressswitch);
              foundaddressswitch:=pointer(foundaddress);
              foundaddress:=pointer(tempdwordarray);

              tempwordarray:=pointer(foundvalue2switch);
              foundvalue2switch:=pointer(foundvalue2);
              foundvalue2:=pointer(tempwordarray);

              flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);


//              blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//              blockwrite(Memoryfile,pointer(foundvalue2)^,number*2,actualwrite);
              found:=0;
            end;
          end;
          inc(wordp);
        end;
        progressbar.stepit;
      end
      else
      for i:=0 to memoryregions do
      begin
        wordp:=pointer(Memory);
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        if actualread>=2 then
        begin
          for j:=0 to actualread-2 do
          begin
            if WordP^>WordValue then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+j;
              foundvalue2[found]:=WordP^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempwordarray:=pointer(foundvalue2switch);
                foundvalue2switch:=pointer(foundvalue2);
                foundvalue2:=pointer(tempwordarray);

                flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);


//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                found:=0;
              end;
            end;

            asm
              inc [Wordp];
            end;
          end;
          progressbar.stepit;
        end;
      end;

      flushthread.datawritten.WaitFor(infinite);
      flushbuffer(addressfile,memoryfile,foundaddress,4*found,foundvalue2,2*found);

//      blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//      blockwrite(Memoryfile,pointer(foundvalue2)^,2*found,actualwrite);
    end;

    if vartype=2 then //dword
    begin
      setlength(foundvalue3,number);
      setlength(foundvalue3switch,number);

      if fastscan then
      for i:=0 to memoryregions do
      begin
        Dwordp:=pointer(memory);
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        for j:=1 to (actualread div 4) do
        begin
          if DwordP^>DwordValue then
          begin
            foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(dwordp)-dword(memory));
            foundvalue3[found]:=DwordP^;
            inc(found);
            if found=number then
            begin
              flushthread.datawritten.WaitFor(infinite);

              tempdwordarray:=pointer(foundaddressswitch);
              foundaddressswitch:=pointer(foundaddress);
              foundaddress:=pointer(tempdwordarray);

              tempdwordarray:=pointer(foundvalue3switch);
              foundvalue3switch:=pointer(foundvalue3);
              foundvalue3:=pointer(tempdwordarray);

              flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//            blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//            blockwrite(Memoryfile,pointer(foundvalue3)^,number*4,actualwrite);
              found:=0;
            end;
          end;
          inc(Dwordp);
        end;
        progressbar.stepit;
      end
      else
      for i:=0 to memoryregions do
      begin
        Dwordp:=pointer(memory);
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        if actualread>=4 then
        begin
          for j:=0 to actualread-4 do
          begin
            if DwordP^>DwordValue then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+j;
              foundvalue3[found]:=DwordP^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempdwordarray:=pointer(foundvalue3switch);
                foundvalue3switch:=pointer(foundvalue3);
                foundvalue3:=pointer(tempdwordarray);

                flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                found:=0;
              end;
            end;
            asm
              inc [DwordP]
            end;

          end;
          progressbar.stepit;
        end;
      end;

      flushthread.datawritten.WaitFor(infinite);
      flushbuffer(addressfile,memoryfile,foundaddress,4*found,foundvalue3,4*found);

//      blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//      blockwrite(Memoryfile,pointer(foundvalue3)^,4*found,actualwrite);
    end;

    if vartype=3 then //float
    begin
      setlength(foundvalue4,number);
      setlength(foundvalue4switch,number);

      SingleValue:=RoundTo(singlevalue,-decim);

      if fastscan then
      for i:=0 to memoryregions do
      begin
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        Singlep:=pointer(memory);
        for j:=1 to (actualread div 4) do
        begin
          helpsingle:=RoundTo(SingleP^,-decim);
          if (not (isnan(singlep^) or isinfinite(singlep^))) and (helpsingle>SingleValue) then
          begin
            foundvalue4[found]:=SingleP^;
            foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(singlep)-dword(memory));
            inc(found);
            if found=number then
            begin
              flushthread.datawritten.WaitFor(infinite);

              tempdwordarray:=pointer(foundaddressswitch);
              foundaddressswitch:=pointer(foundaddress);
              foundaddress:=pointer(tempdwordarray);

              tempsinglearray:=pointer(foundvalue4switch);
              foundvalue4switch:=pointer(foundvalue4);
              foundvalue4:=pointer(tempsinglearray);

              flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//              blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//              blockwrite(Memoryfile,pointer(foundvalue4)^,number*4,actualwrite);
              found:=0;
            end;
          end;
          inc(SingleP);
        end;

        progressbar.stepit;
      end
      else
      for i:=0 to memoryregions do
      begin
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        if actualread>=4 then
        begin
          Singlep:=pointer(memory);
          for j:=0 to actualread-1 do
          begin
            helpsingle:=RoundTo(SingleP^,-decim);
            if (not (isnan(singlep^) or isinfinite(singlep^))) and (helpsingle>SingleValue) then
            begin
              foundvalue4[found]:=SingleP^;
              foundaddress[found]:=Memoryregion[i].BaseAddress+j;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempsinglearray:=pointer(foundvalue4switch);
                foundvalue4switch:=pointer(foundvalue4);
                foundvalue4:=pointer(tempsinglearray);

                flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                found:=0;
              end;
            end;

            asm
              inc [SingleP]
            end;
          end;
        end;

        progressbar.stepit;
      end;

      flushthread.datawritten.WaitFor(infinite);
      flushbuffer(addressfile,memoryfile,foundaddress,4*found,foundvalue4,4*found);
//      blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//      blockwrite(Memoryfile,pointer(foundvalue4)^,4*found,actualwrite);
    end;

    if vartype=4 then //double
    begin
      setlength(foundvalue5,number);
      setlength(foundvalue5switch,number);
      DoubleValue:=RoundTo(doublevalue,-decim);

      if fastscan then
      for i:=0 to memoryregions do
      begin
        Doublep:=pointer(memory);
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        for j:=1 to (actualread div 4) do
        begin
          if (not (isnan(doublep^) or isinfinite(doublep^))) and (RoundTo(DoubleP^,-decim)>doublevalue) then
          begin
            foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(doublep)-dword(memory));
            foundvalue5[found]:=doublep^;
            inc(found);
            if found=number then
            begin
              flushthread.datawritten.WaitFor(infinite);

              tempdwordarray:=pointer(foundaddressswitch);
              foundaddressswitch:=pointer(foundaddress);
              foundaddress:=pointer(tempdwordarray);

              tempdoublearray:=pointer(foundvalue5switch);
              foundvalue5switch:=pointer(foundvalue5);
              foundvalue5:=pointer(tempdoublearray);

              flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//              blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//              blockwrite(Memoryfile,pointer(foundvalue5)^,number*8,actualwrite);
              found:=0;
            end;
          end;
          asm
            add [Doublep],4
          end;
        end;
        progressbar.StepIt;
      end
      else
      for i:=0 to memoryregions do
      begin
        Doublep:=pointer(memory);
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        if actualread>=8 then
        begin
          for j:=0 to actualread-8 do
          begin
            if (not (isnan(doublep^) or isinfinite(doublep^))) and (RoundTo(DoubleP^,-decim)>doublevalue) then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+j;
              foundvalue5[found]:=doublep^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempdoublearray:=pointer(foundvalue5switch);
                foundvalue5switch:=pointer(foundvalue5);
                foundvalue5:=pointer(tempdoublearray);

                flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                found:=0;
              end;
            end;
            asm
              inc [Doublep]
            end;
          end;
        end;
        progressbar.StepIt;
      end;

      flushthread.datawritten.WaitFor(infinite);
      flushbuffer(addressfile,memoryfile,foundaddress,4*found,foundvalue5,8*found);
//      blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//      blockwrite(Memoryfile,pointer(foundvalue5)^,8*found,actualwrite);
    end;

    if vartype=6 then //Int64
    begin
      setlength(foundvalue7,number);
      setlength(foundvalue7switch,number);

      if fastscan then
      for i:=0 to memoryregions do
      begin
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        Int64p:=pointer(memory);

        for j:=1 to (actualread div 4) do
        begin
          if Int64P^>Int64Value then
          begin
            foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(int64p)-dword(memory));
            foundvalue7[found]:=Int64P^;
            inc(found);

            if found=number then
            begin
              flushthread.datawritten.WaitFor(infinite);

              tempdwordarray:=pointer(foundaddressswitch);
              foundaddressswitch:=pointer(foundaddress);
              foundaddress:=pointer(tempdwordarray);

              tempint64array:=pointer(foundvalue7switch);
              foundvalue7switch:=pointer(foundvalue7);
              foundvalue7:=pointer(tempint64array);

              flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue7switch,8*number);


//              blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//              blockwrite(Memoryfile,pointeR(foundvalue7)^,number*8,actualwrite);
              found:=0;
            end;
          end;
          asm
            add [int64p],4
          end;
        end;
        progressbar.stepit;
      end
      else
      for i:=0 to memoryregions do
      begin
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        if actualread>=8 then
        begin
          Int64p:=pointer(memory);
          for j:=0 to actualread-8 do
          begin
            if Int64P^>Int64Value then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+j;
              foundvalue7[found]:=Int64P^;
              inc(found);

              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempint64array:=pointer(foundvalue7switch);
                foundvalue7switch:=pointer(foundvalue7);
                foundvalue7:=pointer(tempint64array);

                flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue7switch,8*number);


//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointeR(foundvalue7)^,number*8,actualwrite);
                found:=0;
              end;
            end;
            asm
              inc [int64p]
            end;
          end;
        end;
        progressbar.stepit;
      end;

      flushthread.datawritten.WaitFor(infinite);
      flushbuffer(addressfile,memoryfile,foundaddress,4*found,foundvalue7,8*found);


//      blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//      blockwrite(Memoryfile,pointer(foundvalue7)^,8*found,actualwrite);
    end;

  end
  else //----------------------------------between---------------------
  if scantype=ValueBetween  then
  begin
    if vartype=0 then  //byte
    begin
      setlength(foundvalue1,number);
      setlength(foundvalue1switch,number);

      for i:=0 to memoryregions do
      begin
        bytep:=pointer(memory);
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        begin
          if actualread>0 then
          for j:=0 to actualread-1 do
          begin
            if (bytep^>=ByteValue) and (bytep^<=bytevalue2) then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+j;
              foundvalue1[found]:=bytep^;
              inc(found);

              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempbytearray:=pointer(foundvalue1switch);
                foundvalue1switch:=pointer(foundvalue1);
                foundvalue1:=pointer(tempbytearray);

                flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue1switch,number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointer(foundvalue1)^,number,actualwrite);
                found:=0;
              end;
            end;
            asm
              inc [bytep]
            end;
          end;
        end;
        progressbar.stepit;
      end;

      flushthread.datawritten.WaitFor(infinite);
      flushbuffer(addressfile,memoryfile,foundaddress,4*found,foundvalue1,found);

//      blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//      blockwrite(Memoryfile,pointer(foundvalue1)^,found,actualwrite);
    end;

    if vartype=1 then //word
    begin
      setlength(foundvalue2,number);
      setlength(foundvalue2switch,number);

      bytep2:=@helpword;
      inc(bytep2);

      if fastscan then
      for i:=0 to memoryregions do
      begin
        wordp:=pointer(Memory);
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        for j:=1 to (actualread div 2) do
        begin
          if (WordP^>=WordValue) and (wordp^<=wordvalue2) then
          begin
            foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(wordp)-dword(memory));
            foundvalue2[found]:=WordP^;
            inc(found);
            if found=number then
            begin
              flushthread.datawritten.WaitFor(infinite);

              tempdwordarray:=pointer(foundaddressswitch);
              foundaddressswitch:=pointer(foundaddress);
              foundaddress:=pointer(tempdwordarray);

              tempwordarray:=pointer(foundvalue2switch);
              foundvalue2switch:=pointer(foundvalue2);
              foundvalue2:=pointer(tempwordarray);

              flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);


//              blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//              blockwrite(Memoryfile,pointer(foundvalue2)^,number*2,actualwrite);
              found:=0;
            end;
          end;
          inc(wordp);
        end;
        progressbar.stepit;
      end
      else
      for i:=0 to memoryregions do
      begin
        wordp:=pointer(Memory);
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        if actualread>=2 then
        begin
          for j:=0 to actualread-2 do
          begin
            if (WordP^>=WordValue) and (Wordp^<=wordvalue2) then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+j;
              foundvalue2[found]:=WordP^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempwordarray:=pointer(foundvalue2switch);
                foundvalue2switch:=pointer(foundvalue2);
                foundvalue2:=pointer(tempwordarray);

                flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                found:=0;
              end;
            end;

            asm
              inc [Wordp];
            end;
          end;
          progressbar.stepit;
        end;
      end;

      flushthread.datawritten.WaitFor(infinite);
      flushbuffer(addressfile,memoryfile,foundaddress,4*found,foundvalue2,2*found);

//      blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//      blockwrite(Memoryfile,pointer(foundvalue2)^,2*found,actualwrite);
    end;

    if vartype=2 then //dword
    begin
      setlength(foundvalue3,number);
      setlength(foundvalue3switch,number);

      if fastscan then
      for i:=0 to memoryregions do
      begin
        Dwordp:=pointer(memory);
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        for j:=1 to (actualread div 4) do
        begin
          if (DwordP^>=DwordValue) and (DwordP^<=DwordValue2) then
          begin
            foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(dwordp)-dword(memory));
            foundvalue3[found]:=DwordP^;
            inc(found);
            if found=number then
            begin
              flushthread.datawritten.WaitFor(infinite);

              tempdwordarray:=pointer(foundaddressswitch);
              foundaddressswitch:=pointer(foundaddress);
              foundaddress:=pointer(tempdwordarray);

              tempdwordarray:=pointer(foundvalue3switch);
              foundvalue3switch:=pointer(foundvalue3);
              foundvalue3:=pointer(tempdwordarray);

              flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//              blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//              blockwrite(Memoryfile,pointer(foundvalue3)^,number*4,actualwrite);
              found:=0;
            end;
          end;
          inc(Dwordp);
        end;
        progressbar.stepit;
      end
      else
      for i:=0 to memoryregions do
      begin
        Dwordp:=pointer(memory);
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        if actualread>=4 then
        begin
          for j:=0 to actualread-4 do
          begin
            if (DwordP^>=DwordValue) and (DwordP^<=DwordValue2) then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+j;
              foundvalue3[found]:=DwordP^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempdwordarray:=pointer(foundvalue3switch);
                foundvalue3switch:=pointer(foundvalue3);
                foundvalue3:=pointer(tempdwordarray);

                flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                found:=0;
              end;
            end;
            asm
              inc [DwordP]
            end;

          end;
          progressbar.stepit;
        end;
      end;

      flushthread.datawritten.WaitFor(infinite);
      flushbuffer(addressfile,memoryfile,foundaddress,4*found,foundvalue3,4*found);

//      blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//      blockwrite(Memoryfile,pointer(foundvalue3)^,4*found,actualwrite);
    end;

    if vartype=3 then //float
    begin
      setlength(foundvalue4,number);
      setlength(foundvalue4switch,number);
      SingleValue:=RoundTo(singlevalue,-decim);

      if fastscan then
      for i:=0 to memoryregions do
      begin
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        Singlep:=pointer(memory);
        for j:=1 to (actualread div 4) do
        begin
          helpsingle:=RoundTo(SingleP^,-decim);
          if (not (isnan(singlep^) or isinfinite(singlep^))) and (helpsingle>=SingleValue) and (helpsingle<=SingleValue2) then
          begin
            foundvalue4[found]:=SingleP^;
            foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(singlep)-dword(memory));
            inc(found);
            if found=number then
            begin
              flushthread.datawritten.WaitFor(infinite);

              tempdwordarray:=pointer(foundaddressswitch);
              foundaddressswitch:=pointer(foundaddress);
              foundaddress:=pointer(tempdwordarray);

              tempsinglearray:=pointer(foundvalue4switch);
              foundvalue4switch:=pointer(foundvalue4);
              foundvalue4:=pointer(tempsinglearray);

              flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//              blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//              blockwrite(Memoryfile,pointer(foundvalue4)^,number*4,actualwrite);
              found:=0;
            end;
          end;
          inc(SingleP);
        end;

        progressbar.stepit;
      end
      else
      for i:=0 to memoryregions do
      begin
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        if actualread>=4 then
        begin
          Singlep:=pointer(memory);
          for j:=0 to actualread-1 do
          begin
            helpsingle:=RoundTo(SingleP^,-decim);
            if (not (isnan(singlep^) or isinfinite(singlep^))) and (helpsingle>=SingleValue) and (helpsingle<=SingleValue2) then
            begin
              foundvalue4[found]:=SingleP^;
              foundaddress[found]:=Memoryregion[i].BaseAddress+j;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempsinglearray:=pointer(foundvalue4switch);
                foundvalue4switch:=pointer(foundvalue4);
                foundvalue4:=pointer(tempsinglearray);

                flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                found:=0;
              end;
            end;

            asm
              inc [SingleP]
            end;
          end;
        end;

        progressbar.stepit;
      end;

      flushthread.datawritten.WaitFor(infinite);
      flushbuffer(addressfile,memoryfile,foundaddress,4*found,foundvalue4,4*found);

//      blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//      blockwrite(Memoryfile,pointer(foundvalue4)^,4*found,actualwrite);
    end;

    if vartype=4 then //double
    begin
      setlength(foundvalue5,number);
      setlength(foundvalue5switch,number);
      DoubleValue:=RoundTo(doublevalue,-decim);

      if fastscan then
      for i:=0 to memoryregions do
      begin
        Doublep:=pointer(memory);
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        for j:=1 to (actualread div 4) do
        begin
          if (not (isnan(doublep^) or isinfinite(doublep^))) and (DoubleP^>=doublevalue) and (doublep^<=doublevalue2) then
          begin
            foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(doublep)-dword(memory));
            foundvalue5[found]:=doublep^;
            inc(found);
            if found=number then
            begin
              flushthread.datawritten.WaitFor(infinite);

              tempdwordarray:=pointer(foundaddressswitch);
              foundaddressswitch:=pointer(foundaddress);
              foundaddress:=pointer(tempdwordarray);

              tempdoublearray:=pointer(foundvalue5switch);
              foundvalue5switch:=pointer(foundvalue5);
              foundvalue5:=pointer(tempdoublearray);

              flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//              blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//              blockwrite(Memoryfile,pointer(foundvalue5)^,number*8,actualwrite);
              found:=0;
            end;
          end;
          asm
            add [Doublep],4
          end;
        end;
        progressbar.StepIt;
      end
      else
      for i:=0 to memoryregions do
      begin
        Doublep:=pointer(memory);
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        if actualread>=8 then
        begin
          for j:=0 to actualread-8 do
          begin
            if (not (isnan(doublep^) or isinfinite(doublep^))) and (DoubleP^>=doublevalue) and (Doublep^<=doublevalue) then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+j;
              foundvalue5[found]:=doublep^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempdoublearray:=pointer(foundvalue5switch);
                foundvalue5switch:=pointer(foundvalue5);
                foundvalue5:=pointer(tempdoublearray);

                flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                found:=0;
              end;
            end;
            asm
              inc [Doublep]
            end;
          end;
        end;
        progressbar.StepIt;
      end;

      flushthread.datawritten.WaitFor(infinite);
      flushbuffer(addressfile,memoryfile,foundaddress,4*found,foundvalue5,8*found);

//      blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//      blockwrite(Memoryfile,pointer(foundvalue5)^,8*found,actualwrite);
    end;

    if vartype=6 then //Int64
    begin
      setlength(foundvalue7,number);
      setlength(foundvalue7switch,number);

      if fastscan then
      for i:=0 to memoryregions do
      begin
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        Int64p:=pointer(memory);

        for j:=1 to (actualread div 4) do
        begin
          if (Int64P^>=Int64Value) and (Int64P^<=Int64Value2) then
          begin
            foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(int64p)-dword(memory));
            foundvalue7[found]:=Int64P^;
            inc(found);

            if found=number then
            begin
              flushthread.datawritten.WaitFor(infinite);

              tempdwordarray:=pointer(foundaddressswitch);
              foundaddressswitch:=pointer(foundaddress);
              foundaddress:=pointer(tempdwordarray);

              tempint64array:=pointer(foundvalue7switch);
              foundvalue7switch:=pointer(foundvalue7);
              foundvalue7:=pointer(tempint64array);

              flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue7switch,8*number);


//              blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//              blockwrite(Memoryfile,pointeR(foundvalue7)^,number*8,actualwrite);
              found:=0;
            end;
          end;
          asm
            add [int64p],4
          end;
        end;
        progressbar.stepit;
      end
      else
      for i:=0 to memoryregions do
      begin
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        if actualread>=8 then
        begin
          Int64p:=pointer(memory);
          for j:=0 to actualread-8 do
          begin
            if (Int64P^>=Int64Value) and (int64p^<=int64value2) then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+j;
              foundvalue7[found]:=Int64P^;
              inc(found);

              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempint64array:=pointer(foundvalue7switch);
                foundvalue7switch:=pointer(foundvalue7);
                foundvalue7:=pointer(tempint64array);

                flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue7switch,8*number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointeR(foundvalue7)^,number*8,actualwrite);
                found:=0;
              end;
            end;
            asm
              inc [int64p]
            end;
          end;
        end;
        progressbar.stepit;
      end;

      flushthread.datawritten.WaitFor(infinite);
      flushbuffer(addressfile,memoryfile,foundaddress,4*found,foundvalue7,8*found);

//      blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//      blockwrite(Memoryfile,pointer(foundvalue7)^,8*found,actualwrite);
    end;
  end
  else  //----------------------------------EXACT---------------------
  if scantype=Exact_value then
  begin
    if vartype=0 then  //byte
    begin
      setlength(foundvalue1,number);
      setlength(foundvalue1switch,number);
      foundvalue1[0]:=bytevalue+1;
      foundvalue1switch[0]:=bytevalue+1;

      for i:=0 to memoryregions do
      begin
        bytep:=pointer(memory);
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        begin
          if actualread>0 then
          for j:=0 to actualread-1 do
          begin
            if bytep^=ByteValue then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+j;
              inc(found);

              if found=number then
              begin
                if foundvalue1[0]<>bytevalue then
                begin
                  for k:=0 to number-1 do foundvalue1[k]:=bytevalue;
                  FoundIsFilled:=true;
                end;

                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempbytearray:=pointer(foundvalue1switch);
                foundvalue1switch:=pointer(foundvalue1);
                foundvalue1:=pointer(tempbytearray);

                flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue1switch,number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointer(foundvalue1)^,number,actualwrite);
                found:=0;
              end;
            end;
            asm
              inc [bytep]
            end;
          end;
        end;
        progressbar.stepit;
      end;

      if foundvalue1[0]<>bytevalue then
      begin
        for k:=0 to found-1 do foundvalue1[k]:=bytevalue;
        FoundIsFilled:=true;
      end;

      flushthread.datawritten.WaitFor(infinite);
      flushbuffer(addressfile,memoryfile,foundaddress,4*found,foundvalue1,found);

//      blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//      blockwrite(Memoryfile,pointer(foundvalue1)^,found,actualwrite);
    end;

    if vartype=1 then //word
    begin
      setlength(foundvalue2,number);
      setlength(foundvalue2switch,number);
      foundvalue2[0]:=wordvalue+1;
      foundvalue2switch[1]:=wordvalue+1;

      if fastscan then
      for i:=0 to memoryregions do
      begin
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        wordp:=pointer(Memory);

        for j:=1 to (actualread div 2) do
        begin
          if WordP^=WordValue then
          begin
            foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(wordp)-dword(memory));
            inc(found);
            if found=number then
            begin
              if foundvalue2[0]<>wordvalue then
              begin
                for k:=0 to number-1 do foundvalue2[k]:=wordvalue;
                FoundIsFilled:=true;
              end;

              flushthread.datawritten.WaitFor(infinite);

              tempdwordarray:=pointer(foundaddressswitch);
              foundaddressswitch:=pointer(foundaddress);
              foundaddress:=pointer(tempdwordarray);

              tempwordarray:=pointer(foundvalue2switch);
              foundvalue2switch:=pointer(foundvalue2);
              foundvalue2:=pointer(tempwordarray);

              flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);


//              blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//              blockwrite(Memoryfile,pointer(foundvalue2)^,number*2,actualwrite);
              found:=0;
            end;
          end;
          inc(wordp);
        end;
        progressbar.stepit;
      end
      else
      for i:=0 to memoryregions do
      begin
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        if actualread>1 then
        begin
          //looking at the byte of the previous loop is not neccesary anymore
          wordp:=pointer(Memory);

          for j:=0 to actualread-2 do
          begin
            if WordP^=WordValue then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+j;
              inc(found);
              if found=number then
              begin
                if foundvalue2[0]<>wordvalue then
                begin
                  for k:=0 to number-1 do foundvalue2[k]:=wordvalue;
                  FoundIsFilled:=true;
                end;

                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempwordarray:=pointer(foundvalue2switch);
                foundvalue2switch:=pointer(foundvalue2);
                foundvalue2:=pointer(tempwordarray);

                flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                found:=0;
              end;
            end;

            asm
              inc [Wordp];
            end;
          end;
        end;
        progressbar.stepit;
      end;
      if foundvalue2[0]<>wordvalue then
      begin
        for k:=0 to found-1 do foundvalue2[k]:=wordvalue;
        FoundIsFilled:=true;
      end;

      flushthread.datawritten.WaitFor(infinite);
      flushbuffer(addressfile,memoryfile,foundaddress,4*found,foundvalue2,2*found);
      
//      blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//      blockwrite(Memoryfile,pointer(foundvalue2)^,2*found,actualwrite);
    end;

    if vartype=2 then //dword
    begin
      setlength(foundvalue3,number);
      setlength(foundvalue3switch,number);
      foundvalue3[0]:=dwordvalue+1;
      foundvalue3switch[0]:=dwordvalue+1;

      if fastscan then
      for i:=0 to memoryregions do
      begin
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        Dwordp:=pointer(memory);

        for j:=1 to (actualread div 4) do
        begin
          if DwordP^=DwordValue then
          begin
            foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(dwordp)-dword(memory));
            inc(found);
            if found=number then
            begin
              if foundvalue3[0]<>dwordvalue then
              begin
                for k:=0 to number-1 do foundvalue3[k]:=dwordvalue;
                FoundIsFilled:=true;
              end;

              flushthread.datawritten.WaitFor(infinite);

              tempdwordarray:=pointer(foundaddressswitch);
              foundaddressswitch:=pointer(foundaddress);
              foundaddress:=pointer(tempdwordarray);

              tempdwordarray:=pointer(foundvalue3switch);
              foundvalue3switch:=pointer(foundvalue3);
              foundvalue3:=pointer(tempdwordarray);

              flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//              blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//              blockwrite(Memoryfile,pointer(foundvalue3)^,number*4,actualwrite);
              found:=0;
            end;
          end;
          inc(DwordP);
        end;
        progressbar.stepit;
      end
      else
      for i:=0 to memoryregions do
      begin
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        if actualread>=4 then
        begin
          Dwordp:=pointer(memory);

          for j:=0 to actualread-4 do
          begin
            if DwordP^=DwordValue then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+j;
              inc(found);
              if found=number then
              begin
                if foundvalue3[0]<>dwordvalue then
                begin
                  for k:=0 to number-1 do foundvalue3[k]:=dwordvalue;
                  FoundIsFilled:=true;
                end;

                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempdwordarray:=pointer(foundvalue3switch);
                foundvalue3switch:=pointer(foundvalue3);
                foundvalue3:=pointer(tempdwordarray);

                flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                found:=0;
              end;
            end;
            asm
              inc [DwordP]
            end;
          end;
        end;
        progressbar.stepit;
      end;
      if foundvalue3[0]<>dwordvalue then
      begin
        for k:=0 to found-1 do foundvalue3[k]:=dwordvalue;
        FoundIsFilled:=true;
      end;

      flushthread.datawritten.WaitFor(infinite);
      flushbuffer(addressfile,memoryfile,foundaddress,4*found,foundvalue3,4*found);

//      blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//      blockwrite(Memoryfile,pointer(foundvalue3)^,4*found,actualwrite);
    end;

    if vartype=3 then //float
    begin
      setlength(foundvalue4,number);
      setlength(foundvalue4switch,number);
      SingleValue:=RoundTo(singlevalue,-decim);


      if decim=0 then helpsingle3:=1 else
        helpsingle3:=1/((decim)*10);  //the range for extremerounded scans

      if fastscan then
      for i:=0 to memoryregions do
      begin
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        Singlep:=pointer(memory);

        for k:=1 to (actualread div 4) do
        begin
          check:=(not (isnan(singlep^) or isinfinite(singlep^)));

          if check then
          case roundingtype of
            rounded:
            begin
              helpsingle:=RoundTo(SingleP^,-decim);
              check:=(helpsingle=SingleValue);
            end;

            extremerounded:
            begin
              //if a scan for 1 it scans for    0<x<2
              //if a scan for 1.0 it scans for  0.0<x<1.1
              check:=((SingleP^<(singlevalue+helpsingle3)) and (SingleP^>(singlevalue-helpsingle3)) );
            end;

            truncated:
            begin
              //if a scan for 1 it scans for    1>=x<2
              //if a scan for 1.0 it scans for 1.0>=x<1.10
              check:=((SingleP^<(singlevalue+helpsingle3)) and (singlep^>=singlevalue));
            end;

            else check:=false;
          end;


          if check then
          begin
            foundvalue4[found]:=SingleP^;
            foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(singlep)-dword(memory));
            inc(found);
            if found=number then
            begin
              flushthread.datawritten.WaitFor(infinite);

              tempdwordarray:=pointer(foundaddressswitch);
              foundaddressswitch:=pointer(foundaddress);
              foundaddress:=pointer(tempdwordarray);

              tempsinglearray:=pointer(foundvalue4switch);
              foundvalue4switch:=pointer(foundvalue4);
              foundvalue4:=pointer(tempsinglearray);

              flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//              blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//              blockwrite(Memoryfile,pointer(foundvalue4)^,number*4,actualwrite);
              found:=0;
            end;
          end;
          inc(SingleP);
        end;
        progressbar.stepit;
      end
      else
      for i:=0 to memoryregions do
      begin
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        if actualread>=4 then
        begin
          Singlep:=pointer(memory);
          for j:=0 to actualread-4 do
          begin
            check:=(not (isnan(singlep^) or isinfinite(singlep^)));

            if check then
            case roundingtype of
              rounded:
              begin
                helpsingle:=RoundTo(SingleP^,-decim);
                check:=(helpsingle=SingleValue);
              end;

              extremerounded:
              begin
                //if a scan for 1 it scans for    0<x<2
                //if a scan for 1.0 it scans for  9.9<x<1.10
                check:=((SingleP^<(singlevalue+helpsingle3)) and (SingleP^>(singlevalue-helpsingle3)) );
              end;

              truncated:
              begin
                //if a scan for 1 it scans for    1>=x<2
                //if a scan for 1.0 it scans for 1.0>=x<1.10
                check:=((SingleP^<(singlevalue+helpsingle3)) and (singlep^>=singlevalue));
              end;

              else check:=false;
            end;


            if check then
            begin
              foundvalue4[found]:=SingleP^;
              foundaddress[found]:=Memoryregion[i].BaseAddress+j;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempsinglearray:=pointer(foundvalue4switch);
                foundvalue4switch:=pointer(foundvalue4);
                foundvalue4:=pointer(tempsinglearray);

                flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                found:=0;
              end;
            end;

            asm
              inc [SingleP]
            end;
          end;
        end;
        progressbar.stepit;
      end;

      flushthread.datawritten.WaitFor(infinite);
      flushbuffer(addressfile,memoryfile,foundaddress,4*found,foundvalue4,4*found);
      
//      blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//      blockwrite(Memoryfile,pointer(foundvalue4)^,4*found,actualwrite);
    end;

    if vartype=4 then //double
    begin
      setlength(foundvalue5,number);
      setlength(foundvalue5switch,number);
      DoubleValue:=RoundTo(doublevalue,-decim);

      if decim=0 then helpdouble3:=1 else
        helpdouble3:=1/((decim)*10);  //the range for extremerounded scans


      if fastscan then
      for i:=0 to memoryregions do
      begin
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        Doublep:=pointer(memory);

        for j:=1 to (actualread div 4) do
        begin
          check:=(not (isnan(doublep^) or isinfinite(doublep^)));

          if check then
          case roundingtype of
            rounded:
            begin
              helpdouble:=RoundTo(doubleP^,-decim);
              check:=(helpdouble=doubleValue);
            end;

            extremerounded:
            begin
              //if a scan for 1 it scans for    0<x<2
              //if a scan for 1.0 it scans for  9.9<x<1.10
              check:=((doubleP^<(doublevalue+helpdouble3)) and (doubleP^>(doublevalue-helpdouble3)) );
            end;

            truncated:
            begin
              //if a scan for 1 it scans for    1>=x<2
              //if a scan for 1.0 it scans for 1.0>=x<1.10
              check:=((doubleP^<(doublevalue+helpdouble3)) and (doublep^>=doublevalue));
            end;

            else check:=false;
          end;

          if check then
          begin
            foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(doublep)-dword(memory));
            foundvalue5[found]:=doublep^;
            inc(found);
            if found=number then
            begin
              flushthread.datawritten.WaitFor(infinite);

              tempdwordarray:=pointer(foundaddressswitch);
              foundaddressswitch:=pointer(foundaddress);
              foundaddress:=pointer(tempdwordarray);

              tempdoublearray:=pointer(foundvalue5switch);
              foundvalue5switch:=pointer(foundvalue5);
              foundvalue5:=pointer(tempdoublearray);

              flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//              blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//              blockwrite(Memoryfile,pointer(foundvalue5)^,number*8,actualwrite);
              found:=0;
            end;
          end;
          asm
            add [Doublep],4
          end;
        end;
        progressbar.stepit;
      end
      else
      for i:=0 to memoryregions do
      begin
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        if actualread>=8 then
        begin
          Doublep:=pointer(memory);
          for j:=0 to actualread-8 do
          begin
            check:=(not (isnan(doublep^) or isinfinite(doublep^)));

            if check then
              case roundingtype of
              rounded:
              begin
                helpdouble:=RoundTo(doubleP^,-decim);
                check:=(helpdouble=doubleValue);
               end;

              extremerounded:
              begin
                //if a scan for 1 it scans for    0<x<2
                //if a scan for 1.0 it scans for  9.9<x<1.10
                check:=((doubleP^<(doublevalue+helpdouble3)) and (doubleP^>(doublevalue-helpdouble3)) );
              end;

              truncated:
              begin
                //if a scan for 1 it scans for    1>=x<2
                //if a scan for 1.0 it scans for 1.0>=x<1.10
                check:=((doubleP^<(doublevalue+helpdouble3)) and (doublep^>=doublevalue));
              end;

              else check:=false;
            end;

            if check then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(doublep)-dword(memory));
              foundvalue5[found]:=doublep^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempdoublearray:=pointer(foundvalue5switch);
                foundvalue5switch:=pointer(foundvalue5);
                foundvalue5:=pointer(tempdoublearray);

                flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                found:=0;
              end;
            end;
            asm
              inc [Doublep]
            end;
          end;
          progressbar.stepit;
        end;

      end;
      flushthread.datawritten.WaitFor(infinite);
      flushbuffer(addressfile,memoryfile,foundaddress,4*found,foundvalue5,8*found);

//      blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//      blockwrite(Memoryfile,pointer(foundvalue5)^,8*found,actualwrite);
    end;


    if vartype=5 then  //bit-scan
    begin
      setlength(foundaddressb,number);

      actualwrite:=length(bitscan);
      blockwrite(memoryfile,actualwrite,4,actualwrite);
      blockwrite(memoryfile,bitscan[0],length(bitscan),actualwrite);

      for i:=0 to memoryregions do
      begin
        bittofind:=0;

        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        begin
          if actualread>0 then
          begin
            //check the previous bytes
            if memoryregion[i].ischild then
            begin
              //check the tempbits array for the bits
              k:=9-(nrofbits mod 8); //k=1st bit in tempbit array

              bytep:=pointer(memory);
              for l:=0 to nrofbits-2 do //all bits except for last one. (at the end k should be 0)
              begin
                for j:=0 to nrofbits-2 do //shift left
                  tempbits[j]:=tempbits[j+1];
                tempbits[nrofbits-1]:=getbit(l,bytep^);

                for j:=0 to nrofbits-1 do
                begin
                  if (bitscan[j]<>2) and (tempbits[j]<>bitscan[j]) then
                  begin
                    inc(k);
                    break;
                  end
                  else
                  begin
                    if j=dword(nrofbits)-1 then
                    begin
                      //found it

                      foundaddressb[found].address:=memoryregion[i].BaseAddress-(1+(nrofbits div 8));
                      foundaddressb[found].bit:=k;
                      inc(found);
                      if found=buffersize then
                      begin
                        blockwrite(Addressfile,pointer(foundaddressB)^,found*(sizeof(Tbitaddress)),actualwrite);
                        found:=0;
                      end;
                    end;
                  end;

                end;
              end;
            end;


            bytep:=pointer(memory);
            j:=0;
            k:=0;
            scanbits(found,number,bytep,nrofbits,i,actualread);
          end;
        end;
        progressbar.stepit;
      end;

      blockwrite(Addressfile,pointer(foundaddressB)^,found*(sizeof(Tbitaddress)),actualwrite);
      if (actualwrite<found*(sizeof(Tbitaddress))) then
      begin
        closefile(addressfile);
        closefile(memoryfile);
        freememory;
        raise Exception.Create('There is not enough room on your disk or the magical limit of 2GB has been reached for the temp file. Anyhow the scan has stopped! You should be able to continue scanning with the current temp file. (You''ll just wont have scanned all the memory.'+chr(13)+chr(10)+
                               'The last address stored in the temp file was:'+inttohex(foundaddressb[actualwrite].address,8));
      end;

      closefile(addressfile);

      try
        try
          resulthelper:=tfilestream.Create(CheatEngineDir+'Addresses.tmp',fmopenread,fmsharedenynone);
          result:=(resulthelper.Size-7) div sizeof(Tbitaddress);
        finally
          resulthelper.free;
        end;
      except
        reset(addressfile,1);
        result:=(filesize(addressfile)-7) div sizeof(Tbitaddress);
        closefile(addressfile);
      end;

      reset(addressfile,1);


      closefile(addressfile);
      closefile(memoryfile);
      freememory;
      exit;
    end;

    if vartype=6 then //int64
    begin
      setlength(foundvalue7,number);
      setlength(foundvalue7switch,number);
      foundvalue7[0]:=int64value+1;
      foundvalue7switch[0]:=int64value+1;

      if fastscan then
      for i:=0 to memoryregions do
      begin
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        int64p:=pointer(memory);

        for j:=1 to (actualread div 4) do
        begin
          if int64p^=int64value then
          begin
            foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(int64p)-dword(memory));
            inc(found);
            if found=number then
            begin
              if foundvalue7[0]<>int64value then
              begin
                for k:=0 to number-1 do foundvalue7[k]:=int64value;
                FoundIsFilled:=true;
              end;

              flushthread.datawritten.WaitFor(infinite);

              tempdwordarray:=pointer(foundaddressswitch);
              foundaddressswitch:=pointer(foundaddress);
              foundaddress:=pointer(tempdwordarray);

              tempint64array:=pointer(foundvalue7switch);
              foundvalue7switch:=pointer(foundvalue7);
              foundvalue7:=pointer(tempint64array);

              flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue7switch,8*number);


//              blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//              blockwrite(Memoryfile,pointer(foundvalue7)^,number*8,actualwrite);
              found:=0;
            end;
          end;
          asm
            add [int64p],4
          end;
        end;
        progressbar.stepit;
      end
      else
      for i:=0 to memoryregions do
      begin
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        if actualread>=8 then
        begin
          int64p:=pointer(memory);
          for j:=0 to actualread-8 do
          begin
            if int64p^=int64value then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+j;
              inc(found);
              if found=number then
              begin
                if foundvalue7[0]<>int64value then
                begin
                  for k:=0 to number-1 do foundvalue6[k]:=int64value;
                  FoundIsFilled:=true;
                end;

                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempint64array:=pointer(foundvalue7switch);
                foundvalue7switch:=pointer(foundvalue7);
                foundvalue7:=pointer(tempint64array);

                flushbuffer(addressfile,memoryfile,foundaddressswitch,4*number,foundvalue7switch,8*number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointer(foundvalue7)^,number*8,actualwrite);
                found:=0;
              end;
            end;
            asm
              inc [int64p]
            end;

          end;
        end;
        progressbar.stepit;
      end;

      if foundvalue7[0]<>int64value then
      begin
        for k:=0 to found-1 do
          foundvalue7[k]:=dwordvalue;

        k:=0; //for-loop vairable'k' may be undefined after loop bla bla bla
        FoundIsFilled:=true;
      end;

      flushthread.datawritten.WaitFor(infinite);
      flushbuffer(addressfile,memoryfile,foundaddress,4*found,foundvalue7,8*found);


//      blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//      blockwrite(Memoryfile,pointer(foundvalue7)^,8*found,actualwrite);
    end;

    if vartype=7 then //text
    begin
      scanlength:=length(scanvalue);
      blockwrite(memoryfile,pointer(scanvalue)^,scanlength,actualread);

      for i:=0 to memoryregions do
      begin
        unicode2:=false;

        bytep:=pointer(memory);
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        if actualread>1 then
        begin
          if not memoryregion[i].IsChild then CharToFind:=1; //else continue with the current char...

          for j:=0 to actualread-1 do
          begin
            if (((not extra) and (uppercase(chr(bytep^))=uppercase(scanvalue[CharToFind])))
               or
               (extra and (chr(bytep^)=scanvalue[CharToFind]))

               or (unicode2 and (bytep^=0) )
               ) then
            begin
              if unicode then unicode2:=bytep^<>0;

              if not unicode2 then
                inc(charToFind);

              if CharToFind=scanlength+1 then //found the string
              begin
                if unicode then
                  foundaddress[found]:=Memoryregion[i].BaseAddress+j-(scanlength*2)+1
                else
                  foundaddress[found]:=Memoryregion[i].BaseAddress+j-scanlength+1;

                inc(found);
                if (findonlyone) then
                begin
                  closefile(Addressfile);
                  closefile(memoryfile);
                  firstresult:=Foundaddress[0];
                  result:=1;
                  freememory;
                  exit;
                end else
                if found=number then
                begin
                  found:=0;
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                end;

                CharToFind:=1;
              end;
            end else CharToFind:=1;

            asm
              inc [bytep]
            end;

          end;
        end;
        progressbar.StepIt;
      end;
      blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);

    end;

{
      bytes: array of integer;  //-1=wildcard
      helpstr,helpstr2: string;
}
    if vartype=8 then //array of byte
    begin
      //find out what kind of syntax was used ' ' '-' or ''
      k:=0;
      ConvertStringToBytes(scanvalue,extra,bytes);

      nrofbytes:=length(bytes);
      setlength(foundvalue8,number*nrofbytes);
      blockwrite(memoryfile,nrofbytes,4,actualwrite); //save how many bytes each record is

      //bytes array now holds the bytes to scan -1=wildcard
      for i:=0 to memoryregions do
      begin
        bytep:=pointer(memory);
        readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
        if actualread>nrofbytes then
        begin
          if not memoryregion[i].IsChild then k:=0;

          for j:=0 to actualread-nrofbytes do
          begin
            if bytes[k]=-1 then
            begin
              inc(k);
            end else
            begin
              if bytep^=byte(bytes[k]) then inc(k) else k:=0;
            end;


            if k=length(bytes) then //found one
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+j-nrofbytes+1;
              copymemory(pointer(@foundvalue8[found*nrofbytes]),pointer(dword(bytep)-nrofbytes+1),nrofbytes);
              k:=0;

              inc(found);

              if (findonlyone) then
              begin
                closefile(Addressfile);
                closefile(memoryfile);
                firstresult:=Foundaddress[0];
                result:=1;
                freememory;
                exit;
              end else
              if found=number then
              begin
                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                blockwrite(Memoryfile,pointeR(foundvalue8)^,nrofbytes*number,actualwrite);
                found:=0;
              end;
            end;

            inc(bytep);
          end;
        end;
        progressbar.stepit;
      end;
      blockwrite(Addressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(Memoryfile,pointeR(foundvalue8)^,nrofbytes*found,actualwrite);
    end;


    if vartype=9 then //all
    begin





    end;
  end;


  finishflushing;

  closefile(Addressfile);  //the memory regions have been saved
  closefile(memoryfile);

  try
    try
      resulthelper:=tfilestream.Create(CheatEngineDir+'Addresses.tmp',fmopenread,fmsharedenynone);
      result:=(resulthelper.Size-7) div 4;
    finally
      resulthelper.free;
    end;
  except
    reset(addressfile,1);
    result:=(filesize(addressfile)-7) div 4;
    closefile(addressfile);
  end;

  except
    on EOutOfMemory do
      begin
        //not enough free memory
        freememory;
        raise exception.Create('Not enough memory free to scan');
        exit;
      end;
  end;

  freememory;
end;


function NextScan2(scantext,scantext2:string; ScanWay, ValType: Integer; roundingtype:tfloatscan; option:boolean; ProgressBar: TProgressbar;fastscan:boolean;unicode,percentage: boolean): dword;
{$ifndef standalonetrainer}
{$ifndef netclient}
var
    check: boolean;
    datatype: String[6];

    actualread,actualwrite: dword;

    done: Boolean;
    Found: dword;


    i: integer;
    k,l: integer; //dword;
    j: integer;
    j2: boolean;

    ByteScanned: Byte;
    BytesScanned: array of byte;
    BytesScannedStart: dword;

    WordScanned: Word;
    WordsScanned: array of word;
    WordsScannedStart: dword;

    DWordScanned: Dword;
    DwordsScanned: array of dword;
    DwordsScannedStart: dword;
    
    DwordScanned2: Dword;
    SingleScanned: single;
    SinglesScanned: array of single;
    SinglesScannedStart: dword;
    
    DoubleScanned: double;
    DoublesScanned: Array of double;
    DoublesScannedStart: dword;

    Int64Scanned: Int64;
    Int64sScanned: array of int64;
    Int64sScannedStart: dword;

    ByteValue,ByteValue2: Byte;
    WordValue,WordValue2: Word;
    DWordValue,DWordValue2: Dword;
    Int64Value,Int64Value2: Int64;

    SingleValue,singlevalue2: Single;
    doubleValue,doublevalue2: Double;

    helpbyte: byte;
    helpword,helpword2:   word;
    helpdword,helpdword2,helpdword3:  dword;
    helpsingle,helpsingle2,helpsingle3: Single;
    helpdouble,helpdouble2,helpdouble3: Double;
    hi64,hi642: int64;

    Total: Dword;
    ToRead: Dword;

    decim: integer;
    decimhelp: Integer;

    StrB:Pchar;
    strc:pwidechar;

    x: Integer;
    maxi: dword;

    Memp: ^byte;
    Memp2: ^byte;

    ByteP: ^byte;
    ByteP2: ^byte;
    ByteP3: ^byte;

    bp1: ^byte;
    bp2: ^byte;

    hs1: ^single;
    hs2: ^single;

    hd1: ^double;
    hd2: ^double;

    WordP: ^word;
    wordp2: ^word;
    wordp3: ^word;

    DwordP: ^dword;
    DwordP2: ^dword;
    Dwordp3: ^dword;

    SingleP: ^Single;
    SingleP2: ^Single;

    DoubleP: ^Double;
    DoubleP2: ^Double;

    Int64P: ^Int64;
    Int64P2: ^Int64;
    Int64P3: ^int64;

    number: dword;

    FoundIsFilled: boolean;
    ok:boolean;
    extra: boolean;

    //array of byte...
    nrofbytes: dword;

    startbit: integer;

    resulthelper: tfilestream;
    unicode2: boolean;
    ws: widestring;

    found2: dword;

    {$ifndef netserver}
    FSHandler: TFirstScanHandler;
    {$endif}

{$endif}
{$endif}
begin
{$ifndef standalonetrainer}
{$ifndef netclient}

  //initialize
  advanced:=false;
  memory2:=nil;
  bytevalue:=0;
  wordvalue:=0;
  dwordvalue:=0;
  singlevalue:=0;
  doublevalue:=0;
  FoundIsFilled:=False;

  if (scanway=Exact_value) or (scanway=Increased_value_by) or (scanway=decreased_value_by) or (scanway=biggerThan) or (scanway=SmallerThan) or (scanway=valuebetween) then
  begin
    if valtype=0 then
    begin
      if option then val('$'+scantext,bytevalue,i)
                             else val(scantext,bytevalue,i);
      if i>0 then raise Exception.Create(scantext+' is not a valid byte notation');
    end;

    if valtype=1 then
    begin
      if option then val('$'+scantext,wordvalue,i)
                             else val(scantext,wordvalue,i);
      if i>0 then raise Exception.Create(scantext+' is not an valid 2-byte notation');
    end;

    if valtype=2 then
    begin
      if option then val('$'+scantext,dwordvalue,i)
                             else val(scantext,dwordvalue,i);
      if i>0 then raise Exception.Create(scantext+' is not an valid 4-byte notation');
    end;

    if valtype=3 then //doesnt have the hexadecimal option
    begin
      val(scantext,singlevalue,i);
      if i>0 then raise Exception.Create(scantext+' is not a valid floating point notation');
    end;

    if valtype=4 then //same as 3
    begin
      val(scantext,doublevalue,i);
      if i>0 then raise Exception.create(scantext+' is not a valid floating point notation');
    end;

    if valtype=5 then
    begin
      if pos('-',scantext)>0 then raise exception.create(scantext+' is not a valid notation!');
      if not option then
      begin
        setlength(bitscan,length(scantext));
        j:=0;
        for i:=length(scantext) downto 1 do
        begin
          if scantext[i]='0' then bitscan[length(scantext)-i]:=0
          else
          if scantext[i]='1' then bitscan[length(scantext)-i]:=1
          else
          if scantext[i]='?' then bitscan[length(scantext)-i]:=2
          else
          if scantext[i]='*' then bitscan[length(scantext)-i]:=2
          else
          begin
            freememory;
            raise Exception.create(scantext+' is not a valid binary notation');
          end;
          inc(j);
        end;
      end
      else
      begin
        try
          dwordvalue:=StrToInt(scantext);
          i:=0;
          while dwordvalue>0 do
          begin
            setlength(bitscan,i+1);

            if (dwordvalue mod 2)>0 then bitscan[i]:=1
                                    else bitscan[i]:=0;

            inc(i);
            dwordvalue:=dwordvalue div 2;
          end;
        except
          raise Exception.create(scantext+' is not a valid notation');
        end;
      end;

      nrofbits:=length(bitscan);
    end;

    if valtype=6 then
    begin
      if option then val('$'+scantext,int64value,i) else
                                  val(scantext,Int64value,i);
      if i>0 then raise Exception.Create(scantext+' is not an valid 8-byte notation');
    end;
  end;

  if scanway=valuebetween then
  begin
    //same for the 2nd one
    //if (scanway=Exact_value) or (scanway=Increased_value_by) or (scanway=decreased_value_by) or (scanway=biggerThan) or (scanway=SmallerThan) or (scanway=valuebetween) then
    begin
      if valtype=0 then
      begin
        if option then val('$'+scantext2,bytevalue2,i)
                               else val(scantext2,bytevalue2,i);
        if i>0 then raise Exception.Create(scantext+' is not a valid byte notation');
      end;

      if valtype=1 then
      begin
        if option then val('$'+scantext2,wordvalue2,i)
                               else val(scantext2,wordvalue2,i);
        if i>0 then raise Exception.Create(scantext2+' is not an valid 2-byte notation');
      end;

      if valtype=2 then
      begin
        if option then val('$'+scantext2,dwordvalue2,i)
                               else val(scantext2,dwordvalue2,i);
        if i>0 then raise Exception.Create(scantext2+' is not an valid 4-byte notation');
      end;

      if valtype=3 then //doesnt have the hexadecimal option
      begin
        val(scantext2,singlevalue2,i);
        if i>0 then raise Exception.Create(scantext2+' is not a valid floating point notation');
      end;

      if valtype=4 then //same as 3
      begin
        val(scantext2,doublevalue2,i);
        if i>0 then raise Exception.create(scantext2+' is not a valid floating point notation');
      end;

      if valtype=6 then
      begin
        if option then val('$'+scantext2,int64value2,i) else
                                    val(scantext2,Int64value2,i);
        if i>0 then raise Exception.Create(scantext2+' is not an valid 8-byte notation');
      end;
    end;


  end;




  found:=0;
  done:=false;

  decim:=0;

  val(scantext,decimhelp,i);
//  i:=pos('.',scantext);
  if i>0 then decim:=length(scantext)-i;


  assignfile(AddressFile,Cheatenginedir+'ADDRESSES.TMP');
  assignfile(Memoryfile,cheatenginedir+'MEMORY.TMP');
  assignfile(newaddressfile,cheatenginedir+'ADDRESSES2.TMP');
  assignfile(NewMemoryFile,cheatenginedir+'MEMORY2.TMP');
  reset(Addressfile,1);
  reset(memoryfile,1);
  rewrite(NewAddressFile,1);
  rewrite(NewMemoryfile,1);
  //-----
  datatype:='NORMAL';
  blockwrite(NewAddressfile,datatype,sizeof(datatype));
  blockread(Addressfile,datatype,sizeof(datatype));
  if datatype='REGION' then
  begin
    //last time we did a region scan
    //First fill in the array with the addresses and sizes!!!

    number:=buffersize;
    setlength(foundaddress,number);
    setlength(foundaddressswitch,number);

    progressbar.Max:=Memoryregions+1;

    //get total size
    maxi:=0;
    For i:=0 to Memoryregions do
    begin
      if maxi<memoryregion[i].MemorySize then maxi:=memoryregion[i].MemorySize;
    end;

    getmem(memory2,maxi);
//    setlength(memory2,maxi+1);

    //find max size of a memoryblock to scan

    if valtype=0 then
    begin
      //It's a Byte Scan
      setlength(foundvalue1,number);
      setlength(foundvalue1switch,number);
      foundvalue1[0]:=bytevalue+1;
      foundvalue1switch[0]:=bytevalue+1;

      if scanway=Exact_value then  //just a copy of "get memoryranges and scan"
      begin
        //It's an Exact value scan
        for i:=0 to memoryregions do
        begin
          bytep:=pointer(memory);
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          begin
            if actualread>0 then
            for j:=0 to actualread-1 do
            begin
              if bytep^=ByteValue then
              begin
                foundaddress[found]:=Memoryregion[i].BaseAddress+j;
                inc(found);

                if found=number then
                begin
                  if foundvalue1[0]<>bytevalue then
                  begin
                    for k:=0 to number-1 do foundvalue1[k]:=bytevalue;
                    FoundIsFilled:=true;
                  end;

                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempbytearray:=pointer(foundvalue1switch);
                  foundvalue1switch:=pointer(foundvalue1);
                  foundvalue1:=pointer(tempbytearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue1switch,number);


//                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(newMemoryfile,pointer(foundvalue1)^,number,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc [bytep]
              end;
            end;
          end;
          progressbar.stepit;
        end;

        if foundvalue1[0]<>bytevalue then
        begin
          for k:=0 to found-1 do foundvalue1[k]:=bytevalue;
          FoundIsFilled:=true;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue1,found);

//        blockwrite(newAddressfile,pointer(foundaddress)^,found*4,actualwrite);
//        blockwrite(newMemoryfile,pointer(foundvalue1)^,found,actualwrite);
        found:=0;
      end;

      if scanway=BiggerThan then
      begin
        //It's an increased value by scan
        for i:=0 to memoryregions do
        begin
          bytep:=pointer(memory);
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          begin
            if actualread>0 then
            for j:=0 to actualread-1 do
            begin
              if bytep^>ByteValue then
              begin
                foundaddress[found]:=Memoryregion[i].BaseAddress+j;
                foundvalue1[found]:=bytep^;
                inc(found);

                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempbytearray:=pointer(foundvalue1switch);
                  foundvalue1switch:=pointer(foundvalue1);
                  foundvalue1:=pointer(tempbytearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue1switch,number);

//                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(newMemoryfile,pointer(foundvalue1)^,number,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc [bytep]
              end;
            end;
          end;
          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue1,found);

//        blockwrite(newAddressfile,pointer(foundaddress)^,found*4,actualwrite);
//        blockwrite(newMemoryfile,pointer(foundvalue1)^,found,actualwrite);
        found:=0;
      end;

      if scanway=SmallerThan then
      begin
        //It's an decreased value by scan
        for i:=0 to memoryregions do
        begin
          bytep:=pointer(memory);
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          begin
            if actualread>0 then
            for j:=0 to actualread-1 do
            begin
              if bytep^<ByteValue then
              begin
                foundaddress[found]:=Memoryregion[i].BaseAddress+j;
                foundvalue1[found]:=bytep^;
                inc(found);

                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempbytearray:=pointer(foundvalue1switch);
                  foundvalue1switch:=pointer(foundvalue1);
                  foundvalue1:=pointer(tempbytearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue1switch,number);

//                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(newMemoryfile,pointer(foundvalue1)^,number,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc [bytep]
              end;
            end;
          end;
          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue1,found);

//        blockwrite(newAddressfile,pointer(foundaddress)^,found*4,actualwrite);
//        blockwrite(newMemoryfile,pointer(foundvalue1)^,found,actualwrite);
        found:=0;
      end;

      if scanway=ValueBetween then
      begin
        //It's an range scan
        for i:=0 to memoryregions do
        begin
          bytep:=pointer(memory);
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          begin
            if actualread>0 then
            for j:=0 to actualread-1 do
            begin
              if (bytep^>=ByteValue) and (bytep^<=Bytevalue2) then
              begin
                foundaddress[found]:=Memoryregion[i].BaseAddress+j;
                foundvalue1[found]:=bytep^;
                inc(found);

                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempbytearray:=pointer(foundvalue1switch);
                  foundvalue1switch:=pointer(foundvalue1);
                  foundvalue1:=pointer(tempbytearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue1switch,number);


//                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(newMemoryfile,pointer(foundvalue1)^,number,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc [bytep]
              end;
            end;
          end;
          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);

        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue1,found);

//        blockwrite(newAddressfile,pointer(foundaddress)^,found*4,actualwrite);
//        blockwrite(newMemoryfile,pointer(foundvalue1)^,found,actualwrite);
        found:=0;
      end;

      if scanway=Increased_Value then
      begin
        //It's an Increased value scan
        bytep2:=pointer(memory);

        for j:=0 to MemoryRegions do
        begin
          bytep:=pointer(bytep2);
          bytep3:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep2,memoryregion[j].MemorySize);

          if actualread>0 then
          for i:=0 to actualread-1 do
          begin
            if bytep3^>bytep^ then
            begin
              foundaddress[found]:=memoryregion[j].BaseAddress+i;
              foundvalue1[found]:=bytep3^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempbytearray:=pointer(foundvalue1switch);
                foundvalue1switch:=pointer(foundvalue1);
                foundvalue1:=pointer(tempbytearray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue1switch,number);

//                blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(NewMemoryfile,pointer(foundvalue1)^,number,actualwrite);
                found:=0;
              end;
            end;

            asm
              inc [bytep]
              inc [bytep3]
            end;
          end;
          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);

        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue1,found);

//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue1)^,found,actualwrite);
        found:=0;
      end;

      if scanway=Increased_Value_by then
      begin
        //It's an Increased value by scan
        bytep2:=pointer(memory);

        for j:=0 to MemoryRegions do
        begin
          bytep:=pointer(bytep2);
          bytep3:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep2,memoryregion[j].MemorySize);

          if actualread>0 then
          begin
            if percentage then
            begin
              for i:=0 to actualread-1 do
              begin
                if bytep3^>=bytep^+trunc(bytep^*(bytevalue/100)) then
                begin
                  foundaddress[found]:=memoryregion[j].BaseAddress+i;
                  foundvalue1[found]:=bytep3^;
                  inc(found);
                  if found=number then
                  begin
                    flushthread.datawritten.WaitFor(infinite);

                    tempdwordarray:=pointer(foundaddressswitch);
                    foundaddressswitch:=pointer(foundaddress);
                    foundaddress:=pointer(tempdwordarray);

                    tempbytearray:=pointer(foundvalue1switch);
                    foundvalue1switch:=pointer(foundvalue1);
                    foundvalue1:=pointer(tempbytearray);

                    flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue1switch,number);

//                    blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                    blockwrite(NewMemoryfile,pointer(foundvalue1)^,number,actualwrite);
                    found:=0;
                  end;
                end;
                asm
                  inc [bytep]
                  inc [bytep3]
                end;
              end;
            end
            else
            begin
              for i:=0 to actualread-1 do
              begin
                if bytep3^=bytep^+bytevalue then
                begin
                  foundaddress[found]:=memoryregion[j].BaseAddress+i;
                  foundvalue1[found]:=bytep3^;
                  inc(found);
                  if found=number then
                  begin
                    flushthread.datawritten.WaitFor(infinite);

                    tempdwordarray:=pointer(foundaddressswitch);
                    foundaddressswitch:=pointer(foundaddress);
                    foundaddress:=pointer(tempdwordarray);

                    tempbytearray:=pointer(foundvalue1switch);
                    foundvalue1switch:=pointer(foundvalue1);
                    foundvalue1:=pointer(tempbytearray);

                    flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue1switch,number);

//                    blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                    blockwrite(NewMemoryfile,pointer(foundvalue1)^,number,actualwrite);
                    found:=0;
                  end;
                end;
                asm
                  inc [bytep]
                  inc [bytep3]
                end;
              end;
            end;
          end;

          progressbar.stepit;
        end;
        flushthread.datawritten.WaitFor(infinite);

        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue1,found);

//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue1)^,found,actualwrite);
        found:=0;
      end;

      if scanway=Decreased_Value then
      begin
        //It's an Decreased value scan
        bytep2:=pointer(memory);

        for j:=0 to MemoryRegions do
        begin
          bytep:=pointer(bytep2);
          bytep3:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep2,memoryregion[j].MemorySize);

          if actualread>0 then
          for i:=0 to actualread-1 do
          begin
            if bytep3^<bytep^ then
            begin
              foundaddress[found]:=memoryregion[j].BaseAddress+i;
              foundvalue1[found]:=bytep3^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempbytearray:=pointer(foundvalue1switch);
                foundvalue1switch:=pointer(foundvalue1);
                foundvalue1:=pointer(tempbytearray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue1switch,number);

//                blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(NewMemoryfile,pointer(foundvalue1)^,number,actualwrite);
                found:=0;
              end;
            end;
            asm
              inc [bytep]
              inc [bytep3]
            end;
          end;
          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue1,found);

//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue1)^,found,actualwrite);
        found:=0;
      end;

      if scanway=Decreased_Value_by then
      begin
        //It's an Decreased value by scan
        bytep2:=pointer(memory);


        for j:=0 to MemoryRegions do
        begin
          bytep:=pointer(bytep2);
          bytep3:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep2,memoryregion[j].MemorySize);

          if actualread>0 then
          begin
            if percentage then
            begin
              for i:=0 to actualread-1 do
              begin
                if bytep3^<=bytep^-trunc(bytep^*(bytevalue/100)) then
                begin
                  foundaddress[found]:=memoryregion[j].BaseAddress+i;
                  foundvalue1[found]:=bytep3^;
                  inc(found);
                  if found=number then
                  begin
                    flushthread.datawritten.WaitFor(infinite);

                    tempdwordarray:=pointer(foundaddressswitch);
                    foundaddressswitch:=pointer(foundaddress);
                    foundaddress:=pointer(tempdwordarray);

                    tempbytearray:=pointer(foundvalue1switch);
                    foundvalue1switch:=pointer(foundvalue1);
                    foundvalue1:=pointer(tempbytearray);

                    flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue1switch,number);


//                    blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                    blockwrite(NewMemoryfile,pointer(foundvalue1)^,number,actualwrite);
                    found:=0;
                  end;
                end;
                asm
                  inc [bytep]
                  inc [bytep3]
                end;
              end;
            end
            else
            begin
              for i:=0 to actualread-1 do
              begin
                if bytep3^=bytep^-bytevalue then
                begin
                  foundaddress[found]:=memoryregion[j].BaseAddress+i;
                  foundvalue1[found]:=bytep3^;
                  inc(found);
                  if found=number then
                  begin
                    flushthread.datawritten.WaitFor(infinite);

                    tempdwordarray:=pointer(foundaddressswitch);
                    foundaddressswitch:=pointer(foundaddress);
                    foundaddress:=pointer(tempdwordarray);

                    tempbytearray:=pointer(foundvalue1switch);
                    foundvalue1switch:=pointer(foundvalue1);
                    foundvalue1:=pointer(tempbytearray);

                    flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue1switch,number);



//                    blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                    blockwrite(NewMemoryfile,pointer(foundvalue1)^,number,actualwrite);
                    found:=0;
                  end;
                end;
                asm
                  inc [bytep]
                  inc [bytep3]
                end;
              end;
            end;
          end;
          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue1,found);


//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue1)^,found,actualwrite);
        found:=0;
      end;

      if scanway=Changed_Value then
      begin
        //It's an Decreased value scan
        bytep2:=pointer(memory);

        for j:=0 to MemoryRegions do
        begin
          bytep:=pointer(bytep2);
          bytep3:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep2,memoryregion[j].MemorySize);

          if actualread>0 then
          for i:=0 to actualread-1 do
          begin
            if bytep3^<>bytep^ then
            begin
              foundaddress[found]:=memoryregion[j].BaseAddress+i;
              foundvalue1[found]:=bytep3^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempbytearray:=pointer(foundvalue1switch);
                foundvalue1switch:=pointer(foundvalue1);
                foundvalue1:=pointer(tempbytearray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue1switch,number);

//                blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(NewMemoryfile,pointer(foundvalue1)^,number,actualwrite);
                found:=0;
              end;
            end;
            asm
              inc [bytep]
              inc [bytep3]
            end;
          end;
          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue1,found);

//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue1)^,found,actualwrite);
        found:=0;
      end;


      if scanway=UnChanged_value then
      begin
        //It's an Unchanged value scan

        bytep2:=pointeR(memory);

        for j:=0 to MemoryRegions do
        begin
          bytep:=pointer(bytep2);
          bytep3:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep2,memoryregion[j].MemorySize);

          if actualread>0 then
          for i:=0 to actualread-1 do
          begin
            if bytep3^=bytep^ then
            begin
              foundaddress[found]:=memoryregion[j].BaseAddress+i;
              foundvalue1[found]:=bytep3^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempbytearray:=pointer(foundvalue1switch);
                foundvalue1switch:=pointer(foundvalue1);
                foundvalue1:=pointer(tempbytearray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue1switch,number);

//                blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(NewMemoryfile,pointer(foundvalue1)^,number,actualwrite);
                found:=0;
              end;
            end;
            asm
              inc [bytep]
              inc [bytep3]
            end;
          end;
          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue1,found);

//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue1)^,found,actualwrite);
        found:=0;
      end;
    end;

    if valtype=1 then
    begin
      //It's a Word Scan
      setlength(foundvalue2,number);
      setlength(foundvalue2switch,number);
      foundvalue2[0]:=wordvalue+1;
      foundvalue2switch[0]:=wordvalue+1;

      if scanway=Exact_value then
      begin
        //It's an Exact value scan

        if fastscan then
        for i:=0 to memoryregions do
        begin
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          wordp:=pointer(Memory);
          for j:=1 to (actualread div 2) do
          begin
            if WordP^=WordValue then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(wordp)-dword(memory));
              inc(found);
              if found=number then
              begin
                if foundvalue2[0]<>wordvalue then
                begin
                  for k:=0 to number-1 do foundvalue2[k]:=wordvalue;
                  FoundIsFilled:=true;
                end;

                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempwordarray:=pointer(foundvalue2switch);
                foundvalue2switch:=pointer(foundvalue2);
                foundvalue2:=pointer(tempwordarray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);



//                blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(newMemoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                found:=0;
              end;
            end;
            inc(Wordp);
          end;
          progressbar.stepit;
        end
        else
        for i:=0 to memoryregions do
        begin
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          if actualread>=2 then
          begin
            wordp:=pointer(Memory);
            for j:=0 to actualread-2 do
            begin
              if WordP^=WordValue then
              begin
                foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(wordp)-dword(memory));
                inc(found);
                if found=number then
                begin
                  if foundvalue2[0]<>wordvalue then
                  begin
                    for k:=0 to number-1 do foundvalue2[k]:=wordvalue;
                    FoundIsFilled:=true;
                  end;


                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempwordarray:=pointer(foundvalue2switch);
                  foundvalue2switch:=pointer(foundvalue2);
                  foundvalue2:=pointer(tempwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);


//                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(newMemoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                  found:=0;
                end;
              end;

              asm
                inc [Wordp];
              end;

            end;
          end;
          progressbar.stepit;
        end;
        if foundvalue2[0]<>wordvalue then
        begin
          for k:=0 to found-1 do foundvalue2[k]:=wordvalue;
          FoundIsFilled:=true;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue2,2*found);



//        blockwrite(newAddressfile,pointer(foundaddress)^,found*4,actualwrite);
//        blockwrite(newMemoryfile,pointer(foundvalue2)^,2*found,actualwrite);
        found:=0;
      end;

      if scanway=BiggerThan then
      begin
        //bigger than
        if fastscan then
        for i:=0 to memoryregions do
        begin
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          wordp:=pointer(Memory);

          for j:=1 to (actualread div 2) do
          begin
            if WordP^>WordValue then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(wordp)-dword(memory));
              foundvalue2[found]:=WordP^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempwordarray:=pointer(foundvalue2switch);
                foundvalue2switch:=pointer(foundvalue2);
                foundvalue2:=pointer(tempwordarray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

//                blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(newMemoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                found:=0;
              end;
            end;

            inc(Wordp);
          end;

          progressbar.stepit;
        end
        else
        for i:=0 to memoryregions do
        begin
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          if actualread>1 then
          begin
            wordp:=pointer(Memory);
            for j:=0 to actualread-2 do
            begin
              if WordP^>WordValue then
              begin
                foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(wordp)-dword(memory));
                foundvalue2[found]:=WordP^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempwordarray:=pointer(foundvalue2switch);
                  foundvalue2switch:=pointer(foundvalue2);
                  foundvalue2:=pointer(tempwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

//                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(newMemoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                  found:=0;
                end;
              end;

              asm
                inc [Wordp];
              end;
            end;

            progressbar.stepit;
          end;
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue2,2*found);


//        blockwrite(newAddressfile,pointer(foundaddress)^,found*4,actualwrite);
//        blockwrite(newMemoryfile,pointer(foundvalue2)^,2*found,actualwrite);
        found:=0;
      end;

      if scanway=Smallerthan then
      begin
        //It's an smaller than... scan
        if fastscan then
        for i:=0 to memoryregions do
        begin
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          wordp:=pointer(Memory);

          for j:=1 to (actualread div 2) do
          begin
            if WordP^<WordValue then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(wordp)-dword(memory));
              foundvalue2[found]:=WordP^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempwordarray:=pointer(foundvalue2switch);
                foundvalue2switch:=pointer(foundvalue2);
                foundvalue2:=pointer(tempwordarray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

//                blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(newMemoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                found:=0;
              end;
            end;
            inc (Wordp);
          end;
          progressbar.stepit;
        end
        else
        for i:=0 to memoryregions do
        begin
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          if actualread>1 then
          begin
            wordp:=pointer(Memory);

            for j:=0 to actualread-2 do
            begin
              if WordP^<WordValue then
              begin
                foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(wordp)-dword(memory));
                foundvalue2[found]:=WordP^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempwordarray:=pointer(foundvalue2switch);
                  foundvalue2switch:=pointer(foundvalue2);
                  foundvalue2:=pointer(tempwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

//                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(newMemoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                  found:=0;
                end;
              end;

              asm
                inc [Wordp];
              end;
            end;
          end;
          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue2,2*found);
        
//        blockwrite(newAddressfile,pointer(foundaddress)^,found*4,actualwrite);
//        blockwrite(newMemoryfile,pointer(foundvalue2)^,2*found,actualwrite);
        found:=0;
      end;

      if scanway=ValueBetween then
      begin
        //It's an smaller than... scan
        if fastscan then
        for i:=0 to memoryregions do
        begin
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          wordp:=pointer(Memory);

          for j:=1 to (actualread div 2) do
          begin
            if (WordP^>=WordValue) and (WordP^<=Wordvalue2) then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(wordp)-dword(memory));
              foundvalue2[found]:=WordP^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempwordarray:=pointer(foundvalue2switch);
                foundvalue2switch:=pointer(foundvalue2);
                foundvalue2:=pointer(tempwordarray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

//                blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(newMemoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                found:=0;
              end;
            end;
            inc (Wordp);
          end;
          progressbar.stepit;
        end
        else
        for i:=0 to memoryregions do
        begin
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          if actualread>1 then
          begin
            wordp:=pointer(Memory);

            for j:=0 to actualread-2 do
            begin
              if (WordP^>=WordValue) and (Wordp^<=Wordvalue2) then
              begin
                foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(wordp)-dword(memory));
                foundvalue2[found]:=WordP^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempwordarray:=pointer(foundvalue2switch);
                  foundvalue2switch:=pointer(foundvalue2);
                  foundvalue2:=pointer(tempwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

//                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(newMemoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                  found:=0;
                end;
              end;

              asm
                inc [Wordp];
              end;
            end;
          end;
          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue2,2*found);
        
//        blockwrite(newAddressfile,pointer(foundaddress)^,found*4,actualwrite);
//        blockwrite(newMemoryfile,pointer(foundvalue2)^,2*found,actualwrite);
        found:=0;
      end;

      if scanway=Increased_Value then
      begin
        bytep:=pointer(memory);

        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          wordp:=pointer(bytep);    //wordp points to the first element of the old memory region
          wordp2:=pointer(memory2); //wordp2 points to the start of the first element of the new memory region
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          for i:=1 to (actualread div 2) do
          begin
            if wordp2^>wordp^ then
            begin
              foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(wordp)-dword(bytep));
              foundvalue2[found]:=wordp2^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempwordarray:=pointer(foundvalue2switch);
                foundvalue2switch:=pointer(foundvalue2);
                foundvalue2:=pointer(tempwordarray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

//                blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(NewMemoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                found:=0;
              end; {if found}
            end; {if wordp2>...}
            inc(wordp);
            inc(wordp2);
          end; {if actualread>)}

          inc(bytep,memoryregion[j].MemorySize);
          progressbar.stepit;
        end
        else
        for j:=0 to MemoryRegions do
        begin
          wordp:=pointer(bytep);    //wordp points to the first element of the old memory region
          wordp2:=pointer(memory2); //wordp2 points to the start of the first element of the new memory region
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);
          if actualread>=2 then
          begin
            for i:=0 to actualread-2 do
            begin
              if wordp2^>wordp^ then
              begin
                foundaddress[found]:=memoryregion[j].BaseAddress+i;
                foundvalue2[found]:=wordp2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempwordarray:=pointer(foundvalue2switch);
                  foundvalue2switch:=pointer(foundvalue2);
                  foundvalue2:=pointer(tempwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                  found:=0;
                end; {if found}
              end; {if wordp2>...}

              asm
                inc [wordp]
                inc [wordp2]
              end; {asm}
            end; {for}
          end; {if actualread>)}

          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue2,2*found);

//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue2)^,found*2,actualwrite);
        found:=0;
      end;


      if scanway=Increased_Value_by then
      begin
        bytep:=pointer(memory);

        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          wordp:=pointer(bytep);    //wordp points to the first element of the old memory region
          wordp2:=pointer(memory2); //wordp2 points to the start of the first element of the new memory region
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          if percentage then
          begin
            for i:=1 to (actualread div 2) do
            begin
              if wordp2^>=wordp^+trunc((wordp^*(wordvalue/100))) then
              begin
                foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(wordp)-dword(bytep));
                foundvalue2[found]:=wordp2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempwordarray:=pointer(foundvalue2switch);
                  foundvalue2switch:=pointer(foundvalue2);
                  foundvalue2:=pointer(tempwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);


//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                  found:=0;
                end; {if found}
              end; {if wordp2>...}

              inc(wordp);
              inc(wordp2);
            end;
          end
          else
          begin
            for i:=1 to (actualread div 2) do
            begin

              if wordp2^=wordp^+wordvalue then
              begin
                foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(wordp)-dword(bytep));
                foundvalue2[found]:=wordp2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempwordarray:=pointer(foundvalue2switch);
                  foundvalue2switch:=pointer(foundvalue2);
                  foundvalue2:=pointer(tempwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);


//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                  found:=0;
                end; {if found}
              end; {if wordp2>...}

              inc(wordp);
              inc(wordp2);
            end;
          end;

          inc(bytep,memoryregion[j].MemorySize);
          progressbar.stepit;
        end
        else
        for j:=0 to MemoryRegions do
        begin
          wordp:=pointer(bytep);    //wordp points to the first element of the old memory region
          wordp2:=pointer(memory2); //wordp2 points to the start of the first element of the new memory region
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);

          if actualread>0 then
          begin
            if percentage then
            begin
              for i:=0 to actualread-2 do
              begin
                if wordp2^>=wordp^+trunc((wordp^*(wordvalue/100))) then
                begin
                  foundaddress[found]:=memoryregion[j].BaseAddress+i;
                  foundvalue2[found]:=wordp2^;
                  inc(found);
                  if found=number then
                  begin
                    flushthread.datawritten.WaitFor(infinite);

                    tempdwordarray:=pointer(foundaddressswitch);
                    foundaddressswitch:=pointer(foundaddress);
                    foundaddress:=pointer(tempdwordarray);

                    tempwordarray:=pointer(foundvalue2switch);
                    foundvalue2switch:=pointer(foundvalue2);
                    foundvalue2:=pointer(tempwordarray);

                    flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);


//                    blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                    blockwrite(NewMemoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                    found:=0;
                  end; {if found}
                end; {if wordp2>...}

                asm
                  inc [wordp]
                  inc [wordp2]
                end; {asm}
              end; {for}

            end
            else
            begin
              for i:=0 to actualread-2 do
              begin
                if wordp2^=wordp^+wordvalue then
                begin
                  foundaddress[found]:=memoryregion[j].BaseAddress+i;
                  foundvalue2[found]:=wordp2^;
                  inc(found);
                  if found=number then
                  begin
                    flushthread.datawritten.WaitFor(infinite);

                    tempdwordarray:=pointer(foundaddressswitch);
                    foundaddressswitch:=pointer(foundaddress);
                    foundaddress:=pointer(tempdwordarray);

                    tempwordarray:=pointer(foundvalue2switch);
                    foundvalue2switch:=pointer(foundvalue2);
                    foundvalue2:=pointer(tempwordarray);

                    flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

                  
//                    blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                    blockwrite(NewMemoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                    found:=0;
                  end; {if found}
                end; {if wordp2>...}

                asm
                  inc [wordp]
                  inc [wordp2]
                end; {asm}
              end; {for}

            end;

          end; {if actualread>)}


          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue2,2*found);
        
//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue2)^,found*2,actualwrite);
        found:=0;
      end;

      if scanway=decreased_Value then
      begin
        bytep:=pointer(memory);
        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          wordp:=pointer(bytep);    //wordp points to the first element of the old memory region
          wordp2:=pointer(memory2); //wordp2 points to the start of the first element of the new memory region
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          for i:=1 to (actualread div 2) do
          begin
            if wordp2^<wordp^ then
            begin
              foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(wordp)-dword(bytep));
              foundvalue2[found]:=wordp2^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempwordarray:=pointer(foundvalue2switch);
                foundvalue2switch:=pointer(foundvalue2);
                foundvalue2:=pointer(tempwordarray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

//                blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(NewMemoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                found:=0;
              end; {if found}
            end; {if wordp2>...}
            inc(wordp);
            inc(wordp2);
          end; {if actualread>)}
          inc(bytep,memoryregion[j].MemorySize);
          progressbar.stepit;
        end
        else
        for j:=0 to MemoryRegions do
        begin
          wordp:=pointer(bytep);    //wordp points to the first element of the old memory region
          wordp2:=pointer(memory2); //wordp2 points to the start of the first element of the new memory region
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);

          if actualread>0 then
          begin
            for i:=0 to actualread-2 do
            begin
              if wordp2^<wordp^ then
              begin
                foundaddress[found]:=memoryregion[j].BaseAddress+i;
                foundvalue2[found]:=wordp2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempwordarray:=pointer(foundvalue2switch);
                  foundvalue2switch:=pointer(foundvalue2);
                  foundvalue2:=pointer(tempwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                  found:=0;
                end; {if found}
              end; {if wordp2>...}

              asm
                inc [wordp]
                inc [wordp2]
              end; {asm}
            end; {for}
          end; {if actualread>)}

          progressbar.stepit;
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue2,2*found);

//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue2)^,found*2,actualwrite);
        found:=0;
      end;


      if scanway=decreased_Value_by then
      begin
        bytep:=pointer(memory);
        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          wordp:=pointer(bytep);    //wordp points to the first element of the old memory region
          wordp2:=pointer(memory2); //wordp2 points to the start of the first element of the new memory region
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          if percentage then
          begin
            for i:=1 to (actualread div 2) do
            begin
              if wordp2^<=wordp^-trunc((wordp^*(wordvalue/100))) then
              begin
                foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(wordp)-dword(bytep));
                foundvalue2[found]:=wordp2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempwordarray:=pointer(foundvalue2switch);
                  foundvalue2switch:=pointer(foundvalue2);
                  foundvalue2:=pointer(tempwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);


//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                  found:=0;
                end; {if found}
              end; {if wordp2>...}
              inc(wordp);
              inc(wordp2);
            end; {if actualread>)}
          end
          else
          begin
            for i:=1 to (actualread div 2) do
            begin
              if wordp2^=wordp^-wordvalue then
              begin
                foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(wordp)-dword(bytep));
                foundvalue2[found]:=wordp2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempwordarray:=pointer(foundvalue2switch);
                  foundvalue2switch:=pointer(foundvalue2);
                  foundvalue2:=pointer(tempwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);


//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                  found:=0;
                end; {if found}
              end; {if wordp2>...}
              inc(wordp);
              inc(wordp2);
            end; {if actualread>)}
          end;
          inc(bytep,memoryregion[j].MemorySize);
          progressbar.stepit;
        end
        else    //not fastscan
        for j:=0 to MemoryRegions do
        begin
          wordp:=pointer(bytep);    //wordp points to the first element of the old memory region
          wordp2:=pointer(memory2); //wordp2 points to the start of the first element of the new memory region
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);


          if actualread>0 then
          begin
            if percentage then
            begin
              for i:=0 to actualread-2 do
              begin
                if wordp2^<=wordp^-trunc((wordp^*(wordvalue/100))) then
                begin
                  foundaddress[found]:=memoryregion[j].BaseAddress+i;
                  foundvalue2[found]:=wordp2^;
                  inc(found);
                  if found=number then
                  begin
                    flushthread.datawritten.WaitFor(infinite);

                    tempdwordarray:=pointer(foundaddressswitch);
                    foundaddressswitch:=pointer(foundaddress);
                    foundaddress:=pointer(tempdwordarray);

                    tempwordarray:=pointer(foundvalue2switch);
                    foundvalue2switch:=pointer(foundvalue2);
                    foundvalue2:=pointer(tempwordarray);

                    flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);


//                    blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                    blockwrite(NewMemoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                    found:=0;
                  end; {if found}
                end; {if wordp2>...}

                asm
                  inc [wordp]
                  inc [wordp2]
                end; {asm}
              end; {for}
            end
            else
            begin

              for i:=0 to actualread-2 do
              begin
                if wordp2^=wordp^-wordvalue then
                begin
                  foundaddress[found]:=memoryregion[j].BaseAddress+i;
                  foundvalue2[found]:=wordp2^;
                  inc(found);
                  if found=number then
                  begin
                    flushthread.datawritten.WaitFor(infinite);

                    tempdwordarray:=pointer(foundaddressswitch);
                    foundaddressswitch:=pointer(foundaddress);
                    foundaddress:=pointer(tempdwordarray);

                    tempwordarray:=pointer(foundvalue2switch);
                    foundvalue2switch:=pointer(foundvalue2);
                    foundvalue2:=pointer(tempwordarray);

                    flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);


//                    blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                    blockwrite(NewMemoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                    found:=0;
                  end; {if found}
                end; {if wordp2>...}

                asm
                  inc [wordp]
                  inc [wordp2]
                end; {asm}
              end; {for}
            end;
          end; {if actualread>)}


          progressbar.stepit;
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue2,2*found);

//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue2)^,found*2,actualwrite);
        found:=0;
      end;

      if scanway=changed_value then
      begin
        bytep:=pointer(memory);
        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          wordp:=pointer(bytep);    //wordp points to the first element of the old memory region
          wordp2:=pointer(memory2); //wordp2 points to the start of the first element of the new memory region
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          for i:=1 to (actualread div 2) do
          begin
            if wordp2^<>wordp^ then
            begin
              foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(wordp)-dword(bytep));
              foundvalue2[found]:=wordp2^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempwordarray:=pointer(foundvalue2switch);
                foundvalue2switch:=pointer(foundvalue2);
                foundvalue2:=pointer(tempwordarray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

//                blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(NewMemoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                found:=0;
              end; {if found}
            end; {if wordp2>...}
            inc(wordp);
            inc(wordp2);
          end; {if actualread>)}
          inc(bytep,memoryregion[j].MemorySize);
          progressbar.stepit;
        end
        else
        for j:=0 to MemoryRegions do
        begin
          wordp:=pointer(bytep);    //wordp points to the first element of the old memory region
          wordp2:=pointer(memory2); //wordp2 points to the start of the first element of the new memory region
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);

          if actualread>0 then
          begin
            for i:=0 to actualread-2 do
            begin
              if wordp2^<>wordp^ then
              begin
                foundaddress[found]:=memoryregion[j].BaseAddress+i;
                foundvalue2[found]:=wordp2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempwordarray:=pointer(foundvalue2switch);
                  foundvalue2switch:=pointer(foundvalue2);
                  foundvalue2:=pointer(tempwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                  found:=0;
                end; {if found}
              end; {if wordp2>...}

              asm
                inc [wordp]
                inc [wordp2]
              end; {asm}
            end; {for}
          end; {if actualread>)}

          progressbar.stepit;
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue2,2*found);

//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue2)^,found*2,actualwrite);
        found:=0;
      end;

      if scanway=unchanged_value then
      begin
        bytep:=pointer(memory);
        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          wordp:=pointer(bytep);    //wordp points to the first element of the old memory region
          wordp2:=pointer(memory2); //wordp2 points to the start of the first element of the new memory region
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          for i:=1 to (actualread div 2) do
          begin
            if wordp2^=wordp^ then
            begin
              foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(wordp)-dword(bytep));
              foundvalue2[found]:=wordp2^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempwordarray:=pointer(foundvalue2switch);
                foundvalue2switch:=pointer(foundvalue2);
                foundvalue2:=pointer(tempwordarray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

//                blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(NewMemoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                found:=0;
              end; {if found}
            end; {if wordp2>...}
            inc(wordp);
            inc(wordp2);
          end; {if actualread>)}
          inc(bytep,memoryregion[j].MemorySize);
          progressbar.stepit;
        end
        else
        for j:=0 to MemoryRegions do
        begin
          wordp:=pointer(bytep);    //wordp points to the first element of the old memory region
          wordp2:=pointer(memory2); //wordp2 points to the start of the first element of the new memory region
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);

          if actualread>0 then
          begin
            for i:=0 to actualread-2 do
            begin
              if wordp2^=wordp^ then
              begin
                foundaddress[found]:=memoryregion[j].BaseAddress+i;
                foundvalue2[found]:=wordp2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempwordarray:=pointer(foundvalue2switch);
                  foundvalue2switch:=pointer(foundvalue2);
                  foundvalue2:=pointer(tempwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                  found:=0;
                end; {if found}
              end; {if wordp2>...}

              asm
                inc [wordp]
                inc [wordp2]
              end; {asm}
            end; {for}
          end; {if actualread>)}

          progressbar.stepit;
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue2,2*found);
        
//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue2)^,found*2,actualwrite);
        found:=0;
      end;

    end;

    if valtype=2 then
    begin
      //It's a DWord Scan
      setlength(foundvalue3,number);
      setlength(foundvalue3switch,number);
      foundvalue3[0]:=dwordvalue+1;
      foundvalue3switch[0]:=dwordvalue+1;

      if scanway=Exact_value then
      begin
        //It's an Exact value scan
        if fastscan then
        for i:=0 to memoryregions do
        begin
          Dwordp:=pointer(memory);

          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          for j:=1 to (actualread div 4) do
          begin
            if DwordP^=DwordValue then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(dwordp)-dword(memory));                inc(found);
              if found=number then
              begin
                if foundvalue3[0]<>dwordvalue then
                begin
                  for k:=0 to number-1 do foundvalue3[k]:=dwordvalue;
                  FoundIsFilled:=true;
                end;

                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempdwordarray:=pointer(foundvalue3switch);
                foundvalue3switch:=pointer(foundvalue3);
                foundvalue3:=pointer(tempdwordarray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//                blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(newMemoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                found:=0;
              end;
            end;
            inc(DwordP);
          end;
          progressbar.stepit;
        end
        else
        for i:=0 to memoryregions do
        begin
          Dwordp:=pointer(memory);

          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          if actualread>=4 then
          begin
            for j:=0 to actualread-4 do
            begin
              if DwordP^=DwordValue then
              begin
                foundaddress[found]:=Memoryregion[i].BaseAddress+j;
                inc(found);
                if found=number then
                begin
                  if foundvalue3[0]<>dwordvalue then
                  begin
                    for k:=0 to number-1 do foundvalue3[k]:=dwordvalue;
                    FoundIsFilled:=true;
                  end;

                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdwordarray:=pointer(foundvalue3switch);
                  foundvalue3switch:=pointer(foundvalue3);
                  foundvalue3:=pointer(tempdwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);


//                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(newMemoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc [DwordP]
              end;

            end;
          end;
          progressbar.stepit;
        end;
        if foundvalue3[0]<>dwordvalue then
        begin
          for k:=0 to found-1 do foundvalue3[k]:=dwordvalue;
          FoundIsFilled:=true;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue3,4*found);

//        blockwrite(newAddressfile,pointer(foundaddress)^,found*4,actualwrite);
//        blockwrite(newMemoryfile,pointer(foundvalue3)^,4*found,actualwrite);
        found:=0;
      end;

      if scanway=biggerthan then
      begin
        if fastscan then
        for i:=0 to memoryregions do
        begin
          Dwordp:=pointer(memory);

          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          for j:=1 to (actualread div 4) do
          begin
            if DwordP^>DwordValue then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(dwordp)-dword(memory));
              foundvalue3[found]:=helpdword;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempdwordarray:=pointer(foundvalue3switch);
                foundvalue3switch:=pointer(foundvalue3);
                foundvalue3:=pointer(tempdwordarray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//                blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(newMemoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                found:=0;
              end;
            end;
            inc(DwordP);
          end;
          progressbar.stepit;
        end
        else
        for i:=0 to memoryregions do
        begin
          Dwordp:=pointer(memory);

          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          if actualread>=4 then
          begin
            for j:=0 to actualread-4 do
            begin
              if DwordP^>DwordValue then
              begin
                foundaddress[found]:=Memoryregion[i].BaseAddress+j;
                foundvalue3[found]:=helpdword;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdwordarray:=pointer(foundvalue3switch);
                  foundvalue3switch:=pointer(foundvalue3);
                  foundvalue3:=pointer(tempdwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);


//                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(newMemoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc [DwordP]
              end;
            end;
          end;
          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue3,4*found);

//        blockwrite(newAddressfile,pointer(foundaddress)^,found*4,actualwrite);
//        blockwrite(newMemoryfile,pointer(foundvalue3)^,4*found,actualwrite);
        found:=0;
      end;

      if scanway=SmallerThan then
      begin
        if fastscan then
        for i:=0 to memoryregions do
        begin
          Dwordp:=pointer(memory);

          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          for j:=1 to (actualread div 4) do
          begin
            if DwordP^<DwordValue then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(dwordp)-dword(memory));
              foundvalue3[found]:=helpdword;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempdwordarray:=pointer(foundvalue3switch);
                foundvalue3switch:=pointer(foundvalue3);
                foundvalue3:=pointer(tempdwordarray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//                blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(newMemoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                found:=0;
              end;
            end;
            inc(DwordP);
          end;
          progressbar.stepit;
        end
        else
        for i:=0 to memoryregions do
        begin
          Dwordp:=pointer(memory);

          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          if actualread>=4 then
          begin
            for j:=0 to actualread-4 do
            begin
              if DwordP^<DwordValue then
              begin
                foundaddress[found]:=Memoryregion[i].BaseAddress+j;
                foundvalue3[found]:=helpdword;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdwordarray:=pointer(foundvalue3switch);
                  foundvalue3switch:=pointer(foundvalue3);
                  foundvalue3:=pointer(tempdwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(newMemoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc [DwordP]
              end;

            end;
          end;
          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue3,4*found);

//        blockwrite(newAddressfile,pointer(foundaddress)^,found*4,actualwrite);
//        blockwrite(newMemoryfile,pointer(foundvalue3)^,4*found,actualwrite);
        found:=0;
      end;

      if scanway=ValueBetween then
      begin
        if fastscan then
        for i:=0 to memoryregions do
        begin
          Dwordp:=pointer(memory);

          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          for j:=1 to (actualread div 4) do
          begin
            if (DwordP^>=DwordValue) and (Dwordp^<=Dwordvalue2) then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(dwordp)-dword(memory));
              foundvalue3[found]:=helpdword;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempdwordarray:=pointer(foundvalue3switch);
                foundvalue3switch:=pointer(foundvalue3);
                foundvalue3:=pointer(tempdwordarray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//                blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(newMemoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                found:=0;
              end;
            end;
            inc(DwordP);
          end;
          progressbar.stepit;
        end
        else
        for i:=0 to memoryregions do
        begin
          Dwordp:=pointer(memory);

          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          if actualread>=4 then
          begin
            for j:=0 to actualread-4 do
            begin
              if (DwordP^>=DwordValue) and (Dwordp^<=Dwordvalue2) then
              begin
                foundaddress[found]:=Memoryregion[i].BaseAddress+j;
                foundvalue3[found]:=helpdword;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdwordarray:=pointer(foundvalue3switch);
                  foundvalue3switch:=pointer(foundvalue3);
                  foundvalue3:=pointer(tempdwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(newMemoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc [DwordP]
              end;

            end;
          end;
          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue3,4*found);
        
//        blockwrite(newAddressfile,pointer(foundaddress)^,found*4,actualwrite);
//        blockwrite(newMemoryfile,pointer(foundvalue3)^,4*found,actualwrite);
        found:=0;
      end;

      if scanway=Increased_Value then
      begin
        bytep:=pointer(memory);
        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          dwordp:=pointer(bytep);
          dwordp2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          for i:=1 to (actualread div 4) do
          begin
            if dwordp2^>dwordp^ then
            begin
              foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(dwordp)-dword(bytep));
              foundvalue3[found]:=dwordp2^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempdwordarray:=pointer(foundvalue3switch);
                foundvalue3switch:=pointer(foundvalue3);
                foundvalue3:=pointer(tempdwordarray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//                blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(NewMemoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                found:=0;
              end;
            end;
            inc(dwordp);
            inc(dwordp2);
          end;
          inc(bytep,memoryregion[j].MemorySize);
          progressbar.stepit;
        end
        else
        for j:=0 to MemoryRegions do
        begin
          dwordp:=pointer(bytep);
          dwordp2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);

          if actualread>0 then
          begin
            for i:=0 to actualread-4 do
            begin
              if dwordp2^>dwordp^ then
              begin
                foundaddress[found]:=memoryregion[j].BaseAddress+i;
                foundvalue3[found]:=dwordp2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdwordarray:=pointer(foundvalue3switch);
                  foundvalue3switch:=pointer(foundvalue3);
                  foundvalue3:=pointer(tempdwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//                blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(NewMemoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc [dwordp]
                inc [dwordp2]
              end;
            end;
          end;
          progressbar.stepit;
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue3,4*found);
        
//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue3)^,found*4,actualwrite);
        found:=0;
      end;

      if scanway=Increased_Value_by then
      begin
        bytep:=pointer(memory);

        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          dwordp:=pointer(bytep);
          dwordp2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          if percentage then
          begin
            for i:=1 to (actualread div 4) do
            begin
              if dwordp2^>=dwordp^+trunc((dwordp^*(dwordvalue/100))) then
              begin
                foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(dwordp)-dword(bytep));
                foundvalue3[found]:=dwordp2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdwordarray:=pointer(foundvalue3switch);
                  foundvalue3switch:=pointer(foundvalue3);
                  foundvalue3:=pointer(tempdwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              inc(dwordp);
              inc(dwordp2);
            end;

          end
          else
          begin
            for i:=1 to (actualread div 4) do
            begin
              if dwordp2^=dwordp^+dwordvalue then
              begin
                foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(dwordp)-dword(bytep));
                foundvalue3[found]:=dwordp2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdwordarray:=pointer(foundvalue3switch);
                  foundvalue3switch:=pointer(foundvalue3);
                  foundvalue3:=pointer(tempdwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              inc(dwordp);
              inc(dwordp2);
            end;

          end;

          inc(bytep,memoryregion[j].MemorySize);
          progressbar.stepit;
        end
        else
        for j:=0 to MemoryRegions do
        begin
          dwordp:=pointer(bytep);
          dwordp2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          if actualread>0 then
          begin
            if percentage then
            begin
              for i:=0 to actualread-4 do
              begin
                if dwordp2^>=dwordp^+trunc((dwordp^*(dwordvalue/100))) then
                begin
                  foundaddress[found]:=memoryregion[j].BaseAddress+i;
                  foundvalue3[found]:=dwordp2^;
                  inc(found);
                  if found=number then
                  begin
                    flushthread.datawritten.WaitFor(infinite);

                    tempdwordarray:=pointer(foundaddressswitch);
                    foundaddressswitch:=pointer(foundaddress);
                    foundaddress:=pointer(tempdwordarray);

                    tempdwordarray:=pointer(foundvalue3switch);
                    foundvalue3switch:=pointer(foundvalue3);
                    foundvalue3:=pointer(tempdwordarray);

                    flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//                    blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                    blockwrite(NewMemoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                    found:=0;
                  end;
                end;
                asm
                  inc [dwordp]
                  inc [dwordp2]
                end;
              end;

            end
            else
            begin
              for i:=0 to actualread-4 do
              begin
                if dwordp2^=dwordp^+dwordvalue then
                begin
                  foundaddress[found]:=memoryregion[j].BaseAddress+i;
                  foundvalue3[found]:=dwordp2^;
                  inc(found);
                  if found=number then
                  begin
                    flushthread.datawritten.WaitFor(infinite);

                    tempdwordarray:=pointer(foundaddressswitch);
                    foundaddressswitch:=pointer(foundaddress);
                    foundaddress:=pointer(tempdwordarray);

                    tempdwordarray:=pointer(foundvalue3switch);
                    foundvalue3switch:=pointer(foundvalue3);
                    foundvalue3:=pointer(tempdwordarray);

                    flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//                    blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                    blockwrite(NewMemoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                    found:=0;
                  end;
                end;
                asm
                  inc [dwordp]
                  inc [dwordp2]
                end;
              end;
            end;
          end;
          progressbar.stepit;
          inc(bytep,memoryregion[j].MemorySize);

        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue3,4*found);

//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue3)^,found*4,actualwrite);
        found:=0;
      end;


      if scanway=decreased_Value then
      begin
        bytep:=pointer(memory);

        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          dwordp:=pointer(bytep);
          dwordp2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          for i:=1 to (actualread div 4) do
          begin
            if dwordp2^<dwordp^ then
            begin
              foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(dwordp)-dword(bytep));
              foundvalue3[found]:=dwordp2^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempdwordarray:=pointer(foundvalue3switch);
                foundvalue3switch:=pointer(foundvalue3);
                foundvalue3:=pointer(tempdwordarray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//                blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(NewMemoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                found:=0;
              end;
            end;
            inc(dwordp);
            inc(dwordp2);
          end;
          inc(bytep,memoryregion[j].MemorySize);
          progressbar.stepit;
        end
        else
        for j:=0 to MemoryRegions do
        begin
          dwordp:=pointer(bytep);
          dwordp2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);

          if actualread>=4 then
          begin
            for i:=0 to actualread-4 do
            begin
              if dwordp2^<dwordp^ then
              begin
                foundaddress[found]:=memoryregion[j].BaseAddress+i;
                foundvalue3[found]:=dwordp2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdwordarray:=pointer(foundvalue3switch);
                  foundvalue3switch:=pointer(foundvalue3);
                  foundvalue3:=pointer(tempdwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc [dwordp]
                inc [dwordp2]
              end;
            end;
          end;
          progressbar.stepit;
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue3,4*found);

//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue3)^,found*4,actualwrite);
        found:=0;
      end;


      if scanway=decreased_Value_by then
      begin
        bytep:=pointer(memory);

        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          dwordp:=pointer(bytep);
          dwordp2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          if percentage then
          begin
            for i:=1 to (actualread div 4) do
            begin
              if dwordp2^<=dwordp^-trunc(dwordp^*(dwordvalue/100)) then
              begin
                foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(dwordp)-dword(bytep));
                foundvalue3[found]:=dwordp2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdwordarray:=pointer(foundvalue3switch);
                  foundvalue3switch:=pointer(foundvalue3);
                  foundvalue3:=pointer(tempdwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              inc(dwordp);
              inc(dwordp2);
            end;
          end
          else
          begin
            for i:=1 to (actualread div 4) do
            begin
              if dwordp2^=dwordp^-dwordvalue then
              begin
                foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(dwordp)-dword(bytep));
                foundvalue3[found]:=dwordp2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdwordarray:=pointer(foundvalue3switch);
                  foundvalue3switch:=pointer(foundvalue3);
                  foundvalue3:=pointer(tempdwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              inc(dwordp);
              inc(dwordp2);
            end;
          end;
          inc(bytep,memoryregion[j].MemorySize);

          progressbar.stepit;
        end
        else
        for j:=0 to MemoryRegions do
        begin
          dwordp:=pointer(bytep);
          dwordp2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);

          if actualread>=4 then
          begin

            if percentage then
            begin
              for i:=0 to actualread-4 do
              begin
                if dwordp2^<=dwordp^-trunc(dwordp^*(dwordvalue/100)) then
                begin
                  foundaddress[found]:=memoryregion[j].BaseAddress+i;
                  foundvalue3[found]:=dwordp2^;
                  inc(found);
                  if found=number then
                  begin
                    flushthread.datawritten.WaitFor(infinite);

                    tempdwordarray:=pointer(foundaddressswitch);
                    foundaddressswitch:=pointer(foundaddress);
                    foundaddress:=pointer(tempdwordarray);

                    tempdwordarray:=pointer(foundvalue3switch);
                    foundvalue3switch:=pointer(foundvalue3);
                    foundvalue3:=pointer(tempdwordarray);

                    flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//                    blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                    blockwrite(NewMemoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                    found:=0;
                  end;
                end;
                asm
                  inc [dwordp]
                  inc [dwordp2]
                end;
              end;
            end
            else
            begin
              for i:=0 to actualread-4 do
              begin
                if dwordp2^=dwordp^-dwordvalue then
                begin
                  foundaddress[found]:=memoryregion[j].BaseAddress+i;
                  foundvalue3[found]:=dwordp2^;
                  inc(found);
                  if found=number then
                  begin
                    flushthread.datawritten.WaitFor(infinite);

                    tempdwordarray:=pointer(foundaddressswitch);
                    foundaddressswitch:=pointer(foundaddress);
                    foundaddress:=pointer(tempdwordarray);

                    tempdwordarray:=pointer(foundvalue3switch);
                    foundvalue3switch:=pointer(foundvalue3);
                    foundvalue3:=pointer(tempdwordarray);

                    flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//                    blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                    blockwrite(NewMemoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                    found:=0;
                  end;
                end;
                asm
                  inc [dwordp]
                  inc [dwordp2]
                end;
              end;
            end;


          end;
          progressbar.stepit;
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue3,4*found);

//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue3)^,found*4,actualwrite);
        found:=0;
      end;

      if scanway=changed_value then
      begin
        bytep:=pointer(memory);

        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          dwordp:=pointer(bytep);
          dwordp2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          for i:=1 to (actualread div 4) do
          begin
            if dwordp2^<>dwordp^ then
            begin
              foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(dwordp)-dword(bytep));
              foundvalue3[found]:=dwordp2^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempdwordarray:=pointer(foundvalue3switch);
                foundvalue3switch:=pointer(foundvalue3);
                foundvalue3:=pointer(tempdwordarray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//                blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(NewMemoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                found:=0;
              end;
            end;
            inc(dwordp);
            inc(dwordp2);
          end;
          inc(bytep,memoryregion[j].MemorySize);
          progressbar.stepit;
        end
        else
        for j:=0 to MemoryRegions do
        begin
          dwordp:=pointer(bytep);
          dwordp2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);

          if actualread>=4 then
          begin

            for i:=0 to actualread-4 do
            begin
              if dwordp2^<>dwordp^ then
              begin
                foundaddress[found]:=memoryregion[j].BaseAddress+i;
                foundvalue3[found]:=dwordp2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdwordarray:=pointer(foundvalue3switch);
                  foundvalue3switch:=pointer(foundvalue3);
                  foundvalue3:=pointer(tempdwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc [dwordp]
                inc [dwordp2]
              end;
            end;
          end;
          progressbar.stepit;
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue3,4*found);

//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue3)^,found*4,actualwrite);
        found:=0;
      end;

      if scanway=unchanged_Value then
      begin
        bytep:=pointer(memory);

        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          dwordp:=pointer(bytep);
          dwordp2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          for i:=1 to (actualread div 4) do
          begin
            if dwordp2^=dwordp^ then
            begin
              foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(dwordp)-dword(bytep));
              foundvalue3[found]:=dwordp2^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempdwordarray:=pointer(foundvalue3switch);
                foundvalue3switch:=pointer(foundvalue3);
                foundvalue3:=pointer(tempdwordarray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//                blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(NewMemoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                found:=0;
              end;
            end;
            inc(dwordp);
            inc(dwordp2);
          end;
          inc(bytep,memoryregion[j].MemorySize);
          progressbar.stepit;
        end
        else
        for j:=0 to MemoryRegions do
        begin
          dwordp:=pointer(bytep);
          dwordp2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);

          if actualread>0 then
          begin
            for i:=0 to actualread-4 do
            begin
              if dwordp2^=dwordp^ then
              begin
                foundaddress[found]:=memoryregion[j].BaseAddress+i;
                foundvalue3[found]:=dwordp2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdwordarray:=pointer(foundvalue3switch);
                  foundvalue3switch:=pointer(foundvalue3);
                  foundvalue3:=pointer(tempdwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc [dwordp]
                inc [dwordp2]
              end;
            end;
          end;
          progressbar.stepit;
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue3,4*found);
        
//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue3)^,found*4,actualwrite);
        found:=0;
      end;

    end;

    if valtype=3 then  //float
    begin
      //It's a FLOAT Scan
      setlength(foundvalue4,number);
      setlength(foundvalue4switch,number);
      singlevalue:=RoundTo(singlevalue,-decim);

      if decim=0 then helpsingle3:=1 else
        helpsingle3:=1/((decim)*10);  //the range for extremerounded scans


      if scanway=Exact_value then
      begin
        //It's an Exact value scan
        if fastscan then
        for i:=0 to memoryregions do
        begin
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          Singlep:=pointer(memory);

          for j:=1 to (actualread div 4) do
          begin
            check:=(not (isnan(singlep^) or isinfinite(singlep^)));
            if check then
            case roundingtype of
              rounded:
              begin
                helpsingle:=RoundTo(SingleP^,-decim);
                check:=(helpsingle=SingleValue);
              end;

              extremerounded:
              begin
                //if a scan for 1 it scans for    0<x<2
                //if a scan for 1.0 it scans for  9.9<x<1.10
                check:=((SingleP^<(singlevalue+helpsingle3)) and (SingleP^>(singlevalue-helpsingle3)) );
              end;

              truncated:
              begin
                //if a scan for 1 it scans for    1>=x<2
                //if a scan for 1.0 it scans for 1.0>=x<1.10
                check:=((SingleP^<(singlevalue+helpsingle3)) and (singlep^>=singlevalue));
              end;

              else check:=false;
            end;

            if check then
            begin
              foundvalue4[found]:=SingleP^;
              foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(singlep)-dword(memory));
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempsinglearray:=pointer(foundvalue4switch);
                foundvalue4switch:=pointer(foundvalue4);
                foundvalue4:=pointer(tempsinglearray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                found:=0;
              end;
            end;
            inc(SingleP);
          end;

          progressbar.stepit;
        end
        else
        for i:=0 to memoryregions do
        begin
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          if actualread>=4 then
          begin
            Singlep:=pointer(memory);

            for j:=0 to actualread-4 do
            begin
              check:=(not (isnan(singlep^) or isinfinite(singlep^)));
              if check then
              case roundingtype of
                rounded:
                begin
                  helpsingle:=RoundTo(SingleP^,-decim);
                  check:=(helpsingle=SingleValue);
                end;

                extremerounded:
                begin
                  //if a scan for 1 it scans for    0<x<2
                  //if a scan for 1.0 it scans for  9.9<x<1.10
                  check:=((SingleP^<(singlevalue+helpsingle3)) and (SingleP^>(singlevalue-helpsingle3)) );
                end;

                truncated:
                begin
                  //if a scan for 1 it scans for    1>=x<2
                  //if a scan for 1.0 it scans for 1.0>=x<1.10
                  check:=((SingleP^<(singlevalue+helpsingle3)) and (singlep^>=singlevalue));
                end;

                else check:=false;
              end;

              if check then
              begin
                foundvalue4[found]:=SingleP^;
                foundaddress[found]:=Memoryregion[i].BaseAddress+j;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempsinglearray:=pointer(foundvalue4switch);
                  foundvalue4switch:=pointer(foundvalue4);
                  foundvalue4:=pointer(tempsinglearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(Memoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                  found:=0;
                end;
              end;

              asm
                inc [SingleP]
              end;
            end;
          end;

          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue4,4*found);

//        blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//        blockwrite(Memoryfile,pointer(foundvalue4)^,4*found,actualwrite);
        found:=0;
      end;

      if scanway=biggerThan then
      begin
        if fastscan then
        for i:=0 to memoryregions do
        begin
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          Singlep:=pointer(memory);
          for j:=1 to (actualread div 4) do
          begin
            helpsingle:=RoundTo(SingleP^,-decim);
            if (not (isnan(singlep^) or isinfinite(singlep^))) and (helpsingle>SingleValue) then
            begin
              foundvalue4[found]:=SingleP^;
              foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(singlep)-dword(memory));
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempsinglearray:=pointer(foundvalue4switch);
                foundvalue4switch:=pointer(foundvalue4);
                foundvalue4:=pointer(tempsinglearray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                found:=0;
              end;
            end;
            inc(SingleP);
          end;

          progressbar.stepit;
        end
        else
        for i:=0 to memoryregions do
        begin
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          if actualread>=4 then
          begin
            Singlep:=pointer(memory);
            for j:=0 to actualread-1 do
            begin
              helpsingle:=RoundTo(SingleP^,-decim);
              if (not (isnan(singlep^) or isinfinite(singlep^))) and (helpsingle>SingleValue) then
              begin
                foundvalue4[found]:=SingleP^;
                foundaddress[found]:=Memoryregion[i].BaseAddress+j;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempsinglearray:=pointer(foundvalue4switch);
                  foundvalue4switch:=pointer(foundvalue4);
                  foundvalue4:=pointer(tempsinglearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(Memoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                  found:=0;
                end;
              end;

              asm
                inc [SingleP]
              end;
            end;
          end;

          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue4,4*found);

//        blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//        blockwrite(Memoryfile,pointer(foundvalue4)^,4*found,actualwrite);
        found:=0;
      end;

      if scanway=SmallerThan then
      begin
        if fastscan then
        for i:=0 to memoryregions do
        begin
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          Singlep:=pointer(memory);
          for j:=1 to (actualread div 4) do
          begin
            helpsingle:=RoundTo(SingleP^,-decim);
            if (not (isnan(singlep^) or isinfinite(singlep^))) and (helpsingle<SingleValue) then
            begin
              foundvalue4[found]:=SingleP^;
              foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(singlep)-dword(memory));
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempsinglearray:=pointer(foundvalue4switch);
                foundvalue4switch:=pointer(foundvalue4);
                foundvalue4:=pointer(tempsinglearray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                found:=0;
              end;
            end;
            inc(SingleP);
          end;

          progressbar.stepit;
        end
        else
        for i:=0 to memoryregions do
        begin
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          begin

            Singlep:=pointer(memory);
            if actualread>0 then
            for j:=0 to actualread-1 do
            begin
              helpsingle:=RoundTo(SingleP^,-decim);
              if (not (isnan(singlep^) or isinfinite(singlep^))) and (helpsingle<SingleValue) then
              begin
                foundvalue4[found]:=SingleP^;
                foundaddress[found]:=Memoryregion[i].BaseAddress+j;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempsinglearray:=pointer(foundvalue4switch);
                  foundvalue4switch:=pointer(foundvalue4);
                  foundvalue4:=pointer(tempsinglearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(Memoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                  found:=0;
                end;
              end;

              asm
                inc [SingleP]
              end;
            end;
          end;

          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue4,4*found);
        
//        blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//        blockwrite(Memoryfile,pointer(foundvalue4)^,4*found,actualwrite);
        found:=0;
      end;

      if scanway=ValueBetween then
      begin
        if fastscan then
        for i:=0 to memoryregions do
        begin
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          Singlep:=pointer(memory);
          for j:=1 to (actualread div 4) do
          begin
            helpsingle:=RoundTo(SingleP^,-decim);
            if (not (isnan(singlep^) or isinfinite(singlep^))) and (helpsingle>=SingleValue) and (helpsingle<=singlevalue2) then
            begin
              foundvalue4[found]:=SingleP^;
              foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(singlep)-dword(memory));
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempsinglearray:=pointer(foundvalue4switch);
                foundvalue4switch:=pointer(foundvalue4);
                foundvalue4:=pointer(tempsinglearray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                found:=0;
              end;
            end;
            inc(SingleP);
          end;

          progressbar.stepit;
        end
        else
        for i:=0 to memoryregions do
        begin
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          begin

            Singlep:=pointer(memory);
            if actualread>0 then
            for j:=0 to actualread-1 do
            begin
              helpsingle:=RoundTo(SingleP^,-decim);
              if (not (isnan(singlep^) or isinfinite(singlep^))) and (helpsingle>=SingleValue) and (helpsingle<=singlevalue2) then
              begin
                foundvalue4[found]:=SingleP^;
                foundaddress[found]:=Memoryregion[i].BaseAddress+j;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempsinglearray:=pointer(foundvalue4switch);
                  foundvalue4switch:=pointer(foundvalue4);
                  foundvalue4:=pointer(tempsinglearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(Memoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                  found:=0;
                end;
              end;

              asm
                inc [SingleP]
              end;
            end;
          end;

          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue4,4*found);
        
//        blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//        blockwrite(Memoryfile,pointer(foundvalue4)^,4*found,actualwrite);
        found:=0;
      end;

      if scanway=Increased_Value then
      begin
        //It's an Increased value scan
        bytep:=pointer(memory);

        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          singlep:=pointer(bytep);
          singlep2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          for i:=1 to (actualread div 4) do
          begin
            if (singlep2^>singlep^) and (not (isnan(singlep^) or isinfinite(singlep^))) then  //no rounding down needed here, the value has changed or not.
            begin
              foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(singlep)-dword(bytep));
              foundvalue4[found]:=singlep2^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempsinglearray:=pointer(foundvalue4switch);
                foundvalue4switch:=pointer(foundvalue4);
                foundvalue4:=pointer(tempsinglearray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//                blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(NewMemoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                found:=0;
              end;
            end;
            inc(singlep);
            inc(singlep2);
          end;
          inc(bytep,memoryregion[j].MemorySize);
          progressbar.stepit;
        end
        else
        for j:=0 to MemoryRegions do
        begin
          singlep:=pointer(bytep);
          singlep2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);

          if actualread>0 then
          begin
            for i:=0 to actualread-4 do
            begin
              if (singlep2^>singlep^) and (not (isnan(singlep^) or isinfinite(singlep^))) then  //no rounding down needed here, the value has changed or not.
              begin
                foundaddress[found]:=memoryregion[j].BaseAddress+i;
                foundvalue4[found]:=singlep2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempsinglearray:=pointer(foundvalue4switch);
                  foundvalue4switch:=pointer(foundvalue4);
                  foundvalue4:=pointer(tempsinglearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc [singlep]
                inc [singlep2]
              end;
            end;
          end;
          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue4,4*found);
        
//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue4)^,found*4,actualwrite);
        found:=0;
      end;

      if scanway=Increased_Value_by then
      begin
        //It's an Increased value scan
        bytep:=pointer(memory);

        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          singlep:=pointer(bytep);
          singlep2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          if percentage then
          begin
            for i:=1 to (actualread div 4) do
            begin
              helpsingle:=roundto(singlep2^,-decim);
              helpsingle2:=singlep^+singlep^*(singlevalue/100);

              if (singlep2^>=singlep^+singlep^*(singlevalue/100)) and (not (isnan(singlep2^) or isinfinite(singlep2^))) then
              begin
                foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(singlep)-dword(bytep));
                foundvalue4[found]:=singlep2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempsinglearray:=pointer(foundvalue4switch);
                  foundvalue4switch:=pointer(foundvalue4);
                  foundvalue4:=pointer(tempsinglearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);
                
//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              inc(singlep);
              inc(singlep2);
            end;

          end
          else
          begin
            for i:=1 to (actualread div 4) do
            begin
              helpsingle:=roundto(singlep2^,-decim);
              helpsingle2:=RoundTo(singlep^+singlevalue,-decim);

              if (singlep2^>singlep^) and (helpsingle=helpsingle2) and (not (isnan(singlep2^) or isinfinite(singlep2^))) then
              begin
                foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(singlep)-dword(bytep));
                foundvalue4[found]:=singlep2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempsinglearray:=pointer(foundvalue4switch);
                  foundvalue4switch:=pointer(foundvalue4);
                  foundvalue4:=pointer(tempsinglearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);
                
//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              inc(singlep);
              inc(singlep2);
            end;
          end;
          inc(bytep,memoryregion[j].MemorySize);
          progressbar.stepit;
        end
        else
        for j:=0 to MemoryRegions do
        begin
          singlep:=pointer(bytep);
          singlep2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);

          if actualread>=4 then
          begin
            if percentage then
            begin
              for i:=0 to actualread-4 do
              begin
                helpsingle:=roundto(singlep2^,-decim);
                helpsingle2:=RoundTo(singlep^+singlevalue,-decim);
                if (singlep2^>=singlep^+singlep^*(singlevalue/100)) and (not (isnan(singlep2^) or isinfinite(singlep2^))) then
                begin
                  foundaddress[found]:=memoryregion[j].BaseAddress+i;
                  foundvalue4[found]:=singlep2^;
                  inc(found);
                  if found=number then
                  begin
                    flushthread.datawritten.WaitFor(infinite);

                    tempdwordarray:=pointer(foundaddressswitch);
                    foundaddressswitch:=pointer(foundaddress);
                    foundaddress:=pointer(tempdwordarray);

                    tempsinglearray:=pointer(foundvalue4switch);
                    foundvalue4switch:=pointer(foundvalue4);
                    foundvalue4:=pointer(tempsinglearray);

                    flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//                    blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                    blockwrite(NewMemoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                    found:=0;
                  end;
                end;
                asm
                  inc [singlep]
                  inc [singlep2]
                end;
              end;

            end
            else
            begin
              for i:=0 to actualread-4 do
              begin
                helpsingle:=roundto(singlep2^,-decim);
                helpsingle2:=RoundTo(singlep^+singlevalue,-decim);
                if (singlep2^>singlep^) and (helpsingle=helpsingle2) and (not (isnan(singlep2^) or isinfinite(singlep2^))) then
                begin
                  foundaddress[found]:=memoryregion[j].BaseAddress+i;
                  foundvalue4[found]:=singlep2^;
                  inc(found);
                  if found=number then
                  begin
                    flushthread.datawritten.WaitFor(infinite);

                    tempdwordarray:=pointer(foundaddressswitch);
                    foundaddressswitch:=pointer(foundaddress);
                    foundaddress:=pointer(tempdwordarray);

                    tempsinglearray:=pointer(foundvalue4switch);
                    foundvalue4switch:=pointer(foundvalue4);
                    foundvalue4:=pointer(tempsinglearray);

                    flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//                    blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                    blockwrite(NewMemoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                    found:=0;
                  end;
                end;
                asm
                  inc [singlep]
                  inc [singlep2]
                end;
              end;
            end;
          end;
          progressbar.stepit;
        end;
//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue4)^,found*4,actualwrite);
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue4,4*found);

        found:=0;
      end;

      if scanway=decreased_Value then
      begin
        //It's an Increased value scan
        bytep:=pointer(memory);

        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          singlep:=pointer(bytep);
          singlep2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          for i:=1 to (actualread div 4) do
          begin
            if (singlep2^<singlep^) and (not (isnan(singlep2^) or isinfinite(singlep2^))) then  //no rounding down needed here, the value has changed or not.
            begin
              foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(singlep)-dword(bytep));
              foundvalue4[found]:=singlep2^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempsinglearray:=pointer(foundvalue4switch);
                foundvalue4switch:=pointer(foundvalue4);
                foundvalue4:=pointer(tempsinglearray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//                blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(NewMemoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                found:=0;
              end;
            end;
            inc(singlep);
            inc(singlep2);
          end;
          inc(bytep,memoryregion[j].MemorySize);

          progressbar.stepit;
        end
        else
        for j:=0 to MemoryRegions do
        begin
          singlep:=pointer(bytep);
          singlep2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);

          if actualread>0 then
          begin

            for i:=0 to actualread-4 do
            begin
              if (singlep2^<singlep^) and (not (isnan(singlep2^) or isinfinite(singlep2^))) then  //no rounding down needed here, the value has changed or not.
              begin
                foundaddress[found]:=memoryregion[j].BaseAddress+i;
                foundvalue4[found]:=singlep2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempsinglearray:=pointer(foundvalue4switch);
                  foundvalue4switch:=pointer(foundvalue4);
                  foundvalue4:=pointer(tempsinglearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc [singlep]
                inc [singlep2]
              end;
            end;
          end;
          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue4,4*found);

//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue4)^,found*4,actualwrite);
        found:=0;
      end;

      if scanway=decreased_Value_by then
      begin
        //It's an Increased value scan
        bytep:=pointer(memory);

        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          singlep:=pointer(bytep);
          singlep2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          if percentage then
          begin
            for i:=1 to (actualread div 4) do
            begin
              helpsingle:=roundto(singlep2^,-decim);
              helpsingle2:=RoundTo(singlep^-singlevalue,-decim);
              if (singlep2^<=singlep^-singlep^*(singlevalue/100)) and (not (isnan(singlep2^) or isinfinite(singlep2^))) then
              begin
                foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(singlep)-dword(bytep));
                foundvalue4[found]:=singlep2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempsinglearray:=pointer(foundvalue4switch);
                  foundvalue4switch:=pointer(foundvalue4);
                  foundvalue4:=pointer(tempsinglearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              inc(singlep);
              inc(singlep2);
            end;

          end
          else
          begin
            for i:=1 to (actualread div 4) do
            begin
              helpsingle:=roundto(singlep2^,-decim);
              helpsingle2:=RoundTo(singlep^-singlevalue,-decim);
              if (singlep2^<singlep^) and (not (isnan(singlep2^) or isinfinite(singlep2^))) and (helpsingle=helpsingle2) then
              begin
                foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(singlep)-dword(bytep));
                foundvalue4[found]:=singlep2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempsinglearray:=pointer(foundvalue4switch);
                  foundvalue4switch:=pointer(foundvalue4);
                  foundvalue4:=pointer(tempsinglearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              inc(singlep);
              inc(singlep2);
            end;
          end;
          inc(bytep,memoryregion[j].MemorySize);
          progressbar.stepit;
        end
        else
        for j:=0 to MemoryRegions do
        begin
          singlep:=pointer(bytep);
          singlep2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);

          if actualread>0 then
          begin
            if percentage then
            begin
              for i:=0 to actualread-4 do
              begin
                helpsingle:=roundto(singlep2^,-decim);
                helpsingle2:=RoundTo(singlep^-singlevalue,-decim);
                if (singlep2^<=singlep^-singlep^*(singlevalue/100)) and (not (isnan(singlep2^) or isinfinite(singlep2^))) then
                begin
                  foundaddress[found]:=memoryregion[j].BaseAddress+i;
                  foundvalue4[found]:=singlep2^;
                  inc(found);
                  if found=number then
                  begin
                    flushthread.datawritten.WaitFor(infinite);

                    tempdwordarray:=pointer(foundaddressswitch);
                    foundaddressswitch:=pointer(foundaddress);
                    foundaddress:=pointer(tempdwordarray);

                    tempsinglearray:=pointer(foundvalue4switch);
                    foundvalue4switch:=pointer(foundvalue4);
                    foundvalue4:=pointer(tempsinglearray);

                    flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//                    blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                    blockwrite(NewMemoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                    found:=0;
                  end;
                end;
                asm
                  inc [singlep]
                  inc [singlep2]
                end;
              end;
            end
            else
            begin
              for i:=0 to actualread-4 do
              begin
                helpsingle:=roundto(singlep2^,-decim);
                helpsingle2:=RoundTo(singlep^-singlevalue,-decim);
                if (singlep2^<singlep^) and (not (isnan(singlep2^) or isinfinite(singlep2^))) and (helpsingle=helpsingle2) then
                begin
                  foundaddress[found]:=memoryregion[j].BaseAddress+i;
                  foundvalue4[found]:=singlep2^;
                  inc(found);
                  if found=number then
                  begin
                    flushthread.datawritten.WaitFor(infinite);

                    tempdwordarray:=pointer(foundaddressswitch);
                    foundaddressswitch:=pointer(foundaddress);
                    foundaddress:=pointer(tempdwordarray);

                    tempsinglearray:=pointer(foundvalue4switch);
                    foundvalue4switch:=pointer(foundvalue4);
                    foundvalue4:=pointer(tempsinglearray);

                    flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//                    blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                    blockwrite(NewMemoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                    found:=0;
                  end;
                end;
                asm
                  inc [singlep]
                  inc [singlep2]
                end;
              end;
            end;
          end;
          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue4,4*found);

//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue4)^,found*4,actualwrite);
        found:=0;
      end;


      if scanway=changed_Value then
      begin
        //It's an Increased value scan
        bytep:=pointer(memory);

        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          singlep:=pointer(bytep);
          singlep2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          for i:=1 to (actualread div 4) do
          begin
            if (singlep2^<>singlep^) and (not (isnan(singlep2^) or isinfinite(singlep2^))) then  //no rounding down needed here, the value has changed or not.
            begin
              foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(singlep)-dword(bytep));
              foundvalue4[found]:=singlep2^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempsinglearray:=pointer(foundvalue4switch);
                foundvalue4switch:=pointer(foundvalue4);
                foundvalue4:=pointer(tempsinglearray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//                blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(NewMemoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                found:=0;
              end;
            end;
            inc(singlep);
            inc(singlep2);
          end;
          inc(bytep,memoryregion[j].MemorySize);
          progressbar.stepit;
        end
        else
        for j:=0 to MemoryRegions do
        begin
          singlep:=pointer(bytep);
          singlep2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);

          if actualread>0 then
          begin

            for i:=0 to actualread-4 do
            begin
              if (singlep2^<>singlep^) and (not (isnan(singlep2^) or isinfinite(singlep2^))) then  //no rounding down needed here, the value has changed or not.
              begin
                foundaddress[found]:=memoryregion[j].BaseAddress+i;
                foundvalue4[found]:=singlep2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempsinglearray:=pointer(foundvalue4switch);
                  foundvalue4switch:=pointer(foundvalue4);
                  foundvalue4:=pointer(tempsinglearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc [singlep]
                inc [singlep2]
              end;
            end;
          end;
          progressbar.stepit;
        end;
//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue4)^,found*4,actualwrite);
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue4,4*found);

        found:=0;
      end;

      if scanway=unchanged_Value then
      begin
        //It's an Increased value scan
        bytep:=pointer(memory);

        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          singlep:=pointer(bytep);
          singlep2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          for i:=1 to (actualread div 4) do
          begin
            if (singlep2^=singlep^) and (not (isnan(singlep2^) or isinfinite(singlep2^))) then  //no rounding down needed here, the value has changed or not.
            begin
              foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(singlep)-dword(bytep));
              foundvalue4[found]:=singlep2^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempsinglearray:=pointer(foundvalue4switch);
                foundvalue4switch:=pointer(foundvalue4);
                foundvalue4:=pointer(tempsinglearray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

//                blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(NewMemoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                found:=0;
              end;
            end;
            inc(singlep);
            inc(singlep2);
          end;
          inc(bytep,memoryregion[j].MemorySize);
          progressbar.stepit;
        end
        else
        for j:=0 to MemoryRegions do
        begin
          singlep:=pointer(bytep);
          singlep2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);

          if actualread>0 then
          begin

            for i:=0 to actualread-4 do
            begin
              if (singlep2^=singlep^) and (not (isnan(singlep2^) or isinfinite(singlep2^))) then  //no rounding down needed here, the value has changed or not.
              begin
                foundaddress[found]:=memoryregion[j].BaseAddress+i;
                foundvalue4[found]:=singlep2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempsinglearray:=pointer(foundvalue4switch);
                  foundvalue4switch:=pointer(foundvalue4);
                  foundvalue4:=pointer(tempsinglearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);
                
//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc [singlep]
                inc [singlep2]
              end;
            end;
          end;
          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue4,4*found);

//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue4)^,found*4,actualwrite);
        found:=0;
      end;

    end;


    if valtype=4 then
    begin
      //It's a double Scan
      setlength(foundvalue5,number);
      setlength(foundvalue5switch,number);

      doublevalue:=RoundTo(doublevalue,-decim);
      if decim=0 then helpdouble3:=1 else
        helpdouble3:=1/((decim)*10);  //the range for extremerounded scans


      if scanway=Exact_value then
      begin
        //It's an Exact value scan
        if fastscan then
        for i:=0 to memoryregions do
        begin
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          doublep:=pointer(memory);

          for j:=1 to (actualread div 8) do
          begin
            check:=(not (isnan(doublep^) or isinfinite(doublep^)));
            if check then
            case roundingtype of
              rounded:
              begin
                helpdouble:=RoundTo(doubleP^,-decim);
                check:=(helpdouble=doubleValue);
              end;

              extremerounded:
              begin
                //if a scan for 1 it scans for    0<x<2
                //if a scan for 1.0 it scans for  9.9<x<1.10
                check:=((doubleP^<(doublevalue+helpdouble3)) and (doubleP^>(doublevalue-helpdouble3)) );
              end;

              truncated:
              begin
                //if a scan for 1 it scans for    1>=x<2
                //if a scan for 1.0 it scans for 1.0>=x<1.10
                check:=((doubleP^<(doublevalue+helpdouble3)) and (doublep^>=doublevalue));
              end;

              else check:=false;
            end;


            if check then
            begin
              foundvalue5[found]:=doublep^;
              foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(doublep)-dword(memory));
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempdoublearray:=pointer(foundvalue5switch);
                foundvalue5switch:=pointer(foundvalue5);
                foundvalue5:=pointer(tempdoublearray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                found:=0;
              end;
            end;
            inc(doublep);
          end;
          progressbar.stepit;
        end
        else
        for i:=0 to memoryregions do
        begin
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          if actualread>7 then
          begin

            doublep:=pointer(memory);

            for j:=0 to actualread-8 do
            begin
              check:=(not (isnan(doublep^) or isinfinite(doublep^)));
              if check then
              case roundingtype of
                rounded:
                begin
                  helpdouble:=RoundTo(doubleP^,-decim);
                  check:=(helpdouble=doubleValue);
                end;

                extremerounded:
                begin
                  //if a scan for 1 it scans for    0<x<2
                  //if a scan for 1.0 it scans for  9.9<x<1.10
                  check:=((doubleP^<(doublevalue+helpdouble3)) and (doubleP^>(doublevalue-helpdouble3)) );
                end;

                truncated:
                begin
                  //if a scan for 1 it scans for    1>=x<2
                  //if a scan for 1.0 it scans for 1.0>=x<1.10
                  check:=((doubleP^<(doublevalue+helpdouble3)) and (doublep^>=doublevalue));
                end;

                else check:=false;
              end;


              if check then
              begin
                foundvalue5[found]:=doublep^;
                foundaddress[found]:=Memoryregion[i].BaseAddress+j;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdoublearray:=pointer(foundvalue5switch);
                  foundvalue5switch:=pointer(foundvalue5);
                  foundvalue5:=pointer(tempdoublearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(Memoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                  found:=0;
                end;
              end;

              asm
                inc [doublep]
              end;
            end;
          end;
          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue5,8*found);

//        blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//        blockwrite(Memoryfile,pointer(foundvalue5)^,8*found,actualwrite);
        found:=0;
      end;

      if scanway=biggerThan then
      begin
        if fastscan then
        for i:=0 to memoryregions do
        begin
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          doublep:=pointer(memory);
          for j:=1 to (actualread div 8) do
          begin
            if (RoundTo(doublep^,-decim)>doublevalue) and (not (isnan(doublep^) or isinfinite(doublep^))) then
            begin
              foundvalue5[found]:=doublep^;
              foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(doublep)-dword(memory));
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempdoublearray:=pointer(foundvalue5switch);
                foundvalue5switch:=pointer(foundvalue5);
                foundvalue5:=pointer(tempdoublearray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                found:=0;
              end;
            end;
            inc(doublep);
          end;

          progressbar.stepit;
        end
        else
        for i:=0 to memoryregions do
        begin
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          if actualread>=8 then
          begin

            doublep:=pointer(memory);
            for j:=0 to actualread-8 do
            begin
              if (RoundTo(doublep^,-decim)>doublevalue) and (not (isnan(doublep^) or isinfinite(doublep^))) then
              begin
                foundvalue5[found]:=doublep^;
                foundaddress[found]:=Memoryregion[i].BaseAddress+j;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdoublearray:=pointer(foundvalue5switch);
                  foundvalue5switch:=pointer(foundvalue5);
                  foundvalue5:=pointer(tempdoublearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(Memoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                  found:=0;
                end;
              end;

              asm
                inc [doublep]
              end;
            end;
          end;

          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue5,8*found);


//        blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//        blockwrite(Memoryfile,pointer(foundvalue5)^,8*found,actualwrite);
        found:=0;
      end;

      if scanway=SmallerThan then
      begin
        if fastscan then
        for i:=0 to memoryregions do
        begin
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          doublep:=pointer(memory);
          for j:=1 to (actualread div 8) do
          begin
            if (RoundTo(doublep^,-decim)<doublevalue) and (not (isnan(doublep^) or isinfinite(doublep^))) then
            begin
              foundvalue5[found]:=doublep^;
              foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(doublep)-dword(memory));
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempdoublearray:=pointer(foundvalue5switch);
                foundvalue5switch:=pointer(foundvalue5);
                foundvalue5:=pointer(tempdoublearray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointer(foundvalue5)^,8*found,actualwrite);
                found:=0;
              end;
            end;
            inc(doublep);
          end;

          progressbar.stepit;
        end
        else
        for i:=0 to memoryregions do
        begin
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          begin
            doublep:=pointer(memory);
            for j:=0 to actualread-8 do
            begin
              if (RoundTo(doublep^,-decim)<doublevalue) and (not (isnan(doublep^) or isinfinite(doublep^))) then
              begin
                foundvalue5[found]:=doublep^;
                foundaddress[found]:=Memoryregion[i].BaseAddress+j;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdoublearray:=pointer(foundvalue5switch);
                  foundvalue5switch:=pointer(foundvalue5);
                  foundvalue5:=pointer(tempdoublearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(Memoryfile,pointer(foundvalue5)^,8*found,actualwrite);
                  found:=0;
                end;
              end;

              asm
                inc [doublep]
              end;
            end;

          end;

          progressbar.stepit;
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue5,8*found);

//        blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//        blockwrite(Memoryfile,pointer(foundvalue5)^,8*found,actualwrite);
        found:=0;
      end;

      if scanway=ValueBetween then
      begin
        if fastscan then
        for i:=0 to memoryregions do
        begin
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          doublep:=pointer(memory);
          for j:=1 to (actualread div 8) do
          begin
            if (doublep^>=doublevalue) and (doublep^<=doublevalue2) and (not (isnan(doublep^) or isinfinite(doublep^))) then
            begin
              foundvalue5[found]:=doublep^;
              foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(doublep)-dword(memory));
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempdoublearray:=pointer(foundvalue5switch);
                foundvalue5switch:=pointer(foundvalue5);
                foundvalue5:=pointer(tempdoublearray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(Memoryfile,pointer(foundvalue5)^,8*found,actualwrite);
                found:=0;
              end;
            end;
            inc(doublep);
          end;

          progressbar.stepit;
        end
        else
        for i:=0 to memoryregions do
        begin
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          begin
            doublep:=pointer(memory);
            for j:=0 to actualread-8 do
            begin
              if (doublep^>=doublevalue) and (doublep^<=doublevalue2) and (not (isnan(doublep^) or isinfinite(doublep^))) then
              begin
                foundvalue5[found]:=doublep^;
                foundaddress[found]:=Memoryregion[i].BaseAddress+j;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdoublearray:=pointer(foundvalue5switch);
                  foundvalue5switch:=pointer(foundvalue5);
                  foundvalue5:=pointer(tempdoublearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(Memoryfile,pointer(foundvalue5)^,8*found,actualwrite);
                  found:=0;
                end;
              end;

              asm
                inc [doublep]
              end;
            end;

          end;

          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue5,8*found);

//        blockwrite(Addressfile,pointer(foundaddress)^,found*4,actualwrite);
//        blockwrite(Memoryfile,pointer(foundvalue5)^,8*found,actualwrite);
        found:=0;
      end;

      if scanway=Increased_Value then
      begin
        //It's an Increased value scan
        bytep:=pointer(memory);

        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          doublep:=pointer(bytep);
          doublep2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          for i:=1 to (actualread div 8) do
          begin
            if (doublep2^>doublep^) and (not (isnan(doublep2^) or isinfinite(doublep2^))) then  //no rounding down needed here, the value has changed or not.
            begin
              foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(doublep)-dword(bytep));
              foundvalue5[found]:=doublep2^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempdoublearray:=pointer(foundvalue5switch);
                foundvalue5switch:=pointer(foundvalue5);
                foundvalue5:=pointer(tempdoublearray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);


//                blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(NewMemoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                found:=0;
              end;
            end;
            inc(doublep);
            inc(doublep2);
          end;
          progressbar.stepit;
          inc(bytep,memoryregion[j].MemorySize);
        end
        else
        for j:=0 to MemoryRegions do
        begin
          doublep:=pointer(bytep);
          doublep2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);

          for i:=0 to actualread-8 do
          begin
            if (doublep2^>doublep^) and (not (isnan(doublep2^) or isinfinite(doublep2^))) then  //no rounding down needed here, the value has changed or not.
            begin
              foundaddress[found]:=memoryregion[j].BaseAddress+i;
              foundvalue5[found]:=doublep2^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempdoublearray:=pointer(foundvalue5switch);
                foundvalue5switch:=pointer(foundvalue5);
                foundvalue5:=pointer(tempdoublearray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//                blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(NewMemoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                found:=0;
              end;
            end;
            asm
              inc [doublep]
              inc [doublep2]
            end;
          end;
          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue5,8*found);
        
//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue5)^,number*8,actualwrite);
        found:=0;
      end;

      if scanway=Increased_Value_by then
      begin
        //It's an Increased value scan
        bytep:=pointer(memory);

        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          doublep:=pointer(bytep);
          doublep2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          if percentage then
          begin
            for i:=1 to (actualread div 8) do
            begin
              if (doublep2^>=doublep^+doublep^*(doublevalue/100)) and (not (isnan(doublep2^) or isinfinite(doublep2^))) then
              begin
                foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(doublep)-dword(bytep));
                foundvalue5[found]:=doublep2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdoublearray:=pointer(foundvalue5switch);
                  foundvalue5switch:=pointer(foundvalue5);
                  foundvalue5:=pointer(tempdoublearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              inc(doublep);
              inc(doublep2);
            end;
          end
          else
          begin
            for i:=1 to (actualread div 8) do
            begin
              if (doublep2^>doublep^) and (not (isnan(doublep2^) or isinfinite(doublep2^))) and (roundto(doublep2^,-decim)=RoundTo(doublep^+doublevalue,-decim)) then
              begin
                foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(doublep)-dword(bytep));
                foundvalue5[found]:=doublep2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdoublearray:=pointer(foundvalue5switch);
                  foundvalue5switch:=pointer(foundvalue5);
                  foundvalue5:=pointer(tempdoublearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              inc(doublep);
              inc(doublep2);
            end;
          end;
          inc(bytep,memoryregion[j].MemorySize);
          progressbar.stepit;
        end
        else
        for j:=0 to MemoryRegions do
        begin
          doublep:=pointer(bytep);
          doublep2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);

          if actualread>=8 then
          begin
            if percentage then
            begin
              for i:=0 to actualread-8 do
              begin
                if (doublep2^>=doublep^+doublep^*(doublevalue/100)) and (not (isnan(doublep2^) or isinfinite(doublep2^))) then
                begin
                  foundaddress[found]:=memoryregion[j].BaseAddress+i;
                  foundvalue5[found]:=doublep2^;
                  inc(found);
                  if found=number then
                  begin
                    flushthread.datawritten.WaitFor(infinite);

                    tempdwordarray:=pointer(foundaddressswitch);
                    foundaddressswitch:=pointer(foundaddress);
                    foundaddress:=pointer(tempdwordarray);

                    tempdoublearray:=pointer(foundvalue5switch);
                    foundvalue5switch:=pointer(foundvalue5);
                    foundvalue5:=pointer(tempdoublearray);

                    flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//                    blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                    blockwrite(NewMemoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                    found:=0;
                  end;
                end;
                asm
                  inc [doublep]
                  inc [doublep2]
                end;
              end;
            end
            else
            begin
              for i:=0 to actualread-8 do
              begin
                if (doublep2^>doublep^) and (not (isnan(doublep2^) or isinfinite(doublep2^))) and (roundto(doublep2^,-decim)=RoundTo(doublep^+doublevalue,-decim)) then
                begin
                  foundaddress[found]:=memoryregion[j].BaseAddress+i;
                  foundvalue5[found]:=doublep2^;
                  inc(found);
                  if found=number then
                  begin
                    flushthread.datawritten.WaitFor(infinite);

                    tempdwordarray:=pointer(foundaddressswitch);
                    foundaddressswitch:=pointer(foundaddress);
                    foundaddress:=pointer(tempdwordarray);

                    tempdoublearray:=pointer(foundvalue5switch);
                    foundvalue5switch:=pointer(foundvalue5);
                    foundvalue5:=pointer(tempdoublearray);

                    flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//                    blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                    blockwrite(NewMemoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                    found:=0;
                  end;
                end;
                asm
                  inc [doublep]
                  inc [doublep2]
                end;
              end;
            end;
          end;
          progressbar.stepit;
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue5,8*found);
        
//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue5)^,found*8,actualwrite);
        found:=0;
      end;

      if scanway=decreased_Value then
      begin
        //It's an Increased value scan
        bytep:=pointer(memory);

        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          doublep:=pointer(bytep);
          doublep2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          for i:=1 to (actualread div 8) do
          begin
            if (doublep2^<doublep^) and (not (isnan(doublep2^) or isinfinite(doublep2^))) then  //no rounding down needed here, the value has changed or not.
            begin
              foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(doublep)-dword(bytep));
              foundvalue5[found]:=doublep2^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempdoublearray:=pointer(foundvalue5switch);
                foundvalue5switch:=pointer(foundvalue5);
                foundvalue5:=pointer(tempdoublearray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//                blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(NewMemoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                found:=0;
              end;
            end;
            inc(doublep);
            inc(doublep2);
          end;
          inc(bytep,memoryregion[j].MemorySize);
          progressbar.stepit;
        end
        else
        for j:=0 to MemoryRegions do
        begin
          doublep:=pointer(bytep);
          doublep2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);

          if actualread>0 then
          begin

            for i:=0 to actualread-8 do
            begin
              if (doublep2^<doublep^) and (not (isnan(doublep2^) or isinfinite(doublep2^))) then  //no rounding down needed here, the value has changed or not.
              begin
                foundaddress[found]:=memoryregion[j].BaseAddress+i;
                foundvalue5[found]:=doublep2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdoublearray:=pointer(foundvalue5switch);
                  foundvalue5switch:=pointer(foundvalue5);
                  foundvalue5:=pointer(tempdoublearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc [doublep]
                inc [doublep2]
              end;
            end;
          end;
          progressbar.stepit;
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue5,8*found);

//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue5)^,found*8,actualwrite);
        found:=0;
      end;

      if scanway=decreased_Value_by then
      begin
        //It's an Increased value scan
        bytep:=pointer(memory);

        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          doublep:=pointer(bytep);
          doublep2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          if percentage then
          begin
            for i:=1 to (actualread div 8) do
            begin
              if (doublep2^<=doublep^-doublep^*(doublevalue/100)) and (not (isnan(doublep2^) or isinfinite(doublep2^))) then
              begin
                foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(doublep)-dword(bytep));
                foundvalue5[found]:=doublep2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdoublearray:=pointer(foundvalue5switch);
                  foundvalue5switch:=pointer(foundvalue5);
                  foundvalue5:=pointer(tempdoublearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              inc(doublep);
              inc(doublep2);
            end;
          end
          else
          begin
            for i:=1 to (actualread div 8) do
            begin
              if (doublep2^<doublep^) and (not (isnan(doublep2^) or isinfinite(doublep2^))) and (roundto(doublep2^,-decim)=RoundTo(doublep^-doublevalue,-decim)) then
              begin
                foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(doublep)-dword(bytep));
                foundvalue5[found]:=doublep2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdoublearray:=pointer(foundvalue5switch);
                  foundvalue5switch:=pointer(foundvalue5);
                  foundvalue5:=pointer(tempdoublearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              inc(doublep);
              inc(doublep2);
            end;
          end;

          inc(bytep,memoryregion[j].MemorySize);
          progressbar.stepit;
        end
        else
        for j:=0 to MemoryRegions do
        begin
          doublep:=pointer(bytep);
          doublep2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);

          if actualread>0 then
          begin
            if percentage then
            begin
              for i:=0 to actualread-8 do
              begin
                if (doublep2^<=doublep^-doublep^*(doublevalue/100)) and (not (isnan(doublep2^) or isinfinite(doublep2^))) then
                begin
                  foundaddress[found]:=memoryregion[j].BaseAddress+i;
                  foundvalue5[found]:=doublep2^;
                  inc(found);
                  if found=number then
                  begin
                    flushthread.datawritten.WaitFor(infinite);

                    tempdwordarray:=pointer(foundaddressswitch);
                    foundaddressswitch:=pointer(foundaddress);
                    foundaddress:=pointer(tempdwordarray);

                    tempdoublearray:=pointer(foundvalue5switch);
                    foundvalue5switch:=pointer(foundvalue5);
                    foundvalue5:=pointer(tempdoublearray);

                    flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//                    blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                    blockwrite(NewMemoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                    found:=0;
                  end;
                end;
                asm
                  inc [doublep]
                  inc [doublep2]
                end;
              end;
            end
            else
            begin
              for i:=0 to actualread-8 do
              begin
                if (doublep2^<doublep^) and (not (isnan(doublep2^) or isinfinite(doublep2^))) and (roundto(doublep2^,-decim)=RoundTo(doublep^-doublevalue,-decim)) then
                begin
                  foundaddress[found]:=memoryregion[j].BaseAddress+i;
                  foundvalue5[found]:=doublep2^;
                  inc(found);
                  if found=number then
                  begin
                    flushthread.datawritten.WaitFor(infinite);

                    tempdwordarray:=pointer(foundaddressswitch);
                    foundaddressswitch:=pointer(foundaddress);
                    foundaddress:=pointer(tempdwordarray);

                    tempdoublearray:=pointer(foundvalue5switch);
                    foundvalue5switch:=pointer(foundvalue5);
                    foundvalue5:=pointer(tempdoublearray);

                    flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//                    blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                    blockwrite(NewMemoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                    found:=0;
                  end;
                end;
                asm
                  inc [doublep]
                  inc [doublep2]
                end;
              end;
            end;
          end;
          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue5,8*found);
        
//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue5)^,found*8,actualwrite);
        found:=0;
      end;


      if scanway=changed_Value then
      begin
        //It's an Increased value scan
        bytep:=pointer(memory);

        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          doublep:=pointer(bytep);
          doublep2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          for i:=1 to (actualread div 8) do
          begin
            if (doublep2^<>doublep^) and (not (isnan(doublep2^) or isinfinite(doublep2^))) then  //no rounding down needed here, the value has changed or not.
            begin
              foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(doublep)-dword(bytep));
              foundvalue5[found]:=doublep2^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempdoublearray:=pointer(foundvalue5switch);
                foundvalue5switch:=pointer(foundvalue5);
                foundvalue5:=pointer(tempdoublearray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//                blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(NewMemoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                found:=0;
              end;
            end;
            inc(doublep);
            inc(doublep2);
          end;
          inc(bytep,memoryregion[j].MemorySize);
          progressbar.stepit;
        end
        else
        for j:=0 to MemoryRegions do
        begin
          doublep:=pointer(bytep);
          doublep2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);

          if actualread>0 then
          begin

            for i:=0 to actualread-8 do
            begin
              if (doublep2^<>doublep^) and (not (isnan(doublep2^) or isinfinite(doublep2^))) then  //no rounding down needed here, the value has changed or not.
              begin
                foundaddress[found]:=memoryregion[j].BaseAddress+i;
                foundvalue5[found]:=doublep2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdoublearray:=pointer(foundvalue5switch);
                  foundvalue5switch:=pointer(foundvalue5);
                  foundvalue5:=pointer(tempdoublearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
  //                blockwrite(NewMemoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc [doublep]
                inc [doublep2]
              end;
            end;
          end;
          progressbar.stepit;
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue5,8*found);

//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue5)^,found*8,actualwrite);
        found:=0;
      end;

      if scanway=unchanged_Value then
      begin
        //It's an Increased value scan
        bytep:=pointer(memory);

        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          doublep:=pointer(bytep);
          doublep2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          for i:=1 to (actualread div 8) do
          begin
            if (doublep2^=doublep^) and (not (isnan(doublep2^) or isinfinite(doublep2^))) then  //no rounding down needed here, the value has changed or not.
            begin
              foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(doublep)-dword(bytep));
              foundvalue5[found]:=doublep2^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempdoublearray:=pointer(foundvalue5switch);
                foundvalue5switch:=pointer(foundvalue5);
                foundvalue5:=pointer(tempdoublearray);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//                blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(NewMemoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                found:=0;
              end;
            end;
            inc(doublep);
            inc(doublep2);
          end;
          inc(bytep,memoryregion[j].MemorySize);
          progressbar.stepit;
        end
        else
        for j:=0 to MemoryRegions do
        begin
          doublep:=pointer(bytep);
          doublep2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);

          if actualread>0 then
          begin

            for i:=0 to actualread-8 do
            begin
              if (doublep2^=doublep^) and (not (isnan(doublep2^) or isinfinite(doublep2^))) then  //no rounding down needed here, the value has changed or not.
              begin
                foundaddress[found]:=memoryregion[j].BaseAddress+i;
                foundvalue5[found]:=doublep2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdoublearray:=pointer(foundvalue5switch);
                  foundvalue5switch:=pointer(foundvalue5);
                  foundvalue5:=pointer(tempdoublearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc [doublep]
                inc [doublep2]
              end;
            end;
          end;
          progressbar.stepit;
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue5,8*found);

//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue5)^,found*8,actualwrite);
        found:=0;
      end;


    end;

    if valtype=6 then
    begin
      //It's a Int64 Scan
      setlength(foundvalue6,number);
      setlength(foundvalue6switch,number);

      if scanway=Exact_value then
      begin
        //It's an Exact value scan
        if fastscan then
        for i:=0 to memoryregions do
        begin
          int64p:=pointer(memory);
          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          for j:=1 to (actualread div 8) do
          begin
            if int64p^=int64value then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(int64p)-dword(memory));
              inc(found);
              if found=number then
              begin
                if foundvalue6[0]<>int64value then
                begin
                  for k:=0 to number-1 do foundvalue6[k]:=int64value;
                  FoundIsFilled:=true;
                end;

                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempint64array:=pointer(foundvalue6switch);
                foundvalue6switch:=pointer(foundvalue6);
                foundvalue6:=pointer(tempint64array);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

//                blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(newMemoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                found:=0;
              end;
            end;
            inc(int64p);
          end;
          progressbar.stepit;
        end
        else
        for i:=0 to memoryregions do
        begin
          int64p:=pointer(memory);

          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          if actualread>=8 then
          begin
            for j:=0 to actualread-8 do
            begin
              if int64p^=int64value then
              begin
                foundaddress[found]:=Memoryregion[i].BaseAddress+j;
                inc(found);
                if found=number then
                begin
                  if foundvalue6[0]<>int64value then
                  begin
                    for k:=0 to number-1 do foundvalue6[k]:=int64value;
                    FoundIsFilled:=true;
                  end;

                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempint64array:=pointer(foundvalue6switch);
                  foundvalue6switch:=pointer(foundvalue6);
                  foundvalue6:=pointer(tempint64array);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);


//                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(newMemoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc [int64p]
              end;
            end;
          end;
          progressbar.stepit;
        end;
        if foundvalue6[0]<>int64value then
        begin
          for k:=0 to found-1 do foundvalue6[k]:=int64value;
          FoundIsFilled:=true;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue6,8*found);

//        blockwrite(newAddressfile,pointer(foundaddress)^,found*4,actualwrite);
//        blockwrite(newMemoryfile,pointer(foundvalue6)^,8*found,actualwrite);
        found:=0;
      end;

      if scanway=biggerthan then
      begin
        if fastscan then
        for i:=0 to memoryregions do
        begin
          int64p:=pointer(memory);

          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          for j:=1 to (actualread div 8) do
          begin
            if int64p^>int64value then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(int64p)-dword(memory));
              foundvalue6[found]:=hi64;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempint64array:=pointer(foundvalue6switch);
                foundvalue6switch:=pointer(foundvalue6);
                foundvalue6:=pointer(tempint64array);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

//                blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(newMemoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                found:=0;
              end;
            end;

            inc(int64p);
          end;
          progressbar.stepit;
        end
        else
        for i:=0 to memoryregions do
        begin
          int64p:=pointer(memory);

          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          if actualread>=8 then
          begin

            for j:=0 to actualread-8 do
            begin
              if int64p^>int64value then
              begin
                foundaddress[found]:=Memoryregion[i].BaseAddress+j;
                foundvalue6[found]:=hi64;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempint64array:=pointer(foundvalue6switch);
                  foundvalue6switch:=pointer(foundvalue6);
                  foundvalue6:=pointer(tempint64array);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);


//                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(newMemoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc [int64p]
              end;

            end;
          end;
          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue6,8*found);


//        blockwrite(newAddressfile,pointer(foundaddress)^,found*4,actualwrite);
//        blockwrite(newMemoryfile,pointer(foundvalue6)^,8*found,actualwrite);
        found:=0;
      end;

      if scanway=SmallerThan then
      begin
        if fastscan then
        for i:=0 to memoryregions do
        begin
          int64p:=pointer(memory);

          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          for j:=1 to (actualread div 8) do
          begin
            if int64p^<int64value then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(int64p)-dword(memory));
              foundvalue6[found]:=hi64;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempint64array:=pointer(foundvalue6switch);
                foundvalue6switch:=pointer(foundvalue6);
                foundvalue6:=pointer(tempint64array);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

//                blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(newMemoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                found:=0;
              end;
            end;
            inc(int64p);
          end;
          progressbar.stepit;
        end
        else
        for i:=0 to memoryregions do
        begin
          int64p:=pointer(memory);

          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          if actualread>=4 then
          begin
            for j:=0 to actualread-8 do
            begin
              if int64p^<int64value then
              begin
                foundaddress[found]:=Memoryregion[i].BaseAddress+j;
                foundvalue6[found]:=hi64;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempint64array:=pointer(foundvalue6switch);
                  foundvalue6switch:=pointer(foundvalue6);
                  foundvalue6:=pointer(tempint64array);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

//                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(newMemoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc [int64p]
              end;
            end;
          end;
          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue6,8*found);
        
//        blockwrite(newAddressfile,pointer(foundaddress)^,found*4,actualwrite);
//        blockwrite(newMemoryfile,pointer(foundvalue6)^,8*found,actualwrite);
        found:=0;
      end;

      if scanway=ValueBetween then
      begin
        if fastscan then
        for i:=0 to memoryregions do
        begin
          int64p:=pointer(memory);

          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          for j:=1 to (actualread div 8) do
          begin
            if (int64p^>=int64value) and (int64p^<=int64value2) then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+(dword(int64p)-dword(memory));
              foundvalue6[found]:=hi64;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempint64array:=pointer(foundvalue6switch);
                foundvalue6switch:=pointer(foundvalue6);
                foundvalue6:=pointer(tempint64array);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

//                blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(newMemoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                found:=0;
              end;
            end;
            inc(int64p);
          end;
          progressbar.stepit;
        end
        else
        for i:=0 to memoryregions do
        begin
          int64p:=pointer(memory);

          readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),Memory,Memoryregion[i].MemorySize,actualread);
          if actualread>=4 then
          begin
            for j:=0 to actualread-8 do
            begin
              if (int64p^>=int64value) and (int64p^<=int64value2) then
              begin
                foundaddress[found]:=Memoryregion[i].BaseAddress+j;
                foundvalue6[found]:=hi64;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempint64array:=pointer(foundvalue6switch);
                  foundvalue6switch:=pointer(foundvalue6);
                  foundvalue6:=pointer(tempint64array);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);


//                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(newMemoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc [int64p]
              end;
            end;
          end;
          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue6,8*found);
        
//        blockwrite(newAddressfile,pointer(foundaddress)^,found*4,actualwrite);
//        blockwrite(newMemoryfile,pointer(foundvalue6)^,8*found,actualwrite);
        found:=0;
      end;


      if scanway=Increased_Value then
      begin
        bytep:=pointer(memory);

        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          int64p:=pointer(bytep);
          int64p2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          for i:=1 to (actualread div 8) do
          begin
            if int64p2^>int64p^ then
            begin
              foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(int64p)-dword(bytep));
              foundvalue6[found]:=int64p2^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempint64array:=pointer(foundvalue6switch);
                foundvalue6switch:=pointer(foundvalue6);
                foundvalue6:=pointer(tempint64array);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

//                blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(NewMemoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                found:=0;
              end;
            end;
            inc(int64p);
            inc(int64p2);
          end;
          inc(bytep,memoryregion[j].MemorySize);

          progressbar.stepit;
        end
        else
        for j:=0 to MemoryRegions do
        begin
          int64p:=pointer(bytep);
          int64p2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);

          if actualread>0 then
          begin
            for i:=0 to actualread-8 do
            begin
              if int64p2^>int64p^ then
              begin
                foundaddress[found]:=memoryregion[j].BaseAddress+i;
                foundvalue6[found]:=int64p2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempint64array:=pointer(foundvalue6switch);
                  foundvalue6switch:=pointer(foundvalue6);
                  foundvalue6:=pointer(tempint64array);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc [int64p]
                inc [int64p2]
              end;
            end;
          end;
          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue6,8*found);
        
//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue6)^,found*8,actualwrite);
        found:=0;
      end;

      if scanway=Increased_Value_by then
      begin
        bytep:=pointer(memory);

        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          int64p:=pointer(bytep);
          int64p2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          if percentage then
          begin
            for i:=1 to (actualread div 8) do
            begin
              if int64p2^>=int64p^+trunc(int64p^*(int64value/100)) then
              begin
                foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(int64p)-dword(bytep));
                foundvalue6[found]:=int64p2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempint64array:=pointer(foundvalue6switch);
                  foundvalue6switch:=pointer(foundvalue6);
                  foundvalue6:=pointer(tempint64array);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              inc(int64p);
              inc(int64p2);
            end;
          end
          else
          begin
            for i:=1 to (actualread div 8) do
            begin
              if int64p2^=int64p^+int64value then
              begin
                foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(int64p)-dword(bytep));
                foundvalue6[found]:=int64p2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempint64array:=pointer(foundvalue6switch);
                  foundvalue6switch:=pointer(foundvalue6);
                  foundvalue6:=pointer(tempint64array);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              inc(int64p);
              inc(int64p2);
            end;
          end;
          inc(bytep,memoryregion[j].MemorySize);
          progressbar.stepit;
        end
        else
        for j:=0 to MemoryRegions do
        begin
          int64p:=pointer(bytep);
          int64p2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);

          if actualread>0 then
          begin

            if percentage then
            begin
              for i:=0 to actualread-8 do
              begin
                if int64p2^>=int64p^+trunc(int64p^*(int64value/100)) then
                begin
                  foundaddress[found]:=memoryregion[j].BaseAddress+i;
                  foundvalue6[found]:=int64p2^;
                  inc(found);
                  if found=number then
                  begin
                    flushthread.datawritten.WaitFor(infinite);

                    tempdwordarray:=pointer(foundaddressswitch);
                    foundaddressswitch:=pointer(foundaddress);
                    foundaddress:=pointer(tempdwordarray);

                    tempint64array:=pointer(foundvalue6switch);
                    foundvalue6switch:=pointer(foundvalue6);
                    foundvalue6:=pointer(tempint64array);

                    flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

//                    blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                    blockwrite(NewMemoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                    found:=0;
                  end;
                end;
                asm
                  inc [int64p]
                  inc [int64p2]
                end;
              end;

            end
            else
            begin
              for i:=0 to actualread-8 do
              begin
                if int64p2^=int64p^+int64value then
                begin
                  foundaddress[found]:=memoryregion[j].BaseAddress+i;
                  foundvalue6[found]:=int64p2^;
                  inc(found);
                  if found=number then
                  begin
                    flushthread.datawritten.WaitFor(infinite);

                    tempdwordarray:=pointer(foundaddressswitch);
                    foundaddressswitch:=pointer(foundaddress);
                    foundaddress:=pointer(tempdwordarray);

                    tempint64array:=pointer(foundvalue6switch);
                    foundvalue6switch:=pointer(foundvalue6);
                    foundvalue6:=pointer(tempint64array);

                    flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

//                    blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                    blockwrite(NewMemoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                    found:=0;
                  end;
                end;
                asm
                  inc [int64p]
                  inc [int64p2]
                end;
              end;
            end;

          end;
          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue6,8*found);
        
//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue6)^,found*8,actualwrite);
        found:=0;
      end;


      if scanway=decreased_Value then
      begin
        bytep:=pointer(memory);

        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          int64p:=pointer(bytep);
          int64p2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          for i:=1 to (actualread div 8) do
          begin
            if int64p2^<int64p^ then
            begin
              foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(int64p)-dword(bytep));
              foundvalue6[found]:=int64p2^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempint64array:=pointer(foundvalue6switch);
                foundvalue6switch:=pointer(foundvalue6);
                foundvalue6:=pointer(tempint64array);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

//                blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(NewMemoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                found:=0;
              end;
            end;
            inc(int64p);
            inc(int64p2);
          end;
          inc(bytep,memoryregion[j].MemorySize);
          progressbar.stepit;
        end
        else
        for j:=0 to MemoryRegions do
        begin
          int64p:=pointer(bytep);
          int64p2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);

          if actualread>0 then
          begin

            for i:=0 to actualread-8 do
            begin
              if int64p2^<int64p^ then
              begin
                foundaddress[found]:=memoryregion[j].BaseAddress+i;
                foundvalue6[found]:=int64p2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempint64array:=pointer(foundvalue6switch);
                  foundvalue6switch:=pointer(foundvalue6);
                  foundvalue6:=pointer(tempint64array);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc [int64p]
                inc [int64p2]
              end;
            end;
          end;
          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue6,8*found);
        
//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue6)^,found*8,actualwrite);
        found:=0;
      end;


      if scanway=decreased_Value_by then
      begin
        bytep:=pointer(memory);

        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          int64p:=pointer(bytep);
          int64p2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          if percentage then
          begin

            for i:=1 to (actualread div 8) do
            begin
              if int64p2^<=int64p^-trunc(int64p^*(int64value/100)) then
              begin
                foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(int64p)-dword(bytep));
                foundvalue6[found]:=int64p2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempint64array:=pointer(foundvalue6switch);
                  foundvalue6switch:=pointer(foundvalue6);
                  foundvalue6:=pointer(tempint64array);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              inc(int64p);
              inc(int64p2);
            end;
          end
          else
          begin

            for i:=1 to (actualread div 8) do
            begin
              if int64p2^=int64p^-int64value then
              begin
                foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(int64p)-dword(bytep));
                foundvalue6[found]:=int64p2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempint64array:=pointer(foundvalue6switch);
                  foundvalue6switch:=pointer(foundvalue6);
                  foundvalue6:=pointer(tempint64array);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);
                
//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              inc(int64p);
              inc(int64p2);
            end;
          end;
          inc(bytep,memoryregion[j].MemorySize);

          progressbar.stepit;
        end
        else
        for j:=0 to MemoryRegions do
        begin
          int64p:=pointer(bytep);
          int64p2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);

          if actualread>0 then
          begin

            if percentage then
            begin
              for i:=0 to actualread-8 do
              begin
                if int64p2^<=int64p^-trunc(int64p^*(int64value/100)) then
                begin
                  foundaddress[found]:=memoryregion[j].BaseAddress+i;
                  foundvalue6[found]:=int64p2^;
                  inc(found);
                  if found=number then
                  begin
                    flushthread.datawritten.WaitFor(infinite);

                    tempdwordarray:=pointer(foundaddressswitch);
                    foundaddressswitch:=pointer(foundaddress);
                    foundaddress:=pointer(tempdwordarray);

                    tempint64array:=pointer(foundvalue6switch);
                    foundvalue6switch:=pointer(foundvalue6);
                    foundvalue6:=pointer(tempint64array);

                    flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

//                    blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                    blockwrite(NewMemoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                    found:=0;
                  end;
                end;
                asm
                  inc [int64p]
                  inc [int64p2]
                end;
              end;

            end
            else
            begin
              for i:=0 to actualread-8 do
              begin
                if int64p2^=int64p^-int64value then
                begin
                  foundaddress[found]:=memoryregion[j].BaseAddress+i;
                  foundvalue6[found]:=int64p2^;
                  inc(found);
                  if found=number then
                  begin
                    flushthread.datawritten.WaitFor(infinite);

                    tempdwordarray:=pointer(foundaddressswitch);
                    foundaddressswitch:=pointer(foundaddress);
                    foundaddress:=pointer(tempdwordarray);

                    tempint64array:=pointer(foundvalue6switch);
                    foundvalue6switch:=pointer(foundvalue6);
                    foundvalue6:=pointer(tempint64array);

                    flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

//                    blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                    blockwrite(NewMemoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                    found:=0;
                  end;
                end;
                asm
                  inc [int64p]
                  inc [int64p2]
                end;
              end;
            end;
          end;
          progressbar.stepit;
        end;
        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
        blockwrite(NewMemoryfile,pointer(foundvalue6)^,found*8,actualwrite);
        found:=0;
      end;

      if scanway=changed_value then
      begin
        bytep:=pointer(memory);

        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          int64p:=pointer(bytep);
          int64p2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          for i:=1 to (actualread div 8) do
          begin
            if int64p2^<>int64p^ then
            begin
              foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(int64p)-dword(bytep));
              foundvalue6[found]:=int64p2^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempint64array:=pointer(foundvalue6switch);
                foundvalue6switch:=pointer(foundvalue6);
                foundvalue6:=pointer(tempint64array);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

//                blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(NewMemoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                found:=0;
              end;
            end;
            inc(int64p);
            inc(int64p2);
          end;
          inc(bytep,memoryregion[j].MemorySize);
          progressbar.stepit;
        end
        else
        for j:=0 to MemoryRegions do
        begin
          int64p:=pointer(bytep);
          int64p2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);

          if actualread>0 then
          begin

            for i:=0 to actualread-8 do
            begin
              if int64p2^<>int64p^ then
              begin
                foundaddress[found]:=memoryregion[j].BaseAddress+i;
                foundvalue6[found]:=int64p2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempint64array:=pointer(foundvalue6switch);
                  foundvalue6switch:=pointer(foundvalue6);
                  foundvalue6:=pointer(tempint64array);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc [int64p]
                inc [int64p2]
              end;
            end;
          end;
          progressbar.stepit;
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue6,8*found);
        
//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue6)^,found*8,actualwrite);
        found:=0;
      end;

      if scanway=unchanged_Value then
      begin
        bytep:=pointer(memory);

        if fastscan then
        for j:=0 to MemoryRegions do
        begin
          int64p:=pointer(bytep);
          int64p2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);

          for i:=1 to (actualread div 8) do
          begin
            if int64p2^=int64p^ then
            begin
              foundaddress[found]:=Memoryregion[j].BaseAddress+(dword(int64p)-dword(bytep));
              foundvalue6[found]:=int64p2^;
              inc(found);
              if found=number then
              begin
                flushthread.datawritten.WaitFor(infinite);

                tempdwordarray:=pointer(foundaddressswitch);
                foundaddressswitch:=pointer(foundaddress);
                foundaddress:=pointer(tempdwordarray);

                tempint64array:=pointer(foundvalue6switch);
                foundvalue6switch:=pointer(foundvalue6);
                foundvalue6:=pointer(tempint64array);

                flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

//                blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                blockwrite(NewMemoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                found:=0;
              end;
            end;
            inc(int64p);
            inc(int64p2);
          end;
          progressbar.stepit;
          inc(bytep,memoryregion[j].MemorySize);

        end
        else
        for j:=0 to MemoryRegions do
        begin
          int64p:=pointer(bytep);
          int64p2:=pointer(memory2);
          readprocessmemory(processhandle,pointer(memoryregion[j].BaseAddress),memory2,memoryregion[j].MemorySize,actualread);
          inc(bytep,memoryregion[j].MemorySize);

          if actualread>0 then
          begin

            for i:=0 to actualread-8 do
            begin
              if int64p2^=int64p^ then
              begin
                foundaddress[found]:=memoryregion[j].BaseAddress+i;
                foundvalue6[found]:=int64p2^;
                inc(found);
                if found=number then
                begin
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempint64array:=pointer(foundvalue6switch);
                  foundvalue6switch:=pointer(foundvalue6);
                  foundvalue6:=pointer(tempint64array);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

//                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
//                  blockwrite(NewMemoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc [int64p]
                inc [int64p2]
              end;
            end;
          end;
          progressbar.stepit;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue6,8*found);
        
//        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
//        blockwrite(NewMemoryfile,pointer(foundvalue6)^,found*8,actualwrite);
        found:=0;
      end;

    end;

    //if valtype=8 then blah!
    //array of byte does not have a advanced scan
    finishflushing;

    freememory;
  end else
  begin
   //normal scan
   //addressfile contains the addresses of each location and memoryfile contains the value! (at least, thats what is suposed to be :) )

    if valtype=5 then number:=((filesize(addressfile)-7) div sizeof(Tbitaddress)) else
                      number:=((filesize(addressfile)-7) div 4);

    if number=0 then
    begin
      closefile(Addressfile);
      closefile(memoryfile);
      closefile(NewAddressFile);
      closefile(NewMemoryfile);
      freememory;
      result:=0;
      exit;
    end;

    number:=number div 10; //do it in at least 10 steps
    if number=0 then
    begin
      if valtype=5 then number:=(filesize(addressfile)-7) div sizeof(Tbitaddress) else
                        number:=(filesize(addressfile)-7) div 4;
    end;

    if number>buffersize then
    begin
      number:=buffersize;
    end;

    if number=0 then number:=2; //just do it, a number of 0 crashes

    setlength(foundaddress,number);
    setlength(foundaddressswitch,number);

    if valtype=5 then
    begin
      //it's a bit scan

      progressbar.Max:=(filesize(addressfile)-7) div sizeof(Tbitaddress);
      progressbar.Position:=0;
      setlength(searchaddressb,number);
      setlength(foundaddressb,number);

      blockwrite(newmemoryfile,nrofbits,4,actualread);
      blockwrite(newmemoryfile,pointer(bitscan)^,nrofbits,actualread);

      setlength(bytearray,(nrofbits div 8)+2);

      if scanway=Exact_Value then
      begin
        actualread:=number*sizeof(Tbitaddress);
        while actualread=number*sizeof(Tbitaddress) do
        begin
          blockread(addressfile,pointer(searchaddressb)^,number*sizeof(Tbitaddress),actualread);
          for i:=0 to (actualread div sizeof(Tbitaddress))-1 do
          begin
            //recalculate the address
            startbit:=searchaddressb[i].bit+bitoffsetchange;

            while startbit<0 do
            begin
              dec(searchaddressb[i].address);
              inc(startbit,8);
            end;

            while startbit>7 do
            begin
              inc(searchaddressb[i].address);
              dec(startbit,8);
            end;

            searchaddressb[i].bit:=startbit;

            //read the memory
            readprocessmemory(processhandle,pointer(searchaddressb[i].address),@bytearray[0],(1+(startbit+nrofbits) div 8),actualwrite);

            extra:=true;
            l:=0;
            for j:=0 to actualwrite-1 do
            begin
              for k:=startbit to 7 do
              begin
                if bitscan[l]<>2 then
                if getbit(k,bytearray[j])<>bitscan[l] then
                begin
                  extra:=false;
                  break;
                end;

                inc(l);
                if l=nrofbits then  //correct
                begin
                  foundaddressb[found].address:=searchaddressb[i].address;
                  foundaddressb[found].bit:=searchaddressb[i].bit;

                  inc(found);
                  if found=number then
                  begin
                    blockwrite(newAddressfile,pointer(foundaddressB)^,found*(sizeof(Tbitaddress)),actualwrite);
                    found:=0;
                  end;
                end;
              end;
              startbit:=0;

              if not extra then break;
            end;
          end;
          progressbar.StepIt;
        end;
        blockwrite(newAddressfile,pointer(foundaddressB)^,found*(sizeof(Tbitaddress)),actualwrite);
      end; //no other scan methods for this(yet, perhaps when I feel like it...)


      closefile(Addressfile);
      closefile(memoryfile);
      closefile(NewAddressFile);
      closefile(NewMemoryFile);

      //create undo data
      deletefile(CheatEngineDir+'Memory.UNDO');
      deletefile(CheatEngineDir+'Addresses.UNDO');
      renamefile(CheatEngineDir+'Memory.tmp',cheatenginedir+'Memory.UNDO');
      renamefile(CheatEngineDir+'Addresses.tmp',CheatEngineDir+'Addresses.UNDO');
      renamefile(CheatEnginedir+'ADDRESSES2.TMP',Cheatenginedir+'ADDRESSES.TMP');
      renamefile(Cheatenginedir+'MEMORY2.TMP',Cheatenginedir+'MEMORY.TMP');

      try
        try
          resulthelper:=tfilestream.Create(CheatEngineDir+'Addresses.tmp',fmopenread,fmsharedenynone);
          result:=(resulthelper.Size-7) div sizeof(Tbitaddress);
        finally
          resulthelper.free;
        end;
      except
        reset(addressfile,1);
        result:=(filesize(addressfile)-7) div sizeof(Tbitaddress);
        closefile(addressfile);
      end;

      freememory;
      exit;
    end;

    if valtype=7 then
    begin
      //it's a text scan
      total:=(filesize(addressfile)-7) div 4;

      progressbar.Max:=total;
      progressbar.Position:=0;

      setlength(searchaddress,number);

      if scanway=Exact_Value then //no other method
      begin
        dwordvalue:=length(scantext);
        getmem(strb,dwordvalue+1);
        getmem(strc,dwordvalue*2+2);

        blockwrite(Newmemoryfile,pointer(scantext)^,dwordvalue,actualread);

        actualread:=number*4;
        ws:=scantext;

        while actualread=number*4 do
        begin
          blockread(addressfile,pointer(searchaddress)^,number*4,actualread);
          for i:=0 to (actualread div 4)-1 do
          begin
            if unicode then
            begin
              readprocessmemory(processhandle,pointer(searchaddress[i]),strc,dwordvalue*2,actualwrite);  //just call this write
              strc[dwordvalue]:=#0;
              ok:=(actualwrite=dwordvalue*2) and (widecomparetext(ws,strc)=0);

            end
            else
            begin
              readprocessmemory(processhandle,pointer(searchaddress[i]),strb,dwordvalue,actualwrite);  //just call this write
              strb[dwordvalue]:=chr(0);


              ok:=(actualwrite=dwordvalue) and ((((option) and (strb=scantext)) or
                  ((not option) and (uppercase(strb)=uppercase(scantext)))
                  ));
            end;

            if ok then
            begin
              foundaddress[found]:=SearchAddress[i];
              inc(found);
              if found=number then
              begin
                //write the currently found addresses to disk
                  blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  found:=0;
              end;
            end;
          end;
          progressbar.StepBy(number);
        end;
        blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
        progressbar.Position:=Total;
      end;


      freemem(strb); //got to save the memory...
      freemem(strc);
    end;

    if valtype=0 then
    begin
      //It's a Byte Scan
      total:=filesize(memoryfile);

      progressbar.Max:=total;
      progressbar.Position:=0;

      setlength(searchaddress,number);
      setlength(searchaddressswitch,number);
      setlength(foundvalue1,number);
      setlength(foundvalue1switch,number);
      setlength(previousmemory1,number);
      setlength(previousmemory1switch,number);
      setlength(bytesscanned,4096);

      foundvalue1[0]:=bytevalue+1;
      foundvalue1switch[0]:=bytevalue+1;


      if scanway=Exact_Value then
      begin
        //It's an Exact value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);



        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          //lets start reading the next block while i'm scanning this block
          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            bytesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(bytesscannedstart+$1000)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(bytesscannedstart),@bytesscanned[0],$1000,actualwrite) then
                j:=$1000
              else
              begin
                while searchaddress[i]<(bytesscannedstart+$1000) do inc(i);
                continue;
              end;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),@bytesscanned[searchaddress[i]-bytesscannedstart],1,actualwrite) then
                j:=1 //only 1
              else
              begin
                inc(i);
                continue;
              end;
            end;

            while (j>0) and (i<l) and (searchaddress[i]<(bytesscannedstart+$1000)) do
            begin
            //readprocessmemory(processhandle,pointer(searchaddress[i]),addr(ByteScanned),1,actualwrite);

              if bytesscanned[searchaddress[i]-bytesscannedstart]=ByteValue then
              begin
                foundaddress[found]:=SearchAddress[i];
                inc(found);
                if found=number then
                begin
                  if foundvalue1[0]<>bytevalue then
                  begin
                    fillmemory(@foundvalue1[0],number,bytevalue);
                    FoundIsFilled:=true;
                  end;
                  //write the currently found addresses to disk

                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempbytearray:=pointer(foundvalue1switch);
                  foundvalue1switch:=pointer(foundvalue1);
                  foundvalue1:=pointer(tempbytearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue1switch,number);
                  found:=0;
                end;
              end;
              inc(i);
              dec(j);
            end;

          end;
          progressbar.StepBy(actualread div 4);
        end;

        if not foundvalue1[0]<>bytevalue then
        begin
          fillmemory(@foundvalue1[0],number,bytevalue);
          FoundIsFilled:=true;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue1,found);

        progressbar.position:=total;
      end;

      if scanway=BiggerThan then
      begin
        //It's an bigger than scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);
        i:=0;

        actualread:=4*number;
        while actualread=4*number do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          //lets start reading the next block while i'm scanning this block
          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            bytesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(bytesscannedstart+$1000)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(bytesscannedstart),@bytesscanned[0],$1000,actualwrite) then
                j:=$1000
              else
              begin
                while searchaddress[i]<(bytesscannedstart+$1000) do inc(i);
                continue;
              end;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),@bytesscanned[searchaddress[i]-bytesscannedstart],1,actualwrite) then
                j:=1 //only 1
              else
              begin
                inc(i);
                continue;
              end;
            end;

            while (j>0) and (i<l) and (searchaddress[i]<(bytesscannedstart+$1000)) do
            begin
              if (BytesScanned[searchaddress[i]-bytesscannedstart]>bytevalue) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue1[found]:=BytesScanned[searchaddress[i]-bytesscannedstart];
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempbytearray:=pointer(foundvalue1switch);
                  foundvalue1switch:=pointer(foundvalue1);
                  foundvalue1:=pointer(tempbytearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue1switch,number);

                  found:=0;
                end;
              end;
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue1,found);
        progressbar.position:=total;
      end;

      if scanway=SmallerThan then
      begin
        //It's an smaller than scan

        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

        actualread:=4*number;
        while actualread=4*number do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          //lets start reading the next block while i'm scanning this block
          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            bytesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(bytesscannedstart+$1000)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(bytesscannedstart),@bytesscanned[0],$1000,actualwrite) then
                j:=$1000
              else
              begin
                while searchaddress[i]<(bytesscannedstart+$1000) do inc(i);
                continue;
              end;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),@bytesscanned[searchaddress[i]-bytesscannedstart],1,actualwrite) then
                j:=1 //only 1
              else
              begin
                inc(i);
                continue;
              end;
            end;

            while (j>0) and (i<l) and (searchaddress[i]<(bytesscannedstart+$1000)) do
            begin
              if (BytesScanned[searchaddress[i]-bytesscannedstart]<bytevalue) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue1[found]:=BytesScanned[searchaddress[i]-bytesscannedstart];
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempbytearray:=pointer(foundvalue1switch);
                  foundvalue1switch:=pointer(foundvalue1);
                  foundvalue1:=pointer(tempbytearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue1switch,number);

                  found:=0;
                end;
              end;
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue1,found);
        progressbar.position:=total;
      end;

      if scanway=ValueBetween then
      begin
        //It's an between

        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);
        i:=0;

        actualread:=4*number;
        while actualread=4*number do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          //lets start reading the next block while i'm scanning this block
          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            bytesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(bytesscannedstart+$1000)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(bytesscannedstart),@bytesscanned[0],$1000,actualwrite) then
                j:=$1000
              else
              begin
                while searchaddress[i]<(bytesscannedstart+$1000) do inc(i);
                continue;
              end;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),@bytesscanned[searchaddress[i]-bytesscannedstart],1,actualwrite) then
                j:=1 //only 1
              else
              begin
                inc(i);
                continue;
              end;
            end;

            while (j>0) and (i<l) and (searchaddress[i]<(bytesscannedstart+$1000)) do
            begin
              if (BytesScanned[searchaddress[i]-bytesscannedstart]>=bytevalue) and (BytesScanned[searchaddress[i]-bytesscannedstart]<=bytevalue2) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue1[found]:=BytesScanned[searchaddress[i]-bytesscannedstart];
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempbytearray:=pointer(foundvalue1switch);
                  foundvalue1switch:=pointer(foundvalue1);
                  foundvalue1:=pointer(tempbytearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue1switch,number);

                  found:=0;
                end;
              end;
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue1,found);
        progressbar.position:=total;
      end;


      if scanway=Increased_Value then
      begin
        //It's an Increased value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory1switch,number);


        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempdwordarray:=pointer(previousmemory1switch);
          previousmemory1switch:=pointer(previousmemory1);
          previousmemory1:=pointer(tempdwordarray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory1switch,number);

          bytep:=@previousmemory1[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            bytesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(bytesscannedstart+$1000)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(bytesscannedstart),@bytesscanned[0],$1000,actualwrite) then
                j:=$1000
              else
              begin
                while searchaddress[i]<(bytesscannedstart+$1000) do inc(i);
                continue;
              end;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),@bytesscanned[searchaddress[i]-bytesscannedstart],1,actualwrite) then
                j:=1 //only 1
              else
              begin
                inc(i);
                continue;
              end;
            end;

            while (j>0) and (i<l) and (searchaddress[i]<(bytesscannedstart+$1000)) do
            begin
              if (BytesScanned[searchaddress[i]-bytesscannedstart]>bytep^) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue1[found]:=BytesScanned[searchaddress[i]-bytesscannedstart];
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempbytearray:=pointer(foundvalue1switch);
                  foundvalue1switch:=pointer(foundvalue1);
                  foundvalue1:=pointer(tempbytearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue1switch,number);
                  found:=0;
                end;
              end;
              inc(bytep);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue1,found);
        progressbar.position:=total;
      end;


      if scanway=Increased_Value_By then
      begin
        //It's an Increased value by scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory1switch,number);


        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempdwordarray:=pointer(previousmemory1switch);
          previousmemory1switch:=pointer(previousmemory1);
          previousmemory1:=pointer(tempdwordarray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory1switch,number);

          bytep:=@previousmemory1[0];

          i:=0;
          l:=actualread div 4;



          while i<l do
          begin
            bytesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(bytesscannedstart+$1000)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(bytesscannedstart),@bytesscanned[0],$1000,actualwrite) then
                j:=$1000
              else
              begin
                while searchaddress[i]<(bytesscannedstart+$1000) do inc(i);
                continue;
              end;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),@bytesscanned[searchaddress[i]-bytesscannedstart],1,actualwrite) then
                j:=1 //only 1
              else
              begin
                inc(i);
                continue;
              end;
            end;

            while (j>0) and (i<l) and (searchaddress[i]<(bytesscannedstart+$1000)) do
            begin
              if (
                 ((not percentage) and (BytesScanned[searchaddress[i]-bytesscannedstart]=bytep^+bytevalue))
                 or
                 (percentage and (BytesScanned[searchaddress[i]-bytesscannedstart]>=bytep^+trunc(bytep^*(bytevalue/100))))
               ) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue1[found]:=BytesScanned[searchaddress[i]-bytesscannedstart];
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempbytearray:=pointer(foundvalue1switch);
                  foundvalue1switch:=pointer(foundvalue1);
                  foundvalue1:=pointer(tempbytearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue1switch,number);
                  found:=0;
                end;
              end;
              inc(bytep);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue1,found);
        progressbar.position:=total;
      end;

      if scanway=Decreased_Value then
      begin
        //It's an Decreased value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory1switch,number);


        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempdwordarray:=pointer(previousmemory1switch);
          previousmemory1switch:=pointer(previousmemory1);
          previousmemory1:=pointer(tempdwordarray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory1switch,number);

          bytep:=@previousmemory1[0];

          i:=0;
          l:=actualread div 4;



          while i<l do
          begin
            bytesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(bytesscannedstart+$1000)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(bytesscannedstart),@bytesscanned[0],$1000,actualwrite) then
                j:=$1000
              else
              begin
                while searchaddress[i]<(bytesscannedstart+$1000) do inc(i);
                continue;
              end;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),@bytesscanned[searchaddress[i]-bytesscannedstart],1,actualwrite) then
                j:=1 //only 1
              else
              begin
                inc(i);
                continue;
              end;
            end;

            while (j>0) and (i<l) and (searchaddress[i]<(bytesscannedstart+$1000)) do
            begin
              if (BytesScanned[searchaddress[i]-bytesscannedstart]<bytep^) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue1[found]:=BytesScanned[searchaddress[i]-bytesscannedstart];
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempbytearray:=pointer(foundvalue1switch);
                  foundvalue1switch:=pointer(foundvalue1);
                  foundvalue1:=pointer(tempbytearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue1switch,number);
                  found:=0;
                end;
              end;
              inc(bytep);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue1,found);
        progressbar.position:=total;
      end;


      if scanway=Decreased_Value_By then
      begin
        //It's an Increased value by scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory1switch,number);


        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempdwordarray:=pointer(previousmemory1switch);
          previousmemory1switch:=pointer(previousmemory1);
          previousmemory1:=pointer(tempdwordarray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory1switch,number);

          bytep:=@previousmemory1[0];

          i:=0;
          l:=actualread div 4;



          while i<l do
          begin
            bytesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(bytesscannedstart+$1000)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(bytesscannedstart),@bytesscanned[0],$1000,actualwrite) then
                j:=$1000
              else
              begin
                while searchaddress[i]<(bytesscannedstart+$1000) do inc(i);
                continue;
              end;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),@bytesscanned[searchaddress[i]-bytesscannedstart],1,actualwrite) then
                j:=1 //only 1
              else
              begin
                inc(i);
                continue;
              end;
            end;

            while (j>0) and (i<l) and (searchaddress[i]<(bytesscannedstart+$1000)) do
            begin
              if (
                 ((not percentage) and (BytesScanned[searchaddress[i]-bytesscannedstart]=bytep^-bytevalue))
                 or
                 (percentage and (BytesScanned[searchaddress[i]-bytesscannedstart]<=bytep^-trunc(bytep^*(bytevalue/100))))
               ) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue1[found]:=BytesScanned[searchaddress[i]-bytesscannedstart];
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempbytearray:=pointer(foundvalue1switch);
                  foundvalue1switch:=pointer(foundvalue1);
                  foundvalue1:=pointer(tempbytearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue1switch,number);
                  found:=0;
                end;
              end;
              inc(bytep);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue1,found);
        progressbar.position:=total;
      end;


      if scanway=Changed_value then
      begin
        //It's an changed value scan

        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory1switch,number);


        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempdwordarray:=pointer(previousmemory1switch);
          previousmemory1switch:=pointer(previousmemory1);
          previousmemory1:=pointer(tempdwordarray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory1switch,number);

          bytep:=@previousmemory1[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            bytesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(bytesscannedstart+$1000)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(bytesscannedstart),@bytesscanned[0],$1000,actualwrite) then
                j:=$1000
              else
              begin
                while searchaddress[i]<(bytesscannedstart+$1000) do inc(i);
                continue;
              end;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),@bytesscanned[searchaddress[i]-bytesscannedstart],1,actualwrite) then
                j:=1 //only 1
              else
              begin
                inc(i);
                continue;
              end;
            end;

            while (j>0) and (i<l) and (searchaddress[i]<(bytesscannedstart+$1000)) do
            begin
              if (BytesScanned[searchaddress[i]-bytesscannedstart]<>bytep^) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue1[found]:=BytesScanned[searchaddress[i]-bytesscannedstart];
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempbytearray:=pointer(foundvalue1switch);
                  foundvalue1switch:=pointer(foundvalue1);
                  foundvalue1:=pointer(tempbytearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue1switch,number);
                  found:=0;
                end;
              end;
              inc(bytep);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue1,found);
        progressbar.position:=total;
      end;

      if scanway=UnChanged_value then
      begin
        //It's an unchanged value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory1switch,number);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempdwordarray:=pointer(previousmemory1switch);
          previousmemory1switch:=pointer(previousmemory1);
          previousmemory1:=pointer(tempdwordarray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory1switch,number);

          bytep:=@previousmemory1[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            bytesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(bytesscannedstart+$1000)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(bytesscannedstart),@bytesscanned[0],$1000,actualwrite) then
                j:=$1000
              else
              begin
                while searchaddress[i]<(bytesscannedstart+$1000) do inc(i);
                continue;
              end;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),@bytesscanned[searchaddress[i]-bytesscannedstart],1,actualwrite) then
                j:=1 //only 1
              else
              begin
                inc(i);
                continue;
              end;
            end;

            while (j>0) and (i<l) and (searchaddress[i]<(bytesscannedstart+$1000)) do
            begin
              if (BytesScanned[searchaddress[i]-bytesscannedstart]=bytep^) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue1[found]:=BytesScanned[searchaddress[i]-bytesscannedstart];
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempbytearray:=pointer(foundvalue1switch);
                  foundvalue1switch:=pointer(foundvalue1);
                  foundvalue1:=pointer(tempbytearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue1switch,number);
                  found:=0;
                end;
              end;
              inc(bytep);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue1,found);
        progressbar.position:=total;
      end;

      {$ifndef netserver}
      if scanway=SameAsFirst then
      begin
        //It's an same as first value scan
        FSHandler:=TFirstscanhandler.create;

        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory1switch,number);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempdwordarray:=pointer(previousmemory1switch);
          previousmemory1switch:=pointer(previousmemory1);
          previousmemory1:=pointer(tempdwordarray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory1switch,number);

          bytep:=@previousmemory1[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            bytesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(bytesscannedstart+$1000)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(bytesscannedstart),@bytesscanned[0],$1000,actualwrite) then
                j:=$1000
              else
              begin
                while searchaddress[i]<(bytesscannedstart+$1000) do inc(i);
                continue;
              end;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),@bytesscanned[searchaddress[i]-bytesscannedstart],1,actualwrite) then
                j:=1 //only 1
              else
              begin
                inc(i);
                continue;
              end;
            end;

            while (j>0) and (i<l) and (searchaddress[i]<(bytesscannedstart+$1000)) do
            begin
              helpbyte:=fshandler.getfirstscanbyte(searchaddress[i]);

              if (BytesScanned[searchaddress[i]-bytesscannedstart]=helpbyte) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue1[found]:=BytesScanned[searchaddress[i]-bytesscannedstart];
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempbytearray:=pointer(foundvalue1switch);
                  foundvalue1switch:=pointer(foundvalue1);
                  foundvalue1:=pointer(tempbytearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue1switch,number);
                  found:=0;
                end;
              end;
              inc(bytep);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue1,found);
        progressbar.position:=total;

        fshandler.Free;
      end;
      {$endif}


    end;

    if valtype=1 then
    begin
      //It's a Word Scan
      total:=filesize(memoryfile) div 2;

      progressbar.Max:=total;
      progressbar.Position:=0;

      setlength(searchaddress,number);
      setlength(searchaddressswitch,number);
      setlength(foundvalue2,number);
      setlength(foundvalue2switch,number);
      setlength(previousmemory2,number);
      setlength(previousmemory2switch,number);
      setlength(wordsscanned,4096);

      foundvalue2[0]:=wordvalue+1;
      foundvalue2switch[0]:=wordvalue+1;
      found2:=0;

      if scanway=Exact_Value then
      begin
        //It's an Exact value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          //lets start reading the next block while i'm scanning this block
          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            wordsscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(wordsscannedstart+$1000-1)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(wordsscannedstart),@wordsscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(wordsscannedstart+$1000-1) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart),2,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(wordsscannedstart+$1000-1)) or (j2)) do
            begin
              if pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart)^=WordValue then
              begin
                foundaddress[found]:=SearchAddress[i];
                inc(found);
                inc(found2);

                if found=number then
                begin
                  if foundvalue2[0]<>wordvalue then
                  begin
                    for k:=0 to number-1 do foundvalue2[k]:=wordvalue;
                    FoundIsFilled:=true;
                  end;

                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempwordarray:=pointer(foundvalue2switch);
                  foundvalue2switch:=pointer(foundvalue2);
                  foundvalue2:=pointer(tempwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

                  found:=0;
                end;
              end;
              inc(i);
              dec(j);
            end;

          end;
          progressbar.StepBy(actualread div 4);
        end;

        if foundvalue2[0]<>wordvalue then
        begin
          for k:=0 to number-1 do foundvalue2[k]:=wordvalue;
          FoundIsFilled:=true;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue2,2*found);

        progressbar.position:=total;
      end;

      if scanway=BiggerThan then
      begin
        //It's an bigger than ... scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          //lets start reading the next block while i'm scanning this block
          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            wordsscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(wordsscannedstart+$1000-1)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(wordsscannedstart),@wordsscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(wordsscannedstart+$1000-1) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart),2,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(wordsscannedstart+$1000-1)) or (j2)) do
            begin
              if pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart)^>WordValue then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue2[found]:=pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart)^;
                inc(found);
                inc(found2);

                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempwordarray:=pointer(foundvalue2switch);
                  foundvalue2switch:=pointer(foundvalue2);
                  foundvalue2:=pointer(tempwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

                  found:=0;
                end;
              end;
              inc(i);
              dec(j);
            end;

          end;
          progressbar.StepBy(actualread div 4);
        end;

        if foundvalue2[0]<>wordvalue then
        begin
          for k:=0 to number-1 do foundvalue2[k]:=wordvalue;
          FoundIsFilled:=true;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue2,2*found);

        progressbar.position:=total;
      end;

      if scanway=SmallerThan then
      begin
        //It's a smaller than scan

        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          //lets start reading the next block while i'm scanning this block
          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            wordsscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(wordsscannedstart+$1000-1)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(wordsscannedstart),@wordsscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(wordsscannedstart+$1000-1) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart),2,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(wordsscannedstart+$1000-1)) or (j2)) do
            begin
              if pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart)^<WordValue then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue2[found]:=pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart)^;
                inc(found);
                inc(found2);

                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempwordarray:=pointer(foundvalue2switch);
                  foundvalue2switch:=pointer(foundvalue2);
                  foundvalue2:=pointer(tempwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

                  found:=0;
                end;
              end;
              inc(i);
              dec(j);
            end;

          end;
          progressbar.StepBy(actualread div 4);
        end;

        if foundvalue2[0]<>wordvalue then
        begin
          for k:=0 to number-1 do foundvalue2[k]:=wordvalue;
          FoundIsFilled:=true;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue2,2*found);

        progressbar.position:=total;
      end;

      if scanway=ValueBetween then
      begin
        //It's an value between scan

        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          //lets start reading the next block while i'm scanning this block
          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            wordsscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(wordsscannedstart+$1000-1)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(wordsscannedstart),@wordsscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(wordsscannedstart+$1000-1) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart),2,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(wordsscannedstart+$1000-1)) or (j2)) do
            begin
              WordScanned:=pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart)^;
              if (wordscanned>=WordValue) and (wordscanned<=WordValue) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue2[found]:=pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart)^;
                inc(found);
                inc(found2);

                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempwordarray:=pointer(foundvalue2switch);
                  foundvalue2switch:=pointer(foundvalue2);
                  foundvalue2:=pointer(tempwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

                  found:=0;
                end;
              end;
              inc(i);
              dec(j);
            end;

          end;
          progressbar.StepBy(actualread div 4);
        end;

        if foundvalue2[0]<>wordvalue then
        begin
          for k:=0 to number-1 do foundvalue2[k]:=wordvalue;
          FoundIsFilled:=true;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue2,2*found);

        progressbar.position:=total;
      end;

      //
      if scanway=Increased_Value then
      begin
        //It's an Increased value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory2switch,number*2);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempwordarray:=pointer(previousmemory2switch);
          previousmemory2switch:=pointer(previousmemory2);
          previousmemory2:=pointer(tempwordarray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory2switch,number*2);

          wordp:=@previousmemory2[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            wordsscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(wordsscannedstart+$1000-1)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(wordsscannedstart),@wordsscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(wordsscannedstart+$1000-1) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart),2,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(wordsscannedstart+$1000-1)) or (j2)) do
            begin
              if pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart)^>wordp^ then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue2[found]:=pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart)^;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempwordarray:=pointer(foundvalue2switch);
                  foundvalue2switch:=pointer(foundvalue2);
                  foundvalue2:=pointer(tempwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

                  found:=0;
                end;
              end;
              inc(wordp);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue2,2*found);
        progressbar.position:=total;
      end;


      if scanway=Increased_Value_by then
      begin
        //It's an Increased value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory2switch,number*2);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempwordarray:=pointer(previousmemory2switch);
          previousmemory2switch:=pointer(previousmemory2);
          previousmemory2:=pointer(tempwordarray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory2switch,number*2);

          wordp:=@previousmemory2[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            wordsscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(wordsscannedstart+$1000-1)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(wordsscannedstart),@wordsscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(wordsscannedstart+$1000-1) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart),2,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(wordsscannedstart+$1000-1)) or (j2)) do
            begin
              wordscanned:=pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart)^;
              
              if
               (
                 ((not percentage) and (wordscanned=wordp^+wordvalue))
                 or
                 (percentage and (wordscanned>=wordp^+trunc(wordp^*(wordvalue/100))))
               ) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue2[found]:=pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart)^;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempwordarray:=pointer(foundvalue2switch);
                  foundvalue2switch:=pointer(foundvalue2);
                  foundvalue2:=pointer(tempwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

                  found:=0;
                end;
              end;
              inc(wordp);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue2,2*found);
        progressbar.position:=total;
      end;


      if scanway=Decreased_Value then
      begin
        //It's a Decreased value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory2switch,number*2);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempdwordarray:=pointer(previousmemory2switch);
          previousmemory2switch:=pointer(previousmemory2);
          previousmemory2:=pointer(tempdwordarray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory2switch,number*2);

          wordp:=@previousmemory2[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            wordsscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(wordsscannedstart+$1000-1)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(wordsscannedstart),@wordsscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(wordsscannedstart+$1000-1) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart),2,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(wordsscannedstart+$1000-1)) or (j2)) do
            begin
              if pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart)^<wordp^ then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue2[found]:=pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart)^;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempwordarray:=pointer(foundvalue2switch);
                  foundvalue2switch:=pointer(foundvalue2);
                  foundvalue2:=pointer(tempwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

                  found:=0;
                end;
              end;
              inc(wordp);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue2,2*found);
        progressbar.position:=total;
      end;


      if scanway=Decreased_Value_by then
      begin
        //It's an Increased value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory2switch,number*2);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempdwordarray:=pointer(previousmemory2switch);
          previousmemory2switch:=pointer(previousmemory2);
          previousmemory2:=pointer(tempdwordarray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory2switch,number*2);

          wordp:=@previousmemory2[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            wordsscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(wordsscannedstart+$1000-1)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(wordsscannedstart),@wordsscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(wordsscannedstart+$1000-1) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart),2,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(wordsscannedstart+$1000-1)) or (j2)) do
            begin
              wordscanned:=pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart)^;
              
              if
               (
                 ((not percentage) and (wordscanned=wordp^-wordvalue))
                 or
                 (percentage and (wordscanned<=wordp^-trunc(wordp^*(wordvalue/100))))
               ) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue2[found]:=pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart)^;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempwordarray:=pointer(foundvalue2switch);
                  foundvalue2switch:=pointer(foundvalue2);
                  foundvalue2:=pointer(tempwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

                  found:=0;
                end;
              end;
              inc(wordp);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue2,2*found);
        progressbar.position:=total;
      end;


      if scanway=Changed_Value then
      begin
        //It's a Changed value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory2switch,number*2);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempdwordarray:=pointer(previousmemory2switch);
          previousmemory2switch:=pointer(previousmemory2);
          previousmemory2:=pointer(tempdwordarray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory2switch,number*2);

          wordp:=@previousmemory2[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            wordsscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(wordsscannedstart+$1000-1)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(wordsscannedstart),@wordsscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(wordsscannedstart+$1000-1) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart),2,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(wordsscannedstart+$1000-1)) or (j2)) do
            begin
              if pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart)^<>wordp^ then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue2[found]:=pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart)^;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempwordarray:=pointer(foundvalue2switch);
                  foundvalue2switch:=pointer(foundvalue2);
                  foundvalue2:=pointer(tempwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

                  found:=0;
                end;
              end;
              inc(wordp);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue2,2*found);
        progressbar.position:=total;
      end;

      if scanway=UnChanged_value then
      begin
        //It's an unchanged value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory2switch,number*2);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempdwordarray:=pointer(previousmemory2switch);
          previousmemory2switch:=pointer(previousmemory2);
          previousmemory2:=pointer(tempdwordarray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory2switch,number*2);

          wordp:=@previousmemory2[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            wordsscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(wordsscannedstart+$1000-1)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(wordsscannedstart),@wordsscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(wordsscannedstart+$1000-1) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart),2,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(wordsscannedstart+$1000-1)) or (j2)) do
            begin
              if pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart)^=wordp^ then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue2[found]:=pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart)^;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempwordarray:=pointer(foundvalue2switch);
                  foundvalue2switch:=pointer(foundvalue2);
                  foundvalue2:=pointer(tempwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

                  found:=0;
                end;
              end;
              inc(wordp);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue2,2*found);
        progressbar.position:=total;
      end;

      {$ifndef netserver}
      if scanway=SameAsFirst then
      begin
        //It's an same as first value scan
        FSHandler:=TFirstscanhandler.create;

        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory2switch,number*2);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempdwordarray:=pointer(previousmemory2switch);
          previousmemory2switch:=pointer(previousmemory2);
          previousmemory2:=pointer(tempdwordarray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory2switch,number*2);

          wordp:=@previousmemory2[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            wordsscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(wordsscannedstart+$1000-1)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(wordsscannedstart),@wordsscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(wordsscannedstart+$1000-1) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart),2,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(wordsscannedstart+$1000-1)) or (j2)) do
            begin
              helpword:=FSHandler.getfirstscanword(SearchAddress[i]);
              if pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart)^=helpword then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue2[found]:=pword(dword(@wordsscanned[0])+searchaddress[i]-wordsscannedstart)^;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempwordarray:=pointer(foundvalue2switch);
                  foundvalue2switch:=pointer(foundvalue2);
                  foundvalue2:=pointer(tempwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue2switch,2*number);

                  found:=0;
                end;
              end;
              inc(wordp);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue2,2*found);
        progressbar.position:=total;
        FSHandler.free;
      end;
      {$endif}
    end;

    if valtype=2 then
    begin
      //It's a DWord Scan
      total:=filesize(memoryfile) div 4;

      progressbar.Max:=total;
      progressbar.Position:=0;

      setlength(searchaddress,number);
      setlength(searchaddressswitch,number);
      setlength(foundvalue3,number);
      setlength(foundvalue3switch,number);
      setlength(previousmemory3,number);
      setlength(previousmemory3switch,number);
      setlength(dwordsscanned,4096); //or 1024

      foundvalue3[0]:=dwordvalue+1;
      foundvalue3switch[0]:=dwordvalue+1;

      if scanway=Exact_Value then
      begin
        //It's an Exact value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          //lets start reading the next block while i'm scanning this block
          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            dwordsscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(dwordsscannedstart+$1000-3)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(dwordsscannedstart),@dwordsscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(dwordsscannedstart+$1000-3) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart),4,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(dwordsscannedstart+$1000-3)) or (j2)) do
            begin
              if pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart)^=dWordValue then
              begin
                foundaddress[found]:=SearchAddress[i];
                inc(found);
                inc(found2);

                if found=number then
                begin
                  if foundvalue3[0]<>dwordvalue then
                  begin
                    for k:=0 to number-1 do foundvalue3[k]:=dwordvalue;
                    FoundIsFilled:=true;
                  end;

                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdwordarray:=pointer(foundvalue3switch);
                  foundvalue3switch:=pointer(foundvalue3);
                  foundvalue3:=pointer(tempdwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

                  found:=0;
                end;
              end;
              inc(i);
              dec(j);
            end;

          end;
          progressbar.StepBy(actualread div 4);
        end;

        if foundvalue3[0]<>dwordvalue then
        begin
          for k:=0 to number-1 do foundvalue3[k]:=dwordvalue;
          FoundIsFilled:=true;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue3,4*found);

        progressbar.position:=total;
      end;


      if scanway=BiggerThan then
      begin
        //It's an Increased value by scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          //lets start reading the next block while i'm scanning this block
          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            dwordsscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(dwordsscannedstart+$1000-3)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(dwordsscannedstart),@dwordsscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(dwordsscannedstart+$1000-3) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart),4,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(dwordsscannedstart+$1000-3)) or (j2)) do
            begin
              if pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart)^>dWordValue then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue3[found]:=pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart)^;
                inc(found);
                inc(found2);

                if found=number then
                begin
                  if foundvalue3[0]<>dwordvalue then
                  begin
                    for k:=0 to number-1 do foundvalue3[k]:=dwordvalue;
                    FoundIsFilled:=true;
                  end;

                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdwordarray:=pointer(foundvalue3switch);
                  foundvalue3switch:=pointer(foundvalue3);
                  foundvalue3:=pointer(tempdwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

                  found:=0;
                end;
              end;
              inc(i);
              dec(j);
            end;

          end;
          progressbar.StepBy(actualread div 4);
        end;

        if foundvalue3[0]<>dwordvalue then
        begin
          for k:=0 to number-1 do foundvalue3[k]:=dwordvalue;
          FoundIsFilled:=true;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue3,4*found);

        progressbar.position:=total;
      end;

      if scanway=SmallerThan then
      begin
        //It's an Increased value by scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          //lets start reading the next block while i'm scanning this block
          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            dwordsscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(dwordsscannedstart+$1000-3)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(dwordsscannedstart),@dwordsscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(dwordsscannedstart+$1000-3) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart),4,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(dwordsscannedstart+$1000-3)) or (j2)) do
            begin
              if pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart)^<dWordValue then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue3[found]:=pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart)^;
                inc(found);
                inc(found2);

                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdwordarray:=pointer(foundvalue3switch);
                  foundvalue3switch:=pointer(foundvalue3);
                  foundvalue3:=pointer(tempdwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

                  found:=0;
                end;
              end;
              inc(i);
              dec(j);
            end;

          end;
          progressbar.StepBy(actualread div 4);
        end;

        if foundvalue3[0]<>dwordvalue then
        begin
          for k:=0 to number-1 do foundvalue3[k]:=dwordvalue;
          FoundIsFilled:=true;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue3,4*found);

        progressbar.position:=total;
      end;

      if scanway=ValueBetween then
      begin
        //It's an Increased value by scan

        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          //lets start reading the next block while i'm scanning this block
          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            dwordsscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(dwordsscannedstart+$1000-3)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(dwordsscannedstart),@dwordsscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(dwordsscannedstart+$1000-3) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart),4,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(dwordsscannedstart+$1000-3)) or (j2)) do
            begin
              dwordscanned:=pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart)^;
              if (dwordscanned>=dWordValue) and (dwordscanned<=dWordValue2) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue3[found]:=dwordscanned;

                inc(found);
                inc(found2);

                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdwordarray:=pointer(foundvalue3switch);
                  foundvalue3switch:=pointer(foundvalue3);
                  foundvalue3:=pointer(tempdwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

                  found:=0;
                end;
              end;
              inc(i);
              dec(j);
            end;

          end;
          progressbar.StepBy(actualread div 4);
        end;

        if foundvalue3[0]<>dwordvalue then
        begin
          for k:=0 to number-1 do foundvalue3[k]:=dwordvalue;
          FoundIsFilled:=true;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue3,4*found);

        progressbar.position:=total;
      end;


      if scanway=Increased_Value then
      begin
        //It's an Increased value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory3switch,number*4);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempdwordarray:=pointer(previousmemory3switch);
          previousmemory3switch:=pointer(previousmemory3);
          previousmemory3:=pointer(tempdwordarray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory3switch,number*4);

          dwordp:=@previousmemory3[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            dwordsscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(dwordsscannedstart+$1000-3)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(dwordsscannedstart),@dwordsscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(dwordsscannedstart+$1000-3) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart),4,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(dwordsscannedstart+$1000-3)) or (j2)) do
            begin
              if pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart)^>dwordp^ then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue3[found]:=pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart)^;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdwordarray:=pointer(foundvalue3switch);
                  foundvalue3switch:=pointer(foundvalue3);
                  foundvalue3:=pointer(tempdwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

                  found:=0;
                end;
              end;
              inc(dwordp);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue3,4*found);
        progressbar.position:=total;
      end;


      if scanway=Increased_Value_By then
      begin
        //It's an Increased value by scan

        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory3switch,number*4);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempdwordarray:=pointer(previousmemory3switch);
          previousmemory3switch:=pointer(previousmemory3);
          previousmemory3:=pointer(tempdwordarray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory3switch,number*4);

          dwordp:=@previousmemory3[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            dwordsscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(dwordsscannedstart+$1000-3)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(dwordsscannedstart),@dwordsscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(dwordsscannedstart+$1000-3) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart),4,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(dwordsscannedstart+$1000-3)) or (j2)) do
            begin
              dwordscanned:=pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart)^;
              if
               (
                 ((not percentage) and (dwordscanned=dwordp^+dwordvalue))
                 or
                 (percentage and (dwordscanned>=dwordp^+trunc(dwordp^*(dwordvalue/100))))
               ) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue3[found]:=pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart)^;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdwordarray:=pointer(foundvalue3switch);
                  foundvalue3switch:=pointer(foundvalue3);
                  foundvalue3:=pointer(tempdwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

                  found:=0;
                end;
              end;
              inc(dwordp);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue3,4*found);
        progressbar.position:=total;
      end;

      if scanway=Decreased_Value then
      begin
        //It's an Decreased value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory3switch,number*4);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempdwordarray:=pointer(previousmemory3switch);
          previousmemory3switch:=pointer(previousmemory3);
          previousmemory3:=pointer(tempdwordarray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory3switch,number*4);

          dwordp:=@previousmemory3[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            dwordsscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(dwordsscannedstart+$1000-3)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(dwordsscannedstart),@dwordsscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(dwordsscannedstart+$1000-3) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart),4,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(dwordsscannedstart+$1000-3)) or (j2)) do
            begin
              if pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart)^<dwordp^ then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue3[found]:=pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart)^;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdwordarray:=pointer(foundvalue3switch);
                  foundvalue3switch:=pointer(foundvalue3);
                  foundvalue3:=pointer(tempdwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

                  found:=0;
                end;
              end;
              inc(dwordp);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue3,4*found);
        progressbar.position:=total;
      end;

      if scanway=Decreased_Value_By then
      begin
        //It's an Increased value by scan

        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory3switch,number*4);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempdwordarray:=pointer(previousmemory3switch);
          previousmemory3switch:=pointer(previousmemory3);
          previousmemory3:=pointer(tempdwordarray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory3switch,number*4);

          dwordp:=@previousmemory3[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            dwordsscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(dwordsscannedstart+$1000-3)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(dwordsscannedstart),@dwordsscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(dwordsscannedstart+$1000-3) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart),4,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(dwordsscannedstart+$1000-3)) or (j2)) do
            begin
              dwordscanned:=pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart)^;
              if
               (
                 ((not percentage) and (dwordscanned=dwordp^-dwordvalue))
                 or
                 (percentage and (dwordscanned<=dwordp^-trunc(dwordp^*(dwordvalue/100))))
               ) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue3[found]:=pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart)^;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdwordarray:=pointer(foundvalue3switch);
                  foundvalue3switch:=pointer(foundvalue3);
                  foundvalue3:=pointer(tempdwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

                  found:=0;
                end;
              end;
              inc(dwordp);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue3,4*found);
        progressbar.position:=total;
      end;


      if scanway=Changed_value then
      begin
        //It's an changed value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory3switch,number*4);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempdwordarray:=pointer(previousmemory3switch);
          previousmemory3switch:=pointer(previousmemory3);
          previousmemory3:=pointer(tempdwordarray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory3switch,number*4);

          dwordp:=@previousmemory3[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            dwordsscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(dwordsscannedstart+$1000-3)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(dwordsscannedstart),@dwordsscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(dwordsscannedstart+$1000-3) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart),4,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(dwordsscannedstart+$1000-3)) or (j2)) do
            begin
              if pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart)^<>dwordp^ then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue3[found]:=pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart)^;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdwordarray:=pointer(foundvalue3switch);
                  foundvalue3switch:=pointer(foundvalue3);
                  foundvalue3:=pointer(tempdwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

                  found:=0;
                end;
              end;
              inc(dwordp);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue3,4*found);
        progressbar.position:=total;
      end;

      if scanway=UnChanged_value then
      begin
       //It's an unchanged value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory3switch,number*4);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempdwordarray:=pointer(previousmemory3switch);
          previousmemory3switch:=pointer(previousmemory3);
          previousmemory3:=pointer(tempdwordarray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory3switch,number*4);

          dwordp:=@previousmemory3[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            dwordsscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(dwordsscannedstart+$1000-3)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(dwordsscannedstart),@dwordsscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(dwordsscannedstart+$1000-3) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart),4,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(dwordsscannedstart+$1000-3)) or (j2)) do
            begin
              if pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart)^=dwordp^ then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue3[found]:=pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart)^;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdwordarray:=pointer(foundvalue3switch);
                  foundvalue3switch:=pointer(foundvalue3);
                  foundvalue3:=pointer(tempdwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

                  found:=0;
                end;
              end;
              inc(dwordp);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue3,4*found);
        progressbar.position:=total;
      end;

      {$ifndef netserver}
      if scanway=SameAsFirst then
      begin
        //It's an same as first value scan
        FSHandler:=TFirstscanhandler.create;
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory3switch,number*4);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempdwordarray:=pointer(previousmemory3switch);
          previousmemory3switch:=pointer(previousmemory3);
          previousmemory3:=pointer(tempdwordarray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory3switch,number*4);

          dwordp:=@previousmemory3[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            dwordsscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(dwordsscannedstart+$1000-3)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(dwordsscannedstart),@dwordsscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(dwordsscannedstart+$1000-3) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart),4,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(dwordsscannedstart+$1000-3)) or (j2)) do
            begin
              helpdword:=fshandler.getfirstscandword(searchaddress[i]);
              if pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart)^=helpdword then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue3[found]:=pdword(dword(@dwordsscanned[0])+searchaddress[i]-dwordsscannedstart)^;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdwordarray:=pointer(foundvalue3switch);
                  foundvalue3switch:=pointer(foundvalue3);
                  foundvalue3:=pointer(tempdwordarray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue3switch,4*number);

                  found:=0;
                end;
              end;
              inc(dwordp);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue3,4*found);
        progressbar.position:=total;
        fshandler.Free;
      end;
      {$endif}


    end;

    if valtype=3 then
    begin
      //It's a single Scan
      total:=filesize(memoryfile) div 4;
      progressbar.Max:=total;

      setlength(searchaddress,number);
      setlength(searchaddressswitch,number);
      setlength(foundvalue4,number);
      setlength(foundvalue4switch,number);
      setlength(previousmemory4,number);
      setlength(previousmemory4switch,number);
      setlength(singlesscanned,4096);

      if scanway=Exact_Value then
      begin

        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

        if decim=0 then helpsingle3:=1 else
          helpsingle3:=1/((decim)*10);  //the range for extremerounded scans

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          //lets start reading the next block while i'm scanning this block
          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            singlesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(singlesscannedstart+$1000-3)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(singlesscannedstart),@singlesscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(singlesscannedstart+$1000-3) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),psingle(dword(@singlesscanned[0])+searchaddress[i]-singlesscannedstart),4,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(singlesscannedstart+$1000-3)) or (j2)) do
            begin
              singlescanned:=psingle(dword(@singlesscanned[0])+searchaddress[i]-singlesscannedstart)^;

              check:=true;
              if (not (isnan(singlescanned) or isinfinite(singlescanned))) then
              begin
                case roundingtype of
                  rounded:
                  begin
                    helpsingle:=RoundTo(singlescanned,-decim);
                    check:=(helpsingle=SingleValue);
                  end;

                  extremerounded:
                  begin
                    //if a scan for 1 it scans for    0<x<2
                    //if a scan for 1.0 it scans for  9.9<x<1.10
                    check:=((singlescanned<(singlevalue+helpsingle3)) and (singlescanned>(singlevalue-helpsingle3)) );
                  end;

                  truncated:
                  begin
                    //if a scan for 1 it scans for    1>=x<2
                    //if a scan for 1.0 it scans for 1.0>=x<1.10
                    check:=((singlescanned<(singlevalue+helpsingle3)) and (singlescanned>=singlevalue));
                  end;

                  else check:=false;
                end;
              end;


              if check then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue4[found]:=singlescanned;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempsinglearray:=pointer(foundvalue4switch);
                  foundvalue4switch:=pointer(foundvalue4);
                  foundvalue4:=pointer(tempsinglearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);
                  found:=0;
                end;
              end;
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue4,4*found);
        progressbar.Position:=total;
      end;


      if scanway=BiggerThan then
      begin
        //It's an bigger than value
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

        if decim=0 then helpsingle3:=1 else
          helpsingle3:=1/((decim)*10);  //the range for extremerounded scans

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          //lets start reading the next block while i'm scanning this block
          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            singlesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(singlesscannedstart+$1000-3)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(singlesscannedstart),@singlesscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(singlesscannedstart+$1000-3) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),psingle(dword(@singlesscanned[0])+searchaddress[i]-singlesscannedstart),4,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(singlesscannedstart+$1000-3)) or (j2)) do
            begin
              singlescanned:=psingle(dword(@singlesscanned[0])+searchaddress[i]-singlesscannedstart)^;

              if (singlescanned>singlevalue) and (not (isnan(singlescanned) or isinfinite(singlescanned))) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue4[found]:=singlescanned;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempsinglearray:=pointer(foundvalue4switch);
                  foundvalue4switch:=pointer(foundvalue4);
                  foundvalue4:=pointer(tempsinglearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);
                  found:=0;
                end;
              end;
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue4,4*found);
        progressbar.Position:=total;
      end;

      if scanway=SmallerThan then
      begin
        //It's a smaller then scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

        if decim=0 then helpsingle3:=1 else
          helpsingle3:=1/((decim)*10);  //the range for extremerounded scans

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          //lets start reading the next block while i'm scanning this block
          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            singlesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(singlesscannedstart+$1000-3)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(singlesscannedstart),@singlesscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(singlesscannedstart+$1000-3) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),psingle(dword(@singlesscanned[0])+searchaddress[i]-singlesscannedstart),4,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(singlesscannedstart+$1000-3)) or (j2)) do
            begin
              singlescanned:=psingle(dword(@singlesscanned[0])+searchaddress[i]-singlesscannedstart)^;

              if (singlescanned<singlevalue) and (not (isnan(singlescanned) or isinfinite(singlescanned))) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue4[found]:=singlescanned;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempsinglearray:=pointer(foundvalue4switch);
                  foundvalue4switch:=pointer(foundvalue4);
                  foundvalue4:=pointer(tempsinglearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);
                  found:=0;
                end;
              end;
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue4,4*found);
        progressbar.Position:=total;
      end;


      if scanway=ValueBetween then
      begin
        //It's an value between scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

        if decim=0 then helpsingle3:=1 else
          helpsingle3:=1/((decim)*10);  //the range for extremerounded scans

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          //lets start reading the next block while i'm scanning this block
          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            singlesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(singlesscannedstart+$1000-3)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(singlesscannedstart),@singlesscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(singlesscannedstart+$1000-3) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),psingle(dword(@singlesscanned[0])+searchaddress[i]-singlesscannedstart),4,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(singlesscannedstart+$1000-3)) or (j2)) do
            begin
              singlescanned:=psingle(dword(@singlesscanned[0])+searchaddress[i]-singlesscannedstart)^;

              if (singlescanned>=singlevalue) and (singlescanned<=singlevalue2) and (not (isnan(singlescanned) or isinfinite(singlescanned))) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue4[found]:=singlescanned;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempsinglearray:=pointer(foundvalue4switch);
                  foundvalue4switch:=pointer(foundvalue4);
                  foundvalue4:=pointer(tempsinglearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);
                  found:=0;
                end;
              end;
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue4,4*found);
        progressbar.Position:=total;
      end;


      if scanway=Increased_Value then
      begin
        //It's an Increased value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory4switch,number*4);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempsinglearray:=pointer(previousmemory4switch);
          previousmemory4switch:=pointer(previousmemory4);
          previousmemory4:=pointer(tempsinglearray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory4switch,number*4);

          singlep:=@previousmemory4[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            singlesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(singlesscannedstart+$1000-3)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(singlesscannedstart),@singlesscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(singlesscannedstart+$1000-3) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),psingle(dword(@singlesscanned[0])+searchaddress[i]-singlesscannedstart),4,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(singlesscannedstart+$1000-3)) or (j2)) do
            begin
              singlescanned:=psingle(dword(@singlesscanned[0])+searchaddress[i]-singlesscannedstart)^;
              if (singlescanned>singlep^) and (not (isnan(singlescanned) or isinfinite(singlescanned))) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue4[found]:=singlescanned;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempsinglearray:=pointer(foundvalue4switch);
                  foundvalue4switch:=pointer(foundvalue4);
                  foundvalue4:=pointer(tempsinglearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

                  found:=0;
                end;
              end;
              inc(singlep);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue4,4*found);
        progressbar.position:=total;
      end;


      if scanway=Increased_Value_by then
      begin
        //It's an Increased value by scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory4switch,number*4);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempsinglearray:=pointer(previousmemory4switch);
          previousmemory4switch:=pointer(previousmemory4);
          previousmemory4:=pointer(tempsinglearray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory4switch,number*4);

          singlep:=@previousmemory4[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            singlesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(singlesscannedstart+$1000-3)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(singlesscannedstart),@singlesscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(singlesscannedstart+$1000-3) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),psingle(dword(@singlesscanned[0])+searchaddress[i]-singlesscannedstart),4,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(singlesscannedstart+$1000-3)) or (j2)) do
            begin
              singlescanned:=psingle(dword(@singlesscanned[0])+searchaddress[i]-singlesscannedstart)^;
              helpsingle:=RoundTo(singlescanned,-decim);
              helpsingle2:=RoundTo(singlep^+singlevalue,-decim);
              if (actualwrite=4) and
              (
              (
              (not percentage)
              and
              (helpsingle=helpsingle2)
              )
              or
              (
              percentage
              and
              (singlescanned>=singlep^+(singlep^*(singlevalue/100)))
              )
              ) and (not (isnan(singlescanned) or isinfinite(singlescanned))) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue4[found]:=singlescanned;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempsinglearray:=pointer(foundvalue4switch);
                  foundvalue4switch:=pointer(foundvalue4);
                  foundvalue4:=pointer(tempsinglearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

                  found:=0;
                end;
              end;
              inc(singlep);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue4,4*found);
        progressbar.position:=total;
      end;

      if scanway=Decreased_Value then
      begin
        //It's an Increased value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory4switch,number*4);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempsinglearray:=pointer(previousmemory4switch);
          previousmemory4switch:=pointer(previousmemory4);
          previousmemory4:=pointer(tempsinglearray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory4switch,number*4);

          singlep:=@previousmemory4[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            singlesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(singlesscannedstart+$1000-3)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(singlesscannedstart),@singlesscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(singlesscannedstart+$1000-3) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),psingle(dword(@singlesscanned[0])+searchaddress[i]-singlesscannedstart),4,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(singlesscannedstart+$1000-3)) or (j2)) do
            begin
              singlescanned:=psingle(dword(@singlesscanned[0])+searchaddress[i]-singlesscannedstart)^;
              if (singlescanned<singlep^) and (not (isnan(singlescanned) or isinfinite(singlescanned))) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue4[found]:=singlescanned;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempsinglearray:=pointer(foundvalue4switch);
                  foundvalue4switch:=pointer(foundvalue4);
                  foundvalue4:=pointer(tempsinglearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

                  found:=0;
                end;
              end;
              inc(singlep);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue4,4*found);
        progressbar.position:=total;
      end;


      if scanway=Decreased_Value_by then
      begin
        //It's an Increased value by scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory4switch,number*4);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempsinglearray:=pointer(previousmemory4switch);
          previousmemory4switch:=pointer(previousmemory4);
          previousmemory4:=pointer(tempsinglearray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory4switch,number*4);

          singlep:=@previousmemory4[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            singlesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(singlesscannedstart+$1000-3)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(singlesscannedstart),@singlesscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(singlesscannedstart+$1000-3) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),psingle(dword(@singlesscanned[0])+searchaddress[i]-singlesscannedstart),4,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(singlesscannedstart+$1000-3)) or (j2)) do
            begin
              singlescanned:=psingle(dword(@singlesscanned[0])+searchaddress[i]-singlesscannedstart)^;
              helpsingle:=RoundTo(singlescanned,-decim);
              helpsingle2:=RoundTo(singlep^-singlevalue,-decim);
              if (actualwrite=4) and
              (
              (
              (not percentage)
              and
              (helpsingle=helpsingle2)
              )
              or
              (
              percentage
              and
              (singlescanned<=singlep^-(singlep^*(singlevalue/100)))
              )
              ) and (not (isnan(singlescanned) or isinfinite(singlescanned))) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue4[found]:=singlescanned;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempsinglearray:=pointer(foundvalue4switch);
                  foundvalue4switch:=pointer(foundvalue4);
                  foundvalue4:=pointer(tempsinglearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

                  found:=0;
                end;
              end;
              inc(singlep);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue4,4*found);
        progressbar.position:=total;
      end;


      if scanway=Changed_value then
      begin
        //It's an changed value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory4switch,number*4);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempsinglearray:=pointer(previousmemory4switch);
          previousmemory4switch:=pointer(previousmemory4);
          previousmemory4:=pointer(tempsinglearray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory4switch,number*4);

          singlep:=@previousmemory4[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            singlesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(singlesscannedstart+$1000-3)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(singlesscannedstart),@singlesscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(singlesscannedstart+$1000-3) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),psingle(dword(@singlesscanned[0])+searchaddress[i]-singlesscannedstart),4,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(singlesscannedstart+$1000-3)) or (j2)) do
            begin
              singlescanned:=psingle(dword(@singlesscanned[0])+searchaddress[i]-singlesscannedstart)^;
              if (singlescanned<>singlep^) and (not (isnan(singlescanned) or isinfinite(singlescanned))) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue4[found]:=singlescanned;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempsinglearray:=pointer(foundvalue4switch);
                  foundvalue4switch:=pointer(foundvalue4);
                  foundvalue4:=pointer(tempsinglearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

                  found:=0;
                end;
              end;
              inc(singlep);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue4,4*found);
        progressbar.position:=total;
      end;

      if scanway=UnChanged_value then
      begin
        //It's an Unchanged value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory4switch,number*4);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempsinglearray:=pointer(previousmemory4switch);
          previousmemory4switch:=pointer(previousmemory4);
          previousmemory4:=pointer(tempsinglearray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory4switch,number*4);

          singlep:=@previousmemory4[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            singlesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(singlesscannedstart+$1000-3)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(singlesscannedstart),@singlesscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(singlesscannedstart+$1000-3) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),psingle(dword(@singlesscanned[0])+searchaddress[i]-singlesscannedstart),4,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(singlesscannedstart+$1000-3)) or (j2)) do
            begin
              singlescanned:=psingle(dword(@singlesscanned[0])+searchaddress[i]-singlesscannedstart)^;
              if (singlescanned=singlep^) and (not (isnan(singlescanned) or isinfinite(singlescanned))) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue4[found]:=singlescanned;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempsinglearray:=pointer(foundvalue4switch);
                  foundvalue4switch:=pointer(foundvalue4);
                  foundvalue4:=pointer(tempsinglearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

                  found:=0;
                end;
              end;
              inc(singlep);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue4,4*found);
        progressbar.position:=total;
      end;

      {$ifndef netserver}
      if scanway=SameAsFirst then
      begin
        //It's an same as first value scan
        FSHandler:=TFirstscanhandler.create;
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory4switch,number*4);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempsinglearray:=pointer(previousmemory4switch);
          previousmemory4switch:=pointer(previousmemory4);
          previousmemory4:=pointer(tempsinglearray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory4switch,number*4);

          singlep:=@previousmemory4[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            singlesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(singlesscannedstart+$1000-3)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(singlesscannedstart),@singlesscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(singlesscannedstart+$1000-3) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),psingle(dword(@singlesscanned[0])+searchaddress[i]-singlesscannedstart),4,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(singlesscannedstart+$1000-3)) or (j2)) do
            begin
              singlescanned:=psingle(dword(@singlesscanned[0])+searchaddress[i]-singlesscannedstart)^;
              helpsingle:=fshandler.getfirstscansingle(searchaddress[i]);
              if (singlescanned=helpsingle) and (not (isnan(singlescanned) or isinfinite(singlescanned))) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue4[found]:=singlescanned;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempsinglearray:=pointer(foundvalue4switch);
                  foundvalue4switch:=pointer(foundvalue4);
                  foundvalue4:=pointer(tempsinglearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue4switch,4*number);

                  found:=0;
                end;
              end;
              inc(singlep);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue4,4*found);
        progressbar.position:=total;
        fshandler.Free;
      end;
      {$endif}


    end;

    //--------------------------------------------------------------------------
    if valtype=4 then
    begin
      //It's a double Scan
      total:=filesize(memoryfile) div 4;
      progressbar.Max:=total;

      setlength(searchaddress,number);
      setlength(searchaddressswitch,number);
      setlength(foundvalue5,number);
      setlength(foundvalue5switch,number);
      setlength(previousmemory5,number);
      setlength(previousmemory5switch,number);
      setlength(doublesscanned,4096);

      if scanway=Exact_Value then
      begin

        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

        if decim=0 then helpdouble3:=1 else
          helpdouble3:=1/((decim)*10);  //the range for extremerounded scans

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          //lets start reading the next block while i'm scanning this block
          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            doublesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(doublesscannedstart+$1000-7)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(doublesscannedstart),@doublesscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(doublesscannedstart+$1000-7) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pdouble(dword(@doublesscanned[0])+searchaddress[i]-doublesscannedstart),8,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(doublesscannedstart+$1000-7)) or (j2)) do
            begin
              doublescanned:=pdouble(dword(@doublesscanned[0])+searchaddress[i]-doublesscannedstart)^;

              check:=true;
              if (not (isnan(doublescanned) or isinfinite(doublescanned))) then
              begin
                case roundingtype of
                  rounded:
                  begin
                    helpdouble:=RoundTo(doublescanned,-decim);
                    check:=(helpdouble=doubleValue);
                  end;

                  extremerounded:
                  begin
                    //if a scan for 1 it scans for    0<x<2
                    //if a scan for 1.0 it scans for  9.9<x<1.10
                    check:=((doublescanned<(doublevalue+helpdouble3)) and (doublescanned>(doublevalue-helpdouble3)) );
                  end;

                  truncated:
                  begin
                    //if a scan for 1 it scans for    1>=x<2
                    //if a scan for 1.0 it scans for 1.0>=x<1.10
                    check:=((doublescanned<(doublevalue+helpdouble3)) and (doublescanned>=doublevalue));
                  end;

                  else check:=false;
                end;
              end;


              if check then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue5[found]:=doublescanned;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdoublearray:=pointer(foundvalue5switch);
                  foundvalue5switch:=pointer(foundvalue5);
                  foundvalue5:=pointer(tempdoublearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);
                  found:=0;
                end;
              end;
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue5,8*found);
        progressbar.Position:=total;
      end;


      if scanway=BiggerThan then
      begin
        //It's an bigger than value
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

        if decim=0 then helpdouble3:=1 else
          helpdouble3:=1/((decim)*10);  //the range for extremerounded scans

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          //lets start reading the next block while i'm scanning this block
          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            doublesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(doublesscannedstart+$1000-7)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(doublesscannedstart),@doublesscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(doublesscannedstart+$1000-7) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pdouble(dword(@doublesscanned[0])+searchaddress[i]-doublesscannedstart),8,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(doublesscannedstart+$1000-7)) or (j2)) do
            begin
              doublescanned:=pdouble(dword(@doublesscanned[0])+searchaddress[i]-doublesscannedstart)^;

              if (doublescanned>doublevalue) and (not (isnan(doublescanned) or isinfinite(doublescanned))) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue5[found]:=doublescanned;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdoublearray:=pointer(foundvalue5switch);
                  foundvalue5switch:=pointer(foundvalue5);
                  foundvalue5:=pointer(tempdoublearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);
                  found:=0;
                end;
              end;
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue5,8*found);
        progressbar.Position:=total;
      end;

      if scanway=SmallerThan then
      begin
        //It's a smaller then scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

        if decim=0 then helpdouble3:=1 else
          helpdouble3:=1/((decim)*10);  //the range for extremerounded scans

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          //lets start reading the next block while i'm scanning this block
          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            doublesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(doublesscannedstart+$1000-7)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(doublesscannedstart),@doublesscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(doublesscannedstart+$1000-7) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pdouble(dword(@doublesscanned[0])+searchaddress[i]-doublesscannedstart),8,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(doublesscannedstart+$1000-7)) or (j2)) do
            begin
              doublescanned:=pdouble(dword(@doublesscanned[0])+searchaddress[i]-doublesscannedstart)^;

              if (doublescanned<doublevalue) and (not (isnan(doublescanned) or isinfinite(doublescanned))) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue5[found]:=doublescanned;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdoublearray:=pointer(foundvalue5switch);
                  foundvalue5switch:=pointer(foundvalue5);
                  foundvalue5:=pointer(tempdoublearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);
                  found:=0;
                end;
              end;
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue5,8*found);
        progressbar.Position:=total;
      end;


      if scanway=ValueBetween then
      begin
        //It's an value between scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

        if decim=0 then helpdouble3:=1 else
          helpdouble3:=1/((decim)*10);  //the range for extremerounded scans

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          //lets start reading the next block while i'm scanning this block
          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            doublesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(doublesscannedstart+$1000-7)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(doublesscannedstart),@doublesscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(doublesscannedstart+$1000-7) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pdouble(dword(@doublesscanned[0])+searchaddress[i]-doublesscannedstart),8,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(doublesscannedstart+$1000-7)) or (j2)) do
            begin
              doublescanned:=pdouble(dword(@doublesscanned[0])+searchaddress[i]-doublesscannedstart)^;

              if (doublescanned>=doublevalue) and (doublescanned<=doublevalue2) and (not (isnan(doublescanned) or isinfinite(doublescanned))) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue5[found]:=doublescanned;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdoublearray:=pointer(foundvalue5switch);
                  foundvalue5switch:=pointer(foundvalue5);
                  foundvalue5:=pointer(tempdoublearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);
                  found:=0;
                end;
              end;
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue5,8*found);
        progressbar.Position:=total;
      end;


      if scanway=Increased_Value then
      begin
        //It's an Increased value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory5switch,number*8);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempdoublearray:=pointer(previousmemory5switch);
          previousmemory5switch:=pointer(previousmemory5);
          previousmemory5:=pointer(tempdoublearray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory5switch,number*8);

          doublep:=@previousmemory5[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            doublesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(doublesscannedstart+$1000-7)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(doublesscannedstart),@doublesscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(doublesscannedstart+$1000-7) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pdouble(dword(@doublesscanned[0])+searchaddress[i]-doublesscannedstart),8,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(doublesscannedstart+$1000-7)) or (j2)) do
            begin
              doublescanned:=pdouble(dword(@doublesscanned[0])+searchaddress[i]-doublesscannedstart)^;
              if (doublescanned>doublep^) and (not (isnan(doublescanned) or isinfinite(doublescanned))) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue5[found]:=doublescanned;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdoublearray:=pointer(foundvalue5switch);
                  foundvalue5switch:=pointer(foundvalue5);
                  foundvalue5:=pointer(tempdoublearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

                  found:=0;
                end;
              end;
              inc(doublep);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue5,8*found);
        progressbar.position:=total;
      end;


      if scanway=Increased_Value_by then
      begin
        //It's an Increased value by scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory5switch,number*8);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempdoublearray:=pointer(previousmemory5switch);
          previousmemory5switch:=pointer(previousmemory5);
          previousmemory5:=pointer(tempdoublearray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory5switch,number*8);

          doublep:=@previousmemory5[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            doublesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(doublesscannedstart+$1000-7)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(doublesscannedstart),@doublesscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(doublesscannedstart+$1000-7) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pdouble(dword(@doublesscanned[0])+searchaddress[i]-doublesscannedstart),8,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(doublesscannedstart+$1000-7)) or (j2)) do
            begin
              doublescanned:=pdouble(dword(@doublesscanned[0])+searchaddress[i]-doublesscannedstart)^;
              helpdouble:=RoundTo(doublescanned,-decim);
              helpdouble2:=RoundTo(doublep^+doublevalue,-decim);
              if (actualwrite=4) and
              (
              (
              (not percentage)
              and
              (helpdouble=helpdouble2)
              )
              or
              (
              percentage
              and
              (doublescanned>=doublep^+(doublep^*(doublevalue/100)))
              )
              ) and (not (isnan(doublescanned) or isinfinite(doublescanned))) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue5[found]:=doublescanned;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdoublearray:=pointer(foundvalue5switch);
                  foundvalue5switch:=pointer(foundvalue5);
                  foundvalue5:=pointer(tempdoublearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

                  found:=0;
                end;
              end;
              inc(doublep);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue5,8*found);
        progressbar.position:=total;
      end;

      if scanway=Decreased_Value then
      begin
        //It's an Increased value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory5switch,number*8);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempdoublearray:=pointer(previousmemory5switch);
          previousmemory5switch:=pointer(previousmemory5);
          previousmemory5:=pointer(tempdoublearray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory5switch,number*8);

          doublep:=@previousmemory5[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            doublesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(doublesscannedstart+$1000-7)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(doublesscannedstart),@doublesscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(doublesscannedstart+$1000-7) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pdouble(dword(@doublesscanned[0])+searchaddress[i]-doublesscannedstart),8,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(doublesscannedstart+$1000-7)) or (j2)) do
            begin
              doublescanned:=pdouble(dword(@doublesscanned[0])+searchaddress[i]-doublesscannedstart)^;
              if (doublescanned<doublep^) and (not (isnan(doublescanned) or isinfinite(doublescanned))) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue5[found]:=doublescanned;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdoublearray:=pointer(foundvalue5switch);
                  foundvalue5switch:=pointer(foundvalue5);
                  foundvalue5:=pointer(tempdoublearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

                  found:=0;
                end;
              end;
              inc(doublep);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue5,8*found);
        progressbar.position:=total;
      end;


      if scanway=Decreased_Value_by then
      begin
        //It's an Increased value by scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory5switch,number*8);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempdoublearray:=pointer(previousmemory5switch);
          previousmemory5switch:=pointer(previousmemory5);
          previousmemory5:=pointer(tempdoublearray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory5switch,number*8);

          doublep:=@previousmemory5[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            doublesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(doublesscannedstart+$1000-7)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(doublesscannedstart),@doublesscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(doublesscannedstart+$1000-7) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pdouble(dword(@doublesscanned[0])+searchaddress[i]-doublesscannedstart),8,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(doublesscannedstart+$1000-7)) or (j2)) do
            begin
              doublescanned:=pdouble(dword(@doublesscanned[0])+searchaddress[i]-doublesscannedstart)^;
              helpdouble:=RoundTo(doublescanned,-decim);
              helpdouble2:=RoundTo(doublep^-doublevalue,-decim);
              if (actualwrite=4) and
              (
              (
              (not percentage)
              and
              (helpdouble=helpdouble2)
              )
              or
              (
              percentage
              and
              (doublescanned<=doublep^-(doublep^*(doublevalue/100)))
              )
              ) and (not (isnan(doublescanned) or isinfinite(doublescanned))) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue5[found]:=doublescanned;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdoublearray:=pointer(foundvalue5switch);
                  foundvalue5switch:=pointer(foundvalue5);
                  foundvalue5:=pointer(tempdoublearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

                  found:=0;
                end;
              end;
              inc(doublep);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue5,8*found);
        progressbar.position:=total;
      end;


      if scanway=Changed_value then
      begin
        //It's an changed value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory5switch,number*8);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempdoublearray:=pointer(previousmemory5switch);
          previousmemory5switch:=pointer(previousmemory5);
          previousmemory5:=pointer(tempdoublearray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory5switch,number*8);

          doublep:=@previousmemory5[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            doublesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(doublesscannedstart+$1000-7)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(doublesscannedstart),@doublesscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(doublesscannedstart+$1000-7) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pdouble(dword(@doublesscanned[0])+searchaddress[i]-doublesscannedstart),8,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(doublesscannedstart+$1000-7)) or (j2)) do
            begin
              doublescanned:=pdouble(dword(@doublesscanned[0])+searchaddress[i]-doublesscannedstart)^;
              if (doublescanned<>doublep^) and (not (isnan(doublescanned) or isinfinite(doublescanned))) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue5[found]:=doublescanned;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdoublearray:=pointer(foundvalue5switch);
                  foundvalue5switch:=pointer(foundvalue5);
                  foundvalue5:=pointer(tempdoublearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

                  found:=0;
                end;
              end;
              inc(doublep);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue5,8*found);
        progressbar.position:=total;
      end;

      if scanway=UnChanged_value then
      begin
        //It's an Unchanged value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory5switch,number*8);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempdoublearray:=pointer(previousmemory5switch);
          previousmemory5switch:=pointer(previousmemory5);
          previousmemory5:=pointer(tempdoublearray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory5switch,number*8);

          doublep:=@previousmemory5[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            doublesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(doublesscannedstart+$1000-7)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(doublesscannedstart),@doublesscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(doublesscannedstart+$1000-7) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pdouble(dword(@doublesscanned[0])+searchaddress[i]-doublesscannedstart),8,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(doublesscannedstart+$1000-7)) or (j2)) do
            begin
              doublescanned:=pdouble(dword(@doublesscanned[0])+searchaddress[i]-doublesscannedstart)^;
              if (doublescanned=doublep^) and (not (isnan(doublescanned) or isinfinite(doublescanned))) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue5[found]:=doublescanned;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdoublearray:=pointer(foundvalue5switch);
                  foundvalue5switch:=pointer(foundvalue5);
                  foundvalue5:=pointer(tempdoublearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

                  found:=0;
                end;
              end;
              inc(doublep);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue5,8*found);
        progressbar.position:=total;
      end;

      {$ifndef netserver}
      if scanway=SameAsFirst then
      begin
        //It's an same as first value scan
        FSHandler:=TFirstscanhandler.create;
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory5switch,number*8);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempdoublearray:=pointer(previousmemory5switch);
          previousmemory5switch:=pointer(previousmemory5);
          previousmemory5:=pointer(tempdoublearray);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory5switch,number*8);

          doublep:=@previousmemory5[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            doublesscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(doublesscannedstart+$1000-7)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(doublesscannedstart),@doublesscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(doublesscannedstart+$1000-7) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pdouble(dword(@doublesscanned[0])+searchaddress[i]-doublesscannedstart),8,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(doublesscannedstart+$1000-7)) or (j2)) do
            begin
              doublescanned:=pdouble(dword(@doublesscanned[0])+searchaddress[i]-doublesscannedstart)^;
              helpdouble:=fshandler.getfirstscandouble(searchaddress[i]);
              if (doublescanned=helpdouble) and (not (isnan(doublescanned) or isinfinite(doublescanned))) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue5[found]:=doublescanned;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempdoublearray:=pointer(foundvalue5switch);
                  foundvalue5switch:=pointer(foundvalue5);
                  foundvalue5:=pointer(tempdoublearray);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue5switch,8*number);

                  found:=0;
                end;
              end;
              inc(doublep);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue5,8*found);
        progressbar.position:=total;
        fshandler.Free;
      end;
      {$endif}

    end;

    //--------------------------------------------------------------------------

    if valtype=6 then
    begin
      //It's a int64 Scan
      total:=filesize(memoryfile) div 4;

      progressbar.Max:=total;
      progressbar.Position:=0;

      setlength(searchaddress,number);
      setlength(searchaddressswitch,number);
      setlength(foundvalue6,number);
      setlength(foundvalue6switch,number);
      setlength(previousmemory6,number);
      setlength(previousmemory6switch,number);
      setlength(int64sscanned,4096); //or 1024

      foundvalue6[0]:=int64value+1;
      foundvalue6switch[0]:=int64value+1;

      if scanway=Exact_Value then
      begin
        //It's an Exact value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          //lets start reading the next block while i'm scanning this block
          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            int64sscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(int64sscannedstart+$1000-7)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(int64sscannedstart),@int64sscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(int64sscannedstart+$1000-7) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pint64(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart),8,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(int64sscannedstart+$1000-7)) or (j2)) do
            begin
              if pint64(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart)^=int64value then
              begin
                foundaddress[found]:=SearchAddress[i];
                inc(found);
                inc(found2);

                if found=number then
                begin
                  if foundvalue6[0]<>int64value then
                  begin
                    for k:=0 to number-1 do foundvalue6[k]:=int64value;
                    FoundIsFilled:=true;
                  end;

                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempint64array:=pointer(foundvalue6switch);
                  foundvalue6switch:=pointer(foundvalue6);
                  foundvalue6:=pointer(tempint64array);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

                  found:=0;
                end;
              end;
              inc(i);
              dec(j);
            end;

          end;
          progressbar.StepBy(actualread div 4);
        end;

        if foundvalue6[0]<>int64value then
        begin
          for k:=0 to number-1 do foundvalue6[k]:=int64value;
          FoundIsFilled:=true;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue6,8*found);

        progressbar.position:=total;
      end;


      if scanway=BiggerThan then
      begin
        //It's an Increased value by scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          //lets start reading the next block while i'm scanning this block
          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            int64sscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(int64sscannedstart+$1000-7)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(int64sscannedstart),@int64sscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(int64sscannedstart+$1000-7) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pint64(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart),8,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(int64sscannedstart+$1000-7)) or (j2)) do
            begin
              if pint64(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart)^>int64value then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue6[found]:=pint64(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart)^;
                inc(found);
                inc(found2);

                if found=number then
                begin
                  if foundvalue6[0]<>int64value then
                  begin
                    for k:=0 to number-1 do foundvalue6[k]:=int64value;
                    FoundIsFilled:=true;
                  end;

                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempint64array:=pointer(foundvalue6switch);
                  foundvalue6switch:=pointer(foundvalue6);
                  foundvalue6:=pointer(tempint64array);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

                  found:=0;
                end;
              end;
              inc(i);
              dec(j);
            end;

          end;
          progressbar.StepBy(actualread div 4);
        end;

        if foundvalue6[0]<>int64value then
        begin
          for k:=0 to number-1 do foundvalue6[k]:=int64value;
          FoundIsFilled:=true;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue6,8*found);

        progressbar.position:=total;
      end;

      if scanway=SmallerThan then
      begin
        //It's an Increased value by scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          //lets start reading the next block while i'm scanning this block
          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            int64sscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(int64sscannedstart+$1000-7)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(int64sscannedstart),@int64sscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(int64sscannedstart+$1000-7) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pint64(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart),8,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(int64sscannedstart+$1000-7)) or (j2)) do
            begin
              if pint64(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart)^<int64value then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue6[found]:=pint64(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart)^;
                inc(found);
                inc(found2);

                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempint64array:=pointer(foundvalue6switch);
                  foundvalue6switch:=pointer(foundvalue6);
                  foundvalue6:=pointer(tempint64array);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

                  found:=0;
                end;
              end;
              inc(i);
              dec(j);
            end;

          end;
          progressbar.StepBy(actualread div 4);
        end;

        if foundvalue6[0]<>int64value then
        begin
          for k:=0 to number-1 do foundvalue6[k]:=int64value;
          FoundIsFilled:=true;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue6,8*found);

        progressbar.position:=total;
      end;

      if scanway=ValueBetween then
      begin
        //It's an Increased value by scan

        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          //lets start reading the next block while i'm scanning this block
          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,nil,0);

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            int64sscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(int64sscannedstart+$1000-7)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(int64sscannedstart),@int64sscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(int64sscannedstart+$1000-7) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pint64(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart),8,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(int64sscannedstart+$1000-7)) or (j2)) do
            begin
              int64scanned:=pint64(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart)^;
              if (int64scanned>=int64value) and (int64scanned<=int64value2) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue6[found]:=int64scanned;

                inc(found);
                inc(found2);

                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempint64array:=pointer(foundvalue6switch);
                  foundvalue6switch:=pointer(foundvalue6);
                  foundvalue6:=pointer(tempint64array);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

                  found:=0;
                end;
              end;
              inc(i);
              dec(j);
            end;

          end;
          progressbar.StepBy(actualread div 4);
        end;

        if foundvalue6[0]<>int64value then
        begin
          for k:=0 to number-1 do foundvalue6[k]:=int64value;
          FoundIsFilled:=true;
        end;

        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue6,8*found);

        progressbar.position:=total;
      end;


      if scanway=Increased_Value then
      begin
        //It's an Increased value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory6switch,number*8);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempint64array:=pointer(previousmemory6switch);
          previousmemory6switch:=pointer(previousmemory6);
          previousmemory6:=pointer(tempint64array);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory6switch,number*8);

          int64p:=@previousmemory6[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            int64sscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(int64sscannedstart+$1000-7)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(int64sscannedstart),@int64sscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(int64sscannedstart+$1000-7) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pint64(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart),8,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(int64sscannedstart+$1000-7)) or (j2)) do
            begin
              if pint64(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart)^>int64p^ then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue6[found]:=pint64(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart)^;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempint64array:=pointer(foundvalue6switch);
                  foundvalue6switch:=pointer(foundvalue6);
                  foundvalue6:=pointer(tempint64array);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

                  found:=0;
                end;
              end;
              inc(int64p);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue6,8*found);
        progressbar.position:=total;
      end;


      if scanway=Increased_Value_By then
      begin
        //It's an Increased value by scan

        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory6switch,number*8);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempint64array:=pointer(previousmemory6switch);
          previousmemory6switch:=pointer(previousmemory6);
          previousmemory6:=pointer(tempint64array);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory6switch,number*8);

          int64p:=@previousmemory6[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            int64sscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(int64sscannedstart+$1000-7)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(int64sscannedstart),@int64sscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(int64sscannedstart+$1000-7) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pint64(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart),8,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(int64sscannedstart+$1000-7)) or (j2)) do
            begin
              int64scanned:=pint64(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart)^;
              if
               (
                 ((not percentage) and (int64scanned=int64p^+int64value))
                 or
                 (percentage and (int64scanned>=int64p^+trunc(int64p^*(int64value/100))))
               ) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue6[found]:=pint64(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart)^;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempint64array:=pointer(foundvalue6switch);
                  foundvalue6switch:=pointer(foundvalue6);
                  foundvalue6:=pointer(tempint64array);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

                  found:=0;
                end;
              end;
              inc(int64p);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue6,8*found);
        progressbar.position:=total;
      end;

      if scanway=Decreased_Value then
      begin
        //It's an Decreased value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory6switch,number*8);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempint64array:=pointer(previousmemory6switch);
          previousmemory6switch:=pointer(previousmemory6);
          previousmemory6:=pointer(tempint64array);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory6switch,number*8);

          int64p:=@previousmemory6[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            int64sscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(int64sscannedstart+$1000-7)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(int64sscannedstart),@int64sscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(int64sscannedstart+$1000-7) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pdword(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart),8,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(int64sscannedstart+$1000-7)) or (j2)) do
            begin
              if pdword(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart)^<int64p^ then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue6[found]:=pdword(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart)^;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempint64array:=pointer(foundvalue6switch);
                  foundvalue6switch:=pointer(foundvalue6);
                  foundvalue6:=pointer(tempint64array);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

                  found:=0;
                end;
              end;
              inc(int64p);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue6,8*found);
        progressbar.position:=total;
      end;

      if scanway=Decreased_Value_By then
      begin
        //It's an Increased value by scan

        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory6switch,number*8);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempint64array:=pointer(previousmemory6switch);
          previousmemory6switch:=pointer(previousmemory6);
          previousmemory6:=pointer(tempint64array);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory6switch,number*8);

          int64p:=@previousmemory6[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            int64sscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(int64sscannedstart+$1000-7)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(int64sscannedstart),@int64sscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(int64sscannedstart+$1000-7) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pdword(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart),8,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(int64sscannedstart+$1000-7)) or (j2)) do
            begin
              int64scanned:=pdword(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart)^;
              if
               (
                 ((not percentage) and (int64scanned=int64p^-int64value))
                 or
                 (percentage and (int64scanned<=int64p^-trunc(int64p^*(int64value/100))))
               ) then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue6[found]:=pdword(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart)^;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempint64array:=pointer(foundvalue6switch);
                  foundvalue6switch:=pointer(foundvalue6);
                  foundvalue6:=pointer(tempint64array);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

                  found:=0;
                end;
              end;
              inc(int64p);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue6,8*found);
        progressbar.position:=total;
      end;


      if scanway=Changed_value then
      begin
        //It's an changed value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory6switch,number*8);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempint64array:=pointer(previousmemory6switch);
          previousmemory6switch:=pointer(previousmemory6);
          previousmemory6:=pointer(tempint64array);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory6switch,number*8);

          int64p:=@previousmemory6[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            int64sscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(int64sscannedstart+$1000-7)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(int64sscannedstart),@int64sscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(int64sscannedstart+$1000-7) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pdword(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart),8,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(int64sscannedstart+$1000-7)) or (j2)) do
            begin
              if pdword(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart)^<>int64p^ then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue6[found]:=pdword(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart)^;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempint64array:=pointer(foundvalue6switch);
                  foundvalue6switch:=pointer(foundvalue6);
                  foundvalue6:=pointer(tempint64array);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

                  found:=0;
                end;
              end;
              inc(int64p);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue6,8*found);
        progressbar.position:=total;
      end;

      if scanway=UnChanged_value then
      begin
       //It's an unchanged value scan
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory6switch,number*8);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempint64array:=pointer(previousmemory6switch);
          previousmemory6switch:=pointer(previousmemory6);
          previousmemory6:=pointer(tempint64array);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory6switch,number*8);

          int64p:=@previousmemory6[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            int64sscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(int64sscannedstart+$1000-7)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(int64sscannedstart),@int64sscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(int64sscannedstart+$1000-7) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pdword(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart),8,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(int64sscannedstart+$1000-7)) or (j2)) do
            begin
              if pdword(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart)^=int64p^ then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue6[found]:=pdword(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart)^;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempint64array:=pointer(foundvalue6switch);
                  foundvalue6switch:=pointer(foundvalue6);
                  foundvalue6:=pointer(tempint64array);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

                  found:=0;
                end;
              end;
              inc(int64p);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue6,8*found);
        progressbar.position:=total;
      end;

      {$ifndef netserver}
      if scanway=SameAsFirst then
      begin
        //It's an same as first value scan
        FSHandler:=TFirstscanhandler.create;
        prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory6switch,number*8);

        actualread:=number*4;
        while actualread=number*4 do
        begin
          actualread:=finishprefetching;

          tempdwordarray:=pointer(searchaddressswitch);
          searchaddressswitch:=pointer(searchaddress);
          searchaddress:=pointer(tempdwordarray);

          tempint64array:=pointer(previousmemory6switch);
          previousmemory6switch:=pointer(previousmemory6);
          previousmemory6:=pointer(tempint64array);

          if actualread=number*4 then prefetchbuffer(addressfile,memoryfile,searchaddressswitch,number*4,previousmemory6switch,number*8);

          int64p:=@previousmemory6[0];

          i:=0;
          l:=actualread div 4;
          while i<l do
          begin
            int64sscannedstart:=searchaddress[i] div $1000 *$1000;

            if (l-i>10) and (searchaddress[i+10]<(int64sscannedstart+$1000-7)) then
            begin
              //do a big block
              if readprocessmemory(processhandle,pointer(int64sscannedstart),@int64sscanned[0],$1000,actualwrite) then
                j:=$1000-1
              else
              begin
                while searchaddress[i]<(int64sscannedstart+$1000-7) do inc(i);
                continue;
              end;
              j2:=false;
            end
            else
            begin
              if readprocessmemory(processhandle,pointer(searchaddress[i]),pdword(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart),8,actualwrite) then
                j:=1
              else
              begin
                inc(i);
                continue;
              end;
              j2:=true; //only 1, allow unalligned endings
            end;

            while (j>0) and (i<l) and ((searchaddress[i]<(int64sscannedstart+$1000-7)) or (j2)) do
            begin
              hi64:=fshandler.getfirstscanint64(searchaddress[i]);
              if pdword(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart)^=hi64 then
              begin
                foundaddress[found]:=SearchAddress[i];
                foundvalue6[found]:=pdword(dword(@int64sscanned[0])+searchaddress[i]-int64sscannedstart)^;
                inc(found);
                if found=number then
                begin
                  //write the currently found addresses to disk
                  flushthread.datawritten.WaitFor(infinite);

                  tempdwordarray:=pointer(foundaddressswitch);
                  foundaddressswitch:=pointer(foundaddress);
                  foundaddress:=pointer(tempdwordarray);

                  tempint64array:=pointer(foundvalue6switch);
                  foundvalue6switch:=pointer(foundvalue6);
                  foundvalue6:=pointer(tempint64array);

                  flushbuffer(newaddressfile,newmemoryfile,foundaddressswitch,4*number,foundvalue6switch,8*number);

                  found:=0;
                end;
              end;
              inc(int64p);
              inc(i);
              dec(j);
            end;
          end;
          progressbar.StepBy(actualread div 4);
        end;
        flushthread.datawritten.WaitFor(infinite);
        flushbuffer(newaddressfile,newmemoryfile,foundaddress,4*found,foundvalue6,8*found);
        progressbar.position:=total;
        fshandler.Free;
      end;
      {$endif}

    end;

    if valtype=8 then
    begin //array of byte
      //will have exact value, changed value and unchanged value
      setlength(searchaddress,number);

      if scanway=Exact_Value then
      begin
        if (pos('-',scantext)>0) or (pos(' ',scantext)>0) then
        begin
          //syntax is xx-xx-xx or xx xx xx
          j:=1;
          k:=0;
          scantext:=scantext+' ';
          for i:=1 to length(scantext) do
          begin
            if (scantext[i]=' ') or (scantext[i]='-') then
            begin
              helpstr:=copy(scantext,j,i-j);
              j:=i+1;
              setlength(bytes,k+1);
              try
                if option then bytes[k]:=strtoint('$'+helpstr)
                                       else bytes[k]:=strtoint(helpstr);
              except
                bytes[k]:=-1;
                //if it is not a '-' or ' ' or a valid value then I assume it is a wildcard.(
              end;
              inc(k);
            end;
          end;
        end else
        begin
          //syntax is xxxxxx
          k:=0;
          j:=1;
          for i:=1 to length(scantext) do
          begin
            if (i mod 2)=0 then
            begin
              helpstr:=copy(scantext,j,i-j+1);
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

        nrofbytes:=length(bytes);
        setlength(foundvalue8,(number*(nrofbytes+1)));
        blockwrite(memoryfile,nrofbytes,4,actualwrite); //save how many bytes each record is
        setlength(bytearray,nrofbytes);
        l:=0;

        //read the addresses
        actualread:=number*4;
        while actualread=number*4 do
        begin
          blockread(addressfile,pointer(searchaddress)^,number*4,actualread);
          for i:=0 to (actualread div 4)-1 do
          begin
            readprocessmemory(processhandle,pointer(searchaddress[i]),addr(bytearray[0]),nrofbytes,actualwrite);
            if (actualwrite=nrofbytes) then
            begin
              //find out if bytearray true with bytes[]
              for j:=0 to nrofbytes-1 do
              begin
                if (bytes[j]=-1) or (byte(bytes[j])=bytearray[j]) then continue;
                break;
              end;

              if dword(j)=nrofbytes then //found one
              begin
                foundaddress[found]:=searchaddress[i];
                copymemory(pointer(@foundvalue8[found*nrofbytes]),pointer(@bytearray[0]),nrofbytes);
                inc(found);

                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointeR(foundvalue8)^,nrofbytes*number,actualwrite);
                  found:=0;
                end;
              end;

            end;
          end;
        end;
        blockwrite(newAddressfile,pointer(foundaddress)^,4*found,actualwrite);
        blockwrite(newMemoryfile,pointeR(foundvalue8)^,nrofbytes*found,actualwrite);

      end;
    end;


  end;

  //----


  finishflushing;

  closefile(Addressfile);
  closefile(memoryfile);
  closefile(NewAddressFile);
  closefile(NewMemoryFile);
  deletefile(CheatEngineDir+'Memory.UNDO');
  deletefile(CheatEngineDir+'Addresses.UNDO');
  renamefile(CheatEngineDir+'Memory.tmp',cheatenginedir+'Memory.UNDO');
  renamefile(CheatEngineDir+'Addresses.tmp',CheatEngineDir+'Addresses.UNDO');
  renamefile(CheatEnginedir+'ADDRESSES2.TMP',Cheatenginedir+'ADDRESSES.TMP');
  renamefile(Cheatenginedir+'MEMORY2.TMP',Cheatenginedir+'MEMORY.TMP');

  try
    try
      resulthelper:=tfilestream.Create(CheatEngineDir+'Addresses.tmp',fmopenread,fmsharedenynone);
      result:=(resulthelper.Size-7) div 4;
    finally
      resulthelper.free;
    end;
  except
    reset(addressfile,1);
    result:=(filesize(addressfile)-7) div 4;
    closefile(addressfile);
  end;

  freememory;

{$endif}
{$endif}

end;


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


function GetMemoryRanges2(Start,Stop: Dword; readonly: boolean; Progressbar: TProgressBar;vartype:integer;fastscan:boolean):int64;
//FoundList will get the this if it's short enough;

var RMPointer: ^Byte;

    mbi : _MEMORY_BASIC_INFORMATION;
    address: Dword;
    size:       dword;

    i: Integer;
    j: Integer;

    actualread: dword;

    //save variables
    dataType:  String[6];  //REGION or NORMAL  (Always region in this procedure)


    TotalToRead: Dword;
    Total: dword;

    NO: boolean;
begin
  datatype:='REGION';
  MemoryRegions:=0;

  assignfile(Addressfile,CheatEngineDir+'Addresses.TMP');
  rewrite(Addressfile,1);

  assignfile(memoryfile,CheatEngineDir+'Memory.TMP');
  rewrite(memoryfile,1);  //just so next scan has a file to delete :)
  closefile(memoryfile);


  blockwrite(Addressfile,datatype,sizeof(datatype));
  closefile(Addressfile);

  address:=start;

  try

  while (Virtualqueryex(processhandle,pointer(address),mbi,sizeof(mbi))<>0) and (address<stop) and ((address+mbi.RegionSize)>address) do
  begin
    if (not (not scan_mem_private and (mbi.type_9=mem_private))) and (not (not scan_mem_image and (mbi.type_9=mem_image))) and (not (not scan_mem_mapped and (mbi.type_9=mem_mapped))) and (mbi.State=mem_commit) and ((mbi.Protect and page_guard)=0) and ((mbi.protect and page_noaccess)=0) then  //look if it is commited
    begin
      if not readonly then  //if the settings are set to not read read-only memory then
      begin
        if (((mbi.AllocationProtect) and (page_readonly or page_execute_read))=0) and
           (((mbi.Protect) and (page_readonly or PAGE_EXECUTE_READ))=0) then //look if the memory is not read-only  , if 0 it means it's not read-only
        begin
          no:=false;

          if Skip_PAGE_NOCACHE and ((mbi.AllocationProtect and PAGE_NOCACHE)=PAGE_NOCACHE) then
          begin
            address:=dword(mbi.BaseAddress)+mbi.RegionSize;
            continue;
          end;

          setlength(memoryregion,memoryregions+1);

          memoryregion[memoryregions].BaseAddress:=dword(mbi.baseaddress);  //remember if it's not read only
          memoryregion[memoryregions].MemorySize:=mbi.RegionSize;
          inc(memoryregions);
        end;
      end else  //if the settings are to also read the read only then:
      begin
        if Skip_PAGE_NOCACHE then
          if (mbi.AllocationProtect and PAGE_NOCACHE)=PAGE_NOCACHE then
          begin
            address:=dword(mbi.BaseAddress)+mbi.RegionSize;
            continue;
          end;

        setlength(memoryregion,memoryregions+1);

        memoryregion[memoryregions].BaseAddress:=dword(mbi.baseaddress);  //just remember this location
        memoryregion[memoryregions].MemorySize:=mbi.RegionSize;
        inc(memoryregions);
      end;
    end;


    address:=dword(mbi.baseaddress)+mbi.RegionSize;
  end;

  if memoryregions=0 then raise exception.create('No memory found in the specified region');
  //lets search really at the start of the location the user specified
  if (memoryregion[0].BaseAddress<start) and (memoryregion[0].MemorySize-(start-memoryregion[0].BaseAddress)>0) then
  begin
    memoryregion[0].MemorySize:=memoryregion[0].MemorySize-(start-memoryregion[0].BaseAddress);
    memoryregion[0].BaseAddress:=start;
  end;

  //also the right end
  if (memoryregion[memoryregions-1].BaseAddress+memoryregion[memoryregions-1].MemorySize)>stop then
    dec(memoryregion[memoryregions-1].MemorySize,(memoryregion[memoryregions-1].BaseAddress+memoryregion[memoryregions-1].MemorySize)-stop-1);

  //if anything went ok memoryregions should now contain all the addresses and sizes
  //to speed it up combine the regions that are attached to eachother.


  j:=0;
  address:=memoryregion[0].BaseAddress;
  size:=memoryregion[0].MemorySize;

  for i:=1 to memoryregions-1 do
  begin
    if memoryregion[i].BaseAddress=address+size then
      inc(size,memoryregion[i].MemorySize)
    else
    begin
      memoryregion[j].BaseAddress:=address;
      memoryregion[j].MemorySize:=size;

      address:=memoryregion[i].BaseAddress;
      size:=memoryregion[i].MemorySize;
      inc(j);
    end;
  end;

  memoryregion[j].BaseAddress:=address;
  memoryregion[j].MemorySize:=size;

  memoryregions:=j+1;
  setlength(memoryregion,memoryregions);
  
  //re-added due to complaints about speed:
  //split up into smaller chunks
  if buffersize>0 then
  begin
    i:=0;
    while i<=memoryregions-1 do
    begin
      if memoryregion[i].MemorySize>dword(buffersize) then
      begin
        inc(memoryregions);
        setlength(memoryregion,memoryregions);

        //copy the next element to the back, and split up the current one
        //(unless this is the item at the back, and not needed)
        if i<memoryregions-2 then
        begin
          //i isnt the last element of the array so do a semi-shift
          memoryregion[memoryregions-1].BaseAddress:=memoryregion[i+1].baseaddress;
          memoryregion[memoryregions-1].MemorySize:=memoryregion[i+1].MemorySize;
        end;



        memoryregion[i+1].IsChild:=true;
        if fastscan then
        case vartype of
        1: begin  //word
             memoryregion[i+1].BaseAddress:=memoryregion[i].BaseAddress+buffersize-1;
             memoryregion[i+1].MemorySize:=memoryregion[i].MemorySize-buffersize+1;
           end;

        2,3: begin  //dword+float
             memoryregion[i+1].BaseAddress:=memoryregion[i].BaseAddress+buffersize-3;
             memoryregion[i+1].MemorySize:=memoryregion[i].MemorySize-buffersize+3;
           end;

        4,6: begin  //double+int64
             memoryregion[i+1].BaseAddress:=memoryregion[i].BaseAddress+buffersize-7;
             memoryregion[i+1].MemorySize:=memoryregion[i].MemorySize-buffersize+7;
           end;
        else begin
               memoryregion[i+1].BaseAddress:=memoryregion[i].BaseAddress+buffersize;
               memoryregion[i+1].MemorySize:=memoryregion[i].MemorySize-buffersize;
             end;
        end
        else
        begin
          memoryregion[i+1].BaseAddress:=memoryregion[i].BaseAddress+buffersize;
          memoryregion[i+1].MemorySize:=memoryregion[i].MemorySize-buffersize;
        end;

        memoryregion[i].MemorySize:=buffersize;
      end;
      inc(i);
    end;
    dec(memoryregions);
  end else memoryregions:=j;

  if fastscan then
  begin
    //optimize regions
    for i:=0 to memoryregions do
    //make it so the regions all start at a location that can be devided by the size of the valtype
    case vartype of
      1: begin  //word (mod 2=0)
           j:=(memoryregion[i].BaseAddress mod 2);
           if j<>0 then
           if memoryregion[i].IsChild then
           begin
             memoryregion[i].BaseAddress:=memoryregion[i].BaseAddress-j;
             inc(memoryregion[i].MemorySize,j);
           end
           else
           begin
             memoryregion[i].BaseAddress:=memoryregion[i].BaseAddress+j;
             dec(memoryregion[i].MemorySize,j);
           end;
         end;

    2,3: begin  //dword+float
           j:=(memoryregion[i].BaseAddress mod 4);
           if j<>0 then
           if memoryregion[i].IsChild then
           begin
             memoryregion[i].BaseAddress:=memoryregion[i].BaseAddress-j;
             inc(memoryregion[i].MemorySize,j);
           end
           else
           begin
             memoryregion[i].BaseAddress:=memoryregion[i].BaseAddress+4-j;
             dec(memoryregion[i].MemorySize,4-j);
           end;
         end;

    4,6: begin //double+int64
           j:=(memoryregion[i].BaseAddress mod 8);
           if j<>0 then
           if memoryregion[i].IsChild then
           begin
             memoryregion[i].BaseAddress:=memoryregion[i].BaseAddress-j;
             inc(memoryregion[i].MemorySize,j);
           end
           else
           begin
             memoryregion[i].BaseAddress:=memoryregion[i].BaseAddress+8-j;
             dec(memoryregion[i].MemorySize,8-j);
           end;
         end;
    end;
  end;

  //sort memoryregions from small to high

  quicksortmemoryregions(0,length(memoryregion)-1);


  TotalToRead:=0;
  For i:=0 to Memoryregions do
    inc(TotalToRead,Memoryregion[i].MemorySize);

  progressbar.max:=memoryregions+1;

  freemem(memory);  //just make sure we have some memory left
  getmem(memory,TotalToRead);

  except
    on EOutOfMemory do
      begin
      //not enough free memory for the 2nd
        raise exception.Create('Not enough memory free to scan');
      end;
  end;

  RMPointer:=pointer(memory);

  total:=0;
  for i:=0 to memoryregions do
  begin
    readprocessmemory(processhandle,pointer(Memoryregion[i].BaseAddress),RMPointer,Memoryregion[i].MemorySize,actualread);

    inc(total,actualread);
    Memoryregion[i].MemorySize:=actualread;
    inc(RMPointer,actualread);

    progressbar.StepIt;
  end;

  advanced:=true;

  result:=total;

  if fastscan then
  begin
    case vartype of
      1: result:=total div 2; //word
      2,3: result:=total div 4; //dword+float
      4,6: result:=total div 8; //double+8bytes
    end;
  end;
end;



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
end;


procedure GetProcessList(ProcessList: TListBox);
var sl: tstringlist;
begin
  sl:=tstringlist.create;
  try
    processlist.Sorted:=false;
    GetProcessList(sl);
    processlist.Items.Clear;
    processlist.Items.AddStrings(sl);
  finally
    sl.free;
  end;
end;


procedure GetProcessList(ProcessList: TStrings);
Var SNAPHandle: THandle;
    ProcessEntry: ProcessEntry32;
    Check: Boolean;
begin
  processlist.clear;



  SNAPHandle:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
  If SnapHandle>0 then
  begin
    ProcessEntry.dwSize:=SizeOf(ProcessEntry);
    Check:=Process32First(SnapHandle,ProcessEntry);
    while check do
    begin
      if processentry.th32ProcessID<>0 then
        ProcessList.Add(IntTohex(processentry.th32ProcessID,8)+'-'+ExtractFilename(processentry.szExeFile));

        
      check:=Process32Next(SnapHandle,ProcessEntry);
    end;
    closehandle(snaphandle);
  end else raise exception.Create('I can''t get the process list. You are propably using windows NT. Use the window list instead!');
end;

procedure GetWindowList(ProcessList: TListBox{; var ArrIcons: TBytes});
var winhandle: Hwnd;
    winprocess: Dword;
    title: Pchar;

    x: tstringlist;
    i:integer;
begin
  getmem(title,101);
  x:=tstringlist.Create;

  processlist.clear;

  winhandle:=getwindow(getforegroundwindow,GW_HWNDFIRST);

  while winhandle<>0 do
  begin
    GetWindowThreadProcessId(winhandle,addr(winprocess));
    title[0]:=#0;
    getwindowtext(winhandle,title,100);

    title[100]:=#0;

    if length(title)>0 then
      x.Add(IntTohex(winprocess,8)+'-'+title);

    winhandle:=getwindow(winhandle,GW_HWNDNEXT);
  end;

  x.Sort;
  for i:=0 to x.Count-1 do
    processlist.Items.Add(x[i]);
    
  freemem(title);
end;

function GetCEdir:string;
var i: Integer;
begin
  i:=length(application.ExeName);
  while (i>1) and (application.ExeName[i]<>'\' ) do dec(i);
  CheatEngineDir:=copy(application.Exename,1,i);
  result:=CheatEngineDir;
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
  Set8087CW(old8087CW);
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
begin
  if s='' then
  begin
    result:=s;
    exit;
  end;
  start:=1;

  if pos('#',s)>0 then
  begin
    ishex:='';
    start:=2;
  end
  else
  begin
    ishex:='$';
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

initialization
  keysfilemapping:=0;
  keys:=nil;

  setlength(windowlist,0);
  setlength(donthidelist,0);
  allwindowsareback:=true;
  stealthhook:=0;
  iswin2kplus:=GetSystemType>=5;

  flushthread:=TSaveDataThread.Create(false); //used for scanning, starts idled because the event isn't triggered
  prefetchthread:=TPrefetchDataThread.create(false);

finalization
  if flushthread<>nil then
  begin
    flushthread.Terminate;
    flushthread.dataavailable.SetEvent;
    flushthread.WaitFor;
    flushthread.free;
  end;

end.




