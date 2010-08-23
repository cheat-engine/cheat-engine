unit CEFuncProc;

{$MODE Delphi}

//This version of CEFuncProc has been COPIED to the server dir
//Cheat Engine regular WONT look at this

interface

uses jwawindows, windows, LCLIntf,StdCtrls,Classes,SysUtils,dialogs,{tlhelp32,}forms,messages,
Graphics,
ComCtrls,
{reinit, }
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
math,syncobjs, shellapi, ProcessHandlerUnit, controls;




//memscan
type TScanOption=(soUnknownValue,soExactValue,soValueBetween,soBiggerThan,soSmallerThan, soIncreasedValue, soIncreasedValueBy, soDecreasedValue, soDecreasedValueBy, soChanged, soUnchanged, soSameAsFirst, soCustom);
type TScanType=(stNewScan, stFirstScan, stNextScan);
type TRoundingType=(rtRounded,rtExtremerounded,rtTruncated);
type TVariableType=(vtByte, vtWord, vtDword, vtQword, vtSingle, vtDouble, vtString, vtUnicodeString, vtByteArray, vtBinary, vtAll, vtAutoAssembler, vtPointer, vtCustom);
type TCustomScanType=(cstNone, cstAutoAssembler, cstCPP, cstDLLFunction);


Type TBytes = array of integer; //An array that represents a row of byte. Ints are used to be able to represent wildcards (-1)
type tfloatscan=(rounded,extremerounded,truncated);
Type TMemoryRegion = record
  BaseAddress: ptrUint;
  MemorySize: qword;
  IsChild: boolean;  //means there is a region before it
  startaddress: pointer; //pointer to a spot in the whole memory copy, it means the start of this region
  end;
type TMemoryRegions = array of TMemoryRegion;
type PMemoryRegions = ^TMemoryRegions;

type TBitAddress = record
  address: ptruint;
  bit: dword;
end;

type TBitAddressArray=array [0..0] of TBitAddress;
type PBitAddressArray=^TBitAddressArray;

type TProcessListInfo=record
  processID: dword;
  processIcon: HICON;
end;
PProcessListInfo=^TProcessListInfo;



function ConvertHexStrToRealStr(const s: string): string;
function HexStrToInt(const S: string): Integer;
function HexStrToInt64(const S: string): Int64;

function OldVarTypeToNewVarType(i: integer):TVariableType;
function VariableTypeToString(variableType: TVariableType): string;
function StringToVariableType(s: string): TVariableType;

function readAndParseAddress(address: ptrUint; variableType: TVariableType): string;
function isjumporcall(address: ptrUint; var addresstojumpto: ptrUint): boolean;
{
procedure quicksortmemoryregions(lo,hi: integer);     //obsolete
}

procedure rewritecode(processhandle: thandle; address:ptrUint; buffer: pointer; size:dword);
procedure rewritedata(processhandle: thandle; address:ptrUint; buffer: pointer; size:dword);

procedure GetProcessList(ProcessList: TListBox); overload;
procedure GetProcessList(ProcessList: TStrings); overload;
procedure GetWindowList(ProcessList: TListBox; showInvisible: boolean=true);
function AvailMem:SIZE_T;
function isreadable(address:ptrUint):boolean;


procedure RemoveAddress(address: Dword;bit: Byte; vartype: Integer);

function GetCEdir:string;
procedure Open_Process;
Procedure Shutdown;
function KeyToStr(key:word):string;


procedure ConvertStringToBytes(scanvalue:string; hex:boolean;var bytes: TBytes);
function getbit(bitnr: integer; bt: qword):integer;
procedure setbit(bitnr: integer; var bt: Byte;state:integer); overload;
procedure setbit(bitnr: integer; var bt: dword;state:integer); overload;
procedure setbit(bitnr: integer; var bt: qword;state:integer); overload;

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
function IntToBin(i: qword):string;
function BinToInt(s: string): int64;

procedure decimal(var key: char);
procedure hexadecimal(var key: char);

function GetSystemType: Integer;


procedure ToggleOtherWindows;

Procedure InjectDll(dllname: string; functiontocall: string='');
Function GetRelativeFilePath(filename: string):string;

function GetCPUCount: integer;
function HasHyperthreading: boolean;
procedure SaveFormPosition(form: Tform; extra: array of integer);
function LoadFormPosition(form: Tform; var x: array of integer):boolean; 

function heapflagstostring(heapflags: dword): string;
function allocationtypetostring(alloctype: dword): string;
function allocationprotecttostring(protect: dword): string;
function freetypetostring(freetype: dword):string;
function isAddress(address: ptrUint):boolean;
function isExecutableAddress(address: ptrUint):boolean;
function MinX(a, b: ptrUint): ptrUint;inline; overload; //fpc2.4.1 has no support for unsigned
function MaxX(a, b: ptrUint): ptrUint;inline; overload;


function InRangeX(const AValue, AMin, AMax: ptrUint): Boolean;inline;

function FindFreeBlockForRegion(base: ptrUint; size: dword): pointer;


procedure errorbeep;



{$ifndef net}
procedure SetLanguage;

{$endif}

{$ifndef standalonetrainer}
procedure DetachIfPossible;
{$endif}

procedure getexecutablememoryregionsfromregion(start: ptrUint; stop:ptrUint; var memoryregions: TMemoryRegions);

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
  Address: ptrUint;  //only used when last pointer in list
  Interpretableaddress: string; //same as address
  offset: integer;
end;

type TCEAlloc=record
  address: ptrUint;
  varname: string;
  size: dword;
  prefered: ptrUint;
end;
type PCEAlloc=^TCEAlloc;
type TCEAllocArray=array of TCEAlloc;

type
  MemoryRecord = record
        Description : string;
        Address : ptrUint;
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
        Address : ptrUint;
        VarType : byte;
        Frozen : boolean;
        FrozenValue : Dword;
  end;

type TPtrUintArray=array[0..100] of ptruint;
type PPtrUintArray=^TPtrUintArray;

type TDwordArray=array[0..100] of dword;
type PDwordArray=^TDwordArray;

type TSingleArray=array[0..100] of single;
type PSingleArray=^TSingleArray;

type TdoubleArray=array[0..100] of double;
type PdoubleArray=^TdoubleArray;

type Tint64Array=array[0..100] of int64;
type Pint64Array=^Tint64Array;

type Tuint64Array=array[0..100] of uint64;
type Puint64Array=^Tuint64Array;

type TExtendedArray=array[0..100] of extended;
type PExtendedArray=^TExtendedArray;



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
  sleeptime: dword; //obsolete
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
  lagaddress: ptrUint;
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
  systemtype: integer;
  old8087CW: word;  //you never know...
  ProcessSelected: Boolean;
  //ProcessID: Dword; //deperecated
  //ProcessHandle: Thandle;

  Skip_PAGE_NOCACHE: boolean=false;
  Scan_MEM_PRIVATE: boolean=true;
  Scan_MEM_IMAGE: boolean=true;
  Scan_MEM_MAPPED: boolean=false;

  CheatEngineDir: String;
  WindowsDir: string;
  GetProcessIcons: Boolean;
  ProcessesWithIconsOnly: boolean;

//scanhelpers
  nrofbits: integer;
  Bitscan: array of byte;
  tempbits: array of byte;

  bitoffsetchange: integer;


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
{  SearchAddress: array of dword;
  searchaddressswitch: array of dword;

  SearchAddressB: array of TBitAddress;}

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



//  MemoryRegion: array of TMemoryRegion;
//  MemoryRegions: Integer;
  
//  Memory: Array of Byte;
  Memory: ^Byte;
  memory2: ^byte;


  advanced: boolean;
  //global files, so when an exception happens I can close them
//  addressfile, memoryfile: File;
//  newAddressfile,newmemoryfile: File;

  buffersize: dword=512*1024;
  overridedebug: boolean;

  totalbytes: dword;
  currentbyte: dword;


  //hide/show windows
  windowlist: array of thandle;
  lastforeground,lastactive: thandle;
  donthidelist: array of string;
  onlyfront: boolean;
  allwindowsareback:boolean;

  //HyperscanFileMapping: THandle;
  //HyperscanView: ^TScanSettings;
  
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

  useAPCtoInjectDLL: boolean;


  tempdir: pchar;
  dontusetempdir: boolean;
  tempdiralternative: string;



  processhandler: TProcessHandler;



type
  SYSTEM_INFO = record
                case longint of
                   0 : ( dwOemId : DWORD;
  		       dwPageSize : DWORD;
            	       lpMinimumApplicationAddress : LPVOID;
            	       lpMaximumApplicationAddress : LPVOID;
            	       dwActiveProcessorMask : DWORD_PTR;
            	       dwNumberOfProcessors : DWORD;
                         dwProcessorType : DWORD;
                         dwAllocationGranularity : DWORD;
                         wProcessorLevel : WORD;
                         wProcessorRevision : WORD;
  			 );
                   1 : (
                        wProcessorArchitecture : WORD;
                      );
         end;

var
  systeminfo: SYSTEM_INFO;

implementation

{$ifdef net}
uses disassembler,debugger,debughelper;
{$endif}

{$ifndef net}

{$ifndef standalonetrainer}
uses disassembler,CEDebugger,debughelper, symbolhandler,frmProcessWatcherUnit,kerneldebugger, formsettingsunit;
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



procedure errorbeep;
begin
  beep;
  sleep(100);
  beep;
  sleep(100);
  beep;
  sleep(100);
end;

function isreadable(address:ptrUint):boolean;
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

procedure getexecutablememoryregionsfromregion(start: ptrUint; stop:ptrUint; var memoryregions: tmemoryregions);
var address: ptrUint;
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
      memoryregions[length(memoryregions)-1].BaseAddress:=ptrUint(mbi.baseaddress);
      memoryregions[length(memoryregions)-1].MemorySize:=mbi.RegionSize;
    end;

    inc(address,mbi.RegionSize);
  end;

end;


{$ifndef standalonetrainer}
procedure FillMemoryProcess(start:ptrUint;count:dword;fillvalue:byte);
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

procedure DetachIfPossible;
begin
  if debuggerthread<>nil then
  begin
    debuggerthread.Terminate;
    debuggerthread.WaitFor;
    freeandnil(debuggerthread);
  end;
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
    position,position2: ptrUint;

    dllLocation: string;
    startaddresS: ptrUint;
    functionloc: ptrUint;
    injectionlocation: pointer;
    threadhandle: thandle;
begin
  //todo: Change this to a full AA script (but make sure not to call injectdll in there :)  )

  h:=LoadLibrary('Kernel32.dll');
  if h=0 then raise exception.Create('No kernel32.dll loaded');

  LoadLibraryPtr:=nil;
  GetProcAddressPtr:=nil;
  injectionlocation:=nil;

  try
    try
      getprocaddressptr:=pointer(symhandler.getAddressFromName('Kernel32!GetProcAddress',true));
    except
      GetProcAddressPtr:=GetProcAddress(h,'GetProcAddress');
    end;

    if getprocaddressptr=nil then raise exception.Create('GetProcAddress not found');

    try
      LoadLibraryPtr:=pointer(symhandler.getAddressFromName('Kernel32!LoadLibraryA',true));
    except
      //failed getting the address of LoadLibraryA, use old method
      LoadLibraryPtr:=GetProcAddress(h,'LoadLibraryA');
    end;


    if LoadLibraryptr=nil then raise exception.Create('LoadLibraryA not found');

    injectionlocation:=VirtualAllocEx(processhandle,nil,4096,MEM_COMMIT,PAGE_EXECUTE_READWRITE);

    if injectionlocation=nil then raise exception.Create('Failed to allocate memory');

    dlllocation:=dllname;

    position:=ptrUint(injectionlocation);
    position2:=0;
    copymemory(@inject[0],pchar(dllLocation+#0),length(dllLocation)+1);
    inc(position,length(dllLocation)+1);
    inc(position2,length(dllLocation)+1);

    functionloc:=position;
    copymemory(@inject[position2],pchar(functiontocall+#0),length(functiontocall)+1);
    inc(position,length(functiontocall)+1);
    inc(position2,length(functiontocall)+1);
    startaddress:=position;

    if processhandler.is64bit then
    begin
      //loadlibrary(cehook);
      assemble('SUB RSP,#32',position,outp);
      copymemory(@inject[position2],outp,length(outp));
      inc(position,length(outp));
      inc(position2,length(outp));

      assemble('MOV RCX,'+IntToHex(ptrUint(injectionlocation),8),position,outp);
      copymemory(@inject[position2],outp,length(outp));
      inc(position,length(outp));
      inc(position2,length(outp));

    end
    else
    begin
      //loadlibrary(cehook);
      assemble('PUSH '+IntToHex(ptrUint(injectionlocation),8),position,outp);
      copymemory(@inject[position2],outp,length(outp));
      inc(position,length(outp));
      inc(position2,length(outp));
    end;

    assemble('CALL '+IntToHex(ptrUint(LoadLibraryPtr),8),position,outp);
    copymemory(@inject[position2],outp,length(outp));
    inc(position,length(outp));
    inc(position2,length(outp));

    if processhandler.is64bit then
    begin
      assemble('ADD RSP,#32',position,outp);
      copymemory(@inject[position2],outp,length(outp));
      inc(position,length(outp));
      inc(position2,length(outp));
    end;

    //safetycode, test if the dll was actually loaded and skip if not
    if processhandler.is64bit then
      assemble('TEST RAX,RAX',position,outp)
    else
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
      if processhandler.is64bit then
      begin
        //loadlibrary(cehook);
        assemble('SUB RSP,#32',position,outp);
        copymemory(@inject[position2],outp,length(outp));
        inc(position,length(outp));
        inc(position2,length(outp));

        assemble('MOV RCX,'+IntToHex(ptrUint(functionloc),8),position,outp);
        copymemory(@inject[position2],outp,length(outp));
        inc(position,length(outp));
        inc(position2,length(outp));
      end
      else
      begin

        assemble('PUSH '+IntToHex(functionloc,8),position,outp);
        copymemory(@inject[position2],outp,length(outp));
        inc(position,length(outp));
        inc(position2,length(outp));

        assemble('PUSH EAX',position,outp);
        copymemory(@inject[position2],outp,length(outp));
        inc(position,length(outp));
        inc(position2,length(outp));
      end;
      assemble('CALL '+IntToHex(ptrUint(GetProcAddressPtr),8),position,outp);
      copymemory(@inject[position2],outp,length(outp));
      inc(position,length(outp));
      inc(position2,length(outp));

      if processhandler.is64bit then
      begin
        assemble('ADD RSP,#32',position,outp);
        copymemory(@inject[position2],outp,length(outp));
        inc(position,length(outp));
        inc(position2,length(outp));
      end;

      if processhandler.is64bit then
        assemble('TEST RAX,RAX',position,outp)
      else
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


      if processhandler.is64bit then
      begin
        //setup stack
        assemble('SUB RSP,#32',position,outp);
        copymemory(@inject[position2],outp,length(outp));
        inc(position,length(outp));
        inc(position2,length(outp));
      end;

      //call function
      if processhandler.is64bit then
        assemble('CALL RAX',position,outp)
      else
        assemble('CALL EAX',position,outp);

      if processhandler.is64bit then
      begin
        //setup stack
        assemble('ADD RSP,#32',position,outp);
        copymemory(@inject[position2],outp,length(outp));
        inc(position,length(outp));
        inc(position2,length(outp));
      end;

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

    //showmessage('injected code at:'+inttohex(startaddress,8));
    //exit;


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
      virtualfreeex(processhandle,injectionlocation,0,MEM_RELEASE);
  end;

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
  setlength(processlist,0);


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

  systemtype:=result;
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

function Inttobin(i: qword): string;
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



function getbit(bitnr: integer; bt: qword):integer;
begin
  result:=(bt shr bitnr) and 1;
end;


procedure setbit(bitnr: integer; var bt: qword;state:integer); overload;
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



function AvailMem:SIZE_T;
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
  address: ptrUint;
  bit: dword;
end;

var

    Addresses: Array [1..number] of ptrUint;
    BAddress: Array [1..number] of BitAddress;
    Memory: Array [1..8*number] of Byte;
    i,j: Integer;

    str: pchar;

    found: boolean;
    check,check2: Integer;
    newmemoryfile, newaddressfile, memoryfile,addressfile: file;


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

    check:=sizeof(pointer)*number;
    while (check=sizeof(pointer)*number) do
    begin
      blockread(addressfile,addresses,sizeof(pointer)*number,check);
      i:=0;
      j:=0;
      while (i<check div sizeof(pointer)) do
      begin
        inc(i);
        if addresses[i]<>address then //if it's not the selected address write it else dont write it.
          blockwrite(newaddressfile,addresses[i],sizeof(pointer),check2);
      end;
    end;
  end
  else
  if vartype<>5 then
  begin
    check:=sizeof(pointer)*number;
    while ((not found) and (check=sizeof(pointer)*number)) do
    begin
      blockread(addressfile,addresses,sizeof(pointer)*number,check);
      i:=0;
      while (not found) and (i<(check div sizeof(pointer))) do
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
        blockwrite(newaddressfile,addresses,sizeof(pointer)*number,check2);
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

      blockwrite(newaddressfile,addresses,check-sizeof(pointer),check2);

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
        check:=sizeof(pointer)*number;
        while (check=sizeof(pointer)*number) do
        begin
          blockread(addressfile,addresses,sizeof(pointer)*number,check);
          blockwrite(newaddressfile,addresses,check,check);
        end;

        //and same for memory
        check:=sizeof(pointer)*number;
        while (check=sizeof(pointer)*number) do
        begin
          blockread(memoryfile,addresses,sizeof(pointer)*number,check);
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

      {
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
         }

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
    pli: PProcessListInfo;
begin
  sl:=tstringlist.create;
  try
    processlist.Sorted:=false;
    for i:=0 to processlist.Items.count-1 do
      if processlist.Items.Objects[i]<>nil then
      begin
        pli:=pointer(processlist.Items.Objects[i]);
        if pli.processIcon>0 then
          DestroyIcon(pli.processIcon);
        freemem(pli);
      end;

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
    ProcessEntry: PROCESSENTRY32;
    Check: Boolean;

    HI: HICON;
    ProcessListInfo: PProcessListInfo;
    i,j: integer;
    s: string;
begin
  HI:=0;

  j:=0;
  for i:=0 to processlist.count-1 do
    if processlist.Objects[i]<>nil then
    begin
      ProcessListInfo:= pointer( processlist.Objects[i]);
      if ProcessListInfo.processIcon>0 then
        DestroyIcon(ProcessListInfo.processIcon);
      freemem(ProcessListInfo);
    end;

  processlist.clear;


  SNAPHandle:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
  If SnapHandle>0 then
  begin
    ZeroMemory(@ProcessEntry, sizeof(ProcessEntry));
    ProcessEntry.dwSize:=SizeOf(ProcessEntry);

    Check:=Process32First(SnapHandle,ProcessEntry);
    while check do
    begin
      if getprocessicons then
      begin
        s:='';


        HI:=ExtractIcon(hinstance,ProcessEntry.szExeFile,0);
        if HI=0 then
        begin
          i:=getlasterror;

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

procedure GetWindowList(ProcessList: TListBox; showInvisible: boolean=true);
var previouswinhandle, winhandle: Hwnd;
    winprocess: Dword;
    temp: Pchar;
    wintitle: string;

    x: tstringlist;
    i,j:integer;

    ProcessListInfo: PProcessListInfo;
    tempdword: dword;
begin
  getmem(temp,101);
  try
    x:=tstringlist.Create;

    for i:=0 to processlist.items.count-1 do
      if processlist.items.Objects[i]<>nil then
      begin
        ProcessListInfo:=PProcessListInfo(processlist.items.Objects[i]);
        if ProcessListInfo.processIcon>0 then
          DestroyIcon(ProcessListInfo.processIcon);

        freemem(ProcessListInfo);
      end;
    processlist.clear;

    winhandle:=getwindow(getforegroundwindow,GW_HWNDFIRST);

    i:=0;
    while (winhandle<>0) and (i<10000) do
    begin
      if showInvisible or IsWindowVisible(winhandle) then
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
          ProcessListInfo.processIcon:=0;
  {$ifndef standalonetrainer}
          if formsettings.cbProcessIcons.checked then
          begin
            tempdword:=0;
            if SendMessageTimeout(winhandle,WM_GETICON,ICON_SMALL,0,SMTO_ABORTIFHUNG, 100, tempdword )<>0 then
            begin
              ProcessListInfo.processIcon:=tempdword;
              if ProcessListInfo.processIcon=0 then
              begin
                if SendMessageTimeout(winhandle,WM_GETICON,ICON_SMALL2,0,SMTO_ABORTIFHUNG, 100, tempdword	)<>0 then
                  ProcessListInfo.processIcon:=tempdword;

                if ProcessListInfo.processIcon=0 then
                  if SendMessageTimeout(winhandle,WM_GETICON,ICON_BIG,0,SMTO_ABORTIFHUNG, 100, tempdword	)<>0 then
                    ProcessListInfo.processIcon:=tempdword;
              end;
            end else
            begin
              inc(i,100); //at worst case scenario this causes the list to wait 10 seconds
            end;
          end;
  {$endif}

          x.AddObject(IntTohex(winprocess,8)+'-'+wintitle,TObject(ProcessListInfo));
        end;
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

  if (pos('-',scanvalue)>0) or (pos(' ',scanvalue)>0) or (pos(',',scanvalue)>0) then
  begin
    //syntax is xx-xx-xx or xx xx xx
    j:=1;
    k:=0;
    scanvalue:=scanvalue+' ';

    for i:=1 to length(scanvalue) do
    begin
      if (scanvalue[i] in [' ', '-', ',']) then
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



procedure rewritedata(processhandle: thandle; address:ptrUint; buffer: pointer; size:dword);
var written: dword;
    original,a: dword;
begin
//make writable, write, restore, flush
  VirtualProtectEx(processhandle,  pointer(address),size,PAGE_EXECUTE_READWRITE,original);
  writeprocessmemory(processhandle,pointer(address),buffer,size,written);
  VirtualProtectEx(processhandle,pointer(address),size,original,a);
end;

procedure rewritecode(processhandle: thandle; address:ptrUint; buffer: pointer; size:dword);
begin
  rewritedata(processhandle,address,buffer,size);
  FlushInstructionCache(processhandle,pointer(address),size);
end;

function HasHyperthreading: boolean;
var a,b,c,d: dword;
begin
  result:=false;
{$ifdef cpu64}
  asm
    push rax
    push rbx
    push rcx
    push rdx
    mov rax,0
    cpuid
    mov a,eax
    mov b,ebx
    mov c,ecx
    mov d,edx
    pop rdx
    pop rcx
    pop rbx
    pop rax
  end;
{$else}
  asm
    pushad
    mov eax,0
    cpuid
    mov a,eax
    mov b,ebx
    mov c,ecx
    mov d,edx
    popad
  end;
{$endif}

  if (b=$756e6547) and (d=$49656e69) and (c=$6c65746e) then
  begin
    //intel cpu
{$ifdef cpu64}
    asm
      push rax
      push rbx
      push rcx
      push rdx
      mov rax,1
      cpuid
      mov a,eax
      mov b,ebx
      mov c,ecx
      mov d,edx
      pop rdx
      pop rcx
      pop rbx
      pop rax
    end;
{$else}
    asm
      pushad
      mov eax,1
      cpuid
      mov a,eax
      mov b,ebx
      mov c,ecx
      mov d,edx
      popad
    end;
{$endif}

    if ((d shr 28) and 1)=1 then
      result:=true; //it has hyperthreading
  end;


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

        if form.top<0 then form.top:=0;
        if form.left<0 then form.left:=0;


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
    t: string;
    f: single;
    d: double;
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

      '(' :
      begin
        if copy(s,1,5)='(INT)' then
        begin
          t:=copy(s,6,length(s));
          val(t, k,j);
          if j=0 then
          begin
            result:='$'+inttohex(k,8);

            if s[1]='-' then
              result:='-'+result;

            if s[1]='+' then
              result:='+'+result;
              
            exit;
          end;
        end;

        if copy(s,1,8)='(DOUBLE)' then
        begin
          t:=copy(s,9,length(s));
          val(t, d,j);
          if j=0 then
          begin
            result:='$'+inttohex(PINT64(@d)^,8);

            if s[1]='-' then
              result:='-'+result;

            if s[1]='+' then
              result:='+'+result;
              
            exit;
          end;
        end;

        if copy(s,1,7)='(FLOAT)' then
        begin
          t:=copy(s,8,length(s));
          val(t, f,j);
          if j=0 then
          begin
            result:='$'+inttohex(pdword(@f)^,8);

            if s[1]='-' then
              result:='-'+result;

            if s[1]='+' then
              result:='+'+result;
              
            exit;
          end;
        end;
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
  else
  begin
    result:=ishex+copy(s,start,length(s));
  end;
end;

function HexStrToInt(const S: string): Integer;
begin
  result:=StrToint(ConvertHexStrToRealStr(s));
end;

function HexStrToInt64(const S: string): Int64;
begin
  result:=StrToint64(ConvertHexStrToRealStr(s));
end;

function isjumporcall(address: ptrUint; var addresstojumpto: ptrUint): boolean;
{
Gets the address jumped to if it is a jump or call.
Currently only called by the memory browser on a low frequency, so speed is of secondary concern
}
var buf: array [0..31] of byte;
    actualread: dword;
    i,j: integer;
    st: string;
    offset: dword;
    haserror: boolean;

    dis: TDisassembler;
begin
{$ifndef standalonetrainer}
  result:=false;

  dis:=TDisassembler.Create;
  dis.showmodules:=false;
  dis.showsymbols:=false;
  dis.dataOnly:=true;
  try
    dis.disassemble(address,st);
    if dis.LastDisassembleData.isjump then
    begin
      if dis.LastDisassembleData.modrmValueType=dvtAddress then
      begin
        addresstojumpto:=0;

        result:=ReadProcessMemory(processhandle, pointer(dis.LastDisassembleData.modrmValue),@addresstojumpto,processhandler.pointersize,actualread);
      end
      else
      if dis.LastDisassembleData.parameterValueType=dvtAddress then
      begin
        addresstojumpto:=dis.LastDisassembleData.parameterValue;
        result:=true;
      end;
    end;
      {

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

                addresstojumpto:=symhandler.getAddressFromName(st, false, haserror); //the pointer interpreter code can do this
                result:=not haserror;
              end;

            end;
          end;


        end;
      end;

    end;   }
  finally
    dis.free;
  end;
{$endif}

end;

function OldVarTypeToNewVarType(i: integer):TVariableType;
begin
  result:=vtDword;
  case i of
    0: result:=vtByte;
    1: result:=vtWord;
    2: result:=vtDword;
    3: result:=vtSingle;
    4: result:=vtDouble;
    5: result:=vtBinary;
    6: result:=vtQword;
    7: result:=vtString;
    8: result:=vtByteArray;
    9: result:=vtCustom;
    255: result:=vtAutoAssembler; //aa script
  end;
end;

function VariableTypeToString(variableType: TVariableType): string;
begin
  case variabletype of
    vtAll: result:='All';
    vtBinary: result:='Binary';
    vtByteArray: Result:='Array of byte';
    vtByte: result:='Byte';
    vtWord: Result:='2 Bytes';
    vtDword: Result:='4 Bytes';
    vtQword: Result:='8 Bytes';
    vtSingle: Result:='Float';
    vtDouble: Result:='Double';
    vtString: Result:='String';
    vtAutoAssembler: Result:='Auto Assembler Script';
  end;
end;

function StringToVariableType(s: string): TVariableType;
begin
  s:=trim(lowercase(s));
  if s='all' then result:=vtAll else
  if s='binary' then result :=vtBinary else
  if s='array of byte' then Result :=vtByteArray else
  if s='byte' then   result :=vtByte else
  if s='2 bytes' then  Result :=vtWord else
  if s='4 bytes' then Result :=vtDword else
  if s='8 bytes' then Result :=vtQword else
  if s='float' then   Result :=vtSingle else
  if s='double' then Result :=vtDouble else
  if s='string' then  Result :=vtString else
  if s='auto assembler script' then result:=vtAutoAssembler;
end;

function readAndParseAddress(address: ptrUint; variableType: TVariableType): string;
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

const HEAP_NO_SERIALIZE               =$00000001;
const HEAP_GROWABLE                   =$00000002;
const HEAP_GENERATE_EXCEPTIONS        =$00000004;
const HEAP_ZERO_MEMORY                =$00000008;
const HEAP_REALLOC_IN_PLACE_ONLY      =$00000010;
const HEAP_TAIL_CHECKING_ENABLED      =$00000020;
const HEAP_FREE_CHECKING_ENABLED      =$00000040;
const HEAP_DISABLE_COALESCE_ON_FREE   =$00000080;
const HEAP_CREATE_ALIGN_16            =$00010000;
const HEAP_CREATE_ENABLE_TRACING      =$00020000;
const HEAP_CREATE_ENABLE_EXECUTE      =$00040000;
const HEAP_MAXIMUM_TAG                =$0FFF;
const HEAP_PSEUDO_TAG_FLAG            =$8000;
const HEAP_TAG_SHIFT                  =18;

function heapflagstostring(heapflags: dword): string;
begin
  result:='';
  if (heapflags and HEAP_NO_SERIALIZE) > 0 then result:=result+'HEAP_NO_SERIALIZE+';
  if (heapflags and HEAP_GROWABLE) > 0 then result:=result+'HEAP_GROWABLE+';
  if (heapflags and HEAP_GENERATE_EXCEPTIONS) > 0 then result:='HEAP_GENERATE_EXCEPTIONS+';
  if (heapflags and HEAP_ZERO_MEMORY) > 0 then result:=result+'HEAP_ZERO_MEMORY+';
  if (heapflags and HEAP_REALLOC_IN_PLACE_ONLY) > 0 then result:=result+'HEAP_REALLOC_IN_PLACE_ONLY+';
  if (heapflags and HEAP_TAIL_CHECKING_ENABLED) > 0 then result:=result+'HEAP_TAIL_CHECKING_ENABLED+';
  if (heapflags and HEAP_FREE_CHECKING_ENABLED) > 0 then result:=result+'HEAP_FREE_CHECKING_ENABLED+';
  if (heapflags and HEAP_DISABLE_COALESCE_ON_FREE) > 0 then result:=result+'HEAP_DISABLE_COALESCE_ON_FREE+';
  if (heapflags and HEAP_CREATE_ALIGN_16) > 0 then result:=result+'HEAP_CREATE_ALIGN_16+';
  if (heapflags and HEAP_CREATE_ENABLE_TRACING) > 0 then result:=result+'HEAP_CREATE_ENABLE_TRACING+';
  if (heapflags and HEAP_CREATE_ENABLE_EXECUTE) > 0 then result:=result+'HEAP_CREATE_ENABLE_EXECUTE+';
  if (heapflags and HEAP_PSEUDO_TAG_FLAG) > 0 then result:=result+'HEAP_PSEUDO_TAG_FLAG+';


  if length(result)>0 then
    result:=Copy(result,1,length(result)-1)+'('+inttohex(heapflags,1)+')';
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


function isAddress(address: ptrUint):boolean;
var mbi: TMemoryBasicInformation;
begin
  result:=false;
  if VirtualQueryEx(processhandle, pointer(address), mbi, sizeof(mbi))>0 then
    result:=(mbi.State=MEM_COMMIT);// and (mbi.AllocationProtect<>PAGE_NOACCESS);
end;

function isExecutableAddress(address: ptrUint):boolean;
var mbi: TMemoryBasicInformation;
begin
  result:=false;
  if VirtualQueryEx(processhandle, pointer(address), mbi, sizeof(mbi))>0 then
    result:=(mbi.State=MEM_COMMIT) and (((mbi.Protect and PAGE_EXECUTE)=PAGE_EXECUTE) or ((mbi.Protect and PAGE_EXECUTE_READ)=PAGE_EXECUTE_READ) or ((mbi.Protect and PAGE_EXECUTE_READWRITE)=PAGE_EXECUTE_READWRITE) or ((mbi.Protect and PAGE_EXECUTE_WRITECOPY)=PAGE_EXECUTE_WRITECOPY) );
end;


function MinX(a, b: ptrUint): ptrUint;inline;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;


function MaxX(a, b: ptrUint): ptrUint;inline;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;


function InRangeX(const AValue, AMin, AMax: ptrUint): Boolean;inline;
begin
  Result:=(AValue>=AMin) and (AValue<=AMax);
end;


function FindFreeBlockForRegion(base: ptrUint; size: dword): pointer;
{
Query the memory arround base to find an empty block that is at least 'size' big
}
var
  mbi: MEMORY_BASIC_INFORMATION;
  x: ptrUint;
  offset: ptrUint;

  b: ptrUint;

  minAddress,maxAddress: ptrUint;
begin
  result:=nil;
 // if not processhandler.is64Bit then exit; //don't bother

  //64-bit

  if base=0 then exit;

  minAddress:=base-$70000000; //let's add in some extra overhead to skip the last fffffff
  maxAddress:=base+$70000000;


  if (minAddress>ptrUint(systeminfo.lpMaximumApplicationAddress)) or (minAddress<ptrUint(systeminfo.lpMinimumApplicationAddress)) then
    minAddress:=ptrUint(systeminfo.lpMinimumApplicationAddress);

  if (maxAddress<ptrUint(systeminfo.lpMinimumApplicationAddress)) or (maxAddress>ptrUint(systeminfo.lpMaximumApplicationAddress)) then
    maxAddress:=ptrUint(systeminfo.lpMaximumApplicationAddress);


  b:=minAddress;


  ZeroMemory(@mbi,sizeof(mbi));
  while VirtualQueryEx(processhandle,pointer(b),mbi,sizeof(mbi))=sizeof(mbi) do
  begin
    if mbi.BaseAddress>pointer(maxAddress) then exit; //no memory found, just return 0 and let windows decide

    if (mbi.State=MEM_FREE) and ((mbi.RegionSize)>size) then
    begin
      if (ptrUint(mbi.baseaddress) mod systeminfo.dwAllocationGranularity)>0 then
      begin
        //the whole size can not be used
        x:=ptrUint(mbi.baseaddress);
        offset:=systeminfo.dwAllocationGranularity - (x mod systeminfo.dwAllocationGranularity);

        //check if there's enough left
        if (mbi.regionsize-offset)>size then
        begin
          //yes
          x:=x+offset;

          if x<base then
          begin
            x:=x+(mbi.regionsize-offset)-size;
            if x>base then x:=base;

            //now decrease x till it's alligned properly
            x:=x-(x mod systeminfo.dwAllocationGranularity);
          end;

          //if the difference is closer then use that
          if abs(ptrInt(x-base))<abs(ptrInt(ptrUint(result)-base)) then
            result:=pointer(x);
        end;
        //nope

      end
      else
      begin
        x:=ptrUint(mbi.BaseAddress);
        if x<base then //try to get it the closest possible (so to the end of the region-size and aligned by dwAllocationGranularity)
        begin
          x:=(x+mbi.RegionSize)-size;
          if x>base then x:=base;

          //now decrease x till it's alligned properly
          x:=x-(x mod systeminfo.dwAllocationGranularity);
        end;


        if abs(ptrInt(x-base))<abs(ptrInt(ptrUint(result)-base)) then
          result:=pointer(x);
      end;

    end;
    b:=ptrUint(mbi.BaseAddress)+mbi.RegionSize;
  end;

end;

initialization
  getmem(tempdir,256);
  GetTempPath(256,tempdir);
  GetWindir;
  keysfilemapping:=0;
  keys:=nil;

  setlength(windowlist,0);
  setlength(donthidelist,0);
  allwindowsareback:=true;
  stealthhook:=0;
  iswin2kplus:=GetSystemType>=5;

  processhandler:=TProcessHandler.create;
  GetSystemInfo(@systeminfo);


finalization

  if tempdir<>nil then
    freemem(tempdir);

end.




