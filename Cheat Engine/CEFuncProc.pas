// Copyright Cheat Engine. All Rights Reserved.


unit CEFuncProc;

{$MODE Delphi}

//This version of CEFuncProc has been COPIED to the server dir
//Cheat Engine regular WONT look at this

interface

uses
  {$ifdef darwin}
   mactypes, LCLType,macport,
  {$endif}
  {$ifdef windows}
   jwawindows, windows,
  {$endif}
  zstream, LazUTF8, LCLIntf,StdCtrls,Classes,SysUtils,dialogs,{tlhelp32,}forms,messages,
Graphics,
ComCtrls,
{reinit, }
Assemblerunit,
  {$ifdef windows}imagehlp,{$endif}
registry,
ExtCtrls,
LastDisassembleData,

{$ifdef netclient}
netapis,
{$else}
NewKernelHandler,
{$ifndef standalonetrainer}
{$ifndef netserver}
hypermode,
{$endif}
{$endif}
{$endif}
 math,syncobjs, {$ifdef windows}shellapi,{$endif} ProcessHandlerUnit, controls, {$ifdef windows}shlobj, ActiveX,{$endif} strutils,
commontypedefs, {$ifdef windows}Win32Int,{$endif} maps, lua, lualib, lauxlib{$ifdef darwin},macportdefines{$endif}, betterControls;


const
  EFLAGS_CF=(1 shl 0);
  EFLAGS_PF=(1 shl 2);
  EFLAGS_AF=(1 shl 4);
  EFLAGS_ZF=(1 shl 6);
  EFLAGS_SF=(1 shl 7);
  EFLAGS_TF=(1 shl 8);
  EFLAGS_IF=(1 shl 9);
  EFLAGS_DF=(1 shl 10);
  EFLAGS_OF=(1 shl 11);
  EFLAGS_NT=(1 shl 14);
  EFLAGS_RF=(1 shl 16);
  EFLAGS_VM=(1 shl 17);
  EFLAGS_AC=(1 shl 18);
  EFLAGS_ID=(1 shl 21);




//function NewVarTypeToOldVarType(i: TVariableType):integer;
function VariableTypeToTranslatedString(variableType: TVariableType): string;
function OldVarTypeToNewVarType(i: integer):TVariableType;
function VariableTypeToString(variableType: TVariableType): string;
function StringToVariableType(s: string): TVariableType;

function isjumporcall(address: ptrUint; var addresstojumpto: ptrUint): boolean;
{
procedure quicksortmemoryregions(lo,hi: integer);     //obsolete
}

function rewritecode(processhandle: thandle; address:ptrUint; buffer: pointer; var size:dword; force: boolean=false): boolean;
function rewritedata(processhandle: thandle; address:ptrUint; buffer: pointer; var size:dword): boolean;

function GetUserNameFromPID(ProcessId: DWORD): string;
//procedure GetProcessList(ProcessList: TListBox; NoPID: boolean=false); overload;
//procedure GetProcessList(ProcessList: TStrings; NoPID: boolean=false; noProcessInfo: boolean=false);  overload;
procedure GetThreadList(threadlist: TStrings);
//procedure cleanProcessList(processlist: TStrings);
procedure GetWindowList2(ProcessList: TStrings; showInvisible: boolean=true);
procedure GetWindowList(ProcessList: TStrings; showInvisible: boolean=true); overload;
procedure GetWindowList(ProcessListBox: TListBox; showInvisible: boolean=true); overload;
procedure GetModuleList(ModuleList: TStrings; withSystemModules: boolean);
procedure cleanModuleList(ModuleList: TStrings);

function AvailMem:SIZE_T;
function isreadable(address:ptrUint):boolean;
function iswritable(address:ptrUint):boolean;

procedure RemoveAddress(address: Dword;bit: Byte; vartype: Integer);

function GetCEdir:string;
procedure Open_Process;
Procedure Shutdown;
function KeyToStr(key:word):string;

procedure EnableWindowsSymbols(warn: boolean=true);

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


function GetPageBase(address: ptruint): ptruint; inline; //return the pageboundary this address belongs to

function ByteStringToText(s: string;hex: boolean):string;
function ByteStringToDouble(s: string;hex: boolean):double;
function ByteStringToSingle(s: string;hex: boolean):single;
function ByteStringToInt(s: string;hex: boolean):int64;
function VarToBytes(v: pointer; size: integer): string;
function RawToString(const buf: array of byte; vartype: integer;showashex: boolean; bufsize: integer):string;

procedure decimal(var key: char);
procedure hexadecimal(var key: char);

function GetSystemType: Integer;


procedure ToggleOtherWindows;

Procedure InjectDll(dllname: string; functiontocall: string='');
Function GetRelativeFilePath(filename: string):string;

function GetCPUCount: integer;
function HasHyperthreading: boolean;
procedure SaveFormPosition(form: TCustomform; const extra: array of integer); overload;
procedure SaveFormPosition(form: TCustomform); overload;
function LoadFormPosition(form: TCustomform; var x: TWindowPosArray):boolean; overload;
function LoadFormPosition(form: TCustomform):boolean; overload;

function heapflagstostring(heapflags: dword): string;
function allocationtypetostring(alloctype: dword): string;
function allocationProtectToString(protect: dword): string;

function AllocationProtectToAccessRights(protect: dword): TAccessRights;
function AccessRightsToAllocationProtect(ar: TAccessRights): Dword;

function freetypetostring(freetype: dword):string;


function MinX(a, b: ptrUint): ptrUint;inline; overload; //fpc2.4.1 has no support for unsigned
function MaxX(a, b: ptrUint): ptrUint;inline; overload;


function InRangeX(const AValue, AMin, AMax: ptrUint): Boolean;inline;
function InRangeQ(const AValue, AMin, AMax: qword): Boolean;inline;



function getProcessnameFromProcessID(pid: dword): string;
function getProcessPathFromProcessID(pid: dword): string;
procedure getDriverList(list: tstrings);

function EscapeStringForRegEx(const S: string): string;

function getthreadCount(pid: qword): integer;

function GetStackStart(threadnr: integer=0): ptruint;
function getDiskFreeFromPath(path: string): int64;
procedure protectme(pid: dword=0);

procedure errorbeep;

{$ifndef net}
procedure SetLanguage;
function getathreadid(processid:dword):dword;

{$endif}

procedure DetachIfPossible;


{$ifdef windows}
procedure Log(s: string);
{$endif}


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



  PAGE_WRITECOMBINE=$400;

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
  disablewhenreleased: boolean;
  keycombo: TKeyCombo;
end;







function bintohexs(var buf; size: integer):string;
function ConvertKeyComboToString(x: tkeycombo):string;

{
ProcessID and ProcessHandle as functions untill all code has been converted to
make use of ProcessHandlerUnit
}
//function ProcessID: dword;
//function ProcessHandle: THandle;

//Global vars:


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



uses disassembler,CEDebugger,debughelper, symbolhandler, symbolhandlerstructs,
     frmProcessWatcherUnit, KernelDebugger, formsettingsunit, MemoryBrowserFormUnit,
     savedscanhandler, networkInterface, networkInterfaceApi, vartypestrings,
     processlist, Parsers, Globals, xinput, luahandler, LuaClass, LuaObject,
     UnexpectedExceptionsHelper, LazFileUtils, autoassembler, Clipbrd, mainunit2, cpuidUnit;


resourcestring
  rsNotSupportedInThisVersion = 'not supported in this version';
  rsNotConvertable = 'Not convertable';
  rsLeftMB = 'Left MB';
  rsMiddleMB = 'Middle MB';
  rsRightMB = 'Right MB';
  rsBreak = 'Break';
  rsBackspace = 'Backspace';
  rsShift = 'Shift';
  rsCtrl = 'Ctrl';
  rsAlt = 'Alt';
  rsTab = 'Tab';
  rsClear = 'Clear';
  rsEnter = 'Enter';
  rsPause = 'Pause';
  rsCapsLock = 'Caps Lock';
  rsEsc = 'Esc';
  rsSpaceBar = 'Space bar';
  rsPageUp = 'Page Up';
  rsPageDown = 'Page Down';
  rsEnd = 'End';
  rsHome = 'Home';
  rsLeftArrow = 'Left Arrow';
  rsUpArrow = 'Up Arrow';
  rsRightArrow = 'Right Arrow';
  rsDownArrow = 'Down Arrow';
  rsSelect = 'Select';
  rsPrint = 'Print';
  rsExecute = 'Execute';
  rsPrintScreen = 'Print Screen';
  rsInsert = 'Insert';
  rsDeleteKey = 'Delete '; //added a space so the translator will leave it alone for th delete line
  rsHelp = 'Help';
  rsLeftWindowsKey = 'Left Windows key';
  rsRightWindowsKey = 'Right Windows key';
  rsApplicationsKey = 'Applications key';
  rsNumeric = 'numeric';
  rsNumLock = 'Num Lock';
  rsScrollLock = 'Scroll Lock';
  rsGetProcAddressNotFound = 'GetProcAddress not found';
  rsLoadLibraryANotFound = 'LoadLibraryA not found';
  rsFailedToAllocateMemory = 'Failed to allocate memory';
  rsFailedToInjectTheDllLoader = 'Failed to inject the dll loader';
  rsFailedToExecuteTheDllLoader = 'Failed to execute the dll loader';
  rsTheInjectionThreadTookLongerThan10SecondsToExecute = 'The injection thread took longer than 10 seconds to execute. Injection routine not freed';
  rsFailedInjectingTheDLL = 'Failed injecting the DLL';
  rsFailedExecutingTheFunctionOfTheDll = 'Failed executing the function of the dll';
  rsUnknownErrorDuringInjection = 'Unknown error during injection';
  rsICanTGetTheProcessListYouArePropablyUsingWindowsNT = 'I can''t get the process list. You are propably using windows NT. Use the window list instead!';
  rsNoKernel32DllLoaded = 'No kernel32.dll loaded';
  rsSeparator = 'Separator';
  rsCEFPDllInjectionFailedSymbolLookupError = 'Dll injection failed: symbol lookup error';
  rsCEFPICantGetTheProcessListYouArePropablyUseinWindowsNtEtc = 'I can''t get the process list. You are propably using windows NT. Use the window list instead!';
  rsPosition = ' Position';
  rsThisCanTakeSomeTime = 'This can take some time if you are missing the '
    +'PDB''s and CE will look frozen. Are you sure?';

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
  i: integer;
begin
  i:=VirtualQueryEx(processhandle,pointer(address),mbi,sizeof(mbi));
  result:=(i=sizeof(mbi)) and (mbi.State=mem_commit);
end;

function iswritable(address:ptrUint):boolean;
var mbi: _MEMORY_BASIC_INFORMATION;
  i: integer;
begin
  i:=VirtualQueryEx(processhandle,pointer(address),mbi,sizeof(mbi));
  result:=(i=sizeof(mbi)) and (mbi.State=mem_commit);
  if result then result:={$ifdef windows}
                         ((mbi.Protect and PAGE_EXECUTE_READWRITE)=PAGE_EXECUTE_READWRITE) or
                         ((mbi.Protect and PAGE_EXECUTE_WRITECOPY)=PAGE_EXECUTE_WRITECOPY) or
  {$endif}
                         ((mbi.Protect and PAGE_READWRITE)=PAGE_READWRITE);
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
      freememandnil(x);
    end;

    8: //array of bytes
    begin
      result:='';
      for i:=0 to bufsize-1 do
        result:=result+'-'+inttohex(buf[bufsize],2);
    end;

    else result:=rsNotSupportedInThisVersion;
  end;
  except
    result:=rsNotConvertable;
  end;
end;

function bintohexs(var buf; size: integer): string;
var hs: pchar;
begin
  getmem(hs,size*2+1);
  BinToHex(@buf,hs,size);
  hs[size*2]:=#0;
  result:=hs;

  freemem(hs);
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
        vk_lbutton: newstr:=rsLeftMB;
        vk_mbutton: newstr:=rsMiddleMB;
        vk_rbutton: newstr:=rsRightMB;
        VK_XBUTTON1: newstr:='MB 4';
        VK_XBUTTON2: newstr:='MB 5';
        VK_CANCEL: newstr:=rsBreak;
        VK_BACK	: newstr:=rsBackspace;
        VK_SHIFT: newstr:=rsShift;
        VK_CONTROL: newstr:=rsCtrl;
        VK_MENU: newstr:=rsAlt;
        VK_TAB	: newstr:=rsTab;
        VK_CLEAR	: newstr:=rsClear;
        VK_RETURN	: newstr:=rsEnter;
        VK_PAUSE	: newstr:=rsPause;
        VK_CAPITAL	: newstr:=rsCapsLock;
        VK_ESCAPE	: newstr:=rsEsc;
        VK_SPACE	: newstr:=rsSpaceBar;
        VK_PRIOR	: newstr:=rsPageUp;
        VK_NEXT	: newstr:=rsPageDown;
        VK_END	: newstr:=rsEnd;
        VK_HOME	: newstr:=rsHome;
        VK_LEFT	: newstr:=rsLeftArrow;
        VK_UP	: newstr:=rsUpArrow;
        VK_RIGHT	: newstr:=rsRightArrow;
        VK_DOWN	: newstr:=rsDownArrow;
        VK_SELECT	: newstr:=rsSelect;
        VK_PRINT	: newstr:=rsPrint;
        VK_EXECUTE	: newstr:=rsExecute;
        VK_SNAPSHOT	: newstr:=rsPrintScreen;
        VK_INSERT	: newstr:=rsInsert;
        VK_DELETE	: newstr:=rsDeleteKey;
        VK_HELP	: newstr:=rsHelp;
        VK_LWIN	: newstr:=rsLeftWindowsKey;
        VK_RWIN	: newstr:=rsRightWindowsKey;
        VK_APPS	: newstr:=rsApplicationsKey;
        VK_NUMPAD0	: newstr:=rsNumeric+' 0';
        VK_NUMPAD1	: newstr:=rsNumeric+' 1';
        VK_NUMPAD2	: newstr:=rsNumeric+' 2';
        VK_NUMPAD3	: newstr:=rsNumeric+' 3';
        VK_NUMPAD4	: newstr:=rsNumeric+' 4';
        VK_NUMPAD5	: newstr:=rsNumeric+' 5';
        VK_NUMPAD6	: newstr:=rsNumeric+' 6';
        VK_NUMPAD7	: newstr:=rsNumeric+' 7';
        VK_NUMPAD8	: newstr:=rsNumeric+' 8';
        VK_NUMPAD9	: newstr:=rsNumeric+' 9';
        VK_MULTIPLY	: newstr:=rsNumeric+' *';
        VK_ADD	: newstr:=rsNumeric+' +';
        VK_SEPARATOR : newstr:=rsNumeric+' Separator';
        VK_SUBTRACT	: newstr:=rsNumeric+' -';
        VK_DECIMAL	: newstr:=rsNumeric+' .';
        VK_DIVIDE	: newstr:=rsNumeric+' /';
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
        VK_NUMLOCK	: newstr:=rsNumLock;
        VK_SCROLL	: newstr:=rsScrollLock;

        VK_OEM_PLUS : newstr:='=';
        VK_OEM_MINUS : newstr:='-';
        VK_OEM_PERIOD : newstr:='.';
        VK_OEM_COMMA : newstr:=',';
        VK_OEM_1 : newstr:=';';
        VK_OEM_2 : newstr:='/';
        VK_OEM_3 : newstr:='`';
        VK_OEM_4 : newstr:='[';
        VK_OEM_5 : newstr:='\';
        VK_OEM_6 : newstr:=']';
        VK_OEM_7 : newstr:='''';

        {$ifdef windows}
        VK_PAD_A : newstr:='[A]';
        VK_PAD_B : newstr:='[B]';
        VK_PAD_X : newstr:='[X]';
        VK_PAD_Y : newstr:='[Y]';
        VK_PAD_RSHOULDER : newstr:='[Right Shoulder]';
        VK_PAD_LSHOULDER : newstr:='[Left Shoulder]';
        VK_PAD_LTRIGGER : newstr:='[Left Trigger]';
        VK_PAD_RTRIGGER : newstr:='[Right Trigger]';
        VK_PAD_DPAD_UP : newstr:='[Up]';
        VK_PAD_DPAD_DOWN : newstr:='[Down]';
        VK_PAD_DPAD_LEFT : newstr:='[Left]';
        VK_PAD_DPAD_RIGHT : newstr:='[Right]';
        VK_PAD_START : newstr:='[Start]';
        VK_PAD_BACK : newstr:='[Back]';
        VK_PAD_LTHUMB_PRESS : newstr:='[Left Thumbstick]';
        VK_PAD_RTHUMB_PRESS : newstr:='[Right Thumbstick]';
        VK_PAD_LTHUMB_UP : newstr:='[Left: Up]';
        VK_PAD_LTHUMB_DOWN : newstr:='[Left: Down]';
        VK_PAD_LTHUMB_RIGHT : newstr:='[Left: Right]';
        VK_PAD_LTHUMB_LEFT : newstr:='[Left: Left]';
        VK_PAD_LTHUMB_UPLEFT : newstr:='[Left: Up Left]';
        VK_PAD_LTHUMB_UPRIGHT : newstr:='[Left: Up Right]';
        VK_PAD_LTHUMB_DOWNRIGHT : newstr:='[Left: Down Right]';
        VK_PAD_LTHUMB_DOWNLEFT : newstr:='[Left: Down Left]';
        VK_PAD_RTHUMB_UP : newstr:='[Right: Up]';
        VK_PAD_RTHUMB_DOWN : newstr:='[Right: Down]';
        VK_PAD_RTHUMB_RIGHT : newstr:='[Right: Right]';
        VK_PAD_RTHUMB_LEFT : newstr:='[Right: Left]';
        VK_PAD_RTHUMB_UPLEFT : newstr:='[Right: Up Left]';
        VK_PAD_RTHUMB_UPRIGHT : newstr:='[Right: Up Right]';
        VK_PAD_RTHUMB_DOWNRIGHT : newstr:='[Right: Down Right]';
        VK_PAD_RTHUMB_DOWNLEFT : newstr:='[Right: Down Left]';
         {$endif}
        48..57      : newstr:=chr(x[i]);
        65..90      : newstr:=chr(x[i]);
        else  newstr:='#'+inttostr(x[i]);
      end;

      result:=result+newstr+'+';
    end;

  result:=copy(result,1,length(result)-1);
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

//Returns a random threadid owned by the target process
function getathreadid(processid:dword):dword;
var i: integer;
    ths: thandle;
    tE: threadentry32;
begin
  {$ifdef windows}
  if frmProcessWatcher<>nil then
  begin
    //first find a processid using the processwatcher

    frmProcessWatcher.processesCS.Enter;
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
      frmProcessWatcher.processesCS.Leave;
    end;

  end;
  {$endif}

  //no exit yet, so use a enumeration of all threads and this processid

  ths:=CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD,processid);
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

  memorybrowser.showDebugPanels:=false;
end;


procedure ForceLoadModule(dllname: string; functiontocall: string=''; callreason: string='');
var
  s: string;
  functionloc: ptruint;
  tid: dword;
  e: boolean;
begin
  try
    lua_getglobal(Luavm, 'loadModule');
    lua_pushstring(Luavm,dllname);
    lua_pushboolean(LuaVM,true);
    lua_pushinteger(LuaVM,10000); //timeout of 10 secs

    if (lua_pcall(Luavm,3,2,0)<>0) then
      raise exception.create('didn''t even run');

    if lua_isnil(Luavm,-2) then
    begin
      s:=Lua_ToString(Luavm,-1);
      lua_pop(Luavm,2);
      raise exception.create(s);
    end
    else
    begin
      lua_pop(Luavm,2);

      if functiontocall<>'' then
      begin
        functionloc:=symhandler.getAddressFromName(functiontocall, false, e);
        if not e then
          CreateRemoteThread(processhandle,nil,0,pointer(functionloc),nil,0,tid);;
      end;
    end;
  except
    on e:exception do
    begin
      s:='Force load module failed:'+e.message;
      if callreason<>'' then
        raise exception.create(callreason+#13#10+s)
      else
        raise exception.create(s);
    end;
  end;
end;

{$ifdef darwin}
Procedure InjectDll(dllname: string; functiontocall: string='');
var s: tstringlist;
    di: TDisableInfo;
    //allocs: TCEAllocArray;
    injector: qword;
    returnvalue: qword;
    i: integer;
    x: ptruint;
    r: dword;

    erroraddress: qword;
    a: qword;
    errs: pchar;

    errorstring: string;
    tid: dword;
    //el: TCEExceptionListArray;
begin
  outputdebugstring('cefuncproc.InjectDLL('''+dllname+''','''+functiontocall+''')');
  if MacIsArm64 then
  begin
    raise exception.create('module injection is not yet supported on m1');
  end
  else
  begin

    s:=tstringlist.create;
    s.add('[enable]');
    s.add('registersymbol(v1)');
    s.add('registersymbol(v2)');
    s.add('registersymbol(v3)');
    s.add('registersymbol(injector)');
    s.add('registersymbol(errorstr)');
    if processhandler.is64bit then
    begin
      s.add('alloc(v1, 8)');
      s.add('alloc(v2, 8)');
      s.add('alloc(v3, 8)');
      s.add('alloc(errorstr, 8)');
    end
    else
    begin
      s.add('alloc(v1, 4)');
      s.add('alloc(v2, 4)');
      s.add('alloc(v3, 4)');
      s.add('alloc(errorstr, 4)');
    end;

    s.add('alloc(injector,512)');
    s.add('alloc(returnvalue, 4)');
    s.add('label(dllname)');
    s.add('label(error)');
    s.add('label(cleanup)');
    s.add('');
    s.add('injector:');
    if processhandler.is64bit then
    begin
      //rsp=*8
      s.add('mov rax,v1');
      s.add('mov [rax],rsp');
      s.add('push rbp');

      //rsp=*0
      s.add('mov rax,v2');
      s.add('mov [rax],rsp');
    end
    else
    begin
      //esp=*c
      s.add('mov [v1],esp');
      s.add('push ebp');
      //esp=*8
      s.add('mov [v2],esp');
    end;

    if processhandler.is64Bit then
    begin
      s.add('mov rdi,dllname');
      s.add('mov rsi,1');
    end
    else
    begin
      s.add('push 1'); //rtld lazy
      //esp=*4
      s.add('push dllname');
      //esp=*0
    end;

    //64-bit: rsp=*0
    //32-bit: esp=*0

    if processhandler.is64Bit then
    begin
      s.add('mov rax,v3');
      s.add('mov [rax],rsp');
    end
    else
      s.add('mov [v3],esp');

    s.add('call dlopen');
    //s.add('xor eax,eax');

    s.add('cmp eax,0');
    s.add('je short error');

    if processhandler.is64Bit then
    begin
      s.add('mov rax,returnvalue');
      s.add('mov dword [rax],1');
      s.adD('jmp short cleanup');
      s.add('error:');
      s.add('mov rax,returnvalue');
      s.add('mov dword [rax],2');
      s.add('call dlerror');
      s.add('mov rsi,errorstr');
      s.add('mov [rsi],rax');
    end
    else
    begin
      s.add('mov dword [returnvalue],1');
      s.adD('jmp short cleanup');
      s.add('error:');
      s.add('mov dword [returnvalue],2');
      s.add('call dlerror');
      s.add('mov [errorstr],eax');
    end;
    s.add('cleanup:');

    if processhandler.is64Bit then
    begin
      s.add('pop rbp');
    end
    else
    begin
      s.add('add esp,8');  //dlopen is a cdecl  (64-bit has no pushed params)
      s.add('pop ebp');
    end;


    s.add('ret');
    s.add('');
    s.add('dllname:');
    s.add('db '''+dllname+''',0');
    s.add('');
    s.add('returnvalue:');
    s.add('dd 0');
    s.add('');
    s.add('[disable]');
    s.add('dealloc(injector)');
    s.add('dealloc(returnvalue)');
  end;
  //clipboard.AsText:=s.Text;

 // raise exception.create('copy to clipboard now');


  di:=TDisableInfo.create;
  //setlength(allocs,0);
  if autoassemble(s,false, true, false, false, di) then
  begin
    injector:=0;
    returnvalue:=0;
    for i:=0 to length(di.allocs)-1 do
      if di.allocs[i].varname='injector' then
        injector:=di.allocs[i].address
      else
      if di.allocs[i].varname='returnvalue' then
        returnvalue:=di.allocs[i].address
      else
      if di.allocs[i].varname='errorstr' then
        erroraddress:=di.allocs[i].address;

    //showmessage('injector='+inttohex(injector,8));

    if (injector=0) or (returnvalue=0) then
      raise exception.create('The dllloader script didn''t properly get injected');

    if CreateRemoteThread(processhandle, nil, 0, pointer(injector),0, 0,tid)=0 then raise exception.Create('Creating the injector thread has failed');

    r:=0;
    i:=10000 div 50;

    while r=0 do
    begin
      dec(i);
      if i=0 then raise exception.create('Timeout on dll inject');

      if readprocessmemory(processhandle, pointer(returnvalue), @r, 4, x)=false then
        raise exception.create('The process has crashed');

      if GetCurrentThreadID = MainThreadID then
        CheckSynchronize; //handle sychronize calls while it's waiting

      if r=0 then sleep(50);



    end;

    outputdebugstring('library injection code executed successful');


    //finally free the injector
    autoassemble(s, false, false, false, false, di);   //disable

    if r=2 then
    begin
      a:=0;
      errorstring:='(Unknown reason)';
      if readprocessmemory(processhandle, pointer(erroraddress), @a, processhandler.pointersize, x) then
      begin
        getmem(errs,256);
        if readprocessmemory(processhandle, pointer(a), errs, 255,x) then
        begin
          errs[255]:=#0;
          errorstring:=errs;
        end;
        freemem(errs);

      end;
      raise exception.create('The dll injection failed in the dlopen part:'+errorstring);

    end;
  end else raise exception.create('injecting the dllloader script failed');

  s.free;

  outputdebugstring('cefuncproc.InjectDll made it to the end');
end;

{$endif}

{$ifdef windows}


type
  EInjectDLLFunctionFailure=class(Exception);
  EInjectError=class(Exception);

Procedure InjectDll(dllname: string; functiontocall: string='');
var LoadLibraryPtr: pointer;
    GetProcAddressPtr: Pointer;


    h: Thandle;

    inject: array [0..4095] of byte;
    x:PtrUInt;

    tid: dword;
    res: dword;

    outp:TAssemblerBytes;
    counter: integer;
    position,position2: ptrUint;

    dllLocation: string;
    startaddresS: ptrUint;
    functionloc: ptrUint;
    injectionlocation: pointer;
    threadhandle: thandle;

    c: TCEConnection;

    ml: TModuleLoader;

    errs: string;
    serr: boolean;
begin
  if alwaysforceload then
  begin
    forceLoadModule(dllname,functiontocall);
    exit;
  end;

  try
    c:=getConnection;
    if (c<>nil) and (c.isNetworkHandle(processhandle)) then //network loadModule
    begin
      if c.loadModule(processhandle, dllname)=false then
        raise exception.create('Failed to load '+dllname);
    end
    else
    begin
      //todo: Change this to a full AA script (but make sure not to call injectdll in there :)  )
      h:=LoadLibrary('Kernel32.dll');
      if h=0 then raise exception.Create(rsNoKernel32DllLoaded);

      LoadLibraryPtr:=nil;
      GetProcAddressPtr:=nil;
      injectionlocation:=nil;

      try
        try
          getprocaddressptr:=pointer(symhandler.getAddressFromName('Kernel32!GetProcAddress',true));
        except
  {$ifdef cpu64}
          if not processhandler.is64Bit then
            raise exception.create(rsCEFPDllInjectionFailedSymbolLookupError);
  {$endif}
          GetProcAddressPtr:=GetProcAddress(h,'GetProcAddress');
        end;

        if getprocaddressptr=nil then raise exception.Create(rsGetProcAddressNotFound);

        try
          LoadLibraryPtr:=pointer(symhandler.getAddressFromName('Kernel32!LoadLibraryA',true));
        except
          //failed getting the address of LoadLibraryA, use old method
          {$ifdef cpu64}
            if not processhandler.is64Bit then
              raise exception.create(rsCEFPDllInjectionFailedSymbolLookupError);
          {$endif}
          LoadLibraryPtr:=GetProcAddress(h,'LoadLibraryA');
        end;


        if LoadLibraryptr=nil then raise exception.Create(rsLoadLibraryANotFound);

        injectionlocation:=VirtualAllocEx(processhandle,nil,4096,MEM_RESERVE or MEM_COMMIT,PAGE_EXECUTE_READWRITE);

        if allocsAddToUnexpectedExceptionList then
          AddUnexpectedExceptionRegion(ptruint(injectionlocation),4096);


        if injectionlocation=nil then raise exception.Create(rsFailedToAllocateMemory);

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
          //at entry stack is unaligned (has an 8 byte return value, so sub rsp,8 to set alignment. After that, just the usual)
          //loadlibrary(cehook);
          assemble('SUB RSP,#40',position,outp);
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
          assemble('ADD RSP,#40',position,outp);
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
            assemble('SUB RSP,#40',position,outp);
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
            assemble('ADD RSP,#40',position,outp);
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
            assemble('SUB RSP,#40',position,outp);
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
            assemble('ADD RSP,#40',position,outp);
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

        if not writeprocessmemory(processhandle, injectionlocation, @inject[0], position2, x) then raise exception.Create(rsFailedToInjectTheDllLoader);


        if useapctoinjectdll then
        begin
          //suspend , message, resume is needed to prevent a crash when it is in a message loop
          //ntsuspendprocess(processhandle);
          x:=getathreadid(processid);
          PostThreadMessage(x,wm_paint,0,0);
          CreateRemoteAPC(x,pointer(startaddress));
          PostThreadMessage(x,wm_paint,0,0);
         // ntresumeprocess(processhandle);

          sleep(1000);
        end
        else
        begin
          threadhandle:=createremotethread(processhandle,nil,0,pointer(startaddress),nil,0,tid);
          if threadhandle=0 then raise exception.Create(rsFailedToExecuteTheDllLoader);

          counter:=10000 div 10;
          while (waitforsingleobject(threadhandle,10)=WAIT_TIMEOUT) and (counter>0) do
          begin
            if GetCurrentThreadID = MainThreadID then
              CheckSynchronize; //handle sychronize calls while it's waiting

            dec(counter);
          end;

          try
            if (counter=0) then
              raise exception.Create(rsTheInjectionThreadTookLongerThan10SecondsToExecute);

            if getexitcodethread(threadhandle,res) then
            begin
              case res of
                1: ;//success
                2: raise EInjectError.Create(utf8toansi(rsFailedInjectingTheDLL));
                3: raise EInjectDLLFunctionFailure.Create(utf8toansi(rsFailedExecutingTheFunctionOfTheDll));
                else raise EInjectError.Create(utf8toansi(rsUnknownErrorDuringInjection));
              end;
            end
            else
            begin
              OutputDebugString('failure to get the exitcode of the thread.'+inttostr(GetLastError));
            end; //else unsure, did it work or not , or is it crashing?


          finally
            closehandle(threadhandle);
          end;
        end;
      finally
        FreeLibrary(h);

        if injectionlocation<>nil then
        begin
          virtualfreeex(processhandle,injectionlocation,0,MEM_RELEASE);
          RemoveUnexpectedExceptionRegion(ptruint(injectionlocation),0);
        end;
      end;

    end;
  except
    on e:EInjectError do
    begin
      forceLoadModule(dllname, functiontocall, 'dllInject failed: '+e.message);
    end;
  end;
end;
{$endif}


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
{$ifdef windows}
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
  {$else}
  application.BringToFront;
  {$endif}
end;

var cachedSystemType: integer=-1;
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

{$IFDEF windows}
var
 osVerInfo : TOSVersionInfo;
 majorVer, minorVer : Integer;
{$ENDIF}

begin
 {$IFDEF windows}
 if cachedSystemType<>-1 then
   exit(cachedSystemType);

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
  {$else}

  result:=cOsUnknown;

 {$ENDIF}
  cachedSystemType:=result;
end;



function KeyToStr(key:word):string;
begin
  case key of
    VK_BACK	: result:=rsBackspace;
    VK_TAB	: result:=rsTab;
    VK_CLEAR	: result:=rsClear;
    VK_RETURN	: result:=rsEnter;
    VK_PAUSE	: result:=rsPause;
    VK_CAPITAL	: result:=rsCapsLock;
    VK_ESCAPE	: result:=rsEsc;
    VK_SPACE	: result:=rsSpaceBar;
    VK_PRIOR	: result:=rsPageUp;
    VK_NEXT	: result:=rsPageDown;
    VK_END	: result:=rsEnd;
    VK_HOME	: result:=rsHome;
    VK_LEFT	: result:=rsLeftArrow;
    VK_UP	: result:=rsUpArrow;
    VK_RIGHT	: result:=rsRightArrow;
    VK_DOWN	: result:=rsDownArrow;
    VK_SELECT	: result:=rsSelect;
    VK_PRINT	: result:=rsPrint;
    VK_EXECUTE	: result:=rsExecute;
    VK_SNAPSHOT	: result:=rsPrintScreen;
    VK_INSERT	: result:=rsInsert;
    VK_DELETE	: result:=rsDeleteKey;
    VK_HELP	: result:=rsHelp;
    VK_LWIN	: result:=rsLeftWindowsKey;
    VK_RWIN	: result:=rsRightWindowsKey;
    VK_APPS	: result:=rsApplicationsKey;
    VK_NUMPAD0	: result:=rsNumeric+' 0';
    VK_NUMPAD1	: result:=rsNumeric+' 1';
    VK_NUMPAD2	: result:=rsNumeric+' 2';
    VK_NUMPAD3	: result:=rsNumeric+' 3';
    VK_NUMPAD4	: result:=rsNumeric+' 4';
    VK_NUMPAD5	: result:=rsNumeric+' 5';
    VK_NUMPAD6	: result:=rsNumeric+' 6';
    VK_NUMPAD7	: result:=rsNumeric+' 7';
    VK_NUMPAD8	: result:=rsNumeric+' 8';
    VK_NUMPAD9	: result:=rsNumeric+' 9';
    VK_MULTIPLY	: result:=rsNumeric+' *';
    VK_ADD	: result:=rsNumeric+' +';
    VK_SEPARATOR : result:=rsNumeric+' '+rsSeparator;
    VK_SUBTRACT	: result:=rsNumeric+' -';
    VK_DECIMAL	: result:=rsNumeric+' .';
    VK_DIVIDE	: result:=rsNumeric+' /';
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
    VK_NUMLOCK	: result:=rsNumLock;
    VK_SCROLL	: result:=rsScrollLock;
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


function GetPageBase(address: ptruint): ptruint; inline; //return the pageboundary this address belongs to
begin
  result:=address and qword($fffffffffffff000);
end;

function AvailMem:SIZE_T;
{$IFDEF windows}
var x: _MEMORYSTATUS;
{$ENDIF}
begin
  {$IFDEF windows}
  x.dwLength:=sizeof(x);
  GlobalMemoryStatus(x);


  if x.dwAvailVirtual>(x.dwAvailPhys+x.dwAvailPageFile) then
    result:=x.dwAvailPhys+x.dwAvailPageFile
  else
    result:=x.dwAvailVirtual;
  {$else}
  result:=-1;
  {$ENDIF}

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
  ProcessHandler.ProcessHandle:=NewKernelHandler.OpenProcess(ifthen(GetSystemType<=6,$1f0fff, process_all_access) ,false,ProcessID);
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
  end else raise exception.Create(rsCEFPICantGetTheProcessListYouArePropablyUseinWindowsNtEtc);
end;   }


function GetUserNameFromPID(ProcessId: DWORD): string;
//credits to Alice0725
//http://forum.cheatengine.org/viewtopic.php?t=564382
{$IFDEF windows}
type
  PTOKEN_USER = ^TOKEN_USER;
var
  hToken: THandle;
  cbBuf: cardinal;
  pUser: PTOKEN_USER=nil;
  snu: SID_NAME_USE;
  ProcessHandle: THandle;
  UserSize: DWORD=0;
  DomainSize: DWORD=0;
  bSuccess: boolean;

  user, domain: string;
{$ENDIF}
begin
  Result := '';
  {$ifdef darwin}
  result:=GetEnvironmentVariable('USER');
  if result='' then
    result:=GetEnvironmentVariable('USERNAME');
  {$endif}

  {$IFDEF windows}
  pUser:=nil;
  ProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION, False, ProcessId);
  if ProcessHandle <> 0 then
  begin
    if OpenProcessToken(ProcessHandle, TOKEN_QUERY, hToken) then
    begin
      bSuccess := GetTokenInformation(hToken, TokenUser, nil, 0, cbBuf);
      while (not bSuccess) and (GetLastError = ERROR_INSUFFICIENT_BUFFER) do
      begin
        ReallocMem(pUser, cbBuf);
        bSuccess := GetTokenInformation(hToken, TokenUser, pUser, cbBuf, cbBuf);
      end;
      CloseHandle(hToken);
      if not bSuccess then Exit;

      LookupAccountSid(nil, pUser^.User.Sid, nil, UserSize, nil, DomainSize, snu);
      if (UserSize <> 0) and (DomainSize <> 0) then
      begin
        SetLength(User, UserSize);
        SetLength(Domain, DomainSize);
        if LookupAccountSid(nil, pUser^.User.Sid, PChar(User), UserSize,
          PChar(Domain), DomainSize, snu) then
        begin
          User := StrPas(PChar(User));
          Domain := StrPas(PChar(Domain));

          result:=user;
        end;
      end;

    end;
    CloseHandle(ProcessHandle);
  end;

  if puser<>nil then
    FreeMemandnil(pUser);
  {$ENDIF}
end;

procedure GetModuleList(ModuleList: TStrings; withSystemModules: boolean);
var ths: thandle;
    me32: MODULEENTRY32;
    x: pchar;
    moduledata: tmoduledata;
    i: integer;
    alreadyInTheList: boolean;
begin
  cleanModuleList(modulelist);

  ths:=CreateToolhelp32Snapshot(TH32CS_SNAPMODULE or TH32CS_SNAPMODULE32,processid);
  if ths<>0 then
  begin
    try
      zeromemory(@me32,sizeof(me32));
      me32.dwSize:=sizeof(me32);
      if module32first(ths,me32) then
      repeat
        x:=@me32.szModule[0];

        if (withSystemModules) or (not symhandler.inSystemModule(ptrUint(me32.modBaseAddr))) then
        begin
          alreadyInTheList:=false;
          for i:=0 to ModuleList.count-1 do
          begin
            moduledata:=tmoduledata(ModuleList.objects[i]);
            if moduledata.moduleaddress=ptrUint(me32.modBaseAddr) then
            begin
              alreadyInTheList:=true;
              break;
            end;
          end;

          if not alreadyInTheList then
          begin
            moduledata:=tmoduledata.Create;
            moduledata.moduleaddress:=ptrUint(me32.modBaseAddr);
            moduledata.modulesize:=me32.modBaseSize;

            ModuleList.AddObject(x,moduledata);
          end;
        end;
      until module32next(ths,me32)=false;

    finally
      closehandle(ths);
    end;
  end;
end;

procedure cleanModuleList(ModuleList: TStrings);
var i: integer;
begin
  for i:=0 to ModuleList.Count-1 do
    if ModuleList.Objects[i]<>nil then
    begin
      tmoduledata(ModuleList.Objects[i]).Free;
      ModuleList.Objects[i]:=nil;
    end;

  ModuleList.Clear;
end;



procedure GetThreadList(threadlist: TStrings);
var
  ths: THandle;
  te32: THREADENTRY32;
begin
  threadlist.clear;
  ths:=CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD,processid);
  te32.dwSize:=sizeof(te32);
  if Thread32First(ths,te32) then
  repeat
    if te32.th32OwnerProcessID=processid then
      threadlist.AddObject(inttohex(te32.th32ThreadID,1), tobject(te32.th32ThreadID));

  until Thread32next(ths,te32)=false;

  closehandle(ths);
end;

function getBaseParentFromWindowHandle(winhandle: HWnd): HWND;
var
  last: hwnd;
  i: integer;

  lastwithcaption: hwnd;
  lastcaption: string;

  test: hwnd;
begin
  {$IFDEF windows}
  i:=0;
  while (winhandle<>0) and (i<10000) do
  begin
    last:=winhandle;

    if GetWindowTextLength(last)>0 then
      lastwithcaption:=last;

    winhandle:=getwindow(winhandle, GW_OWNER);
    inc(i);
  end;

  result:=last; //withcaption;
  {$else}
  result:=0;
  {$ENDIF}

end;


{$IFDEF windows}
function SendMessageTimeout(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM; fuFlags, uTimeout: UINT; var lpdwResult: ptruint): LRESULT; stdcall; external 'user32' name 'SendMessageTimeoutA';
{$ENDIF}


procedure GetWindowList2(ProcessList: TStrings; showInvisible: boolean=true);
{$IFDEF windows}
type
  TBaseHandleMapEntry=record
    entrynr: integer;
    basenr: integer;
  end;

var previouswinhandle, winhandle: Hwnd;
    winprocess: Dword;
    temp: Pchar;
    wintitle: string;

    x: tstringlist;
    i,j:integer;

    ProcessListInfo: PProcessListInfo;
    tempptruint: ptruint;

    basehandle: hwnd;
    basehandlelist: TMap;

    basehandlemapentry: TBaseHandleMapEntry;



    pidlist: TMap;
    arrayid: integer;
    path,s: string;
    hi: HIcon;

  pl: array of record
    pid: dword;
    windowbases: array of record
      pi:PProcessListInfo;
      s: string;
    end;
  end;
  plpos: integer;
  SNAPHandle: THandle=INVALID_HANDLE_VALUE;
  lppe: NewKernelHandler.TProcessEntry32;

  found :boolean;
{$ENDIF}
begin
  {$IFDEF windows}
  //first create a processlist so I get the proper order
  setlength(pl,128);
  plpos:=0;

  zeromemory(@lppe,sizeof(lppe));
  lppe.dwSize:=sizeof(lppe);
  SNAPHandle:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
  if Process32First(snaphandle, lppe) then
  repeat
    pl[plpos].pid:=lppe.th32ProcessID;
    inc(plpos);
    if plpos>=length(pl) then setlength(pl,length(pl)*2);
  until process32next(snaphandle, lppe)=false;



  basehandlelist:=TMap.create(ituPtrSize, sizeof(TBaseHandleMapEntry));
  pidlist:=TMap.Create(ituPtrSize,sizeof(integer));
  arrayid:=0;

  getmem(temp,101);
  try
    x:=tstringlist.Create;

    for i:=0 to processlist.count-1 do
      if processlist.Objects[i]<>nil then
      begin
        ProcessListInfo:=PProcessListInfo(processlist.Objects[i]);
        if (ProcessListInfo.processIcon<>0) and (ProcessListInfo.processIcon<>HWND(-1)) then
        begin
          if ProcessListInfo^.processID<>GetCurrentProcessId then
            DestroyIcon(ProcessListInfo^.processIcon);
        end;

        freememandnil(ProcessListInfo);
      end;
    processlist.clear;

    winhandle:=getwindow(getforegroundwindow,GW_HWNDFIRST);

    i:=0;
    while (winhandle<>0) and (i<10000) do
    begin
      if IsWindowVisible(winhandle) then
      begin
        GetWindowThreadProcessId(winhandle,addr(winprocess));

        arrayid:=-1;
        if pidlist.GetData(winprocess, arrayid)=false then
        begin
          for j:=0 to plpos-1 do
            if pl[j].pid=winprocess then
            begin
              arrayid:=j;
              pidlist.Add(winprocess, arrayid);
            end;
        end;

        if arrayid<>-1 then
        begin
          basehandle:=getBaseParentFromWindowHandle(winhandle);

          temp[0]:=#0;
          getwindowtext(basehandle,temp,100);
          temp[100]:=#0;
          wintitle:=WinCPToUTF8(temp);




          if ((not ProcessesCurrentUserOnly) or (GetUserNameFromPID(winprocess)=username)) and (length(wintitle)>0) then
          begin
            if basehandlelist.GetData(basehandle,basehandlemapentry)=false then
            begin
              //add it
              getmem(ProcessListInfo,sizeof(TProcessListInfo));
              ProcessListInfo.processID:=winprocess;
              ProcessListInfo.processIcon:=0;

              //path:=lowercase(getProcessPathFromProcessID(winprocess));

              //ProcessListInfo.issystemprocess:=(ProcessListInfo.processID=4) or (pos(lowercase(windowsdir),path)>0) or (pos('system32',path)>0);
              ProcessListInfo.winhandle:=basehandle;

              //before adding check if there is already one with Exactly the same title (e.g: origin)
              found:=false;
              for j:=0 to length(pl[arrayid].windowbases)-1 do
                if pl[arrayid].windowbases[j].s=wintitle then
                begin
                  found:=true;
                  break;
                end;

              if not found then
              begin
                //add it to the list
                j:=length(pl[arrayid].windowbases);
                setlength(pl[arrayid].windowbases, j+1);

                pl[arrayid].windowbases[j].pi:=ProcessListInfo;
                pl[arrayid].windowbases[j].s:=wintitle;

                basehandlemapentry.entrynr:=arrayid;
                basehandlemapentry.basenr:=j;
                basehandlelist.Add(basehandle, basehandlemapentry);
              end;
            end; //else already in the list

          end;
        end;
      end;

      previouswinhandle:=winhandle;
      winhandle:=getwindow(winhandle,GW_HWNDNEXT);

      if winhandle=previouswinhandle then break;

      inc(i);
    end;

    for i:=0 to plpos-1 do
    begin
      for j:=0 to length(pl[i].windowbases)-1 do
        x.AddObject(IntTohex(pl[i].pid,8)+'-'+pl[i].windowbases[j].s,TObject(pl[i].windowbases[j].pi));
    end;

    processlist.Assign(x);
  finally
    freememandnil(temp);
    freememandnil(pidlist);
    freememandnil(basehandlelist);
    setlength(pl,0);

    if SNAPHandle<>INVALID_HANDLE_VALUE then
      closehandle(SNAPHandle);
  end;
  {$else}
  processlist.clear;
  {$ENDIF}
end;

procedure GetWindowList(ProcessList: TStrings; showInvisible: boolean=true);
var previouswinhandle, winhandle: Hwnd;
    winprocess: Dword;
    temp: Pchar;
    wintitle: string;

    x: tstringlist;
    i,j:integer;

    {$IFDEF WINDOWS}
    ProcessListInfo: PProcessListInfo;
    {$ENDIF}
    tempptruint: ptruint;
begin
  {$IFDEF windows}
  getmem(temp,101);
  try
    x:=tstringlist.Create;

    for i:=0 to processlist.count-1 do
      if processlist.Objects[i]<>nil then
      begin
        ProcessListInfo:=PProcessListInfo(processlist.Objects[i]);
        if ProcessListInfo.processIcon>0 then
        begin
          if ProcessListInfo^.processID<>GetCurrentProcessId then
            DestroyIcon(ProcessListInfo^.processIcon);
          ProcessListInfo.processIcon:=0;
        end;

        freemem(ProcessListInfo);
        ProcessListInfo:=nil;
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
        wintitle:=WinCPToUTF8(temp);


        if ((not ProcessesCurrentUserOnly) or (GetUserNameFromPID(winprocess)=username)) and (length(wintitle)>0) then
        begin
          getmem(ProcessListInfo,sizeof(TProcessListInfo));
          ProcessListInfo.processID:=winprocess;
          ProcessListInfo.winhandle:=winhandle;
          ProcessListInfo.processIcon:=0;
          x.AddObject(IntTohex(winprocess,8)+'-'+wintitle,TObject(ProcessListInfo));
        end;
      end;

      previouswinhandle:=winhandle;
      winhandle:=getwindow(winhandle,GW_HWNDNEXT);

      if winhandle=previouswinhandle then break;

      inc(i);
    end;

    x.Sort;
    processlist.Assign(x);
  finally
    freemem(temp);
    temp:=nil;
  end;
  {$else}
  processlist.clear;
  {$ENDIF}
end;

procedure GetWindowList(ProcessListBox: TListBox; showInvisible: boolean=true);
begin
  GetWindowList(ProcessListBox.Items, showInvisible);
end;

function GetCEdir:string;
{$IFDEF windows}
var
  PIDL: PItemIDList;
  Path: LPSTR;
  AMalloc: IMalloc;
{$ENDIF}
begin
  CheatEngineDir:=ExtractFilePath(application.ExeName);
  result:=CheatEngineDir;

  {$IFDEF windows}
  //blatantly stolen from http://www.scalabium.com/faq/dct0106.htm
  Path := StrAlloc(MAX_PATH);
  SHGetSpecialFolderLocation(0, CSIDL_PERSONAL, PIDL);
  if SHGetPathFromIDList(PIDL, Path) then
    tablesdir := WinCPToUTF8(Path)+'\'+strMyCheatTables;
  SHGetMalloc(AMalloc);
  AMalloc.Free(PIDL);
  StrDispose(Path);


  if DirectoryExists(tablesdir)=false then
  {$ENDIF}
    tablesdir:='';

end;

function GetWinDir:string;
var x: pchar;
begin
  result:='';
  {$IFDEF windows}
  getmem(x,200);
  if GetWindowsDirectory(x,200)>0 then
  begin
    result:=x;
    WindowsDir:=x;
  end;
  freemem(x);
  x:=nil;
  {$ENDIF}
end;

Procedure Shutdown;
//This will erase the temporary files and close the processhandle (In case it doesnt happen automatically)
begin
  deletefile(CheatEngineDir+'Memory.TMP');
  deletefile(CheatEngineDir+'Addresses.TMP');
  deletefile(CheatEngineDir+'Memory.UNDO');
  deletefile(CheatEngineDir+'Addresses.UNDO');
  freemem(memory);
  memory:=nil;
 // Closehandle(processhandle);

end;





function rewritedata(processhandle: thandle; address:ptrUint; buffer: pointer; var size:dword): boolean;
var original,a: dword;
    s: PtrUInt;
    v: boolean;
begin
  result:=false;
  //make writable, write, restore, flush
  if SystemSupportsWritableExecutableMemory or SkipVirtualProtectEx then
  begin
    v:=(SkipVirtualProtectEx=false) and VirtualProtectEx(processhandle,  pointer(address),size,PAGE_EXECUTE_READWRITE,original);
    result:=writeprocessmemory(processhandle,pointer(address),buffer,size,s);
    size:=s;
    if v then
      VirtualProtectEx(processhandle,pointer(address),size,original,a);
  end
  else
  begin
    ntsuspendProcess(processhandle);
    v:=VirtualProtectEx(processhandle, pointer(address),size,PAGE_READWRITE,original);
    if v then
    begin
      result:=writeprocessmemory(processhandle,pointer(address),buffer,size,s);
      result:=result or VirtualProtectEx(processhandle, pointer(address),size,original,a);
    end;
    ntresumeProcess(processhandle);
  end;
end;

function rewritecode(processhandle: thandle; address:ptrUint; buffer: pointer; var size:dword; force: boolean=false): boolean;
var
  init: dword;
  bytesleft: dword;
  chunk: dword;
begin
  if force then
  begin
    result:=true;

    bytesleft:=size;
    size:=0;
    init:=4096-(address and $fff); //init now contains the number of bytes needed to write to get to the first boundary
    init:=min(init, bytesleft);
    chunk:=init;
    if rewritedata(processhandle, address, buffer, init)=false then
      result:=false;

    size:=size+init;

    address:=address+chunk;
    ptruint(buffer):=ptruint(buffer)+chunk;

    dec(bytesleft, chunk);
    //address now contains the base address of a page so go from here
    while (bytesleft>0) do
    begin
      chunk:=4096;
      if rewritedata(processhandle, address, buffer, chunk)=false then
        result:=false;

      size:=size+chunk;
      address:=address+4096;
      ptruint(buffer):=ptruint(buffer)+4096;
    end;



  end
  else
  begin
    result:=rewritedata(processhandle,address,buffer,size);

    {$IFDEF windows}
    FlushInstructionCache(processhandle,pointer(address),size);
    {$ENDIF}
  end;

end;



function HasHyperthreading: boolean;
{$IFDEF windows}
type PSystemLogicalProcessorInformationArray=array [0..0] of TSystemLogicalProcessorInformation;
{$endif}
var a,b,c,d: dword;

  {$IFDEF windows}
  l: PSystemLogicalProcessorInformation; //8/13/2011: this structure is bugged because it's not propery aligned, but usefull enough for the first one
  la: PSystemLogicalProcessorInformationArray absolute l;
  {$ENDIF}
  needed: dword;

  succeed: boolean;
begin
  result:=false;
  succeed:=false;
  {$ifdef darwinarm64}
  exit(false);
  {$else}

  {$IFDEF windows}
 needed:=0;
  l:=nil;
  GetLogicalProcessorInformation(@l, @needed);

  if needed>0 then
  begin
    getmem(l, needed);
    try
      ZeroMemory(l, needed);
      if GetLogicalProcessorInformation(l, @needed)then
      begin
        if l.Relationship=RelationProcessorCore then //one core, multiple processors. This should be enough indication, but let's check
          result:=getbitcount(l.ProcessorMask)>1; //this cpuCORE has multiple logical processors, hyperthreading

        exit;
      end;
    finally
      freemem(l);
      l:=nil;
    end;
  end;
 {$ENDIF}

  if not succeed then
  begin
    //not supported, fall back to cpuid
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
      begin
        result:=true; //it has support for hyperthreading
      end;
    end;

  end;
  {$endif}

end;


var
  _CPUCOUNT: integer{$ifdef NOTMULTITHREADED}=1{$endif};

function GetCPUCount: integer;
{
this function will return how many active cpu cores there are at your disposal
}
var
  PA,SA: DWORD_PTR;
begin
  if _CPUCOUNT=0 then
  begin

    {$IFDEF windows}
    //get the cpu and system affinity mask, only processmask is used
    GetProcessAffinityMask(getcurrentprocess,PA,SA);

    _CPUCOUNT:=getbitcount(pa);
    //in the future make use of getlogicalprocessorinformation

    if _CPUCOUNT=0 then _CPUCOUNT:=1;
    {$else}
    _CPUCOUNT:=cpucount;

    if result=1 then
    begin
      //doubt!
    {$ifdef darwin}
      _CPUCOUNT:=macport.getCPUCount;
    {$endif}

    end;
    {$ENDIF}
  end;

  exit(_CPUCOUNT);
end;


function LoadFormPosition(form: Tcustomform; var x: TWindowPosArray):boolean;
var reg: tregistry;
    s: string;
    buf: PIntegerArray;
    i: integer;
    r: trect;
    m: TMonitor;
begin
  result:=false;

  buf:=nil;
  try
    reg:=tregistry.create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKey('\Software\'+strCheatEngine,false) then
      begin
        if reg.valueexists('Save window positions') then
          if reg.readbool('Save window positions') = false then exit;
      end;

      if Reg.OpenKey('\Software\'+strCheatEngine+'\Window Positions '+inttostr(screen.PixelsPerInch),false) or Reg.OpenKey('\Software\'+strCheatEngine+'\Window Positions',false) then
      begin
        s:=form.Name;
        s:=s+rsPosition;

        if reg.ValueExists(s) then
        begin
          i:=reg.GetDataSize(s);
          setlength(x, (i div 4)-4);

          getmem(buf, i);

          {$ifdef windows}
          reg.ReadBinaryData(s,buf[0],i);
          {$else}
          HexToBin(pchar(reg.ReadString(s)),pchar(@buf[0]),i);
          {$endif}

          form.position:=poDesigned;
          form.top:=buf[0];
          form.Left:=buf[1];
          form.width:=buf[2];
          form.height:=buf[3];

          r.top:=buf[0];
          r.Left:=buf[1];
          r.Right:=buf[2]+buf[1];
          r.Bottom:=buf[3]+buf[0];


          m:=screen.MonitorFromRect(r, mdNull);
          if m=nil then
          begin
            m:=screen.MonitorFromRect(r);

            if form.top<m.WorkareaRect.Top then form.top:=m.WorkareaRect.Top;
            if form.left<m.WorkareaRect.Left then form.left:=m.WorkareaRect.Left;


            if form.Top>m.WorkareaRect.Bottom-form.height then form.top:=m.WorkareaRect.Bottom-form.height;
            if form.Left>m.WorkareaRect.Right-form.Width then form.left:=m.WorkareaRect.Right-form.Width;
          end;


          for i:=0 to length(x)-1 do
            x[i]:=buf[4+i];



          result:=true;
        end;
      end;
    finally
      if buf<>nil then
        freememandnil(buf);

      reg.free;
    end;

  except
  end;
end;

procedure SaveFormPosition(form: Tcustomform; const extra: array of integer);
{
This function will save the position and the optional data in extra to an array element in the registry
}
var reg: tregistry=nil;
    buf: tmemorystream=nil;
    temp: integer;
    i: integer;
    s: string;
    hs: pchar;
begin
  //save window pos (only when it's in a normal state)
  if form.WindowState=wsNormal then
  begin

    reg:=tregistry.create;


    try
      Reg.RootKey := HKEY_CURRENT_USER;

      //make sure the option to save is enabled
      if Reg.OpenKey('\Software\'+strCheatEngine,false) then
      begin
        if reg.valueexists('Save window positions') then
          if reg.readbool('Save window positions') = false then
          begin
            freeandnil(reg);

            for i:=0 to length(extra)-1 do
            begin
              if extra[i]=$cecece then //the extra array needs to be accessed else fpc will cause stack corruption
              begin
                asm
                  nop
                end;
              end;
            end;

            exit;
          end;
      end;


      if Reg.OpenKey('\Software\'+strCheatEngine+'\Window Positions '+inttostr(screen.PixelsPerInch),true) then
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
          s:=s+rsPosition;



          {$ifdef windows}
          reg.WriteBinaryData(s,buf.Memory^,buf.Size);
          {$else}
          reg.WriteString(s,bintohexs(buf.Memory^, buf.Size));
          {$endif}
        finally
          if buf<>nil then
            freeandnil(buf);
        end;
      end;
    finally
      if reg<>nil then
        freeandnil(reg);
    end;

  end;
end;

procedure SaveFormPosition(form: TCustomform); overload;
var extra: array of integer;
begin
  setlength(extra,0);
  SaveFormPosition(form, extra);
end;

function LoadFormPosition(form: TCustomform):boolean; overload;
var extra: array of integer;
begin
  result:=LoadFormPosition(form, extra);
end;

function GetRelativeFilePath(filename: string):string;
begin
  result:=filename;
  if pos(uppercase(CheatEngineDir),uppercase(filename))=1 then
    result:='.\'+copy(filename,length(CheatEnginedir)+1,length(filename));
end;


function isjumporcall(address: ptrUint; var addresstojumpto: ptrUint): boolean;
{
Gets the address jumped to if it is a jump or call.
Currently only called by the memory browser on a low frequency, so speed is of secondary concern
}
var buf: array [0..31] of byte;
    actualread: PtrUInt;
    i,j: integer;
    st: string;

    dis: TDisassembler;
begin
{$ifndef standalonetrainer}
  result:=false;

  dis:=TDisassembler.Create;
  dis.showmodules:=false;
  dis.showsymbols:=false;
  dis.showsections:=false;

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

  finally
    dis.free;
  end;
{$endif}

end;


    {
function NewVarTypeToOldVarType(i: TVariableType):integer;
begin
  result:=2;
  case i of
    vtByte: result:=0;
    vtWord: result:=1;
    vtDword: result:=2;
    vtSingle: result:=3;
    vtDouble: result:=4;
    vtBinary: result:=5;
    vtQword: result:=6;
    vtString: result:=7;
    vtByteArray: result:=8;
    vtCustom: result:=10;
    vtAutoAssembler: result:=255;
  end;
end;   }

//for loading old pre 6.0 .CT files
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
    10: result:=vtCustom;
    255: result:=vtAutoAssembler; //aa script
  end;
end;

function VariableTypeToTranslatedString(variableType: TVariableType): string;
begin
  case variabletype of
    vtAll: result:=rs_vtAll;
    vtBinary: result:=rs_vtBinary;
    vtByteArray: Result:=rs_vtByteArray;
    vtByte: result:=rs_vtByte;
    vtWord: Result:=rs_vtWord;
    vtDword: Result:=rs_vtDword;
    vtQword: Result:=rs_vtQword;
    vtSingle: Result:=rs_vtSingle;
    vtDouble: Result:=rs_vtDouble;
    vtString: Result:=rs_vtString;
    vtUnicodeString: Result:=rs_vtUnicodeString;
    vtCodePageString: Result:=rs_vtCodePageString;
    vtPointer: result:=rs_vtPointer;
    vtAutoAssembler: Result:=rs_vtAutoAssembler;
    vtCustom: Result:=rs_vtCustom;
    else
     result:='Error';
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
    vtUnicodeString: Result:='Unicode String';
    vtPointer: result:='Pointer';
    vtAutoAssembler: Result:='Auto Assembler Script';
    vtCustom: Result:='Custom';
    else
     result:='Error';
  end;
end;

function StringToVariableType(s: string): TVariableType;
//NEVER translate this, use the vartypestrings unit for that
begin
  result:=vtByte;

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
  if s='unicode string' then result:=vtUnicodeString else
  if s='pointer' then result:=vtPointer else
  if s='custom' then  Result :=vtCustom else
  if s='grouped' then result:=vtGrouped else
  if s='auto assembler script' then result:=vtAutoAssembler;
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
  if (protect and PAGE_EXECUTE) = PAGE_EXECUTE then result:='PAGE_EXECUTE+';
  if (protect and PAGE_EXECUTE_READ) = PAGE_EXECUTE_READ then result:=result+'PAGE_EXECUTE_READ+';
  if (protect and PAGE_EXECUTE_READWRITE) = PAGE_EXECUTE_READWRITE then result:=result+'PAGE_EXECUTE_READWRITE+';
  if (protect and PAGE_EXECUTE_WRITECOPY) = PAGE_EXECUTE_WRITECOPY then result:=result+'PAGE_EXECUTE_WRITECOPY+';
  if (protect and PAGE_NOACCESS) = PAGE_NOACCESS then result:=result+'PAGE_NOACCESS+';
  if (protect and PAGE_READONLY) = PAGE_READONLY then result:=result+'PAGE_READONLY+';
  if (protect and PAGE_READWRITE) = PAGE_READWRITE then result:=result+'PAGE_READWRITE+';
  if (protect and PAGE_WRITECOPY) = PAGE_WRITECOPY then result:=result+'PAGE_WRITECOPY+';
  if (protect and PAGE_GUARD) = PAGE_GUARD then result:=result+'PAGE_GUARD+';
  if (protect and PAGE_NOCACHE) = PAGE_NOCACHE then result:=result+'PAGE_NOCACHE+';
  if (protect and PAGE_WRITECOMBINE) > 0 then result:=result+'PAGE_WRITECOMBINE+';

  if length(result)>0 then
    result:=Copy(result,1,length(result)-1)+'('+inttohex(protect,1)+')';
end;

function AllocationProtectToAccessRights(protect: dword): TAccessRights;
begin
  result:=[];
  if (protect and PAGE_EXECUTE) = PAGE_EXECUTE then result:=result+[arExecute, arRead];
  if (protect and PAGE_EXECUTE_READ) = PAGE_EXECUTE_READ then result:=result+[arExecute, arRead];
  if (protect and PAGE_EXECUTE_READWRITE) = PAGE_EXECUTE_READWRITE then result:=result+[arExecute, arRead, arWrite];
  if (protect and PAGE_EXECUTE_WRITECOPY) = PAGE_EXECUTE_WRITECOPY then result:=result+[arExecute, arRead, arWrite];
  if (protect and PAGE_NOACCESS) = PAGE_NOACCESS then result:=[];
  if (protect and PAGE_READONLY) = PAGE_READONLY then result:=result+[arRead];
  if (protect and PAGE_READWRITE) = PAGE_READWRITE then result:=result+[arRead, arWrite];
  if (protect and PAGE_WRITECOPY) = PAGE_WRITECOPY then result:=result+[arRead, arWrite];
end;

function AccessRightsToAllocationProtect(ar: TAccessRights): Dword;
begin
  result:=PAGE_NOACCESS;

  if ar=[arExecute] then result:=PAGE_EXECUTE else
  if ar=[arExecute, arRead] then result:=PAGE_EXECUTE_READ else
  if ar=[arExecute, arWrite] then result:=PAGE_EXECUTE_READWRITE else
  if ar=[arExecute, arRead, arWrite] then result:=PAGE_EXECUTE_READWRITE else
  if ar=[arRead] then result:=PAGE_READONLY else
  if ar=[arWrite] then result:=PAGE_READWRITE else
  if ar=[arRead, arWrite] then result:=PAGE_READWRITE;
end;

function freetypetostring(freetype: dword):string;
begin
  result:='';
  
  if (freetype and MEM_DECOMMIT) > 0 then result:='MEM_DECOMMIT+';
  if (freetype and MEM_RELEASE) > 0 then result:='MEM_RELEASE+';  
  result:=Copy(result,1,length(result)-1)+'('+inttohex(freetype,1)+')';
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

function InRangeQ(const AValue, AMin, AMax: QWord): Boolean;inline;
begin
  Result:=(AValue>=AMin) and (AValue<=AMax);
end;



function getProcessPathFromProcessID(pid: dword): string;
var ths: thandle;
    me32:MODULEENTRY32;
begin
  outputdebugstring('getProcessPathFromProcessID('+inttostr(pid)+')');
  result:='';
  me32.dwSize:=sizeof(MODULEENTRY32);
  ths:=CreateToolhelp32Snapshot(TH32CS_SNAPMODULE or TH32CS_SNAPMODULE32,pid);
  if ths<>0 then
  begin
    if Module32First(ths,me32) then
    begin
      outputdebugstring('me32.szExePath='+me32.szExePath);
      result:=me32.szExePath;
    end
    else
      OutputDebugString('Module32First failed');

    closehandle(ths);
  end
  else
    OutputDebugString('CreateToolhelp32Snapshot failed');
end;

function getProcessnameFromProcessID(pid: dword): string;
var ths: thandle;
    me32:MODULEENTRY32;
begin
  result:='???';
  me32.dwSize:=sizeof(MODULEENTRY32);
  ths:=CreateToolhelp32Snapshot(TH32CS_SNAPMODULE or TH32CS_SNAPMODULE32,pid);
  if ths<>0 then
  begin
    if Module32First(ths,me32) then
      result:=me32.szModule;

    closehandle(ths);
  end;
end;

procedure getDriverList(list: tstrings);
var need:dword;
    x: PPointerArray;
    i: integer;
    count: integer;
    drivername: pchar;
begin
  list.clear;
  {$IFDEF windows}
  EnumDevicedrivers(nil,0,need);
  getmem(x,need);
  try
    if enumDevicedrivers(@x[0],need,need) then
    begin
      count:=need div sizeof(pointer);
      getmem(drivername,200);
      try
        for i:=0 to count-1 do
        begin
          GetDevicedriverBaseNameA(x[i],drivername,200);
          list.addObject(inttohex(ptrUint(x[i]),8)+' - '+drivername, pointer(x[i]));
        end;


      finally
        freemem(drivername);
        drivername:=nil;
      end;
    end;
  finally
    freememandnil(x);
  end;
  {$ENDIF}
end;




function EscapeStringForRegEx(const S: string): string;      //copied and modified from the RegExprEscapeStr in the OldRegExpr.pp unit (it forgot the '+' check)
var
  i, len   : integer;
  s1: string;
begin
  result:= '';
  s1:='';
  if (S = '') then
   exit;

  len := Length (S);

  for i := 1 to len do
    begin
      if (S [i] in ['(','|', '.', '*', '?', '^', '$', '-', '+', '[', '{', '}', ']', ')', '\']) then
        begin
          s1 := s1 + '\';
        end;

      s1 := s1 + S[i];
    end;
  result:=s1;
end;

function getthreadCount(pid: qword): integer;
var
  ths: THandle;
  c: integer;
  te: TThreadEntry32;
begin
  result:=0;
  ths:=CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, pid);

  te.dwsize:=sizeof(te);;
  if Thread32First(ths, te) then repeat
    if te.th32OwnerProcessID=pid then inc(result);
  until Thread32Next(ths,te)=false;

  closehandle(ths);
end;

var
  StackStartCachePID: dword;
  StackStartCache: tmap;
  StackStartCacheCS: TCriticalSection;
  StackStartCacheKernel32Address: ptruint;

function GetStackStart(threadnr: integer=0): ptruint;
{$IFDEF windows}
var
  c: tcontext;    //do not move, or be sure it's on a proper alignment
  tbi: THREAD_BASIC_INFORMATION;
  stacktop: ptruint;
  x: PtrUInt;

  h: thandle;

  ths: thandle;
  te32: TThreadEntry32;
  i: integer;


  ldtentry: TLDTENTRY;
  mi: TModuleInfo;

  buf: pointer;
  buf32: PDwordArray absolute buf;
  buf64: PQWordArray absolute buf;
{$ENDIF}
//gets the stack base of the main thread, then checks where the "exitThread" entry is located and uses that -pointersize as the stackbase
begin

  StackStartCacheCS.enter;
  try
    if StackStartCachePID<>processid then
      StackStartCache.Clear //different pid, clear the old cache
    else
      if StackStartCache.GetData(threadnr,result) then exit;
  finally
    StackStartCacheCS.leave;
  end;

  //still here, so not cached


  result:=0;

  {$IFDEF windows}
  //get the first thread of this process
  if symhandler.getmodulebyname('kernel32.dll', mi)=false then
  begin
    symhandler.loadmodulelist;
    if symhandler.getmodulebyname('kernel32.dll', mi)=false then
      exit;
  end;




  ths:=CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD,processid);
  if ths<>INVALID_HANDLE_VALUE then
  begin
    zeromemory(@te32,sizeof(te32));
    te32.dwSize:=sizeof(te32);
    if Thread32First(ths, te32) then
    repeat
      if te32.th32OwnerProcessID=processid then
      begin
        //found a thread
        if threadnr=0 then //is it the one I want ?
        begin
          h:=OpenThread(THREAD_QUERY_INFORMATION or THREAD_GET_CONTEXT, false, te32.th32ThreadID);


          if (h<>0) then
          begin
            stacktop:=0;

            if processhandler.is64Bit then
            begin
              x:=0;
              i:=NtQueryInformationThread(h, ThreadBasicInformation, @tbi, sizeof(tbi), @x);
              if i=0 then
                ReadProcessMemory(processhandle, pointer(ptruint(tbi.TebBaseAddress)+8), @stacktop, 8, x);
            end
            else
            begin
              zeromemory(@c,sizeof(c));
              c.ContextFlags:=CONTEXT_SEGMENTS;
              if GetThreadContext(h, c) then
              begin
                if GetThreadSelectorEntry(h, c.segFs, ldtentry) then
                  ReadProcessMemory(processhandle, pointer(ptruint(ldtentry.BaseLow+ldtentry.HighWord.Bytes.BaseMid shl 16+ldtentry.HighWord.Bytes.BaseHi shl 24)+4), @stacktop, 4, x);

              end;


            end;

            closehandle(h);

            if stacktop<>0 then
            begin
              //find the stack entry pointing to the function that calls "ExitXXXXXThread"
              //Fun thing to note: It's the first entry that points to a address in kernel32

              getmem(buf,4096);

              if ReadProcessMemory(processhandle, pointer(stacktop-4096), buf, 4096, x) then
              begin
                if processhandler.is64Bit then
                begin
                  for i:=(4096 div 8)-1 downto 0 do
                    if inrangeq(buf64[i], mi.baseaddress, mi.baseaddress+mi.basesize) then
                    begin
                      result:=stacktop-4096+i*8;
                      break;
                    end;
                end
                else
                begin
                  for i:=(4096 div 4)-1 downto 0 do
                    if inrangeq(buf32[i], mi.baseaddress, mi.baseaddress+mi.basesize) then
                    begin
                      result:=stacktop-4096+i*4;
                      break;
                    end;

                end;
              end;

              freemem(buf);
              buf:=nil;

            end;
          end;


          break;

        end;

        dec(threadnr);
      end;

    until Thread32Next(ths, te32)=false;
    closehandle(ths);
  end;
  {$ENDIF}

  if result<>0 then
  begin
    StackStartCacheCS.enter;
    try
      if StackStartCache.HasId(threadnr)=false then
        StackStartCache.Add(threadnr, result);

      StackStartCachePID:=processid;
    finally
      StackStartCacheCS.leave;
    end;
  end;

end;

function getDiskFreeFromPath(path: string): int64;
var
  d: byte;
  drive: string;
begin
  result:=-1;
  drive:=uppercase(ExtractFileDrive(path));
  if drive<>'' then
  begin
    if drive[1] in ['A'..'Z'] then
    begin
      d:=1+ord(drive[1])-ord('A');
      result:=DiskFree(d);
    end;
  end;
end;

procedure protectme(pid: dword=0);
{$IFDEF windows}
var
  h: thandle;
  sa: SECURITY_ATTRIBUTES;
{$ENDIF}
begin
  {$IFDEF windows}
  if pid=0 then
    pid:=GetCurrentProcessId;
  h:=OpenProcess(ifthen(GetSystemType<=6,$1f0fff, process_all_access), false, pid);

  sa.nLength:=sizeof(sa);
  sa.bInheritHandle:=false;
  if ConvertStringSecurityDescriptorToSecurityDescriptorA('D:P(D;;;;;BG)', SDDL_REVISION_1, sa.lpSecurityDescriptor, nil) then
    SetKernelObjectSecurity(h, DACL_SECURITY_INFORMATION, sa.lpSecurityDescriptor);
  {$ENDIF}
end;

procedure EnableWindowsSymbols(warn: boolean=true);
{$IFDEF windows}
var
  path: string;
  shortpath: pchar;
{$ENDIF}
begin
  {$IFDEF windows}
  if (length(trim(tempdiralternative))>2) and dontusetempdir then
    path:=trim(tempdiralternative)
  else
  begin
    path:=trim(GetEnvironmentVariable('_NT_SYMBOL_PATH'));
    if path='' then
      path:=trim(GetEnvironmentVariable('_NT_ALTERNATE_SYMBOL_PATH'));

    if path='' then
      path:=GetTempDir;
  end;

  path:=path+strCheatEngine+' Symbols';

  ForceDirectory(path);
  if warn and (messagedlg(rsThisCanTakeSomeTime, mtWarning, [mbyes, mbno], 0, mbno)<>mryes) then exit;

  getmem(shortpath,256);
  GetShortPathName(pchar(path),shortpath,255);
  symhandler.setsearchpath('srv*'+path+'*https://msdl.microsoft.com/download/symbols');
  freemem(shortpath);

  symhandler.reinitialize(true);
  {$ENDIF}
end;


procedure Log(s: string);
begin
  OutputDebugString(pchar(s));


end;

var r: TCPUIDResult;

initialization
  StackStartCache:=tmap.Create(itu4,sizeof(ptruint));
  StackStartCachePID:=0;
  StackStartCacheCS:=TCriticalSection.Create;


  if not assigned(OpenProcess) then
  begin
    {$ifdef darwin}
    OpenProcess:=@macport.OpenProcess;
    {$endif}
    {$ifdef windows}
    OpenProcess:=@windows.OpenProcess;
    {$endif}
  end;


  ownprocesshandle := OpenProcess(ifthen(GetSystemType<=6,$1f0fff, process_all_access), True, GetCurrentProcessId);



  getmem(tempdir,256);

  {$ifdef windows}
  GetTempPath(256,tempdir);
  {$else}

  strcopy(tempdir, pchar(GetTempDir));
  {$endif}

  GetWindir;
  keysfilemapping:=0;

  setlength(windowlist,0);
  setlength(donthidelist,0);
  allwindowsareback:=true;
  stealthhook:=0;
  iswin2kplus:=GetSystemType>=5;


  {$IFDEF windows}
  GetSystemInfo(@systeminfo);

  r:=CPUID(0);
  systemSupportsIntelPT:=((r.ebx=1970169159) and (r.ecx=1818588270) and (r.edx=1231384169)) and //intel
   ((CPUID(7,0).ebx shr 25) and 1=1) and //has IPT
   ((CPUID($14,0).ebx and 1)=1); //can select process


  {$ENDIF}

  username:=GetUserNameFromPID(GetCurrentProcessId);


  Screen.HintFont;

  {$ifdef darwin}
  systeminfo.lpMaximumApplicationAddress:=pointer($7fffffffffffffff);
  systeminfo.lpMinimumApplicationAddress:=pointer($10000);
  systeminfo.dwAllocationGranularity:=$10000;
  systeminfo.dwPageSize:=$1000;
  {$endif}


finalization
  if tempdir<>nil then
    freememandnil(tempdir);
end.




