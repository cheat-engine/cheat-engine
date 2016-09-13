unit symbolhandler;

{$MODE Delphi}

interface

{$ifdef windows}

uses jwawindows, windows, classes,LCLIntf,imagehlp,{psapi,}sysutils, cefuncproc,
  newkernelhandler,syncobjs, SymbolListHandler, fgl, typinfo, cvconst, PEInfoFunctions,
  DotNetPipe, DotNetTypes, commonTypeDefs, math, LazUTF8;
{$endif}

{$ifdef unix}
uses unixporthelper, Classes, sysutils, NewKernelHandler, syncobjs, SymbolListHandler,
  fgl, typinfo, cvconst, DotNetPipe, DotNetTypes, commonTypeDefs, math;
{$endif}

{$ifdef windows}
Procedure Free (P : pointer); cdecl; external 'msvcrt' name 'free';
{$endif}

{$ifdef autoassemblerdll}
var
  processid: dword;
  processhandle: thandle;

Type TMemoryRegion = record
  BaseAddress: ptrUint;
  MemorySize: Dword;
  IsChild: boolean;
  startaddress: pointer;
  end;
type TMemoryregions = array of tmemoryregion;
  
{$endif}

type TUDSEnum=record
  address: ptrUint;
  allocsize: dword;
  addressstring: pchar; //points to the string
  doNotSave: boolean;
end;

type symexception=class(Exception);


type TUserdefinedsymbol=record
  symbolname: string;
  address: ptrUint;
  addressstring: string;

  allocsize: dword; //if it is a global alloc, allocsize>0
  processid: dword; //the processid this memory was allocated to (in case of processswitches)
  doNotSave: boolean; //if true this will cause this entry to not be saved when the user saves the table
end;

type TModuleInfo=record
  modulename: string;
  modulepath: string;
  isSystemModule: boolean;
  baseaddress: ptrUint;
  basesize: dword;
  is64bitmodule: boolean;
  symbolsLoaded: boolean; //true if the api symbols have been handled
end;

type TUserdefinedSymbolCallback=procedure;

type
  TSymHandler=class;

  TSymbolloaderthread=class(tthread)
  private

    targetself: boolean;
    owner: Tsymhandler;
    thisprocesshandle: thandle;
    thisprocessid: dword;
    currentModuleName: string;
    currentModuleIsNotStandard: boolean;

    extraSymbolData: TExtraSymbolData;
    //highestsymboladdress: ptruint;
//    highestsymbol: string;

    fprogress: integer;
    modulecount: integer;
    enumeratedModules: integer;
    procedure EnumerateExtendedDebugSymbols;

    procedure LoadDriverSymbols;
    procedure LoadDLLSymbols;
    procedure finishedLoadingSymbols;
    function NetworkES(modulename: string; symbolname: string; address: ptruint; size: integer; secondary: boolean): boolean;
  public
    isloading: boolean;
    apisymbolsloaded: boolean;
    error: boolean;
    symbolsloaded: boolean;

    kernelsymbols: boolean;
    dllsymbols: boolean;
    searchpath: string;

    symbollist: TSymbolListHandler;


    procedure execute; override;
    constructor create(owner: TSymhandler; targetself, CreateSuspended: boolean);
    destructor destroy; override;

    property progress: integer read fProgress;

  end;

  TModuleInfoArray=array of TModuleInfo;

  TTokens=array of string;

  PBoolean=^boolean;

  TDotNetModuleSymbols=record
    modulename: string; //for debugging
    modulebase: qword;  //unique identifier
    symbollist: TSymbolListHandler;
  end;

  TSymHandler=class
  private
    symbolloaderthread: TSymbolloaderthread;

    modulelistpos: integer;
    modulelist: TModuleInfoArray;

    symbolloadervalid: TMultiReadExclusiveWriteSynchronizer;
    modulelistMREW: TMultiReadExclusiveWriteSynchronizer;

    userdefinedsymbolspos: integer;
    userdefinedsymbols: array of TUserdefinedsymbol;
    userdefinedsymbolsCS: TCriticalSection; //not actively used and needs reentry which isn't implemented in the MREW

    fshowmodules: boolean;   //--determines what is returned by getnamefromaddress
    fshowsymbols: boolean;   ///

    UserdefinedSymbolCallback: TUserdefinedSymbolCallback;
    searchpath: string;

    globalalloc: pointer; //if set it hold a pointer to the last free memory that was allocated.
    globalallocsizeleft: integer; //defines how much memory was left
    globalallocpid: integer; //the processid this alloc belongs to

    commonModuleList: tstringlist;
    symbollist: TSymbolListHandler;
    symbollists: array of TSymbolListHandler;
    symbollistsMREW: TMultiReadExclusiveWriteSynchronizer;


    SymbolsLoadedNotification: array of TNotifyEvent;

    dotNetDataCollector: TDotNetPipe;


    dotnetModuleSymbolList: array of TDotNetModuleSymbols;
    dotnetModuleSymbolListMREW: TMultiReadExclusiveWriteSynchronizer; //MREW for adding/removing modules to the list


    function getusedprocesshandle :thandle;
    function getusedprocessid:dword;
    function getisloaded:boolean;
    function geterror:boolean;
    function getProgress:integer;
    function getDotNetAccess: boolean;
    function GetUserdefinedSymbolByNameIndex(symbolname:string):integer;
    function GetUserdefinedSymbolByAddressIndex(address: ptruint):integer;

    function areSymbolsLoadedForModule(symbolname: string): boolean;
    procedure markModuleAsLoaded(address: ptruint); //called by the symbolhandlerthread

    procedure setshowmodules(x: boolean); //todo: Move this to the disassembler and let that decide
    procedure setshowsymbols(x: boolean);
    procedure tokenize(s: string; var tokens: TTokens);
  public

    kernelsymbols: boolean;
    dllsymbols: boolean;
    
    locked: boolean;
    targetself: boolean;

    ExceptionOnLuaLookup: boolean;

    property showmodules: boolean read fshowmodules write setshowmodules;
    property showsymbols: boolean read fshowsymbols write setshowsymbols;

    property usedprocesshandle: thandle read getusedprocesshandle;
    property usedprocessid: dword read getusedprocessid;
    property isloaded: boolean read getisloaded;
    property hasError: boolean read geterror;
    property hasDotNetAccess: boolean read getDotNetAccess;
    property progress: integer read getProgress;

    procedure waitforsymbolsloaded(apisymbolsonly: boolean=false; specificmodule: string='');

    procedure EnumDotNetModule(m: TdotNetmodule; symbolhandler: TSymbolListHandler);
    procedure reinitializeDotNetSymbols(modulename: string='');
    procedure reinitialize(force: boolean=false);
    function loadmodulelist: boolean; //returns true if a change was detected from the previous list
    procedure ReinitializeUserdefinedSymbolList;
    procedure fillMemoryRegionsWithModuleData(var mr: TMemoryregions; startaddress: ptruint; size: dword);
    procedure getModuleList(list: tstrings);
    procedure GetSymbolList(address: ptruint; list: tstrings);

    function getmodulebyaddress(address: ptrUint; var mi: TModuleInfo):BOOLEAN;
    function getmodulebyname(modulename: string; var mi: TModuleInfo):BOOLEAN;
    function inModule(address: ptrUint): BOOLEAN; //returns true if the given address is part of a module
    function inSystemModule(address: ptrUint): BOOLEAN;
    function getNameFromAddress(address:ptrUint):string; overload;
    function getNameFromAddress(address:ptrUint; var found: boolean; hexcharsize: integer=8):string; overload;
    function getNameFromAddress(address:ptrUint;symbols:boolean; modules: boolean; baseaddress: PUINT64=nil; found: PBoolean=nil; hexcharsize: integer=8):string; overload;
    function getExtraDataFromSymbolAtAddress(address: ptruint): TExtraSymbolData;

    function getAddressFromNameL(name: string):ptrUint; //Called by lua. Looks at ExceptionOnLookup
    function getAddressFromName(name: string):ptrUint; overload;
    function getAddressFromName(name: string; waitforsymbols: boolean):ptrUint; overload;
    function getAddressFromName(name: string; waitforsymbols: boolean; out haserror: boolean):ptrUint; overload;
    function getAddressFromName(name: string; waitforsymbols: boolean; out haserror: boolean; context: PContext):ptrUint; overload;

    function getSymbolInfo(name: string; var syminfo: TCESymbolInfo): boolean;

    function GetLayoutFromAddress(address: ptruint; var addressdata: TAddressData): boolean;
    function getsearchpath:string;
    procedure setsearchpath(path:string);

    //userdefined symbols
    function DeleteUserdefinedSymbol(symbolname:string):boolean;
    function GetUserdefinedSymbolByName(symbolname:string):ptrUint;
    function SetUserdefinedSymbolAllocSize(symbolname:string; size: dword; preferedaddress: ptruint=0): boolean;
    function GetUserdefinedSymbolByAddress(address:ptrUint):string;
    procedure AddUserdefinedSymbol(addressstring: string; symbolname: string; donotsave: boolean=false);
    procedure EnumerateUserdefinedSymbols(list:tstrings);

    function ParseAsPointer(s: string; list:tstrings): boolean;
    function ParseRange(s: string; var start: QWORD; var stop: QWORD): boolean;
    function GetAddressFromPointer(s: string; var error: boolean):ptrUint;

    function LookupStructureOffset(s: string; out offset: integer): boolean;

    procedure loadCommonModuleList;
    function getCommonModuleList: Tstringlist;
    procedure RegisterUserdefinedSymbolCallback(callback: TUserdefinedSymbolCallback);


    procedure RemoveFinishedLoadingSymbolsNotification(n: TNotifyEvent);
    procedure AddFinishedLoadingSymbolsNotification(n: TNotifyEvent);

    procedure AddSymbolList(sl: TSymbolListHandler);
    procedure RemoveSymbolList(sl: TSymbolListHandler);

    procedure NotifyFinishedLoadingSymbols; //go through the list of functions to call when the symbollist has finished loading
    constructor create;
    destructor destroy; override;


end;

var symhandler: TSymhandler=nil;
    selfsymhandler: TSymhandler=nil;  //symhandler object for CE itself

type TSymbolLookupCallbackPoint=(
        slStart=0, //The very start of a symbol lookup. Before tokenization
        slNotInt=1, //Called when it has been determined it's not a hexadecimal only string. Before tokenization
        slNotModule=2, //Called when it has been determined the current token is not a modulename
        slNotUserdefinedSymbol=3, //Called when it has been determined it's not a userdefined symbol
        slNotSymbol=4, //: Called when it has been determined it's not a symbol in the symbollist
        slFailure=5 //: Called when it has no clue what the given string is
        );

type TSymbolLookupCallback=function(s: string): PtrUInt of object;
type TAddressLookupCallback=function(address: ptruint): string of object;



function registerSymbolLookupCallback(callback: TSymbolLookupCallback;  cbp: TSymbolLookupCallbackPoint): integer;
procedure unregisterSymbolLookupCallback(id: integer);

function registerAddressLookupCallback(callback: TAddressLookupCallback): integer;
procedure unregisterAddressLookupCallback(id: integer);


{$ifdef windows}
type TSymFromName=function(hProcess: HANDLE; Name: LPSTR; Symbol: PSYMBOL_INFO): BOOL; stdcall;
type TSymFromAddr=function(hProcess:THANDLE; Address:dword64; Displacement:PDWORD64; Symbol:PSYMBOL_INFO):BOOL;stdcall;

var SymFromName: TSymFromName;
    SymFromAddr: TSymFromAddr;
{$endif}

procedure symhandlerInitialize;

implementation

{$ifdef windows}
uses assemblerunit, driverlist, LuaHandler, lualib, lua, lauxlib,
  disassemblerComments, StructuresFrm2, networkInterface, networkInterfaceApi,
  processhandlerunit, Globals, Parsers, MemoryQuery, LuaCaller;
{$endif}

{$ifdef unix}
uses networkInterface, networkInterfaceApi, ProcessHandlerUnit, Globals, Parsers;
{$endif}

resourcestring
  rsSymbolloaderthreadHasCrashed = 'Symbolloaderthread has crashed';
  rsYouCanTChangeThisSettingAtTheMoment = 'You can''t change this setting at the moment';
  rsPleaseProvideABiggerSize = 'Please provide a bigger size';
  rsErrorAllocatingMemory = 'Error allocating memory';
  rsTheSymbolNamedWasPreviouslyDeclared = 'The symbol named %s was previously declared with a size of %s instead of %s. all scripts that use this memory must give the same size. '
    +'Adjust the size, or delete the old alloc from the userdefined symbol list';
  rsAlreadyExists = 'already exists';
  rsYouCanTAddASymbolWithAddress0 = 'You can''t add a symbol with address 0';
  rsFailureDeterminingWhatMeans = 'Failure determining what %s means';


const
  LIST_MODULES_DEFAULT=0;
  LIST_MODULES_32BIT=1;
  LIST_MODULES_64BIT=2;
  LIST_MODULES_ALL=3;

{$IFNDEF UNIX}
type TEnumProcessModulesEx=function(hProcess: HANDLE; lphModule: PHMODULE; cb: DWORD; var lpcbNeeded: DWORD; dwFilterFlag: DWORD): BOOL; stdcall;
type TEnumProcessModules=function(hProcess: HANDLE; lphModule: PHMODULE; cb: DWORD; var lpcbNeeded: DWORD): BOOL; stdcall;
type TGetModuleFileNameEx=function(hProcess: HANDLE; hModule: HMODULE; lpFilename: pchar; nSize: DWORD): DWORD; stdcall;
{$ENDIF}


var
  {$IFNDEF UNIX}
  EnumProcessModulesEx: TEnumProcessModulesEx;
  EnumProcessModules:   TEnumProcessModules;
  GetModuleFileNameEx:  TGetModuleFilenameEx;
  {$ENDIF}

  SymbolLookupCallbacks: array [slStart..slFailure] of array of TSymbolLookupCallback;
  AddressLookupCallbacks: array of TAddressLookupCallback;

function registerSymbolLookupCallback(callback: TSymbolLookupCallback;  cbp: TSymbolLookupCallbackPoint): integer;
var i: integer;
begin
  //first check if there is an unassigned entry to use
  for i:=0 to length(SymbolLookupCallbacks[cbp])-1 do
    if not assigned(SymbolLookupCallbacks[cbp][i]) then
    begin
      result:=i;
      SymbolLookupCallbacks[cbp][i]:=callback;

      result:=result or (integer(cbp) shl 29); //last 3 bits of the ID specify the callbackpoint. sure, it limits the number of callbacks per point to only 536870911 but I don't care
      exit;
    end;

  result:=length(SymbolLookupCallbacks[cbp]);
  setlength(SymbolLookupCallbacks[cbp], result+1);
  SymbolLookupCallbacks[cbp][result]:=callback;
  result:=result or (integer(cbp) shl 29);
end;

procedure unregisterSymbolLookupCallback(id: integer);
var cbp: TSymbolLookupCallbackPoint;
begin
  cbp:=TSymbolLookupCallbackPoint(id shr 29);
  id:=id and $1FFFFFFF;

  if id<length(SymbolLookupCallbacks[cbp]) then
  begin
    CleanupLuaCall(TMethod(SymbolLookupCallbacks[cbp][id]));
    SymbolLookupCallbacks[cbp][id]:=nil;
  end;
end;

function registerAddressLookupCallback(callback: TAddressLookupCallback): integer;
var i: integer;
begin
  //first check if there is an unassigned entry to use
  for i:=0 to length(AddressLookupCallbacks)-1 do
    if not assigned(AddressLookupCallbacks[i]) then
    begin
      result:=i;
      AddressLookupCallbacks[i]:=callback;
      exit;
    end;

  result:=length(AddressLookupCallbacks);
  setlength(AddressLookupCallbacks, result+1);
  AddressLookupCallbacks[result]:=callback;
end;

procedure unregisterAddressLookupCallback(id: integer);
begin
  if id<length(AddressLookupCallbacks) then
  begin
    CleanupLuaCall(TMethod(AddressLookupCallbacks[id]));
    AddressLookupCallbacks[id]:=nil;
  end;
end;


function EnumProcessModulesExNotImplemented(hProcess: HANDLE; lphModule: PHMODULE; cb: DWORD; var lpcbNeeded: DWORD; dwFilterFlag: DWORD): BOOL; stdcall;
begin
{$ifndef unix}
  result:=EnumProcessModules(hProcess,lphModule,cb,lpcbNeeded);
{$else}
  result:=false;
{$endif}
end;


procedure TSymbolloaderthread.LoadDLLSymbols;
var need:dword;
    x: PPointerArray;
    i: integer;
    count: integer;
    modulename: pchar;
    modulelisttype: integer;

begin
  {$ifndef unix}
  need:=0;

  modulelisttype:=LIST_MODULES_ALL;

  EnumProcessModulesEx(thisprocesshandle,nil,0,need, modulelisttype);
  getmem(x,need);
  try
    if EnumProcessModulesEx(thisprocesshandle,@x[0],need,need, modulelisttype) then
    begin

      count:=need div sizeof(pointer);
      modulecount:=count;

      getmem(modulename,1024);
      try
        for i:=0 to count-1 do
        begin

          GetModuleFileNameEx(thisprocesshandle,ptrUint(x[i]),modulename,200);
          symLoadModule64(thisprocesshandle,0,pchar(modulename),nil,ptrUint(x[i]),0);
        end;
      finally
        freemem(modulename);
        modulename:=nil;
      end;
    end;
  finally
    freemem(x);
    x:=nil;
  end;

  {$endif}
end;

procedure TSymbolloaderthread.LoadDriverSymbols;
var need:dword;
    x: PPointerArray;
    i: integer;
    count: integer;
    drivername: pchar;
begin
  {$IFNDEF UNIX}
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
          GetDevicedriverFileName(x[i],drivername,200);
          //add drive letter
          symLoadModule64(thisprocesshandle,0,pchar(drivername),nil,ptrUint(x[i]),0);
        end;
      finally
        freemem(drivername);
        drivername:=nil;
      end;
    end;
  finally
    freemem(x);
    x:=nil;
  end;
  {$ENDIF}
end;

procedure TSymbolloaderthread.finishedLoadingSymbols;
begin
  OutputDebugString('finishedLoadingSymbols called');
  if (not targetself) and (symhandler<>nil) then symhandler.NotifyFinishedLoadingSymbols;
  OutputDebugString('exit finishedLoadingSymbols()');
end;

type
PIMAGEHLP_STACK_FRAME = ^TIMAGEHLP_STACK_FRAME;
TIMAGEHLP_STACK_FRAME = record
        InstructionOffset : ULONG64;
        ReturnOffset : ULONG64;
        FrameOffset : ULONG64;
        StackOffset : ULONG64;
        BackingStoreOffset : ULONG64;
        FuncTableEntry : ULONG64;
        Params : array[0..3] of ULONG64;
        Reserved : array[0..4] of ULONG64;
        Virtual : BOOL;
        Reserved2 : ULONG;
     end;
IMAGEHLP_STACK_FRAME = TIMAGEHLP_STACK_FRAME;
LPIMAGEHLP_STACK_FRAME = PIMAGEHLP_STACK_FRAME;



function symflagsToString(symflags: dword): string;
var s: string;
begin
  {$IFNDEF UNIX}
  s:='';
  if (symFlags and SYMFLAG_VALUEPRESENT)>0 then
    s:=s+'VALUEPRESENT ';
  if (symFlags and SYMFLAG_REGISTER)>0 then
    s:=s+'REGISTER ';
  if (symflags and SYMFLAG_REGREL)>0 then
    s:=s+'REGREL ';
  if (symflags and SYMFLAG_FRAMEREL)>0 then
    s:=s+'FRAMEREL ';
  if (symflags and SYMFLAG_PARAMETER)>0 then
    s:=s+'PARAMETER ';
  if (symflags and SYMFLAG_LOCAL)>0 then
    s:=s+'LOCAL ';
  if (symflags and SYMFLAG_CONSTANT)>0 then
    s:=s+'CONSTANT ';
  if (symflags and SYMFLAG_EXPORT)>0 then
    s:=s+'EXPORTED ';
  if (symflags and SYMFLAG_FORWARDER)>0 then
    s:=s+'FORWARDER ';
  if (symflags and SYMFLAG_FUNCTION)>0 then
    s:=s+'FUNCTION ';
  if (symflags and SYMFLAG_VIRTUAL)>0 then
    s:=s+'VIRTUAL ';
  if (symflags and SYMFLAG_THUNK)>0 then
    s:=s+'THUNK ';
  if (symflags and SYMFLAG_TLSREL)>0 then
    s:=s+'TLSREL ';
  {$ENDIF}
end;

function GetTypeName(h: HANDLE; modbase: UINT64; index: integer; infinitycheck: integer=50): string;
var x: dword;
    type_symtag: TSymTagEnum;
    name: PWCHAR;
begin
  {$IFNDEF UNIX}
  result:='';
  if infinitycheck<0 then exit;

  if SymGetTypeInfo(h, modbase, index, TI_GET_SYMTAG, @type_symtag) then
  begin
    case type_symtag of
      SymTagBaseType:
      begin
        x:=0;
        if SymGetTypeInfo(h, ModBase, index, TI_GET_BASETYPE, @x) then
        begin
          case TBasicType(x) of
            btNoType: result:='NoType';
            btVoid: result:='VOID';
            btChar: result:='CHAR';
            btWChar: result:='WCHAR';
            btInt: result:='INT';
            btUInt: result:='UINT';
            btFloat: result:='FLOAT';
            btBCD: result:='BCD';
            btBool: result:='BOOL';
            btLong: result:='LONG';
            btULong: result:='ULONG';
            btCurrency: result:='CURRENCY';
            btDate: result:='DATE';
            btVariant: result:='VARIANT';
            btComplex: result:='COMPLEX';
            btBit: result:='BIT';
            btBSTR:result:='BTSTR';
            btHresult: result:= 'HRESULT';
            else
              result:='BasicType'+inttostr(x);
          end;



        end;
      end;

      SymTagPointerType:
      begin
        if SymGetTypeInfo(h, ModBase, index, TI_GET_TYPEID, @x) then
          result:=GetTypeName(h, modbase, x, infinitycheck-1)
      end;

      SymTagUDT:
      begin
        name:=nil;
        if SymGetTypeInfo(h, ModBase, index, TI_GET_SYMNAME, @name) then
        begin
          result:=name;
          LocalFree(PTRUINT(name));
        end;
      end;

      SymTagArrayType:
      begin
        if SymGetTypeInfo(h, ModBase, index, TI_GET_ARRAYINDEXTYPEID, @x) then
          result:=GetTypeName(h, modbase, x, infinitycheck-1)+'[]'
        else
          result:='[]';
      end;

      SymTagEnum:
      begin
        name:=nil;
        if SymGetTypeInfo(h, ModBase, index, TI_GET_SYMNAME, @name) then
        begin
          result:='enum '+name;
          LocalFree(PTRUINT(name));
        end;
      end;

      SymTagFunctionType:
      begin
        result:='(function)';
      end;

      SymTagVTableShape:
      begin
        result:='<vtable>';
      end;


      else
      begin
        //something else

      end;

    end;


  end;
  {$ENDIF}

end;

function RegToString(reg: integer): string;
begin
  result:='';

  case reg of
    CV_REG_NONE: result:='';
    CV_ALLREG_ERR   : result:='ERR';
    CV_ALLREG_TEB   : result:='TEB';
    CV_ALLREG_TIMER : result:='TIMER';
    CV_ALLREG_EFAD1 : result:='EFAD1';
    CV_ALLREG_EFAD2 : result:='EFAD2';
    CV_ALLREG_EFAD3 : result:='EFAD3';
    CV_ALLREG_VFRAME: result:='VFRAME';
    CV_ALLREG_HANDLE: result:='HANDLE';
    CV_ALLREG_PARAMS: result:='PARAMS';
    CV_ALLREG_LOCALS: result:='LOCALS';
    CV_ALLREG_TID   : result:='TID';
    CV_ALLREG_ENV   : result:='ENV';
    CV_ALLREG_CMDLN : result:='CMDLN';
    CV_AMD64_RAX    : result:='RAX';
    CV_AMD64_RCX    : result:='RCX';
    CV_AMD64_RDX    : result:='RDX';
    CV_AMD64_RBX    : result:='RBX';
    CV_AMD64_RSP    : result:='RSP';
    CV_AMD64_RBP    : result:='RBP';
    CV_AMD64_RSI    : result:='RSI';
    CV_AMD64_RDI    : result:='RDI';
    CV_AMD64_R8     : result:='R8';
    CV_AMD64_R9     : result:='R9';
    CV_AMD64_R10    : result:='R10';
    CV_AMD64_R11    : result:='R11';
    CV_AMD64_R12    : result:='R12';
    CV_AMD64_R13    : result:='R13';
    CV_AMD64_R14    : result:='R14';
    CV_AMD64_R15    : result:='R15';
    CV_REG_EAX      : result:='EAX';
    CV_REG_ECX      : result:='ECX';
    CV_REG_EDX      : result:='EDX';
    CV_REG_EBX      : result:='EBX';
    CV_REG_ESP      : result:='ESP';
    CV_REG_EBP      : result:='EBP';
    CV_REG_ESI      : result:='ESI';
    CV_REG_EDI      : result:='EDI';
    else
      result:='?';
  end;



end;

function getPositionFromSymInfo(pSymInfo:PSYMBOL_INFO): string;
var addressString: string;
begin
  result:='';

  //try to figure out whee it is stored (register/ offset, etc...)
  result:=RegToString(pSymInfo.Register);
  if (pSymInfo.Address<>0) then
  begin
    addressString:=IntToHexSigned(LONG64(pSymInfo.Address),1);

    if (result<>'') then
    begin
      //it's a reg+address notation
      if LONG64(pSymInfo.Address)>0 then
        result:=result+'+'+addressString
      else
        result:=result+addressString; //already has a - sign


    end
    else
      result:=addressString;


  end;
end;

function ES2(pSymInfo:PSYMBOL_INFO; SymbolSize:ULONG; UserContext:pointer):BOOL;stdcall;
var
  s: string;
  self: TSymbolloaderthread;

  x: DWORD;
  type_symtag: Tsymtagenum;

  isparam: boolean;

  esde: TExtraSymbolDataEntry;
begin
  {$IFNDEF UNIX}
  if pSymInfo.NameLen=0 then
    exit(false);

  self:=TSymbolloaderthread(UserContext);




  isparam:=(pSymInfo.Flags and SYMFLAG_PARAMETER)>0;

  s:=GetTypeName(self.thisprocesshandle, pSymInfo.ModBase, pSymInfo.TypeIndex);


  //add an extra symboldataentry
  esde:=TExtraSymbolDataEntry.create;
  esde.name:=pchar(@pSymInfo.Name);
  esde.vtype:=s;

  esde.position:=getPositionFromSymInfo(pSymInfo);
  esde.syminfo:=pSymInfo^; //the name is known, so no need to do any fancy allocating

  if isparam then
    self.extraSymbolData.parameters.Add(esde)
  else
    self.extraSymbolData.locals.Add(esde);

  result:=(self.terminated=false);
  {$ENDIF}
end;

procedure TSymbolloaderthread.EnumerateExtendedDebugSymbols;
var
  i: integer;
  c: IMAGEHLP_STACK_FRAME;
begin
  {$IFNDEF UNIX}
  for i:=0 to self.symbollist.ExtraSymbolDataList.Count-1 do
  begin
    if (not self.symbollist.ExtraSymbolDataList[i].filledin) and (self.symbollist.ExtraSymbolDataList[i].symboladdress<>0) then
    begin
      //get the data

      self.extraSymbolData:=self.symbollist.ExtraSymbolDataList[i];

      ZeroMemory(@c, sizeof(c));
      c.InstructionOffset:=self.extraSymbolData.symboladdress;
      SymSetContext(self.thisprocesshandle, @c, nil);

      SymEnumSymbols(self.thisprocesshandle, 0, nil, @ES2, self);

      self.extraSymbolData.filledin:=true;
    end;
  end;
  {$ENDIF}
end;

function ES(pSymInfo:PSYMBOL_INFO; SymbolSize:ULONG; UserContext:pointer):BOOL;stdcall;
var
  self: TSymbolloaderthread;
  s: string;
  sym: PCESymbolInfo;

  tempstring: pchar;
  x: dword;
  i: integer;

  pSymInfo2:PSYMBOL_INFO;

  ExtraSymbolData: TExtraSymbolData;
begin


  result:=true;
  if pSymInfo.NameLen=0 then
    exit;


  s:=pchar(@pSymInfo.Name);


  self:=TSymbolloaderthread(UserContext);

  if self.currentModuleIsNotStandard then
    s:='_'+s;

  if TSymTagEnum(pSymInfo.Tag)=SymTagFunction then
  begin
    extraSymbolData:=TExtraSymbolData.create;
    self.symbollist.AddExtraSymbolData(extraSymbolData);

    extraSymbolData.symboladdress:=pSymInfo.Address;
    //fill the rest in later
  end
  else
    extraSymbolData:=nil;

  //don't add if it's a forwarder, but register a userdefined symbol

  //if (pSymInfo.Flags and SYMFLAG_FORWARDER=0) then     //Problem: getJIT is marked as Forwarder, but it's not
  begin
    sym:=self.symbollist.AddSymbol(self.currentModuleName, self.currentModuleName+'.'+s, pSymInfo.Address, symbolsize,false, extraSymbolData);
    sym:=self.symbollist.AddSymbol(self.currentModuleName, s, pSymInfo.Address, symbolsize,true, extraSymbolData); //don't add it as a address->string lookup  , (this way it always shows modulename+symbol)
  end;



  result:=not self.terminated;


end;

function ET(pSymInfo:PSYMBOL_INFO; SymbolSize:ULONG; UserContext:pointer):BOOL;stdcall;
begin
  //todo: Add to structure dissect
  result:=true;
end;

var test: boolean;

function EM(ModuleName:PSTR; BaseOfDll:dword64; UserContext:pointer):bool;stdcall;
var self: TSymbolloaderthread;
    mi: tmoduleinfo;
begin
  {$IFNDEF UNIX}
  self:=TSymbolloaderthread(UserContext);
  self.CurrentModulename:=ModuleName;


  if symhandler.getmodulebyaddress(baseofdll, mi) then
    self.currentModuleIsNotStandard:=ProcessHandler.is64Bit<>mi.is64bitmodule
  else
    self.currentModuleIsNotStandard:=false; //whatever...

 // result:=SymEnumTypes(self.thisprocesshandle, baseofdll, @ET, self);

  result:=(self.terminated=false) and (SymEnumSymbols(self.thisprocesshandle, baseofdll, nil, @ES, self));

  //mark this module as loaded

  symhandler.markModuleAsLoaded(baseofdll);
  inc(self.enumeratedModules);

  self.fprogress:=ceil((self.enumeratedModules / self.modulecount) * 100);
  {$ENDIF}
end;


function TSymbolloaderthread.NetworkES(modulename: string; symbolname: string; address: ptruint; size: integer; secondary: boolean): boolean;
begin

  //if highestsymboladdress<address then
  {
  if symbolname='_end' then
  begin
    highestsymboladdress:=address;
    highestsymbol:=symbolname;
  end;
  }

  symbollist.AddSymbol(modulename, modulename+'.'+symbolname, Address, size, secondary);
  symbollist.AddSymbol(modulename, symbolname, Address, size,true);
  result:=not terminated;
end;

procedure TSymbolloaderthread.execute;
type
  TModInfo=record
    baseaddress: qword;
    size: qword;
  end;
  PModInfo=^TModInfo;
var sp: pchar;
    s: string;

    temp: string;

    c: TCEConnection;

    mpl: Tstringlist;
    i,j,k,l,m: integer;

    dotNetdomains: TDotNetDomainArray;
    dotNetmodules: TDotNetModuleArray;
    dotnettypedefs: TDotNetTypeDefArray;
    dotnetmethods: TDotNetMethodArray;

    address:qword;
    size: integer;
    name: string;



    modinfo: PModInfo;
begin

  try
    try
      c:=getConnection;

      SymbolsLoaded:=false;
      symbollist.clear;

      owner.dotnetModuleSymbolListMREW.Beginwrite;   //lock the list
      try
        for i:=0 to length(owner.dotnetModuleSymbolList)-1 do
          owner.dotnetModuleSymbolList[i].symbollist.Free;

        setlength(owner.dotnetModuleSymbolList,0);
      finally
        owner.dotnetModuleSymbolListMREW.Endwrite;
      end;

      {$IFDEF windows}
      if trim(searchpath)='' then
      begin
        s:='';
        temp:=GetEnvironmentVariable('_NT_SYMBOL_PATH' );
        if temp<>'' then
          s:=temp+';';

        temp:=GetEnvironmentVariable('_NT_ALTERNATE_SYMBOL_PATH' );
        if temp<>'' then
          s:=s+temp+';';

        temp:=getProcessPathFromProcessID(thisprocessid);
        if temp<>'' then
          s:=s+ExtractFilePath(temp);

      end
      else
      {$ENDIF}
        s:=searchpath;


      sp:=pchar(s);

      if c=nil then //local
      begin

        {$IFDEF windows}
        if thisprocessid<>GetCurrentProcessId then //I'm quite sure ce isn't written in .net
        begin

          if owner.dotNetDataCollector.Attached then
          begin


            //enum the modulelist
            setlength(dotNetdomains,0);


            owner.dotNetDataCollector.EnumDomains(dotNetdomains);
            for i:=0 to length(dotNetdomains)-1 do
            begin

              if (i=0) and (not terminated) then //get more info
              begin
                setlength(dotNetmodules,0);

                if not terminated then
                begin
                  owner.dotNetDataCollector.EnumModuleList(dotNetdomains[i].hDomain, dotNetmodules);

                  //still here, add the modules
                  owner.dotnetModuleSymbolListMREW.Beginwrite;

                  setlength(owner.dotnetModuleSymbolList, length(dotNetmodules));
                  for j:=0 to length(dotNetmodules)-1 do
                  begin
                    owner.dotnetModuleSymbolList[j].modulename:=dotNetmodules[j].name;
                    owner.dotnetModuleSymbolList[j].modulebase:=dotNetmodules[j].baseaddress;
                    owner.dotnetModuleSymbolList[j].symbollist:=TSymbolListHandler.create;
                  end;
                end;

                owner.dotnetModuleSymbolListMREW.Endwrite;
              end;

              //cleanup
              owner.dotNetDataCollector.ReleaseObject(dotNetdomains[i].hDomain);
            end;

            //enumerate the first module
            if (not terminated) and (length(dotNetmodules)>0) then
            begin
              owner.EnumDotNetModule(dotNetmodules[0], owner.dotnetModuleSymbolList[0].symbollist);
              owner.dotNetDataCollector.ReleaseObject(dotNetmodules[0].hModule); //free this one already
            end;

          end;


        end;

        SymbolsLoaded:=SymInitialize(thisprocesshandle, sp, true);

        if symbolsloaded then
        begin
          symsetoptions(symgetoptions or SYMOPT_CASE_INSENSITIVE);
          symsetsearchpath(processhandle,pchar(searchpath));

          if kernelsymbols then LoadDriverSymbols;

          LoadDLLSymbols;

          //enumerate the basic data from the symbols
          enumeratedModules:=0;
          SymEnumerateModules64(thisprocesshandle, @EM, self );

          apisymbolsloaded:=true;



          if owner.dotNetDataCollector.Attached then
          begin
            fprogress:=50;

            //Enumerate the other .net module methods
            for i:=1 to length(dotNetmodules)-1 do
            begin
              if not terminated then
              begin
                owner.EnumDotNetModule(dotNetmodules[i], owner.dotnetModuleSymbolList[i].symbollist);
                owner.dotNetDataCollector.ReleaseObject(dotNetmodules[i].hModule);
              end;
            end;
          end;

          //enumerate the extended debug symbols

          EnumerateExtendedDebugSymbols;

          Symcleanup(thisprocesshandle);

        end
        else
        {$ENDIF}
          error:=true;
      end
      else
      begin
        //networked
        //get a list of module paths ,fetch the symbols from those modules (if they are indeed modules) and then store them

        mpl:=tstringlist.create;
        self.owner.modulelistMREW.Beginread;
        try
          for i:=0 to self.owner.modulelistpos-1 do
          begin
            getmem(modinfo, sizeof(TModInfo));
            modinfo^.baseaddress:=self.owner.modulelist[i].baseaddress;
            modinfo^.size:=self.owner.modulelist[i].basesize;
            mpl.AddObject(self.owner.modulelist[i].modulepath, tobject(modinfo));
          end;
        finally
          self.owner.modulelistMREW.Endread;
        end;

        modulecount:=mpl.count;
        enumeratedModules:=0;
        for i:=0 to modulecount-1 do
        begin
          modinfo:=pmodinfo(mpl.Objects[i]);


          c.enumSymbolsFromFile(self.owner.modulelist[i].modulepath, modinfo^.baseaddress, NetworkES);

          inc(enumeratedModules);
          fprogress:=ceil((i/modulecount)*100);

          freemem(modinfo);
          modinfo:=nil;
        end;


        mpl.free;
      end;
    finally
      isloading:=false;

      OutputDebugString('symbolloader thread finished');

      owner.ReinitializeUserdefinedSymbolList;


      if not terminated then
      begin
        OutputDebugString('Symbolhandler: sync: Calling finishedloadingsymbols');
        Queue(finishedloadingsymbols);
        //synchronize(finishedloadingsymbols);
        OutputDebugString('after finishedloadingsymbols');
      end
      else
        OutputDebugString('Symbolhandler was terminated. Not going to sync');

    end;

    OutputDebugString('Symbol loader thread has finished without errors');
  except
    outputdebugstring(rsSymbolloaderthreadHasCrashed);
  end;

  isloading:=false;
end;

destructor TSymbolloaderthread.destroy;
begin
  //close the symbol handler for this processhandle

  inherited destroy;
end;

constructor TSymbolloaderthread.create(owner: TSymhandler; targetself, CreateSuspended: boolean);
var
  _processid: dword;
  _processhandle: thandle;
begin
  self.owner:=owner;
  self.targetself:=targetself;
  
  {$ifdef windows}
  if targetself then
  begin
    _processid:=getcurrentprocessid;
    _processhandle:=getcurrentprocess;
  end
  else
  {$endif}
  begin
    _processid:=processhandlerunit.ProcessID;
    _processhandle:=processhandlerunit.ProcessHandle;
  end;


  thisprocesshandle:=_processhandle;
  thisprocessid:=_processid;
  isloading:=true;
  SymbolsLoaded:=false;

  inherited create(CreateSuspended);
end;

//-------------------Symhandler-----------------------

procedure TSymhandler.tokenize(s: string; var tokens: TTokens);
{
Just a tokenizer for simple address specifiers
}
var
  i: integer;
  last: integer;
  t: string;
  inQuote: boolean;
begin
  last:=1;
  inQuote:=false;

  for i:=1 to length(s) do
  begin
    if (s[i] in ['"', '[', ']', '+', '-', '*']) then
    begin
      if s[i]='"' then
      begin
        if not inQuote then
          last:=i+1;

        inQuote:=not inquote;
      end;

      if not inQuote then
      begin
        t:=trim(copy(s, last, i-last));
        if t<>'' then
        begin
          setlength(tokens,length(tokens)+1);
          tokens[length(tokens)-1]:=t;
        end;

        //store seperator char as well, unless it's "
        if s[i]<>'"' then
        begin
          setlength(tokens,length(tokens)+1);
          tokens[length(tokens)-1]:=s[i];
        end;
        last:=i+1;
      end;
    end;


  end;

  //last part
  t:=trim(copy(s, last,length(s)));
  if t<>'' then
  begin
    setlength(tokens,length(tokens)+1);
    tokens[length(tokens)-1]:=t;
  end;
end;

function TSymHandler.getProgress: integer;
begin
  result:=0;
  symbolloadervalid.beginread;
  if symbolloaderthread<>nil then
  begin
    if symbolloaderthread.Finished then
      result:=100
    else
      result:=symbolloaderthread.fprogress;
  end;

  symbolloadervalid.Endread;
end;

function TSymhandler.geterror:boolean;
begin
  symbolloadervalid.beginread;
  if symbolloaderthread<>nil then
    result:=symbolloaderthread.error
  else
    result:=false; //no error

  symbolloadervalid.endread;
end;

function TSymhandler.getDotNetAccess: boolean;
begin
  result:=dotNetDataCollector.Attached;
end;

function TSymhandler.getisloaded:boolean;
begin
  symbolloadervalid.beginread;
  if symbolloaderthread<>nil then
    result:=not symbolloaderthread.isloading
  else
    result:=false;

  symbolloadervalid.endread;
end;

procedure TSymhandler.RegisterUserdefinedSymbolCallback(callback: TUserdefinedSymbolCallback);
begin
  UserdefinedSymbolCallback:=callback;
end;

procedure TSymhandler.setshowmodules(x: boolean);
begin
  if locked then raise symexception.Create(rsYouCanTChangeThisSettingAtTheMoment);
  fshowmodules:=x;
end;

procedure TSymhandler.setshowsymbols(x: boolean);
begin
  if locked then raise symexception.Create(rsYouCanTChangeThisSettingAtTheMoment);
  fshowsymbols:=x;
end;


function TSymhandler.getusedprocessid:dword;
begin
  symbolloadervalid.beginread;
  if symbolloaderthread<>nil then
    result:=symbolloaderthread.thisprocessid
  else
    result:=0;

  symbolloadervalid.endread;
end;

function TSymhandler.getusedprocesshandle:thandle;
begin
  symbolloadervalid.beginread;
  if symbolloaderthread<>nil then
    result:=symbolloaderthread.thisprocesshandle
  else
    result:=0;

  symbolloadervalid.endread;
end;


procedure TSymhandler.EnumDotNetModule(m: TdotNetmodule; symbolhandler: TSymbolListHandler);
{
Enumerates the methods of the given module.
pre: owner must have already set a valid dotNetDataCollector and not about to get destroyed
}
var
  dotNetTypedefs: TDotNetTypeDefArray;
  dotnetmethods: TDotNetMethodArray;
  dotnettypedefsIterator: integer;
  dotnetmethodsIterator: integer;
  i: integer;
  name: string;
  address: qword;
  size: dword;
begin

  if dotNetDataCollector.Attached then
  begin
    setlength(dotNetTypedefs, 0);
    dotNetDataCollector.EnumTypeDefs(m.hModule, dotNetTypedefs );

    for dotnettypedefsIterator:=0 to length(dotnettypedefs)-1 do
    begin
      setlength(dotnetmethods,0);

      dotNetDataCollector.GetTypeDefMethods(m.hModule, dotnettypedefs[dotnettypedefsIterator].token, dotnetmethods);
      for dotnetmethodsIterator:=0 to length(dotnetmethods)-1 do
      begin
        if dotnetmethods[dotnetmethodsIterator].NativeCode<>0 then
        begin
          if length(dotnetmethods[dotnetmethodsIterator].SecondaryNativeCode)>0 then
          begin
            for i:=0 to length(dotnetmethods[dotnetmethodsIterator].SecondaryNativeCode)-1 do
            begin
              address:=dotnetmethods[dotnetmethodsIterator].SecondaryNativeCode[i].address;
              size:=dotnetmethods[dotnetmethodsIterator].SecondaryNativeCode[i].size;
              if i=0 then //first one
                name:=dotnettypedefs[dotnettypedefsIterator].name+'::'+dotnetmethods[dotnetmethodsIterator].name
              else
                name:=dotnettypedefs[dotnettypedefsIterator].name+'::'+dotnetmethods[dotnetmethodsIterator].name+'_'+inttostr(i+1);

              symbolhandler.AddSymbol(m.name, name, address, size);

            end;

          end
          else
            symbolhandler.AddSymbol(m.name, dotnettypedefs[dotnettypedefsIterator].name+'::'+dotnetmethods[dotnetmethodsIterator].name, dotnetmethods[dotnetmethodsIterator].NativeCode, 1)

        end;

        if dotnetmethods[dotnetmethodsIterator].ILCODE<>0 then //this can happen for PINVOKE's methods
          symbolhandler.AddSymbol(m.name, dotnettypedefs[dotnettypedefsIterator].name+'::'+dotnetmethods[dotnetmethodsIterator].name+'_IL', dotnetmethods[dotnetmethodsIterator].ILCODE,1);



      end;
    end;
  end;


end;

procedure TSymhandler.reinitializeDotNetSymbols(modulename: string='');
{
Called by user. This indicates that the user wants to wait till it's fully loaded
}
var
  domainIterator, moduleIterator: integer;

  domains: TDotNetDomainArray;
  modules: TDotNetModuleArray;
  i: integer;

  sh: TSymbolListHandler;
begin
  dotNetDataCollector.disconnect;
  dotNetDataCollector.connect(processid, processhandler.is64Bit);

  try

    dotNetDataCollector.EnumDomains(domains);
    for domainIterator:=0 to length(domains)-1 do
    begin
      dotNetDataCollector.EnumModuleList(domains[domainIterator].hDomain, modules);

      for moduleIterator:=0 to length(modules)-1 do
      begin
        if (modulename='') or (lowercase(ExtractFileName(modules[moduleIterator].name))=lowercase(modulename)) then
        begin
          //add/update the symbols
          sh:=nil;
          dotnetModuleSymbolListMREW.Beginwrite;
          try
            for i:=0 to length(dotnetModuleSymbolList)-1 do
              if modules[moduleIterator].baseaddress=dotnetModuleSymbolList[i].modulebase then
                sh:=dotnetModuleSymbolList[i].symbollist;

            if sh=nil then //not found
            begin
              //add it
              sh:=TSymbolListHandler.create;
              setlength(dotnetModuleSymbolList, length(dotnetModuleSymbolList)+1);
              dotnetModuleSymbolList[length(dotnetModuleSymbolList)-1].modulebase:=modules[moduleIterator].baseaddress;
              dotnetModuleSymbolList[length(dotnetModuleSymbolList)-1].modulename:=modules[moduleIterator].name;
              dotnetModuleSymbolList[length(dotnetModuleSymbolList)-1].symbollist:=sh;
            end;
          finally
            dotnetModuleSymbolListMREW.Endwrite;
          end;

          EnumDotNetModule(modules[moduleIterator], sh);
        end;
        dotNetDataCollector.ReleaseObject(modules[moduleIterator].hModule);
      end;

      dotNetDataCollector.ReleaseObject(domains[domainIterator].hDomain);
    end;

  except
  end;


end;

procedure TSymhandler.reinitialize(force: boolean=false);
begin
  Log('TSymhandler.reinitialize');
  if loadmodulelist or force then //if loadmodulelist returns true it has detected a change in the previous modulelist (baseaddresschange or new/deleted module)
  begin
    if fetchSymbols then
    begin
      Log('loadmodulelist or force was true');
      symbolloadervalid.Beginread;
      try
        if symbolloaderthread<>nil then
          symbolloaderthread.Terminate; //let's get this started
      finally
        symbolloadervalid.Endread;
      end;

      dotNetDataCollector.disconnect;

      if not targetself then
        dotNetDataCollector.connect(processid, processhandler.is64Bit);

      symbolloadervalid.BeginWrite;
      try
        if symbolloaderthread<>nil then
        begin
          symbolloaderthread.Terminate;

          //OutputDebugString(pchar(inttostr(GetCurrentThreadId)+':Waiting'));
          if symbolloaderthread.Finished=false then
            symbolloaderthread.WaitFor; //wait till it's done

          //OutputDebugString(pchar(inttostr(GetCurrentThreadId)+':Returned'));

          freeandnil(symbolloaderthread);
        end;

        symbolloaderthread:=tsymbolloaderthread.Create(self, targetself,true);
        symbolloaderthread.kernelsymbols:=kernelsymbols;
        symbolloaderthread.searchpath:=searchpath;
        symbolloaderthread.symbollist:=symbollist;
        symbolloaderthread.start;
      finally
        symbolloadervalid.EndWrite;
      end;
    end;
  end;

  ReinitializeUserdefinedSymbolList;
end;

procedure TSymhandler.Waitforsymbolsloaded(apisymbolsonly: boolean=false; specificmodule: string='');
var checkcondition: pboolean;
begin
  symbolloadervalid.beginread;

  if symbolloaderthread<>nil then
  begin
    while (not symbolloaderthread.Finished) and (symbolloaderthread.isloading) and
          not
          (
            (apisymbolsonly and symbolloaderthread.apisymbolsloaded) or  //true if all the symbols are loaded
            ((specificmodule<>'') and areSymbolsLoadedForModule(specificModule)) //true if the module's symbols are loaded
          )
    do
    begin
      sleep(25);
      if GetCurrentThreadID = MainThreadID then
        CheckSynchronize;
    end;
  end;

  symbolloadervalid.endread;
end;

procedure TSymhandler.ReinitializeUserdefinedSymbolList;
var i: integer;
 x: qword;
 err: integer;
 haserror: boolean;
begin
  for i:=0 to userdefinedsymbolspos-1 do
  begin
    val('$'+userdefinedsymbols[i].addressstring, x, err);
    if err>0 then //it's not a hexadecimal value
    begin
      x:=getAddressFromName(userdefinedsymbols[i].addressstring, false,haserror);
      if not haserror then
        userdefinedsymbols[i].address:=x;
    end;
  end;
end;


function TSymhandler.DeleteUserdefinedSymbol(symbolname:string):boolean;
var i,j: integer;
begin
  result:=false;
  userdefinedsymbolsCS.enter;
  try
    for i:=0 to userdefinedsymbolspos-1 do
      if uppercase(userdefinedsymbols[i].symbolname)=uppercase(symbolname) then
      begin
        //found it

        {       NO, not anymore
        //check if it had a alloc, if so, free it
        if (userdefinedsymbols[i].allocsize>0) and (userdefinedsymbols[i].processid=processid) then
          VirtualFreeEx(processhandle,pointer(userdefinedsymbols[i].address),0,MEM_RELEASE);}

        //now move up all the others and decrease the list
        for j:=i to userdefinedsymbolspos-2 do
          userdefinedsymbols[j]:=userdefinedsymbols[j+1];

        dec(userdefinedsymbolspos);
        result:=true;
        break;
      end;
  finally
    userdefinedsymbolsCS.leave;
  end;

  if assigned(UserdefinedSymbolCallback) then
    UserdefinedSymbolCallback();
end;

function TSymhandler.SetUserdefinedSymbolAllocSize(symbolname:string; size: dword; preferedaddress: ptruint=0): boolean;
{
This function will find the userdefined symbol, and when found checks if it already
allocated memory. If not allocate memory, else check if the size matches
}
var i:integer;
 base: pointer;
begin
  result:=false;
  if size=0 then raise exception.Create(rsPleaseProvideABiggerSize);

  userdefinedsymbolsCS.enter;
  try
    i:=GetUserdefinedSymbolByNameIndex(symbolname);
    if i=-1 then
    begin

      {userdefinedalloc: pointer; //if set it hold a pointer to the last free memory that was allocated.
      userdefinedallocsizeleft: integer; //defines how much memory was left
      }
      if (globalallocpid<>processid) or (globalalloc=nil) or (globalallocsizeleft<size) then //new alloc
      begin
        base:=FindFreeBlockForRegion(preferedaddress,max(65536,size));

        globalalloc:=virtualallocex(processhandle,base,max(65536,size),MEM_COMMIT or MEM_RESERVE , PAGE_EXECUTE_READWRITE);
        globalallocpid:=processid;
        globalallocsizeleft:=max(65536,size);
      end;

      if globalalloc=nil then
        raise exception.Create(rsErrorAllocatingMemory);
      AddUserdefinedSymbol(inttohex(ptrUint(globalalloc),8),symbolname);
      i:=GetUserdefinedSymbolByNameIndex(symbolname);
      userdefinedsymbols[i].allocsize:=size;
      userdefinedsymbols[i].processid:=processid;


      size:=(size+15) and not $f;
      dec(globalallocsizeleft, size );
      inc(pbyte(globalalloc), size);

    end
    else
    begin
      //it exists, check first
      if (userdefinedsymbols[i].allocsize>0) and (userdefinedsymbols[i].processid=processid) then
      begin
        //already allocated and processid is the same
        if size<>userdefinedsymbols[i].allocsize then
          raise exception.Create(Format(rsTheSymbolNamedWasPreviouslyDeclared, [userdefinedsymbols[i].symbolname, inttostr(userdefinedsymbols[i].allocsize), inttostr(size)]));
      end;

      if userdefinedsymbols[i].processid<>processid then
      begin
        if (globalallocpid<>processid) or (globalalloc=nil) or (globalallocsizeleft<size) then //new alloc
        begin
          globalallocpid:=processid;
          globalalloc:=virtualallocex(processhandle,FindFreeBlockForRegion(preferedaddress,max(65536,size)),max(65536,size),MEM_COMMIT or MEM_RESERVE , PAGE_EXECUTE_READWRITE);
          globalallocsizeleft:=max(65536,size);
        end;

        if globalalloc=nil then
          raise exception.Create(rsErrorAllocatingMemory);

        userdefinedsymbols[i].address:=ptrUint(globalalloc);
        userdefinedsymbols[i].addressstring:=inttohex(ptrUint(globalalloc),8);
        userdefinedsymbols[i].allocsize:=size;
        userdefinedsymbols[i].processid:=processid;

        size:=(size+15) and not $f;
        dec(globalallocsizeleft, size );
        inc(ptrUint(globalalloc), size);
      end;
    end;
  finally
    userdefinedsymbolsCS.leave;
  end;

  result:=true; //managed to get here without crashing...
  if assigned(UserdefinedSymbolCallback) then
     UserdefinedSymbolCallback();


end;

function TSymhandler.GetUserdefinedSymbolByNameIndex(symbolname:string):integer;
var i: integer;
begin
  result:=-1;
  userdefinedsymbolsCS.enter;
  try
    for i:=0 to userdefinedsymbolspos-1 do
    begin
      if uppercase(userdefinedsymbols[i].symbolname)=uppercase(symbolname) then
      begin
        result:=i;
        break;
      end;
    end;
  finally
    userdefinedsymbolsCS.leave;
  end;
end;

function TSymhandler.GetUserdefinedSymbolByAddressIndex(address: ptruint):integer;
var i: integer;
begin
  result:=-1;
  userdefinedsymbolsCS.enter;
  try
    for i:=0 to userdefinedsymbolspos-1 do
      if userdefinedsymbols[i].address=address then
      begin
        result:=i;
        break;
      end;
  finally
    userdefinedsymbolsCS.leave;
  end;  
end;

function TSymhandler.GetUserdefinedSymbolByName(symbolname:string):ptrUint;
var i:integer;
begin
  result:=0;

  userdefinedsymbolsCS.enter;
  try
    i:=GetUserdefinedSymbolByNameIndex(symbolname);
    if i=-1 then exit;
    result:=userdefinedsymbols[i].address;
  finally
    userdefinedsymbolsCS.leave;
  end;
end;

function TSymhandler.GetUserdefinedSymbolByAddress(address:ptrUint):string;
var i:integer;
begin
  result:='';
  userdefinedsymbolsCS.enter;
  try
    i:=GetUserdefinedSymbolByAddressIndex(address);
    if i=-1 then exit;
    result:=userdefinedsymbols[i].symbolname;
  finally
    userdefinedsymbolsCS.leave;
  end;
end;

procedure TSymhandler.AddUserdefinedSymbol(addressstring: string; symbolname: string; DoNotSave: Boolean=false);
{
This routine will add the symbolname+address combination to the symbollist
}
var
  address: ptruint;
begin
  if getuserdefinedsymbolbyname(symbolname)>0 then raise symexception.Create(symbolname+' '+rsAlreadyExists);

  address:=getAddressFromName(addressstring);
  if address=0 then raise symexception.Create(rsYouCanTAddASymbolWithAddress0);

  userdefinedsymbolsCS.enter;
  try
    if userdefinedsymbolspos+1>=length(userdefinedsymbols) then
      setlength(userdefinedsymbols,length(userdefinedsymbols)*2);

    userdefinedsymbols[userdefinedsymbolspos].address:=address;
    userdefinedsymbols[userdefinedsymbolspos].addressstring:=addressstring;
    userdefinedsymbols[userdefinedsymbolspos].symbolname:=symbolname;
    userdefinedsymbols[userdefinedsymbolspos].allocsize:=0;
    userdefinedsymbols[userdefinedsymbolspos].processid:=0;
    userdefinedsymbols[userdefinedsymbolspos].doNotSave:=DoNotSave;
    inc(userdefinedsymbolspos);
  finally
    userdefinedsymbolsCS.leave;
  end;


  if assigned(UserdefinedSymbolCallback) then
    UserdefinedSymbolCallback();
end;

procedure TSymhandler.EnumerateUserdefinedSymbols(list:tstrings);
{
Enumerates all userdefined symbols and stores them in a list
NOTE: The caller must free the object info added
}

var i: integer;
    extradata: ^TUDSEnum;
begin
  list.Clear;
  userdefinedsymbolsCS.enter;
  for i:=0 to userdefinedsymbolspos-1 do
  begin
    getmem(extradata,sizeof(TUDSEnum));
    extradata.address:=userdefinedsymbols[i].address;
    extradata.allocsize:=userdefinedsymbols[i].allocsize;
    extradata.addressstring:=@userdefinedsymbols[i].addressstring[1];
    extradata.doNotSave:=userdefinedsymbols[i].doNotSave;

    list.Addobject(userdefinedsymbols[i].symbolname,pointer(extradata));
    //just don't forget to free it at the caller's end
  end;
  userdefinedsymbolsCS.leave;
end;

procedure TSymhandler.fillMemoryRegionsWithModuleData(var mr: TMemoryregions; startaddress: ptruint; size: dword);
{
This routine will fill in a TMemoryRegions array with the base and startaddress of the modules it found
}
var currentaddress: ptruint;
    mi: tmoduleinfo;
    sizeleft: dword;
    i: integer;
    closest: integer;
begin
  modulelistMREW.beginread;
  try
    if modulelistpos=0 then exit;

    currentaddress:=startaddress;
    sizeleft:=size;

    while sizeleft>0 do
    begin
      //find a module with currentaddress if nothing found, find the one with the lowest base address after it
      if getmodulebyaddress(currentaddress,mi) then
      begin
        setlength(mr,length(mr)+1);

        mr[length(mr)-1].BaseAddress:=currentaddress;
        mr[length(mr)-1].MemorySize:=mi.basesize-(currentaddress-mi.baseaddress);

        if mr[length(mr)-1].MemorySize>sizeleft then
          mr[length(mr)-1].MemorySize:=sizeleft;

        sizeleft:=sizeleft-mr[length(mr)-1].MemorySize;
        inc(currentaddress,mr[length(mr)-1].MemorySize);
      end
      else
      begin
        //move the currentaddress to the next module
        closest:=-1;
        for i:=0 to modulelistpos-1 do
        begin
          if modulelist[i].baseaddress>currentaddress then
          begin
            closest:=i;
            break;
          end;
        end;

        //first make sure there is a bigger module


        for i:=0 to modulelistpos-1 do
          if (modulelist[i].baseaddress>currentaddress) and (modulelist[i].baseaddress<modulelist[closest].baseaddress) then
            closest:=i;

        if modulelist[closest].baseaddress<currentaddress then exit; //nothing found

        mi:=modulelist[closest];
        inc(sizeleft,mi.baseaddress-currentaddress);
        currentaddress:=mi.baseaddress;
      end;
    end;

  finally
    modulelistMREW.endread;
  end;
end;

function TSymhandler.getSymbolInfo(name: string; var syminfo: TCESymbolInfo): boolean;
var s: PCESymbolInfo;
    i: integer;
begin
  //find the symbol
  result:=false;

  //first check .net
  if dotNetDataCollector.Attached then
  begin
    dotnetModuleSymbolListMREW.beginread;
    try
      for i:=0 to length(dotnetModuleSymbolList)-1 do
      begin
        s:=dotnetModuleSymbolList[i].symbollist.FindSymbol(name);

        if s<>nil then
        begin
          syminfo:=s^;
          result:=true;
          exit;
        end;
      end;

    finally
      dotnetModuleSymbolListMREW.endread;
    end;
  end;

  //then check secondary symbollists
  symbollistsMREW.Beginread;
  try
    for i:=0 to length(symbollists)-1 do
    begin
      s:=symbollists[i].FindSymbol(name);

      if s<>nil then
      begin
        syminfo:=s^;
        result:=true;
        exit;
      end;
    end;
  finally
    symbollistsMREW.Endread;
  end;

  //and finally check the default symbol list
  s:=symbollist.FindSymbol(name);
  if s<>nil then
  begin
    syminfo:=s^;
    result:=true;
  end;
end;


procedure TSymhandler.GetSymbolList(address: ptruint; list: tstrings);
var si: PCESymbolInfo;
    mi: TModuleInfo;
    symbolname: string;
    i: integer;
    params: string;
begin
  list.clear;
  if getmodulebyaddress(address, mi) then
  begin

    if dotNetDataCollector.Attached then
    begin
      //get the .net list if symbols for this module
      dotnetModuleSymbolListMREW.beginread;
      for i:=0 to length(dotnetModuleSymbolList)-1 do
      begin
        if dotnetModuleSymbolList[i].modulebase=mi.baseaddress then
        begin
          si:=dotnetModuleSymbolList[i].symbollist.FindFirstSymbolFromBase(0);
          while si<>nil do
          begin
            symbolname:=si.originalstring;

            list.AddObject(symbolname, pointer(si.address));
            si:=si.next;
          end;
          break;
        end;
      end;

      dotnetModuleSymbolListMREW.endread;
    end;



    symbollistsMREW.Beginread;
    for i:=0 to length(symbollists)-1 do
    begin
      si:=symbollist.FindFirstSymbolFromBase(mi.baseaddress);
      while (si<>nil) and inrangeq(si.address, mi.baseaddress, mi.baseaddress+mi.basesize) do
      begin
        symbolname:=si.originalstring;

        list.AddObject(symbolname, pointer(si.address));
        si:=si.next;
      end;
    end;
    symbollistsMREW.Endread;

    si:=symbollist.FindFirstSymbolFromBase(mi.baseaddress);
    while (si<>nil) and inrangeq(si.address, mi.baseaddress, mi.baseaddress+mi.basesize) do
    begin
      symbolname:=si.originalstring;
      if si.extra<>nil then
      begin
        //add the parameters if there are any
        params:='';
        for i:=0 to si.extra.parameters.Count-1 do
        begin
          if i>0 then
            params:=params+', '+ si.extra.parameters[i].vtype+' '+si.extra.parameters[i].name
          else
            params:=params+si.extra.parameters[i].vtype+' '+si.extra.parameters[i].name;
        end;
        symbolname:=symbolname+'('+params+')';
      end;

      list.AddObject(symbolname, pointer(si.address));
      si:=si.next;
    end;

  end;
end;

procedure TSymhandler.getModuleList(list: tstrings);
var i: integer;
begin
  modulelistMREW.BeginRead;
  for i:=0 to modulelistpos-1 do
    list.AddObject(modulelist[i].modulename,tobject(modulelist[i].baseaddress));


  modulelistMREW.EndRead;
end;

function TSymhandler.inSystemModule(address: ptrUint): BOOLEAN;
var mi: TModuleInfo;
begin
  result:=false;
  if getmodulebyaddress(address,mi) then
    result:=mi.isSystemModule;
end;

function TSymhandler.inModule(address: ptrUint): BOOLEAN; //returns true if the given address is part of a module
var mi: TModuleInfo;
begin
  result:=getmodulebyaddress(address,mi);
end;

function TSymhandler.areSymbolsLoadedForModule(symbolname: string): boolean;
var mi: TModuleInfo;
begin
  if getmodulebyname(symbolname,mi) then
    result:=mi.symbolsLoaded
  else
    result:=false;
end;

procedure TSymhandler.markModuleAsLoaded(address: ptruint);
var i: integer;
begin
  modulelistMREW.beginread;
  for i:=0 to modulelistpos-1 do
  begin
    if (address>=modulelist[i].baseaddress) and (address<modulelist[i].baseaddress+modulelist[i].basesize) then
    begin
      modulelist[i].symbolsLoaded:=true;
      break;
    end;
  end;
  modulelistMREW.endread;
end;

function TSymhandler.getmodulebyaddress(address: ptrUint; var mi: TModuleInfo):BOOLEAN;
var i: integer;
begin
  result:=false;
  modulelistMREW.beginread;
  for i:=0 to modulelistpos-1 do
    if (address>=modulelist[i].baseaddress) and (address<modulelist[i].baseaddress+modulelist[i].basesize) then
    begin
      mi:=modulelist[i];

      result:=true;
      break;
    end;
  modulelistMREW.endread;
end;

function TSymhandler.getmodulebyname(modulename: string; var mi: TModuleInfo):BOOLEAN;
var
  i: integer;
  moduleNameToFind: string;
  currentModuleName: string;
begin
  result:=false;
  moduleNameToFind:=uppercase(modulename);

  if (length(moduleNameToFind)>0) and (moduleNameToFind[1]='"') then
  begin
    moduleNameToFind:=trim(moduleNameToFind);
    moduleNameToFind:=copy(moduleNameToFind, 2, length(moduleNameToFind)-2);
  end;


  modulelistMREW.beginread;

  for i:=0 to modulelistpos-1 do
  begin
    currentModuleName:=uppercase(modulelist[i].modulename);
    if currentModuleName=moduleNameToFind then
    begin
      mi:=modulelist[i];
      result:=true;
      break;
    end;
  end;
  modulelistMREW.endread;
end;

function TSymHandler.getsearchpath:string;
begin
  result:=searchpath;
end;

procedure TSymHandler.setsearchpath(path:string);
begin
  searchpath:=path;
end;

function TSymhandler.getExtraDataFromSymbolAtAddress(address: ptruint): TExtraSymbolData;
//returns the extra data for a symbol (can be nil)
var si: PCESymbolInfo;
begin
  result:=nil;

  symbolloadervalid.beginread;
  si:=symbollist.FindAddress(address);
  if si<>nil then
    result:=si.extra;
  symbolloadervalid.Endread;

  if (result<>nil) and (not result.filledin) then //Not yet loaded
    result:=nil;
end;

function TSymhandler.getNameFromAddress(address:ptrUint;symbols:boolean; modules: boolean; baseaddress: PUINT64=nil; found: PBoolean=nil; hexcharsize: integer=8):string;
var //symbol :PSYMBOL_INFO;
    offset: qword;
    s: string;
    mi: tmoduleinfo;
    si: PCESymbolInfo;
    processhandle: thandle;
    i: integer;
begin
{$ifdef windows}
  if targetself then
  begin
    processhandle:=getcurrentprocess;
  end
  else
{$endif}
  begin
    processhandle:=processhandlerunit.ProcessHandle;
  end;


  for i:=0 to length(AddressLookupCallbacks)-1 do
  begin
    if assigned(AddressLookupCallbacks[i]) then
    begin
      result:=AddressLookupCallbacks[i](address);
      if result<>'' then
        exit;
    end;
  end;


  //check the userdefined symbols
  if found<>nil then
    found^:=false;


  result:=self.GetUserdefinedSymbolByAddress(address);
  if result<>'' then exit;


  if symbols then
  begin
    //first see if it is a symbol
    symbolloadervalid.beginread;
    try
      if (symbolloaderthread<>nil) then
      begin
        //if isloaded then
        begin
          si:=nil;
          if dotNetDataCollector.Attached then
          begin
            dotnetModuleSymbolListMREW.beginread;
            for i:=0 to length(dotnetModuleSymbolList)-1 do
            begin
              si:=dotnetModuleSymbolList[i].symbollist.FindAddress(address);
              if si<>nil then break;
            end;

            dotnetModuleSymbolListMREW.endread;
          end;

          if si=nil then
          begin
            //check the symbollists registered by the user
            symbollistsMREW.Beginread;
            for i:=0 to length(symbollists)-1 do
            begin
              si:=symbollists[i].FindAddress(address);
              if si<>nil then break;
            end;

            symbollistsMREW.endread;

            if si=nil then
              si:=symbollist.FindAddress(address);
          end;


          if si<>nil then
          begin
            offset:=address-si.address;

            result:=si.originalstring;

            if offset>0 then  //unsigned, always bigger
              result:=result+'+'+inttohex(offset,1);

            if baseaddress<>nil then
              baseaddress^:=si.Address;

            if found<>nil then
              found^:=true;



            exit;
          end;

        end;

      end;
    finally
      symbolloadervalid.endread;
    end;


  end;






  if modules then
  begin

    //get the dllname+offset
    if getmodulebyaddress(address,mi) then
    begin
      if address-mi.baseaddress=0 then
        result:=mi.modulename
      else
        result:=mi.modulename+'+'+inttohex(address-mi.baseaddress,1);




      if baseaddress<>nil then
        baseaddress^:=mi.baseaddress;

      if found<>nil then
        found^:=true;
      exit;
    end;
  end;

  result:=inttohex(address,hexcharsize);  //default

end;

function TSymhandler.getNameFromAddress(address:ptrUint; var found: boolean; hexcharsize: integer=8):string;
begin
  result:=getNameFromAddress(address,self.showsymbols,self.showmodules,nil,@found,hexcharsize);
end;

function TSymhandler.getNameFromAddress(address:ptrUint):string;
begin
  result:=getNameFromAddress(address,self.showsymbols,self.showmodules);
end;




function TSymhandler.getAddressFromNameL(name: string):ptrUint;  //Lua
var e: boolean;
begin
  result:=getAddressFromName(name, true, e);
  if e then
  begin
    if ExceptionOnLuaLookup then
      raise symexception.Create(Format(rsFailureDeterminingWhatMeans, [name]))
    else
      result:=0;
  end;
end;

function TSymhandler.getAddressFromName(name:string):ptrUint;
begin
  result:=getAddressFromName(name,true);
end;

function TSymhandler.getAddressFromName(name: string; waitforsymbols: boolean): ptrUint;
var x: boolean;
begin
  result:=getAddressFromName(name,true,x,nil);
  {
  debugger hell:
  tools->debugger options->Language Exceptions
  click add...
  type in "symexception" without the quotes

  this will cause you to still break on normal exception like memory access violations, but not on these
  }

  if x then
    raise symexception.Create(Format(rsFailureDeterminingWhatMeans, [name]));
end;

function TSymhandler.getAddressFromName(name: string; waitforsymbols: boolean; out haserror: boolean):ptrUint;
begin
  result:=getAddressFromName(name, waitforsymbols, haserror,nil);
  if haserror then result:=0;
end;

function TSymhandler.getAddressFromName(name: string; waitforsymbols: boolean; out haserror: boolean; context: PContext):ptrUint;
  function callbackCheck(s: string; cbType: TSymbolLookupCallbackPoint): ptruint;
  var slcindex: integer;
  begin
    result:=0;
    for slcindex:=0 to length(SymbolLookupCallbacks[cbType])-1 do
    begin
      if assigned(SymbolLookupCallbacks[cbType][slcindex]) then
      begin
        result:=SymbolLookupCallbacks[cbType][slcindex](s);
        if result<>0 then exit;
      end;
    end;
  end;

type TCalculation=(calcAddition, calcSubstraction);
var mi: tmoduleinfo;
    si: PCESymbolInfo;
    offset: integer;
    i,j: integer;

    slcindex: integer;

    p: pchar;
    ws: widestring;
    pws: pwidechar;
    error: boolean;

    processhandle: thandle;

    tokens: TTokens;
    mathstring: string;
    hasMultiplication, hasPointer: boolean;

    nextoperation: TCalculation;
    regnr: integer;

    //symbol: PSYMBOL_INFO;
    s: string;
    a: ptruint;
begin
  name:=trim(name);
  hasPointer:=false;
  haserror:=false;
  offset:=0;

  result:=callbackCheck(name, slStart);
  if result<>0 then exit;


  //check if it's a lua symbol notation ('$')
  if length(name)>=2 then
  begin
    if name[1]='$' then
    begin
      val(name,result,i);
      if i=0 then exit; //it's a hexadecimal string starting with a $



      {$IFNDEF UNIX}
      //check if lua thingy
      i:=lua_gettop(luavm); //make sure the stack ends here when done

      s:=copy(name, 2, length(name));
      lua_getglobal(LuaVM,pchar(s));

      if i<>lua_gettop(luavm) then
      begin
        j:=lua_type(LuaVM, -1);
        if j=0 then beep;




        if lua_isuserdata(LuaVM, -1) then
        begin
          result:=ptruint(lua_toceuserdata(Luavm, -1));
          lua_settop(luavm, i);
          exit;
        end
        else
        if (j<>LUA_TSTRING) and (lua_isnumber(LuaVM, -1)) then
        begin
          result:=lua_tointeger(LuaVM, -1);
          lua_settop(luavm, i);
          exit;
        end
        else
        if lua_isstring(LuaVM, -1) then
        begin
          p:=lua_tostring(LuaVM, -1);
          if (p<>nil) then name:=p;
        end;



        lua_settop(luavm, i);
      end;
      {$ENDIF}




    end;
  end;

  {$ifdef windows}
  if targetself then
  begin
    processhandle:=getcurrentprocess;
  end
  else
  {$endif}
  begin
    processhandle:=processhandlerunit.ProcessHandle;
  end;



  val('$'+name,result,i);
  if i=0 then exit; //it's a valid hexadecimal string

  if lowercase(copy(name,1,2))='0x' then
  begin
    val(name,result,i);
    if i=0 then exit;
  end;


  //not a hexadecimal string
  result:=callbackCheck(name, slNotInt);
  if result<>0 then exit;


  tokenize(name, tokens);

  //first check the most basic thing
  if length(tokens)=0 then
  begin
    haserror:=true;
    exit;
  end;

  //if it starts with a * or ends with *, - or +, then it's a bad formula
  if (tokens[0][1] ='*') or (tokens[length(tokens)-1][1] in ['*','+','-']) then
  begin
    haserror:=true;
    exit;
  end;

  //convert the tokens into hexadecimal values

  symbolloadervalid.beginread;
  try

    for i:=0 to length(tokens)-1 do
    begin
      if not (tokens[i][1] in ['[',']','+','-','*']) then
      begin
        val('$'+tokens[i],result,j);
        if j>0 then
        begin
          //not a hexadecimal value



          if getmodulebyname(tokens[i],mi) then
          begin
            tokens[i]:=inttohex(mi.baseaddress,8);
            continue;
          end
          else
          begin
            //not a modulename
            result:=callbackCheck(tokens[i], slNotModule);
            if result>0 then
            begin
              tokens[i]:=inttohex(result,8);
              continue;
            end;

            {$IFDEF windows}
            regnr:=getreg(uppercase(tokens[i]),false);

            if regnr<>-1 then
            begin
              if (context<>nil) and (context^.{$ifdef cpu64}Rip{$else}Eip{$endif}<>0) then
              begin
                //get the register value, and because this is an address specifier, use the full 32-bits

                case regnr of
                  0: tokens[i]:=inttohex(context^.{$ifdef cpu64}rax{$else}eax{$endif},8);
                  1: tokens[i]:=inttohex(context^.{$ifdef cpu64}rcx{$else}ecx{$endif},8);
                  2: tokens[i]:=inttohex(context^.{$ifdef cpu64}rdx{$else}edx{$endif},8);
                  3: tokens[i]:=inttohex(context^.{$ifdef cpu64}rbx{$else}ebx{$endif},8);
                  4: tokens[i]:=inttohex(context^.{$ifdef cpu64}rsp{$else}esp{$endif},8);
                  5: tokens[i]:=inttohex(context^.{$ifdef cpu64}rbp{$else}ebp{$endif},8);
                  6: tokens[i]:=inttohex(context^.{$ifdef cpu64}rsi{$else}esi{$endif},8);
                  7: tokens[i]:=inttohex(context^.{$ifdef cpu64}rdi{$else}edi{$endif},8);
                  {$ifdef cpu64}
                  8: tokens[i]:=inttohex(context^.r8,8);
                  9: tokens[i]:=inttohex(context^.r9,8);
                  10: tokens[i]:=inttohex(context^.r10,8);
                  11: tokens[i]:=inttohex(context^.r11,8);
                  12: tokens[i]:=inttohex(context^.r12,8);
                  13: tokens[i]:=inttohex(context^.r13,8);
                  14: tokens[i]:=inttohex(context^.r14,8);
                  15: tokens[i]:=inttohex(context^.r15,8);
                  {$endif}
                end;

                continue; //handled
              end;

              //not handled, but since it's a register, quit now
            end
            else
            {$ENDIF}
            begin
              //no context or not a register
              result:=GetUserdefinedSymbolByName(tokens[i]);
              if result>0 then
              begin
                tokens[i]:=inttohex(result,8);
                continue;
              end;

              //not a userdefined symbol
              result:=callbackCheck(tokens[i], slNotUserdefinedSymbol);
              if result>0 then
              begin
                tokens[i]:=inttohex(result,8);
                continue;
              end;

              {$ifdef windows}
              if (DBKLoaded) and (length(tokens[i])>6) and (pos('KERNEL_',uppercase(tokens[i]))>0) then
              begin
                tokens[i]:=copy(tokens[i],8,length(tokens[i])-7);
                ws:=tokens[i];
                pws:=@ws[1];
                result:=ptrUint(GetKProcAddress(pws));
                if result<>0 then
                begin
                  tokens[i]:=inttohex(result,8);
                  continue;
                end;
              end;
              //not a kernel symbol
              {$endif}

              //check the symbols
//              if (symbolloaderthread<>nil) then

              if (symbolloaderthread<>nil) then
              begin

                //it's not a valid address, it's not a calculation, it's not a modulename+offset, so lets see if it's a symbol


                //check if it's in
                tokens[i]:=StringReplace(tokens[i],'!','.',[]);

                si:=nil;
                if dotNetDataCollector.Attached then
                begin
                  //check the dotnet list first. (if it's a .net process it's more likely the user wants .net stuff)
                  dotnetModuleSymbolListMREW.beginread;
                  for j:=0 to length(dotnetModuleSymbolList)-1 do
                  begin
                    si:=dotnetModuleSymbolList[j].symbollist.FindSymbol(tokens[i]);
                    if si<>nil then break;
                  end;

                  dotnetModuleSymbolListMREW.Endread;
                end;

                if si=nil then
                begin
                  symbollistsMREW.Beginread;
                  for j:=0 to length(symbollists)-1 do
                  begin
                    si:=symbollists[j].FindSymbol(tokens[i]);
                    if si<>nil then break;
                  end;

                  symbollistsMREW.EndRead;
                  if si=nil then
                    si:=symbollist.FindSymbol(tokens[i]);
                end;

                if si=nil then //not found
                begin
                  {$ifdef windows}
                  if uppercase(copy(tokens[i],1,11))='THREADSTACK' then
                  begin
                    s:=copy(tokens[i], 12, length(tokens[i])-12+1);
                    if s='' then s:='0';

                    if TryStrToInt(s, j) then
                    begin
                      a:=GetStackStart(j);
                      if a<>0 then
                      begin
                        tokens[i]:=inttohex(a,8);
                        continue;
                      end;
                    end;
                  end;
                  {$endif}

                  if waitforsymbols then
                  begin
                    symbolloaderthread.WaitFor;

                    //check again now that the symbols are loaded
                    si:=symbollist.FindSymbol(tokens[i]);
                  end;

                  if si=nil then //STILL not found. Check if it's a structure.element notation
                  begin

                    if LookupStructureOffset(tokens[i], offset) then
                    begin
                      tokens[i]:=inttohex(offset,8);
                      continue;
                    end;

                  end;
                end;

                if si<>nil then
                begin
                  tokens[i]:=inttohex(si.address,8);
                  continue;
                end;




              end;
            end;

            //not a register or symbol
            result:=callbackCheck(tokens[i], slNotSymbol);
            if result>0 then
            begin
              tokens[i]:=inttohex(result,8);
              continue;
            end;


            //One last attempt to fix it, check if it is the last symbol, if not add it. (perhaps the symbol got split into tokens)
            if i<length(tokens)-1 then
            begin
              tokens[i+1]:=tokens[i]+tokens[i+1]; //(if not, it will error out eventually anyhow)
              tokens[i]:=''; //empty
            end
            else
            begin
              //failure
              result:=callbackCheck(tokens[i], slFailure);
              if result>0 then
              begin
                tokens[i]:=inttohex(result,8);
                continue;
              end;

              haserror:=true;
              exit;
            end;

          end;
        end;
      end
      else
      begin
        //it's not a real token
        case tokens[i][1] of
          '*' : hasMultiplication:=true;
          '[',']': hasPointer:=true;
        end;
      end;
    end;

  finally
    symbolloadervalid.endread;
  end;


  mathstring:='';
  for i:=0 to length(tokens)-1 do
    mathstring:=mathstring+tokens[i];

  if haspointer then
  begin
    result:=GetAddressFromPointer(mathstring,haserror);
    exit;
  end;


  //handle the mathstring
  try
    if hasmultiplication then
    begin
      //first do the multiplications
      for i:=0 to length(tokens)-1 do
      begin
        if tokens[i]='*' then
        begin
          //multiply the left and right
          tokens[i-1]:=inttohex(StrToQWordEx('$'+tokens[i-1])*strtoint64('$'+tokens[i+1]),8);
          tokens[i]:='';
          tokens[i+1]:='';
        end;
      end;

    end;

    result:=0;
    //handle addition and subtraction
    nextoperation:=calcAddition;
    for i:=0 to length(tokens)-1 do
    begin
      if length(tokens[i])>0 then
      begin
        case tokens[i][1] of
          '+' : nextoperation:=calcAddition;
          '-' :
          begin
            if nextoperation=calcSubstraction then
              nextoperation:=calcAddition else //--=+
              nextoperation:=calcSubstraction;
          end;

          else
          begin
            //do the calculation
            case nextoperation of
              calcAddition:
                result:=result+StrToQWordEx('$'+tokens[i]);

              calcSubstraction:
                result:=result-StrToQWordEx('$'+tokens[i]);

            end;

            nextoperation:=calcAddition;

          end;
        end;

      end;
    end;

  except
    hasError:=true;
  end;

end;

function TSymhandler.GetLayoutFromAddress(address: ptruint; var addressdata: TAddressData): boolean;
begin
  result:=false;
  if hasDotNetAccess then
  begin
    try
      if dotNetDataCollector.Attached then
        dotNetDataCollector.GetAddressData(address, addressdata);

      result:=addressdata.startaddress<>0;
    except
      freeandnil(dotNetDataCollector);
    end;
  end;
end;

function TSymhandler.loadmodulelist: boolean;  //todo: change to a quicker lookup kind of storage (tree)
var
  ths: thandle;
  me32:MODULEENTRY32;
  s: string;
  x: string;

  i: integer;

  processid: dword;
  modulename: string;

  alreadyInTheList: boolean;

 // oldmodulelist: array of qword;

  is64bitprocess: boolean;


  newmodulelist: TModuleInfoArray;
  newmodulelistpos: integer;
begin


  result:=false;
  is64bitprocess:=processhandler.is64Bit;

  ZeroMemory(@me32, sizeof(MODULEENTRY32));

  try

    if targetself then
      processid:=getcurrentprocessid
    else
      processid:=processhandlerunit.ProcessID;

    if processid=0 then exit;

    {
    modulelistMREW.beginread;

    //make a copy of the old list addresses to compare against
    setlength(oldmodulelist, modulelistpos);
    for i:=0 to modulelistpos-1 do
      oldmodulelist[i]:=modulelist[i].baseaddress;

    modulelistMREW.Endread;  }


    //Note: Just TH32CS_SNAPMODULE32 will result in an empty list
    //Just TH32CS_SNAPMODULE only returns the 64-bit modules
    //There doesn't seem to be a way to make two lists, 32-bit, then 64-bit, and combine them afterwards
    //So for now I just check if it's a system dll, and if so, if it's in the wow64 folder or not
    ths:=CreateToolhelp32Snapshot(TH32CS_SNAPMODULE or TH32CS_SNAPMODULE32,processid);

    modulelistMREW.BeginRead;
    try
      newmodulelistpos:=0;
      setlength(newmodulelist, length(modulelist));

      if ths<>0 then
      begin
        me32.dwSize:=sizeof(MODULEENTRY32);
        if ths<>0 then
        begin
          try
            if module32first(ths,me32) then
            repeat
              s:=WinCPToUTF8(pchar(@me32.szExePath[0]));
              x:=s;
              if (s[1]<>'[') then //do not extract the filename if it's a 'special' marker
                modulename:=extractfilename(s)
              else
                modulename:=s;


              alreadyInTheList:=false;
              //check if this modulename is already in the list, and if so check if it's the same base, else add it
              for i:=0 to newmodulelistpos-1 do
              begin
                if (newmodulelist[i].baseaddress=ptrUint(me32.modBaseAddr)) then
                begin
                  alreadyInTheList:=true;
                  break; //it's in the list, no need to continue looking, break out of the for loop
                end;

              end;

              if not alreadyInTheList then
              begin
                if newmodulelistpos+1>=length(newmodulelist) then
                  setlength(newmodulelist,length(newmodulelist)*2);

                newmodulelist[newmodulelistpos].modulename:=modulename;
                newmodulelist[newmodulelistpos].modulepath:=x;

                //all windows folder files are system modules, except when it is an .exe (minesweeper in xp)
                newmodulelist[newmodulelistpos].isSystemModule:=(pos(lowercase(windowsdir),lowercase(x))>0) and (ExtractFileExt(lowercase(x))<>'.exe');

                newmodulelist[newmodulelistpos].baseaddress:=ptrUint(me32.modBaseAddr);
                newmodulelist[newmodulelistpos].basesize:=me32.modBaseSize;

                if not processhandler.isNetwork then
                begin
                  {$ifdef windows}
                  if peinfo_is64bitfile(x, newmodulelist[newmodulelistpos].is64bitmodule)=false then
                  begin
                    //fallback
                    {$ifdef cpu64}
                    if is64bitprocess then
                      newmodulelist[newmodulelistpos].is64bitmodule:=true
                    else
                    begin
                      if newmodulelist[newmodulelistpos].isSystemModule then
                      begin
                        if pos('wow64', lowercase(ExtractFilePath(x)))>0 then  //todo: Open the file and check if it's 64-bit or not
                          newmodulelist[newmodulelistpos].is64bitmodule:=false
                        else
                          newmodulelist[newmodulelistpos].is64bitmodule:=true;
                      end;
                    end;
                    {$endif}
                  end;
                  {$endif}
                end
                else
                begin
                  newmodulelist[newmodulelistpos].is64bitmodule:=processhandler.is64Bit;
                  if pos('/system/',lowercase(x))=1 then //android thingy
                    newmodulelist[newmodulelistpos].isSystemModule:=true;
                end;

                if (not newmodulelist[newmodulelistpos].isSystemModule) and (commonModuleList<>nil) then //check if it's a common module (e.g nvidia physx dll's)
                  newmodulelist[newmodulelistpos].isSystemModule:=commonModuleList.IndexOf(lowercase(newmodulelist[newmodulelistpos].modulename))<>-1;

                inc(newmodulelistpos);

                if (modulelistpos=0) or (newmodulelistpos>modulelistpos) or (modulelist[newmodulelistpos-1].baseaddress<>newmodulelist[newmodulelistpos-1].baseaddress) then //different size or different order, return true
                  result:=true; //different
              end;

            until not module32next(ths,me32);
          finally
            closehandle(ths);
          end;
        end;
      end;



    finally
      modulelistmrew.EndRead;

    end;

    if newmodulelistpos<>modulelistpos then
      result:=true;


    {begin
      for i:=0 to modulelistpos-1 do
      begin
        if oldmodulelist[i]<>modulelist[i].baseaddress then
        begin
          //the order changed
          result:=true;
          break;
        end;
      end;
    end
    else
      result:=true; //the length of the list changed}

    if result=true then
    begin
      modulelistMREW.BeginWrite;
      try
        modulelist:=newmodulelist;
        modulelistpos:=newmodulelistpos;
      finally
        modulelistMREW.EndWrite;
      end;
    end;

  except
    //MessageBox(0,'procedure TSymhandler.loadmodulelist','procedure TSymhandler.loadmodulelist',0);
  end;


{$ifndef unix}
  if result then
    reinitializeDisassemblerComments; //the comments list is depending on the modulelist since it is written using modulename+offset

{$endif}
end;


function TSymhandler.LookupStructureOffset(s: string; out offset: integer): boolean;
//will search all defines structures and when found return the offset in "offset"
//if found, return true, else false
var
  i,j: integer;
  structurename: string;
  elementname: string;
begin
  result:=false;

  {$IFNDEF UNIX}
  i:=pos('.', s); //check for the .
  if i>0 then
  begin
    //it has one
    structurename:=copy(s, 1, i-1);
    elementname:=copy(s, i+1, length(s)); //just the rest

    if (trim(structurename)<>'') and (trim(elementname)<>'') then
    begin
      structurename:=uppercase(structurename);
      elementname:=uppercase(elementname);

      for i:=0 to DissectedStructs.count-1 do
      begin
        if uppercase(TDissectedStruct(DissectedStructs[i]).name)=structurename then
        begin
          //found the structure
          for j:=0 to TDissectedStruct(DissectedStructs[i]).count-1 do
          begin
            if uppercase(TDissectedStruct(DissectedStructs[i]).element[j].Name)=elementname then
            begin
              //found the element
              offset:=TDissectedStruct(DissectedStructs[i]).element[j].Offset;
              result:=true;
              break;
            end;
          end;
          break;
        end;
      end;
    end;
  end;
  {$ENDIF}
end;

function TSymhandler.GetAddressFromPointer(s: string; var error: boolean):ptrUint;
{
Will return the address of a pointer noted as [[[xxx+xx]+xx]+xx]+xx
If it is a invalid pointer, or can not be resolved, the result is NULL 
}
var i: integer;
    list: tstringlist;
    offsets: array of integer;
    baseaddress: ptruint;
    off: string;
    realaddress, realaddress2: ptrUint;
    check: boolean;
    count: PtrUInt;
begin
  result:=0;
  error:=true;

  list:=tstringlist.create;
  try
    if not ParseAsPointer(s,list) then exit;

    try
      baseaddress:=getaddressfromname(list[0]);
    except
      exit;
    end;

    setlength(offsets,list.count-1);
    for i:=1 to list.Count-1 do //start from the first offset
    begin
      off:=copy(list[i],2,length(list[i]));
      try
        offsets[i-1]:=StrToQWordEx('$'+off);
      except
        exit;
      end;
      if list[i][1]='-' then
        offsets[i-1]:=-offsets[i-1];
    end;

    //still here so notation was correct and baseaddress+offsets are filled in
    //now read
    realaddress2:=baseaddress;
    for i:=0 to length(offsets)-1 do
    begin
      realaddress:=0;
      check:=readprocessmemory(processhandle,pointer(realaddress2),@realaddress,processhandler.pointersize,count);
      if check and (count=processhandler.pointersize) then
        realaddress2:=realaddress+offsets[i]
      else
        exit;
    end;

    result:=realaddress2;
    error:=false;
  finally
    list.free;
  end;
end;

function TSymhandler.ParseRange(s: string; var start: QWORD; var stop: QWORD): boolean;
var
  tokens: ttokens;
  i: integer;
  t2start: integer;
  s1,s2: string;
  err: boolean;
begin
  result:=false;
  tokenize(s, tokens);

  //first find the first part
  s1:='';
  for i:=0 to length(tokens)-2 do
  begin
    s1:=s1+tokens[i];
    err:=false;
    start:=symhandler.getAddressFromName(s1, true, err);
    if (err=false) and (tokens[i+1]='-') then
    begin
      t2start:=i+2;
      break;
    end;
  end;

  if err then exit;

  s2:='';
  for i:=t2start to length(tokens)-1 do
    s2:=s2+tokens[i];

  stop:=symhandler.getAddressFromName(s2, true, err);

  result:=not err;
end;

function TSymhandler.ParseAsPointer(s: string; list:tstrings): boolean;
var i: integer;
    prolog: boolean;
    currentlevel: integer;
    temps: string;
    ispointer: boolean;
begin
  //parse the string
  result:=false;
  currentlevel:=0;
  prolog:=true;
  temps:='';
  ispointer:=false;
  
  for i:=1 to length(s) do
  begin
    if s[i]='[' then
    begin
      if prolog then
      begin
        inc(currentlevel);
        ispointer:=true;
      end
      else
        exit; //bracket open after the prolog is not allowed
    end
    else
    begin
      if prolog then
      begin
        if not (s[i] in [#8,' ']) then  //no space or tab
          prolog:=false;
      end;

      if not prolog then
      begin
        //definition, currentlevel is set, now parse till last ] (currentlevel=0)
        if s[i]=']' then //end of a level
        begin
          dec(currentlevel);
          if temps='' then temps:='+0';          
          list.Add(temps);

          temps:='';

          if currentlevel<0 then exit;
          continue;
        end
        else
          temps:=temps+s[i];
      end;
    end;
  end;


  if temps='' then temps:='+0';
  if (ispointer) and (temps<>'') then list.Add(temps);
  if currentlevel>0 then exit;

  result:=ispointer;
end;

function TSymhandler.getCommonModuleList;
begin
  result:=commonModuleList;
end;

procedure TSymhandler.loadCommonModuleList;
{
Loads the commonmodules list which is used by the module enumaration to flag modules as a system dll's
}
var
  s: string;
  i,j: integer;
begin
  s:=cheatenginedir+'commonmodulelist.txt';
  if FileExists(s) then //if the list exists
  begin
    if commonModuleList=nil then
      commonModuleList:=tstringlist.create;

    commonModuleList.Clear;  
    try
      commonModuleList.LoadFromFile(s);

      i:=0;
      while i<commonModuleList.Count do
      begin
        j:=pos('#', commonModuleList[i]);
        if j>0 then
          commonModuleList[i]:=trim(copy(commonModuleList[i], 1, j-1));

        commonModuleList[i]:=lowercase(commonModuleList[i]);

        if commonModuleList[i]='' then
          commonModuleList.Delete(i)
        else
          inc(i);
      end;
    except
      //don't care if file can't be loaded anyhow
    end;
  end;
end;

procedure TSymhandler.RemoveFinishedLoadingSymbolsNotification(n: TNotifyEvent);
var i,j: integer;
begin
  //search and destroy
  for i:=0 to length(SymbolsLoadedNotification)-1 do
    if (TMethod(SymbolsLoadedNotification[i]).Data = TMethod(n).Data) and (TMethod(SymbolsLoadedNotification[i]).Code = TMethod(n).Code) then
    begin
      for j:=i to length(SymbolsLoadedNotification)-2 do
        SymbolsLoadedNotification[j]:=SymbolsLoadedNotification[j+1];

      setlength(SymbolsLoadedNotification, length(SymbolsLoadedNotification)-1);
      break;
    end;
end;

procedure TSymhandler.AddSymbolList(sl: TSymbolListHandler);
var i: integer;
begin
  symbollistsMREW.beginwrite;
  try
    for i:=0 to length(symbollists)-1 do
      if symbollists[i]=sl then exit; //already in the list

    setlength(symbollists, length(symbollists)+1);
    symbollists[length(symbollists)-1]:=sl;
  finally
    symbollistsMREW.Endwrite;
  end;
end;

procedure TSymhandler.RemoveSymbolList(sl: TSymbolListHandler);
var i,j: integer;
begin
  symbollistsMREW.Beginwrite;
  try
    for i:=0 to length(symbollists)-1 do
      if symbollists[i]=sl then
      begin
        for j:=i to length(symbollists)-2 do
          symbollists[i]:=symbollists[i+1];

        setlength(symbollists, length(symbollists)-1);
      end;

  finally
    symbollistsMREW.Endwrite;
  end;
end;


procedure TSymhandler.AddFinishedLoadingSymbolsNotification(n: TNotifyEvent); //there is no remove
begin
  setlength(SymbolsLoadedNotification, length(SymbolsLoadedNotification)+1);
  SymbolsLoadedNotification[length(SymbolsLoadedNotification)-1]:=n;
end;

procedure TSymhandler.NotifyFinishedLoadingSymbols;
var i: integer;
begin
  //tell all notification routines that the symbollist has been updated and is ready for use
  for i:=0 to length(SymbolsLoadedNotification)-1 do
    if assigned(SymbolsLoadedNotification[i]) then
      SymbolsLoadedNotification[i](self);
end;


destructor TSymhandler.destroy;
begin


  if symbolloaderthread<>nil then
  begin
    symbolloaderthread.Terminate;
    symbolloaderthread.WaitFor;
    freeandnil(symbolloaderthread);
  end;

  if (symbollist<>nil) then
    symbollist.free;

  if commonModuleList<>nil then
    commonModuleList.free;

  modulelistpos:=0;

  symbolloadervalid.Free;
  modulelistMREW.free;
  userdefinedsymbolsCS.free;
  symbollistsMREW.free;

  setlength(userdefinedsymbols,0);
  setlength(modulelist,0);


end;


constructor TSymhandler.create;
begin
  log('TSymhandler.create');
  symbolloadervalid:=TMultiReadExclusiveWriteSynchronizer.create;
  modulelistMREW:=TMultiReadExclusiveWriteSynchronizer.create;
  userdefinedsymbolsCS:=TCriticalSection.create;
  symbollistsMREW:=TMultiReadExclusiveWriteSynchronizer.Create;

  dotnetModuleSymbolListMREW:=TMultiReadExclusiveWriteSynchronizer.create;

  log('TSymhandler.create 1');
  dotNetDataCollector:=TDotNetPipe.create;

  log('TSymhandler.create 2');
  //setlength(internalsymbols,4);
  setlength(userdefinedsymbols,32);
  setlength(modulelist,32);

  showmodules:=true;
  showsymbols:=true;
  ExceptionOnLuaLookup:=true;

  log('TSymhandler.create 3');
  symbollist:=TSymbolListHandler.create;

  log('TSymhandler.create exit');
end;


procedure symhandlerInitialize;
var psa,dbghlp: THandle;
begin
  symhandler:=tsymhandler.create;
  {$ifdef windows}
  if selfsymhandler=nil then
  begin
    selfsymhandler:=Tsymhandler.create;
    selfsymhandler.targetself:=true;
    selfsymhandler.reinitialize;
  end;
  {$endif}

{$ifdef windows}
{$ifdef cpu32}
  dbghlp:=LoadLibrary(pchar(CheatEngineDir+'\win32\dbghelp.dll'));
{$else}
  dbghlp:=LoadLibrary(pchar(CheatEngineDir+'\win64\dbghelp.dll'));
{$endif}
  if dbghlp=0 then //fallback to the search path
    dbghlp:=loadlibrary('Dbghelp.dll');

  SymFromName:=GetProcAddress(dbghlp,'SymFromName');
  SymFromAddr:=GetProcAddress(dbghlp,'SymFromAddr');

  psa:=loadlibrary('Psapi.dll');
  EnumProcessModules:=GetProcAddress(psa,'EnumProcessModules');
  EnumProcessModulesEx:=GetProcAddress(psa,'EnumProcessModulesEx');
  GetModuleFileNameEx:=GetProcAddress(psa,'GetModuleFileNameExA');
  if not assigned(EnumProcessModulesEx) then
    EnumProcessModulesEx:=EnumProcessModulesExNotImplemented;
{$endif}
end;


finalization
  if selfsymhandler<>nil then
    freeandnil(selfsymhandler);

  if symhandler<>nil then
    freeandnil(symhandler);
  
end.








