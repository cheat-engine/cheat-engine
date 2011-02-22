unit symbolhandler;

{$MODE Delphi}

interface


uses jwawindows, windows, classes,LCLIntf,imagehlp,{psapi,}sysutils,{tlhelp32,}cefuncproc,newkernelhandler,syncobjs;

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
end;

type symexception=class(Exception);


type TUserdefinedsymbol=record
  symbolname: string;
  address: ptrUint;
  addressstring: string;

  allocsize: dword; //if it is a global alloc, allocsize>0
  processid: dword; //the processid this memory was allocated to (in case of processswitches)
end;

type TModuleInfo=record
  modulename: string;
  modulepath: string;
  isSystemModule: boolean;
  baseaddress: ptrUint;
  basesize: dword;
end;

type TUserdefinedSymbolCallback=procedure;

type
  TSymbolloaderthread=class(tthread)
  private
    symbolprocesshandle: thandle;
    targetself: boolean;
    thisprocesshandle: thandle;
    thisprocessid: dword;    
    procedure LoadDriverSymbols;
    procedure LoadDLLSymbols;
  public
    isloading: boolean;
    error: boolean;
    symbolsloaded: boolean;

    kernelsymbols: boolean;
    dllsymbols: boolean;
    searchpath: string;

    procedure execute; override;
    constructor create(targetself, CreateSuspended: boolean);
    destructor destroy; override;
  end;

  TTokens=array of string;

  PBoolean=^boolean;

  TSymHandler=class
  private
    symbolloaderthread: TSymbolloaderthread;

    modulelistpos: integer;
    modulelist: array of TModuleInfo;

    symbolloadervalid: TMultiReadExclusiveWriteSynchronizer;
    modulelistMREW: TMultiReadExclusiveWriteSynchronizer;

    userdefinedsymbolspos: integer;
    userdefinedsymbols: array of TUserdefinedsymbol;
    userdefinedsymbolsMREW: TMultireadExclusiveWriteSynchronizer;

    fshowmodules: boolean;   //--determines what is returned by getnamefromaddress
    fshowsymbols: boolean;   ///

    UserdefinedSymbolCallback: TUserdefinedSymbolCallback;
    searchpath: string;

    commonModuleList: tstringlist;

    function getusedprocesshandle :thandle;
    function getusedprocessid:dword;
    function getisloaded:boolean;
    function geterror:boolean;
    function GetUserdefinedSymbolByNameIndex(symbolname:string):integer;
    function GetUserdefinedSymbolByAddressIndex(address: dword):integer;

    procedure setshowmodules(x: boolean); //todo: Move this to the disassembler and let that decide
    procedure setshowsymbols(x: boolean);
    procedure tokenize(s: string; var tokens: TTokens);
  public

    kernelsymbols: boolean;
    dllsymbols: boolean;
    
    locked: boolean;
    targetself: boolean;

    property showmodules: boolean read fshowmodules write setshowmodules;
    property showsymbols: boolean read fshowsymbols write setshowsymbols;

    property usedprocesshandle: thandle read getusedprocesshandle;
    property usedprocessid: dword read getusedprocessid;
    property isloaded: boolean read getisloaded;
    property hasError: boolean read geterror;
    procedure waitforsymbolsloaded;
    procedure reinitialize;
    procedure loadmodulelist;
    procedure ReinitializeUserdefinedSymbolList;
    procedure fillMemoryRegionsWithModuleData(var mr: TMemoryregions; startaddress: dword; size: dword);
    procedure getModuleList(list: tstrings);
    function getmodulebyaddress(address: ptrUint; var mi: TModuleInfo):BOOLEAN;
    function getmodulebyname(modulename: string; var mi: TModuleInfo):BOOLEAN;
    function inModule(address: ptrUint): BOOLEAN; //returns true if the given address is part of a module
    function inSystemModule(address: ptrUint): BOOLEAN;
    function getNameFromAddress(address:ptrUint):string; overload;
    function getNameFromAddress(address:ptrUint; var found: boolean; hexcharsize: integer=8):string; overload;
    function getNameFromAddress(address:ptrUint;symbols:boolean; modules: boolean; baseaddress: PUINT64=nil; found: PBoolean=nil; hexcharsize: integer=8):string; overload;

    function getAddressFromName(name: string):ptrUint; overload;
    function getAddressFromName(name: string; waitforsymbols: boolean):ptrUint; overload;
    function getAddressFromName(name: string; waitforsymbols: boolean; out haserror: boolean):ptrUint; overload;
    function getAddressFromName(name: string; waitforsymbols: boolean; out haserror: boolean; context: PContext):ptrUint; overload;

    function getsearchpath:string;
    procedure setsearchpath(path:string);

    //userdefined symbols
    function DeleteUserdefinedSymbol(symbolname:string):boolean;
    function GetUserdefinedSymbolByName(symbolname:string):ptrUint;
    function SetUserdefinedSymbolAllocSize(symbolname:string; size: dword): boolean;
    function GetUserdefinedSymbolByAddress(address:ptrUint):string;
    procedure AddUserdefinedSymbol(addressstring: string; symbolname: string);
    procedure EnumerateUserdefinedSymbols(list:tstrings);

    function ParseAsPointer(s: string; list:tstrings): boolean;
    function GetAddressFromPointer(s: string; var error: boolean):ptrUint;

    procedure loadCommonModuleList;
    function getCommonModuleList: Tstringlist;
    procedure RegisterUserdefinedSymbolCallback(callback: TUserdefinedSymbolCallback);
    constructor create;
    destructor destroy; override;
end;

var symhandler: TSymhandler=nil;
    selfsymhandler: TSymhandler=nil;  //symhandler object for CE itself

type
    PSYMBOL_INFO = ^TSYMBOL_INFO;
    TSYMBOL_INFO = {packed} record
            SizeOfStruct : ULONG;
            TypeIndex : ULONG;
            Reserved : array[0..1] of ULONG64;
            info : ULONG;
            Size : ULONG;
            ModBase : ULONG64;
            Flags : ULONG;
            Value : ULONG64;
            Address : ULONG64;
            Register : ULONG;
            Scope : ULONG;
            Tag : ULONG;
            NameLen : ULONG;
            MaxNameLen : ULONG;
            Name : array[0..0] of TCHAR;
         end;
    SYMBOL_INFO = TSYMBOL_INFO;
    LPSYMBOL_INFO = PSYMBOL_INFO;

type TSymFromName=function(hProcess: HANDLE; Name: LPSTR; Symbol: PSYMBOL_INFO): BOOL; stdcall;
type TSymFromAddr=function(hProcess:THANDLE; Address:dword64; Displacement:PDWORD64; Symbol:PSYMBOL_INFO):BOOL;stdcall;

var SymFromName: TSymFromName;
    SymFromAddr: TSymFromAddr;

procedure symhandlerInitialize;

implementation

uses assemblerunit, driverlist;


const
  LIST_MODULES_DEFAULT=0;
  LIST_MODULES_32BIT=1;
  LIST_MODULES_64BIT=2;
  LIST_MODULES_ALL=3;

type TEnumProcessModulesEx=function(hProcess: HANDLE; lphModule: PHMODULE; cb: DWORD; var lpcbNeeded: DWORD; dwFilterFlag: DWORD): BOOL; stdcall;
type TEnumProcessModules=function(hProcess: HANDLE; lphModule: PHMODULE; cb: DWORD; var lpcbNeeded: DWORD): BOOL; stdcall;
type TGetModuleFileNameEx=function(hProcess: HANDLE; hModule: HMODULE; lpFilename: pchar; nSize: DWORD): DWORD; stdcall;


var EnumProcessModulesEx: TEnumProcessModulesEx;
    EnumProcessModules:   TEnumProcessModules;
    GetModuleFileNameEx:  TGetModuleFilenameEx;

function EnumProcessModulesExNotImplemented(hProcess: HANDLE; lphModule: PHMODULE; cb: DWORD; var lpcbNeeded: DWORD; dwFilterFlag: DWORD): BOOL; stdcall;
begin
  result:=EnumProcessModules(hProcess,lphModule,cb,lpcbNeeded);
end;

procedure TSymbolloaderthread.LoadDLLSymbols;
var need:dword;
    x: PPointerArray;
    i: integer;
    count: integer;
    modulename: pchar;
begin
  need:=0;

  EnumProcessModulesEx(thisprocesshandle,nil,0,need, LIST_MODULES_ALL);
  getmem(x,need);
  try
    if EnumProcessModulesEx(thisprocesshandle,@x[0],need,need, LIST_MODULES_ALL) then
    begin

      count:=need div sizeof(pointer);
      getmem(modulename,1024);
      try
        for i:=0 to count-1 do
        begin
          GetModuleFileNameEx(thisprocesshandle,ptrUint(x[i]),modulename,200);
          symLoadModule64(thisprocesshandle,0,pchar(modulename),nil,ptrUint(x[i]),0);
        end;
      finally
        freemem(modulename);
      end;
    end;
  finally
    freemem(x);
  end;
end;

procedure TSymbolloaderthread.LoadDriverSymbols;
var need:dword;
    x: PPointerArray;
    i: integer;
    count: integer;
    drivername: pchar;
begin
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
      end;
    end;
  finally
    freemem(x);
  end;
end;

procedure TSymbolloaderthread.execute;
begin
  try
    try
      SymbolsLoaded:=false;
      if symbolprocesshandle<>0 then Symcleanup(symbolprocesshandle); //cleanup first


      SymbolsLoaded:=SymInitialize(thisprocesshandle,nil,true);
      if symbolsloaded then
      begin
        symsetoptions(symgetoptions or SYMOPT_CASE_INSENSITIVE);
        symsetsearchpath(processhandle,pchar(searchpath));

        if kernelsymbols then LoadDriverSymbols;
        LoadDLLSymbols;
      end else error:=true;


      symbolprocesshandle:=processhandle;
    finally
      isloading:=false;
    end;
  except
    outputdebugstring('Symbolloaderthread has crashed');
  end;
end;

destructor TSymbolloaderthread.destroy;
begin
  //close the symbol handler for this processhandle
  if symbolprocesshandle<>0 then Symcleanup(symbolprocesshandle); 
  inherited destroy;
end;

constructor TSymbolloaderthread.create(targetself, CreateSuspended: boolean);
var
  _processid: dword;
  _processhandle: thandle;
begin
  self.targetself:=targetself;
  
{$ifdef autoassemblerdll}
  _processid:=symbolhandler.ProcessID;
  _processhandle:=symbolhandler.processhandle;
{$else}
  if targetself then
  begin
    _processid:=getcurrentprocessid;
    _processhandle:=getcurrentprocess;
  end
  else
  begin
    _processid:=cefuncproc.ProcessID;
    _processhandle:=cefuncproc.ProcessHandle;
  end;
{$endif}

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

function TSymhandler.geterror:boolean;
begin
  symbolloadervalid.beginread;
  if symbolloaderthread<>nil then
    result:=symbolloaderthread.error
  else
    result:=false; //no error

  symbolloadervalid.endread;
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
  if locked then raise symexception.Create('You can''t change this setting at the moment');
  fshowmodules:=x;
end;

procedure TSymhandler.setshowsymbols(x: boolean);
begin
  if locked then raise symexception.Create('You can''t change this setting at the moment');
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

procedure TSymhandler.reinitialize;
begin
  loadmodulelist;

  symbolloadervalid.BeginWrite;
  if symbolloaderthread<>nil then
  begin
    symbolloaderthread.Terminate;
    symbolloaderthread.WaitFor; //wait till it's done
    symbolloaderthread.Free;
  end;


  symbolloaderthread:=tsymbolloaderthread.Create(targetself,true);
  symbolloaderthread.kernelsymbols:=kernelsymbols;
  symbolloaderthread.searchpath:=searchpath;
  symbolloaderthread.start;

  symbolloadervalid.EndWrite;

  ReinitializeUserdefinedSymbolList;
end;

procedure TSymhandler.Waitforsymbolsloaded;
begin
  symbolloadervalid.beginread;
  if symbolloaderthread<>nil then
    symbolloaderthread.WaitFor;
  symbolloadervalid.endread;
end;

procedure TSymhandler.ReinitializeUserdefinedSymbolList;
var i: integer;
 x: dword;
 err: integer;
 haserror: boolean;
begin
  for i:=0 to userdefinedsymbolspos-1 do
  begin
    val('$'+userdefinedsymbols[i].addressstring, x, err);
    if err>0 then //iot's not a hexadecimal value
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
  userdefinedsymbolsMREW.beginwrite;
  try
    for i:=0 to userdefinedsymbolspos-1 do
      if uppercase(userdefinedsymbols[i].symbolname)=uppercase(symbolname) then
      begin
        //found it
        //check if it had a alloc, if so, free it
        if (userdefinedsymbols[i].allocsize>0) and (userdefinedsymbols[i].processid=processid) then
          VirtualFreeEx(processhandle,pointer(userdefinedsymbols[i].address),0,MEM_RELEASE);

        //now move up all the others and decrease the list
        for j:=i to userdefinedsymbolspos-2 do
          userdefinedsymbols[j]:=userdefinedsymbols[j+1];

        dec(userdefinedsymbolspos);
        result:=true;
        break;
      end;
  finally
    userdefinedsymbolsMREW.endwrite;
  end;

  if assigned(UserdefinedSymbolCallback) then
    UserdefinedSymbolCallback();
end;

function TSymhandler.SetUserdefinedSymbolAllocSize(symbolname:string; size: dword): boolean;
{
This function will find the userdefined symbol, and when found checks if it already
allocated memory. If not allocate memory, else check if the size matches
}
var i:integer;
    p: pointer;
begin
  result:=false;
  if size=0 then raise exception.Create('Please provide a bigger size');

  userdefinedsymbolsMREW.beginread;
  try
    i:=GetUserdefinedSymbolByNameIndex(symbolname);
    if i=-1 then //doesn't exist yet. Add it
    begin
      p:=virtualallocex(processhandle,nil,size,MEM_COMMIT , PAGE_EXECUTE_READWRITE);
      if p=nil then
        raise exception.Create('Error allocating memory');
      AddUserdefinedSymbol(inttohex(ptrUint(p),8),symbolname);
      i:=GetUserdefinedSymbolByNameIndex(symbolname);
      userdefinedsymbols[i].allocsize:=size;
      userdefinedsymbols[i].processid:=processid;

    end
    else
    begin
      //it exists, check first
      if (userdefinedsymbols[i].allocsize>0) and (userdefinedsymbols[i].processid=processid) then
      begin
        //already allocated and processid is the same
        if size<>userdefinedsymbols[i].allocsize then
          raise exception.Create('The symbol named '+userdefinedsymbols[i].symbolname+' was previously declared with a size of '+inttostr(userdefinedsymbols[i].allocsize)+' instead of '+inttostr(size)+'. all scripts that use this memory must give the same size. Adjust the size, or delete the old alloc from the userdefined symbol list');
      end;

      if userdefinedsymbols[i].processid<>processid then
      begin
        p:=virtualallocex(processhandle,nil,size,MEM_COMMIT , PAGE_EXECUTE_READWRITE);
        if p=nil then
          raise exception.Create('Error allocating memory');

        userdefinedsymbols[i].address:=ptrUint(p);
        userdefinedsymbols[i].addressstring:=inttohex(ptrUint(p),8);
        userdefinedsymbols[i].allocsize:=size;
        userdefinedsymbols[i].processid:=processid;
      end;
    end;
    result:=true; //managed to get here without crashing...
    if assigned(UserdefinedSymbolCallback) then
      UserdefinedSymbolCallback();
  finally
    userdefinedsymbolsMREW.EndRead;
  end;
end;

function TSymhandler.GetUserdefinedSymbolByNameIndex(symbolname:string):integer;
var i: integer;
begin
  result:=-1;
  userdefinedsymbolsMREW.beginread;
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
    userdefinedsymbolsMREW.endread;
  end;
end;

function TSymhandler.GetUserdefinedSymbolByAddressIndex(address: dword):integer;
var i: integer;
begin
  result:=-1;
  userdefinedsymbolsMREW.beginread;
  try
    for i:=0 to userdefinedsymbolspos-1 do
      if userdefinedsymbols[i].address=address then
      begin
        result:=i;
        break;
      end;
  finally
    userdefinedsymbolsMREW.endread;
  end;  
end;

function TSymhandler.GetUserdefinedSymbolByName(symbolname:string):ptrUint;
var i:integer;
begin
  result:=0;

  userdefinedsymbolsMREW.beginread;
  try
    i:=GetUserdefinedSymbolByNameIndex(symbolname);
    if i=-1 then exit;
    result:=userdefinedsymbols[i].address;
  finally
    userdefinedsymbolsMREW.endread;
  end;
end;

function TSymhandler.GetUserdefinedSymbolByAddress(address:ptrUint):string;
var i:integer;
begin
  result:='';
  userdefinedsymbolsMREW.beginread;
  try
    i:=GetUserdefinedSymbolByAddressIndex(address);
    if i=-1 then exit;
    result:=userdefinedsymbols[i].symbolname;
  finally
    userdefinedsymbolsMREW.endread;
  end;
end;

procedure TSymhandler.AddUserdefinedSymbol(addressstring: string; symbolname: string);
{
This routine will add the symbolname+address combination to the symbollist
}
var
  address: dword;
begin
  if getuserdefinedsymbolbyname(symbolname)>0 then raise symexception.Create(symbolname+' already exists');

  address:=getAddressFromName(addressstring);
  if address=0 then raise symexception.Create('You can''t add a symbol with address 0');

  userdefinedsymbolsMREW.beginwrite;
  try
    if userdefinedsymbolspos+1>=length(userdefinedsymbols) then
      setlength(userdefinedsymbols,length(userdefinedsymbols)*2);

    userdefinedsymbols[userdefinedsymbolspos].address:=address;
    userdefinedsymbols[userdefinedsymbolspos].addressstring:=addressstring;
    userdefinedsymbols[userdefinedsymbolspos].symbolname:=symbolname;
    userdefinedsymbols[userdefinedsymbolspos].allocsize:=0;
    userdefinedsymbols[userdefinedsymbolspos].processid:=0;
    inc(userdefinedsymbolspos);
  finally
    userdefinedsymbolsMREW.endwrite;
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
  userdefinedsymbolsMREW.BeginRead;
  for i:=0 to userdefinedsymbolspos-1 do
  begin
    getmem(extradata,sizeof(TUDSEnum));
    extradata.address:=userdefinedsymbols[i].address;
    extradata.allocsize:=userdefinedsymbols[i].allocsize;
    extradata.addressstring:=@userdefinedsymbols[i].addressstring[1];

    list.Addobject(userdefinedsymbols[i].symbolname,pointer(extradata));
    //just don't forget to free it at the caller's end
  end;
  userdefinedsymbolsMREW.EndRead;
end;

procedure TSymhandler.fillMemoryRegionsWithModuleData(var mr: TMemoryregions; startaddress: dword; size: dword);
{
This routine will fill in a TMemoryRegions array with the base and startaddress of the modules it found
}
var currentaddress: dword;
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
var i: integer;
begin
  result:=false;
  modulelistMREW.beginread;
  for i:=0 to modulelistpos-1 do
    if (uppercase(modulelist[i].modulename)=uppercase(modulename)) then
    begin
      mi:=modulelist[i];
      result:=true;
      break;
    end;
  modulelistMREW.endread;
end;

function TSymHandler.getsearchpath:string;
var sp: pchar;
begin
  getmem(sp,4096);
  if isloaded then
  begin
    if SymGetSearchPath(processhandle,sp,4096) then
    begin
      result:=sp;
    end
    else result:='';
  end;
end;

procedure TSymHandler.setsearchpath(path:string);
begin
  if isloaded then
    symsetsearchpath(processhandle,pchar(path));

  searchpath:=path;
end;


function TSymhandler.getNameFromAddress(address:ptrUint;symbols:boolean; modules: boolean; baseaddress: PUINT64=nil; found: PBoolean=nil; hexcharsize: integer=8):string;
var symbol :PSYMBOL_INFO;
    offset: qword;
    s: string;
    mi: tmoduleinfo;
    processhandle: thandle;
begin
{$ifdef autoassemblerdll}
  processhandle:=symbolhandler.processhandle;
{$else}
  if targetself then
  begin
    processhandle:=getcurrentprocess;
  end
  else
  begin
    processhandle:=cefuncproc.ProcessHandle;
  end;
{$endif}


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
        if isloaded then
        begin
          getmem(symbol,sizeof(TSYMBOL_INFO)+255);
          try
            zeromemory(symbol,sizeof(TSYMBOL_INFO)+255);
            symbol.SizeOfStruct:=sizeof(TSYMBOL_INFO);
            symbol.MaxNameLen:=254;


            if SymFromAddr(processhandle,address,@offset,symbol) then
            begin
              //found it
              s:=pchar(@symbol.Name[0]);
              if offset=0 then
                result:=s
              else
                result:=s+'+'+inttohex(offset,1);

              if baseaddress<>nil then
                baseaddress^:=symbol.Address;

              if found<>nil then
                found^:=true;

              exit;
            end;

          finally
            freemem(symbol);
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
    raise symexception.Create('Failure determining what '+name+' means');
end;

function TSymhandler.getAddressFromName(name: string; waitforsymbols: boolean; out haserror: boolean):ptrUint;
begin
  result:=getAddressFromName(name, waitforsymbols, haserror,nil);
end;

function TSymhandler.getAddressFromName(name: string; waitforsymbols: boolean; out haserror: boolean; context: PContext):ptrUint;
type TCalculation=(calcAddition, calcSubstraction);
var mi: tmoduleinfo;
    offset: dword;
    i,j: integer;

    ws: widestring;
    pws: pwidechar;
    error: boolean;

    processhandle: thandle;

    tokens: TTokens;
    mathstring: string;
    hasMultiplication, hasPointer: boolean;

    nextoperation: TCalculation;
    regnr: integer;

    symbol: PSYMBOL_INFO;
begin
  hasPointer:=false;
  haserror:=false;
  offset:=0;


{$ifdef autoassemblerdll}
  processhandle:=symbolhandler.processhandle;
{$else}
  if targetself then
  begin
    processhandle:=getcurrentprocess;
  end
  else
  begin
    processhandle:=cefuncproc.ProcessHandle;
  end;
{$endif}

  val('$'+name,result,i);
  if i=0 then exit; //it's a valid hexadecimal string

  if copy(name,1,2)='0x' then
  begin
    val(name,result,i);
    if i=0 then exit;
  end;


  //not a hexadecimal string
  tokenize(name, tokens);

  //first check the most basic thing
  if length(tokens)=0 then
  begin
    haserror:=true;
    exit;
  end;

  //if it starts with a *, - or + or ends with it, then it's a bad formula
  if (tokens[0][1] in ['*','+','-']) or (tokens[length(tokens)-1][1] in ['*','+','-']) then
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
            begin
              //no context or not a register
              result:=GetUserdefinedSymbolByName(tokens[i]);
              if result>0 then
              begin
                tokens[i]:=inttohex(result,8);
                continue;
              end;

              //not a userdefined symbol
              {$ifndef autoassemblerdll}
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
              if (symbolloaderthread<>nil) then
              begin
                if symbolloaderthread.isloading and not waitforsymbols then
                begin
                  if not waitforsymbols then
                  begin
                    haserror:=true;
                    exit;
                  end;

                  symbolloaderthread.WaitFor;
                end;

                //it's not a valid address, it's not a calculation, it's not a modulename+offset, so lets see if it's a module

                tokens[i]:=StringReplace(tokens[i],'.','!',[]);

                getmem(symbol,sizeof(TSYMBOL_INFO)+255);
                try
                  zeromemory(symbol,sizeof(TSYMBOL_INFO)+255);
                  symbol.SizeOfStruct:=sizeof(TSYMBOL_INFO);
                  symbol.MaxNameLen:=254;


                  if SymFromName(processhandle, pchar(tokens[i]), symbol ) then
                  begin
                    tokens[i]:=inttohex(symbol.Address,8);
                    continue;
                  end;
                finally
                  freemem(symbol);
                end;
              end;
            end;


            //not a register or symbol
            haserror:=true;
            exit;
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
    result:=GetAddressFromPointer(mathstring,error);
    if not error then
    begin
      result:=result+offset;
      exit;
    end
    else
    begin
      //it has a pointer notation but the pointer didn't get handled... ERROR!
      haserror:=true;
      exit;
    end;
  end;


  //handle the mathstring
  if hasmultiplication then
  begin
    //first do the multiplications
    for i:=0 to length(tokens)-1 do
    begin
      if tokens[i]='*' then
      begin
        //multiply the left and right
        tokens[i-1]:=inttohex(strtoint64('$'+tokens[i-1])*strtoint64('$'+tokens[i+1]),8);
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
              result:=result+strtoint64('$'+tokens[i]);

            calcSubstraction:
              result:=result-strtoint64('$'+tokens[i]);

          end;

        end;
      end;

    end;
  end;

end;


procedure TSymhandler.loadmodulelist;
var
  ths: thandle;
  me32:MODULEENTRY32;
  x: pchar;

  i: integer;

  processid: dword;
  modulename: string;

  alreadyInTheList: boolean;
begin
  try
{$ifdef autoassemblerdll}
  processid:=symbolhandler.ProcessID;
{$else}
  if targetself then
    processid:=getcurrentprocessid
  else
    processid:=cefuncproc.ProcessID;
{$endif}
  
  modulelistMREW.BeginWrite;
  try
    modulelistpos:=0;
    if processid=0 then exit;

    //refresh the module list
    ths:=CreateToolhelp32Snapshot(TH32CS_SNAPMODULE or TH32CS_SNAPMODULE32,processid);
    if ths<>0 then
    begin
      me32.dwSize:=sizeof(MODULEENTRY32);
      if ths<>0 then
      begin
        try
          if module32first(ths,me32) then
          repeat
            x:=me32.szExePath;
            modulename:=extractfilename(x);

            alreadyInTheList:=false;
            //check if this modulename is already in the list, and if so check if it's the same base, else add it
            for i:=0 to modulelistpos-1 do
            begin
              if (modulelist[i].baseaddress=ptrUint(me32.modBaseAddr)) then
              begin
                alreadyInTheList:=true;
                break; //it's in the list, no need to continue looking, break out of the for loop
              end;
            end;

            if not alreadyInTheList then
            begin
              if modulelistpos+1>=length(modulelist) then
                setlength(modulelist,length(modulelist)*2);


              modulelist[modulelistpos].modulename:=modulename;
              modulelist[modulelistpos].modulepath:=x;

              //all windows folder files are system modules, except when it is an .exe (minesweeper in xp)
              modulelist[modulelistpos].isSystemModule:=(pos(lowercase(windowsdir),lowercase(x))>0) and (ExtractFileExt(lowercase(x))<>'.exe');

              if (not modulelist[modulelistpos].isSystemModule) and (commonModuleList<>nil) then //check if it's a common module (e.g nvidia physx dll's)
                modulelist[modulelistpos].isSystemModule:=commonModuleList.IndexOf(lowercase(modulelist[modulelistpos].modulename))<>-1;

              modulelist[modulelistpos].baseaddress:=ptrUint(me32.modBaseAddr);
              modulelist[modulelistpos].basesize:=me32.modBaseSize;
              inc(modulelistpos);
            end;

          until not module32next(ths,me32);
        finally
          closehandle(ths);
        end;
      end;
    end;
  finally
    modulelistmrew.EndWrite;
  end;

  except
    MessageBox(0,'procedure TSymhandler.loadmodulelist','procedure TSymhandler.loadmodulelist',0);
  end;
end;



function TSymhandler.GetAddressFromPointer(s: string; var error: boolean):ptrUint;
{
Will return the address of a pointer noted as [[[xxx+xx]+xx]+xx]+xx
If it is a invalid pointer, or can not be resolved, the result is NULL 
}
var i: integer;
    list: tstringlist;
    offsets: array of dword;
    baseaddress: dword;
    off: string;
    realaddress, realaddress2: ptrUint;
    check: boolean;
    count: dword;
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
        offsets[i-1]:=strtoint64('$'+off);
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


destructor TSymhandler.destroy;
begin
  if symbolloaderthread<>nil then
  begin
    symbolloaderthread.Terminate;
    symbolloaderthread.WaitFor;
    symbolloaderthread.free;
  end;

  if commonModuleList<>nil then
    commonModuleList.free;

  modulelistpos:=0;

  symbolloadervalid.Free;
  modulelistMREW.free;
  userdefinedsymbolsMREW.free;

  setlength(userdefinedsymbols,0);
  setlength(modulelist,0);
end;

constructor TSymhandler.create;
begin
  symbolloadervalid:=TMultiReadExclusiveWriteSynchronizer.create;
  modulelistMREW:=TMultiReadExclusiveWriteSynchronizer.create;
  userdefinedsymbolsMREW:=TMultireadExclusiveWriteSynchronizer.create;

  //setlength(internalsymbols,4);
  setlength(userdefinedsymbols,32);
  setlength(modulelist,32);

  showmodules:=false;
  showsymbols:=true;
end;


procedure symhandlerInitialize;
var psa,dbghlp: THandle;
begin
  symhandler:=tsymhandler.create;
  if selfsymhandler=nil then
  begin
    selfsymhandler:=Tsymhandler.create;
    selfsymhandler.targetself:=true;
  end;

{$ifdef cpu32}
  dbghlp:=LoadLibrary(pchar(CheatEngineDir+'\win32\dbghelp.dll'));
{$else}
  dbghlp:=loadlibrary('Dbghelp.dll');
{$endif}
  SymFromName:=GetProcAddress(dbghlp,'SymFromName');
  SymFromAddr:=GetProcAddress(dbghlp,'SymFromAddr');

  psa:=loadlibrary('Psapi.dll');
  EnumProcessModules:=GetProcAddress(psa,'EnumProcessModules');
  EnumProcessModulesEx:=GetProcAddress(psa,'EnumProcessModulesEx');
  GetModuleFileNameEx:=GetProcAddress(psa,'GetModuleFileNameExA');
  if not assigned(EnumProcessModulesEx) then
    EnumProcessModulesEx:=EnumProcessModulesExNotImplemented;
end;


finalization
  if selfsymhandler<>nil then
    selfsymhandler.free;

  if symhandler<>nil then
    symhandler.free;
  
end.








