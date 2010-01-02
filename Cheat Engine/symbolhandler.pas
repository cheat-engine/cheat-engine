unit symbolhandler;

interface


uses classes,windows,imagehlp,psapi,sysutils,syncobjs,tlhelp32{$ifndef autoassemblerdll},cefuncproc,newkernelhandler{$endif};

{$ifdef autoassemblerdll}
var
  processid: dword;
  processhandle: thandle;

Type TMemoryRegion = record
  BaseAddress: Dword;
  MemorySize: Dword;
  IsChild: boolean;
  startaddress: pointer;
  end;
type TMemoryregions = array of tmemoryregion;
  
{$endif}

type TUDSEnum=record
  address: dword;
  allocsize: dword;
  addressstring: pchar; //points to the string
end;

type symexception=class(Exception);


type TUserdefinedsymbol=record
  symbolname: string;
  address: dword;
  addressstring: string;

  allocsize: dword; //if it is a global alloc, allocsize>0
  processid: dword; //the processid this memory was allocated to (in case of processswitches)
end;

type TModuleInfo=record
  modulename: string;
  modulepath: string;
  isSystemModule: boolean;
  baseaddress: dword;
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

    procedure setshowmodules(x: boolean);
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
    function getmodulebyaddress(address: dword; var mi: TModuleInfo):BOOLEAN;
    function getmodulebyname(modulename: string; var mi: TModuleInfo):BOOLEAN;
    function inModule(address: dword): BOOLEAN; //returns true if the given address is part of a module
    function inSystemModule(address: dword): BOOLEAN;
    function getNameFromAddress(address:dword):string; overload;
    function getNameFromAddress(address:dword;symbols:boolean; modules: boolean; baseaddress: PDWORD=nil):string; overload;

    function getAddressFromName(name: string):dword; overload;
    function getAddressFromName(name: string; waitforsymbols: boolean):dword; overload;
    function getAddressFromName(name: string; waitforsymbols: boolean; var haserror: boolean):dword; overload;
    function getAddressFromName(name: string; waitforsymbols: boolean; var haserror: boolean; context: PContext):dword; overload;    

    function getsearchpath:string;
    procedure setsearchpath(path:string);

    //userdefined symbols
    function DeleteUserdefinedSymbol(symbolname:string):boolean;
    function GetUserdefinedSymbolByName(symbolname:string):dword;
    function SetUserdefinedSymbolAllocSize(symbolname:string; size: dword): boolean;
    function GetUserdefinedSymbolByAddress(address:dword):string;
    procedure AddUserdefinedSymbol(addressstring: string; symbolname: string);
    procedure EnumerateUserdefinedSymbols(list:tstrings);

    function ParseAsPointer(s: string; list:tstrings): boolean;
    function GetAddressFromPointer(s: string; var error: boolean):dword;

    procedure loadCommonModuleList;
    procedure RegisterUserdefinedSymbolCallback(callback: TUserdefinedSymbolCallback);
    constructor create;
    destructor destroy; override;
end;

var symhandler: TSymhandler;
    selfsymhandler: TSymhandler;  //symhandler object for CE itself

implementation

uses assemblerunit;


procedure TSymbolloaderthread.LoadDLLSymbols;
var need:dword;
    x: PPointerArray;
    i: integer;
    count: integer;
    modulename: pchar;
begin
  EnumProcessModules(thisprocesshandle,nil,0,need);
  getmem(x,need);
  try
    if EnumProcessModules(thisprocesshandle,@x[0],need,need) then
    begin
      count:=need div 4;
      getmem(modulename,200);
      try
        for i:=0 to count-1 do
        begin
          GetModuleFileNameEx(thisprocesshandle,dword(x[i]),modulename,200);
          symLoadModule(thisprocesshandle,0,pchar(modulename),nil,dword(x[i]),0);
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
      count:=need div 4;
      getmem(drivername,200);
      try
        for i:=0 to count-1 do
        begin
          GetDevicedriverFileName(x[i],drivername,200);
          //add drive letter
          symLoadModule(thisprocesshandle,0,pchar(drivername),nil,dword(x[i]),0);
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
end;

destructor TSymbolloaderthread.destroy;
begin
  //close the symbol handler for this processhandle
  if symbolprocesshandle<>0 then Symcleanup(symbolprocesshandle); 
  inherited destroy;
end;

constructor TSymbolloaderthread.create(targetself, CreateSuspended: boolean);
var
  processid: dword;
  processhandle: thandle;
begin
  self.targetself:=targetself;
  
{$ifdef autoassemblerdll}
  processid:=symbolhandler.ProcessID;
  processhandle:=symbolhandler.processhandle;
{$else}
  if targetself then
  begin
    processid:=getcurrentprocessid;
    processhandle:=getcurrentprocess;
  end
  else
  begin
    processid:=cefuncproc.ProcessID;
    processhandle:=cefuncproc.ProcessHandle;
  end;
{$endif}

  thisprocesshandle:=processhandle;
  thisprocessid:=processid;
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
begin
  last:=1;
  for i:=1 to length(s) do
  begin
    if s[i] in ['[',']','+','-','*'] then
    begin
      t:=trim(copy(s, last, i-last));
      if t<>'' then
      begin
        setlength(tokens,length(tokens)+1);
        tokens[length(tokens)-1]:=t;
      end;

      setlength(tokens,length(tokens)+1);
      tokens[length(tokens)-1]:=s[i];
      last:=i+1;
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
  symbolloaderthread.Resume;

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
      AddUserdefinedSymbol(inttohex(dword(p),8),symbolname);
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

        userdefinedsymbols[i].address:=dword(p);
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
      if uppercase(userdefinedsymbols[i].symbolname)=uppercase(symbolname) then
      begin
        result:=i;
        break;
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

function TSymhandler.GetUserdefinedSymbolByName(symbolname:string):dword;
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

function TSymhandler.GetUserdefinedSymbolByAddress(address:dword):string;
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
  error: boolean;
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

function TSymhandler.inSystemModule(address: dword): BOOLEAN;
var mi: TModuleInfo;
    mn: string;
    i: integer;
begin
  result:=false;
  if getmodulebyaddress(address,mi) then
    result:=mi.isSystemModule;
end;

function TSymhandler.inModule(address: dword): BOOLEAN; //returns true if the given address is part of a module
var mi: TModuleInfo;
begin
  result:=getmodulebyaddress(address,mi);
end;

function TSymhandler.getmodulebyaddress(address: dword; var mi: TModuleInfo):BOOLEAN;
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


function TSymhandler.getNameFromAddress(address:dword;symbols:boolean; modules: boolean; baseaddress: PDWORD=nil):string;
var symbol :PImagehlpSymbol;
    offset: dword;
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
          getmem(symbol,sizeof(IMAGEHLP_SYMBOL)+255);
          try
            zeromemory(symbol,sizeof(IMAGEHLP_SYMBOL)+255);
            symbol.SizeOfStruct:=sizeof(IMAGEHLP_SYMBOL)+255;
            symbol.MaxNameLength:=254;
            if SymGetSymFromAddr(processhandle,address,@offset,symbol^) then
            begin
              //found it
              s:=pchar(@symbol.Name[0]);
              if offset=0 then
                result:=s
              else
                result:=s+'+'+inttohex(offset,1);

              if baseaddress<>nil then
                baseaddress^:=symbol.Address;
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
      exit;
    end;
  end;

  result:=inttohex(address,8);  //default

end;

function TSymhandler.getNameFromAddress(address:dword):string;
begin
  result:=getNameFromAddress(address,self.showsymbols,self.showmodules);
end;



function TSymhandler.getAddressFromName(name:string):dword;
begin
  result:=getAddressFromName(name,true);
end;

function TSymhandler.getAddressFromName(name: string; waitforsymbols: boolean): dword;
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

function TSymhandler.getAddressFromName(name: string; waitforsymbols: boolean; var haserror: boolean):dword;
begin
  result:=getAddressFromName(name, waitforsymbols, haserror,nil);
end;

function TSymhandler.getAddressFromName(name: string; waitforsymbols: boolean; var haserror: boolean; context: PContext):dword;
type TCalculation=(calcAddition, calcSubstraction);
var mi: tmoduleinfo;
    symbol :PImagehlpSymbol;
    offset: dword;

    sn: string;
    i,j: integer;

    ws: widestring;
    pws: pwidechar;
    error: boolean;

    processhandle: thandle;

    tokens: TTokens;
    x: dword;
    mathstring: string;
    hasMultiplication, hasPointer: boolean;

    nextoperation: TCalculation;
    regnr: integer;
begin
  haserror:=false;
  
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

            if regnr<>9 then
            begin
              if (context<>nil) and (context^.Eip<>0) then
              begin
                //get the register value, and because this is an address specifier, use the full 32-bits
                case regnr of
                  0: tokens[i]:=inttohex(context^.Eax,8);
                  1: tokens[i]:=inttohex(context^.Ecx,8);
                  2: tokens[i]:=inttohex(context^.Edx,8);
                  3: tokens[i]:=inttohex(context^.Ebx,8);
                  4: tokens[i]:=inttohex(context^.Esp,8);
                  5: tokens[i]:=inttohex(context^.Ebp,8);
                  6: tokens[i]:=inttohex(context^.Esi,8);
                  7: tokens[i]:=inttohex(context^.Edi,8);
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
              if (darkbytekernel<>0) and (length(tokens[i])>6) and (pos('KERNEL_',uppercase(tokens[i]))>0) then
              begin
                tokens[i]:=copy(tokens[i],8,length(tokens[i])-7);
                ws:=tokens[i];
                pws:=@ws[1];
                result:=dword(GetKProcAddress(pws));
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

                getmem(symbol,sizeof(IMAGEHLP_SYMBOL)+255);
                try
                  zeromemory(symbol,sizeof(IMAGEHLP_SYMBOL)+255);
                  symbol.SizeOfStruct:=sizeof(IMAGEHLP_SYMBOL)+255;
                  symbol.MaxNameLength:=254;

                  if SymGetSymFromName(processhandle,pchar(tokens[i]),symbol^) then
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
        tokens[i-1]:=inttohex(strtoint('$'+tokens[i-1])*strtoint('$'+tokens[i+1]),8);
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
              result:=result+strtoint('$'+tokens[i]);

            calcSubstraction:
              result:=result-strtoint('$'+tokens[i]);

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

  processid: dword;
begin
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
    ths:=CreateToolhelp32Snapshot(TH32CS_SNAPMODULE,processid);
    if ths<>0 then
    begin
      me32.dwSize:=sizeof(MODULEENTRY32);
      if ths<>0 then
      begin
        try
          if module32first(ths,me32) then
          repeat
            if modulelistpos+1>=length(modulelist) then
              setlength(modulelist,length(modulelist)*2);

            x:=me32.szExePath;
            modulelist[modulelistpos].modulename:=extractfilename(x);
            modulelist[modulelistpos].modulepath:=x;
            //I say that physxcore is also a system module even if it isn't located in the windows dir
            modulelist[modulelistpos].isSystemModule:=(pos(lowercase(windowsdir),lowercase(x))>0);

            if (not modulelist[modulelistpos].isSystemModule) and (commonModuleList<>nil) then //check if it's a common module (e.g nvidia physx dll's)
              modulelist[modulelistpos].isSystemModule:=commonModuleList.IndexOf(lowercase(modulelist[modulelistpos].modulename))<>-1;



            modulelist[modulelistpos].baseaddress:=dword(me32.modBaseAddr);
            modulelist[modulelistpos].basesize:=me32.modBaseSize;
            inc(modulelistpos);

          until not module32next(ths,me32);
        finally
          closehandle(ths);
        end;
      end;
    end;
  finally
    modulelistmrew.EndWrite;
  end;
end;



function TSymhandler.GetAddressFromPointer(s: string; var error: boolean):dword;
{
Will return the address of a pointer noted as [[[xxx+xx]+xx]+xx]+xx
If it is a invalid pointer, or can not be resolved, the result is NULL 
}
var i: integer;
    list: tstringlist;
    offsets: array of dword;
    baseaddress: dword;
    off: string;
    realaddress, realaddress2: dword;
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
        offsets[i-1]:=strtoint('$'+off);
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
      check:=readprocessmemory(processhandle,pointer(realaddress2),@realaddress,4,count);
      if check and (count=4) then
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

procedure TSymhandler.loadCommonModuleList;
{
Loads the commonmodules list which is used by the module enumaration to flag modules as a system dll's
}
var
  s: string;
  f: tstringlist;
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



initialization
  symhandler:=tsymhandler.create;
  selfsymhandler:=Tsymhandler.create;
  selfsymhandler.targetself:=true;

finalization
  if selfsymhandler<>nil then
    selfsymhandler.free;

  if symhandler<>nil then
    symhandler.free;
  
end.








