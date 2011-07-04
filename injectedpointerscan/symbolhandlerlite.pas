unit symbolhandlerlite;

interface

uses classes,windows,imagehlp,sysutils,syncobjs,tlhelp32;

Type TMemoryRegion = record
  BaseAddress: Dword;
  MemorySize: Dword;
  IsChild: boolean;
  startaddress: pointer;
  end;
type TMemoryregions = array of tmemoryregion;

type TUserdefinedsymbol=record
  symbolname: string;
  address: dword;
end;

type TModuleInfo=record
  modulename: string;
  baseaddress: dword;
  basesize: dword;
end;

type TUserdefinedSymbolCallback=procedure;

type TSymHandler=class
  private
    lastmodulelistupdate: integer;
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

    function getusedprocesshandle :thandle;
    function getusedprocessid:dword;
    function getisloaded:boolean;

    procedure setshowmodules(x: boolean);
    procedure setshowsymbols(x: boolean);
  public


    locked: boolean;

    property showmodules: boolean read fshowmodules write setshowmodules;
    property showsymbols: boolean read fshowsymbols write setshowsymbols;

    property usedprocesshandle: thandle read getusedprocesshandle;
    property usedprocessid: dword read getusedprocessid;
    property isloaded: boolean read getisloaded;
    procedure waitforsymbolsloaded;
    procedure reinitialize;
    procedure loadmodulelist;
    procedure fillMemoryRegionsWithModuleData(var mr: TMemoryregions; startaddress: dword; size: dword);
    function getmodulebyaddress(address: dword; var mi: TModuleInfo):BOOLEAN;
    function getmodulebyname(modulename: string; var mi: TModuleInfo):BOOLEAN;
    function getNameFromAddress(address:dword):string;
    function getAddressFromName(name: string):dword; overload;
    function getAddressFromName(name: string; waitforsymbols: boolean):dword; overload;

    //userdefined symbols
    function DeleteUserdefinedSymbol(symbolname:string):boolean;
    function GetUserdefinedSymbolByName(symbolname:string):dword;
    function GetUserdefinedSymbolByAddress(address:dword):string;
    procedure AddUserdefinedSymbol(address: dword; symbolname: string);
    procedure EnumerateUserdefinedSymbols(list:tstrings);

    procedure RegisterUserdefinedSymbolCallback(callback: TUserdefinedSymbolCallback);
    constructor create;
end;

var symhandler: TSymhandler;
    processhandle: thandle;
    processid: dword;

implementation

type TSymbolloaderthread=class(tthread)
  public
    isloading: boolean;
    symbolsloaded: boolean;
    thisprocesshandle: thandle;
    thisprocessid: dword;
    procedure execute; override;
    constructor create(CreateSuspended: boolean);
end;

var symbolloaderthread: TSymbolloaderthread;
    symbolprocesshandle: thandle;

procedure TSymbolloaderthread.execute;
begin
  try
    SymbolsLoaded:=false;
    if symbolprocesshandle<>0 then Symcleanup(symbolprocesshandle); //cleanup first

    SymbolsLoaded:=SymInitialize(thisprocesshandle,nil,true);
    symsetoptions(symgetoptions or SYMOPT_CASE_INSENSITIVE);

    symbolprocesshandle:=processhandle;

  finally
    isloading:=false;
  end;
end;

constructor TSymbolloaderthread.create(CreateSuspended: boolean);
begin
  thisprocesshandle:=processhandle;
  thisprocessid:=processid;
  isloading:=true;
  SymbolsLoaded:=false;

  inherited create(suspended);
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
  if locked then raise exception.Create('You can''t change this setting at the moment');
  fshowmodules:=x;
end;

procedure TSymhandler.setshowsymbols(x: boolean);
begin
  if locked then raise exception.Create('You can''t change this setting at the moment');
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
var previousprocesshandle: thandle;
begin
  loadmodulelist;

  symbolloadervalid.BeginWrite;
  if symbolloaderthread<>nil then
  begin
    symbolloaderthread.WaitFor; //wait till it's done
    symbolloaderthread.Free;
  end;

  symbolloaderthread:=tsymbolloaderthread.Create(false);

  symbolloadervalid.EndWrite;
end;

procedure TSymhandler.Waitforsymbolsloaded;
begin
  symbolloadervalid.beginread;
  if symbolloaderthread<>nil then
    symbolloaderthread.WaitFor;
  symbolloadervalid.endread;
end;

function TSymhandler.DeleteUserdefinedSymbol(symbolname:string):boolean;
var i,j: integer;
begin
  result:=false;
  userdefinedsymbolsMREW.beginwrite;
  for i:=0 to userdefinedsymbolspos-1 do
    if uppercase(userdefinedsymbols[i].symbolname)=uppercase(symbolname) then
    begin
      //found it, now move up all the others and decrease the list
      for j:=i to userdefinedsymbolspos-2 do
        userdefinedsymbols[j]:=userdefinedsymbols[j+1];

      dec(userdefinedsymbolspos);
      result:=true;
      break;
    end;
  userdefinedsymbolsMREW.endwrite;

  if assigned(UserdefinedSymbolCallback) then
    UserdefinedSymbolCallback();
end;

function TSymhandler.GetUserdefinedSymbolByName(symbolname:string):dword;
var i:integer;
begin
  result:=0;
  userdefinedsymbolsMREW.beginread;
  for i:=0 to userdefinedsymbolspos-1 do
    if uppercase(userdefinedsymbols[i].symbolname)=uppercase(symbolname) then
    begin
      result:=userdefinedsymbols[i].address;
      break;
    end;
  userdefinedsymbolsMREW.endread;
end;

function TSymhandler.GetUserdefinedSymbolByAddress(address:dword):string;
var i:integer;
begin
  result:='';
  userdefinedsymbolsMREW.beginread;
  for i:=0 to userdefinedsymbolspos-1 do
    if userdefinedsymbols[i].address=address then
    begin
      result:=userdefinedsymbols[i].symbolname;
      break;
    end;
  userdefinedsymbolsMREW.endread;
end;

procedure TSymhandler.AddUserdefinedSymbol(address: dword; symbolname: string);
begin
  if address=0 then raise exception.Create('You can''t add a symbol with address 0');
  if getuserdefinedsymbolbyname(symbolname)>0 then raise exception.Create(symbolname+' already exists');

  userdefinedsymbolsMREW.beginwrite;
  try
    if userdefinedsymbolspos+1>=length(userdefinedsymbols) then
      setlength(userdefinedsymbols,length(userdefinedsymbols)*2);

    userdefinedsymbols[userdefinedsymbolspos].address:=address;
    userdefinedsymbols[userdefinedsymbolspos].symbolname:=symbolname;
    inc(userdefinedsymbolspos);

    if assigned(UserdefinedSymbolCallback) then
      UserdefinedSymbolCallback();
  finally
    userdefinedsymbolsMREW.endwrite;
  end;
end;

procedure TSymhandler.EnumerateUserdefinedSymbols(list:tstrings);
var i: integer;
begin
  list.Clear;
  userdefinedsymbolsMREW.BeginRead;
  for i:=0 to userdefinedsymbolspos-1 do
    list.Addobject(userdefinedsymbols[i].symbolname,pointer(userdefinedsymbols[i].address));
  userdefinedsymbolsMREW.EndRead;
end;

procedure TSymhandler.fillMemoryRegionsWithModuleData(var mr: TMemoryregions; startaddress: dword; size: dword);
var currentaddress: dword;
    mi: tmoduleinfo;
    sizeleft: dword;
    i: integer;
    closest: integer;
    ok: boolean;
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

        //irst make sure there is a bigger module


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

function TSymhandler.getNameFromAddress(address:dword):string;
var symbol :PImagehlpSymbol;
    offset: dword;
    s: string;
    mi: tmoduleinfo;
begin
  if self.showsymbols then
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

    //check the userdefined symbols
    result:=self.GetUserdefinedSymbolByAddress(address);
    if result<>'' then exit;
  end;


  if self.showmodules then
  begin
    //get the dllname+offset
    if getmodulebyaddress(address,mi) then
    begin
      if address-mi.baseaddress=0 then
        result:=mi.modulename
      else
        result:=mi.modulename+'+'+inttohex(address-mi.baseaddress,1);
      exit;
    end;
  end;

  result:=inttohex(address,8);  //default
end;

function TSymhandler.getAddressFromName(name:string):dword;
begin
  result:=getAddressFromName(name,true);
end;

function TSymhandler.getAddressFromName(name: string; waitforsymbols: boolean):dword;
var mi: tmoduleinfo;
    newaddress: string;
    symbol :PImagehlpSymbol;
    oldoptions: dword;

    offset: dword;

    sn: string;
    i: integer;
begin
  result:=0;

  val('$'+name,result,i);
  if i=0 then exit; //it's a valid hexadecimal string


  try
    //first cut of the name from the offset
    offset:=0;
    for i:=length(name)-1 downto 1 do
      if name[i] in ['+','-'] then
      begin
        sn:=copy(name,i+1,length(name));
        offset:=strtoint('$'+sn);

        if name[i]='-' then
          offset:=-offset;

        name:=copy(name,1,i-1);
        break;
      end;

  except
    raise exception.create(sn+' is not a valid value');
  end;

  val('$'+name,result,i);
  if i=0 then
  begin
    result:=result+offset;
    exit; //it was a simple +/- calculation
  end;


  //see if it is a module
  if getmodulebyname(name,mi) then
    result:=mi.baseaddress+offset
  else
  begin
    //if not, see if you can find it as a symbol
    //first check the userdefined symbols (small list, so faster than the symbols)
    result:=GetUserdefinedSymbolByName(name);
    if result<>0 then exit;


    symbolloadervalid.beginread;
    try
      if (symbolloaderthread<>nil) then
      begin
        if symbolloaderthread.isloading and not waitforsymbols then
          raise exception.create('This is not a valid address');

        symbolloaderthread.WaitFor; //wait for it to finish if it's still busy
        //it's not a valid address, it's not a calculation, it's not a modulename+offset, so lets see if it's a module

        getmem(symbol,sizeof(IMAGEHLP_SYMBOL)+255);
        try
          zeromemory(symbol,sizeof(IMAGEHLP_SYMBOL)+255);
          symbol.SizeOfStruct:=sizeof(IMAGEHLP_SYMBOL)+255;
          symbol.MaxNameLength:=254;
          if SymGetSymFromName(processhandle,pchar(name),symbol^) then
            result:=symbol.Address+offset
          else
            raise exception.Create('This is not a valid address');   //no hex string, no module, no symbol, so invalid
        finally
          freemem(symbol);
        end;
      end else
        raise exception.Create('This is not a valid address');

    finally
      symbolloadervalid.endread;
    end;
  end
end;


procedure TSymhandler.loadmodulelist;
var
  ths: thandle;
  me32:MODULEENTRY32;
  x: pchar;
begin
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

constructor TSymhandler.create;
begin
  symbolloadervalid:=TMultiReadExclusiveWriteSynchronizer.create;
  modulelistMREW:=TMultiReadExclusiveWriteSynchronizer.create;
  userdefinedsymbolsMREW:=TMultireadExclusiveWriteSynchronizer.create;

  setlength(userdefinedsymbols,32);
  setlength(modulelist,32);

  showmodules:=false;
  showsymbols:=true;
end;



initialization
  symbolprocesshandle:=0;
  symhandler:=tsymhandler.create;

finalization
  symhandler.free;

end.
