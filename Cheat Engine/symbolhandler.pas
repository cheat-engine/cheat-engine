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

type symexception=class(Exception);


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
    symbolsloaded: boolean;

    kernelsymbols: boolean;
    dllsymbols: boolean;
    searchpath: string;

    procedure execute; override;
    constructor create(targetself, CreateSuspended: boolean);
    destructor destroy; override;
  end;

  TSymHandler=class
  private
    symbolloaderthread: TSymbolloaderthread;


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
    searchpath: string;

    function getusedprocesshandle :thandle;
    function getusedprocessid:dword;
    function getisloaded:boolean;

    procedure setshowmodules(x: boolean);
    procedure setshowsymbols(x: boolean);
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
    procedure waitforsymbolsloaded;
    procedure reinitialize;
    procedure loadmodulelist;
    procedure fillMemoryRegionsWithModuleData(var mr: TMemoryregions; startaddress: dword; size: dword);
    procedure getModuleList(list: tstrings);
    function getmodulebyaddress(address: dword; var mi: TModuleInfo):BOOLEAN;
    function getmodulebyname(modulename: string; var mi: TModuleInfo):BOOLEAN;
    function getNameFromAddress(address:dword):string; overload;
    function getNameFromAddress(address:dword;symbols:boolean; modules: boolean):string; overload;
    function getAddressFromName(name: string):dword; overload;
    function getAddressFromName(name: string; waitforsymbols: boolean):dword; overload;

    function getsearchpath:string;
    procedure setsearchpath(path:string);

    //userdefined symbols
    function DeleteUserdefinedSymbol(symbolname:string):boolean;
    function GetUserdefinedSymbolByName(symbolname:string):dword;
    function GetUserdefinedSymbolByAddress(address:dword):string;
    procedure AddUserdefinedSymbol(address: dword; symbolname: string);
    procedure EnumerateUserdefinedSymbols(list:tstrings);

    function ParseAsPointer(s: string; list:tstrings): boolean;
    function GetAddressFromPointer(s: string; var error: boolean):dword;


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
    nearest: dword; //nearest other driver (AFTER win32k.sys)
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
    symsetoptions(symgetoptions or SYMOPT_CASE_INSENSITIVE);
    symsetsearchpath(processhandle,pchar(searchpath));

    if kernelsymbols then LoadDriverSymbols;

    LoadDLLSymbols;

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
var previousprocesshandle: thandle;
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
  if address=0 then raise symexception.Create('You can''t add a symbol with address 0');
  if getuserdefinedsymbolbyname(symbolname)>0 then raise symexception.Create(symbolname+' already exists');

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
{
This routine will fill in a TMemoryRegions array with the base and startaddress of the modules it found
}
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


function TSymhandler.getNameFromAddress(address:dword;symbols:boolean; modules: boolean):string;
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


  if modules then
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

function TSymhandler.getNameFromAddress(address:dword):string;
begin
  result:=getNameFromAddress(address,self.showsymbols,self.showmodules);
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

    ws: widestring;
    pws: pwidechar;
    error: boolean;

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

  result:=0;

  val(ConvertHexStrToRealStr(name),result,i);
  if i=0 then exit; //it's a valid hexadecimal string


  try
    //first cut of the name from the offset
    offset:=0;
    for i:=length(name) downto 1 do
      if name[i] in ['+','-'] then
      begin
        sn:=copy(name,i+1,length(name));
        offset:=strtoint('$'+sn);

        if name[i]='-' then
          offset:=-offset;

        name:=copy(name,1,i-1);
        break;
      end
      else
      if name[i] in ['[',']'] then
        break;


  except
    raise symexception.create(sn+' is not a valid value');
  end;

  if name='' then name:='0';
  val('$'+name,result,i);
  if i=0 then
  begin
    result:=result+offset;
    exit; //it was a simple +/- calculation
  end;

  {
  debugger hell:
  tools->debugger options->Language Exceptions
  click add...
  type in "symexception" without the quotes

  this will cause you to still break on normal exception like memory access violations, but not on these
  }
  if getreg(uppercase(name),false)<>9 then
    raise symexception.create('Register'); //can happen in case of eax+# //speed improvement

   
  //see if it is a module
  if getmodulebyname(name,mi) then
    result:=mi.baseaddress+offset
  else
  begin


    //if not, see if you can find it as a symbol
    //first check the userdefined symbols (small list, so faster than the symbols)
    result:=GetUserdefinedSymbolByName(name);
    if result<>0 then
    begin
      result:=result+offset;
      exit;
    end;

    {$ifndef autoassemblerdll}
    if (darkbytekernel<>0) and (length(name)>6) and (pos('KERNEL_',uppercase(name))>0) then
    begin
      name:=copy(name,8,length(name)-7);
      ws:=name;
      pws:=@ws[1];
      result:=dword(GetKProcAddress(pws));
      if result<>0 then
      begin
        result:=result+offset;
        exit;
      end;
    end;
    {$endif}

    { 5.4: check if it is a pointer notation }
    result:=GetAddressFromPointer(name,error);
    if not error then
    begin
      result:=result+offset;
      exit;
    end;
    { 5.4 ^^^ }


    symbolloadervalid.beginread;
    try
      if (symbolloaderthread<>nil) then
      begin
        if symbolloaderthread.isloading and not waitforsymbols then
          raise symexception.create('This is not a valid address');

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
            raise symexception.Create('This is not a valid address');   //no hex string, no module, no symbol, so invalid
        finally
          freemem(symbol);
        end;
      end else
        raise symexception.Create('This is not a valid address');

    finally
      symbolloadervalid.endread;
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


destructor TSymhandler.destroy;
begin
  if symbolloaderthread<>nil then
  begin
    symbolloaderthread.Terminate;
    symbolloaderthread.WaitFor;
    symbolloaderthread.free;
  end;


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
  selfsymhandler.free;
  symhandler.free;
  
end.





