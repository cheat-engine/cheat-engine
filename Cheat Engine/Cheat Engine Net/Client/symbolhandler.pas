unit symbolhandler;

//clientstub for the network version

interface

uses classes,cefuncproc,windows,imagehlp,sysutils,syncobjs,tlhelp32;

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
    SymbolsLoaded:=true;
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
end;

function TSymhandler.GetUserdefinedSymbolByName(symbolname:string):dword;
var i:integer;
begin
  result:=0;
end;

function TSymhandler.GetUserdefinedSymbolByAddress(address:dword):string;
var i:integer;
begin
  result:='';
end;

procedure TSymhandler.AddUserdefinedSymbol(address: dword; symbolname: string);
begin

end;

procedure TSymhandler.EnumerateUserdefinedSymbols(list:tstrings);
var i: integer;
begin
  list.Clear;

end;

function TSymhandler.getmodulebyaddress(address: dword; var mi: TModuleInfo):BOOLEAN;
var i: integer;
begin
  result:=false;

end;

function TSymhandler.getmodulebyname(modulename: string; var mi: TModuleInfo):BOOLEAN;
var i: integer;
begin
  result:=false;

end;

function TSymhandler.getNameFromAddress(address:dword):string;
var symbol :PImagehlpSymbol;
    offset: dword;
    s: string;
    mi: tmoduleinfo;
begin
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

end;


procedure TSymhandler.loadmodulelist;
var
  ths: thandle;
  me32:MODULEENTRY32;
  x: pchar;
begin
  modulelistpos:=0;
 
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

end.
