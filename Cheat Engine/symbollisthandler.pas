unit SymbolListHandler;
{
This unit will keep two trees that link to a list of string to address information records for quick lookup
}

{$mode objfpc}
{$H+}

interface

{$ifdef windows}
uses
  windows, Classes, SysUtils, AvgLvlTree, laz_avl_Tree, math, fgl, cvconst,
  syncobjs, symbolhandlerstructs, DOM;
{$endif}

{$ifdef darwin}
uses
  macport, Classes, SysUtils, AvgLvlTree, math, fgl, cvconst, syncobjs, symbolhandlerstructs;
{$endif}

type
  PSYMBOL_INFO = ^TSYMBOL_INFO;
  (*
  typedef struct _SYMBOL_INFO {
      ULONG       SizeOfStruct;
      ULONG       TypeIndex;        // Type Index of symbol
      ULONG64     Reserved[2];
      ULONG       Index;
      ULONG       Size;
      ULONG64     ModBase;          // Base Address of module comtaining this symbol
      ULONG       Flags;
      ULONG64     Value;            // Value of symbol, ValuePresent should be 1
      ULONG64     Address;          // Address of symbol including base address of module
      ULONG       Register;         // register holding value or pointer to value
      ULONG       Scope;            // scope of the symbol
      ULONG       Tag;              // pdb classification
      ULONG       NameLen;          // Actual length of name
      ULONG       MaxNameLen;
      CHAR        Name[1];          // Name of symbol
  } SYMBOL_INFO, *PSYMBOL_INFO;
 *)
  TSYMBOL_INFO = {packed} record
          SizeOfStruct : ULONG;
          TypeIndex : ULONG;
          Reserved : array[0..1] of ULONG64;
          index : ULONG;
          Size : ULONG;
          ModBase : ULONG64;
          Flags : ULONG;
          Value : ULONG64;
          Address : ULONG64; //it's more a signed address
          Reg : ULONG;
          Scope : ULONG;
          Tag : ULONG;
          NameLen : ULONG;
          MaxNameLen : ULONG;
          Name : array[0..0] of char;
       end;
  SYMBOL_INFO = TSYMBOL_INFO;
  LPSYMBOL_INFO = PSYMBOL_INFO;

type
  TExtraSymbolDataEntry=class
    name: string;            //rebuild ce instead of compile if you see this error. (or perhaps FPC fixes this one day)
    vtype: string;
    position: string;
    syminfo: TSYMBOL_INFO; //just in case it might be useful
  end;

  TExtraSymbolDataEntryList=specialize TFPGList<TExtraSymbolDataEntry>;

  TExtraSymbolData=class
  private
  public
    symboladdress: ptruint; //used to fill in the rest
    filledin: boolean;

    return: string;
    simpleparameters: string; //either simple or the parameters list
    parameters: TExtraSymbolDataEntryList;
    locals: TExtraSymbolDataEntryList;

    forwarder: boolean;
    forwardsTo: ptruint;
    forwardsToString: string;

    constructor create;
    destructor destroy; override;
  end;


  PCESymbolInfo=^TCESymbolInfo;
  TCESymbolInfo=record
    s: pchar; //lowercase string for searching
    originalstring: pchar;
    module: pchar;
    address: qword;
    size: integer;
    extra:TExtraSymbolData;

    previous: PCESymbolInfo;
    next: PCESymbolInfo;

    alternative: PCESymbolInfo; //chain of duplicates
  end;

  TExtraSymbolDataList=TList;


  TExtraModuleInfo=record
          modulename: string;
          modulepath: string;
          baseaddress: ptrUint;
          modulesize: dword;
          is64bitmodule: boolean;
        end;

  TExtraModuleInfoList=array of TExtraModuleInfo;

  TSymbolListHandler=class;

  TSymbolListHandler=class
  private
    cs: TMultiReadExclusiveWriteSynchronizer;

    modulelist: array of TExtraModuleInfo;

    AddressToString: TAvgLvlTree;
    StringToAddress: TAvgLvlTree;

    fExtraSymbolDataList: TExtraSymbolDataList;
    fPID: dword;
    fname: string;
    frefcount: integer;
    fInternalName: string;
    function A2SCheck(Tree: TAvgLvlTree; Data1, Data2: pointer): integer;
    function S2ACheck(Tree: TAvgLvlTree; Data1, Data2: pointer): integer;
    function getCount: integer;

    procedure init;
  public
    constructor create;
    constructor create(_name, _internalname: string);
    destructor destroy; override;
    procedure AddExtraSymbolData(d: TExtraSymbolData);
    procedure RemoveExtraSymbolData(d: TExtraSymbolData);
    procedure AddModule(module:string; path: string; base: ptruint; size: dword; is64bit: boolean);
    procedure DeleteModule(module: string); overload;
    procedure DeleteModule(base: qword); overload;
    function GetModuleByAddress(address: ptrUint; var mi: TModuleInfo):BOOLEAN;
    function getmodulebyname(modulename: string; var mi: TModuleInfo):BOOLEAN;
    procedure GetModuleList(var list: TExtraModuleInfoList);
    procedure GetSymbolList(list: TStrings);
    function AddSymbol(module: string; searchkey: string; address: qword; size: integer; skipaddresstostringlookup: boolean=false; extradata: TExtraSymbolData=nil; skipDuplicateSupport:boolean=false): PCESymbolInfo;
    function FindAddress(address: qword): PCESymbolInfo;
    function FindSymbol(s: string): PCESymbolInfo;
    function FindFirstSymbolFromBase(baseaddress: qword): PCESymbolInfo;

    procedure LoadFromXML(e: TDOMElement);
    procedure SaveToXML(e: TDOMElement);
    function SyncSymbols(otherlist: TSymbolListHandler; dontdelete: boolean; applychanges: boolean):boolean;

    procedure DeleteSymbol(searchkey: string); overload;
    procedure DeleteSymbol(address: qword); overload;
    procedure clear;
    procedure unregisterList;
  published
    property ExtraSymbolDataList: TExtraSymbolDataList read fExtraSymbolDataList;
    property PID: dword read fPID write fPID;
    property count: integer read getCount;
    property name: string read fName write fName;
    property internalname: string read fInternalName;
    property refcount: integer read frefcount write frefcount;
  end;

  TSymbolListHandlerArray=array of TSymbolListHandler;


implementation


uses CEFuncProc, symbolhandler, ProcessHandlerUnit;




constructor TExtraSymbolData.create;
begin
  parameters:=TExtraSymbolDataEntryList.create;
  locals:=TExtraSymbolDataEntryList.create;
end;

destructor TExtraSymbolData.destroy;
var i: integer;
begin
  for i:=0 to parameters.count-1 do
    parameters[i].free;

  if parameters<>nil then
    parameters.free;

  for i:=0 to locals.count-1 do
    locals[i].free;

  if locals<>nil then
    locals.free;

  inherited destroy;
end;

//-------------

function TSymbolListHandler.getCount: integer;
begin
  cs.Beginread;
  result:=StringToAddress.Count;
  cs.Endread;
end;

procedure TSymbolListHandler.AddModule(module:string; path: string; base: ptruint; size: dword; is64bit: boolean);
var i: integer;
begin
  cs.Beginwrite;
  i:=length(modulelist);
  setlength(modulelist, length(modulelist)+1);
  modulelist[i].modulename:=module;
  modulelist[i].modulepath:=path;
  modulelist[i].baseaddress:=base;
  modulelist[i].modulesize:=size;
  modulelist[i].is64bitmodule:=is64bit;
  cs.Endwrite;
end;

procedure TSymbolListHandler.DeleteModule(module: string);
var i,j: integer;
begin
  cs.beginwrite;
  for i:=0 to length(modulelist)-1 do
  begin
    if modulelist[i].modulename=module then
    begin
      for j:=i to length(modulelist)-2 do
        modulelist[j]:=modulelist[j+1];

      setlength(modulelist,length(modulelist)-1);
    end;
  end;
  cs.endwrite;
end;

procedure TSymbolListHandler.DeleteModule(base: qword);
var i,j: integer;
begin
  cs.beginwrite;
  for i:=0 to length(modulelist)-1 do
  begin
    if modulelist[i].baseaddress=base then
    begin
      for j:=i to length(modulelist)-2 do
        modulelist[j]:=modulelist[j+1];

      setlength(modulelist,length(modulelist)-1);
    end;
  end;
  cs.endwrite;
end;

function TSymbolListHandler.GetModuleByAddress(address: ptrUint; var mi: TModuleInfo):BOOLEAN;
var i: integer;
begin
  result:=false;
  cs.Beginread;
  for i:=0 to length(modulelist)-1 do
  begin
    if (address>=modulelist[i].baseaddress) and (address<modulelist[i].baseaddress+modulelist[i].modulesize) then
    begin
      mi.modulename:=modulelist[i].modulename;
      mi.modulepath:=modulelist[i].modulepath;
      mi.isSystemModule:=false;
      mi.baseaddress:=modulelist[i].baseaddress;
      mi.basesize:=modulelist[i].modulesize;
      mi.is64bitmodule:=modulelist[i].is64bitmodule;
      mi.symbolsLoaded:=true;
      mi.hasStructInfo:=false;
      mi.databaseModuleID:=0;
      result:=true;
      break;
    end;
  end;

  cs.endread;
end;

function TSymbolListHandler.getmodulebyname(modulename: string; var mi: TModuleInfo):BOOLEAN;
//pre:modulename is already sanitized and uppercase
var i: integer;
begin
  result:=false;
  cs.Beginread;
  for i:=0 to length(modulelist)-1 do
  begin
    if uppercase(modulelist[i].modulename)=modulename then
    begin
      mi.modulename:=modulelist[i].modulename;
      mi.modulepath:=modulelist[i].modulepath;
      mi.isSystemModule:=false;
      mi.baseaddress:=modulelist[i].baseaddress;
      mi.basesize:=modulelist[i].modulesize;
      mi.is64bitmodule:=modulelist[i].is64bitmodule;
      mi.symbolsLoaded:=true;
      mi.hasStructInfo:=false;
      mi.databaseModuleID:=0;
      result:=true;
      break;
    end;
  end;
  cs.Endread;
end;

procedure TSymbolListHandler.GetSymbolList(list: TStrings);
var si: PCESymbolInfo;
begin
  list.clear;
  cs.Beginread;
  si:=FindFirstSymbolFromBase(0);

  while si<>nil do
  begin
    list.AddObject(si^.originalstring, tobject(ptruint(si^.address)));
    si:=si^.next;
  end;

  cs.endread;
end;

procedure TSymbolListHandler.GetModuleList(var list: TExtraModuleInfoList);
var i: integer;
begin
  cs.Beginread;
  setlength(list, length(modulelist));
  for i:=0 to length(list)-1 do
    list[i]:=modulelist[i];

  cs.Endread;
end;

function TSymbolListHandler.FindFirstSymbolFromBase(baseaddress: qword): PCESymbolInfo;
var search: TCESymbolInfo;
  x: PCESymbolInfo;
  z: TAvgLvlTreeNode;
begin
  result:=nil;
  search.address:=baseaddress;

  cs.Beginread;
  try
    z:=AddressToString.FindNearest(@search);
    if z<>nil then
    begin
      x:=PCESymbolInfo(z.data);

      while (x<>nil) and (x^.address<baseaddress) do
        x:=x^.next;

      result:=x;
    end;

  finally
    cs.Endread;
  end;
end;

function TSymbolListHandler.FindAddress(address: qword): PCESymbolInfo;
var search: TCESymbolInfo;
  x: PCESymbolInfo;
  z: TAvgLvlTreeNode;
begin
  //keep in mind of duplicates
  result:=nil;
  search.address:=address;;

  cs.Beginread;
  try


    z:=AddressToString.FindNearest(@search);

    if z<>nil then
    begin
      //check if it's a match, and if not, check if it's too big or too small

      x:=PCESymbolInfo(z.data);
      if x^.address=address then
      begin
        result:=x;
        exit;
      end
      else
      if x^.address<address then
      begin
        //if too small, check if it fits inside, else try the next one untill x.address>address or x=nil
        while (x<>nil) and (x^.address<=address) do
        begin
          if InRangeQ(address, x^.address, x^.address+x^.size) then
          begin
            result:=x;
            exit;
          end;

          //still here so not valid
          x:=x^.next;
        end;
      end
      else
      begin
        //if too big, check the previous one, until x.address+x.size < address or x=nil
        while (x<>nil) and (x^.address+x^.size>address) do
        begin
          if InRangeQ(address, x^.address, x^.address+x^.size) then
          begin
            result:=x;
            exit;
          end;

          //still here so not valid
          x:=x^.previous;
        end;
      end;


    end;

  finally
    cs.Endread;
  end;
end;

function TSymbolListHandler.FindSymbol(s: string): PCESymbolInfo;
var x: TCESymbolInfo;
  z: TAvgLvlTreeNode;
begin
  s:=lowercase(s);
  x.s:=@s[1];

  cs.Beginread;
  try

    z:=StringToAddress.Find(@x);
    if z<>nil then
      result:=z.data
    else
      result:=nil;

  finally
    cs.Endread;
  end;
end;

function TSymbolListHandler.AddSymbol(module: string; searchkey: string; address: qword; size: integer; skipaddresstostringlookup: boolean=false; extradata: TExtraSymbolData=nil; skipDuplicateSupport:boolean=false): PCESymbolInfo;
var new: PCESymbolInfo;
  n: TAvgLvlTreeNode;
  prev, next: TAvgLvlTreeNode;
  x: PCESymbolInfo;
begin
  new:=getmem(sizeof(TCESymbolInfo));
  new^.module:=strnew(pchar(module));
  new^.originalstring:=strnew(pchar(searchkey));
  new^.s:=strnew(pchar(lowercase(searchkey)));
  new^.address:=address;
  new^.size:=size;
  new^.alternative:=nil;
  new^.extra:=extradata;

  result:=new;

  cs.Beginwrite;
//  sleep(1);
  try
    if not skipaddresstostringlookup then
    begin
      n:=AddressToString.Add(new);
      prev:=AddressToString.FindPrecessor(n);
      next:=AddressToString.FindSuccessor(n);

      if prev=nil then
        new^.previous:=nil
      else
      begin
        new^.previous:=prev.Data;
        PCESymbolInfo(prev.data)^.next:=new;
      end;

      if next=nil then
        new^.next:=nil
      else
      begin
        new^.next:=next.Data;
        PCESymbolInfo(next.data)^.previous:=new;
      end;
    end;

    if skipDuplicateSupport=false then
    begin
      n:=StringToAddress.Find(new);
      if (n<>nil) and (PCESymbolInfo(n.data)^.address<>new^.address)  then
      begin
        //different symbol, same name
        x:=PCESymbolInfo(n.data);
        while x^.alternative<>nil do //chain duplicates
        begin
          x:=x^.alternative;
          if x^.address=new^.address then exit; //duplicate symbol. Same name and address.
        end;

        x^.alternative:=new;
      end
      else
        n:=StringToAddress.Add(new);
    end
    else
      n:=StringToAddress.Add(new);

  finally
    cs.Endwrite;
  end;

end;

function TSymbolListHandler.A2SCheck(Tree: TAvgLvlTree; Data1, Data2: pointer): integer;
begin
  result:=comparevalue(PCESymbolInfo(data1)^.address, PCESymbolInfo(data2)^.address);

end;

function TSymbolListHandler.S2ACheck(Tree: TAvgLvlTree; Data1, Data2: pointer): integer;
begin
  result:=CompareStr(PCESymbolInfo(data1)^.s,PCESymbolInfo(data2)^.s);
end;

procedure TSymbolListHandler.DeleteSymbol(address: qword);
var
  x: TCESymbolInfo;
  z: TAvgLvlTreeNode;
  d: PCESymbolInfo;
begin
  x.address:=address;

  cs.Beginwrite;
  try

    z:=AddressToString.Find(@x);
    if z<>nil then
    begin
      d:=PCESymbolInfo(z.data);

      x.s:=d^.s;

      d^.address:=0;
      d^.next:=nil;
      d^.previous:=nil;


      if d^.originalstring<>nil then
      begin
        StrDispose(d^.originalstring);
        d^.originalstring:=nil;
      end;

      if d^.s<>nil then
      begin
        StrDispose(d^.s);
        d^.s:=nil;
      end;

      if d^.module<>nil then
      begin
        strDispose(d^.module);
        d^.module:=nil;
      end;

      AddressToString.Delete(z);

      //delete the addresstostring reference as well
      z:=StringToAddress.Find(@x);
      if z<>nil then
      begin
        d:=PCESymbolInfo(z.data);

        d^.address:=0;
        d^.next:=nil;
        d^.previous:=nil;

        if d^.originalstring<>nil then
        begin
          StrDispose(d^.originalstring);
          d^.originalstring:=nil;
        end;

        if d^.s<>nil then
        begin
          StrDispose(d^.s);
          d^.s:=nil;
        end;

        if d^.module<>nil then
        begin
          strDispose(d^.module);
          d^.module:=nil;
        end;

        StringToAddress.Delete(z);
      end;
    end;
  finally
    cs.Endwrite;
  end;

end;

procedure TSymbolListHandler.DeleteSymbol(searchkey: string);
var
  x: TCESymbolInfo;
  z: TAvgLvlTreeNode;
  d: PCESymbolInfo;
  s: string;
begin
  s:=lowercase(searchkey);
  x.s:=pchar(s);

  cs.Beginwrite;
  try

    z:=StringToAddress.Find(@x);
    if z<>nil then
    begin
      d:=PCESymbolInfo(z.data);

      x.address:=d^.address;

      d^.address:=0;
      d^.next:=nil;
      d^.previous:=nil;

      if d^.originalstring<>nil then
      begin
        StrDispose(d^.originalstring);
        d^.originalstring:=nil;
      end;

      if d^.s<>nil then
      begin
        StrDispose(d^.s);
        d^.s:=nil;
      end;

      if d^.module<>nil then
      begin
        strDispose(d^.module);
        d^.module:=nil;
      end;

      StringToAddress.Delete(z);

      //delete the addresstostring reference as well
      z:=AddressToString.Find(@x);
      if z<>nil then
      begin
        d:=PCESymbolInfo(z.data);

        d^.address:=0;
        d^.next:=nil;
        d^.previous:=nil;

        if d^.originalstring<>nil then
        begin
          StrDispose(d^.originalstring);
          d^.originalstring:=nil;
        end;

        if d^.s<>nil then
        begin
          StrDispose(d^.s);
          d^.s:=nil;
        end;

        if d^.module<>nil then
        begin
          strDispose(d^.module);
          d^.module:=nil;
        end;

        AddressToString.Delete(z);
      end;
    end;


  finally
    cs.Endwrite;
  end;
end;

procedure CleanSymbolInfoEntry(var d: PCESymbolInfo);
begin
  if d^.originalstring<>nil then
    StrDispose(d^.originalstring);

  if d^.s<>nil then
    StrDispose(d^.s);

  if d^.module<>nil then
    strDispose(d^.module);

  if d^.alternative<>nil then
  begin
    CleanSymbolInfoEntry(d^.alternative);
    freememandnil(d^.alternative);
  end;

  freememandnil(d);
end;

procedure TSymbolListHandler.clear;
var
  x: TAvgLvlTreeNode;
  d:PCESymbolInfo;
  i: integer;

  //e: TAVLTreeNodeEnumerator;
begin
  cs.Beginwrite;
  try
    if StringToAddress<>nil then
    begin
      x:=StringToAddress.FindLowest;
      while x<>nil do
      begin
        d:=PCESymbolInfo(x.Data);
        CleanSymbolInfoEntry(d);
        x.data:=nil;
        x:=StringToAddress.FindSuccessor(x);
      end;



      StringToAddress.Clear;
    end;

    if AddressToString<>nil then
      AddressToString.Clear;

    for i:=0 to ExtraSymbolDataList.count-1 do
      TExtraSymbolData(ExtraSymbolDataList[i]).free;

    ExtraSymbolDataList.clear;

  finally
    cs.endwrite;
  end;
end;

procedure TSymbolListHandler.AddExtraSymbolData(d: TExtraSymbolData);
begin //add here instead of AddSymbol, since AddSymbol can add the same object multiple times
  fExtraSymbolDataList.add(d);
end;

procedure TSymbolListHandler.RemoveExtraSymbolData(d: TExtraSymbolData);
begin
  fExtraSymbolDataList.Remove(d);
end;

procedure TSymbolListHandler.unregisterList;
begin
  if selfsymhandler<>nil then selfsymhandler.RemoveSymbolList(self);
  if symhandler<>nil then symhandler.RemoveSymbolList(self);
end;

procedure TSymbolListHandler.LoadFromXML(e: TDOMElement);
var
  i: integer;
  s: TDOMElement;
  sname, saddress, ssize, sis64bit, spath:  string;
  smodulename: string;
  address: ptruint;
  size: integer;
  is64bit: boolean;

begin
  for i:=0 to e.ChildNodes.Count-1 do
  begin
    if e.ChildNodes[i] is TDOMElement then
    begin
      s:=TDOMElement(e.ChildNodes[i]);
      if s.NodeName='module' then
      begin
        is64bit:=processhandler.is64Bit;

        sname:=s.AttribStrings['name'];
        saddress:=s.AttribStrings['address'];
        ssize:=s.AttribStrings['size'];
        sis64bit:=s.AttribStrings['is64bit'];
        spath:=s.AttribStrings['path'];


        if (sname='') or (saddress='') or (ssize='') then continue;


        try
          address:=StrToInt64(saddress);
          size:=StrToInt(ssize)
        except
          continue;
        end;


        if sis64bit<>'' then
          is64bit:=sis64bit='1';

        AddModule(sname,spath,address,size,is64bit);
      end
      else if s.NodeName='symbol' then
      begin
        sname:=s.AttribStrings['name'];
        saddress:=s.AttribStrings['address'];
        ssize:=s.AttribStrings['size'];
        smodulename:=s.AttribStrings['modulename'];

        if (sname='') or (saddress='') then continue;

        try
          address:=StrToInt64(saddress);

          if ssize<>'' then
            size:=ssize.ToInteger
          else
            size:=1;
        except
          continue;
        end;

        AddSymbol(smodulename, sname, address, size);
      end;

    end;
  end;
end;

procedure TSymbolListHandler.SaveToXML(e: TDOMElement);
var
  e2: TDOMElement;
  enum: TAVLTreeNodeEnumerator;
  si: PCESymbolInfo;
  i: integer;
begin
  cs.Beginread;
  for i:=0 to length(modulelist)-1 do
  begin
    e2:=TDOMElement(e.AppendChild(e.OwnerDocument.CreateElement('module')));

    e2.AttribStrings['name']:=modulelist[i].modulename;
    e2.AttribStrings['address']:=modulelist[i].baseaddress.ToString;
    e2.AttribStrings['size']:=modulelist[i].modulesize.ToString;
    if modulelist[i].is64bitmodule then
      e2.AttribStrings['is64bit']:='1'
    else
      e2.AttribStrings['is64bit']:='0';

    if modulelist[i].modulepath<>'' then
      e2.AttribStrings['path']:=modulelist[i].modulepath;
  end;

  enum:=AddressToString.GetEnumerator;
  while enum.MoveNext do
  begin
    si:=PCESymbolInfo(enum.Current.Data);
    e2:=TDOMElement(e.AppendChild(e.OwnerDocument.CreateElement('symbol')));

    e2.AttribStrings['name']:=si^.originalstring;
    e2.AttribStrings['address']:=si^.address.ToString;
    e2.AttribStrings['size']:=si^.size.ToString;
    e2.AttribStrings['modulename']:=si^.module;
  end;
  enum.free;

  cs.Endread;
end;

function TSymbolListHandler.SyncSymbols(otherlist: TSymbolListHandler; dontdelete: boolean; applychanges: boolean):boolean;
var
  i,j: integer;
  found: boolean;
  e: TAVLTreeNodeEnumerator;
  si,si2: PCESymbolInfo;
  sicopy: PCESymbolInfo;
  n: TAVLTreeNode;
begin
  result:=false;



  if applychanges then
    cs.Beginwrite
  else
    cs.Beginread;

  try
    if name<>otherlist.name then
    begin
      result:=true;
      if applychanges then
        name:=otherlist.name;
    end;


    //add the missing modules to otherlist first
    for i:=0 to length(modulelist)-1 do
    begin
      found:=false;
      for j:=0 to length(otherlist.modulelist)-1 do
        if modulelist[i].modulename=otherlist.modulelist[j].modulename then
        begin
          if modulelist[i].baseaddress<>otherlist.modulelist[j].baseaddress then result:=true; //different base
          found:=true;
          break;
        end;

      if (not found) and dontdelete then
      begin
        otherlist.AddModule(modulelist[i].modulename, modulelist[i].modulepath, modulelist[i].baseaddress, modulelist[i].modulesize, modulelist[i].is64bitmodule);
        result:=true;
      end;
    end;

    //then the symbols
    e:=StringToAddress.GetEnumerator;
    while e.MoveNext do
    begin
      si:=PCESymbolInfo(e.Current.Data);
      n:=otherlist.StringToAddress.Find(si);
      if n=nil then
      begin
        if dontdelete then
          otherlist.AddSymbol(si^.module, si^.originalstring, si^.address, si^.size,false, si^.extra);

        result:=true; //not found, so a change
      end
      else
      begin
        si2:=PCESymbolInfo(n.Data);
        if (result=false) and ((si^.address<>si2^.address) or (String(si^.originalstring)<>string(si2^.originalstring)) or (si^.size<>si2^.size)) then
          result:=true;
      end;
    end;
    e.free;


    if applychanges then
    begin
      clear;
      setlength(modulelist, length(otherlist.modulelist));
      for i:=0 to length(modulelist)-1 do
        modulelist[i]:=otherlist.modulelist[i];

      e:=otherlist.AddressToString.GetEnumerator;
      while e.MoveNext do
      begin
        si:=PCESymbolInfo(e.Current.Data);
        AddSymbol(si^.module, si^.originalstring, si^.address, si^.size, false, si^.extra);
      end;
      e.free;

    end;



  finally
    if applychanges then
      cs.endwrite
    else
      cs.Endread;
  end;

end;

procedure TSymbolListHandler.init;
begin
  AddressToString:=TAvgLvlTree.CreateObjectCompare(@A2SCheck);
  StringToAddress:=TAvgLvlTree.CreateObjectCompare(@S2ACheck);
  fExtraSymbolDataList:=TExtraSymbolDataList.create;
  cs:=TMultiReadExclusiveWriteSynchronizer.create;
  frefcount:=1;

  fname:='unnamed';
end;

constructor TSymbolListHandler.create;
begin
  inherited create;
  init;
  fInternalName:=TGUID.NewGuid.toString;
end;

constructor TSymbolListHandler.create(_name, _internalname: string);
begin
  inherited create;
  init;

  fname:=_name;
  fInternalName:=_internalname;
end;

destructor TSymbolListHandler.destroy;
var i: integer;
begin
  unregisterList;





  clear;
  if AddressToString<>nil then
    freeandnil(AddressToString);

  if StringToAddress<>nil then
    freeandnil(StringToAddress);

  if cs<>nil then
    freeandnil(cs);

  for i:=0 to ExtraSymbolDataList.count-1 do
    TExtraSymbolData(ExtraSymbolDataList[i]).free;

  ExtraSymbolDataList.clear;
  ExtraSymbolDataList.Free;

  inherited destroy;
end;

end.

