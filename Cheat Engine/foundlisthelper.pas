unit foundlisthelper;

{$MODE Delphi}

//todo: Change the list to the same kind of list used in the pointerscan (so no more concatenating the results)


interface




{$ifdef jni}
//in cecore the foundlisthelper is data only. No link to a listview
uses sysutils,classes, symbolhandler, ProcessHandlerUnit, NewKernelHandler, memscan,
     byteinterpreter, CustomTypeHandler, groupscancommandparser, math, AvgLvlTree,
     commonTypeDefs, parsers, unixporthelper;
{$else}
uses {$ifdef darwin}macport,{$endif}
     sysutils,classes,ComCtrls,StdCtrls, symbolhandlerstructs,
     NewKernelHandler, memscan, CustomTypeHandler, byteinterpreter,
     groupscancommandparser, math, AvgLvlTree, commonTypeDefs, parsers;

{$endif}

type TScanType=(fs_advanced,fs_addresslist);


type
  TFoundList=class;
  TRebaseAgain=class(tthread)
  private
    procedure rerebase;
  public
    foundlistClass: TFoundList;
    procedure execute; override;
  end;



  TFoundList=class
  private
    fmemscan: TMemscan;

    foundlist: TListView;

    //foundcountlabel: tlabel;

    addressfile: tfilestream;
    scantype: TScanType;
    fvartype: TVariableType;
    fcustomType: TCustomType;
    varlength: integer; //bitlength, stringlength, customscan
    hexadecimal: boolean; //show result in hexadecimal notation (when possible)
    signed: boolean;
    unicode: boolean;
    codepage: boolean;
    binaryasdecimal: boolean;

    lastrebase: integer;
    addresslist: array [0..1023] of ptruint; //this is a small list of addresses in the list
    addresslistb: array [0..1023] of TBitAddress; //idem, but in case of bit
    addresslistg: PByteArray;
    addresslistfirst: qword; //index number the addresslist[0] has

    valuelist: array [0..1023] of string;
    RebaseAgainThread: TRebaseAgain;
    createdFoundlist: boolean;
    fisUnknownInitialValue: boolean;

    fListName: string;

    fCount: UInt64;

    gcp: Tgroupscancommandparser;
    groupElementSize: integer;


    lookupTree: TAvgLvlTree;

  public
    function getGCP: TGroupscanCommandParser;
    function GetVarLength: integer;
    procedure deleteaddress(i:integer);
    procedure clear;
    procedure RefetchValueList;
    function Initialize: int64; overload;
    function Initialize(vartype: TVariableType; customtype: TCustomType=nil):int64; overload;
    function Initialize(vartype: TVariableType; varlength: integer; hexadecimal,signed,binaryasdecimal,unicode: boolean; customtype: TCustomType=nil):int64; overload;  //initialize after a scan
    function Reinitialize: int64; //initializes it with the previous parameter
    procedure Deinitialize; //free filehandles before the scan
    function GetStartBit(i: integer):dword;
    function GetGroupAddress(i: qword): PGroupAddress;
    function GetAddressOnly(i: qword; var extra: dword; groupdata: PPGroupAddress=nil): ptruint;
    function GetAddress(i: qword;var extra: dword; var value:string): ptruint; overload; //extra for stuff like bitnr
    function GetAddress(i: qword):ptruint; overload;
    function FindClosestAddress(address: ptruint): qword;
    function InModule(i: integer):boolean;
    function GetModuleNamePlusOffset(i: integer):string;
    procedure RebaseAddresslist(i: integer);
    procedure RebaseAddresslistAgain; //calls rebaseaddresslist with the same parameter as last time
    procedure setListName(listname: string);
    constructor create(foundlist: tlistview; memscan: TMemScan; listname: string='');
    destructor destroy; override;
  published
    property memscan: TMemScan read fmemscan;
    property vartype: TVariableType read fvartype;
    property CustomType: TCustomType read fcustomType;
    property isHexadecimal: boolean read hexadecimal;
    property isSigned: boolean read signed;
    property isUnicode: boolean read unicode;
    property isCodePage: boolean read codepage;
    property isUnknownInitialValue: boolean read fisUnknownInitialValue;
    property count: uint64 read fCount;
    property listName: string read fListname write setListName;
end;

type Tscandisplayroutine=procedure(value: pointer; output: pchar);


implementation

{$ifndef jni}
uses CEFuncProc, LCLIntf, mainunit, processhandlerunit, symbolhandler;
{$endif}


resourcestring
  rsUndefinedError = 'Undefined error';
  rsError = 'Error';

type
  TLookupRecord=class(TObject) //lookup record for the tree
                  index: integer;
                  address: ptruint;
                  constructor create(index: integer;  address: ptruint);
                end;

  PLookuprecord=^TlookupRecord;


constructor TLookupRecord.create(index: integer; address: ptruint);
begin
  self.index:=index;
  self.address:=address;
end;

procedure TRebaseAgain.rerebase;
begin
  foundlistClass.RebaseAgainThread:=nil; //so it will spawn a new one if still not done
  foundlistClass.RebaseAddresslistAgain;
  {$ifdef windows}
  foundlistClass.foundlist.items[-1];
  foundlistClass.foundlist.Refresh;
  foundlistClass.foundlist.Refresh; (* lazarus bug bypass *)
  {$endif}
end;

procedure TRebaseAgain.execute;
begin

  freeonterminate:=true;
  sleep(1000);
  synchronize(rerebase);


end;

procedure TFoundList.clear;
begin
  //log('TFoundList.clear');
  if self.foundlist<>nil then
  begin
   // log('self.foundlist<>nil');

    self.foundlist.items.count:=0;
    self.foundlist.clear;
  end;
end;

function TFoundList.InModule(i: integer):boolean;
var mi: tmoduleinfo;
begin
  result:=symhandler.getmodulebyaddress(getaddress(i),mi);
end;

function TFoundList.GetModuleNamePlusOffset(i: integer):string;
var
  mi: tmoduleinfo;
  x: ptrUint;
begin
  x:=getaddress(i);

  if symhandler.getmodulebyaddress(x,mi) then
  begin
    //if the modulename has a special character, place the modulename between quotes
    if (pos(' ',mi.modulename)>0) or (pos('+',mi.modulename)>0) or (pos('-',mi.modulename)>0) or (pos('[',mi.modulename)>0) or (pos(']',mi.modulename)>0) then
      mi.modulename:='"'+mi.modulename+'"';

    result:=mi.modulename+'+'+inttohex(x-mi.baseaddress,1)
  end
  else
    result:=inttohex(x,8);
end;


function TFoundList.GetVarLength:integer;
begin
  result:=varlength;
end;

function TFoundList.getGCP: TGroupscanCommandParser;
begin
  result:=gcp;
end;

procedure TFoundList.deleteaddress(i:integer);
var memoryfile: tfilestream;
    outaddress: tfilestream;
    outmemory: tfilestream;

    addresspos: int64;
    memorypos: int64;

    j,k: integer;
    buf: pointer;

    entrysize: integer=0;

begin
  if addressfile=nil then exit;

  try
    memoryfile:=tfilestream.Create(fmemscan.ScanresultFolder+'MEMORY.'+fListName,fmOpenRead or fmShareDenyNone);
    outaddress:=tfilestream.Create(fmemscan.ScanresultFolder+'ADDRESSES.NEW',fmCreate or fmShareDenyNone);
    outmemory:=tfilestream.Create(fmemscan.ScanresultFolder+'MEMORY.NEW',fmCreate or fmShareDenyNone);
  except
    exit;
  end;

  getmem(buf,512*1024);
  try
    //memoryfile is initialized


    if vartype in [vtBinary,vtAll] then
      addresspos:=7+sizeof(sizeof(TBitAddress))*i
    else
    if vartype =vtGrouped then
      addresspos:=7+sizeof(dword)+groupElementSize
    else
      addresspos:=7+sizeof(sizeof(ptruint))*i;

    case vartype of
      vtByte: entrysize:=sizeof(byte);
      vtWord: entrysize:=sizeof(word);
      vtDword: entrysize:=sizeof(dword);
      vtSingle: entrysize:=sizeof(single);
      vtDouble: entrysize:=sizeof(double);
      vtQword: entrysize:=sizeof(int64);
    end; //no binary , string,aob or group since they have no values stored


    memorypos:=entrysize*i;

    addressfile.Position:=0;
    memoryfile.Position:=0;

    //address
    //copy all address from 0 to addresspos
    outaddress.CopyFrom(addressfile,addresspos);
    if vartype in [vtBinary,vtAll] then
      addressfile.Position:=addresspos+sizeof(TBitAddress)
    else
    if vartype=vtGrouped then
      addressfile.position:=addresspos+groupElementSize
    else
      addressfile.Position:=addresspos+sizeof(ptruint);

    if addressfile.Size-addressfile.Position>0 then //if there are stil addresses left, copy them as well
      outaddress.CopyFrom(addressfile,addressfile.Size-addressfile.Position);

    //memory
    if not (vartype in [vtBinary,vtAll, vtGrouped]) then
    begin
      //copy the memory from 0 to memorypos
      if memorypos>0 then
        outmemory.CopyFrom(memoryfile,memorypos);

      memoryfile.Position:=memorypos+entrysize;

      if memoryfile.Size-memoryfile.Position>0 then //and copy what's left
        outmemory.CopyFrom(memoryfile,memoryfile.Size-memoryfile.Position);
    end;
    

  finally
    freeandnil(memoryfile);
    freeandnil(outaddress);
    freeandnil(outmemory);
    freememandnil(buf);

  end;

  //still here, not crashed, so out with the old, in with the new...
  deinitialize;

  deletefile(fmemscan.ScanresultFolder+'MEMORY.'+fListName);
  deletefile(fmemscan.ScanresultFolder+'ADDRESSES.'+fListName);
  renamefile(fmemscan.ScanresultFolder+'MEMORY.NEW',memscan.ScanresultFolder+'MEMORY.'+fListName);
  renamefile(fmemscan.ScanresultFolder+'ADDRESSES.NEW',memscan.ScanresultFolder+'ADDRESSES.'+fListName);

  Reinitialize;
end;

procedure TFoundList.RebaseAddresslistAgain;
begin
  RebaseAddresslist(lastrebase);

end;

procedure TFoundList.RebaseAddresslist(i: integer);
var j,k: dword;
begin


  if addressfile=nil then exit; //during a scan

  lastrebase:=i;
  
  //reload buffer from index i-512; //so 512 above and below (result has to be bigger than or equal to 0)
  if i>512 then
    j:=i-512
  else
    j:=0;

  //fill addresslist
  k:=foundlist.Items.Count-j;
  if k>1024 then k:=1024;

  if vartype in [vtBinary,vtAll] then
  begin
    //binary/all
    addressfile.Position:=7+j*sizeof(TBitAddress);

    k:=sizeof(TBitAddress)*k;
    addressfile.ReadBuffer(addresslistb[0],k);
    addresslistfirst:=j;
  end
  else
  if vartype=vtGrouped then
  begin                 //7+elementcount+ X*groupelement
    addressfile.Position:=7+sizeof(dword)+j*groupElementSize;
    k:=groupElementSize*k;
    addressfile.ReadBuffer(addresslistG[0],k);
    addresslistfirst:=j;
  end
  else
  begin
    //normal
    addressfile.Position:=7+j*sizeof(ptruint);

    k:=sizeof(ptruint)*k;
    addressfile.ReadBuffer(addresslist[0],k);

    if (k>8) and (addresslist[0]=addresslist[1]) then
    begin
      //create a thread that calls rebase after a second
      if RebaseAgainThread=nil then
      begin
        RebaseAgainThread:=TRebaseAgain.Create(true);
        RebaseAgainThread.foundlistClass:=self;
        RebaseAgainThread.start;
      end;
    end;
    
    addresslistfirst:=j;
  end;

  for i:=0 to 1023 do
    if valuelist[i]<>'' then
      valuelist[i]:='';
end;

procedure TFoundList.RefetchValueList;
var i,j: integer;
    oldvalues: array of string;
    si,l: integer;
    x: dword;
    temp: string;
begin
  setlength(oldvalues,0);

  if addressfile=nil then exit;

  si:=-1;
  l:=foundlist.VisibleRowCount;

  if foundlist.TopItem<>nil then
  begin
    si:=foundlist.TopItem.index;
    if si>=0 then
    begin
      setlength(oldvalues,l);
      j:=0;
      for i:=si to si+l-1 do
      begin
        GetAddress(i,x,oldvalues[j]);
        inc(j);
      end;
    end;
  end;




  for i:=0 to 1023 do
    if valuelist[i]<>'' then
      valuelist[i]:='';

  if si>=0 then
  begin
    //update the changed ones
    j:=0;
    for i:=si to si+l-1 do
    begin
      getaddress(i,x,temp);
      if temp<>oldvalues[j] then
      begin
        foundlist.items[-1];
        foundlist.Refresh;
        foundlist.Refresh; (* lazarus bug bypass *)
        exit;
      end;
      inc(j);
    end;
  end;
end;

function TFoundList.GetStartBit(i: integer):dword;
var extra: dword;
begin
  GetAddressOnly(i,extra);
  result:=extra;
end;

function TFoundList.GetGroupAddress(i: qword): PGroupAddress;
var extra: dword;
    ga: PGroupAddress;
begin
  ga:=nil;
  GetAddressOnly(i, extra, @ga);
  result:=ga;
end;

function AddressLookupCompare(Item1, Item2: Pointer): Integer;
begin
  result:=comparevalue(TLookupRecord(Item1).address, TLookupRecord(Item2).address);
end;

function TFoundList.FindClosestAddress(address: ptruint): qword;
//searched the addresslist for the given address and returns the index for it
//If this is the first time being called create an index of max 100 entries and binary tree to search the index
var last: integer;
    step: integer;
    i: qword;
    n: TAvgLvlTreeNode;
    lr: TLookupRecord;
begin
  result:=qword(-1);
  if lookupTree=nil then
  begin
    lookuptree:=TAvgLvlTree.Create(AddressLookupCompare);

    //first time init
    //split 0 to fCount up into 100 TLookupRecord objects (if possible, else less)
    if fcount>100 then
      step:=fcount div 100
    else
      step:=1;

    i:=0;
    while i<fCount do
    begin
      lookuptree.Add(TLookupRecord.create(i, GetAddress(i)));
      inc(i, step);
    end;
  end;

  lr:=TLookupRecord.create(-1, address);
  n:=lookuptree.FindNearest(lr);
  lr.free;

  if n<>nil then
  begin
    i:=TLookupRecord(n.data).index;
    //find the best spot

    //search till the address is too big
    while (i<fcount-1) and (GetAddress(i)<address) do
      inc(i);

    //i is now at the end, or the address is too big or just correct

    result:=i; //return the exact index, or higher side of the wanted address
  end;
end;

function TFoundList.GetAddressOnly(i: qword; var extra: dword; groupdata: PPGroupAddress=nil): ptruint;
var j: qword;
begin
  extra:=0;
  result:=0;

  if i=qword(-1) then exit;
  if i>=foundlist.Items.Count then exit;

  if addressfile=nil then exit; //during a scan
  if scantype=fs_advanced then exit; //should never happen...

  if not InRangeQ(i, addresslistfirst, addresslistfirst+1023) then
    RebaseAddresslist(i);

  j:=i-addresslistfirst;


  if vartype in [vtAll,vtBinary] then  //bit,all
  begin
    result:=addresslistb[j].address;
    extra:=addresslistb[j].bit;
  end
  else
  if vartype=vtGrouped then
  begin
    result:=PGroupAddress(ptruint(addresslistg)+j*groupElementSize)^.address;
    if groupdata<>nil then
      groupdata^:=PGroupAddress(ptruint(addresslistg)+j*groupElementSize);
  end
  else
  begin
    result:=addresslist[j];

  end;

  if result=0 then //address 0 is usually not possible
  begin
    //so means it wasn't loaded yet, klet's rebase it again (reload resultfile)
    if RebaseAgainThread=nil then
    begin
      RebaseAgainThread:=TRebaseAgain.Create(true);
      RebaseAgainThread.foundlistClass:=self;
      RebaseAgainThread.start;
    end;
  end;


end;


function TFoundList.GetAddress(i: qword):ptruint;
var a: dword;
    b: string;
begin
  result:=getaddress(i,a,b);
end;

function TFoundList.GetAddress(i: qword;var extra: dword; var value: string): ptruint;
var j,k,l: integer;
    currentaddress: ptrUint;
    read1: byte;
    read2: word;
    read3: dword;
    read4: single;
    read5: double;
    read6: uInt64;
    read7: pchar;
    read72: pwidechar;
    read8: array of byte;
    read9: pbyte;
    count: ptrUint;
    nrofbytes: integer;
    temp,temp2: string;
    tempbuf: pointer;
    resultstring: pchar;
    vtype: TVariableType;

    groupdata: PGroupAddress;
begin
  extra:=0;
  value:='';
  result:=0;
  groupdata:=nil;

  if i=qword(-1) then exit;






  currentaddress:=GetAddressOnly(i,extra, @groupdata);

  result:=currentaddress;
  j:=i-addresslistfirst;
  if j<0 then exit(0);


  if valuelist[j]='' then
  begin
    if vartype=vtAll then
    begin
      //override vtype with the type it scanned
      {$ifndef jni}
      if extra >=$1000 then
      begin
        fcustomtype:=tcustomtype(customTypes[extra-$1000]);
        vtype:=vtCustom;
      end
      else
      {$endif}
        vtype:=TVariableType(extra);

    end else vtype:=vartype;



    case vtype of
      vtBinary:
      begin //binary
        //read the bytes

        nrofbytes:=1+((addresslistb[j].bit+varlength) div 8);
        setlength(read8,nrofbytes);

        if readprocessmemory(processhandle,pointer(addresslistb[j].address),@read8[0],nrofbytes,count) then
        begin
          //convert what i need to a string of bits
          temp:='';
          l:=addresslistb[j].bit;
          read9:=@read8[0];
          for k:=1 to varlength do
          begin
            temp:=temp+IntToStr(getbit(l,read9^));
            inc(l);
            if l>=8 then
            begin
              l:=0;
              inc(read9);
            end;
          end;

          temp2:='';
          for k:=length(temp) downto 1 do
            temp2:=temp2+temp[k];

          if binaryasdecimal then
          begin
            try
              valuelist[j]:=IntToStr(bintoint(temp2));
            except
              valuelist[j]:='...';
            end;
          end else valuelist[j]:=temp2;
        end
        else
        valuelist[j]:='??';
      end;


      vtString:
      begin  //text
        if unicode then
        begin
          vtype:=vtUnicodeString;
          nrofbytes:=varlength*2;
        end
        else
        begin
          if fmemscan<>nil then
            if fmemscan.codePage then
              vtype:=vtCodePageString;

          nrofbytes:=varlength;
        end;

      end;

      vtByteArray: nrofbytes:=varlength;

      vtGrouped: //assumption is made that people have upgraded to faster cpu's by now since the ALL type was last added
      begin
        //group check the offsets and parse accordingly (with help of the previously saved groupscan command)

        if groupdata<>nil then
        begin
          valuelist[j]:='';
          for k:=0 to length(gcp.elements)-1 do
          begin
            valuelist[j]:=valuelist[j]+gcp.elements[k].command+'['+inttohex(groupdata^.offsets[k],1)+']:';

            if not gcp.elements[k].wildcard then
              valuelist[j]:=valuelist[j]+readAndParseAddress(currentaddress+groupdata^.offsets[k], gcp.elements[k].vartype, gcp.elements[k].customtype, false, false, gcp.elements[k].bytesize)
            else
              valuelist[j]:=valuelist[j]+'*';


            if k<>length(gcp.elements)-1 then
              valuelist[j]:=valuelist[j]+' ';

          end;
        end;

      end;

    end;

    if not (vtype in [vtBinary,vtGrouped]) then
      valuelist[j]:=readAndParseAddress(currentaddress, vtype, fcustomtype, hexadecimal, signed, nrofbytes);

  end;
  value:=valuelist[j];
end;


function TFoundList.Initialize(vartype: TVariableType; customtype: TCustomType=nil):int64;
var dataType:  String[6];  //REGION or NORMAL  (Always region in this procedure)
    i: uint64;
begin
  result:=0;
  //log('TFoundList.Initialize');
  Deinitialize;

  foundlist.itemindex:=-1;
  foundlist.items.count:=0;


  fvartype:=vartype;
  fcustomType:=customtype;


 // log('Checking if '+fmemscan.ScanresultFolder+'ADDRESSES.'+fListName+' exists');

  if fileexists(fmemscan.ScanresultFolder+'ADDRESSES.'+fListName) then
  begin
   // log('it exists');


    try
      self.addressfile:=tfilestream.Create(fmemscan.ScanresultFolder+'ADDRESSES.'+fListName,fmOpenRead or fmShareDenyNone);
    except
      foundlist.Items.Count:=0;
      scantype:=fs_advanced;
      exit;
    end;


    try
      addressfile.ReadBuffer(dataType,7);

      if datatype='REGION' then
      begin
        foundlist.Items.Count:=0;
        scantype:=fs_advanced;
        fisUnknownInitialValue:=true;
      end
      else
      begin
        fisUnknownInitialValue:=false;
        scantype:=fs_addresslist;

        if vartype in [vtBinary,vtAll] then //bit, or all (address+bit)
        begin
          result:=(addressfile.Size-sizeof(datatype)) div sizeof(TBitAddress);



        end
        else
        if vartype=vtGrouped then
        begin //group
          gcp:=TGroupscanCommandParser.Create(fmemscan.LastScanValue);

          groupElementSize:=sizeof(ptruint)+sizeof(dword)*length(gcp.elements);

          if addresslistg<>nil then
            freememandnil(addresslistg);

          addresslistg:=getmem(1024*groupElementSize);

          result:=(addressfile.Size-sizeof(datatype)) div groupElementSize;
        end
        else //normal (address)
        begin
          result:=(addressfile.Size-sizeof(datatype)) div sizeof(ptruint);


        end;

        i:=min(result,10000000);
        foundlist.Items.Count:=i;
        while foundlist.Items.Count=0 do
        begin
          i:=i div 10;
          foundlist.Items.Count:=i;

          if i=0 then break;
        end;

        rebaseaddresslist(0);
      end;
    except
      foundlist.Items.Count:=0;
      scantype:=fs_advanced;
    end;
  end
  else
  begin
    //messagebox(0,'no','',0);
    //exit;
    //log('it does not exist');

    foundlist.Items.Count:=0;
    scantype:=fs_advanced;
  end;

  if (not createdfoundlist) and (result>0) and (foundlist.Items.Count>0) then
    foundlist.Items[0].MakeVisible(false);


  fCount:=result;

  if fmemscan<>nil then
  begin
    //log('using fmemscan');
    //guess the default display types
    //can later be overriden
    self.hexadecimal:=memscan.isHexadecimal;   //memscan.VarType=vtByteArray;
    self.varlength:=memscan.Getbinarysize div 8;

    self.unicode:=memscan.isUnicode;
    self.codepage:=memscan.codePage;
  end
  else
    OutputDebugString('no fmemscan');

end;


function TFoundList.Reinitialize: int64; //initializes it with the previous parameters
begin
  Deinitialize;
  result:=initialize(fvartype, varlength, hexadecimal, signed, binaryasdecimal, unicode, fcustomType);
end;

function TFoundList.Initialize(vartype: TVariableType; varlength: integer; hexadecimal,signed,binaryasdecimal,unicode: boolean; customtype: TCustomType=nil):int64;
begin
  result:=Initialize(vartype, customtype);

  if scantype=fs_addresslist then
  begin
    self.fvartype:=vartype;
    self.hexadecimal:=hexadecimal;
    self.signed:=signed;
    self.varlength:=varlength;
    self.binaryasdecimal:=binaryasdecimal;
    self.unicode:=unicode;
  end;
end;

function TFoundList.Initialize: int64;
begin
  result:=Initialize(fmemscan.vartype,fmemscan.CustomType);
end;

procedure TFoundlist.Deinitialize;
begin
  //log('TFoundList.Deinitialize');
  fcount:=0;

  //log('0');
  clear;

  //log('1');

  if addressfile<>nil then
    freeandnil(addressfile);

 // log('2');

  if lookupTree<>nil then
    lookupTree.FreeAndClear;

  //log('Return from TFoundList.Deinitialize');
end;

procedure  TFoundlist.setListName(listname: string);
begin
  flistname:=listname;
  Deinitialize;
  Initialize;
end;


destructor TFoundlist.destroy;
begin
  if RebaseAgainThread<>nil then
    RebaseAgainThread.WaitFor;

  if createdfoundlist then
    foundlist.free;

  if addresslistg<>nil then
    freememandnil(addresslistg);
end;

constructor TFoundlist.create(foundlist: tlistview; memscan: TMemScan; listname: string='');
begin
  if foundlist=nil then
  begin
    createdFoundlist:=true;
    self.foundlist:=Tlistview.create(nil);
    self.foundlist.OwnerData:=true;
  end
  else
    self.foundlist:=foundlist;

  //self.foundcountlabel:=foundcountlabel;
  fmemscan:=memscan;
  fmemscan.attachedFoundlist:=self;

  flistname:=listname;
  if flistname='' then
    flistname:='TMP';
end;

end.
