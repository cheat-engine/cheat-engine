unit foundlisthelper;

{$MODE Delphi}

//todo: Change the list to the same kind of list used in the pointerscan (so no more concatenating the results)


interface

uses LCLIntf,sysutils,classes,ComCtrls,StdCtrls,symbolhandler, CEFuncProc,
     NewKernelHandler, memscan, CustomTypeHandler, byteinterpreter,
     groupscancommandparser, math;

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
    memscan: TMemscan;
    foundlist: TListView;
    //foundcountlabel: tlabel;

    addressfile: tfilestream;
    scantype: TScanType;
    fvartype: TVariableType;
    customType: TCustomType;
    varlength: integer; //bitlength, stringlength, customscan
    hexadecimal: boolean; //show result in hexadecimal notation (when possible)
    signed: boolean;
    unicode: boolean;
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

    fCount: UInt64;

    gcp: Tgroupscancommandparser;
    groupElementSize: integer;
  public
    function getGCP: TGroupscanCommandParser;
    function GetVarLength: integer;
    procedure deleteaddress(i:integer);
    procedure clear;
    procedure RefetchValueList;
    function Initialize: int64; overload;
    function Initialize(vartype: TVariableType; customtype: TCustomType):int64; overload;
    function Initialize(vartype: TVariableType; varlength: integer; hexadecimal,signed,binaryasdecimal,unicode: boolean; customtype: TCustomType):int64; overload;  //initialize after a scan
    procedure Deinitialize; //free filehandles before the scan
    function GetStartBit(i: integer):dword;
    function GetGroupAddress(i: qword): PGroupAddress;
    function GetAddressOnly(i: qword; var extra: dword; groupdata: PPGroupAddress=nil): ptruint;
    function GetAddress(i: qword;var extra: dword; var value:string): ptruint; overload; //extra for stuff like bitnr
    function GetAddress(i: qword):ptruint; overload;
    function InModule(i: integer):boolean;
    function GetModuleNamePlusOffset(i: integer):string;
    procedure RebaseAddresslist(i: integer);
    procedure RebaseAddresslistAgain; //calls rebaseaddresslist with the same parameter as last time
    property vartype: TVariableType read fvartype;
    property isHexadecimal: boolean read hexadecimal;
    property isUnicode: boolean read unicode;
    property isUnknownInitialValue: boolean read fisUnknownInitialValue;
    property count: uint64 read fCount;
    constructor create(foundlist: tlistview; memscan: TMemScan);
    destructor destroy; override;
end;

type Tscandisplayroutine=procedure(value: pointer; output: pchar);


implementation

uses mainunit;

resourcestring
  rsUndefinedError = 'Undefined error';
  rsError = 'Error';

procedure TRebaseAgain.rerebase;
begin
  foundlistClass.RebaseAgainThread:=nil; //so it will spawn a new one if still not done
  foundlistClass.RebaseAddresslistAgain;
  foundlistClass.foundlist.items[-1];
  foundlistClass.foundlist.Refresh;
  foundlistClass.foundlist.Refresh; (* lazarus bug bypass *)
end;

procedure TRebaseAgain.execute;
begin

  freeonterminate:=true;
  sleep(1000);
  synchronize(rerebase);


end;

procedure TFoundList.clear;
begin
  if foundlist<>nil then
  begin
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
var mi: tmoduleinfo;
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

begin
  if addressfile=nil then exit;

  try
    memoryfile:=tfilestream.Create(memscan.ScanresultFolder+'Memory.TMP',fmOpenRead or fmShareDenyNone);
    outaddress:=tfilestream.Create(memscan.ScanresultFolder+'Addresses.NEW',fmCreate or fmShareDenyNone);
    outmemory:=tfilestream.Create(memscan.ScanresultFolder+'Memory.NEW',fmCreate or fmShareDenyNone);
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
      vtByte: memorypos:=sizeof(byte)*i;
      vtWord: memorypos:=sizeof(word)*i;
      vtDword: memorypos:=sizeof(dword)*i;
      vtSingle: memorypos:=sizeof(single)*i;
      vtDouble: memorypos:=sizeof(double)*i;
      vtQword: memorypos:=sizeof(int64)*i;
    end; //no binary , string,aob or group since they have no values stored

    addressfile.Position:=0;
    memoryfile.Position:=0;

    outaddress.CopyFrom(addressfile,addresspos);
    if vartype in [vtBinary,vtAll] then
      addressfile.Position:=addresspos+sizeof(TBitAddress)
    else
    if vartype=vtGrouped then
      addressfile.position:=addresspos+groupElementSize
    else
      addressfile.Position:=addresspos+sizeof(ptruint);

    if addressfile.Size-addressfile.Position>0 then
      outaddress.CopyFrom(addressfile,addressfile.Size-addressfile.Position);

    //memory
    if not (vartype in [vtBinary,vtAll, vtGrouped]) then
    begin
      outmemory.CopyFrom(memoryfile,memorypos);
      memoryfile.Position:=memorypos+sizeof(dword);

      if memoryfile.Size-memoryfile.Position>0 then
        outmemory.CopyFrom(memoryfile,memoryfile.Size-memoryfile.Position);
    end;
    

  finally
    memoryfile.free;
    outaddress.free;
    outmemory.free;
    freemem(buf);
  end;

  //still here, not crashed, so out with the old, in with the new...
  deinitialize;

  deletefile(memscan.ScanresultFolder+'Memory.TMP');
  deletefile(memscan.ScanresultFolder+'Addresses.TMP');
  renamefile(memscan.ScanresultFolder+'Memory.NEW',memscan.ScanresultFolder+'Memory.TMP');
  renamefile(memscan.ScanresultFolder+'Addresses.NEW',memscan.ScanresultFolder+'Addresses.TMP');

  Initialize(vartype,customtype);
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

  if addressfile=nil then exit;

  si:=-1;
  if foundlist.TopItem<>nil then
  begin
    si:=foundlist.TopItem.index;
    if si>=0 then
    begin

      l:=foundlist.VisibleRowCount;

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

function TFoundList.GetAddressOnly(i: qword; var extra: dword; groupdata: PPGroupAddress=nil): ptruint;
var j: qword;
begin
  extra:=0;
  result:=0;

  if i=-1 then exit;
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
    count: dword;
    nrofbytes: integer;
    temp,temp2: string;
    tempbuf: pointer;
    resultstring: pchar;
    vtype: TVariableType;

    groupdata: PGroupAddress;
begin
  if i=-1 then exit;



  extra:=0;
  value:='';
  result:=0;
  groupdata:=nil;

  currentaddress:=GetAddressOnly(i,extra, @groupdata);

  result:=currentaddress;
  j:=i-addresslistfirst;

  if valuelist[j]='' then
  begin
    if vartype=vtAll then
    begin
      //override vtype with the type it scanned
      if extra >=$1000 then
      begin
        customtype:=tcustomtype(customTypes[extra-$1000]);
        vtype:=vtCustom;
      end
      else
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
          nrofbytes:=varlength;
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
      valuelist[j]:=readAndParseAddress(currentaddress, vtype, customtype, hexadecimal, signed, nrofbytes);

  end;
  value:=valuelist[j];
end;


function TFoundList.Initialize(vartype: TVariableType; customtype: TCustomType=nil):int64;
var dataType:  String[6];  //REGION or NORMAL  (Always region in this procedure)
    i: uint64;
begin
  result:=0;
  Deinitialize;

  foundlist.itemindex:=-1;
  foundlist.items.count:=0;


  fvartype:=vartype;
  self.customType:=customtype;




  if fileexists(memscan.ScanresultFolder+'Addresses.TMP') then
  begin
    try
      self.addressfile:=tfilestream.Create(memscan.ScanresultFolder+'Addresses.TMP',fmOpenRead or fmShareDenyNone);
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
          gcp:=TGroupscanCommandParser.Create(memscan.LastScanValue);

          groupElementSize:=sizeof(ptruint)+sizeof(dword)*length(gcp.elements);

          if addresslistg<>nil then
            freemem(addresslistg);

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
    foundlist.Items.Count:=0;
    scantype:=fs_advanced;
  end;

  if (not createdfoundlist) and (result>0) and (foundlist.Items.Count>0) then
    foundlist.Items[0].MakeVisible(false);


  fCount:=result;

  if memscan<>nil then
  begin
    //guess the default display types
    //can later be overriden
    self.hexadecimal:=memscan.VarType=vtByteArray;
    self.varlength:=memscan.Getbinarysize div 8;
  end;
end;


function TFoundList.Initialize(vartype: TVariableType; varlength: integer; hexadecimal,signed,binaryasdecimal,unicode: boolean; customtype: TCustomType=nil):int64;
begin
  result:=Initialize(vartype,customtype);

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

  result:=Initialize(memscan.vartype,memscan.CustomType);
end;

procedure TFoundlist.Deinitialize;
begin
  clear;
  if addressfile<>nil then
    freeandnil(addressfile);
end;


destructor TFoundlist.destroy;
begin
  if RebaseAgainThread<>nil then
    RebaseAgainThread.WaitFor;

  if createdfoundlist then
    foundlist.free;

  if addresslistg<>nil then
    freemem(addresslistg);
end;

constructor TFoundlist.create(foundlist: tlistview; memscan: TMemScan);
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
  self.memscan:=memscan;
  memscan.attachedFoundlist:=self;
end;

end.
