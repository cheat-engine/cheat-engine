unit foundlisthelper;

interface

uses windows,sysutils,classes,ComCtrls,StdCtrls,symbolhandler, cefuncproc,
     newkernelhandler, memscan;

type TScanType=(fs_advanced,fs_addresslist);


type
  TFoundList=class;
  TRebaseAgain=class(tthread)
  private
    procedure rerebase;
  public
    foundlist: TFoundList;
    procedure execute; override;
  end;

  TFoundList=class
  private
    foundlist: TListView;
    foundcountlabel: tlabel;

    addressfile: tfilestream;
    scantype: TScanType;
    fvartype: integer;
		varlength: integer; //bitlength, stringlength
		hexadecimal: boolean; //show result in hexadecimal notation (when possible)
    signed: boolean;
		unicode: boolean;
		binaryasdecimal: boolean;

    lastrebase: integer;
    addresslist: array [0..1023] of dword; //this is a small list of addresses in the list
    addresslistb: array [0..1023] of TBitAddress; //idem, but in case of bit
    addresslistfirst: dword; //index number the addresslist[0] has

		valuelist: array [0..1023] of string;
    RebaseAgainThread: TRebaseAgain;
  public
    function GetVarLength: integer;
    procedure DeleteResults;
		procedure deleteaddress(i:integer);
    procedure clear;
    procedure RefetchValueList;
    function Initialize(vartype: integer):int64; overload;
    function Initialize(vartype,varlength: integer; hexadecimal,signed,binaryasdecimal,unicode: boolean):int64; overload;  //initialize after a scan
    procedure Deinitialize; //free filehandles before the scan
    function GetStartBit(i: integer):dword;
    function GetAddressOnly(i: integer; var extra: dword): dword;
    function GetAddress(i: integer;var extra: dword; var value:string): dword; overload; //extra for stuff like bitnr
    function GetAddress(i: integer):dword; overload;
    function InModule(i: integer):boolean;
    function GetModuleNamePlusOffset(i: integer):string;
    procedure RebaseAddresslist(i: integer);
    procedure RebaseAddresslistAgain; //calls rebaseaddresslist with the same parameter as last time
    property vartype: integer read fvartype;
    constructor create(foundlist: tlistview; foundcountlabel: tlabel);
end;

implementation

procedure TRebaseAgain.rerebase;
begin
  foundlist.RebaseAgainThread:=nil; //so it will spawn a new one if still not done
  foundlist.RebaseAddresslistAgain;
  foundlist.foundlist.Refresh;
end;

procedure TRebaseAgain.execute;
begin
  freeonterminate:=true;
  sleep(1000);
  synchronize(rerebase);
end;

procedure TFoundList.clear;
begin
  foundlist.Items.Count:=0;
  addresslistfirst:=0;
  foundlist.Clear;
end;

function TFoundList.InModule(i: integer):boolean;
var mi: tmoduleinfo;
begin
  result:=symhandler.getmodulebyaddress(getaddress(i),mi);
end;

function TFoundList.GetModuleNamePlusOffset(i: integer):string;
var mi: tmoduleinfo;
    x: dword;
begin
  x:=getaddress(i);
  if symhandler.getmodulebyaddress(x,mi) then
    result:=mi.modulename+'+'+inttohex(x-mi.baseaddress,1)
  else
    result:=inttohex(x,8);
end;


function TFoundList.GetVarLength:integer;
begin
  result:=varlength;
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
    memoryfile:=tfilestream.Create(CheatEngineDir+'Memory.TMP',fmOpenRead	or fmShareDenyNone);
		outaddress:=tfilestream.Create(CheatEngineDir+'Addresses.NEW',fmCreate or fmShareDenyNone);
		outmemory:=tfilestream.Create(CheatEngineDir+'Memory.NEW',fmCreate or fmShareDenyNone);
  except
    exit;
  end;

  getmem(buf,512*1024);
	try
		//memoryfile is initialized

    if vartype in [5,9] then
			addresspos:=7+sizeof(sizeof(TBitAddress))*i
		else
			addresspos:=7+sizeof(sizeof(dword))*i;

		case vartype of
			0: memorypos:=sizeof(byte)*i;
			1: memorypos:=sizeof(word)*i;
			2: memorypos:=sizeof(dword)*i;
			3: memorypos:=sizeof(single)*i;
			4: memorypos:=sizeof(double)*i;
			6: memorypos:=sizeof(int64)*i;		
		end; //no 5 and 7 since they have no values

    addressfile.Position:=0;
    memoryfile.Position:=0;

    outaddress.CopyFrom(addressfile,addresspos);
    if vartype in [5,9] then
			addressfile.Position:=addresspos+sizeof(TBitAddress)
		else
			addressfile.Position:=addresspos+sizeof(dword);

    if addressfile.Size-addressfile.Position>0 then
      outaddress.CopyFrom(addressfile,addressfile.Size-addressfile.Position);

    //memory
    if not (vartype in [5,7]) then
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

  deletefile(CheatEngineDir+'Memory.TMP');
  deletefile(CheatEnginedir+'Addresses.TMP');
  renamefile(CheatEngineDir+'Memory.NEW',CheatEngineDir+'Memory.TMP');
  renamefile(CheatEngineDir+'Addresses.NEW',CheatEngineDir+'Addresses.TMP');

  Initialize(vartype);
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

  if vartype in [5,9] then
  begin
    addressfile.Position:=7+j*sizeof(TBitAddress);

    k:=sizeof(TBitAddress)*k;
    addressfile.ReadBuffer(addresslistb[0],k);
    addresslistfirst:=j;
  end
  else
  begin
    addressfile.Position:=7+j*sizeof(dword);

    k:=sizeof(dword)*k;
    addressfile.ReadBuffer(addresslist[0],k);

    if (k>8) and (addresslist[0]=addresslist[1]) then
    begin
      //create a thread that calls rebase after a second
      if RebaseAgainThread=nil then
      begin
        RebaseAgainThread:=TRebaseAgain.Create(true);
        RebaseAgainThread.foundlist:=self;
        RebaseAgainThread.Resume;
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
      if temp<>oldvalues[j] then foundlist.Items[i].Update;
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

function TFoundList.GetAddressOnly(i: integer; var extra: dword): dword;
var j: integer;
begin
  if i=-1 then exit;

  extra:=0;
  result:=0;
  if addressfile=nil then exit; //during a scan
  if scantype=fs_advanced then exit; //should never happen...

  if (i<addresslistfirst) or (i>=addresslistfirst+1024) then
    RebaseAddresslist(i);

  j:=i-addresslistfirst;

  if vartype in [5,9] then  //bit,all
  begin
    result:=addresslistb[j].address;
    extra:=addresslistb[j].bit;
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
      RebaseAgainThread.foundlist:=self;
      RebaseAgainThread.Resume;
    end;
  end;


end;


function TFoundList.GetAddress(i: integer):dword;
var a: dword;
    b: string;
begin
  result:=getaddress(i,a,b);
end;

function TFoundList.GetAddress(i: integer;var extra: dword; var value: string): dword;
var j,k,l: integer;
    currentaddress: dword;
    read1: byte;
    read2: word;
    read3: dword;
    read4: single;
    read5: double;
    read6: Int64;
    read7: pchar;
    read72: pwidechar;
    read8: array of byte;
    read9: pbyte;
		count: dword; 
		nrofbytes: integer;  
		temp,temp2: string;
    vtype: integer;
begin
  if i=-1 then exit;

  extra:=0;
  value:='';
  result:=0;

  currentaddress:=GetAddressOnly(i,extra);

  result:=currentaddress;
  j:=i-addresslistfirst;

  if valuelist[j]='' then
	begin
    if vartype=9 then
    begin
      //override vtype with the type it scanned
      case TVariableType(extra) of
        vtByte:   vtype:=0;
        vtWord:   vtype:=1;
        vtDword:  vtype:=2;
        vtQword:  vtype:=6;
        vtSingle: vtype:=3;
        vtDouble: vtype:=4;
      end;
    end else vtype:=vartype;

	  case vtype of
			0: //byte
			begin
        if readprocessmemory(processhandle,pointer(currentaddress),@read1,1,count) then
				begin
          if hexadecimal then
					  valuelist[j]:=IntToHex(read1,2)
					else if signed then
						valuelist[j]:=IntToStr(ShortInt(read1))
					else
          	valuelist[j]:=IntToStr(read1);
				end
				else valuelist[j]:='??';
			end;

			1: //word
			begin
        if readprocessmemory(processhandle,pointer(currentaddress),@read2,2,count) then
				begin
          if hexadecimal then
					  valuelist[j]:=IntToHex(read2,4)
					else if signed then
						valuelist[j]:=IntToStr(SmallInt(read2))
					else
          	valuelist[j]:=IntToStr(read2);
				end
				else valuelist[j]:='??';
			end;

			2: //dword
			begin
        if readprocessmemory(processhandle,pointer(currentaddress),@read3,4,count) then
				begin
          if hexadecimal then
					  valuelist[j]:=IntToHex(read3,8)
					else if signed then
						valuelist[j]:=IntToStr(Longint(read3))
					else
          	valuelist[j]:=IntToStr(read3);
				end
				else valuelist[j]:='??';
			end;

  	  3:
      begin //float
        if readprocessmemory(processhandle,pointer(currentaddress),@read4,4,count) then
					valuelist[j]:=FloatToStr(read4)
				else 
					valuelist[j]:='??';
      end;

  	  4:
      begin //double
        if readprocessmemory(processhandle,pointer(currentaddress),@read5,8,count) then
					valuelist[j]:=FloatToStr(read5)
				else 
					valuelist[j]:='??';
      end;

    	5:
      begin //binary
        //read the bytes

        nrofbytes:=1+((addresslistb[j].bit+varlength) div 8);
        setlength(read8,nrofbytes);

        if readprocessmemory(processhandle,pointer(addresslistb[j].address),@read8[0],nrofbytes,count) then
        begin
          //convert what i need to a string of bits
          temp:='';
          j:=addresslistb[j].bit;
          read9:=@read8[0];
          for k:=1 to varlength do
          begin
            temp:=temp+IntToStr(getbit(j,read9^));
            inc(j);
            if j>=8 then
            begin
              j:=0;
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

  	  6:
      begin //int64
        if readprocessmemory(processhandle,pointer(currentaddress),@read6,8,count) then
        begin
          if hexadecimal then
						valuelist[j]:=inttohex(read6,16)
					else 
						valuelist[j]:=inttostr(read6)
				end
				else 
					valuelist[j]:='??';
      end;


      7:
      begin  //text
        if unicode then
        begin
          getmem(read72,varlength*2+2);
          if readprocessmemory(processhandle,pointer(addresslist[j]),read72,varlength*2,count) then          
          begin
            read72[varlength]:=chr(0);
            valuelist[j]:=read72;
          end
					else valuelist[j]:='??';
          freemem(read72);
        end
        else
        begin
          getmem(read7,varlength+1);
          if readprocessmemory(processhandle,pointer(addresslist[j]),read7,varlength,count) then
          begin
            read7[varlength]:=chr(0);
            valuelist[j]:=read7;
          end
					else valuelist[j]:='??';
          freemem(read7);
        end;
      end;

    	8:
      begin //array of byte
        setlength(read8,varlength);

        if readprocessmemory(processhandle,pointer(addresslist[j]),read8,varlength,count) then
        begin
          temp:='';
          for j:=0 to varlength-1 do
            temp:=temp+IntToHex(read8[j],2)+' ';

          valuelist[j]:=temp;
        end else valuelist[j]:='??';

        setlength(read8,0);
      end;


    end;

  end;
  value:=valuelist[j];
end;

function TFoundList.Initialize(vartype: integer):int64;
var dataType:  String[6];  //REGION or NORMAL  (Always region in this procedure)
begin
  result:=0;
  Deinitialize;
  fvartype:=vartype;

  if fileexists(CheatEngineDir+'Addresses.TMP') then
  begin
    try
      addressfile:=tfilestream.Create(CheatEngineDir+'Addresses.TMP',fmOpenRead	or fmShareDenyNone);
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
      end
      else
      begin
        scantype:=fs_addresslist;

        if vartype in [5,9] then //bit, or all (address+bit)
        begin
          result:=(addressfile.Size-sizeof(datatype)) div 8;
          foundlist.Items.Count:=result;
        end
        else //normal (address)
        begin
          result:=(addressfile.Size-sizeof(datatype)) div 4;
          foundlist.Items.Count:=result;
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
    foundlist.Items.Count:=0;
    scantype:=fs_advanced;
  end;
end;


function TFoundList.Initialize(vartype,varlength: integer; hexadecimal,signed,binaryasdecimal,unicode: boolean):int64;
begin
  result:=Initialize(vartype);

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

procedure TFoundlist.Deinitialize;
begin
  clear;
  if addressfile<>nil then
    freeandnil(addressfile);
end;

procedure TFoundlist.deleteresults;
begin
  Deinitialize;
  deletefile(pchar(CheatEngineDir+'Addresses.TMP'));
  deletefile(pchar(CheatEngineDir+'Memory.TMP'));
end;

constructor TFoundlist.create(foundlist: tlistview; foundcountlabel: tlabel);
begin
  self.foundlist:=foundlist;
  self.foundcountlabel:=foundcountlabel;
  deleteresults;
end;


end.
