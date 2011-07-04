unit DissectCodeThread;

{$MODE Delphi}

interface

uses
  windows, LCLIntf,sysutils,syncobjs,Classes,disassembler, NewKernelHandler, math, CEFuncProc;


type
  TAddresslist=record
    pos: integer;
    maxsize: integer;
    a: PPtrUintArray;
    isstring: boolean;
  end;
  PAddresslist=^TAddresslist;


  PDissectDataArray=^TDissectDataArray;
  TDissectData=record
    case integer of
      1: (addresslist: PAddresslist); //if this is the last level (7) this is an PAddresslist
      2: (DissectDataArray: PDissectDataArray);
  end;
  TDissectDataArray=array [0..15] of TDissectData;




type tjumptype=(jtUnconditional,jtConditional,jtCall);

type tdissectarray= array of record
                      address: ptrUint;
                      jumptype: tjumptype;
                    end;

type tjumpdata = record
                   address:ptrUint;
                   codes: integer;
                   code: array[0..7] of ptrUint; //default space for 8 addresses

                   nrofextracodes: integer;
                   extracodes: array of ptrUint; //but can be expanded to more if really needed
                 end;

type tjumparray=array of tjumpdata;


type TStringReference=class(tobject)
  address: ptrUint;
  s: string;
  references: array of ptrUint;
end;



type
  TDissectCodeThread = class(TThread)
  private
    { Private declarations }
    calllist: PDissectDataArray;
    unconditionaljumplist: PDissectDataArray;
    conditionaljumplist: PDissectDataArray;
    memorylist: PDissectDataArray;


    function isstring(address: ptrUint): boolean;

    procedure addAddress(list: PDissectDataArray; address: ptrUint; referencedBy: dword; couldbestring: boolean=false);
    function findaddress(list: PDissectDataArray; address: ptrUint):PAddresslist;
  public
    percentagedone: dword;
    processid: dword;

    done: boolean;
    currentaddress:ptrUint;
    memoryregion: tmemoryregions;

    totalmemory: ptrUint;
    totalread: ptrUint;

    nrofunconditionaljumps: integer;
    nrofconditionaljumps: integer;
    nrofcalls: integer;
    nrofdata: integer;
    nrofstring: integer;
    maxoffset: integer;

    function FindOffset(s: string):ptrUint;
    function CheckAddress(address: ptrUint; var aresult: tdissectarray):boolean;
    procedure getstringlist(s: tstrings);
    constructor create(suspended: boolean);
  protected
    procedure Execute; override;
  end;



implementation


{ TDissectCodeThread }

{
This thread will scan the memory for jumps and conditional jumps
that data will be added to a list that the disassemblerview can read out for data

}

type TDatapath=array[0..7] of record
  list: PDissectDataArray;
  entrynr: integer;
end;

type PDatapath=^TDatapath;


function TDissectCodeThread.findaddress(list: PDissectDataArray; address:ptrUint):PAddresslist;
{
locates the given address and returns the AddressList object pointer if found.
returns nil if not found
}
var
  level: integer;
  entrynr: integer;
  currentArray: PDissectDataArray;
begin
  result:=nil;
  level:=0;
  currentarray:=list;
  while level<7 do
  begin
    entrynr:=address shr ((7-level)*4) and $f;
    if currentarray[entrynr].DissectDataArray=nil then exit; //not in the list

    currentarray:=currentarray[entrynr].DissectDataArray;
    inc(level);
  end;

  entrynr:=address shr ((7-level)*4) and $f;
  result:=currentarray[entrynr].addresslist;
end;

function TDissectCodeThread.CheckAddress(address: ptrUint; var aresult: tdissectarray):boolean;
var i,j: integer;
    totalsize: integer;

    unclist, condlist, clist: PAddresslist;
begin
  totalsize:=0;
  unclist:=findaddress(unconditionaljumplist, address);
  if unclist<>nil then //it has unconditonal jumps jumping to it
    inc(totalsize, unclist.pos);

  condlist:=findaddress(conditionaljumplist, address);
  if condlist<>nil then
    inc(totalsize, condlist.pos);

  clist:=findaddress(calllist, address);
  if clist<>nil then
    inc(totalsize, clist.pos);

  if totalsize>0 then
  begin
    //has results
    setlength(aresult,totalsize);
    j:=0;
    if condlist<>nil then
    begin
      for i:=0 to condlist.pos-1 do
      begin
        aresult[j].address:=condlist.a[i];
        aresult[j].jumptype:=jtConditional;
        inc(j);
      end;

    end;

    if unclist<>nil then
    begin
      for i:=0 to unclist.pos-1 do
      begin
        aresult[j].address:=unclist.a[i];
        aresult[j].jumptype:=jtUnconditional;
        inc(j);
      end;

    end;

    if clist<>nil then
    begin
      for i:=0 to clist.pos-1 do
      begin
        aresult[j].address:=clist.a[i];
        aresult[j].jumptype:=jtCall;
        inc(j);
      end;

    end;

    result:=true;
  end
  else result:=false;

end;


procedure addaddresstostringlist(al: PAddresslist; address: ptrUint; s: tstrings);
var o: TStringReference;
begin
  o:=TStringReference.create;
  o.address:=address;
  setlength(o.references, al.pos);
  CopyMemory(@o.references[0], al.a, al.pos*sizeof(pointer));

  s.AddObject(IntToHex(address,8), o);
end;


function datapathToAddress(datapath: PDatapath): ptrUint;
{
for use when find the address of a map after traversing it
}
begin
  result:=datapath[0].entrynr shl 28+
          datapath[1].entrynr shl 24+
          datapath[2].entrynr shl 20+
          datapath[3].entrynr shl 16+
          datapath[4].entrynr shl 12+
          datapath[5].entrynr shl 8+
          datapath[6].entrynr shl 4+
          datapath[7].entrynr;
end;


procedure fillstringlist(datapath: PDatapath; level: integer; s: tstrings);
var
  i: integer;
  list: PDissectDataArray;
begin
  list:=datapath[level].list;
 
  if level<7 then
  begin
    for i:=0 to 15 do
    begin
      if list[i].DissectDataArray<>nil then
      begin
        datapath[level].entrynr:=i;
        datapath[level+1].list:=list[i].DissectDataArray;
        fillstringlist(datapath, level+1, s);
      end;
    end;
  end
  else
  begin
    //final level
    for i:=0 to 15 do
    begin
      if list[i].addresslist<>nil then
      begin
        if list[i].addresslist.isstring then
        begin
          datapath[level].entrynr:=i;

          //evaluate datapath to find the address
          addaddresstostringlist(list[i].addresslist, datapathToAddress(datapath), s);

        end;
      end;
    end;
  end;
end;

procedure TDissectCodeThread.getstringlist(s: tstrings);
var
  datapath: TDatapath;
begin
  ZeroMemory(@datapath, sizeof(datapath));

  datapath[0].list:=memorylist;
  fillstringlist(@datapath, 0, s);


end;


procedure TDissectCodeThread.addAddress(list: PDissectDataArray; address: ptrUint; referencedBy: dword; couldbestring: boolean=false);
var
  level: integer;
  entrynr: integer;
  temp,currentarray: PDissectDataArray;
begin
  currentarray:=list;

  level:=0;
  while level<7 do
  begin
    //add the path if needed
    entrynr:=address shr ((7-level)*4) and $f;
    if currentarray[entrynr].DissectDataArray=nil then //allocate
    begin
      getmem(temp, sizeof(TdissectDataArray));
      ZeroMemory(temp, sizeof(TdissectDataArray));
      currentarray[entrynr].DissectDataArray:=temp;
    end;

    currentarray:=currentarray[entrynr].DissectDataArray;
    inc(level);
  end;

  //got till level 7
  entrynr:=address shr ((7-level)*4) and $f;
  if currentarray[entrynr].addresslist=nil then //allocate
  begin
    getmem(currentarray[entrynr].addresslist,sizeof(Taddresslist));
    ZeroMemory(currentarray[entrynr].addresslist,sizeof(Taddresslist));

    if couldbestring then currentarray[entrynr].addresslist.isstring:=isString(address);

    //allocate some space for it
    currentarray[entrynr].addresslist.maxsize:=2;
    getmem(currentarray[entrynr].addresslist.a, 2*sizeof(pointer));
  end;


  if currentarray[entrynr].addresslist.pos>=currentarray[entrynr].addresslist.maxsize then //realloc
  begin
    ReallocMem(currentarray[entrynr].addresslist.a, currentarray[entrynr].addresslist.maxsize*2*sizeof(pointer));
    currentarray[entrynr].addresslist.maxsize:=currentarray[entrynr].addresslist.maxsize*2;
  end;

  currentarray[entrynr].addresslist.a[currentarray[entrynr].addresslist.pos]:=referencedby;
  inc(currentarray[entrynr].addresslist.pos);

  if couldbestring and currentarray[entrynr].addresslist.isstring then inc(nrofstring);

end;

function TDissectCodeThread.isstring(address: ptrUint): boolean;
var
  tempbuf: array [0..7] of byte;
  x: dword;
  i: integer;
begin
  result:=false;
  if readprocessmemory(processhandle, pointer(address), @tempbuf[0], 8, x) then
  begin
    //check if ascii string
    result:=true;
    for i:=0 to 4 do //only interested in the first 5 for ascii
    begin
      if not (tempbuf[i] in [32..127]) then
      begin
        result:=false;
        break;
      end;
    end;

    if true then exit;

    //check if unicode string
    i:=0;
    result:=true;
    while i<8 do
    begin
      if not (tempbuf[i] in [32..127]) then
      begin
        result:=false;
        break;
      end;
      inc(i,2);
    end;

  end;
end;



function TDissectCodeThread.FindOffset(s: string):ptrUint;
//check an address specifier for an offset
//returns 0 if no offset
//yes yes, I know this also finds the EAx EBx, ECx EDx, but I bet there are bigger offsets to be found
var
  i: integer;
  first: integer;
  o: string;
begin
  result:=0;
  first:=0;
  for i:=1 to length(s) do
  begin
    if first=0 then //find a start
    begin
      if s[i] in ['0'..'9','A'..'F','a'..'f'] then
        first:=i;
    end else  //find an end
    begin
      if not (s[i] in ['0'..'9','A'..'F','a'..'f']) then
      begin
        o:=copy(s, first, i-first);

        result:=StrToQWordEx('$'+o);


        if result>=$10000 then result:=0;
        if isAddress(result) then result:=0;

        first:=0;
      end;
    end;
  end;
end;


procedure TDissectCodeThread.Execute;
var
  i,j: integer;
  d: TDisassembler;
  currentAddress: ptrUint;
  oldaddress: ptrUint;
  x: string;
  s: string;
  tempaddress: ptrUint;

  a, b, o,special: string;


begin
  d:=TDisassembler.Create;
  d.showsymbols:=false;
  d.showmodules:=false;

  //find out how much memory to go through (for the progressbar)
  totalmemory:=0;
  for i:=0 to length(memoryregion)-1 do
    inc(totalmemory,memoryregion[i].MemorySize);

  totalread:=0;
  for i:=0 to length(memoryregion)-1 do
  begin
    currentAddress:=memoryregion[i].BaseAddress;


    while (not terminated) and (currentaddress<memoryregion[i].BaseAddress+memoryregion[i].MemorySize) do
    begin
      oldaddress:=currentaddress;
      s:=d.disassemble(currentaddress, x);
      inc(totalread,currentaddress-oldaddress);
      percentagedone:=(totalread * 100) div totalmemory;


      //evaluate S
      splitDisassembledString(s, false, a,b,o,special);      
      if hasAddress(o,tempaddress) then
      begin
        //check the kind of address (calltarget, jumptarget, memory access/indicator)
        case o[1] of
          'c' : //call
          begin
            if (o[2]='a') and (o[3]='l') then
            begin
              addAddress(calllist, tempaddress, oldaddress);
              inc(nrofcalls);
            end;
          end;

          'j' : //jmp, conditional or not
          begin
            if o[2]='m' then
            begin
              //jmp
              addAddress(unconditionaljumplist, tempaddress,oldaddress);
              inc(nrofunconditionaljumps);
            end
            else
            begin
              //jx
              addAddress(conditionaljumplist, tempaddress,oldaddress);
              inc(nrofconditionaljumps);
            end;
          end;

          else
          begin
            //memoryaccess/indicator
            addAddress(memorylist, tempaddress, oldaddress, true);
            inc(nrofdata);
          end;

        end;

      end else
      begin
        //it doesn't have an address
        j:=pos('[',o);
        if j>0 then
        begin
          //it has an address specifier
          //maxoffset:=max(maxoffset, FindOffset(copy(o,j,pos(']',o)-j+1)));
        end;
      end;
    end;
  end;

  done:=true;



end;

constructor TDissectCodeThread.create(suspended: boolean);
begin
  getmem(callList, sizeof(TDissectDataArray));
  ZeroMemory(calllist, sizeof(TDissectDataArray));
  getmem(unconditionaljumplist, sizeof(TDissectDataArray));
  ZeroMemory(unconditionaljumplist, sizeof(TDissectDataArray));
  getmem(conditionaljumplist, sizeof(TDissectDataArray));
  ZeroMemory(conditionaljumplist, sizeof(TDissectDataArray));
  getmem(memorylist, sizeof(TDissectDataArray));
  ZeroMemory(memorylist, sizeof(TDissectDataArray));


  inherited create(suspended);
end;

end.

