unit DissectCodeThread;

interface

uses
  cefuncproc,windows,sysutils,syncobjs,Classes,disassembler, newkernelhandler;


type
  TAddresslist=record
    pos: integer;
    maxsize: integer;
    a: PDwordArray;
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
                      address: dword;
                      jumptype: tjumptype;
                    end;

type tjumpdata = record
                   address:dword;
                   codes: integer;
                   code: array[0..7] of dword; //default space for 8 addresses

                   nrofextracodes: integer;
                   extracodes: array of dword; //but can be expanded to more if really needed
                 end;

type tjumparray=array of tjumpdata;

type
  TDissectCodeThread = class(TThread)
  private
    { Private declarations }
    unconditionaljump: tjumparray;
    conditionaljump: tjumparray;
    calls: tjumparray;

    calllist: PDissectDataArray;
    unconditionaljumplist: PDissectDataArray;
    conditionaljumplist: PDissectDataArray;
    memorylist: PDissectDataArray;


    function isstring(address: dword): boolean;
    function hasAddress(d: string; var address: dword):boolean;
    function isAddress(address: dword): boolean;

    procedure addAddress(list: PDissectDataArray; address: dword; referencedBy: dword; mustbestring: boolean=false);
    function findaddress(address:dword; const list: tjumparray; currentsize: integer; var recnr: integer):boolean;
  public
    percentagedone: dword;
    processid: dword;

    done: boolean;
    currentaddress:dword;
    memoryregion: tmemoryregions;

    totalmemory: dword;
    totalread: dword;

    nrofunconditionaljumps: integer;
    nrofconditionaljumps: integer;
    nrofcalls: integer;
    nrofdata: integer;    

    function CheckAddress(address: dword; var aresult: tdissectarray):boolean;

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
function TDissectCodeThread.findaddress(address:dword; const list: tjumparray; currentsize: integer; var recnr: integer):boolean;
var i: integer;
    first,last: integer;
begin
  result:=false;

end;

function TDissectCodeThread.CheckAddress(address: dword; var aresult: tdissectarray):boolean;
var a,b,c: integer;
    fa,fb,fc: boolean;
    i,j: integer;
    totalsize: integer;
begin
  result:=false;
  if not done then exit;

  totalsize:=0;

  fa:=false;
  fb:=false;
  fc:=false;
  
  //check the unconditionaljump list
  if findaddress(address,unconditionaljump,nrofunconditionaljumps,a) then
  begin
    totalsize:=unconditionaljump[a].codes+unconditionaljump[a].nrofextracodes;
    fa:=true;
  end;

  //check the conditionaljump list
  if findaddress(address,conditionaljump,nrofconditionaljumps,b) then
  begin
    inc(totalsize,conditionaljump[b].codes+conditionaljump[b].nrofextracodes);
    fb:=true;
  end;

  //check the calllist
  if findaddress(address,calls,nrofcalls,c) then
  begin
    inc(totalsize,calls[c].codes+calls[c].nrofextracodes);
    fc:=true;
  end;

  result:=fa or fb or fc;

  if result then
  begin
    setlength(aresult,totalsize);
    j:=0;

    if fa then
    begin
      for i:=0 to unconditionaljump[a].codes-1 do
      begin
        aresult[j].address:=unconditionaljump[a].code[i];
        aresult[j].jumptype:=jtUnconditional;
        inc(j);
      end;

      for i:=0 to unconditionaljump[a].nrofextracodes-1 do
      begin
        aresult[j].address:=unconditionaljump[a].extracodes[i];
        aresult[j].jumptype:=jtUnconditional;
        inc(j);
      end;
    end;

    if fb then
    begin
      for i:=0 to conditionaljump[b].codes-1 do
      begin
        aresult[j].address:=conditionaljump[b].code[i];
        aresult[j].jumptype:=jtConditional;
        inc(j);
      end;

      for i:=0 to conditionaljump[b].nrofextracodes-1 do
      begin
        aresult[j].address:=conditionaljump[b].extracodes[i];
        aresult[j].jumptype:=jtConditional;
        inc(j);
      end;
    end;

    if fc then
    begin
      for i:=0 to calls[c].codes-1 do
      begin
        aresult[j].address:=calls[c].code[i];
        aresult[j].jumptype:=jtCall;
        inc(j);
      end;

      for i:=0 to calls[c].nrofextracodes-1 do
      begin
        aresult[j].address:=calls[c].extracodes[i];
        aresult[j].jumptype:=jtCall;
        inc(j);
      end;
    end;
  end;

end;

procedure TDissectCodeThread.addAddress(list: PDissectDataArray; address: dword; referencedBy: dword; mustbestring: boolean=false);
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

    if mustbestring then currentarray[entrynr].addresslist.isstring:=isString(address);

    if (not mustbestring) or (currentarray[entrynr].addresslist.isstring) then
    begin
      //allocate some space for it
      currentarray[entrynr].addresslist.maxsize:=2;
      getmem(currentarray[entrynr].addresslist.a, 2*sizeof(pointer));
    end;
  end;

  if (not mustbestring) or (currentarray[entrynr].addresslist.isstring) then
  begin
    //add it, it's eiher a string, or it doesn't matter what type it is
    if currentarray[entrynr].addresslist.pos>=currentarray[entrynr].addresslist.maxsize then //realloc
    begin
      ReallocMem(currentarray[entrynr].addresslist.a, currentarray[entrynr].addresslist.maxsize*2*sizeof(pointer));
      currentarray[entrynr].addresslist.maxsize:=currentarray[entrynr].addresslist.maxsize*2;
    end;

    currentarray[entrynr].addresslist.a[currentarray[entrynr].addresslist.pos]:=referencedby;
    inc(currentarray[entrynr].addresslist.pos);

    if mustbestring then inc(nrofdata);
  end;


end;

function TDissectCodeThread.isstring(address: dword): boolean;
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
    for i:=0 to 3 do //only interested in the first 4 for ascii
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


function TDissectCodeThread.isAddress(address: dword):boolean;
var mbi: TMemoryBasicInformation;
begin
  result:=false;
  if VirtualQueryEx(processhandle, pointer(address), mbi, sizeof(mbi))>0 then
    result:=(mbi.State=MEM_COMMIT) and (mbi.AllocationProtect<>PAGE_NOACCESS);
end;

function TDissectCodeThread.hasAddress(d: string; var address: dword):boolean;
var
  s: string;
  i: integer;
  hexcount: integer;
begin

  result:=false;

  if pos('+',d)>0 then exit; //it has an offset
  
  //check O for a hexadecimal value of 8 bytes and longer.
  hexcount:=0;
  for i:=1 to length(d) do
  begin
    if d[i] in ['a'..'f','A'..'F','0'..'9'] then
    begin
      inc(hexcount);
      if hexcount=8 then
      begin
        //it has a 4 byte hexadecimal value
        s:=copy('$'+d,i-6,8);
        address:=strtoint('$'+s);

        result:=isAddress(address);
      end;
    end else hexcount:=0;

  end;



end;


procedure TDissectCodeThread.Execute;
var
  i: integer;
  d: TDisassembler;
  currentAddress: dword;
  oldaddress: dword;
  x: string;
  s: string;
  tempaddress: dword;

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


    while currentaddress<memoryregion[i].BaseAddress+memoryregion[i].MemorySize do
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
            addAddress(calllist, tempaddress, oldaddress);
            inc(nrofcalls);
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

          end;

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

