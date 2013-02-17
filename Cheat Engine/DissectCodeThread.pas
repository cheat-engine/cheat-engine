unit DissectCodeThread;

{$MODE Delphi}

interface

uses
  windows, LCLIntf,sysutils,syncobjs,Classes,disassembler, NewKernelHandler, math, CEFuncProc, maps;


type
  TAddresslist=record
    pos: integer;
    maxsize: integer;
    a: PPtrUintArray;
    isstring: boolean;
  end;
  PAddresslist=^TAddresslist;




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
    calllist: TMap;
    unconditionaljumplist: TMap;
    conditionaljumplist: TMap;
    memorylist: TMap; //e.g strings

    cs: TCriticalSection;

    haswork: TEvent;


    function isstring(address: ptrUint): boolean;
    function findaddress(list: TMap; address: ptrUint):PAddresslist;
    procedure clearList(list: Tmap);
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
    procedure addAddress(list: TMap; address: ptrUint; referencedBy: ptruint; couldbestring: boolean=false);
    procedure removeAddress(list: TMap; address: ptruint; referencedBy: ptruint);
    procedure clear;

    procedure dowork;


    procedure getstringlist(s: tstrings);
    constructor create(suspended: boolean);
  protected
    procedure Execute; override;
  end;

var dissectcode: tdissectcodethread;


implementation


{ TDissectCodeThread }

{
This thread will scan the memory for jumps and conditional jumps
that data will be added to a list that the disassemblerview can read out for data

}

procedure TDissectCodeThread.dowork;
begin
  //if done=false then exit; Really, if you don't watch the done state it's your own fault

  done:=false;
  haswork.SetEvent;
end;

procedure TDissectCodeThread.clearList(list: Tmap);
var mi: TMapIterator;
  al: PAddresslist;
begin
  cs.enter;
  mi:=TMapIterator.Create(list);
  mi.First;
  while not mi.EOM do
  begin
    mi.GetData(al);
    freemem(al.a);
    freemem(al);
    mi.Next;
  end;

  list.Clear;
  cs.leave;
end;

procedure TDissectCodeThread.clear;
begin
  clearList(calllist);
  clearList(unconditionaljumplist);
  clearList(conditionaljumplist);
  clearList(memorylist);

  totalmemory:=0;
  totalread:=0;

  nrofunconditionaljumps:=0;
  nrofconditionaljumps:=0;
  nrofcalls:=0;
  nrofdata:=0;
  nrofstring:=0;
  maxoffset:=0;

end;

function TDissectCodeThread.findaddress(list: TMap; address:ptrUint):PAddresslist;
{
locates the given address and returns the AddressList object pointer if found.
returns nil if not found
}
begin
  cs.enter;
  try
    if list.GetData(address, result) =false then
      result:=nil;
  finally
    cs.leave;
  end;

end;

function TDissectCodeThread.CheckAddress(address: ptrUint; var aresult: tdissectarray):boolean;
var i,j: integer;
    totalsize: integer;

    unclist, condlist, clist: PAddresslist;
begin
  totalsize:=0;

  cs.Enter;
  try
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

  finally
    cs.leave;
  end;

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


procedure TDissectCodeThread.getstringlist(s: tstrings);
var mi: TMapIterator;
    al: PAddresslist;
    address: ptruint;
begin
  s.clear;

  cs.enter;
  mi:=TMapIterator.Create(memorylist);

  mi.first;
  try
    while not mi.EOM do
    begin
      mi.GetData(al);
      if al.isstring then
      begin
        mi.GetID(address);
        addaddresstostringlist(al, address, s);
      end;
      mi.Next;
    end;

  finally
    cs.leave;
  end;
end;

procedure TDissectCodeThread.removeAddress(list: TMap; address: ptruint; referencedBy: ptruint);
var al: PAddresslist;
    i,j: integer;
begin
  cs.Enter;
  try
    if list.GetData(address, al) then
    begin
      //find this address in the list and remove it
      for i:=0 to al.pos-1 do
      begin
        if al.a[i]=referencedBy then
        begin
          //found it, shift all entries after it one spot back
          for j:=i to al.pos-2 do
            al.a[j]:=al.a[j+1];

          //and decrease the pos
          dec(al.pos);

          if al.pos<=0 then //list is empty
            list.Delete(address);

          exit;
        end;
      end;
    end;

  finally
    cs.leave;
  end;
end;

procedure TDissectCodeThread.addAddress(list: TMap; address: ptrUint; referencedBy: ptruint; couldbestring: boolean=false);
var
  al: PAddresslist;
begin

  cs.enter;
  try
    if list.GetData(address, al)=false then
    begin
      //not in the list yet, add it
      getmem(al, sizeof(TAddresslist));
      ZeroMemory(al, sizeof(TAddresslist));
      if couldbestring then al.isstring:=isString(address);

      //allocate some space for it
      al.maxsize:=2;
      getmem(al.a, 2*sizeof(pointer));

      list.Add(address, al);
    end;

    if al.pos>=al.maxsize then //realloc
    begin
      ReallocMem(al.a, al.maxsize*2*sizeof(pointer));
      al.maxsize:=al.maxsize*2;
    end;

    al.a[al.pos]:=referencedby;
    inc(al.pos);

    if couldbestring and al.isstring then inc(nrofstring);

  finally
    cs.leave;
  end;

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
  try
    d.showsymbols:=false;
    d.showmodules:=false;

    while not terminated do
    begin
      //wait for the haswork event to be set
      if haswork.WaitFor(INFINITE)=wrSignaled then
      begin
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
      end;

      done:=true;
    end;


  finally

    d.free;
  end;
end;

constructor TDissectCodeThread.create(suspended: boolean);
begin
  calllist:=TMap.Create(ituPtrSize, sizeof(PAddresslist) );
  unconditionaljumplist:=TMap.Create(ituPtrSize, sizeof(PAddresslist) );
  conditionaljumplist:=TMap.Create(ituPtrSize, sizeof(PAddresslist) );
  memorylist:=TMap.Create(ituPtrSize, sizeof(PAddresslist) );
  cs:=TCriticalSection.Create;

  haswork:=TEvent.Create(nil, false, false, '');
  inherited create(suspended);
end;

end.

