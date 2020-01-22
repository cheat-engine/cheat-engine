unit DissectCodeThread;

{$MODE Delphi}

interface

uses
  {$ifdef darwin}
  macport,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}
  LCLIntf,sysutils,syncobjs,Classes,disassembler, NewKernelHandler, math,
  MemoryQuery, CEFuncProc, maps, LastDisassembleData, commonTypeDefs;


type
  TAddresslist=record
    pos: integer;
    maxsize: integer;
    a: PPtrUintArray;
    isstring: boolean;
    pointsto: ptruint; //0 if not a pointer
    pointstostring: boolean;
  end;
  PAddresslist=^TAddresslist;




type tjumptype=(jtCall=0, jtUnconditional=1, jtConditional=2, jtMemory=3);

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
  isUnicode: boolean;
end;

type TDissectReference=class(tobject)
  address: ptruint;
  addressname: string;   //fill these in later
  references: array of record
      address: ptruint;
      addressname: string;
    end;
end;

type TOnCallListEntry=procedure(address: ptruint);

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
    ready: TEvent;

    canceled: boolean;


    function isstring(address: ptrUint; v: pqword=nil): boolean;
    function findaddress(list: TMap; address: ptrUint):PAddresslist;
    procedure clearList(list: Tmap);
    procedure saveListToStream(list: TMap; s: TStream);
    procedure loadListFromStream(list: TMap; s: TStream);
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
    procedure addReference(FromAddress, ToAddress: ptruint; reftype: tjumptype; isstring: boolean=false);
    procedure removeReference(FromAddress, ToAddress: ptruint);

    procedure addAddress(list: TMap; address: ptrUint; referencedBy: ptruint; couldbestring: boolean=false);
    procedure removeAddress(list: TMap; address: ptruint; referencedBy: ptruint);
    procedure clear;

    procedure dowork;

    procedure waitTillDone;
    procedure cancelScan;

    procedure saveTofile(filename: string);
    procedure loadFromFile(filename: string);


    procedure getstringlist(s: tstrings);
    procedure getCallList(s: TList);

//    procedure getCallListEx(callback: TOnCallListEntry);
    constructor create(suspended: boolean);
  protected
    procedure Execute; override;
  end;

var dissectcode: tdissectcodethread;


implementation

uses ProcessHandlerUnit, parsers;

resourcestring
  rsInvalidDissectCodeFile = 'Invalid dissect code file';
  rsTDissectCodeThreadgetCallListCalledWithANonEmptyList = 'TDissectCodeThread.getCallList called with a non empty list';

{ TDissectCodeThread }

{
This thread will scan the memory for jumps and conditional jumps
that data will be added to a list that the disassemblerview can read out for data

}
procedure TDissectCodeThread.saveListToStream(list: TMap; s: TStream);
var mi: TMapIterator;
  id: ptruint;
  al: PAddresslist;
  count: dword;

  i: integer;
begin
  mi:=TMapIterator.Create(list);
  try
    count:=0; //first get the count
    mi.First;
    while not mi.EOM do
    begin
      inc(count);

      mi.Next;
    end;

    s.WriteDWord(count);

    mi.First;
    while not mi.EOM do
    begin
      mi.GetID(id);
      mi.GetData(al);

      s.WriteQWord(id);
      s.WriteDWord(ifthen(al.isstring, 1, 0));
      s.WriteDWord(al.pos);


      for i:=0 to al.pos-1 do
        s.writeQword(al.a[i]);

      mi.Next;
    end;

  finally
    mi.free;
  end;
end;

procedure TDissectCodeThread.saveTofile(filename: string);
var fs: Tfilestream;
begin
  fs:=tfilestream.create(filename, fmCreate);

  try
    fs.WriteDWord($ce00dc01);  //cheat engine dissect code v1
    saveListToStream(calllist, fs);
    saveListToStream(unconditionaljumplist, fs);
    saveListToStream(conditionaljumplist, fs);
    saveListToStream(memorylist, fs);
  finally
    fs.free;
  end;
end;

procedure TDissectCodeThread.loadListFromStream(list: TMap; s: TStream);
var
  count: dword;
  i,j: integer;
  id: ptruint;

  pos: integer;

  al: PAddresslist;
begin
  clearList(list);

  count:=s.ReadDWord;
  for i:=0 to count-1 do
  begin
    getmem(al, sizeof(TAddresslist));

    id:=s.ReadQword;
    al.isstring:=s.ReadDword=1;
    al.maxsize:=s.ReadDWord;
    al.pos:=al.maxsize;

    if al.pos>0 then
    begin
      getmem(al.a, al.pos*sizeof(PtrUInt));
      for j:=0 to al.pos-1 do
        al.a[j]:=s.ReadQWord;

      list.Add(id, al);
    end;
  end;
end;

procedure TDissectCodeThread.loadFromFile(filename: string);
var fs: Tfilestream;
begin
  fs:=tfilestream.create(filename, fmOpenRead);

  clear;
  try
    if fs.ReadDWord<>$ce00dc01 then
      raise exception.create(rsInvalidDissectCodeFile);


    loadListFromStream(calllist, fs);
    loadListFromStream(unconditionaljumplist, fs);
    loadListFromStream(conditionaljumplist, fs);
    loadListFromStream(memorylist, fs);
  finally
    fs.free;
  end;


end;

procedure TDissectCodeThread.cancelScan;
begin
  canceled:=true;
  waitTillDone;
  canceled:=false;
end;

procedure TDissectCodeThread.waitTillDone; //mainly for lua
begin
  while done=false do
    ready.WaitFor(INFINITE);
end;

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
    if al<>nil then
    begin
      if al.a<>nil then
        freememandnil(al.a);

      freememandnil(al);
    end;
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

    unclist, condlist, clist, mlist: PAddresslist;
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

    mlist:=findaddress(memorylist, address);
    if mlist<>nil then
      inc(totalsize, mlist.pos);

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

      if mlist<>nil then
      begin
        for i:=0 to mlist.pos-1 do
        begin
          aresult[j].address:=mlist.a[i];
          aresult[j].jumptype:=jtMemory;
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
  CopyMemory(@o.references[0], al.a, al.pos*sizeof(ptrUint));

  s.AddObject(IntToHex(address,8), o);
end;


procedure TDissectCodeThread.getCallList(s: TList);
var
  mi: TMapIterator;
  al: PAddresslist;
  address: ptruint;


  t: TDissectReference;
  i: integer;
begin
  if s.Count>0 then
    raise exception.create(rsTDissectCodeThreadgetCallListCalledWithANonEmptyList);

  cs.enter;
  mi:=TMapIterator.Create(calllist);
  try
    while not mi.EOM do
    begin
      mi.GetData(al);
      mi.GetID(address);

      t:=TDissectReference.Create;
      t.address:=address;
      t.addressname:='';
      setlength(t.references, al.pos);
      for i:=0 to al.pos-1 do
      begin
        t.references[i].address:=al.a[i];
        t.references[i].addressname:='';
      end;

      s.Add(t);
      mi.next;
    end;
  finally
    cs.leave;
  end;

  mi.free;

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

  mi.free;
end;

procedure TDissectCodeThread.addReference(FromAddress, ToAddress: ptruint; reftype: tjumptype; isstring: boolean=false);
begin
  case reftype of
    jtCall: addAddress(calllist, toaddress, fromaddress, isstring);
    jtUnconditional: addAddress(unconditionaljumplist, toaddress, fromaddress, isstring);
    jtConditional: addAddress(conditionaljumplist, toaddress, fromaddress, isstring);
    jtMemory: addAddress(memorylist, toaddress, fromaddress, isstring);
  end;
end;

procedure TDissectCodeThread.removeReference(FromAddress, ToAddress: ptruint);
begin
  //for each list
  removeAddress(calllist, ToAddress, FromAddress);
  removeAddress(unconditionaljumplist, ToAddress, FromAddress);
  removeAddress(conditionaljumplist, ToAddress, FromAddress);
  removeAddress(memorylist, ToAddress, FromAddress);
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
  v: qword;
begin

  cs.enter;
  try
    if list.GetData(address, al)=false then
    begin
      //not in the list yet, add it
      getmem(al, sizeof(TAddresslist));
      ZeroMemory(al, sizeof(TAddresslist));
      if couldbestring then
      begin
        v:=0;
        al.isstring:=isString(address, @v);

        if (al.isstring=false) and (v<>0) then
        begin
          if isAddress(v) then
          begin
            al.pointsto:=v;
            al.pointstostring:=isString(v,nil);
          end;
        end;
      end;

      //allocate some space for it
      al.maxsize:=2;
      getmem(al.a, 2*sizeof(pointer));

      list.Add(address, al);
    end;

    if (couldbestring) and (al.isstring=false) and (al.pointstostring) then
      addAddress(list, al.pointsto,referencedby,couldbestring); //add pointers to pointers to strings

    //might also be used for function references...

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

function TDissectCodeThread.isstring(address: ptrUint; v: pqword=nil): boolean;
var
  p: qword;
  tempbuf: array [0..7] of byte absolute p;
  x: ptruint;
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

    if result then exit;

    //check if unicode string
    i:=0;
    result:=true;
    while i<8 do
    begin
      if not ((tempbuf[i] in [32..127]) and (tempbuf[i+1]=0)) then
      begin
        result:=false;
        break;
      end;
      inc(i,2);
    end;

    if v<>nil then
    begin
      if processhandler.is64Bit then
        p:=p and $ffffffff;

      v^:=p;
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
  tempaddress, tempaddress2: ptrUint;

  value: ptruint;

  a, b, o,special: string;

  br: ptruint;


begin
  d:=TDisassembler.Create;
  try
    d.showsymbols:=false;
    d.showmodules:=false;
    d.aggressivealignment:=true;

    while not terminated do
    begin
      //wait for the haswork event to be set
      if haswork.WaitFor(INFINITE)=wrSignaled then
      begin
        //find out how much memory to go through (for the progressbar)

        ready.ResetEvent;

        totalmemory:=0;
        for i:=0 to length(memoryregion)-1 do
          inc(totalmemory,memoryregion[i].MemorySize);

        totalread:=0;
        for i:=0 to length(memoryregion)-1 do
        begin
          currentAddress:=memoryregion[i].BaseAddress;


          while (not canceled) and (currentaddress<memoryregion[i].BaseAddress+memoryregion[i].MemorySize) do
          begin
            oldaddress:=currentaddress;
            s:=d.disassemble(currentaddress, x);
            inc(totalread,currentaddress-oldaddress);
            percentagedone:=(totalread * 100) div totalmemory;


            //evaluate S
            splitDisassembledString(s, false, a,b,o,special);
            if hasAddress(o,tempaddress) then
            begin

              //check if it's an indirect jump

              tempaddress2:=0;



              if readprocessmemory(processhandle, pointer(tempaddress), @tempaddress2, processhandler.pointersize, br) then
              begin
                if ((tempaddress2 and $ffff)=$25ff) then //it's pointing to a jmp [xxxxxxxx] evaluate that as well
                begin
                  value:=0;
                  if readprocessmemory(processhandle,pointer(tempaddress+2),@value,4,br) then
                  begin
                    if processhandler.is64bit then
                      value:=tempaddress2+6+value; //in 64-bit ff 25 is relative encoded

                    if readprocessmemory(processhandle,pointer(value),@value,processhandler.Pointersize,br) then
                      tempaddress:=value; //get the address it actually points to
                  end;


                end
                else
                begin
                  //use the address it points to
                  if (d.LastDisassembleData.isjump or d.LastDisassembleData.isret) and (d.LastDisassembleData.modrmValueType=dvtAddress) then
                    tempaddress:=tempaddress2;
                end;

              end;


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
      ready.SetEvent;
    end;


  finally

    d.free;
  end;
end;

constructor TDissectCodeThread.create(suspended: boolean);
begin
  done:=true; //ready to do some work
  calllist:=TMap.Create(ituPtrSize, sizeof(PAddresslist) );
  unconditionaljumplist:=TMap.Create(ituPtrSize, sizeof(PAddresslist) );
  conditionaljumplist:=TMap.Create(ituPtrSize, sizeof(PAddresslist) );
  memorylist:=TMap.Create(ituPtrSize, sizeof(PAddresslist) );
  cs:=TCriticalSection.Create;

  haswork:=TEvent.Create(nil, false, false, '');
  ready:=TEvent.create(nil, false, true, '');
  inherited create(suspended);

end;

end.

