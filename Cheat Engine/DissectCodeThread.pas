unit DissectCodeThread;

interface

uses
  cefuncproc,windows,sysutils,syncobjs,Classes,disassembler;

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

    nrofunconditionaljumps: integer;
    nrofconditionaljumps: integer;
    nrofcalls: integer;
    function findaddress(address:dword; const list: tjumparray; currentsize: integer; var recnr: integer):boolean;
  public
    percentagedone: dword;
    processid: dword;

    done: boolean;
    currentaddress:dword;
    memoryregion: tmemoryregions;

    totalmemory: dword;
    totalread: dword;

    accuracy: integer;


    function CheckAddress(address: dword; var aresult: tdissectarray):boolean;
  protected
    procedure Execute; override;
  end;

implementation

{ Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TDissectCodeThread.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ TDissectCodeThread }

{
This thread will scan the memory for jumps and conditional jumps

}
function TDissectCodeThread.findaddress(address:dword; const list: tjumparray; currentsize: integer; var recnr: integer):boolean;
var i: integer;
    first,last: integer;
begin
  result:=false;

  first:=0;
  last:=currentsize;

  while first<last do
  begin
    i:=first+((last-first) div 2);
    if (i=first) or (i=last) then
    begin
      for i:=first to last-1 do
      begin
        if list[i].address=address then
        begin
          recnr:=i;
          result:=true;
          exit;
        end;
        if list[i].address>address then break;
      end;

      break;
    end;

    if list[i].address=address then
    begin
      recnr:=i;
      result:=true;
      exit;
    end;

    if address<list[i].address then
      last:=i
    else
      first:=i;
  end;

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

procedure TDissectCodeThread.Execute;
type ttempjumpdata=record
  address:dword;
  code: dword;
end;

type ttempjumparray=array [0..127] of ttempjumpdata;
var
  tempunconditionaljumplist: ttempjumparray;
  tempconditionaljumplist: ttempjumparray;
  tempcalls: ttempjumparray;

  cun,cc,ccls: dword;
  a,b,c:integer;
  address: dword;
  mbi: MEMORY_BASIC_INFORMATION;
  temp: array of byte;
  br: dword;
  i,j: integer;


  maxregionsize: dword;
  bytesread: dword;

  start,stop: dword;
  ta: dword;
  ts: string;

  procedure insert(var list: tjumparray;var currentsize: integer; address: ttempjumpdata);
  var i,j: integer;
      lastaddress: dword;

      first,last: integer;
      x: dword;
      temp:string;
  begin
    {x:=previousopcode(address.code);
    x:=previousopcode(x);
    disassemble(x,temp);
    disassemble(x,temp);

    if x<>address.code then exit
    }

    if terminated then exit;


    //check if this address is already in the list. if not, add it.

    first:=0;
    last:=currentsize;
    i:=0;

    while first<last do
    begin
      i:=first+((last-first) div 2);
      if (i=first) or (i=last) then
      begin
        for i:=first to last-1 do
        begin
          if list[i].address=address.address then
          begin
            if list[i].codes<=7 then
            begin
              list[i].code[list[i].codes]:=address.code;
              inc(list[i].codes);
            end
            else
            begin
              inc(list[i].nrofextracodes);
              if list[i].nrofextracodes>=length(list[i].extracodes) then
                setlength(list[i].extracodes,length(list[i].extracodes)+length(list[i].extracodes)+8); //8 more

              list[i].extracodes[list[i].nrofextracodes-1]:=address.code;
            end;
            exit;
          end;
          if list[i].address>address.address then break;
        end;

        break;
      end;

      if list[i].address=address.address then
      begin
        if list[i].codes<=7 then
        begin
          list[i].code[list[i].codes]:=address.code;
          inc(list[i].codes);
        end
        else
        begin
          inc(list[i].nrofextracodes);
          if list[i].nrofextracodes>=length(list[i].extracodes) then
            setlength(list[i].extracodes,length(list[i].extracodes)+length(list[i].extracodes)+8); //8 more

          list[i].extracodes[list[i].nrofextracodes-1]:=address.code;
        end;
        exit;
      end;

      if address.address<list[i].address then
        last:=i
      else
        first:=i;
    end;

    //add new after i
    try
      inc(currentsize);
      if currentsize>=length(list) then
        setlength(list,length(list)+length(list)+512); //allocate more

      for j:=currentsize-1 downto i+1 do
        list[j]:=list[j-1];

      list[i].address:=address.address;
      list[i].codes:=1;
      list[i].code[0]:=address.code;

      list[i].nrofextracodes:=0;
      setlength(list[i].extracodes,0);
    except
      //

    end;
  end;

  procedure flushdata(prefix: string; var buffer: tjumparray);
  {
   //sorted and indexed. that should be fast enough
  }
  var i,j: integer;
      f: tfilestream;
      index: array of record  //
        address: dword;
        offset: dword;  //sure, you might want to use a 64 bit integer, but if that would be needed I'd rather put my head in a automated meatgrinder
      end;

      fname,fname2: string;

  begin
  exit; //dont flush

    if length(buffer)=0 then exit;
    
    fname:=cheatenginedir+'dissect\'+prefix+inttohex(buffer[0].address,8)+inttohex(buffer[length(buffer)-1].address,8);
    fname2:=fname;

    i:=2;
    while fileexists(fname) do
    begin
      fname:=fname2+'_'+inttostr(i);
      inc(i);
    end;
    fname:=fname+'.cod';

    i:=length(buffer);
    setlength(index,i); //max possible

    ForceDirectories(cheatenginedir+'dissect\');
    f:=tfilestream.Create(fname,fmcreate);
    try
      f.WriteBuffer(i,4); //number of addresses
      f.Seek(i*8,soFromCurrent); //fill in the index during the loop

      for i:=0 to length(buffer)-1 do
      begin
        index[i].address:=buffer[i].address;
        index[i].offset:=f.Position;
        f.WriteBuffer(buffer[i].address,4);
        j:=length(buffer[i].code);
        f.WriteBuffer(j,4);
        f.WriteBuffer(buffer[i].code[0],4*j);
      end;

      //write index
      f.Seek(4,soFromBeginning);
      f.WriteBuffer(index[0],i*8);
    finally
      f.free;
    end;

    //delete the old list to make room for the new one
    //for i:=0 to length(buffer)-1 do
      //setlength(buffer[i].code,0);
    setlength(buffer,0);
  end;

  procedure updatelist;
  var k,l,x,y: integer;

  begin


    if terminated then
    begin
      cun:=0;
      cc:=0;
      ccls:=0;
      exit;
    end;

    totalread:=bytesread+j;
    percentagedone:=trunc((bytesread+j)/totalmemory*100.0);
    currentaddress:=memoryregion[i].BaseAddress+j;

    if cun>0 then
    begin

      for k:=0 to cun-1 do
        insert(unconditionaljump,nrofunconditionaljumps,tempunconditionaljumplist[k]);

      inc(a);

      if a=buffersize div 2048 then
      begin
        //create the file to store it to
        flushdata('unc',unconditionaljump);
        a:=0;
      end;

    end;

    if cc>0 then
    begin
      for k:=0 to cc-1 do
        insert(conditionaljump,nrofconditionaljumps,tempconditionaljumplist[k]);

      inc(b);
      if b=buffersize div 2048 then
      begin
        flushdata('con',conditionaljump);
        b:=0;
      end;
    end;

    if ccls>0 then
    begin
      for k:=0 to ccls-1 do
        insert(calls,nrofcalls,tempcalls[k]);

      inc(c);
      if c=buffersize div 2048 then
      begin
        flushdata('call',calls);
        c:=0;
      end;
    end;


    cun:=0;
    cc:=0;
    ccls:=0;
  end;

  var sr: tsearchrec;
begin
  { Place thread code here }

  //delete the old dissection
  if ForceDirectories(cheatenginedir+'dissect\') then
  begin
    if findfirst(cheatenginedir+'dissect\*.*',faAnyFile,sr)=0 then
    begin
      repeat
        deletefile(cheatenginedir+'dissect\'+sr.Name);
      until findnext(sr)<>0;
    end;
  end;


  processid:=cefuncproc.ProcessID;
  address:=0;
  zeromemory(@mbi,sizeof(mbi));
  a:=0;
  b:=0;
  c:=0;
  totalmemory:=0;
  totalread:=0;

  maxregionsize:=0;
  totalmemory:=0;
  bytesread:=0;
  for i:=0 to length(memoryregion)-1 do
  begin
    if maxregionsize<memoryregion[i].MemorySize then
      maxregionsize:=memoryregion[i].MemorySize;
    inc(totalmemory,memoryregion[i].MemorySize);
  end;

  if totalmemory>0 then
  begin
    start:=memoryregion[0].BaseAddress;
    stop:=memoryregion[length(memoryregion)-1].BaseAddress+memoryregion[length(memoryregion)-1].MemorySize;
  end;

  setlength(temp,maxregionsize);

  cun:=0;
  cc:=0;
  ccls:=0;

  for i:=0 to length(memoryregion)-1 do
  begin
    if terminated then break;

    if readprocessmemory(processhandle,pointer(memoryregion[i].BaseAddress),@temp[0],memoryregion[i].MemorySize,br) then
    begin
      for j:=0 to memoryregion[i].MemorySize-1 do
      begin
        case temp[j] of
          $0f: //extra jump
          begin
            if j<memoryregion[i].MemorySize-5 then
            begin
              case temp[j+1] of
                $80..$8f: //conditional 4 bytes
                begin
                  ta:=memoryregion[i].BaseAddress+j+pdword(@temp[j+1])^+5;
                  if (ta<start) or (ta>stop) then continue;

                  tempconditionaljumplist[cc].address:=ta;
                  tempconditionaljumplist[cc].code:=memoryregion[i].baseaddress+j;
                  inc(cc);
                  if cc=128 then
                  begin
                    updatelist;
                    if terminated then break;
                  end;
                end;
              end;
            end;
          end;

          $70..$7f,$e3: //conditional jump (1 byte)
          begin
            if j<memoryregion[i].MemorySize-1 then
            begin
              ta:=memoryregion[i].BaseAddress+j+pshortint(@temp[j+1])^+2;
              if (ta<start) or (ta>stop) then continue;

              tempconditionaljumplist[cc].address:=ta;
              tempconditionaljumplist[cc].code:=memoryregion[i].baseaddress+j;
              inc(cc);
              if cc=128 then
              begin
                updatelist;
                if terminated then break;
              end;
            end;
          end;


          $e8: //call
          begin
            if j<memoryregion[i].MemorySize-4 then
            begin
              ta:=memoryregion[i].BaseAddress+j+plongint(@temp[j+1])^+5;
              if (ta<start) or (ta>stop) then continue;

              tempcalls[ccls].address:=ta;
              tempcalls[ccls].code:=memoryregion[i].baseaddress+j;
              inc(ccls);
              if ccls=128 then
              begin
                updatelist;
                if terminated then break;
              end;
            end;
          end;

          $e9: //jump unconditional (4 byte)
          begin
            if j<memoryregion[i].MemorySize-4 then
            begin
              ta:=memoryregion[i].BaseAddress+j+plongint(@temp[j+1])^+5;
              if (ta<start) or (ta>stop) then continue;

              tempunconditionaljumplist[cun].address:=ta;
              tempunconditionaljumplist[cun].code:=memoryregion[i].BaseAddress+j;
              inc(cun);
              if cun=128 then
              begin
                updatelist;
                if terminated then break;
              end;
            end;
          end;

          $eb: //jump (unconditional) 1 byte
          begin
            if j<memoryregion[i].MemorySize-1 then
            begin
              ta:=dword(memoryregion[i].BaseAddress)+j+pbyte(@temp[j+1])^+2;
              if (ta<start) or (ta>stop) then continue;
              
              tempunconditionaljumplist[cun].address:=ta;
              tempunconditionaljumplist[cun].code:=memoryregion[i].BaseAddress+j;
              inc(cun);
              if cun=128 then
              begin
                updatelist;
                if terminated then break;
              end;
            end;
          end;

        end;
      end;
    end;

    inc(bytesread,memoryregion[i].MemorySize);
    totalread:=bytesread;
    percentagedone:=trunc(bytesread/totalmemory*100.0);
    updatelist;
  end;


  flushdata('unc',unconditionaljump);
  flushdata('con',conditionaljump);
  flushdata('call',calls);
  done:=true;
end;

end.

