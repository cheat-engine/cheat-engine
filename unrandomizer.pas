unit unrandomizer;

interface

uses cefuncproc,dialogs,classes,comctrls,windows,sysutils,formsettingsunit,newkernelhandler;

type Tunrandomize=class(tthread)
private
  processid,processhandle: dword;
  originalcode: array of record
                           address: dword;
                           code: array of byte;
                         end;
  threaddone: boolean;
  procedure save(address:dword; buf: pointer; size: integer);
  procedure done;
public
  progressbar: tprogressbar;
  procedure execute; override;
  procedure restore;
  procedure showaddresses;

  destructor destroy; override;
end;

implementation

uses mainunit;

destructor TUnrandomize.destroy;
begin
  if (processid=cefuncproc.ProcessID) and (processhandle=cefuncproc.ProcessHandle) then restore;
  inherited destroy;
end;

procedure TUnrandomize.done;
begin
  try
    if mainform<>nil then
    begin
      progressbar.Hide;
      progressbar.Free;
      mainform.cbUnrandomizer.Enabled:=true;
    end;
  except

  end;
  threaddone:=true;
end;

procedure TUnrandomize.showaddresses;
var s: string;
    i: integer;
begin
  if threaddone then
  begin
    s:='The following addresses got changed:';
    for i:=0 to length(originalcode)-1 do
      s:=s+#13#10+inttohex(originalcode[i].address,8);

    showmessage(s);
  end;

end;

procedure TUnrandomize.restore;
var i: integer;
begin
  if (processhandle<>cefuncproc.ProcessHandle) and (processid=cefuncproc.ProcessID) then
    processhandle:=cefuncproc.ProcessHandle; //e.g debugger

  //restore the replaced code with the original
  for i:=0 to length(originalcode)-1 do
  begin
    rewritecode(processhandle,originalcode[i].address,originalcode[i].code,length(originalcode[i].code));
    setlength(originalcode[i].code,0);
  end;

  setlength(originalcode,0);
end;

procedure TUnrandomize.save(address:dword; buf: pointer; size: integer);
var i: integer;
begin
  i:=length(originalcode);

  setlength(originalcode,i+1);
  originalcode[i].address:=address;

  setlength(originalcode[i].code,size);
  copymemory(originalcode[i].code,buf,size);

end;

procedure TUnrandomize.execute;
type tcodereplace=array of byte;
var memoryregion: tmemoryregions;
    i,j: integer;
    totalmemory: dword;
    ar,aw: dword;
    buffer: array of byte;
    totalread: dword;

    vcreplace: tcodereplace;
    delphireplace: tcodereplace; //also code2
    msvcrtreplace: tcodereplace;
    ibasicreplace: tcodereplace;

    defaultreturn: integer;
    incremental: boolean;
    counter: pointer;
begin
  defaultreturn:=formsettings.unrandomizersettings.defaultreturn;

  incremental:=formsettings.unrandomizersettings.incremental;


  //scan the memory of the current process
  processhandle:=cefuncproc.ProcessHandle;
  processid:=cefuncproc.ProcessID;

  totalmemory:=0;
  ar:=0;
  getexecutablememoryregionsfromregion(0,$7fffffff,memoryregion);
  for i:=0 to length(memoryregion)-1 do
  begin
    totalmemory:=totalmemory+memoryregion[i].MemorySize;
    if ar<memoryregion[i].MemorySize then ar:=memoryregion[i].MemorySize;
  end;

  progressbar.Max:=totalmemory;
  progressbar.Position:=0;

  setlength(buffer,ar);



  //non inremental
  if not incremental then
  begin
    {
    -------------------------------------------------
                           VC++
    -------------------------------------------------
    }
    setlength(vcreplace,6);

    //mov eax,defaultvalue
    vcreplace[0]:=$b8;
    pdword(@vcreplace[1])^:=defaultreturn;
    //ret
    vcreplace[5]:=$c3;

    {
    -------------------------------------------------
                           DELPHI
    -------------------------------------------------
    }

    setlength(delphireplace,8);
    //mov edx,counter
    delphireplace[0]:=$ba;
    pdword(@delphireplace[1])^:=defaultreturn;

    //and eax,edx
    delphireplace[5]:=$21;
    delphireplace[6]:=$d0;

    //ret
    delphireplace[7]:=$c3;

    {
    -------------------------------------------------
                           IBASIC
    -------------------------------------------------
    }

    setlength(ibasicreplace,8);

    //mov eax,defaultvalue
    ibasicreplace[0]:=$b8;
//    pdword(@ibasicreplace[1])^:=dword(defaultreturnf);
    //ret   8
    ibasicreplace[5]:=$c2;
    ibasicreplace[6]:=$08;
    ibasicreplace[7]:=$00;


    //mov eax,defaultvaluef
    //ret 8

  end
  else
  begin
    //allocate 4 bytes to store the current value
    counter:=virtualallocex(processhandle,nil,8,MEM_COMMIT	,PAGE_EXECUTE_READWRITE);
    writeprocessmemory(processhandle,counter,@defaultreturn,4,aw);


    {
    -------------------------------------------------
                           VC++
    -------------------------------------------------
    }
    setlength(vcreplace,23);
    //mov eax,[counter]
    vcreplace[0]:=$8b;
    vcreplace[1]:=$05;
    pdword(@vcreplace[2])^:=dword(counter);

    //inc eax
    vcreplace[6]:=$40;

    //cmp eax,8000
    vcreplace[7]:=$3d;
    pdword(@vcreplace[8])^:=$8000;

    //jb +2
    vcreplace[12]:=$72;
    vcreplace[13]:=$02;

    //xor eax,eax
    vcreplace[14]:=$31;
    vcreplace[15]:=$c0;

    //mov [counter],eax
    vcreplace[16]:=$89;
    vcreplace[17]:=$05;
    pdword(@vcreplace[18])^:=dword(counter);

    //ret
    vcreplace[22]:=$c3;


    {
    -------------------------------------------------
                           DELPHI
    -------------------------------------------------
    }
    setlength(delphireplace,23);

    //push ebx
    delphireplace[0]:=$53;

    //push edx
    delphireplace[1]:=$52;

    //xchg ebx,eax
    delphireplace[2]:=$93;


    //mov eax,[counter]
    delphireplace[3]:=$8b;
    delphireplace[4]:=$05;
    pdword(@delphireplace[5])^:=dword(counter);


    //xor edx,edx
    delphireplace[9]:=$31;
    delphireplace[10]:=$d2;


    //div ebx
    delphireplace[11]:=$f7;
    delphireplace[12]:=$fb;

    //xchg eax,edx
    delphireplace[13]:=$92;

    //inc [counter]
    delphireplace[14]:=$ff;
    delphireplace[15]:=$05;
    pdword(@delphireplace[16])^:=dword(counter);

    //pop edx
    delphireplace[20]:=$5a;

    //pop ebx
    delphireplace[21]:=$5b;


    //ret
    delphireplace[22]:=$c3;


    {
    -------------------------------------------------
                           IBASIC
    -------------------------------------------------
    }


    setlength(ibasicreplace,52);

    //push ebx
    ibasicreplace[0]:=$53;

    //push edx
    ibasicreplace[1]:=$52;

    //mov ebx,[esp+4]
    ibasicreplace[2]:=$8b;
    ibasicreplace[3]:=$5c;
    ibasicreplace[4]:=$24;
    ibasicreplace[5]:=$04;


    //mov eax,[counter]
    ibasicreplace[6]:=$8b;
    ibasicreplace[7]:=$05;
    pdword(@ibasicreplace[8])^:=dword(counter);


    //xor edx,edx
    ibasicreplace[12]:=$31;
    ibasicreplace[13]:=$d2;


    //div ebx
    ibasicreplace[14]:=$f7;
    ibasicreplace[15]:=$fb;

    //xchg eax,edx
    ibasicreplace[16]:=$92;

    //inc [counter]
    ibasicreplace[17]:=$ff;
    ibasicreplace[18]:=$05;
    pdword(@ibasicreplace[19])^:=dword(counter);

    //mov [counter+4],eax
    ibasicreplace[23]:=$89;
    ibasicreplace[24]:=$05;
    pdword(@ibasicreplace[25])^:=dword(counter)+4;

    //fild [counter+4]
    ibasicreplace[29]:=$db;
    ibasicreplace[30]:=$05;
    pdword(@ibasicreplace[31])^:=dword(counter)+4;

    //fstp [counter+4]
    ibasicreplace[35]:=$d9;
    ibasicreplace[36]:=$1d;
    pdword(@ibasicreplace[37])^:=dword(counter)+4;

    //mov eax,[counter+4]
    ibasicreplace[41]:=$8b;
    ibasicreplace[42]:=$05;
    pdword(@ibasicreplace[43])^:=dword(counter)+4;

    //pop edx
    ibasicreplace[47]:=$5a;

    //pop ebx
    ibasicreplace[48]:=$5b;


    //ret   8
    ibasicreplace[49]:=$c2;
    ibasicreplace[50]:=$08;
    ibasicreplace[51]:=$00;

  end;


  msvcrtreplace:=vcreplace; //can use the same code as the vc code



  for i:=0 to length(memoryregion)-1 do
  begin
    if terminated then break;

    ar:=0;
    if readprocessmemory(processhandle,pointer(memoryregion[i].baseaddress),@buffer[0],memoryregion[i].MemorySize,ar) then
    begin
      for j:=0 to ar-1 do
      begin
        {
        -------------------------------------------------
                               DELPHI
        -------------------------------------------------
        }
        if (j<ar-13) and
           (buffer[j]=$53) and
           (buffer[j+1]=$31) and
           (buffer[j+2]=$db) and
           (buffer[j+3]=$69) and
           (buffer[j+4]=$93) and
           (buffer[j+9]=$05) and
           (buffer[j+10]=$84) and
           (buffer[j+11]=$08) and
           (buffer[j+12]=$08) then
        begin
          //save this code and replace
          save(memoryregion[i].BaseAddress+j,@buffer[j],22);
          rewritecode(processhandle,memoryregion[i].BaseAddress+j,@delphireplace[0],length(delphireplace));
        end;

        {
        -------------------------------------------------
                               VC++
        -------------------------------------------------
        }
        if (j<ar-39) and
           (buffer[j]=$55) and
           (buffer[j+1]=$8b) and
           (buffer[j+2]=$ec) and
           (buffer[j+3]=$a1) and
           (buffer[j+8]=$69) and
           (buffer[j+9]=$c0) and
           (buffer[j+10]=$fd) and
           (buffer[j+11]=$43) and
           (buffer[j+12]=$03) and
           (buffer[j+13]=$00) and
           (buffer[j+14]=$05) and
           (buffer[j+15]=$c3) and
           (buffer[j+16]=$9e) and
           (buffer[j+17]=$26) and
           (buffer[j+18]=$00) and
           (buffer[j+19]=$a3) and
           (buffer[j+24]=$a1) and
           (buffer[j+29]=$c1) and
           (buffer[j+30]=$f8) and
           (buffer[j+31]=$10) and
           (buffer[j+32]=$25) and
           (buffer[j+33]=$ff) and
           (buffer[j+34]=$7f) and
           (buffer[j+35]=$00) and
           (buffer[j+36]=$00) and
           (buffer[j+37]=$5d) and
           (buffer[j+38]=$c3) then
        begin
          //save this code and replace
          save(memoryregion[i].BaseAddress+j,@buffer[j],26);
          rewritecode(processhandle,memoryregion[i].BaseAddress+j,@vcreplace[0],length(vcreplace)); //size is always 12
        end;

        {
        -------------------------------------------------
                               msvcrt.rand
        -------------------------------------------------
        }
        if (j<ar-34) and
           (buffer[j]=$e8) and
           (buffer[j+5]=$8b) and
           (buffer[j+6]=$48) and
           (buffer[j+8]=$69) and
           (buffer[j+9]=$c9) and
           (buffer[j+10]=$fd) and
           (buffer[j+11]=$43) and
           (buffer[j+12]=$03) and
           (buffer[j+13]=$00) and
           (buffer[j+14]=$81) and
           (buffer[j+15]=$c1) and
           (buffer[j+16]=$c3) and
           (buffer[j+17]=$9e) and
           (buffer[j+18]=$26) and
           (buffer[j+19]=$00) and
           (buffer[j+20]=$89) and
           (buffer[j+21]=$48) and
           (buffer[j+23]=$8b) and
           (buffer[j+24]=$c1) and
           (buffer[j+25]=$c1) and
           (buffer[j+26]=$e8) and
           (buffer[j+27]=$10) and
           (buffer[j+28]=$25) and
           (buffer[j+29]=$ff) and
           (buffer[j+30]=$7f) and
           (buffer[j+31]=$00) and
           (buffer[j+32]=$00) and
           (buffer[j+33]=$c3) then
        begin
          //save this code and replace
          save(memoryregion[i].BaseAddress+j,@buffer[j],length(msvcrtreplace));
          rewritecode(processhandle,memoryregion[i].BaseAddress+j,@msvcrtreplace[0],length(msvcrtreplace)); //size is always 12
        end;


        {
        -------------------------------------------------
                            ibasic random
        -------------------------------------------------
        }
        if (j<ar-112) and
           (buffer[j]=$55) and
           (buffer[j+1]=$89) and
           (buffer[j+2]=$e5) and
           (buffer[j+3]=$81) and
           (buffer[j+4]=$ec) and
           (buffer[j+5]=$04) and
           (buffer[j+6]=$00) and
           (buffer[j+7]=$00) and
           (buffer[j+8]=$00) and
           (buffer[j+9]=$53) and
           (buffer[j+10]=$57) and
           (buffer[j+11]=$56) and
           (buffer[j+12]=$31) and
           (buffer[j+13]=$c0) and
           (buffer[j+14]=$bb) and

           (buffer[j+93]=$00) and
           (buffer[j+94]=$00) and
           (buffer[j+95]=$00) and
           (buffer[j+96]=$89) and
           (buffer[j+97]=$c2) and
           (buffer[j+98]=$5e) and
           (buffer[j+99]=$89) and
           (buffer[j+100]=$16) and
           (buffer[j+101]=$b8) and

           (buffer[j+106]=$50) and
           (buffer[j+107]=$b8) and
           (buffer[j+108]=$c3) and
           (buffer[j+109]=$9e) and
           (buffer[j+110]=$26) and
           (buffer[j+111]=$00) and
           (buffer[j+112]=$bb) and
           (buffer[j+113]=$fd) and
           (buffer[j+114]=$43) and
           (buffer[j+115]=$03) and
           (buffer[j+116]=$00) and
           (buffer[j+117]=$b9) then
        begin
          //save this code and replace
          save(memoryregion[i].BaseAddress+j,@buffer[j],length(ibasicreplace));
          rewritecode(processhandle,memoryregion[i].BaseAddress+j,@ibasicreplace[0],length(ibasicreplace));
        end;


      end;

    end;

    try
      progressbar.Position:=totalread;
    except
      terminate;
    end;

  end;

  if terminated then synchronize(restore);
  synchronize(done);
end;


end.
