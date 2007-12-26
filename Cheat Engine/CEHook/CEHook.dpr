library CEHook;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  SysUtils,
  Classes,
  windows,
  Dialogs,
  system,
  math,
  Messages,
  graphics,
  speedhack in 'speedhack.pas',
  globals in 'globals.pas';

{$R *.res}

const donescanning=wm_user+1;   //wparam=numberfound
      ESScanningBool=wm_user+2; //wparam=address of scanning boolean.  (should be the same in ce, but I like to verify it)
      ESSettings=wm_user+3;
      ESSetProgressbarMax=wm_user+4; //wparam=max
      ESSetProgressbarPos=wm_user+5; //wparam=currentpos
      HSThreadID=wm_user+6; //wparam=threadid

const
  Exact_value = 0;
  Increased_value = 1;
  Increased_value_by = 2;
  Decreased_value = 3;
  Decreased_value_by = 4;
  Changed_value = 5;
  Unchanged_value = 6;
  Advanced_Scan = 7;
  String_Scan = 8;
  SmallerThan = 9;
  BiggerThan = 10;
  Userdefined = 11;
  valueBetween = 12;



Type TMemoryRegion = record
  BaseAddress: Dword;
  MemorySize: Dword;
  IsChild: boolean;
  end;

Type TBytes = array of integer;
type bitaddress = record
  address: dword;
  bit: dword;
end;

type TScanThread = class(TThread)
  private
  public
    x: integer;
    procedure Execute; override;
  end;

type THyperscanWindow = class(TThread)
  private
  public
    x: integer;
    procedure Execute; override;
  end;



//globals
var scanning: boolean;
    CEScanWindow: THandle;
    x: string;

    bytes: array of integer;  //-1=wildcard
    bytearray: array of byte;


    nrofbits: integer;
    Bitscan: array of byte;
    tempbits: array of byte;
    bitoffsetchange: integer;

    TotalToRead:dword;
    buffersize: Dword;
    AddressFile: File;
    MemoryFile: File;
    NewAddressFile: File;
    NewMemoryFile: File;

    MemoryRegion: array of TMemoryRegion;
    MemoryRegions: Integer;
    memory: ^byte;

    FoundValue1:Array of Byte;
    FoundValue2:Array of word;
    FoundValue3:Array of Dword;
    FoundValue4:Array of Single;
    FoundValue5:Array of Double;
    foundValue6:Array of int64;  //byte ?????
    FoundValue8:array of byte;

    previousvalue1: array of Byte;
    previousvalue2: array of word;
    previousvalue3: array of dword;
    previousvalue4: array of Single;
    previousvalue5: array of Double;
    previousvalue6: array of int64; //Byte;

    FoundAddress,scanaddress: array of dword;
    foundaddressB,scanaddressb: array of bitaddress;

    lastscan:byte;

    
    intercepttimermessages:boolean;


    //speedhack

    timeGetTimePos: pointer;
    GetTickCountPos: pointer;
    QueryPerformanceCounterpos: pointer;

    winmmlib: thandle;
    kernel32lib:thandle;
    ST: TScanThread;
    HTW: THyperscanwindow;
    hyperscanwindowenabled: boolean;

    

procedure CopyMemory(Destination: Pointer; Source: Pointer; Length: DWORD); stdcall; external 'kernel32.dll' name 'RtlMoveMemory';

procedure freememory;
begin
  now;
  setlength(bitscan,0);
  setlength(FoundValue1,0);
  setlength(FoundValue2,0);
  setlength(FoundValue3,0);
  setlength(FoundValue4,0);
  setlength(FoundValue5,0);
  setlength(foundValue6,0);

  setlength(FoundAddress,0);
  setlength(foundaddressB,0);

  setlength(previousvalue1,0);
  setlength(previousvalue2,0);
  setlength(previousvalue3,0);
  setlength(previousvalue4,0);
  setlength(previousvalue5,0);
  setlength(previousvalue6,0);
  setlength(scanaddress,0);
  setlength(scanaddressb,0);
end;

procedure deletefiles;
begin
  deletefile(pchar(scansettings^.CheatEngineDir+'Addresses.TMP'));
  deletefile(pchar(scansettings^.CheatEngineDir+'Memory.TMP'));
  deletefile(pchar(scansettings^.CheatEngineDir+'Addresses2.TMP'));
  deletefile(pchar(scansettings^.CheatEngineDir+'Memory2.TMP'));
end;

procedure closefiles;
begin
  try
    closefile(addressfile);
    closefile(memoryfile);
    closefile(newAddressfile);
    closefile(newmemoryfile);
  except

  end;
end;

procedure ConvertStringToBytes(scanvalue:string; hex:boolean;var bytes: TBytes);
var i,j,k: integer;
    helpstr:string;
begin
  while scanvalue[length(scanvalue)]=' ' do
    scanvalue:=copy(scanvalue,1,length(scanvalue)-1);

  if (pos('-',scanvalue)>0) or (pos(' ',scanvalue)>0) then
  begin
    //syntax is xx-xx-xx or xx xx xx
    j:=1;
    k:=0;
    scanvalue:=scanvalue+' ';

    for i:=1 to length(scanvalue) do
    begin
      if (scanvalue[i]=' ') or (scanvalue[i]='-') then
      begin
        helpstr:=copy(scanvalue,j,i-j);
        j:=i+1;
        setlength(bytes,k+1);
        try
          if hex then bytes[k]:=strtoint('$'+helpstr)
                 else bytes[k]:=strtoint(helpstr);
        except
          bytes[k]:=-1;
          //if it is not a '-' or ' ' or a valid value then I assume it is a wildcard.(
        end;
        inc(k);
      end;
    end;
  end else
  begin
    //syntax is xxxxxx
    k:=0;
    j:=1;
    for i:=1 to length(scanvalue) do
    begin
      if (i mod 2)=0 then
      begin
        helpstr:=copy(scanvalue,j,i-j+1);
        j:=i+1;
        setlength(bytes,k+1);
        try
          bytes[k]:=strtoint('$'+helpstr);
        except
          bytes[k]:=-1;
        end;
        inc(k);
      end;
    end;
  end;
end;

function getbit(bitnr: integer; bt: Byte):integer;
begin
  if (trunc(power(2,bitnr)) and bt)>0 then result:=1 else result:=0;
end;

function scanbits(var found: dword;number:dword;var bytep: pbyte;nrofbits,i,actualread: integer): boolean;
var j,k,l,m: integer;
    actualwrite: dword;
    tempcount: integer;
    tempj,tempk: integer;
    tempb: pbyte;
begin
  for j:=0 to actualread-1 do
  begin

    //scan each bit till you find  bitarray[bittofind]
    for k:=0 to 7 do
    begin
      tempb:=bytep;
      //see if there are enough bits to scan, if not, save the bits to be scanned and exit
      //bitsleft=((actualread-j)*8) - k
      if nrofbits>(((actualread-j)*8) - k) then
      begin
        tempcount:=(((actualread-j)*8) - k);     //should always be nrofbits-1
        setlength(tempbits,nrofbits);

        tempk:=k;
        for l:=1 to tempcount do
        begin
          tempbits[l]:=getbit(tempk,bytep^);
          inc(tempk);
          if tempk>7 then
          begin
            inc(bytep);
            tempk:=0;
          end;
        end;
        result:=true;
        exit;
      end;

      m:=k;
      for l:=0 to nrofbits-1 do
      begin
        if bitscan[l]<>2 then
          if getbit(m,tempb^)<>bitscan[l] then break;

        if l=nrofbits-1 then
        begin
          //foundit
          foundaddressb[found].address:=memoryregion[i].BaseAddress+j;
          foundaddressb[found].bit:=k;
          inc(found);
          if found=number then
          begin
            blockwrite(Addressfile,pointer(foundaddressB)^,found*(sizeof(bitaddress)),actualwrite);
            found:=0;
          end;
        end;

        inc(m);
        if m>7 then
        begin
          m:=0;
          inc(tempb);
        end;
      end;

    end;
    inc(bytep);
  end;


  result:=true;
end;

function RemoveRangewith(address: dword):boolean;
var i,j: integer;
begin
  result:=false;
  for i:=0 to memoryregions-1 do //still -1 here
  begin
    if (address>=(memoryregion[i].BaseAddress)) and
       (address<(memoryregion[i].BaseAddress+memoryregion[i].MemorySize)) then
    begin
      //remove it
      for j:=i to memoryregions-2 do
        memoryregion[j]:=memoryregion[j+1];

      dec(memoryregions);
      setlength(memoryregion,length(memoryregion)-1);
      result:=true;
      exit;
    end;
  end;
end;

procedure GetMemoryRanges(const exclude: array of dword);
var address: Dword;
    mbi : _MEMORY_BASIC_INFORMATION;
    i: Integer;
    j: dword;
    size:       dword;
    skip: boolean;

    dlladdr: dword;
    temp: dword;
    label x;
begin
  buffersize:=scansettings^.buffersize;
  address:=scansettings^.StartAddress;
  memoryregions:=0;
  setlength(memoryregion,0);

  try
    outputdebugstring('GetMemoryRanges');

    while (Virtualquery(pointer(address),mbi,sizeof(mbi))<>0) and (address<scansettings^.stopaddress) do
    begin
      skip:=false;
      for i:=0 to length(exclude)-1 do
      begin
        if (exclude[i]>=dword(mbi.BaseAddress)) and
           (exclude[i]<(dword(mbi.BaseAddress)+mbi.RegionSize)) then
        begin
          skip:=true;
          break;
        end;
      end;
      if skip then
      begin
        address:=dword(mbi.baseaddress)+mbi.RegionSize;
        continue;
      end;

      if (not (not scansettings^.scan_mem_private and (mbi.type_9=mem_private))) and (not (not scansettings^.scan_mem_image and (mbi.type_9=mem_image))) and (not (not scansettings^.scan_mem_mapped and (mbi.type_9=mem_mapped))) and (mbi.State=mem_commit) and ((mbi.Protect and page_guard)=0) and ((mbi.protect and page_noaccess)=0) then  //look if it is commited
      begin
        if not scansettings^.readonly then  //if the settings are set to not read read-only memory then
        begin
          if (((mbi.AllocationProtect) and (page_readonly or page_execute_read))=0) and
             (((mbi.Protect) and (page_readonly or PAGE_EXECUTE_READ))=0) then //look if the memory is not read-only  , if 0 it means it's not read-only
          begin
            if scansettings^.Skip_PAGE_NOCACHE then
              if (mbi.AllocationProtect and PAGE_NOCACHE)=PAGE_NOCACHE then
              begin
                address:=dword(mbi.BaseAddress)+mbi.RegionSize;
                continue;
              end;

            setlength(memoryregion,memoryregions+1);

            memoryregion[memoryregions].BaseAddress:=dword(mbi.baseaddress);  //remember if it's not read only
            memoryregion[memoryregions].MemorySize:=mbi.RegionSize;
            inc(memoryregions);
          end;
        end else  //if the settings are to also read the read only then:
        begin
          if scansettings^.Skip_PAGE_NOCACHE then
            if (mbi.AllocationProtect and PAGE_NOCACHE)=PAGE_NOCACHE then
            begin
              address:=dword(mbi.BaseAddress)+mbi.RegionSize;
              continue;
            end;

          setlength(memoryregion,memoryregions+1);

          memoryregion[memoryregions].BaseAddress:=dword(mbi.baseaddress);  //just remember this location
          memoryregion[memoryregions].MemorySize:=mbi.RegionSize;
          inc(memoryregions);
        end;
      end;


      address:=dword(mbi.baseaddress)+mbi.RegionSize;
    end;

    if memoryregions=0 then raise exception.Create('No memory found');

    if (memoryregion[0].BaseAddress<scansettings^.StartAddress) and (memoryregion[0].MemorySize-(scansettings^.StartAddress-memoryregion[0].BaseAddress)>0) then
    begin
      memoryregion[0].MemorySize:=memoryregion[0].MemorySize-(scansettings^.StartAddress-memoryregion[0].BaseAddress);
      memoryregion[0].BaseAddress:=scansettings^.StartAddress;
    end;

    if (memoryregion[memoryregions-1].BaseAddress+memoryregion[memoryregions-1].MemorySize)>scansettings^.StopAddress then
      dec(memoryregion[memoryregions-1].MemorySize,(memoryregion[memoryregions-1].BaseAddress+memoryregion[memoryregions-1].MemorySize)-scansettings^.StopAddress-1);


    RemoveRangewith(dword(@scansettings));
    RemoveRangeWith(dword(@scansettings.scanning));



    if st<>nil then
      RemoverangeWith(dword(@st.x));

    if htw<>nil then
      RemoverangeWith(dword(@htw.x));

    RemoverangeWith(dword(@Removerangewith));

    //if everything went ok memoryregions should now contain all the addresses and sizes
    //to speed it up combine the regions that are attached to eachother.

    j:=0;
    address:=memoryregion[0].BaseAddress;
    size:=memoryregion[0].MemorySize;

    for i:=1 to memoryregions-1 do
    begin
      if memoryregion[i].BaseAddress=address+size then
        inc(size,memoryregion[i].MemorySize)
      else
      begin
        memoryregion[j].BaseAddress:=address;
        memoryregion[j].MemorySize:=size;

        address:=memoryregion[i].BaseAddress;
        size:=memoryregion[i].MemorySize;
        inc(j);
      end;
    end;

    memoryregion[j].BaseAddress:=address;
    memoryregion[j].MemorySize:=size;

    memoryregions:=j+1;
    setlength(memoryregion,memoryregions);

    //re-added due to complaints about speed:
    //split up into smaller chunks
    if scansettings^.buffersize>0 then
    begin
      i:=0;
      while i<=memoryregions-1 do
      begin
        if memoryregion[i].MemorySize>dword(buffersize) then
        begin
          inc(memoryregions);
          setlength(memoryregion,memoryregions);

          //copy the next element to the back, and split up the current one
          //(unless this is the item at the back, and not needed)
          if i<memoryregions-2 then
          begin
            //i isnt the last element of the array so do a semi-shift
            memoryregion[memoryregions-1].BaseAddress:=memoryregion[i+1].baseaddress;
            memoryregion[memoryregions-1].MemorySize:=memoryregion[i+1].MemorySize;
          end;

          memoryregion[i+1].IsChild:=true;
          if scansettings^.fastscan then
            case scansettings^.ValueType of
              1:
              begin  //word
                memoryregion[i+1].BaseAddress:=memoryregion[i].BaseAddress+buffersize-1;
                memoryregion[i+1].MemorySize:=memoryregion[i].MemorySize-buffersize+1;
              end;

              2,3:
              begin  //dword+float
                memoryregion[i+1].BaseAddress:=memoryregion[i].BaseAddress+buffersize-3;
                memoryregion[i+1].MemorySize:=memoryregion[i].MemorySize-buffersize+3;
              end;

              4,6:
              begin  //double+int64
                memoryregion[i+1].BaseAddress:=memoryregion[i].BaseAddress+buffersize-7;
                memoryregion[i+1].MemorySize:=memoryregion[i].MemorySize-buffersize+7;
              end;
              else
              begin
                memoryregion[i+1].BaseAddress:=memoryregion[i].BaseAddress+buffersize;
                memoryregion[i+1].MemorySize:=memoryregion[i].MemorySize-buffersize;
              end;
            end
          else
          begin
            memoryregion[i+1].BaseAddress:=memoryregion[i].BaseAddress+buffersize;
            memoryregion[i+1].MemorySize:=memoryregion[i].MemorySize-buffersize;
          end;

          memoryregion[i].MemorySize:=buffersize;
        end;
        inc(i);
      end;
      dec(memoryregions);
    end else memoryregions:=j;

    if scansettings^.fastscan then
    begin
      //optimize regions
      for i:=0 to memoryregions do
      //make it so the regions all start at a location that can be devided by the size of the valtype
        case scansettings^.ValueType of
          1:
          begin  //word (mod 2=0)
            j:=(memoryregion[i].BaseAddress mod 2);
            if j<>0 then
              if memoryregion[i].IsChild then
              begin
                memoryregion[i].BaseAddress:=memoryregion[i].BaseAddress-j;
                inc(memoryregion[i].MemorySize,j);
              end
              else
              begin
                memoryregion[i].BaseAddress:=memoryregion[i].BaseAddress+j;
                dec(memoryregion[i].MemorySize,j);
              end;
          end;

          2,3:
          begin  //dword+float
            j:=(memoryregion[i].BaseAddress mod 4);
            if j<>0 then
              if memoryregion[i].IsChild then
              begin
                memoryregion[i].BaseAddress:=memoryregion[i].BaseAddress-j;
                inc(memoryregion[i].MemorySize,j);
              end
              else
              begin
                memoryregion[i].BaseAddress:=memoryregion[i].BaseAddress+4-j;
                dec(memoryregion[i].MemorySize,4-j);
              end;
          end;

          4,6:
          begin //double+int64
            j:=(memoryregion[i].BaseAddress mod 8);
            if j<>0 then
              if memoryregion[i].IsChild then
              begin
                memoryregion[i].BaseAddress:=memoryregion[i].BaseAddress-j;
                inc(memoryregion[i].MemorySize,j);
              end
              else
              begin
                memoryregion[i].BaseAddress:=memoryregion[i].BaseAddress+8-j;
                dec(memoryregion[i].MemorySize,8-j);
              end;
          end;
        end;
    end;

    TotalToRead:=0;
    For i:=0 to Memoryregions do
      inc(TotalToRead,Memoryregion[i].MemorySize);
    postmessage(scansettings^.formscanningHandle,ESSetProgressbarMax,TotalToRead,TotalToRead);
  except
    on e: exception do
    begin
      outputdebugstring(pchar(e.Message));
      raise e;
    end;

  end;

//  for i:=0 to length(exclude)-1 do   RemoveRangewith(exclude[i]);

  //  showmessage('TotalToRead='+IntToStr(totaltoread));
end;

{procedure foundone;
begin
  //inc(form1.c);
  //showmessage(IntToHex(foundaddress,8));
end;

procedure ScanByte(scanvalue:byte;startaddress:dword;ammount: dword);
label done;
asm
  mov edx,startaddress
  mov ecx,ammount
  mov ah,scanvalue

@looper:

  cmp byte ptr [edx],ah
  je @_foundone
@continue_looper:
  inc edx
  loop @looper
  jmp done

@_foundone:
  push eax
  push ecx
  push edx
  mov foundaddress,edx
  call foundone
  pop edx
  pop ecx
  pop eax
  jmp @continue_looper

done:
end;}

function ExactValueFirstScan:integer;
var mbi : _MEMORY_BASIC_INFORMATION;
    address: Dword;
    size:       dword;

    found: dword;

    Decim:       dword;
    Decimhelp:   Integer;

    ByteValue,ByteValue2: Byte;
    WordValue,WordValue2: Word;
    DWordValue,dwordvalue2: Dword;
    SingleValue,singlevalue2: Single;
    doubleValue,doublevalue2: double;
    Int64Value,int64value2: Int64;
    helpsingle,helpsingle2,helpsingle3: single;
    helpdouble,helpdouble2,helpdouble3: double;

    i,k,l: integer;
    j: dword;

    actualread: dword;
    actualwrite: dword;


    //save variables
    dataType:  String[6];  //REGION or NORMAL  (Always region in this procedure)


    CharToFind: Dword;
    memoryposition: Dword;
    position: Dword;
    searching: Boolean;

    scanlength: Dword;

    check:    boolean;

    MPoint: ^Byte;

    ByteP: pbyte;
    bytep2: pbyte;
    WordP: ^Word;
    DwordP: ^Dword;
    Int64P: ^Int64;

    SingleP: ^Single;
    DoubleP: ^double;

    helpword: word;
    helpdworD: dword;


    number: dword;
    maxmem: dword;

    FoundIsFilled: Boolean;

    //arrayofbyte vars
    nrofbytes: dword;
    bytes: TBytes;  //-1=wildcard

    BitToFind: integer;

    traceback: boolean;

    resulthelper: tfilestream;
    tempstring: pchar;

    scanvalue:string;
    scantype: Integer;
    vartype: integer;
    extra: boolean;
    roundingtype: tfloatscan;
    unicode: boolean;
    unicode2: boolean;

    scanvalue2: string;
begin
  lastscan:=0;
  bytevalue:=$90;
  wordvalue:=$1020;
  dwordvalue:=$30405060;
  singlevalue:=123.12345;
  doublevalue:=12345.123;
  int64value:=$1122334455667788;

  try
    GetMemoryRanges([dword(@dwordvalue)]);
   // RemoveRangewith(dword(@dwordvalue));

  except
     raise exception.Create('getmemoryexception');
  end;
  position:=0;
  nrofbits:=999;

  Int64Value:=0;
  FoundIsFilled:=false;

 // setlength(memory,splitvalue+1);
  found:=0;
  scanvalue:=scansettings^.scanvalue;
  scanvalue2:=scansettings^.scanvalue2;
  scantype:=scansettings^.Scantype;
  vartype:=scansettings^.ValueType;
  extra:=scansettings^.Hexadecimal;
  roundingtype:=scansettings^.roundingtype;
  unicode:=scansettings^.unicode;

  if scanvalue='' then scanvalue:='0';
  if scanvalue2='' then scanvalue2:='0';

  if scantype in [Exact_value,Increased_value_by,decreased_value_by,SmallerThan,BiggerThan,valuebetween] then
  begin
    if vartype=0 then
    begin
      if extra then val('$'+scanvalue,bytevalue,i)
               else val(scanvalue,bytevalue,i);
      if i>0 then raise Exception.Create(scanvalue+' is not a valid byte notation');
    end;

    if vartype=1 then
    begin
      if extra then val('$'+scanvalue,wordvalue,i)
               else val(scanvalue,wordvalue,i);
      if i>0 then raise Exception.Create(scanvalue+' is not an valid 2-byte notation');
    end;

    if vartype=2 then
    begin
      if extra then val('$'+scanvalue,dwordvalue,i)
               else val(scanvalue,dwordvalue,i);
      if i>0 then raise Exception.Create(scanvalue+' is not an valid 4-byte notation');
    end;

    if vartype=3 then //doesnt have the hexadecimal option
    begin
      val(scanvalue,singlevalue,i);
      if i>0 then
        if scanvalue[i]=',' then scanvalue[i]:='.';

      val(scanvalue,singlevalue,i);
      if i>0 then raise Exception.Create(scanvalue+' is not a valid floating point notation');
    end;

    if vartype=4 then //same as 3
    begin
      val(scanvalue,doublevalue,i);
      if i>0 then
        if scanvalue[i]=',' then scanvalue[i]:='.';
      val(scanvalue,singlevalue,i);

      if i>0 then raise Exception.create(scanvalue+' is not a valid floating point notation');
    end;

    if vartype=5 then
    begin
      if pos('-',scanvalue)>0 then raise exception.create(scanvalue+' is not a valid notation!');
      if not extra then
      begin
        setlength(bitscan,length(scanvalue));
        j:=0;
        for i:=length(scanvalue) downto 1 do
        begin
          if scanvalue[i]='0' then bitscan[length(scanvalue)-i]:=0
          else
          if scanvalue[i]='1' then bitscan[length(scanvalue)-i]:=1
          else
          if scanvalue[i]='?' then bitscan[length(scanvalue)-i]:=2
          else
          if scanvalue[i]='*' then bitscan[length(scanvalue)-i]:=2
          else
          begin
            freememory;
            raise Exception.create(scanvalue+' is not a valid binary notation');
          end;
          inc(j);
        end;
      end
      else
      begin
        try
          dwordvalue:=StrToInt(scanvalue);
          i:=0;

          if dwordvalue=0 then
          begin
            setlength(bitscan,1);
            bitscan[0]:=0;
          end
          else
          while dwordvalue>0 do
          begin
            setlength(bitscan,i+1);

            if (dwordvalue mod 2)>0 then bitscan[i]:=1
                                    else bitscan[i]:=0;

            inc(i);
            dwordvalue:=dwordvalue div 2;
          end;
        except
          raise Exception.create(scanvalue+' is not a valid notation');
        end;
      end;

      nrofbits:=length(bitscan);
      scansettings^.nrofbits:=nrofbits;
      if nrofbits=0 then raise exception.Create(scanvalue+' did not result in a string of bits');
    end;

    if vartype=6 then
    begin
      if extra then
        val('$'+scanvalue,int64value,i)
      else
        val(scanvalue,Int64value,i);
      if i>0 then raise Exception.Create(scanvalue+' is not an valid 8-byte notation');
    end;
  end;

  if scantype=valueBetween then
  begin
    if vartype=0 then
    begin
      if extra then val('$'+scanvalue2,bytevalue2,i)
               else val(scanvalue2,bytevalue2,i);
      if i>0 then raise Exception.Create(scanvalue2+' is not a valid byte notation');
    end;

    if vartype=1 then
    begin
      if extra then val('$'+scanvalue2,wordvalue2,i)
               else val(scanvalue2,wordvalue2,i);
      if i>0 then raise Exception.Create(scanvalue2+' is not an valid 2-byte notation');
    end;

    if vartype=2 then
    begin
      if extra then val('$'+scanvalue2,dwordvalue2,i)
               else val(scanvalue2,dwordvalue2,i);
      if i>0 then raise Exception.Create(scanvalue2+' is not an valid 4-byte notation');
    end;

    if vartype=3 then //doesnt have the hexadecimal option
    begin
      val(scanvalue2,singlevalue2,i);
      if i>0 then
        if scanvalue2[i]=',' then scanvalue2[i]:='.';

      val(scanvalue2,singlevalue2,i);
      if i>0 then raise Exception.Create(scanvalue2+' is not a valid floating point notation');
    end;

    if vartype=4 then //same as 3
    begin
      val(scanvalue2,doublevalue2,i);
      if i>0 then
        if scanvalue2[i]=',' then scanvalue2[i]:='.';
      val(scanvalue2,singlevalue2,i);

      if i>0 then raise Exception.create(scanvalue2+' is not a valid floating point notation');
    end;


    if vartype=6 then
    begin
      if extra then
        val('$'+scanvalue2,int64value2,i)
      else
        val(scanvalue2,Int64value2,i);
      if i>0 then raise Exception.Create(scanvalue2+' is not an valid 8-byte notation');
    end;
  end;


  helpword:=wordvalue xor 1+random(65534);
  helpdword:=dwordvalue xor helpword;

  //find the digits
  decim:=0;

  val(scanvalue,decimhelp,i);

  //i:=pos('.',scanvalue);
  if i>0 then decim:=length(scanvalue)-i;

  datatype:='NORMAL';

  assignfile(memoryfile,scansettings^.CheatEngineDir+'Memory.TMP');
  rewrite(memoryfile,1);

  assignfile(Addressfile,scansettings^.CheatEngineDir+'Addresses.TMP');
  rewrite(Addressfile,1);
  blockwrite(Addressfile,datatype,sizeof(datatype));

  number:=buffersize;
  setlength(foundaddress,number);

  if vartype=0 then //byte
  begin
    //no need to bother with fastscan routines
    setlength(foundvalue1,number);
    if scantype=exact_value then
    begin
      for i:=0 to memoryregions do
      begin
        if not scansettings^.scanning then break;

        inc(position,memoryregion[i].MemorySize);
        bytep:=pointer(memoryregion[i].BaseAddress);

        try
//          ScanByte(bytevalue,memoryregion[i].BaseAddress,memoryregion[i].MemorySize);
          for j:=0 to memoryregion[i].MemorySize-1 do
          begin
            if bytep^=bytevalue then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+j;
              inc(found);

              if found=number then
              begin
                if not FoundIsFilled then
                begin
                  for k:=0 to number-1 do foundvalue1[k]:=bytevalue;
                  FoundIsFilled:=true;
                end;

                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                blockwrite(Memoryfile,pointer(foundvalue1)^,number,actualwrite);
                found:=0;
              end;
            end;
            inc(bytep);

          end;
        except

        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;

      if not foundisfilled then
        for i:=0 to found-1 do foundvalue1[i]:=bytevalue;

      blockwrite(Addressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(Memoryfile,pointer(foundvalue1)^,found,actualwrite);
      result:=filesize(Memoryfile);
    end;

    if scantype=BiggerThan then
    begin
      for i:=0 to memoryregions do
      begin
        if not scansettings^.scanning then break;

        inc(position,memoryregion[i].MemorySize);
        bytep:=pointer(memoryregion[i].BaseAddress);

        try
          for j:=0 to memoryregion[i].MemorySize-1 do
          begin
            if bytep^>bytevalue then
            begin
              foundaddress[found]:=Memoryregion[i].BaseAddress+j;
              foundvalue1[found]:=bytep^;
              inc(found);

              if found=number then
              begin
                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                blockwrite(Memoryfile,pointer(foundvalue1)^,number,actualwrite);
                found:=0;
              end;
            end;
            inc(bytep);
          end;
        except

        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;
      blockwrite(Addressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(Memoryfile,pointer(foundvalue1)^,found,actualwrite);
      result:=filesize(Memoryfile);
    end;

    if scantype=SmallerThan then
    begin
      for i:=0 to memoryregions do
      begin
        if not scansettings^.scanning then break;

        inc(position,memoryregion[i].MemorySize);
        bytep:=pointer(memoryregion[i].BaseAddress);

        try
          for j:=0 to memoryregion[i].MemorySize-1 do
          begin
            if bytep^<bytevalue then
            begin
              foundaddress[found]:=dword(bytep);
              foundvalue1[found]:=bytep^;
              inc(found);

              if found=number then
              begin
                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                blockwrite(Memoryfile,pointer(foundvalue1)^,number,actualwrite);
                found:=0;
              end;
            end;
            inc(bytep);
          end;
        except

        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;
      blockwrite(Addressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(Memoryfile,pointer(foundvalue1)^,found,actualwrite);
      result:=filesize(Memoryfile);
    end;

    if scantype=ValueBetween then
    begin
      for i:=0 to memoryregions do
      begin
        if not scansettings^.scanning then break;

        inc(position,memoryregion[i].MemorySize);
        bytep:=pointer(memoryregion[i].BaseAddress);

        try
          for j:=0 to memoryregion[i].MemorySize-1 do
          begin
            if (bytep^>=bytevalue) and (bytep^<=bytevalue2) then
            begin
              foundaddress[found]:=dword(bytep);
              foundvalue1[found]:=bytep^;
              inc(found);

              if found=number then
              begin
                blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                blockwrite(Memoryfile,pointer(foundvalue1)^,number,actualwrite);
                found:=0;
              end;
            end;
            inc(bytep);
          end;
        except

        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;
      blockwrite(Addressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(Memoryfile,pointer(foundvalue1)^,found,actualwrite);
      result:=filesize(Memoryfile);
    end;

  end
  else
  if vartype=1 then //word
  begin
    //no need to bother with fastscan routines
    setlength(foundvalue2,number);
    if scantype=exact_value then
    begin
      for i:=0 to memoryregions do
      begin
        if not scansettings^.scanning then break;

        inc(position,memoryregion[i].MemorySize);
        wordp:=pointer(memoryregion[i].BaseAddress);

        try
          if scansettings^.FastScan then
          begin
            for j:=0 to (memoryregion[i].MemorySize div 2)-1 do
            begin
              if wordp^=wordvalue then
              begin
                foundaddress[found]:=dword(wordp);
                inc(found);

                if found=number then
                begin
                  if not FoundIsFilled then
                  begin
                    for k:=0 to number-1 do foundvalue2[k]:=wordvalue;
                    FoundIsFilled:=true;
                  end;

                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                  found:=0;
                end;
              end;
              inc(wordp);
            end;
          end
          else
          begin
            for j:=0 to memoryregion[i].MemorySize-2 do
            begin
              if wordp^=wordvalue then
              begin
                foundaddress[found]:=dword(wordp);
                inc(found);

                if found=number then
                begin
                  if not FoundIsFilled then
                  begin
                    for k:=0 to number-1 do foundvalue2[k]:=wordvalue;
                    FoundIsFilled:=true;
                  end;

                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc wordp;
              end;
            end;
          end;
        except

        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;

      if not foundisfilled then
        for i:=0 to found-1 do foundvalue2[i]:=wordvalue;
        
      blockwrite(Addressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(Memoryfile,pointer(foundvalue2)^,found*2,actualwrite);
      result:=filesize(Memoryfile) div 2;
    end;

    if scantype=BiggerThan then
    begin
      for i:=0 to memoryregions do
      begin
        if not scansettings^.scanning then break;

        inc(position,memoryregion[i].MemorySize);
        wordp:=pointer(memoryregion[i].BaseAddress);

        try
          if scansettings^.FastScan then
          begin
            for j:=0 to (memoryregion[i].MemorySize div 2)-1 do
            begin
              if wordp^>wordvalue then
              begin
                foundaddress[found]:=dword(wordp);
                foundvalue2[found]:=wordp^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                  found:=0;
                end;
              end;
              inc(wordp);
            end;
          end
          else
          begin
            for j:=0 to memoryregion[i].MemorySize-2 do
            begin
              if wordp^>wordvalue then
              begin
                foundaddress[found]:=dword(wordp);
                foundvalue2[found]:=wordp^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc wordp;
              end;
            end;
          end;
        except

        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;

      blockwrite(Addressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(Memoryfile,pointer(foundvalue2)^,found*2,actualwrite);
      result:=filesize(Memoryfile) div 2;
    end;

    if scantype=SmallerThan then
    begin
      for i:=0 to memoryregions do
      begin
        if not scansettings^.scanning then break;

        inc(position,memoryregion[i].MemorySize);
        wordp:=pointer(memoryregion[i].BaseAddress);

        try
          if scansettings^.FastScan then
          begin
            for j:=0 to (memoryregion[i].MemorySize div 2)-1 do
            begin
              if wordp^<wordvalue then
              begin
                foundaddress[found]:=dword(wordp);
                foundvalue2[found]:=wordp^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                  found:=0;
                end;
              end;
              inc(wordp);
            end;
          end
          else
          begin
            for j:=0 to memoryregion[i].MemorySize-2 do
            begin
              if wordp^<wordvalue then
              begin
                foundaddress[found]:=dword(wordp);
                foundvalue2[found]:=wordp^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc wordp;
              end;
            end;
          end;
        except

        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;

      blockwrite(Addressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(Memoryfile,pointer(foundvalue2)^,found*2,actualwrite);
      result:=filesize(Memoryfile) div 2;
    end;


    if scantype=valuebetween then
    begin
      for i:=0 to memoryregions do
      begin
        if not scansettings^.scanning then break;

        inc(position,memoryregion[i].MemorySize);
        wordp:=pointer(memoryregion[i].BaseAddress);

        try
          if scansettings^.FastScan then
          begin
            for j:=0 to (memoryregion[i].MemorySize div 2)-1 do
            begin
              if (wordp^>=wordvalue) and (wordp^<=wordvalue2) then
              begin
                foundaddress[found]:=dword(wordp);
                foundvalue2[found]:=wordp^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                  found:=0;
                end;
              end;
              inc(wordp);
            end;
          end
          else
          begin
            for j:=0 to memoryregion[i].MemorySize-2 do
            begin
              if (wordp^>=wordvalue) and (wordp^<=wordvalue2) then
              begin
                foundaddress[found]:=dword(wordp);
                foundvalue2[found]:=wordp^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue2)^,number*2,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc wordp;
              end;
            end;
          end;
        except

        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;

      blockwrite(Addressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(Memoryfile,pointer(foundvalue2)^,found*2,actualwrite);
      result:=filesize(Memoryfile) div 2;
    end;

  end else
  if vartype=2 then //dword
  begin
    //no need to bother with fastscan routines
    setlength(foundvalue3,number);
    if scantype=exact_value then
    begin
      for i:=0 to memoryregions do
      begin
        if not scansettings^.scanning then break;

        inc(position,memoryregion[i].MemorySize);
        dwordp:=pointer(memoryregion[i].BaseAddress);

        try
          if scansettings^.FastScan then
          begin
            for j:=0 to (memoryregion[i].MemorySize div 4)-1 do
            begin
              if dwordp^=dwordvalue then
              begin
                foundaddress[found]:=dword(dwordp);
                inc(found);

                if found=number then
                begin
                  if not FoundIsFilled then
                  begin
                    for k:=0 to number-1 do foundvalue3[k]:=dwordvalue;
                    FoundIsFilled:=true;
                  end;

                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              inc(dwordp);
            end;
          end
          else
          begin
            for j:=0 to memoryregion[i].MemorySize-4 do
            begin
              if dwordp^=dwordvalue then
              begin
                foundaddress[found]:=dword(dwordp);
                inc(found);

                if found=number then
                begin
                  if not FoundIsFilled then
                  begin
                    for k:=0 to number-1 do foundvalue3[k]:=dwordvalue;
                    FoundIsFilled:=true;
                  end;

                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc dwordp;
              end;
            end;
          end;
        except

        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;

      if not foundisfilled then
        for i:=0 to found-1 do foundvalue3[i]:=dwordvalue;

      blockwrite(Addressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(Memoryfile,pointer(foundvalue3)^,found*4,actualwrite);
      result:=filesize(Memoryfile) div 4;
    end;

    if scantype=BiggerThan then
    begin
      for i:=0 to memoryregions do
      begin
        if not scansettings^.scanning then break;

        inc(position,memoryregion[i].MemorySize);
        dwordp:=pointer(memoryregion[i].BaseAddress);

        try
          if scansettings^.FastScan then
          begin
            for j:=0 to (memoryregion[i].MemorySize-1 div 4)-1 do
            begin
              if dwordp^>dwordvalue then
              begin
                foundaddress[found]:=dword(dwordp);
                foundvalue3[found]:=dwordp^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              inc(dwordp);
            end;
          end
          else
          begin
            for j:=0 to memoryregion[i].MemorySize-4 do
            begin
              if dwordp^>dwordvalue then
              begin
                foundaddress[found]:=dword(dwordp);
                foundvalue3[found]:=dwordp^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc dwordp;
              end;
            end;
          end;
        except

        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;

      blockwrite(Addressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(Memoryfile,pointer(foundvalue3)^,found*4,actualwrite);
      result:=filesize(Memoryfile) div 4;
    end;

    if scantype=SmallerThan then
    begin
      for i:=0 to memoryregions do
      begin
        if not scansettings^.scanning then break;

        inc(position,memoryregion[i].MemorySize);
        dwordp:=pointer(memoryregion[i].BaseAddress);

        try
          if scansettings^.FastScan then
          begin
            for j:=0 to (memoryregion[i].MemorySize div 4)-1 do
            begin
              if dwordp^<dwordvalue then
              begin
                foundaddress[found]:=dword(wordp);
                foundvalue3[found]:=dwordp^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              inc(dwordp);
            end;
          end
          else
          begin
            for j:=0 to memoryregion[i].MemorySize-4 do
            begin
              if dwordp^<dwordvalue then
              begin
                foundaddress[found]:=dword(dwordp);
                foundvalue3[found]:=dwordp^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc dwordp;
              end;
            end;
          end;
        except

        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;

      blockwrite(Addressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(Memoryfile,pointer(foundvalue3)^,found*4,actualwrite);
      result:=filesize(Memoryfile) div 4;
    end;

    if scantype=ValueBetween then
    begin
      for i:=0 to memoryregions do
      begin
        if not scansettings^.scanning then break;

        inc(position,memoryregion[i].MemorySize);
        dwordp:=pointer(memoryregion[i].BaseAddress);

        try
          if scansettings^.FastScan then
          begin
            for j:=0 to (memoryregion[i].MemorySize div 4)-1 do
            begin
              if (dwordp^>=dwordvalue) and (dwordp^<=dwordvalue2) then
              begin
                foundaddress[found]:=dword(wordp);
                foundvalue3[found]:=dwordp^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              inc(dwordp);
            end;
          end
          else
          begin
            for j:=0 to memoryregion[i].MemorySize-4 do
            begin
              if (dwordp^>=dwordvalue) and (dwordp^<=dwordvalue2) then
              begin
                foundaddress[found]:=dword(dwordp);
                foundvalue3[found]:=dwordp^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue3)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc dwordp;
              end;
            end;
          end;
        except

        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;

      blockwrite(Addressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(Memoryfile,pointer(foundvalue3)^,found*4,actualwrite);
      result:=filesize(Memoryfile) div 4;
    end;
  end else
  if vartype=3 then //float
  begin
    //no need to bother with fastscan routines
    setlength(foundvalue4,number);
    if scantype=exact_value then
    begin
      for i:=0 to memoryregions do
      begin
        if not scansettings^.scanning then break;

        inc(position,memoryregion[i].MemorySize);
        singlep:=pointer(memoryregion[i].BaseAddress);

        if decim=0 then helpsingle3:=1 else
          helpsingle3:=1/((decim)*10);  //the range for extremerounded scans


        try
          if scansettings^.FastScan then
          begin
            for j:=0 to (memoryregion[i].MemorySize div 4)-1 do
            begin
              check:=(not (isnan(singlep^) or isinfinite(singlep^)));

              if check then
              case roundingtype of
                rounded:
                begin
                  helpsingle:=RoundTo(SingleP^,-decim);
                  check:=(helpsingle=SingleValue);
                end;

                extremerounded:
                begin
                  //if a scan for 1 it scans for    0<x<2
                  //if a scan for 1.0 it scans for  9.9<x<1.10
                  check:=((SingleP^<(singlevalue+helpsingle3)) and (SingleP^>(singlevalue-helpsingle3)) );
                end;

                truncated:
                begin
                  //if a scan for 1 it scans for    1>=x<2
                  //if a scan for 1.0 it scans for 1.0>=x<1.10
                  check:=((SingleP^<(singlevalue+helpsingle3)) and (singlep^>=singlevalue));
                end;

                else check:=false;
              end;


              if check then
              begin
                foundaddress[found]:=dword(singlep);
                foundvalue4[found]:=singlep^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              inc(singlep);
            end;
          end
          else
          begin
            for j:=0 to memoryregion[i].MemorySize-4 do
            begin
              check:=(not (isnan(singlep^) or isinfinite(singlep^)));

              if check then
              case roundingtype of
                rounded:
                begin
                  helpsingle:=RoundTo(SingleP^,-decim);
                  check:=(helpsingle=SingleValue);
                end;

                extremerounded:
                begin
                  //if a scan for 1 it scans for    0<x<2
                  //if a scan for 1.0 it scans for  9.9<x<1.10
                  check:=((SingleP^<(singlevalue+helpsingle3)) and (SingleP^>(singlevalue-helpsingle3)) );
                end;

                truncated:
                begin
                  //if a scan for 1 it scans for    1>=x<2
                  //if a scan for 1.0 it scans for 1.0>=x<1.10
                  check:=((SingleP^<(singlevalue+helpsingle3)) and (singlep^>=singlevalue));
                end;

                else check:=false;
              end;


              if check then
              begin
                foundaddress[found]:=dword(singlep);
                foundvalue4[found]:=singlep^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc singlep;
              end;
            end;
          end;
        except

        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;

      blockwrite(Addressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(Memoryfile,pointer(foundvalue4)^,found*4,actualwrite);
      result:=filesize(Memoryfile) div 4;
    end;

    if scantype=BiggerThan then
    begin
      for i:=0 to memoryregions do
      begin
        if not scansettings^.scanning then break;

        inc(position,memoryregion[i].MemorySize);
        singlep:=pointer(memoryregion[i].BaseAddress);

        try
          if scansettings^.FastScan then
          begin
            for j:=0 to (memoryregion[i].MemorySize-1 div 4)-1 do
            begin
              if (not (isnan(singlep^) or isinfinite(singlep^))) and (SingleP^>SingleValue) then
              begin
                foundaddress[found]:=dword(singlep);
                foundvalue4[found]:=singlep^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              inc(singlep);
            end;
          end
          else
          begin
            for j:=0 to memoryregion[i].MemorySize-4 do
            begin
              if (not (isnan(singlep^) or isinfinite(singlep^))) and (SingleP^>SingleValue) then
              begin
                foundaddress[found]:=dword(singlep);
                foundvalue4[found]:=singlep^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc singlep;
              end;
            end;
          end;
        except

        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;

      blockwrite(Addressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(Memoryfile,pointer(foundvalue4)^,found*4,actualwrite);
      result:=filesize(Memoryfile) div 4;
    end;

    if scantype=SmallerThan then
    begin
      for i:=0 to memoryregions do
      begin
        if not scansettings^.scanning then break;

        inc(position,memoryregion[i].MemorySize);
        singlep:=pointer(memoryregion[i].BaseAddress);

        try
          if scansettings^.FastScan then
          begin
            for j:=0 to (memoryregion[i].MemorySize-1 div 4)-1 do
            begin
              if (not (isnan(singlep^) or isinfinite(singlep^))) and (SingleP^<SingleValue) then
              begin
                foundaddress[found]:=dword(singlep);
                foundvalue4[found]:=singlep^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              inc(singlep);
            end;
          end
          else
          begin
            for j:=0 to memoryregion[i].MemorySize-4 do
            begin
              if (not (isnan(singlep^) or isinfinite(singlep^))) and (SingleP^<SingleValue) then
              begin
                foundaddress[found]:=dword(singlep);
                foundvalue4[found]:=singlep^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc singlep;
              end;
            end;
          end;
        except

        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;

      blockwrite(Addressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(Memoryfile,pointer(foundvalue4)^,found*4,actualwrite);
      result:=filesize(Memoryfile) div 4;
    end;

    if scantype=ValueBetween then
    begin
      for i:=0 to memoryregions do
      begin
        if not scansettings^.scanning then break;

        inc(position,memoryregion[i].MemorySize);
        singlep:=pointer(memoryregion[i].BaseAddress);

        try
          if scansettings^.FastScan then
          begin
            for j:=0 to (memoryregion[i].MemorySize-1 div 4)-1 do
            begin
              if (not (isnan(singlep^) or isinfinite(singlep^))) and (SingleP^>=SingleValue) and (Singlep^<=singlevalue2) then
              begin
                foundaddress[found]:=dword(singlep);
                foundvalue4[found]:=singlep^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              inc(singlep);
            end;
          end
          else
          begin
            for j:=0 to memoryregion[i].MemorySize-4 do
            begin
              if (not (isnan(singlep^) or isinfinite(singlep^))) and (SingleP^>=SingleValue) and ((SingleP^<=SingleValue2)) then
              begin
                foundaddress[found]:=dword(singlep);
                foundvalue4[found]:=singlep^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue4)^,number*4,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc singlep;
              end;
            end;
          end;
        except

        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;

      blockwrite(Addressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(Memoryfile,pointer(foundvalue4)^,found*4,actualwrite);
      result:=filesize(Memoryfile) div 4;
    end;

  end else
  if vartype=4 then //double
  begin
    //no need to bother with fastscan routines
    setlength(foundvalue5,number);
    if scantype=exact_value then
    begin
      for i:=0 to memoryregions do
      begin
        if not scansettings^.scanning then break;

        inc(position,memoryregion[i].MemorySize);
        doublep:=pointer(memoryregion[i].BaseAddress);

        if decim=0 then helpdouble3:=1 else
          helpdouble3:=1/((decim)*10);  //the range for extremerounded scans


        try
          if scansettings^.FastScan then
          begin
            for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
            begin

              check:=(not (isnan(doublep^) or isinfinite(doublep^)));

              if check then
              case roundingtype of
                rounded:
                begin
                  helpdouble:=RoundTo(doubleP^,-decim);
                  check:=(helpdouble=doubleValue);
                end;

                extremerounded:
                begin
                  //if a scan for 1 it scans for    0<x<2
                  //if a scan for 1.0 it scans for  9.9<x<1.10
                  check:=((doubleP^<(doublevalue+helpdouble3)) and (doubleP^>(doublevalue-helpdouble3)) );
                end;

                truncated:
                begin
                  //if a scan for 1 it scans for    1>=x<2
                  //if a scan for 1.0 it scans for 1.0>=x<1.10
                  check:=((doubleP^<(doublevalue+helpdouble3)) and (doublep^>=doublevalue));
                end;

                else check:=false;
              end;


              if check then
              begin
                foundaddress[found]:=dword(doublep);
                foundvalue5[found]:=doublep^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              inc(doublep);
            end;
          end
          else
          begin
            for j:=0 to memoryregion[i].MemorySize-8 do
            begin
              check:=(not (isnan(doublep^) or isinfinite(doublep^)));

              if check then
              case roundingtype of
                rounded:
                begin
                  helpdouble:=RoundTo(doubleP^,-decim);
                  check:=(helpdouble=doubleValue);
                end;

                extremerounded:
                begin
                  //if a scan for 1 it scans for    0<x<2
                  //if a scan for 1.0 it scans for  9.9<x<1.10
                  check:=((doubleP^<(doublevalue+helpdouble3)) and (doubleP^>(doublevalue-helpdouble3)) );
                end;

                truncated:
                begin
                  //if a scan for 1 it scans for    1>=x<2
                  //if a scan for 1.0 it scans for 1.0>=x<1.10
                  check:=((doubleP^<(doublevalue+helpdouble3)) and (doublep^>=doublevalue));
                end;

                else check:=false;
              end;


              if check then
              begin
                foundaddress[found]:=dword(doublep);
                foundvalue5[found]:=doublep^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc doublep;
              end;
            end;
          end;
        except

        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;

      blockwrite(Addressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(Memoryfile,pointer(foundvalue5)^,found*8,actualwrite);
      result:=filesize(Memoryfile) div 8;
    end;

    if scantype=biggerthan then
    begin
      for i:=0 to memoryregions do
      begin
        if not scansettings^.scanning then break;

        inc(position,memoryregion[i].MemorySize);
        doublep:=pointer(memoryregion[i].BaseAddress);

        try
          if scansettings^.FastScan then
          begin
            for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
            begin
              if (not (isnan(doublep^) or isinfinite(doublep^))) and (doublep^>DoubleValue) then
              begin
                foundaddress[found]:=dword(doublep);
                foundvalue5[found]:=doublep^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              inc(doublep);
            end;
          end
          else
          begin
            for j:=0 to memoryregion[i].MemorySize-8 do
            begin
              if (not (isnan(doublep^) or isinfinite(doublep^))) and (doublep^>DoubleValue) then
              begin
                foundaddress[found]:=dword(doublep);
                foundvalue5[found]:=doublep^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc doublep;
              end;
            end;
          end;
        except

        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;

      blockwrite(Addressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(Memoryfile,pointer(foundvalue5)^,found*8,actualwrite);
      result:=filesize(Memoryfile) div 8;
    end;

    if scantype=smallerthan then
    begin
      for i:=0 to memoryregions do
      begin
        if not scansettings^.scanning then break;

        inc(position,memoryregion[i].MemorySize);
        doublep:=pointer(memoryregion[i].BaseAddress);

        try
          if scansettings^.FastScan then
          begin
            for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
            begin
              if (not (isnan(doublep^) or isinfinite(doublep^))) and (doublep^<DoubleValue) then
              begin
                foundaddress[found]:=dword(doublep);
                foundvalue5[found]:=doublep^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              inc(doublep);
            end;
          end
          else
          begin
            for j:=0 to memoryregion[i].MemorySize-8 do
            begin
              if (not (isnan(doublep^) or isinfinite(doublep^))) and (doublep^<DoubleValue) then
              begin
                foundaddress[found]:=dword(doublep);
                foundvalue5[found]:=doublep^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc doublep;
              end;
            end;
          end;
        except

        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;

      blockwrite(Addressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(Memoryfile,pointer(foundvalue5)^,found*8,actualwrite);
      result:=filesize(Memoryfile) div 8;
    end;

    if scantype=valuebetween then
    begin
      for i:=0 to memoryregions do
      begin
        if not scansettings^.scanning then break;

        inc(position,memoryregion[i].MemorySize);
        doublep:=pointer(memoryregion[i].BaseAddress);

        try
          if scansettings^.FastScan then
          begin
            for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
            begin
              if (not (isnan(doublep^) or isinfinite(doublep^))) and (doublep^>=DoubleValue) and (doublep^<=DoubleValue2) then
              begin
                foundaddress[found]:=dword(doublep);
                foundvalue5[found]:=doublep^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              inc(doublep);
            end;
          end
          else
          begin
            for j:=0 to memoryregion[i].MemorySize-8 do
            begin
              if (not (isnan(doublep^) or isinfinite(doublep^))) and (doublep^>=DoubleValue) and (doublep^<=DoubleValue) then
              begin
                foundaddress[found]:=dword(doublep);
                foundvalue5[found]:=doublep^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue5)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc doublep;
              end;
            end;
          end;
        except

        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;

      blockwrite(Addressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(Memoryfile,pointer(foundvalue5)^,found*8,actualwrite);
      result:=filesize(Memoryfile) div 8;
    end;

  end else
  if vartype=5 then //bit
  begin
    setlength(foundaddressb,number);
    actualwrite:=length(bitscan);
    blockwrite(memoryfile,actualwrite,4,actualwrite);
    blockwrite(memoryfile,bitscan[0],length(bitscan),actualwrite);


    if scantype=exact_value then
    begin
      for i:=0 to memoryregions do
      begin
        inc(position,memoryregion[i].MemorySize);
        bytep:=pointer(memoryregion[i].BaseAddress);
        actualread:=memoryregion[i].MemorySize;

        bittofind:=0;

        if memoryregion[i].ischild then //compare the previous bits
        begin
          //check the tempbits array for the bits
          k:=9-(nrofbits mod 8); //k=1st bit in tempbit array

          for l:=0 to nrofbits-2 do //all bits except for last one. (at the end k should be 0)
          begin
            for j:=0 to nrofbits-2 do //shift left
              tempbits[j]:=tempbits[j+1];
            tempbits[nrofbits-1]:=getbit(l,bytep^);

            for j:=0 to nrofbits-1 do
            begin
              if (bitscan[j]<>2) and (tempbits[j]<>bitscan[j]) then
              begin
                inc(k);
                break;
              end
              else
              begin
                if j=dword(nrofbits)-1 then
                begin
                  //found it

                  foundaddressb[found].address:=memoryregion[i].BaseAddress-(1+(nrofbits div 8));
                  foundaddressb[found].bit:=k;
                  inc(found);
                  if found=buffersize then
                  begin
                    blockwrite(Addressfile,pointer(foundaddressB)^,found*(sizeof(bitaddress)),actualwrite);
                    found:=0;
                  end;
                end;
              end;

            end;
          end;
        end;

        j:=0;
        k:=0;
        try
          scanbits(found,number,bytep,nrofbits,i,actualread);
        except

        end;
        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;
      blockwrite(Addressfile,pointer(foundaddressB)^,found*(sizeof(bitaddress)),actualwrite);

      closefile(addressfile);

      try
        try
          resulthelper:=tfilestream.Create(scansettings^.CheatEngineDir+'Addresses.tmp',fmopenread,fmsharedenynone);
          result:=(resulthelper.Size-7) div sizeof(bitaddress);
        finally
          resulthelper.free;
        end;
      except
        reset(addressfile,1);
        result:=(filesize(addressfile)-7) div sizeof(bitaddress);
        closefile(addressfile);
      end;

      try closefile(addressfile); except end;
      try closefile(memoryfile); except end;
      freememory;
      exit;
    end; //nothing else for bit
  end else
  if vartype=6 then //int64
  begin
    //no need to bother with fastscan routines
    setlength(foundvalue6,number);
    if scantype=exact_value then
    begin
      for i:=0 to memoryregions do
      begin
        if not scansettings^.scanning then break;

        inc(position,memoryregion[i].MemorySize);
        int64p:=pointer(memoryregion[i].BaseAddress);

        try
          if scansettings^.FastScan then
          begin
            for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
            begin
              if int64p^=int64value then
              begin
                foundaddress[found]:=dword(int64p);
                inc(found);

                if found=number then
                begin
                  if not FoundIsFilled then
                  begin
                    for k:=0 to number-1 do foundvalue6[k]:=int64value;
                    FoundIsFilled:=true;
                  end;

                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              inc(int64p);
            end;
          end
          else
          begin
            for j:=0 to memoryregion[i].MemorySize-8 do
            begin
              if int64p^=int64value then
              begin
                foundaddress[found]:=dword(int64p);
                inc(found);

                if found=number then
                begin
                  if not FoundIsFilled then
                  begin
                    for k:=0 to number-1 do foundvalue6[k]:=int64value;
                    FoundIsFilled:=true;
                  end;

                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc int64p;
              end;
            end;
          end;
        except

        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;

      if not foundisfilled then
        for i:=0 to found-1 do foundvalue6[i]:=int64value;

      blockwrite(Addressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(Memoryfile,pointer(foundvalue6)^,found*8,actualwrite);
      result:=filesize(Memoryfile) div 8;
    end;

    if scantype=biggerthan then
    begin
      for i:=0 to memoryregions do
      begin
        if not scansettings^.scanning then break;

        inc(position,memoryregion[i].MemorySize);
        int64p:=pointer(memoryregion[i].BaseAddress);

        try
          if scansettings^.FastScan then
          begin
            for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
            begin
              if int64p^>int64value then
              begin
                foundaddress[found]:=dword(int64p);
                foundvalue6[found]:=int64p^;
                inc(found);

                if found=number then
                begin

                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              inc(int64p);
            end;
          end
          else
          begin
            for j:=0 to memoryregion[i].MemorySize-8 do
            begin
              if int64p^>int64value then
              begin
                foundaddress[found]:=dword(int64p);
                foundvalue6[found]:=int64p^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc int64p;
              end;
            end;
          end;
        except

        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;

      blockwrite(Addressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(Memoryfile,pointer(foundvalue6)^,found*8,actualwrite);
      result:=filesize(Memoryfile) div 8;
    end;


    if scantype=smallerthan then
    begin
      for i:=0 to memoryregions do
      begin
        if not scansettings^.scanning then break;

        inc(position,memoryregion[i].MemorySize);
        int64p:=pointer(memoryregion[i].BaseAddress);

        try
          if scansettings^.FastScan then
          begin
            for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
            begin
              if int64p^<int64value then
              begin
                foundaddress[found]:=dword(int64p);
                foundvalue6[found]:=int64p^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              inc(int64p);
            end;
          end
          else
          begin
            for j:=0 to memoryregion[i].MemorySize-8 do
            begin
              if int64p^<int64value then
              begin
                foundaddress[found]:=dword(int64p);
                foundvalue6[found]:=int64p^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc int64p;
              end;
            end;
          end;
        except

        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;

      blockwrite(Addressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(Memoryfile,pointer(foundvalue6)^,found*8,actualwrite);
      result:=filesize(Memoryfile) div 8;
    end;

    if scantype=valuebetween then
    begin
      for i:=0 to memoryregions do
      begin
        if not scansettings^.scanning then break;

        inc(position,memoryregion[i].MemorySize);
        int64p:=pointer(memoryregion[i].BaseAddress);

        try
          if scansettings^.FastScan then
          begin
            for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
            begin
              if (int64p^>=int64value) and (int64p^<=int64value2) then
              begin
                foundaddress[found]:=dword(int64p);
                foundvalue6[found]:=int64p^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              inc(int64p);
            end;
          end
          else
          begin
            for j:=0 to memoryregion[i].MemorySize-8 do
            begin
              if (int64p^>=int64value) and (int64p^<=int64value2) then
              begin
                foundaddress[found]:=dword(int64p);
                foundvalue6[found]:=int64p^;
                inc(found);

                if found=number then
                begin
                  blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(Memoryfile,pointer(foundvalue6)^,number*8,actualwrite);
                  found:=0;
                end;
              end;
              asm
                inc int64p;
              end;
            end;
          end;
        except

        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;

      blockwrite(Addressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(Memoryfile,pointer(foundvalue6)^,found*8,actualwrite);
      result:=filesize(Memoryfile) div 8;
    end;

  end else
  if vartype=7 then
  begin
    //string
    unicode2:=false;
    scanlength:=length(scanvalue);
    chartofind:=1;
    blockwrite(memoryfile,pointer(scanvalue)^,scanlength,actualread);
    for i:=0 to memoryregions do
    begin
      inc(position,memoryregion[i].MemorySize);
      if not memoryregion[i].IsChild then CharToFind:=1;

      bytep:=pointer(memoryregion[i].BaseAddress);
      try
      for j:=0 to memoryregion[i].MemorySize-1 do
      begin
        if (((not extra) and (uppercase(chr(bytep^))=uppercase(scanvalue[CharToFind])))
           or
           (extra and (chr(bytep^)=scanvalue[CharToFind]))
           or (unicode2 and (bytep^=0) )
           ) then
        begin
          if unicode then unicode2:=bytep^<>0;

          if not unicode2 then
            inc(charToFind);

          if CharToFind=scanlength+1 then //found the string
          begin
            if unicode then
              foundaddress[found]:=Memoryregion[i].BaseAddress+j-(scanlength*2)+1
            else
              foundaddress[found]:=Memoryregion[i].BaseAddress+j-scanlength+1;
            inc(found);
            if found=number then
            begin
              found:=0;
              blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
            end;

            CharToFind:=1;
          end;
        end else CharToFind:=1;

        inc(bytep)
      end;
      except

      end;
      postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
    end;
    blockwrite(Addressfile,pointer(foundaddress)^,4*found,actualwrite);
    result:=(filesize(Addressfile)-sizeof(datatype)) div 4;
  end else
  if vartype=8 then //array of byte
  begin
    result:=0;
    k:=0;
    ConvertStringToBytes(scanvalue,extra,bytes);

    nrofbytes:=length(bytes);
    setlength(foundvalue8,number*nrofbytes);
    blockwrite(memoryfile,nrofbytes,4,actualwrite); //save how many bytes each record is

    //bytes array now holds the bytes to scan -1=wildcard
    for i:=0 to memoryregions do
    begin
      inc(position,Memoryregion[i].MemorySize);
      bytep:=pointer(pointer(Memoryregion[i].BaseAddress));

      if not memoryregion[i].IsChild then k:=0;

      try
      for j:=0 to Memoryregion[i].MemorySize-nrofbytes do
      begin
        if bytes[k]=-1 then
        begin
          inc(k);
        end else
        begin
          if bytep^=byte(bytes[k]) then inc(k) else k:=0;
        end;

        if k=length(bytes) then //found one
        begin
          foundaddress[found]:=Memoryregion[i].BaseAddress+j-nrofbytes+1;
          copymemory(pointer(@foundvalue8[found*nrofbytes]),pointer(dword(bytep)-nrofbytes+1),nrofbytes);

          inc(found);

          k:=0;

          if found=number then
          begin
            blockwrite(Addressfile,pointer(foundaddress)^,4*number,actualwrite);
            blockwrite(Memoryfile,pointeR(foundvalue8)^,nrofbytes*number,actualwrite);
            inc(result,found);
            found:=0;
          end;
        end;

        inc(bytep);
      end;
      except

      end;
      postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
    end;
    blockwrite(Addressfile,pointer(foundaddress)^,4*found,actualwrite);
    blockwrite(Memoryfile,pointeR(foundvalue8)^,nrofbytes*found,actualwrite);
    inc(result,found);
  end;


  freememory;
end;

function NextScan:Integer;
var bytestoscan: Dword;
    biggestchunk: dword;
    i,j,k: integer;
    position:dword;

    valuetype,scantype: integer;
    actualread,actualwrite,actualread2:dword;

    ByteValue,bytevalue2: Byte;
    WordValue,wordvalue2: Word;
    DWordValue,dwordvalue2: Dword;
    SingleValue,singlevalue2: Single;
    doubleValue,doublevalue2: double;
    Int64Value,int64value2: Int64;

    ByteP: pbyte;
    bytep2: pbyte;
    WordP: ^Word;
    DwordP: ^Dword;
    Int64P: ^Int64;
    SingleP: ^Single;
    DoubleP: ^double;

    oldByteP: pbyte;
    oldbytep2: pbyte;
    oldWordP: ^Word;
    oldDwordP: ^Dword;
    oldInt64P: ^Int64;
    oldSingleP: ^Single;
    oldDoubleP: ^double;

    helpsingle,helpsingle2,helpsingle3: single;
    helpdouble,helpdouble2,helpdouble3: double;

    fastscan:boolean;
    number:dword;
    found: dword;
    scanvalue:string;
    vartype: integer;
    extra,check:boolean;
    helpword:word;
    helpdword:dword;
    decim,decimhelp: integer;
    datatype: string[6];
    foundisfilled: boolean;
    l: integer;

    startbit:integer;
    bytearray: array of byte;
    x: pointer;
    helpstr: string;
    nrofbytes: integer;
    roundingtype: tfloatscan;
    unicode: boolean;
    percentage: boolean;

    scanvalue2: string;
begin
  x:=bytearray;
  try

  foundisfilled:=false;
  buffersize:=scansettings^.buffersize;
  number:=buffersize;
  position:=0;
  found:=0;
  valuetype:=scansettings^.ValueType;
  vartype:=valuetype;
  scantype:=scansettings^.Scantype;
  fastscan:=scansettings^.FastScan;
  scanvalue:=scansettings^.scanvalue;
  scanvalue2:=scansettings^.scanvalue2;
  extra:=scansettings^.Hexadecimal;
  roundingtype:=scansettings^.roundingtype;
  unicode:=scansettings^.unicode;
  percentage:=scansettings^.percentage;
  dwordvalue:=0;
  wordvalue:=0;
  singlevalue:=0;
  bytevalue:=0;
  result:=0;

  if (lastscan=1) and ((scansettings^.Scantype=exact_value) or (scansettings^.Scantype=biggerthan) or (scansettings^.Scantype=smallerthan) ) then
  begin
    freemem(memory);
    memory:=nil;
    result:=ExactValueFirstScan;
    exit;
  end;

  if scanvalue='' then scanvalue:='0';
  if scanvalue2='' then scanvalue2:='0';

  if scantype in [Exact_value,Increased_value_by,decreased_value_by,SmallerThan,BiggerThan,valuebetween] then
  begin
    if vartype=0 then
    begin
      if extra then val('$'+scanvalue,bytevalue,i)
               else val(scanvalue,bytevalue,i);
      if i>0 then raise Exception.Create(scanvalue+' is not a valid byte notation');
    end;

    if vartype=1 then
    begin
      if extra then val('$'+scanvalue,wordvalue,i)
               else val(scanvalue,wordvalue,i);
      if i>0 then raise Exception.Create(scanvalue+' is not an valid 2-byte notation');
    end;

    if vartype=2 then
    begin
      if extra then val('$'+scanvalue,dwordvalue,i)
               else val(scanvalue,dwordvalue,i);
      if i>0 then raise Exception.Create(scanvalue+' is not an valid 4-byte notation');
    end;

    if vartype=3 then //doesnt have the hexadecimal option
    begin
      val(scanvalue,singlevalue,i);
      if i>0 then
        if scanvalue[i]=',' then scanvalue[i]:='.';

      val(scanvalue,singlevalue,i);
      if i>0 then raise Exception.Create(scanvalue+' is not a valid floating point notation');
    end;

    if vartype=4 then //same as 3
    begin
      val(scanvalue,doublevalue,i);
      if i>0 then
        if scanvalue[i]=',' then scanvalue[i]:='.';
      val(scanvalue,singlevalue,i);

      if i>0 then raise Exception.create(scanvalue+' is not a valid floating point notation');
    end;

    if vartype=5 then
    begin
      if pos('-',scanvalue)>0 then raise exception.create(scanvalue+' is not a valid notation!');
      if not extra then
      begin
        setlength(bitscan,length(scanvalue));
        j:=0;
        for i:=length(scanvalue) downto 1 do
        begin
          if scanvalue[i]='0' then bitscan[length(scanvalue)-i]:=0
          else
          if scanvalue[i]='1' then bitscan[length(scanvalue)-i]:=1
          else
          if scanvalue[i]='?' then bitscan[length(scanvalue)-i]:=2
          else
          if scanvalue[i]='*' then bitscan[length(scanvalue)-i]:=2
          else
          begin
            freememory;
            raise Exception.create(scanvalue+' is not a valid binary notation');
          end;
          inc(j);
        end;
      end
      else
      begin
        try
          dwordvalue:=StrToInt(scanvalue);
          i:=0;

          if dwordvalue=0 then
          begin
            setlength(bitscan,1);
            bitscan[0]:=0;
          end
          else
          while dwordvalue>0 do
          begin
            setlength(bitscan,i+1);

            if (dwordvalue mod 2)>0 then bitscan[i]:=1
                                    else bitscan[i]:=0;

            inc(i);
            dwordvalue:=dwordvalue div 2;
          end;
        except
          raise Exception.create(scanvalue+' is not a valid notation');
        end;
      end;

      nrofbits:=length(bitscan);
      scansettings^.nrofbits:=nrofbits;
      if nrofbits=0 then raise exception.Create(scanvalue+' did not result in a string of bits');
    end;

    if vartype=6 then
    begin
      if extra then
        val('$'+scanvalue,int64value,i)
      else
        val(scanvalue,Int64value,i);
      if i>0 then raise Exception.Create(scanvalue+' is not an valid 8-byte notation');
    end;
  end;

  if scantype=valuebetween then
  begin
    if vartype=0 then
    begin
      if extra then val('$'+scanvalue2,bytevalue2,i)
               else val(scanvalue2,bytevalue2,i);
      if i>0 then raise Exception.Create(scanvalue2+' is not a valid byte notation');
    end;

    if vartype=1 then
    begin
      if extra then val('$'+scanvalue2,wordvalue2,i)
               else val(scanvalue2,wordvalue2,i);
      if i>0 then raise Exception.Create(scanvalue2+' is not an valid 2-byte notation');
    end;

    if vartype=2 then
    begin
      if extra then val('$'+scanvalue2,dwordvalue2,i)
               else val(scanvalue2,dwordvalue2,i);
      if i>0 then raise Exception.Create(scanvalue2+' is not an valid 4-byte notation');
    end;

    if vartype=3 then //doesnt have the hexadecimal option
    begin
      val(scanvalue2,singlevalue2,i);
      if i>0 then
        if scanvalue2[i]=',' then scanvalue2[i]:='.';

      val(scanvalue2,singlevalue2,i);
      if i>0 then raise Exception.Create(scanvalue2+' is not a valid floating point notation');
    end;

    if vartype=4 then //same as 3
    begin
      val(scanvalue2,doublevalue2,i);
      if i>0 then
        if scanvalue2[i]=',' then scanvalue2[i]:='.';
      val(scanvalue2,singlevalue2,i);

      if i>0 then raise Exception.create(scanvalue2+' is not a valid floating point notation');
    end;

    if vartype=6 then
    begin
      if extra then
        val('$'+scanvalue2,int64value2,i)
      else
        val(scanvalue2,Int64value2,i);
      if i>0 then raise Exception.Create(scanvalue2+' is not an valid 8-byte notation');
    end;

  end;


  helpword:=wordvalue xor 1+random(65534);
  helpdword:=dwordvalue xor helpword;

  //find the digits
  decim:=0;

  val(scanvalue,decimhelp,i);

  //i:=pos('.',scanvalue);
  if i>0 then decim:=length(scanvalue)-i;

  assignfile(Addressfile,scansettings^.CheatEngineDir+'Addresses.TMP');
  reset(Addressfile,1);
  assignfile(memoryfile,scansettings^.CheatEngineDir+'Memory.TMP');
  reset(memoryfile,1);

  assignfile(newAddressfile,scansettings^.CheatEngineDir+'Addresses2.TMP');
  rewrite(newAddressfile,1);
  assignfile(newmemoryfile,scansettings^.CheatEngineDir+'Memory2.TMP');
  rewrite(newmemoryfile,1);

  datatype:='NORMAL';
  blockwrite(NewAddressFile,datatype,sizeof(datatype),actualwrite);


  //previous scan was a unknown initial value scan
  if lastscan=1 then
  begin
    outputdebugstring('lastscan=1');

    lastscan:=0;
    totaltoread:=0;
    biggestchunk:=0;
    for i:=0 to memoryregions do
    begin
      inc(totaltoread,memoryregion[i].MemorySize);
      if biggestchunk<memoryregion[i].MemorySize then biggestchunk:=memoryregion[i].MemorySize;
    end;
    postmessage(scansettings^.formscanningHandle,ESSetProgressbarMax,TotalToRead,TotalToRead);

    //previous scan was an unknown initial value scan
    if scansettings^.LowMemoryUsage then //load the old memory from file and check that
    begin
      getmem(memory,biggestchunk);


      setlength(foundaddress,number);

      if valuetype=0 then //byte  (no fastscan)
      begin
        setlength(foundvalue1,number);
        for i:=0 to memoryregions do
        begin
          bytep:=pointer(memoryregion[i].BaseAddress);
          oldbytep:=pointer(memory);

          inc(position,memoryregion[i].MemorySize);
          actualread:=0;
          blockread(memoryfile,memory^,memoryregion[i].MemorySize,actualread);

          //memory is now filled with the old memory
          if actualread<>memoryregion[i].MemorySize then raise Exception.Create('RAAAH. OMFG. NOOOOOOOO');
          try


            case scantype of
              increased_value:
              begin
                for j:=0 to memoryregion[i].MemorySize-1 do
                begin
                  if bytep^>oldbytep^ then
                  begin
                    foundaddress[found]:=dword(bytep);
                    foundvalue1[found]:=bytep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue1)^,number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(bytep);
                  inc(oldbytep);
                end;
              end;

              increased_value_by:
              begin
                for j:=0 to memoryregion[i].MemorySize-1 do
                begin
                  if ((not percentage) and (bytep^=oldbytep^+bytevalue)) or
                     (percentage and (bytep^>=oldbytep^+trunc(oldbytep^*(bytevalue/100))))
                  then
                  begin
                    foundaddress[found]:=dword(bytep);
                    foundvalue1[found]:=bytep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue1)^,number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(bytep);
                  inc(oldbytep);
                end;
              end;

              decreased_value:
              begin
                for j:=0 to memoryregion[i].MemorySize-1 do
                begin
                  if bytep^<oldbytep^ then
                  begin
                    foundaddress[found]:=dword(bytep);
                    foundvalue1[found]:=bytep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue1)^,number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(bytep);
                  inc(oldbytep);
                end;
              end;

              decreased_value_by:
              begin
                outputdebugstring('decreased value by');
                for j:=0 to memoryregion[i].MemorySize-1 do
                begin
                  if ((not percentage) and (bytep^=oldbytep^-bytevalue)) or
                     (percentage and (bytep^<=oldbytep^-trunc(oldbytep^*(bytevalue/100)))) then

                  begin
                    foundaddress[found]:=dword(bytep);
                    foundvalue1[found]:=bytep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue1)^,number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(bytep);
                  inc(oldbytep);
                end;
              end;

              changed_value:
              begin
                for j:=0 to memoryregion[i].MemorySize-1 do
                begin
                  if bytep^<>oldbytep^ then
                  begin
                    foundaddress[found]:=dword(bytep);
                    foundvalue1[found]:=bytep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue1)^,number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(bytep);
                  inc(oldbytep);
                end;
              end;

              unchanged_value:
              begin
                for j:=0 to memoryregion[i].MemorySize-1 do
                begin
                  if bytep^=oldbytep^ then
                  begin
                    foundaddress[found]:=dword(bytep);
                    foundvalue1[found]:=bytep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue1)^,number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(bytep);
                  inc(oldbytep);
                end;
              end;
            end;
          except

          end;

          postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
        end;

        blockwrite(newaddressfile,pointer(foundaddress)^,4*found,actualwrite);
        blockwrite(newmemoryfile,pointer(foundvalue1)^,found,actualwrite);
        result:=filesize(NewMemoryfile);
      end else
      if valuetype=1 then //word
      begin
        setlength(foundvalue2,number);
        for i:=0 to memoryregions do
        begin
          wordp:=pointer(memoryregion[i].BaseAddress);
          oldwordp:=pointer(memory);

          inc(position,memoryregion[i].MemorySize);
          actualread:=0;
          blockread(memoryfile,memory^,memoryregion[i].MemorySize,actualread);

          //memory is now filled with the old memory
          if actualread<>memoryregion[i].MemorySize then raise Exception.Create('RAAAH. OMFG. NOOOOOOOO');
          try
            case scantype of
              increased_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 2)-1 do
                begin
                  if wordp^>oldwordp^ then
                  begin
                    foundaddress[found]:=dword(wordp);
                    foundvalue2[found]:=wordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  inc(wordp);
                  inc(oldwordp);
                end
                else
                for j:=0 to (memoryregion[i].MemorySize)-2 do
                begin
                  if wordp^>oldwordp^ then
                  begin
                    foundaddress[found]:=dword(wordp);
                    foundvalue2[found]:=wordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  asm
                    inc wordp;
                    inc oldwordp;
                  end;
                end
              end;

              increased_value_by:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 2)-1 do
                begin
                  if ((not percentage) and (wordp^=oldwordp^+wordvalue)) or
                     (percentage and (wordp^>=oldwordp^+trunc(oldwordp^*(wordvalue/100)))) then
                  begin
                    foundaddress[found]:=dword(wordp);
                    foundvalue2[found]:=wordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(wordp);
                  inc(oldwordp);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-2 do
                begin
                  if ((not percentage) and (wordp^=oldwordp^+wordvalue)) or
                     (percentage and (wordp^>=oldwordp^+trunc(oldwordp^*(wordvalue/100)))) then
                  begin
                    foundaddress[found]:=dword(wordp);
                    foundvalue2[found]:=wordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc wordp;
                    inc oldwordp;
                  end;
                end
              end;

              decreased_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 2)-1 do
                begin
                  if wordp^<oldwordp^ then
                  begin
                    foundaddress[found]:=dword(wordp);
                    foundvalue2[found]:=wordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(wordp);
                  inc(oldwordp);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-2 do
                begin
                  if wordp^<oldwordp^ then
                  begin
                    foundaddress[found]:=dword(wordp);
                    foundvalue2[found]:=wordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc wordp;
                    inc oldwordp;
                  end;
                end;
              end;

              decreased_value_by:
              begin
                outputdebugstring('decreased value by');
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 2)-1 do
                begin
                  if ((not percentage) and (wordp^=oldwordp^-wordvalue)) or
                     (percentage and (wordp^<=oldwordp^-trunc(oldwordp^*(wordvalue/100)))) then
                  begin
                    foundaddress[found]:=dword(wordp);
                    foundvalue2[found]:=wordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  inc(wordp);
                  inc(oldwordp);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-2 do
                begin
                  if ((not percentage) and (wordp^=oldwordp^-wordvalue)) or
                     (percentage and (wordp^<=oldwordp^-trunc(oldwordp^*(wordvalue/100)))) then
                  begin
                    foundaddress[found]:=dword(wordp);
                    foundvalue2[found]:=wordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc wordp
                    inc oldwordp
                  end;
                end;
              end;

              changed_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 2)-1 do
                begin
                  if wordp^<>oldwordp^ then
                  begin
                    foundaddress[found]:=dword(wordp);
                    foundvalue2[found]:=wordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(wordp);
                  inc(oldwordp);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-2 do
                begin
                  if wordp^<>oldwordp^ then
                  begin
                    foundaddress[found]:=dword(wordp);
                    foundvalue2[found]:=wordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc wordp;
                    inc oldwordp;
                  end;
                end;
              end;

              unchanged_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 2)-1 do
                begin
                  if wordp^=oldwordp^ then
                  begin
                    foundaddress[found]:=dword(wordp);
                    foundvalue2[found]:=wordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(wordp);
                  inc(oldwordp);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-2 do
                begin
                  if wordp^=oldwordp^ then
                  begin
                    foundaddress[found]:=dword(wordp);
                    foundvalue2[found]:=wordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                      found:=0;
                    end;
                  end;
                  asm
                    inc wordp;
                    inc oldwordp;
                  end;
                end;
              end;
            end;
          except

          end;

          postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
        end;

        blockwrite(newaddressfile,pointer(foundaddress)^,4*found,actualwrite);
        blockwrite(newmemoryfile,pointer(foundvalue2)^,2*found,actualwrite);
        result:=filesize(NewMemoryfile) div 2;
      end else
      if valuetype=2 then //dword
      begin
        setlength(foundvalue3,number);
        for i:=0 to memoryregions do
        begin
          dwordp:=pointer(memoryregion[i].BaseAddress);
          olddwordp:=pointer(memory);

          inc(position,memoryregion[i].MemorySize);
          actualread:=0;
          blockread(memoryfile,memory^,memoryregion[i].MemorySize,actualread);

          //memory is now filled with the old memory
          if actualread<>memoryregion[i].MemorySize then raise Exception.Create('RAAAH. OMFG. NOOOOOOOO');
          try
            case scantype of
              increased_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 4)-1 do
                begin
                  if dwordp^>olddwordp^ then
                  begin
                    foundaddress[found]:=dword(dwordp);
                    foundvalue3[found]:=dwordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  inc(dwordp);
                  inc(olddwordp);
                end
                else
                for j:=0 to (memoryregion[i].MemorySize)-4 do
                begin
                  if dwordp^>olddwordp^ then
                  begin
                    foundaddress[found]:=dword(dwordp);
                    foundvalue3[found]:=dwordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  asm
                    inc dwordp;
                    inc olddwordp;
                  end;
                end
              end;

              increased_value_by:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 4)-1 do
                begin
                  if ((not percentage) and (dwordp^=olddwordp^+dwordvalue)) or
                     (percentage and (dwordp^>=olddwordp^+trunc(olddwordp^*(dwordvalue/100)))) then
                  begin
                    foundaddress[found]:=dword(dwordp);
                    foundvalue3[found]:=dwordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(dwordp);
                  inc(olddwordp);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-4 do
                begin
                  if ((not percentage) and (dwordp^=olddwordp^+dwordvalue)) or
                     (percentage and (dwordp^>=olddwordp^+trunc(olddwordp^*(dwordvalue/100)))) then
                  begin
                    foundaddress[found]:=dword(dwordp);
                    foundvalue3[found]:=dwordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc dwordp;
                    inc olddwordp;
                  end;
                end
              end;

              decreased_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 4)-1 do
                begin
                  if dwordp^<olddwordp^ then
                  begin
                    foundaddress[found]:=dword(dwordp);
                    foundvalue3[found]:=dwordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(dwordp);
                  inc(olddwordp);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-4 do
                begin
                  if dwordp^<olddwordp^ then
                  begin
                    foundaddress[found]:=dword(dwordp);
                    foundvalue3[found]:=dwordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc dwordp;
                    inc olddwordp;
                  end;
                end;
              end;

              decreased_value_by:
              begin
                outputdebugstring('decreased value by');
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 4)-1 do
                begin
                  if ((not percentage) and (dwordp^=olddwordp^-dwordvalue)) or
                     (percentage and (dwordp^<=olddwordp^-trunc(olddwordp^*(dwordvalue/100)))) then
                  begin
                    foundaddress[found]:=dword(dwordp);
                    foundvalue3[found]:=dwordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  inc(dwordp);
                  inc(olddwordp);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-4 do
                begin
                  if ((not percentage) and (dwordp^=olddwordp^-dwordvalue)) or
                     (percentage and (dwordp^<=olddwordp^-trunc(olddwordp^*(dwordvalue/100)))) then
                  begin
                    foundaddress[found]:=dword(dwordp);
                    foundvalue3[found]:=dwordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc dwordp
                    inc olddwordp
                  end;
                end;
              end;

              changed_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 4)-1 do
                begin
                  if dwordp^<>olddwordp^ then
                  begin
                    foundaddress[found]:=dword(dwordp);
                    foundvalue3[found]:=dwordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(dwordp);
                  inc(olddwordp);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-4 do
                begin
                  if dwordp^<>olddwordp^ then
                  begin
                    foundaddress[found]:=dword(dwordp);
                    foundvalue3[found]:=dwordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc dwordp;
                    inc olddwordp;
                  end;
                end;
              end;

              unchanged_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 4)-1 do
                begin
                  if dwordp^=olddwordp^ then
                  begin
                    foundaddress[found]:=dword(dwordp);
                    foundvalue3[found]:=dwordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(dwordp);
                  inc(olddwordp);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-4 do
                begin
                  if dwordp^=olddwordp^ then
                  begin
                    foundaddress[found]:=dword(dwordp);
                    foundvalue3[found]:=dwordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;
                  end;
                  asm
                    inc dwordp;
                    inc olddwordp;
                  end;
                end;
              end;
            end;
          except

          end;

          postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
        end;

        blockwrite(newaddressfile,pointer(foundaddress)^,4*found,actualwrite);
        blockwrite(newmemoryfile,pointer(foundvalue3)^,4*found,actualwrite);
        result:=filesize(NewMemoryfile) div 4;
      end else
      if valuetype=3 then //float
      begin
        setlength(foundvalue4,number);
        if decim=0 then helpsingle3:=1 else
          helpsingle3:=1/((decim)*10);  //the range for extremerounded scans



        for i:=0 to memoryregions do
        begin
          singlep:=pointer(memoryregion[i].BaseAddress);
          oldsinglep:=pointer(memory);

          inc(position,memoryregion[i].MemorySize);
          actualread:=0;
          blockread(memoryfile,memory^,memoryregion[i].MemorySize,actualread);

          //memory is now filled with the old memory
          if actualread<>memoryregion[i].MemorySize then raise Exception.Create('RAAAH. OMFG. NOOOOOOOO');
          try
            case scantype of
              increased_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 4)-1 do
                begin
                  if (not (isnan(singlep^) or isinfinite(singlep^))) and (SingleP^>oldsinglep^) then
                  begin
                    foundaddress[found]:=dword(singlep);
                    foundvalue4[found]:=singlep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  inc(singlep);
                  inc(oldsinglep);
                end
                else
                for j:=0 to (memoryregion[i].MemorySize)-4 do
                begin
                  if (not (isnan(singlep^) or isinfinite(singlep^))) and (SingleP^>Oldsinglep^) then
                  begin
                    foundaddress[found]:=dword(singlep);
                    foundvalue4[found]:=singlep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  asm
                    inc singlep;
                    inc oldsinglep;
                  end;
                end
              end;

              increased_value_by:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 4)-1 do
                begin
                  helpsingle:=RoundTo(SingleP^,-decim);
                  helpsingle2:=RoundTo(OldSingleP^,-decim);
                  if (not (isnan(singlep^) or isinfinite(singlep^))) and
                  (
                  ((not percentage) and (helpsingle=helpsingle2+SingleValue)) or
                  (percentage and (singlep^>=oldsinglep^+oldsinglep^*(singlevalue/100)))
                  )
                  then

                  begin
                    foundaddress[found]:=dword(singlep);
                    foundvalue4[found]:=singlep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(singlep);
                  inc(oldsinglep);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-4 do
                begin
                  helpsingle:=RoundTo(SingleP^,-decim);
                  helpsingle2:=RoundTo(OldSingleP^,-decim);
                  if (not (isnan(singlep^) or isinfinite(singlep^))) and
                  (
                  ((not percentage) and (helpsingle=helpsingle2+SingleValue)) or
                  (percentage and (singlep^>=oldsinglep^+oldsinglep^*(singlevalue/100)))
                  )
                  then
                  begin
                    foundaddress[found]:=dword(singlep);
                    foundvalue4[found]:=singlep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc singlep;
                    inc oldsinglep;
                  end;
                end
              end;

              decreased_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 4)-1 do
                begin
                  if (not (isnan(singlep^) or isinfinite(singlep^))) and (SingleP^<OldSingleP^) then
                  begin
                    foundaddress[found]:=dword(singlep);
                    foundvalue4[found]:=singlep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(singlep);
                  inc(oldsinglep);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-4 do
                begin
                  if (not (isnan(singlep^) or isinfinite(singlep^))) and (SingleP^<OldSingleP^) then
                  begin
                    foundaddress[found]:=dword(singlep);
                    foundvalue4[found]:=singlep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc singlep;
                    inc oldsinglep;
                  end;
                end;
              end;

              decreased_value_by:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 4)-1 do
                begin
                  helpsingle:=RoundTo(SingleP^,-decim);
                  helpsingle2:=RoundTo(OldSingleP^,-decim);
                  if (not (isnan(singlep^) or isinfinite(singlep^))) and
                  (
                  ((not percentage) and (helpsingle=helpsingle2-SingleValue)) or
                  (percentage and (singlep^<=oldsinglep^-oldsinglep^*(singlevalue/100)))
                  )
                  then
                  begin
                    foundaddress[found]:=dword(singlep);
                    foundvalue4[found]:=singlep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  inc(singlep);
                  inc(oldsinglep);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-4 do
                begin
                  if (not (isnan(singlep^) or isinfinite(singlep^))) and
                  (
                  ((not percentage) and (helpsingle=helpsingle2-SingleValue)) or
                  (percentage and (singlep^<=oldsinglep^-oldsinglep^*(singlevalue/100)))
                  )
                  then
                  begin
                    foundaddress[found]:=dword(singlep);
                    foundvalue4[found]:=singlep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc singlep
                    inc oldsinglep
                  end;
                end;
              end;

              changed_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 4)-1 do
                begin
                  if (not (isnan(singlep^) or isinfinite(singlep^))) and (SingleP^<>OldSingleP^) then
                  begin
                    foundaddress[found]:=dword(singlep);
                    foundvalue4[found]:=singlep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(singlep);
                  inc(oldsinglep);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-4 do
                begin
                  if (not (isnan(singlep^) or isinfinite(singlep^))) and (SingleP^<>OldSingleP^) then
                  begin
                    foundaddress[found]:=dword(singlep);
                    foundvalue4[found]:=singlep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc singlep;
                    inc oldsinglep;
                  end;
                end;
              end;

              unchanged_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 4)-1 do
                begin
                  if (not (isnan(singlep^) or isinfinite(singlep^))) and (SingleP^=OldSingleP^) then
                  begin
                    foundaddress[found]:=dword(singlep);
                    foundvalue4[found]:=singlep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(singlep);
                  inc(oldsinglep);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-4 do
                begin
                  if (not (isnan(singlep^) or isinfinite(singlep^))) and (SingleP^=OldSingleP^) then
                  begin
                    foundaddress[found]:=dword(singlep);
                    foundvalue4[found]:=singlep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                      found:=0;
                    end;
                  end;
                  asm
                    inc singlep;
                    inc oldsinglep;
                  end;
                end;
              end;
            end;
          except

          end;

          postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
        end;

        blockwrite(newaddressfile,pointer(foundaddress)^,4*found,actualwrite);
        blockwrite(newmemoryfile,pointer(foundvalue4)^,4*found,actualwrite);
        result:=filesize(NewMemoryfile) div 4;
      end else
      if valuetype=4 then //double
      begin
        setlength(foundvalue4,number);
        for i:=0 to memoryregions do
        begin
          doublep:=pointer(memoryregion[i].BaseAddress);
          olddoublep:=pointer(memory);

          inc(position,memoryregion[i].MemorySize);
          actualread:=0;
          blockread(memoryfile,memory^,memoryregion[i].MemorySize,actualread);

          //memory is now filled with the old memory
          if actualread<>memoryregion[i].MemorySize then raise Exception.Create('RAAAH. OMFG. NOOOOOOOO');
          try
            case scantype of
              increased_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
                begin
                  if (not (isnan(doublep^) or isinfinite(doublep^))) and (doubleP^>olddoublep^) then
                  begin
                    foundaddress[found]:=dword(doublep);
                    foundvalue5[found]:=doublep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  inc(doublep);
                  inc(olddoublep);
                end
                else
                for j:=0 to (memoryregion[i].MemorySize)-8 do
                begin
                  if (not (isnan(doublep^) or isinfinite(doublep^))) and (doubleP^>Olddoublep^) then
                  begin
                    foundaddress[found]:=dword(doublep);
                    foundvalue5[found]:=doublep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  asm
                    inc doublep;
                    inc olddoublep;
                  end;
                end
              end;

              increased_value_by:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
                begin
                  if (not (isnan(doublep^) or isinfinite(doublep^))) and
                  (
                  ((not percentage) and (helpdouble=helpdouble2+doubleValue)) or
                  (percentage and (doublep^>=olddoublep^+olddoublep^*(doublevalue/100)))
                  )
                  then
                  begin
                    foundaddress[found]:=dword(doublep);
                    foundvalue5[found]:=doublep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(doublep);
                  inc(olddoublep);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-8 do
                begin
                  if (not (isnan(doublep^) or isinfinite(doublep^))) and
                  (
                  ((not percentage) and (helpdouble=helpdouble2+doubleValue)) or
                  (percentage and (doublep^>=olddoublep^+olddoublep^*(doublevalue/100)))
                  )
                  then
                  begin
                    foundaddress[found]:=dword(doublep);
                    foundvalue5[found]:=doublep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc doublep;
                    inc olddoublep;
                  end;
                end
              end;

              decreased_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
                begin
                  if (not (isnan(doublep^) or isinfinite(doublep^))) and (doubleP^<OlddoubleP^) then
                  begin
                    foundaddress[found]:=dword(doublep);
                    foundvalue5[found]:=doublep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(doublep);
                  inc(olddoublep);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-8 do
                begin
                  if (not (isnan(doublep^) or isinfinite(doublep^))) and (doubleP^<OlddoubleP^) then
                  begin
                    foundaddress[found]:=dword(doublep);
                    foundvalue5[found]:=doublep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc doublep;
                    inc olddoublep;
                  end;
                end;
              end;

              decreased_value_by:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
                begin
                  if (not (isnan(doublep^) or isinfinite(doublep^))) and
                  (
                  ((not percentage) and (helpdouble=helpdouble2-doubleValue)) or
                  (percentage and (doublep^<=olddoublep^-olddoublep^*(doublevalue/100)))
                  )
                  then
                  begin
                    foundaddress[found]:=dword(doublep);
                    foundvalue5[found]:=doublep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  inc(doublep);
                  inc(olddoublep);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-8 do
                begin
                  if (not (isnan(doublep^) or isinfinite(doublep^))) and
                  (
                  ((not percentage) and (helpdouble=helpdouble2-doubleValue)) or
                  (percentage and (doublep^<=olddoublep^-olddoublep^*(doublevalue/100)))
                  )
                  then
                  begin
                    foundaddress[found]:=dword(doublep);
                    foundvalue5[found]:=doublep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc doublep
                    inc olddoublep
                  end;
                end;
              end;

              changed_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
                begin
                  if (not (isnan(doublep^) or isinfinite(doublep^))) and (doubleP^<>OlddoubleP^) then
                  begin
                    foundaddress[found]:=dword(doublep);
                    foundvalue5[found]:=doublep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(doublep);
                  inc(olddoublep);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-8 do
                begin
                  if (not (isnan(doublep^) or isinfinite(doublep^))) and (doubleP^<>OlddoubleP^) then
                  begin
                    foundaddress[found]:=dword(doublep);
                    foundvalue5[found]:=doublep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc doublep;
                    inc olddoublep;
                  end;
                end;
              end;

              unchanged_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
                begin
                  if (not (isnan(doublep^) or isinfinite(doublep^))) and (doubleP^=OlddoubleP^) then
                  begin
                    foundaddress[found]:=dword(doublep);
                    foundvalue5[found]:=doublep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(doublep);
                  inc(olddoublep);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-8 do
                begin
                  if (not (isnan(doublep^) or isinfinite(doublep^))) and (doubleP^=OlddoubleP^) then
                  begin
                    foundaddress[found]:=dword(doublep);
                    foundvalue5[found]:=doublep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                      found:=0;
                    end;
                  end;
                  asm
                    inc doublep;
                    inc olddoublep;
                  end;
                end;
              end;
            end;
          except

          end;

          postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
        end;

        blockwrite(newaddressfile,pointer(foundaddress)^,4*found,actualwrite);
        blockwrite(newmemoryfile,pointer(foundvalue5)^,8*found,actualwrite);
        result:=filesize(NewMemoryfile) div 8;
      end else //no unknown for 5 (bit)
      if valuetype=6 then //int64
      begin
        setlength(foundvalue4,number);
        for i:=0 to memoryregions do
        begin
          int64p:=pointer(memoryregion[i].BaseAddress);
          oldint64p:=pointer(memory);

          inc(position,memoryregion[i].MemorySize);
          actualread:=0;
          blockread(memoryfile,memory^,memoryregion[i].MemorySize,actualread);

          //memory is now filled with the old memory
          if actualread<>memoryregion[i].MemorySize then raise Exception.Create('RAAAH. OMFG. NOOOOOOOO');
          try
            case scantype of
              increased_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
                begin
                  if int64p^>oldint64p^ then
                  begin
                    foundaddress[found]:=dword(int64p);
                    foundvalue6[found]:=int64p^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue6)^,8*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  inc(int64p);
                  inc(oldint64p);
                end
                else
                for j:=0 to (memoryregion[i].MemorySize)-4 do
                begin
                  if int64p^>oldint64p^ then
                  begin
                    foundaddress[found]:=dword(int64p);
                    foundvalue6[found]:=int64p^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue6)^,8*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  asm
                    inc int64p;
                    inc oldint64p;
                  end;
                end
              end;

              increased_value_by:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
                begin
                  if ((not percentage) and (int64p^=int64p^+int64value)) or
                     (percentage and (int64p^>=oldint64p^+trunc(oldint64p^*(int64value/100)))) then
                  begin
                    foundaddress[found]:=dword(int64p);
                    foundvalue6[found]:=int64p^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue6)^,8*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(int64p);
                  inc(oldint64p);
                end
                else
                for j:=0 to memoryregion[i].memorysize-8 do
                begin
                  if ((not percentage) and (int64p^=int64p^+int64value)) or
                     (percentage and (int64p^>=oldint64p^+trunc(oldint64p^*(int64value/100)))) then
                  begin
                    foundaddress[found]:=dword(int64p);
                    foundvalue6[found]:=int64p^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue6)^,8*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc int64p;
                    inc oldint64p;
                  end;
                end
              end;

              decreased_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
                begin
                  if int64p^<oldint64p^ then
                  begin
                    foundaddress[found]:=dword(int64p);
                    foundvalue6[found]:=int64p^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue6)^,8*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(int64p);
                  inc(oldint64p);
                end
                else
                for j:=0 to memoryregion[i].memorysize-8 do
                begin
                  if int64p^<oldint64p^ then
                  begin
                    foundaddress[found]:=dword(int64p);
                    foundvalue6[found]:=int64p^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue6)^,8*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc int64p;
                    inc oldint64p;
                  end;
                end;
              end;

              decreased_value_by:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
                begin
                  if ((not percentage) and (int64p^=int64p^-int64value)) or
                     (percentage and (int64p^<=oldint64p^-trunc(oldint64p^*(int64value/100)))) then
                  begin
                    foundaddress[found]:=dword(int64p);
                    foundvalue6[found]:=int64p^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue6)^,8*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  inc(int64p);
                  inc(oldint64p);
                end
                else
                for j:=0 to memoryregion[i].memorysize-8 do
                begin
                  if ((not percentage) and (int64p^=int64p^-int64value)) or
                     (percentage and (int64p^<=oldint64p^-trunc(oldint64p^*(int64value/100)))) then
                  begin
                    foundaddress[found]:=dword(int64p);
                    foundvalue6[found]:=int64p^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue6)^,8*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc int64p
                    inc oldint64p
                  end;
                end;
              end;

              changed_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
                begin
                  if int64p^<>oldint64p^ then
                  begin
                    foundaddress[found]:=dword(int64p);
                    foundvalue6[found]:=int64p^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue6)^,8*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(int64p);
                  inc(oldint64p);
                end
                else
                for j:=0 to memoryregion[i].memorysize-8 do
                begin
                  if int64p^<>oldint64p^ then
                  begin
                    foundaddress[found]:=dword(int64p);
                    foundvalue6[found]:=int64p^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue6)^,8*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc int64p;
                    inc oldint64p;
                  end;
                end;
              end;

              unchanged_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
                begin
                  if int64p^=oldint64p^ then
                  begin
                    foundaddress[found]:=dword(int64p);
                    foundvalue6[found]:=int64p^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue6)^,8*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(int64p);
                  inc(oldint64p);
                end
                else
                for j:=0 to memoryregion[i].memorysize-8 do
                begin
                  if int64p^=oldint64p^ then
                  begin
                    foundaddress[found]:=dword(int64p);
                    foundvalue6[found]:=int64p^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue6)^,8*number,actualwrite);
                      found:=0;
                    end;
                  end;
                  asm
                    inc int64p;
                    inc oldint64p;
                  end;
                end;
              end;
            end;
          except

          end;

          postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
        end;

        blockwrite(newaddressfile,pointer(foundaddress)^,4*found,actualwrite);
        blockwrite(newmemoryfile,pointer(foundvalue6)^,4*found,actualwrite);
        result:=filesize(NewMemoryfile) div 8;
      end;

    end
    else
    begin //compare with what is stored in memory
      setlength(foundaddress,number);

      if valuetype=0 then //byte  (no fastscan)
      begin
        setlength(foundvalue1,number);
        for i:=0 to memoryregions do
        begin
          bytep:=pointer(memoryregion[i].BaseAddress);
          oldbytep:=pointer(dword(memory)+position);
          inc(position,memoryregion[i].MemorySize);
          try
            case scantype of
              increased_value:
              begin
                for j:=0 to memoryregion[i].MemorySize-1 do
                begin
                  if bytep^>oldbytep^ then
                  begin
                    foundaddress[found]:=dword(bytep);
                    foundvalue1[found]:=bytep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue1)^,number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(bytep);
                  inc(oldbytep);
                end;
              end;

              increased_value_by:
              begin
                for j:=0 to memoryregion[i].MemorySize-1 do
                begin
                  if ((not percentage) and (bytep^=oldbytep^+bytevalue)) or
                     (percentage and (bytep^>=oldbytep^+trunc(oldbytep^*(bytevalue/100)))) then
                  begin
                    foundaddress[found]:=dword(bytep);
                    foundvalue1[found]:=bytep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue1)^,number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(bytep);
                  inc(oldbytep);
                end;
              end;

              decreased_value:
              begin
                for j:=0 to memoryregion[i].MemorySize-1 do
                begin
                  if bytep^<oldbytep^ then
                  begin
                    foundaddress[found]:=dword(bytep);
                    foundvalue1[found]:=bytep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue1)^,number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(bytep);
                  inc(oldbytep);
                end;
              end;

              decreased_value_by:
              begin
                outputdebugstring('decreased valye by');
                for j:=0 to memoryregion[i].MemorySize-1 do
                begin
                  if ((not percentage) and (bytep^=oldbytep^-bytevalue)) or
                     (percentage and (bytep^<=oldbytep^-trunc(oldbytep^*(bytevalue/100)))) then
                  begin
                    foundaddress[found]:=dword(bytep);
                    foundvalue1[found]:=bytep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue1)^,number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(bytep);
                  inc(oldbytep);
                end;
              end;

              changed_value:
              begin
                for j:=0 to memoryregion[i].MemorySize-1 do
                begin
                  if bytep^<>oldbytep^ then
                  begin
                    foundaddress[found]:=dword(bytep);
                    foundvalue1[found]:=bytep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue1)^,number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(bytep);
                  inc(oldbytep);
                end;
              end;

              unchanged_value:
              begin
                for j:=0 to memoryregion[i].MemorySize-1 do
                begin
                  if bytep^=oldbytep^ then
                  begin
                    foundaddress[found]:=dword(bytep);
                    foundvalue1[found]:=bytep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue1)^,number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(bytep);
                  inc(oldbytep);
                end;
              end;
            end;
          except

          end;

          postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
        end;

        blockwrite(newaddressfile,pointer(foundaddress)^,4*found,actualwrite);
        blockwrite(newmemoryfile,pointer(foundvalue1)^,found,actualwrite);
        result:=filesize(NewMemoryfile);
      end else
      if valuetype=1 then //word
      begin
        setlength(foundvalue2,number);
        for i:=0 to memoryregions do
        begin
          wordp:=pointer(memoryregion[i].BaseAddress);
          oldwordp:=pointer(dword(memory)+position);
          inc(position,memoryregion[i].MemorySize);
          try
            case scantype of
              increased_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 2)-1 do
                begin
                  if wordp^>oldwordp^ then
                  begin
                    foundaddress[found]:=dword(wordp);
                    foundvalue2[found]:=wordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  inc(wordp);
                  inc(oldwordp);
                end
                else
                for j:=0 to (memoryregion[i].MemorySize)-2 do
                begin
                  if wordp^>oldwordp^ then
                  begin
                    foundaddress[found]:=dword(wordp);
                    foundvalue2[found]:=wordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  asm
                    inc wordp;
                    inc oldwordp;
                  end;
                end
              end;

              increased_value_by:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 2)-1 do
                begin
                  if ((not percentage) and (wordp^=oldwordp^+wordvalue)) or
                     (percentage and (wordp^>=oldwordp^+trunc(oldwordp^*(wordvalue/100)))) then
                  begin
                    foundaddress[found]:=dword(wordp);
                    foundvalue2[found]:=wordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(wordp);
                  inc(oldwordp);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-2 do
                begin
                  if ((not percentage) and (wordp^=oldwordp^+wordvalue)) or
                     (percentage and (wordp^>=oldwordp^+trunc(oldwordp^*(wordvalue/100)))) then
                  begin
                    foundaddress[found]:=dword(wordp);
                    foundvalue2[found]:=wordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc wordp;
                    inc oldwordp;
                  end;
                end
              end;

              decreased_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 2)-1 do
                begin
                  if wordp^<oldwordp^ then
                  begin
                    foundaddress[found]:=dword(wordp);
                    foundvalue2[found]:=wordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(wordp);
                  inc(oldwordp);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-2 do
                begin
                  if wordp^<oldwordp^ then
                  begin
                    foundaddress[found]:=dword(wordp);
                    foundvalue2[found]:=wordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc wordp;
                    inc oldwordp;
                  end;
                end;
              end;

              decreased_value_by:
              begin
              outputdebugstring('decreased value by');
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 2)-1 do
                begin
                  if ((not percentage) and (wordp^=oldwordp^-wordvalue)) or
                     (percentage and (wordp^<=oldwordp^-trunc(oldwordp^*(wordvalue/100)))) then
                  begin
                    foundaddress[found]:=dword(wordp);
                    foundvalue2[found]:=wordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  inc(wordp);
                  inc(oldwordp);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-2 do
                begin
                  if ((not percentage) and (wordp^=oldwordp^-wordvalue)) or
                     (percentage and (wordp^<=oldwordp^-trunc(oldwordp^*(wordvalue/100)))) then
                  begin
                    foundaddress[found]:=dword(wordp);
                    foundvalue2[found]:=wordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc wordp
                    inc oldwordp
                  end;
                end;
              end;

              changed_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 2)-1 do
                begin
                  if wordp^<>oldwordp^ then
                  begin
                    foundaddress[found]:=dword(wordp);
                    foundvalue2[found]:=wordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(wordp);
                  inc(oldwordp);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-2 do
                begin
                  if wordp^<>oldwordp^ then
                  begin
                    foundaddress[found]:=dword(wordp);
                    foundvalue2[found]:=wordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc wordp;
                    inc oldwordp;
                  end;
                end;
              end;

              unchanged_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 2)-1 do
                begin
                  if wordp^=oldwordp^ then
                  begin
                    foundaddress[found]:=dword(wordp);
                    foundvalue2[found]:=wordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(wordp);
                  inc(oldwordp);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-2 do
                begin
                  if wordp^=oldwordp^ then
                  begin
                    foundaddress[found]:=dword(wordp);
                    foundvalue2[found]:=wordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                      found:=0;
                    end;
                  end;
                  asm
                    inc wordp;
                    inc oldwordp;
                  end;
                end;
              end;
            end;
          except

          end;

          postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
        end;

        blockwrite(newaddressfile,pointer(foundaddress)^,4*found,actualwrite);
        blockwrite(newmemoryfile,pointer(foundvalue2)^,2*found,actualwrite);
        result:=filesize(NewMemoryfile) div 2;
      end else
      if valuetype=2 then //dword   //after an unknown initial value scan
      begin
        setlength(foundvalue3,number);
        for i:=0 to memoryregions do
        begin
          dwordp:=pointer(memoryregion[i].BaseAddress);
          olddwordp:=pointer(dword(memory)+position);
          inc(position,memoryregion[i].MemorySize);
          try
            case scantype of
              increased_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 4)-1 do
                begin
                  if dwordp^>olddwordp^ then
                  begin
                    foundaddress[found]:=dword(dwordp);
                    foundvalue3[found]:=dwordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  inc(dwordp);
                  inc(olddwordp);
                end
                else
                for j:=0 to (memoryregion[i].MemorySize)-4 do
                begin
                  if dwordp^>olddwordp^ then
                  begin
                    foundaddress[found]:=dword(dwordp);
                    foundvalue3[found]:=dwordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  asm
                    inc dwordp;
                    inc olddwordp;
                  end;
                end
              end;

              increased_value_by:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 4)-1 do
                begin
                  if ((not percentage) and (dwordp^=olddwordp^+dwordvalue)) or
                     (percentage and (dwordp^>=olddwordp^+trunc(olddwordp^*(dwordvalue/100)))) then
                  begin
                    foundaddress[found]:=dword(dwordp);
                    foundvalue3[found]:=dwordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(dwordp);
                  inc(olddwordp);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-4 do
                begin
                  if ((not percentage) and (dwordp^=olddwordp^+dwordvalue)) or
                     (percentage and (dwordp^>=olddwordp^+trunc(olddwordp^*(dwordvalue/100)))) then
                  begin
                    foundaddress[found]:=dword(dwordp);
                    foundvalue3[found]:=dwordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc dwordp;
                    inc olddwordp;
                  end;
                end
              end;

              decreased_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 4)-1 do
                begin
                  if dwordp^<olddwordp^ then
                  begin
                    foundaddress[found]:=dword(dwordp);
                    foundvalue3[found]:=dwordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(dwordp);
                  inc(olddwordp);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-4 do
                begin
                  if dwordp^<olddwordp^ then
                  begin
                    foundaddress[found]:=dword(dwordp);
                    foundvalue3[found]:=dwordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc dwordp;
                    inc olddwordp;
                  end;
                end;
              end;

              decreased_value_by:
              begin
              outputdebugstring('decreased value by');
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 4)-1 do
                begin
                  if ((not percentage) and (dwordp^=olddwordp^-dwordvalue)) or
                     (percentage and (dwordp^<=olddwordp^-trunc(olddwordp^*(dwordvalue/100)))) then
                  begin
                    foundaddress[found]:=dword(dwordp);
                    foundvalue3[found]:=dwordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  inc(dwordp);
                  inc(olddwordp);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-4 do
                begin
                  if ((not percentage) and (dwordp^=olddwordp^-dwordvalue)) or
                     (percentage and (dwordp^<=olddwordp^-trunc(olddwordp^*(dwordvalue/100)))) then
                  begin
                    foundaddress[found]:=dword(dwordp);
                    foundvalue3[found]:=dwordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc dwordp
                    inc olddwordp
                  end;
                end;
              end;

              changed_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 4)-1 do
                begin
                  if dwordp^<>olddwordp^ then
                  begin
                    foundaddress[found]:=dword(dwordp);
                    foundvalue3[found]:=dwordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(dwordp);
                  inc(olddwordp);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-4 do
                begin
                  if dwordp^<>olddwordp^ then
                  begin
                    foundaddress[found]:=dword(dwordp);
                    foundvalue3[found]:=dwordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc dwordp;
                    inc olddwordp;
                  end;
                end;
              end;

              unchanged_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 4)-1 do
                begin
                  if dwordp^=olddwordp^ then
                  begin
                    foundaddress[found]:=dword(dwordp);
                    foundvalue3[found]:=dwordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(dwordp);
                  inc(olddwordp);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-4 do
                begin
                  if dwordp^=olddwordp^ then
                  begin
                    foundaddress[found]:=dword(dwordp);
                    foundvalue3[found]:=dwordp^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;
                  end;
                  asm
                    inc dwordp;
                    inc olddwordp;
                  end;
                end;
              end;
            end;
          except

          end;

          postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
        end;

        blockwrite(newaddressfile,pointer(foundaddress)^,4*found,actualwrite);
        blockwrite(newmemoryfile,pointer(foundvalue3)^,4*found,actualwrite);
        result:=filesize(NewMemoryfile) div 4;
      end else
      if valuetype=3 then //float
      begin

        setlength(foundvalue4,number);
        for i:=0 to memoryregions do
        begin
          singlep:=pointer(memoryregion[i].BaseAddress);
          oldsinglep:=pointer(dword(memory)+position);
          inc(position,memoryregion[i].MemorySize);
          try
            case scantype of
              increased_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 4)-1 do
                begin
                  if (not (isnan(singlep^) or isinfinite(singlep^))) and (SingleP^>Oldsinglep^)
                  then
                  begin
                    foundaddress[found]:=dword(singlep);
                    foundvalue4[found]:=singlep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  inc(singlep);
                  inc(oldsinglep);
                end
                else
                for j:=0 to (memoryregion[i].MemorySize)-4 do
                begin
                  if (not (isnan(singlep^) or isinfinite(singlep^))) and (SingleP^>Oldsinglep^) then
                  begin
                    foundaddress[found]:=dword(singlep);
                    foundvalue4[found]:=singlep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  asm
                    inc singlep;
                    inc oldsinglep;
                  end;
                end
              end;

              increased_value_by:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 4)-1 do
                begin
                  helpsingle:=RoundTo(SingleP^,-decim);
                  helpsingle2:=RoundTo(OldSingleP^,-decim);
                  if (not (isnan(singlep^) or isinfinite(singlep^))) and
                  (
                  ((not percentage) and (helpsingle=helpsingle2+singlevalue))
                  or
                  (percentage and (singlep^>=oldsinglep^+oldsinglep^*(singlevalue/100)))
                  )
                  
                  then
                  begin
                    foundaddress[found]:=dword(singlep);
                    foundvalue4[found]:=singlep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(singlep);
                  inc(oldsinglep);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-4 do
                begin
                  helpsingle:=RoundTo(SingleP^,-decim);
                  helpsingle2:=RoundTo(OldSingleP^,-decim);
                  if (not (isnan(singlep^) or isinfinite(singlep^))) and
                  (
                  ((not percentage) and (helpsingle=helpsingle2+singlevalue))
                  or
                  (percentage and (singlep^>=oldsinglep^+oldsinglep^*(singlevalue/100)))
                  ) then
                  begin
                    foundaddress[found]:=dword(singlep);
                    foundvalue4[found]:=singlep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc singlep;
                    inc oldsinglep;
                  end;
                end
              end;

              decreased_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 4)-1 do
                begin
                  if (not (isnan(singlep^) or isinfinite(singlep^))) and (SingleP^<OldSingleP^) then
                  begin
                    foundaddress[found]:=dword(singlep);
                    foundvalue4[found]:=singlep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(singlep);
                  inc(oldsinglep);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-4 do
                begin
                  if (not (isnan(singlep^) or isinfinite(singlep^))) and (SingleP^<OldSingleP^) then
                  begin
                    foundaddress[found]:=dword(singlep);
                    foundvalue4[found]:=singlep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc singlep;
                    inc oldsinglep;
                  end;
                end;
              end;

              decreased_value_by:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 4)-1 do
                begin
                  helpsingle:=RoundTo(SingleP^,-decim);
                  helpsingle2:=RoundTo(OldSingleP^,-decim);
                  if (not (isnan(singlep^) or isinfinite(singlep^))) and
                  (
                  ((not percentage) and (helpsingle=helpsingle2-singlevalue))
                  or
                  (percentage and (singlep^<=oldsinglep^-oldsinglep^*(singlevalue/100)))
                  ) then
                  begin
                    foundaddress[found]:=dword(singlep);
                    foundvalue4[found]:=singlep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  inc(singlep);
                  inc(oldsinglep);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-4 do
                begin
                  helpsingle:=RoundTo(SingleP^,-decim);
                  helpsingle2:=RoundTo(OldSingleP^,-decim);
                  if (not (isnan(singlep^) or isinfinite(singlep^))) and
                  (
                  ((not percentage) and (helpsingle=helpsingle2-singlevalue))
                  or
                  (percentage and (singlep^<=oldsinglep^-oldsinglep^*(singlevalue/100)))
                  ) then
                  begin
                    foundaddress[found]:=dword(singlep);
                    foundvalue4[found]:=singlep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc singlep
                    inc oldsinglep
                  end;
                end;
              end;

              changed_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 4)-1 do
                begin
                  if (not (isnan(singlep^) or isinfinite(singlep^))) and (SingleP^<>OldSingleP^) then
                  begin
                    foundaddress[found]:=dword(singlep);
                    foundvalue4[found]:=singlep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(singlep);
                  inc(oldsinglep);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-4 do
                begin
                  if (not (isnan(singlep^) or isinfinite(singlep^))) and (SingleP^<>OldSingleP^) then
                  begin
                    foundaddress[found]:=dword(singlep);
                    foundvalue4[found]:=singlep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc singlep;
                    inc oldsinglep;
                  end;
                end;
              end;

              unchanged_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 4)-1 do
                begin
                  if (not (isnan(singlep^) or isinfinite(singlep^))) and (SingleP^=OldSingleP^) then
                  begin
                    foundaddress[found]:=dword(singlep);
                    foundvalue4[found]:=singlep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(singlep);
                  inc(oldsinglep);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-4 do
                begin
                  if (not (isnan(singlep^) or isinfinite(singlep^))) and (SingleP^=OldSingleP^) then
                  begin
                    foundaddress[found]:=dword(singlep);
                    foundvalue4[found]:=singlep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                      found:=0;
                    end;
                  end;
                  asm
                    inc singlep;
                    inc oldsinglep;
                  end;
                end;
              end;
            end;
          except

          end;

          postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
        end;

        blockwrite(newaddressfile,pointer(foundaddress)^,4*found,actualwrite);
        blockwrite(newmemoryfile,pointer(foundvalue4)^,4*found,actualwrite);
        result:=filesize(NewMemoryfile) div 4;
      end else
      if valuetype=4 then //double
      begin
        setlength(foundvalue5,number);
        for i:=0 to memoryregions do
        begin
          doublep:=pointer(memoryregion[i].BaseAddress);
          olddoublep:=pointer(dword(memory)+position);
          inc(position,memoryregion[i].MemorySize);
          try
            case scantype of
              increased_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
                begin
                  if (not (isnan(doublep^) or isinfinite(doublep^))) and (doubleP^>olddoublep^) then
                  begin
                    foundaddress[found]:=dword(doublep);
                    foundvalue5[found]:=doublep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  inc(doublep);
                  inc(olddoublep);
                end
                else
                for j:=0 to (memoryregion[i].MemorySize)-8 do
                begin
                  if (not (isnan(doublep^) or isinfinite(doublep^))) and (doubleP^>Olddoublep^) then
                  begin
                    foundaddress[found]:=dword(doublep);
                    foundvalue5[found]:=doublep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  asm
                    inc doublep;
                    inc olddoublep;
                  end;
                end
              end;

              increased_value_by:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
                begin
                  helpdouble:=RoundTo(doubleP^,-decim);
                  helpdouble2:=RoundTo(OlddoubleP^,-decim);
                  if (not (isnan(doublep^) or isinfinite(doublep^))) and
                  (
                  ((not percentage) and (helpdouble=helpdouble2+doublevalue))
                  or
                  (percentage and (doublep^>=olddoublep^+olddoublep^*(doublevalue/100)))
                  ) then
                  begin
                    foundaddress[found]:=dword(doublep);
                    foundvalue5[found]:=doublep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(doublep);
                  inc(olddoublep);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-8 do
                begin
                  helpdouble:=RoundTo(doubleP^,-decim);
                  helpdouble2:=RoundTo(OlddoubleP^,-decim);
                  if (not (isnan(doublep^) or isinfinite(doublep^))) and
                  (
                  ((not percentage) and (helpdouble=helpdouble2+doublevalue))
                  or
                  (percentage and (doublep^>=olddoublep^+olddoublep^*(doublevalue/100)))
                  ) then
                  begin
                    foundaddress[found]:=dword(doublep);
                    foundvalue5[found]:=doublep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc doublep;
                    inc olddoublep;
                  end;
                end
              end;

              decreased_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
                begin
                  if (not (isnan(doublep^) or isinfinite(doublep^))) and (doubleP^<OlddoubleP^) then
                  begin
                    foundaddress[found]:=dword(doublep);
                    foundvalue5[found]:=doublep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(doublep);
                  inc(olddoublep);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-8 do
                begin
                  if (not (isnan(doublep^) or isinfinite(doublep^))) and (doubleP^<OlddoubleP^) then
                  begin
                    foundaddress[found]:=dword(doublep);
                    foundvalue5[found]:=doublep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue5)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc doublep;
                    inc olddoublep;
                  end;
                end;
              end;

              decreased_value_by:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
                begin
                  helpdouble:=RoundTo(doubleP^,-decim);
                  helpdouble2:=RoundTo(OlddoubleP^,-decim);
                  if (not (isnan(doublep^) or isinfinite(doublep^))) and
                  (
                  ((not percentage) and (helpdouble=helpdouble2-doublevalue))
                  or
                  (percentage and (doublep^<=olddoublep^-olddoublep^*(doublevalue/100)))
                  ) then
                  begin
                    foundaddress[found]:=dword(doublep);
                    foundvalue5[found]:=doublep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  inc(doublep);
                  inc(olddoublep);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-8 do
                begin
                  helpdouble:=RoundTo(doubleP^,-decim);
                  helpdouble2:=RoundTo(OlddoubleP^,-decim);
                  if (not (isnan(doublep^) or isinfinite(doublep^))) and
                  (
                  ((not percentage) and (helpdouble=helpdouble2-doublevalue))
                  or
                  (percentage and (doublep^<=olddoublep^-olddoublep^*(doublevalue/100)))
                  ) then
                  begin
                    foundaddress[found]:=dword(doublep);
                    foundvalue5[found]:=doublep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc doublep
                    inc olddoublep
                  end;
                end;
              end;

              changed_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
                begin
                  if (not (isnan(doublep^) or isinfinite(doublep^))) and (doubleP^<>OlddoubleP^) then
                  begin
                    foundaddress[found]:=dword(doublep);
                    foundvalue5[found]:=doublep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(doublep);
                  inc(olddoublep);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-8 do
                begin
                  if (not (isnan(doublep^) or isinfinite(doublep^))) and (doubleP^<>OlddoubleP^) then
                  begin
                    foundaddress[found]:=dword(doublep);
                    foundvalue5[found]:=doublep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc doublep;
                    inc olddoublep;
                  end;
                end;
              end;

              unchanged_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
                begin
                  if (not (isnan(doublep^) or isinfinite(doublep^))) and (doubleP^=OlddoubleP^) then
                  begin
                    foundaddress[found]:=dword(doublep);
                    foundvalue5[found]:=doublep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue5)^,4*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(doublep);
                  inc(olddoublep);
                end
                else
                for j:=0 to memoryregion[i].MemorySize-8 do
                begin
                  if (not (isnan(doublep^) or isinfinite(doublep^))) and (doubleP^=OlddoubleP^) then
                  begin
                    foundaddress[found]:=dword(doublep);
                    foundvalue5[found]:=doublep^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                      found:=0;
                    end;
                  end;
                  asm
                    inc doublep;
                    inc olddoublep;
                  end;
                end;
              end;
            end;
          except

          end;

          postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
        end;

        blockwrite(newaddressfile,pointer(foundaddress)^,4*found,actualwrite);
        blockwrite(newmemoryfile,pointer(foundvalue5)^,8*found,actualwrite);
        result:=filesize(NewMemoryfile) div 8;
      end else //no unknown for 5 (bit)
      if valuetype=6 then //int64
      begin
        setlength(foundvalue4,number);
        for i:=0 to memoryregions do
        begin
          doublep:=pointer(memoryregion[i].BaseAddress);
          olddoublep:=pointer(dword(memory)+position);
          inc(position,memoryregion[i].MemorySize);
          try
            case scantype of
              increased_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
                begin
                  if int64p^>oldint64p^ then
                  begin
                    foundaddress[found]:=dword(int64p);
                    foundvalue6[found]:=int64p^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue6)^,8*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  inc(int64p);
                  inc(oldint64p);
                end
                else
                for j:=0 to (memoryregion[i].MemorySize)-4 do
                begin
                  if int64p^>oldint64p^ then
                  begin
                    foundaddress[found]:=dword(int64p);
                    foundvalue6[found]:=int64p^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue6)^,8*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  asm
                    inc int64p;
                    inc oldint64p;
                  end;
                end
              end;

              increased_value_by:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
                begin
                  if ((not percentage) and (int64p^=oldint64p^+int64value)) or
                     (percentage and (int64p^>=oldint64p^+trunc(oldint64p^*(int64value/100)))) then
                  begin
                    foundaddress[found]:=dword(int64p);
                    foundvalue6[found]:=int64p^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue6)^,8*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(int64p);
                  inc(oldint64p);
                end
                else
                for j:=0 to memoryregion[i].memorysize-8 do
                begin
                  if ((not percentage) and (int64p^=oldint64p^+int64value)) or
                     (percentage and (int64p^>=oldint64p^+trunc(oldint64p^*(int64value/100)))) then
                  begin
                    foundaddress[found]:=dword(int64p);
                    foundvalue6[found]:=int64p^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue6)^,8*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc int64p;
                    inc oldint64p;
                  end;
                end
              end;

              decreased_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
                begin
                  if int64p^<oldint64p^ then
                  begin
                    foundaddress[found]:=dword(int64p);
                    foundvalue6[found]:=int64p^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue6)^,8*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(int64p);
                  inc(oldint64p);
                end
                else
                for j:=0 to memoryregion[i].memorysize-8 do
                begin
                  if int64p^<oldint64p^ then
                  begin
                    foundaddress[found]:=dword(int64p);
                    foundvalue6[found]:=int64p^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue6)^,8*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc int64p;
                    inc oldint64p;
                  end;
                end;
              end;

              decreased_value_by:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
                begin
                  if ((not percentage) and (int64p^=oldint64p^-int64value)) or
                     (percentage and (int64p^<=oldint64p^-trunc(oldint64p^*(int64value/100)))) then
                  begin
                    foundaddress[found]:=dword(int64p);
                    foundvalue6[found]:=int64p^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue6)^,8*number,actualwrite);
                      found:=0;
                    end;
                  end;

                  inc(int64p);
                  inc(oldint64p);
                end
                else
                for j:=0 to memoryregion[i].memorysize-8 do
                begin
                  if ((not percentage) and (int64p^=oldint64p^-int64value)) or
                     (percentage and (int64p^<=oldint64p^-trunc(oldint64p^*(int64value/100)))) then
                  begin
                    foundaddress[found]:=dword(int64p);
                    foundvalue6[found]:=int64p^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue6)^,8*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc int64p
                    inc oldint64p
                  end;
                end;
              end;

              changed_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
                begin
                  if int64p^<>oldint64p^ then
                  begin
                    foundaddress[found]:=dword(int64p);
                    foundvalue6[found]:=int64p^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue6)^,8*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(int64p);
                  inc(oldint64p);
                end
                else
                for j:=0 to memoryregion[i].memorysize-8 do
                begin
                  if int64p^<>oldint64p^ then
                  begin
                    foundaddress[found]:=dword(int64p);
                    foundvalue6[found]:=int64p^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue6)^,8*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  asm
                    inc int64p;
                    inc oldint64p;
                  end;
                end;
              end;

              unchanged_value:
              begin
                if fastscan then
                for j:=0 to (memoryregion[i].MemorySize div 8)-1 do
                begin
                  if int64p^=oldint64p^ then
                  begin
                    foundaddress[found]:=dword(int64p);
                    foundvalue6[found]:=int64p^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue6)^,8*number,actualwrite);
                      found:=0;
                    end;

                  end;

                  inc(int64p);
                  inc(oldint64p);
                end
                else
                for j:=0 to memoryregion[i].memorysize-8 do
                begin
                  if int64p^=oldint64p^ then
                  begin
                    foundaddress[found]:=dword(int64p);
                    foundvalue6[found]:=int64p^;
                    inc(found);

                    if found=number then
                    begin
                      blockwrite(newaddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      blockwrite(newmemoryfile,pointer(foundvalue6)^,8*number,actualwrite);
                      found:=0;
                    end;
                  end;
                  asm
                    inc int64p;
                    inc oldint64p;
                  end;
                end;
              end;
            end;
          except

          end;

          postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
        end;

        blockwrite(newaddressfile,pointer(foundaddress)^,4*found,actualwrite);
        blockwrite(newmemoryfile,pointer(foundvalue6)^,4*found,actualwrite);
        result:=filesize(NewMemoryfile) div 8;
      end;
    end;
  end
  else
  begin

    if valuetype<>5 then
      postmessage(scansettings^.formscanningHandle,ESSetProgressbarMax,(filesize(addressfile)-7) div 4,(filesize(addressfile)-7) div 4)
    else
      postmessage(scansettings^.formscanningHandle,ESSetProgressbarMax,(filesize(addressfile)-7) div 8,(filesize(addressfile)-7) div 8);

    //normal results
    setlength(foundaddress,number);
    setlength(scanaddress,number);

    actualread:=0;
    blockread(addressfile,datatype,sizeof(datatype),actualread);
    if actualread<>sizeof(datatype) then raise exception.Create('NOOOO. DAMNIT');
    if valuetype=0 then //byte (no fastscan, but that doesnt matter)
    begin
      setlength(foundvalue1,number);
      setlength(previousvalue1,number);
      actualread:=number*4;
      while actualread=(number*4) do
      begin
        blockread(addressfile,pointer(scanaddress)^,number*4,actualread);
        inc(position,actualread);

        if scantype=exact_value then //no old memory compare
        begin
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if pbyte(scanaddress[i])^=bytevalue then
              begin
                foundaddress[found]:=scanaddress[i];
                inc(found);
                if found=number then
                begin
                  if not FoundIsFilled then
                  begin
                    for k:=0 to number-1 do foundvalue1[k]:=bytevalue;
                    FoundIsFilled:=true;
                  end;

                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue1)^,number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=biggerthan then //no old memory compare
        begin
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if pbyte(scanaddress[i])^>bytevalue then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue1[found]:=pbyte(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue1)^,number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=smallerthan then //no old memory compare
        begin
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if pbyte(scanaddress[i])^<bytevalue then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue1[found]:=pbyte(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue1)^,number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=valuebetween then //no old memory compare
        begin
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if (pbyte(scanaddress[i])^>=bytevalue) and (pbyte(scanaddress[i])^<=bytevalue2) then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue1[found]:=pbyte(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue1)^,number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=increased_value then
        begin
          blockread(memoryfile,pointer(previousvalue1)^,actualread div 4,actualread2);
          for i:=0 to actualread2-1 do
          begin
            try
              if pbyte(scanaddress[i])^>previousvalue1[i] then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue1[found]:=pbyte(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue1)^,number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=increased_value_by then
        begin
          blockread(memoryfile,pointer(previousvalue1)^,actualread div 4,actualread2);
          for i:=0 to actualread2-1 do
          begin
            try
              if ((not percentage) and (pbyte(scanaddress[i])^=previousvalue1[i]+bytevalue)) or
                 (percentage and (pbyte(scanaddress[i])^>=previousvalue1[i]+trunc(previousvalue1[i]*(bytevalue/100))))
              then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue1[found]:=pbyte(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue1)^,number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=decreased_value then
        begin
          blockread(memoryfile,pointer(previousvalue1)^,actualread div 4,actualread2);
          for i:=0 to actualread2-1 do
          begin
            try
              if pbyte(scanaddress[i])^<previousvalue1[i] then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue1[found]:=pbyte(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue1)^,number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=decreased_value_by then
        begin
          blockread(memoryfile,pointer(previousvalue1)^,actualread div 4,actualread2);
          for i:=0 to actualread2-1 do
          begin
            try
              if ((not percentage) and (pbyte(scanaddress[i])^=previousvalue1[i]-bytevalue)) or
                 (percentage and (pbyte(scanaddress[i])^<=previousvalue1[i]-trunc(previousvalue1[i]*(bytevalue/100)))) then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue1[found]:=pbyte(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue1)^,number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=changed_value then
        begin
          blockread(memoryfile,pointer(previousvalue1)^,actualread div 4,actualread2);
          for i:=0 to actualread2-1 do
          begin
            try
              if pbyte(scanaddress[i])^<>previousvalue1[i] then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue1[found]:=pbyte(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue1)^,number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=unchanged_value then
        begin
          blockread(memoryfile,pointer(previousvalue1)^,actualread div 4,actualread2);
          for i:=0 to actualread2-1 do
          begin
            try
              if pbyte(scanaddress[i])^=previousvalue1[i] then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue1[found]:=pbyte(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue1)^,number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;

      if (scantype=exact_value) and (not FoundIsFilled) then
      begin
        for k:=0 to number-1 do foundvalue1[k]:=bytevalue;
        FoundIsFilled:=true;
      end;
      blockwrite(newAddressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(newMemoryfile,pointer(foundvalue1)^,found,actualwrite);
      result:=filesize(newMemoryfile);
    end else
    if valuetype=1 then //word
    begin
      setlength(foundvalue2,number);
      setlength(previousvalue2,number);
      actualread:=number*4;
      while actualread=(number*4) do
      begin
        blockread(addressfile,pointer(scanaddress)^,number*4,actualread);
        inc(position,actualread);

        if scantype=exact_value then //no old memory compare
        begin
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if pword(scanaddress[i])^=wordvalue then
              begin
                foundaddress[found]:=scanaddress[i];
                inc(found);
                if found=number then
                begin
                  if not FoundIsFilled then
                  begin
                    for k:=0 to number-1 do foundvalue2[k]:=wordvalue;
                    FoundIsFilled:=true;
                  end;

                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=biggerthan then //no old memory compare
        begin
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if pword(scanaddress[i])^>wordvalue then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue2[found]:=pword(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=smallerthan then //no old memory compare
        begin
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if pword(scanaddress[i])^<wordvalue then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue2[found]:=pword(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=valuebetween then //no old memory compare
        begin
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if (pword(scanaddress[i])^>=wordvalue) and (pword(scanaddress[i])^<=wordvalue2) then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue2[found]:=pword(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=increased_value then
        begin
          blockread(memoryfile,pointer(previousvalue2)^,(actualread div 2),actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if pword(scanaddress[i])^>previousvalue2[i] then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue2[found]:=pword(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=increased_value_by then
        begin
          blockread(memoryfile,pointer(previousvalue2)^,(actualread div 2),actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if ((not percentage) and (pword(scanaddress[i])^=previousvalue2[i]+wordvalue)) or
                 (percentage and (pword(scanaddress[i])^>=previousvalue2[i]+trunc(previousvalue2[i]*(wordvalue/100)))) then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue2[found]:=pword(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=decreased_value then
        begin
          blockread(memoryfile,pointer(previousvalue2)^,(actualread div 2),actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if pword(scanaddress[i])^<previousvalue2[i] then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue2[found]:=pword(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=decreased_value_by then
        begin
          blockread(memoryfile,pointer(previousvalue2)^,(actualread div 2),actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if ((not percentage) and (pword(scanaddress[i])^=previousvalue2[i]-wordvalue)) or
                 (percentage and (pword(scanaddress[i])^<=previousvalue2[i]-trunc(previousvalue2[i]*(wordvalue/100)))) then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue2[found]:=pword(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=changed_value then
        begin
          blockread(memoryfile,pointer(previousvalue2)^,(actualread div 2),actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if pword(scanaddress[i])^<>previousvalue2[i] then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue2[found]:=pword(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=unchanged_value then
        begin
          blockread(memoryfile,pointer(previousvalue2)^,(actualread div 2),actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if pword(scanaddress[i])^=previousvalue2[i] then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue2[found]:=pword(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue2)^,2*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;

      if (scantype=exact_value) and (not FoundIsFilled) then
      begin
        for k:=0 to number-1 do foundvalue2[k]:=wordvalue;
        FoundIsFilled:=true;
      end;
      blockwrite(newAddressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(newMemoryfile,pointer(foundvalue2)^,2*found,actualwrite);
      result:=filesize(newMemoryfile) div 2;
    end else
    if valuetype=2 then //dword
    begin
      setlength(foundvalue3,number);
      setlength(previousvalue3,number);

      actualread:=number*4;
      while actualread=(number*4) do
      begin
        blockread(addressfile,pointer(scanaddress)^,number*4,actualread);
        inc(position,actualread);

        if scantype=exact_value then //no old memory compare
        begin
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if pdword(scanaddress[i])^=dwordvalue then
              begin
                foundaddress[found]:=scanaddress[i];
                inc(found);
                if found=number then
                begin
                  if not FoundIsFilled then
                  begin
                    for k:=0 to number-1 do foundvalue3[k]:=dwordvalue;
                    FoundIsFilled:=true;
                  end;

                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=biggerthan then //no old memory compare
        begin
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if pdword(scanaddress[i])^>dwordvalue then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue3[found]:=pdword(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=smallerthan then //no old memory compare
        begin
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if pdword(scanaddress[i])^<dwordvalue then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue3[found]:=pdword(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=valuebetween then //no old memory compare
        begin
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if (pdword(scanaddress[i])^>=dwordvalue) and (pdword(scanaddress[i])^<=dwordvalue2) then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue3[found]:=pdword(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=increased_value then
        begin
          blockread(memoryfile,pointer(previousvalue3)^,actualread,actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if pdword(scanaddress[i])^>previousvalue3[i] then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue3[found]:=pdword(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=increased_value_by then
        begin
          blockread(memoryfile,pointer(previousvalue3)^,actualread,actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if ((not percentage) and (pdword(scanaddress[i])^=previousvalue3[i]+dwordvalue)) or
                 (percentage and (pdword(scanaddress[i])^>=previousvalue3[i]+trunc(previousvalue3[i]*(dwordvalue/100)))) then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue3[found]:=pdword(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=decreased_value then
        begin
          blockread(memoryfile,pointer(previousvalue3)^,actualread,actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if pdword(scanaddress[i])^<previousvalue3[i] then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue3[found]:=pdword(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=decreased_value_by then
        begin
          blockread(memoryfile,pointer(previousvalue3)^,actualread,actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if ((not percentage) and (pdword(scanaddress[i])^=previousvalue3[i]-dwordvalue)) or
                 (percentage and (pdword(scanaddress[i])^<=previousvalue3[i]-trunc(previousvalue3[i]*(dwordvalue/100)))) then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue3[found]:=pdword(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=changed_value then
        begin
          blockread(memoryfile,pointer(previousvalue3)^,actualread,actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if pdword(scanaddress[i])^<>previousvalue3[i] then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue3[found]:=pdword(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=unchanged_value then
        begin
          blockread(memoryfile,pointer(previousvalue3)^,actualread,actualread2);
          if actualread<>actualread2 then messagebox(0,'read error','Cheat Engine Hyperscan Error',mb_ok);

          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if pdword(scanaddress[i])^=previousvalue3[i] then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue3[found]:=pdword(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue3)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;

      if (scantype=exact_value) and (not FoundIsFilled) then
      begin
        for k:=0 to number-1 do foundvalue3[k]:=dwordvalue;
        FoundIsFilled:=true;
      end;
      blockwrite(newAddressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(newMemoryfile,pointer(foundvalue3)^,4*found,actualwrite);
      result:=filesize(newMemoryfile) div 4;
    end else
    if valuetype=3 then //single (float)
    begin
      setlength(foundvalue4,number);
      setlength(previousvalue4,number);
      actualread:=number*4;

      if decim=0 then helpsingle3:=1 else
        helpsingle3:=1/((decim)*10);  //the range for extremerounded scans

      while actualread=(number*4) do
      begin
        blockread(addressfile,pointer(scanaddress)^,number*4,actualread);
        inc(position,actualread);

        if scantype=exact_value then //no old memory compare
        begin
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              helpsingle:=psingle(scanaddress[i])^;

              check:=(not (isnan(helpsingle) or isinfinite(helpsingle)));

              if check then
              case roundingtype of
                rounded:
                begin
                  helpsingle:=RoundTo(helpsingle,-decim);
                  check:=(helpsingle=SingleValue);
                end;

                extremerounded:
                begin
                  //if a scan for 1 it scans for    0<x<2
                  //if a scan for 1.0 it scans for  9.9<x<1.10
                  check:=((helpsingle<(singlevalue+helpsingle3)) and (helpsingle>(singlevalue-helpsingle3)) );
                end;

                truncated:
                begin
                  //if a scan for 1 it scans for    1>=x<2
                  //if a scan for 1.0 it scans for 1.0>=x<1.10
                  check:=((helpsingle<(singlevalue+helpsingle3)) and (helpsingle>=singlevalue));
                end;

                else check:=false;
              end;

              if check then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue4[found]:=helpsingle;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=biggerthan then
        begin
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              helpsingle:=psingle(scanaddress[i])^;
              if (not (isnan(helpsingle) or isinfinite(helpsingle))) and (helpsingle>singlevalue) then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue4[found]:=psingle(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=smallerthan then //no old memory compare
        begin
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              helpsingle:=psingle(scanaddress[i])^;
              if (not (isnan(helpsingle) or isinfinite(helpsingle))) and (helpsingle<singlevalue) then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue4[found]:=psingle(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=valuebetween then //no old memory compare
        begin
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              helpsingle:=psingle(scanaddress[i])^;
              if (not (isnan(helpsingle) or isinfinite(helpsingle))) and (helpsingle>=singlevalue) and (helpsingle<=singlevalue2) then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue4[found]:=psingle(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=increased_value then
        begin
          blockread(memoryfile,pointer(previousvalue4)^,actualread,actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if psingle(scanaddress[i])^>previousvalue4[i] then
              begin
                helpsingle:=psingle(scanaddress[i])^;
                if (isnan(helpsingle) or isinfinite(helpsingle)) then continue;

                foundaddress[found]:=scanaddress[i];
                foundvalue4[found]:=helpsingle;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=increased_value_by then
        begin
          blockread(memoryfile,pointer(previousvalue4)^,actualread,actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              helpsingle:=psingle(scanaddress[i])^;
              if (not (isnan(helpsingle) or isinfinite(helpsingle))) and
              (
                ((not percentage) and (RoundTo(helpsingle,-decim)=roundto(previousvalue4[i]+singlevalue,-decim)))
               or
                (percentage and (helpsingle>=previousvalue4[i]+previousvalue4[i]*(singlevalue/100)))

              ) then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue4[found]:=helpsingle;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=decreased_value then
        begin
          blockread(memoryfile,pointer(previousvalue4)^,actualread,actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if psingle(scanaddress[i])^<previousvalue4[i] then
              begin
                helpsingle:=psingle(scanaddress[i])^;
                if (isnan(helpsingle) or isinfinite(helpsingle)) then continue;

                foundaddress[found]:=scanaddress[i];
                foundvalue4[found]:=psingle(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=decreased_value_by then
        begin
          blockread(memoryfile,pointer(previousvalue4)^,actualread,actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if (not (isnan(helpsingle) or isinfinite(helpsingle))) and
              (
                ((not percentage) and (RoundTo(helpsingle,-decim)=roundto(previousvalue4[i]-singlevalue,-decim)))
               or
                (percentage and (helpsingle<=previousvalue4[i]-previousvalue4[i]*(singlevalue/100)))

              ) then
              if (not (isnan(helpsingle) or isinfinite(helpsingle))) and (RoundTo(helpsingle,-decim)=roundto(previousvalue4[i]-singlevalue,-decim)) then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue4[found]:=psingle(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=changed_value then
        begin
          blockread(memoryfile,pointer(previousvalue4)^,actualread,actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if psingle(scanaddress[i])^<>previousvalue4[i] then
              begin
                helpsingle:=psingle(scanaddress[i])^;
                if (isnan(helpsingle) or isinfinite(helpsingle)) then continue;

                foundaddress[found]:=scanaddress[i];
                foundvalue4[found]:=psingle(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else

        if scantype=unchanged_value then
        begin
          blockread(memoryfile,pointer(previousvalue4)^,actualread,actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if psingle(scanaddress[i])^=previousvalue4[i] then
              begin
                helpsingle:=psingle(scanaddress[i])^;
                if (isnan(helpsingle) or isinfinite(helpsingle)) then continue;

                foundaddress[found]:=scanaddress[i];
                foundvalue4[found]:=psingle(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue4)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;

      blockwrite(newAddressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(newMemoryfile,pointer(foundvalue4)^,4*found,actualwrite);
      result:=filesize(newMemoryfile) div 4;
    end else
    if valuetype=4 then //double
    begin
      setlength(foundvalue5,number);
      setlength(previousvalue5,number);
      
      actualread:=number*4;
      while actualread=(number*4) do
      begin
        blockread(addressfile,pointer(scanaddress)^,number*4,actualread);
        inc(position,actualread);

        if scantype=exact_value then //no old memory compare
        begin
          if decim=0 then helpdouble3:=1 else
            helpdouble3:=1/((decim)*10);  //the range for extremerounded scans

          for i:=0 to (actualread div 4)-1 do
          begin
            try
              helpdouble:=pdouble(scanaddress[i])^;

              check:=(not (isnan(helpdouble) or isinfinite(helpdouble)));

              if check then
              case roundingtype of
                rounded:
                begin
                  helpdouble:=RoundTo(helpdouble,-decim);
                  check:=(helpdouble=doubleValue);
                end;

                extremerounded:
                begin
                  //if a scan for 1 it scans for    0<x<2
                  //if a scan for 1.0 it scans for  9.9<x<1.10
                  check:=((helpdouble<(doublevalue+helpdouble3)) and (helpdouble>(doublevalue-helpdouble3)) );
                end;

                truncated:
                begin
                  //if a scan for 1 it scans for    1>=x<2
                  //if a scan for 1.0 it scans for 1.0>=x<1.10
                  check:=((helpdouble<(doublevalue+helpdouble3)) and (helpdouble>=doublevalue));
                end;

                else check:=false;
              end;

              if check then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue5[found]:=helpdouble;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=biggerthan then //no old memory compare
        begin
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              helpdouble:=pdouble(scanaddress[i])^;
              if (not (isnan(helpdouble) or isinfinite(helpdouble))) and (helpdouble>doublevalue) then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue5[found]:=pdouble(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=smallerthan then //no old memory compare
        begin
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              helpdouble:=pdouble(scanaddress[i])^;
              if (not (isnan(helpdouble) or isinfinite(helpdouble))) and (helpdouble<doublevalue) then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue5[found]:=pdouble(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=valuebetween then //no old memory compare
        begin
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              helpdouble:=pdouble(scanaddress[i])^;
              if (not (isnan(helpdouble) or isinfinite(helpdouble))) and (helpdouble>=doublevalue) and (helpdouble<=doublevalue2) then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue5[found]:=pdouble(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end

        else
        if scantype=increased_value then
        begin
          blockread(memoryfile,pointer(previousvalue5)^,actualread*2,actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if pdouble(scanaddress[i])^>previousvalue5[i] then
              begin
                helpdouble:=pdouble(scanaddress[i])^;
                if (isnan(helpdouble) or isinfinite(helpdouble)) then continue;

                foundaddress[found]:=scanaddress[i];
                foundvalue5[found]:=helpdouble;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=increased_value_by then
        begin
          blockread(memoryfile,pointer(previousvalue5)^,actualread*2,actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              helpdouble:=pdouble(scanaddress[i])^;
              if (not (isnan(helpdouble) or isinfinite(helpdouble))) and
              (
                ((not percentage) and (RoundTo(helpdouble,-decim)=roundto(previousvalue5[i]+doublevalue,-decim)))
               or
                (percentage and (helpdouble>=previousvalue5[i]+previousvalue5[i]*(doublevalue/100)))

              ) then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue5[found]:=helpdouble;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=decreased_value then
        begin
          blockread(memoryfile,pointer(previousvalue5)^,actualread*2,actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if pdouble(scanaddress[i])^<previousvalue5[i] then
              begin
                helpdouble:=pdouble(scanaddress[i])^;
                if (isnan(helpdouble) or isinfinite(helpdouble)) then continue;

                foundaddress[found]:=scanaddress[i];
                foundvalue5[found]:=pdouble(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=decreased_value_by then
        begin
          blockread(memoryfile,pointer(previousvalue5)^,actualread*2,actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if (not (isnan(helpdouble) or isinfinite(helpdouble))) and
              (
                ((not percentage) and (RoundTo(helpdouble,-decim)=roundto(previousvalue5[i]-doublevalue,-decim)))
               or
                (percentage and (helpdouble<=previousvalue5[i]-previousvalue5[i]*(doublevalue/100)))
              ) then
              if (not (isnan(helpdouble) or isinfinite(helpdouble))) and (RoundTo(helpdouble,-decim)=roundto(previousvalue5[i]-doublevalue,-decim)) then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue5[found]:=pdouble(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=changed_value then
        begin
          blockread(memoryfile,pointer(previousvalue5)^,actualread*2,actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if pdouble(scanaddress[i])^<>previousvalue5[i] then
              begin
                helpdouble:=pdouble(scanaddress[i])^;
                if (isnan(helpdouble) or isinfinite(helpdouble)) then continue;

                foundaddress[found]:=scanaddress[i];
                foundvalue5[found]:=pdouble(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else

        if scantype=unchanged_value then
        begin
          blockread(memoryfile,pointer(previousvalue5)^,actualread*2,actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if pdouble(scanaddress[i])^=previousvalue5[i] then
              begin
                helpdouble:=pdouble(scanaddress[i])^;
                if (isnan(helpdouble) or isinfinite(helpdouble)) then continue;

                foundaddress[found]:=scanaddress[i];
                foundvalue5[found]:=pdouble(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue5)^,8*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;

      blockwrite(newAddressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(newMemoryfile,pointer(foundvalue5)^,5*found,actualwrite);
      result:=filesize(newMemoryfile) div 8;
    end else
    if valuetype=5 then //bit
    begin
      inc(position,actualread);

      setlength(scanaddressb,number);
      setlength(foundaddressb,number);
      blockwrite(newmemoryfile,nrofbits,4,actualread);
      blockwrite(newmemoryfile,pointer(bitscan)^,nrofbits,actualread);

//      setlength(bytearray,(nrofbits div 8)+2);

      actualread:=number*sizeof(bitaddress);
      while actualread=number*sizeof(bitaddress) do
      begin
        blockread(addressfile,pointer(scanaddressb)^,number*sizeof(bitaddress),actualread);
        for i:=0 to (actualread div sizeof(bitaddress))-1 do
        begin
          //recalculate the address
          startbit:=scanaddressb[i].bit+scansettings.bitoffsetchange;

          while startbit<0 do
          begin
            dec(scanaddressb[i].address);
            inc(startbit,8);
          end;

          while startbit>7 do
          begin
            inc(scanaddressb[i].address);
            dec(startbit,8);
          end;

          scanaddressb[i].bit:=startbit;

//          setlength(bytearray,100);
          pointer(bytearray):=pointeR(scanaddressb[i].address);
          try
          //read the memory
        //  readprocessmemory(processhandle,pointer(scanaddressb[i].address),@bytearray[0],(1+(startbit+nrofbits) div 8),actualwrite);

          extra:=true;
          l:=0;
          for j:=0 to actualwrite-1 do
          begin
            for k:=startbit to 7 do
            begin
              if bitscan[l]<>2 then
              if getbit(k,bytearray[j])<>bitscan[l] then
              begin
                extra:=false;
                break;
              end;

              inc(l);
              if l=nrofbits then  //correct
              begin
                foundaddressb[found].address:=scanaddressb[i].address;
                foundaddressb[found].bit:=scanaddressb[i].bit;

                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddressB)^,found*(sizeof(bitaddress)),actualwrite);
                  found:=0;
                end;
              end;

            end;

            startbit:=0;

            if not extra then break;
          end;
          except

          end;
        end;
        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;
      blockwrite(newAddressfile,pointer(foundaddressB)^,found*(sizeof(bitaddress)),actualwrite);


      result:=(filesize(newaddressfile)-7) div sizeof(bitaddress);

    end else
    if valuetype=6 then //int64
    begin
      setlength(foundvalue6,number);
      setlength(previousvalue6,number);

      actualread:=number*4;
      while actualread=(number*4) do
      begin
        blockread(addressfile,pointer(scanaddress)^,number*4,actualread);
        inc(position,actualread);

        if scantype=exact_value then //no old memory compare
        begin
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if pint64(scanaddress[i])^=int64value then
              begin
                foundaddress[found]:=scanaddress[i];
                inc(found);
                if found=number then
                begin
                  if not FoundIsFilled then
                  begin
                    for k:=0 to number-1 do foundvalue6[k]:=int64value;
                    FoundIsFilled:=true;
                  end;

                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue6)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=biggerthan then //no old memory compare
        begin
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if pint64(scanaddress[i])^>int64value then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue6[found]:=pint64(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue6)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=smallerthan then //no old memory compare
        begin
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if pint64(scanaddress[i])^<int64value then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue6[found]:=pint64(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue6)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=increased_value then
        begin
          blockread(memoryfile,pointer(previousvalue6)^,actualread*2,actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if pint64(scanaddress[i])^>previousvalue6[i] then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue6[found]:=pint64(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue6)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=valuebetween then //no old memory compare
        begin
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if (pint64(scanaddress[i])^>=int64value) and (pint64(scanaddress[i])^<=int64value2) then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue6[found]:=pint64(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue6)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=increased_value_by then
        begin
          blockread(memoryfile,pointer(previousvalue6)^,actualread*2,actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if ((not percentage) and (pint64(scanaddress[i])^=previousvalue6[i]+int64value)) or
                 (percentage and (pint64(scanaddress[i])^>=previousvalue6[i]+trunc(previousvalue6[i]*(int64value/100)))) then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue6[found]:=pint64(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue6)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=decreased_value then
        begin
          blockread(memoryfile,pointer(previousvalue6)^,actualread*2,actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if pint64(scanaddress[i])^<previousvalue6[i] then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue6[found]:=pint64(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue6)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=decreased_value_by then
        begin
          blockread(memoryfile,pointer(previousvalue6)^,actualread*2,actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if ((not percentage) and (pint64(scanaddress[i])^=previousvalue6[i]-int64value)) or
                 (percentage and (pint64(scanaddress[i])^<=previousvalue6[i]-trunc(previousvalue6[i]*(int64value/100)))) then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue6[found]:=pint64(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue6)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=changed_value then
        begin
          blockread(memoryfile,pointer(previousvalue6)^,actualread*2,actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if pint64(scanaddress[i])^<>previousvalue6[i] then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue6[found]:=pint64(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue6)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end
        else
        if scantype=unchanged_value then
        begin
          blockread(memoryfile,pointer(previousvalue6)^,actualread*2,actualread2);
          for i:=0 to (actualread div 4)-1 do
          begin
            try
              if pint64(scanaddress[i])^=previousvalue6[i] then
              begin
                foundaddress[found]:=scanaddress[i];
                foundvalue6[found]:=pint64(scanaddress[i])^;
                inc(found);
                if found=number then
                begin
                  blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                  blockwrite(newMemoryfile,pointer(foundvalue6)^,4*number,actualwrite);
                  found:=0;
                end;
              end;
            except
              //not readable, but who cares
            end;
          end;
        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;

      if (scantype=exact_value) and (not FoundIsFilled) then
      begin
        for k:=0 to number-1 do foundvalue6[k]:=int64value;
        FoundIsFilled:=true;
      end;
      blockwrite(newAddressfile,pointer(foundaddress)^,4*found,actualwrite);
      blockwrite(newMemoryfile,pointer(foundvalue6)^,8*found,actualwrite);
      result:=filesize(newMemoryfile) div 8;
    end else
    if vartype=7 then //string
    begin
      dwordvalue:=length(scanvalue);
      blockwrite(Newmemoryfile,pointer(scanvalue)^,dwordvalue,actualread);

      actualread:=number*4;
      while actualread=(number*4) do
      begin
        blockread(addressfile,pointer(scanaddress)^,number*4,actualread);
        inc(position,actualread);

        for i:=0 to (actualread div 4)-1 do
        begin
          try
            if unicode then
            begin
              for j:=0 to length(scanvalue)-1 do
              begin
                if ((extra) and (WideCompareStr(pwidechar(dword(scanaddress[i]+j))^,scanvalue[j])=0)) or
                   ((not extra) and (WideCompareText(pwidechar(dword(scanaddress[i]+j))^,scanvalue[j])=0))
                then
                begin
                  if j=length(scanvalue)-1 then
                  begin
                    foundaddress[found]:=scanAddress[i];
                    inc(found);
                    if found=number then
                    begin
                      //write the currently found addresses to disk
                      blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      found:=0;
                    end;
                  end;
                end else continue;
              end;
            end
            else
            begin
              for j:=0 to length(scanvalue)-1 do
              begin
                if ((extra) and (pchar(dword(scanaddress[i]+j))^=scanvalue[j])) or
                   ((not extra) and (uppercase(pchar(dword(scanaddress[i]+j))^)=uppercase(scanvalue[j]))) then
                begin
                  if j=length(scanvalue)-1 then
                  begin
                    foundaddress[found]:=scanAddress[i];
                    inc(found);
                    if found=number then
                    begin
                      //write the currently found addresses to disk
                      blockwrite(NewAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                      found:=0;
                    end;
                  end;
                end else continue;
              end;
            end;
          except

          end;
        end;

        postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
      end;
      blockwrite(NewAddressfile,pointer(foundaddress)^,4*found,actualwrite);
      result:=(filesize(NewAddressfile)-sizeof(datatype)) div 4;
    end else
    if vartype=8 then
    begin
      setlength(scanaddress,number);

      if scantype=Exact_Value then
      begin
        if (pos('-',scanvalue)>0) or (pos(' ',scanvalue)>0) then
        begin
          //syntax is xx-xx-xx or xx xx xx
          j:=1;
          k:=0;
          scanvalue:=scanvalue+' ';
          for i:=1 to length(scanvalue) do
          begin
            if (scanvalue[i]=' ') or (scanvalue[i]='-') then
            begin
              helpstr:=copy(scanvalue,j,i-j);
              j:=i+1;
              setlength(bytes,k+1);
              try
                if extra then bytes[k]:=strtoint('$'+helpstr)
                          else bytes[k]:=strtoint(helpstr);
              except
                bytes[k]:=-1;
                //if it is not a '-' or ' ' or a valid value then I assume it is a wildcard.(
              end;
              inc(k);
            end;
          end;
        end else
        begin
          //syntax is xxxxxx
          k:=0;
          j:=1;
          for i:=1 to length(scanvalue) do
          begin
            if (i mod 2)=0 then
            begin
              helpstr:=copy(scanvalue,j,i-j+1);
              j:=i+1;
              setlength(bytes,k+1);
              try
                bytes[k]:=strtoint('$'+helpstr);
              except
                bytes[k]:=-1;
              end;
              inc(k);
            end;
          end;
        end;

        nrofbytes:=length(bytes);
        setlength(foundvalue8,(number*(nrofbytes+1)));
        blockwrite(memoryfile,nrofbytes,4,actualwrite); //save how many bytes each record is
        l:=0;


        //read the addresses
        actualread:=number*4;
        while actualread=number*4 do
        begin
          blockread(addressfile,pointer(scanaddress)^,number*4,actualread);
          inc(position,actualread);

          for i:=0 to (actualread div 4)-1 do
          begin
            try
            pointeR(bytearray):=pointer(scanaddress[i]);

            //find out if bytearray true with bytes[]
            for j:=0 to nrofbytes-1 do
            begin
              if (bytes[j]=-1) or (byte(bytes[j])=bytearray[j]) then continue;
              break;
            end;

            if dword(j)=nrofbytes then //found one
            begin
              foundaddress[found]:=scanaddress[i];
              copymemory(pointer(@foundvalue8[found*nrofbytes]),pointer(@bytearray[0]),nrofbytes);
              inc(found);

              if found=number then
              begin
                blockwrite(newAddressfile,pointer(foundaddress)^,4*number,actualwrite);
                blockwrite(newMemoryfile,pointeR(foundvalue8)^,nrofbytes*number,actualwrite);
                found:=0;
              end;
            end;
            except

            end;
          end;
          postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
        end;
        blockwrite(newAddressfile,pointer(foundaddress)^,4*found,actualwrite);
        blockwrite(newMemoryfile,pointeR(foundvalue8)^,nrofbytes*found,actualwrite);
      end;
    end;
  end;

  freememory;

  closefile(memoryfile);
  closefile(addressfile);
  closefile(newmemoryfile);
  closefile(newaddressfile);

  deletefile(pchar(scansettings^.CheatEngineDir+'Memory.UNDO'));
  deletefile(pchar(scansettings^.CheatEngineDir+'Addresses.UNDO'));

  renamefile(scansettings^.CheatEngineDir+'Memory.tmp',scansettings^.cheatenginedir+'Memory.UNDO');
  renamefile(scansettings^.CheatEngineDir+'Addresses.tmp',scansettings^.CheatEngineDir+'Addresses.UNDO');
  renamefile(scansettings^.CheatEnginedir+'ADDRESSES2.TMP',scansettings^.Cheatenginedir+'ADDRESSES.TMP');
  renamefile(scansettings^.Cheatenginedir+'MEMORY2.TMP',scansettings^.Cheatenginedir+'MEMORY.TMP');


  finally
    pointer(bytearray):=x;
  end;
end;


function UnknownInitialValueScan:integer;
var RMPointer: ^Byte;
    i: Integer;
    j: Integer;

    actualread: dword;

    //save variables
    dataType:  String[6];  //REGION or NORMAL  (Always region in this procedure)

    Total: dword;
    Max: dword;
    position: dword;
    toread: dword;
    ok: boolean;
    chk:dword;
    a: DWORD;
    b: DWORD;

    p :pbyte;
begin
  //showmessage(scansettings^.CheatEngineDir);
  LastScan:=1;
  position:=0;


  buffersize:=scansettings^.buffersize;
  datatype:='REGION';
  MemoryRegions:=0;
  total:=0;

  assignfile(Addressfile,scansettings^.CheatEngineDir+'Addresses.TMP');
  rewrite(Addressfile,1);

  assignfile(memoryfile,scansettings^.CheatEngineDir+'Memory.TMP');
  rewrite(memoryfile,1);  //just so next scan has a file to delete :)

  blockwrite(Addressfile,datatype,sizeof(datatype),chk);

  if not scansettings^.LowMemoryUsage then
  begin
    closefile(memoryfile);
    closefile(Addressfile);
  end;

  try
    try
      GetMemoryRanges([dword(@ok)]);
    except
      raise exception.Create('getmemoryexception');
    end;


//    showmessage('memoryregions='+IntToStr(memoryregions));

//    showmessage('Need to read '+IntToStr(totaltoread)+' bytes');
    freemem(memory);
    memory:=nil;

    if not scansettings^.LowMemoryUsage then
      getmem(memory,totaltoread); //reserve memory after getting the regions
  except
    on EOutOfMemory do
      begin
        //not enough free memory for the 2nd
        result:=0;
        raise exception.Create('Not enough memory');
      end;
  end;

  RMPointer:=pointer(memory);
 // blockwrite(memoryfile,pointer($00400000)^,512000,chk);
  //blockwrite(memoryfile,pointer($00400000)^,512000,chk);

  if scansettings^.LowMemoryUsage then
  begin
//    showmessage('doing a low memory useage scan');
    for i:=0 to memoryregions do
    begin
      if not scansettings^.scanning then break;

    //save the memory to disk
      chk:=0;
      inc(position,memoryregion[i].MemorySize);
      blockwrite(memoryfile,pbyte(memoryregion[i].BaseAddress)^,memoryregion[i].MemorySize,chk);
      if chk=0 then
      begin
        while memoryregion[i].MemorySize>=2048 do
        begin
          memoryregion[i].MemorySize:=memoryregion[i].MemorySize div 2;
          chk:=0;
          blockwrite(memoryfile,pointeR(memoryregion[i].BaseAddress)^,memoryregion[i].MemorySize,chk);
          if chk>0 then break;
        end;

        memoryregion[i].MemorySize:=chk;        
        if memoryregion[i].MemorySize<2048 then memoryregion[i].MemorySize:=0;
      end;

      postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);
    end;
  end
  else
  begin
//    showmessage('doing a full memory usage scan');
    for i:=0 to memoryregions do
    begin
      if not scansettings^.scanning then break;


      try
        inc(position,memoryregion[i].MemorySize);
        copymemory(RMPointer,pointer(memoryregion[i].baseAddress),memoryregion[i].MemorySize);
      except
       // showmessage('failed reading on '+IntToStr(i)+' of '+IntToStr(memoryregions));
        while memoryregion[i].MemorySize>=2048 do
        begin

          memoryregion[i].MemorySize:=memoryregion[i].MemorySize div 2;
          try
            copymemory(RMPointer,pointer(memoryregion[i].baseAddress),memoryregion[i].MemorySize);
            break;
          except

          end;
        end;

        if memoryregion[i].MemorySize<2048 then memoryregion[i].MemorySize:=0;
      end;

      inc(RMPointer,memoryregion[i].MemorySize);
      postmessage(scansettings^.formscanningHandle,ESSetProgressbarPos,position,0);

    end;
  end;

  if scansettings^.LowMemoryUsage then
  begin
    closefile(memoryfile);
    closefile(addressfile);
    Total:=0;
    For i:=0 to Memoryregions do
      inc(Total,Memoryregion[i].MemorySize);
  end
  else
    total:=dword(RMPointer)-dword(memory);

  result:=total;
  if scansettings^.fastscan then
  begin
    case scansettings^.ValueType of
      1: result:=total div 2; //word
      2,3: result:=total div 4; //dword+float
      4,6: result:=total div 8; //double+8bytes
    end;
  end;
end;

function ScanMemory:integer;
var count:dword;
    found: integer;
begin
  scansettings^.scanning:=true;
  count:=0;

  case scansettings^.scan of
    0: count:=UnknownInitialValueScan;
    1: count:=ExactValueFirstScan;
    2: count:=NextScan;
  end;

  try
    closefile(addressfile);
    closefile(memoryfile);
  except

  end;

  result:=count;
end;


procedure TScanThread.Execute;
var i,j,k:integer;
    PseudoProcessHandle: THandle;
    PseudoThreadHandle: THandle;
    ProcessHandle: THandle;
    ThreadHandle: THandle;
    oldpriority: dword;
    oldthreadPriority: integer;
    old8087CW: word;
begin
  freeonterminate:=true;
  j:=0;
  k:=1;
  old8087CW:=Get8087CW;

  try
    Set8087CW($133f);

    try
      //tell cheat engine what my thread id is, so if it wants to pause all threads, dont pause THIS one
      postmessage(scansettings^.formscanningHandle,HSThreadID,threadid,0);
      j:=ScanMemory;
      k:=0;
    except

      j:=0;
    end;
  finally
    Set8087CW(old8087CW);

    postmessage(scansettings^.formscanningHandle,donescanning,j,k);
  end;
end;

var cehookdll: thandle;

function WndProc(HWnd: HWND; Msg: UINT;
  wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var i,j,k: integer;
    f: psingle;
begin
  try

  case Msg of
    WM_DESTROY:
    begin
      PostQuitMessage(0);
      Result := 0;
    end;

    wm_user+1:    //prepare for a new scan
    begin
      try
        closefiles;
        deletefiles;
      except

      end;

      if memory<>nil then
        freemem(memory);
      memory:=nil;

      setlength(memoryregion,0);
    end;

    wm_user+2:  //scan
    begin
      try
        st:=TScanThread.Create(true);

        case scansettings^.priority of
          0: st.priority:=tpIdle;
          1: st.priority:=tpLowest;
          2: st.priority:=tpLower;
          3: st.priority:=tpLower;
          4: st.priority:=tpNormal;
          5: st.priority:=tpHigher;
          6: st.Priority:=tpHighest;
          7: st.priority:=tpTimeCritical;
        end;

        st.Resume;
      except
        postmessage(scansettings^.formscanningHandle,donescanning,0,1);
       // messagebox(0,'DAMN IT! I failed to create a thread!','Cheat Engine Hyperscan',MB_OK	);
      end;
    end;

    wm_user+3:  //cancel scan
    begin
      terminatethread(st.Handle,0);
      freememory;
      closefiles;
    end;

    wm_user+4: //speedhack
    begin
      try
        InitializeSpeedhack;
        result:=$11223344;
      except
        result:=0;
      end;
    end;

    wm_user+5: //stop speedhack
    begin
      stopspeedhack;
    end;

    wm_user+6: //set speedhack speeds
    begin
      sleeptime:=wparam;
      acceleration:=psingle(@lparam)^;
      if acceleration<=0 then acceleration:=1;
      result:=12345;
    end;

    wm_user+7: //set speedhack method.  (wparam=0:speed up  1: slow down)
    begin
      slow:=wparam=1;
    end;

    wm_user+8: //poll if the hyperscan window is enabled
    begin
    //note: add in code so it only responses to the cheat engine process
      result:=$AA11BB22;
    end;

    //direct3d hook stuff 20>
   { wm_user+20:
    begin
      //set zoomlevel
      f:=@wparam; //wparam=zoom level
      zoom:=f^;

      //updatecetexture;
    end;  }

    else
      Result := DefWindowProc(HWnd,Msg,WParam,LParam);
  end;

  except
    on e: exception do
    begin
    //something went wrong
      outputdebugstring(pchar('Something went wrong in the wndproc:'+e.Message));
      result:=0;
    end;
  end;
end;

procedure THyperscanWindow.execute;
var x:tagWNDCLASSA;
    w: thandle;
    msg: TMsg;
    i: dword;
    j:integer;
    classname: string;
    windowtitle: string;

begin
  outputdebugstring('In thread');

  hyperscanthreadID:=self.ThreadID;
  
  {$ifdef debug}
  messagebox(0,'The Thread that will create the hyperscan window has been created and is going to be executed','Cheat Engine Hyperscan',mb_ok);
  {$endif}

  randomize;
  classname:='';
  windowtitle:='';
  for j:=1 to 2+(random(20)) do
  begin
    classname:=classname+chr(32+random(64));
    windowtitle:=windowtitle+chr(32+random(64));
  end;

  zeromemory(@x,sizeof(wndclass));
  x.lpszClassName:=pchar(classname);
  x.lpfnWndProc:=@WndProc;
  x.hInstance:=hinstance;
  i:=windows.RegisterClass(x);

  outputdebugstring('createwindow');


  w:=createwindow(
    pchar(classname),
    pchar(windowtitle),
    0,
    random(100),
    random(100),
    1+random(100),
    1+random(100),
    0,
    0,
    hinstance,
    nil);
  if w=0 then messagebox(0,'Failed to create the HYPERmode window','Hypermode error',mb_ok);

  outputdebugstring('Setting hyperscanwindow handle');
  scansettings.hyperscanwindow:=w;

  outputdebugstring('Setting scanning boolean to true (indicate it is loaded');
  scansettings.scanning:=true;  

  {$ifdef debug}
  messagebox(0,pchar('Created a window. Result='+IntToStr(w)),'Cheat Engine Hyperscan',mb_ok);
  {$endif}

  {$ifdef debug}
  messagebox(0,pchar('Sending Cheat Engine the message that the window has been made with handle '+IntToStr(w)),'Cheat Engine Hyperscan',mb_ok);
  {$endif}

  ShowWindow(w,sw_hide);
  
  Updatewindow(w);

  hyperscanwindowenabled:=true;
  while GetMessage(msg,0,0,0) do
  begin
    TranslateMessage(msg);
    DispatchMessage(msg);
  end;

  unregisterclass('CEHYPERSCAN',hInstance);
  sendmessage(scansettings.mainformHandle,wm_user+4,0,0);
  try Closefiles; except end;
  try deletefiles; except end;
  try Freememory; except end;
  if memory<>nil then try Freemem(memory); except end;

  while tickerstopped<>true do sleep(100);
  stopspeedhack;

  exitthread(0);
  //fuck memory!
 // FreeLibraryAndExitThread(cehookdll,0);
end;


procedure InitializeHyperWindow;
var j,k: integer;
    cehookloc: array[0..255] of pchar;
begin
  outputdebugstring('InitializeHyperwindow');
{  try
    if scansettings=nil then InitializeScanSettings;
  except
    outputdebugstring('InitializeScansettings failed');
    exit;
  end;  }
  //loadlibrary(pchar(scansettings.CheatEngineDir+'stealth.dll'));

//  InitializeStealth;

  {$ifdef debug}
  messagebox(0,'The scansettings file mapping object has been opened','Cheat Engine Hyperscan',mb_ok);
  {$endif}

  if GetModuleFileName(hInstance,@cehookloc[0],256)=0 then
  begin
    messagebox(0,'GetModuleFileName failed','Cheat Engine Hyperscan',MB_OK);
    exit;
  end;

  {$ifdef debug}
  messagebox(0,'The Thread that will create the hyperscan window is going to be executed','Cheat Engine Hyperscan',mb_ok);
  {$endif}

  outputdebugstring('Getting cehook modulebase');
  cehookdll:=LoadLibrary(@cehookloc[0]);

  outputdebugstring('Creating hyperscanwindow');
  htw:=THyperScanWindow.Create(false);
end;

Procedure HookDirect3D;
type TInitializeDirectX_Hook=procedure;
var InitializeDirectX_Hook:TInitializeDirectX_Hook;
    DXHookdll: Thandle;
begin
  outputdebugstring('Hooking Direct3D');
  outputdebugstring(pchar('scansettings.CheatEngineDir='+scansettings.CheatEngineDir));

  DXHookdll:=LoadLibrary(pchar(scansettings.CheatEngineDir+'DXHook.dll'));
  if DXHookdll<>0 then
  begin
    Outputdebugstring('Loaded DXHook.DLL');
    InitializeDirectX_Hook:=GetProcAddress(DXHookdll,'InitializeDirectX_Hook');

    Outputdebugstring('Calling InitializeDirectX_Hook');
    InitializeDirectX_Hook;

    Outputdebugstring('Called the InitializeDirectX_Hook method and returned');
  end else outputdebugstring(pchar('DXHOOK.DLL not loaded. Lasterror='+IntToStr(GetLastError)));

end;

Procedure HookOpenGL;
var OpenGLHook: Thandle;
begin
  outputdebugstring('Hooking OpenGL');
  outputdebugstring(pchar('scansettings.CheatEngineDir='+scansettings.CheatEngineDir));

  OpenGLHook:=LoadLibrary(pchar(scansettings.CheatEngineDir+'OpenGLhook.dll'));
  if OpenGLHook<>0 then
  begin
    Outputdebugstring('Loaded DXHook.DLL');
  end else outputdebugstring(pchar('DXHOOK.DLL not loaded. Lasterror='+IntToStr(GetLastError)));

end;


Procedure EnablePacketEditor;
begin
  outputdebugstring('Enabling the Packet Editor');

end;

procedure EnableStealth;
begin
  Outputdebugstring('Enable Stealth');
  loadlibrary(pchar(scansettings.CheatEngineDir+'stealth.dll'));

end;


procedure IHWCI; //Initialize Hyper Window using Code Injection;
var crashcount: integer;
    op: dword;
    ask: boolean;

    x: ^byte;
    s: string;
    i,j: integer;
begin
  outputdebugstring('IHCWI is started');

  x:=pointer(scansettings);
  for i:=0 to 15 do
  begin
    s:='';

    for j:=0 to 15 do
    begin
      s:=s+inttohex(x^,2)+' ';
      inc(x);
    end;
    outputdebugstring(pchar(s));

  end;

  outputdebugstring('IHWCI got called');
  try
    outputdebugstring('1');
    outputdebugstring(pchar('scansettings='+inttohex(dword(scansettings),8)));
    if scansettings^.UseHyperscan then InitializeHyperWindow else outputdebugstring('UseHyperscan=false');
    outputdebugstring('2');
    if scansettings^.hookdirect3d then HookDirect3D;
    outputdebugstring('3');
    if scansettings^.HookOpenGL   then HookOpenGL;
    outputdebugstring('4');
    if scansettings^.packeteditor then EnablePacketEditor;

 // HookNewProcesses

 { crashcount:=0;
  while (crashcount<50*10) and (not hyperscanwindowenabled) do
  begin
    inc(crashcount);
    sleep(20);
  end; }



 // if not hyperscanwindowenabled then showmessage('Failed to create the window')
 // else
    outputdebugstring('5');
    if scansettings^.asktocontinue then showmessage('Press OK to start the program');
 except
   outputdebugstring('exception while reading scansettings.  ');
 end;

 outputdebugstring('no problems');

//  ExitThread(0);
end;

procedure CodeInjection_InitializeHyperscan;
begin

end;

procedure CodeInjection_InitializeDirect3DHook;
begin


end;

procedure CodeInjection_Entry;
begin
  //read config
  showmessage('Press OK to start the program');
end;

function MyHook(code: integer; wParam: word; lParam: longword): longword; stdcall;
var m:PCWPRETSTRUCT;
begin

  m:=pointer(lParam);

  if m.message=wm_user+666 then
  begin
    case m.wParam of
      $22222222: intercepttimermessages:=true;
      $33333333:
      begin
        {$ifdef debug}
        messagebox(0,'The Global Windows Hook received the initialization Message','Cheat Engine Hyperscan',mb_ok);
        {$endif}

        InitializeHyperwindow;

      end;

    end;
  end;

  CallNextHookEx(h,code,wParam,lparam);  //call the next hook proc if there is one
  myhook:=0;
end;

procedure seth(newh:HHook);
begin
  h:=newh;
end;

procedure About; stdcall;
begin
  showmessage('This dll was made for Cheat Engine by Dark Byte. For more info go to www.cheatengine.tk');
end;


//---------------------timers------------------------
type TCETimerhookdata=record
  processed: boolean;
  returnhandle: thandle;
  window: thandle;
  timerid: dword;
  timerproc: dword;
  targetprocess: dword;
end;
var CETIMERHOOKDATA: ^TCETIMERHOOKDATA;
    quitingtimerhook:boolean;
    timerhookhandle: thandle;

function MyTimerHook(code: integer; wParam: word; lParam: longword): longword; stdcall;
var m:PMSG;
begin
  m:=pointer(lParam);
  case m.message of
  wm_timer:
  begin
    if quitingtimerhook then exit;

    if cetimerhookdata=nil then
      CETIMERHOOKDATA:=MapViewOfFile(OpenFileMapping(FILE_MAP_ALL_ACCESS	,false,'CETIMERHOOKDATA'),FILE_MAP_ALL_ACCESS,0,0,0);

    if cetimerhookdata.targetprocess=getcurrentprocessid then
    begin
      while not cetimerhookdata.processed do sleep(0); //give timeslice to other processes
      CEtimerhookdata.window:=m.hwnd;
      cetimerhookdata.timerid:=m.wParam;
      cetimerhookdata.timerproc:=m.lParam;
      cetimerhookdata.processed:=false;
      postmessage(cetimerhookdata.returnhandle,wm_user+1,0,0);
    end;
  end;

  wm_user+666:
  begin
    if m.wParam=$12345678 then
    begin
      if cetimerhookdata<>nil then
        unmapviewoffile(CETIMERHOOKDATA);
        
      quitingtimerhook:=true;
    end;

    if m.wParam=$87654321 then
      timerhookhandle:=lparam;
  end;

  end;

  CallNextHookEx(timerhookhandle,code,wParam,lparam);  //call the next hook proc if there is one
  result:=0;
end;

exports MyHook;
exports MyTimerHook;
exports seth;
exports IHWCI;
exports About;

//var cehookloc: array[0..255] of pchar;
var i: integer;
begin
  memory:=nil;
  intercepttimermessages:=false;
  cetimerhookdata:=0;
  quitingtimerhook:=false;
  winmmlib:=0;
  acceleration:=1;
  sleeptime:=3;
  tickerstopped:=true;
  hyperscanwindowenabled:=false;
end.
