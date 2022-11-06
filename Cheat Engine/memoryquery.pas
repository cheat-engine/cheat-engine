unit MemoryQuery;

{$mode objfpc}{$H+}

interface

{$ifdef jni}
uses
  Classes, SysUtils, unixporthelper, NewKernelHandler;
{$endif}

{$ifdef windows}
uses
  Classes, SysUtils, windows, CEFuncProc, NewKernelHandler;
{$endif}

{$ifdef darwin}
uses
  Classes, SysUtils, macport, CEFuncProc, NewKernelHandler;
{$endif}

function FindFreeBlockForRegion(base: ptrUint; size: dword): pointer;
function isAddress(address: ptrUint):boolean;
function isExecutableAddress(address: ptrUint):boolean;

implementation

uses networkInterface, networkInterfaceApi, ProcessHandlerUnit;

{$ifdef jni}
var
  systeminfo:record
    lpMinimumApplicationAddress: ptruint;
    lpMaximumApplicationAddress: ptruint;
    dwAllocationGranularity: ptruint;
  end;
{$endif}


function isAddress(address: ptruint): boolean;
var
{$ifdef jni}
    x: byte;
    br: ptruint;
{$endif}
  mbi: TMemoryBasicInformation;
begin
  if address=0 then exit(false);
  {$ifdef jni}
  exit(ReadProcessMemory(processhandle, pointer(address), @x, 1, br));
  {$endif}

  result:=false;
  if VirtualQueryEx(processhandle, pointer(address), mbi, sizeof(mbi))>0 then
    result:=(mbi.State=MEM_COMMIT);// and (mbi.AllocationProtect<>PAGE_NOACCESS);
end;

function isExecutableAddress(address: ptrUint):boolean;
var mbi: TMemoryBasicInformation;
begin
  result:=false;
  if VirtualQueryEx(processhandle, pointer(address), mbi, sizeof(mbi))>0 then
    result:=(mbi.State=MEM_COMMIT) and (((mbi.Protect and PAGE_EXECUTE)=PAGE_EXECUTE) or ((mbi.Protect and PAGE_EXECUTE_READ)=PAGE_EXECUTE_READ) or ((mbi.Protect and PAGE_EXECUTE_READWRITE)=PAGE_EXECUTE_READWRITE) or ((mbi.Protect and PAGE_EXECUTE_WRITECOPY)=PAGE_EXECUTE_WRITECOPY) );
end;

{$ifdef darwin}
function CheckIfFree(var base: ptruint; neededSize: dword; increasedirection: boolean): boolean;
//scans for a free region
var
  mbi: TMemoryBasicInformation;
  newbase: ptruint;

  starttime: qword;
  tries: integer;
begin
  if (neededsize and $fff)>0 then
    neededsize:=(neededsize+$1000) and (not $fff);

  ZeroMemory(@mbi,sizeof(mbi));
  starttime:=gettickcount64;
  tries:=0;
  OutputDebugString(format('CheckIfFree(%16x, %x)',[base, neededsize]));

  while VirtualQueryEx(processhandle,pointer(base),mbi,sizeof(mbi))=sizeof(mbi) do
  begin
    if increasedirection then
      OutputDebugString(format('  +inc: %16x %d, %d ',[ptruint(mbi.AllocationBase), mbi.RegionSize, mbi.State]))
    else
      OutputDebugString(format('  -dec: %16x %d, %d',[ptruint(mbi.AllocationBase), mbi.regionsize, mbi.State]));

    if mbi.RegionSize=0 then
    begin
      outputdebugstring('vqe error');
      exit(false); //vqe implementation error
    end;

    if (mbi.State=MEM_FREE) and (mbi.RegionSize>=neededsize) then
    begin
      //match
      if tries=0 then
      begin
        base:=ptruint(mbi.BaseAddress);
        outputdebugstring(format('  match.  Tries =0 so returning baseaddress %16x',[base]));

      end
      else
      begin

        if increasedirection then
          base:=ptruint(mbi.AllocationBase)
        else
          base:=ptruint(mbi.AllocationBase)+mbi.RegionSize-neededsize;   //set pointer to the end of this region

        outputdebugstring(format('    match. Tries=%d. Returning %16x', [tries, base]));
      end;


      exit(true);
    end;

    if increasedirection then
    begin
      newbase:=ptruint(mbi.AllocationBase)+mbi.RegionSize;
      if newbase<base then
      begin
        OutputDebugString('newbase<base');
        exit(false); //overflow
      end;
    end
    else
    begin
      newbase:=ptruint(mbi.AllocationBase)-systeminfo.dwAllocationGranularity;
      if newbase>base then
      begin
        outputdebugstring('newbase>base');
        exit(false); //underflow
      end;
    end;

    if newbase=base then exit(false); //vqe implementation error

    base:=newbase;
    ZeroMemory(@mbi,sizeof(mbi));

    if (tries>50) and (gettickcount64>starttime+2000) then
    begin
      outputdebugstring('giving up');
      exit(false);
    end;
    inc(tries);
  end;

end;

function FindFreeBlockForRegion(base: ptrUint; size: dword): pointer;
{
Query the memory arround base to find an empty block that is at least 'size' big
}
var
  mbi: TMemoryBasicInformation;
  x: ptrUint;
  offset: ptrUint;

  b,oldb: ptrUint;

  minAddress,maxAddress: ptrUint;

  {$ifdef windows}
  c: TCEConnection;
  {$endif}

  found: boolean;
  left, right: ptruint;

  rightok, leftok: boolean;
begin

  //todo: Do some network specific stuff

  OutputDebugString(format('FindFreeBlockForRegion(%16x, %x)',[base,size]));

  result:=nil;
 // if not processhandler.is64Bit then exit; //don't bother

  //64-bit

  if base=0 then exit;

  minAddress:=base-$70000000; //let's add in some extra overhead to skip the last fffffff
  maxAddress:=base+$70000000;

  if processhandler.is64Bit then
  begin
    {$ifdef windows}
    if getConnection<>nil then
    begin
      minAddress:=$8000;
      maxAddress:=$7fffffffffffffff;
    end
    else
    {$endif}
    begin
      if (minAddress>ptrUint(systeminfo.lpMaximumApplicationAddress)) or (minAddress<ptrUint(systeminfo.lpMinimumApplicationAddress)) then
        minAddress:=ptrUint(systeminfo.lpMinimumApplicationAddress);

      if (maxAddress<ptrUint(systeminfo.lpMinimumApplicationAddress)) or (maxAddress>ptrUint(systeminfo.lpMaximumApplicationAddress)) then
        maxAddress:=ptrUint(systeminfo.lpMaximumApplicationAddress);
    end;
  end
  else
  begin
    minaddress:=$10000;
    maxaddress:=$fffffffff;
  end;


  if (systeminfo.dwAllocationGranularity=0) or processhandler.isNetwork then
    systeminfo.dwAllocationGranularity:=4096;

  b:=minAddress;

  OutputDebugString(format('minaddress=%.16x',[minaddress]));
  OutputDebugString(format('maxaddress=%.16x',[maxaddress]));


  right:=base and (not $fff);
  left:=right-$1000;

  rightok:=checkiffree(right,size,true);
  if rightok and (right>maxaddress) then rightok:=false;


  leftok:=checkiffree(left,size,false);
  if leftok and (left<minaddress) then leftok:=false;

  if leftok then OutputDebugString(format('left=%.16x',[left]));
  if rightok then OutputDebugString(format('right=%.16x',[right]));

  if rightok and leftok then
  begin
    base:=specialize ifthen<ptruint>(((right-base)<(base-left)),right, left);
    OutputDebugString(format('returning %.16x',[base]));
    exit(pointer(base));
  end;

  if rightok then exit(pointer(right));
  if leftok then exit(pointer(left));
  exit(pointer(base)); //unlike windows, mac will not fail and allocate near the given address, so in a way this whole routine is pointless

end;
{$else}
//todo: test the mac version of this on windows. Might work better
function FindFreeBlockForRegion(base: ptrUint; size: dword): pointer;
{
Query the memory arround base to find an empty block that is at least 'size' big
}
var
  mbi: TMemoryBasicInformation;
  x: ptrUint;
  offset: ptrUint;

  b,oldb: ptrUint;

  minAddress,maxAddress: ptrUint;

  {$ifdef windows}
  c: TCEConnection;
  {$endif}
begin

  //todo: Do some network specific stuff

  result:=nil;
 // if not processhandler.is64Bit then exit; //don't bother

  //64-bit

  if base=0 then exit;

  if base>$70000000 then
    minAddress:=base-$70000000
  else
    minAddress:=$10000;

  if base+$70000000>base then
    maxAddress:=base+$70000000
  else
    maxAddress:=ptruint($ffffffffffff0000);

  if processhandler.is64Bit then
  begin
    {$ifdef windows}
    if getConnection<>nil then
    begin
      minAddress:=$8000;
      maxAddress:=$7fffffffffffffff;
    end
    else
    {$endif}
    begin
      if (minAddress>ptrUint(systeminfo.lpMaximumApplicationAddress)) or (minAddress<ptrUint(systeminfo.lpMinimumApplicationAddress)) then
        minAddress:=ptrUint(systeminfo.lpMinimumApplicationAddress);

      if (maxAddress<ptrUint(systeminfo.lpMinimumApplicationAddress)) or (maxAddress>ptrUint(systeminfo.lpMaximumApplicationAddress)) then
        maxAddress:=ptrUint(systeminfo.lpMaximumApplicationAddress);
    end;
  end
  else
  begin
    minaddress:=$10000;
    maxaddress:=$fffffffff;
  end;


  if processhandler.isNetwork then
    systeminfo.dwAllocationGranularity:=4096;

  b:=minAddress;


  ZeroMemory(@mbi,sizeof(mbi));
  while VirtualQueryEx(processhandle,pointer(b),mbi,sizeof(mbi))=sizeof(mbi) do
  begin
    if mbi.BaseAddress>pointer(maxAddress) then exit; //no memory found, just return 0 and let windows decide

    if (mbi.State=MEM_FREE) and ((mbi.RegionSize)>size) then
    begin
      if (ptrUint(mbi.baseaddress) mod systeminfo.dwAllocationGranularity)>0 then
      begin
        //the whole size can not be used
        x:=ptrUint(mbi.baseaddress);
        offset:=systeminfo.dwAllocationGranularity - (x mod systeminfo.dwAllocationGranularity);

        //check if there's enough left
        if (mbi.regionsize-offset)>=size then
        begin
          //yes
          x:=x+offset;

          if x<base then
          begin
            x:=x+(mbi.regionsize-offset)-size;
            if x>base then x:=base;

            //now decrease x till it's alligned properly
            x:=x-(x mod systeminfo.dwAllocationGranularity);
          end;

          //if the difference is closer then use that
          if result=nil then
            result:=pointer(x)
          else
            if abs(ptrInt(x-base))<abs(ptrInt(ptrUint(result)-base)) then
              result:=pointer(x);
        end;
        //nope

      end
      else
      begin
        x:=ptrUint(mbi.BaseAddress);
        if x<base then //try to get it the closest possible (so to the end of the region-size and aligned by dwAllocationGranularity)
        begin
          x:=(x+mbi.RegionSize)-size;
          if x>base then x:=base;

          //now decrease x till it's alligned properly
          x:=x-(x mod systeminfo.dwAllocationGranularity);
        end;

        if result=nil then
          result:=pointer(x)
        else
        if (abs(ptrInt(x-base))<abs(ptrInt(ptrUint(result)-base))) then
          result:=pointer(x);
      end;

    end;

    if (mbi.regionsize mod systeminfo.dwAllocationGranularity)>0 then
      mbi.RegionSize:=mbi.regionsize+(systeminfo.dwAllocationGranularity-(mbi.regionsize mod systeminfo.dwAllocationGranularity));


    oldb:=b;
    b:=ptrUint(mbi.BaseAddress)+mbi.RegionSize;

    if b>maxAddress then exit;
    if oldb>b then exit; //overflow
  end;

end;
{$endif}


initialization
  {$ifdef jni}
  systeminfo.lpMinimumApplicationAddress:=$8000;
  systeminfo.lpMaximumApplicationAddress:=$7fffffffffffffff;
  systeminfo.dwAllocationGranularity:=4096;
  {$endif}

end.

