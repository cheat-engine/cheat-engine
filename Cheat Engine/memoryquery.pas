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

uses {$ifdef windows}networkInterface, networkInterfaceApi,{$endif} ProcessHandlerUnit;

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
  {$ifdef jni}
  result:=ReadProcessMemory(processhandle, pointer(address), @x, 1, br);
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


        if abs(ptrInt(x-base))<abs(ptrInt(ptrUint(result)-base)) then
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

initialization
  {$ifdef jni}
  systeminfo.lpMinimumApplicationAddress:=$8000;
  systeminfo.lpMaximumApplicationAddress:=$7fffffffffffffff;
  systeminfo.dwAllocationGranularity:=0;
  {$endif}

end.

