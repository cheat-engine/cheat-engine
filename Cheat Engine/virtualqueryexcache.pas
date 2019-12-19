unit VirtualQueryExCache;
{
Caching system that could be useful in some slow lookup/emulation situations
For windows there's no good way to 'cache' the VQE call, but on linux where
normally the full /proc/pid/map file is parsed for every call this can speed
things up considerably
}

{$mode objfpc}{$H+}

interface

uses
  {$ifdef JNI}
  Classes, SysUtils, ctypes,syncobjs, newkernelhandler, unixporthelper;
  {$else}
  {$ifdef darwin}
  macport,
  {$endif}
  {$ifdef windows}
  jwawindows, windows,
  {$endif} Classes, SysUtils, newkernelhandler, math;
  {$endif}

type
  TVirtualQueryExCache=class
  private
    lastAccessed: integer;
    fhandle: THandle;
    regions: TList; //list of sorted TMEMORYBASICINFORMATION entries
  public
    function getRegion(BaseAddress: ptruint; out mbi: TMEMORYBASICINFORMATION): boolean;
    procedure AddRegion(mbi: TMemoryBasicInformation);
    constructor create(phandle: THandle);
    destructor destroy; override;
    property Handle: THandle read fHandle;
  end;

implementation

destructor  TVirtualQueryExCache.destroy;
begin
  if regions<>nil then
    freeandnil(regions);

  inherited destroy;
end;

constructor TVirtualQueryExCache.create(phandle: THandle);
begin
  fhandle:=phandle;
  lastAccessed:=-1;

  regions:=tlist.create;
end;

function TVirtualQueryExCache.getRegion(BaseAddress: ptruint; out mbi: TMEMORYBASICINFORMATION): boolean;
var
  i: integer;

//usually vqe accesses are sequential so check the next one fist (if there is one)
begin
  result:=false;
  if regions.count>0 then
  begin

    for i:=lastAccessed+1 to regions.count-1 do
    begin
      if BaseAddress<ptruint(PMEMORYBASICINFORMATION(regions[i])^.BaseAddress) then break;

      if BaseAddress>=ptruint(PMEMORYBASICINFORMATION(regions[i])^.BaseAddress) then
      begin
        if baseaddress<ptruint(PMEMORYBASICINFORMATION(regions[i])^.BaseAddress)+PMEMORYBASICINFORMATION(regions[i])^.RegionSize then
        begin
          mbi:=PMEMORYBASICINFORMATION(regions[i])^;

          mbi.BaseAddress:=pointer(ptruint(baseaddress) and qword($fffffffffffff000));
          result:=true;
          lastAccessed:=i;
          exit;
        end;
      end;
    end;

    if lastAccessed<>-1 then
    begin
      for i:=lastAccessed downto 0 do
      begin
        if BaseAddress>ptruint(PMEMORYBASICINFORMATION(regions[i])^.BaseAddress)+PMEMORYBASICINFORMATION(regions[i])^.RegionSize then break;  //won't be found

        if BaseAddress>=ptruint(PMEMORYBASICINFORMATION(regions[i])^.BaseAddress) then
        begin
          if baseaddress<ptruint(PMEMORYBASICINFORMATION(regions[i])^.BaseAddress)+PMEMORYBASICINFORMATION(regions[i])^.RegionSize then
          begin
            mbi:=PMEMORYBASICINFORMATION(regions[i])^;
            mbi.BaseAddress:=pointer(ptruint(baseaddress) and qword($fffffffffffff000));
            result:=true;
            lastAccessed:=i;
            exit;
          end;
        end;
      end;

    end;

    //fallback... find the closest region (should not happen when properly implemented)
    for i:=regions.count-1 downto 0 do
    begin
      if baseaddress>ptruint(PMEMORYBASICINFORMATION(regions[i])^.BaseAddress) then
      begin
        mbi:=PMEMORYBASICINFORMATION(regions[i])^;

        if baseaddress>ptruint(mbi.BaseAddress)+mbi.RegionSize then
        begin
          //overshot it. That means it's not in the list
          if i=regions.count-1 then //the last item in the list was too small. Mark it as the end
            exit;

          mbi.BaseAddress:=pointer(ptruint(baseaddress) and qword($fffffffffffff000));
          if i>0 then
            mbi.AllocationBase:=pointer(ptruint(PMEMORYBASICINFORMATION(regions[i-1])^.BaseAddress)+PMEMORYBASICINFORMATION(regions[i-1])^.RegionSize)
          else
            mbi.AllocationBase:=nil;

          if i<regions.count-1 then
          begin
            mbi.RegionSize:=ptruint(PMEMORYBASICINFORMATION(regions[i])^.BaseAddress)-ptruint(mbi.BaseAddress);
            result:=true;
          end;

          exit;
        end
        else
        begin
          mbi.BaseAddress:=pointer(ptruint(baseaddress) and qword($fffffffffffff000));
          result:=true;
          lastAccessed:=i;
          exit;
        end;
      end;
    end;

  end;
end;

procedure TVirtualQueryExCache.AddRegion(mbi: TMemoryBasicInformation);
var
  p: PMEMORYBASICINFORMATION;
  i: integer;
begin
  getmem(p, sizeof(TMemoryBasicInformation));
  p^:=mbi;

  if (regions.Count>0) and (ptruint(PMEMORYBASICINFORMATION(regions[regions.count-1])^.BaseAddress)>ptruint(mbi.BaseAddress)) then
  begin

    //this should never happen
  //    freemem(p);  raise exception.create('AddRegion called with an region bigger than the previously added region');
    //but just in case add some support
    for i:=0 to regions.count-1 do
      if ptruint(PMEMORYBASICINFORMATION(regions[regions.count-1])^.BaseAddress)>ptruint(mbi.BaseAddress) then
      begin
        regions.Insert(i,p);
        exit;
      end;


  end
  else
    regions.add(p);
end;

end.

