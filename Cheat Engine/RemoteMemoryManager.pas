unit RemoteMemoryManager;

{
this unit will contain a class that will be used to control remotely allocated memory blocks.
Specifically designed for smaller allocations. allocate at least systeminfo.dwAllocationGranularity on each real alloc  (64kb)

Speed is not of essence here. Assumed it that only up to 8KB of memory will be used at MOST

}

{$mode delphi}

interface

uses
  {$ifdef darwin}
  macport,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}
  syncobjs, Classes, SysUtils, cefuncproc, newkernelhandler;

type
  TAllocs=record  //record describing each little allocated block of memory
    address: ptruint;
    size: dword;
  end;
  PAllocs=^TAllocs;

type
  TAllocatedRegion=record
    remotebase: ptruint; //size is not stored, that is systeminfo.dwAllocationGranularity
    allocs: Tlist; //sorted list of allocations
  end;
  PAllocatedRegion=^TAllocatedRegion;

type
  TRemoteMemoryManager=class(tobject)
  private
    cs: TCriticalsection;
    allocatedRegions: Tlist;
    bigAllocs: Tlist;
    function findFreeRegion(size: dword): ptruint;
    procedure addNewAlloc(address: ptruint; size: integer);
  public

    function alloc(size: dword):ptruint;
    procedure dealloc(address: ptruint);
    constructor create;
    destructor destroy; override;
  end;

implementation

uses ProcessHandlerUnit;

procedure TRemoteMemoryManager.addNewAlloc(address: ptruint; size: integer);
var i,j: integer;
  a,b: PAllocs;
  ar: PAllocatedRegion;
begin
  getmem(a, sizeof(TAllocs));
  a.address:=address;
  a.size:=size;

  for i:=0 to allocatedRegions.count-1 do
  begin
    ar:=PAllocatedRegion(allocatedRegions[i]);
    if InRangeQ(address, ar.remotebase, ar.remotebase+systeminfo.dwAllocationGranularity) then
    begin
      for j:=0 to ar.allocs.Count-1 do
      begin
        b:=PAllocs(ar.allocs[j]);
        if b.address>address then //insert the address before this one
        begin
          ar.allocs.Insert(j,a);
          exit;  //inserted the alloc data. leave now
        end;
      end;

      //still here, add it to the end
      ar.allocs.Add(a);
      break;
    end;
  end;
end;

function TRemoteMemoryManager.findFreeRegion(size: dword): ptruint;
var i,j: integer;
  currentTry: ptruint;
  a: PAllocs;
  ar: PAllocatedRegion;
begin
  result:=0;

  //scan the regions for a gap of at least size bytes
  for i:=0 to allocatedRegions.count-1 do
  begin
    ar:=PAllocatedRegion(allocatedRegions[i]);
    currentTry:=ar.remotebase;
    for j:=0 to ar.allocs.count-1 do
    begin
      a:=PAllocs(ar.allocs[j]);    //keep in mind: sorted list
      if ((currentTry+size)>=a.address) then //too big to be used here
        currentTry:=qword(a.address)+a.size  //not usable, try the next block
      else //this block seems usable
        break; //this block seems usable

    end;

    //check if we're not outside of the allocated region
    if (currentTry+size)<(ar.remotebase+ systeminfo.dwAllocationGranularity) then  //found a usable block that falls within a allocated region
    begin
      result:=currentTry;
      break;
    end;
  end;


end;

function TRemoteMemoryManager.alloc(size: dword):ptruint;
var i: integer;
  ar: PAllocatedRegion;
begin
  result:=0;
  if size=0 then exit;

  //make it so size is always at least dividable by 4 (means min size is 4)

  i:=size mod 4;
  if i>0 then
    size:=size + (4-i);


  if size>=systeminfo.dwAllocationGranularity then //can't be handled with the small handler
  begin
    result:=ptruint(VirtualAllocEx(processhandle,nil,size,MEM_COMMIT or MEM_RESERVE,PAGE_EXECUTE_READWRITE));
    if result<>0 then
    begin
      cs.enter;
      bigAllocs.Add(pointer(result));
      cs.leave;
    end;
  end
  else
  begin
    //small enough
    cs.enter;
    try
      result:=findFreeRegion(size);

      if result=0 then
      begin
        //no free region found
        result:=ptruint(VirtualAllocEx(processhandle,nil,systeminfo.dwAllocationGranularity,MEM_COMMIT or MEM_RESERVE,PAGE_EXECUTE_READWRITE));
        if result<>0 then
        begin
          ar:=getmem(sizeof(TAllocatedRegion));
          ar.remotebase:=result;
          ar.allocs:=tlist.create;
          allocatedRegions.Add(ar);
        end;
      end;

      addNewAlloc(result, size);


    finally
      cs.leave;
    end;
  end;

end;

procedure TRemoteMemoryManager.dealloc(address: ptruint);
var i,j: integer;
  ar: PAllocatedRegion;
  b: PAllocs;
begin
  cs.enter;
  try
    for i:=0 to allocatedRegions.count-1 do
    begin
      ar:=PAllocatedRegion(allocatedRegions[i]);
      if InRangeQ(address, ar.remotebase, ar.remotebase+systeminfo.dwAllocationGranularity) then
      begin
        for j:=0 to ar.allocs.Count-1 do
        begin
          b:=PAllocs(ar.allocs[j]);
          if address=b.address then //insert the address before this one
          begin
            //found it
            FreeMemAndNil(b);
            ar.allocs.Delete(j);

            if ar.allocs.Count=0 then //completely freed this region
            begin
              virtualfreeex(processhandle,pointer(ar.remotebase),0,MEM_RELEASE);
              freeandnil(ar.allocs);
              FreeMemAndNil(ar);
              allocatedRegions.Delete(i);
            end;

            exit;
          end;
        end;
      end;
    end;

    //not found, check the big alloc region
    i:=bigallocs.IndexOf(pointer(address));
    if i<>-1 then
    begin
      virtualfreeex(processhandle,pointer(address),0,MEM_RELEASE);
      bigallocs.Delete(i);
    end;

  finally
    cs.leave;
  end;
end;

constructor TRemoteMemoryManager.create;
begin
  cs:=TCriticalsection.create;
  allocatedRegions:=tlist.create;
  bigallocs:=Tlist.create;
end;

destructor TRemoteMemoryManager.destroy;
var i,j: integer;
begin
  //don't dealloc the memory, but do free the structures allocated here
  for i:=0 to allocatedRegions.count-1 do
  begin
    for j:=0 to PAllocatedRegion(allocatedRegions[i]).allocs.Count-1 do
      FreeMem(PAllocatedRegion(allocatedRegions[i]).allocs[j]);

    PAllocatedRegion(allocatedRegions[i]).allocs.Free;
    FreeMem(allocatedRegions[i]);
  end;

  freeandnil(allocatedRegions);
  freeandnil(bigallocs);
  freeandnil(cs);
end;

end.

