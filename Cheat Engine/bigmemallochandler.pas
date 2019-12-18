unit bigmemallochandler;

{$MODE Delphi}


{
This unit will provide a class (TBigMemoryAllocHandler) that will allocate more
memory than it actually needs and just returns pointers to buffers. No tracking
of individual memory allocs, so no data loss for allocating small chunks of memory
}

interface

uses {$ifdef darwin}macport,mactypes,math,{$endif}{$ifdef windows}windows,{$endif}classes,sysutils, newkernelhandler, LCLIntf;

type TBigMemoryAllocHandler=class
private
  allocs: array of pointer;
  allocspos: integer;
  allocssize: integer;

  currentBuffer: pointer; //current memory region to use //updated on each alloc
  lastsize: integer;
  memoryleft: integer;
public
  constructor create;
  destructor destroy; override;
  function alloc(size: integer):pointer;
  function realloc(oldaddress: pointer; oldsize: integer; size: integer): pointer;//just throws away the old region
end;

implementation

resourcestring
  rsAllocError = 'VirtualAlloc failed. You probably don''t have enough system '
    +'memory free. Either install more RAM, or increase the maximum allowed '
    +'paging size';
  rsCEPointerscanMemoryManager = 'CE Pointerscan memory manager';
  rsBMAVirtualAllocFailedYouProbablyDontHaveEnoughtVirtualMemoryFreeEtc = 'VirtualAlloc failed. You probably don''t have enough virtual memory free. Use the 64-bit version instead';

constructor TBigMemoryAllocHandler.create;
begin
  currentbuffer:=nil;
  memoryleft:=0;
  lastsize:=1;


  allocssize:=32;
  allocspos:=0;
  setlength(allocs,32);
end;

destructor TBigMemoryAllocHandler.destroy;
var i: integer;
begin
  for i:=0 to length(allocs)-1 do
    VirtualFree(allocs[i],0,MEM_RELEASE);

  setlength(allocs,0);

  inherited destroy;
end;


function TBigMemoryAllocHandler.alloc(size: integer):pointer;
var newsize: size_t;
  flAllocationType : dword;
  lpm: size_t;
  e: integer;
begin
  try
    if size>memoryleft then
    begin
      //need to alloce a new memory regions
      lpm:=GetLargePageMinimum;
      newsize:=lpm;
      if newsize=0 then
        newsize:=2*1024*1024; //2mb

      newsize:=newsize*lastsize;
      if newsize<16*1024*1024 then
        inc(lastsize); //next time allocate more memory

      while newsize<size do
      begin
        newsize:=newsize*lastsize;
        if newsize<16*1024*1024 then
          inc(lastsize); //next time allocate more memory

//        raise Exception.create('some really fucked up parameter is given for size:'+inttostr(size));
      end;

      flAllocationType:=MEM_COMMIT or MEM_RESERVE;
      if lpm>0 then //cpu supports large pages
        flAllocationType:=flAllocationType or MEM_LARGE_PAGES;


      currentbuffer:=VirtualAlloc(nil,newsize, flAllocationType , PAGE_READWRITE);
      while (currentbuffer=nil) and (newsize>size) do
      begin
        currentbuffer:=VirtualAlloc(nil,newsize, MEM_COMMIT or MEM_RESERVE , PAGE_READWRITE);
        if currentbuffer=nil then
          newsize:=newsize div 2;
      end;

      if currentbuffer=nil then
      begin
        {$ifdef cpu64}
          raise exception.create(rsAllocError);
        {$else}
          raise exception.create(rsBMAVirtualAllocFailedYouProbablyDontHaveEnoughtVirtualMemoryFreeEtc);
        {$endif}
      end;

      allocs[allocspos]:=currentbuffer;
      inc(allocspos);
      if allocspos>=allocssize then
      begin
        allocssize:=min(allocssize*2, allocssize+4096); //allocate twice the ammount it was, with a max of 4096
        setlength(allocs,allocssize);
      end;


      memoryleft:=newsize;
    end;


    result:=currentBuffer;
    inc(pbyte(currentbuffer),size); //adjust the pointer to point to the next free spot
    dec(memoryleft,size);
  except
    on e: Exception do
    begin
      messagebox(0, pchar(e.message), pchar(rsCEPointerscanMemoryManager), 0);
      raise exception.create(e.message);
    end;
  end;

end;

function TBigMemoryAllocHandler.realloc(oldaddress: pointer; oldsize: integer; size: integer): pointer;
begin
  result:=alloc(size);
  CopyMemory(result,oldaddress,oldsize);
end;

end.
