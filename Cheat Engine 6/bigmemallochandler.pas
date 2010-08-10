unit bigmemallochandler;

{$MODE Delphi}


{
This unit will provide a class (TBigMemoryAllocHandler) that will allocate more
memory than it actually needs and just returns pointers to buffers. No tracking
of individual memory allocs, so no data loss for allocating small chunks of memory
}

interface

uses windows,classes;

type TBigMemoryAllocHandler=class
private
  allocs: array of pointer;

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

constructor TBigMemoryAllocHandler.create;
begin
  lastsize:=1024*1024;
  getmem(currentbuffer,lastsize); //1mb
  ZeroMemory(currentbuffer,lastsize);
end;

destructor TBigMemoryAllocHandler.destroy;
var i: integer;
begin
  for i:=0 to length(allocs)-1 do
    freemem(allocs[i]);

  inherited destroy;
end;


function TBigMemoryAllocHandler.alloc(size: integer):pointer;
var newsize: dword;
begin
try
  while size>memoryleft do
  begin
    newsize:=lastsize*2;
    if newsize>(16*1024*1024) then
      newsize:=lastsize;

    if newsize<size then
      newsize:=size*2;

    //allocate a new buffer
    setlength(allocs,length(allocs)+1);


    getmem(currentBuffer,newsize);
    ZeroMemory(currentbuffer,newsize);
    allocs[length(allocs)-1]:=currentBuffer;
    memoryleft:=newsize;
    lastsize:=newsize;

  end;

  result:=currentBuffer;
  inc(pbyte(currentbuffer),size); //adjust the pointer to point to the next free spot
  dec(memoryleft,size);
except
  messagebox(0,'Allocation error','CE Pointerscan memory manager',0);
end;

end;

function TBigMemoryAllocHandler.realloc(oldaddress: pointer; oldsize: integer; size: integer): pointer;
begin
  result:=alloc(size);
  CopyMemory(result,oldaddress,oldsize);
end;

end.
