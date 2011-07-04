unit hookedfunctions;

{$mode delphi}

interface

uses sysutils, windows, messages, syncobjs;

type TNtAllocateVirtualMemory=function(processHandle: THandle; BaseAddress: PPtrUint; ZeroBits: DWORD; RegionSize: PDWORD; AllocationType: DWORD; Protect: DWORD): DWORD; stdcall;
type TNtFreeVirtualMemory=function(processHandle: THandle; BaseAddress: PPtrUint; RegionSize: PDWORD; FreeType: DWORD ): DWORD; stdcall;

type TRtlAllocateHeap=function(HeapHandle: pointer; Flags: DWORD; size: integer): pointer; stdcall;
type TRtlFreeHeap=function(HeapHandle: pointer; Flags: DWORD; HeapBase: pointer): BOOL; stdcall;
type TRtlDestroyHeap=function(HeapHandle: pointer): pointer; stdcall;

function CeAllocateVirtualMemory(processHandle: THandle; BaseAddress: PPtrUint; ZeroBits: DWORD; RegionSize: PDWORD; AllocationType: DWORD; Protect: DWORD): DWORD; stdcall;
function CeFreeVirtualMemory(processHandle: THandle; BaseAddress: PPtrUint; RegionSize: PDWORD; FreeType: DWORD ): DWORD; stdcall;

function CeRtlAllocateHeap(HeapHandle: pointer; Flags: DWORD; size: integer): pointer; stdcall;
function CeRtlFreeHeap(HeapHandle: pointer; Flags: DWORD; HeapBase: pointer): BOOL; stdcall;
function CeRtlDestroyHeap(HeapHandle: pointer): pointer; stdcall;

function CeInitializeAllocHook: BOOL; stdcall;

var
  NtAllocateVirtualMemoryOrig: TNtAllocateVirtualMemory;
  NtFreeVirtualMemoryOrig: TNtFreeVirtualMemory;
  RtlAllocateHeapOrig: TRtlAllocateHeap;
  RtlFreeHeapOrig: TRtlFreeHeap;
  RtlDestroyHeapOrig: TRtlDestroyHeap;

const
  HOOKEVENT_ALLOC=0;
  HOOKEVENT_FREE=1;
  HOOKEVENT_HEAPALLOC=2;
  HOOKEVENT_HEAPFREE=3;
  HOOKEVENT_HEAPDESTROY=4;

type
  TAllocData=record
    handle: qword;
    baseaddress: qword;
    allocationType: dword;
    protect: dword;
    size: dword;
    esp: qword;
  end;

  TFreeData=record
    handle: qword;
    baseaddress: qword;
    size: dword;
    FreeType: dword;
  end;

  THeapAllocData=record
    HeapHandle: qword;
    Flags: DWORD;
    Size: dword;
    esp:qword;
    address: qword;
  end;

  THeapFreeData=record
    HeapHandle: qword;
    Flags: dword;
    HeapBase: qword;
  end;

  THeapDestroyData=record
    HeapHandle: qword;
  end;

  THookEvent = record
    eventtype: integer;
    case integer of
      HOOKEVENT_ALLOC: (AllocEvent: TAllocData);
      HOOKEVENT_FREE: (FreeEvent: TFreeData);
      HOOKEVENT_HEAPALLOC: (HeapAllocEvent: THeapAllocData);
      HOOKEVENT_HEAPFREE: (HeapFreeEvent: THeapFreeData);
      HOOKEVENT_HEAPDESTROY: (HeapDestroyEvent: THeapDestroyData);
  end;


var
  HookEventData: THookEvent;

var HasSetupDataEvent: qword; //set by this routine
var CEHasHandledItEvent: qword; //set by ce
var CEInitializationFinished: qword; //set by CeInitializeAllocHook, and reset by ce (or never)

implementation

var allocCS: TCriticalSection;

function CeInitializeAllocHook: BOOL; stdcall;
//this function will gather the start memory situation using a heapwalk and pass off that information to ce (ce's lookup is faster for the pointerscan)
//call this after the hooks are in place and all configuration has been completed (handles)
var heaphandles: array of thandle;
    size: integer;
    h: THandle;
    i: integer;
    phe: TProcessHeapEntry;
begin

  ResetEvent(CEInitializationFinished); //in case ce forgot to reset it...

  h:=0;
  size:=GetProcessHeaps(0,h);

  if size>0 then
  begin
    setlength(heaphandles,size);
    size:=GetProcessHeaps(size,heaphandles[0]);

    if size>0 then
    begin
      for i:=0 to size-1 do
      begin
        heaplock(heaphandles[i]);
        try
          ZeroMemory(@phe,sizeof(phe));

          while HeapWalk(heaphandles[i], phe) do
          begin
            if phe.wFlags=PROCESS_HEAP_ENTRY_BUSY then
            begin
              allocCS.Enter;
              try
                HookEventData.eventtype:=HOOKEVENT_HEAPALLOC;
                HookEventData.HeapAllocEvent.HeapHandle:=heaphandles[i];
                HookEventData.HeapAllocEvent.flags:=0;
                HookEventData.HeapAllocEvent.size:=phe.cbData;
                HookEventData.HeapAllocEvent.esp:=0;
                HookEventData.HeapAllocEvent.address:=qword(phe.lpData);

                //signal ce that the event has been filled in
                SetEvent(HasSetupDataEvent);

                //and wait till ce is done with it
                if waitforsingleobject(CEHasHandledItEvent,5000)<>WAIT_OBJECT_0	then
                  outputdebugstring('CeRtlAllocateHeap:timeout on CEHasHandledItEvent');
              finally
                allocCS.Leave;
              end;
            end;
          end;
        finally
          heapunlock(heaphandles[i]);
        end;
      end;
    end;
  end;

  result:=true;
  setevent(CEInitializationFinished);
end;

function CeRtlAllocateHeap(HeapHandle: pointer; Flags: DWORD; size: integer): pointer; stdcall;
var
  stack: dword;
begin
  result:=RtlAllocateHeapOrig(heapHandle, flags,size);


  if result<>nil then
  begin
    allocCS.Enter;

    HookEventData.eventtype:=HOOKEVENT_HEAPALLOC;
    HookEventData.HeapAllocEvent.HeapHandle:=qword(HeapHandle);
    HookEventData.HeapAllocEvent.flags:=flags;
    HookEventData.HeapAllocEvent.size:=size;

    HookEventData.HeapAllocEvent.address:=qword(result);

    //signal ce that the event has been filled in
    SetEvent(HasSetupDataEvent);

    //and wait till ce is done with it
    if waitforsingleobject(CEHasHandledItEvent,5000)<>WAIT_OBJECT_0	then
      outputdebugstring('CeRtlAllocateHeap:timeout on CEHasHandledItEvent');

    allocCS.Leave;
  end;
end;

function CeRtlFreeHeap(HeapHandle: pointer; Flags: DWORD; HeapBase: pointer): BOOL; stdcall;
begin
  result:=RtlFreeHeapOrig(heapHandle, Flags, HeapBase);

  if result then
  begin
    allocCS.Enter;

      HookEventData.eventtype:=HOOKEVENT_HEAPFREE;
      HookEventData.HeapFreeEvent.HeapHandle:=qword(HeapHandle);
      HookEventData.HeapFreeEvent.Flags:=flags;
      HookEventData.HeapFreeEvent.HeapBase:=qword(HeapBase);

      //signal ce that the event has been filled in
      SetEvent(HasSetupDataEvent);

      //and wait till ce is done with it
      if waitforsingleobject(CEHasHandledItEvent,5000)<>WAIT_OBJECT_0	then
        outputdebugstring('CeRtlFreeHeap:timeout on CEHasHandledItEvent');

      allocCS.Leave;

  end;
end;

function CeRtlDestroyHeap(HeapHandle: pointer): pointer; stdcall;
//Hooked because destroying a heaphandle destroys ALL related heap allocs
begin
  result:=RtlDestroyHeapOrig(heaphandle);


  if result=nil then
  begin
    allocCS.Enter;

      HookEventData.eventtype:=HOOKEVENT_HEAPDESTROY;
      HookEventData.HeapDestroyEvent.HeapHandle:=qword(HeapHandle);

      //signal ce that the event has been filled in
      SetEvent(HasSetupDataEvent);

      //and wait till ce is done with it
      if waitforsingleobject(CEHasHandledItEvent,5000)<>WAIT_OBJECT_0	then
        outputdebugstring('CeRtlDestroyHeap:timeout on CEHasHandledItEvent');

      allocCS.Leave;

  end;
end;

function CeAllocateVirtualMemory(processHandle: THandle; BaseAddress: PPtrUint; ZeroBits: DWORD; RegionSize: PDWORD; AllocationType: DWORD; Protect: DWORD): DWORD; stdcall;
var
  stack: dword;
begin
  //note: don't call anything that allocs memory
  result:=NtAllocateVirtualMemoryOrig(processHandle, BaseAddress, ZeroBits, RegionSize, AllocationType, Protect);

  if result=0 then //successfully allocated
  begin
    allocCS.Enter;


      //fill in allocation field

      //post a message to the handlerthread with the address of CreateEvent and wait till it's done
      HookEventData.eventtype:=HOOKEVENT_ALLOC;
      HookEventData.AllocEvent.handle:=processHandle;

      if baseaddress<>nil then
        HookEventData.AllocEvent.baseaddress:=BaseAddress^
      else
        HookEventData.AllocEvent.baseaddress:=0;



      HookEventData.AllocEvent.allocationType:=AllocationType;
      HookEventData.AllocEvent.protect:=protect;

     // asm
     //   mov stack,ebp
     // end;
     // HookEventData.AllocEvent.esp:=stack;

      if regionsize<>nil then
        HookEventData.AllocEvent.size:=regionsize^
      else
        HookEventData.AllocEvent.size:=0;

      //signal ce that the createEvent has been filled in
      SetEvent(HasSetupDataEvent);

      //and wait till ce is done with it
      if waitforsingleobject(CEHasHandledItEvent,5000)<>WAIT_OBJECT_0	then
        outputdebugstring('Alloc:timeout on CEHasHandledItEvent');

      allocCS.Leave;

  end;
end;

function CeFreeVirtualMemory(processHandle: THandle; BaseAddress: PPtrUint; RegionSize: PDWORD; FreeType: DWORD ): DWORD; stdcall;
begin
  result:=NtFreeVirtualMemoryOrig(processHandle, BaseAddress, RegionSize, FreeType);



  if result=0 then //success
  begin
    allocCS.Enter;

      HookEventData.eventtype:=HOOKEVENT_FREE;
      HookEventData.FreeEvent.handle:=processHandle;
      if baseaddress <> nil then
        HookEventData.FreeEvent.baseaddress:=baseaddress^
      else
        HookEventData.FreeEvent.baseaddress:=0;

      if regionsize<>nil then
        HookEventData.FreeEvent.size:=regionSize^
      else
        HookEventData.FreeEvent.size:=0;

      HookEventData.FreeEvent.FreeType:=FreeType;

      //signal ce that the createEvent has been filled in
      SetEvent(HasSetupDataEvent);

      //and wait till ce is done with it
      if waitforsingleobject(CEHasHandledItEvent,5000)<>WAIT_OBJECT_0	then
        outputdebugstring('Free:timeout on CEHasHandledItEvent');

      allocCS.Leave;

  end;
end;

initialization
  allocCS:=TCriticalSection.Create;

end.

