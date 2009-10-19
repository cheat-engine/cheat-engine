unit hookedfunctions;

interface

uses sysutils, windows, messages, syncobjs;

type TNtAllocateVirtualMemory=function(processHandle: THandle; BaseAddress: PDWORD; ZeroBits: DWORD; RegionSize: PDWORD; AllocationType: DWORD; Protect: DWORD): DWORD; stdcall;
type TNtFreeVirtualMemory=function(processHandle: THandle; BaseAddress: PDWORD; RegionSize: PDWORD; FreeType: DWORD ): DWORD; stdcall;


function CeAllocateVirtualMemory(processHandle: THandle; BaseAddress: PDWORD; ZeroBits: DWORD; RegionSize: PDWORD; AllocationType: DWORD; Protect: DWORD): DWORD; stdcall;
function CeFreeVirtualMemory(processHandle: THandle; BaseAddress: PDWORD; RegionSize: PDWORD; FreeType: DWORD ): DWORD; stdcall;

var NtAllocateVirtualMemoryOrig: TNtAllocateVirtualMemory;
var NtFreeVirtualMemoryOrig: TNtFreeVirtualMemory;

var
  eventtype: integer;
  CreateEventData: record
    handle: dword;
    baseaddress: dword;
    allocationType: dword;
    protect: dword;
    size: dword;
    esp: dword;
  end;

  FreeEventData: record
    handle: dword;
    baseaddress: dword;
    size: dword;
    FreeType: dword;
  end;

var HasSetupDataEvent: THandle; //set by this routine
var CEHasHandledItEvent: THandle; //set by ce

implementation

var allocCS: TCriticalSection;



function CeAllocateVirtualMemory(processHandle: THandle; BaseAddress: PDWORD; ZeroBits: DWORD; RegionSize: PDWORD; AllocationType: DWORD; Protect: DWORD): DWORD; stdcall;
var
  stack: dword;
begin
  //note: don't call anything that allocs memory
  result:=NtAllocateVirtualMemoryOrig(processHandle, BaseAddress, ZeroBits, RegionSize, AllocationType, Protect);
  if result=0 then //successfully allocated
  begin
    allocCS.Enter;
    try

      //fill in allocation field

      //post a message to the handlerthread with the address of CreateEvent and wait till it's done
      CreateEventData.handle:=processHandle;

      if baseaddress<>nil then
        CreateEventData.baseaddress:=BaseAddress^
      else
        CreateEventData.baseaddress:=0;

      CreateEventData.allocationType:=AllocationType;
      CreateEventData.protect:=protect;
      asm
        mov stack,ebp
      end;
      CreateEventData.esp:=stack;
    
      if regionsize<>nil then
        CreateEventData.size:=regionsize^
      else
        CreateEventData.size:=0;

      eventtype:=0;
      //signal ce that the createEvent has been filled in
      SetEvent(HasSetupDataEvent);

      //and wait till ce is done with it
      if waitforsingleobject(CEHasHandledItEvent,5000)<>WAIT_OBJECT_0	then
        outputdebugstring('Alloc:timeout on CEHasHandledItEvent');
    finally
      allocCS.Leave;
    end;
  end;
end;

function CeFreeVirtualMemory(processHandle: THandle; BaseAddress: pdword; RegionSize: PDWORD; FreeType: DWORD ): DWORD; stdcall;
begin
  result:=NtFreeVirtualMemoryOrig(processHandle, BaseAddress, RegionSize, FreeType);

  if result=0 then //success
  begin
    FreeEventData.handle:=processHandle;
    if baseaddress <> nil then
      FreeEventData.baseaddress:=baseaddress^
    else
      FreeEventData.baseaddress:=0;

    if regionsize<>nil then
      FreeEventData.size:=regionSize^
    else
      FreeEventData.size:=0;

    FreeEventData.FreeType:=FreeType;

    //signal ce that the createEvent has been filled in
    eventtype:=1;
    SetEvent(HasSetupDataEvent);

    //and wait till ce is done with it
    if waitforsingleobject(CEHasHandledItEvent,5000)<>WAIT_OBJECT_0	then
      outputdebugstring('Free:timeout on CEHasHandledItEvent');
  end;
end;

initialization
  allocCS:=TCriticalSection.Create;

end.
