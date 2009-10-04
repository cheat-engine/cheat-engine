unit hookedfunctions;

interface

uses windows,messages;

type TNtAllocateVirtualMemory=function(processHandle: THandle; BaseAddress: PDWORD; ZeroBits: DWORD; RegionSize: PDWORD; AllocationType: DWORD; Protect: DWORD): DWORD; stdcall;
type TNtFreeVirtualMemory=function(processHandle: THandle; BaseAddress: PDWORD; RegionSize: PDWORD; FreeType: DWORD ): DWORD; stdcall;


function CeAllocateVirtualMemory(processHandle: THandle; BaseAddress: PDWORD; ZeroBits: DWORD; RegionSize: PDWORD; AllocationType: DWORD; Protect: DWORD): DWORD; stdcall;
function CeFreeVirtualMemory(processHandle: THandle; BaseAddress: PDWORD; RegionSize: PDWORD; FreeType: DWORD ): DWORD; stdcall;

var NtAllocateVirtualMemoryOrig: TNtAllocateVirtualMemory;
var NtFreeVirtualMemoryOrig: TNtFreeVirtualMemory;
var CeAllocHandlerWindow: THandle;





implementation


function CeAllocateVirtualMemory(processHandle: THandle; BaseAddress: PDWORD; ZeroBits: DWORD; RegionSize: PDWORD; AllocationType: DWORD; Protect: DWORD): DWORD; stdcall;
var
  CreateEvent: record
    handle: dword;
    baseaddress: dword;
    allocationType: dword;
    protect: dword;
    size: dword;
    esp: dword;
  end;
  stack: dword;
begin
  //note: don't call anything that allocs memory
  result:=NtAllocateVirtualMemoryOrig(processHandle, BaseAddress, ZeroBits, RegionSize, AllocationType, Protect);
  if result=0 then //successfully allocated
  begin
    //post a message to the handlerthread with the address of CreateEvent and wait till it's done
    CreateEvent.handle:=processHandle;

    if baseaddress<>nil then
      CreateEvent.baseaddress:=BaseAddress^
    else
      CreateEvent.baseaddress:=0;

    CreateEvent.allocationType:=AllocationType;
    CreateEvent.protect:=protect;
    asm
      mov stack,ebp
    end;
    CreateEvent.esp:=stack;
    
    if regionsize<>nil then
      CreateEvent.size:=regionsize^
    else
      CreateEvent.size:=0;
      
    SendMessage(CeAllocHandlerWindow, wm_user+1, dword(@CreateEvent), 0);
  end;
end;

function CeFreeVirtualMemory(processHandle: THandle; BaseAddress: pdword; RegionSize: PDWORD; FreeType: DWORD ): DWORD; stdcall;
var FreeEvent: record
  handle: dword;
  baseaddress: dword;
  size: dword;
  FreeType: dword;
end;
begin
  result:=NtFreeVirtualMemoryOrig(processHandle, BaseAddress, RegionSize, FreeType);

  if result=0 then //success
  begin
    FreeEvent.handle:=processHandle;
    if baseaddress <> nil then
      FreeEvent.baseaddress:=baseaddress^
    else
      FreeEvent.baseaddress:=0;

    if regionsize<>nil then
      FreeEvent.size:=regionSize^
    else
      FreeEvent.size:=0;

    FreeEvent.FreeType:=FreeType;
    SendMessage(CeAllocHandlerWindow, wm_user+2, dword(@FreeEvent), 0);
  end;
end;

end.
