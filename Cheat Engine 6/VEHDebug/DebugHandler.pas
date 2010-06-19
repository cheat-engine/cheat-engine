unit DebugHandler;

{$mode delphi}

interface

uses
  windows, Classes, SysUtils, syncobjs;


function Handler(ExceptionInfo: PEXCEPTION_POINTERS): LONG; stdcall;
function InternalHandler(ExceptionInfo: PEXCEPTION_POINTERS; threadid: dword): LONG;

implementation

uses init;

var HandlerCS: TCRITICALSECTION;

function InternalHandler(ExceptionInfo: PEXCEPTION_POINTERS; threadid: dword): LONG;
var i: integer;
begin
   HandlerCS.enter; //block any other thread that has an single step exception untill this is handles

   //fill in the exception and context structures
   {$ifdef cpu64}
   VEHSharedMem.Exception64.ExceptionCode:=ExceptionInfo.ExceptionRecord.ExceptionCode;
   VEHSharedMem.Exception64.ExceptionFlags:=ExceptionInfo.ExceptionRecord.ExceptionFlags;
   VEHSharedMem.Exception64.ExceptionRecord:=DWORD64(ExceptionInfo.ExceptionRecord.ExceptionRecord);
   VEHSharedMem.Exception64.NumberParameters:=ExceptionInfo.ExceptionRecord.NumberParameters;
   for i:=0 to ExceptionInfo.ExceptionRecord.NumberParameters do
     VEHSharedMem.Exception64.ExceptionInformation[i]:=ExceptionInfo.ExceptionRecord.ExceptionInformation[i];
   {$else}
   VEHSharedMem.Exception32.ExceptionCode:=ExceptionInfo.ExceptionRecord.ExceptionCode;
   VEHSharedMem.Exception32.ExceptionFlags:=ExceptionInfo.ExceptionRecord.ExceptionFlags;
   VEHSharedMem.Exception32.ExceptionRecord:=DWORD(ExceptionInfo.ExceptionRecord.ExceptionRecord);
   VEHSharedMem.Exception32.NumberParameters:=ExceptionInfo.ExceptionRecord.NumberParameters;
   for i:=0 to ExceptionInfo.ExceptionRecord.NumberParameters do
     VEHSharedMem.Exception32.ExceptionInformation[i]:=ExceptionInfo.ExceptionRecord.ExceptionInformation[i];
   {$endif}

   //setup the context
   CopyMemory(@VEHSharedMem.CurrentContext[0],ExceptionInfo.ContextRecord,sizeof(TCONTEXT));

   VEHSharedMem.ProcessID:=GetCurrentProcessId;
   VEHSharedMem.ThreadID:=threadid;

   SetEvent(VEHSharedMem.HasDebugEvent);
   WaitForSingleObject(VEHSharedMem.HasHandledDebugEvent,INFINITE);

   //after ce is done with it use the new context

   CopyMemory(ExceptionInfo.ContextRecord,@VEHSharedMem.CurrentContext[0],sizeof(TCONTEXT));


   //depending on user options either return EXCEPTION_CONTINUE_SEARCH or EXCEPTION_CONTINUE_EXECUTION
   if VEHSharedMem.ContinueMethod=DBG_CONTINUE then
     result:=EXCEPTION_CONTINUE_EXECUTION
   else
     result:=EXCEPTION_CONTINUE_SEARCH;

   HandlerCS.Leave;

end;

function Handler(ExceptionInfo: PEXCEPTION_POINTERS): LONG; stdcall;
begin
  result:=InternalHandler(ExceptionInfo, getCurrentThreadID);
end;

initialization
  HandlerCS:=TCriticalSection.create;

finalization
  if HandlerCS<>nil then
    HandlerCS.free;

end.

