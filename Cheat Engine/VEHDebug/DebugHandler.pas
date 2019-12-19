unit DebugHandler;

{$mode delphi}

interface

uses
  windows, Classes, SysUtils, syncobjs, extcont;


function Handler(ExceptionInfo: PEXCEPTION_POINTERS): LONG; stdcall;
function InternalHandler(ExceptionInfo: PEXCEPTION_POINTERS; threadid: dword): LONG;
procedure testandfixcs_start;
procedure testandfixcs_final;

var
  HandlerCS: TCRITICALSECTION;
  HandlerLock: dword;

implementation

uses init;

var
  emergency: THandle; //event that is set when

procedure testandfixcs_start;
begin
  if emergency=0 then
    OutputDebugString('emergency event not created');

  setevent(emergency);
end;

procedure testandfixcs_final;
begin
  handlercs.enter;
  handlercs.leave;
  resetevent(emergency);
end;


function InternalHandler(ExceptionInfo: PEXCEPTION_POINTERS; threadid: dword): LONG;
var i: integer;
  eventhandles: array [0..1] of THandle;
  wr: dword;
  contextsize: integer=0;
  heartbeat: DWORD;
begin
  result:=EXCEPTION_CONTINUE_SEARCH;

  if not vehdebugactive then exit(EXCEPTION_CONTINUE_SEARCH);


  while (handlerlock<>0) and (handlerlock<>GetCurrentThreadId) do sleep(50);

  HandlerCS.enter; //block any other thread that has an single step exception until this is handled




  //fill in the exception and context structures
  {$ifdef cpu64}
  VEHSharedMem^.Exception64.ExceptionCode:=ExceptionInfo.ExceptionRecord.ExceptionCode;
  VEHSharedMem^.Exception64.ExceptionFlags:=ExceptionInfo.ExceptionRecord.ExceptionFlags;
  VEHSharedMem^.Exception64.ExceptionRecord:=DWORD64(ExceptionInfo.ExceptionRecord.ExceptionRecord);
  VEHSharedMem^.Exception64.NumberParameters:=ExceptionInfo.ExceptionRecord.NumberParameters;
  for i:=0 to ExceptionInfo.ExceptionRecord.NumberParameters-1 do
    VEHSharedMem^.Exception64.ExceptionInformation[i]:=ExceptionInfo.ExceptionRecord.ExceptionInformation[i];
  {$else}
  VEHSharedMem^.Exception32.ExceptionCode:=ExceptionInfo.ExceptionRecord.ExceptionCode;
  VEHSharedMem^.Exception32.ExceptionFlags:=ExceptionInfo.ExceptionRecord.ExceptionFlags;
  VEHSharedMem^.Exception32.ExceptionRecord:=DWORD(ExceptionInfo.ExceptionRecord.ExceptionRecord);
  VEHSharedMem^.Exception32.NumberParameters:=ExceptionInfo.ExceptionRecord.NumberParameters;
  for i:=0 to ExceptionInfo.ExceptionRecord.NumberParameters-1 do
    VEHSharedMem^.Exception32.ExceptionInformation[i]:=ExceptionInfo.ExceptionRecord.ExceptionInformation[i];
  {$endif}


  //setup the context
  if ExceptionInfo.ContextRecord<>nil then
  begin
    contextsize:=sizeof(TContext);
    {$ifdef cpu32}
      //32-bit
      if (ExceptionInfo.ContextRecord.ContextFlags and CONTEXT_EXTENDED)=CONTEXT_EXTENDED then
        contextsize:=sizeof(TEContext);
    {$endif}

    // messagebox(0,pchar('Copying context:'+inttohex(ptruint(ExceptionInfo.ContextRecord),8)+':'+inttostr(contextsize)), 'InternalHandler', 0);

    CopyMemory(@VEHSharedMem^.CurrentContext[0],ExceptionInfo.ContextRecord,contextsize);
  end
  else
    zeromemory(@VEHSharedMem^.CurrentContext[0], sizeof(TEContext));



  VEHSharedMem^.ProcessID:=GetCurrentProcessId;
  VEHSharedMem^.ThreadID:=threadid;

  if SetEvent(VEHSharedMem^.HasDebugEvent) then
  begin
    eventhandles[0]:=VEHSharedMem^.HasHandledDebugEvent;
    eventhandles[1]:=emergency;

    heartbeat:=VEHSharedMem^.HeartBeat;
    repeat
      wr:=WaitForMultipleObjects(2, @eventhandles, false, 5000);
      if wr=WAIT_TIMEOUT then
      begin
        if heartbeat=VEHSharedMem^.HeartBeat then //unchanged after 5 seconds
        begin
          //CE died.
          {$ifndef SKIPCEHEARTBEAT}
          //MessageBox(0,'CE timeout','CE timeout',0);
          UnloadVEH;

          ResetEvent(VEHSharedMem^.HasDebugEvent);
          result:=EXCEPTION_CONTINUE_SEARCH;
          handlerCS.Leave;

          OutputDebugString('VEH debug terminated because the heartbeat stopped');

          // MessageBoxA(0,'CE''s VEH heartbeat has stopped and an exception happened. Press OK to see what happens next','VEH Debug',MB_ICONERROR or MB_OK);
          exit;
          {$endif}
        end;
      end;
    until wr<>WAIT_TIMEOUT;

    i:=wr -WAIT_OBJECT_0;
    if i=0 then //hashandleddebugevent has been set.  After ce is done with it use the new context
    begin
      if ExceptionInfo.ContextRecord<>nil then
      begin
        CopyMemory(ExceptionInfo.ContextRecord,@VEHSharedMem^.CurrentContext[0],contextsize);

        if VEHSharedMem^.ContinueMethod=DBG_CONTINUE then  //it got handled, set the debug registers (else don't touch them. DR6 might be needed)
        begin
          PContext(@VEHSharedMem^.CurrentContext[0])^.ContextFlags:=CONTEXT_DEBUG_REGISTERS;  //only debug regs
          SetThreadContext(GetCurrentThread, PContext(@VEHSharedMem^.CurrentContext[0])^);
        end;

      end;
    end
    else
    begin
      MessageBox(0,'WaitForMultipleObjects failed', 'VEH Debug Error', MB_OK);
      result:=EXCEPTION_CONTINUE_EXECUTION; //something went wrong VEHSharedmem might even be broken
      HandlerCS.Leave;
      exit;
    end;


    //depending on user options either return EXCEPTION_CONTINUE_SEARCH or EXCEPTION_CONTINUE_EXECUTION
    if VEHSharedMem^.ContinueMethod=DBG_CONTINUE then
      result:=EXCEPTION_CONTINUE_EXECUTION
    else
      result:=EXCEPTION_CONTINUE_SEARCH;

  end;
  //else
  // MessageBox(0,'SetEvent failed', 'VEH Debug Error', MB_OK);


  HandlerCS.Leave;

end;


function Handler(ExceptionInfo: PEXCEPTION_POINTERS): LONG; stdcall;
const STATUS_WX86_SINGLE_STEP = $4000001E;
var i: integer;
  tid: dword;
begin
   //check if the current threadid is in the NoBreakList and if so, and the break is a single step(so not int3 or pagefault, or whatever) continue with the resume flag set
   tid:=GetCurrentThreadId;
   for i:=0 to VEHSharedMem^.NoBreakListSize-1 do
   begin

     if VEHSharedMem^.NoBreakList[i]=tid then
     begin
       if (ExceptionInfo.ExceptionRecord.ExceptionCode = EXCEPTION_SINGLE_STEP) or (ExceptionInfo.ExceptionRecord.ExceptionCode=STATUS_WX86_SINGLE_STEP) then
       begin
         ExceptionInfo.ContextRecord^.EFlags:=ExceptionInfo.ContextRecord^.EFlags or $10000; //set the RF bit
         result:=EXCEPTION_CONTINUE_EXECUTION;
         exit;
       end
       else
         break; //not a single step
     end;
   end;

  result:=InternalHandler(ExceptionInfo, getCurrentThreadID);
end;

initialization
  HandlerCS:=TCriticalSection.create;
  emergency:=CreateEvent(nil,true, false,'');


finalization
  if HandlerCS<>nil then
    HandlerCS.free;

end.

