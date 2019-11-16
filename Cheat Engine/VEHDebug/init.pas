unit init; 

{$mode DELPHI}

interface

uses
  jwaWindows, Windows, Classes, SysUtils, VEHDebugSharedMem;


var
  ConfigName: array [0..255] of byte;
  fm: thandle;
  VEHSharedMem: PVEHDebugSharedMem;


procedure InitializeVEH;
procedure UnloadVEH;

var AddVectoredExceptionHandler: function (FirstHandler: Cardinal; VectoredHandler: PVECTORED_EXCEPTION_HANDLER): pointer; stdcall;
    RemoveVectoredExceptionHandler: function(VectoredHandlerHandle: PVOID): ULONG; stdcall;
    CreateToolhelp32Snapshot: function(dwFlags, th32ProcessID: DWORD): HANDLE; stdcall;
    Thread32First: function(hSnapshot: HANDLE; var lpte: THREADENTRY32): BOOL; stdcall;
    Thread32Next: function(hSnapshot: HANDLE; var lpte: THREADENTRY32): BOOL; stdcall;

var oldExceptionHandler: pointer=nil;
    vehdebugactive: boolean;

implementation

uses DebugHandler,threadpoll;


procedure EmulateInitializeEvents;
var ep: TEXCEPTIONPOINTERS;
    er: TEXCEPTIONRECORD;

    ths: THandle;
    lpte: TThreadEntry32;
    check: boolean;
    cpid: dword;
    isfirst: boolean;

    bpc: TContext;
begin
  //OutputDebugString('EmulateInitializeEvents');
  cpid:=GetCurrentProcessId;
  ep.ContextRecord:=nil;
  ep.ExceptionRecord:=@er;
  er.NumberParameters:=0;

  HandlerCS.Enter;

  ths:=CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD,0);
  if ths<>INVALID_HANDLE_VALUE then
  begin
    zeromemory(@lpte, sizeof(lpte));
    lpte.dwSize:=sizeof(lpte);

    isfirst:=true;
    check:=Thread32First(ths, lpte);
    while check do
    begin
      if lpte.th32OwnerProcessID=cpid then
      begin
        if isfirst then
        begin
          //create process
          er.ExceptionCode:=$ce000000; // $ce000000=create process (just made up)

          InternalHandler(@ep,lpte.th32ThreadID); //I don't care what the return value is
          isfirst:=false;
        end else
        begin
          //create thread
          er.ExceptionCode:=$ce000001;
          InternalHandler(@ep,lpte.th32ThreadID);
        end;
      end;

      check:=Thread32Next(ths, lpte);
    end;

    CloseHandle(ths);
  end;

  //if VEHSharedMem.ThreadWatchMethod=0 then
  //  ThreadPoller:=TThreadPoller.create(false);

  //tell ce that the debugger has been attached
//  ep.ContextRecord:=@VEHSharedMem.CurrentContext[0]; //just some memory properly aligned that can be used as scratchspace
//  GetThreadContext(GetCurrentThread, ep.ContextRecord);

  er.ExceptionCode:=EXCEPTION_BREAKPOINT;
  ep.ContextRecord:=@bpc;
  zeromemory(@bpc,sizeof(bpc));
  bpc.{$ifdef cpu64}rip{$else}eip{$endif}:=$ffffffce;

  Handler(@ep); //don't really cause a breakpoint (while I could just do a int3 myself I think it's safer to just emulate the event)


  HandlerCS.Leave;
end;

procedure InitializeVEH;
var k: THandle;
    m: pchar;
begin
  k:=LoadLibrary('kernel32.dll');
  AddVectoredExceptionHandler:=GetProcAddress(k,'AddVectoredExceptionHandler');
  RemoveVectoredExceptionHandler:=GetProcAddress(k,'RemoveVectoredExceptionHandler');
  CreateToolhelp32Snapshot:=GetProcAddress(k,'CreateToolhelp32Snapshot');
  Thread32First:=GetProcAddress(k,'Thread32First');
  Thread32Next:=GetProcAddress(k,'Thread32Next');


  UnloadVEH;



  testandfixcs_start;

  OutputDebugString('VEHDebug init');


  {if ThreadPoller<>nil then
  begin
    ThreadPoller.Terminate;
    ThreadPoller.WaitFor;
    ThreadPoller.free;
    ThreadPoller:=nil;
  end;  }

  testandfixcs_final;


  //get the shared memory object
  m:=pchar(@ConfigName[0]);
  outputDebugstring(pchar('ConfigName='+m));

//  fm:=CreateFileMapping(INVALID_HANDLE_VALUE,nil,PAGE_READWRITE,0,sizeof(TVEHDebugSharedMem),@ConfigName[0]);

  if fm=0 then
    fm:=OpenFileMapping(FILE_MAP_ALL_ACCESS,false,m);

  OutputDebugString(pchar('fm='+inttohex(fm,8)));

  if (fm=0) then
  begin
    OutputDebugString(pchar('GetLastError='+inttostr(getlasterror)));
    exit;
  end;


  VEHSharedMem:=MapViewOfFile(fm,FILE_MAP_ALL_ACCESS,0,0,0);
  OutputDebugString(pchar('VEHSharedMem='+inttohex(ptruint(VEHSharedMem),8)));

  if VEHSharedMem=nil then
  begin
    OutputDebugString(pchar('GetLastError='+inttostr(getlasterror)));
    exit;
  end;

  VEHSharedMem.VEHVersion:=$cece0000+VEHVERSION;


  OutputDebugString(pchar('HasDebugEvent='+inttohex(VEHSharedMem.HasDebugEvent,8)));
  OutputDebugString(pchar('HasHandledDebugEvent='+inttohex(VEHSharedMem.HasHandledDebugEvent,8)));

  OutputDebugString(pchar('@HasDebugEvent='+inttohex(ptruint(@VEHSharedMem.HasDebugEvent),8)));
  OutputDebugString(pchar('@HasHandledDebugEvent='+inttohex(ptruint(@VEHSharedMem.HasHandledDebugEvent),8)));


  if assigned(AddVectoredExceptionHandler) then
  begin
    //if oldExceptionHandler<>nil then
   //   outputdebugstring('Old exception handler should have been deleted. If not, this will crash');


    OutputDebugString('Testing if it handles normal debug events');
    OutputDebugString('1');
    OutputDebugString('2');
    OutputDebugString('3');

    OutputDebugString('Calling EmulateInitializeEvents');

    handlerlock:=GetCurrentThreadId;
    HandlerCS.enter; //do not handle any external exception while the threadlist is sent to ce

    OutputDebugString('Registering exception handler');
    vehdebugactive:=true;
    if oldExceptionHandler=nil then
      oldExceptionHandler:=AddVectoredExceptionHandler(1,@Handler);

    if oldExceptionHandler=nil then
    begin
      vehdebugactive:=false;
      HandlerCS.leave;
      exit;
    end;

    EmulateInitializeEvents;

    HandlerCS.leave;
    handlerlock:=0;


    OutputDebugString('returned from EmulateInitializeEvents');



    if oldExceptionHandler<>nil then
      OutPutDebugString(pchar('Created exception handler:'+inttohex(ptrUint(oldExceptionHandler),8)))
    else
      outputdebugstring('Failed creating exception handler');


  end;

end;

procedure UnloadVEH;
begin
  vehdebugactive:=false;
  {
  if assigned(RemoveVectoredExceptionHandler) then
  begin
    if oldExceptionHandler<>nil then
    begin
      RemoveVectoredExceptionHandler(oldExceptionHandler);
      oldExceptionHandler:=nil;
    end;
  end;
  }
end;

end.

