unit VEHDebugger;

{$mode delphi}

interface

{$ifdef windows}
uses

  jwaNtStatus, Windows,
  Classes, SysUtils,symbolhandler, symbolhandlerstructs,
  VEHDebugSharedMem,cefuncproc, autoassembler,newkernelhandler,DebuggerInterface,
  Clipbrd,maps;

type

  TVEHDebugInterface=class(TDebuggerInterface)
  private
    guid: TGUID; //to indentify this specific debugger
    guidstring: string;

    HasDebugEvent: THandle;
    HasHandledDebugEvent: THandle;

    ConfigFileMapping: THandle;
    VEHDebugView: PVEHDebugSharedMem;

    active: boolean;
    is64bit: boolean; //stored local so it doesn't have to evaluate the property (saves some time)
    hasPausedProcess: boolean;

    fisInjectedEvent: boolean; //obsolete(ish)
    wasInjectedEvent: boolean;
    injectedEvents: TList;

    lastthreadlist: TStringList;
    lastthreadpoll: qword;

    Heartbeat: TThread;

    CurrentThread: THandle;



    threads: TMap;  //internal threadhandle map

    procedure SynchronizeNoBreakList;
    procedure DoThreadPoll;
  public
    function isInjectedEvent: boolean; override;
    function WaitForDebugEvent(var lpDebugEvent: TDebugEvent; dwMilliseconds: DWORD): BOOL; override;
    function ContinueDebugEvent(dwProcessId: DWORD; dwThreadId: DWORD; dwContinueStatus: DWORD): BOOL; override;
    function SetThreadContext(hThread: THandle; const lpContext: TContext; isFrozenThread: Boolean=false): BOOL; override;
    function GetThreadContext(hThread: THandle; var lpContext: TContext; isFrozenThread: Boolean=false):  BOOL; override;

    function canUseIPT: boolean; override;

    function DebugActiveProcess(dwProcessId: DWORD): WINBOOL; override;
    function DebugActiveProcessStop(dwProcessID: DWORD): WINBOOL; override;
    procedure AddToNoBreakList(threadid: integer); override;
    procedure RemoveFromNoBreakList(threadid: integer); override;
    destructor destroy; override;
    constructor create;
  end;

{$endif}

implementation

{$ifdef windows}
uses ProcessHandlerUnit, Globals, dialogs, mainunit2;

resourcestring
  rsErrorWhileTryingToCreateTheConfigurationStructure = 'Error while trying '
    +'to create the configuration structure! (Which effectively renders this '
    +'whole feature useless) Errorcode=%s';
  rsCheatEngineFailedToGetIntoTheConfig = strCheatEngine+' failed to get into '
    +'the config of the selected program. (Error=%s)';
  rsFailureDuplicatingTheEventHandlesToTheOtherProcess = 'Failure duplicating '
    +'the event handles to the other process';
  rsVEHDebugError = 'VEH Debug error';
  rsFailureDuplicatingTheFilemapping = 'Failure duplicating the filemapping';
  rsTheVEHDllSeemsToHaveFailedToLoad = 'The VEH dll seems to have failed to load';
  rsWrongVEHDllVersion = 'The version of the VEH dll inside the target process (%x) does not match what was expected %x';


type
  TInjectedEvent=class
  public
    eventtype: (etThreadCreate,etThreadDestroy); //0=create thread, 1=destroythread
    threadid: integer;
  end;

  THeartBeat=class(TThread)
  private
    owner: TVEHDebugInterface;
    doVersionCheck: boolean;
    procedure invalidVersionMessage;
    procedure startVersionCheck;
  protected
    procedure execute; override;
  end;

procedure THeartBeat.startVersionCheck;
begin
  doVersionCheck:=true;
end;

procedure THeartBeat.invalidVersionMessage;
begin
  if owner.VEHDebugView.VEHVersion=0 then
    MessageDlg(rsTheVEHDllSeemsToHaveFailedToLoad, mtError, [mbok], 0)
  else
    MessageDlg(format(rsWrongVEHDllVersion, [owner.VEHDebugView.VEHVersion, DWORD($cece0000+VEHVERSION)]), mtWarning, [mbok], 0);
end;

procedure THeartBeat.execute;
var invalidversion: integer;
begin
  invalidversion:=0;
  while not terminated do
  begin
    inc(owner.VEHDebugView.heartbeat);
    sleep(250);

    if doVersionCheck and (owner.VEHDebugView.VEHVersion<>$cece0000+VEHVERSION) then
    begin
      inc(invalidversion);
      if invalidversion=10 then //(10*500 ms=5 seconds);
        queue(invalidVersionMessage);
    end;
  end;
end;

constructor TVEHDebugInterface.create;
begin
  inherited create;
  fDebuggerCapabilities:=fDebuggerCapabilities+[dbcSoftwareBreakpoint,dbcHardwareBreakpoint, dbcExceptionBreakpoint];
  name:='VEH Debugger';

  fmaxSharedBreakpointCount:=4;

  InjectedEvents:=Tlist.create;
  LastThreadList:=TStringList.create;

  lastthreadlist.Sorted:=true;
  lastthreadlist.Duplicates:=dupIgnore;

  threads:=tmap.Create(ituPtrSize,sizeof(THandle));
end;


destructor TVEHDebugInterface.destroy;
begin
  if heartbeat<>nil then
  begin
    heartbeat.Terminate;
    heartbeat.WaitFor;
    freeandnil(heartbeat);
  end;

  if HasDebugEvent<>0 then
    closehandle(HasDebugEvent);

  if HasHandledDebugEvent<>0 then
    closehandle(HasHandledDebugEvent);

  if injectedEvents<>nil then
    freeandnil(InjectedEvents);

  if threads<>nil then
    freeandnil(threads);

  inherited destroy;
end;

function TVEHDebugInterface.SetThreadContext(hThread: THandle; const lpContext: TContext; isFrozenThread: Boolean=false): BOOL;
var c: PContext;
{$ifdef cpu64}
    c32: PContext32 absolute c;
    i: integer;
{$endif}
begin


  if (not wasInjectedEvent) and isFrozenThread then //use the VEHDebugView context
  begin
    result:=true;

    c:=@VEHDebugView.CurrentContext[0];
    {$ifdef cpu64}
    if not is64bit then
    begin
      //the given context needs to be converted to 32-bit

      //c32.ContextFlags:=lpcontext.ContextFlags;
      c32.Dr0:=lpcontext.Dr0;
      c32.Dr1:=lpcontext.Dr1;
      c32.Dr2:=lpcontext.Dr2;
      c32.Dr3:=lpcontext.Dr3;
      c32.Dr6:=lpcontext.Dr6;
      c32.Dr7:=lpcontext.Dr7;

      c32.SegGs:=lpcontext.SegGs;
      c32.SegFs:=lpcontext.SegFs;
      c32.SegEs:=lpcontext.SegEs;
      c32.SegDs:=lpcontext.SegDs;

      c32.edi:=lpcontext.rdi;
      c32.esi:=lpcontext.rsi;
      c32.ebx:=lpcontext.rbx;
      c32.edx:=lpcontext.rdx;
      c32.ecx:=lpcontext.rcx;
      c32.eax:=lpcontext.rax;
      c32.ebp:=lpcontext.rbp;
      c32.eip:=lpcontext.rip;
      c32.SegCs:=lpcontext.SegCs;
      c32.EFlags:=lpcontext.EFlags;
      c32.esp:=lpcontext.rsp;
      c32.SegSs:=lpcontext.SegSs;

      CopyMemory(@c32.ext, @lpContext.fltsave,sizeof(c32.ext));

      for i:=0 to 7 do
        CopyMemory(@c32.FloatSave.RegisterArea[i*10], @lpContext.fltsave.FloatRegisters[i], 10);

    end else c^:=lpContext;

   // end;// else lpContext:=c^;
    {$else}
    c^:=lpContext;
    {$endif}

    //Interesting effect: result:=NewKernelHandler.SetThreadContext(hThread,lpContext);
  end
  else
    result:=NewKernelHandler.SetThreadContext(hThread,lpContext);
end;

function TVEHDebugInterface.GetThreadContext(hThread: THandle; var lpContext: TContext; isFrozenThread: Boolean=false):  BOOL;
var c: PContext;
{$ifdef cpu64}
    c32: PContext32 absolute c;
    i: integer;
{$endif}

begin
  if (not wasInjectedEvent) and isFrozenThread then //use the VEHDebugView context
  begin
    //OutputDebugString('VEH GetThreadContext. From frozen');
    result:=true;
    c:=@VEHDebugView.CurrentContext[0];
    {$ifdef cpu64}
    if not is64bit then
    begin
      ZeroMemory(@lpContext,sizeof(TContext));
      lpContext.ContextFlags:=c32.ContextFlags;
      lpContext.Dr0:=c32.Dr0;
      lpContext.Dr1:=c32.Dr1;
      lpContext.Dr2:=c32.Dr2;
      lpContext.Dr3:=c32.Dr3;
      lpContext.Dr6:=c32.Dr6;
      lpContext.Dr7:=c32.Dr7;


      lpContext.SegGs:=c32.SegGs;
      lpContext.SegFs:=c32.SegFs;
      lpContext.SegEs:=c32.SegEs;
      lpContext.SegDs:=c32.SegDs;

      lpContext.Rdi:=c32.Edi;
      lpContext.Rsi:=c32.Esi;
      lpContext.Rbx:=c32.Ebx;
      lpContext.Rdx:=c32.Edx;
      lpContext.Rcx:=c32.Ecx;
      lpContext.Rax:=c32.Eax;
      lpContext.Rbp:=c32.Ebp;
      lpContext.Rip:=c32.Eip;
      lpContext.SegCs:=c32.SegCs;
      lpContext.EFlags:=c32.EFlags;
      lpContext.Rsp:=c32.Esp;
      lpContext.SegSs:=c32.SegSs;

      CopyMemory(@lpcontext.FltSave, @c32.ext,sizeof(c32.ext));
    end else lpContext:=c^;
    {$else}
    lpContext:=c^;
    {$endif}


  end
  else
  begin
   // OutputDebugString('VEH GetThreadContext. not frozen');
    result:=NewKernelHandler.GetThreadContext(hThread,lpContext);
  end;
end;

function TVEHDebugInterface.isInjectedEvent: boolean;
begin
  result:=fisInjectedEvent;
end;

function TVEHDebugInterface.WaitForDebugEvent(var lpDebugEvent: TDebugEvent; dwMilliseconds: DWORD): BOOL;
var i: integer;
    c: PContext;
{$ifdef cpu64}
    c32: PContext32 absolute c;
{$endif}
    inj: TInjectedEvent;
    h: THandle;

    r:dword;
begin
  currentThread:=0;  //just making sure

  fisInjectedEvent:=false;
  wasInjectedEvent:=false;
  if injectedEvents.count>0 then
  begin
    wasInjectedEvent:=true;

    //fill in lpDebugEvent
    inj:=TInjectedEvent(injectedEvents[0]);
    lpDebugEvent.dwProcessId:=processid;
    lpDebugEvent.dwThreadId:=inj.ThreadId;
    lpDebugEvent.Exception.dwFirstChance:=1;
    if inj.eventtype=etThreadCreate then
    begin
      //create thread

      lpDebugEvent.dwDebugEventCode:=CREATE_THREAD_DEBUG_EVENT;
      lpDebugEvent.CreateThread.hThread:=OpenThread(THREAD_ALL_ACCESS,false, inj.ThreadId);

      lpDebugEvent.CreateThread.lpStartAddress:=nil;
      lpDebugEvent.CreateThread.lpThreadLocalBase:=nil;

      if threads.GetData(lpDebugEvent.dwThreadId,currentthread)=false then
      begin
        CurrentThread:=OpenThread(THREAD_ALL_ACCESS,false, lpDebugEvent.dwThreadId);
        threads.Add(lpDebugEvent.dwThreadId, currentthread);
      end;

      if CurrentThread<>0 then
      begin
        suspendThread(CurrentThread);
      end;
    end
    else
    begin
      //destroy thread
      lpDebugEvent.dwDebugEventCode:=EXIT_THREAD_DEBUG_EVENT;
      lpDebugEvent.ExitThread.dwExitCode:=0;

      if threads.GetData(lpDebugEvent.dwThreadId,h) then
      begin
        if h<>0 then
          closehandle(h);
        threads.Delete(lpDebugEvent.dwThreadId);
      end;
    end;

    inj.free;

    injectedEvents.Delete(0);
    exit(true);
  end;

  r:=waitforsingleobject(HasDebugEvent, dwMilliseconds);
  result:=r=WAIT_OBJECT_0;
  if result then
  begin
    ZeroMemory(@lpDebugEvent, sizeof(TdebugEvent));


   // lpDebugEvent.dwDebugEventCode:=EXCEPTION_DEBUG_EVENT; //exception
    lpDebugEvent.dwProcessId:=VEHDebugView.ProcessID;
    lpDebugEvent.dwThreadId:=VEHDebugView.ThreadID;
    lpDebugEvent.Exception.dwFirstChance:=1;



    case VEHDebugView.Exception64.ExceptionCode of
      $ce000000: //create process
      begin
        lpDebugEvent.dwDebugEventCode:=CREATE_PROCESS_DEBUG_EVENT;
        lpDebugEvent.CreateProcessInfo.hFile:=0;
        lpDebugEvent.CreateProcessInfo.hProcess:=processhandle;

        if threads.GetData(lpDebugEvent.dwThreadId,lpDebugEvent.CreateProcessInfo.hThread)=false then
        begin
          lpDebugEvent.CreateProcessInfo.hThread:=OpenThread(THREAD_ALL_ACCESS,false, lpDebugEvent.dwThreadId);
          threads.Add(lpDebugEvent.dwThreadId,lpDebugEvent.CreateProcessInfo.hThread);
        end;

        currentthread:=lpDebugEvent.CreateProcessInfo.hThread;
        suspendthread(CurrentThread);
      end;

      $ce000001: //create thread
      begin
        lpDebugEvent.dwDebugEventCode:=CREATE_THREAD_DEBUG_EVENT;
        if threads.GetData(lpDebugEvent.dwThreadId,lpDebugEvent.CreateThread.hThread)=false then
        begin
          lpDebugEvent.CreateThread.hThread:=OpenThread(THREAD_ALL_ACCESS,false, lpDebugEvent.dwThreadId);
          threads.Add(lpDebugEvent.dwThreadId, lpDebugEvent.CreateThread.hThread);
        end;
        lpDebugEvent.CreateThread.lpStartAddress:=nil;
        lpDebugEvent.CreateThread.lpThreadLocalBase:=nil;
        lastthreadlist.Add(inttohex(lpDebugEvent.dwThreadId,1));
        lastthreadpoll:=GetTickCount64;

        currentthread:=lpDebugEvent.CreateThread.hThread;
        suspendthread(CurrentThread);

      end;

      $ce000002: //destroy thread
      begin
        lpDebugEvent.dwDebugEventCode:=EXIT_THREAD_DEBUG_EVENT;
        lpDebugEvent.ExitThread.dwExitCode:=0;

        if threads.GetData(lpDebugEvent.dwThreadId,h) then
        begin
          closehandle(h);
          threads.Delete(lpDebugEvent.dwThreadId);
        end;

        i:=lastthreadlist.indexof(inttohex(lpDebugEvent.dwThreadId,1));
        if i<>-1 then
          lastthreadlist.Delete(i);
      end;


      DBG_PRINTEXCEPTION_C:
      begin
        lpDebugEvent.dwDebugEventCode:=OUTPUT_DEBUG_STRING_EVENT;
        lpDebugEvent.DebugString.fUnicode:=0;
        if is64bit then
        begin
          lpDebugEvent.DebugString.nDebugStringLength:=VEHDebugView.Exception64.ExceptionInformation[0];
          lpDebugEvent.DebugString.lpDebugStringData:=pointer(ptrUint(VEHDebugView.Exception64.ExceptionInformation[1]));
        end
        else
        begin
          lpDebugEvent.DebugString.nDebugStringLength:=VEHDebugView.Exception32.ExceptionInformation[0];
          lpDebugEvent.DebugString.lpDebugStringData:=pointer(ptrUint(VEHDebugView.Exception32.ExceptionInformation[1]));
        end;
      end;

      else
      begin
        //unknown event, standard exception
        lpDebugEvent.dwDebugEventCode:=EXCEPTION_DEBUG_EVENT;
        lpDebugEvent.Exception.ExceptionRecord.ExceptionRecord:=nil;
        {$ifdef cpu64}
        if is64bit then
        begin
          lpDebugEvent.Exception.ExceptionRecord.ExceptionCode:=VEHDebugView.Exception64.ExceptionCode;
          lpDebugEvent.Exception.ExceptionRecord.ExceptionFlags:=VEHDebugView.Exception64.ExceptionFlags;
          lpDebugEvent.Exception.ExceptionRecord.ExceptionRecord:=pointer(VEHDebugView.Exception64.ExceptionRecord);
          lpDebugEvent.Exception.ExceptionRecord.ExceptionAddress:=pointer(VEHDebugView.Exception64.ExceptionAddress);
          lpDebugEvent.Exception.ExceptionRecord.NumberParameters:=VEHDebugView.Exception64.NumberParameters;

          for i:=0 to VEHDebugView.Exception64.NumberParameters-1 do
            lpDebugEvent.Exception.ExceptionRecord.ExceptionInformation[i]:=VEHDebugView.Exception64.ExceptionInformation[i];
        end
        else
        {$endif}
        begin
          lpDebugEvent.Exception.ExceptionRecord.ExceptionCode:=VEHDebugView.Exception32.ExceptionCode;
          lpDebugEvent.Exception.ExceptionRecord.ExceptionFlags:=VEHDebugView.Exception32.ExceptionFlags;
          lpDebugEvent.Exception.ExceptionRecord.ExceptionRecord:=pointer(ptrUint(VEHDebugView.Exception32.ExceptionRecord));
          lpDebugEvent.Exception.ExceptionRecord.ExceptionAddress:=pointer(ptrUint(VEHDebugView.Exception32.ExceptionAddress));
          lpDebugEvent.Exception.ExceptionRecord.NumberParameters:=VEHDebugView.Exception32.NumberParameters;

          for i:=0 to VEHDebugView.Exception32.NumberParameters-1 do
            lpDebugEvent.Exception.ExceptionRecord.ExceptionInformation[i]:=VEHDebugView.Exception32.ExceptionInformation[i];
        end;

        if lpdebugEvent.Exception.ExceptionRecord.ExceptionCode=EXCEPTION_BREAKPOINT then
        begin
          c:=@VEHDebugView.CurrentContext[0];
          {$ifdef cpu64}
          if is64bit then
            c.Rip:=c.rip+1
          else
            c32.Eip:=c32.eip+1;
          {$else}
            c.eip:=c.eip+1
          {$endif}



        end;

      end;
    end;

    hasPausedProcess:=true;

  end;

  if (lastthreadpoll>0) and (gettickcount64>lastthreadpoll+500) then
    DoThreadPoll; //adds injected events if needed
end;

function TVEHDebugInterface.ContinueDebugEvent(dwProcessId: DWORD; dwThreadId: DWORD; dwContinueStatus: DWORD): BOOL;
begin
  hasPausedProcess:=false;
  VEHDebugView.ContinueMethod:=dwContinueStatus;

  if currentthread<>0 then
  begin
    resumeThread(currentThread);
    currentThread:=0;
  end;

  if wasInjectedEvent=false then
    SetEvent(HasHandledDebugEvent);



  result:=true;
end;

function TVEHDebugInterface.DebugActiveProcess(dwProcessId: DWORD): WINBOOL;
var
  s: tstringlist;
  e: integer;
  prefix: string;
  testptr: ptruint;
  mi: tmoduleinfo;
  cfm: THandle;
  err: boolean;
begin
  try
    debuggerAttachStatus:='Attaching VEH Debug';
    if dwProcessID<>processhandler.processid then
    begin
      processhandler.processid:=dwProcessID;

      Open_Process;
      symhandler.reinitialize;
    end;

    is64bit:=processhandler.is64Bit;
    if is64bit then
      prefix:='-x86_64'
    else
      prefix:='-i386';


    result:=false;

   { if symhandler.getmodulebyname('vehdebug'+prefix+'.dll',mi) then
      exit; //no reattach supported right now     }




    CreateGUID(guid);
    guidstring:='"'+GUIDToString(guid)+'"';

    //guidstring:='Global\'+GUIDToString(guid);
    OutputDebugString('Creating filemap with name '+pchar(guidstring));
    debuggerAttachStatus:='Creating filemap';
    ConfigFileMapping:=CreateFileMapping(INVALID_HANDLE_VALUE,nil,PAGE_READWRITE,0,sizeof(TVEHDebugSharedMem),pchar(guidstring));

    if ConfigFileMapping=0 then
    begin
      e:=getlasterror;
      OutPutDebugString('Failed:'+inttostr(e));
      raise exception.Create(Format(
        rsErrorWhileTryingToCreateTheConfigurationStructure, [inttostr(e)]));
    end;

    OutPutDebugString('Created the filemap');

    VEHDebugView:=MapViewOfFile(ConfigFileMapping,FILE_MAP_READ or FILE_MAP_WRITE,0,0,sizeof(TVEHDebugSharedMem));
    if (VEHDebugView=nil) then
    begin
      e:=GetLastError;
      closehandle(ConfigFileMapping);

      OutputDebugString('MapViewOfFile failed: '+inttostr(e));

      raise exception.Create(Format(rsCheatEngineFailedToGetIntoTheConfig, [
        inttostr(e)]));
    end;

    ZeroMemory(VEHDebugView,sizeof(TVEHDebugSharedMem));

    Heartbeat:=THeartBeat.Create(true);
    THeartBeat(Heartbeat).owner:=self;
    HeartBeat.Priority:=tpHighest;
    HeartBeat.Start;

    debuggerAttachStatus:='Started hearbeat';

    VEHDebugView.ThreadWatchMethod:=0; //vehthreadwatchmethod;
    if VEHRealContextOnThreadCreation then
      VEHDebugView.ThreadWatchMethodConfig:=TPOLL_TCREATEREALCONTEXT
    else
      VEHDebugView.ThreadWatchMethodConfig:=0;;



    HasDebugEvent:=CreateEvent(nil, false, false, nil);
    HasHandledDebugEvent:=CreateEvent(nil, false, false, nil);

    debuggerAttachStatus:='Duplicating handles';

    if not DuplicateHandle(GetCurrentProcess, HasDebugEvent, processhandle, @VEHDebugView^.HasDebugEvent, 0, false, DUPLICATE_SAME_ACCESS) then
      raise exception.Create(
        rsFailureDuplicatingTheEventHandlesToTheOtherProcess);

    if not DuplicateHandle(GetCurrentProcess, HasHandledDebugEvent, processhandle, @VEHDebugView^.HasHandledDebugEvent, 0, false, DUPLICATE_SAME_ACCESS) then
      raise exception.Create(
        rsFailureDuplicatingTheEventHandlesToTheOtherProcess);

    if not DuplicateHandle(GetCurrentProcess, ConfigFileMapping, processhandle, @cfm, 0, false, DUPLICATE_SAME_ACCESS) then
      raise exception.Create(rsFailureDuplicatingTheFilemapping);


    debuggerAttachStatus:='Waiting for kernel32';
    symhandler.waitforsymbolsloaded(true,'kernel32.dll');

    testptr:=symhandler.getAddressFromName('"vehdebug'+prefix+'.InitializeVEH"',false,err);
    if err or (testptr=0) then
    begin
      try
        debuggerAttachStatus:='Injecting vehdebug'+prefix+'.dll';
        InjectDll(cheatenginedir+'vehdebug'+prefix+'.dll');
      except
      end;
    end;


    debuggerAttachStatus:='reloading symbols';
    symhandler.reinitialize;
    debuggerAttachStatus:='waiting for symbols from vehdebug'+prefix+'.dll';
    symhandler.waitforsymbolsloaded(true,'vehdebug'+prefix+'.dll');

    testptr:=symhandler.getAddressFromName('"vehdebug'+prefix+'.InitializeVEH"');

    s:=tstringlist.Create;
    try
      s.Clear;
      s.Add('"vehdebug'+prefix+'.ConfigName":');
      s.add('db '''+guidstring+''',0');

      s.Add('"vehdebug'+prefix+'.fm":');
      if processhandler.is64Bit then
        s.add('dq '+inttohex(cfm,8))
      else
        s.add('dd '+inttohex(cfm,8));

      s.Add('CreateThread("vehdebug'+prefix+'.InitializeVEH")');

      //Clipboard.SetTextBuf(pchar(s.text));
      debuggerAttachStatus:='Assembling VEH injection script';

      if autoassemble(s,false) then
      begin
        //debugger is attached and ready to go
        active:=true;
        THeartBeat(Heartbeat).StartVersionCheck;
        debuggerAttachStatus:='VEH injection script assembled succesful';
      end
      else
      begin
        debuggerAttachStatus:='VEH injection script failed';
//        showmessage(s.text);
      end;

      result:=true;
    finally
      s.free;
    end;

    debuggerAttachStatus:='Ready';
  except
    on e: exception do
    begin
      debuggerAttachStatus:='Exception: '+e.message;
      messagebox(0, pchar(e.message), pchar(utf8toansi(rsVEHDebugError)), MB_OK or MB_ICONERROR);
      result:=false;
    end;
  end;
end;

function TVEHDebugInterface.DebugActiveProcessStop(dwProcessID: DWORD): WINBOOL;
var
  prefix: string;
  s: Tstringlist;
begin
  result:=false;

  if active and (processhandler.processid=processhandler.processid) then
  begin
    try
      if is64Bit then
        prefix:='-x86_64'
      else
        prefix:='-i386';

      s:=tstringlist.Create;
      try
        s.Add('CreateThread("vehdebug'+prefix+'.UnloadVEH")');
        if autoassemble(s,false) then
        begin
          active:=false;
          result:=true;
        end;
      finally
        s.free;
      end;
    except
    end;
  end;

  if heartbeat<>nil then
  begin
    heartbeat.Terminate;
    heartbeat.WaitFor;
    freeandnil(heartbeat);
  end;
end;

procedure TVEHDebugInterface.SynchronizeNoBreakList;
var i: integer;
begin
  for i:=0 to min(63, length(noBreakList)-1) do
    VEHDebugView.NoBreakList[i]:=noBreakList[i];

  VEHDebugView.NoBreakListSize:=min(63, length(noBreakList)-1);
end;

procedure TVEHDebugInterface.AddToNoBreakList(threadid: integer);
begin
  inherited AddToNoBreakList(threadid);
  SynchronizeNoBreakList;
end;

procedure TVEHDebugInterface.RemoveFromNoBreakList(threadid: integer);
begin
  inherited RemoveFromNoBreakList(threadid);
  SynchronizeNoBreakList;
end;


procedure TVEHDebugInterface.DoThreadPoll;
var
  currentlist: tstringlist;
  i: integer;

  inj: TInjectedEvent;
begin
  currentlist:=tstringlist.create;

  GetThreadList(currentlist);

  for i:=0 to currentlist.count-1 do
  begin
    if lastthreadlist.IndexOf(currentlist[i])=-1 then
    begin
      //new thread event
      inj:=TInjectedEvent.create;
      inj.eventtype:=etThreadCreate;
      inj.threadid:=strtoint('$'+currentlist[i]);

      injectedEvents.Add(inj);
      lastthreadlist.Add(currentlist[i]);
    end;
  end;

  i:=0;
  while i<=lastthreadlist.count-1 do
  begin
    if currentlist.IndexOf(lastthreadlist[i])=-1 then
    begin
      //destroyed thread event
      inj:=TInjectedEvent.create;
      inj.eventtype:=etThreadDestroy;
      inj.threadid:=strtoint('$'+lastthreadlist[i]);

      injectedEvents.Add(inj);
      lastthreadlist.Delete(i);
    end
    else
      inc(i);
  end;

  currentlist.free;

  lastthreadpoll:=GetTickCount64;
end;

function TVEHDebugInterface.canUseIPT: boolean;
begin
  canUseIPT:=true;
end;

{$endif}

end.

