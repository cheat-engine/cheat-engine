unit threadpoll;
{
Keeps a list of all the threads and notifies the debugger when a change has happened
}

{$mode delphi}

interface

uses
  jwawindows,windows,Classes, SysUtils,init, extcont, simpleThread, VEHDebugSharedMem;


type TThreadPoller=class(TSimpleThread)
  private
    threadlist: TList;
    procedure GetCurrentList(list: tlist);
    procedure UpdateList;
    procedure CreateThreadEvent(threadid: dword);
    procedure DestroyThreadEvent(threadid: dword);
  public
    procedure execute; override;
end;

//var ThreadPoller: TThreadPoller;

implementation

uses DebugHandler;


//var debug_oldcontext, debug_newcontext: TContext;

procedure TThreadPoller.CreateThreadEvent(threadid: dword);
var
{$ifdef cpu32}
  c: TEContext;
{$else}
  c: TContext;
{$endif}

  cp: PContext;
  ep: TEXCEPTIONPOINTERS;
  er: TEXCEPTIONRECORD;

  th: thandle;
  useValidContext: boolean;
  hasValidContext: boolean;
begin
  OutputDebugString('CreateThreadEvent');

  useValidContext:=((VEHSharedMem.ThreadWatchMethodConfig and TPOLL_TCREATEREALCONTEXT)>0) and (GetCurrentThreadId<>threadid);

  if usevalidcontext then
  begin
    cp:=@c;
    ep.ContextRecord:=cp;

    OutputDebugString('usevalidcontext=true');

    th:=OpenThread(THREAD_GET_CONTEXT or THREAD_SET_CONTEXT or THREAD_SUSPEND_RESUME, false, threadid);
    if th<>0 then
    begin
      SuspendThread(th);
      zeromemory(@c, sizeof(c));
      c.ContextFlags:=CONTEXT_ALL {$ifdef cpu32} or CONTEXT_EXTENDED{$endif};

      hasValidContext:=GetThreadContext(th, cp^);
      if not hasValidContext then
      begin
        ep.ContextRecord:=nil;
        OutputDebugString(pchar(Format('Failure getting context th=%d @c=%p', [th, @c])));
      end;

     // debug_oldcontext:=c;

    end;


  end
  else
  begin
    ep.ContextRecord:=nil;
    OutputDebugString('usevalidcontext=false');
  end;

  ep.ExceptionRecord:=@er;
  er.NumberParameters:=0;

  er.ExceptionCode:=$ce000001;
  er.ExceptionRecord:=nil;
  OutputDebugString('Emulating CreateThreadEvent');

  InternalHandler(@ep,threadid);

  OutputDebugString('After Emulating CreateThreadEvent');

  if usevalidcontext then
  begin
    if (th<>0) and (hasValidContext) then
    begin

      //OutputDebugString(pchar(Format('old context=%p new context=%p', [@debug_oldcontext, @debug_newcontext])));

      //debug_newcontext:=c;

      c.ContextFlags:=CONTEXT_ALL {$ifdef cpu32} or CONTEXT_EXTENDED{$endif};
      SetThreadContext(th, cp^);
      ResumeThread(th);
    end;

    closehandle(th);
  end;

end;

procedure TThreadPoller.DestroyThreadEvent(threadid: dword);
var
  ep: TEXCEPTIONPOINTERS;
  er: TEXCEPTIONRECORD;
begin
  ep.ContextRecord:=nil;
  ep.ExceptionRecord:=@er;
  er.NumberParameters:=0;

  er.ExceptionCode:=$ce000002; //destroythread
  InternalHandler(@ep,threadid);
end;


procedure TThreadPoller.GetCurrentList(list: tlist);
var
  ths: thandle;
  lpte: TThreadEntry32;
  check: boolean;
  cpi: dword;
begin
  cpi:=GetCurrentProcessId();
  ths:=CreateToolhelp32Snapshot(TH32CS_SNAPALL,cpi);

  if ths<>INVALID_HANDLE_VALUE then
  begin
    zeromemory(@lpte,sizeof(lpte));
    lpte.dwSize:=sizeof(lpte);
    check:=Thread32First(ths, lpte);
    while check do
    begin
      if lpte.th32OwnerProcessID=cpi then
        list.add(pointer(lpte.th32ThreadID));

      check:=Thread32next(ths,lpte);
    end;


    closehandle(ths);
  end;
end;

procedure TThreadPoller.UpdateList;
var newlist: Tlist;
i: integer;
begin
  newlist:=tlist.create;
  GetCurrentList(newlist);

  //now try to find the differences


  //is there a threadid that's not in the current threadlist?
  for i:=0 to newlist.Count-1 do
    if threadlist.IndexOf(newlist[i])=-1 then //not found
      CreateThreadEvent(Dword(newlist[i]));

  for i:=0 to threadlist.count-1 do
    if newlist.IndexOf(threadlist[i])=-1 then //the new list doesn't contain this threadid
      DestroyThreadEvent(Dword(threadlist[i]));

  //free the old list and make the new list the current list
  threadlist.free;
  threadlist:=newlist;
end;

procedure TThreadPoller.execute;
begin
  threadlist:=TList.Create;
  try
    GetCurrentList(threadlist);

    while not terminated do
    begin
      sleep(500);
      UpdateList;
    end;
  finally
    OutputDebugString('TThreadPoller terminated');
    threadlist.free;
  end;
end;




end.

