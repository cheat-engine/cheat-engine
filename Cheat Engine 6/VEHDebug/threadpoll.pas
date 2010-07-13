unit threadpoll;
{
Keeps a list of all the threads and notifies the debugger when a change has happened
}

{$mode delphi}

interface

uses
  jwawindows,windows,Classes, SysUtils,init;

type TThreadPoller=class(tthread)
  private
    threadlist: TList;
    procedure GetCurrentList(list: tlist);
    procedure UpdateList;
    procedure CreateThreadEvent(threadid: dword);
    procedure DestroyThreadEvent(threadid: dword);

  public
    procedure execute; override;
end;

var ThreadPoller: TThreadPoller;

implementation

uses DebugHandler;

procedure TThreadPoller.CreateThreadEvent(threadid: dword);
var
  ep: TEXCEPTIONPOINTERS;
  er: TEXCEPTIONRECORD;
  c: Tcontext;
begin
  //outputdebugstring(pchar('TThreadPoller.CreateThreadEvent('+inttohex(threadid,1)+')'));
  ep.ContextRecord:=@c;
  ep.ExceptionRecord:=@er;
  er.NumberParameters:=0;

  er.ExceptionCode:=$ce000001;
  InternalHandler(@ep,threadid);
end;

procedure TThreadPoller.DestroyThreadEvent(threadid: dword);
var
  ep: TEXCEPTIONPOINTERS;
  er: TEXCEPTIONRECORD;
  c: Tcontext;
begin
  //outputdebugstring(pchar('TThreadPoller.DestroyThreadEvent('+inttohex(threadid,1)+')'));
  ep.ContextRecord:=@c;
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
 // i,j: integer;
  cpi: dword;
begin
  cpi:=GetCurrentProcessId();
  ths:=CreateToolhelp32Snapshot(TH32CS_SNAPALL,cpi);
 // i:=0;
  //j:=0;

  if ths<>INVALID_HANDLE_VALUE then
  begin
    zeromemory(@lpte,sizeof(lpte));
    lpte.dwSize:=sizeof(lpte);
    check:=Thread32First(ths, lpte);
    while check do
    begin
      if lpte.th32OwnerProcessID=cpi then
      begin
        list.add(pointer(lpte.th32ThreadID));
        //inc(i);
      end;
      check:=Thread32next(ths,lpte);
      //inc(j);
    end;

   { if list.count=0 then
      outputdebugstring('GetCurrentList returned 0 threads');

    if i=0 then
      outputdebugstring('It''s actually 0');

    outputdebugstring(pchar('j='+inttostr(j)));  }



    closehandle(ths);
  end;// else outputdebugstring('GetCurrentList failed on CreateToolhelp32Snapshot');
end;

procedure TThreadPoller.UpdateList;
var newlist: Tlist;
i: integer;
begin
 // OutputDebugString('TThreadPoller.UpdateList');

  newlist:=tlist.create;
  GetCurrentList(newlist);

 // outputdebugstring(pchar(format('newlist.count=%d oldlist.count=%d',[newlist.count, threadlist.count])));

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

      sleep(1000);
      UpdateList;
    end;
  finally
    threadlist.free;
  end;
end;

end.

