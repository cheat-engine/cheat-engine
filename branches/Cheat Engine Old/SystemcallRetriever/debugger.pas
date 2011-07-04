unit debugger;

interface

uses Classes,windows,sysutils,Messages,forms,dialogs,controls,Graphics,newkernelhandler;


type TDebugger = class(TThread)
  private
    originalbyte: byte;
    debugging: boolean;

    threadlist: array of array [0..4] of dword;
    procedure SetSingleStepping(threadid:dword);
    procedure UpdateList;
    procedure line;
    procedure itsdone;
    procedure showphase;
  public
    procedure Execute; override;
  end;


type tcallnumber=record
  callnumber:dword;
  parametercount: byte;
end;

var ok:boolean;
    context:_context;
    debuggerdone: boolean;
    debuggedprocesshandle: thandle;

    callnumbers: array [0..3] of array of tcallnumber;

implementation

uses mainunit;

procedure tdebugger.showphase;
begin
  form1.listbox1.Lines.Add('Phase '+inttostr(phase+1)+'/4 (please wait....)');
end;

procedure tdebugger.itsdone;
begin
  form1.ListBox1.Lines.Add('-----done----');

  debugging:=false;
  terminateprocess(debuggedprocesshandle,0);
  
  form1.Timer1.Enabled:=false;
  postmessage(sharedmem^.RetrieverWindowHandle,wm_user+1,0,0);
end;

procedure tdebugger.line;
begin
  form1.ListBox1.Lines.Add('-------------');
end;

procedure TDebugger.UpdateList;
var i:integer;
    ar: dword;
    buf: byte;
    found:boolean;
begin
  if (context.Eax and $1000)=0 then exit; //not one i need

  buf:=0;
  if (paramlist<>0) then
    readprocessmemory(processhandle,pointer(paramlist+(context.Eax and $FFF)),@buf,1,ar);

  found:=false;
  for i:=0 to length(callnumbers[phase])-1 do
    if callnumbers[phase][i].callnumber=(context.eax and $FFF) then
    begin
      found:=true;
      break;
    end;

  if not found then
  begin
    setlength(callnumbers[phase],length(callnumbers[phase])+1);
    callnumbers[phase][length(callnumbers[phase])-1].callnumber:=context.Eax and $FFF;
    callnumbers[phase][length(callnumbers[phase])-1].parametercount:=buf;
  end;

  form1.ListBox1.Lines.Add(IntToHex(context.Eip,8)+' - '+IntToHex(context.Eax and $FFF,4)+' - '+IntToHex(buf,2));
end;

procedure TDebugger.SetSingleStepping(Threadid: dword);
var i: integer;
    context: _context;
begin
  for i:=0 to length(threadlist)-1 do
    if threadlist[i,0]=ThreadId then
    begin
      context.ContextFlags:=CONTEXT_FULL;
      getthreadcontext(threadlist[i,1],context);

      context.ContextFlags:=CONTEXT_FULL; //CONTEXT_CONTROL;
      context.EFlags:=context.EFlags or $100;
      setthreadcontext(threadlist[i,1],context);
      break;
    end;
end;

procedure TDebugger.Execute;
var startupinfo:_STARTUPINFOA;
    int3: byte;
    devent: _Debug_EVENT;
    processinfo:_Process_information;       //make an array of this


    KiFastSystemCallPos: dword;
    ntdll:thandle;

    bpset: boolean;
    x,op:dword;
    currentthread:dword;
    i:integer;
    processhandle:thandle;
    buf:array [0..4] of byte;
    ar:dword;
    hasrun:boolean;

begin
  hasrun:=false;
  debuggerdone:=false;

  bpset:=false;

  FillMemory(@STARTUPINFO,sizeof(STARTUPINFO),0);
  STARTUPINFO.cb :=sizeof(STARTUPINFO);

  synchronize(showphase);

  if (not Createprocess(nil,pchar('systemcallsignal.exe '+IntToStr(phase+1)),nil,nil,false,(DEBUG_PROCESS or NORMAL_PRIORITY_CLASS),nil,nil,Startupinfo,processinfo)) then
  begin
    messagebox(0,'Systemcaller couldn''t be executed','Systemcallretriever error',mb_ok);
    exit;
  end;

  processhandle:=processinfo.hProcess;
  debuggedprocesshandle:=processinfo.hProcess;

  setlength(threadlist,1);
  threadlist[0,0]:=processinfo.dwThreadId;
  threadlist[0,1]:=processinfo.hThread;



  debugging:=true;


  while not terminated and debugging do
  begin
    if WaitForDebugEvent(devent,4000) then  //4 seconds wont cause a hog I hope
    begin
      case devent.dwDebugEventCode of
        EXCEPTION_DEBUG_EVENT:
        begin
          for i:=0 to length(threadlist)-1 do
            if threadlist[i,0]=devent.dwThreadId then
            begin
              context.ContextFlags:=CONTEXT_FULL;
              getthreadcontext(threadlist[i,1],context);
              currentthread:=i;
            end;

          if (sharedmem^.Infunction) then SetSinglestepping(devent.dwThreadId);
          
          if devent.Exception.ExceptionRecord.ExceptionCode=exception_breakpoint then
          begin

            debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);
            continue;
          end;

          if devent.Exception.ExceptionRecord.ExceptionCode=exception_single_step then
          begin
            if (sharedmem^.Infunction) then
            begin
              hasrun:=true;
              if (readprocessmemory(processhandle,pointeR(context.eip),@buf[0],4,ar)) and
                 (
                  ((buf[0]=$CD) and (buf[1]=$2e))
                 or
                  ((buf[0]=$0f) and (buf[1]=$34))
                  ) then synchronize(updatelist);
            end else if hasrun then synchronize(itsdone);

            debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);
            continue;
          end;

          if (devent.Exception.ExceptionRecord.ExceptionCode=STATUS_ACCESS_VIOLATION) then
          begin
            debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);
            continue;
          end;


        //  messagebox(0,pchar('unhandled exception '+IntToHex(devent.Exception.ExceptionRecord.ExceptionCode,8)),'unhandled exception',mb_ok);
          debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_EXCEPTION_NOT_HANDLED);
          continue;

        end;

        CREATE_THREAD_DEBUG_EVENT:
        begin
          messagebox(0,'thread','thread',mb_ok);
          setlength(threadlist,length(threadlist)+1);
          threadlist[length(threadlist)-1,0]:=devent.dwThreadId;
          threadlist[length(threadlist)-1,1]:=devent.CreateThread.hThread;

          threadlist[length(threadlist)-1,2]:=dword(devent.CreateThread.lpStartAddress);
          threadlist[length(threadlist)-1,3]:=dword(devent.CreateThread.lpThreadLocalBase);
          SetSinglestepping(devent.dwThreadId);

          debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);
        end;

        OUTPUT_DEBUG_STRING_EVENT:
        begin
          if devent.DebugString.fUnicode=0 then
          begin
            //8 bit ascii
            synchronize(line);
          end;

          debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);
        end;

        else debugging:=ContinueDebugEvent(devent.dwProcessId,devent.dwThreadId,DBG_CONTINUE);
      end;
    end;
  end;

  debuggerdone:=true;
  freeonterminate:=true;
  sharedmem^.Infunction:=false;

end;


end.

 
