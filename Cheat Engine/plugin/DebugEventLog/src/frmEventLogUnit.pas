unit frmEventLogUnit;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, cepluginsdk, ExtCtrls, Menus;

const wm_debugevent = wm_user+1;

type
  TfrmEventLog = class(TForm)
    ListView1: TListView;
    Panel1: TPanel;
    Button2: TButton;
    Button1: TButton;
    PopupMenu1: TPopupMenu;
    Clear1: TMenuItem;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Clear1Click(Sender: TObject);
  private
    { Private declarations }
    debugeventfunctionid: integer;
    logging: boolean;
    procedure onDebugEvent(var m: TMessage); message wm_debugevent;
  public
    { Public declarations }
  end;

type TControlwindowThread= class(TThread)
  public
    procedure Execute; override;
end;

var
  frmEventLog: TfrmEventLog;
  controlwindowthread: TControlwindowThread;

implementation

{$R *.lfm}

uses exportimplementation;

procedure TControlwindowThread.execute;
begin
  freeonterminate:=true;

  MainThreadID:=GetCurrentThreadId;
  Application.initialize;
  Application.CreateForm(TfrmEventLog, frmEventLog);
  application.run;

  ce_exported.showmessage('end of app');
end;

function _OnDebugEvent(debugevent: PDebugEvent): integer; stdcall;
var x: PDebugEvent;
begin
  //copy the structure so it can be read out by the gui without
  getmem(x,sizeof(TDebugEvent));
  x^:=debugevent^;
  postmessage(frmEventLog.handle, wm_debugevent, ptruint(x),0);

  result:=0; //I want ce to handle this debug event. Perhaps in the future add some filtering, but for now, this is enough
end;

procedure TfrmEventLog.onDebugEvent(var m: TMessage);
var li: tlistitem;
    x: PDebugEvent;
    pid: string;
    tid: string;
    eventname: string;
    details: string;
begin
  x:=PDebugEvent(ptruint(m.WParam));
  pid:=inttohex(x.dwProcessId,4);
  tid:=inttohex(x.dwThreadId,4);


  //might want to add reading the filenames using readprocessmemory
  details:='';
  case x.dwDebugEventCode of
    EXCEPTION_DEBUG_EVENT:
    begin
      eventname:='Exception';
      details:='address:'+inttohex(ptruint(x.Exception.ExceptionRecord.ExceptionAddress),8);
    end;

    CREATE_THREAD_DEBUG_EVENT:
    begin
      eventname:='Create Thread';
      details:='Startaddress:'+inttohex(ptruint(x.CreateThread.lpStartAddress),8);
    end;

    CREATE_PROCESS_DEBUG_EVENT:
    begin
      eventname:='Create Process';
      details:='entry='+inttohex(ptruint(x.CreateProcessInfo.lpStartAddress),8);
    end;

    EXIT_THREAD_DEBUG_EVENT:
    begin
      eventname:='Exit Thread';
      details:='exitcode='+inttostr(x.ExitThread.dwExitCode);
    end;

    EXIT_PROCESS_DEBUG_EVENT:
    begin
      eventname:='Exit Process';
      details:='exitcode='+inttostr(x.ExitProcess.dwExitCode);
    end;

    LOAD_DLL_DEBUG_EVENT:
    begin
      eventname:='Load DLL';
      details:='hInstance='+inttohex(ptruint(x.LoadDll.lpBaseOfDll),8);
    end;

    UNLOAD_DLL_DEBUG_EVENT:
    begin
      eventname:='Unload DLL';
      details:='hInstance='+inttohex(ptruint(x.UnloadDll.lpBaseOfDll),8);
    end;

    OUTPUT_DEBUG_STRING_EVENT:
    begin
      eventname:='OutputDebugString';
      details:='Address:'+inttohex(ptruint(x.DebugString.lpDebugStringData),8);
    end;

    RIP_EVENT:  eventname:='RIP';
    else eventname:='?Unknown?';
  end;



  
  freemem(x); //this object was created at _OnDebugEvent, so free it

  //now add it
  li:=listview1.Items.Add;
  li.Caption:=inttostr(gettickcount);
  li.SubItems.Add(PID);
  li.SubItems.Add(TID);
  li.SubItems.Add(eventname);
  li.SubItems.Add(details);

end;

procedure TfrmEventLog.Button1Click(Sender: TObject);
var init: TFunction3;
begin
  init.callbackroutine:=@_OnDebugEvent;
  debugeventfunctionid:=ce_exported.registerfunction(thispluginid, ptOnDebugEvent, @init);
  logging:=true;
  button1.Enabled:=false;
  button2.Enabled:=true;
end;

procedure TfrmEventLog.Button2Click(Sender: TObject);
begin
  ce_exported.unregisterfunction(thispluginid,debugeventfunctionid);
  logging:=false;
  button2.Enabled:=false;
  button1.Enabled:=true;
end;

procedure TfrmEventLog.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  frmEventLog:=nil;
  if logging then Button2.Click;
  action:=cafree;
end;

procedure TfrmEventLog.Clear1Click(Sender: TObject);
begin
  listview1.Clear;
end;

initialization

end.
