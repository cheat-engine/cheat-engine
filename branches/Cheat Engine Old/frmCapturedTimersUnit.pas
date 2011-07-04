unit frmCapturedTimersUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type TTimerMessage = record
  handle: Thandle;
  TimerID: dword;
  Timerproc:dword;
  count: integer;
end;

type
  TfrmCapturedTimers = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    ListView1: TListView;
    Edit1: TEdit;
    Button2: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    TimerMessages: array of TTimerMessage;
  public
    { Public declarations }
    windowhandle:thandle;
    procedure receivedtimermessage(var Message: TMessage); message WM_USER+1;
  end;

var
  frmCapturedTimers: TfrmCapturedTimers;

implementation

uses frmDissectwindowUnit;

{$R *.dfm}

procedure TfrmCapturedTimers.receivedtimermessage(var Message: TMessage);
var i,j: integer;
    handle: Thandle;
    TimerID: dword;
    Timerproc:dword;
    title,classname: pchar;

begin
  try
  handle:=frmdissectWindow.CETimerHookData.window;
  timerid:=frmdissectWindow.CETimerHookData.timerid;
  timerproc:=frmdissectWindow.CETimerHookData.timerproc;
  frmdissectWindow.CETimerHookData.processed:=true;

  getmem(title,100);
  getmem(classname,100);
  try
    getwindowtext(handle,title,100);
    GetClassName(handle,classname,100);

    j:=-1;
    for i:=0 to length(timermessages)-1 do
    begin
      if (handle=timermessages[i].handle) and
         (timerid=timermessages[i].TimerID) and
         (timerproc=timermessages[i].Timerproc) then
      begin
        j:=i;
        break;
      end;
    end;

    if j=-1 then //add a new one
    begin
      j:=length(timermessages);
      setlength(timermessages,j+1);
      timermessages[j].handle:=handle;
      timermessages[j].TimerID:=timerid;
      timermessages[j].Timerproc:=timerproc;
      timermessages[j].count:=0;
      listview1.Items.Add.Caption:=IntToHex(handle,8);
      listview1.Items[listview1.Items.Count-1].SubItems.add(title);
      listview1.Items[listview1.Items.Count-1].SubItems.add(classname);
      listview1.Items[listview1.Items.Count-1].SubItems.add(IntToStr(TimerID));
      listview1.Items[listview1.Items.Count-1].SubItems.add(IntToHex(TimerProc,8));
      listview1.Items[listview1.Items.Count-1].SubItems.add('1');
    end;
    inc(timermessages[j].count);

    listview1.Items[j].SubItems[4]:=IntToStR(timermessages[j].count);
  except

  end;
  //find it in the listview and edit it's count

  finally
    freemem(title);
    freemem(classname);
  end;
end;

procedure TfrmCapturedTimers.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
end;

procedure TfrmCapturedTimers.Button1Click(Sender: TObject);
begin
  close;
end;

procedure TfrmCapturedTimers.Button2Click(Sender: TObject);
var i,j: integer;
    count: integer;
begin
  val(edit1.Text,count,i);
  if i>0 then beep;

  for i:=0 to count-1 do
  begin
    for j:=0 to length(TimerMessages)-1 do
    begin
      if listview1.Items[j].Selected then
        sendmessage(timermessages[j].handle,wm_timer,timermessages[j].TimerID,timermessages[j].Timerproc);
    end;
  end;
end;

end.
