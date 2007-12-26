unit formChangedAddresses;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,cefuncproc;

type
  TfrmChangedAddresses = class(TForm)
    Changedlist: TListBox;
    OKButton: TButton;
    lblInfo: TLabel;
    procedure OKButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ChangedlistDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmChangedAddresses: TfrmChangedAddresses;

implementation

{$R *.dfm}

uses debugger, MainUnit;

procedure TfrmChangedAddresses.OKButtonClick(Sender: TObject);
var temp: dword;
    i: integer;
begin
  if OKButton.caption='Stop' then
  begin
    if WaitForSingleObject(semaphore,10000)=WAIT_FAILED then
      raise exception.Create('OMG! I can''t stop it!');

    try
      if debuggerthread=nil then exit;

      debuggerthread.Suspend;
      debuggerthread.breakpointset:=false;

      zeromemory(@debuggerthread.DRRegs,sizeof(debuggerthread.DRRegs));
      debuggerthread.DRRegs.ContextFlags:=CONTEXT_DEBUG_REGISTERS;
      debuggerthread.DRRegs.Dr7:=reg0set or reg1set or reg2set or reg3set;

      with debuggerthread do
      for i:=0 to length(threadlist)-1 do
      begin
        suspendthread(threadlist[i,1]);
        setthreadcontext(threadlist[i,1],DRRegs);
        resumethread(threadlist[i,1]);
      end;

      debuggerthread.Resume;

    finally
      releasesemaphore(semaphore,1,nil);
    end;

    okButton.Caption:='Close';
  end else
  modalresult:=mrok;

end;

procedure TfrmChangedAddresses.FormClose(Sender: TObject;
  var Action: TCloseAction);
var temp:dword;
    i: integer;
begin
  action:=caFree;
  if OKButton.caption='Stop' then
  begin
    if WaitForSingleObject(semaphore,10000)=WAIT_FAILED then
      raise exception.Create('OMG! I can''t stop it!');
    debuggerthread.breakpointset:=false;

    zeromemory(@debuggerthread.DRRegs,sizeof(debuggerthread));
    debuggerthread.DRRegs.ContextFlags:=CONTEXT_DEBUG_REGISTERS;
    for i:=0 to length(debuggerthread.threadlist)-1 do
    begin
      suspendthread(debuggerthread.threadlist[i][1]);
      if not SetThreadContext(debuggerthread.threadlist[i][1],debuggerthread.DRRegs) then showmessage('I cant seem to remove the breakpoint from one of the threads!');
      resumethread(debuggerthread.threadlist[i][1]);
    end;
    
    releasesemaphore(semaphore,1,nil);
  end;
end;

procedure TfrmChangedAddresses.FormShow(Sender: TObject);
begin
  OKButton.Caption:='Stop';
end;

procedure TfrmChangedAddresses.ChangedlistDblClick(Sender: TObject);
var i: integer;
    ad: dword;
begin
  ad:=strtoint('$'+changedlist.Items[changedlist.itemindex]);
  with mainform do
  begin
    i:=0;
    while (i<numberofrecords) and
          (
            (memrec[i].Address<>ad) or
            (memrec[i].VarType<>2)
          )  do
          inc(i);
    //find out of this record is in the list
    if i=NumberOfRecords then
    begin
      inc(NumberOfRecords);
      ReserveMem;

      memrec[NumberOfRecords-1].Description:='No Description';
      memrec[NumberOfRecords-1].Address:=ad;
      memrec[NumberOfRecords-1].VarType:=2;

      memrec[NumberOfRecords-1].Frozen:=false;
      memrec[NumberOfRecords-1].FrozenValue:=0;

      memrec[NumberOfRecords-1].Bit:=0;
      hotkeys[NumberOfRecords-1]:=-1;

      updatescreen;
      updatelist;
    end
    else raise Exception.Create(IntToHex(ad,8)+' is already in the list!');


  end;
end;

end.
