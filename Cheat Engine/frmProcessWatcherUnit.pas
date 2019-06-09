unit frmProcessWatcherUnit;

{$MODE Delphi}

interface

uses
  jwawindows, windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs,NewKernelHandler, ExtCtrls, ComCtrls, StdCtrls,CEDebugger,kerneldebugger,
  CEFuncProc, Menus, LResources,{tlhelp32,} symbolhandler, debugHelper, syncobjs;

type tthreaddata=record
  threadid: dword;
end;

type tprocessdata=record
  processid: DWORD;
  peprocess: uint64;
  threadlist: array of tthreaddata;
end;

type tprocesswatchthread=class(tthread)
  private
    error: string;
    created:boolean;
    pid: dword;
    peprocess: uint64;
    tid:dword;
    procedure crash;
    procedure UpdateList;
    procedure Updatelist2;
    procedure UpdateThreadcount(processid:dword;count:dword);
  public
    openedByAutoAttach: boolean;
    procedure execute; override;
  end;

type

  { TfrmProcessWatcher }

  TfrmProcessWatcher = class(TForm)
    btnAttach: TButton;
    btnOpen: TButton;
    pwImageList: TImageList;
    Panel2: TPanel;
    tvProcesslist: TTreeView;
    Panel1: TPanel;
    pmthreadid: TPopupMenu;
    ShowThreadIDs1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure tvProcesslistDblClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnAttachClick(Sender: TObject);
    procedure ShowThreadIDs1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
  private
    { Private declarations }
    processwatchthread: tprocesswatchthread;
    procedure PWOP(ProcessIDString:string);
  public
    { Public declarations }
    processes: array of tprocessdata;
    processesCS: TCriticalSection;
  end;

var
  frmProcessWatcher: TfrmProcessWatcher;

implementation

uses formsettingsunit, MainUnit,AdvancedOptionsUnit, MemoryBrowserFormUnit,
     frmProcesswatcherExtraUnit,plugin, processhandlerunit, Globals;

resourcestring
  rsProcesswatcherThreadError = 'processwatcher: thread error:%s';
  rsFailedStartingTheProcessWatcher = 'Failed starting the process watcher';
  rsIsnTAValidProcessID = '%s isn''t a valid processID';
  rsFirstSelectAProcess = 'First select a process!';
  rsProcessID = 'ProcessID=';
  rsPEPROCESS = 'PEPROCESS=';
  rsThreadID = 'ThreadID:';
  rsConventionalIDs = '----Conventional ID''s----';

procedure tprocesswatchthread.crash;
begin
  showmessage(Format(rsProcesswatcherThreadError, [error]));

end;

procedure TProcesswatchthread.UpdateThreadcount(processid:dword;count:dword);
var i,j: integer;
begin
  //find the processd in the treeview and update the threadcount
  frmProcessWatcher.processesCS.enter;
  try

    with frmProcessWatcher do
    for i:=0 to tvprocesslist.Items.Count-1 do
      if (tvprocesslist.Items[i].Level=0) and (copy(tvprocesslist.Items[i].Text,1,8)=IntToHex(processid,8)) then
      begin
        for j:=length(tvprocesslist.Items[i].Text) downto 1 do
          if tvprocesslist.Items[i].Text[j]='(' then //found the start
          begin
            //change the text
            tvprocesslist.Items[i].Text:=copy(tvprocesslist.Items[i].Text,1,j-1)+'('+inttostr(count)+')';
            break;
          end;

        break;
      end;
  finally
    frmProcessWatcher.processesCS.leave;
  end;
end;

procedure Tprocesswatchthread.Updatelist2;
{
  used vars:
  Created:BOOL;
  ProcessID:DWORD;
  ThreadID:dword;
  PEThread:DWORD;
}
var i,j,k:integer;
found:boolean;
begin
  if frmProcessWatcher=nil then exit;
  
  //add/remove the thread from the processlist

  //and show the threadcount to the list

  //find the process
  frmProcessWatcher.processesCS.Enter;
  try
    for i:=0 to length(frmprocesswatcher.processes)-1 do
      if frmprocesswatcher.processes[i].processid=self.pid then
      begin
        if not self.created then
        begin

          //find the thread and delete it
          found:=false;
          for j:=0 to length(frmprocesswatcher.processes[i].threadlist)-1 do
            if frmprocesswatcher.processes[i].threadlist[j].threadid=self.tid then
            begin
              //memo1.Lines.Add(inttostr(self.processid)+' - removed thread '+inttostr(self.threadid));
              //remove it from the list
              for k:=j to length(frmprocesswatcher.processes[i].threadlist)-2 do
                frmprocesswatcher.processes[i].threadlist[k]:=frmprocesswatcher.processes[i].threadlist[k+1];

              setlength(frmprocesswatcher.processes[i].threadlist,length(frmprocesswatcher.processes[i].threadlist)-1);
              UpdateThreadcount(self.pid,length(frmprocesswatcher.processes[i].threadlist));
              found:=true;
              break;
            end;

          break;
        end
        else
        begin
          //memo1.Lines.Add(inttostr(self.processid)+' - added thread '+inttostr(self.threadid));
          //add the thread to the list
          setlength(frmprocesswatcher.processes[i].threadlist,length(frmprocesswatcher.processes[i].threadlist)+1);

          frmprocesswatcher.processes[i].threadlist[length(frmprocesswatcher.processes[i].threadlist)-1].threadid:=self.tid;
          UpdateThreadcount(self.pid,length(frmprocesswatcher.processes[i].threadlist));
        end;
        break;
      end;
   finally
     frmProcessWatcher.processesCS.Leave;
   end;
end;

procedure tprocesswatchthread.UpdateList;
var i,j:integer;
    deleted:boolean;
    processname: pchar;
    tn: ttreenode; //treenode of new process
    autoAttachThisProcess: boolean;
begin
  autoAttachThisProcess:=false;

  //first fix the processes array
  frmProcessWatcher.processesCS.Enter;
  try
    with frmProcessWatcher do
    begin
      if not created then
      begin
        //find it and delete it
        for i:=0 to length(processes)-1 do
          if processes[i].peprocess=peprocess then
          begin
            //found it
            setlength(processes[i].threadlist,0);
            for j:=i to length(processes)-2 do
              processes[j]:=processes[j+1];

            setlength(processes,length(processes)-1);
            break;
          end;

        //find in the treeview and delete it
        deleted:=false;
        for i:=0 to tvprocesslist.Items.Count-1 do
          if (tvprocesslist.Items[i].Level=0) and (copy(tvprocesslist.Items[i].Text,1,8)=IntToHex(pid,8)) then
          begin
            tvprocesslist.Items[i].DeleteChildren;
            tvprocesslist.Items[i].Delete;
            deleted:=true;
            break;
          end;

      end else
      begin
        //add it
        setlength(processes,length(processes)+1);
        processes[length(processes)-1].processid:=self.pid;
        processes[length(processes)-1].peprocess:=self.peprocess;
        setlength(processes[length(processes)-1].threadlist,0); //init on 0

        getmem(processname,16);
        try
          if GetProcessNameFromPEProcess(peprocess,processname,16)=-1 then
            tvprocesslist.items.Add(nil,IntToHex(self.pid,8)+' ('+IntToHex(self.peprocess,8)+') - ??? (0)')
          else
          begin
            tn:=tvprocesslist.items.Add(nil,IntToHex(self.pid,8)+' ('+IntToHex(self.peprocess,8)+') - '+processname+' (0)');

            if (processid=0) and (processhandle=0) and (mainform.autoattachlist.Count>0) then
            begin
              //check if we should automatically open this one
              if mainform.autoattachlist.IndexOf(processname)<>-1 then //it is in the list
                autoAttachThisProcess:=true;

            end;
          end;
        finally
          freememandnil(processname);

        end;
      end;

    end;

  finally
    frmProcessWatcher.processesCS.Leave;
  end;

  if autoattachthisProcess then
  begin
    //open it
    openedByAutoAttach:=true;
    frmProcessWatcher.tvProcesslist.Selected:=tn;

    frmProcessWatcher.btnOpen.Click; //won't take too long so an be done in this thread
  end else openedByAutoAttach:=false;
end;

procedure tprocesswatchthread.execute;
type tprocesseventstruct=record
  Created:UINT64;
  ProcessID:UINT64;
  PEProcess:UINT64;
end;
type pprocesseventstruct=^tprocesseventstruct;

type tthreadeventstruct=record
  Created:UINT64;
  ProcessID:UINT64;
  ThreadID:UINT64;
end;
type pthreadeventstruct=^tthreadeventstruct;

var processevents: pointer;
    threadevents: pointer;



    y: pprocesseventstruct;
    z: pthreadeventstruct;
    i: integer;
    count: byte;
    res: dword;

begin
  getmem(processevents,50*sizeof(tprocesseventstruct)+1);
  getmem(threadevents,50*sizeof(tthreadeventstruct)+1);

  while not terminated do
  begin
    res:=WaitForProcessListData(processevents,threadevents,3000);

    //processevent
    count:=pbyte(processevents)^;
    y:=pprocesseventstruct(ptrUint(processevents)+1);

    for i:=0 to count-1 do
    begin
      self.created:=y^.Created>0;
      self.pid:=y^.ProcessID;
      self.peprocess:=y^.PEProcess;
      synchronize(updatelist);

      pluginhandler.handlenewprocessplugins(y^.ProcessID,y^.PEProcess, created);

      inc(y); //next element
    end;


    //threadevent
    count:=pbyte(threadevents)^;
    z:=pthreadeventstruct(ptrUint(threadevents)+1);


    for i:=0 to count-1 do
    begin
      self.created:=z^.Created>0;
      self.pid:=z^.ProcessID;
      self.tid:=z^.threadid;

      if not terminated then
        synchronize(updatelist2);
        
      inc(z); //next element
    end;

  end;

  freememandnil(processevents);
  freememandnil(threadevents);
end;

//-------------------------------------------------

procedure TfrmProcessWatcher.FormCreate(Sender: TObject);
begin
  if @startprocesswatch=nil then loaddbk32;

  if StartProcessWatch then
  begin
    //start the thread that gets the data
    processesCS:=TCriticalSection.create;
    processwatchthread:=tprocesswatchthread.Create(true);
    processwatchthread.start;


  end else raise exception.Create(rsFailedStartingTheProcessWatcher);
end;

procedure TFrmProcessWatcher.PWOP(ProcessIDString:string);
var i:integer;
begin
  val('$'+ProcessIDString,ProcessHandler.processid,i);
  if i<>0 then raise exception.Create(Format(rsIsnTAValidProcessID, [processidstring]));
  if Processhandle<>0 then
  begin
    CloseHandle(ProcessHandle);
    ProcessHandler.ProcessHandle:=0;
  end;

  with mainform do
  begin
    if GetSystemType>=4 then
    begin
      cbSpeedhack.checked:=false;
      cbSpeedhack.Enabled:=true;
      cbspeedhack.Checked:=false;
      cbspeedhack.Enabled:=true;
    end;
  end;

  Open_Process;
end;

procedure TfrmProcessWatcher.tvProcesslistDblClick(Sender: TObject);
begin
  if (tvprocesslist.Selected<>nil) and (tvprocesslist.Selected.Level=0) then
    btnopen.Click;
end;

procedure TfrmProcessWatcher.btnOpenClick(Sender: TObject);
var ProcessIDString: String;
    i:               Integer;
    processnode:      ttreenode;
begin
  if (tvprocesslist.Selected<>nil) then
  begin
    unpause;
    DetachIfPossible;

    ProcessIDString:='';
    i:=1;

    if tvprocesslist.Selected.Level=0 then
      processnode:=tvprocesslist.Selected
    else
      processnode:=tvprocesslist.Selected.Parent;

    while processnode.text[i]<>' ' do
    begin
      ProcessIDString:=ProcessIDString+processnode.text[i];
      inc(i);
    end;

    PWOP(ProcessIDString);
    MainForm.ProcessLabel.caption:=processnode.Text;

    mainform.enableGui(false);

    sleep(100); //wait a bit for the process to get fully created if it is a auto attach

    symhandler.reinitialize;
    mainform.reinterpretaddresses;

    
    if self.visible then
      self.hide;
  end;
end;

procedure TfrmProcessWatcher.btnAttachClick(Sender: TObject);
var ProcessIDString: String;
    i:               Integer;
begin
  if tvprocesslist.Selected<>nil then
  begin
    unpause;
    DetachIfPossible;

    ProcessIDString:='';
    i:=1;
    while tvprocesslist.Selected.Text[i]<>' ' do
    begin
      ProcessIDString:=ProcessIDString+tvprocesslist.Selected.Text[i];
      inc(i);
    end;

    val('$'+ProcessIDString,ProcessHandler.processid,i);

    if Processhandle<>0 then
    begin
      CloseHandle(ProcessHandle);
      ProcessHandler.ProcessHandle:=0;
    end;

    Debuggerthread:=TDebuggerThread.MyCreate2(processid);

    mainform.ProcessLabel.Caption:=tvprocesslist.Selected.Text[i];

    ProcessSelected:=true;
    mainform.debugproc:=true;

    mainform.enableGui(false);
    close;
  end else showmessage(rsFirstSelectAProcess);
end;

procedure TfrmProcessWatcher.ShowThreadIDs1Click(Sender: TObject);
var i,j: integer;
    ths: thandle;
    tE: threadentry32;
begin
  if tvProcesslist.Selected<>nil then
  begin
    i:=tvProcesslist.Selected.AbsoluteIndex;

    with tfrmprocesswatcherextra.create(self) do
    begin
      data.lines.add(rsProcessID+inttohex(processes[i].processid,8));
      data.lines.add(rsPEPROCESS+inttohex(processes[i].peprocess,8));

      for j:=0 to length(processes[i].threadlist)-1 do
        data.Lines.Add(rsThreadID+inttohex(processes[i].threadlist[j].threadid,8));

      data.Lines.add('');
      data.Lines.add(rsConventionalIDs);

      ths:=CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD,0);
      if ths<>0 then
      begin
        te.dwSize:=sizeof(te);
        if Thread32First(ths,te) then
        begin
          repeat
            if te.th32OwnerProcessID=processes[i].processid then
              data.lines.add(rsThreadID+IntToHex(te.th32ThreadID,8));

          until not thread32Next(ths,te);
        end;
      end;
      closehandle(ths);

      showmodal;
    end;

  end;
end;

procedure TfrmProcessWatcher.FormDestroy(Sender: TObject);
begin
  if processwatchthread<>nil then
  begin
    processwatchthread.Terminate;
    processwatchthread.WaitFor;
    processwatchthread.Free;
  end;
end;

procedure TfrmProcessWatcher.Panel1Resize(Sender: TObject);
begin
  btnOpen.Left:=(panel1.clientwidth div 2) - (btnopen.width div 2);
  btnAttach.Left:=(panel1.clientwidth div 2) - (btnAttach.width div 2);
  
end;

initialization
  {$i frmProcessWatcherUnit.lrs}

end.
