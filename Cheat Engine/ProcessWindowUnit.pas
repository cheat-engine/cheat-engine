unit ProcessWindowUnit;

{$MODE Delphi}

interface

uses
  jwawindows, windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CEFuncProc,CEDebugger, ComCtrls, ImgList,
  filehandler, Menus, LResources,{tlhelp32,}vmxfunctions, NewKernelHandler, debugHelper{, KIcon}, commonTypeDefs;

type tprocesslistlong = class(tthread)
private
  processcount: integer;
  process: array[0..9] of string;
  procedure drawprocesses;
public
  processlist: tlistbox;
  procedure execute; override;
end;

type

  { TProcessWindow }

  TProcessWindow = class(TForm)
    btnNetwork: TButton;
    btnProcesslist: TButton;
    btnProcessWatch: TButton;
    btnWindowList: TButton;
    CancelButton: TButton;
    miOwnProcessesOnly: TMenuItem;
    OKButton: TButton;
    Panel3: TPanel;
    Panel4: TPanel;
    ProcessList: TListBox;
    Panel1: TPanel;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    PopupMenu1: TPopupMenu;
    InputPIDmanually1: TMenuItem;
    Filter1: TMenuItem;
    Panel2: TPanel;
    btnOpenFile: TButton;
    btnCreateThread: TButton;
    Button4: TButton;
    btnProcessListLong: TButton;
    Showinvisiblewindows1: TMenuItem;
    procedure btnNetworkClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure miOwnProcessesOnlyClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure btnProcesslistClick(Sender: TObject);
    procedure btnWindowListClick(Sender: TObject);
    procedure btnCreateThreadClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure InputPIDmanually1Click(Sender: TObject);
    procedure Filter1Click(Sender: TObject);
    procedure btnProcessWatchClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnProcessListLongClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure ProcessListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormShow(Sender: TObject);
    procedure ProcessListKeyPress(Sender: TObject; var Key: char);
    procedure Showinvisiblewindows1Click(Sender: TObject);
  private
    { Private declarations }
    currentchar: integer;

    ffilter: string;
    currentlist: integer;
    processlistlong: tprocesslistlong;
    procedure setbuttons;
    procedure SetFilter(filter:string);
    property filter:string read ffilter write setfilter;
    procedure filterlist;

  public
    { Public declarations }
    procedure PWOP(ProcessIDString:string);
  end;

var
  ProcessWindow: TProcessWindow;

implementation


uses MainUnit, formsettingsunit, advancedoptionsunit,frmProcessWatcherUnit,
  memorybrowserformunit, networkConfig, ProcessHandlerUnit, processlist, globals;

resourcestring
  rsIsnTAValidProcessID = '%s isn''t a valid processID';
  rsPhysicalMemory = 'Physical Memory';
  rsYouCanOnlyLoadEXEFiles = 'You can only load EXE files';
  rsCreateProcess = 'Create Process';
  rsOptionalLaunchParameters = 'Optional launch parameters';
  rsAttachdebuggerornot = 'Are you sure you want to attach the debugger and not just open this process? (You can later on always attach the debugger)';
  rsPleaseSelectAnotherProcess = 'Please select another process';
  rsFirstSelectAProcess = 'First select a process!';
  rsManualPID = 'Manual PID';
  rsEnterTheProcessID = 'Enter the ProcessID';
  rsFilter = 'Filter';
  rsWhatAreYouLookingFor = 'What are you looking for?';
  rsScanningClickToStop = 'Scanning (Click to stop)';
  rsProcessListLong = 'Process List(long)';
  rsProcessList = 'Process List';

procedure TProcessListLong.drawprocesses;
var i: integer;
begin
  if not terminated then
  begin
    processlist.Items.BeginUpdate;
    for i:=0 to processcount-1 do
      processlist.Items.Add(process[i]);
    processlist.Items.EndUpdate;
    processcount:=0;
  end;
end;

procedure TProcessListLong.execute;
var i: dword;
    h: thandle;

    x: pchar;
    modulename:string;
begin
  i:=0;






  while not terminated and (i<$FFFFFFFF) do
  begin
    h:=windows.OpenProcess(PROCESS_ALL_ACCESS,false,i);
    if h<>0 then
    begin
      modulename:=getProcessnameFromProcessID(i);
      process[processcount]:=inttohex(i,8)+'-'+modulename;

      inc(processcount);
      if processcount>=10 then
        synchronize(drawprocesses);

      closehandle(h);
    end;

    if ((i mod 4096)=0) then
      if processcount>0 then synchronize(drawprocesses);

    inc(i);
  end;

  if processcount>0 then synchronize(drawprocesses);
end;

procedure TProcessWindow.filterlist;
var i:integer;
begin
  if filter='' then exit;

  i:=0;
  while i<processlist.Items.Count do
  begin
    if pos(uppercase(filter),uppercase(processlist.Items[i]))=0 then
      processlist.Items.Delete(i)
    else
      inc(i);
  end;
end;

procedure TProcesswindow.SetFilter(filter:string);
begin
  ffilter:=filter;
  case currentlist of
    0: btnProcesslist.Click;
    1: btnWindowList.click;
  end;

  filterlist;
end;

procedure TProcessWindow.CancelButtonClick(Sender: TObject);
begin
  mainform.canceled:=true;

  //ProcessWindow.close;
  ModalResult:=mrCancel;
end;

procedure TProcessWindow.miOwnProcessesOnlyClick(Sender: TObject);
begin
  ProcessesCurrentUserOnly:=miOwnProcessesOnly.checked;

  if currentlist=1 then
    btnWindowList.click
  else
    btnProcesslist.click;
end;

procedure TProcessWindow.btnNetworkClick(Sender: TObject);
begin
  if frmNetworkConfig=nil then
    frmNetworkConfig:=tfrmNetworkConfig.create(self);

  if frmNetworkConfig.ShowModal=mrok then
    btnProcesslist.Click;

end;

procedure TProcessWindow.setbuttons;
begin
  if formsettings.cbProcesswatcher.Checked then
  begin
    btnProcessWatch.Visible:=true;
    btnProcesslist.Left:=7;
    btnWindowList.Left:=83;
    btnProcessWatch.Left:=159;
  end else
  begin
    btnProcessWatch.Visible:=false;
    btnProcesslist.Left:=44;
    btnWindowList.Left:=120;
  end;


end;

procedure TProcessWindow.PWOP(ProcessIDString:string);
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
      cbunrandomizer.Checked:=false;
      cbunrandomizer.Enabled:=true;
    end;
  end;

  Open_Process;

  ProcessSelected:=true;


  if (processid=0) and ((formsettings.cbKernelReadWriteProcessMemory.checked) or (dbvm_version>=$ce000004)) then
  begin
    ProcessHandler.processid:=$FFFFFFFF;

    if dbvm_version>=$ce000004 then
      DBKPhysicalMemoryDBVM
    else
      DBKPhysicalMemory;
    ProcessHandler.ProcessHandle:=$FFFFFFFF;
  end
  else
  begin
    if usephysical or usephysicaldbvm then
      DBKProcessMemory;
  end;

end;

procedure TProcessWindow.OKButtonClick(Sender: TObject);
var ProcessIDString: String; 
begin
  //Outputdebugstring('OK button click');
  if Processlist.ItemIndex>-1 then
  begin
    unpause;
    DetachIfPossible;

    ProcessIDString:=copy(ProcessList.Items[Processlist.ItemIndex], 1, pos('-',ProcessList.Items[Processlist.ItemIndex])-1);

    PWOP(ProcessIDString);
    MainForm.ProcessLabel.caption:=ProcessList.Items[Processlist.ItemIndex];
    Modalresult:=MROK;
    //ProcessWindow.close;
  end;

  //outputdebugstring('After ok click handler');
end;



//button1click specific:
procedure TProcessWindow.btnProcesslistClick(Sender: TObject);
var oldselection: string;
    oldselectionIndex: integer;
    i: integer;
    found: boolean;
begin

  Showinvisiblewindows1.visible:=false;
  oldselectionindex:=processlist.ItemIndex;

  if oldselectionindex<>-1 then
    oldselection:=processlist.Items[oldselectionIndex];


  currentlist:=0;

  getprocesslist(processlist);


  if formsettings.cbKernelReadWriteProcessMemory.checked or (dbvm_version>=$ce000004) then //driver is active
    processlist.Items.Insert(0, '00000000-['+rsPhysicalMemory+']');

  Filterlist;

  if oldselectionindex=-1 then
    processlist.ItemIndex:=processlist.Items.Count-1 //go to the end
  else
  begin
    i:=processlist.Items.IndexOf(oldselection);
    if i>=0 then
      processlist.ItemIndex:=i
    else
    begin
      //strip out the processid part and search for a entry with the appropriate processname (e.g restarted game)
      oldselection:=copy(oldselection,pos('-',oldselection)+1,length(oldselection));

      found:=false;
      for i:=0 to processlist.Items.Count-1 do
        if pos(oldselection, processlist.items[i])>0 then
        begin
          processlist.ItemIndex:=i;
          found:=true;

          break;
        end;

      if not found then
        processlist.ItemIndex:=processlist.Items.Count-1;
    end;
  end;

end;

procedure TProcessWindow.btnWindowListClick(Sender: TObject);
begin
  currentlist:=1;
  Showinvisiblewindows1.visible:=true;
  getwindowlist(processlist,Showinvisiblewindows1.Checked);
  filterlist;

  processlist.ItemIndex:=processlist.Items.Count-1;  
end;

procedure TProcessWindow.btnCreateThreadClick(Sender: TObject);
var parameters: string;
begin
  if Opendialog1.Execute then
  begin
    if Uppercase(extractfileext(opendialog1.FileName))<>'.EXE' then raise Exception.Create(rsYouCanOnlyLoadEXEFiles);
    parameters:='';
    if not InputQuery(rsCreateProcess, rsOptionalLaunchParameters, parameters) then exit;


    unpause;
    detachIfPossible;


    Debuggerthread:=TDebuggerThread.MyCreate2(opendialog1.FileName, parameters);
    if not Debuggerthread.running then exit;

    mainForm.ProcessLabel.caption:=IntToHex(processid,8)+'-'+ExtractFileName(opendialog1.FileName);


    mainform.debugproc:=true;


    memorybrowser.show;
    modalresult:=mrOk;
  end;
end;

procedure TProcessWindow.Button4Click(Sender: TObject);
var ProcessIDString: String;
    i:               Integer;
begin

  if Processlist.ItemIndex>-1 then
  begin
    if MessageDlg(rsAttachdebuggerornot, mtConfirmation, [mbyes, mbno], 0)=mryes then
    begin
      unpause;
      DetachIfPossible;

      ProcessIDString:='';
      i:=1;
      while ProcessList.Items[Processlist.ItemIndex][i]<>'-' do
      begin
        ProcessIDString:=ProcessIDString+ProcessList.Items[Processlist.ItemIndex][i];
        inc(i);
      end;

      val('$'+ProcessIDString,ProcessHandler.processid,i);

      if Processhandle<>0 then
      begin
        CloseHandle(ProcessHandle);
        ProcessHandler.ProcessHandle:=0;
      end;

      if processid=GetCurrentProcessId then raise exception.create(rsPleaseSelectAnotherProcess);

      Debuggerthread:=TDebuggerThread.MyCreate2(processid);

      mainform.ProcessLabel.Caption:=ProcessList.Items[Processlist.ItemIndex];

      ProcessSelected:=true;
      mainform.debugproc:=true;

      modalresult:=mrOK;
    end
  end else showmessage(rsFirstSelectAProcess);

end;

procedure TProcessWindow.btnOpenFileClick(Sender: TObject);
begin

  if opendialog2.execute then
  begin
    DBKFileAsMemory(opendialog2.filename);
    processselected:=true;
    ProcessHandler.ProcessHandle:=filehandle;
    MainForm.ProcessLabel.caption:=extractfilename(opendialog2.FileName);
    ProcessHandler.processid:=$FFFFFFFF;

    modalresult:=mrok;
  end;

end;

procedure TProcessWindow.InputPIDmanually1Click(Sender: TObject);
var pid: string;
begin
  pid:='0';
  if InputQuery(rsManualPID, rsEnterTheProcessID+':', pid) then
  begin
    unpause;
    DetachIfPossible;

    pwop(pid);
    MainForm.ProcessLabel.caption:=pid;
    modalresult:=mrok;
  end;

end;

procedure TProcessWindow.Filter1Click(Sender: TObject);
var fltr: string;
begin
  if inputquery(rsFilter, rsWhatAreYouLookingFor, fltr) then
    filter:=fltr;
end;

procedure TProcessWindow.btnProcessWatchClick(Sender: TObject);
begin

  if frmprocesswatcher=nil then
    frmprocesswatcher:=tfrmprocesswatcher.Create(mainform);
  frmprocesswatcher.show;
  modalresult:=mrcancel;
end;

procedure TProcessWindow.FormResize(Sender: TObject);
begin
//reset the button positions
  setbuttons;
end;

procedure TProcessWindow.btnProcessListLongClick(Sender: TObject);
begin
  if processlistlong=nil then
  begin
    processlist.Clear;
    btnprocesslistlong.Caption:=rsScanningClickToStop;
    processlistlong:=tprocesslistlong.create(true);
    processlistlong.processlist:=processlist;
    processlistlong.start;
  end
  else
  begin
    btnprocesslistlong.Caption:=rsProcessListLong;
    processlistlong.terminate;
    processlistlong.WaitFor;
    processlistlong.Free;
    processlistlong:=nil;    
  end;
end;

procedure TProcessWindow.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if processlistlong<>nil then
  begin
    processlistlong.terminate;
    processlistlong.WaitFor;
    processlistlong.Free;
    processlistlong:=nil;
    btnprocesslistlong.Caption:=rsProcessListLong;
  end;
end;

procedure TProcessWindow.PopupMenu1Popup(Sender: TObject);
begin
  miOwnProcessesOnly.checked:=ProcessesCurrentUserOnly;
end;

procedure TProcessWindow.ProcessListDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin

  processlist.Canvas.FillRect(rect);


  processlist.Canvas.TextOut(rect.Left+rect.Bottom-rect.Top,rect.Top,processlist.Items[index]);

  if (processlist.Items.Objects[index]<>nil) and (PProcessListInfo(processlist.Items.Objects[index])^.processIcon>0) then
    DrawIconEx(processlist.Canvas.Handle, rect.left, rect.Top, PProcessListInfo(processlist.Items.Objects[index])^.processIcon, rect.Bottom-rect.Top,rect.Bottom-rect.Top,0,0,DI_NORMAL);

end;

procedure TProcessWindow.FormShow(Sender: TObject);
var i: integer;
begin
  ProcessList.ItemHeight:=canvas.TextHeight('QqJjWwSs')+2;

  currentchar:=1;
  btnProcesslist.click;

  setbuttons;

  i:=max(btnProcesslist.width, btnWindowList.width);

  if btnProcessWatch.Visible then
  begin
    i:=max(i, btnProcessWatch.Width);
    btnProcesslist.width:=i;
    btnWindowList.width:=i;
    btnProcessWatch.width:=i;
  end
  else
  begin
    btnProcesslist.width:=i;
    btnWindowList.width:=i;
    btnProcesslist.BorderSpacing.Left:=40;
    btnWindowList.BorderSpacing.Right:=40;
  end;

  autosize:=false;

  panel3.autosize:=false;
  i:=max(panel3.width, button4.width);
  panel3.width:=i;
  cancelbutton.AnchorSideLeft.Control:=nil;
  cancelbutton.AnchorSideRight.Control:=panel3;
  cancelbutton.AnchorSideRight.side:=asrRight;
  cancelbutton.Anchors:=[akTop, akRight];

  if btnProcessWatch.Visible then
    i:=max(i, btnProcesslist.width+btnWindowList.width+btnProcessWatch.Width+5)
  else
    i:=max(i, btnProcesslist.width+btnWindowList.width+3);


  clientwidth:=i+40;
  clientheight:=trunc(panel2.height*1.8);
end;

procedure TProcessWindow.ProcessListKeyPress(Sender: TObject; var Key: char);
begin
  if key=#8 then
    filter:=copy(filter, 1, length(filter)-1)
  else
  if key in [chr(32)..chr(128)] then
    filter:=filter+key;

  if filter<>'' then
    caption:=rsProcessList+' : *'+filter+'*'
  else
    caption:=rsProcessList;
end;

procedure TProcessWindow.Showinvisiblewindows1Click(Sender: TObject);
begin
  if Showinvisiblewindows1.Visible then
  begin
    Showinvisiblewindows1.Checked:=not Showinvisiblewindows1.Checked;
    btnWindowList.Click;
  end;
end;

initialization
  {$i ProcessWindowUnit.lrs}

end.

