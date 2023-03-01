unit ProcessWindowUnit;

{$MODE Delphi}

interface

uses
  {$ifdef darwin}
  macport,
  {$endif}
  {$ifdef windows}
  jwawindows, windows,
  {$endif}
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, CEFuncProc,CEDebugger, ComCtrls, ImgList,
  Filehandler, Menus, LResources,{tlhelp32,}{$ifdef windows}vmxfunctions,{$endif} NewKernelHandler,
  debugHelper{, KIcon}, commonTypeDefs, math,lcltype, syncobjs, Contnrs, betterControls;

type
  TProcesslistlong = class(tthread)
  private
    processcount: integer;
    process: array[0..9] of string;
    procedure drawprocesses;
  public
    processlist: tlistbox;
    procedure execute; override;
  end;

 {$ifdef windows}
  TIconFetchEntry=record
    processid: dword;
    winhandle: hwnd; //optional
    index: integer;
    icon: HIcon; //gets filled in
  end;
  PIconFetchEntry=^TIconFetchEntry;

  TIconFetchThread = class(TThread)
  private
    hasData: TEvent;
    requestsList: TList; //just the PID
    requestsListCS: TCriticalSection;

    resolvedList: TList; //PID and HICON record
    resolvedListCS: TCriticalSection;
    procedure getIcon(e: PIconFetchEntry);
  public
    function queueIconFetch(processid: dword; winhandle: hwnd; index: integer): hicon; overload;
    function queueIconFetch(processid: dword; index: integer): hicon; overload;
    procedure reset;
    procedure execute; override;
    constructor create;
    destructor destroy; override;
  end;
  {$endif}

type

  { TProcessWindow }

  TProcessWindow = class(TForm)
    btnNetwork: TButton;
    btnAttachDebugger: TButton;
    CancelButton: TButton;
    FontDialog1: TFontDialog;
    TabHeader: TPageControl;
    plImageList: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    miConvertPIDToDecimal: TMenuItem;
    miRefresh: TMenuItem;
    miCreateProcess: TMenuItem;
    miOpenFile: TMenuItem;
    N2: TMenuItem;
    miChangeFont: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    N1: TMenuItem;
    miProcessListLong: TMenuItem;
    miOwnProcessesOnly: TMenuItem;
    OKButton: TButton;
    Panel1: TPanel;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    Panel3: TPanel;
    Panel5: TPanel;
    PopupMenu1: TPopupMenu;
    InputPIDmanually1: TMenuItem;
    Filter1: TMenuItem;
    ProcessList: TListBox;
    miShowInvisibleItems: TMenuItem;
    tsApplications: TTabSheet;
    tsProcesses: TTabSheet;
    tsWindows: TTabSheet;
    Timer1: TTimer;
    procedure btnNetworkClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure miProcessListLongClick(Sender: TObject);
    procedure miChangeFontClick(Sender: TObject);
    procedure miOwnProcessesOnlyClick(Sender: TObject);
    procedure miRefreshClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure btnProcesslistClick(Sender: TObject);
    procedure btnWindowListClick(Sender: TObject);
    procedure btnCreateThreadClick(Sender: TObject);
    procedure btnAttachDebuggerClick(Sender: TObject);
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
    procedure miShowInvisibleItemsClick(Sender: TObject);
    procedure TabHeaderChange(Sender: TObject);
    procedure TabHeaderResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    currentchar: integer;
    wantedheight: integer;

    ffilter: string;

    {$ifdef windows}
    IconFetchThread: TIconFetchThread;
    {$endif}
    processlistlong: tprocesslistlong;
    procedure refreshlist;
    procedure setbuttons;
    procedure SetFilter(filter:string);
    property filter:string read ffilter write setfilter;
    procedure filterlist;

{$ifdef windows}
    procedure iconFetchedEvent(sender: TObject; processid: dword; index: integer; icon: hicon);
{$endif}
  public
    { Public declarations }
    procedure PWOP(ProcessIDString:string);
  published
    property TabControl1: TPageControl read TabHeader;
  end;

var
  ProcessWindow: TProcessWindow;
  commonProcessesList: tstringlist;

implementation


uses MainUnit, formsettingsunit, advancedoptionsunit,frmProcessWatcherUnit,
  memorybrowserformunit, networkConfig, ProcessHandlerUnit, processlist, globals,
  registry, fontSaveLoadRegistry, frmOpenFileAsProcessDialogUnit, networkInterfaceApi, MainUnit2;

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

  rsApplications='Applications';
  rsProcesses='Processes';
  rsWindows='Windows';


var errortrace: integer;



{$IFDEF windows}
function SendMessageTimeout(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM; fuFlags, uTimeout: UINT; var lpdwResult: ptruint): LRESULT; stdcall; external 'user32' name 'SendMessageTimeoutA';


procedure TIconFetchThread.getIcon(e: PIconFetchEntry);
var
  s: string;
  HI: HICON;
  tempptruint: ptruint;
begin
  HI:=0;
  if e^.winhandle<>0 then
  begin
    if SendMessageTimeout(e^.winhandle,WM_GETICON,ICON_SMALL,0,SMTO_ABORTIFHUNG, 200, tempptruint )<>0 then
    begin
      HI:=tempptruint;
      if HI=0 then
      begin
        if SendMessageTimeout(e^.winhandle,WM_GETICON,ICON_SMALL2,0,SMTO_ABORTIFHUNG, 100, tempptruint	)<>0 then
          HI:=tempptruint;

        if HI=0 then
          if SendMessageTimeout(e^.winhandle,WM_GETICON,ICON_BIG,0,SMTO_ABORTIFHUNG, 50, tempptruint	)<>0 then
            HI:=tempptruint;
      end;
    end;
  end;

  if HI=0 then
  begin
    s:=GetFirstModuleName(e^.processid);
    HI:=ExtractIcon(hinstance,pchar(s),0);
  end;

  if HI<>0 then
    e^.icon:=HI
  else
    e^.icon:=HWND(-1);

  resolvedListCS.Enter;
  resolvedList.Add(e);
  resolvedListCS.Leave;
end;

procedure TIconFetchThread.execute;
var
  wr: TWaitResult;
  listnotempty: boolean;

  e: PIconFetchEntry;
  pid: dword;
begin
  NameThreadForDebugging('TIconFetchThread', ThreadID);
  while not terminated do
  begin
    wr:=hasdata.WaitFor(1000);
    if terminated then exit;

    if wr=wrSignaled then
    begin
      listnotempty:=true;
      while listnotempty do
      begin
        //fetch an item from the list
        requestsListCS.enter;

        e:=requestsList.last;
        if e<>nil then
          requestsList.Delete(requestsList.Count-1);

        listnotempty:=requestsList.Count>0;
        requestsListCS.leave;

        //get the icon for this PID and then call the IconFetchedEvent
        if e<>nil then
          getIcon(e);
      end;
    end
    else
      if wr<>wrTimeout then break;
  end;
end;

function TIconFetchThread.QueueIconFetch(processid: dword; winhandle: hwnd; index: integer): HIcon;
{
Queues an processid and window for processing
Changes the priority on request
Returns the icon if it has already been processed
}
var
  found: boolean;
  i: integer;
  e: PIconFetchEntry;
begin
  //first check if already in the list
  result:=0;
  found:=false;

  requestsListCS.enter;
  for i:=0 to requestsList.count-1 do
  begin
    e:=requestsList[i];
    if (e^.processid=processid) and (e^.index=index) and (e^.winhandle=winhandle) then
    begin
      found:=true;
      requestsList.Delete(i);
      requestsList.Add(e);
      break;
    end;
  end;
  requestsListCS.leave;

  if not found then
  begin
    //check if in the resolve queue, and if so, return it now
    resolvedListCS.enter;
    for i:=0 to resolvedList.count-1 do
    begin
      e:=resolvedList[i];
      if (e^.processid=processid) and (e^.index=index) and (e^.winhandle=winhandle) then
      begin
        resolvedlist.Delete(i);
        result:=e^.icon;
        found:=true;
        break;
      end;
    end;
    resolvedListCS.leave;
  end;

  if not found then
  begin
    getmem(e,sizeof(TIconFetchEntry));
    e^.processid:=processid;
    e^.winhandle:=winhandle;
    e^.index:=index;
    e^.icon:=0;

    requestsListCS.enter;
    requestsList.Add(e);
    requestsListCS.leave;

    hasData.SetEvent;
  end;
end;

function TIconFetchThread.QueueIconFetch(processid: dword; index: integer): HIcon;
begin
  result:=QueueIconFetch(processid, 0, index);
end;

procedure TIconFetchThread.reset;
var i: integer;
begin
  RemoveQueuedEvents(self);

  resolvedListCS.enter;
  for i:=0 to resolvedList.Count-1 do
    if resolvedList[i]<>nil then
      freemem(resolvedList[i]);

  resolvedList.Clear;
  resolvedListCS.leave;

  requestsListCS.enter;
  for i:=0 to requestsList.Count-1 do
    if requestsList[i]<>nil then
      freemem(requestsList[i]);

  requestsList.clear;
  requestsListCS.leave;
end;

constructor TIconFetchThread.create;
begin
  hasData:=TEvent.create(nil,false,false,'');
  requestsList:=Tlist.create;
  requestsListCS:=TCriticalSection.Create;

  resolvedList:=TList.create;
  resolvedListCS:=TCriticalSection.create;
  inherited create(false);
end;

destructor TIconFetchThread.Destroy;
begin
  terminate;
  hasdata.SetEvent;
  waitfor;

  reset;

  hasdata.free;
  requestsList.Free;
  requestsListCS.free;

  resolvedList.free;
  resolvedListCS.free;
  inherited destroy;
end;
{$ENDIF}

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
  {$ifdef windows}
  i:=0;






  while not terminated and (i<$FFFFFFFF) do
  begin
    h:=windows.OpenProcess(ifthen(GetSystemType<=6,$1f0fff, process_all_access),false,i);
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
  {$endif}
end;

procedure loadCommonProcessesList;
var
  s: string;
  i,j: integer;
begin
  s:=cheatenginedir+'commonProcessesList.txt';
  if FileExists(s) then //if the list exists
  begin
    if commonProcessesList=nil then commonProcessesList:=tstringlist.create;
    try
      commonProcessesList.LoadFromFile(s{$if FPC_FULLVERSION >= 030200}, true{$endif});
      for i:=commonProcessesList.Count-1 downto 0 do
      begin
        j:=pos('#', commonProcessesList[i]);
        if j>0 then commonProcessesList[i]:=copy(commonProcessesList[i], 1, j-1);
        commonProcessesList[i]:=uppercase(trim(commonProcessesList[i]));
        if commonProcessesList[i]='' then commonProcessesList.Delete(i);
      end;
    except
    end;
  end;

end;

function isInCommonProcessesList(processname: string): boolean;
var
  i:integer;
begin
  if commonProcessesList=nil then exit(false);
  for i:=0 to commonProcessesList.Count-1 do
    if commonProcessesList[i]=uppercase(copy(processname,10)) then exit(true);
  result:=false;
end;

procedure TProcessWindow.filterlist;
var
    i:integer;
{$IFDEF WINDOWS}
    pli: PProcessListInfo;
{$ENDIF}
    s: string;
begin
  if (filter='') and (commonProcessesList=nil) then exit;

  ffilter:=uppercase(ffilter);

  i:=0;
  while i<processlist.Items.Count do
  begin
    {$IFDEF WINDOWS}
    pli:=PProcessListInfo(processlist.items.Objects[i]);
    {$ENDIF}

    if ((ffilter<>'') and (pos(ffilter,uppercase(processlist.Items[i]))=0)) or isInCommonProcessesList(processlist.Items[i]) then
    begin
      {$IFDEF WINDOWS}
      if pli<>nil then
      begin
        if pli^.processIcon>0 then
        begin
          if pli^.processID<>GetCurrentProcessId then
            DestroyIcon(pli^.processIcon);

          pli^.processIcon:=0;
        end;

        freememandnil(pli);
      end;
      {$ENDIF}

      processlist.Items.Delete(i);
    end
    else
      inc(i);
  end;
end;

procedure TProcesswindow.SetFilter(filter:string);
begin
  ffilter:=filter;
  refreshlist;
end;

procedure TProcessWindow.CancelButtonClick(Sender: TObject);
begin
  mainform.canceled:=true;

  //ProcessWindow.close;
  ModalResult:=mrCancel;
end;

{$ifdef windows}
procedure TProcessWindow.iconFetchedEvent(sender: TObject; processid: dword; index: integer; icon: hicon);
var
  i: integer;
  pli: PProcessListInfo;
begin
  if (index>=0) and (index<processlist.items.count) then
  begin
    pli:=PProcessListInfo(processlist.Items.Objects[index]);
    if pli<>nil then
    begin
      if pli^.processID=processid then //making sure the list didn't change
      begin

        if pli^.processIcon=0 then
        begin
          pli^.processIcon:=icon;
        end
        else
        begin
          if (icon<>0) and (icon<>HWND(-1)) and (processid<>getcurrentprocessid) then
          begin
            DestroyIcon(icon); //not needed anymore (duplicates shouldn't happen...)
          end;
        end;
      end;

    end;


  end;
end;
{$endif}

procedure TProcessWindow.FormCreate(Sender: TObject);
var
  x: array of integer;
  reg: tregistry;
begin


  {$ifdef darwin}
  {ProcessList.AnchorSideTop:=ProcessWindow.AnchorSideTop;
  ProcessList.AnchorSideLeft:=TabHeader.AnchorSideLeft;
  ProcessList.AnchorSideRight:=TabHeader.AnchorSideRight;
  ProcessList.AnchorSideBottom:=TabHeader.AnchorSideBottom;
  ProcessList.Anchors:=TabHeader.Anchors;
  TabHeader.TabIndex:=1;
  TabHeader.Visible:=false; }
  tsWindows.TabVisible:=false;
  tsWindows.Visible:=false;
  {$endif}



  {$ifdef windows}
  IconFetchThread:=TIconFetchThread.create;
  {$endif}
  tsApplications.Caption:=rsApplications;
  tsProcesses.Caption:=rsProcesses;
  tsWindows.Caption:=rsWindows;

  setlength(x,0);
  if LoadFormPosition(self,x) then
  begin
    autosize:=false;
    if length(x)>0 then
      TabHeader.TabIndex:=x[0];

    if length(x)>1 then
      begin
        miOwnProcessesOnly.checked:=x[1]<>0;
        ProcessesCurrentUserOnly:=x[1]<>0;
      end;
  end
  else
    refreshlist;

  reg:=tregistry.create;
  try
    if reg.OpenKey('\Software\'+strCheatEngine+'\Process Window\Font'+darkmodestring,false) then
      LoadFontFromRegistry(processlist.Font, reg)
    else
      processlist.font.color:=colorset.FontColor;


  finally
    reg.free;
  end;

end;

procedure TProcessWindow.FormDestroy(Sender: TObject);
var x: array of integer;
begin
  setlength(x,3);
  x[0]:=TabHeader.TabIndex;
  x[1]:=ifthen(miOwnProcessesOnly.checked,1,0);
  SaveFormPosition(self,x);
end;

procedure TProcessWindow.MenuItem5Click(Sender: TObject);
begin


end;

procedure TProcessWindow.miProcessListLongClick(Sender: TObject);
begin
  btnProcessListLongClick(nil);
end;

procedure TProcessWindow.miChangeFontClick(Sender: TObject);
var reg: tregistry;
begin
  fontdialog1.font.assign(processlist.font);
  if fontdialog1.execute then
  begin
    //apply settings
    processlist.font.assign(FontDialog1.Font);
    //processlist.Canvas.Refresh;

    Timer1Timer(timer1);

    processlist.Repaint;

    reg:=tregistry.create;
    try
      if reg.OpenKey('\Software\'+strCheatEngine+'\Process Window\Font'+darkmodestring,true) then
        SaveFontToRegistry(FontDialog1.Font, reg);


    finally
      reg.free;
    end;
  end;
end;

procedure TProcessWindow.miOwnProcessesOnlyClick(Sender: TObject);
begin
  ProcessesCurrentUserOnly:=miOwnProcessesOnly.checked;
  refreshlist;
end;

procedure TProcessWindow.miRefreshClick(Sender: TObject);
begin
  refreshList;
end;

procedure TProcessWindow.btnNetworkClick(Sender: TObject);
begin
  if frmNetworkConfig=nil then
    frmNetworkConfig:=tfrmNetworkConfig.create(self);

  if frmNetworkConfig.ShowModal=mrok then
  begin
    tabheader.ShowTabs:=false;
    TabHeaderResize(nil);

    if TabHeader.TabIndex=1 then
      refreshlist
    else
    begin
      TabHeader.Tabindex:=1;
      refreshlist;
    end;

    processlist.SetFocus;
  end;
end;

procedure TProcessWindow.Button1Click(Sender: TObject);
begin

end;

procedure TProcessWindow.setbuttons;
begin

end;

procedure TProcessWindow.PWOP(ProcessIDString:string);
var i:integer;
begin

  val('$'+ProcessIDString,ProcessHandler.processid,i);
  if i<>0 then raise exception.Create(Format(rsIsnTAValidProcessID, [processidstring]));
  if Processhandle<>0 then
    ProcessHandler.ProcessHandle:=0;

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

  {$ifdef windows}
  if (processid<>0) and (UseFileAsMemory or Usephysical or usephysicaldbvm) then
  begin
    //swap back to processmemory
    UseFileAsMemory:=false;
    Usephysical:=false;
    usephysicaldbvm:=false;
    if formsettings.cbKernelOpenProcess.checked then
      UseDBKOpenProcess
    else
      DONTUseDBKOpenProcess;

    if formsettings.cbKernelQueryMemoryRegion.checked then
      UseDBKQueryMemoryRegion
    else
      DONTUseDBKQueryMemoryRegion;

    if formsettings.cbKernelReadWriteProcessMemory.checked then
      UseDBKReadWriteMemory
    else
      DONTUseDBKReadWriteMemory;
  end;
  {$endif}

  Open_Process;

  ProcessSelected:=true;

  {$ifdef windows}
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
  {$endif}

end;

procedure TProcessWindow.OKButtonClick(Sender: TObject);
var ProcessIDString: String; 
begin
  Outputdebugstring('OK button click');
  if Processlist.ItemIndex>-1 then
  begin
    unpause;
    DetachIfPossible;

    ProcessIDString:=copy(ProcessList.Items[Processlist.ItemIndex], 1, pos('-',ProcessList.Items[Processlist.ItemIndex])-1);

    Outputdebugstring('calling PWOD');
    PWOP(ProcessIDString);



    if TabHeader.TabIndex=0 then
      MainForm.ProcessLabel.caption:=ProcessIDString+'-'+extractfilename(getProcessPathFromProcessID(processid))
    else
      MainForm.ProcessLabel.caption:=ProcessList.Items[Processlist.ItemIndex];
    Modalresult:=MROK;
    //ProcessWindow.close;
  end;

  //outputdebugstring('After ok click handler');
end;



//button1click specific:
procedure TProcessWindow.btnProcesslistClick(Sender: TObject);
begin




end;

procedure TProcessWindow.btnWindowListClick(Sender: TObject);
begin
  //miSkipSystemProcesses.visible:=false;


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

procedure TProcessWindow.btnAttachDebuggerClick(Sender: TObject);
var ProcessIDString: String;
    i:               Integer;
    oldpid,newpid: dword;

    starttime: qword;
begin
  oldpid:=processid;

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

      val('$'+ProcessIDString,newpid,i);

      if (Processhandle<>0) and (oldpid<>newpid) then
      begin
        CloseHandle(ProcessHandle);
        ProcessHandler.ProcessHandle:=0;
      end;

      try
        if processid=GetCurrentProcessId then raise exception.create(rsPleaseSelectAnotherProcess);

        starttime:=GetTickCount64;
        Debuggerthread:=TDebuggerThread.MyCreate2(newpid);

      except
        on e: exception do
        begin
          debuggerthread:=nil;
          MessageDlg(e.message, mtError,[mbok],0);
          exit;
        end;
      end;

      OutputDebugString('Debugger attach time='+(GetTickCount64-starttime).ToString);

      mainform.ProcessLabel.Caption:=ProcessList.Items[Processlist.ItemIndex];

      ProcessSelected:=true;
      mainform.debugproc:=true;

      modalresult:=mrOK;
    end
  end else showmessage(rsFirstSelectAProcess);

end;

procedure TProcessWindow.btnOpenFileClick(Sender: TObject);
begin
  {$ifdef windows}
  if opendialog2.execute then
  begin
    if frmOpenFileAsProcessDialog=nil then
      frmOpenFileAsProcessDialog:=tfrmOpenFileAsProcessDialog.create(self);

    if frmOpenFileAsProcessDialog.showmodal=mrok then
    begin
      DBKFileAsMemory(opendialog2.filename, frmOpenFileAsProcessDialog.startaddress);
      processselected:=true;
      ProcessHandler.ProcessHandle:=QWORD(-2);
      MainForm.ProcessLabel.caption:=extractfilename(opendialog2.FileName);
      MainForm.miSaveFile.visible:=true;
      ProcessHandler.processid:=$FFFFFFFF;

      Processhandler.is64Bit:=frmOpenFileAsProcessDialog.rb64.checked;

      modalresult:=mrok;
    end;
  end;
  {$else}
  MessageDlg('Not yet implemented', mtError,[mbok],0);
  {$endif}

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
  fltr:=filter;
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
//  setbuttons;
end;

procedure TProcessWindow.btnProcessListLongClick(Sender: TObject);
begin
  if processlistlong=nil then
  begin
    processlist.Clear;
    miProcessListLong.Caption:=rsScanningClickToStop;
    processlistlong:=tprocesslistlong.create(true);
    processlistlong.processlist:=processlist;
    processlistlong.start;
  end
  else
  begin
    miProcessListLong.Caption:=rsProcessListLong;
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
    miProcessListLong.Caption:=rsProcessListLong;
  end;


  position:=poDesigned;
end;

procedure TProcessWindow.PopupMenu1Popup(Sender: TObject);
begin
  miShowInvisibleItems.visible:=tabheader.TabIndex=2;
end;

procedure TProcessWindow.ProcessListDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  i: integer;
  t: string;

  sep: integer;

  pids: string;
  pid: dword;
  {$IFDEF WINDOWS}
  pli: PProcessListInfo;
  {$ENDIF}
begin
  wantedheight:=ProcessList.canvas.TextHeight('QqJjWwSs')+3;
  {i:=ProcessList.canvas.TextHeight('QqJjWwSs')+3;
  if processlist.itemheight<i then ProcessList.ItemHeight:=i;   }

  processlist.Canvas.FillRect(rect);
  {
  i:=ProcessList.canvas.TextHeight('QqJjWwSs')+3;
  if processlist.itemheight<i then ProcessList.ItemHeight:=i;}


  t:=processlist.Items[index];
  if miConvertPIDToDecimal.checked then
  begin
    sep:=pos('-',t);
    if sep<>0 then
    begin
      try
        pids:=copy(t,1,sep-1);
        pid:=strtoint('$'+pids);
        t:=format('%.8d',[pid])+copy(t,sep);
      except
      end;
    end;
  end;



  if odSelected in state then
    processlist.Canvas.font.color:=clHighlightText
  else
    processlist.Canvas.font.color:=processlist.font.color;

  processlist.Canvas.TextOut(rect.Left+rect.Bottom-rect.Top+3,rect.Top,t);

  if getConnection<>nil then exit;

  {$ifdef windows}
  if getprocessicons and (processlist.Items.Objects[index]<>nil) then
  begin
    pli:=PProcessListInfo(processlist.Items.Objects[index]);
    if pli^.processIcon=0 then
      pli^.processIcon:=IconFetchThread.queueIconFetch(pli^.processID, pli^.winhandle, index);

    if (pli^.processIcon<>0) and (pli^.processIcon<>HWND(-1)) then
      DrawIconEx(processlist.Canvas.Handle, rect.left, rect.Top, pli^.processIcon, rect.Bottom-rect.Top,rect.Bottom-rect.Top,0,0,DI_NORMAL);
  end;
  {$endif}
end;

procedure TProcessWindow.FormShow(Sender: TObject);
var
  tr: trect;
  preferedwidth: integer;

  tabwidth: integer;
  pc: integer;
  s: string;
  i: integer;
begin
  if getconnection<>nil then
    tabheader.ShowTabs:=true;

  OKButton.Constraints.MinHeight:=trunc(1.2*btnAttachDebugger.height);
  CancelButton.Constraints.MinHeight:=OKButton.Constraints.MinHeight;

  loadCommonProcessesList;
  errortrace:=100;
  try
    errortrace:=101;
    processlist.canvas.Refresh;

    errortrace:=102;
    ProcessList.ItemHeight:=max(processlist.canvas.TextHeight('QqJjWwSs')+3, canvas.TextHeight('QqJjWwSs')+3);
    errortrace:=103;
    currentchar:=1;
    errortrace:=104;
    refreshlist;
    errortrace:=105;

    if autosize then
    begin
      autosize:=false;
      //first run or no saving positions
      preferedwidth:=max(clientwidth, canvas.TextWidth('  XXXXXXXX - XXXXXXXXXXXXXXXXXXXXXX  '));


      pc:=tabheader.PageCount;
      tabwidth:=0;
      for i:=0 to pc-1 do
      begin
        tr:=tabheader.TabRect(i);
        tabwidth:=tabwidth+tr.Width;
      end;
      tabwidth:=tabwidth+ canvas.TextWidth(' ');

      if tabwidth>preferedwidth then
        preferedwidth:=tabwidth;



      clientwidth:=preferedwidth;
      height:=mainform.Height-(mainform.height div 3);
      position:=poDesigned;
      position:=poMainFormCenter;



    end;
    errortrace:=106;


    processlist.SetFocus;

  except
    on e:exception do
      raise exception.create('FormShow exception ('+e.message+') at section '+inttostr(errortrace));

  end;

end;

procedure TProcessWindow.ProcessListKeyPress(Sender: TObject; var Key: char);
begin
  if key=#8 then
    filter:=copy(filter, 1, length(filter)-1)
  else
  if key in [chr(32)..chr(128)] then
    filter:=filter+key;
end;

procedure TProcessWindow.RefreshList;
var
    i: integer;
    oldselectionindex: integer;
    oldselection: string;
    found: boolean;

begin
  {$ifdef windows}
  IconFetchThread.reset;
  {$endif}

  processlist.Items.BeginUpdate;
  try
    oldselectionindex:=processlist.ItemIndex;

    if oldselectionindex<>-1 then
      oldselection:=processlist.Items[oldselectionIndex];

    case TabHeader.TabIndex of
      0:
      begin
        {$ifdef windows}
        getwindowlist2(processlist.Items);
        {$else}
        getapplicationlist(processlist.items);
        {$endif}
      end;

      1:
      begin
        getprocesslist(processlist.items);
      end;

      2:
      begin
        {$ifdef windows}
        GetWindowList(processlist.Items, miShowInvisibleItems.Checked);
        processlist.ItemIndex:=processlist.Items.Count-1;
        {$else}
        getprocesslist(processlist.items);
        {$endif}
      end;
    end;

    filterlist;

    if oldselectionindex=-1 then
    begin
      processlist.ItemIndex:=processlist.Items.Count-1; //go to the end
    end
    else
    begin
      i:=processlist.Items.IndexOf(oldselection);
      if i>=0 then
      begin
        processlist.ItemIndex:=i;
      end
      else
      begin
        //strip out the processid part and search for a entry with the appropriate processname (e.g restarted game)
        oldselection:=copy(oldselection,pos('-',oldselection)+1,length(oldselection));

        found:=false;
        for i:=processlist.Items.Count-1 downto 0 do
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

    if filter<>'' then
      caption:=rsProcessList+' : *'+filter+'*'
    else
      caption:=rsProcessList;

    {$ifdef windows}
    if formsettings.cbKernelReadWriteProcessMemory.checked or (dbvm_version>=$ce000004) then //driver is active
    begin
      if TabHeader.TabIndex<=2 then //other script are on their own
        processlist.Items.Insert(0, '00000000-['+rsPhysicalMemory+']');
    end;
    {$endif}

  finally
    processlist.items.EndUpdate;
  end;
end;

procedure TProcessWindow.miShowInvisibleItemsClick(Sender: TObject);
begin
  refreshList;
end;

procedure TProcessWindow.TabHeaderChange(Sender: TObject);
begin
  refreshList;
end;

procedure TProcessWindow.TabHeaderResize(Sender: TObject);
var p: tpoint;
begin
  p:=TabHeader.ClientToParent(point(0,0));

  //if TabHeader.ShowTabs=false then
 //   processlist.top:=tabheader.top
 // else
    processlist.Top:=p.Y;

  processlist.Left:=p.X;
  processlist.Width:=TabHeader.ClientWidth;

 // if tabheader.ShowTabs=false then
    processlist.Height:=TabHeader.ClientHeight
 // else
 //   processlist.Height:=tabheader.Height;
end;

procedure TProcessWindow.Timer1Timer(Sender: TObject);
var
  i: integer;
  {$ifdef windows}
  e: PIconFetchEntry;
  {$endif}
begin
  try
    if processlist.itemheight<>wantedheight then
    begin
      ProcessList.ItemHeight:=wantedheight;
      processlist.canvas.Refresh;
      processlist.Repaint;
    end;

    {$ifdef windows}

    IconFetchThread.resolvedListCS.enter;
    try
      e:=nil;
      for i:=0 to IconFetchThread.resolvedList.count-1 do
      begin
        e:=PIconFetchEntry(IconFetchThread.resolvedList[i]);
        iconFetchedEvent(IconFetchThread, e^.processid, e^.index, e^.icon);
        freemem(e);
      end;
      IconFetchThread.resolvedList.clear;
    finally
      IconFetchThread.resolvedListCS.leave;
    end;


    if e<>nil then processlist.Repaint;
    {$endif}

  except
    timer1.enabled:=false;
    showmessage('timer issue');
  end;
end;


initialization
  {$i ProcessWindowUnit.lrs}

end.


