unit ProcessWindowUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CEFuncProc,debugger, ComCtrls, ImgList,
  undochanges,filehandler, Menus,tlhelp32,newkernelhandler;

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
  TProcessWindow = class(TForm)
    ProcessList: TListBox;
    Panel1: TPanel;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    PopupMenu1: TPopupMenu;
    InputPIDmanually1: TMenuItem;
    Filter1: TMenuItem;
    Panel2: TPanel;
    Button6: TButton;
    Button2: TButton;
    Button1: TButton;
    Button5: TButton;
    Button3: TButton;
    Button4: TButton;
    CancelButton: TButton;
    OKButton: TButton;
    btnProcessListLong: TButton;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure InputPIDmanually1Click(Sender: TObject);
    procedure Filter1Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure ProcessListKeyPress(Sender: TObject; var Key: Char);
    procedure ProcessListClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnProcessListLongClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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
    procedure PWOP(ProcessIDString:string);
  public
    { Public declarations }
  end;

var
  ProcessWindow: TProcessWindow;

implementation

{$R *.DFM}

uses MainUnit, formsettingsunit,memorybrowserformunit,advancedoptionsunit,
  frmProcessWatcherUnit;

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
    ths: thandle;
    me32:MODULEENTRY32;
    x: pchar;
    modulename:string;
    pno: integer;
    j: integer;
    pe: dword;

begin
  freeonterminate:=true;
  i:=0;

 { //failed experiment: processlist by trying each pid and peprocess lookup (good theory, but windows crashes)
  if DarkByteKernel<>0 then
  begin
    pno:=GetProcessnameOffset;
    if pno<>0 then
    begin
      getmem(x,32);
      try
        while (not terminated) and (i<$7fffffff) do
        begin
          pe:=getpeprocess(i);
          if pe<>0 then
          begin
            //GetProcessNameFromPEProcess(pe,x,31);

            process[processcount]:=inttohex(i,8)+'-'+x;
            inc(processcount);
            if processcount>=10 then
              synchronize(drawprocesses);
          end;


          if ((i mod 4096)=0) then
            if processcount>0 then synchronize(drawprocesses);

          inc(i,4);
        end;

      finally
        freemem(x);
      end;

      if processcount>0 then synchronize(drawprocesses);
      exit;
    end;
  end;   }

  freeonterminate:=true;
  me32.dwSize:=sizeof(MODULEENTRY32);


  while not terminated and (i<$FFFFFFFF) do
  begin
    h:=windows.OpenProcess(PROCESS_ALL_ACCESS,false,i);
    if h<>0 then
    begin
      modulename:='???';
      ths:=CreateToolhelp32Snapshot(TH32CS_SNAPMODULE,i);
      if ths<>0 then
      begin
        if Module32First(ths,me32) then
        begin
          x:=me32.szModule;
          modulename:=x;
        end;

        process[processcount]:=inttohex(i,8)+'-'+modulename;
        inc(processcount);
        if processcount>=10 then
          synchronize(drawprocesses);

        closehandle(ths);
      end;
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
    0: button1.Click;
    1: button2.click;
  end;

  filterlist;
end;

procedure TProcessWindow.CancelButtonClick(Sender: TObject);
begin
  mainform.canceled:=true;

  //ProcessWindow.close;
  ModalResult:=mrCancel;
end;

procedure TProcessWindow.setbuttons;
begin
  if not formsettings.cbShowAdvanced.Checked then
  begin
    button4.Visible:=false;
    button3.Visible:=false;
    Panel1.Height:=panel1.Height-40;
    button1.Top:=button1.Top-40;
    button2.Top:=button2.Top-40;
  end;

  if formsettings.cbProcesswatcher.Checked then
  begin
    button6.Visible:=true;
    button1.Left:=7;
    button2.Left:=83;
    button6.Left:=159;
  end else
  begin
    button6.Visible:=false;
    button1.Left:=44;
    button2.Left:=120;
  end;


end;

procedure TProcessWindow.FormCreate(Sender: TObject);
var icons: Tbytes;
begin
  currentchar:=1;
  button1.click;


  setbuttons;
end;

procedure TProcessWindow.PWOP(ProcessIDString:string);
var i:integer;
begin

  val('$'+ProcessIDString,ProcessID,i);
  if i<>0 then raise exception.Create(processidstring+' isn''t a valid processID');
  if Processhandle<>0 then
  begin
    CloseHandle(ProcessHandle);
    ProcessHandle:=0;
  end;

  with mainform do
  begin
    cbFasterscan.checked:=false;

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

  if formsettings.cbEnableHyperscanWhenPossible.checked then
    mainform.cbFasterscan.Checked:=true;


  if (processid=0) and (formsettings.cbKernelReadWriteProcessMemory.checked) then
  begin
    Processid:=$FFFFFFFF;
    DBKPhysicalMemory;
    Processhandle:=$FFFFFFFF;
  end
  else
    DBKProcessMemory;

end;

procedure TProcessWindow.OKButtonClick(Sender: TObject);
var ProcessIDString: String;
    i:               Integer;
begin
  if formsettings.cbUndoMemoryChanges.checked then CheckForChanges;

  if Processlist.ItemIndex>-1 then
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

    PWOP(ProcessIDString);
    MainForm.ProcessLabel.caption:=ProcessList.Items[Processlist.ItemIndex];
    Modalresult:=MROK;
    //ProcessWindow.close;
  end;
end;



procedure TProcessWindow.Button1Click(Sender: TObject);
begin
  currentlisT:=0;
  getprocesslist(processlist);
  if formsettings.cbKernelReadWriteProcessMemory.checked then //driver is active
    processlist.Items.Insert(0,'00000000-[Physical Memory]');

  Filterlist;
end;

procedure TProcessWindow.Button2Click(Sender: TObject);
begin
  currentlist:=1;

  getwindowlist(processlist{,icons});
  filterlist;
end;

procedure TProcessWindow.Button3Click(Sender: TObject);
begin
  if Opendialog1.Execute then
  begin
    unpause;
    detachIfPossible;
    if Uppercase(extractfileext(opendialog1.FileName))<>'.EXE' then raise Exception.Create('You can only load EXE files');

    Debuggerthread:=TDebugger.MyCreate(opendialog1.FileName);

   // processhandle:=$ffffffff;
//    processid:=$ffffffff;


    while (debuggerthread<>nil) and (debuggerthread.attaching) do sleep(1);

    mainForm.ProcessLabel.caption:=IntToHex(processid,8)+'-'+ExtractFileName(opendialog1.FileName);


    mainform.debugproc:=true;

    if formsettings.cbBreakOnAttach.checked then
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
    unpause;
    DetachIfPossible;

    ProcessIDString:='';
    i:=1;
    while ProcessList.Items[Processlist.ItemIndex][i]<>'-' do
    begin
      ProcessIDString:=ProcessIDString+ProcessList.Items[Processlist.ItemIndex][i];
      inc(i);
    end;

    val('$'+ProcessIDString,ProcessID,i);

    if Processhandle<>0 then
    begin
      CloseHandle(ProcessHandle);
      ProcessHandle:=0;
    end;


    Debuggerthread:=TDebugger.MyCreate2(processid);

    i:=0;
    while i<=90 do
    begin
      if i=100 then raise exception.Create('The thread that was supposed to attach the debugger to the process failed. I recommend restarting Cheat Engine.');
      if not debuggerthread.attaching then i:=100;
      inc(i);
      sleep(100);
    end;

    if not debuggerthread.attached then
      raise exception.Create('The thread that was supposed to attach the debugger to the process failed. ');

    mainform.ProcessLabel.Caption:=ProcessList.Items[Processlist.ItemIndex];

    ProcessSelected:=true;
    mainform.debugproc:=true;

    if formsettings.cbBreakOnAttach.checked then
      memorybrowser.show;

    modalresult:=mrOK;

  end else showmessage('First select a process!');
end;

procedure TProcessWindow.Button5Click(Sender: TObject);
begin
  if opendialog2.execute then
  begin
    DBKFileAsMemory(opendialog2.filename);
    processselected:=true;
    processhandle:=filehandle;
    MainForm.ProcessLabel.caption:=extractfilename(opendialog2.FileName);
    processid:=$FFFFFFFF;

    modalresult:=mrok;
  end;
end;

procedure TProcessWindow.InputPIDmanually1Click(Sender: TObject);
var pid: string;
begin
  pid:='0';
  if InputQuery('Manual PID','Enter the ProcessID:',pid) then
  begin
    if formsettings.cbUndoMemoryChanges.checked then CheckForChanges;

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
  if inputquery('Filter','What are you looking for?',fltr) then
    filter:=fltr;
end;

procedure TProcessWindow.Button6Click(Sender: TObject);
begin
  if frmprocesswatcher=nil then
    frmprocesswatcher:=tfrmprocesswatcher.Create(mainform);
  frmprocesswatcher.show;
  modalresult:=mrcancel;
end;

const pr='protectthis';
procedure TProcessWindow.ProcessListKeyPress(Sender: TObject;
  var Key: Char);
var processid: dword;
    processidstring: string;
    i:integer;
begin
  if lowercase(key)=pr[currentchar] then
  begin
    if currentchar=length(pr) then
    begin
      if messagedlg('You can only protect one process at a time (And that includes cheat engine).  Are you sure you want to protect this process?',mtconfirmation,[mbyes,mbno],0)=mryes then
      begin
        ProcessIDString:='';
        i:=1;
        while ProcessList.Items[Processlist.ItemIndex][i]<>'-' do
        begin
          ProcessIDString:=ProcessIDString+ProcessList.Items[Processlist.ItemIndex][i];
          ProcessID:=strtoint('$'+processIDString);
          inc(i);
        end;
        protectprocess(ProcessID);
      end;

    end;

    inc(currentchar);
  end else currentchar:=1;


end;

procedure TProcessWindow.ProcessListClick(Sender: TObject);
begin
  currentchar:=1;
end;

procedure TProcessWindow.FormResize(Sender: TObject);
begin
//reset the button positions
  setbuttons;
end;

procedure TProcessWindow.btnProcessListLongClick(Sender: TObject);
var i:dword;
    h: thandle;
begin
  if processlistlong=nil then
  begin
    processlist.Clear;
    btnprocesslistlong.Caption:='Scanning (Click to stop)';
    processlistlong:=tprocesslistlong.create(true);
    processlistlong.processlist:=processlist;
    processlistlong.Resume;
  end
  else
  begin
    btnprocesslistlong.Caption:='Process List(long)';
    processlistlong.terminate;
  end;
end;

procedure TProcessWindow.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if processlistlong<>nil then
    processlistlong.terminate;
end;

end.

