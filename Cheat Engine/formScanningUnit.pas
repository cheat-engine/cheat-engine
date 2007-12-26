unit formScanningUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls,cefuncproc,debugger;

const donescanning=wm_user+1;   //wparam=numberfound
      ESScanningBool=wm_user+2; //wparam=address of scanning boolean.  (should be the same in ce, but I like to verify it)
      ESSettings=wm_user+3;
      ESSetProgressbarMax=wm_user+4; //wparam=max
      ESSetProgressbarPos=wm_user+5; //wparam=currentpos
      HSThreadID=wm_user+6; //wparam=threadid



type
  TScanner = class(TThread)
  private
  public
    constructor Create2(pr: integer);
    procedure Execute; override;
  end;

type
  TformScanning = class(TForm)
    btnCancel: TButton;
    Label1: TLabel;
    Timer1: TTimer;
    Button1: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Timer2: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Timer2Timer(Sender: TObject);
  private
    { Private declarations }
    stopnow:boolean;
    start: tdatetime;
    scanner: TScanner;

    SEScanningloc: pointer;
    hyperscanthreadid: dword;

  public
    { Public declarations }
    ESSettings: TScanSettings;
    canclose:boolean;
    button: integer;  //0=new/first  1=next
    scan: integer; //0=get mamory ranger  1=get memory+scan  2=nextscan

    fromadd,toadd: dword;
    readonly,fastscan,LowMemoryUsage: boolean;
    stype,vtype: integer;
    scanvalue: string;
    scanvalue2: string;
    hexadecimal: boolean;
    unicode: boolean;
    percentage: boolean;
    priority: integer;

    addresstofind:dword;
    pointerinfo: Tbytes;
    winhandle: Hwnd;
    ESScanSettingsLoc: pointer;
    ExtremeScan:boolean;
    roundingtype: tfloatscan;
    Skip_PAGE_NOCACHE:boolean;
    lasterror: string;
    procedure Finished(var Message: TMessage); message donescanning;
    procedure SetSEScanningLocation(var Message: TMessage); message ESScanningBool;
    procedure SetSettingsLocation(var Message: TMessage); message ESSettings;
    procedure SetProgressbarMax(var Message:TMessage); message ESSetProgressbarMax;
    procedure SetProgressbarPos(var Message:TMessage); message ESSetProgressbarPos;
    procedure GetHSThreadID(var Message:TMessage); message HSThreadID;
  end;

var
  formScanning: TformScanning;



implementation

uses MainUnit, formsettingsunit;

{$R *.dfm}

constructor TScanner.Create2(pr: integer);
begin
  inherited create(true);

  case pr of
    0: priority:=tpIdle;
    1: priority:=tpLowest;
    2: priority:=tpLower;
    3: priority:=tpLower;
    4: priority:=tpNormal;
    5: priority:=tpHigher;
    6: Priority:=tpHighest;
    7: priority:=tpTimeCritical;
  end;
  resume;
end;

procedure TScanner.Execute;
var count: integer;
    original: dword;
    fr: dword;
begin
  if formscanning.extremescan then
  begin
    //set the scansettings
    hyperscanview^.scanning:=true;
    hyperscanview^.mainformHandle:=mainform.Handle;
    hyperscanview^.formscanningHandle:=formscanning.Handle;
    hyperscanview^.StartAddress:=formscanning.fromadd;
    hyperscanview^.StopAddress:=formscanning.toadd;
    hyperscanview^.Scantype:=formscanning.Stype;
    hyperscanview^.ValueType:=formscanning.VType;
    hyperscanview^.roundingtype:=formscanning.roundingtype;
    hyperscanview^.scan:=formscanning.scan;
    hyperscanview^.readonly:=formscanning.readonly;
    hyperscanview^.FastScan:=formscanning.fastscan;
    hyperscanview^.Hexadecimal:=formscanning.hexadecimal;
    hyperscanview^.unicode:=formscanning.unicode;
    hyperscanview^.percentage:=formscanning.percentage;
    hyperscanview^.LowMemoryUsage:=formscanning.LowMemoryUsage;
    hyperscanview^.scanvalue:=formscanning.scanvalue;
    hyperscanview^.scanvalue2:=formscanning.scanvalue2;
    hyperscanview^.CheatEngineDir:=CheatEngineDir;
    hyperscanview^.Skip_PAGE_NOCACHE:=formscanning.Skip_PAGE_NOCACHE;
    hyperscanview^.scan_mem_private:=scan_mem_private;
    hyperscanview^.scan_mem_image:=scan_mem_image;
    hyperscanview^.scan_mem_mapped:=scan_mem_mapped;
    hyperscanview^.buffersize:=buffersize;
    hyperscanview^.priority:=formscanning.Priority;

    if hypermode<>nil then
      postmessage(hypermode.HyperscanWindow,wm_user+2,0,0); //wm_user+2=scan memory

    exit;
  end;

  try
    FreeOnTerminate:=true;
    with formscanning do
    case scan of
      0  : count:=GetMemoryRanges2(FromAdd,ToAdd,readonly,mainform.progressbar1,vtype,fastscan);
      1  : count:=GetMemoryRangesAndScanValue2(fr,FromAdd,ToAdd,readonly,false,SType,vtype,scanvalue,scanvalue2,roundingtype,hexadecimal,mainform.progressbar1,fastscan,unicode);
      2  : count:=nextscan2(scanvalue,scanvalue2,stype,vtype,roundingtype,hexadecimal,mainform.progressbar1,fastscan,unicode,percentage);
      3  : count:=FindPointer(fromadd,toadd,addresstofind,mainform.ProgressBar1,pointerinfo);
      else count:=0;
    end;

    //count now contains the number of result
    postmessage(formScanning.handle,donescanning,count,0);
  except
    on e: exception do
    begin
      formscanning.lasterror:=e.Message;
      closefiles;
      postmessage(formScanning.handle,donescanning,0,1);
    end;
  end;

end;


procedure TFormScanning.SetProgressbarMax(var Message:TMessage);
begin
  mainform.ProgressBar1.Max:=Message.WParam;
end;

procedure TFormScanning.SetProgressbarPos(var Message:TMessage);
begin
  mainform.ProgressBar1.Position:=message.WParam;
end;

procedure TFormScanning.SetSettingsLocation(var Message: TMessage);
begin
  ESScanSettingsLoc:=pointer(1);
  btnCancel.Enabled:=true;
end;

procedure TFormScanning.SetSEScanningLocation(var Message: TMessage);
begin
  sescanningloc:=pointeR(message.WParam);
  btnCancel.Enabled:=true;
end;

procedure TFormScanning.GetHSThreadID(var Message:TMessage);
var i: integer;
begin
  btnCancel.Enabled:=true;
  hyperscanthreadid:=Message.WParam;
  if mainform.cbfasterscan.checked and mainform.cbPauseWhileScanning.checked then
  begin
      //freeze all the threads except the hyperscan thread
      for i:=0 to length(debuggerthread.threadlist)-1 do
        if debuggerthread.threadlist[i][0]<>hyperscanthreadid then //if it's not my thread then freeze it
          suspendthread(debuggerthread.threadlist[i][1]);
  end;
end;

procedure TformScanning.Finished(var Message: TMessage);
var i: integer;
begin
  //done scanning so close the window and resume if hyperscan+pause

  if mainform.cbfasterscan.checked and mainform.cbPauseWhileScanning.checked then
  begin
    //resume all the frozen theads except the hyperscan thread
    for i:=0 to length(debuggerthread.threadlist)-1 do
      if debuggerthread.threadlist[i][0]<>hyperscanthreadid then //if it's not my thread then freeze it
        resumethread(debuggerthread.threadlist[i][1]);
  end;

  if message.LParam<>0 then
  begin
    messagebox(formscanning.handle,pchar('Error while scanning:'+lasterror),'Cheat Engine',MB_OK or MB_ICONERROR);
    canclose:=true;
    modalresult:=mrcancel;
  end;

  mainform.foundcount:=message.WParam;

  canclose:=true;
  modalresult:=mrok;
end;

procedure TformScanning.Timer1Timer(Sender: TObject);
var temp:string;
    x: double;
    h,m,s,ms:word;
begin
  if stopnow then
  begin
    timer1.Enabled:=false;
    close;
    exit;
  end;

  temp:=copy(label1.caption,2,length(label1.caption)-1)+label1.caption[1];
  label1.caption:=temp;

  if mainform.Progressbar1.position=0 then exit;

  x:=((now-start)/mainform.Progressbar1.position)*(mainform.ProgressBar1.Max-mainform.Progressbar1.position);
  decodetime(x,h,m,s,ms);
  label3.Caption:=format('%0.2d:%0.2d:%0.2d',[h,m,s]);
end;

procedure TformScanning.btnCancelClick(Sender: TObject);
var bt:byte;
    x:dword;
    i:integer;
begin
  //the scan got terminated
  if not mainform.cbFasterScan.checked then
  begin
    terminatethread(scanner.Handle,0);
    freememory;
    closefiles;

    canclose:=true;
    modalresult:=mrCancel;
  end
  else
  begin
    //if paused resume the game
    if mainform.cbPauseWhileScanning.checked then
    begin
      //it is paused
      for i:=0 to length(debuggerthread.threadlist)-1 do
        if debuggerthread.threadlist[i][0]<>hyperscanthreadid then //if it's not my thread then freeze it
          resumethread(debuggerthread.threadlist[i][1]);

      debuggerthread.resume;
    end;

    if hypermode<>nil then
      sendmessage(hypermode.HyperscanWindow,wm_user+3,0,0);  //dont come back until done
      
    canclose:=true;
    modalresult:=mrCancel;
  end;
end;

procedure TformScanning.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
  formscanning:=nil;
end;

procedure TformScanning.FormShow(Sender: TObject);
var winprocess: Dword;
    possiblewinhandle: Thandle;
begin
  //create the thread and hope for the best...
  start:=now;

  if mainform.cbFasterScan.checked then
  begin
    btncancel.Enabled:=false;  //enable it when the threadid is received
    if (hypermode=nil) or (hypermode.HyperscanWindow=0) then
    begin
      messagedlg('Hyperscan was not initialized succesfully',mterror,[mbok],0);
      canclose:=true;
      modalresult:=mrCancel;
      stopnow:=true;
      mainform.cbFasterScan.Checked:=false;
      exit;
    end;
  end;

  if mainform.cbFasterScan.checked then
    Scanner:=TScanner.create2(3) //dont set a higher priority in this thread
  else
    Scanner:=TScanner.create2(formsettings.combothreadpriority.itemindex);


  timer2.Enabled:=true;
end;

procedure TformScanning.FormCreate(Sender: TObject);
begin
  canclose:=false;
end;

procedure TformScanning.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  canclose:=self.canclose;
end;

procedure TformScanning.Timer2Timer(Sender: TObject);
begin
  timer2.enabled:=false;
  if mainform.cbFasterScan.checked and (not btnCancel.Enabled) then
  begin
    messagedlg('hyperscan failed! The remote scan routine didn''t tell me how to cancel the scan, which makes me to believe the scan never started.'#13#10'I have disable hyperscan. Try the scan again',mtError,[mbok],0);
    mainform.cbfasterscan.Checked:=false;

    canclose:=true;
    modalresult:=mrCancel;
  end;
end;

end.
