unit pointerscannerfrm;

{$MODE Delphi}

//todo: Make a command prompt version of the distributed scanning pointerscan client, and make it functional in linux as well (real servers)

interface

uses
  windows, LCLIntf, LResources, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, syncobjs,syncobjs2, Menus, math,
  frmRescanPointerUnit, pointervaluelist, rescanhelper,
  virtualmemory, symbolhandler,MainUnit,disassembler,CEFuncProc,NewKernelHandler,
  valuefinder, PointerscanresultReader, maps, zstream, WinSock2, Sockets,
  registry, PageMap, CELazySocket, PointerscanNetworkCommands, resolve, pointeraddresslist,
  pointerscanworker, PointerscanStructures, PointerscanController;



const staticscanner_done=wm_user+1;
const rescan_done=wm_user+2;
const open_scanner=wm_user+3;

type
  Tfrmpointerscanner=class;
  {
  TPointerscanListener=class(tthread)
  private

    serverip: string;
    serverport:word;
    pointerscannerform: Tfrmpointerscanner;

    procedure DoPointerscan;
    procedure DoRescan;
    procedure DoSendResults;
    procedure DoCommand(command: byte; srecv: sockaddr_in; recvsize: tsocklen; port:word);
  public
    executingCommand: boolean;
    done: boolean;
    procedure execute; override;
    constructor create(owner: TfrmPointerscanner; suspended: boolean);
  end;      }



  TRescanWorker=class(TThread)
  private
    procedure flushresults;
    function isMatchToValue(p: pointer): boolean;
  public
    filename: string;
    tempfile: tfilestream;
    tempbuffer: TMemoryStream;

    novaluecheck: boolean;
    PointerAddressToFind: ptrUint;
    forvalue: boolean;
    valuetype: TVariableType;
    valuesize: integer;
    valuescandword: dword;
    valuescansingle: single;
    valuescandouble: double;
    valuescansinglemax: single;
    valuescandoublemax: double;

    mustbeinrange: boolean;
    baseStart: ptruint;
    baseEnd: ptruint;

    startOffsetValues: array of dword;
    endoffsetvalues: array of dword;


    //---
    Pointerscanresults: TPointerscanresultReader;
    pointermap: TPointerListHandler;

    startentry: qword;
    EntriesToCheck: qword;

    rescanhelper: TRescanhelper;

    evaluated: qword;

    useluafilter: boolean; //when set to true each pointer will be passed on to the luafilter function
    luafilter: string; //function name of the luafilter

    done: boolean;
    procedure execute; override;
    destructor destroy; override;
  end;


  Trescanpointers=class(tthread)
  private
    sockethandle: Tsocket;
    sockethandlecs: TCriticalSection;

    workers: array of record
      s: Tsocket;
      TotalPointersToEvaluate: qword;
      PointersEvaluated: qword;
      done: boolean;
    end;

    rescanworkercount: integer;
    rescanworkers: array of TRescanWorker;

    rescanhelper: TRescanHelper;
    Pointerscanresults: TPointerscanresultReader;

    broadcastcount: integer;
    lastBroadcast: dword;
   {
    function Server_HandleRead(s: Tsocket): byte;
        }
    procedure closeOldFile;
    {
    procedure UpdateStatus(done: boolean; TotalPointersToEvaluate:qword; PointersEvaluated: qword);
    procedure LaunchWorker;
    procedure LaunchServer;

    procedure broadcastscan; //sends a broadcast to the local network and the potentialWorkerList
    procedure DoServerLoop;}

  public
    ownerform: TFrmPointerScanner;
    progressbar: tprogressbar;
    filename: string;
    originalptrfile: string;

    pointermapfilename: string;
    pointermapprogressbar: tprogressbar;
    pointermapprogressbarlabel: tlabel;
    pointermap: TPointerListHandler;


    overwrite: boolean;
    address: ptrUint;
    forvalue: boolean;
    delay: integer;
    valuetype: TVariableType;
    valuescandword: dword;
    valuescansingle: single;
    valuescandouble: double;
    valuescansinglemax: single;
    valuescandoublemax: double;

    mustbeinrange: boolean;
    baseStart: ptruint;
    baseEnd: ptruint;

    startOffsetValues: array of dword;
    endoffsetvalues: array of dword;

    novaluecheck: boolean; //when set to true the value and final address are not compared, just check that he final address is in fact readable
    useluafilter: boolean; //when set to true each pointer will be passed on to the luafilter function
    luafilter: string; //function name of the luafilter

    {
    distributedserver: string;
    distributedport: integer;
    distributedrescan: boolean;
    distributedrescanWorker: boolean;
    distributedworkfolder: string;

    broadcastThisScanner: boolean;
    potentialWorkerList: array of THostAddr;      }

    waitforall: boolean;

    procedure execute; override;
    destructor destroy; override;
  end;


  { Tfrmpointerscanner }

  Tfrmpointerscanner = class(TForm)
    btnConnect: TButton;
    btnStopRescanLoop: TButton;
    btnStopScan: TButton;
    btnIncreaseThreadCount: TButton;
    btnDecreaseThreadCount: TButton;
    cbTrusted: TCheckBox;
    cbPriority: TComboBox;
    edtIP: TEdit;
    edtPassword: TEdit;
    edtPort: TEdit;
    gbNetwork: TGroupBox;
    lblIP: TLabel;
    lblPort: TLabel;
    lblPassword: TLabel;
    lblThreadPriority: TLabel;
    lblProgressbar1: TLabel;
    MenuItem2: TMenuItem;
    miCreatePSNnode: TMenuItem;
    miResume: TMenuItem;
    miMergePointerscanResults: TMenuItem;
    odMerge: TOpenDialog;
    pnlData: TPanel;
    pnlStop: TPanel;
    pnlControl: TPanel;
    pnlProgressName: TPanel;
    pnlProgressBar: TPanel;
    pnlProgress: TPanel;
    Panel1: TPanel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    N2: TMenuItem;
    Open1: TMenuItem;
    Pointerscanner1: TMenuItem;
    Method3Fastspeedandaveragememoryusage1: TMenuItem;   //I should probably rename this, it's not really, 'average memory usage' anymore...
    N1: TMenuItem;
    ProgressBar1: TProgressBar;
    Rescanmemory1: TMenuItem;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    SaveDialog2: TSaveDialog;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Timer2: TTimer;
    lvResults: TListView;
    PopupMenu1: TPopupMenu;
    Resyncmodulelist1: TMenuItem;
    cbType: TComboBox;
    tvInfo: TTreeView;
    procedure btnStopRescanLoopClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnIncreaseThreadCountClick(Sender: TObject);
    procedure btnDecreaseThreadCountClick(Sender: TObject);
    procedure cbPriorityChange(Sender: TObject);

    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure lvResultsColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvResultsResize(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure miCreatePSNnodeClick(Sender: TObject);
    procedure miMergePointerscanResultsClick(Sender: TObject);
    procedure miResumeClick(Sender: TObject);
    procedure miSetWorkFolderClick(Sender: TObject);
    procedure miJoinDistributedRescanClick(Sender: TObject);
    procedure miJoinDistributedScanClick(Sender: TObject);
    procedure Method3Fastspeedandaveragememoryusage1Click(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Rescanmemory1Click(Sender: TObject);
    procedure btnStopScanClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure New1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lvResultsData(Sender: TObject; Item: TListItem);
    procedure Resyncmodulelist1Click(Sender: TObject);
    procedure lvResultsDblClick(Sender: TObject);
    procedure cbTypeChange(Sender: TObject);
  private
    { Private declarations }
    start:tdatetime;

    rescan: trescanpointers;
    rescanpointerform: TFrmRescanPointer;

      {
    distributedworkfolder: string;

    PointerscanListener: TPointerscanListener;    }

    infonodes: record
      statistics: record
        node: TTreenode; //+ statistics
        stats: record
          totalTimeScanning: TTreenode;
          totalPathsEvaluated: TTreenode;
          calculatedPathsPerSecond: TTreenode;

          pointersInMap: TTreenode;
          pathQueue: TTreenode;
          resultsFound: TTreenode;
          timeSpentWriting: TTreenode;
          minpath: TTreenode;
          maxpath: TTreenode;
        end;

      end;


      localworkers: record
        node: TTreenode;  // + localworkers
        workernodes: array of record
          node: TTreenode; //threadnr (status)
        end;
      end;

      network: record
        node: TTreenode; // + network

        parent: TTreenode;   //ip+port or disconnected
        parentnodes: record
          lastUpdateSent: TTreenode;
        end;

        connectingto: TTreenode;
        connectingToNodes: array of record
          data: TTreenode; //ip, port, trusted
        end;
        connectedTo: TTreenode;
        connectedToNodes: array of record
          node: TTreenode; //ip:port (status)
          data: record
            trusted: TTreenode; //trusted: true/false
            totalthreadcount: TTreenode; //Total threadcount: %d
            resultsfound: TTreenode; //Total results found: %d
            pathqueuesize: TTreenode; //Queuesize: %d
            totalpathquesize: TTreenode; //Total queuesize: %d
            totalpathsEvaluated: TTreenode; //Total paths evaluated: %d
            pathspersecond: TTreenode; //Paths per second: %d
          end;


        end;
      end;
    end;

    procedure m_staticscanner_done(var message: tmessage); message staticscanner_done;
    procedure rescandone(var message: tmessage); message rescan_done;
    procedure openscanner(var message: tmessage); message open_scanner;
    procedure PointerscanStart(sender: TObject);
    procedure doneui;
    procedure resyncloadedmodulelist;
    procedure OpenPointerfile(filename: string);
    procedure stopscan(savestate: boolean);
    procedure PointerscanDone(sender: TObject; hasError: boolean; errorstring: string); //called by the pointerscan controller thread when done
  public
    { Public declarations }
    Staticscanner: TPointerscanController;

    Pointerscanresults: TPointerscanresultReader;
    {
    procedure JoinPointerscan(host: string='127.0.0.1'; port: word=52737; threadcount: integer=1; scannerpriority:TThreadPriority=tpHigher; UseLoadedPointermap: boolean=false; LoadedPointermapFilename: string='');
    procedure JoinRescan(server: string; port: dword);  }
  end;

//var
//  frmPointerScanner: TfrmPointerScanner;


implementation


uses PointerscannerSettingsFrm, frmMemoryAllocHandlerUnit, frmSortPointerlistUnit,
  LuaHandler, lauxlib, lua, frmPointerscanConnectDialogUnit,
  frmpointerrescanconnectdialogunit, frmMergePointerscanResultSettingsUnit,
  ProcessHandlerUnit, frmResumePointerscanUnit, PointerscanConnector,
  frmSetupPSNNodeUnit, PointerscanNetworkStructures;

resourcestring
  rsErrorDuringScan = 'Error during scan';
  rsGeneratingPointermap = 'Generating pointermap...';
  rsIsNotAValid4ByteValue = '%s is not a valid 4 byte value';
  rsIsNotAValidFloatingPointValue = '%s is not a valid floating point value';
  rsIsNotAValidDoubleValue = '%s is not a valid double value';
  rsAddressSpecifiersFoundInTheWholeProcess = 'Address specifiers found in '
    +'the whole process';
  rsPointerPathsFound = 'Pointer paths found';
  rsThreads = 'Threads';
  rsEvaluated = 'Evaluated';
  rsTime = 'Time';
  rsThread = 'Thread';
  rsCurrentLevel = 'Current Level';
  rsLookingFor = 'Looking for';
  rsSleeping = 'Sleeping';
  rsActive = 'Active';
  rsWritingToDisk = 'Writing to disk';
  rsBaseAddress = 'Base Address';
  rsOffset = 'Offset';
  rsPointsTo = 'Points to';
  rsPointercount = 'pointercount';
  rsOnlyTheFirst1000000EntriesWillBeDisplayed = 'Only the first 1000000 '
    +'entries will be displayed. Rescan will still work with all results.  ('
    +'This is normal for a pointerscan, you MUST do a few rescans)';
  rsPointerScan = 'Pointer scan';
  rsPointerscanResult = 'pointerscan result';

  rsTerminating = 'Terminating';
  rsSavingAndTerminating = 'Saving...';
  rsStop = 'Stop';

  rsOUTOFDISKSPACECleanUpTheDiskOrStop = 'OUT OF DISKSPACE! Clean up the disk '
    +'or stop';

//----------------------- scanner info --------------------------
//----------------------- staticscanner -------------------------



{$ifdef benchmarkps}

  //totalpathsevaluated: qword;

 {
var
 starttime: dword;
  startcount: qword;  }
{$endif}


procedure TFrmpointerscanner.doneui;
begin
  progressbar1.position:=0;
  pnlprogress.visible:=false;

  pnlData.Visible:=false;
  open1.Enabled:=true;
  new1.enabled:=true;
  rescanmemory1.Enabled:=true;

  if (staticscanner<>nil) and (staticscanner.generatePointermapOnly) then
    new1.Click;


  if (staticscanner<>nil) and (staticscanner.filename<>'') then
    OpenPointerfile(staticscanner.filename);


  if rescan<>nil then
  begin
    OpenPointerfile(rescan.filename);
    freeandnil(rescan);
  end;

  {
  if (PointerscanListener<>nil) then
    PointerscanListener.executingCommand:=false; //start listening for new commands   }
end;

procedure Tfrmpointerscanner.PointerscanDone(sender: TObject; hasError: boolean; errorstring: string);
begin
  postmessage(Handle,staticscanner_done,0,0);
end;

procedure Tfrmpointerscanner.m_staticscanner_done(var message: tmessage);
begin
  if staticscanner=nil then exit;

  if staticscanner.useHeapData then
    frmMemoryAllocHandler.memrecCS.leave;  //continue adding new entries

  //update the treeview
  if staticscanner.haserror then
    messagedlg(rsErrorDuringScan+': '+staticscanner.errorString, mtError, [mbok] , 0);

  doneui;
end;

{
procedure TPointerscanListener.DoPointerscan;
var cpucount: integer;
begin
  //join a pointerscan
  cpucount:=GetCPUCount;
  if HasHyperthreading then
    cpucount:=(cpucount div 2)+1;

  pointerscannerform.JoinPointerscan(serverip, serverport, cpucount);

end;

procedure TPointerscanListener.DoRescan;
begin
  pointerscannerform.JoinRescan(serverip, serverport);
end;

procedure TPointerscanListener.DoSendResults;
begin

end;

procedure TPointerscanListener.DoCommand(command: byte; srecv: sockaddr_in; recvsize: tsocklen; port:word);
begin
  if executingCommand then exit; //already doing something

  executingCommand:=true;
  serverip:=NetAddrToStr(srecv.sin_addr);
  serverport:=port;

  case command of
    0: synchronize(DoPointerscan);
    1: synchronize(DoRescan);
    2: synchronize(DoSendResults);
  end;

end;

procedure TPointerscanListener.execute;
var
  s: TSocket;
  srecv: sockaddr_in;
  recvsize: tsocklen;
  cecommand: packed record
    id: byte; // 0xce
    operation: byte;
    port: word;
    test: word;
  end;
  i: integer;
begin
  i:=0;

  s:=fpsocket(PF_INET, SOCK_DGRAM, 0);
  if s>=0 then
  begin
    srecv.sin_family:=PF_INET;
    srecv.sin_addr.s_addr:=htonl(INADDR_ANY);
    srecv.sin_port:=htons(3297);
    i:=fpbind(s, @srecv, sizeof(srecv));

    while (i>=0) and (not terminated) do
    begin
      ZeroMemory(@srecv, sizeof(srecv));
      recvsize:=sizeof(srecv);

      i:=fprecvfrom(s, @cecommand, sizeof(cecommand), 0, @srecv, @recvsize);
      if (i=sizeof(cecommand)) and (cecommand.id=$ce) and (cecommand.test=word((cecommand.id+cecommand.operation+cecommand.port)*599)) then
        DoCommand(cecommand.operation, srecv, recvsize, cecommand.port);
    end;

  end;

  CloseSocket(s);
  done:=true; //todo: Perhaps relaunch ?
end;

constructor TPointerscanListener.create(owner: Tfrmpointerscanner; suspended: boolean);
begin
  self.pointerscannerform:=owner;
  inherited create(suspended);
end;
          }


//---------------------------------main--------------------------
               {
procedure Tfrmpointerscanner.JoinRescan(server: string; port: dword);
begin
  if rescan<>nil then
    freeandnil(rescan);

  rescan:=trescanpointers.create(true);
  rescan.ownerform:=self;
  rescan.progressbar:=progressbar1;


  rescan.distributedrescan:=true;
  rescan.distributedrescanWorker:=true;
  rescan.distributedserver:=server;
  rescan.distributedport:=port;
  rescan.distributedworkfolder:=distributedworkfolder;
  pnlprogress.visible:=true;

  rescan.start;
end;
       }
       {
procedure Tfrmpointerscanner.JoinPointerscan(host: string='127.0.0.1'; port: word=52737; threadcount: integer=1; scannerpriority:TThreadPriority=tpHigher; UseLoadedPointermap: boolean=false; LoadedPointermapFilename: string='');
begin

  new1.click; //setup the gui

  totalpathsevaluated:=0;
  startcount:=0;
  starttime:=0;


  btnStopScan.enabled:=true;
  btnStopScan.Caption:=rsStop;

  pgcPScandata.Visible:=false;
  open1.Enabled:=false;
  new1.enabled:=false;
  rescanmemory1.Enabled:=false;

  cbType.Visible:=false;
  lvResults.Visible:=false;



  //launch the scanner
  staticscanner:=TPointerscanController.Create(true);


  label5.caption:=rsGeneratingPointermap;
  pnlProgress.Visible:=true;


  staticscanner.distributedScanning:=true;
  staticscanner.distributedWorker:=true;
  staticscanner.distributedServer:=host;
  staticscanner.distributedport:=port;
  staticscanner.progressbar:=progressbar1;
  staticscanner.threadcount:=threadcount;
  staticscanner.scannerpriority:=scannerpriority;
  staticscanner.UseLoadedPointermap:=UseLoadedPointermap;
  staticscanner.LoadedPointermapFilename:=LoadedPointermapFilename;

  staticscanner.filename:=IncludeTrailingPathDelimiter(distributedworkfolder);
  staticscanner.pointerlisthandler:=pointerlisthandler;

  open1.Enabled:=false;

  staticscanner.start;

  pgcPScandata.Visible:=true;
end;     }

procedure Tfrmpointerscanner.miJoinDistributedScanClick(Sender: TObject);
{var
  f: tfrmPointerscanConnectDialog;}

begin
{
  f:=tfrmPointerscanConnectDialog.create(self);
  if f.showmodal=mrok then
  begin
    if distributedworkfolder='' then
      miSetWorkFolder.Click;

    if distributedworkfolder='' then exit;

    JoinPointerscan(f.edthost.text, f.port, f.threadcount, f.scannerpriority, f.cbUseLoadedPointermap.checked, f.odLoadPointermap.FileName);
  end;

  f.free;   }
end;

procedure Tfrmpointerscanner.miResumeClick(Sender: TObject);
var
  f: tfrmresumePointerScan;
  filename: string;

{
maxlevel: dword
structsize: dword; //sz
totalpathsevaluated: dword
compressedptr: byte;  //boolean
unalligned: byte; //boolen
noloop: byte; //boolean
muststartwithbase; byte //boolean
LimitToMaxOffsetsPerNode: byte //boolean
onlyOneStaticInPath: byte; //boolean
instantrescan: byte //boolean (not really needed, but it's a nice padding)
mustEndWithSpecificOffset: byte; //boolean ( ^ ^ )
maxoffsetspernode: integer;
basestart: qword;
basestop: qword;


mustendwithoffsetlistlength: integer
mustendwithoffsetlist[]: dword*mustendwithoffsetlistlength



instantrescancount: integer;
instantrescanentry []: record[] [
  filenamelength: integer
  filename: char[filenamelength] //full path
  address: qword
]
}
var
  threadcount: integer;

  config: Tfilestream;
  maxlevel: integer;
  structsize: integer;
  compressedptr: boolean;
  unalligned: boolean;
  noloop: boolean;
  muststartwithbase: boolean;
  LimitToMaxOffsetsPerNode:boolean;
  onlyOneStaticInPath:boolean;
  instantrescan:boolean; //(not really needed, but it's a nice padding)
  mustEndWithSpecificOffset:boolean;// ( ^ ^ )
  maxoffsetspernode: integer;
  basestart: qword;
  basestop: qword;

  mustendwithoffsetlist: array of dword;
  instantrescanentries: array of record
    filename: string;
    address: qword;
  end;

  i: integer;

  resumefilelist: tstringlist;

  pb: TProgressbar;
  lb: TLabel;


begin
  //show a dialog where the user can pick the number of threads to scan
  resumefilelist:=nil;

  if (pointerscanresults<>nil) and Pointerscanresults.CanResume then
  begin
    filename:=Pointerscanresults.filename;

    resumefilelist:=tstringlist.create;
    Pointerscanresults.getFileList(resumefilelist);


    try
      config:=TFileStream.Create(filename+'.resume.config', fmOpenRead or fmShareDenyNone);
    except
      exit;
    end;





    maxlevel:=config.ReadDWord;
    structsize:=config.ReadDWord;
   // totalpathsevaluated:=config.ReadQWord; //IGNORED
    compressedptr:=config.ReadByte=1;
    unalligned:=config.ReadByte=1;
    noloop:=config.ReadByte=1;
    muststartwithbase:=config.ReadByte=1;
    LimitToMaxOffsetsPerNode:=config.ReadByte=1;
    onlyOneStaticInPath:=config.ReadByte=1;
    instantrescan:=config.ReadByte=1;
    mustEndWithSpecificOffset:=config.ReadByte=1;

    maxoffsetspernode:=config.ReadDWord;
    basestart:=config.ReadQWord;
    basestop:=config.ReadQWord;

    setlength(mustendwithoffsetlist, config.ReadDWord);
    config.ReadBuffer(mustendwithoffsetlist[0], sizeof(dword)*length(mustendwithoffsetlist));


    setlength(instantrescanentries, config.readdword);
    for i:=0 to length(instantrescanentries)-1 do
    begin
      instantrescanentries[i].filename:=config.ReadAnsiString;
      instantrescanentries[i].address:=config.ReadQWord;
    end;


    config.free;


    f:=tfrmresumePointerScan.create(self);

    if f.instantrescanfiles<>nil then
      for i:=0 to length(instantrescanentries)-1 do
        f.instantrescanfiles.AddObject(instantrescanentries[i].filename, tobject(instantrescanentries[i].address));

    if f.showmodal=mrOK then
    begin
      threadcount:=f.threadcount;

      for i:=0 to f.instantrescanfiles.Count-1 do
        instantrescanentries[i].filename:=f.instantrescanfiles[i];


      new1.click;

      //default scan
      staticscanner:=TPointerscanController.Create(true);

      btnStopScan.enabled:=true;
      btnStopScan.Caption:=rsStop;

      pnlData.Visible:=false;
      open1.Enabled:=false;
      new1.enabled:=false;
      rescanmemory1.Enabled:=false;

      cbType.Visible:=false;
      lvResults.Visible:=false;




      pnlProgress.Visible:=true;

      try
        staticscanner.OnStartScan:=PointerscanStart;
        staticscanner.OnScanDone:=PointerscanDone;
        staticscanner.threadcount:=threadcount;
        staticscanner.resumescan:=true;
        staticscanner.resumefilelist:=resumefilelist;
        staticscanner.maxlevel:=maxlevel;
        staticscanner.sz:=structsize;
        staticscanner.compressedptr:=compressedptr;
        staticscanner.unalligned:=unalligned;
        staticscanner.noLoop:=noloop;
        staticscanner.mustStartWithBase:=muststartwithbase;
        staticscanner.LimitToMaxOffsetsPerNode:=LimitToMaxOffsetsPerNode;
        staticscanner.onlyOneStaticInPath:=onlyOneStaticInPath;
        staticscanner.instantrescan:=true;
        staticscanner.mustEndWithSpecificOffset:=true;
        staticscanner.MaxOffsetsPerNode:=maxoffsetspernode;
        staticscanner.BaseStart:=basestart;
        staticscanner.BaseStart:=basestop;
        setlength(staticscanner.mustendwithoffsetlist, length(mustendwithoffsetlist));
        for i:=0 to length(mustendwithoffsetlist)-1 do
          staticscanner.mustendwithoffsetlist[i]:=mustendwithoffsetlist[i];


        setlength(staticscanner.instantrescanfiles, length(instantrescanentries));
        for i:=0 to length(staticscanner.instantrescanfiles)-1 do
        begin
          staticscanner.instantrescanfiles[i].address:=instantrescanentries[i].address;
          staticscanner.instantrescanfiles[i].filename:=instantrescanentries[i].filename;
          staticscanner.instantrescanfiles[i].plist:=nil;


          //create a new progressbar to show how long it'll take
          pb:=TProgressBar.Create(self);
          pb.parent:=pnlProgressBar;
          pb.position:=0;
          pb.Max:=100;

          if i=0 then
            pb.top:=ProgressBar1.Top+progressbar1.height
          else
            pb.Top:=staticscanner.instantrescanfiles[i-1].progressbar.Top+staticscanner.instantrescanfiles[i-1].progressbar.Height;

          pb.left:=ProgressBar1.left;
          pb.width:=ProgressBar1.width;
          pb.Anchors:=ProgressBar1.Anchors;

          lb:=TLabel.create(self);
          lb.caption:=extractfilename(instantrescanentries[i].filename);
          lb.Parent:=pnlProgressName;
          lb.top:=pb.Top+(pb.Height div 2)-(lb.height div 2);
          lb.hint:=ansitoutf8(instantrescanentries[i].filename);
          lb.showhint:=true;

          staticscanner.instantrescanfiles[i].progressbar:=pb;
          staticscanner.instantrescanfiles[i].progresslabel:=lb;


          pnlProgress.ClientHeight:=pb.Top+pb.height+1;

        end;

        staticscanner.UseLoadedPointermap:=true;
        staticscanner.LoadedPointermapFilename:=filename+'.resume.scandata';

        staticscanner.progressbar:=ProgressBar1;
        staticscanner.resumeptrfilename:=filename; //free by staticscanner
        staticscanner.filename:=filename;

        staticscanner.Start;

        pnlData.Visible:=true;

        Method3Fastspeedandaveragememoryusage1.Enabled:=false;

      except
        on e: exception do
        begin
          staticscanner.Free;
          staticscanner:=nil;
          MessageDlg(e.message, mtError, [mbok], 0);
        end;
      end;


    end;
  end
  else
    miResume.Visible:=false;

end;

procedure Tfrmpointerscanner.Method3Fastspeedandaveragememoryusage1Click(
  Sender: TObject);
var
  i: integer;
  floataccuracy: integer;
  floatsettings: TFormatSettings;
  filename: string;
  pb: TProgressBar;
  lb: TLabel;

  pfn: string;

  al: TStringlist;
begin
  FloatSettings:=DefaultFormatSettings;


  start:=now;
  if frmpointerscannersettings=nil then
    frmpointerscannersettings:=tfrmpointerscannersettings.create(application);

  if frmpointerscannersettings.Visible then exit; //already open, so no need to make again

  if frmpointerscannersettings.Showmodal=mrok then
  begin
    new1.click;

    if frmpointerscannersettings.rbGeneratePointermap.checked then //show a .scandata dialog instad of a .ptr
    begin
      if not savedialog2.execute then exit;
      filename:=savedialog2.filename;
    end
    else
    begin
      if not savedialog1.Execute then exit;
      filename:=savedialog1.filename;
    end;



        
    btnStopScan.enabled:=true;
    btnStopScan.Caption:=rsStop;

    pnlData.Visible:=false;
    open1.Enabled:=false;
    new1.enabled:=false;
    rescanmemory1.Enabled:=false;

    cbType.Visible:=false;
    lvResults.Visible:=false;




    //initialize array's




    //default scan
    staticscanner:=TPointerscanController.Create(true);


    pnlProgress.Visible:=true;

    try
      staticscanner.OnStartScan:=PointerscanStart;
      staticscanner.OnScanDone:=PointerscanDone;

      staticscanner.allowIncomingChildren:=frmpointerscannersettings.cbAllowRuntimeWorkers.Checked;
      staticscanner.listenport:=frmpointerscannersettings.distributedport;
      staticscanner.childpassword:=frmpointerscannersettings.edtDistributedPassword.text;




      staticscanner.initializer:=true;
      staticscanner.filename:=utf8toansi(fileName);
      staticscanner.generatePointermapOnly:=frmpointerscannersettings.rbGeneratePointermap.checked;
      if staticscanner.generatePointermapOnly then
      begin
        al:=tstringlist.create;
        MainForm.addresslist.getAddressList(al);
        al.SaveToFile(filename+'.addresslist');
        al.free;
      end;

      staticscanner.compressedptr:=frmpointerscannersettings.cbCompressedPointerscanFile.checked;

      staticscanner.noReadOnly:=frmpointerscannersettings.cbNoReadOnly.checked;
      staticscanner.mustBeClassPointers:=frmpointerscannersettings.cbClassPointersOnly.checked;
      staticscanner.acceptNonModuleClasses:=frmpointerscannersettings.cbAcceptNonModuleVtable.checked;


      staticscanner.useStacks:=frmpointerscannersettings.cbStaticStacks.checked;
      staticscanner.stacksAsStaticOnly:=frmPointerscannersettings.cbStackOnly.checked;
      staticscanner.threadstacks:=frmPointerscannersettings.threadstacks;
      staticscanner.stacksize:=frmPointerscannersettings.stacksize;

      staticscanner.UseLoadedPointermap:=frmpointerscannersettings.cbUseLoadedPointermap.Checked;
      staticscanner.LoadedPointermapFilename:=frmpointerscannersettings.odLoadPointermap.FileName;


      if staticscanner.UseLoadedPointermap then
        lblProgressbar1.caption:=extractfilename(staticscanner.LoadedPointermapFilename)
      else
        lblProgressbar1.caption:='Generating pointermap';

      staticscanner.startaddress:=frmpointerscannersettings.start;
      staticscanner.stopaddress:=frmpointerscannersettings.Stop;

      staticscanner.unalligned:=not frmpointerscannersettings.CbAlligned.checked;
      staticscanner.staticonly:=frmpointerscannersettings.cbStaticOnly.checked;
      staticscanner.noLoop:=frmpointerscannersettings.cbNoLoop.checked;
      staticscanner.LimitToMaxOffsetsPerNode:=frmpointerscannersettings.cbMaxOffsetsPerNode.Checked;
      staticscanner.maxOffsetsPerNode:=frmpointerscannersettings.maxOffsetsPerNode;


      staticscanner.automatic:=true;

      staticscanner.automaticaddress:=frmpointerscannersettings.automaticaddress;
      staticscanner.sz:=frmpointerscannersettings.structsize;
      staticscanner.maxlevel:=frmpointerscannersettings.maxlevel-1;


      staticscanner.progressbar:=progressbar1;
      staticscanner.threadcount:=frmpointerscannersettings.threadcount;
      staticscanner.scannerpriority:=frmpointerscannersettings.scannerpriority;
       {
      staticscanner.distributedScanning:=frmpointerscannersettings.cbDistributedScanning.checked;
      staticscanner.distributedport:=frmpointerscannersettings.distributedPort;

      staticscanner.broadcastThisScanner:=frmpointerscannersettings.cbBroadcast.checked;
      staticscanner.potentialWorkerList:=frmpointerscannersettings.resolvediplist;    }


      staticscanner.mustStartWithBase:=frmpointerscannersettings.cbMustStartWithBase.checked;
      staticscanner.BaseStart:=frmpointerscannersettings.baseStart;
      staticscanner.BaseStop:=frmpointerscannersettings.baseStop;

      staticscanner.mustEndWithSpecificOffset:=frmpointerscannersettings.cbMustEndWithSpecificOffset.checked;
      if staticscanner.mustEndWithSpecificOffset then
      begin
        setlength(staticscanner.mustendwithoffsetlist, frmpointerscannersettings.offsetlist.count);
        for i:=0 to frmpointerscannersettings.offsetlist.count-1 do
          staticscanner.mustendwithoffsetlist[i]:=TOffsetEntry(frmpointerscannersettings.offsetlist[i]).offset;
      end;

      staticscanner.instantrescan:=frmpointerscannersettings.cbCompareToOtherPointermaps.checked;
      setlength(staticscanner.instantrescanfiles,0);

      if staticscanner.instantrescan then
      begin
        for i:=0 to frmpointerscannersettings.pdatafilelist.count-1 do
        begin
          pfn:=Utf8ToAnsi(frmpointerscannersettings.pdatafilelist.filenames[i]);
          if pfn<>'' then
          begin
            setlength(staticscanner.instantrescanfiles,length(staticscanner.instantrescanfiles)+1);
            staticscanner.instantrescanfiles[length(staticscanner.instantrescanfiles)-1].filename:=pfn;
            staticscanner.instantrescanfiles[length(staticscanner.instantrescanfiles)-1].address:=frmpointerscannersettings.pdatafilelist.addresses[i];
            staticscanner.instantrescanfiles[length(staticscanner.instantrescanfiles)-1].plist:=nil;


            //create a new progressbar to show how long it'll take
            pb:=TProgressBar.Create(self);
            pb.parent:=pnlProgressBar;
            pb.position:=0;
            pb.Max:=100;

            if i=0 then
              pb.top:=ProgressBar1.Top+progressbar1.height
            else
              pb.Top:=staticscanner.instantrescanfiles[length(staticscanner.instantrescanfiles)-2].progressbar.Top+staticscanner.instantrescanfiles[length(staticscanner.instantrescanfiles)-2].progressbar.Height;

            pb.left:=ProgressBar1.left;
            pb.width:=ProgressBar1.width;
            pb.Anchors:=ProgressBar1.Anchors;

            lb:=TLabel.create(self);
            lb.caption:=extractfilename(pfn);
            lb.Parent:=pnlProgressName;
            lb.top:=pb.Top+(pb.Height div 2)-(lb.height div 2);
            lb.hint:=pfn;
            lb.showhint:=true;

            staticscanner.instantrescanfiles[length(staticscanner.instantrescanfiles)-1].progressbar:=pb;
            staticscanner.instantrescanfiles[length(staticscanner.instantrescanfiles)-1].progresslabel:=lb;


            pnlProgress.ClientHeight:=pb.Top+pb.height+1;
          end;
        end;

        if length(staticscanner.instantrescanfiles)=0 then
          staticscanner.instantrescan:=false; //no rescan at all
      end
      else
        pnlProgress.ClientHeight:=ProgressBar1.Top+progressbar1.height+1;

      staticscanner.onlyOneStaticInPath:=frmpointerscannersettings.cbOnlyOneStatic.checked;

      staticscanner.useHeapData:=frmpointerscannersettings.cbUseHeapData.Checked;
      staticscanner.useOnlyHeapData:=frmpointerscannersettings.cbHeapOnly.checked;


      if staticscanner.useHeapData then
        frmMemoryAllocHandler.memrecCS.enter; //stop adding entries to the list

      //check if the user choose to scan for addresses or for values
      staticscanner.findValueInsteadOfAddress:=frmpointerscannersettings.rbFindValue.checked;
      if staticscanner.findValueInsteadOfAddress then
      begin
        //if values, check what type of value
        floataccuracy:=pos(FloatSettings.DecimalSeparator,frmpointerscannersettings.cbAddress.Text);
        if floataccuracy>0 then
          floataccuracy:=length(frmpointerscannersettings.cbAddress.Text)-floataccuracy;

        case frmpointerscannersettings.cbValueType.ItemIndex of
          0:
          begin
            staticscanner.valuetype:=vtDword;
            val(frmpointerscannersettings.cbAddress.Text, staticscanner.valuescandword, i);
            if i>0 then raise exception.Create(Format(rsIsNotAValid4ByteValue, [frmpointerscannersettings.cbAddress.Text]));
          end;

          1:
          begin
            staticscanner.valuetype:=vtSingle;
            val(frmpointerscannersettings.cbAddress.Text, staticscanner.valuescansingle, i);
            if i>0 then raise exception.Create(Format(rsIsNotAValidFloatingPointValue, [frmpointerscannersettings.cbAddress.Text]));
            staticscanner.valuescansingleMax:=staticscanner.valuescansingle+(1/(power(10,floataccuracy)));
          end;

          2:
          begin
            staticscanner.valuetype:=vtDouble;
            val(frmpointerscannersettings.cbAddress.Text, staticscanner.valuescandouble, i);
            if i>0 then raise exception.Create(Format(rsIsNotAValidDoubleValue, [frmpointerscannersettings.cbAddress.Text]));
            staticscanner.valuescandoubleMax:=staticscanner.valuescandouble+(1/(power(10,floataccuracy)));            
          end;
        end;
      end;


      progressbar1.Max:=staticscanner.stopaddress-staticscanner.startaddress;

      open1.Enabled:=false;
      staticscanner.start;


      pnlData.Visible:=true;

      Method3Fastspeedandaveragememoryusage1.Enabled:=false;

      if frmpointerscannersettings.cbConnectToNode.checked then
      begin
        for i:=0 to frmpointerscannersettings.iplist.count-1 do
        begin
          if frmpointerscannersettings.iplist[i].host<>'' then
            staticscanner.BecomeParentOfNode(frmpointerscannersettings.iplist[i].host, strtoint(frmpointerscannersettings.iplist[i].port), frmpointerscannersettings.iplist[i].password, frmpointerscannersettings.iplist[i].stable);
        end;
      end;
    except
      on e: exception do
      begin
        staticscanner.Free;
        staticscanner:=nil;
        MessageDlg(e.message, mtError, [mbok], 0);
      end;
    end;

  end;
end;

procedure Tfrmpointerscanner.lvResultsResize(Sender: TObject);
var i,l: integer;
begin
  if lvResults.Columns.Count>0 then
  begin
    l:=0;
    for i:=0 to lvResults.columns.count-2 do
      inc(l,lvResults.Columns[i].Width);

    l:=lvResults.ClientWidth-l;
    l:=max(120,l);
    lvResults.Columns[lvResults.columns.count-1].Width:=l;
  end;
end;

procedure Tfrmpointerscanner.MenuItem3Click(Sender: TObject);
begin
  //start a listener for pointerscan related signals
{  if distributedworkfolder='' then
    miSetWorkFolder.Click;

  if distributedworkfolder='' then exit;

  if PointerscanListener<>nil then
  begin
    if PointerscanListener.done then
    begin
      PointerscanListener.terminate;
      freeandnil(pointerscanlistener);
    end;
  end;

  if PointerscanListener=nil then
    PointerscanListener:=TPointerscanListener.create(self, false);
                                                    }


end;

procedure Tfrmpointerscanner.miCreatePSNnodeClick(Sender: TObject);
var f: TfrmSetupPSNNode;
begin
  f:=TfrmSetupPSNNode.Create(self);
  if f.showmodal=mrok then
  begin
    if staticscanner<>nil then
      freeandnil(staticscanner);

    cbType.Visible:=false;
    lvResults.Visible:=false;


    //create a new pointerscanner that acts as a node
    staticscanner:=TPointerscanController.create(true);
    staticscanner.initializer:=false;
    staticscanner.threadcount:=f.threadcount;
    staticscanner.priority:=f.priority;
    staticscanner.publicname:=f.edtPublicname.text;
    staticscanner.listenport:=f.listenport;

    staticscanner.allowIncomingParent:=f.cbAllowParents.checked;
    staticscanner.allowIncomingChildren:=f.cbAllowChildren.checked;
    staticscanner.parentpassword:=f.edtParentPassword.text;
    staticscanner.childpassword:=f.edtChildPassword.text;
    staticscanner.autoTrustIncomingChildren:=f.cbAutoTrustChildren.checked;

    staticscanner.maxResultsToFind:=f.maxresultstofind;
    staticscanner.maxTimeToScan:=f.maxtimetoscan;

    if f.cbConnectToOtherNode.checked then
    begin
      if f.rbConnectAsParent.checked then
        staticscanner.BecomeParentOfNode(f.edtConnectIP.text, f.connectport, f.edtConnectPassword.text)
      else
        staticscanner.BecomeChildOfNode(f.edtConnectIP.text, f.connectport, f.edtConnectPassword.text);
    end;

    staticscanner.allowTempFiles:=f.cbAllowTempFiles.checked;


    staticscanner.Start;

    pnlData.Visible:=true;
    timer2.enabled:=true;
  end;

  f.free;
end;

procedure Tfrmpointerscanner.miMergePointerscanResultsClick(Sender: TObject);
var
  i,j: integer;
  psr: TPointerscanresultReader=nil;

  pfiles: Tstringlist=nil;
  newfiles: Tstringlist=nil;
  destinationpath: string;
  basename: string;

  fname: string;
  startid: integer;
  id: integer;
  s: string;

  allworkerids: array of integer;

  resultfile: TMemorystream=nil;
begin

  setlength(allworkerids,0);

  if Pointerscanresults<>nil then
  begin
    destinationpath:=extractfilepath(Pointerscanresults.filename);
    basename:=ExtractFileName(Pointerscanresults.filename);

    psr:=nil;
    pfiles:=tstringlist.create;
    newfiles:=tstringlist.create;
    Pointerscanresults.getFileList(newfiles); //add the original files (note: These contain a full path)

    //strip the local path if it's possible (there can be results of a previous link merge)
    for i:=0 to newfiles.count-1 do
    begin
      s:=StringReplace(newfiles[i], destinationpath, '', [rfIgnoreCase]);
      if pos(PathDelim, s)=0 then
        newfiles[i]:=s;
    end;


    startid:=1;
    //get a basic start id. (Still first if the file exists)
    for i:=0 to newfiles.count-1 do
    begin
      s:=ExtractFileExt(pfiles[i]);
      s:=copy(s, 2, length(s)-1);

      if TryStrToInt(s, id) then
        startid:=max(id, startid);

    end;

    setlength(allworkerids, Pointerscanresults.mergedresultcount);
    for i:=0 to Pointerscanresults.mergedresultcount-1 do
      allworkerids[i]:=Pointerscanresults.mergedresults[i];


    try
      if odMerge.execute then
      begin
        frmMergePointerscanResultSettings:=TfrmMergePointerscanResultSettings.create(self);
        if frmMergePointerscanResultSettings.showmodal=mrok then
        begin
          //generate the new .ptr file
          resultfile:=tmemorystream.create;
          Pointerscanresults.saveModulelistToResults(resultfile);

          resultfile.WriteDWord(Pointerscanresults.offsetCount);  //offsetcount



          for i:=0 to odmerge.Files.count-1 do
          begin
            psr:=TPointerscanresultReader.create(utf8toansi(odmerge.files[i]), Pointerscanresults);

            if psr.offsetCount<>pointerscanresults.offsetCount then
              raise exception.create(odmerge.files[i] +' is incompatible with the base pointerscan result');

            pfiles.clear;
            psr.getfilelist(pfiles);

            for j:=0 to psr.mergedresultcount-1 do
            begin
              setlength(allworkerids, length(allworkerids)+1);
              allworkerids[length(allworkerids)-1]:=psr.mergedresults[j];
            end;

            freeandnil(psr);

            //copy (/move?) the files in pfiles to the path of pointerscanresults and give them unique names




            for j:=0 to pfiles.count-1 do
            begin
              if frmMergePointerscanResultSettings.rgGroupMethod.ItemIndex in [0,1] then
              begin
                //copy/move

                //find a filename not used yet
                repeat
                  fname:=destinationpath+basename+'.'+inttostr(startid);
                  inc(startid);
                until not FileExists(fname);

                fname:=destinationpath+basename+'.'+inttostr(startid);

                if frmMergePointerscanResultSettings.rgGroupMethod.ItemIndex=0 then //copy
                begin
                  if CopyFile(pchar(pfiles[j]), pchar(fname), true )=false then
                    raise exception.create('Failure copying '+pfiles[j]+' to '+fname);
                end
                else
                begin
                  if MoveFile(pchar(pfiles[j]), pchar(fname) )=false then
                    raise exception.create('Failure moving '+pfiles[j]+' to '+fname);
                end;



                fname:=extractfilename(fname);

              end
              else
              begin
                //link
                fname:=pfiles[j];
              end;
              newfiles.add(fname);
            end;




          end;

          resultfile.WriteDWord(newfiles.Count); //number of ptr files

          //add the files to resultfile
          for i:=0 to newfiles.count-1 do
          begin
            resultfile.WriteDWord(length(newfiles[i]));
            resultfile.WriteBuffer(newfiles[i][1], length(newfiles[i]));
          end;

          resultfile.WriteDWord(pointerscanresults.externalScanners);
          resultfile.WriteDWord(pointerscanresults.generatedByWorkerID);

          resultfile.WriteDWord(length(allworkerids));
          for i:=0 to length(allworkerids)-1 do
            resultfile.WriteDWord(allworkerids[i]);

          resultfile.WriteDWord(ifthen(pointerscanresults.compressedptr,1,0));
          resultfile.WriteDWord(ifthen(pointerscanresults.aligned,1,0));
          resultfile.WriteDWord(pointerscanresults.MaxBitCountModuleIndex);
          resultfile.WriteDWord(pointerscanresults.MaxBitCountLevel);
          resultfile.WriteDWord(pointerscanresults.MaxBitCountOffset);

          resultfile.WriteDWord(pointerscanresults.EndsWithOffsetListCount);
          for i:=0 to pointerscanresults.EndsWithOffsetListCount-1 do
            resultfile.WriteDword(pointerscanresults.EndsWithOffsetList[i]);


          //all done, and no crashes
          New1.Click; //close the current pointerfile and cleanup everything attached



          resultfile.SaveToFile(destinationpath+basename);


          //and reopen it
          OpenPointerfile(destinationpath+basename);

        end;



      end;
    finally
      if psr<>nil then
        psr.free;

      if pfiles<>nil then
        pfiles.free;

      if newfiles<>nil then
        newfiles.free;

      if resultfile<>nil then
        freeandnil(resultfile);

      if frmMergePointerscanResultSettings<>nil then
        freeandnil(frmMergePointerscanResultSettings);
    end;
  end;
end;



procedure Tfrmpointerscanner.miSetWorkFolderClick(Sender: TObject);
var reg: Tregistry;
begin
  {
  if SelectDirectoryDialog1.Execute then
  begin
    distributedworkfolder:=IncludeTrailingPathDelimiter(SelectDirectoryDialog1.filename);

    reg:=tregistry.create;
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('\Software\Cheat Engine',true) then
    begin
      distributedworkfolder:=IncludeTrailingPathDelimiter(SelectDirectoryDialog1.filename);
      reg.WriteString('PointerScanWorkFolder', distributedworkfolder);
    end;
    reg.free;

  end; }
end;





procedure Tfrmpointerscanner.FormDestroy(Sender: TObject);
var x: array of integer;
begin
  setlength(x,1);
  x[0]:=cbtype.itemindex;
  SaveFormPosition(self, x);
end;

procedure Tfrmpointerscanner.btnStopRescanLoopClick(Sender: TObject);
begin
  btnStopRescanLoop.visible:=false;
  rescanpointerform.cbRepeat.checked:=false;
end;

procedure Tfrmpointerscanner.btnConnectClick(Sender: TObject);
begin
  if staticscanner<>nil then
    staticscanner.BecomeParentOfNode(edtIP.text, strtoint(edtPort.text), edtPassword.text, cbTrusted.checked);
end;

procedure Tfrmpointerscanner.btnIncreaseThreadCountClick(Sender: TObject);
begin
    if staticscanner<>nil then
    begin
      staticscanner.addWorkerThread;
      inc(staticscanner.threadcount);
    end;
end;

procedure Tfrmpointerscanner.btnDecreaseThreadCountClick(Sender: TObject);
begin
  if staticscanner<>nil then
  begin
    staticscanner.RemoveWorkerThread;
    if staticscanner.threadcount>0 then
      dec(staticscanner.threadcount);
  end;
end;

procedure Tfrmpointerscanner.cbPriorityChange(Sender: TObject);
var scannerpriority: TThreadPriority;
begin
  if staticscanner<>nil then
  begin
    case cbPriority.itemindex of
      0: scannerpriority:=tpIdle;
      1: scannerpriority:=tpLowest;
      2: scannerpriority:=tpLower;
      3: scannerpriority:=tpNormal;
      4: scannerpriority:=tpHigher;
      5: scannerpriority:=tpHighest;
      6: scannerpriority:=tpTimeCritical;
    end;

    staticscanner.changeWorkerPriority(scannerpriority);
  end;
end;


procedure Tfrmpointerscanner.FormResize(Sender: TObject);
begin
  btnStopRescanLoop.Left:=(clientwidth div 2) - (btnStopRescanLoop.Width div 2);
end;

procedure Tfrmpointerscanner.lvResultsColumnClick(Sender: TObject; Column: TListColumn);
//Using dark byte's super secret "Screw this, I'll just split it into chunks" algorithm
var
  c: integer;
  frmSortPointerlist: TfrmSortPointerlist;
  tempname: string;

  oldname: string;
  oldlist: Tstringlist;
  tempfilelist: tstringlist;

  newname: string;
  i: integer;
  s: string;
begin
  c:=column.index;
  if c=lvResults.ColumnCount-1 then exit; //raise exception.create('The result/value list is unsortable');
  if Pointerscanresults.count<=1 then exit; //don't even bother


  frmSortPointerlist:=TfrmSortPointerlist.Create(self);
  oldname:=Pointerscanresults.filename;
  oldlist:=tstringlist.create;
  tempfilelist:=tstringlist.create;
  Pointerscanresults.getFileList(oldlist);

  if frmSortPointerlist.dowork(column.index, oldname , tempname, tempfilelist) then
  begin
    //sorting done

    new1.Click;

    //delete the old pointerfiles
    for i:=0 to oldlist.Count-1 do
    begin
      s:=oldlist[i];
      DeleteFile(s);
    end;

    deletefile(oldname);
    renamefile(tempname, oldname);

    for i:=0 to tempfilelist.count-1 do
    begin
      newname:=StringReplace(tempfilelist[i], tempname, oldname,[]);
      DeleteFile(newname);

      RenameFile(tempfilelist[i], newname);
    end;


    OpenPointerfile(oldname);
  end;

  oldlist.free;

  frmSortPointerlist.free;
end;

procedure Tfrmpointerscanner.Timer2Timer(Sender: TObject);
var i,j: integer;
    s: string;

    tn,tn2: TTreenode;

    x: qword;
    tpe: qword;

    tpf: qword;

    totalTimeWriting: qword;
    totalTime: qword;

    percentageSpentWriting: single;

    connectinglist: TConnectEntryArray;
    connectionlist: TConnectionEntryArray;
    parentdata: TPublicParentData;

    info: tstringlist;

    statistics: record
      totalTimeScanning: qword;
      totalPathsEvaluated: QWord;
      pathspersecond: double;
      pointersinmap: qword;
      pathqueuesize: integer;
      pathqueueoverflow: dword;
      resultsfound: qword;
      timeSpentWriting: qword;
      percentageTimeSpentWriting: single;
      minpath, maxpath: TDynDwordArray;
      minpaths, maxpaths: string;
    end;

    scanners: array of record
      status: string;
    end;

    sl: TStringList;
begin
  if lvResults.Visible then
    lvResults.repaint;

  try
    //collect data and then update the treeview

    statistics.totalTimeScanning:=0;
    statistics.totalPathsEvaluated:=0;
    statistics.pathspersecond:=0;

    if staticscanner<>nil then
    begin
      if staticscanner.starttime>0 then
        statistics.totalTimeScanning:=GetTickCount64-staticscanner.starttime;

      statistics.totalPathsEvaluated:=staticscanner.totalpathsevaluated;
      if statistics.totalTimeScanning>0 then
        statistics.pathspersecond:=(statistics.totalPathsEvaluated / statistics.totalTimeScanning)*1000; //paths / second

      statistics.pointersinmap:=staticscanner.getPointerlistHandlerCount;
      statistics.pathqueuesize:=staticscanner.pathqueuelength;
      statistics.pathqueueoverflow:=length(staticscanner.overflowqueue);
      statistics.resultsfound:=staticscanner.getTotalResultsFound;
      statistics.timeSpentWriting:=ceil((staticscanner.getTotalTimeWriting / 1000) / staticscanner.getActualThreadCount); //time spend writing in seconds
      statistics.percentageTimeSpentWriting:=(statistics.timeSpentWriting / (statistics.totalTimeScanning / 1000)) * 100; //time spend writing in seconds / total time div

      staticscanner.getMinAndMaxPath(statistics.minpath, statistics.maxpath);
      statistics.minpaths:='';
      for i:=0 to length(statistics.minpath)-1 do
      begin
        statistics.minpaths:=statistics.minpaths+inttohex(statistics.minpath[i],1);
        if i<length(statistics.minpath)-1 then
          statistics.minpaths:=statistics.minpaths+' - ';
      end;

      statistics.maxpaths:='';
      for i:=0 to length(statistics.maxpath)-1 do
      begin
        statistics.maxpaths:=statistics.maxpaths+inttohex(statistics.maxpath[i],1);
        if i<length(statistics.maxpath)-1 then
          statistics.maxpaths:=statistics.maxpaths+' - ';
      end;


      sl:=TStringList.create;
      staticscanner.getThreadStatuses(sl);
      setlength(scanners, sl.Count);
      for i:=0 to sl.count-1 do
        scanners[i].status:=sl[i];

      sl.free;
    end;

    //setup the basics if it hasn't been setup yet
    with infonodes.statistics.stats do
    begin

      if infonodes.statistics.node=nil then
      begin
        //create the statistics node and the childnodes
        infonodes.statistics.node:=tvInfo.Items.Add(nil, 'Statistics');

        pointersInMap:=tvInfo.Items.AddChild(infonodes.statistics.node,'');
        totalTimeScanning:=tvInfo.Items.AddChild(infonodes.statistics.node,'');
        totalPathsEvaluated:=tvInfo.Items.AddChild(infonodes.statistics.node,'');
        calculatedPathsPerSecond:=tvInfo.Items.AddChild(infonodes.statistics.node,'');

        pathQueue:=tvInfo.Items.AddChild(infonodes.statistics.node,'');
        resultsFound:=tvInfo.Items.AddChild(infonodes.statistics.node,'');
        timeSpentWriting:=tvInfo.Items.AddChild(infonodes.statistics.node,'');

        minpath:=tvInfo.Items.AddChild(infonodes.statistics.node,'');


        //maxpath:=tvInfo.Items.AddChild(infonodes.statistics.node,'');

        infonodes.statistics.node.Expand(true);
      end;

      pointersInMap.Text:='Unique pointervalues in target:'+IntToStr(statistics.pointersinmap);
      totalTimeScanning.Text:='Scan duration: '+TimeToStr(TimeStampToDateTime(MSecsToTimeStamp(statistics.totalTimeScanning)));
      totalPathsEvaluated.Text:='Paths evaluated: '+IntToStr(statistics.totalPathsEvaluated);
      calculatedPathsPerSecond.Text:=format('Paths / seconds: (%.0n / s)', [statistics.pathspersecond]);
      pathQueue.Text:='Static queue size: '+inttostr(statistics.pathqueuesize)+' Dynamic queue size:'+inttostr(statistics.pathqueueoverflow);
      resultsFound.Text:='Results found: '+inttostr(statistics.resultsfound);
      timeSpentWriting.Text:='Time spent writing: '+inttostr(statistics.timeSpentWriting)+format(' (%.2f %%)', [statistics.percentageTimeSpentWriting]) ;

      minpath.text:='Lowest known path:'+statistics.minpaths;
     // maxpath.text:='Highest known path:'+statistics.maxpaths;



    end;


    if infonodes.localworkers.node=nil then
      infonodes.localworkers.node:=tvInfo.Items.Add(nil, 'Threads');

    for i:=length(infonodes.localworkers.workernodes)-1 downto length(scanners) do //delete the nodes that are too many
      if infonodes.localworkers.workernodes[i].node<>nil then
        infonodes.localworkers.workernodes[i].node.free;

    setlength(infonodes.localworkers.workernodes, length(scanners));

    for i:=0 to length(infonodes.localworkers.workernodes)-1 do
    begin
      if infonodes.localworkers.workernodes[i].node=nil then
        infonodes.localworkers.workernodes[i].node:=tvInfo.Items.AddChild(infonodes.localworkers.node, '');

      infonodes.localworkers.workernodes[i].node.Text:=scanners[i].status;
    end;

    if staticscanner<>nil then
    begin

      if staticscanner.hasNetworkResponsibility then
      begin
        //display network info
        if infonodes.network.node=nil then
          infonodes.network.node:=tvInfo.Items.Add(nil,'Network');

        staticscanner.getConnectingList(connectinglist);

        for i:=length(infonodes.network.connectingToNodes)-1 downto length(connectinglist) do
            infonodes.network.connectingToNodes[i].data.Free;

        setlength(infonodes.network.connectingToNodes, length(connectinglist));
        //connecting to:
        if infonodes.network.connectingto=nil then
          infonodes.network.connectingto:=tvInfo.Items.AddChild(infonodes.network.node,'Connecting to:');




        for i:=0 to length(connectinglist)-1 do
        begin
          if infonodes.network.connectingToNodes[i].data=nil then //create a new one
            infonodes.network.connectingToNodes[i].data:=tvinfo.Items.AddChild(infonodes.network.connectingto,'');

          s:=connectinglist[i].ip+':'+inttostr(connectinglist[i].port);
          if connectinglist[i].becomeparent=false then
            s:=s+BoolToStr(connectinglist[i].trusted, ' (Trusted)','');

          infonodes.network.connectingToNodes[i].data.Text:=s;
        end;


        staticscanner.getConnectionList(connectionlist);

        for i:=length(infonodes.network.connectedToNodes)-1 downto length(connectionlist) do
          infonodes.network.connectedToNodes[i].node.free;

        setlength(infonodes.network.connectedToNodes, length(connectionlist));


        //connected to:

        if Staticscanner.initializer=false then
        begin
          staticscanner.getParentData(parentdata);

          if infonodes.network.parent=nil then
            infonodes.network.parent:=tvInfo.Items.AddChild(infonodes.network.node,'Parent: ');

          if parentdata.connected then
          begin
            infonodes.network.parent.Text:='Parent: '+parentdata.name+'('+parentdata.ip+':'+inttostr(parentdata.port)+')';

            if infonodes.network.parentnodes.lastUpdateSent=nil then
              infonodes.network.parentnodes.lastUpdateSent:=tvInfo.Items.AddChild(infonodes.network.parent,'');

            if staticscanner.downloadingscandata then
            begin
              infonodes.network.parentnodes.lastUpdateSent.Text:=format('Downloading scandata: %.1f%% (%dKB/%dKB : %d KB/sec)', [staticscanner.downloadingscandata_received/staticscanner.downloadingscandata_total*100, staticscanner.downloadingscandata_received div 1024, staticscanner.downloadingscandata_total div 1024, ceil(((staticscanner.downloadingscandata_received / 1024)/((GetTickCount64-staticscanner.downloadingscandata_starttime)/1000)) )]);
            end
            else
            begin
              infonodes.network.parentnodes.lastUpdateSent.Text:='Last update: '+inttostr((GetTickCount64-parentdata.lastupdatesent) div 1000)+' seconds ago';
            end;




          end
          else
          begin
            //mark that it has no parent (yet/anymore)
            if parentdata.waitingforreconnect then
              infonodes.network.parent.Text:='Parent: <disconnected> (Waiting for reconnect)'
            else
              infonodes.network.parent.Text:='Parent: <none>';

            //if there are nodes, delete them
            if infonodes.network.parentnodes.lastUpdateSent<>nil then
              freeandnil(infonodes.network.parentnodes.lastUpdateSent);
          end;
        end;

        if infonodes.network.connectedTo=nil then
          infonodes.network.connectedTo:=tvInfo.Items.AddChild(infonodes.network.node,'Children:');

        for i:=0 to length(connectionlist)-1 do
        begin
          if infonodes.network.connectedToNodes[i].node=nil then //create it
          begin
            infonodes.network.connectedToNodes[i].node:=tvInfo.Items.AddChild(infonodes.network.connectedTo, s);
            tn:=infonodes.network.connectedToNodes[i].node;

            with infonodes.network.connectedToNodes[i].data do
            begin
              trusted:=tvInfo.Items.AddChild(tn, '');
              totalthreadcount:=tvInfo.Items.AddChild(tn, '');
              resultsfound:=tvInfo.Items.AddChild(tn, '');
              pathqueuesize:=tvInfo.Items.AddChild(tn, '');
              totalpathquesize:=tvInfo.Items.AddChild(tn, '');
              totalpathsEvaluated:=tvInfo.Items.AddChild(tn, '');
             // pathspersecond:=tvInfo.Items.AddChild(tn, '');
            end;
          end;

          s:=connectionlist[i].ip+':'+inttostr(connectionlist[i].port);
          if connectionlist[i].disconnected then
            s:=s+' (Disconnected)'
          else
          if connectionlist[i].queued then
            s:=s+' (Queued: '+inttostr(connectionlist[i].queuepos)+'/'+inttostr(connectionlist[i].queuesize)+')'
          else
          begin
            if connectionlist[i].isidle=false then
              s:=s+' (Active)'
            else
              s:=s+' (Idle)';

            if connectionlist[i].uploadingscandata then
              s:=s+format(' (Uploading scandata: %.1f%% (%dKB/%dKB : %d KB/sec)', [connectionlist[i].ScanDataSent/connectionlist[i].ScanDataTotalSize*100, connectionlist[i].ScanDataSent div 1024, connectionlist[i].ScanDataTotalSize div 1024, ceil(((connectionlist[i].ScanDataSent / 1024)/((GetTickCount64-connectionlist[i].ScanDataStartTime)/1000)) )]);

            if connectionlist[i].downloadingResuls then
              s:=s+' (Downloading and handling results)';
          end;

          infonodes.network.connectedToNodes[i].node.Text:=s;

          with infonodes.network.connectedToNodes[i].data do
          begin
            trusted.text:='Trusted: '+BoolToStr(connectionlist[i].trustedconnection, 'True', 'False');
            totalthreadcount.text:='Total threadcount: '+IntToStr(connectionlist[i].threadcount);
            resultsfound.text:='Results found: '+IntToStr(connectionlist[i].resultsfound);
            pathqueuesize.text:='Queuesize: '+inttostr(connectionlist[i].pathquesize);
            totalpathquesize.text:='Total Queuesize: '+inttostr(connectionlist[i].totalpathqueuesize);
            totalpathsEvaluated.text:='Paths evaluated: '+inttostr(connectionlist[i].pathsevaluated);
            //pathspersecond.text:='Paths/second: '+inttostr(connectionlist[i].pathspersecond);
          end;
        end;


      end;


    end;

  except
   // showmessage('exception happened');
    on e:exception do
    begin
      OutputDebugString('pscangui update timer error: '+e.message);
    end;
  end;


      (*

  info:=tstringlist.create;
  try

    if staticscanner<>nil then
    begin
      if staticscanner<>nil then
      begin
        i:=staticscanner.pathqueuelength;
        j:=length(staticscanner.overflowqueue);
      end
      else
        i:=0;

      s:=rsAddressSpecifiersFoundInTheWholeProcess+':'+inttostr(staticscanner.getPointerlistHandlerCount)+'  (pathqueue: '+inttostr(i)+')';

      if j>0 then
        s:=s+'  (overflow pathqueue: '+inttostr(j)+')';



      info.Add(s);

    end;

    if staticscanner<>nil then
    try
      if staticscanner.isdone then
      begin
        if tvInfo.Items.Count>0 then
          tvInfo.Items.Clear;

        exit;
      end;


      begin
        tpf:=0;
        tpe:=0;
        totalTimeWriting:=0;
        for i:=0 to length(Staticscanner.localscanners)-1 do
        begin
          tpf:=tpf+Staticscanner.localscanners[i].pointersfound;
          tpe:=tpe+Staticscanner.localscanners[i].pathsEvaluated;
          totalTimeWriting:=totalTimeWriting+Staticscanner.localscanners[i].timespentwriting;

          if staticscanner.localscanners[i].isFlushing then
            inc(totalTimeWriting, GetTickCount-staticscanner.localscanners[i].currentwritestart);
        end;

        totalTime:=(gettickcount-starttime)*length(Staticscanner.localscanners);
        percentageSpentWriting:=totalTimeWriting/totalTime*100;

        s:=format(rsPointerPathsFound+': %d', [tpf]);
          end;
          {
        if staticscanner.distributedScanning and (staticscanner.distributedWorker=false) then
        begin
          x:=scount+ staticscanner.workersPointersfoundTotal;

          s:=s+' ('+inttostr(x)+')';
        end;   }
        info.add(s);


  {$ifdef benchmarkps}
        //totalpathsevaluated:=tpe;

        if (starttime=0) and (tpe<>0) then
        begin
          startcount:=tpe;  //get the count from this point on
          starttime:=gettickcount;
        end;

        s:=format(rsEvaluated+': %d '+rsTime+': %d  (%.0n / s)', [tpe-startcount, ((gettickcount-starttime) div 1000), ((tpe-startcount)/(gettickcount-starttime))*1000]);

        {
        if staticscanner.distributedScanning and (staticscanner.distributedWorker=false) then
        begin
          x:=trunc(((totalpathsevaluated-startcount)/(gettickcount-starttime))*1000)+staticscanner.workersPathPerSecondTotal;
          s:=s+' (Total: '+inttostr(x)+' / s)';
        end;    }

        s:=s+format(' Writing: %.2f %%',[percentageSpentWriting]);



        if staticscanner.outofdiskspace then
        begin
          {
          label5.Font.Color:=clRed;
          label5.caption:=rsOUTOFDISKSPACECleanUpTheDiskOrStop;}
          info.add(rsOUTOFDISKSPACECleanUpTheDiskOrStop);
          //lblPscanInfo.font.color:=clRed;
        end
        else
        begin
{          label5.Font.Color:=graphics.clDefault;
          label5.caption:=s;
          label5.Width:=label5.Canvas.TextWidth(label5.caption);}
          info.add(s);
          //lblPscanInfo.Font.Color:=graphics.clDefault;
        end;


  {$endif}


        if tvInfo.Items.Count<length(staticscanner.localscanners) then
        begin
          //add them

          for i:=0 to length(staticscanner.localscanners)-1 do
          begin
            tn:=tvInfo.Items.Add(nil, rsThread+' '+inttostr(i+1));
            tvInfo.Items.AddChild(tn, rsCurrentLevel+':0');
            tvInfo.Items.AddChild(tn, rsLookingFor+' :0-0');
          end;
        end;

        tn:=tvInfo.Items.GetFirstNode;
        i:=0;
        while tn<>nil do
        begin
          if tn.Data<>nil then break; //worker instead of thread

          if i<length(staticscanner.localscanners) then
          begin
            if staticscanner.localscanners[i].isdone then
            begin
              tn.Text:=rsThread+' '+inttostr(i+1)+' ('+rsSleeping+')';
              tn2:=tn.getFirstChild;
              tn2.text:=rsSleeping;
              tn2:=tn2.getNextSibling;
              tn2.text:=rsSleeping;
            end
            else
            begin
              if staticscanner.localscanners[i].isFlushing then
                tn.text:=rsThread+' '+inttostr(i+1)+' ('+rsWritingToDisk+')'
              else
                tn.text:=rsThread+' '+inttostr(i+1)+' ('+rsActive+')';


              if staticscanner.localscanners[i].hasTerminated then
                tn.text:=tn.text+' (TERMINATED)';

              tn2:=tn.getFirstChild;

              begin
                s:='';
                for j:=0 to staticscanner.localscanners[i].currentlevel-1 do
                  s:=s+' '+inttohex(staticscanner.localscanners[i].tempresults[j],8);


                tn2.text:=rsCurrentLevel+':'+inttostr(staticscanner.localscanners[i].currentlevel)+' ('+s+')';
                tn2:=tn2.getNextSibling;
                tn2.text:=rsLookingFor+' :'+inttohex(staticscanner.localscanners[i].lookingformin, 8)+'-'+inttohex(staticscanner.localscanners[i].lookingformax, 8);
              end;
            end;


          end;
          tn:=tn.getNextSibling;
          inc(i);
        end;

        staticscanner.getConnectingList(connectionlist);
        if length(connectionlist)>0 then
        begin
          if infonodes.network.connectingto=nil then
            infonodes.network.connectingto:=tvinfo.Items.add(nil,'Connecting to');

          for i:=length(infonodes.network.connectedToNodes)-1 downto length(connectionlist) do
              infonodes.network.connectedToNodes[i].node.Free; //also frees the data under it

          setlength(infonodes.network.connectedToNodes, length(connectionlist));

          for i:=0 to length(connectionlist)-1 do
          begin
            if infonodes.network.connectedToNodes[i].node=nil then //create a new one
              infonodes.network.connectedToNodes[i].node:=tvinfo.Items.add(infonodes.network.connectingto,'');

            s:=connectionlist[i].ip+':'+inttostr(connectionlist[i].port);
            if connectionlist[i].becomeparent=false then
              s:=s+BoolToStr(connectionlist[i].trusted, ' (Trusted)','');

            infonodes.network.connectedToNodes[i].node.Text:=s;

          end;

        end;
         {
        if staticscanner.distributedScanning and (staticscanner.distributedWorker=false) then
        begin

          if length(Staticscanner.workers)>0 then
          begin
            //add/update workers
            tn:=tvInfo.Items.GetFirstNode; //find first worker entry
            while tn<>nil do
            begin
              if tn.Data<>nil then break;
              tn:=tn.GetNextSibling;
            end;


            for i:=0 to length(Staticscanner.workers)-1 do
            begin
              if tn=nil then //create this one
              begin
                tn:=tvInfo.Items.AddChild(nil, 'Worker :'+inttostr(i));
                tn.Data:=pointer(i+1);
              end;

              s:='';
              if Staticscanner.workers[i].s=-1 then
                s:=s+' (Disconnected)';

              if Staticscanner.workers[i].alldone then
                s:=s+' (Sleeping)';

              tn.text:='Worker '+inttostr(i)+': Found='+inttostr(Staticscanner.workers[i].pointersfound)+' (Threads:'+inttostr(Staticscanner.workers[i].threadcount)+')'+s;
              tn:=tn.GetNextSibling;
            end;

          end;

        end;
        }
      end;
    except

    end;

  finally
    //lblPscanInfo.Caption:=info.Text;

    info.free;
  end;   *)
end;

procedure Tfrmpointerscanner.OpenPointerfile(filename: string);
var
  i: integer;

  col_baseaddress:TListColumn;
  col_pointsto: TListColumn;
  col_offsets: Array of TListColumn;
begin
  new1.Click;

  Pointerscanresults:=TPointerscanresultReader.create(filename);

  lvResults.Items.BeginUpdate;
  lvResults.Columns.BeginUpdate;
  lvResults.Items.Count:=0;
  lvResults.Columns.Clear;

  col_baseaddress:=lvResults.Columns.Add;
  col_baseaddress.Caption:=rsBaseAddress;
  col_baseaddress.Width:=150;
  col_baseaddress.MinWidth:=20;

  setlength(col_offsets, Pointerscanresults.offsetCount);
  for i:=0 to Pointerscanresults.offsetCount-1 do
  begin
    col_offsets[i]:=lvResults.Columns.Add;
    col_offsets[i].Caption:=rsOffset+' '+inttostr(i);
    col_offsets[i].Width:=80;
    col_offsets[i].MinWidth:=10;
  end;

  col_pointsto:=lvResults.Columns.Add;
  col_pointsto.Caption:=rsPointsTo+':';
  col_pointsto.Width:=120;
  col_pointsto.MinWidth:=10;
  col_pointsto.AutoSize:=true;




  panel1.Caption:=rsPointercount+':'+inttostr(Pointerscanresults.count);


  if (Pointerscanresults.count>1000000) then
  begin
    lvResults.Items.Count:=1000000;

    if ((Staticscanner=nil){ or (staticscanner.distributedScanning=false)}) and (pointerscanresults.generatedByWorkerID=-1) then //tell the user
      showmessage(rsOnlyTheFirst1000000EntriesWillBeDisplayed);

  end else lvResults.Items.Count:=Pointerscanresults.count;

  lvResults.Align:=alClient;
  lvResults.Visible:=true;

  lvResults.Columns.EndUpdate;
  lvResults.Items.EndUpdate;

  cbtype.top:=0;
  cbtype.height:=panel1.ClientHeight;
  cbtype.Visible:=true;

  Rescanmemory1.Enabled:=true;
  new1.Enabled:=true;

  miMergePointerscanResults.enabled:=true;

  caption:=rsPointerScan+' : '+extractfilename(filename);

  miResume.visible:=Pointerscanresults.CanResume;
  miResume.Enabled:=Pointerscanresults.CanResume;
end;

procedure Tfrmpointerscanner.Open1Click(Sender: TObject);
begin
  if opendialog1.Execute then
    OpenPointerfile(utf8toansi(Opendialog1.filename));
end;

function TRescanWorker.isMatchToValue(p:pointer): boolean;
begin
  case valuetype of
    vtDword: result:=pdword(p)^=valuescandword;
    vtSingle: result:=(psingle(p)^>=valuescansingle) and (psingle(p)^<valuescansinglemax);
    vtDouble: result:=(pdouble(p)^>=valuescandouble) and (pdouble(p)^<valuescandoublemax);
  end;
end;

procedure TRescanWorker.flushresults;
begin
  tempfile.WriteBuffer(tempbuffer.Memory^,tempbuffer.Position);
  tempbuffer.Seek(0,sofrombeginning);
end;

destructor TRescanworker.destroy;
begin
  if Pointerscanresults<>nil then
    Pointerscanresults.Free;
end;

procedure TRescanWorker.execute;
var
    currentEntry: qword;
    i,j,k: integer;

    baseaddress, address,address2, tempaddress: ptrUint;
    pi: TPageInfo;
    x: dword;
    valid: boolean;

    tempvalue: pointer;
    value: pointer;

    p: ppointerscanresult;
    pointersize: integer;

    L: Plua_State;
    lref: integer;
    lfun: integer;
    ltable: integer;


    mr: TMemoryRegion;
begin
  l:=nil;

  try

  if useluafilter then
  begin
    //create a new lua thread
    luacs.enter;
    try
      l:=lua_newthread(luavm); //pushes the thread on the luavm stack.
      lref:=luaL_ref(luavm,LUA_REGISTRYINDEX); //add a reference so the garbage collector wont destroy the thread (pops the thread off the stack)
    finally
      luacs.leave;
    end;


    lua_getglobal(L, pchar(luafilter));
    lfun:=lua_gettop(L);

    //create a table for the offset
    lua_createtable(L, Pointerscanresults.offsetCount+1,0);   //+1 for a nil
    ltable:=lua_gettop(L);
  end;


  tempfile:=nil;
  tempbuffer:=nil;
  address:=0;
  address2:=0;
  pointersize:=processhandler.pointersize;



  getmem(tempvalue,valuesize);

  try
    tempfile:=tfilestream.Create(self.filename, fmCreate);
    tempbuffer:=TMemoryStream.Create;
    tempbuffer.SetSize(16*1024*1024);

    evaluated:=0;
    currentEntry:=self.startentry;

    if currentEntry>Pointerscanresults.count then exit;


    while evaluated < self.EntriesToCheck do
    begin

      if evaluated=901 then
      begin
        asm
          nop
        end
      end;

      p:=Pointerscanresults.getPointer(currentEntry);
      if p<>nil then
      begin
        valid:=true;
        if pointermap=nil then
        begin
          if p.modulenr=-1 then
            address:=p.moduleoffset
          else
            address:=Pointerscanresults.getModuleBase(p.modulenr)+p.moduleoffset
        end
        else
          address:=pointermap.getAddressFromModuleIndexPlusOffset(p.modulenr,p.moduleoffset);

        baseaddress:=address;

        if address>0 then
        begin
          //if the base must be in a range then check if the base address is in the given range
          if (not mustbeinrange) or (inrangex(address, baseStart, baseEnd)) then
          begin
            //don't care or in range.

            //check if start offet values are given
            if length(startOffsetValues)>0 then
            begin
              //check the offsets
              for i:=0 to length(startOffsetValues)-1 do
                if p.offsets[p.offsetcount-1-i]<>startOffsetValues[i] then
                begin
                  valid:=false;
                  break;
                end;
            end;

            if valid and (length(endoffsetvalues)>0) then
            begin
              j:=0;
              for i:=length(endoffsetvalues)-1 downto 0 do
              begin
                if p.offsets[j]<>endoffsetvalues[i] then
                begin
                  valid:=false;
                  break;
                end;
                inc(j);
              end;
            end;

            if valid then
            begin
              //evaluate the pointer to address
              if pointermap=nil then
              begin
                for i:=p.offsetcount-1 downto 0 do
                begin

                  pi:=rescanhelper.FindPage(address shr 12);
                  if (pi.data<>nil) then
                  begin
                    tempaddress:=0;
                    j:=address and $fff; //offset into the page
                    k:=min(pointersize, 4096-j); //bytes to read from this page


                    if (k<pointersize) then
                    begin
                      //more bytes are needed
                      copymemory(@tempaddress, @pi.data[j], k);

                      pi:=rescanhelper.FindPage((address shr 12)+1);
                      if pi.data<>nil then
                        copymemory(pointer(ptruint(@address)+k), @pi.data[0], pointersize-k)
                      else
                      begin
                        valid:=false;
                        break;
                      end;
                    end
                    else
                      tempaddress:=pptruint(@pi.data[j])^;

                    {$ifdef cpu64}
                    if pointersize=4 then
                      tempaddress:=tempaddress and $ffffffff;
                    {$endif}

                    address:=tempaddress+p.offsets[i];
                  end
                  else
                  begin
                    valid:=false;
                    break;
                  end;
                end;
              end
              else
              begin
                //use pointermap


                for i:=p.offsetcount-1 downto 0 do
                begin
                  address:=pointermap.getPointer(address);
                  if address=0 then
                  begin
                    valid:=false;
                    break;
                  end;
                  address:=address+p.offsets[i];
                end;
              end;
            end;

            if valid then
            begin
              if novaluecheck or forvalue then
              begin
                //evaluate the address (address must be accessible)
                if rescanhelper.ispointer(address) then
                begin

                  if novaluecheck=false then //check if the value is correct
                  begin

                    value:=nil;
                    pi:=rescanhelper.FindPage(address shr 12);
                    if pi.data<>nil then
                    begin
                      i:=address and $fff;
                      j:=min(valuesize, 4096-i);

                      copymemory(tempvalue, @pi.data[i], j);

                      if j<valuesize then
                      begin
                        pi:=rescanhelper.FindPage((address shr 12)+1);
                        if pi.data<>nil then
                          copymemory(pointer(ptruint(tempvalue)+j), @pi.data[0], valuesize-j)
                        else
                          valid:=false;
                      end;
                    end
                    else
                      valid:=false;

                    value:=tempvalue;

                    if (not valid) or (value=nil) or (not isMatchToValue(value)) then
                      valid:=false; //invalid value
                  end;
                end else valid:=false; //unreadable address
              end
              else
              begin
                //check if the address matches
                if address<>PointerAddressToFind then
                  valid:=false;
              end;
            end;

            if valid and useluafilter then
            begin
              //check the lua function
              //first set the offsets
              for i:=0 to p.offsetcount-1 do
              begin
                lua_pushinteger(L, p.offsets[i]);
                lua_rawseti(L, ltable, i+1);
              end;

              //end the table with a nil marker
              lua_pushnil(L);
              lua_rawseti(L, ltable, p.offsetcount+1);

              //setup the function call
              lua_pushvalue(L, lfun);           //function
              lua_pushinteger(L, baseaddress);  //base
              lua_pushvalue(L, ltable);         //offsets
              lua_pushinteger(L, address);      //address
              lua_call(L, 3,1);                 //call and don't expect any errors
              valid:=lua_toboolean(L, -1);
              lua_pop(L, 1);
            end;

            if valid then
            begin
              //checks passed, it's valid
              if pointerscanresults.compressedptr then
                p:=pointerscanresults.LastRawPointer;

              tempbuffer.Write(p^,Pointerscanresults.entrySize);

              if tempbuffer.Position>16*1024*1024 then flushresults;
            end;


          end; //must be in range and it wasn't in the range
        end;
      end;

      if valid = false then
      begin
        asm
        nop
        end;

      end;

      inc(evaluated);
      inc(currentEntry);
    end;

    flushresults;
  finally
    freemem(tempvalue);
    
    if tempfile<>nil then
      freeandnil(tempfile);

    if tempbuffer<>nil then
      freeandnil(tempbuffer);

    if l<>nil then
    begin
      lua_settop(L, 0);

      //remove the reference to the thread
      luacs.enter;
      try
        luaL_unref(LuaVM, LUA_REGISTRYINDEX, lref);
      finally
        luacs.leave;
      end;

    end;

    done:=true;

  end;

  except
    on e: exception do
    begin

      MessageBox(0, 'FUU', pchar(e.message), 0);
    end;
  end;


end;

//------RescanPointers-------
{
procedure Trescanpointers.UpdateStatus(done: boolean; TotalPointersToEvaluate:qword; PointersEvaluated: qword);
var
  r: byte;
  updatestatuscommand: packed record
    command: byte;
    done: byte;
    pointersEvaluated: qword;
    TotalPointersToEvaluate: qword;
  end;

begin

  try
    updatestatuscommand.command:=RCMD_STATUS;
    if done then
      updatestatuscommand.done:=1
    else
      updatestatuscommand.done:=0;

    updatestatuscommand.TotalPointersToEvaluate:=TotalPointersToEvaluate;
    updatestatuscommand.pointersEvaluated:=PointersEvaluated;

    sockethandlecs.enter;
    try
      send(sockethandle, @updatestatuscommand, sizeof(updatestatuscommand));
      receive(sockethandle, @r, 1);
    finally
      sockethandlecs.Leave;
    end;
  except
    on e: TSocketException do
    begin
      //socket error
      LaunchWorker; //reconnects
    end;
  end;

end;

procedure Trescanpointers.LaunchWorker;
var
  sockaddr: TInetSockAddr;
  connected: boolean;
  starttime: dword;

  command: byte;

  x: dword;
  hr: THostResolver;

  setid: packed record
    command: byte;
    workerid: dword;
  end;

  workerid: dword;

  genericQword: qword;
  genericDword: dword;
  genericByte: byte;

  i: integer;

  fname: pchar;
  mlc: dword;


begin


  sockethandle:=socket(AF_INET, SOCK_STREAM, 0);
  sockethandlecs:=TCriticalSection.Create;

  if sockethandle=INVALID_SOCKET then
    raise Exception.create('Failure creating socket');

  sockaddr.sin_family:=AF_INET;
  sockaddr.sin_port:=htons(distributedport);

  hr:=THostResolver.Create(nil);
  try

    sockaddr.sin_addr:=StrToNetAddr(distributedServer);

    if sockaddr.sin_addr.s_bytes[4]=0 then
    begin
      if hr.NameLookup(distributedServer) then
        sockaddr.sin_addr:=hr.NetHostAddress
      else
        raise exception.create('host:'+distributedServer+' could not be resolved');
    end;


  finally
    hr.free;
  end;


  starttime:=gettickcount;
  connected:=false;
  while (not connected) and (gettickcount<starttime+60000) do
  begin
    connected:=fpconnect(sockethandle, @SockAddr, sizeof(SockAddr))=0;
    if not connected then sleep(500) else break;
  end;

  if not connected then raise exception.create('Failure (re)connecting to server. No connection made within 60 seconds');


  command:=RCMD_GETPARAMS;
  send(sockethandle, @command, sizeof(command));
  //receive the scan parameters


  receive(sockethandle, @genericqword, sizeof(genericqword));
  basestart:=genericQword;

  receive(sockethandle, @genericqword, sizeof(genericqword));
  baseend:=genericQword;

  receive(sockethandle, @genericdword, sizeof(genericdword));
  setlength(startOffsetValues, genericdword);
  if length(startOffsetValues)>0 then
    receive(sockethandle, @startoffsetvalues[0], length(startOffsetValues)*sizeof(dword));

  receive(sockethandle, @genericdword, sizeof(genericdword));
  setlength(endoffsetvalues, genericdword);
  if length(endoffsetvalues)>0 then
    receive(sockethandle, @endoffsetvalues[0], length(endoffsetvalues)*sizeof(dword));

  receive(sockethandle, @genericqword, sizeof(genericqword));
  address:=genericqword;

  receive(sockethandle, @genericbyte, sizeof(genericbyte));
  forvalue:=genericbyte<>0;

  receive(sockethandle, @genericbyte, sizeof(genericbyte));
  overwrite:=genericbyte<>0;

  receive(sockethandle, @genericbyte, sizeof(genericbyte));
  mustbeinrange:=genericbyte<>0;

  receive(sockethandle, @valuescandword, sizeof(valuescandword));
  receive(sockethandle, @valuescansingle, sizeof(valuescansingle));
  receive(sockethandle, @valuescansingleMax, sizeof(valuescansingleMax));
  receive(sockethandle, @valuescandouble, sizeof(valuescandouble));
  receive(sockethandle, @valuescandoubleMax, sizeof(valuescandoubleMax));

  receive(sockethandle, @genericdword, sizeof(genericdword));
  getmem(fname, genericdword+1);
  receive(sockethandle, fname, genericdword);
  fname[genericdword]:=#0;

  originalptrfile:=distributedworkfolder+extractfilename(fname);

  receive(sockethandle, @genericdword, sizeof(genericdword));
  getmem(fname, genericdword+1);
  receive(sockethandle, fname, genericdword);
  fname[genericdword]:=#0;
  filename:=distributedworkfolder+extractfilename(fname);

  //figure out the worker id from the filename and workpath
  //check if the worker folder has a

  //check if this file exists, and if so open it and fetch the worker id from that file

  try
    if pointerscanresults=nil then
      pointerscanresults:=TPointerscanresultReader.create(originalptrfile);

    workerid:=pointerscanresults.GeneratedByWorkerID;
  except
    workerid:=-1;
  end;

  //read out the modulelist base addresses
  receive(sockethandle, @mlc, sizeof(mlc));
  for i:=0 to mlc-1 do
  begin
    receive(sockethandle, @genericqword, sizeof(genericqword));
    pointerscanresults.modulebase[i]:=genericQword;
  end;





  setid.command:=RCMD_SETID;
  setid.workerid:=workerid;
  send(sockethandle, @setid, sizeof(setid));


  if workerid=-1 then
  begin
    closehandle(sockethandle);
    terminate;
  end;




end;

function TRescanpointers.Server_HandleRead(s: Tsocket): byte;
type
  TMemRegion=packed record
    BaseAddress: qword;
    MemorySize: qword;
  end;
  PMemRegion=^TMemRegion;
var
  command: byte;
  r: Tmemorystream;

  memoryregions: TMemoryRegions;

  getPagesInput: packed record
    base: qword;
    count: byte;
  end;

  statusInput: packed record
    done: byte;
    pointersEvaluated: qword;
    TotalPointersToEvaluate: qword;
  end;

  i: integer;

  pages: array of TPageInfo;


  newworkerid: dword;
  n: TNetworkStream;

  cs: Tcompressionstream;
  ms: TMemorystream;
begin

  result:=-1;
  r:=tmemorystream.create;
  try
    receive(s, @command, 1);
    result:=command;

    case command of
      RCMD_GETPARAMS:
      begin
        n:=TNetworkStream.create;
        try
          //write the scan parameters to the client
          n.WriteQWord(baseStart);
          n.WriteQWord(baseEnd);
          n.WriteDWord(length(startOffsetValues));
          for i:=0 to length(startOffsetValues)-1 do
            n.writeDword(startOffsetValues[i]);

          n.writeDword(length(endoffsetvalues));
          for i:=0 to length(endoffsetvalues)-1 do
            n.writeDword(endoffsetvalues[i]);


          n.writeqword(address);
          if forvalue then
            n.WriteByte(1)
          else
            n.WriteByte(0);


          if overwrite then
            n.WriteByte(1)
          else
            n.WriteByte(0);

          if mustbeinrange then
            n.writebyte(1)
          else
            n.writebyte(0);


          n.WriteDWord(valuescandword);
          n.Writebuffer(valuescansingle, sizeof(valuescansingle));
          n.Writebuffer(valuescansingleMax, sizeof(valuescansingleMax));
          n.Writebuffer(valuescandouble, sizeof(valuescandouble));
          n.Writebuffer(valuescandoubleMax, sizeof(valuescandoubleMax));

          n.writedword(length(Pointerscanresults.filename));
          n.WriteBuffer(Pointerscanresults.filename[1], length(Pointerscanresults.filename));

          n.writedword(length(filename));
          n.WriteBuffer(filename[1], length(filename));

          //save the modulelist base addresses
          n.WriteDWord(Pointerscanresults.modulelistCount);
          for i:=0 to Pointerscanresults.modulelistCount-1 do
            n.WriteQWord(pointerscanresults.modulebase[i]);

          n.WriteToSocket(s);

        finally
          n.free;
        end;

      end;

      RCMD_SETID:
      begin
        receive(s, @newworkerid, sizeof(newworkerid));
        if newworkerid<length(workers) then
        begin
          if workers[newworkerid].done then
            raise TSocketException.create('This worker is already done');

          workers[newworkerid].s:=s;
        end
        else
          raise TSocketException.create('Invalid worker id');
      end;

      RCMD_GETMEMORYREGIONS:
      begin
        memoryregions:=rescanhelper.getMemoryRegions;

        r.clear;
        r.WriteDWord(length(memoryregions));

        for i:=0 to length(memoryregions)-1 do
        begin
          r.WriteQword(memoryregions[i].BaseAddress);
          r.WriteQword(memoryregions[i].MemorySize);
        end;

        send(s, r.Memory,  r.size);
      end;

      RCMD_GETPAGES:
      begin
        receive(s, @getPagesInput, sizeof(getPagesInput));

        setlength(pages, getpagesinput.count);
        for i:=0 to getPagesInput.count-1 do
          pages[i]:=rescanhelper.FindPage((getPagesInput.base shr 12)+i);

        r.Clear;
        r.writedword(length(pages));

        ms:=TMemoryStream.create;

        for i:=0 to length(pages)-1 do
        begin
          if pages[i].data<>nil then
          begin
            r.WriteByte(1);

            ms.Clear;
            cs:=Tcompressionstream.create(clfastest, ms);
            cs.WriteBuffer(pages[i].data^, 4096);
            cs.destroy;

            r.writedword(ms.Size);
            r.WriteBuffer(ms.Memory^, ms.size);
          end
          else
            r.writeByte(0);
        end;

        ms.free;

        send(s, r.Memory, r.size);
      end;

      RCMD_STATUS:
      begin
        receive(s, @statusInput, sizeof(statusInput));

        for i:=0 to length(workers)-1 do
          if s=workers[i].s then
          begin
            workers[i].PointersEvaluated:=statusinput.pointersEvaluated;
            workers[i].TotalPointersToEvaluate:=statusinput.TotalPointersToEvaluate;
            workers[i].done:=statusinput.done<>0;
          end;

        i:=0;
        send(s, @i, 1);
      end;


      else
        Raise TSocketException.create('Invalid command');
    end;
  except
    on e: TSocketException do
    begin
      for i:=0 to length(workers)-1 do
        if s=workers[i].s then
        begin
          workers[i].s:=-1;
          CloseSocket(s);
        end;
    end;
  end;

  r.free;
end;

procedure TRescanpointers.LaunchServer;
var
  b: bool;
  i,j: integer;
  sockaddr: TInetSockAddr;
begin
  //start listeneing on the "distributedport"
  sockethandle:=socket(AF_INET, SOCK_STREAM, 0);

  if sockethandle=INVALID_SOCKET then
    raise Exception.create('Failure creating socket');

  B:=TRUE;
  fpsetsockopt(sockethandle, SOL_SOCKET, SO_REUSEADDR, @B, sizeof(B));


  sockaddr.sin_family:=AF_INET;
  sockaddr.sin_port:=htons(distributedport);
  sockaddr.sin_addr.s_addr:=INADDR_ANY;
  i:=bind(sockethandle, @sockaddr, sizeof(sockaddr));

  if i=SOCKET_ERROR then
    raise exception.create('Failure to bind port '+inttostr(distributedport));

  i:=listen(sockethandle, 32);
  if i=SOCKET_ERROR then
    raise exception.create('Failure to listen');

  //preallocate the workers
  setlength(workers, ownerform.pointerscanresults.externalScanners);
  for i:=0 to length(workers)-1 do
  begin
    workers[i].s:=-1; //mark as disconnected
    workers[i].done:=false;

    for j:=0 to ownerform.pointerscanresults.mergedresultcount-1 do
      if ownerform.pointerscanresults.mergedresults[j]=i then
        workers[i].done:=true; //mark it as done (it's the local scan) so don't wait for it
  end;
end;

procedure TRescanpointers.broadcastscan;
var
  cecommand: packed record
    id: byte; //$ce
    operation: byte;
    port: word;
    test: word;
  end;

  RecvAddr: sockaddr_in;
  i: integer;
  s: Tsocket;
  v: boolean;

  r: integer;
begin
  //sends a broadcast to the local network and the potentialWorkerList
  cecommand.id:=$ce;
  cecommand.operation:=1;   //rescan
  cecommand.port:=distributedport;
  cecommand.test:=(cecommand.id+cecommand.operation+cecommand.port)*599;

  s:=fpsocket(PF_INET, SOCK_DGRAM, 0);
  v:=true;
  if fpsetsockopt(s, SOL_SOCKET, SO_BROADCAST, @v, sizeof(v)) >=0 then
  begin
    RecvAddr.sin_family:=AF_INET;
    RecvAddr.sin_addr.s_addr:=htonl(INADDR_BROADCAST);
    RecvAddr.sin_port:=htons(3297);

    fpsendto(s,  @cecommand, sizeof(cecommand), 0, @RecvAddr, sizeof(RecvAddr));

    for i:=0 to length(potentialWorkerList)-1 do
    begin
      RecvAddr.sin_addr:=potentialWorkerList[i];
      fpsendto(s,  @cecommand, sizeof(cecommand), 0, @RecvAddr, sizeof(RecvAddr));

    end;
  end;

  CloseSocket(s);
end;

procedure TRescanpointers.DoServerLoop;
var
  readfds: PFDSet;

  TotalPointersToEvaluate: double;
  PointersEvaluated: double;

  maxfd: Integer;
  alldone: boolean;

  client: TSockAddrIn;
  clientsize: integer;

  command: byte;
  workerid: dword;
  i,j: integer;

  timeout: TTimeVal;

  n: TNetworkStream;
begin
  getmem(readfds, sizeof(PtrUInt)+sizeof(TSocket)*(length(workers)+1));

  alldone:=false;

  while not alldone do
  begin
    if broadcastThisScanner and (broadcastcount<10) and (gettickcount>lastBroadcast+1000) then
    begin
      inc(broadcastcount);
      lastbroadcast:=gettickcount;
      broadcastscan;
    end;

    readfds.fd_count:=1;
    readfds.fd_array[0]:=sockethandle;

    maxfd:=sockethandle;

    for i:=0 to length(workers)-1 do
      if workers[i].s<>-1 then
      begin
        readfds.fd_array[i+1]:=workers[i].s;
        inc(readfds.fd_count);
        maxfd:=max(maxfd, workers[i].s);
      end;

    timeout.tv_sec:=0;
    timeout.tv_usec:=250000;
    i:=select(maxfd, readfds, nil, nil, @timeout);
    if i=-1 then
      raise exception.create('Select failed');

    if FD_ISSET(sockethandle, readfds^) then
    begin
      FD_CLR(sockethandle, readfds^);

      clientsize:=sizeof(client);
      i:=fpaccept(sockethandle, @client, @clientsize);
      if i<>INVALID_SOCKET then
      begin
        if Server_HandleRead(i)=RCMD_GETPARAMS then
        begin
          if server_HandleRead(i)<>RCMD_SETID then
            closesocket(i);
        end
        else
          closesocket(i); //wrong first command
      end;
    end;

    for i:=0 to length(workers)-1 do
    begin
      if (workers[i].s<>-1) and (FD_ISSET(workers[i].s, readfds^)) then
        Server_HandleRead(workers[i].s);
    end;




    alldone:=true;
    TotalPointersToEvaluate:=ownerform.pointerscanresults.count;
    PointersEvaluated:=0;

    for i:=0 to length(workers)-1 do //check ALL workers, even those not connected yet
    begin
      if workers[i].done=false then
        alldone:=false;

      TotalPointersToEvaluate:=TotalPointersToEvaluate+workers[i].TotalPointersToEvaluate;
      PointersEvaluated:=PointersEvaluated+workers[i].PointersEvaluated;
    end;

    //check my own threads
    for i:=0 to rescanworkercount-1 do
    begin
      if WaitForAll and (not rescanworkers[i].done) then
        alldone:=false;

      PointersEvaluated:=PointersEvaluated+ rescanworkers[i].evaluated;
    end;

    //update the gui
    progressbar.Position:=trunc(PointersEvaluated / (TotalPointersToEvaluate / 100));
  end;



end;        }

procedure TRescanpointers.closeOldFile;
begin
  ownerform.New1Click(ownerform.new1);
end;



procedure TRescanpointers.execute;
var
  tempstring: string;
  i,j: integer;

  TotalPointersToEvaluate: qword;
  PointersEvaluated: qword;


  blocksize: qword;

  threadhandles: array of Thandle;
  result: tfilestream;


  //rpmcontainer: TReadProcessMemoryContainer;
  temp: dword;


  valuesize: integer;

  f: tfilestream;
  ds: Tdecompressionstream;

begin
  progressbar.Min:=0;
  progressbar.Max:=100;
  progressbar.Position:=0;
  result:=nil;

  sockethandle:=INVALID_SOCKET;


  {if distributedrescan and distributedrescanWorker then
    launchworker
  else  }
  begin
    sleep(delay*1000);
    pointerscanresults:=ownerform.pointerscanresults;
    pointerscanresults.resyncModulelist;
  end;


  if forvalue and (valuetype=vtDouble) then valuesize:=8 else valuesize:=4;

  rescanhelper:=TRescanHelper.create(sockethandle, sockethandlecs);

  if pointermapfilename<>'' then
  begin
    ds:=nil;
    f:=tfilestream.Create(pointermapfilename, fmOpenRead or fmShareDenyNone);
    try
      ds:=Tdecompressionstream.create(f);
      pointermap:=TPointerListHandler.createFromStream(ds, pointermapprogressbar);
    finally
      if ds<>nil then
        freeandnil(ds);

      f.free;
    end;
  end;

  pointermapprogressbar.position:=100;


  //fill the modulelist with baseaddresses
  try
    //the modulelist now holds the baseaddresses (0 if otherwise)
    TotalPointersToEvaluate:=pointerscanresults.count;


    //spawn all threads
    rescanworkercount:=GetCPUCount;
    if HasHyperthreading then rescanworkercount:=ceil((rescanworkercount / 2)+(rescanworkercount / 4));

    blocksize:=TotalPointersToEvaluate div rescanworkercount;
    if blocksize<8 then blocksize:=8;

    setlength(rescanworkers, rescanworkercount);
    setlength(threadhandles, rescanworkercount);
    for i:=0 to rescanworkercount-1 do
    begin
      rescanworkers[i]:=TRescanWorker.Create(true);


      rescanworkers[i].Pointerscanresults:=TPointerscanresultReader.create(originalptrfile, pointerscanresults);
      rescanworkers[i].pointermap:=pointermap;
     { rescanworkers[i].OriginalFilename:=ownerform.pointerscanresults.filename;
      rescanworkers[i].OriginalFileEntrySize:=ownerform.pointerscanresults.sizeOfEntry;
      rescanworkers[i].OriginalFileStartPosition:=ownerform.pointerscanresults.StartPosition;
      rescanworkers[i].offsetlength:=ownerform.OpenedPointerfile.offsetlength;
      rescanworkers[i].modulelist:=ownerform.OpenedPointerfile.modulelist;    }
      rescanworkers[i].PointerAddressToFind:=self.address;
      rescanworkers[i].novaluecheck:=novaluecheck;

      rescanworkers[i].forvalue:=forvalue;
      rescanworkers[i].valuesize:=valuesize;
      rescanworkers[i].valuetype:=valuetype;
      rescanworkers[i].valuescandword:=valuescandword;
      rescanworkers[i].valuescansingle:=valuescansingle;
      rescanworkers[i].valuescandouble:=valuescandouble;
      rescanworkers[i].valuescansinglemax:=valuescansinglemax;
      rescanworkers[i].valuescandoublemax:=valuescandoublemax;

      rescanworkers[i].rescanhelper:=rescanhelper;

      if overwrite then
        rescanworkers[i].filename:=self.filename+'.'+inttostr(i)+'.overwrite'      
      else
        rescanworkers[i].filename:=self.filename+'.'+inttostr(i);

      rescanworkers[i].startEntry:=blocksize*i;
      rescanworkers[i].entriestocheck:=blocksize;
      if i=rescanworkercount-1 then
        rescanworkers[i].entriestocheck:=TotalPointersToEvaluate-rescanworkers[i].startEntry; //to the end


      rescanworkers[i].mustbeinrange:=mustbeinrange;
      rescanworkers[i].baseStart:=baseStart;
      rescanworkers[i].baseEnd:=baseEnd;
      setlength(rescanworkers[i].startOffsetValues, length(startoffsetvalues));
      for j:=0 to length(startOffsetValues)-1 do
        rescanworkers[i].startOffsetValues[j]:=startOffsetValues[j];

      setlength(rescanworkers[i].endoffsetvalues, length(endoffsetvalues));
      for j:=0 to length(EndOffsetValues)-1 do
        rescanworkers[i].EndOffsetValues[j]:=EndOffsetValues[j];


      rescanworkers[i].useluafilter:=useluafilter;
      rescanworkers[i].luafilter:=luafilter;


      threadhandles[i]:=rescanworkers[i].Handle;
      rescanworkers[i].start;
    end;


    if overwrite then
      result:=TFileStream.Create(filename+'.overwrite',fmCreate)
    else
      result:=TFileStream.Create(filename,fmCreate);

    //write header
    //modulelist
    pointerscanresults.saveModulelistToResults(result);

    //offsetlength
    result.Write(pointerscanresults.offsetcount, sizeof(dword));

    //pointerstores
    temp:=length(rescanworkers);
    result.Write(temp,sizeof(temp));
    for i:=0 to length(rescanworkers)-1 do
    begin
      tempstring:=ExtractFileName(rescanworkers[i].filename);
      if overwrite then
        tempstring:=copy(tempstring,1,length(tempstring)-10);
        
      temp:=length(tempstring);
      result.Write(temp,sizeof(temp));
      result.Write(tempstring[1],temp);
    end;


    //extra data
    result.writedword(pointerscanresults.externalScanners);
    result.writedword(Pointerscanresults.generatedByWorkerID);
    result.writedword(Pointerscanresults.mergedresultcount);
    for i:=0 to Pointerscanresults.mergedresultcount-1 do
      result.writedword(Pointerscanresults.mergedresults[i]);

    result.writedword(ifthen(pointerscanresults.compressedptr,1,0));
    result.writedword(ifthen(pointerscanresults.aligned,1,0));

    result.writedword(pointerscanresults.MaxBitCountModuleIndex);
    result.writedword(pointerscanresults.MaxBitCountLevel);
    result.writedword(pointerscanresults.MaxBitCountOffset);

    result.writedword(pointerscanresults.EndsWithOffsetListCount);
    for i:=0 to pointerscanresults.EndsWithOffsetListCount-1 do
      result.writedword(Pointerscanresults.EndsWithOffsetList[i]);


    result.Free;

    {if distributedrescan and (not distributedrescanWorker) then
    begin
      launchServer;
      DoServerLoop;
    end
    else}
    begin
      while WaitForMultipleObjects(rescanworkercount, @threadhandles[0], true, 250) = WAIT_TIMEOUT do      //wait
      begin
        //query all threads the number of pointers they have evaluated
        PointersEvaluated:=0;
        for i:=0 to rescanworkercount-1 do
          inc(PointersEvaluated,rescanworkers[i].evaluated);

        progressbar.Position:=PointersEvaluated div (TotalPointersToEvaluate div 100);
        {
        if distributedrescan and distributedrescanWorker then
          UpdateStatus(false, TotalPointersToEvaluate, PointersEvaluated);}
      end;
    end;
    //no timeout, so finished or crashed

   { if distributedrescan and distributedrescanWorker then
      UpdateStatus(true, TotalPointersToEvaluate, PointersEvaluated); }


    if overwrite then //delete the old ptr file
    begin
      {if distributedrescan and distributedrescanWorker then
        freeandnil(Pointerscanresults);  }

      synchronize(closeoldfile);

      DeleteFile(filename);
      RenameFile(filename+'.overwrite',filename);
    end;

    //destroy workers
    for i:=0 to rescanworkercount-1 do
    begin
      rescanworkers[i].WaitFor; //just to be sure

      if rescanworkers[i].Pointerscanresults<>nil then
        freeandnil(rescanworkers[i].Pointerscanresults);

      rescanworkers[i].Free;
      rescanworkers[i]:=nil;
    end;

    if overwrite then
    begin
      for i:=0 to rescanworkercount-1 do
      begin
        begin
          DeleteFile(filename+'.'+inttostr(i));
          RenameFile(filename+'.'+inttostr(i)+'.overwrite', filename+'.'+inttostr(i));
        end;
      end;
    end;


    rescanworkercount:=0;
    setlength(rescanworkers,0);





  finally
    if sockethandlecs<>nil then
      freeandnil(sockethandlecs);

    if sockethandle<>INVALID_SOCKET then
      CloseSocket(sockethandle);

    if rescanhelper<>nil then
      freeandnil(rescanhelper);

    progressbar.Position:=0;
    postmessage(ownerform.Handle,rescan_done,0,0);


  end;

end;

destructor TRescanpointers.destroy;
begin
  if sockethandlecs<>nil then
    freeandnil(sockethandlecs);

 { if distributedrescanWorker and (Pointerscanresults<>nil) then
    freeandnil(Pointerscanresults);    }

  if pointermapprogressbar<>nil then
    freeandnil(pointermapprogressbar);

  if pointermapprogressbarlabel<>nil then
    freeandnil(pointermapprogressbarlabel);

  if pointermap<>nil then
    freeandnil(pointermap);

  inherited destroy;
end;



procedure Tfrmpointerscanner.miJoinDistributedRescanClick(Sender: TObject);
//var f: tfrmPointerrescanConnectDialog;
begin
 { f:=tfrmPointerrescanConnectDialog.create(self);
  if f.showmodal=mrok then
  begin
    //create a rescanpointers object
    if distributedworkfolder='' then
      miSetWorkFolder.Click;

    if distributedworkfolder='' then exit;

    JoinRescan(f.edtHost.text, f.port);
  end;    }
end;

procedure Tfrmpointerscanner.Rescanmemory1Click(Sender: TObject);
var address: ptrUint;
    FloatSettings: TFormatSettings;
    floataccuracy: integer;
    i: integer;
begin
  floatsettings:=DefaultFormatSettings;


  if rescan<>nil then
    freeandnil(rescan);

  rescan:=trescanpointers.create(true);
  rescan.ownerform:=self;
  rescan.progressbar:=progressbar1;

  lblProgressbar1.caption:='Rescanning';
  pnlProgress.visible:=true;




  try
    if rescanpointerform=nil then
      rescanpointerform:=TFrmRescanPointer.Create(self);

    with rescanpointerform do
    begin
      cbDistributedRescan.visible:=Pointerscanresults.externalScanners>0;
      edtRescanPort.Visible:=Pointerscanresults.externalScanners>0;

      cbBroadcast.visible:=Pointerscanresults.externalScanners>0;
      btnNotifySpecificIPs.visible:=Pointerscanresults.externalScanners>0;
      cbWaitForAll.visible:=Pointerscanresults.externalScanners>0;


      if cbDistributedRescan.visible then
        cbDistributedRescan.OnChange(cbDistributedRescan);


      if (rescanpointerform.cbRepeat.checked) or (showmodal=mrok) then
      begin
        if (rescanpointerform.cbRepeat.checked) or savedialog1.Execute then
        begin
          rescan.novaluecheck:=cbNoValueCheck.checked;


          if cbUseSavedPointermap.checked then
          begin
            rescan.pointermapfilename:=odLoadPointermap.filename;
            rescan.pointermapprogressbar:=TProgressBar.Create(self);
            rescan.pointermapprogressbar.parent:=pnlProgressBar;
            rescan.pointermapprogressbar.position:=0;
            rescan.pointermapprogressbar.max:=100;
            rescan.pointermapprogressbar.top:=progressbar1.top+progressbar1.height;
            rescan.pointermapprogressbar.left:=ProgressBar1.Left;
            rescan.pointermapprogressbar.width:=Progressbar1.width;
            rescan.pointermapprogressbar.anchors:=ProgressBar1.anchors;

            rescan.pointermapprogressbarlabel:=TLabel.create(self);
            rescan.pointermapprogressbarlabel.caption:=extractfilename(rescan.pointermapfilename);
            rescan.pointermapprogressbarlabel.parent:=pnlProgressName;
            rescan.pointermapprogressbarlabel.top:=rescan.pointermapprogressbar.top+(rescan.pointermapprogressbar.height div 2)-(rescan.pointermapprogressbarlabel.height div 2);
            rescan.pointermapprogressbarlabel.hint:=rescan.pointermapfilename;
            rescan.pointermapprogressbarlabel.showhint:=true;

            pnlProgress.ClientHeight:=rescan.pointermapprogressbar.Top+rescan.pointermapprogressbar.height+1;

          end;

          if cbRepeat.Checked then
          begin
            //show the stop rescan repeat button
            btnStopRescanLoop.Visible:=true;
            btnStopRescanLoop.BringToFront;
          end;



          rescan.filename:=utf8toansi(savedialog1.filename);
          if cbDelay.checked then
            rescan.delay:=delay
          else
            rescan.delay:=0;

          rescan.mustbeinrange:=cbBasePointerMustBeInRange.checked;
          if rescan.mustbeinrange then
          begin
            rescan.BaseStart:=baseStart;
            rescan.BaseEnd:=baseEnd;
          end;

          if cbMustStartWithSpecificOffsets.checked then
          begin
            setlength(rescan.startOffsetValues, length(startOffsetValues));
            for i:=0 to length(startOffsetValues)-1 do
              rescan.startOffsetValues[i]:=startOffsetValues[i];
          end
          else
            setlength(rescan.startOffsetValues,0); //shouldn't be necessary, but just in case


          if cbMustEndWithSpecificOffsets.checked then
          begin
            setlength(rescan.endOffsetValues, length(endOffsetValues));
            for i:=0 to length(endOffsetValues)-1 do
              rescan.endOffsetValues[i]:=endOffsetValues[i];
          end
          else
            setlength(rescan.endoffsetvalues,0);

          if uppercase(rescan.filename)=uppercase(pointerscanresults.filename) then
            rescan.overwrite:=true;



          Rescanmemory1.Enabled:=false;
          new1.Enabled:=false;

          if cbNoValueCheck.checked=false then
          begin
            if rbFindAddress.Checked then
            begin
              try
                address:=StrToQWordEx('$'+edtAddress.Text);


              //rescan the pointerlist
              except
                raise exception.create('Find by address requires an address. "'+edtaddress.text+'" is not a valid address');
              end;

              rescan.address:=address;
              rescan.forvalue:=false;

            end
            else
            begin

              //if values, check what type of value
              floataccuracy:=pos(FloatSettings.DecimalSeparator,edtAddress.Text);
              if floataccuracy>0 then
                floataccuracy:=length(edtAddress.Text)-floataccuracy;

              case cbValueType.ItemIndex of
                0:
                begin
                  rescan.valuetype:=vtDword;
                  val(edtAddress.Text, rescan.valuescandword, i);
                  if i>0 then raise exception.Create(Format(rsIsNotAValid4ByteValue, [edtAddress.Text]));
                end;

                1:
                begin
                  rescan.valuetype:=vtSingle;
                  val(edtAddress.Text, rescan.valuescansingle, i);
                  if i>0 then raise exception.Create(Format(rsIsNotAValidFloatingPointValue, [edtAddress.Text]));
                  rescan.valuescansingleMax:=rescan.valuescansingle+(1/(power(10,floataccuracy)));
                end;

                2:
                begin
                  rescan.valuetype:=vtDouble;
                  val(edtAddress.Text, rescan.valuescandouble, i);
                  if i>0 then raise exception.Create(Format(
                    rsIsNotAValidDoubleValue, [edtAddress.Text]));
                  rescan.valuescandoubleMax:=rescan.valuescandouble+(1/(power(10,floataccuracy)));
                end;
              end;

              rescan.forvalue:=true;
            end;


          end;

          rescan.useLuaFilter:=cbLuaFilter.checked;
          rescan.LuaFilter:=edtRescanFunction.text;

          {
          if (Pointerscanresults.externalScanners>0) and (cbDistributedRescan.checked) then
          begin
            rescan.distributedport:=distributedport;
            rescan.distributedrescan:=true;
            rescan.distributedrescanWorker:=false;

            rescan.broadcastThisScanner:=cbDistributedRescan.Checked;
            rescan.potentialWorkerList:=resolvediplist;

            rescan.waitforall:=cbWaitForAll.checked;
          end;        }


          rescan.originalptrfile:=Pointerscanresults.filename;





          rescan.start;
        end;
      end;

    end;


  except
    on e: exception do
    begin
      Rescanmemory1.Enabled:=true;
      new1.Enabled:=true;


      freeandnil(rescan);
      raise exception.create(e.message);
    end;

  end;


end;

procedure tfrmpointerscanner.rescandone(var message: tmessage);


{
The rescan is done. rescan.oldpointerlist (the current pointerlist) can be deleted
and the new pointerlist becomes the current pointerlist
}
begin
  doneui;

  if rescan<>nil then
    freeandnil(rescan);
    


  if (rescanpointerform<>nil) and rescanpointerform.cbRepeat.checked then
  begin
    //repeat
    Rescanmemory1.Click;
  end
  else
  begin
    Rescanmemory1.Enabled:=true;
    new1.Enabled:=true;
  end;
end;

procedure Tfrmpointerscanner.stopscan(savestate: boolean);
var
  i: integer;
  f: tfilestream;
begin
  if staticscanner<>nil then
  begin
    if savestate then
    begin
      btnStopScan.Caption:=rsSavingAndTerminating;

      for i:=0 to pnlProgress.ControlCount-1 do
      begin
        if pnlProgress.Controls[i]<>lblProgressbar1 then
          pnlProgress.Controls[i].Visible:=false;
      end;

      for i:=0 to pnlProgressBar.ControlCount-1 do
      begin
        if pnlProgressBar.Controls[i]<>Progressbar1 then
          pnlProgressBar.Controls[i].Visible:=false;
      end;

      pnlProgress.height:=ProgressBar1.height+1;

      ProgressBar1.visible:=true;
      Progressbar1.Position:=0;
      progressbar1.Max:=100;

      progressbar1.top:=0;
      pnlProgressName.visible:=true;
      pnlProgressBar.visible:=true;
      lblProgressbar1.Visible:=true;
      pnlProgress.Visible:=true;

      staticscanner.TerminateAndSaveState;
    end
    else
    begin
      btnStopScan.Caption:=rsTerminating;
      staticscanner.Terminate;
    end;

    btnStopScan.enabled:=false;

  end;
end;

procedure Tfrmpointerscanner.btnStopScanClick(Sender: TObject);
var c: TModalResult;
begin
  if staticscanner<>nil then
  begin
    if not (staticscanner.useheapdata or staticscanner.findValueInsteadOfAddress) then
      c:=MessageDlg('Do you wish to resume the current pointerscan at a later time?', mtInformation,[mbyes, mbno, mbCancel], 0)
    else
      c:=mrno; //you can't resume scans that do a valuescan or use heapdata

    case c of
      mryes: stopscan(true);
      mrno: stopscan(false);
      else exit;
    end;
  end;
end;

procedure Tfrmpointerscanner.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Staticscanner<>nil then
  begin
    Staticscanner.Terminate;
    Staticscanner.WaitFor;
    freeandnil(Staticscanner);
  end;

  new1.Click;

  action:=cafree; //on close free itself
end;

procedure Tfrmpointerscanner.openscanner(var message: tmessage);
begin
  if frmpointerscannersettings=nil then
    frmpointerscannersettings:=tfrmpointerscannersettings.create(application);

  frmpointerscannersettings.cbAddress.text:=inttohex(message.WParam,8);
  Method3Fastspeedandaveragememoryusage1.Click;
end;

procedure Tfrmpointerscanner.PointerscanStart(sender: TObject);
begin
  pnlProgress.Visible:=false;
  timer2.enabled:=true;
end;


procedure Tfrmpointerscanner.New1Click(Sender: TObject);
begin
  if staticscanner<>nil then
    freeandnil(staticscanner);

 
  pnlData.Visible:=false;
  panel1.Caption:='';
  open1.Enabled:=true;
  new1.enabled:=true;
  rescanmemory1.Enabled:=false;

  miMergePointerscanResults.enabled:=false;

  lvResults.Items.BeginUpdate;
  lvResults.columns.BeginUpdate;
  lvResults.Columns.Clear;
  lvResults.Items.Count:=0;

  lvResults.Items.EndUpdate;
  lvResults.Columns.EndUpdate;

  Method3Fastspeedandaveragememoryusage1.Enabled:=true;

  timer2.Enabled:=false;

  if Pointerscanresults<>nil then
    freeandnil(Pointerscanresults);

end;



procedure Tfrmpointerscanner.FormCreate(Sender: TObject);
var
  x: array of integer;
  reg: tregistry;
begin
  {$ifdef injectedpscan}
  caption:='CE Injected Pointerscan';
  {$endif}
  lvResults.DoubleBuffered:=true;

  lvResults.Align:=alClient;
  lvResults.Visible:=true;

  setlength(x,1);
  if loadformposition(self,x) then
    cbtype.itemindex:=x[0];

  reg:=TRegistry.Create;
  {
  if Reg.OpenKey('\Software\Cheat Engine',false) then
  begin
    if reg.ValueExists('PointerScanWorkFolder') then
    begin
      distributedworkfolder:=IncludeTrailingPathDelimiter(reg.ReadString('PointerScanWorkFolder'));
      SelectDirectoryDialog1.filename:=distributedworkfolder;
    end;
  end; }

  reg.free;
end;

procedure Tfrmpointerscanner.lvResultsData(Sender: TObject;
  Item: TListItem);
var
  p: PPointerscanResult;
  i: integer;
  s: string;
  check: boolean; 
  doublevalue: double;
  dwordvalue: dword absolute doublevalue; //make sure of the same memory
  floatvalue: single absolute doublevalue;
  x: ptruint;

  address: ptrUint;

begin
  if Pointerscanresults<>nil then
  begin

    p:=Pointerscanresults.getPointer(item.index, address);
    if p<>nil then //just to be safe
    begin
      if p.modulenr=-1 then
        item.Caption:=inttohex(p.moduleoffset,8)
      else
      begin
        if p.moduleoffset>=0 then
          item.Caption:=ansitoutf8('"'+pointerscanresults.getModulename(p.modulenr)+'"+'+inttohex(p.moduleoffset,8))
        else
          item.Caption:=ansitoutf8('"'+pointerscanresults.getModulename(p.modulenr)+'"-'+inttohex(-p.moduleoffset,8));
      end;

      for i:=p.offsetcount-1 downto 0 do
        item.SubItems.Add(inttohex(p.offsets[i],1));

      for i:=p.offsetcount to Pointerscanresults.offsetCount-1 do
        item.SubItems.Add('');

      if address=0 then
        item.SubItems.Add('-') else
      begin
        s:=inttohex(address,8);
        if cbType.ItemIndex<>-1 then
        begin
          s:=s+' = ';
          if cbType.ItemIndex=2 then
            check:=readprocessmemory(processhandle, pointer(address),@doublevalue,8,x) else
            check:=readprocessmemory(processhandle, pointer(address),@doublevalue,4,x);

          if check then
          begin
            case cbType.ItemIndex of
              0: s:=s+inttostr(dwordvalue);
              1: s:=s+floattostr(floatvalue);
              2: s:=s+floattostr(doublevalue);
            end;
          end else s:=s+'??';
        end;

        item.SubItems.Add(s);

      end;
    end;
  end;
end;

procedure Tfrmpointerscanner.resyncloadedmodulelist;
begin
  if pointerscanresults<>nil then
    pointerscanresults.resyncModulelist;
end;

procedure Tfrmpointerscanner.Resyncmodulelist1Click(Sender: TObject);
begin
  resyncloadedmodulelist;
  lvResults.Refresh;
end;

procedure Tfrmpointerscanner.lvResultsDblClick(Sender: TObject);
var
  li: tlistitem;
  i: integer;
  offsets: array of integer;
  t: string;
  c: integer;

  vtype: TVariableType;
begin
  if lvResults.ItemIndex<>-1 then
  begin
    li:=lvResults.Items.Item[lvResults.ItemIndex];
    t:=utf8toansi(li.caption);

    try
      setlength(offsets,li.SubItems.Count);
      c:=0;

      for i:=li.SubItems.Count-2 downto 0 do
      begin
        if li.SubItems[i]='' then continue;
        offsets[c]:=strtoint('$'+li.SubItems[i]);
        inc(c);
      end;


      case cbType.ItemIndex of
        1: vtype:=vtSingle;
        2: vtype:=vtDouble;
        else vtype:=vtDword;
      end;

      mainform.addresslist.addaddress(rsPointerscanResult, t, offsets, c, vtype);
    except

    end;
  end;
end;

procedure Tfrmpointerscanner.cbTypeChange(Sender: TObject);
begin
  lvResults.Refresh;

end;

initialization
  {$i pointerscannerfrm.lrs}

end.


