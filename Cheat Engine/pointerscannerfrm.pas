unit pointerscannerfrm;

{$MODE Delphi}

//todo: Make a command prompt version of the distributed scanning pointerscan client, and make it functional in linux as well (real servers)

interface

uses
  windows, LCLIntf, LResources, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls, syncobjs, syncobjs2,
  Menus, math, frmRescanPointerUnit, pointervaluelist, rescanhelper,
  virtualmemory, symbolhandler, MainUnit, disassembler, CEFuncProc,
  NewKernelHandler, valuefinder, PointerscanresultReader, maps, zstream,
  WinSock2, Sockets, registry, PageMap, CELazySocket,
  PointerscanNetworkCommands, resolve, pointeraddresslist, pointerscanworker,
  PointerscanStructures, PointerscanController, sqlite3conn, sqldb,
  frmSelectionlistunit, commonTypeDefs;



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




  TRescanWorker=class(TThread) //todo: move to seperate unit
  private
    procedure flushresults;
    function isMatchToValue(p: pointer): boolean;
  public
    filename: string;
    tempfile: tfilestream;
    tempbuffer: TMemoryStream;

    novaluecheck: boolean;
    filterOutAccessible: boolean;
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

    offsetBase: ptrint;

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


  Trescanpointers=class(tthread) //todo: move to seperate unit
  private
    rescanworkercount: integer;
    rescanworkers: array of TRescanWorker;

    rescanhelper: TRescanHelper;
    Pointerscanresults: TPointerscanresultReader;

    procedure closeOldFile;
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

    offsetBase: ptrint;

    startOffsetValues: array of dword;
    endoffsetvalues: array of dword;

    novaluecheck: boolean; //when set to true the value and final address are not compared, just check that he final address is in fact readable
    filterOutAccessible: boolean; //when set to true, final address should be not accessible
    useluafilter: boolean; //when set to true each pointer will be passed on to the luafilter function
    luafilter: string; //function name of the luafilter

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
    cbTestCrappyConnection: TCheckBox;
    cbNonResponsive: TCheckBox;
    edtIP: TEdit;
    edtPassword: TEdit;
    edtPort: TEdit;
    gbNetwork: TGroupBox;
    psImageList: TImageList;
    lblIP: TLabel;
    lblPort: TLabel;
    lblPassword: TLabel;
    lblThreadPriority: TLabel;
    lblProgressbar1: TLabel;
    MenuItem1: TMenuItem;
    miSigned: TMenuItem;
    miHexadecimal: TMenuItem;
    miDisconnect: TMenuItem;
    miForceDisconnect: TMenuItem;
    miExportTosqlite: TMenuItem;
    MenuItem2: TMenuItem;
    miImportFromsqlite: TMenuItem;
    miCreatePSNnode: TMenuItem;
    miResume: TMenuItem;
    odMerge: TOpenDialog;
    odSqlite: TOpenDialog;
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
    miInfoPopup: TPopupMenu;
    pmType: TPopupMenu;
    ProgressBar1: TProgressBar;
    Rescanmemory1: TMenuItem;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    SaveDialog2: TSaveDialog;
    sdSqlite: TSaveDialog;
    SQLite3: TSQLite3Connection;
    SQLQuery: TSQLQuery;
    SQLTransaction: TSQLTransaction;
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
    procedure cbTestCrappyConnectionChange(Sender: TObject);
    procedure cbNonResponsiveChange(Sender: TObject);
    procedure cbTypeDropDown(Sender: TObject);

    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvResultsColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvResultsResize(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure miDisconnectClick(Sender: TObject);
    procedure miForceDisconnectClick(Sender: TObject);
    procedure miExportTosqliteClick(Sender: TObject);
    procedure miImportFromsqliteClick(Sender: TObject);
    procedure miCreatePSNnodeClick(Sender: TObject);
    procedure miInfoPopupPopup(Sender: TObject);
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
    loadedFormPosition: boolean;
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

          localPathsEvaluated: TTreenode;
          localPathsPerSecond: TTreenode;

          totalPathsEvaluated: TTreenode;
          totalPathsPerSecond: TTreenode;

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
            disconnectreason: TTreenode;
            trusted: TTreenode; //trusted: true/false
            totalthreadcount: TTreenode; //Total threadcount: %d
            resultsfound: TTreenode; //Total results found: %d
            pathqueuesize: TTreenode; //Queuesize: %d
            totalpathquesize: TTreenode; //Total queuesize: %d
            totalpathsEvaluated: TTreenode; //Total paths evaluated: %d
            pathspersecond: TTreenode; //Paths per second: %d
            lastupdate: TTreenode;
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

    SkipNextScanSettings: boolean; //set to true if you wish to start the scan with the predefined settings in the pointerscan settings form
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
  frmSetupPSNNodeUnit, PointerscanNetworkStructures, parsers, byteinterpreter,
  CustomTypeHandler, ceregistry, vartypestrings;

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
  rsPointercount = 'Pointer paths';
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
  rsPSDoYouWishToResumeTheCurrentPointerscanAtaLaterTime = 'Do you wish to resume the current pointerscan at a later time?';
  rsPSGeneratingPointermap = 'Generating pointermap';
  rsPSExportToDatabase = 'Export to database';
  rsPSExportToDatabaseBiggerSizeOrNot = 'Do you want "result" table with indexes and keys? It will take up additional disk space.';
  rsPSExportToDatabaseBiggerSizeOrNot_resultid = 'Do you want resultid column filled? It will take up additional disk space.';
  rsPSGiveaNameForTheseResults = 'Give a name for these results';
  rsPSExporting = 'Exporting...';
  rsPSThisDatabaseDoesntContainAnyPointerFiles = 'This database does not contain any pointer files';
  rsPSInvalidDatabase = 'Invalid database';
  rsPSThereIsAlreadyaPointerFileWithThsiNamePresentinThisDatabase = 'There is already a pointerfile with this name present in this database. Replace it''s content with this one ?';
  rsPSExportAborted = 'Export aborted';
  rsPSImporting = 'Importing...';
  rsPSImporting_sortOrNot = 'Do you wish to sort pointerlist by level, then module, then offsets?';
  rsPSStatistics = 'Statistics';
  rsPSUniquePointervaluesInTarget = 'Unique pointervalues in target:';
  rsPSScanDuration = 'Scan duration: ';
  rsPSPathsEvaluated = 'Paths evaluated: ';
  rsPSPathsSeconds = 'Paths / seconds: (%.0n / s)';
  rsPSTotalPathsEvaluater = 'Total paths evaluated: ';
  rsPSTotalPathsSeconds = 'Total paths / seconds: (%.0n / s)';
  rsPSStaticQueueSize = 'Static queue size: ';
  rsPSDynamicqueuSize = ' Dynamic queue size:';
  rsPSResultsFound = 'Results found: ';
  rsPSTimeSpentWriting = 'Time spent writing: ';
  rsPSLowestKnownPath = 'Lowest known path:';
  rsPSThreads = 'Threads';
  rsPSNetwork = 'Network';
  rsPSConnectingTo = 'Connecting to:';
  rsPSTrusted = ' (Trusted)';
  rsPSParent = 'Parent: ';
  rsPSDownloadingScanData = 'Downloading scandata: %.1f%% (%dKB/%dKB : %d KB/sec)';
  rsPSLastUpdate = 'Last update: ';
  rsPSSecondsAgo = ' seconds ago';
  rsPSParentDisconnectedWaitingForReconnect = 'Parent: <disconnected> (Waiting for reconnect)';
  rsPSParentNone = 'Parent: <none>';
  rsPSChildren = 'Children:';
  rsPSDisconnected = ' (Disconnected)';
  rsPSQueued = ' (Queued: ';
  rsPSActive = ' (Active)';
  rsPSIdle = ' (Idle)';
  rsPSUploadingScandata = ' (Uploading scandata: %.1f%% (%dKB/%dKB : %d KB/sec)';
  rsPSDownloadingAndHandlingResults = ' (Downloading and handling results)';
  rsPSTrusted2 = 'Trusted: ';
  rsPSThreadcount = 'Threadcount: ';
  rsPSQueuesize = 'Queuesize: ';
  rsPSTotalQueuesize = 'Total Queuesize: ';
  rsPSPscanguiUpdateTimerError = 'pscangui update timer error: ';
  rsPSREscanning = 'Rescanning';
  rsPSFindByAddressPart1 = 'Find by address requires an address. "';
  rsPSFindByAddressPart2 = '" is not a valid address';
  rsAreYouSureYouWishYouForceADisconnect = 'Are you sure you wish you force a disconnect. The current paths will be lost';
  rsCEInjectedPointerscan = 'CE Injected Pointerscan';

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

  if SkipNextScanSettings then close;
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
  totalpathsevaluated: qword;
  compressedptr: boolean;
  unalligned: boolean;
  staticonly: boolean;
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

  pb: TProgressbar;
  lb: TLabel;


begin
  //show a dialog where the user can pick the number of threads to scan
  if (pointerscanresults<>nil) and Pointerscanresults.CanResume then
  begin
    filename:=Pointerscanresults.filename;

    try
      config:=TFileStream.Create(filename+'.resume.config', fmOpenRead or fmShareDenyNone);
    except
      exit;
    end;





    maxlevel:=config.ReadDWord;
    structsize:=config.ReadDWord;
    totalpathsevaluated:=config.ReadQWord; //IGNORED
    compressedptr:=config.ReadByte=1;
    unalligned:=config.ReadByte=1;
    staticonly:=config.ReadByte=1;
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

    new1.Click;
    if f.showmodal=mrOK then
    begin
      threadcount:=f.threadcount;

      for i:=0 to f.instantrescanfiles.Count-1 do
        instantrescanentries[i].filename:=f.instantrescanfiles[i];


      //new1.click;

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


      if lblProgressbar1.Height>progressbar1.Height then
        ProgressBar1.Height:=lblProgressbar1.height;

      lblProgressbar1.Top:=progressbar1.Top+(progressbar1.height div 2)-(lblProgressbar1.Height div 2);


      pnlProgress.Visible:=true;

      try
        staticscanner.initializer:=true;
        staticscanner.OnStartScan:=PointerscanStart;
        staticscanner.OnScanDone:=PointerscanDone;
        staticscanner.threadcount:=threadcount;
        staticscanner.resumescan:=true;
        staticscanner.maxlevel:=maxlevel;
        staticscanner.sz:=structsize;
        staticscanner.compressedptr:=compressedptr;
        staticscanner.unalligned:=unalligned;
        staticscanner.staticonly:=staticonly;
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
          pb.height:=Progressbar1.Height;
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
          if pnlProgressname.clientwidth<lb.width then
            pnlProgressName.ClientWidth:=lb.width+10;

        end;

        staticscanner.UseLoadedPointermap:=true;
        staticscanner.LoadedPointermapFilename:=filename+'.resume.scandata';

        staticscanner.progressbar:=ProgressBar1;
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
  SkipNextScanSettingsWasTrue: boolean;
begin
  SkipNextScanSettingsWasTrue:=SkipNextScanSettings;

  FloatSettings:=DefaultFormatSettings;

  start:=now;
  if frmpointerscannersettings=nil then
    frmpointerscannersettings:=tfrmpointerscannersettings.create(application);

  if frmpointerscannersettings.Visible then exit; //already open, so no need to make again


  if SkipNextScanSettingsWasTrue or (frmpointerscannersettings.Showmodal=mrok) then
  begin
    new1.click;

    if frmpointerscannersettings.rbGeneratePointermap.checked then //show a .scandata dialog instad of a .ptr
    begin
      if not savedialog2.execute then
      begin
        if SkipNextScanSettings then close;
        exit;
      end;
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

      staticscanner.negativeOffsets:=frmpointerscannersettings.cbNegativeOffsets.checked;
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
        lblProgressbar1.caption:=rsPSGeneratingPointermap;

      if frmpointerscannersettings.cbLimitScanToRegionFile.Checked then
        staticscanner.RegionFilename:=frmpointerscannersettings.odLoadRegionFile.FileName
      else
        staticscanner.RegionFilename:='';


      staticscanner.startaddress:=frmpointerscannersettings.start;
      staticscanner.stopaddress:=frmpointerscannersettings.Stop;

      staticscanner.unalligned:=not frmpointerscannersettings.CbAlligned.checked;
      staticscanner.staticonly:=frmpointerscannersettings.cbStaticOnly.checked;
      staticscanner.noLoop:=frmpointerscannersettings.cbNoLoop.checked;
      staticscanner.LimitToMaxOffsetsPerNode:=frmpointerscannersettings.cbMaxOffsetsPerNode.Checked;
      staticscanner.maxOffsetsPerNode:=frmpointerscannersettings.maxOffsetsPerNode;

      staticscanner.includeSystemModules:=frmpointerscannersettings.cbIncludeSystemModules.Checked;


      staticscanner.automatic:=true;

      staticscanner.automaticaddress:=frmpointerscannersettings.automaticaddress;
      staticscanner.sz:=frmpointerscannersettings.structsize;
      staticscanner.maxlevel:=frmpointerscannersettings.maxlevel-1;


      staticscanner.progressbarLabel:=lblProgressbar1;
      staticscanner.progressbar:=progressbar1;
      staticscanner.threadcount:=frmpointerscannersettings.threadcount;
      staticscanner.scannerpriority:=frmpointerscannersettings.scannerpriority;


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
            if pnlProgressname.clientwidth<lb.width then
              pnlProgressName.ClientWidth:=lb.width+10;
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

procedure Tfrmpointerscanner.MenuItem1Click(Sender: TObject);
var
  i: integer;
  s: tstringlist;
begin
  if pointerscanresults<>nil then
  begin
    s:=tstringlist.create;
    for i:=0 to pointerscanresults.modulelistCount-1 do
      s.add(inttohex(pointerscanresults.modulebase[i],8)+' = '+pointerscanresults.modulename[i]);

    showmessage(s.text);
    s.free;
  end;

end;

procedure Tfrmpointerscanner.miDisconnectClick(Sender: TObject);
var childid: integer;
begin
  if (tvinfo.Selected<>nil) and (tvinfo.Selected.Data<>nil) then
  begin
    childid:=ptruint(tvinfo.Selected.Data);

    if Staticscanner<>nil then
      Staticscanner.disconnectChild(childid, false);

    miForceDisconnect.Enabled:=true;
  end;
end;

procedure Tfrmpointerscanner.miForceDisconnectClick(Sender: TObject);
var childid: integer;
begin
  //disconnect this one

  if MessageDlg(rsAreYouSureYouWishYouForceADisconnect, mtWarning, [mbyes, mbno], 0, mbNo)<>mryes then exit;

  if (tvinfo.Selected<>nil) and (tvinfo.Selected.Data<>nil) then
  begin
    childid:=ptruint(tvinfo.Selected.Data);

    if Staticscanner<>nil then
      Staticscanner.disconnectChild(childid, true);
  end;

end;

procedure Tfrmpointerscanner.miExportTosqliteClick(Sender: TObject);
var
  filename: string;
  r: TModalResult;
  name: string;
  maxlevel: string;
  compressedptr, unalligned, MaxBitCountModuleIndex, MaxBitCountModuleOffset, MaxBitCountLevel, MaxBitCountOffset: string;
  DidBaseRangeScan, BaseScanRange: string;

  tablenames: Tstringlist;
  fieldnames: tstringlist;

  offsetlist, offsetvalues: string;
  ptrid: string;
  i: integer;
  j: qword;
  resultidcolumnsave: boolean;

  p: PPointerscanResult;
  s: string;

  pb: TProgressbar;
  pbl: TLabel;

  oldpb: string;
begin
  if (Pointerscanresults<>nil) and (sdSqlite.execute) then
  begin
    oldpb:=lblProgressbar1.Caption;

    filename:=utf8toansi(sdsqlite.FileName);
    name:=extractfilename(pointerscanresults.filename);

    if InputQuery(rsPSExportToDatabase,rsPSGiveaNameForTheseResults, name)=false then exit;



    SQLite3.DatabaseName:=filename;
    sqlite3.Connected:=true;

    SQLTransaction.active:=true;

    try
      tablenames:=tstringlist.create;
      sqlite3.GetTableNames(tablenames, false);

      //build the tables if necesary
      if tablenames.IndexOf('pointerfiles')=-1 then
      begin
        sqlite3.ExecuteDirect('CREATE TABLE pointerfiles ('+
	                      '`ptrid`	INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,'+
	                      '`name`	char(256) NOT NULL,'+
	                      '`maxlevel`	INTEGER,'+
	                      '`compressedptr`	INTEGER,'+
	                      '`unalligned`	INTEGER,'+
	                      '`MaxBitCountModuleIndex`	INTEGER,'+
	                      '`MaxBitCountModuleOffset`	INTEGER,'+
	                      '`MaxBitCountLevel`	INTEGER,'+
	                      '`MaxBitCountOffset`	INTEGER,'+
	                      '`DidBaseRangeScan`	INTEGER,'+
	                      '`BaseScanRange`	INTEGER);');

        sqlite3.ExecuteDirect('CREATE UNIQUE INDEX "id_idx" ON pointerfiles( "ptrid" );');
      end;

      if tablenames.IndexOf('pointerfiles_endwithoffsetlist')=-1 then
      begin
        sqlite3.ExecuteDirect('CREATE TABLE pointerfiles_endwithoffsetlist ('+
      	                      '  `ptrid`	INTEGER NOT NULL,'+
      	                      '  `offsetnr`	INTEGER NOT NULL,'+
      	                      '  `offsetvalue`	INTEGER NOT NULL,'+
      	                      '  PRIMARY KEY(ptrid,offsetnr)'+
                              ');');

        sqlite3.ExecuteDirect('CREATE UNIQUE INDEX "ptrid_idx" ON pointerfiles_endwithoffsetlist( "ptrid" );');
      end;



      if tablenames.IndexOf('modules')=-1 then
      begin
        sqlite3.ExecuteDirect('create table modules(ptrid integer not null, moduleid integer not null, name char(256) not null, primary key (ptrid, moduleid) );');
        sqlite3.ExecuteDirect('CREATE UNIQUE INDEX "ptr_mod_id_idx" ON "modules"( ptrid, moduleid );');
      end;

      if tablenames.IndexOf('results')=-1 then
      begin

        offsetlist:='';
        for i:=1 to pointerscanresults.offsetCount do
          offsetlist:=offsetlist+', offset'+inttostr(i)+' integer';


       if messagedlg(rsPSExportToDatabaseBiggerSizeOrNot, mtConfirmation, [mbyes, mbno], 0) = mryes then
        begin
          sqlite3.ExecuteDirect('create table results(ptrid integer not null, resultid integer, offsetcount integer, moduleid integer, moduleoffset integer '+offsetlist+', primary key (ptrid, resultid) );');
          sqlite3.ExecuteDirect('CREATE UNIQUE INDEX "ptr_res_id_idx" ON "results"( ptrid, resultid );');
          sqlite3.ExecuteDirect('CREATE INDEX "modid_modoff_idx" ON "results"( moduleid, moduleoffset );');
        end
        else
          sqlite3.ExecuteDirect('create table results(ptrid integer not null, resultid integer, offsetcount integer, moduleid integer, moduleoffset integer '+offsetlist+');');
      end
      else
      begin
        //might need an update
        fieldnames:=tstringlist.create;
        sqlite3.GetFieldNames('results', fieldnames);

        for i:=1 to pointerscanresults.offsetCount do
          if fieldnames.indexof('offset'+inttostr(i))=-1 then
            sqlite3.ExecuteDirect('ALTER TABLE results ADD COLUMN offset'+inttostr(i)+' integer');

        fieldnames.free;
      end;

      tablenames.free;

      SQLQuery.SQL.Text:='Select ptrid from pointerfiles where name="'+name+'"';
      SQLQuery.Active:=true;

      //SQLQuery.Params;

      if SQLQuery.RecordCount>0 then
      begin
        ptrid:=SQLQuery.FieldByName('ptrid').text;
        if MessageDlg(rsPSExportToDatabase, rsPSThereIsAlreadyaPointerFileWithThsiNamePresentinThisDatabase, mtConfirmation, [mbyes, mbno], 0)<>mryes then
        begin
          showmessage(rsPSExportAborted);
          exit;
        end;

        cursor:=crHourGlass;
        sqlite3.ExecuteDirect('delete from results where ptrid='+ptrid);
        sqlite3.ExecuteDirect('delete from modules where ptrid='+ptrid);
        sqlite3.ExecuteDirect('delete from pointerfiles where ptrid='+ptrid);
        sqlite3.ExecuteDirect('delete from pointerfiles_endwithoffsetlist where ptrid='+ptrid);
        cursor:=crDefault;
      end;
      SQLQuery.Active:=false;



      //and now fill it
      cursor:=crHourGlass;


      lblProgressbar1.Caption:=rsPSExporting;
      progressbar1.position:=0;
      progressbar1.max:=100;
      if lblProgressbar1.Height>progressbar1.Height then
        ProgressBar1.Height:=lblProgressbar1.height;

      lblProgressbar1.Top:=progressbar1.Top+(progressbar1.height div 2)-(lblProgressbar1.Height div 2);
      pnlProgress.visible:=true;

      Update;



      maxlevel:=inttostr(pointerscanresults.offsetCount);
      compressedptr:=inttostr(ifthen(Pointerscanresults.compressedptr, 1, 0));
      if Pointerscanresults.compressedptr then
      begin
        unalligned:=inttostr(ifthen(Pointerscanresults.aligned,0,1));
        MaxBitCountModuleIndex:=IntToStr(Pointerscanresults.MaxBitCountModuleIndex);
        MaxBitCountModuleOffset:=IntToStr(Pointerscanresults.MaxBitCountModuleOffset);
        MaxBitCountLevel:=IntToStr(Pointerscanresults.MaxBitCountLevel);
        MaxBitCountOffset:=IntToStr(Pointerscanresults.MaxBitCountOffset);
      end
      else
      begin
        unalligned:='NULL';
        MaxBitCountModuleIndex:='NULL';
        MaxBitCountModuleOffset:='NULL';
        MaxBitCountLevel:='NULL';
        MaxBitCountOffset:='NULL';
      end;

      DidBaseRangeScan:=inttostr(ifthen(Pointerscanresults.DidBaseRangeScan, 1, 0));
      if Pointerscanresults.DidBaseRangeScan then
        BaseScanRange:=inttostr(Pointerscanresults.BaseScanRange)
      else
        BaseScanRange:='NULL';

      s:='INSERT INTO pointerfiles (name, maxlevel, compressedptr, unalligned, MaxBitCountModuleIndex, MaxBitCountModuleOffset, MaxBitCountLevel, MaxBitCountOffset, DidBaseRangeScan, BaseScanRange) values ("'+name+'", '+maxlevel+','+compressedptr+','+unalligned+','+MaxBitCountModuleIndex+','+MaxBitCountModuleOffset+','+MaxBitCountLevel+','+MaxBitCountOffset+','+DidBaseRangeScan+','+BaseScanRange+')';

      sqlite3.ExecuteDirect(s);
      for i:=0 to Pointerscanresults.EndsWithOffsetListCount-1 do
      begin
        s:='INSERT INTO pointerfiles_endwithoffsetlist (ptrid, offsetnr, offsetvalue) values ("'+ptrid+'", '+inttostr(i)+','+inttostr(Pointerscanresults.EndsWithOffsetList[i])+')';
        sqlite3.ExecuteDirect(s);
      end;



      SQLQuery.SQL.Text:='Select max(ptrid) as max from pointerfiles';
      SQLQuery.Active:=true;

      ptrid:=SQLQuery.FieldByName('max').AsString;

      SQLQuery.active:=false;


      for i:=0 to Pointerscanresults.modulelistCount-1 do
        sqlite3.ExecuteDirect('INSERT INTO modules(ptrid, moduleid, name) values ('+ptrid+','+inttostr(i)+',"'+Pointerscanresults.getModulename(i)+'")');

      resultidcolumnsave:=true;
      if messagedlg(rsPSExportToDatabaseBiggerSizeOrNot_resultid, mtConfirmation, [mbyes, mbno], 0) = mrno then resultidcolumnsave:=false;

      //for j:=0 to Pointerscanresults.count-1 do
      j:=0;
      while j<=Pointerscanresults.count-1 do
      begin
        offsetlist:='';
        offsetvalues:='';
        p:=Pointerscanresults.getPointer(j);

        for i:=1 to p.offsetcount do
        begin
          offsetlist:=offsetlist+',offset'+inttostr(i);
          offsetvalues:=offsetvalues+','+inttostr(p.offsets[i-1]);
        end;

        if resultidcolumnsave then
          s:='INSERT INTO results(ptrid, resultid, offsetcount, moduleid, moduleoffset'+offsetlist+') values ('+ptrid+','+inttostr(j)+','+inttostr(p.offsetcount)+','+inttostr(p.modulenr)+','+inttostr(p.moduleoffset)+offsetvalues+')'
        else 
          s:='INSERT INTO results(ptrid, offsetcount, moduleid, moduleoffset'+offsetlist+') values ('+ptrid+','+inttostr(p.offsetcount)+','+inttostr(p.modulenr)+','+inttostr(p.moduleoffset)+offsetvalues+')';

        sqlite3.ExecuteDirect(s);

        if j mod 50=0 then
        begin
          progressbar1.position:=ceil(j / Pointerscanresults.count * 100);
          progressbar1.Update;
        end;
        inc(j);
      end;
      progressbar1.position:=100;
      progressbar1.update;

      SQLTransaction.Commit;
      SQLTransaction.Active:=false;



    finally
      sqlite3.Connected:=false;

      cursor:=crDefault;

      lblProgressbar1.Caption:=oldpb;
      pnlProgress.visible:=false;

      beep;
    end;
  end;
end;

procedure Tfrmpointerscanner.miImportFromsqliteClick(Sender: TObject);
var
  l: TStringList;
  f: TfrmSelectionList;
  name, filename, offsetlist: string;

  query2: TSQLQuery;

  ptrfile: tfilestream;
  resultptrfile: Tfilestream;

  ptrid: string;
  i: integer;
  compressed: boolean;
  alligned: boolean;
  MaxBitCountModuleIndex: dword;
  MaxBitCountModuleOffset: dword;
  MaxBitCountLevel: dword;
  MaxBitCountOffset: dword;

  MaskModuleIndex: dword;
  MaskLevel: dword;
  MaskOffset: dword;

  maxlevel: integer;

  mustendwithoffsetlistlength: integer;

  compressedEntry: pbytearray;
  compressedEntrySize: integer;
  bit: integer;
  bd8, bm8: dword;

  totalcount: qword;
  importedcount: qword;

  oldpb: string;
begin
  if (odSqlite.execute) then
  begin
    filename:=utf8toansi(odsqlite.FileName);

    SQLite3.DatabaseName:=filename;
    sqlite3.Connected:=true;

    SQLQuery.SQL.Text:='select name from pointerfiles';
    SQLQuery.Active:=true;
    if SQLQuery.RecordCount>0 then
    begin
      l:=tstringlist.create;
      while not SQLQuery.EOF do
      begin
        l.add(SQLQuery.FieldByName('Name').Text);
        SQLQuery.Next;
      end;

    end;
    SQLQuery.Active:=false;

    if l.Count=0 then
    begin
      MessageDlg(rsPSThisDatabaseDoesntContainAnyPointerFiles, mtError, [mbok],0);
      exit;
    end;

    f:=TfrmSelectionList.create(self, l);
    try
      if f.showmodal=mrok then
        name:=f.selected
      else
        exit;
    finally
      f.free;
      l.free;
    end;

    savedialog1.FileName:=name;
    if savedialog1.Execute=false then exit;

    filename:=utf8toansi(savedialog1.filename);


    ptrfile:=nil;
    query2:=nil;


    SQLQuery.SQL.Text:='select * from pointerfiles where name="'+name+'"';
    SQLQuery.Active:=true;
    try
      if (SQLQuery.RecordCount=0) or (SQLQuery.RecordCount>1) then
        raise exception.create(rsPSInvalidDatabase);

      query2:=TSQLQuery.Create(self);  //extra query
      query2.DataBase:=SQLite3;


      ptrfile:=TFileStream.Create(filename, fmCreate);
      ptrfile.writeByte($ce);
      ptrfile.writeByte(pointerscanfileversion);

      ptrid:=SQLQuery.FieldByName('ptrid').text;


      //save the modulelist

      try

        query2.sql.text:='Select count(*) as count from modules where ptrid='+ptrid;
        query2.Active:=true;
        ptrfile.WriteDWord(query2.FieldByName('count').AsInteger);
        query2.active:=false;


        query2.sql.text:='Select * from modules where ptrid='+ptrid+' order by moduleid asc';
        query2.active:=true;

        i:=0;
        while not query2.EOF do
        begin
          if query2.FieldByName('moduleid').AsInteger=i then
          begin
            name:=query2.FieldByName('name').AsString;
            //OutputDebugString(pchar(name));

            ptrfile.WriteAnsiString(name);
            ptrfile.WriteQWord(0);

            query2.next;
          end
          else
          begin
            //this one seems to be missing
            ptrfile.WriteAnsiString('Bogus');
            ptrfile.WriteQword(0);
          end;
          inc(i);
        end;
      finally
        query2.active:=false;
      end;


      maxlevel:=SQLQuery.FieldByName('maxlevel').AsInteger;
      ptrfile.WriteDword(maxlevel);
      if SQLQuery.FieldByName('compressedptr').AsInteger=1 then
      begin
        compressed:=true;
        alligned:=SQLQuery.FieldByName('unalligned').AsInteger=0;
        MaxBitCountModuleIndex:=SQLQuery.FieldByName('MaxBitCountModuleIndex').AsInteger;
        MaxBitCountModuleOffset:=SQLQuery.FieldByName('MaxBitCountModuleOffset').AsInteger;
        MaxBitCountLevel:=SQLQuery.FieldByName('MaxBitCountLevel').AsInteger;
        MaxBitCountOffset:=SQLQuery.FieldByName('MaxBitCountOffset').AsInteger;

        ptrfile.WriteByte(1);
        ptrfile.writeByte(ifthen(alligned, 1,0));
        ptrfile.writeByte(MaxBitCountModuleIndex);
        ptrfile.writeByte(MaxBitCountModuleOffset);
        ptrfile.writeByte(MaxBitCountLevel);
        ptrfile.writeByte(MaxBitCountOffset);

        query2.sql.text:='select count(*) as count from pointerfiles_endwithoffsetlist where ptrid='+ptrid;
        query2.active:=true;


        mustendwithoffsetlistlength:=query2.FieldByName('count').AsInteger;
        ptrfile.writebyte(mustendwithoffsetlistlength);
        query2.active:=false;

        query2.sql.text:='select * from pointerfiles_endwithoffsetlist where ptrid='+ptrid;
        query2.active:=true;
        while not query2.eof do
        begin
          ptrfile.WriteDWord(query2.FieldByName('offsetvalue').AsInteger);
          query2.next;
        end;
        query2.active:=false;
      end
      else
      begin
        compressed:=false;
        ptrfile.WriteByte(0);
      end;

      l:=tstringlist.create;
      sqlite3.GetFieldNames('pointerfiles', l);
      if (l.indexof('DidBaseRangeScan')<>-1) and (SQLQuery.FieldByName('DidBaseRangeScan').AsInteger=1) then
      begin
        ptrfile.WriteByte(1);
        ptrfile.WriteQWord(SQLQuery.FieldByName('BaseScanRange').AsLargeInt);
      end
      else
        ptrfile.WriteByte(0);
      l.free;

    finally
      if ptrfile<>nil then
        freeandnil(ptrfile);

      SQLQuery.Active:=false;

      if query2<>nil then
      begin
        query2.active:=false;
        freeandnil(query2);
      end;
    end;

    oldpb:=lblProgressbar1.Caption;
    lblProgressbar1.Caption:=rsPSImporting;
    progressbar1.position:=0;
    progressbar1.max:=100;

    if lblProgressbar1.Height>progressbar1.Height then
     ProgressBar1.Height:=lblProgressbar1.height;

    lblProgressbar1.Top:=progressbar1.Top+(progressbar1.height div 2)-(lblProgressbar1.Height div 2);

    pnlProgress.visible:=true;

    Update;


    compressedEntry:=nil;
    resultptrfile:=nil;

{      totalcount: qword;
  importedcount: qword;
  }
    importedcount:=0;

    sqlquery.sql.text:='select count(*) as count from results where ptrid='+ptrid;
    SQLQuery.Active:=true;
    totalcount:=SQLQuery.FieldByName('count').AsInteger;
    sqlquery.Active:=false;


    offsetlist:='';
    for i:=1 to maxlevel do offsetlist:=offsetlist+', offset'+inttostr(i);

    if messagedlg(rsPSImporting_sortOrNot, mtConfirmation, [mbyes, mbno], 0) = mryes then
      sqlquery.sql.text:='select * from results where ptrid='+ptrid+' order by offsetcount, moduleid'+offsetlist
    else
      sqlquery.sql.text:='select * from results where ptrid='+ptrid;

    SQLQuery.active:=true;
    try
      resultptrfile:=tfilestream.create(filename+'.results.0', fmcreate);

      if compressed then
      begin
        compressedEntrySize:=MaxBitCountModuleOffset+MaxBitCountModuleIndex+MaxBitCountLevel+MaxBitCountOffset*(maxlevel-mustendwithoffsetlistlength);
        compressedEntrySize:=(compressedEntrySize+7) div 8;

        getmem(compressedEntry, compressedEntrySize+4); //+4 so there's some space for overhead (writing using a dword pointer to the last byte)


        MaskModuleIndex:=0;
        for i:=1 to MaxBitCountModuleIndex do
          MaskModuleIndex:=(MaskModuleIndex shl 1) or 1;

        MaskLevel:=0;
        for i:=1 to MaxBitCountLevel do
          MaskLevel:=(MaskLevel shl 1) or 1;

        MaskOffset:=0;
        for i:=1 to MaxBitCountOffset do
          MaskOffset:=(MaskOffset shl 1) or 1;


      end;

      while not SQLQuery.eof do
      begin
        if compressed then
        begin
          ;
          //-------------------------------------------
          bit:=0;

          pqword(compressedEntry)^:=sqlquery.FieldByName('moduleoffset').AsLargeInt;
          bit:=bit+MaxBitCountModuleOffset;

          bd8:=bit shr 3; //bit div 8;
          pdword(@compressedEntry[bd8])^:=sqlquery.FieldByName('moduleid').AsInteger;
          bit:=bit+MaxBitCountModuleIndex;


          bd8:=bit shr 3; //bit div 8;
          bm8:=bit and $7; //bit mod 8;

          pdword(@compressedEntry[bd8])^:=pdword(@compressedEntry[bd8])^ and (not (MaskLevel shl bm8)) or ((sqlquery.FieldByName('offsetcount').AsInteger-mustendwithoffsetlistlength) shl bm8);
          bit:=bit+MaxBitCountLevel;    //next section



          //compress the offsets
          for i:=1 to sqlquery.FieldByName('offsetcount').AsInteger-mustendwithoffsetlistlength do
          begin
            bd8:=bit shr 3; //bit div 8;
            bm8:=bit and $7; //bit mod 8;

            if alligned then
              pdword(@compressedEntry[bd8])^:=pdword(@compressedEntry[bd8])^ and (not (MaskOffset shl bm8)) or ((sqlquery.FieldByName('offset'+inttostr(i)).AsInteger shr 2) shl bm8)
            else
              pdword(@compressedEntry[bd8])^:=pdword(@compressedEntry[bd8])^ and (not (MaskOffset shl bm8)) or ((sqlquery.FieldByName('offset'+inttostr(i)).AsInteger) shl bm8);

            bit:=bit+MaxBitCountOffset;
          end;

          resultptrfile.WriteBuffer(compressedEntry^, compressedEntrySize);
          //-------------------------------------------
        end
        else
        begin
          resultptrfile.WriteDWord(sqlquery.FieldByName('moduleid').AsInteger);
          resultptrfile.WriteQWord(sqlquery.FieldByName('moduleoffset').AsLargeInt);
          resultptrfile.WriteDWord(sqlquery.FieldByName('offsetcount').AsInteger);
          for i:=1 to maxlevel do
            resultptrfile.WriteDWord(sqlquery.FieldByName('offset'+inttostr(i)).AsInteger);
        end;
        //SQLQuery.FieldByName('');
        SQLQuery.next;
        inc(importedcount);

        if importedcount mod 25=0 then
        begin
          progressbar1.Position:=ceil(importedcount/totalcount*100);
          progressbar1.update;
        end;
      end;
    finally

      pnlProgress.visible:=false;
      lblProgressbar1.caption:=oldpb;


      SQLQuery.active:=false;
      if resultptrfile<>nil then
        freeandnil(resultptrfile);

      if compressedEntry<>nil then
        freememandnil(compressedEntry);
    end;


    OpenPointerfile(filename);

  end;

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

    staticscanner.OnStartScan:=PointerscanStart;
    staticscanner.OnScanDone:=PointerscanDone;

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

procedure Tfrmpointerscanner.miInfoPopupPopup(Sender: TObject);
begin
  miDisconnect.Visible:=(tvinfo.Selected<>nil) and (tvinfo.Selected.Data<>nil);
  miForceDisconnect.Visible:=(tvinfo.Selected<>nil) and (tvinfo.Selected.Data<>nil);
end;

procedure Tfrmpointerscanner.miMergePointerscanResultsClick(Sender: TObject);
begin

end;



procedure Tfrmpointerscanner.miSetWorkFolderClick(Sender: TObject);
begin

end;





procedure Tfrmpointerscanner.FormDestroy(Sender: TObject);
var reg: Tregistry;
begin
  SaveFormPosition(self);

  reg:=tregistry.create;
  if reg.OpenKey('\Software\Cheat Engine\Pointerscan', true) then
  begin
    reg.writeInteger('Display Type', cbtype.itemindex);
    reg.writeBool('Display Signed',miSigned.checked);
    reg.writeBool('Display Hexadecimal',miHexadecimal.checked);
  end;
  reg.free;
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
      else
        scannerpriority:=tpNormal;
    end;

    staticscanner.changeWorkerPriority(scannerpriority);
  end;
end;

procedure Tfrmpointerscanner.cbTestCrappyConnectionChange(Sender: TObject);
begin
  debug_connectionfailure:=cbTestCrappyConnection.checked;
end;

procedure Tfrmpointerscanner.cbNonResponsiveChange(Sender: TObject);
begin
  debug_nonresponsiveconnection:=cbNonResponsive.checked;
end;

procedure Tfrmpointerscanner.cbTypeDropDown(Sender: TObject);
var i: integer;
begin
  //fill in custom types
  while cbtype.Items.Count>8 do
  begin
    cbtype.Items.Delete(8); //delete the ones in the list
  end;

  for i:=0 to customtypes.Count-1 do
    cbtype.Items.AddObject(TcustomType(customtypes[i]).name,customtypes[i]);

  cbtype.DropDownCount:=max(12, cbtype.Items.Count);
end;


procedure Tfrmpointerscanner.FormResize(Sender: TObject);
begin
  btnStopRescanLoop.Left:=(clientwidth div 2) - (btnStopRescanLoop.Width div 2);
end;

procedure Tfrmpointerscanner.FormShow(Sender: TObject);
var i: integer;
begin
  btnIncreaseThreadCount.autosize:=false;
  btnDecreaseThreadCount.autosize:=false;

  i:=max( btnIncreaseThreadCount.width, btnDecreaseThreadCount.width);
  i:=max(i, pnlControl.ClientWidth-2);
  btnIncreaseThreadCount.width:=i;
  btnDecreaseThreadCount.width:=i;

  if loadedFormPosition=false then
  begin
    width:=MainForm.width;
    height:=mainform.height;

    loadedFormPosition:=true;
  end;
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

    for i:=0 to tempfilelist.count-1 do
    begin
      newname:=StringReplace(tempfilelist[i], tempname, oldname+'.results',[]);
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
      totalpathspersecond: double;
      localPathsEvaluated: Qword;
      localpathspersecond: double;
      pointersinmap: qword;
      pathqueuesize: integer;
      pathqueueoverflow: dword;
      resultsfound: qword;
      timeSpentWriting: qword;
      percentageTimeSpentWriting: single;
      minpath, maxpath: TDynDwordArray;
      minpaths, maxpaths: string;

      shownetwork: boolean;
    end;

    scanners: array of record
      status: string;
    end;

    sl: TStringList;
begin
  if lvResults.Visible then
  begin
    lvResults.Update;
    lvResults.repaint;
  end;

  try
    //collect data and then update the treeview
    zeromemory(@statistics, sizeof(statistics));

    statistics.totalTimeScanning:=0;
    statistics.localPathsEvaluated:=0;
    statistics.localpathspersecond:=0;

    if staticscanner<>nil then
    begin
      statistics.shownetwork:=staticscanner.hasNetworkResponsibility;

      if staticscanner.starttime>0 then
        statistics.totalTimeScanning:=GetTickCount64-staticscanner.starttime;

      statistics.totalPathsEvaluated:=staticscanner.totalpathsevaluated;
      statistics.localPathsEvaluated:=staticscanner.localpathsevaluated;

      if statistics.totalTimeScanning>0 then
      begin
        statistics.totalpathspersecond:=(statistics.totalPathsEvaluated / statistics.totalTimeScanning)*1000; //paths / second
        statistics.localPathsPersecond:=(statistics.localPathsEvaluated / statistics.totalTimeScanning)*1000; //paths / second
      end;

      statistics.pointersinmap:=staticscanner.getPointerlistHandlerCount;
      statistics.pathqueuesize:=staticscanner.pathqueuelength;
      statistics.pathqueueoverflow:=length(staticscanner.overflowqueue);
      statistics.resultsfound:=staticscanner.getTotalResultsFound;
      statistics.timeSpentWriting:=ceil((staticscanner.getTotalTimeWriting / 1000) / staticscanner.threadcount); //time spend writing in seconds
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
        infonodes.statistics.node:=tvInfo.Items.Add(nil, rsPSStatistics);

        pointersInMap:=tvInfo.Items.AddChild(infonodes.statistics.node,'');
        totalTimeScanning:=tvInfo.Items.AddChild(infonodes.statistics.node,'');

        localPathsEvaluated:=tvInfo.Items.AddChild(infonodes.statistics.node,'');
        localPathsPerSecond:=tvInfo.Items.AddChild(infonodes.statistics.node,'');


        totalPathsEvaluated:=tvInfo.Items.AddChild(infonodes.statistics.node,'');
        totalPathsPerSecond:=tvInfo.Items.AddChild(infonodes.statistics.node,'');


        pathQueue:=tvInfo.Items.AddChild(infonodes.statistics.node,'');
        resultsFound:=tvInfo.Items.AddChild(infonodes.statistics.node,'');
        timeSpentWriting:=tvInfo.Items.AddChild(infonodes.statistics.node,'');

        minpath:=tvInfo.Items.AddChild(infonodes.statistics.node,'');


        //maxpath:=tvInfo.Items.AddChild(infonodes.statistics.node,'');

        infonodes.statistics.node.Expand(true);
      end;

      pointersInMap.Text:=rsPSUniquePointervaluesInTarget+IntToStr(statistics.pointersinmap);
      totalTimeScanning.Text:=rsPSScanDuration+TimeToStr(TimeStampToDateTime(MSecsToTimeStamp(statistics.totalTimeScanning)));
      localPathsEvaluated.Text:=rsPSPathsEvaluated+IntToStr(statistics.localPathsEvaluated);
      localPathsPerSecond.Text:=format(rsPSPathsSeconds, [statistics.localpathspersecond]);

      totalPathsEvaluated.Text:=rsPSTotalPathsEvaluater+IntToStr(statistics.totalPathsEvaluated);
      totalPathsPerSecond.Text:=format(rsPSTotalPathsSeconds, [statistics.totalpathspersecond]);




      pathQueue.Text:=rsPSStaticQueueSize+inttostr(statistics.pathqueuesize)+rsPSDynamicqueuSize+inttostr(statistics.pathqueueoverflow);
      resultsFound.Text:=rsPSResultsFound+inttostr(statistics.resultsfound);
      timeSpentWriting.Text:=rsPSTimeSpentWriting+inttostr(statistics.timeSpentWriting)+format(' (%.2f %%)', [statistics.percentageTimeSpentWriting]) ;

      minpath.text:=rsPSLowestKnownPath+statistics.minpaths;
     // maxpath.text:='Highest known path:'+statistics.maxpaths;



      totalPathsEvaluated.visible:=statistics.shownetwork;
      totalPathsPerSecond.visible:=statistics.shownetwork;
    end;


    if infonodes.localworkers.node=nil then
      infonodes.localworkers.node:=tvInfo.Items.Add(nil, rsPSThreads);

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
          infonodes.network.node:=tvInfo.Items.Add(nil,rsPSNetwork);

        staticscanner.getConnectingList(connectinglist);

        for i:=length(infonodes.network.connectingToNodes)-1 downto length(connectinglist) do
            infonodes.network.connectingToNodes[i].data.Free;

        setlength(infonodes.network.connectingToNodes, length(connectinglist));
        //connecting to:
        if infonodes.network.connectingto=nil then
          infonodes.network.connectingto:=tvInfo.Items.AddChild(infonodes.network.node,rsPSConnectingTo);




        for i:=0 to length(connectinglist)-1 do
        begin
          if infonodes.network.connectingToNodes[i].data=nil then //create a new one
            infonodes.network.connectingToNodes[i].data:=tvinfo.Items.AddChild(infonodes.network.connectingto,'');

          s:=connectinglist[i].ip+':'+inttostr(connectinglist[i].port);
          if connectinglist[i].becomeparent=false then
            s:=s+BoolToStr(connectinglist[i].trusted, rsPSTrusted,'');

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
            infonodes.network.parent:=tvInfo.Items.AddChild(infonodes.network.node,rsPSParent);

          if parentdata.connected then
          begin
            infonodes.network.parent.Text:=rsPSParent+parentdata.name+'('+parentdata.ip+':'+inttostr(parentdata.port)+')';

            if infonodes.network.parentnodes.lastUpdateSent=nil then
              infonodes.network.parentnodes.lastUpdateSent:=tvInfo.Items.AddChild(infonodes.network.parent,'');

            if staticscanner.downloadingscandata then
            begin
              infonodes.network.parentnodes.lastUpdateSent.Text:=format(rsPSDownloadingScanData, [staticscanner.downloadingscandata_received/staticscanner.downloadingscandata_total*100, staticscanner.downloadingscandata_received div 1024, staticscanner.downloadingscandata_total div 1024, ceil(((staticscanner.downloadingscandata_received / 1024)/((GetTickCount64-staticscanner.downloadingscandata_starttime)/1000)) )]);
            end
            else
            begin
              infonodes.network.parentnodes.lastUpdateSent.Text:=rsPSLastUpdate+inttostr((GetTickCount64-parentdata.lastupdatesent) div 1000)+rsPSSecondsAgo;
            end;




          end
          else
          begin
            //mark that it has no parent (yet/anymore)
            if parentdata.waitingforreconnect then
              infonodes.network.parent.Text:=rsPSParentDisconnectedWaitingForReconnect
            else
              infonodes.network.parent.Text:=rsPSParentNone;

            //if there are nodes, delete them
            if infonodes.network.parentnodes.lastUpdateSent<>nil then
              freeandnil(infonodes.network.parentnodes.lastUpdateSent);
          end;
        end;

        if infonodes.network.connectedTo=nil then
          infonodes.network.connectedTo:=tvInfo.Items.AddChild(infonodes.network.node,rsPSChildren);

        for i:=0 to length(connectionlist)-1 do
        begin
          if infonodes.network.connectedToNodes[i].node=nil then //create it
          begin
            infonodes.network.connectedToNodes[i].node:=tvInfo.Items.AddChild(infonodes.network.connectedTo, s);
            tn:=infonodes.network.connectedToNodes[i].node;

            with infonodes.network.connectedToNodes[i].data do
            begin
              disconnectreason:=tvinfo.items.AddChild(tn, '');
              trusted:=tvInfo.Items.AddChild(tn, '');
              totalthreadcount:=tvInfo.Items.AddChild(tn, '');
              resultsfound:=tvInfo.Items.AddChild(tn, '');
              pathqueuesize:=tvInfo.Items.AddChild(tn, '');
              totalpathquesize:=tvInfo.Items.AddChild(tn, '');
              totalpathsEvaluated:=tvInfo.Items.AddChild(tn, '');
              lastUpdate:=tvInfo.Items.AddChild(tn, '');
             // pathspersecond:=tvInfo.Items.AddChild(tn, '');
            end;
          end;

          s:=connectionlist[i].ip+':'+inttostr(connectionlist[i].port);
          if connectionlist[i].disconnected then
            s:=s+rsPSDisconnected
          else
          if connectionlist[i].queued then
            s:=s+rsPSQueued+inttostr(connectionlist[i].queuepos)+'/'+inttostr(connectionlist[i].queuesize)+')'
          else
          begin
            if connectionlist[i].isidle=false then
              s:=s+rsPSActive
            else
              s:=s+rsPSIdle;

            if connectionlist[i].uploadingscandata then
              s:=s+format(rsPSUploadingScandata, [connectionlist[i].ScanDataSent/connectionlist[i].ScanDataTotalSize*100, connectionlist[i].ScanDataSent div 1024, connectionlist[i].ScanDataTotalSize div 1024, ceil(((connectionlist[i].ScanDataSent / 1024)/((GetTickCount64-connectionlist[i].ScanDataStartTime)/1000)) )]);

            if connectionlist[i].downloadingResuls then
              s:=s+rsPSDownloadingAndHandlingResults;
          end;

          infonodes.network.connectedToNodes[i].node.Text:=s;
          infonodes.network.connectedToNodes[i].node.Data:=pointer(ptruint(connectionlist[i].childid));

          with infonodes.network.connectedToNodes[i].data do
          begin
            disconnectreason.visible:=connectionlist[i].disconnected;
            disconnectreason.Text:=connectionlist[i].lasterror;

            trusted.text:=rsPSTrusted2+BoolToStr(connectionlist[i].trustedconnection, 'True', 'False');
            totalthreadcount.text:=rsPSThreadcount+IntToStr(connectionlist[i].potentialthreadcount)+' ('+IntToStr(connectionlist[i].actualthreadcount)+')';
            resultsfound.text:=rsPSResultsFound+IntToStr(connectionlist[i].resultsfound);
            pathqueuesize.text:=rsPSQueuesize+inttostr(connectionlist[i].pathquesize);
            totalpathquesize.text:=rsPSTotalQueuesize+inttostr(connectionlist[i].totalpathqueuesize);
            totalpathsEvaluated.text:=rsPSPathsEvaluated+inttostr(connectionlist[i].pathsevaluated);
            lastupdate.text:=rsPSLastUpdate+inttostr((GetTickCount64-connectionlist[i].lastUpdateReceived) div 1000);
            //pathspersecond.text:='Paths/second: '+inttostr(connectionlist[i].pathspersecond);
          end;
        end;


      end;


    end;

  except
   // showmessage('exception happened');
    on e:exception do
    begin
      OutputDebugString(rsPSPscanguiUpdateTimerError+e.message);
    end;
  end;

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
   // showmessage(rsOnlyTheFirst1000000EntriesWillBeDisplayed);
  end
  else
    lvResults.Items.Count:=Pointerscanresults.count;


  lvResults.Align:=alClient;
  lvResults.Visible:=true;

  lvResults.Columns.EndUpdate;
  lvResults.Items.EndUpdate;

  cbtype.top:=0;
  cbtype.height:=panel1.ClientHeight;
  cbtype.Visible:=true;

  Rescanmemory1.Enabled:=true;
  new1.Enabled:=true;

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
    vtSingle: result:=(psingle(p)^>=valuescansingle) and (psingle(p)^<=valuescansinglemax);
    vtDouble: result:=(pdouble(p)^>=valuescandouble) and (pdouble(p)^<=valuescandoublemax);
    else
      result:=false;
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

  inherited destroy;
end;

procedure TRescanWorker.execute;
var
    currentEntry: qword;
    i,j,k: integer;

    baseaddress, address,address2, tempaddress: ptrUint;
    pi: TPageInfo;
    x: dword;
    valid: boolean;
    rangeAndStartOffsetsEndOffsets_Valid: boolean;

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
      L:=GetLuaState;

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





          if address>0 then
          begin
            inc(address, offsetBase);
            baseaddress:=address;

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

              rangeAndStartOffsetsEndOffsets_Valid:=valid;

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
                          copymemory(pointer(ptruint(@tempaddress)+k), @pi.data[0], pointersize-k)
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

              //mgr.inz.Player patch:
              //if everything until "evaluate the pointer to address" was fine
              //and user wants all valid (false positive valid) pointers to be removed
              //(so, wants to keep with final address not evaluated), invert valid status.
              //Also, do not check final address readability and do not compare address/value.

              if filterOutAccessible and rangeAndStartOffsetsEndOffsets_Valid then
                valid:=not valid;

              if (not filterOutAccessible) and valid then
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

        inc(evaluated);
        inc(currentEntry);
      end;

      flushresults;
    finally
      freememandnil(tempvalue);

      if tempfile<>nil then
        freeandnil(tempfile);

      if tempbuffer<>nil then
        freeandnil(tempbuffer);

      if l<>nil then
        lua_settop(L, 0);

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

  oldptr: Tmemorystream;

  oldfiles: TStringList;

  ml: Tstringlist;

begin
  progressbar.Min:=0;
  progressbar.Max:=100;
  progressbar.Position:=0;
  result:=nil;


  sleep(delay*1000);
  pointerscanresults:=ownerform.pointerscanresults;
  pointerscanresults.resyncModulelist;

  if forvalue and (valuetype=vtDouble) then valuesize:=8 else valuesize:=4;

  rescanhelper:=TRescanHelper.create;

  if pointermapfilename<>'' then
  begin
    ds:=nil;
    f:=tfilestream.Create(pointermapfilename, fmOpenRead or fmShareDenyNone);
    try
      ds:=Tdecompressionstream.create(f);
      pointermap:=TPointerListHandler.createFromStream(ds, pointermapprogressbar);

      ml:=TStringList.create;
      for i:=0 to pointerscanresults.modulelistCount-1 do
        ml.AddObject(pointerscanresults.getModulename(i), tobject(pointerscanresults.getModuleBase(i)));

      pointermap.reorderModuleIdList(ml);
      ml.free;


    finally
      if ds<>nil then
        freeandnil(ds);

      f.free;

      pointermapprogressbar.position:=100;
    end;
  end;




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
      rescanworkers[i].PointerAddressToFind:=self.address;
      rescanworkers[i].novaluecheck:=novaluecheck;
      rescanworkers[i].filterOutAccessible:=filterOutAccessible;

      rescanworkers[i].forvalue:=forvalue;
      rescanworkers[i].valuesize:=valuesize;
      rescanworkers[i].valuetype:=valuetype;
      rescanworkers[i].valuescandword:=valuescandword;
      rescanworkers[i].valuescansingle:=valuescansingle;
      rescanworkers[i].valuescandouble:=valuescandouble;
      rescanworkers[i].valuescansinglemax:=valuescansinglemax;
      rescanworkers[i].valuescandoublemax:=valuescandoublemax;

      rescanworkers[i].rescanhelper:=rescanhelper;
      rescanworkers[i].filename:=self.filename+'.newresults.'+inttostr(i);

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
      rescanworkers[i].offsetBase:=offsetBase;


      threadhandles[i]:=rescanworkers[i].Handle;
      rescanworkers[i].start;
    end;





    while WaitForMultipleObjects(rescanworkercount, @threadhandles[0], true, 250) = WAIT_TIMEOUT do      //wait
    begin
      //query all threads the number of pointers they have evaluated
      PointersEvaluated:=0;
      for i:=0 to rescanworkercount-1 do
        inc(PointersEvaluated,rescanworkers[i].evaluated);

      progressbar.Position:=PointersEvaluated div (TotalPointersToEvaluate div 100);
    end;

    //no timeout, so finished or crashed

    //destroy workers
    for i:=0 to rescanworkercount-1 do
    begin
      rescanworkers[i].WaitFor; //just to be sure

      if rescanworkers[i].Pointerscanresults<>nil then
        freeandnil(rescanworkers[i].Pointerscanresults);

      rescanworkers[i].Free;
      rescanworkers[i]:=nil;
    end;


    synchronize(closeoldfile);

    oldptr:=tmemorystream.create;
    try
      oldptr.LoadFromFile(originalptrfile);
      oldptr.SaveToFile(filename);
    finally
      oldptr.free;
    end;

    //delete the old files of the destination filename that could conflict
    oldfiles:=tstringlist.create;
    try
      findAllResultFilesForThisPtr(filename, oldfiles);
      for i:=0 to oldfiles.count-1 do
        DeleteFile(oldfiles[i]);
    finally
      oldfiles.free;
    end;



    //rename the newresults to results
    for i:=0 to rescanworkercount-1 do
    begin
      DeleteFile(filename+'.results.'+inttostr(i));  //just to be sure (oldfiles should have cleaned this up)
      RenameFile(filename+'.newresults.'+inttostr(i), filename+'.results.'+inttostr(i));
    end;

    rescanworkercount:=0;
    setlength(rescanworkers,0);

  finally
    if rescanhelper<>nil then
      freeandnil(rescanhelper);

    progressbar.Position:=0;
    postmessage(ownerform.Handle,rescan_done,0,0);
  end;

end;

destructor TRescanpointers.destroy;
begin
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


  try
    if rescanpointerform=nil then
      rescanpointerform:=TFrmRescanPointer.Create(self);

    with rescanpointerform do
    begin

      cbChangeBasePointerOffset.visible:=pointerscanresults.DidBaseRangeScan;
      pnlRangeOffset.visible:=cbChangeBasePointerOffset.visible;

      if pointerscanresults.DidBaseRangeScan then
      begin
        lblOriginalBase.Caption:=inttohex(pointerscanresults.BaseScanRange,8);
        edtNewBase.Text:=inttohex(pointerscanresults.BaseScanRange,8);
      end;


      if ((not rescanpointerform.canceled) and rescanpointerform.cbRepeat.checked) or (showmodal=mrok) then
      begin
        if ((savedialog1.filename<>'') and rescanpointerform.cbRepeat.checked) or savedialog1.Execute then
        begin
          rescan:=trescanpointers.create(true);
          rescan.ownerform:=self;
          rescan.progressbar:=progressbar1;

          rescan.novaluecheck:=cbNoValueCheck.checked;
          rescan.filterOutAccessible:=cbfilterOutAccessible.checked;

          lblProgressbar1.caption:=rsPSREscanning;
          if lblProgressbar1.Height>progressbar1.Height then
           ProgressBar1.Height:=lblProgressbar1.height;

          lblProgressbar1.Top:=progressbar1.Top+(progressbar1.height div 2)-(lblProgressbar1.Height div 2);

          pnlProgress.visible:=true;


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

            if pnlProgressname.clientwidth<rescan.pointermapprogressbarlabel.width then
              pnlProgressName.ClientWidth:=rescan.pointermapprogressbarlabel.width+10;

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

          if (cbNoValueCheck.checked=false) and (cbfilterOutAccessible.checked=false) then
          begin
            if rbFindAddress.Checked then
            begin
              try
                address:=StrToQWordEx('$'+edtAddress.Text);


              //rescan the pointerlist
              except
                raise exception.create(rsPSFindByAddressPart1+edtaddress.text+rsPSFindByAddressPart2);
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

          rescan.offsetBase:=offset;




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
    if staticscanner.initializer=false then
      c:=mryes
    else
    begin
      if not (staticscanner.useheapdata or staticscanner.findValueInsteadOfAddress) then
        c:=MessageDlg(rsPSDoYouWishToResumeTheCurrentPointerscanAtaLaterTime, mtInformation,[mbyes, mbno, mbCancel], 0)
      else
        c:=mrno; //you can't resume scans that do a valuescan or use heapdata
    end;

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
  miResume.enabled:=false;

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

  new1.enabled:=false;

  caption:=rsPointerScan;
end;



procedure Tfrmpointerscanner.FormCreate(Sender: TObject);
var
  x: array of integer;
  reg: tregistry;
begin
  cbtype.Onchange:=nil;
  cbtype.Items.clear;

  cbtype.Items.Add(rs_vtByte);
  cbtype.Items.Add(rs_vtWord);
  cbtype.Items.Add(rs_vtDword);
  cbtype.Items.Add(rs_vtQword);
  cbtype.Items.Add(rs_vtSingle);
  cbtype.Items.Add(rs_vtDouble);
  cbtype.Items.Add(rs_vtString);
  cbtype.Items.Add(rs_vtWidestring);

  cbtype.itemindex:=2;


  {$ifdef cpu64}
    SQLiteLibraryName:='.\win64\sqlite3.dll';
  {$else}
    SQLiteLibraryName:='.\win32\sqlite3.dll';
  {$endif}


  {$ifdef injectedpscan}
  caption:=rsCEInjectedPointerscan;
  {$endif}
  lvResults.DoubleBuffered:=true;

  lvResults.Align:=alClient;
  lvResults.Visible:=true;

  setlength(x,1);
  loadedFormPosition:=loadformposition(self);


  reg:=TRegistry.Create;

  if reg.OpenKey('\Software\Cheat Engine\Pointerscan', false) then
  begin
    if reg.ValueExists('Display Type') then
      cbtype.itemindex:=reg.ReadInteger('Display Type');

    if reg.ValueExists('Display Signed') then
      miSigned.checked:=reg.ReadBool('Display Signed');

    if reg.ValueExists('Display Hexadecimal') then
      miHexadecimal.checked:=reg.readBool('Display Hexadecimal');
  end;

  reg.free;
  cbtype.onchange:=cbTypeChange;
end;

procedure Tfrmpointerscanner.lvResultsData(Sender: TObject;
  Item: TListItem);
var
  p: PPointerscanResult;
  i: integer;
  s: string;
  check: boolean; 
{  doublevalue: double;
  bytevalue: byte absolute doublevalue;
  dwordvalue: dword absolute doublevalue; //make sure of the same memory
  floatvalue: single absolute doublevalue;}
  x: ptruint;

  address: ptrUint;


  vartype: TVariableType;
  ct: TCustomType=nil;

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
      begin
        if p.offsets[i]<0 then
          item.SubItems.Add('-'+inttohex(-p.offsets[i],1))
        else
          item.SubItems.Add(inttohex(p.offsets[i],1));
      end;

      for i:=p.offsetcount to Pointerscanresults.offsetCount-1 do
        item.SubItems.Add('');

      if address=0 then
        item.SubItems.Add('-') else
      begin
        vartype:=vtDword;
        case cbtype.itemindex of
          0: vartype:=vtByte;
          1: vartype:=vtWord;
          2: vartype:=vtDWord;
          3: vartype:=vtQword;
          4: vartype:=vtSingle;
          5: vartype:=vtDouble;
          6: vartype:=vtString;
          7: vartype:=vtUnicodeString;
        end;

        if cbtype.itemindex>=8 then
        begin
          vartype:=vtCustom;
          ct:=TCustomType(cbtype.Items.Objects[cbtype.itemindex]);
        end;

        s:=inttohex(address,8) + ' = ' + readAndParseAddress(address, vartype, ct,miHexadecimal.checked, miSigned.checked, 128);

       {

        if cbType.ItemIndex<>-1 then
        begin
          s:=s+' = ';

          case cbType.ItemIndex of

          end;
          if cbType.ItemIndex in [3,8] then
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
        end;      }

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
  ct: TcustomType;
  ctname: string;
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
        if li.SubItems[i][1]='-' then
          offsets[c]:=-strtoint('$'+copy(li.SubItems[i],2,length(li.SubItems[i])))
        else
          offsets[c]:=strtoint('$'+li.SubItems[i]);

        inc(c);
      end;

      vtype:=vtDword;
      ctname:='';
      case cbtype.itemindex of
        0: vtype:=vtByte;
        1: vtype:=vtWord;
        2: vtype:=vtDWord;
        3: vtype:=vtQword;
        4: vtype:=vtSingle;
        5: vtype:=vtDouble;
      end;

      if cbtype.itemindex>=6 then
      begin
        vtype:=vtCustom;
        ct:=TCustomType(cbtype.Items.Objects[cbtype.itemindex]);
        ctname:=ct.name;
      end;


      mainform.addresslist.addaddress(rsPointerscanResult, t, offsets, c, vtype, ctname);
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


