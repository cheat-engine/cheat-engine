unit pointerscannerfrm;

{$MODE Delphi}

//todo: Make a command prompt version of the distributed scanning pointerscan client, and make it functional in linux as well (real servers)

interface

uses
  windows, LCLIntf, LResources, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, syncobjs,syncobjs2, Menus, math,
  frmRescanPointerUnit, pointervaluelist, rescanhelper,
  virtualmemory, symbolhandler,MainUnit,disassembler,CEFuncProc,NewKernelHandler,
  valuefinder, PointerscanresultReader, maps, zstream, WinSock2, Sockets, resolve,
  registry, PageMap, CELazySocket, PointerscanNetworkCommands;


const staticscanner_done=wm_user+1;
const rescan_done=wm_user+2;
const open_scanner=wm_user+3;
const wm_starttimer=wm_user+4;


type
  TGetScanParametersOut=packed record
    yourID: Int32;
    maxlevel: Uint32;
    structsize: uint32;
    staticonly: Byte;
    noLoop: Byte;
    LimitToMaxOffsetsPerNode: Byte;
    Alligned: Byte;
    DownloadPort: word;
    MaxOffsetsPerNode: UInt16;
    FilenameSize: Uint16;
    Filename: packed record end;
  end;
  PGetScanParametersOut=^TGetScanParametersOut;

  TfrmPointerscanner = class;
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
  end;



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

    function Server_HandleRead(s: Tsocket): byte;

    procedure closeOldFile;
    procedure UpdateStatus(done: boolean; TotalPointersToEvaluate:qword; PointersEvaluated: qword);
    procedure LaunchWorker;
    procedure LaunchServer;
    procedure DoServerLoop;

  public
    ownerform: TFrmPointerScanner;
    progressbar: tprogressbar;
    filename: string;
    originalptrfile: string;
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

    distributedserver: string;
    distributedport: integer;
    distributedrescan: boolean;
    distributedrescanWorker: boolean;
    distributedworkfolder: string;



    procedure execute; override;
    destructor destroy; override;
  end;



  toffsetlist = array of dword;

  TPathQueueElement=record
    tempresults: array of dword;
    valuelist: array of qword;
    valuetofind: qword;
    startlevel: integer;
  end;
  PPathQueueElement=^TPathQueueElement;


  TPathQueueElementArray=array[0..0] of TPathQueueElement;
  PPathQueueElementArray=^TPathQueueElementArray;

  TTransmittedQueueMessage=packed record
    replymessage: byte; //(should be CMDUPDATEREPLY_HEREARESOMEPATHSTOEVALUATE)
    elementcount: byte;
    elements: packed record end;
  end;
  PTransmittedQueueMessage=^TTransmittedQueueMessage;

  TTransmittedQueueMessageClient=packed record
    elementcount: byte;
    elements: packed record end;
  end;
  PTransmittedQueueMessageClient=^TTransmittedQueueMessageClient;


  TStaticscanner = class;

  TReverseScanWorker = class (tthread)
  private
    offsetlist: array of dword;

    results: tmemorystream;
    resultsfile: tfilestream;
    pointersize: integer;

    procedure flushresults;
    procedure rscan(valuetofind:ptrUint; level: valSint);
    procedure StorePath(level: valSint; staticdata: PStaticData);

  public
    ownerform: TFrmPointerscanner;
    valuetofind: ptrUint;
    maxlevel: integer;
    structsize: integer;
//    startaddress: dword;
    startlevel: integer;
    alligned: boolean;
    staticonly: boolean;
    noLoop: boolean;

    LimitToMaxOffsetsPerNode: boolean;
    MaxOffsetsPerNode: integer;



    isdone: boolean;
    hasTerminated: boolean;
    stop: boolean;

    staticscanner: TStaticscanner;
    tempresults: array of dword; //offsetlist
    valuelist: array of ptruint; //used by noLoop  .
    {
    I could have used a map, but inserting in a map takes longer than a array append
    Also, the array is maxlevel big, and usually not that long
    Really not sure what's the best solution in this case though
    }


    //info:
    currentaddress: pointer;
    currentlevel: integer;
    LookingForMin: ptrUint;
    LookingForMax: ptrUint;
    //lastaddress: ptrUint;
    
    filename: string;

    haserror: boolean;
    errorstring: string;

    procedure execute; override;
    constructor create(suspended: boolean);
    destructor destroy; override;
  end;

  TScanDataUploader = class(TThread)
  private
    s: Tsocket;
    f: tfilestream;
    downloaders: array of TSocket;
  public
    procedure execute; override;
    constructor create(filepath: string; port: word);
    destructor destroy; override;
  end;


  TStaticscanner = class(TThread)
  private
    reversescanners: array of treversescanworker;
    pointersize: integer;

    sockethandle: THandle;


    workers: array of record //if server, this will contain a list of connected workers
      s: THandle;
      id: integer;
      threadcount: integer;
      pathsPerSecond: qword;
      pointersfound: qword;
      outofdiskspace: boolean;
      alldone: boolean;
    end;
    myID: integer; //if worker, this will be the ID to identify the generated results, and to reconnect


    scandataUploader: TScandataUploader;

    firsttime: boolean; //For workers. This causes the first update to go without a wait

    broadcastcount: integer;
    lastBroadcast: dword;

    procedure EatFromOverflowQueueIfNeeded;

    procedure launchWorker; //connect to the server
    procedure launchServer; //start listening on the specific port
    procedure broadcastscan; //sends a broadcast to the local network and the potentialWorkerList

    function doDistributedScanningLoop: boolean;  //actually doDistributedScanningLoopIteration
    function doDistributedScanningWorkerLoop: boolean;
    function doDistributedScanningServerLoop: boolean;
    procedure DispatchCommand(s: Tsocket; command: byte);

    function getPathQueueElementSize: integer;
    procedure WritePathQueueElementToMemory(element: PPathQueueElement; var p: pbytearray);
    procedure LoadPathQueueElementFromMemory(element: PPathQueueElement; var p: pbytearray); //returns the next position

    function ismatchtovalue(p: pointer): boolean;  //checks if the pointer points to a value matching the user's input
    procedure reversescan;

  public
    //reverse
    firstaddress: pointer;
    currentaddress: pointer;
    //lastaddress: pointer;

    lookingformin: ptrUint;
    lookingformax: ptrUint;

    reverseScanCS: TCriticalSection;
        
    //reverse^

    ownerform: TfrmPointerscanner;
    
    reverse: boolean;
    automatic: boolean;
    automaticaddress: ptrUint;

    startaddress: ptrUint;
    stopaddress: ptrUint;
    progressbar: TProgressbar;
    sz: integer;
    maxlevel: integer;
    unalligned: boolean;
    codescan: boolean;

    LimitToMaxOffsetsPerNode: boolean;
    MaxOffsetsPerNode: integer; //Sets how many different offsets per node should be handled at most (specifically mentioning different offsets since a pointervalue can have multiple addresses, meaning the same offset, different paths)


    fast: boolean;
    psychotic: boolean;
    writableonly: boolean;
    unallignedbase: boolean;

    useheapdata: boolean;
    useOnlyHeapData: boolean;

    findValueInsteadOfAddress: boolean;
    valuetype: TVariableType;
    valuescandword: dword;
    valuescansingle: single;
    valuescandouble: double;
    valuescansinglemax: single;
    valuescandoublemax: double;


    mustEndWithSpecificOffset: boolean;
    mustendwithoffsetlist: array of dword;
    onlyOneStaticInPath: boolean;
    noReadOnly: boolean;
    mustBeClassPointers: boolean; //when set the pointers must all point to a class object
    noLoop: boolean; //when set a pointerpath may not have the same address multiple times

    useStacks: boolean; //when set the stack regions will be marked as static
    stacksAsStaticOnly: boolean; //when set the only static addresses are stack addresses
    threadstacks: integer; //the number of stacks used as a lookup. (counted from first stack to newer ones)
    stacksize: integer; //Number of bytes in a stack


    threadcount: integer;
    scannerpriority: TThreadPriority;

    filename: string; //the final filename
    phase: integer;


    isdone: boolean;

    staticonly: boolean; //for reverse

    pointersfound: qword;

    hasError: boolean;
    errorString: string;

    LoadedPointermapFilename: string;
    UseLoadedPointermap: boolean;

    pathqueuelength: integer;
    pathqueue: array [0..63] of TPathQueueElement;
    pathqueueCS: TCriticalSection; //critical section used to add/remove entries
    pathqueueSemaphore: THandle; //Event to notify sleeping threads to wake up that there is a new path in the queue

    overflowqueue: array of TPathQueueElement; //this queue will hold a number of paths that the server/worker received too many. (e.g a request for paths was made, but by the time the paths are received, the pathqueue is full again)


    distributedScanning: boolean; //when set to true this will open listening port where other scanners can connect to
    distributedport: word; //port used to listen on if distributed scanning is enabled
    distributedScandataDownloadPort: word;

    distributedWorker: boolean; //set if it's a worker connecting to a server
    distributedServer: string;

    broadcastThisScanner: boolean;
    potentialWorkerList: array of THostAddr;

    workersPathPerSecondTotal: qword;
    workersPointersfoundTotal: qword;

    outofdiskspace: boolean;

    procedure execute; override;
    constructor create(suspended: boolean);
    destructor destroy; override;
  end;

  { Tfrmpointerscanner }

  Tfrmpointerscanner = class(TForm)
    btnStopRescanLoop: TButton;
    Button1: TButton;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    miMergePointerscanResults: TMenuItem;
    miSetWorkFolder: TMenuItem;
    miJoinDistributedScan: TMenuItem;
    miJoinDistributedRescan: TMenuItem;
    ProgressBar1: TProgressBar;
    Panel1: TPanel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    N2: TMenuItem;
    Open1: TMenuItem;
    Pointerscanner1: TMenuItem;
    Method3Fastspeedandaveragememoryusage1: TMenuItem;   //I should probably rename this, it's not really, 'average memory usage' anymore...
    N1: TMenuItem;
    Rescanmemory1: TMenuItem;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Timer2: TTimer;
    pgcPScandata: TPageControl;
    tsPSReverse: TTabSheet;
    tvRSThreads: TTreeView;
    Panel2: TPanel;
    Label5: TLabel;
    lblRSTotalStaticPaths: TLabel;
    Panel3: TPanel;
    btnStopScan: TButton;
    Label6: TLabel;
    ListView1: TListView;
    PopupMenu1: TPopupMenu;
    Resyncmodulelist1: TMenuItem;
    cbType: TComboBox;
    procedure btnStopRescanLoopClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ListView1ColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListView1Resize(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure miMergePointerscanResultsClick(Sender: TObject);
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
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure Resyncmodulelist1Click(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure cbTypeChange(Sender: TObject);
  private
    { Private declarations }
    start:tdatetime;

    rescan: trescanpointers;
    rescanpointerform: TFrmRescanPointer;
    pointerlisthandler: TReversePointerListHandler;   //handled by the form for easy reuse

    distributedworkfolder: string;

    PointerscanListener: TPointerscanListener;

    procedure m_staticscanner_done(var message: tmessage); message staticscanner_done;
    procedure rescandone(var message: tmessage); message rescan_done;
    procedure openscanner(var message: tmessage); message open_scanner;
    procedure _starttimer(var message: TMessage); message wm_starttimer;
    procedure doneui;
    procedure resyncloadedmodulelist;
    procedure OpenPointerfile(filename: string);
  public
    { Public declarations }
    Staticscanner:TStaticScanner;

    Pointerscanresults: TPointerscanresultReader;

    procedure JoinPointerscan(host: string='127.0.0.1'; port: word=52737; threadcount: integer=1; scannerpriority:TThreadPriority=tpHigher; UseLoadedPointermap: boolean=false; LoadedPointermapFilename: string='');
    procedure JoinRescan(server: string; port: dword);
  end;

//var
//  frmPointerScanner: TfrmPointerScanner;


implementation


uses PointerscannerSettingsFrm, frmMemoryAllocHandlerUnit, frmSortPointerlistUnit,
  LuaHandler, lauxlib, lua, frmPointerscanConnectDialogUnit, frmpointerrescanconnectdialogunit;

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
  rsBaseAddress = 'Base Address';
  rsOffset = 'Offset';
  rsPointsTo = 'Points to';
  rsPointercount = 'pointercount';
  rsOnlyTheFirst1000000EntriesWillBeDisplayed = 'Only the first 1000000 '
    +'entries will be displayed. Rescan will still work with all results.  ('
    +'This is completly normal for a pointerscan, you MUST do a few rescans)';
  rsPointerScan = 'Pointer scan';
  rsPointerscanResult = 'pointerscan result';

  rsTerminating = 'Terminating';
  rsStop = 'Stop';

//----------------------- scanner info --------------------------
//----------------------- staticscanner -------------------------


{$ifdef benchmarkps}
var
  totalpathsevaluated: qword;

  starttime: dword;
  startcount: qword;
{$endif}


procedure TFrmpointerscanner.doneui;
begin
  progressbar1.position:=0;
  progressbar1.visible:=false;

  pgcPScandata.Visible:=false;
  open1.Enabled:=true;
  new1.enabled:=true;
  rescanmemory1.Enabled:=true;

  if staticscanner<>nil then
    OpenPointerfile(staticscanner.filename);

  if rescan<>nil then
  begin
    OpenPointerfile(rescan.filename);
    freeandnil(rescan);
  end;

  if (PointerscanListener<>nil) then
    PointerscanListener.executingCommand:=false; //start listening for new commands
end;

procedure Tfrmpointerscanner.m_staticscanner_done(var message: tmessage);
begin
  if staticscanner=nil then exit;

  if staticscanner.useHeapData then
    frmMemoryAllocHandler.memrecCS.leave;  //continue adding new entries

  //update the treeview
  if message.WParam<>0 then
    messagedlg(rsErrorDuringScan+': '+pchar(message.LParam), mtError, [mbok] , 0);

  doneui;
end;

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
    id: byte; //$ce
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

//---------------Reversescanworker
procedure TReverseScanWorker.flushresults;
begin
  resultsfile.WriteBuffer(results.Memory^,results.Position);
  results.Seek(0,sofrombeginning);
 // results.Clear;
end;

constructor TReverseScanWorker.create(suspended:boolean);
begin
  results:=tmemorystream.Create;
  results.SetSize(16*1024*1024);

  isdone:=true;

  pointersize:=processhandler.pointersize;

  inherited create(suspended);
end;

destructor TReverseScanWorker.destroy;
begin
  results.free;
  if resultsfile<>nil then
    freeandnil(resultsfile);
end;

procedure TReverseScanWorker.execute;
var wr: dword;
i: integer;
begin
  try
    try
      resultsfile:= tfilestream.Create(filename,fmcreate);
      resultsfile.free;
      resultsfile:= tfilestream.Create(filename,fmOpenWrite or fmShareDenyNone);

      while (not terminated) and (not self.staticscanner.Terminated) do
      begin
        wr:=WaitForSingleObject(self.staticscanner.pathqueueSemaphore, INFINITE); //obtain semaphore
        if stop then exit;
        if terminated then exit;
        if self.staticscanner.Terminated then exit;

        if wr=WAIT_OBJECT_0 then
        begin
          //fetch the data from the queue and staticscanner
          if staticscanner.outofdiskspace then
          begin
            ReleaseSemaphore(staticscanner.pathqueueSemaphore, 1, nil); //don't use it. give the semaphore back
            sleep(2000);
            continue;
          end;

          self.staticscanner.pathqueueCS.Enter;
          if self.staticscanner.pathqueuelength>0 then
          begin
            dec(staticscanner.pathqueuelength);
            i:=staticscanner.pathqueuelength;
            isdone:=false;
            maxlevel:=staticscanner.maxlevel;
            valuetofind:=staticscanner.pathqueue[i].valuetofind;
            startlevel:=staticscanner.pathqueue[i].startlevel;
            noLoop:=staticscanner.noLoop;
            structsize:=staticscanner.sz;

            CopyMemory(@tempresults[0], @staticscanner.pathqueue[i].tempresults[0], maxlevel*sizeof(dword));
            if noLoop then
              CopyMemory(@valuelist[0], @staticscanner.pathqueue[i].valuelist[0], maxlevel*sizeof(ptruint));
          end;

          self.staticscanner.pathqueueCS.Leave;


          try
            rscan(valuetofind,startlevel);
          finally
            isdone:=true;  //set isdone to true
          end;
        end;
      end;

    except
      on e: exception do
      begin
        OutputDebugString('ScanWorker has error');
        haserror:=true;
        errorstring:='ReverseScanWorker:'+e.message;

        //tell all siblings they should terminate
        staticscanner.reverseScanCS.Enter;
        for i:=0 to length(staticscanner.reversescanners)-1 do
          staticscanner.reversescanners[i].Terminate;

        staticscanner.reverseScanCS.leave;
        terminate;
      end;
    end;
  finally
    isdone:=true;
    hasTerminated:=true;
    OutputDebugString('Scanworker is done');
  end;

end;


var fcount:qword=0;
var scount:qword=0;

procedure TReverseScanWorker.StorePath(level: valSint; staticdata: PStaticData);
{Store the current path to memory and flush if needed}
var i: integer;
begin
  inc(fcount); //increme
  if (staticdata=nil) then exit; //don't store it

  inc(scount);


  //fill in the offset list
  inc(staticscanner.pointersfound);

  results.WriteBuffer(staticdata.moduleindex, sizeof(staticdata.moduleindex));
  results.WriteBuffer(staticdata.offset,sizeof(staticdata.offset));
  i:=level+1; //store how many offsets are actually used (since all are saved)
  results.WriteBuffer(i,sizeof(i));
  results.WriteBuffer(tempresults[0], maxlevel*sizeof(tempresults[0]) ); //todo for 6.3+: Change sizeof(tempresult[0]) with the max size the structsize can generate./ (e.g 4096 is  only 2 bytes, 65536 =3)

  if results.position>15*1024*1024 then //bigger than 15mb
    flushresults;
end;

procedure TReverseScanWorker.rscan(valuetofind:ptrUint; level: valSint);
{
scan through the memory for a address that points in the region of address, if found, recursive call till level maxlevel
}
var p: ^byte;
    pd: ^dword absolute p;
    pq: ^qword absolute p;


    i,j: valSint;
    addedToQueue: boolean;


    ExactOffset: boolean;
    mae: TMemoryAllocEvent;

  startvalue: ptrUint;
  stopvalue: ptrUint;
  plist: PPointerlist;

  nostatic: TStaticData;
  DontGoDeeper: boolean;
  DifferentOffsetsInThisNode: integer;

begin
  if (level>=maxlevel) or (self.staticscanner.Terminated) or (terminated) then exit;



  currentlevel:=level;
  DifferentOffsetsInThisNode:=0;


  exactOffset:=staticscanner.mustEndWithSpecificOffset and (length(staticscanner.mustendwithoffsetlist)-1>=level);

  if exactOffset then
  begin
    startvalue:=valuetofind-staticscanner.mustendwithoffsetlist[level];
    stopvalue:=startvalue;
  end
  else
  begin
    startvalue:=valuetofind-structsize;
    stopvalue:=valuetofind;

    if staticscanner.useheapdata then
    begin
      mae:=frmMemoryAllocHandler.FindAddress(@frmMemoryAllocHandler.HeapBaselevel, valuetofind);
      if mae<>nil then
      begin
        exactoffset:=true;
        startvalue:=mae.BaseAddress;
        stopvalue:=startvalue;
      end
      else //not static and not in heap
       if staticscanner.useOnlyHeapData then
         exit;
    end;
  end;


  if noLoop then
  begin
    //check if this valuetofind is already in the list
    for i:=0 to level-1 do
      if valuelist[i]=valuetofind then
      begin
        exit;
      end;

    //add this valuetofind to the list
    valuelist[level]:=valuetofind;
  end;

  //lastaddress:=maxaddress;

  LookingForMin:=startvalue;
  LookingForMax:=stopvalue;

  dontGoDeeper:=false;
  plist:=nil;
  while stopvalue>=startvalue do
  begin
    if plist=nil then
      plist:=ownerform.pointerlisthandler.findPointerValue(startvalue, stopvalue);

    if plist<>nil then
    begin
      for j:=0 to plist.pos-1 do
      begin
        {$ifdef benchmarkps}
        inc(totalpathsevaluated);
        {$endif}

        tempresults[level]:=valuetofind-stopvalue; //store the offset
        if (plist.list[j].staticdata=nil) then
        begin
          if (not dontGoDeeper) then
          begin
            //check if we should go deeper into these results (not if max level has been reached)


            if (level+1) < maxlevel then
            begin
              addedToQueue:=false;

              if staticscanner.outofdiskspace then //if there is not enough diskspace left wait till it's terminated, or diskspace is freed
              begin
                //!!Out of diskspace!!
                //add to the queue and exit
                while staticscanner.outofdiskspace and (not addedToQueue) do
                begin
                  //try to add it
                  if (not Terminated) and (not self.staticscanner.Terminated) then
                  begin
                    staticscanner.pathqueueCS.Enter;
                    if staticscanner.pathqueuelength<63 then
                    begin
                      //there's room in the queue. Add it

                      CopyMemory(@staticscanner.pathqueue[staticscanner.pathqueuelength].tempresults[0], @tempresults[0], maxlevel*sizeof(dword));
                      if noLoop then
                        CopyMemory(@staticscanner.pathqueue[staticscanner.pathqueuelength].valuelist[0], @valuelist[0], maxlevel*sizeof(ptruint));

                      staticscanner.pathqueue[staticscanner.pathqueuelength].startlevel:=level+1;
                      staticscanner.pathqueue[staticscanner.pathqueuelength].valuetofind:=plist.list[j].address;

                      inc(staticscanner.pathqueuelength);
                      ReleaseSemaphore(staticscanner.pathqueueSemaphore, 1, nil);
                      addedToQueue:=true;
                    end;
                    staticscanner.pathqueueCS.Leave;
                  end
                  else exit; //terminated
                  sleep(500);
                end;

                //^^^^out of diskspace!^^^^
              end
              else
              begin
                if staticscanner.pathqueuelength<63 then //there's room. Add it
                begin
                  if (not Terminated) and (not self.staticscanner.Terminated) then
                  begin


                    staticscanner.pathqueueCS.Enter;
                    if staticscanner.pathqueuelength<63 then
                    begin
                      //still room

                      CopyMemory(@staticscanner.pathqueue[staticscanner.pathqueuelength].tempresults[0], @tempresults[0], maxlevel*sizeof(dword));
                      if noLoop then
                        CopyMemory(@staticscanner.pathqueue[staticscanner.pathqueuelength].valuelist[0], @valuelist[0], maxlevel*sizeof(ptruint));

                      staticscanner.pathqueue[staticscanner.pathqueuelength].startlevel:=level+1;
                      staticscanner.pathqueue[staticscanner.pathqueuelength].valuetofind:=plist.list[j].address;

                      inc(staticscanner.pathqueuelength);
                      ReleaseSemaphore(staticscanner.pathqueueSemaphore, 1, nil);
                      addedToQueue:=true;
                    end;
                    staticscanner.pathqueueCS.Leave;
                  end
                  else
                    exit;
                end;


                if not addedToQueue then
                begin
                  //I'll have to do it myself
                  rscan(plist.list[j].address,level+1);
                end;
              end;
            end
            else
            begin
              //end of the line
              if (not staticonly) then //store this results entry
              begin
                nostatic.moduleindex:=$FFFFFFFF;
                nostatic.offset:=plist.list[j].address;
                StorePath(level,@nostatic);
              end;
            end

          end; //else don't go deeper
        end
        else
        begin
          //found a static one
          StorePath(level, plist.list[j].staticdata);

          if staticscanner.onlyOneStaticInPath then DontGoDeeper:=true;
        end;
      end;


      if LimitToMaxOffsetsPerNode then //check if the current itteration is less than maxOffsetsPerNode
      begin
        inc(DifferentOffsetsInThisNode);

        if (DifferentOffsetsInThisNode>=maxOffsetsPerNode) then
          exit; //the max node has been reached
      end;


      plist:=plist.previous;
      if plist<>nil then
        stopvalue:=plist.pointervalue
      else
        exit; //nothing else to be found

    end else
    begin
      {$ifdef benchmarkps}
      inc(totalpathsevaluated);
      {$endif}
      exit;
    end;

  end;
end;

//--------------------------SCANDATAUPLOADER--------------
procedure TScanDataUploader.execute;
var
  readfds: PFDSet;
  maxfd: integer;
  i,j: integer;
  timeout: TTimeVal;

  client: TSockAddrIn;
  size: integer;

  command: byte;
  datasize: qword;

  gsd: packed record
    offset: qword;
    chunksize: dword;
  end;

  buffer: pointer;
begin
  try
    datasize:=f.Size;

    while not terminated do
    begin
      //wait for the workers

      getmem(readfds, sizeof(PtrUInt)+sizeof(TSocket)*(length(downloaders)+1));
      try
        readfds^.fd_count:=1;
        readfds^.fd_array[0]:=s; //listening socket

        maxfd:=s;
        for i:=0 to length(downloaders)-1 do
        begin
          readfds.fd_array[i+1]:=downloaders[i];
          maxfd:=max(maxfd, downloaders[i]);
          inc(readfds^.fd_count);
        end;

        timeout.tv_sec:=1;
        timeout.tv_usec:=0;
        i:=select(maxfd, readfds, nil, nil, @timeout);
        if i=-1 then
          raise exception.create('Select failed');

        if FD_ISSET(s, readfds^) then
        begin
          FD_CLR(s, readfds^);
          size:=sizeof(client);
          i:=fpaccept(s, @client, @size);
          if i<>INVALID_SOCKET then
          begin
            setlength(downloaders, length(downloaders)+1);
            downloaders[length(downloaders)-1]:=i;
          end;
        end;

        i:=0;
        while i<length(downloaders) do
        begin
          if FD_ISSET(downloaders[i], readfds^) then
          begin
            try
              receive(downloaders[i], @command, sizeof(command));

              case command of
                DCMD_GETSCANDATASIZE: send(downloaders[i], @datasize, sizeof(datasize));
                DCMD_GETSCANDATA:
                begin
                  //this can be done better
                  receive(downloaders[i], @gsd, sizeof(gsd));

                  f.Position:=gsd.offset;

                  if gsd.chunksize=0 then
                    raise TSocketException.create('Assertion failed in DCMD_GETSCANDATA');

                  size:=min(gsd.chunksize, datasize-gsd.offset);
                  getmem(buffer, gsd.chunksize);
                  try
                    f.ReadBuffer(buffer^, size);
                    send(downloaders[i], @size, sizeof(size));
                    send(downloaders[i], buffer, size);
                  finally
                    freemem(buffer);
                  end;
                end
                else
                  raise TSocketException.create('Unknown command');

              end;
            except
              //error/disconnected
              on e: TSocketException do
              begin
                //remove this connection from the list
                CloseSocket(downloaders[i]);

                for j:=i to length(downloaders)-2 do
                  downloaders[i]:=downloaders[i+1];

                setlength(downloaders, length(downloaders)-1);
                continue; //again
              end;
            end;
          end;
          inc(i);
        end;


      finally
        freemem(readfds);
      end;

    end;

  except
    on e: exception do
    begin
      outputdebugstring(pchar('TScanDataUploader error:'+e.message));
    end;
  end;

end;

constructor TScanDataUploader.create(filepath: string; port: word);
var
  B: BOOL;
  i: integer;
  sockaddr: TInetSockAddr;
begin
  f:=Tfilestream.create(filepath, fmopenread);

  s:=socket(AF_INET, SOCK_STREAM, 0);

  if s=INVALID_SOCKET then
    raise Exception.create('Failure creating download socket');

  B:=TRUE;
  fpsetsockopt(s, SOL_SOCKET, SO_REUSEADDR, @B, sizeof(B));


  sockaddr.sin_family:=AF_INET;
  sockaddr.sin_port:=htons(port);
  sockaddr.sin_addr.s_addr:=INADDR_ANY;
  i:=bind(s, @sockaddr, sizeof(sockaddr));

  if i=SOCKET_ERROR then
    raise exception.create('Failure to bind download port '+inttostr(port));

  i:=listen(s, 32);
  if i=SOCKET_ERROR then
    raise exception.create('Failure to listen on download port');

  inherited create(false);
end;

destructor TScanDataUploader.destroy;
begin
  if f<>nil then
    freeandnil(f);

  inherited destroy;
end;

//--------------------------STATICSCANNER------------------

procedure TStaticScanner.LoadPathQueueElementFromMemory(element: PPathQueueElement; var p: pbytearray); //returns the next position
var pos: integer;
begin
  pos:=0;

  if length(element.tempresults)<>maxlevel+1 then
    setlength(element.tempresults, maxlevel+1);


  copymemory(@element.tempresults[0], @p[0], sizeof(dword)*(maxlevel+1));
  inc(pos, sizeof(dword)*(maxlevel+1));
  if noloop then
  begin
    if length(element.valuelist)<>maxlevel+1 then
      setlength(element.valuelist, maxlevel+1);


    copymemory(@element.valuelist[0], @p[pos], sizeof(qword)*(maxlevel+1));
    inc(pos, sizeof(qword)*(maxlevel+1));
  end;

  element.valuetofind:=pqword(@p[pos])^;
  inc(pos, sizeof(qword));
  element.startlevel:=PInteger(@p[pos])^;
  inc(pos, sizeof(integer));

  p:=@p[pos];

{$ifdef XDEBUG}
  assert(pos=getPathQueueElementSize);
{$endif}

end;

procedure TStaticScanner.WritePathQueueElementToMemory(element: PPathQueueElement; var p: pbytearray);
var pos: integer;
begin
{$ifdef XDEBUG}
  assert(element<>nil);
  assert(length(element.tempresults)=maxlevel+1);

  assert((not noloop) or (length(element.tempresults)=maxlevel+1));
{$endif}

  pos:=0;
  copymemory(@p[0], @element.tempresults[0], sizeof(dword)*(maxlevel+1));
  inc(pos, sizeof(dword)*(maxlevel+1));

  if noloop then
  begin
    copymemory(@p[pos], @element.valuelist[0], sizeof(qword)*(maxlevel+1));
    inc(pos, sizeof(qword)*(maxlevel+1));
  end;

  pqword(@p[pos])^:=element.valuetofind;
  inc(pos, sizeof(qword));
  PInteger(@p[pos])^:=element.startlevel;
  inc(pos, sizeof(dword));
  p:=@p[pos];
end;

function TStaticScanner.getPathQueueElementSize: integer;
{
Calculates the size of a single path element when transfered over a stream

definition:
TPathQueueElement=record
  tempresults: array of dword; //will be maxlevel+1 long
  valuelist: array of qword; //will be 0 length if noloop is off, else maxlevel+1. (both server and worker know this)
  valuetofind: qword;
  startlevel: integer;
end;
}
begin
  if noloop then
    result:=sizeof(dword)*(maxlevel+1)+sizeof(qword)*(maxlevel+1)+sizeof(qword)+sizeof(integer)
  else
    result:=sizeof(dword)*(maxlevel+1)+sizeof(qword)+sizeof(integer);
end;

procedure TStaticScanner.EatFromOverflowQueueIfNeeded;
var
  i: integer;
  pathsToCopy: integer;
begin
  if (length(overflowqueue)>0) and (pathqueuelength<63) then //I could use some paths
  begin
    //do I have an overflow I can use ?
    pathqueueCS.enter;
    pathsToCopy:=min(length(overflowqueue), (64-pathqueuelength)); //get the number of paths to transfer from the oveflow queue to the real queue

    for i:=pathqueuelength to pathqueuelength+pathstocopy-1 do
      pathqueue[i]:=overflowqueue[length(overflowqueue)-1-(i-pathqueuelength)];

    inc(pathqueuelength, pathsToCopy);
    ReleaseSemaphore(pathqueueSemaphore, pathsToCopy, nil);
    pathqueueCS.leave;

    setlength(overflowqueue, length(overflowqueue)-pathstocopy);
  end;
end;

procedure TStaticScanner.broadcastscan;
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
  cecommand.operation:=0;   //poinerscan
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

procedure TStaticScanner.launchServer;
var
  B: BOOL;
  i: integer;
  sockaddr: TInetSockAddr;

  s: Tfilestream;
  cs: Tcompressionstream;
begin
  //start listening on the given port.  doDistributedScanningEvent will be responsible for accepting connections
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


  if useLoadedPointermap=false then
  begin
    LoadedPointermapFilename:=self.filename+'.scandata';
    s:=TFileStream.Create(LoadedPointermapFilename, fmCreate);
    try
      cs:=Tcompressionstream.Create(clfastest, s);
      try
        ownerform.pointerlisthandler.exportToStream(cs);
      finally
        cs.free;
      end;
    finally
      s.free;
    end;
  end;

  //do this in a seperate thread as to not cause a bottleneck for actual scanners waiting for paths, and slow downloaders
  scandatauploader:=TScanDataUploader.create(loadedPointermapFilename, distributedScandataDownloadPort);



end;

procedure TStaticScanner.launchWorker;
var
  sockaddr: TInetSockAddr;
  connected: boolean;
  getScanParameters: packed record
    command: byte;
    wantedID: Int32;
    threadcount: UInt32;
  end;

  sp: TGetScanParametersOut;

  fname: pchar;

  hr: THostResolver;

  starttime: dword;

  f: TFileStream;

  buffer: pointer;
  totalsize: qword;
  totaldownloaded: qword;
  chunksize: integer;

  downloadsocket: TSocket;
  command: byte;
  gsd: packed record
    command: byte;
    offset: qword;
    chunksize: dword;
  end;

begin
  //try to connect to the server until it works, or timeouit (60 seconds)
  firsttime:=true;
  sockethandle:=socket(AF_INET, SOCK_STREAM, 0);

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

  //still here, so ask for the scan config
  getScanParameters.command:=CMD_GETSCANPARAMETERS;
  getScanParameters.threadcount:=threadcount;
  getScanParameters.wantedID:=myid;
  send(sockethandle, @getScanParameters, sizeof(getScanParameters));

  //receive the result
  receive(sockethandle, @sp, sizeof(sp));

  getmem(fname, sp.FilenameSize+1);
  try
    receive(sockethandle, fname, sp.FilenameSize);
    fname[sp.FilenameSize]:=#0;

    filename:=ExtractFilePath(filename)+fname;
  finally
    freemem(fname);
  end;

  myID:=sp.yourID;
  maxlevel:=sp.maxlevel;
  sz:=sp.structsize;
  staticonly:=sp.staticonly<>0;
  noLoop:=sp.noLoop<>0;

  LimitToMaxOffsetsPerNode:=sp.LimitToMaxOffsetsPerNode<>0;
  unalligned:=not (sp.Alligned<>0);
  MaxOffsetsPerNode:=sp.MaxOffsetsPerNode;


  if UseLoadedPointermap=false then
  begin
    //download the pointermap from the server
    //connect to sp.DownloadPort and fetch the file
    LoadedPointermapFilename:=filename+'.scandata';

    downloadsocket:=socket(AF_INET, SOCK_STREAM, 0);

    if downloadsocket=INVALID_SOCKET then
      raise Exception.create('Failure creating download socket');

    sockaddr.sin_port:=htons(sp.DownloadPort); //update the port
    //let's assume the host didn't get a different ip address since last connect...


    starttime:=gettickcount;
    connected:=false;
    while (not connected) and (gettickcount<starttime+60000) do
    begin
      connected:=fpconnect(downloadsocket, @SockAddr, sizeof(SockAddr))=0;
      if not connected then sleep(500) else break;
    end;

    if not connected then raise exception.create('Failure connecting to downloadserver. No connection made within 60 seconds');


    f:=TFileStream.Create(LoadedPointermapFilename, fmCreate);

    command:=DCMD_GETSCANDATASIZE;
    send(downloadsocket, @command, sizeof(command));
    receive(downloadsocket, @totalsize, sizeof(totalsize));

    totaldownloaded:=0;
    getmem(buffer, 64*1024);

    while totaldownloaded<totalsize do
    begin
      gsd.command:=DCMD_GETSCANDATA;
      gsd.offset:=totaldownloaded;
      gsd.chunksize:=64*1024;

      try
        send(downloadsocket, @gsd, sizeof(gsd));
        receive(downloadsocket, @chunksize, sizeof(chunksize));
        receive(downloadsocket, buffer, chunksize);

        f.WriteBuffer(buffer^, chunksize);
      except
        on e: TSocketException do
        begin
          CloseSocket(downloadsocket);
          OutputDebugString('Disconnected while downloading. Trying to reconnect');

          //try to reconnect
          connected:=false;
          while (not connected) and (gettickcount<starttime+60000) do
          begin
            connected:=fpconnect(downloadsocket, @SockAddr, sizeof(SockAddr))=0;
            if not connected then sleep(500) else break;
          end;

          if not connected then raise exception.create('Failure reconnecting to downloadserver. No connection made within 60 seconds');
        end;
      end;

      inc(totaldownloaded, chunksize);
    end;

    freemem(buffer);

    f.free;

    UseLoadedPointermap:=true; //in case a reconnect is needed
  end;
end;

function TStaticScanner.doDistributedScanningWorkerLoop: boolean;
var
  updateworkerstatusmessage: packed record
    command: byte;
    pathqueuelength: int32;
    pathsPerSecond: qword;
    pointersfound: qword;
    outofdiskspace: byte;
    alldone: byte;
  end;

  answer: byte;
  elementcount: byte;

  TransmittedQueueMessage: PTransmittedQueueMessageClient;
  i,j: integer;

  buffer: pointer;
  p: PByteArray;
begin
  result:=true; //or CMDUPDATEREPLY_GOKILLYOURSELF is received or it failed to reconnect


  //send state to the server and ask if it has or needs queuedpaths
  if not firsttime then
    sleep(1000+random(2000)); //wait between 1 and 3 seconds before doing something)

  firsttime:=false;
  updateworkerstatusmessage.command:=CMD_UPDATEWORKERSTATUS;
  updateworkerstatusmessage.pathqueuelength:=pathqueuelength;
  updateworkerstatusmessage.pointersfound:=scount;
  updateworkerstatusmessage.pathsPerSecond:=trunc((totalpathsevaluated / (gettickcount-starttime))*1000);
  if outofdiskspace then
    updateworkerstatusmessage.outofdiskspace:=1
  else
    updateworkerstatusmessage.outofdiskspace:=0;



  updateworkerstatusmessage.alldone:=0;
  if pathqueuelength=0 then //could be everything is done
  begin
    pathqueueCS.enter;

    if pathqueuelength=0 then //still 0
    begin
      updateworkerstatusmessage.alldone:=1; //it's now more likely that it's done. But check anyhow
      for i:=0 to length(reversescanners)-1 do
      begin
        if reversescanners[i].isdone=false then
        begin
          updateworkerstatusmessage.alldone:=0;
          break;
        end;
      end;
    end;
    pathqueueCS.Leave;
  end;

  try
    send(sockethandle, @updateworkerstatusmessage, sizeof(updateworkerstatusmessage));

    receive(sockethandle, @answer, sizeof(answer));
    case answer of
      CMDUPDATEREPLY_EVERYTHINGOK: ;  //do nothing

      CMDUPDATEREPLY_HEREARESOMEPATHSTOEVALUATE:
      begin
        //new paths, weeee!
        receive(sockethandle, @elementcount, sizeof(elementcount));

        getmem(buffer, getPathQueueElementSize*elementcount);
        try
          receive(sockethandle, buffer, elementcount*getPathQueueElementSize);

          i:=length(overflowqueue);
          setlength(overflowqueue, length(overflowqueue)+elementcount);


          //load them into the overflow queue
          p:=buffer;
          for j:=i to length(overflowqueue)-1 do
            LoadPathQueueElementFromMemory(@overflowqueue[j], p);

        finally
          freemem(buffer);
        end;


        //and from the overflow queue into the real queue
        EatFromOverflowQueueIfNeeded;
      end;

      CMDUPDATEREPLY_PLEASESENDMESOMEPATHS:
      begin
        //note: This is basically a 1-on-1 copy of the server dispatcher with the exception of no command
        //send about 50% of my queue elements to the server (or max)
        receive(sockethandle, @elementcount, sizeof(elementcount));

        if outofdiskspace then
          elementcount:=length(overflowqueue)+pathqueuelength
        else
          elementcount:=min(elementcount, length(overflowqueue)+pathqueuelength div 2);

        GetMem(TransmittedQueueMessage, 1+getPathQueueElementSize*elementcount);
        TransmittedQueueMessage.elementcount:=0;

        try
          p:=@TransmittedQueueMessage.elements;
          for i:=0 to length(overflowqueue)-1 do
          begin
            WritePathQueueElementToMemory(@overflowqueue[length(overflowqueue)-1-i], p);
            inc(TransmittedQueueMessage.elementcount);

            if TransmittedQueueMessage.elementcount=elementcount then break;
          end;

          setlength(overflowqueue, length(overflowqueue)-TransmittedQueueMessage.elementcount);
          dec(elementcount, TransmittedQueueMessage.elementcount);

          if elementcount>0 then
          begin
            pathqueueCS.enter;
            //get the actual size (must be smaller or equal to the current elementcount)

            elementcount:=min(elementcount, min(32, pathqueuelength div 2));

            //lock the path for as many times as possible (in case the queue suddenly got eaten up completely)
            j:=0;
            for i:=0 to elementcount-1 do
            begin
              if WaitForSingleObject(pathqueueSemaphore, 0)=WAIT_OBJECT_0 then
                inc(j)
              else
                break; //unable to lower the semaphore count
            end;

            elementcount:=j; //the actual number of queue elements that got obtained


            //send from the first elements (tends to be a lower level resulting in more paths)
            for i:=0 to elementcount-1 do
            begin
              WritePathQueueElementToMemory(@pathqueue[i], p);
              inc(TransmittedQueueMessage.elementcount);
            end;

            //move the other queue elements up by elementcount
            for i:=elementcount to pathqueuelength-1 do
              pathqueue[i-elementcount]:=pathqueue[i];

            dec(pathqueuelength, elementcount); //adjust length

            pathqueueCS.leave;
          end;

          send(sockethandle, TransmittedQueueMessage, 1+getPathQueueElementSize*TransmittedQueueMessage.elementcount);

        finally
          freemem(TransmittedQueueMessage);
        end;


      end;

      CMDUPDATEREPLY_GOKILLYOURSELF:
      begin
        result:=false;
      end;
    end;

  except
    on e: TSocketException do
    begin
      try
        //try to reconnect
        launchWorker;
      except
        sockethandle:=-1;
        result:=false;
      end;
    end;
  end;

end;

procedure TStaticScanner.DispatchCommand(s: TSocket; command: byte);
var
  getScanParametersIn: packed record
    wantedID: Int32;
    threadcount: UInt32;
  end;

  getScanParametersOut: PGetScanParametersOut;

  UpdateWorkerStatus: packed record
    pathqueuelength: int32;
    pathsPerSecond: qword;
    pointersFound: qword;
    outofdiskspace: byte;
    alldone: byte;
  end;

  i, j,index: integer;
  found: boolean;


  packetsize: integer;

  TransmittedQueueMessage: PTransmittedQueueMessage;
  RequestQueueMessage: packed record
    replymessage: byte;
    max: byte;
  end;
  elementcount: integer;

  receivedQueueListCount: byte;
  tempqueue: array of TPathQueueElement;

  fname: string;

  everyonedone: boolean;

  p: PByteArray;

  buffer: pointer;

  _workersPathPerSecondTotal: qword;
  _workersPointersfoundTotal: qword;
begin


  //see pointerscancommands.txt
  try

    case command of
      CMD_GETSCANPARAMETERS:
      begin
        //read out the client parameters (wanted ID, threadcount)
        receive(s, @getScanParametersIn, sizeof(getScanParametersIn));


        if getScanParametersIn.wantedID=-1 then
        begin
          //new connection
          index:=length(workers);
          setlength(workers, length(workers)+1);
          workers[index].id:=length(workers)-1;
          workers[index].s:=s;
          workers[index].threadcount:=getScanParametersIn.threadcount;
        end
        else
        begin
          //reconnected
          found:=false;
          for i:=0 to length(workers)-1 do
          begin
            if workers[i].id=getScanParametersIn.wantedID then
            begin
              if workers[i].s<>-1 then //the client disconnected and reconnected before the server saw it
                closehandle(workers[i].s);

              index:=i;
              workers[i].s:=s;
              workers[i].threadcount:=getScanParametersIn.threadcount;
              found:=true;
              break;
            end;
          end;

          if not found then //wtf?
          begin
            OutputDebugString(pchar('A client reconnected with an ID that isn''t in the list: ('+inttostr(getScanParametersIn.wantedID)+')'));
            CloseSocket(s); //ditch it
            exit;
          end;
        end;

        //tell the caller the scanparameters and it's new ID

        fname:=ExtractFileName(filename);

        packetsize:=sizeof(TGetScanParametersOut)+length(fname);
        getmem(getScanParametersOut, packetsize);
        getScanParametersOut.yourID:=index;
        getScanParametersOut.maxlevel:=maxlevel;
        getScanParametersOut.structsize:=sz;
        getScanParametersOut.staticonly:=ifthen(staticonly, 1, 0);
        getScanParametersOut.noLoop:=ifthen(noLoop, 1, 0);
        getScanParametersOut.LimitToMaxOffsetsPerNode:=ifthen(LimitToMaxOffsetsPerNode,1,0);
        getScanParametersOut.Alligned:=ifthen(not self.unalligned,1,0);
        getScanParametersOut.MaxOffsetsPerNode:=MaxOffsetsPerNode;
        getScanParametersOut.DownloadPort:=distributedScandataDownloadPort;
        getScanParametersOut.FilenameSize:=length(fname);
        CopyMemory(@getScanParametersOut.Filename, @fname[1], length(fname));

        send(s, getScanParametersOut, packetsize);
      end;

      CMD_UPDATEWORKERSTATUS:
      begin
        receive(s, @UpdateWorkerStatus, sizeof(UpdateWorkerStatus));

        //update the stats
        _workersPathPerSecondTotal:=0;
        _workersPointersfoundTotal:=0;

        for i:=0 to length(workers)-1 do
        begin
          if workers[i].s=s then
          begin
            workers[i].pathsPerSecond:=UpdateWorkerStatus.pathsPerSecond;
            workers[i].pointersfound:=UpdateWorkerStatus.pointersfound;
            workers[i].outofdiskspace:=UpdateWorkerStatus.outofdiskspace<>0;
            workers[i].alldone:=UpdateWorkerStatus.alldone<>0;
          end;

          inc(_workersPathPerSecondTotal, workers[i].pathsPerSecond);
          inc(_workersPointersfoundTotal, workers[i].pointersfound);
        end;

        workersPathPerSecondTotal:=_workersPathPerSecondTotal;
        workersPointersfoundTotal:=_workersPointersfoundTotal;

        EatFromOverFlowQueueIfNeeded;

        if (UpdateWorkerStatus.outofdiskspace<>0) or //the worker is out of diskspace
           (
            (length(reversescanners)>0) and (
           ((pathqueuelength+length(overflowqueue)<32) and (UpdateWorkerStatus.pathqueuelength>32)) or //Normalize queue
           ((pathqueuelength+length(overflowqueue)<4) and (UpdateWorkerStatus.pathqueuelength>1)))
           ) then
        begin
          //ask the client for his queue elements

          RequestQueueMessage.replymessage:=CMDUPDATEREPLY_PLEASESENDMESOMEPATHS;
          RequestQueueMessage.max:=0;

          if UpdateWorkerStatus.outofdiskspace>0 then
            RequestQueueMessage.max:=UpdateWorkerStatus.pathqueuelength //get all of it (even if I'm out of space as well)
          else
          begin
            if not outofdiskspace then
              RequestQueueMessage.max:=64-(pathqueuelength+length(overflowqueue));
          end;

          if RequestQueueMessage.max>0 then
          begin
            send(s, @RequestQueueMessage, sizeof(RequestQueueMessage));

            //wait for the result

            receive(s, @receivedQueueListCount, sizeof(receivedQueueListCount));

            if receivedQueueListCount>0 then
            begin
              getmem(buffer, receivedQueueListCount*getPathQueueElementSize);
              try
                receive(s, buffer, getPathQueueElementSize*receivedQueueListCount);
                setlength(tempqueue, receivedQueueListCount);

                p:=buffer;
                for i:=0 to receivedQueueListCount-1 do
                  LoadPathQueueElementFromMemory(@tempqueue[i], p);

              finally
                freemem(buffer);
              end;

              j:=length(overflowqueue);
              setlength(overflowqueue, length(overflowqueue)+length(tempqueue));

              for i:=j to j+length(tempqueue)-1 do
                overflowqueue[j+i]:=tempqueue[i];

              EatFromOverflowQueueIfNeeded; //and use what can be used
            end;
          end
          else
          begin
            command:=CMDUPDATEREPLY_EVERYTHINGOK;
            send(s, @command, sizeof(command));
          end;


        end
        else
        if (outofdiskspace) or //i'm out of diskspace
           ((UpdateWorkerStatus.pathqueuelength<32) and (pathqueuelength+length(overflowqueue)>32)) or
           ((UpdateWorkerStatus.pathqueuelength<4) and (pathqueuelength+length(overflowqueue)>=1)) then
        begin
          //send some (about 50%) que elements to the client
          elementcount:=min(32, length(overflowqueue)+(pathqueuelength div 2));
          if elementcount=0 then
            elementcount:=1;

          getmem(TransmittedQueueMessage, 2+getPathQueueElementSize*elementcount);
          TransmittedQueueMessage.replymessage:=CMDUPDATEREPLY_HEREARESOMEPATHSTOEVALUATE;
          TransmittedQueueMessage.elementcount:=0;

          try
            //aquire a the pathqueue lock
            //first copy the overflow queue

            p:=@TransmittedQueueMessage.elements;

            for i:=0 to length(overflowqueue)-1 do
            begin
              WritePathQueueElementToMemory(@overflowqueue[length(overflowqueue)-1-i], p);

              inc(TransmittedQueueMessage.elementcount);
              if TransmittedQueueMessage.elementcount=elementcount then break;
            end;

            setlength(overflowqueue, length(overflowqueue)-TransmittedQueueMessage.elementcount);
            dec(elementcount, TransmittedQueueMessage.elementcount);

            if elementcount>0 then //probably yes
            begin
              pathqueueCS.enter;
              //get the actual size (must be smaller or equal to the current elementcount)

              elementcount:=min(elementcount, min(32, pathqueuelength div 2));
              if elementcount=0 then
                elementcount:=max(pathqueuelength,1);

              //lock the path for as many times as possible (in case the queue suddenly got eaten up completely)
              j:=0;
              for i:=0 to elementcount-1 do
              begin
                if WaitForSingleObject(pathqueueSemaphore, 0)=WAIT_OBJECT_0 then
                  inc(j)
                else
                  break; //unable to lower the semaphore count
              end;

              elementcount:=j; //the actual number of queue elements that got obtained


              //send from the first elements (tends to be a lower level resulting in more paths)
              for i:=0 to elementcount-1 do
              begin
                WritePathQueueElementToMemory(@pathqueue[i], p);
                inc(TransmittedQueueMessage.elementcount);
              end;

              //move the other queue elements up by elementcount
              for i:=elementcount to pathqueuelength-1 do
                pathqueue[i-elementcount]:=pathqueue[i];

              dec(pathqueuelength, elementcount); //adjust length

              pathqueueCS.leave;

            end;

            //transfer the list to the client
            if TransmittedQueueMessage.elementcount>0 then
            begin
              send(s, TransmittedQueueMessage, 2+getPathQueueElementSize*TransmittedQueueMessage.elementcount);

              //mark this worker as not done
              for i:=0 to length(workers)-1 do
                if workers[i].s=s then
                  workers[i].alldone:=false;
            end
            else
            begin
              //I have no que elements to send...
              command:=CMDUPDATEREPLY_EVERYTHINGOK;
              send(s, @command, sizeof(command));
            end;

          finally
            freemem(TransmittedQueueMessage);
          end;
        end
        else
        begin
          //check if all threads are done
          everyonedone:=true;
          for i:=0 to length(workers)-1 do
          begin
            if (workers[i].s<>-1) and (workers[i].alldone=false) then
            begin
              everyonedone:=false;
              break;
            end;
          end;

          if everyonedone then
          begin
            //check if all my own scanners are done
            if pathqueuelength=0 then //the queue seems to be empty
            begin
              pathqueueCS.Enter;
              if pathqueuelength=0 then //it's still 0, so no thread added a new one
              begin
                for i:=0 to length(reversescanners)-1 do
                begin
                  if reversescanners[i].isdone=false then
                  begin
                    everyonedone:=false;  //this thread was not yet done, it may add a new queue element
                    break;
                  end;
                end;
              end;

              pathqueueCS.Leave;

              if everyonedone then
              begin
                command:=CMDUPDATEREPLY_GOKILLYOURSELF;
                send(s, @command, sizeof(command));
                raise TSocketException.Create('No more work for thread');
              end;
            end;



          end;

          //tell the client to go on as usual
          command:=CMDUPDATEREPLY_EVERYTHINGOK;
          send(s, @command, sizeof(command));
        end;



      end;

    end;


  except
    on e: TSocketException do
    begin
      //an socket error happened (read/write error. Disconnect. Done)
      for i:=0 to length(workers)-1 do
        if workers[i].s=s then
        begin
          workers[i].s:=-1; //mark as disconnected
          break;
        end;
      CloseSocket(s);
    end;
  end;
end;

function TStaticScanner.doDistributedScanningServerLoop: boolean;
var
  readfds: PFDSet;
  i,r: integer;
  timeout: TTimeVal;

  client: TSockAddrIn;
  clientsize: integer;
  command: byte;

  maxfd: integer;
begin
  //wait for a status update from any worker, and then either request and send queued paths

  result:=true;

  if broadcastThisScanner and (broadcastcount<10) and (gettickcount>lastBroadcast+1000) then
  begin
    inc(broadcastcount);
    lastbroadcast:=gettickcount;
    broadcastscan;
  end;


  getmem(readfds, sizeof(PtrUInt)+sizeof(TSocket)*(length(workers)+1));
  try

    readfds^.fd_count:=1;
    readfds^.fd_array[0]:=sockethandle;

    maxfd:=sockethandle;
    for i:=1 to length(workers) do
    begin
      if workers[i-1].s<>-1 then //don't add scanners that got disconnected. (they can reconnect)
      begin
        readfds.fd_array[i]:=workers[i-1].s;
        maxfd:=max(maxfd, workers[i-1].s);
        inc(readfds^.fd_count);
      end;
    end;

    timeout.tv_sec:=1;
    timeout.tv_usec:=0;
    i:=select(maxfd, readfds, nil, nil, @timeout);
    if i=-1 then
      raise exception.create('Select failed');

    if i>0 then
    begin
      //at least one is signaled
      if FD_ISSET(sockethandle, readfds^) then
      begin
        //read event on the listening socket (something tries to connect)
        FD_CLR(sockethandle, readfds^);

        clientsize:=sizeof(client);
        i:=fpaccept(sockethandle, @client, @clientsize);
        if i<>INVALID_SOCKET then
        begin
          //wait for the first command (MUST be a "GetScanParameters")
          try
            receive(i, @command, 1);
            if (command=CMD_GETSCANPARAMETERS) then
              DispatchCommand(i, command)
            else
              closehandle(i);
          except
            on e: TSocketException do
            begin
              closehandle(i); //bad connection
            end;
          end;

        end;
      end;


      for i:=0 to length(workers)-1 do //also read from newly created sockets. They always send a message anyhow (getScanParameters)
      begin
        if FD_ISSET(workers[i].s, readfds^) then
        begin
          //handle it
          try
            receive(workers[i].s, @command, 1);
            dispatchCommand(workers[i].s, command);
          except
            on e: TSocketException do
            begin
              CloseSocket(workers[i].s);
              workers[i].s:=-1;
            end;
          end;
        end;
      end;
    end;


  finally
    Freemem(readfds);
  end;
end;


function TStaticScanner.doDistributedScanningLoop: boolean;
begin
  if distributedWorker then
    result:=doDistributedScanningWorkerLoop
  else
    result:=doDistributedScanningServerLoop;
end;

function TStaticScanner.ismatchtovalue(p: pointer): boolean;
begin
  case valuetype of
    vtDword: result:=pdword(p)^=valuescandword;
    vtSingle: result:=(psingle(p)^>=valuescansingle) and (psingle(p)^<valuescansinglemax);
    vtDouble: result:=(pdouble(p)^>=valuescandouble) and (pdouble(p)^<valuescandoublemax);
  end;
end;

procedure TStaticScanner.reversescan;
{
Do a reverse pointer scan
}
var
  i,j: integer;
  alldone: boolean;

  currentaddress: ptrUint;
  addedToQueue: boolean;

  valuefinder: TValueFinder;


begin
  //scan the buffer
  fcount:=0; //debug counter to 0
  scount:=0;
  alldone:=false;

  try
    if maxlevel>0 then
    begin

      if (distributedScanning=false) or (distributedWorker=false) then //don't start the scan if it's a worker system
      begin
        //initialize the first reverse scan worker
        //that one will spawn of all his other siblings if needed

        if Self.findValueInsteadOfAddress then
        begin
          //scan the memory for the value
          ValueFinder:=TValueFinder.create(startaddress,stopaddress);
          ValueFinder.alligned:=not unalligned;
          ValueFinder.valuetype:=valuetype;
          ValueFinder.valuescandword:=valuescandword;
          ValueFinder.valuescansingle:=valuescansingle;
          ValueFinder.valuescandouble:=valuescandouble;
          ValueFinder.valuescansinglemax:=valuescansinglemax;
          ValueFinder.valuescandoublemax:=valuescandoublemax;

          currentaddress:=ptrUint(ValueFinder.FindValue(startaddress));
          while (not terminated) and (currentaddress>0) do
          begin
            //if found, find a idle thread and tell it to look for this address starting from level 0 (like normal)
            addedToQueue:=false;
            while (not terminated) and (not addedToQueue) do
            begin
              if pathqueuelength<63 then //no need to lock
              begin
                pathqueueCS.enter;
                //setup the queueelement
                if pathqueuelength<63 then
                begin
                  pathqueue[pathqueuelength].startlevel:=0;
                  pathqueue[pathqueuelength].valuetofind:=currentaddress;

                  inc(pathqueuelength);
                  ReleaseSemaphore(pathqueueSemaphore, 1, nil);

                  if unalligned then
                    currentaddress:=ValueFinder.FindValue(currentaddress+1)
                  else
                    currentaddress:=ValueFinder.FindValue(currentaddress+pointersize);

                  addedToQueue:=true;
                end;

                pathqueueCS.enter;
              end;

              if (not addedToQueue) and (not terminated) then
                sleep(500); //wait till there is space in the queue
            end;

          end;

          //done with the value finder, wait till all threads are done
          valuefinder.free;
        end
        else
        begin
          //initialize the first thread (it'll spawn new pathqueues)
          pathqueue[pathqueuelength].startlevel:=0;
          pathqueue[pathqueuelength].valuetofind:=self.automaticaddress;
          inc(pathqueuelength);

          starttime:=gettickcount;
          ReleaseSemaphore(pathqueueSemaphore, 1, nil);
        end;

      end;

      //wait till all workers are in isdone state

      if distributedScanning then
      begin
        if not distributedWorker then
          launchServer; //everything is configured now and the scanners are active

        alldone:=not doDistributedScanningLoop;
      end;



      while (not alldone) do
      begin
        outofdiskspace:=getDiskFreeFromPath(filename)<128*1024*1024*length(reversescanners); //128MB for each thread


        if Terminated then
        begin
          OutputDebugString('Forced terminate. Telling the scanworkers to die as well');
          //force the workers to die if they are sleeping
          for i:=0 to length(reversescanners)-1 do
          begin
            reversescanners[i].stop:=true;
            reversescanners[i].Terminate;
          end;

          ReleaseSemaphore(pathqueueSemaphore, 64, nil);

        end;


        EatFromOverflowQueueIfNeeded;

        if distributedScanning then
          alldone:=not doDistributedScanningLoop
        else
          sleep(500);


        if (not alldone) and (pathqueuelength=0) then //it's 0
        begin
          //aquire a lock to see if it's still 0
          pathqueueCS.Enter;
          if pathqueuelength=0 then
          begin //still 0
            alldone:=true;

            if not terminated then
            begin
              for i:=0 to length(reversescanners)-1 do
              begin
                if reversescanners[i].haserror then
                begin

                  OutputDebugString('A worker had an error: '+reversescanners[i].errorstring);

                  haserror:=true;
                  errorstring:=reversescanners[i].errorstring;

                  for j:=0 to length(reversescanners)-1 do reversescanners[j].terminate; //even though the reversescanner already should have done this, let's do it myself as well

                  alldone:=true;
                  break;
                end;

                if not (reversescanners[i].hasTerminated or reversescanners[i].isdone) then //isdone might be enabled
                begin
                  if terminated then
                    OutputDebugString('Worker '+inttostr(i)+' is still active');

                  alldone:=false;
                  break;
                end;



              end;

            end;
          end
          else
            alldone:=false;

          pathqueueCS.Leave;

          if (not terminated) and alldone and distributedScanning then
          begin
            //if this is a distributed scan
            if distributedWorker then
            begin

              if doDistributedScanningLoop then
                alldone:=false; //the server didn't tell me to go kill myself
            end
            else
            begin
              //server scanners are done, check if all the workers are done
              for i:=0 to length(workers)-1 do
                if (workers[i].alldone=false) then
                begin
                  alldone:=false;
                  break; //still some workers active. do not terminate the scan
                end;
            end;
          end;

        end;

      end;


    end;

    isdone:=true;


    //all threads are done
    for i:=0 to length(reversescanners)-1 do
      reversescanners[i].stop:=true;

    ReleaseSemaphore(pathqueueSemaphore, 64, nil);


    for i:=0 to length(reversescanners)-1 do
    begin
      reversescanners[i].WaitFor; //wait till this thread has terminated because the main thread has terminated
      if not haserror then
        reversescanners[i].flushresults;  //write unsaved results to disk

      reversescanners[i].Free;
      reversescanners[i]:=nil;
    end;

    setlength(reversescanners,0);


  finally
    if haserror then
      postmessage(ownerform.Handle,staticscanner_done,1,ptrUint(pchar(errorstring)))
    else
      postmessage(ownerform.Handle,staticscanner_done,0,maxlevel);
  end;

  terminate;
end;

procedure TStaticScanner.execute;
var
    i: integer;

    result: tfilestream;
    temp: dword;
    tempstring: string;

    f: tfilestream;
    ds: Tdecompressionstream;
begin
  if terminated then exit;

  try
    result:=nil;

    if distributedScanning and distributedWorker then
      LaunchWorker; //connects and sets up the parameters

    if distributedScandataDownloadPort=0 then
      distributedScandataDownloadPort:=distributedport+1;

    if ownerform.pointerlisthandler=nil then
    begin
      phase:=1;
      progressbar.Position:=0;
      try
        if useLoadedPointermap then
        begin
          f:=tfilestream.create(LoadedPointermapFilename, fmOpenRead);
          try
            ds:=Tdecompressionstream.create(f);
            try
              ownerform.pointerlisthandler:=TReversePointerListHandler.createFromStream(ds, progressbar);
            finally
              ds.free;
            end;
          finally
            f.free;
          end;
        end
        else
          ownerform.pointerlisthandler:=TReversePointerListHandler.Create(startaddress,stopaddress,not unalligned,progressbar, noreadonly, MustBeClassPointers, useStacks, stacksAsStaticOnly, threadstacks, stacksize);

        postmessage(ownerform.Handle, wm_starttimer, 0,0);


      except
        on e: exception do
        begin
          postmessage(ownerform.Handle,staticscanner_done,1,ptrUint(pchar('Failure copying target process memory'))); //I can just provide this string as it's static in the .code section
          terminate;
          exit;
        end;
      end;
    end; 


    phase:=2;
    progressbar.Position:=0;
  


    i:=0;

    if not (distributedScanning and distributedWorker) then  //not needed anymore if it is a worker (it gets the level from the server, which already execute this)
      maxlevel:=maxlevel-1; //adjust the maxlevel to fix the user input

    //setup the pathqueue
    pathqueuelength:=0;
    pathqueueCS:=TCriticalSection.create;
    pathqueueSemaphore:=CreateSemaphore(nil, 0, 64, nil);

    for i:=0 to 63 do
    begin
      setlength(pathqueue[i].tempresults, maxlevel+1);
      if noLoop then
        setlength(pathqueue[i].valuelist, maxlevel+1);
    end;


    reverseScanCS:=tcriticalsection.Create;
    try
      setlength(reversescanners,threadcount);
      for i:=0 to threadcount-1 do
      begin
        reversescanners[i]:=TReverseScanWorker.Create(true);
        reversescanners[i].ownerform:=ownerform;
        reversescanners[i].Priority:=scannerpriority;
        reversescanners[i].staticscanner:=self;
        setlength(reversescanners[i].tempresults,maxlevel);
        setlength(reversescanners[i].offsetlist,maxlevel);

        if noloop then
          setlength(reversescanners[i].valuelist,maxlevel);

        reversescanners[i].staticonly:=staticonly;
        reversescanners[i].noLoop:=noLoop;

        reversescanners[i].LimitToMaxOffsetsPerNode:=LimitToMaxOffsetsPerNode;
        reversescanners[i].MaxOffsetsPerNode:=MaxOffsetsPerNode;

        reversescanners[i].alligned:=not self.unalligned;
        reversescanners[i].filename:=self.filename+'.'+inttostr(i);

        reversescanners[i].start;
      end;

      //create the headerfile
      result:=TfileStream.create(filename,fmcreate or fmShareDenyWrite);

      //save header (modulelist, and levelsize)
      ownerform.pointerlisthandler.saveModuleListToResults(result);

      //levelsize
      result.Write(maxlevel,sizeof(maxlevel)); //write max level (maxlevel is provided in the message (it could change depending on the settings)

      //pointerstores
      temp:=length(reversescanners);
      result.Write(temp,sizeof(temp));
      for i:=0 to length(reversescanners)-1 do
      begin
        tempstring:=ExtractFileName(reversescanners[i].filename);
        temp:=length(tempstring);
        result.Write(temp,sizeof(temp));
        result.Write(tempstring[1],temp);
      end;

      freeandnil(result);
      reversescan;


      if distributedScanning then
      begin
        //save the number of external workers
        result:=TfileStream.create(filename,fmOpenWrite);
        result.seek(0, soEnd);
        result.writeDword(length(workers)); //0 for a worker (unless I decide to make it a real chaotic mess)

        //save the workerid that generated these results (server=-1)
        result.writeDword(myid);

        freeandnil(result);

      end;

    finally

      if result<>nil then
        freeandnil(result);

      freeandnil(reverseScanCS);

      freeandnil(pathqueueCS);
      closehandle(pathqueueSemaphore);
    end;



  except
    on e: exception do
    begin
      haserror:=true;
      errorstring:='StaticScanner:'+e.message;
      postmessage(ownerform.Handle,staticscanner_done,1,ptrUint(pchar(errorstring)));
      terminate;
    end;
  end;

    
end;

constructor TStaticscanner.create(suspended: boolean);
begin
  pointersize:=processhandler.pointersize;
  myid:=-1;

  reverse:=true;


  inherited create(suspended);
end;

destructor TStaticscanner.destroy;
begin
  terminate;
  waitfor;

  //clean up other stuff
  inherited destroy;
end;

//---------------------------------main--------------------------

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
  progressbar1.visible:=true;

  rescan.start;
end;

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
  listview1.Visible:=false;



  //launch the scanner
  if pointerlisthandler<>nil then
    freeandnil(pointerlisthandler);

  staticscanner:=TStaticscanner.Create(true);
  staticscanner.reverse:=true;

  label5.caption:=rsGeneratingPointermap;
  progressbar1.Visible:=true;

  staticscanner:=TStaticscanner.Create(true);
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


  staticscanner.ownerform:=self;

  open1.Enabled:=false;

  staticscanner.start;

  pgcPScandata.Visible:=true;
end;

procedure Tfrmpointerscanner.miJoinDistributedScanClick(Sender: TObject);
var
  f: tfrmPointerscanConnectDialog;

begin

  f:=tfrmPointerscanConnectDialog.create(self);
  if f.showmodal=mrok then
  begin
    if distributedworkfolder='' then
      miSetWorkFolder.Click;

    if distributedworkfolder='' then exit;

    JoinPointerscan(f.edthost.text, f.port, f.threadcount, f.scannerpriority, f.cbUseLoadedPointermap.checked, f.odLoadPointermap.FileName);
  end;

  f.free;
end;

procedure Tfrmpointerscanner.Method3Fastspeedandaveragememoryusage1Click(
  Sender: TObject);
var
  i: integer;
  floataccuracy: integer;
  floatsettings: TFormatSettings;
begin
  FloatSettings:=DefaultFormatSettings;

  start:=now;
  if frmpointerscannersettings=nil then
    frmpointerscannersettings:=tfrmpointerscannersettings.create(application);

  if frmpointerscannersettings.Visible then exit; //already open, so no need to make again

  {
  if vm<>nil then
    frmpointerscannersettings.cbreuse.Caption:='Reuse memory copy from previous scan';}

  if frmpointerscannersettings.Showmodal=mrok then
  begin
    new1.click;

    starttime:=0;

    if not savedialog1.Execute then exit;

    if (frmpointerscannersettings.cbReusePointermap.checked=false) and (pointerlisthandler<>nil) then
      freeandnil(pointerlisthandler);

    frmpointerscannersettings.cbReusePointermap.enabled:=true; //if it was disabled it's safe to re-enable it now

        
    btnStopScan.enabled:=true;
    btnStopScan.Caption:=rsStop;

    pgcPScandata.Visible:=false;
    open1.Enabled:=false;
    new1.enabled:=false;
    rescanmemory1.Enabled:=false;

    cbType.Visible:=false;
    listview1.Visible:=false;




    //initialize array's




    //default scan
    staticscanner:=TStaticscanner.Create(true);

    label5.caption:=rsGeneratingPointermap;
    progressbar1.Visible:=true;

    try
      staticscanner.ownerform:=self;
      staticscanner.filename:=utf8toansi(savedialog1.FileName);
      staticscanner.reverse:=true; //since 5.6 this is always true

      staticscanner.noReadOnly:=frmpointerscannersettings.cbNoReadOnly.checked;
      staticscanner.mustBeClassPointers:=frmpointerscannersettings.cbClassPointersOnly.checked;

      staticscanner.useStacks:=frmpointerscannersettings.cbStaticStacks.checked;
      staticscanner.stacksAsStaticOnly:=frmPointerscannersettings.cbStackOnly.checked;
      staticscanner.threadstacks:=frmPointerscannersettings.threadstacks;
      staticscanner.stacksize:=frmPointerscannersettings.stacksize;

      staticscanner.UseLoadedPointermap:=frmpointerscannersettings.cbUseLoadedPointermap.Checked;
      staticscanner.LoadedPointermapFilename:=frmpointerscannersettings.odLoadPointermap.FileName;


      staticscanner.startaddress:=frmpointerscannersettings.start;
      staticscanner.stopaddress:=frmpointerscannersettings.Stop;

      staticscanner.unalligned:=not frmpointerscannersettings.CbAlligned.checked;
      pgcPScandata.ActivePage:=tsPSReverse;
      tvRSThreads.Items.Clear;


      staticscanner.codescan:=frmpointerscannersettings.codescan;
      staticscanner.staticonly:=frmpointerscannersettings.cbStaticOnly.checked;
      staticscanner.noLoop:=frmpointerscannersettings.cbNoLoop.checked;
      staticscanner.LimitToMaxOffsetsPerNode:=frmpointerscannersettings.cbMaxOffsetsPerNode.Checked;
      staticscanner.maxOffsetsPerNode:=frmpointerscannersettings.maxOffsetsPerNode;


      staticscanner.automatic:=true;

      staticscanner.automaticaddress:=frmpointerscannersettings.automaticaddress;
      staticscanner.sz:=frmpointerscannersettings.structsize;
      staticscanner.maxlevel:=frmpointerscannersettings.maxlevel;


      staticscanner.progressbar:=progressbar1;
      staticscanner.threadcount:=frmpointerscannersettings.threadcount;
      staticscanner.scannerpriority:=frmpointerscannersettings.scannerpriority;

      staticscanner.distributedScanning:=frmpointerscannersettings.cbDistributedScanning.checked;
      staticscanner.distributedport:=frmpointerscannersettings.distributedPort;

      staticscanner.broadcastThisScanner:=frmpointerscannersettings.cbBroadcast.checked;
      staticscanner.potentialWorkerList:=frmpointerscannersettings.resolvediplist;


      staticscanner.mustEndWithSpecificOffset:=frmpointerscannersettings.cbMustEndWithSpecificOffset.checked;
      if staticscanner.mustEndWithSpecificOffset then
      begin
        setlength(staticscanner.mustendwithoffsetlist, frmpointerscannersettings.offsetlist.count);
        for i:=0 to frmpointerscannersettings.offsetlist.count-1 do
          staticscanner.mustendwithoffsetlist[i]:=TOffsetEntry(frmpointerscannersettings.offsetlist[i]).offset;
      end;



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
        floataccuracy:=pos(FloatSettings.DecimalSeparator,frmpointerscannersettings.edtAddress.Text);
        if floataccuracy>0 then
          floataccuracy:=length(frmpointerscannersettings.edtAddress.Text)-floataccuracy;

        case frmpointerscannersettings.cbValueType.ItemIndex of
          0:
          begin
            staticscanner.valuetype:=vtDword;
            val(frmpointerscannersettings.edtAddress.Text, staticscanner.valuescandword, i);
            if i>0 then raise exception.Create(Format(rsIsNotAValid4ByteValue, [frmpointerscannersettings.edtAddress.Text]));
          end;

          1:
          begin
            staticscanner.valuetype:=vtSingle;
            val(frmpointerscannersettings.edtAddress.Text, staticscanner.valuescansingle, i);
            if i>0 then raise exception.Create(Format(rsIsNotAValidFloatingPointValue, [frmpointerscannersettings.edtAddress.Text]));
            staticscanner.valuescansingleMax:=staticscanner.valuescansingle+(1/(power(10,floataccuracy)));
          end;

          2:
          begin
            staticscanner.valuetype:=vtDouble;
            val(frmpointerscannersettings.edtAddress.Text, staticscanner.valuescandouble, i);
            if i>0 then raise exception.Create(Format(rsIsNotAValidDoubleValue, [frmpointerscannersettings.edtAddress.Text]));
            staticscanner.valuescandoubleMax:=staticscanner.valuescandouble+(1/(power(10,floataccuracy)));            
          end;
        end;
      end;


      progressbar1.Max:=staticscanner.stopaddress-staticscanner.startaddress;


      open1.Enabled:=false;
      staticscanner.start;


      pgcPScandata.Visible:=true;

      Method3Fastspeedandaveragememoryusage1.Enabled:=false;
    except
      on e: exception do
      begin
        staticscanner.Free;
        staticscanner:=nil;
        raise exception.create(e.message);
      end;
    end;

  end;
end;

procedure Tfrmpointerscanner.ListView1Resize(Sender: TObject);
var i,l: integer;
begin
  if listview1.Columns.Count>0 then
  begin
    l:=0;
    for i:=0 to listview1.columns.count-2 do
      inc(l,listview1.Columns[i].Width);

    l:=listview1.ClientWidth-l;
    l:=max(120,l);
    listview1.Columns[listview1.columns.count-1].Width:=l;
  end;
end;

procedure Tfrmpointerscanner.MenuItem3Click(Sender: TObject);
begin
  //start a listener for pointerscan related signals
  if distributedworkfolder='' then
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



end;

procedure Tfrmpointerscanner.miMergePointerscanResultsClick(Sender: TObject);
begin

end;

procedure Tfrmpointerscanner.miSetWorkFolderClick(Sender: TObject);
var reg: Tregistry;
begin

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

  end;
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

procedure Tfrmpointerscanner.Button1Click(Sender: TObject);
var f: tfilestream;

  c: Tcompressionstream;
begin
  f:=tfilestream.create(Staticscanner.filename+'.scandata', fmCreate);

  c:=Tcompressionstream.Create(clfastest, f);
  pointerlisthandler.exportToStream(c);

  c.free;
  f.free;

  button1.visible:=false;
end;

procedure Tfrmpointerscanner.FormResize(Sender: TObject);
begin
  btnStopRescanLoop.Left:=(clientwidth div 2) - (btnStopRescanLoop.Width div 2);
end;

procedure Tfrmpointerscanner.ListView1ColumnClick(Sender: TObject; Column: TListColumn);
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
  if c=listview1.ColumnCount-1 then exit; //raise exception.create('The result/value list is unsortable');
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
begin
  if listview1.Visible then
    listview1.repaint;

  if pointerlisthandler<>nil then
  begin
    if staticscanner<>nil then
      i:=staticscanner.pathqueuelength
    else
      i:=0;

    s:=rsAddressSpecifiersFoundInTheWholeProcess+':'+inttostr(pointerlisthandler.count)+'  (pathqueue: '+inttostr(i)+')';
    label6.caption:=s;
  end;

  if staticscanner<>nil then
  try
    if staticscanner.isdone then
    begin
      if tvRSThreads.Items.Count>0 then
        tvRSThreads.Items.Clear;

      exit;
    end;

    if staticscanner.reverse then
    begin
      s:=format(rsPointerPathsFound+': %d', [scount]);

      if staticscanner.distributedScanning and (staticscanner.distributedWorker=false) then
      begin
        x:=scount+ staticscanner.workersPointersfoundTotal;

        s:=s+' ('+inttostr(x)+')';
      end;
      lblRSTotalStaticPaths.caption:=s;

{$ifdef benchmarkps}
      if (starttime=0) and (totalpathsevaluated<>0) then
      begin
        startcount:=totalpathsevaluated;  //get the count from this point on
        starttime:=gettickcount;
      end;

      s:=format(rsThreads+': '+rsEvaluated+': %d '+rsTime+': %d  (%d / s)', [totalpathsevaluated-startcount, ((gettickcount-starttime) div 1000), trunc(((totalpathsevaluated-startcount)/(gettickcount-starttime))*1000)]);

      if staticscanner.distributedScanning and (staticscanner.distributedWorker=false) then
      begin
        x:=trunc(((totalpathsevaluated-startcount)/(gettickcount-starttime))*1000)+staticscanner.workersPathPerSecondTotal;
        s:=s+' (Total: '+inttostr(x)+' / s)';
      end;
      label5.caption:=s;
      label5.Width:=label5.Canvas.TextWidth(label5.caption);
{$endif}


      if tvRSThreads.Items.Count<length(staticscanner.reversescanners) then
      begin
        //add them

        for i:=0 to length(staticscanner.reversescanners)-1 do
        begin
          tn:=tvRSThreads.Items.Add(nil, rsThread+' '+inttostr(i+1));
          tvRSThreads.Items.AddChild(tn, rsCurrentLevel+':0');
          tvRSThreads.Items.AddChild(tn, rsLookingFor+' :0-0');
        end;
      end;

      tn:=tvRSThreads.Items.GetFirstNode;
      i:=0;
      while tn<>nil do
      begin
        if tn.Data<>nil then break; //worker instead of thread

        if staticscanner.reversescanners[i].isdone then
        begin
          tn.Text:=rsThread+' '+inttostr(i+1)+' ('+rsSleeping+')';
          tn2:=tn.getFirstChild;
          tn2.text:=rsSleeping;
          tn2:=tn2.getNextSibling;
          tn2.text:=rsSleeping;
        end
        else
        begin
          tn.text:=rsThread+' '+inttostr(i+1)+' ('+rsActive+')';
          tn2:=tn.getFirstChild;

          begin
            s:='';
            for j:=0 to staticscanner.reversescanners[i].currentlevel-1 do
              s:=s+' '+inttohex(staticscanner.reversescanners[i].tempresults[j],8);


            tn2.text:=rsCurrentLevel+':'+inttostr(staticscanner.reversescanners[i].currentlevel)+' ('+s+')';
            tn2:=tn2.getNextSibling;
            tn2.text:=rsLookingFor+' :'+inttohex(staticscanner.reversescanners[i].lookingformin, 8)+'-'+inttohex(staticscanner.reversescanners[i].lookingformax, 8);
          end;
        end;

        tn:=tn.getNextSibling;
        inc(i);
      end;


      if staticscanner.distributedScanning and (staticscanner.distributedWorker=false) then
      begin

        if length(Staticscanner.workers)>0 then
        begin
          //add/update workers
          tn:=tvRSThreads.Items.GetFirstNode; //find first worker entry
          while tn<>nil do
          begin
            if tn.Data<>nil then break;
            tn:=tn.GetNextSibling;
          end;


          for i:=0 to length(Staticscanner.workers)-1 do
          begin
            if tn=nil then //create this one
            begin
              tn:=tvRSThreads.Items.AddChild(nil, 'Worker :'+inttostr(i));
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
    end
    else
    begin

    end;


  except

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

  listview1.Items.BeginUpdate;
  listview1.Columns.BeginUpdate;  
  listview1.Items.Count:=0;
  listview1.Columns.Clear;

  col_baseaddress:=listview1.Columns.Add;
  col_baseaddress.Caption:=rsBaseAddress;
  col_baseaddress.Width:=150;
  col_baseaddress.MinWidth:=20;

  setlength(col_offsets, Pointerscanresults.offsetCount);
  for i:=0 to Pointerscanresults.offsetCount-1 do
  begin
    col_offsets[i]:=listview1.Columns.Add;
    col_offsets[i].Caption:=rsOffset+' '+inttostr(i);
    col_offsets[i].Width:=80;
    col_offsets[i].MinWidth:=10;
  end;

  col_pointsto:=listview1.Columns.Add;
  col_pointsto.Caption:=rsPointsTo+':';
  col_pointsto.Width:=120;
  col_pointsto.MinWidth:=10;
  col_pointsto.AutoSize:=true;




  panel1.Caption:=rsPointercount+':'+inttostr(Pointerscanresults.count);

  if ((Staticscanner=nil) or (staticscanner.distributedScanning=false)) and (Pointerscanresults.count>1000000) then
  begin
    listview1.Items.Count:=1000000;
    showmessage(rsOnlyTheFirst1000000EntriesWillBeDisplayed);
  end else listview1.Items.Count:=Pointerscanresults.count;

  listview1.Align:=alClient;
  listview1.Visible:=true;

  listview1.Columns.EndUpdate;
  listview1.Items.EndUpdate;

  cbtype.top:=0;
  cbtype.height:=panel1.ClientHeight;
  cbtype.Visible:=true;

  Rescanmemory1.Enabled:=true;
  new1.Enabled:=true;

  caption:=rsPointerScan+' : '+extractfilename(filename);
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

    temppage: pointer;


    mr: TMemoryRegion;
begin
  l:=nil;

  try

  getmem(temppage, 4096);
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
      p:=Pointerscanresults.getPointer(currentEntry);
      if p<>nil then
      begin
        valid:=true;
        if p.modulenr=-1 then
          address:=p.moduleoffset
        else
          address:=Pointerscanresults.getModuleBase(p.modulenr)+p.moduleoffset;

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
              tempbuffer.Write(p^,Pointerscanresults.entrySize);
              if tempbuffer.Position>16*1024*1024 then flushresults;
            end;


          end; //must be in range and it wasn't in the range
        end; //else not a valid module
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
          workers[newworkerid].s:=s
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
  i: integer;
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
    workers[i].s:=-1; //mark as disconnected
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
      if rescanworkers[i].done=false then
        alldone:=false;

      PointersEvaluated:=PointersEvaluated+ rescanworkers[i].evaluated;
    end;

    //update the gui
    progressbar.Position:=trunc(PointersEvaluated / (TotalPointersToEvaluate / 100));
  end;



end;

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

begin
  progressbar.Min:=0;
  progressbar.Max:=100;
  progressbar.Position:=0;
  result:=nil;

  sockethandle:=INVALID_SOCKET;


  if distributedrescan and distributedrescanWorker then
    launchworker
  else
  begin
    sleep(delay*1000);
    pointerscanresults:=ownerform.pointerscanresults;
    pointerscanresults.resyncModulelist;
  end;


  if forvalue and (valuetype=vtDouble) then valuesize:=8 else valuesize:=4;

  rescanhelper:=TRescanHelper.create(sockethandle, sockethandlecs);



  //fill the modulelist with baseaddresses
  try
    //the modulelist now holds the baseaddresses (0 if otherwhise)
    TotalPointersToEvaluate:=pointerscanresults.count;


    //spawn all threads
    rescanworkercount:=GetCPUCount;
    if HasHyperthreading then rescanworkercount:=(rescanworkercount div 2)+1;

//    rescanworkercount:=1;   //only one for now. Todo: Make this multithreaded

    blocksize:=TotalPointersToEvaluate div rescanworkercount;
    if blocksize<8 then blocksize:=8;

    setlength(rescanworkers, rescanworkercount);
    setlength(threadhandles, rescanworkercount);
    for i:=0 to rescanworkercount-1 do
    begin
      rescanworkers[i]:=TRescanWorker.Create(true);


      rescanworkers[i].Pointerscanresults:=TPointerscanresultReader.create(originalptrfile, pointerscanresults);
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


    result.writedword(pointerscanresults.externalScanners);
    result.writedword(Pointerscanresults.generatedByWorkerID);

    result.Free;

    if distributedrescan and (not distributedrescanWorker) then
    begin
      launchServer;
      DoServerLoop;
    end
    else
    begin
      while WaitForMultipleObjects(rescanworkercount, @threadhandles[0], true, 250) = WAIT_TIMEOUT do      //wait
      begin
        //query all threads the number of pointers they have evaluated
        PointersEvaluated:=0;
        for i:=0 to rescanworkercount-1 do
          inc(PointersEvaluated,rescanworkers[i].evaluated);

        progressbar.Position:=PointersEvaluated div (TotalPointersToEvaluate div 100);

        if distributedrescan and distributedrescanWorker then
          UpdateStatus(false, TotalPointersToEvaluate, PointersEvaluated);
      end;
    end;
    //no timeout, so finished or crashed

    if distributedrescan and distributedrescanWorker then
      UpdateStatus(true, TotalPointersToEvaluate, PointersEvaluated);


    if overwrite then //delete the old ptr file
    begin
      if distributedrescan and distributedrescanWorker then
        freeandnil(Pointerscanresults);

      synchronize(closeoldfile);

      DeleteFile(filename);
      RenameFile(filename+'.overwrite',filename);
    end;

    //destroy workers
    for i:=0 to rescanworkercount-1 do
    begin
      rescanworkers[i].WaitFor; //just to be sure
      rescanworkers[i].Free;

      if overwrite then
      begin
        DeleteFile(filename+'.'+inttostr(i));
        RenameFile(filename+'.'+inttostr(i)+'.overwrite', filename+'.'+inttostr(i));
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

  if distributedrescanWorker and (Pointerscanresults<>nil) then
    freeandnil(Pointerscanresults);

  inherited destroy;
end;



procedure Tfrmpointerscanner.miJoinDistributedRescanClick(Sender: TObject);
var f: tfrmPointerrescanConnectDialog;
begin
  f:=tfrmPointerrescanConnectDialog.create(self);
  if f.showmodal=mrok then
  begin
    //create a rescanpointers object
    if distributedworkfolder='' then
      miSetWorkFolder.Click;

    if distributedworkfolder='' then exit;

    JoinRescan(f.edtHost.text, f.port);
  end;
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
  progressbar1.visible:=true;



  try
    if rescanpointerform=nil then
      rescanpointerform:=TFrmRescanPointer.Create(self);

    with rescanpointerform do
    begin
      cbDistributedRescan.visible:=Pointerscanresults.externalScanners>0;
      edtRescanPort.Visible:=Pointerscanresults.externalScanners>0;


      if cbDistributedRescan.visible then
        cbDistributedRescan.OnChange(cbDistributedRescan);



      if (rescanpointerform.cbRepeat.checked) or (showmodal=mrok) then
      begin
        if (rescanpointerform.cbRepeat.checked) or savedialog1.Execute then
        begin
          rescan.novaluecheck:=cbNoValueCheck.checked;

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
            setlength(rescan.startOffsetValues,0); //shouldn't be necesary, but just in case


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

          if (Pointerscanresults.externalScanners>0) and (cbDistributedRescan.checked) then
          begin
            rescan.distributedport:=distributedport;
            rescan.distributedrescan:=true;
            rescan.distributedrescanWorker:=false;
          end;


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

procedure Tfrmpointerscanner.btnStopScanClick(Sender: TObject);
begin
  if staticscanner<>nil then
  begin
    btnStopScan.Caption:=rsTerminating;
    btnStopScan.enabled:=false;
    staticscanner.Terminate;
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

  if pointerlisthandler<>nil then
    freeandnil(pointerlisthandler);


  action:=cafree; //on close free itself
end;

procedure Tfrmpointerscanner.openscanner(var message: tmessage);
begin
  if frmpointerscannersettings=nil then
    frmpointerscannersettings:=tfrmpointerscannersettings.create(application);

  frmpointerscannersettings.edtAddress.text:=inttohex(message.WParam,8);
  Method3Fastspeedandaveragememoryusage1.Click;
end;

procedure Tfrmpointerscanner._starttimer(var message: TMessage);
begin
  ProgressBar1.Visible:=false;
  timer2.enabled:=true;
end;


procedure Tfrmpointerscanner.New1Click(Sender: TObject);
begin
  btnStopScan.click;

  if staticscanner<>nil then
    freeandnil(staticscanner);

 
  pgcPScandata.Visible:=false;
  panel1.Caption:='';
  open1.Enabled:=true;
  new1.enabled:=true;
  rescanmemory1.Enabled:=false;

  listview1.Items.BeginUpdate;
  listview1.columns.BeginUpdate;
  listview1.Columns.Clear;
  listview1.Items.Count:=0;

  listview1.Items.EndUpdate;
  listview1.Columns.EndUpdate;

  Method3Fastspeedandaveragememoryusage1.Enabled:=true;

  if Pointerscanresults<>nil then
    freeandnil(Pointerscanresults);

end;



procedure Tfrmpointerscanner.FormCreate(Sender: TObject);
var
  x: array of integer;
  reg: tregistry;
begin
  tsPSReverse.TabVisible:=false;

  {$ifdef injectedpscan}
  caption:='CE Injected Pointerscan';
  {$endif}
  listview1.DoubleBuffered:=true;

  listview1.Align:=alClient;
  listview1.Visible:=true;

  setlength(x,1);
  if loadformposition(self,x) then
    cbtype.itemindex:=x[0];

  reg:=TRegistry.Create;

  if Reg.OpenKey('\Software\Cheat Engine',false) then
  begin
    if reg.ValueExists('PointerScanWorkFolder') then
    begin
      distributedworkfolder:=IncludeTrailingPathDelimiter(reg.ReadString('PointerScanWorkFolder'));
      SelectDirectoryDialog1.filename:=distributedworkfolder;
    end;
  end;

  reg.free;
end;

procedure Tfrmpointerscanner.ListView1Data(Sender: TObject;
  Item: TListItem);
var
  p: PPointerscanResult;
  i: integer;
  s: string;
  check: boolean; 
  doublevalue: double;
  dwordvalue: dword absolute doublevalue; //make sure of the same memory
  floatvalue: single absolute doublevalue;
  x: dword;

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
  listview1.Refresh;
end;

procedure Tfrmpointerscanner.ListView1DblClick(Sender: TObject);
var
  li: tlistitem;
  i: integer;
  offsets: array of integer;
  t: string;
  c: integer;

  vtype: TVariableType;
begin
  if listview1.ItemIndex<>-1 then
  begin
    li:=listview1.Items.Item[listview1.ItemIndex];
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
  listview1.Refresh;

end;

initialization
  {$i pointerscannerfrm.lrs}

end.


