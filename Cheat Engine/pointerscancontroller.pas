unit PointerscanController;

{$mode delphi}

interface

uses
  {$ifdef darwin}macport,{$endif}
  {$ifdef windows}windows,{$endif}
  LCLIntf, LCLType, Classes, SysUtils, StdCtrls, ComCtrls, Sockets, syncobjs,
  resolve, math, pointervaluelist,PointerscanWorker, PointerscanStructures,
  pointeraddresslist, PointerscanresultReader, cefuncproc, NewKernelHandler,
  zstream, PointerscanConnector, PointerscanNetworkStructures, {$ifdef windows}WinSock2,{$endif}
  CELazySocket, AsyncTimer, MemoryStreamReader, commonTypeDefs, NullStream, SyncObjs2;


type
  TGetScanParametersOut=packed record
    yourID: Int32;
    maxlevel: Uint32;
    structsize: uint32;
    compressedptr: Byte;
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

  TPointerscanController=class;

  TPointerlistloader=class(tthread)
  private
  public
    filename: string;
    memoryfilestream: TMemoryStream;

    progressbar: TProgressbar;
    pointerlisthandler: TPointerListHandler;
    procedure execute; override;
  end;


  TScanResultDownloader = class (TThread) //class for receiving scan results from a child
  private
    fcontroller: TPointerscanController;
    fChildID: integer;
  public
    procedure execute; override;
    constructor create(controller: TPointerscanController; childid: integer);
  end;

  TScanDataUploader = class(TThread) //class for uploading scandata to a child
  private
    fcontroller: TPointerscanController;
    fchildid: integer;
    starttime: integer;
    procedure UpdateChildProgress(sent: qword; total: qword);
  public
    procedure execute; override;
    constructor create(controller: TPointerscanController; childid: integer);
  end;

  TPointerscanDoneEvent = procedure(sender: TObject; hasError: boolean; errorstring: string) of object;

  TPointerscanController = class(TThread)
  private
    fTerminatedScan: boolean;

    fOnScanDone: TPointerscanDoneEvent;
    fOnStartScan: TNotifyEvent;

    localscanners: array of TPointerscanWorker;
    localscannersCS: TCriticalSection;

    currentscanid: integer; //the scan id of the current scan (for reattaching to parents after an unexpected dc)
    scannerid: integer; //my own scanner id


    nextscanfileid: integer;  //unique id for a ptr (this will be used to generate a list of .ptr files used)
    nextchildid: integer; //unique id for assigning an id to children


    pointerlisthandler: TReversePointerListHandler;
    pointerlisthandlerfile: Tmemorystream; //when no tempfile access is allowed this will hold the contents of the .scandata file

    pointersize: integer;

    listensocket: THandle;

    parentcs: TCriticalSection;
    parent: TPointerscanControllerParent; //the currently selected host
    parentqueue: array of TPointerscanControllerParent; //queue that holds other parents. Access controlled by parentcs

    orphanedSince: qword; //time since the last parent disconnected.  Use this to decide when to give up on it and continue from the queue


    childnodescs: tcriticalsection; //for adding/removing from the array
    childnodes: array of TPointerscanControllerChild;

    parentUpdater: TAsyncTimer;
    lastUpdateSent: qword;


    lastPathCheck: qword; //last time the path check checked the queues
    lastPathCheckMinPath: Array of dword;
    lastPathCheckMaxPath: Array of dword;


    connector: TPointerscanConnector;
    connectorcs: TCriticalSection;
{    sockethandle: THandle;}



  //  myID: integer; //if worker, this will be the ID to identify the generated results, and to reconnect


   {
    firsttime: boolean; //For workers. This causes the first update to go without a wait

    broadcastcount: integer;
    lastBroadcast: dword;     }


    savestate: boolean; //if true and terminated is true then save the current state
    currentscanhasended: boolean;

    resumePtrFileReader: TPointerscanresultReader;

    fstarttime: qword;
    fTotalResultsReceived: qword; //updated when a child sends it results
    fTotalPathsEvaluatedByErasedChildren: qword; //when a child entry is deleted, add it's total paths evaluated value to this

    wasidle: boolean; //state of isIdle since last call to waitForAndHandleNetworkEvent

    newProgressbarLabel: string;

    fShouldQuit: boolean;
    procedure UpdateProgressbarLabel; //synced

    procedure InitializeCompressedPtrVariables;
    procedure InitializeEmptyPathQueue; //initializes the arrays inside the pathqueue


    procedure notifyStartScan;
    function getMaxBitCount(absolutemaxvalue: dword; Signed: boolean): dword;

    procedure EatFromOverflowQueueIfNeeded;

    function childrenDone: boolean;
    function localScannersDone: boolean;

    procedure getQueueStatistics_checkPath(path: TPathQueueElement);
    procedure getQueueStatistics;
    function getPathQueueElementSize: integer;
    function getTotalPathsEvaluatedbyChildren: qword;

    procedure WritePathQueueElementToStream(s: Tstream; element: PPathQueueElement);
    procedure LoadPathQueueElementFromStream(s: Tstream; element: PPathQueueElement);
    {procedure WritePathQueueElementToMemory(element: PPathQueueElement; var p: pbytearray);
    procedure LoadPathQueueElementFromMemory(element: PPathQueueElement; var p: pbytearray); //returns the next position
    }

    procedure appendDynamicPathQueueToOverflowQueue(paths: TDynPathQueue);
    procedure BuildPathListForTransmission(var paths: TDynPathQueue; count: integer; includeVeryGoodPath: boolean);

    function ismatchtovalue(p: pointer): boolean;  //checks if the pointer points to a value matching the user's input
    procedure SaveAndClearQueue(s: TStream);
    procedure SetupQueueForResume;
    procedure reversescan;




    procedure handleParentException(error: string);
    procedure handleChildException(index: integer; error: string);
    procedure handleParentQueueException(index: integer; error: string);
    procedure setupListenerSocket;

    procedure WaitForHello(sockethandle: Tsocket; var msg: TPSHelloMsg);
    procedure SayHello(potentialparent: PPointerscanControllerParent);


    //procedure cleanupScan;
    function sendPathsToParent: integer; //sends a lot of paths to the parent


    procedure HandleUpdateStatusReply_DoNewScan;
    procedure HandleUpdateStatusReply_GiveMeYourPaths;
    procedure HandleUpdateStatusReply_HereAreSomePaths;
    procedure HandleUpdateStatusReply_CurrentScanHasEnded;
    procedure HandleUpdateStatusReply_EverythingOK;
    procedure HandleUpdateStatusReply;

    procedure UpdateStatus_cleanupScan;
    procedure UpdateStatus(sender: tobject); //sends the current status to the parent

    procedure HandleGoodbyeMessage(index: integer);
    procedure HandleQueueMessage(index: integer);
    procedure HandleCanUploadResultsMessage(index: integer);
    procedure HandleUploadResultsMessage(index: integer);
    procedure HandleSendPathsMessage(index: integer);

    procedure HandleUpdateStatusMessage_RequestPathsFromChild(child: PPointerscancontrollerchild; count: integer);
    procedure HandleUpdateStatusMessage_SendPathsToChild(child: PPointerscancontrollerchild; count: integer);
    procedure HandleUpdateStatusMessage(index: integer);

    procedure HandleChildMessage(index: integer);
    procedure waitForAndHandleNetworkEvent;

    procedure acceptConnection;
    procedure workerexception(sender: TObject);
    procedure ConnectorConnect(sender: TObject; sockethandle: TSocket; IBecameAParent: boolean; entry: PConnectEntry);

    procedure OverflowQueueWriter(sender: TObject; PathQueueElement: TPathQueueElement);
    function getTerminatedState: boolean;

    procedure ProcessScanDataFiles;
  protected
    property Terminated:boolean read getTerminatedState;
  public


    //network addition
    initializer: boolean; //indicates that this is the top node of the pointerscan network


    listenport: word;
    publicname: string;

    allowIncomingParent: boolean;
    parentpassword: string;

    allowIncomingChildren: boolean;
    childpassword: string;
    autoTrustIncomingChildren: boolean;

    maxResultsToFind: qword; //number of results found
    maxTimeToScan: qword;    //max time to scan in seconds
    allowTempFiles: boolean;



    {
    workers: array of record //if server, this will contain a list of connected workers
      s: THandle;
      id: integer;
      threadcount: integer;
      pathsPerSecond: qword;
      pointersfound: qword;
      outofdiskspace: boolean;
      alldone: boolean;
    end;       }


    //reverse
    firstaddress: pointer;
    currentaddress: pointer;
    //lastaddress: pointer;

    lookingformin: ptrUint;
    lookingformax: ptrUint;

    reverseScanCS: TCriticalSection;

    //reverse^

    //ownerform: TfrmPointerscanner;

    automatic: boolean;
    automaticaddress: ptrUint;

    startaddress: ptrUint;
    stopaddress: ptrUint;
    progressbar: TProgressbar;
    progressbarLabel: TLabel;
    sz: integer;
    maxlevel: integer;
    unalligned: boolean;

    LimitToMaxOffsetsPerNode: boolean;
    MaxOffsetsPerNode: integer; //Sets how many different offsets per node should be handled at most (specifically mentioning different offsets since a pointervalue can have multiple addresses, meaning the same offset, different paths)
    includeSystemModules: boolean;

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

    mustStartWithBase: boolean;
    BaseStart: ptruint;
    BaseStop: ptruint;


    mustEndWithSpecificOffset: boolean;
    mustendwithoffsetlist: array of dword;
    onlyOneStaticInPath: boolean;
    noReadOnly: boolean;
    mustBeClassPointers: boolean; //when set the pointers must all point to a class object
    acceptNonModuleClasses: boolean; //when set class objects may also be non module objects (jitted)
    noLoop: boolean; //when set a pointerpath may not have the same address multiple times

    useStacks: boolean; //when set the stack regions will be marked as static
    stacksAsStaticOnly: boolean; //when set the only static addresses are stack addresses
    threadstacks: integer; //the number of stacks used as a lookup. (counted from first stack to newer ones)
    stacksize: integer; //Number of bytes in a stack


    threadcount: integer;
    scannerpriority: TThreadPriority;

    filename: string; //the final filename
    phase: integer;


    generatePointermapOnly: boolean;

    negativeOffsets: boolean;
    compressedptr: boolean;
    MaxBitCountModuleOffset: dword;
    MaxBitCountModuleIndex: dword;
    MaxBitCountLevel: dword;
    MaxBitCountOffset: dword;

    staticonly: boolean; //for reverse

    hasError: boolean;
    errorString: string;

    RegionFilename: string;


    LoadedPointermapFilename: string;
    UseLoadedPointermap: boolean;

    pathqueuelength: integer;
    pathqueue: TMainPathQueue;
    pathqueueCS: TCriticalSection; //critical section used to add/remove entries
    {$ifdef windows}
    pathqueueSemaphore: THandle;
    {$else}
    pathqueueSemaphore: TSemaphore;  //Event to notify sleeping threads to wake up that there is a new path in the queue
    {$endif}

    overflowqueuecs: Tcriticalsection;
    overflowqueue: TDynPathQueue; //this queue will hold a number of paths that the server/worker received too many. (e.g a request for paths was made, but by the time the paths are received, the pathqueue is full again) It's accessed by the controller thread only


    outofdiskspace: boolean;

    instantrescan: boolean;
    instantrescanfiles:array of record
      filename: string;
      memoryfilestream: TMemoryStream; //if no tempfiles this holds the scandata file
      address: ptruint;
      plist: TPointerListHandler;
      progressbar: TProgressBar;
      progresslabel: TLabel;
    end;

    resumescan: boolean; //if true load the pointermap from filename.resume.scandata and the queue from filename.resume.queue

    downloadingscandata: boolean; //true while scandata is being downloaded
    downloadingscandata_received: qword;
    downloadingscandata_total: qword;
    downloadingscandata_starttime, downloadingscandata_stoptime: qword;

    function UploadResults(decompressedsize: integer; s: tmemorystream): boolean; //sends the given results (compressed) to the parent.

    procedure BecomeChildOfNode(ip: string; port: word; password: string);
    procedure BecomeParentOfNode(ip: string; port: word; password: string; trusted: boolean=false);


    procedure changeWorkerPriority(priority: TThreadPriority);
    procedure removeWorkerThread;
    procedure addWorkerThread(preferedprocessor: integer=-1);

    procedure disconnectChild(childid: integer; force: boolean);

    function hasNetworkResponsibility: boolean;

    function isIdle: boolean;
    function isDone: boolean;
    procedure getMinAndMaxPath(var minpath: TDynDwordArray; var maxpath: TDynDwordArray);
    procedure getThreadStatuses(s: TStrings);
    function getTotalTimeWriting: qword;
    function getTotalPathsEvaluated: qword;
    function getLocalPathsEvaluated: qword;
    function getTotalResultsFound: qword;
    function getTotalPathQueueSize: integer;
    function getPointerlistHandlerCount: qword;
    function getActualThreadCount: integer;
    function getPotentialThreadCount: integer;
    procedure getConnectingList(var l: TConnectEntryArray);
    procedure getConnectionList(var l: TConnectionEntryArray);
    procedure getParentData(var d: TPublicParentData);

    procedure TerminateAndSaveState;
    procedure execute_nonInitializer;
    procedure execute; override;
    procedure Terminate; //terminate is not overridable but this works for simple stuff, like the pointermap generator quit flag
    constructor create(suspended: boolean);
    destructor destroy; override;

    property starttime: Qword read fstarttime;
    property totalpathsevaluated: qword read getTotalPathsEvaluated;
    property localpathsevaluated: qword read getLocalPathsEvaluated;
    property OnScanDone: TPointerscanDoneEvent read fOnScanDone write fOnScanDone;
    property OnStartScan: TNotifyEvent read fOnStartScan write fOnStartScan;
  end;

  toffsetlist = array of dword;



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



implementation

uses PointerscanNetworkCommands, ValueFinder, ProcessHandlerUnit, Parsers;

resourcestring
  rsFailureCopyingTargetProcessMemory = 'Failure copying target process memory';
  rsPSCTheChildIsSendingMeResultsOfADifferentScan = 'The child is sending me results of a different scan';
  rsPSCTheUploadWasTerminated = 'The upload was terminated';
  rsPSCImpossibleErrorUseLoadedPointermapWasFalseEtc = 'Impossible error: UseLoadedPointermap was false when a child message got handled';
  rsPSCTheScanWasTerminated = 'The scan was terminated';
  rsPSCInvalidResultReceivedAfterUploadingTheScanresults = 'Invalid result received after uploading the scanresults';
  rsPSCSuccesfullySentScandataToChild = 'Succesfully sent scandata to child';
  rsPSCError = ' (Error: ';
  rsPSCTerminated = ':Terminated';
  rsPSCSleeping = ':Sleeping';
  rsPSCWritingToDisk = ':Writing to disk';
  rsPSCWorking = ':Working';
  rsPSCInvalidQueueFile = 'Invalid queue file';
  rsPSCInvalidHandshakeSignature = 'Invalid handshake signature';
  rsPSCAParentTriedToConnect = 'A parent tried to connect';
  rsPSCInvalidParentPassword = 'Invalid parent password';
  rsPSCAChildTriedToConnect = 'A child tried to connect';
  rsPSCInvalidChildPassword = 'Invalid child password';
  rsPSCInvalidMessage = 'Invalid message';
  rsPSCHELLOAfterInitializtion = 'HELLO after initializtion';
  rsPSCInvalidMessageReceived = 'Invalid message received';
  rsPSCTheChildTriedToSendMeResultsWhileIWasStillBusy = 'The child tried to send me results while I was still busy';
  rsPSCTheChildTriedToSendANegativeAmount = 'The child tried to send a negative amount';
  rsPSCTheChildTriedToSendMorePathsAtOnceThanAllowed = 'The child tried to send more paths at once than allowed';
  rsPSCTheChildTriedToSendMorePathsThanAllowedAfterARequest = 'The child tried to send more paths than allowed after a request';
  rsPSCInvalidResultReceivedFromPSUPDATEREPLYCMDHEREARESOMEPATHS = 'Invalid result received from PSUPDATEREPLYCMD_HEREARESOMEPATHS';
  rsPSCForSomeUnknownReasonTheUntrustedChildIsntIdleAnymore = 'For some unknown reason the untrusted child isn''t idle anymore';
  rsPSCInvalidReplyForPSUPDATEREPLYCMDCURRENTSCANHASENDED = 'Invalid reply for PSUPDATEREPLYCMD_CURRENTSCANHASENDED';
  rsPSCChildIsntIdleWhilePreviouslyItWas = 'child isn''t idle while previously it was...';
  rsPSCTheChildDidntRespondToPSUPDATEREPLYCMDEVERYTHINGOKAsExpected = 'The child didn''t respond to PSUPDATEREPLYCMD_EVERYTHINGOK as expected';
  rsPSCNoResumePtrFileReaderPresent = 'no resume ptr file reader present';
  rsPSCNewScanStartedWhileNotDone = 'New scan started while not done';
  rsPSCInvalidScandataReceivedFilecount = 'Invalid scandata received. filecount=0';
  rsPSCTheParentTriedToSendMeANegativeAmmountOfPaths = 'The parent tried to send me a negative ammount of paths';
  rsPSCTheParentTriedToSendMeMorePathsThanAllowedAafterUpdate = 'The parent tried to send me more paths than allowed (after update)';
  rsPSCInvalidUpdateStatusReplyReceived = 'Invalid UpdateStatus reply received';
  rsPSCParentDidntRespondProperlyToPSCMDPREPAREFORMYTERMINATION = 'Parent didn''t respond properly to PSCMD_PREPAREFORMYTERMINATION';
  rsPSCFailureCreatingSocket = 'Failure creating socket';
  rsPSCFailureToBindPort = 'Failure to bind port ';
  rsPSCFailureToListen = 'Failure to listen';
  rsPSCDuringScanFinishing = 'During scan finishing: ';
  rsPSCThePointerlisthandlerWasDestroyedWithoutAGoodReason = 'The pointerlisthandler was destroyed without a good reason';
  rsPSCInvalidCommandWhileWaitingForHello = 'Invalid command while waiting for hello';
  rsPSCAlreadystillConnectedToThisChild = 'Already/still connected to this child';
  rsInvalidData = 'invalid data:';
  rsNoUpdateFromTheClientForOver120Sec = 'No update from the client for over 120 seconds';
  rsAllPathsReceived = 'All paths received';
  rsSavingPointermap = 'Saving pointermap';

//------------------------POINTERLISTLOADER-------------
procedure TPointerlistloader.execute;
var
  s: TStream;
  ds: Tdecompressionstream;
begin
  try
    s:=memoryfilestream;
    if s=nil then
      s:=tfilestream.Create(filename, fmOpenRead or fmShareDenyNone);

    s.position:=0;

    ds:=Tdecompressionstream.create(s);

    pointerlisthandler:=TPointerListHandler.createFromStream(ds, progressbar);

    ds.free;

    if s is TFileStream then
      s.free;

  except
    on e:exception do
    begin
      OutputDebugString('TPointerlistloader exception:'+e.message);
    end;
  end;
end;


//------------------------SCANRESULTDOWNLOADER-------------
procedure TScanResultDownloader.execute;
var
  s: TSocketStream;
  scanid: dword;
  streamSize: dword;
  decompressedStreamSize: dword;
  ms: Tmemorystream;
  ds: Tdecompressionstream;


  resultstream: Tfilestream;
  i: integer;
  EntrySize: integer;
begin
  //first get the socketstream

  if fcontroller.compressedptr then
  begin
    EntrySize:=fcontroller.MaxBitCountModuleOffset+fcontroller.MaxBitCountModuleIndex+fcontroller.MaxBitCountLevel+fcontroller.MaxBitCountOffset*(fcontroller.maxlevel-length(fcontroller.mustendwithoffsetlist));
    EntrySize:=(EntrySize+7) div 8;
  end
  else
    EntrySize:=sizeof(dword)+sizeof(integer)+sizeof(integer)+fcontroller.maxlevel*sizeof(dword);

  s:=nil;
  ms:=nil;
  ds:=nil;

  with fcontroller do
  begin
    fcontroller.childnodescs.Enter;
    try
      for i:=0 to length(fcontroller.childnodes)-1 do
      begin
        if fcontroller.childnodes[i].childid=fchildid then
        begin
          s:=fcontroller.childnodes[i].socket;
          break;
        end;
      end;
    finally
      fcontroller.childnodescs.Leave;
    end;
  end;

  if s=nil then exit; //the child got deleted for some reason

  try
    //download the data
    scanid:=s.ReadDWord;
    if scanid<>fcontroller.currentscanid then raise TSocketException.create(rsPSCTheChildIsSendingMeResultsOfADifferentScan);

    streamsize:=s.ReadDWord;
    decompressedStreamSize:=s.ReadDword;

    ms:=TMemoryStream.create;
    ms.Size:=streamsize;
    ms.CopyFrom(s, streamsize);


    //tell the child it was received
    s.WriteByte(0);
    s.flushWrites;
  except
    on e:exception do
    begin
      fcontroller.childnodescs.enter;

      for i:=0 to length(fcontroller.childnodes)-1 do
      begin
        if fcontroller.childnodes[i].childid=fchildid then
        begin
          fcontroller.handleChildException(i,  e.message);
          break;
        end;
      end;

      fcontroller.childnodescs.leave;
    end;
  end;

  s:=nil; //not needed anymore
  //process the data (while this is still going on the child may not call PSCMD_UPLOADRESULTS)


  try
    if ms<>nil then
    begin
      ms.position:=0;
      try
        if fcontroller.initializer then
        begin
          //decompress ms and write it to disk
          ds:=Tdecompressionstream.create(ms);
          try
            resultstream:=nil;
            //check if there is already a resultstream assigned to this child
            fcontroller.childnodescs.enter;
            try
              for i:=0 to length(fcontroller.childnodes)-1 do
              begin
                if fcontroller.childnodes[i].childid=fchildid then
                begin
                  if fcontroller.childnodes[i].resultstream<>nil then
                    resultstream:=fcontroller.childnodes[i].resultstream
                  else
                  begin
                    //create a resultstream
                    resultstream:=TFileStream.Create(fcontroller.filename+'.results.child'+inttostr(fChildID), fmCreate);
                    fcontroller.childnodes[i].resultstream:=resultstream;
                  end;

                  inc(fcontroller.childnodes[i].resultsfound, decompressedStreamSize div EntrySize);

                  break;
                end;
              end;

            finally
              fcontroller.childnodescs.Leave;
            end;


            if resultstream<>nil then
            begin
              i:=resultstream.CopyFrom(ds, 0);
              if i=0 then
                OutputDebugString('FUUUCK');
            end;




          finally
            ds.free;
          end;
        end
        else
        begin

          while fcontroller.UploadResults(decompressedStreamSize, ms)=false do
          begin
            sleep(10+random(500));
            if terminated then exit;
          end;
        end;

      finally
        ms.free;
      end;


    end;

  except
    on e:exception do
    begin
      if fcontroller.initializer then
      begin
        //unexpected error
        fcontroller.errorString:=e.message;
        fcontroller.terminate;
        exit;
      end
      else
      begin
        //issue with the parent
        fcontroller.parentcs.enter;
        try
          fcontroller.handleParentException(e.message);
        finally
          fcontroller.parentcs.Leave;
        end;
      end;
    end;

  end;
end;

constructor TScanResultDownloader.create(controller: TPointerscanController; childid: integer);
begin
  fchildid:=childid;
  fController:=controller;

  inherited create(false);
end;

//--------------------------SCANDATAUPLOADER--------------
procedure TScanDataUploader.UpdateChildProgress(sent: qword; total: qword);
var
  i: integer;
begin
  fcontroller.childnodescs.Enter;
  try
    for i:=0 to length(fcontroller.childnodes)-1 do
    begin
      if fcontroller.childnodes[i].childid=fchildid then
      begin
        fcontroller.childnodes[i].ScanDataSent:=sent;
        fcontroller.childnodes[i].ScanDataTotalSize:=total;
        break;
      end;
    end;
  finally
    fcontroller.childnodescs.Leave;
  end;
end;

procedure TScanDataUploader.execute;
var
  s: TSocketStream;
  i: integer;
  found: boolean;

  f: array of TStream;

  totalsize: qword;
  sent: qword;

  count: integer;
begin
  if terminated then exit;


  //obtain the socket
  s:=nil;
  with fcontroller do
  begin
    fcontroller.childnodescs.Enter;
    try
      for i:=0 to length(fcontroller.childnodes)-1 do
      begin
        if fcontroller.childnodes[i].childid=fchildid then
        begin
          s:=fcontroller.childnodes[i].socket;
          fcontroller.childnodes[i].ScanDataStartTime:=GetTickCount64;
          break;
        end;
      end;
    finally
      fcontroller.childnodescs.Leave;
    end;
  end;

  if s=nil then exit; //
  try
    if self.terminated then raise exception.create(rsPSCTheUploadWasTerminated);


    s.WriteByte(PSUPDATEREPLYCMD_DONEWSCAN);
    s.WriteDWord(fchildid);  //tell it the childid (it's new scannerid)

    with fcontroller do
    begin
      s.WriteDWord(currentscanid);
      s.WriteDWord(maxlevel);
      s.WriteDWord(sz);
      s.WriteByte(ifthen(compressedptr, 1,0));
      s.WriteByte(ifthen(staticonly,1,0));
      s.WriteByte(ifthen(noLoop,1,0));
      s.WriteByte(ifthen(LimitToMaxOffsetsPerNode,1,0));
      s.WriteByte(ifthen(unalligned,1,0));
      s.WriteWord(MaxOffsetsPerNode);
      s.WriteByte(ifthen(mustStartWithBase,1,0));
      s.WriteQWord(BaseStart);
      s.WriteQWord(BaseStop);
      s.WriteByte(ifthen(onlyOneStaticInPath,1,0));
      s.writebyte(ifthen(mustEndWithSpecificOffset,1,0));
      s.writeWord(length(mustendwithoffsetlist));
      for i:=0 to length(mustendwithoffsetlist)-1 do
        s.WriteDWord(mustendwithoffsetlist[i]);


      s.WriteDWord(1+length(instantrescanfiles));

      if UseLoadedPointermap=false then
        raise exception.create(rsPSCImpossibleErrorUseLoadedPointermapWasFalseEtc);


      s.flushWrites;

      if self.terminated then raise exception.create(rsPSCTheScanWasTerminated);


      setlength(f, length(instantrescanfiles)+1);
      try
        if allowTempFiles then
        begin
          //load the .scandata from the files on the disk
          f[0]:=tfilestream.Create(LoadedPointermapFilename, fmOpenRead or fmShareDenyNone);
          for i:=0 to length(instantrescanfiles)-1 do
            f[i+1]:=tfilestream.Create(instantrescanfiles[i].filename, fmOpenRead or fmShareDenyNone);
        end
        else
        begin
          //get the .scandata files from memory
          f[0]:=TMemoryStreamReader.create(pointerlisthandlerfile);
          for i:=0 to length(instantrescanfiles)-1 do
            f[i+1]:=TMemoryStreamReader.create(instantrescanfiles[i].memoryfilestream);
        end;




        totalsize:=0;
        for i:=0 to length(f)-1 do
          totalsize:=totalsize+f[i].Size;

        s.WriteQWord(totalsize);   //totalsize

        self.starttime:=GetTickCount64;

        sent:=0;
        UpdateChildProgress(sent, totalsize);
        //update the child progress



        for i:=0 to length(f)-1 do
        begin
          //send a header
          if i>0 then
            s.WriteQword(instantrescanfiles[i-1].address)
          else
            s.WriteQWord(0);  //not important for the main  (automaticaddress)

          s.WriteQWord(f[i].Size);

          //and now send the file (in 64KB blocks)

          while f[i].position<f[i].size do
          begin
            inc(sent, s.CopyFrom(f[i], min(65536, f[i].size-f[i].position)));

            s.flushWrites;
            UpdateChildProgress(sent, totalsize);

            if self.terminated then raise exception.create(rsPSCTheScanWasTerminated);
          end;

        end;

      finally
        for i:=0 to length(f)-1 do
          if f[i]<>nil then
            f[i].free;

        setlength(f,0);
      end;


      if s.ReadByte<>0 then
      begin
        raise TSocketException.create(rsPSCInvalidResultReceivedAfterUploadingTheScanresults);
      end
      else
      begin
        OutputDebugString(rsPSCSuccesfullySentScandataToChild);

        //mark it as a child we should keep track off
        fcontroller.childnodescs.Enter;
        try
          for i:=0 to length(fcontroller.childnodes)-1 do
          begin
            if fcontroller.childnodes[i].childid=fchildid then
            begin
              fcontroller.childnodes[i].hasReceivedScandata:=true;
              break;
            end;
          end;
        finally
          fcontroller.childnodescs.Leave;
        end;

      end;

    end;

    //now normally finish (the messagehandler will clean this thread up when it sees it's finished)

  except
    on e:exception do
    begin
      fcontroller.childnodescs.enter;

      found:=false;

      for i:=0 to length(fcontroller.childnodes)-1 do
      begin
        if fcontroller.childnodes[i].childid=fchildid then
        begin
          found:=true;
          fcontroller.handleChildException(i,  e.message);
          break;
        end;
      end;

      fcontroller.childnodescs.leave;
    end;
  end;
end;

constructor TScanDataUploader.create(controller: TPointerscanController; childid: integer);
begin

  fchildid:=childid;
  fController:=controller;

  inherited create(false);
end;


//-------

function TPointerscanController.LocalScannersDone: boolean;
var i: integer;
begin
  result:=true;
  pathqueueCS.enter;
  localscannersCS.Enter;
  try
    for i:=0 to length(localscanners)-1 do
    begin
      if localscanners[i].Finished then continue;

      if localscanners[i].isdone=false then
      begin
        result:=false;
        exit;
      end;
    end;
  finally
    localscannersCS.Leave;
    pathqueueCS.Leave;
  end;
end;

function TPointerscanController.childrendone: boolean;
var i: integer;
begin
  result:=true;
  childnodescs.enter;
  try
    for i:=0 to length(childnodes)-1 do
      if childnodes[i].actualthreadcount>0 then //it still has some threads, so not done. (they can still flush their results when they get destroyed)
      begin
        result:=false;
        exit;
      end;
  finally
    childnodescs.leave;
  end;
end;

function TPointerscanController.isDone: boolean;
var i: integer;
begin
  if not initializer then
    result:=isidle and (getActualThreadCount=0)
  else
  begin
    result:=isidle;

    if result then
      result:=childrendone;

  end;
end;

function TPointerscanController.isIdle: boolean;
var
  i: integer;
begin
  //check if i'm idle
  result:=false;

  childnodescs.enter;
  try
    for i:=0 to length(childnodes)-1 do
    begin
      if not childnodes[i].idle then
        exit;
    end;
  finally
    childnodescs.Leave;
  end;

  //the children are all idle

  overflowqueuecs.enter;
  try
    if (pathqueuelength>0) or (length(overflowqueue)>0) then
      exit;
  finally
    overflowqueuecs.Leave;
  end;

  //the children are idle and all queue entries are empty

  if localscannersdone=false then exit;

  //the children are idle and all queue entries are empty and no thread is currently doing anything

  result:=true; //I guess i'm idle
end;

procedure TPointerscanController.notifyStartScan;
begin
  if assigned(fOnStartScan) then
    fOnStartScan(self);
end;

function TPointerscanController.getTerminatedState: boolean;
begin
  result:=(inherited Terminated) or fTerminatedScan;
end;

function TPointerscanController.getActualThreadCount: integer;
var i: integer;
begin
  localscannerscs.Enter;
  result:=length(localscanners);
  localscannerscs.Leave;

  childnodescs.enter;
  try
    for i:=0 to length(childnodes)-1 do
      inc(result, childnodes[i].actualthreadcount);
  finally
    childnodescs.leave;
  end;

end;


function TPointerscanController.getPotentialThreadCount: integer;
var i: integer;
begin
  result:=threadcount;

  childnodescs.enter;
  try
    for i:=0 to length(childnodes)-1 do
      inc(result, childnodes[i].potentialThreadCount);
  finally
    childnodescs.leave;
  end;
end;

procedure TPointerscanController.getQueueStatistics_checkPath(path: TPathQueueElement);
var j,k: integer;
  offset: dword;
begin
  //get the smallest path
  for j:=0 to length(path.tempresults)-1 do
  begin
    offset:=ifthen(j>path.startlevel, 0, path.tempresults[j]);
    if offset<lastPathCheckMinPath[j] then //it's smaller
    begin
      for k:=0 to path.startlevel do
        lastPathCheckMinPath[k]:=path.tempresults[k];

      for k:=path.startlevel+1 to length(lastPathCheckMinPath)-1 do
        lastPathCheckMinPath[k]:=0;

      break;
    end
    else
    if path.tempresults[j]>lastPathCheckMinPath[j] then
      break;

  end;

  //get the biggest path
  for j:=0 to path.startlevel do
  begin
    offset:=path.tempresults[j];
    if offset>lastPathCheckMaxPath[j] then //it's smaller
    begin
      for k:=0 to path.startlevel do
        lastPathCheckMaxPath[k]:=path.tempresults[k];

      for k:=path.startlevel+1 to length(lastPathCheckMaxPath)-1 do
        lastPathCheckMaxPath[k]:=0;

      break;
    end
    else
    if path.tempresults[j]<lastPathCheckMaxPath[j] then
      break;

  end;

end;

procedure TPointerscanController.getQueueStatistics;
{
This function will traverse the paths and get the smallest and biggest queue element from it
}
var
  i: integer;
begin

  if GetTickCount64>lastPathCheck+5000 then //last time the path check checked the queues
  begin
    //check the queues
    setlength(lastPathCheckMinPath, maxlevel+1);
    setlength(lastPathCheckMaxPath, maxlevel+1);

    for i:=0 to maxlevel do
      lastPathCheckMinPath[i]:=$ffffffff;

    for i:=0 to maxlevel do
      lastPathCheckMaxPath[i]:=0;

    pathqueueCS.enter;

    try
      for i:=0 to pathqueuelength-1 do
        getQueueStatistics_checkPath(pathqueue[i]);
    finally
      pathqueuecs.leave;
    end;

    //and check the overflow queue

    overflowqueuecs.enter;
    try
      for i:=0 to Length(overflowqueue)-1 do
        getQueueStatistics_checkPath(overflowqueue[i]);
    finally
      overflowqueuecs.leave;
    end;
    lastPathCheck:=GetTickCount64;
  end;



end;

procedure TPointerscanController.getMinAndMaxPath(var minpath: TDynDwordArray; var maxpath: TDynDwordArray);
var
  i,j,k: integer;
begin

  setlength(minpath, maxlevel+1);
  for i:=0 to maxlevel do
    minpath[i]:=$ffffffff;

  setlength(maxpath, maxlevel+1);
  for i:=0 to maxlevel do
    maxpath[i]:=0;

  localscannerscs.enter;
  try
    for i:=0 to length(localscanners)-1 do
    begin
      //find min
      for j:=0 to maxlevel do
      begin
        if localscanners[i].tempresults[j]<minpath[j] then
        begin
          for k:=0 to maxlevel do
            minpath[k]:=localscanners[i].tempresults[k];

          break;
        end
        else
        if localscanners[i].tempresults[j]>minpath[j] then
          break;
      end;

      //find max
      for j:=0 to maxlevel do
      begin
        if localscanners[i].tempresults[j]>maxpath[j] then
        begin
          for k:=0 to maxlevel do
            maxpath[k]:=localscanners[i].tempresults[k];

          break;
        end
        else
        if localscanners[i].tempresults[j]<maxpath[j] then
          break;
      end;


    end;
  finally
    localscannerscs.leave;
  end;

  getQueueStatistics;

  for j:=0 to maxlevel do
  begin
    if lastPathCheckMinPath[j]<minpath[j] then
    begin
      for k:=0 to maxlevel do
        minpath[k]:=lastPathCheckMinPath[k];

      break;
    end
    else
    if lastPathCheckMinPath[j]>minpath[j] then
      break;
  end;

  for j:=0 to maxlevel do
  begin
    if lastPathCheckMaxPath[j]>maxpath[j] then
    begin
      for k:=0 to maxlevel do
        maxpath[k]:=lastPathCheckMaxPath[k];

      break;
    end
    else
    if lastPathCheckMaxPath[j]>maxpath[j] then
      break;
  end;
end;

procedure TPointerscanController.getThreadStatuses(s: TStrings);
var i: integer;
  e: string;
begin
  s.Clear;
  localscannersCS.enter;
  try
    for i:=0 to length(localscanners)-1 do
    begin
      if localscanners[i].haserror then
        e:=rsPSCError+localscanners[i].errorString+')'
      else
        e:='';


      if localscanners[i].hasTerminated then
        s.add(IntToStr(i)+rsPSCTerminated+e)
      else
      if localscanners[i].isdone then
        s.add(IntToStr(i)+rsPSCSleeping+e)
      else
      if localscanners[i].isFlushing then
        s.add(IntToStr(i)+rsPSCWritingToDisk+e)
      else
        s.add(IntToStr(i)+rsPSCWorking+e);


    end;
  finally
    localscannersCS.leave;
  end;
end;

function TPointerscanController.getTotalTimeWriting: qword;
var i: integer;
begin
  result:=0;

  localscannersCS.enter;
  try
    for i:=0 to length(localscanners)-1 do
      inc(result, localscanners[i].timespentwriting);
  finally
    localscannersCS.leave;
  end;
end;

function TPointerscanController.getTotalPathsEvaluatedbyChildren: qword;
var i: integer;
begin
  result:=fTotalPathsEvaluatedByErasedChildren;
  //go through the childlist and add their results
  childnodescs.enter;
  try
    for i:=0 to length(childnodes)-1 do
      inc(result, childnodes[i].TotalPathsEvaluated);
  finally
    childnodescs.leave;
  end;
end;

function TPointerscanController.getLocalPathsEvaluated: qword;
var i: integer;
begin
  result:=0;
  localscannersCS.enter;
  try
    for i:=0 to length(localscanners)-1 do
      inc(result, localscanners[i].PathsEvaluated);
  finally
    localscannersCS.leave;
  end;
end;

function TPointerscanController.getTotalPathsEvaluated: qword;
begin
  result:=getTotalPathsEvaluatedbyChildren+getLocalPathsEvaluated
end;

function TPointerscanController.getTotalPathQueueSize: integer;
var i: integer;
begin
  result:=pathqueuelength;
  overflowqueuecs.enter;
  inc(result, length(overflowqueue));
  overflowqueuecs.leave;

  childnodescs.enter;
  try
    for i:=0 to length(childnodes)-1 do
      inc(result, childnodes[i].totalpathqueuesize);
  finally
    childnodescs.leave;
  end;
end;

function TPointerscanController.getTotalResultsFound: qword;
var i: integer;
begin
  result:=fTotalResultsReceived;
  localscannersCS.enter;
  try
    for i:=0 to length(localscanners)-1 do
      inc(result, localscanners[i].pointersfound);
  finally
    localscannersCS.leave;
  end;

  childnodescs.enter;
  for i:=0 to length(childnodes)-1 do
    inc(result, childnodes[i].resultsfound);
  childnodescs.leave;
end;

function TPointerscanController.getPointerlistHandlerCount: qword;
begin
  if pointerlisthandler<>nil then
    result:=pointerlisthandler.count
  else
    result:=0;
end;

procedure TPointerscanController.getConnectionList(var l: TConnectionEntryArray);
var i: integer;
begin
  childnodescs.enter;
  try
    setlength(l, length(childnodes));
    for i:=0 to length(childnodes)-1 do
    begin
      l[i].ip:=childnodes[i].ip;
      l[i].childid:=childnodes[i].childid;
      l[i].port:=childnodes[i].port;
      l[i].isidle:=childnodes[i].idle;
      l[i].potentialthreadcount:=childnodes[i].potentialthreadcount;
      l[i].actualthreadcount:=childnodes[i].actualthreadcount;
      l[i].trustedconnection:=childnodes[i].trusted;
      l[i].pathquesize:=childnodes[i].pathqueuesize;
      l[i].totalpathqueuesize:=childnodes[i].totalpathqueuesize;
      l[i].pathsevaluated:=childnodes[i].totalPathsEvaluated;
      l[i].resultsfound:=childnodes[i].resultsfound;
      l[i].disconnected:=childnodes[i].socket=nil;
      l[i].lasterror:=childnodes[i].Error;
      l[i].uploadingscandata:=childnodes[i].scandatauploader<>nil;
      l[i].ScanDataSent:=childnodes[i].ScanDataSent;
      l[i].ScanDataTotalSize:=childnodes[i].ScanDataTotalSize;
      l[i].ScanDataStartTime:=childnodes[i].ScanDataStartTime;
      l[i].downloadingResuls:=childnodes[i].scanresultDownloader<>nil;
      l[i].lastUpdateReceived:=childnodes[i].lastUpdateReceived;
    end;
  finally
    childnodescs.leave;
  end;
end;

procedure TPointerscanController.getConnectingList(var l: TConnectEntryArray);
begin
  if connector<>nil then
    connector.GetList(l)
  else
    setlength(l,0);
end;

procedure TPointerscanController.getParentData(var d: TPublicParentData);
begin
  d.connected:=parent.socket<>nil;
  d.name:=parent.name;
  d.ip:=parent.ip;
  d.port:=parent.port;
  d.lastupdatesent:=lastUpdateSent;
  d.waitingforreconnect:=orphanedSince<>0;
end;


procedure TPointerscanController.appendDynamicPathQueueToOverflowQueue(paths: TDynPathQueue);
{
Add the paths in this array to the and of the overflow queue, and then add them to the main queue if there is room
}
var oldstart: integer;
  i: integer;
begin
  wasidle:=false; //small speedup to let children tell the parent they are idle

  overflowqueuecs.Enter;
  try
    oldstart:=length(overflowqueue);
    setlength(overflowqueue, length(overflowqueue)+length(paths));
    for i:=0 to length(paths)-1 do
      overflowqueue[oldstart+i]:=paths[i];
  finally
    overflowqueuecs.Leave;
  end;

  EatFromOverflowQueueIfNeeded;
end;


     {
procedure TPointerscanController.LoadPathQueueElementFromMemory(element: PPathQueueElement; var p: pbytearray); //returns the next position
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


end;

procedure TPointerscanController.WritePathQueueElementToMemory(element: PPathQueueElement; var p: pbytearray);
var pos: integer;
begin
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
end;       }

procedure TPointerscanController.WritePathQueueElementToStream(s: Tstream; element: PPathQueueElement);
{
Writes the given element to the stream
}
var
  i: integer;
begin
  s.WriteDWord(element.startlevel);
  s.WriteQWord(element.valuetofind);
  for i:=0 to maxlevel do
    s.writedword(element.tempresults[i]);

  if noloop then
    for i:=0 to maxlevel do
      s.writeqword(element.valuelist[i]);
end;

procedure TPointerscanController.LoadPathQueueElementFromStream(s: Tstream; element: PPathQueueElement);
{
Reads an element from the stream
}
var
  i: integer;
begin
  element.startlevel:=s.ReadDWord;
  element.valuetofind:=s.ReadQWord;

  if length(element.tempresults)<maxlevel+1 then
    setlength(element.tempresults, maxlevel+1);

  for i:=0 to maxlevel do
    element.tempresults[i]:=s.ReadDWord;

  if noloop then
  begin
    if length(element.valuelist)<maxlevel+1 then
      setlength(element.valuelist, maxlevel+1);

    for i:=0 to maxlevel do
      element.valuelist[i]:=s.ReadQWord;
  end;
end;

function TPointerscanController.getPathQueueElementSize: integer;
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
    result:=sizeof(dword)+sizeof(qword)+ sizeof(dword)*(maxlevel+1)+sizeof(qword)*(maxlevel+1)
  else
    result:=sizeof(dword)+sizeof(qword)+ sizeof(dword)*(maxlevel+1);
end;

procedure TPointerscanController.EatFromOverflowQueueIfNeeded;
var
  i: integer;
  oi: integer;
  pathsToCopy: integer;
  listsize, valuelistsize: integer;
begin
  if (pathqueuelength=MAXQUEUESIZE) or (length(overflowqueue)=0) then exit;

  overflowqueuecs.enter;
  try
    if (length(overflowqueue)>0) and (pathqueuelength<MAXQUEUESIZE-1) then //I could use some paths
    begin
      listsize:=sizeof(dword)*(maxlevel+1);
      valuelistsize:=sizeof(qword)*(maxlevel+1);

      //do I have an overflow I can use ?
      pathqueueCS.enter;

      try
        pathsToCopy:=min(length(overflowqueue), (MAXQUEUESIZE-pathqueuelength)); //get the number of paths to transfer from the oveflow queue to the real queue
        if pathstocopy>0 then
        begin
          for i:=pathqueuelength to pathqueuelength+pathstocopy-1 do
          begin
            //don't use this as the arrays get a pointer instead of copy on write: pathqueue[i]:=overflowqueue[length(overflowqueue)-1-(i-pathqueuelength)];
            oi:=length(overflowqueue)-1-(i-pathqueuelength);
            pathqueue[i].startlevel:=overflowqueue[oi].startlevel;
            pathqueue[i].valuetofind:=overflowqueue[oi].valuetofind;

            copymemory(@pathqueue[i].tempresults[0], @overflowqueue[oi].tempresults[0], listsize);
            if noLoop then
              copymemory(@pathqueue[i].valuelist[0], @overflowqueue[oi].valuelist[0], valuelistsize);
          end;

          inc(pathqueuelength, pathsToCopy);
          {$ifdef windows}
          ReleaseSemaphore(pathqueueSemaphore, pathsToCopy, nil);
          {$else}
          pathqueueSemaphore.Release(pathsToCopy);

          {$endif}

        end;

      finally
        pathqueueCS.leave;
      end;

      setlength(overflowqueue, length(overflowqueue)-pathstocopy);
    end;

  finally
    overflowqueuecs.leave;
  end;
end;

function TPointerscanController.ismatchtovalue(p: pointer): boolean;
begin
  case valuetype of
    vtDword: result:=pdword(p)^=valuescandword;
    vtSingle: result:=(psingle(p)^>=valuescansingle) and (psingle(p)^<=valuescansinglemax);
    vtDouble: result:=(pdouble(p)^>=valuescandouble) and (pdouble(p)^<=valuescandoublemax);
    else
       result:=false;
  end;
end;

type
  TScanDataWriter=class(tthread)
  private
  public
    progressbar: TProgressbar;
    filename: string;
    pointerlisthandler: TReversePointerListHandler;
    procedure execute; override;
  end;

procedure TScanDataWriter.execute;
var
  f: TFileStream;
  cs: Tcompressionstream;
begin
  f:=tfilestream.create(filename, fmCreate);
  cs:=Tcompressionstream.create(clfastest, f);
  pointerlisthandler.exportToStream(cs, progressbar);
  cs.free;
  f.free;
end;

procedure TPointerscanController.SetupQueueForResume;
var f: TFileStream;
    offsetcountperlist: integer;

    i, j,k: integer;

    addedToQueue: integer;

    tempentry: TPathQueueElement;

    tempfix: integer;
    listsize, valuelistsize: integer;

    ml: integer;
begin
  //setup the queue
  //load the overflow from the overflow queue

  f:=tfilestream.Create(filename+'.resume.queue', fmOpenRead or fmShareDenyNone);
  ml:=f.ReadDWord;

  if ml<>maxlevel then raise exception.create(rsPSCInvalidQueueFile);

  listsize:=sizeof(dword)*(maxlevel+1);
  valuelistsize:=sizeof(qword)*(maxlevel+1);

  offsetcountperlist:=maxlevel;

  overflowqueuecs.enter;
  pathqueueCS.enter;
  try
    try

      while f.Position<f.Size do
      begin
        i:=length(overflowqueue);
        setlength(overflowqueue, length(overflowqueue)+1);
        if f.Read(overflowqueue[i].valuetofind, sizeof(overflowqueue[i].valuetofind))>0 then
        begin
          f.read(overflowqueue[i].startlevel, sizeof(overflowqueue[i].startlevel));

          if overflowqueue[i].startlevel>offsetcountperlist then
          begin
            j:=f.Position;
            raise exception.create(rsInvalidData+inttostr(f.position));
          end;

          setlength(overflowqueue[i].tempresults, maxlevel+1);
          f.read(overflowqueue[i].tempresults[0], listsize);

          //length(pathqueue[i].tempresults)*sizeof(pathqueue[i].tempresults[0]));

          if noloop then
          begin
            setlength(overflowqueue[i].valuelist, maxlevel+1);
            f.read(overflowqueue[i].valuelist[0], valuelistsize);
          end;
        end;

      end;

      //sort based on level
      for i:=0 to length(overflowqueue)-2 do
      begin
        for j:=i to length(overflowqueue)-1 do
        begin
          if overflowqueue[i].startlevel>overflowqueue[j].startlevel then //swap
          begin
            tempentry:=overflowqueue[j];
            overflowqueue[j]:=overflowqueue[i];
            overflowqueue[i]:=tempentry;
          end;
        end;
      end;

      addedToQueue:=0;
      try

        for i:=length(overflowqueue)-1 downto 0 do
        begin
          if pathqueuelength<MAXQUEUESIZE then
          begin
            pathqueue[pathqueuelength]:=overflowqueue[i];
            inc(pathqueuelength);

            overflowqueue[i].tempresults[0]:=$cece;
            inc(addedToQueue);

          end else break;
        end;
      except
        on e: exception do
        begin
          OutputDebugString('TPointerscanController.SetupQueueForResume Error:'+e.message);
          setlength(overflowqueue,0);
          raise;
        end;
      end
    finally
      setlength(overflowqueue, length(overflowqueue)-addedToQueue);
      {$ifdef windows}
      ReleaseSemaphore(pathqueueSemaphore, addedToQueue, nil);
      {$else}
      pathqueueSemaphore.Release(addedToQueue);
      {$endif}
    end;

  finally
    pathqueueCS.leave;
    overflowqueuecs.leave;
    f.free;
  end;



end;

procedure TPointerscanController.SaveAndClearQueue(s: TStream);
var
  i: integer;
  pathslocked: boolean;

  v: qword;
  l: integer;
  listsize, valuelistsize: integer;
begin
  if s=nil then exit; //can happen if stop is pressed right after the scan is done but before the gui is updated

  listsize:=sizeof(dword)*(maxlevel+1);
  valuelistsize:=sizeof(qword)*(maxlevel+1);

  if pathqueuelength>0 then
  begin
    pathqueueCS.enter;
    try
      //save the current queue and clear it (repeat till all scanners are done)
      for i:=0 to pathqueuelength-1 do
      begin
        v:=pathqueue[i].valuetofind;
        l:=pathqueue[i].startlevel;
        s.Write(v, sizeof(v));
        s.Write(l, sizeof(l));
        s.Write(pathqueue[i].tempresults[0], listsize);

        if noloop then
          s.Write(pathqueue[i].valuelist[0], valuelistsize);
      end;

      //also save the overflow queue
      for i:=0 to length(overflowqueue)-1 do
      begin
        v:=overflowqueue[i].valuetofind;
        l:=overflowqueue[i].startlevel;
        s.Write(v, sizeof(v));
        s.Write(l, sizeof(l));
        s.Write(overflowqueue[i].tempresults[0], listsize);

        if noloop then
          s.Write(overflowqueue[i].valuelist[0], valuelistsize);
      end;

      setlength(overflowqueue,0);

      i:=pathqueuelength;
      pathqueuelength:=0;
      {$ifdef windows}
      ReleaseSemaphore(pathqueueSemaphore, i, nil);
      {$else}
      pathqueueSemaphore.Release(i);
      {$endif}

//

    finally
      pathqueueCS.Leave;
    end;

  end;

end;

procedure TPointerscanController.reversescan;
{
Do a reverse pointer scan
}
var
  i,j: integer;
  alldone: boolean;

  currentaddress: ptrUint;
  addedToQueue: boolean;

  valuefinder: TValueFinder;

  savedqueue: TFilestream;
  scandatawriter: TScanDataWriter;

  terminatedTime: qword;


  cs: Tcompressionstream;
  s: TFileStream;
  overflowqueuebuffer: TDynPathQueue;
  overfloqeueuebufferpos: integer;
  oldstart: integer;

  procedure handleNetwork;
  begin
    if useLoadedPointermap=false then  //one time init. (no incomming connections will get accepted during this time
    begin
      //create a scandata file to send to the children if it gets any
      childnodescs.Enter;   //prevents the connector from adding a child to the list.
      try
        LoadedPointermapFilename:=self.filename+'.scandata';
        s:=TFileStream.Create(LoadedPointermapFilename, fmCreate);
        try
          cs:=Tcompressionstream.Create(clfastest, s);
          try
            pointerlisthandler.exportToStream(cs);
          finally
            cs.free;
          end;
        finally
          s.free;
        end;
      finally
        childnodescs.leave;
      end;

      useLoadedPointermap:=true;
    end;

    waitForAndHandleNetworkEvent;
  end;

  procedure CreateWriterAndQueue;
  begin
    if scandatawriter=nil then
    begin
      scandatawriter:=TScanDataWriter.Create(true);
      scandatawriter.progressbar:=progressbar;
      scandatawriter.filename:=filename+'.resume.scandata';
      scandatawriter.pointerlisthandler:=pointerlisthandler;
      scandatawriter.Start;
    end;

    if savedqueue=nil then
    begin
      savedqueue:=TFileStream.Create(filename+'.resume.queue', fmCreate);
      savedqueue.WriteDWord(maxlevel); //just to be safe
    end;
  end;



begin
  terminatedTime:=0;


  //scan the buffer
  savedqueue:=nil;
  scandatawriter:=nil;


  alldone:=false;


  try
    if maxlevel>0 then
    begin

      if (initializer) then //don't start the scan if it's a worker system
      begin
        //initialize the first reverse scan worker
        //that one will spawn of all his other siblings if needed

        fstarttime:=gettickcount64;

        if Self.findValueInsteadOfAddress then
        begin
          overfloqeueuebufferpos:=0;
          setlength(overflowqueuebuffer,128);
          for i:=0 to 127 do
          begin
            overflowqueuebuffer[i].startlevel:=0;
            setlength(overflowqueuebuffer[i].tempresults,maxlevel+1);
            setlength(overflowqueuebuffer[i].valuelist,maxlevel+1);
          end;

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

            if pathqueuelength<MAXQUEUESIZE-1 then
            begin
              pathqueueCS.enter;
              //setup the queueelement
              if pathqueuelength<MAXQUEUESIZE-1 then
              begin
                pathqueue[pathqueuelength].startlevel:=0;
                pathqueue[pathqueuelength].valuetofind:=currentaddress;
                inc(pathqueuelength);
                addedToQueue:=true;

                {$ifdef windows}
                ReleaseSemaphore(pathqueueSemaphore, 1, nil);
                {$else}
                pathqueueSemaphore.Release;
                {$endif}
              end;

              pathqueueCS.leave;
            end;

            if (not addedToQueue) and (not terminated) then
            begin
              //add it to the overflow queue
              overflowqueuebuffer[overfloqeueuebufferpos].valuetofind:=currentaddress;
              inc(overfloqeueuebufferpos);
              if overfloqeueuebufferpos>=128 then
              begin
                overflowqueuecs.enter;
                oldstart:=length(overflowqueue);
                setlength(overflowqueue,length(overflowqueue)+128);
                for i:=0 to 127 do
                  overflowqueue[oldstart+i]:=overflowqueuebuffer[i];

                overflowqueuecs.leave;
              end;


            end;

            if unalligned then
              currentaddress:=ValueFinder.FindValue(currentaddress+1)
            else
              currentaddress:=ValueFinder.FindValue(currentaddress+4);

          end;

          overflowqueuecs.enter;
          oldstart:=length(overflowqueue);
          setlength(overflowqueue,length(overflowqueue)+length(overflowqueuebuffer));
          for i:=0 to length(overflowqueuebuffer)-1 do
            overflowqueue[oldstart+i]:=overflowqueuebuffer[i];

          overflowqueuecs.leave;


          //done with the value finder, wait till all threads are done
          valuefinder.free;
        end
        else
        begin

          if resumescan then
          begin
            SetupQueueForResume;
          end
          else
          begin
            //initialize the first thread (it'll spawn new pathqueues)
            pathqueueCS.enter;
            pathqueue[pathqueuelength].startlevel:=0;
            pathqueue[pathqueuelength].valuetofind:=self.automaticaddress;
            inc(pathqueuelength);
            pathqueueCS.Leave;
            {$ifdef windows}
            ReleaseSemaphore(pathqueueSemaphore, 1, nil);
            {$else}
            pathqueueSemaphore.Release;
            {$endif}
          end;



        end;

      end;


      while (not alldone) do
      begin
        {$ifdef windows}
        outofdiskspace:=getDiskFreeFromPath(filename)<64*1024*1024*length(localscanners); //64MB for each thread
        {$else}
        outofdiskspace:=false;
        {$endif}


        if haserror then
          break;


        if Terminated then
        begin
       {   OutputDebugString('Forced terminate. Telling the scanworkers to die as well');

          if savestate then
            OutputDebugString('Saving state');        }

          //force the workers to die if they are sleeping
          for i:=0 to length(localscanners)-1 do
          begin
            localscanners[i].savestate:=savestate;
            localscanners[i].stop:=true;
            localscanners[i].Terminate;
          end;

          if terminated and savestate then
            createWriterAndQueue;



          if terminatedTime=0 then
            terminatedTime:=GetTickCount64;

          if GetTickCount64>terminatedTime+10000 then
          begin
            if messagebox(0,'The pointerscanner seems to take a long time to terminate. Force it?', 'Pointerscan Timeout', MB_YESNO)=IDYES then break;


            terminatedTime:=GetTickCount;

          end;

        end;


        EatFromOverflowQueueIfNeeded;

        if hasNetworkResponsibility then
          HandleNetwork
        else
        begin
          if terminated and savestate then
            sleep(10)
          else
            sleep(500);
        end;


        if ((not alldone) and (pathqueuelength=0)) or terminated then //it's 0 or terminated
        begin
          //aquire a lock to see if it's still 0
          EatFromOverflowQueueIfNeeded;

          pathqueueCS.Enter;
          if not terminated then
          begin
            if pathqueuelength=0 then  //still 0
              alldone:=isdone
            else
              alldone:=false;
          end
          else
            alldone:=LocalScannersDone and ChildrenDone; //don't bother about paths, they will get saved later, or discarded


          if terminated and savestate then
          begin
            createWriterAndQueue;
            saveAndClearQueue(savedqueue);
          end;

          pathqueueCS.Leave;
        end;

      end;


    end;


    //all threads are done
    localscannerscs.Enter;
    for i:=0 to length(localscanners)-1 do
    begin

      localscanners[i].terminate;
      localscanners[i].stop:=true;
    end;
    localscannerscs.Leave;


    localscannersCS.enter;
    try
      for i:=0 to length(localscanners)-1 do
      begin
        localscanners[i].WaitFor; //wait till this thread has terminated because the main thread has terminated


        localscanners[i].Free;
        localscanners[i]:=nil;
      end;

      setlength(localscanners,0);

    finally
      localscannersCS.Release;
    end;

    if terminated and savestate then
      saveAndClearQueue(savedqueue);


    if not savestate then
    begin
      //make sure these files are gone
      DeleteFile(filename+'.resume.queue');
      DeleteFile(filename+'.resume.config');
      DeleteFile(filename+'.resume.scandata');
    end;


  finally
    if savedqueue<>nil then
      freeandnil(savedqueue);

    if scandatawriter<>nil then
    begin
      scandatawriter.WaitFor;
      freeandnil(scandatawriter);
    end;

    if assigned(fOnScanDone) then
      fOnScanDone(self, true, errorstring);



  end;

  terminate;
end;

function TPointerscanController.getMaxBitCount(absolutemaxvalue: dword; Signed: boolean): dword;
//converts the given absolutemaxvalue to a mask to be used
//if signed, the mostSignificantbit will get the bit which will mark if it's negative
var
  bitcount: integer;
  mask: dword;
begin
  mask:=0;
  bitcount:=0;
  while absolutemaxvalue>0 do
  begin
    inc(bitcount);
    mask:=(mask shl 1) or 1;
    absolutemaxvalue:=absolutemaxvalue shr 1;
  end;

  if Signed then
  begin
    inc(bitcount);
    mask:=(mask shl 1) or 1;
  end;

  result:=bitcount;
end;



procedure TPointerscanController.WorkerException(sender: TObject);
//usually called by workers
var i: integer;
begin
  if localscannersCS.TryEnter then
  begin
    for i:=0 to length(localscanners)-1 do
      localscanners[i].Terminate;

    if haserror=false then
    begin
      haserror:=true;
      errorstring:=TPointerscanWorker(sender).errorstring;
    end;

    localscannersCS.leave;
  end;
end;

function TPointerscanController.UploadResults(decompressedsize: integer; s: tmemorystream): boolean;
//writes the compressed result stream to the host
//WARNING: This function does not raise an exception. When it encounters an exception, it will handle it, resulting in the invalidation of the parent socket
//After using this function confirm again that the parent socket isn't nil
begin
  result:=false;

  if parentcs.tryEnter then
  begin
    try
      //todo: test me
      if parent.socket=nil then
      begin
        OutputDebugString('Uploadresults called but parent.socket=nil');
        if orphanedSince=0 then //give up sending these results, we have abandoned the parent
        begin
          result:=true;
          OutputDebugString('The parent has been abandoned. Discarding the results');
        end;

        exit; //return to the caller (failure)
      end;

      if parent.scanid<>currentscanid then
      begin
        OutputDebugString('upload results: parent.scanid<>currentscanid');
        exit; //the parent will probably tell the child to kill it's current scan (first a cleanup that will remove this caller)
      end;

      if currentscanhasended and (savestate=false) then
      begin
        OutputDebugString('Curent scan has ended and savestate=true');
        result:=true;
        exit;
      end;

      try
        s.position:=0;
        parent.socket.WriteByte(PSCMD_CANUPLOADRESULTS); //ask if it can handle a new upload from this child
        parent.socket.flushWrites;

        if parent.socket.ReadByte=1 then
        begin
          //yes
          parent.socket.WriteByte(PSCMD_UPLOADRESULTS);
          parent.socket.WriteDWord(currentscanid);
          parent.socket.WriteDWord(s.size);
          parent.socket.WriteDWord(decompressedsize);
          parent.socket.CopyFrom(s,s.size);
          parent.socket.flushWrites;

          if parent.socket.ReadByte<>0 then raise exception.create('Invalid reply from PSCMD_UPLOADRESULTS');

          result:=true; //success

        end; //nope, try again later
      except
        on e:exception do
          handleParentException(e.Message);
      end;

      s.position:=0;
    finally
      parentcs.leave;
    end;
  end;
end;



procedure TPointerscanController.acceptConnection;
{$ifdef windows}
var
  client: TSockAddrIn;
  size: integer;
  s: Tsocket;
  es: Tsocket;
  cehandshakesignature: byte;
  passwordsize: byte;
  password: string;
  connectiontype: byte;
  nonblockingmode: u_long;

  ss: TSocketStream;
  {$endif}
begin
  //accept the incoming connection and create a Host or Child controller
  {$ifdef windows}
  ZeroMemory(@client, sizeof(client));
  size:=sizeof(client);
  s:=fpaccept(listensocket, @client, @size);


  OutputDebugString('Incoming connection from '+inttostr(byte(client.sin_addr.S_un_b.s_b1))
                                                +'.'+inttostr(byte(client.sin_addr.S_un_b.s_b2))
                                                +'.'+inttostr(byte(client.sin_addr.S_un_b.s_b3))
                                                +'.'+inttostr(byte(client.sin_addr.S_un_b.s_b4))
  );



  if s=tsocket(SOCKET_ERROR) then
  begin
    OutputDebugString('s==INVALID_SOCKET');
    OutputDebugString('lasterror='+inttostr(socketerror));
    exit;
  end;

  //connected. Initiate the handshake (3 second timeout max)

{$ifdef windows}
  nonblockingmode:=1;
  ioctlsocket(s, longint(FIONBIO), nonblockingmode);
{$else}
  fcntl(fSocket, F_SETFL, fcntl(socketfd, F_GETFL, 0) | O_NONBLOCK);
{$endif}

  ss:=TSocketStream.create(s, false);

  try
    try
      cehandshakesignature:=ss.ReadByte;
      if cehandshakesignature<>$ce then
        raise TSocketException.create(rsPSCInvalidHandshakeSignature);

      password:=ss.ReadAnsiString8;
      connectiontype:=ss.ReadByte;

      if connectiontype=0 then //parent
      begin
        if not allowIncomingParent then
          raise exception.create(rsPSCAParentTriedToConnect);

        if parentpassword<>password then
          raise TSocketException.create(rsPSCInvalidParentPassword);
      end
      else
      if connectiontype=1 then //child
      begin
        if not allowIncomingChildren then
          raise exception.create(rsPSCAChildTriedToConnect);

        if childpassword<>password then
          raise TSocketException.create(rsPSCInvalidChildPassword);
      end
      else
        raise exception.create(rsPSCInvalidMessage);


      ss.WriteByte(0); //still here, so a valid password
      ss.flushWrites;
    finally
      ss.free;
    end;

    //still here
    ConnectorConnect(self, s, connectiontype=1, nil);

  except
    on e: exception do
    begin
      outputdebugstring('Error while accepting connection:'+e.message);
      closehandle(s);
    end;
  end;
       {$endif}
end;


procedure TPointerscanController.disconnectChild(childid: integer; force: boolean);
var i: integer;
begin
  childnodescs.Enter;
  try
    for i:=0 to length(childnodes)-1 do
      if childnodes[i].childid=childid then
      begin
        childnodes[i].iConnectedTo:=false; //no reconnect
        if force then
          handleChildException(i, 'forced disconnect')
        else
        begin
          childnodes[i].takePathsAndDisconnect:=true;
          childnodes[i].terminating:=true;
        end;

      end;


  finally
    childnodescs.leave;
  end;
end;


procedure TPointerscanController.waitForAndHandleNetworkEvent;
{$ifdef windows}
var
  count: integer;
  i,j: integer;
  readfds: TFDSet;
  maxfd: Tsocket;
  sockets: array of Tsocket;

  timeout: TTimeVal;

  checkedallsockets: boolean;

  idle: boolean;
  {$endif}
begin
  //listen to the listensocket if available and for the children
  {$ifdef windows}
  EatFromOverflowQueueIfNeeded;

  if not initializer then
  begin
    idle:=isIdle;
    if idle and (wasidle=false) then
    begin
      wasidle:=idle;
      if parentUpdater<>nil then
        parentUpdater.TriggerNow; //tell the parent I recently became idle
    end
    else
      wasidle:=idle;
  end;



  if (listensocket=INVALID_SOCKET) and (length(childnodes)=0) then
  begin
    {
    nothing to do. The controller does not accept incomming connections and there
    are no childnodes connected.
    Only the user can change this by manually adding a new child, or activing the
    listening ability
    }
    sleep(500);
    exit;
  end;




  zeromemory(@readfds, sizeof(TFDSet));
  if listensocket<>INVALID_SOCKET then
  begin
    FD_SET(listensocket, readfds);
    maxfd:=listensocket;
  end
  else
    maxfd:=0;




  i:=0;
  checkedallsockets:=false;
  while checkedallsockets=false do
  begin
    childnodescs.Enter;
    try
      while i<length(childnodes) do
      begin
        //cleanup worker threads for this child
        if (childnodes[i].scandatauploader<>nil) and (childnodes[i].scandatauploader.Finished) then  //check if this thread is available
          freeandnil(childnodes[i].scandatauploader);     //cleanup

        if (childnodes[i].scanresultDownloader<>nil) and (childnodes[i].scanresultDownloader.Finished) then
          freeandnil(childnodes[i].scanresultDownloader);


        //add this child to the list of sockets to wait for
        if (childnodes[i].socket<>nil) and (childnodes[i].scandatauploader=nil) and (childnodes[i].scanresultDownloader=nil) then
        begin

          FD_SET(childnodes[i].socket.sockethandle, readfds);
          if readfds.fd_count=FD_SETSIZE then break; //the list is full

          if maxfd<childnodes[i].socket.sockethandle then
            maxfd:=childnodes[i].socket.sockethandle;
        end;
        inc(i);
      end;

      checkedallsockets:=i>=length(childnodes);
    finally
      childnodescs.leave;
    end;

    //listen for this set
    count:=readfds.fd_count;

    if count>0 then
    begin
      timeout.tv_sec:=0;
      timeout.tv_usec:=500000 div (1+(length(childnodes) div FD_SETSIZE));
      j:=select(maxfd, @readfds, nil, nil, @timeout);
    end
    else
    begin
      sleep(500);
      j:=-1;
    end;

    if j<>-1 then
    begin
      if (listensocket<>INVALID_SOCKET) and FD_ISSET(listensocket, readfds) then //accept connection
        acceptConnection;

      childnodescs.Enter;
      try
        i:=0;
        while i<length(childnodes) do
        begin
          if (childnodes[i].socket<>nil) and FD_ISSET(childnodes[i].socket.sockethandle, readfds) then //handle it
          begin
            try
              HandleChildMessage(i);
            except
              on e:exception do //exception happened
                handleChildException(i, e.message); //marks the child as disconnected
            end;
          end;

          {$ifndef DEBUGPROTOCOL}
          if (childnodes[i].socket<>nil) and (childnodes[i].scandatauploader=nil) and (childnodes[i].LastUpdateReceived<>0) and (GetTickCount64-childnodes[i].LastUpdateReceived>120000) then
            handleChildException(i, rsNoUpdateFromTheClientForOver120Sec); //marks the child as disconnected
          {$endif}

          inc(i);
        end;

      finally
        childnodescs.leave;
      end;

    end;




    FD_ZERO(readfds);
    maxfd:=0;
  end;


  i:=0;
  childnodescs.enter;
  try
    //deal with disconnected children
    while i<length(childnodes) do
    begin
      if childnodes[i].socket=nil then
      begin
        if childnodes[i].trusted=false then
        begin
          if gettickcount64>childnodes[i].MissingSince+30*60*1000 then //wait up to 30 minutes before giving up on untrusted nodes
            childnodes[i].MissingSince:=0;
        end
        else
        begin
          if gettickcount64>childnodes[i].MissingSince+1200*60*1000 then //wait up to 2 hours before giving up on trusted nodes
            childnodes[i].MissingSince:=0;
        end;

        if childnodes[i].MissingSince=0 then
        begin
          //delete it
          inc(fTotalPathsEvaluatedByErasedChildren,  childnodes[i].totalPathsEvaluated);
          inc(fTotalResultsReceived, childnodes[i].resultsfound);

          if childnodes[i].scandatauploader<>nil then
          begin
            childnodes[i].scandatauploader.Terminate;
            childnodes[i].scandatauploader.WaitFor;
            childnodes[i].scandatauploader.Free;
          end;

          if childnodes[i].scanresultDownloader<>nil then
          begin
            childnodes[i].scanresultDownloader.Terminate;
            childnodes[i].scanresultDownloader.WaitFor;
            childnodes[i].scanresultDownloader.Free;
          end;

          if childnodes[i].trusted=false then
          begin
            //take back the path(s) I last sent it when it was idle
            appendDynamicPathQueueToOverflowQueue(childnodes[i].nontrustedlastpaths);
            setlength(childnodes[i].nontrustedlastpaths,0);
          end;

          if childnodes[i].resultstream<>nil then
            freeandnil(childnodes[i].resultstream);

          for j:=i to length(childnodes)-2 do
            childnodes[j]:=childnodes[j+1];

          setlength(childnodes, length(childnodes)-1);

          continue;
        end;
      end;
      inc(i);
    end;

  finally
    childnodescs.Leave;
  end;
    {$endif}
end;


procedure TPointerscancontroller.handleParentException(error: string);
var
  shouldreconnect: boolean;
  host, password: string;
  port: word;
  i: integer;
  abandonparent: boolean;
begin
  OutputDebugString('Parent error: '+error);
  shouldreconnect:=false;
  parentcs.enter;

  try
    if parent.socket<>nil then
      FreeAndNil(parent.socket);

    if parent.iConnectedTo then
    begin

      shouldreconnect:=true;
      host:=parent.connectdata.ip;
      password:=parent.connectdata.password;
      port:=parent.connectdata.port;
    end;
  finally
    parentcs.leave;
  end;

  if shouldreconnect then
  begin
    BecomeChildOfNode(host, port, password);
    OutputDebugString('Going to reconnect to parent');
  end;

  abandonparent:=currentscanhasended;
  if abandonparent then
  begin
    localscannersCS.enter;
    try
      for i:=0 to length(localscanners)-1 do
        if localscanners[i].HasResultsPending then
        begin
          OutputDebugString('Not going to abandon the parent because a worker has results for it');
          abandonparent:=false; //try to save this
          break;
        end;
    finally
      localscannersCS.leave;
    end;

  end;

  if abandonparent then
  begin
    OutputDebugString('Abandoning this parent');
    orphanedSince:=0; //we won't miss this one
  end
  else
  begin
    orphanedSince:=GetTickCount64;
    OutputDebugString('Keeping this parent');
  end;

end;

procedure TPointerscancontroller.handleParentQueueException(index: integer; error: string);
var
  i: integer;
  shouldreconnect: boolean;
  host, password: string;
  port: word;
begin
  OutputDebugString('ParentQueue error: '+error);
  shouldreconnect:=false;

  parentcs.enter; //shouldn't be needed as this should be called by something that already has the lock
  try
    if parentqueue[index].socket<>nil then
      freeandnil(parentqueue[index].socket);

    if parentqueue[index].iConnectedTo then
    begin
      shouldreconnect:=true;
      host:=parentqueue[index].connectdata.ip;
      password:=parentqueue[index].connectdata.password;
      port:=parentqueue[index].connectdata.port;
    end;


    for i:=index+1 to length(parentqueue)-2 do
      parentqueue[i]:=parentqueue[i+1];

    setlength(parentqueue, length(parentqueue)-1);
  finally
    parentcs.leave;
  end;

  if shouldreconnect then
    BecomeChildOfNode(host, port, password); //add to the connector list
end;

procedure TPointerscancontroller.handleChildException(index: integer; error: string);
{
Handle socket and other exceptions that should disconnect the child
do not clean up anything else. (threads and other data structures will get cleared by the eventhandler)
}
var
  shouldreconnect: boolean;
  host, password: string;
  port: word;
  trusted: boolean;
begin
  OutputDebugString('Child error: '+error);
  shouldreconnect:=false;

  childnodescs.Enter; //shouldn't be needed as things that raise child exceptions SHOULD already have a lock on it
  try
    if childnodes[index].socket<>nil then
      freeandnil(childnodes[index].socket);

    if (currentscanhasended and childnodes[index].idle) or (childnodes[index].hasReceivedScandata=false) then
      childnodes[index].MissingSince:=0   //I won't miss it
    else
      childnodes[index].MissingSince:=GetTickCount64;

    childnodes[index].Error:=error;
    //else I won't really miss it...

    if childnodes[index].iConnectedTo then
    begin
      shouldreconnect:=true;
      host:=childnodes[index].connectdata.ip;
      password:=childnodes[index].connectdata.password;
      port:=childnodes[index].connectdata.port;
      trusted:=childnodes[index].trusted; //I would say false in this case though...
    end;

  finally
    childnodescs.Leave;
  end;

  if shouldreconnect then
    BecomeParentOfNode(host, port, password, trusted); //add to the connector list
end;



procedure TPointerscancontroller.HandleChildMessage(index: integer);
{
called when waitforandhandlenetworkevent receives a read event from the child
quickly handle it. For commands that take a while (receiving scandata) spawn a new thread and mark the thread as busy for the duration

raises Exception and SocketException
}
var
  command: byte;
  s: TSocketStream;
  canreconnect: boolean;
begin
  s:=childnodes[index].socket;
  if s<>nil then
  begin
    command:=s.ReadByte;
    case command of
      PSCMD_HELLO: raise exception.create(rsPSCHELLOAfterInitializtion);
      PSCMD_YOUREINTHEQUEUE: HandleQueueMessage(index);
      PSCMD_UPDATESTATUS: HandleUpdateStatusMessage(index);
      PSCMD_AMITRUSTED: s.WriteByte(ifthen(childnodes[index].trusted,1,0));
      PSCMD_SENDPATHS: HandleSendPathsMessage(index);
      PSCMD_CANUPLOADRESULTS: HandleCanUploadResultsMessage(index);
      PSCMD_UPLOADRESULTS: HandleUploadResultsMessage(index);
      PSCMD_PREPAREFORMYTERMINATION:
      begin
        childnodes[index].terminating:=true;
        childnodes[index].socket.WriteByte(0);//understood
        childnodes[index].socket.flushWrites;
      end;

      PSCMD_GOODBYE: HandleGoodbyeMessage(index);
      else
         raise exception.create(rsPSCInvalidMessageReceived);
    end;
  end;
end;

procedure TPointerscancontroller.HandleQueueMessage(index: integer);
var
  s: TSocketStream;
begin
  OutputDebugString(childnodes[index].ip+' : HandleQueueMessage');

  s:=childnodes[index].socket;

  childnodes[index].queued:=true;
  childnodes[index].queuepos:=s.ReadDWord;
  childnodes[index].queuesize:=s.ReadDword;

  s.WriteByte(0); //tell it you received and processed the message
  s.flushWrites;
end;

procedure TPointerscanController.HandleGoodbyeMessage(index: integer);
var canreconnect: boolean;
begin
  canreconnect:=childnodes[index].socket.ReadByte=1;
  freeandnil(childnodes[index].socket);
  childnodes[index].MissingSince:=0; //it's gone but not missing.

  if (connector<>nil) and childnodes[index].iConnectedTo and canreconnect then
    connector.AddConnection(childnodes[index].connectdata.ip, childnodes[index].connectdata.port, childnodes[index].connectdata.password, false, childnodes[index].trusted);
end;

procedure TPointerscanController.HandleCanUploadResultsMessage(index: integer);
{
called by PSCMD_CANUPLOADRESULTS
Checks if the current child is busy sending results to the parent
}
begin
  OutputDebugString(childnodes[index].ip+' : HandleCanUploadResultsMessage');

  childnodes[index].socket.WriteByte(ifthen(childnodes[index].scanresultDownloader=nil, 1, 0));
  childnodes[index].socket.flushWrites;
end;

procedure TPointerscanController.HandleUploadResultsMessage(index: integer);
{
The child wants to send me it's found results
spawn a thread that will receive the results and then pass them on to the parent or save to disk
}
begin
  OutputDebugString(childnodes[index].ip+' : HandleCanUploadResultsMessage');

  if childnodes[index].scanresultDownloader<>nil then //the child did not call PSCMD_CANUPLOADRESULTS to see if it could send new results, or blatantly ignored it's result
    raise exception.create(rsPSCTheChildTriedToSendMeResultsWhileIWasStillBusy);

  //spawn a thread
  childnodes[index].scanresultDownloader:=TScanResultDownloader.create(self, childnodes[index].childid);

end;

procedure TPointerscanController.HandleSendPathsMessage(index: integer);
{
if trusted (or terminating child) receive the paths it sends me and add it to the overflow queue
}
var
  child: PPointerscancontrollerchild;
  count: dword;
  paths: TDynPathQueue;

  ms: Tmemorystream;
  i: integer;
begin
  child:=@childnodes[index];

  OutputDebugString(child.ip+' : HandleSendPathsMessage');
  count:=child.socket.ReadDWord;


  if (currentscanhasended and savestate) or child.trusted or child.terminating then
  begin

    if integer(count)<0 then raise exception.create(rsPSCTheChildTriedToSendANegativeAmount);
    if count>65536 then raise exception.create(rsPSCTheChildTriedToSendMorePathsAtOnceThanAllowed);  //actually 1000 but let's allow some customization


    setlength(paths, count);
    if count>0 then
    begin

      ms:=TMemoryStream.Create;
      try
        ms.CopyFrom(child.socket, getPathQueueElementSize*count);
        ms.Position:=0;
        for i:=0 to length(paths)-1 do
          LoadPathQueueElementFromStream(ms, @paths[i]);
      finally
        ms.free;
      end;

      appendDynamicPathQueueToOverflowQueue(paths);
    end;

    child.socket.WriteByte(0); //success
  end
  else
    child.socket.WriteByte(1); //fail because of untrusted

  child.socket.flushWrites;
end;

procedure TPointerscanController.BuildPathListForTransmission(var paths: TDynPathQueue; count: integer; includeVeryGoodPath: boolean);
{
Used by the client and parent when sending pathqueues to the other side
this will remove the entries from the list, so on exception, add them back
}
var
  actualcount: integer;
  c: integer;
  start: integer;
  i: integer;
begin
  if count<0 then
    count:=0;

  if count>65535 then
    count:=65535; //never more


  setlength(paths, count);
  actualcount:=0;

  if includeVeryGoodPath then
  begin
    pathqueueCS.enter;
    try
      if (pathqueuelength>0) then
      begin
        //give it one good path (the best path)

        {$ifdef windows}
        if WaitForSingleObject(pathqueueSemaphore, 0)=WAIT_OBJECT_0 then //lock the entry
        {$else}
        if pathqueueSemaphore.TryAcquire then
        {$endif}
        begin
          paths[actualcount]:=pathqueue[0];

          //todo: test this
          for i:=0 to pathqueuelength-2 do
            pathqueue[i]:=pathqueue[i+1];

          //allocate a new array for the last one (else the arrays of pathqueue[pathqueuelength-1] will point to the arrays of pathqueue[pathqueuelength-2]
          setlength(pathqueue[pathqueuelength-1].tempresults, maxlevel+1);
          if noloop then
            setlength(pathqueue[pathqueuelength-1].valuelist, maxlevel+1);

          dec(pathqueuelength);

          inc(actualcount);
        end;

      end;

    finally
      pathqueueCS.leave;
    end;

  end;

  if actualcount>=count then exit; //done


  //first get rid of the overflow
  c:=0;
  overflowqueuecs.enter;
  try
    for i:=length(overflowqueue)-1 downto 0 do
    begin
      paths[actualcount]:=overflowqueue[i];
      inc(actualcount);
      inc(c);

      if actualcount>=count then break;
    end;

    setlength(overflowqueue, length(overflowqueue)-c);
    if actualcount>=count then exit;

  finally
    overflowqueuecs.leave;
  end;

  if actualcount<count then //get it from the static pathqueue
  begin
    c:=0;
    pathqueueCS.enter;
    try
      //take from the back
      start:=pathqueuelength-1;
      for i:=start downto 0 do
      begin
        {$ifdef windows}
        if WaitForSingleObject(pathqueueSemaphore, 0)=WAIT_OBJECT_0 then //lock it
        {$else}
        if pathqueueSemaphore.TryAcquire then
        {$endif}
        begin
          paths[actualcount]:=pathqueue[i];

          //add new arrays
          setlength(pathqueue[i].tempresults, maxlevel+1);
          if noloop then
            setlength(pathqueue[i].valuelist, maxlevel+1);

          inc(actualcount);
          dec(pathqueuelength);
        end;

        if actualcount>=count then exit;
      end;

    finally
      pathqueueCS.Leave;
    end;
  end;

  if actualcount<count then //shouldn't happen unless the user interferes (e.g adding a new worker)
    setlength(paths, actualcount);
end;

procedure TPointerscanController.HandleUpdateStatusMessage_RequestPathsFromChild(child: PPointerscancontrollerchild; count: integer);
var
  paths: TDynPathQueue;
  buf: Tmemorystream;
  i: integer;
begin
  //todo: test me

  with child^.socket do
  begin
    WriteByte(PSUPDATEREPLYCMD_GIVEMEYOURPATHS);
    WriteDword(count); //maxcount
    flushWrites;

    count:=ReadDWord;

    if count>65536 then
      raise exception.create(rsPSCTheChildTriedToSendMorePathsThanAllowedAfterARequest);


    setlength(paths, count);


    if count>0 then
    begin
      buf:=TMemoryStream.Create;
      try
        buf.CopyFrom(child^.socket, getPathQueueElementSize*count);

        buf.position:=0;
        for i:=0 to count-1 do
          LoadPathQueueElementFromStream(buf, @paths[i]);
      finally
        buf.free;
      end;


      //still here so I guess it's ok
      appendDynamicPathQueueToOverflowQueue(paths);
    end;
  end;

  EatFromOverflowQueueIfNeeded;
end;


function TPointerscanController.sendPathsToParent: integer;
var
  paths: TDynPathQueue;
  i: integer;

begin
  result:=0;

  if parent.socket=nil then exit;

  if (getTotalPathQueueSize>0) and (currentscanhasended or parent.knowsIAmTerminating) then
  begin
    BuildPathListForTransmission(paths, 1000, false); //it's going to send 1000 paths at a time (or less if it can't do that amount)
    if length(paths)=0 then exit; //don't bother the parent or the critical section

    parentcs.enter;
    try
      if parent.socket<>nil then
      begin
        parent.socket.WriteByte(PSCMD_SENDPATHS);
        parent.socket.WriteDWord(length(paths));
        for i:=0 to length(paths)-1 do
          WritePathQueueElementToStream(parent.socket, @paths[i]);

        parent.socket.flushWrites;
        if parent.socket.ReadByte<>0 then
          appendDynamicPathQueueToOverflowQueue(paths) //failure, but don't error out
        else
          result:=length(paths);
      end
      else
        appendDynamicPathQueueToOverflowQueue(paths); //unexpected disconnect, save these paths

    finally
      parentcs.leave;
    end;

  end;



end;

procedure TPointerscanController.HandleUpdateStatusMessage_SendPathsToChild(child: PPointerscancontrollerchild; count: integer);
{
Reply to a received UpdateStatus message
}
var
  paths: TDynPathQueue;
  i: integer;
  actualcount: integer;

  c: integer;
begin
  //todo: test me
  buildPathListForTransmission(paths, count, child^.trusted and (child^.totalpathqueuesize=0));

  try

    with child^.socket do
    begin
      WriteByte(PSUPDATEREPLYCMD_HEREARESOMEPATHS);
      WriteDWord(length(paths)); //number of paths
      for i:=0 to length(paths)-1 do
        WritePathQueueElementToStream(child^.socket, @paths[i]);

      flushWrites;

      if ReadByte<>0 then raise TSocketException.create(rsPSCInvalidResultReceivedFromPSUPDATEREPLYCMDHEREARESOMEPATHS);
    end;



    if not child^.trusted then //save the paths being sent
    begin
      if child^.idle=false then raise exception.create(rsPSCForSomeUnknownReasonTheUntrustedChildIsntIdleAnymore);   //should NEVER happen (childnodescs is locked and this thread is the only one accepting update messages)

      child^.nontrustedlastpaths:=paths;

      if child^.nontrustedlastpathstime>0 then
      begin
        if (gettickcount64-child^.nontrustedlastpathstime)<1000*60*5 then
        begin
          //it went idle within 5 minutes, trust it a bit more
          inc(child^.trustlevel)
        end
        else
        begin
          //it took longer than 5 minutes... Let's decrease the trustlevel in case he never comes back
          if child^.trustlevel>0 then
            dec(child^.trustlevel);
        end;

      end;

      child^.nontrustedlastpathstime:=GetTickCount64;
    end;

    inc(child^.pathqueuesize, length(paths));

    if child^.idle then
      child^.idle:=child^.pathqueuesize=0; //mark it as active if count>0

  except
    //add these paths to the overflow queue
    appendDynamicPathQueueToOverflowQueue(paths);
    setlength(paths,0);

    //reraise the exception
    raise;
  end;


end;

procedure TPointerscanController.HandleUpdateStatusMessage(index: integer);
{
handle the update message received from the child. Update the child statistics and tell it what to do next
If it's a new scan, spawn a new thread to tell it the scandata information
}
var
  child: PPointerscancontrollerchild;
  s: TSocketStream;
  updatemsg: TPSUpdateStatusMsg;

  overflowsize: integer;
  localpathcount: integer;
  localscannercount: integer;
  childcount: integer;

  pathstosend: integer;

  saveresults: boolean;
begin
  //todo: test me
  child:=@childnodes[index];
  s:=child.socket;



  s.ReadBuffer(updatemsg, sizeof(updatemsg));

//  update the childstatus and issue it a command
  child^.idle:=updatemsg.isidle=1;
  child^.potentialthreadcount:=updatemsg.potentialthreadcount;
  child^.actualthreadcount:=updatemsg.actualthreadcount;
  child^.totalPathsEvaluated:=updatemsg.pathsevaluated;
  child^.pathqueuesize:=updatemsg.localpathqueuecount;
  child^.totalpathqueuesize:=updatemsg.totalpathQueueCount;
  child^.queuesize:=updatemsg.queuesize;

  child^.LastUpdateReceived:=GetTickCount64;

  OutputDebugString(child.ip+' : HandleUpdateStatusMessage(idle='+inttostr(updatemsg.isidle)+')');


  if initializer and (isidle or terminated) then //no more pathqueues and all scanners and children's scanners are waiting for new paths (or terminated by the user)
  begin
    if terminated=false then
      savestate:=true;

    currentscanhasended:=true;
  end;


  //now reply
  if currentscanhasended or ((not child^.idle) and (updatemsg.currentscanid<>currentscanid)) then //scan terminated , or
  begin
    OutputDebugString('Telling child current scan has ended.  (currentscanhasended='+BoolToStr(currentscanhasended,'true','false')+' updatemsg.currentscanid='+inttostr(updatemsg.currentscanid)+' currentscanid='+inttostr(currentscanid));

    child^.socket.WriteByte(PSUPDATEREPLYCMD_CURRENTSCANHASENDED);

    if currentscanhasended then
    begin
      saveresults:=not (terminated and (savestate=false)); //only false if the user terminated the scan and chose not to save the state

      if saveresults then
        OutputDebugString('Save the results')
      else
        OutputDebugString('Discard the results');

      child^.socket.WriteByte(ifthen(saveresults, 1, 0))
    end
    else
    begin
      //special case that under normal situations shouldn't occur (could happen if a scan was stopped and a new one was started before the children where idle, or a long lost child joins)

      OutputDebugString('Discard the results');
      child^.socket.WriteByte(0); //wrong scan id. I'm waiting for him to kill his children. Don't let him send me paths...
    end;


    child^.socket.flushWrites;
    if child^.socket.ReadByte<>0 then
      raise exception.create(rsPSCInvalidReplyForPSUPDATEREPLYCMDCURRENTSCANHASENDED);

    exit;
  end;

  if (updatemsg.currentscanid<>currentscanid) then
  begin
    //spawn a new thread and tell him about the scan (as soon as I quit and release the critical section)
    //use child.childid to identify the child object to update when done

    assert(child^.idle, rsPSCChildIsntIdleWhilePreviouslyItWas);
    if child^.idle then
    begin
      child^.ScanDataTotalSize:=0;
      child^.ScanDataSent:=0;
      child^.ScanDataStartTime:=0;
      child^.scanDataUploader:=TScandataUploader.create(self, child.childid);
    end;

    exit;
  end;

  if currentscanid<>0 then
  begin
    //send/receive some paths to the child
    overflowqueuecs.enter;
    overflowsize:=length(overflowqueue);
    overflowqueuecs.leave;

    localpathcount:=pathqueuelength+overflowsize;


    localscannersCS.enter;
    localscannercount:=length(localscanners);
    localscannersCS.leave;

    childnodescs.enter;
    childcount:=length(childnodes);
    childnodescs.leave;

    if (child^.terminating) then
    begin
      HandleUpdateStatusMessage_RequestPathsFromChild(child,min(1000, updatemsg.localpathqueuecount));

      if child^.takePathsAndDisconnect and (updatemsg.localpathqueuecount=0) then
      begin
        handleChildException(index, rsAllPathsReceived);
      end;

      exit;
    end;

    if (updatemsg.potentialthreadcount>0) or (updatemsg.localpathqueuecount>0) then  //check if it's something we should send or get paths from
    begin
      if (child^.trusted) then
      begin
        //equalize the paths
        if (child^.terminating=false) then
        begin
          if (updatemsg.potentialthreadcount=0) and (updatemsg.localpathqueuecount>0) then
          begin
            //get the paths from this node, it's useless (now)
            HandleUpdateStatusMessage_RequestPathsFromChild(child, updatemsg.localpathqueuecount);
            exit;
          end;

          if (localscannercount=0) and (localpathcount>0) then
          begin
            //this node does not handle paths. Send them all
            HandleUpdateStatusMessage_SendPathsToChild(child, 1+(localpathcount div childcount));
            exit;
          end;

          if (overflowsize>0) and (updatemsg.localpathqueuecount<MAXQUEUESIZE) then
          begin
            //I have some overflow. Send what I can to this child
            HandleUpdateStatusMessage_SendPathsToChild(child, 1+min(overflowsize, MAXQUEUESIZE)-updatemsg.localpathqueuecount);
            exit;
          end;

          if (updatemsg.localpathqueuecount<(MAXQUEUESIZE div 2)) and (localpathcount>(MAXQUEUESIZE div 2)) then
          begin
            //equalize (from parent->child)
            HandleUpdateStatusMessage_SendPathsToChild(child, 1+((localpathcount-updatemsg.localpathqueuecount) div 2));
            exit;
          end;
        end;

        if (updatemsg.localpathqueuecount>(MAXQUEUESIZE div 2)) and (localpathcount<(MAXQUEUESIZE div 2)) then
        begin
          //equalize (from child<-parent)
          HandleUpdateStatusMessage_RequestPathsFromChild(child, 1+((updatemsg.localpathqueuecount-localpathcount) div 2));
          exit;
        end;

        if (localpathcount=0) and (localscannercount>0) then
        begin
          //i'm out of paths, give me half of what you have
          HandleUpdateStatusMessage_RequestPathsFromChild(child, 1+(updatemsg.totalpathQueueCount div 2));
          exit;
        end;



      end
      else
      begin
        //unstable/untrusted
        if child^.idle then //only send paths to the non-trusted child if it's completely idle
        begin
          HandleUpdateStatusMessage_SendPathsToChild(child, 1+min(child.trustlevel, (localpathcount div 4) )); //the trustlevel goes up if it goes idle within 5 minutes
          exit;
        end;

      end;
    end;
  end;

  //still here, so everything is ok
  child^.socket.WriteByte(PSUPDATEREPLYCMD_EVERYTHINGOK);
  child^.socket.flushWrites;
  if child^.socket.ReadByte<>0 then
    raise exception.create(rsPSCTheChildDidntRespondToPSUPDATEREPLYCMDEVERYTHINGOKAsExpected);
end;

//parent->child

procedure TPointerscanController.InitializeCompressedPtrVariables;
var
  f: Tfilestream;
  ds: Tdecompressionstream;
  tempplh: TReversePointerListHandler;
begin
  if compressedptr then
  begin
    //calculate the masks for compression
    //moduleid can be negative, so keep that in mind
    if resumescan then
    begin
      if resumeptrfilereader=nil then raise exception.create(rsPSCNoResumePtrFileReaderPresent);
      MaxBitCountModuleIndex:=resumeptrfilereader.MaxBitCountModuleIndex;
      MaxBitCountModuleOffset:=resumeptrfilereader.MaxBitCountModuleOffset;
      MaxBitCountLevel:=resumeptrfilereader.MaxBitCountLevel;
      MaxBitCountOffset:=resumeptrfilereader.MaxBitCountOffset;
    end
    else
    begin
      if pointerlisthandler=nil then  //should never happen, but use it as a fallback
      begin
        //just load the header
        if pointerlisthandlerfile<>nil then //load it from here
        begin
          pointerlisthandlerfile.position:=0;

          ds:=Tdecompressionstream.create(pointerlisthandlerfile);
          try
            tempplh:=TReversePointerListHandler.createFromStreamHeaderOnly(ds);
          finally
            ds.free;
            pointerlisthandlerfile.position:=0;
          end;
        end
        else
        begin
          f:=TFileStream.create(LoadedPointermapFilename, fmOpenRead or fmShareDenyNone);
          try
            ds:=Tdecompressionstream.create(f);
            try
              tempplh:=TReversePointerListHandler.createFromStreamHeaderOnly(ds);
            finally
              ds.free;
            end;
          finally
            f.free;
          end;
        end;
      end
      else
        tempplh:=pointerlisthandler;


      MaxBitCountModuleIndex:=getMaxBitCount(tempplh.modulelist.Count-1, true);
      if tempplh.is64bit and ((not staticonly) or (tempplh.CanHaveStatic)) then
        MaxBitCountModuleOffset:=64
      else
        MaxBitCountModuleOffset:=32;


      MaxBitCountLevel:=getMaxBitCount(maxlevel-length(mustendwithoffsetlist) , false); //counted from 1.  (if level=4 then value goes from 1,2,3,4) 0 means no offsets. This can happen in case of a pointerscan with specific end offsets, which do not get saved.
      MaxBitCountOffset:=getMaxBitCount(sz, false);
      if unalligned=false then MaxBitCountOffset:=MaxBitCountOffset - 2;

      if pointerlisthandler=nil then
        tempplh.free;
    end;
  end;

end;

procedure TPointerscanController.InitializeEmptyPathQueue;
var i,j: integer;
begin
  pathqueueCS.enter;
  try
    pathqueuelength:=0;
    for i:=0 to MAXQUEUESIZE-1 do
    begin
      setlength(pathqueue[i].tempresults, maxlevel+2);
      for j:=0 to maxlevel+1 do
        pathqueue[i].tempresults[j]:=$cececece;


      if noLoop then
      begin
        setlength(pathqueue[i].valuelist, maxlevel+2);
        for j:=0 to maxlevel+1 do
          pathqueue[i].valuelist[j]:=qword($cececececececece);
      end;
    end;

  finally
    pathqueueCS.leave;
  end;

  overflowqueuecs.enter;
  try
    setlength(overflowqueue,0);
  finally
    overflowqueuecs.leave;
  end;

end;

procedure TPointerscanController.HandleUpdateStatusReply_DoNewScan;
{
the parent is going to tell me information about the scan
}
var
  i: integer;
  streamsize: qword;

  tempfilename: string;
  currentstream: TStream;
  ds: Tdecompressionstream;

  files: integer;


  newscannerid: dword;
  newcurrentscanid: dword;


begin
  //todo: test me
  OutputDebugString(parent.ip+' : HandleUpdateStatusReply_DoNewScan');
  if not isDone then
    raise exception.Create(rsPSCNewScanStartedWhileNotDone);

  UpdateStatus_cleanupScan;

  fTotalPathsEvaluatedByErasedChildren:=0;
  fTotalResultsReceived:=0;


  with parent.socket do
  begin
    newscannerid:=ReadDWord;
    newcurrentscanid:=ReadDWord;

    maxlevel:=ReadDWord;
    sz:=ReadDWord;
    compressedptr:=readbyte=1;
    staticonly:=readbyte=1;
    noloop:=readByte=1;
    LimitToMaxOffsetsPerNode:=readByte=1;
    unalligned:=readByte=1;
    MaxOffsetsPerNode:=ReadWord;
    mustStartWithBase:=readByte=1;
    BaseStart:=ReadQWord;
    BaseStop:=ReadQword;
    onlyOneStaticInPath:=readByte=1;
    mustEndWithSpecificOffset:=readbyte=1;
    setlength(mustendwithoffsetlist, ReadWord);
    for i:=0 to length(mustendwithoffsetlist)-1 do
      mustendwithoffsetlist[i]:=ReadDWord;

    files:=readDword;

    OutputDebugString('Filecount='+inttostr(files));

    if files=0 then
      raise exception.create(rsPSCInvalidScandataReceivedFilecount);

    if length(instantrescanfiles)>0 then
    begin
      OutputDebugString('instantrescanfiles was not empty. Cleaning it');
      for i:=0 to length(instantrescanfiles)-1 do
      begin
        if instantrescanfiles[i].memoryfilestream<>nil then
          freeandnil(instantrescanfiles[i].memoryfilestream);

        if instantrescanfiles[i].plist<>nil then
          freeandnil(instantrescanfiles[i].plist);
      end;
    end;

    setlength(instantrescanfiles, 0);

    setlength(instantrescanfiles, files-1); //-1 because the first one is the main file


    downloadingscandata_received:=0;
    downloadingscandata_total:=ReadQWord;
    downloadingscandata_starttime:=GetTickCount64;
    downloadingscandata:=true;

    OutputDebugString('Start downloading files');

    for i:=0 to files-1 do
    begin
      if i=0 then
        ReadQword
      else
        instantrescanfiles[i-1].address:=ReadQWord;

      streamsize:=ReadQWord;

      if allowTempFiles then
      begin
        //create a tempfile and open a TFileStream to it
        tempfilename:=GetTempFileName;
        currentstream:=TFileStream.Create(tempfilename, fmCreate);

        if i=0 then
          LoadedPointermapFilename:=tempfilename
        else
          instantrescanfiles[i-1].filename:=tempfilename;
      end
      else //create a TMemorystream
      begin
        currentstream:=tmemorystream.create;
        if i=0 then
        begin
          if pointerlisthandlerfile<>nil then
            freeandnil(pointerlisthandlerfile);

          pointerlisthandlerfile:=tmemorystream(currentstream)
        end
        else
          instantrescanfiles[i-1].memoryfilestream:=tmemorystream(currentstream);
      end;


      while currentstream.Position<streamsize do
      begin
        inc(downloadingscandata_received, currentstream.CopyFrom(parent.socket, min(65536, streamsize-currentstream.position)));

        if self.Terminated then exit; //too bad
      end;

      if allowTempFiles then
        currentstream.free; //close the filestream. Reopen when needed (After the download is done)
    end;

    WriteByte(0); //tell the parent I received everything
    flushWrites;

    OutputDebugString('Done downloading files');

    OutputDebugString('Processing files');

    downloadingscandata_stoptime:=GetTickCount64;
    downloadingscandata:=false;

    if threadcount>0 then //if it's going to be used right away:
      ProcessScanDataFiles;

    instantrescan:=length(instantrescanfiles)>0;


    OutputDebugString('Done processing files');

  end;

  currentscanhasended:=false;

  InitializeEmptyPathQueue;
  InitializeCompressedPtrVariables;




  fstarttime:=GetTickCount64;
  if assigned(fOnStartScan) then
    synchronize(NotifyStartScan);


  //spawn the threads:
  if threadcount>0 then
  begin
    localscannersCS.enter;
    try
      while length(localscanners)<threadcount do
        addworkerThread;
    finally
      localscannersCS.leave;
    end;
  end;

  //got till here, so everything got loaded
  currentscanid:=newcurrentscanid;
  scannerid:=newscannerid;
  parent.scanid:=currentscanid;



  parentUpdater.TriggerNow; //restart the Updatestatus function as soon as possible to let the parent know it's ready
end;

procedure TPointerscanController.HandleUpdateStatusReply_GiveMeYourPaths;
{
The parent wants some of my paths
}
var
  maxcount: integer;
  paths: TDynPathQueue;
  i: integer;
begin
  //todo: test me
  maxcount:=parent.socket.ReadDWord;
  if maxcount<0 then
    maxcount:=0;

  OutputDebugString(parent.ip+' : HandleUpdateStatusReply_GiveMeYourPaths('+inttostr(maxcount)+')');

  buildPathListForTransmission(paths, maxcount, true);
  try

    with parent.socket do
    begin
      WriteDWord(length(paths)); //number of paths
      for i:=0 to length(paths)-1 do
        WritePathQueueElementToStream(parent.socket, @paths[i]);

      flushWrites;
    end;
  except
    //add these paths to the overflow queue
    appendDynamicPathQueueToOverflowQueue(paths);
    setlength(paths,0);

    //reraise the exception
    raise;
  end;

end;

procedure TPointerscanController.HandleUpdateStatusReply_HereAreSomePaths;
{
The parent has some paths for me
}
var
  count: integer;
  paths: TDynPathQueue;
  buf: Tmemorystream;
  i: integer;
begin
  //todo: test me
  count:=parent.socket.ReadDWord;



  OutputDebugString(parent.ip+' : HandleUpdateStatusReply_HereAreSomePaths('+inttostr(count)+')');

  if count<0 then
    raise exception.create(rsPSCTheParentTriedToSendMeANegativeAmmountOfPaths);

  if count>65536 then
    raise exception.create(rsPSCTheParentTriedToSendMeMorePathsThanAllowedAafterUpdate);

  if count>0 then
  begin
    setlength(paths, count);

    buf:=TMemoryStream.Create;
    try
      buf.CopyFrom(parent.socket, getPathQueueElementSize*count);


      buf.position:=0;
      for i:=0 to count-1 do
        LoadPathQueueElementFromStream(buf, @paths[i]);

      //still here so I guess it's ok

      appendDynamicPathQueueToOverflowQueue(paths);
    finally
      buf.free;
    end;



  end;

  parent.socket.WriteByte(0); //acknowledge that the paths have been received and handled properly
  parent.socket.flushWrites;


end;

procedure TPointerscanController.HandleUpdateStatusReply_CurrentScanHasEnded;
{
The scan has finished (or terminated)
}
var i: integer;
begin
  //todo: test me
  OutputDebugString(parent.ip+' : HandleUpdateStatusReply_CurrentScanHasEnded');




  //stop all the children and wait for them to end the scan (10-20 seconds)
  savestate:=parent.socket.ReadByte=1; //if this is true and currentscanhasended as well, the children will end the scan, but will also send their current paths
  currentscanhasended:=true;  //tell the children that the scan has ended for as long as this is true (when this function returns UpdateStatus will go tell the local scanners to terminate)

  parent.socket.WriteByte(0); //understood
  parent.socket.flushWrites;


end;

procedure TPointerscanController.HandleUpdateStatusReply_EverythingOK;
begin
  OutputDebugString(parent.ip+' : HandleUpdateStatusReply_EverythingOK');

  parent.socket.WriteByte(0); //acknowledge
  parent.socket.flushWrites;
end;

procedure TPointerscanController.HandleUpdateStatusReply;
{
handles the replies the parent gives after it received the updateStatus message
}
var replycommand: byte;
begin
  replycommand:=parent.socket.ReadByte;

  case replycommand of
    PSUPDATEREPLYCMD_DONEWSCAN: HandleUpdateStatusReply_DoNewScan;
    PSUPDATEREPLYCMD_GIVEMEYOURPATHS: HandleUpdateStatusReply_GiveMeYourPaths;
    PSUPDATEREPLYCMD_HEREARESOMEPATHS: HandleUpdateStatusReply_HereAreSomePaths;
    PSUPDATEREPLYCMD_CURRENTSCANHASENDED: HandleUpdateStatusReply_CurrentScanHasEnded;
    PSUPDATEREPLYCMD_EVERYTHINGOK: HandleUpdateStatusReply_EverythingOK; //everything ok
    else
       raise TSocketException.create(rsPSCInvalidUpdateStatusReplyReceived);
  end;
end;

procedure TPointerscanController.UpdateStatus_cleanupScan;
{
Called by Updatestatus or a subfunction of it
It will free the used memory before a new scan can start
Usually called by the idle cleanup of UpdateStatus or by DoNewScan
}
var i: integer;
begin
  //cleanup the instantrescan files.
  //this can be done safely here because the UpdateStatus message is the only route new scanfiles can be made
  parentcs.Enter;
  try
    for i:=0 to length(instantrescanfiles)-1 do
    begin
      if instantrescanfiles[i].memoryfilestream<>nil then
        freeandnil(instantrescanfiles[i].memoryfilestream);

      if instantrescanfiles[i].plist<>nil then
        freeandnil(instantrescanfiles[i].plist);

      if allowtempfiles then
        deletefile(instantrescanfiles[i].filename);

      if instantrescanfiles[i].memoryfilestream<>nil then
        freeandnil(instantrescanfiles[i].memoryfilestream);
    end;

    setlength(instantrescanfiles,0);

    if pointerlisthandler<>nil then
      freeandnil(pointerlisthandler);

    if allowtempfiles then
      deletefile(LoadedPointermapFilename);

    if pointerlisthandlerfile<>nil then
      freeandnil(pointerlisthandlerfile);


  finally
    parentcs.Leave;
  end;
end;

procedure TPointerscanController.UpdateStatus(sender: tobject);
{
Tells the parent the current status, and deal with it's response
}
var
  i,j: integer;
  updatemsg: TPSUpdateStatusMsg;
  allfinished: boolean;

  phase: integer;
begin
  //note: called by another thread (parent responses can take a while)

  phase:=0;

  try
    parentcs.enter;
    try
      try
        OutputDebugString('UpdateStatus');


        if parent.socket=nil then
        begin
          OutputDebugString('Accessing queue');
          for i:=0 to length(parentqueue)-1 do
          begin
            if parentqueue[i].scanid=currentscanid then
            begin
              //parent came back
              OutputDebugString('Parent returned');
              parent:=parentqueue[i];
              for j:=i+1 to length(parentqueue)-2 do
                parentqueue[j]:=parentqueue[j+1];

              setlength(parentqueue, length(parentqueue)-1);
              orphanedSince:=0;
              break;
            end;
          end;

          if (not fTerminatedScan) and (parent.socket=nil) then //still no parent
          begin
            if (currentscanid=0) or (orphanedSince=0) then
            begin
              //not an orphan, check the queue and make the first one in the list my new parent
              if length(parentqueue) > 0 then
              begin
                parent:=parentqueue[0];
                parent.connecttime:=GetTickCount64; //it was accepted at this time (this way the queue time isn't counted)

                for i:=1 to length(parentqueue)-2 do
                  parentqueue[i]:=parentqueue[i+1];

                setlength(parentqueue, length(parentqueue)-1);
              end;
            end
            else
            begin
              //check if we should give up on our original parent...
              if parent.trustsme=false then
              begin
                //the parent didn't trust me anyhow
                if GetTickCount64>orphanedSince+30*60*1000 then //30 minutes

                  orphanedSince:=0; //give up and find a new parent

              end
              else
              begin
                if GetTickCount64>orphanedSince+60*60*1000 then //1 hour
                  orphanedSince:=0; //give up and find a new parent


              end;

              if orphanedSince=0 then //give up on the current scan if one was going on
              begin
                savestate:=false;

                OutputDebugString('Giving up on parent');
                if currentscanhasended=false then
                begin
                  currentscanhasended:=true;
                  fTerminatedScan:=true;
                end;
              end;
            end;
          end;

        end;


        if parent.socket<>nil then
        begin
          //send the update command
          //receive the result
          //handle accordingly
          phase:=1;

          if currentscanhasended=false then
          begin
            if length(parentqueue)>0 then
            begin
              if maxTimeToScan>0 then
              begin
                //check if the scan should stop because of the time

                //if so, terminate the scan,  but don't terminate the thread
                if ((GetTickCount64-parent.connecttime) div 1000)>maxTimeToScan then
                begin
                  savestate:=true;
                  fTerminatedScan:=true;  //from now on terminated will return true
                end;
              end;

              if maxResultsToFind>0 then
              begin
                //check if the scan should stop because of the resultcount
                if getTotalResultsFound>maxResultsToFind then
                begin
                  savestate:=true;
                  fTerminatedScan:=true;
                end;
              end;
            end;

          end;


          if terminated and (parent.knowsIAmTerminating=false) then
          begin
            //tell a parent i'm going to disconnect
            parent.socket.WriteByte(PSCMD_PREPAREFORMYTERMINATION);
            parent.socket.flushWrites;

            parent.knowsIAmTerminating:=true;
            if parent.socket.ReadByte<>0 then
              raise exception.create(rsPSCParentDidntRespondProperlyToPSCMDPREPAREFORMYTERMINATION);
          end;


          OutputDebugString('Updating status');


          phase:=2;
          updatemsg.currentscanid:=currentscanid;
          updatemsg.isidle:=ifthen(isIdle,1,0);
          updatemsg.potentialthreadcount:=getPotentialThreadCount;
          updatemsg.actualthreadcount:=getActualThreadCount;
          updatemsg.pathsevaluated:=getTotalPathsEvaluated;
          overflowqueuecs.enter;
          updatemsg.localpathqueuecount:=pathqueuelength+length(overflowqueue);
          overflowqueuecs.leave;

          updatemsg.totalpathQueueCount:=getTotalPathQueueSize;
          updatemsg.queuesize:=length(parentqueue);

          parent.socket.WriteByte(PSCMD_UPDATESTATUS);
          parent.socket.WriteBuffer(updatemsg, sizeof(updatemsg));
          parent.socket.flushWrites;

          lastUpdateSent:=GetTickCount64;

          HandleUpdateStatusReply;
        end;

      except
        on e: exception do
          handleParentException(e.message);
      end;
    finally
      parentcs.leave;
    end;

    //update the queued parents
    phase:=3;
    parentcs.enter;
    try
      i:=0;
      while i<length(parentqueue)-1 do
      begin
        try
          with parentqueue[i].socket do
          begin
            WriteByte(PSCMD_YOUREINTHEQUEUE);
            WriteDWord(i); //position
            WriteDWord(length(parentqueue));
            flushWrites;
          end;

          inc(i);
        except
          //error. Disconnect
          on e: exception do
            HandleParentQueueException(i, e.message);
        end;
      end;
    finally
      parentcs.leave;
    end;


    //parent released, do some cleanup
    phase:=4;
    if currentscanhasended then //cause a flush of the worker threads
    begin
      allfinished:=true;
      localscannerscs.enter;
      try
        for i:=0 to length(localscanners)-1 do
        begin
          if not localscanners[i].Finished then
          begin
            allfinished:=false;
            localscanners[i].SaveStateAndTerminate;
          end;
        end;

        if allfinished then
        begin
          for i:=0 to length(localscanners)-1 do
            localscanners[i].free;

          setlength(localscanners,0);
        end;
      finally
        localscannerscs.Leave;
      end;

      //cleanup uninitialized children
      phase:=5;
      childnodescs.Enter;
      try
        for i:=0 to length(childnodes)-1 do
          if childnodes[i].scandatauploader<>nil then  //a child is busy getting initialized with an scan that has been terminated. Best kill it
          begin
            allfinished:=false;
            childnodes[i].scandatauploader.terminate;
          end;
      finally
        childnodescs.Leave;
      end;

      if allfinished then //not to be confused with isdone. This can be true, even if some children still have paths to process and send data
        UpdateStatus_cleanupScan;
    end;


  except
    on e: exception do
    begin
      OutputDebugString('Caught an unhandled exception in UpdateStatus: '+e.message+'  ('+inttostr(phase)+')');
    end;
  end;

end;


procedure TPointerscanController.setupListenerSocket;
{$ifdef windows}
var
  B: BOOL;
  i: integer;
  sockaddr: TInetSockAddr;

  s: Tfilestream;
  cs: Tcompressionstream;
  {$endif}
begin
  {$ifdef windows}
  //start listening on the given port. The waitForAndHandleNetworkEvent method will accept the connections
  listensocket:=socket(AF_INET, SOCK_STREAM, 0);

  if listensocket=INVALID_SOCKET then
    raise Exception.create(rsPSCFailureCreatingSocket);

  B:=TRUE;
  fpsetsockopt(listensocket, SOL_SOCKET, SO_REUSEADDR, @B, sizeof(B));


  sockaddr.sin_family:=AF_INET;
  sockaddr.sin_port:=htons(listenport);
  sockaddr.sin_addr.s_addr:=INADDR_ANY;
  i:=bind(listensocket, @sockaddr, sizeof(sockaddr));

  if i=SOCKET_ERROR then
    raise exception.create(rsPSCFailureToBindPort+inttostr(listenport));

  i:=listen(listensocket, 32);
  if i=SOCKET_ERROR then
    raise exception.create(rsPSCFailureToListen);


  {$endif}
end;

function TPointerscanController.hasNetworkResponsibility: boolean;
//method to quickly determine if the current scancontroller should bother handling network events
//(mainly used for cleaner code)
begin
  result:=(connector<>nil) or (not initializer) or allowIncomingParent or allowIncomingChildren or (length(childnodes)>0);
  //you could connect to a child but disallow incomming connections
end;


procedure TPointerscanController.execute_nonInitializer;
var
  i: integer;
  devnull: TNullStream;
  alldone: boolean;
begin
  devnull:=TNullStream.create;

  //this is a childnode
  currentscanhasended:=true;
  UseLoadedPointermap:=true;

  //enter the networking loop and wait for the parent(if there is one) to provide messages, or handle incomming connections

  //setup a parent update timer
  if parentupdater=nil then //should be...
  begin
    parentupdater:=TAsyncTimer.create(false);
    parentupdater.OnTimer:=UpdateStatus;
    parentupdater.Interval:=8000+random(4000); //update the parent every 8 to 12 seconds
    parentupdater.enabled:=true;
  end;

  while true do
  begin
    waitForAndHandleNetworkEvent;

    if currentscanhasended then
    begin
      if savestate then
      begin
        try
          sendpathsToParent
        except
          on e:exception do
            handleParentException(rsPSCDuringScanFinishing+e.message);
        end;
      end
      else
        SaveAndClearQueue(devnull);


      alldone:=true;
      localscannersCS.Enter;
      try
        if length(localscanners)>0 then
        begin
          OutputDebugString('There are threads and currentscanhasended=true');
          for i:=0 to length(localscanners)-1 do
          begin
            if localscanners[i].Finished=false then alldone:=false;

            localscanners[i].savestate:=savestate;
            localscanners[i].stop:=true;

            if (savestate=false) or (not localscanners[i].HasResultsPending) then
              localscanners[i].Terminate;
          end;

          if alldone then
          begin
            OutputDebugString('Not anymore');
            for i:=0 to length(localscanners)-1 do
              localscanners[i].Free;

            setlength(localscanners,0);
          end;
        end;


      finally
        localscannersCS.Leave;
      end;



    end;

    if terminated then
    begin

      if fTerminatedScan then
        OutputDebugString('The current scan has been terminated')
      else
        OutputDebugString('The scanner is being terminated');


      currentscanhasended:=true;

      if parent.knowsIAmTerminating then
        sendpathsToParent
      else
      begin
        if parentupdater<>nil then
          parentUpdater.TriggerNow;
      end;

      //send a message to the parent that i'm gone

      parentcs.enter;
      try
        if (currentscanhasended and isDone) or (savestate=false) then
        begin
          OutputDebugString('Terminated and all children are done, or terminated and no save (savestate='+BoolToStr(savestate) +')');
          UpdateStatus_cleanupScan;  //call this here because there may not be a newscan

          if parent.socket<>nil then
          begin
            parent.socket.WriteByte(PSCMD_GOODBYE);
            parent.socket.WriteByte(ifthen(fTerminatedScan,1,0)); //writes 1 if it's a fake termination

            try
              parent.socket.flushWrites;
            except
              //no biggy
              OutputDebugString('The parent disconnected from me before I could tell him goodbye');
            end;
            freeandnil(parent.socket);
          end;

          fTerminatedScan:=false;

          if terminated then break; //actually terminate if the user wanted to. it's safe
        end
        else
          OutputDebugString('Scan not finished yet');

      finally
        parentcs.Leave;
      end;

      if (not savestate) and (not fTerminatedScan) then //really terminated and savestate was false
      begin
        OutputDebugString('Savestate is false and it''s an actual termination. Goodbye');
        break;
      end;
    end;
  end;



  //cleanup some memory
  if parentUpdater<>nil then
  begin
    parentUpdater.Terminate;
    parentUpdater.WaitFor;
    freeandnil(parentUpdater);
  end;

  if connector<>nil then
  begin
    connector.Terminate;
    connector.WaitFor;
    freeandnil(connector);
  end;

  childnodescs.enter;
  try
    for i:=0 to length(childnodes)-1 do
    begin
      if childnodes[i].scanresultDownloader<>nil then
      begin
        childnodes[i].scanresultDownloader.terminate;
        childnodes[i].scanresultDownloader.WaitFor;
        freeandnil(childnodes[i]);
      end;

      if childnodes[i].scanresultDownloader<>nil then
      begin
        childnodes[i].scanresultDownloader.terminate;
        childnodes[i].scanresultDownloader.waitfor;
        freeandnil(childnodes[i].scanresultDownloader);
      end;

      if childnodes[i].resultstream<>nil then
        freeandnil(childnodes[i].resultstream);

      if childnodes[i].socket<>nil then
        freeandnil(childnodes[i].socket);

    end;
    setlength(childnodes,0);
  finally
    childnodescs.leave;
  end;

  if assigned(fOnScanDone) then
    fOnScanDone(self, hasError, errorstring);


  devnull.free;
end;


procedure TPointerscanController.UpdateProgressbarLabel;
begin
  progressbarLabel.caption:=newProgressbarLabel;
end;

procedure TPointerscanController.execute;
var
    i,j: integer;

    result: tfilestream;

    temp: dword;
    tempstring: string;

    f: tfilestream;
    cs: Tcompressionstream;
    ds: Tdecompressionstream;

    {$ifdef windows}
    pa,sa: DWORD_PTR;

    newAffinity: DWORD_PTR;
    {$endif}
    PreferedProcessorList: array of integer; //a list of cpu numbers available to be used. If hyperthreading is on, this will not contain the uneven cpu numbers
    currentcpu: integer;  //index into PreferedProcessorList. If it's bigger than the size, make the affinity equal to PA (do not care, let windows decide)


    pointerlistloaders: array of TPointerlistloader;

    oldfiles: tstringlist;


begin
  result:=nil;
  if terminated then exit;



  try
    if allowIncomingParent or allowIncomingChildren then
      setupListenerSocket;

    if not initializer then
    begin
      execute_nonInitializer;
      exit;

    end;

    //this is an initiator

    allowTempFiles:=true;

    currentscanid:=1+random(MaxInt-2); //random value, not 0


    result:=nil;

    if resumescan then
    begin
      resumeptrfilereader:=TPointerscanresultReader.create(filename);
      resumeptrfilereader.ReleaseFiles;
    end
    else
    begin
      //not a resume, delete the old files
      oldfiles:=tstringlist.create;
      findAllResultFilesForThisPtr(filename, oldfiles);
      for i:=0 to oldfiles.count-1 do
        DeleteFile(oldfiles[i]);

      oldfiles.free;
    end;

    phase:=1;

    if threadcount>0 then
    begin
      if instantrescan then
      begin
        //launch threads to load these data files
        setlength(pointerlistloaders, length(instantrescanfiles));
        for i:=0 to length(pointerlistloaders)-1 do
        begin
          pointerlistloaders[i]:=TPointerlistloader.Create(true);
          pointerlistloaders[i].progressbar:=instantrescanfiles[i].progressbar;
          pointerlistloaders[i].filename:=instantrescanfiles[i].filename;
          pointerlistloaders[i].Start;
        end;
      end;

    end;

    if useLoadedPointermap then
    begin
      if threadcount>0 then  //don't load it yet if there are going to be no threads that want to use it
      begin
        f:=tfilestream.create(LoadedPointermapFilename, fmOpenRead or fmShareDenyNone);
        try
          ds:=Tdecompressionstream.create(f);
          try
            pointerlisthandler:=TReversePointerListHandler.createFromStream(ds, progressbar);
          finally
            ds.free;
          end;
        finally
          f.free;
        end;
      end;
    end
    else
    begin

      progressbar.Position:=0;
      try
        pointerlisthandler:=TReversePointerListHandler.Create(startaddress,stopaddress,not unalligned,progressbar, noreadonly, MustBeClassPointers, acceptNonModuleClasses, useStacks, stacksAsStaticOnly, threadstacks, stacksize, mustStartWithBase, BaseStart, BaseStop, includeSystemModules, RegionFilename, @fShouldQuit);
        progressbar.position:=100;

        if terminated then
        begin
          fOnScanDone(self, false,'');
          exit;
        end;
      except
        on e: exception do
        begin
          haserror:=true;
          errorString:=rsFailureCopyingTargetProcessMemory + '('+e.message+')';

          if assigned(fOnScanDone) then
            fOnScanDone(self, haserror, errorstring);

          terminate;
          exit;
        end;
      end;
    end;

    if instantrescan then
    begin
      for i:=0 to length(pointerlistloaders)-1 do
      begin
        pointerlistloaders[i].WaitFor;
        instantrescanfiles[i].plist:=pointerlistloaders[i].pointerlisthandler;

        if pointerlisthandler<>nil then //should always be true if the pointerlistloaders got initialized
          instantrescanfiles[i].plist.reorderModuleIdList(pointerlisthandler.modulelist);

        pointerlistloaders[i].Free;
      end;
    end;



    if ((threadcount=0) and (UseLoadedPointermap=false)) or generatePointermapOnly then
    begin
      if generatePointermapOnly then
        LoadedPointermapFilename:=filename
      else
        LoadedPointermapFilename:=filename+'.scandata';


      progressbar.Position:=99;
      newProgressbarLabel:=rsSavingPointermap;
      synchronize(UpdateProgressbarLabel);

      f:=tfilestream.create(LoadedPointermapFilename, fmCreate);
      cs:=Tcompressionstream.create(clfastest, f);
      pointerlisthandler.exportToStream(cs);
      cs.free;
      f.free;

      progressbar.Position:=0;

      if generatePointermapOnly then
      begin
        //that's all we need
        if Assigned(fOnScanDone) then
          fOnScanDone(self, haserror, errorstring);

        filename:='';

        terminate;
        exit;
      end;

      UseLoadedPointermap:=true;
    end;

    phase:=2;
    progressbar.Position:=0;



    i:=0;




    //setup the pathqueue
    InitializeEmptyPathQueue;
    InitializeCompressedPtrVariables;


    reverseScanCS:=tcriticalsection.Create;

    setlength(PreferedProcessorList,0);


    //build a list of cpu id's

    {$ifdef windows}
    PA:=0;


    GetProcessAffinityMask(GetCurrentProcess, PA, SA);
    for i:=0 to BitSizeOf(PA)-1 do
    begin
      if getbit(i, PA)=1 then
      begin
        if (i mod 2=0) or (hasHyperThreading=false) then
        begin
          setlength(PreferedProcessorList, length(PreferedProcessorList)+1);
          PreferedProcessorList[length(PreferedProcessorList)-1]:=i;
        end;
      end;
    end;
    {$endif}

    for i:=0 to threadcount-1 do
    begin
      {$ifdef windows}
      if i<length(PreferedProcessorList) then
        addWorkerThread(PreferedProcessorList[i])
      else
      {$endif}
        addWorkerThread;
    end;


    //now do the actual scan
    if assigned(fOnStartScan) then
      synchronize(NotifyStartScan);

    try
      reversescan;


      //when done generate the ptr file if it's not a resume scan

      if not resumescan then //create a new ptr
      begin
        result:=TfileStream.create(filename,fmcreate or fmShareDenyWrite);

        result.writeByte($ce);
        result.writeByte(pointerscanfileversion);

        if (pointerlisthandler=nil) then
        begin
          if UseLoadedPointermap then
          begin
            f:=tfilestream.create(LoadedPointermapFilename, fmOpenRead);
            ds:=Tdecompressionstream.create(f);
            pointerlisthandler:=TReversePointerListHandler.createFromStreamHeaderOnly(ds);
            pointerlisthandler.saveModuleListToResults(result);

            ds.free;
            f.free;

            freeandnil(pointerlisthandler);
          end
          else
            raise exception.create(rsPSCThePointerlisthandlerWasDestroyedWithoutAGoodReason);
        end
        else
          pointerlisthandler.saveModuleListToResults(result);

        //save the maxlevel:
        result.Write(maxlevel,sizeof(maxlevel));


        //save the compressed data fields
        result.writeByte(ifthen(compressedptr, 1, 0));
        if compressedptr then
        begin
          result.writeByte(ifthen(unalligned, 0, 1)); //1 if alligned (I should really rename this one)
          result.writeByte(MaxBitCountModuleIndex);
          result.writeByte(MaxBitCountModuleOffset);
          result.writeByte(MaxBitCountLevel);
          result.writeByte(MaxBitCountOffset);

          result.writeByte(length(mustendwithoffsetlist));
          for i:=0 to length(mustendwithoffsetlist)-1 do
            result.writeDword(mustendwithoffsetlist[i]);
        end;

        result.writebyte(ifthen(mustStartWithBase,1,0));
        if mustStartWithBase then
          result.WriteQWord(BaseStart);

      end;

    finally
      if result<>nil then
        freeandnil(result);

      if reverseScanCS<>nil then
        freeandnil(reverseScanCS);

      if resumePtrFileReader<>nil then
        FreeAndNil(resumePtrFileReader);

      if initializer then
      begin
        //cleanup the connections
        if connector<>nil then
          connector.terminate;

        childnodescs.enter;
        try
          for i:=0 to length(childnodes)-1 do
          begin
            if childnodes[i].scanresultDownloader<>nil then
            begin
              childnodes[i].scanresultDownloader.Terminate;
              childnodes[i].scanresultDownloader.WaitFor;
              freeandnil(childnodes[i].scanresultDownloader);
            end;

            if childnodes[i].scandatauploader<>nil then
            begin
              childnodes[i].scandatauploader.Terminate;
              childnodes[i].scandatauploader.WaitFor;
              freeandnil(childnodes[i].scandatauploader);
            end;


            if childnodes[i].resultstream<>nil then
              freeandnil(childnodes[i].resultstream);

            if childnodes[i].socket<>nil then
              freeandnil(childnodes[i].socket);
          end;

          if connector<>nil then
          begin
            connector.WaitFor;
            freeandnil(connector);
          end;

          if listensocket<>INVALID_SOCKET then
          begin
            closesocket(listensocket);
            listensocket:=INVALID_SOCKET;
          end;

        finally
          childnodescs.leave;
        end;

      end;

    end;






  except
    on e: exception do
    begin
      haserror:=true;
      errorstring:='StaticScanner:'+e.message;
      if assigned(fOnScanDone) then
        fOnScanDone(self, haserror, errorstring);

      terminate;
    end;
  end;


end;

procedure TPointerscanController.WaitForHello(sockethandle: Tsocket; var msg: TPSHelloMsg);
{
Waits for the hello message on the given socket
Raise TSocketException
}
var command: byte;
    namelength: byte;
    name: pchar;
    result: dword;
    i: integer;
begin
  name:=nil;

  receive(sockethandle, @command, 1);
  if command<>PSCMD_HELLO then
  begin
    OutputDebugString('WaitForHello received '+inttostr(command));
    raise TSocketException.Create(rsPSCInvalidCommandWhileWaitingForHello); //invalid command
  end;

  receive(sockethandle, @namelength, 1);

  getmem(name, namelength+1);
  try
    receive(sockethandle, name, namelength);
    name[namelength]:=#0;
    msg.publicname:=name;
  finally
    FreeMemAndNil(name);
  end;

  receive(sockethandle, @msg.scannerid, sizeof(msg.scannerid));

  //check if the scannerid is in my list of children
  result:=random(MaxInt);
  msg.currentscanid:=currentscanid+1;
  childnodescs.Enter;
  try
    for i:=0 to length(childnodes)-1 do
    begin
      if childnodes[i].childid=msg.scannerid then //this is a returning child
      begin
        msg.currentscanid:=currentscanid;
        result:=currentscanid;
        break;
      end;
    end;
  finally
    childnodescs.Leave;
  end;

  send(sockethandle, @result, sizeof(result)); //got till here so seems to be ok

  namelength:=length(publicname);
  send(sockethandle, @namelength, sizeof(namelength));
  send(sockethandle, @publicname[1], namelength);
end;

procedure TPointerscanController.SayHello(potentialparent: PPointerscanControllerParent);
{
Says hello to the parent
Raises TSocketException
}
var
  result: byte;
begin
  with potentialparent^.socket do
  begin
    WriteByte(PSCMD_HELLO);
    WriteAnsiString8(publicname);
    WriteDword(scannerid);
    flushWrites;

    potentialparent.scanid:=ReadDWord;
    potentialparent.name:=ReadAnsiString8;
  end;
end;

procedure TPointerscanController.ConnectorConnect(sender: TObject; sockethandle: TSocket; IBecameAParent: boolean; entry: PConnectEntry);
{
Handles an connect event. Either from the connector thread, or called by the controller after handing an incomming connect
Raises TSocketException on error
}
{$ifdef windows}
var i: integer;
    hellomsg: TPSHelloMsg;
    child: PPointerscancontrollerchild;
    bm: u_long;

    ipname: TSockAddrIn;
    len: Longint;
    {$endif}

begin

{$ifdef windows}
  child:=nil;

  //mark the socket as non blocking
{$ifdef windows}
  bm:=0;
  ioctlsocket(sockethandle, longint(FIONBIO), bm);
{$else}
  fcntl(fSocket, F_SETFL, fcntl(socketfd, F_GETFL, 0) | O_NONBLOCK);
{$endif}



  if IBecameAParent then
  begin
    //add to the child list if it's a new entry, else update the existing one

    //wait for it to tell me the initialization message
    //receive the hello message
    WaitForHello(sockethandle, hellomsg); //will raise an network exception on error

    //hellomsg now contains a scannerid and scanid of the child

    childnodescs.enter;

    if (hellomsg.currentscanid<>0) and (hellomsg.currentscanid=currentscanid) then //it's a returning child, find it
      for i:=0 to length(childnodes)-1 do
      begin
        if childnodes[i].childid=hellomsg.scannerid then
        begin
          //found it
          if childnodes[i].socket<>nil then
            freeandnil(childnodes[i].socket);

          childnodes[i].socket:=TSocketStream.create(sockethandle); //connect to this socket
          child:=@childnodes[i];
          break;
        end;
      end;

    if child=nil then //not in the list or invalid childid (weird...)
    begin
      //add it as a new entry
      SetLength(childnodes, length(childnodes)+1);
      child:=@childnodes[length(childnodes)-1];

      child.socket:=TSocketStream.create(sockethandle);
      child.childid:=nextchildid;
      child.trusted:=autoTrustIncomingChildren;
      inc(nextchildid);
    end;

    if (entry<>nil) then  //attached by the pointerscanconnector (this info is used to reconnect on socket exceptions)
    begin
      child.iConnectedTo:=true;
      child.connectdata.ip:=entry.ip;
      child.connectdata.port:=entry.port;
      child.connectdata.password:=entry.password;
      child.trusted:=entry.trusted;
    end;

    child.LastUpdateReceived:=GetTickCount64;



    len:=sizeof(ipname);
    if getpeername(sockethandle, ipname, len)<>SOCKET_ERROR then
    begin
      child.ip:=inttostr(byte(ipname.sin_addr.S_un_b.s_b1))+'.'+inttostr(byte(ipname.sin_addr.S_un_b.s_b2))+'.'+
                inttostr(byte(ipname.sin_addr.S_un_b.s_b3))+'.'+inttostr(byte(ipname.sin_addr.S_un_b.s_b4));

      child.port:=ntohs(ipname.sin_port);
    end;



    childnodescs.Leave;
  end
  else
  begin
    //this wants to be a parent

    parentcs.Enter;
    try
      //add it to the queue

      try
        //add this to the parentqueue
        setlength(parentqueue, length(parentqueue)+1);
        parentqueue[length(parentqueue)-1].socket:=TSocketStream.create(sockethandle);
        parentqueue[length(parentqueue)-1].socket.timeout:=60; //change timeout to 60 seconds when talking to the parent (he might be busy dealing with a slow child)

        if entry<>nil then
        begin
          with parentqueue[length(parentqueue)-1] do
          begin
            iConnectedTo:=true;
            connectdata.ip:=entry.ip;
            connectdata.port:=entry.port;
            connectdata.password:=entry.password;
          end;
        end
        else
          parentqueue[length(parentqueue)-1].iConnectedTo:=false;

        len:=sizeof(ipname);
        if getpeername(sockethandle, ipname, len)<>SOCKET_ERROR then
        begin
          parentqueue[length(parentqueue)-1].ip:=inttostr(byte(ipname.sin_addr.S_un_b.s_b1))+'.'+inttostr(byte(ipname.sin_addr.S_un_b.s_b2))+'.'+
                    inttostr(byte(ipname.sin_addr.S_un_b.s_b3))+'.'+inttostr(byte(ipname.sin_addr.S_un_b.s_b4));

          parentqueue[length(parentqueue)-1].port:=ntohs(ipname.sin_port);
        end;




          //you have a new daddy! Say hello to him
        OutputDebugString('Going to say hello');
        sayHello(@parentqueue[length(parentqueue)-1]); //'Hello daddy'...creepy voice
        OutputDebugString('said hello');

      except
        on e:exception do
        begin
          OutputDebugString('Error while accepting parent:'+e.message);
          if parentqueue[length(parentqueue)-1].socket<>nil then
            freeandnil(parentqueue[length(parentqueue)-1].socket);

          setlength(parentqueue, length(parentqueue)-1);
        end;
      end;

    finally
      parentcs.Leave;
    end;


    if parent.socket=nil then //make a new parent if possible
      UpdateStatus(self);
  end;
  {$endif}
end;

procedure TPointerscanController.BecomeChildOfNode(ip: string; port: word; password: string);
begin
  connectorcs.Enter;
  try
    if connector=nil then
      connector:=TPointerscanConnector.create(ConnectorConnect);


    connector.AddConnection(ip, port, password, false);
  finally
    connectorcs.leave;
  end;
end;

procedure TPointerscanController.BecomeParentOfNode(ip: string; port: word; password: string; trusted: boolean=false);
var
  i: integer;
  l: TConnectEntryArray;
begin
  connectorcs.Enter;
  try
    if connector=nil then
      connector:=TPointerscanConnector.create(ConnectorConnect);

    childnodescs.enter;
    try
      for i:=0 to length(childnodes)-1 do
      begin
        if (childnodes[i].socket<>nil) and childnodes[i].iConnectedTo and (uppercase(ip)=uppercase(childnodes[i].connectdata.ip)) and (port=childnodes[i].connectdata.port) then
          raise exception.create(rsPSCAlreadystillConnectedToThisChild);
      end;
    finally
      childnodescs.leave;
    end;

    connector.GetList(l);
    for i:=0 to length(l)-1 do
    begin
      if (uppercase(ip)=uppercase(l[i].ip)) and (port=l[i].port) then
        connector.MarkEntryForDeletion(l[i].id);
    end;

    connector.AddConnection(ip, port, password, true, trusted);
  finally
    connectorcs.leave;
  end;
end;

procedure TPointerscanController.ProcessScanDataFiles;
{
Loads the scandata streams into memory
}
var
  pointerlistloaders: array of TPointerlistloader;
  currentstream: Tstream;
  ds: Tdecompressionstream;
  i: integer;
begin

  //first the rescan streams (they can be done async)
  setlength(pointerlistloaders, length(instantrescanfiles));
  for i:=0 to length(pointerlistloaders)-1 do
  begin
    pointerlistloaders[i]:=TPointerlistloader.Create(true);
    pointerlistloaders[i].progressbar:=instantrescanfiles[i].progressbar;
    pointerlistloaders[i].filename:=instantrescanfiles[i].filename;
    pointerlistloaders[i].memoryfilestream:=instantrescanfiles[i].memoryfilestream;
    pointerlistloaders[i].Start;
  end;


  //while they are busy do the main stream (blocking)
  if allowtempfiles then //open the filestream
    currentstream:=TFileStream.create(LoadedPointermapFilename, fmOpenRead or fmShareDenyNone)
  else
    currentstream:=pointerlisthandlerfile;


  currentstream.position:=0;

  ds:=Tdecompressionstream.create(currentstream);
  try
    if pointerlisthandler<>nil then
      freeandnil(pointerlisthandler);

    pointerlisthandler:=TReversePointerListHandler.createFromStream(ds);
  finally
    ds.free;
    if allowtempfiles then
      currentstream.free;
  end;

  for i:=0 to length(pointerlistloaders)-1 do
  begin
    pointerlistloaders[i].WaitFor;
    instantrescanfiles[i].plist:=pointerlistloaders[i].pointerlisthandler;

    if pointerlisthandler<>nil then //should always be true if the pointerlistloaders got initialized
      instantrescanfiles[i].plist.reorderModuleIdList(pointerlisthandler.modulelist);

    pointerlistloaders[i].Free;
  end;

end;

procedure TPointerscanController.addworkerThread(preferedprocessor: integer=-1);
var
  scanner: TPointerscanWorker;
  j: integer;
  {$ifdef windows}
  NewAffinity: DWORD_PTR;
  {$endif}
  scanfileid: integer;
  downloadtime: qword;

begin
  if pointerlisthandler=nil then
    processScanDataFiles;

  if MaxBitCountModuleIndex=0 then
    InitializeCompressedPtrVariables;

  if initializer then
  begin
    localscannersCS.enter;
    scanfileid:=length(localscanners);
    localscannersCS.leave;

    scanner:=TPointerscanWorkerLocal.Create(true, self.filename+'.results.'+inttostr(scanfileid));
  end
  else
  begin
    scanner:=TPointerscanWorkerNetwork.Create(true);
    TPointerscanWorkerNetwork(scanner).OnFlushResults:=UploadResults;

    if downloadingscandata_stoptime<>downloadingscandata_starttime then
      TPointerscanWorkerNetwork(scanner).FlushSize:=floor((downloadingscandata_total / ((downloadingscandata_stoptime-downloadingscandata_starttime)/1000)) * 5) //just an arbitrary value, it doesn't mean much.
    else
      TPointerscanWorkerNetwork(scanner).FlushSize:=15*1024*1024;  //else just use the default size

  end;

  scanner.OnException:=workerexception;
  scanner.overflowqueuewriter:=OverflowQueueWriter;
  scanner.maxlevel:=maxlevel;
  scanner.structsize:=sz;
  scanner.noLoop:=noLoop;
  scanner.pointerlisthandler:=pointerlisthandler;
  scanner.pathqueueSemaphore:=pathqueueSemaphore;
  scanner.pathqueuelength:=@pathqueuelength;
  scanner.pathqueueCS:=pathqueueCS;
  scanner.pathqueue:=@pathqueue[0];
  scanner.OutOfDiskSpace:=@outofdiskspace;

  scanner.mustEndWithSpecificOffset:=mustEndWithSpecificOffset;
  scanner.mustendwithoffsetlist:=mustendwithoffsetlist;
  scanner.useHeapData:=useHeapData;
  scanner.useOnlyHeapData:=useHeapData;
  scanner.onlyOneStaticInPath:=onlyOneStaticInPath;




  scanner.Priority:=scannerpriority;

  setlength(scanner.tempresults,maxlevel);

  if noloop then
    setlength(scanner.valuelist,maxlevel);

  scanner.staticonly:=staticonly;
  scanner.noLoop:=noLoop;

  scanner.LimitToMaxOffsetsPerNode:=LimitToMaxOffsetsPerNode;
  scanner.MaxOffsetsPerNode:=MaxOffsetsPerNode;

  scanner.alligned:=not self.unalligned;



  //pick a usable cpu. Use the process affinity mask to pick from
  {$ifdef windows}
  if preferedprocessor<>-1 then
  begin
    NewAffinity:=1 shl preferedprocessor;
    NewAffinity:=SetThreadAffinityMask(scanner.Handle, NewAffinity);
  end;
  {$endif}

  scanner.NegativeOffsets:=negativeOffsets;
  scanner.compressedptr:=compressedptr;
  scanner.MaxBitCountModuleIndex:=MaxBitCountModuleIndex;
  scanner.MaxBitCountModuleOffset:=MaxBitCountModuleOffset;
  scanner.MaxBitCountLevel:=MaxBitCountLevel;
  scanner.MaxBitCountOffset:=MaxBitCountOffset;

  scanner.mustendwithoffsetlistlength:=length(mustendwithoffsetlist);


  //rescan data if applicable
  scanner.instantrescan:=instantrescan;
  if instantrescan then
  begin
    scanner.instantrescanlistcount:=length(instantrescanfiles);
    setlength(scanner.instantrescanlist, length(instantrescanfiles));
    setlength(scanner.instantrescanaddress, length(instantrescanfiles));
    for j:=0 to length(instantrescanfiles)-1 do
    begin
      scanner.instantrescanlist[j]:=instantrescanfiles[j].plist;
      scanner.instantrescanaddress[j]:=instantrescanfiles[j].address;
    end;
  end;




  localscannersCS.enter;
  try
    setlength(localscanners, length(localscanners)+1);
    localscanners[length(localscanners)-1]:=scanner;
  finally
    localscannersCS.Leave;
  end;

  scanner.start;
end;

procedure TPointerscanController.changeWorkerPriority(priority: TThreadPriority);
var i: integer;
begin
  localscannersCS.enter;
  try
    for i:=0 to length(localscanners)-1 do
      localscanners[i].Priority:=priority;
  finally
    localscannerscs.Leave;
  end;
end;

procedure TPointerscanController.removeWorkerThread;
begin
  localscannersCS.enter;
  try
    if length(localscanners)>0 then
    begin
      localscanners[length(localscanners)-1].SaveStateAndTerminate;
      localscanners[length(localscanners)-1].WaitFor;

      inc(fTotalPathsEvaluatedByErasedChildren, localscanners[length(localscanners)-1].pathsEvaluated);
      localscanners[length(localscanners)-1].free;
      setlength(localscanners, length(localscanners)-1);
    end;
  finally
    localscannersCS.leave;
  end;
end;

procedure TPointerscanController.OverflowQueueWriter(sender: TObject; PathQueueElement: TPathQueueElement);
{
function called by workers when they don't have time to wait for the queue(disk full or terminated+saving)
}
var i: integer;
begin
  overflowqueuecs.enter;
  try
    i:=length(overflowqueue);
    setlength(overflowqueue, i+1);
    overflowqueue[i].startlevel:=PathQueueElement.startlevel;
    overflowqueue[i].valuetofind:=PathQueueElement.valuetofind;

    setlength(overflowqueue[i].tempresults, length(PathQueueElement.tempresults));
    copymemory(@overflowqueue[i].tempresults[0], @PathQueueElement.tempresults[0], sizeof(dword)*length(PathQueueElement.tempresults));

    setlength(overflowqueue[i].valuelist, length(PathQueueElement.valuelist));
    copymemory(@overflowqueue[i].valuelist[0], @PathQueueElement.valuelist[0], sizeof(PtrUInt)*length(PathQueueElement.valuelist));
  finally
    overflowqueuecs.leave;
  end;
end;

procedure TPointerscanController.terminateAndSaveState;
{
Save config data that isn't saved with the scandata file:

maxlevel: dword
structsize: dword; //sz
totalpathsevaluated: qword
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
  i: integer;
  s: tfilestream;

begin
  s:=tfilestream.create(filename+'.resume.config', fmcreate);

  s.WriteDWord(maxlevel);
  s.WriteDWord(sz);
  s.WriteQWord(totalpathsevaluated);
  s.writebyte(ifthen(compressedptr,1,0));
  s.writebyte(ifthen(unalligned,1,0));
  s.Writebyte(ifthen(staticonly,1,0));
  s.writebyte(ifthen(noLoop,1,0));
  s.writebyte(ifthen(muststartwithbase,1,0));
  s.writebyte(ifthen(LimitToMaxOffsetsPerNode,1,0));
  s.writebyte(ifthen(onlyOneStaticInPath,1,0));
  s.writebyte(ifthen(instantrescan,1,0));
  s.writebyte(ifthen(mustEndWithSpecificOffset,1,0));
  s.WriteDWord(maxoffsetspernode);
  s.WriteQWord(basestart);
  s.WriteQWord(basestop);

  s.WriteDWord(length(mustendwithoffsetlist));
  for i:=0 to length(mustendwithoffsetlist)-1 do
     s.WriteDword(mustendwithoffsetlist[i]);

  s.WriteDword(length(instantrescanfiles));
  for i:=0 to length(instantrescanfiles)-1 do
  begin
    s.WriteAnsiString(instantrescanfiles[i].filename);
    s.WriteQWord(instantrescanfiles[i].address);
  end;

  s.free;

  savestate:=true;
  terminate;
end;

procedure TPointerscancontroller.Terminate;
begin
  fShouldQuit:=true;
  tthread(self).Terminate;
end;

constructor TPointerscanController.create(suspended: boolean);
begin
  pointersize:=processhandler.pointersize;

  listensocket:=INVALID_SOCKET;
  parent.socket:=nil;

  parentcs:=tcriticalsection.create;
  childnodescs:=tcriticalsection.create;

  connectorcs:=TCriticalSection.create;

  localscannersCS:=TCriticalSection.create;

  pathqueueCS:=TCriticalSection.create;
  {$ifdef windows}
  pathqueueSemaphore:=CreateSemaphore(nil, 0, MAXQUEUESIZE, nil);
  {$else}
  pathqueueSemaphore:=TSemaphore.create(MAXQUEUESIZE,true);
  {$endif}

  overflowqueuecs:=TCriticalSection.create;

  nextchildid:=1+random(MaxInt); //just a random start

  inherited create(suspended);
end;

destructor TPointerscanController.destroy;
var i: integer;
begin
  terminate;
  waitfor;

  if connector<>nil then
  begin
    connector.Terminate;
    connector.WaitFor;
    freeandnil(connector);
  end;

  if connectorcs<>nil then
    freeandnil(connectorcs);

    {
  if sockethandle<>-1 then
  begin
    CloseSocket(sockethandle);
    sockethandle:=-1;
  end; }

  if listensocket<>THandle(-1) then
  begin
    closesocket(listensocket);
    listensocket:=THandle(-1);
  end;

  if instantrescan then
  begin
    for i:=0 to length(instantrescanfiles)-1 do
    begin
      if instantrescanfiles[i].plist<>nil then
        freeandnil(instantrescanfiles[i].plist);

      if instantrescanfiles[i].progresslabel<>nil then
        freeandnil(instantrescanfiles[i].progresslabel);

      if instantrescanfiles[i].progressbar<>nil then
        freeandnil(instantrescanfiles[i].progressbar);
    end;

  end;

  if resumeptrfilereader<>nil then
    freeandnil(resumeptrfilereader);

  if parentcs<>nil then
    freeandnil(parentcs);

  if childnodescs<>nil then
    freeandnil(childnodescs);

  if localscannersCS<>nil then
    freeandnil(localscannersCS);

  if pointerlisthandler<>nil then
    freeandnil(pointerlisthandler);

  if overflowqueuecs<>nil then
    freeandnil(overflowqueuecs);

  if pathqueueCS<>nil then
    freeandnil(pathqueueCS);

  if parentUpdater<>nil then
    freeandnil(parentUpdater);



  {$ifdef windows}
  closehandle(pathqueueSemaphore);
  {$else}
  if pathqueueSemaphore<>nil then
    freeandnil(pathqueueSemaphore);
  {$endif}


  //clean up other stuff
  inherited destroy;
end;

end.

