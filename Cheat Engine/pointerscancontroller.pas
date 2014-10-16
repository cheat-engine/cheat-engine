unit PointerscanController;

{$mode delphi}

interface

uses
  Windows, Classes, SysUtils, StdCtrls, ComCtrls, Sockets, syncobjs,
  resolve, math, pointervaluelist,PointerscanWorker, PointerscanStructures,
  pointeraddresslist, PointerscanresultReader, cefuncproc, newkernelhandler,
  zstream, PointerscanConnector, PointerscanNetworkStructures, WinSock2,
  CELazySocket, AsyncTimer, MemoryStreamReader;


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
    parentqueue: array of TPointerscanControllerParent;

    orphanedSince: qword; //time since the last parent disconnected.  Use this to decide when to give up on it and continue from the queue


    childnodescs: tcriticalsection; //for adding/removing from the array
    childnodes: array of TPointerscanControllerChild;

    parentUpdater: TAsyncTimer;
    lastUpdateSent: integer;


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




    procedure notifyStartScan;
    function getMaxBitCount(absolutemaxvalue: dword; Signed: boolean): dword;

    procedure EatFromOverflowQueueIfNeeded;

  {  procedure launchWorker; //connect to the server
    procedure launchServer; //start listening on the specific port
    procedure broadcastscan; //sends a broadcast to the local network and the potentialWorkerList

    function doDistributedScanningLoop: boolean;  //actually doDistributedScanningLoopIteration
    function doDistributedScanningWorkerLoop: boolean;
    function doDistributedScanningServerLoop: boolean;
    procedure DispatchCommand(s: Tsocket; command: byte);  }

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


    procedure cleanupScan;

    procedure HandleUpdateStatusReply_DoNewScan;
    procedure HandleUpdateStatusReply_GiveMeYourPaths;
    procedure HandleUpdateStatusReply_HereAreSomePaths;
    procedure HandleUpdateStatusReply_CurrentScanHasEnded;
    procedure HandleUpdateStatusReply;

    procedure UpdateStatus(sender: tobject); //sends the current status to the parent
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

    maxResultsToFind: qword;
    maxTimeToScan: qword;
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
    sz: integer;
    maxlevel: integer;
    unalligned: boolean;

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

    compressedptr: boolean;
    MaxBitCountModuleIndex: dword;
    MaxBitCountLevel: dword;
    MaxBitCountOffset: dword;

    isdone: boolean;
    staticonly: boolean; //for reverse

    hasError: boolean;
    errorString: string;

    LoadedPointermapFilename: string;
    UseLoadedPointermap: boolean;

    pathqueuelength: integer;
    pathqueue: TMainPathQueue;
    pathqueueCS: TCriticalSection; //critical section used to add/remove entries
    pathqueueSemaphore: THandle; //Event to notify sleeping threads to wake up that there is a new path in the queue

    overflowqueuecs: Tcriticalsection;
    overflowqueue: TDynPathQueue; //this queue will hold a number of paths that the server/worker received too many. (e.g a request for paths was made, but by the time the paths are received, the pathqueue is full again) It's accessed by the controller thread only

     {
    distributedScanning: boolean; //when set to true this will open listening port where other scanners can connect to
    distributedport: word; //port used to listen on if distributed scanning is enabled
    distributedScandataDownloadPort: word;

    distributedWorker: boolean; //set if it's a worker connecting to a server
    distributedServer: string;

    broadcastThisScanner: boolean;
    potentialWorkerList: array of THostAddr;

    workersPathPerSecondTotal: qword;
    workersPointersfoundTotal: qword;     }

    outofdiskspace: boolean;

    instantrescan: boolean;
    instantrescanfiles:array of record
      filename: string;
      memoryfilestream: TMemoryStream;
      address: ptruint;
      plist: TPointerListHandler;
      progressbar: TProgressBar;
      progresslabel: TLabel;
    end;

    resumescan: boolean; //if true load the pointermap from filename.resume.scandata and the queue from filename.resume.queue
    resumefilelist: tstringlist; //list containing the files of the previous scan. If less threads are created make sure at least all these files stay part of the .ptr file
    resumePtrFilename: string;

    downloadingscandata: boolean; //true while scandata is being downloaded
    downloadingscandata_received: qword;
    downloadingscandata_total: qword;

    function UploadResults(decompressedsize: integer; s: tmemorystream): boolean; //sends the given results (compressed) to the parent.

    procedure BecomeChildOfNode(ip: string; port: word; password: string);
    procedure BecomeParentOfNode(ip: string; port: word; password: string; trusted: boolean=false);


    procedure changeWorkerPriority(priority: TThreadPriority);
    procedure removeWorkerThread;
    procedure addWorkerThread(preferedprocessor: integer=-1);

    function hasNetworkResponsibility: boolean;

    function isIdle: boolean;
    procedure getMinAndMaxPath(var minpath: TDynDwordArray; var maxpath: TDynDwordArray);
    procedure getThreadStatuses(s: TStrings);
    function getTotalTimeWriting: qword;
    function getTotalPathsEvaluated: qword;
    function getTotalResultsFound: qword;
    function getTotalPathQueueSize: integer;
    function getPointerlistHandlerCount: qword;
    function getActualThreadCount: integer;
    function getTotalThreadCount: integer;
    procedure getConnectingList(var l: TConnectEntryArray);
    procedure getConnectionList(var l: TConnectionEntryArray);
    procedure getParentData(var d: TPublicParentData);

    procedure TerminateAndSaveState;
    procedure execute; override;
    constructor create(suspended: boolean);
    destructor destroy; override;

    property starttime: Qword read fstarttime;
    property totalpathsevaluated: qword read getTotalPathsEvaluated;
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

uses PointerscanNetworkCommands, ValueFinder, ProcessHandlerUnit;

resourcestring
  rsFailureCopyingTargetProcessMemory = 'Failure copying target process memory';

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
begin
  //first get the socketstream
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
    if scanid<>fcontroller.currentscanid then raise TSocketException.create('The child is sending me results of a different scan');

    streamsize:=s.ReadDWord;
    decompressedStreamSize:=s.ReadDword;

    ms:=TMemoryStream.create;
    ms.Size:=streamsize;
    ms.CopyFrom(s, streamsize);


    //tell the child it was received
    s.WriteByte(0);
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
                    resultstream:=TFileStream.Create(fcontroller.filename+'.child'+inttostr(fChildID), fmCreate);
                    fcontroller.childnodes[i].resultstream:=resultstream;
                  end;
                  break;
                end;
              end;

            finally
              fcontroller.childnodescs.Leave;
            end;

            if resultstream<>nil then
              resultstream.CopyFrom(ds, decompressedStreamSize);


          finally
            ds.free;
          end;
        end
        else
        begin
          while fcontroller.UploadResults(decompressedStreamSize, ms)=false do
          begin
            sleep(250);
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
  percentage: integer;
  i: integer;
begin
  percentage:=ceil(sent/total*100);

  fcontroller.childnodescs.Enter;
  try
    for i:=0 to length(fcontroller.childnodes)-1 do
    begin
      if fcontroller.childnodes[i].childid=fchildid then
      begin

        fcontroller.childnodes[i].receivingScanDataProgress:=percentage;
        fcontroller.childnodes[i].receivingScanDataSpeed:=ceil((sent/(GetTickCount64-starttime))*1000); //bytes/s
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
          break;
        end;
      end;
    finally
      fcontroller.childnodescs.Leave;
    end;
  end;

  if s=nil then exit; //

  if self.terminated then exit;

  try
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
        raise exception.create('Impossible error: UseLoadedPointermap was false when a child message got handled');


      s.flushWrites;

      if self.terminated then exit;


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
            f[i]:=TMemoryStreamReader(instantrescanfiles[i].memoryfilestream);
        end;




        totalsize:=0;
        for i:=0 to length(f)-1 do
          totalsize:=totalsize+f[i].Size;

        s.WriteQWord(totalsize);   //totalsize

        self.starttime:=GetTickCount64;


        UpdateChildProgress(sent, totalsize);
        //update the child progress

        sent:=0;

        for i:=0 to length(instantrescanfiles)-1 do
        begin
          //send a header
          s.WriteDWord(ifthen(i=0,0,instantrescanfiles[i-1].address));  //not important for the main  (automaticaddress)
          s.WriteQWord(f[i].Size);

          //and now send the file (in 64KB blocks)

          while f[i].position<f[i].size do
          begin
            inc(sent, s.CopyFrom(f[i], min(65536, f[i].size-f[i].position)));

            s.flushWrites;
            UpdateChildProgress(sent, totalsize);

            if self.terminated then exit;
          end;

        end;

      finally
        for i:=0 to length(f)-1 do
          if f[i]<>nil then
            f[i].free;

        setlength(f,0);
      end;


      if s.ReadByte<>0 then
        raise TSocketException.create('Invalid result received after uploading the scanresults');

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
      if childnodes[i].idle then
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

  localscannersCS.Enter;
  try
    for i:=0 to length(localscanners)-1 do
    begin
      if localscanners[i].isdone=false then
        exit;
    end;
  finally
    localscannersCS.Leave;
  end;

  //the children are idle and all queue entries are empty and no thread is currently doing anything

  result:=true; //I guess i'm idle
end;

procedure TPointerscanController.notifyStartScan;
begin
  if assigned(fOnStartScan) then
    fOnStartScan(self);
end;

function TPointerscanController.getActualThreadCount: integer;
begin
  localscannerscs.Enter;
  result:=length(localscanners);
  localscannerscs.Leave;
end;

function TPointerscanController.getTotalThreadCount: integer;
var i: integer;
begin
  result:=getActualThreadCount;

  childnodescs.enter;
  try
    for i:=0 to length(childnodes)-1 do
      inc(result, childnodes[i].totalthreadcount);
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
begin
  s.Clear;
  localscannersCS.enter;
  try
    for i:=0 to length(localscanners)-1 do
    begin
      if localscanners[i].hasTerminated then
        s.add(IntToStr(i)+':Terminated')
      else
      if localscanners[i].isdone then
        s.add(IntToStr(i)+':Sleeping')
      else
      if localscanners[i].isFlushing then
        s.add(IntToStr(i)+':Writing to disk')
      else
        s.add(IntToStr(i)+':Working');
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

function TPointerscanController.getTotalPathsEvaluated: qword;
var i: integer;
begin
  result:=getTotalPathsEvaluatedbyChildren;

  localscannersCS.enter;
  try
    for i:=0 to length(localscanners)-1 do
      inc(result, localscanners[i].PathsEvaluated);
  finally
    localscannersCS.leave;
  end;
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
      l[i].port:=childnodes[i].port;
      l[i].isidle:=childnodes[i].idle;
      l[i].threadcount:=childnodes[i].totalthreadcount;
      l[i].trustedconnection:=childnodes[i].trusted;
      l[i].pathquesize:=childnodes[i].pathqueuesize;
      l[i].totalpathqueuesize:=childnodes[i].totalpathqueuesize;
      l[i].pathsevaluated:=childnodes[i].totalPathsEvaluated;
      l[i].resultsfound:=childnodes[i].resultsfound;
      l[i].disconnected:=childnodes[i].socket=nil;
      l[i].uploadingscandata:=childnodes[i].scandatauploader<>nil;
      l[i].uploadscandataprogress:=childnodes[i].receivingScanDataProgress;
      l[i].uploadscandataspeed:=childnodes[i].receivingScanDataSpeed;
      l[i].downloadingResuls:=childnodes[i].scanresultDownloader<>nil;
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
var oldstart: integer;
  i: integer;
begin
  overflowqueuecs.Enter;
  try
    oldstart:=length(overflowqueue);
    setlength(overflowqueue, length(overflowqueue)+length(paths));
    for i:=0 to length(paths)-1 do
      overflowqueue[oldstart+i]:=paths[i];
  finally
    overflowqueuecs.Leave;
  end;
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
  for i:=0 to maxlevel+1 do
    s.writedword(element.tempresults[i]);

  if noloop then
    for i:=0 to maxlevel+1 do
      s.writedword(element.valuelist[i]);
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

  for i:=0 to maxlevel+1 do
    element.tempresults[i]:=s.ReadDWord;

  if noloop then
  begin
    if length(element.valuelist)<maxlevel+1 then
      setlength(element.valuelist, maxlevel+1);

    for i:=0 to maxlevel+1 do
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
  listsize: integer;
begin
  overflowqueuecs.enter;
  try
    if (length(overflowqueue)>0) and (pathqueuelength<MAXQUEUESIZE-1) then //I could use some paths
    begin
      listsize:=sizeof(dword)*(maxlevel+1);

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
              copymemory(@pathqueue[i].valuelist[0], @overflowqueue[oi].valuelist[0], listsize);
          end;

          inc(pathqueuelength, pathsToCopy);
          ReleaseSemaphore(pathqueueSemaphore, pathsToCopy, nil);
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
      {
obsolete
procedure TPointerscanController.broadcastscan;
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

procedure TPointerscanController.launchServer;
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
        pointerlisthandler.exportToStream(cs);
      finally
        cs.free;
      end;
    finally
      s.free;
    end;
  end;

  //do this in a separate thread as to not cause a bottleneck for actual scanners waiting for paths, and slow downloaders
  scandatauploader:=TScanDataUploader.create(loadedPointermapFilename, distributedScandataDownloadPort);



end;



procedure TPointerscanController.launchWorker;
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
  compressedptr:=sp.compressedptr<>0;
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

function TPointerscanController.doDistributedScanningWorkerLoop: boolean;
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
  updateworkerstatusmessage.pointersfound:=0;//scount;
  updateworkerstatusmessage.pathsPerSecond:=0;//trunc((totalpathsevaluated / (gettickcount-starttime))*1000);
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

procedure TPointerscanController.DispatchCommand(s: TSocket; command: byte);
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
        getScanParametersOut.compressedptr:=ifthen(compressedptr, 1, 0);
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
              RequestQueueMessage.max:=MAXQUEUESIZE-(pathqueuelength+length(overflowqueue));
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

function TPointerscanController.doDistributedScanningServerLoop: boolean;
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


function TPointerscanController.doDistributedScanningLoop: boolean;
begin
  if distributedWorker then
    result:=doDistributedScanningWorkerLoop
  else
    result:=doDistributedScanningServerLoop;
end;


         }

function TPointerscanController.ismatchtovalue(p: pointer): boolean;
begin
  case valuetype of
    vtDword: result:=pdword(p)^=valuescandword;
    vtSingle: result:=(psingle(p)^>=valuescansingle) and (psingle(p)^<valuescansinglemax);
    vtDouble: result:=(pdouble(p)^>=valuescandouble) and (pdouble(p)^<valuescandoublemax);
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

    i, j: integer;

    addedToQueue: integer;

    tempentry: TPathQueueElement;
begin
  //setup the queue
  //load the overflow from the overflow queue

  f:=tfilestream.Create(filename+'.resume.queue', fmOpenRead or fmShareDenyNone);
  offsetcountperlist:=f.readDWord;

  if offsetcountperlist<>length(pathqueue[0].tempresults) then raise exception.create('Invalid queue file');

  pathqueueCS.enter;

  while f.Position<f.Size do
  begin
    i:=length(overflowqueue);
    setlength(overflowqueue, length(overflowqueue)+1);
    f.Read(overflowqueue[i].valuetofind, sizeof(pathqueue[i].valuetofind));
    f.read(overflowqueue[i].startlevel, sizeof(overflowqueue[i].startlevel));

    setlength(overflowqueue[i].tempresults, offsetcountperlist);
    f.read(overflowqueue[i].tempresults[0], length(overflowqueue[i].tempresults)*sizeof(overflowqueue[i].tempresults[0]));

    if noloop then
    begin
      setlength(overflowqueue[i].valuelist, offsetcountperlist);
      f.read(overflowqueue[i].valuelist[0], length(overflowqueue[i].valuelist)*sizeof(overflowqueue[i].valuelist[0]));
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

  for i:=length(overflowqueue)-1 downto 0 do
  begin
    if pathqueuelength<MAXQUEUESIZE then
    begin
      pathqueue[(length(overflowqueue)-1)-i]:=overflowqueue[i];


      overflowqueue[i].tempresults[0]:=$cece;
      inc(addedToQueue);
      inc(pathqueuelength);
    end else break;
  end;

  setlength(overflowqueue, length(overflowqueue)-addedToQueue);

  ReleaseSemaphore(pathqueueSemaphore, addedToQueue, nil);
  pathqueueCS.leave;

  f.free;
end;

procedure TPointerscanController.SaveAndClearQueue(s: TStream);
var i: integer;
begin
  if s=nil then exit; //can happen if stop is pressed right after the scan is done but before the gui is updated

  if pathqueuelength>0 then
  begin
    pathqueueCS.enter;
    try
      //save the current queue and clear it (repeat till all scanners are done)
      for i:=0 to pathqueuelength-1 do
      begin
        s.Write(pathqueue[i].valuetofind, sizeof(pathqueue[i].valuetofind));
        s.Write(pathqueue[i].startlevel, sizeof(pathqueue[i].startlevel));
        s.Write(pathqueue[i].tempresults[0], length(pathqueue[i].tempresults)*sizeof(pathqueue[i].tempresults[0]));

        if noloop then
          s.Write(pathqueue[i].valuelist[0], length(pathqueue[i].valuelist)*sizeof(pathqueue[i].valuelist[0]));
      end;

      //also save the overflow queue
      for i:=0 to length(overflowqueue)-1 do
      begin
        s.Write(overflowqueue[i].valuetofind, sizeof(overflowqueue[i].valuetofind));
        s.Write(overflowqueue[i].startlevel, sizeof(overflowqueue[i].startlevel));
        s.Write(overflowqueue[i].tempresults[0], length(overflowqueue[i].tempresults)*sizeof(overflowqueue[i].tempresults[0]));

        if noloop then
          s.Write(overflowqueue[i].valuelist[0], length(overflowqueue[i].valuelist)*sizeof(overflowqueue[i].valuelist[0]));
      end;

      setlength(overflowqueue,0);

      i:=pathqueuelength;
      pathqueuelength:=0;
      ReleaseSemaphore(pathqueueSemaphore, i, nil);

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


  cs: Tcompressionstream;
  s: TFileStream;
begin

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
              if pathqueuelength<MAXQUEUESIZE-1 then //no need to lock
              begin
                pathqueueCS.enter;
                //setup the queueelement
                if pathqueuelength<MAXQUEUESIZE-1 then
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

                pathqueueCS.leave;
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
            ReleaseSemaphore(pathqueueSemaphore, 1, nil);

          end;



        end;

      end;

      //wait till all workers are in isdone state
      {
      if distributedScanning then
      begin
        if not distributedWorker then
          launchServer; //everything is configured now and the scanners are active

        alldone:=not doDistributedScanningLoop;
      end;  }



      while (not alldone) do
      begin
        outofdiskspace:=getDiskFreeFromPath(filename)<64*1024*1024*length(localscanners); //64MB for each thread


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
              savedqueue.WriteDWord(length(pathqueue[0].tempresults)); //number of entries in the tempresult array
            end;
          end;


          if not savestate then
            ReleaseSemaphore(pathqueueSemaphore, MAXQUEUESIZE, nil); //release all queues
        end;


        EatFromOverflowQueueIfNeeded;

        if hasNetworkResponsibility then
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
        end
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
          if (pathqueuelength=0) or terminated then
          begin //still 0
            alldone:=true;


            localscannersCS.Enter;
            for i:=0 to length(localscanners)-1 do
            begin
              if localscanners[i].haserror then
              begin

                OutputDebugString('A worker had an error: '+localscanners[i].errorstring);

                haserror:=true;
                errorstring:=localscanners[i].errorstring;

                for j:=0 to length(localscanners)-1 do localscanners[j].terminate; //even though the reversescanner already should have done this, let's do it myself as well

                alldone:=true;
                break;
              end;

              if not (localscanners[i].hasTerminated or localscanners[i].isdone) then //isdone might be enabled
              begin
                if terminated then
                  OutputDebugString('Worker '+inttostr(i)+' is still active. Waiting till it dies...');

                alldone:=false;
                break;
              end;
            end;

            localscannersCS.Leave;
          end
          else
            alldone:=false;

          if terminated and savestate then
            saveAndClearQueue(savedqueue);

          pathqueueCS.Leave;
          {
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
          end;}

        end;

      end;


    end;

    isdone:=true;


    //all threads are done
    localscannerscs.Enter;
    for i:=0 to length(localscanners)-1 do
      localscanners[i].stop:=true;
    localscannerscs.Leave;

    ReleaseSemaphore(pathqueueSemaphore, MAXQUEUESIZE, nil);

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

type
  TPointerlistloader=class(tthread)
  private
  public
    filename: string;
    progressbar: TProgressbar;
    pointerlisthandler: TPointerListHandler;
    procedure execute; override;
  end;

procedure TPointerlistloader.execute;
var
  fs: TFileStream;
  cs: Tdecompressionstream;
begin
  fs:=tfilestream.Create(filename, fmOpenRead or fmShareDenyNone);
  cs:=Tdecompressionstream.create(fs);

  pointerlisthandler:=TPointerListHandler.createFromStream(cs, progressbar);

  cs.free;
  fs.free;

end;

procedure TPointerscanController.WorkerException(sender: TObject);
//usually called by workers
var i: integer;
begin
  reverseScanCS.Enter;
  for i:=0 to length(localscanners)-1 do
    localscanners[i].Terminate;

  reverseScanCS.leave;
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
      if parent.socket=nil then exit; //return to the caller (failure)
      if parent.scanid<>currentscanid then exit; //the parent will probably tell the child to kill it's current scan (first a cleanup that will remove this caller)


      try
        s.position:=0;
        parent.socket.WriteByte(PSCMD_CANUPLOADRESULTS); //ask if it can handle a new upload from this child
        parent.socket.flushWrites;

        if parent.socket.ReadByte=1 then
        begin
          //yes
          parent.socket.WriteByte(PSCMD_UPLOADRESULTS);
          parent.socket.WriteDWord(s.size);
          parent.socket.WriteDWord(decompressedsize);
          parent.socket.CopyFrom(s,s.size);
          parent.socket.flushWrites;

          if parent.socket.ReadByte=0 then raise exception.create('Invalid reply from PSCMD_UPLOADRESULTS');

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
begin
  //accept the incoming connection and create a Host or Child controller
  ZeroMemory(@client, sizeof(client));
  size:=sizeof(client);
  s:=fpaccept(listensocket, @client, @size);


  OutputDebugString('Incoming connection from '+inttostr(byte(client.sin_addr.S_un_b.s_b1))
                                                +'.'+inttostr(byte(client.sin_addr.S_un_b.s_b2))
                                                +'.'+inttostr(byte(client.sin_addr.S_un_b.s_b3))
                                                +'.'+inttostr(byte(client.sin_addr.S_un_b.s_b4))
  );



  if s=SOCKET_ERROR then
  begin
    OutputDebugString('s==INVALID_SOCKET');
    OutputDebugString('lasterror='+inttostr(socketerror));
    exit;
  end;

  //connected. Initiate the handshake (3 second timeout max)

{$ifdef windows}
  nonblockingmode:=1;
  ioctlsocket(s, FIONBIO, nonblockingmode);
{$else}
  fcntl(fSocket, F_SETFL, fcntl(socketfd, F_GETFL, 0) | O_NONBLOCK);
{$endif}

  ss:=TSocketStream.create(s, false);

  try
    try
      cehandshakesignature:=ss.ReadByte;
      if cehandshakesignature<>$ce then
        raise TSocketException.create('Invalid handshake signature');

      password:=ss.ReadAnsiString8;
      connectiontype:=ss.ReadByte;

      if connectiontype=0 then //parent
      begin
        if not allowIncomingParent then
          raise exception.create('A parent tried to connect');

        if parentpassword<>password then
          raise TSocketException.create('Invalid parent password');
      end
      else
      if connectiontype=1 then //child
      begin
        if not allowIncomingChildren then
          raise exception.create('A child tried to connect');

        if childpassword<>password then
          raise TSocketException.create('Invalid child password');
      end
      else
        raise exception.create('Invalid message');


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




end;

procedure TPointerscanController.waitForAndHandleNetworkEvent;
var
  count: integer;
  i,j: integer;
  readfds: TFDSet;
  maxfd: Tsocket;
  sockets: array of Tsocket;

  timeout: TTimeVal;

  checkedallsockets: boolean;
begin
  //listen to the listensocket if available and for the children
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
  end;



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
        if (childnodes[i].socket<>nil) and (childnodes[i].scandatauploader=nil) then  //this scandatauploader check 'should' not be necesary (the child shouldn't talk to the parent while it's receiving the scandata, not even worker threads as they should have been destroyed beforehand.
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

    timeout.tv_sec:=0;
    timeout.tv_usec:=500000 div (1+(length(childnodes) div FD_SETSIZE));
    j:=select(maxfd, @readfds, nil, nil, @timeout);

    if j=-1 then
      raise exception.create('Select failed');

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

        inc(i);
      end;

    finally
      childnodescs.leave;
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
end;

procedure TPointerscancontroller.handleParentException(error: string);
var
  shouldreconnect: boolean;
  host, password: string;
  port: word;
begin
  shouldreconnect:=false;
  parentcs.enter;

  if parent.socket<>nil then
    FreeAndNil(parent.socket);

  try
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
    BecomeChildOfNode(host, port, password);


  orphanedSince:=GetTickCount64;

end;

procedure TPointerscancontroller.handleParentQueueException(index: integer; error: string);
var
  i: integer;
  shouldreconnect: boolean;
  host, password: string;
  port: word;
begin
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
do not clean uop anything else. (threads and other data structures will get cleared by the eventhandler)
}
var
  shouldreconnect: boolean;
  host, password: string;
  port: word;
  trusted: boolean;
begin
  shouldreconnect:=false;

  childnodescs.Enter; //shouldn't be needed as things that raise child exceptions SHOULD already have a lock on it
  try
    if childnodes[index].socket<>nil then
      freeandnil(childnodes[index].socket);

    childnodes[index].MissingSince:=GetTickCount64;

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
begin
  s:=childnodes[index].socket;
  if s<>nil then
  begin
    command:=s.ReadByte;
    case command of
      PSCMD_HELLO: raise exception.create('HELLO after initializtion');
      PSCMD_YOURINTHEQUEUE: HandleQueueMessage(index);
      PSCMD_UPDATESTATUS: HandleUpdateStatusMessage(index);
      PSCMD_AMITRUSTED: s.WriteByte(ifthen(childnodes[index].trusted,1,0));
      PSCMD_SENDPATHS: HandleSendPathsMessage(index);
      PSCMD_UPLOADRESULTS: HandleUploadResultsMessage(index);
      PSCMD_PREPAREFORMYTERMINATION: childnodes[index].terminating:=true;
      PSCMD_GOODBYE:
      begin
        freeandnil(childnodes[index].socket);
        childnodes[index].MissingSince:=0; //it's gone but not missing.
      end;
    end;
  end;
end;

procedure TPointerscancontroller.HandleQueueMessage(index: integer);
var
  s: TSocketStream;
begin
  s:=childnodes[index].socket;

  childnodes[index].queued:=true;
  childnodes[index].queuepos:=s.ReadDWord;
  childnodes[index].queuesize:=s.ReadDword;

  s.WriteByte(0); //tell it you received and processed the message
  s.flushWrites;
end;

procedure TPointerscanController.HandleCanUploadResultsMessage(index: integer);
{
called by PSCMD_CANUPLOADRESULTS
Checks if the current child is busy sending results to the parent
}
begin
  childnodes[index].socket.WriteByte(ifthen(childnodes[index].scanresultDownloader<>nil, 1, 0));
end;

procedure TPointerscanController.HandleUploadResultsMessage(index: integer);
{
The child wants to send me it's found results
spawn a thread that will receive the results and then pass them on to the parent or save to disk
}
begin
  if childnodes[index].scanresultDownloader<>nil then //the child did not call PSCMD_CANUPLOADRESULTS to see if it could send new results, or blatantly ignored it's result
    raise exception.create('The child tried to send me results while I was still busy');

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

  if (currentscanhasended and savestate) or child.trusted or child.terminating then
  begin
    count:=child.socket.ReadDWord;

    setlength(paths, count);

    ms:=TMemoryStream.Create;
    try
      ms.CopyFrom(child.socket, getPathQueueElementSize*count);
      for i:=0 to length(paths)-1 do
        LoadPathQueueElementFromStream(ms, @paths[i]);
    finally
      ms.free;
    end;

    appendDynamicPathQueueToOverflowQueue(paths);
  end
  else
    child.socket.WriteByte(1); //fail because of untrusted
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

  setlength(paths, count);
  actualcount:=0;

  if includeVeryGoodPath then
  begin
    pathqueueCS.enter;
    try
      if (pathqueuelength>0) then
      begin
        //give it one good path (the best path)

        if WaitForSingleObject(pathqueueSemaphore, 0)=WAIT_OBJECT_0 then //lock the entry
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
        end;

      end;

    finally
      pathqueueCS.leave;
    end;
    inc(actualcount);
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
        if WaitForSingleObject(pathqueueSemaphore, 0)=WAIT_OBJECT_0 then //lock it
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
  with child.socket do
  begin
    WriteByte(PSUPDATEREPLYCMD_GIVEMEYOURPATHS);
    WriteDword(count); //maxcount
    flushWrites;

    count:=ReadDWord;
    setlength(paths, count);

    buf:=TMemoryStream.Create;
    try
      buf.CopyFrom(child.socket, getPathQueueElementSize*count);

      buf.position:=0;
      for i:=0 to count-1 do
        LoadPathQueueElementFromStream(buf, @paths[i]);
    finally
      buf.free;
    end;

    //still here so I guess it's ok
    appendDynamicPathQueueToOverflowQueue(paths);
  end;

  EatFromOverflowQueueIfNeeded;
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
  buildPathListForTransmission(paths, count, child.trusted and (child.totalpathqueuesize=0));

  try

    with child.socket do
    begin
      WriteByte(PSUPDATEREPLYCMD_HEREARESOMEPATHS);
      WriteDWord(length(paths)); //number of paths
      for i:=0 to length(paths)-1 do
        WritePathQueueElementToStream(child.socket, @paths[i]);

      flushWrites;

      if ReadByte<>0 then raise TSocketException.create('Invalid result received from PSUPDATEREPLYCMD_HEREARESOMEPATHS');
    end;



    if not child.trusted then //save the paths being sent
      child.nontrustedlastpaths:=paths;
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
begin
  //todo: test me
  child:=@childnodes[index];
  s:=child.socket;

  s.ReadBuffer(updatemsg, sizeof(updatemsg));

//  update the childstatus and issue it a command
  child.idle:=updatemsg.isidle=1;
  child.totalthreadcount:=updatemsg.totalthreadcount;
  child.totalPathsEvaluated:=updatemsg.pathsevaluated;
  child.pathqueuesize:=updatemsg.localpathqueuecount;
  child.totalpathqueuesize:=updatemsg.totalpathQueueCount;
  child.queuesize:=updatemsg.queuesize;

  //now reply
  if currentscanhasended or ((not child.idle) and (updatemsg.currentscanid<>currentscanid)) then //scan terminated , or
  begin
    child.socket.WriteByte(PSUPDATEREPLYCMD_CURRENTSCANHASENDED);

    if currentscanhasended then
      child.socket.WriteByte(ifthen(savestate,1,0))
    else
    begin
      //special case that under normal situations shouldn't occur (could happen if a scan was stopped and a new one was started before the children where idle, or a long lost child joins)
      child.socket.WriteByte(0); //wrong scan id. I'm waiting for him to kill his children. Don't let him send me paths...
    end;


    child.socket.flushWrites;
    if child.socket.ReadByte<>0 then
      raise exception.create('Invalid reply for PSUPDATEREPLYCMD_CURRENTSCANHASENDED');
    exit;
  end;

  if (updatemsg.currentscanid<>currentscanid) then
  begin
    //spawn a new thread and tell him about the scan (as soon as I quit and release the critical section)
    //use child.childid to identify the child object to update when done

    assert(child.idle, 'child isn''t idle while previously it was...');
    if child.idle then
    begin
      child.receivingScanDataProgress:=0;
      child.scanDataUploader:=TScandataUploader.create(self, child.childid);
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

    if child.trusted then
    begin
      //equalize the paths
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
        HandleUpdateStatusMessage_SendPathsToChild(child, localpathcount-updatemsg.localpathqueuecount);
        exit;
      end;

      if (updatemsg.localpathqueuecount>(MAXQUEUESIZE div 2)) and (localpathcount<(MAXQUEUESIZE div 2)) then
      begin
        //equalize (from child<-parent)
        HandleUpdateStatusMessage_RequestPathsFromChild(child, localpathcount-updatemsg.localpathqueuecount);
        exit;
      end;

    end
    else
    begin
      if child.idle then //only send paths to the non-trusted child if it's completely idle
      begin
        HandleUpdateStatusMessage_SendPathsToChild(child, child.trustlevel+1); //the trustlevel goes up if it goes idle within 5 minutes
        exit;
      end;
    end;
  end;

  //still here, so everything is ok
  child.socket.WriteByte(PSUPDATEREPLYCMD_EVERYTHINGOK);
  child.socket.flushWrites;
  if child.socket.ReadByte<>0 then
    raise exception.create('The child didn''t respond to PSUPDATEREPLYCMD_EVERYTHINGOK as expected');
end;

//parent->child


procedure TPointerscanController.cleanupScan;
//called by the controller when the parent tells it to cleanup or do a new scan. (the cleanup should have been done first though)
var i: integer;
begin
  localscannersCS.Enter;

  if length(localscanners)>0 then //close them
  begin
    for i:=0 to length(localscanners)-1 do
    begin
      localscanners[i].savestate:=savestate;
      localscanners[i].Terminate;
    end;

    pathqueueCS.enter;
    pathqueuelength:=0;
    ReleaseSemaphore(pathqueueSemaphore, MAXQUEUESIZE, nil);
    pathqueueCS.leave;

    for i:=0 to length(localscanners)-1 do
    begin
      localscanners[i].WaitFor;
      localscanners[i].Free;
    end;

    setlength(localscanners,0);

    //refresh this just to be sure:
    closeHandle(pathqueueSemaphore);
    pathqueueSemaphore:=CreateSemaphore(nil, 0, MAXQUEUESIZE, nil);

    if pointerlisthandler<>nil then
      freeAndNil(pointerlisthandler);

    if (not initializer) and allowtempfiles then
      deletefile(LoadedPointermapFilename);

    for i:=0 to Length(instantrescanfiles)-1 do
    begin
      if instantrescanfiles[i].plist<>nil then
        freeandnil(instantrescanfiles[i].plist);

      if instantrescanfiles[i].memoryfilestream<>nil then
        freeandnil(instantrescanfiles[i].memoryfilestream);

      if (not initializer) and allowtempfiles then
        deletefile(instantrescanfiles[i].filename);
    end;
  end;
  localscannerscs.Leave;
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

begin
  //todo: test me
  if not isIdle then
    raise exception.Create('New scan started while not idle');

  cleanupscan;




  with parent.socket do
  begin
    scannerid:=ReadDWord;
    currentscanid:=ReadDWord;
    maxlevel:=ReadDWord;
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

    setlength(instantrescanfiles, ReadDWord-1);


    downloadingscandata_received:=0;
    downloadingscandata_total:=ReadQWord;
    downloadingscandata:=true;

    for i:=0 to length(instantrescanfiles) do //not -1
    begin
      if i=0 then
        ReadDword
      else
        instantrescanfiles[i-1].address:=ReadDWord;

      streamsize:=ReadQWord;

      if allowTempFiles then
      begin
        //create a tempfile and open a TFileStream to it
        tempfilename:=GetTempFileName;
        currentstream:=TFileStream.Create(tempfilename, fmCreate);

        if i=0 then
          LoadedPointermapFilename:=tempfilename
        else
          instantrescanfiles[i].filename:=tempfilename;
      end
      else //create a TMemorystream
      begin
        currentstream:=tmemorystream.create;
        if i=0 then
          pointerlisthandlerfile:=tmemorystream(currentstream)
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

    //process the streams
    //first the main stream
    if allowtempfiles then //open the filestream
      currentstream:=TFileStream.create(LoadedPointermapFilename, fmOpenRead or fmShareDenyNone)
    else
      currentstream:=pointerlisthandlerfile;

    //...
    pointerlisthandler:=TReversePointerListHandler.createFromStream(currentstream);


    if allowtempfiles then
      currentstream.free;


    //and now the rescan streams
    begin
      for i:=0 to length(instantrescanfiles)-1 do
      begin
        if allowtempfiles then //open the filestream
          currentstream:=TFileStream.create(instantrescanfiles[i].filename, fmOpenRead or fmShareDenyNone)
        else
          currentstream:=pointerlisthandlerfile;

        instantrescanfiles[i].plist:=TPointerListHandler.createFromStream(currentstream);

        if allowtempfiles then
          currentstream.free;
      end;
    end;

    instantrescan:=length(instantrescanfiles)>0;



  end;

  currentscanhasended:=false;

  //spawn the threads:
  for i:=0 to threadcount-1 do
    addworkerThread;
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

  setlength(paths, count);

  buf:=TMemoryStream.Create;
  try
    buf.CopyFrom(parent.socket, getPathQueueElementSize*count);
    parent.socket.WriteByte(0); //acknowledge that the paths have been received
    parent.socket.flushWrites;

    buf.position:=0;
    for i:=0 to count-1 do
      LoadPathQueueElementFromStream(buf, @paths[i]);

    //still here so I guess it's ok
    appendDynamicPathQueueToOverflowQueue(paths);


  finally
    buf.free;
  end;


end;

procedure TPointerscanController.HandleUpdateStatusReply_CurrentScanHasEnded;
{
The scan has finished (or terminated)
}
var i: integer;
begin
  //todo: test me



  //stop all the children and wait for them to end the scan (10-20 seconds)
  savestate:=parent.socket.ReadByte=1; //if this is true and currentscanhasended as well, the children will end the scan, but will also send their current paths
  currentscanhasended:=true;  //tell the children that the scan has ended for as long as this is true




  parent.socket.WriteByte(0); //understood
  parent.socket.flushWrites;

  cleanupscan;
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
    PSUPDATEREPLYCMD_EVERYTHINGOK: exit; //everything ok
    else
       raise TSocketException.create('Invalid UpdateStatus reply received');
  end;
end;

procedure TPointerscanController.UpdateStatus(sender: tobject);
{
Tells the parent the current status, and deal with it's response
}
var
  i,j: integer;
  updatemsg: TPSUpdateStatusMsg;
begin
  //note: called by another thread (parent responses can take a while)



  try
    try
      parentcs.enter;
      if parent.socket<>nil then
      begin
        //send the update command
        //receive the result
        //handle accordingly

        OutputDebugString('Updating status');
        updatemsg.currentscanid:=currentscanid;
        updatemsg.isidle:=ifthen(isIdle,1,0);
        updatemsg.totalthreadcount:=getTotalThreadCount;
        updatemsg.pathsevaluated:=getTotalPathsEvaluated;
        overflowqueuecs.enter;
        updatemsg.localpathqueuecount:=pathqueuelength+length(overflowqueue);
        overflowqueuecs.leave;

        updatemsg.totalpathQueueCount:=getTotalPathQueueSize;
        updatemsg.queuesize:=length(parentqueue);


        parent.socket.WriteBuffer(updatemsg, sizeof(updatemsg));
        parent.socket.flushWrites;

        lastUpdateSent:=GetTickCount64;

        HandleUpdateStatusReply;
      end
      else
      begin
        //check if the queue should be accessed
        //first check if there is a parent with my currentscanid
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

            orphanedSince:=0;
            break;
          end;
        end;

        if parent.socket=nil then
        begin
          //still no parent
          if orphanedSince=0 then
          begin
            //not an orphan, check the queue and make the first one in the list my new parent
            if length(parentqueue)-1 > 0 then
            begin
              parent:=parentqueue[0];
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
                orphanedSince:=0; //... fuck
            end;
          end;

        end;

      end;
    except
      on e: exception do
        handleParentException(e.message);
    end;
  finally
    parentcs.leave;
  end;

  //update the queued parents
  parentcs.enter;
  try
    i:=0;
    while i<length(parentqueue)-1 do
    begin
      try
        with parentqueue[i].socket do
        begin
          WriteByte(PSCMD_YOURINTHEQUEUE);
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


end;


procedure TPointerscanController.setupListenerSocket;
var
  B: BOOL;
  i: integer;
  sockaddr: TInetSockAddr;

  s: Tfilestream;
  cs: Tcompressionstream;
begin
  //start listening on the given port. The waitForAndHandleNetworkEvent method will accept the connections
  listensocket:=socket(AF_INET, SOCK_STREAM, 0);

  if listensocket=INVALID_SOCKET then
    raise Exception.create('Failure creating socket');

  B:=TRUE;
  fpsetsockopt(listensocket, SOL_SOCKET, SO_REUSEADDR, @B, sizeof(B));


  sockaddr.sin_family:=AF_INET;
  sockaddr.sin_port:=htons(listenport);
  sockaddr.sin_addr.s_addr:=INADDR_ANY;
  i:=bind(listensocket, @sockaddr, sizeof(sockaddr));

  if i=SOCKET_ERROR then
    raise exception.create('Failure to bind port '+inttostr(listenport));

  i:=listen(listensocket, 32);
  if i=SOCKET_ERROR then
    raise exception.create('Failure to listen');



end;

function TPointerscanController.hasNetworkResponsibility: boolean;
//method to quickly determine if the current scancontroller should bother handling network events
//(mainly used for cleaner code)
begin
  result:=(connector<>nil) or (not initializer) or allowIncomingParent or allowIncomingChildren or (length(childnodes)>0);
  //you could connect to a child but disallow incomming connections
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

    pa,sa: DWORD_PTR;

    newAffinity: DWORD_PTR;
    PreferedProcessorList: array of integer; //a list of cpu numbers available to be used. If hyperthreading is on, this will not contain the uneven cpu numbers
    currentcpu: integer;  //index into PreferedProcessorList. If it's bigger than the size, make the affinity equal to PA (do not care, let windows decide)


    pointerlistloaders: array of TPointerlistloader;
begin
  if terminated then exit;

  try
    if allowIncomingParent or allowIncomingChildren then
      setupListenerSocket;

    if not initializer then
    begin
      //this is a childnode
      //enter the networking loop and wait for the parent(if there is one) to provide messages, or handle incomming connections

      //setup a parent update timer
      if parentupdater=nil then
      begin
        parentupdater:=TAsyncTimer.create(false);
        parentupdater.OnTimer:=UpdateStatus;
        parentupdater.Interval:=8000+random(4000); //update the parent every 8 to 12 seconds
        parentupdater.enabled:=true;
      end;

      while not terminated do
        waitForAndHandleNetworkEvent;

      exit;
    end;

    //this is an initiator

    currentscanid:=1+random(MaxInt-2); //random value, not 0


    result:=nil;

    if resumescan then
      resumeptrfilereader:=TPointerscanresultReader.create(resumeptrfilename);

    {
    if distributedScanning and distributedWorker then
      LaunchWorker; //connects and sets up the parameters

    if distributedScandataDownloadPort=0 then
      distributedScandataDownloadPort:=distributedport+1; }

    phase:=1;
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

    if pointerlisthandler=nil then
    begin

      progressbar.Position:=0;
      try
        if useLoadedPointermap then
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
        end
        else
          pointerlisthandler:=TReversePointerListHandler.Create(startaddress,stopaddress,not unalligned,progressbar, noreadonly, MustBeClassPointers, acceptNonModuleClasses, useStacks, stacksAsStaticOnly, threadstacks, stacksize, mustStartWithBase, BaseStart, BaseStop);


        progressbar.position:=100;
        //sleep(10000);



      except
        on e: exception do
        begin
          haserror:=true;
          errorString:=rsFailureCopyingTargetProcessMemory;

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
        pointerlistloaders[i].Free;
      end;
    end;

    if generatePointermapOnly then
    begin

      f:=tfilestream.create(filename, fmCreate);
      cs:=Tcompressionstream.create(clfastest, f);
      pointerlisthandler.exportToStream(cs);
      cs.free;
      f.free;

      filename:='';
      progressbar.Position:=0;

      if Assigned(fOnScanDone) then
        fOnScanDone(self, haserror, errorstring);

      terminate;
      exit;
    end;

    phase:=2;
    progressbar.Position:=0;



    i:=0;


    if compressedptr then
    begin
      //calculate the masks for compression
      //moduleid can be negative, so keep that in mind
      if resumescan then
        MaxBitCountModuleIndex:=getMaxBitCount(resumeptrfilereader.modulelistCount-1, true)
      else
        MaxBitCountModuleIndex:=getMaxBitCount(pointerlisthandler.modulelist.Count-1, true);

      MaxBitCountLevel:=getMaxBitCount(maxlevel-length(mustendwithoffsetlist) , false); //counted from 1.  (if level=4 then value goes from 1,2,3,4) 0 means no offsets. This can happen in case of a pointerscan with specific end offsets, which do not get saved.
      MaxBitCountOffset:=getMaxBitCount(sz, false);

      if unalligned=false then MaxBitCountOffset:=MaxBitCountOffset - 2;
    end;



    //setup the pathqueue
    pathqueuelength:=0;


    for i:=0 to MAXQUEUESIZE-1 do
    begin
      setlength(pathqueue[i].tempresults, maxlevel+1);
      if noLoop then
        setlength(pathqueue[i].valuelist, maxlevel+1);
    end;

    reverseScanCS:=tcriticalsection.Create;
    try

      //build a list of cpu id's
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

      currentcpu:=0;

      //create the headerfile and save header (modulelist, and levelsize)
      if resumescan then
      begin
        result:=TfileStream.create(filename+'.tmp',fmcreate or fmShareDenyWrite);
        resumePtrFileReader.saveModulelistToResults(result);
      end
      else
      begin
        result:=TfileStream.create(filename,fmcreate or fmShareDenyWrite);
        pointerlisthandler.saveModuleListToResults(result);
      end;


      if resumePtrFileReader<>nil then
        FreeAndNil(resumePtrFileReader); //free the ptr files

      for i:=0 to threadcount-1 do
      begin
        if i<length(PreferedProcessorList) then
          addWorkerThread(PreferedProcessorList[i])
        else
          addWorkerThread;
      end;

      if assigned(fOnStartScan) then
        synchronize(NotifyStartScan);

      //levelsize
      result.Write(maxlevel,sizeof(maxlevel)); //write max level (maxlevel is provided in the message (it could change depending on the settings)

      //todo: change the way the pointerfiles are stored
      //pointerstores
      if resumescan then
      begin
        for i:=0 to length(localscanners)-1 do
        begin
          j:=resumefilelist.IndexOf(TPointerscanWorkerLocal(localscanners[i]).filename);
          if j<>-1 then
            resumefilelist.Delete(j);
        end;
      end;

      temp:=length(localscanners);
      if resumefilelist<>nil then
        inc(temp, resumefilelist.count);

      result.Write(temp,sizeof(temp));
      for i:=0 to length(localscanners)-1 do
      begin
        tempstring:=ExtractFileName(TPointerscanWorkerLocal(localscanners[i]).filename);
        temp:=length(tempstring);
        result.Write(temp,sizeof(temp));
        result.Write(tempstring[1],temp);
      end;

      if resumefilelist<>nil then
      begin
        for i:=0 to resumefilelist.count-1 do
        begin
          tempstring:=resumefilelist[i];
          temp:=length(tempstring);
          result.Write(temp,sizeof(temp));
          result.Write(tempstring[1],temp);
        end;
      end;


      freeandnil(result);



      //now do the actual scan
      reversescan;

      //returned from the scan, save the rest

      if resumescan then
        result:=TfileStream.create(filename+'.tmp',fmOpenWrite)
      else
        result:=TfileStream.create(filename,fmOpenWrite);

      result.seek(0, soEnd); //append from the end



      {if distributedScanning then
      begin
        //save the number of external workers


        result.writeDword(length(workers)); //0 for a worker (unless I decide to make it a real chaotic mess)

        //save the workerid that generated these results (server=-1)
        result.writeDword(myid);

        freeandnil(result);

        if scandataUploader<>nil then
        begin
          scandataUploader.terminate;
          scandataUploader.WaitFor;
          freeandnil(scandataUploader);
        end

      end
      else }
      begin
        //todo: obsolete, there are no workers and no more merging of results
        result.writeDword(0);    //number of workers
        result.writeDword(0);    //my id (ignored)

      end;

      result.writeDword(0); //merged worker count

      result.writeDword(ifthen(compressedptr, 1, 0));
      result.writeDword(ifthen(unalligned, 0, 1)); //1 if alligned (I should really rename this one)
      result.writeDword(MaxBitCountModuleIndex);
      result.writeDword(MaxBitCountLevel);
      result.writeDword(MaxBitCountOffset);

      result.writeDword(length(mustendwithoffsetlist));
      for i:=0 to length(mustendwithoffsetlist)-1 do
        result.writeDword(mustendwithoffsetlist[i]);

    finally

      if result<>nil then
        freeandnil(result);

      freeandnil(reverseScanCS);




      if resumescan then
      begin
        //delete the old ptr file, it's not needed anymore
        if resumePtrFileReader<>nil then
          FreeAndNil(resumePtrFileReader);

        deletefile(filename);
        RenameFile(filename+'.tmp', filename);
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
    raise TSocketException.Create('Invalid command while waiting for hello'); //invalid command

  receive(sockethandle, @namelength, 1);

  getmem(name, namelength+1);
  try
    receive(sockethandle, name, namelength);
    name[namelength]:=#0;
    msg.publicname:=name;
  finally
    freemem(name);
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
  with potentialparent.socket do
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
var i: integer;
    hellomsg: TPSHelloMsg;
    child: PPointerscancontrollerchild;
    bm: u_long;

    ipname: TSockAddrIn;
    len: Longint;
begin
  child:=nil;

  //mark the socket as non blocking
{$ifdef windows}
  bm:=0;
  ioctlsocket(sockethandle, FIONBIO, bm);
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


    len:=sizeof(ipname);
    if getpeername(sockethandle, ipname, len)<>SOCKET_ERROR then
    begin
      child.ip:=inttostr(byte(ipname.sin_addr.S_un_b.s_b1))+'.'+inttostr(byte(ipname.sin_addr.S_un_b.s_b2))+
                inttostr(byte(ipname.sin_addr.S_un_b.s_b3))+'.'+inttostr(byte(ipname.sin_addr.S_un_b.s_b4));

      child.port:=ipname.sin_port;
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
          parentqueue[length(parentqueue)-1].ip:=inttostr(byte(ipname.sin_addr.S_un_b.s_b1))+'.'+inttostr(byte(ipname.sin_addr.S_un_b.s_b2))+
                    inttostr(byte(ipname.sin_addr.S_un_b.s_b3))+'.'+inttostr(byte(ipname.sin_addr.S_un_b.s_b4));

          parentqueue[length(parentqueue)-1].port:=ipname.sin_port;
        end;




          //you have a new daddy! Say hello to him
        sayHello(@parentqueue[length(parentqueue)-1]); //'Hello daddy'...creepy voice

      except
        if parentqueue[length(parentqueue)-1].socket<>nil then
          freeandnil(parentqueue[length(parentqueue)-1].socket);

        setlength(parentqueue, length(parentqueue)-1);
      end;

    finally
      parentcs.Leave;
    end;




  end;

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
          raise exception.create('Already/still connected to this child');
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

procedure TPointerscanController.addworkerThread(preferedprocessor: integer=-1);
var
  scanner: TPointerscanWorker;
  j: integer;
  NewAffinity: DWORD_PTR;
  scanfileid: integer;
begin
  if initializer then
  begin
    localscannersCS.enter;
    scanfileid:=length(localscanners);
    localscannersCS.leave;

    scanner:=TPointerscanWorkerLocal.Create(
                                                     true,
                                                     self.filename+'.'+inttostr(scanfileid),
                                                     resumescan or (scanfileid<nextscanfileid)
                                                     );
    nextscanfileid:=max(nextscanfileid, scanfileid+1);
  end
  else
    scanner:=TPointerscanWorkerNetwork.Create(true);

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
  if preferedprocessor<>-1 then
  begin
    NewAffinity:=1 shl preferedprocessor;
    NewAffinity:=SetThreadAffinityMask(scanner.Handle, NewAffinity);
  end;

  scanner.compressedptr:=compressedptr;
  scanner.MaxBitCountModuleIndex:=MaxBitCountModuleIndex;
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
      localscanners[length(localscanners)-1].free;
      setlength(localscanners, length(localscanners)-1);
    end;
    dec(threadcount);
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

constructor TPointerscanController.create(suspended: boolean);
begin
  pointersize:=processhandler.pointersize;

  listensocket:=-1;
  parent.socket:=nil;

  parentcs:=tcriticalsection.create;
  childnodescs:=tcriticalsection.create;

  connectorcs:=TCriticalSection.create;

  localscannersCS:=TCriticalSection.create;

  pathqueueCS:=TCriticalSection.create;
  pathqueueSemaphore:=CreateSemaphore(nil, 0, MAXQUEUESIZE, nil);

  overflowqueuecs:=TCriticalSection.create;

  nextchildid:=random(MaxInt); //just a random start

  currentscanhasended:=true;

  inherited create(suspended);
end;

destructor TPointerscanController.destroy;
var i: integer;
begin
  terminate;
  waitfor;

  if connectorcs<>nil then
    connectorcs.free;

    {
  if sockethandle<>-1 then
  begin
    CloseSocket(sockethandle);
    sockethandle:=-1;
  end; }

  if listensocket<>-1 then
  begin
    closesocket(listensocket);
    listensocket:=-1;
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

  if resumefilelist<>nil then
    freeandnil(resumefilelist);

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

  closehandle(pathqueueSemaphore);


  //clean up other stuff
  inherited destroy;
end;

end.

