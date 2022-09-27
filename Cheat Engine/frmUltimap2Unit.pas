unit frmUltimap2Unit;

{$mode OBJFPC}{$H+}



interface

uses
  {$ifdef darwin}
  macport, mactypes,
  {$endif}
  {$ifdef windows}
  jwawindows, win32proc,  windows,
  {$endif}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, EditBtn, Menus, iptnative, libipt, ProcessHandlerUnit,
  DBK32functions, commonTypeDefs, MemFuncs, AvgLvlTree, Math, FileMapping,
  syncobjs, CEFuncProc, registry, NewKernelHandler, LazFileUtils, disassembler,
  strutils, Clipbrd, lua, lualib, lauxlib, luaform, LuaClass, frmUltimapUnit,
  genericHotkey, Contnrs, maps, betterControls, DPIHelper;


const
  bifInvalidated=1;
  bifExecuted=2;
  bifIsCall=4;

type
  TRecordState=(rsStopped, rsProcessing, rsRecording);
  TFilterOption=(foNone, foExecuted, foNotExecuted, foNonCALLInstructions, foExecuteCountNotEqual, foNotInRange, foResetCount, foResetAll);

  TByteInfo=packed record
    flags: byte;
    count: byte;
  end;
  PByteInfo=^TByteInfo;

  TValidEntry=record
    address: ptruint;
    byteInfo: PByteInfo;
  end;
  PValidEntry=^TValidEntry;


  TRegionInfo=record
    address: ptruint;
    memory: PByte;
    size: integer;

    info: PByteInfo;

  end;
  PRegionInfo=^TRegionInfo;

  TfrmUltimap2=class;

  TUltimap2WorkerCommand=(uwcProcessData, uwcProcessDataCombined, uwcTerminate);

  TUltimap2WorkerCommandData=record
    command: TUltimap2WorkerCommand;

    case TUltimap2WorkerCommand of
      uwcProcessData: (data: record
          oncompletion: PAPCFUNC;
          workersActive: plongint;
          data: pointer;
          size: dword;
        end);

      uwcProcessDataCombined: (datacombined: record
          oncompletion: PAPCFUNC;
          workersActive: plongint;
          data1: pointer;
          size1: dword;
          data2: pointer;
          size2: dword;
        end);
  end;

  PUltimap2WorkerCommandData=^TUltimap2WorkerCommandData;

  TUltimap2Worker=class(TThread) //many
  private
    commands: TQueue;
    commandsCS: TcriticalSection;

    hasCommandEvent: TEvent;

    localregiontree: TAvgLvlTree; //just pointers to the memory
    lastRegion: PRegionInfo;
    filecount: integer; //number of tracefiles saved

    filemap: TFileMapping;

    disassembler: Tdisassembler;

    iptConfig: pt_config;
    decoder: ppt_insn_decoder;
    callbackImage: PPT_Image;

    rolloverhelper: pointer;
    rolloverhelpersize: integer;


    function RegionCompare(Tree: TAvgLvlTree; Data1, Data2: pointer): integer;

    function addIPPageToRegionTree(IP: QWORD): PRegionInfo;
    function addIPBlockToRegionTree(IP: QWORD): PRegionInfo;
    procedure HandleIP(ip: QWORD; c: pt_insn_class);
    procedure HandleIPForRegion(ip: qword; c: pt_insn_class; region: PRegionInfo);

    function waitForData(timeout: dword; var e: TUltimap2DataEvent): boolean;
    procedure continueFromData(e: TUltimap2DataEvent);

    procedure parseToStringlist(insn: pt_insn; output: Tstrings);

    procedure processWindowsIPTDataWithRollOverImplementation(data1: pointer; datasize1: dword; data2: pointer; datasize2: dword; oncompletion: PAPCFUNC; workersActive: plongint);
    procedure processWindowsIPTDataImplementation(data: pointer; datasize: dword; oncompletion: PAPCFUNC; workersActive: plongint);
  protected
    procedure TerminatedSet; override;
  public
    id: qword;
    ownerThreadHandle: thandle;
    KeepTraceFiles: boolean;
    filename: string;
    fromFile: boolean;
    processFile: TEvent;
    done: boolean;
    ownerForm: TfrmUltimap2;

    processed: qword;
    totalsize: qword;

    parseAsText: boolean;
    textFolder: string;
    ts: TStringList;
    windowsBasedIPT: boolean;

    procedure processWindowsIPTDataWithRollOver(data1: pointer; datasize1: dword; data2: pointer; datasize2: dword; oncompletion: PAPCFUNC; workersActive: plongint);
    procedure processWindowsIPTData(data: pointer; datasize: dword; oncompletion: PAPCFUNC; workersActive: plongint);

    procedure processData(e: TUltimap2DataEvent);
    procedure execute; override;

    constructor create(CreateSuspended: boolean; cpuid: integer; owner: TfrmUltimap2);
    constructor create(CreateSuspended: boolean; tid: qword; owner: TfrmUltimap2; ownerthread: TThreadID);
    destructor destroy; override;
  end;

  TUltimap2FilterWorker=class(tthread)
  private
    procedure FilterExecuted(ri: TRegionInfo);
    procedure FilterNotExecuted(ri: TRegionInfo);
    procedure FilterNonCallInstruction(ri: TRegionInfo);
    procedure FilterExecutionCountNoEqual(ri: TRegionInfo);
    procedure FilterNotInRange(ri: TRegionInfo);
    procedure FilterResetCount(ri: TRegionInfo);
    procedure FilterResetAll(ri: TRegionInfo);
  public
    invalidated: integer;
    filteroption: TfilterOption;
    callcount: integer;
    rangestart, rangestop: qword;
    ExcludeFuturePaths: boolean;

    filterSemaphore: THandle;
    queuepos: pinteger;
    workqueue: ^PRegionInfo;
    queueCS: TCriticalSection;

    done: boolean;

    procedure execute; override;
  end;

  TUltimap2FilterThread=class(tthread) //1
  private
    filterSemaphore: THandle;
    workqueue: ^PRegionInfo;
    queueCS: TCriticalSection;
    queuepos: integer;

    workers: array of TUltimap2FilterWorker;

    procedure EnableGUI;
  public
    ownerform: TfrmUltimap2;
    regiontree: TAvgLvlTree;
    regiontreeMREW: TMultiReadExclusiveWriteSynchronizer;

    filteroption: TfilterOption;
    callcount: integer;
    rangestart, rangestop: qword;
    ExcludeFuturePaths: boolean;

    procedure execute; override;
  end;


  TDataDispatcherEvent=(ddeStartProcessingData, ddeStopProcessingData, ddeSuspendProcessingOfThread, ddeResumeProcessingOfThread, ddeFlush, ddeStartFileProcessing);
  TDataDispatcherEventData=record
    event: TDataDispatcherEvent;
    case TDataDispatcherEvent of
      ddeResumeProcessingOfThread,ddeResumeProcessingOfThread: (threadhandle: thandle);
      //ddeSuspendProcessingOfThread: (threadhandle: thandle);
      ddeFlush,ddeStartFileProcessing: (done: TEvent);
  end;
  PDataDispatcherEventData=^TDataDispatcherEventData;

  TUltimap2ThreadInfo=record
    threadid: qword;
    paused: boolean;
    waspaused: boolean;
    lastoffset: dword;
    overflowsseen: dword;
    totaldata: qword;
    firstseen: qword;
    lastseen: qword;
    lastbytes: qword;

    timesseen: integer;
    lostdata: integer;
    filename: pchar;
    filehandle: thandle;
    overlapped: OVERLAPPED;

    WriteCompleteDataPart1: record   //datablock used when async writing the back part of the buffer before the front
      h:PIPT_TRACE_HEADER;
      datastartpart2: pointer;
      lengthpart2: dword;
      workersActive: plongint;
    end;

    worker: TUltimap2Worker;
  end;

  PUltimap2ThreadInfo=^TUltimap2ThreadInfo;


  TIPTDataDispatcher=class(TThread)
  private
    error: string;

    logtofolder: boolean;
    outputfolder: string;

    eventsCS: TcriticalSection;
    events: TQueue;


    hasEvent: TEvent; //in case the dispatcher has received ddeStopProcessingData mode and is now idling

    paused: boolean;

    trace: PIPT_TRACE_DATA;
    tracesize: dword;

    threadlistMREW: TMultiReadExclusiveWriteSynchronizer;
    threadlist: tmap;

    ownerform: TfrmUltimap2;

    waspaused: boolean; //set when the first processdata after a resume


    procedure showError;
    function processData: boolean;
    procedure decreaseIPTSize;
    procedure StartFileProcessingInternal;
    procedure addEvent(e: TDataDispatcherEventData);

  protected
    procedure TerminatedSet; override;
  public
    keepTraceFiles: boolean;
    parseAsText: boolean;
    textfolder: string;
    procedure resumeProcessing;
    procedure pauseProcessing;
    procedure suspendThreadProcessing(threadHandle: THandle);
    procedure resumeThreadProcessing(threadhandle: THandle);
    procedure flushAndWait;
    procedure flush;
    procedure startFileProcessing;
    function getTotalDataCollected: qword;
    procedure execute; override;

    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt=DefaultStackSize);
    destructor Destroy; override;
  end;

  { TfrmUltimap2 }

  TfrmUltimap2 = class(TForm)
    btnAddRange: TButton;
    btnExecuted: TButton;
    btnFilterCallCount: TButton;
    btnFilterModule: TButton;
    btnNotCalled: TButton;
    btnNotExecuted: TButton;
    btnRecordPause: TButton;
    btnResetCount: TButton;
    btnCancelFilter: TButton;
    btnShowResults: TButton;
    btnReset: TButton;
    cbFilterFuturePaths: TCheckBox;
    cbfilterOutNewEntries: TCheckBox;
    cbDontDeleteTraceFiles: TCheckBox;
    cbParseToTextfile: TCheckBox;
    cbAutoProcess: TCheckBox;
    cbPauseTargetWhileProcessing: TCheckBox;
    cbNoInterrupts: TCheckBox;
    cbTraceAllProcesses: TCheckBox;
    cbUsermode: TCheckBox;
    cbKernelmode: TCheckBox;
    cbWindowsBasedIPT: TCheckBox;
    cbWinIPTBufferSize: TComboBox;
    deTargetFolder: TDirectoryEdit;
    deTextOut: TDirectoryEdit;
    edtFlushInterval: TEdit;
    edtMaxFilesize: TEdit;
    edtBufSize: TEdit;
    edtCallCount: TEdit;
    gbRange: TGroupBox;
    gbThreads: TGroupBox;
    lblBufferSizePerThread: TLabel;
    lvThreads: TListView;
    MainMenu1: TMainMenu;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    miRemoveHotkey: TMenuItem;
    miSetHotkey: TMenuItem;
    OpenDialog1: TOpenDialog;
    pmSetHotkey: TPopupMenu;
    tThreadlistUpdater: TTimer;
    um2ImageList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    lblBuffersPerCPU: TLabel;
    lblIPCount: TLabel;
    lblKB: TLabel;
    lblLastfilterresult: TLabel;
    lbRange: TListBox;
    MenuItem1: TMenuItem;
    miRangeDeleteSelected: TMenuItem;
    miRangeDeleteAll: TMenuItem;
    Panel1: TPanel;
    Panel4: TPanel;
    Panel6: TPanel;
    pmRangeOptions: TPopupMenu;
    Label3: TLabel;
    ListView1: TListView;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel5: TPanel;
    PopupMenu1: TPopupMenu;
    cbWhenFilesizeAbove: TCheckBox;
    cbTraceInterval: TCheckBox;
    rbLogToFolder: TRadioButton;
    rbRuntimeParsing: TRadioButton;
    tActivator: TTimer;
    tProcessor: TTimer;
    procedure btnAddRangeClick(Sender: TObject);
    procedure btnExecutedClick(Sender: TObject);
    procedure btnFilterCallCountClick(Sender: TObject);
    procedure btnFilterModuleClick(Sender: TObject);
    procedure btnNotCalledClick(Sender: TObject);
    procedure btnNotExecutedClick(Sender: TObject);
    procedure btnCancelFilterClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnResetCountClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnShowResultsClick(Sender: TObject);
    procedure cbfilterOutNewEntriesChange(Sender: TObject);
    procedure cbParseToTextfileChange(Sender: TObject);
    procedure cbTraceAllProcessesChange(Sender: TObject);
    procedure cbTraceIntervalChange(Sender: TObject);
    procedure cbWhenFilesizeAboveChange(Sender: TObject);
    procedure cbWindowsBasedIPTChange(Sender: TObject);
    procedure cbWinIPTBufferSizeDropDown(Sender: TObject);
    procedure edtFlushIntervalChange(Sender: TObject);
    procedure edtMaxFilesizeChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure ListView1DblClick(Sender: TObject);
    procedure lvThreadsDblClick(Sender: TObject);
    procedure lvThreadsItemChecked(Sender: TObject; Item: TListItem);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure miCloseClick(Sender: TObject);
    procedure miRangeDeleteSelectedClick(Sender: TObject);
    procedure miRangeDeleteAllClick(Sender: TObject);
    procedure miRemoveHotkeyClick(Sender: TObject);
    procedure miSetHotkeyClick(Sender: TObject);
    procedure Panel5Click(Sender: TObject);
    procedure pmRangeOptionsPopup(Sender: TObject);
    procedure pmSetHotkeyPopup(Sender: TObject);
    procedure rbLogToFolderChange(Sender: TObject);
    procedure tActivatorTimer(Sender: TObject);
    procedure tbRecordPauseChange(Sender: TObject);
    procedure tProcessorTimer(Sender: TObject);
    procedure tThreadlistUpdaterTimer(Sender: TObject);
  private
    { private declarations }
    debugmode: boolean; //when set the kernelmode part is disabled, but processing of files sitll happens


    modulelist: tstringlist;
    ultimap2Initialized: dword;

    regiontree: TAvgLvlTree;
    regiontreeMREW: TMultiReadExclusiveWriteSynchronizer;

    workers: array of TUltimap2Worker;

    fstate: TRecordState;
    PostProcessingFilter: TFilterOption; //called when all threads enter the done state
    Filtercount: byte;
    FilterRangeFrom, FilterRangeTo: qword;

    filterThread: TUltimap2FilterThread;

    filterExcludeFuturePaths: boolean;

    validList: TIndexedAVLTree;

    maxrangecount: integer;

    ticks: integer;
    FlushInterval: integer;
    maxfilesize: integer;
    LastTotalDataCollected: qword;

    filterHotkey: array [-1..1] of TGenericHotkey; //-1,0,1 due to existing tags

    iptdatadispatcher: TIPTDataDispatcher;

    procedure startWindowsBasedIPT;

    function RegionCompare(Tree: TAvgLvlTree; Data1, Data2: pointer): integer;

    function  ValidListCompare(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
    procedure FreeValidList;
    procedure freeRegion(r: PRegionInfo);
    procedure cleanup;
    procedure setConfigGUIState(state: boolean);
    procedure enableConfigGUI;
    procedure disableConfigGUI;

    procedure FilterGUI(state: boolean; showCancel: boolean=true);
    procedure Filter(filterOption: TFilterOption);
    procedure FlushResults(f: TFilterOption=foNone);

    procedure setState(state: TRecordState);
    function ModuleSelectEvent(index: integer; listText: string): string;
    function getMatchCount: integer;

    property state:TRecordState read fstate write setState;
  public
    { public declarations }
    allNewAreInvalid: boolean;

    function IsMatchingAddress(address: ptruint; count: pinteger=nil): boolean;
    procedure lowerIPTSize;
  published
    property Count: integer read getMatchCount;
  end;

procedure initializeLuaUltimap2;

var
  frmUltimap2: TfrmUltimap2;

implementation

{$R *.lfm}

uses symbolhandler, symbolhandlerstructs, frmSelectionlistunit, cpuidUnit, MemoryBrowserFormUnit,
  AdvancedOptionsUnit, vmxfunctions, LuaHandler, frmHotkeyExUnit, mainunit2, debughelper, globals;

resourcestring
rsRecording2 = 'Recording';
rsPaused = 'Paused';
rsProcessingData = 'Processing'#13#10'Data';
rsOnlyForIntelCPUs = 'Sorry, but Ultimap2 only works on Intel CPU''s';
rsSorryButYourCPUSeemsToBeLeackingIPTFeature = 'Sorry, but your CPU seems to be lacking the Intel Processor Trace feature which Ultimap2 makes use of';
rsSorryButYourCPUsImplementationOfTheIPTFeatureIsTooOld = 'Sorry, but your CPU''s implementation of the Intel Processor Trace feature is too old. Ultimap uses multiple ToPA entries';
rsSorryButYourCPUDoesntSeemToBeAbleToSetATargetProcess = 'Sorry, but your CPU doesn''t seem to be able to set a target PROCESS';
rsFirstOpenAProcess = 'First open a process';
rsTargetADifferentProcess = 'Target a different process. Ultimap2 will suspend the target when the buffer is full, and suspending the thing that empties the buffer is not a good idea';
rsTheSizeHasToBe12KbOrHigher = 'The size has to be 12KB or higher';
rsForSomeWeirdReason = 'For some weird reason "';
rsCantBeParsed = '" can''t be parsed';
rsDoesntExistAndCantBeCreated = ' does not exist and can not be created';
rsCPU = 'CPU';
rsThread = 'Thread';
rsFailureLoadingLibipt = 'Failure loading libipt';
rsClosingWillFreeAllCollectedData = 'Closing will free all collected data. Continue? (Tip: You can minimize this window instead)';
rsRangesEmptyForAllMax = 'Ranges: (Empty for all) (Max %d)';
rsDashError = ' -error';
rsMaxAmountOfRangesReachedForYourCpu = 'Max amount of ranges reached for your CPU. Clear one first';
rsModuleList = 'Module list';
rsSelectAModuleOrGiveYourOwnRange = 'Select a module or give your own range';
rsPutBetweenToMarsAsAnAutoStopRange = '(Put between *''s to mark as an auto stop range)';
rsTheRangeYouHaveProvidedIsAnExitRangeBeAware = 'The range you have provided is an ''Exit'' range. Be aware that this doesn''t mean it will always stop at that range, or that the result is what you expect. A context switch to another thread between the start and stop can add a lot of other data';
rsIsAnInvalidRange = ' is an invalid range';
rsInstructionPointerListSize = 'Instruction Pointer List Size:';
rsRangesNeedDBVMInWindows10 = 'To use ranges with Ultimap2 in windows 10, you '
  +'must hide the fact that you use ranges from it. To be able to do that '
  +'DBVM needs to be running. There is a chance running DBVM can crash your '
  +'system and make you lose your data(So don''t forget to save first). Do you'
  +' want to run DBVM?';
rsYouMustSelectABuffersize = 'You must select a buffersize';

//worker



function iptReadMemory(buffer: PByte; size: SIZE_T; asid: PPT_ASID; ip: uint64; context: pointer): integer; cdecl;
var worker: TUltimap2Worker;
  n: TAvgLvlTreeNode;
  e: TRegionInfo;

  s: integer;
begin
  result:=0;

  worker:=TUltimap2Worker(context);
  //watch for page boundaries

  if (worker.lastRegion=nil) or (ip<worker.lastRegion^.address) or (ip>=(worker.lastRegion^.address+worker.lastRegion^.size)) then
  begin
    e.address:=ip;

    n:=worker.localregiontree.Find(@e);
    if n=nil then
    begin
      worker.ownerForm.regiontreeMREW.Beginread;
      n:=worker.ownerForm.regiontree.Find(@e);
      worker.ownerForm.regiontreeMREW.endRead;

      if n<>nil then
        worker.localregiontree.add(n.data);
    end;


    if n<>nil then
      worker.lastRegion:=PRegionInfo(n.Data)
    else
    begin
      //self.lastRegion:=nil;
      worker.lastregion:=worker.addIPBlockToRegionTree(ip);
      if worker.lastregion=nil then
        exit(-integer(pte_nomap))
      else
        worker.localregiontree.add(worker.lastRegion);
    end;
  end;

  if worker.lastRegion<>nil then
  begin
    s:=(worker.lastRegion^.address+worker.lastRegion^.size)-ip;
    if s>size then s:=size;
    CopyMemory(buffer, @worker.lastRegion^.memory[ip-worker.lastRegion^.address], s);

    size:=size-s;
    if size>0 then
    begin
      ip:=ip+s;
      s:=s+iptReadMemory(@buffer[s], size, asid, ip, context);
    end
    else
      result:=s;
  end;


end;


//--------------------TIPTDataDispatcher:---------------------//

procedure TIPTDataDispatcher.showError;
begin
  MessageDlg('IPTDataDispatcher error: '+error, mtError,[mbok],0);
end;

procedure TIPTDataDispatcher.decreaseIPTSize;
begin
  try
    ownerform.lowerIPTSize;
  except
    on e:exception do
    begin
      terminate;
      OutputDebugString('Failure lowering the ipt size: '+e.message);
    end;
  end;
end;

procedure TIPTDataDispatcher.resumeProcessing;
var e: TDataDispatcherEventData;
begin
  e.event:=ddeStartProcessingData;
  addEvent(e);
end;

procedure TIPTDataDispatcher.pauseProcessing;
var e: TDataDispatcherEventData;
begin
  e.event:=ddeStopProcessingData;
  addEvent(e);
end;

procedure TIPTDataDispatcher.suspendThreadProcessing(threadHandle: THandle);
var e: TDataDispatcherEventData;
begin
  e.event:=ddeSuspendProcessingOfThread;
  e.threadhandle:=threadhandle;
  addEvent(e);
end;

procedure TIPTDataDispatcher.resumeThreadProcessing(threadhandle: THandle);
var e: TDataDispatcherEventData;
begin
  e.event:=ddeResumeProcessingOfThread;
  e.threadhandle:=threadhandle;
  addEvent(e);
end;

procedure TIPTDataDispatcher.flushAndWait;
var
  e: TDataDispatcherEventData;
  done: Tevent;
begin
  e.event:=ddeFlush;

  done:=tevent.Create(nil,false,false,'');
  e.done:=done;
  addEvent(e);

  if GetCurrentThreadId=MainThreadID then
  begin
    while done.WaitFor(25)=wrTimeout do
      CheckSynchronize;
  end
  else
    done.WaitFor(INFINITE);

  done.Free;
end;

procedure TIPTDataDispatcher.flush;
var
  e: TDataDispatcherEventData;
begin
  e.event:=ddeFlush;
  e.done:=nil;
  addEvent(e);
end;

procedure TIPTDataDispatcher.startFileProcessing;
var
  e: TDataDispatcherEventData;
  done: TEvent;
begin
  e.event:=ddeStartFileProcessing;

  done:=tevent.Create(nil,false,false,'');
  e.done:=done;
  addEvent(e);

  if GetCurrentThreadId=MainThreadID then
  begin
    while done.WaitFor(25)=wrTimeout do
      CheckSynchronize;
  end
  else
    done.WaitFor(INFINITE);

  done.Free;
end;

procedure TIPTDataDispatcher.addEvent(e: TDataDispatcherEventData);
var pe: PDataDispatcherEventData;
begin
  getmem(pe, sizeof(TDataDispatcherEventData));
  pe^:=e;

  eventsCS.Enter;
  events.Push(pe);
  eventscs.Leave;

  hasEvent.SetEvent;
end;

function TIPTDataDispatcher.getTotalDataCollected: qword;
var
  mi: TMapIterator;
  ti: PUltimap2ThreadInfo;
begin
  result:=0;
  threadlistMREW.Beginread;
  mi:=tmapiterator.Create(threadlist);
  mi.First;
  while not mi.eom do
  begin
    mi.GetData(ti);
    result:=result+ti^.totaldata;
    mi.next;
  end;

  threadlistMREW.Endread;
end;

procedure TIPTDataDispatcher.StartFileProcessingInternal;
var
  mi: TMapIterator;
  ti: PUltimap2ThreadInfo;
  e: TUltimap2DataEvent;
begin
  threadlistMREW.beginread;

  mi:=TMapIterator.create(threadlist);
  mi.first;
  while not mi.eom do
  begin
    mi.GetData(ti);

    closehandle(ti^.filehandle);
    RenameFile(ti^.filename, ti^.filename+'.processing');

    ti^.filehandle:=CreateFile(pchar(ti^.filename),GENERIC_WRITE, FILE_SHARE_READ, nil,CREATE_ALWAYS, FILE_FLAG_OVERLAPPED,0);

    ti^.worker.filemap:=TFileMapping.create(ti^.filename+'.processing');
    ti^.worker.totalsize:=0;
    ti^.worker.done:=false;

    ti^.worker.processWindowsIPTData(ti^.worker.filemap.fileContent, ti^.worker.filemap.filesize, nil, nil);

    mi.next;
  end;
  mi.free;

  threadlistMREW.Endread;

end;


procedure processingComplete(pworkersActive: plongint); stdcall;
begin
  InterLockedDecrement(pworkersActive^);
end;

procedure writeCompletePart2(errorcode:DWORD; dwNumberOfBytesTransfered:DWORD; overlapped:LPOVERLAPPED);stdcall;
var
  e: plongint;
  h: HANDLE;
begin
  {$IFDEF WINDOWS}
  h:=overlapped^.hEvent;
  //hevent is just a pointer to workersActive
  e:=plongint(h);

  InterLockedDecrement(e^);   //I don't think an interlocked is necesary as the overlapped routine runs in an APC from the caller thread
  {$ENDIF}
end;

procedure writeCompletePart1(errorcode:DWORD; dwNumberOfBytesTransfered:DWORD; overlapped:LPOVERLAPPED);stdcall;
var
  ti: PUltimap2ThreadInfo;
  workersActive: plongint;
  h: PIPT_TRACE_HEADER;
begin
  {$IFDEF WINDOWS}
  //hevent is a pointer to the threadinfo
  ti:=PUltimap2ThreadInfo(overlapped^.hevent);
  h:=ti^.WriteCompleteDataPart1.h;

  if ti^.WriteCompleteDataPart1.lengthpart2>0 then
  begin
    overlapped^.hEvent:=handle(ti^.WriteCompleteDataPart1.workersActive);
    overlapped^.Offset:=$ffffffff;
    overlapped^.OffsetHigh:=$ffffffff;

    WriteFileEx(ti^.filehandle, ti^.WriteCompleteDataPart1.datastartpart2,ti^.WriteCompleteDataPart1.lengthpart2, overlapped,@writeCompletePart2);
  end
  else
    InterLockedDecrement(ti^.WriteCompleteDataPart1.workersActive^);
  {$ENDIF}
end;



function TIPTDataDispatcher.processData: boolean;
var
  s: dword;
  last: ptruint;

  h:PIPT_TRACE_HEADER;
  ti: ^TUltimap2ThreadInfo;

  i: integer;
  lastbytes: qword;

  workersActive: longint;


begin
  result:=false;
  {$IFDEF WINDOWS}
  //get the IPT data, and assign worker threads to either save to disk, or process it


  if GetProcessIptTraceSize(processhandle, s)=false then
  begin
    outputdebugstring('GetProcessIptTraceSize failed');
    synchronize(@decreaseIPTSize);
    exit;
  end;

  if (tracesize=0) or (tracesize<s) then
  begin
    if trace<>nil then
      freemem(trace);

    getmem(trace,s);
    if trace=nil then exit;
    tracesize:=s;
  end;

  if GetProcessIptTrace(processhandle,trace,s)=false then exit;

  last:=ptruint(@trace^.TraceData[0])+trace^.TraceSize;
  h:=@trace^.TraceData[0];

  workersActive:=0;
  while ptruint(h)<last do
  begin
    if not threadlist.GetData(h^.ThreadId, ti) then //I can read without a lock, as i'm the only writer
    begin
      ti:=getmem(sizeof(TUltimap2ThreadInfo));
      ti^.threadid:=h^.threadid;
      ti^.lastoffset:=0;
      ti^.totaldata:=0;
      ti^.firstseen:=gettickcount64;
      ti^.lostdata:=0;
      ti^.timesseen:=0;
      ti^.overflowsseen:=0;
      ti^.paused:=false;
      ti^.waspaused:=false;

      threadlistMREW.Beginwrite;
      threadlist.Add(h^.threadid, ti);
      threadlistMREW.Endwrite;

      ti^.worker:=TUltimap2Worker.create(false, ti^.threadid, ownerform, Handle);

      if logtofolder then //create a file for this thread
      begin
        ti^.filename:=strnew(pchar(outputfolder+rsthread+inttohex(ti^.threadid,1)+'.trace'));
        ti^.filehandle:=CreateFile(ti^.filename,GENERIC_WRITE, FILE_SHARE_READ, nil,CREATE_ALWAYS, FILE_FLAG_OVERLAPPED,0);

        if (ti^.filehandle=0) or (ti^.filehandle=INVALID_HANDLE_VALUE) then
        begin
          error:='Failure creating file '+ti^.filename;
          synchronize(@showerror);
          terminate;
          exit;
        end;

        ti^.worker.fromFile:=logtofolder;
        ti^.worker.Filename:=ti^.filename;
        ti^.worker.KeepTraceFiles:=keeptracefiles;
        ti^.worker.parseAsText:=parseAsText;
        ti^.worker.textfolder:=textfolder;


      end
      else
        ti^.filehandle:=0;
    end;

    if ti^.paused=false then
    begin

      ti^.lastseen:=gettickcount64;
      inc(ti^.timesseen);


      if ti^.lastoffset>=8 then
        lastbytes:=pqword(@h^.trace[ti^.lastoffset-8])^
      else
        for i:=0 to 7 do
          pbyte(@lastbytes)[i]:=h^.Trace[(h^.TraceSize+(ti^.lastoffset-8+i)) mod h^.TraceSize];

      if (ti^.totaldata<>0) and (ti^.lastbytes<>lastbytes) then  //the buffer got overwritten
      begin
        if not (waspaused or ti^.waspaused) then
          inc(ti^.lostData);
      end;





      if h^.RingBufferOffset>0 then
        dec(h^.RingBufferOffset);
      while (h^.RingBufferOffset>0) and (h^.Trace[h^.RingBufferOffset]=0) do
        dec(h^.RingBufferOffset);

      inc(h^.RingBufferOffset);


      InterLockedIncrement(workersActive);

      if h^.RingBufferOffset<ti^.lastoffset then
      begin
        inc(ti^.overflowsseen);

        //data from ti^.lastoffset to h^.tracesize and from 0 to h^.RingBufferOffset have to be processed

        if logtofolder then
        begin
          //pass this data to the file
          ti^.overlapped.hEvent:=thandle(ti);
          ti^.overlapped.Offset:=$ffffffff;
          ti^.overlapped.OffsetHigh:=$ffffffff;
          ti^.WriteCompleteDataPart1.h:=h;
          ti^.WriteCompleteDataPart1.workersActive:=@workersActive; //should never change
          ti^.WriteCompleteDataPart1.datastartpart2:=@h^.Trace[0];
          ti^.WriteCompleteDataPart1.lengthpart2:=h^.RingBufferOffset;

          WriteFileEx(ti^.filehandle, @h^.Trace[ti^.lastoffset],h^.TraceSize-ti^.lastoffset, ti^.overlapped,@writeCompletePart1);

        end
        else
        begin
          //pass these blocks on to a decoder
          ti^.worker.processWindowsIPTDataWithRollOver(@h^.Trace[ti^.lastoffset], h^.TraceSize-ti^.lastoffset, @h^.Trace[0], h^.RingBufferOffset, PAPCFUNC(@processingComplete), @workersActive);
          // debug speed: QueueUserAPC(PAPCFUNC(@processingComplete), handle, ULONG_PTR(@workersActive));
        end;

        //todo: copy/process data
        inc(ti^.totaldata, h^.TraceSize-ti^.lastoffset);
        ti^.lastoffset:=0;
      end
      else
      begin

        if logtofolder then
        begin
          //pass this data to the file (2)
          ti^.overlapped.hEvent:=thandle(@workersActive);
          ti^.overlapped.Offset:=$ffffffff;
          ti^.overlapped.OffsetHigh:=$ffffffff;
          WriteFileEx(ti^.filehandle, @h^.Trace[ti^.lastoffset],h^.RingBufferOffset-ti^.lastoffset, ti^.overlapped,@writeCompletePart2);
        end
        else
        begin
          //pass this block on to a decoder (2)
          ti^.worker.processWindowsIPTData(@h^.Trace[ti^.lastoffset], h^.RingBufferOffset-ti^.lastoffset, PAPCFUNC(@processingComplete), @workersActive);

          // QueueUserAPC(PAPCFUNC(@processingComplete), handle, ULONG_PTR(@workersActive));
        end;

      end;

      inc(ti^.totaldata,h^.RingBufferOffset-ti^.lastoffset);

      ti^.lastoffset:=h^.RingBufferOffset;

      //update lastbytes
      lastbytes:=0;
      if ti^.lastoffset>=8 then
        lastbytes:=pqword(@h^.trace[ti^.lastoffset-8])^
      else
        for i:=0 to 7 do
          pbyte(@lastbytes)[i]:=h^.Trace[(h^.TraceSize+(ti^.lastoffset-8+i)) mod h^.TraceSize];

      ti^.lastbytes:=lastbytes;

      ti^.waspaused:=false;
    end
    else
    begin
      ti^.waspaused:=true;
      ti^.lastoffset:=h^.RingBufferOffset;
    end;
    if h^.tracesize=0 then break;
    h:=PIPT_TRACE_HEADER(ptruint(@h^.Trace[0])+h^.tracesize);
  end;



  while (workersActive>0) and (not terminated) do
    SleepEx(2000,true);

  exit(true);
  {$ENDIF}
end;

procedure TIPTDataDispatcher.execute;
var
  wr: TWaitResult;
  dde: PDataDispatcherEventData;
  r: BOOLEAN;
  i: integer;
begin
  while not terminated do
  begin
    if paused then
      wr:=hasEvent.WaitFor(2000);

    if terminated then exit;

    repeat
      eventsCS.Enter;
      dde:=events.Pop;
      eventsCS.Leave;

      if terminated then exit;

      if dde<>nil then
      begin
        case dde^.event of
          ddeStartProcessingData:
          begin
            paused:=false;
          end;

          ddeStopProcessingData:
          begin
            paused:=true;
            waspaused:=true;
          end;


{$IFDEF WINDOWS}
          ddeSuspendProcessingOfThread: PauseThreadIptTracing(dde^.threadhandle, r);
          ddeResumeProcessingOfThread: ResumeThreadIptTracing(dde^.threadhandle, r);
 {$ENDIF}
          ddeFlush:
          begin
            if processData=false then
            if processData=false then
              processData; //try 3 times at most

            waspaused:=paused;
            if dde^.done<>nil then
              dde^.done.SetEvent;
          end;

          ddeStartFileProcessing:
          begin
            StartFileProcessingInternal;
            if dde^.done<>nil then
              dde^.done.SetEvent;
          end;
        end;
        freemem(dde);
      end;

      if not (paused or terminated) then
      begin
        processData;
        waspaused:=false;
      end;

    until (dde=nil) or terminated;
  end;
end;

procedure TIPTDataDispatcher.TerminatedSet;
begin
  if hasEvent<>nil then
    hasEvent.SetEvent;
end;

destructor TIPTDataDispatcher.Destroy;
var
  i: TMapIterator;
  d: PUltimap2ThreadInfo;
begin
  terminate;
  waitfor;

  if hasEvent<>nil then
    freeandnil(hasEvent);

  if eventsCS<>nil then
    freeandnil(eventsCS);

  if events<>nil then
    freeandnil(events);

  i:=TMapIterator.Create(threadlist);
  i.First;
  while not i.eom do
  begin
    i.GetData(d);

    if (d^.filehandle<>0) and (d^.filehandle<>INVALID_HANDLE_VALUE) then
      closehandle(d^.filehandle);

    if d^.worker<>nil then
    begin
      d^.worker.Terminate;
      d^.worker.WaitFor;
      d^.worker.free;
      d^.worker:=nil;
    end;

    strdispose(d^.filename);

    freemem(d);
    i.Next;
  end;
  freeandnil(threadlist);

  threadlistMREW.Free;




  inherited destroy;
end;

constructor TIPTDataDispatcher.Create(CreateSuspended: Boolean; const StackSize: SizeUInt=DefaultStackSize);
begin
  events:=TQueue.Create;
  eventsCS:=TCriticalSection.Create;
  hasEvent:=tevent.Create(nil,false,false,'');

  threadlistMREW:=TMultiReadExclusiveWriteSynchronizer.create;
  threadlist:=TMap.Create(itu8,sizeof(pointer));
  paused:=true;

  inherited create(CreateSuspended, StackSize);
end;

//---------------------TUltimap2Worker:-----------------------//

function TUltimap2Worker.addIPPageToRegionTree(IP: QWORD): PRegionInfo;
//Write lock must be obtained beforehand
var
  page: pbyte;
  baseaddress: ptruint;
  br: ptruint;

  p: PRegionInfo;
begin
  result:=nil;
  baseaddress:=ip and (not qword($fff));

  getmem(page,4096);
  if ReadProcessMemory(processhandle, pointer(baseaddress),page, 4096, br) then
  begin
    //successful read, add it
    getmem(p, sizeof(TRegionInfo));
    p^.address:=baseaddress;
    p^.size:=br;
    p^.memory:=page;
    getmem(p^.info, 4096*sizeof(TByteInfo));

    if ownerform.allNewAreInvalid then
      FillMemory(p^.info, p^.size, $ff) //marks it as filtered out
    else
      zeromemory(p^.info, p^.size*sizeof(TByteInfo));

    ownerForm.regiontree.Add(p);
    result:=p;
  end
  else
    FreeMemAndNil(page);
end;

function TUltimap2Worker.addIPBlockToRegionTree(IP: QWORD): PRegionInfo;
var
  e: TRegionInfo;
  p: PRegionInfo;
  baseaddress: ptruint;
  currentAddress, endaddress: ptruint;
  mbi: TMemoryBasicInformation;
  i: integer;
  br: ptruint;
begin
  //read the memory and add it if necesary
  result:=nil;

  ownerForm.regiontreeMREW.Beginwrite;
  endaddress:=$fffffffffffffff;

  try
    e.address:=ip;
    if ownerForm.regiontree.Find(@e)<>nil then exit; //something else already added it

    //find which pages are in and which ones are not.  Scan till a page is inside a region, or until a memory address is not found

    baseaddress:=ip and (not qword($fff));

    if VirtualQueryEx(processhandle, pointer(baseaddress), mbi, sizeof(mbi))=sizeof(mbi) then
    begin
      if mbi.State=MEM_COMMIT then
      begin
        currentAddress:=baseaddress+4096;
        while currentAddress<endaddress do //scan till the end and see if it's in the list
        begin
          e.address:=currentAddress;

          if ownerForm.regiontree.Find(@e)<>nil then //found something
            break;

          inc(currentAddress,4096);
        end;

        endaddress:=currentAddress;

        //scan backwards (using virtualqueryex and the regiontree)
        i:=0;
        currentaddress:=baseaddress-4096;
        while (VirtualQueryEx(processhandle, pointer(baseaddress), mbi, sizeof(mbi))=sizeof(mbi)) and (i<16) do
        begin
          if mbi.State<>MEM_COMMIT then
            break;

          e.address:=currentAddress;
          if ownerForm.regiontree.Find(@e)<>nil then //found something
            break;

          dec(currentAddress, 4096);
          inc(i);
        end;

        baseaddress:=currentaddress+4096;

        //allocate memory for this and fill it

        getmem(p, sizeof(TRegionInfo));
        p^.address:=baseaddress;
        p^.size:=endaddress-baseaddress;
        getmem(p^.memory, p^.size);
        getmem(p^.info, p^.size*sizeof(TByteInfo));


        br:=0;
        ReadProcessMemory(processhandle, pointer(baseaddress),p^.memory, p^.size, br);
        if br<endaddress-baseaddress then
        begin
          p^.size:=br; //fix size

          if (br=0) or (ip>(baseaddress+br)) then //failure. Try a single page
          begin
            ownerForm.freeRegion(p);
            exit(addIPPageToRegionTree(IP));
          end;
        end;

        if ownerform.allNewAreInvalid then
          FillMemory(p^.info, p^.size, $ff)
        else
          zeromemory(p^.info, p^.size*sizeof(TByteInfo));

        ownerForm.regiontree.Add(p);

        result:=p;

      end
      else
        exit;
    end;

  finally
    ownerform.regiontreeMREW.Endwrite;
  end;
end;


procedure TUltimap2Worker.HandleIPForRegion(ip: qword; c: pt_insn_class; region: PRegionInfo);
var index: integer;
begin
  //do something with this IP
  index:=ip-region^.address;

  if (region^.info[index].flags=bifInvalidated) then exit;

  if (region^.info[index].count=0) then
  begin
    if ownerForm.allNewAreInvalid then
    begin
      region^.info[index].flags:=bifInvalidated;
      exit;
    end;

    region^.info[index].count:=1;


    if c in [ptic_call, ptic_far_call] then
      region^.info[index].flags:=region^.info[index].flags or bifIsCall;
  end
  else
  if region^.info[index].count<255 then
    inc(region^.info[index].count);

  region^.info[index].flags:=region^.info[index].flags or bifExecuted;
end;


function TUltimap2Worker.RegionCompare(Tree: TAvgLvlTree; Data1, Data2: pointer): integer;
var
  d1,d2: PRegionInfo;
begin
  d1:=data1;
  d2:=data2;

  if (d1^.address>=d2^.address) and (d1^.address<d2^.address+d2^.size) then
    result:=0
  else
    result:=CompareValue(d2^.address, d1^.address);
end;


procedure TUltimap2Worker.HandleIP(ip: QWORD; c: pt_insn_class);
var
  e: TRegionInfo;
  n: TAvgLvlTreeNode;
begin
  if ip=$10002B490 then
  asm
  nop
  end;

  if (lastRegion<>nil) and ((ip>=lastRegion^.address) and (ip<lastRegion^.address+lastRegion^.size)) then
  begin
    HandleIPForRegion(ip,c, lastRegion);
    exit;
  end;
  lastregion:=nil;

  e.address:=ip;
  n:=localregiontree.find(@e);
  if n<>nil then
  begin
    lastregion:=n.data;
    HandleIPForRegion(ip, c, lastRegion);
  end
  else
  begin
    //not yet in the local list. check global
    ownerform.regiontreeMREW.Beginread;
    n:=ownerform.regiontree.Find(@e);
    if n<>nil then
      lastRegion:=n.data;

    ownerform.regiontreeMREW.Endread;

    if lastregion=nil then
      lastregion:=addIPBlockToRegionTree(ip);

    if lastRegion<>nil then
    begin
      localregiontree.Add(lastregion); //add to the local region tree, so no locking has to be applied next time
      HandleIPForRegion(ip, c, lastRegion);
    end;
  end;
end;

function TUltimap2Worker.waitForData(timeout: dword; var e: TUltimap2DataEvent): boolean;
begin
  {$ifdef windows}
  result:=false;
  if fromfile then
  begin
    //wait for the fileready event
    if (id=qword(-1)) or (processFile.WaitFor(timeout)=wrSignaled) then
    begin
      if id<>qword(-1) then
        ultimap2_lockfile(id);

      if fileexists(filename) then
      begin
        if fileexists(filename+'.processing') then   //'shouldn't' happen
          deletefile(filename+'.processing');

        if id=qword(-1) then
          copyfile(filename, filename+'.processing')
        else
          renamefile(filename, filename+'.processing');

        if id<>qword(-1) then
          ultimap2_releasefile(id);

        filemap:=TFileMapping.create(filename+'.processing');

        e.Address:=ptruint(filemap.fileContent);
        e.Size:=filemap.filesize;
        e.Cpunr:=id;
        result:=true;
      end;
    end
    else
      done:=true; //nothing to process. There is no file...
  end
  else
  begin
    result:=ultimap2_waitForData(timeout, e);
    if result then
    begin
      OutputDebugString('ultimap2_waitForData returned true for cpu '+inttostr(e.Cpunr));
    end;
  end;
  {$endif}
end;

procedure TUltimap2Worker.continueFromData(e: TUltimap2DataEvent);
var fn: string;
begin
  {$ifdef windows}
  if fromfile then
  begin
    OutputDebugString(inttostr(e.cpunr)+' continueFromData for file');
    if filemap<>nil then
    begin
      fn:=filemap.filename;
      freeandnil(filemap);

      if KeepTraceFiles then
      begin
        RenameFile(fn, filename+'.processed'+inttostr(filecount));
        inc(filecount);
      end
      else
        deletefile(fn);
    end;

    done:=true;
  end
  else
  begin
    if windowsBasedIPT=false then
    begin
      outputdebugstring('Calling ultimap2_continue for cpu '+inttostr(e.cpunr));
      ultimap2_continue(e.Cpunr);
    end;
  end;
  {$endif}
end;

procedure TUltimap2Worker.parseToStringlist(insn: pt_insn; output: Tstrings);
var
  s: string;
  desc: string;
  ip: ptruint;
  i: integer;
begin
  if insn.iclass=ptic_error then
  begin
    output.add('');
    output.Add('<???>');
    output.add('');
  end;
  ip:=insn.ip;

  disassembler.is64bitOverride:=true;
  disassembler.is64BitOverrideState:=insn.mode = ptem_64bit;
  disassembler.disassemble(ip, desc);

  s:=inttohex(insn.ip,8);
  while length(s)<11 do
    s:=s+' ';

  s:=s+' - ';


  for i:=0 to insn.size-1 do
    s:=s+inttohex(insn.raw[i],2)+' ';

  while length(s)<35 do
    s:=s+' ';

  s:=s+' - ';

  s:=s+disassembler.LastDisassembleData.opcode+' '+disassembler.LastDisassembleData.parameters;

  if (insn.flag0 and 1)=1 then
    s:='*';

  if (insn.flag0 and (1 shl 4))=(1 shl 4) then
  begin
    output.add('');
    output.add('-------Start of new block-------');
  end;

  if (insn.flag0 and (1 shl 5))=(1 shl 5) then
  begin
    output.add('');
    output.add('-------Resume of current block-------');
  end;

  output.Add(s);

  if (insn.flag0 and (1 shl 1))=(1 shl 1) then
  begin
    output.add('-------Aborted-------');
    output.add('');
  end;

  if (insn.flag0 and (1 shl 2))=(1 shl 2) then
  begin
    output.add('-------Commited-------');
    output.add('');
  end;

  if (insn.flag0 and (1 shl 3))=(1 shl 3) then
  begin
    output.add('');
    output.add('-------End of block-------');
  end;





  if (insn.flag0 and (1 shl 6))=(1 shl 6) then
  begin
    output.add('-------Interrupted-------');
    output.add('');
  end;

  if (insn.flag0 and (1 shl 7))=(1 shl 7) then
  begin
    output.add('-------Resynced-------');
    output.add('');
  end;

  if (insn.flag0 and (1 shl 8))=(1 shl 8) then
  begin
    output.add('');
    output.add('-------Stopped-------');
    output.add('');
  end;
end;

procedure TUltimap2Worker.processData(e: TUltimap2DataEvent);
var
  insn: pt_insn;
  tf: TFileStream=nil;
  i: integer;
  name:string;
begin
  OutputDebugString(format('%d: Ultimap2Worker data available. Size=%d',[id, e.size]));
  try
    try
      //process the data between e.Address and e.Address+e.Size
      totalsize:=e.Size;
      iptConfig.beginaddress:=pointer(e.Address);
      iptConfig.endaddress:=pointer(e.Address+e.Size);

      decoder:=pt_insn_alloc_decoder(@iptConfig);
      if decoder<>nil then
      begin
        try
          pt_insn_set_image(decoder, callbackImage);

          if parseAsText then //create the textfile
          begin
            if windowsBasedIPT then
              name:='thread'+inttohex(e.Cpunr,4)+'_trace.txt' //cpunr is threadid here
            else
              name:='cpu'+inttostr(e.cpunr)+'_trace.txt' ;

            try
              if FileExists(textFolder+name) then
                tf:=TFileStream.Create(textFolder+name, fmOpenReadWrite or fmShareDenyNone)
              else
                tf:=TFileStream.Create(textFolder+name, fmCreate or fmShareDenyNone)
            except
              OutputDebugString('failed creating or opening '+textFolder+name);
              tf:=nil
            end
          end;

          //scan through this decoder

          i:=0;
          while (pt_insn_sync_forward(decoder)>=0) and (not terminated) do
          begin
            zeromemory(@insn,sizeof(insn));
            while (pt_insn_next(decoder, @insn, sizeof(insn))>=0) and (not terminated) do
            begin
              if parseAsText then
                parseToStringlist(insn, ts);

              if insn.iclass=ptic_error then
              begin
                break;
              end;

              handleIP(insn.ip, insn.iclass);

              inc(i);
              if i>512 then
              begin
                pt_insn_get_offset(decoder, @processed);

                i:=0;


                if parseAsText and (tf<>nil) then //flush to the file
                begin
                  ts.SaveToStream(tf);
                  ts.clear;
                end;
              end;


            end;

            if parseAsText then
            begin
              ts.add('');
              ts.add('-----New block-----');
              ts.add('');
            end;
          end;
        finally
          pt_insn_free_decoder(decoder);

          if parseAsText and (tf<>nil) then
          begin
            if ts.Count>0 then //flush
            begin
              ts.SaveToStream(tf);
              ts.clear;
            end;

            freeandnil(tf); //close
          end;
        end;
      end;

    finally
      processed:=totalsize;
      done:=true;
      continueFromData(e);
    end;

    OutputDebugString(format('%d: Ultimap2Worker data processed successfully', [id]));
  except
    on e:exception do
    begin
      OutputDebugString(format('%d: Ultimap2Worker exception during processing data : %s',[id, e.Message]));
    end;
  end;
end;

procedure TUltimap2Worker.processWindowsIPTDataWithRollOverImplementation(data1: pointer; datasize1: dword; data2: pointer; datasize2: dword; oncompletion: PAPCFUNC; workersActive: plongint);
begin
  if rolloverhelpersize<datasize1+datasize2 then
  begin
    freemem(rolloverhelper);
    getmem(rolloverhelper, datasize1+datasize2);
  end;

  copymemory(rolloverhelper, data1,datasize1);
  copymemory(pointer(ptruint(rolloverhelper+datasize1)), data2, datasize2);
  processWindowsIPTDataImplementation(rolloverhelper, datasize1+datasize2, oncompletion, workersActive);

end;

procedure TUltimap2Worker.processWindowsIPTDataImplementation(data: pointer; datasize: dword; oncompletion: PAPCFUNC; workersActive: plongint);
var
  e: TUltimap2DataEvent;
begin
  {$IFDEF WINDOWS}
  e.Address:=qword(data);
  e.Size:=datasize;
  e.Cpunr:=id;
  try
    processData(e);
  except
    on x: exception do
    begin
      OutputDebugString(x.message);
    end;
  end;

  if assigned(oncompletion) and (workersActive<>nil) then
    QueueUserAPC(oncompletion, ownerThreadHandle, qword(workersActive));
  {$ENDIF}

end;

procedure TUltimap2Worker.processWindowsIPTDataWithRollOver(data1: pointer; datasize1: dword; data2: pointer; datasize2: dword; oncompletion: PAPCFUNC; workersActive: plongint);
var command: PUltimap2WorkerCommandData;
begin
  command:=getmem(sizeof(TUltimap2WorkerCommandData));
  command^.command:=uwcProcessDataCombined;
  command^.datacombined.workersactive:=workersActive;
  command^.datacombined.oncompletion:=oncompletion;
  command^.datacombined.data1:=data1;
  command^.datacombined.size1:=datasize1;
  command^.datacombined.data2:=data2;
  command^.datacombined.size2:=datasize2;

  commandsCS.enter;
  commands.Push(command);
  commandsCS.leave;

  hasCommandEvent.SetEvent;
end;

procedure TUltimap2Worker.processWindowsIPTData(data: pointer; datasize: dword; oncompletion: PAPCFUNC; workersActive: plongint);
var command: PUltimap2WorkerCommandData;
begin
  command:=getmem(sizeof(TUltimap2WorkerCommandData));

  command^.command:=uwcProcessData;
  command^.data.workersactive:=workersActive;
  command^.data.oncompletion:=oncompletion;
  command^.data.data:=data;
  command^.data.size:=datasize;

  commandsCS.enter;
  commands.Push(command);
  commandsCS.leave;

  hasCommandEvent.SetEvent;
end;

procedure TUltimap2Worker.execute;
var
  e: TUltimap2DataEvent;
  command: PUltimap2WorkerCommandData;
  i: integer;
begin
  OutputDebugString(format('%d: Ultimap2Worker launched',[id]));

  if parseAsText then
  begin
    ts:=TStringList.Create;
    disassembler:=TDisassembler.Create;
    disassembler.showmodules:=true;
    disassembler.showsymbols:=true;
    disassembler.showsections:=false;
    disassembler.dataOnly:=true;
  end
  else
    ts:=nil;


  while not terminated do
  begin
    if windowsBasedIPT then
    begin
      //wait for a command
      hasCommandEvent.WaitFor({$ifdef XDEBUG}1000{$else}INFINITE{$endif});

      repeat
        commandsCS.enter;
        command:=Commands.pop;
        commandsCS.leave;


        if not terminated and (command<>nil) then
        begin
          case command^.command of
            uwcTerminate: terminate;
            uwcProcessDataCombined: processWindowsIPTDataWithRollOverImplementation(command^.datacombined.data1, command^.datacombined.size1, command^.datacombined.data2, command^.datacombined.size2, command^.datacombined.oncompletion, command^.datacombined.workersActive);
            uwcProcessData: processWindowsIPTDataImplementation(command^.data.data, command^.data.size, command^.data.oncompletion, command^.data.workersActive);
          end;
        end;

        freemem(command);

      until terminated or (command=nil);

    end
    else
    begin
      if waitForData(250, e) then
      begin
        if terminated then break;
        processData(e);


        OutputDebugString(format('%d: Ultimap2Worker waiting for new data', [id]));
      end else sleep(1);
    end;
  end;

  done:=true;
end;

procedure TUltimap2Worker.TerminatedSet;
begin
  inherited TerminatedSet;

  if hasCommandEvent<>nil then
    hasCommandEvent.SetEvent;

  if processFile<>nil then
    processFile.SetEvent;
end;

destructor TUltimap2Worker.destroy;
begin
  terminate;
  if hasCommandEvent<>nil then
    hasCommandEvent.SetEvent;

  if processFile<>nil then
    processFile.SetEvent;

  waitFor;

  if commandscs<>nil then
    freeandnil(commandscs);

  if commands<>nil then
    freeandnil(commands);

  if hasCommandEvent<>nil then
    freeandnil(hasCommandEvent);

  if callbackImage<>nil then
    pt_image_free(callbackImage);

  if ts<>nil then
    freeandnil(ts);

  if disassembler<>nil then
    freeandnil(disassembler);

  if rolloverhelper<>nil then
    freememandnil(rolloverhelper);

  if localregiontree<>nil then
  begin
    localregiontree.Clear;
    freeandnil(localregiontree);
  end;

  inherited destroy;
end;

constructor TUltimap2Worker.create(CreateSuspended: boolean; cpuid: integer; owner: TfrmUltimap2);
begin
  inherited create(true);
  id:=cpuid;
  ownerform:=owner;

  processFile:=TEvent.Create(nil,false,false,'');

  callbackImage:=pt_image_alloc(pchar('cpu'+inttostr(id)));
  pt_image_set_callback(callbackImage,@iptReadMemory,self);

  pt_config_init(@iptConfig);
  pt_cpu_read(@iptConfig.cpu);
  pt_cpu_errata(@iptConfig.errata, @iptConfig.cpu);

  if not CreateSuspended then start;
end;

constructor TUltimap2Worker.create(CreateSuspended: boolean; tid: qword; owner: TfrmUltimap2; ownerthread: TThreadID);
begin
  {$IFDEF WINDOWS}
  windowsBasedIPT:=true;

  id:=tid;
  callbackImage:=pt_image_alloc(pchar('threadid'+inttostr(id)));
  pt_image_set_callback(callbackImage,@iptReadMemory,self);

  pt_config_init(@iptConfig);
  pt_cpu_read(@iptConfig.cpu);
  pt_cpu_errata(@iptConfig.errata, @iptConfig.cpu);

  commands:=TQueue.Create;
  commandsCS:=TcriticalSection.Create;

  hasCommandEvent:=Tevent.Create(nil,false,true,'');

  ownerThreadHandle:=ownerthread;
  ownerForm:=owner;

  localregiontree:=TAvgLvlTree.CreateObjectCompare(@RegionCompare);

  inherited create(createsuspended);
  {$ENDIF}
end;

procedure TUltimap2FilterWorker.FilterExecuted(ri: TRegionInfo);  //removes executed entries
var
  i: integer;
  bi: PByteInfo;
  w: pword absolute bi;

begin
  bi:=ri.info;
  for i:=0 to ri.size-1 do
  begin
    if (w[i]>0) and ((w[i] and 1)=0) then //has info and not yet invalidated
    begin
      if (bi[i].flags and bifExecuted)<>0 then //filter it out
      begin
        w[i]:=1; //invalidate
        inc(invalidated);
      end;
    end;
  end;
end;

procedure TUltimap2FilterWorker.FilterNotExecuted(ri: TRegionInfo);      //removes entries marked as not executed (since last filter op)
var
  i: integer;
  bi: PByteInfo;
  w: pword absolute bi;

begin
  bi:=ri.info;
  for i:=0 to ri.size-1 do
  begin
    if (w[i]>0) and ((w[i] and 1)=0) then //has info and not yet invalidated
    begin
      if (bi[i].flags and bifExecuted)=0 then //filter it out
      begin
        w[i]:=1; //invalidate
        inc(invalidated);
      end
      else //mark as not executed for next filter op
        bi[i].flags:=bi[i].flags xor bifExecuted;

    end;
  end;
end;

procedure TUltimap2FilterWorker.FilterNonCallInstruction(ri: TRegionInfo);    //removes if not call
var
  i: integer;
  bi: PByteInfo;
  w: pword absolute bi;

begin
  bi:=ri.info;
  for i:=0 to ri.size-1 do
  begin
    if (w[i]>0) and ((w[i] and 1)=0) then //has info and not yet invalidated
    begin
      if (bi[i].flags and bifIsCall)=0 then //filter it out
      begin
        w[i]:=1; //invalidate
        inc(invalidated);
      end
      else //mark as not executed for next filter op
        bi[i].flags:=bi[i].flags and (not bifExecuted);
    end;
  end;
end;

procedure TUltimap2FilterWorker.FilterExecutionCountNoEqual(ri: TRegionInfo);
var
  i: integer;
  bi: PByteInfo;
  w: pword absolute bi;

begin
  bi:=ri.info;
  for i:=0 to ri.size-1 do
  begin
    if (w[i]>0) and ((w[i] and 1)=0) then //has info and not yet invalidated
    begin
      if bi[i].count <> callcount then //filter it out
      begin
        w[i]:=1; //invalidate
        inc(invalidated);
      end
      else //mark as not executed for next filter op
        bi[i].flags:=bi[i].flags and (not bifExecuted);
    end;
  end;
end;

procedure TUltimap2FilterWorker.FilterNotInRange(ri: TRegionInfo); //mark all ranges not in this list as invalid
var
  i: integer;
  bi: PByteInfo;
  w: pword absolute bi;

  startindex, stopindex: integer;
begin
  bi:=ri.info;

       //after it                        before it
  if (ri.address>rangestop) or (ri.address+ri.size<rangestart) then //mark it all invalid
    FillMemory(ri.info, sizeof(TByteInfo)*ri.size,$ff)
  else
  begin
    startindex:=rangestart-ri.address;

    if startindex>0 then //invalidate the part up to this point
      FillMemory(ri.info, sizeof(TByteInfo)*startindex,$ff);

    stopindex:=startindex+(rangestop-rangestart);
    //everything after stopindex is invalid
    if stopindex<ri.size then
      FillMemory(@ri.info[stopindex], sizeof(TByteInfo)*(ri.size-stopindex),$ff);
  end;
end;

procedure TUltimap2FilterWorker.FilterResetCount(ri: TRegionInfo);
var
  i: integer;
  bi: PByteInfo;
  w: pword absolute bi;
begin
  bi:=ri.info;
  for i:=0 to ri.size-1 do
    if (w[i]>0) and ((w[i] and 1)=0) then //has info and not yet invalidated
      bi[i].count:=0;
end;

procedure TUltimap2FilterWorker.FilterResetAll(ri: TRegionInfo);
begin
  zeromemory(ri.info, sizeof(TByteInfo)*ri.size);
end;

procedure TUltimap2FilterWorker.execute;
var
  ri: TRegionInfo;
  i: integer;
  filterRoutine: procedure(ri: TRegionInfo) of object;
begin
  OutputDebugString(format('%d: FilterWorker alive',[GetCurrentThreadId]));
  done:=true;

  {$ifdef windows}

  case filteroption of
    foExecuted: filterRoutine:=@FilterExecuted;//
    foNotExecuted: filterRoutine:=@FilterNotExecuted;
    foNonCALLInstructions: filterRoutine:=@FilterNonCallInstruction;
    foExecuteCountNotEqual: filterRoutine:=@FilterExecutionCountNoEqual;
    foNotInRange: filterRoutine:=@FilterNotInRange;
    foResetCount: filterRoutine:=@FilterResetCount;
    foResetAll: filterRoutine:=@FilterResetAll;
    else //unknown, or foNone
      exit;
  end;

  while not terminated do
  begin
    if WaitForSingleObject(filterSemaphore, 500)=WAIT_OBJECT_0 then
    begin
      if terminated then exit;

      OutputDebugString(format('%d: FilterWorker woke up',[GetCurrentThreadId]));

      queueCS.Enter;
      dec(queuepos^);
      if queuepos^<0 then //should never happen as the only time it can happen is after the thread has been set to terminated
      begin
        OutputDebugString('error: queuepos<0');
        queuecs.Leave;
        exit;
      end;
      ri:=workqueue[queuepos^]^;
      done:=false;
      queuecs.Leave;

      //scan it's region according to the scan options
      filterRoutine(ri);

      done:=true;

      OutputDebugString(format('%d: FilterWorker returned properly. back to sleep',[GetCurrentThreadId]));
    end;
  end;
  {$endif}
end;

{TUltimap2FilterThread}

procedure TUltimap2FilterThread.EnableGUI;
begin
  ownerform.filterThread:=nil;
  ownerform.FilterGUI(true);

  if ExcludeFuturePaths and (not ownerform.cbfilterOutNewEntries.checked) then
    ownerform.cbfilterOutNewEntries.checked:=true;

  ownerform.ListView1.Refresh;
  beep;
end;

procedure TUltimap2FilterThread.execute;
var
  e: TAvgLvlTreeNodeEnumerator;
  ri: PRegionInfo;
  added: boolean;
  alldone: boolean;
  i: integer;
  count: integer;
begin
  {$ifdef windows}
  freeOnTerminate:=true;

  OutputDebugString('Filter thread alive. Spawning workers');
  count:=cpucount;
  queueCS:=TCriticalSection.Create;
  getmem(workqueue, sizeof(PRegionInfo)*count);
  filterSemaphore:=CreateSemaphore(nil, 0, count, nil);

  setlength(workers, cpucount);
  for i:=0 to length(workers)-1 do
  begin
    workers[i]:=TUltimap2FilterWorker.create(true);
    workers[i].filteroption:=filterOption;
    workers[i].callcount:=callcount;
    workers[i].rangestart:=rangestart;
    workers[i].rangestop:=rangestop;
    workers[i].ExcludeFuturePaths:=ExcludeFuturePaths;
    workers[i].queuepos:=@queuepos;
    workers[i].workqueue:=workqueue;
    workers[i].queuecs:=queuecs;
    workers[i].filterSemaphore:=filterSemaphore;
    workers[i].start;
  end;



  try
    //scan and check for terminated to see when it should terminate
    regiontreeMREW.Beginread;
    try
      e:=TAvgLvlTreeNodeEnumerator.Create(regiontree);




      while e.MoveNext do
      begin
        ri:=e.Current.data;
        if ri<>nil then
        begin
          added:=false;
          repeat
            queueCS.enter;
            if queuepos<count then
            begin
              workqueue[queuepos]:=ri;
              inc(queuepos);
              ReleaseSemaphore(filterSemaphore, 1,nil);

              added:=true;
            end;
            queueCS.Leave;

            if not added then sleep(50);
          until added or terminated;
        end;
      end;

      freeandnil(e);
    finally
      regiontreeMREW.Endread;
    end;

    //wait for all the workers to finish their job
    if terminated then
      for i:=0 to length(workers)-1 do
        workers[i].Terminate;


    while (queuepos>0) and (not terminated) do
      sleep(50);

    //queuepos=0 , wait till the last one is done
    if not terminated then
    begin
      alldone:=false;
      while not alldone do
      begin
        alldone:=true;

        for i:=0 to length(workers)-1 do
          if not workers[i].done then
          begin
            alldone:=false;
            break;
          end;

        sleep(50);
      end;
    end;

    OutputDebugString('Filter thread normal end');
  finally
    OutputDebugString('Filter thread cleanup');
    for i:=0 to length(workers)-1 do
      workers[i].Terminate;

    ReleaseSemaphore(filterSemaphore, CPUCount, nil);

    for i:=0 to length(workers)-1 do
    begin
      workers[i].WaitFor;
      workers[i].Free;
    end;
    setlength(workers,0);

    Synchronize(@EnableGUI);

    closehandle(filterSemaphore);
    freeandnil(queueCS);
    FreeMemAndNil(workqueue);
    OutputDebugString('Filter thread cleanup done');
  end;

  {$endif}
end;

{ TfrmUltimap2 }

//RegionCompare

function TfrmUltimap2.RegionCompare(Tree: TAvgLvlTree; Data1, Data2: pointer): integer;
var
  d1,d2: PRegionInfo;
  start,stop: ptruint;
  a: ptruint;
begin
  d1:=data1;
  d2:=data2;
  {
  a:=d1^.address;
  start:=d2^.address;
  stop:=d2^.address+d2^.size;

  outputdebugstring(pchar(format('is %x in %x - %x', d1^.address, start,stop)); }

  if (d1^.address>=d2^.address) and (d1^.address<d2^.address+d2^.size) then
  begin
    result:=0
  end
  else
    result:=CompareValue(d2^.address, d1^.address); //not inside
end;


procedure TfrmUltimap2.freeRegion(r: PRegionInfo);
begin
  if r<>nil then
  begin
    if r^.info<>nil then
    begin
      FreeMemAndNil(r^.info);

    end;

    if r^.memory<>nil then
    begin
      FreeMemAndNil(r^.memory);

    end;

    FreeMemAndNil(r);
  end;
end;

procedure TfrmUltimap2.FilterGUI(state: boolean; showCancel: boolean=true);
begin
  btnReset.enabled:=state;
  btnNotExecuted.enabled:=state;
  btnExecuted.enabled:=state;
  cbFilterFuturePaths.enabled:=state;
  btnNotCalled.enabled:=state;
  btnFilterCallCount.enabled:=state;
  edtCallCount.enabled:=state;
  btnResetCount.enabled:=state;
  btnFilterModule.enabled:=state;
  cbfilterOutNewEntries.enabled:=state;
  btnCancelFilter.Visible:=(not state) and showCancel;
end;

procedure TfrmUltimap2.setConfigGUIState(state: boolean);
begin
  cbWindowsBasedIPT.enabled:=state;
  lblBufferSizePerThread.enabled:=state;
  cbWinIPTBufferSize.enabled:=state;


  lblBuffersPerCPU.enabled:=state;
  edtBufSize.enabled:=state;
  lblKB.enabled:=state;
  rbLogToFolder.enabled:=state;

  if state then
  begin
    deTargetFolder.enabled:=rbLogToFolder.checked;
    cbDontDeleteTraceFiles.enabled:=rbLogToFolder.checked;
  end
  else
  begin
    deTargetFolder.enabled:=false;
    cbDontDeleteTraceFiles.enabled:=false;
  end;

  rbRuntimeParsing.enabled:=state;

  lbRange.enabled:=(maxrangecount>0) and state;
  btnAddRange.enabled:=(maxrangecount>0) and state;
  gbRange.enabled:=(maxrangecount>0) and state;


end;

procedure TfrmUltimap2.enableConfigGUI;
begin
  setConfigGUIState(true);
end;

procedure TfrmUltimap2.disableConfigGUI;
begin
  setConfigGUIState(false);
end;



procedure TfrmUltimap2.FlushResults(f: TFilterOption=foNone);
var i:integer;
begin
  {$ifdef windows}
  OutputDebugString('TfrmUltimap2.FlushResults');
  if cbWindowsBasedIPT.checked=false then
  begin
    ultimap2_resetTraceSize;

    OutputDebugString('1');
    ultimap2_flush;

    OutputDebugString('2');
  end;

  if rbLogToFolder.checked and (state=rsRecording) then
  begin
    OutputDebugString('3');
    if cbPauseTargetWhileProcessing.checked then
    begin
      advancedoptions.Pausebutton.down := True;
      advancedoptions.Pausebutton.Click;
    end;

    if cbWindowsBasedIPT.checked then
    begin
      iptdatadispatcher.flushAndWait;
      iptdatadispatcher.startFileProcessing;
      lvThreads.Columns[3].Visible:=true;   //processing column visible
    end
    else
    begin
      //signal the worker threads to process the files first
      for i:=0 to length(workers)-1 do
      begin
        workers[i].totalsize:=0;
        workers[i].done:=false;
        workers[i].processFile.SetEvent;
      end;
    end;
    tActivator.enabled:=true;

    OutputDebugString('4');


    btnShowResults.enabled:=false;
    btnRecordPause.enabled:=false;

    //when the worker threads are all done, this will become enabled

    PostProcessingFilter:=f;
    state:=rsProcessing;


    if f<>foNone then
    begin
      OutputDebugString('5');
      FilterGUI(false);
      OutputDebugString('6');
    end;
    OutputDebugString('7');
  end
  else
  begin

    OutputDebugString('8');
    //flush only returns after all data has been handled, or the data has already been handled by the file workers
    if f<>foNone then
    begin
      OutputDebugString('9');
      Filter(f);
      OutputDebugString('10');
    end;
    OutputDebugString('11');
  end;
  {$endif}
end;


procedure TfrmUltimap2.setState(state: TRecordState);
var boxsize: integer;
begin
  tProcessor.enabled:=false;

  fstate:=state;
  case state of
    rsRecording:
    begin
      label1.Caption:=rsRecording2;
      panel1.color:=clRed;

      if rbLogToFolder.checked then
      begin
        if cbAutoProcess.checked then
          tProcessor.enabled:=true;
      end;
    end;

    rsStopped:
    begin
      label1.Caption:=rsPaused;
      panel1.Color:=clGreen;
    end;

    rsProcessing:
    begin
      label1.Caption:=rsProcessingData;
      panel1.color:=$ff9900;
    end;
  end;

  boxsize:=64;
  boxsize:=max(boxsize, label1.width+4);
  boxsize:=max(boxsize, label1.height+4);

  panel1.Width:=boxsize;
  panel1.Height:=boxsize;
end;

procedure TfrmUltimap2.cleanup;
var
  i: integer;
  options: IPT_OPTIONS;
begin
  {$ifdef windows}
  if iptdatadispatcher<>nil then
  begin
    iptdatadispatcher.Terminate;
    iptdatadispatcher.WaitFor;
    freeandnil(iptdatadispatcher);

    StopProcessIptTracing(processhandle);
  end;

  FreeValidList;

  //cleanup everything
  for i:=0 to length(workers)-1 do
    workers[i].Terminate;

  for i:=0 to length(workers)-1 do
  begin
    workers[i].Free;
    workers[i]:=nil;
  end;
  setlength(workers,0);

  if regiontree<>nil then
  begin
    while regiontree.root<>nil do
    begin
      freeRegion(PRegionInfo(regiontree.root.Data));
      regiontree.Delete(regiontree.root);
    end;

    regiontree.FreeAndClear;

    freeandnil(regiontree);
  end;

  enableConfigGUI;



  if (debuggerthread<>nil) and (useintelptfordebug=false) then
    debuggerthread.initIntelPTTracing;

  ultimap2_disable;
  ultimap2Initialized:=0;

  {$endif}
end;

procedure TfrmUltimap2.lowerIPTSize;
begin
  //called when the size seems to be too big
  if cbWinIPTBufferSize.itemindex>0 then
    cbWinIPTBufferSize.itemindex:=cbWinIPTBufferSize.itemindex-1
  else
    raise exception.create('Minimum size reached');

  startWindowsBasedIPT;
end;

procedure TfrmUltimap2.startWindowsBasedIPT;
var options: IPT_OPTIONS ;
begin
  {$IFDEF WINDOWS}
  options.AsUlongLong:=0;
  options.flags.OptionVersion:=1;
  options.flags.TopaPagesPow2:=cbWinIPTBufferSize.itemindex;

  if not StartProcessIptTracing(processhandle, options) then
  begin
    //perhaps it was already running
    StopProcessIptTracing(processhandle);
    if not StartProcessIptTracing(processhandle, options) then
      raise exception.create('Failure starting windows based IPT session: '+getlasterror.ToString);
  end;
  {$ENDIF}
end;

procedure TfrmUltimap2.tbRecordPauseChange(Sender: TObject);
var
  bsize: dword;
  s: string;
  ranges: TURangeArray;
  r: TCPUIDResult;
  i: integer;

  regions: TMemoryRegions;

  p: PRegionInfo;
  br: ptruint;

  n: TAvgLvlTreeNode;
  e: TRegionInfo;

  cpuid14_0: TCPUIDResult;
  cpuid14_1: TCPUIDResult;

  initialWorkercount: integer;
begin
  initialWorkercount:=cpucount;
  {$ifdef windows}
  OutputDebugString('tbRecordPauseChange click');
  if state=rsProcessing then exit;
  try
    //if ssCtrl in GetKeyShiftState then
   //   debugmode:=true;


    if ((ultimap2Initialized=0) or (processid<>ultimap2Initialized)) then
    begin
      //first time init
      if ultimap2Initialized<>0 then
        cleanup;

      {ok, I know some of you AMD users will be wasting time removing this check
      and spending countless of hours trying to make this work, but trust me. It
      won't work. Your cpu doesn't have the required features

      Eric (db)
      }

      if not debugmode then
      begin

        r:=CPUID(0);
        if (r.ebx<>1970169159) or (r.ecx<>1818588270) or (r.edx<>1231384169) then
          raise exception.create(rsOnlyForIntelCPUs);

        if (CPUID(7,0).ebx shr 25) and 1=0 then
          raise exception.create(rsSorryButYourCPUSeemsToBeLeackingIPTFeature);

        cpuid14_0:=CPUID($14,0);
        //if ((cpuid14_0.ecx shr 1) and 1)=0 then
        //  raise exception.create(rsSorryButYourCPUsImplementationOfTheIPTFeatureIsTooOld);

        if (cpuid14_0.ebx and 1)=0 then
          raise exception.create(rsSorryButYourCPUDoesntSeemToBeAbleToSetATargetProcess);



        if processid=0 then
          raise exception.create(rsFirstOpenAProcess);

        if processid=GetCurrentProcessId then
          raise exception.create(rsTargetADifferentProcess);

        //initial checks are OK
        if cbWindowsBasedIPT.checked then
        begin
          if cbWinIPTBufferSize.ItemIndex=-1 then
            raise exception.create(rsYouMustSelectABuffersize);
        end
        else
        begin
          bsize:=strtoint(edtBufSize.text)*1024;
          if bsize<12*1024 then
            raise exception.create(rsTheSizeHasToBe12KbOrHigher);
        end;

        setlength(ranges,lbrange.count);
        for i:=0 to lbRange.Count-1 do
        begin
          s:=lbRange.Items[i];

          if length(s)>=2 then
          begin
            if (s[1]='*') and (s[length(s)]='*') then
            begin
              s:=copy(s,2,length(s)-2);
              ranges[i].isStopRange:=1;
            end;
          end;

          if symhandler.ParseRange(s, ranges[i].startAddress, ranges[i].endaddress)=false then
            raise exception.create(rsForSomeWeirdReason+lbRange.Items[i]+rsCantBeParsed);
        end;
      end;

      if rbLogToFolder.Checked then
      begin
        if not DirectoryExistsUTF8(deTargetFolder.Directory) then
        begin
          if ForceDirectoriesUTF8(deTargetFolder.Directory)=false then
            raise exception.create(deTargetFolder.Directory+rsDoesntExistAndCantBeCreated);
        end;

        if cbAutoProcess.Checked then
        begin
          if cbTraceInterval.checked then
            flushinterval:=strtoint(edtFlushInterval.Text);

          if cbWhenFilesizeAbove.checked then
            maxfilesize:=strtoint(edtMaxFilesize.Text)*4096*4096;
        end;

      end;




      //still here so everything seems alright.
      //turn off the config GUI

      OutputDebugString('Disabling config gui');

      disableConfigGUI;

      ultimap2Initialized:=processid;


      OutputDebugString('Initializing libIptInit');
      if not libIptInit then raise exception.create(rsFailureLoadingLibipt);



      if cbWindowsBasedIPT.checked then
      begin
        if iptdatadispatcher<>nil then
          freeandnil(iptdatadispatcher);

        iptdatadispatcher:=TIPTDataDispatcher.Create(true);
        iptdatadispatcher.logtofolder:=rbLogToFolder.Checked;
        iptdatadispatcher.outputfolder:=Utf8ToAnsi(deTargetFolder.Directory);
        iptdatadispatcher.ownerform:=self;
        if iptdatadispatcher.outputfolder[length(iptdatadispatcher.outputfolder)]<>PathDelim then
          iptdatadispatcher.outputfolder:=iptdatadispatcher.outputfolder+PathDelim;


        iptdatadispatcher.KeepTraceFiles:=cbDontDeleteTraceFiles.checked;

        iptdatadispatcher.parseAsText:=cbParseToTextfile.Checked;
        iptdatadispatcher.textFolder:=Utf8ToAnsi(deTextOut.Directory);
        if (iptdatadispatcher.textFolder<>'') and (iptdatadispatcher.textFolder[length(iptdatadispatcher.textFolder)]<>PathDelim) then
          iptdatadispatcher.textFolder:=iptdatadispatcher.textFolder+PathDelim;

        setlength(workers,0);
      end
      else
      begin

        //launch worker threads
        OutputDebugString('Creating '+inttostr(initialWorkercount)+' workers');

        setlength(workers, initialWorkercount);
        for i:=0 to length(workers)-1 do
        begin
          OutputDebugString('Creating worker '+inttostr(i));

          workers[i]:=TUltimap2Worker.Create(true, i, self);
          workers[i].windowsBasedIPT:=cbWindowsBasedIPT.checked;
          workers[i].fromFile:=rbLogToFolder.Checked;
          workers[i].Filename:=Utf8ToAnsi(deTargetFolder.Directory);
          if workers[i].Filename<>'' then
          begin
            if workers[i].Filename[length(workers[i].Filename)]<>PathDelim then
              workers[i].Filename:=workers[i].Filename+PathDelim;

            workers[i].Filename:=workers[i].Filename+rsCPU+inttostr(i)+'.trace';
          end;
          workers[i].KeepTraceFiles:=cbDontDeleteTraceFiles.checked;

          workers[i].parseAsText:=cbParseToTextfile.Checked;
          workers[i].textFolder:=Utf8ToAnsi(deTextOut.Directory);
          if (workers[i].textFolder<>'') and (workers[i].textFolder[length(workers[i].textFolder)]<>PathDelim) then
            workers[i].textFolder:=workers[i].textFolder+PathDelim;

          OutputDebugString('Done creating worker '+inttostr(i));
        end;
      end;


      if length(ranges)>0 then
      begin
        OutputDebugString('Reading the range memory');
        for i:=0 to length(ranges)-1 do
        begin
          getmem(p, sizeof(TRegionInfo));
          p^.address:=ranges[i].startAddress and not qword($fff); //align on a page boundary
          p^.size:=ranges[i].endaddress-p^.address;

          if (p^.size mod 4096)>0 then    //align on a pagesize
            inc(p^.size, 4096-p^.size);

          getmem(p^.memory, p^.size);
          getmem(p^.info, p^.size*sizeof(TByteInfo));
          ReadProcessMemory(processhandle, pointer(p^.address), p^.memory, p^.size, br);
          if br=0 then
            freeRegion(p)
          else
          begin
            p^.size:=br;
            zeromemory(p^.info, p^.size*sizeof(TByteInfo));
            regiontree.Add(p);
          end;
        end;
      end
      else
      begin
        OutputDebugString('Reading the executable memory');

        getexecutablememoryregionsfromregion(0, qword($7fffffffffffffff), regions); //only 7fffffffffffffff as this only records usermode (can be changed)
        for i:=0 to length(regions)-1 do
        begin
          getmem(p, sizeof(TRegionInfo));

          p^.address:=regions[i].BaseAddress;
          p^.size:=regions[i].MemorySize;
          getmem(p^.memory, p^.size);
          getmem(p^.info, p^.size*sizeof(TByteInfo));
          ReadProcessMemory(processhandle, pointer(p^.address), p^.memory, p^.size, br);
          if br=0 then
            freeRegion(p)
          else
          begin
            p^.size:=br;
            zeromemory(p^.info, p^.size*sizeof(TByteInfo));
            regiontree.Add(p);
          end;
        end;
      end;


      //start the recording


      if cbWindowsBasedIPT.checked then
      begin
        OutputDebugString('Initializing IPT.SYS and starting processtrace');
        startWindowsBasedIPT;

        iptdatadispatcher.Start;
      end
      else
      begin
        OutputDebugString('Initializing DBK32');
        DBK32Initialize;

        if not debugmode then
        begin

          if (length(ranges)>0) and (WindowsVersion>=wv10) and (cbNoInterrupts.checked=false) then
          begin
            {$ifndef NOVMX}
            NeedsDBVM(rsRangesNeedDBVMInWindows10);
            dbvm_ultimap2_hideRangeUsage;
            {$else}
            if messagedlg('It is recommended to build in release mode or with NOVMX disabled so that DBVM can be launched at this point. '+
                       'Not doing so will almost surely BSOD you as soon as a performance monitor interrupt triggers(buffer full).'+#13#10+
                       'Alternatively, you could find hal!KdDebuggerNotPresent (NOT nt!KdDebuggerNotPresent which is what hal!KdDebuggerNotPresent points at) and NULL it'#13#10+
                       'Continue?', mtWarning, [mbyes,mbno],0)<>mryes then exit;
            {$endif}
          end;



          OutputDebugString('calling ultimap2()');
          if rbLogToFolder.Checked then
            ultimap2(ifthen(cbTraceAllProcesses.checked,0,processid), bsize, deTargetFolder.Directory, ranges, cbNoInterrupts.checked, cbUsermode.checked, cbKernelmode.checked)
          else
            ultimap2(ifthen(cbTraceAllProcesses.checked,0,processid), bsize, '', ranges, cbNoInterrupts.checked, cbUsermode.checked, cbKernelmode.checked);
        end;
      end;
      if cbTraceAllProcesses.checked then
        FilterGUI(false)
      else
        FilterGUI(true);

      outputdebugstring('Starting the workers');


      if cbWindowsBasedIPT.checked then
        iptdatadispatcher.resumeProcessing
      else
      begin
        for i:=0 to length(workers)-1 do
          workers[i].start;
      end;



      state:=rsRecording;
    end
    else
    begin
      //toggle between active/disabled
      if state=rsStopped then
      begin
        if cbWindowsBasedIPT.checked then
          iptdatadispatcher.resumeProcessing
        else
          ultimap2_resume;

        state:=rsRecording;
      end
      else
      if state=rsRecording then
      begin
        if cbWindowsBasedIPT.checked then
        begin
          iptdatadispatcher.pauseProcessing;
          FlushResults(foNone);
        end
        else
        begin
          ultimap2_pause;

          if cbTraceAllProcesses.checked then
          begin
            ultimap2_resetTraceSize;
            ultimap2_flush;
          end
          else
            FlushResults(foNone);
        end;

        if rbRuntimeParsing.checked then
          state:=rsStopped;
      end;
    end;


  except
    on e: exception do
      messagedlg(e.message,mtError,[mbOK],0);
  end;
  {$endif}
end;

procedure TfrmUltimap2.tProcessorTimer(Sender: TObject);
var d: qword;
begin
  //check the state
  {$ifdef windows}
  inc(ticks);

  if cbTraceInterval.checked then
  begin
    //check if the interval has passed
    if (FlushInterval>0) and (ticks mod FlushInterval=0) then
      FlushResults;
  end;

  if cbWhenFilesizeAbove.checked then
  begin
    //check if the filesize has reached the proper size
    if cbWindowsBasedIPT.checked then
    begin
      d:=iptdatadispatcher.getTotalDataCollected;
      if d>LastTotalDataCollected+MaxFileSize then
      begin
        flushresults;
        LastTotalDataCollected:=d;
      end;
    end
    else
    begin
      if ultimap2_getTraceSize>MaxFileSize then
        FlushResults;
    end;
  end;
 // FlushResults(foNone);

 // ultimap2_getTraceSize
 {$endif}
end;

procedure TfrmUltimap2.tThreadlistUpdaterTimer(Sender: TObject);
var
  results: TFPList;
  mi: TMapIterator;
  ti: PUltimap2ThreadInfo;

  lvti: PUltimap2ThreadInfo;

  tl: tstringlist;
  tid: qword;
  i,j: integer;
  li: Tlistitem;

  totalprocessed: qword;
  totalsize: qword;
begin
  totalprocessed:=0;
  totalsize:=0;

  tl:=tstringlist.create;
  getThreadList(tl);

  if iptdatadispatcher<>nil then
  begin
    results:=TFPList.Create;
    results.Capacity:=max(32,tl.count*2);

    iptdatadispatcher.threadlistMREW.Beginread;
    mi:=TMapIterator.Create(iptdatadispatcher.threadlist);
    mi.first;
    while not mi.eom do
    begin
      mi.GetData(ti);
      results.Add(ti);
      mi.next;
    end;
    iptdatadispatcher.threadlistMREW.endread;

    mi.free;


    //sort based on the way windows returns the threadlist
    for i:=0 to tl.Count-1 do
    begin
      tid:=ptruint(tl.Objects[i]);

      for j:=i to results.count-1 do
      begin
        ti:=PUltimap2ThreadInfo(results[j]);
        if tid=ti^.threadid then
        begin
          results.Move(j,i);
          break;
        end;
      end;
    end;

    //lvthreads.BeginUpdate;
    while lvthreads.items.count>results.count do
      lvthreads.items[lvthreads.items.count-1].Delete;

    lvthreads.OnSelectItem:=nil;
    for i:=0 to results.count-1 do
    begin
      ti:=PUltimap2ThreadInfo(results[i]);

      if i<lvthreads.items.Count then
      begin
        li:=lvthreads.items[i];
        lvti:=PUltimap2ThreadInfo(li.Data);
        if lvti<>ti then //update caption
        begin
          li.Data:=ti;
          li.caption:=inttohex(ti^.threadid,4);
        end;
      end
      else
      begin
        li:=lvthreads.items.Add;
        li.caption:=inttohex(ti^.threadid,4);
        li.subitems.add(''); //data received
        li.subitems.add(''); //buffers missed
        li.subitems.add(''); //processing
        li.Data:=ti;
        li.Checked:=true;
      end;

      li.subitems[0]:=format('%.0n', [double(ti^.totaldata)]);
      li.subitems[1]:=format('%d/%d (%.2f %%)', [ti^.lostdata, ti^.timesseen, ti^.lostdata/ti^.timesseen*100]);

      if state=rsProcessing then
      begin
        totalprocessed:=totalprocessed+ti^.worker.processed;
        totalsize:=totalsize+ti^.worker.totalsize;

        li.subitems[2]:=format('%.2f %%',[ti^.worker.processed/ti^.worker.totalsize*100]);
      end
      else
        li.subitems[2]:='';

      li.checked:=not ti^.paused;
    end;

    lvthreads.OnItemChecked:=@lvThreadsItemChecked;
    //lvthreads.EndUpdate;

    results.free;
  end;

  tl.free;
end;

procedure TfrmUltimap2.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  canclose:=MessageDlg(rsClosingWillFreeAllCollectedData, mtConfirmation,[mbyes,mbno], 0, mbno)=mryes;
end;


procedure TfrmUltimap2.FormDestroy(Sender: TObject);
var
  reg: tregistry;
begin
  SaveFormPosition(self);

  reg:=TRegistry.Create;
  try
    if Reg.OpenKey('\Software\'+strCheatEngine,false) then
    begin
      Reg.WriteString('Ultimap2 Folder', deTargetFolder.Directory);
      Reg.WriteBool('Ultimap2 Keep Trace Files', cbDontDeleteTraceFiles.checked);
      Reg.WriteBool('Ultimap2 Use Disk', rbLogToFolder.Checked);
      Reg.WriteBool('Ultimap2 Auto Process', cbAutoProcess.Checked);
      Reg.WriteBool('Ultimap2 Auto Process Every Interval', cbTraceInterval.Checked);
      Reg.WriteString('Ultimap2 Auto Process Interval', edtFlushInterval.text);
      Reg.WriteBool('Ultimap2 Auto Process When Filesize Above', cbWhenFilesizeAbove.Checked);
      Reg.WriteString('Ultimap2 Auto Process Max Filesize', edtMaxFilesize.text);
      Reg.WriteBool('Ultimap2 Pause while processing', cbPauseTargetWhileProcessing.Checked);
      Reg.WriteBool('Ultimap2 Don''t delete tracefiles', cbDontDeleteTraceFiles.Checked);

      Reg.WriteBool('Ultimap2 Parse Trace As Text', cbParseToTextfile.checked);
      Reg.WriteString('Ultimap2 TextTrace Folder', deTextOut.Directory);

      Reg.WriteBool('Ultimap2 Use built-in windows IPT support',cbWindowsBasedIPT.checked);
    end;

  finally
    freeandnil(reg);
  end;

  cleanup;
  frmUltimap2:=nil;
end;

procedure TfrmUltimap2.FormShow(Sender: TObject);
var i, minwidth: integer;
begin
  {$ifdef windows}
  if WindowsVersion>=wvVista then
  begin
    i:=sendmessage(edtBufSize.Handle, EM_GETMARGINS, 0,0);
    i:=(i shr 16)+(i and $ffff);
  end
  else
  {$endif}
    i:=8;

  minwidth:=i+canvas.GetTextWidth(edtBufSize.Text);
  if edtBufSize.ClientWidth<minwidth then
    edtBufSize.ClientWidth:=minwidth;

  minwidth:=i+canvas.GetTextWidth('XXXXX');
  if edtMaxFilesize.ClientWidth<minwidth then
    edtMaxFilesize.ClientWidth:=minwidth;

  minwidth:=i+canvas.GetTextWidth('XXXX');
  if edtFlushInterval.ClientWidth<minwidth then
    edtFlushInterval.ClientWidth:=minwidth;




  if label1.Width+4>panel1.Height then
  begin
    panel1.Width:=label1.width+4;
    panel1.Height:=panel1.Width;
  end;

  btnReset.Height:=canvas.TextHeight(btnReset.caption)+3;

  DPIHelper.AdjustComboboxSize(cbWinIPTBufferSize,self.canvas);

  gbThreads.Constraints.MinHeight:=canvas.TextHeight('X')*8;
end;



procedure TfrmUltimap2.ListView1Data(Sender: TObject; Item: TListItem);
var data: PValidEntry;
begin
  if validlist<>nil then
  begin
    data:=validlist[item.index];
    item.caption:=symhandler.getNameFromAddress(data^.address);

    if data^.byteInfo^.count=255 then
      item.SubItems.Add('>=255')
    else
      item.SubItems.Add(inttostr(data^.byteInfo^.count));
          {
    if (data^.byteInfo^.flags and bifExecuted)<>0 then
      item.SubItems.Add('X')
    else
      item.SubItems.Add('');

    if (data^.byteInfo^.flags and bifIsCall)<>0 then
      item.SubItems.Add('X')
    else
      item.SubItems.Add('');   }

    if (data^.byteInfo^.flags and bifInvalidated)<>0 then
      item.SubItems.Add('X')
    else
      item.SubItems.Add('');
  end;
end;



procedure TfrmUltimap2.FormCreate(Sender: TObject);
var
  x: TWindowPosArray;
  reg: tregistry;

  r: TCPUIDResult;
  cpuid14_0: TCPUIDResult;
  cpuid14_1: TCPUIDResult;
  d: boolean;
begin
  OutputDebugString('Ultimap 2 window created');
  regiontree:=TAvgLvlTree.CreateObjectCompare(@RegionCompare);
  regiontreeMREW:=TMultiReadExclusiveWriteSynchronizer.Create;

  maxrangecount:=0;

  r:=CPUID(0);
  if (r.ebx=1970169159) and (r.ecx=1818588270) and (r.edx=1231384169) and (r.eax>=$14) and (((CPUID(7,0).ebx shr 25) and 1)=1) then
  begin
    cpuid14_0:=CPUID($14,0);
    if (((cpuid14_0.ecx shr 2) and 1)=1) and (cpuid14_0.eax>=1) then
    begin
      cpuid14_1:=CPUID($14,1);

      maxrangecount:=cpuid14_1.eax and 7;
    end;

  end;

  gbRange.caption:=format(rsRangesEmptyForAllMax,[maxrangecount]);

  if maxrangecount=0 then
  begin
    lbrange.Enabled:=false;
    btnAddRange.enabled:=false;
    gbRange.enabled:=false;;
    gbRange.visible:=false;
  end;


  state:=rsStopped;
  LoadFormPosition(self, x);
  if LoadFormPosition(self, x) then
    AutoSize:=false;

  reg:=TRegistry.Create;
  try
    if Reg.OpenKey('\Software\'+strCheatEngine,false) then
    begin
      if Reg.ValueExists('Ultimap2 Folder') then
        deTargetFolder.Directory:=Reg.ReadString('Ultimap2 Folder');

      if Reg.ValueExists('Ultimap2 Keep Trace Files') then
        cbDontDeleteTraceFiles.Checked:=Reg.ReadBool('Ultimap2 Keep Trace Files');

      if Reg.ValueExists('Ultimap2 Use Disk') then
      begin
        if Reg.ReadBool('Ultimap2 Use Disk') then
          rbLogToFolder.checked:=true
        else
          rbRuntimeParsing.checked:=true;
      end;

      if Reg.ValueExists('Ultimap2 Auto Process') then
        cbAutoProcess.Checked:=Reg.ReadBool('Ultimap2 Use Disk');

      if Reg.ValueExists('Ultimap2 Auto Process Every Interval') then
        cbTraceInterval.Checked:=Reg.ReadBool('Ultimap2 Auto Process Every Interval');

      if Reg.ValueExists('Ultimap2 Auto Process Interval') then
        edtFlushInterval.text:=Reg.ReadString('Ultimap2 Auto Process Interval');

      if Reg.ValueExists('Ultimap2 Auto Process When Filesize Above') then
        cbWhenFilesizeAbove.Checked:=Reg.ReadBool('Ultimap2 Auto Process When Filesize Above');

      if Reg.ValueExists('Ultimap2 Auto Process Max Filesize') then
        edtMaxFilesize.text:=Reg.ReadString('Ultimap2 Auto Process Max Filesize');

      if Reg.ValueExists('Ultimap2 Pause while processing') then
        cbPauseTargetWhileProcessing.Checked:=Reg.ReadBool('Ultimap2 Pause while processing');

      if Reg.ValueExists('Ultimap2 Don''t delete tracefiles') then
        cbDontDeleteTraceFiles.Checked:=Reg.ReadBool('Ultimap2 Don''t delete tracefiles');

      if Reg.ValueExists('Ultimap2 Parse Trace As Text') then
        cbParseToTextfile.checked:=reg.ReadBool('Ultimap2 Parse Trace As Text');

      if Reg.ValueExists('Ultimap2 TextTrace Folder') then
        deTextOut.Directory:=reg.ReadString('Ultimap2 TextTrace Folder');

      if reg.ValueExists('Ultimap2 Use built-in windows IPT support') then
        cbWindowsBasedIPT.checked:=reg.ReadBool('Ultimap2 Use built-in windows IPT support')
      else
        cbWindowsBasedIPT.checked:=true;

    end;
  finally
    freeandnil(reg);
  end;

  FilterGUI(false, false);
end;

function TfrmUltimap2.ModuleSelectEvent(index: integer; listText: string): string;
var
  mi: TModuleInfo;
  address: ptruint;
begin
  if (index<>-1) and (modulelist<>nil) then
  begin
    address:=ptruint(modulelist.Objects[index]);
    if symhandler.getmodulebyaddress(address, mi) then
      exit(inttohex(mi.baseaddress,8)+'-'+inttohex(mi.baseaddress+mi.basesize,8));
  end;

  result:=listText+rsDashError;
end;



procedure TfrmUltimap2.btnAddRangeClick(Sender: TObject);
var
  r: string;
  output: string;
  start, stop: QWORD;
  stoprange: boolean;
begin
  if sender=lbRange then
  begin
    if lbRange.itemindex=-1 then exit;
    output:=lbrange.items[lbRange.itemindex];
  end
  else
    output:='';

  if (sender=btnAddRange) and (lbrange.Items.Count>=maxrangecount) then
  begin
    MessageDlg(rsMaxAmountOfRangesReachedForYourCpu, mtError, [mbok],0);
    exit;
  end;

  if modulelist=nil then
    modulelist:=tstringlist.create;

  symhandler.getModuleList(modulelist);

  ShowSelectionList(self, rsModuleList, rsSelectAModuleOrGiveYourOwnRange+#13#10+rsPutBetweenToMarsAsAnAutoStopRange, modulelist, output, true, @ModuleSelectEvent);
  if output<>'' then
  begin
    //check that output can be parsed
    output:=trim(output);
    stoprange:=false;

    if length(output)>2 then
    begin
      if (output[1]='*') and (output[length(output)]='*') then
      begin
        stoprange:=true;
        messagedlg(rsTheRangeYouHaveProvidedIsAnExitRangeBeAware, mtInformation, [mbok],0);
      end;
    end;

    if symhandler.parseRange(output, start, stop) then
    begin

      if stoprange then
        r:='*'+inttohex(start,8)+'-'+inttohex(stop,8)+'*'
      else
        r:=inttohex(start,8)+'-'+inttohex(stop,8);

      if sender=lbRange then
        lbrange.items[lbRange.itemindex]:=r
      else
        lbrange.Items.Add(r);
    end;
  end;

  freeandnil(modulelist);
end;

procedure TfrmUltimap2.Filter(filterOption: TFilterOption);
begin
  if filterThread<>nil then
  begin
    OutputDebugString('Filter operation canceled. A filter operation was still going on');
    exit;
  end;

  OutputDebugString('going to launch a filter thread');

  //suspend gui
  FilterGUI(false);

  //launch the filter thread
  filterThread:=TUltimap2FilterThread.Create(true);
  filterthread.ownerform:=self;
  filterthread.regiontree:=regiontree;
  filterthread.regiontreeMREW:=regiontreeMREW;
  filterthread.filterOption:=filterOption;
  filterthread.callcount:=Filtercount;
  filterthread.rangestart:=FilterRangeFrom;
  filterthread.rangestop:=FilterRangeTo;
  filterThread.ExcludeFuturePaths:=filterExcludeFuturePaths;
  filterthread.start;

  //the filter thread will reenable the gui when done and update the windowstate  (it also sets filterthread to nil in the mainthread)
end;

procedure TfrmUltimap2.btnExecutedClick(Sender: TObject);
begin
  flushResults(foNotExecuted); //filters out not executed memory
  filterExcludeFuturePaths:=cbFilterFuturePaths.checked;
end;

procedure TfrmUltimap2.btnFilterCallCountClick(Sender: TObject);
begin
  Filtercount:=strtoint(edtCallCount.text);
  flushResults(foExecuteCountNotEqual);
end;

procedure TfrmUltimap2.btnFilterModuleClick(Sender: TObject);
var
  r: string;
  output: string;
begin
  if modulelist=nil then
    modulelist:=tstringlist.create;

  symhandler.getModuleList(modulelist);
  output:='';
  ShowSelectionList(self, rsModuleList, rsSelectAModuleOrGiveYourOwnRange, modulelist, output, true, @ModuleSelectEvent);
  if output<>'' then
  begin
    //check that output can be parsed
    if not symhandler.parseRange(output, FilterRangeFrom, FilterRangeTo) then
    begin
      MessageDlg(output+rsIsAnInvalidRange, mtError, [mbok],0);
      exit;
    end;

  end;

  freeandnil(modulelist);
  flushResults(foNotInRange);
end;

procedure TfrmUltimap2.btnNotCalledClick(Sender: TObject);
begin
  flushResults(foNonCALLInstructions);
end;

procedure TfrmUltimap2.btnNotExecutedClick(Sender: TObject);
begin
  flushResults(foExecuted); //filters out executed memory
end;

procedure TfrmUltimap2.btnResetCountClick(Sender: TObject);
begin
  FlushResults(foResetCount);
end;

procedure TfrmUltimap2.btnCancelFilterClick(Sender: TObject);
begin
  if filterThread<>nil then
  begin
    filterThread.Terminate;
    btnCancelFilter.enabled:=false;
  end;
end;

procedure TfrmUltimap2.btnResetClick(Sender: TObject);
begin
  flushResults(foResetAll);
  cbfilterOutNewEntries.Checked:=false;
end;



procedure TfrmUltimap2.Button1Click(Sender: TObject);
begin

end;

procedure TfrmUltimap2.Button2Click(Sender: TObject);
begin

end;

procedure TfrmUltimap2.ListView1DblClick(Sender: TObject);
var entry: PValidEntry;
begin
  if (validlist<>nil) and (listview1.Selected<>nil) then
  begin
    entry:=validList[listview1.selected.Index];
    MemoryBrowser.disassemblerview.SelectedAddress:=entry^.address;
    MemoryBrowser.show;
  end;
end;

procedure TfrmUltimap2.lvThreadsDblClick(Sender: TObject);
begin

end;

procedure TfrmUltimap2.lvThreadsItemChecked(Sender: TObject; Item: TListItem);
var ti: PUltimap2ThreadInfo;
begin
  ti:=PUltimap2ThreadInfo(item.data);
  ti^.paused:=not item.Checked;
end;

procedure TfrmUltimap2.MenuItem1Click(Sender: TObject);
var
  i: integer;
  sl: Tstringlist;
  s: string;
begin
  sl:=tstringlist.create;

  for i:=0 to listview1.Items.Count-1 do
    if listview1.Items[i].Selected then
    begin
      s:=listview1.Items[i].Caption+' - '+ listview1.Items[i].SubItems[0]+' - '+ listview1.Items[i].SubItems[1];
      sl.add(s);
    end;

  Clipboard.AsText:=sl.text;
  sl.free;
end;

procedure TfrmUltimap2.MenuItem4Click(Sender: TObject);
begin
  close;
end;

procedure TfrmUltimap2.MenuItem5Click(Sender: TObject);
var
  worker: TUltimap2Worker;
  e: TUltimap2DataEvent;
begin
  if not libIptInit then raise exception.create(rsFailureLoadingLibipt);

  if OpenDialog1.execute then
  begin
    worker:=TUltimap2Worker.create(true,-1, self);
    worker.Filename:=opendialog1.filename;
    worker.fromFile:=true;
    worker.waitForData(0,e);
    worker.processData(e);
  end;
end;

procedure TfrmUltimap2.miCloseClick(Sender: TObject);
begin
  close;
end;

function TfrmUltimap2.ValidListCompare(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
begin
  result:=CompareValue(PValidEntry(Data1)^.address, PValidEntry(Data2)^.address);
end;

procedure TfrmUltimap2.FreeValidList;
var
  i: integer;
  n: TIndexedAVLTreeNode;
begin
  listview1.Items.Count:=0;
  if validList<>nil then
  begin
    for i:=0 to validList.Count-1 do
    begin
      n:=validList.GetNodeAtIndex(i);
      if (n<>nil) and (n.Data<>nil) then
      begin
        FreeMemAndNil(n.data);

      end;
    end;
    validlist.Clear;
    freeandnil(validList);
  end;
end;

function TfrmUltimap2.getMatchCount: integer;
begin
  result:=0;
  if (self<>nil) and (regiontreemrew<>nil) then
  begin
    regiontreemrew.Beginread;
    try
      if validlist<>nil then
      begin
        result:=validlist.Count;
      end;
    finally
      regiontreemrew.Endread;
    end;
  end;
end;

function TfrmUltimap2.IsMatchingAddress(address: ptruint; count: pinteger=nil): boolean;
var
  s: TValidEntry;
  r: PValidEntry;
  n: TAvgLvlTreeNode;
begin
  result:=false;
  if self<>nil then
  begin
    if regiontreemrew<>nil then
    begin
      regiontreemrew.Beginread;
      try
        if validlist<>nil then
        begin
          s.address:=address;

          n:=validlist.find(@s);
          if n<>nil then
          begin
            r:=n.Data;
            result:=(r^.byteInfo^.flags and bifInvalidated)=0;

            if result and (count<>nil) then
              count^:=r^.byteInfo^.count;
          end;
        end;
      finally
        regiontreemrew.Endread;
      end;
    end;
  end;
end;

procedure TfrmUltimap2.btnShowResultsClick(Sender: TObject);
var
  e: TAvgLvlTreeNodeEnumerator;
  entry: PValidEntry;
  ri: PRegionInfo;

  i: integer;
  n: TAvgLvlTreeNode;
begin


  //build a indexable list of all the addresses in the list
  if regiontreemrew<>nil then
  begin
    //check if a list exists, and if so, delete it

    FreeValidList;
    validlist:=TIndexedAVLTree.CreateObjectCompare(@ValidListCompare);

    regiontreemrew.Beginread;
    try
      e:=TAvgLvlTreeNodeEnumerator.Create(regiontree);
      while e.MoveNext do
      begin
        ri:=e.Current.Data;
        if ri<>nil then
        begin
          for i:=0 to ri^.size-1 do
          begin
            if (ri^.info[i].count>0) and ((ri^.info[i].flags and bifInvalidated)=0) then
            begin
              getmem(entry, sizeof(TValidEntry));
              entry^.address:=ri^.address+i;
              entry^.byteInfo:=@ri^.info[i];

              validlist.Add(entry);
            end;
          end;
        end;
      end;
    finally
      regiontreemrew.Endread;
    end;

    listview1.Items.Count:=validlist.Count;
    lblIPCount.Caption:=rsInstructionPointerListSize+inttostr(validlist.Count);
  end;
end;

procedure TfrmUltimap2.cbfilterOutNewEntriesChange(Sender: TObject);
begin
  allNewAreInvalid:=cbfilterOutNewEntries.checked;
end;

procedure TfrmUltimap2.cbParseToTextfileChange(Sender: TObject);
begin
  deTextOut.visible:=cbParseToTextfile.Checked;
  deTextOut.Update;
  deTextOut.Refresh;
  deTextOut.Repaint;

  deTextOut.ButtonOnlyWhenFocused:=true;
  deTextOut.ButtonOnlyWhenFocused:=false;
end;

procedure TfrmUltimap2.cbTraceAllProcessesChange(Sender: TObject);
begin
  if cbTraceAllProcesses.checked then
  begin
    rbLogToFolder.checked:=true;
    rbRuntimeParsing.enabled:=false;
    cbAutoProcess.checked:=false;
    cbAutoProcess.Enabled:=false;
    cbPauseTargetWhileProcessing.enabled:=false;
    cbPauseTargetWhileProcessing.checked:=false;
    cbDontDeleteTraceFiles.checked:=true;

  end
  else
  begin
    rbRuntimeParsing.enabled:=true;
    cbAutoProcess.enabled:=true;
  end;
end;

procedure TfrmUltimap2.cbTraceIntervalChange(Sender: TObject);
begin
  if cbTraceInterval.checked then
  begin
    edtFlushInterval.enabled:=true;
    edtFlushIntervalChange(sender);
  end
  else
    edtFlushInterval.Enabled:=false;

end;

procedure TfrmUltimap2.cbWhenFilesizeAboveChange(Sender: TObject);
begin
  if cbWhenFilesizeAbove.checked then
  begin
    edtMaxFilesize.Enabled:=true;
    edtMaxFilesizeChange(sender);
  end
  else
    edtMaxFilesize.enabled:=false;
end;

procedure TfrmUltimap2.cbWindowsBasedIPTChange(Sender: TObject);
var c: boolean;
begin
  c:=cbWindowsBasedIPT.checked;
  BeginFormUpdate;
  lblBufferSizePerThread.visible:=c;;
  cbWinIPTBufferSize.visible:=c;
  gbThreads.Visible:=c;
  lblBuffersPerCPU.Visible:=not c;
  edtBufSize.visible:=not c;
  lblKB.visible:=not c;
  cbTraceAllProcesses.visible:=not c;
  cbUsermode.visible:=not c;
  cbKernelmode.visible:=not c;
  cbNoInterrupts.visible:=not c;




  cbWinIPTBufferSizeDropDown(nil);



  if cbWinIPTBufferSize.items.Count>10 then
    cbWinIPTBufferSize.ItemIndex:=10
  else
    cbWinIPTBufferSize.ItemIndex:=cbWinIPTBufferSize.items.Count-1;

  EndFormUpdate;
end;

procedure TfrmUltimap2.cbWinIPTBufferSizeDropDown(Sender: TObject);
var

  tc: integer;
  maxsize: dword;

  sizelist: TStringList;
  oldii: integer;
begin
  tc:=getthreadcount(processid);

  if tc=0 then tc:=1;


  maxsize:=$ffff0000 div tc;

  sizelist:=tstringlist.create;
  if maxsize>=4096 then
    sizelist.add('4KB');

  if maxsize>=8*1024 then
    sizelist.add('8KB');

  if maxsize>=16*1024 then
    sizelist.add('16KB');

  if maxsize>=32*1024 then
    sizelist.add('32KB');

  if maxsize>=64*1024 then
    sizelist.add('64KB');

  if maxsize>=128*1024 then
    sizelist.add('128KB');

  if maxsize>=256*1024 then
    sizelist.add('256KB');

  if maxsize>=512*1024 then
    sizelist.add('512KB');

  if maxsize>=1*1024*1024 then
    sizelist.add('1MB');

  if maxsize>=2*1024*1024 then
    sizelist.add('2MB');

  if maxsize>=4*1024*1024 then
    sizelist.add('4MB');

  if maxsize>=8*1024*1024 then
    sizelist.add('8MB');

  if maxsize>=16*1024*1024 then
    sizelist.add('16MB');

  if maxsize>=32*1024*1024 then
    sizelist.add('32MB');

  if maxsize>=64*1024*1024 then
    sizelist.add('64MB');

  if maxsize>=128*1024*1024 then
    sizelist.add('128MB');

  oldii:=cbWinIPTBufferSize.ItemIndex;
  cbWinIPTBufferSize.Items.clear;
  cbWinIPTBufferSize.Items.Assign(sizelist);

  if sizelist.count>oldii then
    cbWinIPTBufferSize.ItemIndex:=oldii;
end;

procedure TfrmUltimap2.edtFlushIntervalChange(Sender: TObject);
begin
  try
    flushinterval:=strtoint(edtFlushInterval.Text);
    if FlushInterval=0 then
      FlushInterval:=1;

    edtFlushInterval.Font.Color:=clDefault;
  except
    edtFlushInterval.Font.Color:=clRed;
  end;
end;

procedure TfrmUltimap2.edtMaxFilesizeChange(Sender: TObject);
begin
  try
    maxfilesize:=strtoint(edtMaxFilesize.Text)*4096*4096;
    edtMaxFilesize.Font.Color:=clDefault;
  except
    edtMaxFilesize.Font.Color:=clRed;
  end;
end;

procedure TfrmUltimap2.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;




procedure TfrmUltimap2.miRangeDeleteSelectedClick(Sender: TObject);
var i: integer;
begin
  for i:=lbrange.Items.Count-1 downto 0 do
    if lbrange.Selected[i] then
      lbRange.Items.Delete(i);
end;

procedure TfrmUltimap2.miRangeDeleteAllClick(Sender: TObject);
begin
  lbRange.clear;
end;

procedure TfrmUltimap2.miRemoveHotkeyClick(Sender: TObject);
var
  i: integer;
begin
  if pmSetHotkey.PopupComponent<>nil then
  begin
    i:=pmSetHotkey.PopupComponent.Tag;
    if FilterHotkey[i]<>nil then
      FreeAndNil(FilterHotkey[i]);
  end;
end;

procedure TfrmUltimap2.miSetHotkeyClick(Sender: TObject);
var
  f: TfrmHotkeyEx;
  i: integer;
begin
  if pmSetHotkey.PopupComponent<>nil then
  begin
    i:=pmSetHotkey.PopupComponent.Tag;

    f:=TfrmHotkeyEx.Create(self);

    if FilterHotkey[i]<>nil then
    begin
      f.newhotkey:=filterhotkey[i].keys;
      f.edtHotkey.text:=ConvertKeyComboToString(f.newhotkey);
    end;

    if (f.showmodal = mrok) and (f.newhotkey[0]<>0) then
    begin
      if FilterHotkey[i]=nil then
        FilterHotkey[i]:=TGenericHotkey.create(TButton(pmSetHotkey.PopupComponent).OnClick, f.newhotkey)
      else
        FilterHotkey[i].keys:=f.newhotkey;
    end;

    f.free;
  end;
end;

procedure TfrmUltimap2.Panel5Click(Sender: TObject);
begin

end;

procedure TfrmUltimap2.pmRangeOptionsPopup(Sender: TObject);
begin
  miRangeDeleteSelected.enabled:=lbrange.SelCount>0;
  miRangeDeleteAll.enabled:=lbrange.count>0;
end;


procedure TfrmUltimap2.pmSetHotkeyPopup(Sender: TObject);
var i: integer;
begin
  if pmSetHotkey.PopupComponent<>nil then
  begin
    i:=pmSetHotkey.PopupComponent.Tag;

   // showmessage(pmSetHotkey.PopupComponent.Name);

    miSetHotkey.enabled:=FilterHotkey[i]=nil;
    miRemoveHotkey.enabled:=not miSetHotkey.enabled;

    if miRemoveHotkey.enabled then
      miRemoveHotkey.Caption:=Format(rsRemoveHotkey, [ConvertKeyComboToString(FilterHotkey[i].keys)]);
  end
  else
  begin
    miSetHotkey.enabled:=false;
    miRemoveHotkey.enabled:=false;
  end;
end;

procedure TfrmUltimap2.rbLogToFolderChange(Sender: TObject);
begin
  BeginFormUpdate;
  deTargetFolder.visible:=rbLogToFolder.checked;
  cbDontDeleteTraceFiles.visible:=rbLogToFolder.checked;

  cbAutoProcess.visible:=rbLogToFolder.checked;
  cbTraceInterval.visible:=rbLogToFolder.checked;
  edtFlushInterval.visible:=rbLogToFolder.checked;
  Label4.visible:=rbLogToFolder.checked;
  cbWhenFilesizeAbove.visible:=rbLogToFolder.checked;
  cbDontDeleteTraceFiles.visible:=rbLogToFolder.checked;
  EndFormUpdate;
end;

procedure TfrmUltimap2.tActivatorTimer(Sender: TObject);
var
  done: boolean;
  i: integer;
  totalprocessed, totalsize: qword;
  mi: TMapIterator;
  ti: PUltimap2ThreadInfo;
begin
  done:=true;
  totalprocessed:=0;
  totalsize:=0;
  if cbWindowsBasedIPT.checked then
  begin
    iptdatadispatcher.threadlistMREW.Beginread;
    mi:=TMapIterator.Create(iptdatadispatcher.threadlist);
    mi.first;
    while not mi.eom do
    begin
      mi.GetData(ti);
      totalprocessed:=totalprocessed+ti^.worker.processed;
      totalsize:=totalsize+ti^.worker.totalsize;
      if not ti^.worker.done then
        done:=false;

      mi.next;
    end;
    iptdatadispatcher.threadlistMREW.endread;
  end
  else
  begin
    for i:=0 to length(workers)-1 do
    begin
      if not workers[i].done then
        done:=false;

      if workers[i].totalsize<>0 then
      begin
        totalprocessed:=totalprocessed+workers[i].processed;
        totalsize:=totalsize+workers[i].totalsize;
      end
      else
        totalsize:=totalsize*2;
    end;

  end;

  if not done then
  begin
    if totalsize>0 then
      label1.Caption:=rsProcessingData+#13#10+format('%.2f', [(totalprocessed / totalsize) * 100])+'%'
    else
      label1.Caption:=rsProcessingData+#13#10'0%';

    exit;
  end;

  tActivator.enabled:=false;

  lvThreads.Columns[3].Visible:=false;

  if cbPauseTargetWhileProcessing.checked then
  begin
    advancedoptions.Pausebutton.down := false;
    advancedoptions.Pausebutton.Click;
  end;


  btnShowResults.Enabled:=true;
  btnRecordPause.enabled:=true;

  if PostProcessingFilter<>foNone then
  begin
    Filter(PostProcessingFilter);
    state:=rsRecording;
  end
  else
    state:=rsStopped;
end;

//lua

function frmUltimap2_isMatchingAddress(L: PLua_state): integer; cdecl;
var
  f: TfrmUltimap2;
  r: boolean;
  count: integer;
begin
  result:=0;
  f:=TfrmUltimap2(luaclass_getClassObject(L));
  if lua_gettop(L)>=1 then
  begin
    r:=f.IsMatchingAddress(lua_tointeger(L,1), @count);
    lua_pushboolean(L,r);

    if r then
    begin
      lua_pushinteger(L,count);
      result:=2;
    end
    else
      result:=1;
  end;
end;

function lua_getUltimap2(L: PLua_state): integer; cdecl;
begin
  luaclass_newClass(L,frmUltimap2);
  result:=1;
end;

procedure frmUltimap2_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  customform_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'isMatchingAddress', @frmUltimap2_isMatchingAddress);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'isInList', @frmUltimap2_isMatchingAddress);

end;

procedure initializeLuaUltimap2;
begin
  lua_register(LuaVM, 'getUltimap2', @lua_getUltimap2);
end;


initialization
  registerclass(TfrmUltimap2);

  luaclass_register(TfrmUltimap2, @frmUltimap2_addMetaData);

end.

