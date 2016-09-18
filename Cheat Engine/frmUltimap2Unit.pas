unit frmUltimap2Unit;

{$mode objfpc}{$H+}



interface

uses
  win32proc,  windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, EditBtn, Menus, libipt, ProcessHandlerUnit,
  DBK32functions, commonTypeDefs, MemFuncs, AvgLvlTree, Math, FileMapping,
  syncobjs, CEFuncProc, registry, NewKernelHandler, LazFileUtils, disassembler,
  strutils, Clipbrd;


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

  TUltimap2Worker=class(TThread) //many
  private
    lastRegion: PRegionInfo;
    filecount: integer; //number of tracefiles saved

    filemap: TFileMapping;

    disassembler: Tdisassembler;
    function addIPPageToRegionTree(IP: QWORD): PRegionInfo;
    function addIPBlockToRegionTree(IP: QWORD): PRegionInfo;
    procedure HandleIP(ip: QWORD; c: pt_insn_class);
    procedure HandleIPForRegion(ip: qword; c: pt_insn_class; region: PRegionInfo);

    function waitForData(timeout: dword; var e: TUltimap2DataEvent): boolean;
    procedure continueFromData(e: TUltimap2DataEvent);

    procedure parseToStringlist(insn: pt_insn; output: Tstrings);
  public
    id: integer;
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


    procedure execute; override;

    constructor create(CreateSuspended: boolean);
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
    Button5: TButton;
    btnReset: TButton;
    cbFilterFuturePaths: TCheckBox;
    cbfilterOutNewEntries: TCheckBox;
    cbDontDeleteTraceFiles: TCheckBox;
    cbParseToTextfile: TCheckBox;
    deTargetFolder: TDirectoryEdit;
    deTextOut: TDirectoryEdit;
    edtBufSize: TEdit;
    edtCallCount: TEdit;
    gbRange: TGroupBox;
    Label1: TLabel;
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
    rbLogToFolder: TRadioButton;
    rbRuntimeParsing: TRadioButton;
    tActivator: TTimer;
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
    procedure Button5Click(Sender: TObject);
    procedure cbfilterOutNewEntriesChange(Sender: TObject);
    procedure cbParseToTextfileChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure ListView1DblClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure miCloseClick(Sender: TObject);
    procedure miRangeDeleteSelectedClick(Sender: TObject);
    procedure miRangeDeleteAllClick(Sender: TObject);
    procedure Panel5Click(Sender: TObject);
    procedure pmRangeOptionsPopup(Sender: TObject);
    procedure rbLogToFolderChange(Sender: TObject);
    procedure tActivatorTimer(Sender: TObject);
    procedure tbRecordPauseChange(Sender: TObject);
  private
    { private declarations }
    l: tstringlist;
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
    procedure ExecuteFilter(Sender: TObject);

    procedure setState(state: TRecordState);
    function ModuleSelectEvent(index: integer; listText: string): string;
    property state:TRecordState read fstate write setState;
  public
    { public declarations }
    allNewAreInvalid: boolean;

    function IsMatchingAddress(address: ptruint): boolean;
  end;

var
  frmUltimap2: TfrmUltimap2;

implementation

{$R *.lfm}

uses symbolhandler, frmSelectionlistunit, cpuidUnit, MemoryBrowserFormUnit;

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

//worker



function iptReadMemory(buffer: PByteArray; size: SIZE_T; asid: PPT_ASID; ip: uint64; context: pointer): integer; cdecl;
var self: TUltimap2Worker;
  n: TAvgLvlTreeNode;
  e: TRegionInfo;

  s: integer;
begin
  self:=TUltimap2Worker(context);
  //watch for page boundaries

  if (self.lastRegion=nil) or (ip<self.lastRegion^.address) or (ip>=(self.lastRegion^.address+self.lastRegion^.size)) then
  begin
    e.address:=ip;
    self.ownerForm.regiontreeMREW.Beginread;
    n:=self.ownerForm.regiontree.Find(@e);
    self.ownerForm.regiontreeMREW.endRead;


    if n<>nil then
      self.lastRegion:=PRegionInfo(n.Data)
    else
    begin
      //self.lastRegion:=nil;
      self.lastregion:=self.addIPBlockToRegionTree(ip);
      if self.lastregion=nil then
        exit(-integer(pte_nomap));
    end;
  end;

  if self.lastRegion<>nil then
  begin
    s:=(self.lastRegion^.address+self.lastRegion^.size)-ip;
    if s>size then s:=size;
    CopyMemory(buffer, @self.lastRegion^.memory[ip-self.lastRegion^.address], s);

    size:=size-s;
    if size>0 then
    begin
      ip:=ip+s;
      s:=s+iptReadMemory(@buffer^[s], size, asid, ip, context);
    end
    else
      result:=s;
  end;


end;

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
    freemem(page);
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



procedure TUltimap2Worker.HandleIP(ip: QWORD; c: pt_insn_class);
var
  e: TRegionInfo;
  n: TAvgLvlTreeNode;
begin
  if (lastRegion<>nil) and ((ip>=lastRegion^.address) and (ip<lastRegion^.address+lastRegion^.size)) then
  begin
    HandleIPForRegion(ip,c, lastRegion);
    exit;
  end;

  lastregion:=nil;


  e.address:=ip;
  ownerform.regiontreeMREW.Beginread;
  n:=ownerform.regiontree.Find(@e);
  if n<>nil then
    lastRegion:=n.data;

  ownerform.regiontreeMREW.Endread;

  if lastregion=nil then
    lastregion:=addIPBlockToRegionTree(ip);

  if lastRegion<>nil then
    HandleIPForRegion(ip, c, lastRegion);
end;

function TUltimap2Worker.waitForData(timeout: dword; var e: TUltimap2DataEvent): boolean;
begin
  result:=false;
  if fromfile then
  begin
    //wait for the fileready event
    if processFile.WaitFor(timeout)=wrSignaled then
    begin
      ultimap2_lockfile(id);
      if fileexists(filename) then
      begin
        if fileexists(filename+'.processing') then   //'shouldn't' happen
          deletefile(filename+'.processing');

        renamefile(filename, filename+'.processing');
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
end;

procedure TUltimap2Worker.continueFromData(e: TUltimap2DataEvent);
var fn: string;
begin
  if fromfile then
  begin
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
    outputdebugstring('Calling ultimap2_continue for cpu '+inttostr(e.cpunr));
    ultimap2_continue(e.Cpunr);
  end;
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

procedure TUltimap2Worker.execute;
var
  e: TUltimap2DataEvent;

  iptConfig: pt_config;
  decoder: ppt_insn_decoder;
  callbackImage: PPT_Image;
  insn: pt_insn;
  i: integer;

  tf: TFileStream;
  ts: TStringList;
begin
  callbackImage:=pt_image_alloc('xxx');
  pt_image_set_callback(callbackImage,@iptReadMemory,self);

  pt_config_init(@iptConfig);
  pt_cpu_read(@iptConfig.cpu);
  pt_cpu_errata(@iptConfig.errata, @iptConfig.cpu);

  tf:=nil;

  if parseAsText then
  begin
    ts:=TStringList.Create;
    disassembler:=TDisassembler.Create;
    disassembler.showmodules:=true;
    disassembler.showsymbols:=true;
    disassembler.dataOnly:=true;
  end
  else
    ts:=nil;



  while not terminated do
  begin

    if waitForData(250, e) then
    begin
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
              try
                if FileExists(textFolder+'cpu'+inttostr(e.Cpunr)+'trace.txt') then
                  tf:=TFileStream.Create(textFolder+'cpu'+inttostr(e.Cpunr)+'trace.txt', fmOpenReadWrite or fmShareDenyNone)
                else
                  tf:=TFileStream.Create(textFolder+'cpu'+inttostr(e.Cpunr)+'trace.txt', fmCreate or fmShareDenyNone)
              except
                OutputDebugString('failed creating or opening '+textFolder+'cpu'+inttostr(e.Cpunr)+'trace.txt');
                tf:=nil
              end
            end;

            //scan through this decoder

            i:=0;
            while (pt_insn_sync_forward(decoder)>=0) and (not terminated) do
            begin
              while (pt_insn_next(decoder, @insn, sizeof(insn))>=0) and (not terminated) do
              begin
                if parseAsText then
                  parseToStringlist(insn, ts);

                if insn.iclass=ptic_error then break;


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
    end else sleep(1);
  end;

  pt_image_free(callbackImage);

  if ts<>nil then
    freeandnil(ts);

  if disassembler<>nil then
    freeandnil(disassembler);
end;

destructor TUltimap2Worker.destroy;
begin
  Terminate;
  if processFile<>nil then
    processFile.SetEvent;

  waitfor;
  freeandnil(processFile);
  inherited destroy;
end;

constructor TUltimap2Worker.create(CreateSuspended: boolean);
begin
  inherited create(createsuspended);

  processFile:=TEvent.Create(nil,false,false,'');
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
        w[i]:=1; //invalidate
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
        w[i]:=1 //invalidate
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
        w[i]:=1 //invalidate
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
        w[i]:=1 //invalidate
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
  done:=true;

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
    end;
  end;
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
  freeOnTerminate:=true;


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
  finally
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
    freemem(workqueue);

  end;

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
      freemem(r^.info);
      r^.info:=nil;
    end;

    if r^.memory<>nil then
    begin
      freemem(r^.memory);
      r^.memory:=nil;
    end;

    freemem(r);
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
  lblBuffersPerCPU.enabled:=state;
  edtBufSize.enabled:=state;
  lblKB.enabled:=state;
  rbLogToFolder.enabled:=false;

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
  ultimap2_flush;


  if rbLogToFolder.checked and (state=rsRecording) then
  begin
    //signal the worker threads to process the files first
    for i:=0 to length(workers)-1 do
    begin
      workers[i].totalsize:=0;
      workers[i].done:=false;
      workers[i].processFile.SetEvent;
    end;

    btnRecordPause.enabled:=false;
    tActivator.enabled:=true;
    //when the worker threads are all done, this will become enabled

    PostProcessingFilter:=f;
    state:=rsProcessing;
    if f<>foNone then
      FilterGUI(false);
  end
  else
  begin

    //flush only returns after all data has been handled, or the data has already been handled by the file workers
    if f<>foNone then
      Filter(f);
  end;
end;

procedure TfrmUltimap2.ExecuteFilter(Sender: TObject);
begin

end;

procedure TfrmUltimap2.setState(state: TRecordState);
begin
  fstate:=state;
  case state of
    rsRecording:
    begin
      label1.Caption:=rsRecording2;
      panel1.color:=clRed;
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
end;

procedure TfrmUltimap2.cleanup;
var i: integer;
begin
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

  ultimap2_disable;
  ultimap2Initialized:=0;
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
begin
  if state=rsProcessing then exit;



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

      r:=CPUID(0);
      if (r.ebx<>1970169159) or (r.ecx<>1818588270) or (r.edx<>1231384169) then
        raise exception.create(rsOnlyForIntelCPUs);

      if (CPUID(7,0).ebx shr 25) and 1=0 then
        raise exception.create(rsSorryButYourCPUSeemsToBeLeackingIPTFeature);

      cpuid14_0:=CPUID($14,0);
      if ((cpuid14_0.ecx shr 1) and 1)=0 then
        raise exception.create(rsSorryButYourCPUsImplementationOfTheIPTFeatureIsTooOld);

      if (cpuid14_0.ebx and 1)=0 then
        raise exception.create(rsSorryButYourCPUDoesntSeemToBeAbleToSetATargetProcess);



      if processid=0 then
        raise exception.create(rsFirstOpenAProcess);

      if processid=GetCurrentProcessId then
        raise exception.create(rsTargetADifferentProcess);

      //initial checks are OK
      bsize:=strtoint(edtBufSize.text)*1024;
      if bsize<12*1024 then
        raise exception.create(rsTheSizeHasToBe12KbOrHigher);

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

      if rbLogToFolder.Checked then
      begin
        if not DirectoryExistsUTF8(deTargetFolder.Directory) then
        begin
          if ForceDirectoriesUTF8(deTargetFolder.Directory)=false then
            raise exception.create(deTargetFolder.Directory+rsDoesntExistAndCantBeCreated);
        end;
      end;

      //still here so everything seems alright.
      //turn off the config GUI
      disableConfigGUI;

      ultimap2Initialized:=processid;

      regiontree:=TAvgLvlTree.CreateObjectCompare(@RegionCompare);
      regiontreeMREW:=TMultiReadExclusiveWriteSynchronizer.Create;

      //launch worker threads
      setlength(workers, CPUCount);
      for i:=0 to length(workers)-1 do
      begin
        workers[i]:=TUltimap2Worker.Create(true);
        workers[i].id:=i;
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

        workers[i].ownerForm:=self;
      end;



      if length(ranges)>0 then
      begin
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
        getexecutablememoryregionsfromregion(0, qword($ffffffffffffffff), regions);
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


      if not libIptInit then raise exception.create(rsFailureLoadingLibipt);
      DBK32Initialize;

      if rbLogToFolder.Checked then
        ultimap2(processid, bsize, deTargetFolder.Directory, ranges)
      else
        ultimap2(processid, bsize, '', ranges);

      FilterGUI(true);

      for i:=0 to length(workers)-1 do
        workers[i].start;

      state:=rsRecording;
    end
    else
    begin
      //toggle between active/disabled
      if state=rsStopped then
      begin
        ultimap2_resume;
        state:=rsRecording;
      end
      else
      if state=rsRecording then
      begin
        ultimap2_pause;
        FlushResults(foNone);


        if rbRuntimeParsing.checked then
          state:=rsStopped;
      end;
    end;

end;

procedure TfrmUltimap2.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  canclose:=MessageDlg(rsClosingWillFreeAllCollectedData, mtConfirmation,[mbyes,mbno], 0, mbno)=mryes;
end;


procedure TfrmUltimap2.FormDestroy(Sender: TObject);
var
  x: TWindowPosArray;
  reg: tregistry;
begin
  setlength(x,0);
  SaveFormPosition(self, x);

  reg:=TRegistry.Create;
  try
    if Reg.OpenKey('\Software\Cheat Engine',false) then
    begin
      Reg.WriteString('Ultimap2 Folder', deTargetFolder.Directory);
      Reg.WriteBool('Ultimap2 Keep Trace Files', cbDontDeleteTraceFiles.checked);
      Reg.WriteBool('Ultimap2 Use Disk', rbLogToFolder.Checked);

      Reg.WriteBool('Ultimap2 Parse Trace As Text', cbParseToTextfile.checked);
      Reg.WriteString('Ultimap2 TextTrace Folder', deTextOut.Directory);
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
  if WindowsVersion>=wvVista then
  begin
    i:=sendmessage(edtBufSize.Handle, EM_GETMARGINS, 0,0);
    i:=(i shr 16)+(i and $ffff);
  end
  else
    i:=8;

  minwidth:=i+canvas.GetTextWidth(edtBufSize.Text);
  if edtBufSize.ClientWidth<minwidth then
    edtBufSize.ClientWidth:=minwidth;

  if label1.Width+4>panel1.Height then
  begin
    panel1.Width:=label1.width+4;
    panel1.Height:=panel1.Width;
  end;

  btnReset.Height:=canvas.TextHeight(btnReset.caption)+3;
end;



procedure TfrmUltimap2.ListView1Data(Sender: TObject; Item: TListItem);
var data: PValidEntry;
begin
  if validlist<>nil then
  begin
    data:=validlist[item.index];
    item.caption:=inttohex(data^.address,8);

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
  end;


  state:=rsStopped;
  LoadFormPosition(self, x);
  if LoadFormPosition(self, x) then
    AutoSize:=false;

  reg:=TRegistry.Create;
  try
    if Reg.OpenKey('\Software\Cheat Engine',false) then
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

      if Reg.ValueExists('Ultimap2 Parse Trace As Text') then
        cbParseToTextfile.checked:=reg.ReadBool('Ultimap2 Parse Trace As Text');

      if Reg.ValueExists('Ultimap2 TextTrace Folder') then
        deTextOut.Directory:=reg.ReadString('Ultimap2 TextTrace Folder');


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
  if (index<>-1) and (l<>nil) then
  begin
    address:=ptruint(l.Objects[index]);
    if symhandler.getmodulebyaddress(address, mi) then
      exit(inttohex(mi.baseaddress,8)+'-'+inttohex(mi.baseaddress+mi.basesize,8));
  end;

  result:=listText+rsDashError;
end;



procedure TfrmUltimap2.btnAddRangeClick(Sender: TObject);
var
  r: string;
  output: string;
  start, stop: uint64;
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

  if l=nil then
    l:=tstringlist.create;

  symhandler.getModuleList(l);

  ShowSelectionList(self, rsModuleList, rsSelectAModuleOrGiveYourOwnRange+#13#10+rsPutBetweenToMarsAsAnAutoStopRange, l, output, true, @ModuleSelectEvent);
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

  freeandnil(l);
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
  if l=nil then
    l:=tstringlist.create;

  symhandler.getModuleList(l);
  output:='';
  ShowSelectionList(self, rsModuleList, rsSelectAModuleOrGiveYourOwnRange, l, output, true, @ModuleSelectEvent);
  if output<>'' then
  begin
    //check that output can be parsed
    if not symhandler.parseRange(output, FilterRangeFrom, FilterRangeTo) then
    begin
      MessageDlg(output+rsIsAnInvalidRange, mtError, [mbok],0);
      exit;
    end;

  end;

  freeandnil(l);
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
        freemem(n.data);
        n.data:=nil
      end;
    end;
    validlist.Clear;
    freeandnil(validList);
  end;
end;

function TfrmUltimap2.IsMatchingAddress(address: ptruint): boolean;
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
          end;
        end;
      finally
        regiontreemrew.Endread;
      end;
    end;
  end;
end;

procedure TfrmUltimap2.Button5Click(Sender: TObject);
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
            if (ri^.info[i].count>0) and ((ri^.info[i].flags or bifInvalidated)<>0) then
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

procedure TfrmUltimap2.Panel5Click(Sender: TObject);
begin

end;

procedure TfrmUltimap2.pmRangeOptionsPopup(Sender: TObject);
begin
  miRangeDeleteSelected.enabled:=lbrange.SelCount>0;
  miRangeDeleteAll.enabled:=lbrange.count>0;
end;

procedure TfrmUltimap2.rbLogToFolderChange(Sender: TObject);
begin
  if rbLogToFolder.enabled then
  begin
    deTargetFolder.visible:=rbLogToFolder.checked;
    cbDontDeleteTraceFiles.visible:=rbLogToFolder.checked;
  end;
end;

procedure TfrmUltimap2.tActivatorTimer(Sender: TObject);
var
  done: boolean;
  i: integer;
  totalprocessed, totalsize: qword;
begin
  done:=true;
  totalprocessed:=0;
  totalsize:=0;
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

  if not done then
  begin
    if totalsize>0 then
      label1.Caption:=rsProcessingData+#13#10+format('%.2f', [(totalprocessed / totalsize) * 100])+'%'
    else
      label1.Caption:=rsProcessingData+#13#10'0%';

    exit;
  end;

  btnRecordPause.enabled:=true;
  tActivator.Enabled:=false;


  if PostProcessingFilter<>foNone then
  begin
    Filter(PostProcessingFilter);
    state:=rsRecording;
  end
  else
    state:=rsStopped;
end;



end.

