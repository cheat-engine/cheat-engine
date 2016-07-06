unit frmUltimap2Unit;

{$mode objfpc}{$H+}



interface

uses
  windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, EditBtn, Menus, libipt, ProcessHandlerUnit,
  DBK32functions, commonTypeDefs, MemFuncs, AvgLvlTree, Math, FileMapping,
  syncobjs;


const
  bifInvalidated=1;
  bifExecuted=2;

type
  TByteInfo=packed record
    count: byte;
    flags: byte;
  end;
  PByteInfo=^TByteInfo;

  TRegionInfo=record
    address: ptruint;
    memory: PByte;
    size: integer;

    info: PByteInfo;

  end;
  PRegionInfo=^TRegionInfo;

  TfrmUltimap2=class;

  TUltimap2Worker=class(TThread)
  private
    lastRegion: PRegionInfo;

    filemap: TFileMapping;
    function addIPPageToRegionTree(IP: QWORD): PRegionInfo;
    function addIPBlockToRegionTree(IP: QWORD): PRegionInfo;
    procedure HandleIP(ip: QWORD);
    procedure HandleIPForRegion(ip: qword; region: PRegionInfo);

    function waitForData(timeout: dword; e: TUltimap2DataEvent): boolean;
    procedure continueFromData(e: TUltimap2DataEvent);
  public
    id: integer;
    filename: string;
    fromFile: boolean;
    processFile: TEvent;
    done: boolean;
    ownerForm: TfrmUltimap2;

    procedure execute; override;

    constructor create(CreateSuspended: boolean);
    destructor destroy; override;
  end;


  { TfrmUltimap2 }

  TfrmUltimap2 = class(TForm)
    btnExecuted: TButton;
    btnFilterCallCount: TButton;
    btnFilterModule: TButton;
    btnNotCalled: TButton;
    btnNotExecuted: TButton;
    btnResetCount: TButton;
    btnAddRange: TButton;
    Button1: TButton;
    Button5: TButton;
    Button6: TButton;
    cbFilterFuturePaths: TCheckBox;
    cbfilterOutNewEntries: TCheckBox;
    gbRange: TGroupBox;
    lbRange: TListBox;
    miRangeDeleteSelected: TMenuItem;
    miRangeDeleteAll: TMenuItem;
    pmRangeOptions: TPopupMenu;
    rbLogToFolder: TRadioButton;
    rbRuntimeParsing: TRadioButton;
    deTargetFolder: TDirectoryEdit;
    edtCallCount: TEdit;
    edtBufSize: TEdit;
    lblBuffersPerCPU: TLabel;
    Label3: TLabel;
    lblKB: TLabel;
    lblIPCount: TLabel;
    lblLastfilterresult: TLabel;
    ListView1: TListView;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel5: TPanel;
    tbRecordPause: TToggleBox;
    tActivator: TTimer;
    procedure btnAddRangeClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDestroy(Sender: TObject);
    procedure miRangeDeleteSelectedClick(Sender: TObject);
    procedure miRangeDeleteAllClick(Sender: TObject);
    procedure tActivatorTimer(Sender: TObject);
    procedure tbRecordPauseChange(Sender: TObject);
  private
    { private declarations }
    l: tstringlist;
    ultimap2Initialized: dword;

    regiontree: TAvgLvlTree;
    regiontreeMREW: TMultiReadExclusiveWriteSynchronizer;

    workers: array of TUltimap2Worker;

    function RegionCompare(Tree: TAvgLvlTree; Data1, Data2: pointer): integer;

    procedure freeRegion(r: PRegionInfo);
    procedure cleanup;
    procedure setConfigGUIState(state: boolean);
    procedure enableConfigGUI;
    procedure disableConfigGUI;
    function ModuleSelectEvent(index: integer; listText: string): string;
  public
    { public declarations }
  end;

var
  frmUltimap2: TfrmUltimap2;

implementation

{$R *.lfm}

uses symbolhandler, frmSelectionlistunit, cpuidUnit;

//worker




function iptReadMemory(buffer: PByteArray; size: SIZE_T; asid: PPT_ASID; ip: uint64; context: pointer): integer; cdecl;
var self: TUltimap2Worker;
  lastRegion: PRegionInfo;
  n: TAvgLvlTreeNode;
  e: TRegionInfo;

  s: integer;
begin
  self:=TUltimap2Worker(context);
  //watch for page boundaries

  lastRegion:=self.lastRegion;
  if (lastRegion=nil) or (ip<lastRegion^.address) or (ip>(lastRegion^.address+lastRegion^.size)) then
  begin
    e.address:=ip;
    self.ownerForm.regiontreeMREW.Beginread;
    n:=self.ownerForm.regiontree.Find(@e);
    self.ownerForm.regiontreeMREW.endRead;

    if n<>nil then
      lastRegion:=PRegionInfo(n.Data)
    else
    begin
      lastregion:=self.addIPBlockToRegionTree(ip);
      if lastregion=nil then
        exit(-integer(pte_nomap));
    end;
  end;

  if lastregion<>nil then
  begin
    s:=(lastRegion^.address+lastRegion^.size)-ip;
    if s>size then s:=size;
    CopyMemory(buffer, @lastRegion^.memory[ip-lastRegion^.address], s);

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
    zeromemory(p^.info, 4096);

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
        ReadProcessMemory(processhandle, pointer(baseaddress),p^.memory, endaddress-baseaddress, br);
        if br<endaddress-baseaddress then
        begin
          p^.size:=br; //fix size

          if (br=0) or (ip>(baseaddress+br)) then //failure. Try a single page
          begin
            ownerForm.freeRegion(p);
            exit(addIPPageToRegionTree(IP));
          end;

          ownerForm.regiontree.Add(p);
        end;

      end
      else
        exit;
    end;

  finally
    ownerform.regiontreeMREW.Endwrite;
  end;
end;


procedure TUltimap2Worker.HandleIPForRegion(ip: qword; region: PRegionInfo);
var index: integer;
begin
  //do something with this IP
  index:=ip-region^.address;
  if region^.info[index].count<255 then
    inc(region^.info[index].count);

  region^.info[index].flags:=region^.info[index].flags or bifExecuted;
end;



procedure TUltimap2Worker.HandleIP(ip: QWORD);
var
  e: TRegionInfo;
  n: TAvgLvlTreeNode;
begin
  if (lastRegion<>nil) and ((ip>=lastRegion^.address) and (ip<lastRegion^.address+lastRegion^.size)) then
  begin
    HandleIPForRegion(ip,lastRegion);
    exit;
  end;


  e.address:=ip;
  ownerform.regiontreeMREW.Beginread;
  n:=ownerform.regiontree.Find(@e);
  if n<>nil then
    lastRegion:=n.data;
  ownerform.regiontreeMREW.Endread;

  if lastRegion<>nil then
  begin
    HandleIPForRegion(ip,lastRegion);
    exit;
  end;

  //still here
  lastregion:=addIPBlockToRegionTree(ip);
  if lastregion=nil then exit; //fuck it
end;

function TUltimap2Worker.waitForData(timeout: dword; e: TUltimap2DataEvent): boolean;
begin
  result:=false;
  if fromfile then
  begin
    //wait for the fileready event
    if processFile.WaitFor(timeout)=wrSignaled then
    begin
      ultimap2_lockfile(id);
      filemap:=TFileMapping.create(filename);

      e.Address:=ptruint(filemap.fileContent);
      e.Size:=filemap.filesize;
      result:=true;
    end
  end
  else
    result:=ultimap2_waitForData(timeout, e);
end;

procedure TUltimap2Worker.continueFromData(e: TUltimap2DataEvent);
begin
  if fromfile then
  begin
    if filemap<>nil then
      freeandnil(filemap);

    ultimap2_releasefile(id);

    done:=true;
  end
  else
    ultimap2_continue(e.Cpunr);
end;

procedure TUltimap2Worker.execute;
var
  e: TUltimap2DataEvent;

  iptConfig: pt_config;
  decoder: ppt_insn_decoder;
  callbackImage: PPT_Image;
  insn: pt_insn;
begin
  callbackImage:=pt_image_alloc('');
  pt_image_set_callback(callbackImage,@iptReadMemory,self);

  pt_config_init(@iptConfig);
  pt_cpu_read(@iptConfig.cpu);
  pt_cpu_errata(@iptConfig.errata, @iptConfig.cpu);

  while not terminated do
  begin

    if waitForData(250, e) then
    begin
      try
        //process the data between e.Address and e.Address+e.Size
        iptConfig.beginaddress:=pointer(e.Address);
        iptConfig.endaddress:=pointer(e.Address+e.Size);

        decoder:=pt_insn_alloc_decoder(@iptConfig);
        if decoder<>nil then
        begin
          try
            pt_insn_set_image(decoder, callbackImage);

            //scan through this decoder
            while pt_insn_sync_forward(decoder)>0 do
            begin
              while pt_insn_next(decoder, @insn, sizeof(insn))>0 do
                handleIP(insn.ip);
            end;
          finally
            pt_insn_free_decoder(decoder);
          end;
        end;

      finally
        continueFromData(e);
      end;
    end else sleep(1);
  end;

  pt_image_free(callbackImage);
end;

destructor TUltimap2Worker.destroy;
begin
  Terminate;
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

{ TfrmUltimap2 }

//RegionCompare

function TfrmUltimap2.RegionCompare(Tree: TAvgLvlTree; Data1, Data2: pointer): integer;
var d1,d2: PRegionInfo;
begin
  d1:=data1;
  d2:=data2;

  if (d2^.address>=d1^.address) and (d2^.address<d1^.address) then
    result:=0
  else
    result:=CompareValue(d1^.address, d2^.address); //not inside
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

procedure TfrmUltimap2.setConfigGUIState(state: boolean);
begin
  lblBuffersPerCPU.enabled:=state;
  edtBufSize.enabled:=state;
  lblKB.enabled:=state;
  rbLogToFolder.enabled:=state;
  deTargetFolder.enabled:=state;
  rbRuntimeParsing.enabled:=state;

  gbRange.enabled:=state;
  lbRange.enabled:=state;
  btnAddRange.enabled:=state;
end;

procedure TfrmUltimap2.enableConfigGUI;
begin
  setConfigGUIState(true);
end;

procedure TfrmUltimap2.disableConfigGUI;
begin
  setConfigGUIState(false);
end;


procedure TfrmUltimap2.cleanup;
var i: integer;
begin
  //cleanup everything
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


  for i:=0 to length(workers)-1 do
    workers[i].Terminate;

  for i:=0 to length(workers)-1 do
  begin
    workers[i].Free;
    workers[i]:=nil;
  end;
  setlength(workers,0);


  enableConfigGUI;

  ultimap_disable;
  ultimap2Initialized:=0;
end;

procedure TfrmUltimap2.tbRecordPauseChange(Sender: TObject);
var
  size: dword;
  ranges: TPRangeDynArray;
  r: TCPUIDResult;
  i: integer;

  regions: TMemoryRegions;

  p: PRegionInfo;
  br: ptruint;
begin
  if tbRecordPause.enabled=false then
  begin
    tbRecordPause.OnChange:=nil;
    tbRecordPause.checked:=not tbRecordPause.checked;
    tbRecordPause.OnChange:=@tbRecordPauseChange;
    exit;
  end;

  try


    if ((ultimap2Initialized=0) or (processid<>ultimap2Initialized)) and tbRecordPause.Checked then
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
        raise exception.create('Sorry, but Ultimap2 only works on Intel CPU''s');

      if (CPUID(7,0).ebx shr 25) and 1=0 then
        raise exception.create('Sorry, but your CPU seems to be lacking the Intel Processor Trace feature which Ultimap2 makes use of');

      if ((CPUID($14,0).ecx shr 1) and 1)=0 then
        raise exception.create('Sorry, but your CPU''s implementation of the Intel Processor Trace feature is too old. Ultimap uses multiple ToPA entries');

      if processid=0 then
        raise exception.create('First open a process');

      if processid=GetCurrentProcessId then
        raise exception.create('Target a different process. Ultimap2 will suspend the target when the buffer is full, and suspending the thing that empties the buffer is not a good idea');

      //initial checks are OK
      size:=strtoint(edtBufSize.text)*1024;
      if size<12*1024 then
        raise exception.create('The size has to be 12KB or higher');

      setlength(ranges,lbrange.count);
      for i:=0 to lbRange.Count-1 do
        if symhandler.ParseRange(lbRange.Items[i], ranges[i].startAddress, ranges[i].endaddress)=false then
          raise exception.create('For some weird reason "'+lbRange.Items[i]+'" can''t be parsed');

      //still here so everything seems alright.
      //turn off the config GUI
      disableConfigGUI;

      ultimap2Initialized:=processid;

      regiontree:=TAvgLvlTree.CreateObjectCompare(@RegionCompare);
      regiontreeMREW:=TMultiReadExclusiveWriteSynchronizer.Create;

      //launch worker threads
      setlength(workers, 1); //CPUCount);
      for i:=0 to length(workers)-1 do
      begin
        workers[i]:=TUltimap2Worker.Create(true);
        workers[i].fromFile:=rbLogToFolder.Checked;
        workers[i].Filename:=deTargetFolder.Directory;
        if workers[i].Filename<>'' then
        begin
          if workers[i].Filename[length(workers[i].Filename)]<>PathDelim then
            workers[i].Filename:=workers[i].Filename+PathDelim;

          workers[i].Filename:=workers[i].Filename+'CPU'+inttostr(i)+'.trace';
        end;
        workers[i].ownerForm:=self;
      end;

      if length(ranges)=0 then
      begin
        for i:=0 to length(ranges)-1 do
        begin
          getmem(p, sizeof(TRegionInfo));
          p^.size:=ranges[i].endaddress-ranges[i].startAddress;
          getmem(p^.memory, p^.size);
          getmem(p^.info, size*sizeof(TByteInfo));
          ReadProcessMemory(processhandle, pointer(ranges[i].startAddress), p^.memory, p^.size, br);
          if br=0 then
            freeRegion(p)
          else
          begin
            zeromemory(p^.info, size*sizeof(TByteInfo));
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
          p^.size:=regions[i].MemorySize;
          getmem(p^.memory, p^.size);
          getmem(p^.info, p^.size*sizeof(TByteInfo));
          ReadProcessMemory(processhandle, pointer(regions[i].BaseAddress), p^.memory, p^.size, br);
          if br=0 then
            freeRegion(p)
          else
          begin
            zeromemory(p^.info, size*sizeof(TByteInfo));
            regiontree.Add(p);
          end;
        end;
      end;


      //start the recording
      //ultimap2(processid,


      for i:=0 to length(workers)-1 do
        workers[i].start;
    end
    else
    begin
      //toggle between active/disabled
      if tbRecordPause.Checked then
        ultimap2_resume
      else
      begin
        ultimap2_pause;
        if rbLogToFolder.checked then
        begin
          //signal the worker threads to start processing
          for i:=0 to length(workers)-1 do
          begin
            workers[i].done:=false;
            workers[i].processFile.SetEvent;
          end;

          tbRecordPause.enabled:=false;
          tActivator.enabled:=true;
          //when the worker threads are all done, this will become enabled
        end;
      end;
    end;

  except
    tbRecordPause.OnChange:=nil;
    tbRecordPause.checked:=false;
    tbRecordPause.OnChange:=@tbRecordPauseChange;

    raise;
  end;
end;

procedure TfrmUltimap2.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  canclose:=MessageDlg('Closing will free all collected data. Continue? (Tip: You can minimize this window instead)', mtConfirmation,[mbyes,mbno], 0, mbno)=mryes;
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

  result:=listText+' -error';
end;

procedure TfrmUltimap2.btnAddRangeClick(Sender: TObject);
var
  r: string;
  output: string;
  start, stop: uint64;
begin
  if l=nil then
    l:=tstringlist.create;

  symhandler.getModuleList(l);
  output:='';
  ShowSelectionList(self, 'Module list', 'Select a module or give your own range', l, output, true, @ModuleSelectEvent);
  if output<>'' then
  begin
    //check that output can be parsed

    if symhandler.parseRange(output, start, stop) then
      lbrange.Items.Add(inttohex(start,8)+'-'+inttohex(stop,8));
  end;

  freeandnil(l);
end;

procedure TfrmUltimap2.Button1Click(Sender: TObject);
var n:PRegionInfo;
begin
  regiontree:=TAvgLvlTree.CreateObjectCompare(@RegionCompare);

  getmem(n, sizeof(TRegionInfo));
  n^.memory:=nil;
  n^.info:=nil;
  n^.address:=$004000000;
  n^.size:=$100000;
  regiontree.Add(n);

  getmem(n, sizeof(TRegionInfo));
  n^.address:=$008000000;
  n^.size:=$100000;
  n^.memory:=nil;
  n^.info:=nil;
  regiontree.Add(n);

  getmem(n, sizeof(TRegionInfo));
  n^.address:=$00c000000;
  n^.size:=$100000;
  n^.memory:=nil;
  n^.info:=nil;
  regiontree.Add(n);

  cleanup;
end;



procedure TfrmUltimap2.FormDestroy(Sender: TObject);
begin
  cleanup;
  frmUltimap2:=nil;
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

procedure TfrmUltimap2.tActivatorTimer(Sender: TObject);
var i: integer;
begin
  for i:=0 to length(workers)-1 do
    if not workers[i].done then exit;

  tbRecordPause.enabled:=true;
  tActivator.Enabled:=false;
end;



end.

