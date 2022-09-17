unit frmcodefilterunit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, maps, Menus, syncobjs, newkernelhandler,
  ProcessHandlerUnit, CodeFilterCallOrAllDialog, PEInfoFunctions, PEInfounit,
  lua, lualib, lauxlib, LuaForm, LuaClass{$ifdef darwin},macport{$endif}, betterControls;

type

  { TfrmCodeFilter }

  TfrmCodeFilter = class(TForm)
    btnCancel: TButton;
    btnFilterOutExecutedAddresses: TButton;
    btnFilterOutNonExecutedAddresses: TButton;
    btnLoadAddressesByDisassembling: TButton;
    btnLoadAddressesFromFile: TButton;
    btnLoadAddressesFromTrace: TButton;
    btnShowList: TButton;
    btnStart: TButton;
    btnStop: TButton;
    btnFromUnwindInfo: TButton;
    frmLaunchBranchMapper: TButton;
    GroupBox1: TGroupBox;
    cfImageList: TImageList;
    Label1: TLabel;
    Label3: TLabel;
    lblExecuteCount: TLabel;
    lblStatus: TLabel;
    lblAddressList: TLabel;
    lvResults: TListView;
    miSaveAddressList: TMenuItem;
    miClearList: TMenuItem;
    miDeleteSelectedItems: TMenuItem;
    MenuItem4: TMenuItem;
    OpenDialog: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pnlStatus: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    pbProgress: TProgressBar;
    pmResults: TPopupMenu;
    SaveDialog: TSaveDialog;
    Timer1: TTimer;
    procedure btnCancelClick(Sender: TObject);
    procedure btnLoadAddressesByDisassemblingClick(Sender: TObject);
    procedure btnLoadAddressesFromFileClick(Sender: TObject);
    procedure btnLoadAddressesFromTraceClick(Sender: TObject);
    procedure btnShowListClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnFromUnwindInfoClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure frmLaunchBranchMapperClick(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure FilterClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvResultsData(Sender: TObject; Item: TListItem);
    procedure lvResultsDblClick(Sender: TObject);
    procedure miSaveAddressListClick(Sender: TObject);
    procedure miDeleteSelectedItemsClick(Sender: TObject);
    procedure miClearListClick(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
    procedure Panel3Click(Sender: TObject);
    procedure pmResultsPopup(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    callMapMREW: TMultiReadExclusiveWriteSynchronizer;
    callMap: TMap; //these breakpoints do NOT get added to the breakpoint list (not efficient enough)


    addressList: TList;
    addressListCS: TCriticalSection;

    count: integer; //number of bp's hit since last filter

    worker: TThread;
    addressListBuilder: TThread;
    fgui: boolean;
    nextfilter: (fNoFilter, fHasBeenExecuted, fHasNotBeenExecuted);

    modulelist: tstringlist;
    breakpointsSet: boolean;

    procedure addAddress(address: ptruint);
    procedure enableAllBreakpoints;
    procedure disableAllBreakpoints(wait: boolean=false);

    procedure done;

    procedure dofilter;

    procedure setGui(state: boolean);
    property gui: boolean read fgui write setGui;

    function ModuleListSelectionListSelToText(index: integer; listText: string): string;

    procedure AssemblyScanResultFound(address: ptruint; s:string);
    procedure AssemblyScanDone(sender: TObject);
    procedure AssemblyScanDestroyed(sender: TObject);

  public
    { public declarations }
    function isInList(address: ptruint): boolean;
    function isBreakpoint(address: ptruint; var originalbyte: byte): boolean;
    function handleBreakpoint(address: ptruint): boolean; //called by TDebugThreadHandler's. If true it knows it should just continue from here
    property hasBreakpointsSet: boolean read breakpointsSet;
  end;

procedure initializeLuaCodeFilter;

var
  frmCodeFilter: TfrmCodeFilter;

implementation

{$R *.lfm}

uses CEFuncProc, DissectCodeunit, DissectCodeThread, frmDisassemblyscanunit,
  frmSelectionlistunit, commonTypeDefs, symbolhandler, MemoryBrowserFormUnit,
  CEDebugger, frmBranchMapperUnit, symbolhandlerstructs, LuaHandler;

{ TfrmCodeFilter }

type
  TBPInfo=record
    address: ptruint;
    hasBeenExecuted: boolean;
    originalByte: byte;
  end;
  PBPInfo=^TBPInfo;

  TWorkerType=(wtSetBreakpoints, wtRemoveBreakpoints, wtFilterOutExecutedAddresses, wtFilterOutNonExecutedAddresses);

  TWorker=class(TThread)
  private
    position: integer;
    workertype: TWorkerType;
    procedure done;
  public
    procedure filter(WantedHasBeenExecutedState: boolean);
    procedure EnableAllBreakpoints;
    procedure DisableAllBreakpoints;
    procedure execute; override;
    constructor createEnabler;
    constructor createDisabler;
    constructor createHasBeenExecutedFilter;
    constructor createHasNotBeenExecutedFilter;
  end;

  TaddresslistBuilder=class(TThread)
  private
    procedure done;
  public
    procedure execute; override;
    constructor create;
  end;

resourcestring
  rsOpenAnAddressFile = 'Open an address file';
  rsSaveAnAddressFile = 'Save an address file';
  rsAddressList = 'Address List (%d)';
  rsCodeFilter = 'Code Filter';
  rsXIsAnInvalidRange = '%s is an invalid range (xxxx-xxxx)';
  rsSettingBreakpoints = 'Setting breakpoints';
  rsFilteringAddresses = 'Filtering addresses';
  rsCancel = 'Cancel List Refresh';
  rsYes = 'Yes';
  rsNo = 'No';
  rsShowRefreshList = 'Refresh list';
  rsAssemblyScanPleaseWait = 'Assembly Scan. Please wait';


procedure TaddresslistBuilder.done;
begin
  frmCodeFilter.btnShowList.caption:=rsShowRefreshList;
  frmCodeFilter.btnShowList.Enabled:=true;
end;

procedure TaddresslistBuilder.execute;
var
  mi: TMapIterator;//=nil;
  cs: TCriticalSection;
  al: TList;
  bpinfo: PBPInfo;
begin
  frmCodeFilter.callMapMREW.Beginread;
  al:=frmCodeFilter.addressList;
  cs:=frmCodeFilter.addressListCS;
  try
    mi:=TMapIterator.create(frmCodeFilter.callMap);

    cs.Enter;
    try
      al.Clear;
      al.Capacity:=frmCodeFilter.callMap.Count;
    finally
      cs.leave;
    end;

    mi.first;
    while (not mi.EOM) and (not terminated) do
    begin
      mi.GetData(bpinfo);
      cs.enter;
      al.Add(bpinfo);
      cs.leave;

      mi.next;
    end;
    mi.free;
  finally
    frmCodeFilter.callMapMREW.Endread;
    queue(done);
  end;
end;

constructor TaddresslistBuilder.create;
begin
  inherited create(false);
end;

//-------------TWorker-----------
procedure TWorker.done;
begin
  //make the gui accessible again
  frmCodeFilter.done;
end;

procedure TWorker.filter(WantedHasBeenExecutedState: boolean);
var
  mi: TMapIterator;
  bpinfo: PBPInfo;
  address: ptruint;
  cc: byte=$cc;
  x: ptruint;
begin
  frmCodeFilter.callMapMREW.Beginwrite;
  try
    mi:=TMapIterator.create(frmCodeFilter.callMap);
    mi.First;
    while (mi.valid and (not (mi.EOM or terminated))) do
    begin
      mi.GetData(bpinfo);
      address:=bpinfo^.address;

      mi.next;

      if bpinfo^.hasBeenExecuted<>WantedHasBeenExecutedState then
      begin
        frmCodeFilter.callMap.Delete(address);

        if frmCodeFilter.breakpointsSet and (not bpinfo^.hasBeenExecuted) then //restore
          WriteProcessMemory(processhandle,pointer(address), @bpinfo^.originalByte,1,x);

        freemem(bpinfo);
      end
      else
      begin
        //set the breakpoint back if still active
        if frmCodeFilter.breakpointsSet and bpinfo^.hasBeenExecuted then
        begin
          WriteProcessMemory(processhandle,pointer(address), @cc,1,x);
          bpinfo^.hasBeenExecuted:=false;
        end;

      end;
    end;

    mi.free;
  finally
    frmCodeFilter.callMapMREW.Endwrite;
  end;
end;

procedure TWorker.enableAllBreakpoints;
var mi: TMapIterator;
  bpinfo: PBPInfo;
  address: ptruint;

  cc: byte=$cc;
  x: ptruint;
begin
  position:=0;
  frmCodeFilter.count:=0;
  frmCodeFilter.callMapMREW.Beginread;
  mi:=TMapIterator.Create(frmCodeFilter.callmap);
  mi.First;
  while (not mi.EOM or terminated) do
  begin
    mi.GetID(address);
    mi.GetData(bpinfo);
    ReadProcessMemory(processhandle, pointer(address), @bpinfo^.originalByte,1,x);
    bpinfo^.hasBeenExecuted:=false;

    //set the bp
    WriteProcessMemory(processhandle, pointer(address), @cc,1,x);
    inc(position);

    mi.next;
  end;
  mi.free;
  frmCodeFilter.callMapMREW.Endread;
end;

procedure TWorker.DisableAllBreakpoints;
var mi: TMapIterator;
  bpinfo: PBPInfo;
  address: ptruint;

  oldcount: integer;
  x: ptruint;
begin
  position:=0;
  frmCodeFilter.callMapMREW.Beginread;

  mi:=TMapIterator.Create(frmCodeFilter.callmap);
  mi.First;
  while not mi.EOM do
  begin
    mi.GetID(address);
    mi.GetData(bpinfo);
    if bpinfo^.hasBeenExecuted=false then
      WriteProcessMemory(processhandle, pointer(address), @bpinfo^.originalByte,1,x);

    inc(position);

    mi.next;
  end;
  mi.free;
  frmCodeFilter.callMapMREW.Endread;

  //wait a bit till count has stopped updating
  sleep(50);
  oldcount:=frmCodeFilter.count;
  while oldcount<>frmCodeFilter.count do
  begin
    oldcount:=frmCodeFilter.count;
    sleep(500);
  end;
end;

procedure TWorker.execute;
begin
  case workertype of
    wtSetBreakpoints: EnableAllBreakpoints;
    wtRemoveBreakpoints: DisableAllBreakpoints;
    wtFilterOutExecutedAddresses: filter(false);
    wtFilterOutNonExecutedAddresses: filter(true);
  end;

  queue(done);
end;


constructor TWorker.createHasBeenExecutedFilter;
begin
  workertype:=wtFilterOutNonExecutedAddresses;
  inherited create(false);
end;

constructor TWorker.createHasNotBeenExecutedFilter;
begin
  workertype:=wtFilterOutExecutedAddresses;
  inherited create(false);
end;

constructor Tworker.createDisabler;
begin
  workertype:=wtRemoveBreakpoints;
  inherited create(false);
end;

constructor Tworker.createEnabler;
begin
  workertype:=wtSetBreakpoints;
  inherited create(false);
end;

//-------------------------------

procedure TfrmCodeFilter.Panel2Click(Sender: TObject);
begin

end;

procedure TfrmCodeFilter.Panel3Click(Sender: TObject);
begin

end;

procedure TfrmCodeFilter.pmResultsPopup(Sender: TObject);
begin
  miSaveAddressList.enabled:=lvResults.items.count>0;
  miClearList.enabled:=lvResults.items.count>0;
  miDeleteSelectedItems.enabled:=lvResults.selected<>nil;
end;

procedure TfrmCodeFilter.Timer1Timer(Sender: TObject);
var oldc, newc: integer;
begin
  lblExecuteCount.caption:=inttostr(count);

  if worker<>nil then
    pbProgress.position:=trunc((Tworker(worker).position/callMap.Count)*100);

  addresslistcs.enter;
  try
    oldc:=lvResults.Items.Count;
    newc:=addresslist.count;
    if oldc<>newc then
      lvResults.Items.Count:=newc;
  finally
    addresslistCS.Leave;
  end;

  lvresults.Refresh;
end;

procedure TfrmCodeFilter.done;
begin
  pnlStatus.visible:=false;
  gui:=true;
  btnCancel.visible:=false;

  if Tworker(worker).workertype=wtRemoveBreakpoints then
    breakpointsSet:=false;

  btnstart.enabled:=breakpointsSet=false;
  btnstop.enabled:=breakpointsSet;

  if (Tworker(worker).workertype=wtRemoveBreakpoints) and (nextfilter<>fNoFilter) then dofilter else
  begin
    btnShowList.Enabled:=true;
    btnShowList.Click;
  end;

  lblAddressList.caption:=format(rsAddressList, [callmap.Count]);
end;

procedure TfrmCodeFilter.dofilter;
begin
  if nextfilter=fNoFilter then exit;

  if worker<>nil then
    freeandnil(worker);

  lblStatus.caption:=rsFilteringAddresses;
  pnlStatus.visible:=true;

  gui:=false;
  if nextfilter=fHasBeenExecuted then
    worker:=TWorker.createHasBeenExecutedFilter
  else
  if nextfilter=fHasNotBeenExecuted then
    worker:=TWorker.createHasNotBeenExecutedFilter;


  btnShowList.Enabled:=false;

  nextfilter:=fNoFilter;
end;

procedure TfrmCodeFilter.setGui(state: boolean);
begin
  btnFilterOutNonExecutedAddresses.enabled:=state;
  btnFilterOutExecutedAddresses.enabled:=state;
  btnStart.enabled:=state;
  btnLoadAddressesFromTrace.enabled:=state;
  btnLoadAddressesByDisassembling.enabled:=state;
  btnLoadAddressesFromFile.enabled:=state;

  btnStop.enabled:=state;
end;

procedure TfrmCodeFilter.enableAllBreakpoints;
begin
  if startdebuggerifneeded=false then exit;

  if worker<>nil then
  begin
    worker.free;
    worker:=nil;
  end;

  breakpointsset:=true;

  gui:=false;
  lblStatus.caption:=rsSettingBreakpoints;
  pnlStatus.visible:=true;
  worker:=tworker.createEnabler;

  btnCancel.visible:=true;
  btnCancel.enabled:=true;
  btnStop.enabled:=false; //should be the case anyhow
end;

procedure TfrmCodeFilter.disableAllBreakpoints(wait: boolean=false);
begin
  if worker<>nil then
  begin
    worker.free;
    worker:=nil;
  end;

  gui:=false;
  lblStatus.caption:='Removing breakpoints';
  pnlStatus.visible:=true;
  worker:=tworker.createDisabler;

  if wait then
  begin
    worker.WaitFor;
    freeandnil(worker);
    breakpointsSet:=false;
  end
  else
  begin
    btnstart.enabled:=false;
    btnstop.enabled:=false;
  end;

end;

procedure TfrmCodeFilter.addAddress(address: ptruint);
var
  ob: byte;
  x: ptruint;
  bpinfo: PBPInfo;
begin
  if readprocessmemory(processhandle, pointer(address),@ob,1,x) then
  begin
    getmem(bpinfo, sizeof(tbpinfo));
    bpinfo^.address:=address;
    bpinfo^.originalByte:=ob;
    bpinfo^.hasBeenExecuted:=false;

    callmapmrew.Beginwrite;
    try
      if callmap.HasId(address)=false then
        callmap.Add(address, bpinfo);
    finally
      callmapmrew.Endwrite;
    end;

  end;
end;


function TfrmCodeFilter.isInList(address: ptruint): boolean;
var bpinfo: Pbpinfo;
begin
  callMapMREW.Beginread;
  result:=callmap.HasId(address);
  callMapMREW.Endread;
end;

function TfrmCodeFilter.isBreakpoint(address: ptruint; var originalbyte: byte): boolean;
var bpinfo: Pbpinfo;
begin
  callMapMREW.Beginread;

  if callmap.GetData(address, bpinfo) then
  begin
    originalbyte:= bpinfo^.originalByte;
    result:=bpinfo^.hasBeenExecuted;
  end
  else
    result:=false;

  callMapMREW.Endread;
end;

function TfrmCodeFilter.handleBreakpoint(address: ptruint): boolean;
var bpinfo: Pbpinfo;
  x: ptruint;
begin
  callMapMREW.Beginread;
  if callmap.GetData(address, bpinfo) then
  begin
    WriteProcessMemory(processhandle, pointer(address), @bpinfo^.originalByte,1,x);
    bpinfo^.hasBeenExecuted:=true;
    result:=true;

    inc(count);
  end
  else
    result:=false;

  callMapMREW.Endread;
end;

procedure TfrmCodeFilter.FormCreate(Sender: TObject);
begin
  callmapmrew:=TMultiReadExclusiveWriteSynchronizer.create;
  callmap:=TMap.Create(ituPtrSize,sizeof(PBPInfo));


  addresslist:=Tlist.Create;
  addresslistCS:=TCriticalSection.Create;

  nextfilter:=fNoFilter;

  LoadFormPosition(self);
end;

procedure TfrmCodeFilter.FormDestroy(Sender: TObject);
begin
  SaveFormPosition(self);
end;

procedure TfrmCodeFilter.btnLoadAddressesFromTraceClick(Sender: TObject);
var tv: TTreeview=nil;
  temp: dword;
  f: tfilestream=nil;
  m: tmemorystream=nil;
  d: TForm;

  callorall: TCallOrAllDialog=nil;

  callonly: boolean;

  i,sep: integer;

  s: string;
  a: ptruint;
begin
  OpenDialog.Filter:='CE Trace (*.cetrace)|*.cetrace|All file (*.*)|*.*';
  OpenDialog.Title:='Open a CE Trace file';

  if opendialog.Execute then
  begin
    f:=TFileStream.create(opendialog.filename, fmOpenRead or fmShareDenyNone);
    try
      f.ReadBuffer(temp, sizeof(temp));
      f.readbuffer(temp, sizeof(temp));

      m:=TMemoryStream.create;
      if temp>0 then
        m.CopyFrom(f, temp);

      m.Position:=0;
      tv:=TTreeView.Create(self);
      tv.LoadFromStream(m);

      //parse the treeview

      callorall:=TCallOrAllDialog.Create(self);
      case callorall.showmodal of
        mrCancel: exit;
        mrNoToAll: callonly:=true;
        mrAll: callonly:=false;
      end;

      for i:=0 to tv.Items.Count-1 do
      begin
        s:=tv.items[i].Text;
        if (callonly=false) or (pos('call ',s)>0) then
        begin
          sep:=pos('-',s);
          if sep>0 then
          begin
            s:=trim(copy(s,1,sep-1));
            a:=strtoint64('$'+s);
            addAddress(a);
          end;
        end;
      end;

    finally
      if callorall<>nil then
        callorall.free;

      if m<>nil then
        m.free;

      if tv<>nil then
        tv.free;

      f.free;

      lblAddressList.caption:=format(rsAddressList, [callmap.Count]);
      btnShowList.Click;

      btnStart.enabled:=callmap.count>0;
    end;
  end;
end;

procedure TfrmCodeFilter.btnShowListClick(Sender: TObject);
var
  mi: TMapIterator;
  starttime: qword; //so the gathering takes at most 2 seconds
begin
  if (addresslistBuilder=nil) or (Addresslistbuilder.Finished) then
  begin
    if addresslistBuilder<>nil then
      freeandnil(addressListBuilder);

    addresslistBuilder:=TAddressListBuilder.create;
    btnShowList.Caption:=rsCancel;
  end
  else
  begin
    addresslistBuilder.terminate;
    btnShowList.Enabled:=false;
  end;
end;

procedure TfrmCodeFilter.btnCancelClick(Sender: TObject);
begin
  if worker<>nil then
  begin
    worker.Terminate;
    worker.WaitFor;
  end;

  btnCancel.enabled:=false;
  disableAllBreakpoints;
end;

procedure TfrmCodeFilter.AssemblyScanResultFound(address: ptruint; s:string);
begin
  addAddress(address);
end;

procedure TfrmCodeFilter.AssemblyScanDone(sender: TObject);
begin
  TfrmDisassemblyscan(Sender).close;
end;

procedure TfrmCodeFilter.AssemblyScanDestroyed(sender: TObject);
begin
  btnLoadAddressesByDisassembling.enabled:=true;

  lblAddressList.caption:=format(rsAddressList, [callmap.Count]);
  btnShowList.Click;

  btnStart.enabled:=callmap.count>0;
end;

function TfrmCodeFilter.ModuleListSelectionListSelToText(index: integer; listText: string): string;
begin
  if modulelist<>nil then
    result:=inttohex(tmoduledata(modulelist.Objects[index]).moduleaddress,8)+'-'+inttohex(tmoduledata(modulelist.Objects[index]).moduleaddress+tmoduledata(modulelist.Objects[index]).modulesize,8)
  else
    result:='';
end;

procedure TfrmCodeFilter.btnLoadAddressesByDisassemblingClick(Sender: TObject);
var
  sellist: TfrmSelectionList;
  s: tstringlist;
  scanner:  TfrmDisassemblyscan;

  rangetext: string;
  i: integer;

  startstr, stopstr: string;
  start,stop: ptruint;

  md: tmoduledata;

  buf: array [0..4096] of byte;
  x: ptruint;

  base: ptruint;
  codebase: ptruint;
  codesize: integer;
begin
  if modulelist<>nil then
    cleanModuleList(modulelist);

  modulelist:=tstringlist.create;
  GetModuleList(modulelist,true);

  //adjust the moduleinfo to code section only
  for i:=0 to modulelist.Count-1 do
  begin
    md:=tmoduledata(modulelist.Objects[i]);
    base:=md.moduleaddress;

    if ReadProcessMemory(processhandle, pointer(base), @buf[0],4096,x) then
    begin
      codebase:=peinfo_getcodebase(@buf[0],4096);
      codesize:=peinfo_getcodesize(@buf[0],4096);

      if (codebase>0) and (codesize>0) then
      begin
        md.moduleaddress:=base+codebase;
        md.modulesize:=codesize;
      end;
    end;
  end;


  sellist:=TfrmSelectionList.Create(self, modulelist);
  sellist.Caption:=rsCodeFilter;
  sellist.SelectionToText:=ModuleListSelectionListSelToText;
  sellist.custominput:=true;

  if sellist.ShowModal=mrok then
  begin
    rangetext:=sellist.selected;
    i:=pos('-',rangetext);
    if i=0 then raise exception(format(rsXIsAnInvalidRange, [rangetext]));

    startstr:=copy(rangetext,1,i-1);
    stopstr:=copy(rangetext,i+1,length(rangetext));

    start:=symhandler.getAddressFromName(startstr);
    stop:=symhandler.getAddressFromName(stopstr);

    scanner:=TfrmDisassemblyscan.Create(self);
    s:=tstringlist.create;
    s.add('call *');

    scanner.stringstofind:=s;
    scanner.OnResultFound:=AssemblyScanResultFound;
    scanner.OnScanDone:=AssemblyScanDone;
    scanner.OnDestroy:=AssemblyScanDestroyed;
    scanner.startaddress:=start;
    scanner.stopaddress:=stop;

    scanner.caption:=rsAssemblyScanPleaseWait;

    s.free;


    scanner.show;
    btnLoadAddressesByDisassembling.enabled:=false;
  end;


  freeandnil(sellist);
end;

procedure TfrmCodeFilter.btnLoadAddressesFromFileClick(Sender: TObject);
var
  l: tstringlist;
  i: integer;
  ext: string;

  count: integer;
  x: word;
  f: tfilestream;

  s: string;
  ml: Tstringlist;
  baseaddresses: array of ptruint;
  modinfo: TModuleInfo;
  address: ptruint;
begin
  OpenDialog.Filter:='Address files (*.txt;*.ceaddress)|*.ceaddress;*.txt|All file (*.*)|*.*';
  OpenDialog.Title:=rsOpenAnAddressFile;

  if OpenDialog.Execute then
  begin
    ext:=ExtractFileExt(opendialog.filename);
    if ext='.txt' then
    begin
      l:=tstringlist.create;
      l.LoadFromFile(opendialog.FileName{$if FPC_FULLVERSION>=030200},true{$endif});
      for i:=0 to l.count-1 do
        addAddress(symhandler.getAddressFromName(trim(l[i])));

      l.free;
    end
    else
    if ext='.ceaddress' then
    begin
      f:=tfilestream.create(opendialog.filename, fmOpenRead or fmShareDenyNone);
      try
        count:=f.ReadWord;

        ml:=tstringlist.create;

        setlength(baseaddresses, count);
        for i:=0 to count-1 do
        begin
          s:=f.ReadAnsiString;
          ml.add(s);
          if symhandler.getmodulebyname(s,modinfo) then
            baseaddresses[i]:=modinfo.baseaddress
          else
            baseaddresses[i]:=0;
        end;


        while f.Position<f.Size do
        begin
          x:=f.ReadWord;
          if x=$ffff then
            addAddress(f.ReadQWord)
          else
          begin
            address:=f.ReadQWord;
            if baseaddresses[x]<>0 then
              addAddress(baseaddresses[x]+address);
          end;
        end;
      finally
        f.free;
      end;
    end;

    lblAddressList.caption:=format(rsAddressList, [callmap.Count]);
    btnShowList.Click;

    btnStart.enabled:=callmap.count>0;
  end;
end;

procedure TfrmCodeFilter.btnStartClick(Sender: TObject);
begin
  enableAllBreakpoints;
end;

procedure TfrmCodeFilter.btnStopClick(Sender: TObject);
begin
  nextfilter:=fNoFilter;
  disableAllBreakpoints;
end;

procedure TfrmCodeFilter.btnFromUnwindInfoClick(Sender: TObject);
{$ifdef windows}
var
  sellist: TfrmSelectionList;
  md: tmoduledata;

  el: TExceptionList;
  i: integer;

  rte: TRunTimeEntry;
{$endif}
begin
  {$ifdef windows}
  if modulelist<>nil then
    cleanModuleList(modulelist);

  modulelist:=tstringlist.create;
  GetModuleList(modulelist,true);


  sellist:=TfrmSelectionList.Create(self, modulelist);
  sellist.Caption:=rsCodeFilter;
  sellist.SelectionToText:=ModuleListSelectionListSelToText;

  if sellist.ShowModal=mrok then
  begin
    if sellist.itemindex<>-1 then
    begin
      md:=tmoduledata(modulelist.objects[sellist.itemindex]);
      el:=peinfo_getExceptionList(md.moduleaddress);
      if el<>nil then
      begin
        for i:=0 to el.Count-1 do
          addAddress(md.moduleaddress+el[i].start);
      end;
    end;

    lblAddressList.caption:=format(rsAddressList, [callmap.Count]);
    btnShowList.Click;

    btnStart.enabled:=callmap.count>0;
  end;
  {$else}
  MessageDlg('Only for windows',mtError,[mbok],0);
  {$endif}
end;

procedure TfrmCodeFilter.FormShow(Sender: TObject);
begin
  btnFromUnwindInfo.visible:=processhandler.is64Bit;

end;

procedure TfrmCodeFilter.frmLaunchBranchMapperClick(Sender: TObject);
begin
  if frmBranchMapper=nil then
    frmBranchMapper:=TfrmBranchMapper.Create(application);

  frmBranchMapper.show;
end;

procedure TfrmCodeFilter.Button7Click(Sender: TObject);
begin

end;

procedure TfrmCodeFilter.FilterClick(Sender: TObject);
var hasbeenexecuted: boolean;
begin
  if addressListBuilder<>nil then
  begin
    addressListBuilder.Terminate;
    addressListBuilder.WaitFor;
    freeandnil(addressListBuilder);
  end;

  addressList.Clear;
  lvresults.Items.Count:=0;

  case tbutton(sender).tag of
    0: nextfilter:=fHasBeenExecuted;
    1: nextfilter:=fHasNotBeenExecuted;
  end;

  dofilter;
  count:=0;
end;

procedure TfrmCodeFilter.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if btnCancel.Enabled then
    btncancel.Click;

  if worker<>nil then
    worker.WaitFor;

  freeandnil(worker);
end;

procedure TfrmCodeFilter.lvResultsData(Sender: TObject; Item: TListItem);
var
  i: TMapIterator;
  bpinfo: PBPInfo;
begin
  addresslistCS.enter;
  if item.index<addresslist.Count then
  begin
    bpinfo:=addresslist.Items[item.index];
    item.caption:=symhandler.getNameFromAddress(bpinfo^.address);

    if bpinfo^.hasBeenExecuted then
      item.SubItems.add(rsYes)
    else
      item.SubItems.add(rsNo);
  end;

  addresslistCS.leave;
end;

procedure TfrmCodeFilter.lvResultsDblClick(Sender: TObject);
var bpinfo: pbpinfo;
begin
  if lvResults.ItemIndex<>-1 then
  begin
    addressListCS.enter;
    try
      bpinfo:=addressList[lvResults.itemindex];
      memorybrowser.disassemblerview.SelectedAddress:=bpinfo^.address;

    finally
      addressListCS.leave;
    end;
  end;
end;

procedure TfrmCodeFilter.miSaveAddressListClick(Sender: TObject);
var
  list: Tstringlist;
  i: integer;
begin
  savedialog.Filter:='Address text file (*.txt)|*.txt|All file (*.*)|*.*';
  savedialog.Title:=rsSaveAnAddressFile;
  savedialog.DefaultExt:='.txt';

  if savedialog.execute then
  begin
    list:=tstringlist.create;
    addresslistcs.enter;
    try
      list.Capacity:=addresslist.count;

      for i:=0 to addresslist.Count-1 do
        list.add(symhandler.getNameFromAddress(pbpinfo(addresslist.Items[i])^.address));

      list.SaveToFile(savedialog.filename);
    finally
      addresslistcs.leave;
      list.free;
    end;
  end;
end;

procedure TfrmCodeFilter.miDeleteSelectedItemsClick(Sender: TObject);
var
  bpinfo: PBPInfo;
  i: integer;

  delcount: integer;
  itemindex: integer;
begin
  if worker<>nil then
    worker.WaitFor;

  if btnStop.Enabled then
    disableAllBreakpoints(true);

  delcount:=0;

  callmapmrew.Beginwrite;
  addresslistcs.enter;

  for i:=lvResults.items.Count-1 downto 0 do
  begin
    if lvresults.items[i].Selected then
    begin
      bpinfo:=addresslist.items[i];
      addresslist.Delete(i);
      callmap.Delete(bpinfo^.address);
      freemem(bpinfo);

      inc(delcount);
    end;
  end;

  addressListCS.leave;
  callmapmrew.endwrite;

  lvResults.Items.count:=lvResults.Items.Count-delcount;
end;

procedure TfrmCodeFilter.miClearListClick(Sender: TObject);
var
  mi: TMapIterator;
  bpinfo: PBPInfo;
begin
  if worker<>nil then
    worker.WaitFor;

  if btnStop.Enabled then
    disableAllBreakpoints(true);


  callmapmrew.Beginwrite;
  mi:=TMapIterator.create(callmap);

  while not mi.eom do
  begin
    mi.GetData(bpinfo);
    freemem(bpinfo);
    mi.next;
  end;

  callmap.Clear;
  callmapmrew.endwrite;

  addresslist.Clear;
  lvResults.items.count:=0;
end;

//lua
function frmCodeFilter_isInList(L: PLua_state): integer; cdecl;
var
  f: TfrmCodeFilter;
  r: boolean;
  count: integer;
begin
  result:=0;
  f:=TfrmCodeFilter(luaclass_getClassObject(L));
  if lua_gettop(L)>=1 then
  begin
    lua_pushboolean(L,f.isInList(lua_tointeger(L,1)));
    result:=1;
  end;
end;

function lua_getUltimap2(L: PLua_state): integer; cdecl;
begin
  luaclass_newClass(L,frmCodeFilter);
  result:=1;
end;

procedure frmCodeFilter_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  customform_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'isInList', @frmCodeFilter_isInList);
end;

procedure initializeLuaCodeFilter;
begin
  lua_register(LuaVM, 'getCodeFilter', @lua_getUltimap2);
end;


initialization
  registerclass(TfrmCodeFilter);

  luaclass_register(TfrmCodeFilter, @frmCodeFilter_addMetaData);

end.


end.

