unit frmUltimapUnit;

{$mode delphi}

interface

uses
  windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DBK32functions, NewKernelHandler, cefuncproc, AvgLvlTree, ExtCtrls, ComCtrls,
  math,  symbolhandler, maps, Menus, disassembler, multicpuexecution, syncobjs,
  genericHotkey, HotKeys, frmHotkeyExUnit, frmSelectionlistunit, commonTypeDefs;



type
  PBranchdata=^TBranchData;
  TBranchData=record
    toAddress: PtrUInt;
    lastFromAddress: ptruint;
    //other stuff to come (e.g: triggered since last check)
    count: integer;
{$ifdef predictlog}
    predicted: boolean;
{$endif}
    isCalled: boolean;  //the worker sets this to true when called. The user check sets this to false
    wrong: boolean; //set to true if a user check has failed

    Previous: PBranchData;
    Next: PBranchData;
  end;


  TBTS=packed record
    LastBranchFrom: QWord;
    LastBranchTo:   QWord;
    Predicted:      QWord;
  end;
  PBTS=^TBTS;
  TBTSArray=array [0..0] of TBTS;
  PBTSArray=^TBTSArray;


type
  TUltimap_DataHandlerThread=class(TThread)
  private
    procedure UpdateBranchData(branchdata: PBranchData; BTS: PBTS);
    procedure map(buf: PBTSArray; size: integer);
  public
    workerid: integer;
    branchtree: TAvgLvlTree;
    branchtreeCS: TCriticalSection;
    procedure execute; override;
  end;


type

  { TfrmUltimap }

  TfrmUltimap = class(TForm)
    btnExecuted: TButton;
    btnFilterCallCount: TButton;
    btnFilterModule: TButton;
    btnNotCalled: TButton;
    btnNotExecuted: TButton;
    btnPause: TButton;
    btnResetCount: TButton;
    btnRet: TButton;
    btnStart: TButton;
    btnStop: TButton;
    Button5: TButton;
    Button6: TButton;
    cbFilterFuturePaths: TCheckBox;
    cbfilterOutNewEntries: TCheckBox;
    cbLogToFile: TRadioButton;
    cbParseData: TRadioButton;
    cbPreemptiveFlush: TCheckBox;
    Edit1: TEdit;
    edtBufSize: TEdit;
    edtFilename: TEdit;
    edtWorkerCount: TEdit;
    Flusher: TTimer;
    umImageList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblLastfilterresult: TLabel;
    ListView1: TListView;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    miRemoveHotkey: TMenuItem;
    miSetHotkey: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    pmSetHotkey: TPopupMenu;
    PopupMenu1: TPopupMenu;
    Timer1: TTimer;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnFilterModuleClick(Sender: TObject);
    procedure btnResetCountClick(Sender: TObject);
    procedure btnExecutedClick(Sender: TObject);
    procedure btnNotExecutedClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure btnNotCalledClick(Sender: TObject);
    procedure btnRetClick(Sender: TObject);
    procedure cbPreemptiveFlushChange(Sender: TObject);
    procedure cbfilterOutNewEntriesChange(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FilterClick(Sender: TObject);
    procedure FlusherTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label7Click(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure ListView1DblClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure miSetHotkeyClick(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure miRemoveHotkeyClick(Sender: TObject);
    procedure pmSetHotkeyPopup(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    branchtree: TAvgLvlTree;
    branchtreeCS: syncobjs.TCriticalSection;
    workers: Array of TUltimap_DataHandlerThread;

    validlist: array of PBranchdata;
    userdefinedpos: boolean;

    callTable: TMap;
    retTable: TMap;
    iscalldisassembler: TDisassembler;
    isretdisassembler: TDisassembler;   //not really needed...


    FilterHotkey: array [0..2] of TGenericHotkey;
    paused: boolean;



    target_cr3: qword;
    bufsize: dword;
    function branchcompare(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
    procedure ApplyFilter(f: integer);
    function iscall(address: ptruint): boolean;
    function isret(address: ptruint): boolean;

    procedure flush;
  public
    { public declarations }
  end; 

var
  frmUltimap: TfrmUltimap;

  TotalBranches: system.dword;
  isFlushing: boolean;
  cpulist: TIntegerSet;
  workdone: integer;
  workercount: integer;
  hashandled: THandle;
  flushcs: TCriticalSection;

  filteroutnewentries: boolean;

resourcestring
  rsRemoveHotkey = 'Remove hotkey (%s)';

implementation

{$R *.lfm}

uses MemoryBrowserFormUnit, vmxfunctions, ProcessHandlerUnit, AdvancedOptionsUnit;

resourcestring
  rsUUOld = 'old=';
  rsUUNew = ' new=';
  rsUUErrorDuringMap = 'Error during map';
  rsUUSorryButThisFeatureIsOnlyAvailableOnIntelCpus = 'Sorry, but this feature is only available on intel cpu''s';
  rsUUPleaseRunThe64bitVersionOfCheatEngineToMakeUseOfThisFeature = 'Please run the 64-bit version of Cheat Engine to make use of this feature';
  rsUUThisFunctionNeedsAtLeast200BytesForTheHeaderOfTheBuffer = 'This function needs at least 200 bytes for the header of the buffer';
  rsUUTheMaximumNumberOfWorkersIs64 = 'The maximum number of workers is 64';
  rsUUPause = 'Pause';
  rsUUResume = 'Resume';
  rsUUThisWillResetTheCallcountofFunctionBackto0 = 'This will reset the callcount of functions back to 0. This can not be undone. Continue?';
  rsUUThisWillBringBackAllFoundInstructions = 'This will bring back all found instructions. Continue?';
  rsUULastFilterResults = 'Last filter results: filtered out %d left: %d';

{$ifdef cpu64}
const kernelbase=QWORD($800000000000);        //sign extended to :$ffff800000000000
{$else}
const kernelbase=DWORD($80000000);
{$endif}

{TUltimap_DataHandlerThread}
procedure TUltimap_DataHandlerThread.UpdateBranchData(branchdata: PBranchData; BTS: PBTS);
begin
  InterLockedIncrement(branchdata.count);

  branchdata.lastFromAddress:=BTS.LastBranchFrom;
  {$ifdef predictlog}
  if bts.Predicted=1 then
    branchdata.predicted:=true;
  {$endif}

  branchdata.isCalled:=true;
end;

procedure TUltimap_DataHandlerThread.map(buf: PBTSArray; size: integer);
var i: integer;
    lbt: ptruint;

    temp: TBranchData;
    tn: TAvgLvlTreeNode;

    haslock: boolean;

    newbranchdata: PBranchData;
    prev, next: TAvgLvlTreeNode;

    new,old: integer;
begin
  haslock:=false;
  old:=0;
  new:=0;
  for i:=0 to size-1 do
  begin
    if (buf[i].LastBranchFrom>=kernelbase) then
      continue; //don't add returns from kernelmode (interrupt/taskswitches. Useless info)

    lbt:=buf[i].LastBranchTo;

    temp.toAddress:=lbt;
    tn:=branchtree.Find(@temp);

    if tn<>nil then
    begin
      UpdateBranchData(tn.Data, @buf[i]); //update the node
      inc(old);
    end
    else
    begin
      //not yet in the list, add it

      //aquire lock if I didn't have it yet
      if not haslock then
      begin
        branchtreeCS.Enter; //once I get a lock I won't release it until i'm completely done with my part

        haslock:=true;

        //check if it already has been added
        tn:=branchtree.Find(@temp);
        if tn<>nil then
          UpdateBranchData(tn.Data, @buf[i]);
      end
      else
      begin
        inc(new);
        //I already had the lock so I an be sure nothing else added this
        //add it
        getmem(newbranchdata, sizeof(TBranchData));
        newbranchdata.count:=1;
        newbranchdata.toAddress:=lbt;
        newbranchdata.isCalled:=true;
        newbranchdata.wrong:=filteroutnewentries;
        newbranchdata.lastFromAddress:=buf[i].LastBranchFrom;
{$ifdef predictlog}
        newbranchdata.predicted:=buf[i].Predicted<>0;
{$endif}


        tn:=branchtree.Add(newbranchdata);
        prev:=branchtree.FindPrecessor(tn);
        next:=branchtree.FindSuccessor(tn);

        if prev=nil then
          newbranchdata.previous:=nil
        else
        begin
          newbranchdata.previous:=prev.Data;
          PBranchdata(prev.data).next:=newbranchdata;
        end;

        if next=nil then
          newbranchdata.next:=nil
        else
        begin
          newbranchdata.next:=next.Data;
          PBranchdata(next.data).previous:=newbranchdata;
        end;


        system.InterLockedIncrement(TotalBranches);
      end;

    end;


  end;

  if haslock then
    branchtreeCS.Leave; //release lock

  OutputDebugString(rsUUOld+inttostr(old)+rsUUNew+inttostr(new));
end;


procedure TUltimap_DataHandlerThread.execute;
var UltimapDataEvent: TUltimapDataEvent;
  buf: PBTSArray;
  i: integer;
  z: qword;
begin
  while not terminated do
  begin
    if ultimap_waitForData(1000, @UltimapDataEvent) then
    begin
      try

        OutputDebugString('UltimapDataEvent.BlockID='+inttostr(UltimapDataEvent.BlockID));
        OutputDebugString('UltimapDataEvent.CpuID='+inttostr(UltimapDataEvent.CpuID));
        OutputDebugString('UltimapDataEvent.Address='+inttohex(UltimapDataEvent.Address,8));
        OutputDebugString('UltimapDataEvent.Size='+inttostr(UltimapDataEvent.Size));

        buf:=pointer(UltimapDataEvent.Address);
        try
          //OutputDebugString(format('1=%x 2=%x 3=%x 4=%x\n',[buf[0], buf[1], buf[2], buf[3]]));
          map(buf, UltimapDataEvent.Size div sizeof(TBTS));
        except
          OutputDebugString(rsUUErrorDuringMap);
        end;


      finally
        ultimap_continue(@UltimapDataEvent);

        if isFlushing then
        begin
          flushcs.enter;

          if isflushing then //check again in case it changed back...
          begin
            //Is this cpuid in the list?

            if not (integer(UltimapDataEvent.CpuID) in cpulist) then
            begin
              //If not, add it and increase workdone
              cpulist:=cpulist+[integer(UltimapDataEvent.CpuID)];

              inc(workdone);
              if workdone>=workercount then
                setevent(hashandled);
            end;




          end;

          flushcs.leave;
        end;

      end;
    end;



  end;

  //just make sure it's really finished (could be there are still multiple threads waiting to continue)
  while ultimap_waitForData(10, @UltimapDataEvent) do
    ultimap_continue(@UltimapDataEvent);

end;

{ TfrmUltimap }

function TfrmUltimap.branchcompare(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
begin
  //used to sort the binary tree
  result:=CompareValue(pbranchdata(Data1).toAddress, pbranchdata(Data2).toAddress);
end;


procedure TfrmUltimap.btnStartClick(Sender: TObject);
var


  filename: widestring;

  i: integer;
  //eprocess: qword;
begin
  if isAMD then
    raise exception.create(rsUUSorryButThisFeatureIsOnlyAvailableOnIntelCpus);

  {$ifdef cpu32}
  if Is64bitOS then raise exception.create(rsUUPleaseRunThe64bitVersionOfCheatEngineToMakeUseOfThisFeature);
  {$endif}

  TotalBranches:=0;

  LoadDBK32;

  {$ifndef NOVMX}  //for debugging purpose
  NeedsDBVM;
  {$endif}


  //eprocess:=GetPEProcess(processid);

  //OutputDebugString('Going to start ultimap for processid '+inttohex(processid,1)+' which has EProcess: '+inttohex(eprocess,8));

  GetCR3FromPID(processid, target_cr3);
  OutputDebugString('CR3='+inttohex(target_cr3,8));

  bufsize:=strtoint(edtBufSize.text);
  filename:=edtFilename.text;
  workercount:=strtoint(edtWorkerCount.text);

  if bufsize<2000 then
    raise exception.create(rsUUThisFunctionNeedsAtLeast200BytesForTheHeaderOfTheBuffer);

  if workercount>64 then
    raise exception.create(rsUUTheMaximumNumberOfWorkersIs64);




  branchtree:=TAvgLvlTree.CreateObjectCompare(branchcompare);
  branchtreeCS:=TCriticalSection.Create;

  //setSelectiveMSR; Looks like I was wrong, this is Last Branch Record only, no data store



  if ultimap(target_cr3, (1 shl 6) or (1 shl 7) or (1 shl 9) or (1 shl 8), bufsize, false, pwidechar(filename), workercount) then
  begin
    hashandled:=CreateEvent(nil, true, false, nil);
    setlength(workers, workercount);
    for i:=0 to workercount-1 do
    begin


      workers[i]:=TUltimap_DataHandlerThread.Create(true);
      workers[i].branchtree:=branchtree;
      workers[i].branchtreeCS:=branchtreeCS;
      workers[i].workerid:=i;
      workers[i].Start;
    end;
  end;




  paused:=false;
//  btnPause.tag:=1;
  btnPause.caption:=rsUUPause;
  btnPause.enabled:=true;
  btnStop.enabled:=true;
  btnstart.enabled:=false;

  beep;
end;

procedure TfrmUltimap.btnStopClick(Sender: TObject);
var i: integer;
begin
  ultimap_disable();

  for i:=0 to length(workers)-1 do
    workers[i].Terminate;

  for i:=0 to length(workers)-1 do
  begin
    workers[i].WaitFor;
    workers[i].free;
    workers[i]:=nil;
  end;

  setlength(workers,0);
  workercount:=0;

  paused:=false;

  btnStop.enabled:=false;
  btnpause.enabled:=false;
  btnStart.enabled:=true;

  errorbeep;
end;

procedure TfrmUltimap.btnPauseClick(Sender: TObject);
begin
  if not paused then
  begin
    ultimap_pause;
    paused:=true;
    btnPause.caption:=rsUUResume;
  end
  else
  begin
    ultimap_resume;
    paused:=false;
    btnPause.caption:=rsUUPause;
  end;
  beep;
end;

procedure TfrmUltimap.btnFilterModuleClick(Sender: TObject);
begin
  ApplyFilter(4);
end;

procedure TfrmUltimap.btnResetCountClick(Sender: TObject);
var n: TAvgLvlTreeNode;
  d: PBranchdata;
  count: integer;
begin
  flush;

  if (branchtree<>nil) and (messagedlg(rsUUThisWillResetTheCallcountofFunctionBackto0, mtConfirmation, [mbyes, mbno], 0)=mryes) then
  begin
    branchtreecs.Enter;
    try
      n:=branchtree.FindLowest;

      if n=nil then exit;

      d:=PBranchdata(n.Data);
      while d<>nil do
      begin
        d.count:=0;
        d:=d.Next;
      end;

    finally
      branchtreecs.leave;
    end;
  end;


  listview1.Refresh;
end;

procedure TfrmUltimap.btnExecutedClick(Sender: TObject);
begin
  ApplyFilter(0);
  if cbFilterFuturePaths.checked then
    cbfilterOutNewEntries.checked:=true;
end;

procedure TfrmUltimap.btnNotExecutedClick(Sender: TObject);
begin
  ApplyFilter(1);
end;

procedure TfrmUltimap.Button1Click(Sender: TObject);
begin

end;

procedure TfrmUltimap.btnNotCalledClick(Sender: TObject);
begin
  ApplyFilter(2);
end;

procedure TfrmUltimap.Button5Click(Sender: TObject);
//generate a pointerlist with valid entries, then set the count to the number of not wrong entries
var n: TAvgLvlTreeNode;
  d: PBranchdata;
  count: integer;
  maxvalidlist: integer;
begin
  if branchtree=nil then exit;

  count:=0;

  if length(validlist)=0 then
    setlength(validlist,1024);

  maxvalidlist:=length(validlist);


  //first find the count
  n:=branchtree.FindLowest;
  if n<>nil then
  begin

    d:=PBranchdata(n.Data);
    while d<>nil do
    begin
      if d.wrong=false then
      begin
        inc(count);

        //check if enough memory is available for this list, if not, allocate it
        if count>=maxvalidlist then
        begin
          maxvalidlist:=maxvalidlist*4;
          setlength(validlist, maxvalidlist);
        end;

        validlist[count-1]:=d;

      end;
      d:=d.Next;
    end;

  end;

  listview1.Items.Count:=count;
end;

procedure TfrmUltimap.Button6Click(Sender: TObject);
var n: TAvgLvlTreeNode;
  d: PBranchdata;
  count: integer;
begin
  flush;

  branchtreeCS.enter;
  try
    if (branchtree<>nil) and (messagedlg(rsUUThisWillBringBackAllFoundInstructions, mtConfirmation, [mbyes, mbno], 0)=mryes) then
    begin
      n:=branchtree.FindLowest;
      if n=nil then exit;

      d:=PBranchdata(n.Data);
      while d<>nil do
      begin
        d.wrong:=false;
        d:=d.Next;
      end;
    end;

  finally
    branchtreeCS.leave;
  end;
end;



procedure TfrmUltimap.btnRetClick(Sender: TObject);
begin
  applyfilter(5);
end;

procedure TfrmUltimap.cbPreemptiveFlushChange(Sender: TObject);
begin
  flusher.enabled:=cbPreemptiveFlush.checked;
end;

procedure TfrmUltimap.cbfilterOutNewEntriesChange(Sender: TObject);
begin
  filteroutnewentries:=cbfilterOutNewEntries.checked;
end;

procedure TfrmUltimap.Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key=VK_RETURN then
    btnFilterCallCount.click;
end;

function TfrmUltimap.iscall(address: ptruint): boolean;
var iscall: boolean;
  x: string;
  a: ptruint;
begin
  //check if it's a call
  if callTable=nil then //create the calltable first
  begin
    callTable:=TMap.Create(ituPtrSize,sizeof(boolean));
    iscalldisassembler:=Tdisassembler.create;
  end;

  if not calltable.GetData(address, iscall) then
  begin
    //not yet in the list
    //add if it's a call or not
    a:=address;
    iscalldisassembler.disassemble(a, x);
    iscall:=iscalldisassembler.LastDisassembleData.iscall;

    callTable.Add(address, iscall);
  end;

  result:=iscall;
end;

function TfrmUltimap.isret(address: ptruint): boolean;
var isret: boolean;
  x: string;
  a: ptruint;
begin
  //check if it's a ret
  if rettable=nil then //create the calltable first
  begin
    rettable:=TMap.Create(ituPtrSize,sizeof(boolean));
    isretdisassembler:=Tdisassembler.create;
  end;

  if not rettable.GetData(address, isret) then
  begin
    //not yet in the list
    //add if it's a ret or not
    a:=address;
    isretdisassembler.disassemble(a, x);
    isret:=iscalldisassembler.LastDisassembleData.isret;

    callTable.Add(address, isret);
  end;

  result:=isret;
end;

procedure TfrmUltimap.ApplyFilter(f: integer);
var n: TAvgLvlTreeNode;
  d: PBranchdata;
  wrongcount: integer;
  notwrong: integer;

  countvalue: integer;
  list:  tstringlist;

  startaddress, stopaddress: PtrUInt;

  r: TModalResult;
begin
  if branchtree=nil then
  begin
    errorbeep;
    exit;
  end;

  flush;


  if f=3 then
    countvalue:=strtoint(edit1.text);

  if f=4 then
  begin
    //show a modulelist to pick from
    list:=tstringlist.Create;
    GetModuleList(list, true);
    try

      with TfrmSelectionList.create(self, list) do
      begin
        r:=showmodal;

       // showmessage('showmodal returned '+inttostr(r));

        if r<>mrok then exit;

       // showmessage('itemindex='+inttostr(itemindex));
        if itemindex=-1 then exit;


        startaddress:=tmoduledata(list.objects[itemindex]).moduleaddress;
        stopaddress:=tmoduledata(list.objects[itemindex]).moduleaddress+tmoduledata(list.objects[itemindex]).modulesize;

       // showMessage('startaddress='+inttohex(startaddress,8));
      end;

    finally
      cleanModuleList(list);
      list.free;
    end;

  end;

  wrongcount:=0;
  notwrong:=0;

  branchtreeCS.enter;

  try

    n:=branchtree.FindLowest;
    if n<>nil then
    begin


      d:=PBranchdata(n.Data);
      while d<>nil do
      begin
        if not d.wrong then
        begin
          case f of
            0: if not d.isCalled then d.wrong:=true; //filter out routines that where not called
            1: if d.isCalled then d.wrong:=true;     //filter out routines that where called
            2: if iscall(d.lastFromAddress)=false then d.wrong:=true;
            3: if d.count<>countvalue then d.wrong:=true;
            4: if InRangeQ(d.toAddress, startaddress, stopaddress)=false then d.wrong:=true;
            5: if isret(d.lastFromAddress) then d.wrong:=true;
          end;

          if d.wrong then inc(wrongcount) else inc(notwrong);
        end;

        d.isCalled:=false;
        d:=d.Next;
      end;
    end;

  finally
    branchtreecs.Leave;
  end;

  lblLastfilterresult.caption:=format(rsUULastFilterResults, [wrongcount, notwrong]);
  beep;
end;

procedure TfrmUltimap.FilterClick(Sender: TObject);
var i,f: integer;
begin
  f:=(sender as tbutton).tag;


  ApplyFilter(f);
end;

procedure TfrmUltimap.FlusherTimer(Sender: TObject);
begin
  if btnStop.enabled then
    ultimap_flush;
end;

procedure TfrmUltimap.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var i: integer;
begin
  if btnStop.enabled then
    btnstop.click;

  for i:=0 to 2 do
    if FilterHotkey[i]<>nil then
      freeandnil(FilterHotkey[i]);


  CloseAction:=cafree;
  frmUltimap:=nil;
end;

procedure TfrmUltimap.FormCreate(Sender: TObject);
var x: TWindowPosArray;
  i: integer;
  kc: TKeyCombo;
  ne: TNotifyEvent;
begin
  filteroutnewentries:=false;
  edtWorkerCount.Text:=inttostr(GetCPUCount);
  label4.Caption:=inttostr(sizeof(TBTS));

  flushcs:=TCriticalSection.Create;

  setlength(x, 0);
  ne:=nil;

  userdefinedpos:=loadformposition(self,x);

  if length(x)>=3*6 then
  begin
    for i:=0 to 2 do
    begin
      if x[6*i+0]=1 then  //active hotkey
      begin
        case i of
          0: ne:=btnExecuted.onclick;
          1: ne:=btnNotExecuted.onclick;
          2: ne:=btnPause.onclick;
        end;

        kc[0]:=x[6*i+1];
        kc[1]:=x[6*i+2];
        kc[2]:=x[6*i+3];
        kc[3]:=x[6*i+4];
        kc[4]:=x[6*i+5];

        //register the hotkey
        FilterHotkey[i]:=TGenericHotkey.create(ne, kc)
      end;

    end;

  end;

end;

procedure TfrmUltimap.FormDestroy(Sender: TObject);
var x: array of integer;
  i: integer;
begin
  setlength(x, 3*6); //hk0active,k1,k2,k3,k4,k5,hk1active,k1,k2,k3,k4,k5, hk2active,k1,k2,k3,k4,k5
  for i:=0 to length(x)-1 do
    x[i]:=0;

  for i:=0 to 2 do
  begin
    if FilterHotkey[i]<>nil then
    begin
      x[6*i+0]:=1;
      x[6*i+1]:=FilterHotkey[i].keys[0];
      x[6*i+2]:=FilterHotkey[i].keys[1];
      x[6*i+3]:=FilterHotkey[i].keys[2];
      x[6*i+4]:=FilterHotkey[i].keys[3];
      x[6*i+5]:=FilterHotkey[i].keys[4];
    end;
  end;

  saveformposition(self,x);
end;

procedure TfrmUltimap.FormShow(Sender: TObject);
begin
  autosize:=false;
  constraints.MinWidth:=panel4.width+panel6.width;
  if not userdefinedpos then
    width:=constraints.MinWidth;


end;

procedure TfrmUltimap.Label7Click(Sender: TObject);
begin


end;

procedure TfrmUltimap.ListView1Data(Sender: TObject; Item: TListItem);
begin
  item.caption:=symhandler.getNameFromAddress(validlist[item.Index].toAddress, true, true);
  item.SubItems.Add(symhandler.getNameFromAddress(validlist[item.Index].lastFromAddress, true, true));
  item.SubItems.Add(IntToStr(validlist[item.Index].count));

{$ifdef predictlog}
  if validlist[item.Index].predicted then
    item.subitems.add('X');
{$endif}

end;

procedure TfrmUltimap.ListView1DblClick(Sender: TObject);
var x: integer;
begin
  if listview1.selected<>nil then
  begin
    x:=listview1.ScreenToClient(mouse.CursorPos).x;
    if x>listview1.Column[0].Width then
      memorybrowser.disassemblerview.SelectedAddress:=validlist[listview1.selected.Index].lastFromAddress
    else
      memorybrowser.disassemblerview.SelectedAddress:=validlist[listview1.selected.Index].toAddress;

    if memorybrowser.visible=false then
      memorybrowser.show;
  end;
end;

procedure TfrmUltimap.MenuItem1Click(Sender: TObject);
var
  i: integer;
  p: tpoint;
  a,a2: ptruint;
  size: integer;
begin
  p:=PopupMenu1.PopupPoint;
  for i:=0 to listview1.Items.count-1 do
  begin
    if listview1.Items[i].Selected then
    begin
      if p.x>listview1.Column[0].Width then
        a:=validlist[i].lastFromAddress
      else
        a:=validlist[i].toAddress;

      a2:=a;
      disassemble(a2);

      advancedoptions.AddToCodeList(a, a2-a,false,true);
    end;
  end;
end;

procedure TfrmUltimap.miSetHotkeyClick(Sender: TObject);
var f: TfrmHotkeyEx;
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

procedure TfrmUltimap.MenuItem2Click(Sender: TObject);
begin
  flush;
  listview1.Refresh;
end;

procedure TfrmUltimap.miRemoveHotkeyClick(Sender: TObject);
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

procedure TfrmUltimap.pmSetHotkeyPopup(Sender: TObject);
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

procedure TfrmUltimap.Timer1Timer(Sender: TObject);
begin
  label3.caption:=IntToStr(TotalBranches);
  listview1.Refresh;
end;

procedure TfrmUltimap.flush;
var i: integer;
begin
  if workercount=0 then exit;

  isFlushing:=false;



  //reset
  flushcs.enter;
  try
    cpulist:=[];
    ResetEvent(hashandled);
    workdone:=0;
    isFlushing:=true;
  finally
    flushcs.leave;
  end;


  ultimap_flush; //trigger all cpu cores to flush their data to the workers
  WaitForSingleObject(hashandled, 5000);

  //reset
  isFlushing:=false;

  //flush twice (it's a big turd)
  flushcs.enter;
  try
    cpulist:=[];
    ResetEvent(hashandled);
    workdone:=0;
    isFlushing:=true;
  finally
    flushcs.leave;
  end;
  ultimap_flush;
  WaitForSingleObject(hashandled, 5000);


  //done flushing
  isFlushing:=false;  //no more wasted cycles checking the critical section

end;

initialization
//{$i frmUltimapUnit.lrs}

end.

