unit frmUltimapUnit;

{$mode delphi}

interface

uses
  windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DBK32functions, NewKernelHandler, cefuncproc, AvgLvlTree, ExtCtrls, ComCtrls,
  math,  symbolhandler, maps, Menus, disassembler, multicpuexecution, syncobjs,
  genericHotkey, HotKeys, frmHotkeyExUnit, frmSelectionlistunit;



type
  PBranchdata=^TBranchData;
  TBranchData=record
    toAddress: PtrUInt;
    lastFromAddress: ptruint;
    //other stuff to come (e.g: triggered since last check)
    count: integer;
    isCalled: boolean;  //the worker sets this to true when called. The user check sets this to false
    wrong: boolean; //set to true if a user check has failed

    Previous: PBranchData;
    Next: PBranchData;
  end;


  TBTS=packed record
    LastBranchFrom: PtrUInt;
    LastBranchTo:   PtrUInt;
    Predicted:      PtrUInt;
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
    branchtree: TAvgLvlTree;
    branchtreeCS: TCriticalSection;
    procedure execute; override;
  end;


type

  { TfrmUltimap }

  TfrmUltimap = class(TForm)
    btnPause: TButton;
    btnStart: TButton;
    btnStop: TButton;
    Button1: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    cbLogToFile: TRadioButton;
    cbParseData: TRadioButton;
    Edit1: TEdit;
    edtBufSize: TEdit;
    edtFilename: TEdit;
    edtWorkerCount: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblLastfilterresult: TLabel;
    ListView1: TListView;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel5: TPanel;
    pmSetHotkey: TPopupMenu;
    Timer1: TTimer;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure FilterClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure ListView1DblClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    branchtree: TAvgLvlTree;
    branchtreeCS: syncobjs.TCriticalSection;
    workers: Array of TUltimap_DataHandlerThread;

    validlist: array of PBranchdata;

    callTable: TMap;
    iscalldisassembler: TDisassembler;


    FilterHotkey: array [0..3] of TGenericHotkey;
    paused: boolean;


    target_cr3: qword;
    bufsize: dword;
    function branchcompare(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
    procedure ApplyFilter(f: integer);
    function iscall(address: ptruint): boolean;
  public
    { public declarations }
  end; 

var
  frmUltimap: TfrmUltimap;

  TotalBranches: system.dword;

implementation

{$R *.lfm}

uses MemoryBrowserFormUnit, vmxfunctions;

{$ifdef cpu64}
const kernelbase=QWORD($800000000000);
{$else}
const kernelbase=DWORD($80000000);
{$endif}

{TUltimap_DataHandlerThread}
procedure TUltimap_DataHandlerThread.UpdateBranchData(branchdata: PBranchData; BTS: PBTS);
begin
  InterLockedIncrement(branchdata.count);

  branchdata.lastFromAddress:=BTS.LastBranchFrom;
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
        newbranchdata.wrong:=false;
        newbranchdata.lastFromAddress:=buf[i].LastBranchFrom;


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

  OutputDebugString('old='+inttostr(old)+' new='+inttostr(new));
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
        OutputDebugString('UltimapDataEvent.Address='+inttohex(UltimapDataEvent.Address,8));
        OutputDebugString('UltimapDataEvent.Size='+inttostr(UltimapDataEvent.Size));

        buf:=pointer(UltimapDataEvent.Address);
        try
          //OutputDebugString(format('1=%x 2=%x 3=%x 4=%x\n',[buf[0], buf[1], buf[2], buf[3]]));
          map(buf, UltimapDataEvent.Size div sizeof(TBTS));
        except
          OutputDebugString('Error during map');
        end;


      finally
        ultimap_continue(@UltimapDataEvent);
      end;

    end else OutputDebugString('No event');
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
  workercount: integer;
  i: integer;
begin

  {$ifdef cpu32}
  if Is64bitOS then raise exception.create('Please run the 64-bit version of Cheat Engine to make use of this feature');
  {$endif}

  TotalBranches:=0;

  if not loaddbvmifneeded then
    raise exception.create('This feature requires DBVM');


  GetCR3(processhandle, target_cr3);
  bufsize:=strtoint(edtBufSize.text);
  filename:=edtFilename.text;
  workercount:=strtoint(edtWorkerCount.text);

  if bufsize<2000 then
    raise exception.create('This function needs at least 200 bytes for the header of the buffer');

  if workercount>64 then
    raise exception.create('The maximum number of workers is 64');




  branchtree:=TAvgLvlTree.CreateObjectCompare(branchcompare);
  branchtreeCS:=TCriticalSection.Create;

  //setSelectiveMSR; Looks like I was wrong, this is Last Branch Record only, no data store

  if ultimap(target_cr3, (1 shl 6) or (1 shl 7) or (1 shl 9) or (1 shl 8), bufsize, false, pwidechar(filename), workercount) then
  begin
    setlength(workers, workercount);
    for i:=0 to workercount-1 do
    begin
      workers[i]:=TUltimap_DataHandlerThread.Create(true);
      workers[i].branchtree:=branchtree;
      workers[i].branchtreeCS:=branchtreeCS;
      workers[i].Start;
    end;
  end;


  paused:=false;
  btnPause.tag:=1;
  btnPause.caption:='Pause';
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


  paused:=false;

  btnStop.enabled:=false;
  btnpause.enabled:=false;
  btnStart.enabled:=true;

  errorbeep;
end;

function ultimap_pause(value: pointer): BOOL; stdcall;
begin
  dbvm_ultimap_pause; //stops setting the debugctl msr for the target process but does not reset logged the values
  result:=true;
end;

function ultimap_resume(value: pointer): BOOL; stdcall;
begin
  dbvm_ultimap_resume; //stops setting the debugctl msr for the target process but does not reset logged the values
  result:=true;
end;

procedure TfrmUltimap.btnPauseClick(Sender: TObject);
begin
  if not paused then
  begin
    foreachcpu(ultimap_pause,nil);
    paused:=true;
    btnPause.caption:='Resume';
  end
  else
  begin
    foreachcpu(ultimap_resume,nil);
    paused:=false;
    btnPause.caption:='Pause';
  end;
  beep;
end;

procedure TfrmUltimap.Button1Click(Sender: TObject);
begin
  ApplyFilter(4);
end;

procedure TfrmUltimap.Button3Click(Sender: TObject);
begin
  ApplyFilter(0);
end;

procedure TfrmUltimap.Button4Click(Sender: TObject);
begin
  ApplyFilter(1);
end;

procedure TfrmUltimap.Button7Click(Sender: TObject);
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

  listview1.Items.Count:=count;
end;

procedure TfrmUltimap.Button6Click(Sender: TObject);
var n: TAvgLvlTreeNode;
  d: PBranchdata;
  count: integer;
begin
  if branchtree<>nil then
  begin
    n:=branchtree.FindLowest;

    d:=PBranchdata(n.Data);
    while d<>nil do
    begin
      d.wrong:=false;
      d:=d.Next;
    end;

  end;

end;



procedure TfrmUltimap.Button9Click(Sender: TObject);
begin

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

  if length(workers)>0 then
    ultimap_flush; //there are workers to handle this

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

  n:=branchtree.FindLowest;
  wrongcount:=0;
  notwrong:=0;

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
      end;

      if d.wrong then inc(wrongcount) else inc(notwrong);
    end;

    d.isCalled:=false;
    d:=d.Next;
  end;

  lblLastfilterresult.caption:='Last filter results: filtered out '+inttostr(wrongcount)+' left:'+inttostr(notwrong);
  beep;
end;

procedure TfrmUltimap.FilterClick(Sender: TObject);
var i,f: integer;
begin
  f:=(sender as tbutton).tag;


  ApplyFilter(f);
end;

procedure TfrmUltimap.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if btnStop.enabled then
    btnstop.click;
end;

procedure TfrmUltimap.FormCreate(Sender: TObject);
begin
  edtWorkerCount.Text:=inttostr(GetCPUCount);
  label4.Caption:=inttostr(sizeof(TBTS));

end;

procedure TfrmUltimap.ListView1Data(Sender: TObject; Item: TListItem);
begin
  item.caption:=symhandler.getNameFromAddress(validlist[item.Index].toAddress, true, true);
  item.SubItems.Add(symhandler.getNameFromAddress(validlist[item.Index].lastFromAddress, true, true));
  item.SubItems.Add(IntToStr(validlist[item.Index].count));

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

    if f.showmodal = mrok then
    begin
      if FilterHotkey[i]=nil then
        FilterHotkey[i]:=TGenericHotkey.create(TButton(pmSetHotkey.PopupComponent).OnClick, f.newhotkey)
      else
        FilterHotkey[i].keys:=f.newhotkey;
    end;
  end;
end;

procedure TfrmUltimap.Timer1Timer(Sender: TObject);
begin
  label3.caption:=IntToStr(TotalBranches);
  listview1.Refresh;
end;

end.

