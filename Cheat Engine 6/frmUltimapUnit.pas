unit frmUltimapUnit;

{$mode delphi}

interface

uses
  windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DBK32functions, NewKernelHandler, cefuncproc, AvgLvlTree, ExtCtrls, ComCtrls,
  math,  symbolhandler, maps, disassembler, multicpuexecution, syncobjs;



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
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    cbLogToFile: TRadioButton;
    cbParseData: TRadioButton;
    cbRing0: TCheckBox;
    cbRing1Plus: TCheckBox;
    cbConditional: TCheckBox;
    cbNearRelCall: TCheckBox;
    cbNearIndCall: TCheckBox;
    cbNearRets: TCheckBox;
    cbNearIndirectJump: TCheckBox;
    cbNearRelativeJumps: TCheckBox;
    cbFarBranches: TCheckBox;
    Edit1: TEdit;
    edtBufSize: TEdit;
    edtFilename: TEdit;
    edtWorkerCount: TEdit;
    gbSelective: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ListView1: TListView;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure FilterClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure ListView1DblClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    branchtree: TAvgLvlTree;
    branchtreeCS: syncobjs.TCriticalSection;
    workers: Array of TUltimap_DataHandlerThread;

    validlist: array of PBranchdata;

    callTable: TMap;
    iscalldisassembler: TDisassembler;

    function branchcompare(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
    procedure ApplyFilter(f: integer);
    function iscall(address: ptruint): boolean;
    procedure setSelectiveMsr;
  public
    { public declarations }
  end; 

var
  frmUltimap: TfrmUltimap;

  TotalBranches: system.dword;

implementation

{$R *.lfm}

uses MemoryBrowserFormUnit;

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
end;

{ TfrmUltimap }

function TfrmUltimap.branchcompare(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
begin
  //used to sort the binary tree
  result:=CompareValue(pbranchdata(Data1).toAddress, pbranchdata(Data2).toAddress);
end;

function setSelectiveMsrForEachCpu(value: pointer): BOOL; stdcall;
begin
  writemsr($1c8, dword(value));
  result:=true;
end;

procedure TfrmUltimap.setSelectiveMsr;
var value: dword;
begin

  if gbSelective.Enabled then
  begin
    value:=0;
    if not cbRing0.checked then value:=value or (1 shl 0);
    if not cbRing1Plus.checked then value:=value or (1 shl 1);
    if not cbConditional.checked then value:=value or (1 shl 2);
    if not cbNearRelCall.checked then value:=value or (1 shl 3);
    if not cbNearIndCall.checked then value:=value or (1 shl 4);
    if not cbNearRets.checked then value:=value or (1 shl 5);
    if not cbNearIndirectJump.checked then value:=value or (1 shl 6);
    if not cbNearRelativeJumps.checked then value:=value or (1 shl 7);
    if not cbFarBranches.checked then value:=value or (1 shl 8);

    foreachcpu(setSelectiveMsrForEachCpu,pointer(value));

  end;
end;

procedure TfrmUltimap.Button1Click(Sender: TObject);
var
  cr3: qword;
  bufsize: dword;
  filename: widestring;
  workercount: integer;
  i: integer;
begin
  {$ifdef cpu32}
  if Is64bitOS then raise exception.create('Please run the 64-bit version of Cheat Engine to make use of this feature');
  {$endif}


  LoadDBK32;



  GetCR3(processhandle, cr3);
  bufsize:=strtoint(edtBufSize.text);
  filename:=edtFilename.text;
  workercount:=strtoint(edtWorkerCount.text);

  if bufsize<2000 then
    raise exception.create('This function needs at least 200 bytes for the header of the buffer');

  if workercount>64 then
    raise exception.create('The maximum number of workers is 64');


  setlength(workers, workercount);

  branchtree:=TAvgLvlTree.CreateObjectCompare(branchcompare);
  branchtreeCS:=TCriticalSection.Create;

  setSelectiveMSR;

  if ultimap(cr3, (1 shl 6) or (1 shl 7) or (1 shl 9) or (1 shl 8), bufsize, false, pwidechar(filename), workercount) then
  begin
    for i:=0 to workercount-1 do
    begin
      workers[i]:=TUltimap_DataHandlerThread.Create(true);
      workers[i].branchtree:=branchtree;
      workers[i].branchtreeCS:=branchtreeCS;
      workers[i].Start;
    end;
  end;
end;

procedure TfrmUltimap.Button2Click(Sender: TObject);
var i: integer;
begin
  for i:=0 to length(workers)-1 do
    workers[i].Terminate;

  for i:=0 to length(workers)-1 do
    workers[i].WaitFor;


  ultimap_disable();
end;

procedure TfrmUltimap.Button5Click(Sender: TObject);
//generate a pointerlist with valid entries, then set the count to the number of not wrong entries
var n: TAvgLvlTreeNode;
  d: PBranchdata;
  count: integer;
  maxvalidlist: integer;
begin
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
  n:=branchtree.FindLowest;

  d:=PBranchdata(n.Data);
  while d<>nil do
  begin
    d.wrong:=false;
    d:=d.Next;
  end;

end;

procedure TfrmUltimap.Button9Click(Sender: TObject);
begin

end;

function TfrmUltimap.iscall(address: ptruint): boolean;
var iscall: boolean;
  x: string;
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
    iscalldisassembler.disassemble(address, x);
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
begin
  ultimap_flush;

  if f=3 then
    countvalue:=strtoint(edit1.text);

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
        2: if not iscall(d.lastFromAddress) then d.wrong:=true;
        3: if d.count<>countvalue then d.wrong:=true;
      end;

      if d.wrong then inc(wrongcount) else inc(notwrong);
    end;

    d.isCalled:=false;
    d:=d.Next;
  end;

  showmessage('filtered out '+inttostr(wrongcount)+' left:'+inttostr(notwrong));
end;

procedure TfrmUltimap.FilterClick(Sender: TObject);
var i,f: integer;
begin
  f:=(sender as tbutton).tag;


  ApplyFilter(f);
end;

procedure TfrmUltimap.FormCreate(Sender: TObject);
begin
  edtWorkerCount.Text:=inttostr(GetCPUCount);
  label4.Caption:=inttostr(sizeof(TBTS));

  try
    readMSR($1c8);
    gbSelective.enabled:=true;
    cbConditional.checked:=false;
    cbNearRelCall.checked:=false;
    cbNearIndCall.checked:=false;
    cbNearRets.checked:=false;
    cbNearIndirectJump.checked:=false;
    cbNearRelativeJumps.checked:=false;
  except
  end;
end;

procedure TfrmUltimap.ListView1Data(Sender: TObject; Item: TListItem);
begin
  item.caption:=symhandler.getNameFromAddress(validlist[item.Index].toAddress, true, true);
  item.SubItems.Add(symhandler.getNameFromAddress(validlist[item.Index].lastFromAddress, true, true));
  item.SubItems.Add(IntToStr(validlist[item.Index].count));

end;

procedure TfrmUltimap.ListView1DblClick(Sender: TObject);
begin

  if listview1.selected<>nil then
  begin
    memorybrowser.disassemblerview.SelectedAddress:=validlist[listview1.selected.Index].toAddress;
    if memorybrowser.visible=false then
      memorybrowser.show;
  end;
end;

procedure TfrmUltimap.Timer1Timer(Sender: TObject);
begin
  label3.caption:=IntToStr(TotalBranches);
  listview1.Refresh;
end;

end.

