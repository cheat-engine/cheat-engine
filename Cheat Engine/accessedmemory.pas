unit AccessedMemory;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef windows}windows, {$endif}Classes, SysUtils, FileUtil, laz.VirtualTrees, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, Menus, ComCtrls, genericHotkey, DBK32functions,
  commonTypeDefs, newkernelhandler, betterControls,AvgLvlTree, Laz_AVL_Tree;

resourcestring
  rsAMError = 'Error';
  rsAMYouCantSaveAnEmptyList = 'You can''t save an empty list';

type

  { TfrmAccessedMemory }

  TfrmAccessedMemory = class(TForm)
    btnClearSmallSnapshot: TButton;
    btnClearSmallSnapshot1: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    famrImageList: TImageList;
    Label1: TLabel;
    lblLost: TLabel;
    Label3: TLabel;
    tReader: TTimer;
    vsResults: TLazVirtualStringTree;
    MenuItem4: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    procedure btnClearSmallSnapshot1Click(Sender: TObject);
    procedure btnClearSmallSnapshotClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Edit2KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);

    procedure startMonitor(sender: TObject);
    procedure stopMonitor(sender: TObject);
    procedure tReaderTimer(Sender: TObject);
    procedure vsResultsDblClick(Sender: TObject);
    procedure vsResultsExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var Allowed: Boolean);
    procedure vsResultsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);

  private
    { private declarations }
    startk, stopk: TKeyCombo;

    hkStart: TGenericHotkey;
    hkStop: TGenericHotkey;

    results: TIndexedAVLTree;
    {$ifdef windows}
    watchinfo: PPSAPI_WS_WATCH_INFORMATION;
    {$endif}
    watchinfosize: dword;

    flost: integer;
    procedure setLost(count: integer);
    property lost: integer read flost write setLost;
  public
    { public declarations }


  end;

var
  frmAccessedMemory: TfrmAccessedMemory;

implementation

{$R *.lfm}

uses ProcessHandlerUnit, CEFuncProc, math, symbolhandler, MemoryBrowserFormUnit;

{ TfrmAccessedMemory }

type
  TPListDescriptor=record
    address: ptruint;
    count: dword;
    list: PUintPtr;
  end;

  PPListDescriptor=^TPListDescriptor;

procedure TfrmAccessedMemory.setLost(count: integer);
begin
  flost:=count;
  lbllost.caption:='Lost: '+inttostr(count);
  if (flost<>0) and (lbllost.visible=false) then lbllost.visible:=true;
end;

function ResultCompare(Item1, Item2: Pointer): Integer;
begin
  result:=CompareValue(PPListDescriptor(Item1)^.address, PPListDescriptor(Item2)^.address);
end;

procedure TfrmAccessedMemory.startMonitor(sender: TObject);
begin
  {$ifdef windows}
  if results=nil then
    results:=TIndexedAVLTree.Create(@ResultCompare);

  if InitializeProcessForWsWatch(processhandle) then
  begin
    EmptyWorkingSet(processhandle);
    button3.enabled:=true;
    tReader.enabled:=true;
  end;
  {$endif}

end;

procedure TfrmAccessedMemory.stopMonitor(sender: TObject);
begin
  {$ifdef windows}
  treader.enabled:=false;
  tReaderTimer(treader);
  {$endif}
end;

procedure TfrmAccessedMemory.tReaderTimer(Sender: TObject);
var
  i,j: integer;
  listcount: integer;
  plist: PPListDescriptor;
  found: boolean;
  search: TPListDescriptor;
  n: TAVLTreeNode;
begin
  {$ifdef windows}
  if watchinfo=nil then
  begin
    watchinfosize:=64*1024*1024;
    getmem(watchinfo, watchinfosize);
  end;

  while GetWsChanges(processhandle, watchinfo, watchinfosize)=false do
  begin
    if getlasterror=ERROR_INSUFFICIENT_BUFFER then
    begin
      watchinfosize:=watchinfosize*2;
      ReAllocMem(watchinfo, watchinfosize);
    end
    else
      break;
  end;

  listcount:=watchinfosize div sizeof(PSAPI_WS_WATCH_INFORMATION);

  for i:=0 to listcount-1 do
  begin
    if watchinfo[i].FaultingPc=0 then
    begin
      lost:=lost+watchinfo[i].FaultingVa;
      break;
    end;

    watchinfo[i].FaultingVa:=watchinfo[i].FaultingVa and qword($fffffffffffff000);

    search.address:=watchinfo[i].FaultingVa;
    n:=results.Find(@search);
    if n=nil then
    begin
      getmem(plist, sizeof(TPListDescriptor));

      plist^.address:=watchinfo[i].FaultingVa and qword($fffffffffffff000);
      plist^.Count:=1;
      plist^.list:=getmem(sizeof(pointer));
      plist^.list[0]:=watchinfo[i].FaultingPC;

      results.Add(plist);
      vsResults.AddChild(nil);
    end
    else
    begin
      found:=false;
      plist:=n.Data;
      for j:=0 to plist^.count-1 do
      begin
        if plist^.list[j]=watchinfo[i].FaultingPc then
        begin
          found:=true;
          break;
        end;
      end;

      if not found then
      begin
        inc(plist^.count);
        ReAllocMem(plist^.list, plist^.count*sizeof(pointer));
        plist^.list[plist^.count-1]:=watchinfo[i].FaultingPc;
      end;
    end;
  end;
  {$endif}
end;

procedure TfrmAccessedMemory.vsResultsDblClick(Sender: TObject);
begin
  if vsResults.FocusedNode<>nil then
    MemoryBrowser.disassemblerview.SelectedAddress:=symhandler.getAddressFromName(vsResults.Text[vsResults.FocusedNode,1]);
end;

procedure TfrmAccessedMemory.vsResultsExpanding(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var Allowed: Boolean);
var d: PPListDescriptor;
begin
  if vsResults.GetNodeLevel(node)=0 then
  begin
    d:=results[Node^.index];
    allowed:=d^.count>1;

    vsResults.AddChild(node, nil);
  end;


end;

procedure TfrmAccessedMemory.vsResultsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var d: PPListDescriptor;
begin
  if vsResults.GetNodeLevel(node)=0 then
  begin
    d:=results[Node^.index];
    if column=0 then
      celltext:=d^.address.ToHexString(8)
    else
      celltext:=symhandler.getNameFromAddress(d^.list[0]);

    vsResults.HasChildren[node]:=d^.count>1;
  end
  else
  begin
    d:=results[vsResults.NodeParent[node]^.Index];
    if column=0 then
      celltext:=''
    else
      celltext:=symhandler.getNameFromAddress(d^.list[node^.Index+1]);
  end;
end;

procedure TfrmAccessedMemory.FormCreate(Sender: TObject);
begin
  LoadFormPosition(self);


end;

procedure TfrmAccessedMemory.FormDestroy(Sender: TObject);
begin
  SaveFormPosition(self);
end;

procedure TfrmAccessedMemory.FormShow(Sender: TObject);
begin
  btnClearSmallSnapshot.autosize:=true;
  btnClearSmallSnapshot1.autosize:=true;
  btnClearSmallSnapshot.autosize:=false;
  btnClearSmallSnapshot1.autosize:=false;

  btnClearSmallSnapshot.ClientHeight:=canvas.TextHeight(btnClearSmallSnapshot.caption)+3;
  btnClearSmallSnapshot1.ClientHeight:=canvas.TextHeight(btnClearSmallSnapshot.caption)+3;

  button2.autosize:=true;
  button3.autosize:=true;
  button2.autosize:=false;
  button3.autosize:=false;

  if button2.width>button3.width then button3.width:=button2.width else button2.width:=button3.width;


end;

procedure TfrmAccessedMemory.ListView1Data(Sender: TObject; Item: TListItem);
begin

end;

procedure TfrmAccessedMemory.MenuItem2Click(Sender: TObject);
begin

end;

procedure TfrmAccessedMemory.MenuItem3Click(Sender: TObject);
begin

end;

procedure TfrmAccessedMemory.MenuItem4Click(Sender: TObject);
begin

end;

procedure TfrmAccessedMemory.Button2Click(Sender: TObject);
begin

end;

procedure TfrmAccessedMemory.Edit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if startk[4]=0 then
  begin
    for i:=0 to 4 do
      if startk[i]=0 then
      begin
        startk[i]:=key;
        break;
      end else
      if startk[i]=key then break;
  end;

  edit1.Text:=ConvertKeyComboToString(startk);
  button1.visible:=true;
end;

procedure TfrmAccessedMemory.Edit2KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if stopk[4]=0 then
  begin
    for i:=0 to 4 do
      if stopk[i]=0 then
      begin
        stopk[i]:=key;
        break;
      end else
      if startk[i]=key then break;
  end;

  edit2.Text:=ConvertKeyComboToString(stopk);
  button2.visible:=true;
end;


procedure TfrmAccessedMemory.Button1Click(Sender: TObject);
begin
  if hkstart<>nil then
    freeandnil(hkstart);

  if hkstop<>nil then
    freeandnil(hkstop);

  if startk[0]<>0 then
    hkStart:=TGenericHotkey.create(@startMonitor, startk);

  if stopk[0]<>0 then
    hkStop:=TGenericHotkey.create(@stopMonitor, stopk);


end;

procedure TfrmAccessedMemory.btnClearSmallSnapshotClick(Sender: TObject);
begin
  FillWord(startk, 5,0);
  edit1.text:='';
  edit1.SetFocus;
end;

procedure TfrmAccessedMemory.btnClearSmallSnapshot1Click(Sender: TObject);
begin
  FillWord(stopk, 5,0);
  edit2.text:='';
  edit2.SetFocus;
end;


end.

