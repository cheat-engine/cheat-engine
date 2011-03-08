unit frmStringMapUnit;

{$mode delphi}

{
This unit will create a map that holds the addresses of all the strings in the game
}

interface

uses
  windows, Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, math, ComCtrls, ExtCtrls, StdCtrls,
  maps, cefuncproc, memfuncs, newkernelhandler, AvgLvlTree, bigmemallochandler;

type

  { TfrmStringMap }

  Pstringdata=^TStringdata;
  TStringData=record
    address: ptruint;
    stringsize: integer;
    unicode: boolean;
    previous: PStringData;
    next: PStringdata;
  end;


  TStringScan=class(tthread)
  private
    progressbar: TProgressBar;
    stringtree: TAvgLvlTree;
    bma: TBigMemoryAllocHandler;
    procedure AddString(address: ptruint; size: integer; unicode: boolean);

    procedure docleanup;
  public
    procedure execute; override;
    constructor create(suspended: boolean; progressbar: TProgressbar; stringtree: TAvgLvlTree; bma: TBigMemoryAllocHandler);
  end;

  TfrmStringMap = class(TForm)
    btnScan: TButton;
    btnFree: TButton;
    lblStringCount: TLabel;
    ListView1: TListView;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    procedure btnScanClick(Sender: TObject);
    procedure btnFreeClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
    scanner: TStringScan;
    bma: TBigMemoryAllocHandler;
  public
    { public declarations }
    stringtree: TAvgLvlTree;
    treememorymanager: TAvgLvlTreeNodeMemManager;
    function treecompare(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
    procedure cleanup;
    function isString(address: ptruint): boolean;
    function getString(address: ptruint): PStringData;
  end; 

var
  frmStringMap: TfrmStringMap;

implementation

{ TfrmStringMap }

resourcestring
  rsStop = 'Stop';
  rsGenerateStringMap = 'Generate string map';
  rsStringcount = 'Stringcount: %s';

procedure TStringscan.docleanup;
begin
  if frmstringmap<>nil then //how the fuck did this thread get started if it is nil ?
  begin
    progressbar.Position:=0;

    with frmstringmap do
    begin
      btnScan.Caption:=rsGenerateStringMap;
      lblStringCount.caption:=Format(rsStringcount, [inttostr(stringtree.Count)]);
      btnFree.enabled:=true;
    end;
  end;




end;

procedure TStringScan.AddString(address: ptruint; size: integer; unicode: boolean);
var e: Pstringdata;
  n: TAvgLvlTreeNode;
  prev, next: TAvgLvlTreeNode;

  s: pchar;
  ws: pwidechar;
  x: dword;
begin
  //replace this with saving the results to disk
  e:=bma.alloc(sizeof(TStringData));
  e.address:=address;
  e.stringsize:=size;
  e.unicode:=unicode;


  n:=stringtree.Add(e);
  prev:=stringtree.FindPrecessor(n);
  next:=stringtree.FindSuccessor(n);

  if prev=nil then
    e.previous:=nil
  else
  begin
    e.previous:=prev.Data;
    pstringdata(prev.data).next:=e;
  end;

  if next=nil then
    e.next:=nil
  else
  begin
    e.next:=next.Data;
    pstringdata(next.data).previous:=e;
  end;
end;

procedure TStringScan.execute;
var buf: PByteArray;
  maxbuf: integer;
  address: ptruint;

  total: ptruint;
  totalhandled: ptruint;

  mr: TMemoryRegions;
  i,j: integer;

  s: integer;
  currentbufsize: ptruint;
  currentpos: ptruint; //position in the current memory region
  x: dword;

  unicode: boolean;
  start: integer; //index where the first valid character is
begin
  //get memory regions
  buf:=nil;
  try
    total:=getallmemoryregions(mr);
    if total>0 then
    begin
      maxbuf:=0; //find the max size
      for i:=0 to length(mr)-1 do
        maxbuf:=max(mr[i].MemorySize, maxbuf);

      maxbuf:=min(maxbuf, 512*1024);

      getmem(buf, maxbuf);

    end;

    //cleanup
    for i:=0 to length(mr)-1 do
    begin
      if terminated then break;

      currentpos:=0;
      while (not terminated) and (currentpos<mr[i].MemorySize) do
      begin
        unicode:=false;
        s:=mr[i].MemorySize;
        currentbufsize:=min(s-currentpos, maxbuf);
        if ReadProcessMemory(processhandle, pointer(mr[i].BaseAddress+currentpos) , buf, currentbufsize,x) then
        begin
          //find and add the strings
          start:=-1;
          for j:=0 to currentbufsize-5 do
          begin

            if (buf[j]>=$20) and (buf[j]<=$7f) then
            begin
              if start=-1 then
                start:=j;
            end
            else
            begin
              if (buf[j]=0) and (start<>-1) and (buf[j-1]<>0) then //unicode ?
              begin
                unicode:=true;
                continue;
              end;

              if start<>-1 then
              begin
                //still here, so the previous character was 0 or the current char is invalid
                if ((not unicode) and (j-start>4)) or (unicode and (j-start>9)) then
                begin
                  //found something that resembles a string
                  AddString(mr[i].BaseAddress+currentpos+start,j-start-1, unicode);
                end;
              end;

              start:=-1;
              unicode:=false;
            end;
          end;

        end;

        inc(currentpos, currentbufsize);
        inc(totalhandled, currentbufsize);
        progressbar.Position:=trunc((totalhandled / total) * 100);
      end;
    end;
  finally
    if buf<>nil then
      freemem(buf);

    synchronize(docleanup);
  end;
end;

constructor TStringScan.create(suspended: boolean; progressbar: TProgressbar; stringtree: TAvgLvlTree; bma: TBigMemoryAllocHandler);
begin
  self.stringtree:=stringtree;
  self.progressbar:=progressbar;
  self.bma:=bma;

  progressbar.Position:=0;
  progressbar.max:=100;



  inherited create(suspended);
end;

function TfrmStringMap.treecompare(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
begin
  //used to sort the binary tree
  result:=CompareValue(pstringdata(Data1).address, pstringdata(Data2).address);
end;

//------------------------------------------------------------

procedure TfrmStringMap.cleanup;
var n:  TAvgLvlTreeNode;
  i: integer;
  t,p: Pstringdata;
begin
  if scanner<>nil then
  begin
    scanner.Terminate;
    scanner.WaitFor;
    scanner.free;
    scanner:=nil;
  end;

  if stringtree<>nil then
  begin
    stringtree.clear;
    stringtree.free;
    stringtree:=nil;
  end;

  if treememorymanager<>nil then
  begin
    treememorymanager.Free;
    treememorymanager:=nil;
  end;

  if bma<>nil then
  begin
    bma.free;
    bma:=nil;
  end;

end;

procedure TfrmStringMap.btnScanClick(Sender: TObject);
var mapIdType: TMapIdType;
begin
  if btnScan.caption=rsStop then
  begin
    Cleanup;
    caption:=rsGenerateStringMap;
  end
  else
  begin
    Cleanup;

    if ProcessHandler.is64Bit then
      mapIdType:=itu8 //unsigned 8 bytes
    else
      mapIdType:=itu4;

    stringtree:=TAvgLvlTree.CreateObjectCompare(treecompare);

    treememorymanager:=TAvgLvlTreeNodeMemManager.Create;
    treememorymanager.MinimumFreeNode:=102400;
    treememorymanager.MaximumFreeNodeRatio:=16;


    stringtree.NodeMemManager:=treememorymanager;



    bma:=TBigMemoryAllocHandler.create;

    btnScan.caption:=rsStop;

    scanner:=TStringScan.create(false, progressbar1, stringtree, bma);
  end;

end;

procedure TfrmStringMap.btnFreeClick(Sender: TObject);
begin
  cleanup;
end;

procedure TfrmStringMap.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

function TfrmStringMap.getString(address: ptruint): PStringData;
var
  k: TStringData;
  p: PStringData;
  n: TAvgLvlTreeNode;
begin
  result:=nil;
  if stringtree<>nil then
  begin
    k.address:=address;
    n:=stringtree.FindNearest(@k);

    if n<>nil then
    begin
      p:=PStringData(n.Data);
      while (p<>nil) and (p.address>k.address) do
        p:=p.previous;

      //it is a fact that p.address <= address
      if p<>nil then
      begin
        if p.address+p.stringsize>address then
          result:=p;
      end;

    end;

  end;
end;

function TfrmStringMap.isString(address: ptruint): boolean;
begin
  result:=getString(address)<>nil;
end;


initialization
  {$I frmStringMapUnit.lrs}

end.
