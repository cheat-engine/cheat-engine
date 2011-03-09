unit frmStringPointerScanUnit;

{$mode delphi}

interface

uses
  windows, Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  cefuncproc, newkernelhandler, frmStringMapUnit, MemFuncs, AvgLvlTree, bigmemallochandler, math;

type

  { TfrmStringPointerScan }
  TPointerpath=array of dword;

type
  PMappedRegion=^TMappedRegion;
  TMappedRegion=record
    baseaddress: ptruint; //if a new section overlaps, append to this
    size: ptruint; //if a new section ovrlaps increase this
    previous: PMappedRegion;
    next: PMappedRegion;
  end;

  PPointerListEntry=^TPointerListEntry;
  TPointerListEntry=record
    address: ptruint;
    pointsto: ptruint;
    previous: PPointerListEntry;
    next: PPointerListEntry;
  end;


  TScanner=class(tthread)
  private
    baseaddress: ptruint;
    structsize: ptruint;
    maxlevel: ptruint;
    mappedregions: TAvgLvlTree;
    pointerlist: TAvgLvlTree;
    bma: TBigMemoryAllocHandler;

    count: integer;

    levelblock: array of pdwordarray;
    fillpointerblock: pdwordarray;
   // block64: array of Pint64Array;

    procedure handleBlock(blockaddress: ptruint; level: integer; path: TPointerpath);
    procedure addStringPath(level: integer; path: tpointerpath; stringsize: integer; unicode: boolean);

    procedure mapRegionIfNeeded(blockaddress: ptruint; size: integer);
    procedure fillPointers(base: ptruint; size: integer);
    function getFirstPointerEntry(base: ptruint): PPointerListEntry;
  public
    procedure execute; override;
    constructor create(baseaddress: ptruint; structsize: integer; maxlevel: integer; mappedregions,pointerlist: TAvgLvlTree; bma: TBigMemoryAllocHandler);
    destructor destroy; override;
  end;

  TfrmStringPointerScan = class(TForm)
    Button1: TButton;
    edtMaxLevel: TEdit;
    edtBase: TEdit;
    edtStructsize: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ListView1: TListView;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }

    mappedRegions: TAvgLvlTree; //holds the map of the regions that have been mapped

    pointerlist: TAvgLvlTree; //holds the pointers in the app of the mapped regions
    pointerlistMemManager: TAvgLvlTreeNodeMemManager;
    bma: TBigMemoryAllocHandler;

    scanner: TScanner;

    function mapCompare(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
    function pointerCompare(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
    procedure cleanup;
  public
    { public declarations }
  end;



var
  frmStringPointerScan: TfrmStringPointerScan;



implementation

{ TfrmStringPointerScan }

resourcestring
  rsGeneratingStringmap = 'Generating stringmap';
  rsGeneratedScanning = 'Generated. Scanning...';
  rsThisAddressIsNotAccessible = 'This address is not accessible';





//--------------TScanner---------------
function TScanner.getFirstPointerEntry(base: ptruint): PPointerListEntry;
var pe: TAvgLvlTreeNode;
  search: TPointerListEntry;
  r: PPointerListEntry;
begin
  r:=nil;
  zeromemory(@search, sizeof(search));
  search.address:=base;

  pe:=pointerlist.FindNearest(@search);

  if pe<>nil then
  begin
    r:=pe.data;
    while (r<>nil) and (r.address<base) do
      r:=r.next;

  end;

  result:=r;
end;

procedure TScanner.fillPointers(base: ptruint; size: integer);
var x: dword;
  i: integer;
  p: PPointerListEntry;
  pe, prev, next: TAvgLvlTreeNode;
begin
  if readprocessmemory(processhandle, pointer(base), fillpointerblock, size, x) then
  begin
    for i:=0 to (x div 4)-1 do
    begin
      if (fillpointerblock[i]>$10000) and ((fillpointerblock[i] mod 4)=0) and (isreadable(fillpointerblock[i])) then
      begin
//        p:=bma.alloc(sizeof(TPointerListEntry));
        p:=getmem(sizeof(TPointerListEntry));

       // ZeroMemory(p, sizeof(TPointerListEntry));
        p.address:=base+(i*4);
        p.pointsto:=fillpointerblock[i];

        {    debug code
        if pointerlist.Find(p)<>nil then
        begin
          freemem(p);
          continue;
        end; }

        pe:=pointerlist.Add(p);
        prev:=pointerlist.FindPrecessor(pe);
        next:=pointerlist.FindSuccessor(pe);

        if prev=nil then
          p.previous:=nil
        else
        begin
          p.previous:=prev.Data;
          PPointerListEntry(prev.data).next:=p;
        end;

        if next=nil then
          p.next:=nil
        else
        begin
          p.next:=next.Data;
          PPointerListEntry(next.data).previous:=p;
        end;


      end;
    end;
  end;
end;

procedure TScanner.mapRegionIfNeeded(blockaddress: ptruint; size: integer);
var search: TMappedRegion;
  result: TAvgLvlTreeNode;

   map, oldmap: PMappedRegion;

  prev, next: TAvgLvlTreeNode;

  size2: integer;
begin
  search.baseaddress:=blockaddress;
  result:=mappedregions.FindNearest(@search);


  if blockaddress=$54ebe0 then
  begin
    asm nop end
  end;


  if result<>nil then
    map:=result.data
  else
    map:=nil;

  if (map<>nil) and (not ((map.baseaddress<=blockaddress) and (map.baseaddress+map.size>=blockaddress))) then
  begin
    //if the found map isn't a good enough match then check the next and previous one
    if (map.next<>nil) and ((map.next.baseaddress<=blockaddress) and (map.next.baseaddress+map.next.size>=blockaddress)) then
      map:=map.next
    else
    if (map.previous<>nil) and ((map.previous.baseaddress<=blockaddress) and (map.previous.baseaddress+map.previous.size>=blockaddress)) then
      map:=map.previous
    else
      map:=nil;

  end;



  //if it's not in the range
 { if (map<>nil) and (not InRangeX(blockaddress, map.baseaddress, map.baseaddress+map.size)) then map:=nil;
   }

  if (map<>nil) then
  begin
    //this map can contain or add the extra bytes needed

    size2:=((blockaddress+size) - (map.baseaddress+map.size)); //size now contains the total bytes that need to appended to this map
    if size2>0 then
    begin
      inc(blockaddress, size-size2);
      size:=size2;

      if (map.next<>nil) and (map.next.baseaddress<blockaddress+size) then
      begin
        //adjust the size so it won't overlap with the next map
        size2:=map.next.baseaddress-(map.baseaddress+map.size);
      end;

      if size2>0 then
      begin
        fillPointers(map.baseaddress+map.size, size2);
        map.size:=map.size+size2;
        dec(size, size2);
      end;

      if (size>0) then //this map has been filled as much as it can, try the next one
        mapRegionIfNeeded(blockaddress, size);
    end; //else it's already fully mapped
  end
  else
  begin
    //create a new one
//    map:=bma.alloc(sizeof(TMappedRegion));
    map:=getmem(sizeof(TMappedRegion));

    //ZeroMemory(map, sizeof(TMappedRegion));
    map.baseaddress:=blockaddress;


    result:=mappedregions.Add(map);
    prev:=mappedregions.FindPrecessor(result);
    next:=mappedregions.FindSuccessor(result);

    if prev=nil then
      map.previous:=nil
    else
    begin
      map.previous:=prev.Data;
      PMappedRegion(prev.data).next:=map;
    end;

    if next=nil then
      map.next:=nil
    else
    begin
      map.next:=next.Data;
      PMappedRegion(next.data).previous:=map;
    end;

    map.size:=structsize;
    //adjust the size so it won't overlap with the next block
    if (map.next<>nil) and (map.next.baseaddress<map.baseaddress+map.size) then
      map.size:=map.next.baseaddress-map.baseaddress;

    fillPointers(map.baseaddress, map.size);

    if (map.next<>nil) and (map.next.baseaddress=map.baseaddress+map.size) then
    begin
      //merge (delte old one)
      result:=mappedregions.Find(map.next);
      if result<>nil then
      begin
        oldmap:=result.data;    //adjust the linked list
        map.next:=oldmap.next;
        if map.next<>nil then
          map.next.previous:=map;

        map.size:=map.size+oldmap.size;

        mappedregions.Remove(oldmap);
        freemem(oldmap);
      end;
    end;
  end;

end;

procedure TScanner.addStringPath(level: integer; path: tpointerpath; stringsize: integer; unicode: boolean);
begin
  inc(count);
end;

procedure TScanner.handleBlock(blockaddress: ptruint; level: integer; path: TPointerpath);
var
  x: dword;
  i: integer;

  newaddress: ptruint;

  p: Pstringdata;

  block: pdwordarray;

  pe: PPointerListEntry;
begin
  //check which strings can be found in this block
  p:=frmStringMap.findNearestString(blockaddress);
  while (p<>nil) and (p.address<blockaddress) do
    p:=p.next;

  if p<>nil then
  begin
    while (p<>nil) and (p.address<blockaddress+structsize) do       //found a string, add it
    begin
      path[level]:=p.address-blockaddress;
      addStringPath(level, path, p.stringsize, p.unicode);
      p:=p.next;
    end;
  end;

  if level>=maxlevel then exit; //max level reached, no need to scan for pointers in this block

  //still here, so go through this block looking for pointers

  //see if this block already has it's pointers mapped, if not, map them
  mapRegionIfNeeded(blockaddress, structsize);

  pe:=getFirstPointerEntry(blockaddress);
  while (pe<>nil) and (pe.address<blockaddress+structsize) do
  begin
    path[level]:=pe.address-blockaddress;
    handleBlock(pe.pointsto, level+1, path);
    pe:=pe.next;
  end;
end;

procedure TScanner.execute;
var pointerpath: TPointerpath;
  i: integer;
begin
  try
    setlength(pointerpath, maxlevel+1); //maxlevel=0 means 1 offset
    setlength(levelblock, maxlevel+1);
    for i:=0 to length(levelblock)-1 do
      getmem(levelblock[i], structsize); //one time alloc for each level so it can be reused without having to reallocate each recursion

    getmem(fillpointerblock, structsize);


    handleBlock(baseaddress, 0, pointerpath);

    messagebox(0,pchar(inttostr(count)),'ps',0);

  except
    on e: exception do
      messagebox(0,pchar('Exception:'+e.Message),'ps',0);
  end;
end;

constructor TScanner.create(baseaddress: ptruint; structsize: integer; maxlevel: integer; mappedregions,pointerlist: TAvgLvlTree; bma: TBigMemoryAllocHandler);
begin
  self.baseaddress:=baseaddress;
  self.structsize:=structsize;
  self.maxlevel:=maxlevel;
  self.mappedregions:=mappedregions;
  self.pointerlist:=pointerlist;
  self.bma:=bma;

  count:=0;

  inherited create(false); //let's get started...
end;

destructor TScanner.destroy;
var i: integer;
begin
  for i:=0 to length(levelblock)-1 do
    freemem(levelblock[i]);


  if fillpointerblock<>nil then
    freemem(fillpointerblock);

end;

//----------------------------

function TfrmStringPointerScan.mapCompare(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
begin
  result:=CompareValue(PMappedRegion(Data1).baseaddress, PMappedRegion(Data2).baseaddress);
end;

function TfrmStringPointerScan.pointerCompare(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
begin
  result:=CompareValue(PPointerListEntry(Data1).address, PPointerListEntry(Data2).address);
end;

procedure tfrmStringPointerScan.cleanup;
begin
  if bma<>nil then
    freeAndNil(bma);

  if pointerlist<>nil then
    freeandnil(pointerlist);

  if pointerlistMemManager<>nil then
    freeandnil(pointerlistMemManager);

  if pointerlist<>nil then
    freeandnil(pointerlist);

  if mappedRegions<>nil then
    freeandnil(mappedRegions);
end;

procedure TfrmStringPointerScan.Button1Click(Sender: TObject);
var baseaddress: ptruint;
  structsize: integer;
  maxlevel: integer;
begin
  cleanup;

  baseaddress:=strtoint('$'+edtBase.text);
  if not isreadable(baseaddress) then
    raise exception.create(rsThisAddressIsNotAccessible);

  structsize:=strtoint(edtStructsize.text);
  maxlevel:=strtoint(edtMaxLevel.text);

  if frmStringMap=nil then
    frmStringMap:=tfrmStringMap.Create(nil);


  mappedRegions:=TAvgLvlTree.CreateObjectCompare(mapCompare);
  pointerlist:=TAvgLvlTree.CreateObjectCompare(pointerCompare);
  //pointerlistMemManager:=TAvgLvlTreeNodeMemManager.Create;
//  pointerlist.NodeMemManager:=pointerlistMemManager;
//  pointerlistMemManager.MinimumFreeNode:=102400;
//  pointerlistMemManager.MaximumFreeNodeRatio:=32;

  bma:=TBigMemoryAllocHandler.create;


  frmstringmap.btnScan.click;

  label3.caption:=rsGeneratingStringmap;
  label3.Repaint;
  frmstringmap.scanner.WaitFor;

  label3.caption:=rsGeneratedScanning;

  //the stringmap has been generated

  scanner:=Tscanner.create(baseAddress, structsize, maxlevel, mappedRegions, pointerlist, bma);


end;

initialization
  {$I frmStringPointerScanUnit.lrs}

end.

