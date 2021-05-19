unit sourcecodehandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tcclib, AvgLvlTree;

type
  TSourceCodeInfoCollection=class //contains all the registered TSourceCodeInfo objects. (This 'should' keep up the speed when there are tons of C scripts compiled with source)
  private
    Collection: TAvgLvlTree;
    function RangeLookup(Tree: TAvgLvlTree; Data1, Data2: pointer): integer;
  public
    function getSourceCodeInfo(address: ptruint):TSourceCodeInfo;
    procedure addSourceCodeInfo(SourceCodeInfo: TSourceCodeInfo);
    procedure removeSourceCodeInfo(SourceCodeInfo: TSourceCodeInfo);

    constructor create;
  end;

var SourceCodeInfoCollection: TSourceCodeInfoCollection;


implementation

uses cefuncproc;

type
  TCollectionEntry=record
    startaddress: ptruint;
    stopaddress: ptruint;
    sourceCodeInfo: TSourceCodeInfo;
  end;
  PCollectionEntry=^TCollectionEntry;

function TSourceCodeInfoCollection.RangeLookup(Tree: TAvgLvlTree; Data1, Data2: pointer): integer;
var e1,e2: PCollectionEntry;
begin
  e1:=data1;
  e2:=data2;

  if InRangeQ(e1^.startaddress, e2^.startaddress, e2^.stopaddress+1) or
     InRangeQ(e2^.startaddress, e1^.startaddress, e1^.stopaddress+1) then
    exit(0);

  if e1^.startaddress<e2^.startaddress then exit(-1) else exit(1);
end;

procedure TSourceCodeInfoCollection.addSourceCodeInfo(SourceCodeInfo: TSourceCodeInfo);
var
  e: PCollectionEntry;
begin
  getmem(e,sizeof(TCollectionEntry));
  SourceCodeInfo.getRange(e^.startaddress, e^.stopaddress);
  e^.sourceCodeInfo:=SourceCodeInfo;

  collection.Add(e);
end;

procedure TSourceCodeInfoCollection.removeSourceCodeInfo(SourceCodeInfo: TSourceCodeInfo);
var
  searchkey: TCollectionEntry;
  n: TAvgLvlTreeNode;
begin
  SourceCodeInfo.getRange(searchkey.startaddress, searchkey.stopaddress);
  n:=collection.Find(@searchkey);

  if n<>nil then
  begin
    freemem(n.Data);
    collection.Delete(n);
  end;
end;

function TSourceCodeInfoCollection.getSourceCodeInfo(address: ptruint):TSourceCodeInfo;
var
  searchkey: TCollectionEntry;
  n: TAvgLvlTreeNode;
begin
  searchkey.startaddress:=address;
  searchkey.stopaddress:=address;

  n:=collection.Find(@searchkey);
  if n<>nil then
    result:=PCollectionEntry(n.Data)^.sourceCodeInfo
  else
    result:=nil;
end;



constructor TSourceCodeInfoCollection.create;
begin
  Collection:=TAvgLvlTree.CreateObjectCompare(@RangeLookup);
end;

end.

