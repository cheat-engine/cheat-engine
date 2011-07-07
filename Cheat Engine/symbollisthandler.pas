unit SymbolListHandler;
{
This unit will keep two trees that link to a list of string to address information records for quick lookup
}

{$mode delphi}

interface

uses
  Classes, SysUtils, AvgLvlTree, math;

type
  PCESymbolInfo=^TCESymbolInfo;
  TCESymbolInfo=record
    s: pchar; //lowercase string for searching
    originalstring: pchar;
    address: qword;
    size: integer;
    previous: PCESymbolInfo;
    next: PCESymbolInfo;
  end;



  TSymbolListHandler=class
  private
    AddressToString: TAvgLvlTree;
    StringToAddress: TAvgLvlTree;
    function A2SCheck(Tree: TAvgLvlTree; Data1, Data2: pointer): integer;
    function S2ACheck(Tree: TAvgLvlTree; Data1, Data2: pointer): integer;
  public
    constructor create;
    destructor destroy; override;
    function AddString(s: string; address: ptruint; size: integer; skipaddresstostringlookup: boolean=false): PCESymbolInfo;
    function FindAddress(address: qword): PCESymbolInfo;
    function FindString(s: string): PCESymbolInfo;
    procedure clear;
  end;


implementation

uses CEFuncProc;

function TSymbolListHandler.FindAddress(address: qword): PCESymbolInfo;
var search: TCESymbolInfo;
  x: PCESymbolInfo;
  z: TAvgLvlTreeNode;
begin
  //keep in mind of duplicates
  result:=nil;
  search.address:=address;;

  z:=AddressToString.FindNearest(@search);

  if z<>nil then
  begin
    //check if it's a match, and if not, check if it's too big or too small

    x:=PCESymbolInfo(z.data);
    if x.address=address then
    begin
      result:=x;
      exit;
    end
    else
    if x.address<address then
    begin
      //if too small, check if it fits inside, else try the next one untill x.address>address or x=nil
      while (x<>nil) and (x.address<=address) do
      begin
        if InRangeQ(address, x.address, x.address+x.size) then
        begin
          result:=x;
          exit;
        end;

        //still here so not valid
        x:=x.next;
      end;
    end
    else
    begin
      //if too big, check the previous one, until x.address+x.size < address or x=nil
      while (x<>nil) and (x.address+x.size>address) do
      begin
        if InRangeQ(address, x.address, x.address+x.size) then
        begin
          result:=x;
          exit;
        end;

        //still here so not valid
        x:=x.previous;
      end;
    end;


  end;
end;

function TSymbolListHandler.FindString(s: string): PCESymbolInfo;
var x: TCESymbolInfo;
  z: TAvgLvlTreeNode;
begin
  s:=lowercase(s);
  x.s:=pchar(s);
  z:=StringToAddress.Find(@x);
  if z<>nil then
    result:=z.data
  else
    result:=nil;
end;

function TSymbolListHandler.AddString(s: string; address: ptruint; size: integer; skipaddresstostringlookup: boolean=false): PCESymbolInfo;
var new: PCESymbolInfo;
  n: TAvgLvlTreeNode;
  prev, next: TAvgLvlTreeNode;
begin
  new:=getmem(sizeof(TCESymbolInfo));
  new.originalstring:=strnew(pchar(s));
  new.s:=strnew(pchar(lowercase(s)));
  new.address:=address;
  new.size:=size;

  if not skipaddresstostringlookup then
  begin
    n:=AddressToString.Add(new);
    prev:=AddressToString.FindPrecessor(n);
    next:=AddressToString.FindSuccessor(n);

    if prev=nil then
      new.previous:=nil
    else
    begin
      new.previous:=prev.Data;
      PCESymbolInfo(prev.data).next:=new;
    end;

    if next=nil then
      new.next:=nil
    else
    begin
      new.next:=next.Data;
      PCESymbolInfo(next.data).previous:=new;
    end;
  end;

  StringToAddress.Add(new);
  result:=new;
end;

function TSymbolListHandler.A2SCheck(Tree: TAvgLvlTree; Data1, Data2: pointer): integer;
begin
  result:=comparevalue(PCESymbolInfo(data1).address, PCESymbolInfo(data2).address);

end;

function TSymbolListHandler.S2ACheck(Tree: TAvgLvlTree; Data1, Data2: pointer): integer;
begin
  result:=CompareStr(PCESymbolInfo(data1).s,PCESymbolInfo(data2).s);
end;

procedure TSymbolListHandler.clear;
var x: TAvgLvlTreeNode;
  d:PCESymbolInfo;
begin
  if AddressToString<>nil then
  begin
    x:=AddressToString.FindLowest;
    while x<>nil do
    begin
      d:=PCESymbolInfo(x.Data);

      if d.originalstring<>nil then
        StrDispose(d.originalstring);

      if d.s<>nil then
        StrDispose(d.s);

      freemem(d);
      x:=AddressToString.FindSuccessor(x);
    end;

    AddressToString.Clear;
  end;

  if StringToAddress<>nil then
    StringToAddress.Clear;
end;

constructor TSymbolListHandler.create;
begin
  inherited create;
  AddressToString:=TAvgLvlTree.CreateObjectCompare(A2SCheck);
  StringToAddress:=TAvgLvlTree.CreateObjectCompare(S2ACheck);
end;

destructor TSymbolListHandler.destroy;
begin
  clear;
  if AddressToString<>nil then
    AddressToString.Free;

  if StringToAddress<>nil then
    StringToAddress.Free;

  inherited destroy;
end;

end.

