unit disassemblerComments;

{
this unit will contain the interface for the disassembler comments
}

{$mode delphi}

interface

uses
  Classes, SysUtils, AvgLvlTree, math, cefuncproc, symbolhandler;

type TDisassemblerComments=class
  private
    commentstree: TAvgLvlTree;
    function getCount: integer;
    procedure setComment(address: ptruint; comment: string);
    function getComment(address: ptruint): string;
    procedure setInterpretableAddress(address: ptruint; s: string);
    function getInterpretableAddress(address: ptruint): string;
  public
    procedure clear;

    function getCommentInRegion(address: ptruint; size: integer; out actualaddress: ptruint): string;
    procedure deleteComment(address: ptruint);

    constructor create;
    destructor destroy; override;

    property count: integer read getCount;
    property comments[address: ptruint]: string read getComment write setComment; default;
    property interpretableAddress[address: ptruint]: string read getInterpretableAddress write setInterpretableAddress; //for saving/loading
end;

var
  dassemblercomments: TDisassemblerComments;


implementation

type
  PCommentData=^TCommentData;
  TCommentData=record
    address: ptruint;
    interpretableAddress: pchar;
    comment: pchar;
    previous: PCommentData;
    next: PCommentData;
  end;

function TDisassemblerComments.getCount: integer;
begin
  result:=commentstree.Count;
end;

procedure TDisassemblerComments.setInterpretableAddress(address: ptruint; s: string);
var search: TCommentData;
  c: PCommentData;
  n: TAvgLvlTreeNode;
  prev,next: TAvgLvlTreeNode;
begin
  //check if this comment exists, and if not, add it
  search.address:=address;
  n:=commentstree.Find(@search);

  if n<>nil then
  begin
    c:=n.data;
    if c.interpretableAddress<>nil then
      StrDispose(c.interpretableAddress);

    c.interpretableAddress:=strnew(pchar(s));
  end;
end;

function TDisassemblerComments.getInterpretableAddress(address: ptruint): string;
var
  search: TCommentData;
  n: TAvgLvlTreeNode;
begin
  search.address:=address;
  n:=commentstree.Find(@search);
  if n<>nil then
    result:=PCommentData(n.data).interpretableAddress
  else
    result:='';
end;

procedure TDisassemblerComments.setComment(address: ptruint; comment: string);
var search: TCommentData;
  c: PCommentData;
  n: TAvgLvlTreeNode;
  prev,next: TAvgLvlTreeNode;
begin
  //check if this comment exists, and if not, add it
  search.address:=address;
  n:=commentstree.Find(@search);

  if n=nil then //create the node
  begin
    c:=getmem(sizeof(TCommentData));
    c.address:=address;
    c.comment:=strnew(pchar(comment));
    c.interpretableAddress:=strnew(pchar(symhandler.getNameFromAddress(address)));

    n:=commentstree.Add(c);

    prev:=commentstree.FindPrecessor(n);
    next:=commentstree.FindSuccessor(n);

    if prev=nil then
      c.previous:=nil
    else
    begin
      c.previous:=prev.Data;
      PCommentData(prev.data).next:=c;
    end;

    if next=nil then
      c.next:=nil
    else
    begin
      c.next:=next.Data;
      PCommentData(next.data).previous:=c;
    end;
  end
  else
  begin
    //update
    c:=n.data;
    if c.comment<>nil then
      StrDispose(c.comment);

    c.comment:=strnew(pchar(comment));
  end;
end;

function TDisassemblerComments.getComment(address: ptruint): string;
var
  search: TCommentData;
  n: TAvgLvlTreeNode;
begin
  search.address:=address;
  n:=commentstree.Find(@search);
  if n<>nil then
    result:=PCommentData(n.data).comment
  else
    result:='';
end;

function TDisassemblerComments.getCommentInRegion(address: ptruint; size: integer; out actualaddress: ptruint): string;
var
  search: TCommentData;
  n: TAvgLvlTreeNode;
  c: PCommentData;
begin
  result:='';
  actualaddress:=0;

  search.address:=address;
  n:=commentstree.FindNearest(@search);
  if n<>nil then
  begin
    c:=n.data;
    if c.address>=address+size then
      c:=c.previous;

    if (c<>nil) and (c.address<address) then
      c:=c.next;

    //now check if c fits in the region, if not, not found
    if InRangeX(c.address, address , address+size-1) then
    begin
      actualaddress:=c.address;
      result:=c.comment;
    end;
  end;


end;

procedure TDisassemblerComments.deleteComment(address: ptruint);
var
  search: TCommentData;
  n: TAvgLvlTreeNode;
begin
  search.address:=address;
  n:=commentstree.Find(@search);
  if n<>nil then
  begin
    if PCommentData(n.data).comment<>nil then
      StrDispose(PCommentData(n.data).comment);

    freemem(n.data);
    commentstree.Delete(n);
  end;
end;

function Compare(Item1, Item2: Pointer): Integer;
begin
  result:=comparevalue(PCommentData(item1).address, PCommentData(item2).address);
end;

procedure TDisassemblercomments.clear;
var
  c, prev: PCommentData;
  n: TAvgLvlTreeNode;
begin
  if commentstree<>nil then
  begin
    //free the comments and the comment data objects
    n:=commentstree.FindLowest;
    c:=n.Data;
    while c<>nil do
    begin
      if c.comment<>nil then
        StrDispose(c.comment);

      prev:=c;
      c:=c.next;

      freemem(prev);
    end;


  end;
end;

constructor TDisassemblerComments.create;
begin
  commentstree:=TAvgLvlTree.Create(Compare);
end;

destructor TDisassemblerComments.destroy;
begin
  clear;
  freeandnil(commentstree);
end;

initialization
  dassemblercomments:=TDisassemblerComments.create;

end.

