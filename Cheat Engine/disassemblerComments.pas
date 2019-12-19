unit disassemblerComments;

{
this unit will contain the interface for the disassembler comments
}

{$mode delphi}

interface

uses
{$ifdef darwin}
  macport,
{$endif}
{$ifdef windows}
  windows,
{$endif}
  Classes, SysUtils, AvgLvlTree, math, cefuncproc, symbolhandler,
  symbolhandlerstructs, dom;

type TDisassemblerComments=class
  private
    commentstree: TAvgLvlTree;
    function getCount: integer;

    procedure setComment(address: ptruint; comment: string);
    function getComment(address: ptruint): string;

    procedure setHeader(address: ptruint; header: string);
    function getHeader(address: ptruint): string;
    procedure setInterpretableAddress(address: ptruint; s: string);
    function getInterpretableAddress(address: ptruint): string;
  public
    procedure clear;


    procedure saveToXMLNode(node: TDOMNode);
    procedure loadFromXMLNode(node: TDOMNode);

    function getCommentInRegion(address: ptruint; size: integer; out actualaddress: ptruint): string;
    procedure deleteAddress(address: ptruint);

    constructor create;
    destructor destroy; override;

    property count: integer read getCount;
    property comments[address: ptruint]: string read getComment write setComment; default;
    property commentHeader[address: ptruint]: string read getHeader write setHeader;
    property interpretableAddress[address: ptruint]: string read getInterpretableAddress write setInterpretableAddress; //for saving/loading
end;

var
  dassemblercomments: TDisassemblerComments;

procedure reinitializeDisassemblerComments;

implementation

type
  PCommentData=^TCommentData;
  TCommentData=record
    address: ptruint;
    interpretableAddress: pchar;
    comment: pchar;
    header: pchar;
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
  zeromemory(@search, sizeof(search));
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
  mi: TModuleInfo;
begin

  //check if this comment exists, and if not, add it

  ZeroMemory(@search, sizeof(search));
  search.address:=address;
  n:=commentstree.Find(@search);

  if n=nil then //create the node
  begin
    if comment='' then exit; //don't add it

    c:=getmem(sizeof(TCommentData));
    ZeroMemory(c, sizeof(TCommentData));

    c.address:=address;
    c.comment:=strnew(pchar(comment));
    c.header:=nil;

    if symhandler.getmodulebyaddress(address, mi) then
      c.interpretableAddress:=strnew(pchar('"'+mi.modulename+'"+'+inttohex(address-mi.baseaddress,1) ))
    else
      c.interpretableAddress:=strnew(pchar(inttohex(address,8) ));


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

    if (c.header=nil) and (comment='') then
    begin
      deleteAddress(address);
      exit;
    end;

    if c.comment<>nil then
      StrDispose(c.comment);

    if comment<>'' then
      c.comment:=strnew(pchar(comment))
    else
      c.comment:=nil;
  end;
end;

function TDisassemblerComments.getComment(address: ptruint): string;
var
  search: TCommentData;
  n: TAvgLvlTreeNode;
begin
  zeromemory(@search, sizeof(search));
  search.address:=address;
  n:=commentstree.Find(@search);
  if (n<>nil) and (n.Data<>nil) and (PCommentData(n.data).comment<>nil) then
    result:=PCommentData(n.data).comment
  else
    result:='';
end;

procedure TDisassemblerComments.setHeader(address: ptruint; header: string);
var search: TCommentData;
  c: PCommentData;
  n: TAvgLvlTreeNode;
  prev,next: TAvgLvlTreeNode;
  mi: TModuleInfo;
begin

  //check if this comment exists, and if not, add it
  ZeroMemory(@search, sizeof(search));
  search.address:=address;
  n:=commentstree.Find(@search);

  if n=nil then //create the node
  begin
    if header='' then exit;

    c:=getmem(sizeof(TCommentData));
    ZeroMemory(c, sizeof(TCommentData));

    c.address:=address;

    c.comment:=nil;
    c.header:=strnew(pchar(header));

    if symhandler.getmodulebyaddress(address, mi) then
      c.interpretableAddress:=strnew(pchar('"'+mi.modulename+'"+'+inttohex(address-mi.baseaddress,1) ))
    else
      c.interpretableAddress:=strnew(pchar(inttohex(address,8) ));


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

    if (c.comment=nil) and (header='') then
    begin
      deleteAddress(address);
      exit;
    end;


    if c.header<>nil then
      StrDispose(c.header);

    if header<>'' then
      c.header:=strnew(pchar(header))
    else
      c.header:=nil;
  end;
end;


function TDisassemblerComments.getHeader(address: ptruint): string;
var
  search: TCommentData;
  n: TAvgLvlTreeNode;
begin
  zeromemory(@search, sizeof(search));
  search.address:=address;
  n:=commentstree.Find(@search);
  if (n<>nil) and (n.data<>nil) and (PCommentData(n.data).header<>nil) then
    result:=PCommentData(n.data).header
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
      if c.comment<>nil then
        result:=c.comment
      else
        result:='';
    end;
  end;


end;

procedure TDisassemblerComments.deleteAddress(address: ptruint);
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

    if PCommentData(n.data).header<>nil then
      StrDispose(PCommentData(n.data).header);


    FreeMemAndNil(n.data);
    commentstree.Delete(n);
  end;
end;

procedure TDisassemblerComments.loadFromXMLNode(node: TDOMNode);
var i: integer;
  addressstring: string;
  address: ptruint;
  commentstring: string;
  headerstring: string;
  n,n2: TDomnode;
  e: boolean;
begin
  clear;




  for i:=0 to node.ChildNodes.Count-1 do
  begin
    commentstring:='';
    headerstring:='';

    n:=node.ChildNodes[i];

    if n.NodeName='DisassemblerComment' then
    begin

      n2:=n.FindNode('Address');
      if n2<>nil then
      begin
        addressstring:=n2.TextContent;

        n2:=n.FindNode('Comment');
        if n2<>nil then
          commentstring:=n2.TextContent;

        n2:=n.FindNode('Header');
        if n2<>nil then
          headerstring:=n2.TextContent;


        address:=symhandler.getAddressFromName(addressstring, false, e);
        if e then
          address:=i+1; //as a temp holder

        if commentstring<>'' then
          comments[address]:=commentstring;

        if headerstring<>'' then
          commentHeader[address]:=headerstring;

        interpretableAddress[address]:=addressstring;
      end;
    end;
  end;
end;

procedure TDisassemblerComments.saveToXMLNode(node: TDOMNode);
var n: TAvgLvlTreeNode;
  c: PCommentData;
  doc: TDOMDocument;

  commentnode: TDOMNode;
begin
  doc:=node.OwnerDocument;

  n:=commentstree.FindLowest;
  if n<>nil then
  begin
    c:=n.data;
    while c<>nil do
    begin
      commentnode:=node.AppendChild(doc.CreateElement('DisassemblerComment'));
      commentnode.AppendChild(doc.CreateElement('Address')).TextContent:=c.interpretableAddress;
      if c.comment<>nil then
        commentnode.AppendChild(doc.CreateElement('Comment')).TextContent:=c.comment;

      if c.header<>nil then
        commentnode.AppendChild(doc.CreateElement('Header')).TextContent:=c.header;

      c:=c.next;
    end;
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
    if n<>nil then
    begin
      c:=n.Data;
      while c<>nil do
      begin
        if c.comment<>nil then
        begin
          StrDispose(c.comment);
          c.comment:=nil;
        end;

        if c.header<>nil then
        begin
          StrDispose(c.header);
          c.header:=nil;
        end;

        if c.interpretableAddress<>nil then
        begin
          strdispose(c.interpretableAddress);
          c.interpretableAddress:=nil;
        end;

        prev:=c;
        c:=c.next;

        FreeMemAndNil(prev);
      end;
    end;

    commentstree.Clear;
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

  inherited destroy;
end;

procedure reinitializeDisassemblerComments;
var newcomments, olddassemblercomments: TDisassemblerComments;
  i: integer;
  n: TAvgLvlTreeNode;
  c: PCommentData;
  e: boolean;

  address: ptruint;
  comment, header: string;
begin
  newcomments:=TDisassemblerComments.create;
  n:=dassemblercomments.commentstree.FindLowest;
  if n<>nil then
  begin
    c:=n.data;
    while c<>nil do
    begin
      address:=symhandler.getAddressFromName(c.interpretableAddress, false, e);

      if e then //couldn't get resolved
        address:=c.address;



      if c.comment<>nil then
        comment:=c.comment
      else
        comment:='';


      if c.header<>nil then
        header:=c.header
      else
        header:='';

      newcomments.comments[address]:=comment;
      newcomments.commentHeader[address]:=header;

      newcomments.interpretableAddress[address]:=c.interpretableAddress;  //force the interpretable address in case it failed setting

      c:=c.next;
    end;
  end;

  olddassemblercomments:=dassemblercomments;
  dassemblercomments:=newcomments;
  olddassemblercomments.Free;


end;

initialization
  dassemblercomments:=TDisassemblerComments.create;

end.

