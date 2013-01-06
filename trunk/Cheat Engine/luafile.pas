unit luafile;

{$mode delphi}

interface

uses
  Classes, SysUtils, DOM, zstream, math;

type TLuafile=class
  private
    fname: string;
    filedata: TMemorystream;
  public

    constructor create(name: string; stream: TStream);
    constructor createFromXML(node: TDOMNode);
    procedure saveToXML(node: TDOMNode);
    destructor destroy; override;


  published
    property name: string read fname write fname;
    property stream: TMemoryStream read filedata;
  end;

implementation

constructor TLuafile.createFromXML(node: TDOMNode);
var s: string;
  b: pchar;
  m: TMemorystream;
  dc: Tdecompressionstream;
  maxsize, size: integer;
  read: integer;
begin
  name:=node.NodeName;
  filedata:=TMemorystream.create;

  s:=node.TextContent;

  size:=length(s) div 2;
  maxsize:=max(65536,size); //64KB or the required size if that's bigger

  getmem(b, maxsize);
  try
    HexToBin(pchar(s), b, size);

    m:=tmemorystream.create;
    m.WriteBuffer(b^, size);
    m.position:=0;
    dc:=Tdecompressionstream.create(m, true);

    //reuse the b buffer
    repeat
      read:=dc.read(b^, maxsize);
      filedata.WriteBuffer(b^, read);
    until read=0;

  finally
    freemem(b);
  end;
end;

procedure TLuafile.saveToXML(node: TDOMNode);
var
  outputastext: pchar;
  doc: TDOMDocument;

  m: TMemorystream;
  c: Tcompressionstream;
begin
  outputastext:=nil;
  //compress the file
  m:=tmemorystream.create;
  c:=Tcompressionstream.create(clmax, m, true);
  c.write(filedata.Memory^, filedata.size);
  c.free;

  //convert the compressed file to a hexstring
  getmem(outputastext, m.size*2+1);
  try
    BinToHex(pchar(m.Memory), outputastext, m.Size);

    outputastext[m.size*2]:=#0; //add a 0 terminator
    doc:=node.OwnerDocument;
    Node.AppendChild(doc.CreateElement(name)).TextContent:=outputastext;

  finally
    freemem(outputastext);
    if m<>nil then
      m.free;
  end;
end;

constructor TLuafile.create(name: string; stream: tstream);
begin
  self.name:=name;

  filedata:=tmemorystream.create;
  stream.position:=0;
  filedata.LoadFromStream(stream);
  filedata.position:=0;
end;

destructor TLuafile.destroy;
begin
  if filedata<>nil then
    filedata.free;

  inherited destroy;
end;

end.

