unit luafile;

{$mode delphi}

interface

uses
  Classes, SysUtils, DOM, zstream, math, custombase85{ascii85};

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

  useascii85: boolean;
{  a85: TASCII85DecoderStream;
  a85source: Tstringstream; }
  a: TDOMNode;
begin
  name:=node.NodeName;
  filedata:=TMemorystream.create;

  s:=node.TextContent;

  useascii85:=false;

  if node.HasAttributes then
  begin
    a:=node.Attributes.GetNamedItem('Encoding');
    useascii85:=(a<>nil) and (a.TextContent='Ascii85');
  end;


  if useascii85 then
  begin
    size:=(length(s) div 5)*4+(length(s) mod 5);
    maxsize:=max(65536,size);
    getmem(b, size);
    size:=Base85ToBin(pchar(s), b);
    {
    a85source:=TStringStream.Create(s);
    a85:=TASCII85DecoderStream.Create(a85source);

    size:=a85source.Size*5 div 4;
    maxsize:=max(65536,size); //64KB or the required size if that's bigger

    getmem(b, size);
    read:=a85.Read(b^, size);
    size:=read;

    a85.free;
    a85source.free; }
  end
  else
  begin
    size:=length(s) div 2;
    maxsize:=max(65536,size); //64KB or the required size if that's bigger

    getmem(b, size);
    HexToBin(pchar(s), b, size);
  end;




  try
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
 { a85: TASCII85EncoderStream;
  a85buffer: TStringStream;   }

  n: TDOMNode;
  a: TDOMAttr;
  s: string;
begin
  outputastext:=nil;
  //compress the file
  m:=tmemorystream.create;
  c:=Tcompressionstream.create(clmax, m, true);
  c.write(filedata.Memory^, filedata.size);
  c.free;


  //convert the compressed file to an ascii85 sring
{  a85buffer:=TStringStream.create('');
  a85:=TASCII85EncoderStream.Create(a85buffer);
  a85.Write(m.memory^, m.size);
  a85.Free;

  s:=a85buffer.DataString;
  a85buffer.free;    }


  getmem(outputastext, (m.size div 4) * 5 + 5 );
  BinToBase85(pchar(m.memory), outputastext, m.size);

  doc:=node.OwnerDocument;
  n:=Node.AppendChild(doc.CreateElement(name));
  n.TextContent:=outputastext;


  a:=doc.CreateAttribute('Encoding');
  a.TextContent:='Ascii85';
  n.Attributes.SetNamedItem(a);

  freemem(outputastext);
  m.free;


  {
  //convert the compressed file to a hexstring
  getmem(outputastext, m.size*2+1);
  try
    BinToHex(pchar(m.Memory), outputastext, m.Size);

    outputastext[m.size*2]:=#0; //add a 0 terminator


  finally
    freemem(outputastext);
    if m<>nil then
      m.free;
  end;  }
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

