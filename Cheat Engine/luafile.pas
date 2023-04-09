unit luafile;

{$mode delphi}

interface

uses
  Classes, SysUtils, DOM, zstream, math, custombase85, fgl, xmlutils;

type TLuafile=class
  private
    fname: string;
    filedata: TMemorystream;
    fdonotsave: boolean;
  public

    constructor create(name: string; stream: TStream);
    constructor createFromXML(node: TDOMNode);
    procedure saveToXML(node: TDOMNode);
    destructor destroy; override;


  published
    property name: string read fname write fname;
    property stream: TMemoryStream read filedata;
    property doNotSave: boolean read fdonotsave write fdonotsave;
  end;

  TLuaFileList =  TFPGList<TLuafile>;

implementation

constructor TLuafile.createFromXML(node: TDOMNode);
var s: string;
  b: pchar;
  m: TMemorystream;
  dc: Tdecompressionstream;
  maxsize, size: integer;
  read: integer;

  useascii85: boolean;
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

    a:=node.Attributes.GetNamedItem('Name');
    if a<>nil then
      name:=a.TextContent;
  end;

  if useascii85 then
  begin
    size:=(length(s) div 5)*4+(length(s) mod 5);
    maxsize:=max(65536,size);
    getmem(b, maxsize);
    size:=Base85ToBin(pchar(s), b);
  end
  else
  begin
    size:=length(s) div 2;
    maxsize:=max(65536,size); //64KB or the required size if that's bigger

    getmem(b, maxsize);
    HexToBin(pchar(s), b, size);
  end;

  try
    m:=tmemorystream.create;
    m.WriteBuffer(b^, size);
    m.position:=0;
    dc:=Tdecompressionstream.create(m, true);

    if useascii85 then //this ce version also added a filesize (This is why I usually don't recommend using svn builds for production work. Of coure, not many people using the svn made use of the file stuff)
    begin
      size:=dc.ReadDWord;
      FreeMemAndNil(b);
      getmem(b, size);
      read:=dc.read(b^, size);
      filedata.WriteBuffer(b^, read);
    end
    else
    begin
      //reuse the b buffer
      repeat
        read:=dc.read(b^, maxsize);
        filedata.WriteBuffer(b^, read);
      until read=0;
    end;

  finally
    FreeMemAndNil(b);
  end;
end;

procedure TLuafile.saveToXML(node: TDOMNode);
var
  outputastext: pchar;
  doc: TDOMDocument;

  m: TMemorystream;
  c: Tcompressionstream;

  n: TDOMNode;
  a: TDOMAttr;
  s: string;
  xmlname: boolean;
  i: integer;
  namecount: integer;
begin
  if donotsave then exit;

  outputastext:=nil;
  //compress the file
  m:=tmemorystream.create;
  c:=Tcompressionstream.create(clmax, m, true);

  c.WriteDWord(filedata.size);
  c.write(filedata.Memory^, filedata.size);
  c.free;


  //convert the compressed file to an ascii85 sring
  getmem(outputastext, (m.size div 4) * 5 + 5 );
  BinToBase85(pchar(m.memory), outputastext, m.size);

  doc:=node.OwnerDocument;

  n:=Node.AppendChild(doc.CreateElement('File'+node.ChildNodes.Count.ToString));

  n.TextContent:=outputastext;

  a:=doc.createAttribute('Name');
  a.TextContent:=name;
  n.Attributes.SetNamedItem(a);

  a:=doc.CreateAttribute('Encoding');
  a.TextContent:='Ascii85';
  n.Attributes.SetNamedItem(a);

  FreeMemAndNil(outputastext);
  freeandnil(m);
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

