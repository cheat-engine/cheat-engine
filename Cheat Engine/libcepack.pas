unit libcepack;

{$mode ObjFPC}{$H+}

//library to pack/unpack files in the CE install distribution

interface

uses
  Classes, SysUtils;

procedure cepackfile(source, destination: string);
procedure ceunpackfile(source, destination: string; askpermission: boolean);

implementation

uses zstream, dialogs{$ifndef cepackexe}, controls{$endif};

{$ifndef cepackexe}
resourcestring
  askpermission='Extract %s to %s?';

function askPermissionToUnpack(source, destination: string): boolean;
begin
  result:=false;
  if MainThreadID=MainThreadID then //make sure it's not a new thread
    result:=MessageDlg(format(askpermission, [ExtractFileName(source), ExtractFileName(destination)]), mtWarning, [mbyes,mbno],0)=mryes;
end;
{$endif}

procedure cepackfile(source, destination: string);
var m: tmemorystream=nil;
  s: string;
  o: tmemorystream=nil;
  oz: Tcompressionstream=nil;
begin
  m:=TMemoryStream.Create;
  m.LoadFromFile(source);
  try
    o:=tmemorystream.create;
    s:='CEPACK';
    o.WriteBuffer(s[1],length(s));
    o.WriteDWord(m.size);
    oz:=Tcompressionstream.create(clmax, o,true);
    oz.CopyFrom(m,0);
    oz.flush;

    o.SaveToFile(destination);
  finally
    if oz<>nil then
      freeandnil(oz);

    if o<>nil then
      freeandnil(o);

    if m<>nil then
      freeandnil(m);
  end;
end;

procedure ceunpackfile(source, destination: string; askpermission: boolean);
var
  i: tmemorystream=nil;
  iz: Tdecompressionstream=nil;

  o: tmemorystream=nil;
  header: pchar=nil;
  size: dword;
begin
  {$ifndef cepackexe}
  if askpermission then
  begin
    if not askPermissionToUnpack(source,destination) then
      raise exception.create('Permission denied');
  end;
  {$endif}

  i:=TMemoryStream.Create;
  try
    i.LoadFromFile(source);

    header:=nil;
    getmem(header,7);
    i.position:=0;
    i.ReadBuffer(header^,6);
    header[6]:=#0;
    if header='CEPACK' then
    begin
      size:=i.ReadDWord;
      iz:=Tdecompressionstream.create(i,true);

      o:=tmemorystream.create;
      o.CopyFrom(iz,size);
      o.SaveToFile(destination);
      freeandnil(o);
    end
    else
      raise exception.create('invalid inputfile (wrong header)');

  finally
    if iz<>nil then
      freeandnil(iz);

    if header<>nil then
      freememandnil(header);

    if o<>nil then
      freeandnil(o);

    if i<>nil then
      freeandnil(i);
  end;
end;

end.

