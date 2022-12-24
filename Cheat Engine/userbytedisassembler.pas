unit userbytedisassembler;
{
Lets the user input bytes and those will then get disassembled
}

{$mode ObjFPC}{$H+}

interface

uses
  {$ifdef darwin}
  macport,
  {$else}
  windows,
  {$endif}
  Classes, SysUtils, disassembler,math;

type
  TUserByteDisassembler=class(TDisassembler)
  private
    bytes: pointer;
    bytecount: integer;
  protected
    function readMemory(address: ptruint; destination: pointer; size: integer): integer; override;
  public
    procedure setBytes(b: pointer; size: integer);
    destructor destroy; override;
  end;


implementation

function TUserByteDisassembler.readMemory(address: ptruint; destination: pointer; size: integer): integer;
begin
  if bytes=nil then exit(0);

  result:=min(size, bytecount);
  CopyMemory(destination, bytes, result);
end;

procedure TUserByteDisassembler.setBytes(b: pointer; size: integer);
begin
  bytecount:=size;

  if bytes<>nil then FreeMemAndNil(bytes);

  if size>0 then
  begin
    getmem(bytes, size);
    copymemory(bytes,b,size);
  end;
end;

destructor TUserByteDisassembler.destroy;
begin
  if bytes<>nil then
    freememandnil(bytes);

  inherited destroy;
end;

end.

