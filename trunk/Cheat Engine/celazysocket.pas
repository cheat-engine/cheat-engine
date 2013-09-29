unit CELazySocket;
{
Just some functions to make sockets easier
}

{$mode delphi}

interface

uses
  Classes, SysUtils, Sockets;

type
  TSocketException=class(Exception);

  TNetworkStream=class(TMemoryStream)
  private
  public
    function WriteToSocket(s: tsocket): integer;
    function ReadFromSocket(s: tsocket; readsize: integer): integer;
  end;


function send(socket: TSocket; buffer: pointer; size: integer): integer;
function receive(socket: TSocket; buffer: pointer; size: integer): integer;

//setup listener/connect ?

implementation

function TNetworkStream.WriteToSocket(s: tsocket): integer;
begin
  result:=send(s, memory, size);
end;

function TNetworkStream.ReadFromSocket(s: tsocket; readsize: integer): integer;
var buffer: pchar;
begin
  getmem(buffer, readsize);
  result:=receive(s, buffer, readsize);
  WriteBuffer(buffer^, result);
  freemem(buffer);
end;

function send(socket: TSocket; buffer: pointer; size: integer): integer;
var i: integer;
begin
  result:=0;
  while (result<size) do
  begin
    i:=fpsend(socket, pointer(ptruint(buffer)+result), size, 0);
    if i<=0 then
      raise TSocketException.Create('Send Error');

    inc(result, i);
  end;
end;

function receive(socket: TSocket; buffer: pointer; size: integer): integer;
var
  i: integer;
begin
  result:=0;
  while (result<size) do
  begin
    i:=fprecv(socket, pointer(ptruint(buffer)+result), size-result, 0);
    if i<=0 then
      raise TSocketException.Create('Receive Error');

    inc(result, i);
  end;
end;



end.

