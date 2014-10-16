unit CELazySocket;
{
Just some functions to make sockets easier
}

{$mode delphi}

interface

uses
  Classes, SysUtils, Sockets, winsock, ssockets;

type
  TSocketException=class(Exception);
    {
  TNetworkStream=class(TMemoryStream)
  private
  public
    function WriteToSocket(s: tsocket; timeout: integer=10): integer;
    function ReadFromSocket(s: tsocket; readsize: integer; timeout: integer=10): integer;
  end;
         }
  TSocketStream=class(TStream)
  private
    s: TSocket;
    fbecomeownerofsocket: boolean;
    fTimeout: integer;
    writer: TMemorystream;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;

    function ReadAnsiString8 : String;
    procedure WriteAnsiString8(const S: String);

    procedure flushWrites;
    constructor create(s : tsocket; becomeownerofsocket: boolean=true);
    destructor destroy; override;
    property timeout: integer read ftimeout write ftimeout;
    property sockethandle: Tsocket read s;
  end;


function send(socket: TSocket; buffer: pointer; size: integer; timeout: integer=10): integer;
function receive(socket: TSocket; buffer: pointer; size: integer; timeout: integer=10): integer;

//todo: perhaps setup listener/connect for non blocking ?

implementation

function TSocketStream.Read(var Buffer; Count: Longint): Longint;
begin
  if writer.position>0 then flushwrites; //first make sure everything is sent before reading

  result:=receive(s, @buffer, count, ftimeout);
end;

function TSocketStream.Write(const Buffer; Count: Longint): Longint;
begin
  result:=writer.Write(buffer, count);

  if writer.Position>=65536 then
    flushWrites;
end;

Procedure TSocketStream.WriteAnsiString8 (const S : String);
var L: byte;
begin
  L:=length(S);
  WriteBuffer(L,SizeOf(L));
  WriteBuffer(Pointer(S)^,L);
end;

function TSocketStream.ReadAnsiString8 : String;
//same as readAnsiStream, but make the header only one byte long (max stringsize=255)
Var
  TheSize : byte;
  P : PByte ;
begin
  ReadBuffer (TheSize,SizeOf(TheSize));
  SetLength(Result,TheSize);
  // Illegal typecast if no AnsiStrings defined.
  if TheSize>0 then
  begin
    ReadBuffer (Pointer(Result)^,TheSize);
    P:=Pointer(Result)+TheSize;
    p^:=0;
  end;
end;

procedure TSocketStream.flushWrites;
begin
  send(s, writer.memory, writer.size, ftimeout);
  writer.position:=0;
end;

destructor TSocketStream.destroy;
begin
  if writer<>nil then
    freeandnil(writer);

  if fbecomeownerofsocket then
    closesocket(s);

  inherited destroy;
end;

constructor TSocketStream.create(s: tsocket; becomeownerofsocket: boolean=true);
var bm: u_long;
begin
  inherited create;
  writer:=TMemoryStream.create;
  self.s:=s;
  fTimeout:=10; //default timeout
  fbecomeownerofsocket:=becomeownerofsocket;

  {$ifdef windows}
    bm:=0;
    ioctlsocket(sockethandle, FIONBIO, bm);
  {$else}
    fcntl(fSocket, F_SETFL, fcntl(socketfd, F_GETFL, 0) | O_NONBLOCK);
  {$endif}

end;

//----------------
          {
function TNetworkStream.WriteToSocket(s: tsocket; timeout: integer=10): integer;
begin
  result:=send(s, memory, size);
end;

function TNetworkStream.ReadFromSocket(s: tsocket; readsize: integer; timeout: integer=10): integer;
var buffer: pchar;
begin
  getmem(buffer, readsize);
  try
    result:=receive(s, buffer, readsize);
    WriteBuffer(buffer^, result);
  finally
    freemem(buffer);
  end;
end;      }

function send(socket: TSocket; buffer: pointer; size: integer; timeout: integer=10): integer;
var
  i: integer;
  t: TTimeVal;
  fdset: TFDSet;
begin
  result:=0;
  while (result<size) do
  begin
    i:=fpsend(socket, pointer(ptruint(buffer)+result), size, 0);
    if i<=0 then
    begin
      if i=-1 then
      begin
        i:=socketerror;

        if i=EsockEWOULDBLOCK then
        begin
          //wait till it's available
          fdset.fd_count:=0;
          FD_SET(socket, fdset);

          if timeout>0 then
          begin
            t.tv_sec:=timeout;
            t.tv_usec:=0;


            i:=select(socket, nil, @fdset, nil, @t);
          end
          else
            i:=select(socket, nil, @fdset, nil, nil);

          if i=0 then
            raise TSocketException.create('Timeout while sending data');

          if i<0 then
            raise TSocketException.create('Error while sending data: '+inttostr(socketerror));

        end
        else
          TSocketException.Create('Error while sending data: '+inttostr(i));
      end
      else
        TSocketException.Create('Disconnected while sending data');
    end;

    inc(result, i);
  end;
end;

function receive(socket: TSocket; buffer: pointer; size: integer; timeout: integer=10): integer;
var
  i: integer;
  t: TTimeVal;
  fdset: TFDSet;
begin
  result:=0;
  while (result<size) do
  begin
    i:=fprecv(socket, pointer(ptruint(buffer)+result), size-result, 0);
    if i<=0 then
    begin
      if i=-1 then
      begin
        i:=socketerror;

        if i=EsockEWOULDBLOCK then
        begin
          //wait till it's available
          fdset.fd_count:=0;
          FD_SET(socket, fdset);

          if timeout>0 then
          begin
            t.tv_sec:=timeout;
            t.tv_usec:=0;

            i:=select(socket, @fdset, nil, nil, @t);
          end
          else
            i:=select(socket, @fdset, nil, nil, nil);


          if i=0 then
            raise TSocketException.create('Timeout while receiving data');

          if i<0 then
            raise TSocketException.create('Error while receiving data: '+inttostr(i));

        end
        else
          TSocketException.Create('Error while receiving data: '+inttostr(i));
      end
      else
        TSocketException.Create('Disconnected while receiving data');
    end;

    inc(result, i);
  end;
end;


end.

