unit CELazySocket;
{
Just some functions to make sockets easier
}

{$mode delphi}

interface

uses
  {$ifdef windows}
  windows, Classes, SysUtils, Sockets, winsock, ssockets, NewKernelHandler;
  {$endif}
  {$ifdef darwin}
  Classes, SysUtils, Sockets, ssockets, NewKernelHandler, ctypes, baseunix, macport;
  {$endif}

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

    function flushWrites: integer;
    constructor create(s : tsocket; becomeownerofsocket: boolean=true);
    destructor destroy; override;
    property timeout: integer read ftimeout write ftimeout;
    property sockethandle: Tsocket read s;
  end;


function send(socket: TSocket; buffer: pointer; size: integer; timeout: integer=10): integer;
function receive(socket: TSocket; buffer: pointer; size: integer; timeout: integer=10): integer;

//todo: perhaps setup listener/connect for non blocking ?

var
  debug_connectionfailure: boolean;
  debug_nonresponsiveconnection: boolean;

implementation

resourcestring
  rsWhoopdeedoo = 'Whoopdeedoo';
  rsTimeoutWhileSendingData = 'Timeout while sending data';
  rsErrorWhileSendingData = 'Error while sending data: ';
  rsDisconnectedWhileSendingData = 'Disconnected while sending data';
  rsTimeoutWhileReceivingData = 'Timeout while receiving data';
  rsErrorWhileReceivingData = 'Error while receiving data: ';
  rsDisconnectedWhileReceivingData = 'Disconnected while receiving data';

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

function TSocketStream.flushWrites: integer;
begin
  if writer.position>0 then
  begin
    result:=send(s, writer.memory, writer.position, ftimeout);
    writer.position:=0;
  end
  else
    result:=0;
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
{$ifdef windows}
var bm: u_long;
  {$endif}
begin
  inherited create;
  writer:=TMemoryStream.create;
  self.s:=s;
  fTimeout:=10; //default timeout
  fbecomeownerofsocket:=becomeownerofsocket;

  {$ifdef windows}
    bm:=1;
    ioctlsocket(sockethandle, longint(FIONBIO), bm);
  {$else}
    FpFcntl(sockethandle, F_SETFL, FpFcntl(sockethandle, F_GETFL, 0) or O_NONBLOCK);
  {$endif}

end;

//----------------

function send(socket: TSocket; buffer: pointer; size: integer; timeout: integer=10): integer;
var
  i: integer;
  t: TTimeVal;
  fdset: TFDSet;
begin
  {$ifdef DEBUGPROTOCOL}
  timeout:=0; //just let me test in peace
  {$endif}

  if debug_connectionfailure then
    raise TSocketException.Create(rsWhoopdeedoo);


  result:=0;
  while (result<size) do
  begin
    if debug_nonresponsiveconnection then
      sleep(20000);

    i:=fpsend(socket, pointer(ptruint(buffer)+result), size, 0);
    if i<=0 then
    begin
      if i=-1 then
      begin
        i:=socketerror;

        if i=EsockEWOULDBLOCK then
        begin
          //wait till it's available
          zeromemory(@fdset,sizeof(fdset));
//          fdset.fd_count:=0;

{$ifdef windows}
          FD_SET(socket, fdset);
{$else}
          fpFD_SET(socket, fdset);
{$endif}

          if timeout>0 then
          begin
            t.tv_sec:=timeout;
            t.tv_usec:=0;


            i:={$ifdef unix}fpselect{$else}select{$endif}(socket, nil, @fdset, nil, @t);
          end
          else
            i:={$ifdef unix}fpselect{$else}select{$endif}(socket, nil, @fdset, nil, nil);

          if i=0 then
            raise TSocketException.create(rsTimeoutWhileSendingData);

          if i<0 then
            raise TSocketException.create(rsErrorWhileSendingData+inttostr(socketerror));

          i:=0;
        end
        else
          raise TSocketException.Create(rsErrorWhileSendingData+inttostr(i));
      end
      else
        raise TSocketException.Create(rsDisconnectedWhileSendingData);
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
  {$ifdef DEBUGPROTOCOL}
  timeout:=0;
  {$endif}

  if debug_connectionfailure then
    raise TSocketException.Create(rsWhoopdeedoo);

  result:=0;
  while (result<size) do
  begin
    if debug_nonresponsiveconnection then
      sleep(20000);

    i:=fprecv(socket, pointer(ptruint(buffer)+result), size-result, 0);
    if i<=0 then
    begin

      if i=-1 then
      begin
        i:=socketerror;

        if i=EsockEWOULDBLOCK then
        begin
          //wait till it's available
          zeromemory(@fdset,sizeof(fdset));
//          fdset.fd_count:=0;
          {$ifdef unix}fpFD_SET{$else}FD_SET{$endif}(socket, fdset);

          if timeout>0 then
          begin
            t.tv_sec:=timeout;
            t.tv_usec:=0;

            i:={$ifdef unix}fpselect{$else}select{$endif}(socket, @fdset, nil, nil, @t);
          end
          else
            i:={$ifdef unix}fpselect{$else}select{$endif}(socket, @fdset, nil, nil, nil);


          if i=0 then
          begin
            OutputDebugString('Timeout');
            raise TSocketException.create(rsTimeoutWhileReceivingData);
          end;

          if i<0 then
            raise TSocketException.create(rsErrorWhileReceivingData+inttostr(i));

          i:=0;

        end
        else
          raise TSocketException.Create(rsErrorWhileReceivingData+inttostr(i));
      end
      else
        raise TSocketException.Create(rsDisconnectedWhileReceivingData);
    end;

    inc(result, i);
  end;
end;


end.

