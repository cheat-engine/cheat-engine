unit packetfilter;

interface

uses windows, winsock, idwinsock2;

type Tsend=function(s: integer; buf: pbyte; len: integer; flags: integer): integer; stdcall;
type Tsendto=function(s: integer; buf: pbyte; len: integer; flags: Integer; var addrto: TSockAddr; tolen: Integer): Integer; stdcall;
type Trecv=function(s: TSocket; buf: pbyte; len, flags: Integer): Integer; stdcall;
type Trevcfrom=function (s: TSocket; buf: pbyte; len, flags: Integer; var from: TSockAddr; var fromlen: Integer): Integer; stdcall;

type TWSAsend=function ( const s : TSocket; lpBuffers : LPWSABUF; dwBufferCount : DWORD; var lpNumberOfBytesSent : DWORD; dwFlags : DWORD; lpOverlapped : LPwsaoverlapped; lpCompletionRoutine : LPwsaoverlapped_COMPLETION_ROUTINE ): Integer; stdcall;
type TWSAsendto = function ( const s : TSocket; lpBuffers : LPWSABUF; dwBufferCount : DWORD; var lpNumberOfBytesSent : DWORD; dwFlags : DWORD; lpTo : PSockAddr; iTolen : Integer; lpOverlapped : LPwsaoverlapped; lpCompletionRoutine : LPwsaoverlapped_COMPLETION_ROUTINE ): Integer; stdcall;
type TWSArecv = function ( const s : TSocket; lpBuffers : LPWSABUF; dwBufferCount : DWORD; var lpNumberOfBytesRecvd : DWORD; var lpFlags : DWORD; lpOverlapped : LPwsaoverlapped; lpCompletionRoutine : LPwsaoverlapped_COMPLETION_ROUTINE ): Integer; stdcall;
type TWSArecvfrom = function ( const s : TSocket; lpBuffers : LPWSABUF; dwBufferCount : DWORD; var lpNumberOfBytesRecvd : DWORD; var lpFlags : DWORD; lpFrom : PSockAddr; lpFromlen : PInteger; lpOverlapped : LPwsaoverlapped; lpCompletionRoutine : LPwsaoverlapped_COMPLETION_ROUTINE ): Integer; stdcall;


function ws2send(s: integer; buf: pbyte; len: integer; flags: integer): integer; stdcall;
function ws2sendto(s: integer; buf: pbyte; len: integer; flags: Integer; var addrto: TSockAddr; tolen: Integer): Integer; stdcall;
function ws2recv(s: TSocket; buf: pbyte; len, flags: Integer): Integer; stdcall;
function ws2recvfrom(s: TSocket; buf: pbyte; len, flags: Integer; var from: TSockAddr; var fromlen: Integer): Integer; stdcall;

function ws2WSAsend( const s : TSocket; lpBuffers : LPWSABUF; dwBufferCount : DWORD; var lpNumberOfBytesSent : DWORD; dwFlags : DWORD; lpOverlapped : LPwsaoverlapped; lpCompletionRoutine : LPwsaoverlapped_COMPLETION_ROUTINE ): Integer; stdcall;
function ws2WSAsendto( const s : TSocket; lpBuffers : LPWSABUF; dwBufferCount : DWORD; var lpNumberOfBytesSent : DWORD; dwFlags : DWORD; lpTo : PSockAddr; iTolen : Integer; lpOverlapped : LPwsaoverlapped; lpCompletionRoutine : LPwsaoverlapped_COMPLETION_ROUTINE ): Integer; stdcall;
function ws2WSArecv( const s : TSocket; lpBuffers : LPWSABUF; dwBufferCount : DWORD; var lpNumberOfBytesRecvd : DWORD; var lpFlags : DWORD; lpOverlapped : LPwsaoverlapped; lpCompletionRoutine : LPwsaoverlapped_COMPLETION_ROUTINE ): Integer; stdcall;
function ws2WSArecvfrom( const s : TSocket; lpBuffers : LPWSABUF; dwBufferCount : DWORD; var lpNumberOfBytesRecvd : DWORD; var lpFlags : DWORD; lpFrom : PSockAddr; lpFromlen : PInteger; lpOverlapped : LPwsaoverlapped; lpCompletionRoutine : LPwsaoverlapped_COMPLETION_ROUTINE ): Integer; stdcall;

type
  TsendData=record
    flags: ^integer;
    bufsize: ^integer;
    buf: ^pbyte;
    socket: integer;
    replacedbuf: boolean;
    iswsa: boolean;
    wsamultibuffered: boolean;
    wsaoverlapped: boolean;
    sendto: boolean;
    address: TSockAddr;
  end;
  PSendData=^TsendData;

  TrecvData=record
    bufsize: integer;
    requestedsize: integer;
    buf: pbyte;
    socket: integer;
    iswsa: boolean;
    wsamultibuffered:boolean;
    wsaoverlapped: boolean;
    recvfrom: boolean;
    address: TSockAddr;
  end;
  PrecvData=^TrecvData;

var
  ws2sendorig: Tsend;
  ws2sendtoorig: Tsendto;
  ws2recvorig: Trecv;
  ws2recvfromorig: Trevcfrom;

  ws2WSAsendorig: TWSAsend;
  ws2WSAsendtoorig: TWSAsendto;
  ws2WSArecvorig: TWSArecv;
  ws2WSArecvfromorig: TWSArecvfrom;

implementation

uses mainunit;

function ws2WSAsend( const s : TSocket; lpBuffers : LPWSABUF; dwBufferCount : DWORD; var lpNumberOfBytesSent : DWORD; dwFlags : DWORD; lpOverlapped : LPwsaoverlapped; lpCompletionRoutine : LPwsaoverlapped_COMPLETION_ROUTINE ): Integer; stdcall;
var sd: PsendData;
begin
  //send and wait for finish , so the filter can do something
  getmem(sd,sizeof(TSendData));
  ZeroMemory(sd,sizeof(TSendData));

  sd^.flags:=@dwflags;
  sd^.bufsize:=@lpBuffers.len;
  sd^.buf:=@lpBuffers.buf;
  sd^.socket:=s;
  sd^.isWSA:=true;
  sd^.wsamultibuffered:=dwBufferCount>0;
  sd^.wsaoverlapped:=lpOverlapped<>nil;
  sendmessage(mainform.handle, WM_SEND, dword(sd),0);

  if sd.replacedbuf then
    freemem(sd^.buf);

  freemem(sd);
  
  result:=ws2WSAsendorig(s, lpBuffers, dwBufferCount, lpNumberOfBytesSent, dwFlags, lpOverlapped, lpCompletionRoutine);
end;

function ws2WSAsendto( const s : TSocket; lpBuffers : LPWSABUF; dwBufferCount : DWORD; var lpNumberOfBytesSent : DWORD; dwFlags : DWORD; lpTo : PSockAddr; iTolen : Integer; lpOverlapped : LPwsaoverlapped; lpCompletionRoutine : LPwsaoverlapped_COMPLETION_ROUTINE ): Integer; stdcall;
var sd: PsendData;
begin
  //send and wait for finish , so the filter can do something
  getmem(sd,sizeof(TSendData));
  ZeroMemory(sd,sizeof(TSendData));

  sd^.flags:=@dwflags;
  sd^.bufsize:=@lpBuffers.len;
  sd^.buf:=@lpBuffers.buf;
  sd^.socket:=s;
  sd^.isWSA:=true;
  sd^.wsamultibuffered:=dwBufferCount>0;
  sd^.wsaoverlapped:=lpOverlapped<>nil;
  sd^.sendto:=true;
  sd^.address:=lpTo^;
  sendmessage(mainform.handle, WM_SEND, dword(sd),0);

  if sd.replacedbuf then
    freemem(sd^.buf);

  freemem(sd);
  result:=ws2WSAsendto(s, lpBuffers, dwBufferCount, lpNumberOfBytesSent, dwFlags, lpTo, iTolen, lpOverlapped, lpCompletionRoutine);
end;

function ws2WSArecv( const s : TSocket; lpBuffers : LPWSABUF; dwBufferCount : DWORD; var lpNumberOfBytesRecvd : DWORD; var lpFlags : DWORD; lpOverlapped : LPwsaoverlapped; lpCompletionRoutine : LPwsaoverlapped_COMPLETION_ROUTINE ): Integer; stdcall;
var rd: PrecvData;
begin
  lpNumberOfBytesRecvd:=0;
  result:=ws2WSArecvorig( s, lpBuffers, dwBufferCount, lpNumberOfBytesRecvd, lpFlags, lpOverlapped, lpCompletionRoutine);

  if lpNumberOfBytesRecvd>0 then
  begin
    getmem(rd,sizeof(TSendData));
    ZeroMemory(rd,sizeof(TSendData));
    rd^.bufsize:=lpNumberOfBytesRecvd;
    rd^.buf:=PBYTE(lpBuffers.buf);
    rd^.socket:=s;
    rd^.requestedsize:=lpBuffers.len;
    rd^.isWSA:=true;
    rd^.wsamultibuffered:=dwBufferCount>0;
    rd^.wsaoverlapped:=lpOverlapped<>nil;
    sendmessage(mainform.handle, WM_RECV, dword(rd),0);
    freemem(rd);
  end;

end;

function ws2WSArecvfrom( const s : TSocket; lpBuffers : LPWSABUF; dwBufferCount : DWORD; var lpNumberOfBytesRecvd : DWORD; var lpFlags : DWORD; lpFrom : PSockAddr; lpFromlen : PInteger; lpOverlapped : LPwsaoverlapped; lpCompletionRoutine : LPwsaoverlapped_COMPLETION_ROUTINE ): Integer; stdcall;
var rd: PrecvData;
begin
  lpNumberOfBytesRecvd:=0;
  result:=ws2WSArecvfromorig( s, lpBuffers, dwBufferCount, lpNumberOfBytesRecvd, lpFlags ,lpFrom , lpFromlen ,lpOverlapped ,lpCompletionRoutine);
  if lpNumberOfBytesRecvd>0 then
  begin
    getmem(rd,sizeof(TSendData));
    ZeroMemory(rd,sizeof(TSendData));
    rd^.bufsize:=lpNumberOfBytesRecvd;
    rd^.buf:=PBYTE(lpBuffers.buf);
    rd^.socket:=s;
    rd^.requestedsize:=lpBuffers.len;
    rd^.isWSA:=true;
    rd^.wsamultibuffered:=dwBufferCount>0;
    rd^.wsaoverlapped:=lpOverlapped<>nil;
    rd^.recvfrom:=true;
    rd^.address:=lpFrom^;
    sendmessage(mainform.handle, WM_RECV, dword(rd),0);
    freemem(rd);
  end;  
end;

function ws2send(s: integer; buf: pbyte; len: integer; flags: integer): integer; stdcall;
var sd: PsendData;
begin
  //send and wait for finish , so the filter can do something
  getmem(sd,sizeof(TSendData));
  ZeroMemory(sd,sizeof(TSendData));

  sd^.flags:=@flags;
  sd^.bufsize:=@len;
  sd^.buf:=@buf;
  sd^.socket:=s;
  sendmessage(mainform.handle, WM_SEND, dword(sd),0);

  if sd.replacedbuf then
    freemem(sd^.buf);

  freemem(sd);

  result:=ws2sendorig(s,buf,len,flags);
end;

function ws2sendto(s: integer; buf: pbyte; len: integer; flags: Integer; var addrto: TSockAddr; tolen: Integer): Integer; stdcall;
var sd: PsendData;
begin
  getmem(sd,sizeof(TSendData));
  ZeroMemory(sd,sizeof(TSendData));
  
  sd^.flags:=@flags;
  sd^.bufsize:=@len;
  sd^.buf:=@buf;
  sd^.socket:=s;
  sd^.sendto:=true;
  sd^.address:=addrto;
  sendmessage(mainform.handle, WM_SEND, dword(sd),0);
  if sd.replacedbuf then
    freemem(sd^.buf);

  freemem(sd);
  
  result:=ws2sendtoorig(s,buf,len,flags,addrto, tolen);
end;

function ws2recv(s: TSocket; Buf: pbyte; len, flags: Integer): Integer; stdcall;
var rd: PrecvData;
begin
  result:=ws2recvorig(s,buf,len,flags);

  if result>0 then
  begin
    getmem(rd,sizeof(TSendData));
    ZeroMemory(rd,sizeof(TSendData));
    rd^.bufsize:=result;
    rd^.buf:=buf;
    rd^.socket:=s;
    rd^.requestedsize:=len;
    sendmessage(mainform.handle, WM_RECV, dword(rd),0);
    freemem(rd);
  end;

end;

function ws2recvfrom(s: TSocket; Buf:pbyte; len, flags: Integer; var from: TSockAddr; var fromlen: Integer): Integer; stdcall;
var rd: PrecvData;
begin
  result:=ws2recvfromorig(s,buf,len,flags, from, fromlen);
  if result>0 then
  begin
    getmem(rd,sizeof(TSendData));
    ZeroMemory(rd,sizeof(TSendData));

    rd^.bufsize:=result;
    rd^.buf:=buf;
    rd^.socket:=s;
    rd^.requestedsize:=len;
    rd^.recvfrom:=true;
    rd^.address:=from;
    sendmessage(mainform.handle, WM_RECV, dword(rd),0);
    freemem(rd);
  end;
end;


end.
