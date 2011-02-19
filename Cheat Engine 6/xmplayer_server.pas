unit xmplayer_server;

{$mode delphi}

interface

uses
  windows, Classes, SysUtils, xmplayer_defines, cefuncproc;

type TXMPlayer=class
  private
    fisPlaying: boolean;
    loadedevent: THandle;
    audiopipe: THandle;
  public
    constructor create;
    procedure playXM(filename: string); overload;
    procedure playXM(stream: TStream); overload;
    procedure pause;
    procedure resume;
    procedure stop;
    property isPlaying: boolean read fisplaying;
end;

var
  xmplayer: TXMPlayer;


implementation

constructor TXMPlayer.create; //xmplayer_initialize
var uid: string;
  g: TGUID;
  i: integer;
  z: dword;
begin
  if not fileexists(cheatenginedir+'xmplayer.exe') then
    raise exception.create('xmplayer.exe is missing');

  //create an unique ID
  CreateGUID(g);
  uid:='CEA'+inttohex(g.data1,8)+'_'+inttohex(g.data2,4)+'_'+inttohex(g.data3,4)+'_';
  for i:=0 to 7 do
    uid:=uid+inttohex(g.data4[i],2);

  loadedevent:=CreateEvent(nil, false, false, pchar(uid));
  audiopipe:=CreateNamedPipe(pchar('\\.\pipe\'+uid), PIPE_ACCESS_DUPLEX, PIPE_TYPE_BYTE or PIPE_READMODE_BYTE or PIPE_WAIT, 255, 128*1024, 8, INFINITE, nil);
  if audiopipe=INVALID_HANDLE_VALUE then raise exception.create('Failed creating the audio pipe');


  //launches the xmplayer client
  ShellExecute(0, PChar('open'), PChar(cheatenginedir+'xmplayer.exe'), PChar(uid), PChar(''), SW_SHOW);

  //wait for it to acquire the pipe
  WaitForSingleObject(loadedevent, infinite);


end;

procedure TXMPlayer.playXM(filename: string);
var f: Tfilestream;
begin
  f:=TFilestream.create(filename, fmOpenRead or fmShareDenyNone);
  playXM(f);
  f.free;
end;

procedure TXMPlayer.playXM(stream: TStream);
var buf: TMemorystream;
  command: byte;
  size: integer;
  w: dword;
begin
  stream.Position:=0;
  buf:=TMemorystream.create;

  //setup the message
  command:=xmplayer_defines.XMPLAYER_PLAYXM;
  size:=stream.size;

  buf.WriteBuffer(command, sizeof(command));
  buf.WriteBuffer(size, sizeof(size));
  //data
  buf.CopyFrom(stream, stream.size);
  //send buffer to the audiopipe

  WriteFile(audiopipe, buf.memory^, buf.size, w, nil);

  buf.free;

  fisPlaying:=true;
end;

procedure TXMPlayer.pause;
var command: byte;
  w: dword;
begin
  command:=XMPLAYER_PAUSE;
  writefile(audiopipe, command, 1,w, nil);

  fisPlaying:=false;
end;

procedure TXMPlayer.resume;
var command: byte;
  w: dword;
begin
  command:=XMPLAYER_RESUME;
  writefile(audiopipe, command, 1,w, nil);
  fisPlaying:=true;
end;


procedure TXMPlayer.stop;
var command: byte;
  w: dword;
begin
  command:=XMPLAYER_STOP;
  writefile(audiopipe, command, 1,w, nil);
  fisPlaying:=false;
end;

end.

