unit xmplayer_server;

{$mode delphi}

interface

uses
  windows, Classes, SysUtils, xmplayer_defines, cefuncproc, globals;

resourcestring
  rsXmplayerExeIsMissing = 'xmplayer.exe is missing';
  rsFailedCreatingTheAudioPipe = 'Failed creating the audio pipe';

type TXMPlayer=class
  private
    finitialized: boolean;
    fisPlaying: boolean;
    loadedevent: THandle;
    audiopipe: THandle;
  public
    procedure initialize;
    procedure playXM(filename: string; noloop: boolean=false); overload;
    procedure playXM(stream: TStream; noloop: boolean=false); overload;
    procedure pause;
    procedure resume;
    procedure stop;
    procedure setVolume(v: integer);
  published
    property Initialized: boolean read finitialized;
    property IsPlaying: boolean read fisplaying;
end;

var
  xmplayer: TXMPlayer;


implementation

procedure TXMPlayer.initialize;
var uid: string;
  g: TGUID;
  i: integer;
  z: dword;
begin
  if not fileexists(cheatenginedir+'xmplayer.exe') then
    raise exception.create(rsXmplayerExeIsMissing);

  //create an unique ID
  CreateGUID(g);
  uid:='CEA'+inttohex(g.data1,8)+'_'+inttohex(g.data2,4)+'_'+inttohex(g.data3,4)+'_';
  for i:=0 to 7 do
    uid:=uid+inttohex(g.data4[i],2);

  loadedevent:=CreateEvent(nil, false, false, pchar(uid));
  audiopipe:=CreateNamedPipe(pchar('\\.\pipe\'+uid), PIPE_ACCESS_DUPLEX, PIPE_TYPE_BYTE or PIPE_READMODE_BYTE or PIPE_WAIT, 255, 128*1024, 8, INFINITE, nil);
  if audiopipe=INVALID_HANDLE_VALUE then raise exception.create(
    rsFailedCreatingTheAudioPipe);


  //launches the xmplayer client
  ShellExecute(0, PChar('open'), PChar(cheatenginedir+'xmplayer.exe'), PChar(uid), PChar(''), SW_SHOW);

  //wait for it to acquire the pipe
  WaitForSingleObject(loadedevent, infinite);

  finitialized:=true;


end;

procedure TXMPlayer.playXM(filename: string; noloop: boolean);
var f: Tfilestream;
begin
  if not initialized then
    initialize;

  if FileExists(filename) then
  begin
    f:=TFilestream.create(filename, fmOpenRead or fmShareDenyNone);
    playXM(f, noloop);
    f.free;
  end;
end;

procedure TXMPlayer.playXM(stream: TStream; noloop: boolean=false);
var buf: TMemorystream;
  command: byte;
  size: integer;
  w: dword;
  extraparam: byte;
begin
  if not initialized then
    initialize;

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

  extraparam:=0;
  if noloop then
    extraparam:=extraparam or 8; //no_loop

  buf.WriteBuffer(extraparam, 1);

  WriteFile(audiopipe, buf.memory^, buf.size, w, nil);

  buf.free;

  fisPlaying:=true;
end;

procedure TXMPlayer.pause;
var command: byte;
  w: dword;
begin
  if not initialized then
    initialize;

  command:=XMPLAYER_PAUSE;
  writefile(audiopipe, command, 1,w, nil);

  fisPlaying:=false;
end;

procedure TXMPlayer.resume;
var command: byte;
  w: dword;
begin
  if not initialized then
    initialize;

  command:=XMPLAYER_RESUME;
  writefile(audiopipe, command, 1,w, nil);
  fisPlaying:=true;
end;


procedure TXMPlayer.stop;
var command: byte;
  w: dword;
begin
  if not initialized then
    initialize;

  command:=XMPLAYER_STOP;
  writefile(audiopipe, command, 1,w, nil);
  fisPlaying:=false;
end;

procedure TXMPlayer.setVolume(v: integer);
var command: packed record
  command: byte;
  volume: byte;
end;
  w: dword;
begin
  if not initialized then
    initialize;

  command.command:=XMPLAYER_SETVOLUME;
  command.volume:=v;
  writefile(audiopipe, command, 2,w, nil);

  fisPlaying:=false;
end;

initialization
  xmplayer:=TXMPlayer.create;

end.

