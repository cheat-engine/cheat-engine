unit xmplayer_server;

{$mode delphi}

interface

uses
  {$ifdef darwin}
  macport,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}
  Classes, SysUtils, xmplayer_defines, cefuncproc, globals, mikmod;

type TXMPlayer=class
  private
    finitialized: boolean;
    function getPlayingState: boolean;
  public
    procedure playXM(filename: string; noloop: boolean=false); overload;
    procedure playXM(stream: TStream; noloop: boolean=false); overload;
    procedure pause;
    procedure resume;
    procedure stop;
    procedure setVolume(v: integer);
    constructor create;
  published
    property Initialized: boolean read finitialized;
    property IsPlaying: boolean read getPlayingState;
end;

var
  xmplayer: TXMPlayer;


implementation

function TXMPlayer.getPlayingState: boolean;
begin
  LoadMikMod;
  result:=Player_Active() and (not Player_Paused());
end;

procedure TXMPlayer.playXM(filename: string; noloop: boolean);
begin
  LoadMikMod;
  MikMod_Play(filename,not noloop);
end;

procedure TXMPlayer.playXM(stream: TStream; noloop: boolean=false);
begin
  LoadMikMod;
  MikMod_PlayStream(stream, not noloop);
end;

procedure TXMPlayer.pause;
begin
  LoadMikMod;
  MikMod_Pause;
end;

procedure TXMPlayer.resume;
begin
  LoadMikMod;
  MikMod_UnPause;
end;


procedure TXMPlayer.stop;
begin
  LoadMikMod;

  MikMod_Stop;
end;

procedure TXMPlayer.setVolume(v: integer);
begin
  LoadMikMod;
  Player_SetVolume(v);
end;

constructor TXMPlayer.Create;
begin
  finitialized:=true;
end;

initialization
  xmplayer:=TXMPlayer.create;

end.

