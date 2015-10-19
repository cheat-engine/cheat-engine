unit mikmod;

{$mode objfpc}{$H+}

{
MikMod sound library
half port of the MikMod header file to pascal
}

interface

uses
  windows, Classes, SysUtils;


type
  PMODULE=pointer;
  PREADER=^MREADER;

  MREADER_SEEK=function(self:PREADER; offset:longint; whence:integer):longint;cdecl;
  MREADER_TELL=function(self:PREADER):longint;cdecl;
  MREADER_READ=function(self:PREADER; dest: pointer; lenght: size_t): BOOL; cdecl;
  MREADER_GET=function(self:PREADER):integer; cdecl;
  MREADER_EOF=function(self:PREADER):BOOL; cdecl;

  MREADER=record
    Seek: MREADER_SEEK;
    Tell: MREADER_TELL;
    Read: MREADER_READ;
    Get:  MREADER_GET;
    EOF:  MREADER_EOF;
  end;

function LoadMikMod: boolean;

const
  {These ones take effect only after MikMod_Init or MikMod_Reset}
  DMODE_16BITS       =$0001;
  DMODE_STEREO       =$0002;
  DMODE_SOFT_SNDFX   =$0004;
  DMODE_SOFT_MUSIC   =$0008;
  DMODE_HQMIXER      =$0010;

  {These take effect immediately}
  DMODE_SURROUND     =$0100;
  DMODE_INTERP       =$0200;
  DMODE_REVERSE      =$0400;


var
  MikMod_RegisterAllDrivers: procedure; cdecl;
  MikMod_InfoDriver: function: pchar; cdecl;
  MikMod_RegisterDriver: procedure (mdriver: pointer); cdecl;
  MikMod_DriverFromAlias: function (al: pchar): integer; cdecl;
  MikMod_Init: function (cmdline: pchar): integer; cdecl;
  MikMod_Exit: procedure; cdecl;
  MikMod_Reset: function(cmdlime: pchar): integer; cdecl;
  MikMod_SetNumVoices: function(music, sfx: integer): integer; cdecl;
  MikMod_Active: function:BOOL; cdecl;
  MikMod_EnableOutput: function:integer; cdecl;
  MikMod_DisableOutput: procedure; cdecl;
  MikMod_Update: procedure; cdecl;
  MikMod_InitThreads: function: BOOL; cdecl;
  MikMod_Lock: procedure; cdecl;
  MikMod_Unlock: procedure; cdecl;

  MikMod_RegisterAllLoaders: procedure; cdecl;


  //mod player
  Player_Load: function(filename: pchar; maxchan: integer; curious: BOOL): PMODULE; cdecl;
  Player_LoadFP: function(fp: pointer; maxchan: integer; curious: BOOL): PMODULE; cdecl;
  Player_LoadGeneric: function(reader: PREADER; maxchan: integer; curious: BOOL): PMODULE; cdecl;
  Player_LoadTitle: function(filename: pchar): pchar; cdecl;
  Player_LoadTitleFP: function(fp: pointer): pchar; cdecl;
  Player_Free: procedure(module: PMODULE); cdecl;
  Player_Start: procedure(module: PMODULE); cdecl;
  Player_Active: function:BOOL; cdecl;
  Player_Stop: procedure; cdecl;
  Player_TogglePause: procedure; cdecl;
  Player_Paused: function:BOOL; cdecl;
  Player_NextPosition: procedure; cdecl;
  Player_PrevPosition: procedure; cdecl;
  Player_SetPosition: procedure(pos: Word); cdecl;
  Player_Muted: function(chan: BYTE):BOOL; cdecl;
  Player_SetVolume: procedure(volume: LongInt); cdecl;
  Player_GetModule: function:PMODULE; cdecl;
  Player_SetSpeed: procedure(speed: WORD); cdecl;
  Player_SetTempo: procedure(tempo: WORD); cdecl;



  { These variables can be changed at ANY time and results will be immediate }
  md_volume: PBYTE;
  md_musicvolume: PBYTE;
  md_sndfxvolume: PBYTE;
  md_reverb: PBYTE;
  md_pansep: PBYTE;

  {
  The variables below can be changed at any time, but changes will not be
   implemented until MikMod_Reset is called. A call to MikMod_Reset may result
   in a skip or pop in audio (depending on the soundcard driver and the settings
   changed).
   }
  md_device: PWORD;
  md_mixfreq: PWORD;
  md_mode: PWORD;


  MikMod_errno: PINTEGER;

implementation

var libmikmod: HModule;

function LoadMikMod: boolean;
begin
  result:=libmikmod<>0;
  if result=false then
  begin
    libmikmod:=loadlibrary('libmikmod'+{$ifdef cpu32}'32'{$else}'64'{$endif}+'.dll');

    farproc(MikMod_RegisterAllDrivers):=GetProcAddress(libmikmod, 'MikMod_RegisterAllDrivers');
    farproc(MikMod_RegisterAllLoaders):=GetProcAddress(libmikmod, 'MikMod_RegisterAllLoaders');


    farproc(MikMod_InfoDriver):=GetProcAddress(libmikmod, 'MikMod_InfoDriver');
    farproc(MikMod_RegisterDriver):=GetProcAddress(libmikmod, 'MikMod_RegisterDriver');
    farproc(MikMod_RegisterAllDrivers):=GetProcAddress(libmikmod, 'MikMod_RegisterAllDrivers');
    farproc(MikMod_DriverFromAlias):=GetProcAddress(libmikmod, 'MikMod_DriverFromAlias');
    farproc(MikMod_Init):=GetProcAddress(libmikmod, 'MikMod_Init');

    farproc(MikMod_Exit):=GetProcAddress(libmikmod, 'MikMod_Exit');
    farproc(MikMod_Reset):=GetProcAddress(libmikmod, 'MikMod_Reset');
    farproc(MikMod_SetNumVoices):=GetProcAddress(libmikmod, 'MikMod_SetNumVoices');
    farproc(MikMod_Active):=GetProcAddress(libmikmod, 'MikMod_Active');
    farproc(MikMod_EnableOutput):=GetProcAddress(libmikmod, 'MikMod_EnableOutput');
    farproc(MikMod_DisableOutput):=GetProcAddress(libmikmod, 'MikMod_DisableOutput');
    farproc(MikMod_Update):=GetProcAddress(libmikmod, 'MikMod_Update');
    farproc(MikMod_InitThreads):=GetProcAddress(libmikmod, 'MikMod_InitThreads');
    farproc(MikMod_Lock):=GetProcAddress(libmikmod, 'MikMod_Lock');
    farproc(MikMod_Unlock):=GetProcAddress(libmikmod, 'MikMod_Unlock');


    farproc(Player_Load):=GetProcAddress(libmikmod, 'Player_Load');
    farproc(Player_LoadFP):=GetProcAddress(libmikmod, 'Player_LoadFP');
    farproc(Player_LoadGeneric):=GetProcAddress(libmikmod, 'Player_LoadGeneric');
    farproc(Player_LoadTitle):=GetProcAddress(libmikmod, 'Player_LoadTitle');
    farproc(Player_LoadTitleFP):=GetProcAddress(libmikmod, 'Player_LoadTitleFP');
    farproc(Player_Free):=GetProcAddress(libmikmod, 'Player_Free');
    farproc(Player_Start):=GetProcAddress(libmikmod, 'Player_Start');
    farproc(Player_Active):=GetProcAddress(libmikmod, 'Player_Active');
    farproc(Player_Stop):=GetProcAddress(libmikmod, 'Player_Stop');
    farproc(Player_TogglePause):=GetProcAddress(libmikmod, 'Player_TogglePause');
    farproc(Player_Paused):=GetProcAddress(libmikmod, 'Player_Paused');
    farproc(Player_NextPosition):=GetProcAddress(libmikmod, 'Player_NextPosition');
    farproc(Player_PrevPosition):=GetProcAddress(libmikmod, 'Player_PrevPosition');
    farproc(Player_SetPosition):=GetProcAddress(libmikmod, 'Player_SetPosition');
    farproc(Player_Muted):=GetProcAddress(libmikmod, 'Player_Muted');
    farproc(Player_SetVolume):=GetProcAddress(libmikmod, 'Player_SetVolume');
    farproc(Player_GetModule):=GetProcAddress(libmikmod, 'Player_GetModule');
    farproc(Player_SetSpeed):=GetProcAddress(libmikmod, 'Player_SetSpeed');
    farproc(Player_SetTempo):=GetProcAddress(libmikmod, 'Player_SetTempo');


    md_volume:=GetProcAddress(libmikmod, 'md_volume');
    md_musicvolume:=GetProcAddress(libmikmod, 'md_musicvolume');
    md_sndfxvolume:=GetProcAddress(libmikmod, 'md_sndfxvolume');
    md_reverb:=GetProcAddress(libmikmod, 'md_reverb');
    md_pansep:=GetProcAddress(libmikmod, 'md_pansep');

    md_device:=GetProcAddress(libmikmod, 'md_device');
    md_mixfreq:=GetProcAddress(libmikmod, 'md_mixfreq');
    md_mode:=GetProcAddress(libmikmod, 'md_mode');

    MikMod_errno:=GetProcAddress(libmikmod, 'MikMod_errno');

    result:=true;
  end;
end;

end.

