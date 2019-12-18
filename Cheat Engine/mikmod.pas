unit mikmod;

{$mode objfpc}{$H+}

{
MikMod sound library
half port of the MikMod header file to pascal
}

interface

uses
  {$ifdef darwin}
  mactypes, dynlibs, macport,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}Classes, SysUtils, syncobjs;


type
  PSAMPLE=pointer;
  PMODULE=pointer;
  PREADER=^MREADER;

  MREADER_SEEK=function(self:PREADER; offset:longint; whence:integer):longint;cdecl;
  MREADER_TELL=function(self:PREADER):longint;cdecl;
  MREADER_READ=function(self:PREADER; dest: pointer; l: size_t): BOOL; cdecl;
  MREADER_GET=function(self:PREADER):integer; cdecl;
  MREADER_EOF=function(self:PREADER):BOOL; cdecl;

  MREADER=record
    Seek: MREADER_SEEK;
    Tell: MREADER_TELL;
    Read: MREADER_READ;
    Get:  MREADER_GET;
    EOF:  MREADER_EOF;
    memory: PBYTE;
    size: integer;
    position: integer;
  end;

  TMikModSFX=class
  private
    s: PSAMPLE;
  public
    procedure play;
    constructor create(sample: PSAMPLE);
    destructor destroy; override;
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


  SFX_CRITICAL= 1;

  //Sample format [loading and in-memory] flags:
  SF_16BITS       =$0001;
  SF_STEREO       =$0002;
  SF_SIGNED       =$0004;
  SF_BIG_ENDIAN   =$0008;
  SF_DELTA        =$0010;
  SF_ITPACKED     =$0020;

  SF_FORMATMASK   =$003F;

  // General Playback flags

  SF_LOOP         =$0100;
  SF_BIDI         =$0200;
  SF_REVERSE      =$0400;
  SF_SUSTAIN      =$0800;

  SF_PLAYBACKMASK =$0C00;

  // Module-only Playback Flags

  SF_OWNPAN       =$1000;
  SF_UST_LOOP     =$2000;

  SF_EXTRAPLAYBACKMASK    =$3000;

  //Panning constants
  PAN_LEFT        =0;
  PAN_HALFLEFT    =64;
  PAN_CENTER      =128;
  PAN_HALFRIGHT   =192;
  PAN_RIGHT       =255;
  PAN_SURROUND    =512; // panning value for Dolby Surround


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
  Player_GetLoop: function(module: PMODULE):BOOL; cdecl;
  Player_GetWrap: function(module: PMODULE):BOOL; cdecl;
  Player_SetLoop: procedure(module: PMODULE; state: BOOL); cdecl;
  Player_SetWrap: procedure(module: PMODULE; state: BOOL); cdecl;

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

  Sample_Load: function(filename: pchar): PSAMPLE; cdecl;
  Sample_LoadFP: function(fp: pointer): PSAMPLE; cdecl;
  Sample_LoadGeneric: function(reader: PREADER): PSAMPLE; cdecl;

  Sample_Play: function(s: PSAMPLE; start: ULONG; flags: Word): SmallInt; cdecl;
  Sample_Free: procedure(s: PSAMPLE); cdecl;

  Voice_Stopped: function(v: SmallInt): BOOL; cdecl;



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

  function GenerateMREADER(memory: pointer; size: integer): MREADER;

  procedure MikMod_Play(filename: string; loop: boolean=false);
  procedure MikMod_PlayMemory(memory: pointer; size: integer);
  procedure MikMod_PlayStream(s: TStream; loop: boolean=false);

  function MikMod_CreateSFX(filename: string): TMikModSFX;
//  function MikMod_CreateSFX(memory: pointer): TMikModSFX; overload;
//  function MikMod_CreateSFX(s: TStream): TMikModSFX; overload;


  procedure MikMod_Pause;
  procedure MikMod_Unpause;
  procedure MikMod_Stop;



implementation

uses math;

resourcestring
  rsFailureToInitializeMikMod = 'Failure to initialize MikMod';

const
  MIKMODCMD_PLAYFILE    = 0;
  MIKMODCMD_PLAYMEMORY  = 1;
  MIKMODCMD_PAUSE       = 2;
  MIKMODCMD_UNPAUSE     = 3;
  MIKMODCMD_STOP        = 4;


type
  TMikModThread=class(TThread)
  private
    command: integer;
    filename: string;
    memory: pointer;
    memorysize: integer;
    commandReady: TEvent;
    commandcs: TCriticalSection;

    doloop: boolean;


    memstream: TMemoryStream;
    ready: TEvent;
  public
    procedure waitTillReady;
    procedure play(f: string; loop: boolean=false);
    procedure playMemory(m: pointer; size: integer; loop: boolean=false);
    procedure playStream(s: TStream; loop: boolean=false);
    procedure pause;
    procedure unpause;
    procedure stop;
    procedure execute; override;
    constructor create(LaunchSuspended: boolean);
    destructor destroy; override;
  end;

var
  libmikmod: HModule;
  mikmodthread: TMikModThread;

procedure TMikModThread.play(f: string; loop: boolean=false);
begin
  commandcs.enter;
  self.filename:=f;
  doloop:=loop;
  command:=MIKMODCMD_PLAYFILE;
  commandReady.SetEvent;
  commandcs.leave;
end;

procedure TMikModThread.playMemory(m: pointer; size: integer; loop: boolean=false);
begin
  commandcs.enter;
  memory:=m;
  memorysize:=size;
  doloop:=loop;
  command:=MIKMODCMD_PLAYMEMORY;
  commandReady.SetEvent;
  commandcs.leave;
end;

procedure TMikModThread.playStream(s: TStream; loop: boolean=false);
begin
  if memstream<>nil then
    freeandnil(memstream);

  s.position:=0;
  memstream:=TMemoryStream.create;
  memstream.LoadFromStream(s);

  playMemory(memstream.Memory, memstream.Size, loop);
end;

procedure TMikModThread.pause;
begin
  commandcs.enter;
  command:=MIKMODCMD_PAUSE;
  commandReady.SetEvent;
  commandcs.leave;
end;

procedure TMikModThread.unpause;
begin
  commandcs.enter;
  command:=MIKMODCMD_UNPAUSE;
  commandReady.SetEvent;
  commandcs.leave;
end;

procedure TMikModThread.stop;
begin
  commandcs.enter;
  command:=MIKMODCMD_STOP;
  commandReady.SetEvent;
  commandcs.leave;
end;

procedure TMikModThread.waitTillReady;
begin
  ready.WaitFor(infinite);
end;


procedure TMikModThread.execute;
var m: PMODULE;
  mr: MREADER;

  w,l: BOOL;
begin
  Priority:=tpTimeCritical;
  m:=nil;
  if LoadMikMod then
  begin
    MikMod_RegisterAllDrivers;
    MikMod_RegisterAllLoaders;
    md_mode^:=md_mode^ or DMODE_HQMIXER or DMODE_SOFT_SNDFX;

    if MikMod_Init('')<>0 then
      raise exception.create(rsFailureToInitializeMikMod);

    MikMod_SetNumVoices(-1, 16);
    MikMod_EnableOutput();

    ready.SetEvent;

    try
      while not terminated do
      begin

        if commandReady.WaitFor(ifthen(Player_Active(), 10, 10))=wrSignaled then
        begin
          commandcs.enter;
          case command of
            -1: ; //

            MIKMODCMD_PLAYFILE, MIKMODCMD_PLAYMEMORY:
            begin
              if Player_Active() then
              begin
                Player_Stop();
                MikMod_Update();
              end;

              if m<>nil then
                Player_Free(m);


              if command=MIKMODCMD_PLAYMEMORY then
              begin
                mr:=GenerateMREADER(memory, memorysize);
                m:=Player_LoadGeneric(@mr, 64, FALSE);
              end
              else
                m:=Player_Load(pchar(filename), 64, FALSE);

              if m<>nil then
              begin
                Player_SetWrap(m, doloop);
               // Player_SetLoop(m, false);
                Player_Start(m);
              end;
            end;

            MIKMODCMD_PAUSE:
            begin
              if not Player_Paused() then
                Player_TogglePause();
            end;

            MIKMODCMD_UNPAUSE:
            begin
              if Player_Paused() then
                Player_TogglePause();
            end;

            MIKMODCMD_STOP:
            begin
              if Player_Active() then
                Player_Stop();
            end;

          end;

          command:=-1;
          commandcs.leave;
        end;

       // if Player_Active() then
          MikMod_Update();
      end;
    finally
      MikMod_Exit();
    end;

  end;
end;

constructor TMikModThread.create(LaunchSuspended: Boolean);
begin
  ready:=TEvent.Create(nil, false, false,'');
  commandcs:=TCriticalSection.Create;
  commandReady:=TEvent.Create(nil, false, false,'');
  inherited Create(LaunchSuspended);
end;

destructor TMikModThread.Destroy;
begin
  Terminate;
  if not Finished then
    WaitFor;

  if commandcs<>nil then
    commandcs.free;

  if commandReady<>nil then
    commandReady.free;

  if memstream<>nil then
    freeandnil(memstream);

  inherited destroy;
end;


procedure TMikModSFX.play;
var i: integer;
begin
  MikMod_EnableOutput();
  if Sample_Play(s,0,SFX_CRITICAL or SF_LOOP)=-1 then raise exception.create('fuck');
  MikMod_Update();
end;

constructor TMikModSFX.create(sample: PSAMPLE);
begin
  s:=sample;
end;

destructor TMikModSFX.destroy;
begin
  Sample_Free(s);
  inherited destroy;
end;

function MikMod_CreateSFX(filename: string): TMikModSFX;
var s: PSAMPLE;
begin
  if mikmodthread=nil then
    mikmodthread:=TMikModThread.create(false);

  mikmodthread.waitTillReady;

  s:=Sample_Load(pchar(filename));
  if s<>nil then
    result:=TMikModSFX.create(s)
  else
    result:=nil;
end;

function MikMod_CreateSFX(memory: pointer): TMikModSFX;
begin
  if mikmodthread=nil then
    mikmodthread:=TMikModThread.create(false);

  result:=nil;
end;

function MikMod_CreateSFX(s: TStream): TMikModSFX;
begin
  if mikmodthread=nil then
    mikmodthread:=TMikModThread.create(false);

  result:=nil;
end;


procedure MikMod_Play(filename: string; loop: boolean=false);
begin
  if mikmodthread=nil then
    mikmodthread:=TMikModThread.create(false);

  mikmodthread.play(filename, loop);
end;

procedure MikMod_PlayMemory(memory: pointer; size: integer);
begin
  if mikmodthread=nil then
    mikmodthread:=TMikModThread.create(false);

  mikmodthread.playmemory(memory, size);
end;

procedure MikMod_PlayStream(s: TStream; loop: boolean=false);
begin
  if mikmodthread=nil then
    mikmodthread:=TMikModThread.create(false);

  mikmodthread.playstream(s, loop);
end;

procedure MikMod_Pause;
begin
  if mikmodthread=nil then exit;
  mikmodthread.pause;
end;

procedure MikMod_Unpause;
begin
  if mikmodthread=nil then exit;
  mikmodthread.unpause;
end;

procedure MikMod_Stop;
begin
  if mikmodthread=nil then exit;
  mikmodthread.stop;
end;

//mreader setup

function mr_seek(self:PREADER; offset:longint; whence:integer):longint;cdecl;
const
  Seek_set = 0;
  Seek_Cur = 1;
  Seek_End = 2;
begin
  case whence of
    Seek_set: self^.position:=offset;
    Seek_cur: inc(self^.position, offset);
    Seek_end: self^.position:=self^.size-offset;
  end;

  result:=0;
end;

function mr_tell(self:PREADER):longint;cdecl;
begin
  result:=self^.position;
end;

function mr_read(self:PREADER; dest: pointer; l: size_t): BOOL; cdecl;
begin
  if l>(self^.size-self^.position) then
    l:=self^.size-self^.position;

  copymemory(dest, @self^.memory[self^.position], l);
  inc(self^.position, l);

  result:=true;
end;

function mr_get(self:PREADER):integer; cdecl;
begin
  if self^.EOF(self) then
    result:=-1
  else
  begin
    result:=self^.memory[self^.position];
    inc(self^.position);
  end;
end;

function mr_eof(self:PREADER):BOOL; cdecl;
begin
  result:=self^.position>=self^.size;
end;

function GenerateMREADER(memory: pointer; size: integer): MREADER;
var r: MREADER;
begin
  r.Seek:=@mr_Seek;
  r.Tell:=@mr_Tell;
  r.Read:=@mr_Read;
  r.Get:=@mr_Get;
  r.EOF:=@mr_EOF;
  r.memory:=memory;
  r.size:=size;
  r.position:=0;


  result:=r;
end;

function LoadMikMod: boolean;
begin
  result:=libmikmod<>0;

  if result=false then
  begin
    libmikmod:=loadlibrary('libmikmod'+{$ifdef cpu32}'32'{$else}'64'{$endif}+{$ifdef windows}'.dll'{$endif}{$ifdef darwin}'.dynlib'{$endif});

    if libmikmod<>0 then
    begin
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


      farproc(Player_GetLoop):=GetProcAddress(libmikmod, 'Player_GetLoop');
      farproc(Player_SetLoop):=GetProcAddress(libmikmod, 'Player_SetLoop');
      farproc(Player_GetWrap):=GetProcAddress(libmikmod, 'Player_GetWrap');
      farproc(Player_SetWrap):=GetProcAddress(libmikmod, 'Player_SetWrap');


      farproc(Sample_Load):=GetProcAddress(libmikmod, 'Sample_Load');
      farproc(Sample_LoadFP):=GetProcAddress(libmikmod, 'Sample_LoadFP');
      farproc(Sample_LoadGeneric):=GetProcAddress(libmikmod, 'Sample_LoadGeneric');

      farproc(Sample_Play):=GetProcAddress(libmikmod, 'Sample_Play');
      farproc(Sample_Free):=GetProcAddress(libmikmod, 'Sample_Free');

      farproc(Voice_Stopped):=GetProcAddress(libmikmod, 'Voice_Stopped');









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
end;

finalization
  if mikmodthread<>nil then
  begin
    mikmodthread.Terminate;
    mikmodthread.free;
  end;




end.

