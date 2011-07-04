{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclMIDI.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Robert Rossmair.                                   }
{ Portions created by Robert Rossmair are Copyright (C) Robert Rossmair. All rights reserved.      }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Robert Rossmair                                                                                }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Platform-independent MIDI declarations                                                           }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclMIDI;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes,
  JclBase;

// manifest constants for MIDI message protocol
const
  // MIDI Status Bytes for Channel Voice Messages
  MIDIMsgNoteOff             = $80;
  MIDIMsgNoteOn              = $90;
  MIDIMsgPolyKeyPressure     = $A0;
  MIDIMsgControlChange       = $B0;
  MIDIMsgProgramChange       = $C0;
  MIDIMsgChannelKeyPressure  = $D0;
  MIDIMsgAftertouch = MIDIMsgChannelKeyPressure; // Synonym
  MIDIMsgPitchWheelChange    = $E0;
  // MIDI Status Bytes for System Common Messages
  MIDIMsgSysEx               = $F0;
  MIDIMsgMTCQtrFrame         = $F1; // MIDI Time Code Qtr. Frame
  MIDIMsgSongPositionPtr     = $F2;
  MIDIMsgSongSelect          = $F3;
  MIDIMsgTuneRequest         = $F6;
  MIDIMsgEOX                 = $F7; // marks end of system exclusive message

  // MIDI Status Bytes for System Real-Time Messages
  MIDIMsgTimingClock         = $F8;
  MIDIMsgStartSequence       = $FA;
  MIDIMsgContinueSequence    = $FB;
  MIDIMsgStopSequence        = $FC;
  MIDIMsgActiveSensing       = $FE;
  MIDIMsgSystemReset         = $FF;

  // MIDICC...: MIDI Control Change Messages

  // Continuous Controllers MSB
  MIDICCBankSelect         = $00;
  MIDICCModulationWheel    = $01;
  MIDICCBreathControl      = $02;
  MIDICCFootController     = $04;
  MIDICCPortamentoTime     = $05;
  MIDICCDataEntry          = $06;
  MIDICCChannelVolume      = $07;
  MIDICCMainVolume = MIDICCChannelVolume;
  MIDICCBalance            = $08;
  MIDICCPan                = $0A;
  MIDICCExpression         = $0B;
  MIDICCEffectControl      = $0C;
  MIDICCEffectControl2     = $0D;
  MIDICCGeneralPurpose1    = $10;
  MIDICCGeneralPurpose2    = $11;
  MIDICCGeneralPurpose3    = $12;
  MIDICCGeneralPurpose4    = $13;
  // Continuous Controllers LSB
  MIDICCBankSelectLSB      = $20;
  MIDICCModulationWheelLSB = $21;
  MIDICCBreathControlLSB   = $22;
  MIDICCFootControllerLSB  = $24;
  MIDICCPortamentoTimeLSB  = $25;
  MIDICCDataEntryLSB       = $26;
  MIDICCChannelVolumeLSB   = $27;
  MIDICCMainVolumeLSB = MIDICCChannelVolumeLSB;
  MIDICCBalanceLSB         = $28;
  MIDICCPanLSB             = $2A;
  MIDICCExpressionLSB      = $2B;
  MIDICCEffectControlLSB   = $2C;
  MIDICCEffectControl2LSB  = $2D;
  MIDICCGeneralPurpose1LSB = $30;
  MIDICCGeneralPurpose2LSB = $31;
  MIDICCGeneralPurpose3LSB = $32;
  MIDICCGeneralPurpose4LSB = $33;
  // Switches
  MIDICCSustain            = $40;
  MIDICCPortamento         = $41;
  MIDICCSustenuto          = $42;
  MIDICCSoftPedal          = $43;
  MIDICCLegato             = $44;
  MIDICCHold2              = $45;

  MIDICCSound1             = $46; // (Sound Variation)
  MIDICCSound2             = $47; // (Timbre/Harmonic Intens.)
  MIDICCSound3             = $48; // (Release Time)
  MIDICCSound4             = $49; // (Attack Time)
  MIDICCSound5             = $4A; // (Brightness)
  MIDICCSound6             = $4B; // (Decay Time)
  MIDICCSound7             = $4C; // (Vibrato Rate)
  MIDICCSound8             = $4D; // (Vibrato Depth)
  MIDICCSound9             = $4E; // (Vibrato Delay)
  MIDICCSound10            = $4F; //

  MIDICCGeneralPurpose5    = $50;
  MIDICCGeneralPurpose6    = $51;
  MIDICCGeneralPurpose7    = $52;
  MIDICCGeneralPurpose8    = $53;
  MIDICCPortamentoControl  = $54;

  MIDICCReverbSendLevel    = $5B;
  MIDICCEffects2Depth      = $5C;
  MIDICCTremoloDepth = MIDICCEffects2Depth;
  MIDICCChorusSendLevel    = $5D;
  MIDICCEffects4Depth      = $5E;
  MIDICCCelesteDepth = MIDICCEffects4Depth;
  MIDICCEffects5Depth      = $5F;
  MIDICCPhaserDepth = MIDICCEffects5Depth;

  MIDICCDataEntryInc       = $60;
  MIDICCDataEntryDec       = $61;
  MIDICCNonRegParamNumLSB  = $62;
  MIDICCNonRegParamNumMSB  = $63;
  MIDICCRegParamNumLSB     = $64;
  MIDICCRegParamNumMSB     = $65;

//  Registered Parameter Numbers [CC# 65H,64H]
// -----------------------------------------------------------
//  CC#65 (MSB) | CC#64 (LSB) | Function
//  Hex|Dec|    |  Hex|Dec|   |
//  - - - - - - | - - - - - - |- - - - - - - - - - - - - - - -
//   00 = 0     |  00 = 0     | Pitch Bend Sensitivity
//   00 = 0     |  01 = 1     | Channel Fine Tuning
//   00 = 0     |  02 = 2     | Channel Coarse Tuning
//   00 = 0     |  03 = 3     | Tuning Program Change
//   00 = 0     |  04 = 4     | Tuning Bank Select

  // Channel Mode Messages (Control Change >= $78)
  MIDICCAllSoundOff        = $78;
  MIDICCResetAllControllers = $79;
  MIDICCLocalControl       = $7A;
  MIDICCAllNotesOff        = $7B;

  MIDICCOmniModeOff        = $7C;
  MIDICCOmniModeOn         = $7D;
  MIDICCMonoModeOn         = $7E;
  MIDICCPolyModeOn         = $7F;

type
  TMIDIChannel          = 1..16;
  TMIDIDataByte         = 0..$7F;           //  7 bits
  TMIDIDataWord         = 0..$3FFF;         // 14 bits
  TMIDIStatusByte       = $80..$FF;
  TMIDIVelocity         = TMIDIDataByte;
  TMIDIKey              = TMIDIDataByte;
  TMIDINote             = TMIDIKey;

const
  // Helper definitions
  MIDIDataMask          = $7F;
  MIDIDataWordMask      = $3FFF;
  MIDIChannelMsgMask    = $F0;
  MIDIInvalidStatus     = TMIDIStatusByte(0);
  BitsPerMIDIDataByte   = 7;
  BitsPerMIDIDataWord   = BitsPerMIDIDataByte * 2;
  MIDIPitchWheelCenter  = 1 shl (BitsPerMIDIDataWord - 1);

type
  TMIDINotes = set of TMIDINote;

  TSingleNoteTuningData = packed record
  case Integer of
    0:
      (Key: TMIDINote; Frequency: array [0..2] of TMIDIDataByte);
    1:
      (DWord: LongWord);
  end;

  EJclMIDIError = class(EJclError);

// MIDI Out
  IJclMIDIOut = interface
    ['{A29C3EBD-EB70-4C72-BEC5-700AF57FD4C8}']
    // property access methods
    function GetActiveNotes(Channel: TMIDIChannel): TMIDINotes;
    function GetName: string;
    function GetMIDIStatus: TMIDIStatusByte;
    function GetRunningStatusEnabled: Boolean;
    procedure SetRunningStatusEnabled(const Value: Boolean);
    // General message send method
    procedure SendMessage(const Data: array of Byte);
    // Channel Voice Messages
    procedure SendNoteOff(Channel: TMIDIChannel; Key: TMIDINote; Velocity: TMIDIDataByte = $40);
    procedure SendNoteOn(Channel: TMIDIChannel; Key: TMIDINote; Velocity: TMIDIDataByte);
    procedure SendPolyphonicKeyPressure(Channel: TMIDIChannel; Key: TMIDINote; Value: TMIDIDataByte);
    procedure SendControlChange(Channel: TMIDIChannel; ControllerNum, Value: TMIDIDataByte);
    // High Resolution "macro" for controller numbers <= $13, sends upper 7 bits first,
    //   lower 7 bits per additional <controller name>LSB message afterwards
    procedure SendControlChangeHR(Channel: TMIDIChannel; ControllerNum: TMIDIDataByte; Value: TMIDIDataWord);
    procedure SendSwitchChange(Channel: TMIDIChannel; ControllerNum: TMIDIDataByte; Value: Boolean);
    procedure SendProgramChange(Channel: TMIDIChannel; ProgramNum: TMIDIDataByte);
    procedure SendChannelPressure(Channel: TMIDIChannel; Value: TMIDIDataByte);
    procedure SendPitchWheelChange(Channel: TMIDIChannel; Value: TMIDIDataWord);
    procedure SendPitchWheelPos(Channel: TMIDIChannel; Value: Single);
    // Control Change Messages
    procedure SelectProgram(Channel: TMIDIChannel; BankNum: TMIDIDataWord; ProgramNum: TMIDIDataByte);
    procedure SendModulationWheelChange(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure SendBreathControlChange(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure SendFootControllerChange(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure SendPortamentoTimeChange(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure SendDataEntry(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure SendChannelVolumeChange(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure SendBalanceChange(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure SendPanChange(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure SendExpressionChange(Channel: TMIDIChannel; Value: TMidiDataByte);
    // "high resolution" variants
    procedure SendModulationWheelChangeHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure SendBreathControlChangeHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure SendFootControllerChangeHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure SendPortamentoTimeChangeHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure SendDataEntryHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure SendChannelVolumeChangeHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure SendBalanceChangeHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure SendPanChangeHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure SendExpressionChangeHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    // Control Change Messages: Switches
    procedure SwitchSustain(Channel: TMIDIChannel; Value: Boolean);
    procedure SwitchPortamento(Channel: TMIDIChannel; Value: Boolean);
    procedure SwitchSostenuto(Channel: TMIDIChannel; Value: Boolean);
    procedure SwitchSoftPedal(Channel: TMIDIChannel; Value: Boolean);
    procedure SwitchLegato(Channel: TMIDIChannel; Value: Boolean);
    procedure SwitchHold2(Channel: TMIDIChannel; Value: Boolean);
    // Channel Mode Messages
    procedure SwitchAllSoundOff(Channel: TMIDIChannel);
    procedure ResetAllControllers(Channel: TMIDIChannel);
    procedure SwitchLocalControl(Channel: TMIDIChannel; Value: Boolean);
    procedure SwitchAllNotesOff(Channel: TMIDIChannel);
    procedure SwitchOmniModeOff(Channel: TMIDIChannel);
    procedure SwitchOmniModeOn(Channel: TMIDIChannel);
    procedure SwitchMonoModeOn(Channel: TMIDIChannel; ChannelCount: Integer);
    procedure SwitchPolyModeOn(Channel: TMIDIChannel);
    //
    procedure SendSingleNoteTuningChange(const TargetDeviceID, TuningProgramNum: TMidiDataByte;
      const TuningData: array of TSingleNoteTuningData);
    function NoteIsOn(Channel: TMIDIChannel; Key: TMIDINote): Boolean;
    procedure SwitchActiveNotesOff(Channel: TMIDIChannel); overload;
    procedure SwitchActiveNotesOff; overload;
    // Properties
    property ActiveNotes[Channel: TMIDIChannel]: TMIDINotes read GetActiveNotes;
    property Name: string read GetName;
    property LocalControl[Channel: TMIDIChannel]: Boolean write SwitchLocalControl;
    property MIDIStatus: TMIDIStatusByte read GetMIDIStatus;
      // Tribute to some braindead devices which cannot handle running status (e.g. ESS Solo 1 Win2k driver)
    property RunningStatusEnabled: Boolean read GetRunningStatusEnabled write SetRunningStatusEnabled;
  end;

  // Abstract MIDI Out device class
  TJclMIDIOut = class(TInterfacedObject, IJclMIDIOut)
  private
    FMIDIStatus: TMIDIStatusByte;
    FRunningStatusEnabled: Boolean;
    FActiveNotes: array [TMIDIChannel] of TMIDINotes;
  protected
    function GetActiveNotes(Channel: TMIDIChannel): TMIDINotes;
    function GetName: string; virtual; abstract;
    function GetMIDIStatus: TMIDIStatusByte;
    function IsRunningStatus(StatusByte: TMIDIStatusByte): Boolean;
    function GetRunningStatusEnabled: Boolean;
    procedure SetRunningStatusEnabled(const Value: Boolean);
    procedure SendChannelMessage(Msg: TMIDIStatusByte; Channel: TMIDIChannel;
      Data1, Data2: TMIDIDataByte);
    procedure DoSendMessage(const Data: array of Byte); virtual; abstract;
    procedure SendMessage(const Data: array of Byte);
  public
    destructor Destroy; override;
    // Channel Voice Messages
    procedure SendNoteOff(Channel: TMIDIChannel; Key: TMIDINote; Velocity: TMIDIDataByte = $40);
    procedure SendNoteOn(Channel: TMIDIChannel; Key: TMIDINote; Velocity: TMIDIDataByte);
    procedure SendPolyphonicKeyPressure(Channel: TMIDIChannel; Key: TMIDINote; Value: TMIDIDataByte);
    procedure SendControlChange(Channel: TMIDIChannel; ControllerNum, Value: TMIDIDataByte);
    procedure SendControlChangeHR(Channel: TMIDIChannel; ControllerNum: TMIDIDataByte; Value: TMIDIDataWord);
    procedure SendSwitchChange(Channel: TMIDIChannel; ControllerNum: TMIDIDataByte; Value: Boolean);
    procedure SendProgramChange(Channel: TMIDIChannel; ProgramNum: TMIDIDataByte);
    procedure SendChannelPressure(Channel: TMIDIChannel; Value: TMIDIDataByte);
    procedure SendPitchWheelChange(Channel: TMIDIChannel; Value: TMIDIDataWord);
    procedure SendPitchWheelPos(Channel: TMIDIChannel; Value: Single);
    // Control Change Messages
    procedure SelectProgram(Channel: TMIDIChannel; BankNum: TMIDIDataWord; ProgramNum: TMIDIDataByte);
    procedure SendModulationWheelChange(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure SendBreathControlChange(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure SendFootControllerChange(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure SendPortamentoTimeChange(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure SendDataEntry(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure SendChannelVolumeChange(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure SendBalanceChange(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure SendPanChange(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure SendExpressionChange(Channel: TMIDIChannel; Value: TMidiDataByte);
    // ...high Resolution
    procedure SendModulationWheelChangeHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure SendBreathControlChangeHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure SendFootControllerChangeHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure SendPortamentoTimeChangeHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure SendDataEntryHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure SendChannelVolumeChangeHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure SendBalanceChangeHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure SendPanChangeHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure SendExpressionChangeHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    // Control Change Messages: Switches
    procedure SwitchSustain(Channel: TMIDIChannel; Value: Boolean);
    procedure SwitchPortamento(Channel: TMIDIChannel; Value: Boolean);
    procedure SwitchSostenuto(Channel: TMIDIChannel; Value: Boolean);
    procedure SwitchSoftPedal(Channel: TMIDIChannel; Value: Boolean);
    procedure SwitchLegato(Channel: TMIDIChannel; Value: Boolean);
    procedure SwitchHold2(Channel: TMIDIChannel; Value: Boolean);
    // Channel Mode Messages
    procedure SwitchAllSoundOff(Channel: TMIDIChannel);
    procedure ResetAllControllers(Channel: TMIDIChannel);
    procedure SwitchLocalControl(Channel: TMIDIChannel; Value: Boolean);
    procedure SwitchAllNotesOff(Channel: TMIDIChannel);
    procedure SwitchOmniModeOff(Channel: TMIDIChannel);
    procedure SwitchOmniModeOn(Channel: TMIDIChannel);
    procedure SwitchMonoModeOn(Channel: TMIDIChannel; ChannelCount: Integer);
    procedure SwitchPolyModeOn(Channel: TMIDIChannel);
    //
    procedure SendSingleNoteTuningChange(const TargetDeviceID, TuningProgramNum: TMidiDataByte;
      const TuningData: array of TSingleNoteTuningData);
    function NoteIsOn(Channel: TMIDIChannel; Key: TMIDINote): Boolean;
    procedure SwitchActiveNotesOff(Channel: TMIDIChannel); overload;
    procedure SwitchActiveNotesOff; overload;
    property ActiveNotes[Channel: TMIDIChannel]: TMIDINotes read GetActiveNotes;
    property Name: string read GetName;
    property RunningStatusEnabled: Boolean read GetRunningStatusEnabled write SetRunningStatusEnabled;
  end;

function MIDIOut(DeviceID: Cardinal = 0): IJclMIDIOut;
procedure GetMidiOutputs(const List: TStrings);
function MIDISingleNoteTuningData(Key: TMIDINote; Frequency: Single): TSingleNoteTuningData;
function MIDINoteToStr(Note: TMIDINote): string;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclMIDI.pas $';
    Revision: '$Revision: 2175 $';
    Date: '$Date: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils,
  {$IFDEF MSWINDOWS}
  JclWinMIDI,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  //JclUnixMIDI,
  {$ENDIF UNIX}
  JclResources;

{$IFDEF UNIX}
procedure ErrorNotImplemented;
begin
  raise EJclInternalError.CreateRes(@RsMidiNotImplemented);
end;
{$ENDIF UNIX}

function MIDIOut(DeviceID: Cardinal = 0): IJclMIDIOut;
begin
  Result := nil;
  {$IFDEF MSWINDOWS}
  Result := JclWinMIDI.MIDIOut(DeviceID);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  { TODO -oRobert Rossmair : Unix MIDI Out }
  //Result := JclUnixMIDI.MidiOut(DeviceID);
  ErrorNotImplemented;
  {$ENDIF UNIX}
end;

procedure GetMidiOutputs(const List: TStrings);
begin
  {$IFDEF MSWINDOWS}
  JclWinMIDI.GetMidiOutputs(List);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  { TODO -oRobert Rossmair : Unix GetMIDIOutputs }
  //JclUnixMIDI.GetMidiOutputs(List);
  ErrorNotImplemented;
  {$ENDIF UNIX}
end;

function MIDISingleNoteTuningData(Key: TMIDINote; Frequency: Single): TSingleNoteTuningData;
var
  F: Cardinal;
begin
  Result.Key := Key;
  F := Trunc(Frequency * (1 shl BitsPerMIDIDataWord));
  Result.Frequency[0] := (F shr BitsPerMIDIDataWord) and MIDIDataMask;
  Result.Frequency[1] := (F shr BitsPerMIDIDataByte) and MIDIDataMask;
  Result.Frequency[2] := F and MIDIDataMask;
end;

procedure CheckMIDIChannelNum(Channel: TMIDIChannel);
begin
  if (Channel < Low(TMIDIChannel)) or (Channel > High(TMIDIChannel)) then
    raise EJclMIDIError.CreateResFmt(@RsMidiInvalidChannelNum, [Channel]);
end;

function MIDINoteToStr(Note: TMIDINote): string;
const
  HalftonesPerOctave = 12;
begin
  case Note mod HalftonesPerOctave of
     0:
       Result := RsOctaveC;
     1:
       Result := RsOctaveCSharp;
     2:
       Result := RsOctaveD;
     3:
       Result := RsOctaveDSharp;
     4:
       Result := RsOctaveE;
     5:
       Result := RsOctaveF;
     6:
       Result := RsOctaveFSharp;
     7:
       Result := RsOctaveG;
     8:
       Result := RsOctaveGSharp;
     9:
       Result := RsOctaveA;
    10:
      Result := RsOctaveASharp;
    11:
      Result := RsOctaveB;
  end;
  Result := Format('%s%d', [Result, Note div HalftonesPerOctave - 2]);
end;

// TJclMIDIOut
destructor TJclMIDIOut.Destroy;
begin
  SwitchActiveNotesOff;
  inherited Destroy;
end;

function TJclMIDIOut.GetActiveNotes(Channel: TMIDIChannel): TMIDINotes;
begin
  CheckMIDIChannelNum(Channel);
  Result := FActiveNotes[Channel];
end;

procedure TJclMIDIOut.SendChannelMessage(Msg: TMIDIStatusByte;
  Channel: TMIDIChannel; Data1, Data2: TMIDIDataByte);
begin
  SendMessage([Msg or (Channel - Low(Channel)), Data1, Data2]);
end;

function TJclMIDIOut.GetRunningStatusEnabled: Boolean;
begin
  Result := FRunningStatusEnabled;
end;

function TJclMIDIOut.NoteIsOn(Channel: TMIDIChannel; Key: TMIDINote): Boolean;
begin
  Result := Key in FActiveNotes[Channel];
end;

procedure TJclMIDIOut.SendNoteOff(Channel: TMIDIChannel; Key: TMIDINote; Velocity: TMIDIDataByte);
begin
  SendChannelMessage(MIDIMsgNoteOff, Channel, Key, Velocity);
  Exclude(FActiveNotes[Channel], Key);
end;

procedure TJclMIDIOut.SendNoteOn(Channel: TMIDIChannel; Key: TMIDINote; Velocity: TMIDIDataByte);
begin
  SendChannelMessage(MIDIMsgNoteOn, Channel, Key, Velocity);
  Include(FActiveNotes[Channel], Key);
end;

procedure TJclMIDIOut.SendPolyphonicKeyPressure(Channel: TMIDIChannel;
  Key: TMIDINote; Value: TMIDIDataByte);
begin
  SendChannelMessage(MIDIMsgPolyKeyPressure, Channel, Key, Value);
end;

procedure TJclMIDIOut.SendControlChange(Channel: TMIDIChannel; ControllerNum, Value: TMIDIDataByte);
begin
  SendChannelMessage(MIDIMsgControlChange, Channel, ControllerNum, Value);
end;

procedure TJclMIDIOut.SendControlChangeHR(Channel: TMIDIChannel; ControllerNum: TMIDIDataByte;
  Value: TMIDIDataWord);
begin
  SendControlChange(Channel, ControllerNum, Value shr BitsPerMIDIDataByte and MIDIDataMask);
  if ControllerNum <= $13 then
    SendControlChange(Channel, ControllerNum or $20, Value and MIDIDataMask);
end;

procedure TJclMIDIOut.SendSwitchChange(Channel: TMIDIChannel; ControllerNum: TMIDIDataByte; Value: Boolean);
const
  DataByte: array [Boolean] of Byte = ($00, $7F);
begin
  SendChannelMessage(MIDIMsgControlChange, Channel, ControllerNum, DataByte[Value]);
end;

procedure TJclMIDIOut.SendProgramChange(Channel: TMIDIChannel; ProgramNum: TMIDIDataByte);
begin
  SendChannelMessage(MIDIMsgProgramChange, Channel, ProgramNum, 0);
end;

procedure TJclMIDIOut.SendChannelPressure(Channel: TMIDIChannel; Value: TMIDIDataByte);
begin
  SendChannelMessage(MIDIMsgChannelKeyPressure, Channel, Value, 0);
end;

procedure TJclMIDIOut.SendPitchWheelChange(Channel: TMIDIChannel; Value: TMIDIDataWord);
begin
  SendChannelMessage(MIDIMsgPitchWheelChange, Channel, Value and MidiDataMask, Value shr BitsPerMIDIDataByte);
end;

procedure TJclMIDIOut.SendPitchWheelPos(Channel: TMIDIChannel; Value: Single);
var
  Temp: TMIDIDataWord;
begin
  if Value < 0 then
    Temp := Round(Value * (1 shl 13))
  else
    Temp := Round(Value * (1 shl 13 - 1));
  SendPitchWheelChange(Channel, Temp);
end;

procedure TJclMIDIOut.SwitchAllSoundOff(Channel: TMIDIChannel);
begin
  SendControlChange(Channel, MIDICCAllSoundOff, 0);
end;

procedure TJclMIDIOut.SwitchLocalControl(Channel: TMIDIChannel; Value: Boolean);
begin
  SendSwitchChange(Channel, MIDICCLocalControl, Value);
end;

procedure TJclMIDIOut.ResetAllControllers(Channel: TMIDIChannel);
begin
  SendControlChange(Channel, MIDICCResetAllControllers, 0);
end;

procedure TJclMIDIOut.SwitchAllNotesOff(Channel: TMIDIChannel);
begin
  CheckMIDIChannelNum(Channel);
  SendControlChange(Channel, MIDICCAllNotesOff, 0);
  FActiveNotes[Channel] := [];
end;

procedure TJclMIDIOut.SetRunningStatusEnabled(const Value: Boolean);
begin
  FMIDIStatus := MIDIInvalidStatus;
  FRunningStatusEnabled := Value;
end;

procedure TJclMIDIOut.SendSingleNoteTuningChange(const TargetDeviceID, TuningProgramNum: TMidiDataByte;
  const TuningData: array of TSingleNoteTuningData);
var
  BufSize, Count: Integer;
  Buf: array of Byte;
begin
  Count := High(TuningData) - Low(TuningData) + 1;
  BufSize := 8 + Count * SizeOf(TSingleNoteTuningData);
  SetLength(Buf, BufSize);
  Buf[0] := MIDIMsgSysEx;      // Universal Real Time SysEx header, first byte
  Buf[1] := $7F;               // second byte
  Buf[2] := TargetDeviceID;    // ID of target device (?)
  Buf[3] := 8;                 // sub-ID#1 (MIDI Tuning)
  Buf[4] := 2;                 // sub-ID#2 (note change)
  Buf[5] := TuningProgramNum;  // tuning program number (0 – 127)
  Buf[6] := Count;
  Move(TuningData, Buf[7], Count * SizeOf(TSingleNoteTuningData));
  Buf[BufSize - 1] := MIDIMsgEOX;
  SendMessage(Buf);
end;

procedure TJclMIDIOut.SwitchActiveNotesOff(Channel: TMIDIChannel);
var
  Note: TMIDINote;
begin
  CheckMIDIChannelNum(Channel);
  if FActiveNotes[Channel] <> [] then
    for Note := Low(Note) to High(Note) do
      if Note in FActiveNotes[Channel] then
        SendNoteOff(Channel, Note, $7F);
end;

procedure TJclMIDIOut.SwitchActiveNotesOff;
var
  Channel: TMIDIChannel;
begin
  for Channel := Low(Channel) to High(Channel) do
    SwitchActiveNotesOff(Channel);
end;

procedure TJclMIDIOut.SelectProgram(Channel: TMIDIChannel;
  BankNum: TMIDIDataWord; ProgramNum: TMIDIDataByte);
begin
  SendControlChangeHR(Channel, MIDICCBankSelect, BankNum);
  SendProgramChange(Channel, ProgramNum);
end;

procedure TJclMIDIOut.SendMessage(const Data: array of Byte);
begin
  if IsRunningStatus(Data[0]) then
    {$IFDEF FPC}
    DoSendMessage(PJclByteArray(@Data[1])^)
    {$ELSE}
    DoSendMessage(Slice(Data, 1))
    {$ENDIF FPC}
  else
    DoSendMessage(Data);
end;

function TJclMIDIOut.GetMIDIStatus: TMIDIStatusByte;
begin
  Result := FMIDIStatus;
end;

function TJclMIDIOut.IsRunningStatus(StatusByte: TMIDIStatusByte): Boolean;
begin
  Result := (StatusByte = FMIDIStatus) and
    ((StatusByte and $F0) <> $F0) and       // is channel message
    RunningStatusEnabled;
end;

procedure TJclMIDIOut.SendBalanceChange(Channel: TMIDIChannel; Value: TMidiDataByte);
begin
  SendControlChange(Channel, MIDICCBalance, Value);
end;

procedure TJclMIDIOut.SendBalanceChangeHR(Channel: TMIDIChannel; Value: TMidiDataWord);
begin
  SendControlChangeHR(Channel, MIDICCBalance, Value);
end;

procedure TJclMIDIOut.SendBreathControlChange(Channel: TMIDIChannel; Value: TMidiDataByte);
begin
  SendControlChange(Channel, MIDICCBreathControl, Value);
end;

procedure TJclMIDIOut.SendBreathControlChangeHR(Channel: TMIDIChannel; Value: TMidiDataWord);
begin
  SendControlChangeHR(Channel, MIDICCBreathControl, Value);
end;

procedure TJclMIDIOut.SendDataEntry(Channel: TMIDIChannel; Value: TMidiDataByte);
begin
  SendControlChange(Channel, MIDICCDataEntry, Value);
end;

procedure TJclMIDIOut.SendDataEntryHR(Channel: TMIDIChannel; Value: TMidiDataWord);
begin
  SendControlChangeHR(Channel, MIDICCDataEntry, Value);
end;

procedure TJclMIDIOut.SendExpressionChange(Channel: TMIDIChannel; Value: TMidiDataByte);
begin
  SendControlChange(Channel, MIDICCExpression, Value);
end;

procedure TJclMIDIOut.SendExpressionChangeHR(Channel: TMIDIChannel; Value: TMidiDataWord);
begin
  SendControlChangeHR(Channel, MIDICCExpression, Value);
end;

procedure TJclMIDIOut.SendFootControllerChange(Channel: TMIDIChannel; Value: TMidiDataByte);
begin
  SendControlChange(Channel, MIDICCFootController, Value);
end;

procedure TJclMIDIOut.SendFootControllerChangeHR(Channel: TMIDIChannel; Value: TMidiDataWord);
begin
  SendControlChangeHR(Channel, MIDICCFootController, Value);
end;

procedure TJclMIDIOut.SwitchHold2(Channel: TMIDIChannel; Value: Boolean);
begin
  SendSwitchChange(Channel, MIDICCHold2, Value);
end;

procedure TJclMIDIOut.SwitchLegato(Channel: TMIDIChannel; Value: Boolean);
begin
  SendSwitchChange(Channel, MIDICCLegato, Value);
end;

procedure TJclMIDIOut.SendChannelVolumeChange(Channel: TMIDIChannel;
  Value: TMidiDataByte);
begin
  SendControlChange(Channel, MIDICCChannelVolume, Value);
end;

procedure TJclMIDIOut.SendChannelVolumeChangeHR(Channel: TMIDIChannel;
  Value: TMidiDataWord);
begin
  SendControlChangeHR(Channel, MIDICCChannelVolume, Value);
end;

procedure TJclMIDIOut.SendModulationWheelChange(Channel: TMIDIChannel;
  Value: TMidiDataByte);
begin
  SendControlChange(Channel, MIDICCModulationWheel, Value);
end;

procedure TJclMIDIOut.SendModulationWheelChangeHR(Channel: TMIDIChannel; Value: TMidiDataWord);
begin
  SendControlChangeHR(Channel, MIDICCModulationWheel, Value);
end;

procedure TJclMIDIOut.SendPanChange(Channel: TMIDIChannel; Value: TMidiDataByte);
begin
  SendControlChange(Channel, MIDICCPan, Value);
end;

procedure TJclMIDIOut.SendPanChangeHR(Channel: TMIDIChannel; Value: TMidiDataWord);
begin
  SendControlChangeHR(Channel, MIDICCPan, Value);
end;

procedure TJclMIDIOut.SwitchPortamento(Channel: TMIDIChannel; Value: Boolean);
begin
  SendSwitchChange(Channel, MIDICCPortamento, Value);
end;

procedure TJclMIDIOut.SendPortamentoTimeChange(Channel: TMIDIChannel;
  Value: TMidiDataByte);
begin
  SendControlChange(Channel, MIDICCPortamentoTime, Value);
end;

procedure TJclMIDIOut.SendPortamentoTimeChangeHR(Channel: TMIDIChannel;
  Value: TMidiDataWord);
begin
  SendControlChangeHR(Channel, MIDICCPortamentoTime, Value);
end;

procedure TJclMIDIOut.SwitchSoftPedal(Channel: TMIDIChannel; Value: Boolean);
begin
  SendSwitchChange(Channel, MIDICCSoftPedal, Value);
end;

procedure TJclMIDIOut.SwitchSustain(Channel: TMIDIChannel; Value: Boolean);
begin
  SendSwitchChange(Channel, MIDICCSustain, Value);
end;

procedure TJclMIDIOut.SwitchSostenuto(Channel: TMIDIChannel; Value: Boolean);
begin
  SendSwitchChange(Channel, MIDICCSustenuto, Value);
end;

procedure TJclMIDIOut.SwitchOmniModeOff(Channel: TMIDIChannel);
begin
  SendControlChange(Channel, MIDICCOmniModeOff, 0);
  FActiveNotes[Channel] := []; // implicit All Notes Off
end;

procedure TJclMIDIOut.SwitchOmniModeOn(Channel: TMIDIChannel);
begin
  SendControlChange(Channel, MIDICCOmniModeOn, 0);
  FActiveNotes[Channel] := []; // implicit All Notes Off
end;

procedure TJclMIDIOut.SwitchMonoModeOn(Channel: TMIDIChannel; ChannelCount: Integer);
begin
  SendControlChange(Channel, MIDICCMonoModeOn, ChannelCount);
  FActiveNotes[Channel] := []; // implicit All Notes Off
end;

procedure TJclMIDIOut.SwitchPolyModeOn(Channel: TMIDIChannel);
begin
  SendControlChange(Channel, MIDICCPolyModeOn, 0);
  FActiveNotes[Channel] := []; // implicit All Notes Off
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
