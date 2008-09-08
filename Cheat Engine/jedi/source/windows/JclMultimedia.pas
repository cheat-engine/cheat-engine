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
{ The Original Code is JclMultimedia.pas.                                                          }
{                                                                                                  }
{ The Initial Developers of the Original Code are Marcel van Brakel and Bernhard Berger.           }
{ Portions created by these individuals are Copyright (C) of these individuals.                    }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Marcel van Brakel                                                                              }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Contains a high performance timer based on the MultiMedia API and a routine to open or close the }
{ CD-ROM drive.                                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclMultimedia;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Classes, MMSystem, Contnrs,
  JclBase, JclSynch, JclStrings;

type
  {$IFDEF FPC}
  // declarations missing from mmsystem.pp
  // see also implementation section
  TTimeCaps = TIMECAPS;
  TMixerControl = MIXERCONTROL;
  TMixerCaps = MIXERCAPS;
  TMixerLine = MIXERLINE;
  TMCI_Open_Parms = MCI_OPEN_PARMS;
  {$ENDIF FPC}

  // Multimedia timer
  TMmTimerKind = (tkOneShot, tkPeriodic);
  TMmNotificationKind = (nkCallback, nkSetEvent, nkPulseEvent);

  TJclMultimediaTimer = class(TObject)
  private
    FEvent: TJclEvent;
    FKind: TMmTimerKind;
    FNotification: TMmNotificationKind;
    FOnTimer: TNotifyEvent;
    FPeriod: Cardinal;
    FStartTime: Cardinal;
    FTimeCaps: TTimeCaps;
    FTimerId: Cardinal;
    function GetMinMaxPeriod(Index: Integer): Cardinal;
    procedure SetPeriod(Value: Cardinal);
  protected
    procedure Timer(Id: Cardinal); virtual;
  public
    constructor Create(Kind: TMmTimerKind; Notification: TMmNotificationKind);
    destructor Destroy; override;
    class function GetTime: Cardinal;
    class function BeginPeriod(const Period: Cardinal): Boolean; { TODO -cHelp : Doc }
    class function EndPeriod(const Period: Cardinal): Boolean;   { TODO -cHelp : Doc }
    procedure BeginTimer(const Delay, Resolution: Cardinal);
    procedure EndTimer;
    function Elapsed(const Update: Boolean): Cardinal;
    function WaitFor(const TimeOut: Cardinal): TJclWaitResult;
    property Event: TJclEvent read FEvent;
    property Kind: TMmTimerKind read FKind;
    property MaxPeriod: Cardinal index 0 read GetMinMaxPeriod;
    property MinPeriod: Cardinal index 1 read GetMinMaxPeriod;
    property Notification: TMmNotificationKind read FNotification;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
    property Period: Cardinal read FPeriod write SetPeriod;
  end;

  EJclMmTimerError = class(EJclError);

  // Audio Mixer
  { TODO -cDoc : mixer API wrapper code. Author: Petr Vones }

  EJclMixerError = class(EJclError);

  TJclMixerDevice = class;
  TJclMixerLine = class;
  TJclMixerDestination = class;

  TJclMixerLineControl = class(TObject)
  private
    FControlInfo: TMixerControl;
    FIsList: Boolean;
    FIsMultiple: Boolean;
    FIsUniform: Boolean;
    FListText: TStringList;
    FMixerLine: TJclMixerLine;
    function GetIsDisabled: Boolean;
    function GetID: DWORD;
    function GetListText: TStrings;
    function GetName: string;
    function GetUniformValue: Cardinal;
    function GetValue: TDynCardinalArray;
    function GetValueString: string;
    procedure SetUniformValue(const Value: Cardinal);
    procedure SetValue(const Value: TDynCardinalArray);
  protected
    constructor Create(AMixerLine: TJclMixerLine; const AControlInfo: TMixerControl);
    procedure PrepareControlDetailsStruc(var ControlDetails: TMixerControlDetails; AUniform, AMultiple: Boolean);
  public
    destructor Destroy; override;
    function FormatValue(AValue: Cardinal): string;
    property ControlInfo: TMixerControl read FControlInfo;
    property ID: DWORD read GetID;
    property IsDisabled: Boolean read GetIsDisabled;
    property IsList: Boolean read FIsList;
    property IsMultiple: Boolean read FIsMultiple;
    property IsUniform: Boolean read FIsUniform;
    property ListText: TStrings read GetListText;
    property MixerLine: TJclMixerLine read FMixerLine;
    property Name: string read GetName;
    property UniformValue: Cardinal read GetUniformValue write SetUniformValue;
    property Value: TDynCardinalArray read GetValue write SetValue;
    property ValueString: string read GetValueString;
  end;

  TJclMixerLine = class(TObject)
  private
    FLineControls: TObjectList;
    FLineInfo: TMixerLine;
    FMixerDevice: TJclMixerDevice;
    function GetComponentString: string;
    function GetLineControlByType(ControlType: DWORD): TJclMixerLineControl;
    function GetLineControlCount: Integer;
    function GetLineControls(Index: Integer): TJclMixerLineControl;
    function GetHasControlType(ControlType: DWORD): Boolean;
    function GetID: DWORD;
    function GetName: string;
  protected
    procedure BuildLineControls;
    constructor Create(AMixerDevice: TJclMixerDevice);
  public
    destructor Destroy; override;
    class function ComponentTypeToString(const ComponentType: DWORD): string;
    property ComponentString: string read GetComponentString;
    property HasControlType[ControlType: DWORD]: Boolean read GetHasControlType;
    property ID: DWORD read GetID;
    property LineControlByType[ControlType: DWORD]: TJclMixerLineControl read GetLineControlByType;
    property LineControls[Index: Integer]: TJclMixerLineControl read GetLineControls; default;
    property LineControlCount: Integer read GetLineControlCount;
    property LineInfo: TMixerLine read FLineInfo;
    property Name: string read GetName;
    property MixerDevice: TJclMixerDevice read FMixerDevice;
  end;

  TJclMixerSource = class(TJclMixerLine)
  private
    FMixerDestination: TJclMixerDestination;
  protected
    constructor Create(AMixerDestination: TJclMixerDestination; ASourceIndex: Cardinal);
  public
    property MixerDestination: TJclMixerDestination read FMixerDestination;
  end;

  TJclMixerDestination = class(TJclMixerLine)
  private
    FSources: TObjectList;
    function GetSourceCount: Integer;
    function GetSources(Index: Integer): TJclMixerSource;
  protected
    constructor Create(AMixerDevice: TJclMixerDevice; ADestinationIndex: Cardinal);
    procedure BuildSources;
  public
    destructor Destroy; override;
    property Sources[Index: Integer]: TJclMixerSource read GetSources; default;
    property SourceCount: Integer read GetSourceCount;
  end;

  TJclMixerDevice = class(TObject)
  private
    FCapabilities: TMixerCaps;
    FDestinations: TObjectList;
    FDeviceIndex: Cardinal;
    FHandle: HMIXER;
    FLines: TList;
    function GetProductName: string;
    function GetDestinationCount: Integer;
    function GetDestinations(Index: Integer): TJclMixerDestination;
    function GetLineCount: Integer;
    function GetLines(Index: Integer): TJclMixerLine;
    function GetLineByComponentType(ComponentType: DWORD): TJclMixerLine;
    function GetLineByID(LineID: DWORD): TJclMixerLine;
    function GetLineControlByID(ControlID: DWORD): TJclMixerLineControl;
    function GetLineUniformValue(ComponentType, ControlType: DWORD): Cardinal;
    procedure SetLineUniformValue(ComponentType, ControlType: DWORD; const Value: Cardinal);
  protected
    constructor Create(ADeviceIndex: Cardinal; ACallBackWnd: THandle);
    procedure BuildDestinations;
    procedure BuildLines;
    procedure Close;
    procedure Open(ACallBackWnd: THandle);
  public
    destructor Destroy; override;
    function FindLineControl(ComponentType, ControlType: DWORD): TJclMixerLineControl;
    property Capabilities: TMixerCaps read FCapabilities;
    property DeviceIndex: Cardinal read FDeviceIndex;
    property Destinations[Index: Integer]: TJclMixerDestination read GetDestinations; default;
    property DestinationCount: Integer read GetDestinationCount;
    property Handle: HMIXER read FHandle;
    property LineByID[LineID: DWORD]: TJclMixerLine read GetLineByID;
    property LineByComponentType[ComponentType: DWORD]: TJclMixerLine read GetLineByComponentType;
    property Lines[Index: Integer]: TJclMixerLine read GetLines;
    property LineCount: Integer read GetLineCount;
    property LineControlByID[ControlID: DWORD]: TJclMixerLineControl read GetLineControlByID;
    property LineUniformValue[ComponentType, ControlType: DWORD]: Cardinal read GetLineUniformValue write SetLineUniformValue;
    property ProductName: string read GetProductName;
  end;

  TJclMixer = class(TObject)
  private
    FCallbackWnd: THandle;
    FDeviceList: TObjectList;
    function GetDeviceCount: Integer;
    function GetDevices(Index: Integer): TJclMixerDevice;
    function GetFirstDevice: TJclMixerDevice;
    function GetLineMute(ComponentType: Integer): Boolean;
    function GetLineVolume(ComponentType: Integer): Cardinal;
    function GetLineByID(MixerHandle: HMIXER; LineID: DWORD): TJclMixerLine;
    function GetLineControlByID(MixerHandle: HMIXER; LineID: DWORD): TJclMixerLineControl;
    procedure SetLineMute(ComponentType: Integer; const Value: Boolean);
    procedure SetLineVolume(ComponentType: Integer; const Value: Cardinal);
  protected
    procedure BuildDevices;
  public
    constructor Create(ACallBackWnd: THandle = 0);
    destructor Destroy; override;
    property CallbackWnd: THandle read FCallbackWnd;
    property Devices[Index: Integer]: TJclMixerDevice read GetDevices; default;
    property DeviceCount: Integer read GetDeviceCount;
    property FirstDevice: TJclMixerDevice read GetFirstDevice;
    property LineByID[MixerHandle: HMIXER; LineID: DWORD]: TJclMixerLine read GetLineByID;
    property LineControlByID[MixerHandle: HMIXER; LineID: DWORD]: TJclMixerLineControl read GetLineControlByID;
    property LineMute[ComponentType: Integer]: Boolean read GetLineMute write SetLineMute;
    property LineVolume[ComponentType: Integer]: Cardinal read GetLineVolume write SetLineVolume;
    property SpeakersMute: Boolean index MIXERLINE_COMPONENTTYPE_DST_SPEAKERS read GetLineMute write SetLineMute;
    property SpeakersVolume: Cardinal index MIXERLINE_COMPONENTTYPE_DST_SPEAKERS read GetLineVolume write SetLineVolume;
  end;

  function MixerLeftRightToArray(Left, Right: Cardinal): TDynCardinalArray;

type
  // MCI Error checking
  EJclMciError = class(EJclError)
  private
    FMciErrorNo: DWORD;
    FMciErrorMsg: string;
  public
    constructor Create(MciErrNo: MCIERROR; const Msg: string);
    constructor CreateFmt(MciErrNo: MCIERROR; const Msg: string; const Args: array of const);
    constructor CreateRes(MciErrNo: MCIERROR; Ident: Integer);
    property MciErrorNo: DWORD read FMciErrorNo;
    property MciErrorMsg: string read FMciErrorMsg;
  end;

function MMCheck(const MciError: MCIERROR; const Msg: string = ''): MCIERROR;
function GetMciErrorMessage(const MciErrNo: MCIERROR): string;

// CD Drive MCI Routines
function OpenCdMciDevice(var OpenParams: TMCI_Open_Parms; Drive: Char = #0): MCIERROR;
function CloseCdMciDevice(var OpenParams: TMCI_Open_Parms): MCIERROR;

// CD Drive specific routines
procedure OpenCloseCdDrive(OpenMode: Boolean; Drive: Char = #0);

function IsMediaPresentInDrive(Drive: Char = #0): Boolean;

type
  TJclCdMediaInfo = (miProduct, miIdentity, miUPC);

  TJclCdTrackType = (ttAudio, ttOther);
  TJclCdTrackInfo = record
    Minute: Byte;
    Second: Byte;
    TrackType: TJclCdTrackType;
  end;
  TJclCdTrackInfoArray = array of TJclCdTrackInfo;

function GetCdInfo(InfoType: TJclCdMediaInfo; Drive: Char = #0): string;

function GetCDAudioTrackList(var TrackList: TJclCdTrackInfoArray; Drive: Char = #0): TJclCdTrackInfo; overload;
function GetCDAudioTrackList(TrackList: TStrings; IncludeTrackType: Boolean = False; Drive: Char = #0): string; overload;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/windows/JclMultimedia.pas $';
    Revision: '$Revision: 2175 $';
    Date: '$Date: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007) $';
    LogPath: 'JCL\source\windows'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils,
  JclResources, JclSysUtils;

{ TODO : move to JclWin32? }  
{$IFDEF FPC}
// declarations missing from mmsystem.pp
const
  mmsyst = 'winmm.dll';

type
  TFNTimeCallBack = procedure(uTimerID, uMessage: UINT;
    dwUser, dw1, dw2: DWORD) stdcall;

  PMixerControlDetailsListText = ^TMixerControlDetailsListText;
  TMixerControlDetailsListText = MIXERCONTROLDETAILS_LISTTEXTA;

  TMixerLineControlsA          = MIXERLINECONTROLSA;
  TMixerLineControls           = TMixerLineControlsA;
  TMCI_Status_Parms            = MCI_STATUS_PARMS;
  TMCI_Info_Parms              = MCI_INFO_PARMS;
  TMCI_Set_Parms               = MCI_SET_PARMS;
  
function mixerSetControlDetails(hmxobj: HMIXEROBJ; pmxcd: PMixerControlDetails; fdwDetails: DWORD): MMRESULT; stdcall;
  external mmsyst name 'mixerSetControlDetails';
{$ENDIF FPC}

//=== { TJclMultimediaTimer } ================================================

constructor TJclMultimediaTimer.Create(Kind: TMmTimerKind; Notification: TMmNotificationKind);
begin
  FKind := Kind;
  FNotification := Notification;
  FPeriod := 0;
  FTimerID := 0;
  FEvent := nil;
  FillChar(FTimeCaps, SizeOf(FTimeCaps), #0);
  if timeGetDevCaps(@FTimeCaps, SizeOf(FTimeCaps)) = TIMERR_STRUCT then
    raise EJclMmTimerError.CreateRes(@RsMmTimerGetCaps);
  FPeriod := FTimeCaps.wPeriodMin;
  if Notification <> nkCallback then
    FEvent := TJclEvent.Create(nil, Notification = nkSetEvent, False, '');
end;

destructor TJclMultimediaTimer.Destroy;
begin
  EndTimer;
  FreeAndNil(FEvent);
  FOnTimer := nil;
  inherited Destroy;
end;

procedure MmTimerCallback(TimerId, Msg: Cardinal; User, dw1, dw2: DWORD); stdcall;
begin
  TJclMultimediaTimer(User).Timer(TimerId);
end;

class function TJclMultimediaTimer.BeginPeriod(const Period: Cardinal): Boolean;
begin
  Result := timeBeginPeriod(Period) = TIMERR_NOERROR;
end;

procedure TJclMultimediaTimer.BeginTimer(const Delay, Resolution: Cardinal);
var
  Event: Cardinal;
  TimerCallback: TFNTimeCallBack;
begin
  if FTimerId <> 0 then
    raise EJclMmTimerError.CreateRes(@RsMmTimerActive);
  Event := 0;
  TimerCallback := nil;
  case FKind of
    tkPeriodic:
      Event := TIME_PERIODIC;
    tkOneShot:
      Event := TIME_ONESHOT;
  end;
  case FNotification of
    nkCallback:
      begin
        Event := Event or TIME_CALLBACK_FUNCTION;
        TimerCallback := @MmTimerCallback;
      end;
    nkSetEvent:
      begin
        Event := Event or TIME_CALLBACK_EVENT_SET;
        TimerCallback := TFNTimeCallback(FEvent.Handle);
      end;
    nkPulseEvent:
      begin
        Event := Event or TIME_CALLBACK_EVENT_PULSE;
        TimerCallback := TFNTimeCallback(FEvent.Handle);
      end;
  end;
  FStartTime := GetTime;
  if timeBeginPeriod(FPeriod) = TIMERR_NOERROR then
    FTimerId := timeSetEvent(Delay, Resolution, TimerCallBack, DWORD(Self), Event);
  if FTimerId = 0 then
    raise EJclMmTimerError.CreateRes(@RsMmSetEvent);
end;

function TJclMultimediaTimer.Elapsed(const Update: Boolean): Cardinal;
var
  CurrentTime: Cardinal;
begin
  if FTimerId = 0 then
    Result := 0
  else
  begin
    CurrentTime := GetTime;
    if CurrentTime >= FStartTime then
      Result := CurrentTime - FStartTime
    else
      Result := (High(Cardinal) - FStartTime) + CurrentTime;
    if Update then
      FStartTime := CurrentTime;
  end;
end;

class function TJclMultimediaTimer.EndPeriod(const Period: Cardinal): Boolean;
begin
  Result := timeEndPeriod(Period) = TIMERR_NOERROR;
end;

procedure TJclMultimediaTimer.EndTimer;
begin
  if FTimerId <> 0 then
  begin
    if FKind = tkPeriodic then
      timeKillEvent(FTimerId);
    timeEndPeriod(FPeriod);
    FTimerId := 0;
  end;
end;

function TJclMultimediaTimer.GetMinMaxPeriod(Index: Integer): Cardinal;
begin
  case Index of
    0:
      Result := FTimeCaps.wPeriodMax;
    1:
      Result := FTimeCaps.wPeriodMin;
  else
    Result := 0;
  end;
end;

class function TJclMultimediaTimer.GetTime: Cardinal;
begin
  Result := timeGetTime;
end;

procedure TJclMultimediaTimer.SetPeriod(Value: Cardinal);
begin
  if FTimerId <> 0 then
    raise EJclMmTimerError.CreateRes(@RsMmTimerActive);
  FPeriod := Value;
end;

{ TODO -cHelp : Applications should not call any system-defined functions from
    inside a callback function, except for PostMessage, timeGetSystemTime,
    timeGetTime, timeSetEvent, timeKillEvent, midiOutShortMsg, midiOutLongMsg,
    and OutputDebugString. }
procedure TJclMultimediaTimer.Timer(Id: Cardinal);
begin
  { TODO : A exception in the callbacl i very likely very critically }
  if Id <> FTimerId then
    raise EJclMmTimerError.CreateRes(@RsMmInconsistentId);
  if Assigned(FOnTimer) then
    FOnTimer(Self);
end;

function TJclMultimediaTimer.WaitFor(const TimeOut: Cardinal): TJclWaitResult;
begin
  if FNotification = nkCallback then
    Result := wrError
  else
    Result := FEvent.WaitFor(TimeOut);
end;

//=== { TJclMixerLineControl } ===============================================

function MixerLeftRightToArray(Left, Right: Cardinal): TDynCardinalArray;
begin
  SetLength(Result, 2);
  Result[0] := Left;
  Result[1] := Right;
end;

constructor TJclMixerLineControl.Create(AMixerLine: TJclMixerLine; const AControlInfo: TMixerControl);
begin
  FControlInfo := AControlInfo;
  FMixerLine := AMixerLine;
  FIsList := (ControlInfo.dwControlType and MIXERCONTROL_CT_CLASS_MASK) = MIXERCONTROL_CT_CLASS_LIST;
  FIsMultiple := FControlInfo.fdwControl and MIXERCONTROL_CONTROLF_MULTIPLE <> 0;
  FIsUniform := FControlInfo.fdwControl and MIXERCONTROL_CONTROLF_UNIFORM <> 0;
end;

destructor TJclMixerLineControl.Destroy;
begin
  FreeAndNil(FListText);
  inherited Destroy;
end;

function TJclMixerLineControl.FormatValue(AValue: Cardinal): string;
begin
  case FControlInfo.dwControlType and MIXERCONTROL_CT_UNITS_MASK of
    MIXERCONTROL_CT_UNITS_BOOLEAN:
      Result := BooleanToStr(Boolean(AValue));
    MIXERCONTROL_CT_UNITS_SIGNED:
      Result := Format('%d', [AValue]);
    MIXERCONTROL_CT_UNITS_UNSIGNED:
      Result := Format('%u', [AValue]);
    MIXERCONTROL_CT_UNITS_DECIBELS:
      Result := Format('%.1fdB', [AValue / 10]);
    MIXERCONTROL_CT_UNITS_PERCENT:
      Result := Format('%.1f%%', [AValue / 10]);
  else
    Result := '';
  end;
end;

function TJclMixerLineControl.GetID: DWORD;
begin
  Result := ControlInfo.dwControlID;
end;

function TJclMixerLineControl.GetIsDisabled: Boolean;
begin
  Result := FControlInfo.fdwControl and MIXERCONTROL_CONTROLF_DISABLED <> 0;
end;

function TJclMixerLineControl.GetListText: TStrings;
var
  ControlDetails: TMixerControlDetails;
  ListTexts, P: PMixerControlDetailsListText;
  I: Cardinal;
begin
  if FListText = nil then
  begin
    FListText := TStringList.Create;
    if IsMultiple and IsList then
    begin
      PrepareControlDetailsStruc(ControlDetails, True, IsMultiple);
      ControlDetails.cbDetails := SizeOf(TMixerControlDetailsListText);
      GetMem(ListTexts, SizeOf(TMixerControlDetailsListText) * ControlDetails.cMultipleItems);
      try
        ControlDetails.paDetails := ListTexts;
        if mixerGetControlDetails(MixerLine.MixerDevice.Handle, @ControlDetails, MIXER_GETCONTROLDETAILSF_LISTTEXT) = MMSYSERR_NOERROR then
        begin
          P := ListTexts;
          for I := 1 to ControlDetails.cMultipleItems do
          begin
            FListText.AddObject(P^.szName, Pointer(P^.dwParam1));
            Inc(P);
          end;
        end;  
      finally
        FreeMem(ListTexts);
      end;
    end;
  end;
  Result := FListText;
end;

function TJclMixerLineControl.GetName: string;
begin
  Result := FControlInfo.szName;
end;

function TJclMixerLineControl.GetUniformValue: Cardinal;
var
  ControlDetails: TMixerControlDetails;
begin
  PrepareControlDetailsStruc(ControlDetails, True, False);
  ControlDetails.cbDetails := SizeOf(Cardinal);
  ControlDetails.paDetails := @Result;
  MMCheck(mixerGetControlDetails(MixerLine.MixerDevice.Handle, @ControlDetails, MIXER_GETCONTROLDETAILSF_VALUE));
end;

function TJclMixerLineControl.GetValue: TDynCardinalArray;
var
  ControlDetails: TMixerControlDetails;
  ItemCount: Cardinal;
begin
  PrepareControlDetailsStruc(ControlDetails, IsUniform, IsMultiple);
  if IsUniform then
    ItemCount := 1
  else
    ItemCount := ControlDetails.cChannels;
  if IsMultiple then
    ItemCount := ItemCount * ControlDetails.cMultipleItems;
  SetLength(Result, ItemCount);
  ControlDetails.cbDetails := SizeOf(Cardinal);
  ControlDetails.paDetails := @Result[0];
  MMCheck(mixerGetControlDetails(MixerLine.MixerDevice.Handle, @ControlDetails, MIXER_GETCONTROLDETAILSF_VALUE));
end;

function TJclMixerLineControl.GetValueString: string;
var
  TempValue: TDynCardinalArray;
  I: Integer;
begin
  TempValue := Value;
  Result := '';
  for I := Low(TempValue) to High(TempValue) do
    Result := Result + ',' + FormatValue(TempValue[I]);
  Delete(Result, 1, 1);
end;

procedure TJclMixerLineControl.PrepareControlDetailsStruc(var ControlDetails: TMixerControlDetails;
  AUniform, AMultiple: Boolean);
begin
  FillChar(ControlDetails, SizeOf(ControlDetails), 0);
  ControlDetails.cbStruct := SizeOf(ControlDetails);
  ControlDetails.dwControlID := FControlInfo.dwControlID;
  if AUniform then
    ControlDetails.cChannels := MIXERCONTROL_CONTROLF_UNIFORM
  else
    ControlDetails.cChannels := MixerLine.LineInfo.cChannels;
  if AMultiple then
    ControlDetails.cMultipleItems := FControlInfo.cMultipleItems;
end;

procedure TJclMixerLineControl.SetUniformValue(const Value: Cardinal);
var
  ControlDetails: TMixerControlDetails;
begin
  PrepareControlDetailsStruc(ControlDetails, True, False);
  ControlDetails.cbDetails := SizeOf(Cardinal);
  ControlDetails.paDetails := @Value;
  MMCheck(mixerSetControlDetails(MixerLine.MixerDevice.Handle, @ControlDetails, MIXER_GETCONTROLDETAILSF_VALUE));
end;

procedure TJclMixerLineControl.SetValue(const Value: TDynCardinalArray);
var
  ControlDetails: TMixerControlDetails;
  {$IFDEF ASSERTIONS_ON}
  ItemCount: Cardinal;
  {$ENDIF ASSERTIONS_ON}
begin
  PrepareControlDetailsStruc(ControlDetails, IsUniform, IsMultiple);
  {$IFDEF ASSERTIONS_ON}
  if IsUniform then
    ItemCount := 1
  else
    ItemCount := ControlDetails.cChannels;
  if IsMultiple then
    ItemCount := ItemCount * ControlDetails.cMultipleItems;
  Assert(ItemCount = Cardinal(Length(Value)));
  {$ENDIF ASSERTIONS_ON}
  ControlDetails.cbDetails := SizeOf(Cardinal);
  ControlDetails.paDetails := @Value[0];
  MMCheck(mixerSetControlDetails(MixerLine.MixerDevice.Handle, @ControlDetails, MIXER_GETCONTROLDETAILSF_VALUE));
end;

//=== { TJclMixerLine } ======================================================

function MixerLineCompareID(Item1, Item2: Pointer): Integer;
begin
  Result := Integer(TJclMixerLine(Item1).ID) - Integer(TJclMixerLine(Item2).ID);
end;

function MixerLineSearchID(Param: Pointer; ItemIndex: Integer; const Value): Integer;
begin
  Result := Integer(TJclMixerDevice(Param).Lines[ItemIndex].ID) - Integer(Value);
end;

constructor TJclMixerLine.Create(AMixerDevice: TJclMixerDevice);
begin
  FMixerDevice := AMixerDevice;
  FLineControls := TObjectList.Create;
end;

destructor TJclMixerLine.Destroy;
begin
  FreeAndNil(FLineControls);
  inherited Destroy;
end;

procedure TJclMixerLine.BuildLineControls;
var
  MixerControls: TMixerLineControls;
  Controls, P: PMixerControl;
  I: Cardinal;
  Item: TJclMixerLineControl;
begin
  GetMem(Controls, SizeOf(TMixerControl) * FLineInfo.cControls);
  try
    MixerControls.cbStruct := SizeOf(MixerControls);
    MixerControls.dwLineID := FLineInfo.dwLineID;
    MixerControls.cControls := FLineInfo.cControls;
    MixerControls.cbmxctrl := SizeOf(TMixerControl);
    MixerControls.pamxctrl := Controls;
    if mixerGetLineControls(FMixerDevice.Handle, @MixerControls, MIXER_GETLINECONTROLSF_ALL) = MMSYSERR_NOERROR then
    begin
      P := Controls;
      for I := 1 to FLineInfo.cControls do
      begin
        Item := TJclMixerLineControl.Create(Self, P^);
        FLineControls.Add(Item);
        Inc(P);
      end;
    end;  
  finally
    FreeMem(Controls);
  end;
end;

class function TJclMixerLine.ComponentTypeToString(const ComponentType: DWORD): string;
begin
  case ComponentType of
    MIXERLINE_COMPONENTTYPE_DST_UNDEFINED:
      Result := RsMmMixerUndefined;
    MIXERLINE_COMPONENTTYPE_DST_DIGITAL, MIXERLINE_COMPONENTTYPE_SRC_DIGITAL:
      Result := RsMmMixerDigital;
    MIXERLINE_COMPONENTTYPE_DST_LINE, MIXERLINE_COMPONENTTYPE_SRC_LINE:
      Result := RsMmMixerLine;
    MIXERLINE_COMPONENTTYPE_DST_MONITOR:
      Result := RsMmMixerMonitor;
    MIXERLINE_COMPONENTTYPE_DST_SPEAKERS:
      Result := RsMmMixerSpeakers;
    MIXERLINE_COMPONENTTYPE_DST_HEADPHONES:
      Result := RsMmMixerHeadphones;
    MIXERLINE_COMPONENTTYPE_DST_TELEPHONE, MIXERLINE_COMPONENTTYPE_SRC_TELEPHONE:
      Result := RsMmMixerTelephone;
    MIXERLINE_COMPONENTTYPE_DST_WAVEIN:
      Result := RsMmMixerWaveIn;
    MIXERLINE_COMPONENTTYPE_DST_VOICEIN:
      Result := RsMmMixerVoiceIn;
    MIXERLINE_COMPONENTTYPE_SRC_MICROPHONE:
      Result := RsMmMixerMicrophone;
    MIXERLINE_COMPONENTTYPE_SRC_SYNTHESIZER:
      Result := RsMmMixerSynthesizer;
    MIXERLINE_COMPONENTTYPE_SRC_COMPACTDISC:
      Result := RsMmMixerCompactDisc;
    MIXERLINE_COMPONENTTYPE_SRC_PCSPEAKER:
      Result := RsMmMixerPcSpeaker;
    MIXERLINE_COMPONENTTYPE_SRC_WAVEOUT:
      Result := RsMmMixerWaveOut;
    MIXERLINE_COMPONENTTYPE_SRC_AUXILIARY:
      Result := RsMmMixerAuxiliary;
    MIXERLINE_COMPONENTTYPE_SRC_ANALOG:
      Result := RsMmMixerAnalog;
  else
    Result := '';
  end;
end;

function TJclMixerLine.GetComponentString: string;
begin
  Result := ComponentTypeToString(FLineInfo.dwComponentType);
end;

function TJclMixerLine.GetHasControlType(ControlType: DWORD): Boolean;
begin
  Result := LineControlByType[ControlType] <> nil;
end;

function TJclMixerLine.GetID: DWORD;
begin
  Result := LineInfo.dwLineID;
end;

function TJclMixerLine.GetLineControlByType(ControlType: DWORD): TJclMixerLineControl;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to LineControlCount - 1 do
    if LineControls[I].ControlInfo.dwControlType = ControlType then
    begin
      Result := LineControls[I];
      Break;
    end;
end;

function TJclMixerLine.GetLineControlCount: Integer;
begin
  Result := FLineControls.Count;
  if Result = 0 then
  begin
    BuildLineControls;
    Result := FLineControls.Count;
  end;
end;

function TJclMixerLine.GetLineControls(Index: Integer): TJclMixerLineControl;
begin
  Result := TJclMixerLineControl(FLineControls[Index]);
end;

function TJclMixerLine.GetName: string;
begin
  Result := FLineInfo.szName;
end;

//=== { TJclMixerSource } ====================================================

constructor TJclMixerSource.Create(AMixerDestination: TJclMixerDestination; ASourceIndex: Cardinal);
begin
  inherited Create(AMixerDestination.MixerDevice);
  FMixerDestination := AMixerDestination;
  FLineInfo.cbStruct := SizeOf(FLineInfo);
  FLineInfo.dwDestination := FMixerDestination.LineInfo.dwDestination;
  FLineInfo.dwSource := ASourceIndex;
  MMCheck(mixerGetLineInfo(FMixerDestination.MixerDevice.Handle, @FLineInfo, MIXER_GETLINEINFOF_SOURCE));
end;

//=== { TJclMixerDestination } ===============================================

constructor TJclMixerDestination.Create(AMixerDevice: TJclMixerDevice; ADestinationIndex: Cardinal);
begin
  inherited Create(AMixerDevice);
  FLineInfo.cbStruct := SizeOf(FLineInfo);
  FLineInfo.dwDestination := ADestinationIndex;
  MMCheck(mixerGetLineInfo(AMixerDevice.Handle, @FLineInfo, MIXER_GETLINEINFOF_DESTINATION));
  FSources := TObjectList.Create;
end;

destructor TJclMixerDestination.Destroy;
begin
  FreeAndNil(FSources);
  inherited Destroy;
end;

procedure TJclMixerDestination.BuildSources;
var
  I: Cardinal;
  Item: TJclMixerSource;
begin
  for I := 1 to LineInfo.cConnections do
  begin
    Item := TJclMixerSource.Create(Self, I - 1);
    FSources.Add(Item);
  end;
end;

function TJclMixerDestination.GetSourceCount: Integer;
begin
  Result := FSources.Count;
  if Result = 0 then
  begin
    BuildSources;
    Result := FSources.Count;
  end;
end;

function TJclMixerDestination.GetSources(Index: Integer): TJclMixerSource;
begin
  Result := TJclMixerSource(FSources[Index]);
end;

//=== { TJclMixerDevice } ====================================================

constructor TJclMixerDevice.Create(ADeviceIndex: Cardinal; ACallBackWnd: THandle);
begin
  FDeviceIndex := ADeviceIndex;
  FHandle := -1;
  FDestinations := TObjectList.Create;
  FLines := TList.Create;
  MMCheck(mixerGetDevCaps(ADeviceIndex, @FCapabilities, SizeOf(FCapabilities)));
  Open(ACallBackWnd);
  BuildDestinations;
end;

destructor TJclMixerDevice.Destroy;
begin
  Close;
  FreeAndNil(FDestinations);
  FreeAndNil(FLines);
  inherited Destroy;
end;

procedure TJclMixerDevice.BuildDestinations;
var
  I: Cardinal;
  Item: TJclMixerDestination;
begin
  for I := 1 to FCapabilities.cDestinations do
  begin
    Item := TJclMixerDestination.Create(Self, I - 1);
    FDestinations.Add(Item);
  end;
end;

procedure TJclMixerDevice.BuildLines;
var
  D, I: Integer;
  Dest: TJclMixerDestination;
begin
  for D := 0 to DestinationCount - 1 do
  begin
    Dest := Destinations[D];
    FLines.Add(Dest);
    for I := 0 to Dest.SourceCount - 1 do
      FLines.Add(Dest.Sources[I]);
  end;
  FLines.Sort(MixerLineCompareID);
end;

procedure TJclMixerDevice.Close;
begin
  if FHandle <> -1 then
  begin
    mixerClose(FHandle);
    FHandle := -1;
  end;
end;

function TJclMixerDevice.FindLineControl(ComponentType, ControlType: DWORD): TJclMixerLineControl;
var
  TempLine: TJclMixerLine;
begin
  Result := nil;
  TempLine := LineByComponentType[ComponentType];
  if TempLine <> nil then
    Result := TempLine.LineControlByType[ControlType];
end;

function TJclMixerDevice.GetDestinationCount: Integer;
begin
  Result := FDestinations.Count;
end;

function TJclMixerDevice.GetDestinations(Index: Integer): TJclMixerDestination;
begin
  Result := TJclMixerDestination(FDestinations[Index]);
end;

function TJclMixerDevice.GetLineByComponentType(ComponentType: DWORD): TJclMixerLine;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to LineCount - 1 do
    if Lines[I].LineInfo.dwComponentType = ComponentType then
    begin
      Result := Lines[I];
      Break;
    end;
end;

function TJclMixerDevice.GetLineByID(LineID: DWORD): TJclMixerLine;
var
  I: Integer;
begin
  I := SearchSortedUntyped(Self, LineCount, MixerLineSearchID, Pointer(LineID));
  if I = -1 then
    Result := nil
  else
    Result := Lines[I];
end;

function TJclMixerDevice.GetLineControlByID(ControlID: DWORD): TJclMixerLineControl;
var
  L, C: Integer;
  TempLine: TJclMixerLine;
begin
  Result := nil;
  for L := 0 to LineCount - 1 do
  begin
    TempLine := Lines[L];
    for C := 0 to TempLine.LineControlCount - 1 do
      if TempLine.LineControls[C].ID = ControlID then
      begin
        Result := TempLine.LineControls[C];
        Break;
      end;
  end;
end;

function TJclMixerDevice.GetLineCount: Integer;
begin
  Result := FLines.Count;
  if Result = 0 then
  begin
    BuildLines;
    Result := FLines.Count;
  end;  
end;

function TJclMixerDevice.GetLines(Index: Integer): TJclMixerLine;
begin
  Result := TJclMixerLine(FLines[Index]);
end;

function TJclMixerDevice.GetLineUniformValue(ComponentType, ControlType: DWORD): Cardinal;
var
  LineControl: TJclMixerLineControl;
begin
  LineControl := FindLineControl(ComponentType, ControlType);
  if LineControl <> nil then
    Result := LineControl.UniformValue
  else
    Result := 0;  
end;

function TJclMixerDevice.GetProductName: string;
begin
  Result := FCapabilities.szPname;
end;

procedure TJclMixerDevice.Open(ACallBackWnd: THandle);
var
  Flags: DWORD;
begin
  if FHandle = -1 then
  begin
    Flags := MIXER_OBJECTF_HMIXER;
    if ACallBackWnd <> 0 then
      Inc(Flags, CALLBACK_WINDOW);
    MMCheck(mixerOpen(@FHandle, DeviceIndex, ACallBackWnd, 0, Flags));
  end;
end;

procedure TJclMixerDevice.SetLineUniformValue(ComponentType, ControlType: DWORD; const Value: Cardinal);
var
  LineControl: TJclMixerLineControl;
begin
  LineControl := FindLineControl(ComponentType, ControlType);
  if LineControl <> nil then
    LineControl.UniformValue := Value
  else
    raise EJclMixerError.CreateResFmt(@RsMmMixerCtlNotFound,
      [TJclMixerLine.ComponentTypeToString(ComponentType), ControlType]);
end;

//=== { TJclMixer } ==========================================================

constructor TJclMixer.Create(ACallBackWnd: THandle);
begin
  FDeviceList := TObjectList.Create;
  FCallbackWnd := ACallBackWnd;
  BuildDevices;
end;

destructor TJclMixer.Destroy;
begin
  FreeAndNil(FDeviceList);
  inherited Destroy;
end;

procedure TJclMixer.BuildDevices;
var
  I: Cardinal;
  Item: TJclMixerDevice;
begin
  for I := 1 to mixerGetNumDevs do
  begin
    Item := TJclMixerDevice.Create(I - 1, FCallbackWnd);
    FDeviceList.Add(Item);
  end;
end;

function TJclMixer.GetDeviceCount: Integer;
begin
  Result := FDeviceList.Count;
end;

function TJclMixer.GetDevices(Index: Integer): TJclMixerDevice;
begin
  Result := TJclMixerDevice(FDeviceList.Items[Index]);
end;

function TJclMixer.GetFirstDevice: TJclMixerDevice;
begin
  if DeviceCount = 0 then
    raise EJclMixerError.CreateRes(@RsMmMixerNoDevices);
  Result := Devices[0];
end;

function TJclMixer.GetLineByID(MixerHandle: HMIXER; LineID: DWORD): TJclMixerLine;
var
  I: Integer;
  TempDevice: TJclMixerDevice;
begin
  Result := nil;
  for I := 0 to DeviceCount - 1 do
  begin
    TempDevice := Devices[I];
    if TempDevice.Handle = MixerHandle then
    begin
      Result := TempDevice.LineByID[LineID];
      if Result <> nil then
        Break;
    end;
  end;
end;

function TJclMixer.GetLineControlByID(MixerHandle: HMIXER; LineID: DWORD): TJclMixerLineControl;
var
  I: Integer;
  TempDevice: TJclMixerDevice;
begin
  Result := nil;
  for I := 0 to DeviceCount - 1 do
  begin
    TempDevice := Devices[I];
    if TempDevice.Handle = MixerHandle then
    begin
      Result := TempDevice.LineControlByID[LineID];
      if Result <> nil then
        Break;
    end;
  end;
end;

function TJclMixer.GetLineMute(ComponentType: Integer): Boolean;
begin
  Result := Boolean(FirstDevice.LineUniformValue[Cardinal(ComponentType), MIXERCONTROL_CONTROLTYPE_MUTE]);
end;

function TJclMixer.GetLineVolume(ComponentType: Integer): Cardinal;
begin
  Result := FirstDevice.LineUniformValue[Cardinal(ComponentType), MIXERCONTROL_CONTROLTYPE_VOLUME];
end;

procedure TJclMixer.SetLineMute(ComponentType: Integer; const Value: Boolean);
begin
  FirstDevice.LineUniformValue[Cardinal(ComponentType), MIXERCONTROL_CONTROLTYPE_MUTE] := Cardinal(Value);
end;

procedure TJclMixer.SetLineVolume(ComponentType: Integer; const Value: Cardinal);
begin
  FirstDevice.LineUniformValue[Cardinal(ComponentType), MIXERCONTROL_CONTROLTYPE_VOLUME] := Value;
end;

//=== { EJclMciError } =======================================================

constructor EJclMciError.Create(MciErrNo: MCIERROR; const Msg: string);
begin
  FMciErrorNo := MciErrNo;
  FMciErrorMsg := GetMciErrorMessage(MciErrNo);
  inherited Create(Msg + AnsiLineBreak + RsMmMciErrorPrefix + FMciErrorMsg);
end;

constructor EJclMciError.CreateFmt(MciErrNo: MCIERROR; const Msg: string;
  const Args: array of const);
begin
  FMciErrorNo := MciErrNo;
  FMciErrorMsg := GetMciErrorMessage(MciErrNo);
  inherited CreateFmt(Msg + AnsiLineBreak + RsMmMciErrorPrefix + FMciErrorMsg, Args);
end;

constructor EJclMciError.CreateRes(MciErrNo: MCIERROR; Ident: Integer);
begin
  FMciErrorNo := MciErrNo;
  FMciErrorMsg := GetMciErrorMessage(MciErrNo);
  inherited Create(LoadStr(Ident)+ AnsiLineBreak + RsMmMciErrorPrefix + FMciErrorMsg);
end;

function GetMciErrorMessage(const MciErrNo: MCIERROR): string;
var
  Buffer: array [0..MMSystem.MAXERRORLENGTH - 1] of Char;
begin
  if mciGetErrorString(MciErrNo, Buffer, SizeOf(Buffer)) then
    Result := Buffer
  else
    Result := Format(RsMmUnknownError, [MciErrNo]);
end;

function MMCheck(const MciError: MCIERROR; const Msg: string): MCIERROR;
begin
  if MciError <> MMSYSERR_NOERROR then
    raise EJclMciError.Create(MciError, Msg);
  Result := MciError;
end;

//=== CD Drive MCI Routines ==================================================

function OpenCdMciDevice(var OpenParams: TMCI_Open_Parms; Drive: Char): MCIERROR;
var
  OpenParam: DWORD;
  DriveName: array [0..2] of Char;
begin
  FillChar(OpenParams, SizeOf(OpenParams), 0);
  OpenParam := MCI_OPEN_TYPE or MCI_OPEN_TYPE_ID or MCI_OPEN_SHAREABLE;
  OpenParams.lpstrDeviceType := PChar(MCI_DEVTYPE_CD_AUDIO);
  if Drive <> #0 then
  begin
    OpenParams.lpstrElementName := StrFmt(DriveName, '%s:', [UpCase(Drive)]);
    Inc(OpenParam, MCI_OPEN_ELEMENT);
  end;
  Result := mciSendCommand(0, MCI_OPEN, OpenParam, Cardinal(@OpenParams));
end;

function CloseCdMciDevice(var OpenParams: TMCI_Open_Parms): MCIERROR;
begin
  Result := mciSendCommand(OpenParams.wDeviceID, MCI_CLOSE, MCI_WAIT, 0);
  if Result = MMSYSERR_NOERROR then
    FillChar(OpenParams, SizeOf(OpenParams), 0);
end;

//=== CD Drive specific routines =============================================

procedure OpenCloseCdDrive(OpenMode: Boolean; Drive: Char);
const
  OpenCmd: array [Boolean] of DWORD =
    (MCI_SET_DOOR_CLOSED, MCI_SET_DOOR_OPEN);
var
  Mci: TMCI_Open_Parms;
begin
  MMCheck(OpenCdMciDevice(Mci, Drive), LoadResString(@RsMmNoCdAudio));
  try
    MMCheck(mciSendCommand(Mci.wDeviceID, MCI_SET, OpenCmd[OpenMode], 0));
  finally
    CloseCdMciDevice(Mci);
  end;
end;

function IsMediaPresentInDrive(Drive: Char): Boolean;
var
  Mci: TMCI_Open_Parms;
  StatusParams: TMCI_Status_Parms;
begin
  MMCheck(OpenCdMciDevice(Mci, Drive), LoadResString(@RsMmNoCdAudio));
  try
    FillChar(StatusParams, SizeOf(StatusParams), 0);
    StatusParams.dwItem := MCI_STATUS_MEDIA_PRESENT;
    MMCheck(mciSendCommand(Mci.wDeviceID, MCI_STATUS, MCI_STATUS_ITEM or MCI_WAIT, Cardinal(@StatusParams)));
    Result := Boolean(StatusParams.dwReturn);
  finally
    CloseCdMciDevice(Mci);
  end;
end;

function GetCdInfo(InfoType: TJclCdMediaInfo; Drive: Char): string;
const
  InfoConsts: array [TJclCdMediaInfo] of DWORD =
    (MCI_INFO_PRODUCT, MCI_INFO_MEDIA_IDENTITY, MCI_INFO_MEDIA_UPC);
var
  Mci: TMCI_Open_Parms;
  InfoParams: TMCI_Info_Parms;
  Buffer: array [0..255] of Char;
begin
  Result := '';
  MMCheck(OpenCdMciDevice(Mci, Drive), LoadResString(@RsMmNoCdAudio));
  try
    InfoParams.dwCallback := 0;
    InfoParams.lpstrReturn := Buffer;
    InfoParams.dwRetSize := SizeOf(Buffer) - 1;
    if mciSendCommand(Mci.wDeviceID, MCI_INFO, InfoConsts[InfoType], Cardinal(@InfoParams)) = MMSYSERR_NOERROR then
      Result := Buffer;
  finally
    CloseCdMciDevice(Mci);
  end;
end;

function GetCDAudioTrackList(var TrackList: TJclCdTrackInfoArray; Drive: Char): TJclCdTrackInfo;
var
  Mci: TMCI_Open_Parms;
  SetParams: TMCI_Set_Parms;
  TrackCnt, Ret: Cardinal;
  I: Integer;

  function GetTrackInfo(Command, Item, Track: DWORD): DWORD;
  var
    StatusParams: TMCI_Status_Parms;
  begin
    FillChar(StatusParams, SizeOf(StatusParams), 0);
    StatusParams.dwItem := Item;
    StatusParams.dwTrack := Track;
    if mciSendCommand(Mci.wDeviceID, MCI_STATUS, Command, Cardinal(@StatusParams)) = MMSYSERR_NOERROR then
      Result := StatusParams.dwReturn
    else
      Result := 0;
  end;

begin
  MMCheck(OpenCdMciDevice(Mci, Drive), LoadResString(@RsMmNoCdAudio));
  try
    FillChar(SetParams, SizeOf(SetParams), 0);
    SetParams.dwTimeFormat := MCI_FORMAT_MSF;
    MMCheck(mciSendCommand(Mci.wDeviceID, MCI_SET, MCI_SET_TIME_FORMAT, Cardinal(@SetParams)));
    Result.TrackType := ttOther;
    TrackCnt := GetTrackInfo(MCI_STATUS_ITEM, MCI_STATUS_NUMBER_OF_TRACKS, 0);
    SetLength(TrackList, TrackCnt);
    for I := 0 to TrackCnt - 1 do
    begin
      Ret := GetTrackInfo(MCI_STATUS_ITEM or MCI_TRACK, MCI_STATUS_LENGTH, I + 1);
      TrackList[I].Minute := mci_MSF_Minute(Ret);
      TrackList[I].Second := mci_MSF_Second(Ret);
      Ret := GetTrackInfo(MCI_STATUS_ITEM or MCI_TRACK, MCI_CDA_STATUS_TYPE_TRACK, I + 1);
      if Ret = MCI_CDA_TRACK_AUDIO then
      begin
        Result.TrackType := ttAudio;
        TrackList[I].TrackType := ttAudio;
      end  
      else
        TrackList[I].TrackType := ttOther;
    end;
    Ret := GetTrackInfo(MCI_STATUS_ITEM, MCI_STATUS_LENGTH, 0);
    Result.Minute := mci_MSF_Minute(Ret);
    Result.Second := mci_MSF_Second(Ret);
  finally
    CloseCdMciDevice(Mci);
  end;
end;

function GetCDAudioTrackList(TrackList: TStrings; IncludeTrackType: Boolean; Drive: Char): string;
var
  Tracks: TJclCdTrackInfoArray;
  TotalTime: TJclCdTrackInfo;
  I: Integer;
  S: string;
begin
  TotalTime := GetCDAudioTrackList(Tracks, Drive);
  TrackList.BeginUpdate;
  try
    for I := Low(Tracks) to High(Tracks) do
      with Tracks[I] do
      begin
        if IncludeTrackType then
        begin
          case TrackType of
            ttAudio:
              S := RsMMTrackAudio;
            ttOther:
              S := RsMMTrackOther;
          end;
          S := Format('[%s]', [S]);
        end
        else
          S := '';
        S := Format(RsMmCdTrackNo, [I + 1]) + ' ' + S;
        S := S + ' ' + Format(RsMMCdTimeFormat, [I + 1, Minute, Second]);
        TrackList.Add(S);
      end;
  finally
    TrackList.EndUpdate;
  end;
  Result := Format(RsMMCdTimeFormat, [TotalTime.Minute, TotalTime.Second]);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
