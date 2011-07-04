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
{ The Original Code is JclSchedule.pas.                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel Bestebroer.                                 }
{ Portions created Marcel Bestebroer are Copyright (C) Marcel Bestebroer. All rights reserved.     }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Marcel Bestebroer (marcelb)                                                                    }
{   Robert Rossmair (rrossmair)                                                                    }
{   Petr Vones (pvones)                                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains scheduler classes.                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclSchedule;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils,
  JclBase;

type
  TScheduleRecurringKind = (srkOneShot, srkDaily, srkWeekly, srkMonthly, srkYearly);
  TScheduleEndKind = (sekNone, sekDate, sekTriggerCount, sekDayCount);
  TScheduleWeekDay = (swdMonday, swdTuesday, swdWednesday, swdThursday, swdFriday, swdSaturday,
    swdSunday);
  TScheduleWeekDays = set of TScheduleWeekDay;
  TScheduleIndexKind = (sikNone, sikDay, sikWeekDay, sikWeekendDay, sikMonday, sikTuesday,
    sikWednesday, sikThursday, sikFriday, sikSaturday, sikSunday);

const
  sivFirst = 1;
  sivSecond = 2;
  sivThird = 3;
  sivFourth = 4;
  sivLast = -1;

type
  // Forwards
  IJclSchedule = interface;
  IJclDailySchedule = interface;
  IJclWeeklySchedule = interface;
  IJclMonthlySchedule = interface;
  IJclYearlySchedule = interface;

  ESchedule = class(EJclError);

  IJclSchedule = interface(IUnknown)
    ['{1CC54450-7F84-4F27-B1C1-418C451DAD80}']
    function GetStartDate: TTimeStamp;
    function GetRecurringType: TScheduleRecurringKind;
    function GetEndType: TScheduleEndKind;
    function GetEndDate: TTimeStamp;
    function GetEndCount: Cardinal;
    procedure SetStartDate(const Value: TTimeStamp);
    procedure SetRecurringType(Value: TScheduleRecurringKind);
    procedure SetEndType(Value: TScheduleEndKind);
    procedure SetEndDate(const Value: TTimeStamp);
    procedure SetEndCount(Value: Cardinal);

    function TriggerCount: Cardinal;
    function DayCount: Cardinal;
    function LastTriggered: TTimeStamp;

    procedure InitToSavedState(const LastTriggerStamp: TTimeStamp; const LastTriggerCount,
      LastDayCount: Cardinal);
    procedure Reset;
    function NextEvent(CountMissedEvents: Boolean = False): TTimeStamp;
    function NextEventFrom(const FromEvent: TTimeStamp; CountMissedEvent: Boolean = False): TTimeStamp;
    function NextEventFromNow(CountMissedEvents: Boolean = False): TTimeStamp;

    property StartDate: TTimeStamp read GetStartDate write SetStartDate;
    property RecurringType: TScheduleRecurringKind read GetRecurringType write SetRecurringType;
    property EndType: TScheduleEndKind read GetEndType write SetEndType;
    property EndDate: TTimeStamp read GetEndDate write SetEndDate;
    property EndCount: Cardinal read GetEndCount write SetEndCount;
  end;

  IJclScheduleDayFrequency = interface(IUnknown)
    ['{6CF37F0D-56F4-4AE6-BBCA-7B9DFE60F50D}']
    function GetStartTime: Cardinal;
    function GetEndTime: Cardinal;
    function GetInterval: Cardinal;
    procedure SetStartTime(Value: Cardinal);
    procedure SetEndTime(Value: Cardinal);
    procedure SetInterval(Value: Cardinal);

    property StartTime: Cardinal read GetStartTime write SetStartTime;
    property EndTime: Cardinal read GetEndTime write SetEndTime;
    property Interval: Cardinal read GetInterval write SetInterval;
  end;

  IJclDailySchedule = interface(IUnknown)
    ['{540E22C5-BE14-4539-AFB3-E24A67C58D8A}']
    function GetEveryWeekDay: Boolean;
    function GetInterval: Cardinal;
    procedure SetEveryWeekDay(Value: Boolean);
    procedure SetInterval(Value: Cardinal);

    property EveryWeekDay: Boolean read GetEveryWeekDay write SetEveryWeekDay;
    property Interval: Cardinal read GetInterval write SetInterval;
  end;

  IJclWeeklySchedule = interface(IUnknown)
    ['{73F15D99-C6A1-4526-8DE3-A2110E099BBC}']
    function GetDaysOfWeek: TScheduleWeekDays;
    function GetInterval: Cardinal;
    procedure SetDaysOfWeek(Value: TScheduleWeekDays);
    procedure SetInterval(Value: Cardinal);

    property DaysOfWeek: TScheduleWeekDays read GetDaysOfWeek write SetDaysOfWeek;
    property Interval: Cardinal read GetInterval write SetInterval;
  end;

  IJclMonthlySchedule = interface(IUnknown)
    ['{705E17FC-83E6-4385-8D2D-17013052E9B3}']
    function GetIndexKind: TScheduleIndexKind;
    function GetIndexValue: Integer;
    function GetDay: Cardinal;
    function GetInterval: Cardinal;
    procedure SetIndexKind(Value: TScheduleIndexKind);
    procedure SetIndexValue(Value: Integer);
    procedure SetDay(Value: Cardinal);
    procedure SetInterval(Value: Cardinal);

    property IndexKind: TScheduleIndexKind read GetIndexKind write SetIndexKind;
    property IndexValue: Integer read GetIndexValue write SetIndexValue;
    property Day: Cardinal read GetDay write SetDay;
    property Interval: Cardinal read GetInterval write SetInterval;
  end;

  IJclYearlySchedule = interface(IUnknown)
    ['{3E5303B0-FFA0-495A-96BB-14A718A01C1B}']
    function GetIndexKind: TScheduleIndexKind;
    function GetIndexValue: Integer;
    function GetDay: Cardinal;
    function GetMonth: Cardinal;
    function GetInterval: Cardinal;
    procedure SetIndexKind(Value: TScheduleIndexKind);
    procedure SetIndexValue(Value: Integer);
    procedure SetDay(Value: Cardinal);
    procedure SetMonth(Value: Cardinal);
    procedure SetInterval(Value: Cardinal);

    property IndexKind: TScheduleIndexKind read GetIndexKind write SetIndexKind;
    property IndexValue: Integer read GetIndexValue write SetIndexValue;
    property Day: Cardinal read GetDay write SetDay;
    property Month: Cardinal read GetMonth write SetMonth;
    property Interval: Cardinal read GetInterval write SetInterval;
  end;

function CreateSchedule: IJclSchedule;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclSchedule.pas $';
    Revision: '$Revision: 2175 $';
    Date: '$Date: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  JclDateTime, JclResources;  

{$IFNDEF RTL140_UP}

const
  S_OK    = $00000000;
  E_NOINTERFACE = HRESULT($80004002);

type
  TAggregatedObject = class
  private
    FController: Pointer;
    function GetController: IUnknown;
  protected
    { IUnknown }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(Controller: IUnknown);
    property Controller: IUnknown read GetController;
  end;

  TContainedObject = class(TAggregatedObject, IUnknown)
  protected
    { IUnknown }
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
  end;

//=== { TAggregatedObject } ==================================================

constructor TAggregatedObject.Create(Controller: IUnknown);
begin
  FController := Pointer(Controller);
end;

function TAggregatedObject.GetController: IUnknown;
begin
  Result := IUnknown(FController);
end;

function TAggregatedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := IUnknown(FController).QueryInterface(IID, Obj);
end;

function TAggregatedObject._AddRef: Integer;
begin
  Result := IUnknown(FController)._AddRef;
end;

function TAggregatedObject._Release: Integer; stdcall;
begin
  Result := IUnknown(FController)._Release;
end;

//=== { TContainedObject } ===================================================

function TContainedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then Result := S_OK else Result := E_NOINTERFACE;
end;

{$ENDIF ~RTL140_UP}

//=== { TScheduleAggregate } =================================================

type
  TScheduleAggregate = class(TAggregatedObject)
  protected
    procedure CheckInterfaceAllowed;
    function InterfaceAllowed: Boolean;
    function Schedule: IJclSchedule;
    class function RecurringType: TScheduleRecurringKind; virtual;

    function ValidStamp(const Stamp: TTimeStamp): Boolean; virtual; abstract;
    procedure MakeValidStamp(var Stamp: TTimeStamp); virtual; abstract;
    function NextValidStamp(const Stamp: TTimeStamp): TTimeStamp; virtual; abstract;
  end;

procedure TScheduleAggregate.CheckInterfaceAllowed;
begin
  if not InterfaceAllowed then
    RunError(23); // reIntfCastError
end;

function TScheduleAggregate.InterfaceAllowed: Boolean;
begin
  Result := Schedule.RecurringType = RecurringType;
end;

function TScheduleAggregate.Schedule: IJclSchedule;
begin
  Result := Controller as IJclSchedule;
end;

class function TScheduleAggregate.RecurringType: TScheduleRecurringKind;
begin
  Result := srkOneShot;
end;

//=== { TDailyFreq } =========================================================

type
  TDailyFreq = class(TAggregatedObject)
  private
    FStartTime: Cardinal;
    FEndTime: Cardinal;
    FInterval: Cardinal;
  protected
    function ValidStamp(const Stamp: TTimeStamp): Boolean;
    function NextValidStamp(const Stamp: TTimeStamp): TTimeStamp;
  public
    constructor Create(const Controller: IUnknown);
    // IJclScheduleDayFrequency
    function GetStartTime: Cardinal;
    function GetEndTime: Cardinal;
    function GetInterval: Cardinal;
    procedure SetStartTime(Value: Cardinal);
    procedure SetEndTime(Value: Cardinal);
    procedure SetInterval(Value: Cardinal);

    property StartTime: Cardinal read GetStartTime write SetStartTime;
    property EndTime: Cardinal read GetEndTime write SetEndTime;
    property Interval: Cardinal read GetInterval write SetInterval;
  end;

constructor TDailyFreq.Create(const Controller: IUnknown);
begin
  inherited Create(Controller);
  FStartTime := 0;
  FEndTime := HoursToMSecs(24) - 1;
  FInterval := 500;
end;

function TDailyFreq.ValidStamp(const Stamp: TTimeStamp): Boolean;
begin
  Result := (Cardinal(Stamp.Time) >= FStartTime) and (Cardinal(Stamp.Time) <= FEndTime) and
    ((Cardinal(Stamp.Time) - FStartTime) mod FInterval = 0);
end;

function TDailyFreq.NextValidStamp(const Stamp: TTimeStamp): TTimeStamp;
begin
  Result := Stamp;
  if Stamp.Time < Integer(FStartTime) then
    Result.Time := FStartTime
  else
  if ((Cardinal(Stamp.Time) - FStartTime) mod FInterval) <> 0 then
    Result.Time := Stamp.Time + Integer(FInterval-(Cardinal(Stamp.Time) - FStartTime) mod FInterval)
  else
    Result.Time := Stamp.Time + Integer(FInterval);
  if (Result.Time < 0) or (Cardinal(Result.Time) > FEndTime) then
    Result := NullStamp;
end;

function TDailyFreq.GetStartTime: Cardinal;
begin
  Result := FStartTime;
end;

function TDailyFreq.GetEndTime: Cardinal;
begin
  Result := FEndTime;
end;

function TDailyFreq.GetInterval: Cardinal;
begin
  Result := FInterval;
end;

procedure TDailyFreq.SetStartTime(Value: Cardinal);
begin
  if Value <> FStartTime then
  begin
    if Value >= Cardinal(HoursToMSecs(24)) then
      raise ESchedule.CreateRes(@RsScheduleInvalidTime);
    FStartTime := Value;
    if EndTime < StartTime then
      FEndTime := Value;
  end;
end;

procedure TDailyFreq.SetEndTime(Value: Cardinal);
begin
  if Value <> FEndTime then
  begin
    if Value < FStartTime then
      raise ESchedule.CreateRes(@RsScheduleEndBeforeStart);
    if Value >= Cardinal(HoursToMSecs(24)) then
      raise ESchedule.CreateRes(@RsScheduleInvalidTime);
    FEndTime := Value;
  end;
end;

procedure TDailyFreq.SetInterval(Value: Cardinal);
begin
  if Value <> FInterval then
  begin
    if Value >= Cardinal(HoursToMSecs(24)) then
      raise ESchedule.CreateRes(@RsScheduleInvalidTime);
    if Value = 0 then
    begin
      FEndTime := FStartTime;
      FInterval := 1;
    end
    else
      FInterval := Value;
  end;
end;

//=== { TDailySchedule } =====================================================

type
  TDailySchedule = class(TScheduleAggregate)
  private
    FEveryWeekDay: Boolean;
    FInterval: Cardinal;
  protected
    class function RecurringType: TScheduleRecurringKind; override;

    function ValidStamp(const Stamp: TTimeStamp): Boolean; override;
    procedure MakeValidStamp(var Stamp: TTimeStamp); override;
    function NextValidStamp(const Stamp: TTimeStamp): TTimeStamp; override;
  public
    constructor Create(const Controller: IUnknown);
    // IJclDailySchedule
    function GetEveryWeekDay: Boolean;
    function GetInterval: Cardinal;
    procedure SetEveryWeekDay(Value: Boolean);
    procedure SetInterval(Value: Cardinal);

    property EveryWeekDay: Boolean read GetEveryWeekDay write SetEveryWeekDay;
    property Interval: Cardinal read GetInterval write SetInterval;
  end;

constructor TDailySchedule.Create(const Controller: IUnknown);
begin
  inherited Create(Controller);
  FEveryWeekDay := True;
  FInterval := 1;
end;

class function TDailySchedule.RecurringType: TScheduleRecurringKind;
begin
  Result := srkDaily;
end;

function TDailySchedule.ValidStamp(const Stamp: TTimeStamp): Boolean;
begin
  Result := (FEveryWeekDay and (TimeStampDOW(Stamp) < 6)) or
    (not FEveryWeekDay and (Cardinal(Stamp.Date - Schedule.StartDate.Date) mod Interval = 0));
end;

procedure TDailySchedule.MakeValidStamp(var Stamp: TTimeStamp);
begin
  if FEveryWeekDay and (TimeStampDOW(Stamp) >= 6) then
    Inc(Stamp.Date, 2 - (TimeStampDOW(Stamp) - 6))
  else
  if not FEveryWeekDay and (Cardinal(Stamp.Date - Schedule.StartDate.Date) mod Interval <> 0) then
    Inc(Stamp.Date, Interval - Cardinal(Stamp.Date - Schedule.StartDate.Date) mod Interval);
end;

function TDailySchedule.NextValidStamp(const Stamp: TTimeStamp): TTimeStamp;
begin
  Result := Stamp;
  MakeValidStamp(Result);
  if EqualTimeStamps(Stamp, Result) then
  begin
    // Time stamp has not been adjusted (it was valid). Determine the next time stamp
    if FEveryWeekDay then
    begin
      Inc(Result.Date);
      MakeValidStamp(Result);     // Skip over the weekend.
    end
    else
      Inc(Result.Date, Interval); // always valid as we started with a valid stamp
  end;
end;

function TDailySchedule.GetEveryWeekDay: Boolean;
begin
  CheckInterfaceAllowed;
  Result := FEveryWeekDay;
end;

function TDailySchedule.GetInterval: Cardinal;
begin
  CheckInterfaceAllowed;
  if EveryWeekDay then
    Result := 0
  else
    Result := FInterval;
end;

procedure TDailySchedule.SetEveryWeekDay(Value: Boolean);
begin
  CheckInterfaceAllowed;
  FEveryWeekDay := Value;
end;

procedure TDailySchedule.SetInterval(Value: Cardinal);
begin
  CheckInterfaceAllowed;
  if Value = 0 then
    raise ESchedule.CreateRes(@RsScheduleIntervalZero);
  if FEveryWeekDay then
    FEveryWeekDay := False;
  if Value <> FInterval then
    FInterval := Value;
end;

//=== { TWeeklySchedule } ====================================================

type
  TWeeklySchedule = class(TScheduleAggregate)
  private
    FDaysOfWeek: TScheduleWeekDays;
    FInterval: Cardinal;
  protected
    class function RecurringType: TScheduleRecurringKind; override;

    function ValidStamp(const Stamp: TTimeStamp): Boolean; override;
    procedure MakeValidStamp(var Stamp: TTimeStamp); override;
    function NextValidStamp(const Stamp: TTimeStamp): TTimeStamp; override;
  public
    constructor Create(const Controller: IUnknown);
    // IJclWeeklySchedule
    function GetDaysOfWeek: TScheduleWeekDays;
    function GetInterval: Cardinal;
    procedure SetDaysOfWeek(Value: TScheduleWeekDays);
    procedure SetInterval(Value: Cardinal);

    property DaysOfWeek: TScheduleWeekDays read GetDaysOfWeek write SetDaysOfWeek;
    property Interval: Cardinal read GetInterval write SetInterval;
  end;

constructor TWeeklySchedule.Create(const Controller: IUnknown);
begin
  inherited Create(Controller);
  FDaysOfWeek := [swdMonday];
  FInterval := 1;
end;

class function TWeeklySchedule.RecurringType: TScheduleRecurringKind;
begin
  Result := srkWeekly;
end;

function TWeeklySchedule.ValidStamp(const Stamp: TTimeStamp): Boolean;
begin
  Result := (TScheduleWeekDay(TimeStampDOW(Stamp)) in DaysOfWeek) and
    (Cardinal((Stamp.Date - Schedule.StartDate.Date) div 7) mod Interval = 0);
end;

procedure TWeeklySchedule.MakeValidStamp(var Stamp: TTimeStamp);
begin
  while not (TScheduleWeekDay(TimeStampDOW(Stamp) - 1) in DaysOfWeek) do
    Inc(Stamp.Date);
  if (Stamp.Date - Schedule.StartDate.Date) <> 0 then
  begin
    if Cardinal((Stamp.Date - Schedule.StartDate.Date) div 7) mod Interval <> 0 then
      Inc(Stamp.Date, 7 * (Interval -
        (Cardinal((Stamp.Date - Schedule.StartDate.Date) div 7) mod Interval)));
  end;
end;

function TWeeklySchedule.NextValidStamp(const Stamp: TTimeStamp): TTimeStamp;
begin
  Result := Stamp;
  MakeValidStamp(Result);
  if EqualTimeStamps(Stamp, Result) then
  begin
    // Time stamp has not been adjusted (it was valid). Determine the next time stamp
    Inc(Result.Date);
    MakeValidStamp(Result);    // Skip over unwanted days and weeks
  end;
end;

function TWeeklySchedule.GetDaysOfWeek: TScheduleWeekDays;
begin
  CheckInterfaceAllowed;
  Result := FDaysOfWeek;
end;

function TWeeklySchedule.GetInterval: Cardinal;
begin
  CheckInterfaceAllowed;
  Result := FInterval;
end;

procedure TWeeklySchedule.SetDaysOfWeek(Value: TScheduleWeekDays);
begin
  CheckInterfaceAllowed;
  if Value = [] then
    raise ESchedule.CreateRes(@RsScheduleNoDaySpecified);
  FDaysOfWeek := Value;
end;

procedure TWeeklySchedule.SetInterval(Value: Cardinal);
begin
  CheckInterfaceAllowed;
  if Value = 0 then
    raise ESchedule.CreateRes(@RsScheduleIntervalZero);
  FInterval := Value;
end;

//=== { TMonthlySchedule } ===================================================

type
  TMonthlySchedule = class(TScheduleAggregate)
  private
    FIndexKind: TScheduleIndexKind;
    FIndexValue: Integer;
    FDay: Cardinal;
    FInterval: Cardinal;
  protected
    class function RecurringType: TScheduleRecurringKind; override;

    function ValidStamp(const Stamp: TTimeStamp): Boolean; override;
    procedure MakeValidStamp(var Stamp: TTimeStamp); override;
    function NextValidStamp(const Stamp: TTimeStamp): TTimeStamp; override;

    function ValidStampMonthIndex(const TYear, TMonth, TDay: Word): Boolean;
    procedure MakeValidStampMonthIndex(var TYear, TMonth, TDay: Word);
  public
    constructor Create(const Controller: IUnknown);
    // IJclMonthlySchedule
    function GetIndexKind: TScheduleIndexKind;
    function GetIndexValue: Integer;
    function GetDay: Cardinal;
    function GetInterval: Cardinal;
    procedure SetIndexKind(Value: TScheduleIndexKind);
    procedure SetIndexValue(Value: Integer);
    procedure SetDay(Value: Cardinal); 
    procedure SetInterval(Value: Cardinal);

    property IndexKind: TScheduleIndexKind read GetIndexKind write SetIndexKind;
    property IndexValue: Integer read GetIndexValue write SetIndexValue;
    property Day: Cardinal read GetDay write SetDay;
    property Interval: Cardinal read GetInterval write SetInterval;
  end;

constructor TMonthlySchedule.Create(const Controller: IUnknown);
begin
  inherited Create(Controller);
  FIndexKind := sikNone;
  FIndexValue := sivFirst;
  FDay := 1;
  FInterval := 1;
end;

class function TMonthlySchedule.RecurringType: TScheduleRecurringKind;
begin
  Result := srkMonthly;
end;

function TMonthlySchedule.ValidStamp(const Stamp: TTimeStamp): Boolean;
var
  SYear, SMonth, SDay: Word;
  TYear, TMonth, TDay: Word;
begin
  DecodeDate(TimeStampToDateTime(Schedule.StartDate), SYear, SMonth, SDay);
  DecodeDate(TimeStampToDateTime(Stamp), TYear, TMonth, TDay);
  Result := (((TYear * 12 + TMonth) - (SYear * 12 + SMonth)) mod Integer(Interval) = 0) and
    ValidStampMonthIndex(TYear, TMonth, TDay);
end;

procedure TMonthlySchedule.MakeValidStamp(var Stamp: TTimeStamp);
var
  SYear, SMonth, SDay: Word;
  TYear, TMonth, TDay: Word;
  MonthDiff: Integer;
begin
  DecodeDate(TimeStampToDateTime(Schedule.StartDate), SYear, SMonth, SDay);
  DecodeDate(TimeStampToDateTime(Stamp), TYear, TMonth, TDay);
  MonthDiff := (TYear * 12 + TMonth) - (SYear * 12 + SMonth);
  if MonthDiff mod Integer(Interval) <> 0 then
  begin
    Inc(TMonth, Integer(Interval) - (MonthDiff mod Integer(Interval)));
    if TMonth > 12 then
    begin
      Inc(TYear, TMonth div 12);
      TMonth := TMonth mod 12;
    end;
    TDay := 1;
  end;
  MakeValidStampMonthIndex(TYear, TMonth, TDay);
  while DateTimeToTimeStamp(JclDateTime.EncodeDate(TYear, TMonth, TDay)).Date < Stamp.Date do
  begin
    Inc(TMonth, Integer(Interval));
    if TMonth > 12 then
    begin
      Inc(TYear, TMonth div 12);
      TMonth := TMonth mod 12;
    end;
    MakeValidStampMonthIndex(TYear, TMonth, TDay);
  end;
  Stamp.Date := DateTimeToTimeStamp(JclDateTime.EncodeDate(TYear, TMonth, TDay)).Date;
end;

function TMonthlySchedule.NextValidStamp(const Stamp: TTimeStamp): TTimeStamp;
begin
  Result := Stamp;
  MakeValidStamp(Result);
  if EqualTimeStamps(Stamp, Result) then
  begin
    // Time stamp has not been adjusted (it was valid). Determine the next time stamp
    Inc(Result.Date);
    MakeValidStamp(Result);    // Skip over unwanted days and months
  end;
end;

function TMonthlySchedule.ValidStampMonthIndex(const TYear, TMonth, TDay: Word): Boolean;
var
  DIM: Integer;
  TempDay: Integer;
begin
  DIM := DaysInMonth(JclDateTime.EncodeDate(TYear, TMonth, 1));
  case IndexKind of
    sikNone:
      Result := (TDay = Day) or ((Integer(Day) > DIM) and (TDay = DIM));
    sikDay:
      Result :=
        ((IndexValue = sivLast) and (TDay = DIM)) or
        ((IndexValue <> sivLast) and (
          (TDay = IndexValue) or (
            (IndexValue > DIM) and
            (TDay = DIM)
          ) or (
            (IndexValue < 0) and (
              (TDay = DIM + 1 + IndexValue) or (
                (-IndexValue > DIM) and
                (TDay = 1)
              )
            )
          )
        ));
    sikWeekDay:
      begin
        case IndexValue of
          sivFirst:
            TempDay := FirstWeekDay(TYear, TMonth);
          sivLast:
            TempDay := LastWeekDay(TYear, TMonth);
          else
            TempDay := IndexedWeekDay(TYear, TMonth, IndexValue);
            if TempDay = 0 then
            begin
              if IndexValue > 0 then
                TempDay := LastWeekDay(TYear, TMonth)
              else
              if IndexValue < 0 then
                TempDay := FirstWeekDay(TYear, TMonth);
            end;
        end;
        Result := TDay = TempDay;
      end;
    sikWeekendDay:
      begin
        case IndexValue of
          sivFirst:
            TempDay := FirstWeekendDay(TYear, TMonth);
          sivLast:
            TempDay := LastWeekendDay(TYear, TMonth);
          else
            TempDay := IndexedWeekendDay(TYear, TMonth, IndexValue);
            if TempDay = 0 then
            begin
              if IndexValue > 0 then
                TempDay := LastWeekendDay(TYear, TMonth)
              else
              if IndexValue < 0 then
                TempDay := FirstWeekendDay(TYear, TMonth);
            end;
        end;
        Result := TDay = TempDay;
      end;
    sikMonday..sikSunday:
      begin
        case IndexValue of
          sivFirst:
            TempDay := FirstDayOfWeek(TYear, TMonth, Ord(IndexKind) - Ord(sikWeekendDay));
          sivLast:
            TempDay := LastDayOfWeek(TYear, TMonth, Ord(IndexKind) - Ord(sikWeekendDay));
          else
            TempDay := IndexedDayOfWeek(TYear, TMonth, Ord(IndexKind) - Ord(sikWeekendDay),
              IndexValue);
            if TempDay = 0 then
            begin
              if IndexValue > 0 then
                TempDay := LastDayOfWeek(TYear, TMonth, Ord(IndexKind) - Ord(sikWeekendDay))
              else
              if IndexValue < 0 then
                TempDay := FirstDayOfWeek(TYear, TMonth, Ord(IndexKind) - Ord(sikWeekendDay));
            end;
        end;
        Result := TDay = TempDay;
      end;
    else
      Result := False;
  end;
end;

procedure TMonthlySchedule.MakeValidStampMonthIndex(var TYear, TMonth, TDay: Word);
var
  DIM: Integer;
begin
  DIM := DaysInMonth(JclDateTime.EncodeDate(TYear, TMonth, 1));
  case IndexKind of
    sikNone:
      begin
        TDay := Day;
        if Integer(Day) > DIM then
          TDay := DIM;
      end;
    sikDay:
      begin
        if (IndexValue = sivLast) or (Integer(IndexValue) > DIM) then
          TDay := DIM
        else
        if IndexValue > 0 then
          TDay := IndexValue
        else
        begin
          if -IndexValue > DIM then
            TDay := 1
          else
            TDay := DIM + 1 + IndexValue;
        end;
      end;
    sikWeekDay:
      begin
        case IndexValue of
          sivFirst:
            TDay := FirstWeekDay(TYear, TMonth);
          sivLast:
            TDay := LastWeekDay(TYear, TMonth);
          else
            begin
              TDay := IndexedWeekDay(TYear, TMonth, IndexValue);
              if TDay = 0 then
              begin
                if IndexValue > 0 then
                  TDay := LastWeekDay(TYear, TMonth)
                else
                if IndexValue < 0 then
                  TDay := FirstWeekDay(TYear, TMonth);
              end;
            end;
        end;
      end;
    sikWeekendDay:
      begin
        case IndexValue of
          sivFirst:
            TDay := FirstWeekendDay(TYear, TMonth);
          sivLast:
            TDay := LastWeekendDay(TYear, TMonth);
          else
            begin
              TDay := IndexedWeekendDay(TYear, TMonth, IndexValue);
              if TDay = 0 then
              begin
                if IndexValue > 0 then
                  TDay := LastWeekendDay(TYear, TMonth)
                else
                if IndexValue < 0 then
                  TDay := FirstWeekendDay(TYear, TMonth);
              end;
            end;
        end;
      end;
    sikMonday..sikSunday:
      begin
        case IndexValue of
          sivFirst:
            TDay := FirstDayOfWeek(TYear, TMonth, Ord(IndexKind) - Ord(sikWeekendDay));
          sivLast:
            TDay := LastDayOfWeek(TYear, TMonth, Ord(IndexKind) - Ord(sikWeekendDay));
          else
            TDay := IndexedDayOfWeek(TYear, TMonth, Ord(IndexKind) - Ord(sikWeekendDay),
              IndexValue);
            if TDay = 0 then
            begin
              if IndexValue > 0 then
                TDay := LastDayOfWeek(TYear, TMonth, Ord(IndexKind) - Ord(sikWeekendDay))
              else
              if IndexValue < 0 then
                TDay := FirstDayOfWeek(TYear, TMonth, Ord(IndexKind) - Ord(sikWeekendDay));
            end;
        end;
      end;
  end;
end;

function TMonthlySchedule.GetIndexKind: TScheduleIndexKind;
begin
  CheckInterfaceAllowed;
  Result := FIndexKind;
end;

function TMonthlySchedule.GetIndexValue: Integer;
begin
  CheckInterfaceAllowed;
  if not (FIndexKind in [sikDay .. sikSunday]) then
    raise ESchedule.CreateRes(@RsScheduleIndexValueSup);
  Result := FIndexValue;
end;

function TMonthlySchedule.GetDay: Cardinal;
begin
  CheckInterfaceAllowed;
  Result := FDay;
end;

function TMonthlySchedule.GetInterval: Cardinal;
begin
  CheckInterfaceAllowed;
  Result := FInterval;
end;

procedure TMonthlySchedule.SetIndexKind(Value: TScheduleIndexKind);
begin
  CheckInterfaceAllowed;
  FIndexKind := Value;
end;

procedure TMonthlySchedule.SetIndexValue(Value: Integer);
begin
  CheckInterfaceAllowed;
  if not (FIndexKind in [sikDay .. sikSunday]) then
    raise ESchedule.CreateRes(@RsScheduleIndexValueSup);
  if Value = 0 then
    raise ESchedule.CreateRes(@RsScheduleIndexValueZero);
  FIndexValue := Value;
end;

procedure TMonthlySchedule.SetDay(Value: Cardinal);
begin
  CheckInterfaceAllowed;
  if not (FIndexKind in [sikNone]) then
    raise ESchedule.CreateRes(@RsScheduleDayNotSupported);
  if (Value = 0) or (Value > 31) then
    raise ESchedule.CreateRes(@RsScheduleDayInRange);
  FDay := Value;
end;

procedure TMonthlySchedule.SetInterval(Value: Cardinal);
begin
  CheckInterfaceAllowed;
  if Value = 0 then
    raise ESchedule.CreateRes(@RsScheduleIntervalZero);
  FInterval := Value;
end;

//=== { TYearlySchedule } ====================================================

type
  TYearlySchedule = class(TMonthlySchedule)
  private
    FMonth: Cardinal;
  protected
    class function RecurringType: TScheduleRecurringKind; override;

    function ValidStamp(const Stamp: TTimeStamp): Boolean; override;
    procedure MakeValidStamp(var Stamp: TTimeStamp); override;
    function NextValidStamp(const Stamp: TTimeStamp): TTimeStamp; override;
  public
    constructor Create(const Controller: IUnknown);
    // IJclYearlySchedule
    function GetMonth: Cardinal;
    procedure SetMonth(Value: Cardinal);
    
    property Month: Cardinal read GetMonth write SetMonth;
  end;

constructor TYearlySchedule.Create(const Controller: IUnknown);
begin
  inherited Create(Controller);
  FMonth := 1;
end;

class function TYearlySchedule.RecurringType: TScheduleRecurringKind;
begin
  Result := srkYearly;
end;

function TYearlySchedule.ValidStamp(const Stamp: TTimeStamp): Boolean;
var
  SYear, SMonth, SDay: Word;
  TYear, TMonth, TDay: Word;
begin
  JclDateTime.DecodeDate(TimeStampToDateTime(Schedule.StartDate), SYear, SMonth, SDay);
  JclDateTime.DecodeDate(TimeStampToDateTime(Stamp), TYear, TMonth, TDay);
  Result := ((TYear - SYear) mod Integer(Interval) = 0) and (TMonth = Month) and
    ValidStampMonthIndex(TYear, TMonth, TDay);
end;

procedure TYearlySchedule.MakeValidStamp(var Stamp: TTimeStamp);
var
  SYear, SMonth, SDay: Word;
  TYear, TMonth, TDay: Word;
  YearDiff: Integer;
begin
  JclDateTime.DecodeDate(TimeStampToDateTime(Schedule.StartDate), SYear, SMonth, SDay);
  JclDateTime.DecodeDate(TimeStampToDateTime(Stamp), TYear, TMonth, TDay);
  YearDiff := TYear - SYear;
  if YearDiff mod Integer(Interval) <> 0 then
  begin
    Inc(TYear, Integer(Interval) - (YearDiff mod Integer(Interval)));
    TMonth := Month;
    TDay := 1;
  end;
  MakeValidStampMonthIndex(TYear, TMonth, TDay);
  while DateTimeToTimeStamp(JclDateTime.EncodeDate(TYear, TMonth, TDay)).Date < Stamp.Date do
  begin
    Inc(TYear, Integer(Interval));
    TMonth := Month;
    TDay := 1;
    MakeValidStampMonthIndex(TYear, TMonth, TDay);
  end;
  Stamp.Date := DateTimeToTimeStamp(JclDateTime.EncodeDate(TYear, TMonth, TDay)).Date;
end;

function TYearlySchedule.NextValidStamp(const Stamp: TTimeStamp): TTimeStamp;
begin
  Result := Stamp;
  MakeValidStamp(Result);
  if EqualTimeStamps(Stamp, Result) then
  begin
    // Time stamp has not been adjusted (it was valid). Determine the next time stamp
    Inc(Result.Date);
    MakeValidStamp(Result);    // Skip over unwanted days and months
  end;
end;

function TYearlySchedule.GetMonth: Cardinal;
begin
  CheckInterfaceAllowed;
  Result := FMonth;
end;

procedure TYearlySchedule.SetMonth(Value: Cardinal);
begin
  CheckInterfaceAllowed;
  if (Value < 1) or (Value > 12) then
    raise ESchedule.CreateRes(@RsScheduleMonthInRange);
  FMonth := Value;
end;

//=== { TSchedule } ==========================================================

type
  TSchedule = class(TInterfacedObject, IJclSchedule, IJclScheduleDayFrequency, IJclDailySchedule,
    IJclWeeklySchedule, IJclMonthlySchedule, IJclYearlySchedule)
  private
    FStartDate: TTimeStamp;
    FRecurringType: TScheduleRecurringKind;
    FEndType: TScheduleEndKind;
    FEndDate: TTimeStamp;
    FEndCount: Cardinal;
    FDailyFreq: TDailyFreq;
    FDailySchedule: TDailySchedule;
    FWeeklySchedule: TWeeklySchedule;
    FMonthlySchedule: TMonthlySchedule;
    FYearlySchedule: TYearlySchedule;
  protected
    FTriggerCount: Cardinal;
    FDayCount: Cardinal;
    FLastEvent: TTimeStamp;

    function GetNextEventStamp(const From: TTimeStamp): TTimeStamp;

    property DailyFreq: TDailyFreq read FDailyFreq implements IJclScheduleDayFrequency;
    property DailySchedule: TDailySchedule read FDailySchedule implements IJclDailySchedule;
    property WeeklySchedule: TWeeklySchedule read FWeeklySchedule implements IJclWeeklySchedule;
    property MonthlySchedule: TMonthlySchedule read FMonthlySchedule implements IJclMonthlySchedule;
    property YearlySchedule: TYearlySchedule read FYearlySchedule implements IJclYearlySchedule;
  public
    constructor Create;
    destructor Destroy; override;

    // IJclSchedule
    function GetStartDate: TTimeStamp;
    function GetRecurringType: TScheduleRecurringKind;
    function GetEndType: TScheduleEndKind;
    function GetEndDate: TTimeStamp;
    function GetEndCount: Cardinal;
    procedure SetStartDate(const Value: TTimeStamp);
    procedure SetRecurringType(Value: TScheduleRecurringKind);
    procedure SetEndType(Value: TScheduleEndKind);
    procedure SetEndDate(const Value: TTimeStamp);
    procedure SetEndCount(Value: Cardinal);

    function TriggerCount: Cardinal;
    function DayCount: Cardinal;
    function LastTriggered: TTimeStamp;

    procedure InitToSavedState(const LastTriggerStamp: TTimeStamp; const LastTriggerCount,
      LastDayCount: Cardinal);
    procedure Reset;
    function NextEvent(CountMissedEvents: Boolean = False): TTimeStamp;
    function NextEventFrom(const FromEvent: TTimeStamp;
      CountMissedEvent: Boolean = False): TTimeStamp;
    function NextEventFromNow(CountMissedEvents: Boolean = False): TTimeStamp;

    property StartDate: TTimeStamp read GetStartDate write SetStartDate;
    property RecurringType: TScheduleRecurringKind read GetRecurringType write SetRecurringType;
    property EndType: TScheduleEndKind read GetEndType write SetEndType;
    property EndDate: TTimeStamp read GetEndDate write SetEndDate;
    property EndCount: Cardinal read GetEndCount write SetEndCount;
  end;

constructor TSchedule.Create;
var
  InitialStamp: TTimeStamp;
begin
  inherited Create;
  FDailyFreq := TDailyFreq.Create(Self);
  FDailySchedule := TDailySchedule.Create(Self);
  FWeeklySchedule := TWeeklySchedule.Create(Self);
  FMonthlySchedule := TMonthlySchedule.Create(Self);
  FYearlySchedule := TYearlySchedule.Create(Self);
  InitialStamp := DateTimeToTimeStamp(Now);
  InitialStamp.Time := 1000 * (InitialStamp.Time div 1000); // strip of milliseconds
  StartDate := InitialStamp;
  EndType := sekNone;
  RecurringType := srkOneShot;
end;

destructor TSchedule.Destroy;
begin
  FreeAndNil(FYearlySchedule);
  FreeAndNil(FMonthlySchedule);
  FreeAndNil(FWeeklySchedule);
  FreeAndNil(FDailySchedule);
  FreeAndNil(FDailyFreq);
  inherited Destroy;
end;

function TSchedule.GetNextEventStamp(const From: TTimeStamp): TTimeStamp;
var
  UseFrom: TTimeStamp;
begin
  Result := NullStamp;
  UseFrom := From;
  if (From.Date = 0) or (From.Date < StartDate.Date) then
  begin
    UseFrom := StartDate;
    Dec(UseFrom.Time);
  end;
  case RecurringType of
    srkOneShot:
      if TriggerCount = 0 then
        Result := StartDate;
    srkDaily:
      begin
        Result := DailyFreq.NextValidStamp(UseFrom);
        if IsNullTimeStamp(Result) then
        begin
          Result.Date := UseFrom.Date;
          Result.Time := DailyFreq.StartTime;
          Result := DailySchedule.NextValidStamp(Result);
        end
        else
          DailySchedule.MakeValidStamp(Result);
      end;
    srkWeekly:
      begin
        Result := DailyFreq.NextValidStamp(UseFrom);
        if IsNullTimeStamp(Result) then
        begin
          Result.Date := UseFrom.Date;
          Result.Time := DailyFreq.StartTime;
          Result := WeeklySchedule.NextValidStamp(Result);
        end
        else
          WeeklySchedule.MakeValidStamp(Result);
      end;
    srkMonthly:
      begin
        Result := DailyFreq.NextValidStamp(UseFrom);
        if IsNullTimeStamp(Result) then
        begin
          Result.Date := UseFrom.Date;
          Result.Time := DailyFreq.StartTime;
          Result := MonthlySchedule.NextValidStamp(Result);
        end
        else
          MonthlySchedule.MakeValidStamp(Result);
      end;
    srkYearly:
      begin
        Result := DailyFreq.NextValidStamp(UseFrom);
        if IsNullTimeStamp(Result) then
        begin
          Result.Date := UseFrom.Date;
          Result.Time := DailyFreq.StartTime;
          Result := YearlySchedule.NextValidStamp(Result);
        end
        else
          YearlySchedule.MakeValidStamp(Result);
      end;
  end;
  if CompareTimeStamps(Result, UseFrom) < 0 then
    Result := NullStamp;
  if not IsNullTimeStamp(Result) then
  begin
    if ((EndType = sekDate) and (CompareTimeStamps(Result, EndDate) > 0)) or
        ((EndType = sekDayCount) and (DayCount = EndCount) and (UseFrom.Date <> Result.Date)) or
        ((EndType = sekTriggerCount) and (TriggerCount = EndCount)) then
      Result := NullStamp
    else
    begin
      Inc(FTriggerCount);
      if (UseFrom.Date <> Result.Date) or (DayCount = 0) then
        Inc(FDayCount);
      FLastEvent := Result;
    end;
  end;
end;

function TSchedule.GetStartDate: TTimeStamp;
begin
  Result := FStartDate;
end;

function TSchedule.GetRecurringType: TScheduleRecurringKind;
begin
  Result := FRecurringType;
end;

function TSchedule.GetEndType: TScheduleEndKind;
begin
  Result := FEndType;
end;

function TSchedule.GetEndDate: TTimeStamp;
begin
  Result := FEndDate;
end;

function TSchedule.GetEndCount: Cardinal;
begin
  Result := FEndCount;
end;

procedure TSchedule.SetStartDate(const Value: TTimeStamp);
begin
  FStartDate := Value;
end;

procedure TSchedule.SetRecurringType(Value: TScheduleRecurringKind);
begin
  FRecurringType := Value;
end;

procedure TSchedule.SetEndType(Value: TScheduleEndKind);
begin
  FEndType := Value;
end;

procedure TSchedule.SetEndDate(const Value: TTimeStamp);
begin
  FEndDate := Value;
end;

procedure TSchedule.SetEndCount(Value: Cardinal);
begin
  FEndCount := Value;
end;

function TSchedule.TriggerCount: Cardinal;
begin
  Result := FTriggerCount;
end;

function TSchedule.DayCount: Cardinal;
begin
  Result := FDayCount;
end;

function TSchedule.LastTriggered: TTimeStamp;
begin
  Result := FLastEvent;
end;

procedure TSchedule.InitToSavedState(const LastTriggerStamp: TTimeStamp; const LastTriggerCount,
  LastDayCount: Cardinal);
begin
  FLastEvent := LastTriggerStamp;
  FTriggerCount := LastTriggerCount;
  FDayCount := LastDayCount;
end;

procedure TSchedule.Reset;
begin
  FLastEvent := NullStamp;
  FTriggerCount := 0;
  FDayCount := 0;
end;

function TSchedule.NextEvent(CountMissedEvents: Boolean = False): TTimeStamp;
begin
  Result := NextEventFrom(FLastEvent, CountMissedEvents);
end;

function TSchedule.NextEventFrom(const FromEvent: TTimeStamp;
  CountMissedEvent: Boolean = False): TTimeStamp;
begin
  if CountMissedEvent then
  begin
    Result := FLastEvent;
    repeat
      Result := GetNextEventStamp(Result);
    until IsNullTimeStamp(Result) or (CompareTimeStamps(FromEvent, Result) <= 0);
  end
  else
    Result := GetNextEventStamp(FromEvent);
end;

function TSchedule.NextEventFromNow(CountMissedEvents: Boolean = False): TTimeStamp;
begin
  Result := NextEventFrom(DateTimeToTimeStamp(Now), CountMissedEvents);
end;

function CreateSchedule: IJclSchedule;
begin
  Result := TSchedule.Create;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
