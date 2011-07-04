{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

(*****************************************************************************
  This IDL-file has been converted by "the fIDLer".
  [written by -=Assarbad=- <oliver at assarbad dot net> Sept-2004] under MPL
  Visit the fIDLer homepage at: http://assarbad.net/en/stuff/
  {The 3 above lines should be retained}

  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  NOTE:

  There's no guarantee for correct case of parameter or variable types.
  If you have a type like BLA_YADDA in IDL then fIDLer will have converted it
  to 'TBlaYadda' already. But if the type identifier was BLAYADDA and both
  BLA and YADDA being distinct words the result will not be correctly
  capitalized!
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  The original file was 'MSTask.Idl'
  File converted: 2004-10-08@18:38:57

  Cosmetics and review by:
    2004-10-08 - Oliver Schneider <oliver at assarbad dot net>
  Changes:
    2004-11-15 - Scott Price <scottprice@users dot sourceforge dot net>
 *****************************************************************************)

unit MSTask;

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}
interface

uses
  Windows,
  ActiveX;


(*$HPPEMIT '#include <MSTask.h>' *)

//+----------------------------------------------------------------------------
//
//  Task Scheduler
//
//  Microsoft Windows
//  Copyright (C) Microsoft Corporation, 1992 - 1999.
//
//  File:       mstask.idl
//
//  Contents:   ITaskTrigger, ITask, ITaskScheduler, IEnumWorkItems
//              interfaces and related definitions
//
//  History:    06-Sep-95 EricB created
//
//-----------------------------------------------------------------------------


// import "oaidl.idl";

// import "oleidl.idl";

// 148BD520-A2AB-11CE-B11F-00AA00530503 - Task object class ID
// 148BD52A-A2AB-11CE-B11F-00AA00530503 - Task Scheduler class ID
// A6B952F0-A4B1-11D0-997D-00AA006887EC - IScheduledWorkItem interface ID
// 148BD524-A2AB-11CE-B11F-00AA00530503 - ITask interface ID
// 148BD527-A2AB-11CE-B11F-00AA00530503 - ITaskScheduler interface ID
// 148BD528-A2AB-11CE-B11F-00AA00530503 - IEnumWorkItems interface ID
// 148BD52B-A2AB-11CE-B11F-00AA00530503 - ITaskTrigger interface ID

//+----------------------------------------------------------------------------
//
//  Datatypes
//
//-----------------------------------------------------------------------------

const
{$EXTERNALSYM TASK_SUNDAY}
  TASK_SUNDAY = $1;
const
{$EXTERNALSYM TASK_MONDAY}
  TASK_MONDAY = $2;
const
{$EXTERNALSYM TASK_TUESDAY}
  TASK_TUESDAY = $4;
const
{$EXTERNALSYM TASK_WEDNESDAY}
  TASK_WEDNESDAY = $8;
const
{$EXTERNALSYM TASK_THURSDAY}
  TASK_THURSDAY = $10;
const
{$EXTERNALSYM TASK_FRIDAY}
  TASK_FRIDAY = $20;
const
{$EXTERNALSYM TASK_SATURDAY}
  TASK_SATURDAY = $40;
const
{$EXTERNALSYM TASK_FIRST_WEEK}
  TASK_FIRST_WEEK = 1;
const
{$EXTERNALSYM TASK_SECOND_WEEK}
  TASK_SECOND_WEEK = 2;
const
{$EXTERNALSYM TASK_THIRD_WEEK}
  TASK_THIRD_WEEK = 3;
const
{$EXTERNALSYM TASK_FOURTH_WEEK}
  TASK_FOURTH_WEEK = 4;
const
{$EXTERNALSYM TASK_LAST_WEEK}
  TASK_LAST_WEEK = 5;
const
{$EXTERNALSYM TASK_JANUARY}
  TASK_JANUARY = $1;
const
{$EXTERNALSYM TASK_FEBRUARY}
  TASK_FEBRUARY = $2;
const
{$EXTERNALSYM TASK_MARCH}
  TASK_MARCH = $4;
const
{$EXTERNALSYM TASK_APRIL}
  TASK_APRIL = $8;
const
{$EXTERNALSYM TASK_MAY}
  TASK_MAY = $10;
const
{$EXTERNALSYM TASK_JUNE}
  TASK_JUNE = $20;
const
{$EXTERNALSYM TASK_JULY}
  TASK_JULY = $40;
const
{$EXTERNALSYM TASK_AUGUST}
  TASK_AUGUST = $80;
const
{$EXTERNALSYM TASK_SEPTEMBER}
  TASK_SEPTEMBER = $100;
const
{$EXTERNALSYM TASK_OCTOBER}
  TASK_OCTOBER = $200;
const
{$EXTERNALSYM TASK_NOVEMBER}
  TASK_NOVEMBER = $400;
const
{$EXTERNALSYM TASK_DECEMBER}
  TASK_DECEMBER = $800;

const
{$EXTERNALSYM TASK_FLAG_INTERACTIVE}
  TASK_FLAG_INTERACTIVE = $1;
const
{$EXTERNALSYM TASK_FLAG_DELETE_WHEN_DONE}
  TASK_FLAG_DELETE_WHEN_DONE = $2;
const
{$EXTERNALSYM TASK_FLAG_DISABLED}
  TASK_FLAG_DISABLED = $4;
const
{$EXTERNALSYM TASK_FLAG_START_ONLY_IF_IDLE}
  TASK_FLAG_START_ONLY_IF_IDLE = $10;
const
{$EXTERNALSYM TASK_FLAG_KILL_ON_IDLE_END}
  TASK_FLAG_KILL_ON_IDLE_END = $20;
const
{$EXTERNALSYM TASK_FLAG_DONT_START_IF_ON_BATTERIES}
  TASK_FLAG_DONT_START_IF_ON_BATTERIES = $40;
const
{$EXTERNALSYM TASK_FLAG_KILL_IF_GOING_ON_BATTERIES}
  TASK_FLAG_KILL_IF_GOING_ON_BATTERIES = $80;
const
{$EXTERNALSYM TASK_FLAG_RUN_ONLY_IF_DOCKED}
  TASK_FLAG_RUN_ONLY_IF_DOCKED = $100;
const
{$EXTERNALSYM TASK_FLAG_HIDDEN}
  TASK_FLAG_HIDDEN = $200;
const
{$EXTERNALSYM TASK_FLAG_RUN_IF_CONNECTED_TO_INTERNET}
  TASK_FLAG_RUN_IF_CONNECTED_TO_INTERNET = $400;
const
{$EXTERNALSYM TASK_FLAG_RESTART_ON_IDLE_RESUME}
  TASK_FLAG_RESTART_ON_IDLE_RESUME = $800;
const
{$EXTERNALSYM TASK_FLAG_SYSTEM_REQUIRED}
  TASK_FLAG_SYSTEM_REQUIRED = $1000;
const
{$EXTERNALSYM TASK_FLAG_RUN_ONLY_IF_LOGGED_ON}
  TASK_FLAG_RUN_ONLY_IF_LOGGED_ON = $2000;

const
{$EXTERNALSYM TASK_TRIGGER_FLAG_HAS_END_DATE}
  TASK_TRIGGER_FLAG_HAS_END_DATE = $1;
const
{$EXTERNALSYM TASK_TRIGGER_FLAG_KILL_AT_DURATION_END}
  TASK_TRIGGER_FLAG_KILL_AT_DURATION_END = $2;
const
{$EXTERNALSYM TASK_TRIGGER_FLAG_DISABLED}
  TASK_TRIGGER_FLAG_DISABLED = $4;

//
// 1440 = 60 mins/hour * 24 hrs/day since a trigger/TASK could run all day at
// one minute intervals.
//

const
{$EXTERNALSYM TASK_MAX_RUN_TIMES}
  TASK_MAX_RUN_TIMES: Integer = 1440;

//
// The TASK_TRIGGER_TYPE field of the TASK_TRIGGER structure determines
// which member of the TRIGGER_TYPE_UNION field to use.
//
type
{$EXTERNALSYM _TASK_TRIGGER_TYPE}
  _TASK_TRIGGER_TYPE = (
{$EXTERNALSYM TASK_TIME_TRIGGER_ONCE}
    TASK_TIME_TRIGGER_ONCE, // 0   // Ignore the Type field.
{$EXTERNALSYM TASK_TIME_TRIGGER_DAILY}
    TASK_TIME_TRIGGER_DAILY, // 1   // Use DAILY
{$EXTERNALSYM TASK_TIME_TRIGGER_WEEKLY}
    TASK_TIME_TRIGGER_WEEKLY, // 2   // Use WEEKLY
{$EXTERNALSYM TASK_TIME_TRIGGER_MONTHLYDATE}
    TASK_TIME_TRIGGER_MONTHLYDATE, // 3   // Use MONTHLYDATE
{$EXTERNALSYM TASK_TIME_TRIGGER_MONTHLYDOW}
    TASK_TIME_TRIGGER_MONTHLYDOW, // 4   // Use MONTHLYDOW
{$EXTERNALSYM TASK_EVENT_TRIGGER_ON_IDLE}
    TASK_EVENT_TRIGGER_ON_IDLE, // 5   // Ignore the Type field.
{$EXTERNALSYM TASK_EVENT_TRIGGER_AT_SYSTEMSTART}
    TASK_EVENT_TRIGGER_AT_SYSTEMSTART, // 6   // Ignore the Type field.
{$EXTERNALSYM TASK_EVENT_TRIGGER_AT_LOGON}
    TASK_EVENT_TRIGGER_AT_LOGON // 7 // Ignore the Type field.
    );
{$EXTERNALSYM TASK_TRIGGER_TYPE}
  TASK_TRIGGER_TYPE = _TASK_TRIGGER_TYPE;
  TTaskTriggerType = _TASK_TRIGGER_TYPE;

{$EXTERNALSYM PTASK_TRIGGER_TYPE}
  PTASK_TRIGGER_TYPE = ^_TASK_TRIGGER_TYPE;
  PTaskTriggerType = ^_TASK_TRIGGER_TYPE;


type
{$EXTERNALSYM _DAILY}
  _DAILY = packed record
    DaysInterval: WORD;
  end;
{$EXTERNALSYM DAILY}
  DAILY = _DAILY;
  TDaily = _DAILY;


type
{$EXTERNALSYM _WEEKLY}
  _WEEKLY = packed record
    WeeksInterval: WORD;
    rgfDaysOfTheWeek: WORD;
  end;
{$EXTERNALSYM WEEKLY}
  WEEKLY = _WEEKLY;
  TWeekly = _WEEKLY;


type
{$EXTERNALSYM _MONTHLYDATE}
  _MONTHLYDATE = packed record
    rgfDays: DWORD;
    rgfMonths: WORD;
  end;
{$EXTERNALSYM MONTHLYDATE}
  MONTHLYDATE = _MONTHLYDATE;
  TMonthlyDate = _MONTHLYDATE; // OS: Changed capitalization


type
{$EXTERNALSYM _MONTHLYDOW}
  _MONTHLYDOW = packed record
    wWhichWeek: WORD;
    rgfDaysOfTheWeek: WORD;
    rgfMonths: WORD;
  end;
{$EXTERNALSYM MONTHLYDOW}
  MONTHLYDOW = _MONTHLYDOW;
  TMonthlyDOW = _MONTHLYDOW; // OS: Changed capitalization


type
{$EXTERNALSYM _TRIGGER_TYPE_UNION}
  _TRIGGER_TYPE_UNION = packed record
    case Integer of
      0: (Daily: DAILY);
      1: (Weekly: WEEKLY);
      2: (MonthlyDate: MONTHLYDATE);
      3: (MonthlyDOW: MONTHLYDOW);
  end;
{$EXTERNALSYM TRIGGER_TYPE_UNION}
  TRIGGER_TYPE_UNION = _TRIGGER_TYPE_UNION;
  TTriggerTypeUnion = _TRIGGER_TYPE_UNION;


type
{$EXTERNALSYM _TASK_TRIGGER}
  _TASK_TRIGGER = record // SP: removed packed record statement as seemed to affect SetTrigger
    cbTriggerSize: WORD; // Structure size.
    Reserved1: WORD; // Reserved. Must be zero.
    wBeginYear: WORD; // Trigger beginning date year.
    wBeginMonth: WORD; // Trigger beginning date month.
    wBeginDay: WORD; // Trigger beginning date day.
    wEndYear: WORD; // Optional trigger ending date year.
    wEndMonth: WORD; // Optional trigger ending date month.
    wEndDay: WORD; // Optional trigger ending date day.
    wStartHour: WORD; // Run bracket start time hour.
    wStartMinute: WORD; // Run bracket start time minute.
    MinutesDuration: DWORD; // Duration of run bracket.
    MinutesInterval: DWORD; // Run bracket repetition interval.
    rgFlags: DWORD; // Trigger flags.
    TriggerType: TASK_TRIGGER_TYPE; // Trigger type.
    Type_: TRIGGER_TYPE_UNION; // Trigger data.
    Reserved2: WORD; // Reserved. Must be zero.
    wRandomMinutesInterval: WORD; // Maximum number of random minutes
                                   // after start time.

  end;
{$EXTERNALSYM TASK_TRIGGER}
  TASK_TRIGGER = _TASK_TRIGGER;
  TTaskTrigger = _TASK_TRIGGER;

{$EXTERNALSYM PTASK_TRIGGER}
  PTASK_TRIGGER = ^_TASK_TRIGGER;
  PTaskTrigger = ^_TASK_TRIGGER;


//+----------------------------------------------------------------------------
//
//  Interfaces
//
//-----------------------------------------------------------------------------

//+----------------------------------------------------------------------------
//
//  Interface:  ITaskTrigger
//
//  Synopsis:   Trigger object interface. A Task object may contain several
//              of these.
//
//-----------------------------------------------------------------------------
// {148BD52B-A2AB-11CE-B11F-00AA00530503}
const
{$EXTERNALSYM IID_ITaskTrigger}
  IID_ITaskTrigger: TIID = (D1: $148BD52B; D2: $A2AB; D3: $11CE; D4: ($B1, $1F, $00, $AA, $00, $53, $05, $03));


// interface ITaskTrigger;
type
{$EXTERNALSYM ITaskTrigger}
  ITaskTrigger = interface(IUnknown)
    ['{148BD52B-A2AB-11CE-B11F-00AA00530503}']
// Methods:
    function SetTrigger(const pTrigger: TTaskTrigger): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in} const PTASK_TRIGGER pTrigger |*)
    function GetTrigger(out pTrigger: TTaskTrigger): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {out} PTASK_TRIGGER pTrigger |*)
    function GetTriggerString(out ppwszTrigger: LPWSTR): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {out} LPWSTR * ppwszTrigger |*)
  end;

//+----------------------------------------------------------------------------
//
//  Interface:  IScheduledWorkItem
//
//  Synopsis:   Abstract base class for any runnable work item that can be
//              scheduled by the task scheduler.
//
//-----------------------------------------------------------------------------
// {a6b952f0-a4b1-11d0-997d-00aa006887ec}
const
{$EXTERNALSYM IID_IScheduledWorkItem}
  IID_IScheduledWorkItem: TIID = (D1: $A6B952F0; D2: $A4B1; D3: $11D0; D4: ($99, $7D, $00, $AA, $00, $68, $87, $EC));


// interface IScheduledWorkItem;
type
{$EXTERNALSYM IScheduledWorkItem}
  IScheduledWorkItem = interface(IUnknown)
    ['{A6B952F0-A4B1-11D0-997D-00AA006887EC}']
// Methods concerning scheduling:
    function CreateTrigger(out piNewTrigger: WORD; out ppTrigger: ITaskTrigger): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {out} WORD * piNewTrigger, {out} ITaskTrigger ** ppTrigger |*)
    function DeleteTrigger(iTrigger: WORD): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in} WORD iTrigger |*)
    function GetTriggerCount(out pwCount: WORD): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {out} WORD * pwCount |*)
    function GetTrigger(iTrigger: WORD; out ppTrigger: ITaskTrigger): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in} WORD iTrigger, {out} ITaskTrigger ** ppTrigger |*)
    function GetTriggerString(iTrigger: WORD; out ppwszTrigger: LPWSTR): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in} WORD iTrigger, {out} LPWSTR * ppwszTrigger |*)
    function GetRunTimes(pstBegin: PSystemTime; pstEnd: PSystemTime; var pCount: WORD; out rgstTaskTimes: PSystemTime): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in} const LPSYSTEMTIME pstBegin, {in} const LPSYSTEMTIME pstEnd, {in; out} WORD * pCount, {out} LPSYSTEMTIME * rgstTaskTimes |*)
    function GetNextRunTime(var pstNextRun: SYSTEMTIME): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in; out} SYSTEMTIME * pstNextRun |*)
    function SetIdleWait(wIdleMinutes: WORD; wDeadlineMinutes: WORD): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in} WORD wIdleMinutes, {in} WORD wDeadlineMinutes |*)
    function GetIdleWait(out pwIdleMinutes: WORD; out pwDeadlineMinutes: WORD): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {out} WORD * pwIdleMinutes, {out} WORD * pwDeadlineMinutes |*)
// Other methods:
    function Run(): HRESULT; stdcall;
    function Terminate(): HRESULT; stdcall;
    function EditWorkItem(hParent: HWND; dwReserved: DWORD): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in} HWND hParent, {in} DWORD dwReserved |*)
    function GetMostRecentRunTime(out pstLastRun: SYSTEMTIME): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {out} SYSTEMTIME * pstLastRun |*)
    function GetStatus(out phrStatus: HRESULT): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {out} HRESULT * phrStatus |*)
    function GetExitCode(out pdwExitCode: DWORD): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {out} DWORD * pdwExitCode |*)
// Properties:
    function SetComment(pwszComment: LPCWSTR): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in} LPCWSTR pwszComment |*)
    function GetComment(out ppwszComment: LPWSTR): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {out} LPWSTR * ppwszComment |*)
    function SetCreator(pwszCreator: LPCWSTR): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in} LPCWSTR pwszCreator |*)
    function GetCreator(out ppwszCreator: LPWSTR): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {out} LPWSTR * ppwszCreator |*)
    function SetWorkItemData(cbData: WORD; rgbData: PByte): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in} WORD cbData, {in} BYTE rgbData[] |*)
    function GetWorkItemData(out pcbData: WORD; out prgbData: PByte): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {out} WORD * pcbData, {out} BYTE ** prgbData |*)
    function SetErrorRetryCount(wRetryCount: WORD): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in} WORD wRetryCount |*)
    function GetErrorRetryCount(out pwRetryCount: WORD): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {out} WORD * pwRetryCount |*)
    function SetErrorRetryInterval(wRetryInterval: WORD): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in} WORD wRetryInterval |*)
    function GetErrorRetryInterval(out pwRetryInterval: WORD): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {out} WORD * pwRetryInterval |*)
    function SetFlags(dwFlags: DWORD): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in} DWORD dwFlags |*)
    function GetFlags(out pdwFlags: DWORD): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {out} DWORD * pdwFlags |*)
    function SetAccountInformation(pwszAccountName: LPCWSTR; pwszPassword: LPCWSTR): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in} LPCWSTR pwszAccountName, {in} LPCWSTR pwszPassword |*)
    function GetAccountInformation(out ppwszAccountName: LPWSTR): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {out} LPWSTR * ppwszAccountName |*)
  end;

//+----------------------------------------------------------------------------
//
//  Interface:  ITask
//
//  Synopsis:   Task object interface. The primary means of task object
//              manipulation.
//
//-----------------------------------------------------------------------------
// {148BD524-A2AB-11CE-B11F-00AA00530503}
const
{$EXTERNALSYM IID_ITask}
  IID_ITask: TIID = (D1: $148BD524; D2: $A2AB; D3: $11CE; D4: ($B1, $1F, $00, $AA, $00, $53, $05, $03));


// interface ITask;
type
{$EXTERNALSYM ITask}
  ITask = interface(IScheduledWorkItem)
    ['{148BD524-A2AB-11CE-B11F-00AA00530503}']
// Properties that correspond to parameters of CreateProcess:
    function SetApplicationName(pwszApplicationName: LPCWSTR): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in} LPCWSTR pwszApplicationName |*)
    function GetApplicationName(out ppwszApplicationName: LPWSTR): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {out} LPWSTR * ppwszApplicationName |*)
    function SetParameters(pwszParameters: LPCWSTR): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in} LPCWSTR pwszParameters |*)
    function GetParameters(out ppwszParameters: LPWSTR): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {out} LPWSTR * ppwszParameters |*)
    function SetWorkingDirectory(pwszWorkingDirectory: LPCWSTR): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in} LPCWSTR pwszWorkingDirectory |*)
    function GetWorkingDirectory(out ppwszWorkingDirectory: LPWSTR): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {out} LPWSTR * ppwszWorkingDirectory |*)
    function SetPriority(dwPriority: DWORD): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in} DWORD dwPriority |*)
    function GetPriority(out pdwPriority: DWORD): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {out} DWORD * pdwPriority |*)
// Other properties:
    function SetTaskFlags(dwFlags: DWORD): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in} DWORD dwFlags |*)
    function GetTaskFlags(out pdwFlags: DWORD): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {out} DWORD * pdwFlags |*)
    function SetMaxRunTime(dwMaxRunTimeMS: DWORD): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in} DWORD dwMaxRunTimeMS |*)
    function GetMaxRunTime(out pdwMaxRunTimeMS: DWORD): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {out} DWORD * pdwMaxRunTimeMS |*)
  end;

//+----------------------------------------------------------------------------
//
//  Interface:  IEnumWorkItems
//
//  Synopsis:   Work item object enumerator. Enumerates the work item objects
//              within the Tasks folder.
//
//-----------------------------------------------------------------------------
// {148BD528-A2AB-11CE-B11F-00AA00530503}
const
{$EXTERNALSYM IID_IEnumWorkItems}
  IID_IEnumWorkItems: TIID = (D1: $148BD528; D2: $A2AB; D3: $11CE; D4: ($B1, $1F, $00, $AA, $00, $53, $05, $03));


// interface IEnumWorkItems;
type
{$EXTERNALSYM IEnumWorkItems}
  IEnumWorkItems = interface(IUnknown)
    ['{148BD528-A2AB-11CE-B11F-00AA00530503}']
// Methods:
    function Next(celt: ULONG; out rgpwszNames: PLPWSTR; out pceltFetched: ULONG): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in} ULONG celt, {out} LPWSTR ** rgpwszNames, {out} ULONG * pceltFetched |*)
    function Skip(celt: ULONG): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in} ULONG celt |*)
    function Reset(): HRESULT; stdcall;
    function Clone(out ppEnumWorkItems: IEnumWorkItems): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {out} IEnumWorkItems ** ppEnumWorkItems |*)
  end;

//+----------------------------------------------------------------------------
//
//  Interface:  ITaskScheduler
//
//  Synopsis:   Task Scheduler interface. Provides location transparent
//              manipulation of task and/or queue objects within the Tasks
//              folder.
//
//-----------------------------------------------------------------------------
// {148BD527-A2AB-11CE-B11F-00AA00530503}
const
{$EXTERNALSYM IID_ITaskScheduler}
  IID_ITaskScheduler: TIID = (D1: $148BD527; D2: $A2AB; D3: $11CE; D4: ($B1, $1F, $00, $AA, $00, $53, $05, $03));


// interface ITaskScheduler;
type
{$EXTERNALSYM ITaskScheduler}
  ITaskScheduler = interface(IUnknown)
    ['{148BD527-A2AB-11CE-B11F-00AA00530503}']
// Methods:
    function SetTargetComputer(pwszComputer: LPCWSTR): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in} LPCWSTR pwszComputer |*)
    function GetTargetComputer(out ppwszComputer: LPWSTR): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {out} LPWSTR * ppwszComputer |*)
    function Enum(out ppEnumWorkItems: IEnumWorkItems): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {out} IEnumWorkItems ** ppEnumWorkItems |*)
    function Activate(pwszName: LPCWSTR; const riid: TIID; out ppUnk: IUnknown): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in} LPCWSTR pwszName, {in} REFIID riid, {out} IUnknown ** ppUnk |*)
    function Delete(pwszName: LPCWSTR): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in} LPCWSTR pwszName |*)
    function NewWorkItem(pwszTaskName: LPCWSTR; const rclsid: TCLSID; const riid: TIID; out ppUnk: IUnknown): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in} LPCWSTR pwszTaskName, {in} REFCLSID rclsid, {in} REFIID riid, {out} IUnknown ** ppUnk |*)
    function AddWorkItem(pwszTaskName: LPCWSTR; const pWorkItem: IScheduledWorkItem): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in} LPCWSTR pwszTaskName, {in} IScheduledWorkItem * pWorkItem |*)
    function IsOfType(pwszName: LPCWSTR; const riid: TIID): HRESULT; stdcall;
    (*| Parameter(s) was/were [CPP]: {in} LPCWSTR pwszName, {in} REFIID riid |*)
  end;

// EXTERN_C const CLSID CLSID_CTask;
// EXTERN_C const CLSID CLSID_CTaskScheduler;

// {148BD520-A2AB-11CE-B11F-00AA00530503}
const
{$EXTERNALSYM CLSID_CTask}
  CLSID_CTask: TCLSID = (D1: $148BD520; D2: $A2AB; D3: $11CE; D4: ($B1, $1F, $00, $AA, $00, $53, $05, $03));

// {148BD52A-A2AB-11CE-B11F-00AA00530503}
const
{$EXTERNALSYM CLSID_CTaskScheduler}
  CLSID_CTaskScheduler: TCLSID = (D1: $148BD52A; D2: $A2AB; D3: $11CE; D4: ($B1, $1F, $00, $AA, $00, $53, $05, $03));



//
// NOTE: Definition of HPROPSHEETPAGE is from sdk\inc\prsht.h
//       Including this header file causes numerous redefinition errors.
//

type
{$EXTERNALSYM _PSP}
  _PSP = record end;

type
{$EXTERNALSYM HPROPSHEETPAGE}
  HPROPSHEETPAGE = ^_PSP;

type
{$EXTERNALSYM _TASKPAGE}
  _TASKPAGE = (
{$EXTERNALSYM TASKPAGE_TASK}
    TASKPAGE_TASK, // 0
{$EXTERNALSYM TASKPAGE_SCHEDULE}
    TASKPAGE_SCHEDULE, // 1
{$EXTERNALSYM TASKPAGE_SETTINGS}
    TASKPAGE_SETTINGS // 2
    );
{$EXTERNALSYM TASKPAGE}
  TASKPAGE = _TASKPAGE;
  TTaskPage = _TASKPAGE; // OS: Changed capitalization


//+----------------------------------------------------------------------------
//
//  Interface:  IProvideTaskPage
//
//  Synopsis:   Task property page retrieval interface. With this interface,
//              it is possible to retrieve one or more property pages
//              associated with a task object. Task objects inherit this
//              interface.
//
//-----------------------------------------------------------------------------
// {4086658a-cbbb-11cf-b604-00c04fd8d565}
const
{$EXTERNALSYM IID_IProvideTaskPage}
  IID_IProvideTaskPage: TIID = (D1: $4086658A; D2: $CBBB; D3: $11CF; D4: ($B6, $04, $00, $C0, $4F, $D8, $D5, $65));


// interface IProvideTaskPage;
type
{$EXTERNALSYM IProvideTaskPage}
  IProvideTaskPage = interface(IUnknown)
    ['{4086658A-CBBB-11CF-B604-00C04FD8D565}']
// Methods:
    function GetPage(tpType: TTaskPage; fPersistChanges: BOOL; out phPage: HPROPSHEETPAGE): HRESULT; stdcall; // OS: Changed TASKPAGE to TTaskPage
    (*| Parameter(s) was/were [CPP]: {in} TASKPAGE tpType, {in} BOOL fPersistChanges, {out} HPROPSHEETPAGE * phPage |*)
  end;


type
{$EXTERNALSYM ISchedulingAgent}
  ISchedulingAgent = ITaskScheduler;

type
{$EXTERNALSYM IEnumTasks}
  IEnumTasks = IEnumWorkItems;

const
{$EXTERNALSYM IID_ISchedulingAgent}
  IID_ISchedulingAgent: TIID = (D1: $148BD527; D2: $A2AB; D3: $11CE; D4: ($B1, $1F, $00, $AA, $00, $53, $05, $03));

const
{$EXTERNALSYM CLSID_CSchedulingAgent}
  CLSID_CSchedulingAgent: TCLSID = (D1: $148BD52A; D2: $A2AB; D3: $11CE; D4: ($B1, $1F, $00, $AA, $00, $53, $05, $03));

implementation

end.

