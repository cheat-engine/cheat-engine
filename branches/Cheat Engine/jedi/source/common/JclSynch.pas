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
{ The Original Code is JclSynch.pas.                                                               }
{                                                                                                  }
{ The Initial Developers of the Original Code are Marcel van Brakel and Azret Botash.              }
{ Portions created by these individuals are Copyright (C) of these individuals.                    }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Marcel van Brakel                                                                              }
{   Olivier Sannier (obones)                                                                       }
{   Matthias Thoma (mthoma)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains various classes and support routines for implementing synchronisation in      }
{ multithreaded applications. This ranges from interlocked access to simple typed variables to     }
{ wrapper classes for synchronisation primitives provided by the operating system                  }
{ (critical section, semaphore, mutex etc). It also includes three user defined classes to         }
{ complement these.                                                                                }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-01-26 19:03:00 +0100 (sam., 26 janv. 2008)                         $ }
{ Revision:      $Rev:: 2320                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclSynch;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF CLR}
  System.Threading,
  {$IFDEF CLR20}
  System.Security.AccessControl,
  {$ENDIF CLR20}
  {$ENDIF CLR}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  JclBase;

// Locked Integer manipulation
//
// Routines to manipulate simple typed variables in a thread safe manner
{$IFNDEF CLR11}
function LockedAdd(var Target: Integer; Value: Integer): Integer;
{$ENDIF ~CLR11}
function LockedCompareExchange(var Target: Integer; Exch, Comp: Integer): Integer; overload;
{$IFDEF CLR}
function LockedCompareExchange(var Target: TObject; Exch, Comp: TObject): TObject; overload;
{$ELSE}
function LockedCompareExchange(var Target: Pointer; Exch, Comp: Pointer): Pointer; overload;
{$ENDIF CLR}
function LockedDec(var Target: Integer): Integer;
function LockedExchange(var Target: Integer; Value: Integer): Integer;
function LockedExchangeAdd(var Target: Integer; Value: Integer): Integer;
function LockedExchangeDec(var Target: Integer): Integer;
function LockedExchangeInc(var Target: Integer): Integer;
function LockedExchangeSub(var Target: Integer; Value: Integer): Integer;
function LockedInc(var Target: Integer): Integer;
{$IFNDEF CLR11}
function LockedSub(var Target: Integer; Value: Integer): Integer;
{$ENDIF ~CLR11}

// TJclDispatcherObject
//
// Base class for operating system provided synchronisation primitives
type
  TJclWaitResult = (wrAbandoned, wrError, wrIoCompletion, wrSignaled, wrTimeout);

  {$IFDEF CLR}
  TJclWaitHandle = System.Threading.WaitHandle;
  {$ELSE}
  TJclWaitHandle = THandle;
  {$ENDIF CLR}

  TJclDispatcherObject = class(TObject)
  private
    FExisted: Boolean;
    FHandle: TJclWaitHandle;
    FName: string;
  public
    constructor Attach(AHandle: TJclWaitHandle);
    destructor Destroy; override;
    //function MsgWaitFor(const TimeOut: Cardinal): TJclWaitResult; Mask: DWORD): TJclWaitResult;
    //function MsgWaitForEx(const TimeOut: Cardinal): TJclWaitResult; Mask: DWORD): TJclWaitResult;
    {$IFNDEF CLR11}
    function SignalAndWait(const Obj: TJclDispatcherObject; TimeOut: Cardinal;
      Alertable: Boolean): TJclWaitResult;
    {$ENDIF CLR11}
    function WaitAlertable(const TimeOut: Cardinal): TJclWaitResult;
    function WaitFor(const TimeOut: Cardinal): TJclWaitResult;
    function WaitForever: TJclWaitResult;
    property Existed: Boolean read FExisted;
    property Handle: TJclWaitHandle read FHandle;
    property Name: string read FName;
  end;

// Wait functions
//
// Object enabled Wait functions (takes TJclDispatcher objects as parameter as
// opposed to handles) mostly for convenience
function WaitForMultipleObjects(const Objects: array of TJclDispatcherObject;
  WaitAll: Boolean; TimeOut: Cardinal): Cardinal;
function WaitAlertableForMultipleObjects(const Objects: array of TJclDispatcherObject;
  WaitAll: Boolean; TimeOut: Cardinal): Cardinal;

type
  TJclCriticalSection = class(TObject)
  private
    FCriticalSection: TRTLCriticalSection;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    {$IFNDEF CLR}
    class procedure CreateAndEnter(var CS: TJclCriticalSection);
    {$ENDIF ~CLR}
    procedure Enter;
    procedure Leave;
  end;

  {$IFNDEF CLR}
  TJclCriticalSectionEx = class(TJclCriticalSection)
  private
    FSpinCount: Cardinal;
    function GetSpinCount: Cardinal;
    procedure SetSpinCount(const Value: Cardinal);
  public
    constructor Create; override;
    constructor CreateEx(SpinCount: Cardinal; NoFailEnter: Boolean); virtual;
    class function GetSpinTimeOut: Cardinal;
    class procedure SetSpinTimeOut(const Value: Cardinal);
    function TryEnter: Boolean;
    property SpinCount: Cardinal read GetSpinCount write SetSpinCount;
  end;
  {$ENDIF ~CLR}

  TJclEvent = class(TJclDispatcherObject)
  public
    constructor Create({$IFNDEF CLR}SecAttr: PSecurityAttributes;{$ENDIF} Manual, Signaled: Boolean; const Name: string);
    {$IFNDEF CLR}
    constructor Open(Access: Cardinal; Inheritable: Boolean; const Name: string);
    {$ENDIF ~CLR}
    function Pulse: Boolean;
    function ResetEvent: Boolean;
    function SetEvent: Boolean;
  end;

  {$IFNDEF CLR}
  TJclWaitableTimer = class(TJclDispatcherObject)
  private
    FResume: Boolean;
  public
    constructor Create({$IFNDEF CLR}SecAttr: PSecurityAttributes;{$ENDIF} Manual: Boolean; const Name: string);
    constructor Open(Access: Cardinal; Inheritable: Boolean; const Name: string);
    function Cancel: Boolean;
    function SetTimer(const DueTime: Int64; Period: Longint; Resume: Boolean): Boolean;
    function SetTimerApc(const DueTime: Int64; Period: Longint; Resume: Boolean; Apc: TFNTimerAPCRoutine; Arg: Pointer): Boolean;
  end;
  {$ENDIF ~CLR}

  {$IFNDEF CLR}
  TJclSemaphore = class(TJclDispatcherObject)
  public
    constructor Create(SecAttr: PSecurityAttributes; Initial, Maximum: Longint; const Name: string);
    constructor Open(Access: Cardinal; Inheritable: Boolean; const Name: string);
    function Release(ReleaseCount: Longint): Boolean;
    function ReleasePrev(ReleaseCount: Longint; var PrevCount: Longint): Boolean;
  end;
  {$ENDIF ~CLR}

  {$IFNDEF CLR11}
  TJclMutex = class(TJclDispatcherObject)
  public
    constructor Create(SecAttr: {$IFDEF CLR}MutexSecurity{$ELSE}PSecurityAttributes{$ENDIF}; InitialOwner: Boolean;
      const Name: string);
    constructor Open({$IFDEF CLR}Rights: MutexRights;{$ELSE}Access: Cardinal; Inheritable: Boolean;{$ENDIF} const Name: string);
    function Release: Boolean;
  end;
  {$ENDIF ~CLR11}

  {$IFNDEF CLR}
  POptexSharedInfo = ^TOptexSharedInfo;
  TOptexSharedInfo = record
    SpinCount: Integer;      // number of times to try and enter the optex before
                             // waiting on kernel event, 0 on single processor
    LockCount: Integer;      // count of enter attempts
    ThreadId: Longword;      // id of thread that owns the optex, 0 if free
    RecursionCount: Integer; // number of times the optex is owned, 0 if free
  end;

  TJclOptex = class(TObject)
  private
    FEvent: TJclEvent;
    FExisted: Boolean;
    FFileMapping: THandle;
    FName: string;
    FSharedInfo: POptexSharedInfo;
    function GetUniProcess: Boolean;
    function GetSpinCount: Integer;
    procedure SetSpinCount(Value: Integer);
  public
    constructor Create(const Name: string = ''; SpinCount: Integer = 4000);
    destructor Destroy; override;
    procedure Enter;
    procedure Leave;
    function TryEnter: Boolean;
    property Existed: Boolean read FExisted;
    property Name: string read FName;
    property SpinCount: Integer read GetSpinCount write SetSpinCount;
    property UniProcess: Boolean read GetUniProcess;
  end;

  TMrewPreferred = (mpReaders, mpWriters, mpEqual);

  TMrewThreadInfo = record
    ThreadId: Longword;      // client-id of thread
    RecursionCount: Integer; // number of times a thread accessed the mrew
    Reader: Boolean;         // true if reader, false if writer
  end;
  TMrewThreadInfoArray = array of TMrewThreadInfo;
  {$ENDIF ~CLR}

  TJclMultiReadExclusiveWrite = class(TObject)
  private
    {$IFDEF CLR}
    FHandle: ReaderWriterLock;
    FLockCookie: LockCookie;
    FUpgradedWrite: Boolean;
    {$ELSE ~CLR}
    FLock: TJclCriticalSection;
    FPreferred: TMrewPreferred;
    FSemReaders: TJclSemaphore;
    FSemWriters: TJclSemaphore;
    FState: Integer;
    FThreads: TMrewThreadInfoArray;
    FWaitingReaders: Integer;
    FWaitingWriters: Integer;
    procedure AddToThreadList(ThreadId: Longword; Reader: Boolean);
    procedure RemoveFromThreadList(Index: Integer);
    function FindThread(ThreadId: Longword): Integer;
    procedure ReleaseWaiters(WasReading: Boolean);
  protected
    procedure Release;
    {$ENDIF ~CLR}
  public
    {$IFDEF CLR}
    constructor Create;
    {$ELSE ~CLR}
    constructor Create(Preferred: TMrewPreferred);
    {$ENDIF ~CLR}

    destructor Destroy; override;
    procedure BeginRead;
    procedure BeginWrite;
    procedure EndRead;
    procedure EndWrite;
  end;

  {$IFNDEF CLR}
  PMetSectSharedInfo = ^TMetSectSharedInfo;
  TMetSectSharedInfo = record
    Initialized: LongBool;    // Is the metered section initialized?
    SpinLock: Longint;        // Used to gain access to this structure
    ThreadsWaiting: Longint;  // Count of threads waiting
    AvailableCount: Longint;  // Available resource count
    MaximumCount: Longint;    // Maximum resource count
  end;

  PMeteredSection = ^TMeteredSection;
  TMeteredSection = record
    Event: THandle;           // Handle to a kernel event object
    FileMap: THandle;         // Handle to memory mapped file
    SharedInfo: PMetSectSharedInfo;
  end;

  TJclMeteredSection = class(TObject)
  private
    FMetSect: PMeteredSection;
    procedure CloseMeteredSection;
    function InitMeteredSection(InitialCount, MaxCount: Longint; const Name: string; OpenOnly: Boolean): Boolean;
    function CreateMetSectEvent(const Name: string; OpenOnly: Boolean): Boolean;
    function CreateMetSectFileView(InitialCount, MaxCount: Longint; const Name: string; OpenOnly: Boolean): Boolean;
  protected
    procedure AcquireLock;
    procedure ReleaseLock;
  public
    constructor Create(InitialCount, MaxCount: Longint; const Name: string);
    constructor Open(const Name: string);
    destructor Destroy; override;
    function Enter(TimeOut: Longword): TJclWaitResult;
    function Leave(ReleaseCount: Longint): Boolean; overload;
    function Leave(ReleaseCount: Longint; var PrevCount: Longint): Boolean; overload;
  end;
  {$ENDIF ~CLR}

{$IFNDEF CLR}
// Debugging
//
// Note that the following function and structure declarations are all offically
// undocumented and, except for QueryCriticalSection, require Windows NT since
// it is all part of the Windows NT Native API.
{ TODO -cTest : Test this structures }
type
  TEventInfo = record
    EventType: Longint;       // 0 = manual, otherwise auto
    Signaled: LongBool;       // true is signaled
  end;

  TMutexInfo = record
    SignalState: Longint;     // >0 = signaled, <0 = |SignalState| recurs. acquired
    Owned: ByteBool;          // owned by thread
    Abandoned: ByteBool;      // is abandoned?
  end;

  TSemaphoreCounts = record
    CurrentCount: Longint;    // current semaphore count
    MaximumCount: Longint;    // maximum semaphore count
  end;

  TTimerInfo = record
    Remaining: TLargeInteger; // 100ns intervals until signaled
    Signaled: ByteBool;       // is signaled?
  end;

function QueryCriticalSection(CS: TJclCriticalSection; var Info: TRTLCriticalSection): Boolean;
{ TODO -cTest : Test these 4 functions }
function QueryEvent(Handle: THandle; var Info: TEventInfo): Boolean;
function QueryMutex(Handle: THandle; var Info: TMutexInfo): Boolean;
function QuerySemaphore(Handle: THandle; var Info: TSemaphoreCounts): Boolean;
function QueryTimer(Handle: THandle; var Info: TTimerInfo): Boolean;
{$ENDIF ~CLR}

type
  // Exceptions
  EJclWin32HandleObjectError = class(EJclWin32Error);
  EJclDispatcherObjectError = class(EJclWin32Error);
  EJclCriticalSectionError = class(EJclWin32Error);
  EJclEventError = class(EJclWin32Error);
  EJclWaitableTimerError = class(EJclWin32Error);
  EJclSemaphoreError = class(EJclWin32Error);
  EJclMutexError = class(EJclWin32Error);
  EJclMeteredSectionError = class(EJclError);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclSynch.pas $';
    Revision: '$Revision: 2320 $';
    Date: '$Date: 2008-01-26 19:03:00 +0100 (sam., 26 janv. 2008) $';
    LogPath: 'JCL\source\windows'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils,
  {$IFDEF CLR}
  Math,
  {$ENDIF CLR}
  JclLogic, {$IFNDEF CLR}JclWin32, JclRegistry,{$ENDIF} JclResources, JclSysInfo;

const
  RegSessionManager = {HKLM\} 'SYSTEM\CurrentControlSet\Control\Session Manager';
  RegCritSecTimeout = {RegSessionManager\} 'CriticalSectionTimeout';

// Locked Integer manipulation
{$IFDEF CLR}

{$IFNDEF CLR11}
function LockedAdd(var Target: Integer; Value: Integer): Integer;
begin
  Result := Interlocked.Add(Target, Value);
end;
{$ENDIF ~CLR11}

function LockedCompareExchange(var Target: Integer; Exch, Comp: Integer): Integer;
begin
  Result := Interlocked.CompareExchange(Target, Exch, Comp);
end;

function LockedCompareExchange(var Target: TObject; Exch, Comp: TObject): TObject;
begin
  Result := Interlocked.CompareExchange(Target, Exch, Comp);
end;

function LockedDec(var Target: Integer): Integer;
begin
  Result := Interlocked.Decrement(Target);
end;

function LockedExchange(var Target: Integer; Value: Integer): Integer;
begin
  Result := Interlocked.Exchange(Target, Value);
end;

function LockedExchangeAdd(var Target: Integer; Value: Integer): Integer;
begin
  Result := InterlockedExchangeAdd(Target, Value);  // P/Invoke
end;

function LockedExchangeDec(var Target: Integer): Integer;
begin
  Result := LockedExchangeAdd(Target, -1);
end;

function LockedExchangeInc(var Target: Integer): Integer;
begin
  Result := LockedExchangeAdd(Target, 1);
end;

function LockedExchangeSub(var Target: Integer; Value: Integer): Integer;
begin
  Result := LockedExchangeAdd(Target, -Value);
end;

function LockedInc(var Target: Integer): Integer;
begin
  Result := Interlocked.Increment(Target);
end;

{$IFNDEF CLR11}
function LockedSub(var Target: Integer; Value: Integer): Integer;
begin
  Result := Interlocked.Add(Target, -Value);
end;
{$ENDIF ~CLR11}

{$ELSE}

function LockedAdd(var Target: Integer; Value: Integer): Integer;
asm
        MOV     ECX, EAX
        MOV     EAX, EDX
        LOCK XADD [ECX], EAX
        ADD     EAX, EDX
end;

function LockedCompareExchange(var Target: Integer; Exch, Comp: Integer): Integer;
asm
        XCHG    EAX, ECX
        LOCK CMPXCHG [ECX], EDX
end;

function LockedCompareExchange(var Target: Pointer; Exch, Comp: Pointer): Pointer;
asm
        XCHG    EAX, ECX
        LOCK CMPXCHG [ECX], EDX
end;

function LockedDec(var Target: Integer): Integer;
asm
        MOV     ECX, EAX
        MOV     EAX, -1
        LOCK XADD [ECX], EAX
        DEC     EAX
end;

function LockedExchange(var Target: Integer; Value: Integer): Integer;
asm
        MOV     ECX, EAX
        MOV     EAX, EDX
        LOCK XCHG [ECX], EAX
end;

function LockedExchangeAdd(var Target: Integer; Value: Integer): Integer;
asm
        MOV     ECX, EAX
        MOV     EAX, EDX
        LOCK XADD [ECX], EAX
end;

function LockedExchangeDec(var Target: Integer): Integer;
asm
        MOV     ECX, EAX
        MOV     EAX, -1
        LOCK XADD [ECX], EAX
end;

function LockedExchangeInc(var Target: Integer): Integer;
asm
        MOV     ECX, EAX
        MOV     EAX, 1
        LOCK XADD [ECX], EAX
end;

function LockedExchangeSub(var Target: Integer; Value: Integer): Integer;
asm
        MOV     ECX, EAX
        NEG     EDX
        MOV     EAX, EDX
        LOCK XADD [ECX], EAX
end;

function LockedInc(var Target: Integer): Integer;
asm
        MOV     ECX, EAX
        MOV     EAX, 1
        LOCK XADD [ECX], EAX
        INC     EAX
end;

function LockedSub(var Target: Integer; Value: Integer): Integer;
asm
        MOV     ECX, EAX
        NEG     EDX
        MOV     EAX, EDX
        LOCK XADD [ECX], EAX
        ADD     EAX, EDX
end;

{$ENDIF CLR}

//=== { TJclDispatcherObject } ===============================================

function MapSignalResult(const Ret: DWORD): TJclWaitResult;
begin
  case Ret of
    WAIT_ABANDONED:
      Result := wrAbandoned;
    WAIT_OBJECT_0:
      Result := wrSignaled;
    WAIT_TIMEOUT:
      Result := wrTimeout;
    WAIT_IO_COMPLETION:
      Result := wrIoCompletion;
    WAIT_FAILED:
      Result := wrError;
  else
    Result := wrError;
  end;
end;

constructor TJclDispatcherObject.Attach(AHandle: TJclWaitHandle);
begin
  inherited Create;
  FExisted := True;
  FHandle := AHandle;
  FName := '';
end;

destructor TJclDispatcherObject.Destroy;
begin
  {$IFDEF CLR}
  FHandle.Close;
  {$ELSE}
  CloseHandle(FHandle);
  {$ENDIF CLR}
  inherited Destroy;
end;

{ TODO: Use RTDL Version of SignalObjectAndWait }

{$IFNDEF CLR11}
function TJclDispatcherObject.SignalAndWait(const Obj: TJclDispatcherObject;
  TimeOut: Cardinal; Alertable: Boolean): TJclWaitResult;
begin
  {$IFDEF CLR}
  // Other signal results are handled with exceptions
  if TJclWaitHandle.SignalAndWait(Obj.Handle, Handle, IfThen(TimeOut = INFINITE,
                                  System.Threading.Timeout.Infinite, TimeOut), Alertable) then
    Result := wrSignaled
  else
    Result := wrTimeout;
  {$ELSE}
  // Note: Do not make this method virtual! It's only available on NT 4 up...
  Result := MapSignalResult(Cardinal(Windows.SignalObjectAndWait(Obj.Handle, Handle, TimeOut, Alertable)));
  {$ENDIF CLR}
end;
{$ENDIF ~CLR11}

function TJclDispatcherObject.WaitAlertable(const TimeOut: Cardinal): TJclWaitResult;
begin
  {$IFDEF CLR}
  // Other signal results are handled with exceptions
  if Handle.WaitOne(IfThen(TimeOut = INFINITE, System.Threading.Timeout.Infinite, Timeout), True) then
    Result := wrSignaled
  else
    Result := wrTimeout;
  {$ELSE}
  Result := MapSignalResult(Windows.WaitForSingleObjectEx(FHandle, TimeOut, True));
  {$ENDIF CLR}
end;

function TJclDispatcherObject.WaitFor(const TimeOut: Cardinal): TJclWaitResult;
begin
  {$IFDEF CLR}
  // Other signal results are handled with exceptions
  if Handle.WaitOne(IfThen(TimeOut = INFINITE, System.Threading.Timeout.Infinite, Timeout), False) then
    Result := wrSignaled
  else
    Result := wrTimeout;
  {$ELSE}
  Result := MapSignalResult(Windows.WaitForSingleObject(FHandle, TimeOut));
  {$ENDIF CLR}
end;

function TJclDispatcherObject.WaitForever: TJclWaitResult;
begin
  Result := WaitFor(INFINITE);
end;

// Wait functions
function WaitForMultipleObjects(const Objects: array of TJclDispatcherObject;
  WaitAll: Boolean; TimeOut: Cardinal): Cardinal;
var
  Handles: array of TJclWaitHandle;
  I, Count: Integer;
begin
  Count := High(Objects) + 1;
  SetLength(Handles, Count);
  for I := 0 to Count - 1 do
    Handles[I] := Objects[I].Handle;
  {$IFDEF CLR}
  if WaitAll then
  begin
    if TJclWaitHandle.WaitAll(Handles, IfThen(TimeOut = INFINITE, System.Threading.Timeout.Infinite, TimeOut), False) then
      Result := WAIT_OBJECT_0
    else
      Result := WAIT_TIMEOUT;
  end
  else
    Result := TJclWaitHandle.WaitAny(Handles, IfThen(TimeOut = INFINITE, System.Threading.Timeout.Infinite, TimeOut), False);
  {$ELSE}
  Result := Windows.WaitForMultipleObjects(Count, @Handles[0], WaitAll, TimeOut);
  {$ENDIF CLR}
end;

function WaitAlertableForMultipleObjects(const Objects: array of TJclDispatcherObject;
  WaitAll: Boolean; TimeOut: Cardinal): Cardinal;
var
  Handles: array of TJclWaitHandle;
  I, Count: Integer;
begin
  Count := High(Objects) + 1;
  SetLength(Handles, Count);
  for I := 0 to Count - 1 do
    Handles[I] := Objects[I].Handle;
  {$IFDEF CLR}
  if WaitAll then
  begin
    if TJclWaitHandle.WaitAll(Handles, IfThen(TimeOut = INFINITE, System.Threading.Timeout.Infinite, TimeOut), True) then
      Result := WAIT_OBJECT_0
    else
      Result := WAIT_TIMEOUT;
  end
  else
    Result := TJclWaitHandle.WaitAny(Handles, IfThen(TimeOut = INFINITE, System.Threading.Timeout.Infinite, TimeOut), True);
  {$ELSE}
  Result := Windows.WaitForMultipleObjectsEx(Count, @Handles[0], WaitAll, TimeOut, True);
  {$ENDIF CLR}
end;

//=== { TJclCriticalSection } ================================================

constructor TJclCriticalSection.Create;
begin
  inherited Create;
  Windows.InitializeCriticalSection(FCriticalSection);
end;

destructor TJclCriticalSection.Destroy;
begin
  Windows.DeleteCriticalSection(FCriticalSection);
  inherited Destroy;
end;

{$IFNDEF CLR}
class procedure TJclCriticalSection.CreateAndEnter(var CS: TJclCriticalSection);
var
  NewCritSect: TJclCriticalSection;
begin
  NewCritSect := TJclCriticalSection.Create;
  if LockedCompareExchange(Pointer(CS), Pointer(NewCritSect), nil) <> nil then
  begin
    // LoadInProgress was <> nil -> no exchange took place, free the CS
    NewCritSect.Free;
  end;
  CS.Enter;
end;
{$ENDIF ~CLR}

procedure TJclCriticalSection.Enter;
begin
  Windows.EnterCriticalSection(FCriticalSection);
end;

procedure TJclCriticalSection.Leave;
begin
  Windows.LeaveCriticalSection(FCriticalSection);
end;

//== { TJclCriticalSectionEx } ===============================================

{$IFNDEF CLR}

const
  DefaultCritSectSpinCount = 4000;

constructor TJclCriticalSectionEx.Create;
begin
  CreateEx(DefaultCritSectSpinCount, False);
end;

{ TODO: Use RTDL Version of InitializeCriticalSectionAndSpinCount }

constructor TJclCriticalSectionEx.CreateEx(SpinCount: Cardinal;
  NoFailEnter: Boolean);
begin
  FSpinCount := SpinCount;
  if NoFailEnter then
    SpinCount := SpinCount or Cardinal($80000000);

  if not InitializeCriticalSectionAndSpinCount(FCriticalSection, SpinCount) then
    raise EJclCriticalSectionError.CreateRes(@RsSynchInitCriticalSection);
end;

function TJclCriticalSectionEx.GetSpinCount: Cardinal;
begin
  // Spinning only makes sense on multiprocessor systems. On a single processor
  // system the thread would simply waste cycles while the owning thread is
  // suspended and thus cannot release the critical section.
  if ProcessorCount = 1 then
    Result := 0
  else
    Result := FSpinCount;
end;

class function TJclCriticalSectionEx.GetSpinTimeOut: Cardinal;
begin
  Result := Cardinal(RegReadInteger(HKEY_LOCAL_MACHINE, RegSessionManager,
    RegCritSecTimeout));
end;

{ TODO: Use RTLD version of SetCriticalSectionSpinCount }
procedure TJclCriticalSectionEx.SetSpinCount(const Value: Cardinal);
begin
  FSpinCount := SetCriticalSectionSpinCount(FCriticalSection, Value);
end;

class procedure TJclCriticalSectionEx.SetSpinTimeOut(const Value: Cardinal);
begin
  RegWriteInteger(HKEY_LOCAL_MACHINE, RegSessionManager, RegCritSecTimeout,
    Integer(Value));
end;

{ TODO: Use RTLD version of TryEnterCriticalSection }
function TJclCriticalSectionEx.TryEnter: Boolean;
begin
  Result := TryEnterCriticalSection(FCriticalSection);
end;

{$ENDIF ~CLR}

//== { TJclEvent } ===========================================================

constructor TJclEvent.Create({$IFNDEF CLR} SecAttr: PSecurityAttributes; {$ENDIF ~CLR}
  Manual, Signaled: Boolean; const Name: string);
begin
  inherited Create;
  FName := Name;
  {$IFDEF CLR}
  if Manual then
    FHandle := ManualResetEvent.Create(Signaled)
  else
    FHandle := AutoResetEvent.Create(Signaled);
  {$ELSE ~CLR}
  FHandle := Windows.CreateEvent(SecAttr, Manual, Signaled, PChar(FName));
  if FHandle = 0 then
    raise EJclEventError.CreateRes(@RsSynchCreateEvent);
  FExisted := GetLastError = ERROR_ALREADY_EXISTS;
  {$ENDIF ~CLR}
end;

{$IFNDEF CLR}
constructor TJclEvent.Open(Access: Cardinal; Inheritable: Boolean;
  const Name: string);
begin
  FName := Name;
  FExisted := True;
  FHandle := Windows.OpenEvent(Access, Inheritable, PChar(Name));
  if FHandle = 0 then
    raise EJclEventError.CreateRes(@RsSynchOpenEvent);
end;
{$ENDIF ~CLR}

function TJclEvent.Pulse: Boolean;
begin
  {$IFDEF CLR}
  if FHandle is ManualResetEvent then
    Result := ManualResetEvent(FHandle).&Set and ManualResetEvent(FHandle).Reset
  else
    Result := AutoResetEvent(FHandle).&Set and AutoResetEvent(FHandle).Reset;
  {$ELSE ~CLR}
  Result := Windows.PulseEvent(FHandle);
  {$ENDIF ~CLR}
end;

function TJclEvent.ResetEvent: Boolean;
begin
  {$IFDEF CLR}
  if FHandle is ManualResetEvent then
    Result := ManualResetEvent(FHandle).Reset
  else
    Result := AutoResetEvent(FHandle).Reset;
  {$ELSE ~CLR}
  Result := Windows.ResetEvent(FHandle);
  {$ENDIF ~CLR}
end;

function TJclEvent.SetEvent: Boolean;
begin
  {$IFDEF CLR}
  if FHandle is ManualResetEvent then
    Result := ManualResetEvent(FHandle).&Set
  else
    Result := AutoResetEvent(FHandle).&Set;
  {$ELSE ~CLR}
  Result := Windows.SetEvent(FHandle);
  {$ENDIF ~CLR}
end;

//=== { TJclWaitableTimer } ==================================================

{$IFNDEF CLR}
{ TODO: Use RTLD version of CreateWaitableTimer }
constructor TJclWaitableTimer.Create(SecAttr: PSecurityAttributes;
  Manual: Boolean; const Name: string);
begin
  FName := Name;
  FResume := False;
  FHandle := CreateWaitableTimer(SecAttr, Manual, PChar(Name));
  if FHandle = 0 then
    raise EJclWaitableTimerError.CreateRes(@RsSynchCreateWaitableTimer);
  FExisted := GetLastError = ERROR_ALREADY_EXISTS;
end;

{ TODO: Use RTLD version of CancelWaitableTimer }
function TJclWaitableTimer.Cancel: Boolean;
begin
  Result := CancelWaitableTimer(FHandle);
end;

{ TODO: Use RTLD version of OpenWaitableTimer }

constructor TJclWaitableTimer.Open(Access: Cardinal; Inheritable: Boolean;
  const Name: string);
begin
  FExisted := True;
  FName := Name;
  FResume := False;
  FHandle := OpenWaitableTimer(Access, Inheritable, PChar(Name));
  if FHandle = 0 then
    raise EJclWaitableTimerError.CreateRes(@RsSynchOpenWaitableTimer);
end;

{ TODO: Use RTLD version of SetWaitableTimer }
function TJclWaitableTimer.SetTimer(const DueTime: Int64; Period: Longint;
  Resume: Boolean): Boolean;
var
  DT: Int64;
begin
  DT := DueTime;
  Result := SetWaitableTimer(FHandle, DT, Period, nil, nil, FResume);
end;

{ TODO -cHelp : OS restrictions }
function TJclWaitableTimer.SetTimerApc(const DueTime: Int64; Period: Longint;
  Resume: Boolean; Apc: TFNTimerAPCRoutine; Arg: Pointer): Boolean;
var
  DT: Int64;
begin
  DT := DueTime;
  Result := RtdlSetWaitableTimer(FHandle, DT, Period, Apc, Arg, FResume);
  { TODO : Exception for Win9x, older WinNT? }
  // if not Result and (GetLastError = ERROR_CALL_NOT_IMPLEMENTED) then
  //   RaiseLastOSError;
end;
{$ENDIF ~CLR}

//== { TJclSemaphore } =======================================================

{$IFNDEF CLR}
constructor TJclSemaphore.Create(SecAttr: PSecurityAttributes;
  Initial, Maximum: Integer; const Name: string);
begin
  Assert((Initial >= 0) and (Maximum > 0));
  FName := Name;
  FHandle := Windows.CreateSemaphore(SecAttr, Initial, Maximum, PChar(Name));
  if FHandle = 0 then
    raise EJclSemaphoreError.CreateRes(@RsSynchCreateSemaphore);
  FExisted := GetLastError = ERROR_ALREADY_EXISTS;
end;

constructor TJclSemaphore.Open(Access: Cardinal; Inheritable: Boolean;
  const Name: string);
begin
  FName := Name;
  FExisted := True;
  FHandle := Windows.OpenSemaphore(Access, Inheritable, PChar(Name));
  if FHandle = 0 then
    raise EJclSemaphoreError.CreateRes(@RsSynchOpenSemaphore);
end;

function TJclSemaphore.ReleasePrev(ReleaseCount: Longint;
  var PrevCount: Longint): Boolean;
begin
  Result := Windows.ReleaseSemaphore(FHandle, ReleaseCount, @PrevCount);
end;

function TJclSemaphore.Release(ReleaseCount: Integer): Boolean;
begin
  Result := Windows.ReleaseSemaphore(FHandle, ReleaseCount, nil);
end;
{$ENDIF ~CLR}

//=== { TJclMutex } ==========================================================

{$IFNDEF CLR11}
constructor TJclMutex.Create(SecAttr: {$IFDEF CLR}MutexSecurity{$ELSE}PSecurityAttributes{$ENDIF};
  InitialOwner: Boolean; const Name: string);
begin
  inherited Create;
  FName := Name;
  {$IFDEF CLR}
  FHandle := System.Threading.Mutex.Create(InitialOwner, Name, FExisted, SecAttr);
  FExisted := not FExisted;
  {$ELSE}
  FHandle := JclWin32.CreateMutex(SecAttr, Ord(InitialOwner), PChar(Name));
  if FHandle = 0 then
    raise EJclMutexError.CreateRes(@RsSynchCreateMutex);
  FExisted := GetLastError = ERROR_ALREADY_EXISTS;
  {$ENDIF CLR}
end;

constructor TJclMutex.Open({$IFDEF CLR}Rights: MutexRights;{$ELSE}Access: Cardinal; Inheritable: Boolean;{$ENDIF}
  const Name: string);
begin
  inherited Create;
  FName := Name;
  FExisted := True;
  {$IFDEF CLR}
  FHandle := System.Threading.Mutex.OpenExisting(Name, Rights);
  {$ELSE}
  FHandle := Windows.OpenMutex(Access, Inheritable, PChar(Name));
  if FHandle = 0 then
    raise EJclMutexError.CreateRes(@RsSynchOpenMutex);
  {$ENDIF CLR}
end;

function TJclMutex.Release: Boolean;
begin
  {$IFDEF CLR}
  System.Threading.Mutex(Handle).ReleaseMutex;
  Result := True;
  {$ELSE}
  Result := Windows.ReleaseMutex(FHandle);
  {$ENDIF CLR}
end;
{$ENDIF ~CLR11}

{$IFNDEF CLR}
//=== { TJclOptex } ==========================================================

constructor TJclOptex.Create(const Name: string; SpinCount: Integer);
begin
  FExisted := False;
  FName := Name;
  if Name = '' then
  begin
    // None shared optex, don't need filemapping, sharedinfo is local
    FFileMapping := 0;
    FEvent := TJclEvent.Create(nil, False, False, '');
    FSharedInfo := AllocMem(SizeOf(TOptexSharedInfo));
  end
  else
  begin
    // Shared optex, event protects access to sharedinfo. Creation of filemapping
    // doesn't need protection as it will automatically "open" instead of "create"
    // if another process already created it.
    FEvent := TJclEvent.Create(nil, False, False, 'Optex_Event_' + Name);
    FExisted := FEvent.Existed;
    FFileMapping := Windows.CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE,
      0, SizeOf(TOptexSharedInfo), PChar('Optex_MMF_' + Name));
    Assert(FFileMapping <> 0);
    FSharedInfo := Windows.MapViewOfFile(FFileMapping, FILE_MAP_WRITE, 0, 0, 0);
    Assert(FSharedInfo <> nil);
  end;
  SetSpinCount(SpinCount);
end;

destructor TJclOptex.Destroy;
begin
  FreeAndNil(FEvent);
  if UniProcess then
    FreeMem(FSharedInfo)
  else
  begin
    Windows.UnmapViewOfFile(FSharedInfo);
    Windows.CloseHandle(FFileMapping);
  end;
  inherited Destroy;
end;

procedure TJclOptex.Enter;
var
  ThreadId: Longword;
begin
  if TryEnter then
    Exit;
  ThreadId := Windows.GetCurrentThreadId;
  if Windows.InterlockedIncrement(FSharedInfo^.LockCount) = 1 then
  begin
    // Optex was unowned
    FSharedInfo^.ThreadId := ThreadId;
    FSharedInfo^.RecursionCount := 1;
  end
  else
  begin
    if FSharedInfo^.ThreadId = ThreadId then
    begin
      // We already owned it, increase ownership count
      Inc(FSharedInfo^.RecursionCount)
    end
    else
    begin
      // Optex is owner by someone else, wait for it to be released and then
      // immediately take ownership
      FEvent.WaitForever;
      FSharedInfo^.ThreadId := ThreadId;
      FSharedInfo^.RecursionCount := 1;
    end;
  end;
end;

function TJclOptex.GetSpinCount: Integer;
begin
  Result := FSharedInfo^.SpinCount;
end;

function TJclOptex.GetUniProcess: Boolean;
begin
  Result := FFileMapping = 0;
end;

procedure TJclOptex.Leave;
begin
  Dec(FSharedInfo^.RecursionCount);
  if FSharedInfo^.RecursionCount > 0 then
    Windows.InterlockedDecrement(FSharedInfo^.LockCount)
  else
  begin
    FSharedInfo^.ThreadId := 0;
    if Windows.InterlockedDecrement(FSharedInfo^.LockCount) > 0 then
      FEvent.SetEvent;
  end;
end;

procedure TJclOptex.SetSpinCount(Value: Integer);
begin
  if Value < 0 then
    Value := DefaultCritSectSpinCount;
  // Spinning only makes sense on multiprocessor systems
  if ProcessorCount > 1 then
    Windows.InterlockedExchange(Integer(FSharedInfo^.SpinCount), Value);
end;

function TJclOptex.TryEnter: Boolean;
var
  ThreadId: Longword;
  ThreadOwnsOptex: Boolean;
  SpinCount: Integer;
begin
  ThreadId := Windows.GetCurrentThreadId;
  SpinCount := FSharedInfo^.SpinCount;
  repeat
    //ThreadOwnsOptex := InterlockedCompareExchange(Pointer(FSharedInfo^.LockCount),
    //  Pointer(1), Pointer(0)) = Pointer(0); // not available on win95
    ThreadOwnsOptex := LockedCompareExchange(FSharedInfo^.LockCount, 1, 0) = 0;
    if ThreadOwnsOptex then
    begin
      // Optex was unowned
      FSharedInfo^.ThreadId := ThreadId;
      FSharedInfo^.RecursionCount := 1;
    end
    else
    begin
      if FSharedInfo^.ThreadId = ThreadId then
      begin
        // We already owned the Optex
        Windows.InterlockedIncrement(FSharedInfo^.LockCount);
        Inc(FSharedInfo^.RecursionCount);
        ThreadOwnsOptex := True;
      end;
    end;
    Dec(SpinCount);
  until ThreadOwnsOptex or (SpinCount <= 0);
  Result := ThreadOwnsOptex;
end;
{$ENDIF ~CLR}

//=== { TJclMultiReadExclusiveWrite } ========================================

{$IFDEF CLR}
constructor TJclMultiReadExclusiveWrite.Create;
begin
  inherited Create;
  FHandle := ReaderWriterLock.Create;
end;
{$ELSE ~CLR}
constructor TJclMultiReadExclusiveWrite.Create(Preferred: TMrewPreferred);
begin
  inherited Create;
  FLock := TJclCriticalSection.Create;
  FPreferred := Preferred;
  FSemReaders := TJclSemaphore.Create(nil, 0, MaxInt, '');
  FSemWriters := TJclSemaphore.Create(nil, 0, MaxInt, '');
  SetLength(FThreads, 0);
  FState := 0;
  FWaitingReaders := 0;
  FWaitingWriters := 0;
end;
{$ENDIF ~CLR}

destructor TJclMultiReadExclusiveWrite.Destroy;
begin
  {$IFDEF CLR}
  FHandle.Free;
  {$ELSE ~CLR}
  FreeAndNil(FSemReaders);
  FreeAndNil(FSemWriters);
  FreeAndNil(FLock);
  {$ENDIF ~CLR}
  inherited Destroy;
end;

{$IFDEF CLR}
procedure TJclMultiReadExclusiveWrite.BeginRead;
begin
  if not FHandle.IsWriterLockHeld then
    FHandle.AcquireReaderLock(-1);
end;

procedure TJclMultiReadExclusiveWrite.BeginWrite;
begin
  if FHandle.IsReaderLockHeld then
  begin
    FLockCookie := FHandle.UpgradeToWriterLock(-1);
    FUpgradedWrite := True;
  end
  else
    FHandle.AcquireWriterLock(-1);
end;

procedure TJclMultiReadExclusiveWrite.EndRead;
begin
  if not FHandle.IsWriterLockHeld then
    FHandle.ReleaseReaderLock;
end;

procedure TJclMultiReadExclusiveWrite.EndWrite;
begin
  if FUpgradedWrite then
  begin
    FUpgradedWrite := False;
    FHandle.DowngradeFromWriterLock(FLockCookie);
  end
  else
    FHandle.ReleaseWriterLock;
end;
{$ELSE ~CLR}
procedure TJclMultiReadExclusiveWrite.AddToThreadList(ThreadId: Longword;
  Reader: Boolean);
var
  L: Integer;
begin
  // Caller must own lock
  L := Length(FThreads);
  SetLength(FThreads, L + 1);
  FThreads[L].ThreadId := ThreadId;
  FThreads[L].RecursionCount := 1;
  FThreads[L].Reader := Reader;
end;

procedure TJclMultiReadExclusiveWrite.BeginRead;
var
  ThreadId: Longword;
  Index: Integer;
  MustWait: Boolean;
begin
  MustWait := False;
  ThreadId := Windows.GetCurrentThreadId;
  FLock.Enter;
  try
    Index := FindThread(ThreadId);
    if Index >= 0 then
    begin
      // Thread is on threadslist so it is already reading
      Inc(FThreads[Index].RecursionCount);
    end
    else
    begin
      // Request to read (first time)
      AddToThreadList(ThreadId, True);
      if FState >= 0 then
      begin
        // MREW is unowned or only readers. If there are no waiting writers or
        // readers are preferred then allow thread to continue, otherwise it must
        // wait it's turn
        if (FPreferred = mpReaders) or (FWaitingWriters = 0) then
          Inc(FState)
        else
        begin
          Inc(FWaitingReaders);
          MustWait := True;
        end;
      end
      else
      begin
        // MREW is owner by a writer, must wait
        Inc(FWaitingReaders);
        MustWait := True;
      end;
    end;
  finally
    FLock.Leave;
  end;
  if MustWait then
    FSemReaders.WaitForever;
end;

procedure TJclMultiReadExclusiveWrite.BeginWrite;
var
  ThreadId: Longword;
  Index: Integer;
  MustWait: Boolean;
begin
  MustWait := False;
  FLock.Enter;
  try
    ThreadId := Windows.GetCurrentThreadId;
    Index := FindThread(ThreadId);
    if Index < 0 then
    begin
      // Request to write (first time)
      AddToThreadList(ThreadId, False);
      if FState = 0 then
      begin
        // MREW is unowned so start writing
        FState := -1;
      end
      else
      begin
        // MREW is owner, must wait
        Inc(FWaitingWriters);
        MustWait := True;
      end;
    end
    else
    begin
      if FThreads[Index].Reader then
      begin
        // Request to write while reading
        Inc(FThreads[Index].RecursionCount);
        FThreads[Index].Reader := False;
        Dec(FState);
        if FState = 0 then
        begin
          // MREW is unowned so start writing
          FState := -1;
        end
        else
        begin
          // MREW is owned, must wait
          MustWait := True;
          Inc(FWaitingWriters);
        end;
      end
      else
        // Requesting to write while already writing
        Inc(FThreads[Index].RecursionCount);
    end;
  finally
    FLock.Leave;
  end;
  if MustWait then
    FSemWriters.WaitFor(INFINITE);
end;

procedure TJclMultiReadExclusiveWrite.EndRead;
begin
  Release;
end;

procedure TJclMultiReadExclusiveWrite.EndWrite;
begin
  Release;
end;

function TJclMultiReadExclusiveWrite.FindThread(ThreadId: Longword): Integer;
var
  I: Integer;
begin
  // Caller must lock
  Result := -1;
  for I := 0 to Length(FThreads) - 1 do
    if FThreads[I].ThreadId = ThreadId then
    begin
      Result := I;
      Exit;
    end;
end;

procedure TJclMultiReadExclusiveWrite.Release;
var
  ThreadId: Longword;
  Index: Integer;
  WasReading: Boolean;
begin
  ThreadId := GetCurrentThreadId;
  FLock.Enter;
  try
    Index := FindThread(ThreadId);
    if Index >= 0 then
    begin
      Dec(FThreads[Index].RecursionCount);
      if FThreads[Index].RecursionCount = 0 then
      begin
        WasReading := FThreads[Index].Reader;
        if WasReading then
          Dec(FState)
        else
          FState := 0;
        RemoveFromThreadList(Index);
        if FState = 0 then
          ReleaseWaiters(WasReading);
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TJclMultiReadExclusiveWrite.ReleaseWaiters(WasReading: Boolean);
var
  ToRelease: TMrewPreferred;
begin
  // Caller must Lock
  ToRelease := mpEqual;
  case FPreferred of
    mpReaders:
      if FWaitingReaders > 0 then
        ToRelease := mpReaders
      else
      if FWaitingWriters > 0 then
        ToRelease := mpWriters;
    mpWriters:
      if FWaitingWriters > 0 then
        ToRelease := mpWriters
      else
      if FWaitingReaders > 0 then
        ToRelease := mpReaders;
    mpEqual:
      if WasReading then
      begin
        if FWaitingWriters > 0 then
          ToRelease := mpWriters
        else
        if FWaitingReaders > 0 then
          ToRelease := mpReaders;
      end
      else
      begin
        if FWaitingReaders > 0 then
          ToRelease := mpReaders
        else
        if FWaitingWriters > 0 then
          ToRelease := mpWriters;
      end;
  end;
  case ToRelease of
    mpReaders:
      begin
        FState := FWaitingReaders;
        FWaitingReaders := 0;
        FSemReaders.Release(FState);
      end;
    mpWriters:
      begin
        FState := -1;
        Dec(FWaitingWriters);
        FSemWriters.Release(1);
      end;
    mpEqual:
      // no waiters
  end;
end;

procedure TJclMultiReadExclusiveWrite.RemoveFromThreadList(Index: Integer);
var
  L: Integer;
begin
  // Caller must Lock
  L := Length(FThreads);
  Move(FThreads[Index + 1], FThreads[Index], SizeOf(TMrewThreadInfo) * (L - Index - 1));
  SetLength(FThreads, L - 1);
end;
{$ENDIF ~CLR}

//=== { TJclMeteredSection } =================================================

{$IFNDEF CLR}
const
  MAX_METSECT_NAMELEN = 128;

constructor TJclMeteredSection.Create(InitialCount, MaxCount: Integer; const Name: string);
begin
  if (MaxCount < 1) or (InitialCount > MaxCount) or (InitialCount < 0) or
    (Length(Name) > MAX_METSECT_NAMELEN) then
    raise EJclMeteredSectionError.CreateRes(@RsMetSectInvalidParameter);
  FMetSect := PMeteredSection(AllocMem(SizeOf(TMeteredSection)));
  if FMetSect <> nil then
  begin
    if not InitMeteredSection(InitialCount, MaxCount, Name, False) then
    begin
      CloseMeteredSection;
      FMetSect := nil;
      raise EJclMeteredSectionError.CreateRes(@RsMetSectInitialize);
    end;
  end;
end;

constructor TJclMeteredSection.Open(const Name: string);
begin
  FMetSect := nil;
  if Name = '' then
    raise EJclMeteredSectionError.CreateRes(@RsMetSectNameEmpty);
  FMetSect := PMeteredSection(AllocMem(SizeOf(TMeteredSection)));
  Assert(FMetSect <> nil);
  if not InitMeteredSection(0, 0, Name, True) then
  begin
    CloseMeteredSection;
    FMetSect := nil;
    raise EJclMeteredSectionError.CreateRes(@RsMetSectInitialize);
  end;
end;

destructor TJclMeteredSection.Destroy;
begin
  CloseMeteredSection;
  inherited Destroy;
end;

procedure TJclMeteredSection.AcquireLock;
begin
  while Windows.InterlockedExchange(FMetSect^.SharedInfo^.SpinLock, 1) <> 0 do
    Windows.Sleep(0);
end;

procedure TJclMeteredSection.CloseMeteredSection;
begin
  if FMetSect <> nil then
  begin
    if FMetSect^.SharedInfo <> nil then
      Windows.UnmapViewOfFile(FMetSect^.SharedInfo);
    if FMetSect^.FileMap <> 0 then
      Windows.CloseHandle(FMetSect^.FileMap);
    if FMetSect^.Event <> 0 then
      Windows.CloseHandle(FMetSect^.Event);
    FreeMem(FMetSect);
  end;
end;

function TJclMeteredSection.CreateMetSectEvent(const Name: string; OpenOnly: Boolean): Boolean;
var
  FullName: string;
begin
  if Name = '' then
    FMetSect^.Event := Windows.CreateEvent(nil, False, False, nil)
  else
  begin
    FullName :=  'JCL_MSECT_EVT_' + Name;
    if OpenOnly then
      FMetSect^.Event := Windows.OpenEvent(0, False, PChar(FullName))
    else
      FMetSect^.Event := Windows.CreateEvent(nil, False, False, PChar(FullName));
  end;
  Result := FMetSect^.Event <> 0;
end;

function TJclMeteredSection.CreateMetSectFileView(InitialCount, MaxCount: Longint;
  const Name: string; OpenOnly: Boolean): Boolean;
var
  FullName: string;
  LastError: DWORD;
begin
  Result := False;
  if Name = '' then
    FMetSect^.FileMap := Windows.CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, SizeOf(TMetSectSharedInfo), nil)
  else
  begin
    FullName := 'JCL_MSECT_MMF_' + Name;
    if OpenOnly then
      FMetSect^.FileMap := Windows.OpenFileMapping(0, False, PChar(FullName))
    else
      FMetSect^.FileMap := Windows.CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, SizeOf(TMetSectSharedInfo), PChar(FullName));
  end;
  if FMetSect^.FileMap <> 0 then
  begin
    LastError := GetLastError;
    FMetSect^.SharedInfo := Windows.MapViewOfFile(FMetSect^.FileMap, FILE_MAP_WRITE, 0, 0, 0);
    if FMetSect^.SharedInfo <> nil then
    begin
      if LastError = ERROR_ALREADY_EXISTS then
        while not FMetSect^.SharedInfo^.Initialized do Sleep(0)
      else
      begin
        FMetSect^.SharedInfo^.SpinLock := 0;
        FMetSect^.SharedInfo^.ThreadsWaiting := 0;
        FMetSect^.SharedInfo^.AvailableCount := InitialCount;
        FMetSect^.SharedInfo^.MaximumCount := MaxCount;
        Windows.InterlockedExchange(Integer(FMetSect^.SharedInfo^.Initialized), 1);
      end;
      Result := True;
    end;
  end;
end;

function TJclMeteredSection.Enter(TimeOut: Longword): TJclWaitResult;
begin
  Result := wrSignaled;
  while Result = wrSignaled do
  begin
    AcquireLock;
    try
      if FMetSect^.SharedInfo^.AvailableCount >= 1 then
      begin
        Dec(FMetSect^.SharedInfo^.AvailableCount);
        Result := MapSignalResult(WAIT_OBJECT_0);
        Exit;
      end;
      Inc(FMetSect^.SharedInfo^.ThreadsWaiting);
      Windows.ResetEvent(FMetSect^.Event);
    finally
      ReleaseLock;
    end;
    Result := MapSignalResult(Windows.WaitForSingleObject(FMetSect^.Event, TimeOut));
  end;
end;

function TJclMeteredSection.InitMeteredSection(InitialCount, MaxCount: Longint;
  const Name: string; OpenOnly: Boolean): Boolean;
begin
  Result := False;
  if CreateMetSectEvent(Name, OpenOnly) then
    Result := CreateMetSectFileView(InitialCount, MaxCount, Name, OpenOnly);
end;

function TJclMeteredSection.Leave(ReleaseCount: Integer; var PrevCount: Integer): Boolean;
var
  Count: Integer;
begin
  Result := False;
  AcquireLock;
  try
    PrevCount := FMetSect^.SharedInfo^.AvailableCount;
    if (ReleaseCount < 0) or
      (FMetSect^.SharedInfo^.AvailableCount + ReleaseCount > FMetSect^.SharedInfo^.MaximumCount) then
    begin
      Windows.SetLastError(ERROR_INVALID_PARAMETER);
      Exit;
    end;
    Inc(FMetSect^.SharedInfo^.AvailableCount, ReleaseCount);
    ReleaseCount := Min(ReleaseCount, FMetSect^.SharedInfo^.ThreadsWaiting);
    if FMetSect^.SharedInfo^.ThreadsWaiting > 0 then
    begin
      for Count := 0 to ReleaseCount - 1 do
      begin
        Dec(FMetSect^.SharedInfo^.ThreadsWaiting);
        Windows.SetEvent(FMetSect^.Event);
      end;
    end;
  finally
    ReleaseLock;
  end;
  Result := True;
end;

function TJclMeteredSection.Leave(ReleaseCount: Integer): Boolean;
var
  Previous: Longint;
begin
  Result := Leave(ReleaseCount, Previous);
end;

procedure TJclMeteredSection.ReleaseLock;
begin
  Windows.InterlockedExchange(FMetSect^.SharedInfo^.SpinLock, 0);
end;

//=== Debugging ==============================================================

function QueryCriticalSection(CS: TJclCriticalSection; var Info: TRTLCriticalSection): Boolean;
begin
  Result := CS <> nil;
  if Result then
    Info := CS.FCriticalSection;
end;

// Native API functions
// http://undocumented.ntinternals.net/

{ TODO: RTLD version }

type
 TNtQueryProc = function (Handle: THandle; InfoClass: Byte; Info: Pointer;
     Len: Longint; ResLen: PLongint): Longint; stdcall;

var
  _QueryEvent: TNtQueryProc = nil;
  _QueryMutex: TNtQueryProc = nil;
  _QuerySemaphore: TNtQueryProc = nil;
  _QueryTimer: TNtQueryProc = nil;

function CallQueryProc(var P: TNtQueryProc; const Name: string; Handle: THandle;
  Info: Pointer; InfoSize: Longint): Boolean;
var
  NtDll: THandle;
  Status: Longint;
begin
  Result := False;
  if @P = nil then
  begin
    NtDll := GetModuleHandle(PChar('ntdll.dll'));
    if NtDll <> 0 then
      @P := GetProcAddress(NtDll, PChar(Name));
  end;
  if @P <> nil then
  begin
    Status := P(Handle, 0, Info, InfoSize, nil);
    Result := (Status and $80000000) = 0;
  end;
end;

function QueryEvent(Handle: THandle; var Info: TEventInfo): Boolean;
begin
  Result := CallQueryProc(_QueryEvent, 'NtQueryEvent', Handle, @Info, SizeOf(Info));
end;

function QueryMutex(Handle: THandle; var Info: TMutexInfo): Boolean;
begin
  Result := CallQueryProc(_QueryMutex, 'NtQueryMutex', Handle, @Info, SizeOf(Info));
end;

function QuerySemaphore(Handle: THandle; var Info: TSemaphoreCounts): Boolean;
begin
  Result := CallQueryProc(_QuerySemaphore, 'NtQuerySemaphore', Handle, @Info, SizeOf(Info));
end;

function QueryTimer(Handle: THandle; var Info: TTimerInfo): Boolean;
begin
  Result := CallQueryProc(_QueryTimer, 'NtQueryTimer', Handle, @Info, SizeOf(Info));
end;

{$ENDIF ~CLR}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
