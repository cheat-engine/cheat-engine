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
{ The Original Code is JclSvcCtrl.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Flier Lu (<flier_lu att yahoo dott com dott cn>).  }
{ Portions created by Flier Lu are Copyright (C) Flier Lu.  All Rights Reserved.                   }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Flier Lu (flier)                                                                               }
{   Matthias Thoma (mthoma)                                                                        }
{   Olivier Sannier (obones)                                                                       }
{   Petr Vones (pvones)                                                                            }
{   Rik Barker (rikbarker)                                                                         }
{   Robert Rossmair (rrossmair)                                                                    }
{   Warren Postma                                                                                  }
{   Terry Yapt                                                                                     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains routines and classes to control NT service                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-07-17 08:48:34 +0200 (jeu., 17 juil. 2008)                         $ }
{ Revision:      $Rev:: 2391                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

{$R+} { TODO : Why Rangecheck on here? }

unit JclSvcCtrl;

{$I jcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Classes, SysUtils, Contnrs,
  {$IFDEF FPC}
  JwaWinNT, JwaWinSvc,
  {$ELSE}
  WinSvc,
  {$ENDIF FPC}
  JclBase, JclSysUtils;

// Service Types
type
  TJclServiceType =
   (stKernelDriver,        // SERVICE_KERNEL_DRIVER
    stFileSystemDriver,    // SERVICE_FILE_SYSTEM_DRIVER
    stAdapter,             // SERVICE_ADAPTER
    stRecognizerDriver,    // SERVICE_RECOGNIZER_DRIVER
    stWin32OwnProcess,     // SERVICE_WIN32_OWN_PROCESS
    stWin32ShareProcess,   // SERVICE_WIN32_SHARE_PROCESS
    stInteractiveProcess); // SERVICE_INTERACTIVE_PROCESS

  TJclServiceTypes = set of TJclServiceType;

const
  stDriverService = [stKernelDriver, stFileSystemDriver, stRecognizerDriver];
  stWin32Service = [stWin32OwnProcess, stWin32ShareProcess];
  stAllTypeService = stDriverService + stWin32Service + [stAdapter, stInteractiveProcess];

// Service State
type
  TJclServiceState =
   (ssUnknown,         // Just fill the value 0
    ssStopped,         // SERVICE_STOPPED
    ssStartPending,    // SERVICE_START_PENDING
    ssStopPending,     // SERVICE_STOP_PENDING
    ssRunning,         // SERVICE_RUNNING
    ssContinuePending, // SERVICE_CONTINUE_PENDING
    ssPausePending,    // SERVICE_PAUSE_PENDING
    ssPaused);         // SERVICE_PAUSED

  TJclServiceStates = set of TJclServiceState;

const
  ssPendingStates = [ssStartPending, ssStopPending, ssContinuePending, ssPausePending];

// Start Type
type
  TJclServiceStartType =
   (sstBoot,      // SERVICE_BOOT_START
    sstSystem,    // SERVICE_SYSTEM_START
    sstAuto,      // SERVICE_AUTO_START
    sstDemand,    // SERVICE_DEMAND_START
    sstDisabled); // SERVICE_DISABLED

// Error control type
type
  TJclServiceErrorControlType =
   (ectIgnore,    // SSERVICE_ERROR_IGNORE
    ectNormal,    // SSERVICE_ERROR_NORMAL
    ectSevere,    // SSERVICE_ERROR_SEVERE
    ectCritical); // SERVICE_ERROR_CRITICAL


// Controls Accepted
type
  TJclServiceControlAccepted =
   (caStop,          // SERVICE_ACCEPT_STOP
    caPauseContinue, // SERVICE_ACCEPT_PAUSE_CONTINUE
    caShutdown);     // SERVICE_ACCEPT_SHUTDOWN

  TJclServiceControlAccepteds = set of TJclServiceControlAccepted;

// Service sort type
type
  TJclServiceSortOrderType =
   (sotServiceName,
    sotDisplayName,
    sotDescription,
    sotFileName,
    sotServiceState,
    sotStartType,
    sotErrorControlType,
    sotLoadOrderGroup,
    sotWin32ExitCode);

const
  // Everyone in WinNT/2K or Authenticated users in WinXP
  EveryoneSCMDesiredAccess =
    SC_MANAGER_CONNECT or
    SC_MANAGER_ENUMERATE_SERVICE or
    SC_MANAGER_QUERY_LOCK_STATUS or
    STANDARD_RIGHTS_READ;

  LocalSystemSCMDesiredAccess =
    SC_MANAGER_CONNECT or
    SC_MANAGER_ENUMERATE_SERVICE or
    SC_MANAGER_MODIFY_BOOT_CONFIG or
    SC_MANAGER_QUERY_LOCK_STATUS or
    STANDARD_RIGHTS_READ;

  AdministratorsSCMDesiredAccess = SC_MANAGER_ALL_ACCESS;
  DefaultSCMDesiredAccess = EveryoneSCMDesiredAccess;
  DefaultSvcDesiredAccess = SERVICE_ALL_ACCESS;

// Service description
const
  SERVICE_CONFIG_DESCRIPTION     = 1;
  {$EXTERNALSYM SERVICE_CONFIG_DESCRIPTION}
  SERVICE_CONFIG_FAILURE_ACTIONS = 2;
  {$EXTERNALSYM SERVICE_CONFIG_FAILURE_ACTIONS}

type
  LPSERVICE_DESCRIPTIONA = ^SERVICE_DESCRIPTIONA;
  {$EXTERNALSYM LPSERVICE_DESCRIPTIONA}
  SERVICE_DESCRIPTIONA = record
    lpDescription: LPSTR;
  end;
  {$EXTERNALSYM SERVICE_DESCRIPTIONA}
  TServiceDescriptionA = SERVICE_DESCRIPTIONA;
  PServiceDescriptionA = LPSERVICE_DESCRIPTIONA;

type
  TQueryServiceConfig2A = function(hService: SC_HANDLE; dwInfoLevel: DWORD;
    lpBuffer: PByte; cbBufSize: DWORD; var pcbBytesNeeded: DWORD): BOOL; stdcall;

// Service related classes
type
  TJclServiceGroup = class;
  TJclSCManager = class;

  TJclNtService = class(TObject)
  private
    FSCManager: TJclSCManager;
    FHandle: SC_HANDLE;
    FDesiredAccess: DWORD;
    FServiceName: string;
    FDisplayName: string;
    FDescription: string;
    FFileName: TFileName;
    FServiceStartName: string;
    FDependentServices: TList;
    FDependentGroups: TList;
    FDependentByServices: TList;
    FServiceTypes: TJclServiceTypes;
    FServiceState: TJclServiceState;
    FStartType: TJclServiceStartType;
    FErrorControlType: TJclServiceErrorControlType;
    FWin32ExitCode: DWORD;
    FGroup: TJclServiceGroup;
    FControlsAccepted: TJclServiceControlAccepteds;
    FCommitNeeded:Boolean;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function GetDependentService(const Idx: Integer): TJclNtService;
    function GetDependentServiceCount: Integer;
    function GetDependentGroup(const Idx: Integer): TJclServiceGroup;
    function GetDependentGroupCount: Integer;
    function GetDependentByService(const Idx: Integer): TJclNtService;
    function GetDependentByServiceCount: Integer;
  protected
    constructor Create(const ASCManager: TJclSCManager; const SvcStatus: TEnumServiceStatus);
    procedure Open(const ADesiredAccess: DWORD = DefaultSvcDesiredAccess);
    procedure Close;
    function GetServiceStatus: TServiceStatus;
    procedure UpdateDescription;
    procedure UpdateDependents;
    procedure UpdateStatus(const SvcStatus: TServiceStatus);
    procedure UpdateConfig(const SvcConfig: TQueryServiceConfig);
    procedure CommitConfig(var SvcConfig: TQueryServiceConfig);
    procedure SetStartType(AStartType: TJclServiceStartType);
  public
    destructor Destroy; override;
    procedure Refresh;
    procedure Commit;
    procedure Delete;
    function Controls(const ControlType: DWORD; const ADesiredAccess: DWORD = DefaultSvcDesiredAccess): TServiceStatus;
    procedure Start(const Args: array of string; const Sync: Boolean = True); overload;
    procedure Start(const Sync: Boolean = True); overload;
    procedure Stop(const Sync: Boolean = True);
    procedure Pause(const Sync: Boolean = True);
    procedure Continue(const Sync: Boolean = True);
    function WaitFor(const State: TJclServiceState; const TimeOut: DWORD = INFINITE): Boolean;
    property SCManager: TJclSCManager read FSCManager;
    property Active: Boolean read GetActive write SetActive;
    property Handle: SC_HANDLE read FHandle;
    property ServiceName: string read FServiceName;
    property DisplayName: string read FDisplayName;
    property DesiredAccess: DWORD read FDesiredAccess;
    property Description: string read FDescription; // Win2K or later
    property FileName: TFileName read FFileName;
    property ServiceStartName: string read FServiceStartName;
    property DependentServices[const Idx: Integer]: TJclNtService read GetDependentService;
    property DependentServiceCount: Integer read GetDependentServiceCount;
    property DependentGroups[const Idx: Integer]: TJclServiceGroup read GetDependentGroup;
    property DependentGroupCount: Integer read GetDependentGroupCount;
    property DependentByServices[const Idx: Integer]: TJclNtService read GetDependentByService;
    property DependentByServiceCount: Integer read GetDependentByServiceCount;
    property ServiceTypes: TJclServiceTypes read FServiceTypes;
    property ServiceState: TJclServiceState read FServiceState;
    property StartType: TJclServiceStartType read FStartType write SetStartType;
    property ErrorControlType: TJclServiceErrorControlType read FErrorControlType;
    property Win32ExitCode: DWORD read FWin32ExitCode;
    property Group: TJclServiceGroup read FGroup;
    property ControlsAccepted: TJclServiceControlAccepteds read FControlsAccepted;
  end;

  TJclServiceGroup = class(TObject)
  private
    FSCManager: TJclSCManager;
    FName: string;
    FOrder: Integer;
    FServices: TList;
    function GetService(const Idx: Integer): TJclNtService;
    function GetServiceCount: Integer;
  protected
    constructor Create(const ASCManager: TJclSCManager; const AName: string; const AOrder: Integer);
    function Add(const AService: TJclNtService): Integer;
    function Remove(const AService: TJclNtService): Integer;
  public
    destructor Destroy; override;
    property SCManager: TJclSCManager read FSCManager;
    property Name: string read FName;
    property Order: Integer read FOrder;
    property Services[const Idx: Integer]: TJclNtService read GetService;
    property ServiceCount: Integer read GetServiceCount;
  end;

  TJclSCManager = class(TObject)
  private
    FMachineName: string;
    FDatabaseName: string;
    FDesiredAccess: DWORD;
    FHandle: SC_HANDLE;
    FLock: SC_LOCK;
    FServices: TObjectList;
    FGroups: TObjectList;
    FAdvApi32Handle: TModuleHandle;
    FQueryServiceConfig2A: TQueryServiceConfig2A;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function GetService(const Idx: Integer): TJclNtService;
    function GetServiceCount: Integer;
    function GetGroup(const Idx: Integer): TJclServiceGroup;
    function GetGroupCount: Integer;
    procedure SetOrderAsc(const Value: Boolean);
    procedure SetOrderType(const Value: TJclServiceSortOrderType);
    function GetAdvApi32Handle: TModuleHandle;
    function GetQueryServiceConfig2A: TQueryServiceConfig2A;
  protected
    FOrderType: TJclServiceSortOrderType;
    FOrderAsc: Boolean;
    procedure Open;
    procedure Close;
    function AddService(const AService: TJclNtService): Integer;
    function AddGroup(const AGroup: TJclServiceGroup): Integer;
    function GetServiceLockStatus: PQueryServiceLockStatus;
    property AdvApi32Handle: TModuleHandle read GetAdvApi32Handle;
    property QueryServiceConfig2A: TQueryServiceConfig2A read GetQueryServiceConfig2A;
  public
    constructor Create(const AMachineName: string = '';
      const ADesiredAccess: DWORD = DefaultSCMDesiredAccess;
      const ADatabaseName: string = SERVICES_ACTIVE_DATABASE);
    destructor Destroy; override;
    procedure Clear;
    procedure Refresh(const RefreshAll: Boolean = False);
    function Install(const ServiceName, DisplayName, ImageName: string;
      const Description: string = '';
      ServiceTypes: TJclServiceTypes = [stWin32OwnProcess];
      StartType: TJclServiceStartType = sstDemand;
      ErrorControlType: TJclServiceErrorControlType = ectNormal;
      DesiredAccess: DWORD = DefaultSvcDesiredAccess;
      const LoadOrderGroup: TJclServiceGroup = nil; const Dependencies: PChar = nil;
      const Account: PChar = nil; const Password: PChar = nil): TJclNtService;
    procedure Sort(const AOrderType: TJclServiceSortOrderType; const AOrderAsc: Boolean = True);
    function FindService(const SvcName: string; var NtSvc: TJclNtService): Boolean;
    function FindGroup(const GrpName: string; var SvcGrp: TJclServiceGroup;
      const AutoAdd: Boolean = True): Boolean;
    procedure Lock;
    procedure Unlock;
    function IsLocked: Boolean;
    function LockOwner: string;
    function LockDuration: DWORD;
    class function ServiceType(const SvcType: TJclServiceTypes): DWORD; overload;
    class function ServiceType(const SvcType: DWORD): TJclServiceTypes; overload;
    class function ControlAccepted(const CtrlAccepted: TJclServiceControlAccepteds): DWORD; overload;
    class function ControlAccepted(const CtrlAccepted: DWORD): TJclServiceControlAccepteds; overload;
    property MachineName: string read FMachineName;
    property DatabaseName: string read FDatabaseName;
    property DesiredAccess: DWORD read FDesiredAccess;
    property Active: Boolean read GetActive write SetActive;
    property Handle: SC_HANDLE read FHandle;
    property Services[const Idx: Integer]: TJclNtService read GetService;
    property ServiceCount: Integer read GetServiceCount;
    property Groups[const Idx: Integer]: TJclServiceGroup read GetGroup;
    property GroupCount: Integer read GetGroupCount;
    property OrderType: TJclServiceSortOrderType read FOrderType write SetOrderType;
    property OrderAsc: Boolean read FOrderAsc write SetOrderAsc;
  end;

// helper functions
function GetServiceStatus(ServiceHandle: SC_HANDLE): DWORD;
function GetServiceStatusWaitingIfPending(ServiceHandle: SC_HANDLE): DWORD;

function GetServiceStatusByName(const AServer,AServiceName:string):TJclServiceState;
function StopServiceByName(const AServer, AServiceName: String):Boolean;
function StartServiceByName(const AServer,AServiceName: String):Boolean;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/windows/JclSvcCtrl.pas $';
    Revision: '$Revision: 2391 $';
    Date: '$Date: 2008-07-17 08:48:34 +0200 (jeu., 17 juil. 2008) $';
    LogPath: 'JCL\source\windows'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF FPC}
  WinSysUt,
  JwaRegStr,
  {$ELSE}
  RegStr,
  {$ENDIF FPC}
  Math,
  JclRegistry, JclStrings, JclSysInfo;

const
  INVALID_SCM_HANDLE = 0;

  ServiceTypeMapping: array [TJclServiceType] of DWORD =
    (SERVICE_KERNEL_DRIVER, SERVICE_FILE_SYSTEM_DRIVER, SERVICE_ADAPTER,
     SERVICE_RECOGNIZER_DRIVER, SERVICE_WIN32_OWN_PROCESS,
     SERVICE_WIN32_SHARE_PROCESS, SERVICE_INTERACTIVE_PROCESS);

  ServiceControlAcceptedMapping: array [TJclServiceControlAccepted] of DWORD =
    (SERVICE_ACCEPT_STOP, SERVICE_ACCEPT_PAUSE_CONTINUE, SERVICE_ACCEPT_SHUTDOWN);

//=== { TJclNtService } ======================================================

constructor TJclNtService.Create(const ASCManager: TJclSCManager; const SvcStatus: TEnumServiceStatus);
begin
  Assert(Assigned(ASCManager));
  inherited Create;
  FSCManager := ASCManager;
  FHandle := INVALID_SCM_HANDLE;
  FServiceName := SvcStatus.lpServiceName;
  FDisplayName := SvcStatus.lpDisplayName;
  FDescription := '';
  FGroup := nil;
  FDependentServices := TList.Create;
  FDependentGroups := TList.Create;
  FDependentByServices := nil; // Create on demand
  FSCManager.AddService(Self);
end;

destructor TJclNtService.Destroy;
begin
  FreeAndNil(FDependentServices);
  FreeAndNil(FDependentGroups);
  FreeAndNil(FDependentByServices);
  inherited Destroy;
end;

procedure TJclNtService.UpdateDescription;
var
  Ret: BOOL;
  BytesNeeded: DWORD;
  PSvcDesc: PServiceDescriptionA;
begin
  if Assigned(SCManager.QueryServiceConfig2A) then
  try
    PSvcDesc := nil;
    BytesNeeded := 4096;
    repeat
      ReallocMem(PSvcDesc, BytesNeeded);
      Ret := SCManager.QueryServiceConfig2A(FHandle, SERVICE_CONFIG_DESCRIPTION,
        PByte(PSvcDesc), BytesNeeded, BytesNeeded);
    until Ret or (GetLastError <> ERROR_INSUFFICIENT_BUFFER);
    Win32Check(Ret);

    FDescription := PSvcDesc.lpDescription;
  finally
    FreeMem(PSvcDesc);
  end;
end;

function TJclNtService.GetActive: Boolean;
begin
  Result := FHandle <> INVALID_SCM_HANDLE;
end;

procedure TJclNtService.SetActive(const Value: Boolean);
begin
  if Value <> GetActive then
  begin
    if Value then
      Open
    else
      Close;
    Assert(Value = GetActive);
  end;
end;

procedure TJclNtService.SetStartType(AStartType: TJclServiceStartType);
begin
  if AStartType <> FStartType then
  begin
    FStartType := AStartType;
    FCommitNeeded := True;
  end;
end;

procedure TJclNtService.UpdateDependents;
var
  I: Integer;
  Ret: BOOL;
  PBuf: Pointer;
  PEss: PEnumServiceStatus;
  NtSvc: TJclNtService;
  BytesNeeded, ServicesReturned: DWORD;
begin
  Open(SERVICE_ENUMERATE_DEPENDENTS);
  try
    if Assigned(FDependentByServices) then
      FDependentByServices.Clear
    else
      FDependentByServices := TList.Create;

    try
      PBuf := nil;
      BytesNeeded := 40960;
      repeat
        ReallocMem(PBuf, BytesNeeded);
        Ret := EnumDependentServices(FHandle, SERVICE_STATE_ALL,
          PEnumServiceStatus(PBuf){$IFNDEF FPC}^{$ENDIF}, BytesNeeded, BytesNeeded, ServicesReturned);
      until Ret or (GetLastError <> ERROR_INSUFFICIENT_BUFFER);
      Win32Check(Ret);

      PEss := PBuf;
      for I := 0 to ServicesReturned - 1 do
      begin
        if (PEss.lpServiceName[1] <> SC_GROUP_IDENTIFIER) and
          (SCManager.FindService(PEss.lpServiceName, NtSvc)) then
          FDependentByServices.Add(NtSvc);
        Inc(PEss);
      end;
    finally
      FreeMem(PBuf);
    end;
  finally
    Close;
  end;
end;

function TJclNtService.GetDependentService(const Idx: Integer): TJclNtService;
begin
  Result := TJclNtService(FDependentServices.Items[Idx]);
end;

function TJclNtService.GetDependentServiceCount: Integer;
begin
  Result := FDependentServices.Count;
end;

function TJclNtService.GetDependentGroup(const Idx: Integer): TJclServiceGroup;
begin
  Result := TJclServiceGroup(FDependentGroups.Items[Idx]);
end;

function TJclNtService.GetDependentGroupCount: Integer;
begin
  Result := FDependentGroups.Count;
end;

function TJclNtService.GetDependentByService(const Idx: Integer): TJclNtService;
begin
  if not Assigned(FDependentByServices) then
    UpdateDependents;
  Result := TJclNtService(FDependentByServices.Items[Idx])
end;

function TJclNtService.GetDependentByServiceCount: Integer;
begin
  if not Assigned(FDependentByServices) then
    UpdateDependents;
  Result := FDependentByServices.Count;
end;

function TJclNtService.GetServiceStatus: TServiceStatus;
begin
  Assert(Active);
  Assert((DesiredAccess and SERVICE_QUERY_STATUS) <> 0);
  Win32Check(QueryServiceStatus(FHandle, Result));
end;


procedure TJclNtService.UpdateStatus(const SvcStatus: TServiceStatus);
begin
  with SvcStatus do
  begin
    FServiceTypes := TJclSCManager.ServiceType(dwServiceType);
    FServiceState := TJclServiceState(dwCurrentState);
    FControlsAccepted := TJclSCManager.ControlAccepted(dwControlsAccepted);
    FWin32ExitCode := dwWin32ExitCode;
  end;
end;

procedure TJclNtService.UpdateConfig(const SvcConfig: TQueryServiceConfig);

  procedure UpdateLoadOrderGroup;
  begin
    if not Assigned(FGroup) then
      SCManager.FindGroup(SvcConfig.lpLoadOrderGroup, FGroup)
    else
    if CompareText(Group.Name, SvcConfig.lpLoadOrderGroup) = 0 then
    begin
      FGroup.Remove(Self);
      SCManager.FindGroup(SvcConfig.lpLoadOrderGroup, FGroup);
      FGroup.Add(Self);
    end;
  end;

  procedure UpdateDependencies;
  var
    P: PChar;
    NtSvc: TJclNtService;
    SvcGrp: TJclServiceGroup;
  begin
    P := SvcConfig.lpDependencies;
    FDependentServices.Clear;
    FDependentGroups.Clear;
    if Assigned(P) then
    while P^ <> #0 do
    begin
      if P^ = SC_GROUP_IDENTIFIER then
      begin
        SCManager.FindGroup(P + 1, SvcGrp);
        FDependentGroups.Add(SvcGrp);
      end
      else
      if SCManager.FindService(P, NtSvc) then
        FDependentServices.Add(NtSvc);
      Inc(P, StrLen(P) + 1);
    end;
  end;

begin
  with SvcConfig do
  begin
    FFileName := lpBinaryPathName;
    FStartType := TJclServiceStartType(dwStartType);
    FServiceStartName := lpServiceStartName;
    FErrorControlType := TJclServiceErrorControlType(dwErrorControl);
    UpdateLoadOrderGroup;
    UpdateDependencies;
  end;
end;

procedure TJclNtService.CommitConfig(var SvcConfig: TQueryServiceConfig);
begin
  with SvcConfig do
  begin
    StrCopy(lpBinaryPathName, PChar(FileName));
    dwStartType := Ord(StartType);    {TJclServiceStartType}
    dwErrorControl := Ord(ErrorControlType);  {TJclServiceErrorControlType}
    //UpdateLoadOrderGroup;
    //UpdateDependencies;
  end;
end;

procedure TJclNtService.Open(const ADesiredAccess: DWORD);
begin
  Assert((ADesiredAccess and (not SERVICE_ALL_ACCESS)) = 0);
  Active := False;
  FDesiredAccess := ADesiredAccess;
  FHandle := OpenService(SCManager.Handle, PChar(ServiceName), DesiredAccess);
  Win32Check(FHandle <> INVALID_SCM_HANDLE);
end;

procedure TJclNtService.Close;
begin
  Assert(Active);
  Win32Check(CloseServiceHandle(FHandle));
  FHandle := INVALID_SCM_HANDLE;
end;

procedure TJclNtService.Refresh;
var
  Ret: BOOL;
  BytesNeeded: DWORD;
  PQrySvcCnfg: PQueryServiceConfig;
begin
  Open(SERVICE_QUERY_STATUS or SERVICE_QUERY_CONFIG);
  try
    UpdateDescription;
    UpdateStatus(GetServiceStatus);
    try
      PQrySvcCnfg := nil;
      BytesNeeded := 4096;
      repeat
        ReallocMem(PQrySvcCnfg, BytesNeeded);
        Ret := QueryServiceConfig(FHandle, PQrySvcCnfg, BytesNeeded, BytesNeeded);
      until Ret or (GetLastError <> ERROR_INSUFFICIENT_BUFFER);
      Win32Check(Ret);

      UpdateConfig(PQrySvcCnfg^);
    finally
      FreeMem(PQrySvcCnfg);
    end;
  finally
    Close;
  end;
end;

// Commit is reverse of Refresh.
procedure TJclNtService.Commit;
var
  Ret: BOOL;
  BytesNeeded: DWORD;
  PQrySvcCnfg: PQueryServiceConfig;
begin
 if not FCommitNeeded then
   Exit;
 FCommitNeeded := False;

  Open(SERVICE_CHANGE_CONFIG or SERVICE_QUERY_STATUS or SERVICE_QUERY_CONFIG);
  try
    //UpdateDescription;
    //UpdateStatus(GetServiceStatus);
    try
      PQrySvcCnfg := nil;
      BytesNeeded := 4096;
      repeat
        ReallocMem(PQrySvcCnfg, BytesNeeded);
        Ret := QueryServiceConfig(FHandle, PQrySvcCnfg, BytesNeeded, BytesNeeded);
      until Ret or (GetLastError <> ERROR_INSUFFICIENT_BUFFER);
      Win32Check(Ret);

      CommitConfig(PQrySvcCnfg^);
      Win32Check(ChangeServiceConfig(Handle,
        PQrySvcCnfg^.dwServiceType,
        PQrySvcCnfg^.dwStartType,
        PQrySvcCnfg^.dwErrorControl,
        nil, {PQrySvcCnfg^.lpBinaryPathName,}
        nil, {PQrySvcCnfg^.lpLoadOrderGroup,}
        nil, {PQrySvcCnfg^.dwTagId,}
        nil, {PQrySvcCnfg^.lpDependencies,}
        nil, {PQrySvcCnfg^.lpServiceStartName,}
        nil, {password-write only-not readable}
        PQrySvcCnfg^.lpDisplayName));
    finally
      FreeMem(PQrySvcCnfg);
    end;
  finally
    Close;
  end;
end;

procedure TJclNtService.Delete;
{$IFDEF FPC}
const
  _DELETE = $00010000; { Renamed from DELETE }
{$ENDIF FPC}
begin
  Open(_DELETE);
  try
    Win32Check(DeleteService(FHandle));
  finally
    Close;
  end;
end;

procedure TJclNtService.Start(const Args: array of string; const Sync: Boolean);
type
  PStrArray = ^TStrArray;
  TStrArray = array [0..32767] of PChar;
var
  I: Integer;
  PServiceArgVectors: PChar;
begin
  Open(SERVICE_START);
  try
    try
      if Length(Args) = 0 then
        PServiceArgVectors := nil
      else
      begin
        GetMem(PServiceArgVectors, SizeOf(PChar)*Length(Args));
        for I := 0 to Length(Args) - 1 do
          PStrArray(PServiceArgVectors)^[I] := PChar(Args[I]);
      end;
      Win32Check(StartService(FHandle, Length(Args), PServiceArgVectors));
    finally
      FreeMem(PServiceArgVectors);
    end;
  finally
    Close;
  end;
  if Sync then
    WaitFor(ssRunning);
end;

procedure TJclNtService.Start(const Sync: Boolean = True);
begin
  Start([], Sync);
end;

function TJclNtService.Controls(const ControlType: DWORD; const ADesiredAccess: DWORD): TServiceStatus;
begin
  Open(ADesiredAccess);
  try
    Win32Check(ControlService(FHandle, ControlType, Result));
  finally
    Close;
  end;
end;

procedure TJclNtService.Stop(const Sync: Boolean);
begin
  Controls(SERVICE_CONTROL_STOP, SERVICE_STOP);
  if Sync then
    WaitFor(ssStopped);
end;

procedure TJclNtService.Pause(const Sync: Boolean);
begin
  Controls(SERVICE_CONTROL_PAUSE, SERVICE_PAUSE_CONTINUE);
  if Sync then
    WaitFor(ssPaused);
end;

procedure TJclNtService.Continue(const Sync: Boolean);
begin
  Controls(SERVICE_CONTROL_CONTINUE, SERVICE_PAUSE_CONTINUE);
  if Sync then
    WaitFor(ssRunning);
end;

function TJclNtService.WaitFor(const State: TJclServiceState; const TimeOut: DWORD): Boolean;
var
  SvcStatus: TServiceStatus;
  WaitedState, StartTickCount, OldCheckPoint, WaitTime: DWORD;
begin
  WaitedState := DWORD(State);
  Open(SERVICE_QUERY_STATUS);
  try
    StartTickCount := GetTickCount;
    OldCheckPoint := 0;
    while True do
    begin
      SvcStatus := GetServiceStatus;
      if SvcStatus.dwCurrentState = WaitedState then
        Break;
      if SvcStatus.dwCheckPoint > OldCheckPoint then
      begin
        StartTickCount := GetTickCount;
        OldCheckPoint := SvcStatus.dwCheckPoint;
      end
      else
      begin
        if TimeOut <> INFINITE then
          { TODO : Do we need to disable RangeCheck? }
          if (GetTickCount - StartTickCount) > Max(SvcStatus.dwWaitHint, TimeOut) then
            Break;
      end;
      WaitTime := SvcStatus.dwWaitHint div 10;
      if WaitTime < 1000 then
        WaitTime := 1000
      else
      if WaitTime > 10000 then
        WaitTime := 10000;
      Sleep(WaitTime);
    end;
    Result := SvcStatus.dwCurrentState = WaitedState;
  finally
    Close;
  end;
end;

//=== { TJclServiceGroup } ===================================================

constructor TJclServiceGroup.Create(const ASCManager: TJclSCManager;
  const AName: string; const AOrder: Integer);
begin
  Assert(Assigned(ASCManager));
  inherited Create;
  FSCManager := ASCManager;
  FName := AName;
  if FName <> '' then
    FOrder := AOrder
  else
    FOrder := MaxInt;
  FServices := TList.Create;
end;

destructor TJclServiceGroup.Destroy;
begin
  FreeAndNil(FServices);
  inherited Destroy;
end;

function TJclServiceGroup.Add(const AService: TJclNtService): Integer;
begin
  Result := FServices.Add(AService);
end;

function TJclServiceGroup.Remove(const AService: TJclNtService): Integer;
begin
  Result := FServices.Remove(AService);
end;

function TJclServiceGroup.GetService(const Idx: Integer): TJclNtService;
begin
  Result := TJclNtService(FServices.Items[Idx]);
end;

function TJclServiceGroup.GetServiceCount: Integer;
begin
  Result := FServices.Count;
end;

//=== { TJclSCManager } ======================================================

constructor TJclSCManager.Create(const AMachineName: string;
  const ADesiredAccess: DWORD; const ADatabaseName: string);
begin
  Assert((ADesiredAccess and (not SC_MANAGER_ALL_ACCESS)) = 0);
  inherited Create;
  FMachineName := AMachineName;
  FDatabaseName := ADatabaseName;
  FDesiredAccess := ADesiredAccess;
  FHandle := INVALID_SCM_HANDLE;
  FServices := TObjectList.Create;
  FGroups := TObjectList.Create;
  FOrderType := sotServiceName;
  FOrderAsc := True;
  FAdvApi32Handle := INVALID_MODULEHANDLE_VALUE;
  FQueryServiceConfig2A := nil;
end;

destructor TJclSCManager.Destroy;
begin
  FreeAndNil(FGroups);
  FreeAndNil(FServices);
  Close;
  UnloadModule(FAdvApi32Handle);
  inherited Destroy;
end;

function TJclSCManager.AddService(const AService: TJclNtService): Integer;
begin
  Result := FServices.Add(AService);
end;

function TJclSCManager.GetService(const Idx: Integer): TJclNtService;
begin
  Result := TJclNtService(FServices.Items[Idx]);
end;

function TJclSCManager.GetServiceCount: Integer;
begin
  Result := FServices.Count;
end;

function TJclSCManager.AddGroup(const AGroup: TJclServiceGroup): Integer;
begin
  Result := FGroups.Add(AGroup);
end;

function TJclSCManager.GetGroup(const Idx: Integer): TJclServiceGroup;
begin
  Result := TJclServiceGroup(FGroups.Items[Idx]);
end;

function TJclSCManager.GetGroupCount: Integer;
begin
  Result := FGroups.Count;
end;

procedure TJclSCManager.SetOrderAsc(const Value: Boolean);
begin
  if FOrderAsc <> Value then
    Sort(OrderType, Value);
end;

procedure TJclSCManager.SetOrderType(const Value: TJclServiceSortOrderType);
begin
  if FOrderType <> Value then
    Sort(Value, FOrderAsc);
end;

function TJclSCManager.GetActive: Boolean;
begin
  Result := FHandle <> INVALID_SCM_HANDLE;
end;

procedure TJclSCManager.SetActive(const Value: Boolean);
begin
  if Value <> GetActive then
  begin
    if Value then
      Open
    else
      Close;
    Assert(Value = GetActive);
  end;
end;

procedure TJclSCManager.Open;
begin
  if not Active then
  begin
    FHandle := OpenSCManager(Pointer(FMachineName), Pointer(FDatabaseName), FDesiredAccess);
    Win32Check(FHandle <> INVALID_SCM_HANDLE);
  end;
end;

procedure TJclSCManager.Close;
begin
  if Active then
    Win32Check(CloseServiceHandle(FHandle));
  FHandle := INVALID_SCM_HANDLE;
end;

procedure TJclSCManager.Lock;
begin
  Assert((DesiredAccess and SC_MANAGER_LOCK) <> 0);
  Active := True;
  FLock := LockServiceDatabase(FHandle);
  Win32Check(FLock <> nil);
end;

procedure TJclSCManager.Unlock;
begin
  Assert(Active);
  Assert((DesiredAccess and SC_MANAGER_LOCK) <> 0);
  Assert(FLock <> nil);
  Win32Check(UnlockServiceDatabase(FLock));
end;

procedure TJclSCManager.Clear;
begin
  FServices.Clear;
  FGroups.Clear;
end;

procedure TJclSCManager.Refresh(const RefreshAll: Boolean);

  procedure EnumServices;
  var
    I: Integer;
    Ret: BOOL;
    PBuf: Pointer;
    PEss: PEnumServiceStatus;
    NtSvc: TJclNtService;
    BytesNeeded, ServicesReturned, ResumeHandle: DWORD;
  begin
    Assert((DesiredAccess and SC_MANAGER_ENUMERATE_SERVICE) <> 0);
    // Enum the services
    ResumeHandle := 0; // Must set this value to zero !!!
    try
      PBuf := nil;
      BytesNeeded := 40960;
      repeat
        ReallocMem(PBuf, BytesNeeded);
        Ret := EnumServicesStatus(FHandle, SERVICE_TYPE_ALL, SERVICE_STATE_ALL,
          PEnumServiceStatus(PBuf){$IFNDEF FPC}^{$ENDIF},
          BytesNeeded, BytesNeeded, ServicesReturned, ResumeHandle);
      until Ret or (GetLastError <> ERROR_MORE_DATA);
      Win32Check(Ret);

      PEss := PBuf;
      for I := 0 to ServicesReturned - 1 do
      begin
        NtSvc := TJclNtService.Create(Self, PEss^);
        try
          NtSvc.Refresh;
        except
          // trap invalid services
        end;
        Inc(PEss);
      end;
    finally
      FreeMem(PBuf);
    end;
  end;

  { TODO : Delete after Test }
  {procedure EnumServiceGroups;
  const
    cKeyServiceGroupOrder = 'SYSTEM\CurrentControlSet\Control\ServiceGroupOrder';
    cValList              = 'List';
  var
    Buf: array of Char;
    P: PChar;
    DataSize: DWORD;
  begin
    // Get the service groups
    DataSize := RegReadBinary(HKEY_LOCAL_MACHINE, cKeyServiceGroupOrder, cValList, PChar(nil)^, 0);
    SetLength(Buf, DataSize);
    if DataSize > 0 then
    begin
      DataSize := RegReadBinary(HKEY_LOCAL_MACHINE, cKeyServiceGroupOrder, cValList, Buf[0], DataSize);

      P := @Buf[0];
      while P^ <> #0 do
      begin
        AddGroup(TJclServiceGroup.Create(Self, P, GetGroupCount));
        Inc(P, StrLen(P) + 1);
      end;
    end;
  end;}

  { TODO -cTest : Test, if OK delete function above }
  { TODO -cHelp : }
  procedure EnumServiceGroups;
  const
    cKeyServiceGroupOrder = 'SYSTEM\CurrentControlSet\Control\ServiceGroupOrder';
    cValList = 'List';
  var
    List: TStringList;
    I: Integer;
  begin
    // Get the service groups
    List := TStringList.Create;
    try
      RegReadMultiSz(HKEY_LOCAL_MACHINE, cKeyServiceGroupOrder, cValList, List);
      for I := 0 to List.Count - 1 do
        AddGroup(TJclServiceGroup.Create(Self, List[I], GetGroupCount));
    finally
      List.Free;
    end;
  end;

  procedure RefreshAllServices;
  var
    I: Integer;
  begin
    for I := 0 to GetServiceCount - 1 do
    try
      GetService(I).Refresh;
    except
      // trap invalid services
    end;
  end;

begin
  Active := True;
  if RefreshAll then
  begin
    Clear;
    EnumServiceGroups;
    EnumServices;
  end;
  RefreshAllServices;
end;

function ServiceSortFunc(Item1, Item2: Pointer): Integer;
var
  Svc1, Svc2: TJclNtService;
begin
  Svc1 := Item1;
  Svc2 := Item2;
  case Svc1.SCManager.FOrderType of
    sotServiceName:
      Result := AnsiCompareStr(Svc1.ServiceName, Svc2.ServiceName);
    sotDisplayName:
      Result := AnsiCompareStr(Svc1.DisplayName, Svc2.DisplayName);
    sotDescription:
      Result := AnsiCompareStr(Svc1.Description, Svc2.Description);
    sotFileName:
      Result := AnsiCompareStr(Svc1.FileName, Svc2.FileName);
    sotServiceState:
      Result := Integer(Svc1.ServiceState) - Integer(Svc2.ServiceState);
    sotStartType:
      Result := Integer(Svc1.StartType) - Integer(Svc2.StartType);
    sotErrorControlType:
      Result := Integer(Svc1.ErrorControlType) - Integer(Svc2.ErrorControlType);
    sotLoadOrderGroup:
      Result := Svc1.Group.Order - Svc2.Group.Order;
    sotWin32ExitCode:
      Result := Svc1.Win32ExitCode - Svc2.Win32ExitCode;
  else
    Result := 0;
  end;
  if not Svc1.SCManager.FOrderAsc then
    Result := -Result;
end;

procedure TJclSCManager.Sort(const AOrderType: TJclServiceSortOrderType; const AOrderAsc: Boolean);
begin
  FOrderType := AOrderType;
  FOrderAsc := AOrderAsc;
  FServices.Sort(ServiceSortFunc);
end;

function TJclSCManager.FindService(const SvcName: string; var NtSvc: TJclNtService): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to GetServiceCount - 1 do
  begin
    NtSvc := GetService(I);
    if CompareText(NtSvc.ServiceName, SvcName) = 0 then
    begin
      Result := True;
      Exit;
    end;
  end;
  NtSvc := nil;
end;

function TJclSCManager.FindGroup(const GrpName: string; var SvcGrp: TJclServiceGroup;
  const AutoAdd: Boolean): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to GetGroupCount - 1 do
  begin
    if CompareText(GetGroup(I).Name, GrpName) = 0 then
    begin
      SvcGrp := GetGroup(I);
      Result := True;
      Exit;
    end;
  end;
  if AutoAdd then
  begin
    SvcGrp := TJclServiceGroup.Create(Self, GrpName, GetGroupCount);
    AddGroup(SvcGrp);
  end
  else
    SvcGrp := nil;
end;

function TJclSCManager.GetServiceLockStatus: PQueryServiceLockStatus;
var
  Ret: BOOL;
  BytesNeeded: DWORD;
begin
  Assert((DesiredAccess and SC_MANAGER_QUERY_LOCK_STATUS) <> 0);
  Active := True;

  try
    Result := nil;
    BytesNeeded := 10240;
    repeat
      ReallocMem(Result, BytesNeeded);
      Ret := QueryServiceLockStatus(FHandle, Result{$IFNDEF FPC}^{$ENDIF FPC}, BytesNeeded, BytesNeeded);
    until Ret or (GetLastError <> ERROR_INSUFFICIENT_BUFFER);
    Win32Check(Ret);
  except
    FreeMem(Result);
    raise;
  end;
end;

function TJclSCManager.IsLocked: Boolean;
var
  PQsls: PQueryServiceLockStatus;
begin
  PQsls := GetServiceLockStatus;
  Result := Assigned(PQsls) and (PQsls.fIsLocked <> 0);
  FreeMem(PQsls);
end;

function TJclSCManager.LockOwner: string;
var
  PQsls: PQueryServiceLockStatus;
begin
  PQsls := GetServiceLockStatus;
  if Assigned(PQsls) then
    Result := PQsls.lpLockOwner
  else
    Result := '';
  FreeMem(PQsls);
end;

function TJclSCManager.LockDuration: DWORD;
var
  PQsls: PQueryServiceLockStatus;
begin
  PQsls := GetServiceLockStatus;
  if Assigned(PQsls) then
    Result := PQsls.dwLockDuration
  else
    Result := INFINITE;
  FreeMem(PQsls);
end;

function TJclSCManager.GetAdvApi32Handle: TModuleHandle;
const
  cAdvApi32 = 'advapi32.dll'; // don't localize
begin
  if FAdvApi32Handle = INVALID_MODULEHANDLE_VALUE then
    LoadModule(FAdvApi32Handle, cAdvApi32);
  Result := FAdvApi32Handle;
end;

{ TODO : Standard Rtdl }
function TJclSCManager.GetQueryServiceConfig2A: TQueryServiceConfig2A;
const
  cQueryServiceConfig2 = 'QueryServiceConfig2A'; // don't localize
begin
  // Win2K or later
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 5) then
    FQueryServiceConfig2A := GetModuleSymbol(AdvApi32Handle, cQueryServiceConfig2);

  Result := FQueryServiceConfig2A;
end;

function TJclSCManager.Install(const ServiceName, DisplayName, ImageName, Description: string;
  ServiceTypes: TJclServiceTypes; StartType: TJclServiceStartType;
  ErrorControlType: TJclServiceErrorControlType; DesiredAccess: DWORD;
  const LoadOrderGroup: TJclServiceGroup;
  const Dependencies, Account, Password: PChar): TJclNtService;
var
  LoadOrderGroupName: string;
  LoadOrderGroupNamePtr: PChar;
  EnumServiceStatus: TEnumServiceStatus;
  Svc: THandle;
begin
  if Assigned(LoadOrderGroup) then
  begin
    LoadOrderGroupName := LoadOrderGroup.Name;
    LoadOrderGroupNamePtr := PChar(LoadOrderGroupName);
  end
  else
  begin
    LoadOrderGroupName := '';
    LoadOrderGroupNamePtr := nil;
  end;

  Svc := CreateService(FHandle, PChar(ServiceName), PChar(DisplayName),
    DesiredAccess, TJclSCManager.ServiceType(ServiceTypes), DWORD(StartType),
    DWORD(ErrorControlType), PChar(ImageName), LoadOrderGroupNamePtr, nil,
    Dependencies, Account, Password);
  if Svc = 0 then
    RaiseLastOsError;
  CloseServiceHandle(Svc);

  if (Description <> '') and (IsWin2K or IsWinXP) then
    RegWriteString(HKEY_LOCAL_MACHINE, '\' + REGSTR_PATH_SERVICES + '\' + ServiceName,
      'Description', Description);

  EnumServiceStatus.lpServiceName := PChar(ServiceName);
  EnumServiceStatus.lpDisplayName := PChar(DisplayName);

  Result := TJclNtService.Create(Self, EnumServiceStatus);
  Result.Refresh;
end;

class function TJclSCManager.ServiceType(const SvcType: TJclServiceTypes): DWORD;
var
  AType: TJclServiceType;
begin
  Result := 0;
  for AType := Low(TJclServiceType) to High(TJclServiceType) do
    if AType in SvcType then
      Result := Result or ServiceTypeMapping[AType];
end;

class function TJclSCManager.ServiceType(const SvcType: DWORD): TJclServiceTypes;
var
  AType: TJclServiceType;
begin
  Result := [];
  for AType := Low(TJclServiceType) to High(TJclServiceType) do
    if (SvcType and ServiceTypeMapping[AType]) <> 0 then
      Include(Result, AType);
end;

class function TJclSCManager.ControlAccepted(const CtrlAccepted: TJclServiceControlAccepteds): DWORD;
var
  ACtrl: TJclServiceControlAccepted;
begin
  Result := 0;
  for ACtrl := Low(TJclServiceControlAccepted) to High(TJclServiceControlAccepted) do
    if ACtrl in CtrlAccepted then
      Result := Result or ServiceControlAcceptedMapping[ACtrl];
end;

class function TJclSCManager.ControlAccepted(const CtrlAccepted: DWORD): TJclServiceControlAccepteds;
var
  ACtrl: TJclServiceControlAccepted;
begin
  Result := [];
  for ACtrl := Low(TJclServiceControlAccepted) to High(TJclServiceControlAccepted) do
    if (CtrlAccepted and ServiceControlAcceptedMapping[ACtrl]) <> 0 then
      Include(Result, ACtrl);
end;

function GetServiceStatusByName(const AServer,AServiceName:string):TJclServiceState;
var
  ServiceHandle,
  SCMHandle: DWORD;
  SCMAccess,Access:DWORD;
  ServiceStatus: TServiceStatus;
begin
  Result:=ssUnknown;

  SCMAccess:=SC_MANAGER_CONNECT or SC_MANAGER_ENUMERATE_SERVICE or SC_MANAGER_QUERY_LOCK_STATUS;
  Access:=SERVICE_INTERROGATE or GENERIC_READ;

  SCMHandle:= OpenSCManager(PChar(AServer), Nil, SCMAccess);
  if SCMHandle <> 0 then
  try
    ServiceHandle:=OpenService(SCMHandle,PChar(AServiceName),Access);
    if ServiceHandle <> 0 then
    try
      if QueryServiceStatus(ServiceHandle,ServiceStatus) then
        Result:=TJclServiceState(ServiceStatus.dwCurrentState);
    finally
      CloseServiceHandle(ServiceHandle);
    end;
  finally
    CloseServiceHandle(SCMHandle);
  end;
end;

function StartServiceByName(const AServer,AServiceName: String):Boolean;
var
  ServiceHandle,
  SCMHandle: DWORD;
  p: PChar;
begin
  p:=nil;
  Result:=False;

  SCMHandle:= OpenSCManager(PChar(AServer), nil, SC_MANAGER_ALL_ACCESS);
  if SCMHandle <> 0 then
  try
    ServiceHandle:=OpenService(SCMHandle,PChar(AServiceName),SERVICE_ALL_ACCESS);
    if ServiceHandle <> 0 then
      Result:=StartService(ServiceHandle,0,p);

    CloseServiceHandle(ServiceHandle);
  finally
    CloseServiceHandle(SCMHandle);
  end;
end;

function StopServiceByName(const AServer, AServiceName: String):Boolean;
var
  ServiceHandle,
  SCMHandle: DWORD;
  SS: _Service_Status;
begin
  Result:=False;

  SCMHandle:= OpenSCManager(PChar(AServer), nil, SC_MANAGER_ALL_ACCESS);
  if SCMHandle <> 0 then
  try
    ServiceHandle:=OpenService(SCMHandle,PChar(AServiceName),SERVICE_ALL_ACCESS);
    if ServiceHandle <> 0 then
      Result:=ControlService(ServiceHandle,SERVICE_CONTROL_STOP,SS);

    CloseServiceHandle(ServiceHandle);
  finally
    CloseServiceHandle(SCMHandle);
  end;
end;

function GetServiceStatus(ServiceHandle: SC_HANDLE): DWORD;
var
  ServiceStatus: TServiceStatus;
begin
  if not QueryServiceStatus(ServiceHandle, ServiceStatus) then
    RaiseLastOSError;

  Result := ServiceStatus.dwCurrentState;
end;

function GetServiceStatusWaitingIfPending(ServiceHandle: SC_HANDLE): DWORD;
var
  ServiceStatus: TServiceStatus;
  WaitDuration: DWORD;
  LastCheckPoint: DWORD;
begin
  if not QueryServiceStatus(ServiceHandle, ServiceStatus) then
    RaiseLastOSError;

  Result := ServiceStatus.dwCurrentState;

  while TJclServiceState(Result) in ssPendingStates do
  begin
    LastCheckPoint := ServiceStatus.dwCheckPoint;

    // Multiple operations might alter the expected wait duration, so check inside the loop
    WaitDuration := ServiceStatus.dwWaitHint;
    if WaitDuration < 1000 then
      WaitDuration := 1000
    else
    if WaitDuration > 10000 then
      WaitDuration := 10000;

    Sleep(WaitDuration);

    // Get the new status
    if not QueryServiceStatus(ServiceHandle, ServiceStatus) then
      RaiseLastOSError;

    Result := ServiceStatus.dwCurrentState;

    if ServiceStatus.dwCheckPoint = LastCheckPoint then  // No progress made
      Break;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
