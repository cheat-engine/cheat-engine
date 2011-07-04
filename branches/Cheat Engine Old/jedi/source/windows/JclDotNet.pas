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
{ The Original Code is JclDotNet.pas.                                                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is Flier Lu (<flier_lu att yahoo dott com dott cn>).  }
{ Portions created by Flier Lu are Copyright (C) Flier Lu. All Rights Reserved.                    }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Flier Lu (flier)                                                                               }
{   Robert Marquardt (marquardt)                                                                   }
{   Olivier Sannier (obones)                                                                       }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Microsoft .Net framework support routines and classes.                                           }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-02-03 21:54:20 +0100 (dim., 03 févr. 2008)                        $ }
{ Revision:      $Rev:: 2343                                                                     $ }
{ Author:        $Author:: marcovtje                                                             $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclDotNet;

{**************************************************************************************************}
{ Read this before compile!                                                                        }
{**************************************************************************************************}
{ 1. This unit is developed in Delphi6 with MS.Net v1.0.3705,                                      }
{    you maybe need to modify it for your environment.                                             }
{ 2. Delphi's TLibImp.exe would generate error *_TLB.pas files                                     }
{    when you import mscorlib.tlb, you should modify it by hand                                    }
{    for example, change Pointer to _Pointer...                                                    }
{    or use my modified edition of mscorlib_TLB.pas (mscor.zip)                                    }
{**************************************************************************************************}

interface

{$I jcl.inc}

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows, ActiveX,
  {$ENDIF MSWINDOWS}
  Classes, SysUtils,
  {$IFDEF HAS_UNIT_CONTNRS}
  Contnrs,
  {$ENDIF HAS_UNIT_CONTNRS}
  JclBase, JclWideStrings,
  mscoree_TLB, mscorlib_TLB;

//{$HPPEMIT '#include<Mscoree.h>'}

{ TODO -cDOC : Original code: "Flier Lu" <flier_lu att yahoo dott com dott cn> }

type
  TJclClrBase = TInterfacedObject;

type
  IJclClrAppDomain = mscorlib_TLB._AppDomain;
  IJclClrEvidence  = mscorlib_TLB._Evidence;
  IJclClrAssembly  = mscorlib_TLB._Assembly;
  IJclClrMethod    = mscorlib_TLB._MethodInfo;

type
  TJclClrHostFlavor = (hfServer, hfWorkStation);

  TJclClrHostLoaderFlag =
   (hlOptSingleDomain,
    hlOptMultiDomain,
    hlOptMultiDomainHost,
    hlSafeMode,
    hlSetPreference);
  TJclClrHostLoaderFlags = set of TJclClrHostLoaderFlag;

type
  EJclClrException = class(SysUtils.Exception);
  
  TJclClrAppDomain = class;
  TJclClrAppDomainSetup = class;
  TJclClrAssembly = class;

  TJclClrHost = class(TJclClrBase, ICorRuntimeHost)
  private
    FDefaultInterface: ICorRuntimeHost;
    FAppDomains: TObjectList;
    procedure EnumAppDomains;
    function GetAppDomain(const Idx: Integer): TJclClrAppDomain;
    function GetAppDomainCount: Integer;
    function GetDefaultAppDomain: IJclClrAppDomain;
    function GetCurrentAppDomain: IJclClrAppDomain;
  protected
    function AddAppDomain(const AppDomain: TJclClrAppDomain): Integer;
    function RemoveAppDomain(const AppDomain: TJclClrAppDomain): Integer; 
  public
    constructor Create(const ClrVer: WideString = '';
      const Flavor: TJclClrHostFlavor = hfWorkStation;
      const ConcurrentGC: Boolean = True;
      const LoaderFlags: TJclClrHostLoaderFlags = [hlOptSingleDomain]);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure Refresh;
    function CreateDomainSetup: TJclClrAppDomainSetup;
    function CreateAppDomain(const Name: WideString;
      const Setup: TJclClrAppDomainSetup = nil;
      const Evidence: IJclClrEvidence = nil): TJclClrAppDomain;
    function FindAppDomain(const Intf: IJclClrAppDomain; var Ret: TJclClrAppDomain): Boolean; overload;
    function FindAppDomain(const Name: WideString; var Ret: TJclClrAppDomain): Boolean; overload;
    class function CorSystemDirectory: WideString;
    class function CorVersion: WideString;
    class function CorRequiredVersion: WideString;
    class procedure GetClrVersions(VersionNames: TWideStrings); overload;
    class procedure GetClrVersions(VersionNames: TStrings); overload;
    property DefaultInterface: ICorRuntimeHost read FDefaultInterface implements ICorRuntimeHost;
    property AppDomains[const Idx: Integer]: TJclClrAppDomain read GetAppDomain; default;
    property AppDomainCount: Integer read GetAppDomainCount;
    property DefaultAppDomain: IJclClrAppDomain read GetDefaultAppDomain;
    property CurrentAppDomain: IJclClrAppDomain read GetCurrentAppDomain;
  end;

  TJclClrAssemblyArguments = array of WideString;

  TJclClrAppDomain = class(TJclClrBase, IJclClrAppDomain)
  private
    FHost: TJclClrHost;
    FDefaultInterface: IJclClrAppDomain;
  protected
    constructor Create(const AHost: TJclClrHost; const AAppDomain: IJclClrAppDomain);
  public
    function Load(const AssemblyString: WideString;
      const AssemblySecurity: IJclClrEvidence = nil): TJclClrAssembly; overload;
    function Load(const RawAssemblyStream: TStream;
      const RawSymbolStoreStream: TStream = nil;
      const AssemblySecurity: IJclClrEvidence = nil): TJclClrAssembly; overload;
    function Execute(const AssemblyFile: TFileName;
      const AssemblySecurity: IJclClrEvidence = nil): Integer; overload;
    function Execute(const AssemblyFile: TFileName;
      const Arguments: TJclClrAssemblyArguments;
      const AssemblySecurity: IJclClrEvidence = nil): Integer; overload;
    function Execute(const AssemblyFile: TFileName;
      const Arguments: TStrings;
      const AssemblySecurity: IJclClrEvidence = nil): Integer; overload;
    procedure Unload;
    property Host: TJclClrHost read FHost;
    property DefaultInterface: IJclClrAppDomain read FDefaultInterface implements IJclClrAppDomain;
  end;

  TJclClrAppDomainSetup = class(TJclClrBase, IAppDomainSetup)
  private
    FDefaultInterface: IAppDomainSetup;
    function GetApplicationBase: WideString;
    function GetApplicationName: WideString;
    function GetCachePath: WideString;
    function GetConfigurationFile: WideString;
    function GetDynamicBase: WideString;
    function GetLicenseFile: WideString;
    function GetPrivateBinPath: WideString;
    function GetPrivateBinPathProbe: WideString;
    function GetShadowCopyDirectories: WideString;
    function GetShadowCopyFiles: WideString;
    procedure SetApplicationBase(const Value: WideString);
    procedure SetApplicationName(const Value: WideString);
    procedure SetCachePath(const Value: WideString);
    procedure SetConfigurationFile(const Value: WideString);
    procedure SetDynamicBase(const Value: WideString);
    procedure SetLicenseFile(const Value: WideString);
    procedure SetPrivateBinPath(const Value: WideString);
    procedure SetPrivateBinPathProbe(const Value: WideString);
    procedure SetShadowCopyDirectories(const Value: WideString);
    procedure SetShadowCopyFiles(const Value: WideString);
  protected
    constructor Create(Intf: IAppDomainSetup);
  public
    property DefaultInterface: IAppDomainSetup read FDefaultInterface implements IAppDomainSetup;
    property ApplicationBase: WideString read GetApplicationBase write SetApplicationBase;
    property ApplicationName: WideString read GetApplicationName write SetApplicationName;
    property CachePath: WideString read GetCachePath write SetCachePath;
    property ConfigurationFile: WideString read GetConfigurationFile write SetConfigurationFile;
    property DynamicBase: WideString read GetDynamicBase write SetDynamicBase;
    property LicenseFile: WideString read GetLicenseFile write SetLicenseFile;
    property PrivateBinPath: WideString read GetPrivateBinPath write SetPrivateBinPath;
    property PrivateBinPathProbe: WideString read GetPrivateBinPathProbe write SetPrivateBinPathProbe;
    property ShadowCopyDirectories: WideString read GetShadowCopyDirectories write SetShadowCopyDirectories;
    property ShadowCopyFiles: WideString read GetShadowCopyFiles write SetShadowCopyFiles;
  end;

  TJclClrAssembly = class(TJclClrBase, IJclClrAssembly)
  private
    FDefaultInterface: IJclClrAssembly;
  protected
    constructor Create(Intf: IJclClrAssembly);
  public
    property DefaultInterface: IJclClrAssembly read FDefaultInterface implements IJclClrAssembly;
  end;

type
  TJclClrField = class(TObject)
  end;

  TJclClrProperty = class(TObject)
  end;

  TJclClrMethod = class(TJclClrBase, IJclClrMethod)
  private
    FDefaultInterface: IJclClrMethod;
  public
    property DefaultInterface: IJclClrMethod read FDefaultInterface implements IJclClrMethod;
  end;

  TJclClrObject = class(TObject)
  private
    function GetMethod(const Name: WideString): TJclClrMethod;
    function GetField(const Name: WideString): TJclClrField;
    function GetProperty(const Name: WideString): TJclClrProperty;
  protected
    constructor Create(const AssemblyName, NamespaceName, ClassName: WideString;
      const Parameters: array of const); overload;
    constructor Create(const AssemblyName, NamespaceName, ClassName: WideString;
      const NewInstance: Boolean = False); overload;
  public
    property Fields[const Name: WideString]: TJclClrField read GetField;
    property Properties[const Name: WideString]: TJclClrProperty read GetProperty;
    property Methods[const Name: WideString]: TJclClrMethod read GetMethod;
  end;

function CompareCLRVersions(const LeftVersion, RightVersion: string): Integer;

type
  HDOMAINENUM = Pointer;
  {$EXTERNALSYM HDOMAINENUM}

const
  STARTUP_CONCURRENT_GC                         = $1;
  STARTUP_LOADER_OPTIMIZATION_MASK              = $3 shl 1;
  STARTUP_LOADER_OPTIMIZATION_SINGLE_DOMAIN     = $1 shl 1;
  STARTUP_LOADER_OPTIMIZATION_MULTI_DOMAIN      = $2 shl 1;
  STARTUP_LOADER_OPTIMIZATION_MULTI_DOMAIN_HOST = $3 shl 1;
  STARTUP_LOADER_SAFEMODE                       = $10;
  STARTUP_LOADER_SETPREFERENCE                  = $100;

  RUNTIME_INFO_UPGRADE_VERSION         = $01;
  RUNTIME_INFO_REQUEST_IA64            = $02;
  RUNTIME_INFO_REQUEST_AMD64           = $04;
  RUNTIME_INFO_REQUEST_X86             = $08;
  RUNTIME_INFO_DONT_RETURN_DIRECTORY   = $10;
  RUNTIME_INFO_DONT_RETURN_VERSION     = $20;
  RUNTIME_INFO_DONT_SHOW_ERROR_DIALOG  = $40;

function GetCORSystemDirectory(pbuffer: PWideChar; const cchBuffer: DWORD;
  var dwLength: DWORD): HRESULT; stdcall;
{$EXTERNALSYM GetCORSystemDirectory}
function GetCORVersion(pbuffer: PWideChar; const cchBuffer: DWORD;
  var dwLength: DWORD): HRESULT; stdcall;
{$EXTERNALSYM GetCORVersion}
function GetFileVersion(szFileName, szBuffer: PWideChar; const cchBuffer: DWORD;
  var dwLength: DWORD): HRESULT; stdcall;
{$EXTERNALSYM GetFileVersion}
function GetCORRequiredVersion(pbuffer: PWideChar; const cchBuffer: DWORD;
  var dwLength: DWORD): HRESULT; stdcall;
{$EXTERNALSYM GetCORRequiredVersion}
function GetRequestedRuntimeInfo(pExe, pwszVersion, pConfigurationFile: PWideChar;
  const startupFlags, reserved: DWORD; pDirectory: PWideChar; const dwDirectory: DWORD;
  var dwDirectoryLength: DWORD; pVersion: PWideChar; const cchBuffer: DWORD;
  var dwLength: DWORD): HRESULT; stdcall;
{$EXTERNALSYM GetRequestedRuntimeInfo}
function GetRequestedRuntimeVersion(pExe, pVersion: PWideChar;
  const cchBuffer: DWORD; var dwLength: DWORD): HRESULT; stdcall;
{$EXTERNALSYM GetRequestedRuntimeVersion}
function CorBindToRuntimeHost(pwszVersion, pwszBuildFlavor,
  pwszHostConfigFile: PWideChar; const pReserved: Pointer;
  const startupFlags: DWORD; const rclsid: TCLSID; const riid: TIID;
  out pv): HRESULT; stdcall;
{$EXTERNALSYM CorBindToRuntimeHost}
function CorBindToRuntimeEx(pwszVersion, pwszBuildFlavor: PWideChar;
  startupFlags: DWORD; const rclsid: TCLSID; const riid: TIID;
  out pv): HRESULT; stdcall;
{$EXTERNALSYM CorBindToRuntimeEx}
function CorBindToRuntimeByCfg(const pCfgStream: IStream;
  const reserved, startupFlags: DWORD; const rclsid: TCLSID;
  const riid: TIID; out pv): HRESULT; stdcall;
{$EXTERNALSYM CorBindToRuntimeByCfg}
function CorBindToRuntime(pwszVersion, pwszBuildFlavor: PWideChar;
  const rclsid: TCLSID; const riid: TIID; out pv): HRESULT; stdcall;
{$EXTERNALSYM CorBindToRuntime}
function CorBindToCurrentRuntime(pwszFileName: PWideChar;
  const rclsid: TCLSID; const riid: TIID; out pv): HRESULT; stdcall;
{$EXTERNALSYM CorBindToCurrentRuntime}
function ClrCreateManagedInstance(pTypeName: PWideChar;
  const riid: TIID; out pv): HRESULT; stdcall;
{$EXTERNALSYM ClrCreateManagedInstance}
procedure CorMarkThreadInThreadPool; stdcall;
{$EXTERNALSYM CorMarkThreadInThreadPool}
function RunDll32ShimW(const hwnd: THandle; const hinst: HMODULE;
  lpszCmdLine: PWideChar; const nCmdShow: Integer): HRESULT; stdcall;
{$EXTERNALSYM RunDll32ShimW}
function LoadLibraryShim(szDllName, szVersion: PWideChar;
  const pvReserved: Pointer; out phModDll: HMODULE): HRESULT; stdcall;
{$EXTERNALSYM LoadLibraryShim}
function CallFunctionShim(szDllName: PWideChar; const szFunctionName: PChar;
  const lpvArgument1, lpvArgument2: Pointer; szVersion: PWideChar;
  const pvReserved: Pointer): HRESULT; stdcall;
{$EXTERNALSYM CallFunctionShim}
function GetRealProcAddress(const pwszProcName: PChar;
  out ppv: Pointer): HRESULT; stdcall;
{$EXTERNALSYM GetRealProcAddress}
procedure CorExitProcess(const exitCode: Integer); stdcall;
{$EXTERNALSYM CorExitProcess}

type
  CLSID_RESOLUTION_FLAGS = type Byte;
  {$EXTERNALSYM CLSID_RESOLUTION_FLAGS}

const
  CLSID_RESOLUTION_DEFAULT	  = $0;
  {$EXTERNALSYM CLSID_RESOLUTION_DEFAULT}
	CLSID_RESOLUTION_REGISTERED	= $1;
  {$EXTERNALSYM CLSID_RESOLUTION_REGISTERED}

function GetRequestedRuntimeVersionForCLSID(rclsid: TGuid; pVersion: PWideChar;
  const cchBuffer: DWORD; var dwLength: DWORD;
  const dwResolutionFlags: CLSID_RESOLUTION_FLAGS): HRESULT; stdcall;
{$EXTERNALSYM GetRequestedRuntimeVersionForCLSID}

const
  mscoree_dll = 'mscoree.dll';

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/windows/JclDotNet.pas $';
    Revision: '$Revision: 2343 $';
    Date: '$Date: 2008-02-03 21:54:20 +0100 (dim., 03 févr. 2008) $';
    LogPath: 'JCL\source\windows'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  ComObj,
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  JclSysUtils, JclResources, JclStrings;

function CompareCLRVersions(const LeftVersion, RightVersion: string): Integer;
var
  LeftMajor, RightMajor, LeftMinor, RightMinor, LeftBuild, RightBuild, DotPos: Integer;
  LeftStr, RightStr, LeftNum, RightNum: string;
begin
  if (Length(LeftVersion) = 0) or (LeftVersion[1] <> 'v') then
    raise EJclClrException.CreateResFmt(@RsEUnknownCLRVersion, [LeftVersion]);

  if (Length(RightVersion) = 0) or (RightVersion[1] <> 'v') then
    raise EJclClrException.CreateResFmt(@RsEUnknownCLRVersion, [RightVersion]);

  DotPos := Pos('.', LeftVersion);
  if DotPos = 0 then
    raise EJclClrException.CreateResFmt(@RsEUnknownCLRVersion, [LeftVersion]);
  LeftNum := Copy(LeftVersion, 2, DotPos - 2);
  LeftStr := Copy(LeftVersion, DotPos + 1, Length(LeftVersion) - DotPos);
  if not TryStrToInt(LeftNum, LeftMajor) then
    raise EJclClrException.CreateResFmt(@RsEUnknownCLRVersion, [LeftVersion]);

  DotPos := Pos('.', RightVersion);
  if DotPos = 0 then
    raise EJclClrException.CreateResFmt(@RsEUnknownCLRVersion, [RightVersion]);
  RightNum := Copy(RightVersion, 2, DotPos - 2);
  RightStr := Copy(RightVersion, DotPos + 1, Length(RightVersion) - DotPos);
  if not TryStrToInt(RightNum, RightMajor) then
    raise EJclClrException.CreateResFmt(@RsEUnknownCLRVersion, [RightVersion]);

  Result := -1;
  if LeftMajor < RightMajor then
    Exit;
  Result := 1;
  if LeftMajor > RightMajor then
    Exit;

  DotPos := Pos('.', LeftStr);
  if DotPos = 0 then
    raise EJclClrException.CreateResFmt(@RsEUnknownCLRVersion, [LeftVersion]);
  LeftNum := Copy(LeftStr, 1, DotPos - 1);
  LeftStr := Copy(LeftStr, DotPos + 1, Length(LeftStr) - DotPos);
  if not TryStrToInt(LeftNum, LeftMinor) then
    raise EJclClrException.CreateResFmt(@RsEUnknownCLRVersion, [LeftVersion]);

  DotPos := Pos('.', RightStr);
  if DotPos = 0 then
    raise EJclClrException.CreateResFmt(@RsEUnknownCLRVersion, [RightVersion]);
  RightNum := Copy(RightStr, 1, DotPos - 1);
  RightStr := Copy(RightStr, DotPos + 1, Length(RightStr) - DotPos);
  if not TryStrToInt(RightNum, RightMinor) then
    raise EJclClrException.CreateResFmt(@RsEUnknownCLRVersion, [RightVersion]);

  Result := -1;
  if LeftMinor < RightMinor then
    Exit;
  Result := 1;
  if LeftMinor > RightMinor then
    Exit;

  if not TryStrToInt(LeftStr, LeftBuild) then
    raise EJclClrException.CreateResFmt(@RsEUnknownCLRVersion, [LeftVersion]);
  if not TryStrToInt(RightStr, RightBuild) then
    raise EJclClrException.CreateResFmt(@RsEUnknownCLRVersion, [RightVersion]);

  if LeftBuild < RightBuild then
    Result := -1
  else if LeftBuild > RightBuild then
    Result := 1
  else
    Result := 0;
end;

procedure GetProcedureAddress(var P: Pointer; const ModuleName, ProcName: string);
var
  ModuleHandle: HMODULE;
begin
  if not Assigned(P) then
  begin
    ModuleHandle := GetModuleHandle(PChar(ModuleName));
    if ModuleHandle = 0 then
    begin
      ModuleHandle := SafeLoadLibrary(ModuleName);
      if ModuleHandle = 0 then
        raise EJclError.CreateResFmt(@RsELibraryNotFound, [ModuleName]);
    end;
    P := GetProcAddress(ModuleHandle, PChar(ProcName));
    if not Assigned(P) then
      raise EJclError.CreateResFmt(@RsEFunctionNotFound, [ModuleName, ProcName]);
  end;
end;

{$WARNINGS OFF}

var
  _GetCORSystemDirectory: Pointer = nil;

function GetCORSystemDirectory;
begin
  GetProcedureAddress(_GetCORSystemDirectory, mscoree_dll, 'GetCORSystemDirectory');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCORSystemDirectory]
  end;
end;

var
  _GetCORVersion: Pointer = nil;

function GetCORVersion;
begin
  GetProcedureAddress(_GetCORVersion, mscoree_dll, 'GetCORVersion');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCORVersion]
  end;
end;

var
  _GetFileVersion: Pointer = nil;

function GetFileVersion;
begin
  GetProcedureAddress(_GetFileVersion, mscoree_dll, 'GetFileVersion');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFileVersion]
  end;
end;

var
  _GetCORRequiredVersion: Pointer = nil;

function GetCORRequiredVersion;
begin
  GetProcedureAddress(_GetCORRequiredVersion, mscoree_dll, 'GetCORRequiredVersion');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCORRequiredVersion]
  end;
end;

var
  _GetRequestedRuntimeInfo: Pointer = nil;

function GetRequestedRuntimeInfo;
begin
  GetProcedureAddress(_GetRequestedRuntimeInfo, mscoree_dll, 'GetRequestedRuntimeInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetRequestedRuntimeInfo]
  end;
end;

var
  _GetRequestedRuntimeVersion: Pointer = nil;

function GetRequestedRuntimeVersion;
begin
  GetProcedureAddress(_GetRequestedRuntimeVersion, mscoree_dll, 'GetRequestedRuntimeVersion');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetRequestedRuntimeVersion]
  end;
end;

var
  _CorBindToRuntimeHost: Pointer = nil;

function CorBindToRuntimeHost;
begin
  GetProcedureAddress(_CorBindToRuntimeHost, mscoree_dll, 'CorBindToRuntimeHost');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CorBindToRuntimeHost]
  end;
end;

var
  _CorBindToRuntimeEx: Pointer = nil;

function CorBindToRuntimeEx;
begin
  GetProcedureAddress(_CorBindToRuntimeEx, mscoree_dll, 'CorBindToRuntimeEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CorBindToRuntimeEx]
  end;
end;

var
  _CorBindToRuntimeByCfg: Pointer = nil;

function CorBindToRuntimeByCfg;
begin
  GetProcedureAddress(_CorBindToRuntimeByCfg, mscoree_dll, 'CorBindToRuntimeByCfg');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CorBindToRuntimeByCfg]
  end;
end;

var
  _CorBindToRuntime: Pointer = nil;

function CorBindToRuntime;
begin
  GetProcedureAddress(_CorBindToRuntime, mscoree_dll, 'CorBindToRuntime');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CorBindToRuntime]
  end;
end;

var
  _CorBindToCurrentRuntime: Pointer = nil;

function CorBindToCurrentRuntime;
begin
  GetProcedureAddress(_CorBindToCurrentRuntime, mscoree_dll, 'CorBindToCurrentRuntime');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CorBindToCurrentRuntime]
  end;
end;

var
  _ClrCreateManagedInstance: Pointer = nil;

function ClrCreateManagedInstance;
begin
  GetProcedureAddress(_ClrCreateManagedInstance, mscoree_dll, 'ClrCreateManagedInstance');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ClrCreateManagedInstance]
  end;
end;

var
  _CorMarkThreadInThreadPool: Pointer = nil;

procedure CorMarkThreadInThreadPool;
begin
  GetProcedureAddress(_CorMarkThreadInThreadPool, mscoree_dll, 'CorMarkThreadInThreadPool');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CorMarkThreadInThreadPool]
  end;
end;

var
  _RunDll32ShimW: Pointer = nil;

function RunDll32ShimW;
begin
  GetProcedureAddress(_RunDll32ShimW, mscoree_dll, 'RunDll32ShimW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RunDll32ShimW]
  end;
end;

var
  _LoadLibraryShim: Pointer = nil;

function LoadLibraryShim;
begin
  GetProcedureAddress(_LoadLibraryShim, mscoree_dll, 'LoadLibraryShim');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadLibraryShim]
  end;
end;

var
  _CallFunctionShim: Pointer = nil;

function CallFunctionShim;
begin
  GetProcedureAddress(_CallFunctionShim, mscoree_dll, 'CallFunctionShim');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CallFunctionShim]
  end;
end;

var
  _GetRealProcAddress: Pointer = nil;

function GetRealProcAddress;
begin
  GetProcedureAddress(_GetRealProcAddress, mscoree_dll, 'GetRealProcAddress');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetRealProcAddress]
  end;
end;

var
  _CorExitProcess: Pointer = nil;

procedure CorExitProcess;
begin
  GetProcedureAddress(_CorExitProcess, mscoree_dll, 'CorExitProcess');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CorExitProcess]
  end;
end;

// truncated because the symbol was not found in assembler
var
  _GetRequestedRuntimeVersionForCL: Pointer = nil;

function GetRequestedRuntimeVersionForCLSID;
begin
  GetProcedureAddress(_GetRequestedRuntimeVersionForCL, mscoree_dll, 'GetRequestedRuntimeVersionForCLSID');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetRequestedRuntimeVersionForCL]
  end;
end;

{$WARNINGS ON}

//=== { TJclClrHost } ========================================================

const
  CLR_MAJOR_VERSION = 1;
  CLR_MINOR_VERSION = 0;
  CLR_BUILD_VERSION = 3705;

constructor TJclClrHost.Create(const ClrVer: WideString; const Flavor: TJclClrHostFlavor;
  const ConcurrentGC: Boolean; const LoaderFlags: TJclClrHostLoaderFlags);
const
  ClrHostFlavorNames: array [TJclClrHostFlavor] of WideString = ('srv', 'wks');
  ClrHostLoaderFlagValues: array [TJclClrHostLoaderFlag] of DWORD =
   (STARTUP_LOADER_OPTIMIZATION_SINGLE_DOMAIN,
    STARTUP_LOADER_OPTIMIZATION_MULTI_DOMAIN,
    STARTUP_LOADER_OPTIMIZATION_MULTI_DOMAIN_HOST,
    STARTUP_LOADER_SAFEMODE,
    STARTUP_LOADER_SETPREFERENCE);
var
  Flags: DWORD;
  ALoaderFlag: TJclClrHostLoaderFlag;
begin
  inherited Create;
  Flags := 0;
  if ConcurrentGC then
    Flags := Flags or STARTUP_CONCURRENT_GC;
  for ALoaderFlag := Low(TJclClrHostLoaderFlag) to High(TJclClrHostLoaderFlag) do
    if ALoaderFlag in LoaderFlags then
      Flags := Flags or ClrHostLoaderFlagValues[ALoaderFlag];
  OleCheck(CorBindToRuntimeEx(PWideCharOrNil(ClrVer),
    PWideChar(ClrHostFlavorNames[Flavor]), Flags,
    CLASS_CorRuntimeHost, IID_ICorRuntimeHost, FDefaultInterface));
end;

destructor TJclClrHost.Destroy;
begin
  FreeAndNil(FAppDomains);
  inherited Destroy;
end;

procedure TJclClrHost.EnumAppDomains;
var
  hEnum: Pointer;
  Unk: IUnknown;
begin
  if Assigned(FAppDomains) then
    FAppDomains.Clear
  else
    FAppDomains := TObjectList.Create;

  OleCheck(FDefaultInterface.EnumDomains(hEnum));
  try
    while FDefaultInterface.NextDomain(hEnum, Unk) <> S_FALSE do
      TJclClrAppDomain.Create(Self, Unk as IJclClrAppDomain);
  finally
    OleCheck(FDefaultInterface.CloseEnum(hEnum));
  end;
end;

function TJclClrHost.FindAppDomain(const Intf: IJclClrAppDomain;
  var Ret: TJclClrAppDomain): Boolean;
var
  I: Integer;
begin
  for I := 0 to AppDomainCount-1 do
  begin
    Ret := AppDomains[I];
    if Ret.DefaultInterface = Intf then
    begin
      Result := True;
      Exit;
    end;
  end;
  Ret := nil;
  Result := False;
end;

function TJclClrHost.FindAppDomain(const Name: WideString;
  var Ret: TJclClrAppDomain): Boolean;
var
  I: Integer;
begin
  for I := 0 to AppDomainCount-1 do
  begin
    Ret := AppDomains[I];
    if Ret.DefaultInterface.FriendlyName = Name then
    begin
      Result := True;
      Exit;
    end;
  end;
  Ret := nil;
  Result := False;
end;

function TJclClrHost.GetAppDomain(const Idx: Integer): TJclClrAppDomain;
begin
  Result := TJclClrAppDomain(FAppDomains.Items[Idx]);
end;

function TJclClrHost.GetAppDomainCount: Integer;
begin
  Result := FAppDomains.Count;
end;

function TJclClrHost.GetDefaultAppDomain: IJclClrAppDomain;
var
  Unk: IUnknown;
begin
  OleCheck(FDefaultInterface.GetDefaultDomain(Unk));
  Result := Unk as IJclClrAppDomain;
end;

class procedure TJclClrHost.GetClrVersions(VersionNames: TWideStrings);
  function DirectoryExistsW(const DirectoryName: WideString): Boolean;
  var
    Code: DWORD;
  begin
    Code := GetFileAttributesW(PWideChar(DirectoryName));
    Result := (Code <> $FFFFFFFF) and ((Code and FILE_ATTRIBUTE_DIRECTORY) <> 0);
  end;
const
  WideDirDelimiter: WideChar = '\';
var
  SystemDirectory: WideString;
  Index: Integer;
  PathOk: Boolean;
  FindData: TWin32FindDataW;
  SearchHandle: THandle;
  DirectoryBuffer, VersionBuffer: WideString;
  DirectoryLength, VersionLength, OldErrorMode, RuntimeInfo: DWORD;
begin
  SystemDirectory := CorSystemDirectory;

  if Pos('V1', AnsiUpperCase(CorVersion)) > 0 then
    RunTimeInfo := 0
  else
    RunTimeInfo := RUNTIME_INFO_DONT_SHOW_ERROR_DIALOG;

  if (SystemDirectory = '') or not DirectoryExistsW(SystemDirectory) then
    Exit;

  PathOk := False;
  for Index := Length(SystemDirectory) - 1 downto 1 do
    if SystemDirectory[Index] = WideDirDelimiter then
  begin
    SetLength(SystemDirectory, Index);
    PathOk := True;
    Break;
  end;

  if PathOk then
  begin
    SearchHandle := FindFirstFileW(PWideChar(SystemDirectory + '*.*'), FindData);
    if SearchHandle = INVALID_HANDLE_VALUE then
      Exit;
    try
      repeat
        if ((FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0)
          and (WideString(FindData.cFileName) <> '.') and (WideString(FindData.cFileName) <> '..') then
        begin
          OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
          try
            if (GetRequestedRuntimeInfo(nil, FindData.cFileName, nil, 0, RunTimeInfo,
              nil, 0, DirectoryLength, nil, 0, VersionLength) and $1FFF = ERROR_INSUFFICIENT_BUFFER)
              and (DirectoryLength > 0) and (VersionLength > 0) then
            begin
              SetLength(DirectoryBuffer, DirectoryLength - 1);
              SetLength(VersionBuffer, VersionLength - 1);
              if GetRequestedRuntimeInfo(nil, FindData.cFileName, nil, 0, RUNTIME_INFO_DONT_SHOW_ERROR_DIALOG,
                PWideChar(DirectoryBuffer), DirectoryLength, DirectoryLength,
                PWideChar(VersionBuffer), VersionLength, VersionLength) = S_OK then
                VersionNames.Values[VersionBuffer] := DirectoryBuffer + VersionBuffer;
            end;
          finally
            SetErrorMode(OldErrorMode);
          end;
        end;
      until not FindNextFileW(SearchHandle, FindData);
    finally
      Windows.FindClose(SearchHandle);
    end;
  end;
end;

class procedure TJclClrHost.GetClrVersions(VersionNames: TStrings);
var
  AWideStrings: TWideStrings;
  Index: Integer;
begin
  AWideStrings := TWideStringList.Create;
  try
    GetCLRVersions(AWideStrings);
    for Index := 0 to AWideStrings.Count - 1 do
      VersionNames.Add(AWideStrings.Strings[Index]);
  finally
    AWideStrings.Free;
  end;
end;

function TJclClrHost.GetCurrentAppDomain: IJclClrAppDomain;
var
  Unk: IUnknown;
begin
  OleCheck(FDefaultInterface.CurrentDomain(Unk));
  Result := Unk as IJclClrAppDomain;
end;

function TJclClrHost.AddAppDomain(const AppDomain: TJclClrAppDomain): Integer;
begin
  Result := FAppDomains.Add(AppDomain);
end;

function TJclClrHost.RemoveAppDomain(const AppDomain: TJclClrAppDomain): Integer;
begin
  Result := FAppDomains.Remove(AppDomain);
end;

class function TJclClrHost.CorSystemDirectory: WideString;
var
  Len: DWORD;
begin
  SetLength(Result, MAX_PATH);
  OleCheck(GetCORSystemDirectory(PWideChar(Result), Length(Result), Len));
  if Len > 0 then
    SetLength(Result, Len - 1);
end;

class function TJclClrHost.CorVersion: WideString;
var
  Len: DWORD;
begin
  SetLength(Result, 64);
  OleCheck(GetCORVersion(PWideChar(Result), Length(Result), Len));
  if Len > 0 then
    SetLength(Result, Len - 1);
end;

class function TJclClrHost.CorRequiredVersion: WideString;
var
  Len: DWORD;
begin
  SetLength(Result, 64);
  OleCheck(GetCORRequiredVersion(PWideChar(Result), Length(Result), Len));
  if Len > 0 then
    SetLength(Result, Len - 1);
end;

function TJclClrHost.CreateDomainSetup: TJclClrAppDomainSetup;
var
  pUnk: IUnknown;
begin
  OleCheck(FDefaultInterface.CreateDomainSetup(pUnk));
  Result := TJclClrAppDomainSetup.Create(pUnk as IAppDomainSetup);
end;

function TJclClrHost.CreateAppDomain(const Name: WideString;
  const Setup: TJclClrAppDomainSetup;
  const Evidence: IJclClrEvidence): TJclClrAppDomain;
var
  pUnk: IUnknown;
begin
  OleCheck(FDefaultInterface.CreateDomainEx(PWideChar(Name), Setup as IAppDomainSetup, Evidence, pUnk));
  Result := TJclClrAppDomain.Create(Self, pUnk as IJclClrAppDomain);
end;

procedure TJclClrHost.Start;
begin
  OleCheck(DefaultInterface.Start);
  Refresh;
end;

procedure TJclClrHost.Stop;
begin
  OleCheck(DefaultInterface.Stop);
end;

procedure TJclClrHost.Refresh;
begin
  EnumAppDomains;
end;

//=== { TJclClrAppDomain } ===================================================

constructor TJclClrAppDomain.Create(const AHost: TJclClrHost;
  const AAppDomain: IJclClrAppDomain);
begin
  Assert(Assigned(AHost));
  Assert(Assigned(AAppDomain));
  inherited Create;
  FHost := AHost;
  FDefaultInterface := AAppDomain;
  FHost.AddAppDomain(Self);
end;

function TJclClrAppDomain.Execute(const AssemblyFile: TFileName;
  const Arguments: TJclClrAssemblyArguments;
  const AssemblySecurity: IJclClrEvidence): Integer;
var
  Args: Variant;
begin
  Assert(FileExists(AssemblyFile));
  if Length(Arguments) = 0 then
    Result := Execute(AssemblyFile, AssemblySecurity)
  else
  begin
    DynArrayToVariant(Args, @Arguments[0], TypeInfo(TJclClrAssemblyArguments));
    Result := DefaultInterface.ExecuteAssembly_3(AssemblyFile, AssemblySecurity, PSafeArray(TVarData(Args).VArray));
  end;
end;

function TJclClrAppDomain.Execute(const AssemblyFile: TFileName;
  const AssemblySecurity: IJclClrEvidence): Integer;
begin
  Assert(FileExists(AssemblyFile));
  if Assigned(AssemblySecurity) then
    Result := DefaultInterface.ExecuteAssembly(AssemblyFile, AssemblySecurity)
  else
    Result := DefaultInterface.ExecuteAssembly_2(AssemblyFile);
end;

function TJclClrAppDomain.Execute(const AssemblyFile: TFileName;
  const Arguments: TStrings; const AssemblySecurity: IJclClrEvidence): Integer;
var
  Args: Variant;
  Index: Integer;
begin
  Assert(FileExists(AssemblyFile));
  if Arguments.Count = 0 then
    Result := Execute(AssemblyFile, AssemblySecurity)
  else
  begin
    Args := VarArrayCreate([0, Arguments.Count - 1], varOleStr);
    for Index := 0 to Arguments.Count - 1 do
      Args[Index] := WideString(Arguments.Strings[Index]);
    Result := DefaultInterface.ExecuteAssembly_3(AssemblyFile, AssemblySecurity, PSafeArray(TVarData(Args).VArray));
  end;
end;

function TJclClrAppDomain.Load(const AssemblyString: WideString;
  const AssemblySecurity: IJclClrEvidence): TJclClrAssembly;
begin
  if Assigned(AssemblySecurity) then
    Result := TJclClrAssembly.Create(DefaultInterface.Load_7(AssemblyString, AssemblySecurity))
  else
    Result := TJclClrAssembly.Create(DefaultInterface.Load_2(AssemblyString));
end;

function TJclClrAppDomain.Load(const RawAssemblyStream,
  RawSymbolStoreStream: TStream;
  const AssemblySecurity: IJclClrEvidence): TJclClrAssembly;
var
  RawAssembly, RawSymbolStore: Variant;
begin
  Assert(Assigned(RawAssemblyStream));
  RawAssembly := VarArrayCreate([0, RawAssemblyStream.Size-1], varByte);
  try
    try
      RawAssemblyStream.Read(VarArrayLock(RawAssembly)^, RawAssemblyStream.Size);
    finally
      VarArrayUnlock(RawAssembly);
    end;

    if not Assigned(RawSymbolStoreStream) then
      Result := TJclClrAssembly.Create(DefaultInterface.Load_3(PSafeArray(TVarData(RawAssembly).VArray)))
    else
    begin
      RawSymbolStore := VarArrayCreate([0, RawSymbolStoreStream.Size-1], varByte);
      try
        try
          RawSymbolStoreStream.Read(VarArrayLock(RawSymbolStore)^, RawSymbolStoreStream.Size);
        finally
          VarArrayUnlock(RawSymbolStore);
        end;

        if Assigned(AssemblySecurity) then
          Result := TJclClrAssembly.Create(DefaultInterface.Load_5(
            PSafeArray(TVarData(RawAssembly).VArray),
            PSafeArray(TVarData(RawSymbolStore).VArray),
            AssemblySecurity))
        else
          Result := TJclClrAssembly.Create(DefaultInterface.Load_4(
            PSafeArray(TVarData(RawAssembly).VArray),
            PSafeArray(TVarData(RawSymbolStore).VArray)));
      finally
        VarClear(RawSymbolStore);
      end;
    end;
  finally
    VarClear(RawAssembly);
  end;
end;

procedure TJclClrAppDomain.Unload;
var
  AppDomain: TJclClrAppDomain;
begin
  OleCheck(FHost.DefaultInterface.UnloadDomain(DefaultInterface));
  if FHost.FindAppDomain(DefaultInterface, AppDomain) and (AppDomain = Self) then
    FHost.RemoveAppDomain(Self);
end;

//=== { TJclClrObject } ======================================================

constructor TJclClrObject.Create(const AssemblyName, NamespaceName, ClassName: WideString;
  const Parameters: array of const);
begin
  inherited Create;
end;

constructor TJclClrObject.Create(const AssemblyName, NamespaceName, ClassName: WideString;
  const NewInstance: Boolean);
begin
  Create(AssemblyName, NamespaceName, ClassName, []);
end;

function TJclClrObject.GetField(const Name: WideString): TJclClrField;
begin
  // (rom) added to suppress warning until implementation
  Result := nil;
end;

function TJclClrObject.GetProperty(const Name: WideString): TJclClrProperty;
begin
  // (rom) added to suppress warning until implementation
  Result := nil;
end;

function TJclClrObject.GetMethod(const Name: WideString): TJclClrMethod;
begin
  // (rom) added to suppress warning until implementation
  Result := nil;
end;

//=== { TJclClrAppDomainSetup } ==============================================

constructor TJclClrAppDomainSetup.Create(Intf: IAppDomainSetup);
begin
  Assert(Assigned(Intf));
  inherited Create;
  FDefaultInterface := Intf;
end;

function TJclClrAppDomainSetup.GetApplicationBase: WideString;
begin
  OleCheck(FDefaultInterface.Get_ApplicationBase(Result));
end;

function TJclClrAppDomainSetup.GetApplicationName: WideString;
begin
  OleCheck(FDefaultInterface.Get_ApplicationName(Result));
end;

function TJclClrAppDomainSetup.GetCachePath: WideString;
begin
  OleCheck(FDefaultInterface.Get_CachePath(Result));
end;

function TJclClrAppDomainSetup.GetConfigurationFile: WideString;
begin
  OleCheck(FDefaultInterface.Get_ConfigurationFile(Result));
end;

function TJclClrAppDomainSetup.GetDynamicBase: WideString;
begin
  OleCheck(FDefaultInterface.Get_DynamicBase(Result));
end;

function TJclClrAppDomainSetup.GetLicenseFile: WideString;
begin
  OleCheck(FDefaultInterface.Get_LicenseFile(Result));
end;

function TJclClrAppDomainSetup.GetPrivateBinPath: WideString;
begin
  OleCheck(FDefaultInterface.Get_PrivateBinPath(Result));
end;

function TJclClrAppDomainSetup.GetPrivateBinPathProbe: WideString;
begin
  OleCheck(FDefaultInterface.Get_PrivateBinPathProbe(Result));
end;

function TJclClrAppDomainSetup.GetShadowCopyDirectories: WideString;
begin
  OleCheck(FDefaultInterface.Get_ShadowCopyDirectories(Result));
end;

function TJclClrAppDomainSetup.GetShadowCopyFiles: WideString;
begin
  OleCheck(FDefaultInterface.Get_ShadowCopyFiles(Result));
end;

procedure TJclClrAppDomainSetup.SetApplicationBase(const Value: WideString);
begin
  OleCheck(FDefaultInterface.Set_ApplicationBase(Value));
end;

procedure TJclClrAppDomainSetup.SetApplicationName(const Value: WideString);
begin
  OleCheck(FDefaultInterface.Set_ApplicationName(Value));
end;

procedure TJclClrAppDomainSetup.SetCachePath(const Value: WideString);
begin
  OleCheck(FDefaultInterface.Set_CachePath(Value));
end;

procedure TJclClrAppDomainSetup.SetConfigurationFile(const Value: WideString);
begin
  OleCheck(FDefaultInterface.Set_ConfigurationFile(Value));
end;

procedure TJclClrAppDomainSetup.SetDynamicBase(const Value: WideString);
begin
  OleCheck(FDefaultInterface.Set_DynamicBase(Value));
end;

procedure TJclClrAppDomainSetup.SetLicenseFile(const Value: WideString);
begin
  OleCheck(FDefaultInterface.Set_LicenseFile(Value));
end;

procedure TJclClrAppDomainSetup.SetPrivateBinPath(const Value: WideString);
begin
  OleCheck(FDefaultInterface.Set_PrivateBinPath(Value));
end;

procedure TJclClrAppDomainSetup.SetPrivateBinPathProbe(const Value: WideString);
begin
  OleCheck(FDefaultInterface.Set_PrivateBinPathProbe(Value));
end;

procedure TJclClrAppDomainSetup.SetShadowCopyDirectories(const Value: WideString);
begin
  OleCheck(FDefaultInterface.Set_ShadowCopyDirectories(Value));
end;

procedure TJclClrAppDomainSetup.SetShadowCopyFiles(const Value: WideString);
begin
  OleCheck(FDefaultInterface.Set_ShadowCopyFiles(Value));
end;

//=== { TJclClrAssembly } ====================================================

constructor TJclClrAssembly.Create(Intf: IJclClrAssembly);
begin
  Assert(Assigned(Intf));
  inherited Create;
  FDefaultInterface := Intf;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
