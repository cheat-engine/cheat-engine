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
{ The Original Code is JclWin32Ex.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Virgo Pärna (virgo dott parna att mail dott ee).   }
{ Portions created by Virgo Pärna are Copyright (C) 2006 Virgo Pärna. All Rights Reserved.         }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclWin32Ex;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Sysutils;

type
  TJclWin32ExFunction = (jwfTryEnterCriticalSection, jwfSignalObjectAndWait,
    jwfSetCriticalSectionSpinCount, jwfOpenWaitableTimer,
    jwfInitializeCriticalSectionAndSpinCount, jwfGetFileAttributesEx,
    jwfCreateWaitableTimer, jwfCancelWaitableTimer, jwfglGetString,
    jwfglGetError, jwfwglCreateContext, jwfwglDeleteContext, jwfwglMakeCurrent,
    jwfgluErrorString);
  TJclWin32ExFunctions = set of TJclWin32ExFunction;

function JclTryEnterCriticalSection(lpCriticalSection: TRTLCriticalSection):  Boolean;
function JclSignalObjectAndWait(hObjectToSignal: THandle;
  hObjectToWaitOn: THandle; dwMilliseconds: Cardinal;
  bAlertable: Boolean): Cardinal;
function JclSetCriticalSectionSpinCount(lpCriticalSection: TRTLCriticalSection;
  dwSpinCount: Cardinal): Cardinal;
function JclOpenWaitableTimer(dwDesiredAccess: Cardinal;
  bInheritHandle: Boolean; const lpTimerName: string): THandle;
function JclInitializeCriticalSectionAndSpinCount(lpCriticalSection: TRTLCriticalSection;
  dwSpinCount: Cardinal): Boolean;
function JclGetFileAttributesEx(const lpFileName: string;
  fInfoLevelId: TGetFileExInfoLevels; lpFileInformation: Pointer): Boolean;
function JclCreateWaitableTimer(lpTimerAttributes: PSecurityAttributes;
  bManualReset: Boolean; const lpTimerName: string): THandle;
function JclCancelWaitableTimer(hTimer: THandle): Boolean;

function JclglGetString(name: Cardinal): PChar;
function JclglGetError: Cardinal;

function JclwglCreateContext(hdc: HDC): HGLRC;
function JclwglDeleteContext(hglrc: HGLRC): BOOL;
function JclwglMakeCurrent(hdc: HDC; hglrc: HGLRC): BOOL;

function JclgluErrorString(errCode: Cardinal): PChar;

function JclWin32ExFunctions: TJclWin32ExFunctions;

procedure JclCheckAndInitializeOpenGL;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/windows/JclWin32Ex.pas $';
    Revision: '$Revision: 2175 $';
    Date: '$Date: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007) $';
    LogPath: 'JCL\source\windows'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  JclBase, JclResources;

type
  TTryEnterCriticalSectionProc = function(lpCriticalSection: TRTLCriticalSection): BOOL; stdcall;
  TSignalObjectAndWaitProc = function(hObjectToSignal: THandle;
    hObjectToWaitOn: THandle; dwMilliseconds: DWORD; bAlertable: BOOL): DWORD; stdcall;
  TSetCriticalSectionSpinCountProc = function(lpCriticalSection: TRTLCriticalSection;
    dwSpincCount: DWORD): DWORD; stdcall;
  TInitializeCriticalSectionAndSpinCountProc = function(lpCriticalSection: TRTLCriticalSection;
    dwSpinCount: DWORD): BOOL; stdcall;

  TOpenWaitableTimerAProc = function(dwDesiredAccess: DWORD;
    bInheritHandle: BOOL; lpTimerName: LPCTSTR): THandle; stdcall;
  TCreateWaitableTimerAProc = function(lpTimerAttributes: PSecurityAttributes;
    bManualReset: BOOL; lpTimerName: PAnsiChar): THandle; stdcall;
  TCancelWaitableTimerProc = function(hTimer: THandle): BOOL; stdcall;

  TGetFileAttributesExAProc = function(lpFileName: PChar;
    fInfoLevelId: TGetFileExInfoLevels; lpFileInformation: Pointer): BOOL; stdcall;

  TglGetStringProc = function(name: Cardinal): PChar; stdcall;
  TglGetErrorProc = function: Cardinal; stdcall;

  TwglCreateContextProc = function (hdc: HDC): HGLRC; stdcall;
  TwglDeleteContextProc = function (hglrc: HGLRC): BOOL; stdcall;
  TwglMakeCurrentProc = function (hdc: HDC; hglrc: HGLRC): BOOL; stdcall;

  TgluErrorStringProc = function(errCode: Cardinal): PChar; stdcall;

var
  Kernel32DllHandle: HMODULE = 0;
  OpenGl32DllHandle: HMODULE = 0;
  Glu32DllHandle: HMODULE = 0;

type
  TDllFunctionRec = record
    FunctionName: string;
    FunctionAddr: Pointer;
    DllName: string;
    DllHandle: ^HModule;
  end;

const
  Glu32 = 'glu32.dll';

  Win32ExFunctions: array [TJclWin32ExFunction] of TDllFunctionRec =
   ( // jwfTryEnterCriticalSection
     (FunctionName: 'TryEnterCriticalSection'; FunctionAddr: nil;
      DllName: kernel32; DllHandle: @Kernel32DllHandle),
     // jwfSignalObjectAndWait
     (FunctionName: 'SignalObjectAndWait'; FunctionAddr: nil;
      DllName: kernel32; DllHandle: @Kernel32DllHandle),
     // jwfSetCriticalSectionSpinCount
     (FunctionName: 'SetCriticalSectionSpinCount'; FunctionAddr: nil;
      DllName: kernel32; DllHandle: @Kernel32DllHandle),
     // jwfOpenWaitableTimer
     (FunctionName: 'OpenWaitableTimerA'; FunctionAddr: nil;
      DllName: kernel32; DllHandle: @Kernel32DllHandle),
     // jwfInitializeCriticalSectionAndSpinCount
     (FunctionName: 'InitializeCriticalSectionAndSpinCount'; FunctionAddr: nil;
      DllName: kernel32; DllHandle: @Kernel32DllHandle),
     // jwfGetFileAttributesEx
     (FunctionName: 'GetFileAttributesExA'; FunctionAddr: nil;
      DllName: kernel32; DllHandle: @Kernel32DllHandle),
     // jwfCreateWaitableTimer
     (FunctionName: 'CreateWaitableTimerA'; FunctionAddr: nil;
      DllName: kernel32; DllHandle: @Kernel32DllHandle),
     // jwfCancelWaitableTimer
     (FunctionName: 'CancelWaitableTimer'; FunctionAddr: nil;
      DllName: kernel32; DllHandle: @Kernel32DllHandle),
     // jwfglGetString
     (FunctionName: 'glGetString'; FunctionAddr: nil;
      DllName: opengl32; DllHandle: @OpenGl32DllHandle),
     // jwfglGetError
     (FunctionName: 'glGetError'; FunctionAddr: nil;
      DllName: opengl32; DllHandle: @OpenGl32DllHandle),
     // jwfwglCreateContext
     (FunctionName: 'wglCreateContext'; FunctionAddr: nil;
      DllName: opengl32; DllHandle: @OpenGl32DllHandle),
     // jwfwglDeleteContext
     (FunctionName: 'wglDeleteContext'; FunctionAddr: nil;
      DllName: opengl32; DllHandle: @OpenGl32DllHandle),
     // jwfwglMakeCurrent
     (FunctionName: 'wglMakeCurrent'; FunctionAddr: nil;
      DllName: opengl32; DllHandle: @OpenGl32DllHandle),
     // jwfgluErrorString
     (FunctionName: 'gluErrorString'; FunctionAddr: nil;
      DllName: opengl32; DllHandle: @Glu32DllHandle)
   );

function LoadWin32ExFunction(const Win32ExFunction: TJclWin32ExFunction): Pointer;
begin
  with Win32ExFunctions[Win32ExFunction] do
  begin
    if not Assigned(FunctionAddr) then
    begin
      if DllHandle^ = 0 then
        DllHandle^ := SafeLoadLibrary(DllName);
      if DllHandle^ = 0 then
        raise EJclError.CreateResFmt(@RsELibraryNotFound, [DllName])
      else
        FunctionAddr := GetProcAddress(DllHandle^, PChar(FunctionName));
      if not Assigned(FunctionAddr) then
        raise EJclError.CreateResFmt(@RsEFunctionNotFound, [DllName, FunctionName]);
    end;
    Result := FunctionAddr;
  end;
end;

function JclTryEnterCriticalSection(lpCriticalSection: TRTLCriticalSection): Boolean;
var
  FunctionAddr: Pointer;
begin
  FunctionAddr := Win32ExFunctions[jwfTryEnterCriticalSection].FunctionAddr;
  if not Assigned(FunctionAddr) then
    FunctionAddr := LoadWin32ExFunction(jwfTryEnterCriticalSection);

  Result := TTryEnterCriticalSectionProc(FunctionAddr)(lpCriticalSection);
end;

function JclSignalObjectAndWait(hObjectToSignal: THandle; hObjectToWaitOn: THandle; dwMilliseconds: Cardinal; bAlertable: Boolean): Cardinal;
var
  FunctionAddr: Pointer;
begin
  FunctionAddr := Win32ExFunctions[jwfSignalObjectAndWait].FunctionAddr;
  if not Assigned(FunctionAddr) then
    FunctionAddr := LoadWin32ExFunction(jwfSignalObjectAndWait);

  Result := TSignalObjectAndWaitProc(FunctionAddr)(hObjectToSignal, hObjectToSignal, dwMilliseconds, bAlertable);
end;

function JclSetCriticalSectionSpinCount(lpCriticalSection: TRTLCriticalSection; dwSpinCount: Cardinal): Cardinal;
var
  FunctionAddr: Pointer;
begin
  FunctionAddr := Win32ExFunctions[jwfSetCriticalSectionSpinCount].FunctionAddr;
  if not Assigned(FunctionAddr) then
    FunctionAddr := LoadWin32ExFunction(jwfSetCriticalSectionSpinCount);

  Result := TSetCriticalSectionSpinCountProc(FunctionAddr)(lpCriticalSection, dwSpinCount);
end;

function JclOpenWaitableTimer(dwDesiredAccess: Cardinal; bInheritHandle: Boolean; const lpTimerName: string): THandle;
var
  FunctionAddr: Pointer;
begin
  FunctionAddr := Win32ExFunctions[jwfOpenWaitableTimer].FunctionAddr;
  if not Assigned(FunctionAddr) then
    FunctionAddr := LoadWin32ExFunction(jwfOpenWaitableTimer);

  Result := TOpenWaitableTimerAProc(FunctionAddr)(dwDesiredAccess, bInheritHandle, PChar(lpTimerName));
end;

function JclInitializeCriticalSectionAndSpinCount(lpCriticalSection: TRTLCriticalSection; dwSpinCount: Cardinal): Boolean;
var
  FunctionAddr: Pointer;
begin
  FunctionAddr := Win32ExFunctions[jwfInitializeCriticalSectionAndSpinCount].FunctionAddr;
  if not Assigned(FunctionAddr) then
    FunctionAddr := LoadWin32ExFunction(jwfInitializeCriticalSectionAndSpinCount);

  Result := TInitializeCriticalSectionAndSpinCountProc(FunctionAddr)(lpCriticalSection, dwSpinCount);
end;

function JclGetFileAttributesEx(const lpFileName: string; fInfoLevelId: TGetFileExInfoLevels; lpFileInformation: Pointer): Boolean;
var
  FunctionAddr: Pointer;
begin
  FunctionAddr := Win32ExFunctions[jwfGetFileAttributesEx].FunctionAddr;
  if not Assigned(FunctionAddr) then
    FunctionAddr := LoadWin32ExFunction(jwfGetFileAttributesEx);

  Result := TGetFileAttributesExAProc(FunctionAddr)(PChar(lpFileName), fInfoLevelId, lpFileInformation);
end;

function JclCreateWaitableTimer(lpTimerAttributes: PSecurityAttributes; bManualReset: Boolean; const lpTimerName: string): THandle;
var
  FunctionAddr: Pointer;
begin
  FunctionAddr := Win32ExFunctions[jwfCreateWaitableTimer].FunctionAddr;
  if not Assigned(FunctionAddr) then
    FunctionAddr := LoadWin32ExFunction(jwfCreateWaitableTimer);

  Result := TCreateWaitableTimerAProc(FunctionAddr)(lpTimerAttributes, bManualReset, PChar(lpTimerAttributes));
end;

function JclCancelWaitableTimer(hTimer: THandle): Boolean;
var
  FunctionAddr: Pointer;
begin
  FunctionAddr := Win32ExFunctions[jwfCancelWaitableTimer].FunctionAddr;
  if not Assigned(FunctionAddr) then
    FunctionAddr := LoadWin32ExFunction(jwfCancelWaitableTimer);

  Result := TCancelWaitableTimerProc(FunctionAddr)(hTimer);
end;

function JclglGetString(name: Cardinal): PChar;
var
  FunctionAddr: Pointer;
begin
  FunctionAddr := Win32ExFunctions[jwfglGetString].FunctionAddr;
  if not Assigned(FunctionAddr) then
    FunctionAddr := LoadWin32ExFunction(jwfglGetString);

  Result := TglGetStringProc(FunctionAddr)(name);
end;

function JclglGetError: Cardinal;
var
  FunctionAddr: Pointer;
begin
  FunctionAddr := Win32ExFunctions[jwfglGetError].FunctionAddr;
  if not Assigned(FunctionAddr) then
    FunctionAddr := LoadWin32ExFunction(jwfglGetError);

  Result := TglGetErrorProc(FunctionAddr);
end;

function JclwglCreateContext(hdc: HDC): HGLRC;
var
  FunctionAddr: Pointer;
begin
  FunctionAddr := Win32ExFunctions[jwfwglCreateContext].FunctionAddr;
  if not Assigned(FunctionAddr) then
    FunctionAddr := LoadWin32ExFunction(jwfwglCreateContext);

  Result := TwglCreateContextProc(FunctionAddr)(hdc);
end;

function JclwglDeleteContext(hglrc: HGLRC): BOOL;
var
  FunctionAddr: Pointer;
begin
  FunctionAddr := Win32ExFunctions[jwfwglDeleteContext].FunctionAddr;
  if not Assigned(FunctionAddr) then
    FunctionAddr := LoadWin32ExFunction(jwfwglDeleteContext);

  Result := TwglDeleteContextProc(FunctionAddr)(hglrc);
end;

function JclwglMakeCurrent(hdc: HDC; hglrc: HGLRC): BOOL;
var
  FunctionAddr: Pointer;
begin
  FunctionAddr := Win32ExFunctions[jwfwglMakeCurrent].FunctionAddr;
  if not Assigned(FunctionAddr) then
    FunctionAddr := LoadWin32ExFunction(jwfwglMakeCurrent);

  Result := TwglMakeCurrentProc(FunctionAddr)(hdc, hglrc);
end;

function JclgluErrorString(errCode: Cardinal): PChar;
var
  FunctionAddr: Pointer;
begin
  FunctionAddr := Win32ExFunctions[jwfgluErrorString].FunctionAddr;
  if not Assigned(FunctionAddr) then
    FunctionAddr := LoadWin32ExFunction(jwfgluErrorString);

  Result := TgluErrorStringProc(FunctionAddr)(errCode);
end;

function JclWin32ExFunctions: TJclWin32ExFunctions;
var
  Index: TJclWin32ExFunction;
begin
  Result := [];
  for Index := Low(TJclWin32ExFunction) to High(TJclWin32ExFunction) do
    if Assigned(Win32ExFunctions[Index].FunctionAddr)
      or (LoadWin32ExFunction(Index) <> nil) then
      Include(Result, Index);
end;

procedure UnloadLibraries;
var
  Index: TJclWin32ExFunction;
begin
  for Index := Low(TJclWin32ExFunction) to High(TJclWin32ExFunction) do
    with Win32ExFunctions[Index] do
  begin
    FunctionAddr := nil;
    if DllHandle^ <> 0 then
    begin
      FreeLibrary(DllHandle^);
      DllHandle^ := 0;
    end;
  end;
end;

procedure JclCheckAndInitializeOpenGL;
begin
  if OpenGl32DllHandle = 0 then
    OpenGl32DllHandle := SafeLoadLibrary(opengl32);
  if OpenGl32DllHandle = 0 then
    raise EJclError.CreateResFmt(@RsELibraryNotFound, [opengl32]);
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
  UnloadLibraries;

end.

