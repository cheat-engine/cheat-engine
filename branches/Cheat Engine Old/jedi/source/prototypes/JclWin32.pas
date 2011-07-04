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
{ Portions of this code are translated from DelayImp.h.                                            }
{ The Initial Developer of DelayImp.h is Inprise Corporation. Portions created by Inprise          }
{ Corporation are Copyright (C) 1999, 2000 by Inprise Corporation. All Rights Reserved.            }
{                                                                                                  }
{ The Original Code is JclWin32.pas.                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel. Portions created by Marcel van  }
{ Brakel are Copyright (C) Marcel van Brakel. All Rights Reserved.                                 }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Marcel van Brakel                                                                              }
{   Peter Friese                                                                                   }
{   Andreas Hausladen (ahuser)                                                                     }
{   Flier Lu (flier)                                                                               }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Olivier Sannier (obones)                                                                       }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit defines various Win32 API declarations which are either missing or incorrect in one or }
{ more of the supported Delphi versions. This unit is not intended for regular code, only API      }
{ declarations.                                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-06-28 22:50:25 +0200 (sam., 28 juin 2008)                          $ }
{ Revision:      $Rev:: 2388                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclWin32;

{$I jcl.inc}

{$MINENUMSIZE 4}
{$ALIGN ON}
{$WARNINGS OFF}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, SysUtils,
  {$IFNDEF FPC}
  {$IFDEF CLR}
  System.Runtime.InteropServices, System.Security,
  {$ELSE}
  AccCtrl,
  {$ENDIF CLR}
  ActiveX,
  {$ENDIF ~FPC}
  JclBase;

{$HPPEMIT ''}
{$IFDEF COMPILER5}
{$HPPEMIT '// To lift ambiguity between LONG64 and System::LONG64'}
{$HPPEMIT '#define LONG64 System::LONG64'}
{$HPPEMIT ''}
{$ENDIF COMPILER5}
{$HPPEMIT '#include <WinDef.h>'}
{$HPPEMIT '#include <WinNT.h>'}
{$HPPEMIT '#include <WinBase.h>'}
{$HPPEMIT '#include <BaseTsd.h>'}
{$HPPEMIT '#include <ImageHlp.h>'}
{$HPPEMIT '#include <lm.h>'}
{$HPPEMIT '#include <Nb30.h>'}
{$HPPEMIT '#include <RasDlg.h>'}
{$IFDEF COMPILER6_UP}
{$HPPEMIT '#include <Reason.h>'}
{$ENDIF COMPILER6_UP}
{$HPPEMIT '#include <ShlWApi.h>'}
{$HPPEMIT '#include <WinError.h>'}
{$HPPEMIT '#include <WinIoCtl.h>'}
{$HPPEMIT '#include <WinUser.h>'}
//{$HPPEMIT '#include <Powrprof.h>'}
{$HPPEMIT '#include <delayimp.h>'}
{$HPPEMIT '#include <propidl.h>'}
{$HPPEMIT '#include <msidefs.h>'}
{$HPPEMIT '#include <shlguid.h>'}
{$IFDEF COMPILER6_UP}
{$HPPEMIT '#include <imgguids.h>'}
{$ENDIF COMPILER6_UP}
{$HPPEMIT '#include <objbase.h>'}
{$HPPEMIT '#include <ntsecapi.h>'}
{$HPPEMIT ''}

{$IFDEF CLR}
type
  LPSTR = string;
  LPWSTR = string;
  LPCSTR = string;
  LPCWSTR = string;
  LPCTSTR = string;
  PLongWord = ^LongWord;
  PByte = IntPtr;
{$ENDIF CLR}

{$I win32api\WinDef.int}
{$I win32api\WinNT.int}
{$I win32api\WinBase.int}
{$I win32api\BaseTsd.int}
{$I win32api\AclApi.int}
{$I win32api\ImageHlp.int}
{$I win32api\LmErr.int}
{$I win32api\LmCons.int}
{$I win32api\LmAccess.int}
{$I win32api\LmApiBuf.int}
{$I win32api\Nb30.int}
{$I win32api\RasDlg.int}
{$I win32api\Reason.int}
{$I win32api\ShlObj.int}
{$I win32api\ShlWApi.int}
{$I win32api\WinError.int}
{$I win32api\WinIoctl.int}
{$I win32api\WinNLS.int}
{$I win32api\WinUser.int}
{$I win32api\PowrProf.int}
{$I win32api\DelayImp.int}
{$I win32api\PropIdl.int}
{$I win32api\MsiDefs.int}
{$I win32api\ShlGuid.int}
{$I win32api\imgguids.int}
{$I win32api\ObjBase.int}
{$I win32api\NtSecApi.int}

{$IFDEF MSWINDOWS}

{$IFNDEF CLR}

const
  RtdlSetNamedSecurityInfoW: function(pObjectName: LPWSTR; ObjectType: SE_OBJECT_TYPE;
    SecurityInfo: SECURITY_INFORMATION; psidOwner, psidGroup: PSID;
    pDacl, pSacl: PACL): DWORD stdcall = SetNamedSecurityInfoW;

  RtdlSetWaitableTimer: function(hTimer: THandle; var lpDueTime: TLargeInteger;
    lPeriod: Longint; pfnCompletionRoutine: TFNTimerAPCRoutine;
    lpArgToCompletionRoutine: Pointer; fResume: BOOL): BOOL stdcall = SetWaitableTimer;

  RtdlNetUserAdd: function(servername: LPCWSTR; level: DWORD;
    buf: PByte; parm_err: PDWord): NET_API_STATUS stdcall = NetUserAdd;

  RtdlNetUserDel: function(servername: LPCWSTR;
    username: LPCWSTR): NET_API_STATUS stdcall = NetUserDel;

  RtdlNetGroupAdd: function(servername: LPCWSTR; level: DWORD; buf: PByte;
    parm_err: PDWord): NET_API_STATUS stdcall = NetGroupAdd;

  RtdlNetGroupEnum: function(servername: LPCWSTR; level: DWORD;
    out bufptr: PByte; prefmaxlen: DWORD; out entriesread, totalentries: DWORD;
    resume_handle: PDWORD_PTR): NET_API_STATUS stdcall = NetGroupEnum;

  RtdlNetGroupDel: function(servername: LPCWSTR;
    groupname: LPCWSTR): NET_API_STATUS stdcall = NetGroupDel;

  RtdlNetLocalGroupAdd: function(servername: LPCWSTR; level: DWORD;
    buf: PByte; parm_err: PDWord): NET_API_STATUS stdcall = NetLocalGroupAdd;

  RtdlNetLocalGroupEnum: function(servername: LPCWSTR; level: DWORD;
    out bufptr: PByte; prefmaxlen: DWORD; out entriesread, totalentries: DWORD;
    resumehandle: PDWORD_PTR): NET_API_STATUS stdcall = NetLocalGroupEnum;

  RtdlNetLocalGroupDel: function(servername: LPCWSTR;
    groupname: LPCWSTR): NET_API_STATUS stdcall = NetLocalGroupDel;

  RtdlNetLocalGroupAddMembers: function(servername: LPCWSTR; groupname: LPCWSTR;
    level: DWORD; buf: PByte;
    totalentries: DWORD): NET_API_STATUS stdcall = NetLocalGroupAddMembers;

  RtdlNetApiBufferFree: function(Buffer: Pointer): NET_API_STATUS stdcall = NetApiBufferFree;

  RtdlGetCalendarInfoA: function(Locale: LCID; Calendar: CALID; CalType: CALTYPE;
    lpCalData: PAnsiChar; cchData: Integer;
    lpValue: PDWORD): Integer stdcall = GetCalendarInfoA;

  RtdlGetCalendarInfoW: function(Locale: LCID; Calendar: CALID; CalType: CALTYPE;
    lpCalData: PWideChar; cchData: Integer;
    lpValue: PDWORD): Integer stdcall = GetCalendarInfoW;

  RtdlEnumCalendarInfoExA: function(lpCalInfoEnumProc: TCalInfoEnumProcExA;
    Locale: LCID; Calendar: CALID; CalType: CALTYPE): BOOL stdcall = EnumCalendarInfoExA;

  RtdlGetVolumeNameForVolumeMountPoint: function(lpszVolumeMountPoint: LPCSTR;
    lpszVolumeName: LPSTR; cchBufferLength: DWORD): BOOL stdcall = GetVolumeNameForVolumeMountPoint;

  RtdlSetVolumeMountPoint: function(lpszVolumeMountPoint: LPCSTR;
    lpszVolumeName: LPCSTR): BOOL stdcall = SetVolumeMountPoint;

  RtdlDeleteVolumeMountPoint: function(lpszVolumeMountPoint: LPCSTR): BOOL
    stdcall = DeleteVolumeMountPoint;

  RtdlNetBios: function(P: PNCB): UCHAR stdcall = NetBios;

{$ENDIF ~CLR}
{$ENDIF MSWINDOWS}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/prototypes/JclWin32.pas $';
    Revision: '$Revision: 2388 $';
    Date: '$Date: 2008-06-28 22:50:25 +0200 (sam., 28 juin 2008) $';
    LogPath: 'JCL\source\windows'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  JclResources;

const
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE ~UNICODE}
  AWSuffix = 'A';
  {$ENDIF ~UNICODE}

{$IFNDEF CLR}
procedure GetProcedureAddress(var P: Pointer; const ModuleName, ProcName: string);
var
  ModuleHandle: HMODULE;
begin
  if not Assigned(P) then
  begin
    ModuleHandle := GetModuleHandle(PChar(ModuleName));
    if ModuleHandle = 0 then
    begin
      ModuleHandle := SafeLoadLibrary(PChar(ModuleName));
      if ModuleHandle = 0 then
        raise EJclError.CreateResFmt(@RsELibraryNotFound, [ModuleName]);
    end;
    P := GetProcAddress(ModuleHandle, PChar(ProcName));
    if not Assigned(P) then
      raise EJclError.CreateResFmt(@RsEFunctionNotFound, [ModuleName, ProcName]);
  end;
end;
{$ENDIF ~CLR}

{$I win32api\AclApi.imp}
{$I win32api\ImageHlp.imp}
{$I win32api\LmAccess.imp}
{$I win32api\LmApiBuf.imp}
{$I win32api\Nb30.imp}
{$I win32api\WinBase.imp}
{$I win32api\WinNLS.imp}
{$I win32api\WinNT.imp}
{$I win32api\PowrProf.imp}
{$I win32api\ObjBase.imp}
{$I win32api\NtSecApi.imp}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.



