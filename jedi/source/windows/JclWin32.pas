{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

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

type

//
// Unsigned Basics
//

  USHORT = Word;
  {$EXTERNALSYM USHORT}


//==================================================================================================
// presumable from any older WinNT.h or from WinIfs.h
//==================================================================================================

{$IFNDEF CLR}
//--------------------------------------------------------------------------------------------------
// NTFS Reparse Points
//--------------------------------------------------------------------------------------------------

// The reparse structure is used by layered drivers to store data in a
// reparse point. The constraints on reparse tags are defined below.
// This version of the reparse data buffer is only for Microsoft tags.

(*$HPPEMIT 'typedef struct _REPARSE_DATA_BUFFER {'*)
(*$HPPEMIT ''*)
(*$HPPEMIT '    DWORD   ReparseTag;'*)
(*$HPPEMIT '    WORD    ReparseDataLength;'*)
(*$HPPEMIT '    WORD    Reserved;'*)
(*$HPPEMIT ''*)
(*$HPPEMIT '    union {'*)
(*$HPPEMIT ''*)
(*$HPPEMIT '        struct {'*)
(*$HPPEMIT '            WORD    SubstituteNameOffset;'*)
(*$HPPEMIT '            WORD    SubstituteNameLength;'*)
(*$HPPEMIT '            WORD    PrintNameOffset;'*)
(*$HPPEMIT '            WORD    PrintNameLength;'*)
(*$HPPEMIT '            WCHAR   PathBuffer[1];'*)
(*$HPPEMIT '        } SymbolicLinkReparseBuffer;'*)
(*$HPPEMIT ''*)
(*$HPPEMIT '        struct {'*)
(*$HPPEMIT '            WORD    SubstituteNameOffset;'*)
(*$HPPEMIT '            WORD    SubstituteNameLength;'*)
(*$HPPEMIT '            WORD    PrintNameOffset;'*)
(*$HPPEMIT '            WORD    PrintNameLength;'*)
(*$HPPEMIT '            WCHAR   PathBuffer[1];'*)
(*$HPPEMIT '        } MountPointReparseBuffer;'*)
(*$HPPEMIT ''*)
(*$HPPEMIT '        struct {'*)
(*$HPPEMIT '            UCHAR   DataBuffer[1];'*)
(*$HPPEMIT '        } GenericReparseBuffer;'*)
(*$HPPEMIT '    };'*)
(*$HPPEMIT ''*)
(*$HPPEMIT '} REPARSE_DATA_BUFFER, *PREPARSE_DATA_BUFFER;'*)
(*$HPPEMIT ''*)
(*$HPPEMIT '#ifndef REPARSE_DATA_BUFFER_HEADER_SIZE'*)
(*$HPPEMIT '#define REPARSE_DATA_BUFFER_HEADER_SIZE   8'*)
(*$HPPEMIT '#endif'*)
(*$HPPEMIT ''*)
(*$HPPEMIT 'typedef struct _REPARSE_POINT_INFORMATION {'*)
(*$HPPEMIT '        WORD    ReparseDataLength;'*)
(*$HPPEMIT '        WORD    UnparsedNameLength;'*)
(*$HPPEMIT '} REPARSE_POINT_INFORMATION, *PREPARSE_POINT_INFORMATION;'*)
(*$HPPEMIT ''*)
(*$HPPEMIT '#ifndef IO_REPARSE_TAG_VALID_VALUES'*)
(*$HPPEMIT '#define IO_REPARSE_TAG_VALID_VALUES 0x0E000FFFF'*)
(*$HPPEMIT '#endif'*)
(*$HPPEMIT ''*)

type
  {$EXTERNALSYM _REPARSE_DATA_BUFFER}
  _REPARSE_DATA_BUFFER = record
    ReparseTag: DWORD;
    ReparseDataLength: Word;
    Reserved: Word;
    case Integer of
      0: ( // SymbolicLinkReparseBuffer and MountPointReparseBuffer
        SubstituteNameOffset: Word;
        SubstituteNameLength: Word;
        PrintNameOffset: Word;
        PrintNameLength: Word;
        PathBuffer: array [0..0] of WCHAR);
      1: ( // GenericReparseBuffer
        DataBuffer: array [0..0] of Byte);
  end;
  {$EXTERNALSYM REPARSE_DATA_BUFFER}
  REPARSE_DATA_BUFFER = _REPARSE_DATA_BUFFER;
  {$EXTERNALSYM PREPARSE_DATA_BUFFER}
  PREPARSE_DATA_BUFFER = ^_REPARSE_DATA_BUFFER;
  TReparseDataBuffer = _REPARSE_DATA_BUFFER;
  PReparseDataBuffer = PREPARSE_DATA_BUFFER;

const
  {$EXTERNALSYM REPARSE_DATA_BUFFER_HEADER_SIZE}
  REPARSE_DATA_BUFFER_HEADER_SIZE = 8;

type
  {$EXTERNALSYM _REPARSE_POINT_INFORMATION}
  _REPARSE_POINT_INFORMATION = record
    ReparseDataLength: Word;
    UnparsedNameLength: Word;
  end;
  {$EXTERNALSYM REPARSE_POINT_INFORMATION}
  REPARSE_POINT_INFORMATION = _REPARSE_POINT_INFORMATION;
  {$EXTERNALSYM PREPARSE_POINT_INFORMATION}
  PREPARSE_POINT_INFORMATION = ^_REPARSE_POINT_INFORMATION;
  TReparsePointInformation = _REPARSE_POINT_INFORMATION;
  PReparsePointInformation = PREPARSE_POINT_INFORMATION;

const
  {$EXTERNALSYM IO_REPARSE_TAG_VALID_VALUES}
  IO_REPARSE_TAG_VALID_VALUES = DWORD($E000FFFF);
{$ENDIF ~CLR}

//==================================================================================================

// from JwaWinNT.pas (few declarations from JwaWinType)

type
  ULONGLONG = Int64;
  {$EXTERNALSYM ULONGLONG}

const
  MAXLONGLONG = $7fffffffffffffff;
  {$EXTERNALSYM MAXLONGLONG}

type
  PLONGLONG = ^LONGLONG;
  {$EXTERNALSYM PLONGLONG}
  PULONGLONG = ^ULONGLONG;
  {$EXTERNALSYM PULONGLONG}

const
  ANYSIZE_ARRAY = 1;
  {$EXTERNALSYM ANYSIZE_ARRAY}

  MAX_NATURAL_ALIGNMENT = SizeOf(ULONG);
  {$EXTERNALSYM MAX_NATURAL_ALIGNMENT}

// line 72

const
  VER_SERVER_NT                      = DWORD($80000000);
  {$EXTERNALSYM VER_SERVER_NT}
  VER_WORKSTATION_NT                 = $40000000;
  {$EXTERNALSYM VER_WORKSTATION_NT}
  VER_SUITE_SMALLBUSINESS            = $00000001;
  {$EXTERNALSYM VER_SUITE_SMALLBUSINESS}
  VER_SUITE_ENTERPRISE               = $00000002;
  {$EXTERNALSYM VER_SUITE_ENTERPRISE}
  VER_SUITE_BACKOFFICE               = $00000004;
  {$EXTERNALSYM VER_SUITE_BACKOFFICE}
  VER_SUITE_COMMUNICATIONS           = $00000008;
  {$EXTERNALSYM VER_SUITE_COMMUNICATIONS}
  VER_SUITE_TERMINAL                 = $00000010;
  {$EXTERNALSYM VER_SUITE_TERMINAL}
  VER_SUITE_SMALLBUSINESS_RESTRICTED = $00000020;
  {$EXTERNALSYM VER_SUITE_SMALLBUSINESS_RESTRICTED}
  VER_SUITE_EMBEDDEDNT               = $00000040;
  {$EXTERNALSYM VER_SUITE_EMBEDDEDNT}
  VER_SUITE_DATACENTER               = $00000080;
  {$EXTERNALSYM VER_SUITE_DATACENTER}
  VER_SUITE_SINGLEUSERTS             = $00000100;
  {$EXTERNALSYM VER_SUITE_SINGLEUSERTS}
  VER_SUITE_PERSONAL                 = $00000200;
  {$EXTERNALSYM VER_SUITE_PERSONAL}
  VER_SUITE_BLADE                    = $00000400;
  {$EXTERNALSYM VER_SUITE_BLADE}
  VER_SUITE_EMBEDDED_RESTRICTED      = $00000800;
  {$EXTERNALSYM VER_SUITE_EMBEDDED_RESTRICTED}
  VER_SUITE_SECURITY_APPLIANCE       = $00001000;
  {$EXTERNALSYM VER_SUITE_SECURITY_APPLIANCE}
  VER_SUITE_STORAGE_SERVER           = $00002000;
  {$EXTERNALSYM VER_SUITE_STORAGE_SERVER}
  VER_SUITE_COMPUTE_SERVER           = $00004000;
  {$EXTERNALSYM VER_SUITE_COMPUTE_SERVER}

// line 515

//
//  A language ID is a 16 bit value which is the combination of a
//  primary language ID and a secondary language ID.  The bits are
//  allocated as follows:
//
//       +-----------------------+-------------------------+
//       |     Sublanguage ID    |   Primary Language ID   |
//       +-----------------------+-------------------------+
//        15                   10 9                       0   bit
//
//
//  Language ID creation/extraction macros:
//
//    MAKELANGID    - construct language id from a primary language id and
//                    a sublanguage id.
//    PRIMARYLANGID - extract primary language id from a language id.
//    SUBLANGID     - extract sublanguage id from a language id.
//

function MAKELANGID(PrimaryLang, SubLang: USHORT): WORD;
{$EXTERNALSYM MAKELANGID}
function PRIMARYLANGID(LangId: WORD): WORD;
{$EXTERNALSYM PRIMARYLANGID}
function SUBLANGID(LangId: WORD): WORD;
{$EXTERNALSYM SUBLANGID}

//
//  A locale ID is a 32 bit value which is the combination of a
//  language ID, a sort ID, and a reserved area.  The bits are
//  allocated as follows:
//
//       +-------------+---------+-------------------------+
//       |   Reserved  | Sort ID |      Language ID        |
//       +-------------+---------+-------------------------+
//        31         20 19     16 15                      0   bit
//
//
//  Locale ID creation/extraction macros:
//
//    MAKELCID            - construct the locale id from a language id and a sort id.
//    MAKESORTLCID        - construct the locale id from a language id, sort id, and sort version.
//    LANGIDFROMLCID      - extract the language id from a locale id.
//    SORTIDFROMLCID      - extract the sort id from a locale id.
//    SORTVERSIONFROMLCID - extract the sort version from a locale id.
//

const
  NLS_VALID_LOCALE_MASK = $000fffff;
  {$EXTERNALSYM NLS_VALID_LOCALE_MASK}

function MAKELCID(LangId, SortId: WORD): DWORD;
{$EXTERNALSYM MAKELCID}
function MAKESORTLCID(LangId, SortId, SortVersion: WORD): DWORD;
{$EXTERNALSYM MAKESORTLCID}
function LANGIDFROMLCID(LocaleId: LCID): WORD;
{$EXTERNALSYM LANGIDFROMLCID}
function SORTIDFROMLCID(LocaleId: LCID): WORD;
{$EXTERNALSYM SORTIDFROMLCID}
function SORTVERSIONFROMLCID(LocaleId: LCID): WORD;
{$EXTERNALSYM SORTVERSIONFROMLCID}

// line 1154

////////////////////////////////////////////////////////////////////////
//                                                                    //
//              Security Id     (SID)                                 //
//                                                                    //
////////////////////////////////////////////////////////////////////////
//
//
// Pictorially the structure of an SID is as follows:
//
//         1   1   1   1   1   1
//         5   4   3   2   1   0   9   8   7   6   5   4   3   2   1   0
//      +---------------------------------------------------------------+
//      |      SubAuthorityCount        |Reserved1 (SBZ)|   Revision    |
//      +---------------------------------------------------------------+
//      |                   IdentifierAuthority[0]                      |
//      +---------------------------------------------------------------+
//      |                   IdentifierAuthority[1]                      |
//      +---------------------------------------------------------------+
//      |                   IdentifierAuthority[2]                      |
//      +---------------------------------------------------------------+
//      |                                                               |
//      +- -  -  -  -  -  -  -  SubAuthority[]  -  -  -  -  -  -  -  - -+
//      |                                                               |
//      +---------------------------------------------------------------+
//
//

type
  _SID_IDENTIFIER_AUTHORITY = record
    Value: array [0..5] of Byte;
  end;
  {$EXTERNALSYM _SID_IDENTIFIER_AUTHORITY}
  SID_IDENTIFIER_AUTHORITY = _SID_IDENTIFIER_AUTHORITY;
  {$EXTERNALSYM SID_IDENTIFIER_AUTHORITY}
  PSID_IDENTIFIER_AUTHORITY = ^_SID_IDENTIFIER_AUTHORITY;
  {$EXTERNALSYM PSID_IDENTIFIER_AUTHORITY}

  // PSid = ^SID;
  _SID = record
    Revision: Byte;
    SubAuthorityCount: Byte;
    IdentifierAuthority: SID_IDENTIFIER_AUTHORITY;
    SubAuthority: array [0..ANYSIZE_ARRAY - 1] of DWORD;
  end;
  {$EXTERNALSYM _SID}
  SID = _SID;
  {$EXTERNALSYM SID}
  PPSID = ^PSID;
  {$NODEFINE PPSID}
  TSid = SID;

const
  SID_REVISION                    = (1); // Current revision level
  {$EXTERNALSYM SID_REVISION}
  SID_MAX_SUB_AUTHORITIES         = (15);
  {$EXTERNALSYM SID_MAX_SUB_AUTHORITIES}
  SID_RECOMMENDED_SUB_AUTHORITIES = (1); // Will change to around 6 in a future release.
  {$EXTERNALSYM SID_RECOMMENDED_SUB_AUTHORITIES}

  {$IFNDEF CLR}
  SECURITY_MAX_SID_SIZE = SizeOf(SID) - SizeOf(DWORD) + (SID_MAX_SUB_AUTHORITIES * SizeOf(DWORD));
  {$EXTERNALSYM SECURITY_MAX_SID_SIZE}
  {$ENDIF ~CLR}

{$IFNDEF FPC}
  SidTypeUser           = 1;
  {$EXTERNALSYM SidTypeUser}
  SidTypeGroup          = 2;
  {$EXTERNALSYM SidTypeGroup}
  SidTypeDomain         = 3;
  {$EXTERNALSYM SidTypeDomain}
  SidTypeAlias          = 4;
  {$EXTERNALSYM SidTypeAlias}
  SidTypeWellKnownGroup = 5;
  {$EXTERNALSYM SidTypeWellKnownGroup}
  SidTypeDeletedAccount = 6;
  {$EXTERNALSYM SidTypeDeletedAccount}
  SidTypeInvalid        = 7;
  {$EXTERNALSYM SidTypeInvalid}
  SidTypeUnknown        = 8;
  {$EXTERNALSYM SidTypeUnknown}
  SidTypeComputer       = 9;
  {$EXTERNALSYM SidTypeComputer}
{$ENDIF ~FPC}

type
  _SID_NAME_USE = DWORD;
  {$EXTERNALSYM _SID_NAME_USE}
//  SID_NAME_USE = _SID_NAME_USE;
//  {$EXTERNALSYM SID_NAME_USE}
  PSID_NAME_USE = ^SID_NAME_USE;
  {$EXTERNALSYM PSID_NAME_USE}
  TSidNameUse = SID_NAME_USE;
  PSidNameUSe = PSID_NAME_USE;

  PSID_AND_ATTRIBUTES = ^SID_AND_ATTRIBUTES;
  {$EXTERNALSYM PSID_AND_ATTRIBUTES}
  _SID_AND_ATTRIBUTES = record
    Sid: PSID;
    Attributes: DWORD;
  end;
  {$EXTERNALSYM _SID_AND_ATTRIBUTES}
  SID_AND_ATTRIBUTES = _SID_AND_ATTRIBUTES;
  {$EXTERNALSYM SID_AND_ATTRIBUTES}
  TSidAndAttributes = SID_AND_ATTRIBUTES;
  PSidAndAttributes = PSID_AND_ATTRIBUTES;

  SID_AND_ATTRIBUTES_ARRAY = array [0..ANYSIZE_ARRAY - 1] of SID_AND_ATTRIBUTES;
  {$EXTERNALSYM SID_AND_ATTRIBUTES_ARRAY}
  PSID_AND_ATTRIBUTES_ARRAY = ^SID_AND_ATTRIBUTES_ARRAY;
  {$EXTERNALSYM PSID_AND_ATTRIBUTES_ARRAY}
  PSidAndAttributesArray = ^TSidAndAttributesArray;
  TSidAndAttributesArray = SID_AND_ATTRIBUTES_ARRAY;

/////////////////////////////////////////////////////////////////////////////
//                                                                         //
// Universal well-known SIDs                                               //
//                                                                         //
//     Null SID                     S-1-0-0                                //
//     World                        S-1-1-0                                //
//     Local                        S-1-2-0                                //
//     Creator Owner ID             S-1-3-0                                //
//     Creator Group ID             S-1-3-1                                //
//     Creator Owner Server ID      S-1-3-2                                //
//     Creator Group Server ID      S-1-3-3                                //
//                                                                         //
//     (Non-unique IDs)             S-1-4                                  //
//                                                                         //
/////////////////////////////////////////////////////////////////////////////

const
  SECURITY_NULL_SID_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 0));
  {$EXTERNALSYM SECURITY_NULL_SID_AUTHORITY}
  SECURITY_WORLD_SID_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 1));
  {$EXTERNALSYM SECURITY_WORLD_SID_AUTHORITY}
  SECURITY_LOCAL_SID_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 2));
  {$EXTERNALSYM SECURITY_LOCAL_SID_AUTHORITY}
  SECURITY_CREATOR_SID_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 3));
  {$EXTERNALSYM SECURITY_CREATOR_SID_AUTHORITY}
  SECURITY_NON_UNIQUE_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 4));
  {$EXTERNALSYM SECURITY_NON_UNIQUE_AUTHORITY}
  SECURITY_RESOURCE_MANAGER_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 9));
  {$EXTERNALSYM SECURITY_RESOURCE_MANAGER_AUTHORITY}

  SECURITY_NULL_RID                 = ($00000000);
  {$EXTERNALSYM SECURITY_NULL_RID}
  SECURITY_WORLD_RID                = ($00000000);
  {$EXTERNALSYM SECURITY_WORLD_RID}
  SECURITY_LOCAL_RID                = ($00000000);
  {$EXTERNALSYM SECURITY_LOCAL_RID}

  SECURITY_CREATOR_OWNER_RID        = ($00000000);
  {$EXTERNALSYM SECURITY_CREATOR_OWNER_RID}
  SECURITY_CREATOR_GROUP_RID        = ($00000001);
  {$EXTERNALSYM SECURITY_CREATOR_GROUP_RID}

  SECURITY_CREATOR_OWNER_SERVER_RID = ($00000002);
  {$EXTERNALSYM SECURITY_CREATOR_OWNER_SERVER_RID}
  SECURITY_CREATOR_GROUP_SERVER_RID = ($00000003);
  {$EXTERNALSYM SECURITY_CREATOR_GROUP_SERVER_RID}

/////////////////////////////////////////////////////////////////////////////
//                                                                         //
// NT well-known SIDs                                                        //
//                                                                           //
//     NT Authority            S-1-5                                         //
//     Dialup                  S-1-5-1                                       //
//                                                                           //
//     Network                 S-1-5-2                                       //
//     Batch                   S-1-5-3                                       //
//     Interactive             S-1-5-4                                       //
//     (Logon IDs)             S-1-5-5-X-Y                                   //
//     Service                 S-1-5-6                                       //
//     AnonymousLogon          S-1-5-7       (aka null logon session)        //
//     Proxy                   S-1-5-8                                       //
//     Enterprise DC (EDC)     S-1-5-9       (aka domain controller account) //
//     Self                    S-1-5-10      (self RID)                      //
//     Authenticated User      S-1-5-11      (Authenticated user somewhere)  //
//     Restricted Code         S-1-5-12      (Running restricted code)       //
//     Terminal Server         S-1-5-13      (Running on Terminal Server)    //
//     Remote Logon            S-1-5-14      (Remote Interactive Logon)      //
//     This Organization       S-1-5-15                                      //
//                                                                           //
//     Local System            S-1-5-18                                      //
//     Local Service           S-1-5-19                                      //
//     Network Service         S-1-5-20                                      //
//                                                                           //
//     (NT non-unique IDs)     S-1-5-0x15-... (NT Domain Sids)               //
//                                                                           //
//     (Built-in domain)       S-1-5-0x20                                    //
//                                                                           //
//     (Security Package IDs)  S-1-5-0x40                                    //
//     NTLM Authentication     S-1-5-0x40-10                                 //
//     SChannel Authentication S-1-5-0x40-14                                 //
//     Digest Authentication   S-1-5-0x40-21                                 //
//                                                                           //
//     Other Organization      S-1-5-1000    (>=1000 can not be filtered)    //
//                                                                           //
//                                                                           //
// NOTE: the relative identifier values (RIDs) determine which security      //
//       boundaries the SID is allowed to cross.  Before adding new RIDs,    //
//       a determination needs to be made regarding which range they should  //
//       be added to in order to ensure proper "SID filtering"               //
//                                                                         //
/////////////////////////////////////////////////////////////////////////////

const
  SECURITY_NT_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  {$EXTERNALSYM SECURITY_NT_AUTHORITY}

  SECURITY_DIALUP_RID                 = ($00000001);
  {$EXTERNALSYM SECURITY_DIALUP_RID}
  SECURITY_NETWORK_RID                = ($00000002);
  {$EXTERNALSYM SECURITY_NETWORK_RID}
  SECURITY_BATCH_RID                  = ($00000003);
  {$EXTERNALSYM SECURITY_BATCH_RID}
  SECURITY_INTERACTIVE_RID            = ($00000004);
  {$EXTERNALSYM SECURITY_INTERACTIVE_RID}
  SECURITY_LOGON_IDS_RID              = ($00000005);
  {$EXTERNALSYM SECURITY_LOGON_IDS_RID}
  SECURITY_LOGON_IDS_RID_COUNT        = (3);
  {$EXTERNALSYM SECURITY_LOGON_IDS_RID_COUNT}
  SECURITY_SERVICE_RID                = ($00000006);
  {$EXTERNALSYM SECURITY_SERVICE_RID}
  SECURITY_ANONYMOUS_LOGON_RID        = ($00000007);
  {$EXTERNALSYM SECURITY_ANONYMOUS_LOGON_RID}
  SECURITY_PROXY_RID                  = ($00000008);
  {$EXTERNALSYM SECURITY_PROXY_RID}
  SECURITY_ENTERPRISE_CONTROLLERS_RID = ($00000009);
  {$EXTERNALSYM SECURITY_ENTERPRISE_CONTROLLERS_RID}
  SECURITY_SERVER_LOGON_RID           = SECURITY_ENTERPRISE_CONTROLLERS_RID;
  {$EXTERNALSYM SECURITY_SERVER_LOGON_RID}
  SECURITY_PRINCIPAL_SELF_RID         = ($0000000A);
  {$EXTERNALSYM SECURITY_PRINCIPAL_SELF_RID}
  SECURITY_AUTHENTICATED_USER_RID     = ($0000000B);
  {$EXTERNALSYM SECURITY_AUTHENTICATED_USER_RID}
  SECURITY_RESTRICTED_CODE_RID        = ($0000000C);
  {$EXTERNALSYM SECURITY_RESTRICTED_CODE_RID}
  SECURITY_TERMINAL_SERVER_RID        = ($0000000D);
  {$EXTERNALSYM SECURITY_TERMINAL_SERVER_RID}
  SECURITY_REMOTE_LOGON_RID           = ($0000000E);
  {$EXTERNALSYM SECURITY_REMOTE_LOGON_RID}
  SECURITY_THIS_ORGANIZATION_RID      = ($0000000F);
  {$EXTERNALSYM SECURITY_THIS_ORGANIZATION_RID}

  SECURITY_LOCAL_SYSTEM_RID    = ($00000012);
  {$EXTERNALSYM SECURITY_LOCAL_SYSTEM_RID}
  SECURITY_LOCAL_SERVICE_RID   = ($00000013);
  {$EXTERNALSYM SECURITY_LOCAL_SERVICE_RID}
  SECURITY_NETWORK_SERVICE_RID = ($00000014);
  {$EXTERNALSYM SECURITY_NETWORK_SERVICE_RID}

  SECURITY_NT_NON_UNIQUE       = ($00000015);
  {$EXTERNALSYM SECURITY_NT_NON_UNIQUE}
  SECURITY_NT_NON_UNIQUE_SUB_AUTH_COUNT = (3);
  {$EXTERNALSYM SECURITY_NT_NON_UNIQUE_SUB_AUTH_COUNT}

  SECURITY_BUILTIN_DOMAIN_RID  = ($00000020);
  {$EXTERNALSYM SECURITY_BUILTIN_DOMAIN_RID}

  SECURITY_PACKAGE_BASE_RID       = ($00000040);
  {$EXTERNALSYM SECURITY_PACKAGE_BASE_RID}
  SECURITY_PACKAGE_RID_COUNT      = (2);
  {$EXTERNALSYM SECURITY_PACKAGE_RID_COUNT}
  SECURITY_PACKAGE_NTLM_RID       = ($0000000A);
  {$EXTERNALSYM SECURITY_PACKAGE_NTLM_RID}
  SECURITY_PACKAGE_SCHANNEL_RID   = ($0000000E);
  {$EXTERNALSYM SECURITY_PACKAGE_SCHANNEL_RID}
  SECURITY_PACKAGE_DIGEST_RID     = ($00000015);
  {$EXTERNALSYM SECURITY_PACKAGE_DIGEST_RID}

  SECURITY_MAX_ALWAYS_FILTERED    = ($000003E7);
  {$EXTERNALSYM SECURITY_MAX_ALWAYS_FILTERED}
  SECURITY_MIN_NEVER_FILTERED     = ($000003E8);
  {$EXTERNALSYM SECURITY_MIN_NEVER_FILTERED}

  SECURITY_OTHER_ORGANIZATION_RID = ($000003E8);
  {$EXTERNALSYM SECURITY_OTHER_ORGANIZATION_RID}

/////////////////////////////////////////////////////////////////////////////
//                                                                         //
// well-known domain relative sub-authority values (RIDs)...               //
//                                                                         //
/////////////////////////////////////////////////////////////////////////////

// Well-known users ...

  FOREST_USER_RID_MAX    = ($000001F3);
  {$EXTERNALSYM FOREST_USER_RID_MAX}

  DOMAIN_USER_RID_ADMIN  = ($000001F4);
  {$EXTERNALSYM DOMAIN_USER_RID_ADMIN}
  DOMAIN_USER_RID_GUEST  = ($000001F5);
  {$EXTERNALSYM DOMAIN_USER_RID_GUEST}
  DOMAIN_USER_RID_KRBTGT = ($000001F6);
  {$EXTERNALSYM DOMAIN_USER_RID_KRBTGT}

  DOMAIN_USER_RID_MAX    = ($000003E7);
  {$EXTERNALSYM DOMAIN_USER_RID_MAX}

// well-known groups ...

  DOMAIN_GROUP_RID_ADMINS            = ($00000200);
  {$EXTERNALSYM DOMAIN_GROUP_RID_ADMINS}
  DOMAIN_GROUP_RID_USERS             = ($00000201);
  {$EXTERNALSYM DOMAIN_GROUP_RID_USERS}
  DOMAIN_GROUP_RID_GUESTS            = ($00000202);
  {$EXTERNALSYM DOMAIN_GROUP_RID_GUESTS}
  DOMAIN_GROUP_RID_COMPUTERS         = ($00000203);
  {$EXTERNALSYM DOMAIN_GROUP_RID_COMPUTERS}
  DOMAIN_GROUP_RID_CONTROLLERS       = ($00000204);
  {$EXTERNALSYM DOMAIN_GROUP_RID_CONTROLLERS}
  DOMAIN_GROUP_RID_CERT_ADMINS       = ($00000205);
  {$EXTERNALSYM DOMAIN_GROUP_RID_CERT_ADMINS}
  DOMAIN_GROUP_RID_SCHEMA_ADMINS     = ($00000206);
  {$EXTERNALSYM DOMAIN_GROUP_RID_SCHEMA_ADMINS}
  DOMAIN_GROUP_RID_ENTERPRISE_ADMINS = ($00000207);
  {$EXTERNALSYM DOMAIN_GROUP_RID_ENTERPRISE_ADMINS}
  DOMAIN_GROUP_RID_POLICY_ADMINS     = ($00000208);
  {$EXTERNALSYM DOMAIN_GROUP_RID_POLICY_ADMINS}

// well-known aliases ...

  DOMAIN_ALIAS_RID_ADMINS           = ($00000220);
  {$EXTERNALSYM DOMAIN_ALIAS_RID_ADMINS}
  DOMAIN_ALIAS_RID_USERS            = ($00000221);
  {$EXTERNALSYM DOMAIN_ALIAS_RID_USERS}
  DOMAIN_ALIAS_RID_GUESTS           = ($00000222);
  {$EXTERNALSYM DOMAIN_ALIAS_RID_GUESTS}
  DOMAIN_ALIAS_RID_POWER_USERS      = ($00000223);
  {$EXTERNALSYM DOMAIN_ALIAS_RID_POWER_USERS}

  DOMAIN_ALIAS_RID_ACCOUNT_OPS      = ($00000224);
  {$EXTERNALSYM DOMAIN_ALIAS_RID_ACCOUNT_OPS}
  DOMAIN_ALIAS_RID_SYSTEM_OPS       = ($00000225);
  {$EXTERNALSYM DOMAIN_ALIAS_RID_SYSTEM_OPS}
  DOMAIN_ALIAS_RID_PRINT_OPS        = ($00000226);
  {$EXTERNALSYM DOMAIN_ALIAS_RID_PRINT_OPS}
  DOMAIN_ALIAS_RID_BACKUP_OPS       = ($00000227);
  {$EXTERNALSYM DOMAIN_ALIAS_RID_BACKUP_OPS}

  DOMAIN_ALIAS_RID_REPLICATOR       = ($00000228);
  {$EXTERNALSYM DOMAIN_ALIAS_RID_REPLICATOR}
  DOMAIN_ALIAS_RID_RAS_SERVERS      = ($00000229);
  {$EXTERNALSYM DOMAIN_ALIAS_RID_RAS_SERVERS}
  DOMAIN_ALIAS_RID_PREW2KCOMPACCESS = ($0000022A);
  {$EXTERNALSYM DOMAIN_ALIAS_RID_PREW2KCOMPACCESS}
  DOMAIN_ALIAS_RID_REMOTE_DESKTOP_USERS = ($0000022B);
  {$EXTERNALSYM DOMAIN_ALIAS_RID_REMOTE_DESKTOP_USERS}
  DOMAIN_ALIAS_RID_NETWORK_CONFIGURATION_OPS = ($0000022C);
  {$EXTERNALSYM DOMAIN_ALIAS_RID_NETWORK_CONFIGURATION_OPS}
  DOMAIN_ALIAS_RID_INCOMING_FOREST_TRUST_BUILDERS = ($0000022D);
  {$EXTERNALSYM DOMAIN_ALIAS_RID_INCOMING_FOREST_TRUST_BUILDERS}

  DOMAIN_ALIAS_RID_MONITORING_USERS      = ($0000022E);
  {$EXTERNALSYM DOMAIN_ALIAS_RID_MONITORING_USERS}
  DOMAIN_ALIAS_RID_LOGGING_USERS         = ($0000022F);
  {$EXTERNALSYM DOMAIN_ALIAS_RID_LOGGING_USERS}
  DOMAIN_ALIAS_RID_AUTHORIZATIONACCESS   = ($00000230);
  {$EXTERNALSYM DOMAIN_ALIAS_RID_AUTHORIZATIONACCESS}
  DOMAIN_ALIAS_RID_TS_LICENSE_SERVERS    = ($00000231);
  {$EXTERNALSYM DOMAIN_ALIAS_RID_TS_LICENSE_SERVERS}

// line 2495

////////////////////////////////////////////////////////////////////////
//                                                                    //
//               NT Defined Privileges                                //
//                                                                    //
////////////////////////////////////////////////////////////////////////

const
  SE_CREATE_TOKEN_NAME        = 'SeCreateTokenPrivilege';
  {$EXTERNALSYM SE_CREATE_TOKEN_NAME}
  SE_ASSIGNPRIMARYTOKEN_NAME  = 'SeAssignPrimaryTokenPrivilege';
  {$EXTERNALSYM SE_ASSIGNPRIMARYTOKEN_NAME}
  SE_LOCK_MEMORY_NAME         = 'SeLockMemoryPrivilege';
  {$EXTERNALSYM SE_LOCK_MEMORY_NAME}
  SE_INCREASE_QUOTA_NAME      = 'SeIncreaseQuotaPrivilege';
  {$EXTERNALSYM SE_INCREASE_QUOTA_NAME}
  SE_UNSOLICITED_INPUT_NAME   = 'SeUnsolicitedInputPrivilege';
  {$EXTERNALSYM SE_UNSOLICITED_INPUT_NAME}
  SE_MACHINE_ACCOUNT_NAME     = 'SeMachineAccountPrivilege';
  {$EXTERNALSYM SE_MACHINE_ACCOUNT_NAME}
  SE_TCB_NAME                 = 'SeTcbPrivilege';
  {$EXTERNALSYM SE_TCB_NAME}
  SE_SECURITY_NAME            = 'SeSecurityPrivilege';
  {$EXTERNALSYM SE_SECURITY_NAME}
  SE_TAKE_OWNERSHIP_NAME      = 'SeTakeOwnershipPrivilege';
  {$EXTERNALSYM SE_TAKE_OWNERSHIP_NAME}
  SE_LOAD_DRIVER_NAME         = 'SeLoadDriverPrivilege';
  {$EXTERNALSYM SE_LOAD_DRIVER_NAME}
  SE_SYSTEM_PROFILE_NAME      = 'SeSystemProfilePrivilege';
  {$EXTERNALSYM SE_SYSTEM_PROFILE_NAME}
  SE_SYSTEMTIME_NAME          = 'SeSystemtimePrivilege';
  {$EXTERNALSYM SE_SYSTEMTIME_NAME}
  SE_PROF_SINGLE_PROCESS_NAME = 'SeProfileSingleProcessPrivilege';
  {$EXTERNALSYM SE_PROF_SINGLE_PROCESS_NAME}
  SE_INC_BASE_PRIORITY_NAME   = 'SeIncreaseBasePriorityPrivilege';
  {$EXTERNALSYM SE_INC_BASE_PRIORITY_NAME}
  SE_CREATE_PAGEFILE_NAME     = 'SeCreatePagefilePrivilege';
  {$EXTERNALSYM SE_CREATE_PAGEFILE_NAME}
  SE_CREATE_PERMANENT_NAME    = 'SeCreatePermanentPrivilege';
  {$EXTERNALSYM SE_CREATE_PERMANENT_NAME}
  SE_BACKUP_NAME              = 'SeBackupPrivilege';
  {$EXTERNALSYM SE_BACKUP_NAME}
  SE_RESTORE_NAME             = 'SeRestorePrivilege';
  {$EXTERNALSYM SE_RESTORE_NAME}
  SE_SHUTDOWN_NAME            = 'SeShutdownPrivilege';
  {$EXTERNALSYM SE_SHUTDOWN_NAME}
  SE_DEBUG_NAME               = 'SeDebugPrivilege';
  {$EXTERNALSYM SE_DEBUG_NAME}
  SE_AUDIT_NAME               = 'SeAuditPrivilege';
  {$EXTERNALSYM SE_AUDIT_NAME}
  SE_SYSTEM_ENVIRONMENT_NAME  = 'SeSystemEnvironmentPrivilege';
  {$EXTERNALSYM SE_SYSTEM_ENVIRONMENT_NAME}
  SE_CHANGE_NOTIFY_NAME       = 'SeChangeNotifyPrivilege';
  {$EXTERNALSYM SE_CHANGE_NOTIFY_NAME}
  SE_REMOTE_SHUTDOWN_NAME     = 'SeRemoteShutdownPrivilege';
  {$EXTERNALSYM SE_REMOTE_SHUTDOWN_NAME}
  SE_UNDOCK_NAME              = 'SeUndockPrivilege';
  {$EXTERNALSYM SE_UNDOCK_NAME}
  SE_SYNC_AGENT_NAME          = 'SeSyncAgentPrivilege';
  {$EXTERNALSYM SE_SYNC_AGENT_NAME}
  SE_ENABLE_DELEGATION_NAME   = 'SeEnableDelegationPrivilege';
  {$EXTERNALSYM SE_ENABLE_DELEGATION_NAME}
  SE_MANAGE_VOLUME_NAME       = 'SeManageVolumePrivilege';
  {$EXTERNALSYM SE_MANAGE_VOLUME_NAME}
  SE_IMPERSONATE_NAME         = 'SeImpersonatePrivilege';
  {$EXTERNALSYM SE_IMPERSONATE_NAME}
  SE_CREATE_GLOBAL_NAME       = 'SeCreateGlobalPrivilege';
  {$EXTERNALSYM SE_CREATE_GLOBAL_NAME}

//
// Thread Information Block (TIB)
//

type
  NT_TIB32 = packed record
    ExceptionList: DWORD;
    StackBase: DWORD;
    StackLimit: DWORD;
    SubSystemTib: DWORD;
    case Integer of
      0 : (
        FiberData: DWORD;
        ArbitraryUserPointer: DWORD;
        Self: DWORD;
      );
      1 : (
        Version: DWORD;
      );
  end;
  {$EXTERNALSYM NT_TIB32}
  PNT_TIB32 = ^NT_TIB32;
  {$EXTERNALSYM PNT_TIB32}

  NT_TIB64 = packed record
    ExceptionList: TJclAddr64;
    StackBase: TJclAddr64;
    StackLimit: TJclAddr64;
    SubSystemTib: TJclAddr64;
    case Integer of
      0 : (
        FiberData: TJclAddr64;
        ArbitraryUserPointer: TJclAddr64;
        Self: TJclAddr64;
      );
      1 : (
        Version: DWORD;
      );
  end;
  {$EXTERNALSYM NT_TIB64}
  PNT_TIB64 = ^NT_TIB64;
  {$EXTERNALSYM PNT_TIB64}

// line 2686

//
// Token information class structures
//

type
  PTOKEN_USER = ^TOKEN_USER;
  {$EXTERNALSYM PTOKEN_USER}
  _TOKEN_USER = record
    User: SID_AND_ATTRIBUTES;
  end;
  {$EXTERNALSYM _TOKEN_USER}
  TOKEN_USER = _TOKEN_USER;
  {$EXTERNALSYM TOKEN_USER}
  TTokenUser = TOKEN_USER;
  PTokenUser = PTOKEN_USER;

// line 3858

//
// Define access rights to files and directories
//

//
// The FILE_READ_DATA and FILE_WRITE_DATA constants are also defined in
// devioctl.h as FILE_READ_ACCESS and FILE_WRITE_ACCESS. The values for these
// constants *MUST* always be in sync.
// The values are redefined in devioctl.h because they must be available to
// both DOS and NT.
//

const
  FILE_READ_DATA            = ($0001); // file & pipe
  {$EXTERNALSYM FILE_READ_DATA}
  FILE_LIST_DIRECTORY       = ($0001); // directory
  {$EXTERNALSYM FILE_LIST_DIRECTORY}

  FILE_WRITE_DATA           = ($0002); // file & pipe
  {$EXTERNALSYM FILE_WRITE_DATA}
  FILE_ADD_FILE             = ($0002); // directory
  {$EXTERNALSYM FILE_ADD_FILE}

  FILE_APPEND_DATA          = ($0004); // file
  {$EXTERNALSYM FILE_APPEND_DATA}
  FILE_ADD_SUBDIRECTORY     = ($0004); // directory
  {$EXTERNALSYM FILE_ADD_SUBDIRECTORY}
  FILE_CREATE_PIPE_INSTANCE = ($0004); // named pipe
  {$EXTERNALSYM FILE_CREATE_PIPE_INSTANCE}

  FILE_READ_EA = ($0008); // file & directory
  {$EXTERNALSYM FILE_READ_EA}

  FILE_WRITE_EA = ($0010); // file & directory
  {$EXTERNALSYM FILE_WRITE_EA}

  FILE_EXECUTE = ($0020); // file
  {$EXTERNALSYM FILE_EXECUTE}
  FILE_TRAVERSE = ($0020); // directory
  {$EXTERNALSYM FILE_TRAVERSE}

  FILE_DELETE_CHILD = ($0040); // directory
  {$EXTERNALSYM FILE_DELETE_CHILD}

  FILE_READ_ATTRIBUTES = ($0080); // all
  {$EXTERNALSYM FILE_READ_ATTRIBUTES}

  FILE_WRITE_ATTRIBUTES = ($0100); // all
  {$EXTERNALSYM FILE_WRITE_ATTRIBUTES}

  FILE_ALL_ACCESS = (STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $1FF);
  {$EXTERNALSYM FILE_ALL_ACCESS}

  FILE_GENERIC_READ = (STANDARD_RIGHTS_READ or FILE_READ_DATA or
    FILE_READ_ATTRIBUTES or FILE_READ_EA or SYNCHRONIZE);
  {$EXTERNALSYM FILE_GENERIC_READ}

  FILE_GENERIC_WRITE = (STANDARD_RIGHTS_WRITE or FILE_WRITE_DATA or
    FILE_WRITE_ATTRIBUTES or FILE_WRITE_EA or FILE_APPEND_DATA or SYNCHRONIZE);
  {$EXTERNALSYM FILE_GENERIC_WRITE}

  FILE_GENERIC_EXECUTE = (STANDARD_RIGHTS_EXECUTE or FILE_READ_ATTRIBUTES or
    FILE_EXECUTE or SYNCHRONIZE);
  {$EXTERNALSYM FILE_GENERIC_EXECUTE}

  FILE_SHARE_READ                    = $00000001;
  {$EXTERNALSYM FILE_SHARE_READ}
  FILE_SHARE_WRITE                   = $00000002;
  {$EXTERNALSYM FILE_SHARE_WRITE}
  FILE_SHARE_DELETE                  = $00000004;
  {$EXTERNALSYM FILE_SHARE_DELETE}
  FILE_ATTRIBUTE_READONLY            = $00000001;
  {$EXTERNALSYM FILE_ATTRIBUTE_READONLY}
  FILE_ATTRIBUTE_HIDDEN              = $00000002;
  {$EXTERNALSYM FILE_ATTRIBUTE_HIDDEN}
  FILE_ATTRIBUTE_SYSTEM              = $00000004;
  {$EXTERNALSYM FILE_ATTRIBUTE_SYSTEM}
  FILE_ATTRIBUTE_DIRECTORY           = $00000010;
  {$EXTERNALSYM FILE_ATTRIBUTE_DIRECTORY}
  FILE_ATTRIBUTE_ARCHIVE             = $00000020;
  {$EXTERNALSYM FILE_ATTRIBUTE_ARCHIVE}
  FILE_ATTRIBUTE_DEVICE              = $00000040;
  {$EXTERNALSYM FILE_ATTRIBUTE_DEVICE}
  FILE_ATTRIBUTE_NORMAL              = $00000080;
  {$EXTERNALSYM FILE_ATTRIBUTE_NORMAL}
  FILE_ATTRIBUTE_TEMPORARY           = $00000100;
  {$EXTERNALSYM FILE_ATTRIBUTE_TEMPORARY}
  FILE_ATTRIBUTE_SPARSE_FILE         = $00000200;
  {$EXTERNALSYM FILE_ATTRIBUTE_SPARSE_FILE}
  FILE_ATTRIBUTE_REPARSE_POINT       = $00000400;
  {$EXTERNALSYM FILE_ATTRIBUTE_REPARSE_POINT}
  FILE_ATTRIBUTE_COMPRESSED          = $00000800;
  {$EXTERNALSYM FILE_ATTRIBUTE_COMPRESSED}
  FILE_ATTRIBUTE_OFFLINE             = $00001000;
  {$EXTERNALSYM FILE_ATTRIBUTE_OFFLINE}
  FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = $00002000;
  {$EXTERNALSYM FILE_ATTRIBUTE_NOT_CONTENT_INDEXED}
  FILE_ATTRIBUTE_ENCRYPTED           = $00004000;
  {$EXTERNALSYM FILE_ATTRIBUTE_ENCRYPTED}
  FILE_NOTIFY_CHANGE_FILE_NAME       = $00000001;
  {$EXTERNALSYM FILE_NOTIFY_CHANGE_FILE_NAME}
  FILE_NOTIFY_CHANGE_DIR_NAME        = $00000002;
  {$EXTERNALSYM FILE_NOTIFY_CHANGE_DIR_NAME}
  FILE_NOTIFY_CHANGE_ATTRIBUTES      = $00000004;
  {$EXTERNALSYM FILE_NOTIFY_CHANGE_ATTRIBUTES}
  FILE_NOTIFY_CHANGE_SIZE            = $00000008;
  {$EXTERNALSYM FILE_NOTIFY_CHANGE_SIZE}
  FILE_NOTIFY_CHANGE_LAST_WRITE      = $00000010;
  {$EXTERNALSYM FILE_NOTIFY_CHANGE_LAST_WRITE}
  FILE_NOTIFY_CHANGE_LAST_ACCESS     = $00000020;
  {$EXTERNALSYM FILE_NOTIFY_CHANGE_LAST_ACCESS}
  FILE_NOTIFY_CHANGE_CREATION        = $00000040;
  {$EXTERNALSYM FILE_NOTIFY_CHANGE_CREATION}
  FILE_NOTIFY_CHANGE_SECURITY        = $00000100;
  {$EXTERNALSYM FILE_NOTIFY_CHANGE_SECURITY}
  FILE_ACTION_ADDED                  = $00000001;
  {$EXTERNALSYM FILE_ACTION_ADDED}
  FILE_ACTION_REMOVED                = $00000002;
  {$EXTERNALSYM FILE_ACTION_REMOVED}
  FILE_ACTION_MODIFIED               = $00000003;
  {$EXTERNALSYM FILE_ACTION_MODIFIED}
  FILE_ACTION_RENAMED_OLD_NAME       = $00000004;
  {$EXTERNALSYM FILE_ACTION_RENAMED_OLD_NAME}
  FILE_ACTION_RENAMED_NEW_NAME       = $00000005;
  {$EXTERNALSYM FILE_ACTION_RENAMED_NEW_NAME}
  MAILSLOT_NO_MESSAGE                = DWORD(-1);
  {$EXTERNALSYM MAILSLOT_NO_MESSAGE}
  MAILSLOT_WAIT_FOREVER              = DWORD(-1);
  {$EXTERNALSYM MAILSLOT_WAIT_FOREVER}
  FILE_CASE_SENSITIVE_SEARCH         = $00000001;
  {$EXTERNALSYM FILE_CASE_SENSITIVE_SEARCH}
  FILE_CASE_PRESERVED_NAMES          = $00000002;
  {$EXTERNALSYM FILE_CASE_PRESERVED_NAMES}
  FILE_UNICODE_ON_DISK               = $00000004;
  {$EXTERNALSYM FILE_UNICODE_ON_DISK}
  FILE_PERSISTENT_ACLS               = $00000008;
  {$EXTERNALSYM FILE_PERSISTENT_ACLS}
  FILE_FILE_COMPRESSION              = $00000010;
  {$EXTERNALSYM FILE_FILE_COMPRESSION}
  FILE_VOLUME_QUOTAS                 = $00000020;
  {$EXTERNALSYM FILE_VOLUME_QUOTAS}
  FILE_SUPPORTS_SPARSE_FILES         = $00000040;
  {$EXTERNALSYM FILE_SUPPORTS_SPARSE_FILES}
  FILE_SUPPORTS_REPARSE_POINTS       = $00000080;
  {$EXTERNALSYM FILE_SUPPORTS_REPARSE_POINTS}
  FILE_SUPPORTS_REMOTE_STORAGE       = $00000100;
  {$EXTERNALSYM FILE_SUPPORTS_REMOTE_STORAGE}
  FILE_VOLUME_IS_COMPRESSED          = $00008000;
  {$EXTERNALSYM FILE_VOLUME_IS_COMPRESSED}
  FILE_SUPPORTS_OBJECT_IDS           = $00010000;
  {$EXTERNALSYM FILE_SUPPORTS_OBJECT_IDS}
  FILE_SUPPORTS_ENCRYPTION           = $00020000;
  {$EXTERNALSYM FILE_SUPPORTS_ENCRYPTION}
  FILE_NAMED_STREAMS                 = $00040000;
  {$EXTERNALSYM FILE_NAMED_STREAMS}
  FILE_READ_ONLY_VOLUME              = $00080000;
  {$EXTERNALSYM FILE_READ_ONLY_VOLUME}

// line 4052

//
// The reparse GUID structure is used by all 3rd party layered drivers to
// store data in a reparse point. For non-Microsoft tags, The GUID field
// cannot be GUID_NULL.
// The constraints on reparse tags are defined below.
// Microsoft tags can also be used with this format of the reparse point buffer.
//

type
  TGenericReparseBuffer = record
    DataBuffer: array [0..0] of BYTE;
  end;

  PREPARSE_GUID_DATA_BUFFER = ^REPARSE_GUID_DATA_BUFFER;
  {$EXTERNALSYM PREPARSE_GUID_DATA_BUFFER}
  _REPARSE_GUID_DATA_BUFFER = record
    ReparseTag: DWORD;
    ReparseDataLength: WORD;
    Reserved: WORD;
    ReparseGuid: TGUID;
    GenericReparseBuffer: TGenericReparseBuffer;
  end;
  {$EXTERNALSYM _REPARSE_GUID_DATA_BUFFER}
  REPARSE_GUID_DATA_BUFFER = _REPARSE_GUID_DATA_BUFFER;
  {$EXTERNALSYM REPARSE_GUID_DATA_BUFFER}
  TReparseGuidDataBuffer = REPARSE_GUID_DATA_BUFFER;
  PReparseGuidDataBuffer = PREPARSE_GUID_DATA_BUFFER;

const
  REPARSE_GUID_DATA_BUFFER_HEADER_SIZE = 24;
  {$EXTERNALSYM REPARSE_GUID_DATA_BUFFER_HEADER_SIZE}
//
// Maximum allowed size of the reparse data.
//

const
  MAXIMUM_REPARSE_DATA_BUFFER_SIZE = 16 * 1024;
  {$EXTERNALSYM MAXIMUM_REPARSE_DATA_BUFFER_SIZE}

//
// Predefined reparse tags.
// These tags need to avoid conflicting with IO_REMOUNT defined in ntos\inc\io.h
//

  IO_REPARSE_TAG_RESERVED_ZERO = (0);
  {$EXTERNALSYM IO_REPARSE_TAG_RESERVED_ZERO}
  IO_REPARSE_TAG_RESERVED_ONE  = (1);
  {$EXTERNALSYM IO_REPARSE_TAG_RESERVED_ONE}

//
// The value of the following constant needs to satisfy the following conditions:
//  (1) Be at least as large as the largest of the reserved tags.
//  (2) Be strictly smaller than all the tags in use.
//

  IO_REPARSE_TAG_RESERVED_RANGE = IO_REPARSE_TAG_RESERVED_ONE;
  {$EXTERNALSYM IO_REPARSE_TAG_RESERVED_RANGE}

//
// The reparse tags are a DWORD. The 32 bits are laid out as follows:
//
//   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
//   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
//  +-+-+-+-+-----------------------+-------------------------------+
//  |M|R|N|R|     Reserved bits     |       Reparse Tag Value       |
//  +-+-+-+-+-----------------------+-------------------------------+
//
// M is the Microsoft bit. When set to 1, it denotes a tag owned by Microsoft.
//   All ISVs must use a tag with a 0 in this position.
//   Note: If a Microsoft tag is used by non-Microsoft software, the
//   behavior is not defined.
//
// R is reserved.  Must be zero for non-Microsoft tags.
//
// N is name surrogate. When set to 1, the file represents another named
//   entity in the system.
//
// The M and N bits are OR-able.
// The following macros check for the M and N bit values:
//

//
// Macro to determine whether a reparse point tag corresponds to a tag
// owned by Microsoft.
//

function IsReparseTagMicrosoft(Tag: ULONG): Boolean;
{$EXTERNALSYM IsReparseTagMicrosoft}

//
// Macro to determine whether a reparse point tag corresponds to a file
// that is to be displayed with the slow icon overlay.
//

function IsReparseTagHighLatency(Tag: ULONG): Boolean;
{$EXTERNALSYM IsReparseTagHighLatency}

//
// Macro to determine whether a reparse point tag is a name surrogate
//

function IsReparseTagNameSurrogate(Tag: ULONG): Boolean;
{$EXTERNALSYM IsReparseTagNameSurrogate}

const
  IO_REPARSE_TAG_MOUNT_POINT = DWORD($A0000003);
  {$EXTERNALSYM IO_REPARSE_TAG_MOUNT_POINT}
  IO_REPARSE_TAG_HSM         = DWORD($C0000004);
  {$EXTERNALSYM IO_REPARSE_TAG_HSM}
  IO_REPARSE_TAG_SIS         = DWORD($80000007);
  {$EXTERNALSYM IO_REPARSE_TAG_SIS}
  IO_REPARSE_TAG_DFS         = DWORD($8000000A);
  {$EXTERNALSYM IO_REPARSE_TAG_DFS}
  IO_REPARSE_TAG_FILTER_MANAGER = DWORD($8000000B);
  {$EXTERNALSYM IO_REPARSE_TAG_FILTER_MANAGER}
  IO_COMPLETION_MODIFY_STATE = $0002;
  {$EXTERNALSYM IO_COMPLETION_MODIFY_STATE}
  IO_COMPLETION_ALL_ACCESS   = DWORD(STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $3);
  {$EXTERNALSYM IO_COMPLETION_ALL_ACCESS}
  DUPLICATE_CLOSE_SOURCE     = $00000001;
  {$EXTERNALSYM DUPLICATE_CLOSE_SOURCE}
  DUPLICATE_SAME_ACCESS      = $00000002;
  {$EXTERNALSYM DUPLICATE_SAME_ACCESS}

// line 4763

//
// File header format.
//

{$IFNDEF CLR}

type
  PIMAGE_FILE_HEADER = ^IMAGE_FILE_HEADER;
  {$EXTERNALSYM PIMAGE_FILE_HEADER}
  _IMAGE_FILE_HEADER = record
    Machine: WORD;
    NumberOfSections: WORD;
    TimeDateStamp: DWORD;
    PointerToSymbolTable: DWORD;
    NumberOfSymbols: DWORD;
    SizeOfOptionalHeader: WORD;
    Characteristics: WORD;
  end;
  {$EXTERNALSYM _IMAGE_FILE_HEADER}
  IMAGE_FILE_HEADER = _IMAGE_FILE_HEADER;
  {$EXTERNALSYM IMAGE_FILE_HEADER}
  TImageFileHeader = IMAGE_FILE_HEADER;
  PImageFileHeader = PIMAGE_FILE_HEADER;

const
  IMAGE_SIZEOF_FILE_HEADER = 20;
  {$EXTERNALSYM IMAGE_SIZEOF_FILE_HEADER}

  IMAGE_FILE_RELOCS_STRIPPED         = $0001; // Relocation info stripped from file.
  {$EXTERNALSYM IMAGE_FILE_RELOCS_STRIPPED}
  IMAGE_FILE_EXECUTABLE_IMAGE        = $0002; // File is executable  (i.e. no unresolved externel references).
  {$EXTERNALSYM IMAGE_FILE_EXECUTABLE_IMAGE}
  IMAGE_FILE_LINE_NUMS_STRIPPED      = $0004; // Line nunbers stripped from file.
  {$EXTERNALSYM IMAGE_FILE_LINE_NUMS_STRIPPED}
  IMAGE_FILE_LOCAL_SYMS_STRIPPED     = $0008; // Local symbols stripped from file.
  {$EXTERNALSYM IMAGE_FILE_LOCAL_SYMS_STRIPPED}
  IMAGE_FILE_AGGRESIVE_WS_TRIM       = $0010; // Agressively trim working set
  {$EXTERNALSYM IMAGE_FILE_AGGRESIVE_WS_TRIM}
  IMAGE_FILE_LARGE_ADDRESS_AWARE     = $0020; // App can handle >2gb addresses
  {$EXTERNALSYM IMAGE_FILE_LARGE_ADDRESS_AWARE}
  IMAGE_FILE_BYTES_REVERSED_LO       = $0080; // Bytes of machine word are reversed.
  {$EXTERNALSYM IMAGE_FILE_BYTES_REVERSED_LO}
  IMAGE_FILE_32BIT_MACHINE           = $0100; // 32 bit word machine.
  {$EXTERNALSYM IMAGE_FILE_32BIT_MACHINE}
  IMAGE_FILE_DEBUG_STRIPPED          = $0200; // Debugging info stripped from file in .DBG file
  {$EXTERNALSYM IMAGE_FILE_DEBUG_STRIPPED}
  IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP = $0400; // If Image is on removable media, copy and run from the swap file.
  {$EXTERNALSYM IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP}
  IMAGE_FILE_NET_RUN_FROM_SWAP       = $0800; // If Image is on Net, copy and run from the swap file.
  {$EXTERNALSYM IMAGE_FILE_NET_RUN_FROM_SWAP}
  IMAGE_FILE_SYSTEM                  = $1000; // System File.
  {$EXTERNALSYM IMAGE_FILE_SYSTEM}
  IMAGE_FILE_DLL                     = $2000; // File is a DLL.
  {$EXTERNALSYM IMAGE_FILE_DLL}
  IMAGE_FILE_UP_SYSTEM_ONLY          = $4000; // File should only be run on a UP machine
  {$EXTERNALSYM IMAGE_FILE_UP_SYSTEM_ONLY}
  IMAGE_FILE_BYTES_REVERSED_HI       = $8000; // Bytes of machine word are reversed.
  {$EXTERNALSYM IMAGE_FILE_BYTES_REVERSED_HI}

  IMAGE_FILE_MACHINE_UNKNOWN   = 0;
  {$EXTERNALSYM IMAGE_FILE_MACHINE_UNKNOWN}
  IMAGE_FILE_MACHINE_I386      = $014c; // Intel 386.
  {$EXTERNALSYM IMAGE_FILE_MACHINE_I386}
  IMAGE_FILE_MACHINE_R3000     = $0162; // MIPS little-endian, 0x160 big-endian
  {$EXTERNALSYM IMAGE_FILE_MACHINE_R3000}
  IMAGE_FILE_MACHINE_R4000     = $0166; // MIPS little-endian
  {$EXTERNALSYM IMAGE_FILE_MACHINE_R4000}
  IMAGE_FILE_MACHINE_R10000    = $0168; // MIPS little-endian
  {$EXTERNALSYM IMAGE_FILE_MACHINE_R10000}
  IMAGE_FILE_MACHINE_WCEMIPSV2 = $0169; // MIPS little-endian WCE v2
  {$EXTERNALSYM IMAGE_FILE_MACHINE_WCEMIPSV2}
  IMAGE_FILE_MACHINE_ALPHA     = $0184; // Alpha_AXP
  {$EXTERNALSYM IMAGE_FILE_MACHINE_ALPHA}
  IMAGE_FILE_MACHINE_SH3       = $01a2; // SH3 little-endian
  {$EXTERNALSYM IMAGE_FILE_MACHINE_SH3}
  IMAGE_FILE_MACHINE_SH3DSP    = $01a3;
  {$EXTERNALSYM IMAGE_FILE_MACHINE_SH3DSP}
  IMAGE_FILE_MACHINE_SH3E      = $01a4; // SH3E little-endian
  {$EXTERNALSYM IMAGE_FILE_MACHINE_SH3E}
  IMAGE_FILE_MACHINE_SH4       = $01a6; // SH4 little-endian
  {$EXTERNALSYM IMAGE_FILE_MACHINE_SH4}
  IMAGE_FILE_MACHINE_SH5       = $01a8; // SH5
  {$EXTERNALSYM IMAGE_FILE_MACHINE_SH5}
  IMAGE_FILE_MACHINE_ARM       = $01c0; // ARM Little-Endian
  {$EXTERNALSYM IMAGE_FILE_MACHINE_ARM}
  IMAGE_FILE_MACHINE_THUMB     = $01c2;
  {$EXTERNALSYM IMAGE_FILE_MACHINE_THUMB}
  IMAGE_FILE_MACHINE_AM33      = $01d3;
  {$EXTERNALSYM IMAGE_FILE_MACHINE_AM33}
  IMAGE_FILE_MACHINE_POWERPC   = $01F0; // IBM PowerPC Little-Endian
  {$EXTERNALSYM IMAGE_FILE_MACHINE_POWERPC}
  IMAGE_FILE_MACHINE_POWERPCFP = $01f1;
  {$EXTERNALSYM IMAGE_FILE_MACHINE_POWERPCFP}
  IMAGE_FILE_MACHINE_IA64      = $0200; // Intel 64
  {$EXTERNALSYM IMAGE_FILE_MACHINE_IA64}
  IMAGE_FILE_MACHINE_MIPS16    = $0266; // MIPS
  {$EXTERNALSYM IMAGE_FILE_MACHINE_MIPS16}
  IMAGE_FILE_MACHINE_ALPHA64   = $0284; // ALPHA64
  {$EXTERNALSYM IMAGE_FILE_MACHINE_ALPHA64}
  IMAGE_FILE_MACHINE_MIPSFPU   = $0366; // MIPS
  {$EXTERNALSYM IMAGE_FILE_MACHINE_MIPSFPU}
  IMAGE_FILE_MACHINE_MIPSFPU16 = $0466; // MIPS
  {$EXTERNALSYM IMAGE_FILE_MACHINE_MIPSFPU16}
  IMAGE_FILE_MACHINE_AXP64     = IMAGE_FILE_MACHINE_ALPHA64;
  {$EXTERNALSYM IMAGE_FILE_MACHINE_AXP64}
  IMAGE_FILE_MACHINE_TRICORE   = $0520; // Infineon
  {$EXTERNALSYM IMAGE_FILE_MACHINE_TRICORE}
  IMAGE_FILE_MACHINE_CEF       = $0CEF;
  {$EXTERNALSYM IMAGE_FILE_MACHINE_CEF}
  IMAGE_FILE_MACHINE_EBC       = $0EBC; // EFI Byte Code
  {$EXTERNALSYM IMAGE_FILE_MACHINE_EBC}
  IMAGE_FILE_MACHINE_AMD64     = $8664; // AMD64 (K8)
  {$EXTERNALSYM IMAGE_FILE_MACHINE_AMD64}
  IMAGE_FILE_MACHINE_M32R      = $9041; // M32R little-endian
  {$EXTERNALSYM IMAGE_FILE_MACHINE_M32R}
  IMAGE_FILE_MACHINE_CEE       = $C0EE;
  {$EXTERNALSYM IMAGE_FILE_MACHINE_CEE}

//
// Directory format.
//

const
  IMAGE_NUMBEROF_DIRECTORY_ENTRIES = 16;
  {$EXTERNALSYM IMAGE_NUMBEROF_DIRECTORY_ENTRIES}

//
// Optional header format.
//

type
  PIMAGE_OPTIONAL_HEADER32 = ^IMAGE_OPTIONAL_HEADER32;
  {$EXTERNALSYM PIMAGE_OPTIONAL_HEADER32}

  IMAGE_OPTIONAL_HEADER32 = _IMAGE_OPTIONAL_HEADER;
  {$EXTERNALSYM IMAGE_OPTIONAL_HEADER32}
  TImageOptionalHeader32 = IMAGE_OPTIONAL_HEADER32;
  PImageOptionalHeader32 = PIMAGE_OPTIONAL_HEADER32;

  PIMAGE_ROM_OPTIONAL_HEADER = ^IMAGE_ROM_OPTIONAL_HEADER;
  {$EXTERNALSYM PIMAGE_ROM_OPTIONAL_HEADER}
  _IMAGE_ROM_OPTIONAL_HEADER = record
    Magic: Word;
    MajorLinkerVersion: Byte;
    MinorLinkerVersion: Byte;
    SizeOfCode: DWORD;
    SizeOfInitializedData: DWORD;
    SizeOfUninitializedData: DWORD;
    AddressOfEntryPoint: DWORD;
    BaseOfCode: DWORD;
    BaseOfData: DWORD;
    BaseOfBss: DWORD;
    GprMask: DWORD;
    CprMask: array [0..3] of DWORD;
    GpValue: DWORD;
  end;
  {$EXTERNALSYM _IMAGE_ROM_OPTIONAL_HEADER}
  IMAGE_ROM_OPTIONAL_HEADER = _IMAGE_ROM_OPTIONAL_HEADER;
  {$EXTERNALSYM IMAGE_ROM_OPTIONAL_HEADER}
  TImageRomOptionalHeader = IMAGE_ROM_OPTIONAL_HEADER;
  PImageRomOptionalHeader = PIMAGE_ROM_OPTIONAL_HEADER;

  PIMAGE_OPTIONAL_HEADER64 = ^IMAGE_OPTIONAL_HEADER64;
  {$EXTERNALSYM PIMAGE_OPTIONAL_HEADER64}
  _IMAGE_OPTIONAL_HEADER64 = record
    Magic: Word;
    MajorLinkerVersion: Byte;
    MinorLinkerVersion: Byte;
    SizeOfCode: DWORD;
    SizeOfInitializedData: DWORD;
    SizeOfUninitializedData: DWORD;
    AddressOfEntryPoint: DWORD;
    BaseOfCode: DWORD;
    ImageBase: Int64;
    SectionAlignment: DWORD;
    FileAlignment: DWORD;
    MajorOperatingSystemVersion: Word;
    MinorOperatingSystemVersion: Word;
    MajorImageVersion: Word;
    MinorImageVersion: Word;
    MajorSubsystemVersion: Word;
    MinorSubsystemVersion: Word;
    Win32VersionValue: DWORD;
    SizeOfImage: DWORD;
    SizeOfHeaders: DWORD;
    CheckSum: DWORD;
    Subsystem: Word;
    DllCharacteristics: Word;
    SizeOfStackReserve: Int64;
    SizeOfStackCommit: Int64;
    SizeOfHeapReserve: Int64;
    SizeOfHeapCommit: Int64;
    LoaderFlags: DWORD;
    NumberOfRvaAndSizes: DWORD;
    DataDirectory: array [0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES - 1] of IMAGE_DATA_DIRECTORY;
  end;
  {$EXTERNALSYM _IMAGE_OPTIONAL_HEADER64}
  IMAGE_OPTIONAL_HEADER64 = _IMAGE_OPTIONAL_HEADER64;
  {$EXTERNALSYM IMAGE_OPTIONAL_HEADER64}
  TImageOptionalHeader64 = IMAGE_OPTIONAL_HEADER64;
  PImageOptionalHeader64 = PIMAGE_OPTIONAL_HEADER64;

const
  IMAGE_SIZEOF_ROM_OPTIONAL_HEADER  = 56;
  {$EXTERNALSYM IMAGE_SIZEOF_ROM_OPTIONAL_HEADER}
  IMAGE_SIZEOF_STD_OPTIONAL_HEADER  = 28;
  {$EXTERNALSYM IMAGE_SIZEOF_STD_OPTIONAL_HEADER}
  IMAGE_SIZEOF_NT_OPTIONAL32_HEADER = 224;
  {$EXTERNALSYM IMAGE_SIZEOF_NT_OPTIONAL32_HEADER}
  IMAGE_SIZEOF_NT_OPTIONAL64_HEADER = 240;
  {$EXTERNALSYM IMAGE_SIZEOF_NT_OPTIONAL64_HEADER}

  IMAGE_NT_OPTIONAL_HDR32_MAGIC = $10b;
  {$EXTERNALSYM IMAGE_NT_OPTIONAL_HDR32_MAGIC}
  IMAGE_NT_OPTIONAL_HDR64_MAGIC = $20b;
  {$EXTERNALSYM IMAGE_NT_OPTIONAL_HDR64_MAGIC}
  IMAGE_ROM_OPTIONAL_HDR_MAGIC  = $107;
  {$EXTERNALSYM IMAGE_ROM_OPTIONAL_HDR_MAGIC}

(*
type
  IMAGE_OPTIONAL_HEADER = IMAGE_OPTIONAL_HEADER32;
  {$EXTERNALSYM IMAGE_OPTIONAL_HEADER}
  PIMAGE_OPTIONAL_HEADER = PIMAGE_OPTIONAL_HEADER32;
  {$EXTERNALSYM PIMAGE_OPTIONAL_HEADER}
*)

const
  IMAGE_SIZEOF_NT_OPTIONAL_HEADER = IMAGE_SIZEOF_NT_OPTIONAL32_HEADER;
  {$EXTERNALSYM IMAGE_SIZEOF_NT_OPTIONAL_HEADER}
  IMAGE_NT_OPTIONAL_HDR_MAGIC     = IMAGE_NT_OPTIONAL_HDR32_MAGIC;
  {$EXTERNALSYM IMAGE_NT_OPTIONAL_HDR_MAGIC}

type
  PIMAGE_NT_HEADERS64 = ^IMAGE_NT_HEADERS64;
  {$EXTERNALSYM PIMAGE_NT_HEADERS64}
  _IMAGE_NT_HEADERS64 = record
    Signature: DWORD;
    FileHeader: IMAGE_FILE_HEADER;
    OptionalHeader: IMAGE_OPTIONAL_HEADER64;
  end;
  {$EXTERNALSYM _IMAGE_NT_HEADERS64}
  IMAGE_NT_HEADERS64 = _IMAGE_NT_HEADERS64;
  {$EXTERNALSYM IMAGE_NT_HEADERS64}
  TImageNtHeaders64 = IMAGE_NT_HEADERS64;
  PImageNtHeaders64 = PIMAGE_NT_HEADERS64;

  PIMAGE_NT_HEADERS32 = ^IMAGE_NT_HEADERS32;
  {$EXTERNALSYM PIMAGE_NT_HEADERS32}
  _IMAGE_NT_HEADERS = record
    Signature: DWORD;
    FileHeader: IMAGE_FILE_HEADER;
    OptionalHeader: IMAGE_OPTIONAL_HEADER32;
  end;
  {$EXTERNALSYM _IMAGE_NT_HEADERS}
  IMAGE_NT_HEADERS32 = _IMAGE_NT_HEADERS;
  {$EXTERNALSYM IMAGE_NT_HEADERS32}
  TImageNtHeaders32 = IMAGE_NT_HEADERS32;
  PImageNtHeaders32 = PIMAGE_NT_HEADERS32;

// Subsystem Values

const
  IMAGE_SUBSYSTEM_UNKNOWN                 = 0; // Unknown subsystem.
  {$EXTERNALSYM IMAGE_SUBSYSTEM_UNKNOWN}
  IMAGE_SUBSYSTEM_NATIVE                  = 1; // Image doesn't require a subsystem.
  {$EXTERNALSYM IMAGE_SUBSYSTEM_NATIVE}
  IMAGE_SUBSYSTEM_WINDOWS_GUI             = 2; // Image runs in the Windows GUI subsystem.
  {$EXTERNALSYM IMAGE_SUBSYSTEM_WINDOWS_GUI}
  IMAGE_SUBSYSTEM_WINDOWS_CUI             = 3; // Image runs in the Windows character subsystem.
  {$EXTERNALSYM IMAGE_SUBSYSTEM_WINDOWS_CUI}
  IMAGE_SUBSYSTEM_OS2_CUI                 = 5; // image runs in the OS/2 character subsystem.
  {$EXTERNALSYM IMAGE_SUBSYSTEM_OS2_CUI}
  IMAGE_SUBSYSTEM_POSIX_CUI               = 7; // image runs in the Posix character subsystem.
  {$EXTERNALSYM IMAGE_SUBSYSTEM_POSIX_CUI}
  IMAGE_SUBSYSTEM_NATIVE_WINDOWS          = 8; // image is a native Win9x driver.
  {$EXTERNALSYM IMAGE_SUBSYSTEM_NATIVE_WINDOWS}
  IMAGE_SUBSYSTEM_WINDOWS_CE_GUI          = 9; // Image runs in the Windows CE subsystem.
  {$EXTERNALSYM IMAGE_SUBSYSTEM_WINDOWS_CE_GUI}
  IMAGE_SUBSYSTEM_EFI_APPLICATION         = 10;
  {$EXTERNALSYM IMAGE_SUBSYSTEM_EFI_APPLICATION}
  IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER = 11;
  {$EXTERNALSYM IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER}
  IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER      = 12;
  {$EXTERNALSYM IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER}
  IMAGE_SUBSYSTEM_EFI_ROM                 = 13;
  {$EXTERNALSYM IMAGE_SUBSYSTEM_EFI_ROM}
  IMAGE_SUBSYSTEM_XBOX                    = 14;
  {$EXTERNALSYM IMAGE_SUBSYSTEM_XBOX}

// DllCharacteristics Entries

//      IMAGE_LIBRARY_PROCESS_INIT           0x0001     // Reserved.
//      IMAGE_LIBRARY_PROCESS_TERM           0x0002     // Reserved.
//      IMAGE_LIBRARY_THREAD_INIT            0x0004     // Reserved.
//      IMAGE_LIBRARY_THREAD_TERM            0x0008     // Reserved.
  IMAGE_DLLCHARACTERISTICS_NO_ISOLATION = $0200;    // Image understands isolation and doesn't want it
  {$EXTERNALSYM IMAGE_DLLCHARACTERISTICS_NO_ISOLATION}
  IMAGE_DLLCHARACTERISTICS_NO_SEH  = $0400; // Image does not use SEH.  No SE handler may reside in this image
  {$EXTERNALSYM IMAGE_DLLCHARACTERISTICS_NO_SEH}
  IMAGE_DLLCHARACTERISTICS_NO_BIND = $0800; // Do not bind this image.
  {$EXTERNALSYM IMAGE_DLLCHARACTERISTICS_NO_BIND}

//                                           0x1000     // Reserved.

  IMAGE_DLLCHARACTERISTICS_WDM_DRIVER = $2000; // Driver uses WDM model
  {$EXTERNALSYM IMAGE_DLLCHARACTERISTICS_WDM_DRIVER}

//                                           0x4000     // Reserved.

  IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE = $8000;
  {$EXTERNALSYM IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE}

// Directory Entries

  IMAGE_DIRECTORY_ENTRY_EXPORT    = 0; // Export Directory
  {$EXTERNALSYM IMAGE_DIRECTORY_ENTRY_EXPORT}
  IMAGE_DIRECTORY_ENTRY_IMPORT    = 1; // Import Directory
  {$EXTERNALSYM IMAGE_DIRECTORY_ENTRY_IMPORT}
  IMAGE_DIRECTORY_ENTRY_RESOURCE  = 2; // Resource Directory
  {$EXTERNALSYM IMAGE_DIRECTORY_ENTRY_RESOURCE}
  IMAGE_DIRECTORY_ENTRY_EXCEPTION = 3; // Exception Directory
  {$EXTERNALSYM IMAGE_DIRECTORY_ENTRY_EXCEPTION}
  IMAGE_DIRECTORY_ENTRY_SECURITY  = 4; // Security Directory
  {$EXTERNALSYM IMAGE_DIRECTORY_ENTRY_SECURITY}
  IMAGE_DIRECTORY_ENTRY_BASERELOC = 5; // Base Relocation Table
  {$EXTERNALSYM IMAGE_DIRECTORY_ENTRY_BASERELOC}
  IMAGE_DIRECTORY_ENTRY_DEBUG     = 6; // Debug Directory
  {$EXTERNALSYM IMAGE_DIRECTORY_ENTRY_DEBUG}

//      IMAGE_DIRECTORY_ENTRY_COPYRIGHT       7   // (X86 usage)

  IMAGE_DIRECTORY_ENTRY_ARCHITECTURE   = 7; // Architecture Specific Data
  {$EXTERNALSYM IMAGE_DIRECTORY_ENTRY_ARCHITECTURE}
  IMAGE_DIRECTORY_ENTRY_GLOBALPTR      = 8; // RVA of GP
  {$EXTERNALSYM IMAGE_DIRECTORY_ENTRY_GLOBALPTR}
  IMAGE_DIRECTORY_ENTRY_TLS            = 9; // TLS Directory
  {$EXTERNALSYM IMAGE_DIRECTORY_ENTRY_TLS}
  IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG    = 10; // Load Configuration Directory
  {$EXTERNALSYM IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG}
  IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT   = 11; // Bound Import Directory in headers
  {$EXTERNALSYM IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT}
  IMAGE_DIRECTORY_ENTRY_IAT            = 12; // Import Address Table
  {$EXTERNALSYM IMAGE_DIRECTORY_ENTRY_IAT}
  IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT   = 13; // Delay Load Import Descriptors
  {$EXTERNALSYM IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT}
  IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR = 14; // COM Runtime descriptor
  {$EXTERNALSYM IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR}

//
// Non-COFF Object file header
//

type
  PAnonObjectHeader = ^ANON_OBJECT_HEADER;
  ANON_OBJECT_HEADER = record
    Sig1: Word;        // Must be IMAGE_FILE_MACHINE_UNKNOWN
    Sig2: Word;        // Must be 0xffff
    Version: Word;     // >= 1 (implies the CLSID field is present)
    Machine: Word;
    TimeDateStamp: DWORD;
    ClassID: TCLSID;    // Used to invoke CoCreateInstance
    SizeOfData: DWORD; // Size of data that follows the header
  end;
  {$EXTERNALSYM ANON_OBJECT_HEADER}
  TAnonObjectHeader = ANON_OBJECT_HEADER;

//
// Section header format.
//

const
  IMAGE_SIZEOF_SHORT_NAME = 8;
  {$EXTERNALSYM IMAGE_SIZEOF_SHORT_NAME}

type
  PPImageSectionHeader = ^PImageSectionHeader;

// IMAGE_FIRST_SECTION doesn't need 32/64 versions since the file header is the same either way.

function IMAGE_FIRST_SECTION(NtHeader: PImageNtHeaders): PImageSectionHeader;
{$EXTERNALSYM IMAGE_FIRST_SECTION}

const
  IMAGE_SIZEOF_SECTION_HEADER = 40;
  {$EXTERNALSYM IMAGE_SIZEOF_SECTION_HEADER}

//
// Section characteristics.
//
//      IMAGE_SCN_TYPE_REG                   0x00000000  // Reserved.
//      IMAGE_SCN_TYPE_DSECT                 0x00000001  // Reserved.
//      IMAGE_SCN_TYPE_NOLOAD                0x00000002  // Reserved.
//      IMAGE_SCN_TYPE_GROUP                 0x00000004  // Reserved.

  IMAGE_SCN_TYPE_NO_PAD = $00000008; // Reserved.
  {$EXTERNALSYM IMAGE_SCN_TYPE_NO_PAD}

//      IMAGE_SCN_TYPE_COPY                  0x00000010  // Reserved.

  IMAGE_SCN_CNT_CODE               = $00000020; // Section contains code.
  {$EXTERNALSYM IMAGE_SCN_CNT_CODE}
  IMAGE_SCN_CNT_INITIALIZED_DATA   = $00000040; // Section contains initialized data.
  {$EXTERNALSYM IMAGE_SCN_CNT_INITIALIZED_DATA}
  IMAGE_SCN_CNT_UNINITIALIZED_DATA = $00000080; // Section contains uninitialized data.
  {$EXTERNALSYM IMAGE_SCN_CNT_UNINITIALIZED_DATA}

  IMAGE_SCN_LNK_OTHER = $00000100; // Reserved.
  {$EXTERNALSYM IMAGE_SCN_LNK_OTHER}
  IMAGE_SCN_LNK_INFO  = $00000200; // Section contains comments or some other type of information.
  {$EXTERNALSYM IMAGE_SCN_LNK_INFO}

//      IMAGE_SCN_TYPE_OVER                  0x00000400  // Reserved.

  IMAGE_SCN_LNK_REMOVE = $00000800; // Section contents will not become part of image.
  {$EXTERNALSYM IMAGE_SCN_LNK_REMOVE}
  IMAGE_SCN_LNK_COMDAT = $00001000; // Section contents comdat.
  {$EXTERNALSYM IMAGE_SCN_LNK_COMDAT}

//                                           0x00002000  // Reserved.
//      IMAGE_SCN_MEM_PROTECTED - Obsolete   0x00004000

  IMAGE_SCN_NO_DEFER_SPEC_EXC = $00004000; // Reset speculative exceptions handling bits in the TLB entries for this section.
  {$EXTERNALSYM IMAGE_SCN_NO_DEFER_SPEC_EXC}
  IMAGE_SCN_GPREL             = $00008000; // Section content can be accessed relative to GP
  {$EXTERNALSYM IMAGE_SCN_GPREL}
  IMAGE_SCN_MEM_FARDATA       = $00008000;
  {$EXTERNALSYM IMAGE_SCN_MEM_FARDATA}

//      IMAGE_SCN_MEM_SYSHEAP  - Obsolete    0x00010000

  IMAGE_SCN_MEM_PURGEABLE = $00020000;
  {$EXTERNALSYM IMAGE_SCN_MEM_PURGEABLE}
  IMAGE_SCN_MEM_16BIT     = $00020000;
  {$EXTERNALSYM IMAGE_SCN_MEM_16BIT}
  IMAGE_SCN_MEM_LOCKED    = $00040000;
  {$EXTERNALSYM IMAGE_SCN_MEM_LOCKED}
  IMAGE_SCN_MEM_PRELOAD   = $00080000;
  {$EXTERNALSYM IMAGE_SCN_MEM_PRELOAD}

  IMAGE_SCN_ALIGN_1BYTES    = $00100000;
  {$EXTERNALSYM IMAGE_SCN_ALIGN_1BYTES}
  IMAGE_SCN_ALIGN_2BYTES    = $00200000;
  {$EXTERNALSYM IMAGE_SCN_ALIGN_2BYTES}
  IMAGE_SCN_ALIGN_4BYTES    = $00300000;
  {$EXTERNALSYM IMAGE_SCN_ALIGN_4BYTES}
  IMAGE_SCN_ALIGN_8BYTES    = $00400000;
  {$EXTERNALSYM IMAGE_SCN_ALIGN_8BYTES}
  IMAGE_SCN_ALIGN_16BYTES   = $00500000; // Default alignment if no others are specified.
  {$EXTERNALSYM IMAGE_SCN_ALIGN_16BYTES}
  IMAGE_SCN_ALIGN_32BYTES   = $00600000;
  {$EXTERNALSYM IMAGE_SCN_ALIGN_32BYTES}
  IMAGE_SCN_ALIGN_64BYTES   = $00700000;
  {$EXTERNALSYM IMAGE_SCN_ALIGN_64BYTES}
  IMAGE_SCN_ALIGN_128BYTES  = $00800000;
  {$EXTERNALSYM IMAGE_SCN_ALIGN_128BYTES}
  IMAGE_SCN_ALIGN_256BYTES  = $00900000;
  {$EXTERNALSYM IMAGE_SCN_ALIGN_256BYTES}
  IMAGE_SCN_ALIGN_512BYTES  = $00A00000;
  {$EXTERNALSYM IMAGE_SCN_ALIGN_512BYTES}
  IMAGE_SCN_ALIGN_1024BYTES = $00B00000;
  {$EXTERNALSYM IMAGE_SCN_ALIGN_1024BYTES}
  IMAGE_SCN_ALIGN_2048BYTES = $00C00000;
  {$EXTERNALSYM IMAGE_SCN_ALIGN_2048BYTES}
  IMAGE_SCN_ALIGN_4096BYTES = $00D00000;
  {$EXTERNALSYM IMAGE_SCN_ALIGN_4096BYTES}
  IMAGE_SCN_ALIGN_8192BYTES = $00E00000;
  {$EXTERNALSYM IMAGE_SCN_ALIGN_8192BYTES}

// Unused                                    0x00F00000

  IMAGE_SCN_ALIGN_MASK = $00F00000;
  {$EXTERNALSYM IMAGE_SCN_ALIGN_MASK}

  IMAGE_SCN_LNK_NRELOC_OVFL = $01000000; // Section contains extended relocations.
  {$EXTERNALSYM IMAGE_SCN_LNK_NRELOC_OVFL}
  IMAGE_SCN_MEM_DISCARDABLE = $02000000; // Section can be discarded.
  {$EXTERNALSYM IMAGE_SCN_MEM_DISCARDABLE}
  IMAGE_SCN_MEM_NOT_CACHED  = $04000000; // Section is not cachable.
  {$EXTERNALSYM IMAGE_SCN_MEM_NOT_CACHED}
  IMAGE_SCN_MEM_NOT_PAGED   = $08000000; // Section is not pageable.
  {$EXTERNALSYM IMAGE_SCN_MEM_NOT_PAGED}
  IMAGE_SCN_MEM_SHARED      = $10000000; // Section is shareable.
  {$EXTERNALSYM IMAGE_SCN_MEM_SHARED}
  IMAGE_SCN_MEM_EXECUTE     = $20000000; // Section is executable.
  {$EXTERNALSYM IMAGE_SCN_MEM_EXECUTE}
  IMAGE_SCN_MEM_READ        = $40000000; // Section is readable.
  {$EXTERNALSYM IMAGE_SCN_MEM_READ}
  IMAGE_SCN_MEM_WRITE       = DWORD($80000000); // Section is writeable.
  {$EXTERNALSYM IMAGE_SCN_MEM_WRITE}

// line 6232

//
// Line number format.
//

type
  TImgLineNoType = record
    case Integer of
      0: (SymbolTableIndex: DWORD);               // Symbol table index of function name if Linenumber is 0.
      1: (VirtualAddress: DWORD);                 // Virtual address of line number.
  end;

  PIMAGE_LINENUMBER = ^IMAGE_LINENUMBER;
  {$EXTERNALSYM PIMAGE_LINENUMBER}
  _IMAGE_LINENUMBER = record
    Type_: TImgLineNoType;
    Linenumber: WORD;                         // Line number.
  end;
  {$EXTERNALSYM _IMAGE_LINENUMBER}
  IMAGE_LINENUMBER = _IMAGE_LINENUMBER;
  {$EXTERNALSYM IMAGE_LINENUMBER}
  TImageLineNumber = IMAGE_LINENUMBER;
  PImageLineNumber = PIMAGE_LINENUMBER;

const
  IMAGE_SIZEOF_LINENUMBER = 6;
  {$EXTERNALSYM IMAGE_SIZEOF_LINENUMBER}

// #include "poppack.h"                        // Back to 4 byte packing

//
// Based relocation format.
//

type
  PIMAGE_BASE_RELOCATION = ^IMAGE_BASE_RELOCATION;
  {$EXTERNALSYM PIMAGE_BASE_RELOCATION}
  _IMAGE_BASE_RELOCATION = record
    VirtualAddress: DWORD;
    SizeOfBlock: DWORD;
    //  WORD    TypeOffset[1];
  end;
  {$EXTERNALSYM _IMAGE_BASE_RELOCATION}
  IMAGE_BASE_RELOCATION = _IMAGE_BASE_RELOCATION;
  {$EXTERNALSYM IMAGE_BASE_RELOCATION}
  TImageBaseRelocation = IMAGE_BASE_RELOCATION;
  PImageBaseRelocation = PIMAGE_BASE_RELOCATION;

const
  IMAGE_SIZEOF_BASE_RELOCATION = 8;
  {$EXTERNALSYM IMAGE_SIZEOF_BASE_RELOCATION}

//
// Based relocation types.
//

  IMAGE_REL_BASED_ABSOLUTE     = 0;
  {$EXTERNALSYM IMAGE_REL_BASED_ABSOLUTE}
  IMAGE_REL_BASED_HIGH         = 1;
  {$EXTERNALSYM IMAGE_REL_BASED_HIGH}
  IMAGE_REL_BASED_LOW          = 2;
  {$EXTERNALSYM IMAGE_REL_BASED_LOW}
  IMAGE_REL_BASED_HIGHLOW      = 3;
  {$EXTERNALSYM IMAGE_REL_BASED_HIGHLOW}
  IMAGE_REL_BASED_HIGHADJ      = 4;
  {$EXTERNALSYM IMAGE_REL_BASED_HIGHADJ}
  IMAGE_REL_BASED_MIPS_JMPADDR = 5;
  {$EXTERNALSYM IMAGE_REL_BASED_MIPS_JMPADDR}

  IMAGE_REL_BASED_MIPS_JMPADDR16 = 9;
  {$EXTERNALSYM IMAGE_REL_BASED_MIPS_JMPADDR16}
  IMAGE_REL_BASED_IA64_IMM64     = 9;
  {$EXTERNALSYM IMAGE_REL_BASED_IA64_IMM64}
  IMAGE_REL_BASED_DIR64          = 10;
  {$EXTERNALSYM IMAGE_REL_BASED_DIR64}

//
// Archive format.
//

  IMAGE_ARCHIVE_START_SIZE       = 8;
  {$EXTERNALSYM IMAGE_ARCHIVE_START_SIZE}
  IMAGE_ARCHIVE_START            = '!<arch>'#10;
  {$EXTERNALSYM IMAGE_ARCHIVE_START}
  IMAGE_ARCHIVE_END              = '`'#10;
  {$EXTERNALSYM IMAGE_ARCHIVE_END}
  IMAGE_ARCHIVE_PAD              = #10;
  {$EXTERNALSYM IMAGE_ARCHIVE_PAD}
  IMAGE_ARCHIVE_LINKER_MEMBER    = '/               ';
  {$EXTERNALSYM IMAGE_ARCHIVE_LINKER_MEMBER}
  IMAGE_ARCHIVE_LONGNAMES_MEMBER = '//              ';
  {$EXTERNALSYM IMAGE_ARCHIVE_LONGNAMES_MEMBER}

type
  PIMAGE_ARCHIVE_MEMBER_HEADER = ^IMAGE_ARCHIVE_MEMBER_HEADER;
  {$EXTERNALSYM PIMAGE_ARCHIVE_MEMBER_HEADER}
  _IMAGE_ARCHIVE_MEMBER_HEADER = record
    Name: array [0..15] of Byte; // File member name - `/' terminated.
    Date: array [0..11] of Byte; // File member date - decimal.
    UserID: array [0..5] of Byte; // File member user id - decimal.
    GroupID: array [0..5] of Byte; // File member group id - decimal.
    Mode: array [0..7] of Byte; // File member mode - octal.
    Size: array [0..9] of Byte; // File member size - decimal.
    EndHeader: array [0..1] of Byte; // String to end header.
  end;
  {$EXTERNALSYM _IMAGE_ARCHIVE_MEMBER_HEADER}
  IMAGE_ARCHIVE_MEMBER_HEADER = _IMAGE_ARCHIVE_MEMBER_HEADER;
  {$EXTERNALSYM IMAGE_ARCHIVE_MEMBER_HEADER}
  TImageArchiveMemberHeader = IMAGE_ARCHIVE_MEMBER_HEADER;
  PImageArchiveMemberHeader = PIMAGE_ARCHIVE_MEMBER_HEADER;

const
  IMAGE_SIZEOF_ARCHIVE_MEMBER_HDR = 60;
  {$EXTERNALSYM IMAGE_SIZEOF_ARCHIVE_MEMBER_HDR}

// line 6346

//
// DLL support.
//

//
// Export Format
//

type
  PIMAGE_EXPORT_DIRECTORY = ^IMAGE_EXPORT_DIRECTORY;
  {$EXTERNALSYM PIMAGE_EXPORT_DIRECTORY}
  _IMAGE_EXPORT_DIRECTORY = record
    Characteristics: DWORD;
    TimeDateStamp: DWORD;
    MajorVersion: Word;
    MinorVersion: Word;
    Name: DWORD;
    Base: DWORD;
    NumberOfFunctions: DWORD;
    NumberOfNames: DWORD;
    AddressOfFunctions: DWORD; // RVA from base of image
    AddressOfNames: DWORD; // RVA from base of image
    AddressOfNameOrdinals: DWORD; // RVA from base of image
  end;
  {$EXTERNALSYM _IMAGE_EXPORT_DIRECTORY}
  IMAGE_EXPORT_DIRECTORY = _IMAGE_EXPORT_DIRECTORY;
  {$EXTERNALSYM IMAGE_EXPORT_DIRECTORY}
  TImageExportDirectory = IMAGE_EXPORT_DIRECTORY;
  PImageExportDirectory = PIMAGE_EXPORT_DIRECTORY;

//
// Import Format
//

  PIMAGE_IMPORT_BY_NAME = ^IMAGE_IMPORT_BY_NAME;
  {$EXTERNALSYM PIMAGE_IMPORT_BY_NAME}
  _IMAGE_IMPORT_BY_NAME = record
    Hint: Word;
    Name: array [0..0] of AnsiChar;
  end;
  {$EXTERNALSYM _IMAGE_IMPORT_BY_NAME}
  IMAGE_IMPORT_BY_NAME = _IMAGE_IMPORT_BY_NAME;
  {$EXTERNALSYM IMAGE_IMPORT_BY_NAME}
  TImageImportByName = IMAGE_IMPORT_BY_NAME;
  PImageImportByName = PIMAGE_IMPORT_BY_NAME;

// #include "pshpack8.h"                       // Use align 8 for the 64-bit IAT.

  PIMAGE_THUNK_DATA64 = ^IMAGE_THUNK_DATA64;
  {$EXTERNALSYM PIMAGE_THUNK_DATA64}
  _IMAGE_THUNK_DATA64 = record
    case Integer of
      0: (ForwarderString: ULONGLONG);   // PBYTE
      1: (Function_: ULONGLONG);         // PDWORD
      2: (Ordinal: ULONGLONG);
      3: (AddressOfData: ULONGLONG);     // PIMAGE_IMPORT_BY_NAME
  end;
  {$EXTERNALSYM _IMAGE_THUNK_DATA64}
  IMAGE_THUNK_DATA64 = _IMAGE_THUNK_DATA64;
  {$EXTERNALSYM IMAGE_THUNK_DATA64}
  TImageThunkData64 = IMAGE_THUNK_DATA64;
  PImageThunkData64 = PIMAGE_THUNK_DATA64;

// #include "poppack.h"                        // Back to 4 byte packing

  PIMAGE_THUNK_DATA32 = ^IMAGE_THUNK_DATA32;
  {$EXTERNALSYM PIMAGE_THUNK_DATA32}
  _IMAGE_THUNK_DATA32 = record
    case Integer of
      0: (ForwarderString: DWORD);   // PBYTE
      1: (Function_: DWORD);         // PDWORD
      2: (Ordinal: DWORD);
      3: (AddressOfData: DWORD);     // PIMAGE_IMPORT_BY_NAME
  end;
  {$EXTERNALSYM _IMAGE_THUNK_DATA32}
  IMAGE_THUNK_DATA32 = _IMAGE_THUNK_DATA32;
  {$EXTERNALSYM IMAGE_THUNK_DATA32}
  TImageThunkData32 = IMAGE_THUNK_DATA32;
  PImageThunkData32 = PIMAGE_THUNK_DATA32;

const
  IMAGE_ORDINAL_FLAG64 = ULONGLONG($8000000000000000);
  {$EXTERNALSYM IMAGE_ORDINAL_FLAG64}
  IMAGE_ORDINAL_FLAG32 = DWORD($80000000);
  {$EXTERNALSYM IMAGE_ORDINAL_FLAG32}

function IMAGE_ORDINAL64(Ordinal: ULONGLONG): ULONGLONG;
{$EXTERNALSYM IMAGE_ORDINAL64}
function IMAGE_ORDINAL32(Ordinal: DWORD): DWORD;
{$EXTERNALSYM IMAGE_ORDINAL32}
function IMAGE_SNAP_BY_ORDINAL64(Ordinal: ULONGLONG): Boolean;
{$EXTERNALSYM IMAGE_SNAP_BY_ORDINAL64}
function IMAGE_SNAP_BY_ORDINAL32(Ordinal: DWORD): Boolean;
{$EXTERNALSYM IMAGE_SNAP_BY_ORDINAL32}

//
// Thread Local Storage
//

type
  PIMAGE_TLS_CALLBACK = procedure (DllHandle: Pointer; Reason: DWORD; Reserved: Pointer); stdcall;
  {$EXTERNALSYM PIMAGE_TLS_CALLBACK}
  TImageTlsCallback = PIMAGE_TLS_CALLBACK;

  PIMAGE_TLS_DIRECTORY64 = ^IMAGE_TLS_DIRECTORY64;
  {$EXTERNALSYM PIMAGE_TLS_DIRECTORY64}
  _IMAGE_TLS_DIRECTORY64 = record
    StartAddressOfRawData: ULONGLONG;
    EndAddressOfRawData: ULONGLONG;
    AddressOfIndex: ULONGLONG;         // PDWORD
    AddressOfCallBacks: ULONGLONG;     // PIMAGE_TLS_CALLBACK *;
    SizeOfZeroFill: DWORD;
    Characteristics: DWORD;
  end;
  {$EXTERNALSYM _IMAGE_TLS_DIRECTORY64}
  IMAGE_TLS_DIRECTORY64 = _IMAGE_TLS_DIRECTORY64;
  {$EXTERNALSYM IMAGE_TLS_DIRECTORY64}
  TImageTlsDirectory64 = IMAGE_TLS_DIRECTORY64;
  PImageTlsDirectory64 = PIMAGE_TLS_DIRECTORY64;

  PIMAGE_TLS_DIRECTORY32 = ^IMAGE_TLS_DIRECTORY32;
  {$EXTERNALSYM PIMAGE_TLS_DIRECTORY32}
  _IMAGE_TLS_DIRECTORY32 = record
    StartAddressOfRawData: DWORD;
    EndAddressOfRawData: DWORD;
    AddressOfIndex: DWORD;             // PDWORD
    AddressOfCallBacks: DWORD;         // PIMAGE_TLS_CALLBACK *
    SizeOfZeroFill: DWORD;
    Characteristics: DWORD;
  end;
  {$EXTERNALSYM _IMAGE_TLS_DIRECTORY32}
  IMAGE_TLS_DIRECTORY32 = _IMAGE_TLS_DIRECTORY32;
  {$EXTERNALSYM IMAGE_TLS_DIRECTORY32}
  TImageTlsDirectory32 = IMAGE_TLS_DIRECTORY32;
  PImageTlsDirectory32 = PIMAGE_TLS_DIRECTORY32;

const
  IMAGE_ORDINAL_FLAG = IMAGE_ORDINAL_FLAG32;
  {$EXTERNALSYM IMAGE_ORDINAL_FLAG}

function IMAGE_ORDINAL(Ordinal: DWORD): DWORD;
{$EXTERNALSYM IMAGE_ORDINAL}

type
  IMAGE_THUNK_DATA = IMAGE_THUNK_DATA32;
  {$EXTERNALSYM IMAGE_THUNK_DATA}
  PIMAGE_THUNK_DATA = PIMAGE_THUNK_DATA32;
  {$EXTERNALSYM PIMAGE_THUNK_DATA}
  TImageThunkData = TImageThunkData32;
  PImageThunkData = PImageThunkData32;

function IMAGE_SNAP_BY_ORDINAL(Ordinal: DWORD): Boolean;
{$EXTERNALSYM IMAGE_SNAP_BY_ORDINAL}

type
  IMAGE_TLS_DIRECTORY = IMAGE_TLS_DIRECTORY32;
  {$EXTERNALSYM IMAGE_TLS_DIRECTORY}
  PIMAGE_TLS_DIRECTORY = PIMAGE_TLS_DIRECTORY32;
  {$EXTERNALSYM PIMAGE_TLS_DIRECTORY}
  TImageTlsDirectory = TImageTlsDirectory32;
  PImageTlsDirectory = PImageTlsDirectory32;

  TIIDUnion = record
    case Integer of
      0: (Characteristics: DWORD);         // 0 for terminating null import descriptor
      1: (OriginalFirstThunk: DWORD);      // RVA to original unbound IAT (PIMAGE_THUNK_DATA)
  end;

  PIMAGE_IMPORT_DESCRIPTOR = ^IMAGE_IMPORT_DESCRIPTOR;
  {$EXTERNALSYM PIMAGE_IMPORT_DESCRIPTOR}
  _IMAGE_IMPORT_DESCRIPTOR = record
    Union: TIIDUnion;
    TimeDateStamp: DWORD;                  // 0 if not bound,
                                           // -1 if bound, and real date\time stamp
                                           //     in IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT (new BIND)
                                           // O.W. date/time stamp of DLL bound to (Old BIND)

    ForwarderChain: DWORD;                 // -1 if no forwarders
    Name: DWORD;
    FirstThunk: DWORD;                     // RVA to IAT (if bound this IAT has actual addresses)
  end;
  {$EXTERNALSYM _IMAGE_IMPORT_DESCRIPTOR}
  IMAGE_IMPORT_DESCRIPTOR = _IMAGE_IMPORT_DESCRIPTOR;
  {$EXTERNALSYM IMAGE_IMPORT_DESCRIPTOR}
  TImageImportDescriptor = IMAGE_IMPORT_DESCRIPTOR;
  PImageImportDescriptor = PIMAGE_IMPORT_DESCRIPTOR;

//
// New format import descriptors pointed to by DataDirectory[ IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT ]
//

type
  PIMAGE_BOUND_IMPORT_DESCRIPTOR = ^IMAGE_BOUND_IMPORT_DESCRIPTOR;
  {$EXTERNALSYM PIMAGE_BOUND_IMPORT_DESCRIPTOR}
  _IMAGE_BOUND_IMPORT_DESCRIPTOR = record
    TimeDateStamp: DWORD;
    OffsetModuleName: Word;
    NumberOfModuleForwarderRefs: Word;
    // Array of zero or more IMAGE_BOUND_FORWARDER_REF follows
  end;
  {$EXTERNALSYM _IMAGE_BOUND_IMPORT_DESCRIPTOR}
  IMAGE_BOUND_IMPORT_DESCRIPTOR = _IMAGE_BOUND_IMPORT_DESCRIPTOR;
  {$EXTERNALSYM IMAGE_BOUND_IMPORT_DESCRIPTOR}
  TImageBoundImportDescriptor = IMAGE_BOUND_IMPORT_DESCRIPTOR;
  PImageBoundImportDescriptor = PIMAGE_BOUND_IMPORT_DESCRIPTOR;

  PIMAGE_BOUND_FORWARDER_REF = ^IMAGE_BOUND_FORWARDER_REF;
  {$EXTERNALSYM PIMAGE_BOUND_FORWARDER_REF}
  _IMAGE_BOUND_FORWARDER_REF = record
    TimeDateStamp: DWORD;
    OffsetModuleName: Word;
    Reserved: Word;
  end;
  {$EXTERNALSYM _IMAGE_BOUND_FORWARDER_REF}
  IMAGE_BOUND_FORWARDER_REF = _IMAGE_BOUND_FORWARDER_REF;
  {$EXTERNALSYM IMAGE_BOUND_FORWARDER_REF}
  TImageBoundForwarderRef = IMAGE_BOUND_FORWARDER_REF;
  PImageBoundForwarderRef = PIMAGE_BOUND_FORWARDER_REF;

//
// Resource Format.
//

//
// Resource directory consists of two counts, following by a variable length
// array of directory entries.  The first count is the number of entries at
// beginning of the array that have actual names associated with each entry.
// The entries are in ascending order, case insensitive strings.  The second
// count is the number of entries that immediately follow the named entries.
// This second count identifies the number of entries that have 16-bit integer
// Ids as their name.  These entries are also sorted in ascending order.
//
// This structure allows fast lookup by either name or number, but for any
// given resource entry only one form of lookup is supported, not both.
// This is consistant with the syntax of the .RC file and the .RES file.
//

  PIMAGE_RESOURCE_DIRECTORY = ^IMAGE_RESOURCE_DIRECTORY;
  {$EXTERNALSYM PIMAGE_RESOURCE_DIRECTORY}
  _IMAGE_RESOURCE_DIRECTORY = record
    Characteristics: DWORD;
    TimeDateStamp: DWORD;
    MajorVersion: Word;
    MinorVersion: Word;
    NumberOfNamedEntries: Word;
    NumberOfIdEntries: Word;
    // IMAGE_RESOURCE_DIRECTORY_ENTRY DirectoryEntries[];
  end;
  {$EXTERNALSYM _IMAGE_RESOURCE_DIRECTORY}
  IMAGE_RESOURCE_DIRECTORY = _IMAGE_RESOURCE_DIRECTORY;
  {$EXTERNALSYM IMAGE_RESOURCE_DIRECTORY}
  TImageResourceDirectory = IMAGE_RESOURCE_DIRECTORY;
  PImageResourceDirectory = PIMAGE_RESOURCE_DIRECTORY;

const
  IMAGE_RESOURCE_NAME_IS_STRING    = DWORD($80000000);
  {$EXTERNALSYM IMAGE_RESOURCE_NAME_IS_STRING}
  IMAGE_RESOURCE_DATA_IS_DIRECTORY = DWORD($80000000);
  {$EXTERNALSYM IMAGE_RESOURCE_DATA_IS_DIRECTORY}

//
// Each directory contains the 32-bit Name of the entry and an offset,
// relative to the beginning of the resource directory of the data associated
// with this directory entry.  If the name of the entry is an actual text
// string instead of an integer Id, then the high order bit of the name field
// is set to one and the low order 31-bits are an offset, relative to the
// beginning of the resource directory of the string, which is of type
// IMAGE_RESOURCE_DIRECTORY_STRING.  Otherwise the high bit is clear and the
// low-order 16-bits are the integer Id that identify this resource directory
// entry. If the directory entry is yet another resource directory (i.e. a
// subdirectory), then the high order bit of the offset field will be
// set to indicate this.  Otherwise the high bit is clear and the offset
// field points to a resource data entry.
//

type
  PIMAGE_RESOURCE_DIRECTORY_ENTRY = ^IMAGE_RESOURCE_DIRECTORY_ENTRY;
  {$EXTERNALSYM PIMAGE_RESOURCE_DIRECTORY_ENTRY}
  _IMAGE_RESOURCE_DIRECTORY_ENTRY = record
    case Integer of
      0: (
        // DWORD NameOffset:31;
        // DWORD NameIsString:1;
        NameOffset: DWORD;
        OffsetToData: DWORD
      );
      1: (
        Name: DWORD;
        // DWORD OffsetToDirectory:31;
        // DWORD DataIsDirectory:1;
        OffsetToDirectory: DWORD;
      );
      2: (
        Id: WORD;
      );
  end;
  {$EXTERNALSYM _IMAGE_RESOURCE_DIRECTORY_ENTRY}
  IMAGE_RESOURCE_DIRECTORY_ENTRY = _IMAGE_RESOURCE_DIRECTORY_ENTRY;
  {$EXTERNALSYM IMAGE_RESOURCE_DIRECTORY_ENTRY}
  TImageResourceDirectoryEntry = IMAGE_RESOURCE_DIRECTORY_ENTRY;
  PImageResourceDirectoryEntry = PIMAGE_RESOURCE_DIRECTORY_ENTRY;

//
// For resource directory entries that have actual string names, the Name
// field of the directory entry points to an object of the following type.
// All of these string objects are stored together after the last resource
// directory entry and before the first resource data object.  This minimizes
// the impact of these variable length objects on the alignment of the fixed
// size directory entry objects.
//

type
  PIMAGE_RESOURCE_DIRECTORY_STRING = ^IMAGE_RESOURCE_DIRECTORY_STRING;
  {$EXTERNALSYM PIMAGE_RESOURCE_DIRECTORY_STRING}
  _IMAGE_RESOURCE_DIRECTORY_STRING = record
    Length: Word;
    NameString: array [0..0] of CHAR;
  end;
  {$EXTERNALSYM _IMAGE_RESOURCE_DIRECTORY_STRING}
  IMAGE_RESOURCE_DIRECTORY_STRING = _IMAGE_RESOURCE_DIRECTORY_STRING;
  {$EXTERNALSYM IMAGE_RESOURCE_DIRECTORY_STRING}
  TImageResourceDirectoryString = IMAGE_RESOURCE_DIRECTORY_STRING;
  PImageResourceDirectoryString = PIMAGE_RESOURCE_DIRECTORY_STRING;

  PIMAGE_RESOURCE_DIR_STRING_U = ^IMAGE_RESOURCE_DIR_STRING_U;
  {$EXTERNALSYM PIMAGE_RESOURCE_DIR_STRING_U}
  _IMAGE_RESOURCE_DIR_STRING_U = record
    Length: Word;
    NameString: array [0..0] of WCHAR;
  end;
  {$EXTERNALSYM _IMAGE_RESOURCE_DIR_STRING_U}
  IMAGE_RESOURCE_DIR_STRING_U = _IMAGE_RESOURCE_DIR_STRING_U;
  {$EXTERNALSYM IMAGE_RESOURCE_DIR_STRING_U}
  TImageResourceDirStringU = IMAGE_RESOURCE_DIR_STRING_U;
  PImageResourceDirStringU = PIMAGE_RESOURCE_DIR_STRING_U;

//
// Each resource data entry describes a leaf node in the resource directory
// tree.  It contains an offset, relative to the beginning of the resource
// directory of the data for the resource, a size field that gives the number
// of bytes of data at that offset, a CodePage that should be used when
// decoding code point values within the resource data.  Typically for new
// applications the code page would be the unicode code page.
//

  PIMAGE_RESOURCE_DATA_ENTRY = ^IMAGE_RESOURCE_DATA_ENTRY;
  {$EXTERNALSYM PIMAGE_RESOURCE_DATA_ENTRY}
  _IMAGE_RESOURCE_DATA_ENTRY = record
    OffsetToData: DWORD;
    Size: DWORD;
    CodePage: DWORD;
    Reserved: DWORD;
  end;
  {$EXTERNALSYM _IMAGE_RESOURCE_DATA_ENTRY}
  IMAGE_RESOURCE_DATA_ENTRY = _IMAGE_RESOURCE_DATA_ENTRY;
  {$EXTERNALSYM IMAGE_RESOURCE_DATA_ENTRY}
  TImageResourceDataEntry = IMAGE_RESOURCE_DATA_ENTRY;
  PImageResourceDataEntry = PIMAGE_RESOURCE_DATA_ENTRY;
  
//
// Load Configuration Directory Entry
//

type
  PIMAGE_LOAD_CONFIG_DIRECTORY32 = ^IMAGE_LOAD_CONFIG_DIRECTORY32;
  {$EXTERNALSYM PIMAGE_LOAD_CONFIG_DIRECTORY32}
  IMAGE_LOAD_CONFIG_DIRECTORY32 = record
    Size: DWORD;
    TimeDateStamp: DWORD;
    MajorVersion: WORD;
    MinorVersion: WORD;
    GlobalFlagsClear: DWORD;
    GlobalFlagsSet: DWORD;
    CriticalSectionDefaultTimeout: DWORD;
    DeCommitFreeBlockThreshold: DWORD;
    DeCommitTotalFreeThreshold: DWORD;
    LockPrefixTable: DWORD;            // VA
    MaximumAllocationSize: DWORD;
    VirtualMemoryThreshold: DWORD;
    ProcessHeapFlags: DWORD;
    ProcessAffinityMask: DWORD;
    CSDVersion: WORD;
    Reserved1: WORD;
    EditList: DWORD;                   // VA
    SecurityCookie: DWORD;             // VA
    SEHandlerTable: DWORD;             // VA
    SEHandlerCount: DWORD;
  end;
  {$EXTERNALSYM IMAGE_LOAD_CONFIG_DIRECTORY32}
  TImageLoadConfigDirectory32 = IMAGE_LOAD_CONFIG_DIRECTORY32;
  PImageLoadConfigDirectory32 = PIMAGE_LOAD_CONFIG_DIRECTORY32;

  PIMAGE_LOAD_CONFIG_DIRECTORY64 = ^IMAGE_LOAD_CONFIG_DIRECTORY64;
  {$EXTERNALSYM PIMAGE_LOAD_CONFIG_DIRECTORY64}
  IMAGE_LOAD_CONFIG_DIRECTORY64 = record
    Size: DWORD;
    TimeDateStamp: DWORD;
    MajorVersion: WORD;
    MinorVersion: WORD;
    GlobalFlagsClear: DWORD;
    GlobalFlagsSet: DWORD;
    CriticalSectionDefaultTimeout: DWORD;
    DeCommitFreeBlockThreshold: ULONGLONG;
    DeCommitTotalFreeThreshold: ULONGLONG;
    LockPrefixTable: ULONGLONG;         // VA
    MaximumAllocationSize: ULONGLONG;
    VirtualMemoryThreshold: ULONGLONG;
    ProcessAffinityMask: ULONGLONG;
    ProcessHeapFlags: DWORD;
    CSDVersion: WORD;
    Reserved1: WORD;
    EditList: ULONGLONG;                // VA
    SecurityCookie: ULONGLONG;             // VA
    SEHandlerTable: ULONGLONG;             // VA
    SEHandlerCount: ULONGLONG;
  end;
  {$EXTERNALSYM IMAGE_LOAD_CONFIG_DIRECTORY64}
  TImageLoadConfigDirectory64 = IMAGE_LOAD_CONFIG_DIRECTORY64;
  PImageLoadConfigDirectory64 = PIMAGE_LOAD_CONFIG_DIRECTORY64;

  IMAGE_LOAD_CONFIG_DIRECTORY = IMAGE_LOAD_CONFIG_DIRECTORY32;
  {$EXTERNALSYM IMAGE_LOAD_CONFIG_DIRECTORY}
  PIMAGE_LOAD_CONFIG_DIRECTORY = PIMAGE_LOAD_CONFIG_DIRECTORY32;
  {$EXTERNALSYM PIMAGE_LOAD_CONFIG_DIRECTORY}
  TImageLoadConfigDirectory = TImageLoadConfigDirectory32;
  PImageLoadConfigDirectory = PImageLoadConfigDirectory32;

// line 6802

//
// Debug Format
//
(*
type
  PIMAGE_DEBUG_DIRECTORY = ^IMAGE_DEBUG_DIRECTORY;
  {$EXTERNALSYM PIMAGE_DEBUG_DIRECTORY}
  _IMAGE_DEBUG_DIRECTORY = record
    Characteristics: DWORD;
    TimeDateStamp: DWORD;
    MajorVersion: Word;
    MinorVersion: Word;
    Type_: DWORD;
    SizeOfData: DWORD;
    AddressOfRawData: DWORD;
    PointerToRawData: DWORD;
  end;
  {$EXTERNALSYM _IMAGE_DEBUG_DIRECTORY}
  IMAGE_DEBUG_DIRECTORY = _IMAGE_DEBUG_DIRECTORY;
  {$EXTERNALSYM IMAGE_DEBUG_DIRECTORY}
  TImageDebugDirectory = IMAGE_DEBUG_DIRECTORY;
  PImageDebugDirectory = PIMAGE_DEBUG_DIRECTORY;

const
  IMAGE_DEBUG_TYPE_UNKNOWN       = 0;
  {$EXTERNALSYM IMAGE_DEBUG_TYPE_UNKNOWN}
  IMAGE_DEBUG_TYPE_COFF          = 1;
  {$EXTERNALSYM IMAGE_DEBUG_TYPE_COFF}
  IMAGE_DEBUG_TYPE_CODEVIEW      = 2;
  {$EXTERNALSYM IMAGE_DEBUG_TYPE_CODEVIEW}
  IMAGE_DEBUG_TYPE_FPO           = 3;
  {$EXTERNALSYM IMAGE_DEBUG_TYPE_FPO}
  IMAGE_DEBUG_TYPE_MISC          = 4;
  {$EXTERNALSYM IMAGE_DEBUG_TYPE_MISC}
  IMAGE_DEBUG_TYPE_EXCEPTION     = 5;
  {$EXTERNALSYM IMAGE_DEBUG_TYPE_EXCEPTION}
  IMAGE_DEBUG_TYPE_FIXUP         = 6;
  {$EXTERNALSYM IMAGE_DEBUG_TYPE_FIXUP}
  IMAGE_DEBUG_TYPE_OMAP_TO_SRC   = 7;
  {$EXTERNALSYM IMAGE_DEBUG_TYPE_OMAP_TO_SRC}
  IMAGE_DEBUG_TYPE_OMAP_FROM_SRC = 8;
  {$EXTERNALSYM IMAGE_DEBUG_TYPE_OMAP_FROM_SRC}
  IMAGE_DEBUG_TYPE_BORLAND       = 9;
  {$EXTERNALSYM IMAGE_DEBUG_TYPE_BORLAND}
  IMAGE_DEBUG_TYPE_RESERVED10    = 10;
  {$EXTERNALSYM IMAGE_DEBUG_TYPE_RESERVED10}
  IMAGE_DEBUG_TYPE_CLSID         = 11;
  {$EXTERNALSYM IMAGE_DEBUG_TYPE_CLSID}
*)
type
  PIMAGE_COFF_SYMBOLS_HEADER = ^IMAGE_COFF_SYMBOLS_HEADER;
  {$EXTERNALSYM PIMAGE_COFF_SYMBOLS_HEADER}
  _IMAGE_COFF_SYMBOLS_HEADER = record
    NumberOfSymbols: DWORD;
    LvaToFirstSymbol: DWORD;
    NumberOfLinenumbers: DWORD;
    LvaToFirstLinenumber: DWORD;
    RvaToFirstByteOfCode: DWORD;
    RvaToLastByteOfCode: DWORD;
    RvaToFirstByteOfData: DWORD;
    RvaToLastByteOfData: DWORD;
  end;
  {$EXTERNALSYM _IMAGE_COFF_SYMBOLS_HEADER}
  IMAGE_COFF_SYMBOLS_HEADER = _IMAGE_COFF_SYMBOLS_HEADER;
  {$EXTERNALSYM IMAGE_COFF_SYMBOLS_HEADER}
  TImageCoffSymbolsHeader = IMAGE_COFF_SYMBOLS_HEADER;
  PImageCoffSymbolsHeader = PIMAGE_COFF_SYMBOLS_HEADER;

const
  FRAME_FPO    = 0;
  {$EXTERNALSYM FRAME_FPO}
  FRAME_TRAP   = 1;
  {$EXTERNALSYM FRAME_TRAP}
  FRAME_TSS    = 2;
  {$EXTERNALSYM FRAME_TSS}
  FRAME_NONFPO = 3;
  {$EXTERNALSYM FRAME_NONFPO}

  FPOFLAGS_PROLOG   = $00FF; // # bytes in prolog
  FPOFLAGS_REGS     = $0700; // # regs saved
  FPOFLAGS_HAS_SEH  = $0800; // TRUE if SEH in func
  FPOFLAGS_USE_BP   = $1000; // TRUE if EBP has been allocated
  FPOFLAGS_RESERVED = $2000; // reserved for future use
  FPOFLAGS_FRAME    = $C000; // frame type

type
  PFPO_DATA = ^FPO_DATA;
  {$EXTERNALSYM PFPO_DATA}
  _FPO_DATA = record
    ulOffStart: DWORD;       // offset 1st byte of function code
    cbProcSize: DWORD;       // # bytes in function
    cdwLocals: DWORD;        // # bytes in locals/4
    cdwParams: WORD;         // # bytes in params/4
    Flags: WORD;
  end;
  {$EXTERNALSYM _FPO_DATA}
  FPO_DATA = _FPO_DATA;
  {$EXTERNALSYM FPO_DATA}
  TFpoData = FPO_DATA;
  PFpoData = PFPO_DATA;

const
  SIZEOF_RFPO_DATA = 16;
  {$EXTERNALSYM SIZEOF_RFPO_DATA}

  IMAGE_DEBUG_MISC_EXENAME = 1;
  {$EXTERNALSYM IMAGE_DEBUG_MISC_EXENAME}

type
  PIMAGE_DEBUG_MISC = ^IMAGE_DEBUG_MISC;
  {$EXTERNALSYM PIMAGE_DEBUG_MISC}
  _IMAGE_DEBUG_MISC = record
    DataType: DWORD;   // type of misc data, see defines
    Length: DWORD;     // total length of record, rounded to four byte multiple.
    Unicode: ByteBool; // TRUE if data is unicode string
    Reserved: array [0..2] of Byte;
    Data: array [0..0] of Byte; // Actual data
  end;
  {$EXTERNALSYM _IMAGE_DEBUG_MISC}
  IMAGE_DEBUG_MISC = _IMAGE_DEBUG_MISC;
  {$EXTERNALSYM IMAGE_DEBUG_MISC}
  TImageDebugMisc = IMAGE_DEBUG_MISC;
  PImageDebugMisc = PIMAGE_DEBUG_MISC;

//
// Function table extracted from MIPS/ALPHA/IA64 images.  Does not contain
// information needed only for runtime support.  Just those fields for
// each entry needed by a debugger.
//

  PIMAGE_FUNCTION_ENTRY = ^IMAGE_FUNCTION_ENTRY;
  {$EXTERNALSYM PIMAGE_FUNCTION_ENTRY}
  _IMAGE_FUNCTION_ENTRY = record
    StartingAddress: DWORD;
    EndingAddress: DWORD;
    EndOfPrologue: DWORD;
  end;
  {$EXTERNALSYM _IMAGE_FUNCTION_ENTRY}
  IMAGE_FUNCTION_ENTRY = _IMAGE_FUNCTION_ENTRY;
  {$EXTERNALSYM IMAGE_FUNCTION_ENTRY}
  TImageFunctionEntry = IMAGE_FUNCTION_ENTRY;
  PImageFunctionEntry = PIMAGE_FUNCTION_ENTRY;

  PIMAGE_FUNCTION_ENTRY64 = ^IMAGE_FUNCTION_ENTRY64;
  {$EXTERNALSYM PIMAGE_FUNCTION_ENTRY64}
  _IMAGE_FUNCTION_ENTRY64 = record
    StartingAddress: ULONGLONG;
    EndingAddress: ULONGLONG;
    case Integer of
      0: (EndOfPrologue: ULONGLONG);
      1: (UnwindInfoAddress: ULONGLONG);
  end;
  {$EXTERNALSYM _IMAGE_FUNCTION_ENTRY64}
  IMAGE_FUNCTION_ENTRY64 = _IMAGE_FUNCTION_ENTRY64;
  {$EXTERNALSYM IMAGE_FUNCTION_ENTRY64}
  TImageFunctionEntry64 = IMAGE_FUNCTION_ENTRY64;
  PImageFunctionEntry64 = PIMAGE_FUNCTION_ENTRY64;

//
// Debugging information can be stripped from an image file and placed
// in a separate .DBG file, whose file name part is the same as the
// image file name part (e.g. symbols for CMD.EXE could be stripped
// and placed in CMD.DBG).  This is indicated by the IMAGE_FILE_DEBUG_STRIPPED
// flag in the Characteristics field of the file header.  The beginning of
// the .DBG file contains the following structure which captures certain
// information from the image file.  This allows a debug to proceed even if
// the original image file is not accessable.  This header is followed by
// zero of more IMAGE_SECTION_HEADER structures, followed by zero or more
// IMAGE_DEBUG_DIRECTORY structures.  The latter structures and those in
// the image file contain file offsets relative to the beginning of the
// .DBG file.
//
// If symbols have been stripped from an image, the IMAGE_DEBUG_MISC structure
// is left in the image file, but not mapped.  This allows a debugger to
// compute the name of the .DBG file, from the name of the image in the
// IMAGE_DEBUG_MISC structure.
//

  PIMAGE_SEPARATE_DEBUG_HEADER = ^IMAGE_SEPARATE_DEBUG_HEADER;
  {$EXTERNALSYM PIMAGE_SEPARATE_DEBUG_HEADER}
  _IMAGE_SEPARATE_DEBUG_HEADER = record
    Signature: Word;
    Flags: Word;
    Machine: Word;
    Characteristics: Word;
    TimeDateStamp: DWORD;
    CheckSum: DWORD;
    ImageBase: DWORD;
    SizeOfImage: DWORD;
    NumberOfSections: DWORD;
    ExportedNamesSize: DWORD;
    DebugDirectorySize: DWORD;
    SectionAlignment: DWORD;
    Reserved: array [0..1] of DWORD;
  end;
  {$EXTERNALSYM _IMAGE_SEPARATE_DEBUG_HEADER}
  IMAGE_SEPARATE_DEBUG_HEADER = _IMAGE_SEPARATE_DEBUG_HEADER;
  {$EXTERNALSYM IMAGE_SEPARATE_DEBUG_HEADER}
  TImageSeparateDebugHeader = IMAGE_SEPARATE_DEBUG_HEADER;
  PImageSeparateDebugHeader = PIMAGE_SEPARATE_DEBUG_HEADER;

  _NON_PAGED_DEBUG_INFO = record
    Signature: WORD;
    Flags: WORD;
    Size: DWORD;
    Machine: WORD;
    Characteristics: WORD;
    TimeDateStamp: DWORD;
    CheckSum: DWORD;
    SizeOfImage: DWORD;
    ImageBase: ULONGLONG;
    //DebugDirectorySize
    //IMAGE_DEBUG_DIRECTORY
  end;
  {$EXTERNALSYM _NON_PAGED_DEBUG_INFO}
  NON_PAGED_DEBUG_INFO = _NON_PAGED_DEBUG_INFO;
  {$EXTERNALSYM NON_PAGED_DEBUG_INFO}
  PNON_PAGED_DEBUG_INFO = ^NON_PAGED_DEBUG_INFO;
  {$EXTERNALSYM PNON_PAGED_DEBUG_INFO}

const
  IMAGE_SEPARATE_DEBUG_SIGNATURE = $4944;
  {$EXTERNALSYM IMAGE_SEPARATE_DEBUG_SIGNATURE}
  NON_PAGED_DEBUG_SIGNATURE      = $494E;
  {$EXTERNALSYM NON_PAGED_DEBUG_SIGNATURE}

  IMAGE_SEPARATE_DEBUG_FLAGS_MASK = $8000;
  {$EXTERNALSYM IMAGE_SEPARATE_DEBUG_FLAGS_MASK}
  IMAGE_SEPARATE_DEBUG_MISMATCH   = $8000; // when DBG was updated, the old checksum didn't match.
  {$EXTERNALSYM IMAGE_SEPARATE_DEBUG_MISMATCH}

//
//  The .arch section is made up of headers, each describing an amask position/value
//  pointing to an array of IMAGE_ARCHITECTURE_ENTRY's.  Each "array" (both the header
//  and entry arrays) are terminiated by a quadword of 0xffffffffL.
//
//  NOTE: There may be quadwords of 0 sprinkled around and must be skipped.
//

const
  IAHMASK_VALUE = $00000001; // 1 -> code section depends on mask bit
                             // 0 -> new instruction depends on mask bit
  IAHMASK_MBZ7  = $000000FE; // MBZ
  IAHMASK_SHIFT = $0000FF00; // Amask bit in question for this fixup
  IAHMASK_MBZ16 = DWORD($FFFF0000); // MBZ

type
  PIMAGE_ARCHITECTURE_HEADER = ^IMAGE_ARCHITECTURE_HEADER;
  {$EXTERNALSYM PIMAGE_ARCHITECTURE_HEADER}
  _ImageArchitectureHeader = record
    Mask: DWORD;
    FirstEntryRVA: DWORD;    // RVA into .arch section to array of ARCHITECTURE_ENTRY's
  end;
  {$EXTERNALSYM _ImageArchitectureHeader}
  IMAGE_ARCHITECTURE_HEADER = _ImageArchitectureHeader;
  {$EXTERNALSYM IMAGE_ARCHITECTURE_HEADER}
  TImageArchitectureHeader = IMAGE_ARCHITECTURE_HEADER;
  PImageArchitectureHeader = PIMAGE_ARCHITECTURE_HEADER;

  PIMAGE_ARCHITECTURE_ENTRY = ^IMAGE_ARCHITECTURE_ENTRY;
  {$EXTERNALSYM PIMAGE_ARCHITECTURE_ENTRY}
  _ImageArchitectureEntry = record
    FixupInstRVA: DWORD;                         // RVA of instruction to fixup
    NewInst: DWORD;                              // fixup instruction (see alphaops.h)
  end;
  {$EXTERNALSYM _ImageArchitectureEntry}
  IMAGE_ARCHITECTURE_ENTRY = _ImageArchitectureEntry;
  {$EXTERNALSYM IMAGE_ARCHITECTURE_ENTRY}
  TImageArchitectureEntry = IMAGE_ARCHITECTURE_ENTRY;
  PImageArchitectureEntry = PIMAGE_ARCHITECTURE_ENTRY;

// #include "poppack.h"                // Back to the initial value

// The following structure defines the new import object.  Note the values of the first two fields,
// which must be set as stated in order to differentiate old and new import members.
// Following this structure, the linker emits two null-terminated strings used to recreate the
// import at the time of use.  The first string is the import's name, the second is the dll's name.

const
  IMPORT_OBJECT_HDR_SIG2 = $ffff;
  {$EXTERNALSYM IMPORT_OBJECT_HDR_SIG2}

const
  IOHFLAGS_TYPE = $0003;      // IMPORT_TYPE
  IAHFLAGS_NAMETYPE = $001C;  // IMPORT_NAME_TYPE
  IAHFLAGS_RESERVED = $FFE0;  // Reserved. Must be zero.

type
  PImportObjectHeader = ^IMPORT_OBJECT_HEADER;
  IMPORT_OBJECT_HEADER = record
    Sig1: WORD;                       // Must be IMAGE_FILE_MACHINE_UNKNOWN
    Sig2: WORD;                       // Must be IMPORT_OBJECT_HDR_SIG2.
    Version: WORD;
    Machine: WORD;
    TimeDateStamp: DWORD;             // Time/date stamp
    SizeOfData: DWORD;                // particularly useful for incremental links
    OrdinalOrHint: record
    case Integer of
      0: (Ordinal: WORD);             // if grf & IMPORT_OBJECT_ORDINAL
      1: (Flags: DWORD);
    end;
    Flags: WORD;
    //WORD    Type : 2;                   // IMPORT_TYPE
    //WORD    NameType : 3;               // IMPORT_NAME_TYPE
    //WORD    Reserved : 11;              // Reserved. Must be zero.
  end;
  {$EXTERNALSYM IMPORT_OBJECT_HEADER}
  TImportObjectHeader = IMPORT_OBJECT_HEADER;

  IMPORT_OBJECT_TYPE = (IMPORT_OBJECT_CODE, IMPORT_OBJECT_DATA, IMPORT_OBJECT_CONST);
  {$EXTERNALSYM IMPORT_OBJECT_TYPE}
  TImportObjectType = IMPORT_OBJECT_TYPE;

  IMPORT_OBJECT_NAME_TYPE = (
    IMPORT_OBJECT_ORDINAL,          // Import by ordinal
    IMPORT_OBJECT_NAME,             // Import name == public symbol name.
    IMPORT_OBJECT_NAME_NO_PREFIX,   // Import name == public symbol name skipping leading ?, @, or optionally _.
    IMPORT_OBJECT_NAME_UNDECORATE); // Import name == public symbol name skipping leading ?, @, or optionally _
                                    // and truncating at first @
  {$EXTERNALSYM IMPORT_OBJECT_NAME_TYPE}
  TImportObjectNameType = IMPORT_OBJECT_NAME_TYPE;

  ReplacesCorHdrNumericDefines = DWORD;
  {$EXTERNALSYM ReplacesCorHdrNumericDefines}

const

// COM+ Header entry point flags.

  COMIMAGE_FLAGS_ILONLY               = $00000001;
  {$EXTERNALSYM COMIMAGE_FLAGS_ILONLY}
  COMIMAGE_FLAGS_32BITREQUIRED        = $00000002;
  {$EXTERNALSYM COMIMAGE_FLAGS_32BITREQUIRED}
  COMIMAGE_FLAGS_IL_LIBRARY           = $00000004;
  {$EXTERNALSYM COMIMAGE_FLAGS_IL_LIBRARY}
  COMIMAGE_FLAGS_STRONGNAMESIGNED     = $00000008;
  {$EXTERNALSYM COMIMAGE_FLAGS_STRONGNAMESIGNED}
  COMIMAGE_FLAGS_TRACKDEBUGDATA       = $00010000;
  {$EXTERNALSYM COMIMAGE_FLAGS_TRACKDEBUGDATA}

// Version flags for image.

  COR_VERSION_MAJOR_V2                = 2;
  {$EXTERNALSYM COR_VERSION_MAJOR_V2}
  COR_VERSION_MAJOR                   = COR_VERSION_MAJOR_V2;
  {$EXTERNALSYM COR_VERSION_MAJOR}
  COR_VERSION_MINOR                   = 0;
  {$EXTERNALSYM COR_VERSION_MINOR}
  COR_DELETED_NAME_LENGTH             = 8;
  {$EXTERNALSYM COR_DELETED_NAME_LENGTH}
  COR_VTABLEGAP_NAME_LENGTH           = 8;
  {$EXTERNALSYM COR_VTABLEGAP_NAME_LENGTH}

// Maximum size of a NativeType descriptor.

  NATIVE_TYPE_MAX_CB                  = 1;
  {$EXTERNALSYM NATIVE_TYPE_MAX_CB}
  COR_ILMETHOD_SECT_SMALL_MAX_DATASIZE= $FF;
  {$EXTERNALSYM COR_ILMETHOD_SECT_SMALL_MAX_DATASIZE}

// #defines for the MIH FLAGS

  IMAGE_COR_MIH_METHODRVA             = $01;
  {$EXTERNALSYM IMAGE_COR_MIH_METHODRVA}
  IMAGE_COR_MIH_EHRVA                 = $02;
  {$EXTERNALSYM IMAGE_COR_MIH_EHRVA}
  IMAGE_COR_MIH_BASICBLOCK            = $08;
  {$EXTERNALSYM IMAGE_COR_MIH_BASICBLOCK}

// V-table constants

  COR_VTABLE_32BIT                    = $01;          // V-table slots are 32-bits in size.
  {$EXTERNALSYM COR_VTABLE_32BIT}
  COR_VTABLE_64BIT                    = $02;          // V-table slots are 64-bits in size.
  {$EXTERNALSYM COR_VTABLE_64BIT}
  COR_VTABLE_FROM_UNMANAGED           = $04;          // If set, transition from unmanaged.
  {$EXTERNALSYM COR_VTABLE_FROM_UNMANAGED}
  COR_VTABLE_CALL_MOST_DERIVED        = $10;          // Call most derived method described by
  {$EXTERNALSYM COR_VTABLE_CALL_MOST_DERIVED}

// EATJ constants

  IMAGE_COR_EATJ_THUNK_SIZE           = 32;            // Size of a jump thunk reserved range.
  {$EXTERNALSYM IMAGE_COR_EATJ_THUNK_SIZE}

// Max name lengths
// Change to unlimited name lengths.

  MAX_CLASS_NAME                      = 1024;
  {$EXTERNALSYM MAX_CLASS_NAME}
  MAX_PACKAGE_NAME                    = 1024;
  {$EXTERNALSYM MAX_PACKAGE_NAME}

{$ENDIF ~CLR}

// COM+ 2.0 header structure.

type
  IMAGE_COR20_HEADER = record

    // Header versioning

    cb: DWORD;
    MajorRuntimeVersion: WORD;
    MinorRuntimeVersion: WORD;

    // Symbol table and startup information

    MetaData: IMAGE_DATA_DIRECTORY;
    Flags: DWORD;
    EntryPointToken: DWORD;

    // Binding information

    Resources: IMAGE_DATA_DIRECTORY;
    StrongNameSignature: IMAGE_DATA_DIRECTORY;

    // Regular fixup and binding information

    CodeManagerTable: IMAGE_DATA_DIRECTORY;
    VTableFixups: IMAGE_DATA_DIRECTORY;
    ExportAddressTableJumps: IMAGE_DATA_DIRECTORY;

    // Precompiled image info (internal use only - set to zero)

    ManagedNativeHeader: IMAGE_DATA_DIRECTORY;
  end;
  {$IFDEF COMPILER6_UP}
  {$EXTERNALSYM IMAGE_COR20_HEADER}
  {$ENDIF COMPILER6_UP}
  PIMAGE_COR20_HEADER = ^IMAGE_COR20_HEADER;
  {$IFDEF COMPILER6_UP}
  {$EXTERNALSYM PIMAGE_COR20_HEADER}
  {$ENDIF COMPILER6_UP}
  TImageCor20Header = IMAGE_COR20_HEADER;
  PImageCor20Header = PIMAGE_COR20_HEADER;

// line 7351

const
  COMPRESSION_FORMAT_NONE     = ($0000);
  {$EXTERNALSYM COMPRESSION_FORMAT_NONE}
  COMPRESSION_FORMAT_DEFAULT  = ($0001);
  {$EXTERNALSYM COMPRESSION_FORMAT_DEFAULT}
  COMPRESSION_FORMAT_LZNT1    = ($0002);
  {$EXTERNALSYM COMPRESSION_FORMAT_LZNT1}
  COMPRESSION_ENGINE_STANDARD = ($0000);
  {$EXTERNALSYM COMPRESSION_ENGINE_STANDARD}
  COMPRESSION_ENGINE_MAXIMUM  = ($0100);
  {$EXTERNALSYM COMPRESSION_ENGINE_MAXIMUM}
  COMPRESSION_ENGINE_HIBER    = ($0200);
  {$EXTERNALSYM COMPRESSION_ENGINE_HIBER}

// line 7462

type
  POSVERSIONINFOEXA = ^OSVERSIONINFOEXA;
  {$EXTERNALSYM POSVERSIONINFOEXA}
  _OSVERSIONINFOEXA = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array [0..127] of CHAR;     // Maintenance string for PSS usage
    wServicePackMajor: WORD;
    wServicePackMinor: WORD;
    wSuiteMask: WORD;
    wProductType: BYTE;
    wReserved: BYTE;
  end;
  {$EXTERNALSYM _OSVERSIONINFOEXA}
  OSVERSIONINFOEXA = _OSVERSIONINFOEXA;
  {$EXTERNALSYM OSVERSIONINFOEXA}
  LPOSVERSIONINFOEXA = ^OSVERSIONINFOEXA;
  {$EXTERNALSYM LPOSVERSIONINFOEXA}
  TOSVersionInfoExA = _OSVERSIONINFOEXA;

  POSVERSIONINFOEXW = ^OSVERSIONINFOEXW;
  {$EXTERNALSYM POSVERSIONINFOEXW}
  _OSVERSIONINFOEXW = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array [0..127] of WCHAR;     // Maintenance string for PSS usage
    wServicePackMajor: WORD;
    wServicePackMinor: WORD;
    wSuiteMask: WORD;
    wProductType: BYTE;
    wReserved: BYTE;
  end;
  {$EXTERNALSYM _OSVERSIONINFOEXW}
  OSVERSIONINFOEXW = _OSVERSIONINFOEXW;
  {$EXTERNALSYM OSVERSIONINFOEXW}
  LPOSVERSIONINFOEXW = ^OSVERSIONINFOEXW;
  {$EXTERNALSYM LPOSVERSIONINFOEXW}
  RTL_OSVERSIONINFOEXW = _OSVERSIONINFOEXW;
  {$EXTERNALSYM RTL_OSVERSIONINFOEXW}
  PRTL_OSVERSIONINFOEXW = ^RTL_OSVERSIONINFOEXW;
  {$EXTERNALSYM PRTL_OSVERSIONINFOEXW}
  TOSVersionInfoExW = _OSVERSIONINFOEXW;

{$IFDEF UNICODE}

  OSVERSIONINFOEX = OSVERSIONINFOEXW;
  {$EXTERNALSYM OSVERSIONINFOEX}
  POSVERSIONINFOEX = POSVERSIONINFOEXW;
  {$EXTERNALSYM POSVERSIONINFOEX}
  LPOSVERSIONINFOEX = LPOSVERSIONINFOEXW;
  {$EXTERNALSYM LPOSVERSIONINFOEX}
  TOSVersionInfoEx = TOSVersionInfoExW;

{$ELSE}

  OSVERSIONINFOEX = OSVERSIONINFOEXA;
  {$EXTERNALSYM OSVERSIONINFOEX}
  POSVERSIONINFOEX = POSVERSIONINFOEXA;
  {$EXTERNALSYM POSVERSIONINFOEX}
  LPOSVERSIONINFOEX = LPOSVERSIONINFOEXA;
  {$EXTERNALSYM LPOSVERSIONINFOEX}
  TOSVersionInfoEx = TOSVersionInfoExA;  

{$ENDIF}

//
// RtlVerifyVersionInfo() conditions
//

const
  VER_EQUAL         = 1;
  {$EXTERNALSYM VER_EQUAL}
  VER_GREATER       = 2;
  {$EXTERNALSYM VER_GREATER}
  VER_GREATER_EQUAL = 3;
  {$EXTERNALSYM VER_GREATER_EQUAL}
  VER_LESS          = 4;
  {$EXTERNALSYM VER_LESS}
  VER_LESS_EQUAL    = 5;
  {$EXTERNALSYM VER_LESS_EQUAL}
  VER_AND           = 6;
  {$EXTERNALSYM VER_AND}
  VER_OR            = 7;
  {$EXTERNALSYM VER_OR}

  VER_CONDITION_MASK              = 7;
  {$EXTERNALSYM VER_CONDITION_MASK}
  VER_NUM_BITS_PER_CONDITION_MASK = 3;
  {$EXTERNALSYM VER_NUM_BITS_PER_CONDITION_MASK}

//
// RtlVerifyVersionInfo() type mask bits
//

  VER_MINORVERSION     = $0000001;
  {$EXTERNALSYM VER_MINORVERSION}
  VER_MAJORVERSION     = $0000002;
  {$EXTERNALSYM VER_MAJORVERSION}
  VER_BUILDNUMBER      = $0000004;
  {$EXTERNALSYM VER_BUILDNUMBER}
  VER_PLATFORMID       = $0000008;
  {$EXTERNALSYM VER_PLATFORMID}
  VER_SERVICEPACKMINOR = $0000010;
  {$EXTERNALSYM VER_SERVICEPACKMINOR}
  VER_SERVICEPACKMAJOR = $0000020;
  {$EXTERNALSYM VER_SERVICEPACKMAJOR}
  VER_SUITENAME        = $0000040;
  {$EXTERNALSYM VER_SUITENAME}
  VER_PRODUCT_TYPE     = $0000080;
  {$EXTERNALSYM VER_PRODUCT_TYPE}

//
// RtlVerifyVersionInfo() os product type values
//

  VER_NT_WORKSTATION       = $0000001;
  {$EXTERNALSYM VER_NT_WORKSTATION}
  VER_NT_DOMAIN_CONTROLLER = $0000002;
  {$EXTERNALSYM VER_NT_DOMAIN_CONTROLLER}
  VER_NT_SERVER            = $0000003;
  {$EXTERNALSYM VER_NT_SERVER}

//
// dwPlatformId defines:
//

  VER_PLATFORM_WIN32s        = 0;
  {$EXTERNALSYM VER_PLATFORM_WIN32s}
  VER_PLATFORM_WIN32_WINDOWS = 1;
  {$EXTERNALSYM VER_PLATFORM_WIN32_WINDOWS}
  VER_PLATFORM_WIN32_NT      = 2;
  {$EXTERNALSYM VER_PLATFORM_WIN32_NT}

const
//
//
// Predefined Value Types.
//

  REG_NONE      = ( 0 ); // No value type
  {$EXTERNALSYM REG_NONE}
  REG_SZ        = ( 1 ); // Unicode nul terminated string
  {$EXTERNALSYM REG_SZ}
  REG_EXPAND_SZ = ( 2 ); // Unicode nul terminated string
  {$EXTERNALSYM REG_EXPAND_SZ}
                                            // (with environment variable references)
  REG_BINARY                     = ( 3 ); // Free form binary
  {$EXTERNALSYM REG_BINARY}
  REG_DWORD                      = ( 4 ); // 32-bit number
  {$EXTERNALSYM REG_DWORD}
  REG_DWORD_LITTLE_ENDIAN        = ( 4 ); // 32-bit number (same as REG_DWORD)
  {$EXTERNALSYM REG_DWORD_LITTLE_ENDIAN}
  REG_DWORD_BIG_ENDIAN           = ( 5 ); // 32-bit number
  {$EXTERNALSYM REG_DWORD_BIG_ENDIAN}
  REG_LINK                       = ( 6 ); // Symbolic Link (unicode)
  {$EXTERNALSYM REG_LINK}
  REG_MULTI_SZ                   = ( 7 ); // Multiple Unicode strings
  {$EXTERNALSYM REG_MULTI_SZ}
  REG_RESOURCE_LIST              = ( 8 ); // Resource list in the resource map
  {$EXTERNALSYM REG_RESOURCE_LIST}
  REG_FULL_RESOURCE_DESCRIPTOR   = ( 9 ); // Resource list in the hardware description
  {$EXTERNALSYM REG_FULL_RESOURCE_DESCRIPTOR}
  REG_RESOURCE_REQUIREMENTS_LIST = ( 10 );
  {$EXTERNALSYM REG_RESOURCE_REQUIREMENTS_LIST}
  REG_QWORD                      = ( 11 ); // 64-bit number
  {$EXTERNALSYM REG_QWORD}
  REG_QWORD_LITTLE_ENDIAN        = ( 11 ); // 64-bit number (same as REG_QWORD)
  {$EXTERNALSYM REG_QWORD_LITTLE_ENDIAN}

// line 160

//
// File creation flags must start at the high end since they
// are combined with the attributes
//

const
  FILE_FLAG_WRITE_THROUGH      = DWORD($80000000);
  {$EXTERNALSYM FILE_FLAG_WRITE_THROUGH}
  FILE_FLAG_OVERLAPPED         = $40000000;
  {$EXTERNALSYM FILE_FLAG_OVERLAPPED}
  FILE_FLAG_NO_BUFFERING       = $20000000;
  {$EXTERNALSYM FILE_FLAG_NO_BUFFERING}
  FILE_FLAG_RANDOM_ACCESS      = $10000000;
  {$EXTERNALSYM FILE_FLAG_RANDOM_ACCESS}
  FILE_FLAG_SEQUENTIAL_SCAN    = $08000000;
  {$EXTERNALSYM FILE_FLAG_SEQUENTIAL_SCAN}
  FILE_FLAG_DELETE_ON_CLOSE    = $04000000;
  {$EXTERNALSYM FILE_FLAG_DELETE_ON_CLOSE}
  FILE_FLAG_BACKUP_SEMANTICS   = $02000000;
  {$EXTERNALSYM FILE_FLAG_BACKUP_SEMANTICS}
  FILE_FLAG_POSIX_SEMANTICS    = $01000000;
  {$EXTERNALSYM FILE_FLAG_POSIX_SEMANTICS}
  FILE_FLAG_OPEN_REPARSE_POINT = $00200000;
  {$EXTERNALSYM FILE_FLAG_OPEN_REPARSE_POINT}
  FILE_FLAG_OPEN_NO_RECALL     = $00100000;
  {$EXTERNALSYM FILE_FLAG_OPEN_NO_RECALL}
  FILE_FLAG_FIRST_PIPE_INSTANCE = $00080000;
  {$EXTERNALSYM FILE_FLAG_FIRST_PIPE_INSTANCE}

// line 3189
  

function BackupSeek(hFile: THandle; dwLowBytesToSeek, dwHighBytesToSeek: DWORD;
  out lpdwLowByteSeeked, lpdwHighByteSeeked: DWORD;
  var lpContext: {$IFDEF CLR}IntPtr{$ELSE}Pointer{$ENDIF}): BOOL; stdcall;
  {$IFDEF CLR}external kernel32 name 'BackupSeek';{$ENDIF}
{$EXTERNALSYM BackupSeek}

// line 5454

function AdjustTokenPrivileges(TokenHandle: THandle; DisableAllPrivileges: BOOL;
  const NewState: TTokenPrivileges; BufferLength: DWORD;
  {$IFDEF CLR}
  out PreviousState: TTokenPrivileges;
  out ReturnLength: DWORD
  {$ELSE}
  PreviousState: PTokenPrivileges;
  ReturnLength: PDWORD
  {$ENDIF CLR}
  ): BOOL; stdcall;
  {$IFDEF CLR} external advapi32 name 'AdjustTokenPrivileges';{$ENDIF}
{$EXTERNALSYM AdjustTokenPrivileges}

{
From: Ray Lischner <delphi.at.tempest-sw.com@nospam.com>
Subject: CreateMutex bug
Date: 1999/12/10
Message-ID: <e7tQOEYjVVpXzy6tIn=yUyJnBZXw@4ax.com>#1/1
Content-Transfer-Encoding: 7bit
Organization: Tempest Software, Inc., Corvallis, Oregon
Content-Type: text/plain; charset=us-ascii
Mime-Version: 1.0
Newsgroups: borland.public.delphi.winapi


Windows NT 4 has a bug in CreateMutex. The second argument is documented
to be a BOOL, but in truth, the CreateMutex interprets 1 as True and all
other values as False. (Do I detect an "if (bInitialOwner == TRUE)" in
the implementation of CreateMutex?)

The problem is that Delphi declares CreateMutex according to the
documentation, so bInitialOwner is declared as LongBool. Delphi maps
True values to $FFFFFFFF, which should work, but doesn't in this case.

My workaround is to declare CreateMutex with a LongInt as the second
argument, and pass the value 1 for True.

I have not had this problem on Windows 98.
--
Ray Lischner, author of Delphi in a Nutshell (coming later this year)
http://www.bardware.com and http://www.tempest-sw.com
}
{$IFNDEF CLR}
function CreateMutex(lpMutexAttributes: PSecurityAttributes; bInitialOwner: DWORD; lpName: PChar): THandle; stdcall;
{$EXTERNALSYM CreateMutex}
{$ENDIF ~CLR}

// alternative conversion for WinNT 4.0 SP6 and later (OSVersionInfoEx instead of OSVersionInfo)
{$EXTERNALSYM GetVersionEx}
function GetVersionEx(var lpVersionInformation: TOSVersionInfoEx): BOOL; stdcall; overload;
  {$IFDEF CLR}external version name 'GetVersionEx';{$ENDIF}
{$IFNDEF CLR}
{$EXTERNALSYM GetVersionEx}
function GetVersionEx(lpVersionInformation: POSVERSIONINFOEX): BOOL; stdcall; overload;
  {$IFDEF SUPPORTS_DEPRECATED} deprecated; {$ENDIF}

// line 3585

function SetWaitableTimer(hTimer: THandle; var lpDueTime: TLargeInteger;
  lPeriod: Longint; pfnCompletionRoutine: TFNTimerAPCRoutine;
  lpArgToCompletionRoutine: Pointer; fResume: BOOL): BOOL; stdcall;
  {$EXTERNALSYM SetWaitableTimer}

// WinBase.h line 8839

function SetFileSecurityA(lpFileName: LPCSTR; SecurityInformation: SECURITY_INFORMATION;
  pSecurityDescriptor: PSECURITY_DESCRIPTOR): BOOL; stdcall;
{$EXTERNALSYM SetFileSecurityA}
function SetFileSecurityW(lpFileName: LPCWSTR; SecurityInformation: SECURITY_INFORMATION;
  pSecurityDescriptor: PSECURITY_DESCRIPTOR): BOOL; stdcall;
{$EXTERNALSYM SetFileSecurityW}
function SetFileSecurity(lpFileName: LPCTSTR; SecurityInformation: SECURITY_INFORMATION;
  pSecurityDescriptor: PSECURITY_DESCRIPTOR): BOOL; stdcall;
{$EXTERNALSYM SetFileSecurityA}

function GetFileSecurityA(lpFileName: LPCSTR; RequestedInformation: SECURITY_INFORMATION;
  pSecurityDescriptor: PSECURITY_DESCRIPTOR; nLength: DWORD;
  var lpnLengthNeeded: DWORD): BOOL; stdcall;
{$EXTERNALSYM GetFileSecurityA}
function GetFileSecurityW(lpFileName: LPCWSTR; RequestedInformation: SECURITY_INFORMATION;
  pSecurityDescriptor: PSECURITY_DESCRIPTOR; nLength: DWORD;
  var lpnLengthNeeded: DWORD): BOOL; stdcall;
{$EXTERNALSYM GetFileSecurityW}
function GetFileSecurity(lpFileName: LPCTSTR; RequestedInformation: SECURITY_INFORMATION;
  pSecurityDescriptor: PSECURITY_DESCRIPTOR; nLength: DWORD;
  var lpnLengthNeeded: DWORD): BOOL; stdcall;
{$EXTERNALSYM GetFileSecurityA}

// WinBase.h line 10251

function SetVolumeMountPoint(lpszVolumeMountPoint, lpszVolumeName: LPCSTR): BOOL; stdcall;
{$EXTERNALSYM SetVolumeMountPoint}

function DeleteVolumeMountPoint(lpszVolumeMountPoint: LPCSTR): BOOL; stdcall;
{$EXTERNALSYM DeleteVolumeMountPoint}

function GetVolumeNameForVolumeMountPoint(lpszVolumeMountPoint: LPCSTR;
  lpszVolumeName: LPSTR; cchBufferLength: DWORD): BOOL; stdcall;
{$EXTERNALSYM GetVolumeNameForVolumeMountPoint}

{$ENDIF ~CLR}


type
  {$EXTERNALSYM ULONG_PTR}
  ULONG_PTR = LongWord;      // Need to have the same size like Pointer
  {$EXTERNALSYM DWORD_PTR}
  DWORD_PTR = ULONG_PTR;
  {$EXTERNALSYM PDWORD_PTR}
  PDWORD_PTR = ^PLongWord;

// From JwaAclApi

// line 185

{$IFNDEF CLR}
function SetNamedSecurityInfoW(pObjectName: LPWSTR; ObjectType: SE_OBJECT_TYPE;
  SecurityInfo: SECURITY_INFORMATION; psidOwner, psidGroup: PSID;
  pDacl, pSacl: PACL): DWORD; stdcall;
{$EXTERNALSYM SetNamedSecurityInfoW}
{$ENDIF ~CLR}

{$IFNDEF CLR}

const
  IMAGE_SEPARATION = (64*1024);
  {$EXTERNALSYM IMAGE_SEPARATION}

type
  PLOADED_IMAGE = ^LOADED_IMAGE;
  {$EXTERNALSYM PLOADED_IMAGE}
  _LOADED_IMAGE = record
    ModuleName: PChar;
    hFile: THandle;
    MappedAddress: PAnsiChar;  // PUCHAR;
    FileHeader: PImageNtHeaders;
    LastRvaSection: PImageSectionHeader;
    NumberOfSections: ULONG;
    Sections: PImageSectionHeader;
    Characteristics: ULONG;
    fSystemImage: ByteBool;
    fDOSImage: ByteBool;
    Links: LIST_ENTRY;
    SizeOfImage: ULONG;
  end;
  {$EXTERNALSYM _LOADED_IMAGE}
  LOADED_IMAGE = _LOADED_IMAGE;
  {$EXTERNALSYM LOADED_IMAGE}
  TLoadedImage = LOADED_IMAGE;
  PLoadedImage = PLOADED_IMAGE;

// line 152

function ReBaseImage(CurrentImageName: PAnsiChar; SymbolPath: PAnsiChar; fReBase: BOOL;
  fRebaseSysfileOk: BOOL; fGoingDown: BOOL; CheckImageSize: ULONG;
  var OldImageSize: ULONG; var OldImageBase: ULONG_PTR; var NewImageSize: ULONG;
  var NewImageBase: ULONG_PTR; TimeStamp: ULONG): BOOL; stdcall;
{$EXTERNALSYM ReBaseImage}

function ReBaseImage64(CurrentImageName: PAnsiChar; SymbolPath: PAnsiChar; fReBase: BOOL;
  fRebaseSysfileOk: BOOL; fGoingDown: BOOL; CheckImageSize: ULONG;
  var OldImageSize: ULONG; var OldImageBase: TJclAddr64; var NewImageSize: ULONG;
  var NewImageBase: TJclAddr64; TimeStamp: ULONG): BOOL; stdcall;
{$EXTERNALSYM ReBaseImage64}

// line 199

//
// Define checksum function prototypes.
//

function CheckSumMappedFile(BaseAddress: Pointer; FileLength: DWORD;
  out HeaderSum, CheckSum: DWORD): PImageNtHeaders; stdcall;
{$EXTERNALSYM CheckSumMappedFile}

// line 227

function GetImageUnusedHeaderBytes(const LoadedImage: LOADED_IMAGE;
  var SizeUnusedHeaderBytes: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetImageUnusedHeaderBytes}

// line 285

function MapAndLoad(ImageName, DllPath: PChar; var LoadedImage: LOADED_IMAGE;
  DotDll: BOOL; ReadOnly: BOOL): BOOL; stdcall;
{$EXTERNALSYM MapAndLoad}

function UnMapAndLoad(const LoadedImage: LOADED_IMAGE): BOOL; stdcall;
{$EXTERNALSYM UnMapAndLoad}

function TouchFileTimes(const FileHandle: THandle; const pSystemTime: TSystemTime): BOOL; stdcall;
{$EXTERNALSYM TouchFileTimes}

// line 347

function ImageDirectoryEntryToData(Base: Pointer; MappedAsImage: ByteBool;
  DirectoryEntry: USHORT; var Size: ULONG): Pointer; stdcall;
{$EXTERNALSYM ImageDirectoryEntryToData}

function ImageRvaToSection(NtHeaders: PImageNtHeaders; Base: Pointer; Rva: ULONG): PImageSectionHeader; stdcall;
{$EXTERNALSYM ImageRvaToSection}

function ImageRvaToVa(NtHeaders: PImageNtHeaders; Base: Pointer; Rva: ULONG;
  LastRvaSection: PPImageSectionHeader): Pointer; stdcall;
{$EXTERNALSYM ImageRvaToVa}

{$ENDIF ~CLR}

// line 461

//
// UnDecorateSymbolName Flags
//

const
  UNDNAME_COMPLETE               = ($0000); // Enable full undecoration
  {$EXTERNALSYM UNDNAME_COMPLETE}
  UNDNAME_NO_LEADING_UNDERSCORES = ($0001); // Remove leading underscores from MS extended keywords
  {$EXTERNALSYM UNDNAME_NO_LEADING_UNDERSCORES}
  UNDNAME_NO_MS_KEYWORDS         = ($0002); // Disable expansion of MS extended keywords
  {$EXTERNALSYM UNDNAME_NO_MS_KEYWORDS}
  UNDNAME_NO_FUNCTION_RETURNS    = ($0004); // Disable expansion of return type for primary declaration
  {$EXTERNALSYM UNDNAME_NO_FUNCTION_RETURNS}
  UNDNAME_NO_ALLOCATION_MODEL    = ($0008); // Disable expansion of the declaration model
  {$EXTERNALSYM UNDNAME_NO_ALLOCATION_MODEL}
  UNDNAME_NO_ALLOCATION_LANGUAGE = ($0010); // Disable expansion of the declaration language specifier
  {$EXTERNALSYM UNDNAME_NO_ALLOCATION_LANGUAGE}
  UNDNAME_NO_MS_THISTYPE         = ($0020); // NYI Disable expansion of MS keywords on the 'this' type for primary declaration
  {$EXTERNALSYM UNDNAME_NO_MS_THISTYPE}
  UNDNAME_NO_CV_THISTYPE         = ($0040); // NYI Disable expansion of CV modifiers on the 'this' type for primary declaration
  {$EXTERNALSYM UNDNAME_NO_CV_THISTYPE}
  UNDNAME_NO_THISTYPE            = ($0060); // Disable all modifiers on the 'this' type
  {$EXTERNALSYM UNDNAME_NO_THISTYPE}
  UNDNAME_NO_ACCESS_SPECIFIERS   = ($0080); // Disable expansion of access specifiers for members
  {$EXTERNALSYM UNDNAME_NO_ACCESS_SPECIFIERS}
  UNDNAME_NO_THROW_SIGNATURES    = ($0100); // Disable expansion of 'throw-signatures' for functions and pointers to functions
  {$EXTERNALSYM UNDNAME_NO_THROW_SIGNATURES}
  UNDNAME_NO_MEMBER_TYPE         = ($0200); // Disable expansion of 'static' or 'virtual'ness of members
  {$EXTERNALSYM UNDNAME_NO_MEMBER_TYPE}
  UNDNAME_NO_RETURN_UDT_MODEL    = ($0400); // Disable expansion of MS model for UDT returns
  {$EXTERNALSYM UNDNAME_NO_RETURN_UDT_MODEL}
  UNDNAME_32_BIT_DECODE          = ($0800); // Undecorate 32-bit decorated names
  {$EXTERNALSYM UNDNAME_32_BIT_DECODE}
  UNDNAME_NAME_ONLY              = ($1000); // Crack only the name for primary declaration;
  {$EXTERNALSYM UNDNAME_NAME_ONLY}
                                                                                                   //  return just [scope::]name.  Does expand template params
  UNDNAME_NO_ARGUMENTS    = ($2000); // Don't undecorate arguments to function
  {$EXTERNALSYM UNDNAME_NO_ARGUMENTS}
  UNDNAME_NO_SPECIAL_SYMS = ($4000); // Don't undecorate special names (v-table, vcall, vector xxx, metatype, etc)
  {$EXTERNALSYM UNDNAME_NO_SPECIAL_SYMS}

function UnDecorateSymbolName(DecoratedName: {$IFDEF CLR}string{$ELSE}PAnsiChar{$ENDIF};
  UnDecoratedName: {$IFDEF CLR}string{$ELSE}PAnsiChar{$ENDIF}; UndecoratedLength: DWORD; Flags: DWORD): DWORD; stdcall;
  {$IFDEF CLR}external 'imagehlp.dll' name 'UnDecorateSymbolName';{$ENDIF}
{$EXTERNALSYM UnDecorateSymbolName}

// line 1342

type
  _IMAGEHLP_LINE = packed record
    SizeOfStruct: DWORD;           // set to sizeof(IMAGEHLP_LINE)
    Key: Pointer;                  // internal
    LineNumber: DWORD;             // line number in file
    FileName: PChar;               // full filename
    Address: DWORD;                // first instruction of line
  end;

  IMAGEHLP_LINE = _IMAGEHLP_LINE;
  PIMAGEHLP_LINE = ^_IMAGEHLP_LINE;

  TImageHlpLine = _IMAGEHLP_LINE;
  PImageHlpLine = PIMAGEHLP_LINE;

// line 1475

//
// options that are set/returned by SymSetOptions() & SymGetOptions()
// these are used as a mask
//

const
// defined in ImageHlp.pas
//  SYMOPT_CASE_INSENSITIVE       = $00000001;
//  {$EXTERNALSYM SYMOPT_CASE_INSENSITIVE}
//  SYMOPT_UNDNAME                = $00000002;
//  {$EXTERNALSYM SYMOPT_UNDNAME}
//  SYMOPT_DEFERRED_LOADS         = $00000004;
//  {$EXTERNALSYM SYMOPT_DEFERRED_LOADS}
//  SYMOPT_NO_CPP                 = $00000008;
//  {$EXTERNALSYM SYMOPT_NO_CPP}
  SYMOPT_LOAD_LINES             = $00000010;
  {$EXTERNALSYM SYMOPT_LOAD_LINES}
  SYMOPT_OMAP_FIND_NEAREST      = $00000020;
  {$EXTERNALSYM SYMOPT_OMAP_FIND_NEAREST}
  SYMOPT_LOAD_ANYTHING          = $00000040;
  {$EXTERNALSYM SYMOPT_LOAD_ANYTHING}
  SYMOPT_IGNORE_CVREC           = $00000080;
  {$EXTERNALSYM SYMOPT_IGNORE_CVREC}
  SYMOPT_NO_UNQUALIFIED_LOADS   = $00000100;
  {$EXTERNALSYM SYMOPT_NO_UNQUALIFIED_LOADS}
  SYMOPT_FAIL_CRITICAL_ERRORS   = $00000200;
  {$EXTERNALSYM SYMOPT_FAIL_CRITICAL_ERRORS}
  SYMOPT_EXACT_SYMBOLS          = $00000400;
  {$EXTERNALSYM SYMOPT_EXACT_SYMBOLS}
  SYMOPT_ALLOW_ABSOLUTE_SYMBOLS = $00000800;
  {$EXTERNALSYM SYMOPT_ALLOW_ABSOLUTE_SYMBOLS}
  SYMOPT_IGNORE_NT_SYMPATH      = $00001000;
  {$EXTERNALSYM SYMOPT_IGNORE_NT_SYMPATH}
  SYMOPT_INCLUDE_32BIT_MODULES  = $00002000;
  {$EXTERNALSYM SYMOPT_INCLUDE_32BIT_MODULES}
  SYMOPT_PUBLICS_ONLY           = $00004000;
  {$EXTERNALSYM SYMOPT_PUBLICS_ONLY}
  SYMOPT_NO_PUBLICS             = $00008000;
  {$EXTERNALSYM SYMOPT_NO_PUBLICS}
  SYMOPT_AUTO_PUBLICS           = $00010000;
  {$EXTERNALSYM SYMOPT_AUTO_PUBLICS}
  SYMOPT_NO_IMAGE_SEARCH        = $00020000;
  {$EXTERNALSYM SYMOPT_NO_IMAGE_SEARCH}
  SYMOPT_SECURE                 = $00040000;
  {$EXTERNALSYM SYMOPT_SECURE}
  SYMOPT_NO_PROMPTS             = $00080000;
  {$EXTERNALSYM SYMOPT_NO_PROMPTS}

  SYMOPT_DEBUG                  = $80000000;
  {$EXTERNALSYM SYMOPT_DEBUG}


const
  NERR_Success = 0; // Success
  {$EXTERNALSYM NERR_Success}

// ERROR_ equates can be intermixed with NERR_ equates.

//    NERR_BASE is the base of error codes from network utilities,
//      chosen to avoid conflict with system and redirector error codes.
//      2100 is a value that has been assigned to us by system.

  NERR_BASE = 2100;
  {$EXTERNALSYM NERR_BASE}


//*INTERNAL_ONLY*

{**********WARNING *****************
 *See the comment in lmcons.h for  *
 *info on the allocation of errors *
 ***********************************}

{**********WARNING *****************
 *The range 2750-2799 has been     *
 *allocated to the IBM LAN Server  *
 ***********************************}

{**********WARNING *****************
 *The range 2900-2999 has been     *
 *reserved for Microsoft OEMs      *
 ***********************************}

// UNUSED BASE+0
// UNUSED BASE+1
  NERR_NetNotStarted = (NERR_BASE+2); // The workstation driver is not installed.
  {$EXTERNALSYM NERR_NetNotStarted}
  NERR_UnknownServer = (NERR_BASE+3); // The server could not be located.
  {$EXTERNALSYM NERR_UnknownServer}
  NERR_ShareMem      = (NERR_BASE+4); // An internal error occurred.  The network cannot access a shared memory segment.
  {$EXTERNALSYM NERR_ShareMem}

  NERR_NoNetworkResource = (NERR_BASE+5); // A network resource shortage occurred .
  {$EXTERNALSYM NERR_NoNetworkResource}
  NERR_RemoteOnly        = (NERR_BASE+6); // This operation is not supported on workstations.
  {$EXTERNALSYM NERR_RemoteOnly}
  NERR_DevNotRedirected  = (NERR_BASE+7); // The device is not connected.
  {$EXTERNALSYM NERR_DevNotRedirected}
// NERR_BASE+8 is used for ERROR_CONNECTED_OTHER_PASSWORD
// NERR_BASE+9 is used for ERROR_CONNECTED_OTHER_PASSWORD_DEFAULT
// UNUSED BASE+10
// UNUSED BASE+11
// UNUSED BASE+12
// UNUSED BASE+13
  NERR_ServerNotStarted = (NERR_BASE+14); // The Server service is not started.
  {$EXTERNALSYM NERR_ServerNotStarted}
  NERR_ItemNotFound     = (NERR_BASE+15); // The queue is empty.
  {$EXTERNALSYM NERR_ItemNotFound}
  NERR_UnknownDevDir    = (NERR_BASE+16); // The device or directory does not exist.
  {$EXTERNALSYM NERR_UnknownDevDir}
  NERR_RedirectedPath   = (NERR_BASE+17); // The operation is invalid on a redirected resource.
  {$EXTERNALSYM NERR_RedirectedPath}
  NERR_DuplicateShare   = (NERR_BASE+18); // The name has already been shared.
  {$EXTERNALSYM NERR_DuplicateShare}
  NERR_NoRoom           = (NERR_BASE+19); // The server is currently out of the requested resource.
  {$EXTERNALSYM NERR_NoRoom}
// UNUSED BASE+20
  NERR_TooManyItems    = (NERR_BASE+21); // Requested addition of items exceeds the maximum allowed.
  {$EXTERNALSYM NERR_TooManyItems}
  NERR_InvalidMaxUsers = (NERR_BASE+22); // The Peer service supports only two simultaneous users.
  {$EXTERNALSYM NERR_InvalidMaxUsers}
  NERR_BufTooSmall     = (NERR_BASE+23); // The API return buffer is too small.
  {$EXTERNALSYM NERR_BufTooSmall}
// UNUSED BASE+24
// UNUSED BASE+25
// UNUSED BASE+26
  NERR_RemoteErr = (NERR_BASE+27); // A remote API error occurred.
  {$EXTERNALSYM NERR_RemoteErr}
// UNUSED BASE+28
// UNUSED BASE+29
// UNUSED BASE+30
  NERR_LanmanIniError = (NERR_BASE+31); // An error occurred when opening or reading the configuration file.
  {$EXTERNALSYM NERR_LanmanIniError}
// UNUSED BASE+32
// UNUSED BASE+33
// UNUSED BASE+34
// UNUSED BASE+35
  NERR_NetworkError           = (NERR_BASE+36); // A general network error occurred.
  {$EXTERNALSYM NERR_NetworkError}
  NERR_WkstaInconsistentState = (NERR_BASE+37);
  {$EXTERNALSYM NERR_WkstaInconsistentState}
    // The Workstation service is in an inconsistent state. Restart the computer before restarting the Workstation service.
  NERR_WkstaNotStarted   = (NERR_BASE+38); // The Workstation service has not been started.
  {$EXTERNALSYM NERR_WkstaNotStarted}
  NERR_BrowserNotStarted = (NERR_BASE+39); // The requested information is not available.
  {$EXTERNALSYM NERR_BrowserNotStarted}
  NERR_InternalError     = (NERR_BASE+40); // An internal Windows 2000 error occurred.
  {$EXTERNALSYM NERR_InternalError}
  NERR_BadTransactConfig = (NERR_BASE+41); // The server is not configured for transactions.
  {$EXTERNALSYM NERR_BadTransactConfig}
  NERR_InvalidAPI        = (NERR_BASE+42); // The requested API is not supported on the remote server.
  {$EXTERNALSYM NERR_InvalidAPI}
  NERR_BadEventName      = (NERR_BASE+43); // The event name is invalid.
  {$EXTERNALSYM NERR_BadEventName}
  NERR_DupNameReboot     = (NERR_BASE+44); // The computer name already exists on the network. Change it and restart the computer.
  {$EXTERNALSYM NERR_DupNameReboot}

//
//      Config API related
//              Error codes from BASE+45 to BASE+49


// UNUSED BASE+45
  NERR_CfgCompNotFound  = (NERR_BASE+46); // The specified component could not be found in the configuration information.
  {$EXTERNALSYM NERR_CfgCompNotFound}
  NERR_CfgParamNotFound = (NERR_BASE+47); // The specified parameter could not be found in the configuration information.
  {$EXTERNALSYM NERR_CfgParamNotFound}
  NERR_LineTooLong = (NERR_BASE+49); // A line in the configuration file is too long.
  {$EXTERNALSYM NERR_LineTooLong}

//
//      Spooler API related
//              Error codes from BASE+50 to BASE+79


  NERR_QNotFound        = (NERR_BASE+50); // The printer does not exist.
  {$EXTERNALSYM NERR_QNotFound}
  NERR_JobNotFound      = (NERR_BASE+51); // The print job does not exist.
  {$EXTERNALSYM NERR_JobNotFound}
  NERR_DestNotFound     = (NERR_BASE+52); // The printer destination cannot be found.
  {$EXTERNALSYM NERR_DestNotFound}
  NERR_DestExists       = (NERR_BASE+53); // The printer destination already exists.
  {$EXTERNALSYM NERR_DestExists}
  NERR_QExists          = (NERR_BASE+54); // The printer queue already exists.
  {$EXTERNALSYM NERR_QExists}
  NERR_QNoRoom          = (NERR_BASE+55); // No more printers can be added.
  {$EXTERNALSYM NERR_QNoRoom}
  NERR_JobNoRoom        = (NERR_BASE+56); // No more print jobs can be added.
  {$EXTERNALSYM NERR_JobNoRoom}
  NERR_DestNoRoom       = (NERR_BASE+57); // No more printer destinations can be added.
  {$EXTERNALSYM NERR_DestNoRoom}
  NERR_DestIdle         = (NERR_BASE+58); // This printer destination is idle and cannot accept control operations.
  {$EXTERNALSYM NERR_DestIdle}
  NERR_DestInvalidOp    = (NERR_BASE+59); // This printer destination request contains an invalid control function.
  {$EXTERNALSYM NERR_DestInvalidOp}
  NERR_ProcNoRespond    = (NERR_BASE+60); // The print processor is not responding.
  {$EXTERNALSYM NERR_ProcNoRespond}
  NERR_SpoolerNotLoaded = (NERR_BASE+61); // The spooler is not running.
  {$EXTERNALSYM NERR_SpoolerNotLoaded}
  NERR_DestInvalidState = (NERR_BASE+62); // This operation cannot be performed on the print destination in its current state.
  {$EXTERNALSYM NERR_DestInvalidState}
  NERR_QInvalidState    = (NERR_BASE+63); // This operation cannot be performed on the printer queue in its current state.
  {$EXTERNALSYM NERR_QInvalidState}
  NERR_JobInvalidState  = (NERR_BASE+64); // This operation cannot be performed on the print job in its current state.
  {$EXTERNALSYM NERR_JobInvalidState}
  NERR_SpoolNoMemory    = (NERR_BASE+65); // A spooler memory allocation failure occurred.
  {$EXTERNALSYM NERR_SpoolNoMemory}
  NERR_DriverNotFound   = (NERR_BASE+66); // The device driver does not exist.
  {$EXTERNALSYM NERR_DriverNotFound}
  NERR_DataTypeInvalid  = (NERR_BASE+67); // The data type is not supported by the print processor.
  {$EXTERNALSYM NERR_DataTypeInvalid}
  NERR_ProcNotFound     = (NERR_BASE+68); // The print processor is not installed.
  {$EXTERNALSYM NERR_ProcNotFound}

//
//      Service API related
//              Error codes from BASE+80 to BASE+99


  NERR_ServiceTableLocked  = (NERR_BASE+80); // The service database is locked.
  {$EXTERNALSYM NERR_ServiceTableLocked}
  NERR_ServiceTableFull    = (NERR_BASE+81); // The service table is full.
  {$EXTERNALSYM NERR_ServiceTableFull}
  NERR_ServiceInstalled    = (NERR_BASE+82); // The requested service has already been started.
  {$EXTERNALSYM NERR_ServiceInstalled}
  NERR_ServiceEntryLocked  = (NERR_BASE+83); // The service does not respond to control actions.
  {$EXTERNALSYM NERR_ServiceEntryLocked}
  NERR_ServiceNotInstalled = (NERR_BASE+84); // The service has not been started.
  {$EXTERNALSYM NERR_ServiceNotInstalled}
  NERR_BadServiceName      = (NERR_BASE+85); // The service name is invalid.
  {$EXTERNALSYM NERR_BadServiceName}
  NERR_ServiceCtlTimeout   = (NERR_BASE+86); // The service is not responding to the control function.
  {$EXTERNALSYM NERR_ServiceCtlTimeout}
  NERR_ServiceCtlBusy      = (NERR_BASE+87); // The service control is busy.
  {$EXTERNALSYM NERR_ServiceCtlBusy}
  NERR_BadServiceProgName  = (NERR_BASE+88); // The configuration file contains an invalid service program name.
  {$EXTERNALSYM NERR_BadServiceProgName}
  NERR_ServiceNotCtrl      = (NERR_BASE+89); // The service could not be controlled in its present state.
  {$EXTERNALSYM NERR_ServiceNotCtrl}
  NERR_ServiceKillProc     = (NERR_BASE+90); // The service ended abnormally.
  {$EXTERNALSYM NERR_ServiceKillProc}
  NERR_ServiceCtlNotValid  = (NERR_BASE+91); // The requested pause,continue, or stop is not valid for this service.
  {$EXTERNALSYM NERR_ServiceCtlNotValid}
  NERR_NotInDispatchTbl    = (NERR_BASE+92); // The service control dispatcher could not find the service name in the dispatch table.
  {$EXTERNALSYM NERR_NotInDispatchTbl}
  NERR_BadControlRecv      = (NERR_BASE+93); // The service control dispatcher pipe read failed.
  {$EXTERNALSYM NERR_BadControlRecv}
  NERR_ServiceNotStarting  = (NERR_BASE+94); // A thread for the new service could not be created.
  {$EXTERNALSYM NERR_ServiceNotStarting}

//
//      Wksta and Logon API related
//              Error codes from BASE+100 to BASE+118


  NERR_AlreadyLoggedOn   = (NERR_BASE+100); // This workstation is already logged on to the local-area network.
  {$EXTERNALSYM NERR_AlreadyLoggedOn}
  NERR_NotLoggedOn       = (NERR_BASE+101); // The workstation is not logged on to the local-area network.
  {$EXTERNALSYM NERR_NotLoggedOn}
  NERR_BadUsername       = (NERR_BASE+102); // The user name or group name parameter is invalid.
  {$EXTERNALSYM NERR_BadUsername}
  NERR_BadPassword       = (NERR_BASE+103); // The password parameter is invalid.
  {$EXTERNALSYM NERR_BadPassword}
  NERR_UnableToAddName_W = (NERR_BASE+104); // @W The logon processor did not add the message alias.
  {$EXTERNALSYM NERR_UnableToAddName_W}
  NERR_UnableToAddName_F = (NERR_BASE+105); // The logon processor did not add the message alias.
  {$EXTERNALSYM NERR_UnableToAddName_F}
  NERR_UnableToDelName_W = (NERR_BASE+106); // @W The logoff processor did not delete the message alias.
  {$EXTERNALSYM NERR_UnableToDelName_W}
  NERR_UnableToDelName_F = (NERR_BASE+107); // The logoff processor did not delete the message alias.
  {$EXTERNALSYM NERR_UnableToDelName_F}
// UNUSED BASE+108
  NERR_LogonsPaused        = (NERR_BASE+109); // Network logons are paused.
  {$EXTERNALSYM NERR_LogonsPaused}
  NERR_LogonServerConflict = (NERR_BASE+110); // A centralized logon-server conflict occurred.
  {$EXTERNALSYM NERR_LogonServerConflict}
  NERR_LogonNoUserPath     = (NERR_BASE+111); // The server is configured without a valid user path.
  {$EXTERNALSYM NERR_LogonNoUserPath}
  NERR_LogonScriptError    = (NERR_BASE+112); // An error occurred while loading or running the logon script.
  {$EXTERNALSYM NERR_LogonScriptError}
// UNUSED BASE+113
  NERR_StandaloneLogon     = (NERR_BASE+114); // The logon server was not specified.  Your computer will be logged on as STANDALONE.
  {$EXTERNALSYM NERR_StandaloneLogon}
  NERR_LogonServerNotFound = (NERR_BASE+115); // The logon server could not be found.
  {$EXTERNALSYM NERR_LogonServerNotFound}
  NERR_LogonDomainExists   = (NERR_BASE+116); // There is already a logon domain for this computer.
  {$EXTERNALSYM NERR_LogonDomainExists}
  NERR_NonValidatedLogon   = (NERR_BASE+117); // The logon server could not validate the logon.
  {$EXTERNALSYM NERR_NonValidatedLogon}

//
//      ACF API related (access, user, group)
//              Error codes from BASE+119 to BASE+149


  NERR_ACFNotFound          = (NERR_BASE+119); // The security database could not be found.
  {$EXTERNALSYM NERR_ACFNotFound}
  NERR_GroupNotFound        = (NERR_BASE+120); // The group name could not be found.
  {$EXTERNALSYM NERR_GroupNotFound}
  NERR_UserNotFound         = (NERR_BASE+121); // The user name could not be found.
  {$EXTERNALSYM NERR_UserNotFound}
  NERR_ResourceNotFound     = (NERR_BASE+122); // The resource name could not be found.
  {$EXTERNALSYM NERR_ResourceNotFound}
  NERR_GroupExists          = (NERR_BASE+123); // The group already exists.
  {$EXTERNALSYM NERR_GroupExists}
  NERR_UserExists           = (NERR_BASE+124); // The account already exists.
  {$EXTERNALSYM NERR_UserExists}
  NERR_ResourceExists       = (NERR_BASE+125); // The resource permission list already exists.
  {$EXTERNALSYM NERR_ResourceExists}
  NERR_NotPrimary           = (NERR_BASE+126); // This operation is only allowed on the primary domain controller of the domain.
  {$EXTERNALSYM NERR_NotPrimary}
  NERR_ACFNotLoaded         = (NERR_BASE+127); // The security database has not been started.
  {$EXTERNALSYM NERR_ACFNotLoaded}
  NERR_ACFNoRoom            = (NERR_BASE+128); // There are too many names in the user accounts database.
  {$EXTERNALSYM NERR_ACFNoRoom}
  NERR_ACFFileIOFail        = (NERR_BASE+129); // A disk I/O failure occurred.
  {$EXTERNALSYM NERR_ACFFileIOFail}
  NERR_ACFTooManyLists      = (NERR_BASE+130); // The limit of 64 entries per resource was exceeded.
  {$EXTERNALSYM NERR_ACFTooManyLists}
  NERR_UserLogon            = (NERR_BASE+131); // Deleting a user with a session is not allowed.
  {$EXTERNALSYM NERR_UserLogon}
  NERR_ACFNoParent          = (NERR_BASE+132); // The parent directory could not be located.
  {$EXTERNALSYM NERR_ACFNoParent}
  NERR_CanNotGrowSegment    = (NERR_BASE+133); // Unable to add to the security database session cache segment.
  {$EXTERNALSYM NERR_CanNotGrowSegment}
  NERR_SpeGroupOp           = (NERR_BASE+134); // This operation is not allowed on this special group.
  {$EXTERNALSYM NERR_SpeGroupOp}
  NERR_NotInCache           = (NERR_BASE+135); // This user is not cached in user accounts database session cache.
  {$EXTERNALSYM NERR_NotInCache}
  NERR_UserInGroup          = (NERR_BASE+136); // The user already belongs to this group.
  {$EXTERNALSYM NERR_UserInGroup}
  NERR_UserNotInGroup       = (NERR_BASE+137); // The user does not belong to this group.
  {$EXTERNALSYM NERR_UserNotInGroup}
  NERR_AccountUndefined     = (NERR_BASE+138); // This user account is undefined.
  {$EXTERNALSYM NERR_AccountUndefined}
  NERR_AccountExpired       = (NERR_BASE+139); // This user account has expired.
  {$EXTERNALSYM NERR_AccountExpired}
  NERR_InvalidWorkstation   = (NERR_BASE+140); // The user is not allowed to log on from this workstation.
  {$EXTERNALSYM NERR_InvalidWorkstation}
  NERR_InvalidLogonHours    = (NERR_BASE+141); // The user is not allowed to log on at this time.
  {$EXTERNALSYM NERR_InvalidLogonHours}
  NERR_PasswordExpired      = (NERR_BASE+142); // The password of this user has expired.
  {$EXTERNALSYM NERR_PasswordExpired}
  NERR_PasswordCantChange   = (NERR_BASE+143); // The password of this user cannot change.
  {$EXTERNALSYM NERR_PasswordCantChange}
  NERR_PasswordHistConflict = (NERR_BASE+144); // This password cannot be used now.
  {$EXTERNALSYM NERR_PasswordHistConflict}
  NERR_PasswordTooShort     = (NERR_BASE+145); // The password does not meet the password policy requirements. Check the minimum password length, password complexity and password history requirements.
  {$EXTERNALSYM NERR_PasswordTooShort}
  NERR_PasswordTooRecent    = (NERR_BASE+146); // The password of this user is too recent to change.
  {$EXTERNALSYM NERR_PasswordTooRecent}
  NERR_InvalidDatabase      = (NERR_BASE+147); // The security database is corrupted.
  {$EXTERNALSYM NERR_InvalidDatabase}
  NERR_DatabaseUpToDate     = (NERR_BASE+148); // No updates are necessary to this replicant network/local security database.
  {$EXTERNALSYM NERR_DatabaseUpToDate}
  NERR_SyncRequired         = (NERR_BASE+149); // This replicant database is outdated; synchronization is required.
  {$EXTERNALSYM NERR_SyncRequired}

//
//      Use API related
//              Error codes from BASE+150 to BASE+169


  NERR_UseNotFound    = (NERR_BASE+150); // The network connection could not be found.
  {$EXTERNALSYM NERR_UseNotFound}
  NERR_BadAsgType     = (NERR_BASE+151); // This asg_type is invalid.
  {$EXTERNALSYM NERR_BadAsgType}
  NERR_DeviceIsShared = (NERR_BASE+152); // This device is currently being shared.
  {$EXTERNALSYM NERR_DeviceIsShared}

//
//      Message Server related
//              Error codes BASE+170 to BASE+209


  NERR_NoComputerName     = (NERR_BASE+170); // The computer name could not be added as a message alias.  The name may already exist on the network.
  {$EXTERNALSYM NERR_NoComputerName}
  NERR_MsgAlreadyStarted  = (NERR_BASE+171); // The Messenger service is already started.
  {$EXTERNALSYM NERR_MsgAlreadyStarted}
  NERR_MsgInitFailed      = (NERR_BASE+172); // The Messenger service failed to start.
  {$EXTERNALSYM NERR_MsgInitFailed}
  NERR_NameNotFound       = (NERR_BASE+173); // The message alias could not be found on the network.
  {$EXTERNALSYM NERR_NameNotFound}
  NERR_AlreadyForwarded   = (NERR_BASE+174); // This message alias has already been forwarded.
  {$EXTERNALSYM NERR_AlreadyForwarded}
  NERR_AddForwarded       = (NERR_BASE+175); // This message alias has been added but is still forwarded.
  {$EXTERNALSYM NERR_AddForwarded}
  NERR_AlreadyExists      = (NERR_BASE+176); // This message alias already exists locally.
  {$EXTERNALSYM NERR_AlreadyExists}
  NERR_TooManyNames       = (NERR_BASE+177); // The maximum number of added message aliases has been exceeded.
  {$EXTERNALSYM NERR_TooManyNames}
  NERR_DelComputerName    = (NERR_BASE+178); // The computer name could not be deleted.
  {$EXTERNALSYM NERR_DelComputerName}
  NERR_LocalForward       = (NERR_BASE+179); // Messages cannot be forwarded back to the same workstation.
  {$EXTERNALSYM NERR_LocalForward}
  NERR_GrpMsgProcessor    = (NERR_BASE+180); // An error occurred in the domain message processor.
  {$EXTERNALSYM NERR_GrpMsgProcessor}
  NERR_PausedRemote       = (NERR_BASE+181); // The message was sent, but the recipient has paused the Messenger service.
  {$EXTERNALSYM NERR_PausedRemote}
  NERR_BadReceive         = (NERR_BASE+182); // The message was sent but not received.
  {$EXTERNALSYM NERR_BadReceive}
  NERR_NameInUse          = (NERR_BASE+183); // The message alias is currently in use. Try again later.
  {$EXTERNALSYM NERR_NameInUse}
  NERR_MsgNotStarted      = (NERR_BASE+184); // The Messenger service has not been started.
  {$EXTERNALSYM NERR_MsgNotStarted}
  NERR_NotLocalName       = (NERR_BASE+185); // The name is not on the local computer.
  {$EXTERNALSYM NERR_NotLocalName}
  NERR_NoForwardName      = (NERR_BASE+186); // The forwarded message alias could not be found on the network.
  {$EXTERNALSYM NERR_NoForwardName}
  NERR_RemoteFull         = (NERR_BASE+187); // The message alias table on the remote station is full.
  {$EXTERNALSYM NERR_RemoteFull}
  NERR_NameNotForwarded   = (NERR_BASE+188); // Messages for this alias are not currently being forwarded.
  {$EXTERNALSYM NERR_NameNotForwarded}
  NERR_TruncatedBroadcast = (NERR_BASE+189); // The broadcast message was truncated.
  {$EXTERNALSYM NERR_TruncatedBroadcast}
  NERR_InvalidDevice      = (NERR_BASE+194); // This is an invalid device name.
  {$EXTERNALSYM NERR_InvalidDevice}
  NERR_WriteFault         = (NERR_BASE+195); // A write fault occurred.
  {$EXTERNALSYM NERR_WriteFault}
// UNUSED BASE+196
  NERR_DuplicateName = (NERR_BASE+197); // A duplicate message alias exists on the network.
  {$EXTERNALSYM NERR_DuplicateName}
  NERR_DeleteLater   = (NERR_BASE+198); // @W This message alias will be deleted later.
  {$EXTERNALSYM NERR_DeleteLater}
  NERR_IncompleteDel = (NERR_BASE+199); // The message alias was not successfully deleted from all networks.
  {$EXTERNALSYM NERR_IncompleteDel}
  NERR_MultipleNets  = (NERR_BASE+200); // This operation is not supported on computers with multiple networks.
  {$EXTERNALSYM NERR_MultipleNets}

//
//      Server API related
//             Error codes BASE+210 to BASE+229


  NERR_NetNameNotFound        = (NERR_BASE+210); // This shared resource does not exist.
  {$EXTERNALSYM NERR_NetNameNotFound}
  NERR_DeviceNotShared        = (NERR_BASE+211); // This device is not shared.
  {$EXTERNALSYM NERR_DeviceNotShared}
  NERR_ClientNameNotFound     = (NERR_BASE+212); // A session does not exist with that computer name.
  {$EXTERNALSYM NERR_ClientNameNotFound}
  NERR_FileIdNotFound         = (NERR_BASE+214); // There is not an open file with that identification number.
  {$EXTERNALSYM NERR_FileIdNotFound}
  NERR_ExecFailure            = (NERR_BASE+215); // A failure occurred when executing a remote administration command.
  {$EXTERNALSYM NERR_ExecFailure}
  NERR_TmpFile                = (NERR_BASE+216); // A failure occurred when opening a remote temporary file.
  {$EXTERNALSYM NERR_TmpFile}
  NERR_TooMuchData            = (NERR_BASE+217); // The data returned from a remote administration command has been truncated to 64K.
  {$EXTERNALSYM NERR_TooMuchData}
  NERR_DeviceShareConflict    = (NERR_BASE+218); // This device cannot be shared as both a spooled and a non-spooled resource.
  {$EXTERNALSYM NERR_DeviceShareConflict}
  NERR_BrowserTableIncomplete = (NERR_BASE+219); // The information in the list of servers may be incorrect.
  {$EXTERNALSYM NERR_BrowserTableIncomplete}
  NERR_NotLocalDomain         = (NERR_BASE+220); // The computer is not active in this domain.
  {$EXTERNALSYM NERR_NotLocalDomain}
  NERR_IsDfsShare             = (NERR_BASE+221); // The share must be removed from the Distributed File System before it can be deleted.
  {$EXTERNALSYM NERR_IsDfsShare}

//
//      CharDev API related
//              Error codes BASE+230 to BASE+249


// UNUSED BASE+230
  NERR_DevInvalidOpCode  = (NERR_BASE+231); // The operation is invalid for this device.
  {$EXTERNALSYM NERR_DevInvalidOpCode}
  NERR_DevNotFound       = (NERR_BASE+232); // This device cannot be shared.
  {$EXTERNALSYM NERR_DevNotFound}
  NERR_DevNotOpen        = (NERR_BASE+233); // This device was not open.
  {$EXTERNALSYM NERR_DevNotOpen}
  NERR_BadQueueDevString = (NERR_BASE+234); // This device name list is invalid.
  {$EXTERNALSYM NERR_BadQueueDevString}
  NERR_BadQueuePriority  = (NERR_BASE+235); // The queue priority is invalid.
  {$EXTERNALSYM NERR_BadQueuePriority}
  NERR_NoCommDevs        = (NERR_BASE+237); // There are no shared communication devices.
  {$EXTERNALSYM NERR_NoCommDevs}
  NERR_QueueNotFound     = (NERR_BASE+238); // The queue you specified does not exist.
  {$EXTERNALSYM NERR_QueueNotFound}
  NERR_BadDevString      = (NERR_BASE+240); // This list of devices is invalid.
  {$EXTERNALSYM NERR_BadDevString}
  NERR_BadDev            = (NERR_BASE+241); // The requested device is invalid.
  {$EXTERNALSYM NERR_BadDev}
  NERR_InUseBySpooler    = (NERR_BASE+242); // This device is already in use by the spooler.
  {$EXTERNALSYM NERR_InUseBySpooler}
  NERR_CommDevInUse      = (NERR_BASE+243); // This device is already in use as a communication device.
  {$EXTERNALSYM NERR_CommDevInUse}

//
//      NetICanonicalize and NetIType and NetIMakeLMFileName
//      NetIListCanon and NetINameCheck
//              Error codes BASE+250 to BASE+269


  NERR_InvalidComputer = (NERR_BASE+251); // This computer name is invalid.
  {$EXTERNALSYM NERR_InvalidComputer}
// UNUSED BASE+252
// UNUSED BASE+253
  NERR_MaxLenExceeded = (NERR_BASE+254); // The string and prefix specified are too long.
  {$EXTERNALSYM NERR_MaxLenExceeded}
// UNUSED BASE+255
  NERR_BadComponent = (NERR_BASE+256); // This path component is invalid.
  {$EXTERNALSYM NERR_BadComponent}
  NERR_CantType     = (NERR_BASE+257); // Could not determine the type of input.
  {$EXTERNALSYM NERR_CantType}
// UNUSED BASE+258
// UNUSED BASE+259
  NERR_TooManyEntries = (NERR_BASE+262); // The buffer for types is not big enough.
  {$EXTERNALSYM NERR_TooManyEntries}

//
//      NetProfile
//              Error codes BASE+270 to BASE+276


  NERR_ProfileFileTooBig = (NERR_BASE+270); // Profile files cannot exceed 64K.
  {$EXTERNALSYM NERR_ProfileFileTooBig}
  NERR_ProfileOffset     = (NERR_BASE+271); // The start offset is out of range.
  {$EXTERNALSYM NERR_ProfileOffset}
  NERR_ProfileCleanup    = (NERR_BASE+272); // The system cannot delete current connections to network resources.
  {$EXTERNALSYM NERR_ProfileCleanup}
  NERR_ProfileUnknownCmd = (NERR_BASE+273); // The system was unable to parse the command line in this file.
  {$EXTERNALSYM NERR_ProfileUnknownCmd}
  NERR_ProfileLoadErr    = (NERR_BASE+274); // An error occurred while loading the profile file.
  {$EXTERNALSYM NERR_ProfileLoadErr}
  NERR_ProfileSaveErr    = (NERR_BASE+275); // @W Errors occurred while saving the profile file.  The profile was partially saved.
  {$EXTERNALSYM NERR_ProfileSaveErr}


//
//      NetAudit and NetErrorLog
//              Error codes BASE+277 to BASE+279


  NERR_LogOverflow    = (NERR_BASE+277); // Log file %1 is full.
  {$EXTERNALSYM NERR_LogOverflow}
  NERR_LogFileChanged = (NERR_BASE+278); // This log file has changed between reads.
  {$EXTERNALSYM NERR_LogFileChanged}
  NERR_LogFileCorrupt = (NERR_BASE+279); // Log file %1 is corrupt.
  {$EXTERNALSYM NERR_LogFileCorrupt}


//
//      NetRemote
//              Error codes BASE+280 to BASE+299

  NERR_SourceIsDir      = (NERR_BASE+280); // The source path cannot be a directory.
  {$EXTERNALSYM NERR_SourceIsDir}
  NERR_BadSource        = (NERR_BASE+281); // The source path is illegal.
  {$EXTERNALSYM NERR_BadSource}
  NERR_BadDest          = (NERR_BASE+282); // The destination path is illegal.
  {$EXTERNALSYM NERR_BadDest}
  NERR_DifferentServers = (NERR_BASE+283); // The source and destination paths are on different servers.
  {$EXTERNALSYM NERR_DifferentServers}
// UNUSED BASE+284
  NERR_RunSrvPaused = (NERR_BASE+285); // The Run server you requested is paused.
  {$EXTERNALSYM NERR_RunSrvPaused}
// UNUSED BASE+286
// UNUSED BASE+287
// UNUSED BASE+288
  NERR_ErrCommRunSrv = (NERR_BASE+289); // An error occurred when communicating with a Run server.
  {$EXTERNALSYM NERR_ErrCommRunSrv}
// UNUSED BASE+290
  NERR_ErrorExecingGhost = (NERR_BASE+291); // An error occurred when starting a background process.
  {$EXTERNALSYM NERR_ErrorExecingGhost}
  NERR_ShareNotFound     = (NERR_BASE+292); // The shared resource you are connected to could not be found.
  {$EXTERNALSYM NERR_ShareNotFound}
// UNUSED BASE+293
// UNUSED BASE+294


//
//  NetWksta.sys (redir) returned error codes.
//
//          NERR_BASE + (300-329)


  NERR_InvalidLana     = (NERR_BASE+300); // The LAN adapter number is invalid.
  {$EXTERNALSYM NERR_InvalidLana}
  NERR_OpenFiles       = (NERR_BASE+301); // There are open files on the connection.
  {$EXTERNALSYM NERR_OpenFiles}
  NERR_ActiveConns     = (NERR_BASE+302); // Active connections still exist.
  {$EXTERNALSYM NERR_ActiveConns}
  NERR_BadPasswordCore = (NERR_BASE+303); // This share name or password is invalid.
  {$EXTERNALSYM NERR_BadPasswordCore}
  NERR_DevInUse        = (NERR_BASE+304); // The device is being accessed by an active process.
  {$EXTERNALSYM NERR_DevInUse}
  NERR_LocalDrive      = (NERR_BASE+305); // The drive letter is in use locally.
  {$EXTERNALSYM NERR_LocalDrive}

//
//  Alert error codes.
//
//          NERR_BASE + (330-339)

  NERR_AlertExists       = (NERR_BASE+330); // The specified client is already registered for the specified event.
  {$EXTERNALSYM NERR_AlertExists}
  NERR_TooManyAlerts     = (NERR_BASE+331); // The alert table is full.
  {$EXTERNALSYM NERR_TooManyAlerts}
  NERR_NoSuchAlert       = (NERR_BASE+332); // An invalid or nonexistent alert name was raised.
  {$EXTERNALSYM NERR_NoSuchAlert}
  NERR_BadRecipient      = (NERR_BASE+333); // The alert recipient is invalid.
  {$EXTERNALSYM NERR_BadRecipient}
  NERR_AcctLimitExceeded = (NERR_BASE+334); // A user's session with this server has been deleted
  {$EXTERNALSYM NERR_AcctLimitExceeded}
                                                // because the user's logon hours are no longer valid.

//
//  Additional Error and Audit log codes.
//
//          NERR_BASE +(340-343)

  NERR_InvalidLogSeek = (NERR_BASE+340); // The log file does not contain the requested record number.
  {$EXTERNALSYM NERR_InvalidLogSeek}
// UNUSED BASE+341
// UNUSED BASE+342
// UNUSED BASE+343

//
//  Additional UAS and NETLOGON codes
//
//          NERR_BASE +(350-359)

  NERR_BadUasConfig       = (NERR_BASE+350); // The user accounts database is not configured correctly.
  {$EXTERNALSYM NERR_BadUasConfig}
  NERR_InvalidUASOp       = (NERR_BASE+351); // This operation is not permitted when the Netlogon service is running.
  {$EXTERNALSYM NERR_InvalidUASOp}
  NERR_LastAdmin          = (NERR_BASE+352); // This operation is not allowed on the last administrative account.
  {$EXTERNALSYM NERR_LastAdmin}
  NERR_DCNotFound         = (NERR_BASE+353); // Could not find domain controller for this domain.
  {$EXTERNALSYM NERR_DCNotFound}
  NERR_LogonTrackingError = (NERR_BASE+354); // Could not set logon information for this user.
  {$EXTERNALSYM NERR_LogonTrackingError}
  NERR_NetlogonNotStarted = (NERR_BASE+355); // The Netlogon service has not been started.
  {$EXTERNALSYM NERR_NetlogonNotStarted}
  NERR_CanNotGrowUASFile  = (NERR_BASE+356); // Unable to add to the user accounts database.
  {$EXTERNALSYM NERR_CanNotGrowUASFile}
  NERR_TimeDiffAtDC       = (NERR_BASE+357); // This server's clock is not synchronized with the primary domain controller's clock.
  {$EXTERNALSYM NERR_TimeDiffAtDC}
  NERR_PasswordMismatch   = (NERR_BASE+358); // A password mismatch has been detected.
  {$EXTERNALSYM NERR_PasswordMismatch}


//
//  Server Integration error codes.
//
//          NERR_BASE +(360-369)

  NERR_NoSuchServer       = (NERR_BASE+360); // The server identification does not specify a valid server.
  {$EXTERNALSYM NERR_NoSuchServer}
  NERR_NoSuchSession      = (NERR_BASE+361); // The session identification does not specify a valid session.
  {$EXTERNALSYM NERR_NoSuchSession}
  NERR_NoSuchConnection   = (NERR_BASE+362); // The connection identification does not specify a valid connection.
  {$EXTERNALSYM NERR_NoSuchConnection}
  NERR_TooManyServers     = (NERR_BASE+363); // There is no space for another entry in the table of available servers.
  {$EXTERNALSYM NERR_TooManyServers}
  NERR_TooManySessions    = (NERR_BASE+364); // The server has reached the maximum number of sessions it supports.
  {$EXTERNALSYM NERR_TooManySessions}
  NERR_TooManyConnections = (NERR_BASE+365); // The server has reached the maximum number of connections it supports.
  {$EXTERNALSYM NERR_TooManyConnections}
  NERR_TooManyFiles       = (NERR_BASE+366); // The server cannot open more files because it has reached its maximum number.
  {$EXTERNALSYM NERR_TooManyFiles}
  NERR_NoAlternateServers = (NERR_BASE+367); // There are no alternate servers registered on this server.
  {$EXTERNALSYM NERR_NoAlternateServers}
// UNUSED BASE+368
// UNUSED BASE+369

  NERR_TryDownLevel = (NERR_BASE+370); // Try down-level (remote admin protocol) version of API instead.
  {$EXTERNALSYM NERR_TryDownLevel}

//
//  UPS error codes.
//
//          NERR_BASE + (380-384)

  NERR_UPSDriverNotStarted = (NERR_BASE+380); // The UPS driver could not be accessed by the UPS service.
  {$EXTERNALSYM NERR_UPSDriverNotStarted}
  NERR_UPSInvalidConfig    = (NERR_BASE+381); // The UPS service is not configured correctly.
  {$EXTERNALSYM NERR_UPSInvalidConfig}
  NERR_UPSInvalidCommPort  = (NERR_BASE+382); // The UPS service could not access the specified Comm Port.
  {$EXTERNALSYM NERR_UPSInvalidCommPort}
  NERR_UPSSignalAsserted   = (NERR_BASE+383); // The UPS indicated a line fail or low battery situation. Service not started.
  {$EXTERNALSYM NERR_UPSSignalAsserted}
  NERR_UPSShutdownFailed   = (NERR_BASE+384); // The UPS service failed to perform a system shut down.
  {$EXTERNALSYM NERR_UPSShutdownFailed}

//
//  Remoteboot error codes.
//
//           NERR_BASE + (400-419)
//           Error codes 400 - 405 are used by RPLBOOT.SYS.
//           Error codes 403, 407 - 416 are used by RPLLOADR.COM,
//           Error code 417 is the alerter message of REMOTEBOOT (RPLSERVR.EXE).
//           Error code 418 is for when REMOTEBOOT can't start
//           Error code 419 is for a disallowed 2nd rpl connection
//

  NERR_BadDosRetCode      = (NERR_BASE+400); // The program below returned an MS-DOS error code:
  {$EXTERNALSYM NERR_BadDosRetCode}
  NERR_ProgNeedsExtraMem  = (NERR_BASE+401); // The program below needs more memory:
  {$EXTERNALSYM NERR_ProgNeedsExtraMem}
  NERR_BadDosFunction     = (NERR_BASE+402); // The program below called an unsupported MS-DOS function:
  {$EXTERNALSYM NERR_BadDosFunction}
  NERR_RemoteBootFailed   = (NERR_BASE+403); // The workstation failed to boot.
  {$EXTERNALSYM NERR_RemoteBootFailed}
  NERR_BadFileCheckSum    = (NERR_BASE+404); // The file below is corrupt.
  {$EXTERNALSYM NERR_BadFileCheckSum}
  NERR_NoRplBootSystem    = (NERR_BASE+405); // No loader is specified in the boot-block definition file.
  {$EXTERNALSYM NERR_NoRplBootSystem}
  NERR_RplLoadrNetBiosErr = (NERR_BASE+406); // NetBIOS returned an error: The NCB and SMB are dumped above.
  {$EXTERNALSYM NERR_RplLoadrNetBiosErr}
  NERR_RplLoadrDiskErr    = (NERR_BASE+407); // A disk I/O error occurred.
  {$EXTERNALSYM NERR_RplLoadrDiskErr}
  NERR_ImageParamErr      = (NERR_BASE+408); // Image parameter substitution failed.
  {$EXTERNALSYM NERR_ImageParamErr}
  NERR_TooManyImageParams = (NERR_BASE+409); // Too many image parameters cross disk sector boundaries.
  {$EXTERNALSYM NERR_TooManyImageParams}
  NERR_NonDosFloppyUsed   = (NERR_BASE+410); // The image was not generated from an MS-DOS diskette formatted with /S.
  {$EXTERNALSYM NERR_NonDosFloppyUsed}
  NERR_RplBootRestart     = (NERR_BASE+411); // Remote boot will be restarted later.
  {$EXTERNALSYM NERR_RplBootRestart}
  NERR_RplSrvrCallFailed  = (NERR_BASE+412); // The call to the Remoteboot server failed.
  {$EXTERNALSYM NERR_RplSrvrCallFailed}
  NERR_CantConnectRplSrvr = (NERR_BASE+413); // Cannot connect to the Remoteboot server.
  {$EXTERNALSYM NERR_CantConnectRplSrvr}
  NERR_CantOpenImageFile  = (NERR_BASE+414); // Cannot open image file on the Remoteboot server.
  {$EXTERNALSYM NERR_CantOpenImageFile}
  NERR_CallingRplSrvr     = (NERR_BASE+415); // Connecting to the Remoteboot server...
  {$EXTERNALSYM NERR_CallingRplSrvr}
  NERR_StartingRplBoot    = (NERR_BASE+416); // Connecting to the Remoteboot server...
  {$EXTERNALSYM NERR_StartingRplBoot}
  NERR_RplBootServiceTerm = (NERR_BASE+417); // Remote boot service was stopped; check the error log for the cause of the problem.
  {$EXTERNALSYM NERR_RplBootServiceTerm}
  NERR_RplBootStartFailed = (NERR_BASE+418); // Remote boot startup failed; check the error log for the cause of the problem.
  {$EXTERNALSYM NERR_RplBootStartFailed}
  NERR_RPL_CONNECTED      = (NERR_BASE+419); // A second connection to a Remoteboot resource is not allowed.
  {$EXTERNALSYM NERR_RPL_CONNECTED}

//
//  FTADMIN API error codes
//
//       NERR_BASE + (425-434)
//
//       (Currently not used in NT)
//


//
//  Browser service API error codes
//
//       NERR_BASE + (450-475)
//

  NERR_BrowserConfiguredToNotRun = (NERR_BASE+450); // The browser service was configured with MaintainServerList=No.
  {$EXTERNALSYM NERR_BrowserConfiguredToNotRun}

//
//  Additional Remoteboot error codes.
//
//          NERR_BASE + (510-550)

  NERR_RplNoAdaptersStarted      = (NERR_BASE+510); // Service failed to start since none of the network adapters started with this service.
  {$EXTERNALSYM NERR_RplNoAdaptersStarted}
  NERR_RplBadRegistry            = (NERR_BASE+511); // Service failed to start due to bad startup information in the registry.
  {$EXTERNALSYM NERR_RplBadRegistry}
  NERR_RplBadDatabase            = (NERR_BASE+512); // Service failed to start because its database is absent or corrupt.
  {$EXTERNALSYM NERR_RplBadDatabase}
  NERR_RplRplfilesShare          = (NERR_BASE+513); // Service failed to start because RPLFILES share is absent.
  {$EXTERNALSYM NERR_RplRplfilesShare}
  NERR_RplNotRplServer           = (NERR_BASE+514); // Service failed to start because RPLUSER group is absent.
  {$EXTERNALSYM NERR_RplNotRplServer}
  NERR_RplCannotEnum             = (NERR_BASE+515); // Cannot enumerate service records.
  {$EXTERNALSYM NERR_RplCannotEnum}
  NERR_RplWkstaInfoCorrupted     = (NERR_BASE+516); // Workstation record information has been corrupted.
  {$EXTERNALSYM NERR_RplWkstaInfoCorrupted}
  NERR_RplWkstaNotFound          = (NERR_BASE+517); // Workstation record was not found.
  {$EXTERNALSYM NERR_RplWkstaNotFound}
  NERR_RplWkstaNameUnavailable   = (NERR_BASE+518); // Workstation name is in use by some other workstation.
  {$EXTERNALSYM NERR_RplWkstaNameUnavailable}
  NERR_RplProfileInfoCorrupted   = (NERR_BASE+519); // Profile record information has been corrupted.
  {$EXTERNALSYM NERR_RplProfileInfoCorrupted}
  NERR_RplProfileNotFound        = (NERR_BASE+520); // Profile record was not found.
  {$EXTERNALSYM NERR_RplProfileNotFound}
  NERR_RplProfileNameUnavailable = (NERR_BASE+521); // Profile name is in use by some other profile.
  {$EXTERNALSYM NERR_RplProfileNameUnavailable}
  NERR_RplProfileNotEmpty        = (NERR_BASE+522); // There are workstations using this profile.
  {$EXTERNALSYM NERR_RplProfileNotEmpty}
  NERR_RplConfigInfoCorrupted    = (NERR_BASE+523); // Configuration record information has been corrupted.
  {$EXTERNALSYM NERR_RplConfigInfoCorrupted}
  NERR_RplConfigNotFound         = (NERR_BASE+524); // Configuration record was not found.
  {$EXTERNALSYM NERR_RplConfigNotFound}
  NERR_RplAdapterInfoCorrupted   = (NERR_BASE+525); // Adapter id record information has been corrupted.
  {$EXTERNALSYM NERR_RplAdapterInfoCorrupted}
  NERR_RplInternal               = (NERR_BASE+526); // An internal service error has occurred.
  {$EXTERNALSYM NERR_RplInternal}
  NERR_RplVendorInfoCorrupted    = (NERR_BASE+527); // Vendor id record information has been corrupted.
  {$EXTERNALSYM NERR_RplVendorInfoCorrupted}
  NERR_RplBootInfoCorrupted      = (NERR_BASE+528); // Boot block record information has been corrupted.
  {$EXTERNALSYM NERR_RplBootInfoCorrupted}
  NERR_RplWkstaNeedsUserAcct     = (NERR_BASE+529); // The user account for this workstation record is missing.
  {$EXTERNALSYM NERR_RplWkstaNeedsUserAcct}
  NERR_RplNeedsRPLUSERAcct       = (NERR_BASE+530); // The RPLUSER local group could not be found.
  {$EXTERNALSYM NERR_RplNeedsRPLUSERAcct}
  NERR_RplBootNotFound           = (NERR_BASE+531); // Boot block record was not found.
  {$EXTERNALSYM NERR_RplBootNotFound}
  NERR_RplIncompatibleProfile    = (NERR_BASE+532); // Chosen profile is incompatible with this workstation.
  {$EXTERNALSYM NERR_RplIncompatibleProfile}
  NERR_RplAdapterNameUnavailable = (NERR_BASE+533); // Chosen network adapter id is in use by some other workstation.
  {$EXTERNALSYM NERR_RplAdapterNameUnavailable}
  NERR_RplConfigNotEmpty         = (NERR_BASE+534); // There are profiles using this configuration.
  {$EXTERNALSYM NERR_RplConfigNotEmpty}
  NERR_RplBootInUse              = (NERR_BASE+535); // There are workstations, profiles or configurations using this boot block.
  {$EXTERNALSYM NERR_RplBootInUse}
  NERR_RplBackupDatabase         = (NERR_BASE+536); // Service failed to backup Remoteboot database.
  {$EXTERNALSYM NERR_RplBackupDatabase}
  NERR_RplAdapterNotFound        = (NERR_BASE+537); // Adapter record was not found.
  {$EXTERNALSYM NERR_RplAdapterNotFound}
  NERR_RplVendorNotFound         = (NERR_BASE+538); // Vendor record was not found.
  {$EXTERNALSYM NERR_RplVendorNotFound}
  NERR_RplVendorNameUnavailable  = (NERR_BASE+539); // Vendor name is in use by some other vendor record.
  {$EXTERNALSYM NERR_RplVendorNameUnavailable}
  NERR_RplBootNameUnavailable    = (NERR_BASE+540); // (boot name, vendor id) is in use by some other boot block record.
  {$EXTERNALSYM NERR_RplBootNameUnavailable}
  NERR_RplConfigNameUnavailable  = (NERR_BASE+541); // Configuration name is in use by some other configuration.
  {$EXTERNALSYM NERR_RplConfigNameUnavailable}

//*INTERNAL_ONLY*

//
//  Dfs API error codes.
//
//          NERR_BASE + (560-590)


  NERR_DfsInternalCorruption        = (NERR_BASE+560); // The internal database maintained by the DFS service is corrupt
  {$EXTERNALSYM NERR_DfsInternalCorruption}
  NERR_DfsVolumeDataCorrupt         = (NERR_BASE+561); // One of the records in the internal DFS database is corrupt
  {$EXTERNALSYM NERR_DfsVolumeDataCorrupt}
  NERR_DfsNoSuchVolume              = (NERR_BASE+562); // There is no DFS name whose entry path matches the input Entry Path
  {$EXTERNALSYM NERR_DfsNoSuchVolume}
  NERR_DfsVolumeAlreadyExists       = (NERR_BASE+563); // A root or link with the given name already exists
  {$EXTERNALSYM NERR_DfsVolumeAlreadyExists}
  NERR_DfsAlreadyShared             = (NERR_BASE+564); // The server share specified is already shared in the DFS
  {$EXTERNALSYM NERR_DfsAlreadyShared}
  NERR_DfsNoSuchShare               = (NERR_BASE+565); // The indicated server share does not support the indicated DFS namespace
  {$EXTERNALSYM NERR_DfsNoSuchShare}
  NERR_DfsNotALeafVolume            = (NERR_BASE+566); // The operation is not valid on this portion of the namespace
  {$EXTERNALSYM NERR_DfsNotALeafVolume}
  NERR_DfsLeafVolume                = (NERR_BASE+567); // The operation is not valid on this portion of the namespace
  {$EXTERNALSYM NERR_DfsLeafVolume}
  NERR_DfsVolumeHasMultipleServers  = (NERR_BASE+568); // The operation is ambiguous because the link has multiple servers
  {$EXTERNALSYM NERR_DfsVolumeHasMultipleServers}
  NERR_DfsCantCreateJunctionPoint   = (NERR_BASE+569); // Unable to create a link
  {$EXTERNALSYM NERR_DfsCantCreateJunctionPoint}
  NERR_DfsServerNotDfsAware         = (NERR_BASE+570); // The server is not DFS Aware
  {$EXTERNALSYM NERR_DfsServerNotDfsAware}
  NERR_DfsBadRenamePath             = (NERR_BASE+571); // The specified rename target path is invalid
  {$EXTERNALSYM NERR_DfsBadRenamePath}
  NERR_DfsVolumeIsOffline           = (NERR_BASE+572); // The specified DFS link is offline
  {$EXTERNALSYM NERR_DfsVolumeIsOffline}
  NERR_DfsNoSuchServer              = (NERR_BASE+573); // The specified server is not a server for this link
  {$EXTERNALSYM NERR_DfsNoSuchServer}
  NERR_DfsCyclicalName              = (NERR_BASE+574); // A cycle in the DFS name was detected
  {$EXTERNALSYM NERR_DfsCyclicalName}
  NERR_DfsNotSupportedInServerDfs   = (NERR_BASE+575); // The operation is not supported on a server-based DFS
  {$EXTERNALSYM NERR_DfsNotSupportedInServerDfs}
  NERR_DfsDuplicateService          = (NERR_BASE+576); // This link is already supported by the specified server-share
  {$EXTERNALSYM NERR_DfsDuplicateService}
  NERR_DfsCantRemoveLastServerShare = (NERR_BASE+577); // Can't remove the last server-share supporting this root or link
  {$EXTERNALSYM NERR_DfsCantRemoveLastServerShare}
  NERR_DfsVolumeIsInterDfs          = (NERR_BASE+578); // The operation is not supported for an Inter-DFS link
  {$EXTERNALSYM NERR_DfsVolumeIsInterDfs}
  NERR_DfsInconsistent              = (NERR_BASE+579); // The internal state of the DFS Service has become inconsistent
  {$EXTERNALSYM NERR_DfsInconsistent}
  NERR_DfsServerUpgraded            = (NERR_BASE+580); // The DFS Service has been installed on the specified server
  {$EXTERNALSYM NERR_DfsServerUpgraded}
  NERR_DfsDataIsIdentical           = (NERR_BASE+581); // The DFS data being reconciled is identical
  {$EXTERNALSYM NERR_DfsDataIsIdentical}
  NERR_DfsCantRemoveDfsRoot         = (NERR_BASE+582); // The DFS root cannot be deleted - Uninstall DFS if required
  {$EXTERNALSYM NERR_DfsCantRemoveDfsRoot}
  NERR_DfsChildOrParentInDfs        = (NERR_BASE+583); // A child or parent directory of the share is already in a DFS
  {$EXTERNALSYM NERR_DfsChildOrParentInDfs}
  NERR_DfsInternalError             = (NERR_BASE+590); // DFS internal error
  {$EXTERNALSYM NERR_DfsInternalError}

//
//  Net setup error codes.
//
//          NERR_BASE + (591-600)

  NERR_SetupAlreadyJoined           = (NERR_BASE+591); // This machine is already joined to a domain.
  {$EXTERNALSYM NERR_SetupAlreadyJoined}
  NERR_SetupNotJoined               = (NERR_BASE+592); // This machine is not currently joined to a domain.
  {$EXTERNALSYM NERR_SetupNotJoined}
  NERR_SetupDomainController        = (NERR_BASE+593); // This machine is a domain controller and cannot be unjoined from a domain.
  {$EXTERNALSYM NERR_SetupDomainController}
  NERR_DefaultJoinRequired          = (NERR_BASE+594); // The destination domain controller does not support creating machine accounts in OUs.
  {$EXTERNALSYM NERR_DefaultJoinRequired}
  NERR_InvalidWorkgroupName         = (NERR_BASE+595); // The specified workgroup name is invalid.
  {$EXTERNALSYM NERR_InvalidWorkgroupName}
  NERR_NameUsesIncompatibleCodePage = (NERR_BASE+596); // The specified computer name is incompatible with the default language used on the domain controller.
  {$EXTERNALSYM NERR_NameUsesIncompatibleCodePage}
  NERR_ComputerAccountNotFound      = (NERR_BASE+597); // The specified computer account could not be found.
  {$EXTERNALSYM NERR_ComputerAccountNotFound}
  NERR_PersonalSku                  = (NERR_BASE+598); // This version of Windows cannot be joined to a domain.
  {$EXTERNALSYM NERR_PersonalSku}

//
//  Some Password and account error results
//
//          NERR_BASE + (601 - 608)
//

  NERR_PasswordMustChange           = (NERR_BASE + 601);   // Password must change at next logon
  {$EXTERNALSYM NERR_PasswordMustChange}
  NERR_AccountLockedOut             = (NERR_BASE + 602);   // Account is locked out
  {$EXTERNALSYM NERR_AccountLockedOut}
  NERR_PasswordTooLong              = (NERR_BASE + 603);   // Password is too long
  {$EXTERNALSYM NERR_PasswordTooLong}
  NERR_PasswordNotComplexEnough     = (NERR_BASE + 604);   // Password doesn't meet the complexity policy
  {$EXTERNALSYM NERR_PasswordNotComplexEnough}
  NERR_PasswordFilterError          = (NERR_BASE + 605);   // Password doesn't meet the requirements of the filter dll's
  {$EXTERNALSYM NERR_PasswordFilterError}

//**********WARNING ****************
//The range 2750-2799 has been     *
//allocated to the IBM LAN Server  *
//*********************************

//**********WARNING ****************
//The range 2900-2999 has been     *
//reserved for Microsoft OEMs      *
//*********************************

//*END_INTERNAL*

  MAX_NERR = (NERR_BASE+899); // This is the last error in NERR range.
  {$EXTERNALSYM MAX_NERR}

//
// end of list
//
//    WARNING:  Do not exceed MAX_NERR; values above this are used by
//              other error code ranges (errlog.h, service.h, apperr.h).

// JwaLmCons, complete
// LAN Manager common definitions

const
  NetApi32 = 'netapi32.dll';

//
// NOTE:  Lengths of strings are given as the maximum lengths of the
// string in characters (not bytes).  This does not include space for the
// terminating 0-characters.  When allocating space for such an item,
// use the form:
//
//     TCHAR username[UNLEN+1];
//
// Definitions of the form LN20_* define those values in effect for
// LanMan 2.0.
//

//
// String Lengths for various LanMan names
//

const
  CNLEN      = 15; // Computer name length
  {$EXTERNALSYM CNLEN}
  LM20_CNLEN = 15; // LM 2.0 Computer name length
  {$EXTERNALSYM LM20_CNLEN}
  DNLEN      = CNLEN; // Maximum domain name length
  {$EXTERNALSYM DNLEN}
  LM20_DNLEN = LM20_CNLEN; // LM 2.0 Maximum domain name length
  {$EXTERNALSYM LM20_DNLEN}

//#if (CNLEN != DNLEN)
//#error CNLEN and DNLEN are not equal
//#endif

  UNCLEN      = (CNLEN+2); // UNC computer name length
  {$EXTERNALSYM UNCLEN}
  LM20_UNCLEN = (LM20_CNLEN+2); // LM 2.0 UNC computer name length
  {$EXTERNALSYM LM20_UNCLEN}

  NNLEN      = 80; // Net name length (share name)
  {$EXTERNALSYM NNLEN}
  LM20_NNLEN = 12; // LM 2.0 Net name length
  {$EXTERNALSYM LM20_NNLEN}

  RMLEN      = (UNCLEN+1+NNLEN); // Max remote name length
  {$EXTERNALSYM RMLEN}
  LM20_RMLEN = (LM20_UNCLEN+1+LM20_NNLEN); // LM 2.0 Max remote name length
  {$EXTERNALSYM LM20_RMLEN}

  SNLEN        = 80; // Service name length
  {$EXTERNALSYM SNLEN}
  LM20_SNLEN   = 15; // LM 2.0 Service name length
  {$EXTERNALSYM LM20_SNLEN}
  STXTLEN      = 256; // Service text length
  {$EXTERNALSYM STXTLEN}
  LM20_STXTLEN = 63; // LM 2.0 Service text length
  {$EXTERNALSYM LM20_STXTLEN}

  PATHLEN      = 256; // Max. path (not including drive name)
  {$EXTERNALSYM PATHLEN}
  LM20_PATHLEN = 256; // LM 2.0 Max. path
  {$EXTERNALSYM LM20_PATHLEN}

  DEVLEN      = 80; // Device name length
  {$EXTERNALSYM DEVLEN}
  LM20_DEVLEN = 8; // LM 2.0 Device name length
  {$EXTERNALSYM LM20_DEVLEN}

  EVLEN = 16; // Event name length
  {$EXTERNALSYM EVLEN}

//
// User, Group and Password lengths
//

  UNLEN      = 256; // Maximum user name length
  {$EXTERNALSYM UNLEN}
  LM20_UNLEN = 20; // LM 2.0 Maximum user name length
  {$EXTERNALSYM LM20_UNLEN}

  GNLEN      = UNLEN; // Group name
  {$EXTERNALSYM GNLEN}
  LM20_GNLEN = LM20_UNLEN; // LM 2.0 Group name
  {$EXTERNALSYM LM20_GNLEN}

  PWLEN      = 256; // Maximum password length
  {$EXTERNALSYM PWLEN}
  LM20_PWLEN = 14; // LM 2.0 Maximum password length
  {$EXTERNALSYM LM20_PWLEN}

  SHPWLEN = 8; // Share password length (bytes)
  {$EXTERNALSYM SHPWLEN}

  CLTYPE_LEN = 12; // Length of client type string
  {$EXTERNALSYM CLTYPE_LEN}

  MAXCOMMENTSZ      = 256; // Multipurpose comment length
  {$EXTERNALSYM MAXCOMMENTSZ}
  LM20_MAXCOMMENTSZ = 48; // LM 2.0 Multipurpose comment length
  {$EXTERNALSYM LM20_MAXCOMMENTSZ}

  QNLEN      = NNLEN; // Queue name maximum length
  {$EXTERNALSYM QNLEN}
  LM20_QNLEN = LM20_NNLEN; // LM 2.0 Queue name maximum length
  {$EXTERNALSYM LM20_QNLEN}

//#if (QNLEN != NNLEN)
//# error QNLEN and NNLEN are not equal
//#endif

//
// The ALERTSZ and MAXDEVENTRIES defines have not yet been NT'ized.
// Whoever ports these components should change these values appropriately.
//

  ALERTSZ       = 128; // size of alert string in server
  {$EXTERNALSYM ALERTSZ}
  MAXDEVENTRIES = (SizeOf(Integer)*8); // Max number of device entries
  {$EXTERNALSYM MAXDEVENTRIES}

                                        //
                                        // We use int bitmap to represent
                                        //

  NETBIOS_NAME_LEN = 16; // NetBIOS net name (bytes)
  {$EXTERNALSYM NETBIOS_NAME_LEN}

//
// Value to be used with APIs which have a "preferred maximum length"
// parameter.  This value indicates that the API should just allocate
// "as much as it takes."
//

  MAX_PREFERRED_LENGTH = DWORD(-1);
  {$EXTERNALSYM MAX_PREFERRED_LENGTH}

//
//        Constants used with encryption
//

  CRYPT_KEY_LEN      = 7;
  {$EXTERNALSYM CRYPT_KEY_LEN}
  CRYPT_TXT_LEN      = 8;
  {$EXTERNALSYM CRYPT_TXT_LEN}
  ENCRYPTED_PWLEN    = 16;
  {$EXTERNALSYM ENCRYPTED_PWLEN}
  SESSION_PWLEN      = 24;
  {$EXTERNALSYM SESSION_PWLEN}
  SESSION_CRYPT_KLEN = 21;
  {$EXTERNALSYM SESSION_CRYPT_KLEN}

//
//  Value to be used with SetInfo calls to allow setting of all
//  settable parameters (parmnum zero option)
//

  PARMNUM_ALL = 0;
  {$EXTERNALSYM PARMNUM_ALL}

  PARM_ERROR_UNKNOWN     = DWORD(-1);
  {$EXTERNALSYM PARM_ERROR_UNKNOWN}
  PARM_ERROR_NONE        = 0;
  {$EXTERNALSYM PARM_ERROR_NONE}
  PARMNUM_BASE_INFOLEVEL = 1000;
  {$EXTERNALSYM PARMNUM_BASE_INFOLEVEL}

//
// Only the UNICODE version of the LM APIs are available on NT.
// Non-UNICODE version on other platforms
//

//#if defined( _WIN32_WINNT ) || defined( WINNT ) || defined( FORCE_UNICODE )

{$IFDEF _WIN32_WINNT}
{$DEFINE LM_USE_UNICODE}
{$ENDIF}

{$IFDEF FORCE_UNICODE}
{$DEFINE LM_USE_UNICODE}
{$ENDIF}

{$IFDEF LM_USE_UNICODE}

type
  LMSTR = LPWSTR;
  {$EXTERNALSYM LMSTR}
  LMCSTR = LPCWSTR;
  {$EXTERNALSYM LMCSTR}
  PLMSTR = ^LMSTR;
  {$NODEFINE PLMSTR}

{$ELSE}

type
  LMSTR = LPSTR;
  {$EXTERNALSYM LMSTR}
  LMCSTR = LPCSTR;
  {$EXTERNALSYM LMCSTR}

{$ENDIF}

//
//        Message File Names
//

const
  MESSAGE_FILENAME  = 'NETMSG';
  {$EXTERNALSYM MESSAGE_FILENAME}
  OS2MSG_FILENAME   = 'BASE';
  {$EXTERNALSYM OS2MSG_FILENAME}
  HELP_MSG_FILENAME = 'NETH';
  {$EXTERNALSYM HELP_MSG_FILENAME}

// ** INTERNAL_ONLY **

// The backup message file named here is a duplicate of net.msg. It
// is not shipped with the product, but is used at buildtime to
// msgbind certain messages to netapi.dll and some of the services.
// This allows for OEMs to modify the message text in net.msg and
// have those changes show up.        Only in case there is an error in
// retrieving the messages from net.msg do we then get the bound
// messages out of bak.msg (really out of the message segment).

  BACKUP_MSG_FILENAME = 'BAK.MSG';
  {$EXTERNALSYM BACKUP_MSG_FILENAME}

// ** END_INTERNAL **

//
// Keywords used in Function Prototypes
//

type
  NET_API_STATUS = DWORD;
  {$EXTERNALSYM NET_API_STATUS}
  TNetApiStatus = NET_API_STATUS;

//
// The platform ID indicates the levels to use for platform-specific
// information.
//

const
  PLATFORM_ID_DOS = 300;
  {$EXTERNALSYM PLATFORM_ID_DOS}
  PLATFORM_ID_OS2 = 400;
  {$EXTERNALSYM PLATFORM_ID_OS2}
  PLATFORM_ID_NT  = 500;
  {$EXTERNALSYM PLATFORM_ID_NT}
  PLATFORM_ID_OSF = 600;
  {$EXTERNALSYM PLATFORM_ID_OSF}
  PLATFORM_ID_VMS = 700;
  {$EXTERNALSYM PLATFORM_ID_VMS}

//
//      There message numbers assigned to different LANMAN components
//      are as defined below.
//
//      lmerr.h:        2100 - 2999     NERR_BASE
//      alertmsg.h:     3000 - 3049     ALERT_BASE
//      lmsvc.h:        3050 - 3099     SERVICE_BASE
//      lmerrlog.h:     3100 - 3299     ERRLOG_BASE
//      msgtext.h:      3300 - 3499     MTXT_BASE
//      apperr.h:       3500 - 3999     APPERR_BASE
//      apperrfs.h:     4000 - 4299     APPERRFS_BASE
//      apperr2.h:      4300 - 5299     APPERR2_BASE
//      ncberr.h:       5300 - 5499     NRCERR_BASE
//      alertmsg.h:     5500 - 5599     ALERT2_BASE
//      lmsvc.h:        5600 - 5699     SERVICE2_BASE
//      lmerrlog.h      5700 - 5899     ERRLOG2_BASE
//

  MIN_LANMAN_MESSAGE_ID = NERR_BASE;
  {$EXTERNALSYM MIN_LANMAN_MESSAGE_ID}
  MAX_LANMAN_MESSAGE_ID = 5899;
  {$EXTERNALSYM MAX_LANMAN_MESSAGE_ID}

// line 59

//
// Function Prototypes - User
//

{$IFNDEF CLR}

function NetUserAdd(servername: LPCWSTR; level: DWORD; buf: PByte; parm_err: LPDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetUserAdd}

function NetUserEnum(servername: LPCWSTR; level, filter: DWORD; var bufptr: PByte; prefmaxlen: DWORD; entriesread, totalentries, resume_handle: LPDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetUserEnum}

function NetUserGetInfo(servername, username: LPCWSTR; level: DWORD; var bufptr: PByte): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetUserGetInfo}

function NetUserSetInfo(servername, username: LPCWSTR; level: DWORD; buf: PByte; parm_err: LPDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetUserSetInfo}

function NetUserDel(servername: LPCWSTR; username: LPCWSTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetUserDel}

function NetUserGetGroups(servername, username: LPCWSTR; level: DWORD; var bufptr: PByte; prefmaxlen: DWORD; entriesread, totalentries: LPDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetUserGetGroups}

function NetUserSetGroups(servername, username: LPCWSTR; level: DWORD; buf: PByte; num_entries: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetUserSetGroups}

function NetUserGetLocalGroups(servername, username: LPCWSTR; level, flags: DWORD; var bufptr: PByte; prefmaxlen: DWORD; entriesread, totalentries: LPDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetUserGetLocalGroups}

function NetUserModalsGet(servername: LPCWSTR; level: DWORD; var bufptr: PByte): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetUserModalsGet}

function NetUserModalsSet(servername: LPCWSTR; level: DWORD; buf: PByte; parm_err: LPDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetUserModalsSet}

function NetUserChangePassword(domainname, username, oldpassword, newpassword: LPCWSTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetUserChangePassword}

{$ENDIF ~CLR}

//
//  Data Structures - User
//

type
  LPUSER_INFO_0 = ^USER_INFO_0;
  {$EXTERNALSYM LPUSER_INFO_0}
  PUSER_INFO_0 = ^USER_INFO_0;
  {$EXTERNALSYM PUSER_INFO_0}
  _USER_INFO_0 = record
    usri0_name: LPWSTR;
  end;
  {$EXTERNALSYM _USER_INFO_0}
  USER_INFO_0 = _USER_INFO_0;
  {$EXTERNALSYM USER_INFO_0}
  TUserInfo0 = USER_INFO_0;
  PUserInfo0 = PUSER_INFO_0;

  LPUSER_INFO_1 = ^USER_INFO_1;
  {$EXTERNALSYM LPUSER_INFO_1}
  PUSER_INFO_1 = ^USER_INFO_1;
  {$EXTERNALSYM PUSER_INFO_1}
  _USER_INFO_1 = record
    usri1_name: LPWSTR;
    usri1_password: LPWSTR;
    usri1_password_age: DWORD;
    usri1_priv: DWORD;
    usri1_home_dir: LPWSTR;
    usri1_comment: LPWSTR;
    usri1_flags: DWORD;
    usri1_script_path: LPWSTR;
  end;
  {$EXTERNALSYM _USER_INFO_1}
  USER_INFO_1 = _USER_INFO_1;
  {$EXTERNALSYM USER_INFO_1}
  TUserInfo1 = USER_INFO_1;
  PUserInfo1 = PUSER_INFO_1;

  LPUSER_INFO_2 = ^USER_INFO_2;
  {$EXTERNALSYM LPUSER_INFO_2}
  PUSER_INFO_2 = ^USER_INFO_2;
  {$EXTERNALSYM PUSER_INFO_2}
  _USER_INFO_2 = record
    usri2_name: LPWSTR;
    usri2_password: LPWSTR;
    usri2_password_age: DWORD;
    usri2_priv: DWORD;
    usri2_home_dir: LPWSTR;
    usri2_comment: LPWSTR;
    usri2_flags: DWORD;
    usri2_script_path: LPWSTR;
    usri2_auth_flags: DWORD;
    usri2_full_name: LPWSTR;
    usri2_usr_comment: LPWSTR;
    usri2_parms: LPWSTR;
    usri2_workstations: LPWSTR;
    usri2_last_logon: DWORD;
    usri2_last_logoff: DWORD;
    usri2_acct_expires: DWORD;
    usri2_max_storage: DWORD;
    usri2_units_per_week: DWORD;
    usri2_logon_hours: {$IFDEF CLR}IntPtr{$ELSE}PBYTE{$ENDIF};
    usri2_bad_pw_count: DWORD;
    usri2_num_logons: DWORD;
    usri2_logon_server: LPWSTR;
    usri2_country_code: DWORD;
    usri2_code_page: DWORD;
  end;
  {$EXTERNALSYM _USER_INFO_2}
  USER_INFO_2 = _USER_INFO_2;
  {$EXTERNALSYM USER_INFO_2}
  TUserInfo2 = USER_INFO_2;
  PUserInfo2 = puser_info_2;

// line 799

//
// Special Values and Constants - User
//

//
//  Bit masks for field usriX_flags of USER_INFO_X (X = 0/1).
//

const
  UF_SCRIPT                          = $0001;
  {$EXTERNALSYM UF_SCRIPT}
  UF_ACCOUNTDISABLE                  = $0002;
  {$EXTERNALSYM UF_ACCOUNTDISABLE}
  UF_HOMEDIR_REQUIRED                = $0008;
  {$EXTERNALSYM UF_HOMEDIR_REQUIRED}
  UF_LOCKOUT                         = $0010;
  {$EXTERNALSYM UF_LOCKOUT}
  UF_PASSWD_NOTREQD                  = $0020;
  {$EXTERNALSYM UF_PASSWD_NOTREQD}
  UF_PASSWD_CANT_CHANGE              = $0040;
  {$EXTERNALSYM UF_PASSWD_CANT_CHANGE}
  UF_ENCRYPTED_TEXT_PASSWORD_ALLOWED = $0080;
  {$EXTERNALSYM UF_ENCRYPTED_TEXT_PASSWORD_ALLOWED}

//
// Account type bits as part of usri_flags.
//

  UF_TEMP_DUPLICATE_ACCOUNT    = $0100;
  {$EXTERNALSYM UF_TEMP_DUPLICATE_ACCOUNT}
  UF_NORMAL_ACCOUNT            = $0200;
  {$EXTERNALSYM UF_NORMAL_ACCOUNT}
  UF_INTERDOMAIN_TRUST_ACCOUNT = $0800;
  {$EXTERNALSYM UF_INTERDOMAIN_TRUST_ACCOUNT}
  UF_WORKSTATION_TRUST_ACCOUNT = $1000;
  {$EXTERNALSYM UF_WORKSTATION_TRUST_ACCOUNT}
  UF_SERVER_TRUST_ACCOUNT      = $2000;
  {$EXTERNALSYM UF_SERVER_TRUST_ACCOUNT}

  UF_MACHINE_ACCOUNT_MASK = UF_INTERDOMAIN_TRUST_ACCOUNT or UF_WORKSTATION_TRUST_ACCOUNT or UF_SERVER_TRUST_ACCOUNT;
  {$EXTERNALSYM UF_MACHINE_ACCOUNT_MASK}

  UF_ACCOUNT_TYPE_MASK = UF_TEMP_DUPLICATE_ACCOUNT or UF_NORMAL_ACCOUNT or
    UF_INTERDOMAIN_TRUST_ACCOUNT or UF_WORKSTATION_TRUST_ACCOUNT or UF_SERVER_TRUST_ACCOUNT;
  {$EXTERNALSYM UF_ACCOUNT_TYPE_MASK}

  UF_DONT_EXPIRE_PASSWD                     = $10000;
  {$EXTERNALSYM UF_DONT_EXPIRE_PASSWD}
  UF_MNS_LOGON_ACCOUNT                      = $20000;
  {$EXTERNALSYM UF_MNS_LOGON_ACCOUNT}
  UF_SMARTCARD_REQUIRED                     = $40000;
  {$EXTERNALSYM UF_SMARTCARD_REQUIRED}
  UF_TRUSTED_FOR_DELEGATION                 = $80000;
  {$EXTERNALSYM UF_TRUSTED_FOR_DELEGATION}
  UF_NOT_DELEGATED                          = $100000;
  {$EXTERNALSYM UF_NOT_DELEGATED}
  UF_USE_DES_KEY_ONLY                       = $200000;
  {$EXTERNALSYM UF_USE_DES_KEY_ONLY}
  UF_DONT_REQUIRE_PREAUTH                   = $400000;
  {$EXTERNALSYM UF_DONT_REQUIRE_PREAUTH}
  UF_PASSWORD_EXPIRED                       = DWORD($800000);
  {$EXTERNALSYM UF_PASSWORD_EXPIRED}
  UF_TRUSTED_TO_AUTHENTICATE_FOR_DELEGATION = $1000000;
  {$EXTERNALSYM UF_TRUSTED_TO_AUTHENTICATE_FOR_DELEGATION}


  UF_SETTABLE_BITS =
    UF_SCRIPT or
    UF_ACCOUNTDISABLE or
    UF_LOCKOUT or
    UF_HOMEDIR_REQUIRED or
    UF_PASSWD_NOTREQD or
    UF_PASSWD_CANT_CHANGE or
    UF_ACCOUNT_TYPE_MASK or
    UF_DONT_EXPIRE_PASSWD or
    UF_MNS_LOGON_ACCOUNT or
    UF_ENCRYPTED_TEXT_PASSWORD_ALLOWED or
    UF_SMARTCARD_REQUIRED or
    UF_TRUSTED_FOR_DELEGATION or
    UF_NOT_DELEGATED or
    UF_USE_DES_KEY_ONLY or
    UF_DONT_REQUIRE_PREAUTH or
    UF_PASSWORD_EXPIRED or
    UF_TRUSTED_TO_AUTHENTICATE_FOR_DELEGATION;
  {$EXTERNALSYM UF_SETTABLE_BITS}

// line 1056

//
//  For SetInfo call (parmnum 0) when password change not required
//

  NULL_USERSETINFO_PASSWD = '              ';
  {$EXTERNALSYM NULL_USERSETINFO_PASSWD}

  TIMEQ_FOREVER             = ULONG(-1);
  {$EXTERNALSYM TIMEQ_FOREVER}
  USER_MAXSTORAGE_UNLIMITED = ULONG(-1);
  {$EXTERNALSYM USER_MAXSTORAGE_UNLIMITED}
  USER_NO_LOGOFF            = ULONG(-1);
  {$EXTERNALSYM USER_NO_LOGOFF}
  UNITS_PER_DAY             = 24;
  {$EXTERNALSYM UNITS_PER_DAY}
  UNITS_PER_WEEK            = UNITS_PER_DAY * 7;
  {$EXTERNALSYM UNITS_PER_WEEK}

//
// Privilege levels (USER_INFO_X field usriX_priv (X = 0/1)).
//

  USER_PRIV_MASK  = $3;
  {$EXTERNALSYM USER_PRIV_MASK}
  USER_PRIV_GUEST = 0;
  {$EXTERNALSYM USER_PRIV_GUEST}
  USER_PRIV_USER  = 1;
  {$EXTERNALSYM USER_PRIV_USER}
  USER_PRIV_ADMIN = 2;
  {$EXTERNALSYM USER_PRIV_ADMIN}

// line 1177
  
//
// Group Class
//

//
// Function Prototypes
//

{$IFNDEF CLR}

function NetGroupAdd(servername: LPCWSTR; level: DWORD; buf: PByte; parm_err: LPDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetGroupAdd}

function NetGroupAddUser(servername, GroupName, username: LPCWSTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetGroupAddUser}

function NetGroupEnum(servername: LPCWSTR; level: DWORD; out bufptr: PByte;
  prefmaxlen: DWORD; out entriesread, totalentries: DWORD; resume_handle: PDWORD_PTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetGroupEnum}

function NetGroupGetInfo(servername, groupname: LPCWSTR; level: DWORD; bufptr: PByte): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetGroupGetInfo}

function NetGroupSetInfo(servername, groupname: LPCWSTR; level: DWORD; buf: PByte; parm_err: LPDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetGroupSetInfo}

function NetGroupDel(servername: LPCWSTR; groupname: LPCWSTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetGroupDel}

function NetGroupDelUser(servername: LPCWSTR; GroupName: LPCWSTR; Username: LPCWSTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetGroupDelUser}

function NetGroupGetUsers(servername, groupname: LPCWSTR; level: DWORD; var bufptr: PByte; prefmaxlen: DWORD; entriesread, totalentries: LPDWORD; ResumeHandle: PDWORD_PTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetGroupGetUsers}

function NetGroupSetUsers(servername, groupname: LPCWSTR; level: DWORD; buf: PByte; totalentries: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetGroupSetUsers}

{$ENDIF ~CLR}

//
//  Data Structures - Group
//

type
  LPGROUP_INFO_0 = ^GROUP_INFO_0;
  {$EXTERNALSYM LPGROUP_INFO_0}
  PGROUP_INFO_0 = ^GROUP_INFO_0;
  {$EXTERNALSYM PGROUP_INFO_0}
  _GROUP_INFO_0 = record
    grpi0_name: LPWSTR;
  end;
  {$EXTERNALSYM _GROUP_INFO_0}
  GROUP_INFO_0 = _GROUP_INFO_0;
  {$EXTERNALSYM GROUP_INFO_0}
  TGroupInfo0 = GROUP_INFO_0;
  PGroupInfo0 = PGROUP_INFO_0;

  LPGROUP_INFO_1 = ^GROUP_INFO_1;
  {$EXTERNALSYM LPGROUP_INFO_1}
  PGROUP_INFO_1 = ^GROUP_INFO_1;
  {$EXTERNALSYM PGROUP_INFO_1}
  _GROUP_INFO_1 = record
    grpi1_name: LPWSTR;
    grpi1_comment: LPWSTR;
  end;
  {$EXTERNALSYM _GROUP_INFO_1}
  GROUP_INFO_1 = _GROUP_INFO_1;
  {$EXTERNALSYM GROUP_INFO_1}
  TGroupInfo1 = GROUP_INFO_1;
  PGroupInfo1 = PGROUP_INFO_1;

// line 1380

//
// LocalGroup Class
//

//
// Function Prototypes
//

{$IFNDEF CLR}

function NetLocalGroupAdd(servername: LPCWSTR; level: DWORD; buf: PByte; parm_err: LPDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetLocalGroupAdd}

function NetLocalGroupAddMember(servername, groupname: LPCWSTR; membersid: PSID): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetLocalGroupAddMember}

function NetLocalGroupEnum(servername: LPCWSTR; level: DWORD; out bufptr: PByte;
  prefmaxlen: DWORD; out entriesread, totalentries: DWORD; resumehandle: PDWORD_PTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetLocalGroupEnum}

function NetLocalGroupGetInfo(servername, groupname: LPCWSTR; level: DWORD; var bufptr: PByte): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetLocalGroupGetInfo}

function NetLocalGroupSetInfo(servername, groupname: LPCWSTR; level: DWORD; buf: PByte; parm_err: LPDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetLocalGroupSetInfo}

function NetLocalGroupDel(servername: LPCWSTR; groupname: LPCWSTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetLocalGroupDel}

function NetLocalGroupDelMember(servername: LPCWSTR; groupname: LPCWSTR; membersid: PSID): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetLocalGroupDelMember}

function NetLocalGroupGetMembers(servername, localgroupname: LPCWSTR; level: DWORD; var bufptr: PByte; prefmaxlen: DWORD; entriesread, totalentries: LPDWORD; resumehandle: PDWORD_PTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetLocalGroupGetMembers}

function NetLocalGroupSetMembers(servername, groupname: LPCWSTR; level: DWORD; buf: PByte; totalentries: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetLocalGroupSetMembers}

function NetLocalGroupAddMembers(servername, groupname: LPCWSTR; level: DWORD; buf: PByte; totalentries: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetLocalGroupAddMembers}

function NetLocalGroupDelMembers(servername, groupname: LPCWSTR; level: DWORD; buf: PByte; totalentries: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetLocalGroupDelMembers}

{$ENDIF ~CLR}

//
//  Data Structures - LocalGroup
//

type
  LPLOCALGROUP_INFO_0 = ^LOCALGROUP_INFO_0;
  {$EXTERNALSYM LPLOCALGROUP_INFO_0}
  PLOCALGROUP_INFO_0 = ^LOCALGROUP_INFO_0;
  {$EXTERNALSYM PLOCALGROUP_INFO_0}
  _LOCALGROUP_INFO_0 = record
    lgrpi0_name: LPWSTR;
  end;
  {$EXTERNALSYM _LOCALGROUP_INFO_0}
  LOCALGROUP_INFO_0 = _LOCALGROUP_INFO_0;
  {$EXTERNALSYM LOCALGROUP_INFO_0}
  TLocalGroupInfo0 = LOCALGROUP_INFO_0;
  PLocalGroupInfo0 = PLOCALGROUP_INFO_0;

  LPLOCALGROUP_INFO_1 = ^LOCALGROUP_INFO_1;
  {$EXTERNALSYM LPLOCALGROUP_INFO_1}
  PLOCALGROUP_INFO_1 = ^LOCALGROUP_INFO_1;
  {$EXTERNALSYM PLOCALGROUP_INFO_1}
  _LOCALGROUP_INFO_1 = record
    lgrpi1_name: LPWSTR;
    lgrpi1_comment: LPWSTR;
  end;
  {$EXTERNALSYM _LOCALGROUP_INFO_1}
  LOCALGROUP_INFO_1 = _LOCALGROUP_INFO_1;
  {$EXTERNALSYM LOCALGROUP_INFO_1}
  TLocalGroupInfo1 = LOCALGROUP_INFO_1;
  PLocalGroupInfo1 = PLOCALGROUP_INFO_1;

  LPLOCALGROUP_INFO_1002 = ^LOCALGROUP_INFO_1002;
  {$EXTERNALSYM LPLOCALGROUP_INFO_1002}
  PLOCALGROUP_INFO_1002 = ^LOCALGROUP_INFO_1002;
  {$EXTERNALSYM PLOCALGROUP_INFO_1002}
  _LOCALGROUP_INFO_1002 = record
    lgrpi1002_comment: LPWSTR;
  end;
  {$EXTERNALSYM _LOCALGROUP_INFO_1002}
  LOCALGROUP_INFO_1002 = _LOCALGROUP_INFO_1002;
  {$EXTERNALSYM LOCALGROUP_INFO_1002}
  TLocalGroupInfo1002 = LOCALGROUP_INFO_1002;
  PLocalGroupInfo1002 = PLOCALGROUP_INFO_1002;

  LPLOCALGROUP_MEMBERS_INFO_0 = ^LOCALGROUP_MEMBERS_INFO_0;
  {$EXTERNALSYM LPLOCALGROUP_MEMBERS_INFO_0}
  PLOCALGROUP_MEMBERS_INFO_0 = ^LOCALGROUP_MEMBERS_INFO_0;
  {$EXTERNALSYM PLOCALGROUP_MEMBERS_INFO_0}
  _LOCALGROUP_MEMBERS_INFO_0 = record
    lgrmi0_sid: PSID;
  end;
  {$EXTERNALSYM _LOCALGROUP_MEMBERS_INFO_0}
  LOCALGROUP_MEMBERS_INFO_0 = _LOCALGROUP_MEMBERS_INFO_0;
  {$EXTERNALSYM LOCALGROUP_MEMBERS_INFO_0}
  TLocalGroupMembersInfo0 = LOCALGROUP_MEMBERS_INFO_0;
  PLocalGroupMembersInfo0 = PLOCALGROUP_MEMBERS_INFO_0;

  LPLOCALGROUP_MEMBERS_INFO_1 = ^LOCALGROUP_MEMBERS_INFO_1;
  {$EXTERNALSYM LPLOCALGROUP_MEMBERS_INFO_1}
  PLOCALGROUP_MEMBERS_INFO_1 = ^LOCALGROUP_MEMBERS_INFO_1;
  {$EXTERNALSYM PLOCALGROUP_MEMBERS_INFO_1}
  _LOCALGROUP_MEMBERS_INFO_1 = record
    lgrmi1_sid: PSID;
    lgrmi1_sidusage: SID_NAME_USE;
    lgrmi1_name: LPWSTR;
  end;
  {$EXTERNALSYM _LOCALGROUP_MEMBERS_INFO_1}
  LOCALGROUP_MEMBERS_INFO_1 = _LOCALGROUP_MEMBERS_INFO_1;
  {$EXTERNALSYM LOCALGROUP_MEMBERS_INFO_1}
  TLocalGroupMembersInfo1 = LOCALGROUP_MEMBERS_INFO_1;
  PLocalGroupMembersInfo1 = PLOCALGROUP_MEMBERS_INFO_1;

  LPLOCALGROUP_MEMBERS_INFO_2 = ^LOCALGROUP_MEMBERS_INFO_2;
  {$EXTERNALSYM LPLOCALGROUP_MEMBERS_INFO_2}
  PLOCALGROUP_MEMBERS_INFO_2 = ^LOCALGROUP_MEMBERS_INFO_2;
  {$EXTERNALSYM PLOCALGROUP_MEMBERS_INFO_2}
  _LOCALGROUP_MEMBERS_INFO_2 = record
    lgrmi2_sid: PSID;
    lgrmi2_sidusage: SID_NAME_USE;
    lgrmi2_domainandname: LPWSTR;
  end;
  {$EXTERNALSYM _LOCALGROUP_MEMBERS_INFO_2}
  LOCALGROUP_MEMBERS_INFO_2 = _LOCALGROUP_MEMBERS_INFO_2;
  {$EXTERNALSYM LOCALGROUP_MEMBERS_INFO_2}
  TLocalGroupMembersInfo2 = LOCALGROUP_MEMBERS_INFO_2;
  PLocalGroupMembersInfo2 = PLOCALGROUP_MEMBERS_INFO_2;

  LPLOCALGROUP_MEMBERS_INFO_3 = ^LOCALGROUP_MEMBERS_INFO_3;
  {$EXTERNALSYM LPLOCALGROUP_MEMBERS_INFO_3}
  PLOCALGROUP_MEMBERS_INFO_3 = ^LOCALGROUP_MEMBERS_INFO_3;
  {$EXTERNALSYM PLOCALGROUP_MEMBERS_INFO_3}
  _LOCALGROUP_MEMBERS_INFO_3 = record
    lgrmi3_domainandname: LPWSTR;
  end;
  {$EXTERNALSYM _LOCALGROUP_MEMBERS_INFO_3}
  LOCALGROUP_MEMBERS_INFO_3 = _LOCALGROUP_MEMBERS_INFO_3;
  {$EXTERNALSYM LOCALGROUP_MEMBERS_INFO_3}
  TLocalGroupMembersInfo3 = LOCALGROUP_MEMBERS_INFO_3;
  PLocalGroupMembersInfo3 = PLOCALGROUP_MEMBERS_INFO_3;

{$IFNDEF CLR}

function NetApiBufferFree(Buffer: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetApiBufferFree}

{$ENDIF ~CLR}

{$IFNDEF CLR}

(****************************************************************
 *                                                              *
 *              Data structure templates                        *
 *                                                              *
 ****************************************************************)

const
  NCBNAMSZ = 16;  // absolute length of a net name
  {$EXTERNALSYM NCBNAMSZ}
  MAX_LANA = 254; // lana's in range 0 to MAX_LANA inclusive
  {$EXTERNALSYM MAX_LANA}

//
// Network Control Block
//

type
  PNCB = ^NCB;

  TNcbPost = procedure (P: PNCB); stdcall;

  _NCB = record
    ncb_command: UCHAR;  // command code
    ncb_retcode: UCHAR;  // return code
    ncb_lsn: UCHAR;      // local session number
    ncb_num: UCHAR;      // number of our network name
    ncb_buffer: PChar;   // address of message buffer
    ncb_length: Word;    // size of message buffer
    ncb_callname: array [0..NCBNAMSZ - 1] of Char; // blank-padded name of remote
    ncb_name: array [0..NCBNAMSZ - 1] of Char;     // our blank-padded netname
    ncb_rto: UCHAR;      // rcv timeout/retry count
    ncb_sto: UCHAR;      // send timeout/sys timeout
    ncb_post: TNcbPost;  // POST routine address
    ncb_lana_num: UCHAR; // lana (adapter) number
    ncb_cmd_cplt: UCHAR; // 0xff => commmand pending
    {$IFDEF _WIN64}
    ncb_reserve: array [0..17] of Char; // reserved, used by BIOS
    {$ELSE}
    ncb_reserve: array [0..9] of Char;  // reserved, used by BIOS
    {$ENDIF}
    ncb_event: THandle;   // HANDLE to Win32 event which
                         // will be set to the signalled
                         // state when an ASYNCH command
                         // completes
  end;
  {$EXTERNALSYM _NCB}
  NCB = _NCB;
  {$EXTERNALSYM NCB}
  TNcb = NCB;

//
//  Structure returned to the NCB command NCBASTAT is ADAPTER_STATUS followed
//  by an array of NAME_BUFFER structures.
//

  _ADAPTER_STATUS = record
    adapter_address: array [0..5] of UCHAR;
    rev_major: UCHAR;
    reserved0: UCHAR;
    adapter_type: UCHAR;
    rev_minor: UCHAR;
    duration: WORD;
    frmr_recv: WORD;
    frmr_xmit: WORD;
    iframe_recv_err: WORD;
    xmit_aborts: WORD;
    xmit_success: DWORD;
    recv_success: DWORD;
    iframe_xmit_err: WORD;
    recv_buff_unavail: WORD;
    t1_timeouts: WORD;
    ti_timeouts: WORD;
    reserved1: DWORD;
    free_ncbs: WORD;
    max_cfg_ncbs: WORD;
    max_ncbs: WORD;
    xmit_buf_unavail: WORD;
    max_dgram_size: WORD;
    pending_sess: WORD;
    max_cfg_sess: WORD;
    max_sess: WORD;
    max_sess_pkt_size: WORD;
    name_count: WORD;
  end;
  {$EXTERNALSYM _ADAPTER_STATUS}
  ADAPTER_STATUS = _ADAPTER_STATUS;
  {$EXTERNALSYM ADAPTER_STATUS}
  PADAPTER_STATUS = ^ADAPTER_STATUS;
  {$EXTERNALSYM PADAPTER_STATUS}
  TAdapterStatus = ADAPTER_STATUS;
  PAdapterStatus = PADAPTER_STATUS;

  _NAME_BUFFER = record
    name: array [0..NCBNAMSZ - 1] of Char;
    name_num: UCHAR;
    name_flags: UCHAR;
  end;
  {$EXTERNALSYM _NAME_BUFFER}
  NAME_BUFFER = _NAME_BUFFER;
  {$EXTERNALSYM NAME_BUFFER}
  PNAME_BUFFER = ^NAME_BUFFER;
  {$EXTERNALSYM PNAME_BUFFER}
  TNameBuffer = NAME_BUFFER;
  PNameBuffer = PNAME_BUFFER;

//  values for name_flags bits.

const
  NAME_FLAGS_MASK = $87;
  {$EXTERNALSYM NAME_FLAGS_MASK}

  GROUP_NAME  = $80;
  {$EXTERNALSYM GROUP_NAME}
  UNIQUE_NAME = $00;
  {$EXTERNALSYM UNIQUE_NAME}

  REGISTERING     = $00;
  {$EXTERNALSYM REGISTERING}
  REGISTERED      = $04;
  {$EXTERNALSYM REGISTERED}
  DEREGISTERED    = $05;
  {$EXTERNALSYM DEREGISTERED}
  DUPLICATE       = $06;
  {$EXTERNALSYM DUPLICATE}
  DUPLICATE_DEREG = $07;
  {$EXTERNALSYM DUPLICATE_DEREG}

//
//  Structure returned to the NCB command NCBSSTAT is SESSION_HEADER followed
//  by an array of SESSION_BUFFER structures. If the NCB_NAME starts with an
//  asterisk then an array of these structures is returned containing the
//  status for all names.
//

type
  _SESSION_HEADER = record
    sess_name: UCHAR;
    num_sess: UCHAR;
    rcv_dg_outstanding: UCHAR;
    rcv_any_outstanding: UCHAR;
  end;
  {$EXTERNALSYM _SESSION_HEADER}
  SESSION_HEADER = _SESSION_HEADER;
  {$EXTERNALSYM SESSION_HEADER}
  PSESSION_HEADER = ^SESSION_HEADER;
  {$EXTERNALSYM PSESSION_HEADER}
  TSessionHeader = SESSION_HEADER;
  PSessionHeader = PSESSION_HEADER;

  _SESSION_BUFFER = record
    lsn: UCHAR;
    state: UCHAR;
    local_name: array [0..NCBNAMSZ - 1] of UCHAR;
    remote_name: array [0..NCBNAMSZ - 1] of UCHAR;
    rcvs_outstanding: UCHAR;
    sends_outstanding: UCHAR;
  end;
  {$EXTERNALSYM _SESSION_BUFFER}
  SESSION_BUFFER = _SESSION_BUFFER;
  {$EXTERNALSYM SESSION_BUFFER}
  PSESSION_BUFFER = ^SESSION_BUFFER;
  {$EXTERNALSYM PSESSION_BUFFER}
  TSessionBuffer = SESSION_BUFFER;
  PSessionBuffer = PSESSION_BUFFER;

//  Values for state

const
  LISTEN_OUTSTANDING  = $01;
  {$EXTERNALSYM LISTEN_OUTSTANDING}
  CALL_PENDING        = $02;
  {$EXTERNALSYM CALL_PENDING}
  SESSION_ESTABLISHED = $03;
  {$EXTERNALSYM SESSION_ESTABLISHED}
  HANGUP_PENDING      = $04;
  {$EXTERNALSYM HANGUP_PENDING}
  HANGUP_COMPLETE     = $05;
  {$EXTERNALSYM HANGUP_COMPLETE}
  SESSION_ABORTED     = $06;
  {$EXTERNALSYM SESSION_ABORTED}

//
//  Structure returned to the NCB command NCBENUM.
//
//  On a system containing lana's 0, 2 and 3, a structure with
//  length =3, lana[0]=0, lana[1]=2 and lana[2]=3 will be returned.
//

type
  _LANA_ENUM = record
    length: UCHAR; // Number of valid entries in lana[]
    lana: array [0..MAX_LANA] of UCHAR;
  end;
  {$EXTERNALSYM _LANA_ENUM}
  LANA_ENUM = _LANA_ENUM;
  {$EXTERNALSYM LANA_ENUM}
  PLANA_ENUM = ^LANA_ENUM;
  {$EXTERNALSYM PLANA_ENUM}
  TLanaEnum = LANA_ENUM;
  PLanaEnum = PLANA_ENUM;

//
//  Structure returned to the NCB command NCBFINDNAME is FIND_NAME_HEADER followed
//  by an array of FIND_NAME_BUFFER structures.
//

type
  _FIND_NAME_HEADER = record
    node_count: WORD;
    reserved: UCHAR;
    unique_group: UCHAR;
  end;
  {$EXTERNALSYM _FIND_NAME_HEADER}
  FIND_NAME_HEADER = _FIND_NAME_HEADER;
  {$EXTERNALSYM FIND_NAME_HEADER}
  PFIND_NAME_HEADER = ^FIND_NAME_HEADER;
  {$EXTERNALSYM PFIND_NAME_HEADER}
  TFindNameHeader = FIND_NAME_HEADER;
  PFindNameHeader = PFIND_NAME_HEADER;

  _FIND_NAME_BUFFER = record
    length: UCHAR;
    access_control: UCHAR;
    frame_control: UCHAR;
    destination_addr: array [0..5] of UCHAR;
    source_addr: array [0..5] of UCHAR;
    routing_info: array [0..17] of UCHAR;
  end;
  {$EXTERNALSYM _FIND_NAME_BUFFER}
  FIND_NAME_BUFFER = _FIND_NAME_BUFFER;
  {$EXTERNALSYM FIND_NAME_BUFFER}
  PFIND_NAME_BUFFER = ^FIND_NAME_BUFFER;
  {$EXTERNALSYM PFIND_NAME_BUFFER}
  TFindNameBuffer = FIND_NAME_BUFFER;
  PFindNameBuffer = PFIND_NAME_BUFFER;

//
//  Structure provided with NCBACTION. The purpose of NCBACTION is to provide
//  transport specific extensions to netbios.
//

  _ACTION_HEADER = record
    transport_id: ULONG;
    action_code: USHORT;
    reserved: USHORT;
  end;
  {$EXTERNALSYM _ACTION_HEADER}
  ACTION_HEADER = _ACTION_HEADER;
  {$EXTERNALSYM ACTION_HEADER}
  PACTION_HEADER = ^ACTION_HEADER;
  {$EXTERNALSYM PACTION_HEADER}
  TActionHeader = ACTION_HEADER;
  PActionHeader = PACTION_HEADER;

//  Values for transport_id

const
  ALL_TRANSPORTS = 'M'#0#0#0;
  {$EXTERNALSYM ALL_TRANSPORTS}
  MS_NBF         = 'MNBF';
  {$EXTERNALSYM MS_NBF}

{$ENDIF ~CLR}

(****************************************************************
 *                                                              *
 *              Special values and constants                    *
 *                                                              *
 ****************************************************************)

//
//      NCB Command codes
//

const
  NCBCALL        = $10; // NCB CALL
  {$EXTERNALSYM NCBCALL}
  NCBLISTEN      = $11; // NCB LISTEN
  {$EXTERNALSYM NCBLISTEN}
  NCBHANGUP      = $12; // NCB HANG UP
  {$EXTERNALSYM NCBHANGUP}
  NCBSEND        = $14; // NCB SEND
  {$EXTERNALSYM NCBSEND}
  NCBRECV        = $15; // NCB RECEIVE
  {$EXTERNALSYM NCBRECV}
  NCBRECVANY     = $16; // NCB RECEIVE ANY
  {$EXTERNALSYM NCBRECVANY}
  NCBCHAINSEND   = $17; // NCB CHAIN SEND
  {$EXTERNALSYM NCBCHAINSEND}
  NCBDGSEND      = $20; // NCB SEND DATAGRAM
  {$EXTERNALSYM NCBDGSEND}
  NCBDGRECV      = $21; // NCB RECEIVE DATAGRAM
  {$EXTERNALSYM NCBDGRECV}
  NCBDGSENDBC    = $22; // NCB SEND BROADCAST DATAGRAM
  {$EXTERNALSYM NCBDGSENDBC}
  NCBDGRECVBC    = $23; // NCB RECEIVE BROADCAST DATAGRAM
  {$EXTERNALSYM NCBDGRECVBC}
  NCBADDNAME     = $30; // NCB ADD NAME
  {$EXTERNALSYM NCBADDNAME}
  NCBDELNAME     = $31; // NCB DELETE NAME
  {$EXTERNALSYM NCBDELNAME}
  NCBRESET       = $32; // NCB RESET
  {$EXTERNALSYM NCBRESET}
  NCBASTAT       = $33; // NCB ADAPTER STATUS
  {$EXTERNALSYM NCBASTAT}
  NCBSSTAT       = $34; // NCB SESSION STATUS
  {$EXTERNALSYM NCBSSTAT}
  NCBCANCEL      = $35; // NCB CANCEL
  {$EXTERNALSYM NCBCANCEL}
  NCBADDGRNAME   = $36; // NCB ADD GROUP NAME
  {$EXTERNALSYM NCBADDGRNAME}
  NCBENUM        = $37; // NCB ENUMERATE LANA NUMBERS
  {$EXTERNALSYM NCBENUM}
  NCBUNLINK      = $70; // NCB UNLINK
  {$EXTERNALSYM NCBUNLINK}
  NCBSENDNA      = $71; // NCB SEND NO ACK
  {$EXTERNALSYM NCBSENDNA}
  NCBCHAINSENDNA = $72; // NCB CHAIN SEND NO ACK
  {$EXTERNALSYM NCBCHAINSENDNA}
  NCBLANSTALERT  = $73; // NCB LAN STATUS ALERT
  {$EXTERNALSYM NCBLANSTALERT}
  NCBACTION      = $77; // NCB ACTION
  {$EXTERNALSYM NCBACTION}
  NCBFINDNAME    = $78; // NCB FIND NAME
  {$EXTERNALSYM NCBFINDNAME}
  NCBTRACE       = $79; // NCB TRACE
  {$EXTERNALSYM NCBTRACE}

  ASYNCH = $80; // high bit set == asynchronous
  {$EXTERNALSYM ASYNCH}

//
//      NCB Return codes
//

  NRC_GOODRET = $00; // good return also returned when ASYNCH request accepted
  {$EXTERNALSYM NRC_GOODRET}
  NRC_BUFLEN      = $01; // illegal buffer length
  {$EXTERNALSYM NRC_BUFLEN}
  NRC_ILLCMD      = $03; // illegal command
  {$EXTERNALSYM NRC_ILLCMD}
  NRC_CMDTMO      = $05; // command timed out
  {$EXTERNALSYM NRC_CMDTMO}
  NRC_INCOMP      = $06; // message incomplete, issue another command
  {$EXTERNALSYM NRC_INCOMP}
  NRC_BADDR       = $07; // illegal buffer address
  {$EXTERNALSYM NRC_BADDR}
  NRC_SNUMOUT     = $08; // session number out of range
  {$EXTERNALSYM NRC_SNUMOUT}
  NRC_NORES       = $09; // no resource available
  {$EXTERNALSYM NRC_NORES}
  NRC_SCLOSED     = $0a; // session closed
  {$EXTERNALSYM NRC_SCLOSED}
  NRC_CMDCAN      = $0b; // command cancelled
  {$EXTERNALSYM NRC_CMDCAN}
  NRC_DUPNAME     = $0d; // duplicate name
  {$EXTERNALSYM NRC_DUPNAME}
  NRC_NAMTFUL     = $0e; // name table full
  {$EXTERNALSYM NRC_NAMTFUL}
  NRC_ACTSES      = $0f; // no deletions, name has active sessions
  {$EXTERNALSYM NRC_ACTSES}
  NRC_LOCTFUL     = $11; // local session table full
  {$EXTERNALSYM NRC_LOCTFUL}
  NRC_REMTFUL     = $12; // remote session table full
  {$EXTERNALSYM NRC_REMTFUL}
  NRC_ILLNN       = $13; // illegal name number
  {$EXTERNALSYM NRC_ILLNN}
  NRC_NOCALL      = $14; // no callname
  {$EXTERNALSYM NRC_NOCALL}
  NRC_NOWILD      = $15; // cannot put * in NCB_NAME
  {$EXTERNALSYM NRC_NOWILD}
  NRC_INUSE       = $16; // name in use on remote adapter
  {$EXTERNALSYM NRC_INUSE}
  NRC_NAMERR      = $17; // name deleted
  {$EXTERNALSYM NRC_NAMERR}
  NRC_SABORT      = $18; // session ended abnormally
  {$EXTERNALSYM NRC_SABORT}
  NRC_NAMCONF     = $19; // name conflict detected
  {$EXTERNALSYM NRC_NAMCONF}
  NRC_IFBUSY      = $21; // interface busy, IRET before retrying
  {$EXTERNALSYM NRC_IFBUSY}
  NRC_TOOMANY     = $22; // too many commands outstanding, retry later
  {$EXTERNALSYM NRC_TOOMANY}
  NRC_BRIDGE      = $23; // ncb_lana_num field invalid
  {$EXTERNALSYM NRC_BRIDGE}
  NRC_CANOCCR     = $24; // command completed while cancel occurring
  {$EXTERNALSYM NRC_CANOCCR}
  NRC_CANCEL      = $26; // command not valid to cancel
  {$EXTERNALSYM NRC_CANCEL}
  NRC_DUPENV      = $30; // name defined by anther local process
  {$EXTERNALSYM NRC_DUPENV}
  NRC_ENVNOTDEF   = $34; // environment undefined. RESET required
  {$EXTERNALSYM NRC_ENVNOTDEF}
  NRC_OSRESNOTAV  = $35; // required OS resources exhausted
  {$EXTERNALSYM NRC_OSRESNOTAV}
  NRC_MAXAPPS     = $36; // max number of applications exceeded
  {$EXTERNALSYM NRC_MAXAPPS}
  NRC_NOSAPS      = $37; // no saps available for netbios
  {$EXTERNALSYM NRC_NOSAPS}
  NRC_NORESOURCES = $38; // requested resources are not available
  {$EXTERNALSYM NRC_NORESOURCES}
  NRC_INVADDRESS  = $39; // invalid ncb address or length > segment
  {$EXTERNALSYM NRC_INVADDRESS}
  NRC_INVDDID     = $3B; // invalid NCB DDID
  {$EXTERNALSYM NRC_INVDDID}
  NRC_LOCKFAIL    = $3C; // lock of user area failed
  {$EXTERNALSYM NRC_LOCKFAIL}
  NRC_OPENERR     = $3f; // NETBIOS not loaded
  {$EXTERNALSYM NRC_OPENERR}
  NRC_SYSTEM      = $40; // system error
  {$EXTERNALSYM NRC_SYSTEM}

  NRC_PENDING = $ff; // asynchronous command is not yet finished
  {$EXTERNALSYM NRC_PENDING}

(****************************************************************
 *                                                              *
 *              main user entry point for NetBIOS 3.0           *
 *                                                              *
 * Usage: result = Netbios( pncb );                             *
 ****************************************************************)

{$IFNDEF CLR}
function Netbios(pncb: PNCB): UCHAR; stdcall;
{$EXTERNALSYM Netbios}
{$ENDIF ~CLR}

type
  PRasDialDlg = ^TRasDialDlg;
  tagRASDIALDLG = packed record
    dwSize: DWORD;
    hwndOwner: HWND;
    dwFlags: DWORD;
    xDlg: Longint;
    yDlg: Longint;
    dwSubEntry: DWORD;
    dwError: DWORD;
    reserved: Longword;
    reserved2: Longword;
  end;
  {$EXTERNALSYM tagRASDIALDLG}
  RASDIALDLG = tagRASDIALDLG;
  {$EXTERNALSYM RASDIALDLG}
  TRasDialDlg = tagRASDIALDLG;


// Reason flags

// Flags used by the various UIs.

const
  SHTDN_REASON_FLAG_COMMENT_REQUIRED          = $01000000;
  {$EXTERNALSYM SHTDN_REASON_FLAG_COMMENT_REQUIRED}
  SHTDN_REASON_FLAG_DIRTY_PROBLEM_ID_REQUIRED = $02000000;
  {$EXTERNALSYM SHTDN_REASON_FLAG_DIRTY_PROBLEM_ID_REQUIRED}
  SHTDN_REASON_FLAG_CLEAN_UI                  = $04000000;
  {$EXTERNALSYM SHTDN_REASON_FLAG_CLEAN_UI}
  SHTDN_REASON_FLAG_DIRTY_UI                  = $08000000;
  {$EXTERNALSYM SHTDN_REASON_FLAG_DIRTY_UI}

// Flags that end up in the event log code.

  SHTDN_REASON_FLAG_USER_DEFINED = $40000000;
  {$EXTERNALSYM SHTDN_REASON_FLAG_USER_DEFINED}
  SHTDN_REASON_FLAG_PLANNED      = DWORD($80000000);
  {$EXTERNALSYM SHTDN_REASON_FLAG_PLANNED}

// Microsoft major reasons.

  SHTDN_REASON_MAJOR_OTHER           = $00000000;
  {$EXTERNALSYM SHTDN_REASON_MAJOR_OTHER}
  SHTDN_REASON_MAJOR_NONE            = $00000000;
  {$EXTERNALSYM SHTDN_REASON_MAJOR_NONE}
  SHTDN_REASON_MAJOR_HARDWARE        = $00010000;
  {$EXTERNALSYM SHTDN_REASON_MAJOR_HARDWARE}
  SHTDN_REASON_MAJOR_OPERATINGSYSTEM = $00020000;
  {$EXTERNALSYM SHTDN_REASON_MAJOR_OPERATINGSYSTEM}
  SHTDN_REASON_MAJOR_SOFTWARE        = $00030000;
  {$EXTERNALSYM SHTDN_REASON_MAJOR_SOFTWARE}
  SHTDN_REASON_MAJOR_APPLICATION     = $00040000;
  {$EXTERNALSYM SHTDN_REASON_MAJOR_APPLICATION}
  SHTDN_REASON_MAJOR_SYSTEM          = $00050000;
  {$EXTERNALSYM SHTDN_REASON_MAJOR_SYSTEM}
  SHTDN_REASON_MAJOR_POWER           = $00060000;
  {$EXTERNALSYM SHTDN_REASON_MAJOR_POWER}
  SHTDN_REASON_MAJOR_LEGACY_API      = $00070000;
  {$EXTERNALSYM SHTDN_REASON_MAJOR_LEGACY_API}

// Microsoft minor reasons.

  SHTDN_REASON_MINOR_OTHER           = $00000000;
  {$EXTERNALSYM SHTDN_REASON_MINOR_OTHER}
  SHTDN_REASON_MINOR_NONE            = $000000ff;
  {$EXTERNALSYM SHTDN_REASON_MINOR_NONE}
  SHTDN_REASON_MINOR_MAINTENANCE     = $00000001;
  {$EXTERNALSYM SHTDN_REASON_MINOR_MAINTENANCE}
  SHTDN_REASON_MINOR_INSTALLATION    = $00000002;
  {$EXTERNALSYM SHTDN_REASON_MINOR_INSTALLATION}
  SHTDN_REASON_MINOR_UPGRADE         = $00000003;
  {$EXTERNALSYM SHTDN_REASON_MINOR_UPGRADE}
  SHTDN_REASON_MINOR_RECONFIG        = $00000004;
  {$EXTERNALSYM SHTDN_REASON_MINOR_RECONFIG}
  SHTDN_REASON_MINOR_HUNG            = $00000005;
  {$EXTERNALSYM SHTDN_REASON_MINOR_HUNG}
  SHTDN_REASON_MINOR_UNSTABLE        = $00000006;
  {$EXTERNALSYM SHTDN_REASON_MINOR_UNSTABLE}
  SHTDN_REASON_MINOR_DISK            = $00000007;
  {$EXTERNALSYM SHTDN_REASON_MINOR_DISK}
  SHTDN_REASON_MINOR_PROCESSOR       = $00000008;
  {$EXTERNALSYM SHTDN_REASON_MINOR_PROCESSOR}
  SHTDN_REASON_MINOR_NETWORKCARD     = $00000009;
  {$EXTERNALSYM SHTDN_REASON_MINOR_NETWORKCARD}
  SHTDN_REASON_MINOR_POWER_SUPPLY    = $0000000a;
  {$EXTERNALSYM SHTDN_REASON_MINOR_POWER_SUPPLY}
  SHTDN_REASON_MINOR_CORDUNPLUGGED   = $0000000b;
  {$EXTERNALSYM SHTDN_REASON_MINOR_CORDUNPLUGGED}
  SHTDN_REASON_MINOR_ENVIRONMENT     = $0000000c;
  {$EXTERNALSYM SHTDN_REASON_MINOR_ENVIRONMENT}
  SHTDN_REASON_MINOR_HARDWARE_DRIVER = $0000000d;
  {$EXTERNALSYM SHTDN_REASON_MINOR_HARDWARE_DRIVER}
  SHTDN_REASON_MINOR_OTHERDRIVER     = $0000000e;
  {$EXTERNALSYM SHTDN_REASON_MINOR_OTHERDRIVER}
  SHTDN_REASON_MINOR_BLUESCREEN      = $0000000F;
  {$EXTERNALSYM SHTDN_REASON_MINOR_BLUESCREEN}
  SHTDN_REASON_MINOR_SERVICEPACK           = $00000010;
  {$EXTERNALSYM SHTDN_REASON_MINOR_SERVICEPACK}
  SHTDN_REASON_MINOR_HOTFIX                = $00000011;
  {$EXTERNALSYM SHTDN_REASON_MINOR_HOTFIX}
  SHTDN_REASON_MINOR_SECURITYFIX           = $00000012;
  {$EXTERNALSYM SHTDN_REASON_MINOR_SECURITYFIX}
  SHTDN_REASON_MINOR_SECURITY              = $00000013;
  {$EXTERNALSYM SHTDN_REASON_MINOR_SECURITY}
  SHTDN_REASON_MINOR_NETWORK_CONNECTIVITY  = $00000014;
  {$EXTERNALSYM SHTDN_REASON_MINOR_NETWORK_CONNECTIVITY}
  SHTDN_REASON_MINOR_WMI                   = $00000015;
  {$EXTERNALSYM SHTDN_REASON_MINOR_WMI}
  SHTDN_REASON_MINOR_SERVICEPACK_UNINSTALL = $00000016;
  {$EXTERNALSYM SHTDN_REASON_MINOR_SERVICEPACK_UNINSTALL}
  SHTDN_REASON_MINOR_HOTFIX_UNINSTALL      = $00000017;
  {$EXTERNALSYM SHTDN_REASON_MINOR_HOTFIX_UNINSTALL}
  SHTDN_REASON_MINOR_SECURITYFIX_UNINSTALL = $00000018;
  {$EXTERNALSYM SHTDN_REASON_MINOR_SECURITYFIX_UNINSTALL}
  SHTDN_REASON_MINOR_MMC                   = $00000019;
  {$EXTERNALSYM SHTDN_REASON_MINOR_MMC}
  SHTDN_REASON_MINOR_TERMSRV               = $00000020;
  {$EXTERNALSYM SHTDN_REASON_MINOR_TERMSRV}
  SHTDN_REASON_MINOR_DC_PROMOTION          = $00000021;
  {$EXTERNALSYM SHTDN_REASON_MINOR_DC_PROMOTION}
  SHTDN_REASON_MINOR_DC_DEMOTION           = $00000022;
  {$EXTERNALSYM SHTDN_REASON_MINOR_DC_DEMOTION}

  SHTDN_REASON_UNKNOWN = SHTDN_REASON_MINOR_NONE;
  {$EXTERNALSYM SHTDN_REASON_UNKNOWN}
  SHTDN_REASON_LEGACY_API = (SHTDN_REASON_MAJOR_LEGACY_API or SHTDN_REASON_FLAG_PLANNED);
  {$EXTERNALSYM SHTDN_REASON_LEGACY_API}

// This mask cuts out UI flags.

  SHTDN_REASON_VALID_BIT_MASK = DWORD($c0ffffff);
  {$EXTERNALSYM SHTDN_REASON_VALID_BIT_MASK}

// Convenience flags.

  PCLEANUI = (SHTDN_REASON_FLAG_PLANNED or SHTDN_REASON_FLAG_CLEAN_UI);
  {$EXTERNALSYM PCLEANUI}
  UCLEANUI = (SHTDN_REASON_FLAG_CLEAN_UI);
  {$EXTERNALSYM UCLEANUI}
  PDIRTYUI = (SHTDN_REASON_FLAG_PLANNED or SHTDN_REASON_FLAG_DIRTY_UI);
  {$EXTERNALSYM PDIRTYUI}
  UDIRTYUI = (SHTDN_REASON_FLAG_DIRTY_UI);
  {$EXTERNALSYM UDIRTYUI}


const
  CSIDL_COMMON_APPDATA       = $0023; { All Users\Application Data }
  CSIDL_WINDOWS              = $0024; { GetWindowsDirectory() }
  CSIDL_SYSTEM               = $0025; { GetSystemDirectory() }
  CSIDL_PROGRAM_FILES        = $0026; { C:\Program Files }
  CSIDL_MYPICTURES           = $0027; { C:\Program Files\My Pictures }
  CSIDL_PROFILE              = $0028; { USERPROFILE }
  CSIDL_PROGRAM_FILES_COMMON = $002B; { C:\Program Files\Common }
  CSIDL_COMMON_TEMPLATES     = $002D; { All Users\Templates }
  CSIDL_COMMON_DOCUMENTS     = $002E; { All Users\Documents }
  CSIDL_COMMON_ADMINTOOLS    = $002F; { All Users\Start Menu\Programs\Administrative Tools }
  CSIDL_ADMINTOOLS           = $0030; { <user name>\Start Menu\Programs\Administrative Tools }
  CSIDL_CONNECTIONS          = $0031; { Network and Dial-up Connections }
  CSIDL_COMMON_MUSIC         = $0035; { All Users\My Music }
  CSIDL_COMMON_PICTURES      = $0036; { All Users\My Pictures }
  CSIDL_COMMON_VIDEO         = $0037; { All Users\My Video }
  CSIDL_RESOURCES            = $0038; { Resource Direcotry }
  CSIDL_RESOURCES_LOCALIZED  = $0039; { Localized Resource Direcotry }
  CSIDL_COMMON_OEM_LINKS     = $003A; { Links to All Users OEM specific apps }
  CSIDL_CDBURN_AREA          = $003B; { USERPROFILE\Local Settings\Application Data\Microsoft\CD Burning }
  CSIDL_COMPUTERSNEARME      = $003D; { Computers Near Me (computered from Workgroup membership) }

  {$EXTERNALSYM CSIDL_COMMON_APPDATA}
  {$EXTERNALSYM CSIDL_WINDOWS}
  {$EXTERNALSYM CSIDL_SYSTEM}
  {$EXTERNALSYM CSIDL_PROGRAM_FILES}
  {$EXTERNALSYM CSIDL_MYPICTURES}
  {$EXTERNALSYM CSIDL_PROFILE}
  {$EXTERNALSYM CSIDL_PROGRAM_FILES_COMMON}
  {$EXTERNALSYM CSIDL_COMMON_TEMPLATES}
  {$EXTERNALSYM CSIDL_COMMON_DOCUMENTS}
  {$EXTERNALSYM CSIDL_COMMON_ADMINTOOLS}
  {$EXTERNALSYM CSIDL_ADMINTOOLS}
  {$EXTERNALSYM CSIDL_CONNECTIONS}
  {$EXTERNALSYM CSIDL_COMMON_MUSIC}
  {$EXTERNALSYM CSIDL_COMMON_PICTURES}
  {$EXTERNALSYM CSIDL_COMMON_VIDEO}
  {$EXTERNALSYM CSIDL_RESOURCES}
  {$EXTERNALSYM CSIDL_RESOURCES_LOCALIZED}
  {$EXTERNALSYM CSIDL_COMMON_OEM_LINKS}
  {$EXTERNALSYM CSIDL_CDBURN_AREA}
  {$EXTERNALSYM CSIDL_COMPUTERSNEARME}


{ TODO BCB-compatibility}

const
  DLLVER_PLATFORM_WINDOWS = $00000001;
  {$EXTERNALSYM DLLVER_PLATFORM_WINDOWS}
  DLLVER_PLATFORM_NT      = $00000002;
  {$EXTERNALSYM DLLVER_PLATFORM_NT}

type
  PDllVersionInfo = ^TDllVersionInfo;
  _DLLVERSIONINFO = packed record
    cbSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
  end;
  {$EXTERNALSYM _DLLVERSIONINFO}
  TDllVersionInfo = _DLLVERSIONINFO;
  DLLVERSIONINFO = _DLLVERSIONINFO;
  {$EXTERNALSYM DLLVERSIONINFO}


// JwaWinError
// line 22146

const

//
// Task Scheduler errors
//
//
// MessageId: SCHED_S_TASK_READY
//
// MessageText:
//
//  The task is ready to run at its next scheduled time.
//
  SCHED_S_TASK_READY = HRESULT($00041300);
  {$EXTERNALSYM SCHED_S_TASK_READY}

//
// MessageId: SCHED_S_TASK_RUNNING
//
// MessageText:
//
//  The task is currently running.
//
  SCHED_S_TASK_RUNNING = HRESULT($00041301);
  {$EXTERNALSYM SCHED_S_TASK_RUNNING}

//
// MessageId: SCHED_S_TASK_DISABLED
//
// MessageText:
//
//  The task will not run at the scheduled times because it has been disabled.
//
  SCHED_S_TASK_DISABLED = HRESULT($00041302);
  {$EXTERNALSYM SCHED_S_TASK_DISABLED}

//
// MessageId: SCHED_S_TASK_HAS_NOT_RUN
//
// MessageText:
//
//  The task has not yet run.
//
  SCHED_S_TASK_HAS_NOT_RUN = HRESULT($00041303);
  {$EXTERNALSYM SCHED_S_TASK_HAS_NOT_RUN}

//
// MessageId: SCHED_S_TASK_NO_MORE_RUNS
//
// MessageText:
//
//  There are no more runs scheduled for this task.
//
  SCHED_S_TASK_NO_MORE_RUNS = HRESULT($00041304);
  {$EXTERNALSYM SCHED_S_TASK_NO_MORE_RUNS}

//
// MessageId: SCHED_S_TASK_NOT_SCHEDULED
//
// MessageText:
//
//  One or more of the properties that are needed to run this task on a schedule have not been set.
//
  SCHED_S_TASK_NOT_SCHEDULED = HRESULT($00041305);
  {$EXTERNALSYM SCHED_S_TASK_NOT_SCHEDULED}

//
// MessageId: SCHED_S_TASK_TERMINATED
//
// MessageText:
//
//  The last run of the task was terminated by the user.
//
  SCHED_S_TASK_TERMINATED = HRESULT($00041306);
  {$EXTERNALSYM SCHED_S_TASK_TERMINATED}

//
// MessageId: SCHED_S_TASK_NO_VALID_TRIGGERS
//
// MessageText:
//
//  Either the task has no triggers or the existing triggers are disabled or not set.
//
  SCHED_S_TASK_NO_VALID_TRIGGERS = HRESULT($00041307);
  {$EXTERNALSYM SCHED_S_TASK_NO_VALID_TRIGGERS}

//
// MessageId: SCHED_S_EVENT_TRIGGER
//
// MessageText:
//
//  Event triggers don't have set run times.
//
  SCHED_S_EVENT_TRIGGER = HRESULT($00041308);
  {$EXTERNALSYM SCHED_S_EVENT_TRIGGER}

//
// MessageId: SCHED_E_TRIGGER_NOT_FOUND
//
// MessageText:
//
//  Trigger not found.
//
  SCHED_E_TRIGGER_NOT_FOUND = HRESULT($80041309);
  {$EXTERNALSYM SCHED_E_TRIGGER_NOT_FOUND}

//
// MessageId: SCHED_E_TASK_NOT_READY
//
// MessageText:
//
//  One or more of the properties that are needed to run this task have not been set.
//
  SCHED_E_TASK_NOT_READY = HRESULT($8004130A);
  {$EXTERNALSYM SCHED_E_TASK_NOT_READY}

//
// MessageId: SCHED_E_TASK_NOT_RUNNING
//
// MessageText:
//
//  There is no running instance of the task to terminate.
//
  SCHED_E_TASK_NOT_RUNNING = HRESULT($8004130B);
  {$EXTERNALSYM SCHED_E_TASK_NOT_RUNNING}

//
// MessageId: SCHED_E_SERVICE_NOT_INSTALLED
//
// MessageText:
//
//  The Task Scheduler Service is not installed on this computer.
//
  SCHED_E_SERVICE_NOT_INSTALLED = HRESULT($8004130C);
  {$EXTERNALSYM SCHED_E_SERVICE_NOT_INSTALLED}

//
// MessageId: SCHED_E_CANNOT_OPEN_TASK
//
// MessageText:
//
//  The task object could not be opened.
//
  SCHED_E_CANNOT_OPEN_TASK = HRESULT($8004130D);
  {$EXTERNALSYM SCHED_E_CANNOT_OPEN_TASK}

//
// MessageId: SCHED_E_INVALID_TASK
//
// MessageText:
//
//  The object is either an invalid task object or is not a task object.
//
  SCHED_E_INVALID_TASK = HRESULT($8004130E);
  {$EXTERNALSYM SCHED_E_INVALID_TASK}

//
// MessageId: SCHED_E_ACCOUNT_INFORMATION_NOT_SET
//
// MessageText:
//
//  No account information could be found in the Task Scheduler security database for the task indicated.
//
  SCHED_E_ACCOUNT_INFORMATION_NOT_SET = HRESULT($8004130F);
  {$EXTERNALSYM SCHED_E_ACCOUNT_INFORMATION_NOT_SET}

//
// MessageId: SCHED_E_ACCOUNT_NAME_NOT_FOUND
//
// MessageText:
//
//  Unable to establish existence of the account specified.
//
  SCHED_E_ACCOUNT_NAME_NOT_FOUND = HRESULT($80041310);
  {$EXTERNALSYM SCHED_E_ACCOUNT_NAME_NOT_FOUND}

//
// MessageId: SCHED_E_ACCOUNT_DBASE_CORRUPT
//
// MessageText:
//
//  Corruption was detected in the Task Scheduler security database; the database has been reset.
//
  SCHED_E_ACCOUNT_DBASE_CORRUPT = HRESULT($80041311);
  {$EXTERNALSYM SCHED_E_ACCOUNT_DBASE_CORRUPT}

//
// MessageId: SCHED_E_NO_SECURITY_SERVICES
//
// MessageText:
//
//  Task Scheduler security services are available only on Windows NT.
//
  SCHED_E_NO_SECURITY_SERVICES = HRESULT($80041312);
  {$EXTERNALSYM SCHED_E_NO_SECURITY_SERVICES}

//
// MessageId: SCHED_E_UNKNOWN_OBJECT_VERSION
//
// MessageText:
//
//  The task object version is either unsupported or invalid.
//
  SCHED_E_UNKNOWN_OBJECT_VERSION = HRESULT($80041313);
  {$EXTERNALSYM SCHED_E_UNKNOWN_OBJECT_VERSION}

//
// MessageId: SCHED_E_UNSUPPORTED_ACCOUNT_OPTION
//
// MessageText:
//
//  The task has been configured with an unsupported combination of account settings and run time options.
//
  SCHED_E_UNSUPPORTED_ACCOUNT_OPTION = HRESULT($80041314);
  {$EXTERNALSYM SCHED_E_UNSUPPORTED_ACCOUNT_OPTION}

//
// MessageId: SCHED_E_SERVICE_NOT_RUNNING
//
// MessageText:
//
//  The Task Scheduler Service is not running.
//
  SCHED_E_SERVICE_NOT_RUNNING = HRESULT($80041315);
  {$EXTERNALSYM SCHED_E_SERVICE_NOT_RUNNING}


// line 151

//
// Define the various device type values.  Note that values used by Microsoft
// Corporation are in the range 0-32767, and 32768-65535 are reserved for use
// by customers.
//

type
  DEVICE_TYPE = DWORD;
  {$EXTERNALSYM DEVICE_TYPE}

const
  FILE_DEVICE_BEEP                = $00000001;
  {$EXTERNALSYM FILE_DEVICE_BEEP}
  FILE_DEVICE_CD_ROM              = $00000002;
  {$EXTERNALSYM FILE_DEVICE_CD_ROM}
  FILE_DEVICE_CD_ROM_FILE_SYSTEM  = $00000003;
  {$EXTERNALSYM FILE_DEVICE_CD_ROM_FILE_SYSTEM}
  FILE_DEVICE_CONTROLLER          = $00000004;
  {$EXTERNALSYM FILE_DEVICE_CONTROLLER}
  FILE_DEVICE_DATALINK            = $00000005;
  {$EXTERNALSYM FILE_DEVICE_DATALINK}
  FILE_DEVICE_DFS                 = $00000006;
  {$EXTERNALSYM FILE_DEVICE_DFS}
  FILE_DEVICE_DISK                = $00000007;
  {$EXTERNALSYM FILE_DEVICE_DISK}
  FILE_DEVICE_DISK_FILE_SYSTEM    = $00000008;
  {$EXTERNALSYM FILE_DEVICE_DISK_FILE_SYSTEM}
  FILE_DEVICE_FILE_SYSTEM         = $00000009;
  {$EXTERNALSYM FILE_DEVICE_FILE_SYSTEM}
  FILE_DEVICE_INPORT_PORT         = $0000000a;
  {$EXTERNALSYM FILE_DEVICE_INPORT_PORT}
  FILE_DEVICE_KEYBOARD            = $0000000b;
  {$EXTERNALSYM FILE_DEVICE_KEYBOARD}
  FILE_DEVICE_MAILSLOT            = $0000000c;
  {$EXTERNALSYM FILE_DEVICE_MAILSLOT}
  FILE_DEVICE_MIDI_IN             = $0000000d;
  {$EXTERNALSYM FILE_DEVICE_MIDI_IN}
  FILE_DEVICE_MIDI_OUT            = $0000000e;
  {$EXTERNALSYM FILE_DEVICE_MIDI_OUT}
  FILE_DEVICE_MOUSE               = $0000000f;
  {$EXTERNALSYM FILE_DEVICE_MOUSE}
  FILE_DEVICE_MULTI_UNC_PROVIDER  = $00000010;
  {$EXTERNALSYM FILE_DEVICE_MULTI_UNC_PROVIDER}
  FILE_DEVICE_NAMED_PIPE          = $00000011;
  {$EXTERNALSYM FILE_DEVICE_NAMED_PIPE}
  FILE_DEVICE_NETWORK             = $00000012;
  {$EXTERNALSYM FILE_DEVICE_NETWORK}
  FILE_DEVICE_NETWORK_BROWSER     = $00000013;
  {$EXTERNALSYM FILE_DEVICE_NETWORK_BROWSER}
  FILE_DEVICE_NETWORK_FILE_SYSTEM = $00000014;
  {$EXTERNALSYM FILE_DEVICE_NETWORK_FILE_SYSTEM}
  FILE_DEVICE_NULL                = $00000015;
  {$EXTERNALSYM FILE_DEVICE_NULL}
  FILE_DEVICE_PARALLEL_PORT       = $00000016;
  {$EXTERNALSYM FILE_DEVICE_PARALLEL_PORT}
  FILE_DEVICE_PHYSICAL_NETCARD    = $00000017;
  {$EXTERNALSYM FILE_DEVICE_PHYSICAL_NETCARD}
  FILE_DEVICE_PRINTER             = $00000018;
  {$EXTERNALSYM FILE_DEVICE_PRINTER}
  FILE_DEVICE_SCANNER             = $00000019;
  {$EXTERNALSYM FILE_DEVICE_SCANNER}
  FILE_DEVICE_SERIAL_MOUSE_PORT   = $0000001a;
  {$EXTERNALSYM FILE_DEVICE_SERIAL_MOUSE_PORT}
  FILE_DEVICE_SERIAL_PORT         = $0000001b;
  {$EXTERNALSYM FILE_DEVICE_SERIAL_PORT}
  FILE_DEVICE_SCREEN              = $0000001c;
  {$EXTERNALSYM FILE_DEVICE_SCREEN}
  FILE_DEVICE_SOUND               = $0000001d;
  {$EXTERNALSYM FILE_DEVICE_SOUND}
  FILE_DEVICE_STREAMS             = $0000001e;
  {$EXTERNALSYM FILE_DEVICE_STREAMS}
  FILE_DEVICE_TAPE                = $0000001f;
  {$EXTERNALSYM FILE_DEVICE_TAPE}
  FILE_DEVICE_TAPE_FILE_SYSTEM    = $00000020;
  {$EXTERNALSYM FILE_DEVICE_TAPE_FILE_SYSTEM}
  FILE_DEVICE_TRANSPORT           = $00000021;
  {$EXTERNALSYM FILE_DEVICE_TRANSPORT}
  FILE_DEVICE_UNKNOWN             = $00000022;
  {$EXTERNALSYM FILE_DEVICE_UNKNOWN}
  FILE_DEVICE_VIDEO               = $00000023;
  {$EXTERNALSYM FILE_DEVICE_VIDEO}
  FILE_DEVICE_VIRTUAL_DISK        = $00000024;
  {$EXTERNALSYM FILE_DEVICE_VIRTUAL_DISK}
  FILE_DEVICE_WAVE_IN             = $00000025;
  {$EXTERNALSYM FILE_DEVICE_WAVE_IN}
  FILE_DEVICE_WAVE_OUT            = $00000026;
  {$EXTERNALSYM FILE_DEVICE_WAVE_OUT}
  FILE_DEVICE_8042_PORT           = $00000027;
  {$EXTERNALSYM FILE_DEVICE_8042_PORT}
  FILE_DEVICE_NETWORK_REDIRECTOR  = $00000028;
  {$EXTERNALSYM FILE_DEVICE_NETWORK_REDIRECTOR}
  FILE_DEVICE_BATTERY             = $00000029;
  {$EXTERNALSYM FILE_DEVICE_BATTERY}
  FILE_DEVICE_BUS_EXTENDER        = $0000002a;
  {$EXTERNALSYM FILE_DEVICE_BUS_EXTENDER}
  FILE_DEVICE_MODEM               = $0000002b;
  {$EXTERNALSYM FILE_DEVICE_MODEM}
  FILE_DEVICE_VDM                 = $0000002c;
  {$EXTERNALSYM FILE_DEVICE_VDM}
  FILE_DEVICE_MASS_STORAGE        = $0000002d;
  {$EXTERNALSYM FILE_DEVICE_MASS_STORAGE}
  FILE_DEVICE_SMB                 = $0000002e;
  {$EXTERNALSYM FILE_DEVICE_SMB}
  FILE_DEVICE_KS                  = $0000002f;
  {$EXTERNALSYM FILE_DEVICE_KS}
  FILE_DEVICE_CHANGER             = $00000030;
  {$EXTERNALSYM FILE_DEVICE_CHANGER}
  FILE_DEVICE_SMARTCARD           = $00000031;
  {$EXTERNALSYM FILE_DEVICE_SMARTCARD}
  FILE_DEVICE_ACPI                = $00000032;
  {$EXTERNALSYM FILE_DEVICE_ACPI}
  FILE_DEVICE_DVD                 = $00000033;
  {$EXTERNALSYM FILE_DEVICE_DVD}
  FILE_DEVICE_FULLSCREEN_VIDEO    = $00000034;
  {$EXTERNALSYM FILE_DEVICE_FULLSCREEN_VIDEO}
  FILE_DEVICE_DFS_FILE_SYSTEM     = $00000035;
  {$EXTERNALSYM FILE_DEVICE_DFS_FILE_SYSTEM}
  FILE_DEVICE_DFS_VOLUME          = $00000036;
  {$EXTERNALSYM FILE_DEVICE_DFS_VOLUME}
  FILE_DEVICE_SERENUM             = $00000037;
  {$EXTERNALSYM FILE_DEVICE_SERENUM}
  FILE_DEVICE_TERMSRV             = $00000038;
  {$EXTERNALSYM FILE_DEVICE_TERMSRV}
  FILE_DEVICE_KSEC                = $00000039;
  {$EXTERNALSYM FILE_DEVICE_KSEC}
  FILE_DEVICE_FIPS                = $0000003A;
  {$EXTERNALSYM FILE_DEVICE_FIPS}
  FILE_DEVICE_INFINIBAND          = $0000003B;
  {$EXTERNALSYM FILE_DEVICE_INFINIBAND}

// line 297

//
// Define the method codes for how buffers are passed for I/O and FS controls
//

const
  METHOD_BUFFERED   = 0;
  {$EXTERNALSYM METHOD_BUFFERED}
  METHOD_IN_DIRECT  = 1;
  {$EXTERNALSYM METHOD_IN_DIRECT}
  METHOD_OUT_DIRECT = 2;
  {$EXTERNALSYM METHOD_OUT_DIRECT}
  METHOD_NEITHER    = 3;
  {$EXTERNALSYM METHOD_NEITHER}

//
// Define some easier to comprehend aliases:
//   METHOD_DIRECT_TO_HARDWARE (writes, aka METHOD_IN_DIRECT)
//   METHOD_DIRECT_FROM_HARDWARE (reads, aka METHOD_OUT_DIRECT)
//

  METHOD_DIRECT_TO_HARDWARE     = METHOD_IN_DIRECT;
  {$EXTERNALSYM METHOD_DIRECT_TO_HARDWARE}
  METHOD_DIRECT_FROM_HARDWARE   = METHOD_OUT_DIRECT;
  {$EXTERNALSYM METHOD_DIRECT_FROM_HARDWARE}

//
// Define the access check value for any access
//
//
// The FILE_READ_ACCESS and FILE_WRITE_ACCESS constants are also defined in
// ntioapi.h as FILE_READ_DATA and FILE_WRITE_DATA. The values for these
// constants *MUST* always be in sync.
//
//
// FILE_SPECIAL_ACCESS is checked by the NT I/O system the same as FILE_ANY_ACCESS.
// The file systems, however, may add additional access checks for I/O and FS controls
// that use this value.
//

const
  FILE_ANY_ACCESS     = 0;
  {$EXTERNALSYM FILE_ANY_ACCESS}
  FILE_SPECIAL_ACCESS = FILE_ANY_ACCESS;
  {$EXTERNALSYM FILE_SPECIAL_ACCESS}
  FILE_READ_ACCESS    = $0001;           // file & pipe
  {$EXTERNALSYM FILE_READ_ACCESS}
  FILE_WRITE_ACCESS   = $0002;           // file & pipe
  {$EXTERNALSYM FILE_WRITE_ACCESS}

// line 3425

//
// The following is a list of the native file system fsctls followed by
// additional network file system fsctls.  Some values have been
// decommissioned.
//

const

  FSCTL_REQUEST_OPLOCK_LEVEL_1 = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (0 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_REQUEST_OPLOCK_LEVEL_1}

  FSCTL_REQUEST_OPLOCK_LEVEL_2 = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (1 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_REQUEST_OPLOCK_LEVEL_2}

  FSCTL_REQUEST_BATCH_OPLOCK = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (2 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_REQUEST_BATCH_OPLOCK}

  FSCTL_OPLOCK_BREAK_ACKNOWLEDGE = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (3 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_OPLOCK_BREAK_ACKNOWLEDGE}

  FSCTL_OPBATCH_ACK_CLOSE_PENDING = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (4 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_OPBATCH_ACK_CLOSE_PENDING}

  FSCTL_OPLOCK_BREAK_NOTIFY = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (5 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_OPLOCK_BREAK_NOTIFY}

  FSCTL_LOCK_VOLUME = ((FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or (6 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_LOCK_VOLUME}

  FSCTL_UNLOCK_VOLUME = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (7 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_UNLOCK_VOLUME}

  FSCTL_DISMOUNT_VOLUME = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (8 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_DISMOUNT_VOLUME}

// decommissioned fsctl value                                              9

  FSCTL_IS_VOLUME_MOUNTED = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (10 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_IS_VOLUME_MOUNTED}

  FSCTL_IS_PATHNAME_VALID = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (11 shl 2) or METHOD_BUFFERED);    // PATHNAME_BUFFER,
  {$EXTERNALSYM FSCTL_IS_PATHNAME_VALID}

  FSCTL_MARK_VOLUME_DIRTY = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (12 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_MARK_VOLUME_DIRTY}

// decommissioned fsctl value                                             13

  FSCTL_QUERY_RETRIEVAL_POINTERS = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (14 shl 2) or METHOD_NEITHER);
  {$EXTERNALSYM FSCTL_QUERY_RETRIEVAL_POINTERS}

  FSCTL_GET_COMPRESSION = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (15 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_GET_COMPRESSION}

  FSCTL_SET_COMPRESSION = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or ((FILE_READ_DATA or FILE_WRITE_DATA) shl 14) or
    (16 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_SET_COMPRESSION}

// decommissioned fsctl value                                             17
// decommissioned fsctl value                                             18

  FSCTL_MARK_AS_SYSTEM_HIVE = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (19 shl 2) or METHOD_NEITHER);
  {$EXTERNALSYM FSCTL_MARK_AS_SYSTEM_HIVE}

  FSCTL_OPLOCK_BREAK_ACK_NO_2 = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (20 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_OPLOCK_BREAK_ACK_NO_2}

  FSCTL_INVALIDATE_VOLUMES = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (21 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_INVALIDATE_VOLUMES}

  FSCTL_QUERY_FAT_BPB = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (22 shl 2) or METHOD_BUFFERED); // FSCTL_QUERY_FAT_BPB_BUFFER
  {$EXTERNALSYM FSCTL_QUERY_FAT_BPB}

  FSCTL_REQUEST_FILTER_OPLOCK = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (23 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_REQUEST_FILTER_OPLOCK}

  FSCTL_FILESYSTEM_GET_STATISTICS = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (24 shl 2) or METHOD_BUFFERED); // FILESYSTEM_STATISTICS
  {$EXTERNALSYM FSCTL_FILESYSTEM_GET_STATISTICS}

  FSCTL_GET_NTFS_VOLUME_DATA = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (25 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_GET_NTFS_VOLUME_DATA}

  FSCTL_GET_NTFS_FILE_RECORD = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (26 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_GET_NTFS_FILE_RECORD}

  FSCTL_GET_VOLUME_BITMAP = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (27 shl 2) or METHOD_NEITHER);
  {$EXTERNALSYM FSCTL_GET_VOLUME_BITMAP}

  FSCTL_GET_RETRIEVAL_POINTERS = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (28 shl 2) or METHOD_NEITHER);
  {$EXTERNALSYM FSCTL_GET_RETRIEVAL_POINTERS}

  FSCTL_MOVE_FILE = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_SPECIAL_ACCESS shl 14) or
    (29 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_MOVE_FILE}

  FSCTL_IS_VOLUME_DIRTY = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (30 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_IS_VOLUME_DIRTY}

// decomissioned fsctl value  31
(*  FSCTL_GET_HFS_INFORMATION = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (31 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_GET_HFS_INFORMATION}
*)

  FSCTL_ALLOW_EXTENDED_DASD_IO = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (32 shl 2) or METHOD_NEITHER);
  {$EXTERNALSYM FSCTL_ALLOW_EXTENDED_DASD_IO}

// decommissioned fsctl value                                             33
// decommissioned fsctl value                                             34

(*
  FSCTL_READ_PROPERTY_DATA = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (33 shl 2) or METHOD_NEITHER);
  {$EXTERNALSYM FSCTL_READ_PROPERTY_DATA}

  FSCTL_WRITE_PROPERTY_DATA = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (34 shl 2) or METHOD_NEITHER);
  {$EXTERNALSYM FSCTL_WRITE_PROPERTY_DATA}
*)

  FSCTL_FIND_FILES_BY_SID = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (35 shl 2) or METHOD_NEITHER);  
  {$EXTERNALSYM FSCTL_FIND_FILES_BY_SID}

// decommissioned fsctl value                                             36
// decommissioned fsctl value                                             37

(*  FSCTL_DUMP_PROPERTY_DATA = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (37 shl 2) or METHOD_NEITHER);
  {$EXTERNALSYM FSCTL_DUMP_PROPERTY_DATA}
*)

  FSCTL_SET_OBJECT_ID = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_SPECIAL_ACCESS shl 14) or
    (38 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_SET_OBJECT_ID}

  FSCTL_GET_OBJECT_ID = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (39 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_GET_OBJECT_ID}

  FSCTL_DELETE_OBJECT_ID = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_SPECIAL_ACCESS shl 14) or
    (40 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_DELETE_OBJECT_ID}

  FSCTL_SET_REPARSE_POINT = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_SPECIAL_ACCESS shl 14) or
    (41 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_SET_REPARSE_POINT}

  FSCTL_GET_REPARSE_POINT = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (42 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_GET_REPARSE_POINT}

  FSCTL_DELETE_REPARSE_POINT = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_SPECIAL_ACCESS shl 14) or
    (43 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_DELETE_REPARSE_POINT}

  FSCTL_ENUM_USN_DATA = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (44 shl 2) or METHOD_NEITHER);
  {$EXTERNALSYM FSCTL_ENUM_USN_DATA}

  FSCTL_SECURITY_ID_CHECK = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_READ_DATA shl 14) or
    (45 shl 2) or METHOD_NEITHER);
  {$EXTERNALSYM FSCTL_SECURITY_ID_CHECK}

  FSCTL_READ_USN_JOURNAL = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (46 shl 2) or METHOD_NEITHER);
  {$EXTERNALSYM FSCTL_READ_USN_JOURNAL}

  FSCTL_SET_OBJECT_ID_EXTENDED = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_SPECIAL_ACCESS shl 14) or
    (47 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_SET_OBJECT_ID_EXTENDED}

  FSCTL_CREATE_OR_GET_OBJECT_ID = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (48 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_CREATE_OR_GET_OBJECT_ID}

  FSCTL_SET_SPARSE = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_SPECIAL_ACCESS shl 14) or
    (49 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_SET_SPARSE}

  FSCTL_SET_ZERO_DATA = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_WRITE_DATA shl 14) or
    (50 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_SET_ZERO_DATA}

  FSCTL_QUERY_ALLOCATED_RANGES = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_READ_DATA shl 14) or
    (51 shl 2) or METHOD_NEITHER);
  {$EXTERNALSYM FSCTL_QUERY_ALLOCATED_RANGES}

// decommissioned fsctl value                                             52
(*
  FSCTL_ENABLE_UPGRADE = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_WRITE_DATA shl 14) or
    (52 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_ENABLE_UPGRADE}
*)

  FSCTL_SET_ENCRYPTION = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (53 shl 2) or METHOD_NEITHER);
  {$EXTERNALSYM FSCTL_SET_ENCRYPTION}

  FSCTL_ENCRYPTION_FSCTL_IO = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (54 shl 2) or METHOD_NEITHER);
  {$EXTERNALSYM FSCTL_ENCRYPTION_FSCTL_IO}

  FSCTL_WRITE_RAW_ENCRYPTED = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_SPECIAL_ACCESS shl 14) or
    (55 shl 2) or METHOD_NEITHER);
  {$EXTERNALSYM FSCTL_WRITE_RAW_ENCRYPTED}

  FSCTL_READ_RAW_ENCRYPTED = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_SPECIAL_ACCESS shl 14) or
    (56 shl 2) or METHOD_NEITHER);
  {$EXTERNALSYM FSCTL_READ_RAW_ENCRYPTED}

  FSCTL_CREATE_USN_JOURNAL = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (57 shl 2) or METHOD_NEITHER);
  {$EXTERNALSYM FSCTL_CREATE_USN_JOURNAL}

  FSCTL_READ_FILE_USN_DATA = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (58 shl 2) or METHOD_NEITHER);
  {$EXTERNALSYM FSCTL_READ_FILE_USN_DATA}

  FSCTL_WRITE_USN_CLOSE_RECORD = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (59 shl 2) or METHOD_NEITHER);
  {$EXTERNALSYM FSCTL_WRITE_USN_CLOSE_RECORD}

  FSCTL_EXTEND_VOLUME = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (60 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_EXTEND_VOLUME}

  FSCTL_QUERY_USN_JOURNAL = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (61 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_QUERY_USN_JOURNAL}

  FSCTL_DELETE_USN_JOURNAL = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (62 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_DELETE_USN_JOURNAL}

  FSCTL_MARK_HANDLE = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (63 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_MARK_HANDLE}

  FSCTL_SIS_COPYFILE = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (64 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_SIS_COPYFILE}

  FSCTL_SIS_LINK_FILES = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or ((FILE_READ_DATA or FILE_WRITE_DATA) shl 14) or
    (65 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_SIS_LINK_FILES}

  FSCTL_HSM_MSG = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or ((FILE_READ_DATA or FILE_WRITE_DATA) shl 14) or
    (66 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_HSM_MSG}

// decommissioned fsctl value                                             67
(*
  FSCTL_NSS_CONTROL = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_WRITE_DATA shl 14) or
    (67 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_NSS_CONTROL}
*)

  FSCTL_HSM_DATA = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or ((FILE_READ_DATA or FILE_WRITE_DATA) shl 14) or
    (68 shl 2) or METHOD_NEITHER);
  {$EXTERNALSYM FSCTL_HSM_DATA}

  FSCTL_RECALL_FILE = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (69 shl 2) or METHOD_NEITHER);
  {$EXTERNALSYM FSCTL_RECALL_FILE}

// decommissioned fsctl value                                             70
(*
  FSCTL_NSS_RCONTROL = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_READ_DATA shl 14) or
    (70 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_NSS_RCONTROL}
*)

  FSCTL_READ_FROM_PLEX = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_READ_DATA shl 14) or
    (71 shl 2) or METHOD_OUT_DIRECT);
  {$EXTERNALSYM FSCTL_READ_FROM_PLEX}

  FSCTL_FILE_PREFETCH = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_SPECIAL_ACCESS shl 14) or
    (72 shl 2) or METHOD_BUFFERED);
  {$EXTERNALSYM FSCTL_FILE_PREFETCH}

// line 4553

//
// Structure for FSCTL_SET_ZERO_DATA
//

type

  PFILE_ZERO_DATA_INFORMATION = ^FILE_ZERO_DATA_INFORMATION;
  {$EXTERNALSYM PFILE_ZERO_DATA_INFORMATION}
  _FILE_ZERO_DATA_INFORMATION = record
    FileOffset: LARGE_INTEGER;
    BeyondFinalZero: LARGE_INTEGER;
  end;
  {$EXTERNALSYM _FILE_ZERO_DATA_INFORMATION}
  FILE_ZERO_DATA_INFORMATION = _FILE_ZERO_DATA_INFORMATION;
  {$EXTERNALSYM FILE_ZERO_DATA_INFORMATION}
  TFileZeroDataInformation = FILE_ZERO_DATA_INFORMATION;
  PFileZeroDataInformation = PFILE_ZERO_DATA_INFORMATION;

//
// Structure for FSCTL_QUERY_ALLOCATED_RANGES
//

//
// Querying the allocated ranges requires an output buffer to store the
// allocated ranges and an input buffer to specify the range to query.
// The input buffer contains a single entry, the output buffer is an
// array of the following structure.
//

  PFILE_ALLOCATED_RANGE_BUFFER = ^FILE_ALLOCATED_RANGE_BUFFER;
  {$EXTERNALSYM PFILE_ALLOCATED_RANGE_BUFFER}
  _FILE_ALLOCATED_RANGE_BUFFER = record
    FileOffset: LARGE_INTEGER;
    Length: LARGE_INTEGER;
  end;
  {$EXTERNALSYM _FILE_ALLOCATED_RANGE_BUFFER}
  FILE_ALLOCATED_RANGE_BUFFER = _FILE_ALLOCATED_RANGE_BUFFER;
  {$EXTERNALSYM FILE_ALLOCATED_RANGE_BUFFER}
  TFileAllocatedRangeBuffer = FILE_ALLOCATED_RANGE_BUFFER;
  PFileAllocatedRangeBuffer = PFILE_ALLOCATED_RANGE_BUFFER;


// line 340

//
//  Code Page Default Values.
//

const
  CP_ACP        = 0; // default to ANSI code page
  {$EXTERNALSYM CP_ACP}
  CP_OEMCP      = 1; // default to OEM  code page
  {$EXTERNALSYM CP_OEMCP}
  CP_MACCP      = 2; // default to MAC  code page
  {$EXTERNALSYM CP_MACCP}
  CP_THREAD_ACP = 3; // current thread's ANSI code page
  {$EXTERNALSYM CP_THREAD_ACP}
  CP_SYMBOL     = 42; // SYMBOL translations
  {$EXTERNALSYM CP_SYMBOL}

  CP_UTF7 = 65000; // UTF-7 translation
  {$EXTERNALSYM CP_UTF7}
  CP_UTF8 = 65001; // UTF-8 translation
  {$EXTERNALSYM CP_UTF8}

// line 597

const

//
//  The following LCTypes may be used in combination with any other LCTypes.
//
//    LOCALE_NOUSEROVERRIDE is also used in GetTimeFormat and
//    GetDateFormat.
//
//    LOCALE_USE_CP_ACP is used in many of the A (Ansi) apis that need
//    to do string translation.
//
//    LOCALE_RETURN_NUMBER will return the result from GetLocaleInfo as a
//    number instead of a string.  This flag is only valid for the LCTypes
//    beginning with LOCALE_I.
//

  LOCALE_NOUSEROVERRIDE = DWORD($80000000); // do not use user overrides
  {$EXTERNALSYM LOCALE_NOUSEROVERRIDE}
  LOCALE_USE_CP_ACP     = $40000000; // use the system ACP
  {$EXTERNALSYM LOCALE_USE_CP_ACP}

  LOCALE_RETURN_NUMBER = $20000000; // return number instead of string
  {$EXTERNALSYM LOCALE_RETURN_NUMBER}

// line 841

const
  LOCALE_IDEFAULTEBCDICCODEPAGE = $00001012; // default ebcdic code page
  {$EXTERNALSYM LOCALE_IDEFAULTEBCDICCODEPAGE}
  LOCALE_IPAPERSIZE             = $0000100A; // 1 = letter, 5 = legal, 8 = a3, 9 = a4
  {$EXTERNALSYM LOCALE_IPAPERSIZE}
  LOCALE_SENGCURRNAME           = $00001007; // english name of currency
  {$EXTERNALSYM LOCALE_SENGCURRNAME}
  LOCALE_SNATIVECURRNAME        = $00001008; // native name of currency
  {$EXTERNALSYM LOCALE_SNATIVECURRNAME}
  LOCALE_SYEARMONTH             = $00001006; // year month format string
  {$EXTERNALSYM LOCALE_SYEARMONTH}
  LOCALE_SSORTNAME              = $00001013; // sort name
  {$EXTERNALSYM LOCALE_SSORTNAME}
  LOCALE_IDIGITSUBSTITUTION     = $00001014; // 0 = context, 1 = none, 2 = national
  {$EXTERNALSYM LOCALE_IDIGITSUBSTITUTION}

// line 880

  DATE_YEARMONTH  = $00000008; // use year month picture
  {$EXTERNALSYM DATE_YEARMONTH}
  DATE_LTRREADING = $00000010; // add marks for left to right reading order layout
  {$EXTERNALSYM DATE_LTRREADING}
  DATE_RTLREADING = $00000020; // add marks for right to left reading order layout
  {$EXTERNALSYM DATE_RTLREADING}

//
//  Calendar Types.
//
//  These types are used for the EnumCalendarInfo and GetCalendarInfo
//  NLS API routines.
//  Some of these types are also used for the SetCalendarInfo NLS API
//  routine.
//

//
//  The following CalTypes may be used in combination with any other CalTypes.
//
//    CAL_NOUSEROVERRIDE
//
//    CAL_USE_CP_ACP is used in the A (Ansi) apis that need to do string
//    translation.
//
//    CAL_RETURN_NUMBER will return the result from GetCalendarInfo as a
//    number instead of a string.  This flag is only valid for the CalTypes
//    beginning with CAL_I.
//

  CAL_NOUSEROVERRIDE = LOCALE_NOUSEROVERRIDE; // do not use user overrides
  {$EXTERNALSYM CAL_NOUSEROVERRIDE}
  CAL_USE_CP_ACP     = LOCALE_USE_CP_ACP; // use the system ACP
  {$EXTERNALSYM CAL_USE_CP_ACP}
  CAL_RETURN_NUMBER  = LOCALE_RETURN_NUMBER; // return number instead of string
  {$EXTERNALSYM CAL_RETURN_NUMBER}

// line 1014

  CAL_SYEARMONTH       = $0000002f; // year month format string
  {$EXTERNALSYM CAL_SYEARMONTH}
  CAL_ITWODIGITYEARMAX = $00000030; // two digit year max
  {$EXTERNALSYM CAL_ITWODIGITYEARMAX}

// line 1424

type
  CALINFO_ENUMPROCEXA = function (lpCalendarInfoString: LPSTR; Calendar: CALID): BOOL; stdcall;
  {$EXTERNALSYM CALINFO_ENUMPROCEXA}
  TCalInfoEnumProcExA = CALINFO_ENUMPROCEXA;

// line 1635


{$IFNDEF CLR}

function GetCalendarInfoA(Locale: LCID; Calendar: CALID; CalType: CALTYPE;
  lpCalData: LPSTR; cchData: Integer; lpValue: LPDWORD): Integer; stdcall;
{$EXTERNALSYM GetCalendarInfoA}
function GetCalendarInfoW(Locale: LCID; Calendar: CALID; CalType: CALTYPE;
  lpCalData: LPWSTR; cchData: Integer; lpValue: LPDWORD): Integer; stdcall;
{$EXTERNALSYM GetCalendarInfoW}

// line 1754

function EnumCalendarInfoExA(lpCalInfoEnumProcEx: CALINFO_ENUMPROCEXA;
  Locale: LCID; Calendar: CALID; CalType: CALTYPE): BOOL; stdcall;
{$EXTERNALSYM EnumCalendarInfoExA}

{$ENDIF ~CLR}


type
  {$IFDEF CLR}
  MAKEINTRESOURCEA = Integer;
  MAKEINTRESOURCEW = Integer;
  {$ELSE}
  MAKEINTRESOURCEA = LPSTR;
  {$EXTERNALSYM MAKEINTRESOURCEA}
  MAKEINTRESOURCEW = LPWSTR;
  {$EXTERNALSYM MAKEINTRESOURCEW}
  {$ENDIF CLR}
{$IFDEF UNICODE}
  MAKEINTRESOURCE = MAKEINTRESOURCEW;
  {$EXTERNALSYM MAKEINTRESOURCE}
{$ELSE}
  MAKEINTRESOURCE = MAKEINTRESOURCEA;
  {$EXTERNALSYM MAKEINTRESOURCE}
{$ENDIF}

//
// Predefined Resource Types
//

const
  RT_CURSOR       = MAKEINTRESOURCE(1);
  {$EXTERNALSYM RT_CURSOR}
  RT_BITMAP       = MAKEINTRESOURCE(2);
  {$EXTERNALSYM RT_BITMAP}
  RT_ICON         = MAKEINTRESOURCE(3);
  {$EXTERNALSYM RT_ICON}
  RT_MENU         = MAKEINTRESOURCE(4);
  {$EXTERNALSYM RT_MENU}
  RT_DIALOG       = MAKEINTRESOURCE(5);
  {$EXTERNALSYM RT_DIALOG}
  RT_STRING       = MAKEINTRESOURCE(6);
  {$EXTERNALSYM RT_STRING}
  RT_FONTDIR      = MAKEINTRESOURCE(7);
  {$EXTERNALSYM RT_FONTDIR}
  RT_FONT         = MAKEINTRESOURCE(8);
  {$EXTERNALSYM RT_FONT}
  RT_ACCELERATOR  = MAKEINTRESOURCE(9);
  {$EXTERNALSYM RT_ACCELERATOR}
  RT_RCDATA       = MAKEINTRESOURCE(10);
  {$EXTERNALSYM RT_RCDATA}
  RT_MESSAGETABLE = MAKEINTRESOURCE(11);
  {$EXTERNALSYM RT_MESSAGETABLE}

  DIFFERENCE = 11;
  {$EXTERNALSYM DIFFERENCE}

  RT_GROUP_CURSOR = MAKEINTRESOURCE(ULONG_PTR(RT_CURSOR) + DIFFERENCE);
  {$EXTERNALSYM RT_GROUP_CURSOR}
  RT_GROUP_ICON = MAKEINTRESOURCE(ULONG_PTR(RT_ICON) + DIFFERENCE);
  {$EXTERNALSYM RT_GROUP_ICON}
  RT_VERSION    = MAKEINTRESOURCE(16);
  {$EXTERNALSYM RT_VERSION}
  RT_DLGINCLUDE = MAKEINTRESOURCE(17);
  {$EXTERNALSYM RT_DLGINCLUDE}
  RT_PLUGPLAY   = MAKEINTRESOURCE(19);
  {$EXTERNALSYM RT_PLUGPLAY}
  RT_VXD        = MAKEINTRESOURCE(20);
  {$EXTERNALSYM RT_VXD}
  RT_ANICURSOR  = MAKEINTRESOURCE(21);
  {$EXTERNALSYM RT_ANICURSOR}
  RT_ANIICON    = MAKEINTRESOURCE(22);
  {$EXTERNALSYM RT_ANIICON}
  RT_HTML       = MAKEINTRESOURCE(23);
  {$EXTERNALSYM RT_HTML}
  RT_MANIFEST   = MAKEINTRESOURCE(24);
  CREATEPROCESS_MANIFEST_RESOURCE_ID = MAKEINTRESOURCE(1);
  {$EXTERNALSYM CREATEPROCESS_MANIFEST_RESOURCE_ID}
  ISOLATIONAWARE_MANIFEST_RESOURCE_ID = MAKEINTRESOURCE(2);
  {$EXTERNALSYM ISOLATIONAWARE_MANIFEST_RESOURCE_ID}
  ISOLATIONAWARE_NOSTATICIMPORT_MANIFEST_RESOURCE_ID = MAKEINTRESOURCE(3);
  {$EXTERNALSYM ISOLATIONAWARE_NOSTATICIMPORT_MANIFEST_RESOURCE_ID}
  MINIMUM_RESERVED_MANIFEST_RESOURCE_ID = MAKEINTRESOURCE(1{inclusive});
  {$EXTERNALSYM MINIMUM_RESERVED_MANIFEST_RESOURCE_ID}
  MAXIMUM_RESERVED_MANIFEST_RESOURCE_ID = MAKEINTRESOURCE(16{inclusive});
  {$EXTERNALSYM MAXIMUM_RESERVED_MANIFEST_RESOURCE_ID}

// line 1451  

  KLF_SETFORPROCESS = $00000100;
  {$EXTERNALSYM KLF_SETFORPROCESS}
  KLF_SHIFTLOCK     = $00010000;
  {$EXTERNALSYM KLF_SHIFTLOCK}
  KLF_RESET         = $40000000;
  {$EXTERNALSYM KLF_RESET}


{$IFNDEF CLR}

function IsPwrSuspendAllowed: BOOL; stdcall;
function IsPwrHibernateAllowed: BOOL; stdcall;
function IsPwrShutdownAllowed: BOOL; stdcall;
function SetSuspendState(Hibernate, ForceCritical, DisableWakeEvent: BOOL): BOOL; stdcall;

{$ENDIF ~CLR}
{$IFNDEF CLR}

type
  // Microsoft version (64 bit SDK)
  {$EXTERNALSYM RVA}
  RVA = DWORD;

  // 64-bit PE
  {$EXTERNALSYM ImgDelayDescrV2}
  ImgDelayDescrV2 = packed record
    grAttrs: DWORD;      // attributes
    rvaDLLName: RVA;     // RVA to dll name
    rvaHmod: RVA;        // RVA of module handle
    rvaIAT: RVA;         // RVA of the IAT
    rvaINT: RVA;         // RVA of the INT
    rvaBoundIAT: RVA;    // RVA of the optional bound IAT
    rvaUnloadIAT: RVA;   // RVA of optional copy of original IAT
    dwTimeStamp: DWORD;  // 0 if not bound,
                         // O.W. date/time stamp of DLL bound to (Old BIND)
  end;
  {$EXTERNALSYM TImgDelayDescrV2}
  TImgDelayDescrV2 = ImgDelayDescrV2;
  {$EXTERNALSYM PImgDelayDescrV2}
  PImgDelayDescrV2 = ^ImgDelayDescrV2;

  {$EXTERNALSYM PHMODULE}
  PHMODULE = ^HMODULE;

  // 32-bit PE
  {$EXTERNALSYM ImgDelayDescrV1}
  ImgDelayDescrV1 = packed record
    grAttrs: DWORD;                // attributes
    szName: LPCSTR;                // pointer to dll name
    phmod: PHMODULE;               // address of module handle
    pIAT: PImageThunkData32;       // address of the IAT
    pINT: PImageThunkData32;       // address of the INT
    pBoundIAT: PImageThunkData32;  // address of the optional bound IAT
    pUnloadIAT: PImageThunkData32; // address of optional copy of original IAT
    dwTimeStamp: DWORD;            // 0 if not bound,
                                   // O.W. date/time stamp of DLL bound to (Old BIND)
  end;
  {$EXTERNALSYM TImgDelayDescrV1}
  TImgDelayDescrV1 = ImgDelayDescrV1;
  {$EXTERNALSYM PImgDelayDescrV1}
  PImgDelayDescrV1 = ^ImgDelayDescrV1;

  //{$EXTERNALSYM PImgDelayDescr}
  //PImgDelayDescr = ImgDelayDescr;
  //TImgDelayDescr = ImgDelayDescr;

{$ENDIF ~CLR}
// propidl.h line 386

// Reserved global Property IDs
const
  PID_DICTIONARY         = $00000000; // integer count + array of entries
  {$EXTERNALSYM PID_DICTIONARY}
  PID_CODEPAGE           = $00000001; // short integer
  {$EXTERNALSYM PID_CODEPAGE}
  PID_FIRST_USABLE       = $00000002;
  {$EXTERNALSYM PID_FIRST_USABLE}
  PID_FIRST_NAME_DEFAULT = $00000FFF;
  {$EXTERNALSYM PID_FIRST_NAME_DEFAULT}
  PID_LOCALE             = $80000000;
  {$EXTERNALSYM PID_LOCALE}
  PID_MODIFY_TIME        = $80000001;
  {$EXTERNALSYM PID_MODIFY_TIME}
  PID_SECURITY           = $80000002;
  {$EXTERNALSYM PID_SECURITY}
  PID_BEHAVIOR           = $80000003;
  {$EXTERNALSYM PID_BEHAVIOR}
  PID_ILLEGAL            = $FFFFFFFF;
  {$EXTERNALSYM PID_ILLEGAL}

// Range which is read-only to downlevel implementations

const
  PID_MIN_READONLY = $80000000;
  {$EXTERNALSYM PID_MIN_READONLY}
  PID_MAX_READONLY = $BFFFFFFF;
  {$EXTERNALSYM PID_MAX_READONLY}

// Property IDs for the DiscardableInformation Property Set

const
  PIDDI_THUMBNAIL = $00000002; // VT_BLOB
  {$EXTERNALSYM PIDDI_THUMBNAIL}

// Property IDs for the SummaryInformation Property Set

const
  PIDSI_TITLE        = $00000002; // VT_LPSTR
  {$EXTERNALSYM PIDSI_TITLE}
  PIDSI_SUBJECT      = $00000003; // VT_LPSTR
  {$EXTERNALSYM PIDSI_SUBJECT}
  PIDSI_AUTHOR       = $00000004; // VT_LPSTR
  {$EXTERNALSYM PIDSI_AUTHOR}
  PIDSI_KEYWORDS     = $00000005; // VT_LPSTR
  {$EXTERNALSYM PIDSI_KEYWORDS}
  PIDSI_COMMENTS     = $00000006; // VT_LPSTR
  {$EXTERNALSYM PIDSI_COMMENTS}
  PIDSI_TEMPLATE     = $00000007; // VT_LPSTR
  {$EXTERNALSYM PIDSI_TEMPLATE}
  PIDSI_LASTAUTHOR   = $00000008; // VT_LPSTR
  {$EXTERNALSYM PIDSI_LASTAUTHOR}
  PIDSI_REVNUMBER    = $00000009; // VT_LPSTR
  {$EXTERNALSYM PIDSI_REVNUMBER}
  PIDSI_EDITTIME     = $0000000A; // VT_FILETIME (UTC)
  {$EXTERNALSYM PIDSI_EDITTIME}
  PIDSI_LASTPRINTED  = $0000000B; // VT_FILETIME (UTC)
  {$EXTERNALSYM PIDSI_LASTPRINTED}
  PIDSI_CREATE_DTM   = $0000000C; // VT_FILETIME (UTC)
  {$EXTERNALSYM PIDSI_CREATE_DTM}
  PIDSI_LASTSAVE_DTM = $0000000D; // VT_FILETIME (UTC)
  {$EXTERNALSYM PIDSI_LASTSAVE_DTM}
  PIDSI_PAGECOUNT    = $0000000E; // VT_I4
  {$EXTERNALSYM PIDSI_PAGECOUNT}
  PIDSI_WORDCOUNT    = $0000000F; // VT_I4
  {$EXTERNALSYM PIDSI_WORDCOUNT}
  PIDSI_CHARCOUNT    = $00000010; // VT_I4
  {$EXTERNALSYM PIDSI_CHARCOUNT}
  PIDSI_THUMBNAIL    = $00000011; // VT_CF
  {$EXTERNALSYM PIDSI_THUMBNAIL}
  PIDSI_APPNAME      = $00000012; // VT_LPSTR
  {$EXTERNALSYM PIDSI_APPNAME}
  PIDSI_DOC_SECURITY = $00000013; // VT_I4
  {$EXTERNALSYM PIDSI_DOC_SECURITY}

// Property IDs for the DocSummaryInformation Property Set

const
  PIDDSI_CATEGORY    = $00000002; // VT_LPSTR
  {$EXTERNALSYM PIDDSI_CATEGORY}
  PIDDSI_PRESFORMAT  = $00000003; // VT_LPSTR
  {$EXTERNALSYM PIDDSI_PRESFORMAT}
  PIDDSI_BYTECOUNT   = $00000004; // VT_I4
  {$EXTERNALSYM PIDDSI_BYTECOUNT}
  PIDDSI_LINECOUNT   = $00000005; // VT_I4
  {$EXTERNALSYM PIDDSI_LINECOUNT}
  PIDDSI_PARCOUNT    = $00000006; // VT_I4
  {$EXTERNALSYM PIDDSI_PARCOUNT}
  PIDDSI_SLIDECOUNT  = $00000007; // VT_I4
  {$EXTERNALSYM PIDDSI_SLIDECOUNT}
  PIDDSI_NOTECOUNT   = $00000008; // VT_I4
  {$EXTERNALSYM PIDDSI_NOTECOUNT}
  PIDDSI_HIDDENCOUNT = $00000009; // VT_I4
  {$EXTERNALSYM PIDDSI_HIDDENCOUNT}
  PIDDSI_MMCLIPCOUNT = $0000000A; // VT_I4
  {$EXTERNALSYM PIDDSI_MMCLIPCOUNT}
  PIDDSI_SCALE       = $0000000B; // VT_BOOL
  {$EXTERNALSYM PIDDSI_SCALE}
  PIDDSI_HEADINGPAIR = $0000000C; // VT_VARIANT | VT_VECTOR
  {$EXTERNALSYM PIDDSI_HEADINGPAIR}
  PIDDSI_DOCPARTS    = $0000000D; // VT_LPSTR | VT_VECTOR
  {$EXTERNALSYM PIDDSI_DOCPARTS}
  PIDDSI_MANAGER     = $0000000E; // VT_LPSTR
  {$EXTERNALSYM PIDDSI_MANAGER}
  PIDDSI_COMPANY     = $0000000F; // VT_LPSTR
  {$EXTERNALSYM PIDDSI_COMPANY}
  PIDDSI_LINKSDIRTY  = $00000010; // VT_BOOL
  {$EXTERNALSYM PIDDSI_LINKSDIRTY}

//  FMTID_MediaFileSummaryInfo - Property IDs

const
  PIDMSI_EDITOR      = $00000002; // VT_LPWSTR
  {$EXTERNALSYM PIDMSI_EDITOR}
  PIDMSI_SUPPLIER    = $00000003; // VT_LPWSTR
  {$EXTERNALSYM PIDMSI_SUPPLIER}
  PIDMSI_SOURCE      = $00000004; // VT_LPWSTR
  {$EXTERNALSYM PIDMSI_SOURCE}
  PIDMSI_SEQUENCE_NO = $00000005; // VT_LPWSTR
  {$EXTERNALSYM PIDMSI_SEQUENCE_NO}
  PIDMSI_PROJECT     = $00000006; // VT_LPWSTR
  {$EXTERNALSYM PIDMSI_PROJECT}
  PIDMSI_STATUS      = $00000007; // VT_UI4
  {$EXTERNALSYM PIDMSI_STATUS}
  PIDMSI_OWNER       = $00000008; // VT_LPWSTR
  {$EXTERNALSYM PIDMSI_OWNER}
  PIDMSI_RATING      = $00000009; // VT_LPWSTR
  {$EXTERNALSYM PIDMSI_RATING}
  PIDMSI_PRODUCTION  = $0000000A; // VT_FILETIME (UTC)
  {$EXTERNALSYM PIDMSI_PRODUCTION}
  PIDMSI_COPYRIGHT   = $0000000B; // VT_LPWSTR
  {$EXTERNALSYM PIDMSI_COPYRIGHT}


// msidefs.h line 349

// PIDs given specific meanings for Installer

const
  PID_MSIVERSION  = $0000000E; // integer, Installer version number (major*100+minor)
  {$EXTERNALSYM PID_MSIVERSION}
  PID_MSISOURCE   = $0000000F; // integer, type of file image, short/long, media/tree
  {$EXTERNALSYM PID_MSISOURCE}
  PID_MSIRESTRICT = $00000010; // integer, transform restrictions
  {$EXTERNALSYM PID_MSIRESTRICT}


// shlguid.h line 404

const
  FMTID_ShellDetails: TGUID = '{28636aa6-953d-11d2-b5d6-00c04fd918d0}';
  {$EXTERNALSYM FMTID_ShellDetails}

  PID_FINDDATA        = 0;
  {$EXTERNALSYM PID_FINDDATA}
  PID_NETRESOURCE     = 1;
  {$EXTERNALSYM PID_NETRESOURCE}
  PID_DESCRIPTIONID   = 2;
  {$EXTERNALSYM PID_DESCRIPTIONID}
  PID_WHICHFOLDER     = 3;
  {$EXTERNALSYM PID_WHICHFOLDER}
  PID_NETWORKLOCATION = 4;
  {$EXTERNALSYM PID_NETWORKLOCATION}
  PID_COMPUTERNAME    = 5;
  {$EXTERNALSYM PID_COMPUTERNAME}

// PSGUID_STORAGE comes from ntquery.h
const
  FMTID_Storage: TGUID = '{b725f130-47ef-101a-a5f1-02608c9eebac}';
  {$EXTERNALSYM FMTID_Storage}

// Image properties
const
  FMTID_ImageProperties: TGUID = '{14b81da1-0135-4d31-96d9-6cbfc9671a99}';
  {$EXTERNALSYM FMTID_ImageProperties}

// The GUIDs used to identify shell item attributes (columns). See IShellFolder2::GetDetailsEx implementations...

const
  FMTID_Displaced: TGUID = '{9B174B33-40FF-11d2-A27E-00C04FC30871}';
  {$EXTERNALSYM FMTID_Displaced}
  PID_DISPLACED_FROM = 2;
  {$EXTERNALSYM PID_DISPLACED_FROM}
  PID_DISPLACED_DATE = 3;
  {$EXTERNALSYM PID_DISPLACED_DATE}

const
  FMTID_Briefcase: TGUID = '{328D8B21-7729-4bfc-954C-902B329D56B0}';
  {$EXTERNALSYM FMTID_Briefcase}
  PID_SYNC_COPY_IN = 2;
  {$EXTERNALSYM PID_SYNC_COPY_IN}

const
  FMTID_Misc: TGUID = '{9B174B34-40FF-11d2-A27E-00C04FC30871}';
  {$EXTERNALSYM FMTID_Misc}
  PID_MISC_STATUS      = 2;
  {$EXTERNALSYM PID_MISC_STATUS}
  PID_MISC_ACCESSCOUNT = 3;
  {$EXTERNALSYM PID_MISC_ACCESSCOUNT}
  PID_MISC_OWNER       = 4;
  {$EXTERNALSYM PID_MISC_OWNER}
  PID_HTMLINFOTIPFILE  = 5;
  {$EXTERNALSYM PID_HTMLINFOTIPFILE}
  PID_MISC_PICS        = 6;
  {$EXTERNALSYM PID_MISC_PICS}

const
  FMTID_WebView: TGUID = '{F2275480-F782-4291-BD94-F13693513AEC}';
  {$EXTERNALSYM FMTID_WebView}
  PID_DISPLAY_PROPERTIES = 0;
  {$EXTERNALSYM PID_DISPLAY_PROPERTIES}
  PID_INTROTEXT          = 1;
  {$EXTERNALSYM PID_INTROTEXT}

const
  FMTID_MUSIC: TGUID = '{56A3372E-CE9C-11d2-9F0E-006097C686F6}';
  {$EXTERNALSYM FMTID_MUSIC}
  PIDSI_ARTIST    = 2;
  {$EXTERNALSYM PIDSI_ARTIST}
  PIDSI_SONGTITLE = 3;
  {$EXTERNALSYM PIDSI_SONGTITLE}
  PIDSI_ALBUM     = 4;
  {$EXTERNALSYM PIDSI_ALBUM}
  PIDSI_YEAR      = 5;
  {$EXTERNALSYM PIDSI_YEAR}
  PIDSI_COMMENT   = 6;
  {$EXTERNALSYM PIDSI_COMMENT}
  PIDSI_TRACK     = 7;
  {$EXTERNALSYM PIDSI_TRACK}
  PIDSI_GENRE     = 11;
  {$EXTERNALSYM PIDSI_GENRE}
  PIDSI_LYRICS    = 12;
  {$EXTERNALSYM PIDSI_LYRICS}

const
  FMTID_DRM: TGUID = '{AEAC19E4-89AE-4508-B9B7-BB867ABEE2ED}';
  {$EXTERNALSYM FMTID_DRM}
  PIDDRSI_PROTECTED   = 2;
  {$EXTERNALSYM PIDDRSI_PROTECTED}
  PIDDRSI_DESCRIPTION = 3;
  {$EXTERNALSYM PIDDRSI_DESCRIPTION}
  PIDDRSI_PLAYCOUNT   = 4;
  {$EXTERNALSYM PIDDRSI_PLAYCOUNT}
  PIDDRSI_PLAYSTARTS  = 5;
  {$EXTERNALSYM PIDDRSI_PLAYSTARTS}
  PIDDRSI_PLAYEXPIRES = 6;
  {$EXTERNALSYM PIDDRSI_PLAYEXPIRES}

//  FMTID_VideoSummaryInformation property identifiers
const
  FMTID_Video: TGUID = '{64440491-4c8b-11d1-8b70-080036b11a03}';
  {$EXTERNALSYM FMTID_Video}
  PIDVSI_STREAM_NAME   = $00000002; // "StreamName", VT_LPWSTR
  {$EXTERNALSYM PIDVSI_STREAM_NAME}
  PIDVSI_FRAME_WIDTH   = $00000003; // "FrameWidth", VT_UI4
  {$EXTERNALSYM PIDVSI_FRAME_WIDTH}
  PIDVSI_FRAME_HEIGHT  = $00000004; // "FrameHeight", VT_UI4
  {$EXTERNALSYM PIDVSI_FRAME_HEIGHT}
  PIDVSI_TIMELENGTH    = $00000007; // "TimeLength", VT_UI4, milliseconds
  {$EXTERNALSYM PIDVSI_TIMELENGTH}
  PIDVSI_FRAME_COUNT   = $00000005; // "FrameCount". VT_UI4
  {$EXTERNALSYM PIDVSI_FRAME_COUNT}
  PIDVSI_FRAME_RATE    = $00000006; // "FrameRate", VT_UI4, frames/millisecond
  {$EXTERNALSYM PIDVSI_FRAME_RATE}
  PIDVSI_DATA_RATE     = $00000008; // "DataRate", VT_UI4, bytes/second
  {$EXTERNALSYM PIDVSI_DATA_RATE}
  PIDVSI_SAMPLE_SIZE   = $00000009; // "SampleSize", VT_UI4
  {$EXTERNALSYM PIDVSI_SAMPLE_SIZE}
  PIDVSI_COMPRESSION   = $0000000A; // "Compression", VT_LPWSTR
  {$EXTERNALSYM PIDVSI_COMPRESSION}
  PIDVSI_STREAM_NUMBER = $0000000B; // "StreamNumber", VT_UI2
  {$EXTERNALSYM PIDVSI_STREAM_NUMBER}

//  FMTID_AudioSummaryInformation property identifiers
const
  FMTID_Audio: TGUID = '{64440490-4c8b-11d1-8b70-080036b11a03}';
  {$EXTERNALSYM FMTID_Audio}
  PIDASI_FORMAT        = $00000002; // VT_BSTR
  {$EXTERNALSYM PIDASI_FORMAT}
  PIDASI_TIMELENGTH    = $00000003; // VT_UI4, milliseconds
  {$EXTERNALSYM PIDASI_TIMELENGTH}
  PIDASI_AVG_DATA_RATE = $00000004; // VT_UI4,  Hz
  {$EXTERNALSYM PIDASI_AVG_DATA_RATE}
  PIDASI_SAMPLE_RATE   = $00000005; // VT_UI4,  bits
  {$EXTERNALSYM PIDASI_SAMPLE_RATE}
  PIDASI_SAMPLE_SIZE   = $00000006; // VT_UI4,  bits
  {$EXTERNALSYM PIDASI_SAMPLE_SIZE}
  PIDASI_CHANNEL_COUNT = $00000007; // VT_UI4
  {$EXTERNALSYM PIDASI_CHANNEL_COUNT}
  PIDASI_STREAM_NUMBER = $00000008; // VT_UI2
  {$EXTERNALSYM PIDASI_STREAM_NUMBER}
  PIDASI_STREAM_NAME   = $00000009; // VT_LPWSTR
  {$EXTERNALSYM PIDASI_STREAM_NAME}
  PIDASI_COMPRESSION   = $0000000A; // VT_LPWSTR
  {$EXTERNALSYM PIDASI_COMPRESSION}

const
  FMTID_ControlPanel: TGUID = '{305CA226-D286-468e-B848-2B2E8E697B74}';
  {$EXTERNALSYM FMTID_ControlPanel}
  PID_CONTROLPANEL_CATEGORY = 2;
  {$EXTERNALSYM PID_CONTROLPANEL_CATEGORY}

const
  FMTID_Volume: TGUID = '{9B174B35-40FF-11d2-A27E-00C04FC30871}';
  {$EXTERNALSYM FMTID_Volume}
  PID_VOLUME_FREE       = 2;
  {$EXTERNALSYM PID_VOLUME_FREE}
  PID_VOLUME_CAPACITY   = 3;
  {$EXTERNALSYM PID_VOLUME_CAPACITY}
  PID_VOLUME_FILESYSTEM = 4;
  {$EXTERNALSYM PID_VOLUME_FILESYSTEM}

const
  FMTID_Share: TGUID = '{D8C3986F-813B-449c-845D-87B95D674ADE}';
  {$EXTERNALSYM FMTID_Share}
  PID_SHARE_CSC_STATUS = 2;
  {$EXTERNALSYM PID_SHARE_CSC_STATUS}

const
  FMTID_Link: TGUID = '{B9B4B3FC-2B51-4a42-B5D8-324146AFCF25}';
  {$EXTERNALSYM FMTID_Link}
  PID_LINK_TARGET = 2;
  {$EXTERNALSYM PID_LINK_TARGET}

const
  FMTID_Query: TGUID = '{49691c90-7e17-101a-a91c-08002b2ecda9}';
  {$EXTERNALSYM FMTID_Query}
  PID_QUERY_RANK = 2;
  {$EXTERNALSYM PID_QUERY_RANK}

const
  FMTID_SummaryInformation: TGUID = '{f29f85e0-4ff9-1068-ab91-08002b27b3d9}';
  {$EXTERNALSYM FMTID_SummaryInformation}
  FMTID_DocumentSummaryInformation: TGUID = '{d5cdd502-2e9c-101b-9397-08002b2cf9ae}';
  {$EXTERNALSYM FMTID_DocumentSummaryInformation}
  FMTID_MediaFileSummaryInformation: TGUID = '{64440492-4c8b-11d1-8b70-080036b11a03}';
  {$EXTERNALSYM FMTID_MediaFileSummaryInformation}
  FMTID_ImageSummaryInformation: TGUID = '{6444048f-4c8b-11d1-8b70-080036b11a03}';
  {$EXTERNALSYM FMTID_ImageSummaryInformation}

// imgguids.h line 75

// Property sets
const
  FMTID_ImageInformation: TGUID = '{e5836cbe-5eef-4f1d-acde-ae4c43b608ce}';
  {$EXTERNALSYM FMTID_ImageInformation}
  FMTID_JpegAppHeaders: TGUID = '{1c4afdcd-6177-43cf-abc7-5f51af39ee85}';
  {$EXTERNALSYM FMTID_JpegAppHeaders}



{$IFNDEF CLR}

// objbase.h line 390
const
  STGFMT_STORAGE  = 0;
  {$EXTERNALSYM STGFMT_STORAGE}
  STGFMT_NATIVE   = 1;
  {$EXTERNALSYM STGFMT_NATIVE}
  STGFMT_FILE     = 3;
  {$EXTERNALSYM STGFMT_FILE}
  STGFMT_ANY      = 4;
  {$EXTERNALSYM STGFMT_ANY}
  STGFMT_DOCFILE  = 5;
  {$EXTERNALSYM STGFMT_DOCFILE}
// This is a legacy define to allow old component to builds
  STGFMT_DOCUMENT = 0;
  {$EXTERNALSYM STGFMT_DOCUMENT}

// objbase.h line 913

type
  tagSTGOPTIONS = record
    usVersion: Word;             // Versions 1 and 2 supported
    reserved: Word;              // must be 0 for padding
    ulSectorSize: Cardinal;      // docfile header sector size (512)
    pwcsTemplateFile: PWideChar; // version 2 or above
  end;
  {$EXTERNALSYM tagSTGOPTIONS}
  STGOPTIONS = tagSTGOPTIONS;
  {$EXTERNALSYM STGOPTIONS}
  PSTGOPTIONS = ^STGOPTIONS;
  {$EXTERNALSYM PSTGOPTIONS}

function StgCreateStorageEx(const pwcsName: PWideChar; grfMode: DWORD;
  stgfmt: DWORD; grfAttrs: DWORD; pStgOptions: PSTGOPTIONS; reserved2: Pointer;
  riid: PGUID; out stgOpen: IInterface):HResult; stdcall;
{$EXTERNALSYM StgCreateStorageEx}

function StgOpenStorageEx(const pwcsName: PWideChar; grfMode: DWORD;
  stgfmt: DWORD; grfAttrs: DWORD; pStgOptions: PSTGOPTIONS; reserved2:Pointer;
  riid: PGUID; out stgOpen: IInterface):HResult; stdcall;
{$EXTERNALSYM StgOpenStorageEx}

{$ENDIF ~CLR}


// NtSecApi.h line 566
type
  PLSA_UNICODE_STRING = ^LSA_UNICODE_STRING;
  _LSA_UNICODE_STRING = record
    Length: USHORT;
    MaximumLength: USHORT;
    Buffer: Windows.LPWSTR;
  end;
  LSA_UNICODE_STRING = _LSA_UNICODE_STRING;
  TLsaUnicodeString = LSA_UNICODE_STRING;
  PLsaUnicodeString = PLSA_UNICODE_STRING;

  PLSA_STRING = ^LSA_STRING;
  _LSA_STRING = record
    Length: USHORT;
    MaximumLength: USHORT;
    Buffer: PCHAR;
  end;
  LSA_STRING = _LSA_STRING;
  TLsaString = LSA_STRING;
  PLsaString = PLSA_STRING;

  PLSA_OBJECT_ATTRIBUTES = ^LSA_OBJECT_ATTRIBUTES;
  _LSA_OBJECT_ATTRIBUTES = record
    Length: ULONG;
    RootDirectory: Windows.THandle;
    ObjectName: PLSA_UNICODE_STRING;
    Attributes: ULONG;
    SecurityDescriptor: Pointer; // Points to type SECURITY_DESCRIPTOR
    SecurityQualityOfService: Pointer; // Points to type SECURITY_QUALITY_OF_SERVICE
  end;
  LSA_OBJECT_ATTRIBUTES = _LSA_OBJECT_ATTRIBUTES;
  TLsaObjectAttributes = _LSA_OBJECT_ATTRIBUTES;
  PLsaObjectAttributes = PLSA_OBJECT_ATTRIBUTES;

// NtSecApi.h line 680

////////////////////////////////////////////////////////////////////////////
//                                                                        //
// Local Security Policy Administration API datatypes and defines         //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

//
// Access types for the Policy object
//

const
  POLICY_VIEW_LOCAL_INFORMATION = $00000001;
  {$EXTERNALSYM POLICY_VIEW_LOCAL_INFORMATION}
  POLICY_VIEW_AUDIT_INFORMATION = $00000002;
  {$EXTERNALSYM POLICY_VIEW_AUDIT_INFORMATION}
  POLICY_GET_PRIVATE_INFORMATION = $00000004;
  {$EXTERNALSYM POLICY_GET_PRIVATE_INFORMATION}
  POLICY_TRUST_ADMIN = $00000008;
  {$EXTERNALSYM POLICY_TRUST_ADMIN}
  POLICY_CREATE_ACCOUNT = $00000010;
  {$EXTERNALSYM POLICY_CREATE_ACCOUNT}
  POLICY_CREATE_SECRET = $00000020;
  {$EXTERNALSYM POLICY_CREATE_SECRET}
  POLICY_CREATE_PRIVILEGE = $00000040;
  {$EXTERNALSYM POLICY_CREATE_PRIVILEGE}
  POLICY_SET_DEFAULT_QUOTA_LIMITS = $00000080;
  {$EXTERNALSYM POLICY_SET_DEFAULT_QUOTA_LIMITS}
  POLICY_SET_AUDIT_REQUIREMENTS = $00000100;
  {$EXTERNALSYM POLICY_SET_AUDIT_REQUIREMENTS}
  POLICY_AUDIT_LOG_ADMIN = $00000200;
  {$EXTERNALSYM POLICY_AUDIT_LOG_ADMIN}
  POLICY_SERVER_ADMIN = $00000400;
  {$EXTERNALSYM POLICY_SERVER_ADMIN}
  POLICY_LOOKUP_NAMES = $00000800;
  {$EXTERNALSYM POLICY_LOOKUP_NAMES}
  POLICY_NOTIFICATION = $00001000;
  {$EXTERNALSYM POLICY_NOTIFICATION}

  POLICY_ALL_ACCESS = (STANDARD_RIGHTS_REQUIRED or
                               POLICY_VIEW_LOCAL_INFORMATION or
                               POLICY_VIEW_AUDIT_INFORMATION or
                               POLICY_GET_PRIVATE_INFORMATION or
                               POLICY_TRUST_ADMIN or
                               POLICY_CREATE_ACCOUNT or
                               POLICY_CREATE_SECRET or
                               POLICY_CREATE_PRIVILEGE or
                               POLICY_SET_DEFAULT_QUOTA_LIMITS or
                               POLICY_SET_AUDIT_REQUIREMENTS or
                               POLICY_AUDIT_LOG_ADMIN or
                               POLICY_SERVER_ADMIN or
                               POLICY_LOOKUP_NAMES);
  {$EXTERNALSYM POLICY_ALL_ACCESS}

  POLICY_READ = (STANDARD_RIGHTS_READ or
                               POLICY_VIEW_AUDIT_INFORMATION or
                               POLICY_GET_PRIVATE_INFORMATION);
  {$EXTERNALSYM POLICY_READ}

  POLICY_WRITE = (STANDARD_RIGHTS_WRITE or
                               POLICY_TRUST_ADMIN or
                               POLICY_CREATE_ACCOUNT or
                               POLICY_CREATE_SECRET or
                               POLICY_CREATE_PRIVILEGE or
                               POLICY_SET_DEFAULT_QUOTA_LIMITS or
                               POLICY_SET_AUDIT_REQUIREMENTS or
                               POLICY_AUDIT_LOG_ADMIN or
                               POLICY_SERVER_ADMIN);
  {$EXTERNALSYM POLICY_WRITE}

  POLICY_EXECUTE = (STANDARD_RIGHTS_EXECUTE or
                               POLICY_VIEW_LOCAL_INFORMATION or
                               POLICY_LOOKUP_NAMES);
  {$EXTERNALSYM POLICY_EXECUTE}

// NtSecApi.h line 914
type
  _POLICY_INFORMATION_CLASS = (
    picFill0,
    PolicyAuditLogInformation,
    PolicyAuditEventsInformation,
    PolicyPrimaryDomainInformation,
    PolicyPdAccountInformation,
    PolicyAccountDomainInformation,
    PolicyLsaServerRoleInformation,
    PolicyReplicaSourceInformation,
    PolicyDefaultQuotaInformation,
    PolicyModificationInformation,
    PolicyAuditFullSetInformation,
    PolicyAuditFullQueryInformation,
    PolicyDnsDomainInformation,
    PolicyDnsDomainInformationInt);
  {$EXTERNALSYM _POLICY_INFORMATION_CLASS}
  POLICY_INFORMATION_CLASS = _POLICY_INFORMATION_CLASS;
  {$EXTERNALSYM POLICY_INFORMATION_CLASS}
  PPOLICY_INFORMATION_CLASS = ^POLICY_INFORMATION_CLASS;
  {$EXTERNALSYM PPOLICY_INFORMATION_CLASS}
  TPolicyInformationClass = POLICY_INFORMATION_CLASS;
  {$EXTERNALSYM TPolicyInformationClass}
  PPolicyInformationClass = PPOLICY_INFORMATION_CLASS;
  {$EXTERNALSYM PPolicyInformationClass}

// NtSecApi.h line 1031
//
// The following structure corresponds to the PolicyAccountDomainInformation
// information class.
//
type
  PPOLICY_ACCOUNT_DOMAIN_INFO = ^POLICY_ACCOUNT_DOMAIN_INFO;
  _POLICY_ACCOUNT_DOMAIN_INFO = record
    DomainName: LSA_UNICODE_STRING;
    DomainSid: Windows.PSID;
  end;
  POLICY_ACCOUNT_DOMAIN_INFO = _POLICY_ACCOUNT_DOMAIN_INFO;
  TPolicyAccountDomainInfo = POLICY_ACCOUNT_DOMAIN_INFO;
  PPolicyAccountDomainInfo = PPOLICY_ACCOUNT_DOMAIN_INFO;

// NtSecApi.h line 1298
type
  LSA_HANDLE = Pointer;
  PLSA_HANDLE = ^LSA_HANDLE;
  TLsaHandle = LSA_HANDLE;

// NtSecApi.h line 1714
type
  NTSTATUS = DWORD;

function LsaOpenPolicy(SystemName: PLSA_UNICODE_STRING;
  var ObjectAttributes: LSA_OBJECT_ATTRIBUTES; DesiredAccess: ACCESS_MASK;
  var PolicyHandle: LSA_HANDLE): NTSTATUS; stdcall;
function LsaQueryInformationPolicy(PolicyHandle: LSA_HANDLE;
  InformationClass: POLICY_INFORMATION_CLASS; var Buffer: Pointer): NTSTATUS; stdcall;
function LsaFreeMemory(Buffer: Pointer): NTSTATUS; stdcall;
function LsaFreeReturnBuffer(Buffer: Pointer): NTSTATUS; stdcall;
function LsaClose(ObjectHandle: LSA_HANDLE): NTSTATUS; stdcall;
function LsaNtStatusToWinError(Status: NTSTATUS): ULONG; stdcall;



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

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/windows/JclWin32.pas $';
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


{$IFNDEF CLR}
const
  aclapilib = 'advapi32.dll';

var
  _SetNamedSecurityInfoW: Pointer;

function SetNamedSecurityInfoW;
begin
  GetProcedureAddress(_SetNamedSecurityInfoW, aclapilib, 'SetNamedSecurityInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetNamedSecurityInfoW]
  end;
end;
{$ENDIF ~CLR}



{$IFNDEF CLR}
const
  ImageHlpLib = 'imagehlp.dll';
  
var
  _ReBaseImage: Pointer;

function ReBaseImage;
begin
  GetProcedureAddress(_ReBaseImage, ImageHlpLib, 'ReBaseImage');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReBaseImage]
  end;
end;

var
  _ReBaseImage64: Pointer;

function ReBaseImage64;
begin
  GetProcedureAddress(_ReBaseImage64, ImageHlpLib, 'ReBaseImage64');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReBaseImage64]
  end;
end;

var
  _CheckSumMappedFile: Pointer;

function CheckSumMappedFile;
begin
  GetProcedureAddress(_CheckSumMappedFile, ImageHlpLib, 'CheckSumMappedFile');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CheckSumMappedFile]
  end;
end;

var
  _GetImageUnusedHeaderBytes: Pointer;

function GetImageUnusedHeaderBytes;
begin
  GetProcedureAddress(_GetImageUnusedHeaderBytes, ImageHlpLib, 'GetImageUnusedHeaderBytes');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetImageUnusedHeaderBytes]
  end;
end;

var
  _MapAndLoad: Pointer;

function MapAndLoad;
begin
  GetProcedureAddress(_MapAndLoad, ImageHlpLib, 'MapAndLoad');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MapAndLoad]
  end;
end;

var
  _UnMapAndLoad: Pointer;

function UnMapAndLoad;
begin
  GetProcedureAddress(_UnMapAndLoad, ImageHlpLib, 'UnMapAndLoad');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UnMapAndLoad]
  end;
end;

var
  _TouchFileTimes: Pointer;

function TouchFileTimes;
begin
  GetProcedureAddress(_TouchFileTimes, ImageHlpLib, 'TouchFileTimes');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TouchFileTimes]
  end;
end;

var
  _ImageDirectoryEntryToData: Pointer;

function ImageDirectoryEntryToData;
begin
  GetProcedureAddress(_ImageDirectoryEntryToData, ImageHlpLib, 'ImageDirectoryEntryToData');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ImageDirectoryEntryToData]
  end;
end;

var
  _ImageRvaToSection: Pointer;

function ImageRvaToSection;
begin
  GetProcedureAddress(_ImageRvaToSection, ImageHlpLib, 'ImageRvaToSection');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ImageRvaToSection]
  end;
end;

var
  _ImageRvaToVa: Pointer;

function ImageRvaToVa;
begin
  GetProcedureAddress(_ImageRvaToVa, ImageHlpLib, 'ImageRvaToVa');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ImageRvaToVa]
  end;
end;

var
  _UnDecorateSymbolName: Pointer;

function UnDecorateSymbolName;
begin
  GetProcedureAddress(_UnDecorateSymbolName, ImageHlpLib, 'UnDecorateSymbolName');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UnDecorateSymbolName]
  end;
end;

{$ENDIF MSWINDOWS}



{$IFNDEF CLR}

var
  _NetUserAdd: Pointer;

function NetUserAdd;
begin
  GetProcedureAddress(_NetUserAdd, netapi32, 'NetUserAdd');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetUserAdd]
  end;
end;

var
  _NetUserEnum: Pointer;

function NetUserEnum;
begin
  GetProcedureAddress(_NetUserEnum, netapi32, 'NetUserEnum');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetUserEnum]
  end;
end;

var
  _NetUserGetInfo: Pointer;

function NetUserGetInfo;
begin
  GetProcedureAddress(_NetUserGetInfo, netapi32, 'NetUserGetInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetUserGetInfo]
  end;
end;

var
  _NetUserSetInfo: Pointer;

function NetUserSetInfo;
begin
  GetProcedureAddress(_NetUserSetInfo, netapi32, 'NetUserSetInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetUserSetInfo]
  end;
end;

var
  _NetUserDel: Pointer;

function NetUserDel;
begin
  GetProcedureAddress(_NetUserDel, netapi32, 'NetUserDel');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetUserDel]
  end;
end;

var
  _NetUserGetGroups: Pointer;

function NetUserGetGroups;
begin
  GetProcedureAddress(_NetUserGetGroups, netapi32, 'NetUserGetGroups');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetUserGetGroups]
  end;
end;

var
  _NetUserSetGroups: Pointer;

function NetUserSetGroups;
begin
  GetProcedureAddress(_NetUserSetGroups, netapi32, 'NetUserSetGroups');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetUserSetGroups]
  end;
end;

var
  _NetUserGetLocalGroups: Pointer;

function NetUserGetLocalGroups;
begin
  GetProcedureAddress(_NetUserGetLocalGroups, netapi32, 'NetUserGetLocalGroups');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetUserGetLocalGroups]
  end;
end;

var
  _NetUserModalsGet: Pointer;

function NetUserModalsGet;
begin
  GetProcedureAddress(_NetUserModalsGet, netapi32, 'NetUserModalsGet');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetUserModalsGet]
  end;
end;

var
  _NetUserModalsSet: Pointer;

function NetUserModalsSet;
begin
  GetProcedureAddress(_NetUserModalsSet, netapi32, 'NetUserModalsSet');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetUserModalsSet]
  end;
end;

var
  _NetUserChangePassword: Pointer;

function NetUserChangePassword;
begin
  GetProcedureAddress(_NetUserChangePassword, netapi32, 'NetUserChangePassword');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetUserChangePassword]
  end;
end;

var
  _NetGroupAdd: Pointer;

function NetGroupAdd;
begin
  GetProcedureAddress(_NetGroupAdd, netapi32, 'NetGroupAdd');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetGroupAdd]
  end;
end;

var
  _NetGroupAddUser: Pointer;

function NetGroupAddUser;
begin
  GetProcedureAddress(_NetGroupAddUser, netapi32, 'NetGroupAddUser');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetGroupAddUser]
  end;
end;

var
  _NetGroupEnum: Pointer;

function NetGroupEnum;
begin
  GetProcedureAddress(_NetGroupEnum, netapi32, 'NetGroupEnum');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetGroupEnum]
  end;
end;

var
  _NetGroupGetInfo: Pointer;

function NetGroupGetInfo;
begin
  GetProcedureAddress(_NetGroupGetInfo, netapi32, 'NetGroupGetInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetGroupGetInfo]
  end;
end;

var
  _NetGroupSetInfo: Pointer;

function NetGroupSetInfo;
begin
  GetProcedureAddress(_NetGroupSetInfo, netapi32, 'NetGroupSetInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetGroupSetInfo]
  end;
end;

var
  _NetGroupDel: Pointer;

function NetGroupDel;
begin
  GetProcedureAddress(_NetGroupDel, netapi32, 'NetGroupDel');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetGroupDel]
  end;
end;

var
  _NetGroupDelUser: Pointer;

function NetGroupDelUser;
begin
  GetProcedureAddress(_NetGroupDelUser, netapi32, 'NetGroupDelUser');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetGroupDelUser]
  end;
end;

var
  _NetGroupGetUsers: Pointer;

function NetGroupGetUsers;
begin
  GetProcedureAddress(_NetGroupGetUsers, netapi32, 'NetGroupGetUsers');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetGroupGetUsers]
  end;
end;

var
  _NetGroupSetUsers: Pointer;

function NetGroupSetUsers;
begin
  GetProcedureAddress(_NetGroupSetUsers, netapi32, 'NetGroupSetUsers');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetGroupSetUsers]
  end;
end;

var
  _NetLocalGroupAdd: Pointer;

function NetLocalGroupAdd;
begin
  GetProcedureAddress(_NetLocalGroupAdd, netapi32, 'NetLocalGroupAdd');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetLocalGroupAdd]
  end;
end;

var
  _NetLocalGroupAddMember: Pointer;

function NetLocalGroupAddMember;
begin
  GetProcedureAddress(_NetLocalGroupAddMember, netapi32, 'NetLocalGroupAddMember');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetLocalGroupAddMember]
  end;
end;

var
  _NetLocalGroupEnum: Pointer;

function NetLocalGroupEnum;
begin
  GetProcedureAddress(_NetLocalGroupEnum, netapi32, 'NetLocalGroupEnum');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetLocalGroupEnum]
  end;
end;

var
  _NetLocalGroupGetInfo: Pointer;

function NetLocalGroupGetInfo;
begin
  GetProcedureAddress(_NetLocalGroupGetInfo, netapi32, 'NetLocalGroupGetInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetLocalGroupGetInfo]
  end;
end;

var
  _NetLocalGroupSetInfo: Pointer;

function NetLocalGroupSetInfo;
begin
  GetProcedureAddress(_NetLocalGroupSetInfo, netapi32, 'NetLocalGroupSetInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetLocalGroupSetInfo]
  end;
end;

var
  _NetLocalGroupDel: Pointer;

function NetLocalGroupDel;
begin
  GetProcedureAddress(_NetLocalGroupDel, netapi32, 'NetLocalGroupDel');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetLocalGroupDel]
  end;
end;

var
  _NetLocalGroupDelMember: Pointer;

function NetLocalGroupDelMember;
begin
  GetProcedureAddress(_NetLocalGroupDelMember, netapi32, 'NetLocalGroupDelMember');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetLocalGroupDelMember]
  end;
end;

var
  _NetLocalGroupGetMembers: Pointer;

function NetLocalGroupGetMembers;
begin
  GetProcedureAddress(_NetLocalGroupGetMembers, netapi32, 'NetLocalGroupGetMembers');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetLocalGroupGetMembers]
  end;
end;

var
  _NetLocalGroupSetMembers: Pointer;

function NetLocalGroupSetMembers;
begin
  GetProcedureAddress(_NetLocalGroupSetMembers, netapi32, 'NetLocalGroupSetMembers');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetLocalGroupSetMembers]
  end;
end;

var
  _NetLocalGroupAddMembers: Pointer;

function NetLocalGroupAddMembers;
begin
  GetProcedureAddress(_NetLocalGroupAddMembers, netapi32, 'NetLocalGroupAddMembers');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetLocalGroupAddMembers]
  end;
end;

var
  _NetLocalGroupDelMembers: Pointer;

function NetLocalGroupDelMembers;
begin
  GetProcedureAddress(_NetLocalGroupDelMembers, netapi32, 'NetLocalGroupDelMembers');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetLocalGroupDelMembers]
  end;
end;

{$ENDIF ~CLR}


{$IFNDEF CLR}

var
  _NetApiBufferFree: Pointer;

function NetApiBufferFree;
begin
  GetProcedureAddress(_NetApiBufferFree, netapi32, 'NetApiBufferFree');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetApiBufferFree]
  end;
end;

{$ENDIF ~CLR}



{$IFNDEF CLR}

var
  _Netbios: Pointer;

function Netbios;
begin
  GetProcedureAddress(_Netbios, 'netapi32.dll', 'Netbios');
  asm
    mov esp, ebp
    pop ebp
    jmp [_Netbios]
  end;
end;

{$ENDIF ~CLR}



{$IFNDEF CLR}

var
  _BackupSeek: Pointer;

function BackupSeek;
begin
  GetProcedureAddress(_BackupSeek, kernel32, 'BackupSeek');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BackupSeek]
  end;
end;

var
  _AdjustTokenPrivileges: Pointer;

function AdjustTokenPrivileges;
begin
  GetProcedureAddress(_AdjustTokenPrivileges, advapi32, 'AdjustTokenPrivileges');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AdjustTokenPrivileges]
  end;
end;

function CreateMutex(lpMutexAttributes: PSecurityAttributes; bInitialOwner: DWORD; lpName: PChar): THandle; stdcall;
  external kernel32 name 'CreateMutexA';

function GetVersionEx(var lpVersionInformation: TOSVersionInfoEx): BOOL; stdcall;
  external kernel32 name 'GetVersionExA';
function GetVersionEx(lpVersionInformation: POSVersionInfoEx): BOOL; stdcall;
  external kernel32 name 'GetVersionExA';

var
  _SetWaitableTimer: Pointer;

function SetWaitableTimer;
begin
  GetProcedureAddress(_SetWaitableTimer, kernel32, 'SetWaitableTimer');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetWaitableTimer]
  end;
end;
var
  _SetFileSecurityA: Pointer;

function SetFileSecurityA;
begin
  GetProcedureAddress(_SetFileSecurityA, advapi32, 'SetFileSecurityA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetFileSecurityA]
  end;
end;

var
  _SetFileSecurityW: Pointer;

function SetFileSecurityW;
begin
  GetProcedureAddress(_SetFileSecurityW, advapi32, 'SetFileSecurityW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetFileSecurityW]
  end;
end;

var
  _SetFileSecurity: Pointer;

function SetFileSecurity;
begin
  GetProcedureAddress(_SetFileSecurity, advapi32, 'SetFileSecurity' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetFileSecurity]
  end;
end;

var
  _GetFileSecurityA: Pointer;

function GetFileSecurityA;
begin
  GetProcedureAddress(_GetFileSecurityA, advapi32, 'GetFileSecurityA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetFileSecurityA]
  end;
end;

var
  _GetFileSecurityW: Pointer;

function GetFileSecurityW;
begin
  GetProcedureAddress(_GetFileSecurityW, advapi32, 'GetFileSecurityW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetFileSecurityW]
  end;
end;

var
  _GetFileSecurity: Pointer;

function GetFileSecurity;
begin
  GetProcedureAddress(_GetFileSecurity, advapi32, 'GetFileSecurity' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetFileSecurity]
  end;
end;

var
  _SetVolumeMountPoint: Pointer;

function SetVolumeMountPoint;
begin
  GetProcedureAddress(_SetVolumeMountPoint, kernel32, 'SetVolumeMountPointA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetVolumeMountPoint]
  end;
end;

var
  _DeleteVolumeMountPoint: Pointer;

function DeleteVolumeMountPoint;
begin
  GetProcedureAddress(_DeleteVolumeMountPoint, kernel32, 'DeleteVolumeMountPointA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeleteVolumeMountPoint]
  end;
end;

var
  _GetVolumeNameForVolMountPoint: Pointer;

function GetVolumeNameForVolumeMountPoint;
begin
  GetProcedureAddress(_GetVolumeNameForVolMountPoint, kernel32, 'GetVolumeNameForVolumeMountPointA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetVolumeNameForVolMountPoint]
  end;
end;

{$ENDIF ~CLR}



{$IFNDEF CLR}

var
  _GetCalendarInfoA: Pointer;

function GetCalendarInfoA;
begin
  GetProcedureAddress(_GetCalendarInfoA, kernel32, 'GetCalendarInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCalendarInfoA]
  end;
end;

var
  _GetCalendarInfoW: Pointer;

function GetCalendarInfoW;
begin
  GetProcedureAddress(_GetCalendarInfoW, kernel32, 'GetCalendarInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCalendarInfoW]
  end;
end;

var
  _EnumCalendarInfoExA: Pointer;

function EnumCalendarInfoExA;
begin
  GetProcedureAddress(_EnumCalendarInfoExA, kernel32, 'EnumCalendarInfoExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumCalendarInfoExA]
  end;
end;

{$ENDIF ~CLR}


// line 9078

function MAKELANGID(PrimaryLang, SubLang: USHORT): WORD;
begin
  Result := (SubLang shl 10) or PrimaryLang;
end;

function PRIMARYLANGID(LangId: WORD): WORD;
begin
  Result := LangId and $03FF;
end;

function SUBLANGID(LangId: WORD): WORD;
begin
  Result := LangId shr 10;
end;

function MAKELCID(LangId, SortId: WORD): DWORD;
begin
  Result := (DWORD(SortId) shl 16) or DWORD(LangId);
end;

function MAKESORTLCID(LangId, SortId, SortVersion: WORD): DWORD;
begin
  Result := MAKELCID(LangId, SortId) or (SortVersion shl 20);
end;

function LANGIDFROMLCID(LocaleId: LCID): WORD;
begin
  Result := WORD(LocaleId);
end;

function SORTIDFROMLCID(LocaleId: LCID): WORD;
begin
  Result := WORD((DWORD(LocaleId) shr 16) and $000F);
end;

function SORTVERSIONFROMLCID(LocaleId: LCID): WORD;
begin
  Result := WORD((DWORD(LocaleId) shr 20) and $000F);
end;

// line 9149

function IsReparseTagMicrosoft(Tag: ULONG): Boolean;
begin
  Result := (Tag and ULONG($80000000)) <> 0;
end;

function IsReparseTagHighLatency(Tag: ULONG): Boolean;
begin
  Result := (Tag and ULONG($40000000)) <> 0;
end;

function IsReparseTagNameSurrogate(Tag: ULONG): Boolean;
begin
  Result := (Tag and ULONG($20000000)) <> 0;
end;

{$IFNDEF CLR}

// IMAGE_FIRST_SECTION by Nico Bendlin - supplied by Markus Fuchs

function FieldOffset(const Struc; const Field): Cardinal;
begin
  Result := Cardinal(@Field) - Cardinal(@Struc);
end;

function IMAGE_FIRST_SECTION(NtHeader: PImageNtHeaders): PImageSectionHeader;
begin
  Result := PImageSectionHeader(Cardinal(NtHeader) +
    FieldOffset(NtHeader^, NtHeader^.OptionalHeader) +
    NtHeader^.FileHeader.SizeOfOptionalHeader);
end;

// line 9204

function IMAGE_ORDINAL64(Ordinal: ULONGLONG): ULONGLONG;
begin
  Result := (Ordinal and $FFFF);
end;

function IMAGE_ORDINAL32(Ordinal: DWORD): DWORD;
begin
  Result := (Ordinal and $0000FFFF);
end;

function IMAGE_ORDINAL(Ordinal: DWORD): DWORD;
begin
  Result := (Ordinal and $0000FFFF);
end;

function IMAGE_SNAP_BY_ORDINAL64(Ordinal: ULONGLONG): Boolean;
begin
  Result := ((Ordinal and IMAGE_ORDINAL_FLAG64) <> 0);
end;

function IMAGE_SNAP_BY_ORDINAL32(Ordinal: DWORD): Boolean;
begin
  Result := ((Ordinal and IMAGE_ORDINAL_FLAG32) <> 0);
end;

function IMAGE_SNAP_BY_ORDINAL(Ordinal: DWORD): Boolean;
begin
  Result := ((Ordinal and IMAGE_ORDINAL_FLAG32) <> 0);
end;

{$ENDIF ~CLR}

{$IFNDEF CLR}

const
  PowrprofLib = 'PowrProf.dll';
  
var
  _IsPwrSuspendAllowed: Pointer;

function IsPwrSuspendAllowed;
begin
  GetProcedureAddress(_IsPwrSuspendAllowed, PowrprofLib, 'IsPwrSuspendAllowed');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsPwrSuspendAllowed]
  end;
end;

var
  _IsPwrHibernateAllowed: Pointer;

function IsPwrHibernateAllowed;
begin
  GetProcedureAddress(_IsPwrHibernateAllowed, PowrprofLib, 'IsPwrHibernateAllowed');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsPwrHibernateAllowed]
  end;
end;

var
  _IsPwrShutdownAllowed: Pointer;

function IsPwrShutdownAllowed;
begin
  GetProcedureAddress(_IsPwrShutdownAllowed, PowrprofLib, 'IsPwrShutdownAllowed');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsPwrShutdownAllowed]
  end;
end;

var
  _SetSuspendState: Pointer;

function SetSuspendState;
begin
  GetProcedureAddress(_SetSuspendState, PowrprofLib, 'SetSuspendState');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetSuspendState]
  end;
end;

{$ENDIF ~CLR}

{$IFNDEF CLR}

const
  Ole32Lib = 'ole32.dll';
  
var
  _StgCreateStorageEx: Pointer;

function StgCreateStorageEx;
begin
  GetProcedureAddress(_StgCreateStorageEx, Ole32Lib, 'StgCreateStorageEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_StgCreateStorageEx]
  end;
end;

var
  _StgOpenStorageEx: Pointer;

function StgOpenStorageEx;
begin
  GetProcedureAddress(_StgOpenStorageEx, Ole32Lib, 'StgOpenStorageEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_StgOpenStorageEx]
  end;
end;

{$ENDIF ~CLR}


var
  _LsaOpenPolicy: Pointer;

function LsaOpenPolicy;
begin
  GetProcedureAddress(_LsaOpenPolicy, advapi32, 'LsaOpenPolicy');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LsaOpenPolicy]
  end;
end;

var
  _LsaQueryInformationPolicy: Pointer;

function LsaQueryInformationPolicy;
begin
  GetProcedureAddress(_LsaQueryInformationPolicy, advapi32, 'LsaQueryInformationPolicy');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LsaQueryInformationPolicy]
  end;
end;

var
  _LsaFreeMemory: Pointer;

function LsaFreeMemory;
begin
  GetProcedureAddress(_LsaFreeMemory, advapi32, 'LsaFreeMemory');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LsaFreeMemory]
  end;
end;

var
  _LsaFreeReturnBuffer: Pointer;

function LsaFreeReturnBuffer;
begin
  GetProcedureAddress(_LsaFreeReturnBuffer, advapi32, 'LsaFreeReturnBuffer');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LsaFreeReturnBuffer]
  end;
end;

var
  _LsaClose: Pointer;

function LsaClose;
begin
  GetProcedureAddress(_LsaClose, advapi32, 'LsaClose');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LsaClose]
  end;
end;

var
  _LsaNtStatusToWinError: Pointer;

function LsaNtStatusToWinError;
begin
  GetProcedureAddress(_LsaNtStatusToWinError, advapi32, 'LsaNtStatusToWinError');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LsaNtStatusToWinError]
  end;
end;


{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.



