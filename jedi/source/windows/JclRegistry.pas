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
{ The Original Code is JclRegistry.pas.                                                            }
{                                                                                                  }
{ The Initial Developers of the Original Code are John C Molyneux, Marcel van Brakel and           }
{ Charlie Calvert. Portions created by these individuals are Copyright (C) of these individuals.   }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Marcel van Brakel                                                                              }
{   Stephane Fillon                                                                                }
{   Eric S.Fisher                                                                                  }
{   Peter Friese                                                                                   }
{   Andreas Hausladen (ahuser)                                                                     }
{   Manlio Laschena (manlio)                                                                       }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Olivier Sannier (obones)                                                                       }
{   Petr Vones (pvones)                                                                            }
{   kogerbnz                                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Contains various utility routines to read and write registry values. Using these routines        }
{ prevents you from having to instantiate temporary TRegistry objects and since the routines       }
{ directly call the registry API they do not suffer from the resource overhead as TRegistry does.  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-02-03 21:54:20 +0100 (dim., 03 févr. 2008)                        $ }
{ Revision:      $Rev:: 2343                                                                     $ }
{ Author:        $Author:: marcovtje                                                             $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclRegistry;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Classes,
  JclBase, JclStrings, JclWideStrings;

type
  DelphiHKEY = Longword;
  {$HPPEMIT '// BCB users must typecast the HKEY values to DelphiHKEY or use the HK-values below.'}

  TExecKind = (ekMachineRun, ekMachineRunOnce, ekUserRun, ekUserRunOnce,
    ekServiceRun, ekServiceRunOnce);

  EJclRegistryError = class(EJclError);

{$IFDEF FPC}
const
  HKCR = DelphiHKEY($80000000);
  HKCU = DelphiHKEY($80000001);
  HKLM = DelphiHKEY($80000002);
  HKUS = DelphiHKEY($80000003);
  HKPD = DelphiHKEY($80000004);
  HKCC = DelphiHKEY($80000005);
  HKDD = DelphiHKEY($80000006);
{$ELSE ~FPC}
const
  HKCR = DelphiHKEY(HKEY_CLASSES_ROOT);
  HKCU = DelphiHKEY(HKEY_CURRENT_USER);
  HKLM = DelphiHKEY(HKEY_LOCAL_MACHINE);
  HKUS = DelphiHKEY(HKEY_USERS);
  HKPD = DelphiHKEY(HKEY_PERFORMANCE_DATA);
  HKCC = DelphiHKEY(HKEY_CURRENT_CONFIG);
  HKDD = DelphiHKEY(HKEY_DYN_DATA);
{$ENDIF FPC}

const
  RegKeyDelimiter = '\';

function RegCreateKey(const RootKey: DelphiHKEY; const Key: string): Longint; overload;
function RegCreateKey(const RootKey: DelphiHKEY; const Key, Value: string): Longint; overload;
function RegDeleteEntry(const RootKey: DelphiHKEY; const Key, Name: string): Boolean;
function RegDeleteKeyTree(const RootKey: DelphiHKEY; const Key: string): Boolean;

function RegGetDataSize(const RootKey: DelphiHKEY; const Key, Name: string;
  out DataSize: Cardinal): Boolean;
function RegGetDataType(const RootKey: DelphiHKEY; const Key, Name: string;
  out DataType: Cardinal): Boolean;
function RegReadBool(const RootKey: DelphiHKEY; const Key, Name: string): Boolean;
function RegReadBoolDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Boolean): Boolean;
function RegReadIntegerEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: Integer; RaiseException: Boolean = False): Boolean;
function RegReadInteger(const RootKey: DelphiHKEY; const Key, Name: string): Integer;
function RegReadIntegerDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Integer): Integer;
function RegReadCardinalEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: Cardinal; RaiseException: Boolean = False): Boolean;
function RegReadCardinal(const RootKey: DelphiHKEY; const Key, Name: string): Cardinal;
function RegReadCardinalDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Cardinal): Cardinal;
function RegReadDWORDEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: DWORD; RaiseException: Boolean = False): Boolean;
function RegReadDWORD(const RootKey: DelphiHKEY; const Key, Name: string): DWORD;
function RegReadDWORDDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: DWORD): DWORD;
function RegReadInt64Ex(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: Int64; RaiseException: Boolean = False): Boolean;
function RegReadInt64(const RootKey: DelphiHKEY; const Key, Name: string): Int64;
function RegReadInt64Def(const RootKey: DelphiHKEY; const Key, Name: string; Def: Int64): Int64;
function RegReadUInt64Ex(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: UInt64; RaiseException: Boolean = False): Boolean;
function RegReadUInt64(const RootKey: DelphiHKEY; const Key, Name: string): UInt64;
function RegReadUInt64Def(const RootKey: DelphiHKEY; const Key, Name: string; Def: UInt64): UInt64;
function RegReadSingleEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: Single; RaiseException: Boolean = False): Boolean;
function RegReadSingle(const RootKey: DelphiHKEY; const Key, Name: string): Single;
function RegReadSingleDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Single): Single;
function RegReadDoubleEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: Double; RaiseException: Boolean = False): Boolean;
function RegReadDouble(const RootKey: DelphiHKEY; const Key, Name: string): Double;
function RegReadDoubleDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Double): Double;
function RegReadExtendedEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: Extended; RaiseException: Boolean = False): Boolean;
function RegReadExtended(const RootKey: DelphiHKEY; const Key, Name: string): Extended;
function RegReadExtendedDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Extended): Extended;

function RegReadStringEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: AnsiString; RaiseException: Boolean = False): Boolean;
function RegReadString(const RootKey: DelphiHKEY; const Key, Name: string): string;
function RegReadStringDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: string): string;
function RegReadAnsiStringEx(const RootKey: DelphiHKEY; const Key, Name: AnsiString;
  out RetValue: AnsiString; RaiseException: Boolean = False): Boolean;
function RegReadAnsiString(const RootKey: DelphiHKEY; const Key, Name: AnsiString): AnsiString;
function RegReadAnsiStringDef(const RootKey: DelphiHKEY; const Key, Name: AnsiString; Def: AnsiString): AnsiString;
function RegReadWideStringEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: WideString; RaiseException: Boolean = False): Boolean;
function RegReadWideString(const RootKey: DelphiHKEY; const Key, Name: string): WideString;
function RegReadWideStringDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: WideString): WideString;

function RegReadMultiSzEx(const RootKey: DelphiHKEY; const Key, Name: string; Value: TStrings;
  RaiseException: Boolean = False): Boolean; overload;
function RegReadMultiSzEx(const RootKey: DelphiHKEY; const Key, Name: string; out RetValue: PMultiSz;
  RaiseException: Boolean = False): Boolean; overload;
procedure RegReadMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; Value: TStrings); overload;
function RegReadMultiSz(const RootKey: DelphiHKEY; const Key, Name: string): PMultiSz; overload;
procedure RegReadMultiSzDef(const RootKey: DelphiHKEY; const Key, Name: string; Value, Def: TStrings); overload;
function RegReadMultiSzDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: PMultiSz): PMultiSz; overload;

function RegReadWideMultiSzEx(const RootKey: DelphiHKEY; const Key, Name: string; Value: TWideStrings;
  RaiseException: Boolean = False): Boolean; overload;
function RegReadWideMultiSzEx(const RootKey: DelphiHKEY; const Key, Name: string; out RetValue: PWideMultiSz;
  RaiseException: Boolean = False): Boolean; overload;
procedure RegReadWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; Value: TWideStrings); overload;
function RegReadWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string): PWideMultiSz; overload;
procedure RegReadWideMultiSzDef(const RootKey: DelphiHKEY; const Key, Name: string;
  Value, Def: TWideStrings); overload;
function RegReadWideMultiSzDef(const RootKey: DelphiHKEY; const Key, Name: string;
  Def: PWideMultiSz): PWideMultiSz; overload;

function RegReadBinaryEx(const RootKey: DelphiHKEY; const Key, Name: string; var Value; const ValueSize: Cardinal;
  out DataSize: Cardinal; RaiseException: Boolean = False): Boolean;
function RegReadBinary(const RootKey: DelphiHKEY; const Key, Name: string;
  var Value; const ValueSize: Cardinal): Cardinal;
function RegReadBinaryDef(const RootKey: DelphiHKEY; const Key, Name: string;
  var Value; const ValueSize: Cardinal; const Def: Byte): Cardinal;

procedure RegWriteBool(const RootKey: DelphiHKEY; const Key, Name: string; Value: Boolean); overload;
procedure RegWriteBool(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  Value: Boolean); overload;
procedure RegWriteInteger(const RootKey: DelphiHKEY; const Key, Name: string; Value: Integer); overload;
procedure RegWriteInteger(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  Value: Integer); overload;
procedure RegWriteCardinal(const RootKey: DelphiHKEY; const Key, Name: string; Value: Cardinal); overload;
procedure RegWriteCardinal(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  Value: Cardinal); overload;
procedure RegWriteDWORD(const RootKey: DelphiHKEY; const Key, Name: string; Value: DWORD); overload;
procedure RegWriteDWORD(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  Value: DWORD); overload;
procedure RegWriteInt64(const RootKey: DelphiHKEY; const Key, Name: string; Value: Int64); overload;
procedure RegWriteInt64(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  Value: Int64); overload;
procedure RegWriteUInt64(const RootKey: DelphiHKEY; const Key, Name: string; Value: UInt64); overload;
procedure RegWriteUInt64(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  Value: UInt64); overload;
procedure RegWriteSingle(const RootKey: DelphiHKEY; const Key, Name: string; Value: Single); overload;
procedure RegWriteSingle(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  Value: Single); overload;
procedure RegWriteDouble(const RootKey: DelphiHKEY; const Key, Name: string; Value: Double); overload;
procedure RegWriteDouble(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  Value: Double); overload;
procedure RegWriteExtended(const RootKey: DelphiHKEY; const Key, Name: string; Value: Extended); overload;
procedure RegWriteExtended(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  Value: Extended); overload;

procedure RegWriteString(const RootKey: DelphiHKEY; const Key, Name, Value: string); overload;
procedure RegWriteString(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  Value: string); overload;
procedure RegWriteAnsiString(const RootKey: DelphiHKEY; const Key, Name, Value: AnsiString); overload;
procedure RegWriteAnsiString(const RootKey: DelphiHKEY; const Key, Name: AnsiString; DataType: Cardinal;
  Value: AnsiString); overload;
procedure RegWriteWideString(const RootKey: DelphiHKEY; const Key, Name: string; Value: WideString); overload;
procedure RegWriteWideString(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  Value: WideString); overload;

procedure RegWriteMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; Value: PMultiSz); overload;
procedure RegWriteMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; const Value: TStrings); overload;
procedure RegWriteMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  Value: PMultiSz); overload;
procedure RegWriteMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  const Value: TStrings); overload;
procedure RegWriteWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; Value: PWideMultiSz); overload;
procedure RegWriteWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string;
  const Value: TWideStrings); overload;
procedure RegWriteWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  Value: PWideMultiSz); overload;
procedure RegWriteWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  const Value: TWideStrings); overload;

procedure RegWriteBinary(const RootKey: DelphiHKEY; const Key, Name: string; const Value; const ValueSize: Cardinal);

function RegGetValueNames(const RootKey: DelphiHKEY; const Key: string; const List: TStrings): Boolean;
function RegGetKeyNames(const RootKey: DelphiHKEY; const Key: string; const List: TStrings): Boolean;
function RegHasSubKeys(const RootKey: DelphiHKEY; const Key: string): Boolean;

function AllowRegKeyForEveryone(const RootKey: DelphiHKEY; Path: string): Boolean;
{
From: Jean-Fabien Connault [cycocrew att worldnet dott fr]
Descr: Test whether a registry key exists as a subkey of RootKey
Used test cases:
procedure TForm1.Button1Click(Sender: TObject);
var
  RegKey: HKEY;
begin
  if RegOpenKeyEx(HKEY_CURRENT_USER, 'Software', 0, KEY_READ, RegKey) = ERROR_SUCCESS then
  begin
    Assert(not RegKeyExists(RegKey, 'Microsoft\_Windows'));
    RegCloseKey(RegKey);
  end;
  if RegOpenKeyEx(HKEY_CURRENT_USER, 'Software', 0, KEY_READ, RegKey) = ERROR_SUCCESS then
  begin
    Assert(RegKeyExists(RegKey, 'Microsoft\Windows'));;
    RegCloseKey(RegKey);
  end;
  Assert(RegKeyExists(HKEY_CURRENT_USER, ''));
  Assert(RegKeyExists(HKEY_CURRENT_USER, 'Software'));
  Assert(RegKeyExists(HKEY_CURRENT_USER, 'Software\Microsoft'));
  Assert(RegKeyExists(HKEY_CURRENT_USER, 'Software\Microsoft\Windows'));
  Assert(RegKeyExists(HKEY_CURRENT_USER, '\Software\Microsoft\Windows'));
  Assert(not RegKeyExists(HKEY_CURRENT_USER, '\Software\Microsoft2\Windows'));
end;
}
function RegKeyExists(const RootKey: DelphiHKEY; const Key: string): Boolean;
function RegValueExists(const RootKey: DelphiHKEY; const Key, Name: string): Boolean;

function UnregisterAutoExec(ExecKind: TExecKind; const Name: string): Boolean;
function RegisterAutoExec(ExecKind: TExecKind; const Name, Cmdline: string): Boolean;

function RegSaveList(const RootKey: DelphiHKEY; const Key: string; const ListName: string;
  const Items: TStrings): Boolean;
function RegLoadList(const RootKey: DelphiHKEY; const Key: string; const ListName: string;
  const SaveTo: TStrings): Boolean;
function RegDelList(const RootKey: DelphiHKEY; const Key: string; const ListName: string): Boolean;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/windows/JclRegistry.pas $';
    Revision: '$Revision: 2343 $';
    Date: '$Date: 2008-02-03 21:54:20 +0100 (dim., 03 févr. 2008) $';
    LogPath: 'JCL\source\windows'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils,
  {$IFDEF FPC}
//  JwaAccCtrl,
  {$ELSE}
  AccCtrl,
  {$ENDIF FPC}
  JclResources, JclSysUtils, JclWin32;

type
  TRegKind = REG_NONE..REG_QWORD;
  TRegKinds = set of TRegKind;

const
  cItems = 'Items';
  cRegBinKinds = [REG_SZ..REG_QWORD];  // all types

//=== Internal helper routines ===============================================

function RootKeyName(const RootKey: THandle): string;
begin
  case RootKey of
    HKCR : Result := RsHKCRLong;
    HKCU : Result := RsHKCULong;
    HKLM : Result := RsHKLMLong;
    HKUS : Result := RsHKUSLong;
    HKPD : Result := RsHKPDLong;
    HKCC : Result := RsHKCCLong;
    HKDD : Result := RsHKDDLong;
    else
    {$IFDEF DELPHICOMPILER}
      Result := Format('$%.8x', [RootKey]);
    {$ENDIF DELPHICOMPILER}
    {$IFDEF BCB}
      Result := Format('0x%.8x', [RootKey]);
    {$ENDIF BCB}
  end;
end;

procedure ReadError(const RootKey: THandle; const Key: string);
begin
  raise EJclRegistryError.CreateResFmt(@RsUnableToOpenKeyRead, [RootKeyName(RootKey), Key]);
end;

procedure WriteError(const RootKey: THandle; const Key: string);
begin
  raise EJclRegistryError.CreateResFmt(@RsUnableToOpenKeyWrite, [RootKeyName(RootKey), Key]);
end;

procedure ValueError(const RootKey: THandle; const Key, Name: string);
begin
  raise EJclRegistryError.CreateResFmt(@RsUnableToAccessValue, [RootKeyName(RootKey), Key, Name]);
end;

procedure DataError(const RootKey: THandle; const Key, Name: string);
begin
  raise EJclRegistryError.CreateResFmt(@RsWrongDataType, [RootKeyName(RootKey), Key, Name]);
end;

function GetKeyAndPath(ExecKind: TExecKind; var Key: HKEY; out RegPath: string): Boolean;
begin
  Result := False;
  if (ExecKind in [ekServiceRun, ekServiceRunOnce]) and (Win32Platform = VER_PLATFORM_WIN32_NT) then
    Exit;
  if ExecKind in [ekMachineRun, ekMachineRunOnce, ekServiceRun, ekServiceRunOnce] then
    Key := HKEY_LOCAL_MACHINE
  else
    Key := HKEY_CURRENT_USER;
  RegPath := 'Software\Microsoft\Windows\CurrentVersion\';
  case ExecKind of
    ekMachineRun, ekUserRun:
      RegPath := RegPath + 'Run';
    ekMachineRunOnce, ekUserRunOnce:
      RegPath := RegPath + 'RunOnce';
    ekServiceRun:
      RegPath := RegPath + 'RunServices';
    ekServiceRunOnce:
      RegPath := RegPath + 'RunServicesOnce';
  end;
  Result := True;
end;

function RelativeKey(const RootKey: DelphiHKEY; Key: PChar): PChar;
type
  TRootKey = record
    Key: DelphiHKEY;
    Name: string;
  end;
const
  RootKeys: array [0..13] of TRootKey =
   (
    (Key: HKCR; Name: RsHKCRLong),
    (Key: HKCU; Name: RsHKCULong),
    (Key: HKLM; Name: RsHKLMLong),
    (Key: HKUS; Name: RsHKUSLong),
    (Key: HKPD; Name: RsHKPDLong),
    (Key: HKCC; Name: RsHKCCLong),
    (Key: HKDD; Name: RsHKDDLong),
    (Key: HKCR; Name: RsHKCRShort),
    (Key: HKCU; Name: RsHKCUShort),
    (Key: HKLM; Name: RsHKLMShort),
    (Key: HKUS; Name: RsHKUSShort),
    (Key: HKPD; Name: RsHKPDShort),
    (Key: HKCC; Name: RsHKCCShort),
    (Key: HKDD; Name: RsHKDDShort)
   );
var
  I: Integer;
begin
  Result := Key;
  if Result^ = RegKeyDelimiter then
    Inc(Result);
  for I := Low(RootKeys) to High(RootKeys) do
    if StrPos(Key, PChar(RootKeys[I].Name + RegKeyDelimiter)) = Result then
    begin
      if RootKey <> RootKeys[I].Key then
        raise EJclRegistryError.CreateResFmt(@RsInconsistentPath, [Key])
      else
        Inc(Result, Length(RootKeys[I].Name));
      Break;
    end;
end;

function InternalRegOpenKeyEx(Key: HKEY; SubKey: PChar;
  ulOptions: DWORD; samDesired: REGSAM; var RegKey: HKEY): Longint;
var
  WideKey: WideString;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    WideKey := RelativeKey(Key, SubKey);
    Result := RegOpenKeyExW(Key, PWideChar(WideKey), ulOptions, samDesired, RegKey);
  end
  else
    Result := RegOpenKeyExA(Key, RelativeKey(Key, SubKey), ulOptions, samDesired, RegKey);
end;

function InternalRegQueryValueEx(Key: HKEY; ValueName: PChar;
  lpReserved: Pointer; lpType: PDWORD; lpData: PByte; lpcbData: PDWORD): Longint;
var
  WideName: WideString;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    WideName := ValueName;
    Result := RegQueryValueExW(Key, PWideChar(WideName), lpReserved, lpType, lpData, lpcbData);
  end
  else
    Result := RegQueryValueExA(Key, ValueName, lpReserved, lpType, lpData, lpcbData);
end;

function InternalRegSetValueEx(Key: HKEY; ValueName: PChar;
  Reserved: DWORD; dwType: DWORD; lpData: Pointer; cbData: DWORD): Longint; stdcall;
var
  WideName: WideString;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    WideName := ValueName;
    Result := RegSetValueExW(Key, PWideChar(WideName), Reserved, dwType, lpData, cbData);
  end
  else
    Result := RegSetValueExA(Key, PChar(ValueName), Reserved, dwType, lpData, cbData);
end;

function InternalGetData(const RootKey: DelphiHKEY; const Key, Name: string;
  RegKinds: TRegKinds; ExpectedSize: DWORD;
  out DataType: DWORD; Data: Pointer; out DataSize: DWORD; RaiseException: Boolean): Boolean;
var
  RegKey: HKEY;
begin
  Result := True;
  DataType := REG_NONE;
  DataSize := 0;
  if InternalRegOpenKeyEx(RootKey, PChar(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
    try
      if InternalRegQueryValueEx(RegKey, PChar(Name), nil, @DataType, nil, @DataSize) = ERROR_SUCCESS then
      begin
        if not (DataType in RegKinds) or (DataSize > ExpectedSize) then
          if RaiseException then
            DataError(RootKey, Key, Name)
          else
            Result := False;
        if InternalRegQueryValueEx(RegKey, PChar(Name), nil, nil, Data, @DataSize) <> ERROR_SUCCESS then
          if RaiseException then
            ValueError(RootKey, Key, Name)
          else
            Result := False;
      end
      else
        if RaiseException then
          ValueError(RootKey, Key, Name)
        else
          Result := False;
    finally
      RegCloseKey(RegKey);
    end
  else
    if RaiseException then
      ReadError(RootKey, Key)
    else
      Result := False;
end;

function InternalGetString(const RootKey: DelphiHKEY; const Key, Name: string; MultiFlag: Boolean;
  out RetValue: string; RaiseException: Boolean): Boolean;
var
  RegKey: HKEY;
  DataType, DataSize: DWORD;
  RegKinds: TRegKinds;
begin
  Result := True;
  DataType := REG_NONE;
  DataSize := 0;
  RetValue := '';
  if RegOpenKeyEx(RootKey, RelativeKey(RootKey, PChar(Key)), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
    try
      if RegQueryValueEx(RegKey, PChar(Name), nil, @DataType, nil, @DataSize) = ERROR_SUCCESS then
      begin
        RegKinds := [REG_BINARY, REG_SZ, REG_EXPAND_SZ];
        if MultiFlag then
          RegKinds := RegKinds + [REG_MULTI_SZ];
        if not (DataType in RegKinds) then
          DataError(RootKey, Key, Name);
        SetLength(RetValue, DataSize div SizeOf(Char) + 1);
        if RegQueryValueEx(RegKey, PChar(Name), nil, nil, Pointer(RetValue), @DataSize) <> ERROR_SUCCESS then
        begin
          RetValue := '';
          if RaiseException then
            ValueError(RootKey, Key, Name)
          else
          begin
            Result := False;
            DataSize := 1; // => empty string
          end;
        end;
        SetLength(RetValue, (DataSize - 1) div SizeOf(Char));
      end
      else
        if RaiseException then
          ValueError(RootKey, Key, Name)
        else
          Result := False;
    finally
      RegCloseKey(RegKey);
    end
  else
    if RaiseException then
      ReadError(RootKey, Key)
    else
      Result := False;
end;

function InternalGetWideString(const RootKey: DelphiHKEY; const Key, Name: string; MultiFlag: Boolean;
  out RetValue: WideString; RaiseException: Boolean): Boolean;
var
  RegKey: HKEY;
  DataType, DataSize: DWORD;
  RegKinds: TRegKinds;
begin
  Result := True;
  DataType := REG_NONE;
  DataSize := 0;
  RetValue := '';
  if InternalRegOpenKeyEx(RootKey, PChar(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
    try
      if InternalRegQueryValueEx(RegKey, PChar(Name), nil, @DataType, nil, @DataSize) = ERROR_SUCCESS then
      begin
        if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
          RegKinds := [REG_BINARY]
        else
        if MultiFlag then
          RegKinds := [REG_BINARY, REG_SZ, REG_EXPAND_SZ, REG_MULTI_SZ]
        else
          RegKinds := [REG_BINARY, REG_SZ, REG_EXPAND_SZ];
        if not (DataType in RegKinds) then
          DataError(RootKey, Key, Name);
        SetLength(RetValue, DataSize div SizeOf(WideChar) + 1);
        if InternalRegQueryValueEx(RegKey, PChar(Name), nil, nil, Pointer(RetValue), @DataSize) <> ERROR_SUCCESS then
        begin
          RetValue := '';
          if RaiseException then
            ValueError(RootKey, Key, Name)
          else
          begin
            Result := False;
            DataSize := 1; // => empty string
          end;
        end;
        SetLength(RetValue, (DataSize - 1) div SizeOf(WideChar));
      end
      else
        if RaiseException then
          ValueError(RootKey, Key, Name)
        else
          Result := False;
    finally
      RegCloseKey(RegKey);
    end
  else
    if RaiseException then
      ReadError(RootKey, Key)
    else
      Result := False;
end;

procedure InternalSetData(const RootKey: DelphiHKEY; const Key, Name: string;
  RegKind: TRegKind; Value: Pointer; ValueSize: Cardinal);
var
  RegKey: HKEY;
begin
  if not RegKeyExists(RootKey, Key) then
    RegCreateKey(RootKey, Key);
  if RegOpenKeyEx(RootKey, RelativeKey(RootKey, PChar(Key)), 0, KEY_WRITE, RegKey) = ERROR_SUCCESS then
    try
      if RegSetValueEx(RegKey, PChar(Name), 0, RegKind, Value, ValueSize) <> ERROR_SUCCESS then
      WriteError(RootKey, Key);
    finally
      RegCloseKey(RegKey);
    end
  else
    WriteError(RootKey, Key);
end;

procedure InternalSetWideData(const RootKey: DelphiHKEY; const Key, Name: string;
  RegKind: TRegKind; Value: Pointer; ValueSize: Cardinal);
var
  RegKey: HKEY;
begin
  if not RegKeyExists(RootKey, Key) then
    RegCreateKey(RootKey, Key);
  if InternalRegOpenKeyEx(RootKey, RelativeKey(RootKey, PChar(Key)), 0, KEY_WRITE, RegKey) = ERROR_SUCCESS then
    try
      if InternalRegSetValueEx(RegKey, PChar(Name), 0, RegKind, Value, ValueSize) <> ERROR_SUCCESS then
        WriteError(RootKey, Key);
    finally
      RegCloseKey(RegKey);
    end
  else
    WriteError(RootKey, Key);
end;

//=== Registry ===============================================================

function RegCreateKey(const RootKey: DelphiHKEY; const Key: string): Longint;
var
  RegKey: HKEY;
begin
  Result := Windows.RegCreateKey(RootKey, RelativeKey(RootKey, PChar(Key)), RegKey);
  if Result = ERROR_SUCCESS then
    RegCloseKey(RegKey);
end;

function RegCreateKey(const RootKey: DelphiHKEY; const Key, Value: string): Longint;
begin
  Result := RegSetValue(RootKey, RelativeKey(RootKey, PChar(Key)), REG_SZ, PChar(Value), Length(Value));
end;

function RegDeleteEntry(const RootKey: DelphiHKEY; const Key, Name: string): Boolean;
var
  RegKey: HKEY;
begin
  Result := False;
  if RegOpenKeyEx(RootKey, RelativeKey(RootKey, PChar(Key)), 0, KEY_SET_VALUE, RegKey) = ERROR_SUCCESS then
  begin
    Result := RegDeleteValue(RegKey, PChar(Name)) = ERROR_SUCCESS;
    RegCloseKey(RegKey);
    if not Result then
      ValueError(RootKey, Key, Name);
  end
  else
    WriteError(RootKey, Key);
end;

function RegDeleteKeyTree(const RootKey: DelphiHKEY; const Key: string): Boolean;
var
  RegKey: HKEY;
  I: DWORD;
  Size: DWORD;
  NumSubKeys: DWORD;
  MaxSubKeyLen: DWORD;
  KeyName: string;
begin
  Result := RegOpenKeyEx(RootKey, RelativeKey(RootKey, PChar(Key)), 0, KEY_ALL_ACCESS, RegKey) = ERROR_SUCCESS;
  if Result then
  begin
    RegQueryInfoKey(RegKey, nil, nil, nil, @NumSubKeys, @MaxSubKeyLen, nil, nil, nil, nil, nil, nil);
    if NumSubKeys <> 0 then
      for I := NumSubKeys - 1 downto 0 do
      begin
        Size := MaxSubKeyLen+1;
        SetLength(KeyName, Size);
        RegEnumKeyEx(RegKey, I, PChar(KeyName), Size, nil, nil, nil, nil);
        SetLength(KeyName, StrLen(PChar(KeyName)));
        Result := RegDeleteKeyTree(RootKey, Key + RegKeyDelimiter + KeyName);
        if not Result then
          Break;
      end;
    RegCloseKey(RegKey);
    if Result then
      Result := Windows.RegDeleteKey(RootKey, RelativeKey(RootKey, PChar(Key))) = ERROR_SUCCESS;
  end
  else
    WriteError(RootKey, Key);
end;

function RegGetDataSize(const RootKey: DelphiHKEY; const Key, Name: string;
  out DataSize: Cardinal): Boolean;
var
  RegKey: HKEY;
begin
  DataSize := 0;
  Result := RegOpenKeyEx(RootKey, RelativeKey(RootKey, PChar(Key)), 0, KEY_READ, RegKey) = ERROR_SUCCESS;
  if Result then
  begin
    Result := RegQueryValueEx(RegKey, PChar(Name), nil, nil, nil, @DataSize) = ERROR_SUCCESS;
    RegCloseKey(RegKey);
  end;
end;

function RegGetDataType(const RootKey: DelphiHKEY; const Key, Name: string;
  out DataType: DWORD): Boolean;
var
  RegKey: HKEY;
begin
  DataType := REG_NONE;
  Result := RegOpenKeyEx(RootKey, RelativeKey(RootKey, PChar(Key)), 0, KEY_READ, RegKey) = ERROR_SUCCESS;
  if Result then
  begin
    Result := RegQueryValueEx(RegKey, PChar(Name), nil, @DataType, nil, nil) = ERROR_SUCCESS;
    RegCloseKey(RegKey);
  end;
end;

function RegReadBool(const RootKey: DelphiHKEY; const Key, Name: string): Boolean;
begin
  Result := RegReadInteger(RootKey, Key, Name) <> 0;
end;

function RegReadBoolDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Boolean): Boolean;
begin
  Result := RegReadIntegerDef(RootKey, Key, Name, Ord(Def)) <> 0;
end;

function RegReadIntegerEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: Integer; RaiseException: Boolean): Boolean;
var
  DataType, DataSize: DWORD;
  Ret: Int64;
begin
  Ret := 0;
  RegGetDataType(RootKey, Key, Name, DataType);
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    if RaiseException then
    begin
      Ret := StrToInt64(RegReadString(RootKey, Key, Name));
      Result := True;
    end
    else
      Result := TryStrToInt64(RegReadString(RootKey, Key, Name), Ret)
  else
    Result := InternalGetData(RootKey, Key, Name, [REG_BINARY, REG_DWORD, REG_QWORD],
      SizeOf(Ret), DataType, @Ret, DataSize, RaiseException);
  RetValue := Integer(Ret and $FFFFFFFF);
end;

function RegReadInteger(const RootKey: DelphiHKEY; const Key, Name: string): Integer;
begin
  RegReadIntegerEx(RootKey, Key, Name, Result, True);
end;

function RegReadIntegerDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Integer): Integer;
begin
  try
    if not RegReadIntegerEx(RootKey, Key, Name, Result, False) then
      Result := Def;
  except
    Result := Def;
  end;
end;

function RegReadCardinalEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: Cardinal; RaiseException: Boolean): Boolean;
var
  DataType, DataSize: DWORD;
  Ret: Int64;
begin
  Ret := 0;
  RegGetDataType(RootKey, Key, Name, DataType);
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    if RaiseException then
    begin
      Ret := StrToInt64(RegReadString(RootKey, Key, Name));
      Result := True;
    end
    else
      Result := TryStrToInt64(RegReadString(RootKey, Key, Name), Ret)
  else
    Result := InternalGetData(RootKey, Key, Name, [REG_BINARY, REG_DWORD, REG_QWORD],
      SizeOf(Ret), DataType, @Ret, DataSize, RaiseException);
  RetValue := Cardinal(Ret) and $FFFFFFFF;
end;

function RegReadCardinal(const RootKey: DelphiHKEY; const Key, Name: string): Cardinal;
begin
  RegReadCardinalEx(RootKey, Key, Name, Result, True);
end;

function RegReadCardinalDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Cardinal): Cardinal;
begin
  try
    if not RegReadCardinalEx(RootKey, Key, Name, Result, False) then
      Result := Def;
  except
    Result := Def;
  end;
end;

function RegReadDWORDEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: DWORD; RaiseException: Boolean): Boolean;
begin
  Result := RegReadCardinalEx(RootKey, Key, Name, RetValue, RaiseException);
end;

function RegReadDWORD(const RootKey: DelphiHKEY; const Key, Name: string): DWORD;
begin
  Result := RegReadCardinal(RootKey, Key, Name);
end;

function RegReadDWORDDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: DWORD): DWORD;
begin
  Result := RegReadCardinalDef(RootKey, Key, Name, Def);
end;

function RegReadInt64Ex(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: Int64; RaiseException: Boolean): Boolean;
var
  DataType, DataSize: DWORD;
  Data: array [0..1] of Integer;
  Ret: Int64;
begin
  RegGetDataType(RootKey, Key, Name, DataType);
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
  begin
    // (rom) circumvents internal compiler error for D6
    if RaiseException then
    begin
      Ret := StrToInt64(RegReadString(RootKey, Key, Name));
      Result := True;
    end
    else
      Result := TryStrToInt64(RegReadString(RootKey, Key, Name), Ret);
    RetValue := Ret;
  end
  else
  begin
    FillChar(Data[0], SizeOf(Data), 0);
    Result := InternalGetData(RootKey, Key, Name, [REG_BINARY, REG_DWORD, REG_QWORD],
       SizeOf(Data), DataType, @Data, DataSize, RaiseException);
    // REG_BINARY is implicitly unsigned if DataSize < 8
    if DataType = REG_DWORD then
      // DWORDs get sign extended
      RetValue := Data[0]
    else
      Move(Data[0], RetValue, SizeOf(Data));
  end;
end;

function RegReadInt64(const RootKey: DelphiHKEY; const Key, Name: string): Int64;
begin
  RegReadInt64Ex(RootKey, Key, Name, Result, True);
end;

function RegReadInt64Def(const RootKey: DelphiHKEY; const Key, Name: string; Def: Int64): Int64;
begin
  try
    if not RegReadInt64Ex(RootKey, Key, Name, Result, False) then
      Result := Def;
  except
    Result := Def;
  end;
end;

function RegReadUInt64Ex(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: UInt64; RaiseException: Boolean): Boolean;
var
  DataType, DataSize: DWORD;
  Ret: Int64;
begin
  RegGetDataType(RootKey, Key, Name, DataType);
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
  begin
    // (rom) circumvents internal compiler error for D6
    if RaiseException then
    begin
      Ret := StrToInt64(RegReadString(RootKey, Key, Name));
      Result := True;
    end
    else
      Result := TryStrToInt64(RegReadString(RootKey, Key, Name), Ret);
    RetValue := UInt64(Ret);
  end
  else
  begin
    // type cast required to circumvent internal error in D7
    RetValue := UInt64(0);
    Result := InternalGetData(RootKey, Key, Name, [REG_BINARY, REG_DWORD, REG_QWORD],
      SizeOf(RetValue), DataType, @RetValue, DataSize, RaiseException);
  end;
end;

function RegReadUInt64(const RootKey: DelphiHKEY; const Key, Name: string): UInt64;
begin
  RegReadUInt64Ex(RootKey, Key, Name, Result, True);
end;

function RegReadUInt64Def(const RootKey: DelphiHKEY; const Key, Name: string; Def: UInt64): UInt64;
begin
  try
    if not RegReadUInt64Ex(RootKey, Key, Name, Result, False) then
      Result := Def;
  except
    Result := Def;
  end;
end;

function RegReadSingleEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: Single; RaiseException: Boolean): Boolean;
var
  DataType, DataSize: DWORD;
  OldSep: Char;
begin
  RegGetDataType(RootKey, Key, Name, DataType);
  OldSep := DecimalSeparator;
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    try
      DecimalSeparator := '.';
      if RaiseException then
      begin
        RetValue := StrToFloat(RegReadString(RootKey, Key, Name));
        Result := True;
      end
      else
        Result := TryStrToFloat(RegReadString(RootKey, Key, Name), RetValue);
    finally
      DecimalSeparator := OldSep;
    end
  else
    Result := InternalGetData(RootKey, Key, Name, [REG_BINARY],
      SizeOf(RetValue), DataType, @RetValue, DataSize, RaiseException);
end;

function RegReadSingle(const RootKey: DelphiHKEY; const Key, Name: string): Single;
begin
  RegReadSingleEx(RootKey, KEy, Name, Result, True);
end;

function RegReadSingleDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Single): Single;
begin
  try
    if not RegReadSingleEx(RootKey, KEy, Name, Result, False) then
      Result := Def;
  except
    Result := Def;
  end;
end;

function RegReadDoubleEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: Double; RaiseException: Boolean): Boolean;
var
  DataType, DataSize: DWORD;
  OldSep: Char;
begin
  RegGetDataType(RootKey, Key, Name, DataType);
  OldSep := DecimalSeparator;
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    try
      DecimalSeparator := '.';
      if RaiseException then
      begin
        RetValue := StrToFloat(RegReadString(RootKey, Key, Name));
        Result := True;
      end
      else
        Result := TryStrToFloat(RegReadString(RootKey, Key, Name), RetValue);
    finally
      DecimalSeparator := OldSep;
    end
  else
    Result := InternalGetData(RootKey, Key, Name, [REG_BINARY],
      SizeOf(RetValue), DataType, @RetValue, DataSize, RaiseException);
end;

function RegReadDouble(const RootKey: DelphiHKEY; const Key, Name: string): Double;
begin
  RegReadDoubleEx(RootKey, Key, Name, Result, True);
end;

function RegReadDoubleDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Double): Double;
begin
  try
    if not RegReadDoubleEx(RootKey, Key, Name, Result, False) then
      Result := Def;
  except
    Result := Def;
  end;
end;

function RegReadExtendedEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: Extended; RaiseException: Boolean): Boolean;
var
  DataType, DataSize: DWORD;
  OldSep: Char;
begin
  RegGetDataType(RootKey, Key, Name, DataType);
  OldSep := DecimalSeparator;
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    try
      DecimalSeparator := '.';
      if RaiseException then
      begin
        RetValue := StrToFloat(RegReadString(RootKey, Key, Name));
        Result := True;
      end
      else
        Result := TryStrToFloat(RegReadString(RootKey, Key, Name), RetValue);
    finally
      DecimalSeparator := OldSep;
    end
  else
    Result := InternalGetData(RootKey, Key, Name, [REG_BINARY],
      SizeOf(RetValue), DataType, @RetValue, DataSize, RaiseException);
end;

function RegReadExtended(const RootKey: DelphiHKEY; const Key, Name: string): Extended;
begin
  RegReadExtendedEx(RootKey, Key, Name, Result, True);
end;

function RegReadExtendedDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Extended): Extended;
begin
  try
    if not RegReadExtendedEx(RootKey, Key, Name, Result, False) then
      Result := Def;
  except
    Result := Def;
  end;
end;

function RegReadStringEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: AnsiString; RaiseException: Boolean): Boolean;
begin
  Result := RegReadAnsiStringEx(RootKey, Key, Name, RetValue, RaiseException);
end;

function RegReadString(const RootKey: DelphiHKEY; const Key, Name: string): string;
begin
  Result := RegReadAnsiString(RootKey, Key, Name);
end;

function RegReadStringDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: string): string;
begin
  Result := RegReadAnsiStringDef(RootKey, Key, Name, Def);
end;

function RegReadAnsiStringEx(const RootKey: DelphiHKEY; const Key, Name: AnsiString;
  out RetValue: AnsiString; RaiseException: Boolean): Boolean;
begin
  Result := InternalGetString(RootKey, Key, Name, False, RetValue, RaiseException);
end;

function RegReadAnsiString(const RootKey: DelphiHKEY; const Key, Name: AnsiString): AnsiString;
begin
  RegReadAnsiStringEx(RootKey, Key, Name, Result, True);
end;

function RegReadAnsiStringDef(const RootKey: DelphiHKEY; const Key, Name: AnsiString; Def: AnsiString): AnsiString;
begin
  try
    if not RegReadAnsiStringEx(RootKey, Key, Name, Result, False) then
      Result := Def;
  except
    Result := Def;
  end;
end;

function RegReadWideStringEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: WideString; RaiseException: Boolean): Boolean;
begin
  Result := InternalGetWideString(RootKey, Key, Name, False, RetValue, RaiseException);
end;

function RegReadWideString(const RootKey: DelphiHKEY; const Key, Name: string): WideString;
begin
  RegReadWideStringEx(RootKey, Key, Name, Result, True);
end;

function RegReadWideStringDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: WideString): WideString;
begin
  try
    if not RegReadWideStringEx(RootKey, Key, Name, Result, False) then
      Result := Def;
  except
    Result := Def;
  end;
end;

function RegReadMultiSzEx(const RootKey: DelphiHKEY; const Key, Name: string; Value: TStrings;
  RaiseException: Boolean): Boolean;
var
  S: string;
begin
  Result := InternalGetString(RootKey, Key, Name, True, S, RaiseException);
  if Result then
    MultiSzToStrings(Value, PMultiSz(PChar(S)));
end;

procedure RegReadMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; Value: TStrings);
begin
  RegReadMultiSzEx(RootKey, Key, Name, Value, True);
end;

procedure RegReadMultiSzDef(const RootKey: DelphiHKEY; const Key, Name: string; Value, Def: TStrings);
begin
  try
    if not RegReadMultiSzEx(RootKey, Key, Name, Value, False) then
      Value.Assign(Def);
  except
    Value.Assign(Def);
  end;
end;

function RegReadMultiSzEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: PMultiSz; RaiseException: Boolean): Boolean;
var
  S: string;
begin
  RetValue := nil;
  Result := InternalGetString(RootKey, Key, Name, True, S, RaiseException);
  if Result then
    // always returns a newly allocated PMultiSz
    RetValue := MultiSzDup(PMultiSz(PChar(S)));
end;

function RegReadMultiSz(const RootKey: DelphiHKEY; const Key, Name: string): PMultiSz;
begin
  RegReadMultiSzEx(RootKey, Key, Name, Result, True);
end;

function RegReadMultiSzDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: PMultiSz): PMultiSz;
begin
  try
    if not RegReadMultiSzEx(RootKey, Key, Name, Result, False) then
      // always returns a newly allocated PMultiSz
      Result := MultiSzDup(Def);
  except
    // always returns a newly allocated PMultiSz
    Result := MultiSzDup(Def);
  end;
end;

function RegReadWideMultiSzEx(const RootKey: DelphiHKEY; const Key, Name: string; Value: TWideStrings;
  RaiseException: Boolean): Boolean; 
var
  S: WideString;
begin
  Result := InternalGetWideString(RootKey, Key, Name, True, S, RaiseException);
  if Result then
    WideMultiSzToWideStrings(Value, PWideMultiSz(PWideChar(S)));
end;

procedure RegReadWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; Value: TWideStrings);
begin
  RegReadWideMultiSzEx(RootKey, Key, Name, Value, True);
end;

procedure RegReadWideMultiSzDef(const RootKey: DelphiHKEY; const Key, Name: string; Value, Def: TWideStrings);
begin
  try
    if not RegReadWideMultiSzEx(RootKey, Key, Name, Value, False) then
      Value.Assign(Def);
  except
    Value.Assign(Def);
  end;
end;

function RegReadWideMultiSzEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: PWideMultiSz; RaiseException: Boolean): Boolean; overload;
var
  S: WideString;
begin
  RetValue := nil;
  Result := InternalGetWideString(RootKey, Key, Name, True, S, RaiseException);
  if Result then
    // always returns a newly allocated PMultiWideSz
    RetValue := WideMultiSzDup(PWideMultiSz(PWideChar(S)));
end;

function RegReadWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string): PWideMultiSz;
begin
  RegReadWideMultiSzEx(RootKey, Key, Name, Result, True);
end;

function RegReadWideMultiSzDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: PWideMultiSz): PWideMultiSz;
begin
  try
    if RegReadWideMultiSzEx(RootKey, Key, Name, Result, False) then
      // always returns a newly allocated PWideMultiSz
      Result := WideMultiSzDup(Def);
  except
    // always returns a newly allocated PWideMultiSz
    Result := WideMultiSzDup(Def);
  end;
end;

function RegReadBinaryEx(const RootKey: DelphiHKEY; const Key, Name: string; var Value;
  const ValueSize: Cardinal; out DataSize: Cardinal; RaiseException: Boolean): Boolean;
var
  DataType: DWORD;
begin
  Result := InternalGetData(RootKey, Key, Name, cRegBinKinds, ValueSize, DataType, @Value, DataSize, RaiseException);
end;

function RegReadBinary(const RootKey: DelphiHKEY; const Key, Name: string; var Value;
  const ValueSize: Cardinal): Cardinal;
begin
  RegReadBinaryEx(RootKey, Key, Name, Value, ValueSize, Result, True);
end;

function RegReadBinaryDef(const RootKey: DelphiHKEY; const Key, Name: string;
  var Value; const ValueSize: Cardinal; const Def: Byte): Cardinal;
begin
  try
    if not RegReadBinaryEx(RootKey, Key, Name, Value, ValueSize, Result, False) then
    begin
      FillChar(Value, ValueSize, Def);
      Result := ValueSize;
    end;
  except
    FillChar(Value, ValueSize, Def);
    Result := ValueSize;
  end;
end;

procedure RegWriteBool(const RootKey: DelphiHKEY; const Key, Name: string; Value: Boolean);
begin
  RegWriteCardinal(RootKey, Key, Name, REG_DWORD, Cardinal(Ord(Value)));
end;

procedure RegWriteBool(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: Boolean);
begin
  RegWriteCardinal(RootKey, Key, Name, DataType, Cardinal(Ord(Value)));
end;

procedure RegWriteInteger(const RootKey: DelphiHKEY; const Key, Name: string; Value: Integer);
begin
  RegWriteInteger(RootKey, Key, Name, REG_DWORD, Value);
end;

procedure RegWriteInteger(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: Integer);
var
  Val: Int64;
  Size: Integer;
begin
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    RegWriteString(RootKey, Key, Name, DataType, Format('%d', [Value]))
  else
  if DataType in [REG_DWORD, REG_QWORD, REG_BINARY] then
  begin
    // sign extension
    Val := Value;
    if DataType = REG_QWORD then
      Size := SizeOf(Int64)
    else
      Size := SizeOf(Value);
    InternalSetData(RootKey, Key, Name, DataType, @Val, Size);
  end
  else
    DataError(RootKey, Key, Name);
end;

procedure RegWriteCardinal(const RootKey: DelphiHKEY; const Key, Name: string; Value: Cardinal);
begin
  RegWriteCardinal(RootKey, Key, Name, REG_DWORD, Cardinal(Value));
end;

procedure RegWriteCardinal(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: Cardinal);
var
  Val: Int64;
  Size: Integer;
begin
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    RegWriteString(RootKey, Key, Name, DataType, Format('%u', [Value]))
  else
  if DataType in [REG_DWORD, REG_QWORD, REG_BINARY] then
  begin
    // no sign extension
    Val := Value and $FFFFFFFF;
    if DataType = REG_QWORD then
      Size := SizeOf(Int64)
    else
      Size := SizeOf(Value);
    InternalSetData(RootKey, Key, Name, DataType, @Val, Size);
  end
  else
    DataError(RootKey, Key, Name);
end;

procedure RegWriteDWORD(const RootKey: DelphiHKEY; const Key, Name: string; Value: DWORD);
begin
  RegWriteCardinal(RootKey, Key, Name, REG_DWORD, Cardinal(Value));
end;

procedure RegWriteDWORD(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: DWORD);
begin
  RegWriteCardinal(RootKey, Key, Name, DataType, Cardinal(Value));
end;

procedure RegWriteInt64(const RootKey: DelphiHKEY; const Key, Name: string; Value: Int64);
begin
  RegWriteInt64(RootKey, Key, Name, REG_QWORD, Value);
end;

procedure RegWriteInt64(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: Int64);
begin
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    RegWriteString(RootKey, Key, Name, DataType, Format('%d', [Value]))
  else
    RegWriteUInt64(RootKey, Key, Name, DataType, UInt64(Value));
end;

procedure RegWriteUInt64(const RootKey: DelphiHKEY; const Key, Name: string; Value: UInt64);
begin
  RegWriteUInt64(RootKey, Key, Name, REG_QWORD, Value);
end;

procedure RegWriteUInt64(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: UInt64);
begin
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    RegWriteString(RootKey, Key, Name, DataType, Format('%u', [Value]))
  else
  if DataType in [REG_QWORD, REG_BINARY] then
    InternalSetData(RootKey, Key, Name, DataType, @Value, SizeOf(Value))
  else
    DataError(RootKey, Key, Name);
end;

procedure RegWriteSingle(const RootKey: DelphiHKEY; const Key, Name: string; Value: Single);
begin
  RegWriteSingle(RootKey, Key, Name, REG_BINARY, Value);
end;

procedure RegWriteSingle(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: Single);
begin
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    RegWriteString(RootKey, Key, Name, DataType, Format('%g', [Value]))
  else
  if DataType in [REG_BINARY] then
    InternalSetData(RootKey, Key, Name, DataType, @Value, SizeOf(Value))
  else
    DataError(RootKey, Key, Name);
end;

procedure RegWriteDouble(const RootKey: DelphiHKEY; const Key, Name: string; Value: Double);
begin
  RegWriteDouble(RootKey, Key, Name, REG_BINARY, Value);
end;

procedure RegWriteDouble(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: Double);
begin
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    RegWriteString(RootKey, Key, Name, DataType, Format('%g', [Value]))
  else
  if DataType in [REG_BINARY] then
    InternalSetData(RootKey, Key, Name, DataType, @Value, SizeOf(Value))
  else
    DataError(RootKey, Key, Name);
end;

procedure RegWriteExtended(const RootKey: DelphiHKEY; const Key, Name: string; Value: Extended);
begin
  RegWriteExtended(RootKey, Key, Name, REG_BINARY, Value);
end;

procedure RegWriteExtended(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: Extended);
begin
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    RegWriteString(RootKey, Key, Name, DataType, Format('%g', [Value]))
  else
  if DataType in [REG_BINARY] then
    InternalSetData(RootKey, Key, Name, DataType, @Value, SizeOf(Value))
  else
    DataError(RootKey, Key, Name);
end;

procedure RegWriteString(const RootKey: DelphiHKEY; const Key, Name, Value: string);
begin
  RegWriteAnsiString(RootKey, Key, Name, REG_SZ, Value);
end;

procedure RegWriteString(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: string);
begin
  RegWriteAnsiString(RootKey, Key, Name, DataType, Value);
end;

procedure RegWriteAnsiString(const RootKey: DelphiHKEY; const Key, Name, Value: AnsiString);
begin
  RegWriteAnsiString(RootKey, Key, Name, REG_SZ, Value);
end;

procedure RegWriteAnsiString(const RootKey: DelphiHKEY; const Key, Name: AnsiString; DataType: Cardinal;
  Value: AnsiString);
begin
  if DataType in [REG_BINARY, REG_SZ, REG_EXPAND_SZ] then
    InternalSetData(RootKey, Key, Name, DataType, PChar(Value),
      (Length(Value) + 1) * SizeOf(AnsiChar))
  else
    DataError(RootKey, Key, Name);
end;

procedure RegWriteWideString(const RootKey: DelphiHKEY; const Key, Name: string; Value: WideString);
begin
  RegWriteWideString(RootKey, Key, Name, REG_SZ, Value);
end;

procedure RegWriteWideString(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: WideString);
begin
  if DataType in [REG_BINARY, REG_SZ, REG_EXPAND_SZ] then
    if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
      InternalSetWideData(RootKey, Key, Name, REG_BINARY, PWideChar(Value),
        (Length(Value) + 1) * SizeOf(WideChar))
    else
      InternalSetWideData(RootKey, Key, Name, DataType, PWideChar(Value),
        (Length(Value) + 1) * SizeOf(WideChar))
  else
    DataError(RootKey, Key, Name);
end;

procedure RegWriteMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; Value: PMultiSz);
begin
  RegWriteMultiSz(RootKey, Key, Name, REG_MULTI_SZ, Value);
end;

procedure RegWriteMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: PMultiSz);
begin
  if DataType in [REG_BINARY, REG_MULTI_SZ] then
    InternalSetData(RootKey, Key, Name, DataType, Value,
      MultiSzLength(Value) * SizeOf(Char))
  else
    DataError(RootKey, Key, Name);
end;

procedure RegWriteMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; const Value: TStrings);
begin
  RegWriteMultiSz(RootKey, Key, Name, REG_MULTI_SZ, Value);
end;

procedure RegWriteMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  const Value: TStrings);
var
  Dest: PMultiSz;
begin
  if DataType in [REG_BINARY, REG_MULTI_SZ] then
  begin
    StringsToMultiSz(Dest, Value);
    try
      RegWriteMultiSz(RootKey, Key, Name, DataType, Dest);
    finally
      FreeMultiSz(Dest);
    end;
  end
  else
    DataError(RootKey, Key, Name);
end;

procedure RegWriteWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; Value: PWideMultiSz);
begin
  RegWriteWideMultiSz(RootKey, Key, Name, REG_MULTI_SZ, Value);
end;

procedure RegWriteWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  Value: PWideMultiSz);
begin
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
    DataType := REG_BINARY;
  if DataType in [REG_BINARY, REG_MULTI_SZ] then
    InternalSetWideData(RootKey, Key, Name, DataType, Value,
      WideMultiSzLength(Value) * SizeOf(WideChar))
  else
    DataError(RootKey, Key, Name);
end;

procedure RegWriteWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; const Value: TWideStrings);
begin
  RegWriteWideMultiSz(RootKey, Key, Name, REG_MULTI_SZ, Value);
end;

procedure RegWriteWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  const Value: TWideStrings);
var
  Dest: PWideMultiSz;
begin
  if DataType in [REG_BINARY, REG_MULTI_SZ] then
  begin
    WideStringsToWideMultiSz(Dest, Value);
    try
      RegWriteWideMultiSz(RootKey, Key, Name, DataType, Dest);
    finally
      FreeWideMultiSz(Dest);
    end;
  end
  else
    DataError(RootKey, Key, Name);
end;

procedure RegWriteBinary(const RootKey: DelphiHKEY; const Key, Name: string; const Value; const ValueSize: Cardinal);
begin
  InternalSetData(RootKey, Key, Name, REG_BINARY, @Value, ValueSize);
end;

function UnregisterAutoExec(ExecKind: TExecKind; const Name: string): Boolean;
var
  Key: HKEY;
  RegPath: string;
begin
  Result := GetKeyAndPath(ExecKind, Key, RegPath);
  if Result then
    Result := RegDeleteEntry(Key, RegPath, Name);
end;

function RegisterAutoExec(ExecKind: TExecKind; const Name, Cmdline: string): Boolean;
var
  Key: HKEY;
  RegPath: string;
begin
  Result := GetKeyAndPath(ExecKind, Key, RegPath);
  if Result then
    RegWriteString(Key, RegPath, Name, Cmdline);
end;

function RegGetValueNames(const RootKey: DelphiHKEY; const Key: string; const List: TStrings): Boolean;
var
  RegKey: HKEY;
  I: DWORD;
  Size: DWORD;
  NumSubKeys: DWORD;
  NumSubValues: DWORD;
  MaxSubValueLen: DWORD;
  ValueName: string;
begin
  Result := False;
  List.BeginUpdate;
  try
    List.Clear;
    if RegOpenKeyEx(RootKey, RelativeKey(RootKey, PChar(Key)), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
    begin
      if RegQueryInfoKey(RegKey, nil, nil, nil, @NumSubKeys, nil, nil,
        @NumSubValues, @MaxSubValueLen, nil, nil, nil) = ERROR_SUCCESS then
      begin
        SetLength(ValueName, MaxSubValueLen + 1);
        if NumSubValues <> 0 then
          for I := 0 to NumSubValues - 1 do
          begin
            Size := MaxSubValueLen + 1;
            RegEnumValue(RegKey, I, PChar(ValueName), Size, nil, nil, nil, nil);
            List.Add(PChar(ValueName));
          end;
        Result := True;
      end;
      RegCloseKey(RegKey);
    end
    else
      ReadError(RootKey, Key);
  finally
    List.EndUpdate;
  end;
end;

function RegGetKeyNames(const RootKey: DelphiHKEY; const Key: string; const List: TStrings): Boolean;
var
  RegKey: HKEY;
  I: DWORD;
  Size: DWORD;
  NumSubKeys: DWORD;
  MaxSubKeyLen: DWORD;
  KeyName: string;
begin
  Result := False;
  List.BeginUpdate;
  try
    List.Clear;
    if RegOpenKeyEx(RootKey, RelativeKey(RootKey, PChar(Key)), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
    begin
      if RegQueryInfoKey(RegKey, nil, nil, nil,
        @NumSubKeys, @MaxSubKeyLen, nil, nil, nil, nil, nil, nil) = ERROR_SUCCESS then
      begin
        SetLength(KeyName, MaxSubKeyLen+1);
        if NumSubKeys <> 0 then
          for I := 0 to NumSubKeys-1 do
          begin
            Size := MaxSubKeyLen+1;
            RegEnumKeyEx(RegKey, I, PChar(KeyName), Size, nil, nil, nil, nil);
            List.Add(PChar(KeyName));
          end;
        Result := True;
      end;
      RegCloseKey(RegKey);
    end
    else
      ReadError(RootKey, Key);
  finally
    List.EndUpdate;
  end;
end;

function RegHasSubKeys(const RootKey: DelphiHKEY; const Key: string): Boolean;
var
  RegKey: HKEY;
  NumSubKeys: Integer;
begin
  Result := False;
  if RegOpenKeyEx(RootKey, RelativeKey(RootKey, PChar(Key)), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
  begin
    RegQueryInfoKey(RegKey, nil, nil, nil, @NumSubKeys, nil, nil, nil, nil, nil, nil, nil);
    Result := NumSubKeys <> 0;
    RegCloseKey(RegKey);
  end
  else
    ReadError(RootKey, Key);
end;

function RegKeyExists(const RootKey: DelphiHKEY; const Key: string): Boolean;
var
  RegKey: HKEY;
begin
  Result := (RegOpenKeyEx(RootKey, RelativeKey(RootKey, PChar(Key)), 0, KEY_READ, RegKey) = ERROR_SUCCESS);
  if Result then
    RegCloseKey(RegKey);
end;

function RegValueExists(const RootKey: DelphiHKEY; const Key, Name: string): Boolean;
var
  RegKey: HKEY;
begin
  Result := (RegOpenKeyEx(RootKey, RelativeKey(RootKey, PChar(Key)), 0, KEY_READ, RegKey) = ERROR_SUCCESS);
  if Result then
  begin
    Result := RegQueryValueEx(RegKey, PChar(Name), nil, nil, nil, nil) = ERROR_SUCCESS;
    RegCloseKey(RegKey);
  end;
end;

function RegSaveList(const RootKey: DelphiHKEY; const Key: string;
  const ListName: string; const Items: TStrings): Boolean;
var
  I: Integer;
  SubKey: string;
begin
  Result := False;
  SubKey := Key + RegKeyDelimiter + ListName;
  if RegCreateKey(RootKey, SubKey) = ERROR_SUCCESS then
  begin
    // Save Number of strings
    RegWriteInteger(RootKey, SubKey, cItems, Items.Count);
    for I := 1 to Items.Count do
      RegWriteString(RootKey, SubKey, IntToStr(I), Items[I-1]);
    Result := True;
  end;
end;

function RegLoadList(const RootKey: DelphiHKEY; const Key: string;
  const ListName: string; const SaveTo: TStrings): Boolean;
var
  I, N: Integer;
  SubKey: string;
begin
  SaveTo.BeginUpdate;
  try
    SaveTo.Clear;
    SubKey := Key + RegKeyDelimiter + ListName;
    N := RegReadIntegerDef(RootKey, SubKey, cItems, -1);
    for I := 1 to N do
      SaveTo.Add(RegReadString(RootKey, SubKey, IntToStr(I)));
    Result := N > 0;
  finally
    SaveTo.EndUpdate;
  end;
end;

function RegDelList(const RootKey: DelphiHKEY; const Key: string; const ListName: string): Boolean;
var
  I, N: Integer;
  SubKey: string;
begin
  Result := False;
  SubKey := Key + RegKeyDelimiter + ListName;
  N := RegReadIntegerDef(RootKey, SubKey, cItems, -1);
  if (N > 0) and RegDeleteEntry(RootKey, SubKey, cItems) then
    for I := 1 to N do
    begin
      Result := RegDeleteEntry(RootKey, SubKey, IntToStr(I));
      if not Result then
        Break;
    end;
end;

function AllowRegKeyForEveryone(const RootKey: DelphiHKEY; Path: string): Boolean;
var
  WidePath: PWideChar;
  Len: Integer;
begin
  Result := Win32Platform <> VER_PLATFORM_WIN32_NT;
  if not Result then // Win 2000/XP
  begin
    case RootKey of
      HKLM:
        Path := RsHKLMLong + RegKeyDelimiter + RelativeKey(RootKey, PChar(Path));
      HKCU:
        Path := RsHKCULong + RegKeyDelimiter + RelativeKey(RootKey, PChar(Path));
      HKCR:
        Path := RsHKCRLong + RegKeyDelimiter + RelativeKey(RootKey, PChar(Path));
      HKUS:
        Path := RsHKUSLong + RegKeyDelimiter + RelativeKey(RootKey, PChar(Path));
    end;
    Len := (Length(Path) + 1) * SizeOf(WideChar);
    GetMem(WidePath, Len);
    MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, PChar(Path), -1, WidePath, Len);
    Result := RtdlSetNamedSecurityInfoW(WidePath, SE_REGISTRY_KEY,
      DACL_SECURITY_INFORMATION, nil, nil, nil, nil) = ERROR_SUCCESS;
    FreeMem(WidePath);
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

