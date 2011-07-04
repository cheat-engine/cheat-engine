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
{ The Original Code is JclBase.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel.                                 }
{ Portions created by Marcel van Brakel are Copyright Marcel van Brakel. All rights reserved.      }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Marcel van Brakel,                                                                             }
{   Peter Friese,                                                                                  }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Petr Vones (pvones)                                                                            }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains generic JCL base classes and routines to support earlier                      }
{ versions of Delphi as well as FPC.                                                               }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-07-29 20:25:06 +0200 (mar., 29 juil. 2008)                         $ }
{ Revision:      $Rev:: 2405                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclBase;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF CLR}
  Classes, System.Reflection,
  {$ELSE}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$ENDIF CLR}
  {$IFDEF SUPPORTS_GENERICS}
  {$IFDEF CLR}
  System.Collections.Generic,
  {$ENDIF CLR}
  {$ENDIF SUPPORTS_GENERICS}
  SysUtils;

// Version
const
  JclVersionMajor   = 1;    // 0=pre-release|beta/1, 2, ...=final
  JclVersionMinor   = 102;  // Fifth minor release since JCL 1.90
  JclVersionRelease = 1;    // 0: pre-release|beta/ 1: release
  JclVersionBuild   = 3072; // build number, days since march 1, 2000
  JclVersion = (JclVersionMajor shl 24) or (JclVersionMinor shl 16) or
    (JclVersionRelease shl 15) or (JclVersionBuild shl 0);

// EJclError
type
  EJclError = class(Exception);
  {$IFDEF CLR}
  DWORD = LongWord;
  TIntegerSet = set of 0..SizeOf(Integer) * 8 - 1;
  {$ENDIF CLR}

{$IFDEF CLR}
  UnicodeString = string;
{$ELSE}
  UnicodeString = WideString;
{$ENDIF CLR}

// EJclWin32Error
{$IFDEF MSWINDOWS}
type
  EJclWin32Error = class(EJclError)
  private
    FLastError: DWORD;
    FLastErrorMsg: string;
  public
    constructor Create(const Msg: string);
    constructor CreateFmt(const Msg: string; const Args: array of const);
    {$IFNDEF CLR}
    constructor CreateRes(Ident: Integer); overload;
    constructor CreateRes(ResStringRec: PResStringRec); overload;
    {$ENDIF ~CLR}
    property LastError: DWORD read FLastError;
    property LastErrorMsg: string read FLastErrorMsg;
  end;
{$ENDIF MSWINDOWS}

// EJclInternalError
type
  EJclInternalError = class(EJclError);

// Types
type
  {$IFDEF MATH_EXTENDED_PRECISION}
  Float = Extended;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  Float = Double;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  Float = Single;
  {$ENDIF MATH_SINGLE_PRECISION}

  PFloat = ^Float;

type
  {$IFDEF FPC}
  Largeint = Int64;
  {$ELSE}
  PPointer = ^Pointer;
  {$IFDEF RTL140_UP}
  {$IFDEF CLR}
  PJclByteArray = TBytes;
  {$ELSE}
  PByte = System.PByte;
  Int8 = ShortInt;
  Int16 = Smallint;
  Int32 = Integer;
  UInt8 = Byte;
  UInt16 = Word;
  UInt32 = LongWord;
  {$ENDIF CLR}
  {$ELSE ~RTL140_UP}
  PBoolean = ^Boolean;
  PByte = Windows.PByte;
  {$ENDIF ~RTL140_UP}
  {$ENDIF FPC}
  PCardinal = ^Cardinal;
  {$IFNDEF COMPILER7_UP}
  UInt64 = Int64;
  {$ENDIF ~COMPILER7_UP}
  {$IFNDEF CLR}
  PWideChar = System.PWideChar;
  PPWideChar = ^JclBase.PWideChar;
  PInt64 = type System.PInt64;
  PPInt64 = ^JclBase.PInt64;
  {$ENDIF CLR}

// Interface compatibility
{$IFDEF SUPPORTS_INTERFACE}
{$IFNDEF FPC}
{$IFNDEF RTL140_UP}

type
  IInterface = IUnknown;

{$ENDIF ~RTL140_UP}
{$ENDIF ~FPC}
{$ENDIF SUPPORTS_INTERFACE}

// Int64 support
procedure I64ToCardinals(I: Int64; var LowPart, HighPart: Cardinal);
procedure CardinalsToI64(var I: Int64; const LowPart, HighPart: Cardinal);

// Redefinition of TLargeInteger to relieve dependency on Windows.pas

type
  PLargeInteger = ^TLargeInteger;
  TLargeInteger = Int64;
  {$IFNDEF COMPILER11_UP}
  TBytes = array of Byte;
  {$ENDIF ~COMPILER11_UP}

{$IFDEF CLR}
type
  TJclBytes = TBytes;
{$ELSE}
// Redefinition of PByteArray to avoid range check exceptions.
type
  TJclByteArray = array [0..MaxInt div SizeOf(Byte) - 1] of Byte;
  PJclByteArray = ^TJclByteArray;
  TJclBytes = Pointer; // under .NET System.pas: TBytes = array of Byte;

// Redefinition of TULargeInteger to relieve dependency on Windows.pas
type
  PULargeInteger = ^TULargeInteger;
  TULargeInteger = record
    case Integer of
    0:
     (LowPart: LongWord;
      HighPart: LongWord);
    1:
     (QuadPart: Int64);
  end;
{$ENDIF ~CLR}

// Dynamic Array support
type
  TDynByteArray       = array of Byte;
  TDynShortIntArray   = array of Shortint;
  TDynWordArray       = array of Word;
  TDynSmallIntArray   = array of Smallint;
  TDynLongIntArray    = array of Longint;
  TDynInt64Array      = array of Int64;
  TDynCardinalArray   = array of Cardinal;
  TDynIntegerArray    = array of Integer;
  TDynExtendedArray   = array of Extended;
  TDynDoubleArray     = array of Double;
  TDynSingleArray     = array of Single;
  TDynFloatArray      = array of Float;
  {$IFNDEF CLR}
  TDynPointerArray    = array of Pointer;
  {$ENDIF ~CLR}
  TDynStringArray     = array of string;
  TDynAnsiStringArray = array of AnsiString;
  TDynWideStringArray = array of WideString;
  TDynIInterfaceArray = array of IInterface;
  TDynObjectArray     = array of TObject;

// Cross-Platform Compatibility
const
  // (rom) too basic for JclStrings
  AnsiLineFeed       = AnsiChar(#10);
  AnsiCarriageReturn = AnsiChar(#13);
  AnsiCrLf           = AnsiString(#13#10);
  {$IFDEF MSWINDOWS}
  AnsiLineBreak = AnsiCrLf;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  AnsiLineBreak = AnsiLineFeed;
  {$ENDIF UNIX}

  AnsiSigns                  = ['-', '+'];
  AnsiUppercaseLetters       = ['A'..'Z'];
  AnsiLowercaseLetters       = ['a'..'z'];
  AnsiLetters                = ['A'..'Z', 'a'..'z'];
  AnsiDecDigits              = ['0'..'9'];
  AnsiOctDigits              = ['0'..'7'];
  AnsiHexDigits              = ['0'..'9', 'A'..'F', 'a'..'f'];
  AnsiValidIdentifierLetters = ['0'..'9', 'A'..'Z', 'a'..'z', '_'];

  AnsiHexPrefixPascal = AnsiString('$');
  AnsiHexPrefixC      = AnsiString('0x');

  {$IFDEF BCB}
  AnsiHexPrefix = AnsiHexPrefixC;
  {$ELSE ~BCB}
  AnsiHexPrefix = AnsiHexPrefixPascal;
  {$ENDIF ~BCB}

// basic set types
type
  TSetOfAnsiChar = set of AnsiChar;

{$IFNDEF XPLATFORM_RTL}
procedure RaiseLastOSError;
{$ENDIF ~XPLATFORM_RTL}

procedure MoveArray(var List: TDynIInterfaceArray; FromIndex, ToIndex, Count: Integer); overload;
{$IFNDEF CLR}
procedure MoveArray(var List: TDynStringArray; FromIndex, ToIndex, Count: Integer); overload;
procedure MoveArray(var List: TDynFloatArray; FromIndex, ToIndex, Count: Integer); overload;
procedure MoveArray(var List: TDynPointerArray; FromIndex, ToIndex, Count: Integer); overload;
{$ENDIF ~CLR}
{$IFNDEF FPC}
procedure MoveArray(var List: TDynAnsiStringArray; FromIndex, ToIndex, Count: Integer); overload;
{$ENDIF}
procedure MoveArray(var List: TDynWideStringArray; FromIndex, ToIndex, Count: Integer); overload;
procedure MoveArray(var List: TDynObjectArray; FromIndex, ToIndex, Count: Integer); overload;
procedure MoveArray(var List: TDynSingleArray; FromIndex, ToIndex, Count: Integer); overload;
procedure MoveArray(var List: TDynDoubleArray; FromIndex, ToIndex, Count: Integer); overload;
{$IFNDEF FPC}
procedure MoveArray(var List: TDynExtendedArray; FromIndex, ToIndex, Count: Integer); overload;
{$ENDIF}
procedure MoveArray(var List: TDynIntegerArray; FromIndex, ToIndex, Count: Integer); overload;
procedure MoveArray(var List: TDynCardinalArray; FromIndex, ToIndex, Count: Integer); overload;
procedure MoveArray(var List: TDynInt64Array; FromIndex, ToIndex, Count: Integer); overload;
procedure MoveChar(const Source: string; FromIndex: Integer;
  var Dest: string; ToIndex, Count: Integer); overload; // Index: 0..n-1

{$IFDEF CLR}
function GetBytesEx(const Value): TBytes;
procedure SetBytesEx(var Value; Bytes: TBytes);
procedure SetIntegerSet(var DestSet: TIntegerSet; Value: UInt32); inline;

function AnsiByteArrayStringLen(Data: TBytes): Integer;
function StringToAnsiByteArray(const S: string): TBytes;
function AnsiByteArrayToString(const Data: TBytes; Count: Integer): string;

type
  TStringAnsiBufferStreamHelper = class helper for TStream
  public
    function WriteStringAnsiBuffer(const Buffer: string): Integer; overload;
    function WriteStringAnsiBuffer(const Buffer: string; StrLen: Integer): Integer; overload;
    function ReadStringAnsiBuffer(var Buffer: string; AnsiLen: Integer): Integer; overload;

    function WriteStringAnsiBuffer(const Buffer: AnsiString): Integer; overload;
    function WriteStringAnsiBuffer(const Buffer: AnsiString; StrLen: Integer): Integer; overload;
    function ReadStringAnsiBuffer(var Buffer: AnsiString; AnsiLen: Integer): Integer; overload;
  end;
{$ENDIF CLR}

{$IFNDEF CLR}
function BytesOf(const Value: AnsiString): TBytes; overload;
{$IFDEF COMPILER6_UP}
function BytesOf(const Value: WideString): TBytes; overload;
function BytesOf(const Value: WideChar): TBytes; overload;
{$ENDIF COMPILER6_UP}
function BytesOf(const Value: AnsiChar): TBytes; overload;
function StringOf(const Bytes: array of Byte): AnsiString; overload;
function StringOf(const Bytes: Pointer; Size: Cardinal): AnsiString; overload;
{$ENDIF CLR}

{$IFNDEF COMPILER11_UP}
type // Definitions for 32 Bit Compilers
  // From BaseTsd.h
  INT_PTR = Integer;
  {$EXTERNALSYM INT_PTR}
  LONG_PTR = Longint;
  {$EXTERNALSYM LONG_PTR}
  UINT_PTR = Cardinal;
  {$EXTERNALSYM UINT_PTR}
  ULONG_PTR = LongWord;
  {$EXTERNALSYM ULONG_PTR}
  DWORD_PTR = ULONG_PTR;
  {$EXTERNALSYM DWORD_PTR}

{$ENDIF COMPILER11_UP}


type
  TJclAddr64 = Int64;
  TJclAddr32 = Cardinal;

  {$IFDEF 64BIT}
  TJclAddr = TJclAddr64;
  {$ELSE ~64BIT}
  TJclAddr = TJclAddr32;
  {$ENDIF}

  EJclAddr64Exception = class(EJclError);

function Addr64ToAddr32(const Value: TJclAddr64): TJclAddr32;
function Addr32ToAddr64(const Value: TJclAddr32): TJclAddr64;

{$IFDEF SUPPORTS_GENERICS}
type
  TCompare<T> = function(const Obj1, Obj2: T): Integer;
  TEqualityCompare<T> = function(const Obj1, Obj2: T): Boolean;
  THashConvert<T> = function(const AItem: T): Integer;

  TEquatable<T: class> = class(TObject, IEquatable<T>, IEqualityComparer<T>)
  public
    { IEquatable<T> }
    function Equals(Other: T): Boolean; overload;
    { IEqualityComparer<T> }
    function Equals(A, B: T): Boolean; overload;
    function GetHashCode2(Obj: T): Integer;
    function IEqualityComparer<T>.GetHashCode = GetHashCode2;
  end;

  TJclBase<T> = class
  public
    type
      TDynArray = array of T;
  public
    class procedure MoveArray(var List: TDynArray; FromIndex, ToIndex, Count: Integer);
  end;
{$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclBase.pas $';
    Revision: '$Revision: 2405 $';
    Date: '$Date: 2008-07-29 20:25:06 +0200 (mar., 29 juil. 2008) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  JclResources;

procedure MoveArray(var List: TDynIInterfaceArray; FromIndex, ToIndex, Count: Integer); overload;
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
        List[FromIndex + I] := nil
    else
      // independant
      for I := 0 to Count - 1 do
        List[FromIndex + I] := nil;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
        List[FromIndex + I] := nil
    else
      // independant
      for I := 0 to Count - 1 do
        List[FromIndex + I] := nil;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

{$IFNDEF CLR}
procedure MoveArray(var List: TDynStringArray; FromIndex, ToIndex, Count: Integer); overload;
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;

procedure MoveArray(var List: TDynFloatArray; FromIndex, ToIndex, Count: Integer); overload;
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Clean array }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;

procedure MoveArray(var List: TDynPointerArray; FromIndex, ToIndex, Count: Integer); overload;
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Clean array }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF ~CLR}

{$IFNDEF FPC}
procedure MoveArray(var List: TDynAnsiStringArray; FromIndex, ToIndex, Count: Integer); overload;
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
        List[FromIndex + I] := ''
    else
      // independant
      for I := 0 to Count - 1 do
        List[FromIndex + I] := '';
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
        List[FromIndex + I] := ''
    else
      // independant
      for I := 0 to Count - 1 do
        List[FromIndex + I] := '';
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}
{$ENDIF FPC}

procedure MoveArray(var List: TDynWideStringArray; FromIndex, ToIndex, Count: Integer); overload;
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
        List[FromIndex + I] := ''
    else
      // independant
      for I := 0 to Count - 1 do
        List[FromIndex + I] := '';
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
        List[FromIndex + I] := ''
    else
      // independant
      for I := 0 to Count - 1 do
        List[FromIndex + I] := '';
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure MoveArray(var List: TDynObjectArray; FromIndex, ToIndex, Count: Integer); overload;
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
        List[FromIndex + I] := nil
    else
      // independant
      for I := 0 to Count - 1 do
        List[FromIndex + I] := nil;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
        List[FromIndex + I] := nil
    else
      // independant
      for I := 0 to Count - 1 do
        List[FromIndex + I] := nil;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Clean array }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure MoveArray(var List: TDynSingleArray; FromIndex, ToIndex, Count: Integer); overload;
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
        List[FromIndex + I] := 0.0
    else
      // independant
      for I := 0 to Count - 1 do
        List[FromIndex + I] := 0.0;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
        List[FromIndex + I] := 0.0
    else
      // independant
      for I := 0 to Count - 1 do
        List[FromIndex + I] := 0.0;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Clean array }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure MoveArray(var List: TDynDoubleArray; FromIndex, ToIndex, Count: Integer); overload;
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
        List[FromIndex + I] := 0.0
    else
      // independant
      for I := 0 to Count - 1 do
        List[FromIndex + I] := 0.0;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
        List[FromIndex + I] := 0.0
    else
      // independant
      for I := 0 to Count - 1 do
        List[FromIndex + I] := 0.0;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Clean array }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

{$IFNDEF FPC}
procedure MoveArray(var List: TDynExtendedArray; FromIndex, ToIndex, Count: Integer); overload;
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
        List[FromIndex + I] := 0.0
    else
      // independant
      for I := 0 to Count - 1 do
        List[FromIndex + I] := 0.0;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
        List[FromIndex + I] := 0.0
    else
      // independant
      for I := 0 to Count - 1 do
        List[FromIndex + I] := 0.0;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Clean array }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}
{$ENDIF FPC}

procedure MoveArray(var List: TDynIntegerArray; FromIndex, ToIndex, Count: Integer); overload;
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
        List[FromIndex + I] := 0
    else
      // independant
      for I := 0 to Count - 1 do
        List[FromIndex + I] := 0;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
        List[FromIndex + I] := 0
    else
      // independant
      for I := 0 to Count - 1 do
        List[FromIndex + I] := 0;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Clean array }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure MoveArray(var List: TDynCardinalArray; FromIndex, ToIndex, Count: Integer); overload;
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
        List[FromIndex + I] := 0
    else
      // independant
      for I := 0 to Count - 1 do
        List[FromIndex + I] := 0;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
        List[FromIndex + I] := 0
    else
      // independant
      for I := 0 to Count - 1 do
        List[FromIndex + I] := 0;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Clean array }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure MoveArray(var List: TDynInt64Array; FromIndex, ToIndex, Count: Integer); overload;
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
        List[FromIndex + I] := 0
    else
      // independant
      for I := 0 to Count - 1 do
        List[FromIndex + I] := 0;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
        List[FromIndex + I] := 0
    else
      // independant
      for I := 0 to Count - 1 do
        List[FromIndex + I] := 0;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Clean array }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure MoveChar(const Source: string; FromIndex: Integer;
  var Dest: string; ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
  Buf: array of Char;
begin
  Buf := Dest.ToCharArray;
  if FromIndex <= ToIndex then
    for I := 0 to Count - 1 do
      Buf[ToIndex + I] := Source[FromIndex + I]
  else
    for I := Count - 1 downto 0 do
      Buf[ToIndex + I] := Source[FromIndex + I];
  Dest := System.String.Create(Buf);
{$ELSE}
begin
  Move(Source[FromIndex + 1], Dest[ToIndex + 1], Count * SizeOf(Char));
{$ENDIF CLR}
end;

{$IFDEF CLR}

function GetBytesEx(const Value): TBytes;
begin
  if TObject(Value) is TBytes then
    Result := Copy(TBytes(Value))
  else
  if TObject(Value) is TDynByteArray then
    Result := Copy(TDynByteArray(Value))
  else
  if TObject(Value) is System.Enum then // e.g. TIntegerSet
    BitConverter.GetBytes(UInt32(Value))
  { TODO : Add further types }
  else
    raise EJclError.CreateFmt(RsEGetBytesExFmt, [TObject(Value).GetType.FullName]);
end;

procedure SetBytesEx(var Value; Bytes: TBytes);
begin
  if TObject(Value) is TBytes then
    Value := Copy(Bytes)
  else
  if TObject(Value) is TDynByteArray then
    Value := Copy(Bytes)
  else
  if TObject(Value) is System.Enum then // e.g. TIntegerSet
    Value := BitConverter.ToUInt32(Bytes, 0)
  { TODO : Add further types }
  else
    raise EJclError.CreateFmt(RsESetBytesExFmt, [TObject(Value).GetType.FullName]);
end;

procedure SetIntegerSet(var DestSet: TIntegerSet; Value: UInt32);
begin
  DestSet := TIntegerSet(Value);
end;

function AnsiByteArrayStringLen(Data: TBytes): Integer;
var
  I: Integer;
begin
  for I := 0 to High(Data) do
    if Data[I] = 0 then
    begin
      Result := I + 1;
      Exit;
    end;
  Result := Length(Data);
end;

function StringToAnsiByteArray(const S: string): TBytes;
var
  I: Integer;
  AnsiS: AnsiString;
begin
  AnsiS := S; // convert to AnsiString
  SetLength(Result, Length(AnsiS));
  for I := 0 to High(Result) do
    Result[I] := Byte(AnsiS[I + 1]);
end;

function AnsiByteArrayToString(const Data: TBytes; Count: Integer): string;
var
  I: Integer;
  AnsiS: AnsiString;
begin
  if Length(Data) < Count then
    Count := Length(Data);
  SetLength(AnsiS, Count);
  for I := 0 to Length(AnsiS) - 1 do
    AnsiS[I + 1] := AnsiChar(Data[I]);
  Result := AnsiS; // convert to System.String
end;

// == { TStringAnsiBufferStreamHelper } ======================================

function TStringAnsiBufferStreamHelper.WriteStringAnsiBuffer(const Buffer: string; StrLen: Integer): Integer;
begin
  Result := WriteStringAnsiBuffer(Copy(Buffer, StrLen));
end;

function TStringAnsiBufferStreamHelper.WriteStringAnsiBuffer(const Buffer: string): Integer;
var
  Bytes: TBytes;
begin
  Bytes := StringToAnsiByteArray(Buffer);
  Result := Write(Bytes, Length(Bytes));
end;

function TStringAnsiBufferStreamHelper.ReadStringAnsiBuffer(var Buffer: string; AnsiLen: Integer): Integer;
var
  Bytes: TBytes;
begin
  if AnsiLen > 0 then
  begin
    SetLength(Bytes, AnsiLen);
    Result := Read(Bytes, AnsiLen);
    Buffer := AnsiByteArrayToString(Bytes, Result);
  end
  else
  begin
    Buffer := '';
    Result := 0;
  end;
end;

function TStringAnsiBufferStreamHelper.WriteStringAnsiBuffer(const Buffer: AnsiString; StrLen: Integer): Integer;
begin
  Result := WriteStringAnsiBuffer(Copy(Buffer, StrLen));
end;

function TStringAnsiBufferStreamHelper.WriteStringAnsiBuffer(const Buffer: AnsiString): Integer;
var
  Bytes: TBytes;
begin
  Bytes := BytesOf(Buffer);
  Result := Write(Bytes, Length(Bytes));
end;

function TStringAnsiBufferStreamHelper.ReadStringAnsiBuffer(var Buffer: AnsiString; AnsiLen: Integer): Integer;
var
  Bytes: TBytes;
begin
  if AnsiLen > 0 then
  begin
    SetLength(Bytes, AnsiLen);
    Result := Read(Bytes, AnsiLen);
    SetLength(Bytes, Result);
    Buffer := StringOf(Bytes);
  end
  else
  begin
    Buffer := '';
    Result := 0;
  end;
end;

{$ELSE}

function BytesOf(const Value: AnsiString): TBytes;
begin
  SetLength(Result, Length(Value));
  if Value <> '' then
    Move(Pointer(Value)^, Result[0], Length(Value));
end;

{$IFDEF COMPILER6_UP}
function BytesOf(const Value: WideString): TBytes;
begin
  if Value <> '' then
    Result := JclBase.BytesOf(AnsiString(Value))
  else
    SetLength(Result, 0);
end;

function BytesOf(const Value: WideChar): TBytes;
begin
  Result := JclBase.BytesOf(WideString(Value));
end;
{$ENDIF COMPILER6_UP}

function BytesOf(const Value: AnsiChar): TBytes;
begin
  SetLength(Result, 1);
  Result[0] := Byte(Value);
end;

function StringOf(const Bytes: array of Byte): AnsiString;
begin
  if Length(Bytes) > 0 then
  begin
    SetLength(Result, Length(Bytes));
    Move(Bytes[0], Pointer(Result)^, Length(Bytes));
  end
  else
    Result := '';
end;

function StringOf(const Bytes: Pointer; Size: Cardinal): AnsiString;
begin
  if (Bytes <> nil) and (Size > 0) then
  begin
    SetLength(Result, Size);
    Move(Bytes^, Pointer(Result)^, Size);
  end
  else
    Result := '';
end;

{$ENDIF CLR}

//== { EJclWin32Error } ======================================================

{$IFDEF MSWINDOWS}

constructor EJclWin32Error.Create(const Msg: string);
begin
  {$IFDEF CLR}
  inherited Create(''); // this works because the GC cleans the memory
  {$ENDIF CLR}
  FLastError := GetLastError;
  FLastErrorMsg := SysErrorMessage(FLastError);
  inherited CreateFmt(Msg + AnsiLineBreak + RsWin32Prefix, [FLastErrorMsg, FLastError]);
end;

constructor EJclWin32Error.CreateFmt(const Msg: string; const Args: array of const);
begin
  {$IFDEF CLR}
  inherited Create(''); // this works because the GC cleans the memory
  {$ENDIF CLR}
  FLastError := GetLastError;
  FLastErrorMsg := SysErrorMessage(FLastError);
  inherited CreateFmt(Msg + AnsiLineBreak + Format(RsWin32Prefix, [FLastErrorMsg, FLastError]), Args);
end;

{$IFNDEF CLR}
constructor EJclWin32Error.CreateRes(Ident: Integer);
begin
  FLastError := GetLastError;
  FLastErrorMsg := SysErrorMessage(FLastError);
  inherited CreateFmt(LoadStr(Ident) + AnsiLineBreak + RsWin32Prefix, [FLastErrorMsg, FLastError]);
end;

constructor EJclWin32Error.CreateRes(ResStringRec: PResStringRec);
begin
  FLastError := GetLastError;
  FLastErrorMsg := SysErrorMessage(FLastError);
  {$IFDEF FPC}
  inherited CreateFmt(ResStringRec^ + AnsiLineBreak + RsWin32Prefix, [FLastErrorMsg, FLastError]);
  {$ELSE}
  inherited CreateFmt(LoadResString(ResStringRec) + AnsiLineBreak + RsWin32Prefix, [FLastErrorMsg, FLastError]);
  {$ENDIF FPC}
end;
{$ENDIF ~CLR}

{$ENDIF MSWINDOWS}

// Int64 support

procedure I64ToCardinals(I: Int64; var LowPart, HighPart: Cardinal);
begin
  {$IFDEF CLR}
  LowPart := Cardinal(I and $00000000FFFFFFFF);
  HighPart := Cardinal(I shr 32);
  {$ELSE}
  LowPart := TULargeInteger(I).LowPart;
  HighPart := TULargeInteger(I).HighPart;
  {$ENDIF CLR}
end;

procedure CardinalsToI64(var I: Int64; const LowPart, HighPart: Cardinal);
begin
  {$IFDEF CLR}
  I := Int64(HighPart) shl 16 or LowPart;
  {$ELSE}
  TULargeInteger(I).LowPart := LowPart;
  TULargeInteger(I).HighPart := HighPart;
  {$ENDIF CLR}
end;

// Cross Platform Compatibility

{$IFNDEF XPLATFORM_RTL}
procedure RaiseLastOSError;
begin
  RaiseLastWin32Error;
end;
{$ENDIF ~XPLATFORM_RTL}

{$OVERFLOWCHECKS OFF}

function Addr64ToAddr32(const Value: TJclAddr64): TJclAddr32;
begin
  if (Value shr 32) = 0 then
    Result := Value
  else
    {$IFDEF CLR}
    raise EJclAddr64Exception.CreateFmt(RsCantConvertAddr64, [AnsiHexPrefix, Value]);
    {$ELSE ~CLR}
    raise EJclAddr64Exception.CreateResFmt(@RsCantConvertAddr64, [AnsiHexPrefix, Value]);
    {$ENDIF ~CLR}
end;

function Addr32ToAddr64(const Value: TJclAddr32): TJclAddr64;
begin
  Result := Value;
end;

{$IFDEF OVERFLOWCHECKS_ON}
{$OVERFLOWCHECKS ON}
{$ENDIF OVERFLOWCHECKS_ON}

{$IFDEF SUPPORTS_GENERICS}
//=== { TJclBase<T> } ========================================================

class procedure TJclBase<T>.MoveArray(var List: TDynArray; FromIndex, ToIndex, Count: Integer);
var
  I: Integer;
begin
  if FromIndex < ToIndex then
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I]
  else
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
end;

//=== { TEquatable<T> } ======================================================

function TEquatable<T>.Equals(Other: T): Boolean;
begin
  if Other = nil then
    Result := False
  else
    Result := GetHashCode = Other.GetHashCode;
end;

function TEquatable<T>.Equals(A, B: T): Boolean;
begin
  if A = nil then
    Result := B = nil
  else
  if B = nil then
    Result := False
  else
    Result := A.GetHashCode = B.GetHashCode;
end;

function TEquatable<T>.GetHashCode2(Obj: T): Integer;
begin
  if Obj = nil then
    Result := 0
  else
    Result := Obj.GetHashCode;
end;

{$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
