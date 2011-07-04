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
{ The Original Code is JclStrings.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel.                                 }
{ Portions created by Marcel van Brakel are Copyright (C) Marcel van Brakel. All rights reserved.  }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Alexander Radchenko                                                                            }
{   Andreas Hausladen (ahuser)                                                                     }
{   Anthony Steele                                                                                 }
{   Azret Botash                                                                                   }
{   Barry Kelly                                                                                    }
{   Huanlin Tsai                                                                                   }
{   Jack N.A. Bakker                                                                               }
{   Jean-Fabien Connault (cycocrew)                                                                }
{   John C Molyneux                                                                                }
{   Leonard Wennekers                                                                              }
{   Marcel Bestebroer                                                                              }
{   Martin Kimmings                                                                                }
{   Martin Kubecka                                                                                 }
{   Massimo Maria Ghisalberti                                                                      }
{   Matthias Thoma (mthoma)                                                                        }
{   Michael Winter                                                                                 }
{   Nick Hodges                                                                                    }
{   Olivier Sannier (obones)                                                                       }
{   Pelle F. S. Liljendal                                                                          }
{   Petr Vones (pvones)                                                                            }
{   Rik Barker (rikbarker)                                                                         }
{   Robert Lee                                                                                     }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Andreas Schmidt                                                                                }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Various character and string routines (searching, testing and transforming)                      }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-05-21 22:19:06 +0200 (mer., 21 mai 2008)                          $ }
{ Revision:      $Rev:: 2372                                                                     $ }
{ Author:        $Author:: ahuser                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclStrings;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  Classes, SysUtils,
  {$IFDEF CLR}
  System.Text,
  System.IO,
  {$ELSE}
  JclWideStrings,
  {$ENDIF CLR}
  JclBase, JclSysUtils;

// Character constants and sets

const
  // Misc. often used character definitions
  AnsiNull           = Char(#0);
  AnsiSoh            = Char(#1);
  AnsiStx            = Char(#2);
  AnsiEtx            = Char(#3);
  AnsiEot            = Char(#4);
  AnsiEnq            = Char(#5);
  AnsiAck            = Char(#6);
  AnsiBell           = Char(#7);
  AnsiBackspace      = Char(#8);
  AnsiTab            = Char(#9);
  AnsiLineFeed       = JclBase.AnsiLineFeed;
  AnsiVerticalTab    = Char(#11);
  AnsiFormFeed       = Char(#12);
  AnsiCarriageReturn = JclBase.AnsiCarriageReturn;
  AnsiCrLf           = JclBase.AnsiCrLf;
  AnsiSo             = Char(#14);
  AnsiSi             = Char(#15);
  AnsiDle            = Char(#16);
  AnsiDc1            = Char(#17);
  AnsiDc2            = Char(#18);
  AnsiDc3            = Char(#19);
  AnsiDc4            = Char(#20);
  AnsiNak            = Char(#21);
  AnsiSyn            = Char(#22);
  AnsiEtb            = Char(#23);
  AnsiCan            = Char(#24);
  AnsiEm             = Char(#25);
  AnsiEndOfFile      = Char(#26);
  AnsiEscape         = Char(#27);
  AnsiFs             = Char(#28);
  AnsiGs             = Char(#29);
  AnsiRs             = Char(#30);
  AnsiUs             = Char(#31);
  AnsiSpace          = Char(' ');
  AnsiComma          = Char(',');
  AnsiBackslash      = Char('\');
  AnsiForwardSlash   = Char('/');

  AnsiDoubleQuote = Char('"');
  AnsiSingleQuote = Char('''');

  AnsiLineBreak = JclBase.AnsiLineBreak;

// Misc. character sets

  AnsiWhiteSpace             = [AnsiTab, AnsiLineFeed, AnsiVerticalTab,
    AnsiFormFeed, AnsiCarriageReturn, AnsiSpace];
  AnsiSigns                  = ['-', '+'];
  AnsiUppercaseLetters       = JclBase.AnsiUppercaseLetters;
  AnsiLowercaseLetters       = JclBase.AnsiLowercaseLetters;
  AnsiLetters                = JclBase.AnsiLetters;
  AnsiDecDigits              = JclBase.AnsiDecDigits;
  AnsiOctDigits              = JclBase.AnsiOctDigits;
  AnsiHexDigits              = JclBase.AnsiHexDigits;
  AnsiValidIdentifierLetters = JclBase.AnsiValidIdentifierLetters;

const
  // CharType return values
  C1_UPPER  = $0001; // Uppercase
  C1_LOWER  = $0002; // Lowercase
  C1_DIGIT  = $0004; // Decimal digits
  C1_SPACE  = $0008; // Space characters
  C1_PUNCT  = $0010; // Punctuation
  C1_CNTRL  = $0020; // Control characters
  C1_BLANK  = $0040; // Blank characters
  C1_XDIGIT = $0080; // Hexadecimal digits
  C1_ALPHA  = $0100; // Any linguistic character: alphabetic, syllabary, or ideographic

  {$IFDEF MSWINDOWS}
  {$IFDEF SUPPORTS_EXTSYM}
  {$EXTERNALSYM C1_UPPER}
  {$EXTERNALSYM C1_LOWER}
  {$EXTERNALSYM C1_DIGIT}
  {$EXTERNALSYM C1_SPACE}
  {$EXTERNALSYM C1_PUNCT}
  {$EXTERNALSYM C1_CNTRL}
  {$EXTERNALSYM C1_BLANK}
  {$EXTERNALSYM C1_XDIGIT}
  {$EXTERNALSYM C1_ALPHA}
  {$ENDIF SUPPORTS_EXTSYM}
  {$ENDIF MSWINDOWS}

// String Test Routines
function StrIsAlpha(const S: string): Boolean;
function StrIsAlphaNum(const S: string): Boolean;
function StrIsAlphaNumUnderscore(const S: string): Boolean;
{$IFNDEF CLR}
function StrContainsChars(const S: string; Chars: TSysCharSet; CheckAll: Boolean): Boolean;
{$ENDIF ~CLR}
function StrConsistsOfNumberChars(const S: string): Boolean;
function StrIsDigit(const S: string): Boolean;
{$IFNDEF CLR}
function StrIsSubset(const S: string; const ValidChars: TSysCharSet): Boolean;
{$ENDIF ~CLR}
function StrSame(const S1, S2: string): Boolean;

// String Transformation Routines
function StrCenter(const S: string; L: Integer; C: Char  = ' '): string;
function StrCharPosLower(const S: string; CharPos: Integer): string;
function StrCharPosUpper(const S: string; CharPos: Integer): string;
function StrDoubleQuote(const S: string): string;
function StrEnsureNoPrefix(const Prefix, Text: string): string;
function StrEnsureNoSuffix(const Suffix, Text: string): string;
function StrEnsurePrefix(const Prefix, Text: string): string;
function StrEnsureSuffix(const Suffix, Text: string): string;
function StrEscapedToString(const S: string): string;
function StrLower(const S: string): string;
procedure StrLowerInPlace(var S: string);
{$IFNDEF CLR}
procedure StrLowerBuff(S: PChar);
{$ENDIF ~CLR}
procedure StrMove(var Dest: string; const Source: string; const ToIndex,
  FromIndex, Count: Integer);
function StrPadLeft(const S: string; Len: Integer; C: Char = AnsiSpace ): string;
function StrPadRight(const S: string; Len: Integer; C: Char = AnsiSpace ): string;
function StrProper(const S: string): string;
{$IFNDEF CLR}
procedure StrProperBuff(S: PChar);
{$ENDIF ~CLR}
function StrQuote(const S: string; C: Char): string;
function StrRemoveChars(const S: string; const Chars: TSysCharSet): string;
function StrKeepChars(const S: string; const Chars: TSysCharSet): string;
procedure StrReplace(var S: string; const Search, Replace: string; Flags: TReplaceFlags = []);
function StrReplaceChar(const S: string; const Source, Replace: Char): string;
function StrReplaceChars(const S: string; const Chars: TSysCharSet; Replace: Char): string;
function StrReplaceButChars(const S: string; const Chars: TSysCharSet; Replace: Char): string;
function StrRepeat(const S: string; Count: Integer): string;
function StrRepeatLength(const S: string; L: Integer): string;
function StrReverse(const S: string): string;
procedure StrReverseInPlace(var S: string);
function StrSingleQuote(const S: string): string;
function StrSmartCase(const S: string; Delimiters: TSysCharSet): string;
function StrStringToEscaped(const S: string): string;
function StrStripNonNumberChars(const S: string): string;
function StrToHex(const Source: string): string;
function StrTrimCharLeft(const S: string; C: Char): string;
{$IFNDEF CLR}
function StrTrimCharsLeft(const S: string; const Chars: TSysCharSet): string;
{$ENDIF ~CLR}
function StrTrimCharRight(const S: string; C: Char): string;
{$IFNDEF CLR}
function StrTrimCharsRight(const S: string; const Chars: TSysCharSet): string;
{$ENDIF ~CLR}
function StrTrimQuotes(const S: string): string;
function StrUpper(const S: string): string;
procedure StrUpperInPlace(var S: string);
{$IFNDEF CLR}
procedure StrUpperBuff(S: PChar);
{$ENDIF ~CLR}
{$IFDEF WIN32}
function StrOemToAnsi(const S: string): string;
function StrAnsiToOem(const S: string): string;
{$ENDIF WIN32}

{$IFNDEF CLR}
{$IFNDEF SUPPORTS_UNICODE}
{$IFDEF KEEP_DEPRECATED}
// String Management
procedure StrAddRef(var S: string); {$IFDEF SUPPORTS_DEPRECATED} deprecated; {$ENDIF}
function StrAllocSize(const S: string): Longint; {$IFDEF SUPPORTS_DEPRECATED} deprecated; {$ENDIF}
procedure StrDecRef(var S: string); {$IFDEF SUPPORTS_DEPRECATED} deprecated; {$ENDIF}
function StrLength(const S: string): Longint; {$IFDEF SUPPORTS_DEPRECATED} deprecated; {$ENDIF}
function StrRefCount(const S: string): Longint; {$IFDEF SUPPORTS_DEPRECATED} deprecated; {$ENDIF}
{$ENDIF KEEP_DEPRECATED}
{$ENDIF ~SUPPORTS_UNICODE}

{$ENDIF ~CLR}

// String Search and Replace Routines
function StrCharCount(const S: string; C: Char): Integer;
{$IFNDEF CLR}
function StrCharsCount(const S: string; Chars: TSysCharSet): Integer;
{$ENDIF ~CLR}
function StrStrCount(const S, SubS: string): Integer;
function StrCompare(const S1, S2: string): Integer;
function StrCompareRange(const S1, S2: string; const Index, Count: Integer): Integer;
function StrFillChar(const C: Char; Count: Integer): string;
function StrFind(const Substr, S: string; const Index: Integer = 1): Integer;
function StrHasPrefix(const S: string; const Prefixes: array of string): Boolean;
function StrIndex(const S: string; const List: array of string): Integer;
function StrILastPos(const SubStr, S: string): Integer;
function StrIPos(const SubStr, S: string): Integer;
function StrIsOneOf(const S: string; const List: array of string): Boolean;
function StrLastPos(const SubStr, S: string): Integer;
function StrMatch(const Substr, S: string; const Index: Integer = 1): Integer;
function StrMatches(const Substr, S: string; const Index: Integer = 1): Boolean;
function StrNIPos(const S, SubStr: string; N: Integer): Integer;
function StrNPos(const S, SubStr: string; N: Integer): Integer;
function StrPrefixIndex(const S: string; const Prefixes: array of string): Integer;
function StrSearch(const Substr, S: string; const Index: Integer = 1): Integer;

// String Extraction
function StrAfter(const SubStr, S: string): string;
function StrBefore(const SubStr, S: string): string;
function StrBetween(const S: string; const Start, Stop: Char): string;
function StrChopRight(const S: string; N: Integer): string;
function StrLeft(const S: string; Count: Integer): string;
function StrMid(const S: string; Start, Count: Integer): string;
function StrRestOf(const S: string; N: Integer): string;
function StrRight(const S: string; Count: Integer): string;

// Character Test Routines
function CharEqualNoCase(const C1, C2: Char): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsAlpha(const C: Char): Boolean; {$IFDEF CLR} inline; {$ENDIF}
function CharIsAlphaNum(const C: Char): Boolean; {$IFDEF CLR} inline; {$ENDIF}
function CharIsBlank(const C: Char): Boolean; {$IFDEF CLR} inline; {$ENDIF}
function CharIsControl(const C: Char): Boolean; {$IFDEF CLR} inline; {$ENDIF}
function CharIsDelete(const C: Char): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsDigit(const C: Char): Boolean; {$IFDEF CLR} inline; {$ENDIF}
function CharIsLower(const C: Char): Boolean; {$IFDEF CLR} inline; {$ENDIF}
function CharIsNumberChar(const C: Char): Boolean; {$IFDEF CLR} inline; {$ENDIF}
function CharIsPrintable(const C: Char): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsPunctuation(const C: Char): Boolean; {$IFDEF CLR} inline; {$ENDIF}
function CharIsReturn(const C: Char): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharIsSpace(const C: Char): Boolean; {$IFDEF CLR} inline; {$ENDIF}
function CharIsUpper(const C: Char): Boolean; {$IFDEF CLR} inline; {$ENDIF}
function CharIsWhiteSpace(const C: Char): Boolean; {$IFDEF CLR} inline; {$ENDIF}
{$IFNDEF CLR}
function CharType(const C: Char): Word;
{$ENDIF ~CLR}

// Character Transformation Routines
function CharHex(const C: Char): Byte;
function CharLower(const C: Char): Char; {$IFDEF CLR} inline; {$ENDIF}
function CharUpper(const C: Char): Char; {$IFDEF CLR} inline; {$ENDIF}
function CharToggleCase(const C: Char): Char;

// Character Search and Replace
function CharPos(const S: string; const C: Char; const Index: Integer = 1): Integer;
function CharLastPos(const S: string; const C: Char; const Index: Integer = 1): Integer;
function CharIPos(const S: string; C: Char; const Index: Integer = 1 ): Integer;
function CharReplace(var S: string; const Search, Replace: Char): Integer;

{$IFNDEF CLR}
// PCharVector
type
  PCharVector = ^PChar;

function StringsToPCharVector(var Dest: PCharVector; const Source: TStrings): PCharVector;
function PCharVectorCount(Source: PCharVector): Integer;
procedure PCharVectorToStrings(const Dest: TStrings; Source: PCharVector);
procedure FreePCharVector(var Dest: PCharVector);

// MultiSz Routines
type
  PMultiSz = PChar;
  PWideMultiSz = PWideChar;

function StringsToMultiSz(var Dest: PMultiSz; const Source: TStrings): PMultiSz;
procedure MultiSzToStrings(const Dest: TStrings; const Source: PMultiSz);
function MultiSzLength(const Source: PMultiSz): Integer;
procedure AllocateMultiSz(var Dest: PMultiSz; Len: Integer);
procedure FreeMultiSz(var Dest: PMultiSz);
function MultiSzDup(const Source: PMultiSz): PMultiSz;

function WideStringsToWideMultiSz(var Dest: PWideMultiSz; const Source: TWideStrings): PWideMultiSz;
procedure WideMultiSzToWideStrings(const Dest: TWideStrings; const Source: PWideMultiSz);
function WideMultiSzLength(const Source: PWideMultiSz): Integer;
procedure AllocateWideMultiSz(var Dest: PWideMultiSz; Len: Integer);
procedure FreeWideMultiSz(var Dest: PWideMultiSz);
function WideMultiSzDup(const Source: PWideMultiSz): PWideMultiSz;
{$ENDIF ~CLR}

// TStrings Manipulation
procedure StrIToStrings(S, Sep: string; const List: TStrings; const AllowEmptyString: Boolean = True);
procedure StrToStrings(S, Sep: string; const List: TStrings; const AllowEmptyString: Boolean = True);
function StringsToStr(const List: TStrings; const Sep: string; const AllowEmptyString: Boolean = True): string;
procedure TrimStrings(const List: TStrings; DeleteIfEmpty: Boolean = True );
procedure TrimStringsRight(const List: TStrings; DeleteIfEmpty: Boolean = True);
procedure TrimStringsLeft(const List: TStrings; DeleteIfEmpty: Boolean = True );
function AddStringToStrings(const S: string; Strings: TStrings; const Unique: Boolean): Boolean;

// Miscellaneous
{$IFDEF KEEP_DEPRECATED}
function BooleanToStr(B: Boolean): string;
{$ENDIF KEEP_DEPRECATED}
function FileToString(const FileName: string): AnsiString; // AnsiString here because it is binary data
procedure StringToFile(const FileName: string; const Contents: AnsiString; Append: Boolean = False); // AnsiString here because it is binary data
function StrToken(var S: string; Separator: Char): string;
procedure StrTokens(const S: string; const List: TStrings);
procedure StrTokenToStrings(S: string; Separator: Char; const List: TStrings);
{$IFDEF CLR}
function StrWord(const S: string; var Index: Integer; out Word: string): Boolean;
{$ELSE}
function StrWord(var S: PChar; out Word: string): Boolean;
{$ENDIF CLR}
function StrToFloatSafe(const S: string): Float;
function StrToIntSafe(const S: string): Integer;
procedure StrNormIndex(const StrLen: Integer; var Index: Integer; var Count: Integer); overload;

{$IFDEF CLR}
function ArrayOf(List: TStrings): TDynStringArray; overload;
{$ENDIF CLR}

{$IFDEF COMPILER5} // missing Delphi 5 functions
function TryStrToInt(const S: string; out Value: Integer): Boolean;
function TryStrToInt64(const S: string; out Value: Int64): Boolean;
function TryStrToFloat(const S: string; out Value: Extended): Boolean; overload;
function TryStrToFloat(const S: string; out Value: Double): Boolean; overload;
function TryStrToFloat(const S: string; out Value: Single): Boolean; overload;
function TryStrToCurr(const S: string; out Value: Currency): Boolean;
{$ENDIF COMPILER5}


{$IFDEF CLR}
type
  TJclStringBuilder = System.Text.StringBuilder;
  TStringBuilder = TJclStringBuilder;

function DotNetFormat(const Fmt: string; const Args: array of System.Object): string; overload;
function DotNetFormat(const Fmt: string; const Arg0: System.Object): string; overload;
function DotNetFormat(const Fmt: string; const Arg0, Arg1: System.Object): string; overload;
function DotNetFormat(const Fmt: string; const Arg0, Arg1, Arg2: System.Object): string; overload;

{$ELSE ~CLR}

type
  FormatException = class(EJclError);
  ArgumentException = class(EJclError);
  ArgumentNullException = class(EJclError);
  ArgumentOutOfRangeException = class(EJclError);

  IToString = interface
    ['{C4ABABB4-1029-46E7-B5FA-99800F130C05}']
    function ToString: string;
  end;

  TCharDynArray = array of Char;

  // The TStringBuilder class is a Delphi implementation of the .NET
  // System.Text.StringBuilder.
  // It is zero based and the method that allow an TObject (Append, Insert,
  // AppendFormat) are limited to IToString implementors.
  // This class is not threadsafe. Any instance of TStringBuilder should not
  // be used in different threads at the same time.
  TJclStringBuilder = class(TInterfacedObject, IToString)
  private
    FChars: TCharDynArray;
    FLength: Integer;
    FMaxCapacity: Integer;

    function GetCapacity: Integer;
    procedure SetCapacity(const Value: Integer);
    function GetChars(Index: Integer): Char;
    procedure SetChars(Index: Integer; const Value: Char);
    procedure Set_Length(const Value: Integer);
  protected
    function AppendPChar(Value: PChar; Count: Integer; RepeatCount: Integer = 1): TJclStringBuilder;
    function InsertPChar(Index: Integer; Value: PChar; Count: Integer; RepeatCount: Integer = 1): TJclStringBuilder;
  public
    constructor Create(const Value: string; Capacity: Integer = 16); overload;
    constructor Create(Capacity: Integer = 16; MaxCapacity: Integer = MaxInt); overload;
    constructor Create(const Value: string; StartIndex, Length, Capacity: Integer); overload;

    function Append(const Value: string): TJclStringBuilder; overload;
    function Append(const Value: string; StartIndex, Length: Integer): TJclStringBuilder; overload;
    function Append(Value: Boolean): TJclStringBuilder; overload;
    function Append(Value: Char; RepeatCount: Integer = 1): TJclStringBuilder; overload;
    function Append(const Value: array of Char): TJclStringBuilder; overload;
    function Append(const Value: array of Char; StartIndex, Length: Integer): TJclStringBuilder; overload;
    function Append(Value: Cardinal): TJclStringBuilder; overload;
    function Append(Value: Integer): TJclStringBuilder; overload;
    function Append(Value: Double): TJclStringBuilder; overload;
    function Append(Value: Int64): TJclStringBuilder; overload;
    function Append(Obj: TObject): TJclStringBuilder; overload;
    function AppendFormat(const Fmt: string; const Args: array of const): TJclStringBuilder; overload;
    function AppendFormat(const Fmt: string; Arg0: Variant): TJclStringBuilder; overload;
    function AppendFormat(const Fmt: string; Arg0, Arg1: Variant): TJclStringBuilder; overload;
    function AppendFormat(const Fmt: string; Arg0, Arg1, Arg2: Variant): TJclStringBuilder; overload;

    function Insert(Index: Integer; const Value: string; Count: Integer = 1): TJclStringBuilder; overload;
    function Insert(Index: Integer; Value: Boolean): TJclStringBuilder; overload;
    function Insert(Index: Integer; const Value: array of Char): TJclStringBuilder; overload;
    function Insert(Index: Integer; const Value: array of Char; StartIndex, Length: Integer): TJclStringBuilder; overload;
    function Insert(Index: Integer; Value: Cardinal): TJclStringBuilder; overload;
    function Insert(Index: Integer; Value: Integer): TJclStringBuilder; overload;
    function Insert(Index: Integer; Value: Double): TJclStringBuilder; overload;
    function Insert(Index: Integer; Value: Int64): TJclStringBuilder; overload;
    function Insert(Index: Integer; Obj: TObject): TJclStringBuilder; overload;

    function Replace(OldChar, NewChar: Char; StartIndex: Integer = 0; Count: Integer = -1): TJclStringBuilder; overload;
    function Replace(OldValue, NewValue: string; StartIndex: Integer = 0; Count: Integer = -1): TJclStringBuilder; overload;

    function Remove(StartIndex, Length: Integer): TJclStringBuilder;
    function EnsureCapacity(Capacity: Integer): Integer;

    function ToString: string;

    property __Chars__[Index: Integer]: Char read GetChars write SetChars; default;
    property Chars: TCharDynArray read FChars;
    property Length: Integer read FLength write Set_Length;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property MaxCapacity: Integer read FMaxCapacity;
  end;

  TStringBuilder = TJclStringBuilder;

// DotNetFormat() uses the .NET format style: "{argX}"
function DotNetFormat(const Fmt: string; const Args: array of const): string; overload;
function DotNetFormat(const Fmt: string; const Arg0: Variant): string; overload;
function DotNetFormat(const Fmt: string; const Arg0, Arg1: Variant): string; overload;
function DotNetFormat(const Fmt: string; const Arg0, Arg1, Arg2: Variant): string; overload;

// TJclTabSet
type
  TJclTabSet = class {$IFNDEF CLR}(TInterfacedObject, IToString){$ENDIF}
  private
    FStops: TDynIntegerArray;
    FRealWidth: Integer;
    FWidth: Integer;
    FZeroBased: Boolean;
    procedure CalcRealWidth;
    function GetCount: Integer;
    function GetStops(Index: Integer): Integer;
    function GetTabWidth: Integer;
    function GetZeroBased: Boolean;
    procedure SetStops(Index, Value: Integer);
    procedure SetTabWidth(Value: Integer);
    procedure SetZeroBased(Value: Boolean);
  protected
    function FindStop(Column: Integer): Integer;
    function InternalTabStops: TDynIntegerArray;
    function InternalTabWidth: Integer;
    procedure RemoveAt(Index: Integer);
  public
    constructor Create; overload;
    constructor Create(TabWidth: Integer); overload;
    constructor Create(const Tabstops: array of Integer; ZeroBased: Boolean); overload;
    constructor Create(const Tabstops: array of Integer; ZeroBased: Boolean; TabWidth: Integer); overload;

    // Tab stops manipulation
    function Add(Column: Integer): Integer;
    function Delete(Column: Integer): Integer;

    // Usage
    function Expand(const S: string): string; overload;
    function Expand(const S: string; Column: Integer): string; overload;
    procedure OptimalFillInfo(StartColumn, TargetColumn: Integer; out TabsNeeded, SpacesNeeded: Integer);
    function Optimize(const S: string): string; overload;
    function Optimize(const S: string; Column: Integer): string; overload;
    function StartColumn: Integer;
    function TabFrom(Column: Integer): Integer;
    function UpdatePosition(const S: string): Integer; overload;
    function UpdatePosition(const S: string; Column: Integer): Integer; overload;
    function UpdatePosition(const S: string; var Column, Line: Integer): Integer; overload;

    // Conversions
    function ToString: string; overload;
    function ToString(FormattingOptions: Integer): string; overload;
    class function FromString(const S: string): TJclTabSet; {$IFDEF SUPPORTS_STATIC} static; {$ENDIF}

    // Properties
    property ActualTabWidth: Integer read InternalTabWidth;
    property Count: Integer read GetCount;
    property TabStops[Index: Integer]: Integer read GetStops write SetStops; default;
    property TabWidth: Integer read GetTabWidth write SetTabWidth;
    property ZeroBased: Boolean read GetZeroBased write SetZeroBased;
  end;

// Formatting constants
const
  TabSetFormatting_SurroundStopsWithBrackets = 1;
  TabSetFormatting_EmptyBracketsIfNoStops = 2;
  TabSetFormatting_NoTabStops = 4;
  TabSetFormatting_NoTabWidth = 8;
  TabSetFormatting_AutoTabWidth = 16;
  // common combinations
  TabSetFormatting_Default = 0;
  TabSetFormatting_AlwaysUseBrackets =  TabSetFormatting_SurroundStopsWithBrackets or
                                        TabSetFormatting_EmptyBracketsIfNoStops;
  TabSetFormatting_Full = TabSetFormatting_AlwaysUseBrackets or TabSetFormatting_AutoTabWidth;
  // aliases
  TabSetFormatting_StopsOnly = TabSetFormatting_NoTabWidth;
  TabSetFormatting_TabWidthOnly = TabSetFormatting_NoTabStops;
  TabSetFormatting_StopsWithoutBracketsAndTabWidth = TabSetFormatting_Default;

// Tab expansion routines
function StrExpandTabs(S: string): string; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}overload;
function StrExpandTabs(S: string; TabWidth: Integer): string; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}overload;
function StrExpandTabs(S: string; TabSet: TJclTabSet): string; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}overload;
// Tab optimization routines
function StrOptimizeTabs(S: string): string; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}overload;
function StrOptimizeTabs(S: string; TabWidth: Integer): string; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}overload;
function StrOptimizeTabs(S: string; TabSet: TJclTabSet): string; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}overload;

// move to JclBase?
type
  NullReferenceException = class (EJclError)
  public
    constructor Create; overload;
  end;

function AnsiCompareNaturalStr(const S1, S2: AnsiString): Integer;
function AnsiCompareNaturalText(const S1, S2: AnsiString): Integer;

{$ENDIF ~CLR}

procedure StrResetLength(var S: WideString); overload;
procedure StrResetLength(var S: AnsiString); overload;
procedure StrResetLength(S: TJclStringBuilder); overload;

// Exceptions
type
  EJclStringError = EJclError;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclStrings.pas $';
    Revision: '$Revision: 2372 $';
    Date: '$Date: 2008-05-21 22:19:06 +0200 (mer., 21 mai 2008) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF CLR}
  System.Globalization,
  {$ENDIF CLR}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  {$IFDEF SUPPORTS_UNICODE}
  StrUtils,
  {$ENDIF SUPPORTS_UNICODE}
  JclLogic, JclResources, JclStreams;

//=== Internal ===============================================================

{$IFNDEF CLR}

{$IFNDEF SUPPORTS_UNICODE}
type
  TStrRec = packed record
    AllocSize: Longint;
    RefCount: Longint;
    Length: Longint;
  end;
{$ENDIF ~SUPPORTS_UNICODE}

const
  MaxStrCharCount   = Ord(High(Char)) + 1;       // # of chars in one set
  StrLoOffset       = MaxStrCharCount * 0;       // offset to lower case chars
  StrUpOffset       = MaxStrCharCount * 1;       // offset to upper case chars
  StrReOffset       = MaxStrCharCount * 2;       // offset to reverse case chars
  StrCaseMapSize    = MaxStrCharCount * 3;       // # of chars is a table
  {$IFNDEF SUPPORTS_UNICODE}
  StrRecSize        = SizeOf(TStrRec);           // size of the string header rec
  StrAllocOffset    = 12;                        // offset to AllocSize in StrRec
  StrRefCountOffset = 8;                         // offset to RefCount in StrRec
  StrLengthOffset   = 4;                         // offset to Length in StrRec
  {$ENDIF ~SUPPORTS_UNICODE}

var
  StrCaseMap: array [0..StrCaseMapSize - 1] of Char; // case mappings
  StrCaseMapReady: Boolean = False;         // true if case map exists
  StrCharTypes: array [Char] of Word;

procedure LoadCharTypes;
var
  CurrChar: Char;
  CurrType: Word;
  {$IFDEF CLR}
  Category: System.Globalization.UnicodeCategory;
  {$ENDIF CLR}
begin
  for CurrChar := Low(CurrChar) to High(CurrChar) do
  begin
    {$IFDEF MSWINDOWS}
    GetStringTypeEx(LOCALE_USER_DEFAULT, CT_CTYPE1, @CurrChar, 1, CurrType);
    {$DEFINE CHAR_TYPES_INITIALIZED}
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    CurrType := 0;
    if isupper(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_UPPER;
    if islower(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_LOWER;
    if isdigit(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_DIGIT;
    if isspace(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_SPACE;
    if ispunct(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_PUNCT;
    if iscntrl(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_CNTRL;
    if isblank(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_BLANK;
    if isxdigit(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_XDIGIT;
    if isalpha(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_ALPHA;
    {$DEFINE CHAR_TYPES_INITIALIZED}
    {$ENDIF LINUX}
    StrCharTypes[CurrChar] := CurrType;
    {$IFNDEF CHAR_TYPES_INITIALIZED}
    Implement case map initialization here
    {$ENDIF ~CHAR_TYPES_INITIALIZED}
  end;
end;

procedure LoadCaseMap;
var
  CurrChar, UpCaseChar, LoCaseChar, ReCaseChar: Char;
begin
  if not StrCaseMapReady then
  begin
    for CurrChar := Low(Char) to High(Char) do
    begin
      {$IFDEF MSWINDOWS}
      LoCaseChar := CurrChar;
      UpCaseChar := CurrChar;
      Windows.CharLowerBuff(@LoCaseChar, 1);
      Windows.CharUpperBuff(@UpCaseChar, 1);
      {$DEFINE CASE_MAP_INITIALIZED}
      {$ENDIF MSWINDOWS}
      {$IFDEF LINUX}
      LoCaseChar := Char(tolower(Byte(CurrChar)));
      UpCaseChar := Char(toupper(Byte(CurrChar)));
      {$DEFINE CASE_MAP_INITIALIZED}
      {$ENDIF LINUX}
      {$IFNDEF CASE_MAP_INITIALIZED}
      Implement case map initialization here
      {$ENDIF ~CASE_MAP_INITIALIZED}
      if CharIsUpper(CurrChar) then
        ReCaseChar := LoCaseChar
      else
        if CharIsLower(CurrChar) then
          ReCaseChar := UpCaseChar
        else
          ReCaseChar := CurrChar;
      StrCaseMap[Ord(CurrChar) + StrLoOffset] := LoCaseChar;
      StrCaseMap[Ord(CurrChar) + StrUpOffset] := UpCaseChar;
      StrCaseMap[Ord(CurrChar) + StrReOffset] := ReCaseChar;
    end;
    StrCaseMapReady := True;
  end;
end;
{$ENDIF ~CLR}

// Uppercases or Lowercases a give string depending on the
// passed offset. (UpOffset or LoOffset)

{$IFDEF CLR}
const
  StrLoOffset    = 0;
  StrUpOffset    = 1;

procedure StrCase(var Str: string; const Offset: Integer);
begin
  if Offset = StrUpOffset then
    Str := Str.ToUpper
  else
    Str := Str.ToLower;
end;
{$ELSE}
procedure StrCase(var Str: string; const Offset: Integer);
{$IFDEF SUPPORTS_UNICODE}
var
  Len: Integer;
  RetValue: string;
begin
  case Offset of
    StrUpOffset:
      begin
        Len := LCMapString(LOCALE_USER_DEFAULT, LCMAP_UPPERCASE, PChar(Str), Length(Str), nil, 0);
        SetLength(RetValue, Len);
        if Len > 0 then
          LCMapString(LOCALE_USER_DEFAULT, LCMAP_UPPERCASE, PChar(Str), Length(Str), PChar(RetValue), Len);
      end;
    StrLoOffset:
      begin
        Len := LCMapString(LOCALE_USER_DEFAULT, LCMAP_LOWERCASE, PChar(Str), Length(Str), nil, 0);
        SetLength(RetValue, Len);
        if Len > 0 then
          LCMapString(LOCALE_USER_DEFAULT, LCMAP_LOWERCASE, PChar(Str), Length(Str), PChar(RetValue), Len);
      end
  else
    Assert(False, 'StrReOffset not supported');
    Exit;
  end;
  Str := RetValue;
end;
{$ELSE}
asm
        // make sure that the string is not null

        TEST    EAX, EAX
        JZ      @@StrIsNull

        // create unique string if this one is ref-counted

        PUSH    EDX
        CALL    UniqueString
        POP     EDX

        // make sure that the new string is not null

        TEST    EAX, EAX
        JZ      @@StrIsNull

        // get the length, and prepare the counter

        MOV     ECX, [EAX - StrRecSize].TStrRec.Length
        DEC     ECX
        JS      @@StrIsNull

        // ebx will hold the case map, esi pointer to Str

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        // load case map and prepare variables }

        {$IFDEF PIC}
        LEA     EBX, [EBX][StrCaseMap + EDX]
        {$ELSE}
        LEA     EBX, [StrCaseMap + EDX]
        {$ENDIF PIC}
        MOV     ESI, EAX
        XOR     EDX, EDX
        XOR     EAX, EAX

@@NextChar:
        // get current char from the string

        MOV     DL, [ESI]

        // get corresponding char from the case map

        MOV     AL, [EBX + EDX]

        // store it back in the string

        MOV     [ESI], AL

        // update the loop counter and check the end of stirng

        DEC     ECX
        JL      @@Done

        // do the same thing with next 3 chars

        MOV     DL, [ESI + 1]
        MOV     AL, [EBX + EDX]
        MOV     [ESI + 1], AL

        DEC     ECX
        JL      @@Done
        MOV     DL, [ESI + 2]
        MOV     AL, [EBX+EDX]
        MOV     [ESI + 2], AL

        DEC     ECX
        JL      @@Done
        MOV     DL, [ESI + 3]
        MOV     AL, [EBX + EDX]
        MOV     [ESI + 3], AL

        // point string to next 4 chars

        ADD     ESI, 4

        // update the loop counter and check the end of stirng

        DEC     ECX
        JGE     @@NextChar

@@Done:
        POP     EDI
        POP     ESI
        POP     EBX

@@StrIsNull:
end;
{$ENDIF SUPPORTS_UNICODE}
{$ENDIF CLR}

{$IFNDEF CLR}
// Internal utility function
// Uppercases or Lowercases a give null terminated string depending on the
// passed offset. (UpOffset or LoOffset)

procedure StrCaseBuff(S: PChar; const Offset: Integer);
{$IFDEF SUPPORTS_UNICODE}
var
  Len, SLen: Integer;
  RetValue: string;
begin
  if S <> nil then
  begin
    SLen := StrLen(S);
    case Offset of
      StrUpOffset:
        begin
          Len := LCMapString(LOCALE_USER_DEFAULT, LCMAP_UPPERCASE, S, SLen, nil, 0);
          SetLength(RetValue, Len);
          if Len > 0 then
            LCMapString(LOCALE_USER_DEFAULT, LCMAP_UPPERCASE, S, SLen, PChar(RetValue), Len);
        end;
      StrLoOffset:
        begin
          Len := LCMapString(LOCALE_USER_DEFAULT, LCMAP_LOWERCASE, S, SLen, nil, 0);
          SetLength(RetValue, Len);
          if Len > 0 then
            LCMapString(LOCALE_USER_DEFAULT, LCMAP_LOWERCASE, S, SLen, PChar(RetValue), Len);
        end
    else
      Assert(False, 'StrReOffset not supported');
      Exit;
    end;
    Move(PChar(RetValue)^, S^, Len * SizeOf(Char));
  end;
end;
{$ELSE}
asm
        // make sure the string is not null

        TEST    EAX, EAX
        JZ      @@StrIsNull

        // ebx will hold the case map, esi pointer to Str

        PUSH    EBX
        PUSH    ESI

        // load case map and prepare variables

        {$IFDEF PIC}
        LEA     EBX, [EBX][StrCaseMap + EDX]
        {$ELSE}
        LEA     EBX, [StrCaseMap + EDX]
        {$ENDIF PIC}
        MOV     ESI, EAX
        XOR     EDX, EDX
        XOR     EAX, EAX

@@NextChar:
        // get current char from the string

        MOV     DL, [ESI]

        // check for null char

        TEST    DL, DL
        JZ      @@Done

        // get corresponding char from the case map

        MOV     AL, [EBX + EDX]

        // store it back in the string

        MOV     [ESI], AL

        // do the same thing with next 3 chars

        MOV     DL, [ESI + 1]
        TEST    DL, DL
        JZ      @@Done
        MOV     AL, [EBX+EDX]
        MOV     [ESI + 1], AL

        MOV     DL, [ESI + 2]
        TEST    DL, DL
        JZ      @@Done
        MOV     AL, [EBX+EDX]
        MOV     [ESI + 2], AL

        MOV     DL, [ESI + 3]
        TEST    DL, DL
        JZ      @@Done
        MOV     AL, [EBX+EDX]
        MOV     [ESI + 3], AL

        // point string to next 4 chars

        ADD     ESI, 4
        JMP     @@NextChar

@@Done:
        POP     ESI
        POP     EBX

@@StrIsNull:
end;
{$ENDIF SUPPORTS_UNICODE}

function StrEndW(Str: PWideChar): PWideChar;
begin
  Result := Str;
  while Result^ <> #0 do
    Inc(Result);
end;
{$ENDIF ~CLR}

// String Test Routines
function StrIsAlpha(const S: string): Boolean;
var
  I: Integer;
begin
  Result := S <> '';
  for I := 1 to Length(S) do
  begin
    if not CharIsAlpha(S[I]) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function StrIsAlphaNum(const S: string): Boolean;
var
  I: Integer;
begin
  Result := S <> '';
  for I := 1 to Length(S) do
  begin
    if not CharIsAlphaNum(S[I]) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function StrConsistsofNumberChars(const S: string): Boolean;
var
  I: Integer;
begin
  Result := S <> '';
  for I := 1 to Length(S) do
  begin
    if not CharIsNumberChar(S[I]) then
    begin
      Result := False;
      Exit;
    end;
 end;
end;

{$IFNDEF CLR}
function StrContainsChars(const S: string; Chars: TSysCharSet; CheckAll: Boolean): Boolean;
var
  I: Integer;
  C: Char;
begin
  Result := Chars = [];
  if not Result then
  begin
    if CheckAll then
    begin
      for I := 1 to Length(S) do
      begin
        C := S[I];
        if C in Chars then
        begin
          Chars := Chars - [AnsiChar(C)];
          if Chars = [] then
            Break;
        end;
      end;
      Result := (Chars = []);
    end
    else
    begin
      for I := 1 to Length(S) do
        if S[I] in Chars then
        begin
          Result := True;
          Break;
        end;
    end;
  end;
end;
{$ENDIF ~CLR}

function StrIsAlphaNumUnderscore(const S: string): Boolean;
var
  I: Integer;
  C: Char;
begin
  for I := 1 to Length(S) do
  begin
    C := S[I];

    if not (CharIsAlphaNum(C) or (C = '_')) then
    begin
      Result := False;
      Exit;
    end;
  end;

  Result := True and (Length(S) > 0);
end;

function StrIsDigit(const S: string): Boolean;
var
  I: Integer;
begin
  Result := S <> '';
  for I := 1 to Length(S) do
  begin
    if not CharIsDigit(S[I]) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

{$IFNDEF CLR}
function StrIsSubset(const S: string; const ValidChars: TSysCharSet): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(S) do
  begin
    if not (S[I] in ValidChars) then
    begin
      Result := False;
      Exit;
    end;
  end;

  Result := True and (Length(S) > 0);
end;
{$ENDIF ~CLR}

function StrSame(const S1, S2: string): Boolean;
begin
  Result := StrCompare(S1, S2) = 0;
end;

//=== String Transformation Routines =========================================

function StrCenter(const S: string; L: Integer; C: Char  = ' '): string;
begin
  if Length(S) < L then
  begin
    Result := StringOfChar(C, (L - Length(S)) div 2) + S;
    Result := Result + StringOfChar(C, L - Length(Result));
  end
  else
    Result := S;
end;

function StrCharPosLower(const S: string; CharPos: Integer): string;
begin
  Result := S;
  if (CharPos > 0) and (CharPos <= Length(S)) then
    Result[CharPos] := CharLower(Result[CharPos]);
end;

function StrCharPosUpper(const S: string; CharPos: Integer): string;
begin
  Result := S;
  if (CharPos > 0) and (CharPos <= Length(S)) then
    Result[CharPos] := CharUpper(Result[CharPos]);
end;

function StrDoubleQuote(const S: string): string;
begin
  Result := AnsiDoubleQuote + S + AnsiDoubleQuote;
end;

function StrEnsureNoPrefix(const Prefix, Text: string): string;
var
  PrefixLen : Integer;
begin
  PrefixLen := Length(Prefix);
  if Copy(Text, 1, PrefixLen) = Prefix then
    Result := Copy(Text, PrefixLen + 1, Length(Text))
  else
    Result := Text;
end;

function StrEnsureNoSuffix(const Suffix, Text: string): string;
var
  SuffixLen : Integer;
  StrLength : Integer;
begin
  SuffixLen := Length(Suffix);
  StrLength := Length(Text);
  if Copy(Text, StrLength - SuffixLen + 1, SuffixLen) = Suffix then
    Result := Copy(Text, 1, StrLength - SuffixLen)
  else
    Result := Text;
end;

function StrEnsurePrefix(const Prefix, Text: string): string;
var
  PrefixLen: Integer;
begin
  PrefixLen := Length(Prefix);
  if Copy(Text, 1, PrefixLen) = Prefix then
    Result := Text
  else
    Result := Prefix + Text;
end;

function StrEnsureSuffix(const Suffix, Text: string): string;
var
  SuffixLen: Integer;
begin
  SuffixLen := Length(Suffix);
  if Copy(Text, Length(Text) - SuffixLen + 1, SuffixLen) = Suffix then
    Result := Text
  else
    Result := Text + Suffix;
end;

function StrEscapedToString(const S: string): string;
var
  I, Len, N, Val: Integer;

  procedure HandleHexEscapeSeq;
  const
    HexDigits = string('0123456789abcdefABCDEF');
  begin
    N := Pos(S[I + 1], HexDigits) - 1;
    if N < 0 then
      // '\x' without hex digit following is not escape sequence
      Result := Result + '\x'
    else
    begin
      Inc(I); // Jump over x
      if N >= 16 then
        N := N - 6;
      Val := N;
      // Same for second digit
      if I < Len then
      begin
        N := Pos(S[I + 1], HexDigits) - 1;
        if N >= 0 then
        begin
          Inc(I); // Jump over first digit
          if N >= 16 then
            N := N - 6;
          Val := Val * 16 + N;
        end;
      end;

      if Val > 255 then
        {$IFDEF CLR}
        raise EJclStringError.Create(RsNumericConstantTooLarge);
        {$ELSE}
        raise EJclStringError.CreateRes(@RsNumericConstantTooLarge);
        {$ENDIF CLR}

      Result := Result + Chr(Val);
    end;
  end;

  procedure HandleOctEscapeSeq;
  const
    OctDigits = string('01234567');
  begin
    // first digit
    Val := Pos(S[I], OctDigits) - 1;
    if I < Len then
    begin
      N := Pos(S[I + 1], OctDigits) - 1;
      if N >= 0 then
      begin
        Inc(I);
        Val := Val * 8 + N;
      end;
      if I < Len then
      begin
        N := Pos(S[I + 1], OctDigits) - 1;
        if N >= 0 then
        begin
          Inc(I);
          Val := Val * 8 + N;
        end;
      end;
    end;

    if Val > 255 then
      {$IFDEF CLR}
      raise EJclStringError.Create(RsNumericConstantTooLarge);
      {$ELSE}
      raise EJclStringError.CreateRes(@RsNumericConstantTooLarge);
      {$ENDIF CLR}

    Result := Result + Chr(Val);
  end;

begin
  Result := '';
  I := 1;
  Len := Length(S);
  while I <= Len do
  begin
    if not ((S[I] = '\') and (I < Len)) then
      Result := Result + S[I]
    else
    begin
      Inc(I); // Jump over escape character
      case S[I] of
        'a':
          Result := Result + AnsiBell;
        'b':
          Result := Result + AnsiBackspace;
        'f':
          Result := Result + AnsiFormFeed;
        'n':
          Result := Result + AnsiLineFeed;
        'r':
          Result := Result + AnsiCarriageReturn;
        't':
          Result := Result + AnsiTab;
        'v':
          Result := Result + AnsiVerticalTab;
        '\':
          Result := Result + '\';
        '"':
          Result := Result + '"';
        '''':
          Result := Result + ''''; // Optionally escaped
        '?':
          Result := Result + '?';  // Optionally escaped
        'x':
          if I < Len then
            // Start of hex escape sequence
            HandleHexEscapeSeq
          else
            // '\x' at end of string is not escape sequence
            Result := Result + '\x';
        '0'..'7':
          // start of octal escape sequence
          HandleOctEscapeSeq;
      else
        // no escape sequence
        Result := Result + '\' + S[I];
      end;
    end;
    Inc(I);
  end;
end;

function StrLower(const S: string): string;
begin
  Result := S;
  StrLowerInPlace(Result);
end;

procedure StrLowerInPlace(var S: string);
{$IFDEF PIC}
begin
  StrCase(S, StrLoOffset);
end;
{$ELSE}
asm
        // StrCase(S, StrLoOffset)

        XOR     EDX, EDX         // MOV     EDX, StrLoOffset
        JMP     StrCase
end;
{$ENDIF PIC}

{$IFNDEF CLR}
procedure StrLowerBuff(S: PChar);
{$IFDEF PIC}
begin
  StrCaseBuff(S, StrLoOffset);
end;
{$ELSE}
asm
        // StrCaseBuff(S, LoOffset)
        XOR     EDX, EDX                // MOV     EDX, LoOffset
        JMP     StrCaseBuff
end;
{$ENDIF PIC}
{$ENDIF ~CLR}

{$IFDEF CLR}
procedure MoveString(const Source: string; SrcIndex: Integer;
  var Dest: string; DstIndex, Count: Integer);
begin
  Dec(SrcIndex);
  Dec(DstIndex);
  Dest := Dest.Remove(DstIndex, Count).Insert(DstIndex, Source.Substring(SrcIndex, Count));
end;
{$ENDIF CLR}

procedure StrMove(var Dest: string; const Source: string;
  const ToIndex, FromIndex, Count: Integer);
begin
  // Check strings
  if (Source = '') or (Length(Dest) = 0) then
    Exit;

  // Check FromIndex
  if (FromIndex <= 0) or (FromIndex > Length(Source)) or
     (ToIndex <= 0) or (ToIndex > Length(Dest)) or
     ((FromIndex + Count - 1) > Length(Source)) or ((ToIndex + Count - 1) > Length(Dest)) then
     { TODO : Is failure without notice the proper thing to do here? }
     Exit;

  // Move
  {$IFDEF CLR}
  MoveString(Source, FromIndex, Dest, ToIndex, Count);
  {$ELSE}
  Move(Source[FromIndex], Dest[ToIndex], Count * SizeOf(Char));
  {$ENDIF CLR}
end;

function StrPadLeft(const S: string; Len: Integer; C: Char): string;
var
  L: Integer;
begin
  L := Length(S);
  if L < Len then
    Result := StringOfChar(C, Len - L) + S
  else
    Result := S;
end;

function StrPadRight(const S: string; Len: Integer; C: Char): string;
var
  L: Integer;
begin
  L := Length(S);
  if L < Len then
    Result := S + StringOfChar(C, Len - L)
  else
    Result := S;
end;

function StrProper(const S: string): string;
begin
  {$IFDEF CLR}
  Result := S.ToLower;
  {$ELSE}
  Result := StrLower(S);
  {$ENDIF CLR}
  if Result <> '' then
    Result[1] := UpCase(Result[1]);
end;

{$IFNDEF CLR}
procedure StrProperBuff(S: PChar);
begin
  if (S <> nil) and (S^ <> #0) then
  begin
    StrLowerBuff(S);
    S^ := CharUpper(S^);
  end;
end;
{$ENDIF ~CLR}

function StrQuote(const S: string; C: Char): string;
var
  L: Integer;
begin
  L := Length(S);
  Result := S;
  if L > 0 then
  begin
    if Result[1] <> C then
    begin
      Result := C + Result;
      Inc(L);
    end;
    if Result[L] <> C then
      Result := Result + C;
  end;
end;

function StrRemoveChars(const S: string; const Chars: TSysCharSet): string;
{$IFDEF CLR}
var
  I: Integer;
  sb: StringBuilder;
begin
  sb := StringBuilder.Create(Length(S));
  for I := 0 to S.Length - 1 do
    if not (AnsiChar(S[I]) in Chars) then
      sb.Append(S[I]);
  Result := sb.ToString();
end;
{$ELSE}
var
  Source, Dest: PChar;
  Len, Index: Integer;
begin
  Len := Length(S);
  SetLength(Result, Len);
  UniqueString(Result);
  Source := PChar(S);
  Dest := PChar(Result);
  for Index := 0 to Len - 1 do
  begin
    if not (Source^ in Chars) then
    begin
      Dest^ := Source^;
      Inc(Dest);
    end;
    Inc(Source);
  end;
  SetLength(Result, Dest - PChar(Result));
end;
{$ENDIF CLR}

function StrKeepChars(const S: string; const Chars: TSysCharSet): string;
{$IFDEF CLR}
var
  I: Integer;
  sb: StringBuilder;
begin
  sb := StringBuilder.Create(Length(S));
  for I := 0 to S.Length - 1 do
    if AnsiChar(S[I]) in Chars then
      sb.Append(S[I]);
  Result := sb.ToString();
end;
{$ELSE}
var
  Source, Dest: PChar;
  Len, Index: Integer;
begin
  Len := Length(S);
  SetLength(Result, Len);
  UniqueString(Result);
  Source := PChar(S);
  Dest := PChar(Result);
  for Index := 0 to Len-1 do
  begin
    if Source^ in Chars then
    begin
      Dest^ := Source^;
      Inc(Dest);
    end;
    Inc(Source);
  end;
  SetLength(Result, Dest - PChar(Result));
end;
{$ENDIF CLR}

function StrRepeat(const S: string; Count: Integer): string;
{$IFDEF CLR}
var
  I, Len: Integer;
  sb: StringBuilder;
begin
  Len := Length(S);
  if Len * Count > 0 then
  begin
    sb := StringBuilder.Create(Len * Count);
    for I := Count - 1 downto 0 do
      sb.Append(S);
    Result := sb.ToString();
  end
  else
    Result := '';
end;
{$ELSE}
var
  Len, Index: Integer;
  Dest, Source: PChar;
begin
  Len := Length(S);
  SetLength(Result, Count * Len);
  Dest := PChar(Result);
  Source := PChar(S);
  if Dest <> nil then
    for Index := 0 to Count - 1 do
    begin
      Move(Source^, Dest^, Len * SizeOf(Char));
      Inc(Dest, Len);
    end;
end;
{$ENDIF CLR}

function StrRepeatLength(const S: string; L: Integer): string;
{$IFDEF CLR}
var
  Count: Integer;
  LenS, Index: Integer;
begin
  Result := '';
  LenS := Length(S);

  if (LenS > 0) and (S <> '') then
  begin
    Count := L div LenS;
    if Count * LenS < L then
      Inc(Count);
    SetLength(Result, Count * LenS);
    Index := 1;
    while Count > 0 do
    begin
      MoveString(S, 1, Result, Index, LenS);
      Inc(Index, LenS);
      Dec(Count);
    end;
    if Length(S) > L then
      SetLength(Result, L);
  end;
end;
{$ELSE}
var
  Len: Integer;
  Dest: PChar;
begin
  Result := '';
  Len := Length(S);

  if (Len > 0) and (S <> '') then
  begin
    SetLength(Result,L);
    Dest := PChar(Result);
    while (L > 0) do
    begin
      Move(S[1], Dest^, Min(L, Len) * SizeOf(Char));
      Inc(Dest, Len);
      Dec(L, Len);
    end;
  end;
end;
{$ENDIF CLR}

procedure StrReplace(var S: string; const Search, Replace: string; Flags: TReplaceFlags);
{$IFDEF CLR}
begin
  S := StringReplace(S, Search, Replace, Flags); // !!! Convertion to System.String
end;
{$ELSE}
var
  SearchStr: string;
  ResultStr: string; { result string }
  SourcePtr: PChar;      { pointer into S of character under examination }
  SourceMatchPtr: PChar; { pointers into S and Search when first character has }
  SearchMatchPtr: PChar; { been matched and we're probing for a complete match }
  ResultPtr: PChar;      { pointer into Result of character being written }
  ResultIndex,
  SearchLength,          { length of search string }
  ReplaceLength,         { length of replace string }
  BufferLength,          { length of temporary result buffer }
  ResultLength: Integer; { length of result string }
  C: Char;               { first character of search string }
  IgnoreCase: Boolean;
begin
  if Search = '' then
  begin
    if S = '' then
    begin
      S := Replace;
      Exit;
    end
    else
      raise EJclStringError.CreateRes(@RsBlankSearchString);
  end;

  if S <> '' then
  begin
    IgnoreCase := rfIgnoreCase in Flags;
    if IgnoreCase then
      SearchStr := AnsiUpperCase(Search)
    else
      SearchStr := Search;
    { avoid having to call Length() within the loop }
    SearchLength := Length(Search);
    ReplaceLength := Length(Replace);
    ResultLength := Length(S);
    BufferLength := ResultLength;
    SetLength(ResultStr, BufferLength);
    { get pointers to begin of source and result }
    ResultPtr := PChar(ResultStr);
    SourcePtr := PChar(S);
    C := SearchStr[1];
    { while we haven't reached the end of the string }
    while True do
    begin
      { copy characters until we find the first character of the search string }
      if IgnoreCase then
        while (CharUpper(SourcePtr^) <> C) and (SourcePtr^ <> #0) do
        begin
          ResultPtr^ := SourcePtr^;
          Inc(ResultPtr);
          Inc(SourcePtr);
        end
      else
        while (SourcePtr^ <> C) and (SourcePtr^ <> #0) do
        begin
          ResultPtr^ := SourcePtr^;
          Inc(ResultPtr);
          Inc(SourcePtr);
        end;
      { did we find that first character or did we hit the end of the string? }
      if SourcePtr^ = #0 then
        Break
      else
      begin
        { continue comparing, +1 because first character was matched already }
        SourceMatchPtr := SourcePtr + 1;
        SearchMatchPtr := PChar(SearchStr) + 1;
        if IgnoreCase then
          while (CharUpper(SourceMatchPtr^) = SearchMatchPtr^) and (SearchMatchPtr^ <> #0) do
          begin
            Inc(SourceMatchPtr);
            Inc(SearchMatchPtr);
          end
        else
          while (SourceMatchPtr^ = SearchMatchPtr^) and (SearchMatchPtr^ <> #0) do
          begin
            Inc(SourceMatchPtr);
            Inc(SearchMatchPtr);
          end;
        { did we find a complete match? }
        if SearchMatchPtr^ = #0 then
        begin
          // keep track of result length
          Inc(ResultLength, ReplaceLength - SearchLength);
          if ReplaceLength > 0 then
          begin
            // increase buffer size if required
            if ResultLength > BufferLength then
            begin
              BufferLength := ResultLength * 2;
              ResultIndex := ResultPtr - PChar(ResultStr) + 1;
              SetLength(ResultStr, BufferLength);
              ResultPtr := @ResultStr[ResultIndex];
            end;
            { append replace to result and move past the search string in source }
            Move((@Replace[1])^, ResultPtr^, ReplaceLength * SizeOf(Char));
          end;
          Inc(SourcePtr, SearchLength);
          Inc(ResultPtr, ReplaceLength);
          { replace all instances or just one? }
          if not (rfReplaceAll in Flags) then
          begin
            { just one, copy until end of source and break out of loop }
            while SourcePtr^ <> #0 do
            begin
              ResultPtr^ := SourcePtr^;
              Inc(ResultPtr);
              Inc(SourcePtr);
            end;
            Break;
          end;
        end
        else
        begin
          { copy current character and start over with the next }
          ResultPtr^ := SourcePtr^;
          Inc(ResultPtr);
          Inc(SourcePtr);
        end;
      end;
    end;
    { set result length and copy result into S }
    SetLength(ResultStr, ResultLength);
    S := ResultStr;
  end;
end;
{$ENDIF CLR}

function StrReplaceChar(const S: string; const Source, Replace: Char): string;
{$IFNDEF CLR}
var
  I: Integer;
{$ENDIF ~CLR}
begin
  {$IFDEF CLR}
  Result := S.Replace(Source, Replace);
  {$ELSE}
  Result := S;
  for I := 1 to Length(S) do
    if Result[I] = Source then
      Result[I] := Replace;
  {$ENDIF CLR}
end;

function StrReplaceChars(const S: string; const Chars: TSysCharSet; Replace: Char): string;
var
  I: Integer;
  {$IFDEF CLR}
  sb: StringBuilder;
  {$ENDIF CLR}
begin
  {$IFDEF CLR}
  sb := StringBuilder.Create(S);
  for I := 0 to sb.Length - 1 do
    if AnsiChar(sb[I]) in Chars then
      sb[I] := Replace;
  Result := sb.ToString();
  {$ELSE}
  Result := S;
  for I := 1 to Length(S) do
    if Result[I] in Chars then
      Result[I] := Replace;
  {$ENDIF CLR}
end;

function StrReplaceButChars(const S: string; const Chars: TSysCharSet;
  Replace: Char): string;
var
  I: Integer;
  {$IFDEF CLR}
  sb: StringBuilder;
  {$ENDIF CLR}
begin
  {$IFDEF CLR}
  sb := StringBuilder.Create(S);
  for I := 0 to sb.Length - 1 do
    if not (AnsiChar(sb[I]) in Chars) then
      sb[I] := Replace;
  Result := sb.ToString();
  {$ELSE}
  Result := S;
  for I := 1 to Length(S) do
    if not (Result[I] in Chars) then
      Result[I] := Replace;
  {$ENDIF CLR}
end;

function StrReverse(const S: string): string;
begin
  Result := S;
  StrReverseInplace(Result);
end;

procedure StrReverseInPlace(var S: string);
{ TODO -oahuser : Warning: This is dangerous for unicode surrogates }
{$IFDEF CLR}
var
  I, LenS: Integer;
  sb: StringBuilder;
begin
  LenS := Length(S);
  sb := StringBuilder.Create(LenS);
  sb.Length := LenS;
  for I := 0 to LenS - 1 do
    sb[I] := S[LenS - I - 1];
  S := sb.ToString();
end;
{$ELSE}
var
  P1, P2: PChar;
  C: Char;
begin
  UniqueString(S);
  P1 := PChar(S);
  P2 := P1 + SizeOf(Char) * (Length(S) - 1);
  while P1 < P2 do
  begin
    C := P1^;
    P1^ := P2^;
    P2^ := C;
    Inc(P1);
    Dec(P2);
  end;
end;
{$ENDIF CLR}

function StrSingleQuote(const S: string): string;
begin
  Result := AnsiSingleQuote + S + AnsiSingleQuote;
end;

function StrSmartCase(const S: string; Delimiters: TSysCharSet): string;
var
  {$IFDEF CLR}
  Index: Integer;
  LenS: Integer;
  sb: StringBuilder;
  {$ELSE}
  Source, Dest: PChar;
  Index, Len: Integer;
  {$ENDIF CLR}
begin
  Result := '';
  if Delimiters = [] then
    Include(Delimiters, AnsiSpace);

  if S <> '' then
  begin
    Result := S;
    {$IFDEF CLR}
    sb := StringBuilder.Create(S);
    LenS := Length(S);
    Index := 0;
    while Index < LenS do
    begin
      if (AnsiChar(sb[Index]) in Delimiters) and (Index + 1 < LenS) and
        not (AnsiChar(sb[Index + 1]) in Delimiters) then
        sb[Index + 1] := CharUpper(sb[Index + 1]);
      Inc(Index);
    end;
    sb[0] := CharUpper(sb[0]);
    Result := sb.ToString();
    {$ELSE}
    UniqueString(Result);

    Len := Length(S);
    Source := PChar(S);
    Dest := PChar(Result);
    Inc(Dest);

    for Index := 2 to Len do
    begin
      if (Source^ in Delimiters) and not (Dest^ in Delimiters) then
        Dest^ := CharUpper(Dest^);
      Inc(Dest);
      Inc(Source);
    end;
    Result[1] := CharUpper(Result[1]);
    {$ENDIF CLR}
  end;
end;

function StrStringToEscaped(const S: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    case S[I] of
      AnsiBackspace:
        Result := Result + '\b';
      AnsiBell:
        Result := Result + '\a';
      AnsiCarriageReturn:
        Result := Result + '\r';
      AnsiFormFeed:
        Result := Result + '\f';
      AnsiLineFeed:
        Result := Result + '\n';
      AnsiTab:
        Result := Result + '\t';
      AnsiVerticalTab:
        Result := Result + '\v';
      '\':
        Result := Result + '\\';
      '"':
        Result := Result + '\"';
    else
      // Characters < ' ' are escaped with hex sequence
      if S[I] < #32 then
        Result := Result + Format('\x%.2x',[Integer(S[I])])
      else
        Result := Result + S[I];
    end;
  end;
end;

function StrStripNonNumberChars(const S: string): string;
var
  I: Integer;
  C: Char;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    C := S[I];
    if CharIsNumberChar(C) then
      Result := Result + C;
  end;
end;

function StrToHex(const Source: string): string;
var
  Index: Integer;
  C, L, N: Integer;
  BL, BH: Byte;
  S: string;
  {$IFDEF CLR}
  sb: StringBuilder;
  {$ENDIF CLR}
begin
  {$IFDEF CLR}
  sb := StringBuilder.Create;
  {$ELSE}
  Result := '';
  {$ENDIF CLR}
  if Source <> '' then
  begin
    S := Source;
    L := Length(S);
    if Odd(L) then
    begin
      S := '0' + S;
      Inc(L);
    end;
    Index := 1;
    {$IFDEF CLR}
    sb.Length := L div 2;
    {$ELSE}
    SetLength(Result, L div 2);
    {$ENDIF CLR}
    C := 1;
    N := 1;
    while C <= L do
    begin
      BH := CharHex(S[Index]);
      Inc(Index);
      BL := CharHex(S[Index]);
      Inc(Index);
      Inc(C, 2);
      if (BH = $FF) or (BL = $FF) then
      begin
        Result := '';
        Exit;
      end;
      {$IFDEF CLR}
      sb[N] :=
      {$ELSE}
      Result[N] :=
      {$ENDIF CLR}
        Char((BH shl 4) + BL);
      Inc(N);
    end;
  end;
  {$IFDEF CLR}
  Result := sb.ToString();
  {$ENDIF CLR}
end;

function StrTrimCharLeft(const S: string; C: Char): string;
var
  I, L: Integer;
begin
  I := 1;
  L := Length(S);
  while (I <= L) and (S[I] = C) do Inc(I);
  Result := Copy(S, I, L - I + 1);
end;

{$IFNDEF CLR}
function StrTrimCharsLeft(const S: string; const Chars: TSysCharSet): string;
var
  I, L: Integer;
begin
  I := 1;
  L := Length(S);
  while (I <= L) and (S[I] in Chars) do Inc(I);
  Result := Copy(S, I, L - I + 1);
end;

function StrTrimCharsRight(const S: string; const Chars: TSysCharSet): string;
var
  I: Integer;
begin
  I := Length(S);
  while (I >= 1) and (S[I] in Chars) do Dec(I);
  Result := Copy(S, 1, I);
end;
{$ENDIF ~CLR}

function StrTrimCharRight(const S: string; C: Char): string;
var
  I: Integer;
begin
  I := Length(S);
  while (I >= 1) and (S[I] = C) do Dec(I);
  Result := Copy(S, 1, I);
end;

function StrTrimQuotes(const S: string): string;
var
  First, Last: Char;
  L: Integer;
begin
  L := Length(S);
  if L > 1 then
  begin
    First := S[1];
    Last := S[L];
    if (First = Last) and ((First = AnsiSingleQuote) or (First = AnsiDoubleQuote)) then
      Result := Copy(S, 2, L - 2)
    else
      Result := S;
  end
  else
    Result := S;
end;

function StrUpper(const S: string): string;
begin
  Result := S;
  StrUpperInPlace(Result);
end;

procedure StrUpperInPlace(var S: string);
{$IFDEF PIC}
begin
  StrCase(S, StrUpOffset);
end;
{$ELSE}
asm
        // StrCase(Str, StrUpOffset)
        MOV     EDX, StrUpOffset
        JMP     StrCase
end;
{$ENDIF PIC}

{$IFNDEF CLR}
procedure StrUpperBuff(S: PChar);
{$IFDEF PIC}
begin
  StrCaseBuff(S, StrUpOffset);
end;
{$ELSE}
asm
        // StrCaseBuff(S, UpOffset)
        MOV     EDX, StrUpOffset
        JMP     StrCaseBuff
end;
{$ENDIF PIC}
{$ENDIF ~CLR}

{$IFDEF WIN32}
function StrOemToAnsi(const S: string): string;
begin
  SetLength(Result, Length(S));
  if S <> '' then
    OemToAnsiBuff(@S[1], @Result[1], Length(S));
end;
{$ENDIF WIN32}

{$IFDEF WIN32}
function StrAnsiToOem(const S: string): string;
begin
  SetLength(Result, Length(S));
  if S <> '' then
    AnsiToOemBuff(@S[1], @Result[1], Length(S));
end;
{$ENDIF WIN32}


{$IFNDEF CLR}
//=== String Management ======================================================

{$IFNDEF SUPPORTS_UNICODE}
{$IFDEF KEEP_DEPRECATED}
procedure StrAddRef(var S: string);
var
  Foo: string;
begin
  if StrRefCount(S) = -1 then
    UniqueString(S)
  else
  begin
    Foo := S;
    Pointer(Foo) := nil;
  end;
end;

function StrAllocSize(const S: string): Longint;
var
  P: Pointer;
begin
  Result := 0;
  if Pointer(S) <> nil then
  begin
    P := Pointer(INT_PTR(Pointer(S)) - StrRefCountOffset);
    if Integer(P^) <> -1 then
    begin
      P := Pointer(INT_PTR(Pointer(S)) - StrAllocOffset);
      Result := Integer(P^);
    end;
  end;
end;

procedure StrDecRef(var S: string);
var
  Foo: string;
begin
  case StrRefCount(S) of
    -1, 0: { nothing } ;
     1:
       begin
         Finalize(S);
         Pointer(S) := nil;
       end;
  else
    Pointer(Foo) := Pointer(S);
  end;
end;

function StrLength(const S: string): Longint;
var
  P: Pointer;
begin
  Result := 0;
  if Pointer(S) <> nil then
  begin
    P := Pointer(INT_PTR(Pointer(S)) - StrLengthOffset);
    Result := Longint(P^) and (not $80000000 shr 1);
  end;
end;

function StrRefCount(const S: string): Longint;
var
  P: Pointer;
begin
  Result := 0;
  if Pointer(S) <> nil then
  begin
    P := Pointer(INT_PTR(Pointer(S)) - StrRefCountOffset);
    Result := Longint(P^);
  end;
end;
{$ENDIF KEEP_DEPRECATED}
{$ENDIF ~SUPPORTS_UNICODE}

{$ENDIF ~CLR}

procedure StrResetLength(var S: WideString);
var
  I: Integer;
begin
  for I := 0 to Length(S) - 1 do
    if S[I + 1] = #0 then
    begin
      SetLength(S, I);
      Exit;
    end;
end;

procedure StrResetLength(var S: AnsiString);
var
  I: Integer;
begin
  for I := 0 to Length(S) - 1 do
    if S[I + 1] = #0 then
    begin
      SetLength(S, I);
      Exit;
    end;
end;

procedure StrResetLength(S: TJclStringBuilder);
var
  I: Integer;
begin
  if S <> nil then
    for I := 0 to S.Length - 1 do
      if S[I] = #0 then
      begin
        S.Length := I;
        Exit;
      end;
end;

//=== String Search and Replace Routines =====================================

function StrCharCount(const S: string; C: Char): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
    if S[I] = C then
      Inc(Result);
end;

{$IFNDEF CLR}
function StrCharsCount(const S: string; Chars: TSysCharSet): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
    if S[I] in Chars then
      Inc(Result);
end;
{$ENDIF ~CLR}

function StrStrCount(const S, SubS: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  if (Length(SubS) > Length(S)) or (Length(SubS) = 0) or (Length(S) = 0) then
    Exit;
  if Length(SubS) = 1 then
  begin
    Result := StrCharCount(S, SubS[1]);
    Exit;
  end;
  I := StrSearch(SubS, S, 1);

  if I > 0 then
    Inc(Result);

  while (I > 0) and (Length(S) > I+Length(SubS)) do
  begin
    I := StrSearch(SubS, S, I+1);

    if I > 0 then
      Inc(Result);
  end
end;

{$IFDEF SUPPORTS_UNICODE}
(*
{ 1}  Test(StrCompareRange('', '', 1, 5), 0);
{ 2}  Test(StrCompareRange('A', '', 1, 5), -1);
{ 3}  Test(StrCompareRange('AB', '', 1, 5), -1);
{ 4}  Test(StrCompareRange('ABC', '', 1, 5), -1);
{ 5}  Test(StrCompareRange('', 'A', 1, 5), -1);
{ 6}  Test(StrCompareRange('', 'AB',  1, 5), -1);
{ 7}  Test(StrCompareRange('', 'ABC', 1, 5), -1);
{ 8}  Test(StrCompareRange('A', 'a', 1, 5), -2);
{ 9}  Test(StrCompareRange('A', 'a', 1, 1), -32);
{10}  Test(StrCompareRange('aA', 'aB', 1, 1), 0);
{11}  Test(StrCompareRange('aA', 'aB', 1, 2), -1);
{12}  Test(StrCompareRange('aB', 'aA', 1, 2), 1);
{13}  Test(StrCompareRange('aA', 'aa', 1, 2), -32);
{14}  Test(StrCompareRange('aa', 'aA', 1, 2), 32);
{15}  Test(StrCompareRange('', '', 1, 0), 0);
{16}  Test(StrCompareRange('A', 'A', 1, 0), -2);
{17}  Test(StrCompareRange('Aa', 'A', 1, 0), -2);
{18}  Test(StrCompareRange('Aa', 'Aa', 1, 2), 0);
{19}  Test(StrCompareRange('Aa', 'A', 1, 2), 0);
{20}  Test(StrCompareRange('Ba', 'A', 1, 2), 1);
*)
function StrCompareRangeEx(const S1, S2: string; Index, Count: Integer; CaseSensitive: Boolean): Integer;
var
  Len1, Len2: Integer;
  I: Integer;
  C1, C2: Char;
begin
  {$IFDEF CLR}
  if S1 = S2 then
  {$ELSE}
  if Pointer(S1) = Pointer(S2) then
  {$ENDIF CLR}
  begin
    if (Count <= 0) and (S1 <> '') then
      Result := -2 // no work
    else
      Result := 0;
  end
  else
  if (S1 = '') or (S2 = '') then
    Result := -1 // null string
  else
  if Count <= 0 then
    Result := -2 // no work
  else
  begin
    Len1 := Length(S1);
    Len2 := Length(S2);

    if (Index - 1) + Count > Len1 then
      Result := -2
    else
    begin
      if (Index - 1) + Count > Len2 then // strange behaviour, but the assembler code does it
        Count := Len2 - (Index - 1);

      if CaseSensitive then
      begin
        for I := 0 to Count - 1 do
        begin
          C1 := S1[Index + I];
          C2 := S2[Index + I];
          if C1 <> C2 then
          begin
            Result := Ord(C1) - Ord(C2);
            Exit;
          end;
        end;
      end
      else
      begin
        for I := 0 to Count - 1 do
        begin
          C1 := S1[Index + I];
          C2 := S2[Index + I];
          if C1 <> C2 then
          begin
            C1 := CharLower(C1);
            C2 := CharLower(C2);
            if C1 <> C2 then
            begin
              Result := Ord(C1) - Ord(C2);
              Exit;
            end;
          end;
        end;
      end;
      Result := 0;
    end;
  end;
end;

function StrCompare(const S1, S2: string): Integer;
var
  Len1, Len2: Integer;
begin
  {$IFDEF CLR}
  if S1 = S2 then
  {$ELSE}
  if Pointer(S1) = Pointer(S2) then
  {$ENDIF CLR}
    Result := 0
  else
  begin
    Len1 := Length(S1);
    Len2 := Length(S2);
    Result := Len1 - Len2;
    if Result = 0 then
      Result := StrCompareRangeEx(S1, S2, 1, Len1, False);
  end;
end;

{$ELSE} // SUPPORTS_UNICODE

{$IFDEF PIC}
function _StrCompare(const S1, S2: string): Integer; forward;

function StrCompare(const S1, S2: string): Integer;
begin
  Result := _StrCompare(S1, S2);
end;

function _StrCompare(const S1, S2: string): Integer;
{$ELSE}
function StrCompare(const S1, S2: string): Integer;
{$ENDIF PIC}
asm
        // check if pointers are equal

        CMP     EAX, EDX
        JE      @@Equal

        // if S1 is nil return - Length(S2)

        TEST    EAX, EAX
        JZ      @@Str1Null

        // if S2 is nil return  Length(S1)

        TEST    EDX, EDX
        JZ      @@Str2Null

        // EBX will hold case map, ESI S1, EDI S2

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        // move string pointers

        MOV     ESI, EAX
        MOV     EDI, EDX

        // get the length of strings

        MOV     EAX, [ESI-StrRecSize].TStrRec.Length
        MOV     EDX, [EDI-StrRecSize].TStrRec.Length

        // exit if Length(S1) <> Length(S2)

        CMP     EAX, EDX
        JNE     @@MissMatch

        // check the length just in case

        DEC     EDX
        JS      @@InvalidStr

        DEC     EAX
        JS      @@InvalidStr

        // load case map

        LEA     EBX, StrCaseMap

        // make ECX our loop counter

        MOV     ECX, EAX

        // clear working regs

        XOR     EAX, EAX
        XOR     EDX, EDX

        // get last chars

        MOV     AL, [ESI+ECX]
        MOV     DL, [EDI+ECX]

        // lower case them

        MOV     AL, [EBX+EAX]
        MOV     DL, [EBX+EDX]

        // compare them

        CMP     AL, DL
        JNE     @@MissMatch

        // if there was only 1 char then exit

        JECXZ   @@Match

@@NextChar:
        // case sensitive compare of strings

        REPE    CMPSB
        JE      @@Match

        // if there was a missmatch try case insensitive compare, get the chars

        MOV     AL, [ESI-1]
        MOV     DL, [EDI-1]

        // lowercase and compare them, if equal then continue

        MOV     AL, [EBX+EAX]
        MOV     DL, [EBX+EDX]
        CMP     AL, DL
        JE      @@NextChar

        // if we make it here then strings don't match,  return the difference

@@MissMatch:
        SUB     EAX, EDX
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@Match:
        // match, return 0

        XOR     EAX, EAX
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@InvalidStr:
        XOR     EAX, EAX
        DEC     EAX
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@Str1Null:
        // return = - Length(Str2);

        MOV     EDX, [EDX-StrRecSize].TStrRec.Length
        SUB     EAX, EDX
        RET

@@Str2Null:
        // return = Length(Str2);

        MOV     EAX, [EAX-StrRecSize].TStrRec.Length
        RET

@@Equal:
        XOR     EAX, EAX
end;
{$ENDIF SUPPORTS_UNICODE}

function StrCompareRange(const S1, S2: string; const Index, Count: Integer): Integer;
{$IFDEF SUPPORTS_UNICODE}
begin
  Result := StrCompareRangeEx(S1, S2, Index, Count, True);
end;
{$ELSE}
asm
        TEST    EAX, EAX
        JZ      @@Str1Null

        TEST    EDX, EDX
        JZ      @@StrNull

        DEC     ECX
        JS      @@StrNull

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     EBX, Count
        DEC     EBX
        JS      @@NoWork

        MOV     ESI, EAX
        MOV     EDI, EDX

        MOV     EDX, [ESI - StrRecSize].TStrRec.Length

        // # of chars in S1 - (Index - 1)
        SUB     EDX, ECX
        JLE     @@NoWork

        // # of chars in S1 - (Count - 1)
        SUB     EDX, EBX
        JLE     @@NoWork

        // move to index'th char
        ADD     ESI, ECX

        MOV     ECX, [EDI - StrRecSize].TStrRec.Length
        DEC     ECX
        JS      @@NoWork

        // if Length(S2) > Count then ECX := Count else ECX := Length(S2)

        CMP     ECX, EBX
        JLE     @@Skip1
        MOV     ECX, EBX

@@Skip1:
        XOR     EAX, EAX
        XOR     EDX, EDX

@@Loop:
        MOV     AL, [ESI]
        INC     ESI
        MOV     DL, [EDI]
        INC     EDI

        CMP     AL, DL
        JNE     @@MisMatch

        DEC     ECX
        JGE     @@Loop

@@Match:
        XOR     EAX, EAX
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     @@Exit

@@MisMatch:
        SUB     EAX, EDX
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     @@Exit

@@NoWork:
        MOV     EAX, -2
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     @@Exit

@@Str1Null:
        MOV     EAX, 0
        TEST    EDX, EDX
        JZ      @@Exit

@@StrNull:
        MOV     EAX, -1

@@Exit:
end;
{$ENDIF SUPPORTS_UNICODE}

function StrFillChar(const C: Char; Count: Integer): string;
{$IFDEF CLR}
var
  sb: StringBuilder;
begin
  sb := StringBuilder.Create(Count);
  while Count > 0 do
  begin
    sb.Append(C);
    Dec(Count);
  end;
  Result := sb.ToString();
end;
{$ELSE}
begin
  SetLength(Result, Count);
  if (Count > 0) then
    FillChar(Result[1], Count * SizeOf(Char), Ord(C));
end;
{$ENDIF CLR}

{$IFDEF CLR}
function StrFind(const Substr, S: string; const Index: Integer): Integer;
begin
  Result := System.String(S).ToLower().IndexOf(System.String(SubStr).ToLower(), Index - 1) + 1;
end;
{$ELSE}
function StrFind(const Substr, S: string; const Index: Integer): Integer;
{$IFDEF SUPPORTS_UNICODE}
begin
  Result := PosEx(AnsiLowerCase(SubStr), AnsiLowerCase(S), Index);
end;
{$ELSE}
const
   SearchChar: Byte = 0;
   NumberOfChars: Integer = 0;
asm
        // if SubStr = '' then  Return := 0;

        TEST    EAX, EAX
        JZ      @@SubstrIsNull

        // if Str = '' then  Return := 0;

        TEST    EDX, EDX
        JZ      @@StrIsNull

        // Index := Index - 1; if Index < 0 then Return := 0;

        DEC     ECX
        JL      @@IndexIsSmall

        // EBX will hold the case table, ESI pointer to Str, EDI pointer
        // to Substr and - # of chars in Substr to compare

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        // set the string pointers

        MOV     ESI, EDX
        MOV     EDI, EAX

        // save the Index in EDX

        MOV     EDX, ECX

        // temporary get the length of Substr and Str

        MOV     EBX, [EDI - StrRecSize].TStrRec.Length
        MOV     ECX, [ESI - StrRecSize].TStrRec.Length

        // save the address of Str to compute the result

        PUSH    ESI

        // dec the length of Substr because the first char is brought out of it

        DEC     EBX
        JS      @@NotFound

        // #positions in Str to look at = Length(Str) - Length(Substr) - Index - 2

        SUB     ECX, EBX
        JLE     @@NotFound

        SUB     ECX, EDX
        JLE     @@NotFound

        // # of chars in Substr to compare

        MOV     NumberOfChars, EBX

        // point Str to Index'th char

        ADD     ESI, EDX

        // load case map into EBX, and clear EAX

        LEA     EBX, StrCaseMap
        XOR     EAX, EAX
        XOR     EDX, EDX

        // bring the first char out of the Substr and point Substr to the next char

        MOV     DL, [EDI]
        INC     EDI

        // lower case it

        MOV     DL, [EBX + EDX]
        MOV     SearchChar, DL

        JMP     @@Find

@@FindNext:

        // update the loop counter and check the end of string.
        // if we reached the end, Substr was not found.

        DEC     ECX
        JL      @@NotFound

@@Find:

        // get current char from the string, and point Str to the next one

        MOV     AL, [ESI]
        INC     ESI


        // lower case current char

        MOV     AL, [EBX + EAX]

        // does current char match primary search char? if not, go back to the main loop

        CMP     AL, SearchChar
        JNE     @@FindNext

@@Compare:

        // # of chars in Substr to compare

        MOV     EDX, NumberOfChars

@@CompareNext:

        // dec loop counter and check if we reached the end. If yes then we found it

        DEC     EDX
        JL      @@Found

        // get the chars from Str and Substr, if they are equal then continue comparing

        MOV     AL, [ESI + EDX]
        CMP     AL, [EDI + EDX]
        JE      @@CompareNext

        // otherwise try the reverse case. If they still don't match go back to the Find loop

        MOV     AL, [EBX + EAX + StrReOffset]
        CMP     AL, [EDI + EDX]
        JNE     @@FindNext

        // if they matched, continue comparing

        JMP     @@CompareNext

@@Found:
        // we found it, calculate the result

        MOV     EAX, ESI
        POP     ESI
        SUB     EAX, ESI

        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@NotFound:

        // not found it, clear the result

        XOR     EAX, EAX
        POP     ESI
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@IndexIsSmall:
@@StrIsNull:

        // clear the result

        XOR     EAX, EAX

@@SubstrIsNull:
@@Exit:
end;
{$ENDIF SUPPORTS_UNICODE}
{$ENDIF CLR}

function StrHasPrefix(const S: string; const Prefixes: array of string): Boolean;
begin
  Result := StrPrefixIndex(S, Prefixes) > -1;
end;

function StrIndex(const S: string; const List: array of string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(List) to High(List) do
  begin
    {$IFDEF CLR}
    if SameText(S, List[I]) then
    {$ELSE}
    if AnsiSameText(S, List[I]) then
    {$ENDIF CLR}
    begin
      Result := I;
      Break;
    end;
  end;
end;

function StrILastPos(const SubStr, S: string): Integer;
begin
  Result := StrLastPos(StrUpper(SubStr), StrUpper(S));
end;

function StrIPos(const SubStr, S: string): Integer;
begin
  {$IFDEF CLR}
  Result := Pos(SubStr.ToUpper, S.ToUpper);
  {$ELSE}
  Result := Pos(AnsiUpperCase(SubStr), AnsiUpperCase(S));
  {$ENDIF CLR}
end;

function StrIsOneOf(const S: string; const List: array of string): Boolean;
begin
  Result := StrIndex(S, List) > -1;
end;

function StrLastPos(const SubStr, S: string): Integer;
{$IFDEF CLR}
begin
  Result := System.String(S).LastIndexOf(SubStr) + 1;
end;
{$ELSE}
var
  Last, Current: PChar;
begin
  Result := 0;
  Last := nil;
  Current := PChar(S);

  while (Current <> nil) and (Current^ <> #0) do
  begin
    Current := AnsiStrPos(PChar(Current), PChar(SubStr));
    if Current <> nil then
    begin
      Last := Current;
      Inc(Current);
    end;
  end;
  if Last <> nil then
    Result := Abs(PChar(S) - Last) + 1;
end;
{$ENDIF CLR}

// IMPORTANT NOTE: The StrMatch function does currently not work with the Asterix (*)

function StrMatch(const Substr, S: string; const Index: Integer): Integer;
{$IFDEF CLR}
begin
  { TODO : StrMatch }
  Assert(False, 'Not implemented yet');
  Result := 0;
end;
{$ELSE}
{$IFDEF SUPPORTS_UNICODE}
begin
  { TODO : StrMatch }
  Assert(False, 'Not implemented yet');
  Result := 0;
end;
{$ELSE}
asm
        // make sure that strings are not null

        TEST    EAX, EAX
        JZ      @@SubstrIsNull

        TEST    EDX, EDX
        JZ      @@StrIsNull

        // limit index to satisfy 1 <= index, and dec it

        DEC     ECX
        JL      @@IndexIsSmall

        // EBX will hold the case table, ESI pointer to Str, EDI pointer
        // to Substr and EBP # of chars in Substr to compare

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        // set the string pointers

        MOV     ESI, EDX
        MOV     EDI, EAX

        // save the Index in EDX

        MOV     EDX, ECX

        // save the address of Str to compute the result

        PUSH    ESI

        // temporary get the length of Substr and Str

        MOV     EBX, [EDI - StrRecSize].TStrRec.Length
        MOV     ECX, [ESI - StrRecSize].TStrRec.Length

        // dec the length of Substr because the first char is brought out of it

        DEC     EBX
        JS      @@NotFound

        // #positions in Str to look at = Length(Str) - Length(Substr) - Index - 2

        SUB     ECX, EBX
        JLE     @@NotFound

        SUB     ECX, EDX
        JLE     @@NotFound

        // # of chars in Substr to compare

        MOV     EBP, EBX

        // point Str to Index'th char

        ADD     ESI, EDX

        // load case map into EBX, and clear EAX & ECX

        LEA     EBX, StrCaseMap
        XOR     EAX, EAX
        XOR     ECX, ECX

        // bring the first char out of the Substr and point Substr to the next char

        MOV     CL, [EDI]
        INC     EDI

        // lower case it

        MOV     CL, [EBX + ECX]

@@FindNext:

        // get the current char from Str into al

        MOV     AL, [ESI]
        INC     ESI

        // check the end of string

        TEST    AL, AL
        JZ      @@NotFound


        CMP     CL, '*'    // Wild Card?
        JE      @@Compare

        CMP     CL, '?'    // Wild Card?
        JE      @@Compare

        // lower case current char

        MOV     AL, [EBX + EAX]

        // check if the current char matches the primary search char,
        // if not continue searching

        CMP     AL, CL
        JNE     @@FindNext

@@Compare:

        // # of chars in Substr to compare }

        MOV     EDX, EBP

@@CompareNext:

        // dec loop counter and check if we reached the end. If yes then we found it

        DEC     EDX
        JL      @@Found

        // get the chars from Str and Substr, if they are equal then continue comparing

        MOV     AL, [EDI + EDX]               // char from  Substr

        CMP     AL, '*'                     // wild card?
        JE      @@CompareNext

        CMP     AL, '?'                     // wild card?
        JE      @@CompareNext

        CMP     AL, [ESI + EDX]               // equal to PChar(Str)^ ?
        JE      @@CompareNext

        MOV     AL, [EBX + EAX + StrReOffset]  // reverse case?
        CMP     AL, [ESI + EDX]
        JNE     @@FindNext                  // if still no, go back to the main loop

        // if they matched, continue comparing

        JMP     @@CompareNext

@@Found:
        // we found it, calculate the result

        MOV     EAX, ESI
        POP     ESI
        SUB     EAX, ESI

        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@NotFound:

        // not found it, clear the result

        XOR     EAX, EAX
        POP     ESI
        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@IndexIsSmall:
@@StrIsNull:

        // clear the result

        XOR     EAX, EAX

@@SubstrIsNull:
@@Exit:
end;
{$ENDIF SUPPORTS_UNICODE}
{$ENDIF CLR}

// Derived from "Like" by Michael Winter
function StrMatches(const Substr, S: string; const Index: Integer): Boolean;
{$IFDEF CLR}
begin
  Result := Substr = S;
  { TODO : StrMatches }
end;
{$ELSE}
var
  StringPtr: PChar;
  PatternPtr: PChar;
  StringRes: PChar;
  PatternRes: PChar;
begin
  if SubStr = '' then
    raise EJclStringError.CreateRes(@RsBlankSearchString);

  Result := SubStr = '*';

  if Result or (S = '') then
    Exit;

  if (Index <= 0) or (Index > Length(S)) then
    raise EJclStringError.CreateRes(@RsArgumentOutOfRange);

  StringPtr := PChar(@S[Index]);
  PatternPtr := PChar(SubStr);
  StringRes := nil;
  PatternRes := nil;

  repeat
    repeat
      case PatternPtr^ of
        #0:
          begin
            Result := StringPtr^ = #0;
            if Result or (StringRes = nil) or (PatternRes = nil) then
              Exit;

            StringPtr := StringRes;
            PatternPtr := PatternRes;
            Break;
          end;
        '*':
          begin
            Inc(PatternPtr);
            PatternRes := PatternPtr;
            Break;
          end;
        '?':
          begin
            if StringPtr^ = #0 then
              Exit;
            Inc(StringPtr);
            Inc(PatternPtr);
          end;
        else
          begin
            if StringPtr^ = #0 then
              Exit;
            if StringPtr^ <> PatternPtr^ then
            begin
              if (StringRes = nil) or (PatternRes = nil) then
                Exit;
              StringPtr := StringRes;
              PatternPtr := PatternRes;
              Break;
            end
            else
            begin
              Inc(StringPtr);
              Inc(PatternPtr);
            end;
          end;
      end;
    until False;

    repeat
      case PatternPtr^ of
        #0:
          begin
            Result := True;
            Exit;
          end;
        '*':
          begin
            Inc(PatternPtr);
            PatternRes := PatternPtr;
          end;
        '?':
          begin
            if StringPtr^ = #0 then
              Exit;
            Inc(StringPtr);
            Inc(PatternPtr);
          end;
        else
          begin
            repeat
              if StringPtr^ = #0 then
                Exit;
              if StringPtr^ = PatternPtr^ then
                Break;
              Inc(StringPtr);
            until False;
            Inc(StringPtr);
            StringRes := StringPtr;
            Inc(PatternPtr);
            Break;
          end;
      end;
    until False;
  until False;
end;
{$ENDIF CLR}

function StrNPos(const S, SubStr: string; N: Integer): Integer;
var
  I, P: Integer;
begin
  if N < 1 then
  begin
    Result := 0;
    Exit;
  end;

  Result := StrSearch(SubStr, S, 1);
  I := 1;
  while I < N do
  begin
    P := StrSearch(SubStr, S, Result + 1);
    if P = 0 then
    begin
      Result := 0;
      Break;
    end
    else
    begin
      Result := P;
      Inc(I);
    end;
  end;
end;

function StrNIPos(const S, SubStr: string; N: Integer): Integer;
var
  I, P: Integer;
begin
  if N < 1 then
  begin
    Result := 0;
    Exit;
  end;

  Result := StrFind(SubStr, S, 1);
  I := 1;
  while I < N do
  begin
    P := StrFind(SubStr, S, Result + 1);
    if P = 0 then
    begin
      Result := 0;
      Break;
    end
    else
    begin
      Result := P;
      Inc(I);
    end;
  end;
end;

function StrPrefixIndex(const S: string; const Prefixes: array of string): Integer;
var
  I: Integer;
  Test: string;
begin
  Result := -1;
  for I := Low(Prefixes) to High(Prefixes) do
  begin
    Test := StrLeft(S, Length(Prefixes[I]));
    {$IFDEF CLR}
    if SameText(Test, Prefixes[I]) then
    {$ELSE}
    if AnsiSameText(Test, Prefixes[I]) then
    {$ENDIF CLR}
    begin
      Result := I;
      Break;
    end;
  end;
end;

{$IFDEF CLR}
function StrSearch(const Substr, S: string; const Index: Integer): Integer;
begin
  Result := System.String(S).IndexOf(SubStr, Index - 1) + 1;
end;
{$ELSE}
function StrSearch(const Substr, S: string; const Index: Integer): Integer;
{$IFDEF SUPPORTS_UNICODE}
begin
  Result := PosEx(SubStr, S, Index);
end;
{$ELSE}
asm
        // make sure that strings are not null

        TEST    EAX, EAX
        JZ      @@SubstrIsNull

        TEST    EDX, EDX
        JZ      @@StrIsNull

        // limit index to satisfy 1 <= index, and dec it

        DEC     ECX
        JL      @@IndexIsSmall

        // ebp will hold # of chars in Substr to compare, esi pointer to Str,
        // edi pointer to Substr, ebx primary search char

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        // set the string pointers

        MOV     ESI, EDX
        MOV     EDI, EAX

        // save the (Index - 1) in edx

        MOV     EDX, ECX

        // save the address of Str to compute the result

        PUSH    ESI

        // temporary get the length of Substr and Str

        MOV     EBX, [EDI-StrRecSize].TStrRec.Length
        MOV     ECX, [ESI-StrRecSize].TStrRec.Length

        // dec the length of Substr because the first char is brought out of it

        DEC     EBX
        JS      @@NotFound

        // # of positions in Str to look at = Length(Str) - Length(Substr) - Index - 2

        SUB     ECX, EBX
        JLE     @@NotFound

        SUB     ECX, EDX
        JLE     @@NotFound

        // point Str to Index'th char

        ADD     ESI, EDX

        // # of chars in Substr to compare

        MOV     EBP, EBX

        // clear EAX & ECX (working regs)

        XOR     EAX, EAX
        XOR     EBX, EBX

        // bring the first char out of the Substr, and
        // point Substr to the next char

        MOV     BL, [EDI]
        INC     EDI

        // jump into the loop

        JMP     @@Find

@@FindNext:

        // update the loop counter and check the end of string.
        // if we reached the end, Substr was not found.

        DEC     ECX
        JL      @@NotFound

@@Find:

        // get current char from the string, and /point Str to the next one.
        MOV     AL, [ESI]
        INC     ESI

        // does current char match primary search char? if not, go back to the main loop

        CMP     AL, BL
        JNE     @@FindNext

        // otherwise compare SubStr

@@Compare:

        // move # of char to compare into edx, edx will be our compare loop counter.

        MOV     EDX, EBP

@@CompareNext:

        // check if we reached the end of Substr. If yes we found it.

        DEC     EDX
        JL      @@Found

        // get last chars from Str and SubStr and compare them,
        // if they don't match go back to out main loop.

        MOV     AL, [EDI+EDX]
        CMP     AL, [ESI+EDX]
        JNE     @@FindNext

        // if they matched, continue comparing

        JMP     @@CompareNext

@@Found:
        // we found it, calculate the result and exit.

        MOV     EAX, ESI
        POP     ESI
        SUB     EAX, ESI

        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@NotFound:
        // not found it, clear result and exit.

        XOR     EAX, EAX
        POP     ESI
        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@IndexIsSmall:
@@StrIsNull:
        // clear result and exit.

        XOR     EAX, EAX

@@SubstrIsNull:
@@Exit:
end;
{$ENDIF SUPPORTS_UNICODE}
{$ENDIF CLR}

//=== String Extraction ======================================================

function StrAfter(const SubStr, S: string): string;
var
  P: Integer;
begin
  P := StrFind(SubStr, S, 1); // StrFind is case-insensitive pos
  if P <= 0 then
    Result := ''           // substr not found -> nothing after it
  else
    Result := StrRestOf(S, P + Length(SubStr));
end;

function StrBefore(const SubStr, S: string): string;
var
  P: Integer;
begin
  P := StrFind(SubStr, S, 1);
  if P <= 0 then
    Result := S
  else
    Result := StrLeft(S, P - 1);
end;


function StrBetween(const S: string; const Start, Stop: Char): string;
var
  PosStart, PosEnd: Integer;
  L: Integer;
begin
  PosStart := Pos(Start, S);
  PosEnd := StrSearch(Stop, S, PosStart+1);  // PosEnd has to be after PosStart.

  if (PosStart > 0) and (PosEnd > PosStart) then
  begin
    L := PosEnd - PosStart;
    Result := Copy(S, PosStart + 1, L - 1);
  end
  else
    Result := '';
end;

function StrChopRight(const S: string; N: Integer): string;
begin
  Result := Copy(S, 1, Length(S) - N);
end;

function StrLeft(const S: string; Count: Integer): string;
begin
  Result := Copy(S, 1, Count);
end;

function StrMid(const S: string; Start, Count: Integer): string;
begin
  Result := Copy(S, Start, Count);
end;

function StrRestOf(const S: string; N: Integer ): string;
begin
  Result := Copy(S, N, (Length(S) - N + 1));
end;

function StrRight(const S: string; Count: Integer): string;
begin
  Result := Copy(S, Length(S) - Count + 1, Count);
end;

//=== Character (do we have it ;) ============================================

function CharEqualNoCase(const C1, C2: Char): Boolean;
begin
  //if they are not equal chars, may be same letter different case
  Result := (C1 = C2) or
    (CharIsAlpha(C1) and CharIsAlpha(C2) and (CharLower(C1) = CharLower(C2)));
end;


function CharIsAlpha(const C: Char): Boolean;
begin
  {$IFDEF CLR}
  Result := System.Char.IsLetter(C);
  {$ELSE}
  Result := (StrCharTypes[C] and C1_ALPHA) <> 0;
  {$ENDIF CLR}
end;

function CharIsAlphaNum(const C: Char): Boolean;
begin
  {$IFDEF CLR}
  Result := System.Char.IsLetterOrDigit(C);
  {$ELSE}
  Result := ((StrCharTypes[C] and C1_ALPHA) <> 0) or
    ((StrCharTypes[C] and C1_DIGIT) <> 0);
  {$ENDIF CLR}
end;

function CharIsBlank(const C: Char): Boolean;
begin
  {$IFDEF CLR}
  Result := System.Char.IsSurrogate(C);
  {$ELSE}
  Result := ((StrCharTypes[C] and C1_BLANK) <> 0);
  {$ENDIF CLR}
end;

function CharIsControl(const C: Char): Boolean;
begin
  {$IFDEF CLR}
  Result := System.Char.IsControl(C);
  {$ELSE}
  Result := (StrCharTypes[C] and C1_CNTRL) <> 0;
  {$ENDIF CLR}
end;

function CharIsDelete(const C: Char): Boolean;
begin
  Result := (C = #8);
end;

function CharIsDigit(const C: Char): Boolean;
begin
  {$IFDEF CLR}
  Result := System.Char.IsDigit(C);
  {$ELSE}
  Result := (StrCharTypes[C] and C1_DIGIT) <> 0;
  {$ENDIF CLR}
end;

function CharIsLower(const C: Char): Boolean;
begin
  {$IFDEF CLR}
  Result := System.Char.IsLower(C);
  {$ELSE}
  Result := (StrCharTypes[C] and C1_LOWER) <> 0;
  {$ENDIF CLR}
end;

function CharIsNumberChar(const C: Char): Boolean;
begin
  {$IFDEF CLR}
  Result := System.Char.IsDigit(C) or (C = '+') or (C = '-') or (C = DecimalSeparator);
  {$ELSE}
  Result := ((StrCharTypes[C] and C1_DIGIT) <> 0) or (C in AnsiSigns) or (C = DecimalSeparator);
  {$ENDIF CLR}
end;

function CharIsPrintable(const C: Char): Boolean;
begin
  Result := not CharIsControl(C);
end;

function CharIsPunctuation(const C: Char): Boolean;
begin
  {$IFDEF CLR}
  Result := System.Char.IsPunctuation(C);
  {$ELSE}
  Result := ((StrCharTypes[C] and C1_PUNCT) <> 0);
  {$ENDIF CLR}
end;

function CharIsReturn(const C: Char): Boolean;
begin
  Result := (C = AnsiLineFeed) or (C = AnsiCarriageReturn);
end;

function CharIsSpace(const C: Char): Boolean;
begin
  {$IFDEF CLR}
  Result := System.Char.IsSeparator(C);
  {$ELSE}
  Result := (StrCharTypes[C] and C1_SPACE) <> 0;
  {$ENDIF CLR}
end;

function CharIsUpper(const C: Char): Boolean;
begin
  {$IFDEF CLR}
  Result := System.Char.IsUpper(C);
  {$ELSE}
  Result := (StrCharTypes[C] and C1_UPPER) <> 0;
  {$ENDIF CLR}
end;

function CharIsWhiteSpace(const C: Char): Boolean;
begin
  {$IFDEF CLR}
  Result := System.Char.IsWhiteSpace(C);
  {$ELSE}
  Result := C in AnsiWhiteSpace;
  {$ENDIF CLR}
end;

{$IFNDEF CLR}
function CharType(const C: Char): Word;
begin
  Result := StrCharTypes[C];
end;

//=== PCharVector ============================================================

function StringsToPCharVector(var Dest: PCharVector; const Source: TStrings): PCharVector;
var
  I: Integer;
  S: string;
  List: array of PChar;
begin
  Assert(Source <> nil);
  Dest := AllocMem((Source.Count + SizeOf(Char)) * SizeOf(PChar));
  SetLength(List, Source.Count + SizeOf(Char));
  for I := 0 to Source.Count - 1 do
  begin
    S := Source[I];
    List[I] := StrAlloc(Length(S) + SizeOf(Char));
    StrPCopy(List[I], S);
  end;
  List[Source.Count] := nil;
  Move(List[0], Dest^, (Source.Count + 1) * SizeOf(PChar));
  Result := Dest;
end;

function PCharVectorCount(Source: PCharVector): Integer;
var
  P: PChar;
begin
  Result := 0;
  if Source <> nil then
  begin
    P := Source^;
    while P <> nil do
    begin
      Inc(Result);
      P := PCharVector(INT_PTR(Source) + (SizeOf(PChar) * Result))^;
    end;
  end;
end;

procedure PCharVectorToStrings(const Dest: TStrings; Source: PCharVector);
var
  I, Count: Integer;
  List: array of PChar;
begin
  Assert(Dest <> nil);
  if Source <> nil then
  begin
    Count := PCharVectorCount(Source);
    SetLength(List, Count);
    Move(Source^, List[0], Count * SizeOf(PChar));
    Dest.BeginUpdate;
    try
      Dest.Clear;
      for I := 0 to Count - 1 do
        Dest.Add(List[I]);
    finally
      Dest.EndUpdate;
    end;
  end;
end;

procedure FreePCharVector(var Dest: PCharVector);
var
  I, Count: Integer;
  List: array of PChar;
begin
  if Dest <> nil then
  begin
    Count := PCharVectorCount(Dest);
    SetLength(List, Count);
    Move(Dest^, List[0], Count * SizeOf(PChar));
    for I := 0 to Count - 1 do
      StrDispose(List[I]);
    FreeMem(Dest, (Count + 1) * SizeOf(PChar));
    Dest := nil;
  end;
end;
{$ENDIF ~CLR}

//=== Character Transformation Routines ======================================

function CharHex(const C: Char): Byte;
begin
  Result := $FF;
  {$IFDEF CLR}
  if System.Char.IsDigit(C) then
  {$ELSE ~CLR}
  if C in AnsiDecDigits then
  {$ENDIF ~CLR}
    Result := Ord(CharUpper(C)) - Ord('0')
  else
  begin
    {$IFDEF CLR}
    if (C >= 'A') and (C <= 'F') then
    {$ELSE ~CLR}
    if C in AnsiHexDigits then
    {$ENDIF ~CLR}
      Result := Ord(CharUpper(C)) - (Ord('A')) + 10;
  end;
end;

function CharLower(const C: Char): Char;
begin
  {$IFDEF CLR}
  Result := System.Char.ToLower(C);
  {$ELSE}
  Result := StrCaseMap[Ord(C) + StrLoOffset];
  {$ENDIF CLR}
end;

function CharToggleCase(const C: Char): Char;
begin
  {$IFDEF CLR}
  if System.Char.IsUpper(C) then
    Result := System.Char.ToLower(C)
  else
  if System.Char.IsLower(C) then
    Result := System.Char.ToUpper(C)
  else
    Result := C;
  {$ELSE}
  Result := StrCaseMap[Ord(C) + StrReOffset];
  {$ENDIF CLR}
end;

function CharUpper(const C: Char): Char;
begin
  {$IFDEF CLR}
  Result := System.Char.ToUpper(C);
  {$ELSE}
  Result := StrCaseMap[Ord(C) + StrUpOffset];
  {$ENDIF CLR}
end;

//=== Character Search and Replace ===========================================

function CharLastPos(const S: string; const C: Char; const Index: Integer): Integer;
begin
  if (Index > 0) and (Index <= Length(S)) then
  begin
    for Result := Length(S) downto Index do
      if S[Result] = C then
        Exit;
  end;
  Result := 0;
end;

function CharPos(const S: string; const C: Char; const Index: Integer): Integer;
begin
  if (Index > 0) and (Index <= Length(S)) then
  begin
    for Result := Index to Length(S) do
      if S[Result] = C then
        Exit;
  end;
  Result := 0;
end;

function CharIPos(const S: string; C: Char; const Index: Integer): Integer;
begin
  if (Index > 0) and (Index <= Length(S)) then
  begin
    C := CharUpper(C);
    for Result := Index to Length(S) do
      {$IFDEF CLR}
      if System.Char.ToUpper(S[Result]) = C then
      {$ELSE}
      if StrCaseMap[Ord(S[Result]) + StrUpOffset] = C then
      {$ENDIF CLR}
        Exit;
  end;
  Result := 0;
end;

function CharReplace(var S: string; const Search, Replace: Char): Integer;
{$IFDEF CLR}
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
    if S[I] = Search then
      Inc(Result);
  S := S.Replace(Search, Replace);
end;
{$ELSE}
var
  P: PChar;
  Index, Len: Integer;
begin
  Result := 0;
  if Search <> Replace then
  begin
    UniqueString(S);
    P := PChar(S);
    Len := Length(S);
    for Index := 0 to Len-1 do
    begin
      if P^ = Search then
      begin
        P^ := Replace;
        Inc(Result);
      end;
      Inc(P);
    end;
  end;
end;
{$ENDIF CLR}

{$IFNDEF CLR}
//=== MultiSz ================================================================

function StringsToMultiSz(var Dest: PMultiSz; const Source: TStrings): PMultiSz;
var
  I, TotalLength: Integer;
  P: PMultiSz;
begin
  Assert(Source <> nil);
  TotalLength := 1;
  for I := 0 to Source.Count - 1 do
    if Source[I] = '' then
      raise EJclStringError.CreateRes(@RsInvalidEmptyStringItem)
    else
      Inc(TotalLength, StrLen(PChar(Source[I])) + 1);
  AllocateMultiSz(Dest, TotalLength);
  P := Dest;
  for I := 0 to Source.Count - 1 do
  begin
    P := StrECopy(P, PChar(Source[I]));
    Inc(P);
  end;
  P^ := #0;
  Result := Dest;
end;

procedure MultiSzToStrings(const Dest: TStrings; const Source: PMultiSz);
var
  P: PMultiSz;
begin
  Assert(Dest <> nil);
  Dest.BeginUpdate;
  try
    Dest.Clear;
    if Source <> nil then
    begin
      P := Source;
      while P^ <> #0 do
      begin
        Dest.Add(P);
        P := StrEnd(P);
        Inc(P);
      end;
    end;
  finally
    Dest.EndUpdate;
  end;
end;

function MultiSzLength(const Source: PMultiSz): Integer;
var
  P: PMultiSz;
begin
  Result := 0;
  if Source <> nil then
  begin
    P := Source;
    repeat
      Inc(Result, StrLen(P) + 1);
      P := StrEnd(P);
      Inc(P);
    until P^ = #0;
    Inc(Result);
  end;
end;

procedure AllocateMultiSz(var Dest: PMultiSz; Len: Integer);
begin
  if Len > 0 then
    GetMem(Dest, Len * SizeOf(Char))
  else
    Dest := nil;
end;

procedure FreeMultiSz(var Dest: PMultiSz);
begin
  if Dest <> nil then
    FreeMem(Dest);
  Dest := nil;
end;

function MultiSzDup(const Source: PMultiSz): PMultiSz;
var
  Len: Integer;
begin
  if Source <> nil then
  begin
    Len := MultiSzLength(Source);
    AllocateMultiSz(Result, Len);
    Move(Source^, Result^, Len * SizeOf(Char));
  end
  else
    Result := nil;
end;

function WideStringsToWideMultiSz(var Dest: PWideMultiSz; const Source: TWideStrings): PWideMultiSz;
var
  I, TotalLength: Integer;
  P: PWideMultiSz;
begin
  Assert(Source <> nil);
  TotalLength := 1;
  for I := 0 to Source.Count - 1 do
    if Source[I] = '' then
      raise EJclStringError.CreateRes(@RsInvalidEmptyStringItem)
    else
      Inc(TotalLength, StrLenW(PWideChar(Source[I])) + 1);
  AllocateWideMultiSz(Dest, TotalLength);
  P := Dest;
  for I := 0 to Source.Count - 1 do
  begin
    P := StrECopyW(P, PWideChar(Source[I]));
    Inc(P);
  end;
  P^:= #0;
  Result := Dest;
end;

procedure WideMultiSzToWideStrings(const Dest: TWideStrings; const Source: PWideMultiSz);
var
  P: PWideMultiSz;
begin
  Assert(Dest <> nil);
  Dest.BeginUpdate;
  try
    Dest.Clear;
    if Source <> nil then
    begin
      P := Source;
      while P^ <> #0 do
      begin
        Dest.Add(P);
        P := StrEndW(P);
        Inc(P);
      end;
    end;
  finally
    Dest.EndUpdate;
  end;
end;

function WideMultiSzLength(const Source: PWideMultiSz): Integer;
var
  P: PWideMultiSz;
begin
  Result := 0;
  if Source <> nil then
  begin
    P := Source;
    repeat
      Inc(Result, StrLenW(P) + 1);
      P := StrEndW(P);
      Inc(P);
    until P^ = #0;
    Inc(Result);
  end;
end;

procedure AllocateWideMultiSz(var Dest: PWideMultiSz; Len: Integer);
begin
  if Len > 0 then
    GetMem(Dest, Len * SizeOf(WideChar))
  else
    Dest := nil;
end;

procedure FreeWideMultiSz(var Dest: PWideMultiSz);
begin
  if Dest <> nil then
    FreeMem(Dest);
  Dest := nil;
end;

function WideMultiSzDup(const Source: PWideMultiSz): PWideMultiSz;
var
  Len: Integer;
begin
  if Source <> nil then
  begin
    Len := WideMultiSzLength(Source);
    AllocateWideMultiSz(Result, Len);
    Move(Source^, Result^, Len * SizeOf(WideChar));
  end
  else
    Result := nil;
end;
{$ENDIF ~CLR}

//=== TStrings Manipulation ==================================================

procedure StrToStrings(S, Sep: string; const List: TStrings; const AllowEmptyString: Boolean = True);
var
  I, L: Integer;
  Left: string;
begin
  Assert(List <> nil);
  List.BeginUpdate;
  try
    List.Clear;
    L := Length(Sep);
    I := Pos(Sep, S);
    while I > 0 do
    begin
      Left := StrLeft(S, I - 1);
      if (Left <> '') or AllowEmptyString then
        List.Add(Left);
      Delete(S, 1, I + L - 1);
      I := Pos(Sep, S);
    end;
    if S <> '' then
      List.Add(S);  // Ignore empty strings at the end.
  finally
    List.EndUpdate;
  end;
end;

procedure StrIToStrings(S, Sep: string; const List: TStrings; const AllowEmptyString: Boolean = True);
var
  I, L: Integer;
  LowerCaseStr: string;
  Left: string;
begin
  Assert(List <> nil);
  LowerCaseStr := StrLower(S);
  Sep := StrLower(Sep);
  L := Length(Sep);
  I := Pos(Sep, LowerCaseStr);
  List.BeginUpdate;
  try
    List.Clear;
    while I > 0 do
    begin
      Left := StrLeft(S, I - 1);
      if (Left <> '') or AllowEmptyString then
        List.Add(Left);
      Delete(S, 1, I + L - 1);
      Delete(LowerCaseStr, 1, I + L - 1);
      I := Pos(Sep, LowerCaseStr);
    end;
    if S <> '' then
      List.Add(S);  // Ignore empty strings at the end.
  finally
    List.EndUpdate;
  end;
end;

function StringsToStr(const List: TStrings; const Sep: string; const AllowEmptyString: Boolean): string;
var
  I, L: Integer;
begin
  Result := '';
  for I := 0 to List.Count - 1 do
  begin
    if (List[I] <> '') or AllowEmptyString then
    begin
      // don't combine these into one addition, somehow it hurts performance
      Result := Result + List[I];
      Result := Result + Sep;
    end;
  end;
  // remove terminating separator
  if List.Count <> 0 then
  begin
    L := Length(Sep);
    Delete(Result, Length(Result) - L + 1, L);
  end;
end;

procedure TrimStrings(const List: TStrings; DeleteIfEmpty: Boolean);
var
  I: Integer;
begin
  Assert(List <> nil);
  List.BeginUpdate;
  try
    for I := List.Count - 1 downto 0 do
    begin
      List[I] := Trim(List[I]);
      if (List[I] = '') and DeleteIfEmpty then
        List.Delete(I);
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TrimStringsRight(const List: TStrings; DeleteIfEmpty: Boolean);
var
  I: Integer;
begin
  Assert(List <> nil);
  List.BeginUpdate;
  try
    for I := List.Count - 1 downto 0 do
    begin
      List[I] := TrimRight(List[I]);
      if (List[I] = '') and DeleteIfEmpty then
        List.Delete(I);
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TrimStringsLeft(const List: TStrings; DeleteIfEmpty: Boolean);
var
  I: Integer;
begin
  Assert(List <> nil);
  List.BeginUpdate;
  try
    for I := List.Count - 1 downto 0 do
    begin
      List[I] := TrimLeft(List[I]);
      if (List[I] = '') and DeleteIfEmpty then
        List.Delete(I);
    end;
  finally
    List.EndUpdate;
  end;
end;

function AddStringToStrings(const S: string; Strings: TStrings; const Unique: Boolean): Boolean;
begin
  Assert(Strings <> nil);
  Result := Unique and (Strings.IndexOf(S) <> -1);
  if not Result then
    Result := Strings.Add(S) > -1;
end;

//=== Miscellaneous ==========================================================

{$IFDEF KEEP_DEPRECATED}
function BooleanToStr(B: Boolean): string;
const
  Bools: array [Boolean] of string = ('False', 'True');
begin
  Result := Bools[B];
end;
{$ENDIF KEEP_DEPRECATED}

function FileToString(const FileName: string): AnsiString;
var
  fs: TFileStream;
  Len: Integer;
  {$IFDEF CLR}
  Buf: array of Byte;
  {$ENDIF CLR}
begin
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Len := fs.Size;
    SetLength(Result, Len);
    if Len > 0 then
    {$IFDEF CLR}
    begin
      SetLength(Buf, Len);
      fs.ReadBuffer(Buf, Len);
      Result := Buf;
    end;
    {$ELSE}
      fs.ReadBuffer(Result[1], Len);
    {$ENDIF CLR}
  finally
    fs.Free;
  end;
end;

procedure StringToFile(const FileName: string; const Contents: AnsiString; Append: Boolean);
var
  FS: TFileStream;
  Len: Integer;
begin
  if Append and FileExists(filename) then
    FS := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyWrite)
  else
    FS := TFileStream.Create(FileName, fmCreate);
  try
    if Append then
      StreamSeek(FS, 0, soEnd);  // faster than .Position := .Size
    Len := Length(Contents);
    if Len > 0 then
    {$IFDEF CLR}
    FS.WriteBuffer(BytesOf(Contents), Len);
    {$ELSE}
    FS.WriteBuffer(Contents[1], Len);
    {$ENDIF CLR}
  finally
    FS.Free;
  end;
end;
function StrToken(var S: string; Separator: Char): string;
var
  I: Integer;
begin
  I := Pos(Separator, S);
  if I <> 0 then
  begin
    Result := Copy(S, 1, I - 1);
    Delete(S, 1, I);
  end
  else
  begin
    Result := S;
    S := '';
  end;
end;

{$IFDEF CLR}
procedure StrTokens(const S: string; const List: TStrings);
var
  Start: Integer;
  Token: string;
  Done: Boolean;
begin
  Assert(List <> nil);
  if List = nil then
    Exit;

  List.BeginUpdate;
  try
    List.Clear;
    Start := 0;
    repeat
      Done := StrWord(S, Start, Token);
      if Token <> '' then
        List.Add(Token);
    until Done;
  finally
    List.EndUpdate;
  end;
end;

function StrWord(const S: string; var Index: Integer; out Word: string): Boolean;
var
  Start: Integer;
  C: Char;
begin
  Word := '';
  if (S = nil) or (S = '') then
  begin
    Result := True;
    Exit;
  end;
  Start := Index;
  Result := False;
  while True do
  begin
    case S[Index] of
      #0:
        begin
          if Start <> 0 then
            Word := S.Substring(Start, Index - Start);
          Result := True;
          Exit;
        end;
      AnsiSpace, AnsiLineFeed, AnsiCarriageReturn:
        begin
          if Start <> 0 then
          begin
            Word := S.Substring(Start, Index - Start);
            Exit;
          end
          else
          begin
            C := S[Index];
            while (C = AnsiSpace) or (C = AnsiLineFeed) or (C = AnsiCarriageReturn) do
            begin
              Inc(Index);
              C := S[Index];
            end;
          end;

        end;
    else
      if Start = 0 then
        Start := Index;
      Inc(Index);
    end;
  end;
end;

{$ELSE}

procedure StrTokens(const S: string; const List: TStrings);
var
  Start: PChar;
  Token: string;
  Done: Boolean;
begin
  Assert(List <> nil);
  if List = nil then
    Exit;

  List.BeginUpdate;
  try
    List.Clear;
    Start := Pointer(S);
    repeat
      Done := StrWord(Start, Token);
      if Token <> '' then
        List.Add(Token);
    until Done;
  finally
    List.EndUpdate;
  end;
end;

function StrWord(var S: PChar; out Word: string): Boolean;
var
  Start: PChar;
begin
  Word := '';
  if S = nil then
  begin
    Result := True;
    Exit;
  end;
  Start := nil;
  Result := False;
  while True do
  begin
    case S^ of
      #0:
        begin
          if Start <> nil then
            SetString(Word, Start, S - Start);
          Result := True;
          Exit;
        end;
      AnsiSpace, AnsiLineFeed, AnsiCarriageReturn:
        begin
          if Start <> nil then
          begin
            SetString(Word, Start, S - Start);
            Exit;
          end
          else
            while (S^ in [AnsiSpace, AnsiLineFeed, AnsiCarriageReturn]) do
              Inc(S);
        end;
    else
      if Start = nil then
        Start := S;
      Inc(S);
    end;
  end;
end;
{$ENDIF ~CLR}

procedure StrTokenToStrings(S: string; Separator: Char; const List: TStrings);
var
  Token: string;
begin
  Assert(List <> nil);

  if List = nil then
    Exit;

  List.BeginUpdate;
  try
    List.Clear;
    while S <> '' do
    begin
      Token := StrToken(S, Separator);
      List.Add(Token);
    end;
  finally
    List.EndUpdate;
  end;
end;

function StrToFloatSafe(const S: string): Float;
var
  Temp: string;
  I, J, K: Integer;
  SwapSeparators, IsNegative: Boolean;
  DecSep, ThouSep, C: Char;
  {$IFDEF CLR}
  sb: StringBuilder;
  {$ENDIF CLR}
begin
  {$IFDEF CLR}
  DecSep := Char(DecimalSeparator[1]);
  ThouSep := Char(ThousandSeparator[1]);
  {$ELSE}
  DecSep := DecimalSeparator;
  ThouSep := ThousandSeparator;
  {$ENDIF CLR}
  Temp := S;
  SwapSeparators := False;

  IsNegative := False;
  J := 0;
  for I := 1 to Length(Temp) do
  begin
    C := Temp[I];
    if C = '-' then
      IsNegative := not IsNegative
    else
      if (C <> ' ') and (C <> '(') and (C <> '+') then
      begin
        // if it appears prior to any digit, it has to be a decimal separator
        SwapSeparators := Temp[I] = ThouSep;
        J := I;
        Break;
      end;
  end;

  if not SwapSeparators then
  begin
    K := CharPos(Temp, DecSep);
    SwapSeparators :=
      // if it appears prior to any digit, it has to be a decimal separator
      (K > J) and
      // if it appears multiple times, it has to be a thousand separator
      ((StrCharCount(Temp, DecSep) > 1) or
      // we assume (consistent with Windows Platform SDK documentation),
      // that thousand separators appear only to the left of the decimal
      (K < CharPos(Temp, ThouSep)));
  end;

  if SwapSeparators then
  begin
    // assume a numerical string from a different locale,
    // where DecimalSeparator and ThousandSeparator are exchanged
    {$IFDEF CLR}
    sb := StringBuilder.Create(Temp);
    for I := 0 to sb.Length - 1 do
      if sb[I] = DecimalSeparator then
        sb[I] := ThouSep
      else
      if sb[I] = ThousandSeparator then
        sb[I] := DecSep;
    Temp := sb.ToString;
    {$ELSE}
    for I := 1 to Length(Temp) do
      if Temp[I] = DecSep then
        Temp[I] := ThouSep
      else
        if Temp[I] = ThouSep then
          Temp[I] := DecSep;
    {$ENDIF CLR}
  end;

  Temp := StrKeepChars(Temp, AnsiDecDigits + [AnsiChar(DecSep)]);

  if Length(Temp) > 0 then
  begin
    if Temp[1] = DecSep then
      Temp := '0' + Temp;
    if Temp[Length(Temp)] = DecSep then
      Temp := Temp + '0';
    Result := StrToFloat(Temp);
    if IsNegative then
      Result := -Result;
  end
  else
    Result := 0.0;
end;

function StrToIntSafe(const S: string): Integer;
begin
  Result := Trunc(StrToFloatSafe(S));
end;

procedure StrNormIndex(const StrLen: Integer; var Index: Integer; var Count: Integer); overload;
begin
   Index := Max(1, Min(Index, StrLen+1));
   Count := Max(0, Min(Count, StrLen+1 - Index));
end;

{$IFDEF CLR}
function ArrayOf(List: TStrings): TDynStringArray;
var
  I: Integer;
begin
  if List <> nil then
  begin
    SetLength(Result, List.Count);
    for I := 0 to List.Count - 1 do
      Result[I] := List[I];
  end
  else
    Result := nil;
end;
{$ENDIF CLR}

{$IFDEF COMPILER5} // missing Delphi 5 functions
function TryStrToInt(const S: string; out Value: Integer): Boolean;
var
  Err: Integer;
begin
  Val(S, Value, Err);
  Result := Err = 0;
end;

function TryStrToInt64(const S: string; out Value: Int64): Boolean;
var
  Err: Integer;
begin
  Val(S, Value, Err);
  Result := Err = 0;
end;

function TryStrToFloat(const S: string; out Value: Extended): Boolean;
begin
  Result := TextToFloat(PChar(S), Value, fvExtended);
end;

function TryStrToFloat(const S: string; out Value: Double): Boolean;
var
  F: Extended;
begin
  Result := TryStrToFloat(S, F);
  if Result then
    Value := F;
end;

function TryStrToFloat(const S: string; out Value: Single): Boolean;
var
  F: Extended;
begin
  Result := TryStrToFloat(S, F);
  if Result then
    Value := F;
end;

function TryStrToCurr(const S: string; out Value: Currency): Boolean;
begin
  Result := TextToFloat(PChar(S), Value, fvCurrency);
end;
{$ENDIF COMPILER5}

{$IFDEF CLR}

function DotNetFormat(const Fmt: string; const Args: array of System.Object): string;
begin
  Result := System.String.Format(Fmt, Args);
end;

function DotNetFormat(const Fmt: string; const Arg0: System.Object): string;
begin
  Result := System.String.Format(Fmt, Arg0);
end;

function DotNetFormat(const Fmt: string; const Arg0, Arg1: System.Object): string;
begin
  Result := System.String.Format(Fmt, Arg0, Arg1);
end;

function DotNetFormat(const Fmt: string; const Arg0, Arg1, Arg2: System.Object): string;
begin
  Result := System.String.Format(Fmt, Arg0, Arg1, Arg2);
end;

{$ELSE}

const
  BoolToStr: array [Boolean] of string[5] = ('false', 'true');
  {$IFDEF COMPILER5}
  MaxCurrency: Currency =  922337203685477.5807;

  varShortInt = $0010; { vt_i1     16 }
  varWord     = $0012; { vt_ui2    18 }
  varLongWord = $0013; { vt_ui4    19 }
  varInt64    = $0014; { vt_i8     20 }
  {$ENDIF COMPILER5}

type
  TInterfacedObjectAccess = class(TInterfacedObject);

procedure MoveChar(const Source; var Dest; Count: Integer);
begin
  if Count > 0 then
    Move(Source, Dest, Count * SizeOf(Char));
end;

function DotNetFormat(const Fmt: string; const Arg0: Variant): string;
begin
  Result := DotNetFormat(Fmt, [Arg0]);
end;

function DotNetFormat(const Fmt: string; const Arg0, Arg1: Variant): string;
begin
  Result := DotNetFormat(Fmt, [Arg0, Arg1]);
end;

function DotNetFormat(const Fmt: string; const Arg0, Arg1, Arg2: Variant): string;
begin
  Result := DotNetFormat(Fmt, [Arg0, Arg1, Arg2]);
end;

function DotNetFormat(const Fmt: string; const Args: array of const): string;
var
  F, P: PChar;
  Len, Capacity, Count: Integer;
  Index, ErrorCode: Integer;
  S: string;

  procedure Grow(Count: Integer);
  begin
    if Len + Count > Capacity then
    begin
      Capacity := Capacity * 5 div 3 + Count;
      SetLength(Result, Capacity);
    end;
  end;

  function InheritsFrom(AClass: TClass; const ClassName: string): Boolean;
  begin
    Result := True;
    while AClass <> nil do
    begin
      if CompareText(AClass.ClassName, ClassName) = 0 then
        Exit;
      AClass := AClass.ClassParent;
    end;
    Result := False;
  end;

  function GetStringOf(const V: TVarData; Index: Integer): string; overload;
  begin
    case V.VType of
      varEmpty, varNull:
        raise ArgumentNullException.CreateRes(@RsArgumentIsNull);
      varSmallInt:
        Result := IntToStr(V.VSmallInt);
      varInteger:
        Result := IntToStr(V.VInteger);
      varSingle:
        Result := FloatToStr(V.VSingle);
      varDouble:
        Result := FloatToStr(V.VDouble);
      varCurrency:
        Result := CurrToStr(V.VCurrency);
      varDate:
        Result := DateTimeToStr(V.VDate);
      varOleStr:
        Result := V.VOleStr;
      varBoolean:
        Result := BoolToStr[V.VBoolean <> False];
      varByte:
        Result := IntToStr(V.VByte);
      {$IFDEF COMPILER5}
      varWord:
        Result := IntToStr(Word(V.VSmallint));
      varShortInt:
        Result := IntToStr(ShortInt(V.VByte));
      varLongWord:
        Result := IntToStr(V.VError);
      {$ELSE}
      varWord:
        Result := IntToStr(V.VWord);
      varShortInt:
        Result := IntToStr(V.VShortInt);
      varLongWord:
        Result := IntToStr(V.VLongWord);
      varInt64:
        Result := IntToStr(V.VInt64);
      {$ENDIF COMPILER5}
      varString:
        Result := string(V.VString);

      {varArray,
      varDispatch,
      varError,
      varUnknown,
      varAny,
      varByRef:}
    else
      raise ArgumentNullException.CreateResFmt(@RsDotNetFormatArgumentNotSupported, [Index]);
    end;
  end;

  function GetStringOf(Index: Integer): string; overload;
  var
    V: TVarRec;
    Intf: IToString;
  begin
    V := Args[Index];
    if (V.VInteger = 0) and
       (V.VType in [vtExtended, vtString, vtObject, vtClass, vtCurrency,
                    vtInterface, vtInt64]) then
      raise ArgumentNullException.CreateResFmt(@RsArgumentIsNull, [Index]);

    case V.VType of
      vtInteger:
        Result := IntToStr(V.VInteger);
      vtBoolean:
        Result := BoolToStr[V.VBoolean];
      vtChar:
        Result := V.VChar;
      vtExtended:
        Result := FloatToStr(V.VExtended^);
      vtString:
        Result := V.VString^;
      vtPointer:
        Result := IntToHex(DWORD_PTR(V.VPointer), 8);
      vtPChar:
        Result := V.VPChar;
      vtObject:
        if (V.VObject is TInterfacedObject) and V.VObject.GetInterface(IToString, Intf) then
        begin
          Result := Intf.ToString;
          Pointer(Intf) := nil; // do not release the object
          // undo the RefCount change
          Dec(TInterfacedObjectAccess(V.VObject).FRefCount);
        end
        else
        if InheritsFrom(V.VObject.ClassType, 'TComponent') and V.VObject.GetInterface(IToString, Intf) then
          Result := Intf.ToString
        else
          raise ArgumentNullException.CreateResFmt(@RsDotNetFormatArgumentNotSupported, [Index]);
      vtClass:
        Result := V.VClass.ClassName;
      vtWideChar:
        Result := V.VWideChar;
      vtPWideChar:
        Result := V.VPWideChar;
      vtAnsiString:
        Result := string(V.VAnsiString);
      vtCurrency:
        Result := CurrToStr(V.VCurrency^);
      vtVariant:
        Result := GetStringOf(TVarData(V.VVariant^), Index);
      vtInterface:
        if IInterface(V.VInterface).QueryInterface(IToString, Intf) = 0 then
          Result := IToString(Intf).ToString
        else
          raise ArgumentNullException.CreateResFmt(@RsDotNetFormatArgumentNotSupported, [Index]);
      vtWideString:
        Result := WideString(V.VWideString);
      vtInt64:
        Result := IntToStr(V.VInt64^);
    else
      raise ArgumentNullException.CreateResFmt(@RsDotNetFormatArgumentNotSupported, [Index]);
    end;
  end;

begin
  if Length(Args) = 0 then
  begin
    Result := Fmt;
    Exit;
  end;
  Len := 0;
  Capacity := Length(Fmt);
  SetLength(Result, Capacity);
  if Capacity = 0 then
    raise ArgumentNullException.CreateRes(@RsDotNetFormatNullFormat);

  P := Pointer(Fmt);
  F := P;
  while True do
  begin
    if (P[0] = #0) or (P[0] = '{') then
    begin
      Count := P - F;
      Inc(P);
      if (P[-1] <> #0) and (P[0] = '{') then
        Inc(Count); // include '{'

      if Count > 0 then
      begin
        Grow(Count);
        MoveChar(F[0], Result[Len + 1], Count);
        Inc(Len, Count);
      end;

      if P[-1] = #0 then
        Break;

      if P[0] <> '{' then
      begin
        F := P;
        Inc(P);
        while (P[0] <> #0) and (P[0] <> '}') do
          Inc(P);
        SetString(S, F, P - F);
        Val(S, Index, ErrorCode);
        if ErrorCode <> 0 then
          raise FormatException.CreateRes(@RsFormatException);
        if (Index < 0) or (Index > High(Args)) then
          raise FormatException.CreateRes(@RsFormatException);
        S := GetStringOf(Index);
        if S <> '' then
        begin
          Grow(Length(S));
          MoveChar(S[1], Result[Len + 1], Length(S));
          Inc(Len, Length(S));
        end;

        if P[0] = #0 then
          Break;
      end;
      F := P + 1;
    end
    else
    if (P[0] = '}') and (P[1] = '}') then
    begin
      Count := P - F + 1;
      Inc(P); // skip next '}'

      Grow(Count);
      MoveChar(F[0], Result[Len + 1], Count);
      Inc(Len, Count);
      F := P + 1;
    end;

    Inc(P);
  end;

  SetLength(Result, Len);
end;

//=== { TStringBuilder } =====================================================

constructor TStringBuilder.Create(Capacity: Integer; MaxCapacity: Integer);
begin
  inherited Create;
  SetLength(FChars, Capacity);
  FMaxCapacity := MaxCapacity;
end;

constructor TStringBuilder.Create(const Value: string; Capacity: Integer);
begin
  Create(Capacity);
  Append(Value);
end;

constructor TStringBuilder.Create(const Value: string; StartIndex,
  Length, Capacity: Integer);
begin
  Create(Capacity);
  Append(Value, StartIndex + 1, Length);
end;

function TStringBuilder.ToString: string;
begin
  if FLength > 0 then
    SetString(Result, PChar(@FChars[0]), FLength)
  else
    Result := '';
end;

function TStringBuilder.EnsureCapacity(Capacity: Integer): Integer;
begin
  if System.Length(FChars) < Capacity then
    SetCapacity(Capacity);
  Result := System.Length(FChars);
end;

procedure TStringBuilder.SetCapacity(const Value: Integer);
begin
  if Value <> System.Length(FChars) then
  begin
    SetLength(FChars, Value);
    if Value < FLength then
      FLength := Value;
  end;
end;

function TStringBuilder.GetChars(Index: Integer): Char;
begin
  Result := FChars[Index];
end;

procedure TStringBuilder.SetChars(Index: Integer; const Value: Char);
begin
  FChars[Index] := Value;
end;

procedure TStringBuilder.Set_Length(const Value: Integer);
begin
  FLength := Value;
end;

function TStringBuilder.GetCapacity: Integer;
begin
  Result := System.Length(FChars);
end;

function TStringBuilder.AppendPChar(Value: PChar; Count: Integer; RepeatCount: Integer): TStringBuilder;
var
  Capacity: Integer;
begin
  if (Count > 0) and (RepeatCount > 0) then
  begin
    repeat
      Capacity := System.Length(FChars);
      if Capacity + Count > MaxCapacity then
        raise ArgumentOutOfRangeException.CreateRes(@RsArgumentOutOfRange);
      if Capacity < FLength + Count then
        SetLength(FChars, Capacity * 5 div 3 + Count);
      if Count = 1 then
        FChars[FLength] := Value[0]
      else
        MoveChar(Value[0], FChars[FLength], Count);
      Inc(FLength, Count);
      Dec(RepeatCount);
    until RepeatCount <= 0;
  end;
  Result := Self;
end;

function TStringBuilder.InsertPChar(Index: Integer; Value: PChar; Count,
  RepeatCount: Integer): TStringBuilder;
var
  Capacity: Integer;
begin
  if (Index < 0) or (Index > FLength) then
    raise ArgumentOutOfRangeException.CreateRes(@RsArgumentOutOfRange);

  if Index = FLength then
    AppendPChar(Value, Count, RepeatCount)
  else
  if (Count > 0) and (RepeatCount > 0) then
  begin
    repeat
      Capacity := System.Length(FChars);
      if Capacity + Count > MaxCapacity then
        raise ArgumentOutOfRangeException.CreateRes(@RsArgumentOutOfRange);
      if Capacity < FLength + Count then
        SetLength(FChars, Capacity * 5 div 3 + Count);
      MoveChar(FChars[Index], FChars[Index + Count], FLength - Index);
      if Count = 1 then
        FChars[Index] := Value[0]
      else
        MoveChar(Value[0], FChars[Index], Count);
      Inc(FLength, Count);

      Dec(RepeatCount);

      Inc(Index, Count); // little optimization
    until RepeatCount <= 0;
  end;
  Result := Self;
end;

function TStringBuilder.Append(const Value: array of Char): TStringBuilder;
var
  Len: Integer;
begin
  Len := System.Length(Value);
  if Len > 0 then
    AppendPChar(@Value[0], Len);
  Result := Self;
end;

function TStringBuilder.Append(const Value: array of Char; StartIndex, Length: Integer): TStringBuilder;
var
  Len: Integer;
begin
  Len := System.Length(Value);
  if (Length > 0) and (StartIndex < Len) then
  begin
    if StartIndex + Length > Len then
      Length := Len - StartIndex;
    AppendPChar(PChar(@Value[0]) + StartIndex, Length);
  end;
  Result := Self;
end;

function TStringBuilder.Append(Value: Char; RepeatCount: Integer = 1): TStringBuilder;
begin
  Result := AppendPChar(@Value, 1, RepeatCount);
end;

function TStringBuilder.Append(const Value: string): TStringBuilder;
var
  Len: Integer;
begin
  Len := System.Length(Value);
  if Len > 0 then
    AppendPChar(Pointer(Value), Len);
  Result := Self;
end;

function TStringBuilder.Append(const Value: string; StartIndex, Length: Integer): TStringBuilder;
var
  Len: Integer;
begin
  Len := System.Length(Value);
  if (Length > 0) and (StartIndex < Len) then
  begin
    if StartIndex + Length > Len then
      Length := Len - StartIndex;
    AppendPChar(PChar(Pointer(Value)) + StartIndex, Length);
  end;
  Result := Self;
end;

function TStringBuilder.Append(Value: Boolean): TStringBuilder;
begin
  Result := Append(BoolToStr[Value]);
end;

function TStringBuilder.Append(Value: Cardinal): TStringBuilder;
begin
  Result := Append(IntToStr(Value));
end;

function TStringBuilder.Append(Value: Integer): TStringBuilder;
begin
  Result := Append(IntToStr(Value));
end;

function TStringBuilder.Append(Value: Double): TStringBuilder;
begin
  Result := Append(FloatToStr(Value));
end;

function TStringBuilder.Append(Value: Int64): TStringBuilder;
begin
  Result := Append(IntToStr(Value));
end;

function TStringBuilder.Append(Obj: TObject): TStringBuilder;
begin
  Result := Append(DotNetFormat('{0}', [Obj]));
end;

function TStringBuilder.AppendFormat(const Fmt: string; Arg0: Variant): TStringBuilder;
begin
  Result := Append(DotNetFormat(Fmt, [Arg0]));
end;

function TStringBuilder.AppendFormat(const Fmt: string; Arg0, Arg1: Variant): TStringBuilder;
begin
  Result := Append(DotNetFormat(Fmt, [Arg0, Arg1]));
end;

function TStringBuilder.AppendFormat(const Fmt: string; Arg0, Arg1, Arg2: Variant): TStringBuilder;
begin
  Result := Append(DotNetFormat(Fmt, [Arg0, Arg1, Arg2]));
end;

function TStringBuilder.AppendFormat(const Fmt: string; const Args: array of const): TStringBuilder;
begin
  Result := Append(DotNetFormat(Fmt, Args));
end;

function TStringBuilder.Insert(Index: Integer; const Value: array of Char): TStringBuilder;
var
  Len: Integer;
begin
  Len := System.Length(Value);
  if Len > 0 then
    InsertPChar(Index, @Value[0], Len);
  Result := Self;
end;

function TStringBuilder.Insert(Index: Integer; const Value: string; Count: Integer): TStringBuilder;
var
  Len: Integer;
begin
  Len := System.Length(Value);
  if Len > 0 then
    InsertPChar(Index, Pointer(Value), Len, Count);
  Result := Self;
end;

function TStringBuilder.Insert(Index: Integer; Value: Boolean): TStringBuilder;
begin
  Result := Insert(Index, BoolToStr[Value]);
end;

function TStringBuilder.Insert(Index: Integer; const Value: array of Char;
  StartIndex, Length: Integer): TStringBuilder;
var
  Len: Integer;
begin
  Len := System.Length(Value);
  if (Length > 0) and (StartIndex < Len) then
  begin
    if StartIndex + Length > Len then
      Length := Len - StartIndex;
    InsertPChar(Index, PChar(@Value[0]) + StartIndex, Length);
  end;
  Result := Self;
end;

function TStringBuilder.Insert(Index: Integer; Value: Double): TStringBuilder;
begin
  Result := Insert(Index, FloatToStr(Value));
end;

function TStringBuilder.Insert(Index: Integer; Value: Int64): TStringBuilder;
begin
  Result := Insert(Index, IntToStr(Value));
end;

function TStringBuilder.Insert(Index: Integer; Value: Cardinal): TStringBuilder;
begin
  Result := Insert(Index, IntToStr(Value));
end;

function TStringBuilder.Insert(Index, Value: Integer): TStringBuilder;
begin
  Result := Insert(Index, IntToStr(Value));
end;

function TStringBuilder.Insert(Index: Integer; Obj: TObject): TStringBuilder;
begin
  Result := Insert(Index, Format('{0}', [Obj]));
end;

function TStringBuilder.Remove(StartIndex, Length: Integer): TStringBuilder;
begin
  if (StartIndex < 0) or (Length < 0) or (StartIndex + Length > FLength) then
    raise ArgumentOutOfRangeException.CreateRes(@RsArgumentOutOfRange);
  if Length > 0 then
  begin
    MoveChar(FChars[StartIndex + Length], FChars[StartIndex], Length);
    Dec(FLength, Length);
  end;
  Result := Self;
end;

function TStringBuilder.Replace(OldChar, NewChar: Char; StartIndex,
  Count: Integer): TStringBuilder;
var
  I: Integer;
begin
  if Count = -1 then
    Count := FLength;
  if (StartIndex < 0) or (Count < 0) or (StartIndex + Count > FLength) then
    raise ArgumentOutOfRangeException.CreateRes(@RsArgumentOutOfRange);
  if (Count > 0) and (OldChar <> NewChar) then
  begin
    for I := StartIndex to StartIndex + Length - 1 do
      if FChars[I] = OldChar then
        FChars[I] := NewChar;
  end;
  Result := Self;
end;

function TStringBuilder.Replace(OldValue, NewValue: string; StartIndex, Count: Integer): TStringBuilder;
var
  I: Integer;
  Offset: Integer;
  NewLen, OldLen, Capacity: Integer;
begin
  if Count = -1 then
    Count := FLength;
  if (StartIndex < 0) or (Count < 0) or (StartIndex + Count > FLength) then
    raise ArgumentOutOfRangeException.CreateRes(@RsArgumentOutOfRange);
  if OldValue = '' then
    raise ArgumentException.CreateResFmt(@RsArgumentIsNull, [0]);

  if (Count > 0) and (OldValue <> NewValue) then
  begin
    OldLen := System.Length(OldValue);
    NewLen := System.Length(NewValue);
    Offset := NewLen - OldLen;
    Capacity := System.Length(FChars);
    for I := StartIndex to StartIndex + Length - 1 do
      if FChars[I] = OldValue[1] then
      begin
        if OldLen > 1 then
          if StrLComp(@FChars[I + 1], PChar(OldValue) + 1, OldLen - 1) <> 0 then
            Continue;
        if Offset <> 0 then
        begin
          if FLength - OldLen + NewLen > MaxCurrency then
            raise ArgumentOutOfRangeException.CreateRes(@RsArgumentOutOfRange);
          if Capacity < FLength + Offset then
          begin
            Capacity := Capacity * 5 div 3 + Offset;
            SetLength(FChars, Capacity);
          end;
          if Offset < 0 then
            MoveChar(FChars[I - Offset], FChars[I], FLength - I)
          else
            MoveChar(FChars[I + OldLen], FChars[I + OldLen + Offset], FLength - OldLen - I);
          Inc(FLength, Offset);
        end;
        if NewLen > 0 then
        begin
          if (OldLen = 1) and (NewLen = 1) then
            FChars[I] := NewValue[1]
          else
            MoveChar(NewValue[1], FChars[I], NewLen);
        end;
      end;
  end;
  Result := Self;
end;
{$ENDIF CLR}

{$IFNDEF CLR}
function StrExpandTabs(S: string): string;
begin
  // use an empty tab set, which will default to a tab width of 2
  Result := TJclTabSet(nil).Expand(s);
end;

function StrExpandTabs(S: string; TabWidth: Integer): string;
var
  TabSet: TJclTabSet;
begin
  // create a tab set with no tab stops and the given tab width
  TabSet := TJclTabSet.Create(TabWidth);
  try
    Result := TabSet.Expand(S);
  finally
    TabSet.Free;
  end;
end;

function StrExpandTabs(S: string; TabSet: TJclTabSet): string;
begin
  // use the provided tab set to perform the expansion
  Result := TabSet.Expand(S);
end;

function StrOptimizeTabs(S: string): string;
begin
  // use an empty tab set, which will default to a tab width of 2
  Result := TJclTabSet(nil).Optimize(s);
end;

function StrOptimizeTabs(S: string; TabWidth: Integer): string;
var
  TabSet: TJclTabSet;
begin
  // create a tab set with no tab stops and the given tab width
  TabSet := TJclTabSet.Create(TabWidth);
  try
    Result := TabSet.Optimize(S);
  finally
    TabSet.Free;
  end;
end;

function StrOptimizeTabs(S: string; TabSet: TJclTabSet): string;
begin
  // use the provided tab set to perform the optimization
  Result := TabSet.Optimize(S);
end;

//=== { TJclTabSet } =====================================================

constructor TJclTabSet.Create;
begin
  // no tab stops, tab width set to auto
  Create([], True, 0);
end;

constructor TJclTabSet.Create(TabWidth: Integer);
begin
  // no tab stops, specified tab width
  Create([], True, TabWidth);
end;

constructor TJclTabSet.Create(const Tabstops: array of Integer; ZeroBased: Boolean);
begin
  // specified tab stops, tab width equal to distance between last two tab stops
  Create(Tabstops, ZeroBased, 0);
end;

constructor TJclTabSet.Create(const Tabstops: array of Integer; ZeroBased: Boolean; TabWidth: Integer);
var
  idx: Integer;
begin
  inherited Create;
  for idx := 0 to High(Tabstops) do
    Add(Tabstops[idx]);
  FWidth := TabWidth;
  FZeroBased := ZeroBased;
  CalcRealWidth;
end;

function TJclTabSet.Add(Column: Integer): Integer;
begin
  if Self = nil then
    raise NullReferenceException.Create;
  if Column < StartColumn then
    raise ArgumentOutOfRangeException.Create('Column');
  Result := FindStop(Column);
  if Result < 0 then
  begin
    // the column doesn't exist; invert the result of FindStop to get the correct index position
    Result := not Result;
    // increase the tab stop array
    SetLength(FStops, Length(FStops) + 1);
    // make room at the insert position
    MoveArray(FStops, Result, Result + 1, High(FStops) - Result);
    // add the tab stop at the correct location
    FStops[Result] := Column;
    CalcRealWidth;
  end
  else
  begin
    {$IFDEF CLR}
    raise EJclStringError.Create(RsTabs_DuplicatesNotAllowed);
    {$ELSE}
    raise EJclStringError.CreateRes(@RsTabs_DuplicatesNotAllowed);
    {$ENDIF}
  end;
end;

procedure TJclTabSet.CalcRealWidth;
begin
  if FWidth < 1 then
  begin
    if Length(FStops) > 1 then
      FRealWidth := FStops[High(FStops)] - FStops[Pred(High(FStops))]
    else
    if Length(FStops) = 1 then
      FRealWidth := FStops[0]
    else
      FRealWidth := 2;
  end
  else
    FRealWidth := FWidth;
end;

function TJclTabSet.Delete(Column: Integer): Integer;
begin
  Result := FindStop(Column);
  if Result >= 0 then
    RemoveAt(Result);
end;

function TJclTabSet.Expand(const S: string): string;
begin
  Result := Expand(s, StartColumn);
end;

function TJclTabSet.Expand(const S: string; Column: Integer): string;
var
  sb: TStringBuilder;
  head: PChar;
  cur: PChar;
begin
  if Column < StartColumn then
    raise ArgumentOutOfRangeException.Create('Column');
  sb := TStringBuilder.Create(Length(S));
  try
    cur := PChar(S);
    while cur^ <> #0 do
    begin
      head := cur;
      while (cur^<> #0) and (cur^ <> #9) do
      begin
        if cur^ in [#10, #13] then
          Column := StartColumn
        else
          Inc(Column);
        Inc(cur);
      end;
      if cur > head then
        sb.Append(head, 0, cur - head);
      if cur^ = #9 then
      begin
        sb.Append(' ', TabFrom(Column) - Column);
        Column := TabFrom(Column);
        Inc(cur);
      end;
    end;
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function TJclTabSet.FindStop(Column: Integer): Integer;
begin
  if Self <> nil then
  begin
    Result := High(FStops);
    while (Result >= 0) and (FStops[Result] > Column) do
      Dec(Result);
    if (Result >= 0) and (FStops[Result] <> Column) then
      Result := not Succ(Result);
  end
  else
    Result := -1;
end;

class function TJclTabSet.FromString(const S: string): TJclTabSet;
var
  cur: PChar;

  procedure SkipWhiteSpace;
  begin
    while CharIsWhiteSpace(cur^) do
      Inc(cur);
  end;

  function ParseNumber: Integer;
  var
    head: PChar;
  begin
    SkipWhiteSpace;
    head := cur;
    while cur^ in ['0'..'9'] do
      Inc(cur);
    if (cur <= head) or not TryStrToInt(Copy(head, 1, cur - head), Result) then
      Result := -1;
  end;

  procedure ParseStops;
  var
    openBracket: Boolean;
    num: Integer;
    hadComma: Boolean;
  begin
    SkipWhiteSpace;
    openBracket := cur^ = '[';
    hadComma := False;
    if openBracket then
      Inc(cur);
    repeat
      num := ParseNumber;
      if (num < 0) and hadComma then
        {$IFDEF CLR}
        raise EJclStringError.Create(RsTabs_StopExpected)
        {$ELSE}
        raise EJclStringError.CreateRes(@RsTabs_StopExpected)
        {$ENDIF}
      else
      if num >= 0 then
        Result.Add(num);
      SkipWhiteSpace;
      hadComma := cur^ = ',';
      if hadComma then
        Inc(cur);
    until (cur^ in [#0, '+', ']']);
    if hadComma then
      {$IFDEF CLR}
      raise EJclStringError.Create(RsTabs_StopExpected)
      {$ELSE}
      raise EJclStringError.CreateRes(@RsTabs_StopExpected)
      {$ENDIF}
    else
    if openBracket and (cur^ <> ']') then
      {$IFDEF CLR}
      raise EJclStringError.Create(RsTabs_CloseBracketExpected)
      {$ELSE}
      raise EJclStringError.CreateRes(@RsTabs_CloseBracketExpected)
      {$ENDIF}
  end;

  procedure ParseTabWidth;
  var
    num: Integer;
  begin
    SkipWhiteSpace;
    if cur^ = '+' then
    begin
      Inc(cur);
      SkipWhiteSpace;
      num := ParseNumber;
      if (num < 0) then
        {$IFDEF CLR}
        raise EJclStringError.Create(RsTabs_TabWidthExpected)
        {$ELSE}
        raise EJclStringError.CreateRes(@RsTabs_TabWidthExpected)
        {$ENDIF}
      else
        Result.TabWidth := num;
    end;
  end;

  procedure ParseZeroBasedFlag;
  begin
    SkipWhiteSpace;
    if cur^ = '0' then
    begin
      Inc(cur);
      if CharIsWhiteSpace(cur^) or (cur^ in [#0, '[']) then
      begin
        Result.ZeroBased := True;
        SkipWhiteSpace;
      end
      else
        Dec(cur);
    end;
  end;

begin
  Result := TJclTabSet.Create;
  try
    Result.ZeroBased := False;
    cur := PChar(S);
    ParseZeroBasedFlag;
    ParseStops;
    ParseTabWidth;
  except
    // clean up the partially complete instance (to avoid memory leaks)...
    Result.Free;
    // ... and re-raise the exception
    raise;
  end;
end;

function TJclTabSet.GetCount: Integer;
begin
  if Self <> nil then
    Result := Length(FStops)
  else
    Result := 0;
end;

function TJclTabSet.GetStops(Index: Integer): Integer;
begin
  if Self <> nil then
  begin
    if (Index < 0) or (Index >= Length(FStops)) then
    begin
      {$IFDEF CLR}
      raise EJclStringError.Create(RsArgumentOutOfRange);
      {$ELSE}
      raise EJclStringError.CreateRes(@RsArgumentOutOfRange);
      {$ENDIF CLR}
    end
    else
      Result := FStops[Index]
  end
  else
  begin
    {$IFDEF CLR}
    raise EJclStringError.Create(RsArgumentOutOfRange);
    {$ELSE}
    raise EJclStringError.CreateRes(@RsArgumentOutOfRange);
    {$ENDIF CLR}
  end;
end;

function TJclTabSet.GetTabWidth: Integer;
begin
  if Self <> nil then
    Result := FWidth
  else
    Result := 0;
end;

function TJclTabSet.GetZeroBased: Boolean;
begin
  Result := (Self = nil) or FZeroBased;
end;

procedure TJclTabSet.OptimalFillInfo(StartColumn, TargetColumn: Integer; out TabsNeeded, SpacesNeeded: Integer);
var
  nextTab: Integer;
begin
  if StartColumn < Self.StartColumn then  // starting column less than 1 or 0 (depending on ZeroBased state)
    raise ArgumentOutOfRangeException.Create('StartColumn');
  if (TargetColumn < StartColumn) then    // target lies before the starting column
    raise ArgumentOutOfRangeException.Create('TargetColumn');
  TabsNeeded := 0;
  repeat
    nextTab := TabFrom(StartColumn);
    if nextTab <= TargetColumn then
    begin
      Inc(TabsNeeded);
      StartColumn := nextTab;
    end;
  until nextTab > TargetColumn;
  SpacesNeeded := TargetColumn - StartColumn;
end;

function TJclTabSet.Optimize(const S: string): string;
begin
  Result := Optimize(S, StartColumn);
end;

function TJclTabSet.Optimize(const S: string; Column: Integer): string;
var
  sb: TStringBuilder;
  head: PChar;
  cur: PChar;
  tgt: Integer;

  procedure AppendOptimalWhiteSpace(Target: Integer);
  var
    tabCount: Integer;
    spaceCount: Integer;
  begin
    if cur > head then
    begin
      OptimalFillInfo(Column, Target, tabCount, spaceCount);
      if tabCount > 0 then
        sb.Append(#9, tabCount);
      if spaceCount > 0 then
        sb.Append(' ', spaceCount);
    end;
  end;

begin
  if Column < StartColumn then
    raise ArgumentOutOfRangeException.Create('Column');
  sb := TStringBuilder.Create(Length(S));
  try
    cur := PChar(s);
    while cur^<> #0 do
    begin
      // locate first whitespace character
      head := cur;
      while (cur^ <> #0) and not CharIsWhiteSpace(cur^) do
        Inc(cur);
      // output non whitespace characters
      if cur > head then
        sb.Append(head, 0, cur - head);
      // advance column
      Inc(Column, cur - head);
      // initialize target column indexer
      tgt := Column;
      // locate end of whitespace sequence
      while CharIsWhiteSpace(cur^) do
      begin
        if cur^ in [AnsiLineFeed, AnsiCarriageReturn] then
        begin
          // append optimized whitespace sequence...
          AppendOptimalWhiteSpace(tgt);
          // ...set the column back to the start of the line...
          Column := StartColumn;
          // ...reset target column indexer...
          tgt := Column;
          // ...add the line break character...
          sb.Append(cur^);
        end
        else
        if cur^ = #9 then
          tgt := TabFrom(tgt)       // expand the tab
        else
          Inc(tgt);                 // a normal whitespace; taking up 1 column
        Inc(cur);
      end;
      AppendOptimalWhiteSpace(tgt); // append optimized whitespace sequence...
      Column := tgt;                // ...and memorize the column for the next iteration
    end;
    Result := sb.ToString;          // convert result to a string
  finally
    sb.Free;
  end;
end;

procedure TJclTabSet.RemoveAt(Index: Integer);
begin
  if Self <> nil then
  begin
    MoveArray(FStops, Succ(Index), Index, High(FStops) - Index);
    SetLength(FStops, High(FStops));
    CalcRealWidth;
  end
  else
    raise NullReferenceException.Create;
end;

procedure TJclTabSet.SetStops(Index, Value: Integer);
var
  temp: Integer;
begin
  if Self <> nil then
  begin
    if (Index < 0) or (Index >= Length(FStops)) then
    begin
      {$IFDEF CLR}
      raise ArgumentOutOfRangeException.Create;
      {$ELSE}
      raise ArgumentOutOfRangeException.CreateRes(@RsArgumentOutOfRange);
      {$ENDIF CLR}
    end
    else
    begin
      temp := FindStop(Value);
      if temp < 0 then
      begin
        // remove existing tab stop...
        RemoveAt(Index);
        // now add the new tab stop
        Add(Value);
      end
      else
      if temp <> Index then
      begin
        // new tab stop already present at another index
        {$IFDEF CLR}
        raise EJclStringError.Create(RsTabs_DuplicatesNotAllowed);
        {$ELSE}
        raise EJclStringError.CreateRes(@RsTabs_DuplicatesNotAllowed);
        {$ENDIF}
      end;
    end;
  end
  else
    raise NullReferenceException.Create;
end;

procedure TJclTabSet.SetTabWidth(Value: Integer);
begin
  if Self <> nil then
  begin
    FWidth := Value;
    CalcRealWidth;
  end
  else
    raise NullReferenceException.Create;
end;

procedure TJclTabSet.SetZeroBased(Value: Boolean);
var
  shift: Integer;
  idx: Integer;
begin
  if Self <> nil then
  begin
    if Value <> FZeroBased then
    begin
      FZeroBased := Value;
      if Value then
        shift := -1
      else
        shift := 1;
      for idx := 0 to High(FStops) do
        FStops[idx] := FStops[idx] + shift;
    end;
  end
  else
    raise NullReferenceException.Create;
end;

function TJclTabSet.InternalTabStops: TDynIntegerArray;
begin
  if Self <> nil then
    Result := FStops
  else
    Result := nil;
end;

function TJclTabSet.InternalTabWidth: Integer;
begin
  if Self <> nil then
    Result := FRealWidth
  else
    Result := 2;
end;

function TJclTabSet.StartColumn: Integer;
begin
  if GetZeroBased then
    Result := 0
  else
    Result := 1;
end;

function TJclTabSet.TabFrom(Column: Integer): Integer;
begin
  if Column < StartColumn then
    raise ArgumentOutOfRangeException.Create('Column');
  Result := FindStop(Column);
  if Result < 0 then
    Result := not Result
  else
    Inc(Result);
  if Result >= GetCount then
  begin
    if GetCount > 0 then
      Result := FStops[High(FStops)]
    else
      Result := StartColumn;
    while Result <= Column do
      Inc(Result, ActualTabWidth);
  end
  else
    Result := FStops[Result];
end;

function TJclTabSet.ToString: string;
begin
  Result := ToString(TabSetFormatting_Full);
end;

function TJclTabSet.ToString(FormattingOptions: Integer): string;
var
  sb: TStringBuilder;
  idx: Integer;

  function WantBrackets: Boolean;
  begin
    Result := (TabSetFormatting_SurroundStopsWithBrackets and FormattingOptions) <> 0;
  end;

  function EmptyBrackets: Boolean;
  begin
    Result := (TabSetFormatting_EmptyBracketsIfNoStops and FormattingOptions) <> 0;
  end;

  function IncludeAutoWidth: Boolean;
  begin
    Result := (TabSetFormatting_AutoTabWidth and FormattingOptions) <> 0;  
  end;

  function IncludeTabWidth: Boolean;
  begin
    Result := (TabSetFormatting_NoTabWidth and FormattingOptions) = 0;
  end;

  function IncludeStops: Boolean;
  begin
    Result := (TabSetFormatting_NoTabStops and FormattingOptions) = 0;
  end;

begin
  sb := TStringBuilder.Create;
  try
    // output the fixed tabulation positions if requested...
    if IncludeStops then
    begin
      // output each individual tabulation position
      for idx := 0 to GetCount - 1 do
      begin
        sb.Append(TabStops[idx]);
        sb.Append(',');
      end;
      // remove the final comma if any tabulation positions where outputted
      if sb.Length <> 0 then
        sb.Remove(sb.Length - 1, 1);
      // bracket the tabulation positions if requested
      if WantBrackets and (EmptyBrackets or (sb.Length > 0)) then
      begin
        sb.Insert(0, '[');
        sb.Append(']');
      end;
    end;
    // output the tab width if requested....
    if IncludeTabWidth and (IncludeAutoWidth or (TabWidth > 0)) then
    begin
      // separate the tab width from any outputted tabulation positions with a whitespace
      if sb.Length > 0 then
        sb.Append(' ');
      // flag tab width
      sb.Append('+');
      // finally, output the tab width
      sb.Append(ActualTabWidth);
    end;
    // flag zero-based tabset by outputting a 0 (zero) as the first character.
    if ZeroBased then
      sb.Insert(0, string('0 '));
    Result := StrTrimCharRight(sb.ToString, ' ');
  finally
    sb.Free;
  end;
end;

function TJclTabSet.UpdatePosition(const S: string): Integer;
var
  lines: Integer;
begin
  Result := StartColumn;
  UpdatePosition(S, Result, lines);
end;

function TJclTabSet.UpdatePosition(const S: string; Column: Integer): Integer;
var
  lines: Integer;
begin
  if Column < StartColumn then
    raise ArgumentOutOfRangeException.Create('Column');
  Result := Column;
  UpdatePosition(S, Result, lines);
end;

function TJclTabSet.UpdatePosition(const S: string; var Column, Line: Integer): Integer;
var
  prevChar: Char;
  cur: PChar;
begin
  if Column < StartColumn then
    raise ArgumentOutOfRangeException.Create('Column');
  // initialize loop
  cur := PChar(S);
  // iterate until end of string (the Null-character)
  while cur^ <> #0 do
  begin
    // check for line-breaking characters
    if cur^ in [#10, #13] then
    begin
      // Column moves back all the way to the left
      Column := StartColumn;
      // If this is the first line-break character or the same line-break character, increment the Line parameter
      Inc(Line);
      // check if it's the first of a two-character line-break
      prevChar := cur^;
      Inc(cur);
      // if it isn't a two-character line-break, undo the previous advancement
      if not (cur^ in [#10, #13]) or (cur^ = prevChar) then
        Dec(cur);
    end
    else // check for tab character and expand it
    if cur^ = #9 then
      Column := TabFrom(Column)
    else // a normal character; increment column
      Inc(Column);
    // advance pointer
    Inc(cur);
  end;
  // set the result to the newly calculated column
  Result := Column;
end;

//=== { NullReferenceException } =============================================

constructor NullReferenceException.Create;
begin
  CreateRes(@RsArg_NullReferenceException);
end;

function AnsiCompareNatural(const S1, S2: string; CaseInsensitive: Boolean): Integer;
var
  Cur1: PChar;
  Cur2: PChar;

  procedure NumberCompare;
  var
    IsReallyNumber: Boolean;
    FirstDiffBreaks: Boolean;
  begin
    Result := 0;
    IsReallyNumber := False;
    // count leading spaces in S1
    while Cur1^ = ' ' do
    begin
      Dec(Result);
      Inc(Cur1);
    end;
    // count leading spaces in S2 (canceling them out against the ones in S1)
    while Cur2^ = ' ' do
    begin
      Inc(Result);
      Inc(Cur2);
    end;

    // if spaces match, or both strings are actually followed by a numeric character, continue the checks
    if (Result = 0) or ((Cur1^ in ['+', '-', '0' .. '9']) and (Cur2^ in ['+', '-', '0' .. '9'])) then
    begin
      // Check signed number
      if (Cur1^ = '-') and (Cur2^ <> '-') then
        Result := 1
      else
      if (Cur2^ = '-') and (Cur1^ <> '-') then
        Result := -1
      else
        Result := 0;

      if Cur1^ in ['-', '+'] then
        Inc(Cur1);
      if Cur2^ in ['-', '+'] then
        Inc(Cur2);

      FirstDiffBreaks := (Cur1^ = '0') or (Cur2^ = '0');
      while (Cur1^ in ['0' .. '9']) and (Cur2^ in ['0' .. '9']) do
      begin
        IsReallyNumber := True;
        if (Result = 0) and (Cur1^ < Cur2^) then
          Result := -1
        else
        if (Result = 0) and (Cur1^ > Cur2^) then
          Result := 1;
        if FirstDiffBreaks and (Result <> 0) then
          Break;
        Inc(Cur1);
        Inc(Cur2);
      end;

      if IsReallyNumber then
      begin
        if not FirstDiffBreaks then
        begin
          if Cur1^ in ['0' .. '9'] then
            Result := 1
          else
          if Cur2^ in ['0' .. '9'] then
            Result := -1;
        end;
      end;
    end;
  end;

begin
  Cur1 := PChar(S1);
  Cur2 := PChar(S2);
  Result := 0;
  while (Result = 0) do
  begin
    if (Cur1^ = #0) and (Cur2^ = #0) then
      Break
    else
    if (Cur1^ = '-') and (Cur2^ in ['+','0' .. '9']) then
      Result := -1
    else
    if (Cur2^ = '-') and (Cur1^ in ['+','0' .. '9']) then
      Result := 1
    else
    if (Cur1^ in ['+', '-', ' ', '0' .. '9']) and (Cur2^ in ['+', '-', ' ', '0' .. '9']) then
      NumberCompare
    else
    if (Cur1^ = #0) and (Cur2^ <> #0) then
      Result := -1
    else
    if (Cur1^ <> #0) and (Cur1^ = #0) then
      Result := 1
    else
    begin
      if CaseInsensitive then
        Result := AnsiStrLIComp(Cur1, Cur2, 1)
      else
        Result := AnsiStrLComp(Cur1, Cur2, 1);
      Inc(Cur1);
      Inc(Cur2);
    end;
  end;
end;

function AnsiCompareNaturalStr(const S1, S2: AnsiString): Integer; overload;
begin
  Result := AnsiCompareNatural(S1, S2, False);
end;

function AnsiCompareNaturalText(const S1, S2: AnsiString): Integer; overload;
begin
  Result := AnsiCompareNatural(S1, S2, True);
end;

{$ENDIF ~CLR}

{$IFDEF CLR}
{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);
{$ENDIF UNITVERSIONING}
{$ELSE}
initialization
  LoadCharTypes;  // this table first
  LoadCaseMap;    // or this function does not work
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
{$ENDIF CLR}

{$IFDEF UNITVERSIONING}
finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
