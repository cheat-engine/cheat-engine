{*******************************************************}
{               RichEdit Syntax HighLight               }
{                     version 3.0                       }
{ Author:                                               }
{ Serhiy Perevoznyk                                     }
{ serge_perevoznyk@hotmail.com                          }
{                                                       }
{*******************************************************}

{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterCpp.pas, released 2000-04-10.
The Original Code is based on the dcjCppSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Michael Trier.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: psvCPlusPlus.pas,v 1.2 2007-10-01 00:54:38 darkbyte Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a C++ syntax highlighter for SynEdit)
@author(Michael Trier)
@created(1998)
@lastmod(2001-11-21)
The SynHighlighterCpp unit provides SynEdit with a C++ syntax highlighter.
Thanks to Martin Waldenburg.
}

unit psvCPlusPlus;


interface

uses
  SysUtils, Windows, Messages, Classes, Controls, Graphics, 
  psvRichSyntax;

type
  TtkTokenKind = (tkAsm, tkComment, tkDirective, tkIdentifier, tkKey, tkNull,
    tkNumber, tkSpace, tkString, tkSymbol, tkUnknown,
    tkChar, tkFloat, tkHex, tkOctal); // dj

  TxtkTokenKind = (
    xtkAdd, xtkAddAssign, xtkAnd, xtkAndAssign, xtkArrow, xtkAssign,
    xtkBitComplement, xtkBraceClose, xtkBraceOpen, xtkColon, xtkComma,
    xtkDecrement, xtkDivide, xtkDivideAssign, xtkEllipse, xtkGreaterThan,
    xtkGreaterThanEqual, xtkIncOr, xtkIncOrAssign, xtkIncrement, xtkLessThan,
    xtkLessThanEqual, xtkLogAnd, xtkLogComplement, xtkLogEqual, xtkLogOr,
    xtkMod, xtkModAssign, xtkMultiplyAssign, xtkNotEqual, xtkPoint, xtkQuestion,
    xtkRoundClose, xtkRoundOpen, xtkScopeResolution, xtkSemiColon, xtkShiftLeft,
    xtkShiftLeftAssign, xtkShiftRight, xtkShiftRightAssign, xtkSquareClose,
    xtkSquareOpen, xtkStar, xtkSubtract, xtkSubtractAssign, xtkXor,
    xtkXorAssign);

  TRangeState = (rsUnknown, rsAnsiC, rsAnsiCAsm, rsAnsiCAsmBlock, rsAsm,
    rsAsmBlock, rsDirective, rsDirectiveComment, rsString34, rsString39,
    rsMultiLineString, rsMultiLineDirective); //dj

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

  TpsvCppRTF = class(TpsvRTFSyntax)
  private
    fAsmStart: Boolean;
    fRange: TRangeState;
    fLine: PChar;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    FExtTokenID: TxtkTokenKind;
    fLineNumber: Integer;
    fIdentFuncTable: array[0..206] of TIdentFuncTableFunc;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: String): Boolean;
    function Func17: TtkTokenKind;
    function Func21: TtkTokenKind;
    function Func32: TtkTokenKind;
    function Func34: TtkTokenKind;
    function Func36: TtkTokenKind;
    function Func40: TtkTokenKind;
    function Func42: TtkTokenKind;
    function Func45: TtkTokenKind;
    function Func46: TtkTokenKind;
    function Func48: TtkTokenKind;
    function Func52: TtkTokenKind;
    function Func54: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func58: TtkTokenKind;
    function Func59: TtkTokenKind;
    function Func60: TtkTokenKind;
    function Func61: TtkTokenKind;
    function Func62: TtkTokenKind;
    function Func64: TtkTokenKind;
    function Func65: TtkTokenKind;
    function Func66: TtkTokenKind;
    function Func67: TtkTokenKind;
    function Func68: TtkTokenKind;
    function Func69: TtkTokenKind;
    function Func71: TtkTokenKind;
    function Func74: TtkTokenKind;
    function Func75: TtkTokenKind;
    function Func76: TtkTokenKind;
    function Func78: TtkTokenKind;
    function Func79: TtkTokenKind;
    function Func81: TtkTokenKind;
    function Func82: TtkTokenKind;
    function Func85: TtkTokenKind;
    function Func86: TtkTokenKind;
    function Func88: TtkTokenKind;
    function Func89: TtkTokenKind;
    function Func92: TtkTokenKind;
    function Func97: TtkTokenKind;
    function Func98: TtkTokenKind;
    function Func100: TtkTokenKind;
    function Func101: TtkTokenKind;
    function Func102: TtkTokenKind;
    function Func104: TtkTokenKind;
    function Func105: TtkTokenKind;
    function Func106: TtkTokenKind;
    function Func107: TtkTokenKind;
    function Func109: TtkTokenKind;
    function Func110: TtkTokenKind;
    function Func115: TtkTokenKind;
    function Func116: TtkTokenKind;
    function Func123: TtkTokenKind;
    function Func125: TtkTokenKind;
    function Func141: TtkTokenKind;
    function Func206: TtkTokenKind;
    procedure AnsiCProc;
    procedure AndSymbolProc;
    procedure AsciiCharProc;
    procedure AtSymbolProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CRProc;
    procedure ColonProc;
    procedure CommaProc;
    procedure DirectiveProc;
    procedure DirectiveEndProc; //dj
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure ModSymbolProc;
    procedure NotSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure PointProc;
    procedure QuestionProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SquareCloseProc;
    procedure SquareOpenProc;
    procedure StarProc;
    procedure StringProc;
    procedure TildeProc;
    procedure XOrSymbolProc;
    procedure UnknownProc;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure StringEndProc;
  protected
    function GetExtTokenID: TxtkTokenKind;
    function GetEol: Boolean; override;
    function GetRange: Pointer;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(NewValue: String; LineNumber:Integer); override;
    function GetToken: String; override;
    function GetTokenAttribute: integer; override;
    function GetTokenKind: integer;
    function GetTokenPos: Integer;
    procedure Next; override;
    procedure SetRange(Value: Pointer);
    procedure ResetRange;
    property ExtTokenID: TxtkTokenKind read GetExtTokenID;
    procedure PrepareToken(var AToken : string); override;
    function PrepareOutput(Attr: integer; AToken : string): string;   override;
  public
    constructor Create; override;
    procedure SetupDefaultColors; override;
  end;

implementation


var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable: array[#0..#255] of Integer;

procedure MakeIdentTable;
var
  I: Char;
begin
  for I := #0 to #255 do
  begin
    Case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
    else Identifiers[I] := False;
    end;
    Case I in['_', 'a'..'z', 'A'..'Z'] of
      True:
        begin
          if (I > #64) and (I < #91) then mHashTable[I] := Ord(I) - 64 else
            if (I > #96) then mHashTable[I] := Ord(I) - 95;
        end;
    else mHashTable[I] := 0;
    end;
  end;
end;

procedure TpsvCppRTF.InitIdent;
var
  I: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do begin
    pF^ := AltFunc;
    Inc(pF);
  end;
  fIdentFuncTable[17] := Func17;
  fIdentFuncTable[21] := Func21;
  fIdentFuncTable[32] := Func32;
  fIdentFuncTable[34] := Func34;
  fIdentFuncTable[36] := Func36;
  fIdentFuncTable[40] := Func40;
  fIdentFuncTable[42] := Func42;
  fIdentFuncTable[45] := Func45;
  fIdentFuncTable[46] := Func46;
  fIdentFuncTable[48] := Func48;
  fIdentFuncTable[52] := Func52;
  fIdentFuncTable[54] := Func54;
  fIdentFuncTable[57] := Func57;
  fIdentFuncTable[58] := Func58;
  fIdentFuncTable[59] := Func59;
  fIdentFuncTable[60] := Func60;
  fIdentFuncTable[61] := Func61;
  fIdentFuncTable[62] := Func62;
  fIdentFuncTable[64] := Func64;
  fIdentFuncTable[65] := Func65;
  fIdentFuncTable[66] := Func66;
  fIdentFuncTable[67] := Func67;
  fIdentFuncTable[68] := Func68;
  fIdentFuncTable[69] := Func69;
  fIdentFuncTable[71] := Func71;
  fIdentFuncTable[74] := Func74;
  fIdentFuncTable[75] := Func75;
  fIdentFuncTable[76] := Func76;
  fIdentFuncTable[78] := Func78;
  fIdentFuncTable[79] := Func79;
  fIdentFuncTable[81] := Func81;
  fIdentFuncTable[82] := Func82;
  fIdentFuncTable[85] := Func85;
  fIdentFuncTable[86] := Func86;
  fIdentFuncTable[88] := Func88;
  fIdentFuncTable[89] := Func89;
  fIdentFuncTable[92] := Func92;
  fIdentFuncTable[97] := Func97;
  fIdentFuncTable[98] := Func98;
  fIdentFuncTable[100] := Func100;
  fIdentFuncTable[101] := Func101;
  fIdentFuncTable[102] := Func102;
  fIdentFuncTable[104] := Func104;
  fIdentFuncTable[105] := Func105;
  fIdentFuncTable[106] := Func106;
  fIdentFuncTable[107] := Func107;
  fIdentFuncTable[109] := Func109;
  fIdentFuncTable[110] := Func110;
  fIdentFuncTable[115] := Func115;
  fIdentFuncTable[116] := Func116;
  fIdentFuncTable[123] := Func123;
  fIdentFuncTable[125] := Func125;
  fIdentFuncTable[141] := Func141;
  fIdentFuncTable[206] := Func206;
end;

function TpsvCppRTF.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end; { KeyHash }

function TpsvCppRTF.KeyComp(const aKey: String): Boolean;
var
  I: Integer;
  Temp: PChar;
begin
  Temp := fToIdent;
  if Length(aKey) = fStringLen then
  begin
    Result := True;
    for i := 1 to fStringLen do
    begin
      if Temp^ <> aKey[i] then
      begin
        Result := False;
        break;
      end;
      inc(Temp);
    end;
  end else Result := False;
end; { KeyComp }

function TpsvCppRTF.Func17: TtkTokenKind;
begin
  if KeyComp('if') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func21: TtkTokenKind;
begin
  if KeyComp('do') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func32: TtkTokenKind;
begin
  if KeyComp('cdecl') then Result := tkKey else
    if KeyComp('case') then Result := tkKey else
      if KeyComp('_cdecl') then Result := tkKey else
        if KeyComp('__cdecl') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func34: TtkTokenKind;
begin
  if KeyComp('char') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func36: TtkTokenKind;
begin
  if KeyComp('asm') or KeyComp('_asm') or KeyComp('__asm') then
  begin
    Result := tkKey;
    fRange := rsAsm;
    fAsmStart := True;
  end else
    Result := tkIdentifier;
end;

function TpsvCppRTF.Func40: TtkTokenKind;
begin
  if KeyComp('catch') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func42: TtkTokenKind;
begin
  if KeyComp('for') then Result := tkKey else
    if KeyComp('break') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func45: TtkTokenKind;
begin
  if KeyComp('else') then Result := tkKey else
    if KeyComp('new') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func46: TtkTokenKind;
begin
  if KeyComp('__int8') then Result := tkKey else
    if KeyComp('__int16') then Result := tkKey else
      if KeyComp('int') then Result := tkKey else
        if KeyComp('__int32') then Result := tkKey else
          if KeyComp('__int64') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func48: TtkTokenKind;
begin
  if KeyComp('false') then Result := tkKey else
    if KeyComp('bool') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func52: TtkTokenKind;
begin
  if KeyComp('long') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func54: TtkTokenKind;
begin
  if KeyComp('void') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func57: TtkTokenKind;
begin
  if KeyComp('enum') then Result := tkKey else
    if KeyComp('delete') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func58: TtkTokenKind;
begin
  if KeyComp('_pascal') then Result := tkKey else
    if KeyComp('__pascal') then Result := tkKey else
      if KeyComp('pascal') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func59: TtkTokenKind;
begin
  if KeyComp('class') then Result := tkKey else
    if KeyComp('float') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func60: TtkTokenKind;
begin
  if KeyComp('this') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func61: TtkTokenKind;
begin
  if KeyComp('goto') then Result := tkKey else
    if KeyComp('auto') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func62: TtkTokenKind;
begin
  if KeyComp('__thread') then Result := tkKey else
    if KeyComp('while') then Result := tkKey else
      if KeyComp('friend') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func64: TtkTokenKind;
begin
  if KeyComp('signed') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func65: TtkTokenKind;
begin
  if KeyComp('double') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func66: TtkTokenKind;
begin
  if KeyComp('__try') then Result := tkKey else
    if KeyComp('try') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func67: TtkTokenKind;
begin
  if KeyComp('__dispid') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func68: TtkTokenKind;
begin
  if KeyComp('true') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func69: TtkTokenKind;
begin
  if KeyComp('public') then Result := tkKey else
    if KeyComp('inline') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func71: TtkTokenKind;
begin
  if KeyComp('__rtti') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func74: TtkTokenKind;
begin
  if KeyComp('__classid') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func75: TtkTokenKind;
begin
  if KeyComp('__declspec') then Result := tkKey else
    if KeyComp('using') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func76: TtkTokenKind;
begin
  if KeyComp('const') then Result := tkKey else
    if KeyComp('default') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func78: TtkTokenKind;
begin
  if KeyComp('_stdcall') then Result := tkKey else
    if KeyComp('union') then Result := tkKey else
      if KeyComp('__stdcall') then Result := tkKey else
        if KeyComp('static') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func79: TtkTokenKind;
begin
  if KeyComp('__except') then Result := tkKey else
    if KeyComp('wchar_t') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func81: TtkTokenKind;
begin
  if KeyComp('mutable') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func82: TtkTokenKind;
begin
  if KeyComp('_fastcall') then Result := tkKey else
    if KeyComp('__fastcall') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func85: TtkTokenKind;
begin
  if KeyComp('short') then Result := tkKey else
    if KeyComp('typeid') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func86: TtkTokenKind;
begin
  if KeyComp('sizeof') then Result := tkKey else
    if KeyComp('__finally') then Result := tkKey else
      if KeyComp('namespace') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func88: TtkTokenKind;
begin
  if KeyComp('switch') then Result := tkKey else
    if KeyComp('typedef') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func89: TtkTokenKind;
begin
  if KeyComp('throw') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func92: TtkTokenKind;
begin
  if KeyComp('extern') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func97: TtkTokenKind;
begin
  if KeyComp('__import') then Result := tkKey else
    if KeyComp('_import') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func98: TtkTokenKind;
begin
  if KeyComp('private') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func100: TtkTokenKind;
begin
  if KeyComp('template') then Result := tkKey else
    if KeyComp('__closure') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func101: TtkTokenKind;
begin
  if KeyComp('unsigned') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func102: TtkTokenKind;
begin
  if KeyComp('return') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func104: TtkTokenKind;
begin
  if KeyComp('volatile') then Result := tkKey else
    if KeyComp('_export') then Result := tkKey else
      if KeyComp('__export') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func105: TtkTokenKind;
begin
  if KeyComp('__published') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func106: TtkTokenKind;
begin
  if KeyComp('explicit') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func107: TtkTokenKind;
begin
  if KeyComp('typename') then Result := tkKey else
    if KeyComp('struct') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func109: TtkTokenKind;
begin
  if KeyComp('register') then Result := tkKey else
    if KeyComp('continue') then Result := tkKey else
      if KeyComp('__automated') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func110: TtkTokenKind;
begin
  if KeyComp('virtual') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func115: TtkTokenKind;
begin
  if KeyComp('protected') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func116: TtkTokenKind;
begin
  if KeyComp('operator') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func123: TtkTokenKind;
begin
  if KeyComp('dynamic_cast') then Result := tkKey else
    if KeyComp('const_cast') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func125: TtkTokenKind;
begin
  if KeyComp('static_cast') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func141: TtkTokenKind;
begin
  if KeyComp('__property') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.Func206: TtkTokenKind;
begin
  if KeyComp('reinterpret_cast') then Result := tkKey else Result := tkIdentifier;
end;

function TpsvCppRTF.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TpsvCppRTF.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 207 then Result := fIdentFuncTable[HashKey] else Result := tkIdentifier;
end;

procedure TpsvCppRTF.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      '&': fProcTable[I] := AndSymbolProc;
      #39: fProcTable[I] := AsciiCharProc;
      '@': fProcTable[I] := AtSymbolProc;
      '}': fProcTable[I] := BraceCloseProc;
      '{': fProcTable[I] := BraceOpenProc;
      #13: fProcTable[I] := CRProc;
      ':': fProcTable[I] := ColonProc;
      ',': fProcTable[I] := CommaProc;
      '#': fProcTable[I] := DirectiveProc;
      '=': fProcTable[I] := EqualProc;
      '>': fProcTable[I] := GreaterProc;
      '?': fProcTable[I] := QuestionProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := IdentProc;
      #10: fProcTable[I] := LFProc;
      '<': fProcTable[I] := LowerProc;
      '-': fProcTable[I] := MinusProc;
      '%': fProcTable[I] := ModSymbolProc;
      '!': fProcTable[I] := NotSymbolProc;
      #0: fProcTable[I] := NullProc;
      '0'..'9': fProcTable[I] := NumberProc;
      '|': fProcTable[I] := OrSymbolProc;
      '+': fProcTable[I] := PlusProc;
      '.': fProcTable[I] := PointProc;
      ')': fProcTable[I] := RoundCloseProc;
      '(': fProcTable[I] := RoundOpenProc;
      ';': fProcTable[I] := SemiColonProc;
      '/': fProcTable[I] := SlashProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := SpaceProc;
      ']': fProcTable[I] := SquareCloseProc;
      '[': fProcTable[I] := SquareOpenProc;
      '*': fProcTable[I] := StarProc;
      #34: fProcTable[I] := StringProc;
      '~': fProcTable[I] := TildeProc;
      '^': fProcTable[I] := XOrSymbolProc;
      else fProcTable[I] := UnknownProc;
    end;
end;

constructor TpsvCppRTF.Create;
begin
  inherited Create;
  InitIdent;
  MakeMethodTables;
  fRange := rsUnknown;
  fAsmStart := False;
  CreateColorTable(
   [clBlack,    //1  tkAsm
    clGreen,    //2  tkComment
    clBlack,    //3  tkDirective
    clBlack,    //4  tkIdentifier
    clBlue,     //5  tkKey
    clBlue,     //6  tkNumber
    clBlack,    //7  tkFloat
    clBlack,    //8  tkHex
    clBlack,    //9  tkOctal
    clBlack,    //10 tkSpace
    clBlack,    //11 tkString
    clBlack,    //12 tkChar
    clBlack,    //13 tkSymbol
    clBlack,    //14 tkUnknown
    clBlack]    //15 else
   );

end; { Create }

procedure TpsvCppRTF.SetLine(NewValue: String; LineNumber:Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end; { SetLine }

procedure TpsvCppRTF.AnsiCProc;
begin
  fTokenID := tkComment;
  case FLine[Run] of
    #0:
      begin
        NullProc;
        exit;
      end;
    #10:
      begin
        LFProc;
        exit;
      end;
    #13:
      begin
        CRProc;
        exit;
      end;
  end;

  while FLine[Run] <> #0 do
    case FLine[Run] of
      '*':
        if fLine[Run + 1] = '/' then
        begin
          inc(Run, 2);
          if fRange = rsAnsiCAsm then
            fRange := rsAsm
          else if fRange = rsAnsiCAsmBlock then
            fRange := rsAsmBlock
          else if (fRange = rsDirectiveComment) and
            not (fLine[Run] in [#0, #13, #10]) then
              fRange := rsMultiLineDirective //dj
          else
            fRange := rsUnKnown;
          break;
        end else
          inc(Run);
      #10: break;
      #13: break;
    else inc(Run);
    end;
end;

procedure TpsvCppRTF.AndSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {and assign}
      begin
        inc(Run, 2);
        FExtTokenID := xtkAndAssign;
      end;
    '&':                               {logical and}
      begin
        inc(Run, 2);
        FExtTokenID := xtkLogAnd;
      end;
  else                                 {and}
    begin
      inc(Run);
      FExtTokenID := xtkAnd;
    end;
  end;
end;

procedure TpsvCppRTF.AsciiCharProc;
begin
  fTokenID := tkChar;
  repeat
    if fLine[Run] = '\' then begin
      if fLine[Run + 1] in [#39, '\'] then                                      //ek 2000-04-26
        inc(Run);
    end;
    inc(Run);
  until fLine[Run] in [#0, #10, #13, #39];
  if fLine[Run] = #39 then
    inc(Run);
end;

procedure TpsvCppRTF.AtSymbolProc;
begin
  fTokenID := tkUnknown;
  inc(Run);
end;

procedure TpsvCppRTF.BraceCloseProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
  FExtTokenID := xtkBraceClose;
  if fRange = rsAsmBlock then fRange := rsUnknown;
end;

procedure TpsvCppRTF.BraceOpenProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
  FExtTokenID := xtkBraceOpen;
  if fRange = rsAsm then
  begin
    fRange := rsAsmBlock;
    fAsmStart := True;
  end;
end;

procedure TpsvCppRTF.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run + 1] = #10 then Inc(Run);
end;

procedure TpsvCppRTF.ColonProc;
begin
  fTokenID := tkSymbol;
  Case FLine[Run + 1] of
    ':':                               {scope resolution operator}
      begin
        inc(Run, 2);
        FExtTokenID := xtkScopeResolution;
      end;
  else                                 {colon}
    begin
      inc(Run);
      FExtTokenID := xtkColon;
    end;
  end;
end;

procedure TpsvCppRTF.CommaProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkComma;
end;

procedure TpsvCppRTF.DirectiveProc; // dj, rewritten to support multiline directives properly
begin
  if Trim(fLine)[1] <> '#' then // '#' is not first char on the line, treat it as an invalid char
  begin
    fTokenID := tkUnknown;
    Inc(Run);
    Exit;
  end;
  fTokenID := tkDirective;
  repeat
    if fLine[Run] = '/' then // comment?
    begin
      if fLine[Run + 1] = '/' then // is end of directive as well
      begin
        fRange := rsUnknown;                                              //ek 2000-04-25
        Exit;
      end
      else
        if fLine[Run + 1] = '*' then // might be embedded only
        begin
          fRange := rsDirectiveComment;
          Exit;
        end;
    end;
    if (fLine[Run] = '\') and (fLine[Run +1 ] = #0) then // a multiline directive
    begin
      Inc(Run);
      fRange := rsMultiLineDirective;
      Exit;
    end;
    Inc(Run);
  until fLine[Run] in [#0, #10, #13];
end;

procedure TpsvCppRTF.DirectiveEndProc; // dj, added to support multiline directives properly
begin
  fTokenID := tkDirective;
  case FLine[Run] of
    #0:
      begin
        NullProc;
        Exit;
      end;
    #10:
      begin
        LFProc;
        Exit;
      end;
    #13:
      begin
        CRProc;
        Exit;
      end;
  end;
  fRange := rsUnknown;
  repeat
    case FLine[Run] of
      #0, #10, #13: Break;
      '/': // comment?
        begin
          case fLine[Run + 1] of
            '/': // is end of directive as well
              begin
                fRange := rsUnknown;                                              //ek 2000-04-25
                Exit;
              end;
            '*': // might be embedded only
              begin
                fRange := rsDirectiveComment;
                Exit;
              end;
          end;
        end;
      '\': // yet another line?
        begin
          if fLine[Run + 1] = #0 then
          begin
            Inc(Run);
            fRange := rsMultiLineDirective;
            Exit;
          end;
        end;
    end;
    Inc(Run);
  until fLine[Run] in [#0, #10, #13];
end;

procedure TpsvCppRTF.EqualProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {logical equal}
      begin
        inc(Run, 2);
        FExtTokenID := xtkLogEqual;
      end;
  else                                 {assign}
    begin
      inc(Run);
      FExtTokenID := xtkAssign;
    end;
  end;
end;

procedure TpsvCppRTF.GreaterProc;
begin
  fTokenID := tkSymbol;
  Case FLine[Run + 1] of
    '=':                               {greater than or equal to}
      begin
        inc(Run, 2);
        FExtTokenID := xtkGreaterThanEqual;
      end;
    '>':
      begin
        if FLine[Run + 2] = '=' then   {shift right assign}
        begin
          inc(Run, 3);
          FExtTokenID := xtkShiftRightAssign;
        end
        else                           {shift right}
        begin
          inc(Run, 2);
          FExtTokenID := xtkShiftRight;
        end;
      end;
  else                                 {greater than}
    begin
      inc(Run);
      FExtTokenID := xtkGreaterThan;
    end;
  end;
end;

procedure TpsvCppRTF.QuestionProc;
begin
  fTokenID := tkSymbol;                {conditional}
  FExtTokenID := xtkQuestion;
  inc(Run);
end;

procedure TpsvCppRTF.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TpsvCppRTF.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TpsvCppRTF.LowerProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {less than or equal to}
      begin
        inc(Run, 2);
        FExtTokenID := xtkLessThanEqual;
      end;
    '<':
      begin
        if FLine[Run + 2] = '=' then   {shift left assign}
        begin
          inc(Run, 3);
          FExtTokenID := xtkShiftLeftAssign;
        end
        else                           {shift left}
        begin
          inc(Run, 2);
          FExtTokenID := xtkShiftLeft;
        end;
      end;
  else                                 {less than}
    begin
      inc(Run);
      FExtTokenID := xtkLessThan;
    end;
  end;
end;

procedure TpsvCppRTF.MinusProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {subtract assign}
      begin
        inc(Run, 2);
        FExtTokenID := xtkSubtractAssign;
      end;
    '-':                               {decrement}
      begin
        inc(Run, 2);
        FExtTokenID := xtkDecrement;
      end;
    '>':                               {arrow}
      begin
        inc(Run, 2);
        FExtTokenID := xtkArrow;
      end;
  else                                 {subtract}
    begin
      inc(Run);
      FExtTokenID := xtkSubtract;
    end;
  end;
end;

procedure TpsvCppRTF.ModSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {mod assign}
      begin
        inc(Run, 2);
        FExtTokenID := xtkModAssign;
      end;
  else                                 {mod}
    begin
      inc(Run);
      FExtTokenID := xtkMod;
    end;
  end;
end;

procedure TpsvCppRTF.NotSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {not equal}
      begin
        inc(Run, 2);
        FExtTokenID := xtkNotEqual;
      end;
  else                                 {not}
    begin
      inc(Run);
      FExtTokenID := xtkLogComplement;
    end;
  end;
end;

procedure TpsvCppRTF.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TpsvCppRTF.NumberProc;
var
  idx1: Integer; // token[1]
  i: Integer;
begin
  idx1 := Run;
  Inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in
    ['0'..'9', 'A'..'F', 'a'..'f', '.', 'u', 'U', 'l', 'L', 'x', 'X', '-', '+'] do
  begin
    case FLine[Run] of
      '.':
        if FLine[Succ(Run)] = '.' then
          Break
        else
          if (fTokenID <> tkHex) then
            fTokenID := tkFloat
          else // invalid
          begin
            fTokenID := tkUnknown;
            Exit;
          end;
      '-', '+':
        begin
          if fTokenID <> tkFloat then // number <> float. an arithmetic operator
            Exit;
          if not (FLine[Pred(Run)] in ['e', 'E']) then
            Exit; // number = float, but no exponent. an arithmetic operator
          if not (FLine[Succ(Run)] in ['0'..'9', '+', '-']) then // invalid
          begin
            Inc(Run);
            fTokenID := tkUnknown;
            Exit;
          end
        end;
      '0'..'7':
        if (Run = Succ(idx1)) and (FLine[idx1] = '0') then // octal number
          fTokenID := tkOctal;
      '8', '9':
        if (FLine[idx1] = '0') and
           ((fTokenID <> tkHex) and (fTokenID <> tkFloat)) then // invalid octal char
             fTokenID := tkUnknown;
      'a'..'d', 'A'..'D':
        if fTokenID <> tkHex then // invalid char
          Break;
      'e', 'E':
        if (fTokenID <> tkHex) then
          if FLine[Pred(Run)] in ['0'..'9'] then // exponent
          begin
            for i := idx1 to Pred(Run) do
              if FLine[i] in ['e', 'E'] then // too many exponents
              begin
                //Run := i;
                fTokenID := tkUnknown;
                Exit;
              end;
            if not (FLine[Succ(Run)] in ['0'..'9', '+', '-']) then
              Break
            else
              fTokenID := tkFloat
          end
          else // invalid char
            Break;
      'f', 'F':
        if fTokenID <> tkHex then
        begin
          for i := idx1 to Pred(Run) do
            if FLine[i] in ['f', 'F'] then // declaration syntax error
            begin
              fTokenID := tkUnknown;
              Exit;
            end;
          if fTokenID = tkFloat then
          begin
            if fLine[Pred(Run)] in ['l', 'L'] then // can't mix
              Break;
          end
          else
            fTokenID := tkFloat;
        end;
      'l', 'L':
        begin
          for i := idx1 to Pred(Run) do
            if FLine[i] in ['l', 'L'] then // declaration syntax error
            begin
              fTokenID := tkUnknown;
              Exit;
            end;
          if fTokenID = tkFloat then
            if fLine[Pred(Run)] in ['f', 'F'] then // can't mix
              Break;
        end;
      'u', 'U':
        if fTokenID = tkFloat then // not allowed
          Break
        else
          for i := idx1 to Pred(Run) do
            if FLine[i] in ['u', 'U'] then // declaration syntax error
            begin
              fTokenID := tkUnknown;
              Exit;
            end;
      'x', 'X':
        if (Run = Succ(idx1)) and   // 0x... 'x' must be second char
           (FLine[idx1] = '0') and  // 0x...
           (FLine[Succ(Run)] in ['0'..'9', 'a'..'f', 'A'..'F']) then // 0x... must be continued with a number
             fTokenID := tkHex
           else // invalid char
           begin
             if (not Identifiers[fLine[Succ(Run)]]) and
                (FLine[Succ(idx1)] in ['x', 'X']) then
             begin
               Inc(Run); // highlight 'x' too
               fTokenID := tkUnknown;
             end;
             Break;
           end;
    end; // case
    Inc(Run);
  end; // while
  if FLine[Run] in ['A'..'Z', 'a'..'z', '_'] then
    fTokenID := tkUnknown;
end;

procedure TpsvCppRTF.OrSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {or assign}
      begin
        inc(Run, 2);
        FExtTokenID := xtkIncOrAssign;
      end;
    '|':                               {logical or}
      begin
        inc(Run, 2);
        FExtTokenID := xtkLogOr;
      end;
  else                                 {or}
    begin
      inc(Run);
      FExtTokenID := xtkIncOr;
    end;
  end;
end;

procedure TpsvCppRTF.PlusProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {add assign}
      begin
        inc(Run, 2);
        FExtTokenID := xtkAddAssign;
      end;
    '+':                               {increment}
      begin
        inc(Run, 2);
        FExtTokenID := xtkIncrement;
      end;
  else                                 {add}
    begin
      inc(Run);
      FExtTokenID := xtkAdd;
    end;
  end;
end;

procedure TpsvCppRTF.PointProc;
begin
  fTokenID := tkSymbol;
  if (FLine[Run + 1] = '.') and (FLine[Run + 2] = '.') then
    begin                              {ellipse}
      inc(Run, 3);
      FExtTokenID := xtkEllipse;
    end
  else
    if FLine[Run + 1] in ['0'..'9'] then // float
    begin
      Dec(Run); // numberproc must see the point
      NumberProc;
    end
  else                                 {point}
    begin
      inc(Run);
      FExtTokenID := xtkPoint;
    end;
end;

procedure TpsvCppRTF.RoundCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkRoundClose;
end;

procedure TpsvCppRTF.RoundOpenProc;
begin
  inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundOpen;
end;

procedure TpsvCppRTF.SemiColonProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkSemiColon;
  if fRange = rsAsm then fRange := rsUnknown;
end;

procedure TpsvCppRTF.SlashProc;
begin
  case FLine[Run + 1] of
    '/':                               {c++ style comments}
      begin
        fTokenID := tkComment;
        inc(Run, 2);
        while not (fLine[Run] in [#0, #10, #13]) do Inc(Run);
      end;
    '*':                               {c style comments}
      begin
        fTokenID := tkComment;
        if fRange = rsAsm then
          fRange := rsAnsiCAsm
        else if fRange = rsAsmBlock then
          fRange := rsAnsiCAsmBlock
        else if fRange <> rsDirectiveComment then                          
          fRange := rsAnsiC;
        inc(Run, 2);
        while fLine[Run] <> #0 do
          case fLine[Run] of
            '*':
              if fLine[Run + 1] = '/' then
              begin
                inc(Run, 2);
                if fRange = rsDirectiveComment then
                  fRange := rsMultiLineDirective //dj
                else if fRange = rsAnsiCAsm then
                  fRange := rsAsm
                else
                  begin
                  if fRange = rsAnsiCAsmBlock then
                    fRange := rsAsmBlock
                  else
                    fRange := rsUnKnown;
                  end;
                break;
              end else inc(Run);
            #10, #13:
              begin
                if fRange = rsDirectiveComment then
                  fRange := rsAnsiC;
                break;
              end;
          else inc(Run);
          end;
      end;
    '=':                               {divide assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
        FExtTokenID := xtkDivideAssign;
      end;
  else                                 {divide}
    begin
      inc(Run);
      fTokenID := tkSymbol;
      FExtTokenID := xtkDivide;
    end;
  end;
end;

procedure TpsvCppRTF.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TpsvCppRTF.SquareCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkSquareClose;
end;

procedure TpsvCppRTF.SquareOpenProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkSquareOpen;
end;

procedure TpsvCppRTF.StarProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {multiply assign}
      begin
        inc(Run, 2);
        FExtTokenID := xtkMultiplyAssign;
      end;
  else                                 {star}
    begin
      inc(Run);
      FExtTokenID := xtkStar;
    end;
  end;
end;

procedure TpsvCppRTF.StringProc;
begin
  fTokenID := tkString;
  repeat
    if fLine[Run] = '\' then begin
      case fLine[Run + 1] of
        #34, '\':
          Inc(Run);
        #00:
          begin
            Inc(Run);
            fRange := rsMultilineString;
            Exit;
          end;
      end;
    end;
    inc(Run);
  until fLine[Run] in [#0, #10, #13, #34];
  if FLine[Run] = #34 then
    inc(Run);
end;

procedure TpsvCppRTF.StringEndProc;
begin
  fTokenID := tkString;

  case FLine[Run] of
    #0:
      begin
        NullProc;
        Exit;
      end;
    #10:
      begin
        LFProc;
        Exit;
      end;
    #13:
      begin
        CRProc;
        Exit;
      end;
  end;

  fRange := rsUnknown;

  repeat
    case FLine[Run] of
      #0, #10, #13: Break;
      '\':
        begin
          case fLine[Run + 1] of
            #34, '\':
              Inc(Run);
            #00:
              begin
                Inc(Run);
                fRange := rsMultilineString;
                Exit;
              end;
          end;
        end;
      #34: Break;
    end;
    inc(Run);
  until fLine[Run] in [#0, #10, #13, #34];
  if FLine[Run] = #34 then
    inc(Run);
end;

procedure TpsvCppRTF.TildeProc;
begin
  inc(Run);                            {bitwise complement}
  fTokenId := tkSymbol;
  FExtTokenID := xtkBitComplement;
end;

procedure TpsvCppRTF.XOrSymbolProc;
begin
  fTokenID := tkSymbol;
  Case FLine[Run + 1] of
  	'=':                               {xor assign}
      begin
        inc(Run, 2);
        FExtTokenID := xtkXorAssign;
      end;
  else                                 {xor}
    begin
      inc(Run);
      FExtTokenID := xtkXor;
    end;
  end;
end;

procedure TpsvCppRTF.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then // if FLine[Run] is the leadbyte of MBCS char,then jump 2 chars.
    Inc(Run,2)
  else
{$ENDIF}
  Inc(Run);
  fTokenID := tkUnknown;
end;

procedure TpsvCppRTF.Next;
begin
  fAsmStart := False;
  fTokenPos := Run;
  case fRange of
    rsAnsiC, rsAnsiCAsm,
    rsAnsiCAsmBlock, rsDirectiveComment: AnsiCProc;
    rsMultiLineDirective: DirectiveEndProc; // dj
    rsMultilineString: StringEndProc;                                           //ek 2001-08-02
  else
    begin
      fRange := rsUnknown;
      fProcTable[fLine[Run]];
    end;
  end;
end;


function TpsvCppRTF.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TpsvCppRTF.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TpsvCppRTF.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TpsvCppRTF.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
  if ((fRange = rsAsm) or (fRange = rsAsmBlock)) and not fAsmStart
    and not (fTokenId in [tkComment, tkSpace, tkNull])
  then
    Result := tkAsm;
end;

function TpsvCppRTF.GetExtTokenID: TxtkTokenKind;
begin
  Result := FExtTokenID;
end;

function TpsvCppRTF.GetTokenAttribute: integer;
begin
  case fTokenID of
    tkAsm: Result := 1;
    tkComment: Result := 2;
    tkDirective: Result := 3;
    tkIdentifier: Result := 4;
    tkKey: Result := 5;
    tkNumber: Result := 6;
    tkFloat: Result := 7;
    tkHex: Result := 8;
    tkOctal: Result := 9;
    tkSpace: Result := 10;
    tkString: Result := 11;
    tkChar: Result := 12;
    tkSymbol: Result := 13;
    tkUnknown: Result := 14;
    else Result := 15;
  end;
end;

function TpsvCppRTF.GetTokenKind: integer;
begin
  Result := Ord(GetTokenID);
end;

function TpsvCppRTF.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TpsvCppRTF.ReSetRange;
begin
  fRange:= rsUnknown;
end;

procedure TpsvCppRTF.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TpsvCppRTF.PrepareToken(var AToken : string);
var St : string;
begin
  St := AToken;
  St := StringReplace(St,'\','\\',[rfReplaceAll]);
  St := StringReplace(St,'{','\{',[rfReplaceAll]);
  St := StringReplace(St,'}','\}',[rfReplaceAll]);
  AToken := St;
end;

function TpsvCPPRTF.PrepareOutput(Attr: integer; AToken : string): string;
begin
  case Attr of
    2 : Result  := '\cf2 \i '+ AToken +'\i0 ';
    5 : Result  := '\cf5 \b '+ AToken +'\b0 ';
  else
   Result := Format('\cf%d %s',[Attr,AToken]);
  end;
end;


procedure TpsvCppRTF.SetupDefaultColors;
begin
  CreateColorTable(
   [clBlack,    //1  tkAsm
    clGreen,    //2  tkComment
    clBlack,    //3  tkDirective
    clBlack,    //4  tkIdentifier
    clBlue,     //5  tkKey
    clBlue,     //6  tkNumber
    clBlack,    //7  tkFloat
    clBlack,    //8  tkHex
    clBlack,    //9  tkOctal
    clBlack,    //10 tkSpace
    clBlack,    //11 tkString
    clBlack,    //12 tkChar
    clBlack,    //13 tkSymbol
    clBlack,    //14 tkUnknown
    clBlack]    //15 else
   );
end;

initialization
  MakeIdentTable;
end.

