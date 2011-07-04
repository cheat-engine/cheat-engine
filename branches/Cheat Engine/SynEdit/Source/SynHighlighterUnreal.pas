{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.
                                          PP - 2001/10/24:
The Original Code is based on the UnrealSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Dean Harmon.
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

$Id: SynHighlighterUnreal.pas,v 1.18 2005/01/28 16:53:25 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Unreal syntax highlighter for SynEdit)
@author(Dean Harmon)
@created(2000)
@lastmod(2001-06-29)
}

{$IFNDEF QSYNHIGHLIGHTERUNREAL}
unit SynHighlighterUnreal;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditHighlighter,
  QSynEditTypes,
{$ELSE}
  Graphics,
  Registry,
  Windows, // registry constants
  SynEditHighlighter,
  SynEditTypes,
{$ENDIF}
  SysUtils,
  Classes;

const
  MAXIDENTTABLE = 238;

type
  TtkTokenKind = (
    tkComment,
    tkDirective,
    tkIdentifier,
    tkKey,
    tkKey2,
    tkNull,
    tkNumber,
    tkSpace,
    tkString,
    tkString2,
    tkSymbol,
    tkUnknown);

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

  TRangeState = (rsANil, rsAnsiC, rsDirective, rsDirectiveComment, rsUnKnown);

  TProcTableProc = procedure of Object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of Object;

  TSynUnrealSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fLine: PChar;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    FRoundCount: Integer;
    FSquareCount: Integer;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    FExtTokenID: TxtkTokenKind;
    fLineNumber: Integer;
    fIdentFuncTable: array[0..MAXIDENTTABLE] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fDirecAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fInvalidAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fKey2Attri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fString2Attri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;

    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: String): Boolean;
    function Func15: TtkTokenKind;
    function Func17: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func20: TtkTokenKind;
    function Func28: TtkTokenKind;
    function Func33: TtkTokenKind;
    function Func35: TtkTokenKind;
    function Func37: TtkTokenKind;
    function Func39: TtkTokenKind;
    function Func40: TtkTokenKind;
    function Func41: TtkTokenKind;
    function Func42: TtkTokenKind;
    function Func43: TtkTokenKind;
    function Func44: TtkTokenKind;
    function Func45: TtkTokenKind;
    function Func48: TtkTokenKind;
    function Func49: TtkTokenKind;
    function Func50: TtkTokenKind;
    function Func51: TtkTokenKind;
    function Func52: TtkTokenKind;
    function Func53: TtkTokenKind;
    function Func54: TtkTokenKind;
    function Func55: TtkTokenKind;
    function Func56: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func59: TtkTokenKind;
    function Func62: TtkTokenKind;
    function Func63: TtkTokenKind;
    function Func64: TtkTokenKind;
    function Func65: TtkTokenKind;
    function Func66: TtkTokenKind;
    function Func69: TtkTokenKind;
    function Func70: TtkTokenKind;
    function Func71: TtkTokenKind;
    function Func72: TtkTokenKind;
    function Func73: TtkTokenKind;
    function Func74: TtkTokenKind;
    function Func76: TtkTokenKind;
    function Func78: TtkTokenKind;
    function Func79: TtkTokenKind;
    function Func80: TtkTokenKind;
    function Func81: TtkTokenKind;
    function Func82: TtkTokenKind;
    function Func83: TtkTokenKind;
    function Func84: TtkTokenKind;
    function Func85: TtkTokenKind;
    function Func87: TtkTokenKind;
    function Func89: TtkTokenKind;
    function Func91: TtkTokenKind;
    function Func92: TtkTokenKind;
    function Func96: TtkTokenKind;
    function Func97: TtkTokenKind;
    function Func98: TtkTokenKind;
    function Func99: TtkTokenKind;
    function Func100: TtkTokenKind;
    function Func101: TtkTokenKind;
    function Func102: TtkTokenKind;
    function Func103: TtkTokenKind;
    function Func104: TtkTokenKind;
    function Func106: TtkTokenKind;
    function Func107: TtkTokenKind;
    function Func108: TtkTokenKind;
    function Func109: TtkTokenKind;
    function Func113: TtkTokenKind;
    function Func115: TtkTokenKind;
    function Func120: TtkTokenKind;
    function Func122: TtkTokenKind;
    function Func126: TtkTokenKind;
    function Func127: TtkTokenKind;
    function Func128: TtkTokenKind;
    function Func135: TtkTokenKind;
    function Func136: TtkTokenKind;
    function Func143: TtkTokenKind;
    function Func144: TtkTokenKind;
    function Func146: TtkTokenKind;
    function Func147: TtkTokenKind;
    function Func148: TtkTokenKind;
    function Func156: TtkTokenKind;
    function Func167: TtkTokenKind;
    function Func172: TtkTokenKind;
    function Func174: TtkTokenKind;
    function Func178: TtkTokenKind;
    function Func185: TtkTokenKind;
    function Func190: TtkTokenKind;
    function Func192: TtkTokenKind;
    function Func193: TtkTokenKind;
    function Func210: TtkTokenKind;
    function Func218: TtkTokenKind;
    function Func238: TtkTokenKind;

    procedure AnsiCProc;
    procedure AndSymbolProc;
    procedure AsciiCharProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CRProc;
    procedure ColonProc;
    procedure CommaProc;
    procedure DirectiveProc;
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
    procedure DollarSignProc;
    procedure TildeProc;
    procedure XOrSymbolProc;
    procedure UnknownProc;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetExtTokenID: TxtkTokenKind;
    function IsFilterStored: Boolean; override;
    function GetSampleSource: string; override;
  public
    class function GetCapabilities: TSynHighlighterCapabilities; override;
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(NewValue: String; LineNumber:Integer); override;
    function GetToken: String; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    function UseUserSettings(settingIndex: integer): boolean; override;
    procedure EnumUserSettings(settings: TStrings); override;
    property ExtTokenID: TxtkTokenKind read GetExtTokenID;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property DirecAttri: TSynHighlighterAttributes read fDirecAttri
      write fDirecAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property InvalidAttri: TSynHighlighterAttributes read fInvalidAttri
      write fInvalidAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property Key2Attri: TSynHighlighterAttributes read fKey2Attri write fKey2Attri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SingleStringAttri: TSynHighlighterAttributes read fString2Attri
      write fString2Attri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

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
            if (I > #96) then mHashTable[I] := Ord(I) - 96;
        end;
    else mHashTable[I] := 0;
    end;
  end;
end;

procedure TSynUnrealSyn.InitIdent;
var
  I: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do begin
    pF^ := AltFunc;
    Inc(pF);
  end;
  fIdentFuncTable[15] := Func15;
  fIdentFuncTable[17] := Func17;
  fIdentFuncTable[19] := Func19;
  fIdentFuncTable[20] := Func20;
  fIdentFuncTable[28] := Func28;
  fIdentFuncTable[33] := Func33;
  fIdentFuncTable[35] := Func35;
  fIdentFuncTable[37] := Func37;
  fIdentFuncTable[39] := Func39;
  fIdentFuncTable[40] := Func40;
  fIdentFuncTable[41] := Func41;
  fIdentFuncTable[42] := Func42;
  fIdentFuncTable[43] := Func43;
  fIdentFuncTable[44] := Func44;
  fIdentFuncTable[45] := Func45;
  fIdentFuncTable[48] := Func48;
  fIdentFuncTable[49] := Func49;
  fIdentFuncTable[50] := Func50;
  fIdentFuncTable[51] := Func51;
  fIdentFuncTable[52] := Func52;
  fIdentFuncTable[53] := Func53;
  fIdentFuncTable[54] := Func54;
  fIdentFuncTable[55] := Func55;
  fIdentFuncTable[56] := Func56;
  fIdentFuncTable[57] := Func57;
  fIdentFuncTable[59] := Func59;
  fIdentFuncTable[62] := Func62;
  fIdentFuncTable[63] := Func63;
  fIdentFuncTable[64] := Func64;
  fIdentFuncTable[65] := Func65;
  fIdentFuncTable[66] := Func66;
  fIdentFuncTable[69] := Func69;
  fIdentFuncTable[70] := Func70;
  fIdentFuncTable[71] := Func71;
  fIdentFuncTable[72] := Func72;
  fIdentFuncTable[73] := Func73;
  fIdentFuncTable[74] := Func74;
  fIdentFuncTable[76] := Func76;
  fIdentFuncTable[78] := Func78;
  fIdentFuncTable[79] := Func79;
  fIdentFuncTable[80] := Func80;
  fIdentFuncTable[81] := Func81;
  fIdentFuncTable[82] := Func82;
  fIdentFuncTable[83] := Func83;
  fIdentFuncTable[84] := Func84;
  fIdentFuncTable[85] := Func85;
  fIdentFuncTable[87] := Func87;
  fIdentFuncTable[89] := Func89;
  fIdentFuncTable[91] := Func91;
  fIdentFuncTable[92] := Func92;
  fIdentFuncTable[96] := Func96;
  fIdentFuncTable[97] := Func97;
  fIdentFuncTable[98] := Func98;
  fIdentFuncTable[99] := Func99;
  fIdentFuncTable[100] := Func100;
  fIdentFuncTable[101] := Func101;
  fIdentFuncTable[102] := Func102;
  fIdentFuncTable[103] := Func103;
  fIdentFuncTable[104] := Func104;
  fIdentFuncTable[106] := Func106;
  fIdentFuncTable[107] := Func107;
  fIdentFuncTable[108] := Func108;
  fIdentFuncTable[109] := Func109;
  fIdentFuncTable[113] := Func113;
  fIdentFuncTable[115] := Func115;
  fIdentFuncTable[120] := Func120;
  fIdentFuncTable[122] := Func122;
  fIdentFuncTable[126] := Func126;
  fIdentFuncTable[127] := Func127;
  fIdentFuncTable[128] := Func128;
  fIdentFuncTable[135] := Func135;
  fIdentFuncTable[136] := Func136;
  fIdentFuncTable[143] := Func143;
  fIdentFuncTable[144] := Func144;
  fIdentFuncTable[146] := Func146;
  fIdentFuncTable[147] := Func147;
  fIdentFuncTable[148] := Func148;
  fIdentFuncTable[156] := Func156;
  fIdentFuncTable[167] := Func167;
  fIdentFuncTable[172] := Func172;
  fIdentFuncTable[174] := Func174;
  fIdentFuncTable[178] := Func178;
  fIdentFuncTable[185] := Func185;
  fIdentFuncTable[190] := Func190;
  fIdentFuncTable[192] := Func192;
  fIdentFuncTable[193] := Func193;
  fIdentFuncTable[210] := Func210;
  fIdentFuncTable[218] := Func218;
  fIdentFuncTable[238] := Func238;
end;

function TSynUnrealSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;

  while ToHash^ in TSynValidStringChars do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end; { KeyHash }

function TSynUnrealSyn.KeyComp(const aKey: String): Boolean;
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
      if Uppercase(Temp^) <> aKey[i] then
      begin
        Result := False;
        break;
      end;
      inc(Temp);
    end;
  end else Result := False;
end; { KeyComp }

function TSynUnrealSyn.Func15: TtkTokenKind;
begin
  if KeyComp('IF') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func17: TtkTokenKind;
begin
  if KeyComp('EACH') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func19: TtkTokenKind;
begin
  if KeyComp('DO') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func20: TtkTokenKind;
begin
  if KeyComp('CACHE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func28: TtkTokenKind;
begin
  if KeyComp('CASE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func33: TtkTokenKind;
begin
  if KeyComp('NAME') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func35: TtkTokenKind;
begin
  if KeyComp('CATCH') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func37: TtkTokenKind;
begin
  if KeyComp('BREAK') then Result := tkKey else
    if KeyComp('EXEC') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func39: TtkTokenKind;
begin
  if KeyComp('DOT') then Result := tkSymbol else
    if KeyComp('FOR') then Result := tkKey else
      if KeyComp('RNG') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func40: TtkTokenKind;
begin
  if KeyComp('SCALE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func41: TtkTokenKind;
begin
  if KeyComp('ELSE') then Result := tkKey else
    if KeyComp('VAR') then Result := tkKey else
      if KeyComp('GUID') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func42: TtkTokenKind;
begin
  if KeyComp('FINAL') then Result := tkKey2 else
    if KeyComp('FOR') then Result := tkKey else
      if KeyComp('SELF') then Result := tkKey else
      if KeyComp('NEW') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func43: TtkTokenKind;
begin
  if KeyComp('NEW') then Result := tkKey else
    if KeyComp('INT') then Result := tkKey else
      if KeyComp('SELF') then Result := tkKey else
        if KeyComp('LOCAL') then Result := tkKey else
          if KeyComp('FALSE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func44: TtkTokenKind;
begin
  if KeyComp('FALSE') then Result := tkKey else
    if KeyComp('BOOL') then Result := tkKey else
      if KeyComp('LOCAL') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func45: TtkTokenKind;
begin
  if KeyComp('BOOL') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func48: TtkTokenKind;
begin
  if KeyComp('MESH') then Result := tkKey else
    if KeyComp('LONG') then Result := tkKey else 
      if KeyComp('NONE') then Result := tkKey else
        if KeyComp('PLANE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func49: TtkTokenKind;
begin
  if KeyComp('GLOBAL') then Result := tkKey else
    if KeyComp('MODEL') then Result := tkKey else
      if KeyComp('COERCE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func50: TtkTokenKind;
begin
  if KeyComp('VOID') then Result := tkKey else
    if KeyComp('VECT') then Result := tkKey else Result := tkIdentifier; 
end;

function TSynUnrealSyn.Func51: TtkTokenKind;
begin
  if KeyComp('DELETE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func52: TtkTokenKind;
begin
  if KeyComp('BYTE') then Result := tkKey else
    if KeyComp('INIT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func53: TtkTokenKind;
begin
  if KeyComp('ENUM') then Result := tkKey else
    if KeyComp('ROT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func54: TtkTokenKind;
begin
  if KeyComp('CLASS') then Result := tkKey else
    if KeyComp('CONFIG') then Result := tkKey else
      if KeyComp('FLOAT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func55: TtkTokenKind;
begin
  if KeyComp('SKIP') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func56: TtkTokenKind;
begin
  if KeyComp('FOREACH') then Result := tkKey else
    if KeyComp('OUT') then Result := tkKey else Result := tkIdentifier;
end;                                                                                  

function TSynUnrealSyn.Func57: TtkTokenKind;
begin
  if KeyComp('AUTO') then Result := tkKey else
    if KeyComp('GOTO') then Result := tkKey else
      if KeyComp('WHILE') then Result := tkKey else
        if KeyComp('PLACEABLE') then Result := tkKey2 else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func59: TtkTokenKind;
begin
  if KeyComp('DELEGATE') then Result := tkKey2 else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func62: TtkTokenKind;
begin
  if KeyComp('EDFINDABLE') then Result := tkKey2 else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func63: TtkTokenKind;
begin
  if KeyComp('COLOR') then Result := tkKey else
    if KeyComp('ARRAY') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func64: TtkTokenKind;
begin
  if KeyComp('TRUE') then Result := tkKey else
    if KeyComp('RELIABLE') then Result := tkKey2 else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func65: TtkTokenKind;
begin
  if KeyComp('STATE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func66: TtkTokenKind;
begin
  if KeyComp('EVENT') then Result := tkKey else
    if KeyComp('LENGTH') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func69: TtkTokenKind;
begin
  if KeyComp('DEFAULT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func70: TtkTokenKind;
begin
  if KeyComp('STOP') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func71: TtkTokenKind;
begin
  if KeyComp('CONST') then Result := tkKey2 else
    if KeyComp('NATIVE') then Result := tkKey2 else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func72: TtkTokenKind;
begin
  if KeyComp('LATENT') then Result := tkKey2 else
    if KeyComp('STATIC') then Result := tkKey2 else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func73: TtkTokenKind;
begin
  if KeyComp('SOUND') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func74: TtkTokenKind;
begin
  if KeyComp('CROSS') then Result := tkSymbol else
    if KeyComp('MUTABLE') then Result := tkKey else
      if KeyComp('COORDS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func76: TtkTokenKind;
begin
  if KeyComp('UNTIL') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func78: TtkTokenKind;
begin
  if KeyComp('TRAVEL') then Result := tkKey else
    if KeyComp('REMOVE') then Result := tkKey2 else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func79: TtkTokenKind;
begin
  if KeyComp('SUPER') then Result := tkKey2 else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func80: TtkTokenKind;
begin
  if KeyComp('INPUT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func81: TtkTokenKind;
begin
  if KeyComp('DEPRECATED') then Result := tkKey2 else
    if KeyComp('ALWAYS') then Result := tkKey2 else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func82: TtkTokenKind;
begin
  if KeyComp('SWITCH') then Result := tkKey else
    if KeyComp('ASSERT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func83: TtkTokenKind;
begin
  if KeyComp('EXPANDS') then Result := tkKey else
    if KeyComp('VECTOR') then Result := tkKey else
      if KeyComp('WITHIN') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func84: TtkTokenKind;
begin
  if KeyComp('ABSTRACT') then Result := tkKey2 else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func85: TtkTokenKind;
begin
  if KeyComp('INSERT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func87: TtkTokenKind;
begin
  if KeyComp('LOCALIZED') then Result := tkKey2 else
    if KeyComp('STRING') then Result := tkKey else
      if KeyComp('IGNORES') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func89: TtkTokenKind;
begin
  if KeyComp('INSTANCED') then Result := tkKey2 else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func91: TtkTokenKind;
begin
  if KeyComp('EXTENDS') then Result := tkKey else
    if KeyComp('PRIVATE') then Result := tkKey2 else
      if KeyComp('SAFEREPLACE') then Result := tkKey2 else
        if KeyComp('IMPORT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func92: TtkTokenKind;
begin
  if KeyComp('BUTTON') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func96: TtkTokenKind;
begin
  if KeyComp('RETURN') then Result := tkKey else
    if KeyComp('DEPENDSON') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func97: TtkTokenKind;
begin
  if KeyComp('POINTER') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func98: TtkTokenKind;
begin
  if KeyComp('EXPLICIT') then Result := tkKey else
    if KeyComp('EXPORT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func99: TtkTokenKind;
begin
  if KeyComp('UNRELIABLE') then Result := tkKey2 else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func100: TtkTokenKind;
begin
  if KeyComp('HIDEPARENT') then Result := tkKey else
    if KeyComp('AUTOMATED') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func101: TtkTokenKind;
begin
  if KeyComp('CONTINUE') then Result := tkKey else
    if KeyComp('REGISTER') then Result := tkKey else
      if KeyComp('STRUCT') then Result := tkKey else
        if KeyComp('SINGULAR') then Result := tkKey2 else
          if KeyComp('EDITINLINE') then Result := tkKey2 else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func102: TtkTokenKind;
begin
  if KeyComp('FUNCTION') then Result := tkKey else
    if KeyComp('OPTIONAL') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func103: TtkTokenKind;
begin
  if KeyComp('GLOBALCONFIG') then Result := tkKey else
    if KeyComp('CACHEEXEMPT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func104: TtkTokenKind;
begin
  if KeyComp('SIMULATED') then Result := tkKey2 else
    if KeyComp('CPPTEXT') then Result := tkKey else Result := tkIdentifier;

end;

function TSynUnrealSyn.Func106: TtkTokenKind;
begin
  if KeyComp('ITERATOR') then Result := tkKey else
    if KeyComp('PROTECTED') then Result := tkKey2 else
      if KeyComp('NOTPLACEABLE') then Result := tkKey2 else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func107: TtkTokenKind;
begin
  if KeyComp('ROTATOR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func108: TtkTokenKind;
begin
  if KeyComp('OPERATOR') then Result := tkKey else
    if KeyComp('INVARIANT') then Result := tkKey2 else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func109: TtkTokenKind;
begin
  if KeyComp('EDITCONST') then Result := tkKey2 else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func113: TtkTokenKind;
begin
  if KeyComp('TEXTURE') then Result := tkKey else
    if KeyComp('PARSECONFIG') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func115: TtkTokenKind;
begin
  if KeyComp('INTRINSIC') then Result := tkKey2 else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func120: TtkTokenKind;
begin
  if KeyComp('TRANSIENT') then Result := tkKey2 else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func122: TtkTokenKind;
begin
  if KeyComp('REPLICATION') then Result := tkKey2 else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func126: TtkTokenKind;
begin
  if KeyComp('ENUMCOUNT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func127: TtkTokenKind;
begin
  if KeyComp('NOEXPORT') then Result := tkKey2 else
    if KeyComp('BOUNDINGBOX') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func128: TtkTokenKind;
begin
  if KeyComp('HIDECATEGORIES') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func135: TtkTokenKind;
begin
  if KeyComp('HIDEDROPDOWN') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func136: TtkTokenKind;
begin
  if KeyComp('ARRAYCOUNT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func143: TtkTokenKind;
begin
  if KeyComp('EDITINLINENEW') then Result := tkKey2 else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func144: TtkTokenKind;
begin
  if KeyComp('NOUSERCREATE') then Result := tkKey2 else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func146: TtkTokenKind;
begin
  if KeyComp('EDITINLINEUSE') then Result := tkKey2 else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func147: TtkTokenKind;
begin
  if KeyComp('PREOPERATOR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func148: TtkTokenKind;
begin
  if KeyComp('PEROBJECTCONFIG') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func156: TtkTokenKind;
begin
  if KeyComp('SCRIPTCONST') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func167: TtkTokenKind;
begin
  if KeyComp('SHOWCATEGORIES') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func172: TtkTokenKind;
begin
  if KeyComp('EDITCONSTARRAY') then Result := tkKey2 else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func174: TtkTokenKind;
begin
  if KeyComp('BOUNDINGVOLUME') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func178: TtkTokenKind;
begin
  if KeyComp('POSTOPERATOR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func185: TtkTokenKind;
begin
  if KeyComp('COLLAPSECATEGORIES') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func190: TtkTokenKind;
begin
  if KeyComp('EDITINLINENOTIFY') then Result := tkKey2 else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func192: TtkTokenKind;
begin
  if KeyComp('DONTCOLLAPSECATEGORIES') then Result := tkKey else
    if KeyComp('NOTEDITINLINENEW') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func193: TtkTokenKind;
begin
  if KeyComp('NATIVEREPLICATION') then Result := tkKey2 else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func210: TtkTokenKind;
begin
  if KeyComp('DEFAULTPROPERTIES') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func218: TtkTokenKind;
begin
  if KeyComp('EXPORTSTRUCTS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.Func238: TtkTokenKind;
begin
  if KeyComp('DONTCOLLAPSECATEGORIES') then Result := tkKey else Result := tkIdentifier;
end;

function TSynUnrealSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynUnrealSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < MAXIDENTTABLE + 1 then Result := fIdentFuncTable[HashKey] else Result := tkIdentifier;
end;

procedure TSynUnrealSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      '&': fProcTable[I] := AndSymbolProc;
      #39: fProcTable[I] := AsciiCharProc;
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
      '$', '@': fProcTable[I] := DollarSignProc;
      '~': fProcTable[I] := TildeProc;
      '^': fProcTable[I] := XOrSymbolProc;
      else fProcTable[I] := UnknownProc;
    end;
end;

constructor TSynUnrealSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style:= [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fInvalidAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar);
  AddAttribute(fInvalidAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style:= [fsBold];
  AddAttribute(fKeyAttri);
  fKey2Attri := TSynHighlighterAttributes.Create(SYNS_AttrSecondReservedWord);
  fKey2Attri.Style:= [fsBold];
  AddAttribute(fKey2Attri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  fSpaceAttri.Foreground := clWindow;
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  AddAttribute(fStringAttri);
  fString2Attri := TSynHighlighterAttributes.Create(SYNS_AttrSingleString);
  AddAttribute(fString2Attri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);
  fDirecAttri := TSynHighlighterAttributes.Create(SYNS_AttrDirective);
  AddAttribute(fDirecAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fRange := rsUnknown;
  fDefaultFilter := SYNS_FilterCPP;
end; { Create }

procedure TSynUnrealSyn.SetLine(NewValue: String; LineNumber:Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end; { SetLine }

procedure TSynUnrealSyn.AnsiCProc;
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
          if fRange = rsDirectiveComment then                              
            fRange := rsDirective
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

procedure TSynUnrealSyn.AndSymbolProc;
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

procedure TSynUnrealSyn.AsciiCharProc;
begin
  fTokenID := tkString2;
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
      #92:                             {backslash}
        {if we have an escaped single quote it doesn't count}
        if FLine[Run + 1] = #39 then inc(Run);
    end;
    inc(Run);
  until FLine[Run] = #39;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynUnrealSyn.BraceCloseProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
  FExtTokenID := xtkBraceClose;
end;

procedure TSynUnrealSyn.BraceOpenProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
  FExtTokenID := xtkBraceOpen;
end;

procedure TSynUnrealSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run + 1] = #10 then Inc(Run);
end;

procedure TSynUnrealSyn.ColonProc;
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

procedure TSynUnrealSyn.CommaProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkComma;
end;

procedure TSynUnrealSyn.DirectiveProc;
begin
  if fLine[Run] in [#0, #10, #13] then begin
    if (Run <= 0) then
      fRange := rsUnknown;
    fProcTable[fLine[Run]];
  end else begin
    fTokenID := tkDirective;
    while TRUE do
      case fLine[Run] of
        '/': // comment?
          begin
            if fLine[Run + 1] = '/' then // is end of directive as well
              break
            else if fLine[Run + 1] = '*' then begin // might be embedded only
              fRange := rsDirectiveComment;
              break;
            end else
              Inc(Run);
          end;
        #0, #10, #13:
          begin
            fRange := rsUnknown;
            break;
          end;
        else Inc(Run);
      end;
  end;
end;

procedure TSynUnrealSyn.EqualProc;
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

procedure TSynUnrealSyn.GreaterProc;
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

procedure TSynUnrealSyn.QuestionProc;
begin
  fTokenID := tkSymbol;                {conditional}
  FExtTokenID := xtkQuestion;
  inc(Run);
end;

procedure TSynUnrealSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TSynUnrealSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynUnrealSyn.LowerProc;
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

procedure TSynUnrealSyn.MinusProc;
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

procedure TSynUnrealSyn.ModSymbolProc;
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

procedure TSynUnrealSyn.NotSymbolProc;
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

procedure TSynUnrealSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynUnrealSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in
    ['0'..'9', 'A'..'F', 'a'..'f', '.', 'u', 'U', 'l', 'L', 'x', 'X'] do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
end;

procedure TSynUnrealSyn.OrSymbolProc;
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

procedure TSynUnrealSyn.PlusProc;
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

procedure TSynUnrealSyn.PointProc;
begin
  fTokenID := tkSymbol;
  if (FLine[Run + 1] = '.') and (FLine[Run + 2] = '.') then
    begin                              {ellipse}
      inc(Run, 3);
      FExtTokenID := xtkEllipse;
    end
  else                                 {point}
    begin
      inc(Run);
      FExtTokenID := xtkPoint;
    end;
end;

procedure TSynUnrealSyn.RoundCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkRoundClose;
  dec(FRoundCount);
end;

procedure TSynUnrealSyn.RoundOpenProc;
begin
  inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundOpen;
  inc(FRoundCount);
end;

procedure TSynUnrealSyn.SemiColonProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkSemiColon;
end;

procedure TSynUnrealSyn.SlashProc;
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
        if fRange <> rsDirectiveComment then                               
          fRange := rsAnsiC;
        inc(Run, 2);
        while fLine[Run] <> #0 do
          case fLine[Run] of
            '*':
              if fLine[Run + 1] = '/' then
              begin
                inc(Run, 2);
                if fRange = rsDirectiveComment then
                  fRange := rsDirective
                else
                  fRange := rsUnKnown;
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

procedure TSynUnrealSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynUnrealSyn.SquareCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkSquareClose;
  dec(FSquareCount);
end;

procedure TSynUnrealSyn.SquareOpenProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkSquareOpen;
  inc(FSquareCount);
end;

procedure TSynUnrealSyn.StarProc;
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

procedure TSynUnrealSyn.StringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
      #92:                             {backslash}
        case FLine[Run + 1] of
          #10: inc(Run);               {line continuation character}
          #34: inc(Run);               {escaped quote doesn't count}
          #92: inc(Run);
        end;
    end;
    inc(Run);
  until FLine[Run] = #34;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynUnrealSyn.DollarSignProc;
begin
  fTokenID := tkSymbol;
  inc(run);
end;


procedure TSynUnrealSyn.TildeProc;
begin
  inc(Run);                            {bitwise complement}
  fTokenId := tkSymbol;
  FExtTokenID := xtkBitComplement;
end;

procedure TSynUnrealSyn.XOrSymbolProc;
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

procedure TSynUnrealSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynUnrealSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsAnsiC, rsDirectiveComment: AnsiCProc;
    rsDirective: DirectiveProc;
  else
    begin
      fRange := rsUnknown;
      fProcTable[fLine[Run]];
    end;
  end;
end;

function TSynUnrealSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT   : Result := fCommentAttri;
    SYN_ATTR_KEYWORD   : Result := fKeyAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_STRING    : Result := fStringAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_SYMBOL    : Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynUnrealSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynUnrealSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynUnrealSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynUnrealSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynUnrealSyn.GetExtTokenID: TxtkTokenKind;
begin
  Result := FExtTokenID;
end;


function TSynUnrealSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterCPP;
end; { IsFilterStored }


function TSynUnrealSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkDirective: Result := fDirecAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkKey2: Result := fKey2Attri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkString2: Result := fString2Attri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fInvalidAttri;
    else Result := nil;
  end;
end;

function TSynUnrealSyn.GetTokenKind: integer;
begin
  Result := Ord(GetTokenID);
end;

function TSynUnrealSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynUnrealSyn.ResetRange;
begin
  fRange:= rsUnknown;
end;

procedure TSynUnrealSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TSynUnrealSyn.EnumUserSettings(settings: TStrings);
begin
  { returns the user settings that exist in the registry }
{$IFNDEF SYN_CLX}
  with TBetterRegistry.Create do
  begin
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly('\SOFTWARE\Borland\C++Builder') then
      begin
        try
          GetKeyNames(settings);
        finally
          CloseKey;
        end;
      end;
    finally
      Free;
    end;
  end;
{$ENDIF}
end;

function TSynUnrealSyn.UseUserSettings(settingIndex: integer): boolean;
// Possible parameter values:
//   index into TStrings returned by EnumUserSettings
// Possible return values:
//   true : settings were read and used
//   false: problem reading settings or invalid version specified - old settings
//          were preserved

{$IFNDEF SYN_CLX}
  function ReadCPPBSettings(settingIndex: integer): boolean;

    function ReadCPPBSetting(settingTag: string; attri: TSynHighlighterAttributes; key: string): boolean;

      function ReadCPPB1(settingTag: string; attri: TSynHighlighterAttributes; name: string): boolean;
      var
        i: integer;
      begin
        for i := 1 to Length(name) do
          if name[i] = ' ' then name[i] := '_';
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
             '\SOFTWARE\Borland\C++Builder\'+settingTag+'\Highlight',name,true);
      end; { ReadCPPB1 }

      function ReadCPPB3OrMore(settingTag: string; attri: TSynHighlighterAttributes; key: string): boolean;
      begin
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
                 '\Software\Borland\C++Builder\'+settingTag+'\Editor\Highlight',
                 key,false);
      end; { ReadCPPB3OrMore }

    begin { ReadCPPBSetting }
      try
        if (settingTag[1] = '1')
          then Result := ReadCPPB1(settingTag,attri,key)
          else Result := ReadCPPB3OrMore(settingTag,attri,key);
      except Result := false; end;
    end; { ReadCPPBSetting }

  var
    tmpStringAttri    : TSynHighlighterAttributes;
    tmpNumberAttri    : TSynHighlighterAttributes;
    tmpKeyAttri       : TSynHighlighterAttributes;
    tmpSymbolAttri    : TSynHighlighterAttributes;
    tmpCommentAttri   : TSynHighlighterAttributes;
    tmpIdentifierAttri: TSynHighlighterAttributes;
    tmpInvalidAttri   : TSynHighlighterAttributes;
    tmpSpaceAttri     : TSynHighlighterAttributes;
    tmpDirecAttri     : TSynHighlighterAttributes;
    s                 : TStringList;

  begin { ReadCPPBSettings }
    s := TStringList.Create;
    try
      EnumUserSettings(s);
      if settingIndex >= s.Count then Result := false
      else begin
        tmpStringAttri    := TSynHighlighterAttributes.Create('');
        tmpNumberAttri    := TSynHighlighterAttributes.Create('');
        tmpKeyAttri       := TSynHighlighterAttributes.Create('');
        tmpSymbolAttri    := TSynHighlighterAttributes.Create('');
        tmpCommentAttri   := TSynHighlighterAttributes.Create('');
        tmpIdentifierAttri:= TSynHighlighterAttributes.Create('');
        tmpInvalidAttri   := TSynHighlighterAttributes.Create('');
        tmpSpaceAttri     := TSynHighlighterAttributes.Create('');
        tmpDirecAttri     := TSynHighlighterAttributes.Create('');
        tmpStringAttri    .Assign(fStringAttri);
        tmpNumberAttri    .Assign(fNumberAttri);
        tmpKeyAttri       .Assign(fKeyAttri);
        tmpSymbolAttri    .Assign(fSymbolAttri);
        tmpCommentAttri   .Assign(fCommentAttri);
        tmpIdentifierAttri.Assign(fIdentifierAttri);
        tmpInvalidAttri   .Assign(fInvalidAttri);
        tmpSpaceAttri     .Assign(fSpaceAttri);
        tmpDirecAttri     .Assign(fDirecAttri);
        Result := ReadCPPBSetting(s[settingIndex],fCommentAttri,'Comment')       and
                  ReadCPPBSetting(s[settingIndex],fIdentifierAttri,'Identifier') and
                  ReadCPPBSetting(s[settingIndex],fInvalidAttri,'Illegal Char')  and
                  ReadCPPBSetting(s[settingIndex],fKeyAttri,'Reserved word')     and
                  ReadCPPBSetting(s[settingIndex],fNumberAttri,'Integer')        and
                  ReadCPPBSetting(s[settingIndex],fSpaceAttri,'Whitespace')      and
                  ReadCPPBSetting(s[settingIndex],fStringAttri,'String')         and
                  ReadCPPBSetting(s[settingIndex],fSymbolAttri,'Symbol')         and
                  ReadCPPBSetting(s[settingIndex],fDirecAttri,'Preprocessor');
        if not Result then begin
          fStringAttri    .Assign(tmpStringAttri);
          fString2Attri   .Assign(tmpStringAttri);
          fNumberAttri    .Assign(tmpNumberAttri);
          fKeyAttri       .Assign(tmpKeyAttri);
          fKey2Attri      .Assign(tmpKeyAttri);
          fSymbolAttri    .Assign(tmpSymbolAttri);
          fCommentAttri   .Assign(tmpCommentAttri);
          fIdentifierAttri.Assign(tmpIdentifierAttri);
          fInvalidAttri.Assign(tmpInvalidAttri);
          fSpaceAttri     .Assign(tmpSpaceAttri);
          fDirecAttri     .Assign(tmpDirecAttri);
        end;
        tmpStringAttri    .Free;
        tmpNumberAttri    .Free;
        tmpKeyAttri       .Free;
        tmpSymbolAttri    .Free;
        tmpCommentAttri   .Free;
        tmpIdentifierAttri.Free;
        tmpInvalidAttri   .Free;
        tmpSpaceAttri     .Free;
        tmpDirecAttri     .Free;
      end;
    finally s.Free; end;
  end; { ReadCPPBSettings }
{$ENDIF}

begin
{$IFDEF SYN_CLX}
  Result := False;
{$ELSE}
  Result := ReadCPPBSettings(settingIndex);
{$ENDIF}
end; { TSynUnrealSyn.UseUserSettings }

function TSynUnrealSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', '0'..'9', 'a'..'z', 'A'..'Z'];
end;

class function TSynUnrealSyn.GetLanguageName: string;
begin
  Result := SYNS_LangUnreal;
end;

class function TSynUnrealSyn.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcUserSettings];
end;

function TSynUnrealSyn.GetSampleSource: string;
begin
  Result := '//----Comment-----------------------------------------------------------'#13#10+
            'class TestObject expands Object native;'#13#10+
            #13#10+
            '#exec MESH    IMPORT     MESH=Something ANIVFILE=MODELS\Something.3D DATAFILE=MODELS\Something.3D X=0 Y=0 Z=0 MLOD=0'#13#10+
            #13#10+
            'var() Sound HitSound;'#13#10+
            'function Cast()'#13#10+
            '{'#13#10+
            '  Super.Cast();'#13#10+
            '  CastTime = 50;'#13#10+
            '  GatherEffect = Spawn( class''SomethingCorona'',,, GetStartLoc(), Pawn(Owner).ViewRotation );'#13#10+
            '  GatherEffect.SetFollowPawn( Pawn(Owner) );'#13#10+
            '}'#13#10+
            #13#10+
            'defaultproperties'#13#10+
            '{'#13#10+
            '  PickupMessage="You have picked up a thing."'#13#10+
            '}';
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynUnrealSyn);
{$ENDIF}
end.
