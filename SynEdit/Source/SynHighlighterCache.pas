{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterCache.pas, released 2000-04-21.
The Original Code is based on the mwCacheSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Pavel Krehula.
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

$Id: SynHighlighterCache.pas,v 1.14 2005/01/28 16:53:21 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Cache object script files highlighter for SynEdit)
@author(Pavel Krehula <pavel@mas.cz>, converted to SynEdit by Bruno Mikkelsen <btm@scientist.com>)
@created(1999-12-17, converted to SynEdit 2000-04-21)
@lastmod(2000-06-23)
The SynHighlighterCache unit provides SynEdit with a Cache object script files highlighter.
Thanks to Martin Waldenburg.
}

{$IFNDEF QSYNHIGHLIGHTERCACHE}
unit SynHighlighterCache;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
{$ELSE}
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkClass, tkComment, tkFunction, tkIdentifier, tkKey, tkNull,
    tkNumber, tkDirective, tkSpace, tkString, tkSymbol, tkIndirect, tkLabel,
    tkMacro, tkUserFunction, tkEmbedSQL, tkEmbedText, tkUnknown);

  TRangeState = (rsUnKnown, rsSQL, rsHTML, rsCommand);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

type
  TSynCacheSyn = class(TSynCustomHighlighter)
  private
    fBrace: LongInt;
    fFirstBrace: Boolean;
    fRange: TRangeState;
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..151] of TIdentFuncTableFunc;
    fClassAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fFunctionAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fDirectiveAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fIndirectAttri: TSynHighlighterAttributes;
    fLabelAttri: TSynHighlighterAttributes;
    fMacroAttri: TSynHighlighterAttributes;
    fUserFunctionAttri: TSynHighlighterAttributes;
    fEmbedSQLAttri: TSynHighlighterAttributes;
    fEmbedTextAttri: TSynHighlighterAttributes;

    FCanKey: boolean;    // if true, the next token can be a keyword

    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: String): Boolean;
    function Func1: TtkTokenKind;
    function Func2: TtkTokenKind;
    function Func3: TtkTokenKind;
    function Func4: TtkTokenKind;
    function Func5: TtkTokenKind;
    function Func6: TtkTokenKind;
    function Func7: TtkTokenKind;
    function Func8: TtkTokenKind;
    function Func9: TtkTokenKind;
    function Func10: TtkTokenKind;
    function Func11: TtkTokenKind;
    function Func12: TtkTokenKind;
    function Func13: TtkTokenKind;
    function Func14: TtkTokenKind;
    function Func15: TtkTokenKind;
    function Func16: TtkTokenKind;
    function Func17: TtkTokenKind;
    function Func18: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func20: TtkTokenKind;
    function Func21: TtkTokenKind;
    function Func23: TtkTokenKind;
    function Func24: TtkTokenKind;
    function Func25: TtkTokenKind;
    function Func26: TtkTokenKind;
    function Func27: TtkTokenKind;
    function Func28: TtkTokenKind;
    function Func29: TtkTokenKind;
    function Func30: TtkTokenKind;
    function Func31: TtkTokenKind;
    function Func32: TtkTokenKind;
    function Func33: TtkTokenKind;
    function Func34: TtkTokenKind;
    function Func35: TtkTokenKind;
    function Func36: TtkTokenKind;
    function Func37: TtkTokenKind;
    function Func38: TtkTokenKind;
    function Func39: TtkTokenKind;
    function Func40: TtkTokenKind;
    function Func41: TtkTokenKind;
    function Func42: TtkTokenKind;
    function Func43: TtkTokenKind;
    function Func44: TtkTokenKind;
    function Func45: TtkTokenKind;
    function Func46: TtkTokenKind;
    function Func47: TtkTokenKind;
    function Func48: TtkTokenKind;
    function Func49: TtkTokenKind;
    function Func50: TtkTokenKind;
    function Func51: TtkTokenKind;
    function Func52: TtkTokenKind;
    function Func53: TtkTokenKind;
    function Func54: TtkTokenKind;
    function Func56: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func58: TtkTokenKind;
    function Func59: TtkTokenKind;
    function Func60: TtkTokenKind;
    function Func61: TtkTokenKind;
    function Func62: TtkTokenKind;
    function Func63: TtkTokenKind;
    function Func64: TtkTokenKind;
    function Func65: TtkTokenKind;
    function Func66: TtkTokenKind;
    function Func67: TtkTokenKind;
    function Func68: TtkTokenKind;
    function Func69: TtkTokenKind;
    function Func70: TtkTokenKind;
    function Func71: TtkTokenKind;
    function Func73: TtkTokenKind;
    function Func75: TtkTokenKind;
    function Func76: TtkTokenKind;
    function Func77: TtkTokenKind;
    function Func78: TtkTokenKind;
    function Func79: TtkTokenKind;
    function Func80: TtkTokenKind;
    function Func81: TtkTokenKind;
    function Func82: TtkTokenKind;
    function Func83: TtkTokenKind;
    function Func84: TtkTokenKind;
    function Func85: TtkTokenKind;
    function Func86: TtkTokenKind;
    function Func87: TtkTokenKind;
    function Func88: TtkTokenKind;
    function Func89: TtkTokenKind;
    function Func90: TtkTokenKind;
    function Func91: TtkTokenKind;
    function Func92: TtkTokenKind;
    function Func93: TtkTokenKind;
    function Func94: TtkTokenKind;
    function Func95: TtkTokenKind;
    function Func98: TtkTokenKind;
    function Func100: TtkTokenKind;
    function Func101: TtkTokenKind;
    function Func102: TtkTokenKind;
    function Func103: TtkTokenKind;
    function Func104: TtkTokenKind;
    function Func105: TtkTokenKind;
    function Func106: TtkTokenKind;
    function Func107: TtkTokenKind;
    function Func108: TtkTokenKind;
    function Func110: TtkTokenKind;
    function Func111: TtkTokenKind;
    function Func114: TtkTokenKind;
    function Func115: TtkTokenKind;
    function Func116: TtkTokenKind;
    function Func117: TtkTokenKind;
    function Func123: TtkTokenKind;
    function Func126: TtkTokenKind;
    function Func127: TtkTokenKind;
    function Func128: TtkTokenKind;
    function Func130: TtkTokenKind;
    function Func142: TtkTokenKind;
    function Func143: TtkTokenKind;
    function Func144: TtkTokenKind;
    function Func151: TtkTokenKind;
    procedure CRProc;
    procedure CommentProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure IndirectProc;
    procedure SymbolProc;
    procedure FuncProc;
    procedure DirectiveProc;
    procedure EmbeddedProc;

    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(NewValue: String; LineNumber: Integer); override;
    function GetToken: String; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    property IdentChars;
  published
    property ClassAttri: TSynHighlighterAttributes read fClassAttri
      write fClassAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property FunctionAttri: TSynHighlighterAttributes read fFunctionAttri
      write fFunctionAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property PreprocesorAttri: TSynHighlighterAttributes read fDirectiveAttri
      write fDirectiveAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property IndirectAttri: TSynHighlighterAttributes read fIndirectAttri
      write fIndirectAttri;
    property LabelAttri: TSynHighlighterAttributes read fLabelAttri
      write fLabelAttri;
    property MacroAttri: TSynHighlighterAttributes read fMacroAttri
      write fMacroAttri;
    property UserFunctionAttri: TSynHighlighterAttributes
      read fUserFunctionAttri write fUserFunctionAttri;
    property EmbededSQLandHTMLAttri: TSynHighlighterAttributes
      read fEmbedSQLAttri write fEmbedSQLAttri;
    property EmbededTextAttri: TSynHighlighterAttributes read fEmbedTextAttri
      write fEmbedTextAttri;
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

//------------------------------------------------------------------------------
procedure MakeIdentTable;
var
  I, J: Char;
begin
  for I := #0 to #255 do begin
    Case I of
      '0'..'9', 'a'..'z', 'A'..'Z', '%', '^': Identifiers[I] := True;
      else Identifiers[I] := False;
    end;
    J := UpperCase(I)[1];
    Case I in ['A'..'Z', 'a'..'z'] of
      True: mHashTable[I] := Ord(J) - 64
      else mHashTable[I] := 0;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TSynCacheSyn.InitIdent;
var
  I: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do begin
    pF^ := AltFunc;
    Inc(pF);
  end;
  fIdentFuncTable[1] := Func1;
  fIdentFuncTable[2] := Func2;
  fIdentFuncTable[3] := Func3;
  fIdentFuncTable[4] := Func4;
  fIdentFuncTable[5] := Func5;
  fIdentFuncTable[6] := Func6;
  fIdentFuncTable[7] := Func7;
  fIdentFuncTable[8] := Func8;
  fIdentFuncTable[9] := Func9;
  fIdentFuncTable[10] := Func10;
  fIdentFuncTable[11] := Func11;
  fIdentFuncTable[12] := Func12;
  fIdentFuncTable[13] := Func13;
  fIdentFuncTable[14] := Func14;
  fIdentFuncTable[15] := Func15;
  fIdentFuncTable[16] := Func16;
  fIdentFuncTable[17] := Func17;
  fIdentFuncTable[18] := Func18;
  fIdentFuncTable[19] := Func19;
  fIdentFuncTable[20] := Func20;
  fIdentFuncTable[21] := Func21;
  fIdentFuncTable[23] := Func23;
  fIdentFuncTable[24] := Func24;
  fIdentFuncTable[25] := Func25;
  fIdentFuncTable[26] := Func26;
  fIdentFuncTable[27] := Func27;
  fIdentFuncTable[28] := Func28;
  fIdentFuncTable[29] := Func29;
  fIdentFuncTable[30] := Func30;
  fIdentFuncTable[31] := Func31;
  fIdentFuncTable[32] := Func32;
  fIdentFuncTable[33] := Func33;
  fIdentFuncTable[34] := Func34;
  fIdentFuncTable[35] := Func35;
  fIdentFuncTable[36] := Func36;
  fIdentFuncTable[37] := Func37;
  fIdentFuncTable[38] := Func38;
  fIdentFuncTable[39] := Func39;
  fIdentFuncTable[40] := Func40;
  fIdentFuncTable[41] := Func41;
  fIdentFuncTable[42] := Func42;
  fIdentFuncTable[43] := Func43;
  fIdentFuncTable[44] := Func44;
  fIdentFuncTable[45] := Func45;
  fIdentFuncTable[46] := Func46;
  fIdentFuncTable[47] := Func47;
  fIdentFuncTable[48] := Func48;
  fIdentFuncTable[49] := Func49;
  fIdentFuncTable[50] := Func50;
  fIdentFuncTable[51] := Func51;
  fIdentFuncTable[52] := Func52;
  fIdentFuncTable[53] := Func53;
  fIdentFuncTable[54] := Func54;
  fIdentFuncTable[56] := Func56;
  fIdentFuncTable[57] := Func57;
  fIdentFuncTable[58] := Func58;
  fIdentFuncTable[59] := Func59;
  fIdentFuncTable[60] := Func60;
  fIdentFuncTable[61] := Func61;
  fIdentFuncTable[62] := Func62;
  fIdentFuncTable[63] := Func63;
  fIdentFuncTable[64] := Func64;
  fIdentFuncTable[65] := Func65;
  fIdentFuncTable[66] := Func66;
  fIdentFuncTable[67] := Func67;
  fIdentFuncTable[68] := Func68;
  fIdentFuncTable[69] := Func69;
  fIdentFuncTable[70] := Func70;
  fIdentFuncTable[71] := Func71;
  fIdentFuncTable[73] := Func73;
  fIdentFuncTable[75] := Func75;
  fIdentFuncTable[76] := Func76;
  fIdentFuncTable[77] := Func77;
  fIdentFuncTable[78] := Func78;
  fIdentFuncTable[79] := Func79;
  fIdentFuncTable[80] := Func80;
  fIdentFuncTable[81] := Func81;
  fIdentFuncTable[82] := Func82;
  fIdentFuncTable[83] := Func83;
  fIdentFuncTable[84] := Func84;
  fIdentFuncTable[85] := Func85;
  fIdentFuncTable[86] := Func86;
  fIdentFuncTable[87] := Func87;
  fIdentFuncTable[88] := Func88;
  fIdentFuncTable[89] := Func89;
  fIdentFuncTable[90] := Func90;
  fIdentFuncTable[91] := Func91;
  fIdentFuncTable[92] := Func92;
  fIdentFuncTable[93] := Func93;
  fIdentFuncTable[94] := Func94;
  fIdentFuncTable[95] := Func95;
  fIdentFuncTable[98] := Func98;
  fIdentFuncTable[100] := Func100;
  fIdentFuncTable[101] := Func101;
  fIdentFuncTable[102] := Func102;
  fIdentFuncTable[103] := Func103;
  fIdentFuncTable[104] := Func104;
  fIdentFuncTable[105] := Func105;
  fIdentFuncTable[106] := Func106;
  fIdentFuncTable[107] := Func107;
  fIdentFuncTable[108] := Func108;
  fIdentFuncTable[110] := Func110;
  fIdentFuncTable[111] := Func111;
  fIdentFuncTable[114] := Func114;
  fIdentFuncTable[115] := Func115;
  fIdentFuncTable[116] := Func116;
  fIdentFuncTable[117] := Func117;
  fIdentFuncTable[123] := Func123;
  fIdentFuncTable[126] := Func126;
  fIdentFuncTable[127] := Func127;
  fIdentFuncTable[128] := Func128;
  fIdentFuncTable[130] := Func130;
  fIdentFuncTable[142] := Func142;
  fIdentFuncTable[143] := Func143;
  fIdentFuncTable[144] := Func144;
  fIdentFuncTable[151] := Func151;
end;

//------------------------------------------------------------------------------
function TSynCacheSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['0'..'9', 'a'..'z', 'A'..'Z', '^', '$', '&'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end;

//------------------------------------------------------------------------------
function TSynCacheSyn.KeyComp(const aKey: String): Boolean;
var
  I: Integer;
  Temp: PChar;
begin
  Temp := fToIdent;
  if Length(aKey) = fStringLen then begin
    Result := True;
    for i := 1 to fStringLen do begin
      if mHashTable[Temp^] <> mHashTable[aKey[i]] then begin
        Result := False;
        break;
      end;
      inc(Temp);
    end;
  end else Result := False;
end;

//------------------------------------------------------------------------------
function TSynCacheSyn.Func1: TtkTokenKind;
begin
  if KeyComp('$a') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func2: TtkTokenKind;
begin
  if KeyComp('b') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func3: TtkTokenKind;
begin
  if KeyComp('$c') then Result := tkKey else
    if KeyComp('c') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func4: TtkTokenKind;
begin
  if KeyComp('d') then Result := tkKey else
    if KeyComp('$d') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func5: TtkTokenKind;
begin
  if KeyComp('$e') then Result := tkKey else
    if KeyComp('e') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func6: TtkTokenKind;
begin
  if KeyComp('$f') then Result := tkKey else
    if KeyComp('f') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func7: TtkTokenKind;
begin
  if KeyComp('$g') then Result := tkKey else
    if KeyComp('^$g') then Result := tkKey else
      if KeyComp('g') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func8: TtkTokenKind;
begin
  if KeyComp('$ec') then Result := tkKey else
    if KeyComp('h') then Result := tkKey else
      if KeyComp('$h') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func9: TtkTokenKind;
begin
  if KeyComp('$i') then Result := tkKey else
    if KeyComp('$i') then Result := tkKey else
      if KeyComp('i') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func10: TtkTokenKind;
begin
  if KeyComp('^$j') then Result := tkKey else
      if KeyComp('$j') then Result := tkKey else
        if KeyComp('j') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func11: TtkTokenKind;
begin
  if KeyComp('k') then Result := tkKey else
    if KeyComp('$k') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func12: TtkTokenKind;
begin
  if KeyComp('^$l') then Result := tkKey else
    if KeyComp('l') then Result := tkKey else
      if KeyComp('$l') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func13: TtkTokenKind;
begin
  if KeyComp('m') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func14: TtkTokenKind;
begin
  if KeyComp('$lb') then Result := tkKey else
    if KeyComp('$n') then Result := tkKey else
      if KeyComp('n') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func15: TtkTokenKind;
begin
  if KeyComp('$na') then Result := tkKey else
    if KeyComp('if') then Result := tkKey else
      if KeyComp('$o') then Result := tkKey else
        if KeyComp('o') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func16: TtkTokenKind;
begin
  if KeyComp('$p') then Result := tkKey else
      if KeyComp('p') then Result := tkKey else
        if KeyComp('$ld') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func17: TtkTokenKind;
begin
  if KeyComp('$q') then Result := tkKey else
      if KeyComp('q') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func18: TtkTokenKind;
begin
  if KeyComp('r') then Result := tkKey else
    if KeyComp('^$r') then Result := tkKey else
      if KeyComp('$r') then Result := tkKey else
        if KeyComp('$lf') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func19: TtkTokenKind;
begin
  if KeyComp('$lg') then Result := tkKey else
    if KeyComp('$s') then Result := tkKey else
      if KeyComp('s') then Result := tkKey else
        if KeyComp('do') then Result := tkKey else
          if KeyComp('$s') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func20: TtkTokenKind;
begin
  if KeyComp('$fn') then Result := tkKey else
      if KeyComp('$t') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func21: TtkTokenKind;
begin
  if KeyComp('$li') then Result := tkKey else
    if KeyComp('u') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func23: TtkTokenKind;
begin
  if KeyComp('w') then Result := tkKey else
    if KeyComp('tc') then Result := tkKey else
      if KeyComp('$in') then Result := tkKey else
        if KeyComp('$re') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func24: TtkTokenKind;
begin
  if KeyComp('$es') then Result := tkKey else
    if KeyComp('$ll') then Result := tkKey else
      if KeyComp('$io') then Result := tkKey else
        if KeyComp('x') then Result := tkKey else
          if KeyComp('$x') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func25: TtkTokenKind;
begin
  if KeyComp('$et') then Result := tkKey else
    if KeyComp('$y') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func26: TtkTokenKind;
begin
  if KeyComp('$data') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func27: TtkTokenKind;
begin
  if KeyComp('job') then Result := tkKey else
    if KeyComp('$za') then Result := tkKey else
      if KeyComp('$job') then Result := tkKey else
        if KeyComp('^$job') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func28: TtkTokenKind;
begin
  if KeyComp('$zb') then Result := tkKey else
    if KeyComp('zb') then Result := tkKey else
      if KeyComp('read') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func29: TtkTokenKind;
begin
  if KeyComp('$ql') then Result := tkKey else
    if KeyComp('$zc') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func30: TtkTokenKind;
begin
  if KeyComp('hang') then Result := tkKey else
    if KeyComp('$char') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func31: TtkTokenKind;
begin
  if KeyComp('$vi') then Result := tkKey else
    if KeyComp('$ze') then Result := tkKey else
      if KeyComp('vi') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func32: TtkTokenKind;
begin
  if KeyComp('$get') then Result := tkKey else
    if KeyComp('$tl') then Result := tkKey else
      if KeyComp('$ecode') then Result := tkKey else
        if KeyComp('$zf') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func33: TtkTokenKind;
begin
  if KeyComp('$find') then Result := tkKey else
    if KeyComp('$name') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func34: TtkTokenKind;
begin
    if KeyComp('$zh') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func35: TtkTokenKind;
begin
  if KeyComp('$zi') then Result := tkKey else
    if KeyComp('zi') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func36: TtkTokenKind;
begin
  if KeyComp('$qs') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func37: TtkTokenKind;
begin
  if KeyComp('zk') then Result := tkKey else
    if KeyComp('break') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func38: TtkTokenKind;
begin
  if KeyComp('$piece') then Result := tkKey else
    if KeyComp('zl') then Result := tkKey else
      if KeyComp('$tr') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func39: TtkTokenKind;
begin
  if KeyComp('$st') then Result := tkKey else
    if KeyComp('$st') then Result := tkKey else
      if KeyComp('$zla') then Result := tkKey else
        if KeyComp('for') then Result := tkKey else
          if KeyComp('ts') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func40: TtkTokenKind;
begin
  if KeyComp('zn') then Result := tkKey else
    if KeyComp('$zn') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func41: TtkTokenKind;
begin
  if KeyComp('$key') then Result := tkKey else
    if KeyComp('else') then Result := tkKey else
      if KeyComp('halt') then Result := tkKey else
        if KeyComp('$zo') then Result := tkKey else
          if KeyComp('^$lock') then Result := tkKey else
            if KeyComp('lock') then Result := tkKey else
              if KeyComp('$zlc') then Result := tkKey else
                if KeyComp('$ascii') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func42: TtkTokenKind;
begin
  if KeyComp('zp') then Result := tkKey else
        if KeyComp('$zp') then Result := tkKey else
          if KeyComp('new') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func43: TtkTokenKind;
begin
  if KeyComp('zq') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func44: TtkTokenKind;
begin
  if KeyComp('$zr') then Result := tkKey else
    if KeyComp('kill') then Result := tkKey else
      if KeyComp('set') then Result := tkKey else
        if KeyComp('zr') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func45: TtkTokenKind;
begin
  if KeyComp('zs') then Result := tkKey else
    if KeyComp('use') then Result := tkKey else
      if KeyComp('$zs') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func46: TtkTokenKind;
begin
  if KeyComp('$zt') then Result := tkKey else
    if KeyComp('$zt') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func47: TtkTokenKind;
begin
  if KeyComp('$zu') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func48: TtkTokenKind;
begin
  if KeyComp('$zabs') then Result := tkKey else
    if KeyComp('merge') then Result := tkKey else
      if KeyComp('$zv') then Result := tkKey else
        if KeyComp('&sql') then begin
          Result := tkEmbedSQL;
          fRange := rsSQL;
        end else
          if KeyComp('$device') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func49: TtkTokenKind;
begin
  if KeyComp('^$global') then Result := tkKey else
    if KeyComp('$zw') then Result := tkKey else
      if KeyComp('zw') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func50: TtkTokenKind;
begin
  if KeyComp('$zcrc') then Result := tkKey else
    if KeyComp('$zio') then Result := tkKey else
      if KeyComp('$zwa') then Result := tkKey else
        if KeyComp('$zse') then Result := tkKey else
          if KeyComp('open') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func51: TtkTokenKind;
begin
  if KeyComp('$zcsc') then Result := tkKey else
    if KeyComp('$zpi') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func52: TtkTokenKind;
begin
  if KeyComp('$zz') then Result := tkKey else
    if KeyComp('$zeof') then Result := tkKey else
      if KeyComp('$zwc') then Result := tkKey else
        if KeyComp('$zln') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func53: TtkTokenKind;
begin
  if KeyComp('$zjob') then Result := tkKey else
    if KeyComp('tro') then Result := tkKey else
      if KeyComp('&html') then begin
        Result := tkEmbedSQL;
        fRange := rsHTML;
      end else
        if KeyComp('$zsec') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func54: TtkTokenKind;
begin
  if KeyComp('$stack') then Result := tkKey else
    if KeyComp('$zis') then Result := tkKey else
      if KeyComp('$zth') then Result := tkKey else
        if KeyComp('$stack') then Result := tkKey else
          if KeyComp('close') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func56: TtkTokenKind;
begin
  if KeyComp('$zdate') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func57: TtkTokenKind;
begin
  if KeyComp('goto') then Result := tkKey else
    if KeyComp('$zcyc') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func58: TtkTokenKind;
begin
  if KeyComp('zload') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func59: TtkTokenKind;
begin
  if KeyComp('view') then Result := tkKey else
    if KeyComp('$estack') then Result := tkKey else
      if KeyComp('$zname') then Result := tkKey else
        if KeyComp('$view') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func60: TtkTokenKind;
begin
  if KeyComp('$list') then Result := tkKey else
    if KeyComp('$etrap') then Result := tkKey else
      if KeyComp('$zlog') then Result := tkKey else
        if KeyComp('$order') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func61: TtkTokenKind;
begin
  if KeyComp('$ztan') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func62: TtkTokenKind;
begin
  if KeyComp('$zchild') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func63: TtkTokenKind;
begin
  if KeyComp('$next') then Result := tkKey else
    if KeyComp('zbreak') then Result := tkKey else
      if KeyComp('$zcos') then Result := tkKey else
        if KeyComp('$zhex') then Result := tkKey else
          if KeyComp('$zmode') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func64: TtkTokenKind;
begin
  if KeyComp('$zzdec') then Result := tkKey else
    if KeyComp('$test') then Result := tkKey else
      if KeyComp('$select') then Result := tkKey else
        if KeyComp('$zcot') then Result := tkKey else
          if KeyComp('$zdateh') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func65: TtkTokenKind;
begin
  if KeyComp('$zts') then Result := tkKey else
    if KeyComp('$zwp') then Result := tkKey else
      if KeyComp('$random') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func66: TtkTokenKind;
begin
  if KeyComp('$length') then Result := tkKey else
    if KeyComp('$zseek') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func67: TtkTokenKind;
begin
  if KeyComp('quit') then Result := tkKey else
    if KeyComp('$zerr') then Result := tkKey else
      if KeyComp('$quit') then Result := tkKey else
        if KeyComp('$zwbp') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func68: TtkTokenKind;
begin
  if KeyComp('$zsin') then Result := tkKey else
    if KeyComp('$zlchar') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func69: TtkTokenKind;
begin
  if KeyComp('$text') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func70: TtkTokenKind;
begin
  if KeyComp('zkill') then Result := tkKey else
    if KeyComp('$zincr') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func71: TtkTokenKind;
begin
  if KeyComp('$zexp') then Result := tkKey else
    if KeyComp('$zcvt') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func73: TtkTokenKind;
begin
  if KeyComp('zsave') then Result := tkKey else
    if KeyComp('$ztime') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func75: TtkTokenKind;
begin
  if KeyComp('write') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func76: TtkTokenKind;
begin
  if KeyComp('$zbitand') then Result := tkKey else
    if KeyComp('$tlevel') then Result := tkKey else
      if KeyComp('$zpos') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func77: TtkTokenKind;
begin
  if KeyComp('print') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func78: TtkTokenKind;
begin
  if KeyComp('xecute') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func79: TtkTokenKind;
begin
  if KeyComp('$zlascii') then Result := tkKey else
    if KeyComp('$zwchar') then Result := tkKey else
      if KeyComp('$fnumber') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func80: TtkTokenKind;
begin
  if KeyComp('$zsqr') then Result := tkKey else
    if KeyComp('$zsearch') then Result := tkKey else
      if KeyComp('$zwpack') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func81: TtkTokenKind;
begin
  if KeyComp('ztrap') then Result := tkKey else
    if KeyComp('$ztimeh') then Result := tkKey else
      if KeyComp('$ztrap') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func82: TtkTokenKind;
begin
  if KeyComp('$inumber') then Result := tkKey else
    if KeyComp('$zwbpack') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func83: TtkTokenKind;
begin
  if KeyComp('$zarctan') then Result := tkKey else
    if KeyComp('$qlength') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func84: TtkTokenKind;
begin
  if KeyComp('$znspace') then Result := tkKey else
    if KeyComp('znspace') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func85: TtkTokenKind;
begin
  if KeyComp('$storage') then Result := tkKey else
    if KeyComp('$zarccos') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func86: TtkTokenKind;
begin
  if KeyComp('$zorder') then Result := tkKey else
    if KeyComp('$zorder') then Result := tkKey else
      if KeyComp('$query') then Result := tkKey else
        if KeyComp('$listdata') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func87: TtkTokenKind;
begin
  if KeyComp('zsync') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func88: TtkTokenKind;
begin
  if KeyComp('$zutil') then Result := tkKey else
    if KeyComp('$zbitlen') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func89: TtkTokenKind;
begin
  if KeyComp('$zbitget') then Result := tkKey else
    if KeyComp('$znext') then Result := tkKey else
      if KeyComp('$zzhex') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func90: TtkTokenKind;
begin
  if KeyComp('$zboolean') then Result := tkKey else
    if KeyComp('$zbitor') then Result := tkKey else
      if KeyComp('$zarcsin') then Result := tkKey else
        if KeyComp('$zbitfind') then Result := tkKey else
          if KeyComp('$zwidth') then Result := tkKey else
            if KeyComp('$zwascii') then Result := tkKey else
              if KeyComp('$horolog') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func91: TtkTokenKind;
begin
  if KeyComp('$extract') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func92: TtkTokenKind;
begin
  if KeyComp('$reverse') then Result := tkKey else
    if KeyComp('$listget') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func93: TtkTokenKind;
begin
  if KeyComp('zquit') then Result := tkKey else
    if KeyComp('$listfind') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func94: TtkTokenKind;
begin
  if KeyComp('trollback') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func95: TtkTokenKind;
begin
  if KeyComp('$ziswide') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func98: TtkTokenKind;
begin
  if KeyComp('$zsort') then Result := tkKey else
    if KeyComp('tstart') then Result := tkKey else
      if KeyComp('$principal') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func100: TtkTokenKind;
begin
  if KeyComp('$zparent') then Result := tkKey else
    if KeyComp('$zerror') then Result := tkKey else
      if KeyComp('$zwunp') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func101: TtkTokenKind;
begin
  if KeyComp('$zbitset') then Result := tkKey else
    if KeyComp('zwrite') then Result := tkKey else
      if KeyComp('$increment') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func102: TtkTokenKind;
begin
  if KeyComp('$zwbunp') then Result := tkKey else
    if KeyComp('^$routine') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func103: TtkTokenKind;
begin
  if KeyComp('$zdatetime') then Result := tkKey else
    if KeyComp('$zpower') then Result := tkKey else
      if KeyComp('zprint') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func104: TtkTokenKind;
begin
  if KeyComp('zremove') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func105: TtkTokenKind;
begin
  if KeyComp('$zreference') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func106: TtkTokenKind;
begin
  if KeyComp('$zbitnot') then Result := tkKey else
    if KeyComp('zzdump') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func107: TtkTokenKind;
begin
  if KeyComp('tcommint') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func108: TtkTokenKind;
begin
  if KeyComp('$zstrip') then Result := tkKey else
    if KeyComp('$listbuild') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func110: TtkTokenKind;
begin
  if KeyComp('$translate') then Result := tkKey else
    if KeyComp('$justify') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func111: TtkTokenKind;
begin
  if KeyComp('$zstorage') then Result := tkKey else
    if KeyComp('zinsert') then Result := tkKey else
      if KeyComp('$zdatetimeh') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func114: TtkTokenKind;
begin
  if KeyComp('$zbitxor') then Result := tkKey else
    if KeyComp('$zbitstr') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func115: TtkTokenKind;
begin
  if KeyComp('$zwunpack') then Result := tkKey else
    if KeyComp('$zzenkaku') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func116: TtkTokenKind;
begin
  if KeyComp('$zhorolog') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func117: TtkTokenKind;
begin
  if KeyComp('$zwbunpack') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func123: TtkTokenKind;
begin
  if KeyComp('$zconvert') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func126: TtkTokenKind;
begin
  if KeyComp('$listlength') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func127: TtkTokenKind;
begin
  if KeyComp('$zincrement') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func128: TtkTokenKind;
begin
  if KeyComp('$zversion') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func130: TtkTokenKind;
begin
  if KeyComp('$zbitcount') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func142: TtkTokenKind;
begin
  if KeyComp('$ztimestamp') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func143: TtkTokenKind;
begin
  if KeyComp('$zposition') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func144: TtkTokenKind;
begin
  if KeyComp('$qsubscript') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.Func151: TtkTokenKind;
begin
  if KeyComp('$zprevious') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCacheSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

//------------------------------------------------------------------------------
function TSynCacheSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if (HashKey < 152) and (HashKey>-1) then
    Result := fIdentFuncTable[HashKey]
  else Result := tkIdentifier;
end;

//------------------------------------------------------------------------------
procedure TSynCacheSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #13: fProcTable[I] := CRProc;
      ';': fProcTable[I] := CommentProc;
      'A'..'Z', 'a'..'z', '%', '^': fProcTable[I] := IdentProc;
      '$': fProcTable[i] := FuncProc;
      '@': fProcTable[i] := IndirectProc;
      #10: fProcTable[I] := LFProc;
      #0: fProcTable[I] := NullProc;
      '0'..'9': fProcTable[I] := NumberProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := SpaceProc;
      #34: fProcTable[I] := StringProc;
      '(',')','+','-','[',']','.','<','>','''','=',',',':','/','\',
      '?','!','_','*': fProcTable[i] := SymbolProc;
      '#': fProcTable[i] := DirectiveProc;
      '&': fProcTable[i] := EmbeddedProc;

    else fProcTable[I] := UnknownProc;
    end;
end;

//------------------------------------------------------------------------------
constructor TSynCacheSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fClassAttri := TSynHighlighterAttributes.Create(SYNS_AttrClass);
  AddAttribute(fClassAttri);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fFunctionAttri := TSynHighlighterAttributes.Create(SYNS_AttrFunction);
  AddAttribute(fFunctionAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  AddAttribute(fNumberAttri);
  fDirectiveAttri := TSynHighlighterAttributes.Create(SYNS_AttrDir);
  AddAttribute(fDirectiveAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);
  fIndirectAttri := TSynHighlighterAttributes.Create(SYNS_AttrIndirect);
  AddAttribute(fIndirectAttri);
  fLabelAttri := TSynHighlighterAttributes.Create(SYNS_AttrLabel);
  AddAttribute(fLabelAttri);
  fMacroAttri := TSynHighlighterAttributes.Create(SYNS_AttrMacro);
  AddAttribute(fMacroAttri);
  fUserFunctionAttri := TSynHighlighterAttributes.Create(SYNS_AttrUserFunction);
  AddAttribute(fUserFunctionAttri);
  fEmbedSQLAttri := TSynHighlighterAttributes.Create(SYNS_AttrEmbedSQL);
  AddAttribute(fEmbedSQLAttri);
  fEmbedTextAttri := TSynHighlighterAttributes.Create(SYNS_AttrEmbedText);
  AddAttribute(fEmbedTextAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterCache;
  fRange := rsUnknown;
end;

procedure TSynCacheSyn.SetLine(NewValue: string; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynCacheSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then inc(Run);
  FRange := rsUnknown;
end;

//------------------------------------------------------------------------------
procedure TSynCacheSyn.CommentProc;
begin
  fTokenID := tkComment;
  if FLine[Run+1]=';' then fTokenID := tkEmbedText;

  while FLine[Run] <> #0 do  begin
    case FLine[Run] of
      #10, #13: break;
    end;
    inc(Run);
  end;
end;

//------------------------------------------------------------------------------
//    higlight keywords and identifiers
//------------------------------------------------------------------------------
procedure TSynCacheSyn.IdentProc;
var
  fir: char;
begin
  if FTokenPos=0 then fTokenID := tkLabel
  else begin
    fir := FLine[ Run ];
    if fir in [ '^' ] then FCanKey := true;

    FRange := rsUnknown;
    if FCanKey then  fTokenID := IdentKind((fLine + Run))
    else begin
      fTokenID := tkIdentifier;
      while ( Identifiers[fLine[Run]] ) or ( FLine[Run] in ['0'..'9'])  do inc(Run);
      exit;
    end;
    FRange := rsCommand;
    inc(Run, fStringLen);
    if (not ( FLine[Run] in [ #32, ':', #0, #10, #13 ] )) and ( fir <> '^' ) then
      fTokenID := tkIdentifier;
  end;
  while ( Identifiers[fLine[Run]] ) or ( FLine[Run] in ['0'..'9'])  do inc(Run);
end;

//------------------------------------------------------------------------------
procedure TSynCacheSyn.LFProc;
begin
  fTokenID := tkSpace;
  FCanKey := true;
  inc(Run);
end;

procedure TSynCacheSyn.NullProc;
begin
  fTokenID := tkNull;
end;

//------------------------------------------------------------------------------
procedure TSynCacheSyn.NumberProc;
begin
  if (fTokenPos = 0) and (FLine[Run] in ['0'..'9']) then begin
    fTokenID := tkLabel;
    while Identifiers[fLine[Run]] do inc(Run);
    FCanKey := false;
    exit;
  end;

  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', 'e', 'E'] do  begin
    case FLine[Run] of
      '.':  if FLine[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
  FRange := rsUnknown;
end;

//------------------------------------------------------------------------------
procedure TSynCacheSyn.SpaceProc;
var
  x: integer;
begin
  x := Run;
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
  FCanKey := true;
  if FRange = rsCommand then
    FCanKey := (Run - x > 1);
end;

//------------------------------------------------------------------------------
procedure TSynCacheSyn.StringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = #34;
  if FLine[Run] <> #0 then inc(Run);
  FRange := rsUnknown;
end;

//------------------------------------------------------------------------------
procedure TSynCacheSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

//------------------------------------------------------------------------------
procedure TSynCacheSyn.Next;
begin
  fTokenPos := Run;
  if FLine[Run] = #0 then NullProc
  else
    Case fRange of
      rsSQL,
      rsHTML: EmbeddedProc;
      else fProcTable[fLine[Run]];
    end;
end;

function TSynCacheSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynCacheSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynCacheSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynCacheSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynCacheSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynCacheSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkClass: Result := fClassAttri;
    tkComment: Result := fCommentAttri;
    tkFunction: Result := fFunctionAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkDirective: Result := fDirectiveAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkIndirect: Result := fIndirectAttri;
    tkUnknown: Result := fIdentifierAttri;
    tkLabel: Result := fLabelAttri;
    tkMacro: Result := fMacroAttri;
    tkUserFunction: Result := fUserFunctionAttri;
    tkEmbedSQL: Result := fEmbedSQLAttri;
    tkEmbedText: Result := fEmbedTextAttri;
  else Result := nil;
  end;
end;

function TSynCacheSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynCacheSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynCacheSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynCacheSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

//------------------------------------------------------------------------------
function TSynCacheSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['0'..'9', 'a'..'z', 'A'..'Z', '^', '%'] + TSynSpecialChars;
end;

function TSynCacheSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterCache;
end;

class function TSynCacheSyn.GetLanguageName: string;
begin
  Result := SYNS_LangCache;
end;

//------------------------------------------------------------------------------
//   highlight indirection syntax:   @ident
//------------------------------------------------------------------------------
procedure TSynCacheSyn.IndirectProc;
begin
  fTokenID := tkIndirect;
  inc( Run );
  while Identifiers[ FLine[Run] ] do inc( Run );
  FRange := rsUnknown;
end;

//------------------------------------------------------------------------------
//  highlight symbols
//------------------------------------------------------------------------------
procedure TSynCacheSyn.SymbolProc;
begin
  fTokenID := tkSymbol;
  inc( Run );
  FRange := rsUnknown;
end;

//------------------------------------------------------------------------------
//  highlight user defined functions and macros
//              function:   $$ident
//              macro   :   $$$ident
//------------------------------------------------------------------------------
procedure TSynCacheSyn.FuncProc;
begin
  case FLine[Run] of
    '$': case FLine[ Run+1 ] of
           '$': case Fline[ Run+2 ] of
                  '$': fTokenID := tkMacro;
                  else fTokenID := tkUserFunction;
                end;
           else begin
                  fTokenID := IdentKind((fLine + Run));
                  inc(Run, fStringLen);
                  if fTokenID = tkKey then fTokenID := tkFunction;
                end;
         end;
    else fTokenID := tkIdentifier;
  end;
  while Identifiers[fLine[Run]] or (FLine[Run]='$' ) do inc(Run);
  FRange := rsUnknown;
end;

//------------------------------------------------------------------------------
//    highlight preprocesor directives and class syntax
//              preprocesor:  #identifier
//              class      :  ##class
//------------------------------------------------------------------------------
procedure TSynCacheSyn.DirectiveProc;
var
  i: integer;
begin
  if FLine[Run+1]='#' then fTokenID := tkClass
  else begin
    for i:=fTokenPos downto 0 do
      if not(FLine[i] in [ #32, '#' ]) then begin
        fTokenID := tkSymbol;
        inc( Run );
        exit;
      end;

    fTokenID := tkDirective
  end;

  inc( Run );
  while Identifiers[fLine[Run]] or (FLine[Run]='#') do inc(Run);
  FRange := rsUnknown;
end;

//------------------------------------------------------------------------------
//  highlight embeded SQL and HTML
//                SQL  :    &sql( .... )
//                HTML :    &html<   ..... >
//------------------------------------------------------------------------------
procedure TSynCacheSyn.EmbeddedProc;
begin
  case fRange of
    rsUnknown, rsCommand: begin
                 fTokenID := IdentKind( (fLine + Run) );
                 if fTokenID <> tkEmbedSQL then begin
                   fTokenID := tkSymbol;
                   inc( Run );
                 end else begin
                   fBrace := 1;
                   fFirstBrace := true;
                   inc( Run, fStringLen );
                 end;
               end;
    rsSQL: begin
             fTokenID := tkEmbedSQL;
             while (FLine[Run] <> #0) and (fBrace<>0) do begin
               case FLine[Run] of
                 '(': if not fFirstBrace then inc(fBrace)
                      else fFirstBrace := false;
                 ')': dec(fBrace);
               end;
               inc(Run);
             end;
             if fBrace=0 then fRange := rsUnknown;
           end;
    rsHTML: begin
              fTokenID := tkEmbedSQL;
              while (FLine[Run] <> #0) and (fBrace<>0) do begin
                case FLine[Run] of
                  '<': if not fFirstBrace then inc(fBrace)
                       else fFirstBrace := false;
                  '>': dec(fBrace);
                end;
                inc(Run);
              end;
              if fBrace=0 then fRange := rsUnknown;
            end;
  end;
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynCacheSyn);
{$ENDIF}
end.
