{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Code template generated with SynGen.
The original code is: SynHighlighterDOT.pas, released 2002-11-30.
Description: DOT Syntax Parser/Highlighter
The initial author of this file is nissl (nissl@tiscali.it, nissl@mammuth.it)
Copyright (c) 2002, all rights reserved.

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

$Id: SynHighlighterDOT.pas,v 1.4 2005/01/28 16:53:21 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}
{
@abstract(Provides a ATT DOT highlighter for SynEdit)
@author(Massimo Maria Ghisalberti (nissl@mammuth.it))
@created(november 2002)
@lastmod(2002-11-30)
The SynHighlighterDOT unit provides SynEdit with a DOT Graph Drawing (.dot) highlighter.
The highlighter formats DOT source code ref.: http://www.research.att.com/sw/tools/graphviz/.
}

{$IFNDEF QSYNHIGHLIGHTERDOT}
unit SynHighlighterDOT;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QControls,
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
{$ELSE}
  Windows,
  Controls,
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (
    tkArrowHead,
    tkAttribute,
    tkComment,
    tkDirections,
    tkIdentifier,
    tkKey,
    tkNull,
    tkShape,
    tkSpace,
    tkString,
    tkUnknown,
    tkValue,
    tkSymbol);

  TRangeState = (rsUnKnown, rsCStyleComment, rsString);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

const
  MaxKey = 174;

type
  TSynDOTSyn = class(TSynCustomHighlighter)
  private
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    fRange: TRangeState;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fIdentFuncTable: array[0 .. MaxKey] of TIdentFuncTableFunc;
    fArrowHeadAttri: TSynHighlighterAttributes;
    fAttributeAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fDirectionsAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fShapeAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fValueAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    function Func5: TtkTokenKind;
    function Func13: TtkTokenKind;
    function Func14: TtkTokenKind;
    function Func17: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func21: TtkTokenKind;
    function Func23: TtkTokenKind;
    function Func24: TtkTokenKind;
    function Func25: TtkTokenKind;
    function Func26: TtkTokenKind;
    function Func29: TtkTokenKind;
    function Func30: TtkTokenKind;
    function Func31: TtkTokenKind;
    function Func32: TtkTokenKind;
    function Func33: TtkTokenKind;
    function Func36: TtkTokenKind;
    function Func37: TtkTokenKind;
    function Func38: TtkTokenKind;
    function Func39: TtkTokenKind;
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
    function Func67: TtkTokenKind;
    function Func68: TtkTokenKind;
    function Func69: TtkTokenKind;
    function Func71: TtkTokenKind;
    function Func72: TtkTokenKind;
    function Func73: TtkTokenKind;
    function Func74: TtkTokenKind;
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
    function Func92: TtkTokenKind;
    function Func93: TtkTokenKind;
    function Func94: TtkTokenKind;
    function Func96: TtkTokenKind;
    function Func99: TtkTokenKind;
    function Func100: TtkTokenKind;
    function Func101: TtkTokenKind;
    function Func102: TtkTokenKind;
    function Func104: TtkTokenKind;
    function Func107: TtkTokenKind;
    function Func109: TtkTokenKind;
    function Func111: TtkTokenKind;
    function Func113: TtkTokenKind;
    function Func114: TtkTokenKind;
    function Func117: TtkTokenKind;
    function Func118: TtkTokenKind;
    function Func120: TtkTokenKind;
    function Func121: TtkTokenKind;
    function Func127: TtkTokenKind;
    function Func128: TtkTokenKind;
    function Func129: TtkTokenKind;
    function Func131: TtkTokenKind;
    function Func133: TtkTokenKind;
    function Func134: TtkTokenKind;
    function Func140: TtkTokenKind;
    function Func142: TtkTokenKind;
    function Func143: TtkTokenKind;
    function Func146: TtkTokenKind;
    function Func150: TtkTokenKind;
    function Func155: TtkTokenKind;
    function Func159: TtkTokenKind;
    function Func174: TtkTokenKind;
    procedure IdentProc;
    procedure UnknownProc;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure NullProc;
    procedure SpaceProc;
    procedure CRProc;
    procedure LFProc;
    procedure CStyleCommentOpenProc;
    procedure CStyleCommentProc;
    procedure StringOpenProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure DirectionsProc;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetLanguageName: string; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetKeyWords: string;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(NewValue: String; LineNumber: Integer); override;
    function GetToken: String; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
  published
    property ArrowHeadAttri: TSynHighlighterAttributes read fArrowHeadAttri write fArrowHeadAttri;
    property AttributeAttri: TSynHighlighterAttributes read fAttributeAttri write fAttributeAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property DirectionsAttri: TSynHighlighterAttributes read fDirectionsAttri write fDirectionsAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property ShapeAttri: TSynHighlighterAttributes read fShapeAttri write fShapeAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
    property ValueAttri: TSynHighlighterAttributes read fValueAttri write fValueAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
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
  mHashTable : array[#0..#255] of Integer;

procedure MakeIdentTable;
var
  I, J: Char;
begin
  for I := #0 to #255 do
  begin
    case I of
      '_', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
    else
      Identifiers[I] := False;
    end;
    J := UpCase(I);
    case I in ['_', 'A'..'Z', 'a'..'z'] of
      True: mHashTable[I] := Ord(J) - 64
    else
      mHashTable[I] := 0;
    end;
  end;
end;

procedure TSynDOTSyn.InitIdent;
var
  I: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do
  begin
    pF^ := AltFunc;
    Inc(pF);
  end;
  fIdentFuncTable[5] := Func5;
  fIdentFuncTable[13] := Func13;
  fIdentFuncTable[14] := Func14;
  fIdentFuncTable[17] := Func17;
  fIdentFuncTable[19] := Func19;
  fIdentFuncTable[21] := Func21;
  fIdentFuncTable[23] := Func23;
  fIdentFuncTable[24] := Func24;
  fIdentFuncTable[25] := Func25;
  fIdentFuncTable[26] := Func26;
  fIdentFuncTable[29] := Func29;
  fIdentFuncTable[30] := Func30;
  fIdentFuncTable[31] := Func31;
  fIdentFuncTable[32] := Func32;
  fIdentFuncTable[33] := Func33;
  fIdentFuncTable[36] := Func36;
  fIdentFuncTable[37] := Func37;
  fIdentFuncTable[38] := Func38;
  fIdentFuncTable[39] := Func39;
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
  fIdentFuncTable[67] := Func67;
  fIdentFuncTable[68] := Func68;
  fIdentFuncTable[69] := Func69;
  fIdentFuncTable[71] := Func71;
  fIdentFuncTable[72] := Func72;
  fIdentFuncTable[73] := Func73;
  fIdentFuncTable[74] := Func74;
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
  fIdentFuncTable[92] := Func92;
  fIdentFuncTable[93] := Func93;
  fIdentFuncTable[94] := Func94;
  fIdentFuncTable[96] := Func96;
  fIdentFuncTable[99] := Func99;
  fIdentFuncTable[100] := Func100;
  fIdentFuncTable[101] := Func101;
  fIdentFuncTable[102] := Func102;
  fIdentFuncTable[104] := Func104;
  fIdentFuncTable[107] := Func107;
  fIdentFuncTable[109] := Func109;
  fIdentFuncTable[111] := Func111;
  fIdentFuncTable[113] := Func113;
  fIdentFuncTable[114] := Func114;
  fIdentFuncTable[117] := Func117;
  fIdentFuncTable[118] := Func118;
  fIdentFuncTable[120] := Func120;
  fIdentFuncTable[121] := Func121;
  fIdentFuncTable[127] := Func127;
  fIdentFuncTable[128] := Func128;
  fIdentFuncTable[129] := Func129;
  fIdentFuncTable[131] := Func131;
  fIdentFuncTable[133] := Func133;
  fIdentFuncTable[134] := Func134;
  fIdentFuncTable[140] := Func140;
  fIdentFuncTable[142] := Func142;
  fIdentFuncTable[143] := Func143;
  fIdentFuncTable[146] := Func146;
  fIdentFuncTable[150] := Func150;
  fIdentFuncTable[155] := Func155;
  fIdentFuncTable[159] := Func159;
  fIdentFuncTable[174] := Func174;
end;

function TSynDOTSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', 'a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end;

function TSynDOTSyn.KeyComp(const aKey: String): Boolean;
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
      if mHashTable[Temp^] <> mHashTable[aKey[i]] then
      begin
        Result := False;
        break;
      end;
      inc(Temp);
    end;
  end
  else
    Result := False;
end;

function TSynDOTSyn.Func5: TtkTokenKind;
begin
  if KeyComp('e') then Result := tkValue else Result := tkIdentifier;
end;

function TSynDOTSyn.Func13: TtkTokenKind;
begin
  if KeyComp('id') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDOTSyn.Func14: TtkTokenKind;
begin
  if KeyComp('n') then Result := tkValue else Result := tkIdentifier;
end;

function TSynDOTSyn.Func17: TtkTokenKind;
begin
  if KeyComp('back') then Result := tkValue else Result := tkIdentifier;
end;

function TSynDOTSyn.Func19: TtkTokenKind;
begin
  if KeyComp('egg') then Result := tkShape else
    if KeyComp('s') then Result := tkValue else
      if KeyComp('ne') then Result := tkValue else Result := tkIdentifier;
end;

function TSynDOTSyn.Func21: TtkTokenKind;
begin
  if KeyComp('edge') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDOTSyn.Func23: TtkTokenKind;
begin
  if KeyComp('w') then Result := tkValue else Result := tkIdentifier;
end;

function TSynDOTSyn.Func24: TtkTokenKind;
begin
  if KeyComp('se') then Result := tkValue else Result := tkIdentifier;
end;

function TSynDOTSyn.Func25: TtkTokenKind;
begin
  if KeyComp('all') then Result := tkValue else Result := tkIdentifier;
end;

function TSynDOTSyn.Func26: TtkTokenKind;
begin
  if KeyComp('z') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func29: TtkTokenKind;
begin
  if KeyComp('page') then Result := tkAttribute else
    if KeyComp('page') then Result := tkValue else Result := tkIdentifier;
end;

function TSynDOTSyn.Func30: TtkTokenKind;
begin
  if KeyComp('lhead') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func31: TtkTokenKind;
begin
  if KeyComp('dir') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func32: TtkTokenKind;
begin
  if KeyComp('label') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func33: TtkTokenKind;
begin
  if KeyComp('bold') then Result := tkValue else Result := tkIdentifier;
end;

function TSynDOTSyn.Func36: TtkTokenKind;
begin
  if KeyComp('min') then Result := tkValue else Result := tkIdentifier;
end;

function TSynDOTSyn.Func37: TtkTokenKind;
begin
  if KeyComp('nw') then Result := tkValue else Result := tkIdentifier;
end;

function TSynDOTSyn.Func38: TtkTokenKind;
begin
  if KeyComp('max') then Result := tkValue else
    if KeyComp('same') then Result := tkValue else
      if KeyComp('node') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDOTSyn.Func39: TtkTokenKind;
begin
  if KeyComp('dot') then Result := tkArrowHead else
    if KeyComp('fill') then Result := tkValue else Result := tkIdentifier;
end;

function TSynDOTSyn.Func41: TtkTokenKind;
begin
  if KeyComp('box') then Result := tkShape else Result := tkIdentifier;
end;

function TSynDOTSyn.Func42: TtkTokenKind;
begin
  if KeyComp('sw') then Result := tkValue else Result := tkIdentifier;
end;

function TSynDOTSyn.Func43: TtkTokenKind;
begin
  if KeyComp('false') then Result := tkValue else Result := tkIdentifier;
end;

function TSynDOTSyn.Func44: TtkTokenKind;
begin
  if KeyComp('rank') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func45: TtkTokenKind;
begin
  if KeyComp('both') then Result := tkValue else
    if KeyComp('inv') then Result := tkArrowHead else Result := tkIdentifier;
end;

function TSynDOTSyn.Func48: TtkTokenKind;
begin
  if KeyComp('none') then Result := tkValue else
    if KeyComp('filled') then Result := tkValue else
      if KeyComp('filled') then Result := tkAttribute else
        if KeyComp('none') then Result := tkArrowHead else Result := tkIdentifier;
end;

function TSynDOTSyn.Func49: TtkTokenKind;
begin
  if KeyComp('shape') then Result := tkAttribute else
    if KeyComp('global') then Result := tkValue else Result := tkIdentifier;
end;

function TSynDOTSyn.Func50: TtkTokenKind;
begin
  if KeyComp('when') then Result := tkAttribute else
    if KeyComp('when') then Result := tkAttribute else
      if KeyComp('graph') then Result := tkKey else
        if KeyComp('headlabel') then Result := tkAttribute else
          if KeyComp('circle') then Result := tkShape else Result := tkIdentifier;
end;

function TSynDOTSyn.Func51: TtkTokenKind;
begin
  if KeyComp('url') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func52: TtkTokenKind;
begin
  if KeyComp('merged') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func53: TtkTokenKind;
begin
  if KeyComp('sink') then Result := tkValue else Result := tkIdentifier;
end;

function TSynDOTSyn.Func54: TtkTokenKind;
begin
  if KeyComp('ltail') then Result := tkAttribute else
    if KeyComp('odot') then Result := tkArrowHead else Result := tkIdentifier;
end;

function TSynDOTSyn.Func56: TtkTokenKind;
begin
  if KeyComp('samehead') then Result := tkAttribute else
    if KeyComp('sides') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func57: TtkTokenKind;
begin
  if KeyComp('height') then Result := tkAttribute else
    if KeyComp('auto') then Result := tkValue else Result := tkIdentifier;
end;

function TSynDOTSyn.Func58: TtkTokenKind;
begin
  if KeyComp('skew') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func59: TtkTokenKind;
begin
  if KeyComp('size') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func60: TtkTokenKind;
begin
  if KeyComp('pagedir') then Result := tkAttribute else
    if KeyComp('diamond') then Result := tkShape else Result := tkIdentifier;
end;

function TSynDOTSyn.Func61: TtkTokenKind;
begin
  if KeyComp('layer') then Result := tkAttribute else
    if KeyComp('layer') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDOTSyn.Func62: TtkTokenKind;
begin
  if KeyComp('labelloc') then Result := tkAttribute else
    if KeyComp('margin') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func63: TtkTokenKind;
begin
  if KeyComp('color') then Result := tkAttribute else
    if KeyComp('ratio') then Result := tkAttribute else
      if KeyComp('record') then Result := tkShape else
        if KeyComp('mcircle') then Result := tkShape else
          if KeyComp('digraph') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDOTSyn.Func64: TtkTokenKind;
begin
  if KeyComp('width') then Result := tkAttribute else
    if KeyComp('onto') then Result := tkAttribute else
      if KeyComp('true') then Result := tkValue else Result := tkIdentifier;
end;

function TSynDOTSyn.Func65: TtkTokenKind;
begin
  if KeyComp('center') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func67: TtkTokenKind;
begin
  if KeyComp('minlen') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func68: TtkTokenKind;
begin
  if KeyComp('house') then Result := tkShape else
    if KeyComp('dotted') then Result := tkValue else Result := tkIdentifier;
end;

function TSynDOTSyn.Func69: TtkTokenKind;
begin
  if KeyComp('headURL') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func71: TtkTokenKind;
begin
  if KeyComp('labelangle') then Result := tkAttribute else
    if KeyComp('decorate') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func72: TtkTokenKind;
begin
  if KeyComp('weight') then Result := tkAttribute else
    if KeyComp('bgcolor') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func73: TtkTokenKind;
begin
  if KeyComp('mdiamond') then Result := tkShape else
    if KeyComp('normal') then Result := tkArrowHead else Result := tkIdentifier;
end;

function TSynDOTSyn.Func74: TtkTokenKind;
begin
  if KeyComp('point') then Result := tkShape else
    if KeyComp('taillabel') then Result := tkAttribute else
      if KeyComp('hexagon') then Result := tkShape else Result := tkIdentifier;
end;

function TSynDOTSyn.Func75: TtkTokenKind;
begin
  if KeyComp('octagon') then Result := tkShape else
    if KeyComp('rankdir') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func76: TtkTokenKind;
begin
  if KeyComp('mrecord') then Result := tkShape else Result := tkIdentifier;
end;

function TSynDOTSyn.Func77: TtkTokenKind;
begin
  if KeyComp('group') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func78: TtkTokenKind;
begin
  if KeyComp('ellipse') then Result := tkShape else
    if KeyComp('nodesep') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func79: TtkTokenKind;
begin
  if KeyComp('mclimit') then Result := tkAttribute else
    if KeyComp('rotate') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func80: TtkTokenKind;
begin
  if KeyComp('layers') then Result := tkAttribute else
    if KeyComp('layers') then Result := tkKey else
      if KeyComp('sametail') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func81: TtkTokenKind;
begin
  if KeyComp('shapefile') then Result := tkAttribute else
    if KeyComp('style') then Result := tkAttribute else
      if KeyComp('source') then Result := tkValue else Result := tkIdentifier;
end;

function TSynDOTSyn.Func82: TtkTokenKind;
begin
  if KeyComp('regular') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func83: TtkTokenKind;
begin
  if KeyComp('comment') then Result := tkAttribute else
    if KeyComp('toplabel') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func84: TtkTokenKind;
begin
  if KeyComp('ranksep') then Result := tkAttribute else
    if KeyComp('invdot') then Result := tkArrowHead else Result := tkIdentifier;
end;

function TSynDOTSyn.Func85: TtkTokenKind;
begin
  if KeyComp('forward') then Result := tkValue else
    if KeyComp('section') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func86: TtkTokenKind;
begin
  if KeyComp('triangle') then Result := tkShape else
    if KeyComp('labelfloat') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func87: TtkTokenKind;
begin
  if KeyComp('headport') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func88: TtkTokenKind;
begin
  if KeyComp('fontname') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func89: TtkTokenKind;
begin
  if KeyComp('strict') then Result := tkKey else
    if KeyComp('appendix') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func90: TtkTokenKind;
begin
  if KeyComp('ordering') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func92: TtkTokenKind;
begin
  if KeyComp('minimum') then Result := tkAttribute else
    if KeyComp('subgraph') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDOTSyn.Func93: TtkTokenKind;
begin
  if KeyComp('tailURL') then Result := tkAttribute else
    if KeyComp('arrowhead') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func94: TtkTokenKind;
begin
  if KeyComp('msquare') then Result := tkShape else Result := tkIdentifier;
end;

function TSynDOTSyn.Func96: TtkTokenKind;
begin
  if KeyComp('nslimit') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func99: TtkTokenKind;
begin
  if KeyComp('invodot') then Result := tkArrowHead else Result := tkIdentifier;
end;

function TSynDOTSyn.Func100: TtkTokenKind;
begin
  if KeyComp('fontpath') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func101: TtkTokenKind;
begin
  if KeyComp('compound') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func102: TtkTokenKind;
begin
  if KeyComp('fillcolor') then Result := tkAttribute else
    if KeyComp('labeljust') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func104: TtkTokenKind;
begin
  if KeyComp('polygon') then Result := tkShape else Result := tkIdentifier;
end;

function TSynDOTSyn.Func107: TtkTokenKind;
begin
  if KeyComp('quantum') then Result := tkAttribute else
    if KeyComp('fixedsize') then Result := tkAttribute else
      if KeyComp('labeldistance') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func109: TtkTokenKind;
begin
  if KeyComp('doublecircle') then Result := tkShape else Result := tkIdentifier;
end;

function TSynDOTSyn.Func111: TtkTokenKind;
begin
  if KeyComp('tailport') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func113: TtkTokenKind;
begin
  if KeyComp('searchsize') then Result := tkAttribute else
    if KeyComp('invhouse') then Result := tkShape else Result := tkIdentifier;
end;

function TSynDOTSyn.Func114: TtkTokenKind;
begin
  if KeyComp('fontsize') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func117: TtkTokenKind;
begin
  if KeyComp('arrowtail') then Result := tkAttribute else
    if KeyComp('bottomlabel') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func118: TtkTokenKind;
begin
  if KeyComp('concentrate') then Result := tkAttribute else
    if KeyComp('fontcolor') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func120: TtkTokenKind;
begin
  if KeyComp('labelfontname') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func121: TtkTokenKind;
begin
  if KeyComp('plaintext') then Result := tkShape else Result := tkIdentifier;
end;

function TSynDOTSyn.Func127: TtkTokenKind;
begin
  if KeyComp('multiples') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func128: TtkTokenKind;
begin
  if KeyComp('peripheries') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func129: TtkTokenKind;
begin
  if KeyComp('trapezium') then Result := tkShape else Result := tkIdentifier;
end;

function TSynDOTSyn.Func131: TtkTokenKind;
begin
  if KeyComp('invtriangle') then Result := tkShape else
    if KeyComp('parallelogram') then Result := tkShape else Result := tkIdentifier;
end;

function TSynDOTSyn.Func133: TtkTokenKind;
begin
  if KeyComp('constraint') then Result := tkAttribute else
    if KeyComp('remincross') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func134: TtkTokenKind;
begin
  if KeyComp('arrowsize') then Result := tkAttribute else
    if KeyComp('doubleoctagon') then Result := tkShape else Result := tkIdentifier;
end;

function TSynDOTSyn.Func140: TtkTokenKind;
begin
  if KeyComp('orientation') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func142: TtkTokenKind;
begin
  if KeyComp('clusterrank') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func143: TtkTokenKind;
begin
  if KeyComp('distortion') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func146: TtkTokenKind;
begin
  if KeyComp('labelfontsize') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func150: TtkTokenKind;
begin
  if KeyComp('labelfontcolor') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func155: TtkTokenKind;
begin
  if KeyComp('tripleoctagon') then Result := tkShape else Result := tkIdentifier;
end;

function TSynDOTSyn.Func159: TtkTokenKind;
begin
  if KeyComp('samplepoints') then Result := tkAttribute else Result := tkIdentifier;
end;

function TSynDOTSyn.Func174: TtkTokenKind;
begin
  if KeyComp('invtrapezium') then Result := tkShape else Result := tkIdentifier;
end;

function TSynDOTSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynDOTSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey <= MaxKey then
    Result := fIdentFuncTable[HashKey]
  else
    Result := tkIdentifier;
end;

procedure TSynDOTSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #0: fProcTable[I] := NullProc;
      #10: fProcTable[I] := LFProc;
      #13: fProcTable[I] := CRProc;
      '/': fProcTable[I] := CStyleCommentOpenProc;
      '-': fProcTable[I] := DirectionsProc;
      '''': fProcTable[I] := StringOpenProc;
      #1..#9,
      #11,
      #12,
      #14..#32 : fProcTable[I] := SpaceProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := IdentProc;
      '~', '{', '}', ',', '(', ')', '[', ']', '<', '>', ':', '?', ';', '!', '=': fProcTable[I] := SymbolProc;
    else
      fProcTable[I] := UnknownProc;
    end;
end;

procedure TSynDOTSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    inc(Run);
  until not (fLine[Run] in [#1..#32]);
end;

procedure TSynDOTSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynDOTSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then
    inc(Run);
end;

procedure TSynDOTSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynDOTSyn.DirectionsProc;
begin
  Inc(Run);
  if (fLine[Run] = '>') or (fLine[Run] = '-') then
  begin
    fTokenID := tkDirections;
    inc(Run);
  end
  else
    fTokenID := tkSymbol;
end;

procedure TSynDOTSyn.CStyleCommentOpenProc;
begin
  Inc(Run);
  if (fLine[Run] = '/') then
  begin
    fTokenID := tkComment;
    inc(Run, 2);
    while not (fLine[Run] in [#0, #10, #13]) do Inc(Run);
    Exit;
  end;
  if (fLine[Run] = '*') then
  begin
    fRange := rsCStyleComment;
    CStyleCommentProc;
    fTokenID := tkComment;
  end
  else
    fTokenID := tkIdentifier;
end;

procedure TSynDOTSyn.CStyleCommentProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      fTokenID := tkComment;
      repeat
        if (fLine[Run] = '*') and
           (fLine[Run + 1] = '/') then
        begin
          Inc(Run, 2);
          fRange := rsUnKnown;
          Break;
        end;
        if not (fLine[Run] in [#0, #10, #13]) then
          Inc(Run);
      until fLine[Run] in [#0, #10, #13];
    end;
  end;
end;

procedure TSynDOTSyn.StringOpenProc;
begin
  Inc(Run);
  fRange := rsString;
  StringProc;
  fTokenID := tkString;
end;

procedure TSynDOTSyn.StringProc;
begin
  fTokenID := tkString;
  repeat
    if (fLine[Run] = '''') then
    begin
      Inc(Run, 1);
      fRange := rsUnKnown;
      Break;
    end;
    if not (fLine[Run] in [#0, #10, #13]) then
      Inc(Run);
  until fLine[Run] in [#0, #10, #13];
end;

constructor TSynDOTSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fArrowHeadAttri := TSynHighLighterAttributes.Create(SYNS_AttrArrowHead);
  fArrowHeadAttri.Foreground := clRed;
  AddAttribute(fArrowHeadAttri);

  fAttributeAttri := TSynHighLighterAttributes.Create(SYNS_AttrAttribute);
  AddAttribute(fAttributeAttri);

  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clNavy;
  AddAttribute(fCommentAttri);

  fDirectionsAttri := TSynHighLighterAttributes.Create(SYNS_AttrDirections);
  fDirectionsAttri.Style := [fsBold];
  fDirectionsAttri.Foreground := clYellow;
  AddAttribute(fDirectionsAttri);

  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);

  fShapeAttri := TSynHighLighterAttributes.Create(SYNS_AttrShape);
  fShapeAttri.Style := [fsBold];
  fShapeAttri.Foreground := clRed;
  AddAttribute(fShapeAttri);

  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString);
  AddAttribute(fStringAttri);

  fValueAttri := TSynHighLighterAttributes.Create(SYNS_AttrValue);
  fValueAttri.Style := [fsItalic];
  fValueAttri.Foreground := clRed;
  AddAttribute(fValueAttri);

  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  fSymbolAttri.Style := [fsBold];
  fSymbolAttri.Foreground := clGreen;
  AddAttribute(fSymbolAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterDOT;
  fRange := rsUnknown;
end;

procedure TSynDOTSyn.SetLine(NewValue: String; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynDOTSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do
    Inc(Run);
end;

procedure TSynDOTSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynDOTSyn.SymbolProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynDOTSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsCStyleComment: CStyleCommentProc;
  else
    begin
      fRange := rsUnknown;
      fProcTable[fLine[Run]];
    end;
  end;
end;

function TSynDOTSyn.GetDefaultAttribute(Index: integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT    : Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER : Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD    : Result := fKeyAttri;
    SYN_ATTR_STRING     : Result := fStringAttri;
    SYN_ATTR_WHITESPACE : Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynDOTSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynDOTSyn.GetKeyWords: string;
begin
  Result :=
    '--,->,all,appendix,arrowhead,arrowsize,arrowtail,auto,back,bgcolor,bo' +
    'ld,both,bottomlabel,box,center,circle,clusterrank,color,comment,compou' +
    'nd,concentrate,constraint,decorate,diamond,digraph,dir,distortion,dot,' +
    'dotted,doublecircle,doubleoctagon,e,edge,egg,ellipse,false,fill,fillco' +
    'lor,filled,fixedsize,fontcolor,fontname,fontpath,fontsize,forward,glob' +
    'al,graph,group,headlabel,headport,headURL,height,hexagon,house,id,inv,' +
    'invdot,invhouse,invodot,invtrapezium,invtriangle,label,labelangle,labe' +
    'ldistance,labelfloat,labelfontcolor,labelfontname,labelfontsize,labelj' +
    'ust,labelloc,layer,layers,lhead,ltail,margin,max,mcircle,mclimit,mdiam' +
    'ond,merged,min,minimum,minlen,mrecord,msquare,multiples,n,ne,node,node' +
    'sep,none,normal,nslimit,nw,octagon,odot,onto,ordering,orientation,page' +
    ',pagedir,parallelogram,peripheries,plaintext,point,polygon,quantum,ran' +
    'k,rankdir,ranksep,ratio,record,regular,remincross,rotate,s,same,samehe' +
    'ad,sametail,samplepoints,se,searchsize,section,shape,shapefile,sides,s' +
    'ink,size,skew,source,strict,style,subgraph,sw,taillabel,tailport,tailU' +
    'RL,toplabel,trapezium,triangle,tripleoctagon,true,url,w,weight,when,wi' +
    'dth,z';
end;

function TSynDOTSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynDOTSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynDOTSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkArrowHead: Result := fArrowHeadAttri;
    tkAttribute: Result := fAttributeAttri;
    tkComment: Result := fCommentAttri;
    tkDirections: Result := fDirectionsAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkShape: Result := fShapeAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkValue: Result := fValueAttri;
    tkUnknown: Result := fIdentifierAttri;
    tkSymbol: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynDOTSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynDOTSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynDOTSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', 'a'..'z', 'A'..'Z'];
end;

function TSynDOTSyn.GetSampleSource: string;
begin
  Result :=
    '// ATT DOT Graphic description language'#13#10 +
    'digraph asde91 {'#13#10 +
    '  ranksep=.75; size = "7.5,7.5";'#13#10 +
    '  {'#13#10 +
    '      node [shape=plaintext, fontsize=16];'#13#10 +
    '      /* the time-line graph */'#13#10 +
    '      past -> 1978 -> 1980 -> 1982 -> 1983 -> 1985 -> 1986 ->'#13#10 +
    '      1987 -> 1988 -> 1989 -> 1990 -> "future";'#13#10 +
    '      /* ancestor programs */'#13#10 +
    '      "Bourne sh"; "make"; "SCCS"; "yacc"; "cron"; "Reiser cpp";'#13#10 +
    '      "Cshell"; "emacs"; "build"; "vi"; "<curses>"; "RCS"; "C*";'#13#10 +
    '  }'#13#10 +
    '      { rank = same;'#13#10 +
    '      "Software IS"; "Configuration Mgt"; "Architecture & Libraries";'#13#10 +
    '      "Process";'#13#10 +
    '  };'#13#10 +
    '    node [shape=box];'#13#10 +
    '    { rank = same; "past"; "SCCS"; "make"; "Bourne sh"; "yacc"; "cron"; }'#13#10 +
    '    { rank = same; 1978; "Reiser cpp"; "Cshell"; }'#13#10 +
    '    { rank = same; 1980; "build"; "emacs"; "vi"; }'#13#10 +
    '    { rank = same; 1982; "RCS"; "<curses>"; "IMX"; "SYNED"; }'#13#10 +
    '    { rank = same; 1983; "ksh"; "IFS"; "TTU"; }'#13#10 +
    '    { rank = same; 1985; "nmake"; "Peggy"; }'#13#10 +
    '    { rank = same; 1986; "C*"; "ncpp"; "ksh-i"; "<curses-i>"; "PG2"; }'#13#10 +
    '    { rank = same; 1987; "Ansi cpp"; "nmake 2.0"; "3D File System"; "fdelta";'#13#10 +
    '        "DAG"; "CSAS";}'#13#10 +
    '    { rank = same; 1988; "CIA"; "SBCS"; "ksh-88"; "PEGASUS/PML"; "PAX";'#13#10 +
    '        "backtalk"; }'#13#10 +
    '    { rank = same; 1989; "CIA++"; "APP"; "SHIP"; "DataShare"; "ryacc";'#13#10 +
    '        "Mosaic"; }'#13#10 +
    '    { rank = same; 1990; "libft"; "CoShell"; "DIA"; "IFS-i"; "kyacc"; "sfio";'#13#10 +
    '        "yeast"; "ML-X"; "DOT"; }'#13#10 +
    '    { rank = same; "future"; "Adv. Software Technology"; }'#13#10 +
    '    "PEGASUS/PML" -> "ML-X";'#13#10 +
    '    "SCCS" -> "nmake";'#13#10 +
    '    "SCCS" -> "3D File System";'#13#10 +
    '    "SCCS" -> "RCS";'#13#10 +
    '    "make" -> "nmake";'#13#10 +
    '    "make" -> "build";'#13#10 +
    '}';
end;

function TSynDOTSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterDOT;
end;

class function TSynDOTSyn.GetLanguageName: string;
begin
  Result := SYNS_LangDOT;
end;

procedure TSynDOTSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynDOTSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynDOTSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynDOTSyn);
{$ENDIF}
end.
