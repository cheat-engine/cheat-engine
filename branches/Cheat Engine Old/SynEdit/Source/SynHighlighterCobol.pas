{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Code template generated with SynGen.
The original code is: SynHighlighterCobol.pas, released 2002-08-26.
Description: COBOL Syntax Parser/Highlighter
The author of this file is Andrey Ustinov.
Copyright (c) 2002 Software Mining, http://www.softwaremining.com/,
all rights reserved.

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

$Id: SynHighlighterCobol.pas,v 1.6 2005/01/28 16:53:21 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

{$IFNDEF QSYNHIGHLIGHTERCOBOL}
unit SynHighlighterCobol;
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
  TtkTokenKind = (
    tkComment,
    tkIdentifier,
    tkAIdentifier,
    tkPreprocessor,
    tkKey,
    tkBoolean,
    tkNull,
    tkNumber,
    tkSpace,
    tkString,
    tkSequence,
    tkIndicator,
    tkTagArea,
    tkDebugLines,
    tkUnknown);

  TRangeState = (rsUnknown,
                 rsQuoteString, rsApostString,
                 rsPseudoText,
                 rsQuoteStringMayBe, rsApostStringMayBe);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

const
  MaxKey = 208;

type
  TSynCobolSyn = class(TSynCustomHighlighter)
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
    fIndicator: Char;

    fCodeStartPos: LongInt;
    fCodeMediumPos: LongInt;
    fCodeEndPos: LongInt;

    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fAIdentifierAttri: TSynHighlighterAttributes;
    fPreprocessorAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fBooleanAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSequenceAttri: TSynHighlighterAttributes;
    fIndicatorAttri: TSynHighlighterAttributes;
    fTagAreaAttri: TSynHighlighterAttributes;
    fDebugLinesAttri: TSynHighlighterAttributes;

    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    function Func2: TtkTokenKind;
    function Func5: TtkTokenKind;
    function Func6: TtkTokenKind;
    function Func7: TtkTokenKind;
    function Func9: TtkTokenKind;
    function Func10: TtkTokenKind;
    function Func11: TtkTokenKind;
    function Func13: TtkTokenKind;
    function Func15: TtkTokenKind;
    function Func16: TtkTokenKind;
    function Func17: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func21: TtkTokenKind;
    function Func22: TtkTokenKind;
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
    function Func55: TtkTokenKind;
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
    function Func91: TtkTokenKind;
    function Func92: TtkTokenKind;
    function Func93: TtkTokenKind;
    function Func94: TtkTokenKind;
    function Func95: TtkTokenKind;
    function Func96: TtkTokenKind;
    function Func97: TtkTokenKind;
    function Func98: TtkTokenKind;
    function Func99: TtkTokenKind;
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
    function Func112: TtkTokenKind;
    function Func113: TtkTokenKind;
    function Func114: TtkTokenKind;
    function Func115: TtkTokenKind;
    function Func116: TtkTokenKind;
    function Func117: TtkTokenKind;
    function Func119: TtkTokenKind;
    function Func120: TtkTokenKind;
    function Func121: TtkTokenKind;
    function Func122: TtkTokenKind;
    function Func123: TtkTokenKind;
    function Func124: TtkTokenKind;
    function Func125: TtkTokenKind;
    function Func126: TtkTokenKind;
    function Func127: TtkTokenKind;
    function Func128: TtkTokenKind;
    function Func130: TtkTokenKind;
    function Func131: TtkTokenKind;
    function Func132: TtkTokenKind;
    function Func133: TtkTokenKind;
    function Func134: TtkTokenKind;
    function Func137: TtkTokenKind;
    function Func138: TtkTokenKind;
    function Func140: TtkTokenKind;
    function Func141: TtkTokenKind;
    function Func146: TtkTokenKind;
    function Func147: TtkTokenKind;
    function Func149: TtkTokenKind;
    function Func150: TtkTokenKind;
    function Func152: TtkTokenKind;
    function Func153: TtkTokenKind;
    function Func157: TtkTokenKind;
    function Func160: TtkTokenKind;
    function Func161: TtkTokenKind;
    function Func163: TtkTokenKind;
    function Func165: TtkTokenKind;
    function Func167: TtkTokenKind;
    function Func173: TtkTokenKind;
    function Func174: TtkTokenKind;
    function Func188: TtkTokenKind;
    function Func208: TtkTokenKind;
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
    procedure NumberProc;
    procedure PointProc;
    procedure StringOpenProc;
    procedure StringProc;
    procedure StringEndProc;
    procedure FirstCharsProc;
    procedure LastCharsProc;
    procedure CommentProc;
    procedure DebugProc;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;

    procedure SetCodeStartPos(Value: LongInt);
    procedure SetCodeMediumPos(Value: LongInt);
    procedure SetCodeEndPos(Value: LongInt);
  public
    constructor Create(AOwner: TComponent); override;
    class function GetLanguageName: string; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(NewValue: String; LineNumber: Integer); override;
    function GetToken: String; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property AreaAIdentifierAttri: TSynHighlighterAttributes read fAIdentifierAttri write fAIdentifierAttri;
    property PreprocessorAttri: TSynHighlighterAttributes read fPreprocessorAttri write fPreprocessorAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri write fNumberAttri;
    property BooleanAttri: TSynHighlighterAttributes read fBooleanAttri write fBooleanAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
    property SequenceAttri: TSynHighlighterAttributes read fSequenceAttri write fSequenceAttri;
    property IndicatorAttri: TSynHighlighterAttributes read fIndicatorAttri write fIndicatorAttri;
    property TagAreaAttri: TSynHighlighterAttributes read fTagAreaAttri write fTagAreaAttri;
    property DebugLinesAttri: TSynHighlighterAttributes read fDebugLinesAttri write fDebugLinesAttri;

    property AreaAStartPos: LongInt read fCodeStartPos write SetCodeStartPos;
    property AreaBStartPos: LongInt read fCodeMediumPos write SetCodeMediumPos;
    property CodeEndPos: LongInt read fCodeEndPos write SetCodeEndPos;
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

const
  StringChars: array[TRangeState] of Char = (#0, '"', '''', '=',  '"', '''');

procedure MakeIdentTable;
var
  I, J: Char;
begin
  for I := #0 to #255 do
  begin
    case I of
      '-', '0'..'9', 'a'..'z', 'A'..'Z', '.': Identifiers[I] := True;
    else
      Identifiers[I] := False;
    end;
    J := UpCase(I);
    case I in ['-', 'A'..'Z', 'a'..'z'] of
      True: mHashTable[I] := Ord(J) - 64
    else
      mHashTable[I] := 0;
    end;
  end;
end;

procedure TSynCobolSyn.InitIdent;
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
  fIdentFuncTable[2] := Func2;
  fIdentFuncTable[5] := Func5;
  fIdentFuncTable[6] := Func6;
  fIdentFuncTable[7] := Func7;
  fIdentFuncTable[9] := Func9;
  fIdentFuncTable[10] := Func10;
  fIdentFuncTable[11] := Func11;
  fIdentFuncTable[13] := Func13;
  fIdentFuncTable[15] := Func15;
  fIdentFuncTable[16] := Func16;
  fIdentFuncTable[17] := Func17;
  fIdentFuncTable[19] := Func19;
  fIdentFuncTable[21] := Func21;
  fIdentFuncTable[22] := Func22;
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
  fIdentFuncTable[55] := Func55;
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
  fIdentFuncTable[91] := Func91;
  fIdentFuncTable[92] := Func92;
  fIdentFuncTable[93] := Func93;
  fIdentFuncTable[94] := Func94;
  fIdentFuncTable[95] := Func95;
  fIdentFuncTable[96] := Func96;
  fIdentFuncTable[97] := Func97;
  fIdentFuncTable[98] := Func98;
  fIdentFuncTable[99] := Func99;
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
  fIdentFuncTable[112] := Func112;
  fIdentFuncTable[113] := Func113;
  fIdentFuncTable[114] := Func114;
  fIdentFuncTable[115] := Func115;
  fIdentFuncTable[116] := Func116;
  fIdentFuncTable[117] := Func117;
  fIdentFuncTable[119] := Func119;
  fIdentFuncTable[120] := Func120;
  fIdentFuncTable[121] := Func121;
  fIdentFuncTable[122] := Func122;
  fIdentFuncTable[123] := Func123;
  fIdentFuncTable[124] := Func124;
  fIdentFuncTable[125] := Func125;
  fIdentFuncTable[126] := Func126;
  fIdentFuncTable[127] := Func127;
  fIdentFuncTable[128] := Func128;
  fIdentFuncTable[130] := Func130;
  fIdentFuncTable[131] := Func131;
  fIdentFuncTable[132] := Func132;
  fIdentFuncTable[133] := Func133;
  fIdentFuncTable[134] := Func134;
  fIdentFuncTable[137] := Func137;
  fIdentFuncTable[138] := Func138;
  fIdentFuncTable[140] := Func140;
  fIdentFuncTable[141] := Func141;
  fIdentFuncTable[146] := Func146;
  fIdentFuncTable[147] := Func147;
  fIdentFuncTable[149] := Func149;
  fIdentFuncTable[150] := Func150;
  fIdentFuncTable[152] := Func152;
  fIdentFuncTable[153] := Func153;
  fIdentFuncTable[157] := Func157;
  fIdentFuncTable[160] := Func160;
  fIdentFuncTable[161] := Func161;
  fIdentFuncTable[163] := Func163;
  fIdentFuncTable[165] := Func165;
  fIdentFuncTable[167] := Func167;
  fIdentFuncTable[173] := Func173;
  fIdentFuncTable[174] := Func174;
  fIdentFuncTable[188] := Func188;
  fIdentFuncTable[208] := Func208;
end;

function TSynCobolSyn.KeyHash(ToHash: PChar): Integer;
var
  fRun: LongInt;
begin
  fRun := Run;
  Result := 0;

  if (ToHash^ in ['a'..'z', 'A'..'Z']) and (fRun <= fCodeEndPos) then
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
    inc(fRun);

    while (ToHash^ in ['-', '0'..'9', 'a'..'z', 'A'..'Z']) and (fRun <= fCodeEndPos) do
    begin
      inc(Result, mHashTable[ToHash^]);
      inc(ToHash);
      inc(fRun);
    end;
  end;

  fStringLen := ToHash - fToIdent;
end;

function TSynCobolSyn.KeyComp(const aKey: String): Boolean;
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

function TSynCobolSyn.Func2: TtkTokenKind;
begin
  if KeyComp('B-AND') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func5: TtkTokenKind;
begin
  if KeyComp('I-O') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func6: TtkTokenKind;
begin
  if KeyComp('DB') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func7: TtkTokenKind;
begin
  if KeyComp('CD') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func9: TtkTokenKind;
begin
  if KeyComp('DE') then Result := tkKey else
    if KeyComp('ADD') then Result := tkKey else
      if KeyComp('CF') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func10: TtkTokenKind;
begin
  if KeyComp('FD') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func11: TtkTokenKind;
begin
  if KeyComp('CH') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func13: TtkTokenKind;
begin
  if KeyComp('END-ADD') then Result := tkKey else
    if KeyComp('ID') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func15: TtkTokenKind;
begin
  if KeyComp('IF') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func16: TtkTokenKind;
begin
  if KeyComp('LD') then Result := tkKey else
    if KeyComp('B-OR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func17: TtkTokenKind;
begin
  if KeyComp('CBL') then Result := tkPreprocessor else Result := tkIdentifier;
end;

function TSynCobolSyn.Func19: TtkTokenKind;
begin
  if KeyComp('END-IF') then Result := tkKey else
    if KeyComp('AND') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func21: TtkTokenKind;
begin
  if KeyComp('OF') then Result := tkKey else
    if KeyComp('AT') then Result := tkKey else
      if KeyComp('EGI') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func22: TtkTokenKind;
begin
  if KeyComp('RD') then Result := tkKey else
    if KeyComp('GO') then Result := tkKey else
      if KeyComp('PF') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func23: TtkTokenKind;
begin
  if KeyComp('IN') then Result := tkKey else
    if KeyComp('END') then Result := tkKey else
      if KeyComp('SD') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func24: TtkTokenKind;
begin
  if KeyComp('RF') then Result := tkKey else
    if KeyComp('PH') then Result := tkKey else
      if KeyComp('ARE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func25: TtkTokenKind;
begin
  if KeyComp('AREA') then Result := tkKey else
    if KeyComp('ALL') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func26: TtkTokenKind;
begin
  if KeyComp('RH') then Result := tkKey else
    if KeyComp('FILE-ID') then Result := tkKey else
      if KeyComp('DATA') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func27: TtkTokenKind;
begin
  if KeyComp('BY') then Result := tkKey else
    if KeyComp('DB-DATA-NAME') then Result := tkKey else
      if KeyComp('EMI') then Result := tkKey else
        if KeyComp('OFF') then Result := tkKey else
          if KeyComp('CODE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func28: TtkTokenKind;
begin
  if KeyComp('COMP-9') then Result := tkKey else
    if KeyComp('COMP-8') then Result := tkKey else
      if KeyComp('COMP-7') then Result := tkKey else
        if KeyComp('DBCS') then Result := tkKey else
          if KeyComp('COMP-1') then Result := tkKey else
            if KeyComp('COMP-2') then Result := tkKey else
              if KeyComp('COMP-0') then Result := tkKey else
                if KeyComp('COMP-6') then Result := tkKey else
                  if KeyComp('COMP-5') then Result := tkKey else
                    if KeyComp('PIC') then Result := tkKey else
                      if KeyComp('BEEP') then Result := tkKey else
                        if KeyComp('CALL') then Result := tkKey else
                          if KeyComp('READ') then Result := tkKey else
                            if KeyComp('COMP-4') then Result := tkKey else
                              if KeyComp('COMP-3') then Result := tkKey else
                                if KeyComp('IS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func29: TtkTokenKind;
begin
  if KeyComp('PAGE') then Result := tkKey else
    if KeyComp('NO') then Result := tkKey else
      if KeyComp('ON') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func30: TtkTokenKind;
begin
  if KeyComp('COL') then Result := tkKey else
    if KeyComp('DATE') then Result := tkKey else
      if KeyComp('DAY') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func31: TtkTokenKind;
begin
  if KeyComp('BELL') then Result := tkKey else
    if KeyComp('BIT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func32: TtkTokenKind;
var
  I: Integer;
begin
  if KeyComp('END-READ') then Result := tkKey else
    if KeyComp('B-NOT') then Result := tkKey else
      if KeyComp('FILE') then Result := tkKey else
        if KeyComp('GET') then Result := tkKey else
          if KeyComp('END-CALL') then Result := tkKey else
            if KeyComp('LABEL') then
            begin
              I := Run + Length('LABEL');
              while fLine[I] = ' ' do
                Inc(I);
              if (StrLIComp( PChar(@fLine[I]), 'RECORD', Length('RECORD')) = 0)
                and (I + Length('RECORD') - 1 <= fCodeEndPos) then
                  Result := tkKey
                else
                  Result := tkPreprocessor;
            end
            else
              Result := tkIdentifier;
end;

function TSynCobolSyn.Func33: TtkTokenKind;
begin
  if KeyComp('FIND') then Result := tkKey else
    if KeyComp('OR') then Result := tkKey else
      if KeyComp('NAME') then Result := tkKey else
        if KeyComp('ESI') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func34: TtkTokenKind;
begin
  if KeyComp('EGCS') then Result := tkKey else
    if KeyComp('FREE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func35: TtkTokenKind;
begin
  if KeyComp('TO') then Result := tkKey else
    if KeyComp('CHAIN') then Result := tkKey else
      if KeyComp('END-OF-PAGE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func36: TtkTokenKind;
begin
  if KeyComp('EOP') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func37: TtkTokenKind;
begin
  if KeyComp('INDEX-5') then Result := tkKey else
    if KeyComp('UP') then Result := tkKey else
      if KeyComp('EXEC') then Result := tkKey else
        if KeyComp('INDEX-4') then Result := tkKey else
          if KeyComp('INDEX-3') then Result := tkKey else
            if KeyComp('INDEX-6') then Result := tkKey else
              if KeyComp('INDEX-9') then Result := tkKey else
                if KeyComp('INDEX-8') then Result := tkKey else
                  if KeyComp('INDEX-7') then Result := tkKey else
                    if KeyComp('INDEX-1') then Result := tkKey else
                      if KeyComp('KEEP') then Result := tkKey else
                        if KeyComp('MODE') then Result := tkKey else
                          if KeyComp('INDEX-2') then Result := tkKey else
                            if KeyComp('LIKE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func38: TtkTokenKind;
begin
  if KeyComp('CANCEL') then Result := tkKey else
    if KeyComp('SAME') then Result := tkKey else
      if KeyComp('B-LESS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func39: TtkTokenKind;
begin
  if KeyComp('ENABLE') then Result := tkKey else
    if KeyComp('FOR') then Result := tkKey else
      if KeyComp('INDIC') then Result := tkKey else
        if KeyComp('GOBACK') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func40: TtkTokenKind;
begin
  if KeyComp('ANY') then Result := tkKey else
    if KeyComp('LINE') then Result := tkKey else
      if KeyComp('BLANK') then Result := tkKey else
        if KeyComp('TABLE') then Result := tkKey else
          if KeyComp('REEL') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func41: TtkTokenKind;
begin
  if KeyComp('ELSE') then Result := tkKey else
    if KeyComp('LOCK') then Result := tkKey else
      if KeyComp('KEY') then Result := tkKey else
        if KeyComp('NO-ECHO') then Result := tkKey else
          if KeyComp('CRT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func42: TtkTokenKind;
begin
  if KeyComp('TAPE') then Result := tkKey else
    if KeyComp('SEND') then Result := tkKey else
      if KeyComp('FINAL') then Result := tkKey else
        if KeyComp('COM-REG') then Result := tkKey else
          if KeyComp('FETCH') then Result := tkKey else
            if KeyComp('CHANGED') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func43: TtkTokenKind;
begin
  if KeyComp('EJECT') then Result := tkPreprocessor else
    if KeyComp('BLOCK') then Result := tkKey else
      if KeyComp('DEBUG-SUB-3') then Result := tkKey else
        if KeyComp('FALSE') then Result := tkBoolean else
          if KeyComp('LEFT') then Result := tkKey else
            if KeyComp('DISK') then Result := tkKey else
              if KeyComp('DEBUG-SUB-2') then Result := tkKey else
                if KeyComp('DEBUG-SUB-1') then Result := tkKey else
                  if KeyComp('THAN') then Result := tkKey else
                    if KeyComp('END-ENABLE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func44: TtkTokenKind;
begin
  if KeyComp('AREAS') then Result := tkKey else
    if KeyComp('SPACE') then Result := tkString else
      if KeyComp('SET') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func45: TtkTokenKind;
begin
  if KeyComp('RANGE') then Result := tkKey else
    if KeyComp('DB-SET-NAME') then Result := tkKey else
      if KeyComp('USE') then Result := tkPreprocessor else
        if KeyComp('KANJI') then Result := tkKey else
          if KeyComp('COMMA') then Result := tkKey else
            if KeyComp('B-EXOR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func46: TtkTokenKind;
begin
  if KeyComp('END-SEND') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func47: TtkTokenKind;
begin
  if KeyComp('THEN') then Result := tkKey else
    if KeyComp('ALSO') then Result := tkKey else
      if KeyComp('COMP') then Result := tkKey else
        if KeyComp('TIME') then Result := tkKey else
          if KeyComp('COBOL') then Result := tkKey else
            if KeyComp('TRACE') then Result := tkPreprocessor else Result := tkIdentifier;
end;

function TSynCobolSyn.Func48: TtkTokenKind;
begin
  if KeyComp('HEADING') then Result := tkKey else
    if KeyComp('LINAGE') then Result := tkKey else
      if KeyComp('VALID') then Result := tkKey else
        if KeyComp('BLINK') then Result := tkKey else
          if KeyComp('MERGE') then Result := tkKey else
            if KeyComp('FIXED') then Result := tkKey else
              if KeyComp('NONE') then Result := tkKey else
                if KeyComp('ACCEPT') then Result := tkKey else
                  if KeyComp('ERASE') then Result := tkKey else
                    if KeyComp('CYCLE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func49: TtkTokenKind;
begin
  if KeyComp('GLOBAL') then Result := tkKey else
    if KeyComp('NOT') then Result := tkKey else
      if KeyComp('SIGN') then Result := tkKey else
        if KeyComp('ESCAPE') then Result := tkKey else
          if KeyComp('REALM') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func50: TtkTokenKind;
begin
  if KeyComp('AFTER') then Result := tkKey else
    if KeyComp('WHEN') then Result := tkKey else
      if KeyComp('BASIS') then Result := tkPreprocessor else
        if KeyComp('OPEN') then Result := tkKey else
          if KeyComp('BITS') then Result := tkKey else
            if KeyComp('ACCESS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func51: TtkTokenKind;
begin
  if KeyComp('DETAIL') then Result := tkKey else
    if KeyComp('TOP') then Result := tkKey else
      if KeyComp('BEFORE') then Result := tkKey else
        if KeyComp('DELETE') then Result := tkPreprocessor else
          if KeyComp('FULL') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func52: TtkTokenKind;
begin
  if KeyComp('LAST') then Result := tkKey else
    if KeyComp('KEPT') then Result := tkKey else
      if KeyComp('FORM') then Result := tkKey else
        if KeyComp('COMP-X') then Result := tkKey else
          if KeyComp('FROM') then Result := tkKey else
            if KeyComp('END-ACCEPT') then Result := tkKey else
              if KeyComp('DISABLE') then Result := tkKey else
                if KeyComp('CODE-SET') then Result := tkKey else
                  if KeyComp('LEADING') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func53: TtkTokenKind;
begin
  if KeyComp('EXACT') then Result := tkKey else
    if KeyComp('WAIT') then Result := tkKey else
      if KeyComp('SUM') then Result := tkKey else
        if KeyComp('USAGE') then Result := tkKey else
          if KeyComp('READY') then Result := tkPreprocessor else
            if KeyComp('DROP') then Result := tkKey else
              if KeyComp('ENDING') then Result := tkKey else
                if KeyComp('DEBUG-NAME') then Result := tkKey else
                  if KeyComp('RUN') then Result := tkKey else
                    if KeyComp('DIVIDE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func54: TtkTokenKind;
begin
  if KeyComp('CORR') then Result := tkKey else
    if KeyComp('CLOSE') then Result := tkKey else
      if KeyComp('SEARCH') then Result := tkKey else
        if KeyComp('CLASS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func55: TtkTokenKind;
begin
  if KeyComp('SKIP1') then Result := tkPreprocessor else
    if KeyComp('SKIP3') then Result := tkPreprocessor else
      if KeyComp('SKIP2') then Result := tkPreprocessor else
        if KeyComp('END-DELETE') then Result := tkKey else
          if KeyComp('SHARED') then Result := tkKey else
            if KeyComp('PADDING') then Result := tkKey else
              if KeyComp('MOVE') then Result := tkKey else
                if KeyComp('RELOAD') then Result := tkPreprocessor else
                  if KeyComp('LESS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func56: TtkTokenKind;
begin
  if KeyComp('INDEX') then Result := tkKey else
    if KeyComp('EQUAL') then Result := tkKey else
      if KeyComp('MEMBER') then Result := tkKey else
        if KeyComp('ALTER') then Result := tkKey else
          if KeyComp('END-DISABLE') then Result := tkKey else
            if KeyComp('EXCESS-3') then Result := tkKey else
              if KeyComp('DOWN') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func57: TtkTokenKind;
begin
  if KeyComp('END-DIVIDE') then Result := tkKey else
    if KeyComp('DAY-OF-WEEK') then Result := tkKey else
      if KeyComp('AUTO') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func58: TtkTokenKind;
begin
  if KeyComp('EXIT') then Result := tkKey else
    if KeyComp('END-SEARCH') then Result := tkKey else
      if KeyComp('INTO') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func59: TtkTokenKind;
begin
  if KeyComp('LINES') then Result := tkKey else
    if KeyComp('LINKAGE') then Result := tkKey else
      if KeyComp('NULL') then Result := tkString else
        if KeyComp('SIZE') then Result := tkKey else
          if KeyComp('COPY') then Result := tkPreprocessor else Result := tkIdentifier;
end;

function TSynCobolSyn.Func60: TtkTokenKind;
begin
  if KeyComp('DEBUG-LINE') then Result := tkKey else
    if KeyComp('WITH') then Result := tkKey else
      if KeyComp('ORDER') then Result := tkKey else
        if KeyComp('REPLACE') then Result := tkPreprocessor else Result := tkIdentifier;
end;

function TSynCobolSyn.Func61: TtkTokenKind;
begin
  if KeyComp('VALUE') then Result := tkKey else
    if KeyComp('SYNC') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func62: TtkTokenKind;
begin
  if KeyComp('FILLER') then Result := tkKey else
    if KeyComp('MANUAL') then Result := tkKey else
      if KeyComp('STANDARD-1') then Result := tkKey else
        if KeyComp('RIGHT') then Result := tkKey else
          if KeyComp('STANDARD-4') then Result := tkKey else
            if KeyComp('STANDARD-2') then Result := tkKey else
              if KeyComp('STANDARD-3') then Result := tkKey else
                if KeyComp('ENTER') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func63: TtkTokenKind;
begin
  if KeyComp('SWITCH-4') then Result := tkKey else
    if KeyComp('SWITCH-5') then Result := tkKey else
      if KeyComp('SWITCH-2') then Result := tkKey else
        if KeyComp('SWITCH-8') then Result := tkKey else
          if KeyComp('SWITCH-1') then Result := tkKey else
            if KeyComp('LIMIT') then Result := tkKey else
              if KeyComp('RECORD') then Result := tkKey else
                if KeyComp('NEXT') then Result := tkKey else
                  if KeyComp('SPACES') then Result := tkString else
                    if KeyComp('COLOR') then Result := tkKey else
                      if KeyComp('SWITCH-6') then Result := tkKey else
                        if KeyComp('BACKWARD') then Result := tkKey else
                          if KeyComp('SWITCH-3') then Result := tkKey else
                            if KeyComp('USER') then Result := tkKey else
                              if KeyComp('SWITCH-7') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func64: TtkTokenKind;
begin
  if KeyComp('DB-RECORD-NAME') then Result := tkKey else
    if KeyComp('BOOLEAN') then Result := tkKey else
      if KeyComp('TRUE') then Result := tkBoolean else
        if KeyComp('TEST') then Result := tkKey else
          if KeyComp('UNIT') then Result := tkKey else
            if KeyComp('ZERO') then Result := tkString else
              if KeyComp('SPACE-FILL') then Result := tkKey else
                if KeyComp('SELECT') then Result := tkKey else
                  if KeyComp('SCREEN') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func65: TtkTokenKind;
begin
  if KeyComp('ALPHABET') then Result := tkKey else
    if KeyComp('RANDOM') then Result := tkKey else
      if KeyComp('EXCEEDS') then Result := tkKey else
        if KeyComp('RELEASE') then Result := tkKey else
          if KeyComp('INDEXED') then Result := tkKey else
            if KeyComp('MODIFIED') then Result := tkKey else
              if KeyComp('INDICATE') then Result := tkKey else
                if KeyComp('CHAINING') then Result := tkKey else
                  if KeyComp('FINISH') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func66: TtkTokenKind;
begin
  if KeyComp('TITLE') then Result := tkPreprocessor else
    if KeyComp('SHIFT-IN') then Result := tkKey else
      if KeyComp('LENGTH') then Result := tkKey else
        if KeyComp('UPON') then Result := tkKey else
          if KeyComp('OTHER') then Result := tkKey else
            if KeyComp('ONLY') then Result := tkKey else
              if KeyComp('PROCEED') then Result := tkKey else
                if KeyComp('TYPE') then Result := tkKey else
                  if KeyComp('TIMES') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func67: TtkTokenKind;
begin
  if KeyComp('THRU') then Result := tkKey else
    if KeyComp('RESET') then Result := tkPreprocessor else
      if KeyComp('DISPLAY-7') then Result := tkKey else
        if KeyComp('AREA-VALUE') then Result := tkKey else
          if KeyComp('PURGE') then Result := tkKey else
            if KeyComp('UPDATE') then Result := tkKey else
              if KeyComp('RECEIVE') then Result := tkKey else
                if KeyComp('DISPLAY-5') then Result := tkKey else
                  if KeyComp('DISPLAY-1') then Result := tkKey else
                    if KeyComp('DISPLAY-4') then Result := tkKey else
                      if KeyComp('DEBUG-ITEM') then Result := tkKey else
                        if KeyComp('DISPLAY-2') then Result := tkKey else
                          if KeyComp('DISPLAY-6') then Result := tkKey else
                            if KeyComp('DISPLAY-9') then Result := tkKey else
                              if KeyComp('DISPLAY-3') then Result := tkKey else
                                if KeyComp('DISPLAY-8') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func68: TtkTokenKind;
begin
  if KeyComp('GIVING') then Result := tkKey else
    if KeyComp('PACKED-DECIMAL') then Result := tkKey else
      if KeyComp('PLUS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func69: TtkTokenKind;
begin
  if KeyComp('ASSIGN') then Result := tkKey else
    if KeyComp('BINARY') then Result := tkKey else
      if KeyComp('DEFAULT') then Result := tkKey else
        if KeyComp('DYNAMIC') then Result := tkKey else
          if KeyComp('QUEUE') then Result := tkKey else
            if KeyComp('TEXT') then Result := tkKey else
              if KeyComp('MESSAGE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func70: TtkTokenKind;
begin
  if KeyComp('USING') then Result := tkKey else
    if KeyComp('JUST') then Result := tkKey else
      if KeyComp('TALLY') then Result := tkKey else
        if KeyComp('ADDRESS') then Result := tkKey else
          if KeyComp('APPLY') then Result := tkKey else
            if KeyComp('VARIABLE') then Result := tkKey else
              if KeyComp('STOP') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func71: TtkTokenKind;
begin
  if KeyComp('INVALID') then Result := tkKey else
    if KeyComp('END-RECEIVE') then Result := tkKey else
      if KeyComp('NATIVE') then Result := tkKey else
        if KeyComp('SECURE') then Result := tkKey else
          if KeyComp('USAGE-MODE') then Result := tkKey else
            if KeyComp('JAPANESE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func72: TtkTokenKind;
begin
  if KeyComp('EXTEND') then Result := tkKey else
    if KeyComp('MODIFY') then Result := tkKey else
      if KeyComp('SORT') then Result := tkKey else
        if KeyComp('SUB-SCHEMA') then Result := tkKey else
          if KeyComp('FIRST') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func73: TtkTokenKind;
begin
  if KeyComp('COMMON') then Result := tkKey else
    if KeyComp('COUNT') then Result := tkKey else
      if KeyComp('SUB-QUEUE-3') then Result := tkKey else
        if KeyComp('NORMAL') then Result := tkKey else
          if KeyComp('COMMIT') then Result := tkKey else
            if KeyComp('NUMBER') then Result := tkKey else
              if KeyComp('SUB-QUEUE-1') then Result := tkKey else
                if KeyComp('SUB-QUEUE-2') then Result := tkKey else
                  if KeyComp('REWIND') then Result := tkKey else
                    if KeyComp('FORMAT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func74: TtkTokenKind;
begin
  if KeyComp('ACQUIRE') then Result := tkKey else
    if KeyComp('CONNECT') then Result := tkKey else
      if KeyComp('DB-FORMAT-NAME') then Result := tkKey else
        if KeyComp('TENANT') then Result := tkKey else
          if KeyComp('GREATER') then Result := tkKey else
            if KeyComp('HIGH-VALUE') then Result := tkString else
              if KeyComp('ERROR') then Result := tkKey else
                if KeyComp('VALIDATE') then Result := tkKey else
                  if KeyComp('SUBFILE') then Result := tkKey else
                    if KeyComp('REPEATED') then Result := tkKey else
                      if KeyComp('ROLLBACK') then Result := tkKey else
                        if KeyComp('INITIAL') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func75: TtkTokenKind;
begin
  if KeyComp('OWNER') then Result := tkKey else
    if KeyComp('WRITE') then Result := tkKey else
      if KeyComp('GENERATE') then Result := tkKey else
        if KeyComp('EVERY') then Result := tkKey else
          if KeyComp('EQUALS') then Result := tkKey else
            if KeyComp('ADVANCING') then Result := tkKey else
              if KeyComp('RENAMES') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func76: TtkTokenKind;
begin
  if KeyComp('ASCENDING') then Result := tkKey else
    if KeyComp('UNTIL') then Result := tkKey else
      if KeyComp('RERUN') then Result := tkKey else
        if KeyComp('UNLOCK') then Result := tkKey else
          if KeyComp('PRIOR') then Result := tkKey else
            if KeyComp('DEBUGGING') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func77: TtkTokenKind;
begin
  if KeyComp('GROUP') then Result := tkKey else
    if KeyComp('CHARACTER') then Result := tkKey else
      if KeyComp('EXHIBIT') then Result := tkKey else
        if KeyComp('LENGTH-CHECK') then Result := tkKey else
          if KeyComp('RECORD-NAME') then Result := tkKey else
            if KeyComp('STORE') then Result := tkKey else
              if KeyComp('ALPHABETIC') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func78: TtkTokenKind;
begin
  if KeyComp('DEPENDING') then Result := tkKey else
    if KeyComp('START') then Result := tkKey else
      if KeyComp('NULLS') then Result := tkString else
        if KeyComp('QUOTE') then Result := tkString else
          if KeyComp('COLUMN') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func79: TtkTokenKind;
begin
  if KeyComp('WORDS') then Result := tkKey else
    if KeyComp('EMPTY') then Result := tkKey else
      if KeyComp('PALETTE') then Result := tkKey else
        if KeyComp('REFERENCE') then Result := tkKey else
          if KeyComp('OCCURS') then Result := tkKey else
            if KeyComp('END-WRITE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func80: TtkTokenKind;
begin
  if KeyComp('VALUES') then Result := tkKey else
    if KeyComp('INPUT') then Result := tkKey else
      if KeyComp('LOCALLY') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func81: TtkTokenKind;
begin
  if KeyComp('PRINTER-1') then Result := tkKey else
    if KeyComp('SOURCE') then Result := tkKey else
      if KeyComp('ROUNDED') then Result := tkKey else
        if KeyComp('BEGINNING') then Result := tkKey else
          if KeyComp('SERVICE') then Result := tkPreprocessor else
            if KeyComp('DELIMITED') then Result := tkKey else
              if KeyComp('STANDARD') then Result := tkKey else
                if KeyComp('KEYBOARD') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func82: TtkTokenKind;
begin
  if KeyComp('ENTRY') then Result := tkKey else
    if KeyComp('RECORDS') then Result := tkKey else
      if KeyComp('LIMITS') then Result := tkKey else
        if KeyComp('PROGRAM-ID') then Result := tkKey else
          if KeyComp('END-START') then Result := tkKey else
            if KeyComp('SWITCH') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func83: TtkTokenKind;
begin
  if KeyComp('MORE-LABELS') then Result := tkKey else
    if KeyComp('WITHIN') then Result := tkKey else
      if KeyComp('NEGATIVE') then Result := tkKey else
        if KeyComp('I-O-CONTROL') then Result := tkKey else
          if KeyComp('NUMERIC') then Result := tkKey else
            if KeyComp('ZEROS') then Result := tkString else
              if KeyComp('AUTHOR') then Result := tkKey else
                if KeyComp('CONSOLE') then Result := tkKey else
                  if KeyComp('SEGMENT') then Result := tkKey else
                    if KeyComp('EXECUTE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func84: TtkTokenKind;
begin
  if KeyComp('COMMAND-LINE') then Result := tkKey else
    if KeyComp('DESCENDING') then Result := tkKey else
      if KeyComp('ZERO-FILL') then Result := tkKey else
        if KeyComp('CRT-UNDER') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func85: TtkTokenKind;
begin
  if KeyComp('SENTENCE') then Result := tkKey else
    if KeyComp('CONTAINED') then Result := tkKey else
      if KeyComp('REDEFINES') then Result := tkKey else
        if KeyComp('BOTTOM') then Result := tkKey else
          if KeyComp('REPLACING') then Result := tkKey else
            if KeyComp('SECTION') then Result := tkKey else
              if KeyComp('INSERT') then Result := tkPreprocessor else
                if KeyComp('SEPARATE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func86: TtkTokenKind;
begin
  if KeyComp('PARAGRAPH') then Result := tkKey else
    if KeyComp('FOOTING') then Result := tkKey else
      if KeyComp('INSPECT') then Result := tkKey else
        if KeyComp('REMOVAL') then Result := tkKey else
          if KeyComp('DISPLAY') then Result := tkKey else
            if KeyComp('OMITTED') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func87: TtkTokenKind;
begin
  if KeyComp('DB-STATUS') then Result := tkKey else
    if KeyComp('ROLLING') then Result := tkKey else
      if KeyComp('INITIATE') then Result := tkKey else
        if KeyComp('EVALUATE') then Result := tkKey else
          if KeyComp('STRING') then Result := tkKey else
            if KeyComp('REMAINDER') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func88: TtkTokenKind;
begin
  if KeyComp('PROGRAM') then Result := tkKey else
    if KeyComp('DATE-COMPILED') then Result := tkKey else
      if KeyComp('HIGHLIGHT') then Result := tkKey else
        if KeyComp('ZEROES') then Result := tkString else Result := tkIdentifier;
end;

function TSynCobolSyn.Func89: TtkTokenKind;
begin
  if KeyComp('MODULES') then Result := tkKey else
    if KeyComp('MEMORY') then Result := tkKey else
      if KeyComp('SEQUENCE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func90: TtkTokenKind;
begin
  if KeyComp('TRAILING') then Result := tkKey else
    if KeyComp('EMPTY-CHECK') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func91: TtkTokenKind;
begin
  if KeyComp('CONTENT') then Result := tkKey else
    if KeyComp('END-EVALUATE') then Result := tkKey else
      if KeyComp('UNEQUAL') then Result := tkKey else
        if KeyComp('END-STRING') then Result := tkKey else
          if KeyComp('PERFORM') then Result := tkKey else
            if KeyComp('DUPLICATE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func92: TtkTokenKind;
begin
  if KeyComp('PICTURE') then Result := tkKey else
    if KeyComp('RELATIVE') then Result := tkKey else
      if KeyComp('TERMINAL') then Result := tkKey else
        if KeyComp('REPORT') then Result := tkKey else
          if KeyComp('LOW-VALUE') then Result := tkString else
            if KeyComp('RESERVE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func93: TtkTokenKind;
begin
  if KeyComp('ALLOWING') then Result := tkKey else
    if KeyComp('HIGH-VALUES') then Result := tkString else
      if KeyComp('INDICATOR') then Result := tkKey else
        if KeyComp('AUTO-SKIP') then Result := tkKey else
          if KeyComp('COLLATING') then Result := tkKey else
            if KeyComp('COMPUTE') then Result := tkKey else
              if KeyComp('RECORDING') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func94: TtkTokenKind;
begin
  if KeyComp('RELATION') then Result := tkKey else
    if KeyComp('SESSION-ID') then Result := tkKey else
      if KeyComp('CURSOR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func95: TtkTokenKind;
begin
  if KeyComp('CONTAINS') then Result := tkKey else
    if KeyComp('END-PERFORM') then Result := tkKey else
      if KeyComp('DELIMITER') then Result := tkKey else
        if KeyComp('PROCESS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func96: TtkTokenKind;
begin
  if KeyComp('CHARACTERS') then Result := tkKey else
    if KeyComp('VARYING') then Result := tkKey else
      if KeyComp('RETURN') then Result := tkKey else
        if KeyComp('ALTERNATE') then Result := tkKey else
          if KeyComp('REVERSED') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func97: TtkTokenKind;
begin
  if KeyComp('THROUGH') then Result := tkKey else
    if KeyComp('REQUIRED') then Result := tkKey else
      if KeyComp('CONTROL') then Result := tkPreprocessor else
        if KeyComp('RETAINING') then Result := tkKey else
          if KeyComp('POINTER') then Result := tkKey else
            if KeyComp('PRESENT') then Result := tkKey else
              if KeyComp('QUOTES') then Result := tkString else
                if KeyComp('RECONNECT') then Result := tkKey else
                  if KeyComp('END-COMPUTE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func98: TtkTokenKind;
begin
  if KeyComp('DB-EXCEPTION') then Result := tkKey else
    if KeyComp('REFERENCES') then Result := tkKey else
      if KeyComp('REWRITE') then Result := tkKey else
        if KeyComp('SPECIAL-NAMES') then Result := tkKey else
          if KeyComp('SYMBOLIC') then Result := tkKey else
            if KeyComp('PROMPT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func99: TtkTokenKind;
begin
  if KeyComp('CURRENT') then Result := tkKey else
    if KeyComp('SHIFT-OUT') then Result := tkKey else
      if KeyComp('EXTERNAL') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func100: TtkTokenKind;
begin
  if KeyComp('TALLYING') then Result := tkKey else
    if KeyComp('STATUS') then Result := tkKey else
      if KeyComp('PRINTER') then Result := tkKey else
        if KeyComp('END-RETURN') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func101: TtkTokenKind;
begin
  if KeyComp('DIVISION') then Result := tkKey else
    if KeyComp('CONTINUE') then Result := tkKey else
      if KeyComp('SORT-MERGE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func102: TtkTokenKind;
begin
  if KeyComp('END-REWRITE') then Result := tkKey else
    if KeyComp('FUNCTION') then Result := tkKey else
      if KeyComp('OPTIONAL') then Result := tkKey else
        if KeyComp('DECIMAL-POINT') then Result := tkKey else
          if KeyComp('UNDERLINE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func103: TtkTokenKind;
begin
  if KeyComp('CONTROL-AREA') then Result := tkKey else
    if KeyComp('JUSTIFIED') then Result := tkKey else
      if KeyComp('TIMEOUT') then Result := tkKey else
        if KeyComp('AUTOMATIC') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func104: TtkTokenKind;
begin
  if KeyComp('SUBTRACT') then Result := tkKey else
    if KeyComp('RETURN-CODE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func105: TtkTokenKind;
begin
  if KeyComp('TERMINATE') then Result := tkKey else
    if KeyComp('PROCEDURE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func106: TtkTokenKind;
begin
  if KeyComp('PAGE-COUNTER') then Result := tkKey else
    if KeyComp('ARITHMETIC') then Result := tkKey else
      if KeyComp('DISCONNECT') then Result := tkKey else
        if KeyComp('PROTECTED') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func107: TtkTokenKind;
begin
  if KeyComp('PRINTING') then Result := tkKey else
    if KeyComp('CURRENCY') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func108: TtkTokenKind;
begin
  if KeyComp('END-SUBTRACT') then Result := tkKey else
    if KeyComp('CLOCK-UNITS') then Result := tkKey else
      if KeyComp('STARTING') then Result := tkKey else
        if KeyComp('MULTIPLE') then Result := tkKey else
          if KeyComp('WHEN-COMPILED') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func110: TtkTokenKind;
begin
  if KeyComp('DUPLICATES') then Result := tkKey else
    if KeyComp('RETRIEVAL') then Result := tkKey else
      if KeyComp('FILE-CONTROL') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func111: TtkTokenKind;
begin
  if KeyComp('LOW-VALUES') then Result := tkString else
    if KeyComp('EXCEPTION') then Result := tkKey else
      if KeyComp('REPORTS') then Result := tkKey else
        if KeyComp('NUMERIC-EDITED') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func112: TtkTokenKind;
begin
  if KeyComp('INDICATORS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func113: TtkTokenKind;
begin
  if KeyComp('OUTPUT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func114: TtkTokenKind;
begin
  if KeyComp('INITIALIZE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func115: TtkTokenKind;
begin
  if KeyComp('PASSWORD') then Result := tkKey else
    if KeyComp('POSITIVE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func116: TtkTokenKind;
begin
  if KeyComp('CONTROLS') then Result := tkKey else
    if KeyComp('TRANSCEIVE') then Result := tkKey else
      if KeyComp('OVERFLOW') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func117: TtkTokenKind;
begin
  if KeyComp('POSITION') then Result := tkKey else
    if KeyComp('LINE-COUNTER') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func119: TtkTokenKind;
begin
  if KeyComp('DECLARATIVES') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func120: TtkTokenKind;
begin
  if KeyComp('DATE-WRITTEN') then Result := tkKey else
    if KeyComp('SECURITY') then Result := tkKey else
      if KeyComp('END-TRANSCEIVE') then Result := tkKey else
        if KeyComp('TRAILING-SIGN') then Result := tkKey else
          if KeyComp('EXCLUSIVE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func121: TtkTokenKind;
begin
  if KeyComp('ALPHANUMERIC') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func122: TtkTokenKind;
begin
  if KeyComp('WRITE-ONLY') then Result := tkKey else
    if KeyComp('REPORTING') then Result := tkKey else
      if KeyComp('SORT-MESSAGE') then Result := tkKey else
        if KeyComp('UNSTRING') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func123: TtkTokenKind;
begin
  if KeyComp('SEQUENTIAL') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func124: TtkTokenKind;
begin
  if KeyComp('PROCEDURES') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func125: TtkTokenKind;
begin
  if KeyComp('PREVIOUS') then Result := tkKey else
    if KeyComp('PROCESSING') then Result := tkKey else
      if KeyComp('LINAGE-COUNTER') then Result := tkKey else
        if KeyComp('COMMITMENT') then Result := tkKey else
          if KeyComp('SORT-FILE-SIZE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func126: TtkTokenKind;
begin
  if KeyComp('END-UNSTRING') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func127: TtkTokenKind;
begin
  if KeyComp('SEGMENT-LIMIT') then Result := tkKey else
    if KeyComp('CONVERTING') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func128: TtkTokenKind;
begin
  if KeyComp('REVERSE-VIDEO') then Result := tkKey else
    if KeyComp('MULTIPLY') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func130: TtkTokenKind;
begin
  if KeyComp('SUBPROGRAM') then Result := tkKey else
    if KeyComp('SORT-MODE-SIZE') then Result := tkKey else
      if KeyComp('DEBUG-CONTENTS') then Result := tkKey else
        if KeyComp('DESTINATION') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func131: TtkTokenKind;
begin
  if KeyComp('ALPHABETIC-LOWER') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func132: TtkTokenKind;
begin
  if KeyComp('END-MULTIPLY') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func133: TtkTokenKind;
begin
  if KeyComp('SUPPRESS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func134: TtkTokenKind;
begin
  if KeyComp('TRANSACTION') then Result := tkKey else
    if KeyComp('SORT-CORE-SIZE') then Result := tkKey else
      if KeyComp('LEFT-JUSTIFY') then Result := tkKey else
        if KeyComp('ALPHABETIC-UPPER') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func137: TtkTokenKind;
begin
  if KeyComp('DB-ACCESS-CONTROL-KEY') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func138: TtkTokenKind;
begin
  if KeyComp('IDENTIFICATION') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func140: TtkTokenKind;
begin
  if KeyComp('PRINT-SWITCH') then Result := tkKey else
    if KeyComp('BACKGROUND-COLOR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func141: TtkTokenKind;
begin
  if KeyComp('COMPUTATIONAL-5') then Result := tkKey else
    if KeyComp('COMPUTATIONAL-8') then Result := tkKey else
      if KeyComp('COMPUTATIONAL-0') then Result := tkKey else
        if KeyComp('COMPUTATIONAL-2') then Result := tkKey else
          if KeyComp('COMPUTATIONAL-3') then Result := tkKey else
            if KeyComp('COMPUTATIONAL-6') then Result := tkKey else
              if KeyComp('COMPUTATIONAL-7') then Result := tkKey else
                if KeyComp('COMPUTATIONAL-9') then Result := tkKey else
                  if KeyComp('COMPUTATIONAL-4') then Result := tkKey else
                    if KeyComp('COMPUTATIONAL-1') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func146: TtkTokenKind;
begin
  if KeyComp('INSTALLATION') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func147: TtkTokenKind;
begin
  if KeyComp('OBJECT-COMPUTER') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func149: TtkTokenKind;
begin
  if KeyComp('ALPHANUMERIC-EDITED') then Result := tkKey else
    if KeyComp('ENVIRONMENT') then Result := tkKey else
      if KeyComp('ORGANIZATION') then Result := tkKey else
        if KeyComp('SORT-RETURN') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func150: TtkTokenKind;
begin
  if KeyComp('COMMUNICATION') then Result := tkKey else
    if KeyComp('SORT-CONTROL') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func152: TtkTokenKind;
begin
  if KeyComp('CONFIGURATION') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func153: TtkTokenKind;
begin
  if KeyComp('RIGHT-JUSTIFY') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func157: TtkTokenKind;
begin
  if KeyComp('CORRESPONDING') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func160: TtkTokenKind;
begin
  if KeyComp('SYNCHRONIZED') then Result := tkKey else
    if KeyComp('COMPUTATIONAL') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func161: TtkTokenKind;
begin
  if KeyComp('BACKGROUND-COLOUR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func163: TtkTokenKind;
begin
  if KeyComp('WORKING-STORAGE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func165: TtkTokenKind;
begin
  if KeyComp('COMPUTATIONAL-X') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func167: TtkTokenKind;
begin
  if KeyComp('FOREGROUND-COLOR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func173: TtkTokenKind;
begin
  if KeyComp('SOURCE-COMPUTER') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func174: TtkTokenKind;
begin
  if KeyComp('INPUT-OUTPUT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func188: TtkTokenKind;
begin
  if KeyComp('FOREGROUND-COLOUR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.Func208: TtkTokenKind;
begin
  if KeyComp('EXTERNALLY-DESCRIBED-KEY') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCobolSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynCobolSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if (0 <= HashKey) and (HashKey <= MaxKey) then
    Result := fIdentFuncTable[HashKey]
  else
    Result := tkIdentifier;
end;

procedure TSynCobolSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #0: fProcTable[I] := NullProc;
      #10: fProcTable[I] := LFProc;
      #13: fProcTable[I] := CRProc;
      '"': fProcTable[I] := StringOpenProc;
      '''': fProcTable[I] := StringOpenProc;
      '=': fProcTable[I] := StringOpenProc;
      #1..#9,
      #11,
      #12,
      #14..#32 : fProcTable[I] := SpaceProc;
      '.': fProcTable[I] := PointProc;
      '0'..'9': fProcTable[I] := NumberProc;
      'A'..'Z', 'a'..'z': fProcTable[I] := IdentProc;
    else
      fProcTable[I] := UnknownProc;
    end;
end;

procedure TSynCobolSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    inc(Run);
  until not (fLine[Run] in [#1..#32]);
end;

procedure TSynCobolSyn.FirstCharsProc;
var
  I: Integer;
begin
  if fLine[Run] in [#0, #10, #13] then
    fProcTable[fLine[Run]]
  else
    if Run < fCodeStartPos - 1 then
    begin
      fTokenID := tkSequence;
      repeat
        inc(Run);
      until (Run = fCodeStartPos - 1) or (fLine[Run] in [#0, #10, #13]);
    end
    else
    begin
      fTokenID := tkIndicator;
      case fLine[Run] of
        '*', '/', 'D': fIndicator := fLine[Run];
        '-': if fRange in [rsQuoteStringMayBe, rsApostStringMayBe] then
             begin
               I := Run + 1;
               while fLine[I] = ' ' do
                 Inc(I);
               if (StrLIComp( PChar(@fLine[I]), PChar(StringOfChar(StringChars[fRange], 2)), 2) <> 0)
                 or (I + 1 > fCodeEndPos) then
                   fRange := rsUnknown;
             end;
      end;
      inc(Run);
    end;
end;

procedure TSynCobolSyn.LastCharsProc;
begin
  if fLine[Run] in [#0, #10, #13] then
    fProcTable[fLine[Run]]
  else
  begin
    fTokenID := tkTagArea;
    repeat
      inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end;
end;

procedure TSynCobolSyn.CommentProc;
begin
  fIndicator := #0;

  if fLine[Run] in [#0, #10, #13] then
    fProcTable[fLine[Run]]
  else
  begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until (fLine[Run] in [#0, #10, #13]) or (Run > fCodeEndPos);
  end;
end;

procedure TSynCobolSyn.DebugProc;
begin
  fIndicator := #0;

  if fLine[Run] in [#0, #10, #13] then
    fProcTable[fLine[Run]]
  else
  begin
    fTokenID := tkDebugLines;
    repeat
      Inc(Run);
    until (fLine[Run] in [#0, #10, #13]) or (Run > fCodeEndPos);
  end;
end;

procedure TSynCobolSyn.PointProc;
begin
  if (Run < fCodeEndPos) and (FLine[Run + 1] in ['0'..'9', 'e', 'E']) then
    NumberProc
  else
    UnknownProc;
end;

procedure TSynCobolSyn.NumberProc;
var
  fFloat: Boolean;
begin
  fTokenID := tkNumber;
  Inc(Run);
  fFloat := False;

  while (FLine[Run] in ['0'..'9', '.', 'e', 'E', '-', '+']) and (Run <= fCodeEndPos) do
  begin
    case FLine[Run] of
      '.':
        if not (FLine[Run + 1] in ['0'..'9', 'e', 'E']) then
          Break
        else
          fFloat := True;
      'e', 'E':
          if not (FLine[Run - 1] in ['0'..'9', '.']) then
            Break
          else fFloat := True;
      '-', '+':
        begin
          if (not fFloat) or (not (FLine[Run - 1] in ['e', 'E'])) then
            Break;
        end;
    end;
    Inc(Run);
  end;
end;

procedure TSynCobolSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynCobolSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then
    inc(Run);
end;

procedure TSynCobolSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynCobolSyn.StringOpenProc;
begin
  case fLine[Run] of
    '"': fRange := rsQuoteString;
    '''': fRange := rsApostString;
    else
      if fLine[Run + 1] = '=' then
      begin
        fRange := rsPseudoText;
        Inc(Run);
      end
      else
      begin
        UnknownProc;
        Exit;
      end;
  end;

  Inc(Run);
  StringProc;
  fTokenID := tkString;
end;

procedure TSynCobolSyn.StringProc;
begin
  fTokenID := tkString;

  if Run <= fCodeEndPos then
  repeat
    if (fLine[Run] = StringChars[fRange])
      and ((fLine[Run] <> '=') or ((Run > 0) and (fLine[Run - 1] = '='))) then
    begin
      if (Run = fCodeEndPos) and (fRange in [rsQuoteString, rsApostString]) then
        Inc(fRange, 3)
      else
        fRange := rsUnknown;
      Inc(Run);
      Break;
    end;
    if not (fLine[Run] in [#0, #10, #13]) then
      Inc(Run);
  until (fLine[Run] in [#0, #10, #13]) or (Run > fCodeEndPos);
end;

procedure TSynCobolSyn.StringEndProc;
begin
  if fLine[Run] in [#0, #10, #13] then
    fProcTable[fLine[Run]]
  else
  begin
    fTokenID := tkString;

    if (fRange <> rsPseudoText) and (Run <= fCodeEndPos) then
    repeat
      if (fLine[Run] = StringChars[fRange]) then
      begin
        if fRange in [rsQuoteString, rsApostString] then
          Inc(Run)
        else
        begin
          Inc(Run, 2);
          Dec(fRange, 3);
        end;
        Break;
      end;
      Inc(Run);
    until (fLine[Run] in [#0, #10, #13]) or (Run > fCodeEndPos);

    StringProc;
  end;
end;

constructor TSynCobolSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clGray;
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fAIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrAreaAIdentifier);
  fAIdentifierAttri.Foreground := clTeal;
  fAIdentifierAttri.Style := [fsBold];
  AddAttribute(fAIdentifierAttri);

  fPreprocessorAttri := TSynHighLighterAttributes.Create(SYNS_AttrPreprocessor);
  fPreprocessorAttri.Foreground := clMaroon;
  AddAttribute(fPreprocessorAttri);

  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);

  fNumberAttri := TSynHighLighterAttributes.Create(SYNS_AttrNumber);
  fNumberAttri.Foreground := clGreen;
  AddAttribute(fNumberAttri);

  fBooleanAttri := TSynHighLighterAttributes.Create(SYNS_AttrBoolean);
  fBooleanAttri.Foreground := clGreen;
  AddAttribute(fBooleanAttri);

  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString);
  fStringAttri.Foreground := clBlue;
  AddAttribute(fStringAttri);

  fSequenceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSequence);
  fSequenceAttri.Foreground := clDkGray;
  AddAttribute(fSequenceAttri);

  fIndicatorAttri := TSynHighLighterAttributes.Create(SYNS_AttrIndicator);
  fIndicatorAttri.Foreground := clRed;
  AddAttribute(fIndicatorAttri);

  fTagAreaAttri := TSynHighLighterAttributes.Create(SYNS_AttrTagArea);
  fTagAreaAttri.Foreground := clMaroon;
  AddAttribute(fTagAreaAttri);

  fDebugLinesAttri := TSynHighLighterAttributes.Create(SYNS_AttrDebugLines);
  fDebugLinesAttri.Foreground := clDkGray;
  AddAttribute(fDebugLinesAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterCOBOL;
  fRange := rsUnknown;
  fIndicator := #0;

  fCodeStartPos := 7;
  fCodeMediumPos := 11;
  fCodeEndPos := 71;
end;

procedure TSynCobolSyn.SetLine(NewValue: String; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynCobolSyn.IdentProc;
begin
  if (UpCase(fLine[Run]) in ['X', 'G'])
    and (Run < fCodeEndPos) and (fLine[Run + 1] in ['"', '''']) then
  begin
    Inc(Run);
    StringOpenProc;
  end
  else
  begin
    fTokenID := IdentKind((fLine + Run));
    if (fTokenID = tkIdentifier) and (Run < fCodeMediumPos) then
      fTokenID := tkAIdentifier;
    inc(Run, fStringLen);

    while Identifiers[fLine[Run]] and (Run <= fCodeEndPos) do
      Inc(Run);
  end;
end;

procedure TSynCobolSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynCobolSyn.Next;
begin
  fTokenPos := Run;

  if fTokenPos < fCodeStartPos then
    FirstCharsProc
  else
    case fIndicator of
      '*', '/': CommentProc;
      'D': DebugProc;
      else
        if fTokenPos > fCodeEndPos then
          LastCharsProc
        else
          case fRange of
            rsQuoteString..rsApostStringMayBe: StringEndProc;
          else
            begin
              fRange := rsUnknown;
              fProcTable[fLine[Run]];
            end;
          end;
    end;
end;

function TSynCobolSyn.GetDefaultAttribute(Index: integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT    : Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER : Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD    : Result := fKeyAttri;
    SYN_ATTR_STRING     : Result := fStringAttri;
    SYN_ATTR_WHITESPACE : Result := fSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynCobolSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynCobolSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynCobolSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynCobolSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkAIdentifier: Result := fAIdentifierAttri;
    tkPreprocessor: Result := fPreprocessorAttri;
    tkKey: Result := fKeyAttri;
    tkBoolean: Result := fBooleanAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSequence: Result := fSequenceAttri;
    tkIndicator: Result := fIndicatorAttri;
    tkTagArea: Result := fTagAreaAttri;
    tkDebugLines: Result := fDebugLinesAttri;
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynCobolSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynCobolSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynCobolSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['-', '0'..'9', 'A'..'Z', 'a'..'z'];
end;

function TSynCobolSyn.GetSampleSource: string;
begin
  Result := '000100* This is a sample file to be used to show all TSynCobolSyn''s'#13#10 +
            '000200* features.'#13#10 +
            '000300* This isn''t a valid COBOL program.'#13#10 +
            '000400'#13#10 +
            '000500* 1. Supported COBOL features.'#13#10 +
            '000600'#13#10 +
            '000700* 1.1  Sequence area.'#13#10 +
            '000800*    First six columns in COBOL are reserved for enumeration'#13#10 +
            '000900*    of source lines.'#13#10 +
            '001000* 1.2  Indicator area.'#13#10 +
            '001100*    7th column in COBOL is reserved for special markers like ''*'''#13#10 +
            '001200*    or ''D''.'#13#10 +
            '001300* 1.3  Comment lines.'#13#10 +
            '001400*    Any line started from ''*'' in 7th column is a comment.'#13#10 +
            '001500*    No separate word highlighting will be done by the editor.'#13#10 +
            '001600* 1.4  Debug lines.'#13#10 +
            '001700D    Any line started from ''D'' will be treated as containing debug'#13#10 +
            '001800D    commands. No separate word highlighting will be done'#13#10 +
            '001900D    by the editor.'#13#10 +
            '002000* 1.5  Tag area.'#13#10 +
            '002100*    Only columns from 8th till 72th can be used for COBOL        TAG_AREA'#13#10 +
            '002200*    program. Columns beyond the 72th one may be used by some     TAG_AREA'#13#10 +
            '002300*    COBOL compilers to tag the code in some internal way.        TAG_AREA'#13#10 +
            '002400* 1.6  Area A identifiers.'#13#10 +
            '002500*    In area A (from 8th column till'#13#10 +
            '002600*    11th one) you should type only sections''/paragraphs'' names.'#13#10 +
            '002700*    For example "SOME" is a section name:'#13#10 +
            '002800 SOME SECTION.'#13#10 +
            '002900* 1.7  Preprocessor directives.'#13#10 +
            '003000*    For example "COPY" is a preprocessor directive:'#13#10 +
            '003100     COPY "PRD-DATA.SEL".'#13#10 +
            '003200* 1.8  Key words.'#13#10 +
            '003300*    For example "ACCEPT" and "AT" are COBOL key words:'#13#10 +
            '003400     ACCEPT WS-ENTRY AT 2030.'#13#10 +
            '003500* 1.9  Boolean constants.'#13#10 +
            '003600*    These are "TRUE" and "FALSE" constants. For example:'#13#10 +
            '003700     EVALUATE TRUE.'#13#10 +
            '003800* 1.10 Numbers.'#13#10 +
            '003900*    Here are the examples of numbers:'#13#10 +
            '004000 01  WSV-TEST-REC.'#13#10 +
            '004100     03  WSV-INT-T	       PIC 9(5) VALUE 12345.'#13#10 +
            '004200     03  WSV-PRICES              PIC 9(4)V99 COMP-3 VALUE 0000.33. 		'#13#10 +
            '004300     03  WSV-Z-PRICES            PIC Z(5)9.99- VALUE -2.12. 		'#13#10 +
            '004400     03  WSV-STORE-DATE          PIC 9(4)V99E99 VALUE 0001.33E02.'#13#10 +
            '004500* 1.11 Strings.'#13#10 +
            '004600*    The following types of strings are supported:'#13#10 +
            '004700*    1.11.1 Quoted strings.'#13#10 +
            '004800         MOVE "The name of field is ""PRODUCT""" TO WS-ERR-MESS.'#13#10 +
            '004900         MOVE ''The name of field is ''''PRODUCT'''''' TO WS-ERR-MESS.'#13#10 +
            '005000*    1.11.2 Pseudo-text.'#13#10 +
            '005100         COPY'#13#10 +
            '005200             REPLACING ==+00001== BY  +2'#13#10 +
            '005300                       == 1 ==    BY  -3.'#13#10 +
            '005400*    1.11.3 Figurative constants.'#13#10 +
            '005500*        For example "SPACES" is figurative constant:'#13#10 +
            '005600             DISPLAY SPACES UPON CRT.'#13#10 +
            '005700* 1.12 Continued lines.'#13#10 +
            '005800*    Only continued strings are supported. For example:'#13#10 +
            '005900         MOVE "The name of figurative constant field is'#13#10 +
            '006000-"SPACES" TO WS-ERR-MESS.'#13#10 +
            '006100*    Or (a single quotation mark in 72th column):'#13#10 +
            '005900         MOVE "The name of figurative constant field is  ""SPACES"'#13#10 +
            '006000-""" TO WS-ERR-MESS.'#13#10 +
            '006100'#13#10 +
            '006200* 2. Unsupported COBOL features.'#13#10 +
            '006300'#13#10 +
            '006400* 2.1 Continued lines.'#13#10 +
            '006500*    Continuation of key words is not supported. For example,'#13#10 +
            '006600*    the following COBOL code is valid but TSynCobolSyn won''t'#13#10 +
            '006700*    highlight "VALUE" keyword properly:'#13#10 +
            '006800     03  WSV-STORE-DATE                         PIC 9(4)V99E99 VAL'#13#10 +
            '006900-UE 0001.33E02.'#13#10 +
            '007000* 2.2 Identifiers started from digits.'#13#10 +
            '007100*    They are valid in COBOL but won''t be highlighted properly'#13#10 +
            '007200*    by TSynCobolSyn. For example, "000-main" is a paragraph'#13#10 +
            '007300*    name and should be highlighted as Area A identifier:'#13#10 +
            '007400 000-main.'#13#10 +
            '007500* 2.3 Comment entries in optional paragraphs'#13#10 +
            '007600*    The so called comment-entries in the optional paragraphs'#13#10 +
            '007700*    of the Identification Division are not supported and won''t'#13#10 +
            '007800*    be highlighted properly.';
end;

function TSynCobolSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterCOBOL;
end;

procedure TSynCobolSyn.SetCodeStartPos(Value: LongInt);
begin
  if Value < fCodeMediumPos then
    fCodeStartPos := Value
  else
    fCodeStartPos := fCodeMediumPos;
end;

procedure TSynCobolSyn.SetCodeMediumPos(Value: LongInt);
begin
  if (fCodeStartPos <= Value) and (Value <= fCodeEndPos) then
    fCodeMediumPos := Value
  else
    if Value > fCodeEndPos
    then fCodeMediumPos := fCodeEndPos
    else fCodeMediumPos := fCodeStartPos;
end;

procedure TSynCobolSyn.SetCodeEndPos(Value: LongInt);
begin
  if Value > fCodeMediumPos then
    fCodeEndPos := Value
  else
    fCodeEndPos := fCodeMediumPos;
end;

class function TSynCobolSyn.GetLanguageName: string;
begin
  Result := SYNS_LangCOBOL;
end;

procedure TSynCobolSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynCobolSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynCobolSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynCobolSyn);
{$ENDIF}
end.
