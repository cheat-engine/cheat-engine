{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterJScript.pas, released 2000-04-14.
The Original Code is based on the mwJScript.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Tony de Buys.
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

$Id: SynHighlighterJScript.pas,v 1.22.2.1 2007/04/19 07:09:38 etrusco Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a JavaScript/JScript highlighter for SynEdit)
@author(Tony De Buys [tony@lad.co.za], converted to SynEdit by David Muir <david@loanhead45.freeserve.co.uk>)
@created(December 1999, converted to SynEdit April 14, 2000)
@lastmod(2000-06-23)
The SynHighlighterJScript unit provides SynEdit with a JScript/JavaScript (.js) highlighter.
The highlighter formats JavaScript source code highlighting keywords, strings, numbers and characters.
}

{$IFNDEF QSYNHIGHLIGHTERJSCRIPT}
unit SynHighlighterJScript;
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
  Registry,
  SynEditTypes,
  SynEditHighlighter,
{$ENDIF}
  SysUtils, Classes;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkUnknown, tkNonReservedKey, tkEvent);

  TRangeState = (rsUnknown, rsANSI);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

type
  TSynJScriptSyn = class(TSynCustomHighLighter)
  private
    fRange: TRangeState;
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..252] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNonReservedKeyAttri: TSynHighlighterAttributes;
    fEventAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: String): Boolean;
    function Func5: TtkTokenKind;
    function Func15: TtkTokenKind;
    function Func17: TtkTokenKind;
    function Func18: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func22: TtkTokenKind;
    function Func23: TtkTokenKind;
    function Func25: TtkTokenKind;
    function Func26: TtkTokenKind;
    function Func28: TtkTokenKind;
    function Func29: TtkTokenKind;
    function Func30: TtkTokenKind;
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
    function Func98: TtkTokenKind;
    function Func99: TtkTokenKind;
    function Func100: TtkTokenKind;
    function Func101: TtkTokenKind;
    function Func102: TtkTokenKind;
    function Func103: TtkTokenKind;
    function Func105: TtkTokenKind;
    function Func106: TtkTokenKind;
    function Func107: TtkTokenKind;
    function Func108: TtkTokenKind;
    function Func109: TtkTokenKind;
    function Func110: TtkTokenKind;
    function Func111: TtkTokenKind;
    function Func113: TtkTokenKind;
    function Func114: TtkTokenKind;
    function Func115: TtkTokenKind;
    function Func116: TtkTokenKind;
    function Func117: TtkTokenKind;
    function Func118: TtkTokenKind;
    function Func119: TtkTokenKind;
    function Func120: TtkTokenKind;
    function Func121: TtkTokenKind;
    function Func122: TtkTokenKind;
    function Func123: TtkTokenKind;
    function Func124: TtkTokenKind;
    function Func125: TtkTokenKind;
    function Func126: TtkTokenKind;
    function Func128: TtkTokenKind;
    function Func129: TtkTokenKind;
    function Func130: TtkTokenKind;
    function Func131: TtkTokenKind;
    function Func132: TtkTokenKind;
    function Func133: TtkTokenKind;
    function Func135: TtkTokenKind;
    function Func136: TtkTokenKind;
    function Func139: TtkTokenKind;
    function Func140: TtkTokenKind;
    function Func142: TtkTokenKind;
    function Func143: TtkTokenKind;
    function Func144: TtkTokenKind;
    function Func145: TtkTokenKind;
    function Func146: TtkTokenKind;
    function Func147: TtkTokenKind;
    function Func150: TtkTokenKind;
    function Func155: TtkTokenKind;
    function Func157: TtkTokenKind;
    function Func158: TtkTokenKind;
    function Func160: TtkTokenKind;
    function Func162: TtkTokenKind;
    function Func166: TtkTokenKind;
    function Func167: TtkTokenKind;
    function Func169: TtkTokenKind;
    function Func170: TtkTokenKind;
    function Func176: TtkTokenKind;
    function Func177: TtkTokenKind;
    function Func178: TtkTokenKind;
    function Func188: TtkTokenKind;
    function Func189: TtkTokenKind;
    function Func210: TtkTokenKind;
    function Func220: TtkTokenKind;
    function Func222: TtkTokenKind;
    function Func252: TtkTokenKind;
    procedure AndSymbolProc;
    procedure CommentProc;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure MinusProc;
    procedure ModSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure PointProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StarProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: String; override;
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
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NonReservedKeyAttri: TSynHighlighterAttributes read fNonReservedKeyAttri write fNonReservedKeyAttri;
    property EventAttri: TSynHighlighterAttributes read fEventAttri write fEventAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
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
  I, J: Char;
begin
  for I := #0 to #255 do
  begin
    Case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
      else Identifiers[I] := False;
    end;
    J := UpCase(I);
    Case I in ['_', 'A'..'Z', 'a'..'z'] of
      True: mHashTable[I] := Ord(J) - 64
      else mHashTable[I] := 0;
    end;
  end;
end;

procedure TSynJScriptSyn.InitIdent;
var
  I: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do begin
    pF^ := AltFunc;
    Inc(pF);
  end;
  fIdentFuncTable[5] := Func5;
  fIdentFuncTable[15] := Func15;
  fIdentFuncTable[17] := Func17;
  fIdentFuncTable[18] := Func18;
  fIdentFuncTable[19] := Func19;
  fIdentFuncTable[22] := Func22;
  fIdentFuncTable[23] := Func23;
  fIdentFuncTable[25] := Func25;
  fIdentFuncTable[26] := Func26;
  fIdentFuncTable[28] := Func28;
  fIdentFuncTable[29] := Func29;
  fIdentFuncTable[30] := Func30;
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
  fIdentFuncTable[98] := Func98;
  fIdentFuncTable[99] := Func99;
  fIdentFuncTable[100] := Func100;
  fIdentFuncTable[101] := Func101;
  fIdentFuncTable[102] := Func102;
  fIdentFuncTable[103] := Func103;
  fIdentFuncTable[105] := Func105;
  fIdentFuncTable[106] := Func106;
  fIdentFuncTable[107] := Func107;
  fIdentFuncTable[108] := Func108;
  fIdentFuncTable[109] := Func109;
  fIdentFuncTable[110] := Func110;
  fIdentFuncTable[111] := Func111;
  fIdentFuncTable[113] := Func113;
  fIdentFuncTable[114] := Func114;
  fIdentFuncTable[115] := Func115;
  fIdentFuncTable[116] := Func116;
  fIdentFuncTable[117] := Func117;
  fIdentFuncTable[118] := Func118;
  fIdentFuncTable[119] := Func119;
  fIdentFuncTable[120] := Func120;
  fIdentFuncTable[121] := Func121;
  fIdentFuncTable[122] := Func122;
  fIdentFuncTable[123] := Func123;
  fIdentFuncTable[124] := Func124;
  fIdentFuncTable[125] := Func125;
  fIdentFuncTable[126] := Func126;
  fIdentFuncTable[128] := Func128;
  fIdentFuncTable[129] := Func129;
  fIdentFuncTable[130] := Func130;
  fIdentFuncTable[131] := Func131;
  fIdentFuncTable[132] := Func132;
  fIdentFuncTable[133] := Func133;
  fIdentFuncTable[135] := Func135;
  fIdentFuncTable[136] := Func136;
  fIdentFuncTable[139] := Func139;
  fIdentFuncTable[140] := Func140;
  fIdentFuncTable[142] := Func142;
  fIdentFuncTable[143] := Func143;
  fIdentFuncTable[144] := Func144;
  fIdentFuncTable[145] := Func145;
  fIdentFuncTable[146] := Func146;
  fIdentFuncTable[147] := Func147;
  fIdentFuncTable[150] := Func150;
  fIdentFuncTable[155] := Func155;
  fIdentFuncTable[157] := Func157;
  fIdentFuncTable[158] := Func158;
  fIdentFuncTable[160] := Func160;
  fIdentFuncTable[162] := Func162;
  fIdentFuncTable[166] := Func166;
  fIdentFuncTable[167] := Func167;
  fIdentFuncTable[169] := Func169;
  fIdentFuncTable[170] := Func170;
  fIdentFuncTable[176] := Func176;
  fIdentFuncTable[177] := Func177;
  fIdentFuncTable[178] := Func178;
  fIdentFuncTable[188] := Func188;
  fIdentFuncTable[189] := Func189;
  fIdentFuncTable[210] := Func210;
  fIdentFuncTable[220] := Func220;
  fIdentFuncTable[222] := Func222;
  fIdentFuncTable[252] := Func252;
end;

function TSynJScriptSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end;

function TSynJScriptSyn.KeyComp(const aKey: String): Boolean;
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
end;

function TSynJScriptSyn.Func5: TtkTokenKind;
begin
  if KeyComp('E') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func15: TtkTokenKind;
begin
  if KeyComp('if') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func17: TtkTokenKind;
begin
  if KeyComp('back') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func18: TtkTokenKind;
begin
  if KeyComp('big') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func19: TtkTokenKind;
begin
  if KeyComp('do') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func22: TtkTokenKind;
begin
  if KeyComp('abs') then Result := tkNonReservedKey else
    if KeyComp('go') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func23: TtkTokenKind;
begin
  if KeyComp('in') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func25: TtkTokenKind;
begin
  if KeyComp('Area') then Result := tkNonReservedKey else
    if KeyComp('PI') then Result := tkNonReservedKey else
      if KeyComp('All') then Result := tkNonReservedKey else
        if KeyComp('all') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func26: TtkTokenKind;
begin
  if KeyComp('LN10') then Result := tkNonReservedKey else
    if KeyComp('LN2') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func28: TtkTokenKind;
begin
  if KeyComp('case') then Result := tkKey else
    if KeyComp('call') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func29: TtkTokenKind;
begin
  if KeyComp('NaN') then Result := tkKey else
    if KeyComp('Embed') then Result := tkNonReservedKey else
      if KeyComp('ceil') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func30: TtkTokenKind;
begin
  if KeyComp('Date') then Result := tkNonReservedKey else
    if KeyComp('char') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func33: TtkTokenKind;
begin
  if KeyComp('bold') then Result := tkNonReservedKey else
    if KeyComp('name') then Result := tkNonReservedKey else
      if KeyComp('find') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func34: TtkTokenKind;
begin
  if KeyComp('log') then Result := tkNonReservedKey else
    if KeyComp('java') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func35: TtkTokenKind;
begin
  if KeyComp('Image') then Result := tkNonReservedKey else
    if KeyComp('tan') then Result := tkNonReservedKey else
      if KeyComp('catch') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func36: TtkTokenKind;
begin
  if KeyComp('min') then Result := tkNonReservedKey else
    if KeyComp('hash') then Result := tkNonReservedKey else
      if KeyComp('atan2') then Result := tkNonReservedKey else
        if KeyComp('atan') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func37: TtkTokenKind;
begin
  if KeyComp('break') then Result := tkKey else
    if KeyComp('href') then Result := tkNonReservedKey else
      if KeyComp('cos') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func38: TtkTokenKind;
begin
  if KeyComp('click') then Result := tkNonReservedKey else
    if KeyComp('acos') then Result := tkNonReservedKey else
      if KeyComp('max') then Result := tkNonReservedKey else
        if KeyComp('callee') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func39: TtkTokenKind;
begin
  if KeyComp('LOG10E') then Result := tkNonReservedKey else
    if KeyComp('LOG2E') then Result := tkNonReservedKey else
      if KeyComp('checked') then Result := tkNonReservedKey else
        if KeyComp('clear') then Result := tkNonReservedKey else
          if KeyComp('for') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func40: TtkTokenKind;
begin
  if KeyComp('eval') then Result := tkNonReservedKey else
    if KeyComp('src') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func41: TtkTokenKind;
begin
  if KeyComp('else') then Result := tkKey else
    if KeyComp('var') then Result := tkKey else
      if KeyComp('home') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func42: TtkTokenKind;
begin
  if KeyComp('self') then Result := tkNonReservedKey else
    if KeyComp('Math') then Result := tkNonReservedKey else
      if KeyComp('sin') then Result := tkNonReservedKey else
        if KeyComp('new') then Result := tkKey else
          if KeyComp('sub') then Result := tkNonReservedKey else
            if KeyComp('final') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func43: TtkTokenKind;
begin
  if KeyComp('asin') then Result := tkNonReservedKey else
    if KeyComp('Frame') then Result := tkNonReservedKey else
      if KeyComp('false') then Result := tkKey else
        if KeyComp('int') then Result := tkKey else
          if KeyComp('left') then Result := tkNonReservedKey else
            if KeyComp('align') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func44: TtkTokenKind;
begin
  if KeyComp('Hidden') then Result := tkNonReservedKey else
    if KeyComp('UTC') then Result := tkNonReservedKey else
      if KeyComp('package') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func45: TtkTokenKind;
begin
  if KeyComp('exp') then Result := tkNonReservedKey else
    if KeyComp('match') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func46: TtkTokenKind;
begin
  if KeyComp('Link') then Result := tkNonReservedKey else
    if KeyComp('link') then Result := tkNonReservedKey else
      if KeyComp('body') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func47: TtkTokenKind;
begin
  if KeyComp('Radio') then Result := tkNonReservedKey else
    if KeyComp('tags') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func48: TtkTokenKind;
begin
  if KeyComp('join') then Result := tkNonReservedKey else
    if KeyComp('embeds') then Result := tkNonReservedKey else
      if KeyComp('blink') then Result := tkNonReservedKey else
        if KeyComp('fixed') then Result := tkNonReservedKey else
          if KeyComp('slice') then Result := tkNonReservedKey else
            if KeyComp('long') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func49: TtkTokenKind;
begin
  if KeyComp('escape') then Result := tkNonReservedKey else
    if KeyComp('Global') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func50: TtkTokenKind;
begin
  if KeyComp('open') then Result := tkNonReservedKey else
    if KeyComp('void') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func51: TtkTokenKind;
begin
  if KeyComp('charAt') then Result := tkNonReservedKey else
    if KeyComp('top') then Result := tkNonReservedKey else
      if KeyComp('URL') then Result := tkNonReservedKey else
        if KeyComp('caller') then Result := tkNonReservedKey else
          if KeyComp('delete') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func52: TtkTokenKind;
begin
  if KeyComp('Form') then Result := tkNonReservedKey else
    if KeyComp('form') then Result := tkNonReservedKey else
      if KeyComp('hspace') then Result := tkNonReservedKey else
        if KeyComp('byte') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func53: TtkTokenKind;
begin
  if KeyComp('blur') then Result := tkNonReservedKey else
    if KeyComp('enum') then Result := tkKey else
      if KeyComp('pageX') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func54: TtkTokenKind;
begin
  if KeyComp('pow') then Result := tkNonReservedKey else
    if KeyComp('close') then Result := tkNonReservedKey else
      if KeyComp('search') then Result := tkNonReservedKey else
        if KeyComp('images') then Result := tkNonReservedKey else
          if KeyComp('class') then Result := tkKey else
            if KeyComp('float') then Result := tkKey else
              if KeyComp('Float') then Result := tkNonReservedKey else
                if KeyComp('pageY') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func55: TtkTokenKind;
begin
  if KeyComp('reload') then Result := tkNonReservedKey else
    if KeyComp('Object') then Result := tkNonReservedKey else
      if KeyComp('watch') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func56: TtkTokenKind;
begin
  if KeyComp('this') then Result := tkKey else
    if KeyComp('alert') then Result := tkNonReservedKey else
      if KeyComp('sup') then Result := tkNonReservedKey else
        if KeyComp('domain') then Result := tkNonReservedKey else
          if KeyComp('index') then Result := tkNonReservedKey else
            if KeyComp('concat') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func57: TtkTokenKind;
begin
  if KeyComp('isNaN') then Result := tkNonReservedKey else
    if KeyComp('small') then Result := tkNonReservedKey else
      if KeyComp('while') then Result := tkKey else
        if KeyComp('height') then Result := tkNonReservedKey else
          if KeyComp('goto') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func58: TtkTokenKind;
begin
  if KeyComp('cookie') then Result := tkNonReservedKey else
    if KeyComp('closed') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func59: TtkTokenKind;
begin
  if KeyComp('parse') then Result := tkNonReservedKey else
    if KeyComp('Anchor') then Result := tkNonReservedKey else
      if KeyComp('anchor') then Result := tkNonReservedKey else
        if KeyComp('double') then Result := tkKey else
          if KeyComp('Null') then Result := tkNonReservedKey else
            if KeyComp('null') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func60: TtkTokenKind;
begin
  if KeyComp('with') then Result := tkKey else
    if KeyComp('replace') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func61: TtkTokenKind;
begin
  if KeyComp('onLoad') then Result := tkEvent else
    if KeyComp('value') then Result := tkNonReservedKey else
        if KeyComp('Layer') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func62: TtkTokenKind;
begin
  if KeyComp('action') then Result := tkNonReservedKey else
    if KeyComp('getDate') then Result := tkNonReservedKey else
      if KeyComp('getDay') then Result := tkNonReservedKey else
        if KeyComp('border') then Result := tkNonReservedKey else
          if KeyComp('host') then Result := tkNonReservedKey else
            if KeyComp('frames') then Result := tkNonReservedKey else
              if KeyComp('right') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func63: TtkTokenKind;
begin
  if KeyComp('Array') then Result := tkNonReservedKey else
    if KeyComp('next') then Result := tkNonReservedKey else
      if KeyComp('try') then Result := tkKey else
        if KeyComp('public') then Result := tkKey else
          if KeyComp('Packages') then Result := tkNonReservedKey else
            if KeyComp('logon') then Result := tkNonReservedKey else
              if KeyComp('color') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func64: TtkTokenKind;
begin
  if KeyComp('Boolean') then Result := tkNonReservedKey else
    if KeyComp('Select') then Result := tkNonReservedKey else
      if KeyComp('select') then Result := tkNonReservedKey else
        if KeyComp('taint') then Result := tkNonReservedKey else
          if KeyComp('focus') then Result := tkNonReservedKey else
            if KeyComp('boolean') then Result := tkKey else
              if KeyComp('width') then Result := tkNonReservedKey else
                if KeyComp('true') then Result := tkKey else
                  if KeyComp('screen') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func65: TtkTokenKind;
begin
  if KeyComp('filename') then Result := tkNonReservedKey else
    if KeyComp('links') then Result := tkNonReservedKey else
      if KeyComp('method') then Result := tkNonReservedKey else
        if KeyComp('random') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func66: TtkTokenKind;
begin
  if KeyComp('vspace') then Result := tkNonReservedKey else
    if KeyComp('length') then Result := tkNonReservedKey else
      if KeyComp('title') then Result := tkNonReservedKey else
        if KeyComp('type') then Result := tkNonReservedKey else
          if KeyComp('appName') then Result := tkNonReservedKey else
            if KeyComp('floor') then Result := tkNonReservedKey else
              if KeyComp('event') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func67: TtkTokenKind;
begin
  if KeyComp('onClick') then Result := tkEvent else
    if KeyComp('onChange') then Result := tkEvent else
      if KeyComp('reset') then Result := tkNonReservedKey else
        if KeyComp('Reset') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func69: TtkTokenKind;
begin
  if KeyComp('port') then Result := tkNonReservedKey else
    if KeyComp('Text') then Result := tkNonReservedKey else
      if KeyComp('text') then Result := tkNonReservedKey else
        if KeyComp('default') then Result := tkKey else
          if KeyComp('debugger') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func70: TtkTokenKind;
begin
  if KeyComp('Applet') then Result := tkNonReservedKey else
    if KeyComp('stop') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func71: TtkTokenKind;
begin
  if KeyComp('target') then Result := tkNonReservedKey else
    if KeyComp('Checkbox') then Result := tkNonReservedKey else
      if KeyComp('encoding') then Result := tkNonReservedKey else
        if KeyComp('forms') then Result := tkNonReservedKey else
          if KeyComp('const') then Result := tkKey else
            if KeyComp('native') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func72: TtkTokenKind;
begin
  if KeyComp('round') then Result := tkNonReservedKey else
    if KeyComp('sort') then Result := tkNonReservedKey else
      if KeyComp('bgColor') then Result := tkNonReservedKey else
        if KeyComp('static') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func73: TtkTokenKind;
begin
  if KeyComp('italics') then Result := tkNonReservedKey else
    if KeyComp('Number') then Result := tkNonReservedKey else
      if KeyComp('opener') then Result := tkNonReservedKey else
        if KeyComp('selected') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func74: TtkTokenKind;
begin
  if KeyComp('sqrt') then Result := tkNonReservedKey else
    if KeyComp('SQRT2') then Result := tkNonReservedKey else
      if KeyComp('parent') then Result := tkNonReservedKey else
        if KeyComp('setDate') then Result := tkNonReservedKey else
          if KeyComp('menubar') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func75: TtkTokenKind;
begin
  if KeyComp('write') then Result := tkNonReservedKey else
    if KeyComp('RegExp') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func76: TtkTokenKind;
begin
  if KeyComp('fgColor') then Result := tkNonReservedKey else
    if KeyComp('split') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func77: TtkTokenKind;
begin
  if KeyComp('javaEnabled') then Result := tkNonReservedKey else
    if KeyComp('indexOf') then Result := tkNonReservedKey else
      if KeyComp('print') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func78: TtkTokenKind;
begin
  if KeyComp('anchors') then Result := tkNonReservedKey else
    if KeyComp('confirm') then Result := tkNonReservedKey else
      if KeyComp('pathname') then Result := tkNonReservedKey else
        if KeyComp('start') then Result := tkKey else
          if KeyComp('charCodeAt') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func79: TtkTokenKind;
begin
  if KeyComp('Plugin') then Result := tkNonReservedKey else
    if KeyComp('getTime') then Result := tkNonReservedKey else
      if KeyComp('refresh') then Result := tkNonReservedKey else
        if KeyComp('scroll') then Result := tkNonReservedKey else
          if KeyComp('finally') then Result := tkKey else
            if KeyComp('super') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func80: TtkTokenKind;
begin
  if KeyComp('short') then Result := tkKey else
    if KeyComp('layers') then Result := tkNonReservedKey else
      if KeyComp('input') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func81: TtkTokenKind;
begin
  if KeyComp('getYear') then Result := tkNonReservedKey else
    if KeyComp('interface') then Result := tkKey else
      if KeyComp('style') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func82: TtkTokenKind;
begin
  if KeyComp('onBlur') then Result := tkEvent else
    if KeyComp('strike') then Result := tkNonReservedKey else
      if KeyComp('valueOf') then Result := tkNonReservedKey else
        if KeyComp('moveBy') then Result := tkNonReservedKey else
          if KeyComp('switch') then Result := tkKey else
            if KeyComp('zIndex') then Result := tkNonReservedKey else
              if KeyComp('Undefined') then Result := tkNonReservedKey else
                if KeyComp('undefined') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func83: TtkTokenKind;
begin
  if KeyComp('netscape') then Result := tkNonReservedKey else
    if KeyComp('toolbar') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func84: TtkTokenKind;
begin
  if KeyComp('Submit') then Result := tkNonReservedKey else
    if KeyComp('submit') then Result := tkNonReservedKey else
      if KeyComp('unescape') then Result := tkNonReservedKey else
        if KeyComp('throw') then Result := tkKey else
          if KeyComp('abstract') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func85: TtkTokenKind;
begin
  if KeyComp('onAbort') then Result := tkEvent else
    if KeyComp('forward') then Result := tkNonReservedKey else
      if KeyComp('onDblClick') then Result := tkEvent else
        if KeyComp('bottom') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func86: TtkTokenKind;
begin
  if KeyComp('display') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func87: TtkTokenKind;
begin
  if KeyComp('String') then Result := tkNonReservedKey else
    if KeyComp('typeof') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func88: TtkTokenKind;
begin
  if KeyComp('Window') then Result := tkNonReservedKey else
    if KeyComp('window') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func89: TtkTokenKind;
begin
  if KeyComp('Location') then Result := tkNonReservedKey else
    if KeyComp('location') then Result := tkNonReservedKey else
      if KeyComp('complete') then Result := tkNonReservedKey else
        if KeyComp('applets') then Result := tkNonReservedKey else
          if KeyComp('Option') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func90: TtkTokenKind;
begin
  if KeyComp('lowsrc') then Result := tkNonReservedKey else
    if KeyComp('moveTo') then Result := tkNonReservedKey else
      if KeyComp('unwatch') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func91: TtkTokenKind;
begin
  if KeyComp('setTime') then Result := tkNonReservedKey else
    if KeyComp('import') then Result := tkKey else
      if KeyComp('extends') then Result := tkKey else
        if KeyComp('private') then Result := tkKey else
          if KeyComp('isFinite') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func92: TtkTokenKind;
begin
  if KeyComp('Button') then Result := tkNonReservedKey else
    if KeyComp('reverse') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func93: TtkTokenKind;
begin
  if KeyComp('appCodeName') then Result := tkNonReservedKey else
    if KeyComp('setYear') then Result := tkNonReservedKey else
      if KeyComp('referrer') then Result := tkNonReservedKey else
          if KeyComp('elements') then Result := tkNonReservedKey else
            if KeyComp('onFocus') then Result := tkEvent else
              if KeyComp('onSelect') then Result := tkEvent else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func94: TtkTokenKind;
begin
  if KeyComp('Textarea') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func95: TtkTokenKind;
begin
  if KeyComp('hostname') then Result := tkNonReservedKey else
    if KeyComp('document') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func96: TtkTokenKind;
begin
  if KeyComp('onUnload') then Result := tkEvent else
    if KeyComp('return') then Result := tkKey else
      if KeyComp('onReset') then Result := tkEvent else
        if KeyComp('background') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func98: TtkTokenKind;
begin
  if KeyComp('prompt') then Result := tkNonReservedKey else
    if KeyComp('plugins') then Result := tkNonReservedKey else
      if KeyComp('export') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func99: TtkTokenKind;
begin
  if KeyComp('current') then Result := tkNonReservedKey else
    if KeyComp('untaint') then Result := tkNonReservedKey else
      if KeyComp('substr') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func100: TtkTokenKind;
begin
  if KeyComp('status') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func101: TtkTokenKind;
begin
  if KeyComp('FileUpload') then Result := tkNonReservedKey else
    if KeyComp('writeln') then Result := tkNonReservedKey else
      if KeyComp('continue') then Result := tkKey else
        if KeyComp('platform') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func102: TtkTokenKind;
begin
  if KeyComp('getMonth') then Result := tkNonReservedKey else
    if KeyComp('Function') then Result := tkNonReservedKey else
      if KeyComp('function') then Result := tkKey else
        if KeyComp('parseInt') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func103: TtkTokenKind;
begin
  if KeyComp('onError') then Result := tkEvent else
    if KeyComp('throws') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func105: TtkTokenKind;
begin
  if KeyComp('SQRT1_2') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func106: TtkTokenKind;
begin
  if KeyComp('MimeType') then Result := tkNonReservedKey else
    if KeyComp('instanceof') then Result := tkKey else
      if KeyComp('protected') then Result := tkKey else
        if KeyComp('Infinity') then Result := tkNonReservedKey else
          if KeyComp('scrollBy') then Result := tkNonReservedKey else
            if KeyComp('getUTCDate') then Result := tkNonReservedKey else
              if KeyComp('getUTCDay') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func107: TtkTokenKind;
begin
  if KeyComp('taintEnabled') then Result := tkNonReservedKey else
    if KeyComp('Navigator') then Result := tkNonReservedKey else
      if KeyComp('navigator') then Result := tkNonReservedKey else
        if KeyComp('onKeyUp') then Result := tkEvent else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func108: TtkTokenKind;
begin
  if KeyComp('defaultChecked') then Result := tkNonReservedKey else
    if KeyComp('options') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func109: TtkTokenKind;
begin
  if KeyComp('suffixes') then Result := tkNonReservedKey else
    if KeyComp('linkColor') then Result := tkNonReservedKey else
      if KeyComp('resizeBy') then Result := tkNonReservedKey else
        if KeyComp('fromCharCode') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func110: TtkTokenKind;
begin
  if KeyComp('userAgent') then Result := tkNonReservedKey else
    if KeyComp('alinkColor') then Result := tkNonReservedKey else
      if KeyComp('locationbar') then Result := tkNonReservedKey else
        if KeyComp('handleEvent') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func111: TtkTokenKind;
begin
  if KeyComp('getSeconds') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func113: TtkTokenKind;
begin
  if KeyComp('onSubmit') then Result := tkEvent else
    if KeyComp('parseFloat') then Result := tkNonReservedKey else
      if KeyComp('getHours') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func114: TtkTokenKind;
begin
  if KeyComp('fontsize') then Result := tkNonReservedKey else
    if KeyComp('History') then Result := tkNonReservedKey else
      if KeyComp('history') then Result := tkNonReservedKey else
        if KeyComp('setMonth') then Result := tkNonReservedKey else
          if KeyComp('protocol') then Result := tkNonReservedKey else
            if KeyComp('scrollTo') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func115: TtkTokenKind;
begin
  if KeyComp('Password') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func116: TtkTokenKind;
begin
  if KeyComp('toSource') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func117: TtkTokenKind;
begin
  if KeyComp('lastModified') then Result := tkNonReservedKey else
    if KeyComp('resizeTo') then Result := tkNonReservedKey else
      if KeyComp('innerHeight') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func118: TtkTokenKind;
begin
  if KeyComp('fontcolor') then Result := tkNonReservedKey else
    if KeyComp('Arguments') then Result := tkNonReservedKey else
      if KeyComp('arguments') then Result := tkNonReservedKey else
        if KeyComp('setUTCDate') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func119: TtkTokenKind;
begin
  if KeyComp('scrollbars') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func120: TtkTokenKind;
begin
  if KeyComp('transient') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func121: TtkTokenKind;
begin
  if KeyComp('personalbar') then Result := tkNonReservedKey else
    if KeyComp('statusbar') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func122: TtkTokenKind;
begin
  if KeyComp('toString') then Result := tkNonReservedKey else
    if KeyComp('enabledPlugin') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func123: TtkTokenKind;
begin
  if KeyComp('setSeconds') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func124: TtkTokenKind;
begin
  if KeyComp('innerWidth') then Result := tkNonReservedKey else
    if KeyComp('pageXOffset') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func125: TtkTokenKind;
begin
  if KeyComp('previous') then Result := tkNonReservedKey else
    if KeyComp('setHours') then Result := tkNonReservedKey else
      if KeyComp('mimeTypes') then Result := tkNonReservedKey else
        if KeyComp('pageYOffset') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func126: TtkTokenKind;
begin
  if KeyComp('implements') then Result := tkKey else
    if KeyComp('onKeyDown') then Result := tkEvent else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func128: TtkTokenKind;
begin
  if KeyComp('MIN_VALUE') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func129: TtkTokenKind;
begin
  if KeyComp('lastIndexOf') then Result := tkNonReservedKey else
    if KeyComp('substring') then Result := tkNonReservedKey else
      if KeyComp('selectedIndex') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func130: TtkTokenKind;
begin
  if KeyComp('defaultValue') then Result := tkNonReservedKey else
    if KeyComp('MAX_VALUE') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func131: TtkTokenKind;
begin
  if KeyComp('vlinkColor') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func132: TtkTokenKind;
begin
  if KeyComp('description') then Result := tkNonReservedKey else
    if KeyComp('getFullYear') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func133: TtkTokenKind;
begin
  if KeyComp('getMinutes') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func135: TtkTokenKind;
begin
  if KeyComp('appVersion') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func136: TtkTokenKind;
begin
  if KeyComp('toLowerCase') then Result := tkNonReservedKey else
    if KeyComp('outerHeight') then Result := tkNonReservedKey else
      if KeyComp('visibility') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func139: TtkTokenKind;
begin
  if KeyComp('toUpperCase') then Result := tkNonReservedKey else
    if KeyComp('onMouseUp') then Result := tkEvent else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func140: TtkTokenKind;
begin
  if KeyComp('clearInterval') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func142: TtkTokenKind;
begin
  if KeyComp('defaultSelected') then Result := tkNonReservedKey else
    if KeyComp('clearTimeout') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func143: TtkTokenKind;
begin
  if KeyComp('outerWidth') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func144: TtkTokenKind;
begin
  if KeyComp('setFullYear') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func145: TtkTokenKind;
begin
  if KeyComp('setMinutes') then Result := tkNonReservedKey else
    if KeyComp('setInterval') then Result := tkNonReservedKey else
      if KeyComp('routeEvent') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func146: TtkTokenKind;
begin
  if KeyComp('getUTCMonth') then Result := tkNonReservedKey else
    if KeyComp('getElementById') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func147: TtkTokenKind;
begin
  if KeyComp('setTimeout') then Result := tkNonReservedKey else
    if KeyComp('onKeyPress') then Result := tkEvent else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func150: TtkTokenKind;
begin
  if KeyComp('prototype') then Result := tkKey else
    if KeyComp('releaseEvents') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func155: TtkTokenKind;
begin
  if KeyComp('getUTCSeconds') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func157: TtkTokenKind;
begin
  if KeyComp('onMouseMove') then Result := tkEvent else
    if KeyComp('getUTCHours') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func158: TtkTokenKind;
begin
  if KeyComp('onMouseOut') then Result := tkEvent else
    if KeyComp('onMouseDown') then Result := tkEvent else
      if KeyComp('setUTCMonth') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func160: TtkTokenKind;
begin
  if KeyComp('synchronized') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func162: TtkTokenKind;
begin
  if KeyComp('toGMTString') then Result := tkNonReservedKey else
    if KeyComp('onMouseOver') then Result := tkEvent else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func166: TtkTokenKind;
begin
  if KeyComp('constructor') then Result := tkKey else
    if KeyComp('getMilliseconds') then Result := tkNonReservedKey else
      if KeyComp('toUTCString') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func167: TtkTokenKind;
begin
  if KeyComp('setUTCSeconds') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func169: TtkTokenKind;
begin
  if KeyComp('defaultStatus') then Result := tkNonReservedKey else
     if KeyComp('captureEvents') then Result := tkNonReservedKey else
       if KeyComp('setUTCHours') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func170: TtkTokenKind;
begin
  if KeyComp('toLocaleString') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func176: TtkTokenKind;
begin
  if KeyComp('getUTCFullYear') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func177: TtkTokenKind;
begin
  if KeyComp('getUTCMinutes') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func178: TtkTokenKind;
begin
  if KeyComp('setMilliseconds') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func188: TtkTokenKind;
begin
  if KeyComp('setUTCFullYear') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func189: TtkTokenKind;
begin
  if KeyComp('setUTCMinutes') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func210: TtkTokenKind;
begin
  if KeyComp('getTimezoneOffset') then Result := tkNonReservedKey else
    if KeyComp('getUTCMilliseconds') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func220: TtkTokenKind;
begin
  if KeyComp('NEGATIVE_INFINITY') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func222: TtkTokenKind;
begin
  if KeyComp('setUTCMilliseconds') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.Func252: TtkTokenKind;
begin
  if KeyComp('POSITIVE_INFINITY') then Result := tkNonReservedKey else Result := tkIdentifier;
end;

function TSynJScriptSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynJScriptSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 253 then
    Result := fIdentFuncTable[HashKey]
  else
    Result := tkIdentifier;
end;

procedure TSynJScriptSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      '&': fProcTable[I] := AndSymbolProc;
      #13: fProcTable[I] := CRProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := IdentProc;
      #10: fProcTable[I] := LFProc;
      '-': fProcTable[I] := MinusProc;
      '%': fProcTable[I] := ModSymbolProc;
      #0: fProcTable[I] := NullProc;
      '0'..'9': fProcTable[I] := NumberProc;
      '|': fProcTable[I] := OrSymbolProc;
      '+': fProcTable[I] := PlusProc;
      '.': fProcTable[I] := PointProc;
      '/': fProcTable[I] := SlashProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := SpaceProc;
      '*': fProcTable[I] := StarProc;
      '"', #39: fProcTable[I] := StringProc;
      '~', '{', '}', ',', '(', ')', '[', ']', '<', '>', ':', '?', ';', '!', '=':
        fProcTable[I] := SymbolProc;
    else
      fProcTable[I] := UnknownProc;
    end;
end;

constructor TSynJScriptSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fNonReservedKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrNonReservedKeyword);
  AddAttribute(fNonReservedKeyAttri);
  fEventAttri := TSynHighlighterAttributes.Create(SYNS_AttrEvent);
  AddAttribute(fEventAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterJScript;
  fRange := rsUnknown;
end;

procedure TSynJScriptSyn.SetLine(NewValue: String; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynJScriptSyn.AndSymbolProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in ['=', '&'] then inc(Run);
end;

procedure TSynJScriptSyn.CommentProc;
begin
  if fLine[Run] = #0 then
    fTokenID := tkNull
  else begin
    fTokenID := tkComment;
    repeat
      if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then begin
        fRange := rsUnKnown;
        inc(Run, 2);
        break;
      end;
      inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end;
end;

procedure TSynJScriptSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then inc(Run);
end;

procedure TSynJScriptSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TSynJScriptSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynJScriptSyn.MinusProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in ['=', '-', '>'] then inc(Run);
end;

procedure TSynJScriptSyn.ModSymbolProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '=' then inc(Run);
end;

procedure TSynJScriptSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynJScriptSyn.NumberProc;
var
  idx1: Integer; // token[1]
  isHex: Boolean;
begin
  fTokenID := tkNumber;
  isHex := False;
  idx1 := Run;
  Inc(Run);
  while FLine[Run] in ['0'..'9', '.', 'a'..'f', 'A'..'F', 'x', 'X'] do
  begin
    case FLine[Run] of
      '.':
        if FLine[Succ(Run)] = '.' then
          Break;
      'a'..'f', 'A'..'F':
        if not isHex then
          Break;
      'x', 'X':
        begin
          if (FLine[idx1] <> '0') or (Run > Succ(idx1)) then
            Break;
          if not (FLine[Succ(Run)] in ['0'..'9', 'a'..'f', 'A'..'F']) then
            Break;
          isHex := True;
        end;
    end;
    Inc(Run);
  end;
end;

procedure TSynJScriptSyn.OrSymbolProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in ['=', '|'] then inc(Run);
end;

procedure TSynJScriptSyn.PlusProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in ['=', '+'] then inc(Run);
end;

procedure TSynJScriptSyn.PointProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if (fLine[Run] = '.') and (fLine[Run + 1] = '.') then inc(Run, 2);
end;

procedure TSynJScriptSyn.SlashProc;
begin
  Inc(Run);
  case fLine[Run] of
    '/': begin
           fTokenID := tkComment;
           repeat
             Inc(Run);
           until fLine[Run] in [#0, #10, #13];
         end;
    '*': begin
           fTokenID := tkComment;
           fRange := rsAnsi;
           repeat
             Inc(Run);
             if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then begin
               fRange := rsUnKnown;
               Inc(Run, 2);
               break;
             end;
           until fLine[Run] in [#0, #10, #13];
         end;
    '=': begin
           Inc(Run);
           fTokenID := tkSymbol;
         end;
    else
      fTokenID := tkSymbol;
  end;
end;

procedure TSynJScriptSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynJScriptSyn.StarProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '=' then inc(Run);
end;

procedure TSynJScriptSyn.StringProc;
var
  l_strChar : String;
begin
  fTokenID := tkString;
  l_strChar := FLine[Run];   // We could have '"' or #39
  if (FLine[Run + 1] = l_strChar) and (FLine[Run + 2] = l_strChar) then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until (FLine[Run] = l_strChar) and ((FLine[Pred(Run)] <> '\') or ((Run > 2) and (FLine[Run - 2] = '\'))); // No longer counts \\" as \ then \"
  if FLine[Run] <> #0 then
    Inc(Run);
end;

procedure TSynJScriptSyn.SymbolProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynJScriptSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynJScriptSyn.Next;
begin
  fTokenPos := Run;
  if fRange = rsANSI then
    CommentProc
  else
    fProcTable[fLine[Run]];
end;

function TSynJScriptSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynJScriptSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynJScriptSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynJScriptSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynJScriptSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynJScriptSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNonReservedKey: Result := fNonReservedKeyAttri;
    tkEvent: Result := fEventAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynJScriptSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynJScriptSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynJScriptSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynJScriptSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynJScriptSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

function TSynJScriptSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterJScript;
end;

class function TSynJScriptSyn.GetLanguageName: string;
begin
  Result := SYNS_LangJScript;
end;

function TSynJScriptSyn.GetSampleSource: String;
begin
  Result := '// Syntax highlighting'#13#10+
            'function printNumber()'#13#10+
            '{'#13#10+
            '  var number = 1234;'#13#10+
            '  var x;'#13#10+
            '  document.write("The number is " + number);'#13#10+
            '  for (var i = 0; i <= number; i++)'#13#10+
            '  {'#13#10+
            '    x++;'#13#10+
            '    x--;'#13#10+
            '    x += 1.0;'#13#10+
            '  }'#13#10+
            '  i += @; // illegal character'#13#10+
            '}'#13#10+
            'body.onLoad = printNumber;';
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynJScriptSyn);
{$ENDIF}
end.
