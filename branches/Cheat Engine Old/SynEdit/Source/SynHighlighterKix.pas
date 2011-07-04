{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterKix.pas, released 2000-05-05.
The Original Code is based on the jsKixSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Jeff D. Smith.
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

$Id: SynHighlighterKix.pas,v 1.13 2005/01/28 16:53:24 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Kix syntax highlighter for SynEdit)
@author(Jeff D. Smith)
@created(1999, converted to SynEdit 2000-05-05)
@lastmod(2000-06-23)
The SynHighlighterKix unit provides SynEdit with a Kix script file syntax highlighter.
}

{$IFNDEF QSYNHIGHLIGHTERKIX}
unit SynHighlighterKix;
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
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkMiscellaneous, tkNull,
    tkNumber, tkSpace, tkString, tkSymbol, tkVariable, tkUnknown);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

type
  TSynKixSyn = class(TSynCustomHighlighter)
  private
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..273] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fMiscellaneousAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fVariableAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: String): Boolean;
    function Func7: TtkTokenKind;
    function Func15: TtkTokenKind;
    function Func17: TtkTokenKind;
    function Func18: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func21: TtkTokenKind;
    function Func22: TtkTokenKind;
    function Func23: TtkTokenKind;
    function Func25: TtkTokenKind;
    function Func26: TtkTokenKind;
    function Func28: TtkTokenKind;
    function Func29: TtkTokenKind;
    function Func30: TtkTokenKind;
    function Func31: TtkTokenKind;
    function Func32: TtkTokenKind;
    function Func34: TtkTokenKind;
    function Func35: TtkTokenKind;
    function Func36: TtkTokenKind;
    function Func37: TtkTokenKind;
    function Func38: TtkTokenKind;
    function Func40: TtkTokenKind;
    function Func41: TtkTokenKind;
    function Func44: TtkTokenKind;
    function Func45: TtkTokenKind;
    function Func47: TtkTokenKind;
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
    function Func61: TtkTokenKind;
    function Func62: TtkTokenKind;
    function Func63: TtkTokenKind;
    function Func64: TtkTokenKind;
    function Func65: TtkTokenKind;
    function Func67: TtkTokenKind;
    function Func68: TtkTokenKind;
    function Func69: TtkTokenKind;
    function Func70: TtkTokenKind;
    function Func72: TtkTokenKind;
    function Func73: TtkTokenKind;
    function Func74: TtkTokenKind;
    function Func76: TtkTokenKind;
    function Func77: TtkTokenKind;
    function Func78: TtkTokenKind;
    function Func79: TtkTokenKind;
    function Func80: TtkTokenKind;
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
    function Func93: TtkTokenKind;
    function Func94: TtkTokenKind;
    function Func95: TtkTokenKind;
    function Func96: TtkTokenKind;
    function Func97: TtkTokenKind;
    function Func99: TtkTokenKind;
    function Func100: TtkTokenKind;
    function Func102: TtkTokenKind;
    function Func104: TtkTokenKind;
    function Func105: TtkTokenKind;
    function Func108: TtkTokenKind;
    function Func109: TtkTokenKind;
    function Func110: TtkTokenKind;
    function Func111: TtkTokenKind;
    function Func112: TtkTokenKind;
    function Func114: TtkTokenKind;
    function Func115: TtkTokenKind;
    function Func116: TtkTokenKind;
    function Func118: TtkTokenKind;
    function Func119: TtkTokenKind;
    function Func120: TtkTokenKind;
    function Func123: TtkTokenKind;
    function Func124: TtkTokenKind;
    function Func127: TtkTokenKind;
    function Func130: TtkTokenKind;
    function Func135: TtkTokenKind;
    function Func136: TtkTokenKind;
    function Func139: TtkTokenKind;
    function Func140: TtkTokenKind;
    function Func144: TtkTokenKind;
    function Func148: TtkTokenKind;
    function Func152: TtkTokenKind;
    function Func154: TtkTokenKind;
    function Func156: TtkTokenKind;
    function Func161: TtkTokenKind;
    function Func166: TtkTokenKind;
    function Func169: TtkTokenKind;
    function Func173: TtkTokenKind;
    function Func174: TtkTokenKind;
    function Func177: TtkTokenKind;
    function Func186: TtkTokenKind;
    function Func195: TtkTokenKind;
    function Func196: TtkTokenKind;
    function Func197: TtkTokenKind;
    function Func213: TtkTokenKind;
    function Func221: TtkTokenKind;
    function Func222: TtkTokenKind;
    function Func230: TtkTokenKind;
    function Func233: TtkTokenKind;
    function Func243: TtkTokenKind;
    function Func273: TtkTokenKind;
    procedure AsciiCharProc;
    procedure VariableProc;
    procedure CRProc;
    procedure IdentProc;
    procedure MacroProc;
    procedure PrintProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure CommentProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(NewValue: String; LineNumber: Integer); override;
    function GetToken: String; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property MiscellaneousAttri: TSynHighlighterAttributes
      read fMiscellaneousAttri write fMiscellaneousAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property VariableAttri: TSynHighlighterAttributes read fVariableAttri
      write fVariableAttri;
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
    case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z':
        Identifiers[I] := True;
      else
        Identifiers[I] := False;
    end;
    J := UpCase(I);
    case I in ['_', 'A'..'Z', 'a'..'z'] of
      True: mHashTable[I] := Ord(J) - 64
      else mHashTable[I] := 0;
    end;
  end;
end;

procedure TSynKixSyn.InitIdent;
var
  I: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do begin
    pF^ := AltFunc;
    Inc(pF);
  end;
  fIdentFuncTable[7] := Func7;
  fIdentFuncTable[15] := Func15;
  fIdentFuncTable[17] := Func17;
  fIdentFuncTable[18] := Func18;
  fIdentFuncTable[19] := Func19;
  fIdentFuncTable[21] := Func21;
  fIdentFuncTable[22] := Func22;
  fIdentFuncTable[23] := Func23;
  fIdentFuncTable[25] := Func25;
  fIdentFuncTable[26] := Func26;
  fIdentFuncTable[28] := Func28;
  fIdentFuncTable[29] := Func29;
  fIdentFuncTable[30] := Func30;
  fIdentFuncTable[31] := Func31;
  fIdentFuncTable[32] := Func32;
  fIdentFuncTable[34] := Func34;
  fIdentFuncTable[35] := Func35;
  fIdentFuncTable[36] := Func36;
  fIdentFuncTable[37] := Func37;
  fIdentFuncTable[38] := Func38;
  fIdentFuncTable[40] := Func40;
  fIdentFuncTable[41] := Func41;
  fIdentFuncTable[44] := Func44;
  fIdentFuncTable[45] := Func45;
  fIdentFuncTable[47] := Func47;
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
  fIdentFuncTable[61] := Func61;
  fIdentFuncTable[62] := Func62;
  fIdentFuncTable[63] := Func63;
  fIdentFuncTable[64] := Func64;
  fIdentFuncTable[65] := Func65;
  fIdentFuncTable[67] := Func67;
  fIdentFuncTable[68] := Func68;
  fIdentFuncTable[69] := Func69;
  fIdentFuncTable[70] := Func70;
  fIdentFuncTable[72] := Func72;
  fIdentFuncTable[73] := Func73;
  fIdentFuncTable[74] := Func74;
  fIdentFuncTable[76] := Func76;
  fIdentFuncTable[77] := Func77;
  fIdentFuncTable[78] := Func78;
  fIdentFuncTable[79] := Func79;
  fIdentFuncTable[80] := Func80;
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
  fIdentFuncTable[93] := Func93;
  fIdentFuncTable[94] := Func94;
  fIdentFuncTable[95] := Func95;
  fIdentFuncTable[96] := Func96;
  fIdentFuncTable[97] := Func97;
  fIdentFuncTable[99] := Func99;
  fIdentFuncTable[100] := Func100;
  fIdentFuncTable[102] := Func102;
  fIdentFuncTable[104] := Func104;
  fIdentFuncTable[105] := Func105;
  fIdentFuncTable[108] := Func108;
  fIdentFuncTable[109] := Func109;
  fIdentFuncTable[110] := Func110;
  fIdentFuncTable[111] := Func111;
  fIdentFuncTable[112] := Func112;
  fIdentFuncTable[114] := Func114;
  fIdentFuncTable[115] := Func115;
  fIdentFuncTable[116] := Func116;
  fIdentFuncTable[118] := Func118;
  fIdentFuncTable[119] := Func119;
  fIdentFuncTable[120] := Func120;
  fIdentFuncTable[123] := Func123;
  fIdentFuncTable[124] := Func124;
  fIdentFuncTable[127] := Func127;
  fIdentFuncTable[130] := Func130;
  fIdentFuncTable[135] := Func135;
  fIdentFuncTable[136] := Func136;
  fIdentFuncTable[139] := Func139;
  fIdentFuncTable[140] := Func140;
  fIdentFuncTable[144] := Func144;
  fIdentFuncTable[148] := Func148;
  fIdentFuncTable[152] := Func152;
  fIdentFuncTable[154] := Func154;
  fIdentFuncTable[156] := Func156;
  fIdentFuncTable[161] := Func161;
  fIdentFuncTable[166] := Func166;
  fIdentFuncTable[169] := Func169;
  fIdentFuncTable[173] := Func173;
  fIdentFuncTable[174] := Func174;
  fIdentFuncTable[177] := Func177;
  fIdentFuncTable[186] := Func186;
  fIdentFuncTable[195] := Func195;
  fIdentFuncTable[196] := Func196;
  fIdentFuncTable[197] := Func197;
  fIdentFuncTable[213] := Func213;
  fIdentFuncTable[221] := Func221;
  fIdentFuncTable[222] := Func222;
  fIdentFuncTable[230] := Func230;
  fIdentFuncTable[233] := Func233;
  fIdentFuncTable[243] := Func243;
  fIdentFuncTable[273] := Func273;
end;

function TSynKixSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while Identifiers[ToHash^] do begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end;

function TSynKixSyn.KeyComp(const aKey: String): Boolean;
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
  end else Result := False;
end;

function TSynKixSyn.Func7: TtkTokenKind;
begin
  if KeyComp('CD') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func15: TtkTokenKind;
begin
  if KeyComp('IF') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func17: TtkTokenKind;
begin
  if KeyComp('MD') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func18: TtkTokenKind;
begin
  if KeyComp('BIG') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func19: TtkTokenKind;
begin
  if KeyComp('DO') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func21: TtkTokenKind;
begin
  if KeyComp('DEL') then Result := tkKey else
    if KeyComp('AT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func22: TtkTokenKind;
begin
  if KeyComp('GO') then Result := tkKey else
    if KeyComp('RD') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func23: TtkTokenKind;
begin
  if KeyComp('ASC') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func25: TtkTokenKind;
begin
  if KeyComp('LM') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func26: TtkTokenKind;
begin
  if KeyComp('DIM') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func28: TtkTokenKind;
begin
  if KeyComp('BEEP') then Result := tkKey else
    if KeyComp('CASE') then Result := tkKey else
      if KeyComp('CALL') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func29: TtkTokenKind;
begin
  if KeyComp('CHR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func30: TtkTokenKind;
begin
  if KeyComp('DAY') then Result := tkKey else
    if KeyComp('DATE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func31: TtkTokenKind;
begin
  if KeyComp('DIR') then Result := tkKey else
    if KeyComp('LEN') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func32: TtkTokenKind;
begin
  if KeyComp('SID') then Result := tkKey else
    if KeyComp('GET') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func34: TtkTokenKind;
begin
  if KeyComp('CLS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func35: TtkTokenKind;
begin
  if KeyComp('VAL') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func36: TtkTokenKind;
begin
  if KeyComp('RND') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func37: TtkTokenKind;
begin
  if KeyComp('BREAK') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func38: TtkTokenKind;
begin
  if KeyComp('DOS') then Result := tkKey else
    if KeyComp('RAS') then Result := tkKey else
      if KeyComp('ENDIF') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func40: TtkTokenKind;
begin
  if KeyComp('LCASE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func41: TtkTokenKind;
begin
  if KeyComp('ELSE') then Result := tkKey else
    if KeyComp('BOX') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func44: TtkTokenKind;
begin
  if KeyComp('KIX') then Result := tkKey else
    if KeyComp('SET') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func45: TtkTokenKind;
begin
  if KeyComp('USE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func47: TtkTokenKind;
begin
  if KeyComp('TIME') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func49: TtkTokenKind;
begin
  if KeyComp('YEAR') then Result := tkKey else
    if KeyComp('UCASE') then Result := tkKey else
      if KeyComp('GLOBAL') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func50: TtkTokenKind;
begin
  if KeyComp('ADDKEY') then Result := tkKey else
    if KeyComp('OPEN') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func51: TtkTokenKind;
begin
  if KeyComp('GETS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func52: TtkTokenKind;
begin
  if KeyComp('PWAGE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func53: TtkTokenKind;
begin
  if KeyComp('RUN') then Result := tkKey else
    if KeyComp('SITE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func54: TtkTokenKind;
begin
  if KeyComp('PLAY') then Result := tkKey else
    if KeyComp('CLOSE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func55: TtkTokenKind;
begin
  if KeyComp('SRND') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func56: TtkTokenKind;
begin
  if KeyComp('DOMAIN') then Result := tkKey else
    if KeyComp('SETL') then Result := tkKey else
      if KeyComp('SHELL') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func57: TtkTokenKind;
begin
  if KeyComp('WHILE') then Result := tkKey else
    if KeyComp('SMALL') then Result := tkKey else
      if KeyComp('SETM') then Result := tkKey else
        if KeyComp('GOTO') then Result := tkKey else
          if KeyComp('SLEEP') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func58: TtkTokenKind;
begin
  if KeyComp('COOKIE1') then Result := tkKey else
    if KeyComp('LOOP') then Result := tkKey else
      if KeyComp('EXIT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func59: TtkTokenKind;
begin
  if KeyComp('COPY') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func61: TtkTokenKind;
begin
  if KeyComp('LOGOFF') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func62: TtkTokenKind;
begin
  if KeyComp('DELKEY') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func63: TtkTokenKind;
begin
  if KeyComp('COLOR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func64: TtkTokenKind;
begin
  if KeyComp('GOSUB') then Result := tkKey else
    if KeyComp('SELECT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func65: TtkTokenKind;
begin
  if KeyComp('PRIV') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func67: TtkTokenKind;
begin
  if KeyComp('QUIT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func68: TtkTokenKind;
begin
  if KeyComp('LDOMAIN') then Result := tkKey else
    if KeyComp('READLINE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func69: TtkTokenKind;
begin
  if KeyComp('DELTREE') then Result := tkKey else
    if KeyComp('INWIN') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func70: TtkTokenKind;
begin
  if KeyComp('LDRIVE') then Result := tkKey else
    if KeyComp('MONTH') then Result := tkKey else
      if KeyComp('ADDRESS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func72: TtkTokenKind;
begin
  if KeyComp('MDAYNO') then Result := tkKey else
    if KeyComp('HOMEDIR') then Result := tkKey else
      if KeyComp('LTRIM') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func73: TtkTokenKind;
begin
  if KeyComp('CURDIR') then Result := tkKey else
    if KeyComp('LOADKEY') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func74: TtkTokenKind;
begin
  if KeyComp('WKSTA') then Result := tkKey else
    if KeyComp('ERROR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func76: TtkTokenKind;
begin
  if KeyComp('LOADHIVE') then Result := tkKey else
    if KeyComp('USERID') then Result := tkKey else
      if KeyComp('UNTIL') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func77: TtkTokenKind;
begin
  if KeyComp('EXIST') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func78: TtkTokenKind;
begin
  if KeyComp('RTRIM') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func79: TtkTokenKind;
begin
  if KeyComp('FLUSHKB') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func80: TtkTokenKind;
begin
  if KeyComp('INSTR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func82: TtkTokenKind;
begin
  if KeyComp('WDAYNO') then Result := tkKey else
    if KeyComp('DELVALUE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func83: TtkTokenKind;
begin
  if KeyComp('COMMENT') then Result := tkKey else
    if KeyComp('EXECUTE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func84: TtkTokenKind;
begin
  if KeyComp('DECTOHEX') then Result := tkKey else
    if KeyComp('YDAYNO') then Result := tkKey else
      if KeyComp('FULLNAME') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func85: TtkTokenKind;
begin
  if KeyComp('SETASCII') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func86: TtkTokenKind;
begin
  if KeyComp('DISPLAY') then Result := tkKey else
    if KeyComp('HOMESHR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func87: TtkTokenKind;
begin
  if KeyComp('ENDSELECT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func88: TtkTokenKind;
begin
  if KeyComp('SAVEKEY') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func89: TtkTokenKind;
begin
  if KeyComp('READVALUE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func90: TtkTokenKind;
begin
  if KeyComp('MAXPWAGE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func91: TtkTokenKind;
begin
  if KeyComp('SETTIME') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func93: TtkTokenKind;
begin
  if KeyComp('SERROR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func94: TtkTokenKind;
begin
  if KeyComp('READTYPE') then Result := tkKey else
    if KeyComp('ENUMKEY') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func95: TtkTokenKind;
begin
  if KeyComp('LANROOT') then Result := tkKey else
    if KeyComp('IPADDRESS') then Result := tkKey else
      if KeyComp('HOSTNAME') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func96: TtkTokenKind;
begin
  if KeyComp('RETURN') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func97: TtkTokenKind;
begin
  if KeyComp('SYSLANG') then Result := tkKey else
    if KeyComp('USERLANG') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func99: TtkTokenKind;
begin
  if KeyComp('HOMEDRIVE') then Result := tkKey else
    if KeyComp('WUSERID') then Result := tkKey else
      if KeyComp('MONTHNO') then Result := tkKey else
        if KeyComp('LSERVER') then Result := tkKey else
          if KeyComp('SUBSTR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func100: TtkTokenKind;
begin
  if KeyComp('LOGEVENT') then Result := tkKey else
    if KeyComp('INGROUP') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func102: TtkTokenKind;
begin
  if KeyComp('SENDKEYS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func104: TtkTokenKind;
begin
  if KeyComp('OLECALLFUNC') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func105: TtkTokenKind;
begin
  if KeyComp('RSERVER') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func108: TtkTokenKind;
begin
  if KeyComp('SETFOCUS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func109: TtkTokenKind;
begin
  if KeyComp('STARTDIR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func110: TtkTokenKind;
begin
  if KeyComp('MESSAGEBOX') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func111: TtkTokenKind;
begin
  if KeyComp('SENDMESSAGE') then Result := tkKey else
    if KeyComp('UNLOADHIVE') then Result := tkKey else
      if KeyComp('GETFILETIME') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func112: TtkTokenKind;
begin
  if KeyComp('OLECALLPROC') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func114: TtkTokenKind;
begin
  if KeyComp('ENUMVALUE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func115: TtkTokenKind;
begin
  if KeyComp('WRITELINE') then Result := tkKey else
    if KeyComp('PASSWORD') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func116: TtkTokenKind;
begin
  if KeyComp('SCRIPTDIR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func118: TtkTokenKind;
begin
  if KeyComp('EXISTKEY') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func119: TtkTokenKind;
begin
  if KeyComp('GETDISKSPACE') then Result := tkKey else
    if KeyComp('OLEGETOBJECT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func120: TtkTokenKind;
begin
  if KeyComp('LONGHOMEDIR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func123: TtkTokenKind;
begin
  if KeyComp('GETFILESIZE') then Result := tkKey else
    if KeyComp('GETFILEATTR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func124: TtkTokenKind;
begin
  if KeyComp('SHUTDOWN') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func127: TtkTokenKind;
begin
  if KeyComp('SETCONSOLE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func130: TtkTokenKind;
begin
  if KeyComp('ENUMGROUP') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func135: TtkTokenKind;
begin
  if KeyComp('SETFILEATTR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func136: TtkTokenKind;
begin
  if KeyComp('WRITEVALUE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func139: TtkTokenKind;
begin
  if KeyComp('OLECREATEOBJECT') then Result := tkKey else
    if KeyComp('CLEAREVENTLOG') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func140: TtkTokenKind;
begin
  if KeyComp('OLEENUMOBJECT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func144: TtkTokenKind;
begin
  if KeyComp('ADDPROGRAMITEM') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func148: TtkTokenKind;
begin
  if KeyComp('SETWALLPAPER') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func152: TtkTokenKind;
begin
  if KeyComp('OLERELEASEOBJECT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func154: TtkTokenKind;
begin
  if KeyComp('BACKUPEVENTLOG') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func156: TtkTokenKind;
begin
  if KeyComp('DELPROGRAMITEM') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func161: TtkTokenKind;
begin
  if KeyComp('OLEGETSUBOBJECT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func166: TtkTokenKind;
begin
  if KeyComp('GETFILEVERSION') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func169: TtkTokenKind;
begin
  if KeyComp('COMPAREFILETIMES') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func173: TtkTokenKind;
begin
  if KeyComp('ENUMLOCALGROUP') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func174: TtkTokenKind;
begin
  if KeyComp('ADDPROGRAMGROUP') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func177: TtkTokenKind;
begin
  if KeyComp('PRIMARYGROUP') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func186: TtkTokenKind;
begin
  if KeyComp('DELPROGRAMGROUP') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func195: TtkTokenKind;
begin
  if KeyComp('REDIRECTOUTPUT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func196: TtkTokenKind;
begin
  if KeyComp('READPROFILESTRING') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func197: TtkTokenKind;
begin
  if KeyComp('OLEGETPROPERTY') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func213: TtkTokenKind;
begin
  if KeyComp('SETDEFAULTPRINTER') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func221: TtkTokenKind;
begin
  if KeyComp('ADDPRINTERCONNECTION') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func222: TtkTokenKind;
begin
  if KeyComp('OLEPUTPROPERTY') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func230: TtkTokenKind;
begin
  if KeyComp('SHOWPROGRAMGROUP') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func233: TtkTokenKind;
begin
  if KeyComp('DELPRINTERCONNECTION') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func243: TtkTokenKind;
begin
  if KeyComp('WRITEPROFILESTRING') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.Func273: TtkTokenKind;
begin
  if KeyComp('EXPANDENVIRONMENTVARS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynKixSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynKixSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 274 then
    Result := fIdentFuncTable[HashKey]
  else
    Result := tkIdentifier;
end;

procedure TSynKixSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      '#': fProcTable[I] := AsciiCharProc;
      #13: fProcTable[I] := CRProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := IdentProc;
      #10: fProcTable[I] := LFProc;
      #0: fProcTable[I] := NullProc;
      '0'..'9': fProcTable[I] := NumberProc;
      ';': fProcTable[I] := CommentProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := SpaceProc;
      '"','''': fProcTable[I] := StringProc;
      '@': fProcTable[I] := MacroProc;
      '?': fProcTable[I] := PrintProc;
      '$': fProcTable[I] := VariableProc;
    else
      fProcTable[I] := UnknownProc;
    end;
end;

constructor TSynKixSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrKey);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fMiscellaneousAttri := TSynHighlighterAttributes.Create(SYNS_AttrMiscellaneous);
  AddAttribute(fMiscellaneousAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);
  fVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable);
  AddAttribute(fVariableAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterKIX;
end;

procedure TSynKixSyn.SetLine(NewValue: String; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynKixSyn.AsciiCharProc;
begin
  fTokenID := tkString;
  inc(Run);
  while FLine[Run] in ['0'..'9'] do inc(Run);
end;

procedure TSynKixSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then inc(Run);
end;

procedure TSynKixSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TSynKixSyn.MacroProc;
begin
  inc(Run);
  fTokenID := tkMiscellaneous;
  while FLine[Run] in ['0'..'9', 'A'..'Z', 'a'..'z'] do inc(Run);
end;

procedure TSynKixSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynKixSyn.PrintProc;
begin
  fTokenID := tkKey;
  inc(Run);
end;

procedure TSynKixSyn.VariableProc;
begin
  fTokenId := tkVariable;
  inc(run);
  while FLine[Run] in ['_','0'..'9', 'A'..'Z', 'a'..'z'] do inc(run);
end;

procedure TSynKixSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynKixSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', 'e', 'E'] do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
end;

procedure TSynKixSyn.CommentProc;
begin
  fTokenID := tkComment;
  repeat
    inc(Run);
  until fLine[Run] in [#0, #10, #13];
end;

procedure TSynKixSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynKixSyn.StringProc;
var
   achr : char;
begin
  fTokenID := tkString;
  achr := fline[run];
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = achr;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynKixSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynKixSyn.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];
end;

function TSynKixSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
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

function TSynKixSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynKixSyn.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynKixSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynKixSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkMiscellaneous: Result := fMiscellaneousAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkVariable: Result := fVariableAttri;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynKixSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynKixSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynKixSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

class function TSynKixSyn.GetLanguageName: string;
begin
  Result := SYNS_LangKIX;
end;

function TSynKixSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterKIX;
end;

function TSynKixSyn.GetSampleSource: string;
begin
  Result := '; KiXtart sample source'#13#10 +
            'break on'#13#10 +
            'color b/n'#13#10 +
            #13#10 +
            'AT(1, 30) "Hello World!"'#13#10 +
            '$USERID = @USERID'#13#10 +
            'AT(1, 30) $USERID';
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynKixSyn);
{$ENDIF}
end.
