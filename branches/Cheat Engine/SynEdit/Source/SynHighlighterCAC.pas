{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterCAC.pas, released 2000-04-21.
The Original Code is based on the cwCACSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Carlos Wijders.
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

$Id: SynHighlighterCAC.pas,v 1.11 2005/01/28 16:53:21 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a CA-Clipper syntax highlighter for SynEdit)
@author(Carlos Wijders <ctfbs@sr.net>, converted to SynEdit by Bruno Mikkelsen <btm@scientist.com>)
@created(1998-12-27, converted to SynEdit 2000-04-21)
@lastmod(2000-06-23)
The SynHighlighterCAC unit provides SynEdit with a CA-Clipper syntax highlighter.
Thanks to Primoz Gabrijelcic, Andy Jeffries.
}

{$IFNDEF QSYNHIGHLIGHTERCAC}
unit SynHighlighterCAC;
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
  TtkTokenKind = (tkComment, tkDirective, tkIdentifier, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkOperator, tkUnknown);

  TRangeState = (rsANil, rsCStyle, rsUnknown);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

  TSynCACSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fLine: PChar;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    fLineNumber: Integer;
    fStringAttri: TSynHighlighterAttributes;
    fOperatorAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fDirecAttri: TSynHighlighterAttributes;

    fIdentFuncTable: array[0..124] of TIdentFuncTableFunc;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    function Func10: TtkTokenKind;
    function Func15: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func21: TtkTokenKind;
    function Func22: TtkTokenKind;
    function Func23: TtkTokenKind;
    function Func24: TtkTokenKind;
    function Func26: TtkTokenKind;
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
    function Func63: TtkTokenKind;
    function Func64: TtkTokenKind;
    function Func65: TtkTokenKind;
    function Func66: TtkTokenKind;
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
    function Func81: TtkTokenKind;
    function Func86: TtkTokenKind;
    function Func87: TtkTokenKind;
    function Func89: TtkTokenKind;
    function Func91: TtkTokenKind;
    function Func94: TtkTokenKind;
    function Func96: TtkTokenKind;
    function Func98: TtkTokenKind;
    function Func99: TtkTokenKind;
    function Func100: TtkTokenKind;
    function Func101: TtkTokenKind;
    function Func102: TtkTokenKind;
    function Func105: TtkTokenKind;
    function Func116: TtkTokenKind;
    function Func124: TtkTokenKind;
    procedure StarProc;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SymbolProc;
    procedure StringProc;
    procedure DirectiveProc;
    procedure UnknownProc;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure CStyleProc;
  protected
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
    procedure SetLine(NewValue: string; LineNumber: Integer); override;
    function GetToken: string; override;
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
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property OperatorAttri: TSynHighlighterAttributes read fOperatorAttri
      write fOperatorAttri;
    property DirecAttri: TSynHighlighterAttributes read fDirecAttri
      write fDirecAttri;
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
      '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
    else Identifiers[I] := False;
    end;
    J := UpCase(I);
    case I in ['_', 'a'..'z', 'A'..'Z'] of
      True: mHashTable[I] := Ord(J) - 64
    else mHashTable[I] := 0;
    end;
  end;
end;

procedure TSynCACSyn.InitIdent;
var
  I: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do begin
    pF^ := AltFunc;
    Inc(pF);
  end;
  fIdentFuncTable[10] := Func10;
  fIdentFuncTable[15] := Func15;
  fIdentFuncTable[19] := Func19;
  fIdentFuncTable[21] := Func21;
  fIdentFuncTable[22] := Func22;
  fIdentFuncTable[23] := Func23;
  fIdentFuncTable[24] := Func24;
  fIdentFuncTable[26] := Func26;
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
  fIdentFuncTable[63] := Func63;
  fIdentFuncTable[64] := Func64;
  fIdentFuncTable[65] := Func65;
  fIdentFuncTable[66] := Func66;
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
  fIdentFuncTable[81] := Func81;
  fIdentFuncTable[86] := Func86;
  fIdentFuncTable[87] := Func87;
  fIdentFuncTable[89] := Func89;
  fIdentFuncTable[91] := Func91;
  fIdentFuncTable[94] := Func94;
  fIdentFuncTable[96] := Func96;
  fIdentFuncTable[98] := Func98;
  fIdentFuncTable[99] := Func99;
  fIdentFuncTable[100] := Func100;
  fIdentFuncTable[101] := Func101;
  fIdentFuncTable[102] := Func102;
  fIdentFuncTable[105] := Func105;
  fIdentFuncTable[116] := Func116;
  fIdentFuncTable[124] := Func124;
end;

function TSynCACSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in TSynValidStringChars do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end;

function TSynCACSyn.KeyComp(const aKey: string): Boolean;
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

function TSynCACSyn.Func10: TtkTokenKind;
begin
  if KeyComp('AADD') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func15: TtkTokenKind;
begin
  if KeyComp('IF') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func19: TtkTokenKind;
begin
  if KeyComp('AND') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func21: TtkTokenKind;
begin
  if KeyComp('AT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func22: TtkTokenKind;
begin
  if KeyComp('GO') then Result := tkKey else
    if KeyComp('ABS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func23: TtkTokenKind;
begin
  if KeyComp('BOF') then Result := tkKey else
    if KeyComp('ASC') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func24: TtkTokenKind;
begin
  if KeyComp('IIF') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func26: TtkTokenKind;
begin
  if KeyComp('EOF') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func28: TtkTokenKind;
begin
  if KeyComp('READ') then Result := tkKey else
    if KeyComp('CALL') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func29: TtkTokenKind;
begin
  if KeyComp('CHR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func30: TtkTokenKind;
begin
  if KeyComp('DAY') then Result := tkKey else
    if KeyComp('DATE') then Result := tkKey else
      if KeyComp('COL') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func31: TtkTokenKind;
begin
  if KeyComp('PACK') then Result := tkKey else
    if KeyComp('LEN') then Result := tkKey else
      if KeyComp('DIR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func32: TtkTokenKind;
begin
  if KeyComp('GET') then Result := tkKey else
    if KeyComp('FILE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func33: TtkTokenKind;
begin
  if KeyComp('FIND') then Result := tkKey else
    if KeyComp('OR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func34: TtkTokenKind;
begin
  if KeyComp('LOG') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func35: TtkTokenKind;
begin
  if KeyComp('VAL') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func36: TtkTokenKind;
begin
  if KeyComp('FIELD') then Result := tkKey else
    if KeyComp('MIN') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func37: TtkTokenKind;
begin
  if KeyComp('BEGIN') then Result := tkKey else
    if KeyComp('BREAK') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func38: TtkTokenKind;
begin
  if KeyComp('ENDIF') then Result := tkKey else
    if KeyComp('CANCEL') then Result := tkKey else
      if KeyComp('MAX') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func39: TtkTokenKind;
begin
  if KeyComp('CLEAR') then Result := tkKey else
    if KeyComp('FOR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func40: TtkTokenKind;
begin
  if KeyComp('SEEK') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func41: TtkTokenKind;
begin
  if KeyComp('ELSE') then Result := tkKey else
    if KeyComp('LOCK') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func42: TtkTokenKind;
begin
  if KeyComp('ENDDO') then Result := tkKey else
    if KeyComp('CTOD') then Result := tkKey else
      if KeyComp('DOW') then Result := tkKey else
        if KeyComp('DTOC') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func43: TtkTokenKind;
begin
  if KeyComp('LOCAL') then Result := tkKey else
    if KeyComp('INT') then Result := tkKey else
      if KeyComp('EJECT') then Result := tkKey else
        if KeyComp('ZAP') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func44: TtkTokenKind;
begin
  if KeyComp('SPACE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func45: TtkTokenKind;
begin
  if KeyComp('SAY') then Result := tkKey else
    if KeyComp('EXP') then Result := tkKey else
      if KeyComp('CDOW') then Result := tkKey else
        if KeyComp('USE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func46: TtkTokenKind;
begin
  if KeyComp('PCOL') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func47: TtkTokenKind;
begin
  if KeyComp('FLOCK') then Result := tkKey else
    if KeyComp('TIME') then Result := tkKey else
      if KeyComp('SAVE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func48: TtkTokenKind;
begin
  if KeyComp('DECLARE') then Result := tkKey else
    if KeyComp('ERASE') then Result := tkKey else
      if KeyComp('JOIN') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func49: TtkTokenKind;
begin
  if KeyComp('NOT') then Result := tkKey else
    if KeyComp('YEAR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func51: TtkTokenKind;
begin
  if KeyComp('RECALL') then Result := tkKey else
    if KeyComp('DELETE') then Result := tkKey else
      if KeyComp('ENDCASE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func52: TtkTokenKind;
begin
  if KeyComp('INIT') then Result := tkKey else
    if KeyComp('CREATE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func53: TtkTokenKind;
begin
  if KeyComp('WAIT') then Result := tkKey else
    if KeyComp('SUM') then Result := tkKey else
      if KeyComp('RUN') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func54: TtkTokenKind;
begin
  if KeyComp('CLOSE') then Result := tkKey else
    if KeyComp('NOTE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func55: TtkTokenKind;
begin
  if KeyComp('DELETED') then Result := tkKey else
    if KeyComp('SKIP') then Result := tkKey else
      if KeyComp('RECNO') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func56: TtkTokenKind;
begin
  if KeyComp('ROW') then Result := tkKey else
    if KeyComp('INDEX') then Result := tkKey else
      if KeyComp('LOCATE') then Result := tkKey else
        if KeyComp('RENAME') then Result := tkKey else
          if KeyComp('ELSEIF') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func57: TtkTokenKind;
begin
  if KeyComp('WHILE') then Result := tkKey else
    if KeyComp('STR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func58: TtkTokenKind;
begin
  if KeyComp('EXIT') then Result := tkKey else
    if KeyComp('DTOS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func59: TtkTokenKind;
begin
  if KeyComp('RLOCK') then Result := tkKey else
    if KeyComp('COPY') then Result := tkKey else
      if KeyComp('AVERAGE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func60: TtkTokenKind;
begin
  if KeyComp('REPLACE') then Result := tkKey else
    if KeyComp('LIST') then Result := tkKey else
      if KeyComp('TRIM') then Result := tkKey else
        if KeyComp('WORD') then Result := tkKey else
          if KeyComp('FOUND') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func63: TtkTokenKind;
begin
  if KeyComp('PUBLIC') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func64: TtkTokenKind;
begin
  if KeyComp('SELECT') then Result := tkKey else
    if KeyComp('SELECT') then Result := tkKey else
      if KeyComp('INKEY') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func65: TtkTokenKind;
begin
  if KeyComp('RELEASE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func66: TtkTokenKind;
begin
  if KeyComp('TYPE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func67: TtkTokenKind;
begin
  if KeyComp('UPDATE') then Result := tkKey else
    if KeyComp('QUIT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func68: TtkTokenKind;
begin
  if KeyComp('TOTAL') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func69: TtkTokenKind;
begin
  if KeyComp('TEXT') then Result := tkKey else
    if KeyComp('FIELDNAME') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func70: TtkTokenKind;
begin
  if KeyComp('MONTH') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func72: TtkTokenKind;
begin
  if KeyComp('ROUND') then Result := tkKey else
    if KeyComp('LTRIM') then Result := tkKey else
      if KeyComp('MEMVAR') then Result := tkKey else
        if KeyComp('SORT') then Result := tkKey else
          if KeyComp('STATIC') then Result := tkKey else
            if KeyComp('PROW') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func73: TtkTokenKind;
begin
  if KeyComp('LOWER') then Result := tkKey else
    if KeyComp('COUNT') then Result := tkKey else
      if KeyComp('COMMIT') then Result := tkKey else
        if KeyComp('CMONTH') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func74: TtkTokenKind;
begin
  if KeyComp('SQRT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func76: TtkTokenKind;
begin
  if KeyComp('UPPER') then Result := tkKey else
    if KeyComp('UNLOCK') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func77: TtkTokenKind;
begin
  if KeyComp('STORE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func78: TtkTokenKind;
begin
  if KeyComp('RTRIM') then Result := tkKey else
    if KeyComp('LASTREC') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func79: TtkTokenKind;
begin
  if KeyComp('EMPTY') then Result := tkKey else
    if KeyComp('FCOUNT') then Result := tkKey else
      if KeyComp('SECONDS') then Result := tkKey else
        if KeyComp('REINDEX') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func80: TtkTokenKind;
begin
  if KeyComp('INPUT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func81: TtkTokenKind;
begin
  if KeyComp('KEYBOARD') then Result := tkKey else
    if KeyComp('DEVPOS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func86: TtkTokenKind;
begin
  if KeyComp('DISPLAY') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func87: TtkTokenKind;
begin
  if KeyComp('ANNOUNCE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func89: TtkTokenKind;
begin
  if KeyComp('PCOUNT') then Result := tkKey else
    if KeyComp('REPLICATE') then Result := tkKey else
      if KeyComp('SEQUENCE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func91: TtkTokenKind;
begin
  if KeyComp('PRIVATE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func94: TtkTokenKind;
begin
  if KeyComp('SETPOS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func96: TtkTokenKind;
begin
  if KeyComp('RETURN') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func98: TtkTokenKind;
begin
  if KeyComp('PROMPT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func99: TtkTokenKind;
begin
  if KeyComp('RECCOUNT') then Result := tkKey else
    if KeyComp('EXTERNAL') then Result := tkKey else
      if KeyComp('SUBSTR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func100: TtkTokenKind;
begin
  if KeyComp('RESTORE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func101: TtkTokenKind;
begin
  if KeyComp('CONTINUE') then Result := tkKey else
    if KeyComp('VALTYPE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func102: TtkTokenKind;
begin
  if KeyComp('FUNCTION') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func105: TtkTokenKind;
begin
  if KeyComp('REQUEST') then Result := tkKey else
    if KeyComp('PROCEDURE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func116: TtkTokenKind;
begin
  if KeyComp('PARAMETERS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.Func124: TtkTokenKind;
begin
  if KeyComp('TRANSFORM') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCACSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynCACSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 125 then Result := fIdentFuncTable[HashKey] else Result := tkIdentifier;
end;

procedure TSynCACSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      '@': fProcTable[I] := SymbolProc;
      '&': fProcTable[I] := SymbolProc;
      '{': fProcTable[I] := SymbolProc;
      '}': fProcTable[I] := SymbolProc;
      #13: fProcTable[I] := CRProc;
      ':': fProcTable[I] := SymbolProc;
      ',': fProcTable[I] := SymbolProc;
      '#': fProcTable[I] := DirectiveProc;
      '=': fProcTable[I] := SymbolProc;
      '>': fProcTable[I] := SymbolProc;
      'A'..'Z', 'a'..'z': fProcTable[I] := IdentProc;
      '$': fProcTable[I] := SymbolProc;
      #10: fProcTable[I] := LFProc;
      '<': fProcTable[I] := SymbolProc;
      '-': fProcTable[I] := SymbolProc;
      '!': fProcTable[I] := SymbolProc;
      #0: fProcTable[I] := NullProc;
      '0'..'9': fProcTable[I] := NumberProc;
      '+': fProcTable[I] := SymbolProc;
      '.': fProcTable[I] := SymbolProc;
      '?': fProcTable[I] := SymbolProc;
      ')': fProcTable[I] := SymbolProc;
      '(': fProcTable[I] := SymbolProc;
      ';': fProcTable[I] := SymbolProc;
      '/': fProcTable[I] := SlashProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := SpaceProc;
      ']': fProcTable[I] := SymbolProc;
      '[': fProcTable[I] := SymbolProc;
      '*': fProcTable[I] := StarProc;
      #39, #34: fProcTable[I] := StringProc;
      else fProcTable[I] := UnknownProc;
    end;
end;

constructor TSynCACSyn.Create(AOwner: TComponent);
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
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  AddAttribute(fStringAttri);
  fOperatorAttri := TSynHighlighterAttributes.Create(SYNS_AttrOperator);
  AddAttribute(fOperatorAttri);
  fDirecAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor);
  AddAttribute(fDirecAttri);
  InitIdent;
  SetAttributesOnChange(DefHighlightChange);
  MakeMethodTables;
  fRange := rsUnknown;
  fDefaultFilter := SYNS_FilterCAClipper;
end;

procedure TSynCACSyn.SetLine(NewValue: string; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynCACSyn.CStyleProc;
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

  while fLine[Run] <> #0 do
    case fLine[Run] of
      '*':
        if fLine[Run + 1] = '/' then
        begin
          fRange := rsUnknown;
          inc(Run, 2);
          break;
        end else inc(Run);
      #10: break;
      #13: break;
    else inc(Run);
    end;
end;

procedure TSynCACSyn.CRProc;
begin
  fTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: inc(Run, 2);
  else inc(Run);
  end;
end;

procedure TSynCACSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TSynCACSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynCACSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynCACSyn.NumberProc;
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

procedure TSynCACSyn.SlashProc;
begin
  case FLine[Run + 1] of
    '/':
      begin
        inc(Run, 2);
        fTokenID := tkComment;
        while FLine[Run] <> #0 do
        begin
          case FLine[Run] of
            #10, #13: break;
          end;
          inc(Run);
        end;
      end;
    '*':
      begin
        fTokenID := tkComment;
        fRange := rsCStyle;
        inc(Run, 2);
        while fLine[Run] <> #0 do
          case fLine[Run] of
            '*':
              if fLine[Run + 1] = '/' then
              begin
                fRange := rsUnknown;
                inc(Run, 2);
                break;
              end else inc(Run);
            #10: break;
            #13: break;
          else inc(Run);
          end;
      end;
  else
    begin
      inc(Run);
      fTokenID := tkOperator;
    end;
  end;
end;

procedure TSynCACSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynCACSyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkOperator;
end;

procedure TSynCACSyn.StringProc;
var
  ActiveStr: string[1];
begin
  fTokenID := tkString;
  ActiveStr := FLine[Run];
  if ((FLine[Run + 1] = #39) and (FLine[Run + 2] = #39)) or
    ((FLine[Run + 1] = #34) and (FLine[Run + 2] = #34)) then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until (FLine[Run] = ActiveStr);
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynCACSyn.DirectiveProc;
begin
  fTokenID := tkDirective;
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
      '/': if FLine[Run + 1] = '/' then break;
      #34, #39: break;
    end;
    inc(Run);
  until FLine[Run] = #0;
end;

procedure TSynCACSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynCACSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsCStyle: CStyleProc;
  else fProcTable[fLine[Run]];
  end;
end;

function TSynCACSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynCACSyn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TSynCACSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynCACSyn.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynCACSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynCACSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkDirective: Result := fDirecAttri;
    tkOperator: Result := fOperatorAttri;
    tkUnknown: Result := fOperatorAttri;
    else Result := nil;
  end;
end;

function TSynCACSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynCACSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynCACSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynCACSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynCACSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterCAClipper;
end;

class function TSynCACSyn.GetLanguageName: string;
begin
  Result := SYNS_LangCAClipper;
end;

procedure TSynCACSyn.StarProc;
begin
// if Run is 0 there could be an access violation
  if (Run = 0) or (fLine[Run - 1] in [#0, #10, #13]) then
  begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end else begin

    inc(Run);
    fTokenID := tkOperator;
  end;
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynCACSyn);
{$ENDIF}
end.
